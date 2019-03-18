$(() => {
  function findLastChild(element) {
    let lastChild = element.children(".children").children(".proofStep:last-of-type");
    return lastChild.is("[data-reference-for-last-child]") ? findLastChild(lastChild) : lastChild;
  }
  function findElementByReference(lineReference, pathReference) {
    let outerElement = $(`[data-reference='${lineReference}']`);
    outerElement = outerElement.length ? outerElement : findLastChild($(`[data-reference-for-last-child='${lineReference}']`));
    return pathReference.length ? outerElement.find(`[data-path='${pathReference.join(".")}']`) : outerElement;
  }

  $("[data-premise-references]").each(function () {
    let $this = $(this);
    let premiseReferences = JSON.parse($this.attr("data-premise-references"));
    let premiseElements = _.map(premiseReferences, reference => findElementByReference(reference.lineReference, reference.internalPath));
    let conclusionElement = $this.find('.conclusion');
    $this
      .on("mouseenter", function () {
        _.each(premiseElements, x => x.addClass("highlightPremise"));
        conclusionElement.addClass("highlightConclusion");
      })
      .on("mouseleave", function () {
        _.each(premiseElements, x => x.removeClass("highlightPremise"));
        conclusionElement.removeClass("highlightConclusion");
      });
  });

  let proofLineWithOpenPopover = null;

  $(".proofLine").each(function() {
    let proofLine = $(this);
    proofLine.popover({
      placement: "bottom",
      html: true,
      trigger: "focus"
    });
    proofLine
      .on("click", e => {
        if ($(e.target).parents(".popover").length) {
          return;
        }
        if (proofLineWithOpenPopover && proofLineWithOpenPopover !== proofLine) {
          proofLineWithOpenPopover.popover("hide");
        }
        proofLine.popover("show");
        bindPopover(proofLine);
        e.stopPropagation();
      })
      .on("show.bs.popover", function() {
        proofLineWithOpenPopover = proofLine;
      })
      .on("hide.bs.popover", function() {
        if (proofLineWithOpenPopover === proofLine) {
          proofLineWithOpenPopover = null;
        }
      });
  });
  $("body").on("click", e => {
      // Don't hide the popover if we're clicking ON it or it's triggered a modal
      if ($(e.target).parents(".popover, .modal").length) {
        return;
      }
      if (proofLineWithOpenPopover) {
        proofLineWithOpenPopover.popover("hide");
      }
    });

  function bindPopover(proofLine) {
    let lineReference = proofLine.attr("data-reference");
    proofLine.off("click.editBoundVariable").on("click.editBoundVariable", ".editablePremise .boundVariable", function() {
      let boundVariableElement = $(this);
      let premiseIndex = boundVariableElement.parents(".editablePremise").attr("data-index");
      let index = boundVariableElement.attr("data-index");
      let parentWithPath = boundVariableElement.parents("[data-path]").eq(0);
      let path = parentWithPath ? parentWithPath.attr("data-path") : "";
      let currentName = boundVariableElement.text();
      $("#boundVariableNameInput").val(currentName);
      $("#saveBoundVariableNameButton").off("click").on("click", () => {
        let newName =  $("#boundVariableNameInput").val();
        $.ajax({
          url: `${window.location.pathname}/${lineReference}/premises/${premiseIndex}/statement/${path}/boundVariables/${index}`,
          type: 'PUT',
          data: newName,
          processData: false,
          'contentType': 'text/plain'
        }).then(
          response => {
            boundVariableElement.parents(".editablePremise").html(response.html);
            $("#editBoundVariableModal").modal("hide");
          },
          response => showTemporaryTooltip($("#saveBoundVariableNameButton"), response.responseJSON)
        );
      });
      $("#editBoundVariableModal").modal("show");
    })
  }

  $(document).on("click", "button.proveStatement", function() {
    let button = $(this);
    let reference = button.attr("data-reference");
    let statementHtml = button.attr("data-statement-html");

    $("#statementToProve").html(statementHtml);

    let inferenceSuggestions = new Bloodhound({
      datumTokenizer: Bloodhound.tokenizers.obj.whitespace("value"),
      queryTokenizer: Bloodhound.tokenizers.whitespace,
      remote: {
        url: `${window.location.pathname}/${reference}/suggestions?searchText=%QUERY`,
        wildcard: "%QUERY"
      }
    });
    $("#inferenceName")
      .typeahead("destroy")
      .typeahead(null, {
        display: x => x.inference.name,
        source: inferenceSuggestions
      })
      .bind("typeahead:select", (event, suggestion) => {
        let button = $("#proveStatementSubmitButton");
        button
          .off("click")
          .on("click", () => {
            if (suggestion.substitutions.length === 1) {
              $.ajax({
                url: `${window.location.pathname}/${reference}`,
                type: 'PUT',
                data: JSON.stringify({
                  inferenceId: suggestion.inference.id,
                  substitutions: _.mapValues(suggestion.substitutions[0], x => _.mapValues(x, y => y.serialized))
                }),
                contentType: "application/json; charset=utf-8"
              }).then(
                () => location.reload(true),
                response => showTemporaryTooltip(button, response.responseJSON)
              );
            } else {
              showTemporaryTooltip(button, "No unique substitution");
            }
          });
      });

    if (proofLineWithOpenPopover) {
      proofLineWithOpenPopover.popover("hide");
    }
  });
});
