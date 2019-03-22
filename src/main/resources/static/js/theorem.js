$(() => {
  function findLastChild(element) {
    let lastChild = element.parent().children(".children").children(".proofStep:last-of-type").children(".proofLine");
    return lastChild.is("[data-reference-for-last-child]") ? findLastChild(lastChild) : lastChild;
  }
  function findElementByReference(lineReference, pathReference) {
    let outerElement = $(`[data-reference='${lineReference}'], [data-premise='${lineReference}']`);
    outerElement = outerElement.length ? outerElement : findLastChild($(`[data-reference-for-last-child='${lineReference}']`));
    outerElement = outerElement.find(".conclusion").length ? outerElement.find(".conclusion") : outerElement;
    return pathReference.length ? outerElement.find(`[data-path='${pathReference.join(".")}']`) : outerElement;
  }

  function bindReferences() {
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
  }
  bindReferences();

  let proofLineWithOpenPopover = null;

  $(".proofLine").each(function() {
    let proofLine = $(this);
    proofLine.popover({
      placement: "bottom",
      html: true,
      trigger: "manual"
    });
    proofLine
      .on("click", e => {
        if ($(e.target).parents(".popover").length) {
          return;
        }
        if (proofLineWithOpenPopover && proofLineWithOpenPopover !== proofLine) {
          proofLineWithOpenPopover.popover("hide");
        }

        if (proofLine.attr("data-editable")) {
          getOptionsAndBindPopover(proofLine, true);
        } else {
          proofLine.popover("show");
        }
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

  function getOptionsAndBindPopover(proofLine, showPopover) {
    let lineReference = proofLine.attr("data-reference");
    return $.ajax({
      url: `${window.location.pathname}/${lineReference}/premiseOptions`,
      type: 'GET'
    }).then(
      response => {
        if (showPopover) proofLine.popover("show");
        bindPopover(proofLine, lineReference, response);
      },
      response => showTemporaryTooltip(proofLine, response.responseJSON)
    );
  }

  function bindPopover(proofLine, lineReference, premiseOptions) {

    function updateStep(htmlResponse) {
      let newStepElement = $(htmlResponse.html);
      let newPopoverTitle = newStepElement.children(".proofLine").attr("data-title");
      let newPopoverContent = newStepElement.children(".proofLine").attr("data-content");
      popoverElement.children(".popover-title").html(newPopoverTitle);
      popoverElement.children(".popover-content").html(newPopoverContent);
      getOptionsAndBindPopover(proofLine, false);
    }

    let popoverElement = proofLine.siblings(".popover");

    _.each(popoverElement.find(".editablePremise"), rawElement => {
      let premiseLine = $(rawElement);
      let path = premiseLine.attr("data-path");

      let createTargetButton = $("<button class='btn btn-success'>Create target</button>");
      createTargetButton.on("click", () => {
        $.ajax({
          url: `${window.location.pathname}/${lineReference}/premises/${premiseOption.path.join(".")}/target`,
          type: 'POST',
        }).then(
          response => {
            let newProof = $(response);
            $(".proof").html(newProof.html());
            bindReferences();
          },
          response => showTemporaryTooltip(createTargetButton, response.responseJSON)
        );
      });
      premiseLine.append(createTargetButton);

      let premiseOption = _.find(premiseOptions, x => x.path.join(".") === path);
      if (premiseOption && premiseOption.expansions.length) {
        let dropdownButton = $("<button class='btn btn-success' data-toggle='dropdown'>Expansions</button>");
        let dropDownList = $("<ul class='dropdown-menu'></ul>");
        _.each(premiseOption.expansions, expansion => {
          let link = $(`<a href="#">${expansion.name}</a>`);
          dropDownList.append($("<li></li>").append(link));
          link.on("click", (e) => {
            e.preventDefault();
            $.ajax({
              url: `${window.location.pathname}/${lineReference}/premises/${premiseOption.path.join(".")}/rearrangement`,
              type: 'POST',
              data: expansion.id,
              processData: false,
              'contentType': 'text/plain'
            }).then(
              updateStep,
              response => showTemporaryTooltip(dropdownButton, response.responseJSON)
            );
          });
        });
        let dropDownContainer = $("<span class='dropdown dynamicButton'></span>")
          .append(dropdownButton)
          .append(dropDownList);
        premiseLine.append(dropDownContainer);
      }
    });

    popoverElement.off("click.editBoundVariable").on("click.editBoundVariable", ".editablePremise .boundVariable", function() {
      let boundVariableElement = $(this);
      let premisePath = boundVariableElement.parents(".editablePremise").attr("data-path");
      let index = boundVariableElement.attr("data-index");
      let parentWithPath = boundVariableElement.parents("[data-path]").eq(0);
      let path = parentWithPath ? parentWithPath.attr("data-path") : "";
      let currentName = boundVariableElement.text();
      $("#boundVariableNameInput").val(currentName);
      $("#saveBoundVariableNameButton").off("click").on("click", () => {
        let newName =  $("#boundVariableNameInput").val();
        $.ajax({
          url: `${window.location.pathname}/${lineReference}/premises/${premisePath}/statement/${path}/boundVariables/${index}`,
          type: 'PUT',
          data: newName,
          processData: false,
          'contentType': 'text/plain'
        }).then(
          response => {
            updateStep(response);
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
    let reference = button.parents("[data-reference]").eq(0).attr("data-reference");
    let statementHtml = button.attr("data-statement-html");

    $("#statementToProve").html(statementHtml);

    let inferenceSuggestions = new Bloodhound({
      datumTokenizer: Bloodhound.tokenizers.obj.whitespace("value"),
      queryTokenizer: Bloodhound.tokenizers.whitespace,
      remote: {
        url: `${window.location.pathname}/${reference}/suggestInferences?searchText=%QUERY`,
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
