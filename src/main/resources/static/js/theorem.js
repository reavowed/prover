$(() => {
  $("[data-reference]").each(function () {
    let $this = $(this);
    let escapedReference = _.replace($this.attr("data-reference"), /\./g, "\\.");
    let $premises = $(".highlight-" + escapedReference);
    let $conclusion = $this.find('.conclusion-' + escapedReference);
    $this
      .on("mouseenter", function () {
        $premises.addClass("highlightPremise");
        $conclusion.addClass("highlightConclusion");
      })
      .on("mouseleave", function () {
        $premises.removeClass("highlightPremise");
        $conclusion.removeClass("highlightConclusion");
      });
  });

  let openPopoverHolder = null;

  $(".proofLine").each(function() {
    let $this = $(this);
    let holder = $this.find(".popover-holder");
    holder.popover({
      placement: "bottom",
      html: true,
      trigger: "focus"
    });
    $this
      .on("click", e => {
        if ($(e.target).parents(".popover").length) {
          return;
        }
        if (openPopoverHolder && openPopoverHolder !== holder) {
          openPopoverHolder.popover("hide");
        }
        holder.popover("toggle");
        e.stopPropagation();
      })
      .on("show.bs.popover", function() {
        openPopoverHolder = holder;
      })
      .on("hide.bs.popover", function() {
        if (openPopoverHolder === holder) {
          openPopoverHolder = null;
        }
      });
  });
  $("body").on("click", e => {
      // Don't hide the popover if we're clicking ON it
      if ($(e.target).parents(".popover").length) {
        return;
      }
      if (openPopoverHolder) {
        openPopoverHolder.popover("hide");
      }
    });

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

    if (openPopoverHolder) {
      openPopoverHolder.popover("hide");
    }
  });
});
