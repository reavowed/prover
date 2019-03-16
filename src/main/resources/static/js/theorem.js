$(() => {
  let baseState = {
    transitions: [
      {
        selector: "[data-reference]",
        event: "mouseenter",
        newStateConstructor: highlightLineState
      }
    ]
  };

  function highlightLineState(line) {
    let escapedReference = _.replace(line.attr("data-reference"), /\./g, "\\.");
    let $premises = $(".highlight-" + escapedReference);
    let $conclusion = line.find('.conclusion-' + escapedReference);
    return {
      onEnter: () => {
        $premises.addClass("highlightPremise");
        $conclusion.addClass("highlightConclusion");
      },
      onExit: () => {
        $premises.removeClass("highlightPremise");
        $conclusion.removeClass("highlightConclusion");
      },
      transitions: [
        {
          selector: line,
          event: "mouseleave",
          newState: baseState
        },
        {
          selector: line,
          event: "click",
          newStateConstructor: popupLineState
        }
      ]
    };
  }

  function popupLineState(line) {
    let holder = line.find(".popover-holder");

    return {
      onEnter: () => {
        holder.popover({
          placement: "bottom",
          html: true,
          trigger: "focus"
        });
        holder.popover("show");
      },
      onExit: () => {
        holder.popover('destroy');
      },
      transitions: [
        {
          selector: document,
          event: "click",
          newState: baseState
        },
        {
          selector: "button.proveStatement",
          event: "click",
          newStateConstructor: proveStatementInferenceSelect
        }
      ]
    };
  }

  function proveStatementInferenceSelect(button) {
      let reference = button.attr("data-reference");
      let statementHtml = button.attr("data-statement-html");
      $("#statementToProve").html(statementHtml);

      return {
        onEnter: () => {
          $("#statementToProve").html(statementHtml);

          let inferenceSuggestions = new Bloodhound({
            datumTokenizer: Bloodhound.tokenizers.obj.whitespace("value"),
            queryTokenizer: Bloodhound.tokenizers.whitespace,
            remote: {
              url: `${window.location.pathname}/${reference}/suggestions?searchText=%QUERY`,
              wildcard: "%QUERY"
            }
          });

          $('#proveStatementModal').modal('show');
        },
        onExit: () => {
          $('#proveStatementModal').modal('hide');
          $("#statementToProve").html("");
        },
        transitions: [
        ]
      };
  }

  $(document).on("click", "button.proveStatement", function() {
    let button = $(this);
    let reference = button.attr("data-reference");
    let statementHtml = button.attr("data-statement-html");

    $("#statementToProve").html(statementHtml);

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

  let currentState = null;
  function enterState(newState) {
    function getNewState(transition, element) {
      return transition.newState || transition.newStateConstructor(element);
    }

    currentState && currentState.onExit && currentState.onExit();
    currentState && _.each(currentState.transitions, transition => {
      $(transition.selector).off(transition.event + ".state");
    });
    currentState = newState;
    currentState.onEnter && currentState.onEnter();
    _.each(currentState.transitions, transition => {
      $(transition.selector).on(transition.event + ".state", function(event) {
        event.stopPropagation();
        enterState(getNewState(transition, $(this)));
      });
    });
  }
  enterState(baseState);



  // $("[data-reference]").each(function () {
  //   let $this = $(this);
  //   let escapedReference = _.replace($this.attr("data-reference"), /\./g, "\\.");
  //   let $premises = $(".highlight-" + escapedReference);
  //   let $conclusion = $this.find('.conclusion-' + escapedReference);
  //   $this
  //     .on("mouseenter", function () {
  //       $premises.addClass("highlightPremise");
  //       $conclusion.addClass("highlightConclusion");
  //     })
  //     .on("mouseleave", function () {
  //       $premises.removeClass("highlightPremise");
  //       $conclusion.removeClass("highlightConclusion");
  //     });
  // });
  //
  // let openPopoverHolder = null;
  //
  // $(".proofLine").each(function() {
  //   let $this = $(this);
  //   let holder = $this.find(".popover-holder");
  //   holder.popover({
  //     placement: "bottom",
  //     html: true,
  //     trigger: "focus"
  //   });
  //   $this
  //     .on("click", e => {
  //       if ($(e.target).parents(".popover").length) {
  //         return;
  //       }
  //       if (openPopoverHolder && openPopoverHolder !== holder) {
  //         openPopoverHolder.popover("hide");
  //       }
  //       holder.popover("toggle");
  //       e.stopPropagation();
  //     })
  //     .on("show.bs.popover", function() {
  //       openPopoverHolder = holder;
  //     })
  //     .on("hide.bs.popover", function() {
  //       if (openPopoverHolder === holder) {
  //         openPopoverHolder = null;
  //       }
  //     });
  // });
  // $("body").on("click", e => {
  //     // Don't hide the popover if we're clicking ON it
  //     if ($(e.target).parents(".popover").length) {
  //       return;
  //     }
  //     if (openPopoverHolder) {
  //       openPopoverHolder.popover("hide");
  //     }
  //   });
});
