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
    let holder = $this.find(".popover-holder")
    holder.popover({
      placement: "bottom",
      html: true,
      trigger: "focus"
    });
    $this
      .on("click", function () {
        if (openPopoverHolder && openPopoverHolder !== holder) {
          openPopoverHolder.popover("hide");
        }
        holder.popover("toggle");
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
});