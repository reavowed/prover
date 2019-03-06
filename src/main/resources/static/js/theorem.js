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
});