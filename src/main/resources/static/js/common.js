
function showTemporaryTooltip(parent, text) {
  parent.tooltip({
    placement: "bottom",
    title: text,
    trigger: "manual"
  });
  parent.tooltip("show");
  setTimeout(() => parent.tooltip("hide"), 5000);
}