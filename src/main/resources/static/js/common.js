
function showTemporaryTooltip(parent, text) {
  if (typeof text === "object") {
    text = text.error + ": " + text.message;
  }
  parent.tooltip({
    placement: "bottom",
    title: text,
    trigger: "manual"
  });
  parent.tooltip("show");
  setTimeout(() => parent.tooltip("hide"), 5000);
}