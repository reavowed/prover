$(() => {
  $("button.editShorthand").click(function() {
    let button = $(this);
    let key = button.attr("data-key");
    let currentShorthand = button.attr("data-shorthand");
    $('#editShorthandModalAlert').hide();
    $("#entryDescriptionForShorthand").text(key);
    $("#shorthandInput").val(currentShorthand);
    $("#saveShorthandButton").off("click").on("click", () => {
      let newShorthand = $("#shorthandInput").val()
      $.ajax({
          url: window.location.pathname + `/${key}/shorthand`,
          type: "PUT",
          data: newShorthand,
          processData: false,
          'contentType': 'text/plain'
      })
      .then(
        () => {
          $('#editShorthandModal').modal('hide');
          button.attr("data-shorthand", newShorthand);
        },
        () => {
          $('#editShorthandModalAlertContent').text("Error updating shorthand");
          $('#editShorthandModalAlert').show();
        }
      );
    });
  });
});
