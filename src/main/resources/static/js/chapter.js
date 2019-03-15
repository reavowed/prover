$(() => {
  let shorthands = {};
  $.ajax({type: "GET", url: "/shorthands"}).then(data => shorthands = data);

  $("button.editShorthand").click(function() {
    let button = $(this);
    let key = button.attr("data-key");
    let currentShorthand = button.attr("data-shorthand");
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
        response => showTemporaryTooltip(button, response.responseJSON)
      );
    });
  });

  $("button.deleteInference").click(function() {
    let button = $(this);
    let key = button.attr("data-key");
    $.ajax({
      url: window.location.pathname + `/${key}`,
      type: "DELETE"
    })
    .then(
      () => {
        button.parents(".result").remove();
      },
      response => showTemporaryTooltip(button, response.responseJSON)
    );
  });

  $("#addTheoremButton").click(function() {
    $.ajax({
      url: window.location.pathname + '/theorems',
      type: 'POST',
      data: JSON.stringify({
        name: $('#theoremName').val(),
        premises: _.filter($('#theoremPremises').val().split(/\r?\n/), string => string.length > 0),
        conclusion: $('#theoremConclusion').val()
      }),
      contentType: "application/json; charset=utf-8"
    }).then(
      () => {
        $('#addTheoremModal').modal('hide');
        $('#theoremName').val("");
        $('#theoremPremises').val("");
        $('#theoremConclusion').val("");
        location.reload(true);
      },
      response => showTemporaryTooltip($("#addTheoremButton"), response.responseJSON)
    );
  });

  function replaceShorthandsInText(text) {
    _.each(_.keys(shorthands), valueToReplace => {
      let regex = new RegExp('\\b' + _.escapeRegExp(valueToReplace) + '(?=\\s$)', 'gim');
      text = text.replace(regex, shorthands[valueToReplace]);
    });
    return text;
  }
  $('.replaceShorthands').each(function() {
    let input = $(this);
    let previousStart, previousEnd;

    input.keydown(function(event) {
      previousStart = event.target.value.substring(0, event.target.selectionStart);
      previousEnd = event.target.value.substring(event.target.selectionStart);
    });

    function replaceShorthandsAfterEvent(event) {
      let start = event.target.value.substring(0, event.target.selectionStart);
      let end = event.target.value.substring(event.target.selectionStart);
      if (start.length > 0 &&start.slice(0, -1) === previousStart && end === previousEnd) {
        let replacedStart = replaceShorthandsInText(start);
        if (replacedStart !== start) {
          event.target.value = replacedStart + end;
          event.target.selectionStart = replacedStart.length;
          event.target.selectionEnd = replacedStart.length;
        }
      }
    }

    input.keyup(replaceShorthandsAfterEvent);
    input.blur(replaceShorthandsAfterEvent);
  });
});
