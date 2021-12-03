function (element) {
  var tooltip = $('<div>').addClass('itrex-tooltip').css({
    opacity: 0,
    position: 'fixed',
    left: '50px',
    top: '400px',
    width: '30%',
    'min-width': '250px',
    'z-index': 999999,
  })
  $('#' + element.id + ' .svg-container').append(tooltip)

  // https://plotly.com/javascript/hover-events/
  element.on('plotly_hover', function (data) {
    var point = data.points[0];
    drug_index = point.data.customdata.indexOf(point.y)
    drug_image = point.data.customdata[drug_index + 1]
    tooltip.html('<img src="' + drug_image + '" width="100%">');

    // Fade in the image
    tooltip.stop(true).animate({ opacity: 1 }, 300)
  });

  element.on('plotly_unhover', function (_) {
    // Fade out the image
    tooltip.stop(true).animate({ opacity: 0 }, 500)
  });
}
