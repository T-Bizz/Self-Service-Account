function setStage($, stage) {
  $('.step-heading').addClass('disabled').removeClass('active');
  $('#li-step-' + stage).removeClass('disabled').addClass('active');
}(jQuery);