function setStage($, stage) {
  $('.step-heading').addClass('disabled').removeClass('active');
  $('#li-step-' + stage).removeClass('disabled').addClass('active');
}(jQuery);

function showModal($, title, description) {
  $(".modal-general .modal-title-text").html(title); 
  $(".modal-general .modal-text").html(description); 
  $(".modal-general").modal("show");
}(jQuery);

function showModalError($, title, description) {
  $(".modal-error .modal-title-text").html(title); 
  $(".modal-error .modal-text").html(description); 
  $(".modal-error").modal("show");
}(jQuery);

function focusFirstInput($, id) {
  $('#' + id).find('*').filter(':input:visible:first');
}(jQuery);

function removeErrorMarkup($, id) {
  $('#' + id).removeClass('has-error');
  $('#' + id).find('.help-block').remove();
}(jQuery);

function addErrorMarkup($, id, err) {
  $('#' + id).addClass('has-error');
  $('#' + id).find('.help-block').remove();
  $('#' + id + ' .input-group').after('<span class="help-block" aria-live="assertive" aria-relevant="additions removals">' + err + '</span>');
}(jQuery);
    
function addValidationMarkup($, id, isValid, err) {
  if (isValid === 'false')
  	addErrorMarkup($, id, err);
  else
  	removeErrorMarkup($, id);
}(jQuery);