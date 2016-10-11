function setStage($, stage) {
  $('#step-wizzard li').addClass('disabled').removeClass('active');
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
  $(id).removeClass('has-error');
  $(id).find('.help-block').remove();
}(jQuery);

function addErrorMarkup($, id, err) {
  $(id).addClass('has-error');
  $(id).find('.help-block').remove();
  $(id + ' .input-group').after('<span class="help-block" aria-live="assertive" aria-relevant="additions removals">' + err + '</span>');
}(jQuery);
    
function addValidationMarkup($, id, isValid, err) {
  if (isValid === 'false')
  	addErrorMarkup($, id, err);
  else
  	removeErrorMarkup($, id);
}(jQuery);

function displayPasswordStrength($, $item) {
	var formGroup = $item.parent().parent().parent();
	var strength = zxcvbn($item.val(), user_inputs=[]);
	var pct = 0;
	var colour = "danger";
	var status = "Very Weak";
    var isValid = 'false';

	switch (strength.score) {
		case 0:
		  pct = 10;
          colour = "danger";
          status = "Very Weak";
          break;
        case 1:
		  pct = 25;
          colour = "warning";
          status = "Weak";
          break;
        case 2:
		  pct = 50;
          colour = "info";
          status = "Fair";
          break;
        case 3:
		  pct = 75;
          colour = "primary";
          status = "Good";
          break;
        case 4:
		  pct = 100;
          colour = "success";
          status = "Strong";
          break;
	}

	formGroup.find('.progress-bar').removeClass().addClass('progress-bar').addClass('progress-bar-' + colour).attr('aria-valuenow', pct).attr('style', 'width: ' + pct + '%; min-width: 40px;').find('span').text(status);
    formGroup.find('.strength-warning').removeClass().addClass('text-center').addClass('text-' + colour)

    if (pct >= 50)
    	isValid = 'true';

    addValidationMarkup($, '#' + formGroup.attr('id'), isValid, strength.feedback.warning);
}(jQuery);