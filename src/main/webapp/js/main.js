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

function displayPasswordStrength($) {
	var pwd = $('.question-input:first').val();
	var progressBar = $('#strength-bar');
	var warningText = $('#strength-warning');
	var strength = zxcvbn(pwd, user_inputs=[]);
	var pct = 0;
	var colour = "danger";
	var status = "Very Weak";

	switch (strength.score) {
		case 0:
		  pct = 10;
          colour = "danger";
          status = "Too guessable";
          break;
        case 1:
		  pct = 25;
          colour = "warning";
          status = "Very guessable";
          break;
        case 2:
		  pct = 50;
          colour = "info";
          status = "Somewhat guessable";
          break;
        case 3:
		  pct = 75;
          colour = "primary";
          status = "Safely unguessable";
          break;
        case 4:
		  pct = 100;
          colour = "success";
          status = "Very unguessable";
          break;
	}

	progressBar.removeClass();
	progressBar.addClass('progress-bar').addClass('progress-bar-' + colour);
    progressBar.attr('aria-valuenow', pct);
    progressBar.attr('style', 'width: ' + pct + '%; min-width: 40px;');
    progressBar.find('span').text(status);
    warningText.removeClass();
    warningText.addClass('text-center').addClass('text-' + colour)

    var isValid = 'false';
    if (pct >= 50)
    	isValid = 'true';

    addValidationMarkup($, '.form-group:first', isValid, strength.feedback.warning);
}(jQuery);