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

function displayPasswordStrength($) {
	var pwd = $('.question-input:first').val();
	var progressBar = $('#strengthBar');
	var score = zxcvbn(pwd, user_inputs=[]).score
	var pct = 0;
	var colour = "progress-bar-danger";
	var status = "Very Weak";

	switch (score) {
		case 0:
		  pct = 10;
          colour = "progress-bar-danger";
          status = "Too guessable";
          break;
        case 1:
		  pct = 25;
          colour = "progress-bar-warning";
          status = "Very guessable";
          break;
        case 2:
		  pct = 50;
          colour = "progress-bar-info";
          status = "Somewhat guessable";
          break;
        case 3:
		  pct = 75;
          colour = "progress-bar-primary";
          status = "Safely unguessable";
          break;
        case 4:
		  pct = 100;
          colour = "progress-bar-success";
          status = "Very unguessable";
          break;
	}

	progressBar.removeClass();
	progressBar.addClass('progress-bar').addClass(colour);
    progressBar.attr('aria-valuenow', pct);
    progressBar.attr('style', 'width: ' + pct + '%; min-width: 40px;');
    progressBar.find('span').text(status);
}(jQuery);