$(document).on('click', '.upload-dropzone', function(e) {
  e.preventDefault();
  e.stopPropagation();
  var inputId = $(this).data('target');
  $('#' + inputId).closest('.shiny-input-container').find('input[type="file"]').trigger('click');
});


