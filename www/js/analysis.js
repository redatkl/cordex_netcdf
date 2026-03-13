$(document).on('click', '.upload-dropzone', function() {
  $('#' + $(this).attr('for') + ' input[type="file"]').click();
});