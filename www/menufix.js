$(function() {
  $('.btn-group .btn dropdown-toggle .btn-default .dropdown .bootstrap-select .form-control .shinyjs-resettable .shiny-bound-input .bs3 .open').on('show.bs.dropdown', function () {
    var buttonOffset = $(this).offset().left;
    var dropdownWidth = $(this).find('.dropdown-menu').outerWidth();
    var windowWidth = $(window).width();
    var dropdownMenu = $(this).find('.dropdown-menu');

    if (buttonOffset + dropdownWidth > windowWidth) {
      dropdownMenu.removeClass('dropdown-menu-right');
      dropdownMenu.addClass('dropdown-menu-left');
    } else {
      dropdownMenu.removeClass('dropdown-menu-left');
      dropdownMenu.addClass('dropdown-menu-right');
    }
  });
});
