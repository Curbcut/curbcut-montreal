window.addEventListener('load', function() {
  var dropdownMenu = document.querySelector('.dropdown-menu');
  var xPosition = 0;

  document.addEventListener('mousemove', function(event) {
    xPosition = event.clientX;
    dropdownMenu.style.left = xPosition + 'px';
  });
});