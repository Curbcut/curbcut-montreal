const tooltipBox = document.querySelector('.tooltip-box');
const closeButton = document.querySelector('.close-button');
document.window.alert("work");
tooltipBox.classList.add('show');

closeButton.addEventListener('click', () => {
  tooltipBox.classList.remove('show');


  setTimeout(function () {
    modal.classList.remove('modal--show');
  }, 5000);
});
