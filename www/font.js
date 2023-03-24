let initialSize;
let isIncreased = false;
let isDarkMode = false;

function changeFontSize() {
  const elements = document.querySelectorAll('*');
  if (!initialSize) {
    initialSize = parseInt(window.getComputedStyle(elements[0]).fontSize);
  }
  for (let i = 0; i < elements.length; i++) {
    let fontSize = parseInt(window.getComputedStyle(elements[i]).fontSize);
    if (isIncreased) {
      elements[i].style.fontSize = (fontSize + 2) + 'px';
    } else {
      elements[i].style.fontSize = (fontSize - 2) + 'px';
    }
  }
  isIncreased = !isIncreased;
}

function toggleDarkMode() {
  const elements = document.querySelectorAll('*');
  for (let i = 0; i < elements.length; i++) {
    if (!isDarkMode) {
      elements[i].style.color = 'black';
    } else {
      elements[i].style.color = '';
    }
  }
  isDarkMode = !isDarkMode;
}

changeFontSize();
document.getElementById('font-size-btn').addEventListener('click', changeFontSize);
document.getElementById('dark-mode-btn').addEventListener('click', toggleDarkMode);
