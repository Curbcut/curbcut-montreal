const tooltip = document.querySelector('.tooltip');

function setTooltipContent(element, content) {
  element.setAttribute('data-tooltip', content);
}

setTooltipContent(tooltip, 'Tooltip content for element 1');
