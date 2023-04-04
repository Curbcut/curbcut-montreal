function handleWindowResize() {
  handleDOMContentLoaded();
}

window.addEventListener('resize', handleWindowResize);

document.addEventListener('DOMContentLoaded', handleDOMContentLoaded);

function handleDOMContentLoaded() {
  mediaLimit = (window.innerWidth < 1600);
  const saviezSelection = document.querySelector('#housing-housing-dyk_contents');
  const widgetSection = document.querySelector('#housing-housing-widgets');
  const ulElements = document.querySelectorAll('#housing-housing-dyk_contents ul');
  const rightMenu = document.getElementById('housing-right_panel');
  const threshold = +window.innerWidth - (+getComputedStyle(rightMenu).getPropertyValue('--w-panel').replace('px', ''));

  widgetSection.addEventListener('click', showWidgets);

  document.body.addEventListener('mouseout', function (event) {
    if (mediaLimit && event.clientX < threshold) {
      hideWidgets();
    }
  });

  document.body.addEventListener('mouseover', function (event) {
    if (mediaLimit && event.clientX > threshold) {
      showWidgets();
    }
  });

  function showWidgets() {
    console.log(widgetSection);
    widgetSection.style.display = 'block';
    widgetSection.style.visibility = 'visible';
    saviezSelection.style.display = 'block';
    saviezSelection.style.visibility = 'visible';
    rightMenu.style.opacity = 1;
    for (const ul of ulElements) {
      ul.style.display = 'block';
      ul.style.visibility = 'visible';
    }
  }

  function hideWidgets() {
    widgetSection.style.display = 'none';
    widgetSection.style.visibility = 'hidden';
    saviezSelection.style.display = 'none';
    saviezSelection.style.visibility = 'hidden';
    rightMenu.style.opacity = .9;
  }
}