// Create a MutationObserver to observe changes in the active tab
const activeTabObserver = new MutationObserver(function(mutations) {
  mutations.forEach(function(mutation) {
    // Check if the active tab has changed
    if (mutation.type === 'attributes' && mutation.attributeName === 'class' && mutation.target.classList.contains('active')) {
      // Code to execute when the active tab changes
      const buttons = document.querySelectorAll('.btn dropdown-toggle .btn-default .dropdown .bootstrap-select .form-control .shinyjs-resettable .shiny-bound-input .bs3 .open');
      buttons.forEach(function(button) {
        const menu = button.nextElementSibling;
        button.addEventListener('click', () => {
          if (button.getAttribute('aria-expanded') === "true") {
            const parent = menu.parentElement;
            const rect = parent.getBoundingClientRect();
            const x = (rect.left + rect.right) / 2;
            const y = (rect.bottom + rect.top) / 2;
            var top = undefined;
            var left  = undefined;
            var bottom = undefined;
            var right = undefined;
            if (x > 200) {
              left = 'unset';
              right = `${Math.ceil( window.innerWidth - rect.right )}px`;
            } else {
              left = `${Math.ceil( rect.left )}px`;
              right = 'unset';
            }
            if (y > window.innerHeight / 2) {
              top = 'unset';
              bottom = `${Math.ceil( window.innerHeight - rect.top )}px`;
            } else {
              top = `${Math.ceil( rect.bottom )}px`;
              bottom = 'unset';
            }
            menu.style = `top: ${top}; left: ${left}; bottom: ${bottom}; right: ${right}; display:fixed; z-index`;
          }
        });
      });
    }
  });
});

// Configure the observer to watch for changes to the class attribute of the tab element
const config = { attributes: true, attributeFilter: ['class'] };

// Start observing the active tab
const targetNode = document.querySelector('.tab.active');
activeTabObserver.observe(targetNode, config);
