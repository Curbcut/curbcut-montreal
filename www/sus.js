window.addEventListener('load', (evt) => {
  const head = document.querySelector('head');
  const body = document.querySelector('body');

  const navbar = document.querySelector('.navbar-default');
  const navbarBrand = document.querySelector('.navbar-static-top span.navbar-brand');
  const navbarLinks = document.querySelector('.navbar-static-top ul#sus_page');
  const navbarFixed = document.querySelector('.navbar-static-top div.navbar-fixed');

  const breakpoint = Math.ceil(navbarBrand.clientWidth + navbarLinks.clientWidth + navbarFixed.clientWidth);
  const bodyMinWidth = Math.ceil(navbarBrand.clientWidth + navbarFixed.clientWidth + 54);

  var style = document.createElement('style');
  style.innerText =
`body {
  min-width: calc(max(300px, ${bodyMinWidth}px));
}
.navbar-default {
  min-width: calc(max(300px, ${bodyMinWidth}px));
}
@media (max-width: ${breakpoint}px) {
  .navbar-header {
    float: none;
  }
  .navbar-left,.navbar-right {
    float: none !important;
  }
  .navbar-toggle {
    display: block;
  }
  .navbar-collapse {
    border-top: 1px solid transparent;
    box-shadow: inset 0 1px 0 rgba(255,255,255,0.1);
  }
  .navbar-fixed-top {
    top: 0;
    border-width: 0 0 1px;
  }
  .navbar-collapse.collapse {
    display: none!important;
  }
  .navbar-nav {
    float: none!important;
    margin-top: 7.5px;
  }
  .navbar-nav>li {
    float: none;
  }
  .navbar-nav>li>a {
    padding-top: 10px;
    padding-bottom: 10px;
  }
  .collapse.in{
    display:block !important;
  }
  .navbar-fixed {
    margin-right: 52px
  }
}

@media (max-height: 900px) {
  .sus_sidebar .small_map img {
    display: none !important;
  }
}
`;

  head.appendChild(style);

  const navbarHeight = parseFloat(getComputedStyle(document.documentElement).getPropertyValue('--navbar-height'));

  console.log(navbarHeight);

  window.setInterval(() =>
  {
    const activeTab = targetNode.querySelector('.tab-pane.active');
    const activeTabName = activeTab.getAttribute('data-value');
    const activeTabClassName = `user-tab-${activeTabName}`;
    if (!body.classList.contains(activeTabClassName)) {
      for(var i = 0; i < body.classList.length; i++) {
        if (body.classList[i].startsWith('user-tab-')) {
          console.log(`Old active tab: ${body.classList[i].substring(9)} (polled)`);
          body.classList.remove(body.classList[i]);
          i--;
        }
      }
      body.classList.add(activeTabClassName);
      console.log(`New active tab: ${activeTabName} (polled)`);
    }
    const userScroll = body.getBoundingClientRect().top < -10;
    const userScrollClassName = 'user-scroll';
    if (userScroll != body.classList.contains(userScrollClassName)) {
      body.classList.remove(userScrollClassName);
      if (userScroll) {
        body.classList.add(userScrollClassName);
      }
    }
  }, 50);

  window.setTimeout(() =>{
    navbar.classList.add('loaded');
  }, 150);

  // Select the node that will be observed for mutations
  const targetNode = document.querySelector('.tab-content');

  // Options for the observer (which mutations to observe)
  const config = { attributes: true, childList: true, subtree: true };

  // Callback function to execute when mutations are observed
  const callback = function(mutationsList, observer) {
    // Use traditional 'for loops' for IE 11
    for(const mutation of mutationsList) {
      if (mutation.type === 'attributes' && mutation.target.parentElement === targetNode && mutation.attributeName === 'class') {
        if (mutation.target.classList.contains('active')) {
          const tab = mutation.target.getAttribute('data-value');
          console.log(`New active tab: ${tab}`);
          const tabClass = `user-tab-${tab}`;
          body.classList.add(tabClass);
        } else {
          const tab = mutation.target.getAttribute('data-value');
          console.log(`Old active tab: ${tab}`);
          const tabClass = `user-tab-${tab}`;
          body.classList.remove(tabClass);
        }
      }
    }
  };

  // Create an observer instance linked to the callback function
  const activeTabObserver = new MutationObserver(callback);

  // Start observing the target node for configured mutations
  activeTabObserver.observe(targetNode, config);
});
