const rgbToHex = (r, g, b) => {
  function cToHex(c) {
    var hex = c.toString(16);
    return hex.length == 1 ? "0" + hex : hex;
  }
  return `${cToHex(r)}${cToHex(g)}${cToHex(b)}`;
}

window.addEventListener('load', (evt) => {
  const getLegendColor = (n) => {
    const style = getComputedStyle(document.documentElement);

    const r = parseInt(style.getPropertyValue(`--legend-${n}-r`));
    const g = parseInt(style.getPropertyValue(`--legend-${n}-g`));
    const b = parseInt(style.getPropertyValue(`--legend-${n}-b`));

    return rgbToHex(r, g, b);
  };

  const getPaletteColor = (v1, v2) => {
    const style = getComputedStyle(document.documentElement);

    const r = parseInt(style.getPropertyValue(`--${v1}-${v2}-r`));
    const g = parseInt(style.getPropertyValue(`--${v1}-${v2}-g`));
    const b = parseInt(style.getPropertyValue(`--${v1}-${v2}-b`));

    return rgbToHex(r, g, b);
  };

  const head = document.querySelector('head');
  const body = document.querySelector('body');

  // const overlayContainer = document.createElement('div');
  // overlayContainer.classList.add('overlay-container');
  // body.appendChild(overlayContainer);

  const navbar = document.querySelector('.navbar-default');
  const navbarBrand = document.querySelector('.navbar-static-top span.navbar-brand');
  const navbarLinks = document.querySelector('.navbar-static-top ul#sus_page');
  const navbarFixed = document.querySelector('.navbar-static-top div.navbar-fixed');
  const navbarCollapse = document.querySelector('.navbar-static-top div.navbar-collapse');

  const navbarShadow = document.createElement('div');
  navbarShadow.classList.add('navbar-shadow');
  navbar.parentElement.insertBefore(navbarShadow, navbar);

  var condition = false;

  function reflow(elt){
    if (elt.offsetHeight > 0 && condition) {
      console.info(elt.offsetHeight);
    }
    // console.log(elt.offsetHeight);
  }

  const collapsed = navbarCollapse.classList.contains('collapse');
  
  if (collapsed) {
    navbarCollapse.classList.remove('collapse');
    reflow(navbarCollapse);
  }

  function sumWidths(elts) {
    var w = 0;
    for(var i = 0; i < elts.length; i++) {
      w += elts[i].clientWidth;
    }
    return w;
  }

  const linksMinWidth = Math.ceil(sumWidths(navbarLinks.querySelectorAll(':scope > li > a'))); 
  const breakpoint = Math.ceil(navbarBrand.clientWidth + linksMinWidth + navbarFixed.clientWidth);
  const bodyMinWidth = Math.ceil(navbarBrand.clientWidth + navbarFixed.clientWidth + 54);

  if (collapsed) {
    navbarCollapse.classList.add('collapse');
    reflow(navbarCollapse);
  }

  var style = document.createElement('style');
  style.innerText =
`body {
  min-width: calc(max(300px, ${bodyMinWidth}px));
}
.navbar-default {
  min-width: calc(max(300px, ${bodyMinWidth}px));
}
.navbar-collapse > ul > li > a {
  width: 100%;
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
  .collapse {
    display: none;
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

  const navbarHeight = parseFloat(getComputedStyle(document.documentElement).getPropertyValue('--h-navbar'));

  // console.log('navbarHeight:' + navbarHeight);

  window.setInterval(() =>
  {
    const activeTab = targetNode.querySelector('.tab-pane.active');
    const activeTabName = activeTab.getAttribute('data-value');
    const activeTabClassName = `user-tab-${activeTabName}`;
    if (!body.classList.contains(activeTabClassName)) {
      for(var i = 0; i < body.classList.length; i++) {
        if (body.classList[i].startsWith('user-tab-')) {
          // console.log(`Old active tab: ${body.classList[i].substring(9)} (polled)`);
          body.classList.remove(body.classList[i]);
          i--;
        }
      }
      body.classList.add(activeTabClassName);
      // console.log(`New active tab: ${activeTabName} (polled)`);
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
  const activeTabCallback = function(mutationsList, observer) {
    // Use traditional 'for loops' for IE 11
    for(const mutation of mutationsList) {
      if (mutation.type === 'attributes' && mutation.target.parentElement === targetNode && mutation.attributeName === 'class') {
        if (mutation.target.classList.contains('active')) {
          const tab = mutation.target.getAttribute('data-value');
          // console.log(`New active tab: ${tab}`);
          const tabClass = `user-tab-${tab}`;
          body.classList.add(tabClass);
        } else {
          const tab = mutation.target.getAttribute('data-value');
          // console.log(`Old active tab: ${tab}`);
          const tabClass = `user-tab-${tab}`;
          body.classList.remove(tabClass);
        }
      }
    }
  };

  // Create an observer instance linked to the callback function
  const activeTabObserver = new MutationObserver(activeTabCallback);

  // Start observing the target node for configured mutations
  activeTabObserver.observe(targetNode, config);

  const dropdowns = document.querySelectorAll('.tab-content .dropdown');

  for(var i = 0; i < dropdowns.length; i++) {
    const dropdown = dropdowns[i];
    const menu = dropdown.querySelector('.dropdown-menu');
    dropdowns[i].addEventListener('click', () => {
      const opening = !dropdown.classList.contains('open');
      if (opening) {
        const parent = menu.parentElement;
        const rect = parent.getBoundingClientRect();
        console.log(rect);
        const x = (rect.left + rect.right) / 2;
        const y = (rect.bottom + rect.top) / 2;
        var top = undefined;
        var left  = undefined;
        var bottom = undefined;
        var right = undefined;
        if (x > window.innerWidth / 2) {
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
        menu.style = `top: ${top}; left: ${left}; bottom: ${bottom}; right: ${right};`;
      }
    });
  }

  const scrolls = document.querySelectorAll('.sus-scroll');

  // console.log('scrolls: ' + scrolls.length);

  for(var i = 0; i < scrolls.length; i++) {
    const scroll = scrolls[i];
    const content = scroll.querySelector('.sus-scroll-content');

    const bg = document.createElement('div');
    bg.classList.add('sus-scrollbar');
    
    const handle = document.createElement('div');
    handle.classList.add('sus-scrollbar-handle');

    scroll.appendChild(bg);
    scroll.appendChild(handle);

    const id = `sus-scroll-${i}`;
    scroll.id = id;
    
    content.addEventListener('pointerdown', () => {
      // console.log('#' + scroll.id + ' content pressed');
    });
    content.addEventListener('pointerup', () =>{
      // console.log('#' + scroll.id + ' content released');
    });
    content.addEventListener('wheel', () => {
      // console.log('#' + scroll.id + ' content scrolled');
    });

    handle.addEventListener('pointerdown', () => {
      // console.log('#' + scroll.id +  ' handle pressed');
    });
    handle.addEventListener('pointerup', () =>{
      // console.log('#' + scroll.id + ' handle released');
    });

    const updateScroll = () => {
      const enabled = content.offsetHeight > scroll.offsetHeight;
      // if (scroll.classList.contains('sus-scroll-enabled') != enabled) {
      //   if (enabled) {
      //     scroll.classList.add('sus-scroll-enabled');
      //   } else {
      //     scroll.classList.remove('sus-scroll-enabled');
      //   }
      // }
      if (enabled) {
        scroll.style = `overflow-y: scroll;`;
      } else {
        scroll.style = 'overflow-y: hidden;';
      }
      if (enabled) {
        handle.style = `height: calc(100% * ${scroll.offsetHeight / content.offsetHeight})`;
      }
    };

    var throttle = false;

    // Callback function to execute when mutations are observed
    const scrollCallback = function(mutationsList, observer) {
      if (throttle) {
        return;
      } else {
        updateScroll();
        throttle = true;
        setTimeout(() =>{
          throttle = false;
        }, 20);
      }
    };

    // Create an observer instance linked to the callback function
    const scrollObserver = new MutationObserver(scrollCallback);

    // Start observing the target node for configured mutations
    scrollObserver.observe(scroll, config);
  }

  const links = document.querySelectorAll('a');
  // console.log('links: ' + links.length);
  for(var i = 0; i < links.length; i++) {
    const link = links[i];
    if (link.hasAttribute('href')) {
      const href = link.getAttribute('href');
      if (href.startsWith('#')) {
        if (href.startsWith('#tab')) {
          // console.log('tab link: ' + href);
        } else if (href.length > 1) {
          const target = document.getElementById(href.substring(1));
          if (target != null) {
            // console.log('scroll link: ' + href);
            link.addEventListener('click', (evt) => {
              evt.preventDefault();
              const y = target.getBoundingClientRect().y + window.scrollY;
              // console.log('scrolling to ' + href + ` (${y})`);
              window.scroll({
                top: y,
                left: 0,
                behavior: 'smooth'
              });
            });
          }
        }
      } else {
        // console.log('outbound link: ' + href);
      }
    }
  }
  });
