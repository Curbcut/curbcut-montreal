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
  const navbarLinks = document.querySelector('.navbar-static-top ul#cc_page');
  const navbarFixed = document.querySelector('.navbar-static-top div.navbar-fixed');
  const navbarCollapse = document.querySelector('.navbar-static-top div.navbar-collapse');
  const mainContainer = document.querySelector('body > .container-fluid');

  const collapseTopNav = () => {
    if (navbarCollapse.classList.contains('in')) {
      navbarCollapse.classList.remove('in');
      navbarCollapse.setAttribute('aria-expanded', 'false');
    }
  };

  mainContainer.addEventListener('click', () => {
    collapseTopNav();
  });

  const navbarTabLinks = document.querySelectorAll('.navbar-static-top .navbar-collapse ul li a[data-toggle="tab"]');

  for(var i = 0; i < navbarTabLinks.length; i++) {
    const navbarTabLink = navbarTabLinks[i];
    console.log(navbarTabLink.innerText);
    navbarTabLink.addEventListener('click', () => {
      collapseTopNav();
    });
  }

  const navbarShadow = document.createElement('div');
  navbarShadow.classList.add('navbar-shadow');
  navbar.parentElement.insertBefore(navbarShadow, navbar);

  const navbarShadowCaster = document.createElement('div');
  navbarShadowCaster.classList.add('navbar-shadow-caster');
  navbarShadow.appendChild(navbarShadowCaster);

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
  .navbar-collapse > ul#cc_page {
    box-shadow: var(--box-shadow-md);
    clip-path: inset(0px 0px -500px 0px);
  }
}

@media (max-height: 900px) {
  .sus_sidebar .small_map img {
    display: none !important;
  }
}
`;

  head.appendChild(style);

  const mapdeckDivs = document.querySelectorAll('.map_div');

  for (var i = 0; i < mapdeckDivs.length; i++) {
    const mapdeckDiv = mapdeckDivs[i];

    const shadow = document.createElement('div');
    shadow.classList.add('shadow-md-bottom');

    mapdeckDiv.appendChild(shadow);
  }

  const storiesMapdeckDiv = document.querySelector('.tab-pane[data-value="stories"] .map_div');

  storiesMapdeckDiv.addEventListener('click', () => {

  });

  const storiesBack = document.querySelector('#stories-back');
  const storiesContainer = document.querySelector('#stories-stories');
  const storiesTopBar = document.createElement('div');
  const storiesBackButton = document.createElement('button');
  const storiesCloseButton = document.createElement('button');

  const backIcon = document.createElement('span');
  backIcon.className = 'material-icons';
  backIcon.innerText = 'navigate_before';

  const closeIcon = document.createElement('span');
  closeIcon.className = 'material-icons';
  closeIcon.innerText = 'close';

  storiesBackButton.appendChild(backIcon);
  storiesCloseButton.appendChild(closeIcon);

  storiesBackButton.addEventListener('click', () => storiesBack.click());
  storiesCloseButton.addEventListener('click', () => storiesBack.click());
  
  storiesTopBar.appendChild(storiesBackButton);
  storiesTopBar.appendChild(storiesBack);
  storiesTopBar.appendChild(storiesCloseButton);
  storiesTopBar.id = 'stories-back';
  storiesTopBar.style.display = 'none';

  storiesContainer.parentElement.appendChild(storiesTopBar);
  
  
  
  // PLACE EXPLORER START
  const placeExBack = document.querySelector('#place_explorer-back');
  const placeExContainer = document.querySelector('#place_explorer-main_panel');
  const placeExTopBar = document.createElement('div');
  const placeExBackButton = document.createElement('button');
  const placeExCloseButton = document.createElement('button');

  placeExBackButton.appendChild(backIcon);
  placeExCloseButton.appendChild(closeIcon);

  placeExBackButton.addEventListener('click', () => placeExBack.click());
  placeExCloseButton.addEventListener('click', () => placeExBack.click());
  
  placeExTopBar.appendChild(placeExBackButton);
  placeExTopBar.appendChild(placeExBack);
  placeExTopBar.appendChild(placeExCloseButton);
  placeExTopBar.id = 'place_explorer-back';
  placeExTopBar.style.display = 'none';

  placeExContainer.parentElement.appendChild(placeExTopBar);
  // PLACEE EXPLORER END
  
  
  
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

  // const scrolls = document.querySelectorAll('.sus-scroll');

  // // console.log('scrolls: ' + scrolls.length);

  // for(var i = 0; i < scrolls.length; i++) {
  //   const scroll = scrolls[i];
  //   const content = scroll.querySelector('.sus-scroll-content');

  //   const bg = document.createElement('div');
  //   bg.classList.add('sus-scrollbar');
    
  //   const handle = document.createElement('div');
  //   handle.classList.add('sus-scrollbar-handle');

  //   scroll.appendChild(bg);
  //   scroll.appendChild(handle);

  //   const id = `sus-scroll-${i}`;
  //   scroll.id = id;
    
  //   content.addEventListener('pointerdown', () => {
  //     // console.log('#' + scroll.id + ' content pressed');
  //   });
  //   content.addEventListener('pointerup', () =>{
  //     // console.log('#' + scroll.id + ' content released');
  //   });
  //   content.addEventListener('wheel', () => {
  //     // console.log('#' + scroll.id + ' content scrolled');
  //   });

  //   handle.addEventListener('pointerdown', () => {
  //     // console.log('#' + scroll.id +  ' handle pressed');
  //   });
  //   handle.addEventListener('pointerup', () =>{
  //     // console.log('#' + scroll.id + ' handle released');
  //   });

  //   const paddingRight = getComputedStyle(content).getPropertyValue('padding-right');

  //   const updateScroll = () => {
  //     const enabled = content.offsetHeight > scroll.offsetHeight;
  //     // if (scroll.classList.contains('sus-scroll-enabled') != enabled) {
  //     //   if (enabled) {
  //     //     scroll.classList.add('sus-scroll-enabled');
  //     //   } else {
  //     //     scroll.classList.remove('sus-scroll-enabled');
  //     //   }
  //     // }
  //     console.log(`${scroll.id}: evaluating offset heights (${content.offsetHeight} > ${scroll.offsetHeight} ? ${enabled})`);
  //     const scrollStyle = getComputedStyle(scroll);
  //     const oldOverflow = scrollStyle.getPropertyValue('overflow-y');
  //     const newOverflow = enabled ? 'scroll' : 'hidden';
  //     if (oldOverflow != newOverflow) {
  //       console.log(`${scroll.id}: updating overflow-y (old value: ${oldOverflow}, new value: ${newOverflow})`)
  //       scroll.style = `overflow-y: ${newOverflow};`;
  //       const scrollbarWidth = scroll.offsetWidth - scroll.clientWidth;
  //       content.style = `width: calc(100% + ${scrollbarWidth}px);`;
  //     } else {
  //       console.log(`${scroll.id}: (already up-to-date)`);
  //     }
  //     // if (enabled) {
  //     //   handle.style = `height: calc(100% * ${scroll.offsetHeight / content.offsetHeight})`;
  //     // }
  //   };

  //   var throttle = false;
  //   var throttleTimeout = undefined;

  //   // Callback function to execute when mutations are observed
  //   const scrollCallback = function(mutationsList, observer) {
  //     if (!throttle) {
  //       throttle = true;
  //       setTimeout(() => {
  //         console.log(`${scroll.id}: update scroll (content mutation)`);
  //         updateScroll();
  //       }, 40);
  //       setTimeout(() => {
  //         throttle = false;
  //       }, 50);
  //     } else {
  //       console.log(`${scroll.id}: throttled`);
  //     }
  //   };

  //   // Create an observer instance linked to the callback function
  //   const scrollObserver = new MutationObserver(scrollCallback);

  //   // Start observing the target node for configured mutations
  //   scrollObserver.observe(content, config);

  //   setTimeout(() => {
  //     console.log(`${scroll.id}: update scroll (initial)`);
  //     updateScroll();
  //   }, 100);

  //   window.addEventListener('resize', () => {
  //     console.log(`${scroll.id}: update scroll (window resized)`);
  //     updateScroll();
  //   });
  // }

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

  const getVisibleText = ( node )=> {
    if( node.nodeType === Node.TEXT_NODE ) return node.textContent;
    var style = getComputedStyle( node );
    if( style && style.display === 'none' ) return '';
    var text = '';
    for( var i=0; i<node.childNodes.length; i++ ) 
        text += getVisibleText( node.childNodes[i] );
    return text;
  };

  document.fonts.ready.then(() => {
    const carousels = document.querySelectorAll('.sus-carousel');
    for(var i = 0; i < carousels.length; i++) {
      const carousel = carousels[i];
      const slides = carousel.querySelectorAll('.sus-carousel-slide');
      const navLeft = carousel.querySelector('.sus-carousel-nav-bttn-left');
      const navRight = carousel.querySelector('.sus-carousel-nav-bttn-right');
      const preNext = carousel.querySelector('.sus-carousel-preview-next');
      const prePrev = carousel.querySelector('.sus-carousel-preview-prev');
      const preNextContent = preNext.querySelector('.sus-carousel-preview-content');
      const prePrevContent = prePrev.querySelector('.sus-carousel-preview-content');
      const bulletsContainer = carousel.querySelector('.sus-carousel-bullets');
      const bullets = [];
      const previews = [];
      var activeIndex = -1;
      const showSlide = (index) => {
        if (index < slides.length && index > -1) {
          for(var j = 0; j < slides.length; j++) {
            const slide = slides[j];
            const bullet = bullets[j];
            slide.style.display = 'none';
            bullet.classList.remove('sus-carousel-bullet-active');
          }
          const slide = slides[index];
          const bullet = bullets[index];
          slide.style.display = 'flex';
          bullet.classList.add('sus-carousel-bullet-active');
          const nextIndex = index == slides.length - 1 ? 0 : index + 1;
          const prevIndex = index == 0 ? slides.length - 1 : index - 1;
          prePrevContent.innerText = previews[prevIndex];
          preNextContent.innerText = previews[nextIndex];
          activeIndex = index;
        }
      };
      const maxPreview = 18;
      var minHeight = 0;
      for(var j = 0; j < slides.length; j++) {
        const slide = slides[j];
        slide.style.display = 'flex';
        const height = slide.offsetHeight;
        slide.style.display = 'none';
        minHeight = Math.max(height, minHeight);
        const index = j;
        const bullet = document.createElement('div');
        bullet.classList.add('sus-carousel-bullet');
        bullet.addEventListener('click', () => {
          showSlide(index);
        });
        bulletsContainer.appendChild(bullet);
        bullets.push(bullet);
        var preview = slide.getAttribute("data-preview");
        if (preview == null || preview == undefined || preview.length == 0) {
          console.log(`slide ${index} missing preview`);
          preview = getVisibleText(slide.querySelector('h2')).trim().replace('\n', ' ').replace('\r', ' ').replace('\t', ' ');
          console.log(`slide ${index} h2: \"${preview}\"`);
          if (preview.length == null || preview == undefined || preview.length == 0) {
            console.log(`slide ${index} missing h2 fallback, preview = undefined`);
            preview = undefined;
          } else if (preview.length > maxPreview) {
            console.log(`slide ${index} preview too long`);
            const words = preview.split(' ');
            console.log(`slide ${index} words: ${words}`);
            var abbr = []
            while (words.length > 0) {
              abbr.push(words.shift());
              if (abbr.join(' ').length > maxPreview) {
                console.log(`slide ${index} "${abbr.join(' ')}" too long, popping last`);
                abbr.pop();
                break;
              }
            }
            console.log(`slide ${index} abbr: ${words}`);
            abbr = abbr.join(' ');
            if (abbr.length == 0) {
              preview = abbr.substring(0, maxPreview);
            }
            preview = `${abbr.substring(0, maxPreview)}...`;
            console.log(`slide ${index} preview from h2: \"${preview}\"`);
          }
        } else {
          console.log(`slide ${index} preview from data: \"${preview}\"`);
        }
        previews.push(preview);
      }
      for(var j = 0; j < slides.length; j++) {
        const slide = slides[j];
        slide.style.minHeight = `${minHeight}px`;
        console.log(`setting slide ${j} style.minHeight to ${minHeight}`);
        console.log(`${slide.getAttribute('style')}`);
      }
      if (slides.length > 0) {
        showSlide(0);
      }
      const cycleSlide = (delta) => {
        var index = (activeIndex + delta);
        if (index < 0) {
          index = slides.length - 1;
        } else if (index >= slides.length) {
          index = 0;
        }
        showSlide(index);
      };
      navLeft.addEventListener('click', () => cycleSlide(-1));
      navRight.addEventListener('click', () => cycleSlide(1));
      prePrev.addEventListener('click', () => cycleSlide(1));
      preNext.addEventListener('click', () => cycleSlide(-1));
      preNext.classList.add("whats-good");
      if (slides.length > 1) {
        navLeft.style.opacity = '1';
        navRight.style.opacity = '1';
        preNext.style.opacity = '1';
        prePrev.style.opacity = '1';
        bulletsContainer.style.opacity = '1';
        console.log('activating nav!');
      } else {
        console.log('did not activate nav');
      }
      if (slides.length > 0) {
        carousel.style.opacity = '1';
      }
      window.addEventListener("resize", () => {
        var minHeight = 0;
        for(var j = 0; j < slides.length; j++) {
          const slide = slides[j];
          slide.style.removeProperty('min-height');
        }
        for(var j = 0; j < slides.length; j++) {
          const slide = slides[j];
          slide.style.display = 'flex';
          const height = slide.offsetHeight;
          if(j != activeIndex) {
            slide.style.display = 'none';
          }
          minHeight = Math.max(height, minHeight);
        }
        for(var j = 0; j < slides.length; j++) {
          const slide = slides[j];
          slide.style.minHeight = `${minHeight}px`;
          console.log(`setting slide ${j} style.minHeight to ${minHeight}`);
          console.log(`${slide.getAttribute('style')}`);
        }
      });
    }
  });
