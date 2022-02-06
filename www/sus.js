window.addEventListener('load', (evt) => {
  const head = document.querySelector('head');

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

  const params = new URLSearchParams(window.location.search);

  if (params.has('tab') && params.get('tab') == 'home') {
    var style = document.createElement('style');
    // style.innerText = `.navbar-`;
    head.appendChild(style);
}
});