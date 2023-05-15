
const gallery = document.querySelector('#aside-right-gallery');
const leftArrow = document.querySelector('#gallery-prev');
const rightArrow = document.querySelector('#gallery-next');

let scrollAmount = 0;
const imageWidth = 300; // width of each image in pixels

leftArrow.addEventListener('click', () => {
  scrollAmount -= imageWidth;
  if (scrollAmount < 0) {
    scrollAmount = 0;
  }
  gallery.scrollTo({
    top: 0,
    left: scrollAmount,
    behavior: 'smooth'
  });
});

rightArrow.addEventListener('click', () => {
  const galleryWidth = gallery.offsetWidth;
  const maxScroll = gallery.scrollWidth - galleryWidth;
  scrollAmount += imageWidth;
  if (scrollAmount > maxScroll) {
    scrollAmount = maxScroll;
  }
  gallery.scrollTo({
    top: 0,
    left: scrollAmount,
    behavior: 'smooth'
  });
});


//BIO
const biographyTitle = document.querySelector('.bibliography-toggle');
const biographyText = document.querySelector('.biography-text');

biographyTitle.addEventListener('click', () => {
if (biographyText.classList.contains('hidden')) {
biographyText.classList.remove('hidden');
} else {
biographyText.classList.add('hidden');
}
});