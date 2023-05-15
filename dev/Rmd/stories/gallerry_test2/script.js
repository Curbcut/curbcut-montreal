var accItem = document.getElementsByClassName('accordionItem');
    var accHD = document.getElementsByClassName('accordionItemHeading');
    for (i = 0; i < accHD.length; i++) {
        accHD[i].addEventListener('click', toggleItem, false);
    }
    function toggleItem() {
        var itemClass = this.parentNode.className;
        for (i = 0; i < accItem.length; i++) {
            accItem[i].className = 'accordionItem close';
        }
        if (itemClass == 'accordionItem close') {
            this.parentNode.className = 'accordionItem open';
        }
    }

    var previous = document.getElementById('btnPrevious')
var next = document.getElementById('btnNext')
var gallery = document.getElementById('image-gallery')
var pageIndicator = document.getElementById('page')
var galleryDots = document.getElementById('gallery-dots');

var galleryImages = [
  { title: "Image 0", source: "../tempImagesRep/Fig3.png" },
  { title: "Image 12", source: "../tempImagesRep/Fig12.png" },
  { title: "Image 2", source: "../tempImagesRep/Fig5.png" },
  { title: "Image 3", source: "../tempImagesRep/Fig6.png" },
  { title: "Image 4", source: "../tempImagesRep/Fig7.png" },
  { title: "Image 3", source: "../tempImagesRep/Fig8.png" },
  { title: "Image 4", source: "../tempImagesRep/Fig9.png" },
  { title: "Image 2", source: "../tempImagesRep/Fig10.png" },
  { title: "Image 3", source: "../tempImagesRep/Fig9.png" },
  { title: "Image 4", source: "../tempImagesRep/Fig10.png" },
  { title: "Image 2", source: "../tempImagesRep/Fig11.png" },
  { title: "Image 3", source: "../tempImagesRep/Fig12.png" },
  { title: "Image 4", source: "../tempImagesRep/Fig13.png" },
  { title: "Image 2", source: "../tempImagesRep/Fig32.png" },
  { title: "Image 3", source: "../tempImagesRep/Fig33.png" }
];

var images = [];

for (var i = 0; i < galleryImages.length; i++) {
  images.push({
    title: galleryImages[i].title,
    source: galleryImages[i].source
  });
}
var perPage = 4;
var page = 1;
var pages = Math.ceil(images.length / perPage)


// Gallery dots
for (var i = 0; i < pages; i++){
  var dot = document.createElement('button')
  var dotSpan = document.createElement('span')
  var dotNumber = document.createTextNode(i + 1)
  dot.classList.add('gallery-dot');
  dot.setAttribute('data-index', i);
  dotSpan.classList.add('sr-only');
  
  dotSpan.appendChild(dotNumber);
  dot.appendChild(dotSpan)
  
  dot.addEventListener('click', function(e) {
    var self = e.target
    goToPage(self.getAttribute('data-index'))
  })
  
  galleryDots.appendChild(dot)
}

// Previous Button
previous.addEventListener('click', function() {
  if (page === 1) {
    page = 1;
  } else {
    page--;
    showImages();
  }
})

// Next Button
next.addEventListener('click', function() {
  if (page < pages) {
    page++;
    showImages();
  }
})

// Jump to page
function goToPage(index) {
  index = parseInt(index);
  page =  index + 1;
  
  showImages();
}

// Load images
function showImages() {
  while(gallery.firstChild) gallery.removeChild(gallery.firstChild)
  
  var offset = (page - 1) * perPage;
  var dots = document.querySelectorAll('.gallery-dot');
  
  for (var i = 0; i < dots.length; i++){
    dots[i].classList.remove('active');
  }
  
  dots[page - 1].classList.add('active');
  
  for (var i = offset; i < offset + perPage; i++) {
    if ( images[i] ) {
      var template = document.createElement('div');
      var title = document.createElement('p');
      var titleText = document.createTextNode(images[i].title);
      var img = document.createElement('img');
      
      template.classList.add('template')
      img.setAttribute("src", images[i].source);
      img.style.borderRadius = "10px";
      img.setAttribute('alt', images[i].title);

      title.appendChild(titleText);
      template.appendChild(img);
      template.appendChild(title);
      gallery.appendChild(template);      
    }
  }
  
  // Animate images
  var galleryItems = document.querySelectorAll('.template')
  for (var i = 0; i < galleryItems.length; i++) {
    var onAnimateItemIn = animateItemIn(i);
    setTimeout(onAnimateItemIn, i * 100);
  }
  
  function animateItemIn(i) {
    var item = galleryItems[i];
    return function() {
      item.classList.add('animate');
    }
  }
  
  // Update page indicator
  pageIndicator.textContent = "Page " + page + " of " + pages;
  
}

showImages();



//BIO
document.addEventListener("DOMContentLoaded", () => {
  const biographyTitle = document.querySelector('.bibliography-toggle');
  const biographyText = document.querySelector('.biography-text');

  biographyTitle.addEventListener('click', () => {
    if (biographyText.classList.contains('hidden')) {
      biographyText.classList.remove('hidden');
    } else {
      biographyText.classList.add('hidden');
    }
  });
});