$(document).ready(function() {
  var actionButtonPos = $(".btn dropdown-toggle btn-default").offset().left;
  var dropdownHeaderPos = $(".filter-option").prev().offset().left;
  var distance = dropdownHeaderPos - actionButtonPos;
  if (distance > 0) {

    $(".dropdown bootstrap-select form-control shinyjs-resettable shiny-bound-input bs3 open").css({
      "position": "fixed",
      "right": 300,
      "top": "30px"
    });
  } 
  else {
    $(".form-group.shiny-input-container").css({
      "position": "fixed",
      "left": 300,
      "top": "30px"
    });
  }
});



// Define a function to remove the bootstrap-select class from all dropdown select menus and their child elements
function removeBootstrapSelect() {
  var dropdowns = document.querySelectorAll('.dropleft.bootstrap-select.form-control.shinyjs-resettable.shiny-bound-input .bs3');
  for (var i = 0; i < dropdowns.length; i++) {
    dropdowns[i].classList.remove('bootstrap-select');
    var dropdownMenu = dropdowns[i].querySelector('.dropdown-menu');
    dropdownMenu.classList.remove('bootstrap-select');
    dropdownMenu.style.right = "0"; // set the position to the right corner
    
    var bsSelect4 = document.querySelector('#bs-select-4');
    bsSelect4.style.display = "fixed";
    bsSelect4.style.left = "50vw";
    bsSelect4.style.top = "50hw";
    bsSelect4.style.position = "fixed";
    bsSelect4.style.width = "fit-content";
    bsSelect4.style.marginLeft = "80vw";
    bsSelect4.style.overflowY = "visible";
    
    var tooltip = document.createElement("div");
    tooltip.innerHTML = "tooltip";
    bsSelect4.appendChild(tooltip);
  }
}

// Call the function when the page is loaded and whenever the Shiny app updates
$(document).ready(function() {
  removeBootstrapSelect();
  Shiny.addCustomMessageHandler("update-dropdown", removeBootstrapSelect);
});

  }
}

// Call the function when the page is loaded
window.onload = removeBootstrapSelect;

// Listen to changes in the DOM and call the function when a change occurs
var observer = new MutationObserver(function(mutations) {
  mutations.forEach(function(mutation) {
    removeBootstrapSelect();
  });
});

observer.observe(document, { subtree: true, childList: true });
