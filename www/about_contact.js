$(document).ready(function() {
    // Get all the dropdown toggles
    const dropdownToggles = $(".navbar .container-fluid .navbar-nav .dropdown .dropdown-toggle");

    // Iterate through the dropdown toggles
    dropdownToggles.each(function() {
        // Check if the data-value attribute contains 'About'
        if ($(this).data("value").indexOf("About") !== -1) {
            // Get the corresponding dropdown menu and append the content
            $(this).siblings('.dropdown-menu').append('<li><a href="mailto:contact@curbcut.ca">Contact</a></li>');
            $(this).siblings('.dropdown-menu').append('<li id="newsletter"><a href="#">Newsletter</a></li>');
        }
    });
    
    $('#newsletter').click(function(e) {
    e.preventDefault();
    Shiny.setInputValue('newsletter_click', Math.random(), {priority: 'event'});
  });
});
