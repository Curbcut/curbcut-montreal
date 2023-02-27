// retrieve the cookies and sets the result as an input named cookies
function getCookies(){
  var res = Cookies.get();
  Shiny.setInputValue('cookies', res);
}

// we define two message handlers, one that sets the cookie and another that removes it.
// both of them run the getCookies function defined previously after they are done with their respective operations
// to update the input with the new values after it is set and after it is removed
Shiny.addCustomMessageHandler('cookie-set', function(msg){
  Cookies.set(msg.name, msg.value);
  getCookies();
})

Shiny.addCustomMessageHandler('cookie-remove', function(msg){
  Cookies.remove(msg.name);
  getCookies();
})

$(document).on('shiny:connected', function(ev){
  getCookies();
})
