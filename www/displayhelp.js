/* this js file display a help for first time visitors */


window.alert("this site use cookies");
$(document).ready(function () {
    var visitedBefore = document.cookie.includes("visited_before=true");

    if (!visitedBefore) {
        var modal = document.createElement('div');
        modal.classList.add('modal');
        modal.innerHTML = '<h1>Welcome to our website!</h1><p>This website uses cookies to ensure you get the best experience. By continuing to browse the site, you are agreeing to our use of cookies. Please <a href="/privacy-policy">read our privacy policy</a> for more information.</p><p>If it\'s your first visit, you can find some useful information on our <a href="/help">help page</a>.</p><button>Accept</button><button>Decline</button>';
        document.body.appendChild(modal);

        var acceptButton = modal.querySelector('button:first-of-type');
        acceptButton.addEventListener('click', function () {
            document.cookie = "visited_before=true; expires=Fri, 31 Dec 9999 23:59:59 GMT";
            modal.style.display = 'none';
        });
        var declineButton = modal.querySelector('button:last-of-type');
        declineButton.addEventListener('click', function () {
            modal.parentNode.removeChild(modal);
        });
    } else {
        document.cookie = "visited_before=true; expires=Fri, 31 Dec 9999 23:59:59 GMT";
    }
});
