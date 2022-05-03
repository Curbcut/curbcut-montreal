// {shinybrowser} by Dean Attali

shinybrowser = function() {

  var UNKNOWN = 'UNKNOWN';

  var isMobile = function() {
    // ambiguous results for tablets
  	return /Mobi/.test(navigator.userAgent);
  };

  var getBrowser = function() {
    var browser = UNKNOWN;
    var version = UNKNOWN;
    var ua = navigator.userAgent;

    try {
    	// Opera 15+
    	if (ua.indexOf('OPR/') >= 0) {
    	  browser = 'Opera';
    	  version = ua.match(/OPR\/(\d+)/i)[1];
    	}

    	// Firefox 1+
    	else if (typeof InstallTrigger !== 'undefined') {
    		browser = 'Firefox';
    		version = ua.match(/(firefox(?=\/))\/?\s*(\d+)/i)[2];
    	}

    	// Safari 3+
    	else if (/constructor/i.test(window.HTMLElement) ||
    	         (typeof window.safari !== 'undefined' &&
    	           typeof window.safari.pushNotification !== 'undefined')) {
    		browser = 'Safari';
    		version = ua.match(/version\/(\d+)/i)[1];
    	}

    	// Internet Explorer 6-11
    	else if (/* @cc_on!@*/false || document.documentMode) {
    		browser = 'Internet Explorer';
    		var msie = ua.indexOf('MSIE ');
        if (msie > 0) { // IE 10-
          version = parseInt(ua.substring(msie + 5, ua.indexOf('.', msie)), 10);
        } else {
          if (ua.match(/Trident.*rv\:11\./)) { // IE 11
            version = '11';
          }
        }
    	}

    	// Edge 20+
    	else if (!(document.documentMode) && window.StyleMedia) {
    		browser = 'Edge';
    		var edge = ua.indexOf('Edge/');
    		if (edge > 0) {
    		  version = parseInt(ua.substring(edge + 5, ua.indexOf('.', edge)), 10);
    		}
    	}

    	// Edge based on Chromium
    	else if (window.chrome && ua.indexOf("Edg") != -1) {
    	  browser = 'Edge';
    	  version = ua.match(/Chrom(e|ium)\/([0-9]+)\./i)[2];
    	}

    	// Chrome
    	else if (window.chrome) {
    		browser = 'Chrome';
    		version = ua.match(/Chrom(e|ium)\/([0-9]+)\./i)[2];
    	}

    	// Chrome on iOS
    	else if (ua.indexOf('CriOS/') >= 0) {
    		browser = 'Chrome';
    		version = ua.match(/CriOS\/([0-9]+)\./i)[1];
    	}
    } catch(err) {}

    return { name : browser, version : version };
  };

  var getOS = function() {
    var os = UNKNOWN;
    var version = UNKNOWN;
    var ua = navigator.userAgent;

    try {
    	if (isMobile()) {
    		if (/Windows/.test(ua)) {
    			os = 'Windows Phone';
    			if (/Phone 8.0/.test(ua)) {
    				version = 8;
    			} else if (/Phone 10.0/.test(ua)) {
    				version = 10;
    			}
    		} else if (/android/i.test(ua)) {
          os = 'Android';
    			version = parseInt(ua.match(/android\s([0-9\.]*)/i)[1]);
    		} else if (/iphone;/i.test(navigator.userAgent)) {
    			os = 'iOS';
          version = parseInt((navigator.appVersion).match(/OS (\d+)_(\d+)_?(\d+)?/)[1]);
    		} else if (/ipad;/i.test(navigator.userAgent)) {
    			os = 'iOS';
    			version = parseInt((navigator.appVersion).match(/OS (\d+)_(\d+)_?(\d+)?/)[1]);
    		} else if (/BBd*/.test(navigator.userAgent)) {
    			os = 'BlackBerry';
    		}
    	} else {
    		if (/Windows/.test(navigator.userAgent)) {
    			os = 'Windows';
    			if (/NT 5.1;/.test(navigator.userAgent)) {
    				version = 'XP';
    			} else if (/NT 6.0;/.test(navigator.userAgent)) {
    				version = 'Vista';
    			} else if (/NT 6.1;/.test(navigator.userAgent)) {
    				version = '7';
    			} else if (/NT 6.2;/.test(navigator.userAgent)) {
    				version = '8';
    			} else if (/NT 10.0;/.test(navigator.userAgent)) {
    				version = '10';
    			}
    		} else if (/Mac/.test(navigator.userAgent)) {
    			os = 'Mac';
    			if (/OS X/.test(navigator.userAgent)) {
    				version = 'OS X';
    			}
    		} else if (/Linux|X11/.test(navigator.userAgent)) {
    			os = 'Linux';
    		}
    	}
    } catch(err) {}

  	return { name : os, version : version };
  };

  return {
    detect : function() {
      var props = {
        device : isMobile() ? 'Mobile' : 'Desktop',
        browser : getBrowser(),
        os : getOS(),
    		dimensions : {
    		  width : $(window).width(),
    		  height : $(window).height()
    		},
    		user_agent : navigator.userAgent
      };
      Shiny.setInputValue(".shinybrowser", props);
    },

    init : function() {
      // Make sure to send the message regardless of if the script is loaded on init or not
      if (typeof Shiny === "undefined" || typeof Shiny.setInputValue === "undefined") {
        $(document).on('shiny:connected', function(event) {
          shinybrowser.detect();
        });
      } else {
        shinybrowser.detect();
      }
    }
  };
}();

$(function() { shinybrowser.init(); });