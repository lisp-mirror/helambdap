// -*- Mode: JavaScript -*-

// helambdap-support.js --
// Javascript support code for HELambdaP.
// This code is included as a <script> in each top-level HTML5
// HELambdaP-generated file.
//
// See file COPYING in top level folder for licence and copying
// information.
//
// 20170504 MA: New version.
// 20190216 MA: New version using the Fetch API; to test now we need
//              to start a http server (e.g., nodejs 'http-server') to
//              work around CORS.


function hlp_get_section(id, filename) {
    console.log('>>> ' + id + '; ' + filename);
    console.log('>>> ' + id + '; ' + filename.substring(0, filename.lastIndexOf('/')));

    var requestInit = { mode: 'no-cors' }
    // var myRequest = new Request(filename, requestInit);
    var myRequest = new Request(filename);
    var myArticle = document.getElementById(id)
    
    fetch(myRequest)
	.then(function(response) {
            if (!response.ok) {
		throw new Error("HTTP error, status = " + response.status);
            }
            return response.text();
	})
	.then(function(text) {
            myArticle.innerHTML = text;
	    return text
	})
	.then(function(text) {
	    console.log(text)
	})
	.catch(function(error) {
            myArticle.innerHTML = '';
            myArticle.appendChild(
		document.createTextNode('Error: ' + error.message)
            );
	});
    return;
};


function hlp_load_introduction() {
    hlp_get_section('main', 'introduction.html');
    hlp_get_section('mainnav', 'introduction-navigation.html');
    return;
};


function hlp_load_dictionary() {
    hlp_get_section('main',
		    'dictionary/dictionary-entries.html');
    hlp_get_section('mainnav',
		    'dictionary/dictionary-navigation-map.html');
    return;
};

// end of file -- helambdap-support.js
