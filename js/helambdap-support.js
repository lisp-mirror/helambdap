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


// function set_nav_mode(mode) {
//     str = '<div id="nav_index"></div>';
//     if (mode === 1) {
// 	str = '<div id="nav_map"></div><div id="nav_list"></div>';
//     }
//     document.getElementById('nav').innerHTML = str;
    
//     str = '<div id=\"introduction_frame\"></div>';
//     if (mode === 1) {
// 	str = '<div id="dictionary-entries_frame"></div>';
//     }
//     document.getElementById('main').innerHTML = str;
// };


function hlp_load_section(id, filename) {
    var hlp_xhttp = new XMLHttpRequest();

    // hlp_xhttp.responseType = 'text';
    // hlp_xhttp.open("GET", filename, false); 
    // hlp_xhttp.send();
    // document.getElementById(id).innerHTML = hlp_xhttp.responseText;
    
    console.log('>>> ' + filename + '\n    ' + document.URL);
    document.getElementById(id).innerHTML =
	filename + '<br />' + document.URL;
    return;
};


function hlp_load_introduction() {
    // set_nav_mode(0);
    // hlp_load_section('mainnav', document.URL + 'introduction-navigation.html');
    // hlp_load_section('main', document.URL + 'introduction.html');
    // hlp_get_section('main', document.URL + 'introduction.html');
    hlp_get_section('main', 'introduction.html');
    hlp_get_section('mainnav', 'introduction-navigation.html');
    return;
};


function hlp_load_dictionary() {
    // hlp_load_section('mainnav',
    // 		     // 'dictionary/dictionary-navigation.html'
    // 		     'dictionary/dictionary-navigation-map.html'
    // 		    );
    
    // hlp_load_section('main',
    // 		     'dictionary/dictionary-entries.html');

    hlp_get_section('mainnav',
		    // 'dictionary/dictionary-navigation.html'
		    'dictionary/dictionary-navigation-map.html'
		   );
    
    hlp_get_section('main',
		    'dictionary/dictionary-entries.html');
    return;
};


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
}


// end of file -- helambdap-js-support.js
