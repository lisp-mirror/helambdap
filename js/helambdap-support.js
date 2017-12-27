// -*- Mode: JavaScript -*-

// helambdap-support.js --
// Javascript support code for HELambdaP.
// This code is included as a <script> in each top-level HTML5
// HELambdaP-generated file.
//
// 20170504 MA: New version.
//
// See file COPYING in top level folder for licence and copying
// information.

var hlp_xhttp = new XMLHttpRequest();


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
    hlp_xhttp.open("GET", filename, false); 
    hlp_xhttp.send();
    document.getElementById(id).innerHTML =
	hlp_xhttp.responseText;
};


function hlp_load_introduction() {
    // set_nav_mode(0);
    hlp_load_section('mainnav', 'introduction-navigation.html');
    hlp_load_section('main', 'introduction.html');
};


function hlp_load_dictionary() {
    hlp_load_section('mainnav',
		     // 'dictionary/dictionary-navigation.html'
		     'dictionary/dictionary-navigation-map.html'
		    );
    
    hlp_load_section('main',
		     'dictionary/dictionary-entries.html')
};

// end of file -- helambdap-js-support.js
