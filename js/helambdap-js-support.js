// -*- Mode: JavaScript -*-

// helambdap-js-support.js --
// Javascript support code for HELambdaP.
//
// See file COPYING in top level folder for licence and copying
// information.

var xhttp = new XMLHttpRequest();


function set_nav_mode(mode) {
    str = '<div id="nav_index"></div>';
    if (mode === 1) {
	str = '<div id="nav_map"></div><div id="nav_list"></div>';
    }
    document.getElementById('nav').innerHTML = str;
    
    str = '<div id=\"introduction_frame\"></div>';
    if (mode === 1) {
	str = '<div id="dictionary-entries_frame"></div>';
    }
    document.getElementById('main').innerHTML = str;
};


function load_section(id, filename) {
    xhttp.open("GET", filename, false); 
    xhttp.send();
    document.getElementById(id).innerHTML = xhttp.responseText;
};


function load_index() {
    set_nav_mode(0);
    load_section('nav_index', 'index-navigation.html');
    load_section('introduction_frame', 'introduction.html');
};


function load_dictionary() {
    set_nav_mode(1);

    load_section('nav_map',
		 'dictionary/dictionary-navigation.html');
    
    load_section('dictionary/dictionary-entries_frame',
		 'dictionary/dictionary-entries.html')
};

// end of file -- helambdap-js-support.js
