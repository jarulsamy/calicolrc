/**
 * Calico Jupyter Notebooks Extensions
 *
 * Copyright (c) The Calico Project
 * http://calicoproject.org/ICalico
 *
 * Released under the BSD Simplified License
 *
 **/

$([IPython.events]).on('app_initialized.NotebookApp', function() {

    /* Logo for Calico */
    var img = $('.container img')[0];
    img.src = "/static/custom/icalico_logo.png";
    
    /* Extensions: ------------------------------------------- */
    
    /* Drag and Drop Images ---------------------------------- */
    require(['/static/custom/drag-and-drop.js']);
    /* End Drag and Drop Images ------------------------------ */
        
    /* Document Tools: --------------------------------------- */
    $.getScript('/static/custom/document-tools.js', function () {
	IPython.toolbar.add_buttons_group([
	    // select your icon from http://fortawesome.github.io/Font-Awesome/icons
	    {
		'label'   : 'Number Sections',
		'icon'    : 'icon-sort-by-order', 
		'callback': section_label
	    },
	    {
		'label'   : 'Generate Table of Contents',
		'icon'    : 'icon-sort-by-attributes-alt', 
		'callback': table_of_contents
	    },
	    {
		'label'   : 'Generate References',
		'icon'    : 'icon-book', 
		'callback': generate_references
	    }
	]);
    });
    /* End Document Tools: ----------------------------------- */

    /* Cell Tools: ------------------------------------------- */
    $.getScript('/static/custom/cell-tools.js', function() {
	IPython.toolbar.add_buttons_group([
	    // select your icon from http://fortawesome.github.io/Font-Awesome/icons
	    {
		'label'   : 'Toggle tabbed view on a code cell',
		'icon'    : 'icon-folder-close-alt', 
		'callback': toggle_tabs
	    },
	    {
		'label'   : 'Toggle two-column view on a code cell',
		'icon'    : 'icon-columns', 
		'callback': toggle_columns
	    }
	]);
	$([IPython.events]).on('notebook_loaded.Notebook', function() {
	    console.log("checking for formatting!");
	    checkForFormatting();	
	    console.log("checking for formatting done!");
	});
    });
    /* End Cell Tools: --------------------------------------- */
    
    /* Spelling Checking ------------------------------------- */
    require(['/static/custom/typo/typo.js'], function() {
	var lang = "en_US";
	document.dictionary = new Typo(lang, undefined, undefined, 
				       {"platform": "web", 
					"dictionaryPath": "/static/custom/typo/dictionaries"});
	});

    $.getScript('/static/custom/spell-check.js', function () {
	IPython.toolbar.add_buttons_group([
	    // select your icon from http://fortawesome.github.io/Font-Awesome/icons
	    {
		'label'   : 'Toggle spell checking on a markdown cell',
		'icon'    : 'icon-check-sign',
		'callback': toggle_spell_check
	    }      
	]);
    });
    /* End Spelling Checking --------------------------------- */
});
