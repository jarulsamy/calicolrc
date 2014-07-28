/**
 * Calico Jupyter Notebooks Extensions
 *
 * Copyright (c) The Calico Project
 * http://calicoproject.org/ICalico
 *
 * Released under the BSD Simplified License
 *
 **/

define( function () {

    function toggle_spell_check() {
	// Toggle on/off spelling checking on for markdown and heading cells
	// toggle it!
	var spelling_mode = (IPython.MarkdownCell.options_default.cm_config.mode == "gfm");
	// Change defaults for new cells:
	IPython.MarkdownCell.options_default.cm_config.mode = (spelling_mode ? "spell-check-gfm" : "gfm");
	IPython.HeadingCell.options_default.cm_config = {"mode": (spelling_mode ? "spell-check-mixedhtml" : "mixedhtml")};
	// And change any existing markdown cells:
	var cells = IPython.notebook.get_cells();
	for (var i = 0; i < cells.length; i++) {
            var cell = cells[i];
            if (cell.cell_type == "markdown") {
		IPython.notebook.get_cell(i).code_mirror.setOption('mode', (spelling_mode ? "spell-check-gfm" : "gfm"));
	    } else if (cell.cell_type == "heading") {
		IPython.notebook.get_cell(i).code_mirror.setOption('mode', (spelling_mode ? "spell-check-mixedhtml" : "mixedhtml"));
	    }
	}
    }

    var load_css = function () {
	var link = document.createElement("link");
	link.type = "text/css";
	link.rel = "stylesheet";
	link.href = require.toUrl("/nbextensions/calico-spell-check.css");
	document.getElementsByTagName("head")[0].appendChild(link);
    };

    var typo_check = function(word) {
	// Put your specific method of checking the spell of words here:
	// remove beginning or ending single quote
	return document.dictionary.check(word.replace(/(^')|('$)/g, ""));
    };
	
    var load_ipython_extension = function () {
	// Load the CSS for spelling errors (red wavy line under misspelled word):
	load_css();

	var makeOverlay = function(mode) {
	    return function(config, parserConfig) {
		// This overlay sits on top of a mode, given below.
		// It first checks here to see if a CSS class should be given.
		// Here, "spell-error" is defined in the associated CSS file
		
		// rx_word defines characters not in words. Everything except single quote.
		var rx_word = new RegExp("[^\!\"\#\$\%\&\(\)\*\+\,\-\.\/\:\;\<\=\>\?\@\[\\\]\^\_\`\{\|\}\~\ ]");
		var spellOverlay = {
		    token: function (stream, state) {
			var ch;
			if (stream.match(rx_word)) { 
			    while ((ch = stream.peek()) != null) {
				if (!ch.match(rx_word)) {
				    break;
				}
				stream.next();
			    }
			    if (!typo_check(stream.current()))
				return "spell-error";
			    return null;
			}
			while (stream.next() != null && !stream.match(rx_word, false)) {}
			return null;
		    }
		};
		// Put this overlay on top of mode.
		// opaque: true allows the styles (spell-check and markdown) to be combined.
		return CodeMirror.overlayMode(CodeMirror.getMode(config, mode), spellOverlay, {"opaque": true});
	    }
	}
	
	CodeMirror.defineMode("spell-check-gfm", makeOverlay("gfm"));
	CodeMirror.defineMode("spell-check-mixedhtml", makeOverlay("mixedhtml"));

	// Load dictionary:
	require(['/nbextensions/typo/typo.js'], function() {
	    var lang = "en_US";
	    document.dictionary = new Typo(lang, undefined, undefined, 
					   {"platform": "web", 
					    "dictionaryPath": "/nbextensions/typo/dictionaries"});
	});

	// Put a button on the toolbar:
	if (!IPython.toolbar) {
	    $([IPython.events]).on("app_initialized.NotebookApp", add_toolbar_buttons);
	    return;
	} else {
	    add_toolbar_buttons();
	}
    };

    var add_toolbar_buttons = function () {
	IPython.toolbar.add_buttons_group([
	    // select your icon from http://fortawesome.github.io/Font-Awesome/icons
	    {
		'label'   : 'Toggle spell checking on a markdown cell',
		'icon'    : 'icon-check-sign',
		'callback': toggle_spell_check
	    }      
	]);
    }
    
    return {
        load_ipython_extension : load_ipython_extension,
    };
});
