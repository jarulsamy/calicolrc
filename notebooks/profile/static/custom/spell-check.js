/**
 * Calico Jupyter Notebooks Extensions
 *
 * Copyright (c) The Calico Project
 * http://calicoproject.org/ICalico
 *
 * Released under the BSD Simplified License
 *
 **/

function toggle_spell_check() {
    // Toggle on/off spelling checking on a markdown cell

    var typo_check = function(word) {
	// Put your specific method of checking the spell of words here:
	// remove beginning or ending single quote
	return document.dictionary.check(word.replace(/(^')|('$)/g, ""));
    };
    
    CodeMirror.defineMode("spell-check", function(config, parserConfig) {
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
	// Put this overlay on top of "markdown" mode.
	// opaque: true allows the styles (spell-check and markdown) to be combined.
	return CodeMirror.overlayMode(CodeMirror.getMode(config, "markdown"), spellOverlay, {"opaque": true});
    });

    // Now the toggle the default markdown mode
    var new_mode;
    if (IPython.MarkdownCell.options_default.cm_config.mode == "spell-check") {
	new_mode = "markdown";
    } else {
	new_mode = "spell-check";
    }
    
    // And change any existing markdown cells:
    IPython.MarkdownCell.options_default.cm_config.mode = new_mode;
    var cells = IPython.notebook.get_cells();
    for (var i = 0; i < cells.length; i++) {
        var cell = cells[i];
        if (cell.cell_type == "markdown") {
	    IPython.notebook.get_cell(i).code_mirror.setOption('mode', new_mode);
	}
    }
}

