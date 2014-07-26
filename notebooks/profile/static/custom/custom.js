/**
 *
 *
 * Calico Custom JavaScript for Jupyter Notebooks
 *
 *
 */


$([IPython.events]).on('app_initialized.NotebookApp', function() {
  //... 
  require(['/static/custom/drag-and-drop.js']);
  require(['/static/custom/bibtex.js']);
  require(['/static/custom/typo.js'], function() {
      var lang = "en_US";
      document.dictionary = new Typo(lang, undefined, undefined, 
				     {"platform": "web", 
				      "dictionaryPath": "/static/custom/dictionaries"});
  });
  var img = $('.container img')[0];
  img.src = "/static/custom/icalico_logo.png";
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
  IPython.toolbar.add_buttons_group([
      {
          'label'   : 'Toggle tabbed view on a code cell',
          'icon'    : 'icon-folder-close-alt', 
          'callback': toggle_tabs
      },
      {
          'label'   : 'Toggle two-column view on a code cell',
          'icon'    : 'icon-columns', 
          'callback': toggle_columns
      },
      {
          'label'   : 'Toggle spell checking on a markdown cell',
	  'icon'    : 'icon-check-sign',
          'callback': toggle_spell_check
      }      
  ]);
});

$([IPython.events]).on('notebook_loaded.Notebook', function() {
  checkForFormatting();	
});

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

function section_label() {
    // Label headings with numbers, or toggle them off
    // If there is a table of contents, re-do it
    var cells = IPython.notebook.get_cells();
    var levels = [0,0,0,0,0,0];
    var current_level = 1;
    var flag = false;
    var alert_text = "";
    var alert_flag = false;
    var remove_numbering = true;
    
    for (var i = 0; i < cells.length; i++) {
	var cell = cells[i];
	if (cell.cell_type == "heading") {
            var level = cell.level;
            
            if (level >= current_level) { //just keep incrementing
		current_level = level;
		levels[level-1]++;
            } else {                    //went back a level
		levels[current_level-1] = 0;
		
		if (current_level-level > 1) { //Skipped levels in between
                    for (var j = 1; j < current_level-level; j++) { //back-prop the zeros
			levels[current_level - 1 - j] = 0;
                    }
		}
		
		levels[level -1]++;
		current_level = level;
            }
            
            var error = false;
            var error_no_begin = 0;
            var error_no_end = 0;
            var error_heading_label = "";
            var heading_label = ""; //Generate the appropriate number for the heading
            for (var k = 0; k < level; k++) {
		if (levels[k] == 0) {
                    if (!error) {
			error_heading_label = heading_label;
			error = true;
			error_no_begin = k;
                    } else {
			error_no_end = k + 2;
                    }
		}
		heading_label += levels[k];
		if (level-k == 1 && level > 1) {
                    break;
		}
		heading_label += ".";
            }
	    
            if (error) {
		if (error_no_end == 0) {
                    error_no_end = error_no_begin + 2;
		}
		if (error_heading_label == "") {
                    if (!flag) {
			var temp1 = "Notebook begins with a Header " + error_no_end + " cell." + "\n";
			alert_text += temp1;
			alert_flag = true;
			flag = true;
                    }
		} else{
                    var temp = "You placed a Header " + error_no_end + " cell under a Header " + error_no_begin + " cell in section " + error_heading_label +"\n";
                    alert_text += temp;
                    alert_flag = true;
		}
            }
            
            var heading_text = cell.get_text();
            var old_header = heading_text;
            var re = /(?:\d*\.*)*\s*(.*)/;
	    var match = heading_text.match(re);
            
            if (match) {
		heading_text = heading_label + " " + match[1];
            } else {
		heading_text = heading_label;
            }
            
            if (old_header != heading_text) {
		remove_numbering = false;
		replace_links(old_header, heading_text);
            }
            
            cell.unrender();
	    heading_text = heading_text.trim();
            cell.set_text(heading_text);
            cell.render();
	}
    }
    
    if (alert_flag) {
	alert(alert_text);
    }
    
    if (remove_numbering) {
	for (var i = 0; i < cells.length; i++) {
            var cell = cells[i];
            if (cell.cell_type == "heading") {
		var heading_text = cell.get_text();
		old_header = heading_text;
		var re = /(?:\d*\.*)*\s*(.*)/;
		var match = heading_text.match(re);
		if (match) {
                    heading_text = match[1];
		}
		cell.unrender();
		cell.set_text(heading_text);
		cell.render();
		replace_links(old_header, heading_text);
            }
	}
    }

    // If there is a Table of Contents, replace it:
    var cells = IPython.notebook.get_cells();
    for (var i = 0; i < cells.length; i++) {
        var cell = cells[i];
        if (cell.cell_type == "markdown") {
	    var cell_text = cell.get_text();
	    var match = cell_text.match(/^#+Table of Contents/);
	    if (match) {
		table_of_contents(match[0]);
		break;
	    }
	}
    }
}

function replace_links(old_header, new_header) {
    // Replace an old internal link with new link
    new_header = new_header.trim();
    var cells = IPython.notebook.get_cells();
    for (var i = 0; i < cells.length; i++) {
        var cell = cells[i];
        if (cell.cell_type == "markdown") {
            var cell_text = cell.get_text();
	    // Skip over table of contents:
	    if (cell_text.match(/^#+Table of Contents/)) {
		continue;
	    }
            var re_string = old_header;
            re_string = re_string.replace(/\\/g, "\\\\");
            re_string = re_string.replace(/\//g, "\\/");
            re_string = re_string.replace(/\^/g, "\\^");
            re_string = re_string.replace(/\$/g, "\\$");
            re_string = re_string.replace(/\*/g, "\\*");
            re_string = re_string.replace(/\+/g, "\\+");
            re_string = re_string.replace(/\?/g, "\\?");
            re_string = re_string.replace(/\./g, "\\.");
            re_string = re_string.replace(/\)/g, "%29");
            re_string = re_string.replace(/\|/g, "\\|");
            re_string = re_string.replace(/\[/g, "\\[");
            re_string = re_string.replace(/\]/g, "\\]");
            re_string = re_string.replace(/\(/g, "?:\\(|%28");
            re_string = re_string.replace(/\s/g, "-");
	    re_string = "(\\[.*\\](?::\\s*|\\()#)" + re_string + "(.*\\)|(.*)$)";
            
            var re = new RegExp(re_string, "gm");
            var link_text = new_header.replace(/\s+$/g, ""); //Delete trailing spaces before they become "-"
            link_text = link_text.replace(/\(/g, "%28"); //Replace left parentheses with their encoding
            link_text = link_text.replace(/\)/g, "%29"); //Replace right parentheses with their encoding
            link_text = link_text.replace(/ /g, "-"); //Replace all spaces with dashes to create links
            
            var match = cell_text.match(re);
            if (match) {
                var new_text = cell_text.replace(re, "$1" + link_text + "$2");
                cell.unrender();
                cell.set_text(new_text);
                cell.render();
            }
        }
    }
}

function find_cell(cell_type, text) {
    // Finds first cell of cell_type that starts with text
    // cell_type and text are interpreted as a regular expression
    var cell = undefined;
    var cells = IPython.notebook.get_cells();
    for (var x = 0; x < cells.length; x++) {
	var temp = cells[x];
	if (temp.cell_type.match(cell_type) != undefined) {
            var temp_text = temp.get_text();
            var re = new RegExp("^" + text);
            if (re.test(temp_text)) {
		cell = cells[x];
		break;
            }
	}
    }
    return cell;
}

function table_of_contents() {
    // Create and/or replace Table of Contents
    var cells = IPython.notebook.get_cells();
    var toc_cell = find_cell("markdown", "#+Table of Contents");
    // Default to top-level heading
    var toc_text = "#Table of Contents\n";
    if (toc_cell == undefined) {
	//Create a new markdown cell at the top of the Notebook
	toc_cell = IPython.notebook.select(0).insert_cell_above("markdown"); 
    } else {
	// already exists:
	toc_text = toc_cell.get_text().match(/^#+Table of Contents/)[0] + "\n";
    }
    var prev_lev = 0;
    for (var i = 0; i < cells.length; i++) {
	var cell = cells[i];
	if (cell.cell_type == "heading") {
            
            if (cell.level - prev_lev > 1) { //Skipped levels. Enter Dummy levels
		for (var x = 0; x < ((cell.level - prev_lev) - 1); x++) {
                    for (var y = 0; y < (prev_lev + x); y++) {
			toc_text += "\t";
                    }
                    toc_text += "* &nbsp;\n";
		}
            }
            var cell_text = cell.get_text();
            for (var j = 0; j < cell.level -1; j++) { //Loop to add the proper amount of tabs based on header level
		toc_text += "\t";
            }
            toc_text += "* [";
            toc_text += cell_text;
            toc_text += "](#";
            var link_text = cell_text.replace(/\s+$/g, ""); //Delete trailing spaces before they become "-"
            link_text = link_text.replace(/\(/g, "%28"); //Replace left parentheses with their encoding
            link_text = link_text.replace(/\)/g, "%29"); //Replace right parentheses with their encoding
            link_text = link_text.replace(/ /g, "-"); //Replace all spaces with dashes to create links
            toc_text += link_text;
            toc_text += ")\n";
            prev_lev = cell.level;
	}
    }
    toc_cell.unrender();
    toc_cell.set_text(toc_text);
    toc_cell.render();
}

function generate_references() {
    read_bibliography();
    var citations = get_citations();
    create_reference_section(citations);
    update_refs(citations);
}

function update_refs(citations) {
    // go through and replace all (<a name="..."/>)*[.*](#cite-.*) with [CITE](#cite-.*)
    for (var c in IPython.notebook.get_cells()) {
        var cell = IPython.notebook.get_cell(c);
        if (cell.cell_type == "markdown") {
            var cell_text = cell.get_text();
	    var re = new RegExp("(\\<a name\\=\".*?\"/>)*\\[.*?\\]\\((\#cite-[^\\)]+)\\)", "g");
	    if (cell_text.match(re)) {
		cell_text = cell_text.replace(re, "[CITE]($2)");
		cell.set_text(cell_text);
	    }
	}
    }
    // then go through and replace each [CITE](#cite-.*) with <a name="ref-"/>[(AUTHORS)](#cite-...)
    var refs = 1;
    for (var c in IPython.notebook.get_cells()) {
	var need_to_render = false;
        var cell = IPython.notebook.get_cell(c);
        if (cell.cell_type == "markdown") {
            var cell_text = cell.get_text();
	    var re = new RegExp("\\[CITE\\]\\(\#cite-[^\\)]+\\)");
	    var match = cell_text.match(re);
	    while (match) {
		var citation = match[0].slice(7, -1); // #cite-...
		var cite = citations[citation];
		var reference = make_reference(cite, refs);
		cell_text = cell_text.replace(re, "<a name=\"ref-" + refs + "\"/>[" + reference + "](" + citation + ")");
		cell.set_text(cell_text);
		need_to_render = true;
		match = cell_text.match(re);
		refs++;
	    }
	}
	if (need_to_render) {
	    cell.unrender();
	    cell.render();
	}
    }
}

function make_reference(cite, refs) {
    // APA style, for now:
    if (cite == undefined) {
	return "(UNDEFINED)";
    } else {
	return "(" +  get_surnames(cite, refs) + ", " + cite["YEAR"] + ")";
    }
}

function get_surnames(cite, refs) {
    // could do something based on which ref (first, or later) this is
    // refs is the current ref count
    // cite["REFS"] is refs used for this cite
    var author_list = [];
    var authors = cite["AUTHOR"];
    var state = "surname";
    var current = "";
    var split = split_authors(authors);
    for (var index in split) {
	var word = split[index];
	if (state == "surname") {
	    if (word == ",") {
		author_list.push(current);
		current = "";
		state = "given";
	    } else {
		current += word;
	    }
	} else if (state == "given") {
	    if (word == "and") {
		state = "surname";
	    } 
	    // else, skip over given names
	}
    }
    // author_list is now list of author surnames
    // ["Smith"], ["Smith", "Jones"]...
    var retval = "";
    if (author_list.length <= 5) {
	if (author_list.length == 1) { // only one
	    retval = author_list[0];
	} else {
	    for (var i = 0; i < author_list.length; i++) {
		var surname = author_list[i];
		if (i == (author_list.length - 1)) { // last one, more than one
		    retval += " and ";
		    retval += surname;
		} else {
		    if (retval != "") { // add comma between surnames
			retval += ", ";
		    } 
		    retval += surname;
		}
	    }
        }
    } else {
	retval = author_list[0] + "et al.";
    }
    return retval;
}

function split_authors(string) {
    // string is something like "van Maxwell, Bruce S. and Meeden, Lisa"
    // returns ["van", "Maxwell", ",", "Bruce", "S.", "and", "Meeden", "Lisa"]
    var retval = [];
    var current = "";
    for (var i = 0; i < string.length; i++) {
	var ch = string.charAt(i);
	if (ch == " ") {
	    if (current != "") {
		retval.push(current);
		current = "";
	    }
	} else if (ch == ",") {
	    retval.push(current);
	    current = "";
	    retval.push(ch);
	} else {
	    current += ch;
	}
    }
    if (current != "")
	retval.push(current);
    return retval;
}

function create_reference_section(citations) {
    // If there is a References section, replace it:
    var reference_cell = find_cell("markdown", "#+References");
    var cells = IPython.notebook.get_cells();
    // default to top-level heading:
    var references = "#References\n\n";
    if (reference_cell == undefined) {
        reference_cell = IPython.notebook.select(cells.length-1).insert_cell_below("markdown");
    } else {
	// already exists:
	references = reference_cell.get_text().match("#+References")[0] + "\n\n";
    }
    var citation;
    for (citation in citations) {
        var cite = citations[citation];
	if (cite != undefined) {
            var ref_index;
            references = references + "<a name=\"" + citation.substring(1) + "\"/><sup>"
            for (ref_index in cite["REFS"]) {
		var refs = cite["REFS"][ref_index]
		references += "[^](#ref-" +  refs + ") "
            }
            references += "</sup>" + cite["AUTHOR"] + ". " + cite["YEAR"] + ". _" + cite["TITLE"] + "_." + "\n\n";
	}
    }
    reference_cell.unrender();
    reference_cell.set_text(references);
    reference_cell.render();
}

function get_citations() {
    // Get all citations in this notebook
    // citations are indicated by:
    // [...](#cite-KEY)
    // Returns dictionary with keys of #cite-KEY
    var citations = {};
    var refs = 1;
    for (var c in IPython.notebook.get_cells()) {
        var cell = IPython.notebook.get_cell(c);
        if (cell.cell_type == "markdown") {
            var cell_text = cell.get_text();
            var re = new RegExp("\\[.*?\\]\\((\#cite-.*?)\\)", "g");
            var match;
            while (match = re.exec(cell_text)) {               
                if (match[1] in citations) {
                    citations[match[1]]["REFS"].push(refs);                    
                } else {
                    var citation = match[1];
                    var lookup = document.bibliography[citation.substring(6).toUpperCase()];
		    if (lookup != undefined) {
			lookup["REFS"] = [refs]
			citations[match[1]] = lookup;
		    }
                }
                refs++;
            }
        }
    }
    return citations;
}

function parse_json(string) {
    // FIXME: allow JSON bibtex data
    return {type: "techreport", key: "meeden-1999"}
}

function parse_bibtex(string) {
    var parser = new BibtexParser();
    parser.setInput(string);
    parser.bibtex();
    // {KEY: {AUTHOR:..., BIB_KEY:...}}
    return parser.getEntries();
}

function read_bibliography() {
    // Read the Bibliography notebook
    document.bibliography = {};
    // First, check to see if there is a <!--bibtex here
    var bibtex = find_cell(".*", "<!--bibtex");
    if (bibtex != undefined) {
	var cell_text = bibtex.get_text().replace(/^<!--bibtex/, "");
	cell_text = cell_text.replace(/-->\s*$/, "");
	var json = parse_bibtex(cell_text);
        $.extend(document.bibliography, json);
    } else {
	// if not, read from Bibliography.ipynb in top-level directory:
	// Wait for result:
	$.ajaxSetup({ "async": false });
	$.getJSON("/api/notebooks/Bibliography.ipynb", function(data) {
            var index;
            for (index in data.content.worksheets[0].cells) {
		var cell = data.content.worksheets[0].cells[index];
                var json;
		var cell_text;
		if (cell.cell_type == "markdown") {
		    cell_text = cell.source;
		} else if (cell.cell_type == "raw") {
		    cell_text = cell.source;
		} else if (cell.cell_type == "code") {
		    cell_text = cell.input;
		} else {
		    continue;
		}
                if (cell_text.match(/^<!--bibtex/)) {
		    cell_text = cell_text.replace(/^<!--bibtex/, "");
		    cell_text = cell_text.replace(/-->s*$/, "");
		    json = parse_bibtex(cell_text);
                } else if (cell_text.match(/^<!--json/)) {
		    json = parse_json(cell_text);
                } else {
		    // skip this cell
		    continue;
                }
                // json is a dict keyed by KEY
                $.extend(document.bibliography, json);
            }
	});
    }
}

function show_bibliography() {
    // Read the Bibliography notebook
    $.getJSON("/api/notebooks/Bibliography.ipynb", function(data) {
      var items = [];
      $.each(data, function(key, val) {
          items.push("<tr><td>" + key + ": </td><td>" + val + "</td></tr>");
      });

      element.html($("<table/>", {
          "border": 3,
        "class": "my-new-list",
        html: items.join("")
      }).appendTo("body"));
    });
}

function toggle_columns(evt, input_cell) {
    // Toggle code cells into two columns
    var cells = IPython.notebook.get_cells();
    var cell;

    if (input_cell == undefined) {
	cell = IPython.notebook.get_selected_cell();
    } else {
	cell = input_cell;
    }

    // only toggle columns/rows if code cell:
    if (cell.cell_type == "code") {
    // get the div cell:
        var div = cell.element;
        if (cell.metadata.format == "tab") {
            var toRemove = cell.element[0].getElementsByClassName("tabs");
            if (toRemove.length > 0) {
                var length = toRemove.length;
                for (var i = 0; i < length; i++) {
                    toRemove[0].parentNode.removeChild(toRemove[0]);
                }
                cell.element[0].getElementsByClassName("input")[0].className = 'input';
                cell.element[0].getElementsByClassName("output_wrapper")[0].className = 'output_wrapper';
                cell.element[0].getElementsByClassName("input")[0].id = '';
                cell.element[0].getElementsByClassName("output_wrapper")[0].id = '';
	        cell.metadata.format = "row";
            }
        }
        if (div.css("box-orient") == "vertical") {
            div.css("box-orient", "horizontal");
            div.css("flex-direction", "row");
            var input = div[0].getElementsByClassName("input")[0];
            input.style.width = "50%";
            var output = div[0].getElementsByClassName("output_wrapper")[0];
            output.style.width = "50%";
	    cell.metadata.format = "column";
        } else {
	    //default:
            div.css("box-orient", "vertical");
            div.css("flex-direction", "column");
            var input = div[0].getElementsByClassName("input")[0];
            input.style.width = "";
            var output = div[0].getElementsByClassName("output_wrapper")[0];
            output.style.width = "";
	    cell.metadata.format = "row";
        }
    }
}
    
function toggle_tabs(evt, input_cell) {
    // Toggle code cells into Input/Output tabs
    var cells = IPython.notebook.get_cells();
    var cell;
    var tabLinks = new Array();
    var contentDivs = new Array();

    if (input_cell == undefined) {
        cell = IPython.notebook.get_selected_cell();
    } else {
	cell = input_cell;
    }

    var toRemove = cell.element[0].getElementsByClassName("tabs");
    if (toRemove.length > 0) {
        var length = toRemove.length;
        for (var i = 0; i < length; i++) {
            toRemove[0].parentNode.removeChild(toRemove[0]);
        }
        cell.element[0].getElementsByClassName("input")[0].className = 'input';
        cell.element[0].getElementsByClassName("output_wrapper")[0].className = 'output_wrapper';
        cell.element[0].getElementsByClassName("input")[0].id = '';
        cell.element[0].getElementsByClassName("output_wrapper")[0].id = '';
	cell.metadata.format = "row";
    } else if (cell.cell_type == "code") {
        if (cell.metadata.format == "column") {
            var tempDiv = cell.element;
            tempDiv.css("box-orient", "vertical");
            tempDiv.css("flex-direction", "column");
            var input = tempDiv[0].getElementsByClassName("input")[0];
            input.style.width = "";
            var output = tempDiv[0].getElementsByClassName("output_wrapper")[0];
            output.style.width = "";
	    cell.metadata.format = "row";
        }
        var div = document.createElement("div");
        cell.element[0].insertBefore(div, cell.element[0].getElementsByClassName("input")[0]);
	
        div.className = "tabs";
        div.innerHTML = '<ul id="tabs"><li><a href="#input_tab" class>Input</a></li><li><a href="#output_tab" class>Output</a></li></ul>';
	
        var inputDiv = cell.element[0].getElementsByClassName("input")[0];
        var outputDiv = cell.element[0].getElementsByClassName("output_wrapper")[0];
	
        inputDiv.id = "input_tab";
        outputDiv.id = "output_tab";
	inputDiv.className = 'input tabContent hide';
        outputDiv.className = 'output_wrapper tabContent';
	cell.metadata.format = "tab";
        init();
    }
    
    function init() {
	// Grab the tab links and content divs from the page
	var tabListItems = cell.element[0].getElementsByTagName("ul")[0].childNodes;
	for (var i = 0; i < tabListItems.length; i++) {
            if (tabListItems[i].nodeName == "LI") {
		var tabLink = getFirstChildWithTagName(tabListItems[i], 'A');
		var id = getHash(tabLink.getAttribute('href'));
		tabLinks[id] = tabLink;
		if (id == "input_tab") {
                    contentDivs[id] = cell.element[0].getElementsByClassName("input")[0];
		} else {
                    contentDivs[id] = cell.element[0].getElementsByClassName("output_wrapper")[0];
		}
            }
	}  
	
	// Assign onclick events to the tab links, and
	// highlight the second tab
	var i = 0;
	
	for (var id in tabLinks) {
            tabLinks[id].onclick = showTab;
            tabLinks[id].onfocus = function() { this.blur() };
            if (i == 1) tabLinks[id].className = 'selected';
            i++;
	}
	
	// Hide all content divs except the first
	var i = 0;
	
	for (var id in contentDivs) {
            if (i != 1) {
		if (contentDivs[id].className.indexOf("input") != -1) {
                    contentDivs[id].className = 'input tabContent hide';
		} else {
                    contentDivs[id].className = 'output_wrapper tabContent hide';
		}
            }
            i++;
	}
    }
    
    function showTab() {
	var selectedId = getHash(this.getAttribute('href'));
	
	// Highlight the selected tab, and dim all others.
	// Also show the selected content div, and hide all others.
	for (var id in contentDivs) {
            if (id == selectedId) {
		tabLinks[id].className = 'selected';
		if (contentDivs[id].className.indexOf("input") != -1) {
                    contentDivs[id].className = 'input tabContent';
		} else {
                    contentDivs[id].className = 'output_wrapper tabContent';
		}
            } else {
		tabLinks[id].className = '';
		if (contentDivs[id].className.indexOf("input") != -1) {
                    contentDivs[id].className = 'input tabContent hide';
		} else {
                    contentDivs[id].className = 'output_wrapper tabContent hide';
		}
            }
	}
	
	// Stop the browser following the link
	return false;
    }
    
    function getFirstChildWithTagName(element, tagName) {
	for (var i = 0; i < element.childNodes.length; i++) {
            if (element.childNodes[i].nodeName == tagName) return element.childNodes[i];
	}
    }
    
    function getHash(url) {
	var hashPos = url.lastIndexOf ('#');
	return url.substring(hashPos + 1);
    }
}

function checkForFormatting() {
    // Check to see if code cells have metadata formatting (two-column, tabs)
    // and toggle if they do.
    var cells = IPython.notebook.get_cells();
    for (var i = 0; i < cells.length; i++) {
        var cell = cells[i];
        if (cell.cell_type == "code") {
            if (cell.metadata.format == "tab") {
                toggle_tabs("temp", cell);
            } else if (cell.metadata.format == "column") {
		toggle_columns("temp", cell);
	    }
        }
    }
}

