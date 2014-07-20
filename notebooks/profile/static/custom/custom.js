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
      // add more buttons here if needed.
  ]);
});

function section_label() {
    var cells = IPython.notebook.get_cells();
    var levels = [0,0,0,0,0,0];
    var current_level = 1;
    var flag = false;
    var alert_text = "";
    var alert_flag = false;
    var remove_numbering = true;
    
    for (var i = 0; i < cells.length; i++){
	var cell = cells[i];
	if (cell.cell_type == "heading"){
            var level = cell.level;
            
            if (level >= current_level){ //just keep incrementing
		current_level = level;
		levels[level-1]++;
            } else {                    //went back a level
		levels[current_level-1] = 0;
		
		if (current_level-level > 1){ //Skipped levels in between
                    for(var j = 1; j < current_level-level; j++){ //back-prop the zeros
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
            for (var k = 0; k < level; k++){
		if (levels[k] == 0){
                    if (!error){
			error_heading_label = heading_label;
			error = true;
			error_no_begin = k;
                    } else {
			error_no_end = k + 2;
                    }
		}
		heading_label += levels[k];
		if (level-k == 1 && level > 1){
                    break;
		}
		heading_label += ".";
            }
	    
            if (error){
		if (error_no_end == 0){
                    error_no_end = error_no_begin + 2;
		}
		if (error_heading_label == ""){
                    if (!flag){
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
            
            if (match){
		heading_text = heading_label + " " + match[1];
            } else {
		heading_text = heading_label;
            }
            
            if (old_header != heading_text){
		remove_numbering = false;
		replace_links(old_header, heading_text);
            }
            
            cell.unrender();
	    heading_text = heading_text.trim();
            cell.set_text(heading_text);
            cell.render();
	}
    }
    
    if (alert_flag){
	alert(alert_text);
    }
    
    if (remove_numbering){
	for (var i = 0; i < cells.length; i++){
            var cell = cells[i];
            if (cell.cell_type == "heading"){
		var heading_text = cell.get_text();
		old_header = heading_text;
		var re = /(?:\d*\.*)*\s*(.*)/;
		var match = heading_text.match(re);
		if (match){
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
    for(var i = 0; i < cells.length; i++){
        var cell = cells[i];
        if (cell.cell_type == "markdown") {
	    var cell_text = cell.get_text();
	    if (cell_text.match(/^#Table of Contents/)) {
		table_of_contents();
		break;
	    }
	}
    }
}

function replace_links(old_header, new_header){
    new_header = new_header.trim();
    var cells = IPython.notebook.get_cells();
    for(var i = 0; i < cells.length; i++){
        var cell = cells[i];
        if (cell.cell_type == "markdown") {
            var cell_text = cell.get_text();
	    if (cell_text.match(/^#Table of Contents/)) {
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
            re_string = "(^\\[.*\\]:\\s*#)" + re_string + "(.*)$";
            
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


function table_of_contents() {
    var toc_cell;
    var found = false;
    
    var cells = IPython.notebook.get_cells();
    for (var x = 0; x < cells.length; x++) {
	var temp = cells[x];
	if (temp.cell_type == "markdown"){
            var temp_text = temp.get_text();
            var re = /^#Table of Contents/;
            if (re.test(temp_text)){
		toc_cell = cells[x];
		found = true;
		break;
            }
	}
    }
    
    if (!found){
	IPython.notebook.select(0).insert_cell_above("markdown"); //Create a new markdown cell at the top of the Notebook
	toc_cell = IPython.notebook.get_cell(0);
	cells = IPython.notebook.get_cells();
    }
    
    var toc_text = "#Table of Contents\n";
    var prev_lev = 0;
    
    for (var i = 0; i < cells.length; i++){
	var cell = cells[i];
	if (cell.cell_type == "heading"){
            
            if (cell.level - prev_lev > 1){ //Skipped levels. Enter Dummy levels
		for (var x = 0; x < ((cell.level - prev_lev) - 1); x++){
                    for (var y = 0; y < (prev_lev + x); y++){
			toc_text += "\t";
                    }
                    toc_text += "* &nbsp;\n";
		}
            }
            
            var cell_text = cell.get_text();
            for (var j = 0; j < cell.level -1; j++){ //Loop to add the proper amount of tabs based on header level
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
    //require(['/static/custom/bibtex.js']);
    //console.log("----------------------------------------- Start")
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
		console.log(citation);
		var cite = citations[citation];
		console.log(cite)
		cell_text = cell_text.replace(re, "<a name=\"ref-" + refs + "\"/>[(" + cite["AUTHOR"] + ", " + cite["YEAR"] + ")](" + citation + ")");
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

function create_reference_section(citations) {
    // If there is a References section, replace it:
    var reference_cell;
    var index = -1;
    var cells = IPython.notebook.get_cells();
    for(var i = 0; i < cells.length; i++){
        var cell = cells[i];
        if (cell.cell_type == "markdown") {
            var cell_text = cell.get_text();
            if (cell_text.match(/^#References/)) {
                index = i;
                break;
            }
        }
    }
    if (index == -1) {
        reference_cell = IPython.notebook.select(IPython.notebook.ncells - 1).insert_cell_below("markdown");
    } else {
        reference_cell = IPython.notebook.get_cell(index);
    }
    var references = "#References\n\n";
    var citation;
    for (citation in citations) {
        var cite = citations[citation];
        var ref_index;
        references = references + "<a name=\"" + citation.substring(1) + "\"/><sup>"
        for (ref_index in cite["REFS"]) {
            var refs = cite["REFS"][ref_index]
            references += "[^](#ref-" +  refs + ") "
        }
        references += "</sup>" + cite["AUTHOR"] + ". " + cite["YEAR"] + ". _" + cite["TITLE"] + "_." + "\n\n";
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
                    lookup["REFS"] = [refs]
                    citations[match[1]] = lookup;
                }
                refs++;
            }
        }
    }
    return citations;
}

function parse_json(string) {
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
    // Wait for result:
    $.ajaxSetup( { "async": false } );
    $.getJSON("/api/notebooks/Bibliography.ipynb", function( data ) {
        var index;
        for (index in data.content.worksheets[0].cells) {
            var cell = data.content.worksheets[0].cells[index];
            if (cell.cell_type == "code") {
                var json;
                if (cell.input.match(/^%%bibtex/)) {
                    var cell_text = cell.input.replace(/^%%bibtex/, "");
                    json = parse_bibtex(cell_text);
                } else if (cell.input.match(/^%%json/)) {
                    json = parse_json(cell.input);
                } else {
                    // skip this cell
                    continue;
                }
                // json is a dict keyed by KEY
                $.extend(document.bibliography, json);
            }
        }
    });
}

function show_bibliography() {
    // Read the Bibliography notebook
    $.getJSON("/api/notebooks/Bibliography.ipynb", function( data ) {
      var items = [];
      $.each( data, function( key, val ) {
          items.push( "<tr><td>" + key + ": </td><td>" + val + "</td></tr>" );
      });

      element.html($( "<table/>", {
          "border": 3,
        "class": "my-new-list",
        html: items.join( "" )
      }).appendTo( "body" ));
    });
}
