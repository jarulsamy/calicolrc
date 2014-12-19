/**
 * Calico Jupyter Notebooks Extensions
 *
 * Copyright (c) The Calico Project
 * http://calicoproject.org/ICalico
 *
 * Released under the BSD Simplified License
 *
 **/

define(["require"], function (require) {
    function publish_notebook() {
	// http://jupyter.cs.brynmawr.edu/user/dblank/notebooks/Calico/notebooks/BrainScrew/BrainScrew%20Examples.ipynb
	var base_url = document.URL.substr(0,document.URL.indexOf('/notebooks/'));
	var user = document.URL.substr(document.URL.indexOf('/user/') + 6);
	user = user.substr(0, user.indexOf('/notebooks/'));
	base_url = base_url.replace(/\/user\//g, "/hub/");
	// BrainScrew%20Examples.ipynb
	var filename = document.URL.substr(document.URL.lastIndexOf('/') + 1);
	if (filename.indexOf('?') > 0) {
	    filename = filename.substr(0, filename.indexOf('?'));
	}
	// Calico/notebooks/BrainScrew/BrainScrew%20Examples.ipynb
	var path = document.URL.substr(document.URL.indexOf('/notebooks/') + 11);
	path = path.substr(0, path.lastIndexOf('/'));
	// Replaces spaces:
	user = user.replace(/%20/g, " ");
	path = path.replace(/%20/g, " ");
	filename = filename.replace(/%20/g, " ");
	if (confirm("You want to publish this notebook?\n" + 
		    'user: "' + user + '"\n' +
		    'path: "' + path + '"\n' +
		    'filename: "' + filename + '"\n' +
		    'base_url: "' + base_url + '"\n' +
		    '"/home/' + user + '/' + path + filename + '" to \n' +
		    '"~/Public/' + path + filename + '"')) {
	    IPython.notebook.kernel.execute('"""%%python \n\
\n\
import os \n\
import shutil \n\
import stat \n\
import errno \n\
\n\
def publish(src, dst): \n\
    if dst.startswith("~"): \n\
        dst = os.path.expanduser(dst) \n\
    dst = os.path.abspath(dst) \n\
    # Create the path of the file if dirs do not exist: \n\
    path = os.path.dirname(os.path.abspath(dst)) \n\
    try: \n\
        os.makedirs(path) \n\
    except OSError as exc: # Python >2.5 \n\
        if exc.errno == errno.EEXIST and os.path.isdir(path): \n\
            pass \n\
        else: \n\
            raise \n\
    shutil.copyfile(src, dst) \n\
    os.chmod(dst, stat.S_IRUSR | stat.S_IWUSR | stat.S_IROTH | stat.S_IRGRP) \n\
\n\
publish("/home/' + user + '/' + path + filename + '", "~/Public/' + path + filename + '")"""');
	    alert("Your notebook is available at:\n" +
		  base_url + '/public/' + path.replace(/ /g, "%20") + filename.replace(/ /g, "%20"));
	}
    };

    var load_ipython_extension = function () {
	// Put a button on the toolbar:
	if (!IPython.toolbar) {
	    $([IPython.events]).on("app_initialized.NotebookApp", 
				   add_toolbar_buttons);
	    return;
	} else {
	    add_toolbar_buttons();
	}
    };

    var add_toolbar_buttons = function () {
	var version = IPython.version.substring(0, 1);
	IPython.toolbar.add_buttons_group([
	    // select your icon from http://fortawesome.github.io/Font-Awesome/icons
	    {
		'label'   : 'Publish this notebook',
		'icon'    : (version === "2") ? 'icon-link' : 'fa-link',
		'callback': publish_notebook
	    }
	]);
    };
    
    return {
        load_ipython_extension : load_ipython_extension,
    };
});
