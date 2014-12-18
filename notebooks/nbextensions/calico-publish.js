/**
 * Calico Jupyter Notebooks Extensions
 *
 * Copyright (c) The Calico Project
 * http://calicoproject.org/ICalico
 *
 * Released under the BSD Simplified License
 *
 **/

define(["require", "nbextensions/typo/typo"], function (require) {

    function publish_notebook() {
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
\n\
\n\
source = "Untitled.ipynb" \n\
dst = "~/Public/Untitled.ipynb" \n\
\n\
publish(source, dst)"""');
    }

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
    }
    
    return {
        load_ipython_extension : load_ipython_extension,
    };
});

