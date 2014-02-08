// IPython kernel backend in C#
// Doug Blank

using System.Collections; // IDictionary
using System.Collections.Generic; // Dictionary

public static class Widgets {

    public static Dictionary<string, Widget> comm_id = new Dictionary<string, Widget>();


    public static void onReceive(string id, IDictionary<string, object> data) {
	System.Console.WriteLine(id);
	if (Widgets.comm_id.ContainsKey(id)) {
	    Widgets.comm_id[id].onReceive(data);
	} else {
	    System.Console.Error.WriteLine("Invalid comm_id: " + id);
	}
    }

    public static Widget register(Widget widget) {
	comm_id[widget.comm_id] = widget;
	return widget;
    }

    public class Widget {
	public string target_name;
	public Dictionary<string,object> data;
	public string comm_id;

	public Widget() {
	    target_name = "WidgetModel";
	    data = new Dictionary<string, object>();
	    comm_id = System.Guid.NewGuid().ToString();
	    // Register this widget:
	    Widgets.register(this);
	}

	public void onReceive(IDictionary<string, object> data) {
	    // handle comm_msg for widget
	    // content: {"data":{"method":"backbone","sync_data":{"value":0.8}},"comm_id":"90e92a12f92747aeaf9c6a79e51855eb"}
	    this.data["value"] = ((Dictionary<string,object>)data["sync_data"])["value"];
	}

	public IDictionary<string, object> GetInitialState() {
	    return new Dictionary<string, object> {
		{"target_name", target_name},
  	        {"data", new Dictionary<string, object>()},
		{"comm_id", comm_id}
	    };
	}

	public IDictionary<string, object> GetDisplay() {
	    return new Dictionary<string, object> {
  	        {"data", new Dictionary<string, object> {
			{"method", "display"}
		    }},
		{"comm_id", comm_id}
	    };
	}

	public IDictionary<string, object> GetState() {
	    return new Dictionary<string, object> {
		{"data", data},
		{"comm_id", comm_id}
	    };
	}
    }

    public class FloatSliderView : Widget {

	public FloatSliderView() : base() {
	    data["state"] = new Dictionary<string,object> {
		{"_view_name",  "FloatSliderView"},
		{"orientation", "horizontal"},
		{"min",         0.0},
		{"max",         100.0},
		{"_css",        new Dictionary<string,object>()},
		{"value",       0.0},
		{"readout",     true},
		{"disabled",    false},
		{"visible",     true},
		{"step",        0.1},
		{"description", ""},
	    };
	    data["method"] = "update";
	}
    }
}
