// IPython kernel backend in C#
// Doug Blank

using System.Collections; // IDictionary
using System.Collections.Generic; // Dictionary

public static class Widgets {

    public static Dictionary<string, Widget> comm_id = new Dictionary<string, Widget>();

    public static void Dispatch(string id, IDictionary<string, object> data) {
	System.Console.WriteLine(id);
	if (Widgets.comm_id.ContainsKey(id)) {
	    Widgets.comm_id[id].Dispatch(data);
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
	public System.Func<object,object> on_click_func;
	public Dictionary<string,System.Func<string,string,object>> on_value_change_func = new Dictionary<string,System.Func<string,string,object>>();

	public Widget() {
	    target_name = "WidgetModel";
	    data = new Dictionary<string, object>();
	    comm_id = System.Guid.NewGuid().ToString();
	    // Register this widget:
	    Widgets.register(this);
	}

	public void Dispatch(IDictionary<string, object> data) {
	    // handle comm_msg for widget
	    if (data.ContainsKey("sync_data")) {
		// data: {"method":"backbone","sync_data":{"value":0.8}}
		this.data["value"] = ((Dictionary<string,object>)data["sync_data"])["value"];
	    }
	    if (data.ContainsKey("content")) {
		// data: {"content":{"event":"click"},"method":"custom"}
		string evt = ((Dictionary<string,object>)data["content"])["event"].ToString();
		if (evt == "click") {
		    onClick();
		}
	    }
	}

	public void on_click(System.Func<object,object> function) {
	    on_click_func = function;
	}

	public void on_value_change(System.Func<string,string,object> function, string value_name) {
	    on_value_change_func[value_name] = function;
	}

	public void onClick() {
	    if (on_click_func != null) 
		on_click_func(this);
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

	public void set(string value_name, object value) {
	    data[value_name] = value;
	    // Need to send comm_msg:
	    // data: {"state":{"_view_name":"ButtonView","disabled":false,"_css":{},"description":"XXX","visible":true},"method":"update"}
	}

	public object get(string value_name) {
	    return data[value_name];
	}
    }

    public class FloatSliderWidget : Widget {
	public FloatSliderWidget(double min=0.0, double max=100.0, double step=0.1, 
				 double value=0.0, string description="",
				 string orientation="horizontal", bool readout=true, 
				 bool disabled=false, bool visible=true) : base() {
	    data["state"] = new Dictionary<string,object> {
		{"_view_name",  "FloatSliderView"},
		{"orientation", orientation},
		{"min",         min},
		{"max",         max},
		{"_css",        new Dictionary<string,object>()},
		{"value",       value},
		{"readout",     readout},
		{"disabled",    disabled},
		{"visible",     visible},
		{"step",        step},
		{"description", description},
	    };
	    data["method"] = "update";
	}
    }

    public class ButtonWidget : Widget {
	public ButtonWidget(string description="", bool disabled=false, 
			    bool visible=true, string css="") : base() {
	    data["state"] = new Dictionary<string,object> {
		{"_view_name",  "ButtonView"},
		{"_css",        new Dictionary<string,object>()},
		{"disabled",    disabled},
		{"visible",     visible},
		{"description", description},
	    };
	    data["method"] = "update";
	}
    }

    /*
      BoundedFloatTextWidget
      BoundedIntTextWidget
      ButtonWidget
      CheckboxWidget
      DropdownWidget
      FloatProgressWidget
      FloatSliderWidget - ok
      FloatTextWidget
      HTMLWidget
      ImageWidget
      IntProgressWidget
      IntSliderWidget
      IntTextWidget
      LatexWidget
      RadioButtonsWidget
      SelectWidget
      TextWidget
      TextareaWidget
      ToggleButtonWidget
      ToggleButtonsWidget
    */

}
