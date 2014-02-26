// IPython kernel backend in C#
// Doug Blank

using System; // Int64
using System.Collections; // IDictionary
using System.Collections.Generic; // Dictionary

public static class Widgets {

    public static Dictionary<string, Widget> comm_id = new Dictionary<string, Widget>();

    public static void Dispatch(string id, 
				IDictionary<string, object> data, 
				IDictionary<string, object> parent_header) {
	if (Widgets.comm_id.ContainsKey(id)) {
	    Widgets.comm_id[id].Dispatch(data, parent_header);
	} else {
	    System.Console.Error.WriteLine("Invalid comm_id: " + id);
	}
    }

    public static Widget register(Widget widget) {
	comm_id[widget.comm_id] = widget;
	return widget;
    }

    public class Callback {
	public System.Func<string,object,object> on_value_change_callback;

	public Callback(System.Func<string,object,object> on_value_change_callback) {
	    this.on_value_change_callback = on_value_change_callback;
	}
    }

    public class Widget {
	public string target_name;
	public IDictionary<string,object> data;
	public string comm_id;
	public System.Func<object,object> on_click_func;
	public Dictionary<string,Callback> on_value_change_callback = new Dictionary<string,Callback>();
	ZMQServer.Session session = null;
	public int execution_count;

	public Widget(ZMQServer.Session session) {
	    this.session = session;
	    target_name = "WidgetModel";
	    data = new Dictionary<string, object>();
	    data["_view_name"] = "WidgetView";
	    data["state"] = new Dictionary<string, object>();
	    data["_css"] = get_css();
	    data["disabled"] = false;
	    data["visible"] = true;
	    data["description"] = "A generic Widget";
	    data["method"] = "update";
	    comm_id = System.Guid.NewGuid().ToString();
	    // Register this widget:
	    Widgets.register(this);
	}

	// Attributes
	public bool visible {
	    get { return Convert.ToBoolean(get("visible")); }
	    set { set("visible", Convert.ToBoolean(value)); }
	}
	public bool disabled {
	    get { return Convert.ToBoolean(get("disabled")); }
	    set { set("disabled", Convert.ToBoolean(value)); }
	}
	public string _view_name {
	    get { return Convert.ToString(get("_view_name")); }
	    set { set("_view_name", Convert.ToString(value)); }
	}

	public void clear_output(bool wait) {
	    session.clear_output(this, wait);
	}

	public void Dispatch(IDictionary<string, object> data,
			     IDictionary<string, object> parent_header) {
	    // handle comm_msg for widget
	    session.SetOutputs(9999, parent_header);
	    // Let the frontend know that we are busy:
	    session.update_status("busy", parent_header);
	    if (data.ContainsKey("sync_data")) {
		// data: {"method":"backbone","sync_data":{"value":0.8}}
		// Get names of attributes to sync:
		Dictionary<string,object> value_names = (Dictionary<string,object>)data["sync_data"];
		// session ID ignored, just make it non-zero:
		foreach (String value_name in value_names.Keys) {
		    object value = value_names[value_name];
		    // first, change the value in the widget dictionary:
		    ((Dictionary<string,object>)this.data["state"])[value_name] = value;
		    // Next, call any associated callbacks:
		    if (on_value_change_callback.ContainsKey(value_name)) {
			Callback fcb = on_value_change_callback[value_name];
			try {
			    fcb.on_value_change_callback(value_name, value);
			} catch (Exception e) {
			    System.Console.Error.WriteLine(e.ToString());
			}
		    }
		}
	    }
	    if (data.ContainsKey("content")) {
		// data: {"content":{"event":"click"},"method":"custom"}
		string evt = ((Dictionary<string,object>)data["content"])["event"].ToString();
		if (evt == "click") {
		    onClick();
		} else {
		    System.Console.Error.WriteLine("Need to handle event: " + evt);
		}
	    }
	    // And back to idle:
	    session.update_status("idle", parent_header);
	    session.SetOutputs(0, null); // wait till after widget displays
	}

	public Dictionary<string,object> get_css() {
	    // FIXME: return values from set_css
	    return new Dictionary<string,object>();
	}

	public void set_css(string name, object value) {
	}

	public void on_click(System.Func<object,object> function) {
	    on_click_func = function;
	}

	public void on_value_change(System.Func<string,object,object> function, string value_name) {
	    on_value_change_callback[value_name] = new Callback(function);
	}

	public void onClick() {
	    if (on_click_func != null) {
		try {
		    on_click_func(this);
		} catch (Exception e) {
		    System.Console.Error.WriteLine(e.ToString());
		}
	    }
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
	    ((Dictionary<string,object>)data["state"])[value_name] = value;
	    // not in reaction to a message, no parent_header
	    session.update_state(this, new Dictionary<string, object>());
	}

	public object get(string value_name) {
	    return ((Dictionary<string,object>)data["state"])[value_name];
	}
    }

    public class BoundedFloatTextWidget : Widget {
	// Attributes
	public string description {
	    get { return Convert.ToString(get("description")); }
	    set { set("description", Convert.ToString(value)); }
	}
	public double min {
	    get { return Convert.ToDouble(get("min")); }
	    set { set("min", Convert.ToDouble(value)); }
	}
	public double max {
	    get { return Convert.ToDouble(get("max")); }
	    set { set("max", Convert.ToDouble(value)); }
	}
	public double step {
	    get { return Convert.ToDouble(get("step")); }
	    set { set("step", Convert.ToDouble(value)); }
	}
	public double value {
	    get { return Convert.ToDouble(get("value")); }
	    set { set("value", Convert.ToDouble(value)); }
	}

	public BoundedFloatTextWidget(ZMQServer.Session session,
				      double min=0.0, 
				      double max=100.0, 
				      double step=0.1, 
				      double value=0.0, 
				      string description="", 
				      bool disabled=false, 
				      bool visible=true) : base(session) {
	    data["state"] = new Dictionary<string,object> {
		{"_view_name",  "BoundedFloatTextView"},
		{"_css",        get_css()},
		{"disabled",    disabled},
		{"visible",     visible},
		{"description", description},
		{"min",         min},
		{"max",         max},
		{"step",        step},
		{"value",       value},
	    };
	    data["method"] = "update";
	}
    }

    public class BoundedIntTextWidget : Widget {
	// Attributes
	public string description {
	    get { return Convert.ToString(get("description")); }
	    set { set("description", Convert.ToString(value)); }
	}
	// FIXME: Should these be ints or strings?
	public Int64 min {
	    get { return Convert.ToInt64(get("min")); }
	    set { set("min", Convert.ToInt64(value)); }
	}
	public Int64 max {
	    get { return Convert.ToInt64(get("max")); }
	    set { set("max", Convert.ToInt64(value)); }
	}
	public Int64 step {
	    get { return Convert.ToInt64(get("step")); }
	    set { set("step", Convert.ToInt64(value)); }
	}
	public Int64 value {
	    get { return Convert.ToInt64(get("value")); }
	    set { set("value", Convert.ToInt64(value)); }
	}

	public BoundedIntTextWidget(ZMQServer.Session session,
				    Int64 min=0, 
				    Int64 max=100, 
				    Int64 step=1, 
				    Int64 value=0, 
				    string description="", 
				    bool disabled=false, 
				    bool visible=true) : base(session) {
	    data["state"] = new Dictionary<string,object> {
		{"_view_name",  "BoundedIntTextView"},
		{"_css",        get_css()},
		{"disabled",    disabled},
		{"visible",     visible},
		{"description", description},
		{"min",         min},
		{"max",         max},
		{"step",        step},
		{"value",       value},		
	    };
	    data["method"] = "update";
	}
    }

    public class ButtonWidget : Widget {
	// Attributes
	public string description {
	    get { return Convert.ToString(get("description")); }
	    set { set("description", Convert.ToString(value)); }
	}

	public ButtonWidget(ZMQServer.Session session,
			    string description="", 
			    bool disabled=false, 
			    bool visible=true) : base(session) {
	    data["state"] = new Dictionary<string,object> {
		{"_view_name",  "ButtonView"},
		{"_css",        get_css()},
		{"disabled",    disabled},
		{"visible",     visible},
		{"description", description},
	    };
	    data["method"] = "update";
	}
    }

    public class CheckboxWidget : Widget {
	// Attributes
	public string description {
	    get { return Convert.ToString(get("description")); }
	    set { set("description", Convert.ToString(value)); }
	}
	public bool value {
	    get { return Convert.ToBoolean(get("value")); }
	    set { set("value", Convert.ToBoolean(value)); }
	}

	public CheckboxWidget(ZMQServer.Session session,
			      string description="", 
			      bool value=false,
			      bool disabled=false, 
			      bool visible=true) : base(session) {
	    data["state"] = new Dictionary<string,object> {
		{"_view_name",  "CheckboxView"},
		{"_css",        get_css()},
		{"disabled",    disabled},
		{"visible",     visible},
		{"description", description},
		{"value",       value},
	    };
	    data["method"] = "update";
	}
    }

    public class DropdownWidget : SelectionWidget {
	public DropdownWidget(ZMQServer.Session session,
			    string description="", 
			    bool disabled=false, 
			    bool visible=true) : 
	        base(session, description, disabled, visible) {
	    ((IDictionary<string,object>)data["state"])["_view_name"] = "DropdownView";
	}
    }

    public class FloatProgressWidget : Widget {
	// Attributes
	public string description {
	    get { return Convert.ToString(get("description")); }
	    set { set("description", Convert.ToString(value)); }
	}
	public double min {
	    get { return Convert.ToDouble(get("min")); }
	    set { set("min", Convert.ToDouble(value)); }
	}
	public double max {
	    get { return Convert.ToDouble(get("max")); }
	    set { set("max", Convert.ToDouble(value)); }
	}
	public double step {
	    get { return Convert.ToDouble(get("step")); }
	    set { set("step", Convert.ToDouble(value)); }
	}
	public double value {
	    get { return Convert.ToDouble(get("value")); }
	    set { set("value", Convert.ToDouble(value)); }
	}

	public FloatProgressWidget(ZMQServer.Session session,
				   double min=0.0, 
				   double max=100.0, 
				   double step=0.1, 
				   double value=0.0, 
				   string description="", 
				   bool disabled=false, 
				   bool visible=true) : base(session) {
	    data["state"] = new Dictionary<string,object> {
		{"_view_name",  "FloatProgressView"},
		{"_css",        get_css()},
		{"disabled",    disabled},
		{"visible",     visible},
		{"description", description},
		{"min",         min},
		{"max",         max},
		{"step",        step},
		{"value",       value},
	    };
	    data["method"] = "update";
	}
    }

    public class FloatSliderWidget : Widget {
	// Attributes
	public double min {
	    get { return Convert.ToDouble(get("min")); }
	    set { set("min", Convert.ToDouble(value)); }
	}
	public double max {
	    get { return Convert.ToDouble(get("max")); }
	    set { set("max", Convert.ToDouble(value)); }
	}
	public double step {
	    get { return Convert.ToDouble(get("step")); }
	    set { set("step", Convert.ToDouble(value)); }
	}
	public double value {
	    get { return Convert.ToDouble(get("value")); }
	    set { set("value", Convert.ToDouble(value)); }
	}
	public string description {
	    get { return Convert.ToString(get("description")); }
	    set { set("description", Convert.ToString(value)); }
	}
	public string orientation {
	    get { return Convert.ToString(get("orientation")); }
	    set { set("orientation", Convert.ToString(value)); }
	}
	public bool readout {
	    get { return Convert.ToBoolean(get("readout")); }
	    set { set("readout", Convert.ToBoolean(value)); }
	}

	public FloatSliderWidget(ZMQServer.Session session,
				 double min=0.0, 
				 double max=100.0, 
				 double step=0.1, 
				 double value=0.0, 
				 string description="",
				 string orientation="horizontal", 
				 bool readout=true, 
				 bool disabled=false, 
				 bool visible=true) : base(session) {
	    data["state"] = new Dictionary<string,object> {
		{"_view_name",  "FloatSliderView"},
		{"_css",        get_css()},
		{"min",         min},
		{"max",         max},
		{"step",        step},
		{"value",       value},
		{"orientation", orientation},
		{"readout",     readout},
		{"disabled",    disabled},
		{"visible",     visible},
		{"description", description},
	    };
	    data["method"] = "update";
	}
    }

    public class FloatTextProgressWidget : Widget {
	// Attributes
	public string description {
	    get { return Convert.ToString(get("description")); }
	    set { set("description", Convert.ToString(value)); }
	}
	public double min {
	    get { return Convert.ToDouble(get("min")); }
	    set { set("min", Convert.ToDouble(value)); }
	}
	public double max {
	    get { return Convert.ToDouble(get("max")); }
	    set { set("max", Convert.ToDouble(value)); }
	}
	public double step {
	    get { return Convert.ToDouble(get("step")); }
	    set { set("step", Convert.ToDouble(value)); }
	}
	public double value {
	    get { return Convert.ToDouble(get("value")); }
	    set { set("value", Convert.ToDouble(value)); }
	}

	public FloatTextProgressWidget(ZMQServer.Session session,
				       double min=0.0, 
				       double max=100.0, 
				       double step=0.1, 
				       double value=0.0,
				       string description="", 
				       bool disabled=false, 
				       bool visible=true) : base(session) {
	    data["state"] = new Dictionary<string,object> {
		{"_view_name",  "FloatTextProgressView"},
		{"_css",        get_css()},
		{"disabled",    disabled},
		{"visible",     visible},
		{"description", description},
		{"min",         min},
		{"max",         max},
		{"step",        step},
		{"value",       value},
	    };
	    data["method"] = "update";
	}
    }

    public class FloatTextWidget : Widget {
	// Attributes
	public string description {
	    get { return Convert.ToString(get("description")); }
	    set { set("description", Convert.ToString(value)); }
	}
	public double value {
	    get { return Convert.ToDouble(get("value")); }
	    set { set("value", Convert.ToDouble(value)); }
	}

	public FloatTextWidget(ZMQServer.Session session,
			       double value=0.0,
			       string description="", 
			       bool disabled=false, 
			       bool visible=true) : base(session) {
	    data["state"] = new Dictionary<string,object> {
		{"_view_name",  "FloatTextView"},
		{"_css",        get_css()},
		{"disabled",    disabled},
		{"visible",     visible},
		{"description", description},
		{"value",       value},
	    };
	    data["method"] = "update";
	}
    }

    public class HTMLWidget : Widget {
	// Attributes
	public string description {
	    get { return Convert.ToString(get("description")); }
	    set { set("description", Convert.ToString(value)); }
	}
	public string value {
	    get { return Convert.ToString(get("value")); }
	    set { set("value", Convert.ToString(value)); }
	}

	public HTMLWidget(ZMQServer.Session session,
			  string value="",
			  string description="", 
			  bool disabled=false, 
			  bool visible=true) : base(session) {
	    data["state"] = new Dictionary<string,object> {
		{"_view_name",  "HTMLView"},
		{"_css",        get_css()},
		{"disabled",    disabled},
		{"visible",     visible},
		{"value",       value},
		{"description", description},
	    };
	    data["method"] = "update";
	}
    }

    public class ImageWidget : Widget {
	// Attributes
	public string format {
	    get { return Convert.ToString(get("format")); }
	    set { set("format", Convert.ToString(value)); }
	}
	// FIXME: CastingUnicode: int
	public string width {
	    get { return Convert.ToString(get("width")); }
	    set { set("width", Convert.ToString(value)); }
	}
	// FIXME: CastingUnicode: int
	public string height {
	    get { return Convert.ToString(get("height")); }
	    set { set("height", Convert.ToString(value)); }
	}
	public string _b64value {
	    get { return Convert.ToString(get("_b64value")); }
	    set { set("_b64value", Convert.ToString(value)); }
	}
	public byte[] value {
	    get { return (byte [])get("value"); }
	    set { set("value", value); }
	}

	public ImageWidget(ZMQServer.Session session,
			    byte [] value=null, 
			    string format="png", 
			    string width="", 
			    string height="", 
			    string _b64value="", 
			    bool disabled=false, 
			    bool visible=true) : base(session) {
	    data["state"] = new Dictionary<string,object> {
		{"_view_name",  "ImageView"},
		{"_css",        get_css()},
		{"disabled",    disabled},
		{"visible",     visible},
		{"format",      format},
		{"width",       width},
		{"height",      height},
		{"value",       value},
		{"_b64value",   _b64value},
	    };
	    data["method"] = "update";
	}
    }

    public class IntProgressWidget : Widget {
	// Attributes
	public string description {
	    get { return Convert.ToString(get("description")); }
	    set { set("description", Convert.ToString(value)); }
	}
	public Int64 min {
	    get { return Convert.ToInt64(get("min")); }
	    set { set("min", Convert.ToInt64(value)); }
	}
	public double max {
	    get { return Convert.ToInt64(get("max")); }
	    set { set("max", Convert.ToInt64(value)); }
	}
	public double step {
	    get { return Convert.ToInt64(get("step")); }
	    set { set("step", Convert.ToInt64(value)); }
	}
	public double value {
	    get { return Convert.ToInt64(get("value")); }
	    set { set("value", Convert.ToInt64(value)); }
	}

	public IntProgressWidget(ZMQServer.Session session,
				 Int64 min=0, 
				 Int64 max=100, 
				 Int64 step=1, 
				 Int64 value=0, 
				 string description="", 
				 bool disabled=false, 
				 bool visible=true) : base(session) {
	    data["state"] = new Dictionary<string,object> {
		{"_view_name",  "IntProgressView"},
		{"_css",        get_css()},
		{"disabled",    disabled},
		{"visible",     visible},
		{"description", description},
		{"min",         min},
		{"max",         max},
		{"step",        step},
		{"value",       value},		
	    };
	    data["method"] = "update";
	}
    }

    public class IntSliderWidget : Widget {
	// Attributes
	public string description {
	    get { return Convert.ToString(get("description")); }
	    set { set("description", Convert.ToString(value)); }
	}
	public Int64 min {
	    get { return Convert.ToInt64(get("min")); }
	    set { set("min", Convert.ToInt64(value)); }
	}
	public Int64 max {
	    get { return Convert.ToInt64(get("max")); }
	    set { set("max", Convert.ToInt64(value)); }
	}
	public Int64 step {
	    get { return Convert.ToInt64(get("step")); }
	    set { set("step", Convert.ToInt64(value)); }
	}
	public Int64 value {
	    get { return Convert.ToInt64(get("value")); }
	    set { set("value", Convert.ToInt64(value)); }
	}
	public string orientation {
	    get { return Convert.ToString(get("orientation")); }
	    set { set("orientation", Convert.ToString(value)); }
	}
	public bool readout {
	    get { return Convert.ToBoolean(get("readout")); }
	    set { set("readout", Convert.ToBoolean(value)); }
	}

	public IntSliderWidget(ZMQServer.Session session,
			       Int64 min=0, 
			       Int64 max=100, 
			       Int64 step=1, 
			       Int64 value=0, 
			       string description="", 
			       bool disabled=false, 
			       string orientation="horizontal", 
			       bool readout=true, 
			       bool visible=true) : base(session) {
	    data["state"] = new Dictionary<string,object> {
		{"_view_name",  "IntSliderView"},
		{"_css",        get_css()},
		{"orientation", orientation},
		{"readout",     readout},
		{"disabled",    disabled},
		{"visible",     visible},
		{"description", description},
		{"min",         min},
		{"max",         max},
		{"step",        step},
		{"value",       value},
	    };
	    data["method"] = "update";
	}
    }

    public class IntTextWidget : Widget {
	// Attributes
	public Int64 value {
	    get { return Convert.ToInt64(get("value")); }
	    set { set("value", Convert.ToInt64(value)); }
	}
	public string description {
	    get { return Convert.ToString(get("description")); }
	    set { set("description", Convert.ToString(value)); }
	}

	public IntTextWidget(ZMQServer.Session session,
			     Int64 value=0,
			     string description="",
			     bool disabled=false, 
			     bool visible=true) : base(session) {
	    data["state"] = new Dictionary<string,object> {
		{"_view_name",  "IntTextView"},
		{"_css",        get_css()},
		{"disabled",    disabled},
		{"visible",     visible},
		{"value",       value},
		{"description", description},
	    };
	    data["method"] = "update";
	}
    }

    public class LatexWidget : Widget {
	// Attributes
	public string description {
	    get { return Convert.ToString(get("description")); }
	    set { set("description", Convert.ToString(value)); }
	}
	public string value {
	    get { return Convert.ToString(get("value")); }
	    set { set("value", Convert.ToString(value)); }
	}

	public LatexWidget(ZMQServer.Session session,
			   string value="",
			   string description="", 
			   bool disabled=false, 
			   bool visible=true) : base(session) {
	    data["state"] = new Dictionary<string,object> {
		{"_view_name",  "LatexView"},
		{"_css",        get_css()},
		{"disabled",    disabled},
		{"visible",     visible},
		{"value",       value},
		{"description", description},
	    };
	    data["method"] = "update";
	}
    }

    public class RadioButtonsWidget : SelectionWidget {
	public RadioButtonsWidget(ZMQServer.Session session,
			    string description="", 
			    bool disabled=false, 
			    bool visible=true) : 
	         base(session, description, disabled, visible) {
	    ((IDictionary<string,object>)data["state"])["_view_name"] = "ToggleButtonsView";
	}
    }

    public class SelectWidget : SelectionWidget {
	public SelectWidget(ZMQServer.Session session,
			    string description="", 
			    bool disabled=false, 
			    bool visible=true) : 
	    base(session, description, disabled, visible) {
	    ((IDictionary<string,object>)data["state"])["_view_name"] = "SelectView";
	}
    }

    public class TextWidget : Widget {
	// Attributes
	public string description {
	    get { return Convert.ToString(get("description")); }
	    set { set("description", Convert.ToString(value)); }
	}
	public string value {
	    get { return Convert.ToString(get("value")); }
	    set { set("value", Convert.ToString(value)); }
	}

	public TextWidget(ZMQServer.Session session,
			  string value="",
			  string description="", 
			  bool disabled=false, 
			  bool visible=true) : base(session) {
	    data["state"] = new Dictionary<string,object> {
		{"_view_name",  "TextView"},
		{"_css",        get_css()},
		{"disabled",    disabled},
		{"visible",     visible},
		{"description", description},
		{"value",       value},
	    };
	    data["method"] = "update";
	}
    }

    public class TextareaWidget : Widget {
	// Attributes
	public string description {
	    get { return Convert.ToString(get("description")); }
	    set { set("description", Convert.ToString(value)); }
	}
	public string value {
	    get { return Convert.ToString(get("value")); }
	    set { set("value", Convert.ToString(value)); }
	}

	public TextareaWidget(ZMQServer.Session session,
			      string value="",
			      string description="", 
			      bool disabled=false, 
			      bool visible=true) : base(session) {
	    data["state"] = new Dictionary<string,object> {
		{"_view_name",  "TextareaView"},
		{"_css",        get_css()},
		{"disabled",    disabled},
		{"visible",     visible},
		{"value",       value},
		{"description", description},
	    };
	    data["method"] = "update";
	}
    }

    public class ToggleButtonWidget : Widget {
	// Attributes
	public string description {
	    get { return Convert.ToString(get("description")); }
	    set { set("description", Convert.ToString(value)); }
	}
	public bool value {
	    get { return Convert.ToBoolean(get("value")); }
	    set { set("value", Convert.ToBoolean(value)); }
	}

	public ToggleButtonWidget(ZMQServer.Session session,
				  bool value=false,
				  string description="", 
				  bool disabled=false, 
				  bool visible=true) : base(session) {
	    data["state"] = new Dictionary<string,object> {
		{"_view_name",  "ToggleButtonView"},
		{"_css",        get_css()},
		{"disabled",    disabled},
		{"visible",     visible},
		{"description", description},
		{"value",       value},
	    };
	    data["method"] = "update";
	}
    }

    public class ToggleButtonsWidget : SelectionWidget {
	public ToggleButtonsWidget(ZMQServer.Session session,
				   string description="", 
				   bool disabled=false, 
				   bool visible=true) : 
   	         base(session, description, disabled, visible) {
	    ((IDictionary<string,object>)data["state"])["_view_name"] = "ToggleButtonsView";
	}
    }

    public class ContainerWidget : Widget {
	// Attributes
	public IList children {
	    get { return (IList)get("children"); }
	    set { set("children", value); }
	}

	public ContainerWidget(ZMQServer.Session session,
				   IList children=null, 
				   bool disabled=false, 
				   bool visible=true) : base(session) {
	    if (children == null) {
		children = new List<Widget>();
	    }
	    data["state"] = new Dictionary<string,object> {
		{"_view_name",  "ContainerView"},
		{"_css",        get_css()},
		{"disabled",    disabled},
		{"visible",     visible},
		{"children",    children},
	    };
	    data["method"] = "update";
	}
    }

    public class PopupWidget : Widget {
	// Attributes
	public string description {
	    get { return Convert.ToString(get("description")); }
	    set { set("description", Convert.ToString(value)); }
	}
	public string button_text {
	    get { return Convert.ToString(get("button_text")); }
	    set { set("button_text", Convert.ToString(value)); }
	}

	public PopupWidget(ZMQServer.Session session,
			    string button_text="", 
			    string description="", 
			    bool disabled=false, 
			    bool visible=true) : base(session) {
	    data["state"] = new Dictionary<string,object> {
		{"_view_name",  "PopupView"},
		{"_css",        get_css()},
		{"disabled",    disabled},
		{"visible",     visible},
		{"description", description},
		{"button_text", button_text},
	    };
	    data["method"] = "update";
	}
    }

    public class SelectionWidget : Widget {
	// Attributes
	// FIXME: add stuff to this widget base class
	// value
	// values
	public string description {
	    get { return Convert.ToString(get("description")); }
	    set { set("description", Convert.ToString(value)); }
	}

	public SelectionWidget(ZMQServer.Session session,
			    string description="", 
			    bool disabled=false, 
			    bool visible=true) : base(session) {
	    data["state"] = new Dictionary<string,object> {
		{"_view_name",  "SelectionView"}, // Will be overridden
		{"_css",        get_css()},
		{"disabled",    disabled},
		{"visible",     visible},
		{"description", description},
	    };
	    data["method"] = "update";
	}
    }
    // FIXME: SelectionContainerWidget

    public class PasswordWidget : Widgets.TextWidget {
	public PasswordWidget(ZMQServer.Session session,
			      string value="",
			      string description="", 
			      bool disabled=false, 
			      bool visible=true) : 
	    base(session, value, description, disabled, visible) {

	    ((IDictionary<string,object>)data["state"])["_view_name"] = "PasswordView";
	    session.calico.display(
		   session.calico.Javascript("require([\"notebook/js/widgets/widget\"], function(WidgetManager){" +
					     "  var PasswordView = WidgetManager._view_types['TextView'].extend({  " +
					     "         update: function(options){" +
					     "            this.$textbox.attr('type', 'password');" +
					     "            return PasswordView.__super__.update.apply(this);" +
					     "         }," +
					     "   });" +
					     "   WidgetManager.register_widget_view('PasswordView', PasswordView);" +
					     "});"));
	}
    }
    
    public class CameraWidget : Widgets.Widget {
	public string imgurl {
	    get { return Convert.ToString(get("imgurl")); }
	    set { set("imgurl", Convert.ToString(value)); }
	}

	public CameraWidget(ZMQServer.Session session) : base(session) {
	    ((IDictionary<string,object>)data["state"])["_view_name"] = "CameraView";
	    session.calico.display(
		   session.calico.Javascript(
"require([\"notebook/js/widgets/widget\"], function(WidgetManager){ \n" +
"    var CameraView = IPython.DOMWidgetView.extend({ \n" +
"        render: function(){ \n" +
"            // based on https://developer.mozilla.org/en-US/docs/WebRTC/taking_webcam_photos \n" +
"            var video        = $('<video>')[0]; \n" +
"            var canvas       = $('<canvas>')[0]; \n" +
"            var startbutton  = $('<button>Take Pic</button>')[0]; \n" +
"            var width = 320; \n" +
"            var height = 0; \n" +
"            var that = this; \n" +
" \n" +
"            this.$el.append(video).append(startbutton).append(canvas); \n" +
"            var streaming = false; \n" +
"            navigator.getMedia = ( navigator.getUserMedia || \n" +
"                                 navigator.webkitGetUserMedia || \n" +
"                                 navigator.mozGetUserMedia || \n" +
"                                 navigator.msGetUserMedia); \n" +
" \n" +
"            navigator.getMedia({video: true, audio: false}, \n" +
"                function(stream) { \n" +
"                  if (navigator.mozGetUserMedia) { \n" +
"                    video.mozSrcObject = stream; \n" +
"                  } else { \n" +
"                    var vendorURL = window.URL || window.webkitURL; \n" +
"                    video.src = vendorURL.createObjectURL(stream); \n" +
"                  } \n" +
"                  video.play(); \n" +
"                }, \n" +
"                function(err) { \n" +
"                  console.log(\"An error occured! \" + err); \n" +
"                } \n" +
"            ); \n" +
" \n" +
"            video.addEventListener('canplay', function(ev){ \n" +
"                if (!streaming) { \n" +
"                  height = video.videoHeight / (video.videoWidth/width); \n" +
"                  video.setAttribute('width', width); \n" +
"                  video.setAttribute('height', height); \n" +
"                  canvas.setAttribute('width', width); \n" +
"                  canvas.setAttribute('height', height); \n" +
"                  streaming = true; \n" +
"                } \n" +
"            }, false); \n" +
"            function takepicture() { \n" +
"                canvas.width = width; \n" +
"                canvas.height = height; \n" +
"                canvas.getContext('2d').drawImage(video, 0, 0, width, height); \n" +
"                that.model.set('imageurl',canvas.toDataURL('image/png')); \n" +
"                that.model.save_changes(that); \n" +
"            } \n" +
"            startbutton.addEventListener('click', function(ev){ \n" +
"                takepicture(); \n" +
"                ev.preventDefault(); \n" +
"            }, false); \n" +
"        }, \n" +
"    }); \n" +
"    \n" +
"    // Register the DatePickerView with the widget manager. \n" +
"    WidgetManager.register_widget_view('CameraView', CameraView); \n" +
" \n" +
"});"));
	}
    }
}
