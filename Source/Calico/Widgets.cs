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
	System.Console.WriteLine(id);
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

    public class Widget {
	public string target_name;
	public Dictionary<string,object> data;
	public string comm_id;
	public System.Func<object,object> on_click_func;
	public Dictionary<string,System.Func<string,string,object>> on_value_change_func = new Dictionary<string,System.Func<string,string,object>>();
	ZMQServer.Session session = null;

	public Widget(ZMQServer.Session session) {
	    this.session = session;
	    target_name = "WidgetModel";
	    data = new Dictionary<string, object>();
	    comm_id = System.Guid.NewGuid().ToString();
	    // Register this widget:
	    Widgets.register(this);
	}

	// Attributes
	public bool visible {
	    get { return (bool)get("visible"); }
	    set { set("visible", value); }
	}
	public bool disabled {
	    get { return (bool)get("disabled"); }
	    set { set("disabled", value); }
	}
	public string _view_name {
	    get { return (string)get("_view_name"); }
	    set { set("_view_name", value); }
	}

	public void Dispatch(IDictionary<string, object> data,
			     IDictionary<string, object> parent_header) {
	    // handle comm_msg for widget
	    if (data.ContainsKey("sync_data")) {
		// data: {"method":"backbone","sync_data":{"value":0.8}}
		session.update_status("busy", parent_header);
		((Dictionary<string,object>)this.data["state"])["value"] = ((Dictionary<string,object>)data["sync_data"])["value"];
		session.update_status("idle", parent_header);
	    }
	    if (data.ContainsKey("content")) {
		// data: {"content":{"event":"click"},"method":"custom"}
		string evt = ((Dictionary<string,object>)data["content"])["event"].ToString();
		if (evt == "click") {
		    session.update_status("busy", parent_header);
		    onClick();
		    session.update_status("idle", parent_header);
		}
	    }
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
	    get { return (string)get("description"); }
	    set { set("description", value); }
	}
	public double min {
	    get { return (double)get("min"); }
	    set { set("min", value); }
	}
	public double max {
	    get { return (double)get("max"); }
	    set { set("max", value); }
	}
	public double step {
	    get { return (double)get("step"); }
	    set { set("step", value); }
	}
	public double value {
	    get { return (double)get("value"); }
	    set { set("value", value); }
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
	    get { return (string)get("description"); }
	    set { set("description", value); }
	}
	// FIXME: Should these be ints or strings?
	public Int64 min {
	    get { return (Int64)get("min"); }
	    set { set("min", value); }
	}
	public Int64 max {
	    get { return (Int64)get("max"); }
	    set { set("max", value); }
	}
	public Int64 step {
	    get { return (Int64)get("step"); }
	    set { set("step", value); }
	}
	public Int64 value {
	    get { return (Int64)get("value"); }
	    set { set("value", value); }
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
	    get { return (string)get("description"); }
	    set { set("description", value); }
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
	    get { return (string)get("description"); }
	    set { set("description", value); }
	}
	public bool value {
	    get { return (bool)get("value"); }
	    set { set("value", value); }
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
	    get { return (string)get("description"); }
	    set { set("description", value); }
	}
	public double min {
	    get { return (double)get("min"); }
	    set { set("min", value); }
	}
	public double max {
	    get { return (double)get("max"); }
	    set { set("max", value); }
	}
	public double step {
	    get { return (double)get("step"); }
	    set { set("step", value); }
	}
	public double value {
	    get { return (double)get("value"); }
	    set { set("value", value); }
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
	    get { return (double)get("min"); }
	    set { set("min", value); }
	}
	public double max {
	    get { return (double)get("max"); }
	    set { set("max", value); }
	}
	public double step {
	    get { return (double)get("step"); }
	    set { set("step", value); }
	}
	public double value {
	    get { return (double)get("value"); }
	    set { set("value", value); }
	}
	public string description {
	    get { return (string)get("description"); }
	    set { set("description", value); }
	}
	public string orientation {
	    get { return (string)get("orientation"); }
	    set { set("orientation", value); }
	}
	public bool readout {
	    get { return (bool)get("readout"); }
	    set { set("readout", value); }
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
	    get { return (string)get("description"); }
	    set { set("description", value); }
	}
	public double min {
	    get { return (double)get("min"); }
	    set { set("min", value); }
	}
	public double max {
	    get { return (double)get("max"); }
	    set { set("max", value); }
	}
	public double step {
	    get { return (double)get("step"); }
	    set { set("step", value); }
	}
	public double value {
	    get { return (double)get("value"); }
	    set { set("value", value); }
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
	    get { return (string)get("description"); }
	    set { set("description", value); }
	}
	public double value {
	    get { return (double)get("value"); }
	    set { set("value", value); }
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
	    get { return (string)get("description"); }
	    set { set("description", value); }
	}
	public string value {
	    get { return (string)get("value"); }
	    set { set("value", value); }
	}

	public HTMLWidget(ZMQServer.Session session,
			  string value,
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
	    get { return (string)get("format"); }
	    set { set("format", value); }
	}
	// FIXME: CastingUnicode: int
	public string width {
	    get { return (string)get("width"); }
	    set { set("width", value); }
	}
	// FIXME: CastingUnicode: int
	public string height {
	    get { return (string)get("height"); }
	    set { set("height", value); }
	}
	public string _b64value {
	    get { return (string)get("_b64value"); }
	    set { set("_b64value", value); }
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
	    get { return (string)get("description"); }
	    set { set("description", value); }
	}
	public Int64 min {
	    get { return (Int64)get("min"); }
	    set { set("min", value); }
	}
	public double max {
	    get { return (Int64)get("max"); }
	    set { set("max", value); }
	}
	public double step {
	    get { return (Int64)get("step"); }
	    set { set("step", value); }
	}
	public double value {
	    get { return (Int64)get("value"); }
	    set { set("value", value); }
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
	    get { return (string)get("description"); }
	    set { set("description", value); }
	}
	public Int64 min {
	    get { return (Int64)get("min"); }
	    set { set("min", value); }
	}
	public Int64 max {
	    get { return (Int64)get("max"); }
	    set { set("max", value); }
	}
	public Int64 step {
	    get { return (Int64)get("step"); }
	    set { set("step", value); }
	}
	public Int64 value {
	    get { return (Int64)get("value"); }
	    set { set("value", value); }
	}
	public string orientation {
	    get { return (string)get("orientation"); }
	    set { set("orientation", value); }
	}
	public bool readout {
	    get { return (bool)get("readout"); }
	    set { set("readout", value); }
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
	    get { return (Int64)get("value"); }
	    set { set("value", value); }
	}
	public string description {
	    get { return (string)get("description"); }
	    set { set("description", value); }
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
	    get { return (string)get("description"); }
	    set { set("description", value); }
	}
	public string value {
	    get { return (string)get("value"); }
	    set { set("value", value); }
	}

	public LatexWidget(ZMQServer.Session session,
			   string value,
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
	    get { return (string)get("description"); }
	    set { set("description", value); }
	}
	public string value {
	    get { return (string)get("value"); }
	    set { set("value", value); }
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
	    get { return (string)get("description"); }
	    set { set("description", value); }
	}
	public string value {
	    get { return (string)get("value"); }
	    set { set("value", value); }
	}

	public TextareaWidget(ZMQServer.Session session,
			      string value,
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
	    get { return (string)get("description"); }
	    set { set("description", value); }
	}
	public bool value {
	    get { return (bool)get("value"); }
	    set { set("value", value); }
	}

	public ToggleButtonWidget(ZMQServer.Session session,
				  bool value,
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

    public class ToogleButtonsWidget : SelectionWidget {
	public ToogleButtonsWidget(ZMQServer.Session session,
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
	    get { return (string)get("description"); }
	    set { set("description", value); }
	}
	public string button_text {
	    get { return (string)get("button_text"); }
	    set { set("button_text", value); }
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
	    get { return (string)get("description"); }
	    set { set("description", value); }
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
}
