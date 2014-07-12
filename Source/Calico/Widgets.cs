// IPython kernel backend in C#
// Doug Blank

using System; // Int64
using System.Collections; // IDictionary
using System.Collections.Generic; // Dictionary
using System.Threading;

public static class Widgets {

    public static int next_id = 1;

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
	public System.Func<object,object> on_submit_func;
	public Dictionary<string,Callback> on_value_change_callback = new Dictionary<string,Callback>();
	internal ZMQServer.Session session = null;
	public int execution_count;

	public Widget(ZMQServer.Session session) {
	    this.session = session;
	    target_name = "WidgetModel";
	    data = new Dictionary<string, object>();
	    data["state"] = new Dictionary<string,object>();
	    set("_view_name",  "WidgetView");
	    set("_css", get_css());
	    set("disabled", false);
	    set("visible", true);
	    set("description", "A generic Widget");
	    // and the method of update
	    data["method"] = "update";
	    comm_id = System.Guid.NewGuid().ToString();
	    Widgets.register(this);
	}

	// Attributes
	public string description {
	    get { return Convert.ToString(get("description")); }
	    set { set("description", Convert.ToString(value)); }
	}
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
		    set(value_name, value);
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
		} else if (evt == "submit") {
		    onSubmit();
		} else {
		    System.Console.Error.WriteLine("Widget.Dispatch() needs to handle event: " + evt);
		}
	    }
	    // And back to idle:
	    session.update_status("idle", parent_header);
	    //session.SetOutputs(0, null); // wait till after widget displays
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

	public void on_submit(System.Func<object,object> function) {
	    on_submit_func = function;
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

	public void onSubmit() {
	    if (on_submit_func != null) {
		try {
		    on_submit_func(this);
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

	public void display() {
	    session.display(this);
	}

	public void animate() {
	    session.clear_output();
	    session.display(this);
	}
    }

    public class BoundedFloatTextWidget : Widget {
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

	public BoundedFloatTextWidget(ZMQServer.Session session,
				      double min=0.0, 
				      double max=100.0, 
				      double step=0.1, 
				      double value=0.0, 
				      string description="", 
				      bool disabled=false, 
				      bool visible=true) : base(session) {
	    set("_view_name", "FloatTextView");
	    set("disabled", disabled);
	    set("visible", visible);
	    set("description", description);
	    set("min", min);
	    set("max", max);
	    set("step", step);
	    set("value", value);
	}
    }

    public class BoundedIntTextWidget : Widget {
	// Attributes
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
	    set("_view_name",  "IntTextView");
	    set("disabled",    disabled);
	    set("visible",     visible);
	    set("description", description);
	    set("min",         min);
	    set("max",         max);
	    set("step",        step);
	    set("value",       value);
	}
    }

    public class ButtonWidget : Widget {
	// Attributes
	public ButtonWidget(ZMQServer.Session session,
			    string description="", 
			    bool disabled=false, 
			    bool visible=true) : base(session) {
	    set("_view_name",  "ButtonView");
	    set("disabled",    disabled);
	    set("visible",     visible);
	    set("description", description);
	}
    }

    public class CheckboxWidget : Widget {
	// Attributes
	public bool value {
	    get { return Convert.ToBoolean(get("value")); }
	    set { set("value", Convert.ToBoolean(value)); }
	}

	public CheckboxWidget(ZMQServer.Session session,
			      string description="", 
			      bool value=false,
			      bool disabled=false, 
			      bool visible=true) : base(session) {
	    set("_view_name",  "CheckboxView");
	    set("disabled",    disabled);
	    set("visible",     visible);
	    set("description", description);
	    set("value",       value);
	}
    }

    public class DropdownWidget : _SelectionWidget {
	public DropdownWidget(ZMQServer.Session session,
			      string description="", 
			      IList value_names=null,
			      IDictionary values=null,
			      string value_name=null,
			      bool disabled=false, 
			      bool visible=true) : 
	    base(session, description, value_names, values, value_name, disabled, visible) {
	    set("_view_name", "DropdownView");
	}
    }

    public class FloatProgressWidget : Widget {
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

	public FloatProgressWidget(ZMQServer.Session session,
				   double min=0.0, 
				   double max=100.0, 
				   double step=0.1, 
				   double value=0.0, 
				   string description="", 
				   bool disabled=false, 
				   bool visible=true) : base(session) {
	    set("_view_name",  "ProgressView");
	    set("disabled",    disabled);
	    set("visible",     visible);
	    set("description", description);
	    set("min",         min);
	    set("max",         max);
	    set("step",        step);
	    set("value",       value);
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
	    set("_view_name",  "FloatSliderView");
	    set("min",         min);
	    set("max",         max);
	    set("step",        step);
	    set("value",       value);
	    set("orientation", orientation);
	    set("readout",     readout);
	    set("disabled",    disabled);
	    set("visible",     visible);
	    set("description", description);
	}
    }

    public class FloatTextWidget : Widget {
	// Attributes
	public double value {
	    get { return Convert.ToDouble(get("value")); }
	    set { set("value", Convert.ToDouble(value)); }
	}

	public FloatTextWidget(ZMQServer.Session session,
			       double value=0.0,
			       string description="", 
			       bool disabled=false, 
			       bool visible=true) : base(session) {
	    set("_view_name",  "FloatTextView");
	    set("disabled",    disabled);
	    set("visible",     visible);
	    set("description", description);
	    set("value",       value);
	}
    }

    public class HTMLWidget : Widget {
	// Attributes
	public string value {
	    get { return Convert.ToString(get("value")); }
	    set { set("value", Convert.ToString(value)); }
	}

	public HTMLWidget(ZMQServer.Session session,
			  string value="",
			  string description="", 
			  bool disabled=false, 
			  bool visible=true) : base(session) {
	    set("_view_name",  "HTMLView");
	    set("disabled",    disabled);
	    set("visible",     visible);
	    set("value",       value);
	    set("description", description);
	}
    }

    public class ImageWidget : Widget {
	// Attributes
	public string format {
	    get { return Convert.ToString(get("format")); }
	    set { set("format", Convert.ToString(value)); }
	}
	public string width {
	    get { return Convert.ToString(get("width")); }
	    set { set("width", Convert.ToString(value)); }
	}
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
	    set("_view_name",  "ImageView");
	    set("disabled",    disabled);
	    set("visible",     visible);
	    set("format",      format);
	    set("width",       width);
	    set("height",      height);
	    set("value",       value);
	    set("_b64value",   _b64value);
	}
    }

    public class IntProgressWidget : Widget {
	// Attributes
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
	    set("_view_name",  "ProgressView");
	    set("disabled",    disabled);
	    set("visible",     visible);
	    set("description", description);
	    set("min",         min);
	    set("max",         max);
	    set("step",        step);
	    set("value",       value);
	}
    }

    public class IntSliderWidget : Widget {
	// Attributes
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
	    set("_view_name",  "IntSliderView");
	    set("orientation", orientation);
	    set("readout",     readout);
	    set("disabled",    disabled);
	    set("visible",     visible);
	    set("description", description);
	    set("min",         min);
	    set("max",         max);
	    set("step",        step);
	    set("value",       value);
	}
    }

    public class IntTextWidget : Widget {
	// Attributes
	public Int64 value {
	    get { return Convert.ToInt64(get("value")); }
	    set { set("value", Convert.ToInt64(value)); }
	}

	public IntTextWidget(ZMQServer.Session session,
			     Int64 value=0,
			     string description="",
			     bool disabled=false, 
			     bool visible=true) : base(session) {
	    set("_view_name",  "IntTextView");
	    set("disabled",    disabled);
	    set("visible",     visible);
	    set("value",       value);
	    set("description", description);
	}
    }

    public class LatexWidget : Widget {
	// Attributes
	public string value {
	    get { return Convert.ToString(get("value")); }
	    set { set("value", Convert.ToString(value)); }
	}

	public LatexWidget(ZMQServer.Session session,
			   string value="",
			   string description="", 
			   bool disabled=false, 
			   bool visible=true) : base(session) {
	    set("_view_name",  "LatexView");
	    set("disabled",    disabled);
	    set("visible",     visible);
	    set("value",       value);
	    set("description", description);
	}
    }

    public class RadioButtonsWidget : _SelectionWidget {
	public RadioButtonsWidget(ZMQServer.Session session,
				  string description="", 
				  IList value_names=null,
				  IDictionary values=null,
				  string value_name=null,
				  bool disabled=false, 
				  bool visible=true) : 
	    base(session, description, value_names, values, value_name, disabled, visible) {
	    set("_view_name", "RadioButtonsView");
	}
    }

    public class SelectWidget : _SelectionWidget {
	public SelectWidget(ZMQServer.Session session,
			    string description="", 
			    IList value_names=null,
			    IDictionary values=null,
			    string value_name=null,
			    bool disabled=false, 
			    bool visible=true) : 
	    base(session, description, value_names, values, value_name, disabled, visible) {
	    set("_view_name", "SelectView");
	}
    }

    public class TextWidget : Widget {
	// Attributes
	public string value {
	    get { return Convert.ToString(get("value")); }
	    set { set("value", Convert.ToString(value)); }
	}

	public TextWidget(ZMQServer.Session session,
			  string value="",
			  string description="", 
			  bool disabled=false, 
			  bool visible=true) : base(session) {
	    set("_view_name",  "TextView");
	    set("disabled",    disabled);
	    set("visible",     visible);
	    set("description", description);
	    set("value",       value);
	}
    }

    public class TextareaWidget : Widget {
	// Attributes
	public string value {
	    get { return Convert.ToString(get("value")); }
	    set { set("value", Convert.ToString(value)); }
	}

	public TextareaWidget(ZMQServer.Session session,
			      string value="",
			      string description="", 
			      bool disabled=false, 
			      bool visible=true) : base(session) {
	    set("_view_name",  "TextareaView");
	    set("disabled",    disabled);
	    set("visible",     visible);
	    set("value",       value);
	    set("description", description);
	}
    }

    public class ToggleButtonWidget : Widget {
	// Attributes
	public bool value {
	    get { return Convert.ToBoolean(get("value")); }
	    set { set("value", Convert.ToBoolean(value)); }
	}

	public ToggleButtonWidget(ZMQServer.Session session,
				  bool value=false,
				  string description="", 
				  bool disabled=false, 
				  bool visible=true) : base(session) {
	    set("_view_name",  "ToggleButtonView");
	    set("disabled",    disabled);
	    set("visible",     visible);
	    set("description", description);
	    set("value",       value);
	}
    }

    public class ToggleButtonsWidget : _SelectionWidget {
	public ToggleButtonsWidget(ZMQServer.Session session,
				   string description="", 
				   IList value_names=null,
				   IDictionary values=null,
				   string value_name=null,
				   bool disabled=false, 
				   bool visible=true) : 
	    base(session, description, value_names, values, value_name, disabled, visible) {
	    set("_view_name", "ToggleButtonsView");
	}
    }

    public class ContainerWidget : Widget {
	public List<Widget> children = new List<Widget>(); 
	public Dictionary<string,Widget> widgets = new Dictionary<string,Widget>();

	public ContainerWidget(ZMQServer.Session session,
				   IList<Widget> children=null, 
				   bool disabled=false, 
				   bool visible=true) : base(session) {
	    set("_view_name",  "ContainerView");
	    set("disabled",    disabled);
	    set("visible",     visible);
	    set("_children", new List<string>());
	    widgets[comm_id] = this; // add yourself to list of widgets
	    if (children != null) {
		foreach (Widget widget in children) {
		    AddChild(widget);
		}
	    }
	}

	public void AddChild(Widget widget) {
	    List<string> _children = (List<string>)get("_children");
	    _children.Add(widget.comm_id);
	    children.Add(widget);
	    widgets[widget.comm_id] = widget;
	    // notify client:
	    session.added_child_widget(widget);
	}
    }

    /*
    public class MultipleChoiceCheckWidget : ContainerWidget {

	  answers = ["a) apple", "b) banana", "c) cherry"]
	  question = calico.LatexWidget("Q1: What is the $\dfrac{best}{x}$ fruit?")
	  choices = calico.RadioButtonsWidget(value_names=answers)
	  button = calico.ButtonWidget("Check Me")
	  container = calico.ContainerWidget(children=[question, choices, button])
	  
	  def on_click(button):
	  if answers.index(choices.value_name) == 1:
	  calico.animate("Correct!! The best fruit is the banana")
	  else:
	  calico.animate("Incorrect. Try running the above code to see the best fruit.")
	  
	  button.on_click(on_click)
	  container

	public MultipleChoiceWidget() {
	    on_click_func = function;
	}
    }
    */

    public class PopupWidget : ContainerWidget {
	// Attributes
	public string button_text {
	    get { return Convert.ToString(get("button_text")); }
	    set { set("button_text", Convert.ToString(value)); }
	}

	public PopupWidget(ZMQServer.Session session,
			   string button_text="", 
			   string description="", 
			   IList<Widget> children=null,
			   bool disabled=false, 
			   bool visible=true) : base(session, children, disabled, visible) {
	    set("_view_name",  "PopupView");
	    set("description", description);
	    set("button_text", button_text);
	}
    }

    public class AccordionWidget : _SelectionContainerWidget {

	public AccordionWidget(ZMQServer.Session session,
			       IList<Widget> children=null,
			       bool disabled=false, 
			       bool visible=true) : base(session, children, disabled, visible) {
	    set("_view_name",  "AccordionView");
	}
    }

    public class TabWidget : _SelectionContainerWidget {

	public TabWidget(ZMQServer.Session session,
			 IList<Widget> children=null,
			 bool disabled=false, 
			 bool visible=true) : base(session, children, disabled, visible) {
	    set("_view_name",  "TabView");
	}
    }

    public class _SelectionWidget : Widget {
	// Attributes
	public IList value_names {
	    get { return (IList)get("value_names"); }
	    set { set("value_names", (IList)value); }
	}
	public IDictionary values {
	    get { return (IDictionary)get("values"); }
	    set { set("values", (IDictionary)value); }
	}
	public string value_name {
	    get { return (string)get("value_name"); }
	    set { set("value_name", (string)value_name); }
	}

	public _SelectionWidget(ZMQServer.Session session,
				string description="", 
				IList value_names=null,
				IDictionary values=null,
				string value_name=null,
				bool disabled=false, 
				bool visible=true) : base(session) {
	    set("_view_name",  "_SelectionView"); // Will be overridden
	    set("description", description);
	    set("value_names", value_names == null ? new List<object>() : value_names);
	    set("values", values == null ? makeValuesFromNames() : values);
	    set("value_name", value_name == null ? getDefaultValueName() : value_name);
	    set("disabled",    disabled);
	    set("visible",     visible);
	    set("value", null);
	}

	public string getDefaultValueName() {
	    if (value_names.Count == 0)
		return "";
	    else
		return value_names[0].ToString();
	}

	public IDictionary makeValuesFromNames() {
	    var retval = new Dictionary<string,string>();
	    foreach (string value in value_names) {
		retval[value] = value;
	    }
	    return retval;
	}

    }

    public class _SelectionContainerWidget : ContainerWidget {

	public _SelectionContainerWidget(ZMQServer.Session session,
			 IList<Widget> children=null,
			 bool disabled=false, 
					 bool visible=true) : base(session, children, disabled, visible) {
	    set("_view_name",  "_SelectionContainerView"); // Will be overridden
	    set("_titles",  new Dictionary<int,string>());
	    set("selected_index",  0);
	}
    }

    public class PasswordWidget : Widgets.TextWidget {
	// based on work by Jonathan Frederic
	// http://nbviewer.ipython.org/gist/jdfreder/8795537
	public PasswordWidget(ZMQServer.Session session,
			      string value="",
			      string description="", 
			      bool disabled=false, 
			      bool visible=true) : 
	    base(session, value, description, disabled, visible) {
	    set("_view_name", "PasswordView");
	    // need to inject the javascript now:
	    session.display(session.calico.Javascript(javascript()));
	    Thread.Sleep(100); // miliseconds
	}

	public string javascript() {
	    return "require([\"widgets/js/widget\"], function(WidgetManager){ \n" +
		"  var PasswordView = WidgetManager._view_types['TextView'].extend({  \n" +
		"         update: function(options){ \n" +
		"            this.$textbox.attr('type', 'password'); \n" +
		"            return PasswordView.__super__.update.apply(this); \n" +
		"         }, \n" +
		"   }); \n" +
		"   WidgetManager.register_widget_view('PasswordView', PasswordView); \n" +
		"});\n";
	}
    }
    
    public class CameraWidget : Widgets.Widget {
	// based on work by Jason Grout
	// http://nbviewer.ipython.org/gist/jasongrout/9210458
	public string imageuri {
	    get { return Convert.ToString(get("imageuri")); }
	    set { set("imageuri", Convert.ToString(value)); }
	}

	public CameraWidget(ZMQServer.Session session) : base(session) {
	    set("_view_name", "CameraView");
	    set("imageuri", "");
	    // need to inject the javascript now:
	    session.shell_channel.SetState("waiting", ""); // wait for recv "comm_open"
	    session.display(session.calico.Javascript(javascript()));
	    Thread.Sleep(100); // miliseconds
	}

	public string javascript() {
	    return "require([\"widgets/js/widget\"], function(WidgetManager){\n" +
		"    var CameraView = IPython.DOMWidgetView.extend({\n" +
		"        render: function(){\n" +
		"            // based on https://developer.mozilla.org/en-US/docs/WebRTC/taking_webcam_photos\n" +
		"            var video        = $('<video>')[0];\n" +
		"            var canvas       = $('<canvas>')[0];\n" +
		"            var startbutton  = $('<button id = picture_button>Take Picture</button>')[0];\n" +
		"            var width = 320;\n" +
		"            var height = 0;\n" +
		"            var that = this;\n" +
		"\n" +
		"            setTimeout(function() {that.$el.append(video).append(startbutton).append(canvas);}, 200);\n" +
		"            //$(canvas).hide();\n" +
		"            //window.vvv=video;\n" +
		"            var streaming = false;\n" +
		"            navigator.getMedia = ( navigator.getUserMedia ||\n" +
		"                                 navigator.webkitGetUserMedia ||\n" +
		"                                 navigator.mozGetUserMedia ||\n" +
		"                                 navigator.msGetUserMedia);\n" +
		"\n" +
		"            navigator.getMedia({video: true, audio: false},\n" +
		"                function(stream) {\n" +
		"                  if (navigator.mozGetUserMedia) {\n" +
		"                    video.mozSrcObject = stream;\n" +
		"                  } else {\n" +
		"                    var vendorURL = window.URL || window.webkitURL;\n" +
		"                    video.src = vendorURL.createObjectURL(stream);\n" +
		"                  }\n" +
		"                  video.play();\n" +
		"                },\n" +
		"                function(err) {\n" +
		"                  console.log(\"An error occured! \" + err);\n" +
		"                }\n" +
		"            );\n" +
		"\n" +
		"            video.addEventListener('canplay', function(ev){\n" +
		"                if (!streaming) {\n" +
		"                  height = video.videoHeight / (video.videoWidth/width);\n" +
		"                  video.setAttribute('width', width);\n" +
		"                  video.setAttribute('height', height);\n" +
		"                  canvas.setAttribute('width', width);\n" +
		"                  canvas.setAttribute('height', height);\n" +
		"                  streaming = true;\n" +
		"                }\n" +
		"            }, false);\n" +
		"            function takepicture() {\n" +
		"                canvas.width = width;\n" +
		"                canvas.height = height;\n" +
		//      "                video.pause();\n" +
		//      "                $(video).fadeTo(1,0).delay(100).fadeTo(1,100);\n" +
		//      "                setTimeout(function() {video.play()}, 3000);\n" +
		"                canvas.getContext('2d').drawImage(video, 0, 0, width, height);\n" +
		"                that.model.set('imageuri',canvas.toDataURL('image/png'));\n" +
		"                that.touch();\n" +
		"            }\n" +
		"            startbutton.addEventListener('click', function(ev){\n" +
		"                takepicture();\n" +
		"                ev.preventDefault();\n" +
		"            }, false);\n" +
		"        },\n" +
		"    });\n" +
		"    \n" +
		"    // Register the DatePickerView with the widget manager.\n" +
		"    WidgetManager.register_widget_view('CameraView', CameraView);\n" +
		"\n" +
		"});";
	}

	public void click() {
	    session.calico.display(session.calico.Javascript("document.getElementById('picture_button').click();"));
	}
    }

    // ---------------------------------------------------------------
    // Not exactly widgets, but related Objects

    public static string ToJSON(object value) {
	if (value is bool) 
	    return ToJSON((bool)value);
	else if (value is string) 
	    return ToJSON((string)value);
	else if (value is IDictionary) 
	    return ToJSON((IDictionary)value);
	else if (value == null) 
	    return "null";
	else if (value is IEnumerable<string>) 
	    return ToJSON((IEnumerable<string>)value);
	else if (value is IList)
	    return ToJSON((IList)value);
	else
	    return value.ToString();
    }

    public static string ToJSON(IDictionary dict) {
	string retval = "";
	foreach (string key in dict.Keys) {
	    if (retval != "") {
		retval += ", ";
	    }
	    object value = dict[key];
	    retval += String.Format("'{0}': {1}", key, ToJSON(value));
	}
	return String.Format("{{{0}}}", retval);
    }

    public static string ToJSON(string item) {
	return String.Format("'{0}'", item); // FIXME: escape '
    }
    
    public static string ToJSON(IEnumerable<string> items) {
	// ['item', 'item']
	string retval = "";
	foreach (string item in items) {
	    if (retval != "") {
		retval += ", ";
	    }
	    retval += ToJSON(item);
	}
	return String.Format("[{0}]", retval);
    }

    public static IList Lines(int size) {
        List<List<object>> retval = new List<List<object>>() {new List<object>() { "x" }};
        for(int i=0; i < size; i += 1) {
            retval.Add( new List<object>() { String.Format("{0}", i + 1) });
	}
	return (IList)retval;
    }

    public static IList Lines(int start, int stop, int step) {
        List<List<object>> retval = new List<List<object>>() {new List<object>() { "x" }};
        for(int i=start; i < stop; i += step) {
            retval.Add( new List<object>() { String.Format("{0}", i) });
	}
	return (IList)retval;
    }

    public static IList Lines(IList previous, IList data, string label=null) {
        IList retval = previous;
	((IList)retval[0]).Add(label == null ? "Line " + ((IList)retval[0]).Count : label);
        int count = 0;
        foreach (IList row in retval) {
	    if (count > 0) {
		row.Add(data[count - 1]);
	    }
            count += 1;
	}
	return (IList)retval;
    }

    public static string ToJSON(IList items) {
	// ['item', 'item']
	string retval = "";
	foreach (var item in items) {
	    if (retval != "") {
		retval += ", ";
	    }
	    retval += ToJSON(item);
	}
	return String.Format("[{0}]", retval);
    }

    public class GeoChart {
	ZMQServer.Session session;
	public int id;
	IDictionary options;
	string table;

	public GeoChart(ZMQServer.Session session, IList keys, IDictionary data) 
	    : this(session, keys, data, new Dictionary<string,object>()) {
	}

	public GeoChart(ZMQServer.Session session, IList keys, IDictionary data, IDictionary options) {
	    // GeoChart(calico.sesssion, ['Country', 'Size'], {"Canada": 562, "United States": 987}, {"height"=300, "legend"=False})
	    this.session = session;
	    this.options = options;
	    table = ToJSON(keys);
	    foreach (string key in data.Keys) {
		if (table != "") {
		    table += ",\n";
		}
		table += String.Format("['{0}', {1}]", key, ToJSON(data[key]));
	    }
	}

	public Dictionary<string, string> GetRepresentations() {
	    id = Widgets.next_id++;
	    int height = 300;
	    if (options.Contains("height")) 
		height = Convert.ToInt32(options["height"]);
	    var data = new Dictionary<string, string>();
	    data["text/plain"] = "<GeoChart availble for viewing in notebook>";
	    data["text/html"] =String.Format("<div id=\"chart_div_{4}\" style=\"height: {5}px;\"></div>\n" +
					     "<script type=\"text/javascript\">\n" +
					     "  require(['{3}'], function () {{\n" +
					     "      function draw() {{\n" +
					     "        var chart = new google.visualization.GeoChart(document.getElementById('chart_div_{0}'));\n" +
					     "        chart.draw(google.visualization.arrayToDataTable([{1}]), {2});\n" + 
					     "      }}\n" +
					     "      google.load('visualization', '1.0',\n" + 
					     "                  {{'callback': draw, 'packages':['geochart']}});\n" +
					     "  }});\n" +
					     "</script>\n", 
					     id, table, ToJSON(options), "https://www.google.com/jsapi", id, height);
	    return data;
	}

	public void display() {
	    session.display(this);
	}
	public void animate() {
	    session.clear_output();
	    session.display(this);
	}
    }

    public class GoogleChart  {
	ZMQServer.Session session;
	public int id;
	IDictionary options;
	string table = "";
	string type;

	/*
	public string imageuri {
	    get { return Convert.ToString(get("imageuri")); }
	    set { set("imageuri", Convert.ToString(value)); }
	}
	*/

        public GoogleChart(ZMQServer.Session session, string type, IList list) 
	    : this(session, type, null, list, new Dictionary<string,object>()) {
        }

        public GoogleChart(ZMQServer.Session session, string type, IList keys, IList list) 
	    : this(session, type, keys, list, new Dictionary<string,object>()) {
        }

        public GoogleChart(ZMQServer.Session session, string type, IList list, IDictionary options) 
	    : this(session, type, null, list, options) {
        }

        public GoogleChart(ZMQServer.Session session, string type, IList keys, IList list, IDictionary options) { //: base(session) {
	    // GoogleChart("ScatterChart", ["X", "Y"], [[8, 12], [10, 9], [9, 10], [8, 12]],
            //height=300, width=500, lineWidth=1, legend='"none"')
	    /*
	      set("_view_name",  "ChartView");
	      set("disabled",    false);
	      set("visible",     true);
	      set("description", "");
	      set("imageuri",    "");
	    */
	    this.session = session;
	    this.options = options;
	    this.type = type;
	    int nCols = 0;

        // how many columns are there in the list?
        if (list.Count > 0){
            if (list[0] is IList) nCols = ((IList)list[0]).Count;
            else nCols = 1;
        }

        if (keys != null)
        {
            table = ToJSON(keys);
        }
        else
        {
            // if we aren't given any keys just make blanks; if one column make two labels
            table = "[''";
            int i = 0;
            while (((nCols == 1) && i < 1) || i < nCols-1)
            {
                table += ", ''";
                i = i + 1;
            }
            table += "]";
        }

        int t = 0;
	    foreach (var row in list) {
            if (table != "") {
                table += ",\n";
            }

            // if we only have one column of list, fake a time series
            if (nCols == 1)
            {
                // histograms expect categorical list in position 0
                if (type == "Histogram")
                    table += "['" + (t++) + "', " + ToJSON(row) + "]";
                else
                    table += "[" + (t++) + "," + ToJSON(row) + "]";
            }
            else
            {               
                table += ToJSON(row);
            }
	    }
	}

	public Dictionary<string, string> GetRepresentations() {
	    id = Widgets.next_id++;
	    int height = 300;
	    if (options.Contains("height")) 
		height = Convert.ToInt32(options["height"]);
	    var gdata = new Dictionary<string, string>();
	    gdata["text/plain"] = "<Chart available for viewing in notebook>";
	    gdata["text/html"] =String.Format(
       @"
        <div id=""chart_div_{3}"" style=""height: {4}px;""></div>
        <script type=""text/javascript"">
        require(['https://www.google.com/jsapi'], function () {{
        function drawChart() {{                
          var data = google.visualization.arrayToDataTable([{1}]);  
          var chart = new google.visualization.{5}(document.getElementById('chart_div_{0}'));
          google.visualization.events.addListener(chart, 'ready', function () {{
                if (! ('charts' in document)) {{
                    document.chart_uri = {{}};
                }}
                document.chart_uri['chart_{0}'] = chart.getImageURI();
          }});
          chart.draw(data, {2});
         }}
		google.load('visualization', '1.0', {{'callback': drawChart, 'packages':['corechart']}});
        }});
        </script>",
       id, table, ToJSON(options), id, height, type);
	    return gdata;
	}
	public void display() {
	    session.display(this);
	}

	public void animate() {
	    session.clear_output();
	    session.display(this);
	}
    }

    public class ScatterChart {
	ZMQServer.Session session;
	public int id;
	IDictionary options;
	string table;

	public ScatterChart(ZMQServer.Session session, IList keys, IList data) 
	    : this(session, keys, data, new Dictionary<string,object>()) {
	}

	public ScatterChart(ZMQServer.Session session, IList keys, IList data, IDictionary options) {
	    // ScatterChart(["X", "Y"], [[8, 12], [10, 9], [9, 10], [8, 12]], height=300, width=500, lineWidth=1, legend='"none"')
	    this.session = session;
	    this.options = options;
	    table = ToJSON(keys);
	    foreach (var row in data) {
		if (table != "") {
		    table += ",\n";
		}
		table += ToJSON(row);
	    }
	}

	public Dictionary<string, string> GetRepresentations() {
	    id = Widgets.next_id++;
	    int height = 300;
	    if (options.Contains("height")) 
		height = Convert.ToInt32(options["height"]);
	    var data = new Dictionary<string, string>();
	    data["text/plain"] = "<ScatterChart availble for viewing in notebook>";
	    data["text/html"] =String.Format("<div id=\"chart_div_{4}\" style=\"height: {5}px;\"></div>\n" +
					     "<script type=\"text/javascript\">\n" +
					     "  require(['{3}'], function () {{\n" +
					     "      function draw() {{\n" +
					     "        var chart = new google.visualization.ScatterChart(document.getElementById('chart_div_{0}'));\n" +
					     "        chart.draw(google.visualization.arrayToDataTable([{1}]), {2});\n" + 
					     "      }}\n" +
					     "      google.load('visualization', '1.0',\n" + 
					     "                  {{'callback': draw, 'packages':['corechart']}});\n" +
					     "  }});\n" +
					     "</script>\n", 
					     id, table, ToJSON(options), "https://www.google.com/jsapi", id, height);
	    return data;
	}

	public void display() {
	    session.display(this);
	}
	public void animate() {
	    session.clear_output();
	    session.display(this);
	}
    }

    public class LineChart {
	ZMQServer.Session session;
	public int id;
	IDictionary options;
	string table;

	public LineChart(ZMQServer.Session session, IList data) 
	    : this(session, data, new Dictionary<string,object>()) {
	}

	public LineChart(ZMQServer.Session session, IList data, IDictionary options) {
	    /*
	      var data = google.visualization.arrayToDataTable([
	      ['Year', 'Sales', 'Expenses'],
	      ['2004',  1000,      400],
	      ['2005',  1170,      460],
	      ['2006',  660,       1120],
	      ['2007',  1030,      540]
	      ]);
	    */
	    this.session = session;
	    this.options = options;
	    table = "";
	    foreach (IList<object> row in data) {
		if (table != "") {
		    table += ",\n";
		}
		table += ToJSON(row);
	    }
	}

	public Dictionary<string, string> GetRepresentations() {
	    id = Widgets.next_id++;
	    int height = 300;
	    if (options.Contains("height")) 
		height = Convert.ToInt32(options["height"]);
	    var data = new Dictionary<string, string>();
	    data["text/plain"] = "<LineChart availble for viewing in notebook>";
	    data["text/html"] =String.Format("<div id=\"chart_div_{4}\" style=\"height: {5}px;\"></div>\n" +
					     "<script type=\"text/javascript\">\n" +
					     "  require(['{3}'], function () {{\n" +
					     "      function draw() {{\n" +
					     "        var chart = new google.visualization.LineChart(document.getElementById('chart_div_{0}'));\n" +
					     "        chart.draw(google.visualization.arrayToDataTable([{1}]), {2});\n" + 
					     "      }}\n" +
					     "      google.load('visualization', '1.0',\n" + 
					     "                  {{'callback': draw, 'packages':['corechart']}});\n" +
					     "  }});\n" +
					     "</script>\n", 
					     id, table, ToJSON(options), "https://www.google.com/jsapi", id, height);
	    return data;
	}

	public void display() {
	    session.display(this);
	}
	public void animate() {
	    session.clear_output();
	    session.display(this);
	}
    }
}
