using System;
using System.Collections;
using System.Collections.Generic;
using System.Xml;
using Microsoft.Scripting.Hosting;

namespace Jigsaw
{
	public class CMethodBlock : CBlock
	{
		string assembly_name;
		public string type_name;
		public string method_name;
		public List<string> param_names;
		List<string> types;
		List<string> defaults;
		public string return_type;
		//List<Type> types;
		//List<object> defaults;
		//public Type return_type;
		public static Dictionary<string,double> VariableNames = new Dictionary<string, double>();
		
		private bool _inHandler = false;	// Breaks re-entrant OnPropertyChanged event loops
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CMethodBlock(Double X, Double Y, Widgets.CBlockPalette palette = null) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y),
				new Diagram.CPoint(X + 175, Y + 20)}),
				palette)
		{
			assembly_name = "";
			type_name = "";
			method_name = "";
			param_names = new List<string>();
			types = new List<string>();
			defaults = new List<string>();
			return_type = "System.Void";
			//types = new List<Type>();
			//defaults = new List<object>();
			//return_type = System.Type.GetType("System.Void");
		}
		
		public CMethodBlock(Double X, Double Y, 
				    string assembly_name, 
				    string type_name, 
				    string method_name, 
				    List<string> names,
				    List<string> types,
				    List<string> defaults,
		            string return_type,
		            //List<Type> types,
		            //List<object> defaults,
				    //Type return_type,
				    Widgets.CBlockPalette palette = null) 
				: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
					new Diagram.CPoint(X, Y),
					new Diagram.CPoint(X + 175, Y + 20)}),
					palette )
		{
			setValues(assembly_name, type_name, method_name, names, types, defaults, return_type);
		}
		
		public CMethodBlock(Double X, Double Y) : this(X, Y, null) {}
		
		// - - - 
		public override CBlock Clone(double X, double Y, bool cloneEdges) 
		{	// Clone this block. Optionally clone edges.
			CBlock clone = (CBlock)base.Clone(X, Y, cloneEdges);
			((CMethodBlock)clone).setValues(this.assembly_name, this.type_name, this.method_name, this.param_names, 
								this.types, this.defaults, this.return_type);
			return clone;
		}
		
        // - - - Private util to build delimited parameter name list string - - - - - -
		private string paramListString
		{
			get {
				List<String> paramlist = new List<String>();
				foreach (string pname in param_names) {
					if (_properties.ContainsKey(pname) && _properties[pname].Text.Length > 0) 
						paramlist.Add (_properties[pname].Text);
				}
				return String.Join (", ", paramlist);
			}
		}
		
		// - - - 
	    public void setValues(string assembly_name, 
				  string type_name, 
				  string method_name, 
				  List<string> names,
				  List<string> types,
				  List<string> defaults,
		          string return_type
		          //List<Type> types,
		          //List<object> defaults,
				  //Type return_type
		          )
		{
			this.assembly_name = assembly_name;
			this.type_name = type_name;
			this.method_name = method_name;
			this.param_names = names;				// Names of all parameters
			this.types = types;
			this.defaults = defaults;
			this.return_type = return_type;
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkBlue;
			this.FillColor = Diagram.Colors.LightBlue;
			this.Sizable = false;
			string parameter_list = "";
			string block_text = "pass";
			
			// Make a variable property to hold values *returned* from functions
			if (! return_type.ToString().Equals("System.Void")) {
				//_properties["Variable"] = new CVarNameProperty("Variable", String.Format("{0}", MakeVariableName(method_name)));
				CVarNameProperty tvar = new CVarNameProperty("Variable", "");
				tvar.PropertyChanged += OnPropertyChanged;
				_properties["Variable"] = tvar;
			}
			
			// Make all properties to hold function arguments
			CExpressionProperty tprop = null;
			
			if (names != null)
			{
				// Create parameters
				for (int n = 0; n < names.Count; n++)
				{
					// FIXME: make a default of the appropriate type if one not given
					if (defaults[n] == null || defaults[n] == "") {
						tprop = new CExpressionProperty(names[n], String.Format("{0}", 0));
						tprop.PropertyChanged += OnPropertyChanged;
						_properties[names[n]] = tprop;
						
					//} else if (!(defaults[n].GetType().ToString().Equals("System.DBNull"))) {
					} else if (!(defaults[n].Equals("System.DBNull"))) {
						tprop = new CExpressionProperty(names[n], String.Format("{0}", defaults[n]));
						tprop.PropertyChanged += OnPropertyChanged;
					    _properties[names[n]] = tprop;
						
					} else {
						tprop = new CExpressionProperty(names[n], String.Format("{0}", 0));
						tprop.PropertyChanged += OnPropertyChanged;
					    _properties[names[n]] = tprop;
					}
					
//					if (parameter_list == "")
//						parameter_list = names[n];
//					else
//						parameter_list += "," + names[n];
				}
				
				parameter_list = String.Join (",", names);
				block_text = String.Format("{0}({1})", method_name, parameter_list);
			//} else {
				//block_text = String.Format("method");
			}
			
			//this.Text = block_text;
			
			// Build statement from text
			CStatementProperty Stat = new CStatementProperty("Statement", block_text);
			Stat.Visible = false;
			Stat.PropertyChanged += OnPropertyChanged;
			_properties["Statement"] = Stat;
			this.OnPropertyChanged(null, null);
		}
		
        // - - - Update text when property changes - - - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs e)
		{
			// Setting the Statement property re-raises this event. Supress re-entrant calls.
			if (_inHandler == true) return;
			_inHandler = true;
			
			// Get variable name to assign, if one was set
			string varname = "";
			if (_properties.ContainsKey("Variable")) varname = _properties["Variable"].Text;
			
			// Build string to display in block
			if (varname.Length > 0) {
				this["Statement"] = String.Format("{0}={1}({2})", varname, method_name, paramListString);
			} else {
				this["Statement"] = String.Format("{0}({1})", method_name, paramListString);
			}
			this.Text = String.Format("{0}", this["Statement"]);
			
			// Cancel re-entrant inhibition
			_inHandler = false;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override bool Compile(Microsoft.Scripting.Hosting.ScriptEngine engine, Jigsaw.Canvas cvs)
		{
			try {
				// Rebuild statement and compile. Result is stored in property for later execution.
				CStatementProperty Stat = (CStatementProperty)_properties["Statement"];
				
				// Build statement with no variable to assign.
				string statement = String.Format("{0}.{1}({2})", assembly_name, method_name, paramListString);
				
				// Get variable name to assign, if one was set
				string varname = "";
				if (_properties.ContainsKey("Variable")) varname = _properties["Variable"].Text;
				
				// Add assignment if variable specified
				if (varname.Length > 0) statement = String.Format("{0}={1}", varname, statement);
				
				// Compile
				Stat.Compile(engine, statement);
				
				return true;
				
			} catch (Exception ex) {
				Console.WriteLine ("Block {0} failed compilation: {1}", this.Name, ex.Message);
				return false;
			}
			
		}
		
		// - - - Execute statement - - - - - - - - - - - - - - - - - - - - - 
		public override IEnumerator<RunnerResponse> Runner( ScriptScope scope, CallStack stack ) 
		{
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			// Always place this block of code at the top of all block runners
			this.State = BlockState.Running;				// Indicate that the block is running
			RunnerResponse rr = new RunnerResponse();		// Create and return initial response object
			yield return rr;
			if (this.BreakPoint == true) {					// Indicate if breakpoint is set on this block
				rr.Action = EngineAction.Break;				// so that engine can stop
				rr.Runner = null;
				yield return rr;
			}
			
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			// Execute the statement
			try {
				CStatementProperty Stat = (CStatementProperty)_properties["Statement"];
				Stat.Evaluate(scope);
			} catch (Exception ex) {
				Console.WriteLine(ex.Message);
				this["Message"] = ex.Message;
				
				this.State = BlockState.Error;
				rr.Action = EngineAction.NoAction;
				rr.Runner = null;
			}
			
//			try 
//			{
//				// First, get the expressions from the properties and evaluate them:
//				List<object> args = new List<object>();
//				List<Type> arg_types = new List<Type>();
//				System.Reflection.MethodInfo method = null;
				
//				foreach (string name in param_names) {
//					CExpressionProperty prop = (CExpressionProperty)_properties[name];
//					object value = prop.Evaluate(scope); 
//					args.Add(value);
//					arg_types.Add(value.GetType());
//				}
				
//				// Next, get the type and correct method based on the args above
//				Type type = Reflection.Utils.getType(assembly_name, type_name);
//				if (type != null)
//				{
//					method = Reflection.Utils.getMethodFromArgTypes(type, method_name, arg_types.ToArray());
//					// and call it, if it is valid:
//					object result = null;
//					
//					if (method != null) 
//					{
//						try {
//							result = method.Invoke(type, args.ToArray());
//						} catch (Exception ex) {
//							this["Message"] = ex.Message;
//							Console.WriteLine (this["Message"]);
//							this.State = BlockState.Error;
//							rr.Action = EngineAction.NoAction;
//							rr.Runner = null;
//					    }
//						
//					    if (!(return_type.ToString().Equals("System.Void"))) {
//							CVarNameProperty VarName = (CVarNameProperty)_properties["Variable"];
//							// Need to set LHS to evaluated expression:
//							// First, set _ = RHS
//							scope.SetVariable("_", result);
//							// Then set LHS = _
//							// @@@
//							scope.SetVariable(VarName.Text, "_");
//					    }
//						
//					} else {
//						this["Message"] = "No matching method for these argument types";
//						Console.WriteLine (this["Message"]);
//						this.State = BlockState.Error;
//						rr.Action = EngineAction.NoAction;
//						rr.Runner = null;
//					}
//					
//				} else {
//					this["Message"] = "Can't find assembly";
//					Console.WriteLine (this["Message"]);
//					this.State = BlockState.Error;
//					rr.Action = EngineAction.NoAction;
//					rr.Runner = null;
//				}
//				
//			} catch (Exception ex) {
//				this["Message"] = ex.Message;
//				Console.WriteLine (this["Message"]);
//				this.State = BlockState.Error;
//				rr.Action = EngineAction.NoAction;
//				rr.Runner = null;
//			}
			
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	
			// Go into a loop while block remains in an error state
			while (this.State == BlockState.Error) yield return rr;
	
			// If connected, replace this runner with the next runner to the stack.
			if (this.OutEdge.IsConnected) {
				rr.Action = EngineAction.Replace;
				rr.Runner = this.OutEdge.LinkedTo.Block.Runner(scope, stack);
			} else {
				// If not connected, just remove this runner
				rr.Action = EngineAction.Remove;
				rr.Runner = null;
			}
			
			// Indicate that the block is no longer running
			this.State = BlockState.Idle;
			yield return rr;
		}

//		// - - - 
//		public string MakeVariableName(string methodname)
//		{
//			methodname = methodname.ToLower();
//			string vname = "";
//			if (methodname.StartsWith("make")) {
//				vname = methodname.Substring("make".Length);
//			} else if (methodname.StartsWith("get")) {
//				vname = methodname.Substring("get".Length);
//			} else {
//				vname = methodname;
//			}
//			// FIXME: for some reason, it is created twice on each drop
//			if (!VariableNames.ContainsKey(vname)) {
//				VariableNames[vname] = 0;
//			} else {
//				if (! _isFactory)
//					VariableNames[vname] = VariableNames[vname] + .5;
//			}
//			return vname.ToUpper() + VariableNames[vname].ToString();
//		}
		
		// - - - 
		public override void ReadXmlEndElement(XmlReader r)
		{
			// end of method, call SetValues, even though we have all of the info; we need
			// to do the constructing
			if (r.Name == "method") {
				setValues(assembly_name, type_name, method_name, param_names, types, defaults, return_type);
			} 
		}
		
		// - - - 
		public override void ReadXmlTag(XmlReader xr)
		{
			//</method>
			if (xr.Name == "method")
			{
				//<method assembly_name="Myro" type_name="Myro" method_name="askQuestion" return_type="System.String">
				//Console.WriteLine("ok ReadXmlTag save item");
				assembly_name = xr.GetAttribute("assembly_name");
				type_name = xr.GetAttribute("type_name");
				method_name = xr.GetAttribute("method_name");
				return_type = xr.GetAttribute("return_type");
				//return_type = System.Type.GetType(xr.GetAttribute("return_type"));
				
			} else if (xr.Name == "parameter")
			{
				//<parameter name="question" type="System.String" default="" />
				//<parameter name="choices" type="IronRuby.Builtins.RubyArray" default="" />
				string parameter_name = xr.GetAttribute("name");
				string parameter_type = xr.GetAttribute("type");
				string parameter_default = xr.GetAttribute("default");
				param_names.Add(parameter_name);
				types.Add(parameter_type);
				//types.Add(System.Type.GetType(parameter_type));
				
				if (!parameter_default.Equals("")) {
					defaults.Add(parameter_default);
					//defaults.Add(System.Type.GetType(parameter_default));
				} else {
					defaults.Add("System.DBNull");
					//defaults.Add(System.DBNull.Value);
				}
			}
		}
	
		// - - - 
		protected override void WriteXmlTags(XmlWriter xw)
		{
			// Write the method tag first, so when we read it first
			// and make the full CMethodBlock before the variables
			xw.WriteStartElement("method");
			xw.WriteAttributeString("assembly_name", assembly_name);
			xw.WriteAttributeString("type_name", type_name);
			xw.WriteAttributeString("method_name", method_name);
			xw.WriteAttributeString("return_type", return_type);
			//xw.WriteAttributeString("return_type", return_type.AssemblyQualifiedName);
			
		    for (int n = 0; n < param_names.Count; n++) {
				xw.WriteStartElement("parameter");
				xw.WriteAttributeString("name", param_names[n]);
				xw.WriteAttributeString("type", types[n]);
				//xw.WriteAttributeString("type", types[n].AssemblyQualifiedName);
				xw.WriteAttributeString("default", defaults[n].ToString());
				xw.WriteEndElement();
		    }
	  		xw.WriteEndElement();
	  		base.WriteXmlTags(xw);
		}
	}
}
