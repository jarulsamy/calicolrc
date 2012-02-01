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
		public static Dictionary<string,double> VariableNames = new Dictionary<string, double>();
		
		private string _source = null;
		private Microsoft.Scripting.Hosting.CompiledCode _compiled = null;

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
		}
		
		public CMethodBlock(Double X, Double Y, 
				    string assembly_name, 
				    string type_name, 
				    string method_name, 
				    List<string> names,
				    List<string> types,
				    List<string> defaults,
		            string return_type,
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
			clone.FillColor = this.FillColor;
			clone.LineColor = this.LineColor;
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
		
		private string paramListStringNames
		{
			get {
				List<String> paramlist = new List<String>();
				foreach (string pname in param_names) {
					if (_properties.ContainsKey(pname)) 
						paramlist.Add (pname);
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
		          )
		{
			this.assembly_name = assembly_name;
			this.type_name = type_name;
			this.method_name = method_name;			// Name of function/constructor to call
			this.param_names = names;				// Names of all parameters
			this.types = types;						// Parameter type strings
			this.defaults = defaults;				// Parameter default values (as strings)
			this.return_type = return_type;			// Return type string
			
			this.LineWidth = 2;						// Default block visual characteristics
			this.LineColor = Diagram.Colors.DarkBlue;
			this.FillColor = Diagram.Colors.LightBlue;
			this.Sizable = false;
			
			// Make a variable property to hold values *returned* from functions
			if (! return_type.ToString().Equals("System.Void")) {
				CVarNameProperty tvar = new CVarNameProperty("Variable", "");
				tvar.PropertyChanged += OnPropertyChanged;
				_properties["Variable"] = tvar;
			}
			
			// Make all properties to hold function arguments
			CExpressionProperty tprop = null;
			
			// Create parameters
			if (names != null)
			{
				for (int i = 0; i < names.Count; i++)
				{
					String formatted_default = "";
					
					// FIXME: make a default of the appropriate type if one not given
					if (defaults[i] == null || defaults[i] == "") {
						formatted_default = String.Format("{0}", 0);
						
					} else if (types[i] == "System.String") {
						formatted_default = String.Format("\"{0}\"", defaults[i]);
						
					} else if (!(defaults[i].Equals("System.DBNull"))) {
						formatted_default = String.Format("{0}", defaults[i]);
						
					} else {
						formatted_default = String.Format("{0}", 0);
					}
					
					tprop = new CExpressionProperty(names[i], formatted_default);
					tprop.PropertyChanged += OnPropertyChanged;
				    _properties[names[i]] = tprop;					
					
				}
			}

			// Setup and init Property
			this.OnPropertyChanged(null, null);
		}
		
        // - - - Update text when property changes - - - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs e)
		{
			// Get variable name to assign, if one was set
			string varname = "";
			if (_properties.ContainsKey("Variable")) varname = _properties["Variable"].Text;
			
			// Build string to display in block
			if (varname.Length > 0) {
				if (IsFactory) {
					this.Text = String.Format("{0}={1}({2})", varname, method_name, paramListStringNames);
				} else {
					this.Text = String.Format("{0}={1}({2})", varname, method_name, paramListString);
				}
 			} else {
				if (IsFactory) {
					this.Text = String.Format("{0}({1})", method_name, paramListStringNames);
				} else {
					this.Text = String.Format("{0}({1})", method_name, paramListString);
				}
			}
		}
		
		// - - - Generate and return Python procedure call - - - - - - - - -
		private string ToPython ()
		{
			// Get variable name to assign, if one was set
			string varname = "";
			if (_properties.ContainsKey("Variable")) varname = _properties["Variable"].Text;
			
			// Build Python
			string code = String.Format("{0}.{1}({2})", assembly_name, method_name, paramListString);
			if (varname.Length > 0) code = String.Format("{0} = {1}", varname, code);
			this._source = code;
			
			return code;
		}
		
		// - - - Append line of code to StringBuilder - - - - - - - - - - - -
		public override bool ToPython (System.Text.StringBuilder o, int indent)
		{
			try
			{
				string code = this.ToPython();
				string sindent = new string (' ', 2*indent);
				
				o.AppendFormat("{0}{1}\n", sindent, code);
				
				// Continue to connected Block, if there is one
				if (this.OutEdge.IsConnected) {
					CBlock b = this.OutEdge.LinkedTo.Block;
					b.ToPython(o, indent);
				}
				
			} catch (Exception ex){
				Console.WriteLine("{0} (in CMethodBlock.ToPython)", ex.Message);
				return false;
			}
			
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override bool Compile(Microsoft.Scripting.Hosting.ScriptEngine engine, Jigsaw.Canvas cvs)
		{
			try
			{
				// Compile
				string code = this.ToPython();
				
				ScriptSource ssrc = engine.CreateScriptSourceFromString(code, Microsoft.Scripting.SourceCodeKind.Statements);
				_compiled = ssrc.Compile();
				
				return true;
				
			} catch (Exception ex) {
				Console.WriteLine ("Block {0} failed compilation: {1}", this.Name, ex.Message);
				return false;
			}
			
		}
		
		// - - - Return a list of all assembly names required for this block - - - - - - -
		public override List<string> RequiredAssemblies
		{
			get 
			{
				return new List<string>() {assembly_name};
			}
		}
		
		// - - - Execute statement - - - - - - - - - - - - - - - - - - - - - 
		public override IEnumerator<RunnerResponse> Runner( ScriptScope scope, CallStack stack ) 
		{
			// - - - Always place this block of code at the top of all block runners
			this.State = BlockState.Running;				// Indicate that the block is running
			RunnerResponse rr = new RunnerResponse();		// Create and return initial response object
			yield return rr;
			if (this.BreakPoint == true) {					// Indicate if breakpoint is set on this block
				rr.Action = EngineAction.Pause;				// so that engine can stop
				//rr.Frame = null;
				yield return rr;
			}
			
			// - - - Execute the statement - - - - - - - - - - - - - - - -
			try {
				_compiled.Execute(scope);
			} catch (Exception ex) {
				Console.WriteLine(ex.Message);
				this["Message"] = ex.Message;
				this.State = BlockState.Error;
				rr.Action = EngineAction.Error;
				rr.Frame = null;
			}
			
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	
			// Go into a loop while block remains in an error state
			while (this.State == BlockState.Error) yield return rr;
	
			// If connected, replace this runner with the next runner to the stack.
			if (this.OutEdge.IsConnected) {
				rr.Action = EngineAction.Replace;
				rr.Frame = this.OutEdge.LinkedTo.Block.Frame(scope, stack);
			} else {
				// If not connected, just remove this runner
				rr.Action = EngineAction.Remove;
				rr.Frame = null;
			}
			
			// Indicate that the block is no longer running
			this.State = BlockState.Idle;
			yield return rr;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void ReadXmlEndElement(XmlReader r)
		{
			// end of method, call SetValues, even though we have all of the info; we need
			// to do the constructing
			if (r.Name == "method") {
				setValues(assembly_name, type_name, method_name, param_names, types, defaults, return_type);
			} 
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void ReadXmlTag(XmlReader xr)
		{
			if (xr.Name == "method")
			{
				assembly_name = xr.GetAttribute("assembly_name");
				type_name = xr.GetAttribute("type_name");
				method_name = xr.GetAttribute("method_name");
				return_type = xr.GetAttribute("return_type");
				
			} else if (xr.Name == "parameter") {
				
				string parameter_name = xr.GetAttribute("name");
				string parameter_type = xr.GetAttribute("type");
				string parameter_default = xr.GetAttribute("default");
				param_names.Add(parameter_name);
				types.Add(parameter_type);
				
				if (!parameter_default.Equals("")) {
					defaults.Add(parameter_default);
				} else {
					defaults.Add("System.DBNull");
				}
			}
		}
	
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected override void WriteXmlTags(XmlWriter xw)
		{
			// Write the method tag first, so when we read it first
			// and make the full CMethodBlock before the variables
			xw.WriteStartElement("method");
			xw.WriteAttributeString("assembly_name", assembly_name);
			xw.WriteAttributeString("type_name", type_name);
			xw.WriteAttributeString("method_name", method_name);
			xw.WriteAttributeString("return_type", return_type);
			
		    for (int n = 0; n < param_names.Count; n++) {
				xw.WriteStartElement("parameter");
				xw.WriteAttributeString("name", param_names[n]);
				xw.WriteAttributeString("type", types[n]);
				xw.WriteAttributeString("default", defaults[n].ToString());
				xw.WriteEndElement();
		    }
	  		xw.WriteEndElement();
	  		base.WriteXmlTags(xw);
		}
	}
}
