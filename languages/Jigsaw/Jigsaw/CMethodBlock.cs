//
//  CMethodBlock.cs
//  
//  Author:
//       Mark F. Russo <russomf@gmail.com>
// 
//  Copyright (c) 2013 The Calico Project
// 
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
// 
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
// 
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

using System;
using System.Collections;
using System.Collections.Generic;
using System.Xml;
using Microsoft.Scripting.Hosting;

namespace Jigsaw
{
	public class CMethodBlock : CBlock
	{
		public string AssemblyName;
		public string TypeName;
		public string MethodName;
		public List<string> ParamNames;
		public List<string> ParamTypes;
		public List<string> ParamDefaults;
		private string _returnType = "System.Void";
		//public string ReturnType;
		//public static Dictionary<string,double> VariableNames = new Dictionary<string, double>();
		
		private string _source = null;
		private Microsoft.Scripting.Hosting.CompiledCode _compiled = null;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CMethodBlock(Double X, Double Y, Widgets.CBlockPalette palette = null) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y),
				new Diagram.CPoint(X + CBlock.BlockWidth, Y + 20)}),
				palette)
		{
			setValues("", "", "", new List<string>(), new List<string>(), new List<string>(), "System.Void");
//			assembly_name = "";
//			type_name = "";
//			method_name = "";
//			param_names = new List<string>();
//			types = new List<string>();
//			defaults = new List<string>();
//			return_type = "System.Void";
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
					new Diagram.CPoint(X + CBlock.BlockWidth, Y + 20)}),
					palette )
		{
			setValues(assembly_name, type_name, method_name, names, types, defaults, return_type);
		}
		
		public CMethodBlock(Double X, Double Y) : this(X, Y, null) {}
		
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
			this.AssemblyName = assembly_name;
			this.TypeName = type_name;
			this.MethodName = method_name;			// Name of function/constructor to call
			this.ParamNames = names;				// Names of all parameters
			this.ParamTypes = types;				// Parameter type strings
			this.ParamDefaults = defaults;			// Parameter default values (as strings)

			this.LineWidth = 2;						// Default block visual characteristics
			this.LineColor = Diagram.Colors.DarkBlue;
			this.FillColor = Diagram.Colors.LightBlue;
			this.Sizable = false;

			// Make a variable property to hold values *returned* from functions
			this.ReturnType = return_type;

			// Create parameters as block properties
			if (names != null) {
				for (int i = 0; i < names.Count; i++) {
					AddExpressionProperty(names[i], types[i], defaults[i]);
				}
			}

			// Make a variable property to hold values *returned* from functions
//			this._returnType = return_type;			// Return type string
//			if (!return_type.ToString().Equals("System.Void")) {
//				CVarNameProperty tvar = new CVarNameProperty("Variable", "");
//				tvar.PropertyChanged += OnPropertyChanged;
//				_properties["Variable"] = tvar;
//			}

//			// Make all properties to hold function arguments
//			CExpressionProperty tprop = null;
//			
//			// Create parameters
//			if (names != null)
//			{
//				for (int i = 0; i < names.Count; i++)
//				{
//					String formatted_default = "";
//					
//					// FIXME: make a default of the appropriate type if one not given
//					if (defaults[i] == null || defaults[i] == "") {
//						formatted_default = String.Format("{0}", 0);
//						
//					} else if (types[i] == "System.String") {
//						formatted_default = String.Format("\"{0}\"", defaults[i]);
//						
//					} else if (!(defaults[i].Equals("System.DBNull"))) {
//						formatted_default = String.Format("{0}", defaults[i]);
//						
//					} else {
//						formatted_default = String.Format("{0}", 0);
//					}
//					
//					tprop = new CExpressionProperty(names[i], formatted_default);
//					tprop.PropertyChanged += OnPropertyChanged;
//				    _properties[names[i]] = tprop;
//				}
//			}

			// Setup and init Property
			this.OnPropertyChanged(null, null);
		}

		// - - - Create a new expression property with appropriate default for the block - - -
		public bool AddExpressionProperty(string propName, string propType, string propDefault = null) 
		{
			String formattedDefault = "";

			try 
			{
				// FIXME: make a default of the appropriate type if one not given
				if (propDefault == null || propDefault == "") {
					formattedDefault = String.Format("{0}", 0);
					
				} else if (propType == "System.String") {
					formattedDefault = String.Format("\"{0}\"", propDefault);
					
				} else if (!(propDefault.Equals("System.DBNull"))) {
					formattedDefault = String.Format("{0}", propDefault);
					
				} else {
					formattedDefault = String.Format("{0}", 0);
				}

				// If the property already exists, delete it first
				if (_properties.ContainsKey (propName)) {
					_properties[propName].PropertyChanged -= OnPropertyChanged;
					_properties.Remove (propName);
				}

				// Add the new property
				CExpressionProperty tprop = new CExpressionProperty(propName, formattedDefault);
				tprop.PropertyChanged += OnPropertyChanged;
			    _properties[propName] = tprop;

				this.OnPropertyChanged(null, null);

				return true;
			}
			catch (Exception ex) {
				Console.WriteLine ("Error in CMethodBlock.AddProperty: {0}", ex.Message);
				return false;
			}
		}

		// - - - Sets or modifies the return type of the method block - - - 
		public string ReturnType
		{
			set {
				// Make a variable property to hold values *returned* from functions
				this._returnType = value;			// Return type string

				// Delete current Variable property, if exists
				if (_properties.ContainsKey ("Variable")) {
					_properties["Variable"].PropertyChanged -= OnPropertyChanged;
					_properties.Remove ("Variable");
				}

				if (value != "System.Void") {
					CVarNameProperty tvar = new CVarNameProperty("Variable", "");
					tvar.PropertyChanged += OnPropertyChanged;
					_properties["Variable"] = tvar;
				}

				this.OnPropertyChanged(null, null);
			}
			get
			{
				return _returnType;
			}
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
					this.Text = String.Format("{0}={1}({2})", varname, MethodName, paramListStringNames);
				} else {
					this.Text = String.Format("{0}={1}({2})", varname, MethodName, paramListString);
					RaiseBlockChanged();
				}
 			} else {
				if (IsFactory) {
					this.Text = String.Format("{0}({1})", MethodName, paramListStringNames);
				} else {
					this.Text = String.Format("{0}({1})", MethodName, paramListString);
					RaiseBlockChanged();
				}
			}
		}
				
		// - - - 
		public override CBlock Clone(double X, double Y, bool cloneEdges) 
		{	// Clone this block. Optionally clone edges.
			CBlock clone = (CBlock)base.Clone(X, Y, cloneEdges);
			((CMethodBlock)clone).setValues(this.AssemblyName, this.TypeName, this.MethodName, this.ParamNames, 
								this.ParamTypes, this.ParamDefaults, this.ReturnType);
			clone.FillColor = this.FillColor;
			clone.LineColor = this.LineColor;
			clone.Text = this.Text;
			return clone;
		}

		// - - - Private util to build delimited parameter name list string - - - - - -
		private string paramListString
		{
			get {
				List<String> paramlist = new List<String>();
				foreach (string pname in ParamNames) {
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
				foreach (string pname in ParamNames) {
					if (_properties.ContainsKey(pname)) 
						paramlist.Add (pname);
				}
				return String.Join (", ", paramlist);
			}
		}

		// - - - Generate and return Python procedure call - - - - - - - - -
		private string ToPython ()
		{
			// Get variable name to assign, if one was set
			string varname = "";
			if (_properties.ContainsKey("Variable")) varname = _properties["Variable"].Text;
			
			// Build Python
			string code = String.Format("{0}.{1}({2})", AssemblyName, MethodName, paramListString);
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
				string sindent = new string (' ', Constant.SPACES * indent);
				
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
				_compiled = ssrc.Compile(Compiler.Options(engine));
				
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
				return new List<string>() {AssemblyName};
			}
		}
		
		// - - - Execute statement - - - - - - - - - - - - - - - - - - - - - 
		public override IEnumerator<RunnerResponse> Runner( ScriptScope scope, CallStack stack ) 
		{
			// - - - Always place this block of code at the top of all block runners
			this.State = RunningState.Running;				// Indicate that the block is running
			RunnerResponse rr = new RunnerResponse();		// Create and return initial response object
			yield return rr;
			if (this.BreakPoint == true) {					// Indicate if breakpoint is set on this block
				rr.Action = EngineAction.Pause;				// so that engine can stop
				//rr.Frame = null;
				yield return rr;
			}
			
			// - - - Execute the statement - - - - - - - - - - - - - -
			// - - - - -

			System.Threading.ManualResetEvent ev = new System.Threading.ManualResetEvent(false);
			System.Threading.Thread t = new System.Threading.Thread(() => {
				  try {
					_compiled.Execute(scope);
				  } catch (Exception ex) {
					this["Message"] = ex.Message;
					this.State = RunningState.Error;
					rr.Action = EngineAction.Error;
					rr.Frame = null;
				  } finally {
					ev.Set();
				  }
				});
			t.Start();
			while (!ev.WaitOne(0)) { // Are we done?
			  while (Gtk.Application.EventsPending ())
				Gtk.Application.RunIteration ();
			}
			
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	
			// Go into a loop while block remains in an error state
			while (this.State == RunningState.Error) yield return rr;
	
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
			this.State = RunningState.Idle;
			yield return rr;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void ReadXmlEndElement(XmlReader r)
		{
			// end of method, call SetValues, even though we have all of the info; we need
			// to do the constructing
			if (r.Name == "method") {
				setValues(AssemblyName, TypeName, MethodName, ParamNames, ParamTypes, ParamDefaults, ReturnType);
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void ReadXmlTag(XmlReader xr)
		{
			if (xr.Name == "method")
			{
				AssemblyName = xr.GetAttribute("assembly_name");
				TypeName = xr.GetAttribute("type_name");
				MethodName = xr.GetAttribute("method_name");
				ReturnType = xr.GetAttribute("return_type");
				
			} else if (xr.Name == "parameter") {
				
				string parameter_name = xr.GetAttribute("name");
				string parameter_type = xr.GetAttribute("type");
				string parameter_default = xr.GetAttribute("default");
				ParamNames.Add(parameter_name);
				ParamTypes.Add(parameter_type);
				
				if (!parameter_default.Equals("")) {
					ParamDefaults.Add(parameter_default);
				} else {
					ParamDefaults.Add("System.DBNull");
				}
			}
		}
	
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected override void WriteXmlTags(XmlWriter xw)
		{
			// Write the method tag first, so when we read it first
			// and make the full CMethodBlock before the variables
			xw.WriteStartElement("method");
			xw.WriteAttributeString("assembly_name", AssemblyName);
			xw.WriteAttributeString("type_name", TypeName);
			xw.WriteAttributeString("method_name", MethodName);
			xw.WriteAttributeString("return_type", ReturnType);
			
		    for (int n = 0; n < ParamNames.Count; n++) {
				xw.WriteStartElement("parameter");
				xw.WriteAttributeString("name", ParamNames[n]);
				xw.WriteAttributeString("type", ParamTypes[n]);
				xw.WriteAttributeString("default", ParamDefaults[n].ToString());
				xw.WriteEndElement();
		    }
	  		xw.WriteEndElement();
	  		base.WriteXmlTags(xw);
		}
	}
}
