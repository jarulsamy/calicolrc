using System;
using System.Collections;
using System.Collections.Generic;

namespace Jigsaw
{
		public class CMethodBlock : CBlock
		{	
		  string assembly_name;
		  string type_name;
		  string method_name;
		  List<string> names;
		  List<Type> types;
		  List<object> defaults;
		  Type return_type;
			
	        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	        public CMethodBlock(Double X, Double Y, bool isFactory) : 
			base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y),
				new Diagram.CPoint(X + 175, Y + 20)}),
				isFactory)
			{
		}
		
		public CMethodBlock(Double X, Double Y) : this(X, Y, false) {
		}
		
		public CMethodBlock(Double X, Double Y, 
				    string assembly_name, 
				    string type_name, 
				    string method_name, 
				    List<string> names,
				    List<Type> types,
				    List<object> defaults,
				    Type return_type,
				    bool isFactory) 
				: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
					new Diagram.CPoint(X, Y),
					new Diagram.CPoint(X + 175, Y + 20)}),
					isFactory )
		{
		  setValues(assembly_name, type_name, method_name, names, types, defaults, return_type);
		}

		public override CBlock Clone(double X, double Y, bool cloneEdges) 
		{	// Clone this block. Optionally clone edges.
				CBlock clone = (CBlock)base.Clone(X, Y, cloneEdges);
				((CMethodBlock)clone).setValues(this.assembly_name, this.type_name, this.method_name, this.names, 
								this.types, this.defaults, this.return_type);
				return clone;
		    }	

	    public void setValues(string assembly_name, 
				  string type_name, 
				  string method_name, 
				  List<string> names,
				  List<Type> types,
				  List<object> defaults,
				  Type return_type)
		  {
				this.assembly_name = assembly_name;
				this.type_name = type_name;
				this.method_name = method_name;
				this.names = names;
				this.types = types;
				this.defaults = defaults;
				this.return_type = return_type;
				this.LineWidth = 2;
				this.LineColor = Diagram.Colors.DarkBlue;
				this.FillColor = Diagram.Colors.LightBlue;
				this.Sizable = false;
				string parameter_list = "";
				string block_text;
				if (! return_type.ToString().Equals("System.Void")) {
				  _properties["Variable"] = new CVarNameProperty("Variable", 
										 String.Format("{0}", method_name.ToUpper()));
				}
				if (names != null) {
					for (int n = 0; n < names.Count; n++) {
					  if (types[n].ToString().Equals("System.String")) {
					    if (!(defaults[n].GetType().ToString().Equals("System.DBNull")))
					      _properties[names[n]] = new CExpressionProperty(names[n], 
											      String.Format("'{0}'", 
													    defaults[n])); 
					    else
					      _properties[names[n]] = new CExpressionProperty(names[n], 
											      String.Format("'{0}'", 
													    names[n])); 
					  } else {
					    if (!(defaults[n].GetType().ToString().Equals("System.DBNull")))
					      _properties[names[n]] = new CExpressionProperty(names[n], 
											      String.Format("{0}", defaults[n]));
					    else
					      // FIXME: make a default of the appropriate type
					      _properties[names[n]] = new CExpressionProperty(names[n], 
											      String.Format("{0}", 0));
					  }
					  if (parameter_list == "")
					    parameter_list = names[n];
					  else
					    parameter_list += "," + names[n];
				}
				        block_text = String.Format("{0}({1})", method_name, parameter_list);
				} else {
					block_text = String.Format("method");
				}
	    		this.Text = block_text;
			}

		public override IEnumerator<RunnerResponse> 
		Runner(Dictionary<string, object> locals, Dictionary<string, object> builtins) 
			{	// Execute print statement
	
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
				// Do the print
				// TODO: Allow access to global namespace
				try {
					// First, get the expressions from the properties and evaluate them:
					List<object> args = new List<object>();
					List<Type> arg_types = new List<Type>();
					System.Reflection.MethodInfo method = null;
					foreach (string name in names) {
						CExpressionProperty prop = (CExpressionProperty)_properties[name];
						prop.Expr.Parameters = locals;
						object value = prop.Expr.Evaluate();
						args.Add(value);
						arg_types.Add(value.GetType());
					}
					// Next, get the type and correct method based on the args above
					Type type = Reflection.Utils.getType(assembly_name, type_name);
					if (type != null) {
					  method = Reflection.Utils.getMethodFromArgTypes(type, method_name, arg_types.ToArray());
					  // and call it, if it is valid:
					  object result = null;
					  if (method != null) {
					    try {
					      result = method.Invoke(type, args.ToArray());
					    } catch (Exception ex) {
					      this["Message"] = ex.Message;
					      this.State = BlockState.Error;
					      rr.Action = EngineAction.NoAction;
					      rr.Runner = null;
					    }
					    if (!(return_type is System.Void)) {
					      CVarNameProperty VarName = (CVarNameProperty)_properties["Variable"];
					      locals[VarName.Text] = result;
					    }
					  } else {
					    this["Message"] = "No matching method for these argument types";
					    this.State = BlockState.Error;
					    rr.Action = EngineAction.NoAction;
					    rr.Runner = null;
					  }
					} else {
					  this["Message"] = "Can't find assembly";
					  this.State = BlockState.Error;
					  rr.Action = EngineAction.NoAction;
					  rr.Runner = null;
					}
				} catch (Exception ex) {
				  this["Message"] = ex.Message;
				  this.State = BlockState.Error;
				  rr.Action = EngineAction.NoAction;
				  rr.Runner = null;
				}
				// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	
				// Go into a loop while block remains in an error state
				while (this.State == BlockState.Error) yield return rr;
	
				// If connected, replace this runner with the next runner to the stack.
				if (this.OutEdge.IsConnected) {
					rr.Action = EngineAction.Replace;
					rr.Runner = this.OutEdge.LinkedTo.Block.Runner(locals, builtins);
				} else {
					// If not connected, just remove this runner
					rr.Action = EngineAction.Remove;
					rr.Runner = null;
				}
				
				// Indicate that the block is no longer running
				this.State = BlockState.Idle;
				yield return rr;
			}			
			
	}
}
