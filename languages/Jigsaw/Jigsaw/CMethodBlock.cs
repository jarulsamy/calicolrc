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
			               bool isFactory) 
				: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
					new Diagram.CPoint(X, Y),
					new Diagram.CPoint(X + 175, Y + 20)}),
					isFactory )
		{
			setValues(assembly_name, type_name, method_name, names, types);
		}

		public override CBlock Clone(double X, double Y, bool cloneEdges) 
		{	// Clone this block. Optionally clone edges.
				CBlock clone = (CBlock)base.Clone(X, Y, cloneEdges);
				((CMethodBlock)clone).setValues(this.assembly_name, this.type_name, this.method_name, this.names, this.types);
				return clone;
		    }	

	    public void setValues(string assembly_name, 
			               		  string type_name, 
			               		  string method_name, 
						   		  List<string> names,
						   		  List<Type> types)

		{
				this.assembly_name = assembly_name;
				this.type_name = type_name;
				this.method_name = method_name;
				this.names = names;
				this.types = types;
				this.LineWidth = 2;
				this.LineColor = Diagram.Colors.DarkBlue;
				this.FillColor = Diagram.Colors.LightBlue;
				this.Sizable = false;
				string parameter_list = "";
				string block_text;
				_properties["Variable"] = new CVarNameProperty("Variable", String.Format("{0}{1}", method_name.ToUpper(), 1));
				if (names != null) {
					for (int n = 0; n < names.Count; n++) {
						_properties[names[n]] = new CExpressionProperty(names[n], "X"); // FIXME: get default
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
						if (method != null) {
							CVarNameProperty VarName = (CVarNameProperty)_properties["Variable"];
							locals[VarName.Text] = method.Invoke(type, args.ToArray());
						} else {
							Console.WriteLine("No matching method for these argument types");
							this.State = BlockState.Error;
							rr.Action = EngineAction.NoAction;
							rr.Runner = null;
						}
					} else {
						Console.WriteLine("Can't find assembly");
						this.State = BlockState.Error;
						rr.Action = EngineAction.NoAction;
						rr.Runner = null;
					}
				} catch (Exception ex) {
					Console.WriteLine(ex.Message);
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