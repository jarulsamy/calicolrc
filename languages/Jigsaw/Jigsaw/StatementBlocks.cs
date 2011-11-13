using System;
using System.Collections;
using System.Collections.Generic;
using System.Xml;
using Cairo;

namespace Jigsaw
{
	// -----------------------------------------------------------------------
    public class CAssignment : CBlock
    {	// Variable assignment block shape class
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CAssignment(Double X, Double Y, bool isFactory) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y),
				new Diagram.CPoint(X + 175, Y + 20)	}),
				isFactory ) 
		{
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkGreen;
			this.FillColor = Diagram.Colors.LightGreen;;
			this.Sizable = false;
			
			// Properties
			CVarNameProperty VarName = new CVarNameProperty("Variable", "X");
			CExpressionProperty RHS = new CExpressionProperty("Expression", "0");
			
			VarName.PropertyChanged += OnPropertyChanged;
			RHS.PropertyChanged += OnPropertyChanged;
			
			_properties["Variable"] = VarName;
			_properties["Expression"] = RHS;
			this.OnPropertyChanged(null, null);
		}
		
		public CAssignment(Double X, Double Y) : this(X, Y, false) { }
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs e)
		{	// Update text when property changes
			this.Text = String.Format("let {0} = {1}", this["Variable"], this["Expression"]);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override IEnumerator<RunnerResponse> 
			Runner(Expression.Scope locals, Dictionary<string, object> builtins) 
		{	// Execute a variable assignment
			
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
			// Do the assignment
			// TODO: Allow access to global namespace

			try {
				CVarNameProperty VarName = (CVarNameProperty)_properties["Variable"];
				CExpressionProperty RHS = (CExpressionProperty)_properties["Expression"];
				
				locals.Assignment(VarName.Text, RHS.Expr.ParsedExpression());

			} catch (Exception ex) {
				Console.WriteLine(ex.Message);
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
