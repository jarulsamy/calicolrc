using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using Cairo;
using Microsoft.Scripting.Hosting;

namespace Jigsaw
{
	// -----------------------------------------------------------------------
    public class CAssignment : CBlock
    {	// Variable assignment block shape class
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CAssignment(Double X, Double Y, Widgets.CBlockPalette palette = null) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y),
				new Diagram.CPoint(X + 175, Y + 20)	}),
				palette ) 
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
		public CAssignment(Double X, Double Y) : this(X, Y, null) {}
		
		// - - - Get return variable name - - - - - - - - - - - - - - -
		private String VariableName 
		{
			get 
			{
				return _properties["Variable"].Text.Trim();
			}
		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs e)
		{	// Update text when property changes
			this.Text = String.Format("{0} = {1}", this.VariableName, this["Expression"]);
		}
		
		// - - - Generate and return Python assignment - - - - -
		public override bool ToPython (StringBuilder o, int indent)
		{
			try
			{
				string sindent = new string (' ', 2*indent);
				o.AppendFormat("{0}{1} = {2}\n", sindent, this.VariableName, this["Expression"]);
				
				if (this.OutEdge.IsConnected) {
					CBlock b = this.OutEdge.LinkedTo.Block;
					b.ToPython(o, indent);
				}
				
			} catch (Exception ex){
				Console.WriteLine("{0} (in CAssignment.ToPython)", ex.Message);
				return false;
			}
			
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override bool Compile(Microsoft.Scripting.Hosting.ScriptEngine engine, Jigsaw.Canvas cvs)
		{
			// Executing a print involves evaluting the given exression
			CExpressionProperty RHS = (CExpressionProperty)_properties["Expression"];
			try {
				RHS.Compile(engine);
			} catch (Exception ex) {
				Console.WriteLine ("Block {0} failed compilation: {1}", this.Name, ex.Message);
				return false;
			}
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override IEnumerator<RunnerResponse> Runner(ScriptScope scope, CallStack stack) 
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
			try {
				CVarNameProperty VarName = (CVarNameProperty)_properties["Variable"];
				CExpressionProperty RHS = (CExpressionProperty)_properties["Expression"];
				
				scope.SetVariable(VarName.Text, RHS.Evaluate(scope));
				//scope.Assignment(VarName.Text, RHS.Expr.ParsedExpression());

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
    }

	// -----------------------------------------------------------------------
    public class CStatement : CBlock
    {	// Generic Python statement block shape class
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CStatement(Double X, Double Y, Widgets.CBlockPalette palette = null) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y),
				new Diagram.CPoint(X + 175, Y + 20)	}),
				palette ) 
		{
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkGreen;
			this.FillColor = Diagram.Colors.LightGreen;;
			this.Sizable = false;
			
			// Properties
			CStatementProperty Stat = new CStatementProperty("Statement", "pass");
			Stat.PropertyChanged += OnPropertyChanged;
			_properties["Statement"] = Stat;
			this.OnPropertyChanged(null, null);
			
		}
		public CStatement(Double X, Double Y) : this(X, Y, null) {}
		
        // - - - Update text when property changes - - - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs e)
		{
			this.Text = String.Format("{0}", this["Statement"]);
		}
		
		// - - - Generate and return Python statement - - - - - - - - - - -
		public override bool ToPython (StringBuilder o, int indent)
		{
			try
			{
				string sindent = new string (' ', 2*indent);
				o.AppendFormat("{0}{1}\n", sindent, this["Statement"]);
				
				if (this.OutEdge.IsConnected) {
					CBlock b = this.OutEdge.LinkedTo.Block;
					b.ToPython(o, indent);
				}
				
			} catch (Exception ex){
				Console.WriteLine("{0} (in CStatement.ToPython)", ex.Message);
				return false;
			}
			
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override bool Compile(Microsoft.Scripting.Hosting.ScriptEngine engine, Jigsaw.Canvas cvs)
		{
			// Executing a print involves evaluting the given exression
			CStatementProperty Stat = (CStatementProperty)_properties["Statement"];
			try {
				Stat.Compile(engine);
			} catch (Exception ex) {
				Console.WriteLine ("Block {0} failed compilation: {1}", this.Name, ex.Message);
				return false;
			}
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override IEnumerator<RunnerResponse> Runner(ScriptScope scope, CallStack stack) 
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
    }
}
