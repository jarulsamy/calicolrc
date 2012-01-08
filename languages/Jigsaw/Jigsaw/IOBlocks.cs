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
    public class CInputOutput : CBlock
    {	// Input/Output block shape class
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CInputOutput(Double X, Double Y, Widgets.CBlockPalette palette = null) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y),
				new Diagram.CPoint(X + 175, Y + 20)}),
				palette )
		{
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkBlue;
			this.FillColor = Diagram.Colors.LightBlue;
			this.Sizable = false;
		}
		
		public CInputOutput(Double X, Double Y) : this(X, Y, null) {}
    }
	
	// -----------------------------------------------------------------------
    public class CIOPrint : CInputOutput
    {	// Print block shape class
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CIOPrint(Double X, Double Y, Widgets.CBlockPalette palette=null) 
			: base(X, Y, palette )
		{
			// Properties
			CExpressionProperty Expr = new CExpressionProperty("Expression", "X");
			Expr.PropertyChanged += OnPropertyChanged;
			_properties["Expression"] = Expr;
			this.OnPropertyChanged(null, null);
		}
		public CIOPrint(Double X, Double Y) : this(X, Y, null) {}
		
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs e)
		{	// Update text when property changes
			this.Text = String.Format("print({0})", this["Expression"]);
		}
		
		// - - - Generate and return Python statement - - - - - - - - - - -
		public override bool ToPython (StringBuilder o, int indent)
		{
			try
			{
				string sindent = new string (' ', 2*indent);
				o.AppendFormat("{0}print({1})\n", sindent, this["Expression"]);
				
				if (this.OutEdge.IsConnected) {
					CBlock b = this.OutEdge.LinkedTo.Block;
					b.ToPython(o, indent);
				}
				
			} catch (Exception ex){
				Console.WriteLine("{0} (in CIOPrint.ToPython)", ex.Message);
				return false;
			}
			
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override bool Compile(Microsoft.Scripting.Hosting.ScriptEngine engine, Jigsaw.Canvas cvs)
		{
			// Executing a print involves evaluting the given exression
			CExpressionProperty Expr = (CExpressionProperty)_properties["Expression"];
			try {
				Expr.Compile(engine);
			} catch (Exception ex) {
				Console.WriteLine ("Block {0} failed compilation: {1}", this.Name, ex.Message);
				return false;
			}
			return true;
		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override IEnumerator<RunnerResponse> Runner(ScriptScope scope, CallStack stack) 
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
			try {
				CExpressionProperty Expr = (CExpressionProperty)_properties["Expression"];
				object o = Expr.Evaluate(scope);
				string toPrint = o.ToString();
				Console.WriteLine(toPrint);
				//((InspectorWindow)builtins["Inspector"]).WriteLine(toPrint);
				//((InspectorWindow)scope.GetVariable("_inspector")).WriteLine(toPrint);
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
    public class CIOWriteToFile : CInputOutput
    {	// Write an item to a file path
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CIOWriteToFile(Double X, Double Y, Widgets.CBlockPalette palette=null) 
			: base(X, Y, palette )
		{
			// Properties
			CExpressionProperty Expr = new CExpressionProperty("Expression", "expr");
			CExpressionProperty Path = new CExpressionProperty("FilePath", "file");
			Expr.PropertyChanged += OnPropertyChanged;
			Path.PropertyChanged += OnPropertyChanged;
			_properties["Expression"] = Expr;
			_properties["FilePath"] = Path;
			this.OnPropertyChanged(null, null);
		}
		
		public CIOWriteToFile(Double X, Double Y) : this(X, Y, null) {}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs E)
		{	// Update text when property changes
			this.Text = String.Format("write {0} to {1}", this["Expression"], this["FilePath"]);
		}
	}
}
