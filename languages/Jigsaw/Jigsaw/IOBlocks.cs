using System;
using System.Collections;
using System.Collections.Generic;
using System.Xml;
using Cairo;

namespace Jigsaw
{
	// -----------------------------------------------------------------------
    public class CInputOutput : CBlock
    {	// Input/Output block shape class
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CInputOutput(Double X, Double Y, bool isFactory) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y),
				new Diagram.CPoint(X + 175, Y + 20)}),
				isFactory )
		{
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkBlue;
			this.FillColor = Diagram.Colors.LightBlue;
			this.Sizable = false;
		}
		
		public CInputOutput(Double X, Double Y) : this(X, Y, false) { }
    }
	
	// -----------------------------------------------------------------------
    public class CIOPrint : CInputOutput
    {	// Print block shape class
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CIOPrint(Double X, Double Y, bool isFactory) 
			: base(X, Y, isFactory )
		{
			// Properties
			CExpressionProperty Expr = new CExpressionProperty("Expression", "X");
			Expr.PropertyChanged += OnPropertyChanged;
			_properties["Expression"] = Expr;
			this.OnPropertyChanged(null, null);
		}
		
		public CIOPrint(Double X, Double Y) : this(X, Y, false) { }
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
				CExpressionProperty Expr = (CExpressionProperty)_properties["Expression"];
				Expr.Expr.Parameters = locals;
				object o = Expr.Expr.Evaluate();
				string toPrint = o.ToString();
				//Console.WriteLine(toPrint);
				((InspectorWindow)builtins["Inspector"]).WriteLine(toPrint);
				
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
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Update text when property changes
		public void OnPropertyChanged(object sender, EventArgs e){
			this.Text = String.Format("print {0}", this["Expression"]);
			//.Text = String.Format("print {0}", Expr.Text);
		}
	}

	// -----------------------------------------------------------------------
    public class CIOWriteToFile : CInputOutput
    {	// Write an item to a file path
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CIOWriteToFile(Double X, Double Y, bool isFactory) 
			: base(X, Y, isFactory )
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
		
		public CIOWriteToFile(Double X, Double Y) : this(X, Y, false) { }
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Update text when property changes
		public void OnPropertyChanged(object sender, EventArgs E){
			this.Text = String.Format("write {0} to {1}", this["Expression"], this["FilePath"]);
		}
	}
}
