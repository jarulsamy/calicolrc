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
				rr.Action = EngineAction.Pause;				// so that engine can stop
				//rr.Frame = null;
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
				rr.Action = EngineAction.Pause;				// so that engine can stop
				//rr.Frame = null;
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
    }
	
	// --- Generic Python statement block shape class --------------------------------
    public class CInlineComment : CBlock
    {
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CInlineComment(Double X, Double Y, Widgets.CBlockPalette palette = null) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y),
				new Diagram.CPoint(X + 175, Y + 20)	}),
				palette )
		{
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.Gray;
			this.FillColor = Diagram.Colors.LightYellow;;
			this.Sizable = false;
			
			// Properties
			CStringProperty comment = new CStringProperty("Comment", "Comment");
			comment.PropertyChanged += OnPropertyChanged;
			_properties["Comment"] = comment;
			this.OnPropertyChanged(null, null);
		}
		
		public CInlineComment(Double X, Double Y) : this(X, Y, null) {}
		
        // - - - Update text when property changes - - - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs e)
		{
			this.Text = String.Format("{0}", this["Comment"]);
		}
		
		// - - - Generate and return Python statement - - - - - - - - - - -
		public override bool ToPython (StringBuilder o, int indent)
		{
			try
			{
				string sindent = new string (' ', 2*indent);
				o.AppendFormat("{0}# {1}\n", sindent, this["Comment"]);
				
				if (this.OutEdge.IsConnected) {
					CBlock b = this.OutEdge.LinkedTo.Block;
					b.ToPython(o, indent);
				}
				
			} catch (Exception ex){
				Console.WriteLine("{0} (in CInlineComment.ToPython)", ex.Message);
				return false;
			}
			
			return true;
		}
	}
	
	// --- Generic Python statement block shape class --------------------------------
    public class CComment : CBlock
    {
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CComment(Double X, Double Y, Widgets.CBlockPalette palette = null) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y),
				new Diagram.CPoint(X + 175, Y + 40)	}),
				palette )
		{
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.Gray;
			this.FillColor = Diagram.Colors.LightYellow;;
			this.Sizable = true;
			
			// Properties
			CStringProperty comment = new CStringProperty("Comment", "Comment");
			comment.PropertyChanged += OnPropertyChanged;
			_properties["Comment"] = comment;
			this.OnPropertyChanged(null, null);
		}
		
		public CComment(Double X, Double Y) : this(X, Y, null) {}
		
        // - - - Update text when property changes - - - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs e)
		{
			this.Text = String.Format("{0}", this["Comment"]);
		}
		
		// - - - Return a list of all edges - - - - - - - - - - - - - - - -
		public override List<CEdge> Edges 
		{	// No edges on a comment
			get {
				return new List<CEdge>();
			}
		}
		
		// - - - Draw comment so it wraps within the block shape - - - - - - -
		protected override void DrawLabels(Cairo.Context g)
		{
			if (this.Text.Length > 0)
            {
	            double x = this.left;
	            double y = this.top;
	            double w = this.width;
	            //double h = this.height;
				
				double cx = x + 0.5*w;
				double cy = y + 0.5*20;

				//int layoutWidth, layoutHeight;
				
				g.Color = this.TextColor;

				Pango.Layout layout = Pango.CairoHelper.CreateLayout(g);
				Pango.FontDescription desc = Pango.FontDescription.FromString(
						   String.Format("{0} {1} {2}", this.fontFace, this.fontWeight, this.fontSize));
				layout.FontDescription = desc;
				layout.Alignment = Pango.Alignment.Left; //Center;
				//layout.Ellipsize = Pango.EllipsizeMode.End;
				layout.Width = (int)((w-10.0)*Pango.Scale.PangoScale);
				
				layout.SetText(text);
				g.MoveTo(x+10.0, y+3.0+_textYOffset);
				Pango.CairoHelper.ShowLayout(g, layout);
            }
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected override void SetPath(Cairo.Context g) 
		{	// Base block outline graphics path figure
			double x = this.left;
            double y = this.top;
            double w = this.width;
            double h = this.height;
			double r = 6.0;
			double hpi = 0.5*Math.PI;
			
			g.MoveTo( x, y+r );
			g.Arc(    x+r, y+r, r, Math.PI, -hpi );
			g.LineTo( x+w-r, y );
			g.Arc(    x+w-r, y+r, r, -hpi, 0.0 );
			g.LineTo( x+w, y+h-r );
			g.Arc(    x+w-r, y+h-r, r, 0.0, hpi);
			g.LineTo( x+r, y+h );
			g.Arc(    x+r, y+h-r, r, hpi, Math.PI );
			g.LineTo( x, y+r );
            g.ClosePath();
		}

    }
}
