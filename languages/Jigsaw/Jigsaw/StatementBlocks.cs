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
				new Diagram.CPoint(X + CBlock.BlockWidth, Y + 20)	}),
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
			this.State = RunningState.Running;				// Indicate that the block is running
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
				
				this.State = RunningState.Error;
				rr.Action = EngineAction.Error;
				rr.Frame = null;
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
    }
	
	// -----------------------------------------------------------------------
    public class CRandom : CBlock
    {	// Random number generator

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CRandom(Double X, Double Y, Widgets.CBlockPalette palette = null) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y),
				new Diagram.CPoint(X + CBlock.BlockWidth, Y + 20)	}),
				palette ) 
		{
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkGreen;
			this.FillColor = Diagram.Colors.LightGreen;;
			this.Sizable = false;
			
			// Properties
			CVarNameProperty VarName = new CVarNameProperty("Variable", "X");
			CExpressionProperty Min = new CExpressionProperty("Min", "0");
			CExpressionProperty Max = new CExpressionProperty("Max", "100");

			VarName.PropertyChanged += OnPropertyChanged;
			Min.PropertyChanged += OnPropertyChanged;
			Max.PropertyChanged += OnPropertyChanged;
			
			_properties["Variable"] = VarName;
			_properties["Min"] = Min;
			_properties["Max"] = Max;
			this.OnPropertyChanged(null, null);
		}
		public CRandom(Double X, Double Y) : this(X, Y, null) {}
		
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
			
			if (this.VariableName.Length > 0) {
				this.Text = String.Format("{0} = random({1}, {2})", this.VariableName, this["Min"], this["Max"]);
			} else {
				this.Text = String.Format("random({0}, {1})", this["Min"], this["Max"]);
			}
		}
		
		// - - - Generate and return Python statement - - - - -
		private string ToPython ()
		{
			string code = String.Format("Common.Utils.random({0}, {1})", this["Min"], this["Max"]);
			if (this.VariableName.Length > 0) 
				code = String.Format("{0} = Common.Utils.random({1}, {2})", this.VariableName, this["Min"], this["Max"]);
			
			return code;
		}
		
		public override bool ToPython (StringBuilder o, int indent)
		{
			try
			{
				string sindent = new string (' ', 2*indent);
				
				string code = this.ToPython ();
				o.AppendFormat("{0}{1}\n", sindent, code);
				
				if (this.OutEdge.IsConnected) {
					CBlock b = this.OutEdge.LinkedTo.Block;
					b.ToPython(o, indent);
				}
			
			} catch (Exception ex){
				Console.WriteLine("{0} (in CRandom.ToPython)", ex.Message);
				return false;
			}
			
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override bool Compile(Microsoft.Scripting.Hosting.ScriptEngine engine, Jigsaw.Canvas cvs)
		{
			// Executing a print involves evaluting the given exression
			CExpressionProperty Min = (CExpressionProperty)_properties["Min"];
			CExpressionProperty Max = (CExpressionProperty)_properties["Max"];
			try {
				Min.Compile(engine);
				Max.Compile(engine);
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
			this.State = RunningState.Running;				// Indicate that the block is running
			RunnerResponse rr = new RunnerResponse();		// Create and return initial response object
			yield return rr;
			if (this.BreakPoint == true) {					// Indicate if breakpoint is set on this block
				rr.Action = EngineAction.Pause;				// so that engine can stop
				yield return rr;
			}
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			// Do the assignment
			try {
				CExpressionProperty Min = (CExpressionProperty)_properties["Min"];
				CExpressionProperty Max = (CExpressionProperty)_properties["Max"];
				double min = Double.Parse (Min.Evaluate(scope).ToString ());
				double max = Double.Parse (Max.Evaluate(scope).ToString ());
				double val = Common.Utils.random(min, max);
				scope.SetVariable(VariableName, val);

			} catch (Exception ex) {
				Console.WriteLine(ex.Message);
				this["Message"] = ex.Message;
				
				this.State = RunningState.Error;
				rr.Action = EngineAction.Error;
				rr.Frame = null;
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
    }

	// -----------------------------------------------------------------------
    public class CStatement : CBlock
    {	// Generic Python statement block shape class
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CStatement(Double X, Double Y, Widgets.CBlockPalette palette = null) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y),
				new Diagram.CPoint(X + CBlock.BlockWidth, Y + 20)	}),
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
			this.State = RunningState.Running;				// Indicate that the block is running
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
				
				this.State = RunningState.Error;
				rr.Action = EngineAction.Error;
				rr.Frame = null;
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
    }
	
	// --- Generic Python statement block shape class --------------------------------
    public class CInlineComment : CBlock
    {
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CInlineComment(Double X, Double Y, Widgets.CBlockPalette palette = null) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y),
				new Diagram.CPoint(X + CBlock.BlockWidth, Y + 20)	}),
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
		private bool _suppressOnPropertyChanged = false;
		private string[] words = null;	// List of words in comment
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CComment(Double X, Double Y, Widgets.CBlockPalette palette = null) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y),
				new Diagram.CPoint(X + CBlock.BlockWidth, Y + 40)	}),
				palette )
		{
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.Gray;
			this.FillColor = Diagram.Colors.LightYellow;;
			this.Sizable = true;
			
			// Handle case when shape is transformed in some way
			this.Transformed += OnTransformed;

			// Properties
			CStringProperty comment = new CStringProperty("Comment", "Comment");
			CIntegerProperty width = new CIntegerProperty("Width", CBlock.BlockWidth);
			CIntegerProperty height = new CIntegerProperty("Height", 40);
			width.Visible = false;
			height.Visible = false;
			
			comment.PropertyChanged += OnPropertyChanged;
			width.PropertyChanged += OnPropertyChanged;
			height.PropertyChanged += OnPropertyChanged;
			
			_properties["Comment"] = comment;
			_properties["Width"] = width;
			_properties["Height"] = height;
			
			this.OnPropertyChanged(null, null);
		}
		
		public CComment(Double X, Double Y) : this(X, Y, null) {}
		
        // - - - Update text and size when property changes - - - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs e)
		{
			string comment = String.Format("{0}", this["Comment"]);
			this.Text = comment;
			
			if (_suppressOnPropertyChanged == true) return;
			_suppressOnPropertyChanged = true;

			CIntegerProperty width = (CIntegerProperty)_properties["Width"];
			CIntegerProperty height = (CIntegerProperty)_properties["Height"];
			this.Width = width.Value;
			this.Height = height.Value;
			
			_suppressOnPropertyChanged = false;
		}

		// - - - When block changes size, save in properties so saved to file - - - 
        public void OnTransformed (Diagram.CShape shp, Diagram.ShapeEventArgs e)
        {
			// The size has already changed. Just update properties. Don't re-resize.
			_suppressOnPropertyChanged = true;
			
			CIntegerProperty width = (CIntegerProperty)_properties["Width"];
			CIntegerProperty height = (CIntegerProperty)_properties["Height"];
			width.Value = (int)this.Width;
			height.Value = (int)this.Height;
			
			_suppressOnPropertyChanged = false;
        }
		
		// - - Override Text property to also split into words
        public override String Text
        {
            get { return this.text; }
            set {
				base.Text = value;
				words = value.Split (' ');		// Pre-split comment into words on spaces when changed
			}
        }

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override string this[string key]
		{	// Override property access to set shape width and height
			get { 
				return _properties[key].Text;  
			}
			set { 
				_properties[key].Text = value;
				
				switch (key) {
				case "Width":
					this.Width = Double.Parse (value);
					break;
				case "Height":
					this.Height = Double.Parse (value);
					break;
				}
			}
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
			if (this.Text.Length > 0 && words.Length > 0)
            {
	            double x = this.left;
	            double y = this.top;
	            double w = this.width;
	            double h = this.height;

				g.Save ();
				g.Rectangle(x, y, w-1.0, h-1.0);
				g.Clip ();
				g.Color = this.TextColor;
				g.SelectFontFace(this.fontFace, this.fontSlant, this.fontWeight);
				g.SetFontSize(this.fontSize);

				// Do word wrap. Slightly insane.
				
				// @@@ Note that in the following there are several hard-coded 
				// size tweaks that probably ought to be a function of the font size, 
				// not fixed values. Some day this should be fixed.
				double xx = x+10.0;
				double yy = y+3.0+_textYOffset;
				double ws = 5.0;						// Word spacing
				double ls = this.fontSize + 2.0;		// Line spacing
				int i = 0;								// Word counter
				string word = words[i];					// First word
				TextExtents te = g.TextExtents(word);	// Word extents

				for (;;) {
					// Render one word at next position
					g.MoveTo(xx, yy);
					g.ShowText(word);
					
					// Move to next x position and next word
					xx = xx + te.Width + ws;
					i++;
					if (i < words.Length) {
						word = words[i];
						
						// Get extents for next word
						te = g.TextExtents(word);

						// If the next word would be rendered outside the clip region, 
						// move to next line. Otherwise, continue.
						if ( xx + te.Width > x + w ) {
							xx = x + 10.0;
							yy = yy + ls;
						}
						
					} else {
						break;
					}
				}
				
				g.Restore();
				
//				g.Color = this.TextColor;
//				g.SelectFontFace(this.fontFace, this.fontSlant, this.fontWeight);
//				g.SetFontSize(this.fontSize);
//				//TextExtents te = g.TextExtents(text);
//				g.MoveTo(x+10.0, y+3.0+_textYOffset);
//				g.ShowText(text);
				
				
//				g.Color = this.TextColor;
//				Pango.Layout layout = Pango.CairoHelper.CreateLayout(g);
//				Pango.FontDescription desc = Pango.FontDescription.FromString(
//						   String.Format("{0} {1} {2}", this.fontFace, this.fontWeight, this.fontSize));
//				layout.FontDescription = desc;
//				layout.Alignment = Pango.Alignment.Left; //Center;
//				//layout.Ellipsize = Pango.EllipsizeMode.End;
//				layout.Width = (int)((w-10.0)*Pango.Scale.PangoScale);
//				
//				layout.SetText(text);
//				g.MoveTo(x+10.0, y+3.0+_textYOffset);
//				Pango.CairoHelper.ShowLayout(g, layout);
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
