using System;
using System.Collections;
using System.Collections.Generic;
using System.Xml;
using Cairo;

namespace Jigsaw
{
	// -----------------------------------------------------------------------
    public class CGfxWindow : CBlock
    {	// Block to create a new window
		// Stores Window object as varable named 'win' locals
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CGfxWindow(Double X, Double Y, bool isFactory) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y),
				new Diagram.CPoint(X + 175, Y + 20)}),
			isFactory) {
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkGreen;
			this.FillColor = Diagram.Colors.LightGreen;
			this.Sizable = false;
			
			// Properties
			CStringProperty _Title = new CStringProperty("Title", "Gfx1");
			CIntegerProperty _Width = new CIntegerProperty("Width", 300);
			CIntegerProperty _Height = new CIntegerProperty("Height", 300);
			
			_Title.PropertyChanged += OnPropertyChanged;
			_Width.PropertyChanged += OnPropertyChanged;
			_Height.PropertyChanged += OnPropertyChanged;
			
			_properties["Title"] = _Title;
			_properties["Width"] = _Width;
			_properties["Height"] = _Height;
			
			this.OnPropertyChanged(null, null);
		}
		
		public CGfxWindow(Double X, Double Y) : this(X, Y, false) { }
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs e)
		{	// Update text when property changes
			this.Text = String.Format("gfx window ({0})", this["Title"]);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override IEnumerator<RunnerResponse> 
			Runner(Dictionary<string, object> locals, Dictionary<string, object> builtins) 
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
				CIntegerProperty _Width = (CIntegerProperty)_properties["Width"];
				CIntegerProperty _Height = (CIntegerProperty)_properties["Height"];
				locals["win"] = Graphics.makeWindow(this["Title"], (int)_Width.Value, (int)_Height.Value);

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

	// -----------------------------------------------------------------------
    public class CGfxLine : CBlock
    {	// Block to create a new line and add it to the window

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CGfxLine(Double X, Double Y, bool isFactory) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y),
				new Diagram.CPoint(X + 175, Y + 20) }),
				isFactory )
			{
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkGreen;
			this.FillColor = Diagram.Colors.LightGreen;
			this.Sizable = false;
			
			// Properties
			CExpressionProperty _X1 = new CExpressionProperty("X1", "50");
			CExpressionProperty _Y1 = new CExpressionProperty("Y1", "50");
			CExpressionProperty _X2 = new CExpressionProperty("X2", "150");
			CExpressionProperty _Y2 = new CExpressionProperty("Y2", "150");
			
			_X1.PropertyChanged += OnPropertyChanged;
			_Y1.PropertyChanged += OnPropertyChanged;
			_X2.PropertyChanged += OnPropertyChanged;
			_Y2.PropertyChanged += OnPropertyChanged;
			
			_properties["X1"] = _X1;
			_properties["Y1"] = _Y1;
			_properties["X2"] = _X2;
			_properties["Y2"] = _Y2;
			
			// Go ahead and trigger updates
			this.OnPropertyChanged(null, null);
		}
		
		public CGfxLine(Double X, Double Y) : this(X, Y, false) { }
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs e)
		{	// Update text when property changes
			this.Text = String.Format("create line ({0},{1})-({2},{3})", this["X1"], this["Y1"], this["X2"], this["Y2"]);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override IEnumerator<RunnerResponse> 
			Runner(Dictionary<string, object> locals, Dictionary<string, object> builtins) 
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
				Graphics.WindowClass win = (Graphics.WindowClass)locals["win"];
				CExpressionProperty _X1 = (CExpressionProperty)_properties["X1"];
				CExpressionProperty _Y1 = (CExpressionProperty)_properties["Y1"];
				CExpressionProperty _X2 = (CExpressionProperty)_properties["X2"];
				CExpressionProperty _Y2 = (CExpressionProperty)_properties["Y2"];
				
				_X1.Expr.Parameters = locals;
				double dx1 = Convert.ToDouble( _X1.Expr.Evaluate() );
				
				_Y1.Expr.Parameters = locals;
				double dy1 = Convert.ToDouble( _Y1.Expr.Evaluate() );

				_X2.Expr.Parameters = locals;
				double dx2 = Convert.ToDouble( _X2.Expr.Evaluate() );

				_Y2.Expr.Parameters = locals;
				double dy2 = Convert.ToDouble( _Y2.Expr.Evaluate() );
				
				Graphics.Point pt1 = new Graphics.Point( dx1, dy1 );
				Graphics.Point pt2 = new Graphics.Point( dx2, dy2 );
				Graphics.Line ln = new Graphics.Line(pt1, pt2);
				ln.draw(win);

			} catch (Exception ex) {
				Console.WriteLine(ex.Message);
				this["Message"] = ex.Message;
				//MsgProp.Text = ex.Message;
				
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

	// -----------------------------------------------------------------------
    public class CGfxCircle : CBlock
    {	// Block to create a new line and add it to the window
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CGfxCircle(Double X, Double Y, bool isFactory) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y),
				new Diagram.CPoint(X + 175, Y + 20)}),
				isFactory ) 
		{
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkGreen;
			this.FillColor = Diagram.Colors.LightGreen;
			this.Sizable = false;
			
			// Properties
			CExpressionProperty _X1 = new CExpressionProperty("X", "50");
			CExpressionProperty _Y1 = new CExpressionProperty("Y", "50");
			CExpressionProperty _Diameter = new CExpressionProperty("Diameter", "50");
			
			_X1.PropertyChanged += OnPropertyChanged;
			_Y1.PropertyChanged += OnPropertyChanged;
			_Diameter.PropertyChanged += OnPropertyChanged;
			
			_properties["X"] = _X1;
			_properties["Y"] = _Y1;
			_properties["Diameter"] = _Diameter;
			
			this.OnPropertyChanged(null, null);
		}
		
		public CGfxCircle(Double X, Double Y) : this(X, Y, false) { }
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Update text when property changes
		public void OnPropertyChanged(object sender, EventArgs e){
			this.Text = String.Format("create circle ({0},{1}) {2}", this["X"], this["Y"], this["Diameter"]);
			//this.Text = String.Format("create circle ({0},{1}) {2}", _X1.Text, _Y1.Text, _Diameter.Text);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Execute a variable assignment
		public override IEnumerator<RunnerResponse> 
			Runner(Dictionary<string, object> locals, Dictionary<string, object> builtins) 
		{
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
				// Access window. This should have already been created with a previous block
				Graphics.WindowClass win = (Graphics.WindowClass)locals["win"];
				CExpressionProperty _X1 = (CExpressionProperty)_properties["X"];
				CExpressionProperty _Y1 = (CExpressionProperty)_properties["Y"];
				CExpressionProperty _Diameter = (CExpressionProperty)_properties["Diameter"];
				
				// Evaluate position and diameter
				_X1.Expr.Parameters = locals;
				double dx1 = Convert.ToDouble(_X1.Expr.Evaluate());
				_Y1.Expr.Parameters = locals;
				double dy1 = Convert.ToDouble(_Y1.Expr.Evaluate());
				_Diameter.Expr.Parameters = locals;
				int idiam = Convert.ToInt32(_Diameter.Expr.Evaluate());
				
				// Create Circle object
				Graphics.Point pt1 = new Graphics.Point( dx1, dy1 );
				Graphics.Circle c = new Graphics.Circle(pt1, idiam);
				
				// Draw circle
				c.draw(win);
				
			} catch (Exception ex) {
				Console.WriteLine(ex.Message);
				this["Message"] = ex.Message;
				//MsgProp.Text = ex.Message;
				
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
	
	// -----------------------------------------------------------------------
    public class CGfxText : CBlock
    {	// Block to create a new Text object and add it to the window

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CGfxText(Double X, Double Y, bool isFactory) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y),
				new Diagram.CPoint(X + 175, Y + 20)}),
				isFactory ) 
		{
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkGreen;
			this.FillColor = Diagram.Colors.LightGreen;
			this.Sizable = false;
			
			// Properties
			CExpressionProperty _X = new CExpressionProperty("X", "50");
			CExpressionProperty _Y = new CExpressionProperty("Y", "50");
			CExpressionProperty _Text = new CExpressionProperty("Text", "'Hello'");
			
			_X.PropertyChanged += OnPropertyChanged;
			_Y.PropertyChanged += OnPropertyChanged;
			_Text.PropertyChanged += OnPropertyChanged;
			
			_properties["X"] = _X;
			_properties["Y"] = _Y;
			_properties["Text"] = _Text;
			
			this.OnPropertyChanged(null, null);
		}
		
		public CGfxText(Double X, Double Y) : this(X, Y, false) { }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Update text when property changes
		public void OnPropertyChanged(object sender, EventArgs e){
			this.Text = String.Format("draw text ({0},{1}) {2}", this["X"], this["Y"], this["Text"]);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Execute a variable assignment
		public override IEnumerator<RunnerResponse> 
			Runner(Dictionary<string, object> locals, Dictionary<string, object> builtins) 
		{
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
			// Do the assignment
			// TODO: Allow access to global namespace

			try {
				// Access window. This should have already been created with a previous block
				Graphics.WindowClass win = (Graphics.WindowClass)locals["win"];
				CExpressionProperty _X = (CExpressionProperty)_properties["X"];
				CExpressionProperty _Y = (CExpressionProperty)_properties["Y"];
				CExpressionProperty _Text = (CExpressionProperty)_properties["Text"];
				
				// Evaluate position and diameter
				_X.Expr.Parameters = locals;
				double dx1 = Convert.ToDouble(_X.Expr.Evaluate());
				_Y.Expr.Parameters = locals;
				double dy1 = Convert.ToDouble(_Y.Expr.Evaluate());
				_Text.Expr.Parameters = locals;
				string txt = Convert.ToString(_Text.Expr.Evaluate());
				
				// Create Text object
				Graphics.Point pt1 = new Graphics.Point( dx1, dy1 );
				Graphics.Text t = new Graphics.Text(pt1, txt);
				
				// Draw text
				t.draw(win);
				
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
