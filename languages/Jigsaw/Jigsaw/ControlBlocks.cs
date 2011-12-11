using System;
using System.Collections;
using System.Collections.Generic;
using System.Xml;
using Cairo;

namespace Jigsaw
{
	// -----------------------------------------------------------------------
    public class CControlStart : CBlock
    {	// Control start block shape class

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CControlStart(Double X, Double Y, bool isFactory) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y), 
				new Diagram.CPoint(X + 175, Y + 30) }),
				isFactory) 
		{
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkGoldenrod;
			this.FillColor = Diagram.Colors.PaleGoldenrod;
			this.Sizable = false;
			this.Text = "when program starts";
			textYOffset = 10;							// Block text offset
		}
		
		public CControlStart(Double X, Double Y) : this(X, Y, false) { }
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override List<CEdge> Edges 
		{	// Return a list of all edges
			// Control start blocks only have an output edge
			get {
				return new List<CEdge>() { this.OutEdge };
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected override void SetPath(Cairo.Context g) 
		{
			double x = this.left;
            double y = this.top;
			
			// If absolute positioning, convert x and y back to screen coordinates
			//if (this.Dock == Diagram.DockSide.Left) g.InverseTransformPoint(ref x, ref y);
			
            double w = this.width;
            double h = this.height;
			double r = 6.0;
			double hpi = 0.5*Math.PI;
			
			g.MoveTo( x, y+10);
			g.Arc(    x+50, y+95, 100, -0.665*Math.PI, -0.324*Math.PI);
			g.LineTo( x+w-r, y+10);
			g.Arc(    x+w-r, y+10+r, r, -hpi, 0.0 );
			g.LineTo( x+w, y+h-r );
			g.Arc(    x+w-r, y+h-r, r, 0.0, hpi);
			g.LineTo( x+27, y+h );
			g.LineTo( x+24, y+h+4 );
			g.LineTo( x+14, y+h+4 );
			g.LineTo( x+11, y+h );
			g.LineTo( x+r, y+h );
			g.Arc(    x+r, y+h-r, r, hpi, Math.PI );
			g.LineTo( x, y+10 );
            g.ClosePath();
		}
    }

	// -----------------------------------------------------------------------
    public class CControlEnd : CBlock
    {	// Control end block shape class
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CControlEnd(Double X, Double Y, bool isFactory) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y), 
				new Diagram.CPoint(X + 175, Y + 20)}),
				isFactory) 
		{
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkGoldenrod;
			this.FillColor = Diagram.Colors.PaleGoldenrod;
			this.Sizable = false;
			this.Text = "stop script";
		}
		
		public CControlEnd(Double X, Double Y) : this(X, Y, false) { }
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override List<CEdge> Edges 
		{	// Return a list of all edges
			// Control end blocks only have an input edge
			get {
				return new List<CEdge>() { this.InEdge };
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected override void SetPath(Cairo.Context g) 
		{
			double x = this.left;
            double y = this.top;

			// If absolute positioning, convert x and y back to screen coordinates
			//if (this.Dock == Diagram.DockSide.Left) g.InverseTransformPoint(ref x, ref y);
			
            double w = this.width;
            double h = this.height;
			double r = 6.0;
			double hpi = 0.5*Math.PI;
			
			g.MoveTo( x, y+r );
			g.Arc(    x+r, y+r, r, Math.PI, -hpi );
			g.LineTo( x+11, y );
			g.LineTo( x+14, y+4 );
			g.LineTo( x+24, y+4 );
			g.LineTo( x+27, y );
			g.LineTo( x+w-r, y );
			g.Arc(    x+w-r, y+r, r, -hpi, 0.0 );
			g.LineTo( x+w, y+h-r );
			g.Arc(    x+w-r, y+h-r, r, 0.0, hpi);
			g.LineTo( x+11, y+h );
			g.LineTo( x+r, y+h );
			g.Arc(    x+r, y+h-r, r, hpi, Math.PI );
			g.LineTo( x, y+r );
            g.ClosePath();
		}
    }

	// -----------------------------------------------------------------------
    public class CControlRepeat : CBlock
    {	// Repeat control block shape class
		
		// Internal edge
		public CEdge LoopEdge = null;
		
		// Properties
		//public CExpressionProperty Repetitions = null;
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CControlRepeat(Double X, Double Y, bool isFactory) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] {
				new Diagram.CPoint(X, Y), 
				new Diagram.CPoint(X + 175, Y + 50) }),
				isFactory) 
		{
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkGoldenrod;
			this.FillColor = Diagram.Colors.PaleGoldenrod;
			this.Sizable = false;
			
			// Create inner edge to connect loop stack
			double offsetX = 0.5*this.Width + 10.0;
			LoopEdge = new CEdge(this, "Loop", EdgeType.Out, null, offsetX, 20.0, 20.0, 20.0, this.Width-20.0);
			
			// Properties
			CExpressionProperty Repetitions = new CExpressionProperty("Repetitions", "3");
			Repetitions.PropertyChanged += OnPropertyChanged;
			_properties["Repetitions"] = Repetitions;
			this.OnPropertyChanged(null, null);
		}
		
		public CControlRepeat(Double X, Double Y) : this(X, Y, false) { }
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Returns a list of all block properties
//		public override List<CProperty> Properties 
//		{
//			get {
//				List<CProperty> props = base.Properties;
//				props.Add(this.Repetitions);
//				return props;
//			}
//		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override Diagram.CShape HitShape(Diagram.CPoint pt, Diagram.Canvas cvs)
        {	// If this block is hit, return a self reference.
        	
			if (!this.visible) return null;
			
			double X = pt.X;
			double Y = pt.Y;
			double Ymin = this.Top;
			double Xmin = this.Left;
			double Ymax = Ymin + this.Height;
			double Xmax = Xmin + this.Width;
			
			// If point in outer bounding box...
			if (X >= Xmin && X <= Xmax && Y >= Ymin && Y <= Ymax)
			{	// ...and also in inner bounding box
				if (X >= (Xmin + 20) && Y >= (Ymin + 20) && Y <= (Ymax - 20))
				{	// ...then not hit
					return null;
				}
				else
				{	// Otherwise, hit
					return this;
				}
			}
			else
			{	// Not hit if outside outer bounding box
				return null;
			}
        }
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		internal override bool SetProperty(string name, string val) {
//			string lname = name.ToLower();
//			switch(lname) {
//			case "repetitions":
//				Repetitions.Text = val;
//				break;
//			default:
//				return false;
//			}
//			return true;
//		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		internal override CEdge GetEdgeByName(string name)
		{
			// First try base class behavior.
			// If edge not found, look for custom edges.
			string tname = name.ToLower();
			
			CEdge e = base.GetEdgeByName(tname);
			if (e == null) {
				if (tname == "loop") {
					e = LoopEdge;
				}
			}
			return e;
		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Write custom tags for this block
//        protected override void WriteXmlTags(XmlWriter w)
//        {
//			base.WriteXmlTags(w);
//			this.WriteXmlProperty(w, "Repetitions", Repetitions.Text);
//        }
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Update text when property changes
		public void OnPropertyChanged(object sender, EventArgs e){
			this.Text = String.Format("repeat {0} times", this["Repetitions"]);
			//this.Text = String.Format("repeat {0} times", Repetitions.Text);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Execute a simple repetition
		public override IEnumerator<RunnerResponse> 
			Runner(Expression.Scope locals, Dictionary<string, object> builtins) 
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
			// Do the repetition
			int count = 1;
			int maxreps = 0;

			// Start by getting the number of repetitions as an integer
			// TODO: Allow access to global namespace
			try {
				CExpressionProperty Repetitions = (CExpressionProperty)_properties["Repetitions"];
				object oreps = Repetitions.Expr.Evaluate(locals);
				maxreps = (int)oreps;

			} catch (Exception ex) {
				this["Message"] = ex.Message;
				//MsgProp.Text = ex.Message;
				
				this.State = BlockState.Error;
				rr.Action = EngineAction.NoAction;
				rr.Runner = null;
			}
			// Go into a loop while block remains in an error state
			while (this.State == BlockState.Error) yield return rr;
			
			// Execute the repetition
			while ( count <= maxreps )
			{
				// Next perform one iteration of the enclosed stack
				if (this.LoopEdge.IsConnected) {
					rr.Action = EngineAction.Add;
					rr.Runner = this.LoopEdge.LinkedTo.Block.Runner(locals, builtins);
					yield return rr;
				}
				
				// At this point the body of the loop has completed
				// Increment counter
				count++;
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
		public override void RepositionBlocks(CEdge entryEdge)
		{	// Reposition this block wrt the entry edge
			
			// If the LoopEdge is connected, prior to repositiion the block wrt to
			// the rest of the diagram, calculate the LoopEdge stack and resize this block
			// This way, the remaining repositioning of embedded blocks fits nicely.
			
			if (this.LoopEdge.IsConnected) {
				CEdge linkedEdge = this.LoopEdge.LinkedTo;
				CBlock linkedBlock = linkedEdge.Block;
				linkedBlock.RepositionBlocks(linkedEdge);
				this.Height = linkedBlock.StackHeight + 40.0;
				
			} else {
				this.Height = 50.0;
			}
			
			base.RepositionBlocks(entryEdge);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override List<CEdge> Edges 
		{	// Return a list of all edges
			get {
				List<CEdge> prts = base.Edges;
				prts.Add(this.LoopEdge);			// Add special LoopEdge to the standard edges
				return prts;
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected override void SetPath(Cairo.Context g) 
		{
			double x = this.left;
            double y = this.top;
			
			// If absolute positioning, convert x and y back to screen coordinates
			//if (this.Dock == Diagram.DockSide.Left) g.InverseTransformPoint(ref x, ref y);
			
            double w = this.width;
            double h = this.height;
			double r = 6.0;
			double hpi = 0.5*Math.PI;
			
			g.MoveTo( x, y+r );
			g.Arc(    x+r, y+r, r, Math.PI, -hpi );
			g.LineTo( x+11, y );
			g.LineTo( x+14, y+4 );
			g.LineTo( x+24, y+4 );
			g.LineTo( x+27, y );
			g.LineTo( x+w-r, y );
			g.Arc(    x+w-r, y+r, r, -hpi, 0.0 );
			g.LineTo( x+w, y+20-r );
			g.Arc(    x+w-r, y+20-r, r, 0.0, hpi );
			g.LineTo( x+27+20, y+20 );
			g.LineTo( x+24+20, y+20+4 );
			g.LineTo( x+14+20, y+20+4 );
			g.LineTo( x+11+20, y+20 );
			g.LineTo( x+20+r, y+20 );
			g.ArcNegative(    x+20+r, y+20+r, r, -hpi, Math.PI );
			g.LineTo( x+20, y+h-20-r );
			g.ArcNegative(    x+20+r, y+h-20-r, r, Math.PI, hpi);
			g.LineTo( x+11+20, y+h-20);
			g.LineTo( x+14+20, y+h-20+4);
			g.LineTo( x+24+20, y+h-20+4);
			g.LineTo( x+27+20, y+h-20);
			g.LineTo( x+w-r, y+h-20 );
			g.Arc(    x+w-r, y+h-20+r, r, -hpi, 0.0);
			g.LineTo( x+w, y+h-r );
			g.Arc(    x+w-r, y+h-r, r, 0.0, hpi);
			g.LineTo( x+27, y+h );
			g.LineTo( x+24, y+h+4 );
			g.LineTo( x+14, y+h+4 );
			g.LineTo( x+11, y+h );
			g.LineTo( x+r, y+h );
			g.Arc(    x+r, y+h-r, r, hpi, Math.PI );
			g.LineTo( x, y+r );
            g.ClosePath();
		}		
    }

	// -----------------------------------------------------------------------
    public class CControlIf : CBlock
    {	// Execute inner block if conditional evaluates to true
		
		// Internal edge
		public CEdge IfEdge = null;
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CControlIf(Double X, Double Y, bool isFactory) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] {
				new Diagram.CPoint(X, Y), 
				new Diagram.CPoint(X + 175, Y + 50)}),
				isFactory) 
		{
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkGoldenrod;
			this.FillColor = Diagram.Colors.PaleGoldenrod;
			this.Sizable = false;
			
			// Create inner edge to connect if-block stack
			double offsetX = 0.5*this.Width + 10.0;
			IfEdge = new CEdge(this, "If", EdgeType.Out, null, offsetX, 20.0, 20.0, 20.0, this.Width-20.0);
			
			// Properties
			CExpressionProperty IfTest = new CExpressionProperty("IfTest", "True");
			IfTest.PropertyChanged += OnPropertyChanged;
			
			_properties["IfTest"] = IfTest;
			
			this.OnPropertyChanged(null, null);
		}
		
		public CControlIf(Double X, Double Y) : this(X, Y, false) { }
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Returns a list of all block properties
//		public override List<CProperty> Properties 
//		{
//			get {
//				List<CProperty> props = base.Properties;
//				props.Add(this.IfTest);
//				return props;
//			}
//		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override Diagram.CShape HitShape(Diagram.CPoint pt, Diagram.Canvas cvs)
        {	// If this block is hit, return a self reference.
        	
			if (!this.visible) return null;
			
			double X = pt.X;
			double Y = pt.Y;
			double Ymin = this.Top;
			double Xmin = this.Left;
			double Ymax = Ymin + this.Height;
			double Xmax = Xmin + this.Width;
			
			// If docked, undo translate
//			if (this.Dock == Diagram.DockSide.Left) {
//				X = X + cvs.offsetX;
//				Y = Y + cvs.offsetY;
//			}
			
			// If point in outer bounding box...
			if (X >= Xmin && X <= Xmax && Y >= Ymin && Y <= Ymax)
			{	// ...and also in inner bounding box
				if (X >= (Xmin + 20) && Y >= (Ymin + 20) && Y <= (Ymax - 20))
				{	// ...then not hit
					return null;
				}
				else
				{	// Otherwise, hit
					return this;
				}
			}
			else
			{	// Not hit if outside outer bounding box
				return null;
			}
        }
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		internal override bool SetProperty(string name, string val) {
//			string lname = name.ToLower();
//			switch(lname) {
//			case "if":
//				IfTest.Text = val;
//				break;
//			default:
//				return false;
//			}
//			return true;
//		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		internal override CEdge GetEdgeByName(string name)
		{
			// First try base class behavior.
			// If edge not found, look for custom edges.
			string tname = name.ToLower();
			
			CEdge e = base.GetEdgeByName(tname);
			if (e == null) {
				if (tname == "if") {
					e = IfEdge;
				}
			}
			return e;
		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Write custom tags for this block
//        protected override void WriteXmlTags(XmlWriter w)
//        {
//			base.WriteXmlTags(w);
//			this.WriteXmlProperty(w, "if", IfTest.Text);
//        }
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs e)
		{	// Update text when property changes
			this.Text = String.Format("if {0}", this["IfTest"]);
			//this.Text = String.Format("if {0}", IfTest.Text);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Execute a simple repetition
		public override IEnumerator<RunnerResponse> 
			Runner(Expression.Scope locals, Dictionary<string, object> builtins) 
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
			
			// Do the if-test
			bool doIf = false;

			// Start by evaluating the if expression
			// TODO: Allow access to global namespace
			try {
				CExpressionProperty IfTest = (CExpressionProperty)_properties["IfTest"];
				object otest = IfTest.Expr.Evaluate(locals);
				doIf = (bool)otest;

			} catch (Exception ex) {
				this["Message"] = ex.Message;
				//MsgProp.Text = ex.Message;
				
				this.State = BlockState.Error;
				rr.Action = EngineAction.NoAction;
				rr.Runner = null;
			}
			// Go into a loop while block remains in an error state
			while (this.State == BlockState.Error) yield return rr;
			
			// Execute the repetition
			if ( doIf )
			{	// Execute enclosed stack
				if (this.IfEdge.IsConnected) {
					rr.Action = EngineAction.Add;
					rr.Runner = this.IfEdge.LinkedTo.Block.Runner(locals, builtins);
					yield return rr;
				}
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
		public override void RepositionBlocks(CEdge entryEdge)
		{	// Reposition this block wrt the entry edge
			
			// If the IfEdge is connected, prior to repositiion the block wrt to
			// the rest of the diagram, calculate the IfEdge stack and resize this block
			// This way, the remaining repositioning of embedded blocks fits nicely.
			
			if (this.IfEdge.IsConnected) {
				CEdge linkedEdge = this.IfEdge.LinkedTo;
				CBlock linkedBlock = linkedEdge.Block;
				linkedBlock.RepositionBlocks(linkedEdge);
				this.Height = linkedBlock.StackHeight + 40.0;
				
			} else {
				this.Height = 50.0;
			}
			
			base.RepositionBlocks(entryEdge);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override List<CEdge> Edges 
		{	// Return a list of all edges
			get {
				List<CEdge> prts = base.Edges;
				prts.Add(this.IfEdge);			// Add special IfEdge to the standard edges
				return prts;
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected override void SetPath(Cairo.Context g) 
		{
			double x = this.left;
            double y = this.top;

			// If absolute positioning, convert x and y back to screen coordinates
			//if (this.Dock == Diagram.DockSide.Left) g.InverseTransformPoint(ref x, ref y);

            double w = this.width;
            double h = this.height;
			double r = 6.0;
			double hpi = 0.5*Math.PI;
			
			g.MoveTo( x, y+r );
			g.Arc(    x+r, y+r, r, Math.PI, -hpi );
			g.LineTo( x+11, y );
			g.LineTo( x+14, y+4 );
			g.LineTo( x+24, y+4 );
			g.LineTo( x+27, y );
			g.LineTo( x+w-r, y );
			g.Arc(    x+w-r, y+r, r, -hpi, 0.0 );
			g.LineTo( x+w, y+20-r );
			g.Arc(    x+w-r, y+20-r, r, 0.0, hpi );
			g.LineTo( x+27+20, y+20 );
			g.LineTo( x+24+20, y+20+4 );
			g.LineTo( x+14+20, y+20+4 );
			g.LineTo( x+11+20, y+20 );
			g.LineTo( x+20+r, y+20 );
			g.ArcNegative( x+20+r, y+20+r, r, -hpi, Math.PI );
			g.LineTo( x+20, y+h-20-r );
			g.ArcNegative( x+20+r, y+h-20-r, r, Math.PI, hpi);
			g.LineTo( x+11+20, y+h-20);
			g.LineTo( x+14+20, y+h-20+4);
			g.LineTo( x+24+20, y+h-20+4);
			g.LineTo( x+27+20, y+h-20);
			g.LineTo( x+w-r, y+h-20 );
			g.Arc(    x+w-r, y+h-20+r, r, -hpi, 0.0);
			g.LineTo( x+w, y+h-r );
			g.Arc(    x+w-r, y+h-r, r, 0.0, hpi);
			g.LineTo( x+27, y+h );
			g.LineTo( x+24, y+h+4 );
			g.LineTo( x+14, y+h+4 );
			g.LineTo( x+11, y+h );
			g.LineTo( x+r, y+h );
			g.Arc(    x+r, y+h-r, r, hpi, Math.PI );
			g.LineTo( x, y+r );
            g.ClosePath();
		}
    }

	// -----------------------------------------------------------------------
    public class CControlIfElse : CBlock
    {
		// Internal edges
		public CEdge IfEdge = null;
		public CEdge ElseEdge = null;
		
		// Internal stack heights
		private double _ibh = 10.0;		// If block height
		private double _ebh = 10.0;		// Else block height
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CControlIfElse(Double X, Double Y, bool isFactory) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] {
				new Diagram.CPoint(X, Y), 
				new Diagram.CPoint(X + 175, Y + 80)}),
				isFactory)
		{
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkGoldenrod;
			this.FillColor = Diagram.Colors.PaleGoldenrod;
			this.Sizable = false;
			
			// Create inner edge to connect if-block stack and else-block stack
			double offsetX = 0.5*this.Width + 10.0;
			IfEdge = new CEdge(this, "If", EdgeType.Out, null, offsetX, 20.0, 20.0, 20.0, this.Width-20.0);
			ElseEdge = new CEdge(this, "Else", EdgeType.Out, null, offsetX, 50.0, 20.0, 50.0, this.Width-20.0);
			
			// Properties
			CExpressionProperty IfTest = new CExpressionProperty("IfTest", "True");
			IfTest.PropertyChanged += OnPropertyChanged;
			
			_properties["IfTest"] = IfTest;
			
			this.OnPropertyChanged(null, null);
		}
		
		public CControlIfElse(Double X, Double Y) : this(X, Y, false) { }

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Clone an if-else block
		public override CBlock Clone(double X, double Y, bool cloneEdges) 
		{	// Clone this block. Optionally clone edges.
			// This special version is necessary due to two internal block stacks.
			
			CControlIfElse clone = (CControlIfElse)base.Clone(X, Y, cloneEdges);
			
			// Set internal heights of internal block stacks
			clone._ibh = this._ibh;
			clone._ebh = this._ebh;
			
			return (CBlock)clone;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override Diagram.CShape HitShape(Diagram.CPoint pt, Diagram.Canvas cvs)
        {	// If this block is hit, return a self reference.
        	
			if (!this.visible) return null;
			
			double X = pt.X;
			double Y = pt.Y;
			double Ymin = this.Top;
			double Xmin = this.Left;
			double Ymax = Ymin + this.Height;
			double Xmax = Xmin + this.Width;
			
			// If point in outer bounding box...
			if (X >= Xmin && X <= Xmax && Y >= Ymin && Y <= Ymax)
			{	// ...and if also in inner if-block stack
				if (X >= (Xmin + 20) && Y >= (Ymin + 20) && Y <= (Ymin + 20 + _ibh))
				{	// ...then not hit
					return null;
				} // ... and if also in inner else-block stack
				else if (X >= (Xmin + 20) && Y >= (Ymin + 40 + _ibh) && Y <= (Ymax - 20))
				{	// ...then not hit
					return null;
				}
				else
				{	// Otherwise, hit
					return this;
				}
			}
			else
			{	// Not hit if outside outer bounding box
				return null;
			}
        }
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		internal override CEdge GetEdgeByName(string name)
		{
			// First try base class behavior.
			// If edge not found, look for custom edges.
			string tname = name.ToLower();
			
			CEdge e = base.GetEdgeByName(tname);
			if (e == null) {
				switch (tname) {
				case "if":
					e = IfEdge;
					break;
				case "else":
					e = ElseEdge;
					break;
				}
			}
			return e;
		}

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Update text when property changes
		public void OnPropertyChanged(object sender, EventArgs e){
			this.Text = String.Format("if ({0})", this["IfTest"]);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Execute a simple repetition
		public override IEnumerator<RunnerResponse> 
			Runner(Expression.Scope locals, Dictionary<string, object> builtins) 
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
			
			// Do the if-else-test
			bool doIf = false;

			// Start by evaluating the if expression
			// TODO: Allow access to global namespace
			try {
				CExpressionProperty IfTest = (CExpressionProperty)_properties["IfTest"];
				object otest = IfTest.Expr.Evaluate(locals);
				doIf = (bool)otest;

			} catch (Exception ex) {
				this["Message"] = ex.Message;
				//MsgProp.Text = ex.Message;
				
				this.State = BlockState.Error;
				rr.Action = EngineAction.NoAction;
				rr.Runner = null;
			}
			// Go into a loop while block remains in an error state
			while (this.State == BlockState.Error) yield return rr;
			
			// Execute the if-part or else-part
			if ( doIf )
			{	// Execute enclosed stack
				if (this.IfEdge.IsConnected) {
					rr.Action = EngineAction.Add;
					rr.Runner = this.IfEdge.LinkedTo.Block.Runner(locals, builtins);
					yield return rr;
				}
			} else {
				if (this.ElseEdge.IsConnected) {
					rr.Action = EngineAction.Add;
					rr.Runner = this.ElseEdge.LinkedTo.Block.Runner(locals, builtins);
					yield return rr;
				}
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
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void RepositionBlocks(CEdge entryEdge)
		{	// Reposition this block and related components wrt the entry edge
			
			// If the IfEdge is connected, prior to repositiion the block wrt to
			// the rest of the diagram, calculate the IfEdge stack and resize this block
			// This way, the remaining repositioning of embedded blocks fits nicely.
			
			CEdge linkedEdge;
			CBlock linkedBlock;
			
			// Check if part
			if (this.IfEdge.IsConnected) {
				linkedEdge = this.IfEdge.LinkedTo;
				linkedBlock = linkedEdge.Block;
				linkedBlock.RepositionBlocks(linkedEdge);
				_ibh = (float)linkedBlock.StackHeight; 
				
			} else {
				_ibh = 10.0F;		// Height of non-connected slot
			}
			
			// Check else part
			if (this.ElseEdge.IsConnected) {
				linkedEdge = this.ElseEdge.LinkedTo;
				linkedBlock = linkedEdge.Block;
				linkedBlock.RepositionBlocks(linkedEdge);
				_ebh = (float)linkedBlock.StackHeight;
				
			} else {
				_ebh = 10.0F;		// Height of non-connected slot
			}
			
			// Reposition Else edge
			ElseEdge._offsetY = 40.0 + _ibh;
			ElseEdge._azY = 40.0 + _ibh;
			
			// Reset height
			this.Height = 60.0 + _ibh + _ebh;
			
			base.RepositionBlocks(entryEdge);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override List<CEdge> Edges 
		{	// Return a list of all edges
			get {
				List<CEdge> prts = base.Edges;
				prts.Add(this.IfEdge);
				prts.Add(this.ElseEdge);
				return prts;
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected override void DrawLabels(Cairo.Context g) {

            double x = this.left;
            double y = this.top;
			
			// If absolute positioning, convert x and y back to screen coordinates
			//if (this.Dock == Diagram.DockSide.Left) g.InverseTransformPoint(ref x, ref y);
			
            double w = this.width;
            //double h = this.height;
			
			double cx = x + 0.5*w;
			double cy = y + 0.5*20;
			g.Color = this.TextColor;

			double teHeight, teWidth;
			int layoutWidth, layoutHeight;

			Pango.Layout layout = Pango.CairoHelper.CreateLayout(g);
			Pango.FontDescription desc = Pango.FontDescription.FromString(
					   String.Format("{0} {1} {2}", this.fontFace, this.fontWeight, this.fontSize));
			layout.FontDescription = desc;
			layout.Alignment = Pango.Alignment.Center;

			if (this.Text.Length > 0)
            {
				layout.SetText(text);
				layout.GetSize(out layoutWidth, out layoutHeight);
				teHeight = (double)layoutHeight / Pango.Scale.PangoScale; 
				teWidth = (double)layoutWidth / Pango.Scale.PangoScale;
				g.MoveTo(cx - 0.5*teWidth, cy - 0.5*teHeight + textYOffset); 
				Pango.CairoHelper.ShowLayout(g, layout);
            }
			
			layout.SetText("else");
			layout.GetSize(out layoutWidth, out layoutHeight);
			teHeight = (double)layoutHeight / Pango.Scale.PangoScale; 
			teWidth = (double)layoutWidth / Pango.Scale.PangoScale;
			g.MoveTo(cx - 0.5*teWidth, cy - 0.5*teHeight + textYOffset+20+_ibh); 
			Pango.CairoHelper.ShowLayout(g, layout);
			
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		protected override GraphicsPath Figure() 
		protected override void SetPath(Cairo.Context g) 
		{
			double x = this.left;
            double y = this.top;
			
			// If absolute positioning, convert x and y back to screen coordinates
			//if (this.Dock == Diagram.DockSide.Left) g.InverseTransformPoint(ref x, ref y);
			
            double w = this.width;
            double h = this.height;

			double r = 6.0;
			double hpi = 0.5*Math.PI;
			
			// If-arm top left corner
			g.MoveTo( x, y+r );
			g.Arc(    x+r, y+r, r, Math.PI, -hpi );
			
			// If-arm in-edge
			g.LineTo( x+11,  y );
			g.LineTo( x+14,  y+4 );
			g.LineTo( x+24,  y+4 );
			g.LineTo( x+27,  y );
			g.LineTo( x+w-r, y );
			
			// If-arm right side
			g.Arc(    x+w-r, y+r, r, -hpi, 0.0 );
			g.LineTo( x+w,   y+20-r );
			g.Arc(    x+w-r, y+20-r, r, 0.0, hpi );
			
			// If-arm out edge
			g.LineTo( x+27+20, y+20 );	
			g.LineTo( x+24+20, y+20+4 );
			g.LineTo( x+14+20, y+20+4 );
			g.LineTo( x+11+20, y+20 );
			g.LineTo( x+20+r,  y+20 );
			
			// If-block left inner side
			g.ArcNegative( x+20+r, y+20+r, r, -hpi, Math.PI );
			g.LineTo(      x+20,   y+20+_ibh-r );
			g.ArcNegative( x+20+r, y+20+_ibh-r, r, Math.PI, hpi);
			
			// Else-arm in side
			g.LineTo( x+11+20, y+20+_ibh );
			g.LineTo( x+14+20, y+20+_ibh+4 );
			g.LineTo( x+24+20, y+20+_ibh+4 );
			g.LineTo( x+27+20, y+20+_ibh );
			g.LineTo( x+w-r,   y+20+_ibh );
			
			// Else-arm right side
			g.Arc(    x+w-r, y+20+_ibh+r, r, -hpi, 0.0 );
			g.LineTo( x+w,   y+40+_ibh-r );
			g.Arc(    x+w-r, y+40+_ibh-r, r, 0.0, hpi );			
			
			// Else-arm out side
			g.LineTo( x+27+20, y+40+_ibh );	
			g.LineTo( x+24+20, y+40+_ibh+4 );
			g.LineTo( x+14+20, y+40+_ibh+4 );
			g.LineTo( x+11+20, y+40+_ibh );
			g.LineTo( x+20+r,  y+40+_ibh );
			
			// Else-block left inner side
			g.ArcNegative( x+20+r, y+40+_ibh+r, r, -hpi, Math.PI );
			g.LineTo(      x+20,   y+h-20-r );
			g.ArcNegative( x+20+r, y+h-20-r, r, Math.PI, hpi);
			
			// Else-block out side
			g.LineTo( x+11+20, y+h-20);
			g.LineTo( x+14+20, y+h-20+4);
			g.LineTo( x+24+20, y+h-20+4);
			g.LineTo( x+27+20, y+h-20);
			g.LineTo( x+w-r,   y+h-20 );
			
			// Bottom arm right side
			g.Arc(    x+w-r, y+h-20+r, r, -hpi, 0.0);
			g.LineTo( x+w,   y+h-r );
			g.Arc(    x+w-r, y+h-r, r, 0.0, hpi);
			
			// Bottom arm bottom side
			g.LineTo( x+27, y+h );
			g.LineTo( x+24, y+h+4 );
			g.LineTo( x+14, y+h+4 );
			g.LineTo( x+11, y+h );
			g.LineTo( x+r,  y+h );
			
			// left side of block
			g.Arc(    x+r, y+h-r, r, hpi, Math.PI );
			g.LineTo( x, y+r );
            g.ClosePath();			
		}
    }

	// -----------------------------------------------------------------------
    public class CControlWhile : CBlock
    {	// Repeat control block while consition is true
		
		// Internal edge
		public CEdge LoopEdge = null;
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CControlWhile(Double X, Double Y, bool isFactory) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] {
				new Diagram.CPoint(X, Y), 
				new Diagram.CPoint(X + 175, Y + 50) }),
				isFactory) 
		{
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkGoldenrod;
			this.FillColor = Diagram.Colors.PaleGoldenrod;
			this.Sizable = false;
			
			// Create inner edge to connect loop stack
			double offsetX = 0.5*this.Width + 10.0;
			LoopEdge = new CEdge(this, "Loop", EdgeType.Out, null, offsetX, 20.0, 20.0, 20.0, this.Width-20.0);
			
			// Properties
			CExpressionProperty WhileTest = new CExpressionProperty("WhileTest", "True");
			WhileTest.PropertyChanged += OnPropertyChanged;
			_properties["WhileTest"] = WhileTest;
			this.OnPropertyChanged(null, null);
		}
		
		public CControlWhile(Double X, Double Y) : this(X, Y, false) { }
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override Diagram.CShape HitShape(Diagram.CPoint pt, Diagram.Canvas cvs)
        {	// If this block is hit, return a self reference.
        	
			if (!this.visible) return null;
			
			double X = pt.X;
			double Y = pt.Y;
			double Ymin = this.Top;
			double Xmin = this.Left;
			double Ymax = Ymin + this.Height;
			double Xmax = Xmin + this.Width;
			
			// If point in outer bounding box...
			if (X >= Xmin && X <= Xmax && Y >= Ymin && Y <= Ymax)
			{	// ...and also in inner bounding box
				if (X >= (Xmin + 20) && Y >= (Ymin + 20) && Y <= (Ymax - 20))
				{	// ...then not hit
					return null;
				}
				else
				{	// Otherwise, hit
					return this;
				}
			}
			else
			{	// Not hit if outside outer bounding box
				return null;
			}
        }
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		internal override CEdge GetEdgeByName(string name)
		{
			// First try base class behavior.
			// If edge not found, look for custom edges.
			string tname = name.ToLower();
			
			CEdge e = base.GetEdgeByName(tname);
			if (e == null) {
				if (tname == "loop") {
					e = LoopEdge;
				}
			}
			return e;
		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Update text when property changes
		public void OnPropertyChanged(object sender, EventArgs e){
			this.Text = String.Format("while ({0})", this["WhileTest"]);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Execute a simple repetition
		public override IEnumerator<RunnerResponse> 
			Runner(Expression.Scope locals, Dictionary<string, object> builtins) 
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
			// TODO: Allow access to global namespace
			// Do the while loop
			bool doWhile = false;
			while(true) {

				try {
					CExpressionProperty WhileTest = (CExpressionProperty)_properties["WhileTest"];
					object otest = WhileTest.Expr.Evaluate(locals);
					doWhile = (bool)otest;
	
				} catch (Exception ex) {
					this["Message"] = ex.Message;
					
					this.State = BlockState.Error;
					rr.Action = EngineAction.NoAction;
					rr.Runner = null;
				}
				
				// Go into a loop while block remains in an error state
				while (this.State == BlockState.Error) yield return rr;
			
				// Next perform one iteration of the enclosed stack
				if (doWhile == true && this.LoopEdge.IsConnected) {
					// Push internal runner on to stack
					// Pops itself off stack when done
					rr.Action = EngineAction.Add;
					rr.Runner = this.LoopEdge.LinkedTo.Block.Runner(locals, builtins);
					yield return rr;
				} 
				else 
				{	// Break out of internal while loop
					break;
				}

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
		// Execute a simple repetition
//		public override IEnumerator<RunnerResponse> 
//			Runner(Dictionary<string, object> locals, Dictionary<string, object> builtins) 
//		{
//			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//			// Always place this block of code at the top of all block runners
//			this.State = BlockState.Running;				// Indicate that the block is running
//			RunnerResponse rr = new RunnerResponse();		// Create and return initial response object
//			yield return rr;
//			if (this.BreakPoint == true) {					// Indicate if breakpoint is set on this block
//				rr.Action = EngineAction.Break;				// so that engine can stop
//				rr.Runner = null;
//				yield return rr;
//			}
//			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//
//			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//			// Do the while-test
//			bool doWhile = false;
//
//
//			// TODO: Allow access to global namespace
//			try {
//				CExpressionProperty Repetitions = (CExpressionProperty)_properties["WhileTest"];
//				Repetitions.Expr.Parameters = locals;
//				object oreps = Repetitions.Expr.Evaluate();
//				maxreps = (int)oreps;
//
//			} catch (Exception ex) {
//				this["Message"] = ex.Message;
//				//MsgProp.Text = ex.Message;
//				
//				this.State = BlockState.Error;
//				rr.Action = EngineAction.NoAction;
//				rr.Runner = null;
//			}
//			// Go into a loop while block remains in an error state
//			while (this.State == BlockState.Error) yield return rr;
//			
//			// Execute the repetition
//			while ( count <= maxreps )
//			{
//				// Next perform one iteration of the enclosed stack
//				if (this.LoopEdge.IsConnected) {
//					rr.Action = EngineAction.Add;
//					rr.Runner = this.LoopEdge.LinkedTo.Block.Runner(locals, builtins);
//					yield return rr;
//				}
//				
//				// At this point the body of the loop has completed
//				// Increment counter
//				count++;
//			}			
//			
//			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//
//			// Go into a loop while block remains in an error state
//			while (this.State == BlockState.Error) yield return rr;
//
//			// If connected, replace this runner with the next runner to the stack.
//			if (this.OutEdge.IsConnected) {
//				rr.Action = EngineAction.Replace;
//				rr.Runner = this.OutEdge.LinkedTo.Block.Runner(locals, builtins);
//			} else {
//				// If not connected, just remove this runner
//				rr.Action = EngineAction.Remove;
//				rr.Runner = null;
//			}
//			
//			// Indicate that the block is no longer running
//			this.State = BlockState.Idle;
//			yield return rr;
//		}
				
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void RepositionBlocks(CEdge entryEdge)
		{	// Reposition this block wrt the entry edge
			
			// If the LoopEdge is connected, prior to repositiion the block wrt to
			// the rest of the diagram, calculate the LoopEdge stack and resize this block
			// This way, the remaining repositioning of embedded blocks fits nicely.
			
			if (this.LoopEdge.IsConnected) {
				CEdge linkedEdge = this.LoopEdge.LinkedTo;
				CBlock linkedBlock = linkedEdge.Block;
				linkedBlock.RepositionBlocks(linkedEdge);
				this.Height = linkedBlock.StackHeight + 40.0;
				
			} else {
				this.Height = 50.0;
			}
			
			base.RepositionBlocks(entryEdge);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override List<CEdge> Edges 
		{	// Return a list of all edges
			get {
				List<CEdge> prts = base.Edges;
				prts.Add(this.LoopEdge);			// Add special LoopEdge to the standard edges
				return prts;
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected override void SetPath(Cairo.Context g) 
		{
			double x = this.left;
            double y = this.top;
			
			// If absolute positioning, convert x and y back to screen coordinates
			//if (this.Dock == Diagram.DockSide.Left) g.InverseTransformPoint(ref x, ref y);
			
            double w = this.width;
            double h = this.height;
			double r = 6.0;
			double hpi = 0.5*Math.PI;
			
			g.MoveTo( x, y+r );
			g.Arc(    x+r, y+r, r, Math.PI, -hpi );
			g.LineTo( x+11, y );
			g.LineTo( x+14, y+4 );
			g.LineTo( x+24, y+4 );
			g.LineTo( x+27, y );
			g.LineTo( x+w-r, y );
			g.Arc(    x+w-r, y+r, r, -hpi, 0.0 );
			g.LineTo( x+w, y+20-r );
			g.Arc(    x+w-r, y+20-r, r, 0.0, hpi );
			g.LineTo( x+27+20, y+20 );
			g.LineTo( x+24+20, y+20+4 );
			g.LineTo( x+14+20, y+20+4 );
			g.LineTo( x+11+20, y+20 );
			g.LineTo( x+20+r, y+20 );
			g.ArcNegative(    x+20+r, y+20+r, r, -hpi, Math.PI );
			g.LineTo( x+20, y+h-20-r );
			g.ArcNegative(    x+20+r, y+h-20-r, r, Math.PI, hpi);
			g.LineTo( x+11+20, y+h-20);
			g.LineTo( x+14+20, y+h-20+4);
			g.LineTo( x+24+20, y+h-20+4);
			g.LineTo( x+27+20, y+h-20);
			g.LineTo( x+w-r, y+h-20 );
			g.Arc(    x+w-r, y+h-20+r, r, -hpi, 0.0);
			g.LineTo( x+w, y+h-r );
			g.Arc(    x+w-r, y+h-r, r, 0.0, hpi);
			g.LineTo( x+27, y+h );
			g.LineTo( x+24, y+h+4 );
			g.LineTo( x+14, y+h+4 );
			g.LineTo( x+11, y+h );
			g.LineTo( x+r, y+h );
			g.Arc(    x+r, y+h-r, r, hpi, Math.PI );
			g.LineTo( x, y+r );
            g.ClosePath();
		}		
    }

	// -----------------------------------------------------------------------
//    public class CControlWhile : CBlock
//    {	// Repeat control block shape class
//		
//		// Internal edge
//		public CEdge LoopEdge = null;
//		
//		// Properties
//		public CExpressionProperty WhileTest = null;
//		
//        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//        public CControlWhile(Double X, Double Y) 
//			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] {
//				new Diagram.CPoint(X, Y), 
//				new Diagram.CPoint(X + 175, Y + 50)
//			})) {
//			this.LineWidth = 2;
//			this.LineColor = Color.DarkGoldenrod;
//			this.FillColor = Color.PaleGoldenrod;
//			this.FontStyle = FontStyle.Bold;
//			this.Sizable = false;
//			
//			// Create inner edge to connect loop stack
//			double offsetX = 0.5*this.Width + 10.0;
//			LoopEdge = new CEdge(this, "Loop", EdgeType.Out, null, offsetX, 20.0, 20.0, 20.0, this.Width-20.0);
//			
//			// Properties
//			WhileTest = new CExpressionProperty("while", "true");
//			WhileTest.PropertyChanged += OnPropertyChanged;
//			this.OnPropertyChanged(null, null);
//		}
//		
//		public CControlWhile(Double X, Double Y, bool isFactory) : this(X, Y)
//		{
//			this._isFactory = isFactory;
//		}
//		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		// Returns a list of all block properties
//		public override List<CProperty> Properties 
//		{
//			get {
//				List<CProperty> props = base.Properties;
//				props.Add(this.WhileTest);
//				return props;
//			}
//		}
//		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//        public override Diagram.CShape HitShape(Diagram.CPoint pt, Diagram.Canvas cvs)
//        {	// If this block is hit, return a self reference.
//        	
//			double X = pt.X;
//			double Y = pt.Y;
//			double Ymin = this.Top;
//			double Xmin = this.Left;
//			double Ymax = Ymin + this.Height;
//			double Xmax = Xmin + this.Width;
//			
//			// If point in outer bounding box...
//			if (X >= Xmin && X <= Xmax && Y >= Ymin && Y <= Ymax)
//			{	// ...and also in inner bounding box
//				if (X >= (Xmin + 20) && Y >= (Ymin + 20) && Y <= (Ymax - 20))
//				{	// ...then not hit
//					return null;
//				}
//				else
//				{	// Otherwise, hit
//					return this;
//				}
//			}
//			else
//			{	// Not hit if outside outer bounding box
//				return null;
//			}
//        }
//		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		internal override bool SetProperty(string name, string val) {
//			string lname = name.ToLower();
//			switch(lname) {
//			case "while":
//				WhileTest.Text = val;
//				break;
//			default:
//				return false;
//			}
//			return true;
//		}
//		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		internal override CEdge GetEdgeByName(string name)
//		{
//			// First try base class behavior.
//			// If edge not found, look for custom edges.
//			string tname = name.ToLower();
//			
//			CEdge e = base.GetEdgeByName(tname);
//			if (e == null) {
//				if (tname == "loop") {
//					e = LoopEdge;
//				}
//			}
//			return e;
//		}
//		
//        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//        // Write custom tags for this block
//        protected override void WriteXmlTags(XmlWriter w)
//        {
//			base.WriteXmlTags(w);
//			this.WriteXmlProperty(w, "while", WhileTest.Text);
//        }
//		
//        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		// Update text when property changes
//		public void OnPropertyChanged(object sender, EventArgs e){
//			this.Text = String.Format("while ({0})", WhileTest.Text);
//		}
//		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		// Execute a simple repetition
//		public override IEnumerator<RunnerResponse> 
//			Runner(Dictionary<string, object> locals, Dictionary<string, object> builtins) 
//		{
//			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//			// Always place this block of code at the top of all block runners
//			this.State = BlockState.Running;				// Indicate that the block is running
//			RunnerResponse rr = new RunnerResponse();		// Create and return initial response object
//			yield return rr;
//			if (this.BreakPoint == true) {					// Indicate if breakpoint is set on this block
//				rr.Action = EngineAction.Break;				// so that engine can stop
//				rr.Runner = null;
//				yield return rr;
//			}
//			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//
//			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//			// TODO: Allow access to global namespace
//			// Do the while loop
//			bool doWhile = false;
//			while(true) {
//
//				try {
//					WhileTest.Expr.Parameters = locals;
//					object otest = WhileTest.Expr.Evaluate();
//					doWhile = (bool)otest;
//	
//				} catch (Exception ex) {
//					MsgProp.Text = ex.Message;
//					
//					this.State = BlockState.Error;
//					rr.Action = EngineAction.NoAction;
//					rr.Runner = null;
//				}
//				
//				// Go into a loop while block remains in an error state
//				while (this.State == BlockState.Error) yield return rr;
//			
//				// Next perform one iteration of the enclosed stack
//				if (doWhile == true && this.LoopEdge.IsConnected) {
//					// Push internal runner on to stack
//					// Pops itself off stack when done
//					rr.Action = EngineAction.Add;
//					rr.Runner = this.LoopEdge.LinkedTo.Block.Runner(locals, builtins);
//					yield return rr;
//				} 
//				else 
//				{	// Break out of internal while loop
//					break;
//				}
//
//			}
//			
//			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//
//			// Go into a loop while block remains in an error state
//			while (this.State == BlockState.Error) yield return rr;
//
//			// If connected, replace this runner with the next runner to the stack.
//			if (this.OutEdge.IsConnected) {
//				rr.Action = EngineAction.Replace;
//				rr.Runner = this.OutEdge.LinkedTo.Block.Runner(locals, builtins);
//			} else {
//				// If not connected, just remove this runner
//				rr.Action = EngineAction.Remove;
//				rr.Runner = null;
//			}
//			
//			// Indicate that the block is no longer running
//			this.State = BlockState.Idle;
//			yield return rr;
//		}
//				
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		public override void RepositionBlocks(CEdge entryEdge)
//		{	// Reposition this block wrt the entry edge
//			
//			// If the LoopEdge is connected, prior to repositiion the block wrt to
//			// the rest of the diagram, calculate the LoopEdge stack and resize this block
//			// This way, the remaining repositioning of embedded blocks fits nicely.
//			
//			if (this.LoopEdge.IsConnected) {
//				CEdge linkedEdge = this.LoopEdge.LinkedTo;
//				CBlock linkedBlock = linkedEdge.Block;
//				linkedBlock.RepositionBlocks(linkedEdge);
//				this.Height = linkedBlock.StackHeight + 40.0;
//				
//			} else {
//				this.Height = 50.0;
//			}
//			
//			base.RepositionBlocks(entryEdge);
//		}
//		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		public override List<CEdge> Edges 
//		{	// Return a list of all edges
//			get {
//				List<CEdge> prts = base.Edges;
//				prts.Add(this.LoopEdge);			// Add special LoopEdge to the standard edges
//				return prts;
//			}
//		}
//		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		protected override GraphicsPath Figure() 
//		{
//			float x = (float)(this.left);
//            float y = (float)(this.top);
//            float w = (float)(this.width);
//            float h = (float)(this.height);
//			
//			GraphicsPath path = new GraphicsPath();
//            path.StartFigure();
//			path.AddArc(x, y, 6F, 6F, 180F, 90F);
//			path.AddLine(x+11, y, x+14, y+4);
//			path.AddLine(x+24, y+4, x+27, y);
//			path.AddArc(x+w-6, y, 6F, 6F, 270F, 90F);
//			path.AddArc(x+w-6, y+20-6, 6F, 6F, 0F, 90F);
//			path.AddLine(x+27+20, y+20, x+24+20, y+20+4);
//			path.AddLine(x+14+20, y+20+4, x+11+20, y+20);
//			path.AddArc(x+20, y+20, 6F, 6F, 270F, -90F);
//			path.AddArc(x+20, y+h-20-6, 6F, 6F, 180F, -90F);
//			path.AddLine(x+11+20, y+h-20, x+14+20, y+h-20+4);
//			path.AddLine(x+24+20, y+h-20+4, x+27+20, y+h-20);
//			path.AddArc(x+w-6, y+h-20, 6F, 6F, 270F, 90F);
//			path.AddArc(x+w-6, y+h-6, 6F, 6F, 0F, 90F);
//			path.AddLine(x+27, y+h, x+24, y+h+4);
//			path.AddLine(x+14, y+h+4, x+11, y+h);
//			path.AddArc(x, y+h-6, 6F, 6F, 90F, 90F);
//			path.CloseFigure();
//			
//			return path;
//		}
//    }

	// -----------------------------------------------------------------------
//    public class CControlStart : CBlock
//    {	// Control start block shape class
//
//        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//        public CControlStart(Double X, Double Y) 
//			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
//				new Diagram.CPoint(X, Y), 
//				new Diagram.CPoint(X + 175, Y + 30)
//			})) {
//			this.LineWidth = 2;
//			this.LineColor = Color.DarkGoldenrod;
//			this.FillColor = Color.PaleGoldenrod;
//			this.FontStyle = FontStyle.Bold;
//			this.Sizable = false;
//			this.Text = "when program starts";
//			textYOffset = 13;							// Block text offset
//		}
//		
//		public CControlStart(Double X, Double Y, bool isFactory) : this(X, Y)
//		{
//			this._isFactory = isFactory;
//		}
//		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		public override List<CEdge> Edges 
//		{	// Return a list of all edges
//			// Control start blocks only have an output edge
//			get {
//				return new List<CEdge>() { this.OutEdge };
//			}
//		}
//
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		// Block outline graphics path figure
//		protected override GraphicsPath Figure() 
//		{
//			float x = (float)(this.left);
//            float y = (float)(this.top);
//            float w = (float)(this.width);
//            float h = (float)(this.height);
//			
//			GraphicsPath path = new GraphicsPath();
//            path.StartFigure();
//			
//			path.AddBezier(new Point((int)x, (int)y+10), 
//			               new Point((int)x+55, (int)y-20),
//			               new Point((int)x+100, (int)y+10),
//			               new Point((int)x+110, (int)y+10) );
//			path.AddLine(x+110, y+10, x+w-6, y+10);
//			path.AddArc(x+w-6, y+10, 6F, 6F, 270F, 90F);
//			path.AddArc(x+w-6, y+h-6, 6F, 6F, 0F, 90F);
//			path.AddLine(x+27, y+h, x+24, y+h+4);
//			path.AddLine(x+14, y+h+4, x+11, y+h);
//			path.AddArc(x, y+h-6, 6F, 6F, 90F, 90F);
//			path.CloseFigure();
//			
//			return path;
//		}
//    }

	// -----------------------------------------------------------------------
//    public class CControlEnd : CBlock
//    {	// Control end block shape class
//		
//        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//        public CControlEnd(Double X, Double Y) 
//			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
//				new Diagram.CPoint(X, Y), 
//				new Diagram.CPoint(X + 175, Y + 20)
//			})) {
//			this.LineWidth = 2;
//			this.LineColor = Color.DarkGoldenrod;
//			this.FillColor = Color.PaleGoldenrod;
//			this.FontStyle = FontStyle.Bold;
//			this.Sizable = false;
//			this.Text = "stop script";
//		}
//		
//		public CControlEnd(Double X, Double Y, bool isFactory) : this(X, Y)
//		{
//			this._isFactory = isFactory;
//		}
//		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		public override List<CEdge> Edges 
//		{	// Return a list of all edges
//			// Control end blocks only have an input edge
//			get {
//				return new List<CEdge>() { this.InEdge };
//			}
//		}
//		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		protected override GraphicsPath Figure() 
//		{
//			float x = (float)(this.left);
//            float y = (float)(this.top);
//            float w = (float)(this.width);
//            float h = (float)(this.height);
//			
//			GraphicsPath path = new GraphicsPath();
//            path.StartFigure();
//			path.AddArc(x, y, 6F, 6F, 180F, 90F);
//			path.AddLine(x+11, y, x+14, y+4);
//			path.AddLine(x+24, y+4, x+27, y);
//			path.AddArc(x+w-6, y, 6F, 6F, 270F, 90F);
//			path.AddArc(x+w-6, y+h-6, 6F, 6F, 0F, 90F);
//			path.AddArc(x, y+h-6, 6F, 6F, 90F, 90F);
//            path.CloseFigure();
//			
//			return path;
//		}
//    }
	
//	// -----------------------------------------------------------------------
//    public class CControlRepeat : CBlock
//    {	// Repeat control block shape class
//		
//		// Internal edge
//		public CEdge LoopEdge = null;
//		
//		// Properties
//		public CExpressionProperty Repetitions = null;
//		
//        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//        public CControlRepeat(Double X, Double Y) 
//			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] {
//				new Diagram.CPoint(X, Y), 
//				new Diagram.CPoint(X + 175, Y + 50)
//			})) {
//			this.LineWidth = 2;
//			this.LineColor = Color.DarkGoldenrod;
//			this.FillColor = Color.PaleGoldenrod;
//			this.FontStyle = FontStyle.Bold;
//			this.Sizable = false;
//			
//			// Create inner edge to connect loop stack
//			double offsetX = 0.5*this.Width + 10.0;
//			LoopEdge = new CEdge(this, "Loop", EdgeType.Out, null, offsetX, 20.0, 20.0, 20.0, this.Width-20.0);
//			
//			// Properties
//			Repetitions = new CExpressionProperty("Repetitions", "3");
//			Repetitions.PropertyChanged += OnPropertyChanged;
//			this.OnPropertyChanged(null, null);
//		}
//		
//		public CControlRepeat(Double X, Double Y, bool isFactory) : this(X, Y)
//		{
//			this._isFactory = isFactory;
//		}
//		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		// Returns a list of all block properties
//		public override List<CProperty> Properties 
//		{
//			get {
//				List<CProperty> props = base.Properties;
//				props.Add(this.Repetitions);
//				return props;
//			}
//		}
//		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//        public override Diagram.CShape HitShape(Diagram.CPoint pt, Diagram.Canvas cvs)
//        {	// If this block is hit, return a self reference.
//        	
//			double X = pt.X;
//			double Y = pt.Y;
//			double Ymin = this.Top;
//			double Xmin = this.Left;
//			double Ymax = Ymin + this.Height;
//			double Xmax = Xmin + this.Width;
//			
//			// If point in outer bounding box...
//			if (X >= Xmin && X <= Xmax && Y >= Ymin && Y <= Ymax)
//			{	// ...and also in inner bounding box
//				if (X >= (Xmin + 20) && Y >= (Ymin + 20) && Y <= (Ymax - 20))
//				{	// ...then not hit
//					return null;
//				}
//				else
//				{	// Otherwise, hit
//					return this;
//				}
//			}
//			else
//			{	// Not hit if outside outer bounding box
//				return null;
//			}
//        }
//		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		internal override bool SetProperty(string name, string val) {
//			string lname = name.ToLower();
//			switch(lname) {
//			case "repetitions":
//				Repetitions.Text = val;
//				break;
//			default:
//				return false;
//			}
//			return true;
//		}
//		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		internal override CEdge GetEdgeByName(string name)
//		{
//			// First try base class behavior.
//			// If edge not found, look for custom edges.
//			string tname = name.ToLower();
//			
//			CEdge e = base.GetEdgeByName(tname);
//			if (e == null) {
//				if (tname == "loop") {
//					e = LoopEdge;
//				}
//			}
//			return e;
//		}
//		
//        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//        // Write custom tags for this block
//        protected override void WriteXmlTags(XmlWriter w)
//        {
//			base.WriteXmlTags(w);
//			this.WriteXmlProperty(w, "Repetitions", Repetitions.Text);
//        }
//		
//        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		// Update text when property changes
//		public void OnPropertyChanged(object sender, EventArgs e){
//			this.Text = String.Format("repeat {0} times", Repetitions.Text);
//		}
//		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		// Execute a simple repetition
//		public override IEnumerator<RunnerResponse> 
//			Runner(Dictionary<string, object> locals, Dictionary<string, object> builtins) 
//		{
//			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//			// Always place this block of code at the top of all block runners
//			this.State = BlockState.Running;				// Indicate that the block is running
//			RunnerResponse rr = new RunnerResponse();		// Create and return initial response object
//			yield return rr;
//			if (this.BreakPoint == true) {					// Indicate if breakpoint is set on this block
//				rr.Action = EngineAction.Break;				// so that engine can stop
//				rr.Runner = null;
//				yield return rr;
//			}
//			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//
//			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//			// Do the repetition
//			int count = 1;
//			int maxreps = 0;
//
//			// Start by getting the number of repetitions as an integer
//			// TODO: Allow access to global namespace
//			try {
//				Repetitions.Expr.Parameters = locals;
//				object oreps = Repetitions.Expr.Evaluate();
//				maxreps = (int)oreps;
//
//			} catch (Exception ex) {
//				MsgProp.Text = ex.Message;
//				
//				this.State = BlockState.Error;
//				rr.Action = EngineAction.NoAction;
//				rr.Runner = null;
//			}
//			// Go into a loop while block remains in an error state
//			while (this.State == BlockState.Error) yield return rr;
//			
//			// Execute the repetition
//			while ( count <= maxreps )
//			{
//				// Next perform one iteration of the enclosed stack
//				if (this.LoopEdge.IsConnected) {
//					rr.Action = EngineAction.Add;
//					rr.Runner = this.LoopEdge.LinkedTo.Block.Runner(locals, builtins);
//					yield return rr;
//				}
//				
//				// At this point the body of the loop has completed
//				// Increment counter
//				count++;
//			}			
//			
//			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//
//			// Go into a loop while block remains in an error state
//			while (this.State == BlockState.Error) yield return rr;
//
//			// If connected, replace this runner with the next runner to the stack.
//			if (this.OutEdge.IsConnected) {
//				rr.Action = EngineAction.Replace;
//				rr.Runner = this.OutEdge.LinkedTo.Block.Runner(locals, builtins);
//			} else {
//				// If not connected, just remove this runner
//				rr.Action = EngineAction.Remove;
//				rr.Runner = null;
//			}
//			
//			// Indicate that the block is no longer running
//			this.State = BlockState.Idle;
//			yield return rr;
//		}
//				
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		public override void RepositionBlocks(CEdge entryEdge)
//		{	// Reposition this block wrt the entry edge
//			
//			// If the LoopEdge is connected, prior to repositiion the block wrt to
//			// the rest of the diagram, calculate the LoopEdge stack and resize this block
//			// This way, the remaining repositioning of embedded blocks fits nicely.
//			
//			if (this.LoopEdge.IsConnected) {
//				CEdge linkedEdge = this.LoopEdge.LinkedTo;
//				CBlock linkedBlock = linkedEdge.Block;
//				linkedBlock.RepositionBlocks(linkedEdge);
//				this.Height = linkedBlock.StackHeight + 40.0;
//				
//			} else {
//				this.Height = 50.0;
//			}
//			
//			base.RepositionBlocks(entryEdge);
//		}
//		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		public override List<CEdge> Edges 
//		{	// Return a list of all edges
//			get {
//				List<CEdge> prts = base.Edges;
//				prts.Add(this.LoopEdge);			// Add special LoopEdge to the standard edges
//				return prts;
//			}
//		}
//		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		protected override GraphicsPath Figure() 
//		{
//			float x = (float)(this.left);
//            float y = (float)(this.top);
//            float w = (float)(this.width);
//            float h = (float)(this.height);
//			
//			GraphicsPath path = new GraphicsPath();
//            path.StartFigure();
//			path.AddArc(x, y, 6F, 6F, 180F, 90F);
//			path.AddLine(x+11, y, x+14, y+4);
//			path.AddLine(x+24, y+4, x+27, y);
//			path.AddArc(x+w-6, y, 6F, 6F, 270F, 90F);
//			path.AddArc(x+w-6, y+20-6, 6F, 6F, 0F, 90F);
//			path.AddLine(x+27+20, y+20, x+24+20, y+20+4);
//			path.AddLine(x+14+20, y+20+4, x+11+20, y+20);
//			path.AddArc(x+20, y+20, 6F, 6F, 270F, -90F);
//			path.AddArc(x+20, y+h-20-6, 6F, 6F, 180F, -90F);
//			path.AddLine(x+11+20, y+h-20, x+14+20, y+h-20+4);
//			path.AddLine(x+24+20, y+h-20+4, x+27+20, y+h-20);
//			path.AddArc(x+w-6, y+h-20, 6F, 6F, 270F, 90F);
//			path.AddArc(x+w-6, y+h-6, 6F, 6F, 0F, 90F);
//			path.AddLine(x+27, y+h, x+24, y+h+4);
//			path.AddLine(x+14, y+h+4, x+11, y+h);
//			path.AddArc(x, y+h-6, 6F, 6F, 90F, 90F);
//			path.CloseFigure();
//			
//			return path;
//		}
//    }

//	// -----------------------------------------------------------------------
//    public class CControlIf : CBlock
//    {	// Execute inner block if conditional evaluates to true
//		
//		// Internal edge
//		public CEdge IfEdge = null;
//		
//		// Properties
//		public CExpressionProperty IfTest = null;
//		
//        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//        public CControlIf(Double X, Double Y) 
//			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] {
//				new Diagram.CPoint(X, Y), 
//				new Diagram.CPoint(X + 175, Y + 50)
//			})) {
//			this.LineWidth = 2;
//			this.LineColor = Color.DarkGoldenrod;
//			this.FillColor = Color.PaleGoldenrod;
//			this.FontStyle = FontStyle.Bold;
//			this.Sizable = false;
//			
//			// Create inner edge to connect if-block stack
//			double offsetX = 0.5*this.Width + 10.0;
//			IfEdge = new CEdge(this, "If", EdgeType.Out, null, offsetX, 20.0, 20.0, 20.0, this.Width-20.0);
//			
//			// Properties
//			IfTest = new CExpressionProperty("If-test", "true");
//			IfTest.PropertyChanged += OnPropertyChanged;
//			this.OnPropertyChanged(null, null);
//		}
//		
//		public CControlIf(Double X, Double Y, bool isFactory) : this(X, Y)
//		{
//			this._isFactory = isFactory;
//		}
//		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		// Returns a list of all block properties
//		public override List<CProperty> Properties 
//		{
//			get {
//				List<CProperty> props = base.Properties;
//				props.Add(this.IfTest);
//				return props;
//			}
//		}
//		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//        public override Diagram.CShape HitShape(Diagram.CPoint pt, Diagram.Canvas cvs)
//        {	// If this block is hit, return a self reference.
//        	
//			double X = pt.X;
//			double Y = pt.Y;
//			double Ymin = this.Top;
//			double Xmin = this.Left;
//			double Ymax = Ymin + this.Height;
//			double Xmax = Xmin + this.Width;
//			
//			// If point in outer bounding box...
//			if (X >= Xmin && X <= Xmax && Y >= Ymin && Y <= Ymax)
//			{	// ...and also in inner bounding box
//				if (X >= (Xmin + 20) && Y >= (Ymin + 20) && Y <= (Ymax - 20))
//				{	// ...then not hit
//					return null;
//				}
//				else
//				{	// Otherwise, hit
//					return this;
//				}
//			}
//			else
//			{	// Not hit if outside outer bounding box
//				return null;
//			}
//        }
//		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		internal override bool SetProperty(string name, string val) {
//			string lname = name.ToLower();
//			switch(lname) {
//			case "if":
//				IfTest.Text = val;
//				break;
//			default:
//				return false;
//			}
//			return true;
//		}
//		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		internal override CEdge GetEdgeByName(string name)
//		{
//			// First try base class behavior.
//			// If edge not found, look for custom edges.
//			string tname = name.ToLower();
//			
//			CEdge e = base.GetEdgeByName(tname);
//			if (e == null) {
//				if (tname == "if") {
//					e = IfEdge;
//				}
//			}
//			return e;
//		}
//		
//        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//        // Write custom tags for this block
//        protected override void WriteXmlTags(XmlWriter w)
//        {
//			base.WriteXmlTags(w);
//			this.WriteXmlProperty(w, "if", IfTest.Text);
//        }
//		
//        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		// Update text when property changes
//		public void OnPropertyChanged(object sender, EventArgs e){
//			this.Text = String.Format("if ({0})", IfTest.Text);
//		}
//		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		// Execute a simple repetition
//		public override IEnumerator<RunnerResponse> 
//			Runner(Dictionary<string, object> locals, Dictionary<string, object> builtins) 
//		{
//			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//			// Always place this block of code at the top of all block runners
//			this.State = BlockState.Running;				// Indicate that the block is running
//			RunnerResponse rr = new RunnerResponse();		// Create and return initial response object
//			yield return rr;
//			if (this.BreakPoint == true) {					// Indicate if breakpoint is set on this block
//				rr.Action = EngineAction.Break;				// so that engine can stop
//				rr.Runner = null;
//				yield return rr;
//			}
//			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//			
//			// Do the if-test
//			bool doIf = false;
//
//			// Start by evaluating the if expression
//			// TODO: Allow access to global namespace
//			try {
//				IfTest.Expr.Parameters = locals;
//				object otest = IfTest.Expr.Evaluate();
//				doIf = (bool)otest;
//
//			} catch (Exception ex) {
//				MsgProp.Text = ex.Message;
//				
//				this.State = BlockState.Error;
//				rr.Action = EngineAction.NoAction;
//				rr.Runner = null;
//			}
//			// Go into a loop while block remains in an error state
//			while (this.State == BlockState.Error) yield return rr;
//			
//			// Execute the repetition
//			if ( doIf )
//			{	// Execute enclosed stack
//				if (this.IfEdge.IsConnected) {
//					rr.Action = EngineAction.Add;
//					rr.Runner = this.IfEdge.LinkedTo.Block.Runner(locals, builtins);
//					yield return rr;
//				}
//			}			
//			
//			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//			// Go into a loop while block remains in an error state
//			while (this.State == BlockState.Error) yield return rr;
//
//			// If connected, replace this runner with the next runner to the stack.
//			if (this.OutEdge.IsConnected) {
//				rr.Action = EngineAction.Replace;
//				rr.Runner = this.OutEdge.LinkedTo.Block.Runner(locals, builtins);
//			} else {
//				// If not connected, just remove this runner
//				rr.Action = EngineAction.Remove;
//				rr.Runner = null;
//			}
//			
//			// Indicate that the block is no longer running
//			this.State = BlockState.Idle;
//			yield return rr;
//		}
//				
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		public override void RepositionBlocks(CEdge entryEdge)
//		{	// Reposition this block wrt the entry edge
//			
//			// If the IfEdge is connected, prior to repositiion the block wrt to
//			// the rest of the diagram, calculate the IfEdge stack and resize this block
//			// This way, the remaining repositioning of embedded blocks fits nicely.
//			
//			if (this.IfEdge.IsConnected) {
//				CEdge linkedEdge = this.IfEdge.LinkedTo;
//				CBlock linkedBlock = linkedEdge.Block;
//				linkedBlock.RepositionBlocks(linkedEdge);
//				this.Height = linkedBlock.StackHeight + 40.0;
//				
//			} else {
//				this.Height = 50.0;
//			}
//			
//			base.RepositionBlocks(entryEdge);
//		}
//		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		public override List<CEdge> Edges 
//		{	// Return a list of all edges
//			get {
//				List<CEdge> prts = base.Edges;
//				prts.Add(this.IfEdge);			// Add special IfEdge to the standard edges
//				return prts;
//			}
//		}
//		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		protected override GraphicsPath Figure() 
//		{
//			float x = (float)(this.left);
//            float y = (float)(this.top);
//            float w = (float)(this.width);
//            float h = (float)(this.height);
//			
//			GraphicsPath path = new GraphicsPath();
//            path.StartFigure();
//			path.AddArc(x, y, 6F, 6F, 180F, 90F);
//			path.AddLine(x+11, y, x+14, y+4);
//			path.AddLine(x+24, y+4, x+27, y);
//			path.AddArc(x+w-6, y, 6F, 6F, 270F, 90F);
//			path.AddArc(x+w-6, y+20-6, 6F, 6F, 0F, 90F);
//			path.AddLine(x+27+20, y+20, x+24+20, y+20+4);
//			path.AddLine(x+14+20, y+20+4, x+11+20, y+20);
//			path.AddArc(x+20, y+20, 6F, 6F, 270F, -90F);
//			path.AddArc(x+20, y+h-20-6, 6F, 6F, 180F, -90F);
//			path.AddLine(x+11+20, y+h-20, x+14+20, y+h-20+4);
//			path.AddLine(x+24+20, y+h-20+4, x+27+20, y+h-20);
//			path.AddArc(x+w-6, y+h-20, 6F, 6F, 270F, 90F);
//			path.AddArc(x+w-6, y+h-6, 6F, 6F, 0F, 90F);
//			path.AddLine(x+27, y+h, x+24, y+h+4);
//			path.AddLine(x+14, y+h+4, x+11, y+h);
//			path.AddArc(x, y+h-6, 6F, 6F, 90F, 90F);
//			path.CloseFigure();
//			
//			return path;
//		}
//    }
	
}
