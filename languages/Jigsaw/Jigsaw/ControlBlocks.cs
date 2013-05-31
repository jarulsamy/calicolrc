//
//  ControlBlocks.cs
//  
//  Author:
//       Mark F. Russo <russomf@gmail.com>
// 
//  Copyright (c) 2013 The Calico Project
// 
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
// 
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
// 
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using Cairo;
using Microsoft.Scripting.Hosting;

namespace Jigsaw
{
	// --- Control start block shape class -----------------------------------------
    public class CControlStart : CBlock
    {
		public CEdge StartEdge = null;
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CControlStart(Double X, Double Y, Widgets.CBlockPalette palette = null) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y), 
				new Diagram.CPoint(X + CBlock.BlockWidth, Y + 60) }),
				palette) 
		{
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkGoldenrod;
			this.FillColor = Diagram.Colors.PaleGoldenrod;
			this.Sizable = false;
			this.Text = "when script starts";
			
			double offsetX = 0.5*this.Width + 10.0;
			this.StartEdge = new CEdge(this, "Start", EdgeType.Out, null, offsetX, 30.0, 20.0, 30.0, this.Width-20.0);
			
			_textYOffset = 22;							// Block text offset
		}
		
		public CControlStart(Double X, Double Y) : this(X, Y, null) {}
		
		// - - - Return a list of all edges - - - - - - - - - - - - - - - -
		public override List<CEdge> Edges 
		{	// Control start blocks only have an inner start edge
			get {
				return new List<CEdge>() { this.StartEdge };
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
				if (tname == "start") {
					e = StartEdge;
				}
			}
			return e;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override IEnumerator<RunnerResponse> Runner( ScriptScope scope, CallStack stack ) 
		{
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
			
			// If connected, replace this runner with the next runner to the stack.
			if (this.StartEdge.IsConnected) {
				rr.Action = EngineAction.Replace;
				rr.Frame = this.StartEdge.LinkedTo.Block.Frame(scope, stack);
			} else {
				// If not connected, just remove this runner
				rr.Action = EngineAction.Remove;
				rr.Frame = null;
			}
			
			// Indicate that the block is no longer running
			this.State = RunningState.Idle;
			yield return rr;
		}
		
		// - - - Generate and return Python statement - - - - -
		public override bool ToPython (StringBuilder o, int indent)
		{
			// This generates main statements right at the top level
			try
			{
				string sindent = new string (' ', Constant.SPACES * indent);
				if (this.StartEdge.IsConnected) {
					CBlock b = this.StartEdge.LinkedTo.Block;
					b.ToPython(o, indent);
//				} else {
//					o.AppendFormat ("{0}pass\n", sindent);
				}
				
			} catch (Exception ex){
				Console.WriteLine("{0} (in CControlStart.ToPython)", ex.Message);
				return false;
			}
			
			return true;
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
			{	// ...and also in inner bounding box
				if (X >= (Xmin + 20) && Y >= (Ymin + 30) && Y <= (Ymax - 30))
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
		public override void RepositionBlocks(CEdge entryEdge)
		{	// Reposition this block wrt the entry edge

			if (this.StartEdge.IsConnected) {
				CEdge linkedEdge = this.StartEdge.LinkedTo;
				CBlock linkedBlock = linkedEdge.Block;
				linkedBlock.RepositionBlocks(linkedEdge);
				this.Height = linkedBlock.StackHeight + 50.0;
				
			} else {
				this.Height = 60.0;
			}
			
			base.RepositionBlocks(entryEdge);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected override void SetPath(Cairo.Context g) 
		{
			double x = this.left;
            double y = this.top;
            double w = this.width;
            double h = this.height;
			double r = 6.0;
			double hpi = 0.5*Math.PI;
			
			g.MoveTo( x, y+10);
			g.Arc(    x+50, y+95, 100, -0.665*Math.PI, -0.324*Math.PI);
			g.LineTo( x+w-r, y+10);
			g.Arc(    x+w-r, y+10+r, r, -hpi, 0.0 );
			
			g.LineTo( x+w, y+30-r );
			g.Arc(    x+w-r, y+30-r, r, 0.0, hpi );
			g.LineTo( x+27+20, y+30 );
			g.LineTo( x+24+20, y+30+4 );
			g.LineTo( x+14+20, y+30+4 );
			g.LineTo( x+11+20, y+30 );
			g.LineTo( x+20+r, y+30 );
			g.ArcNegative( x+20+r, y+30+r, r, -hpi, Math.PI );
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
			g.LineTo( x+11, y+h );
			g.LineTo( x+r, y+h );
			g.Arc(    x+r, y+h-r, r, hpi, Math.PI );
			
//			g.LineTo( x+w, y+h-r );
//			g.Arc(    x+w-r, y+h-r, r, 0.0, hpi);
//			g.LineTo( x+27, y+h );
//			g.LineTo( x+24, y+h+4 );
//			g.LineTo( x+14, y+h+4 );
//			g.LineTo( x+11, y+h );
//			g.LineTo( x+r, y+h );
//			g.Arc(    x+r, y+h-r, r, hpi, Math.PI );
			
			g.LineTo( x, y+10 );
            g.ClosePath();
		}
    }
		
	// -----------------------------------------------------------------------
    public class CControlEnd : CBlock
    {	// Control end block shape class
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CControlEnd(Double X, Double Y, Widgets.CBlockPalette palette = null) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y), 
				new Diagram.CPoint(X + CBlock.BlockWidth, Y + 20)}),
				palette) 
		{
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkGoldenrod;
			this.FillColor = Diagram.Colors.PaleGoldenrod;
			this.Sizable = false;
			this.Text = "end script";
		}
		
		public CControlEnd(Double X, Double Y) : this(X, Y, null) {}
		
		
		// - - - Generate and return Python statement - - - - -
		public override bool ToPython (StringBuilder o, int indent)
		{
			try
			{
				// If end block is connected to start block, add a pass
				if (this.InEdge.IsConnected) {
					string sindent = new string (' ', Constant.SPACES * indent);
					o.AppendFormat ("{0}sys.exit(0)\n", sindent);
				}
				
			} catch (Exception ex){
				Console.WriteLine("{0} (in CControlEnd.ToPython)", ex.Message);
				return false;
			}
			
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override List<CEdge> Edges 
		{	// Return a list of all edges
			// Control end blocks only have an input edge
			get {
				return new List<CEdge>() { this.InEdge };
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override IEnumerator<RunnerResponse> Runner(ScriptScope scope, CallStack stack) 
		{
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
			// Execute the end block
			rr.Action = EngineAction.Stop;
			rr.Frame = null;
			
			// Indicate that the block is no longer running
			this.State = RunningState.Idle;
			yield return rr;
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected override void SetPath(Cairo.Context g) 
		{
			double x = this.left;
            double y = this.top;
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
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CControlRepeat(Double X, Double Y, Widgets.CBlockPalette palette = null) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] {
				new Diagram.CPoint(X, Y), 
				new Diagram.CPoint(X + CBlock.BlockWidth, Y + 50) }),
				palette) 
		{
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkGoldenrod;
			this.FillColor = Diagram.Colors.PaleGoldenrod;
			this.Sizable = false;
			this._breakStop = true;
			
			// Create inner edge to connect loop stack
			double offsetX = 0.5*this.Width + 10.0;
			LoopEdge = new CEdge(this, "Loop", EdgeType.Out, null, offsetX, 20.0, 20.0, 20.0, this.Width-20.0);
			
			// Properties
			CExpressionProperty Repetitions = new CExpressionProperty("Repetitions", "3");
			Repetitions.PropertyChanged += OnPropertyChanged;
			_properties["Repetitions"] = Repetitions;
			this.OnPropertyChanged(null, null);
		}
		
		public CControlRepeat(Double X, Double Y) : this(X, Y, null) {}
		
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
		
        // - - - Update text when property changes - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs e)
		{
			this.Text = String.Format("repeat `{0}` times", this["Repetitions"]);
			RaiseBlockChanged();
		}
		
		// - - - Generate and return Python if statement - - - - -
		public override bool ToPython (StringBuilder o, int indent)
		{
			try
			{
				string sindent = new string (' ', Constant.SPACES * indent);
				o.AppendFormat ("{0}for _ in range({1}):\n", sindent, this["Repetitions"]);

				if (this.LoopEdge.IsConnected) {
					CBlock b = this.LoopEdge.LinkedTo.Block;
					b.ToPython(o, indent+1);
				} else {
					string sindent2 = new string (' ', Constant.SPACES * (indent+1));
					o.AppendFormat ("{0}pass\n", sindent2);
				}
			
				if (this.OutEdge.IsConnected) {
					CBlock b = this.OutEdge.LinkedTo.Block;
					b.ToPython(o, indent);
//				} else {
//					string sindent2 = new string (' ', 2*(indent+1));
//					o.AppendFormat ("{0}pass\n", sindent2);
				}
				
			} catch (Exception ex){
				Console.WriteLine("{0} (in CControlRepeat.ToPython)", ex.Message);
				return false;
			}
			
			return true;
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override bool Compile(ScriptEngine engine, Jigsaw.Canvas cvs)
		{
			// Executing an if-block involves evaluting the given exression
			CExpressionProperty Repetitions = (CExpressionProperty)_properties["Repetitions"];
			try {
				Repetitions.Compile(engine);
			} catch (Exception ex) {
				Console.WriteLine ("Block {0} failed compilation: {1}", this.Name, ex.Message);
				return false;
			}
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override IEnumerator<RunnerResponse> Runner(ScriptScope scope, CallStack stack) 
		{	// Execute a simple repetition
			
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
			// Do the repetition
			int count = 1;
			int maxreps = 0;

			// Start by getting the number of repetitions as an integer
			try {
				CExpressionProperty Repetitions = (CExpressionProperty)_properties["Repetitions"];
				object oreps = Repetitions.Evaluate(scope);
				maxreps = (int)oreps;

			} catch (Exception ex) {
				this["Message"] = ex.Message;
				this.State = RunningState.Error;
				rr.Action = EngineAction.Error;
				rr.Frame = null;
			}
			
			// Go into a loop while block remains in an error state
			while (this.State == RunningState.Error) yield return rr;
			
			// Execute the repetition
			while ( count <= maxreps )
			{
				// Next perform one iteration of the enclosed stack
				if (this.LoopEdge.IsConnected) {
					rr.Action = EngineAction.Add;
					rr.Frame = this.LoopEdge.LinkedTo.Block.Frame(scope, stack);
					yield return rr;
				}
				
				// At this point the body of the loop has completed
				// Increment counter
				count++;
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
        public CControlIf(Double X, Double Y, Widgets.CBlockPalette palette = null) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] {
				new Diagram.CPoint(X, Y), 
				new Diagram.CPoint(X + CBlock.BlockWidth, Y + 50)}),
				palette) 
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
		
		public CControlIf(Double X, Double Y) : this(X, Y, null) {}
		
        // - - - Update text when property changes - - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs e)
		{
			this.Text = String.Format("if `{0}`", this["IfTest"]);
			RaiseBlockChanged();
		}
		
		// - - - 
		public override bool ToPython (StringBuilder o, int indent)
		{
			try
			{
				string sindent = new string (' ', Constant.SPACES * indent);
				o.AppendFormat ("{0}if {1}:\n", sindent, this["IfTest"]);

				if (this.IfEdge.IsConnected) {
					CBlock b = this.IfEdge.LinkedTo.Block;
					b.ToPython(o, indent+1);
				} else {
					string sindent2 = new string (' ', Constant.SPACES * (indent+1));
					o.AppendFormat ("{0}pass\n", sindent2);
				}
				
				if (this.OutEdge.IsConnected) {
					CBlock b = this.OutEdge.LinkedTo.Block;
					b.ToPython(o, indent);
//				} else {
//					string sindent2 = new string (' ', Constant.SPACES * (indent+1));
//					o.AppendFormat ("{0}pass\n", sindent2);
				}
				
			} catch (Exception ex){
				Console.WriteLine("{0} (in CControlIf.ToPython)", ex.Message);
				return false;
			}
			
			return true;
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
				if (tname == "if") {
					e = IfEdge;
				}
			}
			return e;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override bool Compile(ScriptEngine engine, Jigsaw.Canvas cvs)
		{
			// Executing an if-block involves evaluting the given exression
			CExpressionProperty IfTest = (CExpressionProperty)_properties["IfTest"];
			try {
				IfTest.Compile(engine);
			} catch (Exception ex) {
				Console.WriteLine ("Block {0} failed compilation: {1}", this.Name, ex.Message);
				return false;
			}
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override IEnumerator<RunnerResponse> Runner(ScriptScope scope, CallStack stack) 
		{	// Execute a conditional
			
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
			
			// Do the if-test
			bool doIf = false;

			// Start by evaluating the if expression
			try {
				CExpressionProperty IfTest = (CExpressionProperty)_properties["IfTest"];
				object otest = IfTest.Evaluate(scope);
				doIf = (bool)otest;

			} catch (Exception ex) {
				this["Message"] = ex.Message;
				this.State = RunningState.Error;
				rr.Action = EngineAction.Error;
				rr.Frame = null;
			}
			// Go into a loop while block remains in an error state
			while (this.State == RunningState.Error) yield return rr;
			
			// Execute the repetition
			if ( doIf )
			{	// Execute enclosed stack
				if (this.IfEdge.IsConnected) {
					rr.Action = EngineAction.Add;
					rr.Frame = this.IfEdge.LinkedTo.Block.Frame(scope, stack);
					yield return rr;
				}
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
        public CControlIfElse(Double X, Double Y, Widgets.CBlockPalette palette = null) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] {
				new Diagram.CPoint(X, Y), 
				new Diagram.CPoint(X + CBlock.BlockWidth, Y + 80)}),
				palette)
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
		
		public CControlIfElse(Double X, Double Y) : this(X, Y, null) {}
		
		// - - - -Clone this block. Optionally clone edges. - - - - - - - - -
		public override CBlock Clone(double X, double Y, bool cloneEdges) 
		{
			// This special version is necessary due to two internal block stacks.
			
			CControlIfElse clone = (CControlIfElse)base.Clone(X, Y, cloneEdges);
			
			// Set internal heights of internal block stacks
			clone._ibh = this._ibh;
			clone._ebh = this._ebh;
			
			return (CBlock)clone;
		}

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs e)
		{	// Update text when property changes
			
			this.Text = String.Format("if `{0}`", this["IfTest"]);
			RaiseBlockChanged();
		}
		
		// - - - Generate and return Python if statement - - - - -
		public override bool ToPython (StringBuilder o, int indent)
		{
			try
			{
				string sindent = new string (' ', Constant.SPACES * indent);
				o.AppendFormat ("{0}if {1}:\n", sindent, this["IfTest"]);

				if (this.IfEdge.IsConnected) {
					CBlock b = this.IfEdge.LinkedTo.Block;
					b.ToPython(o, indent+1);
				} else {
					string sindent2 = new string (' ', Constant.SPACES * (indent+1));
					o.AppendFormat ("{0}pass\n", sindent2);
				}
				
				o.AppendFormat ("{0}else:\n", sindent);
				
				if (this.ElseEdge.IsConnected) {
					CBlock b = this.ElseEdge.LinkedTo.Block;
					b.ToPython(o, indent+1);
				} else {
					string sindent2 = new string (' ', Constant.SPACES * (indent+1));
					o.AppendFormat ("{0}pass\n", sindent2);
				}
				
				if (this.OutEdge.IsConnected) {
					CBlock b = this.OutEdge.LinkedTo.Block;
					b.ToPython(o, indent);
//				} else {
//					string sindent2 = new string (' ', 2*(indent+1));
//					o.AppendFormat ("{0}pass\n", sindent2);
				}
				
			} catch (Exception ex){
				Console.WriteLine("{0} (in CControlIfElse.ToPython)", ex.Message);
				return false;
			}
			
			return true;
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
		public override bool Compile(ScriptEngine engine, Jigsaw.Canvas cvs)
		{
			// Executing an if-block involves evaluting the given exression
			CExpressionProperty IfTest = (CExpressionProperty)_properties["IfTest"];
			try {
				IfTest.Compile(engine);
			} catch (Exception ex) {
				Console.WriteLine ("Block {0} failed compilation: {1}", this.Name, ex.Message);
				return false;
			}
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override IEnumerator<RunnerResponse> Runner(ScriptScope scope, CallStack stack) 
		{	// Execute a simple repetition
			
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
			
			// Do the if-else-test
			bool doIf = false;

			// Start by evaluating the if expression
			try {
				CExpressionProperty IfTest = (CExpressionProperty)_properties["IfTest"];
				object otest = IfTest.Evaluate(scope);
				doIf = (bool)otest;

			} catch (Exception ex) {
				this["Message"] = ex.Message;
				this.State = RunningState.Error;
				rr.Action = EngineAction.Error;
				rr.Frame = null;
			}
			// Go into a loop while block remains in an error state
			while (this.State == RunningState.Error) yield return rr;
			
			// Execute the if-part or else-part
			if ( doIf )
			{	// Execute enclosed stack
				if (this.IfEdge.IsConnected) {
					rr.Action = EngineAction.Add;
					rr.Frame = this.IfEdge.LinkedTo.Block.Frame(scope, stack);
					yield return rr;
				}
			} else {
				if (this.ElseEdge.IsConnected) {
					rr.Action = EngineAction.Add;
					rr.Frame = this.ElseEdge.LinkedTo.Block.Frame(scope, stack);
					yield return rr;
				}
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
			
            //double w = this.width;
            //double h = this.height;
			
			// Use base class method to draw top label
			base.DrawLabels(g);
			
			// Add else-part
			g.MoveTo(x + 10.0, y + 20.0 + 3.0 + _ibh + _textYOffset);
			g.ShowText("else");

//			double cx = x + 0.5*w;
//			double cy = y + 0.5*20;
//			g.Color = this.TextColor;
//
//			double teHeight, teWidth;
//			int layoutWidth, layoutHeight;
//
//			Pango.Layout layout = Pango.CairoHelper.CreateLayout(g);
//			Pango.FontDescription desc = Pango.FontDescription.FromString(
//					   String.Format("{0} {1} {2}", this.fontFace, this.fontWeight, this.fontSize));
//			layout.FontDescription = desc;
//			layout.Alignment = Pango.Alignment.Left; //Center;
//
//			if (this.Text.Length > 0)
//            {
//				layout.SetText(text);
//				layout.GetSize(out layoutWidth, out layoutHeight);
//				teHeight = (double)layoutHeight / Pango.Scale.PangoScale; 
//				teWidth = (double)layoutWidth / Pango.Scale.PangoScale;
//				//g.MoveTo(cx - 0.5*teWidth, cy - 0.5*teHeight + textYOffset);
//				g.MoveTo(x+10.0, y+3.0+_textYOffset);
//				Pango.CairoHelper.ShowLayout(g, layout);
//            }
//			
//			layout.SetText("else");
//			layout.GetSize(out layoutWidth, out layoutHeight);
//			teHeight = (double)layoutHeight / Pango.Scale.PangoScale; 
//			teWidth = (double)layoutWidth / Pango.Scale.PangoScale;
//			//g.MoveTo(cx - 0.5*teWidth, cy - 0.5*teHeight + textYOffset+20+_ibh);
//			g.MoveTo(x + 10.0, y + 3.0 + _textYOffset + 20 + _ibh);
//			Pango.CairoHelper.ShowLayout(g, layout);
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
        public CControlWhile(Double X, Double Y, Widgets.CBlockPalette palette = null) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] {
				new Diagram.CPoint(X, Y), 
				new Diagram.CPoint(X + CBlock.BlockWidth, Y + 50) }),
				palette) 
		{
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkGoldenrod;
			this.FillColor = Diagram.Colors.PaleGoldenrod;
			this.Sizable = false;
			this._breakStop = true;
			
			// Create inner edge to connect loop stack
			double offsetX = 0.5*this.Width + 10.0;
			LoopEdge = new CEdge(this, "Loop", EdgeType.Out, null, offsetX, 20.0, 20.0, 20.0, this.Width-20.0);
			
			// Properties
			CExpressionProperty WhileTest = new CExpressionProperty("WhileTest", "True");
			WhileTest.PropertyChanged += OnPropertyChanged;
			_properties["WhileTest"] = WhileTest;
			this.OnPropertyChanged(null, null);
		}
		
		public CControlWhile(Double X, Double Y) : this(X, Y, null) {}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs e)
		{	// Update text when property changes
			this.Text = String.Format("while `{0}`", this["WhileTest"]);
			RaiseBlockChanged();
		}
		
		// - - - Generate and return Python if statement - - - - -
		public override bool ToPython (StringBuilder o, int indent)
		{
			try
			{
				string sindent = new string (' ', Constant.SPACES * indent);
				o.AppendFormat ("{0}while ({1}):\n", sindent, this["WhileTest"]);

				if (this.LoopEdge.IsConnected) {
					CBlock b = this.LoopEdge.LinkedTo.Block;
					b.ToPython(o, indent+1);
				} else {
					string sindent2 = new string (' ', Constant.SPACES * (indent+1));
					o.AppendFormat ("{0}pass\n", sindent2);
				}
				
				if (this.OutEdge.IsConnected) {
					CBlock b = this.OutEdge.LinkedTo.Block;
					b.ToPython(o, indent);
//				} else {
//					string sindent2 = new string (' ', 2*(indent+1));
//					o.AppendFormat ("{0}pass\n", sindent2);
				}
			
			} catch (Exception ex){
				Console.WriteLine("{0} (in CControlWhile.ToPython)", ex.Message);
				return false;
			}
			
			return true;
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
		public override bool Compile(ScriptEngine engine, Jigsaw.Canvas cvs)
		{
			// Executing an if-block involves evaluting the given exression
			CExpressionProperty WhileTest = (CExpressionProperty)_properties["WhileTest"];
			try {
				WhileTest.Compile(engine);
			} catch (Exception ex) {
				Console.WriteLine ("Block {0} failed compilation: {1}", this.Name, ex.Message);
				return false;
			}
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override IEnumerator<RunnerResponse> Runner(ScriptScope scope, CallStack stack) 
		{	// Execute a simple repetition
			
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
			// Do the while loop
			bool doWhile = false;
			while(true) {

				try {
					CExpressionProperty WhileTest = (CExpressionProperty)_properties["WhileTest"];
					object otest = WhileTest.Evaluate(scope);
					doWhile = (bool)otest;
	
				} catch (Exception ex) {
					this["Message"] = ex.Message;
					this.State = RunningState.Error;
					rr.Action = EngineAction.Error;
					rr.Frame = null;
				}
				
				// Go into a loop while block remains in an error state
				while (this.State == RunningState.Error) yield return rr;
			
				// Next perform one iteration of the enclosed stack
				if (doWhile == true) {
					
					if (this.LoopEdge.IsConnected) {
						// Push internal runner on to stack
						// Pops itself off stack when done
						rr.Action = EngineAction.Add;
						rr.Frame = this.LoopEdge.LinkedTo.Block.Frame(scope, stack);
					} else {
						rr.Action = EngineAction.NoAction;
					}
					yield return rr;
				} 
				else 
				{	// Break out of internal while loop
					break;
				}

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
    public class CControlBreak : CBlock
    {	// Break block shape class
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CControlBreak(Double X, Double Y, Widgets.CBlockPalette palette = null) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y), 
				new Diagram.CPoint(X + CBlock.BlockWidth, Y + 20)}),
				palette) 
		{
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkGoldenrod;
			this.FillColor = Diagram.Colors.PaleGoldenrod;
			this.Sizable = false;
			this.Text = "exit loop";
		}
		
		public CControlBreak(Double X, Double Y) : this(X, Y, null) {}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override List<CEdge> Edges 
		{	// Return a list of all edges
			// Break blocks only have an input edge
			get {
				return new List<CEdge>() { this.InEdge };
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override IEnumerator<RunnerResponse> Runner(ScriptScope scope, CallStack stack) 
		{	// Break from a loop or conditional
			
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
			// Go into a loop while block remains in an error state
			while (this.State == RunningState.Error) yield return rr;
			
			rr.Action = EngineAction.Break;
			rr.Frame = null;
			rr.RetVal = scope;
			this.State = RunningState.Idle;
			yield return rr;

		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected override void SetPath(Cairo.Context g) 
		{
			double x = this.left;
            double y = this.top;
			
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
		
		// - - - Generate and return Python translation of a break - - - - -
		private string ToPython ()
		{
			return "break";
		}
		
		public override bool ToPython (StringBuilder o, int indent)
		{
			try
			{
				string sindent = new string (' ', Constant.SPACES * indent);
				o.AppendFormat("{0}{1}\n", sindent, this.ToPython ());
				
			} catch (Exception ex){
				Console.WriteLine(ex.Message);
				return false;
			}
			
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    }
	
	// -----------------------------------------------------------------------
    public class CControlForeach : CBlock
    {	// Repeat control block shape class
		
		// Internal edge
		public CEdge LoopEdge = null;
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CControlForeach(Double X, Double Y, Widgets.CBlockPalette palette = null) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] {
				new Diagram.CPoint(X, Y), 
				new Diagram.CPoint(X + CBlock.BlockWidth, Y + 50) }),
				palette) 
		{
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkGoldenrod;
			this.FillColor = Diagram.Colors.PaleGoldenrod;
			this.Sizable = false;
			this._breakStop = true;
			
			// Create inner edge to connect loop stack
			double offsetX = 0.5*this.Width + 10.0;
			LoopEdge = new CEdge(this, "Loop", EdgeType.Out, null, offsetX, 20.0, 20.0, 20.0, this.Width-20.0);
			
			// Properties
			CExpressionProperty Sequence = new CExpressionProperty("Sequence", "[1, 2, 3]");
			Sequence.PropertyChanged += OnPropertyChanged;
			_properties["Sequence"] = Sequence;
			CVarNameProperty Variable = new CVarNameProperty("Variable", "item");
			Variable.PropertyChanged += OnPropertyChanged;
			_properties["Variable"] = Variable;
			this.OnPropertyChanged(null, null);
		}
		
		public CControlForeach(Double X, Double Y) : this(X, Y, null) {}
		
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
		
        // - - - Update text when property changes - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs e)
		{
			this.Text = String.Format("for each `{0}` in `{1}`", this["Variable"], this["Sequence"]);
			RaiseBlockChanged();
		}
		
		// - - - Generate and return Python if statement - - - - -
		public override bool ToPython (StringBuilder o, int indent)
		{
			try
			{
				string sindent = new string (' ', Constant.SPACES * indent);
				o.AppendFormat ("{0}for {1} in {2}:\n", sindent, this["Variable"], this["Sequence"]);

				if (this.LoopEdge.IsConnected) {
					CBlock b = this.LoopEdge.LinkedTo.Block;
					b.ToPython(o, indent+1);
				} else {
					string sindent2 = new string (' ', Constant.SPACES * (indent+1));
					o.AppendFormat ("{0}pass\n", sindent2);
				}
			
				if (this.OutEdge.IsConnected) {
					CBlock b = this.OutEdge.LinkedTo.Block;
					b.ToPython(o, indent);
//				} else {
//					string sindent2 = new string (' ', 2*(indent+1));
//					o.AppendFormat ("{0}pass\n", sindent2);
				}
				
			} catch (Exception ex){
				Console.WriteLine("{0} (in CControlForeach.ToPython)", ex.Message);
				return false;
			}
			
			return true;
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override bool Compile(ScriptEngine engine, Jigsaw.Canvas cvs)
		{
			// Executing an if-block involves evaluting the given exression
			CExpressionProperty Sequence = (CExpressionProperty)_properties["Sequence"];
			try {
				Sequence.Compile(engine);
			} catch (Exception ex) {
				Console.WriteLine ("Block {0} failed compilation: {1}", this.Name, ex.Message);
				return false;
			}
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override IEnumerator<RunnerResponse> Runner(ScriptScope scope, CallStack stack) 
		{	// Execute a for-each
			
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
			CExpressionProperty Sequence = (CExpressionProperty)_properties["Sequence"];
			IEnumerable sequence = null;
			try {
				sequence = (IEnumerable)Sequence.Evaluate(scope);
			} catch (Exception ex) {
				this["Message"] = ex.Message;
                this.State = RunningState.Error;
                rr.Action = EngineAction.Error;
                rr.Frame = null;
			}
			
			// Go into a loop while block remains in an error state
			while (this.State == RunningState.Error) yield return rr;
			
			// Execute the repetition
			foreach (object item in sequence)
			{
				CVarNameProperty VarName = (CVarNameProperty)_properties["Variable"];
				SetVariable(scope, VarName.Text, item);
				//Compiler.ExecAssignment (scope, VarName.Text, item);

				// Next perform one iteration of the enclosed stack
				if (this.LoopEdge.IsConnected) {
					rr.Action = EngineAction.Add;
					rr.Frame = this.LoopEdge.LinkedTo.Block.Frame(scope, stack);
					yield return rr;
				}
				
				// At this point the body of the loop has completed
				// Increment counter
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
}
