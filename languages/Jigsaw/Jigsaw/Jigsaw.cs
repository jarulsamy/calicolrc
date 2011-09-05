using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Text;
using System.Xml;
using System.IO;
using Cairo;
//using System.Reflection;

// TODO:
// Allow the out-edge of a complete stack (not just single blocks) being dragged
//   to activate and connect to unconnected in-edges of stationary blocks/stacks.
// Stop Script blocks should not attach to inner blocks
// Block output edge should activate when near an input edge
// Can Graphics.Rendering.Pango.Markup be used to render stylized text in a Block?
// Implement Delete Stack context menu item
// Add IO blocks [Write to file], [Add to file], [Read from file]

// Model:
// Blocks have one or more edges (connection points). 
// Edges have activation zones and activating points.
// Activating points, when dragged over an edge's activation zone, activate that edge.
// Only one edge can/should be activated at a time. This applies to the entire diagram.
// When a block is dropped, if an edge is activated, a connection is made.

namespace Jigsaw
{
	// -----------------------------------------------------------------------
	
	/// <summary>
	/// Enumerates block edge types
	/// </summary>
	public enum EdgeType
	{
		In = 1, Out = 2
	}

	public enum BlockState
	{
		Idle = 1, Running = 2, Error = 3
	}
	
	// -----------------------------------------------------------------------
	
	/// <summary>
	/// Subclass of Canvas that adds custom behavior 
	/// </summary>
	public class Canvas : Diagram.Canvas
	{	
		// _currentPath holds the path to the currently open program file.
		// If _currentPath holds a value of null, no file is open.
		// _isModified is set to true whenever something is done that requires the 
		// current program to be saved. It is used to check with the user when the 
		// program is attempted to be closed with unsaved changes.
		private string _currentPath = null;
		private bool _isModified = false;
		
		// References to button that are accessed in order to enable/disable them at runtime.
		private Widgets.CRoundedButton bStop;
		private Widgets.CRoundedButton bRun;
		
		// Reference to single PropertiesWindow object
		private InspectorWindow _inspector = null;
		
		internal int _X;		// Cache
		internal int _Y;
		
		// A private reference to the engine that runs Jigsaw programs.
		private Engine _engine = null;
		
		/// <summary>
		/// Constructor 
		/// </summary>
		/// <param name="width">
		/// A <see cref="System.Int32"/>
		/// </param>
		/// <param name="height">
		/// A <see cref="System.Int32"/>
		/// </param>
		public Canvas(int width, int height) : base(width, height) 
		{
			// Setup some colors
			BackColor = Diagram.Colors.LightSlateGray;
			this.lasso.LineColor = Diagram.Colors.WhiteSmoke;
			
			// Properties window shared by all blocks
			_inspector = new Jigsaw.InspectorWindow(this);
			
			// Engine to run Jigsaw programs
			_engine = new Engine();
			_engine.EngineRun   += OnEngineRun;
			_engine.EngineStep  += OnEngineStep;
			_engine.EngineStop  += OnEngineStop;
			_engine.EngineReset += OnEngineReset;
			_engine.Reset(this, _inspector);
			
			// Set up all widgets
			
			// Build tabbed panel for blocks
			Widgets.CRoundedTab tbCtrl     = new Widgets.CRoundedTab(0,  67, 100, 30, "Control");
			Widgets.CRoundedTab tbVars     = new Widgets.CRoundedTab(0, 100, 100, 30, "Variables");
			Widgets.CRoundedTab tbInOut    = new Widgets.CRoundedTab(0, 133, 100, 30, "Input/Output");
			Widgets.CRoundedTab tbMyro     = new Widgets.CRoundedTab(0, 166, 100, 30, "Myro");
			Widgets.CRoundedTab tbGraphics = new Widgets.CRoundedTab(0, 199, 100, 30, "Graphics");
			Widgets.CRoundedTab tbTools    = new Widgets.CRoundedTab(0, 298, 100, 30, "Tools");
			Widgets.CRoundedTab tbNotes    = new Widgets.CRoundedTab(0, 331, 100, 30, "Notes");
			
			tbInOut.AddTabs(    new List<Widgets.CRoundedTab>() {tbMyro,  tbVars, tbCtrl, tbTools,  tbNotes, tbGraphics});
			tbCtrl.AddTabs(     new List<Widgets.CRoundedTab>() {tbInOut, tbMyro, tbVars, tbTools,  tbNotes, tbGraphics});
			tbVars.AddTabs(     new List<Widgets.CRoundedTab>() {tbInOut, tbMyro, tbCtrl, tbTools,  tbNotes, tbGraphics});
			tbMyro.AddTabs(     new List<Widgets.CRoundedTab>() {tbInOut, tbCtrl, tbVars, tbTools,  tbNotes, tbGraphics});
			tbGraphics.AddTabs( new List<Widgets.CRoundedTab>() {tbInOut, tbMyro, tbCtrl, tbVars,   tbTools,  tbNotes});
			tbTools.AddTabs(    new List<Widgets.CRoundedTab>() {tbInOut, tbMyro, tbCtrl, tbVars,   tbNotes, tbGraphics});
			tbNotes.AddTabs(    new List<Widgets.CRoundedTab>() {tbInOut, tbMyro, tbCtrl, tbTools,  tbVars,   tbGraphics});
			
			// Add tabs to the canvas
			this.AddShape(tbCtrl);
			this.AddShape(tbVars);
			this.AddShape(tbInOut);
			this.AddShape(tbMyro);
			this.AddShape(tbGraphics);
			this.AddShape(tbTools);
			this.AddShape(tbNotes);
			
			// Add block panel background to canvas
			Diagram.CRectangle pnlBlock = new Diagram.CRectangle(
			    new List<Diagram.CPoint>() {new Diagram.CPoint(95.0, 0.0), new Diagram.CPoint(300.0, 1200.0)}, 
				"", Diagram.Colors.Transparent, Diagram.Colors.Honeydew, 1, Diagram.Colors.Honeydew, 
				true, false, false, false, false);
			this.AddShape(pnlBlock);
			
			// Factory Blocks for block area
			
			// Control
			CControlStart block20 = new CControlStart(110, 70, true);
			this.AddShape(block20);
			tbCtrl.AddShape(block20);

			CControlIf block21 = new CControlIf(110, 120, true);
			this.AddShape(block21);
			tbCtrl.AddShape(block21);

			CControlRepeat block22 = new CControlRepeat(110, 190, true);
			this.AddShape(block22);
			tbCtrl.AddShape(block22);
			
			CControlEnd block23 = new CControlEnd(110, 260, true);
			this.AddShape(block23);
			tbCtrl.AddShape(block23);
			
			// IO
			CIOPrint _cioprint = new CIOPrint(110, 70, true);
			this.AddShape(_cioprint);
			tbInOut.AddShape(_cioprint);
			
			CIOWriteToFile _ciowritefile = new CIOWriteToFile(110, 110, true);
			this.AddShape(_ciowritefile);
			tbInOut.AddShape(_ciowritefile);
			
			// Shapes
			Diagram.CRectangle _shrect = new Diagram.CRectangle(130, 70, 135, 30);
			_shrect.FillColor = Diagram.Colors.LightYellow;
			_shrect.LineColor = Diagram.Colors.Gray;
			_shrect.LineWidth = 2;
			_shrect._isFactory = true;
			this.AddShape(_shrect);
			tbNotes.AddShape(_shrect);

			Diagram.CRoundedRectangle _shrrect = new Diagram.CRoundedRectangle(130, 115, 135, 30);
			_shrrect.FillColor = Diagram.Colors.LightYellow;
			_shrrect.LineColor = Diagram.Colors.Gray;
			_shrrect.LineWidth = 2;
			_shrrect.Radius = 8;
			_shrrect._isFactory = true;
			this.AddShape(_shrrect);
			tbNotes.AddShape(_shrrect);
			
			Diagram.CEllipse _shellipse = new Diagram.CEllipse(130, 160, 135, 30);
			_shellipse.FillColor = Diagram.Colors.LightYellow;
			_shellipse.LineColor = Diagram.Colors.Gray;
			_shellipse.LineWidth = 2;
			_shellipse._isFactory = true;
			this.AddShape(_shellipse);
			tbNotes.AddShape(_shellipse);
			
//			Diagram.CConnector _shconn = new Diagram.CConnector(
//			                             	new List<Diagram.CPoint>() { 
//												new Diagram.CPoint(130, 210), 
//												new Diagram.CPoint(198, 210),
//												new Diagram.CPoint(198, 230),
//												new Diagram.CPoint(266, 230) } );
//			_shconn.LineColor = Diagram.Colors.LightBlue;
//			_shconn.LineWidth = 2;
//			_shconn.FillColor = Diagram.Colors.Transparent;
//			_shconn._isFactory = true;
//			this.AddShape(_shconn);
//			tbNotes.AddShape(_shconn);

			// Myro
			CRobot block1 = new CRobot(110, 70, true);
			block1.Text = "init robot on [COM]";
			this.AddShape(block1);
			tbMyro.AddShape(block1);
	
			CRobot block2 = new CRobot(110, 110, true);
			block2.Text = "forward [by] for [secs]";
			this.AddShape(block2);
			tbMyro.AddShape(block2);
	
			CRobot block3 = new CRobot(110, 150, true);
			block3.Text = "backward [by] for [secs]";
			this.AddShape(block3);
			tbMyro.AddShape(block3);
			
			CRobot block4 = new CRobot(110, 190, true);
			block4.Text = "turn left [by] for [secs]";
			this.AddShape(block4);
			tbMyro.AddShape(block4);
	
			CRobot block5 = new CRobot(110, 230, true);
			block5.Text = "turn right [by] for [secs]";
			this.AddShape(block5);
			tbMyro.AddShape(block5);
		
//			Widgets.CTextBox tb1 = new Widgets.CTextBox(110, 70, 100, 25, "Blah", cvsFixed);
//			cvs.AddShape(tb1);
//			tbProp.AddShape(tb1);
//			
//			Blocks.CRobot block6 = new Blocks.CRobot(100, 380, true);
//			block6.Text = "stop robot";
//			cvs.AddShape(block6);
//			
//			Blocks.CRobot block7 = new Blocks.CRobot(100, 420, true);
//			block7.Text = "take picture";
//			cvs.AddShape(block7);
//			
//			Blocks.CRobot block8 = new Blocks.CRobot(100, 460, true);
//			block8.Text = "save picture to [file]";
//			cvs.AddShape(block8);
//			
//			Blocks.CRobot block9 = new Blocks.CRobot(100, 500, true);
//			block9.Text = "beep";
//			cvs.AddShape(block9);
//			
//			Blocks.CRobot block10 = new Blocks.CRobot(100, 540, true);
//			block10.Text = "speak [message]";
//			cvs.AddShape(block10);
			
			// Variable Blocks
			CAssignment vblock1 = new CAssignment(110, 70, true);
			this.AddShape(vblock1);
			tbVars.AddShape(vblock1);
			
			// Run tab
			bRun = new Widgets.CRoundedButton(150, 70, 100, 25, "Auto-Step");
			bRun.MouseDown += OnRunMouseDown;
			this.AddShape(bRun);
			tbTools.AddShape(bRun);
			
			bStop = new Widgets.CRoundedButton(150, 110, 100, 25, "Stop");
			bStop.Enabled = false;
			bStop.MouseDown += OnStopMouseDown;
			this.AddShape(bStop);
			tbTools.AddShape(bStop);
			
			Widgets.CRoundedButton bReset = new Widgets.CRoundedButton(150, 150, 100, 25, "Reset");
			bReset.MouseDown += OnResetMouseDown;
			this.AddShape(bReset);
			tbTools.AddShape(bReset);
			
			Widgets.CRoundedButton bStep = new Widgets.CRoundedButton(150, 190, 100, 25, "Step");
			bStep.MouseDown += OnStepMouseDown;
			this.AddShape(bStep);
			tbTools.AddShape(bStep);

			// Select first tab
			tbCtrl.SetToggle(this, true);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Manage the current path to the Jigsaw program
		public string CurrentPath {
		
			get {
				return _currentPath;
			}
			set {
				_currentPath = value;
				if (_currentPath != null)
					_inspector.Title = "Jigsaw Inspector - " + System.IO.Path.GetFileName(_currentPath);
				else
					_inspector.Title = "Jigsaw Inspector";
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		void OnResetMouseDown(Diagram.CShape shp, Diagram.ShapeEventArgs e)
		{
			_engine.Reset(this, _inspector);
			_inspector.ClearLocals();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		void OnStepMouseDown(Diagram.CShape shp, Diagram.ShapeEventArgs e)
		{
			// Step the engine
			_engine.Step();
			
			// Update the locals display
			_inspector.DisplayLocals(_engine);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		void OnStopMouseDown(Diagram.CShape shp, Diagram.ShapeEventArgs e)
		{
			_engine.Stop();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		void OnRunMouseDown(Diagram.CShape shp, Diagram.ShapeEventArgs e)
		{
			if (_engine.IsRunning == false) _engine.Reset(this, _inspector);
			_engine.Run();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		void OnEngineRun(object sender, EventArgs e)
		{	
			bRun.Enabled = false;
			bStop.Enabled = true;
			this.Invalidate();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		void OnEngineStep(object sender, EventArgs e)
		{	
			this.Invalidate();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		void OnEngineReset(object sender, EventArgs e)
		{	
			// What else to do here?
			this.Invalidate();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		void OnEngineStop(object sender, EventArgs e)
		{	
			bStop.Enabled = false;
			bRun.Enabled = true;
			this.Invalidate();
		}
		
		/// <summary>
		/// Return a list of all non-factory blocks  
		/// </summary>
		/// <returns>
		/// A <see cref="List<CBlock>"/>
		/// </returns>
		public List<CBlock> AllBlocks(){
			List<CBlock> blocks = new List<CBlock>();
			
			foreach (Diagram.CShape s in this.AllShapes()) {
				if (s is CBlock) {
					CBlock b = (CBlock)s;
					if (!b.IsFactory) {
						blocks.Add(b);
					}
				}
			}
			return blocks;
		}
		
		/// <summary>
		/// Properly delete given block 
		/// </summary>
		/// <param name="b">
		/// A <see cref="CBlock"/>
		/// </param>
		public void DeleteBlock(CBlock b) {
			Diagram.Canvas cvs = (Diagram.Canvas)this;
			b.Deselect(cvs);
			b.Deactivate(cvs);
			b.Disconnect();
			cvs.DeleteShape(b);
		}
		
		/// <summary>
		/// Delete all blocks in preparation for a new program 
		/// </summary>
		public void DeleteAllBlocks() {
			Diagram.Canvas cvs = (Diagram.Canvas)this;
			foreach (CBlock b in AllBlocks()) {
				b.Deselect(cvs);
				b.Deactivate(cvs);
				b.Disconnect();
				cvs.DeleteShape(b);
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void OnMouseDown(Diagram.Canvas cvs, Diagram.MouseEventArgs e)
        {
			// Intercept right-mouse on canvas
			if (e.Button == Diagram.MouseButtons.Right) 
			{
				this.ShowContextMenu(cvs, (int)e.X, (int)e.Y);
				return;
			}
			
			base.OnMouseDown(cvs, e);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected bool ShowContextMenu(Diagram.Canvas cvs, int X, int Y) {
			// Create the context menu for this block
			
			// Cache info
			_X = X;
			_Y = Y;
	
			// Create and show context menu
			Gtk.Menu mnu = new Gtk.Menu();
			
			Gtk.MenuItem mnuNew = new Gtk.MenuItem("New");
			mnuNew.Activated += OnNewFile;
			Gtk.MenuItem mnuOpen = new Gtk.MenuItem("Open...");
			mnuOpen.Activated += OnOpenFile;
			Gtk.MenuItem mnuSave = new Gtk.MenuItem("Save");
			mnuSave.Activated += OnSave;
			Gtk.MenuItem mnuSaveAs = new Gtk.MenuItem("Save As...");
			mnuSaveAs.Activated += OnSaveAs;
			Gtk.MenuItem mnuProps = new Gtk.MenuItem("Inspect");
			mnuProps.Activated += OnInspectorShow;
			
			mnu.Append(mnuNew);
			mnu.Append(mnuOpen);
			mnu.Append( new Gtk.SeparatorMenuItem() );
			mnu.Append(mnuSave);
			mnu.Append(mnuSaveAs);
			mnu.Append( new Gtk.SeparatorMenuItem() );
			mnu.Append(mnuProps);
			
			mnu.ShowAll();
			mnu.Popup();
			
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected void OnInspectorShow(object sender, EventArgs e)
		{
			this.ShowInspectorWindow();
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void ShowInspectorWindow()
		{
			_inspector.ShowAll();
			_inspector.SetPosition(Gtk.WindowPosition.Mouse);
			_inspector.KeepAbove = true;	// The Mono 2.6.7 runtime needs this here for the Window to stay above others
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected void OnSave(object sender, EventArgs e)
		{	// Save to current file
			
			// If no current file, go to save as and request file path
			if (_currentPath == null || _currentPath.Trim().Length == 0 ) {
				OnSaveAs(sender, e);
			} else {
				XmlWriter xw = new XmlTextWriter(_currentPath, Encoding.ASCII);
				this.ToXml(xw);
				xw.Close();
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected void OnSaveAs(object sender, EventArgs e)
		{
			// Set up and show the file chooer
			Gtk.FileChooserDialog fc = null;
			fc = new Gtk.FileChooserDialog("Save as Jigsaw file...", null,
			                               Gtk.FileChooserAction.Save,
			                               "Cancel", Gtk.ResponseType.Cancel,
			                               "Save",   Gtk.ResponseType.Accept);
			Gtk.FileFilter f1 = new Gtk.FileFilter();
			f1.Name = "XML files";
			f1.AddPattern("*.xml");
			Gtk.FileFilter f2 = new Gtk.FileFilter();
			f2.Name = "All files";
			f2.AddPattern("*.*");
			fc.AddFilter(f1);
			fc.AddFilter(f2);
			if ( _currentPath != null ) fc.SetFilename(_currentPath);
			
			// Collect the path
			int response = fc.Run();
			
			// Save the path and go to OnSave for the actual save
			if (response == (int)Gtk.ResponseType.Accept) 
			{
				CurrentPath = fc.Filename;
				fc.Destroy();
				OnSave(sender, e);
				
			} else {
				// Must call Destroy() to close FileChooserDialog window.
				fc.Destroy();
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected void OnNewFile(object sender, EventArgs e)
		{
			this.DeleteAllBlocks();
			CurrentPath = null;
			this.Invalidate();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Open a file and recreate the jigsaw program
		protected void OnOpenFile(object sender, EventArgs e)
		{
			Gtk.FileChooserDialog fc = null;
			fc = new Gtk.FileChooserDialog("Jigsaw file to open", null,
			                               Gtk.FileChooserAction.Open,
			                               "Cancel", Gtk.ResponseType.Cancel,
			                               "Open",   Gtk.ResponseType.Accept);
			Gtk.FileFilter f1 = new Gtk.FileFilter();
			f1.Name = "XML files";
			f1.AddPattern("*.xml");
			Gtk.FileFilter f2 = new Gtk.FileFilter();
			f2.Name = "All files";
			f2.AddPattern("*.*");
			fc.AddFilter(f1);
			fc.AddFilter(f2);
			
			if (fc.Run() == (int)Gtk.ResponseType.Accept) 
			{
				// Temp vars to hold parse vals
				string name;
				string val;
				string typeName;
				int X;
				int Y;
				int id;
				int edgeId;
				CBlock tBlock = null;
				CEdge tEdge = null;
				CEdge tLinkedEdge = null;
				
				// Start by wiping out what's there
				this.DeleteAllBlocks();
				CurrentPath = fc.Filename;
				
				// === First Step : Read XML and ceate all blocks 
				//     and build dictionaries of CBlock and CEdge references
				Dictionary<string,CBlock> blocks = new Dictionary<string, CBlock>();
				Dictionary<string,CEdge> edges = new Dictionary<string, CEdge>();
				
				XmlReader xr = null;

				xr = new XmlTextReader(fc.Filename);
				
				while (xr.Read()) {
					
					switch (xr.NodeType) {
					case XmlNodeType.Element:
						name = xr.Name.ToLower();
						
						switch (name) {
						case "jigsaw":
							break;
						
						case "block":		// <block id="1" typeName="Jigsaw.CControlStart" left="415" top="50">							
							typeName = xr.GetAttribute("typeName");
							X = int.Parse(xr.GetAttribute("left"));
							Y = int.Parse(xr.GetAttribute("top"));
							id = int.Parse(xr.GetAttribute("id"));
							Type typ = Type.GetType(typeName);
							System.Object[] args = new System.Object[] {X, Y};
							tBlock = (CBlock)Activator.CreateInstance(typ, args);
							blocks["b"+id.ToString()] = tBlock;
							this.AddShape(tBlock);
							break;
							
						case "edge":		// <edge id="3" name="Out" type="Out" linkedTo="4" />
							name = xr.GetAttribute("name");
							edgeId = int.Parse(xr.GetAttribute("id"));
							edges["e"+edgeId.ToString()] = tBlock.GetEdgeByName(name);
							break;
							
						case "property":	// <property name="variable" value="X" />
							// Add a property to the block
							name = xr.GetAttribute("name");
							val = xr.GetAttribute("value");
							tBlock.SetProperty(name, val);
							break;
						}
						
						break;
						
					case XmlNodeType.Text:
						//string content;
						//content = xr.Value;
						//Console.WriteLine("content {0}", content);
						break;
						
					case XmlNodeType.EndElement:
						name = xr.Name.ToLower();

						switch (name) {
						case "jigsaw":
							break;
						case "block":
							tBlock = null;
							break;
						}
						
						break;
					}
				}
				
				xr.Close();
				xr = null;
				
				// === Second step : Reread XML and link up edges
				xr = new XmlTextReader(fc.Filename);

				while (xr.Read()) {
					
					if (xr.NodeType == XmlNodeType.Element) {
						name = xr.Name.ToLower();
						
						if (name == "block") {
							// Get a reference to the current block
							id = int.Parse(xr.GetAttribute("id"));
							tBlock = blocks["b"+id.ToString()];

						} else if (name == "edge") {
							// Link block edges
							edgeId = int.Parse(xr.GetAttribute("linkedTo"));
							if (edgeId > 0) {
								id = int.Parse(xr.GetAttribute("id"));
								tEdge = edges["e"+id.ToString()];
								tLinkedEdge = edges["e"+edgeId.ToString()];
								tEdge.LinkedTo = tLinkedEdge;
								tLinkedEdge.LinkedTo = tEdge;
							}
						}
					}
				}
				
				xr.Close();
				xr = null;
				
				// === Third step : reposition all blocks at top of stacks
				foreach (CBlock b in blocks.Values) {
					if (!b.InEdge.IsConnected)
						b.RepositionBlocks(null);
				}
			
			}
			
			// Must call Destroy() to close FileChooserDialog window.
			fc.Destroy();
			
			// Update screen
			this.Invalidate();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Completely replace serializtion of Jigsaw Program
        public override void ToXml(XmlWriter w)
        {
            w.WriteStartElement("jigsaw");

            // Assign temporary ids 
			List<CBlock> blocks = this.AllBlocks();
            int idCount = 0;
			int eIdCount = 0;
			
            foreach (CBlock b in blocks)
            {	// Assign temp id to block
                idCount++;
                b._id = idCount;
				
				// Assign temp ids to all edges
				foreach (CEdge e in b.Edges) {
					eIdCount++;
					e._id = eIdCount;
				}
            }
			
			// Write all blocks
            foreach (CBlock b in blocks)
            {	// Write XML
                b.ToXml(w);
			}
			
            // Must close by resetting all temp ids to 0 (unassigned)
            // otherwise subsequent save may be incorrect.
            foreach (CBlock b in blocks)
            {
                b._id = 0;
				foreach (CEdge e in b.Edges) e._id = 0;
            }
        }
	}
	
	// -----------------------------------------------------------------------

	/// <summary>
	/// Base block class
	/// </summary>
	public class CBlock : Diagram.CShape
	{
		protected BlockState _state = BlockState.Idle;	// Current state of this block
		public CEdge InEdge;							// By default, all Blocks have one main input edge and one main output edge
		public CEdge OutEdge;		
		
		public CReadOnlyProperty TextProp = null;		// Just a property to reflect text into Inspector
		public CReadOnlyProperty MsgProp = null;		// All blocks have a Message property

		protected int textYOffset = 0;					// Y offset for when a block's text
		
		protected bool _hasBreakPoint = false;			// True if a has a debugging break point applied
		
		protected Gtk.Window _propDialog = null;
		protected Gtk.Window _contextMenu = null;
		
		/// <summary>
		/// CBlock constructor. 
		/// </summary>
		/// <param name="pts">
		/// A <see cref="List<CPoint>"/>
		/// </param>
		public CBlock(List<Diagram.CPoint> pts) : base(pts) {
			double offsetX = 0.5*this.Width;
			double offsetY = this.Height;
			//this.AlignVertical = StringAlignment.Near;		// Change text vertical alignment to start from top
			
			// Default edges
			InEdge = new CEdge(this, "In", EdgeType.In, null, offsetX, 0.0, 0.0, 0.0, this.Width);
			OutEdge = new CEdge(this, "Out", EdgeType.Out, null, offsetX, offsetY, 0.0, this.Height, this.Width);
			
			// Default properties
			TextProp = new CReadOnlyProperty("Label", "");
			MsgProp = new CReadOnlyProperty("Message", "");
		}
		
		/// <summary>
		/// State getter/setter 
		/// </summary>
		public BlockState State {
			get { return _state;  }
			set { _state = value; }
		}
		
		/// <summary>
		/// Method to clone a CBlock at X,Y
		/// </summary>
		/// <param name="X">
		/// A <see cref="System.Double"/>
		/// </param>
		/// <param name="Y">
		/// A <see cref="System.Double"/>
		/// </param>
		/// <returns>
		/// A <see cref="CBlock"/>
		/// </returns>
		public override Diagram.CShape Clone(double X, double Y) 
		{	// Clone this CBlock
			return this.Clone(X, Y, true);
		}

		/// <summary>
		/// Method to clone a CBlock at X,Y with option to clone edges. 
		/// </summary>
		/// <param name="X">
		/// A <see cref="System.Double"/>
		/// </param>
		/// <param name="Y">
		/// A <see cref="System.Double"/>
		/// </param>
		/// <param name="cloneEdges">
		/// A <see cref="System.Boolean"/>
		/// </param>
		/// <returns>
		/// A <see cref="CBlock"/>
		/// </returns>
		public virtual CBlock Clone(double X, double Y, bool cloneEdges) 
		{	// Clone this block. Optionally clone edges.
			CBlock clone = (CBlock)base.Clone(X, Y);

			// @@@ The following line is necessary for blocks that change height 
			// @@@ depending upon embedded stacks such as control-repeat. 
			// @@@ It will fail with blocks that require more than height to size properly.
			// @@@ The Clone method should copy shape including overall size and internal sizing params.
			clone.Height = this.Height;
			
			// Clone edges if requested
			if (cloneEdges) {
				clone.InEdge = this.InEdge.Clone(clone);
				clone.OutEdge = this.OutEdge.Clone(clone);
			}
			return clone;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Override Text property of blocks to also set the TextProp value when assigned
        public override String Text
        {
            get { return this.text; }
            set {
				this.text = value;
				this.TextProp.Text = value;
			}
        }
		
		// This is a special internal method used when a program file is opened.
		// Each block subclass should override and handle their special properties by name.
		internal virtual bool SetProperty(string name, string val)
		{
			string lname = name.ToLower();
			switch(lname) {
			case "text":
				TextProp.Text = val;
				break;
			default:
				return false;
			}
			return true;
		}

		// This is a special internal method used when a program file is openned.
		// Each block subclass should override and handle their special properties by name.
		internal virtual CEdge GetEdgeByName(string name)
		{
			string tname = name.ToLower();
			
			if (tname == "in") {
				return InEdge;
			} else if (tname == "out") {
				return OutEdge;
			}
			return null;		// Not found
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void ToggleBreakPoint() {
			this.BreakPoint = !this.BreakPoint;
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public bool BreakPoint {
			get {
				return this._hasBreakPoint;
			}
			set {
				this._hasBreakPoint = value;
			}
		}
		
		/// <summary>
		/// All blocks need a Block Runner, which is an IEnumerator that executes the block's behavior.
		/// Block Runner IEnumerators return a RunnerResponse object.
		/// Block Runners are provided the local scope and builtin scope in which they run. 
		/// Base behavior only calls output blocks and manages state.  
		/// </summary>
		/// <param name="locals">
		/// A <see cref="Dictionary<System.String, System.Object>"/>
		/// </param>
		/// <param name="builtins">
		/// A <see cref="Dictionary<System.String, System.Object>"/>
		/// </param>
		/// <returns>
		/// A <see cref="IEnumerator<RunnerResponse>"/>
		/// </returns>
		public virtual IEnumerator<RunnerResponse> Runner(
								    	Dictionary<string, object> locals, 
								        Dictionary<string, object> builtins) 
		{
			// !!! Important. The engine always calls the block runner once after it is added to the call stack
			// !!! and the response is essentially ignored.
			// !!! It is important to always call these three lines at the top of every block runner 
			// !!! implementation to ensure proper operation.
			
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
			// Custom behavior will occur 
			
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
        // CBlock subclasses must provide a method that outputs an 
        // XML representation of itself to the given XmlWriter
        public override void ToXml(XmlWriter w)
        {
            w.WriteStartElement("block");
            this.WriteXmlAttributes(w); 
			this.WriteXmlTags(w);
            w.WriteEndElement();
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Write the base standard attributes shared by all shapes
        protected override void WriteXmlAttributes(XmlWriter w)
        {
            // Get object assembly and full name
            Type typ = this.GetType();
            //String FullAsmName = typ.Assembly.FullName;
            //String[] items = FullAsmName.Split(new String[] { "," }, StringSplitOptions.RemoveEmptyEntries);
            //String AsmName = items[0];
            String FullName = typ.FullName;

            w.WriteAttributeString("id", this._id.ToString());
			w.WriteAttributeString("typeName", FullName);
            //w.WriteAttributeString("typeName", this.GetType().Name);
            //w.WriteAttributeString("typeName", String.Format("{0};{1}", AsmName, FullName));
			w.WriteAttributeString("left", this.Left.ToString());
			w.WriteAttributeString("top", this.Top.ToString());
        }
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Override to write custom Xml content of a shape.
        protected override void WriteXmlTags(XmlWriter w)
        {
			foreach(CEdge e in this.Edges) e.ToXml(w);
        }
		
	 	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected virtual void WriteXmlProperty(XmlWriter w, string name, string val) {
	        w.WriteStartElement("property");
			w.WriteAttributeString("name", name);
			w.WriteAttributeString("value", val);
			w.WriteEndElement();	
		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// All blocks follow the same pattern for drawing.
		// Block subclasses only need to define the graphics path, fill color and text.
        public override void Draw(Cairo.Context g)
        {	// Draw block on the canvas
			
            // Cannot draw with negative width or height, 
            // so use bounding box points to draw
            double x = this.left;
            double y = this.top;
            double w = this.width;
            double h = this.height;
			
			g.Save();
			
            // Block outline			
			SetPath(g, x, y, w, h);
			
			// Set fill color based on block state
			if (this._state == BlockState.Running) {
				g.Color = Diagram.Colors.White;
			} else {
				g.Color = this.FillColor;
			}
			
			// Fill
			g.FillPreserve();
			
			// Stroke
			g.Color = this.LineColor;
			if (this.DashStyle != null) g.SetDash(this.DashStyle, 0.0);
			g.LineWidth = this.LineWidth;
			g.Stroke();

			// Text
            if (this.Text.Length > 0)
            {
				double cx = x + 0.5*w;
				double cy = y + 0.5*20;
				g.Color = this.TextColor;
				g.SelectFontFace(this.fontFace, this.fontSlant, this.fontWeight);
				g.SetFontSize(this.fontSize);
				Cairo.TextExtents te = g.TextExtents(this.Text);
				g.MoveTo(cx - 0.5*te.Width - te.XBearing, cy - 0.5*te.Height - te.YBearing + textYOffset); 
				g.ShowText(this.Text);
            }
			
			// If in an error state, x-out
			if (this._state == BlockState.Error) {
				g.Color = new Color(1.0, 0.0, 0.0, 0.5);
				g.LineWidth = 5;
				g.MoveTo(x, y);
				g.LineTo(x+w, y+h);
				g.MoveTo(x+w, y);
				g.LineTo(x, y+h);
				g.Stroke();
				g.Color = Diagram.Colors.LightPink;
			}
			
			// Draw breakpoint, if necessary
			if (this._hasBreakPoint) {
				g.Color = Diagram.Colors.Red;
				g.MoveTo(x+2, y+7);
				g.Arc(x+7, y+7, 5, 0.0, 2.0*Math.PI);
				g.ClosePath();
				g.Fill();
			}
			
			g.Restore();
			
            // Finally, draw any shape decorator shapes
            this.DrawDecorators(g);
        }

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Base block outline graphics path figure
		protected virtual void SetPath(Cairo.Context g, double x, double y, double w, double h) 
		{
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
			g.LineTo( x+27, y+h );
			g.LineTo( x+24, y+h+4 );
			g.LineTo( x+14, y+h+4 );
			g.LineTo( x+11, y+h );
			g.LineTo( x+r, y+h );
			g.Arc(    x+r, y+h-r, r, hpi, Math.PI );
			g.LineTo( x, y+r );
            g.ClosePath();
		}
		
		/// <summary>
		/// Returns a list of all edges. Override if subclass adds edges to block.
		/// </summary>
		public virtual List<CEdge> Edges 
		{
			get {
				return new List<CEdge>() { this.InEdge, this.OutEdge };
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Returns a list of all block properties
		public virtual List<CProperty> Properties 
		{
			get {
				return new List<CProperty>() { this.TextProp, this.MsgProp };
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CEdge StackOutEdge
		{	// Return the out-edge from an entire stack of blocks
			get {
				if (this.OutEdge.IsConnected) {
					return this.OutEdge.LinkedTo.Block.StackOutEdge;
				} else {
					return this.OutEdge;
				}
			}
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CEdge StackInEdge
		{	// Return the in-edge from an entire stack of blocks
			get {
				if (this.InEdge.IsConnected) {
					return this.InEdge.LinkedTo.Block.StackInEdge;
				} else {
					return this.InEdge;
				}
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Return the top of the stack starting at this block
		public CBlock StackTop
		{
			get {
				CBlock top = this;
				
				// Iterative implementation
				while (top.InEdge.IsConnected){
					top = top.InEdge.LinkedTo.Block;
				}
				
				// Recursive implementation
//				if (top.InEdge.IsConnected){
//					top = top.InEdge.LinkedTo.Block.StackTop;
//				}
				
				return top;
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public double StackHeight
		{	// Return the height of the stack starting at this block and going down
			get {
				// Iterative implementation
				double theight = height;
				CBlock tblock = this;
				while (tblock.OutEdge.IsConnected) {
					tblock = tblock.OutEdge.LinkedTo.Block;
					theight += tblock.height;
				}
				return theight;
				
				// Recursive implementation
//				if (this.OutEdge.IsConnected) {
//					double theight = this.OutEdge.LinkedTo.Block.StackHeight;
//					return theight + height;
//				} else {
//					return height;
//				}
			}
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public bool BranchesContain(CBlock blk) 
		{	// Returns true if blk is in the branches of the tree starting with this
			if (this == blk) return true;
			
			bool rslt = false;
			foreach (CEdge prt in Edges)
				if (prt.Type == EdgeType.Out && prt.IsConnected 
				    && prt.LinkedTo.Block.BranchesContain(blk)) 
						rslt = true;
			
			return rslt;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void OnMouseDown(Diagram.Canvas cvs, Diagram.MouseEventArgs e)
        {	// Handle mouse down event
			// Completely override default behavior in CShape
			
            // If the canvas is in the editing state
            if (cvs.Mode == Diagram.EMode.Editing)
            {
				cvs.DeselectAll();		// Deselect everything. Cannot select multiple blocks.
	            this.Select(cvs);		// Select this shape
				
				// Indicate that the canvas selection has changed
            	cvs.RaiseSelectionChangedEvent();
				
				// Intercept the right-mouse and show inspector window
				if (e.Button == Diagram.MouseButtons.Right && this.IsFactory == false) 
				{
					this.ShowContextMenu(cvs, (int)e.X, (int)e.Y);
					
					// Reset event handler. We are done.
					cvs.handler = cvs;
					
				} else {
	            
	                // Change the canvas state to "drag left start"
	                if (this.Draggable == true) cvs.EditMode = Diagram.EMode.DragLeftStart;
					
					// Do not reset event handler. Block may be doing more.
				}
				
                // Redraw
                cvs.Invalidate();
            }
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected override bool ShowContextMenu(Diagram.Canvas cvs, int X, int Y) {
			// Create the context menu for this block
			
			// Cache info
			_cvs = cvs;
			_X = X;
			_Y = Y;
	
			// Create and show context menu
			Gtk.Menu mnu = new Gtk.Menu();
			
			Gtk.MenuItem mnuDelBlock = new Gtk.MenuItem("Delete");
			mnuDelBlock.Activated += OnDeleteBlock;
			
			Gtk.MenuItem mnuDelStack = new Gtk.MenuItem("Delete Substack");
			mnuDelStack.Activated += OnDeleteStack;
			
			Gtk.MenuItem mnuToFront = new Gtk.MenuItem("Bring to Front");
			mnuToFront.Activated += OnBringToFront;
			
			Gtk.MenuItem mnuToBack = new Gtk.MenuItem("Send to Back");
			mnuToBack.Activated += OnSendToBack;
			
			Gtk.MenuItem mnuBreak = new Gtk.MenuItem("Toggle Breakpoint");
			mnuBreak.Activated += OnToggleBreakpoint;
			Gtk.MenuItem mnuProps = new Gtk.MenuItem("Properties");
			mnuProps.Activated += OnPropertiesShow;
			Gtk.MenuItem mnuInspect = new Gtk.MenuItem("Inspector");
			mnuInspect.Activated += OnInspectorShow;
			
			mnu.Append(mnuDelBlock);
			mnu.Append(mnuDelStack);
			mnu.Append( new Gtk.SeparatorMenuItem() );
			mnu.Append(mnuToFront);
			mnu.Append(mnuToBack);
			mnu.Append( new Gtk.SeparatorMenuItem() );
			mnu.Append(mnuBreak);
			mnu.Append(mnuProps);
			mnu.Append(mnuInspect);
			
			mnu.ShowAll();
			mnu.Popup();
			
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected void OnInspectorShow(object sender, EventArgs e)
		{
			(_cvs as Jigsaw.Canvas).ShowInspectorWindow();
			_cvs = null;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected virtual void OnToggleBreakpoint(object sender, EventArgs e)
		{
			this.ToggleBreakPoint();
			_cvs.Invalidate();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected virtual void OnDeleteBlock(object sender, EventArgs e)
		{
			// First get the stack top
			CBlock top = this.StackTop;
			
			(_cvs as Canvas).DeleteBlock(this);
			if (top != this) top.RepositionBlocks(null);
			
			_cvs.Invalidate();
			_cvs = null;
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected virtual void OnDeleteStack(object sender, EventArgs evt)
		{
			// First get the stack top
			CBlock top = this.StackTop;
			
			// Maintain a stack of blocks to be deleted and we progress down through the tree
			List<CBlock> toDelete = new List<CBlock>();
			toDelete.Add(this);
			
			while (toDelete.Count > 0) {
				// Get the block on top of the stack
				CBlock nextBlock = toDelete[0];
				toDelete.RemoveAt(0);
				
				// Add all output child blocks to list of blocks to be deleted
				foreach (CEdge e in nextBlock.Edges) {
					if ( e.Type != EdgeType.In && e.IsConnected ) 
						toDelete.Add(e.LinkedTo.Block);
				}
				
				// Delete the block
				(_cvs as Canvas).DeleteBlock(nextBlock);
			}
			
			if (top != this) top.RepositionBlocks(null);
			
			_cvs.Invalidate();
			_cvs = null;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void OnDrag(Diagram.Canvas cvs)
		{	// Invoke base class behavior
			base.OnDrag(cvs);

			// If possible, activate another block's edge
			foreach (CBlock b in ((Jigsaw.Canvas)cvs).AllBlocks()) {
				if (!this.BranchesContain(b)) 		// Skip if shape under drag is linked in branches to shape being moved (i.e. it is in the dragged stack)
					if (b.TryActivateWith(cvs, this)) 
						break;
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void OnDrop(Diagram.Canvas cvs) 
		{	// Implement drop behavior.
			
			// Completely replace behavior of base class.
			// Not sure how to use base implementation because cannot get a reference to 
			// newly created object if this is a factory.
			
			CBlock dropped = null;
			CBlock linked = null;
			CEdge prt = null;
			
			// If not a factory, moveteh block
			if (!this.IsFactory) {
				this.MatchOutline(cvs);
				dropped = this;
				
			} else {
				// When a factory object is dropped, create a new instance.

				dropped = (CBlock)this.Clone(this.Outline.Left, this.Outline.Top);
				cvs.AddShape( dropped );
			}
			
			// Deselect all shapes including factory and select dropped block
			cvs.DeselectAll();
			dropped.Select(cvs);
			
			// When a block is dropped, always disconnect anything linked to input edges
			// of dragged top block. An opportunity to reconnect follows.
			// Also reposition any blocks previously connected whose shape might be affected by
			// the fact that this block is no longer connected.
			CBlock prevConnected = null;
			if (dropped.InEdge.IsConnected) prevConnected = dropped.InEdge.LinkedTo.Block;
			dropped.InEdge.Disconnect();
			if (prevConnected != null) prevConnected.RepositionBlocks(null);
			
			// If the block has an edge that is activating another edge at the time of the drop
			// establish the connection
			prt = dropped.GetActivatingEdge();		// Look for an activating edge
			if (prt != null) {
				dropped.Connect(prt, cvs);			// Set up the connection and deactivate other edge
				linked = prt.LinkedTo.Block;		// Get a reference to the newly linked block
				linked.RepositionBlocks(null);
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public bool TryActivateWith(Diagram.Canvas cvs, CBlock blk) 
		{	// Try to activate an edge in this stationary block with an edge in the dragged block blk
			foreach (CEdge prt1 in blk.Edges)
				foreach (CEdge prt2 in this.Edges)
					if (prt2.TryActivateWith(cvs, prt1) == true) 
						return true;
			
			return false;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Deactivate all activation zones
		public void Deactivate(Diagram.Canvas cvs) {
			foreach (CEdge e in this.Edges) e.Deactivate(cvs);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void StartOutline(Diagram.Canvas cvs)
        {
			// If an outline is already started, do nothing
			if (this.Outline != null) return;
			
            // Create the outlining shape to be drawn
            // Add the outline to the top of the canvas annotation list
			this.Outline = this.Clone(this.Left, this.Top, false);
			
			this.Outline.TextColor = new Color(this.TextColor.R, this.TextColor.G, this.TextColor.B, 0.4); //  Color.FromArgb(100, this.TextColor);
			this.Outline.LineColor = new Color(this.LineColor.R, this.LineColor.G, this.LineColor.B, 0.4); //Color.FromArgb(100, this.LineColor);
			this.Outline.FillColor = new Color(this.FillColor.R, this.FillColor.G, this.FillColor.B, 0.4); //Color.FromArgb(100, this.FillColor);
			this.Outline.Visible = true;
			this.Outline.Draggable = false;
			this.Outline.Sizable = false;
			this.Outline.Selectable = false;
			this.Outline.Connectable = false;
			cvs.AddAnnotation(this.Outline);
			
			// When starting the outline for a CBlock,
			// also start the outline for linked blocks
			foreach (CEdge prt in this.Edges)
				if (prt.Type == EdgeType.Out && prt.IsConnected)
					prt.LinkedTo.Block.StartOutline(cvs);
		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void StopOutline(Diagram.Canvas cvs)
        {
			base.StopOutline(cvs);
			
			// When stopping the outline for a CBlock,
			// also stop the outline for linked blocks
			foreach (CEdge prt in this.Edges)
				if (prt.Type == EdgeType.Out && prt.IsConnected) 
					prt.LinkedTo.Block.StopOutline(cvs);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void TranslateOutline(Diagram.Canvas cvs)
		{	
			base.TranslateOutline(cvs);
			
			// Also translate any linked block outlines
			foreach (CEdge prt in this.Edges)
				if (prt.Type == EdgeType.Out && prt.IsConnected) 
					prt.LinkedTo.Block.TranslateOutline(cvs);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void MatchOutline(Diagram.Canvas cvs)
        {	
			base.MatchOutline(cvs);
			
			// Match outlines of all linked blocks
			foreach (CEdge prt in this.Edges)
				if (prt.Type == EdgeType.Out && prt.IsConnected) 
					prt.LinkedTo.Block.MatchOutline(cvs);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CEdge GetActivatingEdge()
		{	// Find and return an edge in this block that is activating another edge
			foreach (CEdge e in this.Edges)
				if (e.IsActivating != null) 
					return e;

			return null;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// A utility method to compeltely disconnect this block for all others
		public override void Disconnect()
		{
			foreach (CEdge e in this.Edges) e.Disconnect();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public bool Connect(CEdge activatingEdge, Diagram.Canvas cvs)
		{	// Establish the putative link of an activating edge
			
			// Get and check the edge that this edge is activating
			CEdge activatedEdge = activatingEdge.IsActivating;
			if (activatedEdge == null) return false;
			
			if (activatedEdge.Type == EdgeType.Out) 
			{
				// Save edge this edge is linked to, if any					
				// Get the in and out edge of the entire stack being connected
				CEdge nextInEdge = activatedEdge.LinkedTo;
				CEdge activatingOutEdge = activatingEdge.Block.StackOutEdge;
				
				// Rearrange connections
				activatedEdge.LinkedTo = activatingEdge;
				activatingEdge.LinkedTo = activatedEdge;
				activatingOutEdge.LinkedTo = nextInEdge;
				if (nextInEdge != null) nextInEdge.LinkedTo = activatingOutEdge;
				
				// Deactivate the activated edge after the connection is made
				activatedEdge.Deactivate(cvs);
				
				return true;
			} 
			else if (activatedEdge.Type == EdgeType.In) 
			{
				//CEdge prevOutEdge = activatedEdge.LinkedTo;
				
				// Rearrange connections
				activatedEdge.LinkedTo = activatingEdge;
				activatingEdge.LinkedTo = activatedEdge;
				//if (prevOutEdge != null) prevOutEdge.LinkedTo = activatingEdge.Block.InEdge;

				// Deactivate the activated edge after the connection is made
				activatedEdge.Deactivate(cvs);				
				
				return true;
			}
			
			return false;
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public virtual void RepositionBlocks(CEdge entryEdge)
		{	// Reposition this block wrt the entry edge
			CBlock linkedBlk = null;
			CEdge linkedEdge = null;
			
			// First, reposition this block wrt entry edge
			if (entryEdge == this.InEdge) {
				linkedEdge = this.InEdge.LinkedTo;
				linkedBlk = linkedEdge.Block;
				this.Top = linkedBlk.Top + linkedEdge._azY;
				this.Left = linkedBlk.Left + linkedEdge._azX;
			}
			
			if (entryEdge == this.OutEdge) {
				linkedEdge = this.OutEdge.LinkedTo;
				linkedBlk = linkedEdge.Block;
				this.Top = linkedBlk.Top - linkedEdge._azY - this.Height;
				this.Left = linkedBlk.Left - linkedEdge._azX;				
			}
			
			// Then, recursively reposition all blocks 
			// that are not connected through the entry edge.
			foreach (CEdge prt in this.Edges) {
				if (entryEdge != prt && prt.IsConnected) {
					linkedEdge = prt.LinkedTo;
					linkedBlk = linkedEdge.Block;
					linkedBlk.RepositionBlocks(linkedEdge);
				}
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void UpdateBoundingBox()
        {	// Extend method to move edges as well
			base.UpdateBoundingBox();

			// Move OutEdge
			if (this.OutEdge != null) {
				this.OutEdge._azY = this.Height;
				this.OutEdge._azW = this.Width;
			}
		}
	}

	// -----------------------------------------------------------------------
    public class CRobot : CBlock
    {	// Robot block shape class
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CRobot(Double X, Double Y) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y),
				new Diagram.CPoint(X + 175, Y + 20)
			})) {
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkRed;
			this.FillColor = Diagram.Colors.LightPink;
			//this.FontStyle = FontStyle.Bold;
			this.Sizable = false;
		}
		
		public CRobot(Double X, Double Y, bool isFactory) : this(X, Y)
		{
			this._isFactory = isFactory;;
		}
    }
	
	// -----------------------------------------------------------------------
    public class CInputOutput : CBlock
    {	// Input/Output block shape class
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CInputOutput(Double X, Double Y) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y),
				new Diagram.CPoint(X + 175, Y + 20)
			})) {
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkBlue;
			this.FillColor = Diagram.Colors.LightBlue;
			//this.FontStyle = FontStyle.Bold;
			this.Sizable = false;
		}
		
		public CInputOutput(Double X, Double Y, bool isFactory) : this(X, Y)
		{
			this._isFactory = isFactory;
		}
    }
	
	// -----------------------------------------------------------------------
    public class CIOPrint : CInputOutput
    {	// Print block shape class

		public CExpressionProperty Expr = null;
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CIOPrint(Double X, Double Y) : base(X, Y) {
			// Properties
			Expr = new CExpressionProperty("Expression", "X");
			Expr.PropertyChanged += OnPropertyChanged;
			this.OnPropertyChanged(null, null);
		}
		
		public CIOPrint(Double X, Double Y, bool isFactory) : this(X, Y) {
			this._isFactory = isFactory;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Returns a list of all block properties
		public override List<CProperty> Properties 
		{
			get {
				List<CProperty> props = base.Properties;
				props.Add(this.Expr);
				return props;
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		internal override bool SetProperty(string name, string val) {
			string lname = name.ToLower();
			switch(lname) {
			case "expression":
				Expr.Text = val;
				break;
			default:
				return false;
			}
			return true;
		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Write custom tags for this block
        protected override void WriteXmlTags(XmlWriter w)
        {
			base.WriteXmlTags(w);
			this.WriteXmlProperty(w, "Expression", Expr.Text);
        }
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Execute print statement
		public override IEnumerator<RunnerResponse> 
		Runner(Dictionary<string, object> locals, Dictionary<string, object> builtins) {

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
			this.Text = String.Format("print {0}", Expr.Text);
		}
	}

	// -----------------------------------------------------------------------
    public class CIOWriteToFile : CInputOutput
    {	// Write an item to a file path
		
		public CExpressionProperty Expr = null;
		public CExpressionProperty Path = null;
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CIOWriteToFile(Double X, Double Y) : base(X, Y) {
			// Properties
			Expr = new CExpressionProperty("Expression", "expr");
			Path = new CExpressionProperty("File Path", "file");
			Expr.PropertyChanged += OnPropertyChanged;
			Path.PropertyChanged += OnPropertyChanged;
			this.OnPropertyChanged(null, null);
		}
		public CIOWriteToFile(Double X, Double Y, bool isFactory) : this(X, Y) {
			this._isFactory = isFactory;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Returns a list of all block properties
		public override List<CProperty> Properties 
		{
			get {
				List<CProperty> props = base.Properties;
				props.Add(this.Expr);
				props.Add(this.Path);
				return props;
			}
		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Update text when property changes
		public void OnPropertyChanged(object sender, EventArgs E){
			this.Text = String.Format("write {0} to {1}", Expr.Text, Path.Text);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		internal override bool SetProperty(string name, string val) {
			string lname = name.ToLower();
			switch(lname) {
			case "expression":
				Expr.Text = val;
				break;
			case "path":
				Path.Text = val;
				break;
			default:
				return false;
			}
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Write custom tags for this block
        protected override void WriteXmlTags(XmlWriter w)
        {
			base.WriteXmlTags(w);
			this.WriteXmlProperty(w, "Path", Path.Text);
			this.WriteXmlProperty(w, "Expression", Expr.Text);
        }
	}
	
	// -----------------------------------------------------------------------
    public class CControlRepeat : CBlock
    {	// Repeat control block shape class
		
		// Internal edge
		public CEdge LoopEdge = null;
		
		// Properties
		public CExpressionProperty Repetitions = null;
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CControlRepeat(Double X, Double Y) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] {
				new Diagram.CPoint(X, Y), 
				new Diagram.CPoint(X + 175, Y + 50)
			})) {
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkGoldenrod;
			this.FillColor = Diagram.Colors.PaleGoldenrod;
			//this.DashStyle = Diagram.DashStyle.Dash;
			this.Sizable = false;
			
			// Create inner edge to connect loop stack
			double offsetX = 0.5*this.Width + 10.0;
			LoopEdge = new CEdge(this, "Loop", EdgeType.Out, null, offsetX, 20.0, 20.0, 20.0, this.Width-20.0);
			
			// Properties
			Repetitions = new CExpressionProperty("Repetitions", "3");
			Repetitions.PropertyChanged += OnPropertyChanged;
			this.OnPropertyChanged(null, null);
		}
		
		public CControlRepeat(Double X, Double Y, bool isFactory) : this(X, Y)
		{
			this._isFactory = isFactory;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Returns a list of all block properties
		public override List<CProperty> Properties 
		{
			get {
				List<CProperty> props = base.Properties;
				props.Add(this.Repetitions);
				return props;
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override Diagram.CShape HitShape(Diagram.CPoint pt, Diagram.Canvas cvs)
        {	// If this block is hit, return a self reference.
        	
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
		internal override bool SetProperty(string name, string val) {
			string lname = name.ToLower();
			switch(lname) {
			case "repetitions":
				Repetitions.Text = val;
				break;
			default:
				return false;
			}
			return true;
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
        // Write custom tags for this block
        protected override void WriteXmlTags(XmlWriter w)
        {
			base.WriteXmlTags(w);
			this.WriteXmlProperty(w, "Repetitions", Repetitions.Text);
        }
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Update text when property changes
		public void OnPropertyChanged(object sender, EventArgs e){
			this.Text = String.Format("repeat {0} times", Repetitions.Text);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Execute a simple repetition
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
			// Do the repetition
			int count = 1;
			int maxreps = 0;

			// Start by getting the number of repetitions as an integer
			// TODO: Allow access to global namespace
			try {
				Repetitions.Expr.Parameters = locals;
				object oreps = Repetitions.Expr.Evaluate();
				maxreps = (int)oreps;

			} catch (Exception ex) {
				MsgProp.Text = ex.Message;
				
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
		protected override void SetPath(Cairo.Context g, double x, double y, double w, double h) 
		{
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
		
		// Properties
		public CExpressionProperty IfTest = null;
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CControlIf(Double X, Double Y) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] {
				new Diagram.CPoint(X, Y), 
				new Diagram.CPoint(X + 175, Y + 50)
			})) {
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkGoldenrod;
			this.FillColor = Diagram.Colors.PaleGoldenrod;
			//this.FontStyle = FontStyle.Bold;
			this.Sizable = false;
			
			// Create inner edge to connect if-block stack
			double offsetX = 0.5*this.Width + 10.0;
			IfEdge = new CEdge(this, "If", EdgeType.Out, null, offsetX, 20.0, 20.0, 20.0, this.Width-20.0);
			
			// Properties
			IfTest = new CExpressionProperty("If-test", "true");
			IfTest.PropertyChanged += OnPropertyChanged;
			this.OnPropertyChanged(null, null);
		}
		
		public CControlIf(Double X, Double Y, bool isFactory) : this(X, Y)
		{
			this._isFactory = isFactory;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Returns a list of all block properties
		public override List<CProperty> Properties 
		{
			get {
				List<CProperty> props = base.Properties;
				props.Add(this.IfTest);
				return props;
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override Diagram.CShape HitShape(Diagram.CPoint pt, Diagram.Canvas cvs)
        {	// If this block is hit, return a self reference.
        	
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
		internal override bool SetProperty(string name, string val) {
			string lname = name.ToLower();
			switch(lname) {
			case "if":
				IfTest.Text = val;
				break;
			default:
				return false;
			}
			return true;
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
        // Write custom tags for this block
        protected override void WriteXmlTags(XmlWriter w)
        {
			base.WriteXmlTags(w);
			this.WriteXmlProperty(w, "if", IfTest.Text);
        }
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Update text when property changes
		public void OnPropertyChanged(object sender, EventArgs e){
			this.Text = String.Format("if {0}", IfTest.Text);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Execute a simple repetition
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
			
			// Do the if-test
			bool doIf = false;

			// Start by evaluating the if expression
			// TODO: Allow access to global namespace
			try {
				IfTest.Expr.Parameters = locals;
				object otest = IfTest.Expr.Evaluate();
				doIf = (bool)otest;

			} catch (Exception ex) {
				MsgProp.Text = ex.Message;
				
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
		protected override void SetPath(Cairo.Context g, double x, double y, double w, double h) 
		{
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
    public class CControlStart : CBlock
    {	// Control start block shape class

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CControlStart(Double X, Double Y) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y), 
				new Diagram.CPoint(X + 175, Y + 30)
			})) {
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkGoldenrod;
			this.FillColor = Diagram.Colors.PaleGoldenrod;
			this.Sizable = false;
			this.Text = "when program starts";
			textYOffset = 10;							// Block text offset
		}
		
		public CControlStart(Double X, Double Y, bool isFactory) : this(X, Y)
		{
			this._isFactory = isFactory;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override List<CEdge> Edges 
		{	// Return a list of all edges
			// Control start blocks only have an output edge
			get {
				return new List<CEdge>() { this.OutEdge };
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected override void SetPath(Cairo.Context g, double x, double y, double w, double h) 
		{
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
        public CControlEnd(Double X, Double Y) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y), 
				new Diagram.CPoint(X + 175, Y + 20)
			})) {
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkGoldenrod;
			this.FillColor = Diagram.Colors.PaleGoldenrod;
			//this.DashStyle = Diagram.DashStyle.Dot;
			this.Sizable = false;
			this.Text = "stop script";
		}
		
		public CControlEnd(Double X, Double Y, bool isFactory) : this(X, Y)
		{
			this._isFactory = isFactory;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override List<CEdge> Edges 
		{	// Return a list of all edges
			// Control end blocks only have an input edge
			get {
				return new List<CEdge>() { this.InEdge };
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected override void SetPath(Cairo.Context g, double x, double y, double w, double h) 
		{
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
    public class CAssignment : CBlock
    {	// Variable assignment block shape class
		
		// These should not be public, but set up to modify Text when changed
		public CVarNameProperty VarName = null;
		public CExpressionProperty RHS = null;
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CAssignment(Double X, Double Y) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y),
				new Diagram.CPoint(X + 175, Y + 20)
			})) {
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkGreen;
			this.FillColor = Diagram.Colors.LightGreen;
			//this.FontStyle = FontStyle.Bold;
			this.Sizable = false;
			
			// Properties
			VarName = new CVarNameProperty("Variable", "X");
			RHS = new CExpressionProperty("Expression", "0");
			
			VarName.PropertyChanged += OnPropertyChanged;
			RHS.PropertyChanged += OnPropertyChanged;
			this.OnPropertyChanged(null, null);
		}
		
		public CAssignment(Double X, Double Y, bool isFactory) : this(X, Y)
		{
			this._isFactory = isFactory;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Returns a list of all block properties
		public override List<CProperty> Properties 
		{
			get {
				List<CProperty> props = base.Properties;
				props.Add(this.VarName);
				props.Add(this.RHS);
				return props;
			}
		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // Write custom tags for this block
        protected override void WriteXmlTags(XmlWriter w)
        {
			base.WriteXmlTags(w);
			this.WriteXmlProperty(w, "Variable", VarName.Text);
			this.WriteXmlProperty(w, "Expression", RHS.Text);
        }

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		internal override bool SetProperty(string name, string val) {
			string lname = name.ToLower();
			switch(lname) {
			case "variable":
				VarName.Text = val;
				break;
			case "expression":
				RHS.Text = val;
				break;
			default:
				return false;
			}
			return true;
		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Update text when property changes
		public void OnPropertyChanged(object sender, EventArgs e){
			this.Text = String.Format("let {0} = {1}", VarName.Text, RHS.Text);
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
				RHS.Expr.Parameters = locals;
				locals[VarName.Text] = RHS.Expr.Evaluate();

			} catch (Exception ex) {
				Console.WriteLine(ex.Message);
				MsgProp.Text = ex.Message;
				
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
    public class CEdge
	{	// Class that holds details of a place to link CBlocks
		internal CBlock _block = null;				// The CBlock to which this edge is attached
		internal string _name = null;				// Name of edge
		private EdgeType _type = EdgeType.Out;		// The type of this edge
		private CEdge _linkedTo = null;				// The edge this one is linked to, if any
		private double _offsetX = 0.0;				// Offset from the block's Left to activating point
		private double _offsetY = 0.0;				// Offset from the block's Top to activating point
		internal double _azX = 0.0;					// Offset from block's left to left of activating zone
		internal double _azY = 0.0;					// Offset from block's top to top of activation zone
		internal double _azW = 100.0;				// Width of activation zone
		internal double _azH = 6.0; 				// Height of activation zone
		private Diagram.CShape _activationZone = null;		// Not null if this edge is activated by another
		private CEdge _isActivating = null;			// Ref to a edge that this edge is currently activating
		internal int _id = 0;
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CEdge(CBlock block, string name, EdgeType typ, CEdge linkedTo, 
		             double offsetX, double offsetY, 
		             double offsetLeft, double offsetTop, double width)
		{	// Constructor
			_block = block;
			_name = name;
			_type = typ;
			_linkedTo = linkedTo;
			_offsetX = offsetX;			// Offset to activation point
			_offsetY = offsetY;
			_azX = offsetLeft;			// offset of activation zone from top of block
			_azY = offsetTop;				
			_azW = width;				// Size of activation zone
			_azH = 6.0;
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CEdge Clone(CBlock parent)
		{	// Clone this edge, especially the isActivating reference
			CEdge prt = new CEdge(parent, this._name, this._type, this._linkedTo, 
			                      this._offsetX, this._offsetY, this._azX, this._azY, this._azW);
			prt._isActivating = this._isActivating;
			return prt;
		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public void ToXml(XmlWriter w)
        {
            w.WriteStartElement("edge");
			w.WriteAttributeString("id", _id.ToString());
			w.WriteAttributeString("name", _name);
			//w.WriteAttributeString("type", _type.ToString());
			if (this.IsConnected == true) 
				w.WriteAttributeString("linkedTo", _linkedTo._id.ToString());
			else
				w.WriteAttributeString("linkedTo", "-1");
            w.WriteEndElement();
        }
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public double ActivatingX
		{	// x-coordinate of activating point
			get {
				if (_block.Outline != null)
					return _block.Outline.Left + this._offsetX;
				else
					return _block.Left + this._offsetX;
			}
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public double ActivatingY 
		{	// y-coordinate of activating point
			get {
				if (_block.Outline != null)
					return _block.Outline.Top + this._offsetY;
				else
					return _block.Top + this._offsetY;
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public bool TryActivateWith(Diagram.Canvas cvs, CEdge prt)
		{	// Try to activate this edge with another edge
			// 1. The types must be complementary
			// 2. The other edges activating point must be within this edge's activation zone
			
			// Check edge type compatibility
			
			if (this._type == EdgeType.In) {
				if (prt.Type != EdgeType.Out) return false;
				if (prt.IsConnected) return false;
				if (this.IsConnected) return false;
			}
			
			if (this._type == EdgeType.Out) {
				if (prt.Type != EdgeType.In) return false;
			}

			// Check if activating point location is within activation zone
			double l = _block.Left + this._azX;
			double r = l + this._azW;
			double t = _block.Top + this._azY;
			double b = t + this._azH;
			double ax = prt.ActivatingX;
			double ay = prt.ActivatingY;
			
			//Console.WriteLine("{0} in [{1},{2}], {3} in [{4},{5}]", ax, l, r, ay, t, b);
			
			if (ax >= l && ax <= r && ay >= t && ay <= b)
			{
				// If the given edge is already activated by this edge, do nothing
				// Otherwise, deactivate the edge
				if (prt._isActivating == this)
					return true;
				else
					this.Deactivate(cvs);
				
				// Activate this edge and display activation zone
				List<Diagram.CPoint> pts = new List<Diagram.CPoint>() { new Diagram.CPoint( l, t ), new Diagram.CPoint( r, b ) };
				Color clr = new Color(1.0, 1.0, 1.0, 0.7);
	            this._activationZone = new Diagram.CRectangle(pts, "", Diagram.Colors.Transparent, clr, 1, clr,
	                                             true, false, false, false, false);
	            cvs.AddAnnotation(this._activationZone);
				
				// Save ref to this edge as being activated by given edge
				prt._isActivating = this;
				
				return true;
				
			} else {
				this.Deactivate(cvs);
				prt._isActivating = null;
				return false;
			}
		}
			    
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public EdgeType Type
		{	// Read-only edge type property
			get {
				return _type;
			}
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public string Name
		{	// Read-only edge name
			get {
				return _name;
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CBlock Block
		{	// Read-only CBlock that holds this CEdge
			get {
				return _block;
			}
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CEdge LinkedTo
		{	// Linked edge
			get {
				return _linkedTo;
			}
			set
			{
				_linkedTo = value;
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public bool IsActivated
		{	// Returns true if this edge is activated by another edge
			get {
				return (_activationZone != null);
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CEdge IsActivating
		{	// Returns another edge that is being activated by this edge
			get {
				return _isActivating;
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public bool IsConnected
		{	// Returns true if connected
			get {
				return (_linkedTo != null);
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void Deactivate(Diagram.Canvas cvs) 
		{	// Deactivate this edge and remove the annotation shape
			if (this._activationZone != null)
			{
	            try {
	                cvs.DeleteAnnotation(this._activationZone);
	            } catch /*(Exception e)*/ {
	            	// do nothing
	            }
				this._activationZone = null;
			}
            
			this._isActivating = null;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void Disconnect()
		{	// Disconnect this edge, if connected
			if (_linkedTo != null) {
				_linkedTo._linkedTo = null;
				_linkedTo = null;
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	}
}