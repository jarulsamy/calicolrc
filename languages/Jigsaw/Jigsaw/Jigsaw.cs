using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Text;
using System.Xml;
using System.IO;
using Cairo;
using Microsoft.Scripting.Hosting;

// Model for Block Connections:
// Blocks have one or more edges (connection points). 
// Edges have activation zones and activating points.
// Activating points, when dragged over an edge's activation zone, activate that edge.
// Only one edge can/should be activated at a time. This applies to the entire diagram.
// When a block is dropped, if an edge is activated, a connection is made.

namespace Jigsaw
{
	// -----------------------------------------------------------------------
	
	// --- Enumerates block edge types -------------------------------------
	public enum EdgeType
	{
		In = 1, Out = 2
	}
	
	// --- Enumerates block states -------------------------------------------
	public enum BlockState
	{
		Idle = 1, Running = 2, Error = 3
	}
	
	// -----------------------------------------------------------------------
	public class Canvas : Diagram.Canvas
	{	// Subclass of Canvas that adds custom behavior 
		
		private string _currentPath = null; // path to the currently open program file

		// Reference to single PropertiesWindow object
		private InspectorWindow _inspector = null;
		
		// Jigsaw events
		public event EventHandler JigsawRun;
		public event EventHandler JigsawStep;
		public event EventHandler JigsawStop;
		public event EventHandler JigsawPause;

		// A private reference to the engine that runs Jigsaw programs.
		private Engine _engine = null;
		
		// Path to look for modules to load
		private string _modulePath = null;
		
		// Reference to block panel
		private Widgets.CBlockPalette pnlBlock = null;
		
		private List<Widgets.CRoundedTab> allTabs = null;
		
		// A flag to help track running state
		private bool _isRunning = false;
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public Canvas(string modulePath, int width, int height, double worldWidth, double worldHeight) : base(width, height, worldWidth, worldHeight) 
		{
			// Setup some colors
			BackColor = Diagram.Colors.LightSlateGray;
			this.lasso.LineColor = Diagram.Colors.WhiteSmoke;
			
			this.CanFocus = true;
			
			// Properties window shared by all blocks
			_inspector = new Jigsaw.InspectorWindow(this);
			
			
			
			// Engine to run Jigsaw programs
			_engine = new Engine();
			_engine.EngineRun   += OnEngineRun;
			_engine.EngineStep  += OnEngineStep;
			_engine.EngineStop  += OnEngineStop;
			_engine.EnginePause += OnEnginePause;
			_engine.EngineReset += OnEngineReset;
			_engine.Reset(this, _inspector);
			//_engine.Reset(this); //, _inspector);
			
			// Set up all widgets
			//List<Widgets.CRoundedTab> allTabs = new List<Widgets.CRoundedTab>();
			allTabs = new List<Widgets.CRoundedTab>();
			
			// Add block panel background to canvas
			//Widgets.CBlockPalette pnlBlock = new Widgets.CBlockPalette( 95.0, 0.0, 205.0, 10000.0);
			pnlBlock = new Widgets.CBlockPalette( 95.0, 0.0, 205.0, 10000.0);
			this.AddShape(pnlBlock);
			
			// Build tabbed panel for blocks
			int tabY = 33;
			
			// ----- Control tab and factory blocks
			tabY += 33;
			Widgets.CRoundedTab tbCtrl = new Widgets.CRoundedTab(0,  tabY, 100, 30, "Control", pnlBlock);
			tbCtrl.Dock = Diagram.DockSide.Left;
			this.AddShape(tbCtrl);
			allTabs.Add (tbCtrl);
			
			CControlStart block20 = new CControlStart(110, 70, pnlBlock);
			this.AddShape(block20);
			tbCtrl.AddShape(block20);

			CControlIf block21 = new CControlIf(110, 120, pnlBlock);
			this.AddShape(block21);
			tbCtrl.AddShape(block21);

			CControlIfElse block22 = new CControlIfElse(110, 190, pnlBlock);
			this.AddShape(block22);
			tbCtrl.AddShape(block22);
			
			CControlWhile block23 = new CControlWhile(110, 290, pnlBlock);
			this.AddShape(block23);
			tbCtrl.AddShape(block23);
			
			CControlRepeat block24 = new CControlRepeat(110, 360, pnlBlock);
			this.AddShape(block24);
			tbCtrl.AddShape(block24);
			
			CControlEnd block25 = new CControlEnd(110, 430, pnlBlock);
			this.AddShape(block25);
			tbCtrl.AddShape(block25);
			
			// ----- Statement tab and factory blocks	
			tabY += 33;
			Widgets.CRoundedTab tbStats = new Widgets.CRoundedTab(0, tabY, 100, 30, "Statements", pnlBlock);
			tbStats.Dock = Diagram.DockSide.Left;
			this.AddShape(tbStats);
			allTabs.Add (tbStats);

			CAssignment vblock1 = new CAssignment(110, 70, pnlBlock);
			this.AddShape(vblock1);
			tbStats.AddShape(vblock1);

			CStatement vblock2 = new CStatement(110, 110, pnlBlock);
			this.AddShape(vblock2);
			tbStats.AddShape(vblock2);
			
			CComment bCmt1 = new CComment(110, 150, pnlBlock);
			this.AddShape(bCmt1);
			tbStats.AddShape(bCmt1);
			
			// ----- IO tab and factory blocks
			tabY += 33;
			Widgets.CRoundedTab tbInOut = new Widgets.CRoundedTab(0, tabY, 100, 30, "Input/Output", pnlBlock);
			tbInOut.Dock = Diagram.DockSide.Left;
			this.AddShape(tbInOut);
			allTabs.Add (tbInOut);
			
			CIOPrint _cioprint = new CIOPrint(110, 70, pnlBlock);
			this.AddShape(_cioprint);
			tbInOut.AddShape(_cioprint);
			
			CIOWriteToFile _ciowritefile = new CIOWriteToFile(110, 110, pnlBlock);
			this.AddShape(_ciowritefile);
			tbInOut.AddShape(_ciowritefile);
			
			// ----- Procedures tab and factory blocks
			tabY += 33;
			Widgets.CRoundedTab tbProc = new Widgets.CRoundedTab(0, tabY, 100, 30, "Procedures", pnlBlock);
			tbProc.Dock = Diagram.DockSide.Left;
			this.AddShape(tbProc);
			allTabs.Add (tbProc);

			CProcedureStart bProcStart = new CProcedureStart(110, 70, pnlBlock);
			this.AddShape(bProcStart);
			tbProc.AddShape(bProcStart);
			
			CProcedureReturn bProcRet = new CProcedureReturn(110, 120, pnlBlock);
			this.AddShape(bProcRet);
			tbProc.AddShape(bProcRet);
			
			CProcedureCall bProcCall = new CProcedureCall(110, 160, pnlBlock);
			this.AddShape(bProcCall);
			tbProc.AddShape(bProcCall);

			// Add all tabs to each tabs so that they work as expected
			foreach (Widgets.CRoundedTab tab in allTabs) tab.AddTabs( allTabs );
			
			// Bring panel to top after all tabs added to canvas
			pnlBlock.BringToFront(this);
			
			// Look for map files in module path and try to load
			if (modulePath != null) {
				// Look for all map files and load
				string[] filePaths = System.IO.Directory.GetFiles(modulePath, "*.map");
				
				// Give each a go
				foreach (string pth in filePaths) {
					try {
						UseLibrary(pth);
					} catch (Exception ex){
						Console.WriteLine (ex.Message);
					}
				}
			}
			
			// Select first tab
			tbCtrl.SetToggle(this, true);
			
			// No changes so far
			this.Modified = false;
			
			// Init to starting state with initial set of blocks
			this.OnFileNew(null, null);
			
//			// ----- Myro tab and factory blocks
//			Widgets.CRoundedTab tbMyro = new Widgets.CRoundedTab(0, 199, 100, 30, "Myro", pnlBlock);
//			tbMyro.Dock = Diagram.DockSide.Left;
//			this.AddShape(tbMyro);
//			allTabs.Add (tbMyro);
//			
//			if (engine.LoadAssembly("/Programs/Mono/Calico-dev/modules/Myro.dll") == false) {
//				Console.WriteLine ("Failed to load Myro");	
//			} else {
//				foreach (CBlock cblock in makeBlocksFromDll("Myro", 70, pnlBlock)) {
//				  this.AddShape(cblock);
//				  tbMyro.AddShape(cblock);
//				}
//			}
//
//			// ----- Graphics tab and factory blocks
//			Widgets.CRoundedTab tbGraphics = new Widgets.CRoundedTab(0, 232, 100, 30, "Graphics", pnlBlock);
//			tbGraphics.Dock = Diagram.DockSide.Left;
//			this.AddShape(tbGraphics);
//			allTabs.Add (tbGraphics);
//			
//			if (engine.LoadAssembly("/Programs/Mono/Calico-dev/modules/Graphics.dll") == false) {
//				Console.WriteLine ("Failed to load Graphics");	
//			} else {
//				foreach (CBlock cblock in makeBlocksFromDll("Graphics", 70, pnlBlock)) {
//				  this.AddShape(cblock);
//				  tbGraphics.AddShape(cblock);
//				}
//			}			
//
//			// ----- Shapes tab and factory blocks
//			Widgets.CRoundedTab tbShapes = new Widgets.CRoundedTab(0, 265, 100, 30, "Shapes", pnlBlock);
//			tbShapes.Dock = Diagram.DockSide.Left;
//			this.AddShape(tbShapes);
//			allTabs.Add (tbShapes);
//			
//			if (engine.LoadAssembly("/Programs/Mono/Calico-dev/modules/Shapes.dll") == false) {
//				Console.WriteLine ("Failed to load Shapes");	
//			} else {
//				foreach (CBlock cblock in makeBlocksFromDll("Shapes", 70, pnlBlock)) {
//				  this.AddShape(cblock);
//				  tbShapes.AddShape(cblock);
//				}
//			}
			
//			// ----- Notes tab and factory blocks
//			Widgets.CRoundedTab tbNotes = new Widgets.CRoundedTab(0, 331, 100, 30, "Notes", pnlBlock);
//			tbNotes.Dock = Diagram.DockSide.Left;
//			this.AddShape(tbNotes);
//			allTabs.Add (tbNotes);
//			
//			Diagram.CRectangle _shrect = new Diagram.CRectangle(130, 70, 135, 30);
//			_shrect.FillColor = Diagram.Colors.LightYellow;
//			_shrect.LineColor = Diagram.Colors.Gray;
//			_shrect.LineWidth = 2;
//			_shrect._isFactory = true;
//			this.AddShape(_shrect);
//			tbNotes.AddShape(_shrect);
//
//			Diagram.CRoundedRectangle _shrrect = new Diagram.CRoundedRectangle(130, 115, 135, 30);
//			_shrrect.FillColor = Diagram.Colors.LightYellow;
//			_shrrect.LineColor = Diagram.Colors.Gray;
//			_shrrect.LineWidth = 2;
//			_shrrect.Radius = 8;
//			_shrrect._isFactory = true;
//			this.AddShape(_shrrect);
//			tbNotes.AddShape(_shrrect);
//			
//			Diagram.CEllipse _shellipse = new Diagram.CEllipse(130, 160, 135, 30);
//			_shellipse.FillColor = Diagram.Colors.LightYellow;
//			_shellipse.LineColor = Diagram.Colors.Gray;
//			_shellipse.LineWidth = 2;
//			_shellipse._isFactory = true;
//			this.AddShape(_shellipse);
//			tbNotes.AddShape(_shellipse);
//
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

//			// --- Run tab
//			bRun = new Widgets.CRoundedButton(150, 70, 100, 25, "Auto-Step");
//			bRun.Dock = Diagram.DockSide.Left;
//			bRun.MouseDown += OnRunMouseDown;
//			this.AddShape(bRun);
//			tbTools.AddShape(bRun);
//			
//			bStop = new Widgets.CRoundedButton(150, 110, 100, 25, "Stop");
//			bStop.Dock = Diagram.DockSide.Left;
//			bStop.Enabled = false;
//			bStop.MouseDown += OnStopMouseDown;
//			this.AddShape(bStop);
//			tbTools.AddShape(bStop);
//			
//			Widgets.CRoundedButton bReset = new Widgets.CRoundedButton(150, 150, 100, 25, "Reset");
//			bReset.Dock = Diagram.DockSide.Left;
//			bReset.MouseDown += OnResetMouseDown;
//			this.AddShape(bReset);
//			tbTools.AddShape(bReset);
//			
//			Widgets.CRoundedButton bStep = new Widgets.CRoundedButton(150, 190, 100, 25, "Step");
//			bStep.Dock = Diagram.DockSide.Left;
//			bStep.MouseDown += OnStepMouseDown;
//			this.AddShape(bStep);
//			tbTools.AddShape(bStep);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public string ModulePath
		{
			// Returns the absolute module path for DLLs
			get { return _modulePath; }
			// Set with relative or absolute; saves absolute
			set {
				if (value == null)
					_modulePath = null;
				else
					_modulePath = System.IO.Path.GetFullPath(value); 
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public bool UseLibrary(string mapfile)
		{
			// Load and build blocks from assembly mapfile
			List<CBlock> blocks = new List<CBlock>();
			string assembly_name = "";
	        string type_name = "";
	        string method_name = "";
			string constructor_name = "";
			string return_type = "";
			
			List<string> param_names = null;
			List<string> param_type_names = null;
			List<Type> param_types = null;
			List<string> param_defaults = null;
			
			int y = 70;
			CBlock block = null;
			
			try
			{
				using (XmlReader xr = XmlTextReader.Create(mapfile)) {
			        while (xr.Read()) {
			            switch (xr.NodeType) {
						
			            case XmlNodeType.Element:
			                string name = xr.Name.ToLower();
							
			                switch (name) {
			                case "map":
								// TODO: If assembly not found, stop immediately.
								// Start by loading the assembly
								string assembly_path = xr.GetAttribute("path");
								engine.LoadAssembly(assembly_path);
			                    break;
								
							case "method":
								assembly_name = xr.GetAttribute("assembly_name");
			                    type_name     = xr.GetAttribute("type_name");
			                    method_name   = xr.GetAttribute("method_name");
								return_type   = xr.GetAttribute("return_type");
								
								// Create new containers for new object. 
								// These must be new, they cannot be cleared because they are passed by ref to the constructor.
								param_names = new List<string>();
								param_type_names = new List<string>();
								param_types = new List<Type>();
								param_defaults = new List<string>();
								
			                    break;

							case "constructor":
								assembly_name    = xr.GetAttribute("assembly_name");
			                    type_name        = xr.GetAttribute("type_name");
			                    constructor_name = xr.GetAttribute("constructor_name");
								return_type      = type_name;		// Constructors return instances of themselves
								
								// Create new containers for new object. 
								// These must be new, they cannot be cleared because they are passed by ref to the constructor.
								param_names = new List<string>();
								param_type_names = new List<string>();
								param_types = new List<Type>();
								param_defaults = new List<string>();
								
			                    break;
								
			                case "parameter":
			                    string param_name    = xr.GetAttribute("name");
			                    string param_type    = xr.GetAttribute("type");
			                    string param_default = xr.GetAttribute("default");
								
								param_names.Add(param_name);
								param_type_names.Add(param_type);
								//param_types.Add( System.Type.GetType(param_type) );
								param_defaults.Add (param_default);
								
			                    break;
		
			                }
			                break;
							
						case XmlNodeType.EndElement:
							name = xr.Name.ToLower();
							
							switch (name) {
			                case "method":
						    	block = new CMethodBlock(110, y, assembly_name, type_name, method_name, 
										    			 param_names, param_type_names, param_defaults, 
														 return_type, pnlBlock);
								block.Visible = false;
					      		blocks.Add(block);
					      		y += 40;
		
								break;
								
			                case "constructor":
								return_type = type_name;
						    	block = new CMethodBlock(110, y, assembly_name, type_name, type_name, 
										    			 param_names, param_type_names, param_defaults, 
								                         return_type, pnlBlock);
								block.Visible = false;
					      		blocks.Add(block);
					      		y += 40;
		
								break;
							}
							break;
			            }
			        }
			        xr.Close();
				}
				
				
				// Get next available tab location
				double tabY = 0.0;
				foreach (Widgets.CRoundedTab tt in allTabs) {
					if (tt.Top > tabY) tabY = tt.Top;
				}
				
				// Add tab
				tabY += 33.0;
				Widgets.CRoundedTab tab = new Widgets.CRoundedTab(0, tabY, 100, 30, assembly_name, pnlBlock);
				tab.Dock = Diagram.DockSide.Left;
				this.AddShape(tab);
				pnlBlock.BringToFront(this);
				
				// Add this tab to all other tabs
				foreach (Widgets.CRoundedTab tt in allTabs) tt.AddTab(tab);
				
				// Add all other tabs to this tab
				tab.AddTabs (allTabs);
				
				// Add this tab to list of all tabs
				allTabs.Add (tab);
				
				// Add blocks
				foreach (CBlock cblock in blocks) {
				  this.AddShape(cblock);
				  tab.AddShape(cblock);
				}
				
				// Select the new tab
				tab.SetToggle(this, true);
				
				// Redraw
				this.Invalidate();
				
				return true;
				
			} catch (Exception ex) {
					
				// Inform user of error
				Gtk.MessageDialog dlg2 = new Gtk.MessageDialog(
					null,
					Gtk.DialogFlags.Modal | Gtk.DialogFlags.DestroyWithParent, 
					Gtk.MessageType.Error,
					Gtk.ButtonsType.Ok,
					String.Format ("Unable to load library:\n\n{0}", ex.Message));
				dlg2.Title = "Library load failed";
				
				Gtk.ResponseType rsp2 = (Gtk.ResponseType)dlg2.Run ();
				dlg2.Destroy();
				
				return false;
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		public static List<CBlock> makeBlocksFromDll(string assembly_name, int y=70, Widgets.CBlockPalette palette=null) 
//		{
//		  	List<CBlock> retval = new List<CBlock>();
//		  	List<string> blocknames = new List<string>();
//			
//		  	Reflection.Utils.Mapping mapping = new Reflection.Utils.Mapping(assembly_name, "level-1");
//		  	foreach (string type_name in Reflection.Utils.getTypeNames(assembly_name))
//			{
//		    	foreach (string method_name in Reflection.Utils.getStaticMethodNames(assembly_name, type_name, mapping)) 
//				{
//					List<List<string>> names = Reflection.Utils.getParameterNames(assembly_name, type_name, method_name);
//					List<List<Type>> types = Reflection.Utils.getParameterTypes(assembly_name, type_name, method_name);
//					List<List<object>> defaults = Reflection.Utils.getParameterDefaults(assembly_name, type_name, method_name, mapping);
//					Type return_type = Reflection.Utils.getMethodReturnType(assembly_name, type_name, method_name);
//					
//					for (int n = 0; n < names.Count; n++) 
//					{
//						if (mapping.CheckSignature(type_name, method_name, types[n])) 
//						{
//					    	CBlock block = new CMethodBlock(110, y, assembly_name, type_name, method_name, 
//									    					names[n], types[n], defaults[n], return_type, palette);
//					    	if (! blocknames.Contains(block.Text)) 
//							{
//					      		retval.Add(block);
//					      		blocknames.Add(block.Text);
//					      		y += 40;
//					    	}
//					  	}
//					}							
//				}
//		  	}
//		  	return retval;
//		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public Engine engine
		{
			get {
				return _engine;
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public string CurrentPath
		{	// Manage the current path to the Jigsaw program
			get {
				return _currentPath;
			}
			set {
				if (value != null) value = value.Trim (); 
				_currentPath = value;
			}
		}

		// - - Raise the JigsawRun event  - - - - - - - - - - - - - - 
        public void RaiseJigsawRun()
        {
            if (JigsawRun != null)
            {
				EventArgs e = new EventArgs();
            	JigsawRun(this, e);
            }
        }
		
		// - - Raise the JigsawPause event  - - - - - - - - - - - - - - 
        public void RaiseJigsawPause()
        {
            if (JigsawPause != null)
            {
				EventArgs e = new EventArgs();
            	JigsawPause(this, e);
            }
        }
		
		// - - Raise the JigsawStep event  - - - - - - - - - - - - - - 
        public void RaiseJigsawStep()
        {
            if (JigsawStep != null)
            {
				EventArgs e = new EventArgs();
            	JigsawStep(this, e);
            }
        }

		// - - Raise the JigsawStop event  - - - - - - - - - - - - - - 
        public void RaiseJigsawStop()
        {
            if (JigsawStop != null)
            {
				EventArgs e = new EventArgs();
            	JigsawStop(this, e);
            }
        }
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void Run()
		{
			//if (_engine.IsRunning == false) _engine.Reset(this); //, _inspector);
			//if (!_isRunning) engine.Reset(this);
			if (!_isRunning) engine.Reset(this, _inspector);
			_isRunning = true;
			_engine.Run();
			RaiseJigsawRun();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void Stop()
		{
			_engine.Stop();
			_engine.Reset(this, _inspector);
			//_engine.Reset(this);
			_isRunning = false;
			//RaiseJigsawStop();	// Already raised in OnEngineStop
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void Pause()
		{
			_engine.Pause();
			RaiseJigsawPause();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void Step()
		{
			//if (!_isRunning) engine.Reset(this);
			if (!_isRunning) engine.Reset(this, _inspector);
			_isRunning = true;
			//_engine.Stop();
			_engine.Step();
			RaiseJigsawStep();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		void OnEngineRun(object sender, EventArgs e)
		{	
//			bRun.Enabled = false;
//			bStop.Enabled = true;
			this.Invalidate();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		void OnEngineStep(object sender, EventArgs e)
		{	
			this.Invalidate();
			
			// Update the locals display
			//_inspector.DisplayLocals(_engine);
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
//			bStop.Enabled = false;
//			bRun.Enabled = true;
			_isRunning = false;
			this.Invalidate();
			RaiseJigsawStop();
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		void OnEnginePause(object sender, EventArgs e)
		{	
			this.Invalidate();
			RaiseJigsawPause();
		}
		
		// - - - Return a list of all non-factory blocks - - - - - - -
		// TODO: Change to an enumerator
		public List<CBlock> AllBlocks()
		{
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
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void DeleteBlock(CBlock b) 
		{	// Properly delete given block 
			Diagram.Canvas cvs = (Diagram.Canvas)this;
			b.Deselect(cvs);
			b.Deactivate(cvs);
			b.Disconnect();
			cvs.DeleteShape(b);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void DeleteAllBlocks() 
		{	// Delete all blocks in preparation for a new program 
			Diagram.Canvas cvs = (Diagram.Canvas)this;
			foreach (CBlock b in AllBlocks()) {
				b.Deselect(cvs);
				b.Deactivate(cvs);
				b.Disconnect();
				cvs.DeleteShape(b);
			}
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void DeactivateAllBlocks() 
		{	// Deactivate all blocks
			Diagram.Canvas cvs = (Diagram.Canvas)this;
			foreach (CBlock b in AllBlocks()) b.Deactivate(cvs);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		public override void OnScroll(Diagram.Canvas cvs, Gdk.EventScroll e) {
//			// If on palette, scroll palette
//			
//			// Otherwise, defer to default behavior
//			base.OnScroll(cvs, e);	
//		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void OnMouseDown(Diagram.Canvas cvs, Diagram.MouseEventArgs e)
        {
			// Intercept right-mouse on canvas
//			if (e.Button == Diagram.MouseButtons.Right) 
//			{
//				this.ShowContextMenu(cvs, (int)e.X, (int)e.Y);
//				return;
//				
//			} else {
				
            	if (this.Mode == Diagram.EMode.Editing)
            	{
		            int ndeselected = 0;									// Deselect all if click on canvas with no shift key
					if ((this.ModifierKeys & Gdk.ModifierType.ShiftMask) == 0) ndeselected = this.DeselectAll();
		            if (ndeselected > 0) this.RaiseSelectionChangedEvent();	// Indicate that the canvas selection has changed
					this.EditMode = Diagram.EMode.TranslatingStart;			// Start translating diagram
					
					this.Invalidate();										// Redraw
				}
//			}
		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void OnMouseMove(Diagram.Canvas cvs, Diagram.MouseEventArgs e)
        {	// Canvas mouse move handler
            if (this.Mode == Diagram.EMode.Editing)
            {
				if (this.EditMode == Diagram.EMode.TranslatingStart || this.EditMode == Diagram.EMode.Translating) 
				{
					double dx = (e.X - this.mouseDownExact.X);
					double dy = (e.Y - this.mouseDownExact.Y);
					
					this.offsetX += dx;
					this.offsetY += dy;
					this.EditMode = Diagram.EMode.Translating;
					
					// Clip offsets
					this.ClipOffsets();
					
					// Find all docked shapes and redock them
					//this.ReDockShapes(dx, dy);
					
					this.Invalidate();
				}
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void OnMouseUp(Diagram.Canvas cvs, Diagram.MouseEventArgs e)
        {
			if (this.Mode == Diagram.EMode.Editing)
            {
				this.EditMode = Diagram.EMode.Idle;
				this.Invalidate();
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		protected bool ShowContextMenu(Diagram.Canvas cvs, int X, int Y) {
//			// Create the context menu for this block
//			
//			// Cache info
//			_X = X;
//			_Y = Y;
//	
//			// Create and show context menu
//			Gtk.Menu mnu = new Gtk.Menu();
//			
//			Gtk.MenuItem mnuNew = new Gtk.MenuItem("New");
//			mnuNew.Activated += OnFileNew;
//			Gtk.MenuItem mnuOpen = new Gtk.MenuItem("Open...");
//			mnuOpen.Activated += OnFileOpen;
//			Gtk.MenuItem mnuSave = new Gtk.MenuItem("Save");
//			mnuSave.Activated += OnFileSave;
//			Gtk.MenuItem mnuSaveAs = new Gtk.MenuItem("Save As...");
//			mnuSaveAs.Activated += OnFileSaveAs;
//			//Gtk.MenuItem mnuProps = new Gtk.MenuItem("Inspect");
//			//mnuProps.Activated += OnInspectorShow;
//			Gtk.MenuItem mnuZoomIn = new Gtk.MenuItem("Zoom in");
//			mnuZoomIn.Activated += OnViewZoomIn;
//			Gtk.MenuItem mnuZoomOut = new Gtk.MenuItem("Zoom out");
//			mnuZoomOut.Activated += OnViewZoomOut;
//			Gtk.MenuItem mnuResetZoom = new Gtk.MenuItem("Zoom reset");
//			mnuResetZoom.Activated += OnViewZoom100;
//			Gtk.MenuItem mnuToggleInset = new Gtk.MenuItem("Toggle inset");
//			mnuToggleInset.Activated += OnViewToggleInset;
//			
//			mnu.Append(mnuNew);
//			mnu.Append(mnuOpen);
//			mnu.Append( new Gtk.SeparatorMenuItem() );
//			mnu.Append(mnuSave);
//			mnu.Append(mnuSaveAs);
//			mnu.Append( new Gtk.SeparatorMenuItem() );
//			//mnu.Append(mnuProps);
//			//mnu.Append( new Gtk.SeparatorMenuItem() );
//			mnu.Append(mnuZoomIn);
//			mnu.Append(mnuZoomOut);
//			mnu.Append(mnuResetZoom);
//			mnu.Append( new Gtk.SeparatorMenuItem() );
//			mnu.Append(mnuToggleInset);
//			
//			mnu.ShowAll();
//			mnu.Popup();
//			
//			return true;
//		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		public string Export()
//		{
//			Exporter exporter = new Exporter(this);
//			exporter.ToPython("Jigsaw.py");
//			return System.IO.Path.GetFullPath("Jigsaw.py");
//		}
		
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
		public bool SaveDocument(string filename) 
		{
	       	XmlWriterSettings settings = new XmlWriterSettings();
	       	settings.Indent = true;
			settings.IndentChars = "    ";
			settings.Encoding = Encoding.ASCII;
			using (XmlWriter xw = XmlWriter.Create(filename, settings)) {
				this.ToXml(xw);
			}
			return true;
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void OnFileSave(object sender, EventArgs e)
		{	// Save to current file
			
			// If no current file, go to save as and request file path
			if (_currentPath == null || _currentPath.Trim().Length == 0 ) {
				OnFileSaveAs(sender, e);
			} else {
				SaveDocument(_currentPath);
				this.Modified = false;
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void OnFileSaveAs(object sender, EventArgs e)
		{
			// Get top level window
			Gtk.Window toplevel = null;
			if (this.Toplevel.IsTopLevel) toplevel = (Gtk.Window)this.Toplevel;
			
			// Set up and show the file chooser
			Gtk.FileChooserDialog fc = null;
			fc = new Gtk.FileChooserDialog("Save as Jigsaw file...",
			                               toplevel,
			                               Gtk.FileChooserAction.Save,
			                               "Cancel", Gtk.ResponseType.Cancel,
			                               "Save",   Gtk.ResponseType.Accept);
			fc.DoOverwriteConfirmation = true;
			
			Gtk.FileFilter f1 = new Gtk.FileFilter();
			f1.Name = "Jigsaw files";
			f1.AddPattern("*.jig");
			fc.AddFilter(f1);
			
			Gtk.FileFilter f2 = new Gtk.FileFilter();
			f2.Name = "XML files";
			f2.AddPattern("*.xml");
			fc.AddFilter(f2);
			
			Gtk.FileFilter f3 = new Gtk.FileFilter();
			f3.Name = "All files";
			f3.AddPattern("*.*");
			fc.AddFilter(f3);
			
			if ( _currentPath != null ) fc.SetFilename(_currentPath);
			
			// Collect the path
			int response = fc.Run();
			
			// Save the path and go to OnFileSave for the actual save
			if (response == (int)Gtk.ResponseType.Accept) 
			{
				try
				{
					CurrentPath = fc.Filename;
					
					// Add .xml extension if missing
					if (!_currentPath.EndsWith(".jig", StringComparison.OrdinalIgnoreCase)) CurrentPath += ".jig";
					fc.Destroy();
					Directory.SetCurrentDirectory(System.IO.Path.GetDirectoryName(CurrentPath));
					OnFileSave(sender, e);
				} catch (DirectoryNotFoundException ex) {
					Console.WriteLine("The specified directory does not exist. {0}", ex);
				} catch (Exception ex) {
					Console.WriteLine("Error saving file: {0}", ex);
				}
			
			} else {
				// Must call Destroy() to close FileChooserDialog window.
				fc.Destroy();
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void OnFileSaveAsPython(object sender, EventArgs e)
		{
			// Get top level window
			Gtk.Window toplevel = null;
			if (this.Toplevel.IsTopLevel) toplevel = (Gtk.Window)this.Toplevel;
			
			// Set up and show the file chooer
			Gtk.FileChooserDialog fc = null;
			fc = new Gtk.FileChooserDialog("Save as Python file...", 
			                               toplevel,
			                               Gtk.FileChooserAction.Save,
			                               "Cancel", Gtk.ResponseType.Cancel,
			                               "Save",   Gtk.ResponseType.Accept);
			fc.DoOverwriteConfirmation = true;
			Gtk.FileFilter f1 = new Gtk.FileFilter();
			f1.Name = "Python Files";
			f1.AddPattern("*.py");
			Gtk.FileFilter f2 = new Gtk.FileFilter();
			f2.Name = "All files";
			f2.AddPattern("*.*");
			fc.AddFilter(f1);
			fc.AddFilter(f2);
			
			// Collect the path
			int response = fc.Run();
			
			// Save the path and go to OnFileSave for the actual save
			if (response == (int)Gtk.ResponseType.Accept) 
			{
				String ppath = fc.Filename;
				
				// Add .py extension if missing
				if (!ppath.EndsWith(".py", StringComparison.OrdinalIgnoreCase))	ppath += ".py";

				// Destroy dialog
				fc.Destroy();
				
				// Do the code generation
		        using (StreamWriter outfile = new StreamWriter(ppath)) {
		            outfile.Write(this.ToPython());
		        }
				
			} else {
				// Must call Destroy() to close FileChooserDialog window.
				fc.Destroy();
			}
		}
		
		// - - - Query to save if unsaved changes - - -
		public bool ResolveUnsavedChanges()
		{
			// Get top level window
			Gtk.Window toplevel = null;
			if (this.Toplevel.IsTopLevel) toplevel = (Gtk.Window)this.Toplevel;
			
			// If modifications were made ...
			if (this.Modified == true) {
				
				// Set prompt to previous file name, if there is one.
				string msg = "Save changes?";
				if (this._currentPath != null && this._currentPath.Length > 0) 
					msg = String.Format ("Save changes to {0}?", System.IO.Path.GetFileName( _currentPath));
				
				// Show dialog asking to save changes
				Gtk.MessageDialog dlg = new Gtk.MessageDialog(
					toplevel, 
					Gtk.DialogFlags.Modal | Gtk.DialogFlags.DestroyWithParent, 
					Gtk.MessageType.Warning,
					Gtk.ButtonsType.None,
					msg);
				dlg.AddButton("Yes", Gtk.ResponseType.Yes);
				dlg.AddButton("No", Gtk.ResponseType.No);
				dlg.AddButton("Cancel", Gtk.ResponseType.Cancel);
				dlg.Title = "Unsaved Changes";
				Gtk.ResponseType rsp = (Gtk.ResponseType)dlg.Run ();
				dlg.Destroy();
				
				// If cancel, then the whole thing should be aborted. Return false.
				if (rsp == Gtk.ResponseType.Cancel)
				{
					return false;		// Not resolved
				} else if (rsp == Gtk.ResponseType.Yes) {
					// If yes, then attempt to save.
					this.OnFileSave (null, null);
					return !this.Modified;			// If save was cancelled (i.e. Modified == true), abort.
				} else {
					// If no, changes are to be abandoned.
					this.Modified = false;
					return true;
				}
			}
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void OnFileNew(object sender, EventArgs e)
		{
			// Resolve any unsaved changes.
			if (this.ResolveUnsavedChanges() == false) return;
			
			// Delete all existing non-factory blocks
			this.DeleteAllBlocks();
			
			// Clear current path
			this.CurrentPath = null;
			
			// Created default starting blocks
			CControlStart b1 = new CControlStart(410, 70);
			CControlEnd   b2 = new CControlEnd(410, 150);
			this.AddShape(b1);
			this.AddShape(b2);
			
			// Connect and reposition
			b1.OutEdge.LinkedTo = b2.InEdge;
			b2.InEdge.LinkedTo = b1.OutEdge;
			b2.RepositionBlocks(b2.InEdge);
			
			// Reset modified flag
			this.Modified = false;
			
			// Redraw
			this.Invalidate();
		}
		
		// - - Open a file and recreate the jigsaw program - - - - - - -
		public void OnFileOpen(object sender, EventArgs e)
		{
			// Resolve any unsaved changes.
			if (this.ResolveUnsavedChanges() == false) return;
			
			// Get file to open
			Gtk.FileChooserDialog fc = null;
			fc = new Gtk.FileChooserDialog("Jigsaw file to open", null,
			                               Gtk.FileChooserAction.Open,
			                               "Cancel", Gtk.ResponseType.Cancel,
			                               "Open",   Gtk.ResponseType.Accept);
			
			Gtk.FileFilter f1 = new Gtk.FileFilter();
			f1.Name = "Jigsaw files";
			f1.AddPattern("*.jig");
			fc.AddFilter(f1);
			
			Gtk.FileFilter f2 = new Gtk.FileFilter();
			f2.Name = "XML files";
			f2.AddPattern("*.xml");
			fc.AddFilter(f2);
			
			Gtk.FileFilter f3 = new Gtk.FileFilter();
			f3.Name = "All files";
			f3.AddPattern("*.*");
			fc.AddFilter(f3);
			
			int response = fc.Run ();
			
			// If file selected, read it
			if (response == (int)Gtk.ResponseType.Accept) {
				ReadFile(fc.Filename);
				CurrentPath = fc.Filename;
				Directory.SetCurrentDirectory(System.IO.Path.GetDirectoryName(CurrentPath));
			}
			
			// Must call Destroy() to close FileChooserDialog window.
			fc.Destroy();
			
			// Reset modified flag
			this.Modified = false;
			
			// Redraw
			this.Invalidate();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public bool ReadFile(string filename) {
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
				CurrentPath = filename;
				
				// === First Step : Read XML and ceate all blocks 
				//     and build dictionaries of CBlock and CEdge references
				Dictionary<string,CBlock> blocks = new Dictionary<string, CBlock>();
				Dictionary<string,CEdge> edges = new Dictionary<string, CEdge>();
				
				XmlReader xr = null;

				xr = new XmlTextReader(filename);
				// FIXME: add a unserialize to Block to get additional info
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
							tBlock[name] = val;
							//tBlock.SetProperty(name, val);
							break;
						default:
						        tBlock.ReadXmlTag(xr);
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
						default:
						  tBlock.ReadXmlEndElement(xr);
						  break;
						}
						
						break;
					}
				}
				
				xr.Close();
				xr = null;
				
				// === Second step : Reread XML and link up edges
				xr = new XmlTextReader(filename);

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
				return true;
		}

		// - - - Generate a Python version of the program - - - - - - - - - - -
		public string ToPython()
		{
			// Validate
			// Check for exactly one start block. If 0 or > 1 found, can't generate.
			// Also collect all libraries to import
			List<CBlock> allBlocks = this.AllBlocks();
			
			int count = 0;
			CControlStart sblock = null;
			foreach (CBlock b in allBlocks)
			{
				if (b is CControlStart) {
					sblock = (CControlStart)b;
					count++;
				}
				
				// @@@ Identify the required library to import for this block, if any
				
			}
			
			if (count == 0 || count > 1) {
				Console.WriteLine("Error. Exactly one Control Start Block must exist for Python generation. {0} Control Start Blocks were found", count);
				return null;
			}
			
			// Add header
			StringBuilder o = new StringBuilder();
			
			o.AppendLine("# This Python program was automatically generated by Calico Jigsaw");
			o.AppendLine("# http://calicoproject.org");
			o.AppendLine();
			
			// @@@ Do necessary imports here
			
			// Generate all procedures
			foreach (CBlock b in allBlocks)
			{
				if (b is CProcedureStart) {
					b.ToPython(o, 0);
					o.AppendLine();			// Add blank line to end of all procedures
				}
			}
			
			// Generate main program
			sblock.ToPython(o, 0);
			
			// Return result
			return o.ToString();
		}
		
		// - - - Completely replace serialization of Jigsaw Program - - - - - -
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
	public class CBlock : Diagram.CShape
	{	// Base block class
		protected BlockState _state = BlockState.Idle;	// Current state of this block
		public CEdge InEdge;							// By default, all Blocks have one main input edge and one main output edge
		public CEdge OutEdge;		
		
		protected int _textYOffset = 0;					// Y offset for when a block's text
		protected bool _hasBreakPoint = false;			// True if a has a debugging break point applied
		
		protected Gtk.Window _propDialog = null;
		protected Gtk.Window _contextMenu = null;

		protected Dictionary<String, CProperty> _properties;
		
		private Widgets.CBlockPalette _palette = null;	// Reference to block palette, if a factory
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CBlock(List<Diagram.CPoint> pts, Widgets.CBlockPalette palette = null) : base(pts)
		{	// Constructor
			double offsetX = 0.5*this.Width;
			double offsetY = this.Height;
			
			this._palette = palette;
			if (palette != null) {
				this._isFactory = true;
				this.Dock = Diagram.DockSide.Left;
			}
			
			// Default edges
			InEdge = new CEdge(this, "In", EdgeType.In, null, offsetX, 0.0, 0.0, 0.0, this.Width);
			OutEdge = new CEdge(this, "Out", EdgeType.Out, null, offsetX, offsetY, 0.0, this.Height, this.Width);
			
			// Default properties
			CReadOnlyProperty TextProp = new CReadOnlyProperty("Label", "");
			CReadOnlyProperty MsgProp = new CReadOnlyProperty("Message", "");
			
			_properties = new Dictionary<String, CProperty>();
			_properties["Label"] = TextProp;
			_properties["Message"] = MsgProp;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public BlockState State
		{	// State getter/setter 
			get { return _state;  }
			set { _state = value; }
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override Diagram.CShape Clone(double X, double Y) 
		{	// Method to clone a CBlock at X,Y
			// This method is used when a factory object is dropped on a canvas and a new clone is created
			
			return this.Clone(X, Y, true);
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public virtual CBlock Clone(double X, double Y, bool cloneEdges) 
		{	// Method to clone a CBlock at X,Y with option to clone edges.
			
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
		public string this[string key]
		{	// Access property values
			get { return _properties[key].Text;  }
			set { _properties[key].Text = value; }
		}
		
		// - - Override Text property of blocks to also set the TextProp value when assigned
        public override String Text
        {
            get { return this.text; }
            set {
				this.text = value;
				this["Label"] = value;
				//this.TextProp.Text = value;
			}
        }
		
		// This is a special internal method used when a program file is opened.
		// Each block subclass should override and handle their special properties by name.
//		internal virtual bool SetProperty(string name, string val)
//		{
//			string lname = name.ToLower();
//			switch(lname) {
//			case "text":
//				this["Label"] = val;
//				//TextProp.Text = val;
//				break;
//			default:
//				return false;
//			}
//			return true;
//		}

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
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public virtual bool Compile( ScriptEngine engine, Jigsaw.Canvas cvs )
		{	// Compile the block code into something that can be executed.
			// This behavior is block-specific.
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public virtual IEnumerator<RunnerResponse> Runner( ScriptScope scope, CallStack stack ) 
		{
			// All blocks need a Block Runner, which is an IEnumerator that executes the block's behavior.
			// Block Runner IEnumerators return a RunnerResponse object.
			// Block Runners are provided the local scope and builtin scope in which they run. 
			// Base behavior only calls output blocks and manages state. 
			
			// !!! Important. The engine always calls the block runner once after it is added to the call stack
			// !!! and the response is essentially ignored.
			// !!! It is important to always add the following section to the top of every block runner 
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
			// Custom behavior will occur here
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			
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

		// - - - Generate an return Python translation of a block - - - - -
		public virtual bool ToPython (StringBuilder o, int indent)
		{
			return true;
		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void ToXml(XmlWriter w)
        {	// CBlock subclasses must provide a method that outputs an 
        	// XML representation of itself to the given XmlWriter
            w.WriteStartElement("block");
            this.WriteXmlAttributes(w); 
			this.WriteXmlTags(w);
            w.WriteEndElement();
        }

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        protected override void WriteXmlAttributes(XmlWriter w)
        {	// Write the base standard attributes shared by all shapes
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
        public virtual void ReadXmlTag(XmlReader r)
        {	// Virtual to read custom Xml tags
	  		// nothing to do
		}

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void ReadXmlEndElement(XmlReader r)
		{	// Virtual to read custom Xml content end of an element
		  	// nothing to do
		}

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        protected override void WriteXmlTags(XmlWriter w)
        {	// Override to write custom Xml content of a shape.
			foreach (CEdge e in this.Edges) e.ToXml(w);
			foreach (CProperty p in this.Properties) p.ToXml(w);
        }
		
//	 	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		protected virtual void WriteXmlProperty(XmlWriter w, string name, string val) {
//	        w.WriteStartElement("property");
//			w.WriteAttributeString("name", name);
//			w.WriteAttributeString("value", val);
//			w.WriteEndElement();	
//		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void Draw(Cairo.Context g)
        {	// All blocks follow the same pattern for drawing.
			// Block subclasses only need to define the graphics path, fill color and text.
			// Draw block on the canvas
			
            // Cannot draw with negative width or height, 
            // so use bounding box points to draw
            double x = this.left;
            double y = this.top;
            double w = this.width;
            double h = this.height;
			
			g.Save();
			
            // Block outline
			SetPath(g);
			
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
			DrawLabels(g);
			
			// If in an error state, add an x to the block
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
			
			// Draw breakpoint, if set
			if (this._hasBreakPoint) {
				g.Color = Diagram.Colors.Red;
				g.MoveTo(x+2, y+7);
				g.Arc(x+7, y+7, 5, 0.0, 2.0*Math.PI);
				g.ClosePath();
				g.Fill();
			}
			
			// Add a connection indicator to in-edge, if connected
			if (InEdge.IsConnected) {
				g.Color = Diagram.Colors.SemiWhite;
				g.Rectangle(x+0.5*w-10, y+1, 20, 2);
				g.Fill ();
			}
			
            // Finally, draw any shape decorator shapes
            this.DrawDecorators(g);

			g.Restore();
        }
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected virtual void DrawLabels(Cairo.Context g)
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
				layout.Ellipsize = Pango.EllipsizeMode.End;
				layout.Width = (int)((w-10.0)*Pango.Scale.PangoScale);
				
				layout.SetText(text);
				g.MoveTo(x+10.0, y+3.0+_textYOffset);
				Pango.CairoHelper.ShowLayout(g, layout);
            }
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected virtual void SetPath(Cairo.Context g) 
		{	// Base block outline graphics path figure
			double x = this.left;
            double y = this.top;
            double w = this.width;
            double h = this.height;
			double r = 6.0;
			double hpi = 0.5*Math.PI;
			
			//if (this.Dock == Diagram.DockSide.Left) g.InverseTransformPoint(ref x, ref y);
			
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
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public virtual List<CEdge> Edges 
		{	// Returns a list of all edges. Override if subclass adds edges to block.
			get {
				return new List<CEdge>() { this.InEdge, this.OutEdge };
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public virtual List<CProperty> Properties 
		{	// Returns a list of all block properties
			get {
				return new List<CProperty>( _properties.Values );
				//return new List<CProperty>() { this.TextProp, this.MsgProp };
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
		
		// -- Return the top of the stack starting at this block - - - -
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
		
		// - - - Handle mouse down event - - - - - - - - - - - - - -
        public override void OnMouseDown(Diagram.Canvas cvs, Diagram.MouseEventArgs e)
        {
			// Override default behavior in CShape
			
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
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void OnScroll(Diagram.Canvas cvs, Gdk.EventScroll e)
		{
			// If this is a factory block...
			if (this.IsFactory && _palette != null) {
				// delegate event to block palette
				_palette.OnScroll(cvs, e);
			} else {
				// Default behavior is to delegate event to canvas
				cvs.OnScroll(cvs, e);
			}
		}
		
		// - - - Create the context menu for this block - - - - - - - - - - -
		protected override bool ShowContextMenu(Diagram.Canvas cvs, int X, int Y) 
		{
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
			//Gtk.MenuItem mnuInspect = new Gtk.MenuItem("Inspector");
			//mnuInspect.Activated += OnInspectorShow;
			
			mnu.Append(mnuDelBlock);
			mnu.Append(mnuDelStack);
			mnu.Append( new Gtk.SeparatorMenuItem() );
			mnu.Append(mnuToFront);
			mnu.Append(mnuToBack);
			mnu.Append( new Gtk.SeparatorMenuItem() );
			mnu.Append(mnuBreak);
			mnu.Append(mnuProps);
			//mnu.Append(mnuInspect);
			
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
			
			Jigsaw.Canvas js = (Jigsaw.Canvas)cvs;
			
			// If possible, activate another block's edge
			foreach (CBlock b in js.AllBlocks()) {
				// Skip if shape under drag is linked in branches to shape being moved (i.e. it is in the dragged stack)
				if (!this.BranchesContain(b)) {
					//b.TryActivateWith(cvs, this);		// Activates all blocks possible
					if (b.TryActivateWith(cvs, this)) break;
				}
			}
			
			// Check if the block being dragged is near an edge.
			// Translate canvas if it is as a kind of canvas scrolling.
			double xmin = 0.0, ymin = 0.0, xmax = 0.0, ymax = 0.0;
			int sw, sh;
			cvs.GdkWindow.GetSize(out sw, out sh);
			cvs.TransformPoint(0.0, 0.0, out xmin, out ymin);
			cvs.TransformPoint(sw, sh, out xmax, out ymax);
			if (this.Outline.Top < ymin) {
				cvs.DoTranslate( 0.0, 40.0); //wymin - this.Outline.Top );
			} else if (this.Outline.Left < xmin) {
				cvs.DoTranslate( 40.0, 0.0); //wxmin - this.Outline.Left, 0.0 );
			} else if (this.Outline.Top + this.Outline.Height > ymax) {
				cvs.DoTranslate( 0.0, -40.0);
			} else if (this.Outline.Left + this.Outline.Width > xmax) {
				cvs.DoTranslate(-40.0, 0.0);
			}
			
			// Set modified flag
			js.Modified = true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void OnDrop(Diagram.Canvas cvs) 
		{	// Implement drop behavior.
			
			// Completely replace behavior of base class.
			// Not sure how to use base implementation because cannot get a reference to 
			// newly created object if this is a factory.
			
			CBlock dropped = null;
			CBlock linked = null;
			
			// If not a factory, move the block
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
			List<CEdge> edges = null;
			edges = dropped.GetActivatingEdges();		// Look for an activating edge
			foreach (CEdge prt in edges) {
				dropped.Connect(prt, cvs);			// Set up the connection and deactivate other edge
				linked = prt.LinkedTo.Block;		// Get a reference to the newly linked block
				linked.RepositionBlocks(null);
			}
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public bool TryActivateWith(Diagram.Canvas cvs, CBlock blk) 
		{	// Try to activate an edge in this stationary block with an edge in the dragged block blk
			foreach (CEdge edg1 in blk.Edges)
				foreach (CEdge edg2 in this.Edges)
					if (edg2.TryActivateWith(cvs, edg1) == true) 
						return true;
			
			return false;
		}
		
		// - - - Deactivate all activation zones - - - - - - - - - - - - - -
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
		public List<CEdge> GetActivatingEdges()
		{	// Find and return an edge in this block that is activating another edge
			List<CEdge> edges = new List<CEdge>();
			
			foreach (CEdge e in this.Edges) {
				if (e.IsActivating != null) {
					edges.Add (e);
					//return e;
				}
			}
			
			return edges;
		}
		
		// - - -A utility method to completely disconnect this block for all others - - -
		public override void Disconnect()
		{
			foreach (CEdge e in this.Edges) e.Disconnect();
		}
		
		// - - Establish the putative link of an activating edge - - - -
		public bool Connect(CEdge activatingEdge, Diagram.Canvas cvs)
		{
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
			
			// Then, recursively reposition all subordinate blocks 
			// not connected through the entry edge.
			foreach (CEdge edg in this.Edges) {				// Loop over all edges in this block
				if (entryEdge != edg && edg.IsConnected) {	// Skip the entry edge of this block
					linkedEdge = edg.LinkedTo;				// Get the edge linked to this edge
					linkedBlk = linkedEdge.Block;			// Get the block linked through this edge
					linkedBlk.RepositionBlocks(linkedEdge);	// Reposition all other blocks
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
    public class CEdge
	{	// Class that holds details of a place to link CBlocks
		internal CBlock _block = null;				// The CBlock to which this edge is attached
		internal string _name = null;				// Name of edge
		private EdgeType _type = EdgeType.Out;		// The type of this edge
		private CEdge _linkedTo = null;				// The edge this one is linked to, if any
		internal double _offsetX = 0.0;				// Offset from the block's Left to activating point
		internal double _offsetY = 0.0;				// Offset from the block's Top to activating point
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
				//Color clr = new Color(1.0, 1.0, 1.0, 0.7);
	            this._activationZone = new Diagram.CRectangle(
					pts, "", Diagram.Colors.Transparent,
					Diagram.Colors.SemiWhite, 0, Diagram.Colors.SemiWhite,
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
