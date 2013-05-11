using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Text;
using System.Xml;
using System.IO;
using System.Reflection;
using Cairo;
using Microsoft.Scripting;
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
	public static class Constant {
		public const int SPACES = 4; // used in ToPython indent level
	}
	
	// --- Enumerates block edge types -------------------------------------
	public enum EdgeType
	{
		In = 1, Out = 2
	}
	
	// --- Enumerates running states -------------------------------------------
	public enum RunningState
	{
		Idle = 1, Running = 2, Paused = 3, Error = 4
	}

	// --- Used to define a shared set of compiler options ---------------------
	public static class Compiler 
	{
		private static CompilerOptions _compiler_options = null;

		public static CompilerOptions Options(ScriptEngine engine) 
		{
			if (_compiler_options == null) {
				CompilerOptions compiler_options = engine.GetCompilerOptions();
				((IronPython.Compiler.PythonCompilerOptions)compiler_options).PrintFunction = true;
				((IronPython.Compiler.PythonCompilerOptions)compiler_options).AllowWithStatement = true;
				((IronPython.Compiler.PythonCompilerOptions)compiler_options).TrueDivision = true;
				_compiler_options = compiler_options;
			}
			return _compiler_options;
		}
	}

	// ---
	public class XmlWrapper : XmlTextReader {
		string filename = null;
		string contents = null;
		
		public XmlWrapper(string filename) : base(filename) {
			this.filename = filename;
		}
		
		public XmlWrapper(string contents, bool text) : base(new StringReader(contents)) {
			this.contents = contents;
		}
		
		public XmlWrapper Clone() {
			if (filename != null)
				return new XmlWrapper(filename);
			else 
				return new XmlWrapper(contents, true);
		}
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
		public event EventHandler JigsawError;

		// A private reference to the engine that runs Jigsaw programs.
		private Engine _engine = null;

		// Current state of Jigsaw object
		private RunningState _state = RunningState.Idle;
		
		// Path to look for modules to load
		private string _modulePath = null;
		
		// Reference to block and tab palettes
		internal Widgets.CBlockPalette pnlBlock = null;
		internal Widgets.CTabPalette pnlTab = null;
		
		// List of all tab widgets
		private List<Widgets.CRoundedTab> allTabs = null;

		// True if to update block display while running
		private bool _updateDisplay = true;
		
		// lastSelectedPage, automatically show properties dialog when dropping new block
		internal bool _autoProperties = true; //false;
		
		// Ref to internal search helper object
		public SearchHelper _searchHelper = null;
		
		// Left tab height
		public int tabHeight = 33;

		// Scrollbars
		//private Widgets.CYScrollBar ybar;

		protected int _X;										// Cache for context menu
		protected int _Y;

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public Canvas(string modulePath, int width, int height, double worldWidth, double worldHeight) : base(width, height, worldWidth, worldHeight) 
		{
			// Setup some colors
			BackColor = Diagram.Colors.LightSlateGray;
			this.lasso.LineColor = Diagram.Colors.WhiteSmoke;
			
			this.CanFocus = true;
			
			// Expand provided module path to a full path and save
			this._modulePath = System.IO.Path.GetFullPath(modulePath);;
			
			// Properties window shared by all blocks
			//_inspector = new Jigsaw.InspectorWindow(this);

			// Engine to run Jigsaw programs
			_engine = new Engine();
			_engine.EngineRun   += OnEngineRun;
			_engine.EngineStep  += OnEngineStep;
			_engine.EngineStop  += OnEngineStop;
			_engine.EnginePause += OnEnginePause;
			_engine.EngineReset += OnEngineReset;
			_engine.EngineError += OnEngineError;
			_engine.Reset(this, _inspector);
			_engine.LoadAssembly( System.IO.Path.Combine(this._modulePath, "Common.dll"));
			//_engine.Reset(this); //, _inspector);
			
			// Set up all widgets
			//List<Widgets.CRoundedTab> allTabs = new List<Widgets.CRoundedTab>();
			allTabs = new List<Widgets.CRoundedTab>();
			
			// Add block palette background to canvas
			pnlBlock = new Widgets.CBlockPalette( 95.0, 0.0, CBlock.BlockWidth + 30.0, 10000.0);
			this.AddShape (pnlBlock);				// Add the block panel to the shapes list
			this.AddShape (pnlBlock.YScrollbar);

			pnlTab = new Widgets.CTabPalette(0.0, 0.0, 100.0, 10000.0);
			pnlTab.allTabs = allTabs;
			this.AddShape(pnlTab);
			this.AddShape(pnlTab.YScrollbar);

			// Build tabbed panel for blocks
			int tabY = 0; //33;
			int os = 34;

			// ----- Control tab and factory blocks
			tabY += tabHeight;
			Widgets.CRoundedTab tbCtrl = new Widgets.CRoundedTab(0, tabY, 100, tabHeight-3, "Control", pnlBlock, pnlTab,
											Diagram.Colors.PaleGoldenrod, Diagram.Colors.DarkGoldenrod);
			tbCtrl.Dock = Diagram.DockSide.Left;
			this.AddShape(tbCtrl);
			allTabs.Add (tbCtrl);
			
			CControlStart block20 = new CControlStart(110, 70-os, pnlBlock);
			this.AddShape(block20);
			tbCtrl.AddShape(block20);

			CControlIf block21 = new CControlIf(110, 150-os, pnlBlock);
			this.AddShape(block21);
			tbCtrl.AddShape(block21);

			CControlIfElse block22 = new CControlIfElse(110, 220-os, pnlBlock);
			this.AddShape(block22);
			tbCtrl.AddShape(block22);
			
			CControlWhile block23 = new CControlWhile(110, 320-os, pnlBlock);
			this.AddShape(block23);
			tbCtrl.AddShape(block23);
			
			CControlRepeat block24 = new CControlRepeat(110, 390-os, pnlBlock);
			this.AddShape(block24);
			tbCtrl.AddShape(block24);

			CControlForeach block27 = new CControlForeach(110, 460-os, pnlBlock);
			this.AddShape(block27);
			tbCtrl.AddShape(block27);
			
			CControlBreak block25 = new CControlBreak(110, 530-os, pnlBlock);
			this.AddShape(block25);
			tbCtrl.AddShape(block25);
			
			CControlEnd block26 = new CControlEnd(110, 570-os, pnlBlock);
			this.AddShape(block26);
			tbCtrl.AddShape(block26);
			
			// ----- Statement tab and factory blocks	
			tabY += tabHeight;
			Widgets.CRoundedTab tbStats = new Widgets.CRoundedTab(0, tabY, 100, tabHeight-3, "Statements", pnlBlock, pnlTab,
												Diagram.Colors.LightGreen, Diagram.Colors.DarkGreen);
			tbStats.Dock = Diagram.DockSide.Left;
			this.AddShape(tbStats);
			allTabs.Add (tbStats);

			CAssignment vblock1 = new CAssignment(110, 70-os, pnlBlock);
			this.AddShape(vblock1);
			tbStats.AddShape(vblock1);

			CStatement vblock2 = new CStatement(110, 110-os, pnlBlock);
			this.AddShape(vblock2);
			tbStats.AddShape(vblock2);
			
			CRandom sRandomBlock = new CRandom(110, 150-os, pnlBlock);
			this.AddShape(sRandomBlock);
			tbStats.AddShape(sRandomBlock);
			
			CInlineComment bCmt1 = new CInlineComment(110, 190-os, pnlBlock);
			this.AddShape(bCmt1);
			tbStats.AddShape(bCmt1);
			
			CComment bCmt2 = new CComment(110, 230-os, pnlBlock);
			this.AddShape(bCmt2);
			tbStats.AddShape(bCmt2);
			
			// ----- IO tab and factory blocks
			tabY += tabHeight;
			Widgets.CRoundedTab tbInOut = new Widgets.CRoundedTab(0, tabY, 100, tabHeight-3, "Input/Output", pnlBlock, pnlTab,
												Diagram.Colors.LightBlue, Diagram.Colors.DarkBlue);
			tbInOut.Dock = Diagram.DockSide.Left;
			this.AddShape(tbInOut);
			allTabs.Add (tbInOut);
			
			CIOPrint _cioprint = new CIOPrint(110, 70-os, pnlBlock);
			this.AddShape(_cioprint);
			tbInOut.AddShape(_cioprint);
			
			CIOAsk _cioask = new CIOAsk(110, 110-os, pnlBlock);
			this.AddShape(_cioask);
			tbInOut.AddShape(_cioask);

			CIOTell _ciotell = new CIOTell(110, 150-os, pnlBlock);
			this.AddShape(_ciotell);
			tbInOut.AddShape(_ciotell);

			CBeep sBeep = new CBeep(110, 190-os, pnlBlock);
			this.AddShape(sBeep);
			tbInOut.AddShape(sBeep);
			
//			CIOWriteToFile _ciowritefile = new CIOWriteToFile(110, 150-os, pnlBlock);
//			this.AddShape(_ciowritefile);
//			tbInOut.AddShape(_ciowritefile);
			
			// ----- Procedures tab and factory blocks
			tabY += tabHeight;
			Widgets.CRoundedTab tbProc = new Widgets.CRoundedTab(0, tabY, 100, tabHeight-3, "Procedures", pnlBlock, pnlTab,
												Diagram.Colors.Thistle, Diagram.Colors.Purple);
			tbProc.Dock = Diagram.DockSide.Left;
			this.AddShape(tbProc);
			allTabs.Add (tbProc);

			CProcedureStart bProcStart = new CProcedureStart(110, 70-os, pnlBlock);
			this.AddShape(bProcStart);
			tbProc.AddShape(bProcStart);
			
			CProcedureReturn bProcRet = new CProcedureReturn(110, 150-os, pnlBlock);
			this.AddShape(bProcRet);
			tbProc.AddShape(bProcRet);
			
			CProcedureCall bProcCall = new CProcedureCall(110, 190-os, pnlBlock);
			this.AddShape(bProcCall);
			tbProc.AddShape(bProcCall);

			// Add all tabs to each tabs so that they work as expected
			foreach (Widgets.CRoundedTab tab in allTabs) tab.AddTabs( allTabs );

			// Bring panel to top after all tabs added to canvas
			pnlTab.BringToFront (this);
			pnlBlock.BringToFront (this);
			pnlTab.YScrollbar.BringToFront (this);

			// Select first tab
			tbCtrl.SetToggle(this, true);

			//ybar = new Widgets.CYScrollBar(500.0, 0.0, 14, 100);
			//this.AddShape(ybar);

			// No changes so far
			this.Modified = false;

			// Init to starting state with initial set of blocks
			this.OnFileNew(null, null);
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public static void log(object msg, string path) 
		{
			using (StreamWriter w = File.AppendText(path))
			{
				w.WriteLine("{0}: {1}", DateTime.Now.ToString(), msg.ToString());
			}
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public static void MsgBox(object msg)
		{
			Gtk.MessageDialog md = new Gtk.MessageDialog(
				null, 
				Gtk.DialogFlags.DestroyWithParent, 
				Gtk.MessageType.Info, 
				Gtk.ButtonsType.Close, 
				msg.ToString ());
			md.Run();
			md.Destroy();
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public new RunningState State {
			get {
				return _state;
			}
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

		public List<Color> makeColor(string assembname) {
			// Find a good, random, same-for-everyone color for this DLL:
			// Make a hash from name:
			int hash = 17;
			foreach (char c in assembname) hash = hash * 31 + c.GetHashCode();
			hash = Math.Abs(hash);
			
			// Get the color from the color dictionary:
			//Dictionary<string,Color> dict = Widgets.Colors.colors;
			//List<string> names = new List<string>(dict.Keys);
			//Color fill_color = dict[names[hash % names.Count]];
			Random rnd = new Random(hash);
			Color fill_color = new Cairo.Color( rnd.NextDouble()*0.5+0.5, rnd.NextDouble()*0.5+0.5, rnd.NextDouble()*0.5+0.5);
			
			// Make the line color be a little darker than fill:
			Color line_color = new Color(fill_color.R * .4, fill_color.G * .4, fill_color.B * .4);
			return new List<Color>() {fill_color, line_color};
		}
		
		// - - - Create XML map file in StringBuilder object - - - - - - - - -
		public StringBuilder CreateMapFile(string dllfile)
		{	
			StringBuilder sb = new StringBuilder();
			try
			{
				string assembname = System.IO.Path.GetFileNameWithoutExtension(dllfile);
				
				XmlWriterSettings settings = new XmlWriterSettings();
				settings.Indent = true;
				settings.IndentChars = "    ";
				settings.Encoding = Encoding.ASCII;
				
				using (XmlWriter xw = XmlWriter.Create(sb, settings)) {
					xw.WriteStartElement("map");
					xw.WriteAttributeString("path", dllfile);
					xw.WriteElementString("docstring", "");
					
					List<Color> colors = makeColor(assembname);
					Color fill_color = colors[0];
					Color line_color = colors[1];
					
					xw.WriteStartElement("fill_color");
					xw.WriteAttributeString("red", String.Format("{0:0.000}", fill_color.R)); //"0.6758"); 
					xw.WriteAttributeString("green", String.Format("{0:0.000}", fill_color.G)); //"0.8437"); 
					xw.WriteAttributeString("blue", String.Format("{0:0.000}", fill_color.B)); //"0.8984");
					xw.WriteAttributeString("alpha", String.Format("{0:0.000}", fill_color.A)); //"1.0");
					xw.WriteEndElement();
					
					xw.WriteStartElement("line_color");
					xw.WriteAttributeString("red", String.Format("{0:0.000}", line_color.R)); //"0.0"); 
					xw.WriteAttributeString("green", String.Format("{0:0.000}", line_color.G)); //"0.0"); 
					xw.WriteAttributeString("blue", String.Format("{0:0.000}", line_color.B)); //"0.5430");
					xw.WriteAttributeString("alpha", String.Format("{0:0.000}", line_color.A)); //"1.0");
					xw.WriteEndElement();
					
				  	foreach (string type_name in Reflection.Utils.getTypeNames(dllfile))
					{
						Type type = Reflection.Utils.getType(dllfile, type_name);
						
						// Write constructors
						foreach (ConstructorInfo ci in Reflection.Utils.getConstructors(type))
						{	// write it
							xw.WriteStartElement("constructor");
					  	    xw.WriteAttributeString("assembly_name", assembname);
						    xw.WriteAttributeString("type_name", type_name);
						    xw.WriteAttributeString("constructor_name", ci.Name);
							xw.WriteElementString("docstring", "");
							
						    foreach (ParameterInfo pi in ci.GetParameters()) {
						      xw.WriteStartElement("parameter");
						      xw.WriteAttributeString("name", pi.Name);
						      xw.WriteAttributeString("type", pi.ParameterType.FullName);
						      xw.WriteAttributeString("default", pi.DefaultValue.ToString());
						      xw.WriteEndElement();
						    }
							
						  	xw.WriteEndElement();
						}
						
						// Write static methods
						foreach (MethodInfo mi in Reflection.Utils.getStaticMethods(type))
						{	// write it
							xw.WriteStartElement("method");
					  	    xw.WriteAttributeString("assembly_name", assembname);
						    xw.WriteAttributeString("type_name", type_name);
						    xw.WriteAttributeString("method_name", mi.Name);
							xw.WriteAttributeString("return_type", mi.ReturnType.FullName);
							xw.WriteElementString("docstring", "");
							
						    foreach (ParameterInfo pi in mi.GetParameters()) {
						      xw.WriteStartElement("parameter");
						      xw.WriteAttributeString("name", pi.Name);
						      xw.WriteAttributeString("type", pi.ParameterType.FullName);
						      xw.WriteAttributeString("default", pi.DefaultValue.ToString());
						      xw.WriteEndElement();
						    }
						  	xw.WriteEndElement();
						}
				  	}
					
					xw.WriteEndElement();	// close map
					
				}
				
				return sb;
				
			} catch (Exception ex) {
				Console.WriteLine("Map file created failed: {0}", ex.Message);
				return null;
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
		public void UseLibrary(string filename)
		{
			// Interface for Calico
			Stop();
			string filetype = System.IO.Path.GetExtension(filename).ToLower();
			try
			{	// Decide what to do based on file extension
				if (filetype == ".map") {
					System.IO.TextReader tr = new System.IO.StreamReader(filename);
					UseLibraryMap(tr);
				} else {	// Assume it's a dll
					if (!engine.loadedAssemblies.ContainsKey(filename)) 
						UseLibraryDLL(filename);
				}
			} catch (Exception ex) {
				Console.Error.WriteLine ("Error loading library: {0}", ex.Message);
			}
		}
	
		// - - - Use Library and assign random color - - - - - - - - - - - - - - - - - - - -
		public bool UseLibraryDLL(string dllfile)
		{
			// Load and build blocks from assembly dllfile
			List<CBlock > blocks = new List<CBlock> ();								// List of new blocks
			List<String> tabNames = new List<string>();								// List of new tab names
			Dictionary<CBlock, String> tabMap = new Dictionary<CBlock, String>();	// Assign blocks to new tab names
			Dictionary<String,int> blockCount = new Dictionary<string, int>();		// Count number of blocks on tab
			Dictionary<String,int> blockPos = new Dictionary<string, int>();		// Track position of new blocks in a tab

			Widgets.CRoundedTab tab = null;
			CBlock block = null;

			string assembly_name = System.IO.Path.GetFileNameWithoutExtension (dllfile);
			System.Reflection.Assembly assembly = engine.LoadAssembly (dllfile);

			// Init default tab data structures
			tabNames.Add (assembly_name);
			blockPos[assembly_name] = 34;
			blockCount[assembly_name] = 0;

			foreach (string type_name in Reflection.Utils.getTypeNames(assembly).ToArray()) {
				Type type = Reflection.Utils.getType (assembly, type_name);
				if (type_name.Contains("Extensions")) {
				    continue;
				}

				// Skip constructors for now
//				foreach (System.Reflection.ConstructorInfo ci in Reflection.Utils.getConstructors(type)) {
//
//					// Look for attributes and skip if tab name is set to null
//					object[] attrs = ci.GetCustomAttributes(false); 
//					if (attrs.Length > 0) {
//						if ( attrs[0].GetType() == typeof(JigsawTabAttribute) ){
//						//if ( attrs[0].GetType().ToString () == typeof(JigsawTabAttribute).ToString () ) {
//							// If null is set for tab name, do not display
//							JigsawTabAttribute tattr = (JigsawTabAttribute)attrs[0];
//							if ( tattr.Name == null ) continue;
//						}
//					}
//
//					List<string > param_names = new List<string> ();
//					List<string > param_type_names = new List<string> ();
//					List<string > param_defaults = new List<string> ();
//					foreach (System.Reflection.ParameterInfo pi in ci.GetParameters()) {
//						param_names.Add (pi.Name);
//						param_type_names.Add (pi.ParameterType.FullName);
//						param_defaults.Add (pi.DefaultValue.ToString ());
//					}
//					string return_type = type_name;
//					block = new CMethodBlock (110, y, assembly_name, type_name, type_name,
//                                                    param_names, param_type_names, param_defaults,
//                                                    return_type, pnlBlock);
//					block.Visible = false;
//					block.LineColor = line_color;
//					block.FillColor = fill_color;
//					blocks.Add (block);
//					y += 40;
//				}


				foreach (System.Reflection.MethodInfo mi in Reflection.Utils.getStaticMethods(type)) {

					string tabName = assembly_name;		// Default tab name

					// Look for special methods
					if (mi.Name == "set_gui_thread_id")
					{
						mi.Invoke(null, new object [] {System.Threading.Thread.CurrentThread.ManagedThreadId});
						continue;
					}

					// Look for attributes and skip if tab name is set to null
					object[] attrs = mi.GetCustomAttributes(false); 

					if (attrs.Length > 0) {
						//if (attrs[0].GetType ().ToString() == typeof(JigsawTabAttribute).ToString ())
						//Console.WriteLine ("{0} {1}", mi.Name, attrs[0].GetType ());
						if ( attrs[0].GetType() == typeof(JigsawTabAttribute) )
						{
							JigsawTabAttribute tattr = (JigsawTabAttribute)attrs[0];
							if ( tattr.Name == null ) {		// If null is set for tab name, do not make block
								continue;
							} else {						// Save tab data
								tabName = tattr.Name;
								if (!tabNames.Contains (tabName)) {
									tabNames.Add (tabName);
									blockPos[tabName] = 34;
									blockCount[tabName] = 0;
								}
							}
						}
					}

					List<string > param_names = new List<string> ();
					List<string > param_type_names = new List<string> ();
					List<string > param_defaults = new List<string> ();
					foreach (System.Reflection.ParameterInfo pi in mi.GetParameters()) {
						param_names.Add (pi.Name);
						param_type_names.Add (pi.ParameterType.FullName);
						try {
							param_defaults.Add (pi.DefaultValue.ToString ());
						} catch {
							param_defaults.Add ("");
						}
					}

					string return_type = mi.ReturnType.ToString ();
					int y = blockPos[tabName];
					block = new CMethodBlock (110, y, assembly_name, type_name, mi.Name,
                                                param_names, param_type_names, param_defaults,
                                                return_type, pnlBlock);
					block.Visible = false;
					List<Color> colors = makeColor(tabName);
					Color fill_color = colors[0];
					Color line_color = colors[1];
					block.LineColor = line_color;
					block.FillColor = fill_color;
					blocks.Add (block);
					tabMap[block] = tabName;
					blockPos[tabName] = y + 40;
					blockCount[tabName]++;
				}
			}

			// Get next available tab location on tab pallette
			double tabY = 0.0;
			foreach (Widgets.CRoundedTab tt in allTabs) {
				if (tt.Top > tabY)
					tabY = tt.Top;
			}

			// Add all new tabs and blocks
			string [] tabArray = tabNames.ToArray();
			Array.Sort(tabArray);
			foreach (String tabName in tabArray) 
			{
				// Skip empty tabs
				if (blockCount[tabName] == 0) continue;

				// Add tab if has at least one block
				tabY += tabHeight;
				List<Color> colors = makeColor(tabName);
				tab = new Widgets.CRoundedTab (0, tabY, 100, tabHeight-3, tabName, pnlBlock, pnlTab, colors[0], colors[1]);
				tab.Dock = Diagram.DockSide.Left;
				this.AddShape (tab);
				pnlBlock.BringToFront (this);

				foreach (Widgets.CRoundedTab tt in allTabs) tt.AddTab (tab);	// Add this tab to all other tabs
				tab.AddTabs (allTabs);											// Add all other tabs to this tab
				allTabs.Add (tab);												// Add this tab to list of all tabs

				// Add blocks to tab
				foreach (CBlock cblock in blocks) {
					if ( tabMap[cblock] == tabName ) {
						this.AddShape (cblock);
						tab.AddShape (cblock);
					}
				}
			}

			// Select the last new tab
			if (tab != null) tab.SetToggle (this, true);

			// Move the scrollbar to the top and reconfigure panel.
			pnlTab.YScrollbar.BringToFront(this);
			pnlTab.DoConfigure(this);

			// Redraw
			this.Invalidate ();

			return true;
		}

//		// - - - Use Library with given colors - - - - - - - - - - - - - - - - - - - -
//		public bool UseLibraryDLL(string dllfile, Color fill_color, Color line_color)
//		{
//			// Load and build blocks from assembly dllfile
//			List<CBlock > blocks = new List<CBlock> ();
//			//Dictionary<String,CBlock> tabMap = new Dictionary<string, CBlock>();
//
//			int y = 34;
//			CBlock block = null;
//
//			string assembly_name = System.IO.Path.GetFileNameWithoutExtension (dllfile);
//			System.Reflection.Assembly assembly = engine.LoadAssembly (dllfile);
//
//			foreach (string type_name in Reflection.Utils.getTypeNames(assembly)) {
//				Type type = Reflection.Utils.getType (assembly, type_name);
//
//				// Skip constructors for now
//				foreach (System.Reflection.ConstructorInfo ci in Reflection.Utils.getConstructors(type)) {
//
//					// Look for attributes and skip if tab name is set to null
//					object[] attrs = ci.GetCustomAttributes(false); 
//					if (attrs.Length > 0) {
//						if ( attrs[0].GetType() == typeof(JigsawTabAttribute) ){
//						//if ( attrs[0].GetType().ToString () == typeof(JigsawTabAttribute).ToString () ) {
//							// If null is set for tab name, do not display
//							JigsawTabAttribute tattr = (JigsawTabAttribute)attrs[0];
//							if ( tattr.Name == null ) continue;
//						}
//					}
//
//					List<string > param_names = new List<string> ();
//					List<string > param_type_names = new List<string> ();
//					List<string > param_defaults = new List<string> ();
//					foreach (System.Reflection.ParameterInfo pi in ci.GetParameters()) {
//						param_names.Add (pi.Name);
//						param_type_names.Add (pi.ParameterType.FullName);
//						param_defaults.Add (pi.DefaultValue.ToString ());
//					}
//					string return_type = type_name;
//					block = new CMethodBlock (110, y, assembly_name, type_name, type_name,
//                                                    param_names, param_type_names, param_defaults,
//                                                    return_type, pnlBlock);
//					block.Visible = false;
//					block.LineColor = line_color;
//					block.FillColor = fill_color;
//					blocks.Add (block);
//					y += 40;
//				}
//
//				foreach (System.Reflection.MethodInfo mi in Reflection.Utils.getStaticMethods(type)) {
//
//					// Look for attributes and skip if tab name is set to null
//					object[] attrs = mi.GetCustomAttributes(false); 
//					if (attrs.Length > 0) {
//						if ( attrs[0].GetType() == typeof(JigsawTabAttribute) ){
//							// If null is set for tab name, do not make block
//							JigsawTabAttribute tattr = (JigsawTabAttribute)attrs[0];
//							if ( tattr.Name == null ) continue;
//						}
//					}
//
//					List<string > param_names = new List<string> ();
//					List<string > param_type_names = new List<string> ();
//					List<string > param_defaults = new List<string> ();
//					foreach (System.Reflection.ParameterInfo pi in mi.GetParameters()) {
//						param_names.Add (pi.Name);
//						param_type_names.Add (pi.ParameterType.FullName);
//						try {
//							param_defaults.Add (pi.DefaultValue.ToString ());
//						} catch {
//							param_defaults.Add ("");
//						}
//					}
//					string return_type = mi.ReturnType.ToString ();
//					block = new CMethodBlock (110, y, assembly_name, type_name, mi.Name,
//                                                param_names, param_type_names, param_defaults,
//                                                return_type, pnlBlock);
//					block.Visible = false;
//					block.LineColor = line_color;
//					block.FillColor = fill_color;
//					blocks.Add (block);
//					y += 40;
//				}
//			}
//
//
//			// Get next available tab location
//			double tabY = 0.0;
//			foreach (Widgets.CRoundedTab tt in allTabs) {
//				if (tt.Top > tabY)
//					tabY = tt.Top;
//			}
//
//			// Add tab
//			tabY += 33.0;
//			Widgets.CRoundedTab tab = new Widgets.CRoundedTab (0, tabY, 100, 30, assembly_name, pnlBlock);
//			tab.Dock = Diagram.DockSide.Left;
//			this.AddShape (tab);
//			pnlBlock.BringToFront (this);
//
//			// Add this tab to all other tabs
//			foreach (Widgets.CRoundedTab tt in allTabs)
//				tt.AddTab (tab);
//
//			// Add all other tabs to this tab
//			tab.AddTabs (allTabs);
//
//			// Add this tab to list of all tabs
//			allTabs.Add (tab);
//
//			// Add blocks
//			foreach (CBlock cblock in blocks) {
//				this.AddShape (cblock);
//				tab.AddShape (cblock);
//			}
//
//			// Select the new tab
//			tab.SetToggle (this, true);
//
//			// Redraw
//			this.Invalidate ();
//
//			return true;
//		}

		// - - - Load and build blocks from assembly map xml - - - - - - - - - - - - - - -
		public bool UseLibraryMap(TextReader tr)
		{
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
			Color fill_color = Diagram.Colors.LightBlue;
			Color line_color = Diagram.Colors.DarkBlue;
			double R, G, B, A;
			
			try
			{
				using (XmlReader xr = XmlTextReader.Create(tr)) {
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
								
							case "docstring":
								// TODO: Do something with document string
								break;
								
							case "fill_color":
								R = fill_color.R;
								G = fill_color.G;
								B = fill_color.B;
								A = fill_color.A;
								
								Double.TryParse (xr.GetAttribute("red"), out R);
								Double.TryParse (xr.GetAttribute("green"), out G);
								Double.TryParse (xr.GetAttribute("blue"), out B);
								Double.TryParse (xr.GetAttribute("alpha"), out A);
								
								fill_color = new Color(R,G,B,A);

								break;
								
							case "line_color":
								R = line_color.R;
								G = line_color.G;
								B = line_color.B;
								A = line_color.A;
								
								Double.TryParse (xr.GetAttribute("red"), out R);
								Double.TryParse (xr.GetAttribute("green"), out G);
								Double.TryParse (xr.GetAttribute("blue"), out B);
								Double.TryParse (xr.GetAttribute("alpha"), out A);
								
								line_color = new Color(R,G,B,A);
								
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
								block.FillColor = fill_color;
								block.LineColor = line_color;
					      		blocks.Add(block);
					      		y += 40;
		
								break;
								
			                case "constructor":
								return_type = type_name;
						    	block = new CMethodBlock(110, y, assembly_name, type_name, type_name, 
										    			 param_names, param_type_names, param_defaults, 
								                         return_type, pnlBlock);
								block.Visible = false;
								block.FillColor = fill_color;
								block.LineColor = line_color;
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
				tabY += tabHeight;
				List<Color> colors = makeColor(assembly_name);
				Widgets.CRoundedTab tab = new Widgets.CRoundedTab(0, tabY, 100, tabHeight-3, assembly_name, pnlBlock, pnlTab,
												colors[0], colors[1]);
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

				// Move the scrollbar to the top and reconfigure panel.
				pnlTab.YScrollbar.BringToFront(this);
				pnlTab.DoConfigure(this);

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
		
		// - - - Set/get the mode that automatically displays the properties dialog - - -
		public bool AutoProperties
		{
			set {
				_autoProperties = value;
			}
			
			get {
				return _autoProperties;
			}
		}
		
		// - - - Set/get the rate that the engine timer runs - - - - - - -
		public double TimeOut 
		{
			set
			{
				uint newTimeOut;
				UpdateDisplay = true;

				// if timeout is too small, and not already paused, then pause and be done
				if (value <= 0.0) {
					if (this.State != RunningState.Paused) {
						_engine.Pause ();
						Console.WriteLine ("Stepping...");
					}
					return;
				}

				// Compute a new timeout
				newTimeOut = (uint)(2000.0/value);

				// If the new timeout hasn't changed, then nothing to do
				if ( _engine.TimeOut == newTimeOut ) return;

				// If get this far, then we have a new timeout to set
				_engine.TimeOut = newTimeOut;

				// If currently paused and a new timeout was set, then start running
				if (this.State == RunningState.Paused) this.Run();
			}
			get
			{
				return 2000.0/_engine.TimeOut;
			}
		}

		// - - - Set internal flag indicating if display should update while running - - - - - - -
		public bool UpdateDisplay 
		{
			set
			{
				_updateDisplay = value;
			}
			get
			{
				return _updateDisplay;
			}
		}

		// - - - Get internal flag indicating if running - - - - - - -
//		public bool IsRunning 
//		{
//			get
//			{
//				return _engine.IsRunning; //_isRunning;
//			}
//		}

		// - - - Raise the JigsawRun event  - - - - - - - - - - - - - - 
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

		// - - Raise the JigsawError event  - - - - - - - - - - - - - - 
        public void RaiseJigsawError()
        {
            if (JigsawError != null)
            {
				EventArgs e = new EventArgs();
            	JigsawError(this, e);
            }
        }

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void RunBlockStack(CBlock b)
		{
			if (_engine.State != EngineState.Paused) {
				Console.WriteLine ("Resetting");
				//Console.WriteLine (_engine.State);
				_engine.Reset(this, _inspector);
			}

			// Load block onto call stack enabled
			_engine.LoadBlockStack(b, true);
			
			// If engine is not running, kick start by calling step
			if (_engine.State != EngineState.Running) _engine.Step ();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void Run()
		{
			if (_engine.State != EngineState.Paused) {
				Console.WriteLine ("Resetting");
				//Console.WriteLine (_engine.State);
				_engine.Reset(this, _inspector);
			}
			_engine.Run();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void Stop()
		{
			_engine.Stop();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void Pause()
		{
			if (this.State == RunningState.Paused) return;
			_engine.Pause();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void Step()
		{
			_engine.Step(true);
			RaiseJigsawStep();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void Reset()
		{
			engine.Reset(this, _inspector);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		void OnEngineRun(object sender, EventArgs e)
		{
			_state = RunningState.Running;
			RaiseJigsawRun();
			this.Invalidate();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		void OnEngineStep(object sender, EventArgs e)
		{	
			if (_updateDisplay) this.Invalidate();
			//while (Gtk.Application.EventsPending ()) Gtk.Application.RunIteration ();
			//RaiseJigsawStep ();	// This is done in Step()
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		void OnEngineReset(object sender, EventArgs e)
		{	
			//Console.WriteLine ("Jigsaw.OnEngineReset()");
			this.Invalidate();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		void OnEngineStop(object sender, EventArgs e)
		{
			//Console.WriteLine ("Jigsaw.OnEngineStop() - State={0}", this._state);
			
			// If running and not paused, go to idle and raise a stopped event.
			// If paused, do not stop. Need to maintain state.
			if (_state != RunningState.Paused) {
				_state = RunningState.Idle;
				RaiseJigsawStop();
			}
			this.Invalidate();
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		void OnEnginePause(object sender, EventArgs e)
		{	
			_state = RunningState.Paused;
			this.Invalidate();
			RaiseJigsawPause();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		void OnEngineError(object sender, EventArgs e)
		{
			_state = RunningState.Error;
			RaiseJigsawError();
		}

		// - - - Update Modified when block changes - - - - - - - - - - - -
		public void OnBlockChanged(object sender, EventArgs e)
		{
			this.Modified = true;
		}


		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
		public List<Widgets.CRoundedTab> AllTabs()
		{
		    return allTabs;
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
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
		// Get all blocks on a Tab
		public List<Diagram.CShape> AllShapes(int position)
		{
		    Widgets.CRoundedTab tab = AllTabs()[position];
			List<Diagram.CShape> shapes = new List<Diagram.CShape>();
			
			foreach (Diagram.CShape s in tab.AllShapes()) {
			    shapes.Add(s);
			}
			return shapes;
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void ToggleBreakPoint()
		{
			foreach (Diagram.CShape s in this.AllShapes()) {
				if (s is CBlock) {
					CBlock b = (CBlock)s;
					if (!b.IsFactory && b.Selected) {
						b.ToggleBreakPoint();
					}
				}
			}
			this.Invalidate();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public bool HasBreakPointSet()
		{
			foreach (Diagram.CShape s in this.AllShapes()) {
				if (s is CBlock) {
					CBlock b = (CBlock)s;
					if (!b.IsFactory && b.BreakPoint) {
						return true;
					}
				}
			}
			return false;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void AddBlock(CBlock b)
		{	// Add a block to the canvas
			this.AddShape (b);
			b.BlockChanged += OnBlockChanged;
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void DeleteBlock(CBlock b) 
		{	// Properly delete given block
			CEdge outEdge = null;
			CEdge inEdge = null;

			Diagram.Canvas cvs = (Diagram.Canvas)this;
			b.Deselect(cvs);
			b.Deactivate(cvs);

			// If there are blocks connected to the input and output edges, then save references for subsequent reconnection.
			if (b.InEdge.IsConnected && b.OutEdge.IsConnected) {
				outEdge = b.InEdge.LinkedTo;
				inEdge = b.OutEdge.LinkedTo;
			}

			// Break all connections
			b.Disconnect();

			// Reconnect, if possible
			if (outEdge != null && inEdge != null) {
				outEdge.LinkedTo = inEdge;
				inEdge.LinkedTo = outEdge;
			}

			b.StopOutline (cvs);
			b.BlockChanged -= OnBlockChanged;
			cvs.DeleteShape(b);
		}
		
		// - - - Delete an entire stack of blocks given the block that is the top of stack - - - - -
		public void DeleteStack(CBlock block)
		{
			CBlock top = block.StackTop;
			
			// Maintain a stack of blocks to be deleted and we progress down through the tree
			List<CBlock> toDelete = new List<CBlock>();
			toDelete.Add(block);
			
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
				this.DeleteBlock(nextBlock);
			}
			
			top.RepositionBlocks(null);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void DeleteAllBlocks() 
		{	// Delete all blocks in preparation for a new program 
			Diagram.Canvas cvs = (Diagram.Canvas)this;
			foreach (CBlock b in AllBlocks()) {
				b.Deselect(cvs);
				b.Deactivate(cvs);
				b.Disconnect();
				b.BlockChanged -= OnBlockChanged;
				cvs.DeleteShape(b);
			}
		}
		
		// - - - Bring an entire stack of blocks to the front - - - - -
		public void BringStackToFront(CBlock block)
		{
			// Maintain a stack of blocks to be brought to top as we progress down through the tree
			List<CBlock> toProcess = new List<CBlock>();
			toProcess.Add(block);
			
			while (toProcess.Count > 0) {
				// Get the block on top of the stack
				CBlock nextBlock = toProcess[0];
				toProcess.RemoveAt (0);
				this.BringToFront (nextBlock);
				
				// Add all output child blocks to list of blocks to be deleted
				foreach (CEdge e in nextBlock.Edges) {
					if ( e.Type != EdgeType.In && e.IsConnected ) 
						toProcess.Add(e.LinkedTo.Block);
				}
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void DeactivateAllBlocks() 
		{	// Deactivate all blocks
			Diagram.Canvas cvs = (Diagram.Canvas)this;
			foreach (CBlock b in AllBlocks()) b.Deactivate(cvs);
		}

		// - - - Checks to see if a block is selected - - - - -
		public bool HasSelection()
		{
			foreach (Diagram.CShape s in this.AllShapes()) {
				if (s is CBlock) {
					CBlock b = (CBlock)s;
					if (!b.IsFactory && b.Selected) {
						return true;
					}
				}
			}
			return false;
		}

		// - - - Returns selected block - - - - -
		public object GetSelection()
		{
			foreach (Diagram.CShape s in this.AllShapes()) {
				if (s is CBlock) {
					CBlock b = (CBlock)s;
					if (!b.IsFactory && b.Selected) {
						return b;
					}
				}
			}
			return null;
		}

		// - - - Returns a dictionary that holds the bounds of the current blocks, including factories - - - - -
		public Dictionary<string,double> GetBlockBounds()
		{
			Dictionary<string,double> bounds = new Dictionary<string, double>();
			
			foreach (Diagram.CShape s in this.AllShapes()) {
				if (s is CBlock) {
					CBlock bl = (CBlock)s;
					if (!bl.IsFactory) {
						
						double t = bl.Top;
						double l = bl.Left;
						double r = l + bl.Width;
						double b = t + bl.Height;
						
						// Left
						if (!bounds.ContainsKey("Left")) {
							bounds["Left"] = l;
						} else if (l < bounds["Left"]) {
							bounds["Left"] = l;
						}
						
						// Right
						if (!bounds.ContainsKey("Right")) {
							bounds["Right"] = r;
						} else if (r > bounds["Right"]) {
							bounds["Right"] = r;
						}
						
						// Top
						if (!bounds.ContainsKey("Top")) {
							bounds["Top"] = t;
						} else if (t < bounds["Top"]) {
							bounds["Top"] = t;
						}
						
						// Bottom
						if (!bounds.ContainsKey("Bottom")) {
							bounds["Bottom"] = b;
						} else if (b > bounds["Bottom"]) {
							bounds["Bottom"] = b;
						}
					}
				}
			}
			return bounds;
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void OnMouseDown(Diagram.Canvas cvs, Diagram.MouseEventArgs e)
        {
        	if (this.Mode == Diagram.EMode.Editing)
        	{
	            int ndeselected = 0;									// Deselect all if click on canvas with no shift key
				if ((this.ModifierKeys & Gdk.ModifierType.ShiftMask) == 0) ndeselected = this.DeselectAll();
	            if (ndeselected > 0) this.RaiseSelectionChangedEvent();	// Indicate that the canvas selection has changed

				// Intercept the right-mouse
				if (e.Button == Diagram.MouseButtons.Right) 
				{
					this.ShowContextMenu((int)e.X, (int)e.Y);
				} else {
					this.EditMode = Diagram.EMode.TranslatingStart;		// Start translating diagram
				}

				this.Invalidate();										// Redraw
			}
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
					
//					this.offsetX += dx;
//					this.offsetY += dy;
					this.DoScroll(dx, dy);

					this.EditMode = Diagram.EMode.Translating;
					
					// Clip offsets
					//this.ClipOffsets();
					
					// Find all docked shapes and redock them
					//this.ReDockShapes(dx, dy);
					this.FixAbsolutePositionedShapes();
					
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

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void OnScroll(Diagram.Canvas cvs, Gdk.EventScroll scl)
		{	// Handle DrawingArea wheel scroll events
			double deltaX = 0.0;
			double deltaY = 0.0;

			if (scl.Direction == Gdk.ScrollDirection.Up) {
				if ((this.ModifierKeys & Gdk.ModifierType.ControlMask) > 0) {		// If control key is pressed
					this.DoZoom (1.05);												// zoom up
				} else if ((this.ModifierKeys & Gdk.ModifierType.ShiftMask) > 0) {	// If shift key is pressed
					deltaX = 20.0;													// scroll right
				} else {															// If neither is pressed
					deltaY = 20.0;													// scroll down
				}
			} else if (scl.Direction == Gdk.ScrollDirection.Down) {
				if ((this.ModifierKeys & Gdk.ModifierType.ControlMask) > 0) {		// If control key is pressed
					this.DoZoom (1.0/1.05);											// zoom down
				} else if ((this.ModifierKeys & Gdk.ModifierType.ShiftMask) > 0) {	// If shift key is pressed
					deltaX = -20.0;													// scroll left
				} else {															// If neither is pressed
					deltaY = -20.0;													// scroll up
				}
			} else if (scl.Direction == Gdk.ScrollDirection.Left) {
				deltaX = -20.0;
			} else if (scl.Direction == Gdk.ScrollDirection.Right) {
				deltaX = 20.0;
			}

			this.DoScroll(deltaX, deltaY);
			this.FixAbsolutePositionedShapes();
			this.Invalidate();
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void OnResetScrollZoom(object sender, EventArgs e)
		{
			this.DoScroll(-this.offsetX, -this.offsetY);
			this.DoResetZoom();
			this.FixAbsolutePositionedShapes();
			this.Invalidate();
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void DoScroll(double dX, double dY)
		{	// Test if a scroll is acceptible based on window bounds and block positions
			// and perform is acceptible.

			this.offsetX += dX;
			this.offsetY += dY;

//			int cvsHeight = Convert.ToInt32(this.Allocation.Height/this.scale);
//			int cvsWidth = Convert.ToInt32(this.Allocation.Width/this.scale);

//			double barFrac = cvsHeight/10000.0; //  stackHeight/ySpan;
//			double barLoc = -this.offsetY/10000.0; //(ySpanMax - maxBottom)/ySpan;		// Offset from the bottom
//			double barSpan = 1.0; //ySpan/cvsHeight;
			
			//ybar.Update((cvsWidth - ybar.Width), -this.offsetY, ybar.Width, cvsHeight, barFrac, barLoc, barSpan);
			//ybar.Update(200.0, 0.0, ybar.Width, cvsHeight, 0.1, 0.5, barSpan);
			//Console.WriteLine ("{0} {1} {2} {3} {4} {5} {6}", (cvsWidth - ybar.Width), -this.offsetY, ybar.Width, cvsHeight, barFrac, barLoc, barSpan);
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected void OnHelpShow(object sender, EventArgs e)
		{
			string target = String.Format ("http://calicoproject.org/Calico_Jigsaw");
			try
			{
				System.Diagnostics.Process.Start(target);
			} catch (System.Exception other) {
				Console.WriteLine(other.Message);
			}
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected void OnContextRun(object sender, EventArgs e)
		{
			this.Run ();
		}

		// - - - Create the context menu for this block - - - - - - - - - - -
		protected virtual bool ShowContextMenu(int X, int Y) 
		{
			// Cache info
			_X = X;
			_Y = Y;
			
			// Create and show context menu
			Gtk.Menu mnu = new Gtk.Menu();

			Gtk.MenuItem mnuRun = new Gtk.MenuItem("Run");
			mnuRun.Activated += OnContextRun;
			
			Gtk.MenuItem mnuToggleInset = new Gtk.MenuItem("Toggle _Inset");
			mnuToggleInset.Activated += OnViewToggleInset;

			Gtk.MenuItem mnuResetScroll = new Gtk.MenuItem("_Reset Scroll and Zoom");
			mnuResetScroll.Activated += OnResetScrollZoom;

			Gtk.MenuItem mnuHelp = new Gtk.MenuItem("Help");
			mnuHelp.Activated += OnHelpShow;
			
			mnu.Append(mnuToggleInset);
			mnu.Append(mnuResetScroll);
			mnu.Append(mnuRun);
			//OnResetScroll
			mnu.Append( new Gtk.SeparatorMenuItem() );
			mnu.Append(mnuHelp);
			
			mnu.ShowAll();
			mnu.Popup();
			
			return true;
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		protected void OnInspectorShow(object sender, EventArgs e)
//		{
//			this.ShowInspectorWindow();
//		}
//
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		public void ShowInspectorWindow()
//		{
//			_inspector.ShowAll();
//			_inspector.Deiconify();
//			_inspector.SetPosition(Gtk.WindowPosition.Mouse);
//			_inspector.KeepAbove = true;	// The Mono 2.6.7 runtime needs this here for the Window to stay above others
//		}

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
		public string ToXml() 
		{
	       	XmlWriterSettings settings = new XmlWriterSettings();
	       	settings.Indent = true;
			settings.IndentChars = "    ";
			settings.Encoding = Encoding.ASCII;
			StringBuilder sb = new StringBuilder();
			using (XmlWriter xw = XmlWriter.Create(sb, settings)) {
				this.ToXml(xw);
			}
			// FIXME: why does this have utf-16 when we explicitly say ASCII?
			return sb.ToString().Replace ("encoding=\"utf-16\"?>", "encoding=\"us-ascii\"?>");
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
					
					// Add .jig extension if missing
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
		public string OnFileSaveAsPython(object sender, EventArgs e)
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
			
			// Suggest a name:
			if ( _currentPath != null) {
			    string suggestedName = _currentPath;
			    if (suggestedName.EndsWith(".jig", StringComparison.OrdinalIgnoreCase))	
				suggestedName = suggestedName.Substring(0, suggestedName.Length - 4) + ".py";
			    fc.SetFilename(suggestedName);
			    fc.CurrentName = System.IO.Path.GetFileName(suggestedName);
			} else {
			    fc.SetFilename("JigsawExport.py");
			    fc.CurrentName = "JigsawExport.py";
			}
			
			// Collect the path
			int response = fc.Run();
			String ppath = null;
			// Save the path and go to OnFileSave for the actual save
			if (response == (int)Gtk.ResponseType.Accept) 
			{
				ppath = fc.Filename;
				
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
			
			return ppath;
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
			CControlStart b1 = new CControlStart(125 + CBlock.BlockWidth + 20, 36);
			this.AddBlock(b1); //this.AddShape(b1);
			
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

				// Reset the offsets and reposition absolute shapes.
				this.ResetOffsets ();

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
			// Start by wiping out what's there
			this.DeleteAllBlocks();
			CurrentPath = filename;
			
			XmlWrapper xr = null;
			xr = new XmlWrapper(filename);
			return ProcessXml(xr);
		}

		public bool ProcessXml(XmlWrapper xr) {
			return ProcessXml (xr, 0, 0);
		}

		int intParse(XmlWrapper xr, string attr) {
		    int retval = 0;
		    string value = "";
		    try {
			value = xr.GetAttribute(attr);
			retval = int.Parse(value);
		    } catch {
			System.Console.Error.WriteLine(String.Format("Error reading '{0}' from '{1}'; continuing... ", attr, value));
		    }
		    return retval;
		}

		public bool ProcessXml(XmlWrapper xr, int dx, int dy) {
			// Temp vars to hold parse vals
			string name, val, typeName = "";
			double R, G, B, A, X=0.0, Y=0.0;
			int id=0, edgeId=0;
			CBlock tBlock = null;
			CMethodBlock tMethodBlock = null;
			CEdge tEdge = null;
			CEdge tLinkedEdge = null;
			Type typ;
			System.Object[] args = null;
			
			// First Step : Read XML and create all blocks 

			// Build dictionaries of CBlock and CEdge objects
			Dictionary<string,CBlock> blocks = new Dictionary<string, CBlock>();
			Dictionary<string,CEdge> edges = new Dictionary<string, CEdge>();
			
			// FIXME: add a unserialize to Block to get additional info
			while (xr.Read())
			{
				switch (xr.NodeType) {
				case XmlNodeType.Element:
					name = xr.Name.ToLower();
					
					switch (name) {
					case "jigsaw":
						break;
					
					case "module":
						string modName = xr.GetAttribute("name");
						string mpath = System.IO.Path.Combine( ModulePath, modName );
						if (!engine.loadedAssemblies.ContainsKey(mpath)) UseLibrary (mpath);
						break;
					
					case "block":		// <block id="1" typeName="Jigsaw.CControlStart" left="415" top="50">							
						typeName = xr.GetAttribute("typeName");
						X = intParse(xr, "left") + dx;
						Y = intParse(xr, "top") + dy;
						id = intParse(xr, "id");

						// This will be set to a non-null value if we are dealing with a CMethodBlock.
						// Otherwise, it remains null as a signal that we do not have one.
						tMethodBlock = null;

						typ = Type.GetType(typeName);
						args = new System.Object[] {X, Y};
						tBlock = (CBlock)Activator.CreateInstance(typ, args);
						blocks["b"+id.ToString()] = tBlock;
						this.AddBlock(tBlock); //this.AddShape(tBlock);
						break;

					case "method":	//<method assembly_name="Processing" type_name="Processing" method_name="year" return_type="System.Int32" />
						string methodAssembly = xr.GetAttribute("assembly_name");
						string methodType = xr.GetAttribute("type_name");
						string methodName = xr.GetAttribute("method_name");
						string methodReturnType = xr.GetAttribute("return_type");

						tMethodBlock = (CMethodBlock)tBlock;
						tMethodBlock.AssemblyName = methodAssembly;
						tMethodBlock.MethodName = methodName;
						tMethodBlock.TypeName = methodType;
						tMethodBlock.ReturnType = methodReturnType;

						break;

					case "parameter":
						// Add parameters as block properties. This tag is only found with MethodBlocks.
						string paramName = xr.GetAttribute("name");
						string paramType = xr.GetAttribute("type");
						string paramDefault = xr.GetAttribute("default");
						tMethodBlock.ParamNames.Add ( paramName );
						tMethodBlock.ParamTypes.Add ( paramType );
						tMethodBlock.ParamDefaults.Add ( paramDefault );
						tMethodBlock.AddExpressionProperty(paramName, paramType, paramDefault);
						break;

					case "edge":		// <edge id="3" name="Out" type="Out" linkedTo="4" />
						name = xr.GetAttribute("name");
						edgeId = intParse(xr, "id");
						edges["e"+edgeId.ToString()] = tBlock.GetEdgeByName(name);
						break;

					case "property":	// <property name="variable" value="X" />
						// Add a property to the block
						try {
							name = xr.GetAttribute("name");
							val = xr.GetAttribute("value");
							//Console.WriteLine ("ProcessXml {0} {1} {2}", tBlock.Text, name, val);
							//foreach (CProperty p in tBlock.Properties) Console.WriteLine ("{0} {1}", tBlock.Text, p.Name);
							//tBlock.SetProperty(name, val);
							tBlock[name] = val;

						} catch (Exception ex) {
							Console.WriteLine("Error in Jigsaw.Canvas.ProcessXml while reading property {0}: {1}", name, ex.Message);
						}
						break;
					
					case "fill_color":
						R = double.Parse(xr.GetAttribute("R"));
						G = double.Parse(xr.GetAttribute("G"));
						B = double.Parse(xr.GetAttribute("B"));
						A = double.Parse(xr.GetAttribute("A"));
						tBlock.FillColor = new Color(R,G,B,A);
						break;
					
					case "line_color":
						R = double.Parse(xr.GetAttribute("R"));
						G = double.Parse(xr.GetAttribute("G"));
						B = double.Parse(xr.GetAttribute("B"));
						A = double.Parse(xr.GetAttribute("A"));
						tBlock.LineColor = new Color(R,G,B,A);
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
			//xr = null;
			
			// === Second step : Reread XML and link up edges
			xr = xr.Clone(); // = new XmlTextReader(filename);

			while (xr.Read()) 
			{	
				if (xr.NodeType == XmlNodeType.Element) 
				{
					name = xr.Name.ToLower();
					
					if (name == "block") {
						// Get a reference to the current block
					        id = intParse(xr, "id");
						tBlock = blocks["b"+id.ToString()];

					} else if (name == "edge") {
						// Link block edges
					        edgeId = intParse(xr, "linkedTo");
						if (edgeId > 0) {
						        id = intParse(xr, "id");
							if (edges.ContainsKey("e"+id.ToString()) && 
						    	edges.ContainsKey("e"+edgeId.ToString())) {
								tEdge = edges["e"+id.ToString()];
								tLinkedEdge = edges["e"+edgeId.ToString()];
								tEdge.LinkedTo = tLinkedEdge;
								tLinkedEdge.LinkedTo = tEdge;
							}
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
			Dictionary<string, string> allAssemblies = new Dictionary<string, string>();
			
			List<CControlStart> sblocks = new List<CControlStart>();
			foreach (CBlock b in allBlocks)
			{
				if (b is CControlStart) {
					sblocks.Add((CControlStart)b);
				}
				
				// Identify the required library to import for this block, if any
				//foreach (string asm in b.RequiredAssemblies) allAssemblies[asm] = null;
			}
			
			if (sblocks.Count == 0) {
				Console.Error.WriteLine("Error. At least one Control Start Block must exist for Python generation.");
				return null;
			}
			
			// Add header
			StringBuilder o = new StringBuilder();
			
			o.AppendLine("# This Python program was automatically generated by Calico Jigsaw");
			o.AppendLine("# http://calicoproject.org");
			o.AppendLine();
			
			// Do necessary imports here
			// This is not necessary if run in Calico:
			//o.AppendLine ("import clr");
			//foreach (string k in allAssemblies.Keys) o.AppendFormat("clr.AddReference('{0}')\n", k);
			//foreach (string k in allAssemblies.Keys) o.AppendFormat("import {0}\n", k);
			foreach (string dllPath in engine.loadedAssemblies.Keys) {
				string assemblyName = System.IO.Path.GetFileNameWithoutExtension(dllPath);
				o.AppendFormat("import {0}\n", assemblyName);
			}
			// Special imports for specific functionality
			if (sblocks.Count > 1) {
				o.AppendLine("from Myro import doTogether");
			}
			// Look for exit, for sys
			bool import_sys = false;
			foreach (CBlock b in allBlocks)
			{
				if (b is CControlEnd) {
					import_sys = true;
					break;
				}
			}
			if (import_sys) {
				o.AppendLine("import sys");
			}
			o.AppendLine ();

			// Generate all procedures
			foreach (CBlock b in allBlocks)
			{
				if (b is CProcedureStart) {
					b.ToPython(o, 0);
					o.AppendLine();			// Add blank line to end of all procedures
				}
			}
			
			if (sblocks.Count > 1) {
				int count = 1;
				string functions = "";
				foreach(CControlStart sblock in sblocks) {
					o.AppendFormat("def main{0}():\n", count);
					sblock.ToPython(o, 1);
					if (functions != "") functions += ", ";
					functions += String.Format("main{0}", count);
					o.AppendLine();
					count++;
				}
				o.AppendFormat("doTogether({0})\n", functions);
			} else {
				// Generate main program
				sblocks[0].ToPython(o, 0);
			}
			
			// Return result
			return o.ToString();
		}
		
		// - - - Completely replace serialization of Jigsaw Program - - - - - -
        public override void ToXml(XmlWriter w)
        {
            w.WriteStartElement("jigsaw");
			
			// Write all loaded modules to the Jigsaw file
			foreach (string dllPath in engine.loadedAssemblies.Keys) 
			{
				string modName = System.IO.Path.GetFileName(dllPath);
				w.WriteStartElement ("module");
				w.WriteAttributeString("name", modName);
				w.WriteEndElement();
			}
			
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
		
		// - - -
		public class SearchHelper 
		{
			int _startTab = 0;			// Position of first tab in search
			int _startBlock = 0;		// Position of first block in search
			int _currTab = 0;			// Position of current tab being searched
			int _currBlock = 0;			// Position of current block being searched
			bool _searchForward = true;	// Search direction	
			Canvas _cvs = null;			// Ref to Jigsaw.Canvas
			
			// - - - 
			public SearchHelper (Canvas cvs, bool forward) {
				_cvs = cvs;
				_searchForward = forward;
				
				// Find the tab currently selected
				_currTab = 0;
				Widgets.CRoundedTab tab = cvs.allTabs[_currTab];
				for (int i=0; i<cvs.allTabs.Count; i++) {
					if (cvs.allTabs[i].Toggled) {
						_currTab = i;
						tab = cvs.allTabs[i];
						break;
					}
				}
				
				// Find the factory block on the tab that is selected, if any
				_currBlock = 0;
				for (int i=0; i<tab._shapes.Count; i++) {
					Diagram.CShape shp = tab._shapes[i].Target as Diagram.CShape;
					if (shp is CBlock) {
						CBlock bb = (CBlock) shp;
						_currBlock = i;
						break;
					}
				}
				
				// Set the starting position for search 
				// so we know when we have made one whole loop 
				_startTab =_currTab;
				_startBlock = _currBlock;
			}
			
			// - - - Increment block to search
			private void Increment() 
			{
				Widgets.CRoundedTab tab = _cvs.allTabs[_currTab];
				_currBlock++;
				if (_currBlock >= tab._shapes.Count) {
					_currTab++;
					if (_currTab >= _cvs.allTabs.Count) _currTab = 0;	
					_currBlock = 0;
				}
			}

			// - - - Decrement block to search
			private void Decrement() 
			{
				_currBlock--;
				if (_currBlock < 0) {
					_currTab--;
					if (_currTab < 0) _currTab = _cvs.allTabs.Count-1;	
					Widgets.CRoundedTab tab = _cvs.allTabs[_currTab];
					_currBlock = tab._shapes.Count-1;
				}
			}

			// - - - Find the next matching factory block in the current direction
			public bool More(string s) 
			{
				if (s == "")
					return true;
				
				// Set up string to find
				string ss = s.ToLower();
				
				while (true) 
				{
					// Get current tab and shape
					Widgets.CRoundedTab tab = _cvs.allTabs[_currTab];
					Diagram.CShape shp = tab._shapes[_currBlock].Target as Diagram.CShape;
					
					// If shape is a block
					if (shp is CBlock) {
						CBlock bb = (CBlock) shp;
						
						// Get text of block
						string txt = bb.Text.Replace("`", "").ToLower();

						// A match is found. Select block.
						if (txt.Contains ( ss )) {
							// Select the tab
							tab.SetToggle(_cvs, true);
							
							// Shift found block into view. Account for canvas y-translate
							double dY = 70.0 - bb.Top; 
							_cvs.pnlBlock.DoScroll(_cvs, dY - _cvs.offsetY);
							
							// Select the found block
							_cvs.DeselectAll();
							bb.Select(_cvs);

							_cvs.Invalidate();
							return true;
						}
					}
					
					// Otherwise, increment tab and block position
					if (_searchForward == true) {
						this.Increment ();
					} else { 
						this.Decrement();
					}					
					
					//Console.WriteLine ("{0} {1} {2} {3}", _currTab, _startTab, _currBlock, _startBlock);
					
					// Check if all blocks have been searched
					if (_currTab == _startTab && _currBlock == _startBlock) break;
				}
				return false;
			}

			// - - - Find the next matching factory block in the forward direction
			public bool Next(string s) 
			{
				// If switching search directions, reset start
				if (_searchForward == false) {
					_searchForward = true;
					_startTab = _currTab;
					_startBlock = _currBlock;
				}
				this.Increment ();
				return this.More (s);
			}
			
			// - - - Find the next matching factory block in the reverse direction
			public bool Previous(string s) 
			{
				// If switching search directions, reset start
				if (_searchForward == true) {
					_searchForward = false;
					_startTab = _currTab;
					_startBlock = _currBlock;
				}
				this.Decrement ();
				return this.More (s);
			}
		}
		
	    // - - - Cancel a search and cleanup - - - - - - - 
	    public void SearchCancel() {
			_searchHelper = null;
		}
		
		public void SearchStart() {
			_searchHelper = new SearchHelper(this, true);
		}
		
	    // - - - Look for the next block matching the search string - - - - - - - 
	    public bool SearchNext(string s) {
			return _searchHelper.Next(s);
		}

	    // - - - Look for the previous block matching the search string - - - - - - - 
	    public bool SearchPrevious(string s) {
			return _searchHelper.Previous(s);
		}
		
	    // - - -
	    public bool SearchMore(string s) {
	        if (_searchHelper == null) 
				_searchHelper = new SearchHelper(this, true);
			return _searchHelper.More(s);
		}

	}
	
	// -----------------------------------------------------------------------
	public class CBlock : Diagram.CShape
	{	// Base block class
		protected RunningState _state = RunningState.Idle;	// Current state of this block
		public CEdge InEdge;							// By default, all Blocks have one main input edge and one main output edge
		public CEdge OutEdge;		
		
		protected int _textYOffset = 12;				// Y offset for when a block's text
		protected bool _hasBreakPoint = false;			// True if a has a debugging break point applied
		internal bool _breakStop = false;				// lastSelectedPage, marks a block as a stopping point for popping frames from stack 
														// when a break is executed. Should be set to true for all loops.
		protected Gtk.Window _propDialog = null;
		protected Gtk.Window _contextMenu = null;

		protected Dictionary<String, CProperty> _properties;

		private Widgets.CBlockPalette _palette = null;	// Reference to block palette, if a factory
		public static int BlockWidth = 275;             // default block size

		public event EventHandler BlockChanged;			// Raised when blocked changed and file needs resaving

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CBlock(List<Diagram.CPoint> pts, Widgets.CBlockPalette palette = null) : base(pts)
		{	// Constructor
			double offsetX = 0.5*this.Width;
			double offsetY = this.Height;
			
			this._palette = palette;
			if (palette != null) {
				this._isFactory = true;
				this.positionAbsolute = true;
				this.TopMost = true;
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
		public RunningState State
		{	// State getter/setter 
			get { return _state;  }
			set { _state = value; }
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void RaiseBlockChanged()
        {
            if (BlockChanged != null)
            {
				EventArgs e = new EventArgs();
				BlockChanged(this, e);
			}
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
			clone.Text = this.Text;
			
			// Clone edges if requested
			if (cloneEdges) {
				clone.InEdge = this.InEdge.Clone(clone);
				clone.OutEdge = this.OutEdge.Clone(clone);
			}
			return clone;
		}
		
		// - - - Access property values - - - - - - - - - - - - - - - - -
		public virtual string this[string key]
		{
			get { return _properties[key].Text;  }
			set { _properties[key].Text = value.Replace("`", ""); }
		}
		
		// - - Override Text property of basic shapes
        public override String Text
        {
            get { return this.text; }
            set {
				this.text = value;
				this["Label"] = value;		// Also set block label
				//this.TextProp.Text = value;
			}
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
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public virtual bool Compile( ScriptEngine engine, Jigsaw.Canvas cvs )
		{	// Compile the block code into something that can be executed.
			// This behavior is block-specific.
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public virtual StackFrame Frame(ScriptScope scope, CallStack stack ) 
		{
			StackFrame frm = new StackFrame(this, this.Runner (scope, stack), scope );
			return frm;
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
			this.State = RunningState.Running;				// Indicate that the block is running
			RunnerResponse rr = new RunnerResponse();		// Create and return initial response object
			yield return rr;
			if (this.BreakPoint == true) {					// Indicate if breakpoint is set on this block
				rr.Action = EngineAction.Pause;				// so that engine can stop
				//rr.Frame = null;
				yield return rr;
			}
			
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			// Custom behavior will occur here
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			
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
		
		// - - - Generate an return Python translation of a block - - - - -
		public virtual bool ToPython (StringBuilder o, int indent)
		{	// The default behavior is to add nothing.
			return true;
		}
		
		// - - - Return a list of all assembly names required for this block - - -
		public virtual List<string> RequiredAssemblies
		{
			get 
			{
				return new List<string>();
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // CBlock subclasses must provide a method that outputs an 
        // XML representation of itself as a string.
		public virtual string ToXml(Canvas cvs) 
		{
			StringBuilder sb = new StringBuilder();
			XmlWriterSettings settings = new XmlWriterSettings();
			settings.Indent = true;
			settings.IndentChars = "    ";
			settings.Encoding = Encoding.ASCII;
			using (XmlWriter xw = XmlWriter.Create(sb, settings)) {
				// Maintain a stack of blocks to be converted and we progress down through the tree
				List<CBlock> toXml = new List<CBlock>();
				toXml.Add(this);
				
	            // Number shapes
	            int idCount = 0;
				int eIdCount = 0;
	            foreach (CBlock b in cvs.AllBlocks())
	            {	// Assign temp id to block
	                idCount++;
	                b._id = idCount;
					
					// Assign temp ids to all edges
					foreach (CEdge e in b.Edges) {
						eIdCount++;
						e._id = eIdCount;
					}
	            }

				xw.WriteStartElement("jigsaw");
				while (toXml.Count > 0) {
					// Get the block on top of the stack
					CBlock nextBlock = toXml[0];
					toXml.RemoveAt(0);
					
					// Add all output child blocks to list of blocks to be output
					foreach (CEdge e in nextBlock.Edges) {
						if ( e.Type != EdgeType.In && e.IsConnected ) 
							toXml.Add(e.LinkedTo.Block);
					}
					// Process the block
					nextBlock.ToXml(xw);
				}
				// End jigsaw:
				xw.WriteEndElement();

				// Reset numbers:
	            // Must close by resetting all temp ids to 0 (unassigned)
	            // otherwise subsequent save may be incorrect.
	            foreach (CBlock b in cvs.AllBlocks())
	            {
	                b._id = 0;
					foreach (CEdge e in b.Edges) e._id = 0;
	            }
			}
			return sb.ToString();
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
            String FullName = typ.FullName;

            w.WriteAttributeString("id", this._id.ToString());
			w.WriteAttributeString("typeName", FullName);
			w.WriteAttributeString("left", this.Left.ToString());
			w.WriteAttributeString("top", ((int)this.Top).ToString());
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
			
			// Write block colors
			Color fill = this.FillColor;
			w.WriteStartElement("fill_color");
			w.WriteAttributeString("R", fill.R.ToString());
			w.WriteAttributeString("G", fill.G.ToString());
			w.WriteAttributeString("B", fill.B.ToString());
			w.WriteAttributeString("A", fill.A.ToString());
			w.WriteEndElement();
			
			Color line = this.LineColor;
			w.WriteStartElement("line_color");
			w.WriteAttributeString("R", line.R.ToString());
			w.WriteAttributeString("G", line.G.ToString());
			w.WriteAttributeString("B", line.B.ToString());
			w.WriteAttributeString("A", line.A.ToString());
			w.WriteEndElement();
        }
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void Draw(Cairo.Context g)
        {	// All blocks follow the same pattern for drawing.
			// Block subclasses only need to define the graphics path, fill color and text.
			// Draw block on the canvas
			
            // Cannot draw with negative width or height, so use bounding box points to draw
            double x = this.left;
            double y = this.top;
            double w = this.width;
            double h = this.height;
			
			g.Save();
			
            // Block outline
			SetPath(g);
			
			// Set fill color based on block state
			if (this._state == RunningState.Running) {
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
			
			// Add a connection indicator to in-edge, if connected
			if (InEdge.IsConnected) {
				g.Color = Diagram.Colors.SemiWhite;
				//g.Rectangle(x+0.5*w-10, y+1, 20, 3);
				g.MoveTo(x+0.5*w-20, y+1);
				g.LineTo(x+0.5*w, y+7);		// Governs height of inverted triangle
				g.LineTo(x+0.5*w+20, y+1);
				g.ClosePath();
				g.Fill ();
			}
			
			// Text
			DrawLabels(g);
			
			// Draw breakpoint, if set
			if (this._hasBreakPoint) {
				g.Color = Diagram.Colors.Red;
				g.MoveTo(x+2, y+7);
				g.Arc(x+7, y+7, 5, 0.0, 2.0*Math.PI);
				g.ClosePath();
				g.Fill();
			}
			
			// If in an error state, draw an x over top of the block
			if (this._state == RunningState.Error) {
				g.Color = new Color(1.0, 0.0, 0.0, 0.5);
				g.LineWidth = 5;
				g.MoveTo(x, y);
				g.LineTo(x+w, y+h);
				g.MoveTo(x+w, y);
				g.LineTo(x, y+h);
				g.Stroke();
				g.Color = Diagram.Colors.LightPink;
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
	            double h = this.height;
				
//				double cx = x + 0.5*w;
//				double cy = y + 0.5*20;
//				int layoutWidth, layoutHeight;
				
				// Get ready to render block text
				g.Color = this.TextColor;
				g.SelectFontFace(this.fontFace, this.fontSlant, this.fontWeight);
				g.SetFontSize(this.fontSize);
				TextExtents te = g.TextExtents(text);
				
				// If text is larger than block, clip to a more narrow region and add ellipses.
				// Otherwise, standard clip and display.
				if (te.Width >= w) {
					g.Save ();
					g.Rectangle(x, y+6.0+_textYOffset, w-12.0, -h);
					g.Clip ();
					ShowText(x+10.0, y+3.0+_textYOffset, g, text);
					g.Restore ();
					g.MoveTo (x+w-11.0, y+3.0+_textYOffset);
					g.ShowText("...");
				} else {
					g.Save ();
					g.Rectangle(x, y+6.0+_textYOffset, w, -h);
					g.Clip ();
					ShowText(x+10.0, y+3.0+_textYOffset, g, text);
					g.Restore ();
				}
				
//				Pango.Layout layout = Pango.CairoHelper.CreateLayout(g);
//				Pango.FontDescription desc = Pango.FontDescription.FromString(
//						   String.Format("{0} {1} {2}", this.fontFace, this.fontWeight, this.fontSize));
//				layout.FontDescription = desc;
//				layout.Alignment = Pango.Alignment.Left; //Center;
//				layout.Ellipsize = Pango.EllipsizeMode.End;
//				layout.Width = (int)((w-10.0)*Pango.Scale.PangoScale);
//				
//				layout.SetText(text);
//				g.MoveTo(x+10.0, y+3.0+_textYOffset);
//				Pango.CairoHelper.ShowLayout(g, layout);
//				
//				// Clean up?
//				layout.FontDescription = null;
//				desc.Dispose();
//				layout.Dispose();
            }
		}

		protected virtual void ShowHighlights (Cairo.Context g, string text)
		{
			if (text.Contains ("(") || text.Contains("[")) {
				g.Save ();
				int inside = 0;
				for(int i=0; i<text.Length; i++) {
					string c = text.Substring(i, 1);
					if (c == ")" || c == "]") 
						inside--;
					TextExtents te = g.TextExtents(c);
					if (inside > 0) {
						g.Color = Diagram.Colors.White;
						g.RelMoveTo(-1, 1);
						g.RelLineTo (0, - (10 + 2));
						g.RelLineTo (te.Width + 2, 0);
						g.RelLineTo (0, 10 + 2);
						g.RelLineTo (-(te.Width + 2), 0);
						g.ClosePath();
						g.Fill();
						g.RelMoveTo(1, -1); // back where we began
					}
					g.RelMoveTo(te.Width, 0); // advance width of character
					if (c == "(" || c == "[")
						inside++;
				}
				g.Restore ();
				g.Color = this.TextColor; // let's make sure it is back to original
			}
		}

		List<String> SplitIntoWords (string text)
		{
			List<String> retval = new List<String>();
			string delimiters = "(), `";
			string current = "";
			Stack<string> stack = new Stack<string>();
			for (int i=0; i<text.Length; i++) {
				string s = text.Substring(i, 1);
				if (stack.Count > 0 && stack.Peek() == "'") {
					current += s;
					if (s == "'") {
						retval.Add(current);
						current = "";
					}
				} else if (stack.Count > 0 && stack.Peek() == "\"") {
					current += s;
					if (s == "\"") {
						retval.Add(current);
						current = "";
					}
				} else if (stack.Count > 0 && stack.Peek() == "[") {
					current += s;
					if (s == "]") {
						retval.Add(current);
						current = "";
					}
				} else if (s == "\"") {
					current += "\"";
					stack.Push ("\"");
				} else if (s == "'") {
					current += "'";
					stack.Push ("'");
				} else if (s == "[") {
					current += "[";
					stack.Push ("[");
				} else if (delimiters.Contains(s)) {
					if (current != "")
						retval.Add(current);
					retval.Add(s);
					current = "";
				} else {
					current += s;
				}
			}
			if (current != "")
				retval.Add(current);
			return retval;
		}

		protected virtual void ShowText (double x, double y, Cairo.Context g, string text)
		{
			double cx = x, cy = y;
			if (text.Contains ("(") || text.Contains ("`")) { 
				// break it up
				int inside = 0;
				List<String> words = SplitIntoWords(text);
				for(int i=0; i<words.Count; i++) {
					string word = words[i];
					TextExtents te = g.TextExtents(word);
					if (word == ")") 
						inside--;
					else if (word == "`") {
						if (inside > 0) {
							inside--;
						} else {
							inside++;
						}
					}
					double w = te.Width; // space width
					double advance = te.XAdvance; // advance
					if (word == " ") {
						cx += 1.0;
						continue;
					}
					if (word != "`") {
						if (inside > 0 && word != ",") {
							// fill in background:
							g.Save ();
							g.Color = Diagram.Colors.White;
							g.MoveTo (cx - .25, cy + 2);
							g.LineTo (cx - .25, cy - 9.0); // height
							g.LineTo (cx + w + 1.0, cy - 9.0); // height
							g.LineTo (cx + w + 1.0, cy + 2);
							g.LineTo (cx - .25, cy + 2);
							g.ClosePath();
							g.Fill();
							g.Restore ();
							g.Color = Diagram.Colors.DarkBlue;
						} else {
							g.Color = this.TextColor;
						}
						g.MoveTo(cx, cy);
						g.ShowText (word);
						cx = cx + 1.0 + advance;
					}
					if (word == "(")
						inside++;
				}
				g.Color = this.TextColor; // let's make sure it is back to original
			} else {
				g.MoveTo(cx, cy);
				g.ShowText (text);
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

			Gtk.MenuItem mnuRun = new Gtk.MenuItem("Run");
			mnuRun.Activated += OnRunBlockStack;
			
			Gtk.MenuItem mnuBreak = new Gtk.MenuItem("Toggle _Breakpoint");
			mnuBreak.Activated += OnToggleBreakpoint;
			//Gtk.MenuItem mnuProps = new Gtk.MenuItem("Properties");
			//mnuProps.Activated += OnPropertiesShow;
			//Gtk.MenuItem mnuInspect = new Gtk.MenuItem("Inspector");
			//mnuInspect.Activated += OnInspectorShow;

			Gtk.MenuItem mnuToggleInset = new Gtk.MenuItem("Toggle _Inset");
			mnuToggleInset.Activated += _cvs.OnViewToggleInset;

			Gtk.MenuItem mnuHelp = new Gtk.MenuItem("Help");
			mnuHelp.Activated += OnHelpShow;
			
			mnu.Append(mnuDelBlock);
			mnu.Append(mnuDelStack);
			mnu.Append( new Gtk.SeparatorMenuItem() );
			mnu.Append(mnuToFront);
			mnu.Append(mnuToBack);
			mnu.Append(mnuBreak);
			mnu.Append( new Gtk.SeparatorMenuItem() );
			//mnu.Append(mnuProps);
			//mnu.Append(mnuInspect);
			mnu.Append(mnuToggleInset);
			mnu.Append(mnuRun);
			mnu.Append( new Gtk.SeparatorMenuItem() );
			mnu.Append(mnuHelp);
			
			mnu.ShowAll();
			mnu.Popup();
			
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
		//protected void OnToggleInset(object sender, EventArgs a)
		//{	
		//	this.ToggleInset();
		//	this.Invalidate();
		//}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		protected void OnInspectorShow(object sender, EventArgs e)
//		{
//			(_cvs as Jigsaw.Canvas).ShowInspectorWindow();
//			_cvs = null;
//		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected void OnHelpShow(object sender, EventArgs e)
		{
			string target = String.Format ("http://calicoproject.org/Calico_Jigsaw#{0}", this.GetType ().ToString ());
			Console.WriteLine ("Navigating to {0}", target);
			try
			{
				 System.Diagnostics.Process.Start(target);
			} catch (System.Exception other) {
				  Console.WriteLine(other.Message);
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected virtual void OnToggleBreakpoint(object sender, EventArgs e)
		{
			this.ToggleBreakPoint();
			_cvs.Invalidate();
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected virtual void OnRunBlockStack(object sender, EventArgs e)
		{
			Jigsaw.Canvas js = (Jigsaw.Canvas)_cvs;
			
			js.engine.CompileStack(this, js);
			
			js.RunBlockStack(this);
			js.RaiseJigsawRun();
			_cvs.Invalidate();
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void OnDoubleClick(Diagram.Canvas cvs, Diagram.MouseEventArgs e)
        {	// Handle double click event
			_cvs = cvs;
			if (this.IsFactory == false) this.OnRunBlockStack(null, null);
			
        }

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected virtual void OnDeleteBlock(object sender, EventArgs e)
		{
			// First get the stack top
			CBlock top = this.StackTop;
			
			// Get top level window
			Gtk.Window toplevel = null;
			if (_cvs.Toplevel.IsTopLevel) toplevel = (Gtk.Window)_cvs.Toplevel;
			
			// Ask to delete the block
			Gtk.MessageDialog dlg = new Gtk.MessageDialog(
				toplevel,
				Gtk.DialogFlags.Modal | Gtk.DialogFlags.DestroyWithParent, 
				Gtk.MessageType.Question,
				Gtk.ButtonsType.YesNo,
				"Delete the selected block?");
			dlg.Title = "Delete Block?";
			Gtk.ResponseType rsp = (Gtk.ResponseType)dlg.Run ();
			dlg.Destroy();
			if (rsp == Gtk.ResponseType.No) return;
			
			// Delete and reposition what's left
			(_cvs as Canvas).DeleteBlock(this);
			if (top != this) top.RepositionBlocks(null);
			
			// Redraw
			_cvs.Invalidate();
			_cvs = null;
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected virtual void OnDeleteStack(object sender, EventArgs evt)
		{
			// Get top level window
			Gtk.Window toplevel = null;
			if (_cvs.Toplevel.IsTopLevel) toplevel = (Gtk.Window)_cvs.Toplevel;
			
			// Ask to delete the block
			Gtk.MessageDialog dlg = new Gtk.MessageDialog(
				toplevel,
				Gtk.DialogFlags.Modal | Gtk.DialogFlags.DestroyWithParent, 
				Gtk.MessageType.Question,
				Gtk.ButtonsType.YesNo,
				"Delete the selected block and all blocks below it in its stack?");
			dlg.Title = "Delete Stack?";
			Gtk.ResponseType rsp = (Gtk.ResponseType)dlg.Run ();
			dlg.Destroy();
			if (rsp == Gtk.ResponseType.No) return;
			
			(_cvs as Canvas).DeleteStack(this);
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
			
			// Auto-translate based on mouse position, not block boundaries
			double margin = 10.0;
			if (cvs.mouseExact.Y < ymin + margin) {
				cvs.DoTranslate( 0.0, margin);
			} else if (cvs.mouseExact.X < xmin + margin) {
				cvs.DoTranslate( margin, 0.0);
			} else if (cvs.mouseExact.Y > ymax - margin) {
				cvs.DoTranslate( 0.0, -margin);
			} else if (cvs.mouseExact.X > xmax - margin) {
				cvs.DoTranslate(-margin, 0.0);
			}

//			if (this.Outline.Top < ymin) {
//				cvs.DoTranslate( 0.0, 40.0); //wymin - this.Outline.Top );
//			} else if (this.Outline.Left < xmin) {
//				cvs.DoTranslate( 40.0, 0.0); //wxmin - this.Outline.Left, 0.0 );
//			} else if (this.Outline.Top + this.Outline.Height > ymax) {
//				cvs.DoTranslate( 0.0, -40.0);
//			} else if (this.Outline.Left + this.Outline.Width > xmax) {
//				cvs.DoTranslate(-40.0, 0.0);
//			}
			
			// Set modified flag if not a factory block
			if (this.IsFactory == false) js.Modified = true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void OnDrop(Diagram.Canvas cvs) 
		{	// Implement drop behavior.
			
			// Completely replace behavior of base class.
			// Not sure how to use base implementation because cannot get a reference to 
			// newly created object if this is a factory.
			
			CBlock dropped = null;
			CBlock linked = null;
			
			// Get jigsaw canvas
			Jigsaw.Canvas js = (Jigsaw.Canvas)cvs;
			
			// If not a factory, move the block
			if (!this.IsFactory) {
				
				// Move the block
				this.MatchOutline(cvs);
				
				// If dropped on the palette, delete the block
				if (js.pnlBlock.ContainsPoint( this.Center.X, this.Center.Y, js )) {
					_cvs = js;
					this.OnDeleteStack(null, null);;
					return;
					
				} else {
					// Otherwise, set dropped variable and continue
					dropped = this;
				}
				
			} else {
				// If a factory block is dropped on the palette, do nothing
				if (js.pnlBlock.ContainsPoint( this.Outline.Center.X, this.Outline.Center.Y, js)) {
					return;
				} else {
					// When a factory object is dropped, create a new instance.
					dropped = (CBlock)this.Clone(this.Outline.Left, this.Outline.Top);
					(cvs as Jigsaw.Canvas).AddBlock( dropped ); //cvs.AddShape( dropped );
				}
			}
			
			// Deselect all shapes including factory and select dropped block.
			// Move all dropped blocks to top
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
			
			// Always bring stack of blocks to front
			js.BringStackToFront(dropped);
			
			// If in auto properties dialog mode, automatically show the dialog
//			bool popup = (cvs as Jigsaw.Canvas)._autoProperties;
//			if (this.IsFactory && popup) dropped.OnDoubleClick(cvs, null);
//			if (this.IsFactory && popup) cvs.ShowPropertiesWindow();
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
		
		// - - -A utility method to completely disconnect this block from all others - - -
		public override void Disconnect()
		{
			// Disconnect all edges of current block
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
		internal double _azH = 10.0; 				// Height of activation zone
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
			_azH = 12.0;
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
			double t = _block.Top + this._azY - 0.5*this._azH;
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
