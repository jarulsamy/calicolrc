using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;

// This file contains all things InspectorWindow. 
// This includes the window itself and all property classes.
// The namespace remains Jigsaw.

namespace Jigsaw
{
	public class PropertyWindow : Gtk.Window
	{
		private Gtk.TreeView propTree = null;		// Property List
		private Gtk.ListStore propList = null;		// List of properties for the Property Tree
		private Jigsaw.Canvas cvs = null;
		
		public PropertyWindow(Diagram.Canvas _cvs) : base(Gtk.WindowType.Toplevel)
		{
			this.Title = "Jigsaw Property Window";
			this.Icon = new Gdk.Pixbuf(System.IO.Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory, "plugin.png"));
			this.Resize(300, 200);
			this.KeepAbove = true;
			this.DeleteEvent += DoDeleteEvent;		// Invoked when Inspector is closed
			
			this.cvs = (Jigsaw.Canvas)_cvs;			// Jigsaw canvas and all event handlers
			this.cvs.SelectionChanged += OnJigsawSelectionChanged;
			
			// See http://www.mono-project.com/GtkSharp_TreeView_Tutorial
			
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			// New Tree view to display properties
			propTree = new Gtk.TreeView();
			
			// Create name columns
			Gtk.TreeViewColumn nameCol = new Gtk.TreeViewColumn ();
			nameCol.Title = "Name";
			
			Gtk.TreeViewColumn valCol = new Gtk.TreeViewColumn ();
			valCol.Title = "Value";
			
			// Add to TreeView
			propTree.AppendColumn(nameCol);
			propTree.AppendColumn(valCol);
			
			// Create and insert cell renderer
			Gtk.CellRendererText nameRenderer = new Gtk.CellRendererText();
  			nameCol.PackStart(nameRenderer, true);

			// Create and insert cell renderer
			Gtk.CellRendererText valRenderer = new Gtk.CellRendererText();
  			valCol.PackStart(valRenderer, true);

			// Link it all together
			nameCol.SetCellDataFunc(nameRenderer, new Gtk.TreeCellDataFunc(RenderPropertyName));
			valCol.SetCellDataFunc(valRenderer, new Gtk.TreeCellDataFunc(RenderPropertyValue));
			
			// Make the value column editable
			valRenderer.Editable = true;
			valRenderer.Edited += OnPropValRendererEdited;
			
			// Create a ListStore as the Model
			propList = new Gtk.ListStore(typeof (CProperty));
			propTree.Model = propList;
			
			this.Add(propTree);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// React to a change in selection on the Jigsaw canvas
		void OnJigsawSelectionChanged(Diagram.Canvas cvs, Diagram.ShapeListEventArgs e)
		{
			// Before finishing, update PropertyWindow
			// If at least one block is selected, show properties of selected block.
			this.ClearProperties();
			List<Diagram.CShape> shps = cvs.SelectedShapes();
			if (shps.Count > 0){
				if (shps[0] is CBlock) {
					CBlock b = (CBlock)shps[0];
					if (b.IsFactory == false) {
						//b.PopulatePropertiesWindow(this);
						foreach (CProperty p in b.Properties)
							propList.AppendValues(p);
					}
				}
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		private void RenderPropertyName(Gtk.TreeViewColumn column, Gtk.CellRenderer cell, Gtk.TreeModel model, Gtk.TreeIter iter)
		{
			CProperty prop = (CProperty)model.GetValue(iter, 0);
			Gtk.CellRendererText cellText = (Gtk.CellRendererText)cell;
			cellText.Text = prop.Name;
			cellText.Background = "lightgray";
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		private void RenderPropertyValue(Gtk.TreeViewColumn column, Gtk.CellRenderer cell, Gtk.TreeModel model, Gtk.TreeIter iter)
		{
			CProperty prop = (CProperty)model.GetValue(iter, 0);
			Gtk.CellRendererText cellText = (Gtk.CellRendererText)cell;
			cellText.Text = prop.Text;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// When a value is edited, copy data back to CProperty
		void OnPropValRendererEdited(object o, Gtk.EditedArgs args)
		{
			Gtk.TreeIter iter;
			propList.GetIter(out iter, new Gtk.TreePath(args.Path));
			CProperty prop = (CProperty)propList.GetValue(iter, 0);
			prop.Text = args.NewText;
			
			// Redraw
			cvs.Invalidate();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Remove all properties from the InspectorWindow property list
		public void ClearProperties() {
			propList.Clear();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected void DoDeleteEvent (object obj, Gtk.DeleteEventArgs args)
		{
			this.Hide();		// If HideAll() is used here instead, when ShowAll is called, Window forgets its position
			args.RetVal = true;
		}
	}
	
	/// <summary>
	/// Top level floating window that displays the currently selected CBlock properties and other items
	/// </summary>
	public class InspectorWindow : Gtk.Window 
	{

//		private Gtk.TreeView localsTree = null;		// Locals List
//		private Gtk.ListStore localsList = null;	// List of items in the local scope for the Locals Tree
		private Gtk.TextView localsView = null;		// Locals window
		private Gtk.TextView outputView = null;		// Output window
		
		private Jigsaw.Canvas cvs = null;
		
		/// <summary>
		/// Inspector Window Constructor 
		/// </summary>
		public InspectorWindow(Diagram.Canvas _cvs) : base(Gtk.WindowType.Toplevel)
		{
			this.Title = "Jigsaw Inspector";
			this.Icon = new Gdk.Pixbuf(System.IO.Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory, "plugin.png"));
			this.Resize(300, 200);
			this.KeepAbove = true;
			this.DeleteEvent += DoDeleteEvent;		// Invoked when Inspector is closed
			
			this.cvs = (Jigsaw.Canvas)_cvs;			// Jigsaw canvas and all event handlers
			
//			// See http://www.mono-project.com/GtkSharp_TreeView_Tutorial
//			
//			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//			// New Tree view to display properties
//			propTree = new Gtk.TreeView();
//			
//			// Create name columns
//			Gtk.TreeViewColumn nameCol = new Gtk.TreeViewColumn ();
//			nameCol.Title = "Name";
//			
//			Gtk.TreeViewColumn valCol = new Gtk.TreeViewColumn ();
//			valCol.Title = "Value";
//			
//			// Add to TreeView
//			propTree.AppendColumn(nameCol);
//			propTree.AppendColumn(valCol);
//			
//			// Create and insert cell renderer
//			Gtk.CellRendererText nameRenderer = new Gtk.CellRendererText();
//  			nameCol.PackStart(nameRenderer, true);
//
//			// Create and insert cell renderer
//			Gtk.CellRendererText valRenderer = new Gtk.CellRendererText();
//  			valCol.PackStart(valRenderer, true);
//
//			// Link it all together
//			nameCol.SetCellDataFunc(nameRenderer, new Gtk.TreeCellDataFunc(RenderPropertyName));
//			valCol.SetCellDataFunc(valRenderer, new Gtk.TreeCellDataFunc(RenderPropertyValue));
//			
//			// Make the value column editable
//			valRenderer.Editable = true;
//			valRenderer.Edited += OnPropValRendererEdited;
//			
//			// Create a ListStore as the Model
//			propList = new Gtk.ListStore(typeof (CProperty));
//			propTree.Model = propList;
			
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			// Locals Tree
//			localsTree = new Gtk.TreeView();
//			
//			// Create name columns
//			Gtk.TreeViewColumn locNameCol = new Gtk.TreeViewColumn ();
//			locNameCol.Title = "Name";
//			
//			Gtk.TreeViewColumn locValCol = new Gtk.TreeViewColumn ();
//			locValCol.Title = "Value";
//			
//			// Add to TreeView
//			localsTree.AppendColumn(locNameCol);
//			localsTree.AppendColumn(locValCol);
//			
//			// Create and insert cell renderer
//			Gtk.CellRendererText locNameRenderer = new Gtk.CellRendererText();
//  			locNameCol.PackStart(locNameRenderer, true);
//
//			// Create and insert cell renderer
//			Gtk.CellRendererText locValRenderer = new Gtk.CellRendererText();
//  			locValCol.PackStart(locValRenderer, true);
//
//			// Link it all together
//			locNameCol.SetCellDataFunc(locNameRenderer, new Gtk.TreeCellDataFunc(RenderPropertyName));
//			locValCol.SetCellDataFunc(locValRenderer, new Gtk.TreeCellDataFunc(RenderPropertyValue));
//			
//			// Make the value column editable
//			locValRenderer.Editable = true;
//			locValRenderer.Edited += OnLocalsValRendererEdited;
//			
//			// Create a ListStore as the Model
//			localsList = new Gtk.ListStore(typeof(CProperty));
//			localsTree.Model = localsList;
			
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			// locals display window
			localsView = new Gtk.TextView();
			Gtk.ScrolledWindow lsw = new Gtk.ScrolledWindow();
			lsw.CanFocus = true;
			lsw.VscrollbarPolicy = Gtk.PolicyType.Always;
			lsw.Add(localsView);
			
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			// Output window
			outputView = new Gtk.TextView();
			Gtk.ScrolledWindow osw = new Gtk.ScrolledWindow();
			osw.CanFocus = true;
			osw.VscrollbarPolicy = Gtk.PolicyType.Always;
			osw.Add(outputView);
			
			// Put it all in a Notebook
			Gtk.Notebook nb = new Gtk.Notebook();
			//nb.AppendPage(propTree, new Gtk.Label("Properties"));
			nb.AppendPage(lsw, new Gtk.Label("Locals"));
			//nb.AppendPage(localsTree, new Gtk.Label("Locals"));
			nb.AppendPage(new Gtk.Label("REPL Window"), new Gtk.Label("Debug"));
			nb.AppendPage(osw, new Gtk.Label("Output"));
			
			this.Add(nb);
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void WriteLine(String msg) {
			// Add text to the buffer
			outputView.Buffer.Text += msg + "\n";

			// Always scroll to the cursor position
			Gtk.TextIter ti = outputView.Buffer.GetIterAtLine(outputView.Buffer.LineCount-1); 
			Gtk.TextMark tm = outputView.Buffer.CreateMark("eot", ti,false); 
			outputView.ScrollToMark(tm, 0, false, 0, 0); 
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Clear contents of output window
		public void ClearOutput() {
			outputView.Buffer.Clear();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Render all locals to window
		public void DisplayLocals(Engine e) 
		{
			Dictionary<string,object> locals = null;
			StringBuilder sb = new StringBuilder();
			
			foreach (CallStack cs in e.CallStacks) {
				locals = cs.Locals;

				//Console.WriteLine("------");
				sb.AppendLine("------");
				foreach (string k in locals.Keys) {
					//Console.WriteLine("{0} = {1}", k, locals[k]);
					sb.AppendFormat("{0} = {1}\n", k, locals[k]);
				}
			}
			
			localsView.Buffer.Text = sb.ToString();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Clear contents of locals window
		public void ClearLocals() {
			localsView.Buffer.Clear();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Render the given locals scope in its window
		public void RenderLocals() {
			
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// When a locals value is edited, copy data back to CProperty
//		void OnLocalsValRendererEdited(object o, Gtk.EditedArgs args)
//		{
//			Gtk.TreeIter iter;
//			localsList.GetIter(out iter, new Gtk.TreePath(args.Path));
//			CProperty prop = (CProperty)localsList.GetValue(iter, 0);
//			
//			// TODO: Must evaluate the exprssion here before resetting?
//			
//			prop.Text = args.NewText;
//			
//			// Redraw
//			//cvs.Invalidate();
//		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Remove all locals properties from the InspectorWindow locals list
//		public void ClearLocals() {
//			localsList.Clear();
//		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected void DoDeleteEvent (object obj, Gtk.DeleteEventArgs args)
		{
			this.Hide();		// If HideAll() is used here instead, when ShowAll is called, Window forgets its position
			args.RetVal = true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	}
	
	// This is a class that holds data for one property in PropertyWindow
	public class CProperty
	{
		protected string _Name;
		protected string _Text;
		
		// Event that is raised whenever a property value is changed. 
		public event EventHandler PropertyChanged;
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CProperty(CBlock shp) : this ("", "") {
		}
		
		public CProperty(string name) : this (name, "") {
		}
		
		public CProperty(string name, string txt) {
			Name = name;
			Text = txt;
		}

		public CProperty() : this ("", "") {
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public virtual void RaisePropertyChanged()
        {
            if (PropertyChanged != null)
            {
				EventArgs e = new EventArgs();
				PropertyChanged(this, e);
			}
        }
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Setting/getting Name value
		public string Name {
			get { return _Name; }
			set { 
				_Name = value;
				this.RaisePropertyChanged();
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Setting/getting value text
		// This should be overridden and the text parsed and validated if intended to be a different type
		public virtual string Text {
			get { return _Text; }
			set {
				_Text = value;
				this.RaisePropertyChanged();
			}
		}
	}
	
	/// <summary>
	/// Subclass of CProperty with a simple string value and string value holding an expression 
	/// </summary>
	public class CExpressionProperty : CProperty
	{
		private NCalc.Expression _Expr = null;
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CExpressionProperty(string name, string txt) : base(name, txt) 
		{ }
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Setting/getting Text parses and updates internal expression
		public override string Text {
			get { return _Text; }
			set {
				_Expr = new NCalc.Expression(value);
				if (_Expr.HasErrors()) {
					_Text = "Err: " + value;
				} else {
					_Text = _Expr.ParsedExpression.ToString();
					this.RaisePropertyChanged();
				}
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public NCalc.Expression Expr {
			get { return _Expr; }
		}
	}
	
	/// <summary>
	/// Property that holds a variable name (identifier) 
	/// </summary>
	public class CVarNameProperty : CProperty
	{	
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CVarNameProperty(string name, string txt) : base(name, txt) 
		{ }
	}

	/// <summary>
	/// Property that holds a string 
	/// </summary>
	public class CStringProperty : CProperty
	{
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CStringProperty(string name, string txt) : base(name, txt) 
		{ }
	}
	
	/// <summary>
	/// Property that can be read only. Changing value does nothing to block.
	/// </summary>
	public class CReadOnlyProperty : CProperty
	{
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CReadOnlyProperty(string name) : this (name, "") {
		}
//		
		public CReadOnlyProperty(string name, string txt) : base(name, txt) {
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Getting value text. Cannot set.
		public override string Text {
			get { return _Text; }
		}
	}
	
	/// <summary>
	/// Property that holds an integer 
	/// </summary>
	public class CIntegerProperty : CProperty
	{	
		private int _val = 0;
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CIntegerProperty(string name, int val) : base(name, val.ToString()) 
		{
			this.Value = val;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Setting/getting value
		public int Value {
			get { return _val; }
			set { 
				_val = value;
				_Text = _val.ToString();
				this.RaisePropertyChanged();
			}
		}
	}
	
	/// <summary>
	/// Property that holds a path to a file 
	/// </summary>
	public class CPathProperty : CProperty
	{
//		string file = @"\\MyServer\MyFolderA\MyFolderB\MyFile.jpg";
//        FileInfo fi = new FileInfo(file);
//        Console.WriteLine(fi.Name);                  // Prints MyFile.jpg
//        Console.WriteLine(fi.Directory.Name);        // Prints MyFolderB
//        Console.WriteLine(fi.Directory.Parent.Name); // Prints MyFolderA
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CPathProperty(string name, string txt) : base(name, txt) 
		{ }
	}
	
}

