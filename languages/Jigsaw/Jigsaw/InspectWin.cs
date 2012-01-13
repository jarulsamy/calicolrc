using System;
using System.Text;
using System.Xml;
using System.Collections;
using System.Collections.Generic;

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
			this.ParentWindow = _cvs.ParentWindow;
			this.DestroyWithParent = true;
			
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
		
		// - - - React to a change in selection on the Jigsaw canvas - - - - - - -
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
							if (p.Visible) propList.AppendValues(p);
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
		void OnPropValRendererEdited(object o, Gtk.EditedArgs args)
		{	// When a value is edited, copy data back to CProperty
			Gtk.TreeIter iter;
			propList.GetIter(out iter, new Gtk.TreePath(args.Path));
			CProperty prop = (CProperty)propList.GetValue(iter, 0);
			prop.Text = args.NewText;
			
			// Redraw
			cvs.Invalidate();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void ClearProperties()
		{	// Remove all properties from the InspectorWindow property list
			propList.Clear();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected void DoDeleteEvent (object obj, Gtk.DeleteEventArgs args)
		{
			this.Hide();		// If HideAll() is used here instead, when ShowAll is called, Window forgets its position
			args.RetVal = true;
		}
	}
	
	// ----------------------------------------------------------------------
	public class InspectorWindow : Gtk.Window 
	{

//		private Gtk.TreeView localsTree = null;		// Locals List
//		private Gtk.ListStore localsList = null;	// List of items in the local scope for the Locals Tree
		private Gtk.TextView localsView = null;		// Locals window
		private Gtk.TextView outputView = null;		// Output window
		
		private Jigsaw.Canvas cvs = null;
		
		// - - - Inspector Window Constructor 
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
		public void ClearOutput()
		{	// Clear contents of output window
			outputView.Buffer.Clear();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		public void DisplayLocals(Engine e) 
//		{	// Render all locals to window
//			
//			//Microsoft.Scripting.Hosting.ScriptScope scope = e.Scope;
//			StringBuilder sb = new StringBuilder();
//			
//			foreach (CallStack cs in e.CallStacks) {
//
//				sb.AppendLine("------");
//				foreach (string k in scope.GetVariableNames()) {
//					//Console.WriteLine("{0} = {1}", k, locals[k]);
//					sb.AppendFormat("{0} = {1}\n", k, scope.GetVariable(k));
//				}
//			}
//			
//			localsView.Buffer.Text = sb.ToString();
//		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void ClearLocals()
		{	// Clear contents of locals window
			localsView.Buffer.Clear();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void RenderLocals()
		{	// Render the given locals scope in its window
			
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
	
	// -----------------------------------------------------------------------
	public class CProperty
	{	// This is a class that holds data for one property in PropertyWindow
		
		protected string _Name;
		protected string _Text;
		protected bool _Visible = true;
		
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
		public virtual string Name 
		{	// Setting/getting Name value
			get { return _Name; }
			set { 
				_Name = value;
				this.RaisePropertyChanged();
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public virtual string Text 
		{ 	// Setting/getting value text
			// This should be overridden and the text parsed and validated if intended to be a different type
			get { return _Text; }
			set {
				_Text = value;
				this.RaisePropertyChanged();
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public virtual bool Visible 
		{ 	// Setting/getting visible property
			get { return _Visible; }
			set {
				_Visible = value;
				this.RaisePropertyChanged();
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public virtual void ToXml(XmlWriter w) {
	        w.WriteStartElement("property");
			w.WriteAttributeString("name", _Name);
			w.WriteAttributeString("value", _Text);
			w.WriteEndElement();	
		}
	}
	
	// -----------------------------------------------------------------------
	public class CExpressionProperty : CProperty
	{	// Subclass of CProperty with a simple string value and string value holding an expression 
		
		//private Expression.Expression _Expr = null;
		private Microsoft.Scripting.Hosting.ScriptSource _Source = null;
		private Microsoft.Scripting.Hosting.CompiledCode _Compiled = null;
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CExpressionProperty(string name, string txt) : base(name, txt) 
		{ }

//		public override string Text 
//		{	// Setting/getting Text parses and updates internal expression
//			get { return _Text; }
//			set {
//				_Expr = Expression.Engine.makeExpression(value);
//				if (_Expr.HasErrors()) {
//					_Text = "Err: " + value;
//				} else {
//					_Text = _Expr.ParsedExpression();
//					this.RaisePropertyChanged();
//				}
//			}
//		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void Compile(Microsoft.Scripting.Hosting.ScriptEngine engine) 
		{
			_Source = engine.CreateScriptSourceFromString(_Text, Microsoft.Scripting.SourceCodeKind.Expression);
			_Compiled = _Source.Compile();
			Text = _Source.GetCode();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public object Evaluate(Microsoft.Scripting.Hosting.ScriptScope scope) {
			return _Compiled.Execute(scope);
			//return _Expr.Execute(scope);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public Microsoft.Scripting.Hosting.ScriptSource Expr {
			get { return _Source; }
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void ToXml(XmlWriter w) {
	        w.WriteStartElement("property");
			w.WriteAttributeString("name", _Name);
			if (_Source != null)
				w.WriteAttributeString("value", _Source.GetCode());
			else
				w.WriteAttributeString("value", _Text);
			w.WriteEndElement();	
		}
	}
	
	// -----------------------------------------------------------------------
	public class CStatementProperty : CProperty
	{	// Subclass of CProperty with a simple string value and string value holding an arbitrary statement 
		
		private Microsoft.Scripting.Hosting.ScriptSource _Source = null;
		private Microsoft.Scripting.Hosting.CompiledCode _Compiled = null;
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CStatementProperty(string name, string txt) : base(name, txt) 
		{ }
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void Compile(Microsoft.Scripting.Hosting.ScriptEngine engine) 
		{
			_Source = engine.CreateScriptSourceFromString(_Text, Microsoft.Scripting.SourceCodeKind.Statements);
			_Compiled = _Source.Compile();
			Text = _Source.GetCode();
		}
		
		// - - - Replaces statement with given string and compile - - - - - - -
		public void Compile(Microsoft.Scripting.Hosting.ScriptEngine engine, string statement) 
		{
			_Source = engine.CreateScriptSourceFromString(statement, Microsoft.Scripting.SourceCodeKind.Statements);
			_Compiled = _Source.Compile();
			Text = _Source.GetCode();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void Evaluate(Microsoft.Scripting.Hosting.ScriptScope scope)
		{	// Its a statement ... nothing to return
			_Compiled.Execute(scope);
			//_Source.Execute(scope);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public Microsoft.Scripting.Hosting.ScriptSource Statement {
			get { return _Source; }
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void ToXml(XmlWriter w) {
	        w.WriteStartElement("property");
			w.WriteAttributeString("name", _Name);
			if (_Source != null)
				w.WriteAttributeString("value", _Source.GetCode());
			else
				w.WriteAttributeString("value", "");
			w.WriteEndElement();
		}
	}
	
	// -----------------------------------------------------------------------
	public class CVarNameProperty : CProperty
	{	// Property that holds a variable name (identifier) 
		public CVarNameProperty(string name, string txt) : base(name, txt) 
		{
			this.Name = name.Trim ();
			this.Text = txt.Trim ();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override string Name 
		{	// Setting/getting Name value
			get { return _Name; }
			set { 
				_Name = value.Trim ();
				this.RaisePropertyChanged();
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override string Text 
		{ 	// Setting/getting value text
			// This should be overridden and the text parsed and validated if intended to be a different type
			get { return _Text; }
			set {
				_Text = value.Trim ();
				this.RaisePropertyChanged();
			}
		}
	}
	
	// -----------------------------------------------------------------------
	public class CStringProperty : CProperty
	{	// Property that holds a string 
		public CStringProperty(string name, string txt) : base(name, txt) 
		{ }
	}
	
	// -----------------------------------------------------------------------
	public class CReadOnlyProperty : CProperty
	{	// Property that can be read only. Changing value does nothing to block.
		public CReadOnlyProperty(string name) : this (name, "") {
		}

		public CReadOnlyProperty(string name, string txt) : base(name, txt) {
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override string Text
		{	// Getting value text. Cannot set.
			get { return _Text; }
		}
	}
	
	// -----------------------------------------------------------------------
	public class CIntegerProperty : CProperty
	{	// Property that holds an integer 
		private int _val = 0;
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CIntegerProperty(string name, int val) : base(name, val.ToString()) 
		{
			this.Value = val;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override string Text 
		{	// Setting/getting Text parses and updates integer
			get { return _val.ToString(); }
			set {
				try {
					_val = int.Parse(value);
					this.RaisePropertyChanged();
				} catch (Exception ex) {
					// TODO: Add a message window to the Properties Dialog and show this message there
					Console.WriteLine("Err: " + ex.Message);
				}
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public int Value 
		{	// Setting/getting value
			get { return _val; }
			set { 
				_val = value;
				_Text = _val.ToString();
				this.RaisePropertyChanged();
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void ToXml(XmlWriter w) {
	        w.WriteStartElement("property");
			w.WriteAttributeString("name", _Name);
			w.WriteAttributeString("value", _val.ToString());
			w.WriteEndElement();	
		}
	}
	
	// -----------------------------------------------------------------------
	public class CPathProperty : CProperty
	{	// Property that holds a path to a file 
		
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