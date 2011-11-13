
// This file has been generated by the GUI designer. Do not modify.
namespace Calico
{
	public partial class MainWindow
	{
		private global::Gtk.UIManager UIManager;
		private global::Gtk.Action FileAction;
		private global::Gtk.Action NewAction;
		private global::Gtk.Action newAction;
		private global::Gtk.Action openAction;
		private global::Gtk.Action yesAction;
		private global::Gtk.Action noAction;
		private global::Gtk.Action goForwardAction;
		private global::Gtk.Action EditAction;
		private global::Gtk.Action openAction1;
		private global::Gtk.Action saveAction;
		private global::Gtk.Action saveAsAction;
		private global::Gtk.Action quitAction;
		private global::Gtk.Action copyAction;
		private global::Gtk.Action pasteAction;
		private global::Gtk.Action cutAction;
		private global::Gtk.Action undoAction;
		private global::Gtk.Action redoAction;
		private global::Gtk.Action selectAllAction;
		private global::Gtk.Action indentAction;
		private global::Gtk.Action unindentAction;
		private global::Gtk.Action CommentRegionAction;
		private global::Gtk.Action UncommentRegionAction;
		private global::Gtk.Action HelpAction;
		private global::Gtk.Action helpAction;
		private global::Gtk.Action aboutAction;
		private global::Gtk.Action ScriptAction;
		private global::Gtk.Action LanguageAction;
		private global::Gtk.Action yesAction1;
		private global::Gtk.Action noAction11;
		private global::Gtk.Action ViewAction;
		private global::Gtk.Action zoomInAction;
		private global::Gtk.Action zoomOutAction;
		private global::Gtk.Action selectFontAction;
		private global::Gtk.Action ShellAction;
		private global::Gtk.Action SelectAction;
		private global::Gtk.RadioAction Option1Action;
		private global::Gtk.RadioAction Option2Action;
		private global::Gtk.Action ExamplesAction;
		private global::Gtk.Action RecentScriptsAction;
		private global::Gtk.Action ShowAction;
		private global::Gtk.ToggleAction EnvironmentTabAction;
		private global::Gtk.ToggleAction LocalsTabAction;
		private global::Gtk.Action ExportAction;
		private global::Gtk.Action ExportAction1;
		private global::Gtk.VBox vbox1;
		private global::Gtk.MenuBar menubar2;
		private global::Gtk.Toolbar toolbar1;
		private global::Gtk.VPaned vpaned2;
		private global::Gtk.Notebook notebook_docs;
		private global::Gtk.HBox hbox1;
		private global::Gtk.VButtonBox vbuttonbox1;
		private global::Gtk.Button button4;
		private global::Gtk.Button button3;
		private global::Gtk.Button button2;
		private global::Gtk.Button button100;
		private global::Gtk.Image image69;
		private global::Gtk.Label label2;
		private global::Gtk.HBox hbox2;
		private global::Gtk.VBox vbox2;
		private global::Gtk.VBox vbox3;
		private global::Gtk.Label prompt;
		private global::Gtk.HBox hbox3;
		private global::Gtk.Button history_up;
		private global::Gtk.Button history_down;
		private global::Gtk.ScrolledWindow scrolledwindow1;
		private global::Gtk.Label label1;
		private global::Gtk.Notebook notebook_tools;
		private global::Gtk.ScrolledWindow GtkScrolledWindow;
		private global::Gtk.TextView textview1;
		private global::Gtk.Label label3;
		private global::Gtk.ScrolledWindow GtkScrolledWindow1;
		private global::Gtk.TreeView treeview1;
		private global::Gtk.Label label4;
		private global::Gtk.ScrolledWindow scrolledwindow2;
		private global::Gtk.TreeView treeview2;
		private global::Gtk.Label label5;
		private global::Gtk.Statusbar statusbar1;
		private global::Gtk.Label label6;
		private global::Gtk.Label status_langauge;
        
		protected virtual void Build ()
		{
			global::Stetic.Gui.Initialize (this);
			// Widget Calico.MainWindow
			this.UIManager = new global::Gtk.UIManager ();
			global::Gtk.ActionGroup w1 = new global::Gtk.ActionGroup ("Default");
			this.FileAction = new global::Gtk.Action ("FileAction", global::Mono.Unix.Catalog.GetString ("File"), null, null);
			this.FileAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("File");
			w1.Add (this.FileAction, null);
			this.NewAction = new global::Gtk.Action ("NewAction", global::Mono.Unix.Catalog.GetString ("New"), null, null);
			this.NewAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("Open");
			w1.Add (this.NewAction, null);
			this.newAction = new global::Gtk.Action ("newAction", null, null, "gtk-new");
			w1.Add (this.newAction, null);
			this.openAction = new global::Gtk.Action ("openAction", null, null, "gtk-open");
			w1.Add (this.openAction, null);
			this.yesAction = new global::Gtk.Action ("yesAction", null, null, "gtk-yes");
			w1.Add (this.yesAction, null);
			this.noAction = new global::Gtk.Action ("noAction", global::Mono.Unix.Catalog.GetString ("Stop Script"), null, "gtk-no");
			this.noAction.Sensitive = false;
			this.noAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("Stop Script");
			w1.Add (this.noAction, "<Mod2>Escape");
			this.goForwardAction = new global::Gtk.Action ("goForwardAction", null, null, "gtk-go-forward");
			this.goForwardAction.Sensitive = false;
			w1.Add (this.goForwardAction, null);
			this.EditAction = new global::Gtk.Action ("EditAction", global::Mono.Unix.Catalog.GetString ("Edit"), null, null);
			this.EditAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("Edit");
			w1.Add (this.EditAction, null);
			this.openAction1 = new global::Gtk.Action ("openAction1", global::Mono.Unix.Catalog.GetString ("_Open"), null, "gtk-open");
			this.openAction1.ShortLabel = global::Mono.Unix.Catalog.GetString ("_Open");
			w1.Add (this.openAction1, null);
			this.saveAction = new global::Gtk.Action ("saveAction", global::Mono.Unix.Catalog.GetString ("_Save"), null, "gtk-save");
			this.saveAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("_Save");
			w1.Add (this.saveAction, null);
			this.saveAsAction = new global::Gtk.Action ("saveAsAction", global::Mono.Unix.Catalog.GetString ("Save _As"), null, "gtk-save-as");
			this.saveAsAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("Save _As");
			w1.Add (this.saveAsAction, "<Alt><Mod2>a");
			this.quitAction = new global::Gtk.Action ("quitAction", global::Mono.Unix.Catalog.GetString ("_Quit"), null, "gtk-quit");
			this.quitAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("_Quit");
			w1.Add (this.quitAction, null);
			this.copyAction = new global::Gtk.Action ("copyAction", global::Mono.Unix.Catalog.GetString ("_Copy"), null, "gtk-copy");
			this.copyAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("_Copy");
			w1.Add (this.copyAction, null);
			this.pasteAction = new global::Gtk.Action ("pasteAction", global::Mono.Unix.Catalog.GetString ("_Paste"), null, "gtk-paste");
			this.pasteAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("_Paste");
			w1.Add (this.pasteAction, null);
			this.cutAction = new global::Gtk.Action ("cutAction", global::Mono.Unix.Catalog.GetString ("Cu_t"), null, "gtk-cut");
			this.cutAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("Cu_t");
			w1.Add (this.cutAction, null);
			this.undoAction = new global::Gtk.Action ("undoAction", global::Mono.Unix.Catalog.GetString ("_Undo"), null, "gtk-undo");
			this.undoAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("_Undo");
			w1.Add (this.undoAction, "<Control><Mod2>z");
			this.redoAction = new global::Gtk.Action ("redoAction", global::Mono.Unix.Catalog.GetString ("_Redo"), null, "gtk-redo");
			this.redoAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("_Redo");
			w1.Add (this.redoAction, "<Control><Shift>z");
			this.selectAllAction = new global::Gtk.Action ("selectAllAction", global::Mono.Unix.Catalog.GetString ("Select _All"), null, "gtk-select-all");
			this.selectAllAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("Select _All");
			w1.Add (this.selectAllAction, "<Control><Mod2>a");
			this.indentAction = new global::Gtk.Action ("indentAction", global::Mono.Unix.Catalog.GetString ("Indent Region"), null, "gtk-indent");
			this.indentAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("Indent Region");
			w1.Add (this.indentAction, "<Control><Mod2>bracketleft");
			this.unindentAction = new global::Gtk.Action ("unindentAction", global::Mono.Unix.Catalog.GetString ("Unindent Region"), null, "gtk-unindent");
			this.unindentAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("Unindent Region");
			w1.Add (this.unindentAction, "<Control><Mod2>bracketright");
			this.CommentRegionAction = new global::Gtk.Action ("CommentRegionAction", global::Mono.Unix.Catalog.GetString ("Comment Region"), null, null);
			this.CommentRegionAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("Comment Region");
			w1.Add (this.CommentRegionAction, "<Control><Mod2>braceleft");
			this.UncommentRegionAction = new global::Gtk.Action ("UncommentRegionAction", global::Mono.Unix.Catalog.GetString ("Uncomment Region"), null, null);
			this.UncommentRegionAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("Uncomment Region");
			w1.Add (this.UncommentRegionAction, "<Control><Mod2>braceright");
			this.HelpAction = new global::Gtk.Action ("HelpAction", global::Mono.Unix.Catalog.GetString ("Help"), null, null);
			this.HelpAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("Help");
			w1.Add (this.HelpAction, null);
			this.helpAction = new global::Gtk.Action ("helpAction", null, null, "gtk-help");
			w1.Add (this.helpAction, null);
			this.aboutAction = new global::Gtk.Action ("aboutAction", global::Mono.Unix.Catalog.GetString ("_About"), null, "gtk-about");
			this.aboutAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("_About");
			w1.Add (this.aboutAction, "<Mod2>F10");
			this.ScriptAction = new global::Gtk.Action ("ScriptAction", global::Mono.Unix.Catalog.GetString ("Script"), null, null);
			this.ScriptAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("Shell");
			w1.Add (this.ScriptAction, null);
			this.LanguageAction = new global::Gtk.Action ("LanguageAction", global::Mono.Unix.Catalog.GetString ("Language"), null, null);
			this.LanguageAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("Language");
			w1.Add (this.LanguageAction, null);
			this.yesAction1 = new global::Gtk.Action ("yesAction1", global::Mono.Unix.Catalog.GetString ("Execute Script"), null, "gtk-yes");
			this.yesAction1.Sensitive = false;
			this.yesAction1.ShortLabel = global::Mono.Unix.Catalog.GetString ("Execute Script");
			w1.Add (this.yesAction1, "<Mod2>F5");
			this.noAction11 = new global::Gtk.Action ("noAction11", global::Mono.Unix.Catalog.GetString ("Stop Script"), null, "gtk-no");
			this.noAction11.Sensitive = false;
			this.noAction11.ShortLabel = global::Mono.Unix.Catalog.GetString ("Stop Script");
			w1.Add (this.noAction11, "<Mod2>Escape");
			this.ViewAction = new global::Gtk.Action ("ViewAction", global::Mono.Unix.Catalog.GetString ("View"), null, null);
			this.ViewAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("Options");
			w1.Add (this.ViewAction, null);
			this.zoomInAction = new global::Gtk.Action ("zoomInAction", global::Mono.Unix.Catalog.GetString ("Zoom _In"), null, "gtk-zoom-in");
			this.zoomInAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("Zoom _In");
			w1.Add (this.zoomInAction, "<Control><Mod2>plus");
			this.zoomOutAction = new global::Gtk.Action ("zoomOutAction", global::Mono.Unix.Catalog.GetString ("Zoom _Out"), null, "gtk-zoom-out");
			this.zoomOutAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("Zoom _Out");
			w1.Add (this.zoomOutAction, "<Control><Mod2>minus");
			this.selectFontAction = new global::Gtk.Action ("selectFontAction", global::Mono.Unix.Catalog.GetString ("Change _Font..."), null, "gtk-select-font");
			this.selectFontAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("Change _Font...");
			w1.Add (this.selectFontAction, null);
			this.ShellAction = new global::Gtk.Action ("ShellAction", global::Mono.Unix.Catalog.GetString ("Shell"), null, null);
			this.ShellAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("Shell");
			w1.Add (this.ShellAction, "<Mod2>F7");
			this.SelectAction = new global::Gtk.Action ("SelectAction", global::Mono.Unix.Catalog.GetString ("Select"), null, null);
			this.SelectAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("Select");
			w1.Add (this.SelectAction, null);
			this.Option1Action = new global::Gtk.RadioAction ("Option1Action", global::Mono.Unix.Catalog.GetString ("Option 1"), null, null, 0);
			this.Option1Action.Group = new global::GLib.SList (global::System.IntPtr.Zero);
			this.Option1Action.ShortLabel = global::Mono.Unix.Catalog.GetString ("Option 1");
			w1.Add (this.Option1Action, "<Control>1");
			this.Option2Action = new global::Gtk.RadioAction ("Option2Action", global::Mono.Unix.Catalog.GetString ("Option 2"), null, null, 0);
			this.Option2Action.Group = this.Option1Action.Group;
			this.Option2Action.ShortLabel = global::Mono.Unix.Catalog.GetString ("Option 2");
			w1.Add (this.Option2Action, "<Control>2");
			this.ExamplesAction = new global::Gtk.Action ("ExamplesAction", global::Mono.Unix.Catalog.GetString ("Examples"), null, null);
			this.ExamplesAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("Examples");
			w1.Add (this.ExamplesAction, null);
			this.RecentScriptsAction = new global::Gtk.Action ("RecentScriptsAction", global::Mono.Unix.Catalog.GetString ("Recent Scripts"), null, null);
			this.RecentScriptsAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("Recent Scripts");
			w1.Add (this.RecentScriptsAction, null);
			this.ShowAction = new global::Gtk.Action ("ShowAction", global::Mono.Unix.Catalog.GetString ("Show"), null, null);
			this.ShowAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("Show");
			w1.Add (this.ShowAction, null);
			this.EnvironmentTabAction = new global::Gtk.ToggleAction ("EnvironmentTabAction", global::Mono.Unix.Catalog.GetString ("Environment Tab"), null, null);
			this.EnvironmentTabAction.Active = true;
			this.EnvironmentTabAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("Environment Tab");
			w1.Add (this.EnvironmentTabAction, "<Control><Mod2>e");
			this.LocalsTabAction = new global::Gtk.ToggleAction ("LocalsTabAction", global::Mono.Unix.Catalog.GetString ("Locals Tab"), null, null);
			this.LocalsTabAction.Active = true;
			this.LocalsTabAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("Locals Tab");
			w1.Add (this.LocalsTabAction, null);
			this.ExportAction = new global::Gtk.Action ("ExportAction", global::Mono.Unix.Catalog.GetString ("Export..."), null, null);
			this.ExportAction.ShortLabel = global::Mono.Unix.Catalog.GetString ("Export...");
			w1.Add (this.ExportAction, null);
			this.ExportAction1 = new global::Gtk.Action ("ExportAction1", global::Mono.Unix.Catalog.GetString ("Export..."), null, null);
			this.ExportAction1.ShortLabel = global::Mono.Unix.Catalog.GetString ("Export...");
			w1.Add (this.ExportAction1, null);
			this.UIManager.InsertActionGroup (w1, 0);
			this.AddAccelGroup (this.UIManager.AccelGroup);
			this.Name = "Calico.MainWindow";
			this.Title = global::Mono.Unix.Catalog.GetString ("Calico");
			this.WindowPosition = ((global::Gtk.WindowPosition)(4));
			// Container child Calico.MainWindow.Gtk.Container+ContainerChild
			this.vbox1 = new global::Gtk.VBox ();
			this.vbox1.Name = "vbox1";
			this.vbox1.BorderWidth = ((uint)(5));
			// Container child vbox1.Gtk.Box+BoxChild
			this.UIManager.AddUiFromString ("<ui><menubar name='menubar2'><menu name='FileAction' action='FileAction'><menuitem name='openAction1' action='openAction1'/><menu name='NewAction' action='NewAction'/><separator/><menu name='RecentScriptsAction' action='RecentScriptsAction'/><menu name='ExamplesAction' action='ExamplesAction'/><separator/><menuitem name='ExportAction1' action='ExportAction1'/><separator/><menuitem name='saveAction' action='saveAction'/><menuitem name='saveAsAction' action='saveAsAction'/><separator/><menuitem name='quitAction' action='quitAction'/></menu><menu name='EditAction' action='EditAction'><menuitem name='copyAction' action='copyAction'/><menuitem name='pasteAction' action='pasteAction'/><menuitem name='cutAction' action='cutAction'/><separator/><menuitem name='undoAction' action='undoAction'/><menuitem name='redoAction' action='redoAction'/><separator/><menuitem name='selectAllAction' action='selectAllAction'/><separator/><menuitem name='indentAction' action='indentAction'/><menuitem name='unindentAction' action='unindentAction'/><separator/><menuitem name='CommentRegionAction' action='CommentRegionAction'/><menuitem name='UncommentRegionAction' action='UncommentRegionAction'/></menu><menu name='ViewAction' action='ViewAction'><menuitem name='zoomInAction' action='zoomInAction'/><menuitem name='zoomOutAction' action='zoomOutAction'/><separator/><menuitem name='selectFontAction' action='selectFontAction'/><menu name='ShowAction' action='ShowAction'><menuitem name='EnvironmentTabAction' action='EnvironmentTabAction'/><menuitem name='LocalsTabAction' action='LocalsTabAction'/></menu></menu><menu name='ScriptAction' action='ScriptAction'><menu name='LanguageAction' action='LanguageAction'/><menuitem name='yesAction1' action='yesAction1'/><menuitem name='noAction' action='noAction'/><menuitem name='ShellAction' action='ShellAction'/></menu><menu name='HelpAction' action='HelpAction'><menuitem name='helpAction' action='helpAction'/><menuitem name='aboutAction' action='aboutAction'/></menu></menubar></ui>");
			this.menubar2 = ((global::Gtk.MenuBar)(this.UIManager.GetWidget ("/menubar2")));
			this.menubar2.Name = "menubar2";
			this.vbox1.Add (this.menubar2);
			global::Gtk.Box.BoxChild w2 = ((global::Gtk.Box.BoxChild)(this.vbox1 [this.menubar2]));
			w2.Position = 0;
			w2.Expand = false;
			w2.Fill = false;
			// Container child vbox1.Gtk.Box+BoxChild
			this.UIManager.AddUiFromString ("<ui><toolbar name='toolbar1'><toolitem name='newAction' action='newAction'/><toolitem name='openAction' action='openAction'/><toolitem name='yesAction1' action='yesAction1'/><toolitem name='noAction' action='noAction'/><toolitem name='goForwardAction' action='goForwardAction'/></toolbar></ui>");
			this.toolbar1 = ((global::Gtk.Toolbar)(this.UIManager.GetWidget ("/toolbar1")));
			this.toolbar1.Name = "toolbar1";
			this.toolbar1.ShowArrow = false;
			this.vbox1.Add (this.toolbar1);
			global::Gtk.Box.BoxChild w3 = ((global::Gtk.Box.BoxChild)(this.vbox1 [this.toolbar1]));
			w3.Position = 1;
			w3.Expand = false;
			w3.Fill = false;
			// Container child vbox1.Gtk.Box+BoxChild
			this.vpaned2 = new global::Gtk.VPaned ();
			this.vpaned2.CanFocus = true;
			this.vpaned2.Name = "vpaned2";
			this.vpaned2.Position = 307;
			// Container child vpaned2.Gtk.Paned+PanedChild
			this.notebook_docs = new global::Gtk.Notebook ();
			this.notebook_docs.CanFocus = true;
			this.notebook_docs.Name = "notebook_docs";
			this.notebook_docs.CurrentPage = 1;
			this.notebook_docs.Scrollable = true;
			// Container child notebook_docs.Gtk.Notebook+NotebookChild
			this.hbox1 = new global::Gtk.HBox ();
			this.hbox1.Name = "hbox1";
			this.hbox1.Spacing = 6;
			// Container child hbox1.Gtk.Box+BoxChild
			this.vbuttonbox1 = new global::Gtk.VButtonBox ();
			this.vbuttonbox1.Name = "vbuttonbox1";
			this.vbuttonbox1.LayoutStyle = ((global::Gtk.ButtonBoxStyle)(1));
			// Container child vbuttonbox1.Gtk.ButtonBox+ButtonBoxChild
			this.button4 = new global::Gtk.Button ();
			this.button4.CanFocus = true;
			this.button4.Name = "button4";
			this.button4.UseUnderline = true;
			// Container child button4.Gtk.Container+ContainerChild
			global::Gtk.Alignment w4 = new global::Gtk.Alignment (0.5F, 0.5F, 0F, 0F);
			// Container child GtkAlignment.Gtk.Container+ContainerChild
			global::Gtk.HBox w5 = new global::Gtk.HBox ();
			w5.Spacing = 2;
			// Container child GtkHBox.Gtk.Container+ContainerChild
			global::Gtk.Image w6 = new global::Gtk.Image ();
			w6.Pixbuf = global::Stetic.IconLoader.LoadIcon (this, "gtk-help", global::Gtk.IconSize.LargeToolbar);
			w5.Add (w6);
			// Container child GtkHBox.Gtk.Container+ContainerChild
			global::Gtk.Label w8 = new global::Gtk.Label ();
			w8.LabelProp = global::Mono.Unix.Catalog.GetString ("Getting started...");
			w8.UseUnderline = true;
			w5.Add (w8);
			w4.Add (w5);
			this.button4.Add (w4);
			this.vbuttonbox1.Add (this.button4);
			global::Gtk.ButtonBox.ButtonBoxChild w12 = ((global::Gtk.ButtonBox.ButtonBoxChild)(this.vbuttonbox1 [this.button4]));
			w12.Expand = false;
			w12.Fill = false;
			// Container child vbuttonbox1.Gtk.ButtonBox+ButtonBoxChild
			this.button3 = new global::Gtk.Button ();
			this.button3.CanFocus = true;
			this.button3.Name = "button3";
			this.button3.UseUnderline = true;
			// Container child button3.Gtk.Container+ContainerChild
			global::Gtk.Alignment w13 = new global::Gtk.Alignment (0.5F, 0.5F, 0F, 0F);
			// Container child GtkAlignment.Gtk.Container+ContainerChild
			global::Gtk.HBox w14 = new global::Gtk.HBox ();
			w14.Spacing = 2;
			// Container child GtkHBox.Gtk.Container+ContainerChild
			global::Gtk.Image w15 = new global::Gtk.Image ();
			w15.Pixbuf = global::Stetic.IconLoader.LoadIcon (this, "gtk-new", global::Gtk.IconSize.LargeToolbar);
			w14.Add (w15);
			// Container child GtkHBox.Gtk.Container+ContainerChild
			global::Gtk.Label w17 = new global::Gtk.Label ();
			w17.LabelProp = global::Mono.Unix.Catalog.GetString ("New script");
			w17.UseUnderline = true;
			w14.Add (w17);
			w13.Add (w14);
			this.button3.Add (w13);
			this.vbuttonbox1.Add (this.button3);
			global::Gtk.ButtonBox.ButtonBoxChild w21 = ((global::Gtk.ButtonBox.ButtonBoxChild)(this.vbuttonbox1 [this.button3]));
			w21.Position = 1;
			w21.Expand = false;
			w21.Fill = false;
			// Container child vbuttonbox1.Gtk.ButtonBox+ButtonBoxChild
			this.button2 = new global::Gtk.Button ();
			this.button2.CanFocus = true;
			this.button2.Name = "button2";
			this.button2.UseUnderline = true;
			// Container child button2.Gtk.Container+ContainerChild
			global::Gtk.Alignment w22 = new global::Gtk.Alignment (0.5F, 0.5F, 0F, 0F);
			// Container child GtkAlignment.Gtk.Container+ContainerChild
			global::Gtk.HBox w23 = new global::Gtk.HBox ();
			w23.Spacing = 2;
			// Container child GtkHBox.Gtk.Container+ContainerChild
			global::Gtk.Image w24 = new global::Gtk.Image ();
			w24.Pixbuf = global::Stetic.IconLoader.LoadIcon (this, "gtk-open", global::Gtk.IconSize.LargeToolbar);
			w23.Add (w24);
			// Container child GtkHBox.Gtk.Container+ContainerChild
			global::Gtk.Label w26 = new global::Gtk.Label ();
			w26.LabelProp = global::Mono.Unix.Catalog.GetString ("Open...");
			w26.UseUnderline = true;
			w23.Add (w26);
			w22.Add (w23);
			this.button2.Add (w22);
			this.vbuttonbox1.Add (this.button2);
			global::Gtk.ButtonBox.ButtonBoxChild w30 = ((global::Gtk.ButtonBox.ButtonBoxChild)(this.vbuttonbox1 [this.button2]));
			w30.Position = 2;
			w30.Expand = false;
			w30.Fill = false;
			// Container child vbuttonbox1.Gtk.ButtonBox+ButtonBoxChild
			this.button100 = new global::Gtk.Button ();
			this.button100.Sensitive = false;
			this.button100.CanFocus = true;
			this.button100.Name = "button100";
			this.button100.UseUnderline = true;
			this.button100.Label = global::Mono.Unix.Catalog.GetString ("Load Last Script...");
			this.vbuttonbox1.Add (this.button100);
			global::Gtk.ButtonBox.ButtonBoxChild w31 = ((global::Gtk.ButtonBox.ButtonBoxChild)(this.vbuttonbox1 [this.button100]));
			w31.Position = 3;
			w31.Expand = false;
			w31.Fill = false;
			this.hbox1.Add (this.vbuttonbox1);
			global::Gtk.Box.BoxChild w32 = ((global::Gtk.Box.BoxChild)(this.hbox1 [this.vbuttonbox1]));
			w32.Position = 0;
			w32.Expand = false;
			w32.Fill = false;
			// Container child hbox1.Gtk.Box+BoxChild
			this.image69 = new global::Gtk.Image ();
			this.image69.Name = "image69";
			this.image69.Pixbuf = new global::Gdk.Pixbuf (global::System.IO.Path.Combine (global::System.AppDomain.CurrentDomain.BaseDirectory, "abstract-butterfly-sm.gif"));
			this.hbox1.Add (this.image69);
			global::Gtk.Box.BoxChild w33 = ((global::Gtk.Box.BoxChild)(this.hbox1 [this.image69]));
			w33.Position = 1;
			this.notebook_docs.Add (this.hbox1);
			// Notebook tab
			this.label2 = new global::Gtk.Label ();
			this.label2.Name = "label2";
			this.label2.LabelProp = global::Mono.Unix.Catalog.GetString ("Home");
			this.notebook_docs.SetTabLabel (this.hbox1, this.label2);
			this.label2.ShowAll ();
			// Container child notebook_docs.Gtk.Notebook+NotebookChild
			this.hbox2 = new global::Gtk.HBox ();
			this.hbox2.Name = "hbox2";
			this.hbox2.Spacing = 6;
			// Container child hbox2.Gtk.Box+BoxChild
			this.vbox2 = new global::Gtk.VBox ();
			this.vbox2.Name = "vbox2";
			this.vbox2.Spacing = 6;
			// Container child vbox2.Gtk.Box+BoxChild
			this.vbox3 = new global::Gtk.VBox ();
			this.vbox3.Name = "vbox3";
			this.vbox3.Spacing = 6;
			// Container child vbox3.Gtk.Box+BoxChild
			this.prompt = new global::Gtk.Label ();
			this.prompt.Name = "prompt";
			this.prompt.Xalign = 0F;
			this.prompt.LabelProp = global::Mono.Unix.Catalog.GetString ("python>");
			this.vbox3.Add (this.prompt);
			global::Gtk.Box.BoxChild w35 = ((global::Gtk.Box.BoxChild)(this.vbox3 [this.prompt]));
			w35.Position = 0;
			w35.Expand = false;
			w35.Fill = false;
			// Container child vbox3.Gtk.Box+BoxChild
			this.hbox3 = new global::Gtk.HBox ();
			this.hbox3.Name = "hbox3";
			this.hbox3.Spacing = 6;
			// Container child hbox3.Gtk.Box+BoxChild
			this.history_up = new global::Gtk.Button ();
			this.history_up.Sensitive = false;
			this.history_up.Name = "history_up";
			this.history_up.UseUnderline = true;
			this.history_up.FocusOnClick = false;
			// Container child history_up.Gtk.Container+ContainerChild
			global::Gtk.Alignment w36 = new global::Gtk.Alignment (0.5F, 0.5F, 0F, 0F);
			// Container child GtkAlignment.Gtk.Container+ContainerChild
			global::Gtk.HBox w37 = new global::Gtk.HBox ();
			w37.Spacing = 2;
			// Container child GtkHBox.Gtk.Container+ContainerChild
			global::Gtk.Image w38 = new global::Gtk.Image ();
			w38.Pixbuf = global::Stetic.IconLoader.LoadIcon (this, "gtk-go-up", global::Gtk.IconSize.Menu);
			w37.Add (w38);
			// Container child GtkHBox.Gtk.Container+ContainerChild
			global::Gtk.Label w40 = new global::Gtk.Label ();
			w37.Add (w40);
			w36.Add (w37);
			this.history_up.Add (w36);
			this.hbox3.Add (this.history_up);
			global::Gtk.Box.BoxChild w44 = ((global::Gtk.Box.BoxChild)(this.hbox3 [this.history_up]));
			w44.Position = 0;
			w44.Expand = false;
			w44.Fill = false;
			// Container child hbox3.Gtk.Box+BoxChild
			this.history_down = new global::Gtk.Button ();
			this.history_down.Sensitive = false;
			this.history_down.Name = "history_down";
			this.history_down.UseUnderline = true;
			this.history_down.FocusOnClick = false;
			// Container child history_down.Gtk.Container+ContainerChild
			global::Gtk.Alignment w45 = new global::Gtk.Alignment (0.5F, 0.5F, 0F, 0F);
			// Container child GtkAlignment.Gtk.Container+ContainerChild
			global::Gtk.HBox w46 = new global::Gtk.HBox ();
			w46.Spacing = 2;
			// Container child GtkHBox.Gtk.Container+ContainerChild
			global::Gtk.Image w47 = new global::Gtk.Image ();
			w47.Pixbuf = global::Stetic.IconLoader.LoadIcon (this, "gtk-go-down", global::Gtk.IconSize.Menu);
			w46.Add (w47);
			// Container child GtkHBox.Gtk.Container+ContainerChild
			global::Gtk.Label w49 = new global::Gtk.Label ();
			w46.Add (w49);
			w45.Add (w46);
			this.history_down.Add (w45);
			this.hbox3.Add (this.history_down);
			global::Gtk.Box.BoxChild w53 = ((global::Gtk.Box.BoxChild)(this.hbox3 [this.history_down]));
			w53.Position = 1;
			w53.Expand = false;
			w53.Fill = false;
			this.vbox3.Add (this.hbox3);
			global::Gtk.Box.BoxChild w54 = ((global::Gtk.Box.BoxChild)(this.vbox3 [this.hbox3]));
			w54.Position = 1;
			w54.Expand = false;
			w54.Fill = false;
			this.vbox2.Add (this.vbox3);
			global::Gtk.Box.BoxChild w55 = ((global::Gtk.Box.BoxChild)(this.vbox2 [this.vbox3]));
			w55.Position = 0;
			w55.Expand = false;
			w55.Fill = false;
			this.hbox2.Add (this.vbox2);
			global::Gtk.Box.BoxChild w56 = ((global::Gtk.Box.BoxChild)(this.hbox2 [this.vbox2]));
			w56.Position = 0;
			w56.Expand = false;
			w56.Fill = false;
			// Container child hbox2.Gtk.Box+BoxChild
			this.scrolledwindow1 = new global::Gtk.ScrolledWindow ();
			this.scrolledwindow1.CanFocus = true;
			this.scrolledwindow1.Name = "scrolledwindow1";
			this.scrolledwindow1.ShadowType = ((global::Gtk.ShadowType)(1));
			this.hbox2.Add (this.scrolledwindow1);
			global::Gtk.Box.BoxChild w57 = ((global::Gtk.Box.BoxChild)(this.hbox2 [this.scrolledwindow1]));
			w57.Position = 1;
			this.notebook_docs.Add (this.hbox2);
			global::Gtk.Notebook.NotebookChild w58 = ((global::Gtk.Notebook.NotebookChild)(this.notebook_docs [this.hbox2]));
			w58.Position = 1;
			// Notebook tab
			this.label1 = new global::Gtk.Label ();
			this.label1.Name = "label1";
			this.label1.LabelProp = global::Mono.Unix.Catalog.GetString ("Shell");
			this.notebook_docs.SetTabLabel (this.hbox2, this.label1);
			this.label1.ShowAll ();
			this.vpaned2.Add (this.notebook_docs);
			global::Gtk.Paned.PanedChild w59 = ((global::Gtk.Paned.PanedChild)(this.vpaned2 [this.notebook_docs]));
			w59.Resize = false;
			// Container child vpaned2.Gtk.Paned+PanedChild
			this.notebook_tools = new global::Gtk.Notebook ();
			this.notebook_tools.CanFocus = true;
			this.notebook_tools.Name = "notebook_tools";
			this.notebook_tools.CurrentPage = 2;
			this.notebook_tools.Scrollable = true;
			// Container child notebook_tools.Gtk.Notebook+NotebookChild
			this.GtkScrolledWindow = new global::Gtk.ScrolledWindow ();
			this.GtkScrolledWindow.Name = "GtkScrolledWindow";
			this.GtkScrolledWindow.ShadowType = ((global::Gtk.ShadowType)(1));
			// Container child GtkScrolledWindow.Gtk.Container+ContainerChild
			this.textview1 = new global::Gtk.TextView ();
			this.textview1.CanFocus = true;
			this.textview1.Name = "textview1";
			this.textview1.Editable = false;
			this.textview1.AcceptsTab = false;
			this.GtkScrolledWindow.Add (this.textview1);
			this.notebook_tools.Add (this.GtkScrolledWindow);
			// Notebook tab
			this.label3 = new global::Gtk.Label ();
			this.label3.Name = "label3";
			this.label3.LabelProp = global::Mono.Unix.Catalog.GetString ("Output");
			this.notebook_tools.SetTabLabel (this.GtkScrolledWindow, this.label3);
			this.label3.ShowAll ();
			// Container child notebook_tools.Gtk.Notebook+NotebookChild
			this.GtkScrolledWindow1 = new global::Gtk.ScrolledWindow ();
			this.GtkScrolledWindow1.Name = "GtkScrolledWindow1";
			this.GtkScrolledWindow1.ShadowType = ((global::Gtk.ShadowType)(1));
			// Container child GtkScrolledWindow1.Gtk.Container+ContainerChild
			this.treeview1 = new global::Gtk.TreeView ();
			this.treeview1.CanFocus = true;
			this.treeview1.Name = "treeview1";
			this.GtkScrolledWindow1.Add (this.treeview1);
			this.notebook_tools.Add (this.GtkScrolledWindow1);
			global::Gtk.Notebook.NotebookChild w63 = ((global::Gtk.Notebook.NotebookChild)(this.notebook_tools [this.GtkScrolledWindow1]));
			w63.Position = 1;
			// Notebook tab
			this.label4 = new global::Gtk.Label ();
			this.label4.Name = "label4";
			this.label4.LabelProp = global::Mono.Unix.Catalog.GetString ("Environment");
			this.notebook_tools.SetTabLabel (this.GtkScrolledWindow1, this.label4);
			this.label4.ShowAll ();
			// Container child notebook_tools.Gtk.Notebook+NotebookChild
			this.scrolledwindow2 = new global::Gtk.ScrolledWindow ();
			this.scrolledwindow2.CanFocus = true;
			this.scrolledwindow2.Name = "scrolledwindow2";
			this.scrolledwindow2.ShadowType = ((global::Gtk.ShadowType)(1));
			// Container child scrolledwindow2.Gtk.Container+ContainerChild
			this.treeview2 = new global::Gtk.TreeView ();
			this.treeview2.CanFocus = true;
			this.treeview2.Name = "treeview2";
			this.scrolledwindow2.Add (this.treeview2);
			this.notebook_tools.Add (this.scrolledwindow2);
			global::Gtk.Notebook.NotebookChild w65 = ((global::Gtk.Notebook.NotebookChild)(this.notebook_tools [this.scrolledwindow2]));
			w65.Position = 2;
			// Notebook tab
			this.label5 = new global::Gtk.Label ();
			this.label5.Name = "label5";
			this.label5.LabelProp = global::Mono.Unix.Catalog.GetString ("Locals");
			this.notebook_tools.SetTabLabel (this.scrolledwindow2, this.label5);
			this.label5.ShowAll ();
			this.vpaned2.Add (this.notebook_tools);
			global::Gtk.Paned.PanedChild w66 = ((global::Gtk.Paned.PanedChild)(this.vpaned2 [this.notebook_tools]));
			w66.Resize = false;
			w66.Shrink = false;
			this.vbox1.Add (this.vpaned2);
			global::Gtk.Box.BoxChild w67 = ((global::Gtk.Box.BoxChild)(this.vbox1 [this.vpaned2]));
			w67.Position = 2;
			// Container child vbox1.Gtk.Box+BoxChild
			this.statusbar1 = new global::Gtk.Statusbar ();
			this.statusbar1.Name = "statusbar1";
			this.statusbar1.Spacing = 6;
			// Container child statusbar1.Gtk.Box+BoxChild
			this.label6 = new global::Gtk.Label ();
			this.label6.Name = "label6";
			this.label6.LabelProp = global::Mono.Unix.Catalog.GetString ("Language:");
			this.statusbar1.Add (this.label6);
			global::Gtk.Box.BoxChild w68 = ((global::Gtk.Box.BoxChild)(this.statusbar1 [this.label6]));
			w68.Position = 0;
			w68.Expand = false;
			w68.Fill = false;
			// Container child statusbar1.Gtk.Box+BoxChild
			this.status_langauge = new global::Gtk.Label ();
			this.status_langauge.Name = "status_langauge";
			this.status_langauge.LabelProp = global::Mono.Unix.Catalog.GetString ("<i>Python</i>");
			this.status_langauge.UseMarkup = true;
			this.statusbar1.Add (this.status_langauge);
			global::Gtk.Box.BoxChild w69 = ((global::Gtk.Box.BoxChild)(this.statusbar1 [this.status_langauge]));
			w69.Position = 1;
			w69.Expand = false;
			w69.Fill = false;
			this.vbox1.Add (this.statusbar1);
			global::Gtk.Box.BoxChild w70 = ((global::Gtk.Box.BoxChild)(this.vbox1 [this.statusbar1]));
			w70.Position = 3;
			w70.Expand = false;
			w70.Fill = false;
			this.Add (this.vbox1);
			if ((this.Child != null)) {
				this.Child.ShowAll ();
			}
			this.DefaultWidth = 607;
			this.DefaultHeight = 558;
			this.Show ();
			this.DeleteEvent += new global::Gtk.DeleteEventHandler (this.OnDeleteEvent);
			this.KeyPressEvent += new global::Gtk.KeyPressEventHandler (this.OnKeyPressEvent);
			this.newAction.Activated += new global::System.EventHandler (this.OnNewActionActivated);
			this.openAction.Activated += new global::System.EventHandler (this.OnOpenActionActivated);
			this.noAction.Activated += new global::System.EventHandler (this.OnNoActionActivated);
			this.goForwardAction.Activated += new global::System.EventHandler (this.OnGoForwardActionActivated);
			this.openAction1.Activated += new global::System.EventHandler (this.OnOpenAction1Activated);
			this.saveAction.Activated += new global::System.EventHandler (this.OnSaveActionActivated);
			this.saveAsAction.Activated += new global::System.EventHandler (this.OnSaveAsActionActivated);
			this.quitAction.Activated += new global::System.EventHandler (this.OnQuitActionActivated);
			this.copyAction.Activated += new global::System.EventHandler (this.OnCopyActionActivated);
			this.pasteAction.Activated += new global::System.EventHandler (this.OnPasteActionActivated);
			this.cutAction.Activated += new global::System.EventHandler (this.OnCutActionActivated);
			this.undoAction.Activated += new global::System.EventHandler (this.OnUndoActionActivated);
			this.redoAction.Activated += new global::System.EventHandler (this.OnRedoActionActivated);
			this.selectAllAction.Activated += new global::System.EventHandler (this.OnSelectAllActionActivated);
			this.indentAction.Activated += new global::System.EventHandler (this.OnIndentActionActivated);
			this.unindentAction.Activated += new global::System.EventHandler (this.OnUnindentActionActivated);
			this.CommentRegionAction.Activated += new global::System.EventHandler (this.OnCommentRegionActionActivated);
			this.UncommentRegionAction.Activated += new global::System.EventHandler (this.OnUncommentRegionActionActivated);
			this.yesAction1.Activated += new global::System.EventHandler (this.OnYesAction1Activated);
			this.zoomInAction.Activated += new global::System.EventHandler (this.OnZoomInActionActivated);
			this.zoomOutAction.Activated += new global::System.EventHandler (this.OnZoomOutActionActivated);
			this.ShellAction.Activated += new global::System.EventHandler (this.OnShellActionActivated);
			this.EnvironmentTabAction.Activated += new global::System.EventHandler (this.OnEnvironmentTabActionActivated);
			this.ExportAction1.Activated += new global::System.EventHandler (this.OnExportAction1Activated);
			this.notebook_docs.SwitchPage += new global::Gtk.SwitchPageHandler (this.OnNotebookDocsSwitchPage);
			this.button4.Clicked += new global::System.EventHandler (this.OnButton4Clicked);
			this.button3.Clicked += new global::System.EventHandler (this.OnButton3Clicked);
			this.button2.Clicked += new global::System.EventHandler (this.OnButton2Clicked);
			this.history_up.Clicked += new global::System.EventHandler (this.OnHistoryUpClicked);
			this.history_down.Clicked += new global::System.EventHandler (this.OnHistoryDownClicked);
		}
	}
}
