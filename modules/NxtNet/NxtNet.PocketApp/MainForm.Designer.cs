namespace NxtNet.PocketApp
{
	partial class MainForm
	{
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.IContainer components = null;
		private System.Windows.Forms.MainMenu mnuMain;

		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		/// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
		protected override void Dispose( bool disposing )
		{
			if( disposing && ( components != null ) )
			{
				components.Dispose();
			}
			base.Dispose( disposing );
		}

		#region Windows Form Designer generated code

		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
			System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager( typeof( MainForm ) );
			this.mnuMain = new System.Windows.Forms.MainMenu();
			this.mnuFile = new System.Windows.Forms.MenuItem();
			this.mnuExit = new System.Windows.Forms.MenuItem();
			this.tabControl = new System.Windows.Forms.TabControl();
			this.tabConnect = new System.Windows.Forms.TabPage();
			this.lblStatus = new System.Windows.Forms.Label();
			this.lblStatusPrompt = new System.Windows.Forms.Label();
			this.btnDisconnect = new System.Windows.Forms.Button();
			this.btnConnect = new System.Windows.Forms.Button();
			this.cboPort = new System.Windows.Forms.ComboBox();
			this.lblPortPrompt = new System.Windows.Forms.Label();
			this.tabStatus = new System.Windows.Forms.TabPage();
			this.btnUpdateStatus = new System.Windows.Forms.Button();
			this.lblFreeFlash = new System.Windows.Forms.Label();
			this.lblFreeFlashPrompt = new System.Windows.Forms.Label();
			this.lblBluetoothAddress = new System.Windows.Forms.Label();
			this.lblBluetoothAddressPrompt = new System.Windows.Forms.Label();
			this.lblBattery = new System.Windows.Forms.Label();
			this.pbBattery = new System.Windows.Forms.ProgressBar();
			this.lblBatteryPrompt = new System.Windows.Forms.Label();
			this.btnRename = new System.Windows.Forms.Button();
			this.txtName = new System.Windows.Forms.TextBox();
			this.lblNamePrompt = new System.Windows.Forms.Label();
			this.lblVersion = new System.Windows.Forms.Label();
			this.lblVersionPrompt = new System.Windows.Forms.Label();
			this.tabSensors = new System.Windows.Forms.TabPage();
			this.pbLogo = new System.Windows.Forms.PictureBox();
			this.tabControl.SuspendLayout();
			this.tabConnect.SuspendLayout();
			this.tabStatus.SuspendLayout();
			this.SuspendLayout();
			// 
			// mnuMain
			// 
			this.mnuMain.MenuItems.Add( this.mnuFile );
			this.mnuMain.MenuItems.Add( this.mnuExit );
			// 
			// mnuFile
			// 
			this.mnuFile.Text = "File";
			// 
			// mnuExit
			// 
			this.mnuExit.Text = "Exit";
			this.mnuExit.Click += new System.EventHandler( this.mnuExit_Click );
			// 
			// tabControl
			// 
			this.tabControl.Controls.Add( this.tabConnect );
			this.tabControl.Controls.Add( this.tabStatus );
			this.tabControl.Controls.Add( this.tabSensors );
			this.tabControl.Dock = System.Windows.Forms.DockStyle.Fill;
			this.tabControl.Location = new System.Drawing.Point( 0, 0 );
			this.tabControl.Name = "tabControl";
			this.tabControl.SelectedIndex = 0;
			this.tabControl.Size = new System.Drawing.Size( 240, 268 );
			this.tabControl.TabIndex = 0;
			this.tabControl.SelectedIndexChanged += new System.EventHandler( this.tabControl_SelectedIndexChanged );
			// 
			// tabConnect
			// 
			this.tabConnect.Controls.Add( this.pbLogo );
			this.tabConnect.Controls.Add( this.lblStatus );
			this.tabConnect.Controls.Add( this.lblStatusPrompt );
			this.tabConnect.Controls.Add( this.btnDisconnect );
			this.tabConnect.Controls.Add( this.btnConnect );
			this.tabConnect.Controls.Add( this.cboPort );
			this.tabConnect.Controls.Add( this.lblPortPrompt );
			this.tabConnect.Location = new System.Drawing.Point( 0, 0 );
			this.tabConnect.Name = "tabConnect";
			this.tabConnect.Size = new System.Drawing.Size( 240, 245 );
			this.tabConnect.Text = "Connect";
			// 
			// lblStatus
			// 
			this.lblStatus.Location = new System.Drawing.Point( 65, 172 );
			this.lblStatus.Name = "lblStatus";
			this.lblStatus.Size = new System.Drawing.Size( 168, 20 );
			this.lblStatus.Text = "Not connected";
			// 
			// lblStatusPrompt
			// 
			this.lblStatusPrompt.Location = new System.Drawing.Point( 7, 172 );
			this.lblStatusPrompt.Name = "lblStatusPrompt";
			this.lblStatusPrompt.Size = new System.Drawing.Size( 55, 20 );
			this.lblStatusPrompt.Text = "Status:";
			// 
			// btnDisconnect
			// 
			this.btnDisconnect.Location = new System.Drawing.Point( 133, 52 );
			this.btnDisconnect.Name = "btnDisconnect";
			this.btnDisconnect.Size = new System.Drawing.Size( 100, 100 );
			this.btnDisconnect.TabIndex = 3;
			this.btnDisconnect.Text = "Disconnect";
			this.btnDisconnect.Click += new System.EventHandler( this.btnDisconnect_Click );
			// 
			// btnConnect
			// 
			this.btnConnect.Location = new System.Drawing.Point( 7, 52 );
			this.btnConnect.Name = "btnConnect";
			this.btnConnect.Size = new System.Drawing.Size( 100, 100 );
			this.btnConnect.TabIndex = 2;
			this.btnConnect.Text = "Connect";
			this.btnConnect.Click += new System.EventHandler( this.btnConnect_Click );
			// 
			// cboPort
			// 
			this.cboPort.Location = new System.Drawing.Point( 113, 7 );
			this.cboPort.Name = "cboPort";
			this.cboPort.Size = new System.Drawing.Size( 120, 22 );
			this.cboPort.TabIndex = 1;
			// 
			// lblPortPrompt
			// 
			this.lblPortPrompt.Location = new System.Drawing.Point( 7, 9 );
			this.lblPortPrompt.Name = "lblPortPrompt";
			this.lblPortPrompt.Size = new System.Drawing.Size( 100, 20 );
			this.lblPortPrompt.Text = "Bluetooth port:";
			// 
			// tabStatus
			// 
			this.tabStatus.Controls.Add( this.btnUpdateStatus );
			this.tabStatus.Controls.Add( this.lblFreeFlash );
			this.tabStatus.Controls.Add( this.lblFreeFlashPrompt );
			this.tabStatus.Controls.Add( this.lblBluetoothAddress );
			this.tabStatus.Controls.Add( this.lblBluetoothAddressPrompt );
			this.tabStatus.Controls.Add( this.lblBattery );
			this.tabStatus.Controls.Add( this.pbBattery );
			this.tabStatus.Controls.Add( this.lblBatteryPrompt );
			this.tabStatus.Controls.Add( this.btnRename );
			this.tabStatus.Controls.Add( this.txtName );
			this.tabStatus.Controls.Add( this.lblNamePrompt );
			this.tabStatus.Controls.Add( this.lblVersion );
			this.tabStatus.Controls.Add( this.lblVersionPrompt );
			this.tabStatus.Location = new System.Drawing.Point( 0, 0 );
			this.tabStatus.Name = "tabStatus";
			this.tabStatus.Size = new System.Drawing.Size( 232, 242 );
			this.tabStatus.Text = "Status";
			// 
			// btnUpdateStatus
			// 
			this.btnUpdateStatus.Location = new System.Drawing.Point( 77, 172 );
			this.btnUpdateStatus.Name = "btnUpdateStatus";
			this.btnUpdateStatus.Size = new System.Drawing.Size( 100, 60 );
			this.btnUpdateStatus.TabIndex = 22;
			this.btnUpdateStatus.Text = "Update";
			this.btnUpdateStatus.Click += new System.EventHandler( this.btnUpdateStatus_Click );
			// 
			// lblFreeFlash
			// 
			this.lblFreeFlash.Location = new System.Drawing.Point( 83, 143 );
			this.lblFreeFlash.Name = "lblFreeFlash";
			this.lblFreeFlash.Size = new System.Drawing.Size( 150, 20 );
			this.lblFreeFlash.Text = "N/A";
			// 
			// lblFreeFlashPrompt
			// 
			this.lblFreeFlashPrompt.Location = new System.Drawing.Point( 7, 143 );
			this.lblFreeFlashPrompt.Name = "lblFreeFlashPrompt";
			this.lblFreeFlashPrompt.Size = new System.Drawing.Size( 70, 20 );
			this.lblFreeFlashPrompt.Text = "Free mem:";
			// 
			// lblBluetoothAddress
			// 
			this.lblBluetoothAddress.Location = new System.Drawing.Point( 83, 123 );
			this.lblBluetoothAddress.Name = "lblBluetoothAddress";
			this.lblBluetoothAddress.Size = new System.Drawing.Size( 150, 20 );
			this.lblBluetoothAddress.Text = "N/A";
			// 
			// lblBluetoothAddressPrompt
			// 
			this.lblBluetoothAddressPrompt.Location = new System.Drawing.Point( 7, 123 );
			this.lblBluetoothAddressPrompt.Name = "lblBluetoothAddressPrompt";
			this.lblBluetoothAddressPrompt.Size = new System.Drawing.Size( 70, 20 );
			this.lblBluetoothAddressPrompt.Text = "BT Address:";
			// 
			// lblBattery
			// 
			this.lblBattery.Location = new System.Drawing.Point( 179, 88 );
			this.lblBattery.Name = "lblBattery";
			this.lblBattery.Size = new System.Drawing.Size( 54, 20 );
			this.lblBattery.Text = "N/A";
			// 
			// pbBattery
			// 
			this.pbBattery.Location = new System.Drawing.Point( 83, 88 );
			this.pbBattery.Maximum = 9000;
			this.pbBattery.Name = "pbBattery";
			this.pbBattery.Size = new System.Drawing.Size( 90, 20 );
			// 
			// lblBatteryPrompt
			// 
			this.lblBatteryPrompt.Location = new System.Drawing.Point( 7, 88 );
			this.lblBatteryPrompt.Name = "lblBatteryPrompt";
			this.lblBatteryPrompt.Size = new System.Drawing.Size( 70, 20 );
			this.lblBatteryPrompt.Text = "Battery:";
			// 
			// btnRename
			// 
			this.btnRename.Location = new System.Drawing.Point( 83, 54 );
			this.btnRename.Name = "btnRename";
			this.btnRename.Size = new System.Drawing.Size( 90, 20 );
			this.btnRename.TabIndex = 9;
			this.btnRename.Text = "Rename";
			this.btnRename.Click += new System.EventHandler( this.btnRename_Click );
			// 
			// txtName
			// 
			this.txtName.Location = new System.Drawing.Point( 83, 27 );
			this.txtName.MaxLength = 15;
			this.txtName.Name = "txtName";
			this.txtName.Size = new System.Drawing.Size( 150, 21 );
			this.txtName.TabIndex = 8;
			this.txtName.Text = "N/A";
			// 
			// lblNamePrompt
			// 
			this.lblNamePrompt.Location = new System.Drawing.Point( 7, 28 );
			this.lblNamePrompt.Name = "lblNamePrompt";
			this.lblNamePrompt.Size = new System.Drawing.Size( 70, 20 );
			this.lblNamePrompt.Text = "Name:";
			// 
			// lblVersion
			// 
			this.lblVersion.Location = new System.Drawing.Point( 83, 4 );
			this.lblVersion.Name = "lblVersion";
			this.lblVersion.Size = new System.Drawing.Size( 150, 20 );
			this.lblVersion.Text = "N/A";
			// 
			// lblVersionPrompt
			// 
			this.lblVersionPrompt.Location = new System.Drawing.Point( 7, 4 );
			this.lblVersionPrompt.Name = "lblVersionPrompt";
			this.lblVersionPrompt.Size = new System.Drawing.Size( 70, 20 );
			this.lblVersionPrompt.Text = "Version:";
			// 
			// tabSensors
			// 
			this.tabSensors.Location = new System.Drawing.Point( 0, 0 );
			this.tabSensors.Name = "tabSensors";
			this.tabSensors.Size = new System.Drawing.Size( 232, 242 );
			this.tabSensors.Text = "Sensors";
			// 
			// pbLogo
			// 
			this.pbLogo.Image = ( (System.Drawing.Image) ( resources.GetObject( "pbLogo.Image" ) ) );
			this.pbLogo.Location = new System.Drawing.Point( 189, 194 );
			this.pbLogo.Name = "pbLogo";
			this.pbLogo.Size = new System.Drawing.Size( 48, 48 );
			this.pbLogo.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage;
			// 
			// MainForm
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF( 96F, 96F );
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.AutoScroll = true;
			this.ClientSize = new System.Drawing.Size( 240, 268 );
			this.Controls.Add( this.tabControl );
			this.Icon = ( (System.Drawing.Icon) ( resources.GetObject( "$this.Icon" ) ) );
			this.Menu = this.mnuMain;
			this.Name = "MainForm";
			this.Text = "NXT.NET Remote";
			this.Load += new System.EventHandler( this.MainForm_Load );
			this.tabControl.ResumeLayout( false );
			this.tabConnect.ResumeLayout( false );
			this.tabStatus.ResumeLayout( false );
			this.ResumeLayout( false );

		}

		#endregion

		private System.Windows.Forms.MenuItem mnuFile;
		private System.Windows.Forms.MenuItem mnuExit;
		private System.Windows.Forms.TabControl tabControl;
		private System.Windows.Forms.TabPage tabConnect;
		private System.Windows.Forms.TabPage tabSensors;
		private System.Windows.Forms.Label lblPortPrompt;
		private System.Windows.Forms.ComboBox cboPort;
		private System.Windows.Forms.Button btnDisconnect;
		private System.Windows.Forms.Button btnConnect;
		private System.Windows.Forms.Label lblStatus;
		private System.Windows.Forms.Label lblStatusPrompt;
		private System.Windows.Forms.TabPage tabStatus;
		private System.Windows.Forms.Label lblNamePrompt;
		private System.Windows.Forms.Label lblVersion;
		private System.Windows.Forms.Label lblVersionPrompt;
		private System.Windows.Forms.Button btnRename;
		private System.Windows.Forms.TextBox txtName;
		private System.Windows.Forms.Label lblBattery;
		private System.Windows.Forms.ProgressBar pbBattery;
		private System.Windows.Forms.Label lblBatteryPrompt;
		private System.Windows.Forms.Label lblFreeFlashPrompt;
		private System.Windows.Forms.Label lblBluetoothAddress;
		private System.Windows.Forms.Label lblBluetoothAddressPrompt;
		private System.Windows.Forms.Label lblFreeFlash;
		private System.Windows.Forms.Button btnUpdateStatus;
		private System.Windows.Forms.PictureBox pbLogo;
	}
}

