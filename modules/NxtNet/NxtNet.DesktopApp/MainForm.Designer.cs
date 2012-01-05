namespace NxtNet.DesktopApp
{
	partial class MainForm
	{
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.IContainer components = null;

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
			this.cboPort = new System.Windows.Forms.ComboBox();
			this.btnConnect = new System.Windows.Forms.Button();
			this.lblStatus = new System.Windows.Forms.Label();
			this.btnDisconnect = new System.Windows.Forms.Button();
			this.lblVersion = new System.Windows.Forms.Label();
			this.txtName = new System.Windows.Forms.TextBox();
			this.lblPortPrompt = new System.Windows.Forms.Label();
			this.lblVersionPrompt = new System.Windows.Forms.Label();
			this.lblNamePrompt = new System.Windows.Forms.Label();
			this.btnRename = new System.Windows.Forms.Button();
			this.lblBatteryPrompt = new System.Windows.Forms.Label();
			this.pbBattery = new System.Windows.Forms.ProgressBar();
			this.lblBattery = new System.Windows.Forms.Label();
			this.lblBluetoothAddressPrompt = new System.Windows.Forms.Label();
			this.lblBluetoothAddress = new System.Windows.Forms.Label();
			this.lblFreeFlashPrompt = new System.Windows.Forms.Label();
			this.lblFreeFlash = new System.Windows.Forms.Label();
			this.btnUpdateStatus = new System.Windows.Forms.Button();
			this.grpStatus = new System.Windows.Forms.GroupBox();
			this.grpSensors = new System.Windows.Forms.GroupBox();
			this.lblScaledValuePrompt = new System.Windows.Forms.Label();
			this.lblRawValuePrompt = new System.Windows.Forms.Label();
			this.lblNormalizedValuePrompt = new System.Windows.Forms.Label();
			this.btnUpdateSensors = new System.Windows.Forms.Button();
			this.pbLogo = new System.Windows.Forms.PictureBox();
			this.lblKeepAlivePrompt = new System.Windows.Forms.Label();
			this.lblKeepAlive = new System.Windows.Forms.Label();
			this.ctrlUltrasonicSensor = new NxtNet.DesktopApp.SensorStateControl();
			this.ctrlLightSensor = new NxtNet.DesktopApp.SensorStateControl();
			this.ctrlSoundSensor = new NxtNet.DesktopApp.SensorStateControl();
			this.ctrlTouchSensor = new NxtNet.DesktopApp.SensorStateControl();
			this.grpStatus.SuspendLayout();
			this.grpSensors.SuspendLayout();
			( (System.ComponentModel.ISupportInitialize) ( this.pbLogo ) ).BeginInit();
			this.SuspendLayout();
			// 
			// cboPort
			// 
			this.cboPort.FormattingEnabled = true;
			this.cboPort.Location = new System.Drawing.Point( 101, 13 );
			this.cboPort.Name = "cboPort";
			this.cboPort.Size = new System.Drawing.Size( 111, 21 );
			this.cboPort.TabIndex = 0;
			// 
			// btnConnect
			// 
			this.btnConnect.Location = new System.Drawing.Point( 218, 11 );
			this.btnConnect.Name = "btnConnect";
			this.btnConnect.Size = new System.Drawing.Size( 75, 23 );
			this.btnConnect.TabIndex = 1;
			this.btnConnect.Text = "Connect";
			this.btnConnect.UseVisualStyleBackColor = true;
			this.btnConnect.Click += new System.EventHandler( this.btnConnect_Click );
			// 
			// lblStatus
			// 
			this.lblStatus.AutoSize = true;
			this.lblStatus.Location = new System.Drawing.Point( 380, 16 );
			this.lblStatus.Name = "lblStatus";
			this.lblStatus.Size = new System.Drawing.Size( 78, 13 );
			this.lblStatus.TabIndex = 2;
			this.lblStatus.Text = "Not connected";
			// 
			// btnDisconnect
			// 
			this.btnDisconnect.Location = new System.Drawing.Point( 299, 11 );
			this.btnDisconnect.Name = "btnDisconnect";
			this.btnDisconnect.Size = new System.Drawing.Size( 75, 23 );
			this.btnDisconnect.TabIndex = 3;
			this.btnDisconnect.Text = "Disconnect";
			this.btnDisconnect.UseVisualStyleBackColor = true;
			this.btnDisconnect.Click += new System.EventHandler( this.btnDisconnect_Click );
			// 
			// lblVersion
			// 
			this.lblVersion.AutoSize = true;
			this.lblVersion.Location = new System.Drawing.Point( 86, 48 );
			this.lblVersion.Name = "lblVersion";
			this.lblVersion.Size = new System.Drawing.Size( 27, 13 );
			this.lblVersion.TabIndex = 4;
			this.lblVersion.Text = "N/A";
			// 
			// txtName
			// 
			this.txtName.Location = new System.Drawing.Point( 89, 20 );
			this.txtName.MaxLength = 15;
			this.txtName.Name = "txtName";
			this.txtName.Size = new System.Drawing.Size( 111, 20 );
			this.txtName.TabIndex = 5;
			this.txtName.Text = "N/A";
			// 
			// lblPortPrompt
			// 
			this.lblPortPrompt.AutoSize = true;
			this.lblPortPrompt.Location = new System.Drawing.Point( 18, 16 );
			this.lblPortPrompt.Name = "lblPortPrompt";
			this.lblPortPrompt.Size = new System.Drawing.Size( 76, 13 );
			this.lblPortPrompt.TabIndex = 6;
			this.lblPortPrompt.Text = "Bluetooth port:";
			// 
			// lblVersionPrompt
			// 
			this.lblVersionPrompt.AutoSize = true;
			this.lblVersionPrompt.Location = new System.Drawing.Point( 6, 48 );
			this.lblVersionPrompt.Name = "lblVersionPrompt";
			this.lblVersionPrompt.Size = new System.Drawing.Size( 45, 13 );
			this.lblVersionPrompt.TabIndex = 7;
			this.lblVersionPrompt.Text = "Version:";
			// 
			// lblNamePrompt
			// 
			this.lblNamePrompt.AutoSize = true;
			this.lblNamePrompt.Location = new System.Drawing.Point( 6, 23 );
			this.lblNamePrompt.Name = "lblNamePrompt";
			this.lblNamePrompt.Size = new System.Drawing.Size( 38, 13 );
			this.lblNamePrompt.TabIndex = 8;
			this.lblNamePrompt.Text = "Name:";
			// 
			// btnRename
			// 
			this.btnRename.Location = new System.Drawing.Point( 206, 18 );
			this.btnRename.Name = "btnRename";
			this.btnRename.Size = new System.Drawing.Size( 75, 23 );
			this.btnRename.TabIndex = 9;
			this.btnRename.Text = "Rename";
			this.btnRename.UseVisualStyleBackColor = true;
			this.btnRename.Click += new System.EventHandler( this.btnRename_Click );
			// 
			// lblBatteryPrompt
			// 
			this.lblBatteryPrompt.AutoSize = true;
			this.lblBatteryPrompt.Location = new System.Drawing.Point( 6, 128 );
			this.lblBatteryPrompt.Name = "lblBatteryPrompt";
			this.lblBatteryPrompt.Size = new System.Drawing.Size( 43, 13 );
			this.lblBatteryPrompt.TabIndex = 10;
			this.lblBatteryPrompt.Text = "Battery:";
			// 
			// pbBattery
			// 
			this.pbBattery.Location = new System.Drawing.Point( 89, 123 );
			this.pbBattery.Maximum = 9000;
			this.pbBattery.Name = "pbBattery";
			this.pbBattery.Size = new System.Drawing.Size( 111, 23 );
			this.pbBattery.Step = 1;
			this.pbBattery.Style = System.Windows.Forms.ProgressBarStyle.Continuous;
			this.pbBattery.TabIndex = 11;
			// 
			// lblBattery
			// 
			this.lblBattery.AutoSize = true;
			this.lblBattery.Location = new System.Drawing.Point( 206, 128 );
			this.lblBattery.Name = "lblBattery";
			this.lblBattery.Size = new System.Drawing.Size( 27, 13 );
			this.lblBattery.TabIndex = 12;
			this.lblBattery.Text = "N/A";
			// 
			// lblBluetoothAddressPrompt
			// 
			this.lblBluetoothAddressPrompt.AutoSize = true;
			this.lblBluetoothAddressPrompt.Location = new System.Drawing.Point( 6, 73 );
			this.lblBluetoothAddressPrompt.Name = "lblBluetoothAddressPrompt";
			this.lblBluetoothAddressPrompt.Size = new System.Drawing.Size( 65, 13 );
			this.lblBluetoothAddressPrompt.TabIndex = 13;
			this.lblBluetoothAddressPrompt.Text = "BT Address:";
			// 
			// lblBluetoothAddress
			// 
			this.lblBluetoothAddress.AutoSize = true;
			this.lblBluetoothAddress.Location = new System.Drawing.Point( 86, 73 );
			this.lblBluetoothAddress.Name = "lblBluetoothAddress";
			this.lblBluetoothAddress.Size = new System.Drawing.Size( 27, 13 );
			this.lblBluetoothAddress.TabIndex = 14;
			this.lblBluetoothAddress.Text = "N/A";
			// 
			// lblFreeFlashPrompt
			// 
			this.lblFreeFlashPrompt.AutoSize = true;
			this.lblFreeFlashPrompt.Location = new System.Drawing.Point( 6, 98 );
			this.lblFreeFlashPrompt.Name = "lblFreeFlashPrompt";
			this.lblFreeFlashPrompt.Size = new System.Drawing.Size( 56, 13 );
			this.lblFreeFlashPrompt.TabIndex = 15;
			this.lblFreeFlashPrompt.Text = "Free flash:";
			// 
			// lblFreeFlash
			// 
			this.lblFreeFlash.AutoSize = true;
			this.lblFreeFlash.Location = new System.Drawing.Point( 86, 98 );
			this.lblFreeFlash.Name = "lblFreeFlash";
			this.lblFreeFlash.Size = new System.Drawing.Size( 27, 13 );
			this.lblFreeFlash.TabIndex = 16;
			this.lblFreeFlash.Text = "N/A";
			// 
			// btnUpdateStatus
			// 
			this.btnUpdateStatus.Location = new System.Drawing.Point( 9, 183 );
			this.btnUpdateStatus.Name = "btnUpdateStatus";
			this.btnUpdateStatus.Size = new System.Drawing.Size( 75, 23 );
			this.btnUpdateStatus.TabIndex = 17;
			this.btnUpdateStatus.Text = "Update";
			this.btnUpdateStatus.UseVisualStyleBackColor = true;
			this.btnUpdateStatus.Click += new System.EventHandler( this.btnUpdateStatus_Click );
			// 
			// grpStatus
			// 
			this.grpStatus.Controls.Add( this.lblKeepAlive );
			this.grpStatus.Controls.Add( this.lblKeepAlivePrompt );
			this.grpStatus.Controls.Add( this.lblNamePrompt );
			this.grpStatus.Controls.Add( this.btnUpdateStatus );
			this.grpStatus.Controls.Add( this.txtName );
			this.grpStatus.Controls.Add( this.lblBattery );
			this.grpStatus.Controls.Add( this.lblFreeFlash );
			this.grpStatus.Controls.Add( this.pbBattery );
			this.grpStatus.Controls.Add( this.btnRename );
			this.grpStatus.Controls.Add( this.lblBluetoothAddress );
			this.grpStatus.Controls.Add( this.lblFreeFlashPrompt );
			this.grpStatus.Controls.Add( this.lblVersionPrompt );
			this.grpStatus.Controls.Add( this.lblBluetoothAddressPrompt );
			this.grpStatus.Controls.Add( this.lblBatteryPrompt );
			this.grpStatus.Controls.Add( this.lblVersion );
			this.grpStatus.Location = new System.Drawing.Point( 12, 50 );
			this.grpStatus.Name = "grpStatus";
			this.grpStatus.Size = new System.Drawing.Size( 293, 217 );
			this.grpStatus.TabIndex = 18;
			this.grpStatus.TabStop = false;
			this.grpStatus.Text = "Status";
			// 
			// grpSensors
			// 
			this.grpSensors.Controls.Add( this.lblScaledValuePrompt );
			this.grpSensors.Controls.Add( this.lblRawValuePrompt );
			this.grpSensors.Controls.Add( this.lblNormalizedValuePrompt );
			this.grpSensors.Controls.Add( this.ctrlUltrasonicSensor );
			this.grpSensors.Controls.Add( this.ctrlLightSensor );
			this.grpSensors.Controls.Add( this.ctrlSoundSensor );
			this.grpSensors.Controls.Add( this.ctrlTouchSensor );
			this.grpSensors.Controls.Add( this.btnUpdateSensors );
			this.grpSensors.Location = new System.Drawing.Point( 317, 50 );
			this.grpSensors.Name = "grpSensors";
			this.grpSensors.Size = new System.Drawing.Size( 370, 217 );
			this.grpSensors.TabIndex = 19;
			this.grpSensors.TabStop = false;
			this.grpSensors.Text = "Sensors";
			// 
			// lblScaledValuePrompt
			// 
			this.lblScaledValuePrompt.AutoSize = true;
			this.lblScaledValuePrompt.Location = new System.Drawing.Point( 270, 23 );
			this.lblScaledValuePrompt.Name = "lblScaledValuePrompt";
			this.lblScaledValuePrompt.Size = new System.Drawing.Size( 43, 13 );
			this.lblScaledValuePrompt.TabIndex = 27;
			this.lblScaledValuePrompt.Text = "Scaled:";
			// 
			// lblRawValuePrompt
			// 
			this.lblRawValuePrompt.AutoSize = true;
			this.lblRawValuePrompt.Location = new System.Drawing.Point( 198, 23 );
			this.lblRawValuePrompt.Name = "lblRawValuePrompt";
			this.lblRawValuePrompt.Size = new System.Drawing.Size( 32, 13 );
			this.lblRawValuePrompt.TabIndex = 26;
			this.lblRawValuePrompt.Text = "Raw:";
			// 
			// lblNormalizedValuePrompt
			// 
			this.lblNormalizedValuePrompt.AutoSize = true;
			this.lblNormalizedValuePrompt.Location = new System.Drawing.Point( 92, 23 );
			this.lblNormalizedValuePrompt.Name = "lblNormalizedValuePrompt";
			this.lblNormalizedValuePrompt.Size = new System.Drawing.Size( 62, 13 );
			this.lblNormalizedValuePrompt.TabIndex = 25;
			this.lblNormalizedValuePrompt.Text = "Normalized:";
			// 
			// btnUpdateSensors
			// 
			this.btnUpdateSensors.Location = new System.Drawing.Point( 6, 183 );
			this.btnUpdateSensors.Name = "btnUpdateSensors";
			this.btnUpdateSensors.Size = new System.Drawing.Size( 75, 23 );
			this.btnUpdateSensors.TabIndex = 0;
			this.btnUpdateSensors.Text = "Update";
			this.btnUpdateSensors.UseVisualStyleBackColor = true;
			this.btnUpdateSensors.Click += new System.EventHandler( this.btnUpdateSensors_Click );
			// 
			// pbLogo
			// 
			this.pbLogo.Image = ( (System.Drawing.Image) ( resources.GetObject( "pbLogo.Image" ) ) );
			this.pbLogo.Location = new System.Drawing.Point( 608, 273 );
			this.pbLogo.Name = "pbLogo";
			this.pbLogo.Size = new System.Drawing.Size( 79, 79 );
			this.pbLogo.TabIndex = 20;
			this.pbLogo.TabStop = false;
			// 
			// lblKeepAlivePrompt
			// 
			this.lblKeepAlivePrompt.AutoSize = true;
			this.lblKeepAlivePrompt.Location = new System.Drawing.Point( 6, 158 );
			this.lblKeepAlivePrompt.Name = "lblKeepAlivePrompt";
			this.lblKeepAlivePrompt.Size = new System.Drawing.Size( 60, 13 );
			this.lblKeepAlivePrompt.TabIndex = 18;
			this.lblKeepAlivePrompt.Text = "Keep alive:";
			// 
			// lblKeepAlive
			// 
			this.lblKeepAlive.AutoSize = true;
			this.lblKeepAlive.Location = new System.Drawing.Point( 86, 158 );
			this.lblKeepAlive.Name = "lblKeepAlive";
			this.lblKeepAlive.Size = new System.Drawing.Size( 27, 13 );
			this.lblKeepAlive.TabIndex = 21;
			this.lblKeepAlive.Text = "N/A";
			// 
			// ctrlUltrasonicSensor
			// 
			this.ctrlUltrasonicSensor.Location = new System.Drawing.Point( 7, 150 );
			this.ctrlUltrasonicSensor.Mode = NxtNet.SensorMode.Raw;
			this.ctrlUltrasonicSensor.Name = "ctrlUltrasonicSensor";
			this.ctrlUltrasonicSensor.Nxt = null;
			this.ctrlUltrasonicSensor.Port = NxtNet.SensorPort.Port4;
			this.ctrlUltrasonicSensor.Size = new System.Drawing.Size( 330, 30 );
			this.ctrlUltrasonicSensor.TabIndex = 24;
			this.ctrlUltrasonicSensor.Title = "Distance:";
			this.ctrlUltrasonicSensor.Type = NxtNet.SensorType.LowSpeed9V;
			// 
			// ctrlLightSensor
			// 
			this.ctrlLightSensor.Location = new System.Drawing.Point( 6, 113 );
			this.ctrlLightSensor.Mode = NxtNet.SensorMode.FullScale;
			this.ctrlLightSensor.Name = "ctrlLightSensor";
			this.ctrlLightSensor.Nxt = null;
			this.ctrlLightSensor.Port = NxtNet.SensorPort.Port3;
			this.ctrlLightSensor.Size = new System.Drawing.Size( 330, 30 );
			this.ctrlLightSensor.TabIndex = 23;
			this.ctrlLightSensor.Title = "Light (inactive):";
			this.ctrlLightSensor.Type = NxtNet.SensorType.LightInactive;
			// 
			// ctrlSoundSensor
			// 
			this.ctrlSoundSensor.Location = new System.Drawing.Point( 6, 77 );
			this.ctrlSoundSensor.Mode = NxtNet.SensorMode.FullScale;
			this.ctrlSoundSensor.Name = "ctrlSoundSensor";
			this.ctrlSoundSensor.Nxt = null;
			this.ctrlSoundSensor.Port = NxtNet.SensorPort.Port2;
			this.ctrlSoundSensor.Size = new System.Drawing.Size( 330, 30 );
			this.ctrlSoundSensor.TabIndex = 22;
			this.ctrlSoundSensor.Title = "Sound (dBA):";
			this.ctrlSoundSensor.Type = NxtNet.SensorType.SoundDba;
			// 
			// ctrlTouchSensor
			// 
			this.ctrlTouchSensor.Location = new System.Drawing.Point( 6, 41 );
			this.ctrlTouchSensor.Mode = NxtNet.SensorMode.Boolean;
			this.ctrlTouchSensor.Name = "ctrlTouchSensor";
			this.ctrlTouchSensor.Nxt = null;
			this.ctrlTouchSensor.Port = NxtNet.SensorPort.Port1;
			this.ctrlTouchSensor.Size = new System.Drawing.Size( 330, 30 );
			this.ctrlTouchSensor.TabIndex = 21;
			this.ctrlTouchSensor.Title = "Touch:";
			this.ctrlTouchSensor.Type = NxtNet.SensorType.Switch;
			// 
			// MainForm
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF( 6F, 13F );
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.ClientSize = new System.Drawing.Size( 699, 365 );
			this.Controls.Add( this.pbLogo );
			this.Controls.Add( this.grpSensors );
			this.Controls.Add( this.grpStatus );
			this.Controls.Add( this.lblPortPrompt );
			this.Controls.Add( this.btnDisconnect );
			this.Controls.Add( this.lblStatus );
			this.Controls.Add( this.btnConnect );
			this.Controls.Add( this.cboPort );
			this.Icon = ( (System.Drawing.Icon) ( resources.GetObject( "$this.Icon" ) ) );
			this.Name = "MainForm";
			this.Text = "NXT.NET Desktop Remote";
			this.Load += new System.EventHandler( this.MainForm_Load );
			this.grpStatus.ResumeLayout( false );
			this.grpStatus.PerformLayout();
			this.grpSensors.ResumeLayout( false );
			this.grpSensors.PerformLayout();
			( (System.ComponentModel.ISupportInitialize) ( this.pbLogo ) ).EndInit();
			this.ResumeLayout( false );
			this.PerformLayout();

		}

		#endregion

		private System.Windows.Forms.ComboBox cboPort;
		private System.Windows.Forms.Button btnConnect;
		private System.Windows.Forms.Label lblStatus;
		private System.Windows.Forms.Button btnDisconnect;
		private System.Windows.Forms.Label lblVersion;
		private System.Windows.Forms.TextBox txtName;
		private System.Windows.Forms.Label lblPortPrompt;
		private System.Windows.Forms.Label lblVersionPrompt;
		private System.Windows.Forms.Label lblNamePrompt;
		private System.Windows.Forms.Button btnRename;
		private System.Windows.Forms.Label lblBatteryPrompt;
		private System.Windows.Forms.ProgressBar pbBattery;
		private System.Windows.Forms.Label lblBattery;
		private System.Windows.Forms.Label lblBluetoothAddressPrompt;
		private System.Windows.Forms.Label lblBluetoothAddress;
		private System.Windows.Forms.Label lblFreeFlashPrompt;
		private System.Windows.Forms.Label lblFreeFlash;
		private System.Windows.Forms.Button btnUpdateStatus;
		private System.Windows.Forms.GroupBox grpStatus;
		private System.Windows.Forms.GroupBox grpSensors;
		private System.Windows.Forms.PictureBox pbLogo;
		private System.Windows.Forms.Button btnUpdateSensors;
		private SensorStateControl ctrlTouchSensor;
		private SensorStateControl ctrlSoundSensor;
		private SensorStateControl ctrlLightSensor;
		private SensorStateControl ctrlUltrasonicSensor;
		private System.Windows.Forms.Label lblNormalizedValuePrompt;
		private System.Windows.Forms.Label lblRawValuePrompt;
		private System.Windows.Forms.Label lblScaledValuePrompt;
		private System.Windows.Forms.Label lblKeepAlive;
		private System.Windows.Forms.Label lblKeepAlivePrompt;
	}
}

