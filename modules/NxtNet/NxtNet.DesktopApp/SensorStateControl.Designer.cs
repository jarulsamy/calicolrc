namespace NxtNet.DesktopApp
{
	partial class SensorStateControl
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

		#region Component Designer generated code

		/// <summary> 
		/// Required method for Designer support - do not modify 
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
			this.lblTitle = new System.Windows.Forms.Label();
			this.pbNormalizedValue = new System.Windows.Forms.ProgressBar();
			this.lblNormalizedValue = new System.Windows.Forms.Label();
			this.lblRawValue = new System.Windows.Forms.Label();
			this.lblScaledValue = new System.Windows.Forms.Label();
			this.SuspendLayout();
			// 
			// lblTitle
			// 
			this.lblTitle.AutoSize = true;
			this.lblTitle.Location = new System.Drawing.Point( 3, 8 );
			this.lblTitle.Name = "lblTitle";
			this.lblTitle.Size = new System.Drawing.Size( 27, 13 );
			this.lblTitle.TabIndex = 0;
			this.lblTitle.Text = "N/A";
			// 
			// pbNormalizedValue
			// 
			this.pbNormalizedValue.Location = new System.Drawing.Point( 86, 3 );
			this.pbNormalizedValue.Maximum = 1023;
			this.pbNormalizedValue.Name = "pbNormalizedValue";
			this.pbNormalizedValue.Size = new System.Drawing.Size( 100, 23 );
			this.pbNormalizedValue.Style = System.Windows.Forms.ProgressBarStyle.Continuous;
			this.pbNormalizedValue.TabIndex = 1;
			// 
			// lblNormalizedValue
			// 
			this.lblNormalizedValue.AutoSize = true;
			this.lblNormalizedValue.BackColor = System.Drawing.SystemColors.Control;
			this.lblNormalizedValue.Location = new System.Drawing.Point( 123, 8 );
			this.lblNormalizedValue.Name = "lblNormalizedValue";
			this.lblNormalizedValue.Size = new System.Drawing.Size( 27, 13 );
			this.lblNormalizedValue.TabIndex = 2;
			this.lblNormalizedValue.Text = "N/A";
			// 
			// lblRawValue
			// 
			this.lblRawValue.AutoSize = true;
			this.lblRawValue.Location = new System.Drawing.Point( 192, 8 );
			this.lblRawValue.Name = "lblRawValue";
			this.lblRawValue.Size = new System.Drawing.Size( 27, 13 );
			this.lblRawValue.TabIndex = 3;
			this.lblRawValue.Text = "N/A";
			// 
			// lblScaledValue
			// 
			this.lblScaledValue.AutoSize = true;
			this.lblScaledValue.Location = new System.Drawing.Point( 264, 8 );
			this.lblScaledValue.Name = "lblScaledValue";
			this.lblScaledValue.Size = new System.Drawing.Size( 27, 13 );
			this.lblScaledValue.TabIndex = 4;
			this.lblScaledValue.Text = "N/A";
			// 
			// SensorStateControl
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF( 6F, 13F );
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.Controls.Add( this.lblScaledValue );
			this.Controls.Add( this.lblRawValue );
			this.Controls.Add( this.lblNormalizedValue );
			this.Controls.Add( this.pbNormalizedValue );
			this.Controls.Add( this.lblTitle );
			this.Name = "SensorStateControl";
			this.Size = new System.Drawing.Size( 330, 30 );
			this.ResumeLayout( false );
			this.PerformLayout();

		}

		#endregion

		private System.Windows.Forms.Label lblTitle;
		private System.Windows.Forms.ProgressBar pbNormalizedValue;
		private System.Windows.Forms.Label lblNormalizedValue;
		private System.Windows.Forms.Label lblRawValue;
		private System.Windows.Forms.Label lblScaledValue;
	}
}
