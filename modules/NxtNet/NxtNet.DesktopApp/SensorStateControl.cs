using System;
using System.ComponentModel;
using System.Globalization;
using System.Windows.Forms;
using System.Diagnostics.CodeAnalysis;

namespace NxtNet.DesktopApp
{
	/// <summary>
	/// Control that is able to read and display sensor values.
	/// </summary>
	public partial class SensorStateControl : UserControl
	{
		#region Public properties

		/// <summary>
		/// The port the sensor is connected to.
		/// </summary>
		[Category( "NXT" )]
		[Description( "The port the sensor is connected to." )]
		[CLSCompliant( false )]
		public SensorPort Port { get; set; }

		/// <summary>
		/// The type of the sensor connected to the port.
		/// </summary>
		[SuppressMessage( "Microsoft.Naming", "CA1721:PropertyNamesShouldNotMatchGetMethods" ), Category( "NXT" )]
		[Description( "The type of the sensor connected to the port." )]
		[CLSCompliant( false )]
		public SensorType Type { get; set; }

		/// <summary>
		/// The mode in which the sensor operates. The sensor mode affects the scaled value, 
		/// which the NXT firmware calculates depending on the sensor type and sensor mode.
		/// </summary>
		[Category( "NXT" )]
		[Description( "The mode in which the sensor operates. The sensor mode affects the scaled value, which the NXT firmware calculates depending on the sensor type and sensor mode." )]
		[CLSCompliant( false )]
		public SensorMode Mode { get; set; }

		/// <summary>
		/// The <see cref="Nxt"/> the sensor is connected to.
		/// </summary>
		[SuppressMessage( "Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "Nxt" ), Browsable( false )]
		[CLSCompliant( false )]
		public Nxt Nxt { get; set; }

		/// <summary>
		/// The name of the sensor that will be displayed.
		/// </summary>
		[Category( "NXT" )]
		[Description( "The name of the sensor that will be displayed." )]
		public string Title { get; set; }

		#endregion

		/// <summary>
		/// Creates a new <see cref="SensorStateControl"/> instance.
		/// </summary>
		public SensorStateControl()
		{
			this.InitializeComponent();
		}


		/// <summary>
		/// Re-initializes the sensor, re-reads and displays its values.
		/// </summary>
		public void UpdateValues()
		{
			// Configure the sensor on the input port.
			this.Nxt.SetInputMode( this.Port, this.Type, this.Mode );

			// Read sensor values.
			SensorState state = this.Nxt.GetInputValues( this.Port );

			// Display sensor values.
			this.lblTitle.Text = this.Title;
			this.pbNormalizedValue.Value = state.NormalizedValue;
			this.lblNormalizedValue.Text = state.NormalizedValue.ToString( CultureInfo.CurrentCulture );
			this.lblRawValue.Text = state.RawValue.ToString( CultureInfo.CurrentCulture );

			// Format the scaled value according to SensorMode.
			switch( this.Mode )
			{
				case SensorMode.Boolean:
					this.lblScaledValue.Text = state.ScaledValue == 1 ? "pressed" : "released";				
					break;

				case SensorMode.FullScale:
					this.lblScaledValue.Text = String.Format( CultureInfo.CurrentCulture, "{0}%", state.ScaledValue );
					break;

				default:
					break;
			}
		}




	}
}
