using System;
using System.Diagnostics.CodeAnalysis;

namespace NxtNet
{
	/// <summary>
	/// Base class for common sensor features.
	/// </summary>
	public class Sensor
	{
		#region Fields

		/// <summary>
		/// Private store for the <see cref="Port"/> property.
		/// </summary>
		private SensorPort _port = SensorPort.None;

		#endregion


		#region Properties

		/// <summary>
		/// The <see cref="Nxt"/> object that is responsible for managing all communication with the NXT.
		/// </summary>
		[SuppressMessage( "Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "Nxt" )]
		public Nxt Nxt { get; set; }

		/// <summary>
		/// The port to which the sensor is currently connected.
		/// </summary>
		public SensorPort Port 
		{
			get
			{
				return this._port;
			}
			set
			{
				this._port = value;
			}
		}

		/// <summary>
		/// The type of the sensor. The default is <see cref="SensorType.NoSensor"/>.
		/// </summary>
		[SuppressMessage( "Microsoft.Naming", "CA1721:PropertyNamesShouldNotMatchGetMethods" )]
		protected virtual SensorType Type 
		{
			get
			{
				return SensorType.NoSensor;
			}
		}

		/// <summary>
		/// The mode in which the sensor operates. The default is <see cref="SensorMode.Raw"/>.
		/// </summary>
		protected virtual SensorMode Mode 
		{
			get
			{
				return SensorMode.Raw;
			}
		}

		/// <summary>
		/// The current state of the sensor, including the value of the sensor.
		/// </summary>
		public SensorState State { get; private set; }

		#endregion


		#region Events

		/// <summary>
		/// Event that is raised when the value of the sensor is read.
		/// </summary>
		public event EventHandler<SensorEventArgs> Polled;

		#endregion


		/// <summary>
		/// Connects the sensor to the specified port.
		/// </summary>
		public virtual void Init()
		{
			this.Nxt.SetInputMode( this.Port, this.Type, this.Mode );
		}


		/// <summary>
		/// Reads the current value of the sensor and raises the <see cref="Polled"/> event when the new value is avaiable.
		/// </summary>
		/// <exception cref="InvalidOperationException">
		/// If the <see cref="Nxt"/> property is not initialized or the <see cref="Port"/> is not set.
		/// </exception>
		public virtual void Poll()
		{
			// Abort if the NXT is not initialized.
			if( this.Nxt == null )
			{
				throw new InvalidOperationException( "Set the Nxt property before reading the value of the sensor." );
			}

			if( this.Port == SensorPort.None )
			{
				throw new InvalidOperationException( "The sensor should be connected to a port." );
			}

			// Read the current value of the sensor.
			this.State = this.Nxt.GetInputValues( this.Port );

			// Raise an event to signal that the new value is available.
			if( this.Polled != null )
			{
				this.Polled( this, new SensorEventArgs( this ) );
			}		
		}

	}
}
