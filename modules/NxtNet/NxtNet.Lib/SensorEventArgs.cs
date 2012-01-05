using System;

namespace NxtNet
{
	/// <summary>
	/// Event parameters for sensor related events.
	/// </summary>
	public class SensorEventArgs : EventArgs
	{
		/// <summary>
		/// The sensor that raised the event
		/// </summary>
		public Sensor Sensor { get; private set; }

		/// <summary>
		/// The current raw value of the sensor that raised the event.
		/// </summary>
		public UInt16 RawValue 
		{
			get
			{
				return this.Sensor.State.RawValue;
			}
		}

		/// <summary>
		/// The current state of the sensor that raised the event.
		/// </summary>
		public SensorState State 
		{
			get
			{
				return this.Sensor.State;
			}
		}

		/// <summary>
		/// Creates a new <see cref="SensorEventArgs"/> instance using the specified <see cref="Sensor"/>.
		/// </summary>
		/// <param name="sensor">The sensor that raised the event.</param>
		public SensorEventArgs( Sensor sensor )
		{
			this.Sensor = sensor;
		}

	}

}
