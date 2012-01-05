using System;

namespace NxtNet
{
	/// <summary>
	/// The class that is responsible for managing a touch (pressure) sensor.
	/// The touch sensor is usually connected to port 1.
	/// </summary>
	public class TouchSensor : Sensor
	{
		#region Base class properties

		/// <summary>
		/// The touch sensor always operates in <see cref="SensorMode.Boolean"/> mode.
		/// </summary>
		protected override SensorMode Mode
		{
			get
			{
				return SensorMode.Boolean;
			}
		}


		/// <summary>
		/// The type of the touch sensor is <see cref="SensorType.Switch"/>.
		/// </summary>
		protected override SensorType Type
		{
			get
			{
				return SensorType.Switch;
			}
		}

		#endregion

		#region Local properties

		/// <summary>
		/// Returns <c>true</c> if the touch sensor was pressed when the sensor was last read.
		/// </summary>
		public bool IsPressed 
		{
			get
			{
				return ( base.State.ScaledValue == 1 );
			}
		}

		#endregion

	}

}
