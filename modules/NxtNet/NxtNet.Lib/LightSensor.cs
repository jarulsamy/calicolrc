using System;

namespace NxtNet
{
	/// <summary>
	/// The class that is responsible for managing a light sensor.
	/// The light sensor is usually connected to port 3.
	/// </summary>
	public class LightSensor : Sensor
	{
		#region Base class properties

		/// <summary>
		/// The light sensor always operates in <see cref="SensorMode.FullScale"/> mode.
		/// </summary>
		protected override SensorMode Mode
		{
			get
			{
				return SensorMode.FullScale;
			}
		}


		/// <summary>
		/// The type of the touch sensor is <see cref="SensorType.LightActive"/> or 
		/// <see cref="SensorType.LightInactive"/> depending on the value of <see cref="LightActive"/>.
		/// </summary>
		protected override SensorType Type
		{
			get
			{
				return this.LightActive ? SensorType.LightActive : SensorType.LightInactive;
			}
		}

		#endregion

		#region Local properties

		/// <summary>
		/// Returns <c>true</c> if the light sensor has its light turned on 
		/// or <c>false</c> if the sensor measures ambient light.
		/// </summary>
		public bool LightActive { get; set; }


		/// <summary>
		/// The current value of the light sensor.
		/// </summary>
		/// <remarks>
		/// Returns <c>this.State.ScaledValue</c>.
		/// </remarks>
		public int Value
		{
			get
			{
				return this.State.ScaledValue;
			}
		}

		#endregion

	}

}
