using System;

namespace NxtNet
{
	/// <summary>
	/// The class that is responsible for managing a sound sensor.
	/// The sound sensor is usually connected to port 2.
	/// </summary>
	public class SoundSensor : Sensor
	{
		#region Base class properties

		/// <summary>
		/// The sound sensor always operates in <see cref="SensorMode.FullScale"/> mode.
		/// </summary>
		protected override SensorMode Mode
		{
			get
			{
				return SensorMode.FullScale;
			}
		}


		/// <summary>
		/// The type of the sound sensor is <see cref="SensorType.SoundDba"/>
		/// or <see cref="SensorType.SoundDB"/> depending on the value of <see cref="AdjustForHumanEar"/>.
		/// </summary>
		protected override SensorType Type
		{
			get
			{
				return this.AdjustForHumanEar ? SensorType.SoundDba : SensorType.SoundDB;
			}
		}

		#endregion

		#region Local properties

		/// <summary>
		/// <c>True</c> if the sensor should compensate for the sensitivity
		/// of the human ear, <c>false</c> otherwise.
		/// </summary>
		public bool AdjustForHumanEar { get; set; }


		/// <summary>
		/// The current value of the sound sensor.
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
