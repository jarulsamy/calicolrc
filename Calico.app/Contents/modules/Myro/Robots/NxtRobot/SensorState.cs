using System;

using System.Globalization;



namespace NxtNet

{

	/// <summary>

	/// Structure that describes the current state and value of a sensor.

	/// </summary>

	public class SensorState

	{

		/// <summary>

		/// <c>True</c>, if new data value should be seen as valid data.

		/// </summary>

		public bool Valid { get; set; }



		/// <summary>

		/// <c>True</c>, if calibration file found and used for <see cref="CalibratedValue"/> field.

		/// </summary>

		public bool Calibrated { get; set; }



		/// <summary>

		/// The type of the sensor.

		/// </summary>

		[System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1721:PropertyNamesShouldNotMatchGetMethods")]

		public SensorType Type { get; set; }



		/// <summary>

		/// The mode in which the sensor currently operates.

		/// </summary>

		public SensorMode Mode { get; set; }



		/// <summary>

		/// Raw A/D value of the sensor. (UWORD, device dependent)

		/// </summary>

		public UInt16 RawValue { get; set; }



		/// <summary>

		/// Normalized A/D value of the sensor. (UWORD, type dependent, Range: 0-1023)

		/// </summary>

		public UInt16 NormalizedValue { get; set; }



		/// <summary>

		/// Scaled value. (SWORD, mode dependent)

		/// The sensor mode affects the scaled value, which the NXT firmware calculates 

		/// depending on the sensor type and sensor mode.

		/// </summary>

		/// <remarks>

		/// The legal value range depends on <see cref="SensorMode"/>, as listed below:

		/// Raw: [0, 1023]

		/// Boolean: [0, 1]

		/// TransitionCount: [0, 65535]

		/// PeriodCounter: [0, 65535]

		/// FullScale: [0, 100]

		/// Celsius: [-200, 700] (readings in 10th of a degree Celsius)

		/// Fahrenheit: [-400, 1580] (readings in 10th of a degree Fahrenheit)

		/// AngleStep: [0, 65535]

		/// </remarks>

		public Int16 ScaledValue { get; set; }



		/// <summary>

		/// Value scaled according to calibration (SWORD, currently unused by the NXT)

		/// </summary>

		public Int16 CalibratedValue { get; set; }





		/// <summary>

		/// Returns the complete sensor state in string format.

		/// </summary>

		/// <returns>All details of the current state of the sensor in string format.</returns>

		public override string ToString()

		{

			return String.Format(CultureInfo.InvariantCulture,

				"Valid: {0}, Calibrated: {1}, Type: {2}, Mode: {3}, RawValue: {4}, NormalizedValue: {5}, ScaledValue: {6}, CalibratedValue: {7}",

				this.Valid, this.Calibrated, this.Type, this.Mode, this.RawValue, this.NormalizedValue, this.ScaledValue, this.CalibratedValue);

		}



	}

}

