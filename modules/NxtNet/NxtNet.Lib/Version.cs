using System;
using System.Globalization;

namespace NxtNet
{
	/// <summary>
	/// The current protocol and firmware version running on the NXT.
	/// </summary>
	public class Version
	{
		/// <summary>
		/// The version of the firmware loaded to the NXT.
		/// </summary>
		public string Firmware { get; set; }

		/// <summary>
		/// The version of the protocol used by the NXT.
		/// </summary>
		public string Protocol { get; set; }


		/// <summary>
		/// Gets the string representation of both version numbers.
		/// </summary>
		/// <returns>Firmware and protocol numbers in a single string.</returns>
		/// <remarks>The format is the following: "Firmware: {0}, Protocol: {1}", this.Firmware, this.Protocol</remarks>
		public override string ToString()
		{
			return String.Format( CultureInfo.InvariantCulture, "Firmware: {0}, Protocol: {1}", this.Firmware, this.Protocol );
		}

	}

}
