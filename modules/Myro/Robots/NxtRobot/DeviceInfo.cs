using System;

using System.Globalization;



namespace NxtNet

{

	/// <summary>

	/// The structure that describes the NXT brick.

	/// </summary>

	public class DeviceInfo

	{

		/// <summary>

		/// The name of the NXT brick. Maximum 14 characters.

		/// </summary>

		public string Name { get; set; }



		/// <summary>

		/// The Bluetooth address of the NXT.

		/// </summary>

		public byte[] BluetoothAddress { get; set; }



		/// <summary>

		/// Bluetooth signal strength.

		/// </summary>

		public UInt32 SignalStrength { get; set; }



		/// <summary>

		/// Available memory for user applications in bytes.

		/// </summary>

		public UInt32 FreeUserFlash { get; set; }





		/// <summary>

		/// Gets the string representation of all device information data..

		/// </summary>

		/// <returns>All <see cref="DeviceInfo"/> property values in a single string.</returns>

		/// <remarks>The format is the following: "Name: {0}, BluetoothAddress: {1}, SignalStrength: {2}, FreeUserFlash: {3}"</remarks>

		public override string ToString()

		{

			return String.Format( CultureInfo.InvariantCulture, "Name: {0}, BluetoothAddress: {1}, SignalStrength: {2}, FreeUserFlash: {3}",

				this.Name, this.BluetoothAddress.ToHexString(), this.SignalStrength, this.FreeUserFlash );

		}



	}



}

