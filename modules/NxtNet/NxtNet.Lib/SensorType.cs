using System;
using System.Diagnostics.CodeAnalysis;

namespace NxtNet
{
	/// <summary>
	/// Available sensor types.
	/// </summary>
	public enum SensorType
	{
		/// <summary>
		/// No sensor connected to the port.
		/// </summary>
		NoSensor = 0x00,

		/// <summary>
		/// NXT or RCX touch sensor.
		/// </summary>
		Switch = 0x01,

		/// <summary>
		/// RCX temperature sensor.
		/// </summary>
		Temperature = 0x02,

		/// <summary>
		/// RCX light sensor.
		/// </summary>
		Reflection = 0x03,

		/// <summary>
		/// RCX rotation sensor.
		/// </summary>
		Angle = 0x04,

		/// <summary>
		/// NXT light sensor with floodlight enabled.
		/// </summary>
		LightActive = 0x05,

		/// <summary>
		/// NXT light sensor detecting ambient light (floodlight disabled).
		/// </summary>
		LightInactive = 0x06,

		/// <summary>
		/// NXT sound sensor, decibel scaling.
		/// </summary>
		SoundDB = 0x07,

		/// <summary>
		/// NXT sound sensor, dBA scaling (decibel adjusted for the human ear).
		/// </summary>
		[SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "Dba")]
		SoundDba = 0x08,

		/// <summary>
		/// Custom sensor, unused in NXT programs.
		/// </summary>
		Custom = 0x09,

		/// <summary>
		/// Low speed (I2C digital) sensor.
		/// </summary>
		LowSpeed = 0x0A,

		/// <summary>
		/// Low speed (I2C digital) sensor, 9V power (like the ultrasonic sensor).
		/// </summary>
		LowSpeed9V = 0x0B,

		/// <summary>
		/// None of the specified sensor types. High speed. Unused in NXT programs.
		/// </summary>
		NoneOfSensorTypes = 0x0C
	}
}
