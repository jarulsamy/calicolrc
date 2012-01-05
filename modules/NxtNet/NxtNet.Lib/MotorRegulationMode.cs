using System;

namespace NxtNet
{
	/// <summary>
	/// Available motor regulation modes.
	/// </summary>
	public enum MotorRegulationMode
	{
		/// <summary>
		/// No regulation will be enabled.
		/// </summary>
		Idle = 0x00,

		/// <summary>
		/// Power control will be enabled on the specified output.
		/// </summary>
		/// <remarks>
		/// Speed regulation means that the NXT firmware attempts to 
		/// maintain a certain speed according to the <c>power</c> set-point
		/// To accomplish this, the NXT firmware automatically adjusts the actual 
		/// PWM duty cycle if the motor is affected by a physical load.
		/// </remarks>
		Speed = 0x01,

		/// <summary>
		/// Synchronization will be enabled (needs enabled on two output).
		/// </summary>
		/// <remarks>
		/// Synchronization means that the firmware attempts keep any two motors in synch 
		/// regardless of varying physical loads. You typically use this mode is to maintain 
		/// a straight path for a vehicle robot automatically. You also can use this mode with 
		/// the <c>turnRatio</c> parameter property to provide proportional turning. 
		/// You must set <c>Sync</c> on at least two motor ports to have the desired affect. 
		/// If <c>Sync</c> is set on all three motor ports, only the first two (A and B) are synchronized.
		/// </remarks>
		Sync = 0x02
	}
}
