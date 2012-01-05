using System;
using System.Diagnostics.CodeAnalysis;

namespace NxtNet
{
	/// <summary>
	/// Structure that describes the current state of a motor.
	/// </summary>
	public class MotorState
	{
		/// <summary>
		/// Power (also referred as speed) set point. Range: -100-100.
		/// </summary>
		/// <remarks>
		/// The absolute value of <see cref="Power"/> is used as a percentage of the full power capabilities of the motor.
		/// The sign of <see cref="Power"/> specifies rotation direction. 
		/// Positive values for <see cref="Power"/> instruct the firmware to turn the motor forward, 
		/// while negative values instruct the firmware to turn the motor backward. 
		/// "Forward" and "backward" are relative to a standard orientation for a particular type of motor.
		/// Note that direction is not a meaningful concept for outputs like lamps. 
		/// Lamps are affected only by the absolute value of <see cref="Power"/>.
		/// </remarks>
		public sbyte Power { get; set; }

		/// <summary>
		/// Motor mode. (Bit-field.)
		/// </summary>
		public MotorModes Mode { get; set; }

		/// <summary>
		/// Motor regulation mode.
		/// </summary>
		public MotorRegulationMode Regulation { get; set; }

		/// <summary>
		/// This property specifies the proportional turning ratio for synchronized turning using two motors. Range: -100-100.
		/// </summary>
		/// <remarks>
		/// Negative <paramref name="turnRatio"/> values shift power towards the left motor, 
		/// whereas positive <paramref name="turnRatio"/> values shift power towards the right motor. 
		/// In both cases, the actual power applied is proportional to the <paramref name="power"/> set-point, 
		/// such that an absolute value of 50% for <paramref name="turnRatio"/> normally results in one motor stopping,
		/// and an absolute value of 100% for <paramref name="turnRatio"/> normally results in the two motors 
		/// turning in opposite directions at equal power.
		/// </remarks>
		public sbyte TurnRatio { get; set; }

		/// <summary>
		/// Motor run state.
		/// </summary>
		public MotorRunState RunState { get; set; }

		/// <summary>
		/// Current limit on a movement in progress, if any.
		/// </summary>
		/// <remarks>
		/// This property specifies the rotational distance in 
		/// degrees that you want to turn the motor. Range: 0-4294967295, O: run forever.
		/// The sign of the <see cref="Power"/> property specifies the direction of rotation.
		/// </remarks>
		[SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "Tacho")]
		public UInt32 TachoLimit { get; set; }

		/// <summary>
		/// Internal count. Number of counts since last reset of the motor counter.
		/// </summary>
		/// <remarks>
		/// This property returns the internal position counter value for the specified port.
		/// The sign of <see cref="TachoCount"/> specifies rotation direction. 
		/// Positive values correspond to forward rotation while negative values correspond to backward rotation. 
		/// "Forward" and "backward" are relative to a standard orientation for a particular type of motor.
		/// </remarks>
		[SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "Tacho")]
		public Int32 TachoCount { get; set; }

		/// <summary>
		/// Current position relative to last programmed movement. Range: -2147483648-2147483647.
		/// </summary>
		/// <remarks>
		/// This property reports the block-relative position counter value for the specified port.
		/// The sign of <see cref="BlockTachoCount" /> specifies the rotation direction. Positive values correspond to forward
		/// rotation while negative values correspond to backward rotation. "Forward" and "backward" are relative to
		/// a standard orientation for a particular type of motor.
		/// </remarks>
		[SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "Tacho")]
		public Int32 BlockTachoCount { get; set; }

		/// <summary>
		/// Current position relative to last reset of the rotation sensor for this motor. Range: -2147483648-2147483647.
		/// </summary>
		/// <remarks>
		/// This property returns the program-relative position counter value for the specified port.
		/// The sign of <see cref="RotationCount" /> specifies rotation direction. Positive values correspond to forward rotation
		/// while negative values correspond to backward rotation. 
		/// "Forward" and "backward" are relative to a standard orientation for a particular type of motor.
		/// </remarks>
		public Int32 RotationCount { get; set; }
	}

}
