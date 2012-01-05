using System;
using System.Diagnostics.CodeAnalysis;

namespace NxtNet
{
	/// <summary>
	/// Available motor ports.
	/// </summary>
	public enum MotorPort
	{
		/// <summary>
		/// Motor connected to port A.
		/// </summary>
		[SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "A")]
		PortA = 0,

		/// <summary>
		/// Motor connected to port B.
		/// </summary>
		[SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "B")]
		PortB = 1,

		/// <summary>
		/// Motor connected to port C.
		/// </summary>
		[SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "C")]
		PortC = 2,

		/// <summary>
		/// All motors connected to A, B and C ports.
		/// </summary>
		All = 0xFF
	}
}
