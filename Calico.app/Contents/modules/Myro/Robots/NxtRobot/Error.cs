using System;

using System.Diagnostics.CodeAnalysis;



namespace NxtNet

{

	/// <summary>

	/// Possible error codes sent by the NXT.

	/// </summary>

	[SuppressMessage( "Microsoft.Design", "CA1028:EnumStorageShouldBeInt32" ), SuppressMessage( "Microsoft.Naming", "CA1716:IdentifiersShouldNotMatchKeywords", MessageId = "Error" )]

	public enum Error : byte

	{

		/// <summary>

		/// Successful communication, no error.

		/// </summary>

		Success = 0x0,



		/// <summary>

		/// Pending communication transaction in progress.

		/// </summary>

		PendingTransactionInProgress = 0x20,



		/// <summary>

		/// Specified mailbox queue is empty.

		/// </summary>

		SpecifiedMailboxQueueEmpty = 0x40,



		/// <summary>

		/// Request failed (i.e. specified file not found).

		/// </summary>

		RequestFailed = 0xBD,



		/// <summary>

		/// Unknown command opcode.

		/// </summary>

		[SuppressMessage("Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "Opcode")]

		UnknownCommandOpcode = 0xBE,



		/// <summary>

		/// Insane packet.

		/// </summary>

		InsanePacket = 0xBF,



		/// <summary>

		/// Data contains out-of-range values.

		/// </summary>

		OutOfRangeValues = 0xC0,



		/// <summary>

		/// Communication bus error.

		/// </summary>

		CommunicationBusError = 0xDD,



		/// <summary>

		/// No free memory in communication buffer.

		/// </summary>

		NoFreeMemoryInComBuffer = 0xDE,



		/// <summary>

		/// Specified channel/connection is not valid.

		/// </summary>

		SpecifiedChannelIsNotValid = 0xDF,



		/// <summary>

		/// Specified channel/connection not configured or busy.

		/// </summary>

		SpecifiedChannelNotConfiguredOrBusy = 0xE0,



		/// <summary>

		/// No active program.

		/// </summary>

		NoActiveProgram = 0xEC,



		/// <summary>

		/// Illegal size specified.

		/// </summary>

		IllegalSizeSpecified = 0xED,



		/// <summary>

		/// Illegal mailbox queue ID specified.

		/// </summary>

		IllegalMailboxQueueId = 0xEE,



		/// <summary>

		/// Attempted to access invalid field of a structure.

		/// </summary>

		InvalidFieldAccess = 0xEF,



		/// <summary>

		/// Bad input or output specified.

		/// </summary>

		BadInputOrOutput = 0xF0,



		/// <summary>

		/// Insufficient memory available.

		/// </summary>

		InsufficientMemoryAvailable = 0xFB,



		/// <summary>

		/// Bad arguments.

		/// </summary>

		BadArguments = 0xFF

	}

}

