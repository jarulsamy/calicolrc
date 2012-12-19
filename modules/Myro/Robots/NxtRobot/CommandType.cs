using System;



namespace NxtNet

{

	/// <summary>

	/// Possible command types.

	/// </summary>

	enum CommandType : byte

	{

		/// <summary>

		/// Direct command with a response from the NXT (0x00).

		/// </summary>

		DirectCommandWithResponse = 0x00,



		/// <summary>

		/// Direct command without a response from the NXT (0x80).

		/// </summary>

		DirectCommandWithoutResponse = 0x80,



		/// <summary>

		/// Reply command. (Command type for response packages, 0x02.)

		/// </summary>

		Reply = 0x02,



		/// <summary>

		/// System command with a response from the NXT (0x01).

		/// </summary>

		SystemCommandWithResponse = 0x01,



		/// <summary>

		/// System command without a response from the NXT (0x81).

		/// </summary>

		SystemCommandWithoutResponse = 0x81

	}

}

