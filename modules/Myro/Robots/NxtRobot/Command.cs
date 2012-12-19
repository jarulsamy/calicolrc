using System;



namespace NxtNet

{

	/// <summary>

	/// Available commands recognized by the NXT.

	/// </summary>

	enum Command : byte

	{

		GetBatteryLevel = 0x0B,

		GetCurrentProgramName = 0x11,

		GetDeviceInfo = 0x9B,

		GetFirmwareVersion = 0x88,

		GetInputValues = 0x07,

		GetOutputState = 0x06,

		KeepAlive = 0x0D,

		LowSpeedGetStatus = 0x0E,

		LowSpeedRead = 0x10,

		LowSpeedWrite = 0x0F,

		MessageRead = 0x13,

		MessageWrite  =0x09,

		PlayTone = 0x03,

		PlaySoundFile = 0x02,

		ResetInputScaledValue = 0x08,

		ResetMotorPosition = 0x0A,

		SetBrickName = 0x98,

		SetInputMode = 0x05,

		SetOutputState = 0x04,

		StartProgram = 0x00,

		StopProgram = 0x01,

		StopSoundPlayback = 0x0C

	}

}

