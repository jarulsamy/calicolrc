using System;
using System.Diagnostics.CodeAnalysis;
using System.Globalization;
using System.IO;
using System.IO.Ports;
using System.Threading;

namespace NxtNet
{
	/// <summary>
	/// The class that is responsible for managing the communication with the NXT.
	/// </summary>
	[SuppressMessage( "Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "Nxt" )]
	public class Nxt : IDisposable
	{
		#region IDisposable Members

		/// <summary>
		/// <c>True</c>, if the class is already disposed and should not be used any more.
		/// </summary>
		private bool _isDisposed;


		/// <summary>
		/// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
		/// </summary>
		/// <param name="disposing">
		/// <c>True</c>, if the method is called directly from user code or
		/// <c>false</c>, if the method is called from inside the finalizer.
		/// </param>
		/// <remarks>
		/// This method executes in two distinct scenarios. 
		/// If <paramref name="disposing"/> equals <c>true</c>, the method has been called 
		/// directly or indirectly by a user's code. 
		/// Managed and unmanaged resources can be disposed.
		/// If <paramref name="disposing"/> equals <c>false</c>, the method has been called 
		/// by the runtime from inside the finalizer and you should not reference other objects. 
		/// Only unmanaged resources can be disposed.
		/// </remarks>
		protected virtual void Dispose( bool disposing )
		{
			if( !this._isDisposed )
			{
				if( disposing )
				{
					// Dispose managed resources.
					if( this._port != null )
					{
						if( this._port.IsOpen )
						{
							this._port.Close();
						}

						this._port.Dispose();
					}

					if( this._mre != null )
					{
						this._mre.Close();
					}
				}

				// Free native resources here if necessary.

				// Disposing has been done.
				this._isDisposed = true;
			}
		}


		/// <summary>
		/// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
		/// </summary>
		/// <remarks>
		/// Closes the serial port and disposes the channel used for communication with the NXT.
		/// </remarks>
		public void Dispose()
		{
			this.Dispose( true );

			// This object will be cleaned up by the Dispose method.
			// Take this object off the finalization queue and prevent finalization code 
			// for this object from executing a second time.
			GC.SuppressFinalize( this );
		}


		/// <summary>
		/// Finalizer.
		/// </summary>
		/// <remarks>
		/// Forwards the call to the <see cref="Dispose(bool)"/> method the clean up resources.
		/// </remarks>
		~Nxt()
		{
			this.Dispose( false );
		}


		#endregion


		#region Private fields

		/// <summary>
		/// The serial port used for Bluetooth communication.
		/// </summary>
		private SerialPort _port;

		/// <summary>
		/// Flag to help waiting for responses.
		/// </summary>
		private ManualResetEvent _mre = new ManualResetEvent( false );

		/// <summary>
		/// The last command sent to the NXT.
		/// </summary>
		private byte _lastCommandSent;

		#endregion


		#region Public properties

		/// <summary>
		/// The error code related to the last operation.
		/// </summary>
		public byte LastError { get; private set; }


		/// <summary>
		/// The detailed content of the last response message from the NXT.
		/// </summary>
		public byte[] LastResponse { get; private set; }

		#endregion


		#region Public infrastructure methods

		/// <summary>
		/// Connects the the specified port.
		/// </summary>
		/// <param name="portName">
		/// The name of the port for communication (eg. "COM8"), including but not limited to all available COM ports.
		/// </param>
		/// <exception cref="InvalidOperationException">
		/// If the port is already open. Please call <see cref="Disconnect"/> first.
		/// </exception>
		public void Connect( string portName )
		{
			if( this._port != null && this._port.IsOpen )
			{
				throw new InvalidOperationException( "Port is already open. Please call Disconnect first!" );
			}

			this._port = new SerialPort( portName );
			this._port.Open();
			this._port.DataReceived += new SerialDataReceivedEventHandler( this.OnDataReceived );
			this._port.ErrorReceived += new SerialErrorReceivedEventHandler( this.OnErrorReceived );
		}


		/// <summary>
		/// Disconnects from the connected port.
		/// </summary>
		public void Disconnect()
		{
			if( this._port != null && this._port.IsOpen )
			{
				this._port.Close();
				this._port.Dispose();
			}
		}

		#endregion


		#region Private infrastructure methods

		/// <summary>
		/// Initializes the header of the command to be sent to the NXT.
		/// Byte 0 will contain the command type and byte 1 the command code.
		/// </summary>
		/// <param name="command">The command to be sent to the NXT.</param>
		/// <param name="type">The type of the command.</param>
		/// <param name="dataLength">The length of the complete message in bytes.</param>
		/// <returns>
		/// A byte array with the length of the complete message,
		/// initialized with the command and command type.
		/// </returns>
		/// <exception cref="ArgumentOutOfRangeException">If the <paramref name="dataLength"/> is less than 2.</exception>
		private static byte[] InitializeData( Command command, CommandType type, int dataLength )
		{
			// Validate dataLength parameter: should be 2 or more.
			if( dataLength < 2 )
			{
				throw new ArgumentOutOfRangeException( "dataLength", "Data length cannot be less than 2." );
			}

			// Construct a package with the specified length
			byte[] data = new byte[ dataLength ];

			// Set byte 0 to the command type.
			data[ 0 ] = (byte) type;

			// Set byte 1 to the command code.
			data[ 1 ] = (byte) command;

			return data;
		}


		/// <summary>
		/// Callback method that is called by the runtime when an error occurs in the serial port communication.
		/// </summary>
		/// <param name="sender">Sender.</param>
		/// <param name="e">Event parameters.</param>
		/// <exception cref="IOException">This method throws <see cref="IOException"/> to signal the error to the host.</exception>
		/// <remarks>
		/// This method is responsible for managing errors in the serial port communication.
		/// This method will be called asynchronously by the .NET runtime on a separate thread
		/// and should never be called directly from any user code.
		/// </remarks>
		private void OnErrorReceived( object sender, SerialErrorReceivedEventArgs e )
		{
			throw new IOException( "Error in the serial port communication: " + e.EventType.ToString() );
		}


		/// <summary>
		/// Callback method that is called by the runtime when data is received on the serial port.
		/// </summary>
		/// <param name="sender">Sender.</param>
		/// <param name="e">Event parameters.</param>
		/// <exception cref="InvalidDataException">If the reponse message type, command or size is invalid.</exception>
		/// <remarks>
		/// This method is responsible for parsing the incoming messages from the NXT.
		/// This method will be called asynchronously by the .NET runtime on a separate thread
		/// and should never be called directly from any user code.
		/// </remarks>
		private void OnDataReceived( object sender, SerialDataReceivedEventArgs e )
		{
			// Clear the last error.
			this.LastError = 0;

			try
			{
				if( e.EventType == SerialData.Chars )
				{
					// Return immediately if there is no data to read.
					if( this._port.BytesToRead == 0 )
					{
						return;
					}

					// If the response is too short, empty the read buffer.
					if( this._port.BytesToRead < 4 )
					{
						this.LastError = (byte) Error.InsufficientMemoryAvailable;
						this.ClearReadBuffer();
					}
					else
					{
						// Parse the header that contains the message size.
						int lsb = this._port.ReadByte();
						int msb = this._port.ReadByte();
						int size = lsb + msb * 256;

						// Parse the rest of the message if the size is correct.
						if( size == this._port.BytesToRead )
						{
							// Read the response from the input buffer.
							this.LastResponse = new byte[ size ];
							this._port.Read( this.LastResponse, 0, size );

							// If the first byte in the reply is not 0x02 (reply command) - throw exception.
							if( this.LastResponse[ 0 ] != (byte) CommandType.Reply )
							{
								throw new InvalidDataException( "Unexpected return message type: " + this.LastResponse[ 0 ].ToString( CultureInfo.InvariantCulture ) );
							}
							// If the second byte in the reply does not match the command sent - throw exception.
							else if( this.LastResponse[ 1 ] != this._lastCommandSent )
							{
								throw new InvalidDataException( "Unexpected return command: " + this.LastResponse[ 1 ].ToString( CultureInfo.InvariantCulture ) );
							}

							// Save the status byte as the last error value.
							this.LastError = this.LastResponse[ 2 ];
						}
						else
						{
							throw new InvalidDataException( "Invalid message size: " + size.ToString( CultureInfo.InvariantCulture ) );
						}
					}
				}

				// Set the flag to indicate that the caller can parse the response.
				this._mre.Set();
			}
			catch( Exception ex )
			{
				throw new IOException( "Parsing the received data failed: " + ex.Message, ex );
			}
		}


		/// <summary>
		/// Transmits the specified data to the NXT.
		/// </summary>
		/// <param name="data">The data to be sent to the NXT.</param>
		private void Transmit( byte[] data )
		{
			this.Transmit( data, 0 );
		}


		/// <summary>
		/// Transmits the specified data to the NXT without blocking the current thread.
		/// </summary>
		/// <param name="data">The data to be sent to the NXT.</param>
		/// <param name="responseLength">Number of bytes the response should contain.</param>
		/// <exception cref="InvalidOperationException">If the port is not open or the previous transmit not completed.</exception>
		/// <exception cref="Exception">If a transmit error occurs.</exception>
		/// <exception cref="ObjectDisposedException">If the Nxt object is already disposed.</exception>
		/// <remarks>
		/// This method is responsible for transmitting messages to the NXT.
		/// The transmission is always asynchronous, responses are handled by the <see cref="OnDataReceived"/> event handler.
		/// This method performs object dispose check to throw <see cref="ObjectDisposedException"/> 
		/// if the current object (this) is already disposed.
		/// </remarks>
		private void Transmit( byte[] data, int responseLength )
		{
			// Abort if the object is already disposed.
			if( this._isDisposed )
			{
				throw new ObjectDisposedException( "Nxt", "Nxt object already disposed!" );
			}

			// Abort if the port is not open.
			if( !this._port.IsOpen )
			{
				throw new InvalidOperationException( "Port is not open!" );
			}

			// Abort if previous ouput transmit is not completed yet.
			if( this._port.BytesToWrite > 0 )
			{
				throw new InvalidOperationException( "Transmit timeout!" );
			}

			// Clear previously received data from the input buffer.
			if( this._port.BytesToRead > 0 )
			{
				this.ClearReadBuffer();
			}

			try
			{
				// Calculate the complete response length.
				if( responseLength > 0 )
				{
					this._port.ReceivedBytesThreshold = responseLength + 2;  // Standard 2 byte response with 0x02 and command code.
				}
				else
				{
					this._port.ReceivedBytesThreshold = 1;
				}

				// Construct and send the bluetooth header.
				byte[] header = new byte[ 2 ];
				byte len = (byte) ( data.Length & 0x3f ); // 64 byte packet
				header[ 0 ] = len;
				header[ 1 ] = 0;
				this._port.Write( header, 0, header.Length );

				// Send the command.
				this._port.Write( data, 0, len );

				// Save the current command to validate the response package.
				this._lastCommandSent = data[ 1 ];
			}
			catch( Exception ex )
			{
				throw new IOException( "Transmit error: " + ex.Message, ex );
			}
		}


		/// <summary>
		/// Transmits the message to the NXT, blocks the current thread for the specified milliseconds
		/// and validates the length and the status byte in the response package.
		/// </summary>
		/// <param name="data">The data to be sent to the NXT.</param>
		/// <param name="responseLength">Number of bytes the response should contain.</param>
		/// <remarks>
		/// After this method returns, the response is available and valid in the <see cref="LastResponse"/> property.
		/// </remarks>
		/// <exception cref="InvalidOperationException">
		/// If the status byte is not <see cref="Error.Success"/> or if the 
		/// length of the response differ from the expected response length specified in <paramref name="responseLength"/>.
		/// </exception>
		private void TransmitAndWait( byte[] data, int responseLength )
		{
			// Constant timeout value in milliseconds.
			const int millisecondsTimeout = 1500;

			// Reset the wait flag.
			this._mre.Reset();

			// Transmit the message.
			this.Transmit( data, responseLength );

			// Wait for the response or timeout.
			if( !this._mre.WaitOne( millisecondsTimeout, false ) )
			{
				throw new TimeoutException( "Timeout by waiting for the response!" );
			}

			// Validate the response length and status byte.
			if( this.LastError != (byte) Error.Success || this.LastResponse.Length != responseLength )
			{
				throw new InvalidOperationException( "Failed response: " + this.LastResponse.ToHexString() );
			}
		}


		/// <summary>
		/// Reads all data from the read buffer then throws them away.
		/// </summary>
		private void ClearReadBuffer()
		{
			// Number of bytes in the read buffer.
			int dataLength = this._port.BytesToRead;

			// If there is data in the read buffer, read them to a temp variable and dispose it.
			if( dataLength > 0 )
			{
				byte[] buffer = new byte[ dataLength ];
				this._port.Read( buffer, 0, dataLength );
			}
		}

		#endregion


		#region Public commands

		#region Generic commands

		/// <summary>
		/// Returns the current version of the firmware and the protocol used by the NXT.
		/// </summary>
		[SuppressMessage( "Microsoft.Design", "CA1024:UsePropertiesWhereAppropriate" )]
		public Version GetVersion()
		{
			byte[] data = Nxt.InitializeData( Command.GetFirmwareVersion, CommandType.SystemCommandWithResponse, 2 );

			this.TransmitAndWait( data, 7 ); // Return package: 0:0x02, 1:Command, 2:StatusByte, 3-4: protocol minor-major version, 5-6: firmware minor-major version.

			return new Version
			{
				Firmware = String.Format( CultureInfo.InvariantCulture, "{0}.{1}", this.LastResponse[ 6 ], this.LastResponse[ 5 ] ),
				Protocol = String.Format( CultureInfo.InvariantCulture, "{0}.{1}", this.LastResponse[ 4 ], this.LastResponse[ 3 ] )
			};
		}


		/// <summary>
		/// Sets the NXT brick name to the specified value.
		/// </summary>
		/// <param name="name">The new name of the brick to set. Maximum 14 characters.</param>
		/// <exception cref="ArgumentNullException">If the <paramref name="name"/> is null or empty.</exception>
		/// <exception cref="ArgumentOutOfRangeException">If the <paramref name="name"/> is longer than 14 characters.</exception>
		/// <remarks>
		/// This is a system command and waits for the response packet with blocking the current thread.
		/// </remarks>
		public void SetBrickName( string name )
		{
			// Validate input: name should be specified.
			if( String.IsNullOrEmpty( name ) )
			{
				throw new ArgumentNullException( "name", "The new brick name should be specified." );
			}

			// Validate input: name cannot be longer than 14 characters.
			if( name.Length > 14 )
			{
				throw new ArgumentOutOfRangeException( "name", "The new brick name cannot be longer than 14 characters." );
			}

			// Initialize command
			byte[] data = Nxt.InitializeData( Command.SetBrickName, CommandType.SystemCommandWithResponse, 18 );
			name.ToAsciiBytes().CopyTo( data, 2 );  // Byte 2-17: new name of the brick.

			this.TransmitAndWait( data, 3 ); // Return package: 0:0x02, 1:Command, 2:StatusByte

		}


		/// <summary>
		/// Returns general info about the NXT.
		/// </summary>
		/// <returns>A <see cref="DeviceInfo"/> that contains general parameters of the NXT.</returns>
		/// <remarks>
		/// This is a system command and waits for the response packet with blocking the current thread.
		/// </remarks>
		[SuppressMessage( "Microsoft.Design", "CA1024:UsePropertiesWhereAppropriate" )]
		public DeviceInfo GetDeviceInfo()
		{
			byte[] data = Nxt.InitializeData( Command.GetDeviceInfo, CommandType.SystemCommandWithResponse, 2 );

			this.TransmitAndWait( data, 33 ); // Return package: 0:0x02, 1:Command, 2:StatusByte, 3-17: name, 18-24 BT address, 25+28: signal strength, 29+32: free user flash.

			// Parse the response package.
			return new DeviceInfo
			{
				Name = this.LastResponse.ToAsciiString( 3, 15 ),             // Byte 3-17 : NXT name
				BluetoothAddress = this.LastResponse.Copy( 18, 6 ),          // Byte 18-24: Bluetooth address
				//SignalStrength = (UInt16) ( this.LastResponse[ 25 ] + 0x100 * this.LastResponse[ 28 ] ),  // Byte 25, 28: Bluetooth signal strength
				//FreeUserFlash = (UInt16) ( this.LastResponse[ 29 ] + 0x100 * this.LastResponse[ 32 ] )  // Byte 29, 32: Free user flash
				SignalStrength = this.LastResponse.ToUInt32( 25 ),
				FreeUserFlash = this.LastResponse.ToUInt32( 29 )
			};

		}


		/// <summary>
		/// Reads the battery level of the NXT.
		/// </summary>
		/// <returns>The battery level in millivolts.</returns>
		[SuppressMessage( "Microsoft.Design", "CA1024:UsePropertiesWhereAppropriate" )]
		public UInt16 GetBatteryLevel()
		{
			byte[] data = Nxt.InitializeData( Command.GetBatteryLevel, CommandType.DirectCommandWithResponse, 2 );

			this.TransmitAndWait( data, 5 ); // Return package: 0:0x02, 1:Command, 2:StatusByte, 3-4: Voltage in millivolts (UWORD)

			return this.LastResponse.ToUInt16( 3 );
		}


		/// <summary>
		/// Resets the sleep timer and returns the current sleep time limit.
		/// </summary>
		/// <remarks>
		/// This syscall method resets the NXT brick's internal sleep timer and returns the current time limit, 
		/// in milliseconds, until the next automatic sleep. Use this method to keep the NXT brick from automatically
		/// turning off. Use the NXT brick's UI menu to configure the sleep time limit.
		/// </remarks>
		/// <returns>The currently set sleep time limit in milliseconds.</returns>
		public UInt64 KeepAlive()
		{
			byte[] data = Nxt.InitializeData( Command.KeepAlive, CommandType.DirectCommandWithResponse, 2 );

			this.TransmitAndWait( data, 7 ); // Return package: 0:0x02, 1:Command, 2:StatusByte, 3-6: Current sleep time limit (ULONG)

			return this.LastResponse.ToUInt32( 3 );
		}

		#endregion

		#region Sound commands

		/// <summary>
		/// Plays a tone on the NXT with the specified frequency for the specified duration.
		/// </summary>
		/// <param name="frequency">Frequency for the tone in Hz. Range: 200-14000 Hz</param>
		/// <param name="duration">Duration of the tone in ms.</param>
		public void PlayTone( int frequency, int duration )
		{
			byte[] data = Nxt.InitializeData( Command.PlayTone, CommandType.DirectCommandWithoutResponse, 6 );
			data[ 2 ] = frequency.ToUpperByte();
			data[ 3 ] = frequency.ToLowerByte();
			data[ 4 ] = duration.ToUpperByte();
			data[ 5 ] = duration.ToLowerByte();

			this.Transmit( data ); //Return package: 0:0x02, 1:Command, 2:StatusByte
		}


		/// <summary>
		/// Plays the specified sound file on the NXT.
		/// </summary>
		/// <param name="fileName">
		/// The name of the file to play. ASCIIZ-string with maximum size 15.3 characters, the default extension is .rso.
		/// </param>
		/// <param name="loop"><c>True</c> to loop indefinitely or <c>false</c> to play the file only once.</param>
		public void PlaySoundFile( string fileName, bool loop )
		{
			byte[] data = Nxt.InitializeData( Command.PlaySoundFile, CommandType.DirectCommandWithoutResponse, 23 );
			data[ 2 ] = Convert.ToByte( loop );
			fileName.ToAsciiBytes().CopyTo( data, 3 );

			this.Transmit( data ); // Return package: 0:0x02, 1:Command, 2:StatusByte
		}


		/// <summary>
		/// Stops playing the current sound on the NXT.
		/// </summary>
		public void StopSoundPlayback()
		{
			byte[] data = Nxt.InitializeData( Command.StopSoundPlayback, CommandType.DirectCommandWithoutResponse, 2 );

			this.Transmit( data );  //Return package: 0:0x02, 1:Command, 2:StatusByte
		}

		#endregion

		#region Program commands

		/// <summary>
		/// Starts the specified program on the NXT.
		/// </summary>
		/// <param name="fileName">The name of the file to start. ASCIIZ-string with maximum size 15.3 characters, the default extension is .rxe .</param>
		public void StartProgram( string fileName )
		{
			byte[] data = Nxt.InitializeData( Command.StartProgram, CommandType.DirectCommandWithoutResponse, 22 );
			fileName.ToAsciiBytes().CopyTo( data, 2 );

			this.Transmit( data ); // Return package: 0:0x02, 1:Command, 2:StatusByte
		}


		/// <summary>
		/// Stops the currently running program on the NXT.
		/// </summary>
		public void StopProgram()
		{
			byte[] data = Nxt.InitializeData( Command.StopProgram, CommandType.DirectCommandWithoutResponse, 2 );

			this.Transmit( data ); // Return package: 0:0x02, 1:Command, 2:StatusByte
		}


		/// <summary>
		/// Returns the name of the program currently running on the NXT.
		/// </summary>
		/// <returns>
		/// The name of the file currently executing or <c>null</c> if no program is currently running. 
		/// Format: ASCIIZ-string with maximum 15.3 characters.
		/// </returns>
		[SuppressMessage( "Microsoft.Design", "CA1024:UsePropertiesWhereAppropriate" )]
		public string GetCurrentProgramName()
		{
			byte[] data = Nxt.InitializeData( Command.GetCurrentProgramName, CommandType.DirectCommandWithResponse, 2 );

			try
			{
				this.TransmitAndWait( data, 23 ); // Return package: 0:0x02, 1:Command, 2:StatusByte, 3-22 file name.
				return this.LastResponse.ToAsciiString( 3 );
			}
			catch
			{
				// Return null if there is no active program currently executing on the NXT, and forward any other errors.
				if( this.LastError == (byte) Error.NoActiveProgram )
				{
					return null;
				}
				else
				{
					throw;
				}
			}
		}

		#endregion

		#region Sensor commands

		/// <summary>
		/// Configures a sensor on the specified port.
		/// </summary>
		/// <param name="port">The port the sensor is connected to.</param>
		/// <param name="type">The type of the sensor connected to the port.</param>
		/// <param name="mode">
		/// The mode in which the sensor operates. The sensor mode affects the scaled value, 
		/// which the NXT firmware calculates depending on the sensor type and sensor mode.
		/// </param>
		public void SetInputMode( SensorPort port, SensorType type, SensorMode mode )
		{
			byte[] data = Nxt.InitializeData( Command.SetInputMode, CommandType.DirectCommandWithoutResponse, 5 );
			data[ 2 ] = (byte) port;
			data[ 3 ] = (byte) type;
			data[ 4 ] = (byte) mode;

			this.Transmit( data ); // Return package: 0:0x02, 1:Command, 2:StatusByte
		}


		/// <summary>
		/// Reads the current state of the sensor connected to the specified port.
		/// </summary>
		/// <param name="port">The port to which the sensor is connected.</param>
		/// <returns>A <see cref="SensorState"/> instance that describes the current state and value of the sensor.</returns>
		public SensorState GetInputValues( SensorPort port )
		{
			byte[] data = Nxt.InitializeData( Command.GetInputValues, CommandType.DirectCommandWithResponse, 3 );
			data[ 2 ] = (byte) port;

			this.TransmitAndWait( data, 16 );

			// Parse the response 
			return new SensorState
			{
				Valid = Convert.ToBoolean( this.LastResponse[ 4 ] ),
				Calibrated = Convert.ToBoolean( this.LastResponse[ 5 ] ),
				Type = (SensorType) this.LastResponse[ 6 ],
				Mode = (SensorMode) this.LastResponse[ 7 ],
				RawValue = this.LastResponse.ToUInt16( 8 ),
				NormalizedValue = this.LastResponse.ToUInt16( 10 ),
				ScaledValue = this.LastResponse.ToInt16( 12 ),
				CalibratedValue = this.LastResponse.ToInt16( 14 )
			};
		}


		/// <summary>
		/// Resets the value of the sensor connected to the specified port.
		/// </summary>
		/// <param name="port">The port to which the sensor is connected.</param>
		public void ResetInputScaledValue( SensorPort port )
		{
			byte[] data = Nxt.InitializeData( Command.ResetInputScaledValue, CommandType.DirectCommandWithoutResponse, 3 );
			data[ 2 ] = (byte) port;

			this.Transmit( data ); // Return package: 0:0x02, 1:Command, 2:StatusByte
		}

		#endregion

		#region Motor commands

		/// <summary>
		/// Resets the motor position on the specified motor port.
		/// </summary>
		/// <param name="motorPort">The motor to reset.</param>
		/// <param name="relative"><c>True</c>, if position relative to last movement, <c>false</c> for absolute position.</param>
		public void ResetMotorPosition( MotorPort motorPort, bool relative )
		{
			byte[] data = Nxt.InitializeData( Command.ResetMotorPosition, CommandType.DirectCommandWithoutResponse, 4 );
			data[ 2 ] = (byte) motorPort;
			data[ 3 ] = Convert.ToByte( relative );

			this.Transmit( data ); // Return package: 0:0x02, 1:Command, 2:StatusByte
		}


		/// <summary>
		/// Sends a command to the motor connected to the specified port.
		/// </summary>
		/// <param name="motorPort">The port where the motor is connected to.</param>
		/// <param name="power">
		/// Power (also referred as speed) set point. Range: -100-100.
		/// The absolute value of <paramref name="power"/> is used as a percentage of the full power capabilities of the motor.
		/// The sign of <paramref name="power"/> specifies rotation direction. 
		/// Positive values for <paramref name="power"/> instruct the firmware to turn the motor forward, 
		/// while negative values instruct the firmware to turn the motor backward. 
		/// "Forward" and "backward" are relative to a standard orientation for a particular type of motor.
		/// Note that direction is not a meaningful concept for outputs like lamps. 
		/// Lamps are affected only by the absolute value of <paramref name="power"/>.
		/// </param>
		/// <param name="mode">Motor mode (bit-field).</param>
		/// <param name="regulation">Regulation mode.</param>
		/// <param name="turnRatio">
		/// This property specifies the proportional turning ratio for synchronized turning using two motors. Range: -100-100.
		/// <remarks>
		/// Negative <paramref name="turnRatio"/> values shift power towards the left motor, 
		/// whereas positive <paramref name="turnRatio"/> values shift power towards the right motor. 
		/// In both cases, the actual power applied is proportional to the <paramref name="power"/> set-point, 
		/// such that an absolute value of 50% for <paramref name="turnRatio"/> normally results in one motor stopping,
		/// and an absolute value of 100% for <paramref name="turnRatio"/> normally results in the two motors 
		/// turning in opposite directions at equal power.
		/// </remarks>
		/// </param>
		/// <param name="runState">Motor run state.</param>
		/// <param name="tachoLimit">
		/// Tacho limit. This property specifies the rotational distance in 
		/// degrees that you want to turn the motor. Range: 0-4294967295, O: run forever.
		/// The sign of the <paramref name="power"/> property specifies the direction of rotation.
		/// </param>
		[SuppressMessage( "Microsoft.Naming", "CA1704:IdentifiersShouldBeSpelledCorrectly", MessageId = "tacho" )]
		public void SetOutputState( MotorPort motorPort, sbyte power, MotorModes mode, MotorRegulationMode regulation,
			sbyte turnRatio, MotorRunState runState, UInt32 tachoLimit )
		{
			byte[] data = Nxt.InitializeData( Command.SetOutputState, CommandType.DirectCommandWithoutResponse, 13 );
			data[ 2 ] = (byte) motorPort;
			data[ 3 ] = (byte) power;
			data[ 4 ] = (byte) mode;
			data[ 5 ] = (byte) regulation;
			data[ 6 ] = (byte) turnRatio;
			data[ 7 ] = (byte) runState;
			data[ 8 ] = (byte) ( tachoLimit & 0xFF );   // Byte 8-12: tacho limit ULONG
			data[ 9 ] = (byte) ( tachoLimit >> 8 );
			data[ 10 ] = (byte) ( tachoLimit >> 16 );
			data[ 11 ] = (byte) ( tachoLimit >> 24 );

			this.Transmit( data ); // Return package: 0:0x02, 1:Command, 2:StatusByte
		}


		/// <summary>
		/// Returns the current state of the specified motor.
		/// </summary>
		/// <param name="motorPort">The port where the motor is connected.</param>
		/// <returns>The current state of the specified motor.</returns>
		public MotorState GetOutputState( MotorPort motorPort )
		{
			byte[] data = Nxt.InitializeData( Command.GetOutputState, CommandType.DirectCommandWithResponse, 3 );
			data[ 2 ] = (byte) motorPort;

			this.TransmitAndWait( data, 25 );

			// Parse the response.
			return new MotorState
			{
				Power = (sbyte) this.LastResponse[ 4 ],
				Mode = (MotorModes) this.LastResponse[ 5 ],
				Regulation = (MotorRegulationMode) this.LastResponse[ 6 ],
				TurnRatio = (sbyte) this.LastResponse[ 7 ],
				RunState = (MotorRunState) this.LastResponse[ 8 ],
				TachoLimit = this.LastResponse.ToUInt32( 9 ),
				TachoCount = this.LastResponse.ToInt32( 13 ),
				BlockTachoCount = this.LastResponse.ToInt32( 17 ),
				RotationCount = this.LastResponse.ToInt32( 21 )
			};

		}


		#endregion

		#region Low speed (I2C) commands

		/// <summary>
		/// Returns the number of available bytes to read on the specified port.
		/// </summary>
		/// <param name="port">The sensor port to read.</param>
		/// <returns>The number of available bytes to read.</returns>
		public int LowSpeedGetStatus( SensorPort port )
		{
			byte[] data = Nxt.InitializeData( Command.LowSpeedGetStatus, CommandType.DirectCommandWithResponse, 4 );
			data[ 2 ] = (byte) port;

			this.TransmitAndWait( data, 4 ); // Return package: 0:0x02, 1:Command, 2:StatusByte, 3: bytes ready (count of available bytes to read).

			// Parse the return package.
			// Byte 3 contains the available bytes to read.
			return this.LastResponse[ 3 ];
		}


		/// <summary>
		/// Performs a low speed read on the specified sensor port.
		/// </summary>
		/// <param name="port">The sensor port to read.</param>
		/// <returns>The received data, maximum 16 bytes.</returns>
		/// <remarks>
		/// For low speed communication on the NXT, data lengths are limited to 16 bytes per command.
		/// Furthermore, this protocol does not support variable-length return packages, so the
		/// response will always contain 16 data bytes, with invalid data bytes padded with zeroes.
		/// </remarks>
		public byte[] LowSpeedRead( SensorPort port )
		{
			byte[] data = Nxt.InitializeData( Command.LowSpeedRead, CommandType.DirectCommandWithResponse, 3 );
			data[ 2 ] = (byte) port;

			this.TransmitAndWait( data, 20 ); // Return package: 0:0x02, 1:Command, 2:StatusByte, 3: bytes read, 4-19: received data (padded).

			// Parse the response.
			// Byte 3 contains the length of the received payload.
			int responseDataLength = this.LastResponse[ 3 ];

			// Copy the payload from the complete response package.
			byte[] responseData = this.LastResponse.Copy( 4, responseDataLength );

			return responseData;
		}


		/// <summary>
		/// Writes the specified message to the specified sensor port.
		/// </summary>
		/// <param name="port">The sensor port to write to.</param>
		/// <param name="message">The message to write to the sensor port.</param>
		/// <param name="responseLength">The length of the expected response message.</param>
		/// <remarks>
		/// For low speed communication on the NXT, data lengths are limites to 16 bytes per command.
		/// Response data length must be specified in the write command since reading from the 
		/// device is done on a master-slave basis.
		/// </remarks>
		/// <exception cref="ArgumentOutOfRangeException">
		/// If <paramref name="message"/> length is not between 1 and 16 bytes.
		/// </exception>
		public void LowSpeedWrite( SensorPort port, byte[] message, int responseLength )
		{
			// Validate input: message to transmit cannot be longer than 16 bytes.
			if( message.Length < 1 || message.Length > 16 )
			{
				throw new ArgumentOutOfRangeException( "message", "Message length must be between 1 and 16 bytes." );
			}

			byte[] data = Nxt.InitializeData( Command.LowSpeedWrite, CommandType.DirectCommandWithoutResponse, 5 + message.Length );
			data[ 2 ] = (byte) port;
			data[ 3 ] = (byte) message.Length;                  // Byte 3: Transmit data length
			data[ 4 ] = (byte) responseLength;                  // Byte 4: Response data length
			Array.Copy( message, 0, data, 5, message.Length ); // Byte 5-n: Transmit data.

			this.Transmit( data );
		}


		#endregion

		#region Message commands

		/// <summary>
		/// Writes the specified <paramref name="message"/> to the specified <paramref name="inboxNumber"/>.
		/// </summary>
		/// <param name="inboxNumber">The inbox number (0-9).</param>
		/// <param name="message">The message to write. Cannot be longer than 58 bytes to be legal on USB.</param>
		/// <exception cref="ArgumentOutOfRangeException">
		/// If <paramref name="inboxNumber"/> is not between 0 and 9 or <paramref name="message"/> is longer than 58 bytes.
		/// </exception>
		public void MessageWrite( byte inboxNumber, byte[] message )
		{
			// Validate input: inbox number should be between 0 and 9.
			if( inboxNumber < 0 || inboxNumber > 9 )
			{
				throw new ArgumentOutOfRangeException( "inboxNumber", "Inbox number should be between 0 and 9." );
			}

			// Validate input: message length cannot exceed 58 bytes.
			if( message.Length > 58 )
			{
				throw new ArgumentOutOfRangeException( "message", "Message cannot be longer than 58 bytes." );
			}

			// Construct message.
			byte[] data = Nxt.InitializeData( Command.MessageWrite, CommandType.DirectCommandWithoutResponse, message.Length + 5 );
			data[ 2 ] = inboxNumber;                            // Byte 2: inbox number.
			data[ 3 ] = (byte) ( message.Length + 1 );          // Byte 3: message length including the null termination byte.
			Array.Copy( message, 0, data, 4, message.Length );  // Byte 4-n: message.
			data[ message.Length + 4 ] = 0;                     // Last byte: null termination.

			this.Transmit( data );  // Return package: 0:0x02, 1:Command, 2:StatusByte
		}


		/// <summary>
		/// Reads a message from the specified local inbox.
		/// </summary>
		/// <param name="remoteInboxNumber">Remote inbox number (0-19). </param>
		/// <param name="localInboxNumber">Local inbox number (0-9).</param>
		/// <param name="removeFromRemoteInbox"><c>True</c> to clear the message from the remote inbox, <c>false</c> otherwise.</param>
		/// <returns>The incoming message including null termination byte.</returns>
		/// <remarks>
		/// Remote inbox number may specify a value of 0-19, while all other mailbox numbers should remain below 9.
		/// This is due to the master-slave relationship between the connected NXT bricks.
		/// Slave devices may not initiate communication transactions with their masters,
		/// so they store outgoing messages in the upper 10 mailboxes (indices 10-19).
		/// Use the MessageRead command from the master device to retrieve these messages.
		/// </remarks>
		/// <exception cref="ArgumentOutOfRangeException">
		/// If <paramref name="remoteInboxNumber"/> not between 0 and 19 or 
		/// <paramref name="localInboxNumber"/> not between 0 and 9.
		/// </exception>
		public byte[] MessageRead( byte remoteInboxNumber, byte localInboxNumber, bool removeFromRemoteInbox )
		{
			// Validate input: remote inbox number should be between 0 and 19.
			if( remoteInboxNumber < 0 || remoteInboxNumber > 19 )
			{
				throw new ArgumentOutOfRangeException( "remoteInboxNumber", "Remote inbox number should be between 0 and 19." );
			}

			// Validate input: local inbox number should be between 0 and 9.
			if( localInboxNumber < 0 || localInboxNumber > 9 )
			{
				throw new ArgumentOutOfRangeException( "localInboxNumber", "Local inbox number should be between 0 and 9." );
			}

			byte[] data = Nxt.InitializeData( Command.MessageRead, CommandType.DirectCommandWithResponse, 5 );
			data[ 2 ] = remoteInboxNumber;
			data[ 3 ] = localInboxNumber;
			data[ 4 ] = Convert.ToByte( removeFromRemoteInbox );

			this.TransmitAndWait( data, 64 ); // Return package: 0:0x02, 1:Command, 2:StatusByte, 3:local inbox number, 4:message size, 5-63:message.

			// Parse the response package.
			// Byte 4: message size including null termination byte.
			int messageLength = this.LastResponse[ 4 ];

			// Copy the response message from the complete response.
			byte[] message = this.LastResponse.Copy( 5, messageLength );

			return message;
		}


		#endregion

		#endregion
	}
}
