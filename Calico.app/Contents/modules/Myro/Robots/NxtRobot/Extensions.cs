using System;

using System.Text;





namespace NxtNet

{

	/// <summary>

	/// This class contains extension methods to make type conversion easier.

	/// </summary>

	public static class Extensions

	{

		/// <summary>

		/// TODO

		/// </summary>

		/// <param name="value"></param>

		/// <returns></returns>

		public static byte ToUpperByte( this int value )

		{

			return (byte) ( value & 0xff );

		}



		/// <summary>

		/// TODO

		/// </summary>

		/// <param name="value"></param>

		/// <returns></returns>

		public static byte ToLowerByte( this int value )

		{

			return (byte) ( value >> 8 );

		}





		/// <summary>

		/// Returns bytes from the current byte array from the specified <paramref name="sourceStartIndex"/> in the specified <paramref name="length"/>.

		/// </summary>

		/// <param name="sourceBuffer">The current byte array that contains the bytes to copy.</param>

		/// <param name="sourceStartIndex">The index of the first byte in the current byte array that should be copied.</param>

		/// <param name="length">The number of bytes that should be copied.</param>

		/// <returns>A sub-array from the current byte array with the specified <paramref name="length"/>.</returns>

		public static byte[] Copy( this byte[] sourceBuffer, int sourceStartIndex, int length )

		{

			// Construct a temp byte array and copy bytes from the sourceBuffer to the specified length. 

			byte[] temp = new byte[ length ];

			Array.Copy( sourceBuffer, sourceStartIndex, temp, 0, length );



			return temp;

		}





		/// <summary>

		/// Converts four bytes from the specified buffer starting from the specified index to uint (32-bit unsigned integer).

		/// </summary>

		/// <param name="buffer">The byte array that contains the values to convert.</param>

		/// <param name="startIndex">The index from where the conversion should start.</param>

		/// <returns>The value of four bytes converted to uint (32-bit unsigned integer).</returns>

		public static UInt32 ToUInt32( this byte[] buffer, int startIndex )

		{

			UInt32 result = 0;



			for( int i = 0; i < 4; i++ )

			{

				result += (UInt32) ( 1 << i * 8 ) * (UInt32) buffer[ startIndex++ ];

			}



			return result;

		}





		/// <summary>

		/// Converts four bytes from the specified buffer starting from the specified index to int (32-bit integer).

		/// </summary>

		/// <param name="buffer">The byte array that contains the values to convert.</param>

		/// <param name="startIndex">The index from where the conversion should start.</param>

		/// <returns>The value of four bytes converted to int (32-bit integer).</returns>

		public static Int32 ToInt32( this byte[] buffer, int startIndex )

		{

			return (Int32) buffer.ToUInt32( startIndex );

		}





		/// <summary>

		/// Converts two bytes from the specified buffer starting from the specified index to ushort (16-bit unsigned integer).

		/// </summary>

		/// <param name="buffer">The byte array that contains the values to convert.</param>

		/// <param name="startIndex">The index from where the conversion should start.</param>

		/// <returns>The value of two bytes converted to ushort (16-bit unsigned integer).</returns>

		/// <exception cref="ArgumentOutOfRangeException">

		/// If the <paramref name="startIndex"/> is greater than <paramref name="buffer"/>.Length - 1.

		/// </exception>

		public static UInt16 ToUInt16( this byte[] buffer, int startIndex )

		{

			if( startIndex > buffer.Length - 1 )

			{

				throw new ArgumentOutOfRangeException( "startIndex", "The startIndex cannot be greater than buffer.Length - 1." );

			}



			return (UInt16) ( buffer[ startIndex ] + 0x100 * buffer[ startIndex + 1 ] );

		}





		/// <summary>

		/// Converts two bytes from the specified buffer starting from the specified index to short (16-bit integer).

		/// </summary>

		/// <param name="buffer">The byte array that contains the values to convert.</param>

		/// <param name="startIndex">The index from where the conversion should start.</param>

		/// <returns>The value of two bytes converted to short (16-bit integer).</returns>

		public static Int16 ToInt16( this byte[] buffer, int startIndex )

		{

			return (Int16) buffer.ToUInt16( startIndex );

		}





		/// <summary>

		/// Converts the specified string to a byte array using ASCII encoding.

		/// </summary>

		/// <param name="value">The string to convert.</param>

		/// <returns>The string converted to a byte array using ASCII encoding.</returns>

		public static byte[] ToAsciiBytes( this string value )

		{

			return Encoding.ASCII.GetBytes( value );

		}





		/// <summary>

		/// Converts bytes starting from the specified start index in the buffer to an ASCII encoded string.

		/// </summary>

		/// <param name="buffer">The buffer that contains the values to convert.</param>

		/// <param name="startIndex">The index from where the conversion should start.</param>

		/// <returns>The value of the buffer converted to string using ASCII encoding.</returns>

		public static string ToAsciiString( this byte[] buffer, int startIndex )

		{

			// Converts the byte array to string using ASCII encoding and removes all trailing zeroes.

			return Encoding.ASCII.GetString( buffer, startIndex, buffer.Length - startIndex ).TrimEnd( '\0' );

		}





		/// <summary>

		/// Converts bytes starting from the specified start index in the buffer to an ASCII encoded string.

		/// </summary>

		/// <param name="buffer">The buffer that contains the values to convert.</param>

		/// <param name="startIndex">The index from where the conversion should start.</param>

		/// <param name="length">The number of bytes in the buffer that contains the string to convert.</param>

		/// <returns>The value of the buffer converted to string.</returns>

		public static string ToAsciiString( this byte[] buffer, int startIndex, int length )

		{

			// Construct a temp array.

			byte[] data = new byte[ length ];

			Array.Copy( buffer, startIndex, data, 0, length );



			return data.ToAsciiString( 0 );

		}





		/// <summary>

		/// Converts the byte array to a readable string with hexadecimal values.

		/// </summary>

		/// <param name="value">The byte array to convert.</param>

		/// <returns>A human readable format of the specified byte array.</returns>

		public static string ToHexString( this byte[] value )

		{

			string result = BitConverter.ToString( value );

			return result;

		}



	}

}

