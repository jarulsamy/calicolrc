
using System;
using System.Threading;

public class Test {

    public static void Main(string [] args) {
	
	Console.CancelKeyPress += (object sender, ConsoleCancelEventArgs cargs) => {
	    cargs.Cancel = true;
	    Console.WriteLine("\ncontrol+c!");
	};
	
	while (true) {
	    Thread.Sleep(1000);
	    Console.Write(".");
	}
    }
}
