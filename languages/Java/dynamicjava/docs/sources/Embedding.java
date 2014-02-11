import java.io.*;
import javax.swing.JFrame;

import koala.dynamicjava.interpreter.*;
import koala.dynamicjava.parser.wrapper.*;

public class Embedding {
    public static void main(String[] args) {
	JFrame frame = new JFrame();

	// Create the interpreter. It will use the default JavaCC parser.
	Interpreter interpreter = new TreeInterpreter(new JavaCCParserFactory());

	// Export the JFrame to the interpreter.
	interpreter.defineVariable("frame", frame);
	
	// Interpret the script
	try {
	    interpreter.interpret("init.djava");
	} catch (InterpreterException e) {
	    System.err.println(e);
	} catch (Throwable e) {
	    System.err.println(e);
	}

	// Show the frame
	frame.show();
    }
}
