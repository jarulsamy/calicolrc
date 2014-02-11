/*
 * DynamicJava - Copyright (C) 1999-2001
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files
 * (the "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to permit
 * persons to whom the Software is furnished to do so, subject to the
 * following conditions:
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL DYADE BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 * Except as contained in this notice, the name of Dyade shall not be
 * used in advertising or otherwise to promote the sale, use or other
 * dealings in this Software without prior written authorization from
 * Dyade.
 *
 */

package koala.dynamicjava.interpreter;

import java.io.*;
import java.lang.reflect.*;
import java.net.*;
import java.util.*;

import koala.dynamicjava.parser.wrapper.*;
import koala.dynamicjava.util.*;

/**
 * The classes that implements this interface represent the
 * objects that holds the objects needed for interpretation.
 *
 * @author  Stephane Hillion
 * @version 1.3 - 1999/11/21
 */

public interface Interpreter {
    /**
     * Runs the interpreter
     * @param is    the input stream from which the statements are read
     * @param fname the name of the parsed stream
     * @return the result of the evaluation of the last statement
     */
    Object interpret(InputStream is, String fname) throws InterpreterException;
    
    /**
     * Runs the interpreter
     * @param r     the reader
     * @param fname the name of the parsed stream
     * @return the result of the evaluation of the last statement
     */
    Object interpret(Reader r, String fname) throws InterpreterException;
    
    /**
     * Runs the interpreter
     * @param fname the name of a file to interpret
     * @return the result of the evaluation of the last statement
     */
    Object interpretFile(String fname) throws InterpreterException, IOException;
    
    /**
     * Defines a variable in the interpreter environment
     * @param name  the variable's name
     * @param value the initial value of the variable
     * @exception IllegalStateException if name is already defined
     */
    public void defineVariable(String name, Object value);

    /**
     * Sets the value a variable
     * @param name  the variable's name
     * @param value the value of the variable
     * @exception IllegalStateException if the assignment is invalid
     */
    public void setVariable(String name, Object value);

    /**
     * Gets the value of a variable
     * @param name  the variable's name
     * @exception IllegalStateException if the variable do not exist
     */
    public Object getVariable(String name);

    /**
     * Gets the class of a variable
     * @param name  the variable's name
     * @exception IllegalStateException if the variable do not exist
     */
    public Class getVariableClass(String name);

    /**
     * Returns the defined variable names
     * @return a set of strings
     */
    public Set getVariableNames();

    /**
     * Returns the defined class names
     * @return a set of strings
     */
    public Set getClassNames();

    /**
     * Set the interpreter contexts to override public/protected/private
     * access restrictions on the methods and fields it handles.  Default
     * should be false, i.e. enforce Java access.  Setting to true should
     * override this and allow access to all fields.
     */
    public void setAccessible(boolean accessible);
 
    /**
     * Observe the state of calls to setAccessible()
     */
    public boolean getAccessible();

    /**
     * Adds a class search path
     * @param path the path to add
     */
    public void addClassPath(String path);

    /**
     * Adds a class search URL
     * @param url the url to add
     */
    public void addClassURL(URL url);

    /**
     * Adds a library search path
     * @param path the path to add
     */
    public void addLibraryPath(String path);

    /**
     * Adds a library file suffix
     * @param s the suffix to add
     */
    public void addLibrarySuffix(String s);

    /**
     * Loads an interpreted class
     * @param s the fully qualified name of the class to load
     * @exception ClassNotFoundException if the class cannot be find
     */
    public Class loadClass(String name) throws ClassNotFoundException;

    /**
     * Converts an array of bytes into an instance of class Class
     * @exception ClassFormatError if the class could not be defined
     */
    public Class defineClass(String name, byte[] code);

    /**
     * Gets the class loader
     */
    public ClassLoader getClassLoader();

    /**
     * Gets the library finder
     */
    public LibraryFinder getLibraryFinder();

    /**
     * Gets the parser factory
     */
    public ParserFactory getParserFactory();
}
