// F# Function Examples

module MyFunctions  // used in files to be able to load

open System // bring in Console

// Create a function that will add 1 to a given number:

let add1 x = x + 1

// Create a function that will add two numbers together:

let addem x y = x + y

// Call both of the functions, and display the results:

Console.WriteLine(add1 4)
Console.WriteLine(addem 42 8 )
