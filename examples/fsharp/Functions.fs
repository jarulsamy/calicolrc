// F# Function Examples

module MyFunctions  // used in files to be able to load

open System; // bring in Console

// Create a function that will add 1 to a given number:

let add1 x = x + 1;

// Create a function that will add two numbers together:

let addem x y = x + y;

// Call both of the functions, and display the results:

Console.WriteLine(add1 4);
Console.WriteLine(addem 42 8 );

//let odd even n = even(odd n+1);;

//let even odd n = odd(even (n+1));;

let rec Even x =
   if x = 0 then true
   else Odd (x - 1)
and Odd x =
   if x = 1 then true
   else Even (x - 1);

Console.WriteLine(Odd(1));
