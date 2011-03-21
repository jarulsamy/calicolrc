// C# Function Examples

using System; // to get Action

// Create a function that will add 1 to a given number
// and display the result:

Action<int> add1 = t => Console.WriteLine(t + 1);

// Create a function that will add two numbers together
// and display the result:

Action<int,int> addem = (x,y) => Console.WriteLine(x + y);

// Call both of the functions:

add1(4);
addem(42, 8);
