with Advent.Parsers.Integers;
with Advent.Containers.Integer_Vectors;

package Advent.Integer_Parsers is new Advent.Parsers.Integers (
   Element_Type => Integer,
   Element_Vectors => Advent.Containers.Integer_Vectors
);
