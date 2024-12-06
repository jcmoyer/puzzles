with Advent.Parsers.Integers;
with Advent.Containers.Long_Vectors;

package Advent.Long_Parsers is new Advent.Parsers.Integers (
   Element_Type => Long_Long_Integer,
   Element_Vectors => Advent.Containers.Long_Vectors
);
