with Ada.Containers.Vectors;

package Advent.Containers.Long_Vectors is new Ada.Containers.Vectors
  (Index_Type => Positive, Element_Type => Long_Long_Integer);
