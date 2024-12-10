with Ada.Containers.Indefinite_Vectors;

package Advent.Containers.String_Vectors is new Ada.Containers.Indefinite_Vectors
  (Index_Type => Natural, Element_Type => String);

pragma Preelaborate (Advent.Containers.String_Vectors);
