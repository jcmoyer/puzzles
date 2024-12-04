with Advent.Parsers.Scanners; use Advent.Parsers.Scanners;
with Advent.Testing; use Advent.Testing;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;

procedure Test_Scanner is

   Test_String : aliased constant String := "hello123,456mul(777,818)";

   S : Scanner;
   P : String_Span;

begin

   --  TODO: This is "bad" but there's not a good way to avoid copying large
   --  strings aside from wrapping them in some kind of ref counted class... We
   --  can either eat the initial copy cost or keep doing unsafe things. (Keep
   --  in mind that String is indefinite so we cannot reasonably store one
   --  in the scanner. It will always need to be through a pointer or
   --  pointer-like abstraction.)
   S.Source   := Test_String'Unchecked_Access;
   S.Position := 1;

   Assert (S.Position = 1);
   Read_While (S, Lower_Set, P);
   Assert (S.Position = 6);
   Assert (P.First = 1);
   Assert (P.Last = 5);

   Read_While_Not (S, To_Set (','), P);
   Assert (S.Position = 9);
   Assert (P.First = 6);
   Assert (P.Last = 8);

   Read_Until (S, "mul(", P);
   Assert (S.Position = 13);
   Assert (P.First = 13);
   Assert (P.Last = 16);

   --  TODO: Read_Past
   S.Position := P.Last + 1;

   Read_While (S, Decimal_Digit_Set, P);
   Assert (Integer'Value (Test_String (P.First .. P.Last)) = 777);

end Test_Scanner;
