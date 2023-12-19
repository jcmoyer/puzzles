with Advent;                  use Advent;
with Ada.Containers.Vectors;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;

procedure Day03 is
   Map : constant Char_Matrix := Read_Tilemap (Ada.Command_Line.Argument (1));
   J   : Integer;

   N_Start : Integer;
   Sum1    : Integer := 0;
   Sum2    : Integer := 0;

   type Grid_Number is record
      Value : Integer;
      Rect  : Rectangle;
   end record;

   package Grid_Number_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Grid_Number);
   subtype Grid_Number_Vector is Grid_Number_Vectors.Vector;

   Grid_Numbers : Grid_Number_Vector;

   type Grid_Symbol is record
      Value    : Character;
      Location : Point;
   end record;

   package Grid_Symbol_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Grid_Symbol);
   subtype Grid_Symbol_Vector is Grid_Symbol_Vectors.Vector;

   Grid_Symbols : Grid_Symbol_Vector;
   Value        : Integer := 0;

begin
   for I in 1 .. Rows (Map) loop
      J := 1;
      while J <= Cols (Map) loop
         if Is_Digit (Map (I, J)) then
            N_Start := J;
            Value   := 0;
            while J <= Cols (Map) and then Is_Digit (Map (I, J)) loop
               Value := Value * 10 + Integer'Value ((1 => Map (I, J)));
               J     := J + 1;
            end loop;

            declare
               Rect : Rectangle := (Left => N_Start, Right => J - 1, Top => I, Bottom => I);
            begin
               Inflate (Rect, 1, 1);
               Grid_Numbers.Append ((Value => Value, Rect => Rect));
            end;
         else
            if Map (I, J) /= '.' then
               Grid_Symbols.Append ((Value => Map (I, J), Location => (Y => I, X => J)));
            end if;
            J := J + 1;
         end if;
      end loop;
   end loop;

   for Sym of Grid_Symbols loop
      declare
         N_Gear  : Integer := 0;
         Product : Integer := 1;
      begin
         for Num of Grid_Numbers loop
            if Contains (Num.Rect, Sym.Location) then
               Sum1 := Sum1 + Num.Value;
               if Sym.Value = '*' then
                  N_Gear  := N_Gear + 1;
                  Product := Product * Num.Value;
               end if;
            end if;
         end loop;
         if N_Gear = 2 then
            Sum2 := Sum2 + Product;
         end if;
      end;
   end loop;

   Solution (Sum1);
   Solution (Sum2);
end Day03;
