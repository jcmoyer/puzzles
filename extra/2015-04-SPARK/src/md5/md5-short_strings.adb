package body Md5.Short_Strings with
  SPARK_Mode
is
   procedure Assign (Target : in out Short_String; Source : String) is
   begin
      for I in 1 .. Source'Length loop
         Target.Buf (I) := Source (Source'First - 1 + I);
      end loop;
      Target.Len := Source'Length;
   end Assign;

   function Num_Digits (I : Unsigned_32) return Integer is
   begin
      --  Assumptions:
      --
      --  1. We will spend most of our time iterating through large numbers, so larger powers of 10 should be checked
      --  first.
      --
      --  2. Solutions for part 2 seem to be in the millions, so optimize for that case.
      if I >= 1_000_000 then
         if I >= 10_000_000 then
            if I >= 100_000_000 then
               if I >= 1_000_000_000 then
                  return 10;
               end if;
               return 9;
            end if;
            return 8;
         end if;
         return 7;
      end if;
      if I >= 100_000 then
         return 6;
      end if;
      if I >= 10_000 then
         return 5;
      end if;
      if I >= 1_000 then
         return 4;
      end if;
      if I >= 100 then
         return 3;
      end if;
      if I >= 10 then
         return 2;
      end if;
      return 1;
   end Num_Digits;

   procedure Overwrite_Digits (Target : in out Short_String; From : Short_String_Index; Number : Unsigned_32) is
      Nd   : constant Integer            := Num_Digits (Number);
      Dest : constant Short_String_Index := From + Nd - 1;
      N    : Unsigned_32                 := Number;
   begin
      Target.Len := Short_String_Index'Max (Target.Len, Dest);
      for I in 1 .. Nd loop
         Target.Buf (Dest - I + 1) := Character'Val (Character'Pos ('0') + (N rem 10));
         N                         := N / 10;
      end loop;
   end Overwrite_Digits;

end Md5.Short_Strings;
