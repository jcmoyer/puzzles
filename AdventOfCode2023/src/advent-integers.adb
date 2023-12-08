package body Advent.Integers is

   function Gcd (A, B : Element_Type) return Element_Type is
      M : Element_Type := A;
      N : Element_Type := B;
      T : Element_Type;
   begin
      while N /= 0 loop
         T := M;
         M := N;
         N := T rem N;
      end loop;
      return M;
   end Gcd;

   function Lcm (A, B : Element_Type) return Element_Type is
   begin
      if A = 0 or else B = 0 then
         return 0;
      end if;
      return abs (A) * (abs (B) / Gcd (A, B));
   end Lcm;

   function Lcm (Xs : Vector) return Element_Type is
      R : Element_Type := Xs (1);
   begin
      if Xs.Length = 1 then
         return R;
      end if;

      for I in Xs.First_Index + 1 .. Xs.Last_Index loop
         R := Lcm (R, Xs (I));
      end loop;

      return R;
   end Lcm;

end Advent.Integers;
