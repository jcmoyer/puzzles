package body Md5.Hex_Strings with
  SPARK_Mode
is
   function Hex (Byte : Unsigned_8) return Hex_String_U8 is
      Hi : constant Unsigned_8 := Shift_Right (Byte, 4);
      Lo : constant Unsigned_8 := 16#0F# and Byte;
   begin
      return Hex_Digits (Hex_Digits'First + Integer (Hi)) & Hex_Digits (Hex_Digits'First + Integer (Lo));
   end Hex;

   function Hex (Word : Unsigned_32) return Hex_String_U32 is
      X1 : constant Unsigned_8 := Unsigned_8 (16#FF# and Word);
      X2 : constant Unsigned_8 := Unsigned_8 (16#FF# and Shift_Right (Word, 8));
      X3 : constant Unsigned_8 := Unsigned_8 (16#FF# and Shift_Right (Word, 16));
      X4 : constant Unsigned_8 := Unsigned_8 (16#FF# and Shift_Right (Word, 24));
   begin
      return Hex (X1) & Hex (X2) & Hex (X3) & Hex (X4);
   end Hex;
end Md5.Hex_Strings;
