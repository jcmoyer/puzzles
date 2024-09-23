with Md5.Hex_Strings;
with Md5.Short_Strings;

package body Md5.Hashing with
  SPARK_Mode
is
   S11 : constant := 7;
   S12 : constant := 12;
   S13 : constant := 17;
   S14 : constant := 22;

   S21 : constant := 5;
   S22 : constant := 9;
   S23 : constant := 14;
   S24 : constant := 20;

   S31 : constant := 4;
   S32 : constant := 11;
   S33 : constant := 16;
   S34 : constant := 23;

   S41 : constant := 6;
   S42 : constant := 10;
   S43 : constant := 15;
   S44 : constant := 21;

   function T_F (X, Y, Z : U32) return U32 is
   begin
      return (X and Y) or ((not X) and Z);
      --  discovered via program synthesis: return ((Y xor Z) and X) xor Z;
   end T_F;
   pragma Inline (T_F);

   function T_FF (A, B, C, D, X, S, Ac : U32) return U32 is
      Val : constant U32 := A + T_F (B, C, D) + X + Ac;
   begin
      return Rotate_Left (Val, Natural (S)) + B;
   end T_FF;
   pragma Inline (T_FF);

   function T_G (X, Y, Z : U32) return U32 is
   begin
      return (X and Z) or (Y and (not Z));
   end T_G;
   pragma Inline (T_G);

   function T_GG (A, B, C, D, X, S, Ac : U32) return U32 is
      Val : constant U32 := A + T_G (B, C, D) + X + Ac;
   begin
      return Rotate_Left (Val, Natural (S)) + B;
   end T_GG;
   pragma Inline (T_GG);

   function T_H (X, Y, Z : U32) return U32 is
   begin
      return (X xor Y xor Z);
   end T_H;
   pragma Inline (T_H);

   function T_HH (A, B, C, D, X, S, Ac : U32) return U32 is
      Val : constant U32 := A + T_H (B, C, D) + X + Ac;
   begin
      return Rotate_Left (Val, Natural (S)) + B;
   end T_HH;
   pragma Inline (T_HH);

   function T_I (X, Y, Z : U32) return U32 is
   begin
      return Y xor (X or (not Z));
   end T_I;
   pragma Inline (T_I);

   function T_II (A, B, C, D, X, S, Ac : U32) return U32 is
      Val : constant U32 := A + T_I (B, C, D) + X + Ac;
   begin
      return Rotate_Left (Val, Natural (S)) + B;
   end T_II;
   pragma Inline (T_II);

   type Word_Buffer is array (0 .. 15) of U32;

   procedure Extract_Block_Words (Buf : Block; Words : out Word_Buffer) is
      Base : Block_Index;
   begin
      for I in 0 .. 15 loop
         Base := Buf'First + Block_Index (I * 4);

         Words (I) :=
           Shift_Left (U32 (Buf (Base + 3)), 24) or Shift_Left (U32 (Buf (Base + 2)), 16) or
           Shift_Left (U32 (Buf (Base + 1)), 8) or U32 (Buf (Base + 0));
      end loop;
   end Extract_Block_Words;

   procedure Transform (S : in out State) with
     Depends => (S => S), Post => S.Bit_Count = S.Bit_Count'Old
   is
      A : U32 := S.A;
      B : U32 := S.B;
      C : U32 := S.C;
      D : U32 := S.D;
      V : Word_Buffer;
   begin
      Extract_Block_Words (S.Buffer, V);

      A := T_FF (A, B, C, D, V (0), S11, 16#d76a_a478#);
      D := T_FF (D, A, B, C, V (1), S12, 16#e8c7_b756#);
      C := T_FF (C, D, A, B, V (2), S13, 16#2420_70db#);
      B := T_FF (B, C, D, A, V (3), S14, 16#c1bd_ceee#);
      A := T_FF (A, B, C, D, V (4), S11, 16#f57c_0faf#);
      D := T_FF (D, A, B, C, V (5), S12, 16#4787_c62a#);
      C := T_FF (C, D, A, B, V (6), S13, 16#a830_4613#);
      B := T_FF (B, C, D, A, V (7), S14, 16#fd46_9501#);
      A := T_FF (A, B, C, D, V (8), S11, 16#6980_98d8#);
      D := T_FF (D, A, B, C, V (9), S12, 16#8b44_f7af#);
      C := T_FF (C, D, A, B, V (10), S13, 16#ffff_5bb1#);
      B := T_FF (B, C, D, A, V (11), S14, 16#895c_d7be#);
      A := T_FF (A, B, C, D, V (12), S11, 16#6b90_1122#);
      D := T_FF (D, A, B, C, V (13), S12, 16#fd98_7193#);
      C := T_FF (C, D, A, B, V (14), S13, 16#a679_438e#);
      B := T_FF (B, C, D, A, V (15), S14, 16#49b4_0821#);

      A := T_GG (A, B, C, D, V (1), S21, 16#f61e_2562#);
      D := T_GG (D, A, B, C, V (6), S22, 16#c040_b340#);
      C := T_GG (C, D, A, B, V (11), S23, 16#265e_5a51#);
      B := T_GG (B, C, D, A, V (0), S24, 16#e9b6_c7aa#);
      A := T_GG (A, B, C, D, V (5), S21, 16#d62f_105d#);
      D := T_GG (D, A, B, C, V (10), S22, 16#244_1453#);
      C := T_GG (C, D, A, B, V (15), S23, 16#d8a1_e681#);
      B := T_GG (B, C, D, A, V (4), S24, 16#e7d3_fbc8#);
      A := T_GG (A, B, C, D, V (9), S21, 16#21e1_cde6#);
      D := T_GG (D, A, B, C, V (14), S22, 16#c337_07d6#);
      C := T_GG (C, D, A, B, V (3), S23, 16#f4d5_0d87#);
      B := T_GG (B, C, D, A, V (8), S24, 16#455a_14ed#);
      A := T_GG (A, B, C, D, V (13), S21, 16#a9e3_e905#);
      D := T_GG (D, A, B, C, V (2), S22, 16#fcef_a3f8#);
      C := T_GG (C, D, A, B, V (7), S23, 16#676f_02d9#);
      B := T_GG (B, C, D, A, V (12), S24, 16#8d2a_4c8a#);

      A := T_HH (A, B, C, D, V (5), S31, 16#fffa_3942#);
      D := T_HH (D, A, B, C, V (8), S32, 16#8771_f681#);
      C := T_HH (C, D, A, B, V (11), S33, 16#6d9d_6122#);
      B := T_HH (B, C, D, A, V (14), S34, 16#fde5_380c#);
      A := T_HH (A, B, C, D, V (1), S31, 16#a4be_ea44#);
      D := T_HH (D, A, B, C, V (4), S32, 16#4bde_cfa9#);
      C := T_HH (C, D, A, B, V (7), S33, 16#f6bb_4b60#);
      B := T_HH (B, C, D, A, V (10), S34, 16#bebf_bc70#);
      A := T_HH (A, B, C, D, V (13), S31, 16#289b_7ec6#);
      D := T_HH (D, A, B, C, V (0), S32, 16#eaa1_27fa#);
      C := T_HH (C, D, A, B, V (3), S33, 16#d4ef_3085#);
      B := T_HH (B, C, D, A, V (6), S34, 16#488_1d05#);
      A := T_HH (A, B, C, D, V (9), S31, 16#d9d4_d039#);
      D := T_HH (D, A, B, C, V (12), S32, 16#e6db_99e5#);
      C := T_HH (C, D, A, B, V (15), S33, 16#1fa2_7cf8#);
      B := T_HH (B, C, D, A, V (2), S34, 16#c4ac_5665#);

      A := T_II (A, B, C, D, V (0), S41, 16#f429_2244#);
      D := T_II (D, A, B, C, V (7), S42, 16#432a_ff97#);
      C := T_II (C, D, A, B, V (14), S43, 16#ab94_23a7#);
      B := T_II (B, C, D, A, V (5), S44, 16#fc93_a039#);
      A := T_II (A, B, C, D, V (12), S41, 16#655b_59c3#);
      D := T_II (D, A, B, C, V (3), S42, 16#8f0c_cc92#);
      C := T_II (C, D, A, B, V (10), S43, 16#ffef_f47d#);
      B := T_II (B, C, D, A, V (1), S44, 16#8584_5dd1#);
      A := T_II (A, B, C, D, V (8), S41, 16#6fa8_7e4f#);
      D := T_II (D, A, B, C, V (15), S42, 16#fe2c_e6e0#);
      C := T_II (C, D, A, B, V (6), S43, 16#a301_4314#);
      B := T_II (B, C, D, A, V (13), S44, 16#4e08_11a1#);
      A := T_II (A, B, C, D, V (4), S41, 16#f753_7e82#);
      D := T_II (D, A, B, C, V (11), S42, 16#bd3a_f235#);
      C := T_II (C, D, A, B, V (2), S43, 16#2ad7_d2bb#);
      B := T_II (B, C, D, A, V (9), S44, 16#eb86_d391#);

      S.A := S.A + A;
      S.B := S.B + B;
      S.C := S.C + C;
      S.D := S.D + D;
   end Transform;

   procedure Reset (S : out State) is
   begin
      S := State'(others => <>);
   end Reset;
   pragma Inline (Reset);

   --  Returns the offset into the buffer where the next byte should go.
   function Write_Head (S : State) return Block_Index is (Block_Index ((S.Bit_Count rem 512) / 8));

   type Block_Count is range 0 .. 64;

   --  Returns the number of bytes that must be written to finish the current block.
   function Buffer_Space (S : State) return Block_Count is (64 - Block_Count (Write_Head (S)));

   procedure Update (S : in out State; Bytes : Byte_Array) is
      Write : Block_Index := Write_Head (S);
   begin
      for Byte of Bytes loop
         S.Buffer (Write) := Byte;
         S.Bit_Count      := S.Bit_Count + 8;
         if (S.Bit_Count rem 512) = 0 then
            Transform (S);
         end if;
         Write := Write + 1;
      end loop;
   end Update;

   function Hex_Digest (S : State) return String is
      use Md5.Hex_Strings;
   begin
      return Hex (S.A) & Hex (S.B) & Hex (S.C) & Hex (S.D);
   end Hex_Digest;

   function To_Bytes (S : String) return Byte_Array with
     Post => (To_Bytes'Result'First = S'First and then To_Bytes'Result'Last = S'Last)
   is
      Result : Byte_Array (S'Range);
   begin
      for I in S'Range loop
         Result (I) := U8 (Character'Pos (S (I)));
      end loop;
      return Result;
   end To_Bytes;

   subtype U64_Bytes is Byte_Array (0 .. 7);

   procedure U64_To_Bytes_LE (Val : U64; A : out U64_Bytes) is
   begin
      for I in U64_Bytes'Range loop
         A (I) := U8 (16#FF# and Shift_Right (Val, I * 8));
      end loop;
   end U64_To_Bytes_LE;

   procedure Hash_String (S : out State; Str : Short_String) is
      use Md5.Short_Strings;
   begin
      Reset (S);
      for I in 1 .. Length (Str) loop
         S.Buffer (Block_Index (I - 1)) := U8 (Character'Pos (Str.Buf (Short_String_Index (I))));
      end loop;
      S.Buffer (Block_Index (Length (Str))) := 16#80#;
      S.Bit_Count                           := 8 * Bit_Count_Type (Length (Str));
      declare
         Bit_Count_Bytes : U64_Bytes;
      begin
         U64_To_Bytes_LE (U64 (S.Bit_Count), Bit_Count_Bytes);
         for I in Bit_Count_Bytes'Range loop
            S.Buffer (Block_Index (56 + I - Bit_Count_Bytes'First)) := Bit_Count_Bytes (I);
         end loop;
      end;
      Transform (S);
   end Hash_String;

   procedure Hash_String (S : out State; Str : String) is
   begin
      Reset (S);
      Update (S, To_Bytes (Str));
      Final (S);
   end Hash_String;

   procedure Final (S : in out State) is
      Remaining_Space_Bytes : constant Block_Count := Buffer_Space (S);

      --  We need 9 bytes for the pad byte and 64-bit message size at the end. If there are fewer than 9 bytes of space
      --  left, we need to finish the digest in the next 64-byte block.
      --
      --  This constant is the minimum number of bytes required to finish the MD5 digest.
      Add_Bytes_Size : constant Positive :=
        Positive (if Remaining_Space_Bytes < 9 then 64 + Remaining_Space_Bytes else Remaining_Space_Bytes);

      Add_Bytes : Byte_Array (1 .. Add_Bytes_Size) :=
        (1      => 16#80#,
         others => 0);

      Bit_Count_Bytes : U64_Bytes;

   begin
      U64_To_Bytes_LE (U64 (S.Bit_Count), Bit_Count_Bytes);
      Add_Bytes (Add_Bytes'Last - 7 .. Add_Bytes'Last) := Bit_Count_Bytes;
      Update (S, Add_Bytes);
   end Final;

end Md5.Hashing;
