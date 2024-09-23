with Interfaces; use Interfaces;

package Md5.Hex_Strings with
  SPARK_Mode
is
   pragma Pure;

   Hex_Digits : constant String := "0123456789ABCDEF";

   subtype Hex_String_U8 is String (1 .. 2);
   subtype Hex_String_U32 is String (1 .. 8);

   function Hex (Byte : Unsigned_8) return Hex_String_U8;
   function Hex (Word : Unsigned_32) return Hex_String_U32;
end Md5.Hex_Strings;
