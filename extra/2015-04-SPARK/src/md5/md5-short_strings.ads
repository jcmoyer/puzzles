--  Defines operations for Md5.Short_String.

with Interfaces; use Interfaces;

package Md5.Short_Strings with
  SPARK_Mode
is
   pragma Pure;

   function To_String (Input : Short_String) return String with
     Global => null;

   function Length (Input : Short_String) return Short_String_Count with
     Global => null;

   function Capacity (Input : Short_String) return Short_String_Count with
     Global => null;

   procedure Assign (Target : in out Short_String; Source : String) with
     Global  => null,
     Pre     => Source'Length <= Capacity (Target),
     Post    => Length (Target) = Source'Length,
     Depends => (Target =>
         (Target,
          Source));

   function Num_Digits (I : Unsigned_32) return Integer with
     Global => null, Post => Num_Digits'Result in 1 .. 10;

   procedure Overwrite_Digits (Target : in out Short_String; From : Short_String_Index; Number : Unsigned_32) with
     Global => null,
     Pre    => From + Num_Digits (Number) - 1 in 1 .. Capacity (Target),
     Post   => Length (Target) = Short_String_Count'Max (Length (Target), From + Num_Digits (Number) - 1);

private

   function To_String (Input : Short_String) return String is (String (Input.Buf (1 .. Input.Len)));
   pragma Inline (To_String);

   function Length (Input : Short_String) return Short_String_Count is (Input.Len);
   pragma Inline (Length);

   function Capacity (Input : Short_String) return Short_String_Count is (Input.Capacity);
   pragma Inline (Capacity);

end Md5.Short_Strings;
