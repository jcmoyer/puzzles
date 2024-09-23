package Md5 with
  SPARK_Mode
is
   pragma Pure;

   Maximum_Length : constant := 55;

   subtype Short_String_Count is Natural range 0 .. Maximum_Length;
   subtype Short_String_Index is Natural range 1 .. Maximum_Length;

   type Short_String (Capacity : Short_String_Count) is private;

private

   type Char_Buffer is array (Short_String_Count range <>) of Character;

   --  Short_String is defined here instead of in a child package for performance reasons. Md5.Hashing wants to access
   --  the buffer directly to avoid copying. It doesn't seem possible to avoid copies using a sibling package with
   --  inline getter. Measured 15% runtime difference. Similarly, this is also why we're not using Bounded_String.
   type Short_String (Capacity : Short_String_Count) is record
      Buf : Char_Buffer (1 .. Capacity);
      Len : Short_String_Count;
   end record with
     Predicate => Len <= Capacity;

end Md5;
