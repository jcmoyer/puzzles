with Interfaces; use Interfaces;

package Md5.Hashing with
  SPARK_Mode
is
   pragma Pure;

   subtype U8 is Unsigned_8;
   subtype U32 is Unsigned_32;
   subtype U64 is Unsigned_64;

   type Byte_Array is array (Natural range <>) of U8;

   pragma Warnings (Off, "2 ** 64 may have been intended here");
   type Block_Index is mod 64;
   pragma Warnings (On, "2 ** 64 may have been intended here");

   type Block is array (Block_Index) of U8;

   type Bit_Count_Type is mod 2**64;

   type State is record
      Buffer    : Block          := (others => 0);
      A         : U32            := 16#6745_2301#;
      B         : U32            := 16#efcd_ab89#;
      C         : U32            := 16#98ba_dcfe#;
      D         : U32            := 16#1032_5476#;
      Bit_Count : Bit_Count_Type := 0;
   end record;

   procedure Reset (S : out State) with
     Global => null, Post => S = State'(others => <>);

   procedure Update (S : in out State; Bytes : Byte_Array) with
     Global => null, Depends => (S =>
         (S,
          Bytes));

   procedure Final (S : in out State) with
     Global => null, Depends => (S => S);

   procedure Hash_String (S : out State; Str : Short_String) with
     Global => null, Depends => (S => Str);

   procedure Hash_String (S : out State; Str : String) with
     Global => null, Depends => (S => Str);

   function Hex_Digest (S : State) return String with
     Global => null;

end Md5.Hashing;
