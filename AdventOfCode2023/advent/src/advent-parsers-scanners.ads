with Ada.Strings.Maps; use Ada.Strings.Maps;

package Advent.Parsers.Scanners is

   Not_Found : exception;

   type String_Ptr is access constant String;

   type String_Span is record
      First : Positive;
      Last  : Natural;
   end record;

   type Scanner is record
      Source   : String_Ptr;
      Position : Positive;
   end record;

   procedure Read_While (S : in out Scanner; Set : Character_Set);
   procedure Read_While
     (S : in out Scanner; Set : Character_Set; Span : out String_Span);

   procedure Read_While_Not (S : in out Scanner; Set : Character_Set);
   procedure Read_While_Not (S : in out Scanner; Set : Character_Set; Span : out String_Span);

   function Try_Read_Until (S : in out Scanner; Substring : String) return Boolean;

   --  `Span` will contain the indices of `Substring`.
   procedure Read_Until (S : in out Scanner; Substring : String; Span : out String_Span);


end Advent.Parsers.Scanners;
