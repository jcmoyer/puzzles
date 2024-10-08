package Advent.Testing is
   pragma Pure;

   Assertion_Error : exception;

   procedure Assert (Condition : Boolean);
end Advent.Testing;
