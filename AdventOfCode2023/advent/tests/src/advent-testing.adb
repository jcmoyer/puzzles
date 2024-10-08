package body Advent.Testing is

   procedure Assert (Condition : Boolean) is
   begin
      if not Condition then
         raise Assertion_Error;
      end if;
   end Assert;

end Advent.Testing;
