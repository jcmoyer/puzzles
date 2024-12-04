package body Advent.Parsers.Scanners is

   procedure Read_While (S : in out Scanner; Set : Character_Set) is
   begin
      while S.Position <= S.Source'Length loop
         exit when not Is_In (S.Source (S.Position), Set);
         S.Position := S.Position + 1;
      end loop;
   end Read_While;

   procedure Read_While
     (S : in out Scanner; Set : Character_Set; Span : out String_Span) is
   begin
      Span.First := S.Position;
      Read_While (S, Set);
      Span.Last := S.Position - 1;
   end Read_While;

   procedure Read_While_Not (S : in out Scanner; Set : Character_Set) is
   begin
      while S.Position <= S.Source'Length loop
         exit when Is_In (S.Source (S.Position), Set);
         S.Position := S.Position + 1;
      end loop;
   end Read_While_Not;

   procedure Read_While_Not
     (S : in out Scanner; Set : Character_Set; Span : out String_Span) is
   begin
      Span.First := S.Position;
      Read_While_Not (S, Set);
      Span.Last := S.Position - 1;
   end Read_While_Not;

   function Try_Read_Until
     (S : in out Scanner; Substring : String) return Boolean is
   begin
      while S.Position <= S.Source'Length loop
         if not (S.Position + Substring'Length - 1 in S.Source'Range) then
            return False;
         end if;

         if S.Source (S.Position .. S.Position + Substring'Length - 1)
           = Substring
         then
            return True;
         end if;

         S.Position := S.Position + 1;
      end loop;

      return False;
   end Try_Read_Until;

   procedure Read_Until
     (S : in out Scanner; Substring : String; Span : out String_Span) is
   begin
      if Try_Read_Until (S, Substring) then
         Span.First := S.Position;
         Span.Last := S.Position + Substring'Length - 1;
      else
         raise Not_Found with "substring not found: " & Substring;
      end if;
   end Read_Until;

end Advent.Parsers.Scanners;
