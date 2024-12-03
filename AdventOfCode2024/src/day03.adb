with Advent.IO;
with Ada.Command_Line;
with Ada.Characters;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers.Vectors;

procedure Day03 is
   type Token_Type is
     (Mul, Comma, LParen, RParen, Number, T_Do, T_Dont, Unrecognized);

   type Token_Type_Array is array (Positive range <>) of Token_Type;

   type Token is record
      Source_First : Integer;
      Source_Last  : Integer;
      Kind         : Token_Type;
   end record;

   package Token_Vectors is new
     Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Token);

   function To_Int (Source : String; Tok : Token) return Integer
   is (Integer'Value (Source (Tok.Source_First .. Tok.Source_Last)))
   with Pre => Tok.Kind = Number;

   function Lex (Source : String) return Token_Vectors.Vector is
      Result : Token_Vectors.Vector;
      Cursor : Integer := Source'First;

      procedure Move (N : Integer) is
      begin
         Cursor := Cursor + N;
      end Move;

      procedure Next is
      begin
         Move (1);
      end Next;

      function Match_String (S : String) return Boolean
      is (Cursor + S'Length - 1 in Source'Range
          and then Source (Cursor .. Cursor + S'Length - 1) = S);

      procedure Lex_Lower is
      begin
         if Match_String ("mul") then
            Result.Append
              (Token'
                 (Source_First => Cursor,
                  Source_Last  => Cursor + 2,
                  Kind         => Mul));
            Move (3);
         elsif Match_String ("don't") then
            --  IMPORTANT: match don't before do because do is a substring of
            --  don't; if we match in the opposite order, "don't" will be tokenized
            --  as `do` + `n` + `'` + `t`
            Result.Append
              (Token'
                 (Source_First => Cursor,
                  Source_Last  => Cursor + 4,
                  Kind         => T_Dont));
            Move (5);
         elsif Match_String ("do") then
            Result.Append
              (Token'
                 (Source_First => Cursor,
                  Source_Last  => Cursor + 1,
                  Kind         => T_Do));
            Move (2);
         else
            Result.Append
              (Token'
                 (Source_First => Cursor,
                  Source_Last  => Cursor,
                  Kind         => Unrecognized));
            Next;
         end if;
      end Lex_Lower;

      procedure Lex_Number is
         Number_First : constant Integer := Cursor;
         Number_Last  : Integer := Number_First;
      begin
         while Is_Digit (Source (Cursor)) loop
            Number_Last := Cursor;
            Cursor := Cursor + 1;
         end loop;
         Result.Append
           (Token'
              (Source_First => Number_First,
               Source_Last  => Number_Last,
               Kind         => Number));
      end Lex_Number;
   begin
      while Cursor <= Source'Last loop
         if Is_Lower (Source (Cursor)) then
            Lex_Lower;
         elsif Is_Digit (Source (Cursor)) then
            Lex_Number;
         elsif Source (Cursor) = '(' then
            Result.Append
              (Token'
                 (Source_First => Cursor,
                  Source_Last  => Cursor,
                  Kind         => LParen));
            Next;
         elsif Source (Cursor) = ')' then
            Result.Append
              (Token'
                 (Source_First => Cursor,
                  Source_Last  => Cursor,
                  Kind         => RParen));
            Next;
         elsif Source (Cursor) = ',' then
            Result.Append
              (Token'
                 (Source_First => Cursor,
                  Source_Last  => Cursor,
                  Kind         => Comma));
            Next;
         else
            Result.Append
              (Token'
                 (Source_First => Cursor,
                  Source_Last  => Cursor,
                  Kind         => Unrecognized));
            Next;
         end if;
      end loop;

      return Result;
   end Lex;

   function Eval_Muls
     (Source : String; Tokens : Token_Vectors.Vector; Handle_Do_Dont : Boolean)
      return Integer
   is
      Cursor : Integer := Tokens.First_Index;
      Result : Integer := 0;
      Enable : Boolean := True;

      function Match_Sequence (A : Token_Type_Array) return Boolean is
      begin
         --  bounds check
         if not (Cursor + A'Length - 1 in
                   Tokens.First_Index .. Tokens.Last_Index)
         then
            return False;
         end if;

         --  token kind check
         for I in A'Range loop
            if Tokens (Cursor + I - A'First).Kind /= A (I) then
               return False;
            end if;
         end loop;

         return True;
      end Match_Sequence;

      Seq_Mul  : constant Token_Type_Array :=
        (Mul, LParen, Number, Comma, Number, RParen);
      Seq_Do   : constant Token_Type_Array := (T_Do, LParen, RParen);
      Seq_Dont : constant Token_Type_Array := (T_Dont, LParen, RParen);

      function Match_Mul return Boolean
      is (Match_Sequence (Seq_Mul));

      function Match_Do return Boolean
      is (Match_Sequence (Seq_Do));

      function Match_Dont return Boolean
      is (Match_Sequence (Seq_Dont));

   begin
      while Cursor <= Tokens.Last_Index loop
         if Enable and then Match_Mul then
            Result :=
              Result
              + To_Int (Source, Tokens (Cursor + 2))
                * To_Int (Source, Tokens (Cursor + 4));
            Cursor := Cursor + Seq_Mul'Length;
         elsif Handle_Do_Dont and then Match_Do then
            Enable := True;
            Cursor := Cursor + Seq_Do'Length;
         elsif Handle_Do_Dont and then Match_Dont then
            Enable := False;
            Cursor := Cursor + Seq_Dont'Length;
         else
            Cursor := Cursor + 1;
         end if;
      end loop;
      return Result;
   end Eval_Muls;

   Text : constant String :=
     Advent.IO.Read_All_Text (Ada.Command_Line.Argument (1));

   Tokens : constant Token_Vectors.Vector := Lex (Text);

begin
   Advent.IO.Solution (Eval_Muls (Text, Tokens, False)'Image);
   Advent.IO.Solution (Eval_Muls (Text, Tokens, True)'Image);
end Day03;
