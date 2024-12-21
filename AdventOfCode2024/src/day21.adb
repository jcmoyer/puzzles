with Advent.IO;                  use Advent.IO;
with Advent.Strings;             use Advent.Strings;
with Advent.Directions;          use Advent.Directions;
with Advent.Integer_Vector_Math; use Advent.Integer_Vector_Math;
with Advent.Long_Parsers;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Text_IO;                use Ada.Text_IO;
with Advent.Containers.Linked_Deques;
with Ada.Containers.Hashed_Maps;
with Ada.Containers;
procedure Day21 is

   use type Ada.Containers.Count_Type;
   use type Ada.Containers.Hash_Type;

   package AIP renames Advent.Long_Parsers;

   Input_Error : exception;

   --  A command is abstractly a single instruction for a robot to perform.
   --  Commands can be "lowered" in the code-gen sense to a sequence of
   --  commands for a robot controlling another robot.
   type Command is (West, East, North, South, Press);

   package Command_Vectors is new Ada.Containers.Vectors (Positive, Command);

   package Command_Vectors_Vectors is new Ada.Containers.Vectors
     (Positive, Command_Vectors.Vector, "=" => Command_Vectors."=");

   type Number_Pad_Key is (Np_A, Np_0, Np_1, Np_2, Np_3, Np_4, Np_5, Np_6, Np_7, Np_8, Np_9);

   function Parse_Np_Key (C : Character) return Number_Pad_Key is
   begin
      case C is
         --!pp off
         when 'A' => return Np_A;
         when '0' => return Np_0;
         when '1' => return Np_1;
         when '2' => return Np_2;
         when '3' => return Np_3;
         when '4' => return Np_4;
         when '5' => return Np_5;
         when '6' => return Np_6;
         when '7' => return Np_7;
         when '8' => return Np_8;
         when '9' => return Np_9;
         when others =>
            raise Program_Error;
         --!pp on
      end case;
   end Parse_Np_Key;

   --  +---+---+---+
   --  | 7 | 8 | 9 |
   --  +---+---+---+
   --  | 4 | 5 | 6 |
   --  +---+---+---+
   --  | 1 | 2 | 3 |
   --  +---+---+---+
   --      | 0 | A |
   --      +---+---+
   function Position (K : Number_Pad_Key) return Vec2 is
   begin
      case K is
         --!pp off
         when Np_7 => return (1, 1);
         when Np_8 => return (1, 2);
         when Np_9 => return (1, 3);
         when Np_4 => return (2, 1);
         when Np_5 => return (2, 2);
         when Np_6 => return (2, 3);
         when Np_1 => return (3, 1);
         when Np_2 => return (3, 2);
         when Np_3 => return (3, 3);
         when Np_0 => return (4, 2);
         when Np_A => return (4, 3);
         --!pp on
      end case;
   end Position;

   type Directional_Pad_Key is (Dp_Left, Dp_Up, Dp_Down, Dp_Right, Dp_A);

   function Direction_To_Command (C : Cardinal_Direction) return Command is
   begin
      case C is
         --!pp off
         when East =>  return East;
         when South => return South;
         when West =>  return West;
         when North => return North;
         --!pp on
      end case;
   end Direction_To_Command;

   function Command_To_Key (C : Command) return Directional_Pad_Key is
   begin
      case C is
         --!pp off
         when East =>  return Dp_Right;
         when South => return Dp_Down;
         when West =>  return Dp_Left;
         when North => return Dp_Up;
         when Press => return Dp_A;
         --!pp on
      end case;
   end Command_To_Key;

   --      +---+---+
   --      | ^ | A |
   --  +---+---+---+
   --  | < | v | > |
   --  +---+---+---+
   function Position (K : Directional_Pad_Key) return Vec2 is
   begin
      case K is
         --!pp off
         when Dp_Up =>    return (1, 2);
         when Dp_A =>     return (1, 3);
         when Dp_Left =>  return (2, 1);
         when Dp_Down =>  return (2, 2);
         when Dp_Right => return (2, 3);
         --!pp on
      end case;
   end Position;

   procedure Print (Cv : Command_Vectors.Vector) is
   begin
      for C of Cv loop
         case C is
            when North =>
               Put ('^');
            when West =>
               Put ('<');
            when South =>
               Put ('v');
            when East =>
               Put ('>');
            when Press =>
               Put ('A');
         end case;
      end loop;
      New_Line;
   end Print;

   type Visited_Map is array (Integer range <>, Integer range <>) of Integer with
     Default_Component_Value => Integer'Last;

   package Direction_Vectors is new Ada.Containers.Vectors (Positive, Cardinal_Direction);

   function Directions_To_Commands (V : Direction_Vectors.Vector) return Command_Vectors.Vector is
      R : Command_Vectors.Vector;
   begin
      for X of V loop
         R.Append (Direction_To_Command (X));
      end loop;
      R.Append (Press);
      return R;
   end Directions_To_Commands;

   type Generic_Path_State is record
      Pos  : Vec2;
      Dirs : Direction_Vectors.Vector;
   end record;

   type Deque_Block_Size is mod 256;

   package Queues is new Advent.Containers.Linked_Deques
     (Element_Type => Generic_Path_State, Block_Index => Deque_Block_Size);

   Block_Numpad : constant Vec2 := (4, 1);
   Max_Numpad   : constant Vec2 := (4, 3);

   Block_Dirpad : constant Vec2 := (1, 1);
   Max_Dirpad   : constant Vec2 := (2, 3);

   --  This just produces all shortest paths from `From` to `To` where movement
   --  to `Block` is blocked.
   function Bfs_Generic (From, To, Block, Max : Vec2) return Command_Vectors_Vectors.Vector is
      Q : Queues.Deque;
      V : Visited_Map (1 .. 4, 1 .. 4); --  large enough to accomodate both keypads
      C : Generic_Path_State;
      N : Vec2;

      All_Paths : Command_Vectors_Vectors.Vector;

      function Valid_Neighbor (Pos : Vec2) return Boolean is
        (Pos (X) in 1 .. Max (X) and then Pos (Y) in 1 .. Max (Y) and then Pos /= Block
         and then
         (V (Pos (X), Pos (Y)) = Integer'Last
          or else V (C.Pos (X), C.Pos (Y)) + 1 <= V (Pos (X), Pos (Y))));

   begin
      Q.Push_Back ((Pos => From, others => <>));
      V (From (X), From (Y)) := 0;

      while not Q.Empty loop
         Q.Pop_Front (C);

         if C.Pos = To then
            All_Paths.Append (Directions_To_Commands (C.Dirs));
         else
            for D in Cardinal_Direction loop
               N := To_Vector (D) + C.Pos;
               if Valid_Neighbor (N) then
                  declare
                     Dirs : Direction_Vectors.Vector := C.Dirs;
                  begin
                     Dirs.Append (D);
                     Q.Push_Back ((Pos => N, Dirs => Dirs));
                     V (N (X), N (Y)) := V (C.Pos (X), C.Pos (Y)) + 1;
                  end;
               end if;
            end loop;
         end if;
      end loop;

      return All_Paths;
   end Bfs_Generic;

   type Memo_State is record
      From, To, Block, Max : Vec2;
      Depth                : Integer;
   end record;

   function Hash (S : Memo_State) return Ada.Containers.Hash_Type is
   begin
      return Hash (S.From) xor Hash (S.To);
   end Hash;

   package Memo_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Memo_State,
      Element_Type    => Long_Long_Integer,
      Hash            => Hash,
      Equivalent_Keys => "=");

   Cache : Memo_Maps.Map;

   function Lower_Commands
     (From, To, Block, Max : Vec2;
      Robot_Count          : Integer;
      Depth                : Integer := 0)
      return Long_Long_Integer
   is
      All_Paths : Command_Vectors_Vectors.Vector;

      --  Directional pad controlling us, which we will lower commands to.
      Dpad : Directional_Pad_Key := Dp_A;

      Sum : Long_Long_Integer := 0;
      Min : Long_Long_Integer := Long_Long_Integer'Last;

      Key : constant Memo_State :=
        (From => From, To => To, Block => Block, Max => Max, Depth => Depth);
   begin
      if Cache.Contains (Key) then
         return Cache.Element (Key);
      end if;
      All_Paths := Bfs_Generic (From, To, Block, Max);

      --  We're trying to pick the shortest path of All_Paths all the way to
      --  the final keypad.
      for Path of All_Paths loop
         if Depth = Robot_Count then
            --  Base case, at this depth we only care about the length of the
            --  command sequence
            Min := Long_Long_Integer'Min (Min, Long_Long_Integer (Path.Length));
         else
            --  Else we need to lower commands for the dpad controlling us...
            Sum  := 0;
            Dpad := Dp_A;

            --  Lower each command for this path individually; sum is the
            --  length of this command sequence
            for Cmd of Path loop
               Sum :=
                 Sum +
                 Lower_Commands
                   (Position (Dpad), Position (Command_To_Key (Cmd)), Block_Dirpad, Max_Dirpad,
                    Robot_Count, Depth + 1);

               Dpad := Command_To_Key (Cmd);
            end loop;

            --  Take the minimum length of all lowered command sequences
            Min := Long_Long_Integer'Min (Min, Sum);
         end if;
      end loop;

      Cache.Include (Key, Min);
      return Min;
   end Lower_Commands;

   function Do_Code (S : String; Robot_Count : Integer) return Long_Long_Integer is
      Npad : Number_Pad_Key    := Np_A;
      Sum  : Long_Long_Integer := 0;
   begin
      for C of S loop
         --  We start the search with numpad parameters, and it will recurse
         --  with dirpad parameters.
         Sum :=
           Sum +
           Lower_Commands
             (Position (Npad), Position (Parse_Np_Key (C)), Block_Numpad, Max_Numpad, Robot_Count);

         Npad := Parse_Np_Key (C);
      end loop;

      declare
         Ints : AIP.Array_Type (1 .. 1);
         N    : constant Natural := AIP.Extract_Integers (S, Ints);
      begin
         if 1 /= N then
            raise Input_Error with "expected 1 integer in code";
         end if;
         return Ints (1) * Sum;
      end;
   end Do_Code;

   Lines : constant String_Array := Read_All_Lines (Ada.Command_Line.Argument (1));

   Part_1 : Long_Long_Integer := 0;
   Part_2 : Long_Long_Integer := 0;

begin
   for Line of Lines loop
      Part_1 := Part_1 + Do_Code (Line, 2);
   end loop;

   Cache.Clear;

   for Line of Lines loop
      Part_2 := Part_2 + Do_Code (Line, 25);
   end loop;

   Solution (Part_1);
   Solution (Part_2);
end Day21;
