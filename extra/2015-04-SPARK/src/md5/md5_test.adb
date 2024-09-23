with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Md5.Hashing;

procedure Md5_Test is

   Passed, Failed : Integer := 0;

   procedure Hash_Compare (Input : String; Expected : String) is
      Hash : Md5.Hashing.State;
   begin
      Md5.Hashing.Hash_String (Hash, Input);
      declare
         Result : constant String := Md5.Hashing.Hex_Digest (Hash);
      begin
         if Result = Expected then
            Passed := Passed + 1;
         else
            Failed := Failed + 1;
            Put_Line ("FAILED: '" & Input & "'");
            Put_Line ("    Expected: " & Expected);
            Put_Line ("    Got:      " & Result);
            Ada.Command_Line.Set_Exit_Status (1);
         end if;
      end;
   end Hash_Compare;

begin
   --  MD5 test suite:
   Hash_Compare ("", "D41D8CD98F00B204E9800998ECF8427E");
   Hash_Compare ("a", "0CC175B9C0F1B6A831C399E269772661");
   Hash_Compare ("abc", "900150983CD24FB0D6963F7D28E17F72");
   Hash_Compare ("message digest", "F96B697D7CB7938D525A2F31AAF161D0");
   Hash_Compare ("abcdefghijklmnopqrstuvwxyz", "C3FCD3D76192E4007DFB496CCA67E13B");
   Hash_Compare ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789", "D174AB98D277D9F5A5611C2C9F419D9F");
   Hash_Compare
     ("123456789012345678901234567890123456789012345678901234567890" & "12345678901234567890",
      "57EDF4A22BE3C955AC49DA2E2107B67A");

   --  Extras
   --  64 byte digest (requires 2 blocks)
   Hash_Compare
     ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz012345678910", "F997C0913706F704C4D76AB99F26793B");
   --  128 byte digest (3 blocks)
   Hash_Compare
     ("1111111111111111111111111111111111111111111111111111111111111111" &
      "1111111111111111111111111111111111111111111111111111111111111111",
      "79BBE7E17D3AD1582A76E8CB2FFBDEB3");
   --  129 byte digest (3 blocks)
   Hash_Compare
     ("1111111111111111111111111111111111111111111111111111111111111111" &
      "1111111111111111111111111111111111111111111111111111111111111111" & "1",
      "B6D19A990F888FD08F978EDEF35C59E9");

   Put_Line (Passed'Image & " test(s) passed; " & Failed'Image & " test(s) failed");
end Md5_Test;
