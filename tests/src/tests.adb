with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

with STB.Image;

with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces.C; use Interfaces.C;
with Interfaces; use Interfaces;

with GNAT.OS_Lib;

with QOI; use QOI;

with Reference_QOI;

with AAA.Strings;

procedure Tests is

   type Storage_Array_Access is access all Storage_Array;

   type Input_Data is record
      Data : Storage_Array_Access;
      Desc : QOI.QOI_Desc;
   end record;

   procedure Write_To_File (Filename : String;
                            D : Storage_Array;
                            Size : Storage_Count);

   function Load_PNG (Filename : String) return Input_Data;
   function Load_QOI (Filename : String) return Input_Data;

   function Img (I : Input_Data) return String
   is ("Width:" & I.Desc.Width'Img &
       " Height:" & I.Desc.Height'Img &
         " Channels:" & I.Desc.Channels'Img &
         " Data (" & I.Data'First'Img & " .." &
         I.Data'Last'Img
       & ")");

   -------------------
   -- Write_To_File --
   -------------------

   procedure Write_To_File (Filename : String;
                            D : Storage_Array;
                            Size : Storage_Count)
   is
      use GNAT.OS_Lib;

      FD : File_Descriptor;
      Ret : Integer;
   begin

      FD := GNAT.OS_Lib.Create_File (Filename, Binary);

      if FD = Invalid_FD then
         Ada.Text_IO.Put_Line (GNAT.OS_Lib.Errno_Message);
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      Ret := Write (FD, D'Address, Integer (Size));

      if Ret /= Integer (Size) then
         Ada.Text_IO.Put_Line (GNAT.OS_Lib.Errno_Message);
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      Close (FD);
   end Write_To_File;

   --------------
   -- Load_PNG --
   --------------

   function Load_PNG (Filename : String) return Input_Data is
      W, H, Channels_In_File : Interfaces.C.int;

      Pixels : constant System.Address := STB.Image.Load
        (Filename, W, H, Channels_In_File, 0);

      Len : constant Storage_Count := Storage_Count (W * H * Channels_In_File);

      From_File : aliased Storage_Array (1 .. Len)
        with Address => Pixels;

      Data : constant Storage_Array_Access := new Storage_Array (1 .. Len);
      Result : Input_Data;
   begin

      Data.all := From_File;

      Result.Desc := (Width      => Storage_Count (W),
                      Height     => Storage_Count (H),
                      Channels   => Storage_Count (Channels_In_File),
                      Colorspace => QOI.SRGB);
      Result.Data := Data;
      return Result;
   end Load_PNG;

   --------------
   -- Load_QOI --
   --------------

   function Load_QOI (Filename : String) return Input_Data is
      use GNAT.OS_Lib;

      FD : File_Descriptor;
      Ret : Integer;

      Result : Input_Data;
   begin

      FD := GNAT.OS_Lib.Open_Read (Filename, Binary);

      if FD = Invalid_FD then
         Ada.Text_IO.Put_Line (Standard_Error, GNAT.OS_Lib.Errno_Message);
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      declare
         Len : constant Storage_Count := Storage_Count (File_Length (FD));
         In_Data : constant Storage_Array_Access :=
           new Storage_Array (1 .. Len);
      begin
         Ret := Read (FD, In_Data.all'Address, In_Data.all'Length);

         if Ret /= In_Data'Length then
            Ada.Text_IO.Put_Line (GNAT.OS_Lib.Errno_Message);
            GNAT.OS_Lib.OS_Exit (1);
         end if;

         Close (FD);

         QOI.Get_Desc (In_Data.all, Result.Desc);

         declare
            Out_Len : constant Storage_Count :=
              Result.Desc.Width * Result.Desc.Height * Result.Desc.Channels;
            Out_Data : constant Storage_Array_Access :=
              new Storage_Array (1 .. Out_Len);
            Output_Size : Storage_Count;
         begin
            QOI.Decode (Data        => In_Data.all,
                        Desc        => Result.Desc,
                        Output      => Out_Data.all,
                        Output_Size => Output_Size);

            Result.Data := Out_Data;

            if Reference_QOI.Check_Decode
              (In_Data.all,
               Result.Desc,
               Out_Data.all (Out_Data'First .. Out_Data'First + Output_Size - 1))
            then
               Put_Line ("Compare with reference decoder: OK");
            else
               Put_Line ("Compare with reference decoder: FAIL");
               GNAT.OS_Lib.OS_Exit (1);
            end if;

            return Result;
         end;
      end;
   end Load_QOI;

   Input : Input_Data;
begin

   if Ada.Command_Line.Argument_Count /= 2 then
      Put_Line (Standard_Error, "Usage: tests <infile> <outfile>");
      GNAT.OS_Lib.OS_Exit (1);
   end if;

   if AAA.Strings.Has_Suffix (Ada.Command_Line.Argument (1), ".png") then
      Put_Line ("Load PNG: " & Ada.Command_Line.Argument (1));
      Input := Load_PNG (Ada.Command_Line.Argument (1));
   elsif  AAA.Strings.Has_Suffix (Ada.Command_Line.Argument (1), ".qoi") then
      Put_Line ("Load QOI: " & Ada.Command_Line.Argument (1));
      Input := Load_QOI (Ada.Command_Line.Argument (1));
   else
      Put_Line (Standard_Error, "Invalid input file extension: '" &
                  Ada.Command_Line.Argument (1) & "'");
      GNAT.OS_Lib.OS_Exit (1);
   end if;

   Put_Line ("Loaded -> " & Img (Input));

   if AAA.Strings.Has_Suffix (Ada.Command_Line.Argument (2), ".png") then

      declare
         Result : Interfaces.C.int;
      begin
         Result := STB.Image.Write_PNG (Ada.Command_Line.Argument (2),
                                        int (Input.Desc.Width),
                                        int (Input.Desc.Height),
                                        int (Input.Desc.Channels),
                                        Input.Data.all'Address,
                                        0);

         if Result = 0 then
            Put_Line (Standard_Error, "PNG write error: '" &
                        Ada.Command_Line.Argument (2) & "'");
            GNAT.OS_Lib.OS_Exit (1);
         end if;
      end;
   elsif  AAA.Strings.Has_Suffix (Ada.Command_Line.Argument (2), ".qoi") then

      declare
         Output : Storage_Array (1 .. QOI.Encode_Worst_Case (Input.Desc));
         Output_Size : Storage_Count;

      begin
         QOI.Encode (Input.Data.all,
                     Input.Desc,
                     Output,
                     Output_Size);

         if Output_Size /= 0 then
            Put_Line ("Encode: OK");

            if Reference_QOI.Check_Encode
              (Input.Data.all,
               Input.Desc,
               Output (Output'First .. Output'First + Output_Size - 1))
            then
               Put_Line ("Compare with reference encoder: OK");
            else
               Put_Line ("Compare with reference encoder: FAIL");
               GNAT.OS_Lib.OS_Exit (1);
            end if;

            Write_To_File (Ada.Command_Line.Argument (2), Output, Output_Size);
         else
            Ada.Text_IO.Put_Line ("Encode failed");
            GNAT.OS_Lib.OS_Exit (1);
         end if;
      end;

   else
      Put_Line (Standard_Error, "Invalid output file extension: '" &
                  Ada.Command_Line.Argument (2) & "'");
      GNAT.OS_Lib.OS_Exit (1);
   end if;
end Tests;
