with System; use System;

with Interfaces.C; use Interfaces.C;

with Ada.Text_IO; use Ada.Text_IO;
package body Reference_QOI is

   ------------------
   -- Check_Encode --
   ------------------

   function Check_Encode (Pix    : Storage_Array;
                          Desc   : QOI.QOI_Desc;
                          Output : Storage_Array)
                          return Boolean
   is
      Ref_Output_Ptr : System.Address;
      R_Desc       : aliased Ref_Desc;
      Ref_Out_Len    : aliased Interfaces.C.int;
   begin
      R_Desc.Width      := unsigned (Desc.Width);
      R_Desc.height     := unsigned (Desc.Height);
      R_Desc.channels   := char'Enum_Val (Desc.Channels);
      R_Desc.colorspace := char'Enum_Val (Desc.Colorspace'Enum_Rep);

      Ref_Output_Ptr := Encode (Data    => Pix'Address,
                                Desc    => R_Desc'Unrestricted_Access,
                                Out_Len => Ref_Out_Len'Access);

      if Ref_Output_Ptr = System.Null_Address then
         Put_Line ("Ref QOI failed to encode");
         return False;
      end if;

      declare
         Ref_Output : Storage_Array (1 .. Storage_Count (Ref_Out_Len))
           with Address => Ref_Output_Ptr;

         To_Compare : Storage_Count :=
           Storage_Count'Min (Ref_Output'Length, Output'Length);
      begin
         if Ref_Output /= Output then
            if Ref_Output'Length > Output'Length then
               Put_Line ("Ref QOI output bigger (" &
                           Ref_Output'Length'Img & ")" &
                           " than ours (" & Output'Length'Img & ")");

            elsif Ref_Output'Length < Output'Length then
               Put_Line ("Ref QOI output smaller (" &
                           Ref_Output'Length'Img & ")" &
                           " than ours (" & Output'Length'Img & ")");
            end if;

            for Offset in 0 .. To_Compare - 1 loop
               declare
                  A : constant Storage_Element :=
                    Ref_Output (Ref_Output'First + Offset);
                  B : constant Storage_Element :=
                    Output (Output'First + Offset);
               begin
                  if A /= B then
                     Put_Line ("Byte diff" & Offset'Img &
                                 " Ref:" & A'Img &
                                 " Act:" & B'Img);
                     return False;
                  end if;
               end;
            end loop;

            return False;
         end if;
      end;

      return True;
   end Check_Encode;

   ------------------
   -- Check_Decode --
   ------------------

   function Check_Decode (Data     : Storage_Array;
                          Out_Desc : QOI.QOI_Desc;
                          Out_Data : Storage_Array)
                          return Boolean
   is
      Ref_Output_Ptr : System.Address;
      R_Desc         : aliased Ref_Desc;
   begin
      Ref_Output_Ptr := Decode (Data     => Data'Address,
                                Size     => Data'Length,
                                Desc     => R_Desc'Unchecked_Access,
                                Channels => 0);

      if Ref_Output_Ptr = System.Null_Address then
         Put_Line ("Ref QOI failed to decode");
         return False;
      end if;

      if R_Desc.width /= unsigned (Out_Desc.Width) then
         Put_Line ("Ref width (" & R_Desc.width'Img & ") diff from ours " &
                     "(" & Out_Desc.Width'Img & ")");
         return False;
      end if;
      if R_Desc.height /= unsigned (Out_Desc.Height) then
         Put_Line ("Ref height (" & R_Desc.height'Img & ") diff from ours " &
                     "(" & Out_Desc.Height'Img & ")");
         return False;
      end if;
      if R_Desc.channels /= char'Enum_Val (Out_Desc.Channels) then
         Put_Line ("Ref channels (" & R_Desc.channels'Enum_Rep'Img &
                     ") diff from ours " &
                     "(" & Out_Desc.Channels'Img & ")");
         return False;
      end if;
      if R_Desc.colorspace /= char'Enum_Val (Out_Desc.Colorspace'Enum_Rep)
      then
         Put_Line ("Ref colorspace (" & R_Desc.colorspace'Enum_Rep'Img &
                     ") diff from ours " &
                     "(" & Out_Desc.Colorspace'Enum_Rep'Img & ")");
         return False;
      end if;

      declare
         Ref_Out_Len : constant Storage_Count :=
           Storage_Count (R_Desc.width *
                            R_Desc.height *
                              R_Desc.channels'Enum_Rep);

         Ref_Output : Storage_Array (1 .. Storage_Count (Ref_Out_Len))
           with Address => Ref_Output_Ptr;

         To_Compare : Storage_Count :=
           Storage_Count'Min (Ref_Output'Length, Out_Data'Length);
      begin
         if Ref_Output /= Out_Data then
            if Ref_Output'Length > Out_Data'Length then
               Put_Line ("Ref QOI decode output bigger (" &
                           Ref_Output'Length'Img & ")" &
                           " than ours (" & Out_Data'Length'Img & ")");

            elsif Ref_Output'Length < Out_Data'Length then
               Put_Line ("Ref QOI decode output smaller (" &
                           Ref_Output'Length'Img & ")" &
                           " than ours (" & Out_Data'Length'Img & ")");
            end if;

            for Offset in 0 .. To_Compare - 1 loop
               declare
                  A : constant Storage_Element :=
                    Ref_Output (Ref_Output'First + Offset);
                  B : constant Storage_Element :=
                    Out_Data (Out_Data'First + Offset);
               begin
                  if A /= B then
                     Put_Line ("Byte diff" & Offset'Img &
                                 " Ref:" & A'Img &
                                 " Act:" & B'Img);
                     return False;
                  end if;
               end;
            end loop;

            return False;
         end if;
      end;

      return True;
   end Check_Decode;

end Reference_QOI;
