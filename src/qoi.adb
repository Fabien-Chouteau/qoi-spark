package body QOI
with SPARK_Mode
is

   pragma Compile_Time_Error (Storage_Element'Size /= 8,
                              "Invalid element size");

   function Shift_Left
     (Value  : Storage_Element;
      Amount : Natural) return Storage_Element
     with
       Import,
       Convention => Intrinsic;
   function Shift_Right
     (Value  : Storage_Element;
      Amount : Natural) return Storage_Element
     with
       Import,
       Convention => Intrinsic;

   QOI_INDEX   : constant Storage_Element := 16#00#; -- 00xxxxxx
   QOI_RUN_8   : constant Storage_Element := 16#40#; -- 010xxxxx
   QOI_RUN_16  : constant Storage_Element := 16#60#; -- 011xxxxx
   QOI_DIFF_8  : constant Storage_Element := 16#80#; -- 10xxxxxx
   QOI_DIFF_16 : constant Storage_Element := 16#c0#; -- 110xxxxx
   QOI_DIFF_24 : constant Storage_Element := 16#e0#; -- 1110xxxx
   QOI_COLOR   : constant Storage_Element := 16#f0#; -- 1111xxxx

   QOI_MASK_2 : constant Storage_Element := 16#c0#; -- 11000000
   QOI_MASK_3 : constant Storage_Element := 16#e0#; -- 11100000
   QOI_MASK_4 : constant Storage_Element := 16#f0#; -- 11110000

   QOI_MAGIC : constant Unsigned_32 :=
     (113 * 2 ** 24) + (111 * 2 ** 16) + (105 * 2 ** 8) + 102;

   type Color is record
      R, G, B, A : Storage_Element;
   end record
     with Size => 32;

   type Index_Range is range 0 .. 63;
   subtype Run_Range is Unsigned_32 range 0 .. 8224;

   function Hash (C : Color) return Storage_Element;

   ----------
   -- Hash --
   ----------

   function Hash (C : Color) return Storage_Element is
   begin
      return C.R xor C.G xor C.B xor C.A;
   end Hash;

   ------------
   -- Encode --
   ------------

   procedure Encode (Pix         :     Storage_Array;
                     Desc        :     QOI_Desc;
                     Output      : out Storage_Array;
                     Output_Size : out Storage_Count)
   is
      Max_Chunk_Size : constant Storage_Count := Desc.Channels + 1;
      Max_Size : constant Storage_Count := Pix'Length * Max_Chunk_Size +
        QOI_HEADER_SIZE + QOI_PADDING;

      pragma Assert (Max_Size > Pix'Length);

      P : Storage_Count := Output'First;

      procedure Push (D : Unsigned_32)
        with Post => P = P'Old + 4;

      procedure Push (D : Storage_Element)
        with Post => P = P'Old + 1;

      procedure Diff8 (VR, VG, VB : Integer)
        with Pre => VR in -2 .. 1
           and then VG in -2 .. 1
           and then VB in -2 .. 1,
        Post => P = P'Old + 1;

      procedure Diff16 (VR, VG, VB : Integer)
        with Pre => VR in -16 .. 15
            and then VG in -8 .. 7
            and then VB in -8 .. 7,
        Post => P = P'Old + 2;

      procedure Diff24 (VR, VG, VB, VA : Integer)
        with Pre => VR in -16 .. 15 and then VG in -16 .. 15
           and then VB in -16 .. 15 and then VA in -16 .. 15,
        Post => P = P'Old + 3;

      procedure Push (D : Unsigned_32) is
      begin
         pragma Style_Checks ("M100");
         Output (P)     := Storage_Element (Shift_Right (D and 16#FF_00_00_00#, 24));
         Output (P + 1) := Storage_Element (Shift_Right (D and 16#00_FF_00_00#, 16));
         Output (P + 2) := Storage_Element (Shift_Right (D and 16#00_00_FF_00#, 8));
         Output (P + 3) := Storage_Element (Shift_Right (D and 16#00_00_00_FF#, 0));

         P := P + 4;
      end Push;

      procedure Push  (D : Storage_Element) is
      begin
         Output (P) := D;
         P := P + 1;
      end Push;

      procedure Diff8 (VR, VG, VB : Integer) is
         R : constant Storage_Element := Storage_Element (VR + 2);
         G : constant Storage_Element := Storage_Element (VG + 2);
         B : constant Storage_Element := Storage_Element (VB + 2);
      begin
         Push (QOI_DIFF_8
                or
                  Shift_Left (R, 4)
                or
                  Shift_Left (G, 2)
                or
                  Shift_Left (B, 0)
               );
      end Diff8;

      procedure Diff16 (VR, VG, VB : Integer) is
         R : constant Storage_Element := Storage_Element (VR + 16);
         G : constant Storage_Element := Storage_Element (VG + 8);
         B : constant Storage_Element := Storage_Element (VB + 8);
      begin
         Push (QOI_DIFF_16 or R);
         Push (Shift_Left (G, 4)
                or
                Shift_Left (B, 0));
      end Diff16;

      procedure Diff24 (VR, VG, VB, VA : Integer) is
         R : constant Storage_Element := Storage_Element (VR + 16);
         G : constant Storage_Element := Storage_Element (VG + 16);
         B : constant Storage_Element := Storage_Element (VB + 16);
         A : constant Storage_Element := Storage_Element (VA + 16);
      begin
         Push (QOI_DIFF_24 or Shift_Right (R, 1));

         Push (Shift_Left (R, 7)
                or
                Shift_Left (G, 2)
                or
                Shift_Right (B, 3));

         Push (Shift_Left (B, 5) or A);
      end Diff24;

      Run : Run_Range := 0;
      --  Prev_Run : Run_Range with Ghost;

      procedure Push_Run
        with
          Pre => Run /= 0,
          Post => Run = 0
          and then (P = P'Old + 1
                    or else
                    P = P'Old + 2);

      procedure Push_Run is
      begin
         if Run < 33 then
            Run := Run - 1;
            Push (QOI_RUN_8 or Storage_Element (Run));
         else
            Run := Run - 33;
            Push (QOI_RUN_16 or Storage_Element (Shift_Right (Run, 8)));
            Push (Storage_Element (Run and 16#FF#));
         end if;

         Run := 0;
      end Push_Run;

      Number_Of_Pixels : constant Storage_Count := Pix'Length / Desc.Channels;

      subtype Pixel_Index_Range
        is Storage_Count range 0 .. Number_Of_Pixels - 1;

      function Read (Index : Pixel_Index_Range) return Color;

      function Read (Index : Pixel_Index_Range) return Color is
         Result : Color;
         Offset : constant Storage_Count := Index * Desc.Channels;
         Buffer_Index : constant Storage_Count := Pix'First + Offset;
      begin
         Result.R := Pix (Buffer_Index);
         Result.G := Pix (Buffer_Index + 1);
         Result.B := Pix (Buffer_Index + 2);

         if Desc.Channels = 4 then
            Result.A := Pix (Buffer_Index + 3);
         else
            Result.A := 0;
         end if;
         return Result;
      end Read;

      Index   : array (Index_Range) of Color := (others => ((0, 0, 0, 0)));
      Px_Prev : Color := (R => 0, G => 0, B => 0, A => 255);
      Px      : Color;
   begin

      if Output'Length < Encode_Worst_Case (Desc) then
         Output_Size := 0;
         return;
      end if;

      Push (QOI_MAGIC);
      Push (Unsigned_32 (Desc.Width));
      Push (Unsigned_32 (Desc.Height));
      Push (Storage_Element (Desc.Channels));
      Push (Storage_Element (Desc.Colorspace'Enum_Rep));

      for Px_Index in Pixel_Index_Range loop
         Px := Read (Px_Index);

         if Px = Px_Prev then
            Run := Run + 1;
            if Run = Run_Range'Last
              or else
               Px_Index = Pixel_Index_Range'Last
            then
               Push_Run;
            end if;

         else

            if Run > 0 then
               Push_Run;
            end if;

            pragma Assert (Run = 0);

            declare
               Index_Pos : constant Index_Range :=
                 Index_Range (Hash (Px) mod Index'Length);
            begin
               if Index (Index_Pos) = Px then
                  Push (QOI_INDEX or Storage_Element (Index_Pos));
               else
                  Index (Index_Pos) := Px;

                  declare
                     VR : constant Integer :=
                       Integer (Px.R) - Integer (Px_Prev.R);
                     VG : constant Integer :=
                       Integer (Px.G) - Integer (Px_Prev.G);
                     VB : constant Integer :=
                       Integer (Px.B) - Integer (Px_Prev.B);
                     VA : constant Integer :=
                       Integer (Px.A) - Integer (Px_Prev.A);
                  begin
                     if         VR in -16 .. 15
                       and then VG in -16 .. 15
                       and then VB in -16 .. 15
                       and then VA in -16 .. 15
                     then
                        if         VA = 0
                          and then VR in -2 .. 1
                          and then VG in -2 .. 1
                          and then VB in -2 .. 1
                        then
                           Diff8 (VR, VG, VB);

                        elsif      VA = 0
                          and then VR in -16 .. 15
                          and then VG in -8 .. 7
                          and then VB in -8 .. 7
                        then
                           Diff16 (VR, VG, VB);
                        else
                           Diff24 (VR, VG, VB, VA);
                        end if;
                     else
                        Push (QOI_COLOR
                               or (if VR /= 0 then 8 else 0)
                               or (if VG /= 0 then 4 else 0)
                               or (if VB /= 0 then 2 else 0)
                               or (if VA /= 0 then 1 else 0));

                        if VR /= 0 then
                           Push (Px.R);
                        end if;
                        if VG /= 0 then
                           Push (Px.G);
                        end if;
                        if VB /= 0 then
                           Push (Px.B);
                        end if;
                        if Desc.Channels = 4 and then VA /= 0 then
                           Push (Px.A);
                        end if;
                     end if;
                  end;
               end if;
            end;
         end if;

         Px_Prev := Px;
      end loop;

      for Count in 1 .. QOI_PADDING loop
         Push (Storage_Element (0));
      end loop;

      Output_Size := P - Output'First;
   end Encode;

   --------------
   -- Get_Desc --
   --------------

   procedure Get_Desc (Data :     Storage_Array;
                       Desc : out QOI_Desc)
   is
      P : Storage_Count := Data'First;

      function Pop8 return Storage_Element;
      function Pop32 return Unsigned_32;

      function Pop8 return Storage_Element is
         Res : Storage_Element;
      begin
         Res := Data (P);
         P := P + 1;
         return Res;
      end Pop8;

      function Pop32 return Unsigned_32 is
         A : constant Unsigned_32 := Unsigned_32 (Data (P));
         B : constant Unsigned_32 := Unsigned_32 (Data (P + 1));
         C : constant Unsigned_32 := Unsigned_32 (Data (P + 2));
         D : constant Unsigned_32 := Unsigned_32 (Data (P + 3));
      begin
         P := P + 4;
         return Shift_Left (A, 24)
           or Shift_Left (B, 16)
           or Shift_Left (C, 8)
           or D;
      end Pop32;

      Magic : Unsigned_32;
   begin
      if Data'Length < QOI_HEADER_SIZE then
         Desc := (0, 0, 0, SRGB);
         return;
      end if;

      Magic := Pop32;

      if Magic /= QOI_MAGIC then
         Desc := (0, 0, 0, SRGB);
         return;
      end if;

      Desc.Width := Storage_Count (Pop32);
      Desc.Height := Storage_Count (Pop32);
      Desc.Channels := Storage_Count (Pop8);
      Desc.Colorspace := Colorspace_Kind'Enum_Val (Pop8);
   end Get_Desc;

   ------------
   -- Decode --
   ------------

   procedure Decode (Data        :     Storage_Array;
                     Desc        : out QOI_Desc;
                     Output      : out Storage_Array;
                     Output_Size : out Storage_Count)
   is
      P : Storage_Count;
      Out_Index : Storage_Count := Output'First;

      function Pop8 return Storage_Element;
      procedure Push (D : Storage_Element);

      function Pop8 return Storage_Element is
         Res : Storage_Element;
      begin
         Res := Data (P);
         P := P + 1;
         return Res;
      end Pop8;

      procedure Push (D : Storage_Element) is
      begin
         Output (Out_Index) := D;
         Out_Index := Out_Index + 1;
      end Push;

   begin

      Get_Desc (Data, Desc);

      if Desc.Width = 0
        or else
         Desc.Height = 0
        or else
         Desc.Channels not in 3 .. 4
        or else
         Output'Length < Desc.Width * Desc.Height * Desc.Channels
      then
         Output_Size := 0;
         return;
      end if;

      P := Data'First + QOI_HEADER_SIZE;
      declare
         Number_Of_Pixels : constant Storage_Count := Desc.Width * Desc.Height;

         subtype Pixel_Index_Range
           is Storage_Count range 0 .. Number_Of_Pixels - 1;

         Last_Chunk : constant Storage_Count := Data'Last - QOI_PADDING;

         Index   : array (Index_Range) of Color := (others => ((0, 0, 0, 0)));
         Px      : Color := (R => 0, G => 0, B => 0, A => 255);
         B1, B2, B3 : Storage_Element;
         Run : Run_Range := 0;
      begin
         for Px_Index in Pixel_Index_Range loop
            if Run > 0 then
               Run := Run - 1;
            elsif P <= Last_Chunk then
               B1 := Pop8;

               if (B1 and QOI_MASK_2) = QOI_INDEX then
                  Px := Index (Index_Range (B1 and 2#0011_1111#));

               elsif (B1 and QOI_MASK_3) = QOI_RUN_8 then
                  Run := Run_Range (B1 and 2#0001_1111#);

               elsif (B1 and QOI_MASK_3) = QOI_RUN_16 then
                  B2 := Pop8;
                  Run := Run_Range (B1 and 2#0001_1111#) * 2 ** 8 +
                    Run_Range (B2) + 32;

               elsif (B1 and QOI_MASK_2) = QOI_DIFF_8 then
                  Px.R := Px.R + ((Shift_Right (B1, 4) and 16#03#) - 2);
                  Px.G := Px.G + ((Shift_Right (B1, 2) and 16#03#) - 2);
                  Px.B := Px.B + ((Shift_Right (B1, 0) and 16#03#) - 2);

               elsif (B1 and QOI_MASK_3) = QOI_DIFF_16 then
                  B2 := Pop8;
                  Px.R := Px.R + ((Shift_Right (B1, 0) and 16#1F#) - 16);
                  Px.G := Px.G + ((Shift_Right (B2, 4) and 16#0F#) - 8);
                  Px.B := Px.B + ((Shift_Right (B2, 0) and 16#0F#) - 8);

               elsif (B1 and QOI_MASK_4) = QOI_DIFF_24 then
                  B2 := Pop8;
                  B3 := Pop8;

                  pragma Style_Checks ("M110");
                  Px.R := Px.R + ((Shift_Left (B1 and 16#0F#, 1) or Shift_Right (B2, 7))  - 16);
                  Px.G := Px.G + (Shift_Right (B2 and 16#7C#, 2) - 16);
                  Px.B := Px.B + ((Shift_Left (B2 and 16#03#, 3) or Shift_Right (B3 and 16#E0#, 5)) - 16);
                  Px.A := Px.A + ((Shift_Right (B3, 0) and 16#1F#) - 16);

               elsif (B1 and QOI_MASK_4) = QOI_COLOR then

                  if (B1 and 8) /= 0 then
                     Px.R := Pop8;
                  end if;
                  if (B1 and 4) /= 0 then
                     Px.G := Pop8;
                  end if;
                  if (B1 and 2) /= 0 then
                     Px.B := Pop8;
                  end if;
                  if (B1 and 1) /= 0 then
                     Px.A := Pop8;
                  end if;
               end if;

               Index (Index_Range (Hash (Px) mod Index'Length)) := Px;
            end if;

            Push (Px.R);
            Push (Px.G);
            Push (Px.B);
            if Desc.Channels = 4 then
               Push (Px.A);
            end if;
         end loop;
      end;

      Output_Size := Out_Index - Output'First;
   end Decode;

end QOI;
