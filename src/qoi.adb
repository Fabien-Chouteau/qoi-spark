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
      P   : Storage_Count := Output'First;
      Run : Run_Range     := 0;

      function Valid_Parameters return Boolean is
        (Valid_Size (Desc)
         and then Output'First >= 0
         and then Output'Last < Storage_Count'Last
         and then Output'Length >= Encode_Worst_Case (Desc));

      procedure Push (D : Unsigned_32)
      with
        Pre  =>
          Valid_Parameters
            and then P in Output'First .. Output'Last - 3
            and then Output (Output'First .. P - 1)'Initialized,
        Post =>
          P = P'Old + 4 and then Output (Output'First .. P - 1)'Initialized;

      procedure Push (D : Storage_Element)
      with
        Pre  =>
          Valid_Parameters
            and then P in Output'Range
            and then Output (Output'First .. P - 1)'Initialized,
        Post =>
          P = P'Old + 1 and then Output (Output'First .. P - 1)'Initialized;

      procedure Push_Run
      with
        Pre            =>
          Run /= 0
            and then Valid_Parameters
            and then P in Output'First .. Output'Last - (if Run < 33 then 0 else 1)
            and then Output (Output'First .. P - 1)'Initialized,
        Post           =>
          Run = 0 and then Output (Output'First .. P - 1)'Initialized,
        Contract_Cases =>
          (Run < 33 => P = P'Old + 1,
           others   => P = P'Old + 2);

      procedure Diff8 (VR, VG, VB : Integer)
      with
        Pre =>
          VR in -2 .. 1
            and then VG in -2 .. 1
            and then VB in -2 .. 1
            and then Valid_Parameters
            and then P in Output'Range
            and then Output (Output'First .. P - 1)'Initialized,
        Post =>
          P = P'Old + 1 and then Output (Output'First .. P - 1)'Initialized;

      procedure Diff16 (VR, VG, VB : Integer)
      with
        Pre =>
          VR in -16 .. 15
            and then VG in -8 .. 7
            and then VB in -8 .. 7
            and then Valid_Parameters
            and then P in Output'First .. Output'Last - 1
            and then Output (Output'First .. P - 1)'Initialized,
        Post =>
          P = P'Old + 2 and then Output (Output'First .. P - 1)'Initialized;

      procedure Diff24 (VR, VG, VB, VA : Integer)
      with
        Pre =>
          VR in -16 .. 15
            and then VG in -16 .. 15
            and then VB in -16 .. 15
            and then VA in -16 .. 15
            and then Valid_Parameters
            and then P in Output'First .. Output'Last - 2
            and then Output (Output'First .. P - 1)'Initialized,
        Post =>
          P = P'Old + 3 and then Output (Output'First .. P - 1)'Initialized;

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

      pragma Assert (P = Output'First + QOI_HEADER_SIZE);
      pragma Assert (Run = 0);
      pragma Assert (Output (Output'First .. P - 1)'Initialized);
      for Px_Index in Pixel_Index_Range loop

         pragma Loop_Invariant
           (Run in 0 .. Run_Range'Last - 1);
         pragma Loop_Invariant
           (P - Output'First in
              0
                ..
              QOI_HEADER_SIZE
              + (Desc.Channels + 1) * (Storage_Count (Px_Index) - Storage_Count (Run)));
         pragma Loop_Invariant (Output (Output'First .. P - 1)'Initialized);

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
            pragma Assert
              (P - Output'First in
                 0 .. QOI_HEADER_SIZE
                      + (Desc.Channels + 1)
                          * Storage_Count (Px_Index));

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

         pragma Assert (Output (Output'First .. P - 1)'Initialized);
         Px_Prev := Px;
      end loop;

      pragma Assert (Output (Output'First .. P - 1)'Initialized);
      pragma Assert (P - Output'First in
        0 .. QOI_HEADER_SIZE + (Desc.Channels + 1) * Number_Of_Pixels);

      for Count in Storage_Offset range 1 .. QOI_PADDING loop
         pragma Loop_Invariant
           (P - Output'First in 0 .. Encode_Worst_Case (Desc) - QOI_PADDING + Count - 1);
         pragma Loop_Invariant (Output (Output'First .. P - 1)'Initialized);
         Push (Storage_Element (0));
      end loop;

      pragma Assert (Output (Output'First .. P - 1)'Initialized);
      Output_Size := P - Output'First;
   end Encode;

   --------------
   -- Get_Desc --
   --------------

   procedure Get_Desc (Data :     Storage_Array;
                       Desc : out QOI_Desc)
   is
      P : Storage_Count := Data'First;

      procedure Pop8  (Result : out Storage_Element);
      procedure Pop32 (Result : out Unsigned_32);

      procedure Pop8 (Result : out Storage_Element) is
      begin
         Result := Data (P);
         P := P + 1;
      end Pop8;

      procedure Pop32 (Result : out Unsigned_32) is
         A : constant Unsigned_32 := Unsigned_32 (Data (P));
         B : constant Unsigned_32 := Unsigned_32 (Data (P + 1));
         C : constant Unsigned_32 := Unsigned_32 (Data (P + 2));
         D : constant Unsigned_32 := Unsigned_32 (Data (P + 3));
      begin
         Result :=
           Shift_Left (A, 24)
           or Shift_Left (B, 16)
           or Shift_Left (C, 8)
           or D;
         P := P + 4;
      end Pop32;

      Magic   : Unsigned_32;
      Temp_32 : unsigned_32;
      Temp_8  : Storage_Element;
   begin
      if Data'Length < QOI_HEADER_SIZE then
         Desc := (0, 0, 0, SRGB);
         return;
      end if;

      Pop32 (Magic);

      if Magic /= QOI_MAGIC then
         Desc := (0, 0, 0, SRGB);
         return;
      end if;

      Pop32 (Temp_32);
      Desc.Width := Storage_Count (Temp_32);
      Pop32 (Temp_32);
      Desc.Height := Storage_Count (Temp_32);
      Pop8 (Temp_8);
      Desc.Channels := Storage_Count (Temp_8);
      Pop8 (Temp_8);
      pragma Assert (P = Data'First + QOI_HEADER_SIZE);
      if Temp_8 not in Storage_Element (Colorspace_Kind'Enum_Rep (SRGB))
                     | Storage_Element (Colorspace_Kind'Enum_Rep (SRGB_Linear_Alpha))
                     | Storage_Element (Colorspace_Kind'Enum_Rep (Linear))
      then
         Desc := (0, 0, 0, SRGB);
         return;
      end if;
      Desc.Colorspace := Colorspace_Kind'Enum_Val (Temp_8);
   end Get_Desc;

   ------------
   -- Decode --
   ------------

   procedure Decode (Data        :     Storage_Array;
                     Desc        : out QOI_Desc;
                     Output      : out Storage_Array;
                     Output_Size : out Storage_Count)
   is
      P         : Storage_Count;
      Out_Index : Storage_Count := Output'First;

      procedure Pop8 (Result : out Storage_Element) with
        Pre  =>
          P in Data'Range
            and then Data'Last < Storage_Count'Last,
        Post => P = P'Old + 1;
      procedure Push (D      :     Storage_Element) with
        Pre  =>
          Out_Index in Output'Range
            and then Output'Last < Storage_Count'Last
            and then Output (Output'First .. Out_Index - 1)'Initialized,
        Post =>
          Out_Index = Out_Index'Old + 1
            and then Output (Output'First .. Out_Index - 1)'Initialized;

      procedure Pop8 (Result : out Storage_Element) is
      begin
         Result := Data (P);
         P := P + 1;
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
         Desc.Height > Storage_Count'Last / Desc.Width
        or else
         Desc.Channels > Storage_Count'Last / (Desc.Width * Desc.Height)
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

            pragma Loop_Invariant (P >= Data'First);
            pragma Loop_Invariant
              (Out_Index
               = Output'First + Desc.Channels * (Px_Index - Pixel_Index_Range'First));
            pragma Loop_Invariant (Output (Output'First .. Out_Index - 1)'Initialized);

            if Run > 0 then
               Run := Run - 1;
            elsif P <= Last_Chunk then
               Pop8 (B1);

               if (B1 and QOI_MASK_2) = QOI_INDEX then
                  Px := Index (Index_Range (B1 and 2#0011_1111#));

               elsif (B1 and QOI_MASK_3) = QOI_RUN_8 then
                  Run := Run_Range (B1 and 2#0001_1111#);

               elsif (B1 and QOI_MASK_3) = QOI_RUN_16 then
                  Pop8 (B2);
                  Run := Run_Range (B1 and 2#0001_1111#) * 2 ** 8 +
                    Run_Range (B2) + 32;

               elsif (B1 and QOI_MASK_2) = QOI_DIFF_8 then
                  Px.R := Px.R + ((Shift_Right (B1, 4) and 16#03#) - 2);
                  Px.G := Px.G + ((Shift_Right (B1, 2) and 16#03#) - 2);
                  Px.B := Px.B + ((Shift_Right (B1, 0) and 16#03#) - 2);

               elsif (B1 and QOI_MASK_3) = QOI_DIFF_16 then
                  Pop8 (B2);
                  Px.R := Px.R + ((Shift_Right (B1, 0) and 16#1F#) - 16);
                  Px.G := Px.G + ((Shift_Right (B2, 4) and 16#0F#) - 8);
                  Px.B := Px.B + ((Shift_Right (B2, 0) and 16#0F#) - 8);

               elsif (B1 and QOI_MASK_4) = QOI_DIFF_24 then
                  Pop8 (B2);
                  Pop8 (B3);

                  pragma Style_Checks ("M110");
                  Px.R := Px.R + ((Shift_Left (B1 and 16#0F#, 1) or Shift_Right (B2, 7))  - 16);
                  Px.G := Px.G + (Shift_Right (B2 and 16#7C#, 2) - 16);
                  Px.B := Px.B + ((Shift_Left (B2 and 16#03#, 3) or Shift_Right (B3 and 16#E0#, 5)) - 16);
                  Px.A := Px.A + ((Shift_Right (B3, 0) and 16#1F#) - 16);

               elsif (B1 and QOI_MASK_4) = QOI_COLOR then

                  if (B1 and 8) /= 0 then
                     Pop8 (Px.R);
                  end if;
                  if (B1 and 4) /= 0 then
                     Pop8 (Px.G);
                  end if;
                  if (B1 and 2) /= 0 then
                     Pop8 (Px.B);
                  end if;
                  if (B1 and 1) /= 0 then
                     Pop8 (Px.A);
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
