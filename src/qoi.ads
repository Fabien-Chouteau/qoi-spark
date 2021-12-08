with Interfaces; use Interfaces;

with System.Storage_Elements; use System.Storage_Elements;

package QOI
  with SPARK_Mode
is

   type Pixels is array (Storage_Count range <>) of Unsigned_8
     with Default_Component_Value => 0;
   type Pixels_Access is access Pixels;

   type Data is array (Storage_Count range  <>) of Unsigned_8
     with Default_Component_Value => 0;
   type Data_Access is access Data;

   type Colorspace_Kind is (SRGB, SRGB_Linear_Alpha, Linear);

   type QOI_Desc is record
      Width, Height : Storage_Count;
      Channels      : Storage_Count;
      Colorspace    : Colorspace_Kind;
   end record;

   QOI_HEADER_SIZE : constant := 14;
   QOI_PADDING     : constant := 4;

   function Valid_Size (Desc : QOI_Desc) return Boolean is
     (Desc.Width in 1 .. Storage_Count (Integer_32'Last)
        and then Desc.Height in 1 .. Storage_Count (Integer_32'Last)
        and then Desc.Channels in 3 .. 4
        and then Desc.Width <= Storage_Count'Last / Desc.Height
        and then Desc.Channels + 1 <= Storage_Count'Last / (Desc.Width * Desc.Height)
        and then
          QOI_HEADER_SIZE + QOI_PADDING
          <= Storage_Count'Last - (Desc.Width * Desc.Height * (Desc.Channels + 1)));

   function Encode_Worst_Case (Desc : QOI_Desc) return Storage_Count
   is (Desc.Width * Desc.Height * (Desc.Channels + 1) +
         QOI_HEADER_SIZE + QOI_PADDING)
   with
     Pre  => Valid_Size (Desc),
     Post => Encode_Worst_Case'Result >= QOI_HEADER_SIZE + QOI_PADDING;

   procedure Encode (Pix         :     Storage_Array;
                     Desc        :     QOI_Desc;
                     Output      : out Storage_Array;
                     Output_Size : out Storage_Count)
   with
     Relaxed_Initialization => Output,
     Pre  =>
       Valid_Size (Desc)
         and then Output'First >= 0
         and then Output'Last < Storage_Count'Last
         and then Output'Length >= Encode_Worst_Case (Desc)
         and then Pix'First >= 1
         and then Pix'Length = (Desc.Width * Desc.Height * Desc.Channels),
     Post => Output (Output'First .. Output'First - 1 + Output_Size)'Initialized;

   procedure Get_Desc (Data :     Storage_Array;
                       Desc : out QOI_Desc)
   with
     Pre =>
       Data'First >= 0
         and then Data'Last < Storage_Count'Last;

   procedure Decode (Data        :     Storage_Array;
                     Desc        : out QOI_Desc;
                     Output      : out Storage_Array;
                     Output_Size : out Storage_Count)
   with
     Relaxed_Initialization => Output,
     Pre  =>
       Output'First >= 0
         and then Output'Last < Storage_Count'Last
         and then Data'First >= 0
         and then Data'Last < Storage_Count'Last
         and then Data'Length >= QOI_HEADER_SIZE + QOI_PADDING,
     Post =>
       (if Output_Size /= 0
        then
          Desc.Height <= Storage_Count'Last / Desc.Width
            and then
          Desc.Channels <= Storage_Count'Last / (Desc.Width * Desc.Height)
            and then
          Output_Size = Desc.Width * Desc.Height * Desc.Channels
            and then
          Output (Output'First .. Output'First - 1 + Output_Size)'Initialized);

private

   for Colorspace_Kind'Size use 8;
   for Colorspace_Kind use (SRGB              => 16#00#,
                            SRGB_Linear_Alpha => 16#01#,
                            Linear            => 16#0f#);
end QOI;
