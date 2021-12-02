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

   function Encode_Worst_Case (Desc : QOI_Desc) return Storage_Count
   is (Desc.Width * Desc.Height * (Desc.Channels + 1) +
         QOI_HEADER_SIZE + QOI_PADDING);

   procedure Encode (Pix         :     Storage_Array;
                     Desc        :     QOI_Desc;
                     Output      : out Storage_Array;
                     Output_Size : out Storage_Count)
     with Pre => Desc.Channels in 3 .. 4
     and then Output'Length >= Encode_Worst_Case (Desc)
     and then Pix'Length = (Desc.Width * Desc.Height * Desc.Channels);

   procedure Get_Desc (Data :     Storage_Array;
                       Desc : out QOI_Desc);

   procedure Decode (Data        :     Storage_Array;
                     Desc        : out QOI_Desc;
                     Output      : out Storage_Array;
                     Output_Size : out Storage_Count);

private

   for Colorspace_Kind'Size use 8;
   for Colorspace_Kind use (SRGB              => 16#00#,
                            SRGB_Linear_Alpha => 16#01#,
                            Linear            => 16#0f#);
end QOI;
