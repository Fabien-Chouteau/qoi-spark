with System;
with Interfaces.C;
with Interfaces.C.Strings;

with System.Storage_Elements; use System.Storage_Elements;

with QOI;

package Reference_QOI is

   type Ref_Desc is record
      width      : Interfaces.C.unsigned;
      height     : Interfaces.C.unsigned;
      channels   : Interfaces.C.char;
	   colorspace : Interfaces.C.char;
   end record
     with Convention => C;

   type Ref_Desc_Acc is access all Ref_Desc;
   function Encode (Data     : System.Address;
                    Desc     : not null Ref_Desc_Acc;
                    Out_Len  : not null access Interfaces.C.int)
                    return System.Address;
   pragma Import (C, Encode, "qoi_encode");

   function Check_Encode (Pix    : Storage_Array;
                          Desc   : QOI.QOI_Desc;
                          Output : Storage_Array)
                          return Boolean;

   function Decode (Data     : System.Address;
                    Size     : Interfaces.C.int;
                    Desc     : not null Ref_Desc_Acc;
                    Channels : Interfaces.C.int)
                    return System.Address;
   pragma Import (C, Decode, "qoi_decode");

   function Check_Decode (Data     : Storage_Array;
                          Out_Desc : QOI.QOI_Desc;
                          Out_Data : Storage_Array)
                          return Boolean;

end Reference_QOI;
