with System;
with Interfaces.C;

package STB.Image is

   function Load (Filename : String;
                  X, Y, Channels_In_File : out Interfaces.C.int;
                  Desired_Channels   : Interfaces.C.int)
                  return System.Address;

   function Write_PNG (Filename : String;
                       W, H, Channels : Interfaces.C.int;
                       Data : System.Address;
                       Len  : Interfaces.C.int)
                       return Interfaces.C.int;

end STB.Image;
