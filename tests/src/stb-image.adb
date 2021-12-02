with Interfaces.C.Strings;

package body STB.Image is

   ----------
   -- Load --
   ----------

   function Load (Filename : String;
                  X, Y, Channels_In_File : out Interfaces.C.int;
                  Desired_Channels : Interfaces.C.int)
                  return System.Address
   is
      function C_Load (Filename : Interfaces.C.Strings.chars_ptr;
                       X, Y, Comp : access Interfaces.C.int;
                       Req_Comp : Interfaces.C.int)
                       return System.Address;
      pragma Import (C, C_Load, "stbi_load");

      Filename_C : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (Filename);

      Result : System.Address;

      X_A, Y_A, Comp_A : aliased Interfaces.C.int;
   begin

      Result := C_Load (Filename_C, X_A'Access, Y_A'Access,
                        Comp_A 'Access, Desired_Channels);

      Interfaces.C.Strings.Free (Filename_C);

      X := X_A;
      Y := Y_A;
      Channels_In_File := Comp_A;
      return Result;
   end Load;

   ---------------
   -- Write_PNG --
   ---------------

   function Write_PNG (Filename : String;
                       W, H, Channels : Interfaces.C.int;
                       Data : System.Address;
                       Len  : Interfaces.C.int)
                       return Interfaces.C.int
   is

      function C_Write (Filename : Interfaces.C.Strings.chars_ptr;
                        W, H, Comp : Interfaces.C.int;
                        Data : System.Address;
                        Len  : Interfaces.C.int)
                        return Interfaces.C.int;
      pragma Import (C, C_Write, "stbi_write_png");

      Filename_C : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String (Filename);
      Result : Interfaces.C.int;
   begin
      Result := C_Write (Filename_C, W, H, Channels, Data, Len);
      Interfaces.C.Strings.Free (Filename_C);
      return Result;
   end Write_PNG;

end STB.Image;
