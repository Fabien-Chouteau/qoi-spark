# qoi-spark
“Quite OK Image” Ada/SPARK implementation

This is based on [QOI](https://qoiformat.org/) format specification V1.

To call the `Encode`/`Decode` procedure you have to provide a large enough
output buffer. If the provided output buffer is not large enough, each
procedure will return with an `Output_Size` of zero. For `Encode` the minimum
size for the output buffer is given by the `Encode_Worst_Case` function based
on the dimensions of the image and the number of channels. For `Decode` you
should use the `Get_Desc` procedure to get the image specification and then the
exact output size will be `Desc.Width * Desc.Height * Desc.Channels`.
