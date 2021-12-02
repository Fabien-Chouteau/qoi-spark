# qoi-ada
“Quite OK Image” Ada implementation

This is based on [QOI](https://github.com/phoboslab/qoi) commit
`fda5167d76d05de67b821c787824c8d177fd22d8`. The QOI specification is likely to
change soon so it might become incompatible...

To call the `Encode`/`Decode` procedure you have to provide a large enough
output buffer. If the provided output buffer is not large enough, each
procedure will return with an `Output_Size` of zero. For `Encode` the minimum
size for the output buffer is given by the `Encode_Worst_Case` function based
on the dimensions of the image and the number of channels. For `Decode` you
should use the `Get_Desc` procedure to get the image specification and then the
exact output size will be `Desc.Width * Desc.Height * Desc.Channels`.
