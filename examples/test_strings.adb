with Ada.Streams;
with Ada.Text_IO;
with iconv.Strings;
procedure Test_Strings is
	use type Ada.Streams.Stream_Element_Array;
	L1_A : constant String := "A";
	U16BE_A : constant Ada.Streams.Stream_Element_Array := (0, 16#41#);
begin
	declare
		D : iconv.Strings.Decoder := iconv.Strings.From ("UTF-16BE");
	begin
		pragma Assert (iconv.Strings.Decode (D, U16BE_A) = L1_A);
		null;
	end;
	declare
		E : iconv.Strings.Encoder := iconv.Strings.To ("UTF-16BE");
	begin
		pragma Assert (iconv.Strings.Encode (E, L1_A) = U16BE_A);
		null;
	end;
	pragma Debug (Ada.Text_IO.Put_Line ("OK"));
end Test_Strings;
