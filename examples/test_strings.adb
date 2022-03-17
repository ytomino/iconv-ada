with Ada.Streams;
with Ada.Text_IO;
with iconv.Strings;
procedure Test_Strings is
	use type Ada.Streams.Stream_Element_Array;
	L1_A : constant String := "A";
	U16BE_A : constant Ada.Streams.Stream_Element_Array (1 .. 2) := (0, 16#41#);
	U16BE_JAPANEASE_A : constant Ada.Streams.Stream_Element_Array (1 .. 2) :=
		(16#30#, 16#42#);
begin
	declare
		D : iconv.Strings.Decoder := iconv.Strings.From ("UTF-16BE");
	begin
		pragma Assert (iconv.Strings.Decode (D, U16BE_A) = L1_A);
		pragma Assert (iconv.Strings.Decode (D, U16BE_JAPANEASE_A) = "");
		pragma Assert (
			iconv.Strings.Decode (D, U16BE_A & U16BE_JAPANEASE_A & U16BE_A) = L1_A & L1_A);
		iconv.Strings.Set_Substitute (D, (0 => Character'Pos ('?')));
		pragma Assert (
			iconv.Strings.Decode (D, U16BE_A & U16BE_JAPANEASE_A & U16BE_A) =
			L1_A & "?" & L1_A);
	end;
	declare
		E : iconv.Strings.Encoder := iconv.Strings.To ("UTF-16BE");
		pragma Unmodified (E);
	begin
		pragma Assert (iconv.Strings.Encode (E, L1_A) = U16BE_A);
		null;
	end;
	-- finish
	Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error.all, "ok");
end Test_Strings;
