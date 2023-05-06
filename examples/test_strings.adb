with Ada.Streams;
with Ada.Text_IO;
with System;
with iconv.Generic_Strings;
with iconv.Strings;
procedure Test_Strings is
	use type Ada.Streams.Stream_Element_Array;
begin
	declare -- LATIN1 and UTF-16
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
	end;
	declare -- LATIN1 and JIS
		L1_CENT_SIGN : constant String (1 .. 1) := (1 => Character'Val (16#A2#));
		JIS_CENT_SIGN : constant Ada.Streams.Stream_Element_Array (1 .. 8) :=
			(16#1B#, 16#24#, 16#42#, 16#21#, 16#71#, 16#1B#, 16#28#, 16#42#);
	begin
		declare
			E : iconv.Strings.Encoder := iconv.Strings.To ("ISO-2022-JP");
			pragma Unmodified (E);
		begin
			pragma Assert (
				iconv.Strings.Encode (E, L1_CENT_SIGN) = JIS_CENT_SIGN);
			null;
		end;
		declare
			D : iconv.Strings.Decoder := iconv.Strings.From ("ISO-2022-JP");
			pragma Unmodified (D);
		begin
			pragma Assert (
				iconv.Strings.Decode (D, JIS_CENT_SIGN) = L1_CENT_SIGN);
			null;
		end;
	end;
	declare -- UTF-8 and JIS
		U8_JAPANEASE_A : constant String (1 .. 3) :=
			(Character'Val (16#E3#), Character'Val (16#81#), Character'Val (16#82#));
		JIS_JAPANEASE_A : constant Ada.Streams.Stream_Element_Array (1 .. 8) :=
			(16#1B#, 16#24#, 16#42#, 16#24#, 16#22#, 16#1B#, 16#28#, 16#42#);
		package iconv_UTF8_Strings is
			new iconv.Generic_Strings (Character, String, "UTF-8");
	begin
		declare
			E : iconv_UTF8_Strings.Encoder := iconv_UTF8_Strings.To ("ISO-2022-JP");
		begin
			pragma Assert (
				iconv_UTF8_Strings.Encode (E, U8_JAPANEASE_A) = JIS_JAPANEASE_A);
			iconv_UTF8_Strings.Set_Substitute (E, (0 => Character'Pos ('?')));
			pragma Assert (
				iconv_UTF8_Strings.Encode (E, U8_JAPANEASE_A (1 .. 2)) =
					(Character'Pos ('?'), Character'Pos ('?')));
				-- truncated input
		end;
	end;
	declare -- UTF-16 and JIS
		U16_CENT_SIGN : constant Wide_String (1 .. 1) :=
			(1 => Wide_Character'Val (16#00A2#));
		JIS_CENT_SIGN : constant Ada.Streams.Stream_Element_Array (1 .. 8) :=
			(16#1B#, 16#24#, 16#42#, 16#21#, 16#71#, 16#1B#, 16#28#, 16#42#);
		package iconv_UTF16_Strings is
			new iconv.Generic_Strings (
				Wide_Character,
				Wide_String,
				(case System.Default_Bit_Order is
					when System.High_Order_First => "UTF-16BE",
					when System.Low_Order_First => "UTF-16LE"));
	begin
		declare
			E : iconv_UTF16_Strings.Encoder := iconv_UTF16_Strings.To ("ISO-2022-JP");
			pragma Unmodified (E);
		begin
			pragma Assert (
				iconv_UTF16_Strings.Encode (E, U16_CENT_SIGN) = JIS_CENT_SIGN);
			null;
		end;
		declare
			D : iconv_UTF16_Strings.Decoder := iconv_UTF16_Strings.From ("ISO-2022-JP");
			pragma Unmodified (D);
		begin
			pragma Assert (
				iconv_UTF16_Strings.Decode (D, JIS_CENT_SIGN) = U16_CENT_SIGN);
			null;
		end;
	end;
	-- finish
	Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error.all, "ok");
end Test_Strings;
