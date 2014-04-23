with Ada.Streams;
with Ada.Text_IO.Text_Streams;
with iconv.Streams;
procedure stream_w is
	use type Ada.Streams.Stream_Element_Offset;
	Std_Input : constant Ada.Text_IO.Text_Streams.Stream_Access :=
		Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Standard_Input.all);
	Std_Output : constant Ada.Text_IO.Text_Streams.Stream_Access :=
		Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Standard_Output.all);
	iconv_Converter : aliased iconv.Converter :=
		iconv.Open (
			To => "UTF-8",
			From => "ISO-2022-JP-3");
	iconv_Output : aliased iconv.Streams.Out_Type :=
		iconv.Streams.Open (
			iconv_Converter,
			Stream => Std_Output);
	S : Ada.Streams.Stream_Element_Array (1 .. 1);
	Last : Ada.Streams.Stream_Element_Count;
begin
	loop
		Ada.Streams.Read (Std_Input.all, S, Last);
		exit when Last = 0;
		Ada.Streams.Write (iconv.Streams.Stream (iconv_Output).all, S);
	end loop;
	iconv.Streams.Finish (iconv_Output);
end stream_w;
