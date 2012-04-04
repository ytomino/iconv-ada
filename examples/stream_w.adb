with Ada.Streams;
with Ada.Text_IO.Text_Streams;
with iconv.Streams;
procedure stream_w is
	use type Ada.Streams.Stream_Element_Offset;
	Std_Input : constant Ada.Text_IO.Text_Streams.Stream_Access :=
		Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Standard_Input.all);
	Std_Output : constant Ada.Text_IO.Text_Streams.Stream_Access :=
		Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Standard_Output.all);
	Encoding : aliased iconv.Encoding :=
		iconv.Open ("UTF-8", "ISO-2022-JP-3");
	iconv_Output : iconv.Streams.Stream :=
		iconv.Streams.Create (Std_Output, Encoding'Access);
	S : Ada.Streams.Stream_Element_Array (1 .. 1);
	Last : Ada.Streams.Stream_Element_Count;
begin
	loop
		Ada.Streams.Read (Std_Input.all, S, Last);
		exit when Last = 0;
		Ada.Streams.Write (Ada.Streams.Root_Stream_Type'Class (iconv_Output), S);
	end loop;
end stream_w;
