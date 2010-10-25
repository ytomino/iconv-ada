with Ada.Streams;
with Ada.Text_IO.Text_Streams;
with iconv.Streams;
procedure stream_r is
	use type Ada.Streams.Stream_Element_Offset;
	Std_Input : constant Ada.Text_IO.Text_Streams.Stream_Access :=
		Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Standard_Input.all);
	Std_Output : constant Ada.Text_IO.Text_Streams.Stream_Access :=
		Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Standard_Output.all);
	Encoding : aliased iconv.Encoding :=
		iconv.Open ("ISO-2022-JP-3", "UTF-8");
	pragma Warnings (Off, Encoding); -- suppress warning by bug of gcc-4.5.1
	iconv_Input : iconv.Streams.Stream :=
		iconv.Streams.Create (Std_Input, Encoding'Access);
	S : Ada.Streams.Stream_Element_Array (1 .. 1);
	Last : Ada.Streams.Stream_Element_Count;
begin
	loop
		Ada.Streams.Read (Ada.Streams.Root_Stream_Type'Class (iconv_Input), S, Last);
		exit when Last = 0;
		Ada.Streams.Write (Std_Output.all, S);
	end loop;
end stream_r;
