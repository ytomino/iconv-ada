with Ada.Streams;
with Ada.Text_IO.Text_Streams;
with iconv.Streams;
procedure stream_w is
	use type Ada.Streams.Stream_Element_Offset;
	Std_Input : constant Ada.Text_IO.Text_Streams.Stream_Access :=
		Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Standard_Input.all);
	Std_Output : constant Ada.Text_IO.Text_Streams.Stream_Access :=
		Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Standard_Output.all);
	Internal : aliased constant String := "ISO-2022-JP-3";
	External : aliased constant String := "UTF-8";
	iconv_Output : aliased iconv.Streams.Inout_Type :=
		iconv.Streams.Open (
			Internal => Internal'Access,
			External => External'Access,
			Stream => Std_Output);
	S : Ada.Streams.Stream_Element_Array (1 .. 1);
	Last : Ada.Streams.Stream_Element_Count;
begin
	loop
		Ada.Streams.Read (Std_Input.all, S, Last);
		exit when Last = 0;
		Ada.Streams.Write (iconv.Streams.Stream (iconv_Output).all, S);
	end loop;
end stream_w;
