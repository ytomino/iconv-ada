-- test for uninitialized objects
with Ada.Streams;
with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with iconv;
with iconv.Streams;
procedure test_nop is
	use type Ada.Streams.Stream_Element_Offset;
begin
	declare
		C : iconv.Converter;
		pragma Unmodified (C);
	begin
		begin
			declare
				In_Last : Ada.Streams.Stream_Element_Offset;
				Out_Item : Ada.Streams.Stream_Element_Array (0 .. 0);
				Out_Last : Ada.Streams.Stream_Element_Offset;
				Status : iconv.Substituting_Status_Type;
			begin
				iconv.Convert (
					C,
					(0 .. -1 => <>),
					In_Last,
					Out_Item,
					Out_Last,
					True,
					Status);
				raise Program_Error; -- Status_Error shold be raised
			end;
		exception
			when iconv.Status_Error => null;
		end;
	end;
	declare
		E : aliased iconv.Encoding;
	begin
		begin
			declare
				S : iconv.Streams.Inout_Type := iconv.Streams.Create (
					Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Standard_Output.all),
					E'Access);
				pragma Unreferenced (S);
			begin
				raise Program_Error; -- Status_Error shold be raised
			end;
		exception
			when iconv.Status_Error => null;
		end;
	end;
	Ada.Text_IO.Put_Line ("OK");
end test_nop;
