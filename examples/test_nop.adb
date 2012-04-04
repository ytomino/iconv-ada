-- test for uninitialized objects
with Ada.Text_IO;
with iconv;
procedure test_nop is
begin
	declare
		C : iconv.Converter;
	begin
		begin
			declare
				S : String := iconv.Convert (C, "");
				pragma Unreferenced (S);
			begin
				raise Program_Error; -- Status_Error shold be raised
			end;
		exception
			when iconv.Status_Error => null;
		end;
	end;
	declare
		E : iconv.Encoding;
	begin
		begin
			declare
				S : String := iconv.Encode (E, "");
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
