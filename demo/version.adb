with Ada.Text_IO;
with iconv;
procedure version is
begin
	Ada.Text_IO.Put_Line (iconv.Version);
end version;
