with iconv;
with Ada.Text_IO; use Ada.Text_IO;
procedure list is
	procedure Process (Name : in String) is
	begin
		Put_Line (Name);
	end Process;
begin
	iconv.Iterate (Process'Access);
end list;
