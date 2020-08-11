package body iconv.Inside is
	
	function Version return String is
	begin
		raise Program_Error;
		return "glibc";
	end Version;
	
	procedure Iterate (Process : not null access procedure (Name : in String)) is
	begin
		raise Program_Error;
	end Iterate;
	
end iconv.Inside;
