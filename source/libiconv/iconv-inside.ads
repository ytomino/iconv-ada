-- using libiconv
private package iconv.Inside is
	pragma Preelaborate;
	pragma Linker_Options ("-liconv");
	
	function Version return String;
	
	procedure Iterate (Process : not null access procedure (Name : in String));
	
end iconv.Inside;
