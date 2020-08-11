-- using glibc provided by Linux From Scratch
private package iconv.Inside is
	pragma Preelaborate;
	
	function Version return String;
	
	procedure Iterate (Process : not null access procedure (Name : in String));
	
end iconv.Inside;
