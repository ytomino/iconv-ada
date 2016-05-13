with C.iconv;
with C.string;
package body iconv.Inside is
	use type C.unsigned_int;
	
	function Do_One (
		namescount : C.unsigned_int;
		names : access C.char_const_ptr;
		data : C.void_ptr)
		return C.signed_int;
	pragma Convention (C, Do_One);
	function Do_One (
		namescount : C.unsigned_int;
		names : access C.char_const_ptr;
		data : C.void_ptr)
		return C.signed_int
	is
		Process : access procedure (Name : in String);
		pragma Import (Ada, Process);
		for Process'Address use System.Address (data);
		Names_Array : array (1 .. namescount) of C.char_const_ptr;
		pragma Import (Ada, Names_Array);
		for Names_Array'Address use names.all'Address;
	begin
		for I in 1 .. namescount loop
			declare
				Length : constant Natural := Natural (C.string.strlen (Names_Array (I)));
				Name : String (1 .. Length);
				pragma Import (Ada, Name);
				for Name'Address use Names_Array (I).all'Address;
			begin
				Process (Name);
			end;
		end loop;
		return 0;
	end Do_One;
	
	-- implementation
	
	function Version return String is
		V : constant C.signed_int := C.iconv.qlibiconv_version;
		Major : constant C.unsigned_int := C.Shift_Right (C.unsigned_int (V), 8);
		Minor : constant C.unsigned_int := C.unsigned_int (V) and (2 ** 8 - 1);
		Major_Image : constant String := C.unsigned_int'Image (Major);
		Minor_Image : constant String := C.unsigned_int'Image (Minor);
	begin
		pragma Assert (Major_Image (Major_Image'First) = ' ');
		pragma Assert (Minor_Image (Minor_Image'First) = ' ');
		return Major_Image (Major_Image'First + 1 .. Major_Image'Last)
			& '.' & Minor_Image (Minor_Image'First + 1 .. Minor_Image'Last);
	end Version;
	
	procedure Iterate (Process : not null access procedure (Name : in String)) is
	begin
		C.iconv.iconvlist (Do_One'Access, C.void_ptr (Process'Address));
	end Iterate;
	
end iconv.Inside;
