with Ada.Unchecked_Conversion;
with C.iconv;
with C.string;
package body iconv.Inside is
	use type C.unsigned_int;
	use type C.size_t;
	
	function Do_One (
		namescount : C.unsigned_int;
		names : access C.char_const_ptr;
		data : C.void_ptr)
		return C.signed_int
		with Convention => C;
	
	function Do_One (
		namescount : C.unsigned_int;
		names : access C.char_const_ptr;
		data : C.void_ptr)
		return C.signed_int is
	begin
		if namescount > 0 then
			declare
				type Process_Type is access procedure (Name : in String);
				function To_Process is
					new Ada.Unchecked_Conversion (C.void_ptr, Process_Type);
				type char_const_ptr_arrayN is
					array (0 .. C.size_t (namescount) - 1) of C.char_const_ptr
					with Convention => C;
				type char_const_ptr_arrayN_const_ptr is
					access constant char_const_ptr_arrayN
					with Convention => C;
				function To_char_const_ptr_arrayN_const_ptr is
					new Ada.Unchecked_Conversion (
						C.char_const_ptr_ptr,
						char_const_ptr_arrayN_const_ptr);
				Names_Array : char_const_ptr_arrayN
					renames To_char_const_ptr_arrayN_const_ptr (names).all;
			begin
				for I in 0 .. C.size_t (namescount) - 1 loop
					declare
						Length : constant Natural :=
							Natural (C.string.strlen (Names_Array (I)));
						Name : String (1 .. Length);
						for Name'Address use Names_Array (I).all'Address;
					begin
						To_Process (data) (Name);
					end;
				end loop;
			end;
		end if;
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
		type Process_Type is access procedure (Name : in String);
		function To_void_ptr is
			new Ada.Unchecked_Conversion (Process_Type, C.void_ptr);
	begin
		C.iconv.iconvlist (Do_One'Access, To_void_ptr (Process));
	end Iterate;
	
end iconv.Inside;
