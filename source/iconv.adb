with Ada.Unchecked_Conversion;
with C.iconv;
with C.string;
package body iconv is
	use type System.Address;
	use type C.signed_int;
	use type C.size_t;
	
	procedure Convert (
		Object : in Converter;
		In_Item : in String;
		In_Last : out Natural;
		Out_Item : out String;
		Out_Last : out Natural;
		Status : out Error_Status)
	is
		pragma Suppress (All_Checks);
		-- C.char_ptr will be mapped access C.char / Interfaces.C.chars_ptr,
		-- There is no shared convert way from System.Address (access Character)
		-- to these two types, except Unchecked_Conversion.
		function To_Pointer is
			new Ada.Unchecked_Conversion (System.Address, C.char_ptr);
		function To_Pointer is
			new Ada.Unchecked_Conversion (System.Address, C.char_const_ptr);
		In_Pointer : aliased C.char_const_ptr :=
			To_Pointer (In_Item (In_Item'First)'Address);
		In_Size : aliased C.size_t := In_Item'Length;
		Out_Pointer : aliased C.char_ptr :=
			To_Pointer (Out_Item (Out_Item'First)'Address);
		Out_Size : aliased C.size_t := Out_Item'Length;
		errno : C.signed_int;
	begin
		if C.iconv.iconv (
			C.iconv.iconv_t (Object.Handle),
			In_Pointer'Access,
			In_Size'Access,
			Out_Pointer'Access,
			Out_Size'Access) = C.size_t'Last
		then
			errno := C.errno.errno;
			case errno is
				when C.errno.E2BIG =>
					raise Constraint_Error;
				when C.errno.EINVAL
					-- dirty hack for OSX
					| -1601902748 | -1603246236 | -1609021596 | -1610246300
				=>
					Status := Invalid;
				when C.errno.EILSEQ =>
					Status := Illegal_Sequence;
				when others =>
					raise Program_Error with "iconv.Convert_Single (" & errno'Img & ")";
			end case;
		else
			Status := Fine;
		end if;
		In_Last := In_Item'First + (In_Item'Length - Natural (In_Size)) - 1;
		Out_Last := Out_Item'First + (Out_Item'Length - Natural (Out_Size)) - 1;
	end Convert;

	procedure Convert (
		Object : in Converter;
		In_Item : in String;
		Out_Item : out String;
		Out_Last : out Natural;
		Invalid_Character : in Character := '?')
	is
		In_Index : Natural := In_Item'First;
		Out_Index : Natural := Out_Item'First;
	begin
		loop
			declare
				Status : Error_Status;
				In_Last : Natural;
			begin
				Convert (
					Object,
					In_Item (In_Index .. In_Item'Last),
					In_Last,
					Out_Item (Out_Index .. Out_Item'Last),
					Out_Last,
					Status);
				In_Index := In_Last + 1;
				Out_Index := Out_Last + 1;
				case Status is
					when Fine =>
						null;
					when Invalid | Illegal_Sequence =>
						Out_Item (Out_Index) := Invalid_Character;
						Out_Index := Out_Index + 1;
						In_Index := In_Index + 1;
				end case;
				exit when In_Index > In_Item'Last;
			end;
		end loop;
	end Convert;

	function Convert (Object : Converter; S : String) return String is
		Result : aliased String (1 .. Max_Length_Of_Single_Character * S'Length);
		Last : Natural;
	begin
		Convert (Object, S, Result, Last);
		return Result (1 .. Last);
	end Convert;

	function Decode (Object : Encoding; S : String) return String is
	begin
		return Convert (Object.Reading, S);
	end Decode;
	
	function Encode (Object : Encoding; S : String) return String is
	begin
		return Convert (Object.Writing, S);
	end Encode;
	
	procedure Finalize (Object : in out Converter) is
	begin
		if C.iconv.iconv_close (C.iconv.iconv_t (Object.Handle)) /= 0 then
			raise Status_Error;
		end if;
	end Finalize;
	
	-- local
	function Do_One (
		namescount : C.unsigned_int;
		names : access C.char_const_ptr;
		data : C.void_ptr) return C.signed_int;
	pragma Convention (C, Do_One);
	function Do_One (
		namescount : C.unsigned_int;
		names : access C.char_const_ptr;
		data : C.void_ptr) return C.signed_int
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

	procedure Iterate (Process : not null access procedure (Name : in String)) is
	begin
		C.iconv.iconvlist (Do_One'Access, C.void_ptr (Process'Address));
	end Iterate;
	
	-- local
	procedure Open (
		Object : in out Converter;
		To_Code, From_Code : not null access constant C.char)
	is
		Invalid : constant System.Address :=
			System'To_Address (System.Memory_Size - 1);
		Handle : constant System.Address :=
			System.Address (C.iconv.iconv_open (To_Code, From_Code));
	begin
		if Handle = Invalid then
			raise Name_Error;
		end if;
		Object.Handle := Handle;
	end Open;
	
	function Open (To_Code, From_Code : String) return Converter is
		Z_To_Code : C.char_array (0 .. To_Code'Length);
		Z_From_Code : C.char_array (0 .. From_Code'Length);
		Dummy : C.void_ptr;
		pragma Unreferenced (Dummy);
	begin
		Dummy := C.string.memmove (
			C.void_ptr (Z_To_Code (0)'Address),
			C.void_const_ptr (To_Code (To_Code'First)'Address),
			Z_To_Code'Last);
		Z_To_Code (Z_To_Code'Last) := C.char'Val (0);
		Dummy := C.string.memmove (
			C.void_ptr (Z_From_Code (0)'Address),
			C.void_const_ptr (From_Code (From_Code'First)'Address),
			Z_From_Code'Last);
		Z_From_Code (Z_From_Code'Last) := C.char'Val (0);
		return Result : Converter := (Ada.Finalization.Limited_Controlled with
			Handle => <>)
		do
			Open (
				Result,
				To_Code => Z_To_Code (0)'Access,
				From_Code => Z_From_Code (0)'Access);
		end return;
	end Open;
	
	function Open (Encoded, Decoded : String) return Encoding is
		Z_Encoded : C.char_array (0 .. Encoded'Length);
		Z_Decoded : C.char_array (0 .. Decoded'Length);
		Dummy : C.void_ptr;
		pragma Warnings (Off, Dummy);
	begin
		Dummy := C.string.memmove (
			C.void_ptr (Z_Encoded (0)'Address),
			C.void_const_ptr (Encoded (Encoded'First)'Address),
			Z_Encoded'Last);
		Z_Encoded (Z_Encoded'Last) := C.char'Val (0);
		Dummy := C.string.memmove (
			C.void_ptr (Z_Decoded (0)'Address),
			C.void_const_ptr (Decoded (Decoded'First)'Address),
			Z_Decoded'Last);
		Z_Decoded (Z_Decoded'Last) := C.char'Val (0);
		return Result : Encoding := (
			Writing => (Ada.Finalization.Limited_Controlled with
				Handle => <>),
			Reading => (Ada.Finalization.Limited_Controlled with
				Handle => <>))
		do
			Open (
				Result.Writing,
				To_Code => Z_Encoded (0)'Access,
				From_Code => Z_Decoded (0)'Access);
			Open (
				Result.Reading,
				To_Code => Z_Decoded (0)'Access,
				From_Code => Z_Encoded (0)'Access);
		end return;
	end Open;
	
end iconv;
