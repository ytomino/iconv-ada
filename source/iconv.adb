pragma Ada_2012;
with Ada.Unchecked_Conversion;
with System.Storage_Elements;
with C.iconv;
with C.string;
with iconv.Inside;
package body iconv is
	use type Ada.Streams.Stream_Element_Offset;
	use type System.Address;
	use type C.signed_int;
	use type C.size_t;
	use type C.unsigned_int;
	
	-- implementation
	
	function Version return String
		renames Inside.Version;
	
	procedure Iterate (Process : not null access procedure (Name : in String))
		renames Inside.Iterate;
	
	procedure Open (
		Object : in out Converter;
		To : String;
		From : String)
	is
		C_To : C.char_array (0 .. To'Length);
		C_From : C.char_array (0 .. From'Length);
		Dummy : C.void_ptr;
		pragma Unreferenced (Dummy);
	begin
		if Handle (Object) /= System.Null_Address then
			raise Status_Error;
		end if;
		Dummy := C.string.memcpy (
			C.void_ptr (C_To'Address),
			C.void_const_ptr (To'Address),
			C_To'Last);
		C_To (C_To'Last) := C.char'Val (0);
		Dummy := C.string.memcpy (
			C.void_ptr (C_From'Address),
			C.void_const_ptr (From'Address),
			C_From'Last);
		C_From (C_From'Last) := C.char'Val (0);
		Open (
			Object,
			To_Code => C_To (0)'Access,
			From_Code => C_From (0)'Access);
	end Open;
	
	function Open (
		To : String;
		From : String)
		return Converter is
	begin
		return Result : Converter do
			Open (
				Result,
				To => To,
				From => From);
		end return;
	end Open;
	
	function Is_Open (Object : Converter) return Boolean is
	begin
		return Handle (Object) /= System.Null_Address;
	end Is_Open;
	
	function Substitute (Object : Converter)
		return Ada.Streams.Stream_Element_Array
	is
		NC_Converter : constant not null access constant Non_Controlled_Converter :=
			Constant_Reference (Object);
	begin
		return NC_Converter.Substitute (1 .. NC_Converter.Substitute_Length);
	end Substitute;
	
	procedure Set_Substitute (
		Object : in out Converter;
		Substitute : in Ada.Streams.Stream_Element_Array)
	is
		NC_Converter : constant not null access Non_Controlled_Converter :=
			Reference (Object);
	begin
		if Substitute'Length > NC_Converter.Substitute'Length then
			raise Constraint_Error;
		end if;
		NC_Converter.Substitute_Length := Substitute'Length;
		NC_Converter.Substitute (1 .. NC_Converter.Substitute_Length) := Substitute;
	end Set_Substitute;
	
	procedure Convert (
		Object : in Converter;
		In_Item : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
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
		Handle : constant System.Address := iconv.Handle (Object);
	begin
		if Handle = System.Null_Address then
			raise Status_Error;
		else
			declare
				In_Pointer : aliased C.char_const_ptr :=
					To_Pointer (In_Item (In_Item'First)'Address);
				In_Size : aliased C.size_t := In_Item'Length;
				Out_Pointer : aliased C.char_ptr :=
					To_Pointer (Out_Item (Out_Item'First)'Address);
				Out_Size : aliased C.size_t := Out_Item'Length;
				errno : C.signed_int;
			begin
				if C.iconv.iconv (
					C.iconv.iconv_t (Handle),
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
							raise Program_Error
								with "iconv failed (errno =" & C.signed_int'Image (errno) & ")";
					end case;
				else
					Status := Fine;
				end if;
				In_Last := In_Item'First
					+ (In_Item'Length - Ada.Streams.Stream_Element_Offset (In_Size))
					- 1;
				Out_Last := Out_Item'First
					+ (Out_Item'Length - Ada.Streams.Stream_Element_Offset (Out_Size))
					- 1;
			end;
		end if;
	end Convert;
	
	procedure Convert (
		Object : in Converter;
		In_Item : in Ada.Streams.Stream_Element_Array;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset)
	is
		In_Index : Ada.Streams.Stream_Element_Offset := In_Item'First;
		Out_Index : Ada.Streams.Stream_Element_Offset := Out_Item'First;
	begin
		loop
			declare
				Status : Error_Status;
				In_Last : Ada.Streams.Stream_Element_Offset;
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
						declare
							Is_Overflow : Boolean;
						begin
							Put_Substitute (
								Object,
								Out_Item (Out_Index .. Out_Item'Last),
								Out_Index,
								Is_Overflow);
							if Is_Overflow then
								raise Constraint_Error;
							end if;
						end;
						Out_Index := Out_Index + 1;
						In_Index := In_Index + 1;
				end case;
				exit when In_Index > In_Item'Last;
			end;
		end loop;
	end Convert;
	
	function Open (Encoded, Decoded : String) return Encoding is
	begin
		return Result : Encoding do
			Open (
				Result.Writing,
				To => Encoded,
				From => Decoded);
			Open (
				Result.Reading,
				To => Decoded,
				From => Encoded);
		end return;
	end Open;
	
	function Is_Open (Object : Encoding) return Boolean is
	begin
		return Is_Open (Object.Reading);
	end Is_Open;
	
	package body Controlled is
		
		procedure Open (
			Object : in out Converter;
			To_Code, From_Code : not null access constant C.char)
		is
			Invalid : constant System.Address := System.Storage_Elements.To_Address (
				System.Storage_Elements.Integer_Address'Mod (-1));
			Handle : constant System.Address :=
				System.Address (C.iconv.iconv_open (To_Code, From_Code));
		begin
			if Handle = Invalid then
				raise Name_Error;
			end if;
			Object.Handle := Handle;
			-- about "To"
			Object.Data.Substitute_Length := 0;
		end Open;
		
		function Handle (Object : Converter) return System.Address is
		begin
			return Object.Handle;
		end Handle;
		
		function Reference (Object : in out Converter)
			return not null access Non_Controlled_Converter is
		begin
			if Object.Handle = System.Null_Address then
				raise Status_Error;
			end if;
			return Object.Data'Unchecked_Access;
		end Reference;
		
		function Constant_Reference (Object : Converter)
			return not null access constant Non_Controlled_Converter is
		begin
			if Object.Handle = System.Null_Address then
				raise Status_Error;
			end if;
			return Object.Data'Unchecked_Access;
		end Constant_Reference;
		
		procedure Finalize (Object : in out Converter) is
		begin
			if C.iconv.iconv_close (C.iconv.iconv_t (Object.Handle)) /= 0 then
				null; -- raise Status_Error;
			end if;
		end Finalize;
		
	end Controlled;
	
	procedure Put_Substitute (
		Object : in Converter;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Is_Overflow : out Boolean)
	is
		NC_Converter : constant not null access constant Non_Controlled_Converter :=
			Constant_Reference (Object);
	begin
		Out_Last := Out_Item'First - 1;
		Is_Overflow := Out_Item'Length < NC_Converter.Substitute_Length;
		if not Is_Overflow then
			Out_Last := Out_Last + NC_Converter.Substitute_Length;
			Out_Item (Out_Item'First .. Out_Last) :=
				NC_Converter.Substitute (1 .. NC_Converter.Substitute_Length);
		end if;
	end Put_Substitute;
	
end iconv;
