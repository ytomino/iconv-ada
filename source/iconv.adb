pragma Ada_2012;
with System.Address_To_Access_Conversions;
with System.Storage_Elements;
with C.errno;
with C.iconv;
with C.string;
with iconv.Inside;
package body iconv is
	use type Ada.Streams.Stream_Element_Offset;
	use type System.Address;
	use type C.signed_int;
	use type C.size_t;
	use type C.unsigned_int;
	
	package char_ptr_Conv is
		new System.Address_To_Access_Conversions (C.char);
	
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
		pragma Check (Dynamic_Predicate,
			not Is_Open (Object) or else raise Status_Error);
	begin
		Do_Open (
			Object,
			To => To,
			From => From);
	end Open;
	
	function Open (
		To : String;
		From : String)
		return Converter is
	begin
		return Result : Converter do
			Do_Open (
				Result,
				To => To,
				From => From);
		end return;
	end Open;
	
	function Is_Open (Object : Converter) return Boolean is
	begin
		return Handle (Object) /= System.Null_Address;
	end Is_Open;
	
	function Min_Size_In_From_Stream_Elements (
		Object : Converter)
		return Ada.Streams.Stream_Element_Offset
	is
		pragma Check (Dynamic_Predicate,
			Is_Open (Object) or else raise Status_Error);
		NC_Converter : constant not null access constant Non_Controlled_Converter :=
			Constant_Reference (Object);
	begin
		return NC_Converter.Min_Size_In_From_Stream_Elements;
	end Min_Size_In_From_Stream_Elements;
	
	function Substitute (
		Object : Converter)
		return Ada.Streams.Stream_Element_Array
	is
		pragma Check (Dynamic_Predicate,
			Is_Open (Object) or else raise Status_Error);
		NC_Converter : constant not null access constant Non_Controlled_Converter :=
			Constant_Reference (Object);
	begin
		return NC_Converter.Substitute (1 .. NC_Converter.Substitute_Length);
	end Substitute;
	
	procedure Set_Substitute (
		Object : in out Converter;
		Substitute : in Ada.Streams.Stream_Element_Array)
	is
		pragma Check (Dynamic_Predicate,
			Is_Open (Object) or else raise Status_Error);
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
		Finish : in Boolean;
		Status : out Subsequence_Status_Type)
	is
		Continuing_Status : Continuing_Status_Type;
		Finishing_Status : Finishing_Status_Type;
	begin
		Convert (
			Object, -- Status_Error would be raised if Object is not open
			In_Item,
			In_Last,
			Out_Item,
			Out_Last,
			Status => Continuing_Status);
		Status := Subsequence_Status_Type (Continuing_Status);
		if Finish and then Status = Success and then In_Last = In_Item'Last then
			Convert (
				Object,
				Out_Item (Out_Last + 1 .. Out_Item'Last),
				Out_Last,
				Finish => True,
				Status => Finishing_Status);
			Status := Subsequence_Status_Type (Finishing_Status);
		end if;
	end Convert;
	
	procedure Convert (
		Object : in Converter;
		In_Item : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Status : out Continuing_Status_Type)
	is
		pragma Check (Dynamic_Predicate,
			Is_Open (Object) or else raise Status_Error);
		pragma Suppress (All_Checks);
		Handle : constant System.Address := iconv.Handle (Object);
		In_Pointer : aliased C.char_const_ptr :=
			C.char_const_ptr (
				char_ptr_Conv.To_Pointer (In_Item (In_Item'First)'Address));
		In_Size : aliased C.size_t := In_Item'Length;
		Out_Pointer : aliased C.char_ptr :=
			C.char_ptr (
				char_ptr_Conv.To_Pointer (Out_Item (Out_Item'First)'Address));
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
					Status := Overflow;
				when C.errno.EINVAL
					-- dirty hack for OSX
					| -1601902748 | -1603246236 | -1609021596 | -1610246300
				=>
					Status := Truncated;
				when C.errno.EILSEQ =>
					Status := Illegal_Sequence;
				when others =>
					raise Use_Error
						with "iconv failed (errno =" & C.signed_int'Image (errno) & ")";
			end case;
		else
			Status := Success;
		end if;
		In_Last := In_Item'First
			+ (In_Item'Length - Ada.Streams.Stream_Element_Offset (In_Size))
			- 1;
		Out_Last := Out_Item'First
			+ (Out_Item'Length - Ada.Streams.Stream_Element_Offset (Out_Size))
			- 1;
	end Convert;
	
	procedure Convert (
		Object : in Converter;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Finish : in True_Only;
		Status : out Finishing_Status_Type)
	is
		pragma Unreferenced (Finish);
		pragma Check (Dynamic_Predicate,
			Is_Open (Object) or else raise Status_Error);
		Handle : constant System.Address := iconv.Handle (Object);
		Out_Pointer : aliased C.char_ptr :=
			C.char_ptr (char_ptr_Conv.To_Pointer (Out_Item'Address));
		Out_Size : aliased C.size_t := Out_Item'Length;
		errno : C.signed_int;
	begin
		if C.iconv.iconv (
			C.iconv.iconv_t (Handle),
			C.char_const_ptr_ptr'(null),
			null,
			Out_Pointer'Access,
			Out_Size'Access) = C.size_t'Last
		then
			errno := C.errno.errno;
			case errno is
				when C.errno.E2BIG =>
					Status := Overflow;
				when others => -- unknown
					raise Use_Error
						with "iconv failed (errno =" & C.signed_int'Image (errno) & ")";
			end case;
		else
			Status := Finished;
		end if;
		Out_Last := Out_Item'First
			+ (Out_Item'Length - Ada.Streams.Stream_Element_Offset (Out_Size))
			- 1;
	end Convert;
	
	procedure Convert (
		Object : in Converter;
		In_Item : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Finish : in True_Only;
		Status : out Status_Type) is
	begin
		In_Last := In_Item'First - 1;
		Out_Last := Out_Item'First - 1;
		loop
			declare
				Subsequence_Status : Subsequence_Status_Type;
			begin
				Convert (
					Object, -- Status_Error would be raised if Object is not open
					In_Item (In_Last + 1 .. In_Item'Last),
					In_Last,
					Out_Item (Out_Last + 1 .. Out_Item'Last),
					Out_Last,
					Finish => Finish,
					Status => Subsequence_Status);
				pragma Assert (Subsequence_Status in
					Subsequence_Status_Type (Status_Type'First) ..
					Subsequence_Status_Type (Status_Type'Last));
				case Status_Type (Subsequence_Status) is
					when Finished =>
						Status := Finished;
						return;
					when Success =>
						Status := Success;
						return;
					when Overflow =>
						Status := Overflow;
						return;
					when Illegal_Sequence =>
						declare
							Is_Overflow : Boolean;
						begin
							Put_Substitute (
								Object,
								Out_Item (Out_Last + 1 .. Out_Item'Last),
								Out_Last,
								Is_Overflow);
							if Is_Overflow then
								Status := Overflow;
								return; -- wait a next try
							end if;
						end;
						declare
							NC_Converter : constant
								not null access constant Non_Controlled_Converter :=
								Constant_Reference (Object);
							New_Last : Ada.Streams.Stream_Element_Offset :=
								In_Last + NC_Converter.Min_Size_In_From_Stream_Elements;
						begin
							if New_Last > In_Item'Last
								or else New_Last < In_Last -- overflow
							then
								New_Last := In_Item'Last;
							end if;
							In_Last := New_Last;
						end;
				end case;
			end;
		end loop;
	end Convert;
	
	procedure Convert (
		Object : in Converter;
		In_Item : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Finish : in True_Only;
		Status : out Substituting_Status_Type) is
	begin
		In_Last := In_Item'First - 1;
		Out_Last := Out_Item'First - 1;
		loop
			declare
				Subsequence_Status : Subsequence_Status_Type;
			begin
				Convert (
					Object, -- Status_Error would be raised if Object is not open
					In_Item (In_Last + 1 .. In_Item'Last),
					In_Last,
					Out_Item (Out_Last + 1 .. Out_Item'Last),
					Out_Last,
					Finish => Finish,
					Status => Subsequence_Status);
				pragma Assert (Subsequence_Status in
					Subsequence_Status_Type (Status_Type'First) ..
					Subsequence_Status_Type (Status_Type'Last));
				case Status_Type (Subsequence_Status) is
					when Finished =>
						Status := Finished;
						return;
					when Success =>
						Status := Success;
						return;
					when Overflow =>
						Status := Overflow;
						return;
					when Illegal_Sequence =>
						declare
							Is_Overflow : Boolean;
						begin
							Put_Substitute (
								Object,
								Out_Item (Out_Last + 1 .. Out_Item'Last),
								Out_Last,
								Is_Overflow);
							if Is_Overflow then
								raise Constraint_Error;
							end if;
						end;
						declare
							NC_Converter : constant
								not null access constant Non_Controlled_Converter :=
								Constant_Reference (Object);
							New_Last : Ada.Streams.Stream_Element_Offset :=
								In_Last + NC_Converter.Min_Size_In_From_Stream_Elements;
						begin
							if New_Last > In_Item'Last
								or else New_Last < In_Last -- overflow
							then
								New_Last := In_Item'Last;
							end if;
							In_Last := New_Last;
						end;
				end case;
			end;
		end loop;
	end Convert;
	
	package body Controlled is
		
		procedure Do_Open (
			Object : out Converter;
			To : in String;
			From : in String)
		is
			C_To : C.char_array (0 .. To'Length);
			C_From : C.char_array (0 .. From'Length);
			Dummy : C.void_ptr;
			pragma Unreferenced (Dummy);
			Invalid : constant System.Address := System.Storage_Elements.To_Address (
				System.Storage_Elements.Integer_Address'Mod (-1));
			Handle : System.Address;
		begin
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
			-- open
			Handle := System.Address (
				C.iconv.iconv_open (
					C_To (0)'Access,
					C_From (0)'Access));
			if Handle = Invalid then
				raise Name_Error;
			end if;
			Object.Handle := Handle;
			-- about "From"
			Object.Data.Min_Size_In_From_Stream_Elements := 1; -- fallback
			declare
				In_Buffer : aliased constant C.char_array (
					0 ..
					Max_Length_Of_Single_Character - 1) := (others => C.char'Val (0));
			begin
				for I in C.size_t'(1) .. Max_Length_Of_Single_Character loop
					declare
						In_Pointer : aliased C.char_const_ptr := In_Buffer (0)'Unchecked_Access;
						In_Size : aliased C.size_t := I;
						Out_Buffer : aliased C.char_array (
							0 ..
							Max_Length_Of_Single_Character - 1);
						Out_Pointer : aliased C.char_ptr := Out_Buffer (0)'Unchecked_Access;
						Out_Size : aliased C.size_t := Max_Length_Of_Single_Character;
					begin
						if C.iconv.iconv (
							C.iconv.iconv_t (Handle),
							In_Pointer'Access,
							In_Size'Access,
							Out_Pointer'Access,
							Out_Size'Access) /= C.size_t'Last
						then
							Object.Data.Min_Size_In_From_Stream_Elements :=
								Ada.Streams.Stream_Element_Offset (I);
							exit;
						end if;
					end;
				end loop;
			end;
			-- about "To"
			Object.Data.Substitute_Length := 0;
		end Do_Open;
		
		function Handle (Object : Converter) return System.Address is
		begin
			return Object.Handle;
		end Handle;
		
		function Reference (Object : in out Converter)
			return not null access Non_Controlled_Converter is
		begin
			return Object.Data'Unchecked_Access;
		end Reference;
		
		function Constant_Reference (Object : Converter)
			return not null access constant Non_Controlled_Converter is
		begin
			return Object.Data'Unchecked_Access;
		end Constant_Reference;
		
		procedure Finalize (Object : in out Converter) is
		begin
			if C.iconv.iconv_close (C.iconv.iconv_t (Object.Handle)) /= 0 then
				null;
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
