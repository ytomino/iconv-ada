pragma Ada_2012;
package body iconv.Streams is
	use type Ada.Streams.Stream_Element;
	use type Ada.Streams.Stream_Element_Offset;
	use type Ada.Streams.Stream_Element_Array;
	
	procedure Initialize (Context : in out Reading_Context_Type);
	procedure Initialize (Context : in out Reading_Context_Type) is
	begin
		Context.Size := 0;
		Context.Converted_Size := 0;
		Context.Status := Continuing;
	end Initialize;
	
	procedure Set_Substitute_To_Reading_Converter (
		Object : in out Converter;
		Substitute : Ada.Streams.Stream_Element_Array)
		renames Set_Substitute; -- iconv.Set_Substitute
	
	procedure Read (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out Ada.Streams.Stream_Element_Array;
		Last : out Ada.Streams.Stream_Element_Offset;
		Object : Converter;
		Context : in out Reading_Context_Type);
	procedure Read (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out Ada.Streams.Stream_Element_Array;
		Last : out Ada.Streams.Stream_Element_Offset;
		Object : Converter;
		Context : in out Reading_Context_Type)
	is
		Read_Zero : Boolean := False;
	begin
		Last := Item'First - 1;
		while Last < Item'Last loop
			-- filling
			if Context.Status = Continuing then
				if Context.Size < Buffer_Type'Length then
					declare
						Old_Context_Last : constant Ada.Streams.Stream_Element_Offset :=
							Context.Size;
					begin
						Ada.Streams.Read (
							Stream.all,
							Context.Buffer (Context.Size + 1 .. Buffer_Type'Last),
							Context.Size);
						Read_Zero := Old_Context_Last = Context.Size;
					exception
						when End_Error =>
							Context.Status := Finishing;
					end;
				end if;
			end if;
			-- converting
			if Context.Status <= Finishing then
				-- try to convert subsequence
				declare
					In_Last : Ada.Streams.Stream_Element_Offset;
					Old_Converted_Last : constant Ada.Streams.Stream_Element_Offset :=
						Context.Converted_Size;
					Status : Subsequence_Status_Type;
				begin
					Convert (
						Object,
						Context.Buffer (1 .. Context.Size),
						In_Last,
						Context.Converted_Buffer (
							Context.Converted_Size + 1 ..
							Buffer_Type'Last),
						Context.Converted_Size,
						Finish => Context.Status > Continuing,
						Status => Status);
					case Status is
						when Finished =>
							Context.Status := Ended;
						when Success =>
							null;
						when Overflow =>
							if Context.Converted_Size < Context.Converted_Buffer'First then
								raise Constraint_Error; -- Converted_Buffer is too smaller
							end if;
						when Truncated =>
							pragma Assert (Context.Status = Continuing);
							if Context.Converted_Size = Old_Converted_Last then
								exit; -- wait tail-bytes
							end if;
						when Illegal_Sequence =>
							declare
								Is_Overflow : Boolean;
							begin
								Put_Substitute (
									Object,
									Context.Converted_Buffer (
										Context.Converted_Size + 1 ..
										Context.Converted_Buffer'Last),
									Context.Converted_Size,
									Is_Overflow);
								if Is_Overflow then
									exit; -- wait a next try
								end if;
							end;
							-- skip one element
							In_Last := Ada.Streams.Stream_Element_Offset'Min (
								Context.Size,
								In_Last + Min_Size_In_From_Stream_Elements (Object));
					end case;
					-- drop converted subsequence
					declare
						New_In_Size : constant Ada.Streams.Stream_Element_Count :=
							Context.Size - In_Last;
					begin
						Context.Buffer (1 .. New_In_Size) :=
							Context.Buffer (In_Last + 1 .. Context.Size);
						Context.Size := New_In_Size;
					end;
				end;
			end if;
			-- copy converted elements
			declare
				Copy_Size : constant Ada.Streams.Stream_Element_Count :=
					Ada.Streams.Stream_Element_Count'Min (
						Context.Converted_Size,
						Item'Last - Last);
				New_Last : constant Ada.Streams.Stream_Element_Offset := Last + Copy_Size;
				New_Out_Size : constant Ada.Streams.Stream_Element_Count :=
					Context.Converted_Size - Copy_Size;
			begin
				Item (Last + 1 .. New_Last) := Context.Converted_Buffer (1 .. Copy_Size);
				Context.Converted_Buffer (1 .. New_Out_Size) :=
					Context.Converted_Buffer (Copy_Size + 1 .. Context.Converted_Size);
				Last := New_Last;
				Context.Converted_Size := New_Out_Size;
			end;
			exit when (Context.Status = Ended or else Read_Zero)
				and then Context.Converted_Size = 0;
		end loop;
		if Last = Item'First - 1 -- do not use "<" since underflow
			and then Context.Status = Ended
			and then Context.Converted_Size = 0
		then
			raise End_Error;
		end if;
	end Read;
	
	procedure Initialize (Context : in out Writing_Context_Type);
	procedure Initialize (Context : in out Writing_Context_Type) is
	begin
		Context.Size := 0;
	end Initialize;
	
	procedure Set_Substitute_To_Writing_Converter (
		Object : in out Converter;
		Substitute : Ada.Streams.Stream_Element_Array);
	procedure Set_Substitute_To_Writing_Converter (
		Object : in out Converter;
		Substitute : Ada.Streams.Stream_Element_Array)
	is
		Substitute_Last : Ada.Streams.Stream_Element_Offset :=
			Substitute'First - 1;
		S2 : Ada.Streams.Stream_Element_Array (1 .. Max_Substitute_Length);
		S2_Last : Ada.Streams.Stream_Element_Offset := S2'First - 1;
	begin
		-- convert substitute from internal to external
		loop
			declare
				Status : Substituting_Status_Type;
			begin
				Convert (
					Object,
					Substitute (Substitute_Last + 1 .. Substitute'Last),
					Substitute_Last,
					S2 (S2_Last + 1 .. S2'Last),
					S2_Last,
					Finish => True,
					Status => Status);
				case Status is
					when Finished =>
						exit;
					when Success =>
						null;
					when Overflow =>
						raise Constraint_Error;
				end case;
			end;
		end loop;
		Set_Substitute (
			Object,
			S2 (1 .. S2_Last));
	end Set_Substitute_To_Writing_Converter;
	
	procedure Write (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : Ada.Streams.Stream_Element_Array;
		Object : Converter;
		Context : in out Writing_Context_Type);
	procedure Write (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : Ada.Streams.Stream_Element_Array;
		Object : Converter;
		Context : in out Writing_Context_Type) is
	begin
		if Item'Length > 0 then
			declare
				In_Buffer : constant Ada.Streams.Stream_Element_Array :=
					Context.Buffer (1 .. Context.Size) & Item;
				In_Index : Ada.Streams.Stream_Element_Offset := In_Buffer'First;
				Out_Buffer : Ada.Streams.Stream_Element_Array
					(1 .. Max_Length_Of_Single_Character * In_Buffer'Length);
				Out_Index : Ada.Streams.Stream_Element_Offset := Out_Buffer'First;
			begin
				loop
					declare
						In_Last : Ada.Streams.Stream_Element_Offset;
						Out_Last : Ada.Streams.Stream_Element_Offset;
						Status : Continuing_Status_Type;
					begin
						Convert (
							Object,
							In_Buffer (In_Index .. In_Buffer'Last),
							In_Last,
							Out_Buffer (Out_Index .. Out_Buffer'Last),
							Out_Last,
							Status => Status);
						In_Index := In_Last + 1;
						Out_Index := Out_Last + 1;
						case Status is
							when Success =>
								null;
							when Overflow =>
								if Out_Last < Out_Buffer'First then
									raise Constraint_Error; -- Out_Buffer is too smaller
								end if;
							when Truncated =>
								exit; -- wait tail-bytes
							when Illegal_Sequence =>
								declare
									Is_Overflow : Boolean;
								begin
									Put_Substitute (
										Object,
										Out_Buffer (Out_Index .. Out_Buffer'Last),
										Out_Index,
										Is_Overflow);
									if Is_Overflow then
										exit; -- wait a next try
									end if;
								end;
								Out_Index := Out_Index + 1;
								-- skip one element
								In_Index := Ada.Streams.Stream_Element_Offset'Min (
									Context.Size,
									In_Index
										+ Min_Size_In_From_Stream_Elements (Object));
						end case;
						exit when In_Index > In_Buffer'Last;
					end;
				end loop;
				Context.Size := In_Buffer'Last - In_Index + 1;
				Context.Buffer (1 .. Context.Size) :=
					In_Buffer (In_Index .. In_Buffer'Last);
				Ada.Streams.Write (Stream.all, Out_Buffer (1 .. Out_Index - 1));
			end;
		end if;
	end Write;
	
	procedure Finish (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Object : Converter;
		Context : in out Writing_Context_Type);
	procedure Finish (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Object : Converter;
		Context : in out Writing_Context_Type)
	is
		Out_Buffer : Ada.Streams.Stream_Element_Array (0 .. 63);
		Out_Last : Ada.Streams.Stream_Element_Offset := -1;
		Status : Finishing_Status_Type;
	begin
		if Context.Size > 0 then
			-- put substitute instead of incomplete sequence in the buffer
			declare
				Is_Overflow : Boolean; -- ignore
			begin
				Put_Substitute (
					Object,
					Out_Buffer (Out_Last + 1 .. Out_Buffer'Last),
					Out_Last,
					Is_Overflow);
			end;
			Initialize (Context); -- reset indexes
		end if;
		-- finish
		loop
			Convert (
				Object,
				Out_Buffer (Out_Last + 1 .. Out_Buffer'Last),
				Out_Last,
				Finish => True,
				Status => Status);
			Ada.Streams.Write (
				Stream.all,
				Out_Buffer (Out_Buffer'First .. Out_Last));
			case Status is
				when Finished =>
					exit;
				when Success =>
					null;
				when Overflow =>
					if Out_Last < Out_Buffer'First then
						raise Constraint_Error; -- Out_Buffer is too smaller
					end if;
			end case;
		end loop;
	end Finish;
	
	-- implementation of bidirectional
	
	function Open (
		Internal : not null access constant String;
		External : not null access constant String;
		Stream : not null access Ada.Streams.Root_Stream_Type'Class)
		return Inout_Type
	is
		pragma Suppress (Accessibility_Check);
	begin
		return Result : Inout_Type do
			Result.Internal := Internal;
			Result.External := External;
			Result.Stream := Stream;
			Result.Substitute_Length := 0; -- default is empty
			Initialize (Result.Reading_Context);
			Initialize (Result.Writing_Context);
		end return;
	end Open;
	
	function Is_Open (Object : Inout_Type) return Boolean is
	begin
		return Object.Stream /= null;
	end Is_Open;
	
	function Substitute (Object : Inout_Type)
		return Ada.Streams.Stream_Element_Array is
	begin
		return Object.Substitute (1 .. Object.Substitute_Length);
	end Substitute;
	
	procedure Set_Substitute (
		Object : in out Inout_Type;
		Substitute : Ada.Streams.Stream_Element_Array) is
	begin
		if Substitute'Length > Object.Substitute'Length then
			raise Constraint_Error;
		end if;
		Object.Substitute_Length := Substitute'Length;
		Object.Substitute (1 .. Object.Substitute_Length) := Substitute;
		-- set to converters
		if Is_Open (Object.Reading_Converter) then
			Set_Substitute_To_Reading_Converter (
				Object.Reading_Converter,
				Substitute);
		end if;
		if Is_Open (Object.Writing_Converter) then
			Set_Substitute_To_Writing_Converter (
				Object.Writing_Converter,
				Substitute);
		end if;
	end Set_Substitute;
	
	function Stream (Object : aliased in out Inout_Type)
		return not null access Ada.Streams.Root_Stream_Type'Class is
	begin
		if not Is_Open (Object) then
			raise Status_Error;
		end if;
		return Object'Unchecked_Access;
	end Stream;
	
	procedure Finish (Object : in out Inout_Type) is
	begin
		if not Is_Open (Object) then
			raise Status_Error;
		end if;
		if Is_Open (Object.Writing_Converter) then
			Finish (
				Object.Stream,
				Object.Writing_Converter,
				Object.Writing_Context);
		end if;
	end Finish;
	
	overriding procedure Read (
		Object : in out Inout_Type;
		Item : out Ada.Streams.Stream_Element_Array;
		Last : out Ada.Streams.Stream_Element_Offset) is
	begin
		if not Is_Open (Object.Reading_Converter) then
			Open (
				Object.Reading_Converter,
				To => Object.Internal.all,
				From => Object.External.all);
			if Object.Substitute_Length >= 0 then
				Set_Substitute_To_Reading_Converter (
					Object.Reading_Converter,
					Object.Substitute (1 .. Object.Substitute_Length));
			end if;
		end if;
		Read (
			Object.Stream,
			Item,
			Last,
			Object.Reading_Converter,
			Object.Reading_Context);
	end Read;
	
	overriding procedure Write (
		Object : in out Inout_Type;
		Item : in Ada.Streams.Stream_Element_Array) is
	begin
		if not Is_Open (Object.Writing_Converter) then
			Open (
				Object.Writing_Converter,
				To => Object.External.all,
				From => Object.Internal.all);
			if Object.Substitute_Length >= 0 then
				Set_Substitute_To_Writing_Converter (
					Object.Writing_Converter,
					Object.Substitute (1 .. Object.Substitute_Length));
			end if;
		end if;
		Write (
			Object.Stream,
			Item,
			Object.Writing_Converter,
			Object.Writing_Context);
	end Write;
	
end iconv.Streams;
