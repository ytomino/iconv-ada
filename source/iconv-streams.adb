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
	end Initialize;
	
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
		Want_Tail : Boolean := Context.Size = 0;
	begin
		Last := Item'First - 1;
		while Last < Item'Last loop
			if Context.Converted_Size = 0 then
				-- Fill source buffer
				if Want_Tail then
					Want_Tail := False;
					declare
						Index : constant Ada.Streams.Stream_Element_Offset :=
							Context.Size + 1;
					begin
						Ada.Streams.Read (
							Stream.all,
							Context.Buffer (Index .. Index),
							Context.Size);
						if Context.Size < Index then
							if Context.Size = 0 then
								return; -- Input empty
							else
								exit;
							end if;
						end if;
					end;
				end if;
				-- Convert Single Character
				declare
					In_Last : Ada.Streams.Stream_Element_Offset;
					Out_Last : Ada.Streams.Stream_Element_Offset;
					Status : Subsequence_Status_Type;
				begin
					Convert (
						Object,
						Context.Buffer (1 .. Context.Size),
						In_Last,
						Context.Converted_Buffer,
						Out_Last,
						Finish => False,
						Status => Status);
					Want_Tail := Context.Size = 0;
					case Status is
						when Finished | Success =>
							null;
						when Overflow =>
							if Out_Last < Context.Converted_Buffer'First then
								raise Constraint_Error; -- Out_Buffer is too smaller
							end if;
						when Truncated =>
							Want_Tail := True;
						when Illegal_Sequence =>
							declare
								Is_Overflow : Boolean;
							begin
								Put_Substitute (
									Object,
									Context.Converted_Buffer (
										Out_Last + 1 ..
										Context.Converted_Buffer'Last),
									Out_Last,
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
					Context.Converted_Size := Out_Last;
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
			-- Read from rest of converted buffer
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
		end loop;
	end Read;
	
	procedure Initialize (Context : in out Writing_Context_Type);
	procedure Initialize (Context : in out Writing_Context_Type) is
	begin
		Context.Size := 0;
	end Initialize;
	
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
			Initialize (Result.Reading_Context);
			Initialize (Result.Writing_Context);
		end return;
	end Open;
	
	function Is_Open (Object : Inout_Type) return Boolean is
	begin
		return Object.Stream /= null;
	end Is_Open;
	
	function Stream (Object : aliased in out Inout_Type)
		return not null access Ada.Streams.Root_Stream_Type'Class is
	begin
		if not Is_Open (Object) then
			raise Status_Error;
		end if;
		return Object'Unchecked_Access;
	end Stream;
	
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
		end if;
		Write (
			Object.Stream,
			Item,
			Object.Writing_Converter,
			Object.Writing_Context);
	end Write;
	
end iconv.Streams;
