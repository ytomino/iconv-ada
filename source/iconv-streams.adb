package body iconv.Streams is
	use type Ada.Streams.Stream_Element;
	use type Ada.Streams.Stream_Element_Offset;
	use type Ada.Streams.Stream_Element_Array;
	
	function Create (
		Target : not null access Ada.Streams.Root_Stream_Type'Class;
		Encoding : not null access constant iconv.Encoding)
		return Stream
	is
		pragma Suppress (Accessibility_Check);
	begin
		if not Is_Open (Encoding.all) then
			raise Status_Error;
		end if;
		return Stream'(Ada.Streams.Root_Stream_Type with
			Target => Target,
			Encoding => Encoding,
			In_Buffer => <>,
			Out_Buffer => <>,
			In_Size => 0,
			Out_Size => 0);
	end Create;
	
	overriding procedure Read (
		Object : in out Stream;
		Item : out Ada.Streams.Stream_Element_Array;
		Last : out Ada.Streams.Stream_Element_Offset)
	is
		Want_Tail : Boolean := Object.In_Size = 0;
	begin
		Last := Item'First - 1;
		while Last < Item'Last loop
			if Object.Out_Size = 0 then
				-- Fill source buffer
				if Want_Tail then
					Want_Tail := False;
					declare
						Index : constant Ada.Streams.Stream_Element_Offset :=
							Object.In_Size + 1;
					begin
						Ada.Streams.Read (
							Object.Target.all,
							Object.In_Buffer (Index .. Index),
							Object.In_Size);
						if Object.In_Size < Index then
							if Object.In_Size = 0 then
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
					Status : Error_Status;
				begin
					Convert (
						Object.Encoding.Reading,
						Object.In_Buffer (1 .. Object.In_Size),
						In_Last,
						Object.Out_Buffer,
						Out_Last,
						Status);
					Want_Tail := Object.In_Size = 0;
					case Status is
						when Fine =>
							null;
						when Invalid =>
							Want_Tail := True;
						when Illegal_Sequence =>
							declare
								Is_Overflow : Boolean;
							begin
								Put_Substitute (
									Object.Encoding.Reading,
									Object.Out_Buffer (Out_Last + 1 .. Object.Out_Buffer'Last),
									Out_Last,
									Is_Overflow);
								if Is_Overflow then
									exit; -- wait a next try
								end if;
							end;
							In_Last := In_Last + 1;
					end case;
					Object.Out_Size := Out_Last;
					declare
						New_In_Size : constant Ada.Streams.Stream_Element_Count :=
							Object.In_Size - In_Last;
					begin
						Object.In_Buffer (1 .. New_In_Size) :=
							Object.In_Buffer (In_Last + 1 .. Object.In_Size);
						Object.In_Size := New_In_Size;
					end;
				end;
			end if;
			-- Read from rest of converted buffer
			declare
				Copy_Size : constant Ada.Streams.Stream_Element_Count :=
					Ada.Streams.Stream_Element_Count'Min (
						Object.Out_Size,
						Item'Last - Last);
				New_Last : constant Ada.Streams.Stream_Element_Offset := Last + Copy_Size;
				New_Out_Size : constant Ada.Streams.Stream_Element_Count :=
					Object.Out_Size - Copy_Size;
			begin
				Item (Last + 1 .. New_Last) := Object.Out_Buffer (1 .. Copy_Size);
				Object.Out_Buffer (1 .. New_Out_Size) :=
					Object.Out_Buffer (Copy_Size + 1 .. Object.Out_Size);
				Last := New_Last;
				Object.Out_Size := New_Out_Size;
			end;
		end loop;
	end Read;
	
	overriding procedure Write (
		Object : in out Stream;
		Item : in Ada.Streams.Stream_Element_Array) is
	begin
		if Item'Length > 0 then
			declare
				In_Buffer : constant Ada.Streams.Stream_Element_Array :=
					Object.In_Buffer (1 .. Object.In_Size) & Item;
				In_Index : Ada.Streams.Stream_Element_Offset := In_Buffer'First;
				Out_Buffer : Ada.Streams.Stream_Element_Array
					(1 .. Max_Length_Of_Single_Character * In_Buffer'Length);
				Out_Index : Ada.Streams.Stream_Element_Offset := Out_Buffer'First;
			begin
				loop
					declare
						Status : Error_Status;
						In_Last : Ada.Streams.Stream_Element_Offset;
						Out_Last : Ada.Streams.Stream_Element_Offset;
					begin
						Convert (
							Object.Encoding.Writing,
							In_Buffer (In_Index .. In_Buffer'Last),
							In_Last,
							Out_Buffer (Out_Index .. Out_Buffer'Last),
							Out_Last,
							Status);
						In_Index := In_Last + 1;
						Out_Index := Out_Last + 1;
						case Status is
							when Fine =>
								null;
							when Invalid =>
								exit; -- wait tail-bytes
							when Illegal_Sequence =>
								declare
									Is_Overflow : Boolean;
								begin
									Put_Substitute (
										Object.Encoding.Writing,
										Out_Buffer (Out_Index .. Out_Buffer'Last),
										Out_Index,
										Is_Overflow);
									if Is_Overflow then
										exit; -- wait a next try
									end if;
								end;
								Out_Index := Out_Index + 1;
								In_Index := In_Index + 1;
						end case;
						exit when In_Index > In_Buffer'Last;
					end;
				end loop;
				Object.In_Size := In_Buffer'Last - In_Index + 1;
				Object.In_Buffer (1 .. Object.In_Size) :=
					In_Buffer (In_Index .. In_Buffer'Last);
				Ada.Streams.Write (Object.Target.all, Out_Buffer (1 .. Out_Index - 1));
			end;
		end if;
	end Write;
	
end iconv.Streams;
