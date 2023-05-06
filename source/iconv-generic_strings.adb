package body iconv.Generic_Strings is
	use type Ada.Streams.Stream_Element_Offset;
	
	pragma Compile_Time_Error (
		String_Type'Component_Size /= Character_Type'Size,
		"String_Type is not packed");
	pragma Compile_Time_Error (
		Character_Type'Size rem Ada.Streams.Stream_Element'Size /= 0,
		"String_Type could not be treated as Stream_Element_Array");
	
	-- decoder
	
	function From (
		From_Encoding : String;
		To_Encoding : String := Default_Encoding)
		return Decoder is
	begin
		return Result : Decoder do
			Do_Open (Converter (Result), To => To_Encoding, From => From_Encoding);
		end return;
	end From;
	
	procedure Decode (
		Object : in Decoder;
		In_Item : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Item : out String_Type;
		Out_Last : out Natural;
		Finish : in Boolean;
		Status : out Subsequence_Status_Type)
	is
		CS_In_SE : constant Ada.Streams.Stream_Element_Count :=
			Character_Type'Size / Ada.Streams.Stream_Element'Size;
		Out_Item_2 :
			Ada.Streams.Stream_Element_Array (1 .. Out_Item'Length * CS_In_SE);
		for Out_Item_2'Address use Out_Item'Address;
		Out_Last_2 : Ada.Streams.Stream_Element_Offset;
	begin
		Convert (
			Object,
			In_Item,
			In_Last,
			Out_Item_2,
			Out_Last_2,
			Finish => Finish,
			Status => Status);
		pragma Assert (Out_Last_2 rem CS_In_SE = 0);
		Out_Last := Out_Item'First + Natural (Out_Last_2 / CS_In_SE) - 1;
	end Decode;
	
	procedure Decode (
		Object : in Decoder;
		In_Item : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Item : out String_Type;
		Out_Last : out Natural;
		Status : out Continuing_Status_Type)
	is
		CS_In_SE : constant Ada.Streams.Stream_Element_Count :=
			Character_Type'Size / Ada.Streams.Stream_Element'Size;
		Out_Item_2 :
			Ada.Streams.Stream_Element_Array (1 .. Out_Item'Length * CS_In_SE);
		for Out_Item_2'Address use Out_Item'Address;
		Out_Last_2 : Ada.Streams.Stream_Element_Offset;
	begin
		Convert (
			Object,
			In_Item,
			In_Last,
			Out_Item_2,
			Out_Last_2,
			Status => Status);
		pragma Assert (Out_Last_2 rem CS_In_SE = 0);
		Out_Last := Out_Item'First + Natural (Out_Last_2 / CS_In_SE) - 1;
	end Decode;
	
	procedure Decode (
		Object : in Decoder;
		Out_Item : out String_Type;
		Out_Last : out Natural;
		Finish : in True_Only;
		Status : out Finishing_Status_Type)
	is
		CS_In_SE : constant Ada.Streams.Stream_Element_Count :=
			Character_Type'Size / Ada.Streams.Stream_Element'Size;
		Out_Item_2 :
			Ada.Streams.Stream_Element_Array (1 .. Out_Item'Length * CS_In_SE);
		for Out_Item_2'Address use Out_Item'Address;
		Out_Last_2 : Ada.Streams.Stream_Element_Offset;
	begin
		Convert (
			Object,
			Out_Item_2,
			Out_Last_2,
			Finish => Finish,
			Status => Status);
		pragma Assert (Out_Last_2 rem CS_In_SE = 0);
		Out_Last := Out_Item'First + Natural (Out_Last_2 / CS_In_SE) - 1;
	end Decode;
	
	procedure Decode (
		Object : in Decoder;
		In_Item : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Item : out String_Type;
		Out_Last : out Natural;
		Finish : in True_Only;
		Status : out Status_Type)
	is
		CS_In_SE : constant Ada.Streams.Stream_Element_Count :=
			Character_Type'Size / Ada.Streams.Stream_Element'Size;
		Out_Item_2 :
			Ada.Streams.Stream_Element_Array (1 .. Out_Item'Length * CS_In_SE);
		for Out_Item_2'Address use Out_Item'Address;
		Out_Last_2 : Ada.Streams.Stream_Element_Offset;
	begin
		Convert (
			Object,
			In_Item,
			In_Last,
			Out_Item_2,
			Out_Last_2,
			Finish => Finish,
			Status => Status);
		pragma Assert (Out_Last_2 rem CS_In_SE = 0);
		Out_Last := Out_Item'First + Natural (Out_Last_2 / CS_In_SE) - 1;
	end Decode;
	
	procedure Decode (
		Object : in Decoder;
		In_Item : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Item : out String_Type;
		Out_Last : out Natural;
		Finish : in True_Only;
		Status : out Substituting_Status_Type)
	is
		CS_In_SE : constant Ada.Streams.Stream_Element_Count :=
			Character_Type'Size / Ada.Streams.Stream_Element'Size;
		Out_Item_2 :
			Ada.Streams.Stream_Element_Array (1 .. Out_Item'Length * CS_In_SE);
		for Out_Item_2'Address use Out_Item'Address;
		Out_Last_2 : Ada.Streams.Stream_Element_Offset;
	begin
		Convert (
			Object,
			In_Item,
			In_Last,
			Out_Item_2,
			Out_Last_2,
			Finish => Finish,
			Status => Status);
		pragma Assert (Out_Last_2 rem CS_In_SE = 0);
		Out_Last := Out_Item'First + Natural (Out_Last_2 / CS_In_SE) - 1;
	end Decode;
	
	function Decode (Object : Decoder; S : Ada.Streams.Stream_Element_Array)
		return String_Type
	is
		CS_In_SE : constant Integer :=
			Character_Type'Size / Ada.Streams.Stream_Element'Size;
		In_Last : Ada.Streams.Stream_Element_Offset := S'First - 1;
		Result :
			String_Type (
				1 .. (Max_Length_Of_Single_Character + CS_In_SE - 1) / CS_In_SE * S'Length);
		Out_Last : Natural := 0;
		Status : Substituting_Status_Type;
	begin
		loop
			Decode (
				Object,
				S (In_Last + 1 .. S'Last),
				In_Last,
				Result (Out_Last + 1 .. Result'Last),
				Out_Last,
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
		end loop;
		return Result (Result'First .. Out_Last);
	exception
		when others =>
			Reset_State (Converter (Object));
			raise;
	end Decode;
	
	-- encoder
	
	function To (
		To_Encoding : String;
		From_Encoding : String := Default_Encoding)
		return Encoder is
	begin
		return Result : Encoder do
			Do_Open (Converter (Result), To => To_Encoding, From => From_Encoding);
		end return;
	end To;
	
	procedure Encode (
		Object : in Encoder;
		In_Item : in String_Type;
		In_Last : out Natural;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Finish : in Boolean;
		Status : out Subsequence_Status_Type)
	is
		CS_In_SE : constant Ada.Streams.Stream_Element_Count :=
			Character_Type'Size / Ada.Streams.Stream_Element'Size;
		In_Item_2 : Ada.Streams.Stream_Element_Array (1 .. In_Item'Length * CS_In_SE);
		for In_Item_2'Address use In_Item'Address;
		In_Last_2 : Ada.Streams.Stream_Element_Offset;
	begin
		Convert (
			Object,
			In_Item_2,
			In_Last_2,
			Out_Item,
			Out_Last,
			Finish => Finish,
			Status => Status);
		pragma Assert (In_Last_2 rem CS_In_SE = 0);
		In_Last := In_Item'First + Natural (In_Last_2 / CS_In_SE) - 1;
	end Encode;
	
	procedure Encode (
		Object : in Encoder;
		In_Item : in String_Type;
		In_Last : out Natural;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Status : out Continuing_Status_Type)
	is
		CS_In_SE : constant Ada.Streams.Stream_Element_Count :=
			Character_Type'Size / Ada.Streams.Stream_Element'Size;
		In_Item_2 : Ada.Streams.Stream_Element_Array (1 .. In_Item'Length * CS_In_SE);
		for In_Item_2'Address use In_Item'Address;
		In_Last_2 : Ada.Streams.Stream_Element_Offset;
	begin
		Convert (
			Object,
			In_Item_2,
			In_Last_2,
			Out_Item,
			Out_Last,
			Status => Status);
		pragma Assert (In_Last_2 rem CS_In_SE = 0);
		In_Last := In_Item'First + Natural (In_Last_2 / CS_In_SE) - 1;
	end Encode;
	
	procedure Encode (
		Object : in Encoder;
		In_Item : in String_Type;
		In_Last : out Natural;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Finish : in True_Only;
		Status : out Status_Type)
	is
		CS_In_SE : constant Ada.Streams.Stream_Element_Count :=
			Character_Type'Size / Ada.Streams.Stream_Element'Size;
		In_Item_2 : Ada.Streams.Stream_Element_Array (1 .. In_Item'Length * CS_In_SE);
		for In_Item_2'Address use In_Item'Address;
		In_Last_2 : Ada.Streams.Stream_Element_Offset;
	begin
		Convert (
			Object,
			In_Item_2,
			In_Last_2,
			Out_Item,
			Out_Last,
			Finish => Finish,
			Status => Status);
		pragma Assert (In_Last_2 rem CS_In_SE = 0);
		In_Last := In_Item'First + Natural (In_Last_2 / CS_In_SE) - 1;
	end Encode;
	
	procedure Encode (
		Object : in Encoder;
		In_Item : in String_Type;
		In_Last : out Natural;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Finish : in True_Only;
		Status : out Substituting_Status_Type)
	is
		CS_In_SE : constant Ada.Streams.Stream_Element_Count :=
			Character_Type'Size / Ada.Streams.Stream_Element'Size;
		In_Item_2 : Ada.Streams.Stream_Element_Array (1 .. In_Item'Length * CS_In_SE);
		for In_Item_2'Address use In_Item'Address;
		In_Last_2 : Ada.Streams.Stream_Element_Offset;
	begin
		Convert (
			Object,
			In_Item_2,
			In_Last_2,
			Out_Item,
			Out_Last,
			Finish => Finish,
			Status => Status);
		pragma Assert (In_Last_2 rem CS_In_SE = 0);
		In_Last := In_Item'First + Natural (In_Last_2 / CS_In_SE) - 1;
	end Encode;
	
	function Encode (Object : Encoder; S : String_Type)
		return Ada.Streams.Stream_Element_Array
	is
		In_Last : Natural := S'First - 1;
		Result :
			Ada.Streams.Stream_Element_Array (
				0 .. Max_Length_Of_Single_Character * S'Length - 1);
		Out_Last : Ada.Streams.Stream_Element_Offset := -1;
		Status : Substituting_Status_Type;
	begin
		loop
			Encode (
				Object,
				S (In_Last + 1 .. S'Last),
				In_Last,
				Result (Out_Last + 1 .. Result'Last),
				Out_Last,
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
		end loop;
		return Result (Result'First .. Out_Last);
	exception
		when others =>
			Reset_State (Converter (Object));
			raise;
	end Encode;
	
end iconv.Generic_Strings;
