package body iconv.Generic_Strings is
	use type Ada.Streams.Stream_Element_Offset;
	
	-- decoder
	
	function From (
		From_Encoding : String;
		To_Encoding : String := Default_Encoding)
		return Decoder is
	begin
		return Result : Decoder do
			Open (
				Result,
				To => To_Encoding,
				From => From_Encoding);
		end return;
	end From;
	
	procedure Decode (
		Object : in Decoder;
		In_Item : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Item : out String_Type;
		Out_Last : out Natural;
		Status : out Error_Status)
	is
		CS_In_SE : constant Ada.Streams.Stream_Element_Count :=
			Character_Type'Size / Ada.Streams.Stream_Element'Size;
		Out_Item_2 : Ada.Streams.Stream_Element_Array (
			1 ..
			Out_Item'Length * CS_In_SE);
		for Out_Item_2'Address use Out_Item'Address;
		Out_Last_2 : Ada.Streams.Stream_Element_Offset;
	begin
		Convert (
			Object,
			In_Item,
			In_Last,
			Out_Item_2,
			Out_Last_2,
			Status);
		pragma Assert (Out_Last_2 rem CS_In_SE = 0);
		Out_Last := Out_Item'First + Natural (Out_Last_2 / CS_In_SE) - 1;
	end Decode;
	
	procedure Decode (
		Object : in Decoder;
		In_Item : in Ada.Streams.Stream_Element_Array;
		Out_Item : out String_Type;
		Out_Last : out Natural)
	is
		CS_In_SE : constant Ada.Streams.Stream_Element_Count :=
			Character_Type'Size / Ada.Streams.Stream_Element'Size;
		Out_Item_2 : Ada.Streams.Stream_Element_Array (
			1 ..
			Out_Item'Length * CS_In_SE);
		for Out_Item_2'Address use Out_Item'Address;
		Out_Last_2 : Ada.Streams.Stream_Element_Offset;
	begin
		Convert (
			Object,
			In_Item,
			Out_Item_2,
			Out_Last_2);
		pragma Assert (Out_Last_2 rem CS_In_SE = 0);
		Out_Last := Out_Item'First + Natural (Out_Last_2 / CS_In_SE) - 1;
	end Decode;
	
	function Decode (
		Object : Decoder;
		S : Ada.Streams.Stream_Element_Array)
		return String_Type
	is
		Result : String_Type (
			1 ..
			Max_Length_Of_Single_Character * S'Length);
		Last : Natural;
	begin
		Decode (
			Object,
			S,
			Result,
			Last);
		return Result (1 .. Last);
	end Decode;
	
	-- encoder
	
	function To (
		To_Encoding : String;
		From_Encoding : String := Default_Encoding)
		return Encoder is
	begin
		return Result : Encoder do
			Open (
				Result,
				To => To_Encoding,
				From => From_Encoding);
		end return;
	end To;
	
	procedure Encode (
		Object : in Encoder;
		In_Item : in String_Type;
		In_Last : out Natural;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Status : out Error_Status)
	is
		CS_In_SE : constant Ada.Streams.Stream_Element_Count :=
			Character_Type'Size / Ada.Streams.Stream_Element'Size;
		In_Item_2 : Ada.Streams.Stream_Element_Array (
			1 ..
			In_Item'Length * CS_In_SE);
		for In_Item_2'Address use In_Item'Address;
		In_Last_2 : Ada.Streams.Stream_Element_Offset;
	begin
		Convert (
			Object,
			In_Item_2,
			In_Last_2,
			Out_Item,
			Out_Last,
			Status);
		pragma Assert (In_Last_2 rem CS_In_SE = 0);
		In_Last := In_Item'First + Natural (In_Last_2 / CS_In_SE) - 1;
	end Encode;
	
	procedure Encode (
		Object : in Encoder;
		In_Item : in String_Type;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset)
	is
		CS_In_SE : constant Ada.Streams.Stream_Element_Count :=
			Character_Type'Size / Ada.Streams.Stream_Element'Size;
		In_Item_2 : Ada.Streams.Stream_Element_Array (
			1 ..
			In_Item'Length * CS_In_SE);
		for In_Item_2'Address use In_Item'Address;
	begin
		Convert (
			Object,
			In_Item_2,
			Out_Item,
			Out_Last);
	end Encode;
	
	function Encode (
		Object : Encoder;
		S : String_Type)
		return Ada.Streams.Stream_Element_Array
	is
		CS_In_SE : constant Ada.Streams.Stream_Element_Count :=
			Character_Type'Size / Ada.Streams.Stream_Element'Size;
		Result : Ada.Streams.Stream_Element_Array (
			0 ..
			CS_In_SE * Max_Length_Of_Single_Character * S'Length - 1);
		Last : Ada.Streams.Stream_Element_Offset;
	begin
		Encode (
			Object,
			S,
			Result,
			Last);
		return Result (0 .. Last);
	end Encode;
	
end iconv.Generic_Strings;
