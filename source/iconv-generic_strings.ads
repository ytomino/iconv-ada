generic
	type Character_Type is (<>);
	type String_Type is array (Positive range <>) of Character_Type;
	Default_Encoding : in String;
package iconv.Generic_Strings is
	pragma Preelaborate;
	
	-- decoder
	
	type Decoder is new Converter;
	
	function From (
		From_Encoding : String;
		To_Encoding : String := Default_Encoding)
		return Decoder;
	
	-- decode subsequence
	procedure Decode (
		Object : in Decoder;
		In_Item : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Item : out String_Type;
		Out_Last : out Natural;
		Status : out Error_Status);
	
	-- decode all character sequence with substitute
	procedure Decode (
		Object : in Decoder;
		In_Item : in Ada.Streams.Stream_Element_Array;
		Out_Item : out String_Type;
		Out_Last : out Natural);
	
	function Decode (
		Object : Decoder;
		S : Ada.Streams.Stream_Element_Array)
		return String_Type;
	
	-- encoder
	
	type Encoder is new Converter;
	
	function To (
		To_Encoding : String;
		From_Encoding : String := Default_Encoding)
		return Encoder;
	
	-- encode subsequence
	procedure Encode (
		Object : in Encoder;
		In_Item : in String_Type;
		In_Last : out Natural;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Status : out Error_Status);
	
	-- encode all character sequence with substitute
	procedure Encode (
		Object : in Encoder;
		In_Item : in String_Type;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset);
	
	function Encode (
		Object : Encoder;
		S : String_Type)
		return Ada.Streams.Stream_Element_Array;
	
end iconv.Generic_Strings;
