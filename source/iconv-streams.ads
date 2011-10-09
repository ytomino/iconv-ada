with Ada.Streams;
package iconv.Streams is
	pragma Preelaborate;
	
	procedure Convert (
		Object : in Converter;
		In_Item : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Status : out Error_Status);
	
	type Stream (<>) is limited new Ada.Streams.Root_Stream_Type with private;
	
	function Create (
		Target : not null access Ada.Streams.Root_Stream_Type'Class;
		Encoding : not null access constant iconv.Encoding;
		Substitute : Character := '?')
		return Stream;
	
private
	
	type Stream is limited new Ada.Streams.Root_Stream_Type with record
		Target : not null access Ada.Streams.Root_Stream_Type'Class;
		Encoding : not null access constant iconv.Encoding;
		In_Buffer : aliased Ada.Streams.Stream_Element_Array
			(1 .. Max_Length_Of_Single_Character);
		Out_Buffer : aliased Ada.Streams.Stream_Element_Array
			(1 .. Max_Length_Of_Single_Character);
		In_Size : Ada.Streams.Stream_Element_Count;
		Out_Size : Ada.Streams.Stream_Element_Count;
		Substitute : Ada.Streams.Stream_Element;
	end record;
	
	overriding procedure Read (
		Object : in out Stream;
		Item : out Ada.Streams.Stream_Element_Array;
		Last : out Ada.Streams.Stream_Element_Offset);
	overriding procedure Write (
		Object : in out Stream;
		Item : in Ada.Streams.Stream_Element_Array);
	
end iconv.Streams;
