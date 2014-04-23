with Ada.Streams;
package iconv.Streams is
	pragma Preelaborate;
	
	type Stream (<>) is limited new Ada.Streams.Root_Stream_Type with private;
	
	function Create (
		Target : not null access Ada.Streams.Root_Stream_Type'Class;
		Encoding : not null access constant iconv.Encoding)
		return Stream;
	
private
	
	subtype Buffer_Type is
		Ada.Streams.Stream_Element_Array (1 .. Max_Length_Of_Single_Character);
	
	type Reading_Context_Type is record
		Buffer : Buffer_Type;
		Size : Ada.Streams.Stream_Element_Count;
		Converted_Buffer : Buffer_Type;
		Converted_Size : Ada.Streams.Stream_Element_Count;
	end record;
	pragma Suppress_Initialization (Reading_Context_Type);
	
	type Writing_Context_Type is record
		Buffer : Buffer_Type;
		Size : Ada.Streams.Stream_Element_Count;
	end record;
	pragma Suppress_Initialization (Writing_Context_Type);
	
	type Stream is limited new Ada.Streams.Root_Stream_Type with record
		Encoding : access constant iconv.Encoding;
		Stream : access Ada.Streams.Root_Stream_Type'Class;
		-- reading
		Reading_Context : Reading_Context_Type;
		-- writing
		Writing_Context : Writing_Context_Type;
	end record;
	
	overriding procedure Read (
		Object : in out Stream;
		Item : out Ada.Streams.Stream_Element_Array;
		Last : out Ada.Streams.Stream_Element_Offset);
	overriding procedure Write (
		Object : in out Stream;
		Item : in Ada.Streams.Stream_Element_Array);
	
end iconv.Streams;
