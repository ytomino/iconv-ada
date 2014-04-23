pragma Ada_2012;
with Ada.Streams;
package iconv.Streams is
	pragma Preelaborate;
	
	-- bidirectional
	
	type Inout_Type is limited private;
	
	-- management
	function Open (
		Internal : not null access constant String;
		External : not null access constant String;
		Stream : not null access Ada.Streams.Root_Stream_Type'Class)
		return Inout_Type;
	function Is_Open (Object : Inout_Type) return Boolean;
	pragma Inline (Is_Open);
	
	-- substitute (encoded as internal)
	function Substitute (Object : Inout_Type)
		return Ada.Streams.Stream_Element_Array;
	procedure Set_Substitute (
		Object : in out Inout_Type;
		Substitute : Ada.Streams.Stream_Element_Array);
	
	-- stream access
	function Stream (Object : aliased in out Inout_Type)
		return not null access Ada.Streams.Root_Stream_Type'Class;
	
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
	
	-- bidirectional
	
	type Inout_Type is limited new Ada.Streams.Root_Stream_Type with record
		Internal : access constant String;
		External : access constant String;
		Stream : access Ada.Streams.Root_Stream_Type'Class;
		-- substitute (encoded as internal)
		Substitute_Length : Ada.Streams.Stream_Element_Offset;
		Substitute : Ada.Streams.Stream_Element_Array (
			1 ..
			Max_Substitute_Length);
		-- reading
		Reading_Converter : Converter;
		Reading_Context : Reading_Context_Type;
		-- writing
		Writing_Converter : Converter;
		Writing_Context : Writing_Context_Type;
	end record;
	
	overriding procedure Read (
		Object : in out Inout_Type;
		Item : out Ada.Streams.Stream_Element_Array;
		Last : out Ada.Streams.Stream_Element_Offset);
	overriding procedure Write (
		Object : in out Inout_Type;
		Item : in Ada.Streams.Stream_Element_Array);
	
end iconv.Streams;
