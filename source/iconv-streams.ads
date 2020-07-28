with Ada.Streams;
package iconv.Streams is
	pragma Preelaborate;
	
	-- only reading
	
	type In_Type is limited private;
	
--	subtype Open_In_Type is In_Type
--		with
--			Dynamic_Predicate => Is_Open (Open_In_Type),
--			Predicate_Failure => raise Status_Error;
	
	-- management
	function Open (
		Decoder : in out Converter; -- neither access nor aliased for derived types
		Stream : not null access Ada.Streams.Root_Stream_Type'Class)
		return In_Type;
	function Is_Open (Object : In_Type) return Boolean;
	
	pragma Inline (Is_Open);
	
	-- stream access
	function Stream (
		Object : aliased in out In_Type) -- Open_In_Type
		return not null access Ada.Streams.Root_Stream_Type'Class;
	
	-- only writing
	
	type Out_Type is limited private;
	
--	subtype Open_Out_Type is In_Type
--		with
--			Dynamic_Predicate => Is_Open (Open_Out_Type),
--			Predicate_Failure => raise Status_Error;
	
	-- management
	function Open (
		Encoder : in out Converter; -- same as above
		Stream : not null access Ada.Streams.Root_Stream_Type'Class)
		return Out_Type;
	function Is_Open (Object : Out_Type) return Boolean;
	
	pragma Inline (Is_Open);
	
	-- stream access
	function Stream (
		Object : aliased in out Out_Type) -- Open_Out_Type
		return not null access Ada.Streams.Root_Stream_Type'Class;
	
	-- finish writing
	procedure Finish (
		Object : in out Out_Type); -- Open_Out_Type
	
	-- bidirectional
	
	type Inout_Type is limited private;
	
--	subtype Open_Inout_Type is In_Type
--		with
--			Dynamic_Predicate => Is_Open (Open_Inout_Type),
--			Predicate_Failure => raise Status_Error;
	
	-- management
	function Open (
		Internal : not null access constant String;
		External : not null access constant String;
		Stream : not null access Ada.Streams.Root_Stream_Type'Class)
		return Inout_Type;
	function Is_Open (Object : Inout_Type) return Boolean;
	
	pragma Inline (Is_Open);
	
	-- substitute (encoded as internal)
	function Substitute (
		Object : Inout_Type) -- Open_Inout_Type
		return Ada.Streams.Stream_Element_Array;
	procedure Set_Substitute (
		Object : in out Inout_Type; -- Open_Inout_Type
		Substitute : in Ada.Streams.Stream_Element_Array);
	
	-- stream access
	function Stream (
		Object : aliased in out Inout_Type) -- Open_Inout_Type
		return not null access Ada.Streams.Root_Stream_Type'Class;
	
	-- finish writing
	procedure Finish (
		Object : in out Inout_Type); -- Open_Inout_Type
	
	-- exceptions
	
	Mode_Error : exception
		renames Ada.IO_Exceptions.Mode_Error;
	End_Error : exception
		renames Ada.IO_Exceptions.End_Error;
	
private
	use type Ada.Streams.Stream_Element_Offset;
	
	Half_Buffer_Length : constant := 64;
	
	subtype Buffer_Type is
		Ada.Streams.Stream_Element_Array (0 .. 2 * Half_Buffer_Length - 1);
	
	type Reading_Status_Type is (Continuing, Finishing, Ended);
	pragma Discard_Names (Reading_Status_Type);
	
	type Reading_Context_Type is record
		Buffer : Buffer_Type;
		First : Ada.Streams.Stream_Element_Offset;
		Last : Ada.Streams.Stream_Element_Offset;
		Converted_Buffer : Buffer_Type;
		Converted_First : Ada.Streams.Stream_Element_Offset;
		Converted_Last : Ada.Streams.Stream_Element_Offset;
		Status : Reading_Status_Type;
	end record;
	pragma Suppress_Initialization (Reading_Context_Type);
	
	type Writing_Context_Type is record
		Buffer : Buffer_Type;
		First : Ada.Streams.Stream_Element_Offset;
		Last : Ada.Streams.Stream_Element_Offset;
	end record;
	pragma Suppress_Initialization (Writing_Context_Type);
	
	-- only reading
	
	type In_Type is limited new Ada.Streams.Root_Stream_Type with record
		Stream : access Ada.Streams.Root_Stream_Type'Class;
		Reading_Converter : access Converter;
		Reading_Context : Reading_Context_Type;
	end record;
	
	overriding procedure Read (
		Object : in out In_Type;
		Item : out Ada.Streams.Stream_Element_Array;
		Last : out Ada.Streams.Stream_Element_Offset);
	overriding procedure Write (
		Object : in out In_Type;
		Item : in Ada.Streams.Stream_Element_Array);
	
	-- only writing
	
	type Out_Type is limited new Ada.Streams.Root_Stream_Type with record
		Stream : access Ada.Streams.Root_Stream_Type'Class;
		Writing_Converter : access Converter;
		Writing_Context : Writing_Context_Type;
	end record;
	
	overriding procedure Read (
		Object : in out Out_Type;
		Item : out Ada.Streams.Stream_Element_Array;
		Last : out Ada.Streams.Stream_Element_Offset);
	overriding procedure Write (
		Object : in out Out_Type;
		Item : in Ada.Streams.Stream_Element_Array);
	
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
