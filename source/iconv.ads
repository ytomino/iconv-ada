with Ada.Finalization;
with Ada.IO_Exceptions;
with Ada.Streams;
private with System;
package iconv is
	pragma Preelaborate;
	
	-- get info
	
	function Version return String;
	
	pragma Inline (Version); -- renamed
	
	procedure Iterate (Process : not null access procedure (Name : in String));
	
	pragma Inline (Iterate); -- renamed
	
	-- subsidiary types to converter
	
	type Subsequence_Status_Type is (
		Finished,
		Success,
		Overflow, -- the output buffer is not large enough
		Illegal_Sequence, -- a input character could not be mapped to the output
		Truncated); -- the input buffer is broken off at a multi-byte character
	
	type Continuing_Status_Type is
		new Subsequence_Status_Type range
			Success .. Subsequence_Status_Type'Last;
	type Finishing_Status_Type is
		new Subsequence_Status_Type range
			Finished .. Overflow;
	type Status_Type is
		new Subsequence_Status_Type range
			Finished .. Illegal_Sequence;
	
	type Substituting_Status_Type is
		new Status_Type range
			Finished .. Overflow;
	
	subtype True_Only is Boolean range True .. True;
	
	-- converter
	
	type Converter is limited private;
	
--	subtype Open_Converter is Converter
--		with
--			Dynamic_Predicate => Is_Open (Open_Converter),
--			Predicate_Failure => raise Status_Error;
	
	procedure Open (Object : in out Converter; To : in String; From : in String);
	function Open (To : String; From : String) return Converter;
	
	function Is_Open (Object : Converter) return Boolean;
	
	function Min_Size_In_From_Stream_Elements (
		Object : Converter) -- Open_Converter
		return Ada.Streams.Stream_Element_Offset;
	
	function Substitute (
		Object : Converter) -- Open_Converter
		return Ada.Streams.Stream_Element_Array;
	
	procedure Set_Substitute (
		Object : in out Converter; -- Open_Converter
		Substitute : in Ada.Streams.Stream_Element_Array);
	
	-- convert subsequence
	procedure Convert (
		Object : in Converter; -- Open_Converter
		In_Item : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Finish : in Boolean;
		Status : out Subsequence_Status_Type);
	
	procedure Convert (
		Object : in Converter; -- Open_Converter
		In_Item : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Status : out Continuing_Status_Type);
	
	procedure Convert (
		Object : in Converter; -- Open_Converter
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Finish : in True_Only;
		Status : out Finishing_Status_Type);
	
	-- convert all character sequence
	procedure Convert (
		Object : in Converter; -- Open_Converter
		In_Item : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Finish : in True_Only;
		Status : out Status_Type);
	
	-- convert all character sequence with substitute
	procedure Convert (
		Object : in Converter; -- Open_Converter
		In_Item : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Finish : in True_Only;
		Status : out Substituting_Status_Type);
	
	-- exceptions
	
	Status_Error : exception
		renames Ada.IO_Exceptions.Status_Error;
	Name_Error : exception
		renames Ada.IO_Exceptions.Name_Error;
	Use_Error : exception
		renames Ada.IO_Exceptions.Use_Error;
	
private
	
	-- max length of one multi-byte character
	
	Max_Length_Of_Single_Character : constant := 8;
	Max_Substitute_Length : constant := Max_Length_Of_Single_Character;
	
	-- For example: ISO-8859-1 "\xA2" ("¢") is converted to
	-- ISO-2022-JP "\x1B\x24\x42\x21\x71\x1B\x28\x42".
	
	-- converter
	
	type Non_Controlled_Converter is record
		Handle : System.Address;
		-- about "From"
		Min_Size_In_From_Stream_Elements : Ada.Streams.Stream_Element_Offset;
		-- about "To"
		Substitute_Length : Ada.Streams.Stream_Element_Offset;
		Substitute : Ada.Streams.Stream_Element_Array (1 .. Max_Substitute_Length);
	end record;
	pragma Suppress_Initialization (Non_Controlled_Converter);
	
	package Controlled is
		
		type Converter is limited private;
		
		function Variable_View (Object : Converter) return not null access Converter;
		pragma Inline (Variable_View);
		
		function Reference (Object : in out iconv.Converter)
			return not null access Non_Controlled_Converter;
		function Constant_Reference (Object : iconv.Converter)
			return not null access constant Non_Controlled_Converter;
		
		pragma Inline (Reference);
		pragma Inline (Constant_Reference);
		
	private
		
		type Converter is limited new Ada.Finalization.Limited_Controlled
			with record
				Variable_View : not null access Converter := Converter'Unchecked_Access;
				Data : aliased Non_Controlled_Converter :=
					(Handle => System.Null_Address, others => <>);
			end record;
		
		overriding procedure Finalize (Object : in out Converter);
		
	end Controlled;
	
	type Converter is new Controlled.Converter;
	
	procedure Do_Open (Object : out Converter; To : in String; From : in String);
	
	procedure Reset_State (Object : in Converter);
	
	procedure Put_Substitute (
		Object : in Converter;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Is_Overflow : out Boolean);
	
end iconv;
