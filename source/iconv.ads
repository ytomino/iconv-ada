pragma Ada_2012;
with Ada.Finalization;
with Ada.IO_Exceptions;
with Ada.Streams;
private with C.errno;
private with System;
package iconv is
	pragma Preelaborate;
	
	-- get info
	
	function Version return String;
	
	procedure Iterate (Process : not null access procedure (Name : in String));
	
	-- subsidiary types to converter
	
	package Errors is
		type Error_Status is (Fine, Invalid, Illegal_Sequence);
	private
		for Error_Status use (
			Fine => 0,
			Invalid => C.errno.EINVAL,
			Illegal_Sequence => C.errno.EILSEQ);
	end Errors;
	type Error_Status is new Errors.Error_Status;
	
	-- converter
	
	type Converter is limited private;
	
	procedure Open (
		Object : in out Converter;
		To : in String;
		From : in String);
	function Open (
		To : String;
		From : String)
		return Converter;
	function Is_Open (Object : Converter) return Boolean;
	
	function Substitute (Object : Converter)
		return Ada.Streams.Stream_Element_Array;
	
	procedure Set_Substitute (
		Object : in out Converter;
		Substitute : in Ada.Streams.Stream_Element_Array);
	
	-- convert subsequence
	procedure Convert (
		Object : in Converter;
		In_Item : in Ada.Streams.Stream_Element_Array;
		In_Last : out Ada.Streams.Stream_Element_Offset;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Status : out Error_Status);
	
	-- convert all character sequence with substitute
	procedure Convert (
		Object : in Converter;
		In_Item : in Ada.Streams.Stream_Element_Array;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset);
	
	-- two-way
	
	type Encoding is limited private;
	
	function Open (Encoded, Decoded : String) return Encoding;
	function Is_Open (Object : Encoding) return Boolean;
	
	-- exceptions
	
	Name_Error : exception
		renames Ada.IO_Exceptions.Name_Error;
	Status_Error : exception
		renames Ada.IO_Exceptions.Status_Error;
	
private
	
	-- max length of one multi-byte character
	
	Max_Length_Of_Single_Character : constant := 6; -- UTF-8
	Max_Substitute_Length : constant := Max_Length_Of_Single_Character;
	
	-- converter
	
	type Non_Controlled_Converter is record
		-- about "To"
		Substitute_Length : Ada.Streams.Stream_Element_Offset;
		Substitute : Ada.Streams.Stream_Element_Array (
			1 ..
			Max_Substitute_Length);
	end record;
	pragma Suppress_Initialization (Non_Controlled_Converter);
	
	package Controlled is
		
		type Converter is limited private;
		
		procedure Open (
			Object : in out Converter;
			To_Code, From_Code : not null access constant C.char);
		
		function Handle (Object : Converter) return System.Address;
		
		pragma Inline (Handle);
		
		function Reference (Object : in out Converter)
			return not null access Non_Controlled_Converter;
		function Constant_Reference (Object : Converter)
			return not null access constant Non_Controlled_Converter;
		
		pragma Inline (Reference);
		pragma Inline (Constant_Reference);
		
	private
		
		type Converter is
			limited new Ada.Finalization.Limited_Controlled with
		record
			Handle : aliased System.Address := System.Null_Address;
			Data : aliased Non_Controlled_Converter;
		end record;
		
		overriding procedure Finalize (Object : in out Converter);
		
	end Controlled;
	
	type Converter is new Controlled.Converter;
	
	procedure Put_Substitute (
		Object : in Converter;
		Out_Item : out Ada.Streams.Stream_Element_Array;
		Out_Last : out Ada.Streams.Stream_Element_Offset;
		Is_Overflow : out Boolean);
	
	type Encoding is limited record
		Writing : Converter;
		Reading : Converter;
	end record;
	
end iconv;
