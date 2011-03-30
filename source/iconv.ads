with Ada.Finalization;
with Ada.IO_Exceptions;
private with C.errno;
private with System;
package iconv is
	pragma Preelaborate;
	pragma Linker_Options ("-liconv");
	
	package Errors is
		type Error_Status is (Fine, Invalid, Illegal_Sequence);
	private
		for Error_Status use (
			Fine => 0,
			Invalid => C.errno.EINVAL,
			Illegal_Sequence => C.errno.EILSEQ);
	end Errors;
	type Error_Status is new Errors.Error_Status;
	
	type Converter is limited private;
	
	function Open (To_Code, From_Code : String) return Converter;
	
	procedure Convert (
		Object : in Converter;
		In_Item : in String;
		In_Last : out Natural;
		Out_Item : out String;
		Out_Last : out Natural;
		Status : out Error_Status);
	
	procedure Convert (
		Object : in Converter;
		In_Item : in String;
		Out_Item : out String;
		Out_Last : out Natural;
		Invalid_Character : in Character := '?');
	
	function Convert (Object : Converter; S : String) return String;
	
	-- two-way
	
	type Encoding is limited private;
	
	function Open (Encoded, Decoded : String) return Encoding;
	
	function Encode (Object : Encoding; S : String) return String;
	function Decode (Object : Encoding; S : String) return String;
	
	-- get info
	
	procedure Iterate (Process : not null access procedure (Name : in String));
	
	-- exception
	
	Name_Error : exception renames Ada.IO_Exceptions.Name_Error;
	Status_Error : exception renames Ada.IO_Exceptions.Status_Error;
	
private
	
	type Converter is
		limited new Ada.Finalization.Limited_Controlled with
	record
		Handle : System.Address;
	end record;
	
	overriding procedure Finalize (Object : in out Converter);
	
	type Encoding is limited record
		Writing : Converter;
		Reading : Converter;
	end record;
	
	Max_Length_Of_Single_Character : constant := 6; -- UTF-8
	
end iconv;
