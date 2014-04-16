with iconv.Generic_Strings;
package iconv.Strings is
	new Generic_Strings (Character, String, "LATIN1");
-- Encoding / decoding between String and various encodings.
pragma Preelaborate (iconv.Strings);
