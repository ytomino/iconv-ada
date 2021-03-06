iconv interface library for gcc-Ada (GNAT)
==========================================

What's this?
------------

Ada binding to the International Codeset Conversion Library.

Depending tools and libraries
-----------------------------

libiconv (GNU version)
 http://www.gnu.org/software/libiconv/
headmaster
 http://github.com/ytomino/headmaster

Usage
-----

1. Translate C headers with headmaster. ::
   
    $ headmaster --to ada -p -D import-dir iconv-ada/source/import.h

2. Add source directories of iconv-ada and translated headers
   to search path for gnatmake. ::
   
    $ gnatmake -Iiconv-ada/source -Iiconv-ada/source/libiconv -Iimport-dir your_main.adb
   
   If iconv is provided by glibc in your system (Linux), some functions of
   libiconv are missing. So use source/glibc instead of source/libiconv. ::
   
    $ gnatmake -Iiconv-ada/source -Iiconv-ada/source/glibc -Iimport-dir your_main.adb
   
   Or please write .gpr file for your environment.

License
-------

It is dual-licensed under the New BSD License and LGPL, see below.
Please apply LGPL when static linking libiconv.a of GNU version.

**license of iconv-ada (1)** ::

 Copyright 2010-2020 YT. All rights reserved.
 
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 
 THIS SOFTWARE IS PROVIDED BY THE AUTHOR(S) ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR(S) BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

**license of iconv-ada (2) and GNU libiconv** ::

 This file is part of iconv-ada.
 
 iconv-ada is free software: you can redistribute it and/or modify
 it under the terms of the GNU Library General Public License as published by
 the Free Software Foundation, either version 2 of the License, or
 (at your option) any later version.
 
 iconv-ada is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU Library General Public License for more details.
 
 You should have received a copy of the GNU Library General Public License
 along with iconv-ada.  If not, see <http://www.gnu.org/licenses/>.
