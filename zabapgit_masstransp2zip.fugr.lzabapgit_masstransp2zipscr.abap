********************************************************************************
* The MIT License (MIT)
*
* Copyright (c) 2019 abapGit Contributors
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
********************************************************************************
*( )_( )
*(='.'=)
*(")_(")
* This program allow to generate Abapgit ZIP files from transport request(s)
* in a given folder with the given logic ( FULL or PREFIX )

*======================== SELECTION SCREEN ============================*
SELECTION-SCREEN BEGIN OF SCREEN 100 TITLE TEXT-tit.

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE TEXT-t01.

* Transport request
SELECT-OPTIONS s_trkorr FOR lcl_data_selector=>gv_trkorr OBLIGATORY.

* Output Folder
PARAMETERS p_folder TYPE lcl_transport_zipper=>ty_folder OBLIGATORY LOWER CASE MEMORY ID fol.

* Package logic
PARAMETERS p_logic TYPE lcl_transport_zipper=>ty_logic OBLIGATORY DEFAULT lcl_transport_zipper=>gc_logic_full.

SELECTION-SCREEN END OF BLOCK a.

SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME TITLE TEXT-t02.

* Display ALV log
PARAMETERS p_dsplog TYPE flag AS CHECKBOX.

* Open folder after file generation.
PARAMETERS p_openfl TYPE flag AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK b.

SELECTION-SCREEN END OF SCREEN 100.
*======================================================================*

*============================= EVENTS =================================*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_folder.
  lcl_gui=>f4_folder( CHANGING cv_folder = p_folder ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_trkorr-low.
  lcl_gui=>f4_transport_request( CHANGING cv_trkorr = s_trkorr-low ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_trkorr-high.
  lcl_gui=>f4_transport_request( CHANGING cv_trkorr = s_trkorr-high ).

AT SELECTION-SCREEN.
  PERFORM f_check_folder USING p_folder.

*======================================================================*
