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


*&---------------------------------------------------------------------*
*& Form F_MAIN
*&---------------------------------------------------------------------*
FORM f_main.

  DATA:
    lt_trkorr   TYPE lcl_data_selector=>tt_trkorr,
    ls_sel_crit TYPE lcl_data_selector=>ty_sel_criterias.

  DATA:
    lo_transport_zipper TYPE REF TO lcl_transport_zipper,
    lo_reporter         TYPE REF TO lcl_reporter,
    lo_except           TYPE REF TO cx_root.

  ls_sel_crit-t_r_trkorr[] = s_trkorr[].

  TRY.

      lt_trkorr = lcl_data_selector=>get_transport_requests( is_sel_crit = ls_sel_crit ).

      IF lt_trkorr[] IS NOT INITIAL.

* Instantiate reporter
        CREATE OBJECT lo_reporter TYPE lcl_reporter.

* Instantiate transport zipper object that will also create the timestamped output folder
        CREATE OBJECT lo_transport_zipper TYPE lcl_transport_zipper
          EXPORTING
            iv_folder   = p_folder
            io_reporter = lo_reporter.

* Generate the local zip files from the given list of transport requests
        lo_transport_zipper->generate_files( it_trkorr = lt_trkorr
                                             iv_logic  = p_logic ).

* Open output folder if user asked it
        IF p_openfl EQ abap_true.

          lcl_gui=>open_folder_frontend( iv_folder = lo_transport_zipper->gv_full_folder  ).

        ENDIF.

      ELSE.
* No data found for the provided selection criterias
        zcx_abapgit_exception=>raise( 'No data found for the provided selection criterias' ).
      ENDIF.

      IF p_dsplog EQ abap_true.
        lo_reporter->display_report( iv_start_column = 5
                                     iv_end_column   = 140
                                     iv_start_line   = 5
                                     iv_end_line     = 20 ).
      ENDIF.

    CATCH cx_wrong_data
          zcx_abapgit_exception INTO lo_except.

      MESSAGE lo_except->get_text( ) TYPE 'E'.

  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_CHECK_FOLDER
*&---------------------------------------------------------------------*
FORM f_check_folder  USING iv_folder TYPE string.

  DATA: lo_except TYPE REF TO cx_root,
        lv_error  TYPE string.

  TRY.
      IF lcl_transport_zipper=>does_folder_exist( iv_folder = iv_folder ) = abap_false.
        SET CURSOR FIELD 'P_FOLDER'.
* Invalid folder %1

* No data found for the provided selection criterias
        CONCATENATE 'Invalid folder'(m04) iv_folder INTO lv_error SEPARATED BY space.
        zcx_abapgit_exception=>raise( lv_error ).

      ENDIF.

    CATCH zcx_abapgit_exception INTO lo_except.

      MESSAGE lo_except->get_text( ) TYPE 'E'.

  ENDTRY.


ENDFORM.
