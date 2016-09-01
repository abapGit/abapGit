REPORT zabapgit LINE-SIZE 100.

* See http://www.abapgit.org

CONSTANTS: gc_xml_version  TYPE string VALUE 'v1.0.0',      "#EC NOTEXT
           gc_abap_version TYPE string VALUE 'v1.17.6'.     "#EC NOTEXT

********************************************************************************
* The MIT License (MIT)
*
* Copyright (c) 2014 abapGit Contributors
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

SELECTION-SCREEN BEGIN OF SCREEN 1001.
* dummy for triggering screen
SELECTION-SCREEN END OF SCREEN 1001.

INCLUDE zabapgit_password_dialog. " !!! Contains SELECTION SCREEN

INCLUDE zabapgit_definitions.
INCLUDE zabapgit_exceptions.
INCLUDE zabapgit_zlib.
INCLUDE zabapgit_util.
INCLUDE zabapgit_xml.

CLASS lcl_gui DEFINITION DEFERRED.
CLASS lcl_persistence_user DEFINITION DEFERRED.
CLASS lcl_repo_srv DEFINITION DEFERRED.
CLASS lcl_persistence_db DEFINITION DEFERRED.

INCLUDE zabapgit_app.
INCLUDE zabapgit_persistence.
INCLUDE zabapgit_html.
INCLUDE zabapgit_dot_abapgit.
INCLUDE zabapgit_sap_package.

CLASS lcl_repo_online DEFINITION DEFERRED.

INCLUDE zabapgit_stage.
INCLUDE zabapgit_repo.
INCLUDE zabapgit_stage_logic.
INCLUDE zabapgit_objects.
INCLUDE zabapgit_tadir.
INCLUDE zabapgit_file_status.
INCLUDE zabapgit_object.
INCLUDE zabapgit_object_acid.
INCLUDE zabapgit_object_auth.
INCLUDE zabapgit_object_doct.
INCLUDE zabapgit_object_docv.
INCLUDE zabapgit_object_doma.
INCLUDE zabapgit_object_dtel.
INCLUDE zabapgit_object_enho.
INCLUDE zabapgit_object_enhs.
INCLUDE zabapgit_object_enqu.
INCLUDE zabapgit_object_ensc.
INCLUDE zabapgit_object_iarp.
INCLUDE zabapgit_object_iasp.
INCLUDE zabapgit_object_iatu.
INCLUDE zabapgit_object_msag.
INCLUDE zabapgit_object_nrob.
INCLUDE zabapgit_object_para.
INCLUDE zabapgit_object_pinf.
INCLUDE zabapgit_object_sfbf.
INCLUDE zabapgit_object_sfbs.
INCLUDE zabapgit_object_sfsw.
INCLUDE zabapgit_object_shi3.
INCLUDE zabapgit_object_shlp.
INCLUDE zabapgit_object_sicf.
INCLUDE zabapgit_object_smim.
INCLUDE zabapgit_object_splo.
INCLUDE zabapgit_object_ssfo.
INCLUDE zabapgit_object_ssst.
INCLUDE zabapgit_object_susc.
INCLUDE zabapgit_object_suso.
INCLUDE zabapgit_object_tabl.
INCLUDE zabapgit_object_tobj.
INCLUDE zabapgit_object_tran.
INCLUDE zabapgit_object_ttyp.
INCLUDE zabapgit_object_type.
INCLUDE zabapgit_object_vcls.
INCLUDE zabapgit_object_view.
INCLUDE zabapgit_object_w3xx.
INCLUDE zabapgit_object_wdya.
INCLUDE zabapgit_object_wdyn.
INCLUDE zabapgit_object_webi.
INCLUDE zabapgit_object_xslt.

INCLUDE zabapgit_git.
INCLUDE zabapgit_repo_impl.
INCLUDE zabapgit_background.
INCLUDE zabapgit_zip.
INCLUDE zabapgit_transport.
INCLUDE zabapgit_popups.

INCLUDE zabapgit_page.
INCLUDE zabapgit_page_commit.
INCLUDE zabapgit_page_merge.
INCLUDE zabapgit_page_branch_overview.
INCLUDE zabapgit_page_stage.
INCLUDE zabapgit_page_db.
INCLUDE zabapgit_page_main.
INCLUDE zabapgit_page_background.
INCLUDE zabapgit_gui.
INCLUDE zabapgit_app_impl.
INCLUDE zabapgit_unit_test.
INCLUDE zabapgit_forms.

INITIALIZATION.
  lcl_password_dialog=>on_screen_init( ).

START-OF-SELECTION.
  PERFORM run.

* Hide Execute button from screen
AT SELECTION-SCREEN OUTPUT.
  IF sy-dynnr = lcl_password_dialog=>DYNNR.
    lcl_password_dialog=>on_screen_output( ).
  ELSE.
    PERFORM output.
  ENDIF.

* SAP back command re-direction
AT SELECTION-SCREEN ON EXIT-COMMAND.
  PERFORM exit.

AT SELECTION-SCREEN.
  IF sy-dynnr = lcl_password_dialog=>DYNNR.
    lcl_password_dialog=>on_screen_event( sscrfields-ucomm ).
  ENDIF.