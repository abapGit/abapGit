REPORT zabapgit LINE-SIZE 100.

* See http://www.abapgit.org

CONSTANTS: gc_xml_version  TYPE string VALUE 'v1.0.0',      "#EC NOTEXT
           gc_abap_version TYPE string VALUE 'v1.26.4'.     "#EC NOTEXT

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
* dummy for triggering screen on Java SAP GUI
SELECTION-SCREEN END OF SCREEN 1001.

INCLUDE zabapgit_password_dialog. " !!! Contains SELECTION SCREEN

INCLUDE zabapgit_definitions.
INCLUDE zabapgit_macros.
INCLUDE zabapgit_exceptions.
INCLUDE zabapgit_zlib.
INCLUDE zabapgit_html.
INCLUDE zabapgit_util.
INCLUDE zabapgit_xml.

INCLUDE zabapgit_app.              " Some deferred definitions here
INCLUDE zabapgit_persistence_old.
INCLUDE zabapgit_persistence.
INCLUDE zabapgit_dot_abapgit.
INCLUDE zabapgit_sap_package.

INCLUDE zabapgit_stage.
INCLUDE zabapgit_git_helpers.
INCLUDE zabapgit_repo.
INCLUDE zabapgit_stage_logic.
INCLUDE zabapgit_http.
INCLUDE zabapgit_git.
INCLUDE zabapgit_objects.
INCLUDE zabapgit_tadir.
INCLUDE zabapgit_file_status.
INCLUDE zabapgit_popups.
INCLUDE zabapgit_zip.
INCLUDE zabapgit_objects_impl.

INCLUDE zabapgit_object_serializing.  " All serializing classes here

INCLUDE zabapgit_repo_impl.
INCLUDE zabapgit_background.
INCLUDE zabapgit_transport.

INCLUDE zabapgit_services.            " All services here
INCLUDE zabapgit_gui_asset_manager.
INCLUDE zabapgit_gui_pages.           " All GUI pages here
INCLUDE zabapgit_gui_pages_userexit IF FOUND.
INCLUDE zabapgit_gui_router.
INCLUDE zabapgit_gui.

INCLUDE zabapgit_app_impl.
INCLUDE zabapgit_unit_test.
INCLUDE zabapgit_migrations.          " Data migration routines
INCLUDE zabapgit_forms.

**********************************************************************
INITIALIZATION.
  lcl_password_dialog=>on_screen_init( ).

START-OF-SELECTION.
  PERFORM run.

* Hide Execute button from screen
AT SELECTION-SCREEN OUTPUT.
  IF sy-dynnr = lcl_password_dialog=>dynnr.
    lcl_password_dialog=>on_screen_output( ).
  ELSE.
    PERFORM output.
  ENDIF.

* SAP back command re-direction
AT SELECTION-SCREEN ON EXIT-COMMAND.
  PERFORM exit.

AT SELECTION-SCREEN.
  IF sy-dynnr = lcl_password_dialog=>dynnr.
    lcl_password_dialog=>on_screen_event( sscrfields-ucomm ).
  ENDIF.
