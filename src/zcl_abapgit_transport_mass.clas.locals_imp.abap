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

*================================ CLASSES =============================*

*================================  GUI ================================*
CLASS lcl_gui DEFINITION FINAL.

  PUBLIC SECTION.

    CLASS lcl_data_selector DEFINITION LOAD.

    CLASS-METHODS f4_folder CHANGING cv_folder TYPE string RAISING zcx_abapgit_exception.
    CLASS-METHODS open_folder_frontend IMPORTING iv_folder TYPE string.
    CLASS-METHODS select_tr_requests EXPORTING et_trkorr TYPE trwbo_request_headers.

  PRIVATE SECTION.
    CLASS-DATA: gv_last_folder TYPE string.

ENDCLASS.

CLASS lcl_gui IMPLEMENTATION.

  METHOD f4_folder.

    DATA: lv_folder TYPE string,
          lv_title  TYPE string.

    lv_title = 'Choose the destination folder for the ZIP files '(t01).

    cl_gui_frontend_services=>directory_browse(
      EXPORTING
        window_title         = lv_title
        initial_folder       = gv_last_folder
      CHANGING
        selected_folder      = lv_folder
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).

    IF sy-subrc EQ 0.

      IF lv_folder IS NOT INITIAL.

        cv_folder      = lv_folder.
        gv_last_folder = lv_folder. "Store the last directory for user friendly UI

      ELSE.

        cv_folder = gv_last_folder.

      ENDIF.

    ELSE.
      zcx_abapgit_exception=>raise( 'Folder matchcode exception'(002) ).
    ENDIF.

  ENDMETHOD.

  METHOD open_folder_frontend.

    IF NOT iv_folder IS INITIAL.

      cl_gui_frontend_services=>execute(
        EXPORTING
          document               = iv_folder
        EXCEPTIONS
          cntl_error             = 1
          error_no_gui           = 2
          bad_parameter          = 3
          file_not_found         = 4
          path_not_found         = 5
          file_extension_unknown = 6
          error_execute_failed   = 7
          OTHERS                 = 8 ).
      IF sy-subrc <> 0.
        MESSAGE 'Problem when opening output folder'(m05) TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD select_tr_requests.

    DATA: ls_popup     TYPE strhi_popup,
          ls_selection TYPE trwbo_selection.

    ls_popup-start_column = 5.
    ls_popup-start_row    = 5.

*- Prepare the selection ----------------------------------------------*
    ls_selection-trkorrpattern = space.
    ls_selection-client        = space.
    ls_selection-stdrequest    = space.
    ls_selection-reqfunctions  = 'K'.
    ls_selection-reqstatus     = 'RNODL'.

*- Call transport selection popup -------------------------------------*
    CALL FUNCTION 'TRINT_SELECT_REQUESTS'
      EXPORTING
        iv_username_pattern    = '*'
        iv_via_selscreen       = 'X'
        is_selection           = ls_selection
        iv_complete_projects   = space
        iv_title               = 'ABAPGit Transport Mass Downloader'(p01)
        is_popup               = ls_popup
      IMPORTING
        et_requests            = et_trkorr
      EXCEPTIONS
        action_aborted_by_user = 1
        OTHERS                 = 2.
    IF sy-subrc <> 0.
      CLEAR et_trkorr.
    ELSE.
      SORT et_trkorr BY trkorr.
      DELETE ADJACENT DUPLICATES FROM et_trkorr COMPARING trkorr.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

*=================== TRANSPORT ZIPPER =============================*
CLASS lcl_transport_zipper DEFINITION FINAL.

  PUBLIC SECTION.
* Folder
    TYPES ty_folder TYPE string.
* Filename
    TYPES ty_filename TYPE fb_icrc_pfile.
* File extension
    CONSTANTS gc_zip_ext TYPE string VALUE '.zip' ##NO_TEXT.

    DATA: gv_timestamp   TYPE string,
          gv_full_folder TYPE ty_folder READ-ONLY.

    METHODS constructor  IMPORTING iv_folder TYPE ty_folder
                         RAISING   zcx_abapgit_exception.

    METHODS generate_files IMPORTING it_trkorr TYPE trwbo_request_headers
                                     iv_logic  TYPE any
                           RAISING   zcx_abapgit_exception.

    CLASS-METHODS does_folder_exist IMPORTING iv_folder              TYPE string
                                    RETURNING VALUE(rv_folder_exist) TYPE abap_bool
                                    RAISING   zcx_abapgit_exception.

  PRIVATE SECTION.

    METHODS get_full_folder IMPORTING iv_folder             TYPE ty_folder
                            RETURNING VALUE(rv_full_folder) TYPE ty_folder
                            RAISING   zcx_abapgit_exception.

    METHODS save_binstring IMPORTING is_trkorr    TYPE trwbo_request_header
                                     iv_binstring TYPE xstring
                           RAISING   zcx_abapgit_exception.

ENDCLASS.

CLASS lcl_transport_zipper IMPLEMENTATION.

  METHOD constructor.

    CONCATENATE sy-datlo sy-timlo INTO me->gv_timestamp SEPARATED BY '_'.

    me->gv_full_folder = get_full_folder( iv_folder = iv_folder ).

  ENDMETHOD.

  METHOD does_folder_exist.

    cl_gui_frontend_services=>directory_exist(
      EXPORTING
        directory            = iv_folder
      RECEIVING
        result               = rv_folder_exist
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error from cl_gui_frontend_services=>directory_exist'(e03) ).
    ENDIF.

  ENDMETHOD.

  METHOD get_full_folder.

    DATA: lv_sep TYPE c,
          lv_rc  TYPE i.

*-obtain file separator character---------------------------------------
    cl_gui_frontend_services=>get_file_separator(
      CHANGING
        file_separator       = lv_sep
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Internal error getting file separator'(m03) ).
    ENDIF.

    CONCATENATE iv_folder
                gv_timestamp
           INTO rv_full_folder SEPARATED BY lv_sep.

    IF does_folder_exist( iv_folder = rv_full_folder ) = abap_false.

      cl_gui_frontend_services=>directory_create(
        EXPORTING
          directory                = rv_full_folder
        CHANGING
          rc                       = lv_rc    " Return Code
        EXCEPTIONS
          directory_create_failed  = 1
          cntl_error               = 2
          error_no_gui             = 3
          directory_access_denied  = 4
          directory_already_exists = 5
          path_not_found           = 6
          unknown_error            = 7
          not_supported_by_gui     = 8
          wrong_parameter          = 9
          OTHERS                   = 10
      ).
      IF sy-subrc <> 0 AND sy-subrc <> 5.
        zcx_abapgit_exception=>raise( 'Error from cl_gui_frontend_services=>directory_create'(e02) ).
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD save_binstring.

    DATA: lv_filename TYPE lcl_transport_zipper=>ty_filename.

    DATA:
      lt_rawdata  TYPE solix_tab.

* Generate filename
    CONCATENATE is_trkorr-trkorr '_' is_trkorr-as4text '_' gv_timestamp gc_zip_ext
    INTO lv_filename.

* Remove reserved characters (for Windows based systems)
    TRANSLATE lv_filename USING '/ \ : " * > < ? | '.

    lt_rawdata = cl_bcs_convert=>xstring_to_solix( iv_binstring ).

    CONCATENATE gv_full_folder lv_filename INTO lv_filename SEPARATED BY '\'.

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = xstrlen( iv_binstring )
        filename                  = lv_filename
        filetype                  = 'BIN'
      CHANGING
        data_tab                  = lt_rawdata
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from gui_download'(e01) ).
    ENDIF.


  ENDMETHOD.

  METHOD generate_files.

    DATA: ls_trkorr       LIKE LINE OF it_trkorr,
          lv_zipbinstring TYPE xstring.

    LOOP AT it_trkorr INTO ls_trkorr.

      lv_zipbinstring = zcl_abapgit_transport_mass=>zip( is_trkorr = ls_trkorr
                                                         iv_logic  = iv_logic ).

      me->save_binstring( iv_binstring = lv_zipbinstring
                          is_trkorr    = ls_trkorr ).

    ENDLOOP. "it_trkorr

  ENDMETHOD.

ENDCLASS.
