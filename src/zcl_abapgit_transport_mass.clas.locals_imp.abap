CLASS lcl_gui DEFINITION FINAL.

  PUBLIC SECTION.

    CLASS-METHODS f4_folder RETURNING VALUE(rv_folder) TYPE string RAISING zcx_abapgit_exception.
    CLASS-METHODS open_folder_frontend IMPORTING iv_folder TYPE string.
    CLASS-METHODS select_tr_requests RETURNING VALUE(rt_trkorr) TYPE trwbo_request_headers.

  PRIVATE SECTION.
    CLASS-DATA: gv_last_folder TYPE string.

ENDCLASS.

CLASS lcl_gui IMPLEMENTATION.

  METHOD f4_folder.

    DATA: lv_title  TYPE string.

    lv_title = 'Choose the destination folder for the ZIP files'.

    cl_gui_frontend_services=>directory_browse(
      EXPORTING
        window_title         = lv_title
        initial_folder       = gv_last_folder
      CHANGING
        selected_folder      = rv_folder
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).

    IF sy-subrc = 0.
      gv_last_folder = rv_folder. "Store the last directory for user friendly UI
    ELSE.
      zcx_abapgit_exception=>raise( 'Folder matchcode exception' ).
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
        MESSAGE 'Problem when opening output folder' TYPE 'S' DISPLAY LIKE 'E'.
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
        iv_title               = 'ABAPGit Transport Mass Downloader'
        is_popup               = ls_popup
      IMPORTING
        et_requests            = rt_trkorr
      EXCEPTIONS
        action_aborted_by_user = 1
        OTHERS                 = 2.
    IF sy-subrc <> 0.
      CLEAR rt_trkorr.
    ELSE.
      SORT rt_trkorr BY trkorr.
      DELETE ADJACENT DUPLICATES FROM rt_trkorr COMPARING trkorr.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_transport_zipper DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES ty_folder TYPE string.
    TYPES ty_filename TYPE string.

* File extension
    CONSTANTS gc_zip_ext TYPE string VALUE '.zip'.

    METHODS constructor  IMPORTING iv_folder TYPE ty_folder
                         RAISING   zcx_abapgit_exception.

    METHODS generate_files IMPORTING it_trkorr TYPE trwbo_request_headers
                                     ig_logic  TYPE any
                           RAISING   zcx_abapgit_exception.

    METHODS get_folder RETURNING VALUE(rv_full_folder) TYPE ty_folder.

    CLASS-METHODS does_folder_exist IMPORTING iv_folder              TYPE string
                                    RETURNING VALUE(rv_folder_exist) TYPE abap_bool
                                    RAISING   zcx_abapgit_exception.

  PRIVATE SECTION.
    DATA: mv_timestamp   TYPE string,
          mv_separator   TYPE c,
          mv_full_folder TYPE ty_folder.

    METHODS get_full_folder IMPORTING iv_folder             TYPE ty_folder
                            RETURNING VALUE(rv_full_folder) TYPE ty_folder
                            RAISING   zcx_abapgit_exception.

    METHODS get_filename IMPORTING is_trkorr          TYPE trwbo_request_header
                         RETURNING VALUE(rv_filename) TYPE ty_filename.

ENDCLASS.

CLASS lcl_transport_zipper IMPLEMENTATION.

  METHOD constructor.

    CONCATENATE sy-datlo sy-timlo INTO mv_timestamp SEPARATED BY '_'.

    mv_full_folder = get_full_folder( iv_folder = iv_folder ).

    cl_gui_frontend_services=>get_file_separator(
      CHANGING
        file_separator       = mv_separator
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).
    IF sy-subrc <> 0.
      mv_separator = '\'. "Default MS Windows separator
    ENDIF.

  ENDMETHOD.

  METHOD get_folder.
    rv_full_folder = mv_full_folder.
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
      zcx_abapgit_exception=>raise( 'Error from cl_gui_frontend_services=>directory_exist' ).
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
      zcx_abapgit_exception=>raise( 'Internal error getting file separator' ).
    ENDIF.

    CONCATENATE iv_folder
                mv_timestamp
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
          OTHERS                   = 10 ).
      IF sy-subrc <> 0 AND sy-subrc <> 5.
        zcx_abapgit_exception=>raise( 'Error from cl_gui_frontend_services=>directory_create' ).
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD get_filename.

* Generate filename
    CONCATENATE is_trkorr-trkorr '_' is_trkorr-as4text '_' mv_timestamp gc_zip_ext
      INTO rv_filename.

* Remove reserved characters (for Windows based systems)
    TRANSLATE rv_filename USING '/ \ : " * > < ? | '.

    CONCATENATE mv_full_folder rv_filename INTO rv_filename SEPARATED BY mv_separator.

  ENDMETHOD.

  METHOD generate_files.

    DATA: ls_trkorr       LIKE LINE OF it_trkorr,
          lv_zipbinstring TYPE xstring.

    LOOP AT it_trkorr INTO ls_trkorr.

      lv_zipbinstring = zcl_abapgit_transport_mass=>zip( is_trkorr         = ls_trkorr
                                                         iv_logic          = ig_logic
                                                         iv_show_log_popup = abap_false ).

      zcl_abapgit_zip=>save_binstring_to_localfile( iv_binstring = lv_zipbinstring
                                                    iv_filename  = get_filename( ls_trkorr ) ).

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
