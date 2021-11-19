CLASS lcl_gui DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS f4_folder
      RETURNING
        VALUE(rv_folder) TYPE string
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS open_folder_frontend
      IMPORTING
        iv_folder TYPE string
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS select_tr_requests
      RETURNING
        VALUE(rt_trkorr) TYPE trwbo_request_headers.

  PRIVATE SECTION.
    CLASS-DATA gv_last_folder TYPE string.

ENDCLASS.

CLASS lcl_gui IMPLEMENTATION.

  METHOD f4_folder.

    DATA: lv_title   TYPE string,
          lo_fe_serv TYPE REF TO zif_abapgit_frontend_services.

    lo_fe_serv = zcl_abapgit_ui_factory=>get_frontend_services( ).
    lv_title = 'Choose the destination folder for the ZIP files'.

    lo_fe_serv->directory_browse(
      EXPORTING
         iv_window_title   = lv_title
         iv_initial_folder = gv_last_folder
      CHANGING
        cv_selected_folder = rv_folder ).

    "Store the last directory for user friendly UI
    gv_last_folder = rv_folder.

  ENDMETHOD.

  METHOD open_folder_frontend.
    IF iv_folder IS INITIAL.
      RETURN.
    ENDIF.

    zcl_abapgit_ui_factory=>get_frontend_services( )->execute( iv_document = iv_folder ).
  ENDMETHOD.

  METHOD select_tr_requests.

    DATA: ls_popup     TYPE strhi_popup,
          ls_selection TYPE trwbo_selection.

    ls_popup-start_column = 5.
    ls_popup-start_row    = 5.

    " Prepare the selection
    ls_selection-trkorrpattern = space.
    ls_selection-client        = space.
    ls_selection-stdrequest    = space.
    ls_selection-reqfunctions  = 'K'.
    ls_selection-reqstatus     = 'RNODL'.

    " Call transport selection popup
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

    CONSTANTS c_zip_ext TYPE string VALUE '.zip'.

    METHODS constructor
      IMPORTING
        iv_folder TYPE ty_folder
      RAISING
        zcx_abapgit_exception.

    METHODS generate_files
      IMPORTING
        it_trkorr TYPE trwbo_request_headers
        ig_logic  TYPE any
      RAISING
        zcx_abapgit_exception.

    METHODS get_folder
      RETURNING
        VALUE(rv_full_folder) TYPE ty_folder.

    CLASS-METHODS does_folder_exist
      IMPORTING
        iv_folder              TYPE string
      RETURNING
        VALUE(rv_folder_exist) TYPE abap_bool
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.
    DATA: mv_timestamp   TYPE string,
          mv_separator   TYPE c,
          mv_full_folder TYPE ty_folder.

    METHODS get_full_folder
      IMPORTING
        iv_folder             TYPE ty_folder
      RETURNING
        VALUE(rv_full_folder) TYPE ty_folder
      RAISING
        zcx_abapgit_exception.

    METHODS get_filename
      IMPORTING
        is_trkorr          TYPE trwbo_request_header
      RETURNING
        VALUE(rv_filename) TYPE ty_filename.

ENDCLASS.

CLASS lcl_transport_zipper IMPLEMENTATION.

  METHOD constructor.
    DATA lo_fe_serv TYPE REF TO zif_abapgit_frontend_services.

    lo_fe_serv = zcl_abapgit_ui_factory=>get_frontend_services( ).

    mv_timestamp = |{ sy-datlo }_{ sy-timlo }|.
    mv_full_folder = get_full_folder( iv_folder ).

    TRY.
        lo_fe_serv->get_file_separator( CHANGING cv_file_separator = mv_separator ).
      CATCH zcx_abapgit_exception.
        "Default MS Windows separator
        mv_separator = '\'.
    ENDTRY.
  ENDMETHOD.

  METHOD get_folder.
    rv_full_folder = mv_full_folder.
  ENDMETHOD.

  METHOD does_folder_exist.
    rv_folder_exist = zcl_abapgit_ui_factory=>get_frontend_services( )->directory_exist( iv_directory = iv_folder ).
  ENDMETHOD.

  METHOD get_full_folder.

    DATA: lv_sep     TYPE c,
          lv_rc      TYPE i,
          lo_fe_serv TYPE REF TO zif_abapgit_frontend_services.

    lo_fe_serv = zcl_abapgit_ui_factory=>get_frontend_services( ).

    lo_fe_serv->get_file_separator( CHANGING cv_file_separator = lv_sep ).
    rv_full_folder = |{ iv_folder }{ lv_sep }{ mv_timestamp }|.

    IF does_folder_exist( rv_full_folder ) = abap_false.
      lo_fe_serv->directory_create(
        EXPORTING
          iv_directory = rv_full_folder
        CHANGING
          cv_rc        = lv_rc ).
    ENDIF.
  ENDMETHOD.

  METHOD get_filename.

    " Generate filename
    rv_filename = |{ is_trkorr-trkorr }_{ is_trkorr-as4text }_{ mv_timestamp }{ c_zip_ext }|.

    " Remove reserved characters (for Windows based systems)
    TRANSLATE rv_filename USING '/ \ : " * > < ? | '.

    rv_filename = |{ mv_full_folder }{ mv_separator }{ rv_filename }|.

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
