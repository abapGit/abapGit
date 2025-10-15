CLASS zcl_abapgit_flow_page_utils DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS render_table
      IMPORTING
        it_files                TYPE zif_abapgit_flow_logic=>ty_path_name_tt
        it_transport_duplicates TYPE zif_abapgit_flow_logic=>ty_transport_duplicates_tt OPTIONAL
        is_user_settings        TYPE zif_abapgit_persist_user=>ty_flow_settings OPTIONAL
        iv_repo_key             TYPE zif_abapgit_persistence=>ty_repo-key
      RETURNING
        VALUE(ri_html)          TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS call_diff
      IMPORTING
        !ii_event         TYPE REF TO zif_abapgit_gui_event
      RETURNING
        VALUE(rs_handled) TYPE zif_abapgit_gui_event_handler=>ty_handling_result
      RAISING
        zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_FLOW_PAGE_UTILS IMPLEMENTATION.


  METHOD call_diff.

    DATA lv_key TYPE zif_abapgit_persistence=>ty_value.
    DATA lv_remote_sha1 TYPE zif_abapgit_git_definitions=>ty_sha1.
    DATA ls_file TYPE zif_abapgit_git_definitions=>ty_file.
    DATA li_repo_online TYPE REF TO zif_abapgit_repo_online.
    DATA li_repo TYPE REF TO zif_abapgit_repo.
    DATA lv_blob TYPE xstring.
    DATA ls_local TYPE zif_abapgit_git_definitions=>ty_file.
    DATA ls_remote TYPE zif_abapgit_git_definitions=>ty_file.
    DATA lt_filter TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA lo_filter TYPE REF TO zcl_abapgit_object_filter_obj.
    DATA lt_files_item TYPE zif_abapgit_definitions=>ty_files_item_tt.
    DATA ls_file_item LIKE LINE OF lt_files_item.
    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS <ls_filter> LIKE LINE OF lt_filter.

    lv_key = ii_event->query( )->get( 'KEY' ).
    li_repo_online ?= zcl_abapgit_repo_srv=>get_instance( )->get( lv_key ).
    li_repo ?= li_repo_online.

    lv_remote_sha1 = ii_event->query( )->get( 'REMOTE_SHA1' ).

    ls_file-path     = ii_event->query( )->get( 'PATH' ).
    ls_file-filename = ii_event->query( )->get( 'FILENAME' ). " unescape ?

    zcl_abapgit_filename_logic=>file_to_object(
      EXPORTING
        iv_filename = ls_file-filename
        iv_path     = ls_file-path
        iv_devclass = li_repo->get_package( )
        io_dot      = li_repo->get_dot_abapgit( )
      IMPORTING
        es_item     = ls_item ).

    APPEND INITIAL LINE TO lt_filter ASSIGNING <ls_filter>.
    <ls_filter>-object = ls_item-obj_type.
    <ls_filter>-obj_name = ls_item-obj_name.
    CREATE OBJECT lo_filter EXPORTING it_filter = lt_filter.

    lt_files_item = li_repo_online->zif_abapgit_repo~get_files_local_filtered( lo_filter ).
    READ TABLE lt_files_item INTO ls_file_item WITH KEY file-path = ls_file-path
      file-filename = ls_file-filename.

    lv_blob = zcl_abapgit_git_factory=>get_v2_porcelain( )->fetch_blob(
      iv_url  = li_repo_online->get_url( )
      iv_sha1 = lv_remote_sha1 ).

    ls_remote-path = ls_file-path.
    ls_remote-filename = ls_file-filename.
    ls_remote-sha1 = lv_remote_sha1.
    ls_remote-data = lv_blob.

    ls_local-path = ls_remote-path.
    ls_local-filename = ls_remote-filename.
    ls_local-sha1 = ls_file_item-file-sha1.
    ls_local-data = ls_file_item-file-data.

    rs_handled-page = zcl_abapgit_gui_page_diff_file=>create(
      iv_obj_type = ls_item-obj_type
      iv_obj_name = ls_item-obj_name
      is_local    = ls_local
      is_remote   = ls_remote ).

    rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page_w_bookmark.

  ENDMETHOD.


  METHOD render_table.

    DATA ls_path_name LIKE LINE OF it_files.
    DATA lv_status    TYPE string.
    DATA lv_param     TYPE string.
    DATA li_repo      TYPE REF TO zif_abapgit_repo.
    DATA ls_item      TYPE zif_abapgit_definitions=>ty_item.
    DATA lv_duplicate TYPE abap_bool.


    ASSERT iv_repo_key IS NOT INITIAL.
    li_repo = zcl_abapgit_repo_srv=>get_instance( )->get( iv_repo_key ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( |<table>| ).
    ri_html->add( |<tr><td><u>Filename</u></td><td><u>Remote</u></td><td><u>Local</u></td><td></td><td></td></tr>| ).

    LOOP AT it_files INTO ls_path_name.
      CLEAR lv_status.

      zcl_abapgit_filename_logic=>file_to_object(
        EXPORTING
          iv_filename = ls_path_name-filename
          iv_path     = ls_path_name-path
          iv_devclass = li_repo->get_package( )
          io_dot      = li_repo->get_dot_abapgit( )
        IMPORTING
          es_item     = ls_item ).
      READ TABLE it_transport_duplicates
        TRANSPORTING NO FIELDS
        WITH KEY obj_type = ls_item-obj_type
                 obj_name = ls_item-obj_name.
      lv_duplicate = boolc( sy-subrc = 0 ).

      IF ls_path_name-remote_sha1 = ls_path_name-local_sha1.
        IF is_user_settings-hide_matching_files = abap_true AND lv_duplicate = abap_false.
          CONTINUE.
        ENDIF.
        lv_status = 'Match'.
      ELSEIF ls_path_name-remote_sha1 IS NOT INITIAL
          AND ls_path_name-local_sha1 IS NOT INITIAL.

        lv_param = zcl_abapgit_html_action_utils=>file_encode(
          iv_key   = iv_repo_key
          ig_file  = ls_path_name ).
        lv_status = ri_html->a(
          iv_txt = 'Diff'
          iv_act = |{ zif_abapgit_definitions=>c_action-go_file_diff }?{
            lv_param }&remote_sha1={ ls_path_name-remote_sha1 }| ).
      ENDIF.

      ri_html->add( |<tr><td><tt>{ ls_path_name-path }{ ls_path_name-filename }</tt></td><td>{
        ls_path_name-remote_sha1(7) }</td><td>{
        ls_path_name-local_sha1(7) }</td><td>{ lv_status }</td><td>| ).
      IF lv_duplicate = abap_true.
        ri_html->add_icon( iv_name = 'exclamation-triangle/red'
                           iv_hint = 'In duplicate transports' ).
      ENDIF.
      ri_html->add( |</td></tr>| ).
    ENDLOOP.
    ri_html->add( |</table>| ).

  ENDMETHOD.
ENDCLASS.
