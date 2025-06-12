CLASS zcl_abapgit_flow_page_utils DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS render_table
      IMPORTING
        it_files         TYPE zif_abapgit_flow_logic=>ty_path_name_tt
        is_user_settings TYPE zif_abapgit_persist_user=>ty_flow_settings OPTIONAL
        iv_repo_key      TYPE zif_abapgit_persistence=>ty_repo-key
      RETURNING
        VALUE(ri_html)   TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.
ENDCLASS.

CLASS zcl_abapgit_flow_page_utils IMPLEMENTATION.

  METHOD render_table.

    DATA ls_path_name LIKE LINE OF it_files.
    DATA lv_status    TYPE string.
    DATA lv_param     TYPE string.

    ASSERT iv_repo_key IS NOT INITIAL.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( |<table>| ).
    ri_html->add( |<tr><td><u>Filename</u></td><td><u>Remote</u></td><td><u>Local</u></td><td></td></tr>| ).

    LOOP AT it_files INTO ls_path_name.
      IF ls_path_name-remote_sha1 = ls_path_name-local_sha1.
        IF is_user_settings-hide_matching_files = abap_true.
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
        ls_path_name-local_sha1(7) }</td><td>{ lv_status }</td></tr>| ).
    ENDLOOP.
    ri_html->add( |</table>| ).

  ENDMETHOD.

ENDCLASS.
