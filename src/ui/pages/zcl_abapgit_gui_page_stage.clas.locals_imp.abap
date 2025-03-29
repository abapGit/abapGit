*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

INTERFACE lif_selected.

  METHODS stage_selected
    IMPORTING
      ii_event        TYPE REF TO zif_abapgit_gui_event
      it_status       TYPE zif_abapgit_definitions=>ty_results_ts_path
      it_local        TYPE zif_abapgit_definitions=>ty_files_item_tt
    RETURNING
      VALUE(ro_stage) TYPE REF TO zcl_abapgit_stage
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.

CLASS lcl_selected DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_instance) TYPE REF TO lif_selected.

    INTERFACES lif_selected.

  PRIVATE SECTION.
    METHODS check_selected
      IMPORTING
        io_files TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception.

    METHODS read_status_case_insensitive
      IMPORTING
        is_file          TYPE zif_abapgit_git_definitions=>ty_file
        it_status        TYPE zif_abapgit_definitions=>ty_results_ts_path
      RETURNING
        VALUE(rs_status) TYPE zif_abapgit_definitions=>ty_result
      RAISING
        zcx_abapgit_exception.

    CLASS-DATA:
      gi_instance TYPE REF TO lif_selected.

ENDCLASS.


CLASS lcl_selected IMPLEMENTATION.

  METHOD lif_selected~stage_selected.

    DATA ls_file  TYPE zif_abapgit_git_definitions=>ty_file.
    DATA ls_status LIKE LINE OF it_status.
    DATA lo_files TYPE REF TO zcl_abapgit_string_map.

    FIELD-SYMBOLS:
      <ls_file> LIKE LINE OF it_local,
      <ls_item> LIKE LINE OF lo_files->mt_entries.

    lo_files = ii_event->form_data( ).

    IF lo_files->size( ) = 0.
      zcx_abapgit_exception=>raise( 'process_stage_list: empty list' ).
    ENDIF.

    check_selected( lo_files ).

    CREATE OBJECT ro_stage.

    LOOP AT lo_files->mt_entries ASSIGNING <ls_item>
      "Ignore Files that we don't want to stage, so any errors don't stop the staging process
      WHERE v <> zif_abapgit_definitions=>c_method-skip.

      zcl_abapgit_path=>split_file_location(
        EXPORTING
          iv_fullpath = <ls_item>-k
        IMPORTING
          ev_path     = ls_file-path
          ev_filename = ls_file-filename ).

      " you should remember that ls_file is sent from the client and is always uppercase,
      " whereas it_status and it_local path could be lower, upper or mixed case.
      ls_status = read_status_case_insensitive(
                      is_file   = ls_file
                      it_status = it_status ).

      " ls_status has the right case, therefore use it also for ls_file
      ls_file-path = ls_status-path.
      ls_file-filename = ls_status-filename.

      CASE <ls_item>-v.
        WHEN zif_abapgit_definitions=>c_method-add.
          READ TABLE it_local ASSIGNING <ls_file>
            WITH KEY file-path     = ls_file-path
                     file-filename = ls_file-filename.

          IF sy-subrc <> 0.
            zcx_abapgit_exception=>raise( |process_stage_list: unknown file { ls_file-path }{ ls_file-filename }| ).
          ENDIF.

          ro_stage->add( iv_path     = <ls_file>-file-path
                         iv_filename = <ls_file>-file-filename
                         is_status   = ls_status
                         iv_data     = <ls_file>-file-data ).
        WHEN zif_abapgit_definitions=>c_method-ignore.
          ro_stage->ignore( iv_path     = ls_file-path
                            iv_filename = ls_file-filename ).
        WHEN zif_abapgit_definitions=>c_method-rm.
          ro_stage->rm( iv_path     = ls_file-path
                        is_status   = ls_status
                        iv_filename = ls_file-filename ).
        WHEN zif_abapgit_definitions=>c_method-skip.
          " Do nothing. Never happens as it is filtered out before. Just for completeness.
        WHEN OTHERS.
          zcx_abapgit_exception=>raise( |process_stage_list: unknown method { <ls_item>-v }| ).
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD check_selected.

    DATA:
      ls_file    TYPE zif_abapgit_git_definitions=>ty_file,
      lv_pattern TYPE string,
      lv_msg     TYPE string.

    FIELD-SYMBOLS:
      <ls_item>     LIKE LINE OF io_files->mt_entries,
      <ls_item_chk> LIKE LINE OF io_files->mt_entries.

    " Check all added files if the exist in different paths (packages) without being removed
    LOOP AT io_files->mt_entries ASSIGNING <ls_item> WHERE v = zif_abapgit_definitions=>c_method-add.

      " Allow mixed case path, but check filename to lower case
      zcl_abapgit_path=>split_file_location(
        EXPORTING
          iv_fullpath = <ls_item>-k
        IMPORTING
          ev_path     = ls_file-path
          ev_filename = ls_file-filename ).

      ls_file-filename = to_lower( ls_file-filename ).

      " Skip packages since they all have identical filenames
      IF ls_file-filename <> 'package.devc.xml'.
        lv_pattern = '*/' && to_upper( ls_file-filename ).
        REPLACE ALL OCCURRENCES OF '#' IN lv_pattern WITH '##'. " for CP

        LOOP AT io_files->mt_entries ASSIGNING <ls_item_chk>
          WHERE k CP lv_pattern AND k <> <ls_item>-k AND v <> zif_abapgit_definitions=>c_method-rm.

          lv_msg = |In order to add { to_lower( <ls_item>-k ) }, | &&
                   |you have to remove { to_lower( <ls_item_chk>-k ) }|.
          zcx_abapgit_exception=>raise( lv_msg ).

        ENDLOOP.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_instance.

    IF gi_instance IS INITIAL.
      CREATE OBJECT gi_instance TYPE lcl_selected.
    ENDIF.

    ro_instance = gi_instance.

  ENDMETHOD.


  METHOD read_status_case_insensitive.

    FIELD-SYMBOLS: <ls_status> TYPE zif_abapgit_definitions=>ty_result.

    LOOP AT it_status ASSIGNING <ls_status>.

      IF to_upper( <ls_status>-filename ) = to_upper( is_file-filename )
      AND to_upper( <ls_status>-path )    = to_upper( is_file-path ).
        rs_status = <ls_status>.
        RETURN.
      ENDIF.

    ENDLOOP.

    " see https://github.com/abapGit/abapGit/issues/3073
    zcx_abapgit_exception=>raise(
      |Unable to stage { is_file-filename }. If the filename contains spaces, this is a known issue.| &&
      | Consider ignoring or staging the file at a later time.| ).

  ENDMETHOD.

ENDCLASS.
