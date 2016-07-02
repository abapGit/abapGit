*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_FILE_STATUS
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_file_status DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_file_status DEFINITION FINAL.

  PUBLIC SECTION.

    CLASS-METHODS status
      IMPORTING io_repo           TYPE REF TO lcl_repo
                io_log            TYPE REF TO lcl_log OPTIONAL
      RETURNING VALUE(rt_results) TYPE ty_results_tt
      RAISING   lcx_exception.

  PRIVATE SECTION.

    CLASS-METHODS compare_files
      IMPORTING it_repo         TYPE ty_files_tt
                is_gen          TYPE ty_file
      RETURNING VALUE(rv_match) TYPE sap_bool
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_file_status DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_file_status IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_file_status IMPLEMENTATION.

  METHOD compare_files.

    READ TABLE it_repo WITH KEY
      path = is_gen-path
      filename = is_gen-filename
      data = is_gen-data
      TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      rv_match = abap_false.
    ELSE.
      rv_match = abap_true.
    ENDIF.

  ENDMETHOD.                    "compare_files

  METHOD status.

    DATA: lv_pre    TYPE tadir-obj_name,
          lt_files  TYPE ty_files_tt,
          ls_result LIKE LINE OF rt_results,
          lv_type   TYPE string,
          ls_item   TYPE ty_item,
          lt_tadir  TYPE ty_tadir_tt,
          lt_local  TYPE ty_files_item_tt,
          ls_tadir  TYPE tadir,
          lt_remote TYPE ty_files_tt,
          lv_ext    TYPE string.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF lt_remote,
                   <ls_tadir>  LIKE LINE OF lt_tadir,
                   <ls_result> LIKE LINE OF rt_results,
                   <ls_local>  LIKE LINE OF lt_local,
                   <ls_gen>    LIKE LINE OF lt_files.


    lt_remote = io_repo->get_files_remote( ).
    lt_local = io_repo->get_files_local( io_log ).

    LOOP AT lt_remote ASSIGNING <ls_remote>.
      lcl_progress=>show( iv_key     = 'Status'
                          iv_current = sy-tabix
                          iv_total   = lines( lt_remote )
                          iv_text    = <ls_remote>-filename ) ##NO_TEXT.

      SPLIT <ls_remote>-filename AT '.' INTO lv_pre lv_type lv_ext.
      TRANSLATE lv_pre TO UPPER CASE.
      TRANSLATE lv_type TO UPPER CASE.

      IF lv_ext <> 'xml' OR strlen( lv_type ) <> 4.
        CONTINUE. " current loop
      ENDIF.

* handle namespaces
      REPLACE ALL OCCURRENCES OF '#' IN lv_pre WITH '/'.

      CLEAR ls_result.
      ls_result-obj_type = lv_type.
      ls_result-obj_name = lv_pre.

      CLEAR ls_item.
      ls_item-obj_type = lv_type.
      ls_item-obj_name = lv_pre.

      CLEAR lt_files.
      LOOP AT lt_local ASSIGNING <ls_local> WHERE item = ls_item.
        APPEND <ls_local>-file TO lt_files.
      ENDLOOP.

      IF lt_files[] IS INITIAL.
* item does not exist locally
        ls_result-filename = <ls_remote>-filename.
        APPEND ls_result TO rt_results.
        CONTINUE. " current loop
      ENDIF.

      LOOP AT lt_files ASSIGNING <ls_gen>.
        ls_result-filename = <ls_gen>-filename.
        ls_result-match = compare_files( it_repo = lt_remote
                                         is_gen  = <ls_gen> ).
        APPEND ls_result TO rt_results.
      ENDLOOP.
    ENDLOOP.

* find files only existing remotely, including non abapGit related
    LOOP AT lt_remote ASSIGNING <ls_remote>.
      READ TABLE rt_results WITH KEY filename = <ls_remote>-filename
        TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        IF io_repo->get_dot_abapgit( )->is_ignored(
            iv_path = <ls_remote>-path
            iv_filename = <ls_remote>-filename ) = abap_true.
          CONTINUE.
        ENDIF.

        CLEAR ls_result.
        ls_result-match    = abap_true.
        ls_result-filename = <ls_remote>-filename.
        APPEND ls_result TO rt_results.
      ENDIF.
    ENDLOOP.

* find objects only existing locally
    lt_tadir = lcl_tadir=>read( io_repo->get_package( ) ).
    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      READ TABLE rt_results
        WITH KEY obj_type = <ls_tadir>-object
        obj_name = <ls_tadir>-obj_name
        TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        ls_item-obj_type = <ls_tadir>-object.
        ls_item-obj_name = <ls_tadir>-obj_name.
        IF lcl_objects=>is_supported( ls_item ) = abap_false.
          CONTINUE.
        ENDIF.

        CLEAR ls_result.
        ls_result-match    = abap_true.
        ls_result-obj_type = <ls_tadir>-object.
        ls_result-obj_name = <ls_tadir>-obj_name.
        APPEND ls_result TO rt_results.
      ENDIF.
    ENDLOOP.

* add path information for files
    LOOP AT lt_remote ASSIGNING <ls_remote>.
      READ TABLE rt_results ASSIGNING <ls_result> WITH KEY filename = <ls_remote>-filename.
      IF sy-subrc = 0.
        <ls_result>-path = <ls_remote>-path.
      ENDIF.
    ENDLOOP.

* add package information
    LOOP AT rt_results ASSIGNING <ls_result> WHERE NOT obj_type IS INITIAL.
      ls_tadir = lcl_tadir=>read_single( iv_object   = <ls_result>-obj_type
                                         iv_obj_name = <ls_result>-obj_name ).
      <ls_result>-package = ls_tadir-devclass.
    ENDLOOP.

    SORT rt_results BY
      obj_type ASCENDING
      obj_name ASCENDING
      filename ASCENDING.
    DELETE ADJACENT DUPLICATES FROM rt_results
      COMPARING obj_type obj_name filename.

    lcl_sap_package=>check(
      io_log     = io_log
      it_results = rt_results
      iv_start   = io_repo->get_dot_abapgit( )->get_starting_folder( )
      iv_top     = io_repo->get_package( ) ).

  ENDMETHOD.                    "status

ENDCLASS.                    "lcl_file_status IMPLEMENTATION