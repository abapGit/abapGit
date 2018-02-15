class ZCL_ABAPGIT_STAGE_LOGIC definition
  public
  final
  create public .

  PUBLIC SECTION.
    CLASS-METHODS:
      get
        IMPORTING io_repo         TYPE REF TO zcl_abapgit_repo_online
        RETURNING VALUE(rs_files) TYPE zif_abapgit_definitions=>ty_stage_files
        RAISING   zcx_abapgit_exception,
      count
        IMPORTING io_repo         TYPE REF TO zcl_abapgit_repo_online
        RETURNING VALUE(rv_count) TYPE i
        RAISING   zcx_abapgit_exception.

  PRIVATE SECTION.
    CLASS-METHODS:
      remove_ignored
        IMPORTING io_repo  TYPE REF TO zcl_abapgit_repo_online
        CHANGING  cs_files TYPE zif_abapgit_definitions=>ty_stage_files,
      remove_identical
        CHANGING cs_files TYPE zif_abapgit_definitions=>ty_stage_files.

ENDCLASS.



CLASS ZCL_ABAPGIT_STAGE_LOGIC IMPLEMENTATION.


  METHOD count.

    DATA: ls_files TYPE zif_abapgit_definitions=>ty_stage_files.

    ls_files = get( io_repo ).

    rv_count = lines( ls_files-remote ) + lines( ls_files-local ).

  ENDMETHOD.


  METHOD get.

    rs_files-local  = io_repo->get_files_local( ).
    rs_files-remote = io_repo->get_files_remote( ).
    remove_identical( CHANGING cs_files = rs_files ).
    remove_ignored( EXPORTING io_repo  = io_repo
                    CHANGING  cs_files = rs_files ).

  ENDMETHOD.


  METHOD remove_identical.

    DATA: lv_index  TYPE i,
          ls_remote LIKE LINE OF cs_files-remote.

    FIELD-SYMBOLS: <ls_local> LIKE LINE OF cs_files-local.

    SORT cs_files-remote BY path filename.

    LOOP AT cs_files-local ASSIGNING <ls_local>.
      lv_index = sy-tabix.

      READ TABLE cs_files-remote INTO ls_remote
        WITH KEY path = <ls_local>-file-path filename = <ls_local>-file-filename
        BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE cs_files-remote INDEX sy-tabix.
        IF ls_remote-sha1 = <ls_local>-file-sha1.
          DELETE cs_files-local INDEX lv_index.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD remove_ignored.

    DATA: lv_index TYPE i.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF cs_files-remote.


    LOOP AT cs_files-remote ASSIGNING <ls_remote>.
      lv_index = sy-tabix.

      IF io_repo->get_dot_abapgit( )->is_ignored(
          iv_path     = <ls_remote>-path
          iv_filename = <ls_remote>-filename ) = abap_true.
        DELETE cs_files-remote INDEX lv_index.
      ELSEIF <ls_remote>-path     = zif_abapgit_definitions=>gc_root_dir
         AND <ls_remote>-filename = zif_abapgit_definitions=>gc_dot_abapgit.
        " Remove .abapgit from remotes - it cannot be removed or ignored
        DELETE cs_files-remote INDEX lv_index.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
