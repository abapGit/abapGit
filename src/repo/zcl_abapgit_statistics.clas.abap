CLASS zcl_abapgit_statistics DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      ty_size TYPE p LENGTH 16 DECIMALS 0,
      BEGIN OF ty_repo,
        key             TYPE zif_abapgit_persistence=>ty_repo-key,
        name            TYPE string,
        url             TYPE string,
        package         TYPE devclass,
        offline         TYPE abap_bool,
        favorite        TYPE abap_bool,
        write_protected TYPE abap_bool,
        host            TYPE string,
        objects         TYPE i,
        files           TYPE i,
        size            TYPE ty_size,
        error           TYPE string,
      END OF ty_repo,
      ty_repos TYPE STANDARD TABLE OF ty_repo WITH DEFAULT KEY,
      BEGIN OF ty_snapshot,
        refreshed_at TYPE timestamp,
        repos        TYPE ty_repos,
      END OF ty_snapshot.

    CLASS-METHODS read
      RETURNING VALUE(rs_snapshot) TYPE ty_snapshot
      RAISING zcx_abapgit_exception.
    CLASS-METHODS refresh
      RETURNING VALUE(rs_snapshot) TYPE ty_snapshot
      RAISING zcx_abapgit_exception.

  PRIVATE SECTION.
    CLASS-METHODS collect
      IMPORTING ii_repo        TYPE REF TO zif_abapgit_repo
                iv_favorite    TYPE abap_bool
      RETURNING VALUE(rs_repo) TYPE ty_repo
      RAISING zcx_abapgit_exception.
    CLASS-METHODS host
      IMPORTING iv_url         TYPE string
      RETURNING VALUE(rv_host) TYPE string.
    CLASS-METHODS measure
      IMPORTING it_files TYPE zif_abapgit_definitions=>ty_files_item_tt
      CHANGING cs_repo   TYPE ty_repo.
ENDCLASS.



CLASS zcl_abapgit_statistics IMPLEMENTATION.


  METHOD collect.

    DATA lt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA lt_files TYPE zif_abapgit_definitions=>ty_files_item_tt.
    DATA li_log TYPE REF TO zif_abapgit_log.
    DATA lt_messages TYPE zif_abapgit_log=>ty_log_outs.
    DATA ls_message LIKE LINE OF lt_messages.
    DATA lx_error TYPE REF TO zcx_abapgit_exception.

    rs_repo-key = ii_repo->get_key( ).
    rs_repo-name = ii_repo->get_name( ).
    rs_repo-package = ii_repo->get_package( ).
    rs_repo-offline = ii_repo->is_offline( ).
    rs_repo-favorite = iv_favorite.
    rs_repo-write_protected = ii_repo->get_local_settings( )-write_protected.
    IF rs_repo-offline = abap_false.
      rs_repo-url = ii_repo->ms_data-url.
      rs_repo-host = host( ii_repo->ms_data-url ).
    ENDIF.

    " A failing TADIR read must not prevent attempting serialization.
    TRY.
        lt_tadir = ii_repo->get_tadir_objects( ).
        rs_repo-objects = lines( lt_tadir ).
      CATCH zcx_abapgit_exception INTO lx_error.
        rs_repo-error = lx_error->get_text( ).
    ENDTRY.

    TRY.
        CREATE OBJECT li_log TYPE zcl_abapgit_log.
        " Force serialization while preserving the remote cache.
        lt_files = ii_repo->get_files_local( ii_log = li_log
                                            iv_force = abap_true ).
        measure( EXPORTING it_files = lt_files CHANGING cs_repo = rs_repo ).
        lt_messages = li_log->get_messages( ).
        LOOP AT lt_messages INTO ls_message WHERE type = 'E' OR type = 'A'.
          rs_repo-error = |{ rs_repo-error } { ls_message-text }|.
        ENDLOOP.
      CATCH zcx_abapgit_exception INTO lx_error.
        rs_repo-error = |{ rs_repo-error } { lx_error->get_text( ) }|.
    ENDTRY.
    CONDENSE rs_repo-error.

  ENDMETHOD.


  METHOD host.

    DATA lv_host TYPE string.

    rv_host = 'Others'.
    TRY.
        lv_host = to_lower( zcl_abapgit_url=>host( iv_url ) ).
        IF lv_host = 'https://github.com' OR lv_host = 'http://github.com'.
          rv_host = 'GitHub'.
        ELSEIF lv_host = 'https://gitlab.com' OR lv_host = 'http://gitlab.com'.
          rv_host = 'GitLab'.
        ENDIF.
      CATCH zcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD measure.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF it_files.

    cs_repo-files = lines( it_files ).
    CLEAR cs_repo-size.
    LOOP AT it_files ASSIGNING <ls_file>.
      cs_repo-size = cs_repo-size + xstrlen( <ls_file>-file-data ).
    ENDLOOP.

  ENDMETHOD.


  METHOD read.

    DATA lv_xml TYPE string.
    DATA li_input TYPE REF TO zif_abapgit_xml_input.
    DATA lt_repos TYPE zif_abapgit_persistence=>ty_repos.
    DATA ls_repo LIKE LINE OF lt_repos.
    FIELD-SYMBOLS <ls_saved> LIKE LINE OF rs_snapshot-repos.

    TRY.
        lv_xml = zcl_abapgit_persistence_db=>get_instance( )->read(
          iv_type = zcl_abapgit_persistence_db=>c_type_statistics
          iv_value = sy-uname ).
      CATCH zcx_abapgit_not_found.
        RETURN.
    ENDTRY.
    CREATE OBJECT li_input TYPE zcl_abapgit_xml_input EXPORTING iv_xml = lv_xml.
    li_input->read( EXPORTING iv_name = 'STATISTICS' CHANGING cg_data = rs_snapshot ).

    " Complete navigation metadata in older snapshots without serializing.
    lt_repos = zcl_abapgit_persist_factory=>get_repo( )->list( ).
    LOOP AT rs_snapshot-repos ASSIGNING <ls_saved>
        WHERE key IS INITIAL OR ( offline = abap_false AND url IS INITIAL ).
      READ TABLE lt_repos INTO ls_repo WITH KEY package = <ls_saved>-package offline = <ls_saved>-offline.
      IF sy-subrc = 0.
        IF <ls_saved>-key IS INITIAL.
          <ls_saved>-key = ls_repo-key.
        ENDIF.
        IF <ls_saved>-offline = abap_false AND <ls_saved>-url IS INITIAL.
          <ls_saved>-url = ls_repo-url.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD refresh.

    DATA lt_repos TYPE zif_abapgit_repo_srv=>ty_repo_list.
    DATA li_repo LIKE LINE OF lt_repos.
    DATA ls_repo TYPE ty_repo.
    DATA li_output TYPE REF TO zif_abapgit_xml_output.
    DATA li_progress TYPE REF TO zif_abapgit_progress.
    DATA lv_current TYPE i.
    DATA lt_favorites TYPE zif_abapgit_persist_user=>ty_favorites.
    DATA lv_favorite TYPE abap_bool.

    lt_repos = zcl_abapgit_repo_srv=>get_instance( )->list( ).
    lt_favorites = zcl_abapgit_persist_factory=>get_user( )->get_favorites( ).
    li_progress = zcl_abapgit_progress=>get_instance( lines( lt_repos ) ).
    LOOP AT lt_repos INTO li_repo.
      lv_current = lv_current + 1.
      li_progress->show( iv_current = lv_current
                         iv_text = |Statistics: { li_repo->get_package( ) }| ).
      READ TABLE lt_favorites WITH KEY table_line = li_repo->get_key( ) TRANSPORTING NO FIELDS.
      lv_favorite = boolc( sy-subrc = 0 ).
      ls_repo = collect( ii_repo = li_repo
                         iv_favorite = lv_favorite ).
      APPEND ls_repo TO rs_snapshot-repos.
    ENDLOOP.
    li_progress->off( ).
    SORT rs_snapshot-repos BY name package.
    GET TIME STAMP FIELD rs_snapshot-refreshed_at.

    CREATE OBJECT li_output TYPE zcl_abapgit_xml_output.
    li_output->add( iv_name = 'STATISTICS'
                    ig_data = rs_snapshot ).
    zcl_abapgit_persistence_db=>get_instance( )->modify(
      iv_type = zcl_abapgit_persistence_db=>c_type_statistics
      iv_value = sy-uname
      iv_data = li_output->render( ) ).
    COMMIT WORK AND WAIT.

  ENDMETHOD.
ENDCLASS.
