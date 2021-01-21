CLASS zcl_abapgit_gui_page_sett_info DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler .
    INTERFACES zif_abapgit_gui_renderable .

    CLASS-METHODS create
      IMPORTING
        !io_repo       TYPE REF TO zcl_abapgit_repo
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception .
    METHODS constructor
      IMPORTING
        !io_repo TYPE REF TO zcl_abapgit_repo
      RAISING
        zcx_abapgit_exception .

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_stats,
        measure TYPE string,
        local   TYPE i,
        remote  TYPE i,
      END OF ty_stats.

    CONSTANTS:
      BEGIN OF c_id,
        info            TYPE string VALUE 'info',
        created_by      TYPE string VALUE 'created_by',
        created_at      TYPE string VALUE 'created_at',
        deserialized_by TYPE string VALUE 'deserialized_by',
        deserialized_at TYPE string VALUE 'deserialized_at',
        stats           TYPE string VALUE 'stats',
        stats_table     TYPE string VALUE 'stats_table',
      END OF c_id.
    CONSTANTS:
      BEGIN OF c_event,
        go_back TYPE string VALUE 'go-back',
        save    TYPE string VALUE 'save',
      END OF c_event.

    DATA mo_form TYPE REF TO zcl_abapgit_html_form .
    DATA mo_form_data TYPE REF TO zcl_abapgit_string_map .

    DATA mo_repo TYPE REF TO zcl_abapgit_repo.
    DATA mt_stats TYPE STANDARD TABLE OF ty_stats WITH KEY measure.

    METHODS get_form_schema
      RETURNING
        VALUE(ro_form) TYPE REF TO zcl_abapgit_html_form
      RAISING
        zcx_abapgit_exception.
    METHODS read_settings
      RAISING
        zcx_abapgit_exception.
    METHODS read_stats
      RAISING
        zcx_abapgit_exception.
    METHODS format_user
      IMPORTING
        !iv_username   TYPE xubname
      RETURNING
        VALUE(rv_user) TYPE string.
    METHODS format_timestamp
      IMPORTING
        !iv_timestamp       TYPE timestampl
      RETURNING
        VALUE(rv_timestamp) TYPE string.
    METHODS format_size
      IMPORTING
        !iv_size       TYPE i
      RETURNING
        VALUE(rv_size) TYPE string.
ENDCLASS.



CLASS zcl_abapgit_gui_page_sett_info IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).
    CREATE OBJECT mo_form_data.
    mo_repo = io_repo.
    mo_form = get_form_schema( ).

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_sett_info.

    CREATE OBJECT lo_component
      EXPORTING
        io_repo = io_repo.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Repository Stats'
      io_page_menu       = zcl_abapgit_gui_chunk_lib=>settings_repo_toolbar(
                             iv_key = io_repo->get_key( )
                             iv_act = zif_abapgit_definitions=>c_action-repo_infos )
      ii_child_component = lo_component ).

  ENDMETHOD.


  METHOD format_size.

    DATA:
      lv_size TYPE p LENGTH 16 DECIMALS 2,
      lv_unit TYPE string.

    IF iv_size > 1024 * 1024 * 1024.
      lv_size = iv_size / 1024 / 1024 / 1024.
      lv_unit = 'GB'.
    ELSEIF iv_size > 1024 * 1024.
      lv_size = iv_size / 1024 / 1024.
      lv_unit = 'MB'.
    ELSEIF iv_size > 1024.
      lv_size = iv_size / 1024.
      lv_unit = 'KB'.
    ELSE.
      lv_size = iv_size.
      lv_unit = 'Bytes'.
    ENDIF.

    rv_size = |{ lv_size } { lv_unit }|.

  ENDMETHOD.


  METHOD format_timestamp.

    IF iv_timestamp IS INITIAL.
      rv_timestamp = 'n/a'.
      RETURN.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_TIMES_OUTPUT'
      EXPORTING
        input  = iv_timestamp
      IMPORTING
        output = rv_timestamp.

  ENDMETHOD.


  METHOD format_user.

    DATA:
      ls_user_address TYPE addr3_val,
      lv_title        TYPE string.

    IF iv_username IS INITIAL.
      rv_user = 'n/a'.
      RETURN.
    ENDIF.

    IF iv_username <> zcl_abapgit_objects_super=>c_user_unknown.
      CALL FUNCTION 'SUSR_USER_ADDRESS_READ'
        EXPORTING
          user_name              = iv_username
        IMPORTING
          user_address           = ls_user_address
        EXCEPTIONS
          user_address_not_found = 1
          OTHERS                 = 2.
      IF sy-subrc = 0.
        lv_title = ls_user_address-name_text.
      ENDIF.
    ENDIF.

    rv_user = iv_username.
    IF lv_title IS NOT INITIAL.
      rv_user = |{ rv_user } ({ lv_title })|.
    ENDIF.

  ENDMETHOD.


  METHOD get_form_schema.

    ro_form = zcl_abapgit_html_form=>create(
                iv_form_id   = 'repo-infos-form'
                iv_help_page = 'https://docs.abapgit.org/guide-repo-infos.html' ).

    ro_form->start_group(
      iv_name        = c_id-info
      iv_label       = 'Stats'
    )->text(
      iv_name        = c_id-created_by
      iv_label       = 'Created By'
      iv_readonly    = abap_true
    )->text(
      iv_name        = c_id-created_at
      iv_label       = 'Created At'
      iv_readonly    = abap_true
    )->text(
      iv_name        = c_id-deserialized_by
      iv_label       = 'Last Deserialized By'
      iv_readonly    = abap_true
    )->text(
      iv_name        = c_id-deserialized_at
      iv_label       = 'Last Deserialized At'
      iv_readonly    = abap_true
    )->table(
      iv_name        = c_id-stats_table
      iv_label       = 'Statistics'
    )->column(
      iv_label       = 'Measure'
      iv_width       = '50%'
      iv_readonly    = abap_true
    )->column(
      iv_label       = 'Local'
      iv_width       = '25%'
      iv_readonly    = abap_true
    )->column(
      iv_label       = 'Remote'
      iv_width       = '25%'
      iv_readonly    = abap_true
   )->command(
      iv_label       = 'Back'
      iv_action      = c_event-go_back ).

  ENDMETHOD.


  METHOD read_settings.

    DATA:
      ls_repo  TYPE zif_abapgit_persistence=>ty_repo,
      ls_stats TYPE ty_stats,
      lv_row   TYPE i,
      lv_int   TYPE i,
      lv_val   TYPE string.

    " Get infos from DB
    TRY.
        ls_repo = zcl_abapgit_persist_factory=>get_repo( )->read( mo_repo->get_key( ) ).
      CATCH zcx_abapgit_not_found.
        zcx_abapgit_exception=>raise( |Repo not found, key { mo_repo->get_key( ) }| ).
    ENDTRY.

    read_stats( ).

    " Infos
    mo_form_data->set(
      iv_key = c_id-created_by
      iv_val = format_user( ls_repo-created_by ) ).
    mo_form_data->set(
      iv_key = c_id-created_at
      iv_val = format_timestamp( ls_repo-created_at ) ).
    mo_form_data->set(
      iv_key = c_id-deserialized_by
      iv_val = format_user( ls_repo-deserialized_by ) ).
    mo_form_data->set(
      iv_key = c_id-deserialized_at
      iv_val = format_timestamp( ls_repo-deserialized_at ) ).

    LOOP AT mt_stats INTO ls_stats.
      lv_row = sy-tabix.
      DO 3 TIMES.
        CASE sy-index.
          WHEN 1.
            lv_val = ls_stats-measure.
          WHEN 2.
            lv_val = ls_stats-local.
          WHEN 3.
            lv_val = ls_stats-remote.
        ENDCASE.

        IF ls_stats-measure CS 'Size' AND sy-index BETWEEN 2 AND 3.
          lv_int = lv_val.
          lv_val = format_size( lv_int ).
        ENDIF.

        mo_form_data->set(
          iv_key = |{ c_id-stats_table }-{ lv_row }-{ sy-index }|
          iv_val = lv_val ).
      ENDDO.
    ENDLOOP.

    mo_form_data->set(
      iv_key = |{ c_id-stats_table }-{ zif_abapgit_html_form=>c_rows }|
      iv_val = |{ lv_row }| ).

  ENDMETHOD.


  METHOD read_stats.

    DATA:
      lt_local        TYPE zif_abapgit_definitions=>ty_files_item_tt,
      lt_remote       TYPE zif_abapgit_definitions=>ty_files_tt,
      ls_item         TYPE zif_abapgit_definitions=>ty_item,
      lt_local_items  TYPE STANDARD TABLE OF zif_abapgit_definitions=>ty_item WITH DEFAULT KEY,
      lt_remote_items TYPE STANDARD TABLE OF zif_abapgit_definitions=>ty_item WITH DEFAULT KEY,
      lt_results      TYPE zif_abapgit_definitions=>ty_results_tt,
      lv_ignored      TYPE abap_bool,
      lv_state        TYPE c LENGTH 1,
      ls_stats        TYPE ty_stats.

    FIELD-SYMBOLS:
      <ls_local>  LIKE LINE OF lt_local,
      <ls_remote> LIKE LINE OF lt_remote,
      <ls_result> LIKE LINE OF lt_results.

    CLEAR mt_stats.

    lt_local = mo_repo->get_files_local( ).

    ls_stats-measure = 'Number of Files'.
    ls_stats-local   = lines( lt_local ).

    IF mo_repo->has_remote_source( ) = abap_true.
      lt_remote = mo_repo->get_files_remote( ).
      ls_stats-remote = lines( lt_remote ).
    ENDIF.

    APPEND ls_stats TO mt_stats.

    IF lt_remote IS NOT INITIAL.
      ls_stats-measure = 'Number of Ignored Files'.
      ls_stats-local  = ls_stats-remote - ls_stats-local. " should be >= 0
      ls_stats-remote = 0.
      APPEND ls_stats TO mt_stats.
    ENDIF.

    lt_results = zcl_abapgit_file_status=>status( mo_repo ).

    DO 3 TIMES.
      CLEAR ls_stats.

      CASE sy-index.
        WHEN 1.
          ls_stats-measure = 'Number of Modified Files'.
          lv_state = zif_abapgit_definitions=>c_state-modified.
        WHEN 2.
          ls_stats-measure = 'Number of Added Files'.
          lv_state = zif_abapgit_definitions=>c_state-added.
        WHEN 3.
          ls_stats-measure = 'Number of Deleted Files'.
          lv_state = zif_abapgit_definitions=>c_state-deleted.
      ENDCASE.

      LOOP AT lt_results ASSIGNING <ls_result>.
        IF <ls_result>-lstate = lv_state.
          ls_stats-local = ls_stats-local + 1.
        ENDIF.
        IF <ls_result>-rstate = lv_state.
          ls_stats-remote = ls_stats-remote + 1.
        ENDIF.
      ENDLOOP.

      APPEND ls_stats TO mt_stats.
    ENDDO.

    CLEAR ls_stats.
    ls_stats-measure = 'Size of Files'.

    LOOP AT lt_local ASSIGNING <ls_local>.
      ls_stats-local = ls_stats-local + xstrlen( <ls_local>-file-data ).

      COLLECT <ls_local>-item INTO lt_local_items.
    ENDLOOP.

    IF mo_repo->has_remote_source( ) = abap_true.
      LOOP AT lt_remote ASSIGNING <ls_remote>.
        ls_stats-remote = ls_stats-remote + xstrlen( <ls_remote>-data ).

        lv_ignored = mo_repo->get_dot_abapgit( )->is_ignored(
                       iv_filename = <ls_remote>-filename
                       iv_path     = <ls_remote>-path ).

        IF <ls_remote>-filename IS NOT INITIAL AND lv_ignored = abap_false.
          TRY.
              zcl_abapgit_file_status=>identify_object(
                EXPORTING
                  iv_filename = <ls_remote>-filename
                  iv_path     = <ls_remote>-path
                  iv_devclass = mo_repo->get_package( )
                  io_dot      = mo_repo->get_dot_abapgit( )
                IMPORTING
                  es_item     = ls_item ).

              COLLECT ls_item INTO lt_remote_items.
            CATCH zcx_abapgit_exception ##NO_HANDLER.
          ENDTRY.
        ENDIF.

      ENDLOOP.
    ENDIF.

    APPEND ls_stats TO mt_stats.

    CLEAR ls_stats.
    ls_stats-measure = 'Number of Objects'.

    DELETE lt_local_items WHERE obj_type IS INITIAL OR obj_name IS INITIAL.
    ls_stats-local = lines( lt_local_items ).

    DELETE lt_remote_items WHERE obj_type IS INITIAL OR obj_name IS INITIAL.
    ls_stats-remote = lines( lt_remote_items ).

    APPEND ls_stats TO mt_stats.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    IF ii_event->mv_action = c_event-go_back.
      rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back_to_bookmark.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    gui_services( )->register_event_handler( me ).

    read_settings( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( `<div class="repo">` ).
    ri_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top(
                    io_repo               = mo_repo
                    iv_show_commit        = abap_false
                    iv_interactive_branch = abap_true ) ).
    ri_html->add( `</div>` ).

    ri_html->add( mo_form->render( mo_form_data ) ).

  ENDMETHOD.
ENDCLASS.
