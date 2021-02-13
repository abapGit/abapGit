CLASS zcl_abapgit_gui_page_commit DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF c_action,
        commit_post   TYPE string VALUE 'commit_post',
        commit_cancel TYPE string VALUE 'commit_cancel',
      END OF c_action .

    METHODS constructor
      IMPORTING
        io_repo  TYPE REF TO zcl_abapgit_repo_online
        io_stage TYPE REF TO zcl_abapgit_stage
      RAISING
        zcx_abapgit_exception.

    METHODS zif_abapgit_gui_event_handler~on_event
        REDEFINITION .
  PROTECTED SECTION.

    CLASS-METHODS parse_commit_request
      IMPORTING
        !ii_event TYPE REF TO zif_abapgit_gui_event
      RETURNING
        VALUE(rs_commit) TYPE zif_abapgit_services_git=>ty_commit_fields
      RAISING
        zcx_abapgit_exception .

    METHODS render_content REDEFINITION .

private section.

  data MO_REPO type ref to ZCL_ABAPGIT_REPO_ONLINE .
  data MO_STAGE type ref to ZCL_ABAPGIT_STAGE .
  data MS_COMMIT type ZIF_ABAPGIT_SERVICES_GIT=>TY_COMMIT_FIELDS .

  methods PROPOSE_DEFAULT_BODY
    changing
      !C_BODY type STRING .
  methods PROPOSE_DEFAULT_COMMENT
    changing
      !C_COMMENT type STRING .
  methods SUPPLEMENT_TRANSPORT_LOCKS
    changing
      !CS_COMMIT type ZIF_ABAPGIT_SERVICES_GIT=>TY_COMMIT_FIELDS .
    METHODS render_menu
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    METHODS render_stage
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS render_form
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS render_text_input
      IMPORTING
        !iv_name       TYPE string
        !iv_label      TYPE string
        !iv_value      TYPE string OPTIONAL
        !iv_max_length TYPE string OPTIONAL
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    METHODS get_comment_default
      RETURNING
        VALUE(rv_text) TYPE string .
    METHODS get_comment_object
      IMPORTING
        !it_stage      TYPE zif_abapgit_definitions=>ty_stage_tt
      RETURNING
        VALUE(rv_text) TYPE string .
    METHODS get_comment_file
      IMPORTING
        !it_stage      TYPE zif_abapgit_definitions=>ty_stage_tt
      RETURNING
        VALUE(rv_text) TYPE string .
    METHODS render_scripts
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_COMMIT IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    mo_repo   = io_repo.
    mo_stage  = io_stage.

    ms_control-page_title = 'Commit'.
  ENDMETHOD.


  METHOD get_comment_default.

    DATA: lo_settings TYPE REF TO zcl_abapgit_settings,
          lt_stage    TYPE zif_abapgit_definitions=>ty_stage_tt.

    " Get setting for default comment text
    lo_settings = zcl_abapgit_persist_settings=>get_instance( )->read( ).

    rv_text = lo_settings->get_commitmsg_comment_default( ).

    IF rv_text IS INITIAL.
      RETURN.
    ENDIF.

    " Determine texts for scope of commit
    lt_stage = mo_stage->get_all( ).

    REPLACE '$FILE'   IN rv_text WITH get_comment_file( lt_stage ).

    REPLACE '$OBJECT' IN rv_text WITH get_comment_object( lt_stage ).

  ENDMETHOD.


  METHOD get_comment_file.

    DATA: lv_count TYPE i,
          lv_value TYPE c LENGTH 10.

    FIELD-SYMBOLS: <ls_stage> LIKE LINE OF it_stage.

    lv_count = lines( it_stage ).

    IF lv_count = 1.
      " Just one file so we use the file name
      READ TABLE it_stage ASSIGNING <ls_stage> INDEX 1.
      ASSERT sy-subrc = 0.

      rv_text = <ls_stage>-file-filename.
    ELSE.
      " For multiple file we use the count instead
      WRITE lv_count TO lv_value LEFT-JUSTIFIED.
      CONCATENATE lv_value 'files' INTO rv_text SEPARATED BY space.
    ENDIF.

  ENDMETHOD.


  METHOD get_comment_object.

    DATA: lv_count TYPE i,
          lv_value TYPE c LENGTH 10,
          ls_item  TYPE zif_abapgit_definitions=>ty_item,
          lt_items TYPE zif_abapgit_definitions=>ty_items_tt.

    FIELD-SYMBOLS: <ls_stage> LIKE LINE OF it_stage.

    " Get objects
    LOOP AT it_stage ASSIGNING <ls_stage>.
      CLEAR ls_item.
      ls_item-obj_type = <ls_stage>-status-obj_type.
      ls_item-obj_name = <ls_stage>-status-obj_name.
      COLLECT ls_item INTO lt_items.
    ENDLOOP.

    lv_count = lines( lt_items ).

    IF lv_count = 1.
      " Just one object so we use the object name
      READ TABLE lt_items INTO ls_item INDEX 1.
      ASSERT sy-subrc = 0.

      CONCATENATE ls_item-obj_type ls_item-obj_name INTO rv_text SEPARATED BY space.
    ELSE.
      " For multiple objects we use the count instead
      WRITE lv_count TO lv_value LEFT-JUSTIFIED.
      CONCATENATE lv_value 'objects' INTO rv_text SEPARATED BY space.
    ENDIF.

  ENDMETHOD.


  METHOD parse_commit_request.

    DATA lo_map TYPE REF TO zcl_abapgit_string_map.

    lo_map = ii_event->form_data( ).
    lo_map->to_abap( CHANGING cs_container = rs_commit ).
    REPLACE ALL OCCURRENCES
      OF zif_abapgit_definitions=>c_crlf
      IN rs_commit-body
      WITH zif_abapgit_definitions=>c_newline.

  ENDMETHOD.


  METHOD propose_default_body.

    "Collect Requests of Staged Objects
    DATA: lt_requests TYPE SORTED TABLE OF trkorr
            WITH UNIQUE KEY table_line.
    DATA: li_cts_api TYPE REF TO zif_abapgit_cts_api.

    DATA: lt_request_docu TYPE STANDARD TABLE OF tline
            WITH EMPTY KEY.

    DATA: lt_tasks TYPE SORTED TABLE OF e070
            WITH UNIQUE KEY trkorr.

    li_cts_api = zcl_abapgit_factory=>get_cts_api( ).
    LOOP AT mo_stage->get_all( ) INTO DATA(ls_object)
      WHERE status-obj_type IS NOT INITIAL
        AND status-obj_name IS NOT INITIAL
        AND status-package  IS NOT INITIAL.

      TRY.
          DATA(transport) = li_cts_api->get_transport_for_object(
                              is_item = value #( obj_type = ls_object-status-obj_type
                                                 obj_name = ls_object-status-obj_name )
                              iv_resolve_task_to_request = abap_false ).
          INSERT transport INTO TABLE lt_requests.
        CATCH zcx_abapgit_exception.
          CONTINUE.
      ENDTRY.

    ENDLOOP.

    "Get Documentation of Requests
    IF lt_requests IS NOT INITIAL.
      SELECT trkorr, trfunction
        FROM e070
        FOR ALL ENTRIES IN @lt_requests
        WHERE trkorr     = @lt_requests-table_line
          AND trfunction = 'S'  "We only use texts of tasks
        INTO CORRESPONDING FIELDS OF TABLE @lt_tasks.
    ENDIF.

    LOOP AT lt_requests INTO DATA(request).

      IF NOT line_exists( lt_tasks[ trkorr = request ] ).
        CONTINUE.
      ENDIF.

      CLEAR: lt_request_docu.
      CALL FUNCTION 'TRINT_DOCU_INTERFACE'
        EXPORTING
          iv_object           = request
          iv_action           = 'R'
          iv_modify_appending = 'X'
        TABLES
          tt_line             = lt_request_docu
        EXCEPTIONS
          OTHERS              = 1.
      IF sy-subrc <> 0 OR lt_request_docu IS INITIAL.
        CONTINUE.
      ENDIF.

      IF lt_request_docu IS NOT INITIAL.

        DATA(task_header) = |{ request }:|.
        c_body = COND #( WHEN c_body IS INITIAL
                           THEN c_body && task_header
                         ELSE c_body && cl_abap_char_utilities=>cr_lf && task_header ).

        LOOP AT lt_request_docu INTO DATA(ls_docu).
          c_body = SWITCH #( ls_docu-tdformat
                             WHEN '=' "Line Continuation
                               THEN c_body && ls_docu-tdline
                             ELSE COND #( WHEN c_body IS INITIAL
                                            THEN c_body && ls_docu-tdline
                                          ELSE c_body && cl_abap_char_utilities=>cr_lf && ls_docu-tdline ) ).
        ENDLOOP.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD propose_default_comment.

    "Collect Requests of Staged Objects
    DATA: lt_requests TYPE SORTED TABLE OF trkorr
            WITH UNIQUE KEY table_line.
    DATA: li_cts_api TYPE REF TO zif_abapgit_cts_api.

    DATA: lt_request_docu TYPE STANDARD TABLE OF tline
            WITH EMPTY KEY.

    DATA: lt_docu TYPE SORTED TABLE OF e07t
            WITH UNIQUE KEY trkorr.

    li_cts_api = zcl_abapgit_factory=>get_cts_api( ).
    LOOP AT mo_stage->get_all( ) INTO DATA(ls_object)
      WHERE status-obj_type IS NOT INITIAL
        AND status-obj_name IS NOT INITIAL
        AND status-package  IS NOT INITIAL.

      TRY.
          DATA(transport) = li_cts_api->get_transport_for_object(
                              is_item = value #( obj_type = ls_object-status-obj_type
                                                 obj_name = ls_object-status-obj_name )
                              iv_resolve_task_to_request = abap_false ).
          INSERT transport INTO TABLE lt_requests.
        CATCH zcx_abapgit_exception.
          CONTINUE.
      ENDTRY.

    ENDLOOP.

    IF lt_requests IS NOT INITIAL.
      SELECT e07t~trkorr, as4text
        FROM e07t
        INNER JOIN e070 ON e070~trkorr = e07t~trkorr
        FOR ALL ENTRIES IN @lt_requests
        WHERE e07t~trkorr     = @lt_requests-table_line
          AND e07t~langu      = 'E'
          AND e070~trfunction = 'S'  "We only use texts of tasks
          AND e07t~as4text IS NOT NULL
          AND e07t~as4text <> ''
        INTO CORRESPONDING FIELDS OF TABLE @lt_docu.
    ENDIF.

    IF lines( lt_docu ) = 1.
      "There is exactly one match that we can use as a proposal for the comment
      c_comment = lt_docu[ 1 ]-as4text.
    ENDIF.

  ENDMETHOD.


  METHOD render_content.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div class="repo">' ).
    ri_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top(
      io_repo         = mo_repo
      iv_show_package = abap_false ) ).

    ri_html->add( render_menu( ) ).
    ri_html->add( render_form( ) ).
    ri_html->add( render_stage( ) ).
    ri_html->add( '</div>' ).

    register_deferred_script( render_scripts( ) ).

  ENDMETHOD.


  METHOD render_form.

    CONSTANTS: lc_body_col_max TYPE i VALUE 150.

    DATA: li_user      TYPE REF TO zif_abapgit_persist_user.
    DATA: lv_user      TYPE string.
    DATA: lv_email     TYPE string.
    DATA: lv_s_param   TYPE string.
    DATA: lo_settings  TYPE REF TO zcl_abapgit_settings.
    DATA: lv_body_size TYPE i.
    DATA: lv_comment   TYPE string.
    DATA: lv_body      TYPE string.
    DATA: lv_author_name TYPE string.
    DATA: lv_author_email TYPE string.

* see https://git-scm.com/book/ch5-2.html
* commit messages should be max 50 characters
* body should wrap at 72 characters

    li_user = zcl_abapgit_persistence_user=>get_instance( ).

    lv_user  = li_user->get_repo_git_user_name( mo_repo->get_url( ) ).
    IF lv_user IS INITIAL.
      lv_user  = li_user->get_default_git_user_name( ).
    ENDIF.
    IF lv_user IS INITIAL.
      " get default from user record
      lv_user = zcl_abapgit_user_record=>get_instance( sy-uname )->get_name( ).
    ENDIF.

    lv_email = li_user->get_repo_git_user_email( mo_repo->get_url( ) ).
    IF lv_email IS INITIAL.
      lv_email = li_user->get_default_git_user_email( ).
    ENDIF.
    IF lv_email IS INITIAL.
      " get default from user record
      lv_email = zcl_abapgit_user_record=>get_instance( sy-uname )->get_email( ).
    ENDIF.

    IF ms_commit IS NOT INITIAL.
      lv_user = ms_commit-committer_name.
      lv_email = ms_commit-committer_email.
      lv_comment = ms_commit-comment.
      lv_body = ms_commit-body.
      lv_author_name = ms_commit-author_name.
      lv_author_email = ms_commit-author_email.
    ENDIF.

    IF lv_comment IS INITIAL.
      lv_comment = get_comment_default( ).
    ENDIF.

    propose_default_comment( CHANGING c_comment = lv_comment ).
    propose_default_body( CHANGING c_body = lv_body ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div class="form-container">' ).
    ri_html->add( '<form id="commit_form" class="aligned-form"'
               && ' method="post" action="sapevent:commit_post">' ).

    ri_html->add( render_text_input( iv_name  = 'committer_name'
                                     iv_label = 'Committer Name'
                                     iv_value = lv_user ) ).

    ri_html->add( render_text_input( iv_name  = 'committer_email'
                                     iv_label = 'Committer E-mail'
                                     iv_value = lv_email ) ).

    lo_settings = zcl_abapgit_persist_settings=>get_instance( )->read( ).

    lv_s_param = lo_settings->get_commitmsg_comment_length( ).

    ri_html->add( render_text_input( iv_name       = 'comment'
                                     iv_label      = 'Comment'
                                     iv_value      = lv_comment
                                     iv_max_length = lv_s_param ) ).

    ri_html->add( '<div class="row">' ).
    ri_html->add( '<label for="c-body">Body</label>' ).

    lv_body_size = lo_settings->get_commitmsg_body_size( ).
    IF lv_body_size > lc_body_col_max.
      lv_body_size = lc_body_col_max.
    ENDIF.
    ri_html->add( |<textarea id="c-body" name="body" rows="10" cols="| &&
                  |{ lv_body_size }">{ lv_body }</textarea>| ).

    ri_html->add( '<input type="submit" class="hidden-submit">' ).
    ri_html->add( '</div>' ).

    ri_html->add( '<div class="row">' ).
    ri_html->add( '<span class="cell"></span>' ).
    ri_html->add( '<span class="cell sub-title">Optionally,'
               && ' specify author (same as committer by default)</span>' ).
    ri_html->add( '</div>' ).

    ri_html->add( render_text_input( iv_name  = 'author_name'
                                     iv_label = 'Author Name'
                                     iv_value = lv_author_name ) ).

    ri_html->add( render_text_input( iv_name  = 'author_email'
                                     iv_label = 'Author E-mail'
                                     iv_value = lv_author_email ) ).

    ri_html->add( '</form>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_menu.

    DATA lo_toolbar TYPE REF TO zcl_abapgit_html_toolbar.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    CREATE OBJECT lo_toolbar.

    lo_toolbar->add( iv_act = 'submitFormById(''commit_form'');'
                     iv_txt = 'Commit'
                     iv_typ = zif_abapgit_html=>c_action_type-onclick
                     iv_opt = zif_abapgit_html=>c_html_opt-strong ).

    lo_toolbar->add( iv_act = c_action-commit_cancel
                     iv_txt = 'Cancel'
                     iv_opt = zif_abapgit_html=>c_html_opt-cancel ).

    ri_html->add( '<div class="paddings">' ).
    ri_html->add( lo_toolbar->render( ) ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_scripts.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).
    ri_html->add( 'setInitialFocus("comment");' ).

  ENDMETHOD.


  METHOD render_stage.

    DATA: lt_stage TYPE zif_abapgit_definitions=>ty_stage_tt.

    FIELD-SYMBOLS: <ls_stage> LIKE LINE OF lt_stage.


    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    lt_stage = mo_stage->get_all( ).

    ri_html->add( '<table class="stage_tab">' ).
    ri_html->add( '<thead>' ).
    ri_html->add( '<tr>' ).
    ri_html->add( '<th colspan="3">Staged Files</th>' ).
    ri_html->add( '</tr>' ).
    ri_html->add( '</thead>' ).

    ri_html->add( '<tbody>' ).
    LOOP AT lt_stage ASSIGNING <ls_stage>.
      ri_html->add( '<tr>' ).
      ri_html->add( '<td>' ).
      ri_html->add( zcl_abapgit_gui_chunk_lib=>render_item_state(
        iv_lstate = <ls_stage>-status-lstate
        iv_rstate = <ls_stage>-status-rstate ) ).
      ri_html->add( '</td>' ).
      ri_html->add( '<td class="method">' ).
      ri_html->add( |<b>{ zcl_abapgit_stage=>method_description( <ls_stage>-method ) }</b>| ).
      ri_html->add( '</td>' ).
      ri_html->add( '<td>' ).
      ri_html->add( <ls_stage>-file-path && <ls_stage>-file-filename ).
      ri_html->add( '</td>' ).
      ri_html->add( '</tr>' ).
    ENDLOOP.
    ri_html->add( '</tbody>' ).

    ri_html->add( '</table>' ).

  ENDMETHOD.


  METHOD render_text_input.

    DATA lv_attrs TYPE string.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    IF iv_value IS NOT INITIAL AND
       iv_max_length IS NOT INITIAL.
      lv_attrs = | value="{ iv_value }" maxlength="{ iv_max_length }"|.
    ELSEIF iv_value IS NOT INITIAL.
      lv_attrs = | value="{ iv_value }"|.

    ELSEIF iv_max_length IS NOT INITIAL.
      lv_attrs = | maxlength="{ iv_max_length }"|.
    ENDIF.

    ri_html->add( '<div class="row">' ).
    ri_html->add( |<label for="{ iv_name }">{ iv_label }</label>| ).
    ri_html->add( |<input id="{ iv_name }" name="{ iv_name }" type="text"{ lv_attrs }>| ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD supplement_transport_locks.

    "Supplement Transport Request/Task Lock Links

    "Collect Locks
    DATA: lt_locks TYPE SORTED TABLE OF trkorr
            WITH UNIQUE KEY table_line.
    DATA: li_cts_api TYPE REF TO zif_abapgit_cts_api.

    li_cts_api = zcl_abapgit_factory=>get_cts_api( ).
    LOOP AT mo_stage->get_all( ) INTO DATA(ls_object)
      WHERE status-obj_type IS NOT INITIAL
        AND status-obj_name IS NOT INITIAL
        AND status-package  IS NOT INITIAL.

      TRY.
          DATA(transport) = li_cts_api->get_transport_for_object(
                              is_item = value #( obj_type = ls_object-status-obj_type
                                                 obj_name = ls_object-status-obj_name )
                              iv_resolve_task_to_request = abap_false ).
          INSERT transport INTO TABLE lt_locks.
        CATCH zcx_abapgit_exception.
          CONTINUE.
      ENDTRY.

    ENDLOOP.

    "Supplement Information
    "zif_abapgit_definitions=>c_newline
    IF lt_locks IS NOT INITIAL.
      DATA(lock_link) = | (|.
      LOOP AT lt_locks INTO DATA(lock).
        IF sy-tabix > 1.
          lock_link = lock_link && |, { lock }|.
        ELSE.
          lock_link = lock_link && |{ lock }|.
        ENDIF.
      ENDLOOP.
      lock_link = lock_link && |)|.
      cs_commit-comment = cs_commit-comment && lock_link.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_action-commit_post.

        ms_commit = parse_commit_request( ii_event ).

        supplement_transport_locks(
          CHANGING cs_commit = ms_commit ).

        ms_commit-repo_key = mo_repo->get_key( ).

        zcl_abapgit_services_git=>commit(
          is_commit = ms_commit
          io_repo   = mo_repo
          io_stage  = mo_stage ).

        MESSAGE 'Commit was successful' TYPE 'S'.

        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back_to_bookmark.

      WHEN c_action-commit_cancel.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.

      WHEN OTHERS.
        rs_handled = super->zif_abapgit_gui_event_handler~on_event( ii_event ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
