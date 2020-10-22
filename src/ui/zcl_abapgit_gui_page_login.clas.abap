CLASS zcl_abapgit_gui_page_login DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler .
    INTERFACES zif_abapgit_gui_renderable .

    CLASS-METHODS create
      IMPORTING
        !iv_url        TYPE string
        !iv_count      TYPE i
        !iv_digest     TYPE string
        !io_page       TYPE REF TO zif_abapgit_gui_renderable
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception .
    METHODS constructor
      IMPORTING
        !iv_url    TYPE string
        !iv_count  TYPE i
        !iv_digest TYPE string
        !io_page   TYPE REF TO zif_abapgit_gui_renderable OPTIONAL
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_id,
        user     TYPE string VALUE 'user',
        password TYPE string VALUE 'password',
      END OF c_id .
    CONSTANTS:
      BEGIN OF c_event,
        login   TYPE string VALUE 'login',
        go_back TYPE string VALUE 'go_back',
      END OF c_event .
    DATA mo_validation_log TYPE REF TO zcl_abapgit_string_map .
    DATA mo_form_data TYPE REF TO zcl_abapgit_string_map .
    DATA mo_form TYPE REF TO zcl_abapgit_html_form .
    DATA mv_url TYPE string .
    DATA mv_count TYPE i .
    DATA mv_digest TYPE string .
    DATA mo_page TYPE REF TO zif_abapgit_gui_renderable .
    DATA mv_default_user TYPE string .

    CLASS-METHODS export_title
      IMPORTING
        !iv_url        TYPE string
      RETURNING
        VALUE(rv_host) TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS validate_form
      IMPORTING
        !io_form_data            TYPE REF TO zcl_abapgit_string_map
      RETURNING
        VALUE(ro_validation_log) TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception .
    METHODS get_form_schema
      IMPORTING
        iv_username    TYPE string
      RETURNING
        VALUE(ro_form) TYPE REF TO zcl_abapgit_html_form .
ENDCLASS.



CLASS zcl_abapgit_gui_page_login IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    mv_url    = iv_url.
    mv_count  = iv_count.
    mv_digest = iv_digest.
    mo_page   = io_page.
    mv_default_user = zcl_abapgit_login_manager=>get_default_user( mv_url ).

    CREATE OBJECT mo_validation_log.
    CREATE OBJECT mo_form_data.
    mo_form = get_form_schema( mv_default_user ).

    mo_form_data->set( iv_key = c_id-user
                       iv_val = mv_default_user ).

  ENDMETHOD.


  METHOD create.

    DATA:
      lo_component TYPE REF TO zcl_abapgit_gui_page_login,
      lo_repo      TYPE REF TO zcl_abapgit_repo,
      lv_host      TYPE string,
      lv_name      TYPE string.

    CREATE OBJECT lo_component
      EXPORTING
        iv_url    = iv_url
        iv_count  = iv_count
        iv_digest = iv_digest
        io_page   = io_page.

    lv_host = export_title( iv_url ).

    IF iv_url <> zcl_abapgit_login_manager=>gc_proxy.
      lo_repo = zcl_abapgit_repo_srv=>get_instance( )->get_repo_from_url( iv_url ).
      IF lo_repo IS BOUND.
        lv_name = |({ zcl_abapgit_html=>icon( 'cloud-upload-alt/grey' )
                  } { lo_repo->get_name( ) })|.
      ENDIF.
    ENDIF.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = |Login to <b>{ lv_host }</b> { lv_name }|
      ii_child_component = lo_component ).

  ENDMETHOD.


  METHOD export_title.

    DATA lv_title TYPE sy-title.

    rv_host = zcl_abapgit_login_manager=>get_host( iv_url ).

    lv_title = |Login: { rv_host }|.
    EXPORT title = lv_title TO MEMORY ID zif_abapgit_definitions=>gc_memoryid_title.

  ENDMETHOD.


  METHOD get_form_schema.

    ro_form = zcl_abapgit_html_form=>create( iv_form_id = 'login-form' ).

    ro_form->text(
      iv_name        = c_id-user
      iv_required    = abap_true
      iv_autofocus   = boolc( iv_username IS INITIAL )
      iv_label       = 'User Name'
    )->text(
      iv_name        = c_id-password
      iv_required    = abap_true
      iv_autofocus   = boolc( iv_username IS NOT INITIAL )
      iv_password    = abap_true
      iv_label       = 'Password or Token'
    )->command(
      iv_label       = 'Login'
      iv_is_main     = abap_true
      iv_action      = c_event-login
    )->command(
      iv_label       = 'Cancel'
      iv_action      = c_event-go_back ).

  ENDMETHOD.


  METHOD validate_form.

    ro_validation_log = mo_form->validate_required_fields( io_form_data ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA lv_username TYPE string.

    " import data from html before re-render
    mo_form_data = mo_form->normalize_form_data( ii_event->form_data( ) ).

    CASE ii_event->mv_action.
      WHEN c_event-go_back.
        zcl_abapgit_login_manager=>reset_count( mv_url ).

        CREATE OBJECT rs_handled-page TYPE zcl_abapgit_gui_page_main.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page_replacing.
      WHEN c_event-login.
        mo_validation_log = validate_form( mo_form_data ).

        IF mo_validation_log->is_empty( ) = abap_true.
          lv_username = mo_form_data->get( c_id-user ).

          " Save new default user for repo
          zcl_abapgit_login_manager=>set_default_user(
            iv_uri      = mv_url
            iv_username = lv_username ).

          " Pass username and password to login manager
          zcl_abapgit_login_manager=>set(
            iv_uri      = mv_url
            iv_username = lv_username
            iv_password = mo_form_data->get( c_id-password )
            iv_digest   = mv_digest ).

          rs_handled-page  = mo_page.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page_replacing.
        ELSE.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render. " Display errors
        ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    DATA lv_msg TYPE string.

    gui_services( )->register_event_handler( me ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    IF mv_count = 1.
      IF mv_url = zcl_abapgit_login_manager=>gc_proxy.
        lv_msg = 'You have to login to your proxy, first'.
      ELSE.
        lv_msg = 'You have to login to access this repository'.
      ENDIF.

      ri_html->add( zcl_abapgit_gui_chunk_lib=>render_warning_banner(
                      iv_text        = lv_msg
                      iv_extra_style = 'wmax600px auto-center margin-v1' ) ).
    ELSEIF mv_count > 1.
      lv_msg = 'Sorry, username or password are not correct. Try again!'.
      ri_html->add( zcl_abapgit_gui_chunk_lib=>render_error(
                      iv_error       = lv_msg
                      iv_extra_style = 'wmax600px auto-center margin-v1' ) ).
    ENDIF.

    ri_html->add( mo_form->render(
      iv_form_class     = 'dialog wmax600px auto-center margin-30px'
      io_values         = mo_form_data
      io_validation_log = mo_validation_log ) ).

  ENDMETHOD.
ENDCLASS.
