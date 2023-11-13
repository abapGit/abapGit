CLASS zcl_abapgit_gui_page_pull DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abapgit_gui_menu_provider.
    INTERFACES zif_abapgit_gui_renderable.

    CONSTANTS: BEGIN OF c_action,
                 back    TYPE string VALUE 'back',
                 pull    TYPE string VALUE 'pull',
                 refresh TYPE string VALUE 'refresh',
               END OF c_action.

    CLASS-METHODS create
      IMPORTING
        io_repo        TYPE REF TO zcl_abapgit_repo
        ii_obj_filter  TYPE REF TO zif_abapgit_object_filter OPTIONAL
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      IMPORTING
        io_repo       TYPE REF TO zcl_abapgit_repo
        ii_obj_filter TYPE REF TO zif_abapgit_object_filter OPTIONAL
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA mo_repo       TYPE REF TO zcl_abapgit_repo.
    DATA mi_obj_filter TYPE REF TO zif_abapgit_object_filter.

ENDCLASS.


CLASS zcl_abapgit_gui_page_pull IMPLEMENTATION.

  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_pull.

    CREATE OBJECT lo_component
      EXPORTING
        io_repo       = io_repo
        ii_obj_filter = ii_obj_filter.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Pull'
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.

  METHOD zif_abapgit_gui_menu_provider~get_menu.

    CREATE OBJECT ro_toolbar EXPORTING iv_id = 'toolbar-main'.

    ro_toolbar->add(
      iv_txt = 'Refresh'
      iv_act = c_action-refresh ).

    ro_toolbar->add(
      iv_txt = 'Back'
      iv_act = c_action-back ).

  ENDMETHOD.

  METHOD constructor.

    super->constructor( ).

    mo_repo       = io_repo.
    mi_obj_filter = ii_obj_filter.

  ENDMETHOD.

  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_action-refresh.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_action-pull.
      " mo_repo->deserialize(
        BREAK-POINT.
    ENDCASE.

  ENDMETHOD.

  METHOD zif_abapgit_gui_renderable~render.

    DATA ls_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks.
    DATA lt_filter TYPE zif_abapgit_definitions=>ty_tadir_tt.

    FIELD-SYMBOLS <ls_overwrite> LIKE LINE OF ls_checks-overwrite.


    IF mi_obj_filter IS NOT INITIAL.
      lt_filter = mi_obj_filter->get_filter( ).
    ENDIF.

    register_handlers( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    ri_html->add( '<div class="repo-overview">' ).

    ls_checks = mo_repo->deserialize_checks( ).

    IF lines( ls_checks-overwrite ) = 0.
      zcx_abapgit_exception=>raise(
        'There is nothing to pull. The local state completely matches the remote repository.' ).
    ENDIF.

    IF ls_checks-requirements-met = zif_abapgit_definitions=>c_no.
      ri_html->add( 'todo, requirements not met<br>' ).
    ENDIF.

    LOOP AT ls_checks-overwrite ASSIGNING <ls_overwrite>.
      IF lines( lt_filter ) > 0.
        READ TABLE lt_filter WITH KEY object = <ls_overwrite>-obj_type
          obj_name = <ls_overwrite>-obj_name TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
      ENDIF.
      ri_html->add( |todo, overwrite, <tt>{ <ls_overwrite>-obj_type } { <ls_overwrite>-obj_name }</tt><br>| ).
    ENDLOOP.

    LOOP AT ls_checks-warning_package ASSIGNING <ls_overwrite>.
      ri_html->add( 'todo, warning package<br>' ).
    ENDLOOP.

    IF ls_checks-transport-required = abap_true AND ls_checks-transport-transport IS INITIAL.
      ri_html->add( 'todo, transport required<br>' ).
    ENDIF.

    ri_html->add( '</div>' ).

  ENDMETHOD.

ENDCLASS.
