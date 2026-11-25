CLASS zcl_abapgit_gui_page_runit DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abapgit_gui_renderable.
    INTERFACES zif_abapgit_gui_menu_provider.

    CLASS-METHODS create
      IMPORTING
        !ii_repo       TYPE REF TO zif_abapgit_repo
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      IMPORTING
        !ii_repo   TYPE REF TO zif_abapgit_repo
        !ii_runner TYPE REF TO zif_abapgit_unit_test_runner.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_actions,
        rerun TYPE string VALUE 'rerun' ##NO_TEXT,
      END OF c_actions .

    DATA mi_repo TYPE REF TO zif_abapgit_repo.
    DATA mi_runner TYPE REF TO zif_abapgit_unit_test_runner.

    METHODS render_method
      IMPORTING
        !is_method     TYPE zif_abapgit_unit_test_runner=>ty_method
      RETURNING
        VALUE(rv_html) TYPE string.

ENDCLASS.



CLASS zcl_abapgit_gui_page_runit IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).
    mi_repo = ii_repo.
    mi_runner = ii_runner.

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_runit.
    DATA li_runner    TYPE REF TO zif_abapgit_unit_test_runner.

    TRY.
        li_runner = zcl_abapgit_aunit_factory=>get_runner( ii_repo ).

        CREATE OBJECT lo_component
          EXPORTING
            ii_repo   = ii_repo
            ii_runner = li_runner.

        ri_page = zcl_abapgit_gui_page_hoc=>create(
          iv_page_title         = |Unit Tests|
          ii_page_menu_provider = lo_component
          ii_child_component    = lo_component ).

      CATCH zcx_abapgit_exception.

        " Fallback if neither runner is supported in the release
        ri_page = zcl_abapgit_gui_page_code_insp=>create(
                    ii_repo          = ii_repo
                    iv_check_variant = 'SWF_ABAP_UNIT' ).

    ENDTRY.

  ENDMETHOD.


  METHOD render_method.

    DATA lv_text    TYPE string.
    DATA lv_runtime TYPE string.

    IF is_method-has_runtime = abap_true.
      lv_runtime = |{ is_method-runtime_ms } ms|.
    ENDIF.

    IF is_method-failure IS INITIAL.
      lv_text = |<span class="boxed green-filled-set">PASSED</span>|.
      IF is_method-runtime_ms > 100.
        lv_text = lv_text && | <span class="warning">{ lv_runtime }</span>|.
      ELSE.
        lv_text = lv_text && | <span class="success">{ lv_runtime }</span>|.
      ENDIF.
    ELSE.
      lv_text = |<span class="boxed red-filled-set">{ is_method-failure }</span>|.
    ENDIF.

    rv_html = |<tr><td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;{ is_method-name }</td><td>{ lv_text }</td></tr>|.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_actions-rerun.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN OTHERS.
        ASSERT 1 = 1.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_menu_provider~get_menu.

    ro_toolbar = zcl_abapgit_html_toolbar=>create( 'toolbar-unit-tests' ).

    ro_toolbar->add(
      iv_txt = 'Re-Run'
      iv_act = c_actions-rerun
      iv_cur = abap_false ).

    ro_toolbar->add(
      iv_txt = 'Back'
      iv_act = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    DATA ls_result TYPE zif_abapgit_unit_test_runner=>ty_result.
    DATA lv_error  TYPE string.
    DATA ls_item   TYPE zif_abapgit_definitions=>ty_repo_item.

    FIELD-SYMBOLS <ls_program> TYPE zif_abapgit_unit_test_runner=>ty_program.
    FIELD-SYMBOLS <ls_class>   TYPE zif_abapgit_unit_test_runner=>ty_class.
    FIELD-SYMBOLS <ls_method>  TYPE zif_abapgit_unit_test_runner=>ty_method.


    register_handlers( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div class="repo">' ).
    ri_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top( ii_repo        = mi_repo
                                                              iv_show_commit = abap_false ) ).

    ls_result = mi_runner->run( ).

    IF ls_result-programs IS INITIAL.
      ri_html->add( '<div class="ci-head">' ).
      ri_html->add( 'No unit tests found' ).
      ri_html->add( '</div>' ).
      ri_html->add( '</div>' ).
      RETURN.
    ENDIF.

    ri_html->add( |<table class="unit_tests">| ).
    LOOP AT ls_result-errors INTO lv_error.
      ri_html->add( |<tr><td><span class="boxed red-filled-set">{ lv_error }</span></td></tr>| ).
    ENDLOOP.
    ri_html->add( '</table>' ).

    ri_html->add( '<div class="ci-head">' ).
    ri_html->add( |Unit tests completed with <strong>{ lines( ls_result-errors ) } errors</strong>|
      && | ({ ls_result-summary })| ).
    ri_html->add( '</div>' ).

    ri_html->add( |<hr><table class="unit_tests">| ).

    LOOP AT ls_result-programs ASSIGNING <ls_program>.
      IF <ls_program>-obj_type IS NOT INITIAL.
        CLEAR ls_item.
        ls_item-obj_type = <ls_program>-obj_type.
        ls_item-obj_name = <ls_program>-obj_name.
        ri_html->add( |<tr><td>{ zcl_abapgit_gui_chunk_lib=>get_item_icon( ls_item ) } { ls_item-obj_type }|
          && | { zcl_abapgit_gui_chunk_lib=>get_item_link( ls_item ) }</td><td></td></tr>| ).
      ELSE.
        ri_html->add( |<tr><td>{ <ls_program>-name }</td><td></td></tr>| ).
      ENDIF.

      LOOP AT <ls_program>-classes ASSIGNING <ls_class>.
        ri_html->add( |<tr><td>&nbsp;&nbsp;&nbsp;&nbsp;{ <ls_class>-name }</td><td></td></tr>| ).

        LOOP AT <ls_class>-methods ASSIGNING <ls_method>.
          ri_html->add( render_method( <ls_method> ) ).
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    ri_html->add( '</table></div>' ).

  ENDMETHOD.
ENDCLASS.
