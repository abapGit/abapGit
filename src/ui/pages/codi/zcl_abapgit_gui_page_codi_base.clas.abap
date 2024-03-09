CLASS zcl_abapgit_gui_page_codi_base DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component.

  PUBLIC SECTION.

  PROTECTED SECTION.

    CONSTANTS:
      BEGIN OF c_actions,
        rerun  TYPE string VALUE 'rerun',
        sort_1 TYPE string VALUE 'sort_1',
        sort_2 TYPE string VALUE 'sort_2',
        sort_3 TYPE string VALUE 'sort_3',
        stage  TYPE string VALUE 'stage',
        commit TYPE string VALUE 'commit',
      END OF c_actions .
    DATA mo_repo TYPE REF TO zcl_abapgit_repo .
    DATA mt_result TYPE zif_abapgit_code_inspector=>ty_results .
    DATA mv_summary TYPE string.

    METHODS on_event
      IMPORTING
        ii_event          TYPE REF TO zif_abapgit_gui_event
      RETURNING
        VALUE(rs_handled) TYPE zif_abapgit_gui_event_handler=>ty_handling_result
      RAISING
        zcx_abapgit_exception.

    METHODS render_variant
      IMPORTING
        !iv_variant    TYPE sci_chkv
        !iv_summary    TYPE string
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    METHODS render_result
      IMPORTING
        !ii_html   TYPE REF TO zif_abapgit_html
        !it_result TYPE zif_abapgit_code_inspector=>ty_results .
    METHODS render_result_line
      IMPORTING
        !ii_html   TYPE REF TO zif_abapgit_html
        !is_result TYPE zif_abapgit_code_inspector=>ty_result .
    METHODS explain_include
      IMPORTING
        !is_result TYPE zif_abapgit_code_inspector=>ty_result
      RETURNING
        VALUE(rv_txt) TYPE string.
    METHODS render_limit_warning
      IMPORTING
        !ii_html   TYPE REF TO zif_abapgit_html.
    METHODS build_nav_link
      IMPORTING
        !is_result     TYPE zif_abapgit_code_inspector=>ty_result
      RETURNING
        VALUE(rv_link) TYPE string .
    METHODS jump
      IMPORTING
        !is_item        TYPE zif_abapgit_definitions=>ty_item
        !is_sub_item    TYPE zif_abapgit_definitions=>ty_item
        !iv_line_number TYPE i
      RAISING
        zcx_abapgit_exception .
    METHODS build_base_menu
      RETURNING
        VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar .
  PRIVATE SECTION.
    CONSTANTS c_object_separator TYPE c LENGTH 1 VALUE '|'.
    CONSTANTS c_ci_sig TYPE string VALUE 'cinav:'.
    CONSTANTS c_limit TYPE i VALUE 500.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_CODI_BASE IMPLEMENTATION.


  METHOD build_base_menu.

    DATA lo_sort_menu TYPE REF TO zcl_abapgit_html_toolbar.

    CREATE OBJECT lo_sort_menu.

    lo_sort_menu->add(
      iv_txt = 'By Object, Check, Sub-object'
      iv_act = c_actions-sort_1
    )->add(
      iv_txt = 'By Object, Sub-object, Line'
      iv_act = c_actions-sort_2
    )->add(
      iv_txt = 'By Check, Object, Sub-object'
      iv_act = c_actions-sort_3 ).

    CREATE OBJECT ro_menu.

    ro_menu->add(
      iv_txt = 'Sort'
      io_sub = lo_sort_menu ).

    ro_menu->add(
      iv_txt = 'Re-Run'
      iv_act = c_actions-rerun ).

  ENDMETHOD.


  METHOD build_nav_link.

    rv_link = |{ c_ci_sig }| &&
      |{ is_result-objtype }{ is_result-objname }| &&
      |{ c_object_separator }{ is_result-sobjtype }{ is_result-sobjname }| &&
      |{ c_object_separator }{ is_result-line }|.

  ENDMETHOD.


  METHOD explain_include.

    DATA ls_mtdkey TYPE seocpdkey.

    TRY.
        CASE is_result-sobjname+30(*).
          WHEN 'CCDEF'.
            rv_txt = |CLAS { is_result-objname } : Local Definitions|.
          WHEN 'CCIMP'.
            rv_txt = |CLAS { is_result-objname } : Local Implementations|.
          WHEN 'CCMAC'.
            rv_txt = |CLAS { is_result-objname } : Macros|.
          WHEN 'CCAU'.
            rv_txt = |CLAS { is_result-objname } : Test Classes|.
          WHEN 'CU'.
            rv_txt = |CLAS { is_result-objname } : Public Section|.
          WHEN 'CO'.
            rv_txt = |CLAS { is_result-objname } : Protected Section|.
          WHEN 'CI'.
            rv_txt = |CLAS { is_result-objname } : Private Section|.
          WHEN OTHERS.
            cl_oo_classname_service=>get_method_by_include(
              EXPORTING
                incname             = is_result-sobjname
              RECEIVING
                mtdkey              = ls_mtdkey
              EXCEPTIONS
                class_not_existing  = 1
                method_not_existing = 2
                OTHERS              = 3 ).
            IF sy-subrc = 0.
              rv_txt = |CLAS { ls_mtdkey-clsname }->{ ls_mtdkey-cpdname }|.
            ELSE.
              rv_txt = |{ is_result-objtype } { is_result-sobjname }|.
            ENDIF.

        ENDCASE.
      CATCH cx_root.
        rv_txt = ''. "use default below
    ENDTRY.

  ENDMETHOD.


  METHOD jump.

    DATA: lo_test             TYPE REF TO cl_ci_test_root,
          ls_info             TYPE scir_rest,
          lo_result           TYPE REF TO cl_ci_result_root,
          lv_adt_jump_enabled TYPE abap_bool,
          lv_line_number      TYPE i,
          ls_item             TYPE zif_abapgit_definitions=>ty_item,
          ls_sub_item         TYPE zif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF mt_result.


    IF is_sub_item IS NOT INITIAL.
      READ TABLE mt_result WITH KEY objtype  = is_item-obj_type
                                    objname  = is_item-obj_name
                                    sobjtype = is_sub_item-obj_type
                                    sobjname = is_sub_item-obj_name
                                    line     = iv_line_number
                           ASSIGNING <ls_result>.
    ELSE.
      READ TABLE mt_result WITH KEY objtype = is_item-obj_type
                                    objname = is_item-obj_name
                                    line    = iv_line_number
                           ASSIGNING <ls_result>.
    ENDIF.
    ASSERT <ls_result> IS ASSIGNED.
    ls_item-obj_name = <ls_result>-objname.
    ls_item-obj_type = <ls_result>-objtype.

    ls_sub_item-obj_name = <ls_result>-sobjname.
    ls_sub_item-obj_type = <ls_result>-sobjtype.

    " see SCI_LCL_DYNP_530 / HANDLE_DOUBLE_CLICK

    lv_adt_jump_enabled = zcl_abapgit_persist_factory=>get_settings( )->read( )->get_adt_jump_enabled( ).

    TRY.
        IF lv_adt_jump_enabled = abap_true.

          lv_line_number = <ls_result>-line.

          zcl_abapgit_objects=>jump(
            is_item        = ls_item
            is_sub_item    = ls_sub_item
            iv_line_number = lv_line_number ).
          RETURN.

        ENDIF.
      CATCH zcx_abapgit_exception.
    ENDTRY.

    TRY.
        CALL METHOD ('CL_CI_TESTS')=>('GET_TEST_REF')
          EXPORTING
            p_test   = <ls_result>-test
          RECEIVING
            p_result = lo_test.
      CATCH cx_root.
        zcx_abapgit_exception=>raise( |Jump to object not supported in your NW release| ).
    ENDTRY.

    lo_result = lo_test->get_result_node( <ls_result>-kind ).

    MOVE-CORRESPONDING <ls_result> TO ls_info.

    lo_result->set_info( ls_info ).
    lo_result->if_ci_test~navigate( ).

  ENDMETHOD.


  METHOD on_event.

    DATA: ls_item          TYPE zif_abapgit_definitions=>ty_item,
          ls_sub_item      TYPE zif_abapgit_definitions=>ty_item,
          lv_temp          TYPE string,
          lv_main_object   TYPE string,
          lv_sub_object    TYPE string,
          lv_line_number_s TYPE string,
          lv_line_number   TYPE i.

    lv_temp = replace(
      val   = ii_event->mv_action
      regex = |^{ c_ci_sig }|
      with  = `` ).

    IF lv_temp <> ii_event->mv_action. " CI navigation request detected

      SPLIT lv_temp AT c_object_separator INTO lv_main_object lv_sub_object lv_line_number_s.
      ls_item-obj_type = to_upper( lv_main_object(4) ).
      ls_item-obj_name = to_upper( lv_main_object+4(*) ).

      IF lv_sub_object IS NOT INITIAL.
        ls_sub_item-obj_type = to_upper( lv_sub_object(4) ).
        ls_sub_item-obj_name = to_upper( lv_sub_object+4(*) ).
      ENDIF.

      lv_line_number = lv_line_number_s.

      jump( is_item        = ls_item
            is_sub_item    = ls_sub_item
            iv_line_number = lv_line_number ).

      rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.

    ENDIF.

    CASE ii_event->mv_action.

      WHEN c_actions-sort_1.
        SORT mt_result BY objtype objname test code sobjtype sobjname line col.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_actions-sort_2.
        SORT mt_result BY objtype objname sobjtype sobjname line col test code.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_actions-sort_3.
        SORT mt_result BY test code objtype objname sobjtype sobjname line col.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

    ENDCASE.

  ENDMETHOD.


  method render_limit_warning.
    ii_html->add( '<div class="dummydiv warning">' ).
    ii_html->add( ii_html->icon( 'exclamation-triangle' ) ).
    ii_html->add( |Only first { c_limit } findings shown in list!| ).
    ii_html->add( '</div>' ).
  endmethod.


  METHOD render_result.

    FIELD-SYMBOLS <ls_result> LIKE LINE OF it_result.

    ii_html->add( '<div class="ci-result">' ).

    LOOP AT it_result ASSIGNING <ls_result> TO c_limit.
      render_result_line(
        ii_html   = ii_html
        is_result = <ls_result> ).
    ENDLOOP.

    ii_html->add( '</div>' ).

    IF lines( it_result ) > c_limit.
      render_limit_warning( ii_html ).
    ENDIF.

  ENDMETHOD.


  METHOD render_result_line.

    DATA: lv_class   TYPE string,
          lv_obj_txt TYPE string,
          lv_msg     TYPE string,
          lv_line    TYPE i.

    CASE is_result-kind.
      WHEN 'E'.
        lv_class = 'ci-error'.
      WHEN 'W'.
        lv_class = 'ci-warning'.
      WHEN OTHERS.
        lv_class = 'ci-info'.
    ENDCASE.

    lv_msg = escape(
      val    = is_result-text
      format = cl_abap_format=>e_html_attr ).

    IF is_result-sobjname IS INITIAL OR
       ( is_result-sobjname = is_result-objname AND
         is_result-sobjtype = is_result-objtype ).
      lv_obj_txt = |{ is_result-objtype } { is_result-objname }|.
    ELSEIF is_result-objtype = 'CLAS' OR
         ( is_result-objtype = 'PROG' AND NOT is_result-sobjname+30(*) IS INITIAL ).
      lv_obj_txt = explain_include( is_result ).
    ENDIF.

    IF lv_obj_txt IS INITIAL.
      lv_obj_txt = |{ is_result-objtype } { is_result-objname } &gt; { is_result-sobjtype } { is_result-sobjname }|.
    ENDIF.

    lv_line    = is_result-line. " convert from numc to integer
    lv_obj_txt = |{ lv_obj_txt } [ @{ lv_line } ]|.

    ii_html->add( |<li class="{ lv_class }">| ).
    ii_html->add_a(
      iv_txt = lv_obj_txt
      iv_act = build_nav_link( is_result )
      iv_typ = zif_abapgit_html=>c_action_type-sapevent ).
    ii_html->add( |<span>{ lv_msg }</span>| ).
    ii_html->add( '</li>' ).

  ENDMETHOD.


  METHOD render_variant.

    ri_html = zcl_abapgit_html=>create( )->div(
      iv_class = 'ci-head'
      iv_content =
        |Code inspector check variant <span class="ci-variant">{
        iv_variant }</span> completed ({ iv_summary })| ).

  ENDMETHOD.
ENDCLASS.
