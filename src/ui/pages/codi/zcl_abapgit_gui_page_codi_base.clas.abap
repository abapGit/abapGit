CLASS zcl_abapgit_gui_page_codi_base DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_html_table.

  PROTECTED SECTION.

    CONSTANTS:
      BEGIN OF c_actions,
        rerun  TYPE string VALUE 'rerun',
        sort_1 TYPE string VALUE 'sort_1',
        sort_2 TYPE string VALUE 'sort_2',
        sort_3 TYPE string VALUE 'sort_3',
        stage  TYPE string VALUE 'stage',
        commit TYPE string VALUE 'commit',
        apply_filter TYPE string VALUE 'apply_filter',
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
        !ii_html       TYPE REF TO zif_abapgit_html
        !iv_variant    TYPE sci_chkv
        !iv_summary    TYPE string
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    METHODS render_result
      IMPORTING
        !ii_html   TYPE REF TO zif_abapgit_html
        !it_result TYPE zif_abapgit_code_inspector=>ty_results
      RAISING
        zcx_abapgit_exception.
    METHODS render_stats
      IMPORTING
        !ii_html   TYPE REF TO zif_abapgit_html
        !it_result TYPE zif_abapgit_code_inspector=>ty_results
      RAISING
        zcx_abapgit_exception.
    METHODS render_success
      IMPORTING
        ii_html TYPE REF TO zif_abapgit_html
        iv_message TYPE string.

    METHODS build_base_menu
      RETURNING
        VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar.

  PRIVATE SECTION.
    CONSTANTS c_object_separator TYPE c LENGTH 1 VALUE '|'.
    CONSTANTS c_ci_sig TYPE string VALUE 'cinav:'.
    CONSTANTS c_limit TYPE i VALUE 500.
    DATA mv_filter TYPE string.

    TYPES:
      BEGIN OF ty_result_view,
        kind     TYPE zif_abapgit_code_inspector=>ty_result-kind,
        obj_type TYPE zif_abapgit_code_inspector=>ty_result-objtype,
        location TYPE string,
        text     TYPE string,
        nav      TYPE string,
      END OF ty_result_view,
      ty_view_tab TYPE STANDARD TABLE OF ty_result_view WITH DEFAULT KEY.

    METHODS convert_result_to_view
      IMPORTING
        it_result TYPE zif_abapgit_code_inspector=>ty_results
      RETURNING
        VALUE(rt_view) TYPE ty_view_tab.
    METHODS explain_include
      IMPORTING
        !is_result    TYPE zif_abapgit_code_inspector=>ty_result
      RETURNING
        VALUE(rv_txt) TYPE string.
    METHODS render_limit_warning
      IMPORTING
        !ii_html TYPE REF TO zif_abapgit_html.
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


  METHOD convert_result_to_view.

    FIELD-SYMBOLS <ls_r> LIKE LINE OF it_result.
    FIELD-SYMBOLS <ls_v> LIKE LINE OF rt_view.
    DATA lv_line TYPE i.

    LOOP AT it_result ASSIGNING <ls_r>.
      APPEND INITIAL LINE TO rt_view ASSIGNING <ls_v>.
      <ls_v>-kind     = <ls_r>-kind.
      <ls_v>-obj_type = <ls_r>-objtype.
      <ls_v>-nav      = build_nav_link( <ls_r> ).
      <ls_v>-text     = <ls_r>-text.

      IF <ls_r>-sobjname IS INITIAL OR
         ( <ls_r>-sobjname = <ls_r>-objname AND
           <ls_r>-sobjtype = <ls_r>-objtype ).
        <ls_v>-location = to_lower( <ls_r>-objname ).
      ELSEIF <ls_r>-objtype = 'CLAS' OR
           ( <ls_r>-objtype = 'PROG' AND NOT <ls_r>-sobjname+30(*) IS INITIAL ).
        <ls_v>-location = explain_include( <ls_r> ).
      ENDIF.

      IF <ls_v>-location IS INITIAL. " Fallback to a reasonable default
        <ls_v>-location = to_lower( |{ <ls_r>-objname } &gt; { <ls_r>-sobjtype } { <ls_r>-sobjname }| ).
      ENDIF.

      lv_line         = <ls_r>-line. " convert from numc to integer
      <ls_v>-location = |{ <ls_v>-location }&nbsp;@{ lv_line }|.

    ENDLOOP.

  ENDMETHOD.


  METHOD explain_include.

    DATA ls_mtdkey TYPE seocpdkey.

    TRY.
        CASE is_result-sobjname+30(*).
          WHEN 'CCDEF'.
            rv_txt = |{ to_lower( is_result-objname ) }: Local Definitions|.
          WHEN 'CCIMP'.
            rv_txt = |{ to_lower( is_result-objname ) }: Local Implementations|.
          WHEN 'CCMAC'.
            rv_txt = |{ to_lower( is_result-objname ) }: Macros|.
          WHEN 'CCAU'.
            rv_txt = |{ to_lower( is_result-objname ) }: Test Classes|.
          WHEN 'CU'.
            rv_txt = |{ to_lower( is_result-objname ) }: Public Section|.
          WHEN 'CO'.
            rv_txt = |{ to_lower( is_result-objname ) }: Protected Section|.
          WHEN 'CI'.
            rv_txt = |{ to_lower( is_result-objname ) }: Private Section|.
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
              rv_txt = to_lower( |{ ls_mtdkey-clsname }->{ ls_mtdkey-cpdname }| ).
            ELSE.
              rv_txt = to_lower( |{ is_result-sobjname }| ).
            ENDIF.

        ENDCASE.
      CATCH cx_root.
        " leave empty, fallback to default, defined elsewhere
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


  METHOD render_limit_warning.
    ii_html->add( '<div class="dummydiv warning">' ).
    ii_html->add( ii_html->icon( 'exclamation-triangle' ) ).
    ii_html->add( |Only first { c_limit } findings shown in list!| ).
    ii_html->add( '</div>' ).
  ENDMETHOD.


  METHOD render_result.

    DATA li_table TYPE REF TO zcl_abapgit_html_table.

    li_table = zcl_abapgit_html_table=>create( me
      )->define_column(
        iv_column_id    = 'kind'
        iv_column_title = zcl_abapgit_html=>icon( 'exclamation-circle' )
      )->define_column(
        iv_column_id    = 'obj_type'
        iv_column_title = 'Type'
      )->define_column(
        iv_column_id    = 'location'
        iv_column_title = 'Obj. name / location'
      )->define_column(
        iv_column_id    = 'text'
        iv_column_title = 'Text'
      ).

    ii_html->div(
      iv_class   = 'ci-result'
      ii_content = li_table->render(
        iv_with_cids = abap_true
        it_data      = convert_result_to_view( it_result ) ) ).

    " TODO
    " - status design
    " - sorting
    " - filter
    " - search

    IF lines( it_result ) > c_limit.
      render_limit_warning( ii_html ).
    ENDIF.

  ENDMETHOD.


  METHOD render_stats.

    FIELD-SYMBOLS <ls_i> LIKE LINE OF it_result.

    DATA lv_errors TYPE i.
    DATA lv_warnings TYPE i.
    DATA lv_infos TYPE i.

    LOOP AT it_result ASSIGNING <ls_i>.
      CASE <ls_i>-kind.
        WHEN 'E'.
          lv_errors = lv_errors + 1.
        WHEN 'W'.
          lv_warnings = lv_warnings + 1.
        WHEN OTHERS.
          lv_infos = lv_infos + 1.
      ENDCASE.
    ENDLOOP.

    ii_html->add( '<div class="ci-stats">' ).

    ii_html->add( |<form class="inline" method="post" action="sapevent:{ c_actions-apply_filter }">| ).
    ii_html->add( zcl_abapgit_gui_chunk_lib=>render_text_input(
      iv_name      = 'filter'
      iv_label     = 'Filter:'
      iv_value     = mv_filter ) ). " TODO placeholder ?
    ii_html->add( |<input type="submit" class="hidden-submit" title="Filter">| ).
    ii_html->add( |</form>| ).

    IF lv_errors > 0.
      ii_html->add( |<span class="error-count">{ lv_errors } errors</span>| ).
    ENDIF.
    IF lv_warnings > 0.
      ii_html->add( |<span class="warn-count">{ lv_warnings } warnings</span>| ).
    ENDIF.
    IF lv_infos > 0.
      ii_html->add( |<span class="info-count">{ lv_infos } info</span>| ).
    ENDIF.
    ii_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_success.

    ii_html->add( '<div class="dummydiv success">' ).
    ii_html->add( ii_html->icon( 'check' ) ).
    ii_html->add( iv_message ).
    ii_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_variant.

    ii_html->div(
      iv_class = 'ci-head'
      iv_content =
        |Code inspector check variant <span class="ci-variant">{
        iv_variant }</span> completed ({ iv_summary })| ).

  ENDMETHOD.


  METHOD zif_abapgit_html_table~get_row_attrs.

    FIELD-SYMBOLS <ls_item> TYPE ty_result_view.
    DATA lv_render_kind TYPE string.

    ASSIGN is_row TO <ls_item>.
    rs_attrs-data-name = 'kind'.

    CASE <ls_item>-kind.
      WHEN 'E'.
        rs_attrs-data-value = 'error'.
      WHEN 'W'.
        rs_attrs-data-value = 'warning'.
      WHEN OTHERS.
        rs_attrs-data-value = 'info'.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_html_table~render_cell.

    FIELD-SYMBOLS <ls_item> TYPE ty_result_view.

    ASSIGN is_row TO <ls_item>.

    CASE iv_column_id.
      WHEN 'kind'.
        rs_render-content = <ls_item>-kind.
      WHEN 'obj_type'.
        rs_render-content = <ls_item>-obj_type.
      WHEN 'location'.
        rs_render-content = zcl_abapgit_html=>create( )->a(
          iv_txt = <ls_item>-location
          iv_act = <ls_item>-nav
          iv_typ = zif_abapgit_html=>c_action_type-sapevent ).
      WHEN 'text'.
        rs_render-content = escape(
          val    = <ls_item>-text
          format = cl_abap_format=>e_html_attr ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
