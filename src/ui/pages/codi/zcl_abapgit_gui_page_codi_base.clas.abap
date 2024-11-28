CLASS zcl_abapgit_gui_page_codi_base DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_html_table.

  PROTECTED SECTION.

    CONSTANTS:
      BEGIN OF c_actions,
        rerun  TYPE string VALUE 'rerun',
        stage  TYPE string VALUE 'stage',
        commit TYPE string VALUE 'commit',
        filter_kind  TYPE string VALUE 'filter_kind',
        apply_filter TYPE string VALUE 'apply_filter',
      END OF c_actions .

    DATA mo_repo TYPE REF TO zcl_abapgit_repo.
    DATA mt_result TYPE zif_abapgit_code_inspector=>ty_results.
    DATA mv_summary TYPE string.

    METHODS on_event
      IMPORTING
        ii_event          TYPE REF TO zif_abapgit_gui_event
      RETURNING
        VALUE(rs_handled) TYPE zif_abapgit_gui_event_handler=>ty_handling_result
      RAISING
        zcx_abapgit_exception.

    METHODS render_ci_report
      IMPORTING
        !ii_html        TYPE REF TO zif_abapgit_html
        !iv_variant     TYPE sci_chkv
        !iv_success_msg TYPE string
      RAISING
        zcx_abapgit_exception.
    METHODS render_head
      IMPORTING
        !ii_html    TYPE REF TO zif_abapgit_html
        !iv_variant TYPE sci_chkv
        !iv_summary TYPE string.
    METHODS render_detail
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
        ii_html    TYPE REF TO zif_abapgit_html
        iv_message TYPE string.
    METHODS build_base_menu
      RETURNING
        VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar.

  PRIVATE SECTION.

    CONSTANTS c_object_separator TYPE c LENGTH 1 VALUE '|'.
    CONSTANTS c_ci_sig TYPE string VALUE 'cinav:'.
    CONSTANTS c_limit TYPE i VALUE 500.
    DATA mv_filter_kind TYPE string.
    DATA ms_sorting_state TYPE zif_abapgit_html_table=>ty_sorting_state.

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
        it_result      TYPE zif_abapgit_code_inspector=>ty_results
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
        VALUE(rv_link) TYPE string.
    METHODS jump
      IMPORTING
        !is_item        TYPE zif_abapgit_definitions=>ty_item
        !is_sub_item    TYPE zif_abapgit_definitions=>ty_item
        !iv_line_number TYPE i
      RAISING
        zcx_abapgit_exception.
    METHODS apply_sorting
      CHANGING
        ct_view TYPE ty_view_tab.
    METHODS apply_filter_kind
      CHANGING
        ct_view TYPE ty_view_tab.
    METHODS handle_navigation
      IMPORTING
        iv_link TYPE string
      RAISING
        zcx_abapgit_exception.
    METHODS render_stat
      IMPORTING
        !ii_html  TYPE REF TO zif_abapgit_html
        !iv_count TYPE i
        !iv_type  TYPE string
        !iv_title TYPE string
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_gui_page_codi_base IMPLEMENTATION.


  METHOD apply_filter_kind.

    CASE mv_filter_kind.
      WHEN 'error'.
        DELETE ct_view WHERE kind <> 'E'.
      WHEN 'warn'.
        DELETE ct_view WHERE kind <> 'W'.
      WHEN 'info'.
        DELETE ct_view WHERE NOT ( kind = 'E' OR kind = 'W' ).
      WHEN OTHERS.
        RETURN.
    ENDCASE.

  ENDMETHOD.


  METHOD apply_sorting.

    DATA lv_field TYPE abap_compname.

    CASE ms_sorting_state-column_id.
      WHEN 'kind' OR 'obj_type' OR 'location' OR 'text'.
        lv_field = to_upper( ms_sorting_state-column_id ).
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    IF ms_sorting_state-descending = abap_true.
      SORT ct_view BY (lv_field) DESCENDING.
    ELSE.
      SORT ct_view BY (lv_field) ASCENDING.
    ENDIF.

  ENDMETHOD.


  METHOD build_base_menu.

    ro_menu = zcl_abapgit_html_toolbar=>create( )->add(
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
      CATCH cx_root ##NO_HANDLER.
        " leave empty, fallback to default, defined elsewhere
    ENDTRY.

  ENDMETHOD.


  METHOD handle_navigation.

    DATA: ls_item          TYPE zif_abapgit_definitions=>ty_item,
          ls_sub_item      TYPE zif_abapgit_definitions=>ty_item,
          lv_main_object   TYPE string,
          lv_sub_object    TYPE string,
          lv_line_number_s TYPE string,
          lv_line_number   TYPE i.

    SPLIT iv_link AT c_object_separator INTO lv_main_object lv_sub_object lv_line_number_s.
    ls_item-obj_type = to_upper( lv_main_object(4) ).
    ls_item-obj_name = to_upper( lv_main_object+4(*) ).

    IF lv_sub_object IS NOT INITIAL.
      ls_sub_item-obj_type = to_upper( lv_sub_object(4) ).
      ls_sub_item-obj_name = to_upper( lv_sub_object+4(*) ).
    ENDIF.

    lv_line_number = lv_line_number_s.

    jump(
      is_item        = ls_item
      is_sub_item    = ls_sub_item
      iv_line_number = lv_line_number ).

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
      CATCH zcx_abapgit_exception ##NO_HANDLER.
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

    DATA lv_temp TYPE string.
    DATA ls_sorting_req TYPE zif_abapgit_html_table=>ty_sorting_state.

    lv_temp = replace(
      val   = ii_event->mv_action
      regex = |^{ c_ci_sig }|
      with  = `` ).

    IF lv_temp <> ii_event->mv_action. " CI navigation request detected
      handle_navigation( lv_temp ).
      rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
    ENDIF.

    ls_sorting_req = zcl_abapgit_html_table=>detect_sorting_request( ii_event->mv_action ).
    IF ls_sorting_req IS NOT INITIAL.
      ms_sorting_state = ls_sorting_req.
      rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
    ENDIF.

    IF ii_event->mv_action = c_actions-filter_kind.
      mv_filter_kind = ii_event->query( )->get( 'kind' ).
      rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
    ENDIF.

  ENDMETHOD.


  METHOD render_ci_report.

    ii_html->add( '<div class="ci">' ).

    render_head(
      ii_html    = ii_html
      iv_variant = iv_variant
      iv_summary = mv_summary ).

    IF lines( mt_result ) = 0.
      render_success(
        ii_html    = ii_html
        iv_message = iv_success_msg ).
    ELSE.
      render_stats(
        ii_html   = ii_html
        it_result = mt_result ).
      render_detail(
        ii_html   = ii_html
        it_result = mt_result ).
    ENDIF.

    ii_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_detail.

    DATA li_table TYPE REF TO zcl_abapgit_html_table.
    DATA lt_view TYPE ty_view_tab.

    li_table = zcl_abapgit_html_table=>create(
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
        iv_column_title = 'Text' ).

    lt_view = convert_result_to_view( it_result ).

    IF ms_sorting_state-column_id IS INITIAL.
      ms_sorting_state-column_id = 'kind'.
    ENDIF.

    apply_sorting( CHANGING ct_view = lt_view ).
    apply_filter_kind( CHANGING ct_view = lt_view ).

    ii_html->div(
      iv_class   = 'ci-detail'
      ii_content = li_table->render(
        ii_renderer      = me
        is_sorting_state = ms_sorting_state
        iv_wrap_in_div   = 'default-table-container'
        iv_css_class     = 'default-table'
        iv_with_cids     = abap_true
        it_data          = lt_view ) ).

    IF lines( it_result ) > c_limit.
      render_limit_warning( ii_html ).
    ENDIF.

  ENDMETHOD.


  METHOD render_head.

    ii_html->div(
      iv_class = 'ci-msg'
      iv_content =
        |Code inspector check variant <span class="ci-variant">{
        iv_variant }</span> completed ({ iv_summary })| ).

  ENDMETHOD.


  METHOD render_limit_warning.
    ii_html->add( '<div class="dummydiv warning">' ).
    ii_html->add( ii_html->icon( 'exclamation-triangle' ) ).
    ii_html->add( |Only first { c_limit } findings shown in list!| ).
    ii_html->add( '</div>' ).
  ENDMETHOD.


  METHOD render_stat.

    DATA lv_txt TYPE string.

    lv_txt = |{ iv_count } { iv_title }|.

    IF iv_type <> mv_filter_kind.
      lv_txt = ii_html->a(
        iv_txt = lv_txt
        iv_act = |{ c_actions-filter_kind }?kind={ iv_type }|
        iv_typ = zif_abapgit_html=>c_action_type-sapevent ).
    ENDIF.

    ii_html->add( |<span class="count { iv_type }-count">{ lv_txt }</span>| ).

  ENDMETHOD.


  METHOD render_stats.

    FIELD-SYMBOLS <ls_i> LIKE LINE OF it_result.

    DATA lv_errors TYPE i.
    DATA lv_warnings TYPE i.
    DATA lv_infos TYPE i.

    IF mv_filter_kind IS INITIAL.
      mv_filter_kind = 'all'.
    ENDIF.

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

    IF lv_errors > 0.
      render_stat(
        ii_html  = ii_html
        iv_count = lv_errors
        iv_type  = 'error'
        iv_title = 'errors' ).
    ENDIF.
    IF lv_warnings > 0.
      render_stat(
        ii_html  = ii_html
        iv_count = lv_warnings
        iv_type  = 'warn'
        iv_title = 'warnings' ).
    ENDIF.
    IF lv_infos > 0.
      render_stat(
        ii_html  = ii_html
        iv_count = lv_infos
        iv_type  = 'info'
        iv_title = 'infos' ).
    ENDIF.
    render_stat(
      ii_html  = ii_html
      iv_count = lv_infos + lv_errors + lv_warnings
      iv_type  = 'all'
      iv_title = 'all' ).

    ii_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_success.

    ii_html->add( '<div class="dummydiv success">' ).
    ii_html->add( ii_html->icon( 'check' ) ).
    ii_html->add( iv_message ).
    ii_html->add( '</div>' ).

  ENDMETHOD.


  METHOD zif_abapgit_html_table~get_row_attrs.

    FIELD-SYMBOLS <ls_item> TYPE ty_result_view.

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
        rs_render-content = |<span>{ <ls_item>-kind }</span>|.
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
