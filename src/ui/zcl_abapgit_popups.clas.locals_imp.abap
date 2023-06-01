CLASS lcl_object_descision_list DEFINITION FINAL.
  PUBLIC SECTION.

    CONSTANTS c_default_column     TYPE abap_componentdescr-name VALUE 'DEFAULT_COLUMN'.
    CONSTANTS c_fieldname_selected TYPE abap_componentdescr-name VALUE 'SELECTED'.
    CONSTANTS c_answer_cancel      TYPE c LENGTH 1 VALUE 'A'.
    CONSTANTS c_fieldname_obj_type TYPE abap_componentdescr-name VALUE 'OBJ_TYPE'.
    CONSTANTS c_own_pfstatus       TYPE sy-pfkey VALUE 'DECIDE_DIALOG'.

    METHODS constructor
      IMPORTING
        !it_list               TYPE STANDARD TABLE
        !iv_title              TYPE lvc_title DEFAULT space
        !iv_header_text        TYPE csequence DEFAULT space
        !is_position           TYPE zif_abapgit_popups=>ty_popup_position
        !iv_striped_pattern    TYPE abap_bool DEFAULT abap_false
        !iv_optimize_col_width TYPE abap_bool DEFAULT abap_true
        !iv_selection_mode     TYPE salv_de_constant DEFAULT if_salv_c_selection_mode=>multiple
        !iv_select_column_text TYPE csequence DEFAULT space
        !it_columns_to_display TYPE zif_abapgit_popups=>ty_alv_column_tt
        !it_preselected_rows   TYPE zif_abapgit_popups=>ty_rows OPTIONAL
      RAISING
        zcx_abapgit_exception.

    METHODS display
      RAISING
        zcx_abapgit_exception.
    METHODS get_selected
      EXPORTING
        VALUE(et_list) TYPE STANDARD TABLE.

  PRIVATE SECTION.

    DATA mr_table TYPE REF TO data.
    DATA mo_table_descr TYPE REF TO cl_abap_tabledescr.
    DATA mo_alv TYPE REF TO cl_salv_table.
    DATA mv_cancel TYPE abap_bool.
    DATA ms_position TYPE zif_abapgit_popups=>ty_popup_position.

    " Events
    METHODS on_select_list_link_click
      FOR EVENT link_click OF cl_salv_events_table
      IMPORTING
        !row
        !column.
    METHODS on_select_list_function_click
      FOR EVENT added_function OF cl_salv_events_table
      IMPORTING
        !e_salv_function.
    METHODS on_double_click
      FOR EVENT double_click OF cl_salv_events_table
      IMPORTING
        !row
        !column.

    " Methods
    METHODS create_new_selectable_table
      IMPORTING
        it_list TYPE STANDARD TABLE.
    METHODS preselect
      IMPORTING
        it_preselected_rows TYPE zif_abapgit_popups=>ty_rows OPTIONAL
      RAISING
        zcx_abapgit_exception.
    METHODS create_alv
      RETURNING
        VALUE(ro_alv) TYPE REF TO cl_salv_table
      RAISING
        cx_salv_msg.
    METHODS setup_columns
      IMPORTING
        io_columns            TYPE REF TO cl_salv_columns_table
        iv_selection_mode     TYPE salv_de_constant
        iv_select_column_text TYPE csequence
        it_columns_to_display TYPE zif_abapgit_popups=>ty_alv_column_tt
      RAISING
        cx_salv_msg.
    METHODS setup_toolbar
      IMPORTING
        !iv_selection_mode TYPE salv_de_constant
        !iv_object_list    TYPE abap_bool.
    METHODS ask_user_for_obj_category
      RETURNING
        VALUE(rv_category) TYPE string.
    METHODS mark_category
      IMPORTING
        iv_category TYPE string.
    METHODS mark_all
      IMPORTING
        iv_selected TYPE abap_bool.
    METHODS mark_visible
      IMPORTING
        iv_selected TYPE abap_bool.
    METHODS mark_selected.
    METHODS mark_indexed
      IMPORTING
        iv_selected TYPE abap_bool DEFAULT abap_true
        iv_invert   TYPE abap_bool DEFAULT abap_false
        it_scope    TYPE lvc_t_fidx.
    METHODS are_all_marked
      IMPORTING
        it_scope      TYPE lvc_t_fidx
      RETURNING
        VALUE(rv_yes) TYPE abap_bool.

ENDCLASS.

CLASS lcl_object_descision_list IMPLEMENTATION.

  METHOD display.

    mo_alv->display( ).

    IF mv_cancel = abap_true.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

  ENDMETHOD.

  METHOD get_selected.

    DATA:
      lv_condition     TYPE string,
      lr_exporting     TYPE REF TO data,
      lo_data_descr    TYPE REF TO cl_abap_datadescr,
      lo_selections    TYPE REF TO cl_salv_selections,
      lt_selected_rows TYPE salv_t_row.

    FIELD-SYMBOLS:
      <lg_exporting>    TYPE any,
      <lt_table>        TYPE STANDARD TABLE,
      <ls_line>         TYPE any,
      <lg_value>        TYPE any,
      <lv_selected>     TYPE abap_bool,
      <lv_selected_row> TYPE LINE OF salv_t_row.

    CLEAR et_list.

    " Make sure we don't accidentally return anything
    IF mv_cancel = abap_true.
      RETURN.
    ENDIF.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    lo_selections = mo_alv->get_selections( ).

    IF lo_selections->get_selection_mode( ) = if_salv_c_selection_mode=>single.

      lt_selected_rows = lo_selections->get_selected_rows( ).

      LOOP AT lt_selected_rows ASSIGNING <lv_selected_row>.

        READ TABLE <lt_table> ASSIGNING <ls_line> INDEX <lv_selected_row>.
        CHECK <ls_line> IS ASSIGNED.

        ASSIGN COMPONENT c_fieldname_selected OF STRUCTURE <ls_line> TO <lv_selected>.
        CHECK <lv_selected> IS ASSIGNED.

        <lv_selected> = abap_true.

      ENDLOOP.

    ENDIF.

    lv_condition = |{ c_fieldname_selected } = ABAP_TRUE|.

    CREATE DATA lr_exporting LIKE LINE OF et_list.
    ASSIGN lr_exporting->* TO <lg_exporting>.

    lo_data_descr = mo_table_descr->get_table_line_type( ).

    LOOP AT <lt_table> ASSIGNING <ls_line> WHERE (lv_condition).
      CLEAR <lg_exporting>.

      CASE lo_data_descr->kind.
        WHEN cl_abap_elemdescr=>kind_elem.
          ASSIGN COMPONENT c_default_column OF STRUCTURE <ls_line> TO <lg_value>.
          ASSERT <lg_value> IS ASSIGNED.
          <lg_exporting> = <lg_value>.

        WHEN OTHERS.
          MOVE-CORRESPONDING <ls_line> TO <lg_exporting>.

      ENDCASE.
      APPEND <lg_exporting> TO et_list.
    ENDLOOP.

  ENDMETHOD.

  METHOD create_alv.

    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    cl_salv_table=>factory(
      IMPORTING
        r_salv_table = ro_alv
      CHANGING
        t_table      = <lt_table> ).

  ENDMETHOD.

  METHOD constructor.

    DATA:
      lv_object_list  TYPE abap_bool,
      lo_events       TYPE REF TO cl_salv_events_table,
      lo_columns      TYPE REF TO cl_salv_columns_table,
      lo_table_header TYPE REF TO cl_salv_form_text.

    create_new_selectable_table( it_list ).
    preselect( it_preselected_rows ).

    TRY.
        mo_alv = create_alv( ).
        mo_alv->set_screen_popup(
          start_column = is_position-start_column
          end_column   = is_position-end_column
          start_line   = is_position-start_row
          end_line     = is_position-end_row ).
        ms_position = is_position.

        lo_events = mo_alv->get_event( ).

        SET HANDLER on_select_list_link_click FOR lo_events.
        SET HANDLER on_select_list_function_click FOR lo_events.
        SET HANDLER on_double_click FOR lo_events.

        IF iv_title CN ' _0'.
          mo_alv->get_display_settings( )->set_list_header( iv_title ).
        ENDIF.

        IF iv_header_text CN ' _0'.
          CREATE OBJECT lo_table_header EXPORTING text = iv_header_text.
          mo_alv->set_top_of_list( lo_table_header ).
        ENDIF.

        mo_alv->get_display_settings( )->set_striped_pattern( iv_striped_pattern ).
        mo_alv->get_selections( )->set_selection_mode( iv_selection_mode ).

        lo_columns = mo_alv->get_columns( ).
        lo_columns->set_optimize( iv_optimize_col_width ).

        TRY.
            lo_columns->get_column( |{ c_fieldname_obj_type }| ).
            lv_object_list = abap_true.
          CATCH cx_salv_not_found.
        ENDTRY.

        setup_columns(
          io_columns            = lo_columns
          iv_selection_mode     = iv_selection_mode
          iv_select_column_text = iv_select_column_text
          it_columns_to_display = it_columns_to_display ).

        setup_toolbar(
          iv_object_list    = lv_object_list
          iv_selection_mode = iv_selection_mode ).

      CATCH cx_salv_msg.
        zcx_abapgit_exception=>raise( 'ALV error from object decision list' ).
    ENDTRY.


  ENDMETHOD.

  METHOD create_new_selectable_table.

    " create and populate a table on the fly derived from
    " it_data with a select column

    DATA:
      lr_struct        TYPE REF TO data,
      lt_components    TYPE cl_abap_structdescr=>component_table,
      lo_data_descr    TYPE REF TO cl_abap_datadescr,
      lo_elem_descr    TYPE REF TO cl_abap_elemdescr,
      lo_struct_descr  TYPE REF TO cl_abap_structdescr,
      lo_struct_descr2 TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS:
      <lt_table>     TYPE STANDARD TABLE,
      <ls_component> TYPE abap_componentdescr,
      <ls_line>      TYPE data,
      <lg_data>      TYPE any,
      <lg_value>     TYPE any.

    mo_table_descr ?= cl_abap_tabledescr=>describe_by_data( it_list ).
    lo_data_descr = mo_table_descr->get_table_line_type( ).

    CASE lo_data_descr->kind.
      WHEN cl_abap_elemdescr=>kind_elem.
        lo_elem_descr ?= mo_table_descr->get_table_line_type( ).
        INSERT INITIAL LINE INTO lt_components ASSIGNING <ls_component> INDEX 1.
        <ls_component>-name = c_default_column.
        <ls_component>-type = lo_elem_descr.

      WHEN cl_abap_elemdescr=>kind_struct.
        lo_struct_descr ?= mo_table_descr->get_table_line_type( ).
        lt_components = lo_struct_descr->get_components( ).

    ENDCASE.

    IF lt_components IS INITIAL.
      RETURN.
    ENDIF.

    INSERT INITIAL LINE INTO lt_components ASSIGNING <ls_component> INDEX 1.
    <ls_component>-name = c_fieldname_selected.
    <ls_component>-type ?= cl_abap_datadescr=>describe_by_name( 'FLAG' ).

    lo_struct_descr2 = cl_abap_structdescr=>create( lt_components ).
    mo_table_descr = cl_abap_tabledescr=>create( lo_struct_descr2 ).

    CREATE DATA mr_table TYPE HANDLE mo_table_descr.
    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    CREATE DATA lr_struct TYPE HANDLE lo_struct_descr2.
    ASSIGN lr_struct->* TO <ls_line>.
    ASSERT sy-subrc = 0.

    LOOP AT it_list ASSIGNING <lg_data>.
      CLEAR <ls_line>.
      CASE lo_data_descr->kind.
        WHEN cl_abap_elemdescr=>kind_elem.
          ASSIGN COMPONENT c_default_column OF STRUCTURE <lg_data> TO <lg_value>.
          ASSERT <lg_value> IS ASSIGNED.
          <ls_line> = <lg_value>.

        WHEN OTHERS.
          MOVE-CORRESPONDING <lg_data> TO <ls_line>.

      ENDCASE.
      INSERT <ls_line> INTO TABLE <lt_table>.
    ENDLOOP.

  ENDMETHOD.

  METHOD preselect.

    FIELD-SYMBOLS:
      <lt_table>    TYPE STANDARD TABLE,
      <lv_row>      LIKE LINE OF it_preselected_rows,
      <ls_line>     TYPE any,
      <lv_selected> TYPE data.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    LOOP AT it_preselected_rows ASSIGNING <lv_row>.

      READ TABLE <lt_table> INDEX <lv_row> ASSIGNING <ls_line>.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Preselected row { <lv_row> } doesn't exist| ).
      ENDIF.

      ASSIGN COMPONENT c_fieldname_selected OF STRUCTURE <ls_line> TO <lv_selected>.
      ASSERT sy-subrc = 0.
      <lv_selected> = abap_true.

    ENDLOOP.

  ENDMETHOD.

  METHOD on_double_click.

    DATA lo_selections TYPE REF TO cl_salv_selections.

    lo_selections = mo_alv->get_selections( ).

    IF lo_selections->get_selection_mode( ) = if_salv_c_selection_mode=>single.
      mo_alv->close_screen( ).
    ENDIF.

  ENDMETHOD.

  METHOD on_select_list_function_click.

    " Work for functions of SAPMSVIM and OWN
    CASE e_salv_function.
      WHEN 'O.K.' OR 'OK'.
        mv_cancel = abap_false.
        mo_alv->close_screen( ).

      WHEN 'ABR' OR 'CANCEL'.
        " Canceled: clear list to overwrite nothing
        mv_cancel = abap_true.
        mo_alv->close_screen( ).

      WHEN 'SALL' OR 'SEL_ALL'.
        mark_visible( abap_true ).
        mo_alv->refresh( ).

      WHEN 'DSEL' OR 'SEL_DEL'.
        mark_visible( abap_false ).
        mo_alv->refresh( ).

      WHEN 'SEL_KEY'.
        mark_selected( ).
        mo_alv->refresh( ).

      WHEN 'SEL_CAT'.
        mark_category( ask_user_for_obj_category( ) ).
        mo_alv->refresh( ).

      WHEN OTHERS.
        mv_cancel = abap_true.
        mo_alv->close_screen( ).

    ENDCASE.

  ENDMETHOD.

  METHOD mark_all.

    FIELD-SYMBOLS:
      <lt_table>    TYPE STANDARD TABLE,
      <ls_line>     TYPE any,
      <lv_selected> TYPE abap_bool.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    LOOP AT <lt_table> ASSIGNING <ls_line>.

      ASSIGN COMPONENT c_fieldname_selected OF STRUCTURE <ls_line> TO <lv_selected>.
      ASSERT sy-subrc = 0.
      <lv_selected> = iv_selected.

    ENDLOOP.

  ENDMETHOD.

  METHOD are_all_marked.

    DATA lv_index LIKE LINE OF it_scope.

    FIELD-SYMBOLS:
      <lt_table>    TYPE STANDARD TABLE,
      <ls_line>     TYPE any,
      <lv_selected> TYPE abap_bool.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    LOOP AT it_scope INTO lv_index.

      READ TABLE <lt_table> ASSIGNING <ls_line> INDEX lv_index.
      CHECK sy-subrc = 0.

      ASSIGN COMPONENT c_fieldname_selected OF STRUCTURE <ls_line> TO <lv_selected>.
      ASSERT sy-subrc = 0.

      IF <lv_selected> = abap_true.
        rv_yes = abap_true.
      ELSE.
        rv_yes = abap_false.
        RETURN.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD mark_selected.

    DATA lt_clear TYPE salv_t_row.
    DATA lt_scope TYPE lvc_t_fidx.

    lt_scope = mo_alv->get_selections( )->get_selected_rows( ).

    IF lines( lt_scope ) > 0.
      mark_indexed(
        it_scope    = lt_scope
        iv_selected = boolc( are_all_marked( lt_scope ) = abap_false ) ).
      mo_alv->get_selections( )->set_selected_rows( lt_clear ).
    ELSE.
      MESSAGE 'Select rows first to mark them' TYPE 'S'.
    ENDIF.

  ENDMETHOD.

  METHOD mark_visible.

    DATA lt_filters TYPE lvc_t_filt.
    DATA lt_scope   TYPE lvc_t_fidx.

    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    " If nothing selected, select all VISIBLE
    lt_filters = cl_salv_controller_metadata=>get_lvc_filter( mo_alv->get_filters( ) ).
    IF lines( lt_filters ) = 0.
      mark_all( iv_selected ). " No filters - just select all
      RETURN.
    ENDIF.

    CALL FUNCTION 'LVC_FILTER_APPLY'
      EXPORTING
        it_filter              = lt_filters
      IMPORTING
        et_filter_index_inside = lt_scope
      TABLES
        it_data                = <lt_table>.

    mark_indexed(
      it_scope    = lt_scope
      iv_selected = iv_selected ).

  ENDMETHOD.

  METHOD mark_indexed.

    DATA lv_index LIKE LINE OF it_scope.

    FIELD-SYMBOLS:
      <lt_table>    TYPE STANDARD TABLE,
      <ls_line>     TYPE any,
      <lv_selected> TYPE abap_bool.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    LOOP AT it_scope INTO lv_index.

      READ TABLE <lt_table> ASSIGNING <ls_line> INDEX lv_index.
      CHECK sy-subrc = 0.

      ASSIGN COMPONENT c_fieldname_selected OF STRUCTURE <ls_line> TO <lv_selected>.
      ASSERT sy-subrc = 0.

      IF iv_invert = abap_true.
        <lv_selected> = boolc( <lv_selected> = abap_false ).
      ELSE.
        <lv_selected> = iv_selected.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD ask_user_for_obj_category.

    DATA:
      lv_answer    TYPE c LENGTH 1,
      ls_position  TYPE zif_abapgit_popups=>ty_popup_position,
      ls_selection TYPE spopli,
      lt_selection TYPE TABLE OF spopli.

    ls_selection-varoption = 'Packages'.
    APPEND ls_selection TO lt_selection.
    ls_selection-varoption = 'DDIC objects'.
    APPEND ls_selection TO lt_selection.
    ls_selection-varoption = 'Source code'.
    APPEND ls_selection TO lt_selection.
    ls_selection-varoption = 'Enhancements'.
    APPEND ls_selection TO lt_selection.

    ls_position-start_column = ms_position-start_column + 20.
    ls_position-start_row    = ms_position-start_row + 5.

    CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
      EXPORTING
        titel      = 'Selection'
        textline1  = 'Which objects should be added to the selection?'
        start_col  = ls_position-start_column
        start_row  = ls_position-start_row
        cursorline = 1
      IMPORTING
        answer     = lv_answer
      TABLES
        t_spopli   = lt_selection
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0 OR lv_answer = c_answer_cancel.
      RETURN.
    ENDIF.

    READ TABLE lt_selection INDEX lv_answer INTO ls_selection.
    IF sy-subrc = 0.
      rv_category = ls_selection-varoption.
    ENDIF.

  ENDMETHOD.

  METHOD mark_category.

    FIELD-SYMBOLS:
      <lt_table>    TYPE STANDARD TABLE,
      <lv_obj_type> TYPE tadir-object,
      <ls_line>     TYPE any,
      <lv_selected> TYPE abap_bool.

    IF iv_category IS INITIAL.
      RETURN.
    ENDIF.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    LOOP AT <lt_table> ASSIGNING <ls_line>.

      ASSIGN COMPONENT c_fieldname_obj_type OF STRUCTURE <ls_line> TO <lv_obj_type>.
      ASSERT sy-subrc = 0.

      ASSIGN COMPONENT c_fieldname_selected OF STRUCTURE <ls_line> TO <lv_selected>.
      ASSERT sy-subrc = 0.

      CASE iv_category.
        WHEN 'Packages'.
          IF <lv_obj_type> <> 'DEVC'.
            CONTINUE.
          ENDIF.
        WHEN 'DDIC objects'.
          IF zcl_abapgit_objects_activation=>is_ddic_type( <lv_obj_type> ) = abap_false.
            CONTINUE.
          ENDIF.
        WHEN 'Source code'.
          IF 'CLAS,FUGR,INTF,PROG,TYPE' NS <lv_obj_type>.
            CONTINUE.
          ENDIF.
        WHEN 'Enhancements'.
          IF 'ENHO,ENHS,ENHC,ENSC' NS <lv_obj_type>.
            CONTINUE.
          ENDIF.
        WHEN OTHERS.
          RETURN. " Unexpected category
      ENDCASE.

      <lv_selected> = abap_true.

    ENDLOOP.

  ENDMETHOD.

  METHOD on_select_list_link_click.

    FIELD-SYMBOLS:
      <lt_table>    TYPE STANDARD TABLE,
      <ls_line>     TYPE any,
      <lv_selected> TYPE abap_bool.

    ASSIGN mr_table->* TO <lt_table>.
    ASSERT sy-subrc = 0.

    READ TABLE <lt_table> ASSIGNING <ls_line> INDEX row.
    IF sy-subrc = 0.

      ASSIGN COMPONENT c_fieldname_selected OF STRUCTURE <ls_line> TO <lv_selected>.
      ASSERT sy-subrc = 0.
      <lv_selected> = boolc( <lv_selected> = abap_false ).

    ENDIF.

    mo_alv->refresh( ).

  ENDMETHOD.

  METHOD setup_columns.

    DATA:
      lt_columns TYPE salv_t_column_ref,
      ls_column  TYPE salv_s_column_ref,
      lo_column  TYPE REF TO cl_salv_column_list.

    FIELD-SYMBOLS <ls_column_to_display> TYPE zif_abapgit_popups=>ty_alv_column.

    lt_columns = io_columns->get( ).

    LOOP AT lt_columns INTO ls_column.

      lo_column ?= ls_column-r_column.

      IF    iv_selection_mode    = if_salv_c_selection_mode=>multiple
        AND ls_column-columnname = c_fieldname_selected.
        lo_column->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ).
        lo_column->set_output_length( 20 ).
        lo_column->set_short_text( |{ iv_select_column_text }| ).
        lo_column->set_medium_text( |{ iv_select_column_text }| ).
        lo_column->set_long_text( |{ iv_select_column_text }| ).
        CONTINUE.
      ENDIF.

      READ TABLE it_columns_to_display
        ASSIGNING <ls_column_to_display>
        WITH KEY name = ls_column-columnname.

      CASE sy-subrc.
        WHEN 0.
          IF <ls_column_to_display>-text CN ' _0'.
            lo_column->set_short_text( |{ <ls_column_to_display>-text }| ).
            lo_column->set_medium_text( |{ <ls_column_to_display>-text }| ).
            lo_column->set_long_text( |{ <ls_column_to_display>-text }| ).
          ENDIF.

          IF <ls_column_to_display>-length > 0.
            lo_column->set_output_length( <ls_column_to_display>-length ).
          ENDIF.

          IF <ls_column_to_display>-show_icon = abap_true.
            lo_column->set_icon( abap_true ).
          ENDIF.

          IF <ls_column_to_display>-center = abap_true.
            lo_column->set_alignment( if_salv_c_alignment=>centered ).
          ENDIF.

        WHEN OTHERS.
          lo_column->set_technical( abap_true ). " Hide column

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD setup_toolbar.

    DATA:
      lv_report    TYPE sy-repid,
      lv_pfstatus  TYPE sy-pfkey,
      lo_functions TYPE REF TO cl_salv_functions_list,
      lt_func_list TYPE salv_t_ui_func,
      lv_fn        TYPE string,
      ls_func      LIKE LINE OF lt_func_list.

    CALL FUNCTION 'RS_CUA_STATUS_CHECK'
      EXPORTING
        program          = sy-cprog
        objectname       = c_own_pfstatus
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.

    IF sy-subrc = 0.

      mo_alv->set_screen_status(
        report   = sy-cprog
        pfstatus = c_own_pfstatus ).

    ELSE.

      lv_report  = 'SAPMSVIM'.

      IF iv_selection_mode = if_salv_c_selection_mode=>single.
        lv_pfstatus = '110'.
      ELSE.
        lv_pfstatus = '102'.
      ENDIF.

      mo_alv->set_screen_status(
        report   = lv_report
        pfstatus = lv_pfstatus ).

    ENDIF.

    lo_functions = mo_alv->get_functions( ).

    lt_func_list = lo_functions->get_functions( ).
    LOOP AT lt_func_list INTO ls_func.
      lv_fn = ls_func-r_function->get_name( ).
      IF lv_fn = 'OK' OR lv_fn = 'CANCEL'.
        ls_func-r_function->set_visible( abap_true ).
      ELSEIF iv_object_list = abap_true.
        ls_func-r_function->set_visible( abap_true ).
      ELSE.
        ls_func-r_function->set_visible( abap_false ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
