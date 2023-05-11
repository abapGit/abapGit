CLASS zcl_abapgit_html_form DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_abapgit_gui_component
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_html_form .
    INTERFACES zif_abapgit_gui_hotkeys .

    CLASS-METHODS create
      IMPORTING
        !iv_form_id    TYPE csequence OPTIONAL
        !iv_help_page  TYPE csequence OPTIONAL
      RETURNING
        VALUE(ro_form) TYPE REF TO zcl_abapgit_html_form .
    METHODS render
      IMPORTING
        !iv_form_class     TYPE csequence DEFAULT 'dialog-form'
        !io_values         TYPE REF TO zcl_abapgit_string_map
        !io_validation_log TYPE REF TO zcl_abapgit_string_map OPTIONAL
      RETURNING
        VALUE(ri_html)     TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS command
      IMPORTING
        !iv_label      TYPE csequence
        !iv_action     TYPE csequence
        !iv_cmd_type   TYPE i DEFAULT zif_abapgit_html_form=>c_cmd_type-input
      RETURNING
        VALUE(ro_self) TYPE REF TO zcl_abapgit_html_form .
    METHODS text
      IMPORTING
        !iv_label       TYPE csequence
        !iv_name        TYPE csequence
        !iv_hint        TYPE csequence OPTIONAL
        !iv_required    TYPE abap_bool DEFAULT abap_false
        !iv_upper_case  TYPE abap_bool DEFAULT abap_false
        !iv_readonly    TYPE abap_bool DEFAULT abap_false
        !iv_password    TYPE abap_bool DEFAULT abap_false
        !iv_condense    TYPE abap_bool OPTIONAL
        !iv_placeholder TYPE csequence OPTIONAL
        !iv_side_action TYPE csequence OPTIONAL
        !iv_min         TYPE i DEFAULT cl_abap_math=>min_int4
        !iv_max         TYPE i DEFAULT cl_abap_math=>max_int4
      RETURNING
        VALUE(ro_self)  TYPE REF TO zcl_abapgit_html_form .
    METHODS textarea
      IMPORTING
        !iv_label       TYPE csequence
        !iv_name        TYPE csequence
        !iv_hint        TYPE csequence OPTIONAL
        !iv_required    TYPE abap_bool DEFAULT abap_false
        !iv_readonly    TYPE abap_bool DEFAULT abap_false
        !iv_placeholder TYPE csequence OPTIONAL
        !iv_rows        TYPE i OPTIONAL
        !iv_cols        TYPE i OPTIONAL
        !iv_upper_case  TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ro_self)  TYPE REF TO zcl_abapgit_html_form .
    METHODS number
      IMPORTING
        !iv_label      TYPE csequence
        !iv_name       TYPE csequence
        !iv_hint       TYPE csequence OPTIONAL
        !iv_required   TYPE abap_bool DEFAULT abap_false
        !iv_readonly   TYPE abap_bool DEFAULT abap_false
        !iv_min        TYPE i DEFAULT cl_abap_math=>min_int4
        !iv_max        TYPE i DEFAULT cl_abap_math=>max_int4
      RETURNING
        VALUE(ro_self) TYPE REF TO zcl_abapgit_html_form .
    METHODS checkbox
      IMPORTING
        !iv_label      TYPE csequence
        !iv_name       TYPE csequence
        !iv_hint       TYPE csequence OPTIONAL
      RETURNING
        VALUE(ro_self) TYPE REF TO zcl_abapgit_html_form .
    METHODS radio
      IMPORTING
        !iv_label         TYPE csequence
        !iv_name          TYPE csequence
        !iv_default_value TYPE csequence OPTIONAL
        !iv_hint          TYPE csequence OPTIONAL
        !iv_condense      TYPE abap_bool DEFAULT abap_false
        !iv_action        TYPE csequence OPTIONAL
      RETURNING
        VALUE(ro_self)    TYPE REF TO zcl_abapgit_html_form .
    METHODS option
      IMPORTING
        !iv_label      TYPE csequence
        !iv_value      TYPE csequence
      RETURNING
        VALUE(ro_self) TYPE REF TO zcl_abapgit_html_form .
    METHODS table
      IMPORTING
        !iv_label      TYPE csequence
        !iv_name       TYPE csequence
        !iv_hint       TYPE csequence OPTIONAL
      RETURNING
        VALUE(ro_self) TYPE REF TO zcl_abapgit_html_form .
    METHODS column
      IMPORTING
        !iv_label      TYPE csequence
        !iv_width      TYPE csequence OPTIONAL
        !iv_readonly   TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ro_self) TYPE REF TO zcl_abapgit_html_form .
    METHODS start_group
      IMPORTING
        !iv_label      TYPE csequence
        !iv_name       TYPE csequence
        !iv_hint       TYPE csequence OPTIONAL
      RETURNING
        VALUE(ro_self) TYPE REF TO zcl_abapgit_html_form .
    METHODS hidden
      IMPORTING
        !iv_name       TYPE csequence
      RETURNING
        VALUE(ro_self) TYPE REF TO zcl_abapgit_html_form .
    METHODS get_fields
      RETURNING
        VALUE(rt_fields) TYPE zif_abapgit_html_form=>ty_fields .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_attr,
        value       TYPE string,
        error       TYPE string,
        hint        TYPE string,
        readonly    TYPE string,
        placeholder TYPE string,
        required    TYPE string,
        autofocus   TYPE string,
      END OF ty_attr .

    DATA mt_fields TYPE zif_abapgit_html_form=>ty_fields .
    DATA:
      mt_commands TYPE STANDARD TABLE OF zif_abapgit_html_form=>ty_command .
    DATA mv_form_id TYPE string .
    DATA mv_help_page TYPE string .

    METHODS render_field
      IMPORTING
        !ii_html           TYPE REF TO zif_abapgit_html
        !io_values         TYPE REF TO zcl_abapgit_string_map
        !io_validation_log TYPE REF TO zcl_abapgit_string_map
        !is_field          TYPE zif_abapgit_html_form=>ty_field
        !iv_autofocus      TYPE abap_bool .
    METHODS render_field_text
      IMPORTING
        !ii_html  TYPE REF TO zif_abapgit_html
        !is_field TYPE zif_abapgit_html_form=>ty_field
        !is_attr  TYPE ty_attr .
    METHODS render_field_textarea
      IMPORTING
        !ii_html  TYPE REF TO zif_abapgit_html
        !is_field TYPE zif_abapgit_html_form=>ty_field
        !is_attr  TYPE ty_attr .
    METHODS render_field_checkbox
      IMPORTING
        !ii_html  TYPE REF TO zif_abapgit_html
        !is_field TYPE zif_abapgit_html_form=>ty_field
        !is_attr  TYPE ty_attr .
    METHODS render_field_radio
      IMPORTING
        !ii_html  TYPE REF TO zif_abapgit_html
        !is_field TYPE zif_abapgit_html_form=>ty_field
        !is_attr  TYPE ty_attr .
    METHODS render_field_table
      IMPORTING
        !ii_html   TYPE REF TO zif_abapgit_html
        !is_field  TYPE zif_abapgit_html_form=>ty_field
        !is_attr   TYPE ty_attr
        !io_values TYPE REF TO zcl_abapgit_string_map .
    METHODS render_command
      IMPORTING
        !ii_html TYPE REF TO zif_abapgit_html
        !is_cmd  TYPE zif_abapgit_html_form=>ty_command .
    METHODS render_field_hidden
      IMPORTING
        !ii_html  TYPE REF TO zif_abapgit_html
        !is_field TYPE zif_abapgit_html_form=>ty_field
        !is_attr  TYPE ty_attr .
ENDCLASS.



CLASS ZCL_ABAPGIT_HTML_FORM IMPLEMENTATION.


  METHOD checkbox.

    DATA ls_field LIKE LINE OF mt_fields.

    ls_field-type  = zif_abapgit_html_form=>c_field_type-checkbox.
    ls_field-name  = iv_name.
    ls_field-label = iv_label.
    ls_field-hint  = iv_hint.

    APPEND ls_field TO mt_fields.

    ro_self = me.

  ENDMETHOD.


  METHOD column.

    FIELD-SYMBOLS <ls_last> LIKE LINE OF mt_fields.
    DATA ls_column LIKE LINE OF <ls_last>-subitems.
    DATA lv_size TYPE i.

    lv_size = lines( mt_fields ).
    ASSERT lv_size > 0. " Exception ? Maybe add zcx_no_check ?

    READ TABLE mt_fields INDEX lv_size ASSIGNING <ls_last>.
    ASSERT sy-subrc = 0.
    ASSERT <ls_last>-type = zif_abapgit_html_form=>c_field_type-table.

    ls_column-label    = iv_label.
    ls_column-value    = iv_width.
    ls_column-readonly = iv_readonly.

    APPEND ls_column TO <ls_last>-subitems.

    ro_self = me.

  ENDMETHOD.


  METHOD command.

    DATA ls_cmd LIKE LINE OF mt_commands.

    ASSERT iv_cmd_type BETWEEN 1 AND 4.

    ls_cmd-label    = iv_label.
    ls_cmd-action   = iv_action.
    ls_cmd-cmd_type = iv_cmd_type.

    APPEND ls_cmd TO mt_commands.

    ro_self = me.

  ENDMETHOD.


  METHOD create.

    DATA lv_ts TYPE timestampl.

    CREATE OBJECT ro_form.
    ro_form->mv_form_id = iv_form_id.
    ro_form->mv_help_page = iv_help_page.

    IF ro_form->mv_form_id IS INITIAL.
      GET TIME STAMP FIELD lv_ts.
      ro_form->mv_form_id = |form_{ lv_ts }|.
    ENDIF.

  ENDMETHOD.


  METHOD get_fields.
    rt_fields = mt_fields.
  ENDMETHOD.


  METHOD hidden.

    DATA ls_field LIKE LINE OF mt_fields.

    ls_field-type  = zif_abapgit_html_form=>c_field_type-hidden.
    ls_field-name  = iv_name.
    APPEND ls_field TO mt_fields.

    ro_self = me.

  ENDMETHOD.


  METHOD number.

    DATA ls_field LIKE LINE OF mt_fields.

    ls_field-type     = zif_abapgit_html_form=>c_field_type-number.
    ls_field-name     = iv_name.
    ls_field-label    = iv_label.
    ls_field-readonly = iv_readonly.
    ls_field-min      = iv_min.
    ls_field-max      = iv_max.
    ls_field-hint     = iv_hint.
    ls_field-required = iv_required.

    APPEND ls_field TO mt_fields.

    ro_self = me.

  ENDMETHOD.


  METHOD option.

    FIELD-SYMBOLS <ls_last> LIKE LINE OF mt_fields.
    DATA ls_option LIKE LINE OF <ls_last>-subitems.
    DATA lv_size TYPE i.

    lv_size = lines( mt_fields ).
    ASSERT lv_size > 0. " Exception ? Maybe add zcx_no_check ?

    READ TABLE mt_fields INDEX lv_size ASSIGNING <ls_last>.
    ASSERT sy-subrc = 0.
    ASSERT <ls_last>-type = zif_abapgit_html_form=>c_field_type-radio. " Or dropdown - TODO in future

    ls_option-label = iv_label.
    ls_option-value = iv_value.

    APPEND ls_option TO <ls_last>-subitems.

    ro_self = me.

  ENDMETHOD.


  METHOD radio.

    DATA ls_field LIKE LINE OF mt_fields.

    ls_field-type  = zif_abapgit_html_form=>c_field_type-radio.
    ls_field-name  = iv_name.
    ls_field-label = iv_label.
    ls_field-default_value = iv_default_value.
    ls_field-hint  = iv_hint.
    ls_field-click = iv_action.

    " put options into one column instead of side-by-side
    ls_field-condense = iv_condense.

    APPEND ls_field TO mt_fields.

    ro_self = me.

  ENDMETHOD.


  METHOD render.

    FIELD-SYMBOLS <ls_field> LIKE LINE OF mt_fields.
    FIELD-SYMBOLS <ls_cmd> LIKE LINE OF mt_commands.
    DATA lv_hint TYPE string.
    DATA ls_form_id TYPE string.
    DATA ls_form_action TYPE string.
    DATA lv_cur_group TYPE string.
    DATA lv_url TYPE string.
    DATA lv_autofocus TYPE abap_bool.

    IF mv_form_id IS NOT INITIAL.
      ls_form_id = | id="{ mv_form_id }"|.
    ENDIF.
    LOOP AT mt_commands ASSIGNING <ls_cmd> WHERE cmd_type = zif_abapgit_html_form=>c_cmd_type-input_main.
      ls_form_action = | action="sapevent:{ <ls_cmd>-action }"|.
      EXIT.
    ENDLOOP.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( |<div class="dialog { iv_form_class }">| ). " to center use 'dialog-form-center'
    ri_html->add( |<form method="post"{ ls_form_id }{ ls_form_action }>| ).

    " Add hidden button that triggers main command when pressing enter
    LOOP AT mt_commands ASSIGNING <ls_cmd> WHERE cmd_type = zif_abapgit_html_form=>c_cmd_type-input_main.
      ri_html->add( |<button type="submit" formaction="sapevent:{ <ls_cmd>-action }" class="hidden-submit"|
                 && | aria-hidden="true" tabindex="-1"></button>| ).
      EXIT.
    ENDLOOP.

    lv_autofocus = abap_true.
    LOOP AT mt_fields ASSIGNING <ls_field>.
      AT FIRST.
        IF <ls_field>-type <> zif_abapgit_html_form=>c_field_type-field_group.
          ri_html->add( |<ul>| ).
        ENDIF.
      ENDAT.

      IF <ls_field>-type = zif_abapgit_html_form=>c_field_type-field_group.
        IF lv_cur_group IS NOT INITIAL AND lv_cur_group <> <ls_field>-name.
          ri_html->add( |</ul>| ).
          ri_html->add( |</fieldset>| ).
        ENDIF.
        IF <ls_field>-hint IS NOT INITIAL.
          lv_hint = | title="{ <ls_field>-hint }"|.
        ELSE.
          lv_hint = ''.
        ENDIF.
        lv_cur_group = <ls_field>-name.
        ri_html->add( |<fieldset name="{ <ls_field>-name }">| ).
        ri_html->add( |<legend{ lv_hint }>{ <ls_field>-label }</legend>| ).
        ri_html->add( |<ul>| ).
        CONTINUE.
      ENDIF.

      render_field(
        ii_html           = ri_html
        io_values         = io_values
        io_validation_log = io_validation_log
        is_field          = <ls_field>
        iv_autofocus      = lv_autofocus ).

      lv_autofocus = abap_false.

      AT LAST.
        ri_html->add( |</ul>| ).
        IF lv_cur_group IS NOT INITIAL.
          ri_html->add( |</fieldset>| ).
        ENDIF.
      ENDAT.
    ENDLOOP.

    ri_html->add( |<ul>| ).
    ri_html->add( |<li class="dialog-commands">| ).

    IF mv_help_page IS NOT INITIAL.
      lv_url = escape( val    = mv_help_page
                       format = cl_abap_format=>e_url ).
      ri_html->add_a(
        iv_txt   = zcl_abapgit_gui_buttons=>help( )
        iv_act   = |{ zif_abapgit_definitions=>c_action-url }?url={ lv_url }|
        iv_class = 'dialog-help'
        iv_title = 'Help' ).
    ENDIF.

    LOOP AT mt_commands ASSIGNING <ls_cmd>.
      render_command(
        ii_html = ri_html
        is_cmd  = <ls_cmd> ).
    ENDLOOP.

    ri_html->add( |</li>| ).
    ri_html->add( |</ul>| ).
    ri_html->add( |</form>| ).
    ri_html->add( |</div>| ).

    register_handlers( ).

  ENDMETHOD.


  METHOD render_command.

    CASE is_cmd-cmd_type.
      WHEN zif_abapgit_html_form=>c_cmd_type-link.

        ii_html->add_a(
          iv_txt   = is_cmd-label
          iv_act   = is_cmd-action
          iv_class = 'dialog-commands' ).

      WHEN zif_abapgit_html_form=>c_cmd_type-button.

        ii_html->add( |<button type="submit" name="action" value="{ is_cmd-action }"|
                   && | class="action-commands">{ is_cmd-label }</button>| ).

      WHEN zif_abapgit_html_form=>c_cmd_type-input.

        ii_html->add( |<input type="submit" value="{ is_cmd-label }" formaction="sapevent:{ is_cmd-action }">| ).

      WHEN zif_abapgit_html_form=>c_cmd_type-input_main.

        ii_html->add( |<input type="submit" value="{ is_cmd-label }" class="main">| ).

      WHEN OTHERS.
        ASSERT 0 = 1.

    ENDCASE.

  ENDMETHOD.


  METHOD render_field.

    DATA:
      ls_attr       TYPE ty_attr,
      lv_item_class TYPE string.

    " Get value and validation error
    ls_attr-value = io_values->get( is_field-name ).

    IF is_field-type <> zif_abapgit_html_form=>c_field_type-textarea.
      ls_attr-value = escape( val    = ls_attr-value
                              format = cl_abap_format=>e_html_attr ).
    ENDIF.

    IF io_validation_log IS BOUND.
      ls_attr-error = io_validation_log->get( is_field-name ).
      IF ls_attr-error IS NOT INITIAL.
        ls_attr-error = escape( val    = ls_attr-error
                                format = cl_abap_format=>e_html_text ).
        ls_attr-error = |<small>{ ls_attr-error }</small>|.
      ENDIF.
    ENDIF.

    " Prepare field attributes
    IF is_field-required = abap_true.
      ls_attr-required = ' <em>*</em>'.
    ENDIF.

    IF is_field-hint IS NOT INITIAL.
      ls_attr-hint = escape( val    = is_field-hint
                             format = cl_abap_format=>e_html_attr ).
      ls_attr-hint = | title="{ ls_attr-hint }"|.
    ENDIF.

    IF is_field-placeholder IS NOT INITIAL.
      ls_attr-placeholder = escape( val    = is_field-placeholder
                                    format = cl_abap_format=>e_html_attr ).
      ls_attr-placeholder = | placeholder="{ ls_attr-placeholder }"|.
    ENDIF.

    IF is_field-readonly = abap_true.
      ls_attr-readonly = ' readonly'.
    ENDIF.

    IF iv_autofocus = abap_true.
      ls_attr-autofocus = ' autofocus'.
    ENDIF.

    " Prepare item class
    lv_item_class = is_field-item_class.
    IF ls_attr-error IS NOT INITIAL.
      lv_item_class = condense( lv_item_class && ' error' ).
    ENDIF.
    IF is_field-type = zif_abapgit_html_form=>c_field_type-text AND is_field-max BETWEEN 1 AND 20.
      " Reduced width for short fields
      lv_item_class = lv_item_class && ' w40'.
    ENDIF.
    IF is_field-type = zif_abapgit_html_form=>c_field_type-hidden.
      lv_item_class = lv_item_class && ' hidden'.
    ENDIF.
    IF lv_item_class IS NOT INITIAL.
      lv_item_class = | class="{ lv_item_class }"|.
    ENDIF.

    " Render field
    ii_html->add( |<li{ lv_item_class }>| ).

    CASE is_field-type.
      WHEN zif_abapgit_html_form=>c_field_type-text OR zif_abapgit_html_form=>c_field_type-number.

        render_field_text(
          ii_html  = ii_html
          is_field = is_field
          is_attr  = ls_attr ).

      WHEN zif_abapgit_html_form=>c_field_type-textarea.

        render_field_textarea(
          ii_html  = ii_html
          is_field = is_field
          is_attr  = ls_attr ).

      WHEN zif_abapgit_html_form=>c_field_type-checkbox.

        render_field_checkbox(
          ii_html  = ii_html
          is_field = is_field
          is_attr  = ls_attr ).

      WHEN zif_abapgit_html_form=>c_field_type-radio.

        render_field_radio(
          ii_html  = ii_html
          is_field = is_field
          is_attr  = ls_attr ).

      WHEN zif_abapgit_html_form=>c_field_type-table.

        render_field_table(
          ii_html   = ii_html
          is_field  = is_field
          is_attr   = ls_attr
          io_values = io_values ).

      WHEN zif_abapgit_html_form=>c_field_type-hidden.

        render_field_hidden(
          ii_html  = ii_html
          is_field = is_field
          is_attr  = ls_attr ).

      WHEN OTHERS.
        ASSERT 1 = 0.
    ENDCASE.

    ii_html->add( '</li>' ).

  ENDMETHOD.


  METHOD render_field_checkbox.

    DATA lv_checked TYPE string.

    IF is_attr-error IS NOT INITIAL.
      ii_html->add( is_attr-error ).
    ENDIF.

    IF is_attr-value = abap_true OR is_attr-value = 'on'.
      " boolc return ` ` which is not initial -> bug after 1st validation
      lv_checked = ' checked'.
    ENDIF.

    ii_html->add( |<input type="checkbox" name="{ is_field-name }" id="{ is_field-name }"| &&
                  |{ lv_checked }{ is_attr-readonly }{ is_attr-autofocus }>| ).
    ii_html->add( |<label for="{ is_field-name }"{ is_attr-hint }>{ is_field-label }</label>| ).

  ENDMETHOD.


  METHOD render_field_hidden.

    ii_html->add( |<input type="hidden" name="{ is_field-name }" id="{ is_field-name }" value="{ is_attr-value }">| ).

  ENDMETHOD.


  METHOD render_field_radio.

    DATA:
      lv_checked   TYPE string,
      lv_opt_id    TYPE string,
      lv_opt_value TYPE string,
      lv_onclick   TYPE string.

    FIELD-SYMBOLS <ls_opt> LIKE LINE OF is_field-subitems.

    ii_html->add( |<label{ is_attr-hint }>{ is_field-label }</label>| ).

    IF is_attr-error IS NOT INITIAL.
      ii_html->add( is_attr-error ).
    ENDIF.

    ii_html->add( |<div class="radio-container">| ).

    LOOP AT is_field-subitems ASSIGNING <ls_opt>.
      lv_opt_value = escape( val    = <ls_opt>-value
                             format = cl_abap_format=>e_html_attr ).

      CLEAR lv_checked.
      IF is_attr-value = lv_opt_value OR ( is_attr-value IS INITIAL AND lv_opt_value = is_field-default_value ).
        lv_checked = ' checked'.
      ENDIF.

      CLEAR lv_onclick.
      IF is_field-click IS NOT INITIAL.
        lv_onclick = |onclick="document.getElementById('{ mv_form_id }').action = 'sapevent:|
                  && |{ is_field-click }'; document.getElementById('{ mv_form_id }').submit()"|.
      ENDIF.

      lv_opt_id = |{ is_field-name }{ sy-tabix }|.
      IF is_field-condense = abap_true.
        ii_html->add( '<div>' ).
      ENDIF.
      ii_html->add( |<input type="radio" name="{ is_field-name }" id="{ lv_opt_id }"|
                 && | value="{ lv_opt_value }"{ lv_checked }{ is_attr-autofocus }|
                 && | { lv_onclick }>| ).
      ii_html->add( |<label for="{ lv_opt_id }">{ <ls_opt>-label }</label>| ).
      IF is_field-condense = abap_true.
        ii_html->add( '</div>' ).
      ENDIF.
    ENDLOOP.

    ii_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_field_table.

    DATA:
      lv_value    TYPE string,
      lv_readonly TYPE string,
      lv_rows     TYPE i,
      lv_cell_id  TYPE string.

    FIELD-SYMBOLS <ls_subitem> LIKE LINE OF is_field-subitems.

    ii_html->add( |<label for="{ is_field-name }"{ is_attr-hint }>{ is_field-label }</label>| ).

    IF is_attr-error IS NOT INITIAL.
      ii_html->add( is_attr-error ).
    ENDIF.

    lv_rows = io_values->get( |{ is_field-name }-{ zif_abapgit_html_form=>c_rows }| ).

    " Render table only if there are some data rows
    IF lv_rows > 0.

      ii_html->add( |<table name="{ is_field-name }" id="{ is_field-name }" class="table-container">| ).

      ii_html->add( |<thead>| ).
      ii_html->add( |<tr>| ).
      LOOP AT is_field-subitems ASSIGNING <ls_subitem>.
        CLEAR lv_value.
        IF <ls_subitem>-value IS NOT INITIAL.
          lv_value = escape( val    = <ls_subitem>-value
                             format = cl_abap_format=>e_html_attr ).
          lv_value = | width="{ lv_value }"|.
        ENDIF.
        ii_html->add( |<td{ lv_value }>{ <ls_subitem>-label }</td>| ).
      ENDLOOP.
      ii_html->add( |</tr>| ).
      ii_html->add( |</thead>| ).

      ii_html->add( |<tbody>| ).
      DO lv_rows TIMES.
        lv_rows = sy-index.
        ii_html->add( |<tr>| ).
        LOOP AT is_field-subitems ASSIGNING <ls_subitem>.
          lv_cell_id = |{ is_field-name }-{ lv_rows }-{ sy-tabix }|.
          lv_value = escape( val    = io_values->get( lv_cell_id )
                             format = cl_abap_format=>e_html_attr ).
          CLEAR lv_readonly.
          IF <ls_subitem>-readonly = abap_true.
            lv_readonly = | readonly|.
          ENDIF.
          ii_html->add( |<td><input type="text" name="{ lv_cell_id }" id="{
                        lv_cell_id }" value="{ lv_value }"{ lv_readonly }></td>| ).
        ENDLOOP.
        ii_html->add( |</tr>| ).
      ENDDO.
      ii_html->add( |</tbody>| ).

      ii_html->add( |</table>| ).

    ELSE.
      ii_html->add( |<input type="text" name="{ is_field-name }" id="{
                    is_field-name }" value="Not available" readonly>| ).
    ENDIF.

    " Hidden field with number of rows to simplify getting values from form
    lv_value = |{ is_field-name }-{ zif_abapgit_html_form=>c_rows }|.
    ii_html->add( |<input type="number" name="{ lv_value }" id="{ lv_value }"|
               && | value="{ lv_rows }" style="display:none">| ).

  ENDMETHOD.


  METHOD render_field_text.

    DATA:
      lv_type      TYPE string,
      lv_minlength TYPE string,
      lv_maxlength TYPE string.

    ii_html->add( |<label for="{ is_field-name }"{ is_attr-hint }>{ is_field-label }{ is_attr-required }</label>| ).

    IF is_attr-error IS NOT INITIAL.
      ii_html->add( is_attr-error ).
    ENDIF.

    IF is_field-side_action IS NOT INITIAL.
      ii_html->add( '<div class="input-container">' ). " Ugly :(
    ENDIF.

    IF is_field-type = zif_abapgit_html_form=>c_field_type-number.
      lv_type = 'number'.
    ELSEIF is_field-password = abap_true.
      lv_type = 'password'.
    ELSE.
      lv_type = 'text'.
    ENDIF.

    IF is_field-min > 0.
      lv_minlength = | minlength={ is_field-min }|.
    ENDIF.
    IF is_field-max > 0 AND is_field-max < cl_abap_math=>max_int4.
      lv_maxlength = | maxlength={ is_field-max }|.
    ENDIF.

    ii_html->add( |<input type="{ lv_type }" name="{ is_field-name }" id="{ is_field-name }"|
               && | value="{ is_attr-value }"{ is_field-dblclick }{ is_attr-placeholder }|
               && |{ is_attr-readonly }{ is_attr-autofocus }{ lv_minlength }{ lv_maxlength }>| ).

    IF is_field-side_action IS NOT INITIAL.
      ii_html->add( '</div>' ).
      ii_html->add( '<div class="command-container">' ).
      ii_html->add( |<input type="submit" value="&#x2026;" formaction="sapevent:{ is_field-side_action }"|
                 && | title="{ is_field-label }">| ).
      ii_html->add( '</div>' ).
    ENDIF.

  ENDMETHOD.


  METHOD render_field_textarea.

    DATA lv_rows TYPE string.
    DATA lv_cols TYPE string.
    DATA lv_html TYPE string.

    ii_html->add( |<label for="{ is_field-name }"{ is_attr-hint }>{ is_field-label }{ is_attr-required }</label>| ).

    IF is_attr-error IS NOT INITIAL.
      ii_html->add( is_attr-error ).
    ENDIF.

    IF is_field-rows > 0.
      lv_rows = | rows="{ is_field-rows }"|.
    ELSEIF is_attr-value IS NOT INITIAL.
      lv_rows = | rows="{ lines( zcl_abapgit_convert=>split_string( is_attr-value ) ) + 1 }"|.
    ENDIF.

    IF is_field-cols > 0.
      lv_cols = | cols="{ is_field-cols }"|.
    ENDIF.

    " Avoid adding line-breaks inside textarea tag (except for the actual value)
    lv_html = |<textarea name="{ is_field-name }" id="{ is_field-name }"{ lv_rows }{ lv_cols }|
           && |{ is_attr-readonly }{ is_attr-autofocus }{ is_attr-placeholder }>|.
    lv_html = lv_html && escape( val    = is_attr-value
                                 format = cl_abap_format=>e_html_attr ).
    lv_html = lv_html && |</textarea>|.

    ii_html->add( lv_html ).

  ENDMETHOD.


  METHOD start_group.

    DATA ls_field LIKE LINE OF mt_fields.

    ls_field-type  = zif_abapgit_html_form=>c_field_type-field_group.
    ls_field-label = iv_label.
    ls_field-name  = iv_name.
    ls_field-hint  = iv_hint.

    APPEND ls_field TO mt_fields.

    ro_self = me.

  ENDMETHOD.


  METHOD table.

    DATA ls_field LIKE LINE OF mt_fields.

    ls_field-type  = zif_abapgit_html_form=>c_field_type-table.
    ls_field-name  = iv_name.
    ls_field-label = iv_label.
    ls_field-hint  = iv_hint.

    APPEND ls_field TO mt_fields.

    ro_self = me.

  ENDMETHOD.


  METHOD text.

    DATA ls_field LIKE LINE OF mt_fields.

    ls_field-type       = zif_abapgit_html_form=>c_field_type-text.
    ls_field-name       = iv_name.
    ls_field-label      = iv_label.
    ls_field-upper_case = iv_upper_case.
    ls_field-readonly   = iv_readonly.
    ls_field-min        = iv_min.
    ls_field-max        = iv_max.
    ls_field-password   = iv_password.
    ls_field-condense   = iv_condense.
    ls_field-hint       = iv_hint.
    ls_field-required   = iv_required.
    ls_field-placeholder = iv_placeholder.

    IF iv_side_action IS NOT INITIAL AND mv_form_id IS NOT INITIAL.
      ls_field-item_class = 'with-command'.
      ls_field-side_action = iv_side_action.
      ls_field-dblclick = | ondblclick="document.getElementById('{ mv_form_id }').action = 'sapevent:|
                       && |{ iv_side_action }'; document.getElementById('{ mv_form_id }').submit()"|.
    ENDIF.

    APPEND ls_field TO mt_fields.

    ro_self = me.

  ENDMETHOD.


  METHOD textarea.

    DATA ls_field LIKE LINE OF mt_fields.

    ls_field-type        = zif_abapgit_html_form=>c_field_type-textarea.
    ls_field-name        = iv_name.
    ls_field-label       = iv_label.
    ls_field-readonly    = iv_readonly.
    ls_field-hint        = iv_hint.
    ls_field-required    = iv_required.
    ls_field-placeholder = iv_placeholder.
    ls_field-rows        = iv_rows.
    ls_field-cols        = iv_cols.
    ls_field-upper_case  = iv_upper_case.

    APPEND ls_field TO mt_fields.

    ro_self = me.

  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey_actions.
    FIELD-SYMBOLS: <ls_command> TYPE zif_abapgit_html_form=>ty_command.

    ls_hotkey_action-ui_component = |Form-{ mv_form_id }|.

    READ TABLE mt_commands WITH KEY cmd_type = zif_abapgit_html_form=>c_cmd_type-input_main
                           ASSIGNING <ls_command>.
    IF sy-subrc = 0.
      ls_hotkey_action-description = <ls_command>-label.
      ls_hotkey_action-action      = <ls_command>-action.
      ls_hotkey_action-hotkey      = |Enter|.
      INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.
    ENDIF.

    READ TABLE mt_commands WITH KEY action = zif_abapgit_definitions=>c_action-go_back
                           ASSIGNING <ls_command>.
    IF sy-subrc = 0.
      ls_hotkey_action-description = <ls_command>-label.
      ls_hotkey_action-action      = <ls_command>-action.
      ls_hotkey_action-hotkey      = |F3|.
      INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
