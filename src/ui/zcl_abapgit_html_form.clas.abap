CLASS zcl_abapgit_html_form DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        !iv_form_id    TYPE csequence OPTIONAL
        !iv_help_page  TYPE csequence OPTIONAL
      RETURNING
        VALUE(ro_form) TYPE REF TO zcl_abapgit_html_form.
    METHODS render
      IMPORTING
        !iv_form_class     TYPE csequence
        !io_values         TYPE REF TO zcl_abapgit_string_map
        !io_validation_log TYPE REF TO zcl_abapgit_string_map OPTIONAL
      RETURNING
        VALUE(ri_html)     TYPE REF TO zif_abapgit_html.
    METHODS command
      IMPORTING
        !iv_label      TYPE csequence
        !iv_action     TYPE csequence
        !iv_is_main    TYPE abap_bool DEFAULT abap_false
        !iv_as_a       TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ro_self) TYPE REF TO zcl_abapgit_html_form.
    METHODS text
      IMPORTING
        !iv_label       TYPE csequence
        !iv_name        TYPE csequence
        !iv_hint        TYPE csequence OPTIONAL
        !iv_required    TYPE abap_bool DEFAULT abap_false
        !iv_upper_case  TYPE abap_bool DEFAULT abap_false
        !iv_readonly    TYPE abap_bool DEFAULT abap_false
        !iv_password    TYPE abap_bool DEFAULT abap_false
        !iv_placeholder TYPE csequence OPTIONAL
        !iv_side_action TYPE csequence OPTIONAL
        !iv_min         TYPE i DEFAULT cl_abap_math=>min_int4
        !iv_max         TYPE i DEFAULT cl_abap_math=>max_int4
      RETURNING
        VALUE(ro_self)  TYPE REF TO zcl_abapgit_html_form.
    METHODS textarea
      IMPORTING
        !iv_label       TYPE csequence
        !iv_name        TYPE csequence
        !iv_rows        TYPE i DEFAULT 3
        !iv_hint        TYPE csequence OPTIONAL
        !iv_required    TYPE abap_bool DEFAULT abap_false
        !iv_readonly    TYPE abap_bool DEFAULT abap_false
        !iv_placeholder TYPE csequence OPTIONAL
      RETURNING
        VALUE(ro_self)  TYPE REF TO zcl_abapgit_html_form.
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
        VALUE(ro_self) TYPE REF TO zcl_abapgit_html_form.
    METHODS checkbox
      IMPORTING
        !iv_label      TYPE csequence
        !iv_name       TYPE csequence
        !iv_hint       TYPE csequence OPTIONAL
      RETURNING
        VALUE(ro_self) TYPE REF TO zcl_abapgit_html_form.
    METHODS radio
      IMPORTING
        !iv_label         TYPE csequence
        !iv_name          TYPE csequence
        !iv_default_value TYPE csequence OPTIONAL
        !iv_hint          TYPE csequence OPTIONAL
      RETURNING
        VALUE(ro_self)    TYPE REF TO zcl_abapgit_html_form.
    METHODS option
      IMPORTING
        !iv_label      TYPE csequence
        !iv_value      TYPE csequence
      RETURNING
        VALUE(ro_self) TYPE REF TO zcl_abapgit_html_form.
    METHODS start_group
      IMPORTING
        !iv_label      TYPE csequence
        !iv_name       TYPE csequence
        !iv_hint       TYPE csequence OPTIONAL
      RETURNING
        VALUE(ro_self) TYPE REF TO zcl_abapgit_html_form.
    METHODS normalize_form_data
      IMPORTING
        !io_form_data       TYPE REF TO zcl_abapgit_string_map
      RETURNING
        VALUE(ro_form_data) TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception.
    METHODS validate_required_fields
      IMPORTING
        !io_form_data            TYPE REF TO zcl_abapgit_string_map
      RETURNING
        VALUE(ro_validation_log) TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_subitem,
        label TYPE string,
        value TYPE string,
      END OF ty_subitem.
    TYPES:
      ty_subitems TYPE STANDARD TABLE OF ty_subitem WITH DEFAULT KEY.
    TYPES:
      BEGIN OF ty_field,
        type          TYPE i,
        name          TYPE string,
        label         TYPE string,
        hint          TYPE string,
        dblclick      TYPE string,
        placeholder   TYPE string,
        required      TYPE string,
        upper_case    TYPE abap_bool,
        item_class    TYPE string,
        error         TYPE string,
        default_value TYPE string,
        side_action   TYPE string,
        subitems      TYPE ty_subitems,
        readonly      TYPE abap_bool,
        password      TYPE abap_bool,
        min           TYPE i,
        max           TYPE i,
        rows          TYPE i,
*        onclick ???
      END OF ty_field.
    TYPES:
      BEGIN OF ty_command,
        label   TYPE string,
        action  TYPE string,
        is_main TYPE abap_bool,
        as_a    TYPE abap_bool,
*        onclick ???
      END OF ty_command.

    CONSTANTS:
      BEGIN OF c_field_type,
        text        TYPE i VALUE 1,
        radio       TYPE i VALUE 2,
        checkbox    TYPE i VALUE 3,
        field_group TYPE i VALUE 4,
        number      TYPE i VALUE 5,
        textarea    TYPE i VALUE 6,
      END OF c_field_type.
    DATA:
      mt_fields TYPE STANDARD TABLE OF ty_field
          WITH UNIQUE SORTED KEY by_name COMPONENTS name.
    DATA:
      mt_commands TYPE STANDARD TABLE OF ty_command.
    DATA mv_form_id TYPE string.
    DATA mv_help_page TYPE string.

    METHODS render_field
      IMPORTING
        !ii_html           TYPE REF TO zif_abapgit_html
        !io_values         TYPE REF TO zcl_abapgit_string_map
        !io_validation_log TYPE REF TO zcl_abapgit_string_map
        !is_field          TYPE ty_field.
    METHODS render_command
      IMPORTING
        !ii_html TYPE REF TO zif_abapgit_html
        !is_cmd  TYPE ty_command.
ENDCLASS.



CLASS zcl_abapgit_html_form IMPLEMENTATION.


  METHOD checkbox.

    DATA ls_field LIKE LINE OF mt_fields.

    ls_field-type  = c_field_type-checkbox.
    ls_field-name  = iv_name.
    ls_field-label = iv_label.
    ls_field-hint  = iv_hint.

    APPEND ls_field TO mt_fields.

    ro_self = me.

  ENDMETHOD.


  METHOD command.

    DATA ls_cmd LIKE LINE OF mt_commands.

    ASSERT iv_as_a IS INITIAL OR iv_is_main IS INITIAL.

    ls_cmd-label = iv_label.
    ls_cmd-action = iv_action.
    ls_cmd-is_main = iv_is_main.
    ls_cmd-as_a = iv_as_a.

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


  METHOD normalize_form_data.

    DATA lv_value TYPE string.
    FIELD-SYMBOLS <ls_field> LIKE LINE OF mt_fields.

    CREATE OBJECT ro_form_data.

    LOOP AT mt_fields ASSIGNING <ls_field>.
      CLEAR lv_value.
      lv_value = io_form_data->get( <ls_field>-name ).

      IF <ls_field>-type = c_field_type-checkbox.
        ro_form_data->set(
          iv_key = <ls_field>-name
          iv_val = boolc( lv_value = 'on' ) ).
      ELSEIF <ls_field>-type = c_field_type-text AND <ls_field>-upper_case = abap_true.
        ro_form_data->set(
          iv_key = <ls_field>-name
          iv_val = to_upper( lv_value ) ).
      ELSEIF <ls_field>-type = c_field_type-number.
        IF lv_value NA '0123456789- '.

        ENDIF.
        ro_form_data->set(
          iv_key = <ls_field>-name
          iv_val = lv_value ).
      ELSE.
        ro_form_data->set(
          iv_key = <ls_field>-name
          iv_val = lv_value ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD number.

    DATA ls_field LIKE LINE OF mt_fields.

    ls_field-type     = c_field_type-number.
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
    ASSERT <ls_last>-type = c_field_type-radio. " Or dropdown - TODO in future

    ls_option-label = iv_label.
    ls_option-value = iv_value.

    APPEND ls_option TO <ls_last>-subitems.

    ro_self = me.

  ENDMETHOD.


  METHOD radio.

    DATA ls_field LIKE LINE OF mt_fields.

    ls_field-type  = c_field_type-radio.
    ls_field-name  = iv_name.
    ls_field-label = iv_label.
    ls_field-default_value = iv_default_value.
    ls_field-hint  = iv_hint.

    APPEND ls_field TO mt_fields.

    ro_self = me.

  ENDMETHOD.


  METHOD render.

    FIELD-SYMBOLS <ls_field> LIKE LINE OF mt_fields.
    FIELD-SYMBOLS <ls_cmd> LIKE LINE OF mt_commands.
    DATA lv_hint TYPE string.
    DATA ls_form_id TYPE string.
    DATA lv_cur_group TYPE string.

    IF mv_form_id IS NOT INITIAL.
      ls_form_id = | id="{ mv_form_id }"|.
    ENDIF.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( |<div class="{ iv_form_class }">| ).
    ri_html->add( |<form method="post"{ ls_form_id }>| ).

    " Add hidden button that triggers main command when pressing enter
    LOOP AT mt_commands ASSIGNING <ls_cmd> WHERE is_main = abap_true.
      ri_html->add( |<button type="submit" formaction="sapevent:{ <ls_cmd>-action
                    }" class="hidden-submit" aria-hidden="true" tabindex="-1"></button>| ).
      EXIT.
    ENDLOOP.

    LOOP AT mt_fields ASSIGNING <ls_field>.
      AT FIRST.
        IF <ls_field>-type <> c_field_type-field_group.
          ri_html->add( |<ul>| ).
        ENDIF.
      ENDAT.

      IF <ls_field>-type = c_field_type-field_group.
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
        is_field          = <ls_field> ).

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
      ri_html->add_a(
        iv_txt   = zcl_abapgit_gui_buttons=>help( )
        iv_typ   = zif_abapgit_html=>c_action_type-url
        iv_act   = mv_help_page
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

  ENDMETHOD.


  METHOD render_command.

    DATA lv_main_submit TYPE string.

    IF is_cmd-as_a = abap_true.
      ii_html->add_a(
        iv_txt = is_cmd-label
        iv_act = is_cmd-action
        iv_class = 'dialog-commands' ).
    ELSE.
      IF is_cmd-is_main = abap_true.
        lv_main_submit = ' class="main"'.
      ELSE.
        CLEAR lv_main_submit.
      ENDIF.
      ii_html->add( |<input type="submit" value="{
        is_cmd-label }"{ lv_main_submit } formaction="sapevent:{ is_cmd-action }">| ).
    ENDIF.

  ENDMETHOD.


  METHOD render_field.

    DATA lv_opt_id TYPE string.
    DATA lv_error TYPE string.
    DATA lv_value TYPE string.
    DATA lv_checked TYPE string.
    DATA lv_item_class TYPE string.
    DATA lv_hint TYPE string.
    DATA lv_required TYPE string.
    DATA lv_attr TYPE string.
    DATA lv_type TYPE string.
    FIELD-SYMBOLS <ls_opt> LIKE LINE OF is_field-subitems.

    " Get value and validation error from maps
    lv_value = io_values->get( is_field-name ).
    IF io_validation_log IS BOUND.
      lv_error = io_validation_log->get( is_field-name ).
    ENDIF.

    " Prepare item class
    lv_item_class = is_field-item_class.
    IF lv_error IS NOT INITIAL.
      lv_item_class = condense( lv_item_class && ' error' ).
    ENDIF.
    IF lv_item_class IS NOT INITIAL.
      lv_item_class = | class="{ lv_item_class }"|.
    ENDIF.

    IF is_field-required = abap_true.
      lv_required = ' <em>*</em>'.
    ENDIF.

    IF is_field-hint IS NOT INITIAL.
      lv_hint = | title="{ is_field-hint }"|.
    ENDIF.

    IF is_field-readonly = abap_true.
      lv_attr = lv_attr && ' readonly'.
    ENDIF.

    IF is_field-placeholder IS NOT INITIAL.
      lv_attr = lv_attr && | placeholder="{ is_field-placeholder }"|.
    ENDIF.

    " Render field
    ii_html->add( |<li{ lv_item_class }>| ).

    CASE is_field-type.
      WHEN c_field_type-text OR c_field_type-number.

        ii_html->add( |<label for="{ is_field-name }"{ lv_hint }>{ is_field-label }{ lv_required }</label>| ).
        IF lv_error IS NOT INITIAL.
          ii_html->add( |<small>{ lv_error }</small>| ).
        ENDIF.

        IF is_field-side_action IS NOT INITIAL.
          ii_html->add( '<div class="input-container">' ). " Ugly :(
        ENDIF.

        IF is_field-type = c_field_type-number.
          lv_type = 'number'.
        ELSEIF is_field-password = abap_true.
          lv_type = 'password'.
        ELSE.
          lv_type = 'text'.
        ENDIF.

        ii_html->add( |<input type="{ lv_type }" name="{ is_field-name }" id="{
          is_field-name }" value="{ lv_value }"{ is_field-dblclick }{ lv_attr }>| ).

        IF is_field-side_action IS NOT INITIAL.
          ii_html->add( '</div>' ).
          ii_html->add( '<div class="command-container">' ).
          ii_html->add( |<input type="submit" value="&#x2026;" formaction="sapevent:{ is_field-side_action }">| ).
          ii_html->add( '</div>' ).
        ENDIF.

      WHEN c_field_type-textarea.

        ii_html->add( |<label for="{ is_field-name }"{ lv_hint }>{ is_field-label }{ lv_required }</label>| ).
        IF lv_error IS NOT INITIAL.
          ii_html->add( |<small>{ lv_error }</small>| ).
        ENDIF.

        ii_html->add( |<textarea name="{ is_field-name }" id="{
          is_field-name }" value="{ lv_value }" rows="{ is_field-rows }"{ lv_attr }>| ).
        ii_html->add( |</textarea>| ).

      WHEN c_field_type-checkbox.

        IF lv_error IS NOT INITIAL.
          ii_html->add( |<small>{ lv_error }</small>| ).
        ENDIF.
        IF lv_value = abap_true OR lv_value = 'on'. " boolc return ` ` which is not initial -> bug after 1st validation
          lv_checked = ' checked'.
        ENDIF.
        ii_html->add( |<input type="checkbox" name="{ is_field-name }" id="{ is_field-name }"{ lv_checked }>| ).
        ii_html->add( |<label for="{ is_field-name }"{ lv_hint }>{ is_field-label }</label>| ).

      WHEN c_field_type-radio.

        ii_html->add( |<label{ lv_hint }>{ is_field-label }</label>| ).
        IF lv_error IS NOT INITIAL.
          ii_html->add( |<small>{ lv_error }</small>| ).
        ENDIF.
        ii_html->add( |<div class="radio-container">| ).

        LOOP AT is_field-subitems ASSIGNING <ls_opt>.
          CLEAR lv_checked.
          IF lv_value = <ls_opt>-value OR ( lv_value IS INITIAL AND <ls_opt>-value = is_field-default_value ).
            lv_checked = ' checked'.
          ENDIF.
          lv_opt_id = |{ is_field-name }{ sy-tabix }|.
          ii_html->add( |<input type="radio" name="{ is_field-name }" id="{
            lv_opt_id }" value="{ <ls_opt>-value }"{ lv_checked }>| ).
          ii_html->add( |<label for="{ lv_opt_id }">{ <ls_opt>-label }</label>| ).
        ENDLOOP.

        ii_html->add( '</div>' ).

      WHEN OTHERS.
        ASSERT 1 = 0.
    ENDCASE.

    ii_html->add( '</li>' ).

  ENDMETHOD.


  METHOD start_group.

    DATA ls_field LIKE LINE OF mt_fields.

    ls_field-type  = c_field_type-field_group.
    ls_field-label = iv_label.
    ls_field-name  = iv_name.
    ls_field-hint  = iv_hint.

    APPEND ls_field TO mt_fields.

    ro_self = me.

  ENDMETHOD.


  METHOD text.

    DATA ls_field LIKE LINE OF mt_fields.

    ls_field-type       = c_field_type-text.
    ls_field-name       = iv_name.
    ls_field-label      = iv_label.
    ls_field-upper_case = iv_upper_case.
    ls_field-readonly   = iv_readonly.
    ls_field-min        = iv_min.
    ls_field-max        = iv_max.
    ls_field-password   = iv_password.
    ls_field-hint       = iv_hint.
    ls_field-required   = iv_required.
    ls_field-placeholder = iv_placeholder.

    IF iv_side_action IS NOT INITIAL AND mv_form_id IS NOT INITIAL.
      ls_field-item_class = 'with-command'.
      ls_field-side_action = iv_side_action.
      ls_field-dblclick = | ondblclick="document.getElementById('{ mv_form_id
        }').action = 'sapevent:{ iv_side_action
        }'; document.getElementById('{ mv_form_id
        }').submit()"|.
    ENDIF.

    APPEND ls_field TO mt_fields.

    ro_self = me.

  ENDMETHOD.


  METHOD textarea.

    DATA ls_field LIKE LINE OF mt_fields.

    ls_field-type        = c_field_type-textarea.
    ls_field-name        = iv_name.
    ls_field-label       = iv_label.
    ls_field-readonly    = iv_readonly.
    ls_field-hint        = iv_hint.
    ls_field-required    = iv_required.
    ls_field-placeholder = iv_placeholder.
    ls_field-rows        = iv_rows.

    APPEND ls_field TO mt_fields.

    ro_self = me.

  ENDMETHOD.


  METHOD validate_required_fields.

    DATA lv_value TYPE string.
    DATA lv_number TYPE i.
    FIELD-SYMBOLS <ls_field> LIKE LINE OF mt_fields.

    CREATE OBJECT ro_validation_log.

    LOOP AT mt_fields ASSIGNING <ls_field>.
      lv_value = io_form_data->get( <ls_field>-name ).
      IF <ls_field>-required IS NOT INITIAL AND lv_value IS INITIAL.
        ro_validation_log->set(
          iv_key = <ls_field>-name
          iv_val = |{ <ls_field>-label } cannot be empty| ).
      ENDIF.
      CASE <ls_field>-type.
        WHEN c_field_type-text.
          IF <ls_field>-min <> cl_abap_math=>min_int4 AND strlen( lv_value ) < <ls_field>-min.
            ro_validation_log->set(
              iv_key = <ls_field>-name
              iv_val = |{ <ls_field>-label } must not be shorter than { <ls_field>-min } characters| ).
          ENDIF.
          IF <ls_field>-max <> cl_abap_math=>max_int4 AND strlen( lv_value ) > <ls_field>-max.
            ro_validation_log->set(
              iv_key = <ls_field>-name
              iv_val = |{ <ls_field>-label } must not be longer than { <ls_field>-max } characters| ).
          ENDIF.
        WHEN c_field_type-number.
          TRY.
              lv_number = lv_value.
            CATCH cx_root.
              ro_validation_log->set(
                iv_key = <ls_field>-name
                iv_val = |{ <ls_field>-label } is not a number| ).
              CONTINUE.
          ENDTRY.
          IF <ls_field>-min <> cl_abap_math=>min_int4 AND lv_number < <ls_field>-min.
            ro_validation_log->set(
              iv_key = <ls_field>-name
              iv_val = |{ <ls_field>-label } must not be lower than { <ls_field>-min }| ).
          ENDIF.
          IF <ls_field>-max <> cl_abap_math=>max_int4 AND lv_number > <ls_field>-max.
            ro_validation_log->set(
              iv_key = <ls_field>-name
              iv_val = |{ <ls_field>-label } must not be higher than { <ls_field>-max }| ).
          ENDIF.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
