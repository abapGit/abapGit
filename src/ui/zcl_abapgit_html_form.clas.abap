CLASS zcl_abapgit_html_form DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS create
      RETURNING
        VALUE(ro_form) TYPE REF TO zcl_abapgit_html_form.

    METHODS render
      IMPORTING
        iv_form_class TYPE string
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html.

    METHODS set_submit_params
      IMPORTING
        iv_label TYPE string
        iv_action TYPE string.

    METHODS command
      IMPORTING
        iv_label TYPE string
        iv_action TYPE string.

    METHODS text
      IMPORTING
        iv_label TYPE string
        iv_name TYPE string
        iv_value TYPE string OPTIONAL
        iv_hint TYPE string OPTIONAL
        iv_required TYPE abap_bool DEFAULT abap_false
        iv_placeholder TYPE string OPTIONAL
        iv_side_action TYPE string OPTIONAL.

    METHODS checkbox
      IMPORTING
        iv_label TYPE string
        iv_name TYPE string
        iv_hint TYPE string OPTIONAL.

    METHODS radio
      IMPORTING
        iv_label TYPE string
        iv_name TYPE string
        iv_hint TYPE string OPTIONAL.

    METHODS option
      IMPORTING
        iv_label TYPE string
        iv_value TYPE string
        iv_selected TYPE abap_bool DEFAULT abap_false.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_subitem,
        label TYPE string,
        value TYPE string,
        selected TYPE string,
      END OF ty_subitem.
    TYPES:
      tty_subitems TYPE STANDARD TABLE OF ty_subitem WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_field,
        type TYPE i,
        name TYPE string,
        label TYPE string,
        hint TYPE string,
        dblclick TYPE string,
        placeholder TYPE string,
        required TYPE string,
        item_class TYPE string,
        value TYPE string,
        side_action TYPE string,
        subitems TYPE tty_subitems,
      END OF ty_field.

    TYPES:
      BEGIN OF ty_command,
        label TYPE string,
        action TYPE string,
*        onclick ???
      END OF ty_command.

    CONSTANTS:
      BEGIN OF c_field_type,
        text TYPE i VALUE 1,
        radio TYPE i VALUE 2,
        checkbox TYPE i VALUE 3,
      END OF c_field_type.

    DATA mt_fields TYPE STANDARD TABLE OF ty_field.
    DATA ms_submit TYPE ty_command.
    DATA mt_commands TYPE STANDARD TABLE OF ty_command.

    METHODS render_field
      IMPORTING
        ii_html TYPE REF TO zif_abapgit_html
        is_field TYPE ty_field.

ENDCLASS.



CLASS ZCL_ABAPGIT_HTML_FORM IMPLEMENTATION.


  METHOD checkbox.

    DATA ls_field LIKE LINE OF mt_fields.

    ls_field-type = c_field_type-checkbox.
    ls_field-name = iv_name.
    ls_field-label = iv_label.

    " TODO "checked" ?

    IF iv_hint IS NOT INITIAL.
      ls_field-hint    = | title="{ iv_hint }"|.
    ENDIF.

    APPEND ls_field TO mt_fields.

  ENDMETHOD.


  METHOD command.

    DATA ls_cmd LIKE LINE OF mt_commands.

    ls_cmd-label = iv_label.
    ls_cmd-action = iv_action.

    APPEND ls_cmd TO mt_commands.

  ENDMETHOD.


  METHOD create.
    CREATE OBJECT ro_form.
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

    IF iv_selected = abap_true.
      ls_option-selected = ' checked'.
    ENDIF.

    APPEND ls_option TO <ls_last>-subitems.

  ENDMETHOD.


  METHOD radio.

    DATA ls_field LIKE LINE OF mt_fields.

    ls_field-type = c_field_type-radio.
    ls_field-name = iv_name.
    ls_field-label = iv_label.

    IF iv_hint IS NOT INITIAL.
      ls_field-hint    = | title="{ iv_hint }"|.
    ENDIF.

    APPEND ls_field TO mt_fields.

  ENDMETHOD.


  METHOD render.

    FIELD-SYMBOLS <ls_field> LIKE LINE OF mt_fields.
    FIELD-SYMBOLS <ls_cmd> LIKE LINE OF mt_commands.

    ASSERT ms_submit-action IS NOT INITIAL AND ms_submit-label IS NOT INITIAL.

    ri_html = zcl_abapgit_html=>create( ).

    ri_html->add( |<ul class="{ iv_form_class }">| ).
    ri_html->add( |<form action="sapevent:{ ms_submit-action }" method="post">| ).

    LOOP AT mt_fields ASSIGNING <ls_field>.
      render_field(
        ii_html = ri_html
        is_field = <ls_field> ).
    ENDLOOP.

    ri_html->add( |<li class="dialog-commands">| ).
    ri_html->add( |<input type="submit" value="{ ms_submit-label }">| ).

    LOOP AT mt_commands ASSIGNING <ls_cmd>.
      ri_html->add_a(
        iv_txt = <ls_cmd>-label
        iv_act = <ls_cmd>-action
        iv_class = 'dialog-button' ).
    ENDLOOP.

    ri_html->add( |</li>| ).

    ri_html->add( |</form>| ).
    ri_html->add( |</ul>| ).

  ENDMETHOD.


  METHOD render_field.

    DATA lv_infomark TYPE string.
    DATA lv_opt_id TYPE string.
    FIELD-SYMBOLS <ls_opt> LIKE LINE OF is_field-subitems.

    IF is_field-hint IS NOT INITIAL.
      lv_infomark = ' <span>&#x1f6c8;</span>'. " (i)
    ENDIF.

    CASE is_field-type.
      WHEN c_field_type-text.

        ii_html->add( |<li{ is_field-item_class }>| ).
        ii_html->add( |<label for="{ is_field-name }"{ is_field-hint }>{ is_field-label }{ lv_infomark }</label>| ).
        ii_html->add( |<input type="text" name="{ is_field-name }" id="{
          is_field-name }"{ is_field-required }{ is_field-placeholder }{ is_field-value }{ is_field-dblclick }>| ).
        IF is_field-side_action IS NOT INITIAL.
          ii_html->add_a(
            iv_txt = '&#x2026;'
            iv_act = is_field-side_action ).
        ENDIF.
        ii_html->add( '</li>' ).

      WHEN c_field_type-checkbox.

        ii_html->add( |<li{ is_field-item_class }>| ).
        ii_html->add( |<input type="checkbox" name="{ is_field-name }" id="{ is_field-name }">| ).
        ii_html->add( |<label for="{ is_field-name }"{ is_field-hint }>{ is_field-label }</label>| ).
        ii_html->add( '</li>' ).

      WHEN c_field_type-radio.

        ii_html->add( |<li{ is_field-item_class }>| ).
        ii_html->add( |<label{ is_field-hint }>{ is_field-label }{ lv_infomark }</label>| ).
        ii_html->add( |<div class="radio-container">| ).

        LOOP AT is_field-subitems ASSIGNING <ls_opt>.
          lv_opt_id = |{ is_field-name }{ sy-tabix }|.
          ii_html->add( |<input type="radio" name="{ is_field-name }" id="{
            lv_opt_id }" value="{ <ls_opt>-value }"{ <ls_opt>-selected }>| ).
          ii_html->add( |<label for="{ lv_opt_id }">{ <ls_opt>-label }</label>| ).
        ENDLOOP.

        ii_html->add( '</div>' ).
        ii_html->add( '</li>' ).

      WHEN OTHERS.
        ASSERT 1 = 0.
    ENDCASE.

  ENDMETHOD.


  METHOD set_submit_params.
    ms_submit-label = iv_label.
    ms_submit-action = iv_action.
  ENDMETHOD.


  METHOD text.

    DATA ls_field LIKE LINE OF mt_fields.

    ls_field-type = c_field_type-text.
    ls_field-name = iv_name.
    ls_field-label = iv_label.

    IF iv_hint IS NOT INITIAL.
      ls_field-hint    = | title="{ iv_hint }"|.
    ENDIF.

    IF iv_side_action IS NOT INITIAL.
      ls_field-item_class = ' class="with-command"'.
      ls_field-side_action = iv_side_action.
      ls_field-dblclick = | ondblclick="submitSapeventForm(null, '{ iv_side_action }')"|.
    ENDIF.

    IF iv_required = abap_true.
      ls_field-required = ' required'.
    ENDIF.

    IF iv_placeholder IS NOT INITIAL.
      ls_field-placeholder = | placeholder="{ iv_placeholder }"|.
    ENDIF.

    IF iv_value IS NOT INITIAL.
      ls_field-value = | value="{ iv_value }"|.
    ENDIF.

    APPEND ls_field TO mt_fields.

  ENDMETHOD.
ENDCLASS.
