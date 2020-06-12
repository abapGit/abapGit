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
        iv_action TYPE string
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html.

    METHODS text
      IMPORTING
        iv_label TYPE string
        iv_name TYPE string
        iv_value TYPE string OPTIONAL
        iv_hint TYPE string OPTIONAL
        iv_required TYPE abap_bool DEFAULT abap_false
        iv_placeholder TYPE string OPTIONAL
        iv_side_action TYPE string OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_field_type,
        text TYPE i VALUE 1,
        radio TYPE i VALUE 2,
        checkbox TYPE i VALUE 3,
      END OF c_field_type.

    DATA mo_fields TYPE REF TO zcl_abapgit_html.
    DATA mo_commands TYPE REF TO zcl_abapgit_html.
    DATA mv_last_field_type TYPE i.
ENDCLASS.



CLASS ZCL_ABAPGIT_HTML_FORM IMPLEMENTATION.


  METHOD create.

    CREATE OBJECT ro_form.
    CREATE OBJECT ro_form->mo_fields.
    CREATE OBJECT ro_form->mo_commands.

  ENDMETHOD.


  METHOD render.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( |<ul class="{ iv_form_class }">| ).
    ri_html->add( |<form action="sapevent:{ iv_action }" method="post">| ).
    ri_html->add( mo_fields ).
    ri_html->add( |<li>| ).
    ri_html->add( mo_commands ).
    ri_html->add( |</li>| ).
    ri_html->add( |</form>| ).
    ri_html->add( |</ul>| ).

  ENDMETHOD.


  METHOD text.

    DATA lv_title TYPE string.
    DATA lv_infomark TYPE string.
    DATA lv_placeholder TYPE string.
    DATA lv_required TYPE string.
    DATA lv_with_command TYPE string.
    DATA lv_value TYPE string.

    mv_last_field_type = c_field_type-text.

    IF iv_hint IS NOT INITIAL.
      lv_title = | title="{ iv_hint }"|.
      lv_infomark = ' <span>&#x1f6c8;</span>'.
    ENDIF.

    IF iv_side_action IS NOT INITIAL.
      lv_with_command = ' class="with-command"'.
    ENDIF.

    IF iv_required = abap_true.
      lv_required = ' required'.
    ENDIF.

    IF iv_placeholder IS NOT INITIAL.
      lv_placeholder = | placeholder="{ iv_placeholder }"|.
    ENDIF.

    IF iv_value IS NOT INITIAL.
      lv_value = | value="{ iv_value }"|.
    ENDIF.

    mo_fields->add( |<li>{ lv_with_command }| ).
    mo_fields->add( |<label for="{ iv_name }"{ lv_title }>{ iv_label }{ lv_infomark }</label>| ).
    mo_fields->add( |<input type="text" name="{ iv_name }" id="{ iv_name }"{ lv_required }{ lv_placeholder }{ lv_value }>| ).
    IF iv_side_action IS NOT INITIAL.
      mo_fields->add_a(
        iv_txt = '&#x2026;'
        iv_act = iv_side_action ).
    ENDIF.
    mo_fields->add( '</li>' ).

  ENDMETHOD.
ENDCLASS.
