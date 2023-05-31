CLASS zcl_abapgit_html_form_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        !io_form            TYPE REF TO zcl_abapgit_html_form
      RETURNING
        VALUE(ro_form_util) TYPE REF TO zcl_abapgit_html_form_utils .
    CLASS-METHODS is_dirty
      IMPORTING
        !io_form_data    TYPE REF TO zcl_abapgit_string_map
        !io_compare_with TYPE REF TO zcl_abapgit_string_map
      RETURNING
        VALUE(rv_dirty) TYPE abap_bool .

    METHODS constructor
      IMPORTING
        !io_form TYPE REF TO zcl_abapgit_html_form .
    METHODS normalize
      IMPORTING
        !io_form_data       TYPE REF TO zcl_abapgit_string_map
      RETURNING
        VALUE(ro_form_data) TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception .
    METHODS validate
      IMPORTING
        !io_form_data            TYPE REF TO zcl_abapgit_string_map
      RETURNING
        VALUE(ro_validation_log) TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception .
    METHODS is_empty
      IMPORTING
        !io_form_data   TYPE REF TO zcl_abapgit_string_map
      RETURNING
        VALUE(rv_empty) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS set_data
      IMPORTING
        !io_form_data TYPE REF TO zcl_abapgit_string_map .
    METHODS exit
      IMPORTING
        !io_form_data            TYPE REF TO zcl_abapgit_string_map
        !io_check_changes_versus TYPE REF TO zcl_abapgit_string_map OPTIONAL
      RETURNING
        VALUE(rv_state)          TYPE i
      RAISING
        zcx_abapgit_exception .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_form      TYPE REF TO zcl_abapgit_html_form .
    DATA mo_form_data TYPE REF TO zcl_abapgit_string_map .

ENDCLASS.



CLASS ZCL_ABAPGIT_HTML_FORM_UTILS IMPLEMENTATION.


  METHOD constructor.
    mo_form = io_form.
  ENDMETHOD.


  METHOD create.
    CREATE OBJECT ro_form_util
      EXPORTING
        io_form = io_form.
  ENDMETHOD.


  METHOD exit.

    DATA lv_answer TYPE c LENGTH 1.
    DATA lo_compare_with LIKE io_check_changes_versus.

    lo_compare_with = io_check_changes_versus.
    IF lo_compare_with IS NOT BOUND.
      " TODO: remove this if and make io_check_changes_versus mandatory once all forms are converted
      lo_compare_with = mo_form_data.
    ENDIF.

    IF is_dirty(
      io_form_data    = io_form_data
      io_compare_with = lo_compare_with ) = abap_true.
      lv_answer = zcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
        iv_titlebar       = 'abapGit - Unsaved Changes'
        iv_text_question  = 'There are unsaved changes. Do you want to exit the form?'
        iv_default_button = '2' ).

      IF lv_answer = '1'.
        rv_state = zcl_abapgit_gui=>c_event_state-go_back_to_bookmark.
      ELSE.
        rv_state = zcl_abapgit_gui=>c_event_state-no_more_act.
      ENDIF.
    ELSE.
      rv_state = zcl_abapgit_gui=>c_event_state-go_back_to_bookmark.
    ENDIF.

  ENDMETHOD.


  METHOD is_dirty.
    rv_dirty = boolc( io_form_data->mt_entries <> io_compare_with->mt_entries ).
  ENDMETHOD.


  METHOD is_empty.

    DATA:
      lt_fields TYPE zif_abapgit_html_form=>ty_fields,
      lv_value  TYPE string,
      lv_rows   TYPE i,
      lv_row    TYPE i.

    FIELD-SYMBOLS <ls_field> LIKE LINE OF lt_fields.

    rv_empty = abap_true.
    lt_fields = mo_form->get_fields( ).
    LOOP AT lt_fields ASSIGNING <ls_field> WHERE type <> zif_abapgit_html_form=>c_field_type-field_group.
      lv_value = condense(
        val = io_form_data->get( <ls_field>-name )
        del = ` ` ).

      IF <ls_field>-type = zif_abapgit_html_form=>c_field_type-number.
        rv_empty = boolc( lv_value IS INITIAL OR lv_value = '0' ).
      ELSEIF <ls_field>-type = zif_abapgit_html_form=>c_field_type-table.
        lv_rows = io_form_data->get( |{ <ls_field>-name }-{ zif_abapgit_html_form=>c_rows }| ).
        DO lv_rows TIMES.
          lv_row = sy-index.
          DO lines( <ls_field>-subitems ) TIMES.
            lv_value = io_form_data->get( |{ <ls_field>-name }-{ lv_row }-{ sy-index }| ).
            rv_empty = boolc( lv_value IS INITIAL ).
            IF rv_empty <> abap_true.
              RETURN.
            ENDIF.
          ENDDO.
        ENDDO.
      ELSEIF <ls_field>-type = zif_abapgit_html_form=>c_field_type-textarea.
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_value WITH ''.
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_value WITH ''.
        rv_empty = boolc( lv_value IS INITIAL ).
      ELSE.
        rv_empty = boolc( lv_value IS INITIAL ).
      ENDIF.

      IF rv_empty <> abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD normalize.

    DATA:
      lt_fields TYPE zif_abapgit_html_form=>ty_fields,
      lv_value  TYPE string,
      lv_rows   TYPE i,
      lv_row    TYPE i,
      lv_len    TYPE i.

    FIELD-SYMBOLS <ls_field> LIKE LINE OF lt_fields.

    CREATE OBJECT ro_form_data.

    IF io_form_data->is_empty( ) = abap_true.
      RETURN.
    ENDIF.

    lt_fields = mo_form->get_fields( ).
    LOOP AT lt_fields ASSIGNING <ls_field> WHERE type <> zif_abapgit_html_form=>c_field_type-field_group
      AND type <> zif_abapgit_html_form=>c_field_type-hidden.

      CLEAR lv_value.
      lv_value = io_form_data->get( <ls_field>-name ).
      IF <ls_field>-condense = abap_true.
        lv_value = condense( val = lv_value
                             del = ` ` ).
      ENDIF.

      IF <ls_field>-type = zif_abapgit_html_form=>c_field_type-checkbox.
        ro_form_data->set(
          iv_key = <ls_field>-name
          iv_val = boolc( lv_value = 'on' ) ) ##TYPE.
      ELSEIF ( <ls_field>-type = zif_abapgit_html_form=>c_field_type-text
          OR <ls_field>-type = zif_abapgit_html_form=>c_field_type-textarea )
          AND <ls_field>-upper_case = abap_true.
        ro_form_data->set(
          iv_key = <ls_field>-name
          iv_val = to_upper( lv_value ) ).
      ELSEIF <ls_field>-type = zif_abapgit_html_form=>c_field_type-number.
        " Numeric value is checked in validation
        ro_form_data->set(
          iv_key = <ls_field>-name
          iv_val = condense( val = lv_value del = ` ` ) ).
      ELSEIF <ls_field>-type = zif_abapgit_html_form=>c_field_type-table.
        lv_rows = io_form_data->get( |{ <ls_field>-name }-{ zif_abapgit_html_form=>c_rows }| ).
        DO lv_rows TIMES.
          lv_row = sy-index.
          DO lines( <ls_field>-subitems ) TIMES.
            lv_value = io_form_data->get( |{ <ls_field>-name }-{ lv_row }-{ sy-index }| ).
            ro_form_data->set(
              iv_key = |{ <ls_field>-name }-{ lv_row }-{ sy-index }|
              iv_val = lv_value ).
          ENDDO.
        ENDDO.
        ro_form_data->set(
          iv_key = |{ <ls_field>-name }-{ zif_abapgit_html_form=>c_rows }|
          iv_val = |{ lv_rows }| ).
      ELSEIF <ls_field>-type = zif_abapgit_html_form=>c_field_type-textarea.
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_value
          WITH cl_abap_char_utilities=>newline.

        " Remove last line if empty (ie 2x newline)
        lv_len = strlen( lv_value ) - 2.
        IF lv_len >= 0 AND lv_value+lv_len(1) = cl_abap_char_utilities=>newline.
          lv_len = lv_len + 1.
          lv_value = lv_value(lv_len).
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


  METHOD set_data.
    mo_form_data = io_form_data.
  ENDMETHOD.


  METHOD validate.

    DATA:
      lt_fields TYPE zif_abapgit_html_form=>ty_fields,
      lv_value  TYPE string,
      lv_number TYPE i.

    FIELD-SYMBOLS <ls_field> LIKE LINE OF lt_fields.

    CREATE OBJECT ro_validation_log.

    lt_fields = mo_form->get_fields( ).
    LOOP AT lt_fields ASSIGNING <ls_field>.
      lv_value = io_form_data->get( <ls_field>-name ).
      IF <ls_field>-condense = abap_true.
        lv_value = condense( val = lv_value
                             del = ` ` ).
      ENDIF.
      IF <ls_field>-required IS NOT INITIAL AND lv_value IS INITIAL.
        ro_validation_log->set(
          iv_key = <ls_field>-name
          iv_val = |{ <ls_field>-label } cannot be empty| ).
      ENDIF.
      CASE <ls_field>-type.
        WHEN zif_abapgit_html_form=>c_field_type-text.
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
        WHEN zif_abapgit_html_form=>c_field_type-number.
          TRY.
              lv_number = lv_value.
            CATCH cx_root.
              ro_validation_log->set(
                iv_key = <ls_field>-name
                iv_val = |{ <ls_field>-label } is not numeric| ).
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
