CLASS zcl_abapgit_field_rules DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_field_rules.
    CLASS-METHODS create
      RETURNING
        VALUE(ro_result) TYPE REF TO zif_abapgit_field_rules.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_item,
        tabname   TYPE tabname,
        fieldname TYPE fieldname,
        fill_rule TYPE zif_abapgit_field_rules=>ty_fill_rule,
      END OF ty_item,
      ty_items TYPE SORTED TABLE OF ty_item WITH UNIQUE KEY tabname fieldname.

    DATA mt_item TYPE ty_items.

    METHODS fill_value
      IMPORTING
        iv_rule  TYPE zif_abapgit_field_rules=>ty_fill_rule
      CHANGING
        cv_value TYPE any.
ENDCLASS.



CLASS zcl_abapgit_field_rules IMPLEMENTATION.

  METHOD create.
    CREATE OBJECT ro_result TYPE zcl_abapgit_field_rules.
  ENDMETHOD.

  METHOD zif_abapgit_field_rules~add.
    DATA ls_item TYPE zcl_abapgit_field_rules=>ty_item.

    ls_item-tabname   = iv_table.
    ls_item-fieldname = iv_field.
    ls_item-fill_rule = iv_fill_rule.
    INSERT ls_item INTO TABLE mt_item.

    ro_self = me.
  ENDMETHOD.


  METHOD zif_abapgit_field_rules~apply_clear_logic.
    DATA ls_item TYPE ty_item.

    FIELD-SYMBOLS <ls_data> TYPE any.
    FIELD-SYMBOLS <lv_value> TYPE any.

    IF mt_item IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT ct_data ASSIGNING <ls_data>.
      LOOP AT mt_item INTO ls_item WHERE tabname = iv_table.
        ASSIGN COMPONENT ls_item-fieldname OF STRUCTURE <ls_data> TO <lv_value>.
        IF sy-subrc = 0.
          CLEAR <lv_value>.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_abapgit_field_rules~apply_fill_logic.
    DATA ls_item TYPE ty_item.

    FIELD-SYMBOLS <ls_data> TYPE any.
    FIELD-SYMBOLS <lv_value> TYPE any.

    IF mt_item IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT ct_data ASSIGNING <ls_data>.
      LOOP AT mt_item INTO ls_item WHERE tabname = iv_table.
        ASSIGN COMPONENT ls_item-fieldname OF STRUCTURE <ls_data> TO <lv_value>.
        IF sy-subrc = 0.
          fill_value( EXPORTING iv_rule  = ls_item-fill_rule
                      CHANGING  cv_value = <lv_value> ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD fill_value.
    CASE iv_rule.
      WHEN zif_abapgit_field_rules=>c_fill_rule-date.
        cv_value = sy-datum.
      WHEN zif_abapgit_field_rules=>c_fill_rule-time.
        cv_value = sy-uzeit.
      WHEN zif_abapgit_field_rules=>c_fill_rule-timestamp.
        GET TIME STAMP FIELD cv_value.
      WHEN zif_abapgit_field_rules=>c_fill_rule-user.
        cv_value = sy-uname.
      WHEN zif_abapgit_field_rules=>c_fill_rule-client.
        cv_value = sy-mandt.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
