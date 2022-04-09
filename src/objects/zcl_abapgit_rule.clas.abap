CLASS zcl_abapgit_rule DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_rule .
    CLASS-METHODS create
      RETURNING
        VALUE(ro_result) TYPE REF TO zif_abapgit_rule.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_item TYPE zif_abapgit_rule~ty_items.
    METHODS get_fields
      IMPORTING
        it_data          TYPE STANDARD TABLE
      RETURNING
        VALUE(rt_result) TYPE ddfields.
    METHODS fill_value
      IMPORTING
        iv_rule  TYPE zif_abapgit_rule=>ty_fill_rule
      CHANGING
        cv_value TYPE any.
ENDCLASS.



CLASS zcl_abapgit_rule IMPLEMENTATION.

  METHOD create.
    ro_result = NEW zcl_abapgit_rule( ).
  ENDMETHOD.

  METHOD zif_abapgit_rule~add_item.
    INSERT is_item INTO TABLE mt_item.
  ENDMETHOD.

  METHOD zif_abapgit_rule~add_items.
    INSERT LINES OF it_item INTO TABLE mt_item.
  ENDMETHOD.

  METHOD zif_abapgit_rule~apply_clear_logic.
    DATA ls_item TYPE zif_abapgit_rule~ty_item.
    DATA lt_field type ddfields.

    FIELD-SYMBOLS <ls_data> TYPE any.
    FIELD-SYMBOLS <ls_field> TYPE dfies.
    FIELD-SYMBOLS <lv_value> TYPE any.

    CHECK mt_item IS NOT INITIAL.

    lt_field = get_fields( ct_data ).
    LOOP AT ct_data ASSIGNING <ls_data>.
      LOOP AT lt_field ASSIGNING <ls_field>.
        CLEAR ls_item.
        READ TABLE mt_item INTO ls_item WITH KEY tabname   = iv_table
                                                 fieldname = <ls_field>-fieldname.
        IF ls_item-clear_field = abap_true.
          ASSIGN COMPONENT <ls_field>-fieldname OF STRUCTURE <ls_data> TO <lv_value>.
          CLEAR <lv_value>.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_abapgit_rule~apply_fill_logic.
    DATA ls_item TYPE zif_abapgit_rule~ty_item.
    DATA lt_field type ddfields.

    FIELD-SYMBOLS <ls_data> TYPE any.
    FIELD-SYMBOLS <ls_field> TYPE dfies.
    FIELD-SYMBOLS <lv_value> TYPE any.

    CHECK mt_item IS NOT INITIAL.

    lt_field = get_fields( ct_data ).
    LOOP AT ct_data ASSIGNING <ls_data>.
      LOOP AT lt_field ASSIGNING <ls_field>.
        CLEAR ls_item.
        READ TABLE mt_item INTO ls_item WITH KEY tabname   = iv_table
                                                 fieldname = <ls_field>-fieldname.
        IF sy-subrc = 0.
          ASSIGN COMPONENT <ls_field>-fieldname OF STRUCTURE <ls_data> TO <lv_value>.
          fill_value( EXPORTING iv_rule  = ls_item-fill_rule
                      CHANGING  cv_value = <lv_value> ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_fields.
    DATA lo_tabledescr TYPE REF TO cl_abap_tabledescr.
    DATA lo_structdescr TYPE REF TO cl_abap_structdescr.

    lo_tabledescr ?= cl_abap_tabledescr=>describe_by_data( it_data ).
    lo_structdescr ?= lo_tabledescr->get_table_line_type( ).
    rt_result = lo_structdescr->get_ddic_field_list( ).
  ENDMETHOD.


  METHOD fill_value.
    CASE iv_rule.
      WHEN zif_abapgit_rule=>c_fill_rule-date.
        cv_value = sy-datum.
      WHEN zif_abapgit_rule=>c_fill_rule-time.
        cv_value = sy-uzeit.
      WHEN zif_abapgit_rule=>c_fill_rule-timestamp.
        GET TIME STAMP FIELD cv_value.
      WHEN zif_abapgit_rule=>c_fill_rule-user.
        cv_value = sy-uname.
      WHEN zif_abapgit_rule=>c_fill_rule-client.
        cv_value = sy-mandt.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
