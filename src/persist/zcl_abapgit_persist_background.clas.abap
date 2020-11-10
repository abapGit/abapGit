CLASS zcl_abapgit_persist_background DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_xml,
        method   TYPE string,
        username TYPE string,
        password TYPE string,
        settings TYPE zif_abapgit_background=>ty_settings_tt,
      END OF ty_xml .
    TYPES:
      BEGIN OF ty_background,
        key TYPE zif_abapgit_persistence=>ty_value.
        INCLUDE TYPE ty_xml.
    TYPES: END OF ty_background .
    TYPES:
      ty_background_keys TYPE STANDARD TABLE OF ty_background WITH DEFAULT KEY .

    METHODS constructor .
    METHODS list
      RETURNING
        VALUE(rt_list) TYPE ty_background_keys
      RAISING
        zcx_abapgit_exception .
    METHODS get_by_key
      IMPORTING
        !iv_key        TYPE ty_background-key
      RETURNING
        VALUE(rs_data) TYPE ty_background
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_not_found .
    METHODS modify
      IMPORTING
        !is_data TYPE ty_background
      RAISING
        zcx_abapgit_exception .
    METHODS delete
      IMPORTING
        !iv_key TYPE ty_background-key
      RAISING
        zcx_abapgit_exception .
    METHODS exists
      IMPORTING
        !iv_key       TYPE ty_background-key
      RETURNING
        VALUE(rv_yes) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_db TYPE REF TO zcl_abapgit_persistence_db .

    METHODS from_xml
      IMPORTING
        !iv_string    TYPE string
      RETURNING
        VALUE(rs_xml) TYPE ty_xml
      RAISING
        zcx_abapgit_exception .
    METHODS to_xml
      IMPORTING
        !is_background   TYPE ty_background
      RETURNING
        VALUE(rv_string) TYPE string .
ENDCLASS.



CLASS ZCL_ABAPGIT_PERSIST_BACKGROUND IMPLEMENTATION.


  METHOD constructor.
    mo_db = zcl_abapgit_persistence_db=>get_instance( ).
  ENDMETHOD.


  METHOD delete.

    TRY.
        mo_db->read( iv_type  = zcl_abapgit_persistence_db=>c_type_background
                     iv_value = iv_key ).
      CATCH zcx_abapgit_not_found.
        RETURN.
    ENDTRY.

    mo_db->delete( iv_type  = zcl_abapgit_persistence_db=>c_type_background
                   iv_value = iv_key ).

  ENDMETHOD.


  METHOD exists.

    TRY.
        mo_db->read( iv_type  = zcl_abapgit_persistence_db=>c_type_background
                     iv_value = iv_key ).
        rv_yes = abap_true.
      CATCH zcx_abapgit_not_found.
        rv_yes = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD from_xml.
    CALL TRANSFORMATION id
      OPTIONS value_handling = 'accept_data_loss'
      SOURCE XML iv_string
      RESULT data = rs_xml.
  ENDMETHOD.


  METHOD get_by_key.

    DATA: lt_list TYPE ty_background_keys.

    lt_list = list( ).

    READ TABLE lt_list WITH KEY key = iv_key INTO rs_data.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abapgit_not_found.
    ENDIF.

  ENDMETHOD.


  METHOD list.

    DATA: lt_list TYPE zif_abapgit_persistence=>ty_contents,
          ls_xml  TYPE ty_xml.

    FIELD-SYMBOLS: <ls_list>   LIKE LINE OF lt_list,
                   <ls_output> LIKE LINE OF rt_list.


    lt_list = mo_db->list_by_type( zcl_abapgit_persistence_db=>c_type_background ).

    LOOP AT lt_list ASSIGNING <ls_list>.
      ls_xml = from_xml( <ls_list>-data_str ).

      APPEND INITIAL LINE TO rt_list ASSIGNING <ls_output>.
      MOVE-CORRESPONDING ls_xml TO <ls_output>.
      <ls_output>-key = <ls_list>-value.
    ENDLOOP.

  ENDMETHOD.


  METHOD modify.

    ASSERT NOT is_data-key IS INITIAL.

    mo_db->modify(
      iv_type  = zcl_abapgit_persistence_db=>c_type_background
      iv_value = is_data-key
      iv_data  = to_xml( is_data ) ).

  ENDMETHOD.


  METHOD to_xml.
    DATA: ls_xml TYPE ty_xml.

    MOVE-CORRESPONDING is_background TO ls_xml.

    CALL TRANSFORMATION id
      SOURCE data = ls_xml
      RESULT XML rv_string.
  ENDMETHOD.
ENDCLASS.
