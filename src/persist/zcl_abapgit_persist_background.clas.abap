CLASS zcl_abapgit_persist_background DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_persist_factory.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_persist_background.

    METHODS constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_db TYPE REF TO zcl_abapgit_persistence_db .

    METHODS from_xml
      IMPORTING
        !iv_string    TYPE string
      RETURNING
        VALUE(rs_xml) TYPE zif_abapgit_persist_background=>ty_xml
      RAISING
        zcx_abapgit_exception .

    METHODS to_xml
      IMPORTING
        !is_background   TYPE zif_abapgit_persist_background=>ty_background
      RETURNING
        VALUE(rv_string) TYPE string .
ENDCLASS.



CLASS zcl_abapgit_persist_background IMPLEMENTATION.


  METHOD constructor.
    mo_db = zcl_abapgit_persistence_db=>get_instance( ).
  ENDMETHOD.


  METHOD from_xml.
    CALL TRANSFORMATION id
      OPTIONS value_handling = 'accept_data_loss'
      SOURCE XML iv_string
      RESULT data = rs_xml.
  ENDMETHOD.


  METHOD to_xml.
    DATA ls_xml TYPE zif_abapgit_persist_background=>ty_xml.

    MOVE-CORRESPONDING is_background TO ls_xml.

    CALL TRANSFORMATION id
      SOURCE data = ls_xml
      RESULT XML rv_string.
  ENDMETHOD.


  METHOD zif_abapgit_persist_background~delete.

    TRY.
        mo_db->read( iv_type  = zcl_abapgit_persistence_db=>c_type_background
                     iv_value = iv_key ).
      CATCH zcx_abapgit_not_found.
        RETURN.
    ENDTRY.

    mo_db->delete( iv_type  = zcl_abapgit_persistence_db=>c_type_background
                   iv_value = iv_key ).

  ENDMETHOD.


  METHOD zif_abapgit_persist_background~exists.

    TRY.
        mo_db->read( iv_type  = zcl_abapgit_persistence_db=>c_type_background
                     iv_value = iv_key ).
        rv_yes = abap_true.
      CATCH zcx_abapgit_not_found.
        rv_yes = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_persist_background~get_by_key.

    DATA: lt_list TYPE zif_abapgit_persist_background=>ty_background_keys.

    lt_list = zif_abapgit_persist_background~list( ).

    READ TABLE lt_list WITH KEY key = iv_key INTO rs_data.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abapgit_not_found.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_persist_background~list.

    DATA: lt_list TYPE zif_abapgit_persistence=>ty_contents,
          ls_xml  TYPE zif_abapgit_persist_background=>ty_xml.

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


  METHOD zif_abapgit_persist_background~modify.

    ASSERT NOT is_data-key IS INITIAL.

    mo_db->modify(
      iv_type  = zcl_abapgit_persistence_db=>c_type_background
      iv_value = is_data-key
      iv_data  = to_xml( is_data ) ).

  ENDMETHOD.
ENDCLASS.
