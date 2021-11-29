CLASS zcl_abapgit_object_chkc DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_object_common_aff
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS zif_abapgit_object~changed_by  REDEFINITION.
    METHODS zif_abapgit_object~serialize   REDEFINITION.
    METHODS zif_abapgit_object~deserialize REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_abapgit_object_chkc IMPLEMENTATION.

  METHOD zif_abapgit_object~changed_by.

    DATA: lr_data        TYPE REF TO data,
          lo_chkc_db_api TYPE REF TO object,
          lv_name        TYPE c LENGTH 120,
          lx_error       TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lg_chkc_header> TYPE any,
                   <lg_chkc_user>   TYPE any.

    IF ms_item-obj_type <> 'CHKC'.
      RETURN.
    ENDIF.

    TRY.
        CREATE OBJECT lo_chkc_db_api TYPE ('CL_CHKC_DB_API').
        CREATE DATA lr_data TYPE ('CL_CHKC_DB_API=>TY_HEADER').
        ASSIGN lr_data->* TO <lg_chkc_header>.

        lv_name = ms_item-obj_name.

        CALL METHOD lo_chkc_db_api->('GET_HEADER')
          EXPORTING
            name    = lv_name
            version = 'I'
          RECEIVING
            header  = <lg_chkc_header>.

        IF <lg_chkc_header> IS INITIAL.
          CALL METHOD lo_chkc_db_api->('GET_HEADER')
            EXPORTING
              name    = lv_name
              version = 'A'
            RECEIVING
              header  = <lg_chkc_header>.
        ENDIF.

        ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <lg_chkc_header> TO <lg_chkc_user>.
        rv_user = <lg_chkc_user>.

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.



  METHOD zif_abapgit_object~deserialize.

    DATA lr_chkc TYPE REF TO data.

    FIELD-SYMBOLS <lg_chkc_agit> TYPE any.

    IF ms_item-obj_type <> 'CHKC'.
      RETURN.
    ENDIF.

    CREATE DATA lr_chkc TYPE ('ZIF_ABAPGIT_AFF_CHKC_V1=>TY_MAIN').
    ASSIGN lr_chkc->* TO <lg_chkc_agit>.

    ASSERT <lg_chkc_agit> IS ASSIGNED.

    super->zif_abapgit_object~deserialize(
      EXPORTING
        iv_package = iv_package
        io_xml     = io_xml
        iv_step    = iv_step
        ii_log     = ii_log   ).

  ENDMETHOD.



  METHOD zif_abapgit_object~serialize.

    DATA lr_chkc TYPE REF TO data.

    FIELD-SYMBOLS  <lg_chkc_agit>  TYPE any.

    IF ms_item-obj_type <> 'CHKC'.
      RETURN.
    ENDIF.

    CREATE DATA lr_chkc TYPE ('ZIF_ABAPGIT_AFF_CHKC_V1=>TY_MAIN').
    ASSIGN lr_chkc->* TO <lg_chkc_agit>.
    ASSERT sy-subrc = 0.

    super->zif_abapgit_object~serialize( io_xml = io_xml ).

  ENDMETHOD.
ENDCLASS.
