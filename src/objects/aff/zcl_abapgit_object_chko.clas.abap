CLASS zcl_abapgit_object_chko DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_object_common_aff
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS zif_abapgit_object~changed_by REDEFINITION.
    METHODS zif_abapgit_object~serialize REDEFINITION.
    METHODS zif_abapgit_object~deserialize REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_abapgit_object_chko IMPLEMENTATION.

  METHOD zif_abapgit_object~changed_by.

    DATA: lr_data        TYPE REF TO data,
          lo_chko_db_api TYPE REF TO object,
          lv_name        TYPE c LENGTH 120,
          lx_error       TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lg_chko_header> TYPE any,
                   <lg_chko_user>   TYPE any.

    IF ms_item-obj_type <> 'CHKO'.
      RETURN.
    ENDIF.

    TRY.
        CREATE OBJECT lo_chko_db_api TYPE ('CL_CHKO_DB_API').
        CREATE DATA lr_data TYPE ('CL_CHKO_DB_API=>TY_HEADER').
        ASSIGN lr_data->* TO <lg_chko_header>.

        lv_name = ms_item-obj_name.

        CALL METHOD lo_chko_db_api->('GET_HEADER')
          EXPORTING
            name    = lv_name
            version = 'I'
          RECEIVING
            header  = <lg_chko_header>.

        IF <lg_chko_header> IS INITIAL.
          CALL METHOD lo_chko_db_api->('GET_HEADER')
            EXPORTING
              name    = lv_name
              version = 'A'
            RECEIVING
              header  = <lg_chko_header>.
        ENDIF.

        ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <lg_chko_header> TO <lg_chko_user>.
        rv_user = <lg_chko_user>.

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.



  METHOD zif_abapgit_object~deserialize.

    DATA lr_chko TYPE REF TO data.

    FIELD-SYMBOLS <lg_chko_agit> TYPE any.

    IF ms_item-obj_type <> 'CHKO'.
      RETURN.
    ENDIF.

    CREATE DATA lr_chko TYPE ('ZIF_ABAPGIT_AFF_CHKO_V1=>TY_MAIN').
    ASSIGN lr_chko->* TO <lg_chko_agit>.

    ASSERT <lg_chko_agit> IS ASSIGNED.

    super->zif_abapgit_object~deserialize(
      EXPORTING
        iv_package = iv_package
        io_xml     = io_xml
        iv_step    = iv_step
        ii_log     = ii_log   ).

  ENDMETHOD.



  METHOD zif_abapgit_object~serialize.

    DATA lr_chko TYPE REF TO data.

    FIELD-SYMBOLS  <lg_chko_agit>  TYPE any.

    IF ms_item-obj_type <> 'CHKO'.
      RETURN.
    ENDIF.

    CREATE DATA lr_chko TYPE ('ZIF_ABAPGIT_AFF_CHKO_V1=>TY_MAIN').
    ASSIGN lr_chko->* TO <lg_chko_agit>.
    ASSERT sy-subrc = 0.

    super->zif_abapgit_object~serialize( io_xml = io_xml ).

  ENDMETHOD.
ENDCLASS.
