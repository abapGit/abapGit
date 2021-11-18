CLASS zcl_abapgit_object_chko DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_CHKO IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.

    DATA  lr_data          TYPE REF TO data.
    DATA  lo_chko_db_api   TYPE REF TO object.
    DATA  lv_name          TYPE c LENGTH 30.

    FIELD-SYMBOLS <lg_chko_header> TYPE any.
    FIELD-SYMBOLS <lg_chko_user>   TYPE any.

    IF ms_item-obj_type <> 'CHKO'.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_chko_db_api TYPE ('CL_CHKO_DB_API').

    CREATE DATA lr_data TYPE ('CL_CHKO_DB_API=>TY_HEADER').
    ASSIGN lr_data->* TO <lg_chko_header>.

    IF <lg_chko_header> IS NOT ASSIGNED OR lo_chko_db_api IS NOT BOUND.
      RETURN.
    ENDIF.

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

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA  lo_chko_db_api TYPE REF TO object.
    DATA  lv_name        TYPE c LENGTH 30.

    IF ms_item-obj_type <> 'CHKO'.
      RETURN.
    ENDIF.

    lv_name = ms_item-obj_name.

    CREATE OBJECT lo_chko_db_api TYPE ('CL_CHKO_DB_API').
    IF lo_chko_db_api IS NOT BOUND.
      RETURN.
    ENDIF.

    CALL METHOD lo_chko_db_api->('DELETE')
      EXPORTING
        name    = lv_name
        version = 'I'.

    CALL METHOD lo_chko_db_api->('DELETE')
      EXPORTING
        name    = lv_name
        version = 'A'.
  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA lv_object            TYPE trkey.
    DATA lr_chko              TYPE REF TO data.
    DATA lo_ajson             TYPE REF TO object.
    DATA lo_persistence       TYPE REF TO lcl_chko_persistence.
    DATA lv_json_as_xstring   TYPE xstring.
    DATA lx_exception         TYPE REF TO cx_static_check.

    FIELD-SYMBOLS <lg_chko_agit> TYPE any.

    IF ms_item-obj_type <> 'CHKO'.
      RETURN.
    ENDIF.

    CREATE DATA lr_chko TYPE ('ZIF_ABAPGIT_AFF_CHKO_V1=>TY_MAIN').
    ASSIGN lr_chko->* TO <lg_chko_agit>.

    IF <lg_chko_agit> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_ajson TYPE ('ZCL_ABAPGIT_AJSON_CNT_HANDLER').

    lv_json_as_xstring = mo_files->read_raw( iv_ext = 'json' ).

    lv_object-devclass = ms_item-devclass.
    lv_object-obj_type = ms_item-obj_type.
    lv_object-obj_name = ms_item-obj_name.

    TRY.
        CALL METHOD lo_ajson->('DESERIALIZE')
          EXPORTING
            iv_content = lv_json_as_xstring
          IMPORTING
            ev_data    = <lg_chko_agit>.

        CREATE OBJECT lo_persistence.
        lo_persistence->save_content(
           iv_data     = <lg_chko_agit>
           iv_object   = lv_object
           iv_language = mv_language
           iv_version  = 'I'
           iv_saved_by = sy-uname ).
      CATCH cx_static_check INTO lx_exception.
        " to do: is this the right exception handling?
        ii_log->add_exception(
            ix_exc  = lx_exception
            is_item = ms_item ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA lo_chko_db_api TYPE REF TO object.
    DATA lv_name        TYPE c LENGTH 30.

    IF ms_item-obj_type <> 'CHKO'.
      RETURN.
    ENDIF.

    lv_name = ms_item-obj_name.
    CREATE OBJECT lo_chko_db_api TYPE ('CL_CHKO_DB_API').

    IF lo_chko_db_api IS NOT BOUND.
      RETURN.
    ENDIF.

    CALL METHOD lo_chko_db_api->('EXISTS')
      EXPORTING
        name   = lv_name
      RECEIVING
        exists = rv_bool.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    DATA lv_lock_object   TYPE string.
    DATA lv_argument      TYPE seqg3-garg.

    lv_lock_object = |{ ms_item-obj_type }{ ms_item-obj_name }*|.
    lv_argument  = lv_lock_object.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESWB_EO'
                                            iv_argument    = lv_argument  ).
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.
    DATA lv_json_xstring TYPE xstring.
    DATA lv_object_chko  TYPE trkey.
    DATA lr_chko         TYPE REF TO data.
    DATA lo_ajson        TYPE REF TO object.
    DATA lo_persistence  TYPE REF TO lcl_chko_persistence.
    DATA lx_exception    TYPE REF TO cx_root.

    FIELD-SYMBOLS <lg_chko_agit> TYPE any.

    IF ms_item-obj_type <> 'CHKO'.
      RETURN.
    ENDIF.

    CREATE DATA lr_chko TYPE ('ZIF_ABAPGIT_AFF_CHKO_V1=>TY_MAIN').
    ASSIGN lr_chko->* TO <lg_chko_agit>.

    IF <lg_chko_agit> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_ajson TYPE ('ZCL_ABAPGIT_AJSON_CNT_HANDLER').

    lv_object_chko-devclass = ms_item-devclass.
    lv_object_chko-obj_type = ms_item-obj_type.
    lv_object_chko-obj_name = ms_item-obj_name.

    TRY.
        CREATE OBJECT lo_persistence.
        lo_persistence->get_content(
          EXPORTING
            iv_object   = lv_object_chko
            iv_language = mv_language
            iv_version  = 'A'
          IMPORTING
            ev_data     = <lg_chko_agit> ).

        CALL METHOD lo_ajson->('SERIALIZE')
          EXPORTING
            iv_data   = <lg_chko_agit>
          RECEIVING
            rv_result = lv_json_xstring.

        mo_files->add_raw( iv_ext = 'json'
                           iv_data = lv_json_xstring ).

      CATCH cx_root INTO lx_exception.
        zcx_abapgit_exception=>raise_with_text( lx_exception ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
