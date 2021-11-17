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
    DATA  lobj_chko_db_api TYPE REF TO object.
    DATA  l_name           TYPE c LENGTH 30.

    FIELD-SYMBOLS <chko_header> TYPE any.
    FIELD-SYMBOLS <chko_user>   TYPE any.

    CREATE OBJECT lobj_chko_db_api TYPE ('CL_CHKO_DB_API').

    CREATE DATA lr_data TYPE ('CL_CHKO_DB_API=>TY_HEADER').
    ASSIGN lr_data->* TO <chko_header>.

    IF <chko_header> IS NOT ASSIGNED OR lobj_chko_db_api IS NOT BOUND.
      RETURN.
    ENDIF.

    l_name = ms_item-obj_name.

    CALL METHOD lobj_chko_db_api->('GET_HEADER')
      EXPORTING
        name    = l_name
        version = 'I'
      RECEIVING
        header  = <chko_header>.

    IF <chko_header> IS INITIAL.
      CALL METHOD lobj_chko_db_api->('GET_HEADER')
        EXPORTING
          name    = l_name
          version = 'A'
        RECEIVING
          header  = <chko_header>.
    ENDIF.

    ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <chko_header> TO <chko_user>.
    rv_user = <chko_user>.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA  lobj_chko_db_api TYPE REF TO object.
    DATA  l_name TYPE      c LENGTH 30.

    l_name = ms_item-obj_name.

    CREATE OBJECT lobj_chko_db_api TYPE ('CL_CHKO_DB_API').
    IF lobj_chko_db_api IS NOT BOUND.
      RETURN.
    ENDIF.

    CALL METHOD lobj_chko_db_api->('DELETE')
      EXPORTING
        name    = l_name
        version = 'I'.

    CALL METHOD lobj_chko_db_api->('DELETE')
      EXPORTING
        name    = l_name
        version = 'A'.
  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA l_object       TYPE trkey.
    DATA lr_chko        TYPE REF TO data.

    DATA lobj_ajson     TYPE REF TO object.
    DATA lobj_persistence  TYPE REF TO lcl_chko_persistence.
    DATA json_as_xstring   TYPE xstring.
    DATA lobj_exception    TYPE REF TO cx_static_check.

    FIELD-SYMBOLS <chko_agit> TYPE any.

    CREATE DATA lr_chko TYPE ('ZIF_ABAPGIT_AFF_CHKO_V1=>TY_MAIN').
    ASSIGN lr_chko->* TO <chko_agit>.

    IF <chko_agit> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    CREATE OBJECT lobj_ajson TYPE ('ZCL_ABAPGIT_AJSON_CNT_HANDLER').

    json_as_xstring = mo_files->read_raw( iv_ext = 'json' ).

    l_object-devclass = ms_item-devclass.
    l_object-obj_type = ms_item-obj_type.
    l_object-obj_name = ms_item-obj_name.

    TRY.
        CALL METHOD lobj_ajson->('DESERIALIZE')
          EXPORTING
            i_content = json_as_xstring
          IMPORTING
            e_data    = <chko_agit>.

        CREATE OBJECT lobj_persistence.
        lobj_persistence->save_content(
           i_data     = <chko_agit>
           i_object   = l_object
           i_language = mv_language
           i_version  = 'I'
           i_saved_by = sy-uname ).
      CATCH cx_static_check INTO lobj_exception.
        " to do: is this the right exception handling?
        ii_log->add_exception(
            ix_exc  = lobj_exception
            is_item = ms_item ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA lobj_chko_db_api TYPE REF TO object.
    DATA l_name           TYPE c LENGTH 30.

    l_name = ms_item-obj_name.
    CREATE OBJECT lobj_chko_db_api TYPE ('CL_CHKO_DB_API').

    IF lobj_chko_db_api IS NOT BOUND.
      RETURN.
    ENDIF.

    CALL METHOD lobj_chko_db_api->('EXISTS')
      EXPORTING
        name   = l_name
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
    DATA lock_object TYPE string.
    DATA l_argument  TYPE seqg3-garg.

    lock_object = |{ ms_item-obj_type }{ ms_item-obj_name }*|.
    l_argument  = lock_object.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESWB_EO'
                                            iv_argument    = l_argument  ).
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.
    DATA ljson_xstring TYPE xstring.
    DATA lobject_chko  TYPE trkey.
    DATA lr_chko      TYPE REF TO data.

    DATA lobj_ajson     TYPE REF TO object.
    DATA lobj_persistence  TYPE REF TO lcl_chko_persistence.
    DATA lobj_exception    TYPE REF TO cx_root.

    FIELD-SYMBOLS <chko_agit> TYPE any.

    CREATE DATA lr_chko TYPE ('ZIF_ABAPGIT_AFF_CHKO_V1=>TY_MAIN').
    ASSIGN lr_chko->* TO <chko_agit>.

    IF <chko_agit> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    CREATE OBJECT lobj_ajson TYPE ('ZCL_ABAPGIT_AJSON_CNT_HANDLER').

    lobject_chko-devclass = ms_item-devclass.
    lobject_chko-obj_type = ms_item-obj_type.
    lobject_chko-obj_name = ms_item-obj_name.

    TRY.
        CREATE OBJECT lobj_persistence.
        lobj_persistence->get_content(
          EXPORTING
            i_object   = lobject_chko
            i_language = mv_language
            i_version  = 'A'
          IMPORTING
            e_data     = <chko_agit> ).

        CALL METHOD lobj_ajson->('SERIALIZE')
          EXPORTING
            i_data   = <chko_agit>
          RECEIVING
            r_result = ljson_xstring.

        mo_files->add_raw( iv_ext = 'json'
                           iv_data = ljson_xstring ).

      CATCH cx_root INTO lobj_exception.
        zcx_abapgit_exception=>raise_with_text( lobj_exception ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
