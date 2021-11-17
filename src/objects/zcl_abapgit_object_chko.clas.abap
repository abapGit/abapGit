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

    DATA  lr_data     TYPE REF TO data.
    DATA  chko_db_api TYPE REF TO object.
    DATA  name        TYPE c LENGTH 30.

    FIELD-SYMBOLS <chko_header> TYPE any.
    FIELD-SYMBOLS <chko_user>   TYPE any.

    CREATE OBJECT chko_db_api TYPE ('CL_CHKO_DB_API').

    CREATE DATA lr_data TYPE ('CL_CHKO_DB_API=>TY_HEADER').
    ASSIGN lr_data->* TO <chko_header>.

    IF <chko_header> IS NOT ASSIGNED OR chko_db_api IS NOT BOUND.
      RETURN.
    ENDIF.

    name = ms_item-obj_name.

    CALL METHOD chko_db_api->('GET_HEADER')
      EXPORTING
        name    = name
        version = 'I'
      RECEIVING
        header  = <chko_header>.

    IF <chko_header> IS INITIAL.
      CALL METHOD chko_db_api->('GET_HEADER')
        EXPORTING
          name    = name
          version = 'A'
        RECEIVING
          header  = <chko_header>.
    ENDIF.

    ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <chko_header> TO <chko_user>.
    rv_user = <chko_user>.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA  chko_db_api TYPE REF TO object.
    DATA  name TYPE c LENGTH 30.

    name = ms_item-obj_name.

    CREATE OBJECT chko_db_api TYPE ('CL_CHKO_DB_API').
    CHECK chko_db_api IS BOUND.

    CALL METHOD chko_db_api->('DELETE')
      EXPORTING
        name    = name
        version = 'I'.

    CALL METHOD chko_db_api->('DELETE')
      EXPORTING
        name    = name
        version = 'A'.
  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA object       TYPE trkey.
    DATA json_xstring TYPE xstring.
    DATA object_chko  TYPE trkey.
    DATA lr_chko      TYPE REF TO data.

    DATA object_ajson    TYPE REF TO object.
    DATA persistence     TYPE REF TO lcl_chko_persistence.
    DATA json_as_xstring TYPE xstring.
    DATA exception       TYPE REF TO cx_static_check.

    FIELD-SYMBOLS <chko_agit> TYPE any.

    CREATE DATA lr_chko TYPE ('ZIF_ABAPGIT_AFF_CHKO_V1=>TY_MAIN').
    ASSIGN lr_chko->* TO <chko_agit>.

    IF <chko_agit> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    CREATE OBJECT object_ajson TYPE ('ZCL_ABAPGIT_AJSON_CNT_HANDLER').

    json_as_xstring = mo_files->read_raw( iv_ext = 'json' ) ##NO_TEXT.

    object-devclass = ms_item-devclass.
    object-obj_type = ms_item-obj_type.
    object-obj_name = ms_item-obj_name.

    TRY.
        CALL METHOD object_ajson->('DESERIALIZE')
          EXPORTING
            content = json_as_xstring
          IMPORTING
            data    = <chko_agit>.

        CREATE OBJECT persistence.
        persistence->save_content(
           data     = <chko_agit>
           object   = object
           language = mv_language
           version  = 'I'
           saved_by = sy-uname ).
      CATCH cx_static_check INTO exception.
        " to do: is this the right exception handling?
        ii_log->add_exception(
            ix_exc  = exception
            is_item = ms_item ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA chko_db_api TYPE REF TO object.
    DATA name TYPE c LENGTH 30.

    name = ms_item-obj_name .
    CREATE OBJECT chko_db_api TYPE ('CL_CHKO_DB_API').

    CHECK chko_db_api IS BOUND.

    CALL METHOD chko_db_api->('EXISTS')
      EXPORTING
        name   = name
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
    DATA argument    TYPE seqg3-garg.

    lock_object = |{ ms_item-obj_type }{ ms_item-obj_name }*|.
    argument = lock_object.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESWB_EO'
                                            iv_argument    = argument  ).
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.
    DATA json_xstring TYPE xstring.
    DATA object_chko  TYPE trkey.
    DATA lr_chko      TYPE REF TO data.

    DATA object_ajson TYPE REF TO object.
    DATA persistence  TYPE REF TO lcl_chko_persistence.
    DATA exception    TYPE REF TO cx_root.

    FIELD-SYMBOLS <chko_agit> TYPE any.

    CREATE DATA lr_chko TYPE ('ZIF_ABAPGIT_AFF_CHKO_V1=>TY_MAIN').
    ASSIGN lr_chko->* TO <chko_agit>.

    IF <chko_agit> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    CREATE OBJECT object_ajson TYPE ('ZCL_ABAPGIT_AJSON_CNT_HANDLER').

    object_chko-devclass = ms_item-devclass.
    object_chko-obj_type = ms_item-obj_type.
    object_chko-obj_name = ms_item-obj_name.

    TRY.
        CREATE OBJECT persistence.
        persistence->get_content(
          EXPORTING
            object   = object_chko
            language = mv_language
            version  = 'A'
          IMPORTING
            data     = <chko_agit> ).

        CALL METHOD object_ajson->('SERIALIZE')
          EXPORTING
            data   = <chko_agit>
          RECEIVING
            result = json_xstring.

        mo_files->add_raw( iv_ext = 'json' iv_data = json_xstring ).


      CATCH cx_root INTO exception.
        zcx_abapgit_exception=>raise_with_text( exception ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
