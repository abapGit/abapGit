CLASS zcl_abapgit_object_apis DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object.

    METHODS:
      constructor
        IMPORTING
          is_item     TYPE zif_abapgit_definitions=>ty_item
          iv_language TYPE spras
        RAISING
          zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS gc_model TYPE string VALUE 'ARS_S_API_ABAPGIT'.
    DATA mo_handler TYPE REF TO object.
    METHODS initialize.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_APIS IMPLEMENTATION.


  METHOD constructor.

    DATA mr_data TYPE REF TO data.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    TRY.
        CREATE DATA mr_data TYPE (gc_model).
      CATCH cx_sy_create_error.
        zcx_abapgit_exception=>raise( |APIS not supported by your NW release| ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    initialize( ).

    TRY.
        CALL METHOD mo_handler->('IF_ARS_API_ABAPGIT~GET_CHANGED_BY')
          RECEIVING
            rv_changed_by = rv_user.
      CATCH cx_root.
        rv_user = c_user_unknown.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

* IF_ARS_API_ABAPGIT~DELETE_API_STATE dumps and checks fail, even tho I as a developer can delete it

    DATA lo_db   TYPE REF TO object.
    DATA lr_data TYPE REF TO data.
    FIELD-SYMBOLS <ls_api_key> TYPE any.


    CREATE DATA lr_data TYPE ('IF_ARS_STATE_DB_ACCESS=>TY_S_API_KEY').
    ASSIGN lr_data->* TO <ls_api_key>.
    <ls_api_key> = ms_item-obj_name.
    ASSERT <ls_api_key> IS NOT INITIAL.
    BREAK-POINT.

    CALL METHOD cl_ars_state_db_access=>('GET_INSTANCE')
      RECEIVING
        ro_state_db_access = lo_db.

    CALL METHOD lo_db->('IF_ARS_STATE_DB_ACCESS~DELETE')
      EXPORTING
        is_api_key = <ls_api_key>.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA lr_data TYPE REF TO data.
    FIELD-SYMBOLS <ls_data> TYPE any.

    CREATE DATA lr_data TYPE (gc_model).
    ASSIGN lr_data->* TO <ls_data>.

    initialize( ).

    io_xml->read(
      EXPORTING
        iv_name = 'APIS'
      CHANGING
        cg_data = <ls_data> ).

    CALL METHOD mo_handler->('IF_ARS_API_ABAPGIT~SAVE_API_STATE')
      EXPORTING
        is_api_state = <ls_data>
        iv_package   = iv_package
        iv_request   = iv_transport.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    TRY.
        initialize( ).
        CALL METHOD mo_handler->('IF_ARS_API_ABAPGIT~CHECK_EXISTS')
          RECEIVING
            rv_api_exists = rv_bool.
      CATCH cx_root.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-late TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-version = 'v2.0.0'.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = zif_abapgit_object~exists( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
* looks like there is no enqueue lock
* E_ARS_API ?
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    " todo
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA lr_data TYPE REF TO data.
    FIELD-SYMBOLS <ls_data> TYPE any.

    CREATE DATA lr_data TYPE (gc_model).
    ASSIGN lr_data->* TO <ls_data>.

    initialize( ).

    CALL METHOD mo_handler->('IF_ARS_API_ABAPGIT~GET_API_STATE')
      RECEIVING
        rs_apis_object = <ls_data>.

    io_xml->add( iv_name = 'APIS'
                 ig_data = <ls_data> ).

  ENDMETHOD.


  METHOD initialize.

    IF mo_handler IS NOT BOUND.
      CREATE OBJECT mo_handler TYPE ('CL_ARS_API_ABAPGIT')
        EXPORTING
          iv_api_object_name = ms_item-obj_name.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
