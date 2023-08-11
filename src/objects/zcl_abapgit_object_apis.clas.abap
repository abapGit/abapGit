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
          iv_language TYPE spras.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_state,
             sub_object_type           TYPE c LENGTH 20,
             sub_object_name           TYPE c LENGTH 120,
             compatibility_contract    TYPE c LENGTH 2,
             release_state             TYPE c LENGTH 40,
             use_in_key_user_apps      TYPE c LENGTH 1,
             use_in_sap_cloud_platform TYPE c LENGTH 1,
           END OF ty_state.

    TYPES: BEGIN OF ty_apis,
             object_id   TYPE c LENGTH 36,
             object_type TYPE tadir-object,
             object_name TYPE tadir-obj_name,
             api_states  TYPE STANDARD TABLE OF ty_state WITH DEFAULT KEY,
           END OF ty_apis.

    DATA mo_handler TYPE REF TO object.

    METHODS initialize.

ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_APIS IMPLEMENTATION.


  METHOD constructor.
    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

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

    DATA lx_error TYPE REF TO cx_static_check.

    initialize( ).

    TRY.
        CALL METHOD mo_handler->('IF_ARS_API_ABAPGIT~DELETE_API_STATE')
          EXPORTING
            iv_request = iv_transport.
      CATCH cx_static_check INTO lx_error.
        RAISE EXCEPTION TYPE zcx_abapgit_exception
          EXPORTING
            previous = lx_error.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    ASSERT 1 = 'todo'.

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
    ASSERT 1 = 'todo'.
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

    DATA ls_apis TYPE ty_apis.

    initialize( ).

    CALL METHOD mo_handler->('IF_ARS_API_ABAPGIT~GET_API_STATE')
      RECEIVING
        rs_apis_object = ls_apis.

    io_xml->add( iv_name = 'APIS'
                 ig_data = ls_apis ).

  ENDMETHOD.


  METHOD initialize.

    IF mo_handler IS NOT BOUND.
      CREATE OBJECT mo_handler TYPE ('CL_ARS_API_ABAPGIT')
        EXPORTING
          iv_api_object_name = ms_item-obj_name.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
