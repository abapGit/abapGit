CLASS zcl_abapgit_object_oa2s DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_object,
        pgmid    TYPE tadir-pgmid,
        object   TYPE trobjtype,
        obj_name TYPE sobj_name,
      END OF ty_object.

    TYPES:
      BEGIN OF ty_scope,
        pgmid       TYPE tadir-pgmid,
        object      TYPE trobjtype,
        obj_name    TYPE sobj_name,
        description TYPE ddtext,
      END OF ty_scope.

    CONSTANTS c_manager TYPE seoclsname VALUE 'CL_OAUTH2_S_SCOPE_MANAGER'.

    METHODS read_object
      RETURNING
        VALUE(rs_object) TYPE ty_object
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS zcl_abapgit_object_oa2s IMPLEMENTATION.


  METHOD read_object.

    SELECT SINGLE pgmid object obj_name FROM ('OA2_SD_SC') INTO rs_object
      WHERE scope_id = ms_item-obj_name.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |OAuth 2.0 scope { ms_item-obj_name } not found| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE created_by FROM ('OA2_SD_SC') INTO rv_user
      WHERE scope_id = ms_item-obj_name.
    IF sy-subrc <> 0 OR rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA ls_object    TYPE ty_object.
    DATA lv_transport TYPE trkorr.
    DATA lx_error     TYPE REF TO cx_root.

    ls_object = read_object( ).
    lv_transport = iv_transport.

    TRY.
        CALL METHOD (c_manager)=>('DELETE_SCOPE_FROM_OBJECT')
          EXPORTING
            is_object              = ls_object
            i_no_dialog            = abap_true
          CHANGING
            c_transport_request_id = lv_transport.
      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA ls_scope     TYPE ty_scope.
    DATA ls_object    TYPE ty_object.
    DATA lv_transport TYPE trkorr.
    DATA lx_error     TYPE REF TO cx_root.

    io_xml->read( EXPORTING iv_name = 'OA2S'
                  CHANGING  cg_data = ls_scope ).

    MOVE-CORRESPONDING ls_scope TO ls_object.

    IF zif_abapgit_object~exists( ) = abap_true.
      IF read_object( ) = ls_object.
        RETURN.
      ENDIF.
      zif_abapgit_object~delete( iv_package   = iv_package
                                 iv_transport = iv_transport
                                 ii_log       = ii_log ).
    ENDIF.

    lv_transport = iv_transport.

    TRY.
        CALL METHOD (c_manager)=>('CREATE_SCOPE_FROM_OBJECT')
          EXPORTING
            is_object              = ls_object
            i_description          = ls_scope-description
            i_devclass             = iv_package
            i_language             = mv_language
          CHANGING
            c_transport_request_id = lv_transport.
      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    " the scope ID is derived from the assigned object
    IF zif_abapgit_object~exists( ) = abap_false.
      zcx_abapgit_exception=>raise( |Scope created for { ls_object-object } { ls_object-obj_name }|
        && | does not match OAuth 2.0 scope { ms_item-obj_name }| ).
    ENDIF.

    tadir_insert( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA lv_scope_id TYPE string.

    lv_scope_id = ms_item-obj_name.

    TRY.
        CALL METHOD (c_manager)=>('CHECK_SCOPE_EXIST')
          EXPORTING
            i_scope_id    = lv_scope_id
          RECEIVING
            r_scope_exist = rv_bool.
      CATCH cx_sy_dyn_call_error.
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
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA ls_scope  TYPE ty_scope.
    DATA ls_object TYPE ty_object.

    ls_object = read_object( ).
    MOVE-CORRESPONDING ls_object TO ls_scope.

    SELECT SINGLE description FROM ('OA2_SD_SCT') INTO ls_scope-description
      WHERE scope_id = ms_item-obj_name
      AND langu = mv_language.

    io_xml->add( iv_name = 'OA2S'
                 ig_data = ls_scope ).

  ENDMETHOD.
ENDCLASS.
