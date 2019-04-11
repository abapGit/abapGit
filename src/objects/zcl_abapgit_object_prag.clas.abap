CLASS zcl_abapgit_object_prag DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_pragma,
             pragma      TYPE c LENGTH 40,
             extension   TYPE c LENGTH 1,
             signature   TYPE c LENGTH 10,
             description TYPE c LENGTH 255,
           END OF ty_pragma.

    METHODS:
      _raise_pragma_not_exists
        RAISING
          zcx_abapgit_exception,

      _raise_pragma_exists
        RAISING
          zcx_abapgit_exception,

      _raise_pragma_enqueue
        RAISING
          zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_PRAG IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.

    rv_user = c_user_unknown.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lo_pragma TYPE REF TO cl_abap_pragma.

    TRY.
        lo_pragma = cl_abap_pragma=>get_ref( ms_item-obj_name ).

        lo_pragma->delete( ).

      CATCH cx_abap_pragma_not_exists.
        _raise_pragma_not_exists( ).
      CATCH cx_abap_pragma_enqueue.
        _raise_pragma_enqueue( ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_pragma TYPE ty_pragma,
          lo_pragma TYPE REF TO cl_abap_pragma.

    TRY.
        io_xml->read(
          EXPORTING
            iv_name = 'PRAG'
          CHANGING
            cg_data = ls_pragma ).

        lo_pragma = cl_abap_pragma=>create( p_pragma  = ms_item-obj_name
                                            p_package = iv_package ).

        lo_pragma->set_info( p_description = ls_pragma-description
                             p_signature   = ls_pragma-signature
                             p_extension   = ls_pragma-extension ).

        lo_pragma->save( ).

      CATCH cx_abap_pragma_not_exists.
        _raise_pragma_not_exists( ).
      CATCH cx_abap_pragma_exists.
        _raise_pragma_exists( ).
      CATCH cx_abap_pragma_enqueue.
        _raise_pragma_enqueue( ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    TRY.
        cl_abap_pragma=>get_ref( ms_item-obj_name ).

      CATCH cx_abap_pragma_not_exists.
        rv_bool = abap_false.
        RETURN.
    ENDTRY.

    rv_bool = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.

    rs_metadata = get_metadata( ).
    rs_metadata-delete_tadir = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = ms_item-obj_name
        object_type         = ms_item-obj_type
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lo_pragma TYPE REF TO cl_abap_pragma,
          ls_pragma TYPE zcl_abapgit_object_prag=>ty_pragma.

    TRY.
        lo_pragma = cl_abap_pragma=>get_ref( ms_item-obj_name ).

        ls_pragma-pragma      = lo_pragma->pragma.
        ls_pragma-extension   = lo_pragma->extension.
        ls_pragma-signature   = lo_pragma->signature.
        ls_pragma-description = lo_pragma->description.

        io_xml->add( iv_name = 'PRAG'
                     ig_data = ls_pragma ).

      CATCH cx_abap_pragma_not_exists.
        _raise_pragma_not_exists( ).
    ENDTRY.

  ENDMETHOD.


  METHOD _raise_pragma_enqueue.
    zcx_abapgit_exception=>raise( |Pragma { ms_item-obj_name } enqueue error| ).
  ENDMETHOD.


  METHOD _raise_pragma_exists.
    zcx_abapgit_exception=>raise( |Pragma { ms_item-obj_name } exists| ).
  ENDMETHOD.


  METHOD _raise_pragma_not_exists.
    zcx_abapgit_exception=>raise( |Pragma { ms_item-obj_name } doesn't exist| ).
  ENDMETHOD.
ENDCLASS.
