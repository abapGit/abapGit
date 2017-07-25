*&---------------------------------------------------------------------*
*&  Include  zabapgit_object_prag
*&---------------------------------------------------------------------*

CLASS lcl_object_prag DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.

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
          lcx_exception,

      _raise_pragma_exists
        RAISING
          lcx_exception,

      _raise_pragma_enqueue
        RAISING
          lcx_exception.

ENDCLASS.

CLASS lcl_object_prag IMPLEMENTATION.

  METHOD lif_object~has_changed_since.

    rv_changed = abap_true.

  ENDMETHOD.

  METHOD lif_object~changed_by.

    rv_user = c_user_unknown.

  ENDMETHOD.

  METHOD lif_object~get_metadata.

    rs_metadata = get_metadata( ).
    rs_metadata-delete_tadir = abap_true.

  ENDMETHOD.

  METHOD lif_object~exists.

    TRY.
        cl_abap_pragma=>get_ref( ms_item-obj_name ).

      CATCH cx_abap_pragma_not_exists.
        rv_bool = abap_false.
        RETURN.
    ENDTRY.

    rv_bool = abap_true.

  ENDMETHOD.

  METHOD lif_object~serialize.

    DATA: lo_pragma TYPE REF TO cl_abap_pragma,
          pragma    TYPE lcl_object_prag=>ty_pragma.

    TRY.
        lo_pragma = cl_abap_pragma=>get_ref( ms_item-obj_name ).

        pragma-pragma      = lo_pragma->pragma.
        pragma-extension   = lo_pragma->extension.
        pragma-signature   = lo_pragma->signature.
        pragma-description = lo_pragma->description.

        io_xml->add( iv_name = 'PRAG'
                     ig_data = pragma ).

      CATCH cx_abap_pragma_not_exists.
        _raise_pragma_not_exists( ).
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~deserialize.

    DATA: pragma    TYPE ty_pragma,
          lo_pragma TYPE REF TO cl_abap_pragma.

    TRY.
        io_xml->read(
          EXPORTING
            iv_name = 'PRAG'
          CHANGING
            cg_data = pragma ).

        lo_pragma = cl_abap_pragma=>create( p_pragma  = ms_item-obj_name
                                            p_package = iv_package ).

        lo_pragma->set_info( p_description = pragma-description
                             p_signature   = pragma-signature
                             p_extension   = pragma-extension ).

        lo_pragma->save( ).

      CATCH cx_abap_pragma_not_exists.
        _raise_pragma_not_exists( ).
      CATCH cx_abap_pragma_exists.
        _raise_pragma_exists( ).
      CATCH cx_abap_pragma_enqueue.
        _raise_pragma_enqueue( ).
    ENDTRY.

  ENDMETHOD.

  METHOD lif_object~delete.

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

  METHOD lif_object~jump.

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

  METHOD lif_object~compare_to_remote_version.

    CREATE OBJECT ro_comparison_result TYPE lcl_comparison_null.

  ENDMETHOD.

  METHOD _raise_pragma_enqueue.

    lcx_exception=>raise( |Pragma { ms_item-obj_name } enqueue error| ).

  ENDMETHOD.

  METHOD _raise_pragma_exists.

    lcx_exception=>raise( |Pragma { ms_item-obj_name } exists| ).

  ENDMETHOD.

  METHOD _raise_pragma_not_exists.

    lcx_exception=>raise( |Pragma { ms_item-obj_name } doesn't exist| ).

  ENDMETHOD.

ENDCLASS.
