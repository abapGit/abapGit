CLASS zcl_abapgit_object_enho DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PRIVATE SECTION.

    METHODS:
      factory
        IMPORTING
          iv_tool        TYPE enhtooltype
        RETURNING
          VALUE(ri_enho) TYPE REF TO zif_abapgit_object_enho
        RAISING
          zcx_abapgit_exception.

ENDCLASS.

CLASS zcl_abapgit_object_enho IMPLEMENTATION.

  METHOD zif_abapgit_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.  "zif_abapgit_object~has_changed_since

  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "zif_abapgit_object~get_metadata

  METHOD zif_abapgit_object~changed_by.
    rv_user = c_user_unknown. " todo
  ENDMETHOD.                    "zif_abapgit_object~changed_by

  METHOD zif_abapgit_object~exists.

    DATA: lv_enh_id TYPE enhname.


    lv_enh_id = ms_item-obj_name.
    TRY.
        cl_enh_factory=>get_enhancement(
          enhancement_id   = lv_enh_id
          bypassing_buffer = abap_true ).
        rv_bool = abap_true.
      CATCH cx_enh_root.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.                    "zif_abapgit_object~exists

  METHOD zif_abapgit_object~serialize.

    DATA: lv_enh_id   TYPE enhname,
          li_enho     TYPE REF TO zif_abapgit_object_enho,
          li_enh_tool TYPE REF TO if_enh_tool.


    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lv_enh_id = ms_item-obj_name.
    TRY.
        li_enh_tool = cl_enh_factory=>get_enhancement(
          enhancement_id   = lv_enh_id
          bypassing_buffer = abap_true ).
      CATCH cx_enh_root.
        zcx_abapgit_exception=>raise( 'Error from CL_ENH_FACTORY' ).
    ENDTRY.

    li_enho = factory( li_enh_tool->get_tool( ) ).

    li_enho->serialize( io_xml      = io_xml
                        ii_enh_tool = li_enh_tool ).

  ENDMETHOD.                    "serialize

  METHOD factory.

    CASE iv_tool.
      WHEN cl_enh_tool_badi_impl=>tooltype.
        CREATE OBJECT ri_enho TYPE zcl_abapgit_object_enho_badi
          EXPORTING
            is_item  = ms_item
            io_files = mo_files.
      WHEN cl_enh_tool_hook_impl=>tooltype.
        CREATE OBJECT ri_enho TYPE zcl_abapgit_object_enho_hook
          EXPORTING
            is_item  = ms_item
            io_files = mo_files.
      WHEN cl_enh_tool_class=>tooltype.
        CREATE OBJECT ri_enho TYPE zcl_abapgit_object_enho_class
          EXPORTING
            is_item  = ms_item
            io_files = mo_files.
      WHEN cl_enh_tool_intf=>tooltype.
        CREATE OBJECT ri_enho TYPE zcl_abapgit_object_enho_intf
          EXPORTING
            is_item  = ms_item
            io_files = mo_files.
      WHEN cl_wdr_cfg_enhancement=>tooltype.
        CREATE OBJECT ri_enho TYPE zcl_abapgit_object_enho_wdyc
          EXPORTING
            is_item  = ms_item
            io_files = mo_files.
      WHEN 'FUGRENH'.
        CREATE OBJECT ri_enho TYPE zcl_abapgit_object_enho_fugr
          EXPORTING
            is_item  = ms_item
            io_files = mo_files.
      WHEN 'WDYENH'.
        CREATE OBJECT ri_enho TYPE zcl_abapgit_object_enho_wdyn
          EXPORTING
            is_item  = ms_item
            io_files = mo_files.
      WHEN OTHERS.
        zcx_abapgit_exception=>raise( |Unsupported ENHO type { iv_tool }| ).
    ENDCASE.

  ENDMETHOD.                    "factory

  METHOD zif_abapgit_object~deserialize.

    DATA: lv_tool TYPE enhtooltype,
          li_enho TYPE REF TO zif_abapgit_object_enho.


    IF zif_abapgit_object~exists( ) = abap_true.
      zif_abapgit_object~delete( ).
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'TOOL'
                  CHANGING cg_data = lv_tool ).

    li_enho = factory( lv_tool ).

    li_enho->deserialize( io_xml     = io_xml
                          iv_package = iv_package ).

    zcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

  METHOD zif_abapgit_object~delete.

    DATA: lv_enh_id     TYPE enhname,
          li_enh_object TYPE REF TO if_enh_object.


    lv_enh_id = ms_item-obj_name.
    TRY.
        li_enh_object = cl_enh_factory=>get_enhancement(
          enhancement_id = lv_enh_id
          lock           = abap_true ).
        li_enh_object->delete( ).
        li_enh_object->save( ).
        li_enh_object->unlock( ).
      CATCH cx_enh_root.
        zcx_abapgit_exception=>raise( 'Error deleting ENHO' ).
    ENDTRY.

  ENDMETHOD.                    "delete

  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'ENHO'
        in_new_window = abap_true.

  ENDMETHOD.                    "jump

  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
  ENDMETHOD.                    "zif_abapgit_object~compare_to_remote_version

ENDCLASS.                    "zcl_abapgit_object_enho IMPLEMENTATION
