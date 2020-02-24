CLASS zcl_abapgit_object_enho DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PROTECTED SECTION.
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



CLASS ZCL_ABAPGIT_OBJECT_ENHO IMPLEMENTATION.


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

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA: lv_enh_id   TYPE enhname,
          lt_log      TYPE enh_log_it,
          li_log_obj  TYPE REF TO if_enh_log,
          ls_enhlog   TYPE enhlog,
          lv_lines    TYPE i,
          lt_enhlog   TYPE STANDARD TABLE OF enhlog WITH DEFAULT KEY,
          li_enh_tool TYPE REF TO if_enh_tool.


    lv_enh_id = ms_item-obj_name.
    TRY.
        li_enh_tool = cl_enh_factory=>get_enhancement(
          enhancement_id   = lv_enh_id
          bypassing_buffer = abap_true ).
      CATCH cx_enh_root.
        rv_user = c_user_unknown.
        RETURN.
    ENDTRY.

    lt_log = li_enh_tool->get_log( ).

    LOOP AT lt_log INTO li_log_obj.
      ls_enhlog = li_log_obj->get_enhlog( ).
      APPEND ls_enhlog TO lt_enhlog.
    ENDLOOP.

    lv_lines = lines( lt_enhlog ).
    READ TABLE lt_enhlog INTO ls_enhlog INDEX lv_lines.
    IF sy-subrc = 0.
      rv_user = ls_enhlog-loguser.
    ELSE.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lv_enh_id     TYPE enhname,
          li_enh_object TYPE REF TO if_enh_object.

    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lv_enh_id = ms_item-obj_name.
    TRY.
        li_enh_object = cl_enh_factory=>get_enhancement(
          enhancement_id = lv_enh_id
          lock           = abap_true ).
        li_enh_object->delete( ).
        li_enh_object->save( run_dark = abap_true ).
        li_enh_object->unlock( ).
      CATCH cx_enh_root.
        zcx_abapgit_exception=>raise( 'Error deleting ENHO' ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lv_tool TYPE enhtooltype,
          li_enho TYPE REF TO zif_abapgit_object_enho.


    IF zif_abapgit_object~exists( ) = abap_true.
      zif_abapgit_object~delete( iv_package ).
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'TOOL'
                  CHANGING cg_data = lv_tool ).

    li_enho = factory( lv_tool ).

    li_enho->deserialize( io_xml     = io_xml
                          iv_package = iv_package ).

    zcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.


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

    DATA: lv_object TYPE seqg3-garg.

    lv_object = |{ ms_item-obj_type }{ ms_item-obj_name }|.
    OVERLAY lv_object WITH '                                          '.
    lv_object = lv_object && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'E_ENHANCE'
                                            iv_argument    = lv_object ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'ENHO'
        in_new_window = abap_true.

  ENDMETHOD.


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

  ENDMETHOD.
ENDCLASS.
