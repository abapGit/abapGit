*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_ENHO
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_object_enho DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_enho DEFINITION INHERITING FROM lcl_objects_super FINAL.
* For complete list of tool_type - see ENHTOOLS table
  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_spaces,
             full_name TYPE string.
    TYPES: spaces TYPE STANDARD TABLE OF i WITH DEFAULT KEY,
           END OF ty_spaces.

    TYPES: ty_spaces_tt TYPE STANDARD TABLE OF ty_spaces WITH DEFAULT KEY.

    METHODS deserialize_badi
      IMPORTING io_xml     TYPE REF TO lcl_xml_input
                iv_package TYPE devclass
      RAISING   lcx_exception.
    METHODS deserialize_hook
      IMPORTING io_xml     TYPE REF TO lcl_xml_input
                iv_package TYPE devclass
      RAISING   lcx_exception.

    METHODS serialize_badi
      IMPORTING io_xml      TYPE REF TO lcl_xml_output
                iv_tool     TYPE enhtooltype
                ii_enh_tool TYPE REF TO if_enh_tool
      RAISING   lcx_exception.
    METHODS serialize_hook
      IMPORTING io_xml      TYPE REF TO lcl_xml_output
                iv_tool     TYPE enhtooltype
                ii_enh_tool TYPE REF TO if_enh_tool
      RAISING   lcx_exception.

    METHODS hook_impl_serialize
      EXPORTING et_spaces TYPE ty_spaces_tt
      CHANGING  ct_impl   TYPE enh_hook_impl_it
      RAISING   lcx_exception.
    METHODS hook_impl_deserialize
      IMPORTING it_spaces TYPE ty_spaces_tt
      CHANGING  ct_impl   TYPE enh_hook_impl_it
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_enho DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_enho IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_enho IMPLEMENTATION.

  METHOD lif_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.  "lif_object~has_changed_since

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~changed_by.
    rv_user = c_user_unknown. " todo
  ENDMETHOD.

  METHOD hook_impl_serialize.
* handle normalization of XML values
* i.e. remove leading spaces

    FIELD-SYMBOLS: <ls_impl>  LIKE LINE OF ct_impl,
                   <ls_space> LIKE LINE OF et_spaces,
                   <lv_space> TYPE i,
                   <lv_line>  TYPE string.


    LOOP AT ct_impl ASSIGNING <ls_impl>.
      APPEND INITIAL LINE TO et_spaces ASSIGNING <ls_space>.
      <ls_space>-full_name = <ls_impl>-full_name.
      LOOP AT <ls_impl>-source ASSIGNING <lv_line>.
        APPEND INITIAL LINE TO <ls_space>-spaces ASSIGNING <lv_space>.
        WHILE strlen( <lv_line> ) >= 1 AND <lv_line>(1) = ` `.
          <lv_line> = <lv_line>+1.
          <lv_space> = <lv_space> + 1.
        ENDWHILE.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD hook_impl_deserialize.

    FIELD-SYMBOLS: <ls_impl>   LIKE LINE OF ct_impl,
                   <lv_line>   TYPE string,
                   <lv_space>  TYPE i,
                   <ls_spaces> LIKE LINE OF it_spaces.


    LOOP AT ct_impl ASSIGNING <ls_impl>.
      READ TABLE it_spaces ASSIGNING <ls_spaces> WITH KEY full_name = <ls_impl>-full_name.
      IF sy-subrc = 0.
        LOOP AT <ls_impl>-source ASSIGNING <lv_line>.
          READ TABLE <ls_spaces>-spaces ASSIGNING <lv_space> INDEX sy-tabix.
          IF sy-subrc = 0 AND <lv_space> > 0.
            DO <lv_space> TIMES.
              CONCATENATE space <lv_line> INTO <lv_line> RESPECTING BLANKS.
            ENDDO.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD lif_object~exists.

    DATA: ls_tadir TYPE tadir.

* todo, it should look up in the ENHO database tables or call some methods
* to see if the object exists, looking in TADIR will not work
    ls_tadir = lcl_tadir=>read_single(
      iv_object   = ms_item-obj_type
      iv_obj_name = ms_item-obj_name ).
    IF ls_tadir IS NOT INITIAL.
      rv_bool = abap_true.
    ENDIF.
  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~serialize.

    DATA: lv_enh_id   TYPE enhname,
          lv_tool     TYPE enhtooltype,
          li_enh_tool TYPE REF TO if_enh_tool.


    IF lif_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lv_enh_id = ms_item-obj_name.
    TRY.
        li_enh_tool = cl_enh_factory=>get_enhancement(
          enhancement_id   = lv_enh_id
          bypassing_buffer = abap_true ).
      CATCH cx_enh_root.
        lcx_exception=>raise( 'Error from CL_ENH_FACTORY' ).
    ENDTRY.
    lv_tool = li_enh_tool->get_tool( ).

    CASE lv_tool.
      WHEN cl_enh_tool_badi_impl=>tooltype.
        serialize_badi( io_xml = io_xml
                        iv_tool = lv_tool
                        ii_enh_tool = li_enh_tool ).
      WHEN cl_enh_tool_hook_impl=>tooltype.
        serialize_hook( io_xml = io_xml
                        iv_tool = lv_tool
                        ii_enh_tool = li_enh_tool ).
* ToDo:
*      WHEN cl_enh_tool_class=>tooltype.
*      WHEN 'ENHFUGRDATA'. "cl_enh_tool_fugr
*      WHEN cl_enh_tool_intf=>tooltype.
*      WHEN cl_wdr_cfg_enhancement=>tooltype.
*      WHEN 'ENHWDYN'. "cl_enh_tool_wdy
      WHEN OTHERS.
        lcx_exception=>raise( |Unsupported ENHO type { lv_tool }| ).
    ENDCASE.

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: lv_tool TYPE enhtooltype.

    IF lif_object~exists( ) = abap_true.
      lif_object~delete( ).
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'TOOL'
                  CHANGING cg_data = lv_tool ).

    CASE lv_tool.
      WHEN cl_enh_tool_badi_impl=>tooltype.
        deserialize_badi( io_xml     = io_xml
                          iv_package = iv_package ).
      WHEN cl_enh_tool_hook_impl=>tooltype.
        deserialize_hook( io_xml     = io_xml
                          iv_package = iv_package ).
* ToDo:
*      WHEN cl_enh_tool_class=>tooltype.
*      WHEN 'ENHFUGRDATA'. "cl_enh_tool_fugr
*      WHEN cl_enh_tool_intf=>tooltype.
*      WHEN cl_wdr_cfg_enhancement=>tooltype.
*      WHEN 'ENHWDYN'. "cl_enh_tool_wdy
      WHEN OTHERS.
        lcx_exception=>raise( |Unsupported ENHO type { lv_tool }| ).
    ENDCASE.

    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

  METHOD deserialize_badi.

    DATA: lv_spot_name TYPE enhspotname,
          lv_shorttext TYPE string,
          lv_enhname   TYPE enhname,
          lo_badi      TYPE REF TO cl_enh_tool_badi_impl,
          li_tool      TYPE REF TO if_enh_tool,
          lv_package   TYPE devclass,
          lt_impl      TYPE enh_badi_impl_data_it.

    FIELD-SYMBOLS: <ls_impl> LIKE LINE OF lt_impl.


    io_xml->read( EXPORTING iv_name = 'SHORTTEXT'
                  CHANGING cg_data = lv_shorttext ).
    io_xml->read( EXPORTING iv_name = 'SPOT_NAME'
                  CHANGING cg_data = lv_spot_name ).
    io_xml->read( EXPORTING iv_name = 'IMPL'
                  CHANGING cg_data = lt_impl ).

    lv_enhname = ms_item-obj_name.
    lv_package = iv_package.
    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname       = lv_enhname
            enhtype       = cl_abstract_enh_tool_redef=>credefinition
            enhtooltype   = cl_enh_tool_badi_impl=>tooltype
          IMPORTING
            enhancement   = li_tool
          CHANGING
            devclass      = lv_package ).
        lo_badi ?= li_tool.

        lo_badi->set_spot_name( lv_spot_name ).
        lo_badi->if_enh_object_docu~set_shorttext( lv_shorttext ).
        LOOP AT lt_impl ASSIGNING <ls_impl>.
          lo_badi->add_implementation( <ls_impl> ).
        ENDLOOP.
        lo_badi->if_enh_object~save( ).
        lo_badi->if_enh_object~unlock( ).
      CATCH cx_enh_root.
        lcx_exception=>raise( 'error deserializing ENHO badi' ).
    ENDTRY.

  ENDMETHOD.                    "deserialize_badi

  METHOD deserialize_hook.

    DATA: lv_shorttext       TYPE string,
          lo_hook_impl       TYPE REF TO cl_enh_tool_hook_impl,
          li_tool            TYPE REF TO if_enh_tool,
          lv_enhname         TYPE enhname,
          lv_package         TYPE devclass,
          ls_original_object TYPE enh_hook_admin,
          lt_spaces          TYPE ty_spaces_tt,
          lt_enhancements    TYPE enh_hook_impl_it.

    FIELD-SYMBOLS: <ls_enhancement> LIKE LINE OF lt_enhancements.


    io_xml->read( EXPORTING iv_name = 'SHORTTEXT'
                  CHANGING cg_data = lv_shorttext ).
    io_xml->read( EXPORTING iv_name = 'ORIGINAL_OBJECT'
                  CHANGING cg_data = ls_original_object ).
    io_xml->read( EXPORTING iv_name = 'ENHANCEMENTS'
                  CHANGING cg_data = lt_enhancements ).
    io_xml->read( EXPORTING iv_name = 'SPACES'
                  CHANGING cg_data = lt_spaces ).

    hook_impl_deserialize( EXPORTING it_spaces = lt_spaces
                           CHANGING ct_impl = lt_enhancements ).

    lv_enhname = ms_item-obj_name.
    lv_package = iv_package.
    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname       = lv_enhname
            enhtype       = cl_abstract_enh_tool_redef=>credefinition
            enhtooltype   = cl_enh_tool_hook_impl=>tooltype
          IMPORTING
            enhancement   = li_tool
          CHANGING
            devclass      = lv_package ).
        lo_hook_impl ?= li_tool.

        lo_hook_impl->if_enh_object_docu~set_shorttext( lv_shorttext ).
        lo_hook_impl->set_original_object(
            pgmid       = ls_original_object-pgmid
            obj_name    = ls_original_object-org_obj_name
            obj_type    = ls_original_object-org_obj_type
            program     = ls_original_object-programname
            main_type   = ls_original_object-org_main_type
            main_name   = ls_original_object-org_main_name ).
        lo_hook_impl->set_include_bound( ls_original_object-include_bound ).

        LOOP AT lt_enhancements ASSIGNING <ls_enhancement>.
          lo_hook_impl->add_hook_impl(
              overwrite                 = <ls_enhancement>-overwrite
              method                    = <ls_enhancement>-method
              enhmode                   = <ls_enhancement>-enhmode
              full_name                 = <ls_enhancement>-full_name
              source                    = <ls_enhancement>-source
              spot                      = <ls_enhancement>-spotname
              parent_full_name          = <ls_enhancement>-parent_full_name ).
        ENDLOOP.
        lo_hook_impl->if_enh_object~save( ).
        lo_hook_impl->if_enh_object~unlock( ).
      CATCH cx_enh_root.
        lcx_exception=>raise( 'error deserializing ENHO hook' ).
    ENDTRY.

  ENDMETHOD.                    "deserialize_hook

  METHOD serialize_badi.

    DATA: lo_badi_impl TYPE REF TO cl_enh_tool_badi_impl,
          lv_spot_name TYPE enhspotname,
          lv_shorttext TYPE string,
          lt_impl      TYPE enh_badi_impl_data_it.

    lo_badi_impl ?= ii_enh_tool.

    lv_shorttext = lo_badi_impl->if_enh_object_docu~get_shorttext( ).
    lv_spot_name = lo_badi_impl->get_spot_name( ).
    lt_impl      = lo_badi_impl->get_implementations( ).

    io_xml->add( iv_name = 'TOOL'
                 ig_data = iv_tool ).
    io_xml->add( ig_data = lv_shorttext
                 iv_name = 'SHORTTEXT' ).
    io_xml->add( iv_name = 'SPOT_NAME'
                 ig_data = lv_spot_name ).
    io_xml->add( iv_name = 'IMPL'
                 ig_data = lt_impl ).

  ENDMETHOD.                    "serialize_badi

  METHOD serialize_hook.

    DATA: lv_shorttext       TYPE string,
          lo_hook_impl       TYPE REF TO cl_enh_tool_hook_impl,
          ls_original_object TYPE enh_hook_admin,
          lt_spaces          TYPE ty_spaces_tt,
          lt_enhancements    TYPE enh_hook_impl_it.


    lo_hook_impl ?= ii_enh_tool.

    lv_shorttext = lo_hook_impl->if_enh_object_docu~get_shorttext( ).
    lo_hook_impl->get_original_object(
      IMPORTING
        pgmid     = ls_original_object-pgmid
        obj_name  = ls_original_object-org_obj_name
        obj_type  = ls_original_object-org_obj_type
        main_type = ls_original_object-org_main_type
        main_name = ls_original_object-org_main_name
        program   = ls_original_object-programname ).
    ls_original_object-include_bound = lo_hook_impl->get_include_bound( ).
    lt_enhancements = lo_hook_impl->get_hook_impls( ).

    hook_impl_serialize(
      IMPORTING et_spaces = lt_spaces
      CHANGING ct_impl = lt_enhancements ).

    io_xml->add( iv_name = 'TOOL'
                 ig_data = iv_tool ).
    io_xml->add( ig_data = lv_shorttext
                 iv_name = 'SHORTTEXT' ).
    io_xml->add( ig_data = ls_original_object
                 iv_name = 'ORIGINAL_OBJECT' ).
    io_xml->add( iv_name = 'ENHANCEMENTS'
                 ig_data = lt_enhancements ).
    io_xml->add( iv_name = 'SPACES'
                 ig_data = lt_spaces ).

  ENDMETHOD.                    "serialize_hook

  METHOD lif_object~delete.

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
        lcx_exception=>raise( 'Error deleting ENHO' ).
    ENDTRY.

  ENDMETHOD.                    "delete

  METHOD lif_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'ENHO'
        in_new_window = abap_true.

  ENDMETHOD.                    "jump

ENDCLASS.                    "lcl_object_enho IMPLEMENTATION