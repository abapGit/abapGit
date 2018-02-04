CLASS zcl_abapgit_object_enho_intf DEFINITION PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          is_item  TYPE zif_abapgit_definitions=>ty_item
          io_files TYPE REF TO zcl_abapgit_objects_files.
    INTERFACES: zif_abapgit_object_enho.

  PRIVATE SECTION.
    DATA: ms_item  TYPE zif_abapgit_definitions=>ty_item,
          mo_files TYPE REF TO zcl_abapgit_objects_files.

ENDCLASS.

CLASS zcl_abapgit_object_enho_intf IMPLEMENTATION.

  METHOD constructor.
    ms_item  = is_item.
    mo_files = io_files.
  ENDMETHOD.                    "constructor

  METHOD zif_abapgit_object_enho~serialize.

    DATA: lo_enh_intf  TYPE REF TO cl_enh_tool_intf,
          lv_class     TYPE seoclsname,
          lv_shorttext TYPE string.


    lo_enh_intf ?= ii_enh_tool.

    lv_shorttext = lo_enh_intf->if_enh_object_docu~get_shorttext( ).
    lo_enh_intf->get_class( IMPORTING class_name = lv_class ).

    io_xml->add( iv_name = 'TOOL'
                 ig_data = ii_enh_tool->get_tool( ) ).
    io_xml->add( ig_data = lv_shorttext
                 iv_name = 'SHORTTEXT' ).
    io_xml->add( iv_name = 'CLASS'
                 ig_data = lv_class ).

    zcl_abapgit_object_enho_clif=>serialize(
      io_xml  = io_xml
      io_files = mo_files
      io_clif = lo_enh_intf ).

  ENDMETHOD.                    "zif_abapgit_object_enho~serialize

  METHOD zif_abapgit_object_enho~deserialize.

    DATA: lo_enh_intf  TYPE REF TO cl_enh_tool_intf,
          li_tool      TYPE REF TO if_enh_tool,
          lv_shorttext TYPE string,
          lv_class     TYPE seoclsname,
          lv_enhname   TYPE enhname,
          lv_package   TYPE devclass.


    io_xml->read( EXPORTING iv_name = 'SHORTTEXT'
                  CHANGING cg_data  = lv_shorttext ).
    io_xml->read( EXPORTING iv_name = 'CLASS'
                  CHANGING cg_data  = lv_class ).

    lv_enhname = ms_item-obj_name.
    lv_package = iv_package.
    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname     = lv_enhname
            enhtype     = ''
            enhtooltype = cl_enh_tool_intf=>tooltype
          IMPORTING
            enhancement = li_tool
          CHANGING
            devclass    = lv_package ).
        lo_enh_intf ?= li_tool.

        lo_enh_intf->if_enh_object_docu~set_shorttext( lv_shorttext ).
        lo_enh_intf->set_class( lv_class ).

        zcl_abapgit_object_enho_clif=>deserialize(
          io_xml  = io_xml
          io_clif = lo_enh_intf ).

        lo_enh_intf->if_enh_object~save( ).
        lo_enh_intf->if_enh_object~unlock( ).
      CATCH cx_enh_root.
        zcx_abapgit_exception=>raise( 'error deserializing ENHO interface' ).
    ENDTRY.

  ENDMETHOD.                    "zif_abapgit_object_enho~deserialize

ENDCLASS.                    "zcl_abapgit_object_enho_interface IMPLEMENTATION
