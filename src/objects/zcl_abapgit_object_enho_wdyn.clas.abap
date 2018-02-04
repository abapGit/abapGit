CLASS zcl_abapgit_object_enho_wdyn DEFINITION PUBLIC.

  PUBLIC SECTION.
    METHODS: constructor
      IMPORTING
        is_item  TYPE zif_abapgit_definitions=>ty_item
        io_files TYPE REF TO zcl_abapgit_objects_files.
    INTERFACES: zif_abapgit_object_enho.

  PRIVATE SECTION.
    DATA: ms_item  TYPE zif_abapgit_definitions=>ty_item.

ENDCLASS.

CLASS zcl_abapgit_object_enho_wdyn IMPLEMENTATION.

  METHOD constructor.
    ms_item = is_item.
  ENDMETHOD.                    "constructor

  METHOD zif_abapgit_object_enho~deserialize.

    DATA: ls_enh_data TYPE enhwdyn,
          li_tool     TYPE REF TO if_enh_tool,
          lo_wdyn     TYPE REF TO cl_enh_tool_wdy,
          tool_type   TYPE enhtooltype,
          package     TYPE devclass.

    FIELD-SYMBOLS: <controller_data> TYPE enhwdyc,
                   <view_data>       TYPE enhwdyv.

    io_xml->read(
      EXPORTING
        iv_name = 'TOOL'
      CHANGING
        cg_data = tool_type ).

    io_xml->read(
      EXPORTING
        iv_name = 'COMPONENT_DATA'
      CHANGING
        cg_data = ls_enh_data ).

    package = iv_package.

    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname     = |{ ms_item-obj_name }|
            enhtype     = ''
            enhtooltype = tool_type
          IMPORTING
            enhancement = li_tool
          CHANGING
            devclass    = package ).

        lo_wdyn ?= li_tool.

        lo_wdyn->initialize( ls_enh_data-component_name ).

        lo_wdyn->set_component_data( ls_enh_data-component_data ).

        LOOP AT ls_enh_data-controller_data ASSIGNING <controller_data>.

          lo_wdyn->set_controller_data( p_controller_name = <controller_data>-controller_name
                                        p_enh_data        = <controller_data> ).

        ENDLOOP.

        LOOP AT ls_enh_data-view_data ASSIGNING <view_data>.

          lo_wdyn->set_view_data( p_view_name = <view_data>-view_name
                                  p_enh_data  = <view_data> ).

        ENDLOOP.

        lo_wdyn->if_enh_object~save( ).
        lo_wdyn->if_enh_object~unlock( ).

      CATCH cx_root.
        zcx_abapgit_exception=>raise( |error deserializing ENHO wdyn { ms_item-obj_name }| ).
    ENDTRY.

  ENDMETHOD.                    "zif_abapgit_object_enho~deserialize

  METHOD zif_abapgit_object_enho~serialize.

    DATA: lo_wdyn        TYPE REF TO cl_enh_tool_wdy,
          component_name TYPE wdy_component_name,
          ls_enh_data    TYPE enhwdyn.

    lo_wdyn ?= ii_enh_tool.

    component_name = lo_wdyn->get_component_name( ).

    TRY.
        lo_wdyn->get_all_data_for_comp(
          EXPORTING
            p_component_name = component_name
          IMPORTING
            p_enh_data       = ls_enh_data ).

        io_xml->add( iv_name = 'TOOL'
                     ig_data = ii_enh_tool->get_tool( ) ).

        io_xml->add( iv_name = 'COMPONENT_DATA'
                     ig_data = ls_enh_data ).

      CATCH cx_enh_not_found.
        zcx_abapgit_exception=>raise( |error serializing ENHO wdyn { ms_item-obj_name }| ).
    ENDTRY.

  ENDMETHOD.                    "zif_abapgit_object_enho~serialize

ENDCLASS.                    "zcl_abapgit_object_enho_wdyconf IMPLEMENTATION
