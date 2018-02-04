CLASS zcl_abapgit_object_enho_fugr DEFINITION PUBLIC.

  PUBLIC SECTION.
    METHODS: constructor
      IMPORTING
        is_item  TYPE zif_abapgit_definitions=>ty_item
        io_files TYPE REF TO zcl_abapgit_objects_files.
    INTERFACES: zif_abapgit_object_enho.

  PRIVATE SECTION.
    DATA: ms_item  TYPE zif_abapgit_definitions=>ty_item,
          mo_files TYPE REF TO zcl_abapgit_objects_files.

ENDCLASS.                    "zcl_abapgit_object_enho_wdyconf DEFINITION

CLASS zcl_abapgit_object_enho_fugr IMPLEMENTATION.

  METHOD constructor.
    ms_item = is_item.
    mo_files = io_files.
  ENDMETHOD.                    "constructor

  METHOD zif_abapgit_object_enho~deserialize.

    DATA: lo_fugrdata  TYPE REF TO cl_enh_tool_fugr,
          ls_enha_data TYPE enhfugrdata,
          li_tool      TYPE REF TO if_enh_tool,
          tool         TYPE enhtooltype,
          lv_package   TYPE devclass.

    FIELD-SYMBOLS: <fuba> TYPE enhfugrfuncdata.

    io_xml->read(
      EXPORTING
        iv_name = 'TOOL'
      CHANGING
        cg_data = tool ).

    io_xml->read(
      EXPORTING
        iv_name = 'FUGRDATA'
      CHANGING
        cg_data = ls_enha_data ).

    lv_package = iv_package.

    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname     = |{ ms_item-obj_name }|
            enhtype     = ''
            enhtooltype = tool
          IMPORTING
            enhancement = li_tool
          CHANGING
            devclass    = lv_package ).

        lo_fugrdata ?= li_tool.

        lo_fugrdata->set_fugr( ls_enha_data-fugr ).

        LOOP AT ls_enha_data-enh_fubas ASSIGNING <fuba>.

          lo_fugrdata->set_func_data( func_name     = <fuba>-fuba
                                      func_enhadata = <fuba> ).

        ENDLOOP.

        lo_fugrdata->if_enh_object~save( ).
        lo_fugrdata->if_enh_object~unlock( ).

      CATCH cx_enh_root.
        zcx_abapgit_exception=>raise( |error deserializing ENHO fugrdata { ms_item-obj_name }| ).
    ENDTRY.

  ENDMETHOD.                    "zif_abapgit_object_enho~deserialize

  METHOD zif_abapgit_object_enho~serialize.

    DATA: lo_fugrdata  TYPE REF TO cl_enh_tool_fugr,
          fugr_name    TYPE rs38l-area,
          ls_enha_data TYPE enhfugrdata.

    FIELD-SYMBOLS: <docuobj> TYPE enhfugrparamdocu.

    lo_fugrdata ?= ii_enh_tool.

    lo_fugrdata->get_fugr(
      IMPORTING
        fugr_name = fugr_name ).

    TRY.
        lo_fugrdata->get_all_data_for_fugr(
          EXPORTING
            fugr_name = fugr_name
          IMPORTING
            enha_data = ls_enha_data ).

        LOOP AT ls_enha_data-docuobjs ASSIGNING <docuobj>.

          CLEAR: <docuobj>-shorttext,
                 <docuobj>-longtext.

        ENDLOOP.

      CATCH cx_enh_not_found.
        zcx_abapgit_exception=>raise( |error deserializing ENHO fugrdata { ms_item-obj_name }| ).
    ENDTRY.

    io_xml->add( iv_name = 'TOOL'
                 ig_data = lo_fugrdata->if_enh_tool~get_tool( ) ).

    io_xml->add( iv_name = 'FUGRDATA'
                 ig_data = ls_enha_data ).

  ENDMETHOD.                    "zif_abapgit_object_enho~serialize

ENDCLASS.                    "zcl_abapgit_object_enho_wdyconf IMPLEMENTATION
