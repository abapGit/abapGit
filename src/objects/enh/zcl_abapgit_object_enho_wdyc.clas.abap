CLASS zcl_abapgit_object_enho_wdyc DEFINITION PUBLIC.

  PUBLIC SECTION.
    METHODS: constructor
      IMPORTING
        is_item TYPE zif_abapgit_definitions=>ty_item.
    INTERFACES: zif_abapgit_object_enho.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: ms_item  TYPE zif_abapgit_definitions=>ty_item.

ENDCLASS.



CLASS zcl_abapgit_object_enho_wdyc IMPLEMENTATION.


  METHOD constructor.
    ms_item = is_item.
  ENDMETHOD.


  METHOD zif_abapgit_object_enho~deserialize.

    DATA: lv_enhname TYPE enhname,
          lo_wdyconf TYPE REF TO cl_wdr_cfg_enhancement,
          li_tool    TYPE REF TO if_enh_tool,
          ls_obj     TYPE wdy_config_key,
          lv_xml     TYPE string,
          lt_data    TYPE wdy_cfg_expl_data_tab,
          lv_package TYPE devclass.

    ii_xml->read( EXPORTING iv_name = 'ORIGINAL_OBJECT'
                  CHANGING  cg_data = ls_obj ).

    ii_xml->read( EXPORTING iv_name = 'ENHANCEMENT_DATA'
                  CHANGING  cg_data = lv_xml ).

    lv_enhname = ms_item-obj_name.
    lv_package = iv_package.
    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname     = lv_enhname
            enhtype     = ''
            enhtooltype = cl_wdr_cfg_enhancement=>tooltype
          IMPORTING
            enhancement = li_tool
          CHANGING
            devclass    = lv_package ).

        lo_wdyconf ?= li_tool.

        CALL METHOD cl_wdr_cfg_persistence_utils=>('COMP_XML_TO_TABLES')
          EXPORTING
            xml_content   = lv_xml
          IMPORTING
            expl_data_tab = lt_data.

* only works on new ABAP versions, parameters differ between versions
        CALL METHOD lo_wdyconf->('SET_ENHANCEMENT_DATA')
          EXPORTING
            p_enh_data = lt_data.

        lo_wdyconf->if_enh_object~save( run_dark = abap_true ).
        lo_wdyconf->if_enh_object~unlock( ).
      CATCH cx_enh_root cx_static_check.
        TRY.
            lo_wdyconf->if_enh_object~unlock( ).
          CATCH cx_sy_ref_is_initial cx_enh_mod_not_allowed ##NO_HANDLER.
        ENDTRY.
        zcx_abapgit_exception=>raise( 'error deserializing ENHO wdyconf' ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_abapgit_object_enho~serialize.

    DATA: lo_wdyconf  TYPE REF TO cl_wdr_cfg_enhancement,
          lt_data     TYPE wdy_cfg_expl_data_tab,
          ls_outline  TYPE wdy_cfg_outline_data,
          ls_obj      TYPE wdy_config_key,
          li_document TYPE REF TO if_ixml_document,
          li_element  TYPE REF TO if_ixml_element.


    lo_wdyconf ?= ii_enh_tool.

    ls_obj = lo_wdyconf->get_original_object( ).
    ii_xml->add( iv_name = 'TOOL'
                 ig_data = ii_enh_tool->get_tool( ) ).
    ii_xml->add( iv_name = 'ORIGINAL_OBJECT'
                 ig_data = ls_obj ).

* only works on new ABAP versions, parameters differ between versions
    CALL METHOD lo_wdyconf->('GET_ENHANCEMENT_DATA')
      EXPORTING
        p_scope    = 1
      IMPORTING
        p_enh_data = lt_data.

    CALL METHOD cl_wdr_cfg_persistence_utils=>('COMP_TABLES_TO_XML')
      EXPORTING
        outline_data  = ls_outline
        expl_data_tab = lt_data
      IMPORTING
        element       = li_element
      CHANGING
        document      = li_document.

    ii_xml->add_xml( iv_name = 'ENHANCEMENT_DATA'
                     ii_xml = li_element ).

  ENDMETHOD.
ENDCLASS.
