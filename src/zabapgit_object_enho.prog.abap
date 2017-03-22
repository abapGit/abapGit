*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_ENHO
*&---------------------------------------------------------------------*

* For complete list of tool_type - see ENHTOOLS table

INTERFACE lif_object_enho.

  METHODS:
    deserialize
      IMPORTING io_xml     TYPE REF TO lcl_xml_input
                iv_package TYPE devclass
      RAISING   lcx_exception,
    serialize
      IMPORTING io_xml      TYPE REF TO lcl_xml_output
                ii_enh_tool TYPE REF TO if_enh_tool
      RAISING   lcx_exception.

ENDINTERFACE.                    "lif_object_enho

*----------------------------------------------------------------------*
*       CLASS lcl_object_enho_wdyconf DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_enho_wdyconf DEFINITION.

  PUBLIC SECTION.
    METHODS: constructor
      IMPORTING
        is_item  TYPE ty_item
        io_files TYPE REF TO lcl_objects_files.
    INTERFACES: lif_object_enho.

  PRIVATE SECTION.
    DATA: ms_item  TYPE ty_item,
          mo_files TYPE REF TO lcl_objects_files.

ENDCLASS.                    "lcl_object_enho_wdyconf DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_enho_wdyconf IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_enho_wdyconf IMPLEMENTATION.

  METHOD constructor.
    ms_item = is_item.
    mo_files = io_files.
  ENDMETHOD.                    "constructor

  METHOD lif_object_enho~deserialize.

    DATA: lv_enhname TYPE enhname,
          lo_wdyconf TYPE REF TO cl_wdr_cfg_enhancement,
          li_tool    TYPE REF TO if_enh_tool,
          ls_obj     TYPE wdy_config_key,
          lv_package TYPE devclass.


    io_xml->read( EXPORTING iv_name = 'ORIGINAL_OBJECT'
                  CHANGING cg_data  = ls_obj ).

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

* todo
* io_xml->read_xml()
* CL_WDR_CFG_PERSISTENCE_UTILS=>COMP_XML_TO_TABLES( )
* lo_wdyconf->set_enhancement_data( )
        ASSERT 0 = 1.

        lo_wdyconf->if_enh_object~save( ).
        lo_wdyconf->if_enh_object~unlock( ).
      CATCH cx_enh_root.
        lcx_exception=>raise( 'error deserializing ENHO wdyconf' ).
    ENDTRY.

  ENDMETHOD.                    "lif_object_enho~deserialize

  METHOD lif_object_enho~serialize.

    DATA: lo_wdyconf  TYPE REF TO cl_wdr_cfg_enhancement,
          lt_data     TYPE wdy_cfg_expl_data_tab,
          ls_outline  TYPE wdy_cfg_outline_data,
          ls_obj      TYPE wdy_config_key,
          li_document TYPE REF TO if_ixml_document,
          li_element  TYPE REF TO if_ixml_element.


    lo_wdyconf ?= ii_enh_tool.

    ls_obj = lo_wdyconf->get_original_object( ).
    io_xml->add( iv_name = 'TOOL'
                 ig_data = ii_enh_tool->get_tool( ) ).
    io_xml->add( iv_name = 'ORIGINAL_OBJECT'
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

    io_xml->add_xml( iv_name = 'ENHANCEMENT_DATA'
                     ii_xml = li_element ).

  ENDMETHOD.                    "lif_object_enho~serialize

ENDCLASS.                    "lcl_object_enho_wdyconf IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS lcl_object_enho_clif DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_enho_clif DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      deserialize
        IMPORTING io_xml  TYPE REF TO lcl_xml_input
                  io_clif TYPE REF TO cl_enh_tool_clif
        RAISING   lcx_exception
                  cx_enh_root,
      serialize
        IMPORTING io_xml   TYPE REF TO lcl_xml_output
                  io_files TYPE REF TO lcl_objects_files
                  io_clif  TYPE REF TO cl_enh_tool_clif
        RAISING   lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS: serialize_includes
      IMPORTING io_files TYPE REF TO lcl_objects_files
                io_clif  TYPE REF TO cl_enh_tool_clif
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_enho_clif DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_enho_clif IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_enho_clif IMPLEMENTATION.

  METHOD serialize_includes.

    DATA: lt_includes TYPE enhnewmeth_tabincl_plus_enha,
          lt_source   TYPE TABLE OF abaptxt255,
          lv_include  TYPE programm.

    FIELD-SYMBOLS: <ls_include> LIKE LINE OF lt_includes.


    lt_includes = io_clif->get_enh_method_includes( ).
    LOOP AT lt_includes ASSIGNING <ls_include>.
      lv_include = io_clif->if_enh_tool~get_name( ).
      TRANSLATE lv_include USING ' ='.
      lv_include+30 = 'EM'.
      lv_include+32(8) = <ls_include>-includenr.

      CALL FUNCTION 'RPY_PROGRAM_READ'
        EXPORTING
          program_name     = lv_include
          with_lowercase   = abap_true
        TABLES
          source_extended  = lt_source
        EXCEPTIONS
          cancelled        = 1
          not_found        = 2
          permission_error = 3
          OTHERS           = 4.
      IF sy-subrc = 0.
        io_files->add_abap( iv_extra = |EM{ <ls_include>-includenr }|
                            it_abap  = lt_source ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "serialize_includes

  METHOD serialize.

    DATA: lt_tab_attributes TYPE enhclasstabattrib,
          lt_tab_methods    TYPE enhnewmeth_tab.

    FIELD-SYMBOLS: <ls_attr> LIKE LINE OF lt_tab_attributes,
                   <ls_meth> LIKE LINE OF lt_tab_methods.


    io_clif->get_enhattributes(
      IMPORTING
        tab_attributes = lt_tab_attributes ).

    io_clif->get_enh_new_methodes(
      IMPORTING
        tab_methodes = lt_tab_methods ).

    serialize_includes( io_clif  = io_clif
                        io_files = io_files ).

    LOOP AT lt_tab_attributes ASSIGNING <ls_attr>.
      CLEAR: <ls_attr>-author,
             <ls_attr>-createdon,
             <ls_attr>-changedby,
             <ls_attr>-changedon.
    ENDLOOP.

    LOOP AT lt_tab_methods ASSIGNING <ls_meth>.
      CLEAR: <ls_meth>-meth_header-author,
             <ls_meth>-meth_header-createdon,
             <ls_meth>-meth_header-changedby,
             <ls_meth>-meth_header-changedon,
             <ls_meth>-meth_header-descript_id.
    ENDLOOP.

    io_xml->add( iv_name = 'TAB_ATTRIBUTES'
                 ig_data = lt_tab_attributes ).
    io_xml->add( iv_name = 'TAB_METHODS'
                 ig_data = lt_tab_methods ).

  ENDMETHOD.                    "serialize

  METHOD deserialize.

    DATA: lt_tab_attributes TYPE enhclasstabattrib,
          lt_tab_methods    TYPE enhnewmeth_tab,
          ls_header         TYPE vseomethod,
          ls_param          TYPE vseomepara,
          ls_exc            TYPE vseoexcep.

    FIELD-SYMBOLS: <ls_method> LIKE LINE OF lt_tab_methods,
                   <ls_param>  LIKE LINE OF <ls_method>-meth_param,
                   <ls_exc>    LIKE LINE OF <ls_method>-meth_exc.


    io_xml->read( EXPORTING iv_name = 'TAB_ATTRIBUTES'
                  CHANGING cg_data = lt_tab_attributes ).
    io_xml->read( EXPORTING iv_name = 'TAB_METHODS'
                  CHANGING cg_data = lt_tab_methods ).

    io_clif->set_enhattributes( tab_attributes = lt_tab_attributes ).

* todo: deserialize includes

* SAP standard SET_ENH_NEW_METHOS does not work

    LOOP AT lt_tab_methods ASSIGNING <ls_method>.

      MOVE-CORRESPONDING <ls_method>-meth_header TO ls_header.

      io_clif->add_change_new_enh_method(
        methkey       = <ls_method>-methkey
        method_header = ls_header ).

* parameters
      LOOP AT <ls_method>-meth_param ASSIGNING <ls_param>.
        MOVE-CORRESPONDING <ls_param> TO ls_param.
        io_clif->add_change_enh_methparam(
          methname   = <ls_method>-methkey-cmpname
          param_line = ls_param ).
      ENDLOOP.

* exceptions
      LOOP AT <ls_method>-meth_exc ASSIGNING <ls_exc>.
        MOVE-CORRESPONDING <ls_exc> TO ls_exc.
        io_clif->add_change_enh_methexc(
          methname    = <ls_method>-methkey-cmpname
          except_line = ls_exc ).
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.                    "deserialize

ENDCLASS.                    "lcl_object_enho_clif IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_enho_badi DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_enho_badi DEFINITION.

  PUBLIC SECTION.
    METHODS: constructor
      IMPORTING
        is_item  TYPE ty_item
        io_files TYPE REF TO lcl_objects_files.
    INTERFACES: lif_object_enho.

  PRIVATE SECTION.
    DATA: ms_item  TYPE ty_item,
          mo_files TYPE REF TO lcl_objects_files.

ENDCLASS.                    "lcl_object_enho_badi DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_enho_badi IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_enho_badi IMPLEMENTATION.

  METHOD constructor.
    ms_item = is_item.
    mo_files = io_files.
  ENDMETHOD.                    "constructor

  METHOD lif_object_enho~serialize.

    DATA: lo_badi_impl TYPE REF TO cl_enh_tool_badi_impl,
          lv_spot_name TYPE enhspotname,
          lv_shorttext TYPE string,
          lt_impl      TYPE enh_badi_impl_data_it.

    FIELD-SYMBOLS: <ls_impl>   LIKE LINE OF lt_impl,
                   <ls_values> LIKE LINE OF <ls_impl>-filter_values,
                   <ls_filter> LIKE LINE OF <ls_impl>-filters.


    lo_badi_impl ?= ii_enh_tool.

    lv_shorttext = lo_badi_impl->if_enh_object_docu~get_shorttext( ).
    lv_spot_name = lo_badi_impl->get_spot_name( ).
    lt_impl      = lo_badi_impl->get_implementations( ).

    LOOP AT lt_impl ASSIGNING <ls_impl>.
* make sure the XML serialization does not dump, field type = N
      LOOP AT <ls_impl>-filter_values ASSIGNING <ls_values>.
        IF <ls_values>-filter_numeric_value1 CA space.
          CLEAR <ls_values>-filter_numeric_value1.
        ENDIF.
      ENDLOOP.
      LOOP AT <ls_impl>-filters ASSIGNING <ls_filter>.
        IF <ls_filter>-filter_numeric_value1 CA space.
          CLEAR <ls_filter>-filter_numeric_value1.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    io_xml->add( iv_name = 'TOOL'
                 ig_data = ii_enh_tool->get_tool( ) ).
    io_xml->add( ig_data = lv_shorttext
                 iv_name = 'SHORTTEXT' ).
    io_xml->add( iv_name = 'SPOT_NAME'
                 ig_data = lv_spot_name ).
    io_xml->add( iv_name = 'IMPL'
                 ig_data = lt_impl ).

  ENDMETHOD.                    "lif_object_enho~serialize

  METHOD lif_object_enho~deserialize.

    DATA: lv_spot_name TYPE enhspotname,
          lv_shorttext TYPE string,
          lv_enhname   TYPE enhname,
          lo_badi      TYPE REF TO cl_enh_tool_badi_impl,
          li_tool      TYPE REF TO if_enh_tool,
          lv_package   TYPE devclass,
          lt_impl      TYPE enh_badi_impl_data_it.

    FIELD-SYMBOLS: <ls_impl> LIKE LINE OF lt_impl.


    io_xml->read( EXPORTING iv_name = 'SHORTTEXT'
                  CHANGING cg_data  = lv_shorttext ).
    io_xml->read( EXPORTING iv_name = 'SPOT_NAME'
                  CHANGING cg_data  = lv_spot_name ).
    io_xml->read( EXPORTING iv_name = 'IMPL'
                  CHANGING cg_data  = lt_impl ).

    lv_enhname = ms_item-obj_name.
    lv_package = iv_package.
    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname     = lv_enhname
            enhtype     = cl_abstract_enh_tool_redef=>credefinition
            enhtooltype = cl_enh_tool_badi_impl=>tooltype
          IMPORTING
            enhancement = li_tool
          CHANGING
            devclass    = lv_package ).
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

  ENDMETHOD.                    "lif_object_enho~deserialize

ENDCLASS.                    "lcl_object_enho_badi IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_enho_hook DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_enho_hook DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor
      IMPORTING
        is_item  TYPE ty_item
        io_files TYPE REF TO lcl_objects_files.

    INTERFACES: lif_object_enho.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_spaces,
             full_name TYPE string.
    TYPES: spaces TYPE STANDARD TABLE OF i WITH DEFAULT KEY,
           END OF ty_spaces.

    TYPES: ty_spaces_tt TYPE STANDARD TABLE OF ty_spaces WITH DEFAULT KEY.

    DATA: ms_item TYPE ty_item.
    DATA: mo_files TYPE REF TO lcl_objects_files.

    METHODS hook_impl_deserialize
      IMPORTING it_spaces TYPE ty_spaces_tt
      CHANGING  ct_impl   TYPE enh_hook_impl_it
      RAISING   lcx_exception.

    METHODS hook_impl_serialize
      EXPORTING et_spaces TYPE ty_spaces_tt
      CHANGING  ct_impl   TYPE enh_hook_impl_it
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_enho_hook DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_enho_hook IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_enho_hook IMPLEMENTATION.

  METHOD constructor.
    ms_item = is_item.
    mo_files = io_files.
  ENDMETHOD.                    "constructor

  METHOD lif_object_enho~serialize.

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
                 ig_data = ii_enh_tool->get_tool( ) ).
    io_xml->add( ig_data = lv_shorttext
                 iv_name = 'SHORTTEXT' ).
    io_xml->add( ig_data = ls_original_object
                 iv_name = 'ORIGINAL_OBJECT' ).
    io_xml->add( iv_name = 'ENHANCEMENTS'
                 ig_data = lt_enhancements ).
    io_xml->add( iv_name = 'SPACES'
                 ig_data = lt_spaces ).

  ENDMETHOD.                    "lif_object_enho~serialize

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
  ENDMETHOD.                    "hook_impl_serialize

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

  ENDMETHOD.                    "hook_impl_deserialize

  METHOD lif_object_enho~deserialize.

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
                  CHANGING cg_data  = lv_shorttext ).
    io_xml->read( EXPORTING iv_name = 'ORIGINAL_OBJECT'
                  CHANGING cg_data  = ls_original_object ).
    io_xml->read( EXPORTING iv_name = 'ENHANCEMENTS'
                  CHANGING cg_data  = lt_enhancements ).
    io_xml->read( EXPORTING iv_name = 'SPACES'
                  CHANGING cg_data  = lt_spaces ).

    hook_impl_deserialize( EXPORTING it_spaces = lt_spaces
                           CHANGING ct_impl    = lt_enhancements ).

    lv_enhname = ms_item-obj_name.
    lv_package = iv_package.
    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname     = lv_enhname
            enhtype     = cl_abstract_enh_tool_redef=>credefinition
            enhtooltype = cl_enh_tool_hook_impl=>tooltype
          IMPORTING
            enhancement = li_tool
          CHANGING
            devclass    = lv_package ).
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
              overwrite        = <ls_enhancement>-overwrite
              method           = <ls_enhancement>-method
              enhmode          = <ls_enhancement>-enhmode
              full_name        = <ls_enhancement>-full_name
              source           = <ls_enhancement>-source
              spot             = <ls_enhancement>-spotname
              parent_full_name = <ls_enhancement>-parent_full_name ).
        ENDLOOP.
        lo_hook_impl->if_enh_object~save( ).
        lo_hook_impl->if_enh_object~unlock( ).
      CATCH cx_enh_root.
        lcx_exception=>raise( 'error deserializing ENHO hook' ).
    ENDTRY.

  ENDMETHOD.                    "lif_object_enho~deserialize

ENDCLASS.                    "lcl_object_enho_hook IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_enho_interface DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_enho_interface DEFINITION.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          is_item  TYPE ty_item
          io_files TYPE REF TO lcl_objects_files.
    INTERFACES: lif_object_enho.

  PRIVATE SECTION.
    DATA: ms_item  TYPE ty_item,
          mo_files TYPE REF TO lcl_objects_files.

ENDCLASS.                    "lcl_object_enho_interface DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_enho_interface IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_enho_interface IMPLEMENTATION.

  METHOD constructor.
    ms_item  = is_item.
    mo_files = io_files.
  ENDMETHOD.                    "constructor

  METHOD lif_object_enho~serialize.

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

    lcl_object_enho_clif=>serialize(
      io_xml  = io_xml
      io_files = mo_files
      io_clif = lo_enh_intf ).

  ENDMETHOD.                    "lif_object_enho~serialize

  METHOD lif_object_enho~deserialize.

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

        lcl_object_enho_clif=>deserialize(
          io_xml  = io_xml
          io_clif = lo_enh_intf ).

        lo_enh_intf->if_enh_object~save( ).
        lo_enh_intf->if_enh_object~unlock( ).
      CATCH cx_enh_root.
        lcx_exception=>raise( 'error deserializing ENHO interface' ).
    ENDTRY.

  ENDMETHOD.                    "lif_object_enho~deserialize

ENDCLASS.                    "lcl_object_enho_interface IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_enho_class DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_enho_class DEFINITION.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          is_item  TYPE ty_item
          io_files TYPE REF TO lcl_objects_files.
    INTERFACES: lif_object_enho.

  PRIVATE SECTION.
    DATA: ms_item TYPE ty_item.
    DATA: mo_files TYPE REF TO lcl_objects_files.

ENDCLASS.                    "lcl_object_enho_class DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_enho_class IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_enho_class IMPLEMENTATION.

  METHOD constructor.
    ms_item = is_item.
    mo_files = io_files.
  ENDMETHOD.                    "constructor

  METHOD lif_object_enho~serialize.

    DATA: lo_enh_class TYPE REF TO cl_enh_tool_class,
          lt_owr       TYPE enhmeth_tabkeys,
          lt_pre       TYPE enhmeth_tabkeys,
          lt_post      TYPE enhmeth_tabkeys,
          lt_source    TYPE rswsourcet,
          lv_class     TYPE seoclsname,
          lv_shorttext TYPE string.


    lo_enh_class ?= ii_enh_tool.

    lv_shorttext = lo_enh_class->if_enh_object_docu~get_shorttext( ).
    lt_owr = lo_enh_class->get_owr_methods( ).
    lt_pre = lo_enh_class->get_pre_methods( ).
    lt_post = lo_enh_class->get_post_methods( ).
    lt_source = lo_enh_class->get_eimp_include( ).
    lo_enh_class->get_class( IMPORTING class_name = lv_class ).

    io_xml->add( iv_name = 'TOOL'
                 ig_data = ii_enh_tool->get_tool( ) ).
    io_xml->add( ig_data = lv_shorttext
                 iv_name = 'SHORTTEXT' ).
    io_xml->add( iv_name = 'CLASS'
                 ig_data = lv_class ).
    io_xml->add( iv_name = 'OWR_METHODS'
                 ig_data = lt_owr ).
    io_xml->add( iv_name = 'PRE_METHODS'
                 ig_data = lt_pre ).
    io_xml->add( iv_name = 'POST_METHODS'
                 ig_data = lt_post ).

    mo_files->add_abap( lt_source ).

    lcl_object_enho_clif=>serialize(
      io_xml   = io_xml
      io_files = mo_files
      io_clif  = lo_enh_class ).

  ENDMETHOD.                    "lif_object_enho~serialize

  METHOD lif_object_enho~deserialize.

    DATA: lo_enh_class TYPE REF TO cl_enh_tool_class,
          lt_owr       TYPE enhmeth_tabkeys,
          lt_pre       TYPE enhmeth_tabkeys,
          lt_post      TYPE enhmeth_tabkeys,
          lt_source    TYPE rswsourcet,
          li_tool      TYPE REF TO if_enh_tool,
          lv_shorttext TYPE string,
          lv_class     TYPE seoclsname,
          lv_enhname   TYPE enhname,
          lv_package   TYPE devclass.


    io_xml->read( EXPORTING iv_name = 'SHORTTEXT'
                  CHANGING cg_data  = lv_shorttext ).
    io_xml->read( EXPORTING iv_name = 'OWR_METHODS'
                  CHANGING cg_data  = lt_owr ).
    io_xml->read( EXPORTING iv_name = 'PRE_METHODS'
                  CHANGING cg_data  = lt_pre ).
    io_xml->read( EXPORTING iv_name = 'POST_METHODS'
                  CHANGING cg_data  = lt_post ).
    io_xml->read( EXPORTING iv_name = 'CLASS'
                  CHANGING cg_data  = lv_class ).
    lt_source = mo_files->read_abap( ).

    lv_enhname = ms_item-obj_name.
    lv_package = iv_package.
    TRY.
        cl_enh_factory=>create_enhancement(
          EXPORTING
            enhname     = lv_enhname
            enhtype     = ''
            enhtooltype = cl_enh_tool_class=>tooltype
          IMPORTING
            enhancement = li_tool
          CHANGING
            devclass    = lv_package ).
        lo_enh_class ?= li_tool.

        lo_enh_class->if_enh_object_docu~set_shorttext( lv_shorttext ).
        lo_enh_class->set_class( lv_class ).
        lo_enh_class->set_owr_methods( version     = 'I'
                                       owr_methods = lt_owr ).
        lo_enh_class->set_pre_methods( version     = 'I'
                                       pre_methods = lt_pre ).
        lo_enh_class->set_post_methods( version      = 'I'
                                        post_methods = lt_post ).
        lo_enh_class->set_eimp_include( version     = 'I'
                                        eimp_source = lt_source ).

        lcl_object_enho_clif=>deserialize(
          io_xml  = io_xml
          io_clif = lo_enh_class ).

        lo_enh_class->if_enh_object~save( ).
        lo_enh_class->if_enh_object~unlock( ).
      CATCH cx_enh_root.
        lcx_exception=>raise( 'error deserializing ENHO class' ).
    ENDTRY.

  ENDMETHOD.                    "lif_object_enho~deserialize

ENDCLASS.                    "lcl_object_enho_class IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_enho DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_enho DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

  PRIVATE SECTION.

    METHODS:
      factory
        IMPORTING
          iv_tool        TYPE enhtooltype
        RETURNING
          value(ri_enho) TYPE REF TO lif_object_enho
        RAISING
          lcx_exception.

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
  ENDMETHOD.                    "lif_object~changed_by

  METHOD lif_object~exists.

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

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~serialize.

    DATA: lv_enh_id   TYPE enhname,
          li_enho     TYPE REF TO lif_object_enho,
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

    li_enho = factory( li_enh_tool->get_tool( ) ).

    li_enho->serialize( io_xml      = io_xml
                        ii_enh_tool = li_enh_tool ).

  ENDMETHOD.                    "serialize

  METHOD factory.

    CASE iv_tool.
      WHEN cl_enh_tool_badi_impl=>tooltype.
        CREATE OBJECT ri_enho TYPE lcl_object_enho_badi
          EXPORTING
            is_item  = ms_item
            io_files = mo_files.
      WHEN cl_enh_tool_hook_impl=>tooltype.
        CREATE OBJECT ri_enho TYPE lcl_object_enho_hook
          EXPORTING
            is_item  = ms_item
            io_files = mo_files.
      WHEN cl_enh_tool_class=>tooltype.
        CREATE OBJECT ri_enho TYPE lcl_object_enho_class
          EXPORTING
            is_item  = ms_item
            io_files = mo_files.
      WHEN cl_enh_tool_intf=>tooltype.
        CREATE OBJECT ri_enho TYPE lcl_object_enho_interface
          EXPORTING
            is_item  = ms_item
            io_files = mo_files.
      WHEN cl_wdr_cfg_enhancement=>tooltype.
        CREATE OBJECT ri_enho TYPE lcl_object_enho_wdyconf
          EXPORTING
            is_item  = ms_item
            io_files = mo_files.
* ToDo:
*      WHEN 'ENHFUGRDATA'. "cl_enh_tool_fugr
*      WHEN 'ENHWDYN'. "cl_enh_tool_wdy
      WHEN OTHERS.
        lcx_exception=>raise( |Unsupported ENHO type { iv_tool }| ).
    ENDCASE.

  ENDMETHOD.                    "factory

  METHOD lif_object~deserialize.

    DATA: lv_tool TYPE enhtooltype,
          li_enho TYPE REF TO lif_object_enho.


    IF lif_object~exists( ) = abap_true.
      lif_object~delete( ).
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'TOOL'
                  CHANGING cg_data = lv_tool ).

    li_enho = factory( lv_tool ).

    li_enho->deserialize( io_xml     = io_xml
                          iv_package = iv_package ).

    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

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

  METHOD lif_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_null_comparison_result.
  ENDMETHOD.                    "lif_object~compare_to_remote_version

ENDCLASS.                    "lcl_object_enho IMPLEMENTATION
