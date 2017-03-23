*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_INTF
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_object_intf DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_intf DEFINITION FINAL INHERITING FROM lcl_objects_program.
  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.
    METHODS constructor
      IMPORTING
        is_item     TYPE ty_item
        iv_language TYPE spras.
  PROTECTED SECTION.
    METHODS deserialize_abap
      IMPORTING io_xml     TYPE REF TO lcl_xml_input
                iv_package TYPE devclass
      RAISING   lcx_exception.

    METHODS deserialize_docu
      IMPORTING io_xml TYPE REF TO lcl_xml_input
      RAISING   lcx_exception.

  PRIVATE SECTION.
    DATA mo_object_oriented_object_fct TYPE REF TO lif_object_oriented_object_fnc.

    METHODS serialize_xml
      IMPORTING io_xml TYPE REF TO lcl_xml_output
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_intf DEFINITION


CLASS lcl_object_intf IMPLEMENTATION.
  METHOD constructor.
    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).
    mo_object_oriented_object_fct = lcl_object_oriented_factory=>make( iv_object_type = ms_item-obj_type ).
  ENDMETHOD.
  METHOD lif_object~deserialize.
    deserialize_abap( io_xml     = io_xml
                      iv_package = iv_package ).

    deserialize_docu( io_xml ).
  ENDMETHOD.
  METHOD deserialize_abap.
    DATA: ls_vseointerf   TYPE vseointerf,
          lt_source       TYPE seop_source_string,
          lt_descriptions TYPE ty_seocompotx_tt,
          ls_clskey       TYPE seoclskey.
    ls_clskey-clsname = ms_item-obj_name.

    lt_source = mo_files->read_abap( ).

    io_xml->read( EXPORTING iv_name = 'VSEOINTERF'
                  CHANGING cg_data = ls_vseointerf ).

    mo_object_oriented_object_fct->create(
      EXPORTING
        iv_package    = iv_package
      CHANGING
        is_properties = ls_vseointerf ).

    mo_object_oriented_object_fct->deserialize_source(
      is_key               = ls_clskey
      it_source            = lt_source ).

    io_xml->read( EXPORTING iv_name = 'DESCRIPTIONS'
                  CHANGING cg_data = lt_descriptions ).

    mo_object_oriented_object_fct->update_descriptions(
      is_key          = ls_clskey
      it_descriptions = lt_descriptions ).

    mo_object_oriented_object_fct->add_to_activation_list( is_item = ms_item ).
  ENDMETHOD.

  METHOD deserialize_docu.

    DATA: lt_lines  TYPE tlinetab,
          lv_object TYPE dokhl-object.

    io_xml->read( EXPORTING iv_name = 'LINES'
                  CHANGING cg_data = lt_lines ).

    IF lt_lines[] IS INITIAL.
      RETURN.
    ENDIF.

    lv_object = ms_item-obj_name.

    mo_object_oriented_object_fct->create_documentation(
      it_lines       = lt_lines
      iv_object_name = lv_object
      iv_language    = mv_language ).
  ENDMETHOD.

  METHOD lif_object~has_changed_since.
    DATA:
      lv_program  TYPE program,
      lt_includes TYPE seoincl_t.

    lt_includes = mo_object_oriented_object_fct->get_includes( ms_item-obj_name ).
    READ TABLE lt_includes INDEX 1 INTO lv_program.
    "lv_program = cl_oo_classname_service=>get_interfacepool_name( lv_clsname ).
    rv_changed = check_prog_changed_since(
      iv_program   = lv_program
      iv_timestamp = iv_timestamp
      iv_skip_gui  = abap_true ).
  ENDMETHOD.

  METHOD lif_object~serialize.

    DATA: lt_source        TYPE seop_source_string,
          ls_interface_key TYPE seoclskey.

    ls_interface_key-clsname = ms_item-obj_name.

    IF lif_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        version = seoc_version_active
        force   = seox_true.
    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        version = seoc_version_inactive
        force   = seox_true.

    lt_source = mo_object_oriented_object_fct->serialize_abap( ls_interface_key ).

    mo_files->add_abap( lt_source ).

    serialize_xml( io_xml ).
  ENDMETHOD.

  METHOD serialize_xml.
    DATA:
      lt_descriptions TYPE ty_seocompotx_tt,
      ls_vseointerf   TYPE vseointerf,
      ls_clskey       TYPE seoclskey,
      lt_lines        TYPE tlinetab.


    ls_clskey-clsname = ms_item-obj_name.

    ls_vseointerf = mo_object_oriented_object_fct->get_interface_properties( is_interface_key = ls_clskey ).

    CLEAR: ls_vseointerf-uuid,
           ls_vseointerf-author,
           ls_vseointerf-createdon,
           ls_vseointerf-changedby,
           ls_vseointerf-changedon,
           ls_vseointerf-r3release.

    io_xml->add( iv_name = 'VSEOINTERF'
                 ig_data = ls_vseointerf ).

    lt_lines = mo_object_oriented_object_fct->read_documentation(
      iv_class_name = ls_clskey-clsname
      iv_language   = mv_language ).
    IF lines( lt_lines ) > 0.
      io_xml->add( iv_name = 'LINES'
                   ig_data = lt_lines ).
    ENDIF.

    lt_descriptions = mo_object_oriented_object_fct->read_descriptions( ls_clskey-clsname ).
    IF lines( lt_descriptions ) > 0.
      io_xml->add( iv_name = 'DESCRIPTIONS'
                   ig_data = lt_descriptions ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_object~changed_by.
    TYPES: BEGIN OF ty_includes,
             programm TYPE programm,
           END OF ty_includes.

    TYPES: BEGIN OF ty_reposrc,
             unam  TYPE reposrc-unam,
             udat  TYPE reposrc-udat,
             utime TYPE reposrc-utime,
           END OF ty_reposrc.

    DATA: lt_reposrc  TYPE STANDARD TABLE OF ty_reposrc,
          ls_reposrc  LIKE LINE OF lt_reposrc,
          lt_includes TYPE STANDARD TABLE OF ty_includes.

    lt_includes = mo_object_oriented_object_fct->get_includes( ms_item-obj_name ).
    ASSERT lines( lt_includes ) > 0.

    SELECT unam udat utime FROM reposrc
      INTO TABLE lt_reposrc
      FOR ALL ENTRIES IN lt_includes
      WHERE progname = lt_includes-programm
      AND   r3state = 'A'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ELSE.
      SORT lt_reposrc BY udat DESCENDING utime DESCENDING.
      READ TABLE lt_reposrc INDEX 1 INTO ls_reposrc.
      ASSERT sy-subrc = 0.
      rv_user = ls_reposrc-unam.
    ENDIF.
  ENDMETHOD.

  METHOD lif_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_null_comparison_result.
  ENDMETHOD.

  METHOD lif_object~delete.
    DATA: ls_clskey TYPE seoclskey.
    ls_clskey-clsname = ms_item-obj_name.

    mo_object_oriented_object_fct->delete( ls_clskey ).
  ENDMETHOD.

  METHOD lif_object~exists.

    DATA: ls_class_key TYPE seoclskey,
          lv_category  TYPE seoclassdf-category.

    ls_class_key-clsname = ms_item-obj_name.

    rv_bool = mo_object_oriented_object_fct->exists( iv_object_name = ls_class_key ).

    IF rv_bool = abap_true.
      SELECT SINGLE category FROM seoclassdf INTO lv_category
        WHERE clsname = ls_class_key-clsname
        AND ( version = '1'
        OR version = '0' ) ##warn_ok.                   "#EC CI_GENBUFF
      IF sy-subrc = 0 AND lv_category = seoc_category_webdynpro_class.
        rv_bool = abap_false.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.

  METHOD lif_object~jump.
    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'INTF'
        in_new_window = abap_true.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_object_oriented_interface DEFINITION
  INHERITING FROM lcl_object_oriented_base.
  PUBLIC SECTION.
    METHODS:
      lif_object_oriented_object_fnc~create REDEFINITION,
      lif_object_oriented_object_fnc~get_includes REDEFINITION,
      lif_object_oriented_object_fnc~get_interface_properties REDEFINITION,
      lif_object_oriented_object_fnc~delete REDEFINITION.
ENDCLASS.

CLASS lcl_object_oriented_interface IMPLEMENTATION.
  METHOD lif_object_oriented_object_fnc~create.
    CALL FUNCTION 'SEO_INTERFACE_CREATE_COMPLETE'
      EXPORTING
        devclass        = iv_package
        overwrite       = iv_overwrite
      CHANGING
        interface       = is_properties
      EXCEPTIONS
        existing        = 1
        is_class        = 2
        db_error        = 3
        component_error = 4
        no_access       = 5
        other           = 6
        OTHERS          = 7.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'Error from SEO_INTERFACE_CREATE_COMPLETE' ).
    ENDIF.
  ENDMETHOD.
  METHOD lif_object_oriented_object_fnc~get_includes.
    DATA lv_interface_name TYPE seoclsname.
    lv_interface_name = iv_object_name.
    APPEND cl_oo_classname_service=>get_interfacepool_name( lv_interface_name ) TO rt_includes.
  ENDMETHOD.

  METHOD lif_object_oriented_object_fnc~get_interface_properties.
    CALL FUNCTION 'SEO_CLIF_GET'
      EXPORTING
        cifkey       = is_interface_key
        version      = seoc_version_active
      IMPORTING
        interface    = rs_interface_properties
      EXCEPTIONS
        not_existing = 1
        deleted      = 2
        model_only   = 3
        OTHERS       = 4.
    IF sy-subrc = 1.
      RETURN. " in case only inactive version exists
    ELSEIF sy-subrc <> 0.
      lcx_exception=>raise( 'error from seo_clif_get' ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_object_oriented_object_fnc~delete.
    CALL FUNCTION 'SEO_INTERFACE_DELETE_COMPLETE'
      EXPORTING
        intkey       = is_deletion_key
      EXCEPTIONS
        not_existing = 1
        is_class     = 2
        db_error     = 3
        no_access    = 4
        other        = 5
        OTHERS       = 6.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'Error from SEO_INTERFACE_DELETE_COMPLETE' ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
