CLASS zcl_abapgit_object_intf DEFINITION PUBLIC FINAL INHERITING FROM zcl_abapgit_objects_program.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.
    METHODS constructor
      IMPORTING
        is_item     TYPE zif_abapgit_definitions=>ty_item
        iv_language TYPE spras.
  PROTECTED SECTION.
    METHODS deserialize_abap
      IMPORTING io_xml     TYPE REF TO zcl_abapgit_xml_input
                iv_package TYPE devclass
      RAISING   zcx_abapgit_exception.

    METHODS deserialize_docu
      IMPORTING io_xml TYPE REF TO zcl_abapgit_xml_input
      RAISING   zcx_abapgit_exception.

  PRIVATE SECTION.
    DATA mo_object_oriented_object_fct TYPE REF TO zif_abapgit_oo_object_fnc.

    METHODS serialize_xml
      IMPORTING io_xml TYPE REF TO zcl_abapgit_xml_output
      RAISING   zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_INTF IMPLEMENTATION.


  METHOD constructor.
    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).
    mo_object_oriented_object_fct = zcl_abapgit_oo_factory=>make( iv_object_type = ms_item-obj_type ).
  ENDMETHOD.


  METHOD deserialize_abap.
    DATA: ls_vseointerf   TYPE vseointerf,
          lt_source       TYPE seop_source_string,
          lt_descriptions TYPE zif_abapgit_definitions=>ty_seocompotx_tt,
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


  METHOD serialize_xml.
    DATA:
      lt_descriptions TYPE zif_abapgit_definitions=>ty_seocompotx_tt,
      ls_vseointerf   TYPE vseointerf,
      ls_clskey       TYPE seoclskey,
      lt_lines        TYPE tlinetab.


    ls_clskey-clsname = ms_item-obj_name.

    ls_vseointerf = mo_object_oriented_object_fct->get_interface_properties( ls_clskey ).

    CLEAR: ls_vseointerf-uuid,
           ls_vseointerf-author,
           ls_vseointerf-createdon,
           ls_vseointerf-changedby,
           ls_vseointerf-changedon,
           ls_vseointerf-chgdanyby,
           ls_vseointerf-chgdanyon,
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


  METHOD zif_abapgit_object~changed_by.
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


  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.
    DATA: ls_clskey TYPE seoclskey.
    ls_clskey-clsname = ms_item-obj_name.

    mo_object_oriented_object_fct->delete( ls_clskey ).
  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.
    deserialize_abap( io_xml     = io_xml
                      iv_package = iv_package ).

    deserialize_docu( io_xml ).
  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

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


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~has_changed_since.
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


  METHOD zif_abapgit_object~jump.
    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'INTF'
        in_new_window = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lt_source        TYPE seop_source_string,
          ls_interface_key TYPE seoclskey.

    ls_interface_key-clsname = ms_item-obj_name.

    IF zif_abapgit_object~exists( ) = abap_false.
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
ENDCLASS.
