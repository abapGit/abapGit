*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_CLAS
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_object_clas DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_clas DEFINITION INHERITING FROM lcl_objects_program.

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
    DATA mo_object_oriented_object_fct TYPE REF TO lif_object_oriented_object_fnc.
  PRIVATE SECTION.
    DATA mv_skip_testclass TYPE abap_bool.

    METHODS deserialize_tpool
      IMPORTING io_xml TYPE REF TO lcl_xml_input
      RAISING   lcx_exception.

    METHODS deserialize_sotr
      IMPORTING io_xml     TYPE REF TO lcl_xml_input
                iv_package TYPE devclass
      RAISING   lcx_exception.


    METHODS serialize_xml
      IMPORTING io_xml TYPE REF TO lcl_xml_output
      RAISING   lcx_exception.


ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_clas IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_clas IMPLEMENTATION.

  METHOD lif_object~has_changed_since.
    DATA:
      lt_includes TYPE seoincl_t.

    FIELD-SYMBOLS <incl> LIKE LINE OF lt_includes.

    lt_includes = mo_object_oriented_object_fct->get_includes( ms_item-obj_name ).
    LOOP AT lt_includes ASSIGNING <incl>.
      rv_changed = check_prog_changed_since(
        iv_program   = <incl>
        iv_timestamp = iv_timestamp
        iv_skip_gui  = abap_true ).
      IF rv_changed = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.  "lif_object~has_changed_since

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

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

  METHOD lif_object~exists.
    DATA: ls_class_key TYPE seoclskey.
    ls_class_key-clsname = ms_item-obj_name.

    rv_bool = mo_object_oriented_object_fct->exists( iv_object_name = ls_class_key ).
  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~jump.
    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'CLAS'
        in_new_window = abap_true.
  ENDMETHOD.                    "jump

  METHOD lif_object~delete.
    DATA: ls_clskey TYPE seoclskey.
    ls_clskey-clsname = ms_item-obj_name.

    mo_object_oriented_object_fct->delete( ls_clskey ).
  ENDMETHOD.                    "delete

  METHOD lif_object~serialize.

    DATA: lt_source    TYPE seop_source_string,
          ls_class_key TYPE seoclskey.

    ls_class_key-clsname = ms_item-obj_name.

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

    lt_source = mo_object_oriented_object_fct->serialize_abap( ls_class_key ).

    mo_files->add_abap( lt_source ).

    lt_source = mo_object_oriented_object_fct->serialize_abap(
      is_class_key = ls_class_key
      iv_type      = seop_ext_class_locals_def ).
    IF NOT lt_source[] IS INITIAL.
      mo_files->add_abap( iv_extra = 'locals_def'
                          it_abap  = lt_source ).           "#EC NOTEXT
    ENDIF.

    lt_source = mo_object_oriented_object_fct->serialize_abap(
      is_class_key = ls_class_key
      iv_type      = seop_ext_class_locals_imp ).
    IF NOT lt_source[] IS INITIAL.
      mo_files->add_abap( iv_extra = 'locals_imp'
                          it_abap  = lt_source ).           "#EC NOTEXT
    ENDIF.

    lt_source = mo_object_oriented_object_fct->serialize_abap(
      is_class_key            = ls_class_key
      iv_type                 = seop_ext_class_testclasses ).

    mv_skip_testclass = mo_object_oriented_object_fct->get_skip_test_classes( ).
    IF NOT lt_source[] IS INITIAL AND mv_skip_testclass = abap_false.
      mo_files->add_abap( iv_extra = 'testclasses'
                          it_abap  = lt_source ).           "#EC NOTEXT
    ENDIF.

    lt_source = mo_object_oriented_object_fct->serialize_abap(
      is_class_key = ls_class_key
      iv_type      = seop_ext_class_macros ).
    IF NOT lt_source[] IS INITIAL.
      mo_files->add_abap( iv_extra = 'macros'
                          it_abap  = lt_source ).           "#EC NOTEXT
    ENDIF.

    serialize_xml( io_xml ).

  ENDMETHOD.                    "serialize



  METHOD serialize_xml.

    DATA: ls_vseoclass    TYPE vseoclass,
          lt_tpool        TYPE textpool_table,
          lv_object       TYPE dokhl-object,
          lv_state        TYPE dokhl-dokstate,
          lt_descriptions TYPE ty_seocompotx_tt,
          ls_clskey       TYPE seoclskey,
          lt_sotr         TYPE ty_sotr_tt,
          lt_lines        TYPE tlinetab.


    ls_clskey-clsname = ms_item-obj_name.

    ls_vseoclass = mo_object_oriented_object_fct->get_class_properties( is_class_key = ls_clskey ).

    CLEAR: ls_vseoclass-uuid,
           ls_vseoclass-author,
           ls_vseoclass-createdon,
           ls_vseoclass-changedby,
           ls_vseoclass-changedon,
           ls_vseoclass-r3release,
           ls_vseoclass-chgdanyby,
           ls_vseoclass-chgdanyon.

    IF mv_skip_testclass = abap_true.
      CLEAR ls_vseoclass-with_unit_tests.
    ENDIF.

    io_xml->add( iv_name = 'VSEOCLASS'
                 ig_data = ls_vseoclass ).

    lt_tpool = mo_object_oriented_object_fct->read_text_pool(
      iv_class_name = ls_clskey-clsname
      iv_language   = mv_language ).
    io_xml->add( iv_name = 'TPOOL'
                 ig_data = add_tpool( lt_tpool ) ).

    IF ls_vseoclass-category = seoc_category_exception.
      lt_sotr =  mo_object_oriented_object_fct->read_sotr( ms_item-obj_name ).
      IF lines( lt_sotr ) > 0.
        io_xml->add( iv_name = 'SOTR'
                     ig_data = lt_sotr ).
      ENDIF.
    ENDIF.

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

  ENDMETHOD.                    "serialize_xml

  METHOD lif_object~deserialize.
    deserialize_abap( io_xml     = io_xml
                      iv_package = iv_package ).

    deserialize_tpool( io_xml ).

    deserialize_sotr( io_xml     = io_xml
                      iv_package = iv_package ).

    deserialize_docu( io_xml ).
  ENDMETHOD.                    "deserialize

  METHOD deserialize_sotr.
    "OTR stands for Online Text Repository
    DATA: lt_sotr    TYPE ty_sotr_tt,
          lt_objects TYPE sotr_objects.

    io_xml->read( EXPORTING iv_name = 'SOTR'
                  CHANGING cg_data = lt_sotr ).

    IF lines( lt_sotr ) = 0.
      RETURN.
    ENDIF.

    mo_object_oriented_object_fct->create_sotr(
      iv_package    = iv_package
      it_sotr       = lt_sotr ).
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

  METHOD deserialize_tpool.

    DATA: lv_clsname   TYPE seoclsname,
          lt_tpool_ext TYPE ty_tpool_tt,
          lt_tpool     TYPE textpool_table.


    io_xml->read( EXPORTING iv_name = 'TPOOL'
                  CHANGING cg_data = lt_tpool_ext ).
    lt_tpool = read_tpool( lt_tpool_ext ).

    IF lt_tpool[] IS INITIAL.
      RETURN.
    ENDIF.

    lv_clsname = ms_item-obj_name.

    mo_object_oriented_object_fct->insert_text_pool(
      iv_class_name = lv_clsname
      it_text_pool  = lt_tpool
      iv_language   = mv_language ).

  ENDMETHOD.                    "deserialize_textpool

  METHOD deserialize_abap.

    DATA: ls_vseoclass             TYPE vseoclass,
          lt_source                TYPE seop_source_string,
          lt_local_definitions     TYPE seop_source_string,
          lt_local_implementations TYPE seop_source_string,
          lt_local_macros          TYPE seop_source_string,
          lt_test_classes          TYPE seop_source_string,
          lt_descriptions          TYPE ty_seocompotx_tt,
          ls_class_key             TYPE seoclskey.


    lt_source = mo_files->read_abap( ).

    lt_local_definitions = mo_files->read_abap( iv_extra = 'locals_def'
                                                iv_error = abap_false ). "#EC NOTEXT

    lt_local_implementations = mo_files->read_abap( iv_extra = 'locals_imp'
                                                    iv_error = abap_false ). "#EC NOTEXT

    lt_local_macros = mo_files->read_abap( iv_extra = 'macros'
                                           iv_error = abap_false ). "#EC NOTEXT

    lt_test_classes = mo_files->read_abap( iv_extra = 'testclasses'
                                           iv_error = abap_false ). "#EC NOTEXT

    ls_class_key-clsname = ms_item-obj_name.

    io_xml->read( EXPORTING iv_name = 'VSEOCLASS'
                  CHANGING cg_data = ls_vseoclass ).

    mo_object_oriented_object_fct->create(
      EXPORTING
        iv_package    = iv_package
      CHANGING
        is_properties = ls_vseoclass ).
    mo_object_oriented_object_fct->generate_locals(
      is_key                   = ls_class_key
      iv_force                 = seox_true
      it_local_definitions     = lt_local_definitions
      it_local_implementations = lt_local_implementations
      it_local_macros          = lt_local_macros
      it_local_test_classes    = lt_test_classes ).

    mo_object_oriented_object_fct->deserialize_source(
      is_key               = ls_class_key
      it_source            = lt_source ).

    io_xml->read( EXPORTING iv_name = 'DESCRIPTIONS'
                  CHANGING cg_data = lt_descriptions ).

    mo_object_oriented_object_fct->update_descriptions(
      is_key          = ls_class_key
      it_descriptions = lt_descriptions ).

    mo_object_oriented_object_fct->add_to_activation_list( is_item = ms_item  ).
  ENDMETHOD.                    "deserialize

  METHOD lif_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_null_comparison_result.
  ENDMETHOD.

  METHOD constructor.
    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).
    mo_object_oriented_object_fct = lcl_object_oriented_factory=>make( iv_object_type = ms_item-obj_type ).
  ENDMETHOD.

ENDCLASS.                    "lcl_object_CLAS IMPLEMENTATION

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
      lt_tpool        TYPE textpool_table,
      lv_object       TYPE dokhl-object,
      lv_state        TYPE dokhl-dokstate,
      lt_descriptions TYPE ty_seocompotx_tt,
      ls_vseointerf   TYPE vseointerf,
      ls_clskey       TYPE seoclskey,
      lt_sotr         TYPE ty_sotr_tt,
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
    DATA: ls_class_key TYPE seoclskey.
    ls_class_key-clsname = ms_item-obj_name.

    rv_bool = mo_object_oriented_object_fct->exists( iv_object_name = ls_class_key ).
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
