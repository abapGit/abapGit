
CLASS ltd_spy_oo_object DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES: lif_object_oriented_object.
    DATA:
      mv_package               TYPE devclass,
      mv_overwrite             TYPE seox_boolean,
      ms_interface_properties  TYPE vseointerf,
      ms_class_properties      TYPE vseoclass,
      ms_locals_key            TYPE seoclskey,
      mt_local_definitions     TYPE rswsourcet,
      mt_local_implementations TYPE rswsourcet,
      mt_local_macros          TYPE rswsourcet,
      mt_local_test_classes    TYPE rswsourcet,
      mv_force                 TYPE seoflag,
      ms_deserialize_key       TYPE seoclskey,
      mt_source                TYPE ty_string_tt,
      ms_item_to_activate      TYPE ty_item,
      mt_descriptions          TYPE ty_seocompotx_tt,
      ms_description_key       TYPE seoclskey,
      mv_text_pool_class_name  TYPE seoclsname,
      mt_text_pool             TYPE textpool_table,
      mv_text_pool_inserted    TYPE abap_bool,
      mt_sotr                  TYPE ty_sotr_tt,
      mt_sotr_package          TYPE devclass,
      mv_docu_object_name      TYPE dokhl-object,
      mv_docu_language         TYPE spras,
      mt_docu_lines            TYPE tlinetab,
      mv_get_includes_called   TYPE abap_bool.
ENDCLASS.
CLASS ltd_spy_oo_object IMPLEMENTATION.
  METHOD lif_object_oriented_object~create.
    DATA lv_properties_structure_name TYPE string.
    lv_properties_structure_name = cl_abap_typedescr=>describe_by_data( is_properties )->absolute_name.
    IF lv_properties_structure_name = cl_abap_typedescr=>describe_by_data( ms_interface_properties )->absolute_name.
      ms_interface_properties = is_properties.
    ELSE.
      ms_class_properties     = is_properties.
    ENDIF.
    mv_package                = iv_package.
    mv_overwrite              = iv_overwrite.
  ENDMETHOD.
  METHOD lif_object_oriented_object~generate_locals.
    ms_locals_key            = is_key.
    mt_local_definitions     = it_local_definitions.
    mt_local_implementations = it_local_implementations.
    mt_local_macros          = it_local_macros.
    mt_local_test_classes    = it_local_test_classes.
    mv_force                 = iv_force.
  ENDMETHOD.

  METHOD lif_object_oriented_object~deserialize_source.
    ms_deserialize_key = is_key.
    mt_source          = it_source.
  ENDMETHOD.

  METHOD lif_object_oriented_object~add_to_activation_list.
    ms_item_to_activate = is_item.
  ENDMETHOD.

  METHOD lif_object_oriented_object~update_descriptions.
    ms_description_key = is_key.
    mt_descriptions    = it_descriptions.
  ENDMETHOD.

  METHOD lif_object_oriented_object~insert_text_pool.
    mv_text_pool_inserted = abap_true.
    mv_text_pool_class_name = iv_class_name.
    mt_text_pool = it_text_pool.
    cl_abap_unit_assert=>assert_equals(
      act = iv_language
      exp = sy-langu ).
  ENDMETHOD.

  METHOD lif_object_oriented_object~create_sotr.
    mt_sotr = it_sotr.
    mt_sotr_package = iv_package.
  ENDMETHOD.

  METHOD lif_object_oriented_object~create_documentation.
    mv_docu_object_name = iv_object_name.
    mv_docu_language    = iv_language.
    mt_docu_lines       = it_lines.
  ENDMETHOD.

  METHOD lif_object_oriented_object~get_includes.
    APPEND 'dummy' TO rt_includes.
    mv_get_includes_called = abap_true.
  ENDMETHOD.

ENDCLASS.

CLASS ltd_fake_object_files DEFINITION FOR TESTING
  INHERITING FROM  lcl_objects_files.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS read_abap REDEFINITION.
    DATA:
      mt_sources               TYPE seop_source_string,
      mt_local_definitions     TYPE seop_source_string,
      mt_local_implementations TYPE seop_source_string,
      mt_local_macros          TYPE seop_source_string,
      mt_local_test_classes    TYPE seop_source_string.
ENDCLASS.
CLASS ltd_fake_object_files IMPLEMENTATION.
  METHOD read_abap.
    CASE iv_extra.
      WHEN 'locals_def'.
        rt_abap = mt_local_definitions.
      WHEN 'locals_imp'.
        rt_abap = mt_local_implementations.
      WHEN 'macros'.
        rt_abap = mt_local_macros.
      WHEN 'testclasses'.
        rt_abap = mt_local_test_classes.
      WHEN OTHERS.
        rt_abap = mt_sources.
        RETURN.
    ENDCASE.

    cl_abap_unit_assert=>assert_equals( act = iv_error
                                        exp = abap_false ).
  ENDMETHOD.
  METHOD constructor.
    DATA ls_empty_item TYPE ty_item.
    super->constructor( ls_empty_item ).
    APPEND 'source'         TO me->mt_sources.
    APPEND 'definition'     TO me->mt_local_definitions.
    APPEND 'implementation' TO me->mt_local_implementations.
    APPEND 'macro'          TO me->mt_local_macros.
    APPEND 'test'           TO me->mt_local_test_classes.
  ENDMETHOD.

ENDCLASS.

CLASS ltc_oo_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PROTECTED SECTION.
    DATA:
      mo_spy_oo_object     TYPE REF TO ltd_spy_oo_object,
      mo_fake_object_files TYPE REF TO ltd_fake_object_files,
      mo_xml_input         TYPE REF TO lcl_xml_input,
      mo_xml_out           TYPE REF TO lcl_xml_output,
      mo_oo_object         TYPE REF TO lif_object,
      ms_item              TYPE ty_item.
    METHODS: when_deserializing
      RAISING
        lcx_exception,
      then_should_deserialize_source,
      given_the_descriptions
        IMPORTING
          it_descriptions TYPE ty_seocompotx_tt
        RAISING
          lcx_exception,
      then_shuld_update_descriptions
        IMPORTING
          it_descriptions TYPE ty_seocompotx_tt,
      then_it_should_add_activation,
      given_documentation_in_xml_as
        IMPORTING
          it_lines TYPE tlinetab
        RAISING
          lcx_exception,
      then_docu_should_be_created
        IMPORTING
          it_lines TYPE tlinetab.

ENDCLASS.
CLASS ltc_oo_test IMPLEMENTATION.

  METHOD then_docu_should_be_created.
    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->mt_docu_lines
      exp = it_lines ).

    cl_abap_unit_assert=>assert_equals(
       act = mo_spy_oo_object->mv_docu_object_name
       exp = ms_item-obj_name ).

    cl_abap_unit_assert=>assert_equals(
       act = mo_spy_oo_object->mv_docu_language
       exp = sy-langu ).
  ENDMETHOD.

  METHOD given_documentation_in_xml_as.
    mo_xml_out->add(
      iv_name = 'LINES'
      ig_data = it_lines ).
  ENDMETHOD.

  METHOD then_it_should_add_activation.
    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->ms_item_to_activate
      exp = ms_item ).
  ENDMETHOD.

  METHOD then_shuld_update_descriptions.
    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->mt_descriptions
      exp = it_descriptions ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->ms_description_key
      exp = ms_item-obj_name ).
  ENDMETHOD.

  METHOD given_the_descriptions.
    mo_xml_out->add(
      iv_name = 'DESCRIPTIONS'
      ig_data = it_descriptions ).
  ENDMETHOD.

  METHOD then_should_deserialize_source.
    cl_abap_unit_assert=>assert_equals(
       act = mo_spy_oo_object->mt_source
       exp = mo_fake_object_files->mt_sources ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->ms_deserialize_key
      exp = ms_item-obj_name ).
  ENDMETHOD.

  METHOD when_deserializing.
    CREATE OBJECT mo_xml_input
      EXPORTING
        iv_xml = mo_xml_out->render( ).
    mo_oo_object->deserialize(
      iv_package    = 'package_name'
      io_xml        = mo_xml_input ).
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_class_deserialization DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT
INHERITING FROM ltc_oo_test.
  PRIVATE SECTION.
    METHODS:
      setup,
      given_a_class_properties
        RAISING
          lcx_exception,
      then_should_create_class,
      then_it_should_generate_locals,
      should_create_class        FOR TESTING RAISING cx_static_check,
      should_generate_locals     FOR TESTING RAISING cx_static_check,
      should_deserialize_source  FOR TESTING RAISING cx_static_check,
      should_update_descriptions FOR TESTING RAISING cx_static_check,
      should_add_to_activation   FOR TESTING RAISING cx_static_check,
      no_text_pool_no_insert     FOR TESTING RAISING cx_static_check,
      insert_text_pool           FOR TESTING RAISING cx_static_check,
      create_stor_from_xml       FOR TESTING RAISING cx_static_check,
      create_documentation       FOR TESTING RAISING cx_static_check.
    DATA:
      ms_class_properties  TYPE vseoclass.
ENDCLASS.

CLASS ltcl_class_deserialization IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT mo_fake_object_files.
    CREATE OBJECT mo_spy_oo_object.
    CREATE OBJECT mo_xml_out.
    lth_oo_factory_injector=>inject( mo_spy_oo_object ).

    ms_item-devclass = 'package_name'.
    ms_item-obj_name = 'zcl_class'.
    ms_item-obj_type = 'CLAS'.

    CREATE OBJECT mo_oo_object TYPE lcl_object_clas
      EXPORTING
        is_item     = ms_item
        iv_language = sy-langu.
    mo_oo_object->mo_files = mo_fake_object_files.
  ENDMETHOD.

  METHOD should_create_class.
    ms_class_properties-clsname = ms_item-obj_name.

    given_a_class_properties( ).

    when_deserializing( ).

    then_should_create_class( ).
  ENDMETHOD.

  METHOD should_generate_locals.
    given_a_class_properties( ).

    when_deserializing( ).

    then_it_should_generate_locals( ).
  ENDMETHOD.

  METHOD should_deserialize_source.
    given_a_class_properties( ).

    when_deserializing( ).

    then_should_deserialize_source( ).
  ENDMETHOD.

  METHOD should_update_descriptions.
    DATA:
      ls_description  TYPE seocompotx,
      lt_descriptions TYPE ty_seocompotx_tt.

    given_a_class_properties( ).

    ls_description-clsname =  ms_item-obj_name.
    ls_description-cmpname = 'a_method'.
    APPEND ls_description TO lt_descriptions.
    given_the_descriptions( lt_descriptions ).

    when_deserializing( ).

    then_shuld_update_descriptions( lt_descriptions ).
  ENDMETHOD.

  METHOD should_add_to_activation.
    given_a_class_properties( ).

    when_deserializing( ).

    then_it_should_add_activation( ).
  ENDMETHOD.

  METHOD given_a_class_properties.
    mo_xml_out->add(
      iv_name = 'VSEOCLASS'
      ig_data = ms_class_properties ).
  ENDMETHOD.

  METHOD then_should_create_class.
    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->ms_class_properties
      exp = ms_class_properties ).

    cl_abap_unit_assert=>assert_equals( act = mo_spy_oo_object->mv_overwrite
                                        exp = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->mv_package
      exp = 'package_name' ).
  ENDMETHOD.


  METHOD then_it_should_generate_locals.
    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->ms_locals_key
      exp = ms_item-obj_name ).

    cl_abap_unit_assert=>assert_equals( act = mo_spy_oo_object->mv_force
                                        exp = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->mt_local_definitions
      exp = mo_fake_object_files->mt_local_definitions  ).

    cl_abap_unit_assert=>assert_equals(
       act = mo_spy_oo_object->mt_local_implementations
       exp = mo_fake_object_files->mt_local_implementations ).

    cl_abap_unit_assert=>assert_equals(
       act = mo_spy_oo_object->mt_local_macros
       exp = mo_fake_object_files->mt_local_macros ).

    cl_abap_unit_assert=>assert_equals(
       act = mo_spy_oo_object->mt_local_test_classes
       exp = mo_fake_object_files->mt_local_test_classes ).
  ENDMETHOD.
  METHOD no_text_pool_no_insert.
    given_a_class_properties( ).

    when_deserializing( ).

    cl_abap_unit_assert=>assert_equals( act = mo_spy_oo_object->mv_text_pool_inserted
                                        exp = abap_false ).
  ENDMETHOD.

  METHOD insert_text_pool.
    DATA: lt_pool_external TYPE textpool_table,
          ls_pool_external TYPE ty_tpool.
    ls_pool_external-id = 'ID'.
    ls_pool_external-key = 'KEY'.
    APPEND ls_pool_external TO lt_pool_external.

    given_a_class_properties( ).

    mo_xml_out->add(
      iv_name = 'TPOOL'
      ig_data = lt_pool_external ).

    when_deserializing( ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->mt_text_pool
      exp = lt_pool_external ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->mv_text_pool_class_name
      exp = 'zcl_class' ).
  ENDMETHOD.

  METHOD create_stor_from_xml.
    DATA:
      lt_sotr TYPE ty_sotr_tt,
      ls_sotr LIKE LINE OF lt_sotr.

    given_a_class_properties( ).

    ls_sotr-header-concept = 'HEADER'.
    APPEND ls_sotr TO lt_sotr.
    mo_xml_out->add(
      iv_name = 'SOTR'
      ig_data = lt_sotr ).

    when_deserializing( ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->mt_sotr
      exp = lt_sotr ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->mt_sotr_package
      exp = 'package_name' ).
  ENDMETHOD.

  METHOD create_documentation.
    DATA: lt_lines TYPE tlinetab,
          ls_line  TYPE LINE OF tlinetab.
    ls_line-tdline = 'Class Line Doc'.
    APPEND ls_line TO lt_lines.

    given_a_class_properties( ).

    given_documentation_in_xml_as( lt_lines ).

    when_deserializing( ).

    then_docu_should_be_created( lt_lines ).
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_interface_deserialization DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT
INHERITING FROM ltc_oo_test.
  PRIVATE SECTION.
    METHODS:
      setup,
      given_an_interface_properties
        RAISING
          lcx_exception,
      then_should_create_interface,
      create_interface    FOR TESTING RAISING cx_static_check,
      update_descriptions FOR TESTING RAISING cx_static_check,
      add_to_activation   FOR TESTING RAISING cx_static_check,
      deserialize_source  FOR TESTING RAISING cx_static_check,
      create_documentation FOR TESTING RAISING cx_static_check.
    DATA:
          ms_interface_properties TYPE vseointerf.
ENDCLASS.
CLASS ltcl_interface_deserialization IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT mo_fake_object_files.
    CREATE OBJECT mo_spy_oo_object.
    CREATE OBJECT mo_xml_out.
    lth_oo_factory_injector=>inject( mo_spy_oo_object ).

    ms_item-devclass = 'package_name'.
    ms_item-obj_name = 'zif_interface'.
    ms_item-obj_type = 'INTF'.

    CREATE OBJECT mo_oo_object TYPE lcl_object_intf
      EXPORTING
        is_item     = ms_item
        iv_language = sy-langu.
    mo_oo_object->mo_files = mo_fake_object_files.
  ENDMETHOD.

  METHOD create_interface.
    ms_interface_properties-clsname = ms_item-obj_name.
    given_an_interface_properties( ).

    when_deserializing( ).

    then_should_create_interface( ).
  ENDMETHOD.

  METHOD update_descriptions.
    DATA:
      ls_description  TYPE seocompotx,
      lt_descriptions TYPE ty_seocompotx_tt.

    given_an_interface_properties( ).

    ls_description-clsname =  ms_item-obj_name.
    ls_description-cmpname = 'a_method'.
    APPEND ls_description TO lt_descriptions.
    given_the_descriptions( lt_descriptions ).

    when_deserializing( ).

    then_shuld_update_descriptions( lt_descriptions ).
  ENDMETHOD.

  METHOD add_to_activation.
    given_an_interface_properties( ).

    when_deserializing( ).

    then_it_should_add_activation( ).
  ENDMETHOD.

  METHOD deserialize_source.
    given_an_interface_properties( ).

    when_deserializing( ).

    then_should_deserialize_source( ).
  ENDMETHOD.

  METHOD given_an_interface_properties.
    mo_xml_out->add(
      iv_name = 'VSEOINTERF'
      ig_data = ms_interface_properties ).
  ENDMETHOD.

  METHOD then_should_create_interface.
    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->ms_interface_properties
      exp = ms_interface_properties ).

    cl_abap_unit_assert=>assert_equals( act = mo_spy_oo_object->mv_overwrite
                                        exp = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->mv_package
      exp = 'package_name' ).
  ENDMETHOD.

  METHOD create_documentation.
    DATA: lt_lines TYPE tlinetab,
          ls_line  TYPE LINE OF tlinetab.
    ls_line-tdline = 'Interface Line Doc'.
    APPEND ls_line TO lt_lines.

    given_an_interface_properties( ).

    given_documentation_in_xml_as( lt_lines ).

    when_deserializing( ).

    then_docu_should_be_created( lt_lines ).
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_class_changed DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT
INHERITING FROM ltc_oo_test.
  PRIVATE SECTION.
    METHODS:
      setup,
      changed_by_call_get_includes FOR TESTING RAISING cx_static_check,
      changed_since_call_get_include FOR TESTING RAISING cx_static_check.

ENDCLASS.
CLASS ltcl_class_changed IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT mo_fake_object_files.
    CREATE OBJECT mo_spy_oo_object.
    CREATE OBJECT mo_xml_out.
    lth_oo_factory_injector=>inject( mo_spy_oo_object ).

    ms_item-devclass = 'package_name'.
    ms_item-obj_name = 'zcl_class'.
    ms_item-obj_type = 'CLAS'.

    CREATE OBJECT mo_oo_object TYPE lcl_object_clas
      EXPORTING
        is_item     = ms_item
        iv_language = sy-langu.
    mo_oo_object->mo_files = mo_fake_object_files.
  ENDMETHOD.
  METHOD changed_by_call_get_includes.
    DATA lv_username TYPE xubname.
    lv_username = mo_oo_object->changed_by( ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->mv_get_includes_called
      exp = abap_true ).

    cl_abap_unit_assert=>assert_equals(
       act = lv_username
       exp = lcl_objects_super=>c_user_unknown ).
  ENDMETHOD.
  METHOD changed_since_call_get_include.
    DATA lv_timestamp TYPE timestamp.
    GET TIME STAMP FIELD lv_timestamp.
    mo_oo_object->has_changed_since( lv_timestamp ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->mv_get_includes_called
      exp = abap_true ).
  ENDMETHOD.
ENDCLASS.
CLASS ltcl_interface_changed DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT
INHERITING FROM ltc_oo_test.
  PRIVATE SECTION.
    METHODS:
      setup,
      changed_by_call_get_includes FOR TESTING RAISING cx_static_check,
      changed_since_call_get_include FOR TESTING RAISING cx_static_check.
ENDCLASS.
CLASS ltcl_interface_changed IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT mo_fake_object_files.
    CREATE OBJECT mo_spy_oo_object.
    CREATE OBJECT mo_xml_out.
    lth_oo_factory_injector=>inject( mo_spy_oo_object ).

    ms_item-devclass = 'package_name'.
    ms_item-obj_name = 'zif_interface'.
    ms_item-obj_type = 'INTF'.

    CREATE OBJECT mo_oo_object TYPE lcl_object_intf
      EXPORTING
        is_item     = ms_item
        iv_language = sy-langu.
    mo_oo_object->mo_files = mo_fake_object_files.
  ENDMETHOD.
  METHOD changed_by_call_get_includes.
    DATA lv_username TYPE xubname.
    lv_username = mo_oo_object->changed_by( ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->mv_get_includes_called
      exp = abap_true ).

    cl_abap_unit_assert=>assert_equals(
       act = lv_username
       exp = lcl_objects_super=>c_user_unknown ).
  ENDMETHOD.
  METHOD changed_since_call_get_include.
    DATA lv_timestamp TYPE timestamp.
    GET TIME STAMP FIELD lv_timestamp.
    mo_oo_object->has_changed_since( lv_timestamp ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_spy_oo_object->mv_get_includes_called
      exp = abap_true ).
  ENDMETHOD.
ENDCLASS.
