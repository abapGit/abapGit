CLASS zcl_abapgit_object_clas DEFINITION PUBLIC INHERITING FROM zcl_abapgit_object_clas_old.

  PUBLIC SECTION.
    METHODS: constructor
      IMPORTING
        is_item     TYPE zif_abapgit_definitions=>ty_item
        iv_language TYPE spras.

  PROTECTED SECTION.
    METHODS:
      deserialize_abap REDEFINITION.

ENDCLASS.

CLASS zcl_abapgit_object_clas IMPLEMENTATION.

  METHOD constructor.
    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    CREATE OBJECT mo_object_oriented_object_fct TYPE zcl_abapgit_oo_class_new.
  ENDMETHOD.

  METHOD deserialize_abap.
* same as in zcl_abapgit_object_clas, but without "mo_object_oriented_object_fct->add_to_activation_list"

    DATA: ls_vseoclass             TYPE vseoclass,
          lt_source                TYPE seop_source_string,
          lt_local_definitions     TYPE seop_source_string,
          lt_local_implementations TYPE seop_source_string,
          lt_local_macros          TYPE seop_source_string,
          lt_test_classes          TYPE seop_source_string,
          lt_descriptions          TYPE zif_abapgit_definitions=>ty_seocompotx_tt,
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
      is_key    = ls_class_key
      it_source = lt_source ).

    io_xml->read( EXPORTING iv_name = 'DESCRIPTIONS'
                  CHANGING cg_data = lt_descriptions ).

    mo_object_oriented_object_fct->update_descriptions(
      is_key          = ls_class_key
      it_descriptions = lt_descriptions ).

  ENDMETHOD.

ENDCLASS.