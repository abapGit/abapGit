INTERFACE zif_abapgit_oo_object_fnc PUBLIC.

  TYPES: BEGIN OF ty_includes,
           programm TYPE programm,
         END OF ty_includes,
         ty_includes_tt TYPE STANDARD TABLE OF ty_includes WITH DEFAULT KEY.

  TYPES:
    ty_seocompotx_tt TYPE STANDARD TABLE OF seocompotx WITH DEFAULT KEY .

  METHODS:
    create
      IMPORTING
        iv_package    TYPE devclass
        iv_overwrite  TYPE abap_bool DEFAULT abap_true
        it_attributes TYPE zif_abapgit_definitions=>ty_obj_attribute_tt OPTIONAL
      CHANGING
        cg_properties TYPE any
      RAISING
        zcx_abapgit_exception,
    generate_locals
      IMPORTING
        is_key                   TYPE seoclskey
        iv_force                 TYPE abap_bool DEFAULT abap_true
        it_local_definitions     TYPE seop_source_string OPTIONAL
        it_local_implementations TYPE seop_source_string OPTIONAL
        it_local_macros          TYPE seop_source_string OPTIONAL
        it_local_test_classes    TYPE seop_source_string OPTIONAL
      RAISING
        zcx_abapgit_exception,
    deserialize_source
      IMPORTING
        is_key    TYPE seoclskey
        it_source TYPE zif_abapgit_definitions=>ty_string_tt
      RAISING
        zcx_abapgit_exception
        cx_sy_dyn_call_error,
    insert_text_pool
      IMPORTING
        iv_class_name TYPE seoclsname
        it_text_pool  TYPE textpool_table
        iv_language   TYPE spras
        iv_state      TYPE c DEFAULT 'I'
      RAISING
        zcx_abapgit_exception,
    update_descriptions
      IMPORTING
        is_key          TYPE seoclskey
        it_descriptions TYPE ty_seocompotx_tt,
    add_to_activation_list
      IMPORTING
        is_item TYPE zif_abapgit_definitions=>ty_item
      RAISING
        zcx_abapgit_exception,
    create_sotr
      IMPORTING
        iv_object_name TYPE sobj_name
        iv_package     TYPE devclass
        ii_xml         TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception,
    create_documentation
      IMPORTING
        it_lines         TYPE tlinetab
        iv_object_name   TYPE dokhl-object
        iv_language      TYPE spras
        iv_no_masterlang TYPE abap_bool OPTIONAL
      RAISING
        zcx_abapgit_exception,
    get_includes
      IMPORTING
        iv_object_name     TYPE sobj_name
      RETURNING
        VALUE(rt_includes) TYPE ty_includes_tt
      RAISING
        zcx_abapgit_exception,
    exists
      IMPORTING
        is_object_name   TYPE seoclskey
      RETURNING
        VALUE(rv_exists) TYPE abap_bool,
    serialize_abap
      IMPORTING
        is_class_key     TYPE seoclskey
        iv_type          TYPE seop_include_ext_app OPTIONAL
      RETURNING
        VALUE(rt_source) TYPE zif_abapgit_definitions=>ty_string_tt
      RAISING
        zcx_abapgit_exception
        cx_sy_dyn_call_error,
    get_skip_test_classes
      RETURNING
        VALUE(rv_skip) TYPE abap_bool,
    get_class_properties
      IMPORTING
        is_class_key               TYPE seoclskey
      RETURNING
        VALUE(rs_class_properties) TYPE vseoclass
      RAISING
        zcx_abapgit_exception,
    get_interface_properties
      IMPORTING
        is_interface_key               TYPE seoclskey
      RETURNING
        VALUE(rs_interface_properties) TYPE vseointerf
      RAISING
        zcx_abapgit_exception,
    read_text_pool
      IMPORTING
        iv_class_name       TYPE seoclsname
        iv_language         TYPE spras
      RETURNING
        VALUE(rt_text_pool) TYPE textpool_table,
    read_documentation
      IMPORTING
        iv_class_name   TYPE seoclsname
        iv_language     TYPE spras
      RETURNING
        VALUE(rt_lines) TYPE tlinetab,
    read_sotr
      IMPORTING
        iv_object_name TYPE sobj_name
        ii_xml         TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception,
    read_descriptions
      IMPORTING
        iv_obejct_name         TYPE seoclsname
        iv_language            TYPE spras OPTIONAL
      RETURNING
        VALUE(rt_descriptions) TYPE ty_seocompotx_tt,
    delete
      IMPORTING
        is_deletion_key TYPE seoclskey
      RAISING
        zcx_abapgit_exception,
    read_superclass
      IMPORTING
        iv_classname         TYPE seoclsname
      RETURNING
        VALUE(rv_superclass) TYPE seoclsname,
    read_attributes
      IMPORTING
        iv_object_name       TYPE seoclsname
      RETURNING
        VALUE(rt_attributes) TYPE zif_abapgit_definitions=>ty_obj_attribute_tt.
ENDINTERFACE.
