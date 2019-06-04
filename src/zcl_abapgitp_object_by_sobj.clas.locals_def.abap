CLASS lcx_obj_exception DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        textid   LIKE textid OPTIONAL
        previous LIKE previous OPTIONAL
        iv_text  TYPE string OPTIONAL.

    METHODS: get_error_text RETURNING VALUE(rv_text) TYPE string.

  PROTECTED SECTION.
    DATA mv_text TYPE string.
ENDCLASS.

INTERFACE lif_external_object_container.
  TYPES: BEGIN OF ty_s_component,
           pos       TYPE i,
           name      TYPE cl_abap_structdescr=>component-name,
           type_kind TYPE abap_typekind,
           length    TYPE i,
           decimals  TYPE i,
           is_key    TYPE abap_bool,
         END OF ty_s_component,
         ty_t_component TYPE STANDARD TABLE OF ty_s_component WITH KEY primary_key COMPONENTS pos
                                WITH UNIQUE SORTED KEY name COMPONENTS name
                                WITH NON-UNIQUE SORTED KEY is_key COMPONENTS is_key pos.
  TYPES:
    BEGIN OF ty_s_table_content,
      tabname       TYPE tabname,
      field_catalog TYPE ty_t_component,
      data_tab      TYPE REF TO data,
    END OF ty_s_table_content.
  TYPES:
    ty_t_table_content TYPE SORTED TABLE OF ty_s_table_content WITH UNIQUE KEY tabname.

  TYPES ty_t_tabname TYPE SORTED TABLE OF tabname WITH UNIQUE KEY table_line.

  METHODS store_obj_table
    IMPORTING
              is_table_content TYPE ty_s_table_content
    RAISING   lcx_obj_exception.


  METHODS get_persisted_table_content
    IMPORTING
              it_relevant_table TYPE ty_t_tabname
    EXPORTING
              et_table_content  TYPE ty_t_table_content
    RAISING   lcx_obj_exception.

  METHODS backup_replaced_version
    IMPORTING
              it_table_content TYPE ty_t_table_content
    RAISING   lcx_obj_exception.

ENDINTERFACE.

CLASS lcl_abapgit_xml_container DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_external_object_container.
    DATA mo_xml_input TYPE REF TO zif_abapgitp_xml_input READ-ONLY.
    DATA mo_xml_output TYPE REF TO zif_abapgitp_xml_output READ-ONLY.

    METHODS set_xml_input
      IMPORTING
        io_xml TYPE REF TO zif_abapgitp_xml_input.

    METHODS set_xml_output
      IMPORTING
        io_xml TYPE REF TO zif_abapgitp_xml_output.

  PROTECTED SECTION.
    CONSTANTS co_suffix_fieldcat TYPE string VALUE '_field_catalog'.
    METHODS create_table_descriptor
      IMPORTING
        it_field_catalog TYPE lif_external_object_container=>ty_s_table_content-field_catalog
      EXPORTING
        eo_tabledescr    TYPE REF TO cl_abap_tabledescr
      RAISING
        lcx_obj_exception.
ENDCLASS.

CLASS ltcl_catalog DEFINITION DEFERRED.

CLASS lcl_catalog DEFINITION FRIENDS ltcl_catalog.

  PUBLIC SECTION.
    CLASS-METHODS build
      IMPORTING iv_tobj_name      TYPE objsl-tobj_name
      RETURNING VALUE(rt_catalog) TYPE lif_external_object_container=>ty_t_component
      RAISING   lcx_obj_exception.

ENDCLASS.

CLASS lcl_tlogo_bridge DEFINITION.
* This class is inspired by CL_RSO_TLOGO_XML_BRIDGE which provides
* features to serialize a logical transport object based on its definition
* in SOBJ. Sadly, the original class hides some information needed in
* private tables and is not easily readable.
* This class shall be easily consumable and offer additional features
* such as existence checks etc.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_object      TYPE tadir-object "This is the name of the SOBJ-object (M1) which is the type of all objects derived from it
                iv_object_name TYPE sobj_name
      RAISING   lcx_obj_exception.

    METHODS export_object
      IMPORTING
                io_object_container TYPE REF TO lif_external_object_container
      RAISING   lcx_obj_exception.

    METHODS import_object
      IMPORTING
                io_object_container TYPE REF TO lif_external_object_container
      RAISING   lcx_obj_exception.

    METHODS instance_exists
      RETURNING VALUE(rv_exists) TYPE abap_bool
      RAISING   lcx_obj_exception.

    METHODS delete_object_on_db.

  PROTECTED SECTION.
    TYPES BEGIN OF ty_s_object_table.
    INCLUDE TYPE   objsl AS objsl.
    TYPES          field_catalog TYPE lif_external_object_container=>ty_t_component.
    TYPES END OF ty_s_object_table.

    TYPES ty_t_object_table TYPE SORTED TABLE OF ty_s_object_table
                                 WITH UNIQUE KEY objectname objecttype trwcount
                                 WITH NON-UNIQUE SORTED KEY table_name COMPONENTS tobj_name.

    TYPES ty_t_object_method TYPE SORTED TABLE OF objm WITH UNIQUE KEY objectname objecttype method.

    DATA mv_object                      TYPE objh-objectname. "This is the name of the SOBJ-object (M1) which is the type of all objects derived from it
    DATA mv_object_name                 TYPE sobj_name.
    DATA ms_object_header               TYPE objh.
    DATA mt_object_table                TYPE ty_t_object_table.
    DATA mt_object_method               TYPE ty_t_object_method.
    DATA mv_tolerate_additional_tables  TYPE abap_bool.
    DATA mv_tolerate_deviating_fields   TYPE abap_bool.
    DATA mv_tolerate_deviating_types    TYPE abap_bool.

    METHODS get_primary_table
      RETURNING
                VALUE(rs_object_table) TYPE ty_s_object_table
      RAISING   lcx_obj_exception.

    METHODS
      get_where_clause
        IMPORTING
                  iv_tobj_name            TYPE sobj_name
        RETURNING VALUE(rv_where_on_keys) TYPE string.

    METHODS before_export.
    METHODS after_import.
    METHODS get_current_tables_content
      IMPORTING
                io_object_container TYPE REF TO lif_external_object_container OPTIONAL
      EXPORTING
                et_table_content    TYPE lif_external_object_container=>ty_t_table_content
      RAISING   lcx_obj_exception.
    METHODS do_insert
      IMPORTING
        iv_table_name TYPE lif_external_object_container=>ty_s_table_content-tabname
        it_data       TYPE STANDARD TABLE.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_s_objkey,
             num   TYPE numc3,
             value TYPE char128,
           END OF ty_s_objkey,
           ty_t_objkey TYPE SORTED TABLE OF ty_s_objkey WITH UNIQUE KEY num.

    METHODS val_fieldcatalog_compatibility
      IMPORTING
                it_imported_fieldcatalog TYPE lif_external_object_container=>ty_t_component
                it_local_fieldcatalog    TYPE lif_external_object_container=>ty_t_component
      EXPORTING
                ev_is_identical          TYPE abap_bool
      RAISING   lcx_obj_exception.

    METHODS do_delete
      IMPORTING
        iv_table_name    TYPE lcl_tlogo_bridge=>ty_s_object_table-tobj_name
        iv_where_on_keys TYPE string.

    METHODS split_value_to_keys
      IMPORTING
        it_key_component TYPE lcl_tlogo_bridge=>ty_s_object_table-field_catalog
      CHANGING
        ct_objkey        TYPE lcl_tlogo_bridge=>ty_t_objkey
        cs_objkey        TYPE lcl_tlogo_bridge=>ty_s_objkey
        cv_non_value_pos TYPE numc3.

    METHODS distribute_name_to_components
      IMPORTING
        it_key_component TYPE lcl_tlogo_bridge=>ty_s_object_table-field_catalog
      CHANGING
        ct_objkey        TYPE lcl_tlogo_bridge=>ty_t_objkey
        cs_objkey        TYPE lcl_tlogo_bridge=>ty_s_objkey
        cv_non_value_pos TYPE numc3.

ENDCLASS.
