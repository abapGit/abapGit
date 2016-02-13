CLASS lcl_rso_tlogo_xml_bridge DEFINITION
  CREATE PUBLIC .

**************************************************************************
* This is slightly modified-copy of CL_RSO_TLOGO_XML_BRIDGE which provides
* features to serialize a logical transport object based on its definition
* in SOBJ. Sadly, the original class hides some information needed in
* private tables. The modification done is to move all this content into
* the protected one.

*"* public components of class CL_RSO_TLOGO_XML_BRIDGE
*"* do not include other source files here!!!
  PUBLIC SECTION.
    TYPE-POOLS rs .
    INTERFACE if_rso_object_xmic LOAD .
    INTERFACE if_rso_repository_xml_const LOAD .

    CONSTANTS p_c_xml_tag_transport TYPE char40
   VALUE 'transport'.                                     "#EC NOTEXT .

    CLASS-METHODS class_constructor .
    METHODS activate .
    METHODS compose_xml
      IMPORTING
        !i_objnm      TYPE sobj_name
        !i_objvers    TYPE rsobjvers DEFAULT rs_c_objvers-active
      EXPORTING
        !e_string_xml TYPE string .
    METHODS constructor
      IMPORTING
        !i_tlogo TYPE rstlogo
        iv_include_last_changed type abap_bool optional
      EXCEPTIONS
        tlogo_doesnt_exist .
    METHODS parse_xml
      IMPORTING
        !i_objnm      TYPE sobj_name
        !i_string_xml TYPE string
        !i_detlevel   TYPE bal_s_msg-detlevel DEFAULT '1'
      EXPORTING
        !e_subrc      TYPE sysubrc .
    METHODS save
      IMPORTING
        !i_detlevel TYPE bal_s_msg-detlevel DEFAULT '1'
      EXPORTING
        !e_subrc    TYPE sysubrc .
    METHODS get_timestmp_of_data
      RETURNING
        VALUE(r_timestmp) TYPE rstimestmp .

  PROTECTED SECTION.
*"* protected components of class CL_RSO_TLOGO_XML_BRIDGE
*"* do not include other source files here!!!

    METHODS before_export .
    " private section.
*"* private components of class CL_RSO_TLOGO_XML_BRIDGE
*"* do not include other source files here!!!

    TYPES:
      pt_t_objsl TYPE STANDARD TABLE OF objsl .
    TYPES:
      pt_ts_objsl TYPE SORTED TABLE OF objsl
        WITH UNIQUE KEY objectname objecttype trwcount .
    TYPES:
      BEGIN OF pt_s_tlogo_tables,
        tabname      TYPE ddobjname,
        where_clause TYPE string,
        data         TYPE REF TO data,
      END OF pt_s_tlogo_tables. .
    TYPES:
      pt_t_tlogo_tables TYPE STANDARD TABLE OF pt_s_tlogo_tables .
    TYPES:
      pt_ts_tlogo_tables TYPE SORTED TABLE OF pt_s_tlogo_tables
        WITH UNIQUE KEY tabname .
    TYPES:
      pt_t_objm TYPE STANDARD TABLE OF objm .
    TYPES:
      pt_ts_objm TYPE SORTED TABLE OF objm
        WITH UNIQUE KEY objectname objecttype method .

    CONSTANTS p_c_xml_tag_field TYPE char40 VALUE 'field' .
    CONSTANTS p_c_xml_tag_key TYPE char40 VALUE 'key' .
    CONSTANTS p_c_xml_tag_row TYPE char40 VALUE 'row' .
    CONSTANTS p_c_xml_tag_table TYPE char40 VALUE 'table' .
    CLASS-DATA p_r_ixml TYPE REF TO if_ixml .
    CLASS-DATA p_r_repository TYPE REF TO cl_rso_repository .
    CLASS-DATA p_r_stream_factory TYPE REF TO if_ixml_stream_factory .
    DATA p_objnm TYPE sobj_name .
    DATA p_s_objh TYPE objh .
    DATA p_s_tlogoprop TYPE rstlogoprop .
    DATA p_tlogo TYPE rstlogo .
    DATA p_ts_objm TYPE pt_ts_objm .
    DATA p_ts_objsl TYPE pt_ts_objsl .
    DATA p_ts_tlogo_tables TYPE pt_ts_tlogo_tables .
    DATA p_timestmp TYPE rstimestmp .
    data mv_include_last_changed type abap_bool.

    METHODS read_tlogo_prop .

    METHODS do_delete
      IMPORTING
        iv_tlogo_table  TYPE lcl_rso_tlogo_xml_bridge=>pt_s_tlogo_tables-tabname
        iv_where_clause type lcl_rso_tlogo_xml_bridge=>pt_s_tlogo_tables-where_clause.

    METHODS do_insert
      IMPORTING
        iv_tlogo_table  TYPE lcl_rso_tlogo_xml_bridge=>pt_s_tlogo_tables-tabname
        it_data         TYPE STANDARD TABLE.
ENDCLASS.

CLASS lcl_tlogo_xml_bridge DEFINITION INHERITING FROM lcl_rso_tlogo_xml_bridge
  CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_obj_metadata,
             db_table          TYPE tabname,
             count             TYPE i,
             fields_definition TYPE dfies_table,
           END OF ty_obj_metadata,
           tt_obj_metadata TYPE SORTED TABLE OF ty_obj_metadata WITH UNIQUE KEY db_table.

    METHODS
      exists
        IMPORTING
                  iv_objname              TYPE objname
        RETURNING VALUE(rv_object_exists) TYPE abap_bool.

    METHODS delete_object
      IMPORTING
                iv_objname        TYPE objname
      RETURNING VALUE(rv_deleted) TYPE abap_bool.

    METHODS get_table_metadata
      IMPORTING
        iv_objname         TYPE objname
      RETURNING
        VALUE(rt_metadata) TYPE lcl_tlogo_xml_bridge=>tt_obj_metadata.

  PROTECTED SECTION.
    METHODS
      get_where_clause
        IMPORTING
                  iv_tobj_name            TYPE sobj_name
                  iv_objname              TYPE objname
        RETURNING VALUE(rv_where_on_keys) TYPE string.

    METHODS validate_count_prim_table
      IMPORTING
        iv_objname              TYPE objname
        iv_dbcount              LIKE sy-dbcnt.
  PRIVATE SECTION.

    METHODS timestamp_to_xml
      IMPORTING iv_timestamp     TYPE timestamp
      RETURNING VALUE(rv_string) TYPE string.

    METHODS get_prim_table_objsl
      RETURNING
        VALUE(rs_objsl) LIKE line of p_ts_objsl.
ENDCLASS.