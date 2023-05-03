CLASS zcl_abapgit_object_sqsc DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_object.

    METHODS:
      constructor
        IMPORTING
          is_item     TYPE zif_abapgit_definitions=>ty_item
          iv_language TYPE spras
        RAISING
          zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    " Downport original structures from
    "   - IF_DBPROC_PROXY_UI
    "   - IF_DBPROC_PROXY_BASIC_TYPES

    TYPES:
      ty_db_name         TYPE c LENGTH 256,
      ty_abap_name       TYPE c LENGTH 30,
      ty_param_direction TYPE c LENGTH 10,
      ty_param_kind      TYPE c LENGTH 10,
      ty_ddic_name       TYPE ddobjname,

      BEGIN OF ty_db_simple_type_s,
        name   TYPE ty_db_name,
        length TYPE i,
        decs   TYPE i,
      END OF ty_db_simple_type_s,

      BEGIN OF ty_abap_simple_type_s,
        name   TYPE ty_abap_name,
        length TYPE i,
        decs   TYPE i,
      END OF ty_abap_simple_type_s,

      BEGIN OF ty_abap_simple_type_ui_s,
        typ  TYPE ty_abap_simple_type_s,
        text TYPE string,
      END OF ty_abap_simple_type_ui_s,

      BEGIN OF ty_header_ui_s,
        db_repository_package   TYPE ty_db_name,
        db_repository_proc_name TYPE ty_db_name,
        db_catalog_schema       TYPE ty_db_name,
        db_catalog_proc_name    TYPE ty_db_name,
        read_only               TYPE abap_bool,
        interface_pool          TYPE ty_abap_name,
      END OF  ty_header_ui_s,

      BEGIN OF ty_param_ui_s,
        position              TYPE i,
        db_name               TYPE ty_db_name,
        direction             TYPE ty_param_direction,
        kind                  TYPE ty_param_kind,
        db_table_type_schema  TYPE ty_db_name,
        db_table_type_name    TYPE ty_db_name,
        db_table_type_is_ddic TYPE abap_bool,
        transfer_table_schema TYPE ty_db_name,
        transfer_table_name   TYPE ty_db_name,
        abap_name             TYPE ty_abap_name,
        abap_name_is_ro       TYPE abap_bool,
        ddic_table            TYPE ty_ddic_name,
        ddic_table_is_ro      TYPE abap_bool,
      END OF  ty_param_ui_s,
      ty_param_ui_t            TYPE STANDARD TABLE OF ty_param_ui_s WITH KEY position,

      ty_abap_simple_type_ui_t TYPE STANDARD TABLE OF ty_abap_simple_type_ui_s WITH DEFAULT KEY,

      BEGIN OF ty_param_type_ui_s,
        param_position       TYPE i,
        comp_index           TYPE i,
        db_comp_name         TYPE ty_db_name,
        abap_comp_name       TYPE ty_abap_name,
        abap_comp_name_is_ro TYPE abap_bool,
        db_type              TYPE ty_db_simple_type_s,
        db_type_text         TYPE string,
        abap_type            TYPE ty_abap_simple_type_ui_s,
        abap_type_is_ro      TYPE abap_bool,
        abap_type_selection  TYPE ty_abap_simple_type_ui_t,
        ddic_type            TYPE ty_ddic_name,
        ddic_type_is_ro      TYPE abap_bool,
      END OF ty_param_type_ui_s ,
      ty_param_type_ui_t TYPE STANDARD TABLE OF ty_param_type_ui_s WITH KEY param_position comp_index,

      BEGIN OF ty_proxy,
        description     TYPE ddtext,
        header          TYPE ty_header_ui_s,
        parameters      TYPE ty_param_ui_t,
        parameter_types TYPE ty_param_type_ui_t,
      END OF ty_proxy.

    DATA:
      mo_proxy TYPE REF TO object.

    METHODS:
      delete_interface_if_it_exists
        IMPORTING
          iv_package   TYPE devclass
          iv_transport TYPE trkorr
          iv_interface TYPE ty_abap_name
        RAISING
          zcx_abapgit_exception.
ENDCLASS.



CLASS zcl_abapgit_object_sqsc IMPLEMENTATION.


  METHOD constructor.

    FIELD-SYMBOLS: <lv_dbproxyname> TYPE ty_abap_name.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    TRY.
        CREATE OBJECT mo_proxy
          TYPE ('CL_DDIC_WB_DBPROC_PROXY').

        ASSIGN ('MO_PROXY->IF_DDIC_WB_DBPROC_PROXY~DBPROXYNAME')
            TO <lv_dbproxyname>.
        ASSERT sy-subrc = 0.

      CATCH cx_root.
        zcx_abapgit_exception=>raise( |SQSC not supported| ).
    ENDTRY.

    <lv_dbproxyname> = ms_item-obj_name.

  ENDMETHOD.


  METHOD delete_interface_if_it_exists.

    DATA: ls_item      TYPE zif_abapgit_definitions=>ty_item,
          lo_interface TYPE REF TO zcl_abapgit_object_intf.

    " The interface is managed by the proxy. If abapGit
    " has created it before we have to delete it. Otherwise
    " if_dbproc_proxy_ui~create will throw errors.

    ls_item-obj_name = iv_interface.
    ls_item-obj_type = 'INTF'.

    IF zcl_abapgit_objects=>exists( ls_item ) = abap_true.

      CREATE OBJECT lo_interface
        EXPORTING
          is_item     = ls_item
          iv_language = mv_language.

      lo_interface->zif_abapgit_object~delete( iv_package   = iv_package
                                               iv_transport = iv_transport ).

    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    rv_user = c_user_unknown.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lx_error TYPE REF TO cx_root.

    TRY.
        CALL METHOD mo_proxy->('IF_DBPROC_PROXY_UI~DELETE')
          EXPORTING
            if_transport_req = iv_transport.

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_proxy TYPE ty_proxy,
          lx_error TYPE REF TO cx_root.

    io_xml->read(
      EXPORTING
        iv_name = 'SQSC'
      CHANGING
        cg_data = ls_proxy ).

    IF zif_abapgit_object~exists( ) = abap_false.

      delete_interface_if_it_exists(
          iv_package   = iv_package
          iv_transport = iv_transport
          iv_interface = ls_proxy-header-interface_pool ).

      CALL METHOD mo_proxy->('IF_DBPROC_PROXY_UI~CREATE')
        EXPORTING
          if_interface_pool = ls_proxy-header-interface_pool
          if_transport_req  = iv_transport
          if_package        = iv_package
          if_langu          = mv_language.

    ENDIF.

    TRY.
        CALL METHOD mo_proxy->('IF_DBPROC_PROXY_UI~WRITE_TO_SOURCE')
          EXPORTING
            if_transport_req  = iv_transport
            is_header         = ls_proxy-header
            it_parameter      = ls_proxy-parameters
            it_parameter_type = ls_proxy-parameter_types.

        CALL METHOD mo_proxy->('IF_DBPROC_PROXY_UI~WRITE_DESCR')
          EXPORTING
            if_langu = mv_language
            if_descr = ls_proxy-description.

        CALL METHOD mo_proxy->('IF_DBPROC_PROXY_UI~ACTIVATE').

        tadir_insert( iv_package ).

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    CALL METHOD mo_proxy->('IF_DBPROC_PROXY_UI~EXISTS')
      RECEIVING
        ef_exists = rv_bool.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_ADT_LINK=>JUMP
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: ls_proxy TYPE ty_proxy,
          lx_error TYPE REF TO cx_root.

    TRY.
        CALL METHOD mo_proxy->('IF_DBPROC_PROXY_UI~READ_FROM_SOURCE')
          EXPORTING
            if_version        = 'A'
          IMPORTING
            es_header         = ls_proxy-header
            et_parameter      = ls_proxy-parameters
            et_parameter_type = ls_proxy-parameter_types.

        CALL METHOD mo_proxy->('IF_DBPROC_PROXY_UI~READ_DESCR')
          EXPORTING
            if_langu   = mv_language
            if_version = 'A'
          IMPORTING
            ef_descr   = ls_proxy-description.

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    io_xml->add( iv_name = 'SQSC'
                 ig_data = ls_proxy ).

  ENDMETHOD.
ENDCLASS.
