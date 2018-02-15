*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECTS
*&---------------------------------------------------------------------*

**********************************************************************
* Enable plugins

*CLASS lcl_objects_bridge DEFINITION INHERITING FROM zcl_abapgit_objects_super FINAL.
*
*  PUBLIC SECTION.
*
*    CLASS-METHODS class_constructor.
*
*    METHODS constructor
*      IMPORTING is_item TYPE zif_abapgit_definitions=>ty_item
*      RAISING   cx_sy_create_object_error.
*
*    INTERFACES zif_abapgit_object.
*    ALIASES mo_files FOR zif_abapgit_object~mo_files.
*
*  PRIVATE SECTION.
*    DATA: mo_plugin TYPE REF TO object.
*
*    TYPES: BEGIN OF ty_s_objtype_map,
*             obj_typ      TYPE trobjtype,
*             plugin_class TYPE seoclsname,
*           END OF ty_s_objtype_map,
*           ty_t_objtype_map TYPE SORTED TABLE OF ty_s_objtype_map WITH UNIQUE KEY obj_typ.
*
*    CLASS-DATA gt_objtype_map TYPE ty_t_objtype_map.
*
*ENDCLASS.                    "lcl_objects_bridge DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_objects_bridge IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
*CLASS lcl_objects_bridge IMPLEMENTATION.
*
*  METHOD zif_abapgit_object~has_changed_since.
*    rv_changed = abap_true.
*  ENDMETHOD.  "lif_object~has_changed_since
*
*  METHOD zif_abapgit_object~get_metadata.
*
*    CALL METHOD mo_plugin->('ZIF_ABAPGITP_PLUGIN~GET_METADATA')
*      RECEIVING
*        rs_metadata = rs_metadata.
*
*  ENDMETHOD.                    "lif_object~get_metadata
*
*  METHOD zif_abapgit_object~changed_by.
*    rv_user = c_user_unknown. " todo
*  ENDMETHOD.
*
*  METHOD constructor.
*
*    DATA ls_objtype_map LIKE LINE OF gt_objtype_map.
*
*    super->constructor( is_item = is_item
*                        iv_language = zif_abapgit_definitions=>gc_english ).
*
**    determine the responsible plugin
*    READ TABLE gt_objtype_map INTO ls_objtype_map
*      WITH TABLE KEY obj_typ = is_item-obj_type.
*    IF sy-subrc = 0.
*      CREATE OBJECT mo_plugin TYPE (ls_objtype_map-plugin_class).
*
*      CALL METHOD mo_plugin->('SET_ITEM')
*        EXPORTING
*          iv_obj_type = is_item-obj_type
*          iv_obj_name = is_item-obj_name.
*    ELSE.
*      RAISE EXCEPTION TYPE cx_sy_create_object_error
*        EXPORTING
*          classname = 'LCL_OBJECTS_BRIDGE'.
*    ENDIF.
*  ENDMETHOD.                    "constructor
*
*  METHOD zif_abapgit_object~serialize.
*
*    CALL METHOD mo_plugin->('WRAP_SERIALIZE')
*      EXPORTING
*        io_xml = io_xml.
*
*  ENDMETHOD.                    "lif_object~serialize
*
*  METHOD zif_abapgit_object~deserialize.
*
*    DATA: lx_plugin        TYPE REF TO cx_static_check.
*
*    TRY.
*        CALL METHOD mo_plugin->('WRAP_DESERIALIZE')
*          EXPORTING
*            iv_package = iv_package
*            io_xml     = io_xml.
*      CATCH cx_static_check INTO lx_plugin.
*        zcx_abapgit_exception=>raise( lx_plugin->get_text( ) ).
*    ENDTRY.
*  ENDMETHOD.                    "lif_object~deserialize
*
*  METHOD zif_abapgit_object~delete.
*    DATA lx_plugin TYPE REF TO cx_static_check.
*
*    TRY.
*        CALL METHOD mo_plugin->('ZIF_ABAPGITP_PLUGIN~DELETE').
*      CATCH cx_static_check INTO lx_plugin.
*        zcx_abapgit_exception=>raise( lx_plugin->get_text( ) ).
*    ENDTRY.
*
*  ENDMETHOD.                    "lif_object~delete
*
*  METHOD zif_abapgit_object~exists.
*
*    CALL METHOD mo_plugin->('ZIF_ABAPGITP_PLUGIN~EXISTS')
*      RECEIVING
*        rv_bool = rv_bool.
*
*  ENDMETHOD.                    "lif_object~exists
*
*  METHOD zif_abapgit_object~jump.
*
*    CALL METHOD mo_plugin->('ZIF_ABAPGITP_PLUGIN~JUMP').
*
*  ENDMETHOD.                    "lif_object~jump
*
*  METHOD class_constructor.
*
*    DATA lt_plugin_class    TYPE STANDARD TABLE OF seoclsname WITH DEFAULT KEY.
*    DATA lv_plugin_class    LIKE LINE OF lt_plugin_class.
*    DATA lo_plugin          TYPE REF TO object.
*    DATA lt_plugin_obj_type TYPE objtyptable.
*    DATA ls_objtype_map     LIKE LINE OF gt_objtype_map.
*
*
*    SELECT ext~clsname
*      FROM vseoextend AS ext
*      INTO TABLE lt_plugin_class
*      WHERE ext~refclsname LIKE 'ZCL_ABAPGITP_OBJECT%'
*      AND ext~version = '1'.                              "#EC CI_SUBRC
*
*    CLEAR gt_objtype_map.
*    LOOP AT lt_plugin_class INTO lv_plugin_class
*        WHERE table_line <> 'ZCL_ABAPGITP_OBJECT_BY_SOBJ'.
** have the generic plugin only as fallback
*      TRY.
*          CREATE OBJECT lo_plugin TYPE (lv_plugin_class).
*        CATCH cx_sy_create_object_error.
*          CONTINUE. ">>>>>>>>>>>>>>
*      ENDTRY.
*
*      CALL METHOD lo_plugin->('GET_SUPPORTED_OBJ_TYPES')
*        IMPORTING
*          rt_obj_type = lt_plugin_obj_type.
*
*      ls_objtype_map-plugin_class = lv_plugin_class.
*      LOOP AT lt_plugin_obj_type INTO ls_objtype_map-obj_typ.
*        INSERT ls_objtype_map INTO TABLE gt_objtype_map.
*        IF sy-subrc <> 0.
** No exception in class-contructor possible.
** Anyway, a shortdump is more appropriate in this case
*          ASSERT 'There must not be' =
*            |multiple abapGit-Plugins for the same object type {
*            ls_objtype_map-obj_typ }|.
*        ENDIF.
*      ENDLOOP.
*    ENDLOOP. "at plugins
*
** and the same for the generic plugin if exists
** have the generic plugin only as fallback
*    LOOP AT lt_plugin_class INTO lv_plugin_class
*        WHERE table_line = 'ZCL_ABAPGITP_OBJECT_BY_SOBJ'.
*      CREATE OBJECT lo_plugin TYPE (lv_plugin_class).
*
*      CALL METHOD lo_plugin->('GET_SUPPORTED_OBJ_TYPES')
*        RECEIVING
*          rt_obj_type = lt_plugin_obj_type.
*
*      ls_objtype_map-plugin_class = lv_plugin_class.
*      LOOP AT lt_plugin_obj_type INTO ls_objtype_map-obj_typ.
*        INSERT ls_objtype_map INTO TABLE gt_objtype_map. "knowingly ignore the subrc
*      ENDLOOP.
*    ENDLOOP. "at plugins
*
*  ENDMETHOD.                    "class_constructor
*
*  METHOD zif_abapgit_object~compare_to_remote_version.
*    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
*  ENDMETHOD.
*
*ENDCLASS.                    "lcl_objects_bridge IMPLEMENTATION

**********************************************************************

*----------------------------------------------------------------------*
*       CLASS lcl_object DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
*CLASS lcl_objects DEFINITION FINAL.
*
*  PUBLIC SECTION.
*    TYPES: ty_types_tt TYPE STANDARD TABLE OF tadir-object WITH DEFAULT KEY.
*
*    TYPES: BEGIN OF ty_deserialization,
*             obj     TYPE REF TO zif_abapgit_object,
*             xml     TYPE REF TO zcl_abapgit_xml_input,
*             package TYPE devclass,
*             item    TYPE zif_abapgit_definitions=>ty_item,
*           END OF ty_deserialization.
*
*    TYPES: ty_deserialization_tt TYPE STANDARD TABLE OF ty_deserialization WITH DEFAULT KEY.
*
*    CLASS-METHODS serialize
*      IMPORTING is_item         TYPE zif_abapgit_definitions=>ty_item
*                iv_language     TYPE spras
*                io_log          TYPE REF TO zcl_abapgit_log OPTIONAL
*      RETURNING VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_files_tt
*      RAISING   zcx_abapgit_exception.
*
*    CLASS-METHODS deserialize
*      IMPORTING io_repo                  TYPE REF TO lcl_repo
*      RETURNING VALUE(rt_accessed_files) TYPE zif_abapgit_definitions=>ty_file_signatures_tt
*      RAISING   zcx_abapgit_exception.
*
*    CLASS-METHODS delete
*      IMPORTING it_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt
*      RAISING   zcx_abapgit_exception.
*
*    CLASS-METHODS jump
*      IMPORTING is_item TYPE zif_abapgit_definitions=>ty_item
*      RAISING   zcx_abapgit_exception.
*
*    CLASS-METHODS changed_by
*      IMPORTING is_item        TYPE zif_abapgit_definitions=>ty_item
*      RETURNING VALUE(rv_user) TYPE xubname
*      RAISING   zcx_abapgit_exception.
*
*    CLASS-METHODS has_changed_since
*      IMPORTING is_item           TYPE zif_abapgit_definitions=>ty_item
*                iv_timestamp      TYPE timestamp
*      RETURNING VALUE(rv_changed) TYPE abap_bool
*      RAISING   zcx_abapgit_exception.
*
*    CLASS-METHODS is_supported
*      IMPORTING is_item        TYPE zif_abapgit_definitions=>ty_item
*                iv_native_only TYPE abap_bool DEFAULT abap_false
*      RETURNING VALUE(rv_bool) TYPE abap_bool.
*
*    CLASS-METHODS exists
*      IMPORTING is_item        TYPE zif_abapgit_definitions=>ty_item
*      RETURNING VALUE(rv_bool) TYPE abap_bool.
*
*    CLASS-METHODS supported_list
*      RETURNING VALUE(rt_types) TYPE ty_types_tt.
*
*  PRIVATE SECTION.
*
*    CLASS-METHODS check_duplicates
*      IMPORTING it_files TYPE zif_abapgit_definitions=>ty_files_tt
*      RAISING   zcx_abapgit_exception.
*
*    CLASS-METHODS create_object
*      IMPORTING is_item        TYPE zif_abapgit_definitions=>ty_item
*                iv_language    TYPE spras
*                is_metadata    TYPE zif_abapgit_definitions=>ty_metadata OPTIONAL
*                iv_native_only TYPE abap_bool DEFAULT abap_false
*      RETURNING VALUE(ri_obj)  TYPE REF TO zif_abapgit_object
*      RAISING   zcx_abapgit_exception.
*
*    CLASS-METHODS
*      prioritize_deser
*        IMPORTING it_results        TYPE zif_abapgit_definitions=>ty_results_tt
*        RETURNING VALUE(rt_results) TYPE zif_abapgit_definitions=>ty_results_tt.
*
*    CLASS-METHODS class_name
*      IMPORTING is_item              TYPE zif_abapgit_definitions=>ty_item
*      RETURNING VALUE(rv_class_name) TYPE string.
*
*    CLASS-METHODS warning_overwrite
*      CHANGING ct_results TYPE zif_abapgit_definitions=>ty_results_tt
*      RAISING  zcx_abapgit_exception.
*
*    CLASS-METHODS warning_package
*      IMPORTING is_item          TYPE zif_abapgit_definitions=>ty_item
*                iv_package       TYPE devclass
*      RETURNING VALUE(rv_cancel) TYPE abap_bool
*      RAISING   zcx_abapgit_exception.
*
*    CLASS-METHODS update_package_tree
*      IMPORTING iv_package TYPE devclass.
*
*    CLASS-METHODS delete_obj
*      IMPORTING is_item TYPE zif_abapgit_definitions=>ty_item
*      RAISING   zcx_abapgit_exception.
*
*    CLASS-METHODS compare_remote_to_local
*      IMPORTING
*        io_object TYPE REF TO zif_abapgit_object
*        it_remote TYPE zif_abapgit_definitions=>ty_files_tt
*        is_result TYPE zif_abapgit_definitions=>ty_result
*      RAISING
*        zcx_abapgit_exception.
*
*    CLASS-METHODS deserialize_objects
*      IMPORTING it_objects TYPE ty_deserialization_tt
*                iv_ddic    TYPE abap_bool DEFAULT abap_false
*                iv_descr   TYPE string
*      CHANGING  ct_files   TYPE zif_abapgit_definitions=>ty_file_signatures_tt
*      RAISING   zcx_abapgit_exception.
*
*ENDCLASS.                    "lcl_object DEFINITION
