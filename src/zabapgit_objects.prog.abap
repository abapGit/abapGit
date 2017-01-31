*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECTS
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_objects_activation DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects_activation DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS add
      IMPORTING iv_type TYPE trobjtype
                iv_name TYPE clike
      RAISING   lcx_exception.

    CLASS-METHODS add_item
      IMPORTING is_item TYPE ty_item
      RAISING   lcx_exception.

    CLASS-METHODS activate
      IMPORTING iv_ddic TYPE abap_bool DEFAULT abap_false
      RAISING   lcx_exception.

    CLASS-METHODS clear.

  PRIVATE SECTION.
    CLASS-METHODS fix_class_methods
      IMPORTING iv_obj_name TYPE trobj_name
      CHANGING  ct_objects  TYPE dwinactiv_tab.

    CLASS-DATA: gt_objects TYPE TABLE OF dwinactiv.

ENDCLASS.                    "lcl_objects_activation DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_objects_activation IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects_activation IMPLEMENTATION.

  METHOD add_item.
    add( iv_type = is_item-obj_type
         iv_name = is_item-obj_name ).
  ENDMETHOD.                    "add_item

  METHOD clear.
    CLEAR gt_objects.
  ENDMETHOD.                    "clear

  METHOD activate.

    IF NOT gt_objects IS INITIAL.
      CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
        EXPORTING
          activate_ddic_objects  = iv_ddic
          with_popup             = abap_true
        TABLES
          objects                = gt_objects
        EXCEPTIONS
          excecution_error       = 1
          cancelled              = 2
          insert_into_corr_error = 3
          OTHERS                 = 4.
      IF sy-subrc <> 0.
        lcx_exception=>raise( 'error from RS_WORKING_OBJECTS_ACTIVATE' ).
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "activate

  METHOD fix_class_methods.
* function module RS_WORKING_OBJECTS_ACTIVATE assumes that
* METH lines contains spaces between class and method name
* however, classes named with 30 characters
* eg. ZCL_CLAS_TESTTESTTESTTESTTESTT
* this will not be true, so find all the method includes instead

    DATA: lt_methods TYPE seop_methods_w_include,
          lv_class   TYPE seoclsname.

    FIELD-SYMBOLS: <ls_method> LIKE LINE OF lt_methods,
                   <ls_object> LIKE LINE OF ct_objects.


    lv_class = iv_obj_name.

    cl_oo_classname_service=>get_all_method_includes(
      EXPORTING
        clsname            = lv_class
      RECEIVING
        result             = lt_methods
      EXCEPTIONS
        class_not_existing = 1
        OTHERS             = 2 ).
    ASSERT sy-subrc = 0.
    DELETE ct_objects WHERE object = 'METH'.
    LOOP AT lt_methods ASSIGNING <ls_method>.
      APPEND INITIAL LINE TO ct_objects ASSIGNING <ls_object>.
      <ls_object>-object = 'METH'.
      <ls_object>-obj_name = <ls_method>-incname.
    ENDLOOP.

  ENDMETHOD.

  METHOD add.

* function group SEWORKINGAREA
* function module RS_INSERT_INTO_WORKING_AREA
* class CL_WB_ACTIVATION_WORK_AREA

    DATA: lt_objects  TYPE dwinactiv_tab,
          lv_obj_name TYPE dwinactiv-obj_name.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF lt_objects.


    lv_obj_name = iv_name.

    CASE iv_type.
      WHEN 'CLAS' OR 'WDYN'.
* todo, move this to the object type include instead
        CALL FUNCTION 'RS_INACTIVE_OBJECTS_IN_OBJECT'
          EXPORTING
            obj_name         = lv_obj_name
            object           = iv_type
          TABLES
            inactive_objects = lt_objects
          EXCEPTIONS
            object_not_found = 1
            OTHERS           = 2.
        IF sy-subrc <> 0.
          lcx_exception=>raise( 'Error from RS_INACTIVE_OBJECTS_IN_OBJECT' ).
        ENDIF.

        IF iv_type = 'CLAS'.
          fix_class_methods( EXPORTING iv_obj_name = lv_obj_name
                             CHANGING ct_objects = lt_objects ).
        ENDIF.

        APPEND LINES OF lt_objects TO gt_objects.
      WHEN OTHERS.
        APPEND INITIAL LINE TO gt_objects ASSIGNING <ls_object>.
        <ls_object>-object   = iv_type.
        <ls_object>-obj_name = lv_obj_name.
    ENDCASE.

  ENDMETHOD.                    "activate

ENDCLASS.                    "lcl_objects_activation IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_objects_files DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects_files DEFINITION.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING is_item TYPE ty_item,
      add_string
        IMPORTING iv_extra  TYPE clike OPTIONAL
                  iv_ext    TYPE string
                  iv_string TYPE string
        RAISING   lcx_exception,
      read_string
        IMPORTING iv_extra         TYPE clike OPTIONAL
                  iv_ext           TYPE string
        RETURNING VALUE(rv_string) TYPE string
        RAISING   lcx_exception,
      add_xml
        IMPORTING iv_extra     TYPE clike OPTIONAL
                  io_xml       TYPE REF TO lcl_xml_output
                  iv_normalize TYPE sap_bool DEFAULT abap_true
                  is_metadata  TYPE ty_metadata OPTIONAL
        RAISING   lcx_exception,
* needed since type-check during dynamic call fails even if the object is compatible
      add_xml_from_plugin
        IMPORTING iv_extra     TYPE clike OPTIONAL
                  io_xml       TYPE REF TO object
                  iv_normalize TYPE sap_bool DEFAULT abap_true
        RAISING   lcx_exception ##called,
      read_xml
        IMPORTING iv_extra      TYPE clike OPTIONAL
        RETURNING VALUE(ro_xml) TYPE REF TO lcl_xml_input
        RAISING   lcx_exception,
      read_abap
        IMPORTING iv_extra       TYPE clike OPTIONAL
                  iv_error       TYPE sap_bool DEFAULT abap_true
        RETURNING VALUE(rt_abap) TYPE abaptxt255_tab
        RAISING   lcx_exception,
      add_abap
        IMPORTING iv_extra TYPE clike OPTIONAL
                  it_abap  TYPE STANDARD TABLE
        RAISING   lcx_exception,
      add
        IMPORTING is_file TYPE ty_file,
      add_raw
        IMPORTING iv_extra TYPE clike OPTIONAL
                  iv_ext   TYPE string
                  iv_data  TYPE xstring
        RAISING   lcx_exception,
      read_raw
        IMPORTING iv_extra       TYPE clike OPTIONAL
                  iv_ext         TYPE string
        RETURNING VALUE(rv_data) TYPE xstring
        RAISING   lcx_exception,
      get_files
        RETURNING VALUE(rt_files) TYPE ty_files_tt,
      set_files
        IMPORTING it_files TYPE ty_files_tt,
      get_accessed_files
        RETURNING VALUE(rt_files) TYPE ty_file_signatures_tt.

  PRIVATE SECTION.
    DATA: ms_item           TYPE ty_item,
          mt_accessed_files TYPE ty_file_signatures_tt,
          mt_files          TYPE ty_files_tt.

    METHODS:
      read_file
        IMPORTING iv_filename TYPE string
                  iv_error    TYPE abap_bool DEFAULT abap_true
        EXPORTING ev_data     TYPE xstring
        RAISING   lcx_exception,
      filename
        IMPORTING iv_extra           TYPE clike OPTIONAL
                  iv_ext             TYPE string
        RETURNING VALUE(rv_filename) TYPE string.

ENDCLASS.                    "lcl_objects_files DEFINITION

INTERFACE lif_object_comparison_result.
  METHODS:
    show_confirmation_dialog,
    is_result_complete_halt
      RETURNING VALUE(rv_response) TYPE abap_bool.

ENDINTERFACE.

"Null Object Pattern
CLASS lcl_null_comparison_result DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_object_comparison_result.
ENDCLASS.
CLASS lcl_null_comparison_result IMPLEMENTATION.

  METHOD lif_object_comparison_result~is_result_complete_halt.
    rv_response = abap_false.
  ENDMETHOD.

  METHOD lif_object_comparison_result~show_confirmation_dialog.
    RETURN.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       INTERFACE lif_object DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
INTERFACE lif_object.

  METHODS:
    serialize
      IMPORTING io_xml TYPE REF TO lcl_xml_output
      RAISING   lcx_exception,
    deserialize
      IMPORTING iv_package TYPE devclass
                io_xml     TYPE REF TO lcl_xml_input
      RAISING   lcx_exception,
    delete
      RAISING lcx_exception,
    exists
      RETURNING VALUE(rv_bool) TYPE abap_bool
      RAISING   lcx_exception,
    changed_by
      RETURNING VALUE(rv_user) TYPE xubname
      RAISING   lcx_exception,
    jump
      RAISING lcx_exception,
    get_metadata
      RETURNING VALUE(rs_metadata) TYPE ty_metadata,
    has_changed_since
      IMPORTING iv_timestamp      TYPE timestamp
      RETURNING VALUE(rv_changed) TYPE abap_bool
      RAISING   lcx_exception.
  METHODS:
    compare_to_remote_version
      IMPORTING io_remote_version_xml       TYPE REF TO lcl_xml_input
      RETURNING VALUE(ro_comparison_result) TYPE REF TO lif_object_comparison_result
      RAISING   lcx_exception.

  DATA: mo_files TYPE REF TO lcl_objects_files.

ENDINTERFACE.                    "lif_object DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_objects_files IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects_files IMPLEMENTATION.

  METHOD constructor.
    ms_item = is_item.
  ENDMETHOD.                    "constructor

  METHOD add.
    APPEND is_file TO mt_files.
  ENDMETHOD.                    "add

  METHOD get_files.
    rt_files = mt_files.
  ENDMETHOD.                    "get_files

  METHOD set_files.
    mt_files = it_files.
  ENDMETHOD.                    "set_files

  METHOD get_accessed_files.
    rt_files = mt_accessed_files.
  ENDMETHOD.  " get_accessed_files.

  METHOD read_string.

    DATA: lv_filename TYPE string,
          lv_data     TYPE xstring.

    lv_filename = filename( iv_extra = iv_extra
                            iv_ext   = iv_ext ).            "#EC NOTEXT

    read_file( EXPORTING iv_filename = lv_filename
               IMPORTING ev_data     = lv_data ).

    rv_string = lcl_convert=>xstring_to_string_utf8( lv_data ).

  ENDMETHOD.                    "read_string

  METHOD read_abap.

    DATA: lv_filename TYPE string,
          lv_data     TYPE xstring,
          lv_abap     TYPE string.


    lv_filename = filename( iv_extra = iv_extra
                            iv_ext   = 'abap' ).            "#EC NOTEXT

    read_file( EXPORTING iv_filename = lv_filename
                         iv_error    = iv_error
               IMPORTING ev_data     = lv_data ).

    IF lv_data IS INITIAL. " Post-handling of iv_error = false
      RETURN.
    ENDIF.

    lv_abap = lcl_convert=>xstring_to_string_utf8( lv_data ).

    SPLIT lv_abap AT gc_newline INTO TABLE rt_abap.

  ENDMETHOD.                    "read_abap

  METHOD add_abap.

    DATA: lv_source TYPE string,
          ls_file   TYPE ty_file.


    CONCATENATE LINES OF it_abap INTO lv_source SEPARATED BY gc_newline.
* when editing files via eg. GitHub web interface it adds a newline at end of file
    lv_source = lv_source && gc_newline.

    ls_file-path = '/'.
    ls_file-filename = filename( iv_extra = iv_extra
                                 iv_ext   = 'abap' ).       "#EC NOTEXT
    ls_file-data = lcl_convert=>string_to_xstring_utf8( lv_source ).

    APPEND ls_file TO mt_files.

  ENDMETHOD.                    "abap_to_file

  METHOD add_string.

    DATA: ls_file TYPE ty_file.


    ls_file-path = '/'.
    ls_file-filename = filename( iv_extra = iv_extra
                                 iv_ext   = iv_ext ).       "#EC NOTEXT
    ls_file-data = lcl_convert=>string_to_xstring_utf8( iv_string ).

    APPEND ls_file TO mt_files.

  ENDMETHOD.                    "add_string

  METHOD add_xml.

    DATA: lv_xml  TYPE string,
          ls_file TYPE ty_file.


    lv_xml = io_xml->render( iv_normalize = iv_normalize
                             is_metadata = is_metadata ).
    ls_file-path = '/'.

    ls_file-filename = filename( iv_extra = iv_extra
                                 iv_ext   = 'xml' ).        "#EC NOTEXT

    REPLACE FIRST OCCURRENCE
      OF REGEX '<\?xml version="1\.0" encoding="[\w-]+"\?>'
      IN lv_xml
      WITH '<?xml version="1.0" encoding="utf-8"?>'.
    ASSERT sy-subrc = 0.

    ls_file-data = lcl_convert=>string_to_xstring_utf8( lv_xml ).

    APPEND ls_file TO mt_files.

  ENDMETHOD.                    "do

  METHOD read_xml.

    DATA: lv_filename TYPE string,
          lv_data     TYPE xstring,
          lv_xml      TYPE string.

    lv_filename = filename( iv_extra = iv_extra
                            iv_ext   = 'xml' ).             "#EC NOTEXT

    read_file( EXPORTING iv_filename = lv_filename
               IMPORTING ev_data     = lv_data ).

    lv_xml = lcl_convert=>xstring_to_string_utf8( lv_data ).

    CREATE OBJECT ro_xml
      EXPORTING
        iv_xml = lv_xml.

  ENDMETHOD.                    "read_xml

  METHOD filename.

    DATA: lv_obj_name TYPE string.


    IF ms_item-obj_type = 'SICF'.
* multiple SICF nodes with same name cannot be added to repository
      lv_obj_name = ms_item-obj_name(15).
    ELSE.
      lv_obj_name = ms_item-obj_name.
    ENDIF.
* handle namespaces
    REPLACE ALL OCCURRENCES OF '/' IN lv_obj_name WITH '#'.

    IF iv_extra IS INITIAL.
      CONCATENATE lv_obj_name '.' ms_item-obj_type '.' iv_ext
        INTO rv_filename.                                   "#EC NOTEXT
    ELSE.
      CONCATENATE lv_obj_name '.' ms_item-obj_type '.' iv_extra '.' iv_ext
        INTO rv_filename.                                   "#EC NOTEXT
    ENDIF.
    TRANSLATE rv_filename TO LOWER CASE.

  ENDMETHOD.                    "filename

  METHOD add_xml_from_plugin.
*    this method wraps add_xml as in the plugin. This is necessary as the wrapped
*    xml-object in the plugin can only be typed to object.
*    ABAP does not perform implicit type casts (also if compatible) in signatures,
*    therefore this method's signature is typed ref to object
    DATA lo_xml TYPE REF TO lcl_xml_output.

    lo_xml ?= io_xml.

    me->add_xml(
      iv_extra     = iv_extra
      io_xml       = lo_xml
      iv_normalize = iv_normalize ).

  ENDMETHOD.                    "add_xml_from_plugin

  METHOD read_file.

    FIELD-SYMBOLS: <ls_file>     LIKE LINE OF mt_files,
                   <ls_accessed> LIKE LINE OF mt_accessed_files.

    CLEAR ev_data.
    READ TABLE mt_files ASSIGNING <ls_file> WITH KEY filename = iv_filename.

    IF sy-subrc <> 0.
      IF iv_error = abap_true.
        lcx_exception=>raise( |File not found: { iv_filename }| ).
      ELSE.
        RETURN.
      ENDIF.
    ENDIF.

    " Update access table
    READ TABLE mt_accessed_files TRANSPORTING NO FIELDS
      WITH KEY path = <ls_file>-path filename = <ls_file>-filename.
    IF sy-subrc > 0. " Not found ? -> Add
      APPEND INITIAL LINE TO mt_accessed_files ASSIGNING <ls_accessed>.
      MOVE-CORRESPONDING <ls_file> TO <ls_accessed>.
    ENDIF.

    ev_data = <ls_file>-data.

  ENDMETHOD.  " read_file.

  METHOD add_raw.

    DATA: ls_file TYPE ty_file.

    ls_file-path     = '/'.
    ls_file-data     = iv_data.
    ls_file-filename = filename( iv_extra = iv_extra
                                 iv_ext   = iv_ext ).

    APPEND ls_file TO mt_files.

  ENDMETHOD.                    "add_raw

  METHOD read_raw.

    DATA: lv_filename TYPE string.

    lv_filename = filename( iv_extra = iv_extra
                            iv_ext   = iv_ext ).

    read_file( EXPORTING iv_filename = lv_filename
               IMPORTING ev_data     = rv_data ).

  ENDMETHOD.                    "read_raw

ENDCLASS.                    "lcl_objects_files IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_objects_super DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects_super DEFINITION ABSTRACT.

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING
          is_item     TYPE ty_item
          iv_language TYPE spras.

    CONSTANTS: c_user_unknown TYPE xubname VALUE 'UNKNOWN'.

  PROTECTED SECTION.

    DATA: ms_item     TYPE ty_item,
          mv_language TYPE spras.

    METHODS:
      get_metadata
        RETURNING VALUE(rs_metadata) TYPE ty_metadata,
      corr_insert
        IMPORTING iv_package TYPE devclass
        RAISING   lcx_exception,
      jump_se11
        IMPORTING iv_radio TYPE string
                  iv_field TYPE string
        RAISING   lcx_exception.

ENDCLASS.                    "lcl_objects_super DEFINITION

**********************************************************************
* Enable plugins

CLASS lcl_objects_bridge DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.

    CLASS-METHODS class_constructor.

    METHODS constructor
      IMPORTING is_item TYPE ty_item
      RAISING   cx_sy_create_object_error.

    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

  PRIVATE SECTION.
    DATA: mo_plugin TYPE REF TO object.

    TYPES: BEGIN OF ty_s_objtype_map,
             obj_typ      TYPE trobjtype,
             plugin_class TYPE seoclsname,
           END OF ty_s_objtype_map,
           ty_t_objtype_map TYPE SORTED TABLE OF ty_s_objtype_map WITH UNIQUE KEY obj_typ.

    CLASS-DATA gt_objtype_map TYPE ty_t_objtype_map.

ENDCLASS.                    "lcl_objects_bridge DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_objects_bridge IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects_bridge IMPLEMENTATION.

  METHOD lif_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.  "lif_object~has_changed_since

  METHOD lif_object~get_metadata.

    CALL METHOD mo_plugin->('ZIF_ABAPGIT_PLUGIN~GET_METADATA')
      RECEIVING
        rs_metadata = rs_metadata.

  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~changed_by.
    rv_user = c_user_unknown. " todo
  ENDMETHOD.

  METHOD constructor.

    DATA ls_objtype_map LIKE LINE OF gt_objtype_map.

    super->constructor( is_item = is_item
                        iv_language = gc_english ).

*    determine the responsible plugin
    READ TABLE gt_objtype_map INTO ls_objtype_map
      WITH TABLE KEY obj_typ = is_item-obj_type.
    IF sy-subrc = 0.
      CREATE OBJECT mo_plugin TYPE (ls_objtype_map-plugin_class).

      CALL METHOD mo_plugin->('SET_ITEM')
        EXPORTING
          iv_obj_type = is_item-obj_type
          iv_obj_name = is_item-obj_name.
    ELSE.
      RAISE EXCEPTION TYPE cx_sy_create_object_error
        EXPORTING
          classname = 'LCL_OBJECTS_BRIDGE'.
    ENDIF.
  ENDMETHOD.                    "constructor

  METHOD lif_object~serialize.

    CALL METHOD mo_plugin->('WRAP_SERIALIZE')
      EXPORTING
        io_xml = io_xml.

  ENDMETHOD.                    "lif_object~serialize

  METHOD lif_object~deserialize.

    DATA: lx_plugin        TYPE REF TO cx_static_check.

    TRY.
        CALL METHOD mo_plugin->('WRAP_DESERIALIZE')
          EXPORTING
            iv_package = iv_package
            io_xml     = io_xml.
      CATCH cx_static_check INTO lx_plugin.
        RAISE EXCEPTION TYPE lcx_exception
          EXPORTING
            ix_previous = lx_plugin
            iv_text     = lx_plugin->get_text( ).
    ENDTRY.
  ENDMETHOD.                    "lif_object~deserialize

  METHOD lif_object~delete.
    DATA lx_plugin TYPE REF TO cx_static_check.

    TRY.
        CALL METHOD mo_plugin->('ZIF_ABAPGIT_PLUGIN~DELETE').
      CATCH cx_static_check INTO lx_plugin.
        RAISE EXCEPTION TYPE lcx_exception
          EXPORTING
            ix_previous = lx_plugin
            iv_text     = lx_plugin->get_text( ).
    ENDTRY.

  ENDMETHOD.                    "lif_object~delete

  METHOD lif_object~exists.

    CALL METHOD mo_plugin->('ZIF_ABAPGIT_PLUGIN~EXISTS')
      RECEIVING
        rv_bool = rv_bool.

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~jump.

    CALL METHOD mo_plugin->('ZIF_ABAPGIT_PLUGIN~JUMP').

  ENDMETHOD.                    "lif_object~jump

  METHOD class_constructor.

    DATA lt_plugin_class    TYPE STANDARD TABLE OF seoclsname WITH DEFAULT KEY.
    DATA lv_plugin_class    LIKE LINE OF lt_plugin_class.
    DATA lo_plugin          TYPE REF TO object.
    DATA lt_plugin_obj_type TYPE objtyptable.
    DATA ls_objtype_map     LIKE LINE OF gt_objtype_map.


    SELECT ext~clsname
      FROM vseoextend AS ext
      INTO TABLE lt_plugin_class
      WHERE ext~refclsname LIKE 'ZCL_ABAPGIT_OBJECT%'
      AND ext~version = '1'.                              "#EC CI_SUBRC

    CLEAR gt_objtype_map.
    LOOP AT lt_plugin_class INTO lv_plugin_class
        WHERE table_line <> 'ZCL_ABAPGIT_OBJECT_BY_SOBJ'.
* have the generic plugin only as fallback
      TRY.
          CREATE OBJECT lo_plugin TYPE (lv_plugin_class).
        CATCH cx_sy_create_object_error.
          CONTINUE. ">>>>>>>>>>>>>>
      ENDTRY.

      CALL METHOD lo_plugin->('GET_SUPPORTED_OBJ_TYPES')
        IMPORTING
          rt_obj_type = lt_plugin_obj_type.

      ls_objtype_map-plugin_class = lv_plugin_class.
      LOOP AT lt_plugin_obj_type INTO ls_objtype_map-obj_typ.
        INSERT ls_objtype_map INTO TABLE gt_objtype_map.
        IF sy-subrc <> 0.
* No exception in class-contructor possible.
* Anyway, a shortdump is more appropriate in this case
          ASSERT 'There must not be' =
            |multiple abapGit-Plugins for the same object type {
            ls_objtype_map-obj_typ }|.
        ENDIF.
      ENDLOOP.
    ENDLOOP. "at plugins

* and the same for the generic plugin if exists
* have the generic plugin only as fallback
    LOOP AT lt_plugin_class INTO lv_plugin_class
        WHERE table_line = 'ZCL_ABAPGIT_OBJECT_BY_SOBJ'.
      CREATE OBJECT lo_plugin TYPE (lv_plugin_class).

      CALL METHOD lo_plugin->('GET_SUPPORTED_OBJ_TYPES')
        RECEIVING
          rt_obj_type = lt_plugin_obj_type.

      ls_objtype_map-plugin_class = lv_plugin_class.
      LOOP AT lt_plugin_obj_type INTO ls_objtype_map-obj_typ.
        INSERT ls_objtype_map INTO TABLE gt_objtype_map. "knowingly ignore the subrc
      ENDLOOP.
    ENDLOOP. "at plugins

  ENDMETHOD.                    "class_constructor

  METHOD lif_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_null_comparison_result.
  ENDMETHOD.

ENDCLASS.                    "lcl_objects_bridge IMPLEMENTATION

**********************************************************************

CLASS lcl_objects_program DEFINITION INHERITING FROM lcl_objects_super.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_progdir,
             name    TYPE progdir-name,
             state   TYPE progdir-state,
             sqlx    TYPE progdir-sqlx,
             edtx    TYPE progdir-edtx,
             varcl   TYPE progdir-varcl,
             dbapl   TYPE progdir-dbapl,
             dbna    TYPE progdir-dbna,
             clas    TYPE progdir-clas,
             type    TYPE progdir-type,
             occurs  TYPE progdir-occurs,
             subc    TYPE progdir-subc,
             appl    TYPE progdir-appl,
             secu    TYPE progdir-secu,
             cnam    TYPE progdir-cnam,
             cdat    TYPE progdir-cdat,
             unam    TYPE progdir-unam,
             udat    TYPE progdir-udat,
             vern    TYPE progdir-vern,
             levl    TYPE progdir-levl,
             rstat   TYPE progdir-rstat,
             rmand   TYPE progdir-rmand,
             rload   TYPE progdir-rload,
             fixpt   TYPE progdir-fixpt,
             sset    TYPE progdir-sset,
             sdate   TYPE progdir-sdate,
             stime   TYPE progdir-stime,
             idate   TYPE progdir-idate,
             itime   TYPE progdir-itime,
             ldbname TYPE progdir-ldbname,
             uccheck TYPE progdir-uccheck,
           END OF ty_progdir.

    METHODS serialize_program
      IMPORTING io_xml     TYPE REF TO lcl_xml_output OPTIONAL
                is_item    TYPE ty_item
                io_files   TYPE REF TO lcl_objects_files
                iv_program TYPE programm OPTIONAL
                iv_extra   TYPE clike OPTIONAL
      RAISING   lcx_exception.

    METHODS read_progdir
      IMPORTING iv_program        TYPE programm
      RETURNING VALUE(rs_progdir) TYPE ty_progdir.

    METHODS deserialize_program
      IMPORTING is_progdir TYPE ty_progdir
                it_source  TYPE abaptxt255_tab
                it_tpool   TYPE textpool_table
                iv_package TYPE devclass
      RAISING   lcx_exception.

  PROTECTED SECTION.

    TYPES: ty_spaces_tt TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_dynpro,
             header     TYPE rpy_dyhead,
             containers TYPE dycatt_tab,
             fields     TYPE dyfatc_tab,
             flow_logic TYPE swydyflow,
             spaces     TYPE ty_spaces_tt,
           END OF ty_dynpro.

    TYPES: ty_dynpro_tt TYPE STANDARD TABLE OF ty_dynpro WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_cua,
             adm TYPE rsmpe_adm,
             sta TYPE STANDARD TABLE OF rsmpe_stat WITH DEFAULT KEY,
             fun TYPE STANDARD TABLE OF rsmpe_funt WITH DEFAULT KEY,
             men TYPE STANDARD TABLE OF rsmpe_men WITH DEFAULT KEY,
             mtx TYPE STANDARD TABLE OF rsmpe_mnlt WITH DEFAULT KEY,
             act TYPE STANDARD TABLE OF rsmpe_act WITH DEFAULT KEY,
             but TYPE STANDARD TABLE OF rsmpe_but WITH DEFAULT KEY,
             pfk TYPE STANDARD TABLE OF rsmpe_pfk WITH DEFAULT KEY,
             set TYPE STANDARD TABLE OF rsmpe_staf WITH DEFAULT KEY,
             doc TYPE STANDARD TABLE OF rsmpe_atrt WITH DEFAULT KEY,
             tit TYPE STANDARD TABLE OF rsmpe_titt WITH DEFAULT KEY,
             biv TYPE STANDARD TABLE OF rsmpe_buts WITH DEFAULT KEY,
           END OF ty_cua.

    METHODS serialize_dynpros
      IMPORTING iv_program_name  TYPE programm
      RETURNING VALUE(rt_dynpro) TYPE ty_dynpro_tt
      RAISING   lcx_exception.

    METHODS serialize_cua
      IMPORTING iv_program_name TYPE programm
      RETURNING VALUE(rs_cua)   TYPE ty_cua
      RAISING   lcx_exception.

    METHODS deserialize_dynpros
      IMPORTING it_dynpros TYPE ty_dynpro_tt
      RAISING   lcx_exception.

    METHODS deserialize_textpool
      IMPORTING iv_program TYPE programm
                it_tpool   TYPE textpool_table
      RAISING   lcx_exception.

    METHODS deserialize_cua
      IMPORTING iv_program_name TYPE programm
                is_cua          TYPE ty_cua
      RAISING   lcx_exception.

    METHODS check_prog_changed_since
      IMPORTING iv_program        TYPE programm
                iv_timestamp      TYPE timestamp
                iv_skip_gui       TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(rv_changed) TYPE abap_bool.

    CLASS-METHODS:
      add_tpool
        IMPORTING it_tpool        TYPE textpool_table
        RETURNING VALUE(rt_tpool) TYPE ty_tpool_tt,
      read_tpool
        IMPORTING it_tpool        TYPE ty_tpool_tt
        RETURNING VALUE(rt_tpool) TYPE ty_tpool_tt.

  PRIVATE SECTION.
    METHODS:
      condense_flow
        EXPORTING et_spaces TYPE ty_spaces_tt
        CHANGING  ct_flow   TYPE swydyflow,
      uncondense_flow
        IMPORTING it_flow        TYPE swydyflow
                  it_spaces      TYPE ty_spaces_tt
        RETURNING VALUE(rt_flow) TYPE swydyflow.


ENDCLASS.                    "lcl_objects_program DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_objects_program IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects_program IMPLEMENTATION.

  METHOD condense_flow.

    DATA: lv_spaces LIKE LINE OF et_spaces.

    FIELD-SYMBOLS: <ls_flow> LIKE LINE OF ct_flow.


    CLEAR et_spaces.

    LOOP AT ct_flow ASSIGNING <ls_flow>.
      lv_spaces = 0.

      WHILE NOT <ls_flow>-line IS INITIAL AND <ls_flow>-line(1) = space.
        lv_spaces = lv_spaces + 1.
        <ls_flow>-line = <ls_flow>-line+1.
      ENDWHILE.

      APPEND lv_spaces TO et_spaces.
    ENDLOOP.

  ENDMETHOD.

  METHOD uncondense_flow.

    DATA: lv_spaces LIKE LINE OF it_spaces.

    FIELD-SYMBOLS: <ls_flow>   LIKE LINE OF it_flow,
                   <ls_output> LIKE LINE OF rt_flow.


    LOOP AT it_flow ASSIGNING <ls_flow>.
      APPEND INITIAL LINE TO rt_flow ASSIGNING <ls_output>.
      <ls_output>-line = <ls_flow>-line.

      READ TABLE it_spaces INDEX sy-tabix INTO lv_spaces.
      IF sy-subrc = 0.
        SHIFT <ls_output>-line RIGHT BY lv_spaces PLACES IN CHARACTER MODE.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD serialize_program.

    DATA: ls_progdir      TYPE ty_progdir,
          lv_program_name TYPE programm,
          lt_dynpros      TYPE ty_dynpro_tt,
          ls_cua          TYPE ty_cua,
          lt_source       TYPE TABLE OF abaptxt255,
          lt_tpool        TYPE textpool_table,
          ls_tpool        LIKE LINE OF lt_tpool,
          lo_xml          TYPE REF TO lcl_xml_output.

    IF iv_program IS INITIAL.
      lv_program_name = is_item-obj_name.
    ELSE.
      lv_program_name = iv_program.
    ENDIF.

    CALL FUNCTION 'RPY_PROGRAM_READ'
      EXPORTING
        program_name     = lv_program_name
        with_lowercase   = abap_true
      TABLES
        source_extended  = lt_source
        textelements     = lt_tpool
      EXCEPTIONS
        cancelled        = 1
        not_found        = 2
        permission_error = 3
        OTHERS           = 4.
    IF sy-subrc = 2.
      RETURN.
    ELSEIF sy-subrc <> 0.
      lcx_exception=>raise( 'Error reading program' ).
    ENDIF.

    ls_progdir = read_progdir( lv_program_name ).

    IF io_xml IS BOUND.
      lo_xml = io_xml.
    ELSE.
      CREATE OBJECT lo_xml.
    ENDIF.

    lo_xml->add( iv_name = 'PROGDIR'
                 ig_data = ls_progdir ).
    IF ls_progdir-subc = '1'.
      lt_dynpros = serialize_dynpros( lv_program_name ).
      lo_xml->add( iv_name = 'DYNPROS'
                   ig_data = lt_dynpros ).

      ls_cua = serialize_cua( lv_program_name ).
      IF NOT ls_cua IS INITIAL.
        lo_xml->add( iv_name = 'CUA'
                     ig_data = ls_cua ).
      ENDIF.
    ENDIF.

    READ TABLE lt_tpool WITH KEY id = 'R' INTO ls_tpool.
    IF sy-subrc = 0 AND ls_tpool-key = '' AND ls_tpool-length = 0.
      DELETE lt_tpool INDEX sy-tabix.
    ENDIF.

    lo_xml->add( iv_name = 'TPOOL'
                 ig_data = add_tpool( lt_tpool ) ).

    IF NOT io_xml IS BOUND.
      io_files->add_xml( iv_extra = iv_extra
                         io_xml   = lo_xml ).
    ENDIF.

    io_files->add_abap( iv_extra = iv_extra
                        it_abap  = lt_source ).

  ENDMETHOD.                    "serialize_program

  METHOD deserialize_program.

    DATA: lv_exists      TYPE sap_bool,
          lv_progname    TYPE reposrc-progname,
          ls_tpool       LIKE LINE OF it_tpool,
          lv_title       TYPE rglif-title,
          ls_progdir_new TYPE progdir.

    FIELD-SYMBOLS: <lg_any> TYPE any.


    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = is_progdir-name
        object_class        = 'ABAP'
        devclass            = iv_package
        master_language     = mv_language
        mode                = 'INSERT'
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.
    IF sy-subrc = 1.
      lcx_exception=>raise( 'Cancelled' ).
    ELSEIF sy-subrc <> 0.
      lcx_exception=>raise( 'error from RS_CORR_INSERT' ).
    ENDIF.

    READ TABLE it_tpool INTO ls_tpool WITH KEY id = 'R'.  "#EC CI_SUBRC
    IF sy-subrc = 0.
* there is a bug in RPY_PROGRAM_UPDATE, the header line of TTAB is not
* cleared, so the title length might be inherited from a different program.
      ASSIGN ('(SAPLSIFP)TTAB') TO <lg_any>.
      IF sy-subrc = 0.
        CLEAR <lg_any>.
      ENDIF.

      lv_title = ls_tpool-entry.
    ENDIF.

    SELECT SINGLE progname FROM reposrc INTO lv_progname
      WHERE progname = is_progdir-name
      AND r3state = 'A'.
    IF sy-subrc = 0.
      lv_exists = abap_true.
    ELSE.
      lv_exists = abap_false.
    ENDIF.

    IF lv_exists = abap_true.
      CALL FUNCTION 'RPY_PROGRAM_UPDATE'
        EXPORTING
          program_name     = is_progdir-name
          title_string     = lv_title
          save_inactive    = 'I'
        TABLES
          source_extended  = it_source
        EXCEPTIONS
          cancelled        = 1
          permission_error = 2
          not_found        = 3
          OTHERS           = 4.
      IF sy-subrc <> 0.
        IF sy-msgid = 'EU' AND sy-msgno = '510'.
          lcx_exception=>raise( 'User is currently editing program' ).
        ELSE.
          lcx_exception=>raise( 'PROG, error updating' ).
        ENDIF.
      ENDIF.
    ELSE.
* function module RPY_PROGRAM_INSERT cannot handle function group includes

      IF strlen( is_progdir-name ) > 30.
        " special treatment for extenstions
        " if the program name exceeds 30 characters it is not a usual
        " ABAP program but might be some extension, which requires the internal
        " addition EXTENSION TYPE, see
        " http://help.sap.com/abapdocu_751/en/abapinsert_report_internal.htm#!ABAP_ADDITION_1@1@
        " This e.g. occurs in case of transportable Code Inspector variants (ending with ===VC)
        INSERT REPORT is_progdir-name
         FROM it_source
         STATE 'I'
         EXTENSION TYPE is_progdir-name+30.
        IF sy-subrc <> 0.
          lcx_exception=>raise( 'error from INSERT REPORT .. EXTENSION TYPE' ).
        ENDIF.
      ELSE.
        INSERT REPORT is_progdir-name
          FROM it_source
          STATE 'I'
          PROGRAM TYPE is_progdir-subc.
        IF sy-subrc <> 0.
          lcx_exception=>raise( 'error from INSERT REPORT' ).
        ENDIF.
      ENDIF.

      IF NOT it_tpool[] IS INITIAL.
        INSERT TEXTPOOL is_progdir-name
          FROM it_tpool
          LANGUAGE mv_language
          STATE 'I'.
        IF sy-subrc <> 0.
          lcx_exception=>raise( 'error from INSERT TEXTPOOL' ).
        ENDIF.
      ENDIF.

    ENDIF.

    CALL FUNCTION 'READ_PROGDIR'
      EXPORTING
        i_progname = is_progdir-name
        i_state    = 'I'
      IMPORTING
        e_progdir  = ls_progdir_new
      EXCEPTIONS
        not_exists = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'not found in PROGDIR' ).
    ENDIF.

* todo, package?

    ls_progdir_new-ldbname = is_progdir-ldbname.
    ls_progdir_new-dbna    = is_progdir-dbna.
    ls_progdir_new-dbapl   = is_progdir-dbapl.
    ls_progdir_new-rload   = is_progdir-rload.
    ls_progdir_new-fixpt   = is_progdir-fixpt.
    ls_progdir_new-varcl   = is_progdir-varcl.
    ls_progdir_new-appl    = is_progdir-appl.
    ls_progdir_new-rstat   = is_progdir-rstat.

    CALL FUNCTION 'UPDATE_PROGDIR'
      EXPORTING
        i_progdir    = ls_progdir_new
        i_progname   = ls_progdir_new-name
        i_state      = ls_progdir_new-state
      EXCEPTIONS
        not_executed = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'PROG, error inserting' ).
    ENDIF.

    SELECT SINGLE * FROM progdir INTO ls_progdir_new
      WHERE name = ls_progdir_new-name
      AND state = ls_progdir_new-state.
    IF sy-subrc = 0 AND is_progdir-varcl = space AND ls_progdir_new-varcl = abap_true.
* function module UPDATE_PROGDIR does not update VARCL
      UPDATE progdir SET varcl = is_progdir-varcl
        WHERE name = ls_progdir_new-name
        AND state = ls_progdir_new-state.
    ENDIF.

    lcl_objects_activation=>add( iv_type = 'REPS'
                                 iv_name = is_progdir-name ).

  ENDMETHOD.                    "deserialize_program

  METHOD read_progdir.

    DATA: ls_sapdir TYPE progdir.


    CALL FUNCTION 'READ_PROGDIR'
      EXPORTING
        i_progname = iv_program
        i_state    = 'A'
      IMPORTING
        e_progdir  = ls_sapdir.
    MOVE-CORRESPONDING ls_sapdir TO rs_progdir.

    CLEAR: rs_progdir-edtx,
           rs_progdir-cnam,
           rs_progdir-cdat,
           rs_progdir-unam,
           rs_progdir-udat,
           rs_progdir-levl,
           rs_progdir-vern,
           rs_progdir-rmand,
           rs_progdir-sdate,
           rs_progdir-stime,
           rs_progdir-idate,
           rs_progdir-itime.

  ENDMETHOD.                    "read_progdir

  METHOD serialize_cua.

    CALL FUNCTION 'RS_CUA_INTERNAL_FETCH'
      EXPORTING
        program         = iv_program_name
        language        = mv_language
        state           = 'A'
      IMPORTING
        adm             = rs_cua-adm
      TABLES
        sta             = rs_cua-sta
        fun             = rs_cua-fun
        men             = rs_cua-men
        mtx             = rs_cua-mtx
        act             = rs_cua-act
        but             = rs_cua-but
        pfk             = rs_cua-pfk
        set             = rs_cua-set
        doc             = rs_cua-doc
        tit             = rs_cua-tit
        biv             = rs_cua-biv
      EXCEPTIONS
        not_found       = 1
        unknown_version = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from RS_CUA_INTERNAL_FETCH' ).
    ENDIF.

  ENDMETHOD.                    "serialize_cua

  METHOD serialize_dynpros.

    DATA: ls_header               TYPE rpy_dyhead,
          lt_containers           TYPE dycatt_tab,
          lt_fields_to_containers TYPE dyfatc_tab,
          lt_flow_logic           TYPE swydyflow,
          lt_d020s                TYPE TABLE OF d020s.

    FIELD-SYMBOLS: <ls_d020s>       LIKE LINE OF lt_d020s,
                   <lv_outputstyle> TYPE scrpostyle,
                   <ls_field>       LIKE LINE OF lt_fields_to_containers,
                   <ls_dynpro>      LIKE LINE OF rt_dynpro.


    CALL FUNCTION 'RS_SCREEN_LIST'
      EXPORTING
        dynnr     = ''
        progname  = iv_program_name
      TABLES
        dynpros   = lt_d020s
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc = 2.
      lcx_exception=>raise( 'error from screen_list' ).
    ENDIF.

* loop dynpros and skip generated selection screens
    LOOP AT lt_d020s ASSIGNING <ls_d020s> WHERE type <> 'S'.

      CALL FUNCTION 'RPY_DYNPRO_READ'
        EXPORTING
          progname             = iv_program_name
          dynnr                = <ls_d020s>-dnum
        IMPORTING
          header               = ls_header
        TABLES
          containers           = lt_containers
          fields_to_containers = lt_fields_to_containers
          flow_logic           = lt_flow_logic
        EXCEPTIONS
          cancelled            = 1
          not_found            = 2
          permission_error     = 3
          OTHERS               = 4.
      IF sy-subrc <> 0.
        lcx_exception=>raise( 'Error while reading dynpro' ).
      ENDIF.

      LOOP AT lt_fields_to_containers ASSIGNING <ls_field>.
* output style is a NUMC field, the XML conversion will fail if it contains invalid value
* field does not exist in all versions
        ASSIGN COMPONENT 'OUTPUTSTYLE' OF STRUCTURE <ls_field> TO <lv_outputstyle>.
        IF sy-subrc = 0 AND <lv_outputstyle> = '  '.
          CLEAR <lv_outputstyle>.
        ENDIF.
      ENDLOOP.

      APPEND INITIAL LINE TO rt_dynpro ASSIGNING <ls_dynpro>.
      <ls_dynpro>-header     = ls_header.
      <ls_dynpro>-containers = lt_containers.
      <ls_dynpro>-fields     = lt_fields_to_containers.

      condense_flow( IMPORTING et_spaces = <ls_dynpro>-spaces
                     CHANGING ct_flow = lt_flow_logic ).
      <ls_dynpro>-flow_logic = lt_flow_logic.

    ENDLOOP.

  ENDMETHOD.                    "serialize_dynpros


  METHOD deserialize_dynpros.

    DATA: lv_name   TYPE dwinactiv-obj_name,
          ls_dynpro LIKE LINE OF it_dynpros.


* ls_dynpro is changed by the function module, a field-symbol will cause
* the program to dump since it_dynpros cannot be changed
    LOOP AT it_dynpros INTO ls_dynpro.

      ls_dynpro-flow_logic = uncondense_flow(
        it_flow = ls_dynpro-flow_logic
        it_spaces = ls_dynpro-spaces ).

      CALL FUNCTION 'RPY_DYNPRO_INSERT'
        EXPORTING
          header                 = ls_dynpro-header
          suppress_exist_checks  = abap_true
        TABLES
          containers             = ls_dynpro-containers
          fields_to_containers   = ls_dynpro-fields
          flow_logic             = ls_dynpro-flow_logic
        EXCEPTIONS
          cancelled              = 1
          already_exists         = 2
          program_not_exists     = 3
          not_executed           = 4
          missing_required_field = 5
          illegal_field_value    = 6
          field_not_allowed      = 7
          not_generated          = 8
          illegal_field_position = 9
          OTHERS                 = 10.
      IF sy-subrc <> 2 AND sy-subrc <> 0.
        lcx_exception=>raise( 'error from RPY_DYNPRO_INSERT' ).
      ENDIF.
* todo, RPY_DYNPRO_UPDATE?

      CONCATENATE ls_dynpro-header-program ls_dynpro-header-screen
        INTO lv_name RESPECTING BLANKS.
      ASSERT NOT lv_name IS INITIAL.

      lcl_objects_activation=>add( iv_type = 'DYNP'
                                   iv_name = lv_name ).

    ENDLOOP.

  ENDMETHOD.                    "deserialize_dynpros

  METHOD add_tpool.

    FIELD-SYMBOLS: <ls_tpool_in>  LIKE LINE OF it_tpool,
                   <ls_tpool_out> LIKE LINE OF rt_tpool.


    LOOP AT it_tpool ASSIGNING <ls_tpool_in>.
      APPEND INITIAL LINE TO rt_tpool ASSIGNING <ls_tpool_out>.
      MOVE-CORRESPONDING <ls_tpool_in> TO <ls_tpool_out>.
      IF <ls_tpool_out>-id = 'S'.
        <ls_tpool_out>-split = <ls_tpool_out>-entry.
        <ls_tpool_out>-entry = <ls_tpool_out>-entry+8.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "add_tpool

  METHOD read_tpool.

    FIELD-SYMBOLS: <ls_tpool_in>  LIKE LINE OF it_tpool,
                   <ls_tpool_out> LIKE LINE OF rt_tpool.


    LOOP AT it_tpool ASSIGNING <ls_tpool_in>.
      APPEND INITIAL LINE TO rt_tpool ASSIGNING <ls_tpool_out>.
      MOVE-CORRESPONDING <ls_tpool_in> TO <ls_tpool_out>.
      IF <ls_tpool_out>-id = 'S'.
        CONCATENATE <ls_tpool_in>-split <ls_tpool_in>-entry
          INTO <ls_tpool_out>-entry
          RESPECTING BLANKS.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "read_tpool

  METHOD deserialize_textpool.

    READ TABLE it_tpool WITH KEY id = 'R' TRANSPORTING NO FIELDS.
    IF ( sy-subrc = 0 AND lines( it_tpool ) = 1 ) OR lines( it_tpool ) = 0.
      RETURN. " no action for includes
    ENDIF.

    INSERT TEXTPOOL iv_program
      FROM it_tpool
      LANGUAGE mv_language
      STATE 'I'.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from INSERT TEXTPOOL' ).
    ENDIF.

    lcl_objects_activation=>add( iv_type = 'REPT'
                                 iv_name = iv_program ).

  ENDMETHOD.                    "deserialize_textpool

  METHOD deserialize_cua.

    DATA: ls_tr_key TYPE trkey.


    IF lines( is_cua-sta ) = 0
        AND lines( is_cua-fun ) = 0
        AND lines( is_cua-men ) = 0
        AND lines( is_cua-mtx ) = 0
        AND lines( is_cua-act ) = 0
        AND lines( is_cua-but ) = 0
        AND lines( is_cua-pfk ) = 0
        AND lines( is_cua-set ) = 0
        AND lines( is_cua-doc ) = 0
        AND lines( is_cua-tit ) = 0
        AND lines( is_cua-biv ) = 0.
      RETURN.
    ENDIF.

    SELECT SINGLE devclass INTO ls_tr_key-devclass
      FROM tadir
      WHERE pgmid = 'R3TR'
      AND object = ms_item-obj_type
      AND obj_name = ms_item-obj_name.                  "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'not found in tadir' ).
    ENDIF.

    ls_tr_key-obj_type = ms_item-obj_type.
    ls_tr_key-obj_name = ms_item-obj_name.
    ls_tr_key-sub_type = 'CUAD'.
    ls_tr_key-sub_name = iv_program_name.

    sy-tcode = 'SE41' ##write_ok. " evil hack, workaround to handle fixes in note 2159455
    CALL FUNCTION 'RS_CUA_INTERNAL_WRITE'
      EXPORTING
        program   = iv_program_name
        language  = mv_language
        tr_key    = ls_tr_key
        adm       = is_cua-adm
        state     = 'I'
      TABLES
        sta       = is_cua-sta
        fun       = is_cua-fun
        men       = is_cua-men
        mtx       = is_cua-mtx
        act       = is_cua-act
        but       = is_cua-but
        pfk       = is_cua-pfk
        set       = is_cua-set
        doc       = is_cua-doc
        tit       = is_cua-tit
        biv       = is_cua-biv
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from RS_CUA_INTERNAL_WRITE' ).
    ENDIF.

    lcl_objects_activation=>add( iv_type = 'CUAD'
                                 iv_name = iv_program_name ).

  ENDMETHOD.                    "deserialize_cua

  METHOD check_prog_changed_since.

    DATA: lv_date    TYPE dats,
          lv_time    TYPE tims,
          lv_ts      TYPE timestamp,
          lt_screens TYPE STANDARD TABLE OF d020s,
          lt_eudb    TYPE STANDARD TABLE OF eudb.

    FIELD-SYMBOLS: <ls_screen> LIKE LINE OF lt_screens,
                   <ls_eudb>   LIKE LINE OF lt_eudb.

    SELECT SINGLE udat utime FROM reposrc " Program
      INTO (lv_date, lv_time)
      WHERE progname = iv_program
      AND   r3state = 'A'.

    _object_check_timestamp lv_date lv_time.

    SELECT SINGLE udat utime FROM repotext " Program text pool
      INTO (lv_date, lv_time)
      WHERE progname = iv_program
      AND   r3state = 'A'.

    IF sy-subrc = 0. " Text not found ? Assuming no changes, see #404
      _object_check_timestamp lv_date lv_time.
    ENDIF.

    IF iv_skip_gui = abap_true.
      RETURN.
    ENDIF.

    SELECT dgen tgen FROM d020s           " Screens
      INTO CORRESPONDING FIELDS OF TABLE lt_screens
      WHERE prog = iv_program ##TOO_MANY_ITAB_FIELDS.

    LOOP AT lt_screens ASSIGNING <ls_screen>.
      _object_check_timestamp <ls_screen>-dgen <ls_screen>-tgen.
    ENDLOOP.

    SELECT vdatum vzeit FROM eudb         " GUI
      INTO CORRESPONDING FIELDS OF TABLE lt_eudb
      WHERE relid = 'CU'
      AND   name  = iv_program
      AND   srtf2 = 0 ##TOO_MANY_ITAB_FIELDS.

    LOOP AT lt_eudb ASSIGNING <ls_eudb>.
      _object_check_timestamp <ls_eudb>-vdatum <ls_eudb>-vzeit.
    ENDLOOP.

  ENDMETHOD.  "check_prog_changed_since

ENDCLASS.                    "lcl_objects_program IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_objects_super IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects_super IMPLEMENTATION.

  METHOD constructor.
    ms_item = is_item.
    ASSERT NOT ms_item IS INITIAL.
    mv_language = iv_language.
    ASSERT NOT mv_language IS INITIAL.
  ENDMETHOD.                    "constructor

  METHOD jump_se11.

    DATA: lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.


    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPLSD_ENTRY'.
    <ls_bdcdata>-dynpro   = '1000'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=WB_DISPLAY'.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = iv_radio.
    <ls_bdcdata>-fval = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = iv_field.
    <ls_bdcdata>-fval = ms_item-obj_name.

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      STARTING NEW TASK 'GIT'
      EXPORTING
        tcode                 = 'SE11'
        mode_val              = 'E'
      TABLES
        using_tab             = lt_bdcdata
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        resource_failure      = 3
        OTHERS                = 4
        ##fm_subrc_ok.                                                   "#EC CI_SUBRC

  ENDMETHOD.                                                "jump_se11

  METHOD get_metadata.
    rs_metadata-class =
      cl_abap_classdescr=>describe_by_object_ref( me )->get_relative_name( ).
    rs_metadata-version = 'v1.0.0' ##no_text.
  ENDMETHOD.                    "get_metadata

  METHOD corr_insert.

    DATA: ls_object TYPE ddenqs.


    ls_object-objtype = ms_item-obj_type.
    ls_object-objname = ms_item-obj_name.

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = ls_object
        object_class        = 'DICT'
        devclass            = iv_package
        master_language     = mv_language
        mode                = 'INSERT'
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.
    IF sy-subrc = 1.
      lcx_exception=>raise( 'Cancelled' ).
    ELSEIF sy-subrc <> 0.
      lcx_exception=>raise( 'error from RS_CORR_INSERT' ).
    ENDIF.

  ENDMETHOD.                    "corr_insert

ENDCLASS.                    "lcl_objects_super IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: ty_types_tt TYPE STANDARD TABLE OF tadir-object WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_deserialization,
             obj     TYPE REF TO lif_object,
             xml     TYPE REF TO lcl_xml_input,
             package TYPE devclass,
             item    TYPE ty_item,
           END OF ty_deserialization.

    TYPES: ty_deserialization_tt TYPE STANDARD TABLE OF ty_deserialization WITH DEFAULT KEY.

    CLASS-METHODS serialize
      IMPORTING is_item         TYPE ty_item
                iv_language     TYPE spras
                io_log          TYPE REF TO lcl_log OPTIONAL
      RETURNING VALUE(rt_files) TYPE ty_files_tt
      RAISING   lcx_exception.

    CLASS-METHODS deserialize
      IMPORTING io_repo                  TYPE REF TO lcl_repo
      RETURNING VALUE(rt_accessed_files) TYPE ty_file_signatures_tt
      RAISING   lcx_exception.

    CLASS-METHODS delete
      IMPORTING it_tadir TYPE ty_tadir_tt
      RAISING   lcx_exception.

    CLASS-METHODS jump
      IMPORTING is_item TYPE ty_item
      RAISING   lcx_exception.

    CLASS-METHODS changed_by
      IMPORTING is_item        TYPE ty_item
      RETURNING VALUE(rv_user) TYPE xubname
      RAISING   lcx_exception.

    CLASS-METHODS has_changed_since
      IMPORTING is_item           TYPE ty_item
                iv_timestamp      TYPE timestamp
      RETURNING VALUE(rv_changed) TYPE abap_bool
      RAISING   lcx_exception.

    CLASS-METHODS is_supported
      IMPORTING is_item        TYPE ty_item
                iv_native_only TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(rv_bool) TYPE abap_bool.

    CLASS-METHODS exists
      IMPORTING is_item        TYPE ty_item
      RETURNING VALUE(rv_bool) TYPE abap_bool.

    CLASS-METHODS supported_list
      RETURNING VALUE(rt_types) TYPE ty_types_tt.

  PRIVATE SECTION.

    CLASS-DATA: mv_langs_installed TYPE scplangs.

    CLASS-METHODS check_duplicates
      IMPORTING it_files TYPE ty_files_tt
      RAISING   lcx_exception.

    CLASS-METHODS create_object
      IMPORTING is_item        TYPE ty_item
                iv_language    TYPE spras
                is_metadata    TYPE ty_metadata OPTIONAL
                iv_native_only TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(ri_obj)  TYPE REF TO lif_object
      RAISING   lcx_exception.

    CLASS-METHODS
      prioritize_deser
        IMPORTING it_results        TYPE ty_results_tt
        RETURNING VALUE(rt_results) TYPE ty_results_tt.

    CLASS-METHODS
      path_to_package
        IMPORTING iv_top            TYPE devclass
                  iv_start          TYPE string
                  iv_path           TYPE string
        RETURNING VALUE(rv_package) TYPE devclass
        RAISING   lcx_exception.

    CLASS-METHODS class_name
      IMPORTING is_item              TYPE ty_item
      RETURNING VALUE(rv_class_name) TYPE string.

    CLASS-METHODS resolve_ddic
      CHANGING ct_tadir TYPE ty_tadir_tt
      RAISING  lcx_exception.

    CLASS-METHODS warning_overwrite
      CHANGING ct_results TYPE ty_results_tt
      RAISING  lcx_exception.

    CLASS-METHODS warning_package
      IMPORTING is_item          TYPE ty_item
                iv_package       TYPE devclass
      RETURNING VALUE(rv_cancel) TYPE abap_bool
      RAISING   lcx_exception.

    CLASS-METHODS update_package_tree
      IMPORTING iv_package TYPE devclass.

    CLASS-METHODS delete_obj
      IMPORTING is_item TYPE ty_item
      RAISING   lcx_exception.

    CLASS-METHODS compare_remote_to_local
      IMPORTING
        io_object TYPE REF TO lif_object
        it_remote TYPE ty_files_tt
        is_result TYPE ty_result
      RAISING
        lcx_exception.

    CLASS-METHODS deserialize_objects
      IMPORTING it_objects TYPE ty_deserialization_tt
                iv_ddic    TYPE abap_bool DEFAULT abap_false
                iv_descr   TYPE string
      CHANGING  ct_files   TYPE ty_file_signatures_tt
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object DEFINITION
