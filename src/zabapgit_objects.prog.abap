*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECTS
*&---------------------------------------------------------------------*

*"Null Object Pattern
*CLASS lcl_comparison_null DEFINITION FINAL.
*  PUBLIC SECTION.
*    INTERFACES zif_abapgit_comparison_result.
*ENDCLASS.
*
*CLASS lcl_comparison_null IMPLEMENTATION.
*
*  METHOD zif_abapgit_comparison_result~is_result_complete_halt.
*    rv_response = abap_false.
*  ENDMETHOD.
*
*  METHOD zif_abapgit_comparison_result~show_confirmation_dialog.
*    RETURN.
*  ENDMETHOD.
*
*ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_objects_super DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
*CLASS lcl_objects_super DEFINITION ABSTRACT.
*
*  PUBLIC SECTION.
*
*    METHODS:
*      constructor
*        IMPORTING
*          is_item     TYPE zif_abapgit_definitions=>ty_item
*          iv_language TYPE spras.
*
*    CLASS-METHODS:
*      jump_adt
*        IMPORTING i_obj_name TYPE zif_abapgit_definitions=>ty_item-obj_name
*                  i_obj_type TYPE zif_abapgit_definitions=>ty_item-obj_type
*        RAISING   zcx_abapgit_exception.
*
*    CONSTANTS: c_user_unknown TYPE xubname VALUE 'UNKNOWN'.
*
*  PROTECTED SECTION.
*
*    DATA: ms_item     TYPE zif_abapgit_definitions=>ty_item,
*          mv_language TYPE spras.
*
*    METHODS:
*      check_timestamp
*        IMPORTING
*          iv_timestamp      TYPE timestamp
*          iv_date           TYPE datum
*          iv_time           TYPE uzeit
*        RETURNING
*          VALUE(rv_changed) TYPE abap_bool,
*      get_metadata
*        RETURNING VALUE(rs_metadata) TYPE zif_abapgit_definitions=>ty_metadata,
*      corr_insert
*        IMPORTING iv_package TYPE devclass
*        RAISING   zcx_abapgit_exception,
*      tadir_insert
*        IMPORTING iv_package TYPE devclass
*        RAISING   zcx_abapgit_exception,
*      jump_se11
*        IMPORTING iv_radio TYPE string
*                  iv_field TYPE string
*        RAISING   zcx_abapgit_exception.
*
*  PRIVATE SECTION.
*
*    CLASS-METHODS:
*      is_adt_jump_possible
*        IMPORTING io_object                     TYPE REF TO cl_wb_object
*                  io_adt                        TYPE REF TO object
*        RETURNING VALUE(r_is_adt_jump_possible) TYPE abap_bool
*        RAISING   zcx_abapgit_exception.
*
*ENDCLASS.                    "lcl_objects_super DEFINITION

**********************************************************************
* Enable plugins

CLASS lcl_objects_bridge DEFINITION INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.

    CLASS-METHODS class_constructor.

    METHODS constructor
      IMPORTING is_item TYPE zif_abapgit_definitions=>ty_item
      RAISING   cx_sy_create_object_error.

    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

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

  METHOD zif_abapgit_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.  "lif_object~has_changed_since

  METHOD zif_abapgit_object~get_metadata.

    CALL METHOD mo_plugin->('ZIF_ABAPGITP_PLUGIN~GET_METADATA')
      RECEIVING
        rs_metadata = rs_metadata.

  ENDMETHOD.                    "lif_object~get_metadata

  METHOD zif_abapgit_object~changed_by.
    rv_user = c_user_unknown. " todo
  ENDMETHOD.

  METHOD constructor.

    DATA ls_objtype_map LIKE LINE OF gt_objtype_map.

    super->constructor( is_item = is_item
                        iv_language = zif_abapgit_definitions=>gc_english ).

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

  METHOD zif_abapgit_object~serialize.

    CALL METHOD mo_plugin->('WRAP_SERIALIZE')
      EXPORTING
        io_xml = io_xml.

  ENDMETHOD.                    "lif_object~serialize

  METHOD zif_abapgit_object~deserialize.

    DATA: lx_plugin        TYPE REF TO cx_static_check.

    TRY.
        CALL METHOD mo_plugin->('WRAP_DESERIALIZE')
          EXPORTING
            iv_package = iv_package
            io_xml     = io_xml.
      CATCH cx_static_check INTO lx_plugin.
        zcx_abapgit_exception=>raise( lx_plugin->get_text( ) ).
    ENDTRY.
  ENDMETHOD.                    "lif_object~deserialize

  METHOD zif_abapgit_object~delete.
    DATA lx_plugin TYPE REF TO cx_static_check.

    TRY.
        CALL METHOD mo_plugin->('ZIF_ABAPGITP_PLUGIN~DELETE').
      CATCH cx_static_check INTO lx_plugin.
        zcx_abapgit_exception=>raise( lx_plugin->get_text( ) ).
    ENDTRY.

  ENDMETHOD.                    "lif_object~delete

  METHOD zif_abapgit_object~exists.

    CALL METHOD mo_plugin->('ZIF_ABAPGITP_PLUGIN~EXISTS')
      RECEIVING
        rv_bool = rv_bool.

  ENDMETHOD.                    "lif_object~exists

  METHOD zif_abapgit_object~jump.

    CALL METHOD mo_plugin->('ZIF_ABAPGITP_PLUGIN~JUMP').

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
      WHERE ext~refclsname LIKE 'ZCL_ABAPGITP_OBJECT%'
      AND ext~version = '1'.                              "#EC CI_SUBRC

    CLEAR gt_objtype_map.
    LOOP AT lt_plugin_class INTO lv_plugin_class
        WHERE table_line <> 'ZCL_ABAPGITP_OBJECT_BY_SOBJ'.
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
        WHERE table_line = 'ZCL_ABAPGITP_OBJECT_BY_SOBJ'.
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

  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
  ENDMETHOD.

ENDCLASS.                    "lcl_objects_bridge IMPLEMENTATION

**********************************************************************

*CLASS lcl_objects_program DEFINITION INHERITING FROM lcl_objects_super.
*
*  PUBLIC SECTION.
*    TYPES: BEGIN OF ty_progdir,
*             name    TYPE progdir-name,
*             state   TYPE progdir-state,
*             sqlx    TYPE progdir-sqlx,
*             edtx    TYPE progdir-edtx,
*             varcl   TYPE progdir-varcl,
*             dbapl   TYPE progdir-dbapl,
*             dbna    TYPE progdir-dbna,
*             clas    TYPE progdir-clas,
*             type    TYPE progdir-type,
*             occurs  TYPE progdir-occurs,
*             subc    TYPE progdir-subc,
*             appl    TYPE progdir-appl,
*             secu    TYPE progdir-secu,
*             cnam    TYPE progdir-cnam,
*             cdat    TYPE progdir-cdat,
*             unam    TYPE progdir-unam,
*             udat    TYPE progdir-udat,
*             vern    TYPE progdir-vern,
*             levl    TYPE progdir-levl,
*             rstat   TYPE progdir-rstat,
*             rmand   TYPE progdir-rmand,
*             rload   TYPE progdir-rload,
*             fixpt   TYPE progdir-fixpt,
*             sset    TYPE progdir-sset,
*             sdate   TYPE progdir-sdate,
*             stime   TYPE progdir-stime,
*             idate   TYPE progdir-idate,
*             itime   TYPE progdir-itime,
*             ldbname TYPE progdir-ldbname,
*             uccheck TYPE progdir-uccheck,
*           END OF ty_progdir.
*
*    METHODS serialize_program
*      IMPORTING io_xml     TYPE REF TO zcl_abapgit_xml_output OPTIONAL
*                is_item    TYPE zif_abapgit_definitions=>ty_item
*                io_files   TYPE REF TO zcl_abapgit_objects_files
*                iv_program TYPE programm OPTIONAL
*                iv_extra   TYPE clike OPTIONAL
*      RAISING   zcx_abapgit_exception.
*
*    METHODS read_progdir
*      IMPORTING iv_program        TYPE programm
*      RETURNING VALUE(rs_progdir) TYPE ty_progdir.
*
*    METHODS deserialize_program
*      IMPORTING is_progdir TYPE ty_progdir
*                it_source  TYPE abaptxt255_tab
*                it_tpool   TYPE textpool_table
*                iv_package TYPE devclass
*      RAISING   zcx_abapgit_exception.
*
*  PROTECTED SECTION.
*
*    TYPES: ty_spaces_tt TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
*
*    TYPES: BEGIN OF ty_dynpro,
*             header     TYPE rpy_dyhead,
*             containers TYPE dycatt_tab,
*             fields     TYPE dyfatc_tab,
*             flow_logic TYPE swydyflow,
*             spaces     TYPE ty_spaces_tt,
*           END OF ty_dynpro.
*
*    TYPES: ty_dynpro_tt TYPE STANDARD TABLE OF ty_dynpro WITH DEFAULT KEY.
*
*    TYPES: BEGIN OF ty_cua,
*             adm TYPE rsmpe_adm,
*             sta TYPE STANDARD TABLE OF rsmpe_stat WITH DEFAULT KEY,
*             fun TYPE STANDARD TABLE OF rsmpe_funt WITH DEFAULT KEY,
*             men TYPE STANDARD TABLE OF rsmpe_men WITH DEFAULT KEY,
*             mtx TYPE STANDARD TABLE OF rsmpe_mnlt WITH DEFAULT KEY,
*             act TYPE STANDARD TABLE OF rsmpe_act WITH DEFAULT KEY,
*             but TYPE STANDARD TABLE OF rsmpe_but WITH DEFAULT KEY,
*             pfk TYPE STANDARD TABLE OF rsmpe_pfk WITH DEFAULT KEY,
*             set TYPE STANDARD TABLE OF rsmpe_staf WITH DEFAULT KEY,
*             doc TYPE STANDARD TABLE OF rsmpe_atrt WITH DEFAULT KEY,
*             tit TYPE STANDARD TABLE OF rsmpe_titt WITH DEFAULT KEY,
*             biv TYPE STANDARD TABLE OF rsmpe_buts WITH DEFAULT KEY,
*           END OF ty_cua.
*
*    METHODS serialize_dynpros
*      IMPORTING iv_program_name  TYPE programm
*      RETURNING VALUE(rt_dynpro) TYPE ty_dynpro_tt
*      RAISING   zcx_abapgit_exception.
*
*    METHODS serialize_cua
*      IMPORTING iv_program_name TYPE programm
*      RETURNING VALUE(rs_cua)   TYPE ty_cua
*      RAISING   zcx_abapgit_exception.
*
*    METHODS deserialize_dynpros
*      IMPORTING it_dynpros TYPE ty_dynpro_tt
*      RAISING   zcx_abapgit_exception.
*
*    METHODS deserialize_textpool
*      IMPORTING iv_program    TYPE programm
*                it_tpool      TYPE textpool_table
*                iv_language   TYPE langu OPTIONAL
*                iv_is_include TYPE abap_bool DEFAULT abap_false
*      RAISING   zcx_abapgit_exception.
*
*    METHODS deserialize_cua
*      IMPORTING iv_program_name TYPE programm
*                is_cua          TYPE ty_cua
*      RAISING   zcx_abapgit_exception.
*
*    METHODS check_prog_changed_since
*      IMPORTING iv_program        TYPE programm
*                iv_timestamp      TYPE timestamp
*                iv_skip_gui       TYPE abap_bool DEFAULT abap_false
*      RETURNING VALUE(rv_changed) TYPE abap_bool.
*
*    CLASS-METHODS:
*      add_tpool
*        IMPORTING it_tpool        TYPE textpool_table
*        RETURNING VALUE(rt_tpool) TYPE zif_abapgit_definitions=>ty_tpool_tt,
*      read_tpool
*        IMPORTING it_tpool        TYPE zif_abapgit_definitions=>ty_tpool_tt
*        RETURNING VALUE(rt_tpool) TYPE zif_abapgit_definitions=>ty_tpool_tt.
*
*  PRIVATE SECTION.
*    METHODS:
*      condense_flow
*        EXPORTING et_spaces TYPE ty_spaces_tt
*        CHANGING  ct_flow   TYPE swydyflow,
*      uncondense_flow
*        IMPORTING it_flow        TYPE swydyflow
*                  it_spaces      TYPE ty_spaces_tt
*        RETURNING VALUE(rt_flow) TYPE swydyflow.
*
*
*ENDCLASS.                    "lcl_objects_program DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_objects_program IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
*CLASS lcl_objects_program IMPLEMENTATION.
*
*  METHOD condense_flow.
*
*    DATA: lv_spaces LIKE LINE OF et_spaces.
*
*    FIELD-SYMBOLS: <ls_flow> LIKE LINE OF ct_flow.
*
*
*    CLEAR et_spaces.
*
*    LOOP AT ct_flow ASSIGNING <ls_flow>.
*      lv_spaces = 0.
*
*      WHILE NOT <ls_flow>-line IS INITIAL AND <ls_flow>-line(1) = space.
*        lv_spaces = lv_spaces + 1.
*        <ls_flow>-line = <ls_flow>-line+1.
*      ENDWHILE.
*
*      APPEND lv_spaces TO et_spaces.
*    ENDLOOP.
*
*  ENDMETHOD.
*
*  METHOD uncondense_flow.
*
*    DATA: lv_spaces LIKE LINE OF it_spaces.
*
*    FIELD-SYMBOLS: <ls_flow>   LIKE LINE OF it_flow,
*                   <ls_output> LIKE LINE OF rt_flow.
*
*
*    LOOP AT it_flow ASSIGNING <ls_flow>.
*      APPEND INITIAL LINE TO rt_flow ASSIGNING <ls_output>.
*      <ls_output>-line = <ls_flow>-line.
*
*      READ TABLE it_spaces INDEX sy-tabix INTO lv_spaces.
*      IF sy-subrc = 0.
*        SHIFT <ls_output>-line RIGHT BY lv_spaces PLACES IN CHARACTER MODE.
*      ENDIF.
*    ENDLOOP.
*
*  ENDMETHOD.
*
*  METHOD serialize_program.
*
*    DATA: ls_progdir      TYPE ty_progdir,
*          lv_program_name TYPE programm,
*          lt_dynpros      TYPE ty_dynpro_tt,
*          ls_cua          TYPE ty_cua,
*          lt_source       TYPE TABLE OF abaptxt255,
*          lt_tpool        TYPE textpool_table,
*          ls_tpool        LIKE LINE OF lt_tpool,
*          lo_xml          TYPE REF TO zcl_abapgit_xml_output.
*
*    IF iv_program IS INITIAL.
*      lv_program_name = is_item-obj_name.
*    ELSE.
*      lv_program_name = iv_program.
*    ENDIF.
*
*    zcl_abapgit_language=>set_current_language( mv_language ).
*
*    CALL FUNCTION 'RPY_PROGRAM_READ'
*      EXPORTING
*        program_name     = lv_program_name
*        with_lowercase   = abap_true
*      TABLES
*        source_extended  = lt_source
*        textelements     = lt_tpool
*      EXCEPTIONS
*        cancelled        = 1
*        not_found        = 2
*        permission_error = 3
*        OTHERS           = 4.
*
*    IF sy-subrc = 2.
*      zcl_abapgit_language=>restore_login_language( ).
*      RETURN.
*    ELSEIF sy-subrc <> 0.
*      zcl_abapgit_language=>restore_login_language( ).
*      zcx_abapgit_exception=>raise( 'Error reading program' ).
*    ENDIF.
*
*    zcl_abapgit_language=>restore_login_language( ).
*
*    ls_progdir = read_progdir( lv_program_name ).
*
*    IF io_xml IS BOUND.
*      lo_xml = io_xml.
*    ELSE.
*      CREATE OBJECT lo_xml.
*    ENDIF.
*
*    lo_xml->add( iv_name = 'PROGDIR'
*                 ig_data = ls_progdir ).
*    IF ls_progdir-subc = '1' OR ls_progdir-subc = 'M'.
*      lt_dynpros = serialize_dynpros( lv_program_name ).
*      lo_xml->add( iv_name = 'DYNPROS'
*                   ig_data = lt_dynpros ).
*
*      ls_cua = serialize_cua( lv_program_name ).
*      IF NOT ls_cua IS INITIAL.
*        lo_xml->add( iv_name = 'CUA'
*                     ig_data = ls_cua ).
*      ENDIF.
*    ENDIF.
*
*    READ TABLE lt_tpool WITH KEY id = 'R' INTO ls_tpool.
*    IF sy-subrc = 0 AND ls_tpool-key = '' AND ls_tpool-length = 0.
*      DELETE lt_tpool INDEX sy-tabix.
*    ENDIF.
*
*    lo_xml->add( iv_name = 'TPOOL'
*                 ig_data = add_tpool( lt_tpool ) ).
*
*    IF NOT io_xml IS BOUND.
*      io_files->add_xml( iv_extra = iv_extra
*                         io_xml   = lo_xml ).
*    ENDIF.
*
*    io_files->add_abap( iv_extra = iv_extra
*                        it_abap  = lt_source ).
*
*  ENDMETHOD.                    "serialize_program
*
*  METHOD deserialize_program.
*
*    DATA: lv_exists      TYPE sap_bool,
*          lv_progname    TYPE reposrc-progname,
*          ls_tpool       LIKE LINE OF it_tpool,
*          lv_title       TYPE rglif-title,
*          ls_progdir_new TYPE progdir.
*
*    FIELD-SYMBOLS: <lg_any> TYPE any.
*
*
*    CALL FUNCTION 'RS_CORR_INSERT'
*      EXPORTING
*        object              = is_progdir-name
*        object_class        = 'ABAP'
*        devclass            = iv_package
*        master_language     = mv_language
*        mode                = 'INSERT'
*      EXCEPTIONS
*        cancelled           = 1
*        permission_failure  = 2
*        unknown_objectclass = 3
*        OTHERS              = 4.
*    IF sy-subrc = 1.
*      zcx_abapgit_exception=>raise( 'Cancelled' ).
*    ELSEIF sy-subrc <> 0.
*      zcx_abapgit_exception=>raise( 'error from RS_CORR_INSERT' ).
*    ENDIF.
*
*    READ TABLE it_tpool INTO ls_tpool WITH KEY id = 'R'.  "#EC CI_SUBRC
*    IF sy-subrc = 0.
** there is a bug in RPY_PROGRAM_UPDATE, the header line of TTAB is not
** cleared, so the title length might be inherited from a different program.
*      ASSIGN ('(SAPLSIFP)TTAB') TO <lg_any>.
*      IF sy-subrc = 0.
*        CLEAR <lg_any>.
*      ENDIF.
*
*      lv_title = ls_tpool-entry.
*    ENDIF.
*
*    SELECT SINGLE progname FROM reposrc INTO lv_progname
*      WHERE progname = is_progdir-name
*      AND r3state = 'A'.
*    IF sy-subrc = 0.
*      lv_exists = abap_true.
*    ELSE.
*      lv_exists = abap_false.
*    ENDIF.
*
*    IF lv_exists = abap_true.
*      zcl_abapgit_language=>set_current_language( mv_language ).
*
*      CALL FUNCTION 'RPY_PROGRAM_UPDATE'
*        EXPORTING
*          program_name     = is_progdir-name
*          title_string     = lv_title
*          save_inactive    = 'I'
*        TABLES
*          source_extended  = it_source
*        EXCEPTIONS
*          cancelled        = 1
*          permission_error = 2
*          not_found        = 3
*          OTHERS           = 4.
*
*      IF sy-subrc <> 0.
*        zcl_abapgit_language=>restore_login_language( ).
*
*        IF sy-msgid = 'EU' AND sy-msgno = '510'.
*          zcx_abapgit_exception=>raise( 'User is currently editing program' ).
*        ELSE.
*          zcx_abapgit_exception=>raise( 'PROG, error updating' ).
*        ENDIF.
*      ENDIF.
*
*      zcl_abapgit_language=>restore_login_language( ).
*    ELSE.
** function module RPY_PROGRAM_INSERT cannot handle function group includes
*
*      IF strlen( is_progdir-name ) > 30.
*        " special treatment for extensions
*        " if the program name exceeds 30 characters it is not a usual
*        " ABAP program but might be some extension, which requires the internal
*        " addition EXTENSION TYPE, see
*        " http://help.sap.com/abapdocu_751/en/abapinsert_report_internal.htm#!ABAP_ADDITION_1@1@
*        " This e.g. occurs in case of transportable Code Inspector variants (ending with ===VC)
*        INSERT REPORT is_progdir-name
*         FROM it_source
*         STATE 'I'
*         EXTENSION TYPE is_progdir-name+30.
*        IF sy-subrc <> 0.
*          zcx_abapgit_exception=>raise( 'error from INSERT REPORT .. EXTENSION TYPE' ).
*        ENDIF.
*      ELSE.
*        INSERT REPORT is_progdir-name
*          FROM it_source
*          STATE 'I'
*          PROGRAM TYPE is_progdir-subc.
*        IF sy-subrc <> 0.
*          zcx_abapgit_exception=>raise( 'error from INSERT REPORT' ).
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*    IF NOT it_tpool[] IS INITIAL.
*      INSERT TEXTPOOL is_progdir-name
*        FROM it_tpool
*        LANGUAGE mv_language
*        STATE 'I'.
*      IF sy-subrc <> 0.
*        zcx_abapgit_exception=>raise( 'error from INSERT TEXTPOOL' ).
*      ENDIF.
*    ENDIF.
*
*    CALL FUNCTION 'READ_PROGDIR'
*      EXPORTING
*        i_progname = is_progdir-name
*        i_state    = 'I'
*      IMPORTING
*        e_progdir  = ls_progdir_new
*      EXCEPTIONS
*        not_exists = 1
*        OTHERS     = 2.
*    IF sy-subrc <> 0.
*      zcx_abapgit_exception=>raise( 'not found in PROGDIR' ).
*    ENDIF.
*
** todo, package?
*
*    ls_progdir_new-ldbname = is_progdir-ldbname.
*    ls_progdir_new-dbna    = is_progdir-dbna.
*    ls_progdir_new-dbapl   = is_progdir-dbapl.
*    ls_progdir_new-rload   = is_progdir-rload.
*    ls_progdir_new-fixpt   = is_progdir-fixpt.
*    ls_progdir_new-varcl   = is_progdir-varcl.
*    ls_progdir_new-appl    = is_progdir-appl.
*    ls_progdir_new-rstat   = is_progdir-rstat.
*
*    CALL FUNCTION 'UPDATE_PROGDIR'
*      EXPORTING
*        i_progdir    = ls_progdir_new
*        i_progname   = ls_progdir_new-name
*        i_state      = ls_progdir_new-state
*      EXCEPTIONS
*        not_executed = 1
*        OTHERS       = 2.
*    IF sy-subrc <> 0.
*      zcx_abapgit_exception=>raise( 'PROG, error inserting' ).
*    ENDIF.
*
*    SELECT SINGLE * FROM progdir INTO ls_progdir_new
*      WHERE name = ls_progdir_new-name
*      AND state = ls_progdir_new-state.
*    IF sy-subrc = 0 AND is_progdir-varcl = space AND ls_progdir_new-varcl = abap_true.
** function module UPDATE_PROGDIR does not update VARCL
*      UPDATE progdir SET varcl = is_progdir-varcl
*        WHERE name = ls_progdir_new-name
*        AND state = ls_progdir_new-state.                 "#EC CI_SUBRC
*    ENDIF.
*
*    zcl_abapgit_objects_activation=>add(
*      iv_type = 'REPS'
*      iv_name = is_progdir-name ).
*
*  ENDMETHOD.                    "deserialize_program
*
*  METHOD read_progdir.
*
*    DATA: ls_sapdir TYPE progdir.
*
*
*    CALL FUNCTION 'READ_PROGDIR'
*      EXPORTING
*        i_progname = iv_program
*        i_state    = 'A'
*      IMPORTING
*        e_progdir  = ls_sapdir.
*    MOVE-CORRESPONDING ls_sapdir TO rs_progdir.
*
*    CLEAR: rs_progdir-edtx,
*           rs_progdir-cnam,
*           rs_progdir-cdat,
*           rs_progdir-unam,
*           rs_progdir-udat,
*           rs_progdir-levl,
*           rs_progdir-vern,
*           rs_progdir-rmand,
*           rs_progdir-sdate,
*           rs_progdir-stime,
*           rs_progdir-idate,
*           rs_progdir-itime.
*
*  ENDMETHOD.                    "read_progdir
*
*  METHOD serialize_cua.
*
*    CALL FUNCTION 'RS_CUA_INTERNAL_FETCH'
*      EXPORTING
*        program         = iv_program_name
*        language        = mv_language
*        state           = 'A'
*      IMPORTING
*        adm             = rs_cua-adm
*      TABLES
*        sta             = rs_cua-sta
*        fun             = rs_cua-fun
*        men             = rs_cua-men
*        mtx             = rs_cua-mtx
*        act             = rs_cua-act
*        but             = rs_cua-but
*        pfk             = rs_cua-pfk
*        set             = rs_cua-set
*        doc             = rs_cua-doc
*        tit             = rs_cua-tit
*        biv             = rs_cua-biv
*      EXCEPTIONS
*        not_found       = 1
*        unknown_version = 2
*        OTHERS          = 3.
*    IF sy-subrc <> 0.
*      zcx_abapgit_exception=>raise( 'error from RS_CUA_INTERNAL_FETCH' ).
*    ENDIF.
*
*  ENDMETHOD.                    "serialize_cua
*
*  METHOD serialize_dynpros.
*
*    DATA: ls_header               TYPE rpy_dyhead,
*          lt_containers           TYPE dycatt_tab,
*          lt_fields_to_containers TYPE dyfatc_tab,
*          lt_flow_logic           TYPE swydyflow,
*          lt_d020s                TYPE TABLE OF d020s.
*
*    FIELD-SYMBOLS: <ls_d020s>       LIKE LINE OF lt_d020s,
*                   <lv_outputstyle> TYPE scrpostyle,
*                   <ls_field>       LIKE LINE OF lt_fields_to_containers,
*                   <ls_dynpro>      LIKE LINE OF rt_dynpro.
*
*
*    CALL FUNCTION 'RS_SCREEN_LIST'
*      EXPORTING
*        dynnr     = ''
*        progname  = iv_program_name
*      TABLES
*        dynpros   = lt_d020s
*      EXCEPTIONS
*        not_found = 1
*        OTHERS    = 2.
*    IF sy-subrc = 2.
*      zcx_abapgit_exception=>raise( 'error from screen_list' ).
*    ENDIF.
*
** loop dynpros and skip generated selection screens
*    LOOP AT lt_d020s ASSIGNING <ls_d020s> WHERE type <> 'S'.
*
*      CALL FUNCTION 'RPY_DYNPRO_READ'
*        EXPORTING
*          progname             = iv_program_name
*          dynnr                = <ls_d020s>-dnum
*        IMPORTING
*          header               = ls_header
*        TABLES
*          containers           = lt_containers
*          fields_to_containers = lt_fields_to_containers
*          flow_logic           = lt_flow_logic
*        EXCEPTIONS
*          cancelled            = 1
*          not_found            = 2
*          permission_error     = 3
*          OTHERS               = 4.
*      IF sy-subrc <> 0.
*        zcx_abapgit_exception=>raise( 'Error while reading dynpro' ).
*      ENDIF.
*
*      LOOP AT lt_fields_to_containers ASSIGNING <ls_field>.
** output style is a NUMC field, the XML conversion will fail if it contains invalid value
** field does not exist in all versions
*        ASSIGN COMPONENT 'OUTPUTSTYLE' OF STRUCTURE <ls_field> TO <lv_outputstyle>.
*        IF sy-subrc = 0 AND <lv_outputstyle> = '  '.
*          CLEAR <lv_outputstyle>.
*        ENDIF.
*      ENDLOOP.
*
*      APPEND INITIAL LINE TO rt_dynpro ASSIGNING <ls_dynpro>.
*      <ls_dynpro>-header     = ls_header.
*      <ls_dynpro>-containers = lt_containers.
*      <ls_dynpro>-fields     = lt_fields_to_containers.
*
*      condense_flow( IMPORTING et_spaces = <ls_dynpro>-spaces
*                     CHANGING ct_flow = lt_flow_logic ).
*      <ls_dynpro>-flow_logic = lt_flow_logic.
*
*    ENDLOOP.
*
*  ENDMETHOD.                    "serialize_dynpros
*
*
*  METHOD deserialize_dynpros.
*
*    DATA: lv_name   TYPE dwinactiv-obj_name,
*          ls_dynpro LIKE LINE OF it_dynpros.
*
*
** ls_dynpro is changed by the function module, a field-symbol will cause
** the program to dump since it_dynpros cannot be changed
*    LOOP AT it_dynpros INTO ls_dynpro.
*
*      ls_dynpro-flow_logic = uncondense_flow(
*        it_flow = ls_dynpro-flow_logic
*        it_spaces = ls_dynpro-spaces ).
*
*      CALL FUNCTION 'RPY_DYNPRO_INSERT'
*        EXPORTING
*          header                 = ls_dynpro-header
*          suppress_exist_checks  = abap_true
*        TABLES
*          containers             = ls_dynpro-containers
*          fields_to_containers   = ls_dynpro-fields
*          flow_logic             = ls_dynpro-flow_logic
*        EXCEPTIONS
*          cancelled              = 1
*          already_exists         = 2
*          program_not_exists     = 3
*          not_executed           = 4
*          missing_required_field = 5
*          illegal_field_value    = 6
*          field_not_allowed      = 7
*          not_generated          = 8
*          illegal_field_position = 9
*          OTHERS                 = 10.
*      IF sy-subrc <> 2 AND sy-subrc <> 0.
*        zcx_abapgit_exception=>raise( 'error from RPY_DYNPRO_INSERT' ).
*      ENDIF.
** todo, RPY_DYNPRO_UPDATE?
*
*      CONCATENATE ls_dynpro-header-program ls_dynpro-header-screen
*        INTO lv_name RESPECTING BLANKS.
*      ASSERT NOT lv_name IS INITIAL.
*
*      zcl_abapgit_objects_activation=>add(
*        iv_type = 'DYNP'
*        iv_name = lv_name ).
*
*    ENDLOOP.
*
*  ENDMETHOD.                    "deserialize_dynpros
*
*  METHOD add_tpool.
*
*    FIELD-SYMBOLS: <ls_tpool_in>  LIKE LINE OF it_tpool,
*                   <ls_tpool_out> LIKE LINE OF rt_tpool.
*
*
*    LOOP AT it_tpool ASSIGNING <ls_tpool_in>.
*      APPEND INITIAL LINE TO rt_tpool ASSIGNING <ls_tpool_out>.
*      MOVE-CORRESPONDING <ls_tpool_in> TO <ls_tpool_out>.
*      IF <ls_tpool_out>-id = 'S'.
*        <ls_tpool_out>-split = <ls_tpool_out>-entry.
*        <ls_tpool_out>-entry = <ls_tpool_out>-entry+8.
*      ENDIF.
*    ENDLOOP.
*
*  ENDMETHOD.                    "add_tpool
*
*  METHOD read_tpool.
*
*    FIELD-SYMBOLS: <ls_tpool_in>  LIKE LINE OF it_tpool,
*                   <ls_tpool_out> LIKE LINE OF rt_tpool.
*
*
*    LOOP AT it_tpool ASSIGNING <ls_tpool_in>.
*      APPEND INITIAL LINE TO rt_tpool ASSIGNING <ls_tpool_out>.
*      MOVE-CORRESPONDING <ls_tpool_in> TO <ls_tpool_out>.
*      IF <ls_tpool_out>-id = 'S'.
*        CONCATENATE <ls_tpool_in>-split <ls_tpool_in>-entry
*          INTO <ls_tpool_out>-entry
*          RESPECTING BLANKS.
*      ENDIF.
*    ENDLOOP.
*
*  ENDMETHOD.                    "read_tpool
*
*  METHOD deserialize_textpool.
*
*    DATA lv_language TYPE langu.
*    DATA lv_state    TYPE c.
*    DATA lv_delete   TYPE abap_bool.
*
*    IF iv_language IS INITIAL.
*      lv_language = mv_language.
*    ELSE.
*      lv_language = iv_language.
*    ENDIF.
*
*    IF lv_language = mv_language.
*      lv_state = 'I'. "Textpool in master language needs to be activated
*    ELSE.
*      lv_state = 'A'. "Translations are always active
*    ENDIF.
*
*    IF it_tpool IS INITIAL.
*      IF iv_is_include = abap_false OR lv_state = 'A'.
*        DELETE TEXTPOOL iv_program "Remove initial description from textpool if
*          LANGUAGE iv_program      "original program does not have a textpool
*          STATE lv_state.
*
*        lv_delete = abap_true.
*      ELSE.
*        INSERT TEXTPOOL iv_program "In case of includes: Deletion of textpool in
*          FROM it_tpool            "master language cannot be activated because
*          LANGUAGE lv_language     "this woul activate the deletion of the textpool
*          STATE lv_state.          "of the mail program -> insert empty textpool
*      ENDIF.
*    ELSE.
*      IF lines( it_tpool ) = 1 AND lv_language = mv_language.
*        READ TABLE it_tpool WITH KEY id = 'R' TRANSPORTING NO FIELDS.
*        IF sy-subrc = 0.
*          RETURN. "No action because description in master language is already there
*        ENDIF.
*      ENDIF.
*
*      INSERT TEXTPOOL iv_program
*        FROM it_tpool
*        LANGUAGE lv_language
*        STATE lv_state.
*      IF sy-subrc <> 0.
*        zcx_abapgit_exception=>raise( 'error from INSERT TEXTPOOL' ).
*      ENDIF.
*    ENDIF.
*
*    IF lv_state = 'I'. "Textpool in master language needs to be activated
*      zcl_abapgit_objects_activation=>add(
*        iv_type   = 'REPT'
*        iv_name   = iv_program
*        iv_delete = lv_delete ).
*    ENDIF.
*  ENDMETHOD.                    "deserialize_textpool
*
*  METHOD deserialize_cua.
*
*    DATA: ls_tr_key TYPE trkey.
*
*
*    IF lines( is_cua-sta ) = 0
*        AND lines( is_cua-fun ) = 0
*        AND lines( is_cua-men ) = 0
*        AND lines( is_cua-mtx ) = 0
*        AND lines( is_cua-act ) = 0
*        AND lines( is_cua-but ) = 0
*        AND lines( is_cua-pfk ) = 0
*        AND lines( is_cua-set ) = 0
*        AND lines( is_cua-doc ) = 0
*        AND lines( is_cua-tit ) = 0
*        AND lines( is_cua-biv ) = 0.
*      RETURN.
*    ENDIF.
*
*    SELECT SINGLE devclass INTO ls_tr_key-devclass
*      FROM tadir
*      WHERE pgmid = 'R3TR'
*      AND object = ms_item-obj_type
*      AND obj_name = ms_item-obj_name.                  "#EC CI_GENBUFF
*    IF sy-subrc <> 0.
*      zcx_abapgit_exception=>raise( 'not found in tadir' ).
*    ENDIF.
*
*    ls_tr_key-obj_type = ms_item-obj_type.
*    ls_tr_key-obj_name = ms_item-obj_name.
*    ls_tr_key-sub_type = 'CUAD'.
*    ls_tr_key-sub_name = iv_program_name.
*
*    sy-tcode = 'SE41' ##write_ok. " evil hack, workaround to handle fixes in note 2159455
*    CALL FUNCTION 'RS_CUA_INTERNAL_WRITE'
*      EXPORTING
*        program   = iv_program_name
*        language  = mv_language
*        tr_key    = ls_tr_key
*        adm       = is_cua-adm
*        state     = 'I'
*      TABLES
*        sta       = is_cua-sta
*        fun       = is_cua-fun
*        men       = is_cua-men
*        mtx       = is_cua-mtx
*        act       = is_cua-act
*        but       = is_cua-but
*        pfk       = is_cua-pfk
*        set       = is_cua-set
*        doc       = is_cua-doc
*        tit       = is_cua-tit
*        biv       = is_cua-biv
*      EXCEPTIONS
*        not_found = 1
*        OTHERS    = 2.
*    IF sy-subrc <> 0.
** if moving code from SAPlink, see https://github.com/larshp/abapGit/issues/562
*      zcx_abapgit_exception=>raise( 'error from RS_CUA_INTERNAL_WRITE' ).
*    ENDIF.
*
*    zcl_abapgit_objects_activation=>add(
*      iv_type = 'CUAD'
*      iv_name = iv_program_name ).
*
*  ENDMETHOD.                    "deserialize_cua
*
*  METHOD check_prog_changed_since.
*
*    DATA: lv_date    TYPE dats,
*          lv_time    TYPE tims,
*          lt_screens TYPE STANDARD TABLE OF d020s,
*          lt_eudb    TYPE STANDARD TABLE OF eudb.
*
*    FIELD-SYMBOLS: <ls_screen> LIKE LINE OF lt_screens,
*                   <ls_eudb>   LIKE LINE OF lt_eudb.
*
*    SELECT SINGLE udat utime FROM reposrc " Program
*      INTO (lv_date, lv_time)
*      WHERE progname = iv_program
*      AND   r3state = 'A'.
*
*    rv_changed = check_timestamp(
*      iv_timestamp = iv_timestamp
*      iv_date      = lv_date
*      iv_time      = lv_time ).
*    IF rv_changed = abap_true.
*      RETURN.
*    ENDIF.
*
*    SELECT SINGLE udat utime FROM repotext " Program text pool
*      INTO (lv_date, lv_time)
*      WHERE progname = iv_program
*      AND   r3state = 'A'.
*
*    IF sy-subrc = 0. " Text not found ? Assuming no changes, see #404
*      rv_changed = check_timestamp(
*        iv_timestamp = iv_timestamp
*        iv_date      = lv_date
*        iv_time      = lv_time ).
*      IF rv_changed = abap_true.
*        RETURN.
*      ENDIF.
*    ENDIF.
*
*    IF iv_skip_gui = abap_true.
*      RETURN.
*    ENDIF.
*
*    SELECT dgen tgen FROM d020s           " Screens
*      INTO CORRESPONDING FIELDS OF TABLE lt_screens
*      WHERE prog = iv_program
*      ORDER BY PRIMARY KEY ##TOO_MANY_ITAB_FIELDS.        "#EC CI_SUBRC
*
*    LOOP AT lt_screens ASSIGNING <ls_screen>.
*      rv_changed = check_timestamp(
*        iv_timestamp = iv_timestamp
*        iv_date      = <ls_screen>-dgen
*        iv_time      = <ls_screen>-tgen ).
*      IF rv_changed = abap_true.
*        RETURN.
*      ENDIF.
*    ENDLOOP.
*
*    SELECT vdatum vzeit FROM eudb         " GUI
*      INTO CORRESPONDING FIELDS OF TABLE lt_eudb
*      WHERE relid = 'CU'
*      AND   name  = iv_program
*      AND   srtf2 = 0
*      ORDER BY PRIMARY KEY ##TOO_MANY_ITAB_FIELDS.        "#EC CI_SUBRC
*
*    LOOP AT lt_eudb ASSIGNING <ls_eudb>.
*      rv_changed = check_timestamp(
*        iv_timestamp = iv_timestamp
*        iv_date      = <ls_eudb>-vdatum
*        iv_time      = <ls_eudb>-vzeit ).
*      IF rv_changed = abap_true.
*        RETURN.
*      ENDIF.
*    ENDLOOP.
*
*  ENDMETHOD.  "check_prog_changed_since
*
*ENDCLASS.                    "lcl_objects_program IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_objects_super IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
*CLASS lcl_objects_super IMPLEMENTATION.
*
*  METHOD constructor.
*    ms_item = is_item.
*    ASSERT NOT ms_item IS INITIAL.
*    mv_language = iv_language.
*    ASSERT NOT mv_language IS INITIAL.
*  ENDMETHOD.                    "constructor
*
*  METHOD jump_se11.
*
*    DATA: lt_bdcdata TYPE TABLE OF bdcdata.
*
*    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.
*
*
*    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
*    <ls_bdcdata>-program  = 'SAPLSD_ENTRY'.
*    <ls_bdcdata>-dynpro   = '1000'.
*    <ls_bdcdata>-dynbegin = abap_true.
*
*    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
*    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
*    <ls_bdcdata>-fval = '=WB_DISPLAY'.
*
*    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
*    <ls_bdcdata>-fnam = iv_radio.
*    <ls_bdcdata>-fval = abap_true.
*
*    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
*    <ls_bdcdata>-fnam = iv_field.
*    <ls_bdcdata>-fval = ms_item-obj_name.
*
*    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
*      STARTING NEW TASK 'GIT'
*      EXPORTING
*        tcode                 = 'SE11'
*        mode_val              = 'E'
*      TABLES
*        using_tab             = lt_bdcdata
*      EXCEPTIONS
*        system_failure        = 1
*        communication_failure = 2
*        resource_failure      = 3
*        OTHERS                = 4
*        ##fm_subrc_ok.                                                   "#EC CI_SUBRC
*
*  ENDMETHOD.                                                "jump_se11
*
*  METHOD jump_adt.
*
*    DATA: adt_link          TYPE string,
*          obj_type          TYPE trobjtype,
*          obj_name          TYPE trobj_name,
*          li_object         TYPE REF TO cl_wb_object,
*          li_adt            TYPE REF TO object,
*          li_adt_uri_mapper TYPE REF TO object,
*          li_adt_objref     TYPE REF TO object ##needed.
*
*    FIELD-SYMBOLS: <uri> TYPE string.
*
*    obj_name = i_obj_name.
*    obj_type = i_obj_type.
*
*    TRY.
*        cl_wb_object=>create_from_transport_key( EXPORTING  p_object = obj_type p_obj_name = obj_name
*                                                 RECEIVING  p_wb_object = li_object
*                                                 EXCEPTIONS OTHERS   = 1 ).
*        IF sy-subrc <> 0.
*          zcx_abapgit_exception=>raise( 'ADT Jump Error' ).
*        ENDIF.
*
*        CALL METHOD ('CL_ADT_TOOLS_CORE_FACTORY')=>('GET_INSTANCE')
*          RECEIVING
*            result = li_adt.
*
*        IF is_adt_jump_possible( io_object = li_object
*                                 io_adt    = li_adt ) = abap_false.
*          zcx_abapgit_exception=>raise( 'ADT Jump Error' ).
*        ENDIF.
*
*        CALL METHOD li_adt->('IF_ADT_TOOLS_CORE_FACTORY~GET_URI_MAPPER')
*          RECEIVING
*            result = li_adt_uri_mapper.
*
*        CALL METHOD li_adt_uri_mapper->('IF_ADT_URI_MAPPER~MAP_WB_OBJECT_TO_OBJREF')
*          EXPORTING
*            wb_object = li_object
*          RECEIVING
*            result    = li_adt_objref.
*
*        ASSIGN ('li_adt_objref->ref_data-uri') TO <uri>.
*        ASSERT sy-subrc = 0.
*
*        CONCATENATE 'adt://' sy-sysid <uri> INTO adt_link.
*
*        cl_gui_frontend_services=>execute( EXPORTING  document = adt_link
*                                           EXCEPTIONS OTHERS   = 1 ).
*
*        IF sy-subrc <> 0.
*          zcx_abapgit_exception=>raise( 'ADT Jump Error' ).
*        ENDIF.
*
*      CATCH cx_root.
*        zcx_abapgit_exception=>raise( 'ADT Jump Error' ).
*    ENDTRY.
*
*  ENDMETHOD.
*
*  METHOD check_timestamp.
*
*    DATA: lv_ts TYPE timestamp.
*
*    IF sy-subrc = 0 AND iv_date IS NOT INITIAL AND iv_time IS NOT INITIAL.
*      cl_abap_tstmp=>systemtstmp_syst2utc(
*        EXPORTING syst_date = iv_date
*                  syst_time = iv_time
*        IMPORTING utc_tstmp = lv_ts ).
*      IF lv_ts < iv_timestamp.
*        rv_changed = abap_false. " Unchanged
*      ELSE.
*        rv_changed = abap_true.
*      ENDIF.
*    ELSE. " Not found? => changed
*      rv_changed = abap_true.
*    ENDIF.
*
*  ENDMETHOD.
*
*  METHOD get_metadata.
*    rs_metadata-class =
*      cl_abap_classdescr=>describe_by_object_ref( me )->get_relative_name( ).
*    rs_metadata-version = 'v1.0.0' ##no_text.
*  ENDMETHOD.                    "get_metadata
*
*  METHOD tadir_insert.
*
*    CALL FUNCTION 'TR_TADIR_INTERFACE'
*      EXPORTING
*        wi_test_modus       = abap_false
*        wi_tadir_pgmid      = 'R3TR'
*        wi_tadir_object     = ms_item-obj_type
*        wi_tadir_obj_name   = ms_item-obj_name
*        wi_tadir_author     = sy-uname
*        wi_tadir_devclass   = iv_package
*        wi_tadir_masterlang = mv_language
*        iv_delflag          = abap_false
*      EXCEPTIONS
*        OTHERS              = 1.
*    IF sy-subrc <> 0.
*      zcx_abapgit_exception=>raise( 'error from TR_TADIR_INTERFACE' ).
*    ENDIF.
*
*  ENDMETHOD.
*
*  METHOD corr_insert.
*
*    DATA: ls_object TYPE ddenqs.
*
*
*    ls_object-objtype = ms_item-obj_type.
*    ls_object-objname = ms_item-obj_name.
*
*    CALL FUNCTION 'RS_CORR_INSERT'
*      EXPORTING
*        object              = ls_object
*        object_class        = 'DICT'
*        devclass            = iv_package
*        master_language     = mv_language
*        mode                = 'INSERT'
*      EXCEPTIONS
*        cancelled           = 1
*        permission_failure  = 2
*        unknown_objectclass = 3
*        OTHERS              = 4.
*    IF sy-subrc = 1.
*      zcx_abapgit_exception=>raise( 'Cancelled' ).
*    ELSEIF sy-subrc <> 0.
*      zcx_abapgit_exception=>raise( 'error from RS_CORR_INSERT' ).
*    ENDIF.
*
*  ENDMETHOD.                    "corr_insert
*
*
*  METHOD is_adt_jump_possible.
*
*    DATA: li_wb_request         TYPE REF TO cl_wb_request,
*          li_adt_uri_mapper_vit TYPE REF TO object,
*          is_vit_wb_request     TYPE abap_bool.
*
*    cl_wb_request=>create_from_object_ref(
*      EXPORTING
*        p_wb_object       = io_object
*      RECEIVING
*        p_wb_request      = li_wb_request
*      EXCEPTIONS
*        illegal_operation = 1
*        cancelled         = 2
*        OTHERS            = 3 ).
*
*    IF sy-subrc <> 0.
*      zcx_abapgit_exception=>raise( 'ADT Jump Error' ).
*    ENDIF.
*
*    TRY.
*        CALL METHOD io_adt->('IF_ADT_TOOLS_CORE_FACTORY~GET_URI_MAPPER_VIT')
*          RECEIVING
*            result = li_adt_uri_mapper_vit.
*
*        CALL METHOD li_adt_uri_mapper_vit->('IF_ADT_URI_MAPPER_VIT~IS_VIT_WB_REQUEST')
*          EXPORTING
*            wb_request = li_wb_request
*          RECEIVING
*            result     = is_vit_wb_request.
*
*        IF is_vit_wb_request = abap_true.
*          r_is_adt_jump_possible = abap_false.
*        ELSE.
*          r_is_adt_jump_possible = abap_true.
*        ENDIF.
*
*      CATCH cx_root.
*        zcx_abapgit_exception=>raise( 'ADT Jump Error' ).
*    ENDTRY.
*
*  ENDMETHOD.
*
*ENDCLASS.                    "lcl_objects_super IMPLEMENTATION

*CLASS lcl_objects_saxx_super DEFINITION ABSTRACT
*                                INHERITING FROM lcl_objects_super.
** common class for SAPC and SAMC objects
*
*  PUBLIC SECTION.
*    INTERFACES:
*      zif_abapgit_object.
*
*  PROTECTED SECTION.
*    METHODS:
*      get_persistence_class_name ABSTRACT
*        RETURNING
*          VALUE(r_persistence_class_name) TYPE seoclsname,
*
*      get_data_class_name ABSTRACT
*        RETURNING
*          VALUE(r_data_class_name) TYPE seoclsname,
*
*      get_data_structure_name ABSTRACT
*        RETURNING
*          VALUE(r_data_structure_name) TYPE string.
*
*  PRIVATE SECTION.
*    DATA: mo_persistence          TYPE REF TO if_wb_object_persist,
*          mo_appl_obj_data        TYPE REF TO if_wb_object_data_model,
*          mv_data_structure_name  TYPE string,
*          mv_appl_obj_cls_name    TYPE seoclsname,
*          mv_persistence_cls_name TYPE seoclsname.
*
*    METHODS:
*      create_channel_objects
*        RAISING
*          zcx_abapgit_exception,
*
*      get_data
*        EXPORTING
*          p_data TYPE any
*        RAISING
*          zcx_abapgit_exception,
*
*      lock
*        RAISING
*          zcx_abapgit_exception,
*
*      unlock
*        RAISING
*          zcx_abapgit_exception,
*
*      get_names.
*
*ENDCLASS.
*
*CLASS lcl_objects_saxx_super IMPLEMENTATION.
*
*  METHOD zif_abapgit_object~has_changed_since.
*    rv_changed = abap_true.
*  ENDMETHOD.
*
*  METHOD zif_abapgit_object~changed_by.
*
*    DATA: lr_data TYPE REF TO data.
*
*    FIELD-SYMBOLS: <lg_data>       TYPE any,
*                   <lg_header>     TYPE any,
*                   <lg_changed_by> TYPE any.
*
*    create_channel_objects( ).
*
*    TRY.
*        CREATE DATA lr_data TYPE (mv_data_structure_name).
*        ASSIGN lr_data->* TO <lg_data>.
*
*      CATCH cx_root.
*        zcx_abapgit_exception=>raise( |{ ms_item-obj_name } not supported| ).
*    ENDTRY.
*
*    get_data(
*      IMPORTING
*        p_data = <lg_data> ).
*
*    ASSIGN COMPONENT 'HEADER' OF STRUCTURE <lg_data> TO <lg_header>.
*    ASSERT sy-subrc = 0.
*    ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <lg_header> TO <lg_changed_by>.
*    ASSERT sy-subrc = 0.
*
*    IF <lg_changed_by> IS NOT INITIAL.
*      rv_user = <lg_changed_by>.
*    ELSE.
*      rv_user = c_user_unknown.
*    ENDIF.
*
*  ENDMETHOD.
*
*  METHOD zif_abapgit_object~get_metadata.
*    rs_metadata = get_metadata( ).
*    rs_metadata-delete_tadir = abap_true.
*  ENDMETHOD.
*
*  METHOD zif_abapgit_object~exists.
*
*    DATA: object_key TYPE seu_objkey.
*
*    create_channel_objects( ).
*
*    object_key = ms_item-obj_name.
*
*    TRY.
*        mo_persistence->get( p_object_key           = object_key
*                             p_version              = 'A'
*                             p_existence_check_only = abap_true  ).
*
*      CATCH cx_swb_object_does_not_exist cx_swb_exception.
*        rv_bool = abap_false.
*        RETURN.
*    ENDTRY.
*
*    rv_bool = abap_true.
*
*  ENDMETHOD.
*
*  METHOD zif_abapgit_object~serialize.
*
*    DATA: lr_data             TYPE REF TO data.
*
*    FIELD-SYMBOLS: <ls_data>   TYPE any,
*                   <ls_header> TYPE any,
*                   <field>     TYPE any.
*
*    create_channel_objects( ).
*
*    TRY.
*        CREATE DATA lr_data TYPE (mv_data_structure_name).
*        ASSIGN lr_data->* TO <ls_data>.
*
*      CATCH cx_root.
*        zcx_abapgit_exception=>raise( |{ ms_item-obj_type } not supported| ).
*    ENDTRY.
*
*    get_data(
*      IMPORTING
*        p_data = <ls_data> ).
*
*    ASSIGN COMPONENT 'HEADER' OF STRUCTURE <ls_data> TO <ls_header>.
*    ASSERT sy-subrc = 0.
*
*    ASSIGN COMPONENT 'CHANGED_ON' OF STRUCTURE <ls_header> TO <field>.
*    ASSERT sy-subrc = 0.
*    CLEAR <field>.
*
*    ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <ls_header> TO <field>.
*    ASSERT sy-subrc = 0.
*    CLEAR <field>.
*
*    ASSIGN COMPONENT 'CHANGED_AT' OF STRUCTURE <ls_header> TO <field>.
*    ASSERT sy-subrc = 0.
*    CLEAR <field>.
*
*    ASSIGN COMPONENT 'CHANGED_CLNT' OF STRUCTURE <ls_header> TO <field>.
*    ASSERT sy-subrc = 0.
*    CLEAR <field>.
*
*    ASSIGN COMPONENT 'CREATED_ON' OF STRUCTURE <ls_header> TO <field>.
*    ASSERT sy-subrc = 0.
*    CLEAR <field>.
*
*    ASSIGN COMPONENT 'CREATED_BY' OF STRUCTURE <ls_header> TO <field>.
*    ASSERT sy-subrc = 0.
*    CLEAR <field>.
*
*    ASSIGN COMPONENT 'CREATED_AT' OF STRUCTURE <ls_header> TO <field>.
*    ASSERT sy-subrc = 0.
*    CLEAR <field>.
*
*    ASSIGN COMPONENT 'CREATED_CLNT' OF STRUCTURE <ls_header> TO <field>.
*    ASSERT sy-subrc = 0.
*    CLEAR <field>.
*
*    io_xml->add( iv_name = ms_item-obj_type
*                 ig_data = <ls_data> ).
*
*  ENDMETHOD.
*
*  METHOD zif_abapgit_object~deserialize.
*
*    DATA: lr_data TYPE REF TO data.
*
*    FIELD-SYMBOLS: <ls_data> TYPE any.
*
*    create_channel_objects( ).
*
*    TRY.
*        CREATE DATA lr_data TYPE (mv_data_structure_name).
*        ASSIGN lr_data->* TO <ls_data>.
*
*      CATCH cx_root.
*        zcx_abapgit_exception=>raise( |{ ms_item-obj_type } not supported| ).
*    ENDTRY.
*
*    io_xml->read(
*      EXPORTING
*        iv_name = ms_item-obj_type
*      CHANGING
*        cg_data = <ls_data> ).
*
*    IF zif_abapgit_object~exists( ) = abap_true.
*      zif_abapgit_object~delete( ).
*    ENDIF.
*
*    TRY.
*        lock( ).
*
*        CALL FUNCTION 'RS_CORR_INSERT'
*          EXPORTING
*            object              = ms_item-obj_name
*            object_class        = ms_item-obj_type
*            mode                = 'I'
*            global_lock         = abap_true
*            devclass            = iv_package
*            master_language     = mv_language
*          EXCEPTIONS
*            cancelled           = 1
*            permission_failure  = 2
*            unknown_objectclass = 3
*            OTHERS              = 4.
*
*        IF sy-subrc <> 0.
*          zcx_abapgit_exception=>raise( |Error occured while creating { ms_item-obj_type }| ).
*        ENDIF.
*
*        mo_appl_obj_data->set_data( <ls_data> ).
*
*        mo_persistence->save( mo_appl_obj_data ).
*
*        unlock( ).
*
*      CATCH cx_swb_exception.
*        zcx_abapgit_exception=>raise( |Error occured while creating { ms_item-obj_type }| ).
*    ENDTRY.
*
*  ENDMETHOD.
*
*  METHOD zif_abapgit_object~delete.
*
*    DATA: object_key TYPE seu_objkey.
*
*    create_channel_objects( ).
*
*    object_key = ms_item-obj_name.
*
*    TRY.
*        lock( ).
*
*        mo_persistence->delete( object_key ).
*
*        unlock( ).
*
*      CATCH cx_swb_exception.
*        zcx_abapgit_exception=>raise( |Error occured while deleting { ms_item-obj_type }| ).
*    ENDTRY.
*
*  ENDMETHOD.
*
*  METHOD zif_abapgit_object~jump.
*
*    CALL FUNCTION 'RS_TOOL_ACCESS'
*      EXPORTING
*        operation   = 'SHOW'
*        object_name = ms_item-obj_name
*        object_type = ms_item-obj_type.
*
*  ENDMETHOD.
*
*  METHOD zif_abapgit_object~compare_to_remote_version.
*    CREATE OBJECT ro_comparison_result TYPE lcl_comparison_null.
*  ENDMETHOD.
*
*  METHOD create_channel_objects.
*
*    get_names( ).
*
*    TRY.
*        IF mo_appl_obj_data IS NOT BOUND.
*          CREATE OBJECT mo_appl_obj_data TYPE (mv_appl_obj_cls_name).
*        ENDIF.
*
*        IF mo_persistence IS NOT BOUND.
*          CREATE OBJECT mo_persistence TYPE (mv_persistence_cls_name).
*        ENDIF.
*
*      CATCH cx_root.
*        zcx_abapgit_exception=>raise( |{ ms_item-obj_type } not supported| ).
*    ENDTRY.
*
*  ENDMETHOD.
*
*  METHOD get_data.
*
*    DATA: object_key TYPE seu_objkey.
*
*    object_key = ms_item-obj_name.
*
*    TRY.
*        mo_persistence->get(
*          EXPORTING
*            p_object_key  = object_key
*            p_version     = 'A'
*          CHANGING
*            p_object_data = mo_appl_obj_data ).
*
*      CATCH cx_root.
*        zcx_abapgit_exception=>raise( |{ ms_item-obj_type } not supported| ).
*    ENDTRY.
*
*    mo_appl_obj_data->get_data(
*      IMPORTING
*        p_data = p_data ).
*
*  ENDMETHOD.
*
*  METHOD lock.
*
*    DATA: objname    TYPE trobj_name,
*          object_key TYPE seu_objkey,
*          objtype    TYPE trobjtype.
*
*    objname    = ms_item-obj_name.
*    object_key = ms_item-obj_name.
*    objtype    = ms_item-obj_type.
*
*    mo_persistence->lock(
*      EXPORTING
*        p_objname_tr   = objname
*        p_object_key   = object_key
*        p_objtype_tr   = objtype
*      EXCEPTIONS
*        foreign_lock   = 1
*        error_occurred = 2
*        OTHERS         = 3 ).
*
*    IF sy-subrc <> 0.
*      zcx_abapgit_exception=>raise( |Error occured while locking { ms_item-obj_type } | && objname ).
*    ENDIF.
*
*  ENDMETHOD.                    "lock
*
*  METHOD unlock.
*
*    DATA: objname    TYPE trobj_name,
*          object_key TYPE seu_objkey,
*          objtype    TYPE trobjtype.
*
*    objname    = ms_item-obj_name.
*    object_key = ms_item-obj_name.
*    objtype    = ms_item-obj_type.
*
*    mo_persistence->unlock( p_objname_tr = objname
*                            p_object_key = object_key
*                            p_objtype_tr = objtype ).
*
*  ENDMETHOD.                    "unlock
*
*  METHOD get_names.
*
*    IF mv_data_structure_name IS INITIAL.
*      mv_data_structure_name  = get_data_structure_name( ).
*    ENDIF.
*
*    IF mv_appl_obj_cls_name IS INITIAL.
*      mv_appl_obj_cls_name    = get_data_class_name( ).
*    ENDIF.
*
*    IF mv_persistence_cls_name IS INITIAL.
*      mv_persistence_cls_name = get_persistence_class_name( ).
*    ENDIF.
*
*  ENDMETHOD.
*
*ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_object DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: ty_types_tt TYPE STANDARD TABLE OF tadir-object WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_deserialization,
             obj     TYPE REF TO zif_abapgit_object,
             xml     TYPE REF TO zcl_abapgit_xml_input,
             package TYPE devclass,
             item    TYPE zif_abapgit_definitions=>ty_item,
           END OF ty_deserialization.

    TYPES: ty_deserialization_tt TYPE STANDARD TABLE OF ty_deserialization WITH DEFAULT KEY.

    CLASS-METHODS serialize
      IMPORTING is_item         TYPE zif_abapgit_definitions=>ty_item
                iv_language     TYPE spras
                io_log          TYPE REF TO zcl_abapgit_log OPTIONAL
      RETURNING VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_files_tt
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS deserialize
      IMPORTING io_repo                  TYPE REF TO lcl_repo
      RETURNING VALUE(rt_accessed_files) TYPE zif_abapgit_definitions=>ty_file_signatures_tt
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS delete
      IMPORTING it_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS jump
      IMPORTING is_item TYPE zif_abapgit_definitions=>ty_item
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS changed_by
      IMPORTING is_item        TYPE zif_abapgit_definitions=>ty_item
      RETURNING VALUE(rv_user) TYPE xubname
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS has_changed_since
      IMPORTING is_item           TYPE zif_abapgit_definitions=>ty_item
                iv_timestamp      TYPE timestamp
      RETURNING VALUE(rv_changed) TYPE abap_bool
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS is_supported
      IMPORTING is_item        TYPE zif_abapgit_definitions=>ty_item
                iv_native_only TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(rv_bool) TYPE abap_bool.

    CLASS-METHODS exists
      IMPORTING is_item        TYPE zif_abapgit_definitions=>ty_item
      RETURNING VALUE(rv_bool) TYPE abap_bool.

    CLASS-METHODS supported_list
      RETURNING VALUE(rt_types) TYPE ty_types_tt.

  PRIVATE SECTION.

    CLASS-METHODS check_duplicates
      IMPORTING it_files TYPE zif_abapgit_definitions=>ty_files_tt
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS create_object
      IMPORTING is_item        TYPE zif_abapgit_definitions=>ty_item
                iv_language    TYPE spras
                is_metadata    TYPE zif_abapgit_definitions=>ty_metadata OPTIONAL
                iv_native_only TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(ri_obj)  TYPE REF TO zif_abapgit_object
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS
      prioritize_deser
        IMPORTING it_results        TYPE zif_abapgit_definitions=>ty_results_tt
        RETURNING VALUE(rt_results) TYPE zif_abapgit_definitions=>ty_results_tt.

    CLASS-METHODS class_name
      IMPORTING is_item              TYPE zif_abapgit_definitions=>ty_item
      RETURNING VALUE(rv_class_name) TYPE string.

    CLASS-METHODS warning_overwrite
      CHANGING ct_results TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING  zcx_abapgit_exception.

    CLASS-METHODS warning_package
      IMPORTING is_item          TYPE zif_abapgit_definitions=>ty_item
                iv_package       TYPE devclass
      RETURNING VALUE(rv_cancel) TYPE abap_bool
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS update_package_tree
      IMPORTING iv_package TYPE devclass.

    CLASS-METHODS delete_obj
      IMPORTING is_item TYPE zif_abapgit_definitions=>ty_item
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS compare_remote_to_local
      IMPORTING
        io_object TYPE REF TO zif_abapgit_object
        it_remote TYPE zif_abapgit_definitions=>ty_files_tt
        is_result TYPE zif_abapgit_definitions=>ty_result
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS deserialize_objects
      IMPORTING it_objects TYPE ty_deserialization_tt
                iv_ddic    TYPE abap_bool DEFAULT abap_false
                iv_descr   TYPE string
      CHANGING  ct_files   TYPE zif_abapgit_definitions=>ty_file_signatures_tt
      RAISING   zcx_abapgit_exception.

ENDCLASS.                    "lcl_object DEFINITION
