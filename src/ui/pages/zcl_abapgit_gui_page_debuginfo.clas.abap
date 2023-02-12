CLASS zcl_abapgit_gui_page_debuginfo DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler .
    INTERFACES zif_abapgit_gui_renderable .

    CLASS-METHODS create
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception .
    METHODS constructor
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_exit_standalone TYPE c LENGTH 30 VALUE 'ZABAPGIT_USER_EXIT' ##NO_TEXT.
    CONSTANTS c_exit_class TYPE c LENGTH 30 VALUE 'ZCL_ABAPGIT_USER_EXIT' ##NO_TEXT.
    CONSTANTS c_exit_interface TYPE c LENGTH 30 VALUE 'ZIF_ABAPGIT_EXIT' ##NO_TEXT.
    CONSTANTS:
      BEGIN OF c_action,
        save TYPE string VALUE 'save',
        back TYPE string VALUE 'back',
      END OF c_action.
    DATA mv_html TYPE string .

    CLASS-METHODS build_toolbar
      RETURNING
        VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar.
    METHODS render_debug_info
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS render_exit_info
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS render_exit_info_methods
      IMPORTING
        !it_source     TYPE string_table
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS render_supported_object_types
      RETURNING
        VALUE(rv_html) TYPE string .
    METHODS render_scripts
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS get_jump_object
      IMPORTING
        !iv_obj_type   TYPE csequence DEFAULT 'CLAS'
        !iv_obj_name   TYPE csequence
      RETURNING
        VALUE(rv_html) TYPE string .
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_DEBUGINFO IMPLEMENTATION.


  METHOD build_toolbar.

    CREATE OBJECT ro_menu EXPORTING iv_id = 'toolbar-debug'.

    ro_menu->add(
      iv_txt = 'Save'
      iv_act = c_action-save ).
    ro_menu->add(
      iv_txt = 'Back'
      iv_act = c_action-back ).

  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).
  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_debuginfo.

    CREATE OBJECT lo_component.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Debug Info'
      io_page_menu       = build_toolbar( )
      ii_child_component = lo_component ).

  ENDMETHOD.


  METHOD get_jump_object.

    DATA lv_encode TYPE string.
    DATA li_html TYPE REF TO zif_abapgit_html.

    CREATE OBJECT li_html TYPE zcl_abapgit_html.

    lv_encode = zcl_abapgit_html_action_utils=>jump_encode( iv_obj_type = |{ iv_obj_type }|
                                                            iv_obj_name = |{ iv_obj_name }| ).

    rv_html = li_html->a(
      iv_txt = |{ iv_obj_name }|
      iv_act = |{ zif_abapgit_definitions=>c_action-jump }?{ lv_encode }| ).

  ENDMETHOD.


  METHOD render_debug_info.

    DATA: lt_ver_tab       TYPE filetable,
          lv_rc            TYPE i,
          ls_release       TYPE zif_abapgit_environment=>ty_release_sp,
          lv_gui_version   TYPE string,
          ls_version       LIKE LINE OF lt_ver_tab,
          lv_devclass      TYPE devclass,
          lo_frontend_serv TYPE REF TO zif_abapgit_frontend_services.

    lo_frontend_serv = zcl_abapgit_ui_factory=>get_frontend_services( ).
    TRY.
        lo_frontend_serv->get_gui_version( CHANGING ct_version_table = lt_ver_tab cv_rc = lv_rc ).
      CATCH zcx_abapgit_exception ##NO_HANDLER.
        " Continue rendering even if this fails
    ENDTRY.

    READ TABLE lt_ver_tab INTO ls_version INDEX 1. " gui release
    lv_gui_version = ls_version-filename.
    READ TABLE lt_ver_tab INTO ls_version INDEX 2. " gui sp
    lv_gui_version = |{ lv_gui_version }.{ ls_version-filename }|.
    READ TABLE lt_ver_tab INTO ls_version INDEX 3. " gui patch
    lv_gui_version = |{ lv_gui_version }.{ ls_version-filename }|.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    IF zcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_true.
      ri_html->add( '<h2>abapGit - Standalone Version</h2>' ).
      ri_html->add( '<div>To keep abapGit up-to-date (or also to contribute) you need to' ).
      ri_html->add( |install it as a repository ({ ri_html->a(
        iv_txt = 'Developer Version'
        iv_act = 'https://github.com/abapGit/abapGit'
        iv_typ = zif_abapgit_html=>c_action_type-url ) }).</div>| ).
    ELSE.
      lv_devclass = zcl_abapgit_services_abapgit=>is_installed( ).
      ri_html->add( '<h2>abapGit - Developer Version</h2>' ).
      ri_html->add( |<div>abapGit is installed in package { lv_devclass }</div>| ).
    ENDIF.

    ri_html->add( '<br><div>' ).
    ri_html->add_a(
      iv_txt = 'Contribution guidelines for abapGit'
      iv_act = 'https://github.com/abapGit/abapGit/blob/main/CONTRIBUTING.md'
      iv_typ = zif_abapgit_html=>c_action_type-url ).
    ri_html->add( '</div>' ).

    ls_release = zcl_abapgit_factory=>get_environment( )->get_basis_release( ).

    ri_html->add( '<h2>Environment</h2>' ).

    ri_html->add( |<table>| ).
    ri_html->add( |<tr><td>abapGit version:</td><td>{ zif_abapgit_version=>c_abap_version }</td></tr>| ).
    ri_html->add( |<tr><td>XML version:    </td><td>{ zif_abapgit_version=>c_xml_version }</td></tr>| ).
    ri_html->add( |<tr><td>GUI version:    </td><td>{ lv_gui_version }</td></tr>| ).
    ri_html->add( |<tr><td>APACK version:  </td><td>{
                  zcl_abapgit_apack_migration=>c_apack_interface_version }</td></tr>| ).
    ri_html->add( |<tr><td>LCL_TIME:       </td><td>{ zcl_abapgit_git_time=>get_unix( ) }</td></tr>| ).
    ri_html->add( |<tr><td>SY time:        </td><td>{ sy-datum } { sy-uzeit } { sy-tzone }</td></tr>| ).
    ri_html->add( |<tr><td>SY release:     </td><td>{ ls_release-release } SP { ls_release-sp }</td></tr>| ).
    ri_html->add( |</table>| ).
    ri_html->add( |<br>| ).

  ENDMETHOD.


  METHOD render_exit_info.

    DATA lt_source TYPE string_table.
    DATA ls_class_key TYPE seoclskey.
    DATA lo_oo_serializer TYPE REF TO zcl_abapgit_oo_serializer.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<h2>User Exits</h2>' ).

    IF zcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_true.
      " Standalone version
      READ REPORT c_exit_standalone INTO lt_source.
      IF sy-subrc = 0.
        ri_html->add( |<div>User exits are active (include { get_jump_object(
          iv_obj_type = 'PROG'
          iv_obj_name = c_exit_standalone ) } found)</div><br>| ).
        ri_html->add( render_exit_info_methods( lt_source ) ).
      ELSE.
        ri_html->add( |<div>No user exits implemented (include { c_exit_standalone } not found)</div><br>| ).
      ENDIF.
    ELSE.
      " Developer version
      TRY.
          ls_class_key-clsname = c_exit_class.
          CREATE OBJECT lo_oo_serializer.
          lt_source = lo_oo_serializer->serialize_abap_clif_source( ls_class_key ).

          ri_html->add( |<div>User exits are active (class { get_jump_object( c_exit_class ) } found)</div><br>| ).
          ri_html->add( render_exit_info_methods( lt_source ) ).
        CATCH cx_root.
          ri_html->add( |<div>No user exits implemented (class { c_exit_class } not found)</div><br>| ).
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD render_exit_info_methods.

    DATA:
      lo_scanner TYPE REF TO cl_oo_source_scanner_class,
      lx_exc     TYPE REF TO cx_root,
      lt_methods TYPE cl_oo_source_scanner_class=>type_method_implementations,
      lv_method  LIKE LINE OF lt_methods,
      lt_source  TYPE seop_source_string,
      lv_source  TYPE string,
      lv_rest    TYPE string.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<table border="1px"><thead><tr>' ).
    ri_html->add( '<td>Exit</td><td class="center">Implemented?</td>' ).
    ri_html->add( '</tr></thead><tbody>' ).

    TRY.
        lo_scanner = cl_oo_source_scanner_class=>create_class_scanner(
          clif_name = c_exit_class
          source    = it_source ).
        lo_scanner->scan( ).

        lt_methods = lo_scanner->get_method_implementations( ).

        LOOP AT lt_methods INTO lv_method WHERE table_line CS c_exit_interface.
          lt_source = lo_scanner->get_method_impl_source( lv_method ).
          DELETE lt_source INDEX 1.
          DELETE lt_source INDEX lines( lt_source ).
          CONCATENATE LINES OF lt_source INTO lv_source.
          lv_source = to_upper( condense(
            val = lv_source
            del = ` ` ) ).
          SPLIT lv_method AT '~' INTO lv_rest lv_method.
          ri_html->add( |<tr><td>{ lv_method }</td><td class="center">|  ).
          IF lv_source IS INITIAL OR lv_source = 'RETURN.' OR lv_source = 'EXIT.'.
            ri_html->add( 'No' ).
          ELSE.
            ri_html->add( '<strong>Yes</strong>' ).
          ENDIF.
          ri_html->add( |</td></tr>| ).
        ENDLOOP.

      CATCH cx_root INTO lx_exc.
        ri_html->add( |<tr><td colspan="2">{ lx_exc->get_text( ) }</td></tr>| ).
    ENDTRY.

    ri_html->add( '</tbody></table>' ).

  ENDMETHOD.


  METHOD render_scripts.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).
    ri_html->add( 'debugOutput("<table><tr><td>Browser:</td><td>" + navigator.userAgent + ' &&
      '"</td></tr><tr><td>Frontend time:</td><td>" + new Date() + "</td></tr></table>", "debug_info");' ).

  ENDMETHOD.


  METHOD render_supported_object_types.

    DATA: lv_list     TYPE string,
          li_html     TYPE REF TO zif_abapgit_html,
          lt_types    TYPE zcl_abapgit_objects=>ty_types_tt,
          lv_type     LIKE LINE OF lt_types,
          lt_obj      TYPE STANDARD TABLE OF ko100 WITH DEFAULT KEY,
          lv_class    TYPE seoclsname,
          li_object   TYPE REF TO zif_abapgit_object,
          ls_item     TYPE zif_abapgit_definitions=>ty_item,
          ls_metadata TYPE zif_abapgit_definitions=>ty_metadata,
          lv_step     TYPE zif_abapgit_definitions=>ty_deserialization_step,
          lt_steps    TYPE zif_abapgit_definitions=>ty_deserialization_step_tt.

    FIELD-SYMBOLS: <ls_obj> TYPE ko100.

    CALL FUNCTION 'TR_OBJECT_TABLE'
      TABLES
        wt_object_text = lt_obj.

    lt_types = zcl_abapgit_objects=>supported_list( ).

    CREATE OBJECT li_html TYPE zcl_abapgit_html.

    rv_html = '<h2>Object Types</h2>'.

    rv_html = rv_html && li_html->a(
      iv_txt = 'Complete list of object types supported by abapGit'
      iv_act = 'https://docs.abapgit.org/ref-supported.html'
      iv_typ = zif_abapgit_html=>c_action_type-url ).

    rv_html = rv_html && |<br><br>Supported object types in <strong>this</strong> system:<br><br>|.

    rv_html = rv_html && |<table border="1px"><thead><tr>|.
    rv_html = rv_html && |<td>Object</td><td>Description</td><td>Class</td><td>Version</td>|.
    rv_html = rv_html && |<td>Steps</td>|.
    rv_html = rv_html && |</tr></thead><tbody>|.

    LOOP AT lt_types INTO lv_type.
      lv_class = 'ZCL_ABAPGIT_OBJECT_' && lv_type.

      rv_html = rv_html && |<tr>|.

      rv_html = rv_html && |<td>{ lv_type }</td>|.

      READ TABLE lt_obj ASSIGNING <ls_obj> WITH KEY pgmid = 'R3TR' object = lv_type.
      IF sy-subrc = 0.
        rv_html = rv_html && |<td>{ <ls_obj>-text }</td>|.
      ELSE.
        rv_html = rv_html && |<td class="warning">No description</td>|.
      ENDIF.


      TRY.
          ls_item-obj_type = lv_type.
          ls_item-obj_name = 'TEST'.

          CREATE OBJECT li_object TYPE (lv_class)
            EXPORTING
              is_item     = ls_item
              iv_language = sy-langu.

          rv_html = rv_html && |<td>{ get_jump_object( lv_class ) }</td>|.

        CATCH cx_sy_create_object_error.
          TRY. " 2nd step, try looking for plugins
              CREATE OBJECT li_object TYPE zcl_abapgit_objects_bridge
                EXPORTING
                  is_item = ls_item.
            CATCH cx_sy_create_object_error.
              rv_html = rv_html && |<td class="error" colspan="5">{ lv_class } - error instantiating class</td>|.
              CONTINUE.
          ENDTRY.

          rv_html = rv_html && |<td>{ get_jump_object( lv_class ) } (Plug-in)</td>|.
      ENDTRY.

      ls_metadata = li_object->get_metadata( ).

      rv_html = rv_html && |<td>{ ls_metadata-version }</td>|.

      lt_steps = li_object->get_deserialize_steps( ).

      CLEAR lv_list.
      LOOP AT lt_steps INTO lv_step.
        CASE lv_step.
          WHEN zif_abapgit_object=>gc_step_id-early.
            lv_step = |<i>{ lv_step } (1)</i>|.
          WHEN zif_abapgit_object=>gc_step_id-ddic.
            lv_step = |<strong>{ lv_step } (2)</strong>|.
          WHEN zif_abapgit_object=>gc_step_id-abap.
            lv_step = |{ lv_step } (3)|.
          WHEN zif_abapgit_object=>gc_step_id-late.
            lv_step = |<i>{ lv_step } (4)</i>|.
          WHEN OTHERS.
            ASSERT 1 = 2.
        ENDCASE.
        IF lv_list IS INITIAL.
          lv_list = lv_step.
        ELSE.
          lv_list = lv_list && `, ` && lv_step.
        ENDIF.
      ENDLOOP.

      rv_html = rv_html && |<td>{ lv_list }</td>|.

      rv_html = rv_html && |</tr>|.

    ENDLOOP.

    rv_html = rv_html && |</tbody></table>|.
    rv_html = rv_html && |<br>|.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA:
      lv_path     TYPE string,
      lv_filename TYPE string,
      li_fe_serv  TYPE REF TO zif_abapgit_frontend_services.

    CASE ii_event->mv_action.
      WHEN c_action-save.

        CONCATENATE 'abapGit_Debug_Info_' sy-datlo '_' sy-timlo '.html' INTO lv_filename.

        li_fe_serv = zcl_abapgit_ui_factory=>get_frontend_services( ).

        lv_path = li_fe_serv->show_file_save_dialog(
          iv_title            = 'abapGit - Debug Info'
          iv_extension        = 'html'
          iv_default_filename = lv_filename ).

        li_fe_serv->file_download(
          iv_path = lv_path
          iv_xstr = zcl_abapgit_convert=>string_to_xstring_utf8( mv_html ) ).

        MESSAGE 'abapGit Debug Info successfully saved' TYPE 'S'.

        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_action-back.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    register_handlers( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div id="debug_info" class="debug_container">' ).
    ri_html->add( render_debug_info( ) ).
    ri_html->add( '</div>' ).

    ri_html->add( '<div id="exit_info" class="debug_container">' ).
    ri_html->add( render_exit_info( ) ).
    ri_html->add( '</div>' ).

    ri_html->add( '<div id="supported_objects" class="debug_container">' ).
    ri_html->add( render_supported_object_types( ) ).
    ri_html->add( '</div>' ).

    mv_html = '<!DOCTYPE html><html lang="en"><title>abapGit Debug Info</title></head>'.
    mv_html = |<body>{ ri_html->render( ) }</body></html>|.

    register_deferred_script( render_scripts( ) ).

  ENDMETHOD.
ENDCLASS.
