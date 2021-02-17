CLASS zcl_abapgit_gui_page_debuginfo DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      RAISING zcx_abapgit_exception.

  PROTECTED SECTION.
    METHODS:
      render_content REDEFINITION.

  PRIVATE SECTION.

    METHODS get_jump_class
      IMPORTING
        !iv_class      TYPE seoclsname
      RETURNING
        VALUE(rv_html) TYPE string .
    METHODS render_debug_info
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
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_DEBUGINFO IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    ms_control-page_title = 'Debug Info'.
  ENDMETHOD.


  METHOD get_jump_class.

    DATA lv_encode TYPE string.
    DATA li_html TYPE REF TO zif_abapgit_html.

    CREATE OBJECT li_html TYPE zcl_abapgit_html.

    lv_encode = zcl_abapgit_html_action_utils=>jump_encode( iv_obj_type = 'CLAS'
                                                            iv_obj_name = |{ iv_class }| ).

    rv_html = li_html->a(
      iv_txt = |{ iv_class }|
      iv_act = |{ zif_abapgit_definitions=>c_action-jump }?{ lv_encode }| ).

  ENDMETHOD.


  METHOD render_content.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div id="debug_info" class="debug_container">' ).
    ri_html->add( render_debug_info( ) ).
    ri_html->add( '</div>' ).

    ri_html->add( '<div id="supported_objects" class="debug_container">' ).
    ri_html->add( render_supported_object_types( ) ).
    ri_html->add( '</div>' ).

    register_deferred_script( render_scripts( ) ).

  ENDMETHOD.


  METHOD render_debug_info.

    DATA: lt_ver_tab     TYPE filetable,
          lv_rc          TYPE i,
          lv_gui_version TYPE string,
          ls_version     LIKE LINE OF lt_ver_tab,
          lv_devclass    TYPE devclass.

    cl_gui_frontend_services=>get_gui_version(
      CHANGING version_table = lt_ver_tab rc = lv_rc
      EXCEPTIONS OTHERS = 1 ).
    READ TABLE lt_ver_tab INTO ls_version INDEX 1. " gui release
    lv_gui_version = ls_version-filename.
    READ TABLE lt_ver_tab INTO ls_version INDEX 2. " gui sp
    lv_gui_version = |{ lv_gui_version }.{ ls_version-filename }|.
    READ TABLE lt_ver_tab INTO ls_version INDEX 3. " gui patch
    lv_gui_version = |{ lv_gui_version }.{ ls_version-filename }|.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( |<table>| ).
    ri_html->add( |<tr><td>abapGit version:</td><td>{ zif_abapgit_version=>gc_abap_version }</td></tr>| ).
    ri_html->add( |<tr><td>XML version:    </td><td>{ zif_abapgit_version=>gc_xml_version }</td></tr>| ).
    ri_html->add( |<tr><td>GUI version:    </td><td>{ lv_gui_version }</td></tr>| ).
    ri_html->add( |<tr><td>APACK version:  </td><td>{
                  zcl_abapgit_apack_migration=>c_apack_interface_version }</td></tr>| ).
    ri_html->add( |<tr><td>LCL_TIME:       </td><td>{ zcl_abapgit_time=>get_unix( ) }</td></tr>| ).
    ri_html->add( |<tr><td>SY time:        </td><td>{ sy-datum } { sy-uzeit } { sy-tzone }</td></tr>| ).
    ri_html->add( |</table>| ).
    ri_html->add( |<br>| ).

    lv_devclass = zcl_abapgit_services_abapgit=>is_installed( ).
    IF NOT lv_devclass IS INITIAL.
      ri_html->add( 'abapGit installed in package&nbsp;' ).
      ri_html->add( lv_devclass ).
    ELSE.
      ri_html->add( ' - To keep abapGit up-to-date (or also to contribute) you need to' ).
      ri_html->add( 'install it as a repository.' ).
    ENDIF.

    ri_html->add( |<br><br>| ).

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

    rv_html = li_html->a(
      iv_txt = 'Complete list of object types supported by abapGit'
      iv_act = 'https://docs.abapgit.org/ref-supported.html'
      iv_typ = zif_abapgit_html=>c_action_type-url ).

    rv_html = rv_html && |<br><br>Supported object types in <strong>this</strong> system:<br><br>|.

    rv_html = rv_html && |<table border="1px"><thead><tr>|.
    rv_html = rv_html && |<td>Object</td><td>Description</td><td>Class</td><td>Version</td><td>DDIC</td>|.
    rv_html = rv_html && |<td>Delete TADIR</td><td>Steps</td>|.
    rv_html = rv_html && |</tr></thead><tbody>|.

    LOOP AT lt_types INTO lv_type.
      lv_class = 'ZCL_ABAPGIT_OBJECT_' && lv_type.

      rv_html = rv_html && |<tr>|.

      rv_html = rv_html && |<td>{ lv_type }</td>|.

      READ TABLE lt_obj ASSIGNING <ls_obj> WITH KEY pgmid = 'R3TR' object = lv_type.
      IF sy-subrc = 0.
        rv_html = rv_html && |<td>{ <ls_obj>-text }</td>|.
      ELSE.
        rv_html = rv_html && |<td class="warning">>No description</td>|.
      ENDIF.


      TRY.
          ls_item-obj_type = lv_type.
          ls_item-obj_name = 'TEST'.

          CREATE OBJECT li_object TYPE (lv_class)
            EXPORTING
              is_item     = ls_item
              iv_language = sy-langu.

          rv_html = rv_html && |<td>{ get_jump_class( lv_class ) }</td>|.

        CATCH cx_sy_create_object_error.
          TRY. " 2nd step, try looking for plugins
              CREATE OBJECT li_object TYPE zcl_abapgit_objects_bridge
                EXPORTING
                  is_item = ls_item.
            CATCH cx_sy_create_object_error.
              rv_html = rv_html && |<td class="error" colspan="5">{ lv_class } - error instantiating class</td>|.
              CONTINUE.
          ENDTRY.

          rv_html = rv_html && |<td>{ get_jump_class( lv_class ) } (Plug-in)</td>|.
      ENDTRY.

      ls_metadata = li_object->get_metadata( ).

      rv_html = rv_html && |<td>{ ls_metadata-version }</td>|.
      rv_html = rv_html && |<td>{ ls_metadata-ddic }</td>|.
      rv_html = rv_html && |<td>{ ls_metadata-delete_tadir }</td>|.

      lt_steps = li_object->get_deserialize_steps( ).

      CLEAR lv_list.
      LOOP AT lt_steps INTO lv_step.
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
ENDCLASS.
