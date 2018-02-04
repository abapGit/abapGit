CLASS zcl_abapgit_object_wapa DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_page,
             attributes     TYPE o2pagattr,
             event_handlers TYPE o2pagevh_tabletype,
             parameters     TYPE o2pagpar_tabletype,
             types          TYPE rswsourcet,
           END OF ty_page.

    TYPES: ty_pages_tt TYPE STANDARD TABLE OF ty_page WITH DEFAULT KEY.

    CONSTANTS: c_active TYPE so2_version VALUE 'A'.

    METHODS:
      get_page_content
        IMPORTING io_page           TYPE REF TO cl_o2_api_pages
        RETURNING VALUE(rv_content) TYPE xstring
        RAISING   zcx_abapgit_exception,
      to_page_content
        IMPORTING iv_content        TYPE xstring
        RETURNING VALUE(rt_content) TYPE o2pageline_table,
      read_page
        IMPORTING is_page        TYPE o2pagattr
        RETURNING VALUE(rs_page) TYPE ty_page
        RAISING   zcx_abapgit_exception.

ENDCLASS.

CLASS zcl_abapgit_object_wapa IMPLEMENTATION.

  METHOD zif_abapgit_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.  "zif_abapgit_object~has_changed_since

  METHOD zif_abapgit_object~changed_by.

    DATA: lv_name   TYPE o2applname,
          lt_pages  TYPE STANDARD TABLE OF o2pagdir WITH DEFAULT KEY,
          ls_latest LIKE LINE OF lt_pages.


    lv_name = ms_item-obj_name.

    SELECT * FROM o2pagdir INTO TABLE lt_pages WHERE applname = lv_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
      RETURN.
    ENDIF.

    SORT lt_pages BY changedon DESCENDING changetime DESCENDING.

    READ TABLE lt_pages INDEX 1 INTO ls_latest.
    ASSERT sy-subrc = 0.

    rv_user = ls_latest-changedby.

  ENDMETHOD.

  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "zif_abapgit_object~get_metadata

  METHOD zif_abapgit_object~exists.

    DATA: lv_name TYPE o2applname.


    lv_name = ms_item-obj_name.

    cl_o2_api_application=>load(
      EXPORTING
        p_application_name  = lv_name
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3 ).
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "zif_abapgit_object~exists

  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = ms_item-obj_type
        in_new_window = abap_true.

  ENDMETHOD.                    "jump

  METHOD zif_abapgit_object~delete.

    DATA: lv_name        TYPE o2applname,
          lo_bsp         TYPE REF TO cl_o2_api_application,
          ls_pagekey     TYPE o2pagkey,
          lv_object      TYPE seu_objkey,
          lt_pages       TYPE o2pagelist,
          lt_local_mimes TYPE o2pagename_table.

    FIELD-SYMBOLS: <ls_page>       LIKE LINE OF lt_pages,
                   <ls_local_mime> TYPE o2pagename.

    lv_name = ms_item-obj_name.

    cl_o2_api_application=>load(
      EXPORTING
        p_application_name  = lv_name
      IMPORTING
        p_application       = lo_bsp
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3 ).
    ASSERT sy-subrc = 0.

    lo_bsp->set_changeable(
      p_changeable           = abap_true
      p_complete_application = abap_true ).

    cl_o2_api_pages=>get_all_pages(
      EXPORTING
        p_applname = lv_name
        p_version  = c_active
      IMPORTING
        p_pages    = lt_pages ).

    LOOP AT lt_pages ASSIGNING <ls_page>.
      CLEAR ls_pagekey.
      ls_pagekey-applname = lv_name.
      ls_pagekey-pagekey  = <ls_page>-pagekey.

      cl_o2_page=>delete_page_for_application(
        EXPORTING
          p_pagekey           = ls_pagekey
        EXCEPTIONS
          object_not_existing = 1
          error_occured       = 2 ).
      ASSERT sy-subrc = 0.
    ENDLOOP.

    lo_bsp->get_local_mimes(
      IMPORTING
        p_local_mimes  = lt_local_mimes
      EXCEPTIONS
        object_invalid = 1
        object_deleted = 2
        error_occured  = 3
        OTHERS         = 4 ).

    LOOP AT lt_local_mimes ASSIGNING <ls_local_mime>.
      CLEAR ls_pagekey.
      ls_pagekey-applname = <ls_local_mime>-applname.
      ls_pagekey-pagekey  = <ls_local_mime>-pagekey.

      cl_o2_page=>delete_page_for_application(
        EXPORTING
          p_pagekey           = ls_pagekey
        EXCEPTIONS
          object_not_existing = 1
          error_occured       = 2 ).
      ASSERT sy-subrc = 0.
    ENDLOOP.

    lo_bsp->delete(
      EXCEPTIONS
        object_not_empty      = 1
        object_not_changeable = 2
        object_invalid        = 3
        action_cancelled      = 4
        permission_failure    = 5
        error_occured         = 6
        OTHERS                = 7 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |WAPA - error from delete: { sy-subrc }| ).
    ENDIF.

* release lock
    lv_object = lv_name.
    cl_o2_api_application=>call_access_permission(
      p_mode                 = 'FREE'
      p_object               = lv_object
      p_complete_application = abap_true ).

  ENDMETHOD.                    "delete

  METHOD zif_abapgit_object~deserialize.

    DATA: lo_bsp        TYPE REF TO cl_o2_api_application,
          ls_attributes TYPE o2applattr,
          lt_nodes      TYPE o2applnode_table,
          lt_navgraph   TYPE o2applgrap_table,
          lv_objkey     TYPE seu_objkey,
          lv_obj_name   TYPE string,
          ls_item       LIKE ms_item,
          lv_extra      TYPE string,
          lv_content    TYPE xstring,
          lv_ext        TYPE string,
          lo_page       TYPE REF TO cl_o2_api_pages,
          lt_pages_info TYPE ty_pages_tt.

    FIELD-SYMBOLS: <ls_page> LIKE LINE OF lt_pages_info.


    io_xml->read( EXPORTING iv_name = 'ATTRIBUTES'
                  CHANGING cg_data = ls_attributes ).
    io_xml->read( EXPORTING iv_name = 'NAVGRAPH'
                  CHANGING cg_data = lt_navgraph ).
    io_xml->read( EXPORTING iv_name = 'PAGES'
                  CHANGING cg_data = lt_pages_info ).

    ls_attributes-devclass = iv_package.

    IF me->zif_abapgit_object~exists( ) = abap_true.
      me->zif_abapgit_object~delete( ).
    ENDIF.

    cl_o2_api_application=>create_new(
      EXPORTING
        p_application_data      = ls_attributes
        p_nodes                 = lt_nodes
        p_navgraph              = lt_navgraph
      IMPORTING
        p_application           = lo_bsp
      EXCEPTIONS
        object_already_existing = 1
        object_just_created     = 2
        not_authorized          = 3
        undefined_name          = 4
        author_not_existing     = 5
        action_cancelled        = 6
        error_occured           = 7
        invalid_parameter       = 8 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |WAPA - error from create_new: { sy-subrc }| ).
    ENDIF.

    lo_bsp->save( ).

    lo_bsp->set_changeable(
      p_changeable           = abap_false
      p_complete_application = abap_true ).

    ls_item-obj_type = 'WAPD'.
    ls_item-obj_name = ms_item-obj_name.
    zcl_abapgit_objects_activation=>add_item( ls_item ).

    lv_objkey = ls_item-obj_name.
* todo, hmm, the WAPD is not added to the worklist during activation
    cl_o2_api_application=>activate( lv_objkey ).

    LOOP AT lt_pages_info ASSIGNING <ls_page>.
      cl_o2_api_pages=>create_new_page(
        EXPORTING
          p_pageattrs = <ls_page>-attributes
        IMPORTING
          p_page      = lo_page ).

      IF <ls_page>-attributes-pagetype <> so2_controller.

        SPLIT <ls_page>-attributes-pagename AT '.' INTO lv_extra lv_ext.
        REPLACE ALL OCCURRENCES OF '/' IN lv_extra WITH '_-'.
        REPLACE ALL OCCURRENCES OF '/' IN lv_ext WITH '_-'.
        lv_content = mo_files->read_raw( iv_extra = lv_extra
                                         iv_ext   = lv_ext ).
        lo_page->set_page( to_page_content( lv_content ) ).

        lo_page->set_event_handlers( <ls_page>-event_handlers ).
        lo_page->set_parameters( <ls_page>-parameters ).
        lo_page->set_type_source( <ls_page>-types ).

      ENDIF.

      lo_page->save( p_with_all_texts = abap_true ).

      lv_obj_name = cl_wb_object_type=>get_concatenated_key_from_id(
        p_key_component1 = <ls_page>-attributes-applname
        p_key_component2 = <ls_page>-attributes-pagekey
        p_external_id    = 'WG ' ).

      zcl_abapgit_objects_activation=>add( iv_type = 'WAPP'
                                           iv_name = lv_obj_name ).

    ENDLOOP.

  ENDMETHOD.                    "deserialize

  METHOD zif_abapgit_object~serialize.

    DATA: lv_name       TYPE o2applname,
          ls_attributes TYPE o2applattr,
          lt_navgraph   TYPE o2applgrap_table,
          lt_pages      TYPE o2pagelist,
          lt_pages_info TYPE ty_pages_tt,
          lo_bsp        TYPE REF TO cl_o2_api_application.

    FIELD-SYMBOLS: <ls_page> LIKE LINE OF lt_pages.


    lv_name = ms_item-obj_name.

    cl_o2_api_application=>load(
      EXPORTING
        p_application_name  = lv_name
      IMPORTING
        p_application       = lo_bsp
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lo_bsp->get_attributes(
      EXPORTING
        p_version    = c_active
      IMPORTING
        p_attributes = ls_attributes ).

    CLEAR: ls_attributes-author,
           ls_attributes-createdon,
           ls_attributes-changedby,
           ls_attributes-changedon,
           ls_attributes-devclass.

    io_xml->add( iv_name = 'ATTRIBUTES'
                 ig_data = ls_attributes ).

    lo_bsp->get_navgraph(
      EXPORTING
        p_version  = c_active
      IMPORTING
        p_navgraph = lt_navgraph ).

    io_xml->add( iv_name = 'NAVGRAPH'
                 ig_data = lt_navgraph ).

    cl_o2_api_pages=>get_all_pages(
      EXPORTING
        p_applname = lv_name
        p_version  = c_active
      IMPORTING
        p_pages    = lt_pages ).

    LOOP AT lt_pages ASSIGNING <ls_page>.
      APPEND read_page( <ls_page> ) TO lt_pages_info.
    ENDLOOP.

    io_xml->add( iv_name = 'PAGES'
                 ig_data = lt_pages_info ).

  ENDMETHOD.                    "serialize

  METHOD read_page.

    DATA: lv_name    TYPE o2applname,
          ls_pagekey TYPE o2pagkey,
          lv_content TYPE xstring,
          lv_extra   TYPE string,
          lv_ext     TYPE string,
          lo_page    TYPE REF TO cl_o2_api_pages.


    lv_name = ms_item-obj_name.

    ls_pagekey-applname = lv_name.
    ls_pagekey-pagekey = is_page-pagekey.

    cl_o2_api_pages=>load(
      EXPORTING
        p_pagekey = ls_pagekey
      IMPORTING
        p_page    = lo_page ).

    lo_page->get_attrs(
      IMPORTING
        p_attrs = rs_page-attributes ).

    IF rs_page-attributes-pagetype <> so2_controller.

      lo_page->get_event_handlers(
        IMPORTING
          p_ev_handler = rs_page-event_handlers
        EXCEPTIONS
          page_deleted = 1
          invalid_call = 2 ).
      ASSERT sy-subrc = 0.

      lo_page->get_parameters(
        IMPORTING
          p_parameters = rs_page-parameters
        EXCEPTIONS
          page_deleted = 1
          invalid_call = 2
          OTHERS       = 3 ).
      ASSERT sy-subrc = 0.

      lo_page->get_type_source(
        IMPORTING
          p_source     = rs_page-types
        EXCEPTIONS
          page_deleted = 1
          invalid_call = 2
          OTHERS       = 3 ).
      ASSERT sy-subrc = 0.

      lv_content = get_page_content( lo_page ).
      SPLIT is_page-pagename AT '.' INTO lv_extra lv_ext.
      REPLACE ALL OCCURRENCES OF '/' IN lv_ext WITH '_-'.
      REPLACE ALL OCCURRENCES OF '/' IN lv_extra WITH '_-'.
      mo_files->add_raw(
        iv_extra = lv_extra
        iv_ext   = lv_ext
        iv_data  = lv_content ).

      CLEAR: rs_page-attributes-implclass.

    ENDIF.

    CLEAR: rs_page-attributes-author,
           rs_page-attributes-createdon,
           rs_page-attributes-changedby,
           rs_page-attributes-changedon,
           rs_page-attributes-changetime,
           rs_page-attributes-gendate,
           rs_page-attributes-gentime,
           rs_page-attributes-devclass.

  ENDMETHOD.

  METHOD to_page_content.

    DATA: lv_string TYPE string.


    lv_string = zcl_abapgit_convert=>xstring_to_string_utf8( iv_content ).

    SPLIT lv_string AT zif_abapgit_definitions=>gc_newline INTO TABLE rt_content.

  ENDMETHOD.

  METHOD get_page_content.

    DATA: lt_content TYPE o2pageline_table,
          lv_string  TYPE string.

    io_page->get_page(
      IMPORTING
        p_content = lt_content
      EXCEPTIONS
        invalid_call = 1
        page_deleted = 2
        OTHERS       = 3 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |WAPA - error from get_page_content| ).
    ENDIF.

    CONCATENATE LINES OF lt_content INTO lv_string SEPARATED BY zif_abapgit_definitions=>gc_newline RESPECTING BLANKS.

    rv_content = zcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).

  ENDMETHOD.

  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
  ENDMETHOD.

ENDCLASS.                    "zcl_abapgit_object_tran IMPLEMENTATION
