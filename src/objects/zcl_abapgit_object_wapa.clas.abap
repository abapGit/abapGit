CLASS zcl_abapgit_object_wapa DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
  PROTECTED SECTION.
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
        RETURNING VALUE(rt_content) TYPE o2pageline_table
        RAISING   zcx_abapgit_exception,
      read_page
        IMPORTING is_page         TYPE o2pagattr
                  iv_no_files_add TYPE abap_bool OPTIONAL
        RETURNING VALUE(rs_page)  TYPE ty_page
        RAISING   zcx_abapgit_exception,
      create_new_application
        IMPORTING is_attributes TYPE o2applattr
                  it_nodes      TYPE o2applnode_table
                  it_navgraph   TYPE o2applgrap_table
        RETURNING VALUE(ro_bsp) TYPE REF TO cl_o2_api_application
        RAISING   zcx_abapgit_exception,
      create_new_page
        IMPORTING
          is_page_attributes TYPE o2pagattr
        RETURNING
          VALUE(ro_page)     TYPE REF TO cl_o2_api_pages
        RAISING
          zcx_abapgit_exception,
      delete_superfluous_pages
        IMPORTING
          it_local_pages  TYPE o2pagelist
          it_remote_pages TYPE ty_pages_tt
        RAISING
          zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_object_wapa IMPLEMENTATION.


  METHOD create_new_application.

    DATA: ls_item   LIKE ms_item,
          lv_objkey TYPE seu_objkey.

    cl_o2_api_application=>create_new(
      EXPORTING
        p_application_data      = is_attributes
        p_nodes                 = it_nodes
        p_navgraph              = it_navgraph
      IMPORTING
        p_application           = ro_bsp
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

    ro_bsp->save( ).

    ro_bsp->set_changeable(
      p_changeable           = abap_false
      p_complete_application = abap_true ).

    ls_item-obj_type = 'WAPD'.
    ls_item-obj_name = ms_item-obj_name.
    zcl_abapgit_objects_activation=>add_item( ls_item ).

    lv_objkey = ls_item-obj_name.
* todo, hmm, the WAPD is not added to the worklist during activation
    cl_o2_api_application=>activate( lv_objkey ).


  ENDMETHOD.


  METHOD create_new_page.

    cl_o2_api_pages=>create_new_page(
      EXPORTING
        p_pageattrs = is_page_attributes
      IMPORTING
        p_page      = ro_page
      EXCEPTIONS
        object_already_exists = 1
        invalid_name          = 2
        error_occured         = 3
        o2appl_not_existing   = 4
        OTHERS                = 5 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error { sy-subrc } from CL_O2_API_PAGES=>CREATE_NEW_PAGE| ).
    ENDIF.

  ENDMETHOD.


  METHOD delete_superfluous_pages.

    DATA: ls_pagekey TYPE o2pagkey.
    FIELD-SYMBOLS: <ls_local_page> LIKE LINE OF it_local_pages.

    " delete local pages which doesn't exists remotely
    LOOP AT it_local_pages ASSIGNING <ls_local_page>.

      READ TABLE it_remote_pages WITH KEY attributes-pagekey = <ls_local_page>-pagekey
                               TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        " page exists locally but not remotely -> delete

        ls_pagekey-applname = <ls_local_page>-applname.
        ls_pagekey-pagekey = <ls_local_page>-pagekey.

        cl_o2_page=>delete_page_for_application(
          EXPORTING
            p_pagekey           = ls_pagekey
          EXCEPTIONS
            object_not_existing = 1
            error_occured       = 2 ).

        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( |Error { sy-subrc } from CL_O2_PAGE=>DELETE_PAGE_FOR_APPLICATION| ).
        ENDIF.

      ENDIF.

    ENDLOOP.

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

    CONCATENATE LINES OF lt_content INTO lv_string SEPARATED BY cl_abap_char_utilities=>newline RESPECTING BLANKS.

    rv_content = zcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).

  ENDMETHOD.


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

    lo_page->get_attrs( IMPORTING p_attrs = rs_page-attributes ).

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
      IF iv_no_files_add = abap_false.
        zif_abapgit_object~mo_files->add_raw(
          iv_extra = lv_extra
          iv_ext   = lv_ext
          iv_data  = lv_content ).
      ENDIF.

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

    SPLIT lv_string AT cl_abap_char_utilities=>newline INTO TABLE rt_content.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA: lv_name   TYPE o2applname,
          lt_pages  TYPE STANDARD TABLE OF o2pagdir WITH DEFAULT KEY,
          ls_latest LIKE LINE OF lt_pages.


    lv_name = ms_item-obj_name.

    SELECT * FROM o2pagdir INTO TABLE lt_pages WHERE applname = lv_name
      ORDER BY changedon DESCENDING changetime DESCENDING.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
      RETURN.
    ENDIF.

    READ TABLE lt_pages INDEX 1 INTO ls_latest.
    ASSERT sy-subrc = 0.

    rv_user = ls_latest-changedby.

  ENDMETHOD.


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

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lo_bsp            TYPE REF TO cl_o2_api_application,
          ls_attributes     TYPE o2applattr,
          lt_nodes          TYPE o2applnode_table,
          lt_navgraph       TYPE o2applgrap_table,
          lv_obj_name       TYPE string,
          lv_extra          TYPE string,
          lv_ext            TYPE string,
          lo_page           TYPE REF TO cl_o2_api_pages,
          lt_pages_info     TYPE ty_pages_tt,
          ls_pagekey        TYPE o2pagkey,
          ls_local_page     TYPE ty_page,
          lt_remote_content TYPE o2pageline_table,
          lt_local_content  TYPE o2pageline_table,
          lt_local_pages    TYPE o2pagelist.

    FIELD-SYMBOLS: <ls_remote_page> LIKE LINE OF lt_pages_info.

    io_xml->read( EXPORTING iv_name = 'ATTRIBUTES'
                  CHANGING cg_data = ls_attributes ).
    io_xml->read( EXPORTING iv_name = 'NAVGRAPH'
                  CHANGING cg_data = lt_navgraph ).
    io_xml->read( EXPORTING iv_name = 'PAGES'
                  CHANGING cg_data = lt_pages_info ).

    ls_attributes-devclass = iv_package.

    cl_o2_api_application=>load(
      EXPORTING
        p_application_name  = ls_attributes-applname    " Application Name
      IMPORTING
        p_application       = lo_bsp    " Instance Created
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3
        OTHERS              = 4 ).

    CASE sy-subrc.
      WHEN 0.

        cl_o2_api_pages=>get_all_pages(
          EXPORTING
            p_applname = ls_attributes-applname
            p_version  = c_active
          IMPORTING
            p_pages    = lt_local_pages ).

      WHEN 1.

        lo_bsp = create_new_application( is_attributes = ls_attributes
                                         it_nodes      = lt_nodes
                                         it_navgraph   = lt_navgraph ).

      WHEN OTHERS.

        zcx_abapgit_exception=>raise( |Error { sy-subrc } from CL_O2_API_APPLICATION=>LOAD| ).

    ENDCASE.

    LOOP AT lt_pages_info ASSIGNING <ls_remote_page>.

      ls_pagekey-applname = <ls_remote_page>-attributes-applname.
      ls_pagekey-pagekey = <ls_remote_page>-attributes-pagekey.

      cl_o2_api_pages=>load(
        EXPORTING
          p_pagekey             = ls_pagekey
        IMPORTING
          p_page                = lo_page
        EXCEPTIONS
          object_not_existing   = 1
          version_not_existing  = 2
          OTHERS                = 3 ).

      CASE sy-subrc.
        WHEN 0.

          ls_local_page = read_page( is_page = <ls_remote_page>-attributes
                                     iv_no_files_add = abap_true ).

        WHEN 1.

          lo_page = create_new_page( <ls_remote_page>-attributes ).

        WHEN 2.

          " Do nothing...

        WHEN OTHERS.

          zcx_abapgit_exception=>raise( |Error { sy-subrc } from CL_O2_API_PAGES=>LOAD| ).

      ENDCASE.

      SPLIT <ls_remote_page>-attributes-pagename AT '.' INTO lv_extra lv_ext.
      REPLACE ALL OCCURRENCES OF '/' IN lv_extra WITH '_-'.
      REPLACE ALL OCCURRENCES OF '/' IN lv_ext WITH '_-'.

      lt_remote_content = to_page_content( zif_abapgit_object~mo_files->read_raw( iv_extra = lv_extra
                                                                                  iv_ext   = lv_ext ) ).
      lt_local_content = to_page_content( get_page_content( lo_page ) ).

      IF ls_local_page = <ls_remote_page> AND lt_local_content = lt_remote_content.
        " no changes -> nothing to do
        CONTINUE.
      ENDIF.

      IF <ls_remote_page>-attributes-pagetype <> so2_controller.

        lo_page->set_page( lt_remote_content ).

        lo_page->set_event_handlers( <ls_remote_page>-event_handlers ).
        lo_page->set_parameters( <ls_remote_page>-parameters ).
        lo_page->set_type_source( <ls_remote_page>-types ).

      ENDIF.

      lo_page->save( p_with_all_texts = abap_true ).

      lv_obj_name = cl_wb_object_type=>get_concatenated_key_from_id(
        p_key_component1 = <ls_remote_page>-attributes-applname
        p_key_component2 = <ls_remote_page>-attributes-pagekey
        p_external_id    = 'WG ' ).

      zcl_abapgit_objects_activation=>add( iv_type = 'WAPP'
                                           iv_name = lv_obj_name ).

    ENDLOOP.

    delete_superfluous_pages( it_local_pages  = lt_local_pages
                              it_remote_pages = lt_pages_info ).

    zcl_abapgit_sotr_handler=>create_sotr(
      iv_package = iv_package
      io_xml     = io_xml ).

  ENDMETHOD.


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
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


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

    zcl_abapgit_sotr_handler=>read_sotr(
      iv_pgmid    = 'LIMU'
      iv_object   = 'WAPP'
      iv_obj_name = ms_item-obj_name
      io_xml      = io_xml ).

  ENDMETHOD.
ENDCLASS.
