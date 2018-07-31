CLASS zcl_abapgit_object_ssfo DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PRIVATE SECTION.
    METHODS: fix_ids IMPORTING ii_xml_doc TYPE REF TO if_ixml_document.

ENDCLASS.

CLASS zcl_abapgit_object_ssfo IMPLEMENTATION.

  METHOD zif_abapgit_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.

  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE lastuser FROM stxfadm INTO rv_user
      WHERE formname = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.

  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-delete_tadir = abap_true.
  ENDMETHOD.

  METHOD zif_abapgit_object~exists.

    DATA: lv_formname TYPE stxfadm-formname.


    SELECT SINGLE formname FROM stxfadm INTO lv_formname
      WHERE formname = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.

  METHOD zif_abapgit_object~jump.

    DATA: lt_bdcdata  TYPE TABLE OF bdcdata,
          lv_formtype TYPE stxfadm-formtype.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPMSSFO'.
    <ls_bdcdata>-dynpro   = '0100'.
    <ls_bdcdata>-dynbegin = abap_true.

    SELECT SINGLE formtype FROM stxfadm INTO lv_formtype
           WHERE formname = ms_item-obj_name.

    IF lv_formtype = cssf_formtype_text.

      APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
      <ls_bdcdata>-fnam = 'RB_TX'.
      <ls_bdcdata>-fval = abap_true.

      APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
      <ls_bdcdata>-fnam = 'BDC_OKCODE'.
      <ls_bdcdata>-fval = '=RB'.

      APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
      <ls_bdcdata>-program  = 'SAPMSSFO'.
      <ls_bdcdata>-dynpro   = '0100'.
      <ls_bdcdata>-dynbegin = abap_true.

      APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
      <ls_bdcdata>-fnam = 'SSFSCREEN-TNAME'.
      <ls_bdcdata>-fval = ms_item-obj_name.

    ELSE.

      APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
      <ls_bdcdata>-fnam = 'SSFSCREEN-FNAME'.
      <ls_bdcdata>-fval = ms_item-obj_name.

    ENDIF.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=DISPLAY'.

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      STARTING NEW TASK 'GIT'
      EXPORTING
        tcode                 = 'SMARTFORMS'
        mode_val              = 'E'
      TABLES
        using_tab             = lt_bdcdata
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        resource_failure      = 3
        OTHERS                = 4
        ##fm_subrc_ok.                                                   "#EC CI_SUBRC

  ENDMETHOD.

  METHOD zif_abapgit_object~delete.

    DATA: lv_formname TYPE tdsfname.


    lv_formname = ms_item-obj_name.

    CALL FUNCTION 'FB_DELETE_FORM'
      EXPORTING
        i_formname            = lv_formname
        i_with_dialog         = abap_false
        i_with_confirm_dialog = abap_false
      EXCEPTIONS
        no_name               = 1
        no_form               = 2
        form_locked           = 3
        no_access_permission  = 4
        illegal_language      = 5
        illegal_formtype      = 6
        OTHERS                = 7.
    IF sy-subrc <> 0 AND sy-subrc <> 2.
      zcx_abapgit_exception=>raise( 'Error from FB_DELETE_FORM' ).
    ENDIF.

  ENDMETHOD.

  METHOD zif_abapgit_object~serialize.
* see function module FB_DOWNLOAD_FORM

    DATA: lo_sf       TYPE REF TO cl_ssf_fb_smart_form,
          lv_name     TYPE string,
          li_node     TYPE REF TO if_ixml_node,
          li_element  TYPE REF TO if_ixml_element,
          li_iterator TYPE REF TO if_ixml_node_iterator,
          lv_formname TYPE tdsfname,
          li_ixml     TYPE REF TO if_ixml,
          li_xml_doc  TYPE REF TO if_ixml_document.


    li_ixml = cl_ixml=>create( ).
    li_xml_doc = li_ixml->create_document( ).

    CREATE OBJECT lo_sf.
    lv_formname = ms_item-obj_name. " convert type
    TRY.
        lo_sf->load( im_formname = lv_formname
                     im_language = '' ).
      CATCH cx_ssf_fb.
* the smartform is not present in system, or other error occured
        RETURN.
    ENDTRY.

    lo_sf->xml_download( EXPORTING parent   = li_xml_doc
                         CHANGING  document = li_xml_doc ).

    li_iterator = li_xml_doc->create_iterator( ).
    li_node = li_iterator->get_next( ).
    WHILE NOT li_node IS INITIAL.

      lv_name = li_node->get_name( ).
      IF lv_name = 'DEVCLASS'
          OR lv_name = 'LASTDATE'
          OR lv_name = 'LASTTIME'.
        li_node->set_value( '' ).
      ENDIF.
      IF lv_name = 'FIRSTUSER'
          OR lv_name = 'LASTUSER'.
        li_node->set_value( 'DUMMY' ).
      ENDIF.

      li_node = li_iterator->get_next( ).
    ENDWHILE.

    fix_ids( li_xml_doc ).

    li_element = li_xml_doc->get_root_element( ).
    li_element->set_attribute(
      name      = 'sf'
      namespace = 'xmlns'
      value     = 'urn:sap-com:SmartForms:2000:internal-structure' ). "#EC NOTEXT
    li_element->set_attribute(
      name  = 'xmlns'
      value = 'urn:sap-com:sdixml-ifr:2000' ).              "#EC NOTEXT

    io_xml->set_raw( li_xml_doc->get_root_element( ) ).

  ENDMETHOD.

  METHOD fix_ids.
* makes sure ID and IDREF values are the same values for each serialization run
* the standard code has a counter that keeps increasing values

    DATA: lv_name     TYPE string,
          li_idref    TYPE REF TO if_ixml_node,
          li_node     TYPE REF TO if_ixml_node,
          li_attr     TYPE REF TO if_ixml_named_node_map,
          li_iterator TYPE REF TO if_ixml_node_iterator,
          lt_idref    TYPE STANDARD TABLE OF string WITH DEFAULT KEY.


    li_iterator = ii_xml_doc->create_iterator( ).
    li_node = li_iterator->get_next( ).
    WHILE NOT li_node IS INITIAL.
      lv_name = li_node->get_name( ).
      IF lv_name = 'NODE' OR lv_name = 'WINDOW'.
        li_idref = li_node->get_attributes( )->get_named_item( 'IDREF' ).
        IF li_idref IS BOUND.
          APPEND li_idref->get_value( ) TO lt_idref.
          li_idref->set_value( |{ sy-tabix }| ).
        ENDIF.
      ENDIF.
      li_node = li_iterator->get_next( ).
    ENDWHILE.

    li_iterator = ii_xml_doc->create_iterator( ).
    li_node = li_iterator->get_next( ).
    WHILE NOT li_node IS INITIAL.
      lv_name = li_node->get_name( ).
      IF lv_name = 'NODE' OR lv_name = 'WINDOW'.
        li_idref = li_node->get_attributes( )->get_named_item( 'ID' ).
        IF li_idref IS BOUND.
          lv_name = li_idref->get_value( ).
          READ TABLE lt_idref WITH KEY table_line = lv_name TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            li_idref->set_value( |{ sy-tabix }| ).
          ELSE.
            li_attr = li_node->get_attributes( ).
            li_attr->remove_named_item( 'ID' ).
          ENDIF.
        ENDIF.
      ENDIF.
      li_node = li_iterator->get_next( ).
    ENDWHILE.

  ENDMETHOD.

  METHOD zif_abapgit_object~deserialize.
* see function module FB_UPLOAD_FORM

    DATA: li_node     TYPE REF TO if_ixml_node,
          lv_formname TYPE tdsfname,
          lv_name     TYPE string,
          li_iterator TYPE REF TO if_ixml_node_iterator,
          lo_sf       TYPE REF TO cl_ssf_fb_smart_form,
          lo_res      TYPE REF TO cl_ssf_fb_smart_form,
          lx_error    TYPE REF TO cx_ssf_fb,
          lv_text     TYPE string.


    CREATE OBJECT lo_sf.

* set "created by" and "changed by" to current user
    li_iterator = io_xml->get_raw( )->get_root_element( )->create_iterator( ).
    li_node = li_iterator->get_next( ).
    WHILE NOT li_node IS INITIAL.
      lv_name = li_node->get_name( ).
      CASE lv_name.
        WHEN 'LASTDATE'.
          li_node->set_value(
            sy-datum(4) && '-' && sy-datum+4(2) && '-' && sy-datum+6(2) ).
        WHEN 'LASTTIME'.
          li_node->set_value(
            sy-uzeit(2) && ':' && sy-uzeit+2(2) && ':' && sy-uzeit+4(2) ).
        WHEN 'FIRSTUSER' OR 'LASTUSER'.
          li_node->set_value( sy-uname && '' ).
      ENDCASE.

      li_node = li_iterator->get_next( ).
    ENDWHILE.

    tadir_insert( iv_package ).

    lv_formname = ms_item-obj_name.

    TRY.
        lo_sf->enqueue( suppress_corr_check = space
                        master_language     = mv_language
                        mode                = 'INSERT'
                        formname            = lv_formname ).

        lo_sf->xml_upload( EXPORTING dom      = io_xml->get_raw( )->get_root_element( )
                                     formname = lv_formname
                                     language = mv_language
                           CHANGING  sform    = lo_res ).

        lo_res->store( im_formname = lo_res->header-formname
                       im_language = mv_language
                       im_active   = abap_true ).

        lo_sf->dequeue( lv_formname ).

      CATCH cx_ssf_fb INTO lx_error.
        lv_text = lx_error->get_text( ).
        zcx_abapgit_exception=>raise( |{ ms_item-obj_type } { ms_item-obj_name }: { lv_text } | ).
    ENDTRY.

  ENDMETHOD.

  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
  ENDMETHOD.

  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'E_SMFORM'
                                            iv_argument    = |{ ms_item-obj_name }| ).

  ENDMETHOD.

ENDCLASS.
