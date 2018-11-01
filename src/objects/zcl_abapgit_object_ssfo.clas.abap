CLASS zcl_abapgit_object_ssfo DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PRIVATE SECTION.
    TYPES: ty_string_range TYPE RANGE OF string.

    CLASS-DATA: range_node_codes TYPE ty_string_range.
    CONSTANTS: attrib_abapgit_leadig_spaces TYPE string VALUE 'abapgit-leadig-spaces' ##NO_TEXT.

    METHODS fix_ids IMPORTING ii_xml_doc TYPE REF TO if_ixml_document.
    METHODS set_attribute_leading_spaces IMPORTING i_name                TYPE string
                                                   i_node                TYPE REF TO if_ixml_node
                                         CHANGING  c_within_code_section TYPE abap_bool.
    METHODS handle_attrib_leading_spaces IMPORTING i_name                TYPE string
                                                   i_node                TYPE REF TO if_ixml_node
                                         CHANGING  c_within_code_section TYPE abap_bool.
    METHODS get_range_node_codes RETURNING VALUE(e_range_node_codes) TYPE ty_string_range.
    METHODS code_item_section_handling IMPORTING i_name                TYPE string
                                                 i_node                TYPE REF TO if_ixml_node
                                       EXPORTING e_code_item_element   TYPE REF TO if_ixml_element
                                       CHANGING  c_within_code_section TYPE abap_bool
                                       RAISING   zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_object_ssfo IMPLEMENTATION.


  METHOD code_item_section_handling.
    CONSTANTS: node_item TYPE string VALUE 'item' ##NO_TEXT.
    CONSTANTS: node_text TYPE string VALUE '#text' ##NO_TEXT.

    IF i_name IN get_range_node_codes( ).
      c_within_code_section = abap_true.
    ENDIF.

    IF c_within_code_section = abap_true.
      IF i_name = node_item.
        TRY.
            e_code_item_element ?= i_node.
            RETURN.
          CATCH cx_sy_move_cast_error ##no_handler.
        ENDTRY.

      ELSEIF i_name NOT IN get_range_node_codes( ) AND
             i_name <> node_text.
        c_within_code_section = abap_false.
      ENDIF.
    ENDIF.

    RAISE EXCEPTION TYPE zcx_abapgit_exception.

  ENDMETHOD.


  METHOD fix_ids.

    " makes sure ID and IDREF values are the same values for each serialization run
    " the standard code has a counter that keeps increasing values.
    "
    " It is important that IDs and IDREFs which are the same before the fix
    " are also the same after the fix.

    TYPES:
      BEGIN OF ty_id_mapping,
        old TYPE string,
        new TYPE string,
      END OF ty_id_mapping,
      tty_id_mapping TYPE HASHED TABLE OF ty_id_mapping
                          WITH UNIQUE KEY old.

    DATA: lv_name       TYPE string,
          li_idref      TYPE REF TO if_ixml_node,
          li_node       TYPE REF TO if_ixml_node,
          li_attr       TYPE REF TO if_ixml_named_node_map,
          li_iterator   TYPE REF TO if_ixml_node_iterator,
          lt_id_mapping TYPE tty_id_mapping,
          ls_id_mapping LIKE LINE OF lt_id_mapping.

    li_iterator = ii_xml_doc->create_iterator( ).
    li_node = li_iterator->get_next( ).
    WHILE NOT li_node IS INITIAL.
      lv_name = li_node->get_name( ).
      IF lv_name = 'NODE' OR lv_name = 'WINDOW'.
        li_idref = li_node->get_attributes( )->get_named_item( 'IDREF' ).
        IF li_idref IS BOUND.

          ls_id_mapping-old = li_idref->get_value( ).
          READ TABLE lt_id_mapping WITH KEY old = ls_id_mapping-old
                                   INTO ls_id_mapping.
          IF sy-subrc <> 0.
            lv_name = lines( lt_id_mapping ) + 1.
            ls_id_mapping-new = condense( lv_name ).
            INSERT ls_id_mapping INTO TABLE lt_id_mapping.
          ENDIF.

          li_idref->set_value( |{ ls_id_mapping-new }| ).
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

          ls_id_mapping-old = li_idref->get_value( ).
          READ TABLE lt_id_mapping WITH KEY old = ls_id_mapping-old
                                   INTO ls_id_mapping.
          IF sy-subrc = 0.
            li_idref->set_value( |{ ls_id_mapping-new }| ).
          ELSE.
            li_attr = li_node->get_attributes( ).
            li_attr->remove_named_item( 'ID' ).
          ENDIF.

        ENDIF.
      ENDIF.
      li_node = li_iterator->get_next( ).
    ENDWHILE.

  ENDMETHOD.


  METHOD get_range_node_codes.

    DATA: range_node_code TYPE LINE OF ty_string_range.

    IF me->range_node_codes IS INITIAL.
      range_node_code-sign   = 'I'.
      range_node_code-option = 'EQ'.
      range_node_code-low    = 'CODE'.
      INSERT range_node_code INTO TABLE me->range_node_codes.
      range_node_code-low    = 'GTYPES'.
      INSERT range_node_code INTO TABLE me->range_node_codes.
      range_node_code-low    = 'GCODING'.
      INSERT range_node_code INTO TABLE me->range_node_codes.
      range_node_code-low    = 'FCODING'.
      INSERT range_node_code INTO TABLE me->range_node_codes.
    ENDIF.

    e_range_node_codes = me->range_node_codes.

  ENDMETHOD.


  METHOD handle_attrib_leading_spaces.

    DATA element        TYPE REF TO if_ixml_element.
    DATA leading_spaces TYPE string.
    DATA coding_line    TYPE string.

    TRY.
        code_item_section_handling( EXPORTING i_name                = i_name
                                              i_node                = i_node
                                    IMPORTING e_code_item_element   = element
                                    CHANGING  c_within_code_section = c_within_code_section ).

        leading_spaces = element->get_attribute_ns( name = zcl_abapgit_object_ssfo=>attrib_abapgit_leadig_spaces ).

        coding_line = element->get_value( ).
        SHIFT coding_line RIGHT BY leading_spaces PLACES.
        element->set_value( coding_line ).
      CATCH zcx_abapgit_exception ##no_handler.
    ENDTRY.

  ENDMETHOD.


  METHOD set_attribute_leading_spaces.

    DATA: element             TYPE REF TO if_ixml_element.
    DATA: code_line           TYPE string.
    DATA: offset              TYPE i.

    TRY.
        code_item_section_handling( EXPORTING i_name                = i_name
                                              i_node                = i_node
                                    IMPORTING e_code_item_element   = element
                                    CHANGING  c_within_code_section = c_within_code_section ).

        code_line = i_node->get_value( ).
        "find 1st non space char
        FIND FIRST OCCURRENCE OF REGEX '\S' IN code_line MATCH OFFSET offset.
        IF sy-subrc = 0 AND offset > 0.
          TRY.
              element ?= i_node.
              element->set_attribute( name  = zcl_abapgit_object_ssfo=>attrib_abapgit_leadig_spaces
                                      value = |{ offset }| ).

            CATCH cx_sy_move_cast_error ##no_handler.
          ENDTRY.
        ENDIF.
      CATCH zcx_abapgit_exception ##no_handler.
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE lastuser FROM stxfadm INTO rv_user
      WHERE formname = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
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
        no_form               = 1
        OTHERS                = 2.
    IF sy-subrc <> 0 AND sy-subrc <> 1.
      zcx_abapgit_exception=>raise( 'Error from FB_DELETE_FORM' ).
    ENDIF.

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
    DATA: within_code_section TYPE abap_bool.

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

      handle_attrib_leading_spaces( EXPORTING i_name                = lv_name
                                              i_node                = li_node
                                    CHANGING  c_within_code_section = within_code_section ).

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


  METHOD zif_abapgit_object~exists.

    DATA: lv_formname TYPE stxfadm-formname.

    SELECT SINGLE formname FROM stxfadm INTO lv_formname
      WHERE formname = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-delete_tadir = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.

    DATA: ssfo_formname TYPE tdsfname.

    ssfo_formname = ms_item-obj_name.
    CALL FUNCTION 'SSF_STATUS_INFO'
      EXPORTING
        i_formname = ssfo_formname
      IMPORTING
        o_inactive = ms_item-inactive.

    rv_active = boolc( ms_item-inactive = abap_false ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'E_SMFORM'
                                            iv_argument    = |{ ms_item-obj_name }| ).

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
        tcode     = 'SMARTFORMS'
        mode_val  = 'E'
      TABLES
        using_tab = lt_bdcdata
      EXCEPTIONS
        OTHERS    = 1
        ##fm_subrc_ok.                                                   "#EC CI_SUBRC

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
    DATA: within_code_section TYPE abap_bool.

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
      set_attribute_leading_spaces( EXPORTING i_name                = lv_name
                                              i_node                = li_node
                                    CHANGING  c_within_code_section = within_code_section ).

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
ENDCLASS.
