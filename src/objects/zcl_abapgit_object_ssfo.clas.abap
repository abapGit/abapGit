CLASS zcl_abapgit_object_ssfo DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      ty_string_range TYPE RANGE OF string .

    CLASS-DATA gt_range_node_codes TYPE ty_string_range .
    CONSTANTS c_prefix TYPE string VALUE 'File:'.

    METHODS fix_ids
      IMPORTING
        !ii_xml_doc TYPE REF TO if_ixml_document .
    CLASS-METHODS sort_texts
      IMPORTING
        !ii_xml_doc TYPE REF TO if_ixml_document
      RAISING
        zcx_abapgit_exception .
    METHODS get_range_node_codes
      RETURNING
        VALUE(rt_range_node_codes) TYPE ty_string_range .
    METHODS deserialize_sources
      IMPORTING
        !ii_node TYPE REF TO if_ixml_node
      RAISING
        zcx_abapgit_exception.
    METHODS serialize_sources
      IMPORTING
        !ii_node TYPE REF TO if_ixml_node
      RAISING
        zcx_abapgit_exception.
    METHODS get_hash_for_path
      IMPORTING
        !ii_node       TYPE REF TO if_ixml_node
      RETURNING
        VALUE(rv_hash) TYPE string
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS zcl_abapgit_object_ssfo IMPLEMENTATION.


  METHOD deserialize_sources.

    DATA:
      lv_extra   TYPE string,
      ls_abap    TYPE abaptxt255,
      lt_abap    TYPE abaptxt255_tab,
      li_node    TYPE REF TO if_ixml_node,
      li_ixml    TYPE REF TO if_ixml,
      li_xml_doc TYPE REF TO if_ixml_document.

    li_ixml = cl_ixml=>create( ).
    li_xml_doc = li_ixml->create_document( ).

    " Old format
    lv_extra = ii_node->get_value( ).
    IF lv_extra NS c_prefix.
      RETURN.
    ENDIF.

    " New format
    lv_extra = lv_extra+5(*).

    lt_abap = mo_files->read_abap( iv_extra = lv_extra ).

    ii_node->set_value( '' ).
    LOOP AT lt_abap INTO ls_abap.
      li_node = li_xml_doc->create_element( 'item' ).
      li_node->set_value( |{ ls_abap-line }| ).
      ii_node->append_child( li_node ).
    ENDLOOP.

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
      ty_id_mappings TYPE HASHED TABLE OF ty_id_mapping
                          WITH UNIQUE KEY old.

    DATA: lv_name       TYPE string,
          li_idref      TYPE REF TO if_ixml_node,
          li_node       TYPE REF TO if_ixml_node,
          li_attr       TYPE REF TO if_ixml_named_node_map,
          li_iterator   TYPE REF TO if_ixml_node_iterator,
          lt_id_mapping TYPE ty_id_mappings,
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


  METHOD get_hash_for_path.

    DATA:
      lv_name  TYPE string,
      lv_path  TYPE string,
      li_node  TYPE REF TO if_ixml_node,
      li_name  TYPE REF TO if_ixml_node,
      li_iname TYPE REF TO if_ixml_node.

    li_node = ii_node->get_parent( ).
    WHILE NOT li_node IS INITIAL.
      lv_name = li_node->get_name( ).
      IF ( lv_name = 'CODE' OR lv_name = 'WINDOW' OR lv_name = 'PAGE' )
        AND li_node->get_namespace_prefix( ) IS NOT INITIAL.
        li_name  = li_node->get_first_child( ).
        li_iname = li_node->get_first_child( ).
        lv_name  = lv_name && ':' && li_iname->get_value( ).
      ENDIF.
      lv_path = lv_name && '/' && lv_path.
      li_node = li_node->get_parent( ).
    ENDWHILE.

    rv_hash = substring(
      val = zcl_abapgit_hash=>sha1_string( lv_path )
      len = 8 ).

  ENDMETHOD.


  METHOD get_range_node_codes.

    DATA: ls_range_node_code TYPE LINE OF ty_string_range.

    IF gt_range_node_codes IS INITIAL.
      ls_range_node_code-sign   = 'I'.
      ls_range_node_code-option = 'EQ'.
      ls_range_node_code-low    = 'CODE'.
      INSERT ls_range_node_code INTO TABLE gt_range_node_codes.
      ls_range_node_code-low    = 'GTYPES'.
      INSERT ls_range_node_code INTO TABLE gt_range_node_codes.
      ls_range_node_code-low    = 'GCODING'.
      INSERT ls_range_node_code INTO TABLE gt_range_node_codes.
      ls_range_node_code-low    = 'FCODING'.
      INSERT ls_range_node_code INTO TABLE gt_range_node_codes.
    ENDIF.

    rt_range_node_codes = gt_range_node_codes.

  ENDMETHOD.


  METHOD serialize_sources.

    DATA:
      lv_extra    TYPE string,
      ls_abap     TYPE abaptxt255,
      lt_abap     TYPE abaptxt255_tab,
      li_node     TYPE REF TO if_ixml_node,
      li_iterator TYPE REF TO if_ixml_node_iterator.

    " Store code as separate ABAP files instead of XML
    lv_extra    = to_lower( ii_node->get_name( ) ).
    li_iterator = ii_node->get_children( )->create_iterator( ).
    li_node     = li_iterator->get_next( ).
    WHILE NOT li_node IS INITIAL.
      ls_abap-line = li_node->get_value( ).
      INSERT ls_abap INTO TABLE lt_abap.

      li_node = li_iterator->get_next( ).
    ENDWHILE.

    " For CODE sections, get full path and hash it
    IF lv_extra = 'code'.
      lv_extra = get_hash_for_path( ii_node ).
    ENDIF.

    mo_files->add_abap(
      iv_extra = lv_extra
      it_abap  = lt_abap ).

    ii_node->set_value( c_prefix && lv_extra ).

  ENDMETHOD.


  METHOD sort_texts.

    DATA: li_node      TYPE REF TO if_ixml_node,
          li_item      TYPE REF TO if_ixml_node,
          li_field     TYPE REF TO if_ixml_node,
          li_item_list TYPE REF TO if_ixml_node_list,
          li_iterator  TYPE REF TO if_ixml_node_iterator,
          li_items     TYPE REF TO if_ixml_node_iterator,
          lv_index     TYPE i,
          lv_field     TYPE fieldname,
          ls_item      TYPE stxfobjt,
          lt_items     TYPE STANDARD TABLE OF stxfobjt.

    FIELD-SYMBOLS <lv_field> TYPE any.

    li_iterator = ii_xml_doc->create_iterator( ).
    li_node = li_iterator->get_next( ).
    WHILE NOT li_node IS INITIAL.
      IF li_node->get_name( ) = 'T_CAPTION'.

        " Read all records for T_CAPTION
        CLEAR lt_items.
        li_item_list = li_node->get_children( ).
        li_items = li_item_list->create_iterator( ).
        DO.
          li_item = li_items->get_next( ).
          IF li_item IS INITIAL.
            EXIT.
          ENDIF.
          CLEAR ls_item.
          li_field = li_item->get_first_child( ).
          WHILE NOT li_field IS INITIAL.
            lv_field = li_field->get_name( ).
            ASSIGN COMPONENT lv_field OF STRUCTURE ls_item TO <lv_field>.
            ASSERT sy-subrc = 0.
            <lv_field> = li_field->get_value( ).
            li_field = li_field->get_next( ).
          ENDWHILE.
          INSERT ls_item INTO TABLE lt_items.
        ENDDO.

        SORT lt_items.

        " Write all records back after sorting
        lv_index = 1.
        li_items = li_item_list->create_iterator( ).
        DO.
          li_item = li_items->get_next( ).
          IF li_item IS INITIAL.
            EXIT.
          ENDIF.
          READ TABLE lt_items INTO ls_item INDEX lv_index.
          li_field = li_item->get_first_child( ).
          WHILE NOT li_field IS INITIAL.
            lv_field = li_field->get_name( ).
            ASSIGN COMPONENT lv_field OF STRUCTURE ls_item TO <lv_field>.
            ASSERT sy-subrc = 0.
            li_field->set_value( |{ <lv_field> }| ).
            li_field = li_field->get_next( ).
          ENDWHILE.

* guess this can only happen for CAPTION field, as other are key fields
* always add the empty values or they will cause diffs
          IF lv_field <> 'CAPTION'.
            ii_xml_doc->create_simple_element(
              name   = 'CAPTION'
              value  = |{ ls_item-caption }|
              parent = li_item ).
          ENDIF.

          lv_index = lv_index + 1.
        ENDDO.

      ENDIF.
      li_node = li_iterator->get_next( ).
    ENDWHILE.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE lastuser FROM stxfadm INTO rv_user
      WHERE formname = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

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
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.
* see function module FB_UPLOAD_FORM

    DATA:
      li_node     TYPE REF TO if_ixml_node,
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
          li_node->set_value( sy-datum(4) && '-' && sy-datum+4(2) && '-' && sy-datum+6(2) ).
        WHEN 'LASTTIME'.
          li_node->set_value( sy-uzeit(2) && ':' && sy-uzeit+2(2) && ':' && sy-uzeit+4(2) ).
        WHEN 'FIRSTUSER' OR 'LASTUSER'.
          li_node->set_value( sy-uname && '' ).

      ENDCASE.

      IF lv_name IN get_range_node_codes( ) AND li_node->get_namespace_prefix( ) IS INITIAL.
        deserialize_sources( li_node ).
      ENDIF.

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

    DATA: lv_ssfo_formname TYPE tdsfname.
    DATA lv_inactive TYPE abap_bool.

    lv_ssfo_formname = ms_item-obj_name.

    CALL FUNCTION 'SSF_STATUS_INFO'
      EXPORTING
        i_formname = lv_ssfo_formname
      IMPORTING
        o_inactive = lv_inactive.

    rv_active = boolc( lv_inactive = abap_false ).

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

    zcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SMARTFORMS'
      it_bdcdata = lt_bdcdata ).

    rv_exit = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
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
* the smartform is not present in system, or other error occurred
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

      IF lv_name IN get_range_node_codes( ) AND li_node->get_namespace_prefix( ) IS INITIAL.
        serialize_sources( li_node ).
      ENDIF.

      li_node = li_iterator->get_next( ).
    ENDWHILE.

    fix_ids( li_xml_doc ).

    sort_texts( li_xml_doc ).

    li_element = li_xml_doc->get_root_element( ).
    li_element->set_attribute(
      name      = 'sf'
      namespace = 'xmlns'
      value     = 'urn:sap-com:SmartForms:2000:internal-structure' ).
    li_element->set_attribute(
      name  = 'xmlns'
      value = 'urn:sap-com:sdixml-ifr:2000' ).

    io_xml->set_raw( li_xml_doc->get_root_element( ) ).

  ENDMETHOD.
ENDCLASS.
