CLASS zcl_abapgit_ecatt_helper DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      build_xml_of_object
        IMPORTING
          iv_object_name       TYPE etobj_name
          iv_object_version    TYPE etobj_ver
          iv_object_type       TYPE etobj_type
          io_download          TYPE REF TO cl_apl_ecatt_download
        RETURNING
          VALUE(rv_xml_stream) TYPE xstring
        RAISING
          zcx_abapgit_exception,

      download_data
        IMPORTING
          ii_template_over_all TYPE REF TO if_ixml_document
        RETURNING
          VALUE(rv_xml_stream) TYPE xstring,

      upload_data_from_stream
        IMPORTING
          iv_xml_stream               TYPE xstring
        RETURNING
          VALUE(ri_template_over_all) TYPE REF TO if_ixml_document
        RAISING
          cx_ecatt_apl,

      is_object_loaded
        IMPORTING
          io_object        TYPE REF TO cl_apl_ecatt_object
          ix_error         TYPE REF TO cx_root
        RETURNING
          VALUE(rv_loaded) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      c_xml        TYPE i VALUE 1, " downport of if_apl_ecatt_xml=>co_xml
      c_deep_data  TYPE string VALUE 'DEEP_DATA',
      " DOM node names instead of get_type( ), which open-abap does not support
      c_text_node  TYPE string VALUE '#text',
      c_cdata_node TYPE string VALUE '#cdata-section'.

    CLASS-METHODS:
      strip_deep_data_whitespace
        IMPORTING
          ii_node TYPE REF TO if_ixml_node,

      strip_mixed_content
        IMPORTING
          ii_node TYPE REF TO if_ixml_node.

ENDCLASS.



CLASS zcl_abapgit_ecatt_helper IMPLEMENTATION.


  METHOD is_object_loaded.

    " cl_apl_ecatt_object=>show_object also raises warnings, for example about
    " unused command interfaces, after the object has been read completely.
    " CL_APL_ECATT_DOWNLOAD carries on after any exception. Carry on as well,
    " but not if the object is missing or the exception aborts the load.

    FIELD-SYMBOLS <lv_abort_flag> TYPE any.

    IF io_object IS INITIAL.
      RETURN.
    ENDIF.

    ASSIGN ix_error->('ABORT_FLAG') TO <lv_abort_flag>.
    IF sy-subrc = 0 AND <lv_abort_flag> IS INITIAL.
      rv_loaded = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD build_xml_of_object.

    " downport of CL_APL_ECATT_DOWNLOAD=>BUILD_XML_OF_OBJECT

    DATA: lo_load_help_dummy TYPE REF TO cl_apl_ecatt_load_help,
          lx_ecatt           TYPE REF TO cx_ecatt_apl,
          lv_text            TYPE string,
          li_download        TYPE REF TO zif_abapgit_ecatt_download.

    "download method will create the xml stream
    "note: it's the redefined download( ) of each object type specific download, which is called
    TRY.
        CREATE OBJECT lo_load_help_dummy
          EXPORTING
            im_maintain_function = ''.

        io_download->download( im_object_name    = iv_object_name
                               im_object_version = iv_object_version
                               im_object_type    = iv_object_type
                               im_load_help      = lo_load_help_dummy ).

      CATCH cx_ecatt_apl INTO lx_ecatt.
        lv_text = lx_ecatt->get_text( ).
        zcx_abapgit_exception=>raise( lv_text ).
        " note, exception cx_ecatt_ui_attachment doesn't exist in 702
      CATCH cx_ecatt ##NO_HANDLER.
        "will never be raised from download, when called with mv_generate_xml_no_download = 'X'.
    ENDTRY.

    li_download ?= io_download.

    rv_xml_stream = li_download->get_xml_stream( ).

  ENDMETHOD.


  METHOD download_data.

    DATA:
      lo_xml TYPE REF TO cl_apl_ecatt_xml.

    TRY.
        CALL METHOD cl_apl_ecatt_xml=>('CREATE') " doesn't exist in 702
          EXPORTING
            im_type = c_xml
          RECEIVING
            re_xml  = lo_xml.

        " heal objects that were imported with the pretty printer's whitespace
        IF ii_template_over_all IS BOUND.
          strip_deep_data_whitespace( ii_template_over_all->get_root_element( ) ).
        ENDIF.

        lo_xml->set_attributes( im_dom = ii_template_over_all ).

        lo_xml->get_attributes( IMPORTING ex_xml = rv_xml_stream ).

      CATCH cx_ecatt_apl_xml.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD upload_data_from_stream.

    DATA:
      lo_xml           TYPE REF TO cl_apl_ecatt_xml,
      lv_xstr          TYPE xstring,
      li_nc_xmlref_typ TYPE REF TO if_ixml_node_collection,
      li_n_xmlref_typ  TYPE REF TO if_ixml_node,
      lv_index         TYPE i VALUE 0,
      lv_count         TYPE i.

    lv_xstr = iv_xml_stream.

    CALL METHOD cl_apl_ecatt_xml=>('CREATE') " doesn't exist in 702
      EXPORTING
        im_type = c_xml
      RECEIVING
        re_xml  = lo_xml.

* whitespace stripping needs a namespace
* remove white spaces only at the time of upload
    lo_xml->stream_to_dom( im_xstream            = lv_xstr
                           im_ignore_white_space = 'X'
                           im_uri                = cl_apl_xml_const=>schema_uri ).

    lo_xml->get_attributes( IMPORTING ex_dom = ri_template_over_all ).

    IF ri_template_over_all IS BOUND.
      strip_deep_data_whitespace( ri_template_over_all->get_root_element( ) ).
    ENDIF.

* MD: Workaround, because nodes starting with "XML" are not allowed
    li_nc_xmlref_typ ?= ri_template_over_all->get_elements_by_tag_name_ns( 'XMLREF_TYP' ).
    CALL METHOD li_nc_xmlref_typ->('GET_LENGTH')  " downport
      RECEIVING
        rval = lv_count.

    WHILE lv_index < lv_count.
      li_n_xmlref_typ = li_nc_xmlref_typ->get_item( lv_index ).
      li_n_xmlref_typ->set_name( 'X-MLREF_TYP' ).
      lv_index = lv_index + 1.
    ENDWHILE.

  ENDMETHOD.


  METHOD strip_deep_data_whitespace.

    " The abapGit XML is rendered with indentation. Below DEEP_DATA the elements
    " carry their own namespaces (rfc:, ddic:, ...), so the whitespace stripping
    " of the parsers does not apply there. Without clean-up, the indentation is
    " stored as text and every pull adds more blank lines to DEEP_DATA.

    DATA li_child TYPE REF TO if_ixml_node.

    IF ii_node IS NOT BOUND.
      RETURN.
    ENDIF.

    IF ii_node->get_name( ) = c_deep_data.
      strip_mixed_content( ii_node ).
      RETURN.
    ENDIF.

    li_child = ii_node->get_first_child( ).
    WHILE li_child IS BOUND.
      strip_deep_data_whitespace( li_child ).
      li_child = li_child->get_next( ).
    ENDWHILE.

  ENDMETHOD.


  METHOD strip_mixed_content.

    " Only elements with child elements are touched: whitespace-only text between
    " the elements is removed, other text (e.g. the X next to valueRefName) is
    " trimmed. Elements with text content only keep their value unchanged.
    " A text that the parser split into several nodes (entities, CDATA) is only
    " trimmed at its outer ends, so inner blanks are kept.

    DATA: li_child        TYPE REF TO if_ixml_node,
          li_next         TYPE REF TO if_ixml_node,
          lv_name         TYPE string,
          lv_next_name    TYPE string,
          lv_prev_is_text TYPE abap_bool,
          lv_next_is_text TYPE abap_bool,
          lv_has_element  TYPE abap_bool,
          lv_value        TYPE string,
          lv_trimmed      TYPE string.

    " element names never start with #, other node types do (#text, #comment, ...)
    li_child = ii_node->get_first_child( ).
    WHILE li_child IS BOUND.
      lv_name = li_child->get_name( ).
      IF lv_name IS NOT INITIAL AND lv_name(1) <> '#'.
        lv_has_element = abap_true.
        EXIT.
      ENDIF.
      li_child = li_child->get_next( ).
    ENDWHILE.

    IF lv_has_element = abap_false.
      RETURN.
    ENDIF.

    li_child = ii_node->get_first_child( ).
    WHILE li_child IS BOUND.
      " read the sibling first, the current node might be removed
      li_next = li_child->get_next( ).

      lv_name = li_child->get_name( ).

      CLEAR lv_next_name.
      IF li_next IS BOUND.
        lv_next_name = li_next->get_name( ).
      ENDIF.
      lv_next_is_text = boolc( lv_next_name = c_text_node OR lv_next_name = c_cdata_node ).

      IF lv_name = c_text_node.
        lv_value = li_child->get_value( ).
        FIND REGEX '\S' IN lv_value.
        IF sy-subrc <> 0 AND lv_prev_is_text = abap_false AND lv_next_is_text = abap_false.
          li_child->remove_node( ).
          " the removed node is no neighbour of the next one
          li_child = li_next.
          CONTINUE.
        ENDIF.

        lv_trimmed = lv_value.
        IF lv_prev_is_text = abap_false.
          REPLACE REGEX '\A\s+' IN lv_trimmed WITH ''.
        ENDIF.
        IF lv_next_is_text = abap_false.
          REPLACE REGEX '\s+\z' IN lv_trimmed WITH ''.
        ENDIF.
        IF lv_trimmed <> lv_value.
          li_child->set_value( lv_trimmed ).
        ENDIF.
      ELSEIF lv_name IS NOT INITIAL AND lv_name(1) <> '#'.
        strip_mixed_content( li_child ).
      ENDIF.

      lv_prev_is_text = boolc( lv_name = c_text_node OR lv_name = c_cdata_node ).
      li_child = li_next.
    ENDWHILE.

  ENDMETHOD.
ENDCLASS.
