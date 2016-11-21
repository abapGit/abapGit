*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_W3XX
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_object_W3SUPER DEFINITION
*----------------------------------------------------------------------*
*   Web Reporting/Internet Transaction Server MIME Types (super class)
*----------------------------------------------------------------------*
CLASS lcl_object_w3super DEFINITION INHERITING FROM lcl_objects_super ABSTRACT.

  PUBLIC SECTION.
    INTERFACES lif_object.

  PRIVATE SECTION.
    METHODS init_key RETURNING VALUE(rs_key) TYPE wwwdatatab.

ENDCLASS. "lcl_object_W3SUPER DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_W3SUPER IMPLEMENTATION
*----------------------------------------------------------------------*
*   Web Reporting/Internet Transaction Server MIME Types (super class)
*----------------------------------------------------------------------*
CLASS lcl_object_w3super IMPLEMENTATION.

  METHOD lif_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.  "lif_object~has_changed_since

  METHOD lif_object~changed_by.
    rv_user = c_user_unknown. " todo
  ENDMETHOD.

  METHOD init_key.
    rs_key-relid = ms_item-obj_type+2(2).
    rs_key-objid = ms_item-obj_name.
  ENDMETHOD.                    " init_key

  METHOD lif_object~jump.
    " No idea how to just to SMW0
    lcx_exception=>raise( 'Please go to SMW0 for W3MI object' ).
  ENDMETHOD.                    "jump

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " W3xx EXISTS
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  METHOD lif_object~exists.
    DATA ls_key   TYPE wwwdatatab.

    ls_key = init_key( ).

    SELECT SINGLE objid INTO ls_key-objid
      FROM wwwdata
      WHERE relid = ls_key-relid
      AND   objid = ls_key-objid
      AND   srtf2 = 0.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    rv_bool = abap_true.

  ENDMETHOD.                    "lif_object~exists

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " W3xx SERIALIZE
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  METHOD lif_object~serialize.
    DATA ls_key       TYPE wwwdatatab.
    DATA lt_w3mime    TYPE STANDARD TABLE OF w3mime.
    DATA lt_w3html    TYPE STANDARD TABLE OF w3html.
    DATA lt_w3params  TYPE STANDARD TABLE OF wwwparams.
    DATA lv_xstring   TYPE xstring.
    DATA ls_wwwparam  LIKE LINE OF lt_w3params.
    DATA lv_size      TYPE int4.
    DATA lv_base64str TYPE string.
    DATA lo_utility   TYPE REF TO cl_http_utility.

    ls_key = init_key( ).

    SELECT SINGLE * INTO CORRESPONDING FIELDS OF ls_key
      FROM wwwdata
      WHERE relid = ls_key-relid
      AND   objid = ls_key-objid
      AND   srtf2 = 0.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key               = ls_key
      TABLES
        mime              = lt_w3mime
        html              = lt_w3html
      EXCEPTIONS
        wrong_object_type = 1
        import_error      = 2.

    IF sy-subrc IS NOT INITIAL.
      lcx_exception=>raise( 'Cannot read W3xx data' ).
    ENDIF.

    CALL FUNCTION 'WWWPARAMS_READ_ALL'
      EXPORTING
        type             = ls_key-relid
        objid            = ls_key-objid
      TABLES
        params           = lt_w3params
      EXCEPTIONS
        entry_not_exists = 1.

    IF sy-subrc IS NOT INITIAL.
      lcx_exception=>raise( 'Cannot read W3xx data' ).
    ENDIF.

    READ TABLE lt_w3params INTO ls_wwwparam WITH KEY name = 'filesize' ##NO_TEXT.
    IF sy-subrc IS NOT INITIAL.
      lcx_exception=>raise( 'Cannot read W3xx filesize' ).
    ENDIF.

    lv_size = ls_wwwparam-value.

    CASE ls_key-relid.
      WHEN 'MI'.
        CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
          EXPORTING
            input_length = lv_size
          IMPORTING
            buffer       = lv_xstring
          TABLES
            binary_tab   = lt_w3mime
          EXCEPTIONS
            failed       = 1.
      WHEN 'HT'.
        CALL FUNCTION 'SCMS_TEXT_TO_XSTRING'
          IMPORTING
            buffer   = lv_xstring
          TABLES
            text_tab = lt_w3html
          EXCEPTIONS
            failed   = 1.
      WHEN OTHERS.
        lcx_exception=>raise( 'Wrong W3xx type' ).
    ENDCASE.

    IF sy-subrc IS NOT INITIAL.
      lcx_exception=>raise( 'Cannot convert W3xx to xstring' ).
    ENDIF.

    CREATE OBJECT lo_utility.
    lv_base64str = lo_utility->encode_x_base64( lv_xstring ).

    io_xml->add( iv_name = 'NAME'
                 ig_data = ls_key-objid ).

    io_xml->add( iv_name = 'TEXT'
                 ig_data = ls_key-text ).

    io_xml->add( iv_name = 'DATA'
                 ig_data = lv_base64str ).

    io_xml->add( iv_name = 'PARAMS'
                 ig_data = lt_w3params ).

  ENDMETHOD.                    "serialize

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " W3xx DESERIALIZE
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  METHOD lif_object~deserialize.

    DATA ls_key       TYPE wwwdatatab.
    DATA lv_base64str TYPE string.
    DATA lt_w3params  TYPE STANDARD TABLE OF wwwparams.
    DATA lv_xstring   TYPE xstring.
    DATA lo_utility   TYPE REF TO cl_http_utility.
    DATA lt_w3mime    TYPE STANDARD TABLE OF w3mime.
    DATA lt_w3html    TYPE STANDARD TABLE OF w3html.
    DATA lv_size      TYPE int4.
    DATA lv_tadir_obj TYPE tadir-object.

    ls_key = init_key( ).

    io_xml->read( EXPORTING iv_name = 'TEXT'
                  CHANGING  cg_data = ls_key-text ).

    io_xml->read( EXPORTING iv_name = 'DATA'
                  CHANGING  cg_data = lv_base64str ).

    io_xml->read( EXPORTING iv_name = 'PARAMS'
                  CHANGING  cg_data = lt_w3params ).

    CREATE OBJECT lo_utility.
    lv_xstring = lo_utility->decode_x_base64( lv_base64str ).

    CASE ls_key-relid.
      WHEN 'MI'.
        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            buffer        = lv_xstring
          IMPORTING
            output_length = lv_size
          TABLES
            binary_tab    = lt_w3mime.
      WHEN 'HT'.
        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            buffer        = lv_xstring
          IMPORTING
            output_length = lv_size
          TABLES
            binary_tab    = lt_w3mime.

        CALL FUNCTION 'SCMS_BINARY_TO_TEXT'
          EXPORTING
            input_length  = lv_size
          IMPORTING
            output_length = lv_size
          TABLES
            binary_tab    = lt_w3mime
            text_tab      = lt_w3html
          EXCEPTIONS
            failed        = 1.
        IF sy-subrc IS NOT INITIAL.
          lcx_exception=>raise( 'Cannot update W3xx params' ).
        ENDIF.

        CLEAR lt_w3mime.
      WHEN OTHERS.
        lcx_exception=>raise( 'Wrong W3xx type' ).
    ENDCASE.

    CALL FUNCTION 'WWWPARAMS_UPDATE'
      TABLES
        params       = lt_w3params
      EXCEPTIONS
        update_error = 1.

    IF sy-subrc IS NOT INITIAL.
      lcx_exception=>raise( 'Cannot update W3xx params' ).
    ENDIF.

    ls_key-tdate    = sy-datum.
    ls_key-ttime    = sy-uzeit.
    ls_key-chname   = sy-uname.
    ls_key-devclass = iv_package.

    CALL FUNCTION 'WWWDATA_EXPORT'
      EXPORTING
        key               = ls_key
      TABLES
        mime              = lt_w3mime
        html              = lt_w3html
      EXCEPTIONS
        wrong_object_type = 1
        export_error      = 2.

    IF sy-subrc IS NOT INITIAL.
      lcx_exception=>raise( 'Cannot upload W3xx data' ).
    ENDIF.

    CONCATENATE 'W3' ls_key-relid INTO lv_tadir_obj.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_tadir_pgmid                 = 'R3TR'
        wi_tadir_object                = lv_tadir_obj
        wi_tadir_devclass              = iv_package
        wi_tadir_obj_name              = ls_key-objid
        wi_test_modus                  = space
      EXCEPTIONS
        tadir_entry_not_existing       = 1
        tadir_entry_ill_type           = 2
        no_systemname                  = 3
        no_systemtype                  = 4
        original_system_conflict       = 5
        object_reserved_for_devclass   = 6
        object_exists_global           = 7
        object_exists_local            = 8
        object_is_distributed          = 9
        obj_specification_not_unique   = 10
        no_authorization_to_delete     = 11
        devclass_not_existing          = 12
        simultanious_set_remove_repair = 13
        order_missing                  = 14
        no_modification_of_head_syst   = 15
        pgmid_object_not_allowed       = 16
        masterlanguage_not_specified   = 17
        devclass_not_specified         = 18
        specify_owner_unique           = 19
        loc_priv_objs_no_repair        = 20
        gtadir_not_reached             = 21
        object_locked_for_order        = 22
        change_of_class_not_allowed    = 23
        no_change_from_sap_to_tmp      = 24
        OTHERS                         = 99.

    IF sy-subrc IS NOT INITIAL.
      lcx_exception=>raise( 'Cannot update TADIR for W3xx' ).
    ENDIF.

  ENDMETHOD.                    "lif_object~deserialize

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " W3xx DELETE
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  METHOD lif_object~delete.
    DATA ls_key TYPE wwwdatatab.

    ls_key = init_key( ).

    CALL FUNCTION 'WWWDATA_DELETE'
      EXPORTING
        key               = ls_key
      EXCEPTIONS
        wrong_object_type = 1
        delete_error      = 2.

    IF sy-subrc IS NOT INITIAL.
      lcx_exception=>raise( 'Cannot delete W3xx data' ).
    ENDIF.

    CALL FUNCTION 'WWWPARAMS_DELETE_ALL'
      EXPORTING
        key          = ls_key
      EXCEPTIONS
        delete_error = 1.

    IF sy-subrc IS NOT INITIAL.
      lcx_exception=>raise( 'Cannot delete W3xx params' ).
    ENDIF.

  ENDMETHOD.                    "lif_object~delete

  METHOD lif_object~compare_to_previous_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_null_comparison_result.
  ENDMETHOD.

ENDCLASS. "lcl_object_W3SUPER IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_W3MI DEFINITION
*----------------------------------------------------------------------*
*   Web Reporting/Internet Transaction Server MIME Types (binary data)
*----------------------------------------------------------------------*
CLASS lcl_object_w3mi DEFINITION INHERITING FROM lcl_object_w3super FINAL.
ENDCLASS.                    "lcl_object_W3MI DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_W3HT DEFINITION
*----------------------------------------------------------------------*
*   Web Reporting/Internet Transaction Server MIME Types (html data)
*----------------------------------------------------------------------*
CLASS lcl_object_w3ht DEFINITION INHERITING FROM lcl_object_w3super FINAL.
ENDCLASS.                    "lcl_object_W3HT DEFINITION