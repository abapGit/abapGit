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

    TYPES ty_wwwparams_tt TYPE STANDARD TABLE OF wwwparams WITH DEFAULT KEY.

    CONSTANTS: BEGIN OF c_param_names,
                 version  TYPE w3_name VALUE 'version',
                 fileext  TYPE w3_name VALUE 'fileextension',
                 filesize TYPE w3_name VALUE 'filesize',
                 filename TYPE w3_name VALUE 'filename',
                 mimetype TYPE w3_name VALUE 'mimetype',
               END OF c_param_names.

    METHODS constructor
      IMPORTING
        is_item     TYPE ty_item
        iv_language TYPE spras.

  PROTECTED SECTION.

    METHODS get_metadata REDEFINITION.

  PRIVATE SECTION.

    DATA ms_key TYPE wwwdatatab.

    METHODS get_ext
      IMPORTING it_params TYPE ty_wwwparams_tt
      RETURNING VALUE(rv_ext) TYPE string
      RAISING   lcx_exception.

    METHODS normalize_params
      IMPORTING iv_size   TYPE i
      CHANGING  ct_params TYPE ty_wwwparams_tt  " Param table to patch
      RAISING   lcx_exception.

    METHODS strip_params
      CHANGING  ct_params TYPE ty_wwwparams_tt
      RAISING   lcx_exception.

    METHODS find_param
      IMPORTING it_params TYPE ty_wwwparams_tt
                iv_name   TYPE w3_name
      RETURNING VALUE(rv_value) TYPE string
      RAISING   lcx_exception.

ENDCLASS. "lcl_object_W3SUPER DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_W3SUPER IMPLEMENTATION
*----------------------------------------------------------------------*
*   Web Reporting/Internet Transaction Server MIME Types (super class)
*----------------------------------------------------------------------*
CLASS lcl_object_w3super IMPLEMENTATION.

  METHOD constructor.
    super->constructor( is_item = is_item iv_language = iv_language ).
    ms_key-relid = ms_item-obj_type+2(2).
    ms_key-objid = ms_item-obj_name.
  ENDMETHOD.  " constructor.

  METHOD lif_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.  "lif_object~has_changed_since

  METHOD lif_object~changed_by.
    rv_user = c_user_unknown. " todo
  ENDMETHOD.

  METHOD lif_object~jump.
    " No idea how to jump to SMW0
    lcx_exception=>raise( 'Please go to SMW0 for W3MI object' ).
  ENDMETHOD.                    "jump

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD get_metadata. "Redefinition
    rs_metadata         = super->get_metadata( ).
    rs_metadata-version = 'v2.0.0'. " Seriazation v2, separate data file
  ENDMETHOD.  " get_metadata. "Redefinition

  METHOD lif_object~exists.

    SELECT SINGLE objid INTO ms_key-objid
      FROM wwwdata
      WHERE relid = ms_key-relid
      AND   objid = ms_key-objid
      AND   srtf2 = 0.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    rv_bool = abap_true.

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~serialize.

    DATA lt_w3mime    TYPE STANDARD TABLE OF w3mime.
    DATA lt_w3html    TYPE STANDARD TABLE OF w3html.
    DATA lt_w3params  TYPE STANDARD TABLE OF wwwparams.
    DATA lv_xstring   TYPE xstring.
    DATA lv_size      TYPE int4.

    SELECT SINGLE * INTO CORRESPONDING FIELDS OF ms_key
      FROM wwwdata
      WHERE relid = ms_key-relid
      AND   objid = ms_key-objid
      AND   srtf2 = 0.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key               = ms_key
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
        type             = ms_key-relid
        objid            = ms_key-objid
      TABLES
        params           = lt_w3params
      EXCEPTIONS
        entry_not_exists = 1.

    IF sy-subrc IS NOT INITIAL.
      lcx_exception=>raise( 'Cannot read W3xx data' ).
    ENDIF.

    lv_size = find_param( it_params = lt_w3params iv_name = c_param_names-filesize ).
    " Clean params (remove version, filesize & clear filename from path)
    strip_params( CHANGING  ct_params = lt_w3params ).

    CASE ms_key-relid.
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

    io_xml->add( iv_name = 'NAME'
                 ig_data = ms_key-objid ).

    io_xml->add( iv_name = 'TEXT'
                 ig_data = ms_key-text ).

    io_xml->add( iv_name = 'PARAMS'
                 ig_data = lt_w3params ).

    " Seriazation v2, separate data file. 'extra' added to prevent conflict with .xml
    lif_object~mo_files->add_raw( iv_data  = lv_xstring
                                  iv_extra = 'data'
                                  iv_ext   = get_ext( lt_w3params ) ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA lv_base64str TYPE string.
    DATA lt_w3params  TYPE STANDARD TABLE OF wwwparams.
    DATA lv_xstring   TYPE xstring.
    DATA lt_w3mime    TYPE STANDARD TABLE OF w3mime.
    DATA lt_w3html    TYPE STANDARD TABLE OF w3html.
    DATA lv_size      TYPE int4.
    DATA lv_tadir_obj TYPE tadir-object.


    io_xml->read( EXPORTING iv_name = 'TEXT'
                  CHANGING  cg_data = ms_key-text ).

    io_xml->read( EXPORTING iv_name = 'PARAMS'
                  CHANGING  cg_data = lt_w3params ).

    CASE io_xml->get_metadata( )-version.
      WHEN 'v1.0.0'.
        io_xml->read( EXPORTING iv_name = 'DATA'
                      CHANGING  cg_data = lv_base64str ).
        lv_xstring = cl_http_utility=>decode_x_base64( lv_base64str ).
      WHEN 'v2.0.0'.
        lv_xstring = lif_object~mo_files->read_raw( iv_extra = 'data'
                                                    iv_ext   = get_ext( lt_w3params ) ).
      WHEN OTHERS.
        lcx_exception=>raise( 'W3xx: Unknown serializer version' ).
    ENDCASE.

    CASE ms_key-relid.
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

    " Update size of file based on actual data file size, prove param object name
    normalize_params( EXPORTING iv_size   = lv_size
                      CHANGING  ct_params = lt_w3params ).

    CALL FUNCTION 'WWWPARAMS_UPDATE'
      TABLES
        params       = lt_w3params
      EXCEPTIONS
        update_error = 1.

    IF sy-subrc IS NOT INITIAL.
      lcx_exception=>raise( 'Cannot update W3xx params' ).
    ENDIF.

    ms_key-tdate    = sy-datum.
    ms_key-ttime    = sy-uzeit.
    ms_key-chname   = sy-uname.
    ms_key-devclass = iv_package.

    CALL FUNCTION 'WWWDATA_EXPORT'
      EXPORTING
        key               = ms_key
      TABLES
        mime              = lt_w3mime
        html              = lt_w3html
      EXCEPTIONS
        wrong_object_type = 1
        export_error      = 2.

    IF sy-subrc IS NOT INITIAL.
      lcx_exception=>raise( 'Cannot upload W3xx data' ).
    ENDIF.

    CONCATENATE 'W3' ms_key-relid INTO lv_tadir_obj.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_tadir_pgmid                 = 'R3TR'
        wi_tadir_object                = lv_tadir_obj
        wi_tadir_devclass              = iv_package
        wi_tadir_obj_name              = ms_key-objid
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

  METHOD lif_object~delete.

    CALL FUNCTION 'WWWDATA_DELETE'
      EXPORTING
        key               = ms_key
      EXCEPTIONS
        wrong_object_type = 1
        delete_error      = 2.

    IF sy-subrc IS NOT INITIAL.
      lcx_exception=>raise( 'Cannot delete W3xx data' ).
    ENDIF.

    CALL FUNCTION 'WWWPARAMS_DELETE_ALL'
      EXPORTING
        key          = ms_key
      EXCEPTIONS
        delete_error = 1.

    IF sy-subrc IS NOT INITIAL.
      lcx_exception=>raise( 'Cannot delete W3xx params' ).
    ENDIF.

  ENDMETHOD.                    "lif_object~delete

  METHOD get_ext.

    rv_ext = find_param( it_params = it_params iv_name = c_param_names-fileext ).
    SHIFT rv_ext LEFT DELETING LEADING '.'.

  ENDMETHOD.  " get_ext.

  METHOD normalize_params.

    FIELD-SYMBOLS <param> LIKE LINE OF ct_params.

    " Ensure filesize param exists
    READ TABLE ct_params ASSIGNING <param> WITH KEY name = c_param_names-filesize.
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO ct_params ASSIGNING <param>.
      <param>-name  = c_param_names-filesize.
    ENDIF.

    LOOP AT ct_params ASSIGNING <param>.
      <param>-relid = ms_key-relid. " Ensure param key = object key
      <param>-objid = ms_key-objid.
      IF <param>-name = c_param_names-filesize. " Patch filesize = real file size
        <param>-value = iv_size.
        CONDENSE <param>-value.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.  " normalize_params.

  METHOD strip_params.

    FIELD-SYMBOLS <param> LIKE LINE OF ct_params.

    " Remove path from filename
    find_param( it_params = ct_params iv_name = c_param_names-filename ). " Check exists
    READ TABLE ct_params ASSIGNING <param> WITH KEY name = c_param_names-filename.
    <param>-value = lcl_path=>get_filename_from_syspath( |{ <param>-value }| ).

    " Clear version & filesize
    DELETE ct_params WHERE name = c_param_names-version.
    DELETE ct_params WHERE name = c_param_names-filesize.

  ENDMETHOD.  " strip_params.

  METHOD find_param.

    FIELD-SYMBOLS <param> LIKE LINE OF it_params.

    READ TABLE it_params ASSIGNING <param> WITH KEY name = iv_name.
    IF sy-subrc > 0.
      lcx_exception=>raise( |W3xx: Cannot find { iv_name } for { ms_key-objid }| ).
    ENDIF.

    rv_value = <param>-value.

  ENDMETHOD.  " find_param.

  METHOD lif_object~compare_to_remote_version.
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
