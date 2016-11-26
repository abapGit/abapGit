*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_GUI_ASSET_MANAGER
*&---------------------------------------------------------------------*

CLASS lcl_gui_asset_manager DEFINITION FINAL CREATE PRIVATE FRIENDS lcl_gui.
  PUBLIC SECTION.

    METHODS get_asset
      IMPORTING iv_asset_name  TYPE string
      RETURNING VALUE(rv_data) TYPE xstring
      RAISING   lcx_exception.

  PRIVATE SECTION.

    METHODS get_inline_asset
      IMPORTING iv_asset_name  TYPE string
      RETURNING VALUE(rv_data) TYPE xstring
      RAISING   lcx_exception.

    METHODS get_mime_asset
      IMPORTING iv_asset_name  TYPE c
      RETURNING VALUE(rv_data) TYPE xstring
      RAISING   lcx_exception.

ENDCLASS. "lcl_gui_asset_manager

CLASS lcl_gui_asset_manager IMPLEMENTATION.

  METHOD get_asset.

    DATA: lv_asset_name TYPE string,
          lv_mime_name  TYPE wwwdatatab-objid.

    lv_asset_name = to_upper( iv_asset_name ).

    CASE lv_asset_name.
      WHEN 'CSS_COMMON'.
        lv_mime_name = 'ZABAPGIT_CSS_COMMON'.
      WHEN 'JS_COMMON'.
        lv_mime_name = 'ZABAPGIT_JS_COMMON'.
      WHEN OTHERS.
        lcx_exception=>raise( |Improper resource name: { iv_asset_name }| ).
    ENDCASE.

    rv_data = get_mime_asset( lv_mime_name ).
    IF rv_data IS INITIAL. " Fallback to inline asset
      rv_data = get_inline_asset( lv_asset_name ).
    ENDIF.

    IF rv_data IS INITIAL.
      lcx_exception=>raise( |Failed to get GUI resource: { iv_asset_name }| ).
    ENDIF.

  ENDMETHOD.  " get_asset.

  METHOD get_mime_asset.

    DATA: ls_key    TYPE wwwdatatab,
          lv_size_c TYPE wwwparams-value,
          lv_size   TYPE i,
          lt_w3mime TYPE STANDARD TABLE OF w3mime.

    ls_key-relid = 'MI'.
    ls_key-objid = iv_asset_name.

    " Get exact file size
    CALL FUNCTION 'WWWPARAMS_READ'
      EXPORTING
        relid            = ls_key-relid
        objid            = ls_key-objid
        name             = 'filesize'
      IMPORTING
        value            = lv_size_c
      EXCEPTIONS
        entry_not_exists = 1.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    lv_size = lv_size_c.

    " Get binary data
    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key               = ls_key
      TABLES
        mime              = lt_w3mime
      EXCEPTIONS
        wrong_object_type = 1
        import_error      = 2.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = lv_size
      IMPORTING
        buffer       = rv_data
      TABLES
        binary_tab   = lt_w3mime
      EXCEPTIONS
        failed       = 1.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

  ENDMETHOD.  " get_mime_asset.

  METHOD get_inline_asset.

    DATA: lt_data TYPE ty_string_tt,
          lv_str  TYPE string.

    CASE iv_asset_name.
      WHEN 'CSS_COMMON'.
        " @@abapmerge include src/zabapgit_css_common.data.css > APPEND '$$' TO lt_data.
      WHEN 'JS_COMMON'.
        " @@abapmerge include src/zabapgit_js_common.data.css > APPEND '$$' TO lt_data.
      WHEN OTHERS.
        lcx_exception=>raise( |No inline resource: { iv_asset_name }| ).
    ENDCASE.

    CONCATENATE LINES OF lt_data INTO lv_str SEPARATED BY gc_newline.

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text      = lv_str
      IMPORTING
        buffer    = rv_data
      EXCEPTIONS
        OTHERS    = 1.
    ASSERT sy-subrc = 0.

  ENDMETHOD.  " get_inline_asset.

ENDCLASS. "lcl_gui_asset_manager