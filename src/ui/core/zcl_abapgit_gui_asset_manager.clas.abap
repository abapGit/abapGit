CLASS zcl_abapgit_gui_asset_manager DEFINITION PUBLIC FINAL CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_asset_manager.

    TYPES:
      BEGIN OF ty_asset_entry.
        INCLUDE TYPE zif_abapgit_gui_asset_manager~ty_web_asset.
    TYPES: mime_name TYPE wwwdatatab-objid,
           END OF ty_asset_entry ,
           tt_asset_register TYPE STANDARD TABLE OF ty_asset_entry WITH KEY url.

    METHODS register_asset
      IMPORTING
        !iv_url       TYPE string
        !iv_type      TYPE string
        !iv_cachable  TYPE abap_bool DEFAULT abap_true
        !iv_mime_name TYPE wwwdatatab-objid OPTIONAL
        !iv_base64    TYPE string OPTIONAL
        !iv_inline    TYPE string OPTIONAL .

    METHODS finalize_registration
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF gty_css_var,
        name  TYPE string,
        value TYPE string,
      END OF gty_css_var,
      gty_css_var_tab TYPE SORTED TABLE OF gty_css_var WITH UNIQUE KEY name.

    DATA mt_asset_register TYPE tt_asset_register.

    METHODS get_mime_asset
      IMPORTING
        iv_mime_name    TYPE c
      RETURNING
        VALUE(rv_xdata) TYPE xstring
      RAISING
        zcx_abapgit_exception.

    METHODS load_asset
      IMPORTING
        is_asset_entry  TYPE ty_asset_entry
      RETURNING
        VALUE(rs_asset) TYPE zif_abapgit_gui_asset_manager~ty_web_asset
      RAISING
        zcx_abapgit_exception.

    METHODS get_css_vars_in_string
      IMPORTING
        iv_string           TYPE string
      RETURNING
        VALUE(rt_variables) TYPE gty_css_var_tab.

ENDCLASS.



CLASS zcl_abapgit_gui_asset_manager IMPLEMENTATION.


  METHOD get_mime_asset.

    DATA: ls_key    TYPE wwwdatatab,
          lv_size_c TYPE wwwparams-value,
          lv_size   TYPE i,
          lt_w3mime TYPE STANDARD TABLE OF w3mime.

    ls_key-relid = 'MI'.
    ls_key-objid = iv_mime_name.

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

    rv_xdata = zcl_abapgit_convert=>bintab_to_xstring(
      iv_size   = lv_size
      it_bintab = lt_w3mime ).

  ENDMETHOD.


  METHOD load_asset.

    MOVE-CORRESPONDING is_asset_entry TO rs_asset.
    IF rs_asset-content IS INITIAL AND is_asset_entry-mime_name IS NOT INITIAL.
      " inline content has the priority
      rs_asset-content = get_mime_asset( is_asset_entry-mime_name ).
    ENDIF.
    IF rs_asset-content IS INITIAL.
      zcx_abapgit_exception=>raise( |failed to load GUI asset: { is_asset_entry-url }| ).
    ENDIF.

  ENDMETHOD.


  METHOD register_asset.

    DATA ls_asset LIKE LINE OF mt_asset_register.

    SPLIT iv_type AT '/' INTO ls_asset-type ls_asset-subtype.
    ls_asset-url          = iv_url.
    ls_asset-mime_name    = iv_mime_name.
    ls_asset-is_cacheable = iv_cachable.
    IF iv_base64 IS NOT INITIAL.
      ls_asset-content = zcl_abapgit_convert=>base64_to_xstring( iv_base64 ).
    ELSEIF iv_inline IS NOT INITIAL.
      ls_asset-content = zcl_abapgit_convert=>string_to_xstring( iv_inline ).
    ENDIF.

    APPEND ls_asset TO mt_asset_register.

  ENDMETHOD.


  METHOD zif_abapgit_gui_asset_manager~get_all_assets.

    FIELD-SYMBOLS <ls_a> LIKE LINE OF mt_asset_register.

    LOOP AT mt_asset_register ASSIGNING <ls_a>.
      APPEND load_asset( <ls_a> ) TO rt_assets.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_gui_asset_manager~get_asset.

    FIELD-SYMBOLS <ls_a> LIKE LINE OF mt_asset_register.

    READ TABLE mt_asset_register WITH KEY url = iv_url ASSIGNING <ls_a>.
    IF <ls_a> IS NOT ASSIGNED.
      zcx_abapgit_exception=>raise( |Cannot find GUI asset: { iv_url }| ).
    ENDIF.
    rs_asset = load_asset( <ls_a> ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_asset_manager~get_text_asset.

    DATA ls_asset TYPE zif_abapgit_gui_asset_manager~ty_web_asset.
    ls_asset = me->zif_abapgit_gui_asset_manager~get_asset( iv_url ).

    rv_asset = zcl_abapgit_convert=>xstring_to_string_utf8( ls_asset-content ).

  ENDMETHOD.


  METHOD finalize_registration.
    TYPES: BEGIN OF lty_asset_refs,
             asset   TYPE REF TO ty_asset_entry,
             content TYPE string,
           END OF lty_asset_refs.
    DATA: lt_css_variables    TYPE gty_css_var_tab,
          lt_css_vars_in_file TYPE gty_css_var_tab,
          lt_assets           TYPE STANDARD TABLE OF lty_asset_refs WITH DEFAULT KEY.
    FIELD-SYMBOLS: <ls_asset>        LIKE LINE OF mt_asset_register,
                   <ls_css_variable> LIKE LINE OF lt_css_vars_in_file,
                   <ls_asset_refs>   LIKE LINE OF lt_assets.

    " 1. Get all CSS assets and determine variable values. The latest variable definition overrides all previous ones.
    LOOP AT mt_asset_register ASSIGNING <ls_asset> WHERE type         = 'text'
                                                     AND subtype      = 'css'
                                                     AND is_cacheable = abap_true.
      IF <ls_asset>-content IS INITIAL.
        <ls_asset>-content = load_asset( <ls_asset> )-content.
      ENDIF.

      APPEND INITIAL LINE TO lt_assets ASSIGNING <ls_asset_refs>.
      GET REFERENCE OF <ls_asset> INTO <ls_asset_refs>-asset.
      <ls_asset_refs>-content = zcl_abapgit_convert=>xstring_to_string_utf8( <ls_asset>-content ).

      lt_css_vars_in_file = get_css_vars_in_string( <ls_asset_refs>-content ).

      LOOP AT lt_css_vars_in_file ASSIGNING <ls_css_variable>.
        INSERT <ls_css_variable> INTO TABLE lt_css_variables.
        IF sy-subrc <> 0.
          MODIFY TABLE lt_css_variables FROM <ls_css_variable>.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    " 2. Replace all variable usages by inlining the values.
    LOOP AT lt_assets ASSIGNING <ls_asset_refs>.
      LOOP AT lt_css_variables ASSIGNING <ls_css_variable>.
        REPLACE ALL OCCURRENCES OF |var(--{ <ls_css_variable>-name })|
                IN <ls_asset_refs>-content
                WITH <ls_css_variable>-value.
      ENDLOOP.
      <ls_asset_refs>-asset->content = zcl_abapgit_convert=>string_to_xstring_utf8( <ls_asset_refs>-content ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_css_vars_in_string.
    CONSTANTS: lc_root_pattern     TYPE string VALUE `:root\s*\{([^\}]*)\}`,
               lc_variable_pattern TYPE string VALUE `\-\-([\w\d-]+)\s*:\s*([^\n\r\s]*)\s*;`.
    DATA: lv_root     TYPE string,
          lo_matcher  TYPE REF TO cl_abap_matcher,
          lo_regex    TYPE REF TO cl_abap_regex,
          ls_variable LIKE LINE OF rt_variables.

    " Only the :root element may define variables for now

    FIND FIRST OCCURRENCE OF REGEX lc_root_pattern IN iv_string SUBMATCHES lv_root.
    IF sy-subrc = 0 AND lv_root IS NOT INITIAL.
      CREATE OBJECT lo_regex
        EXPORTING
          pattern = lc_variable_pattern.
      lo_matcher = lo_regex->create_matcher( text = lv_root ).
      WHILE lo_matcher->find_next( ) = abap_true.
        ls_variable-name = lo_matcher->get_submatch( 1 ).
        ls_variable-value = lo_matcher->get_submatch( 2 ).
        INSERT ls_variable INTO TABLE rt_variables.
        IF sy-subrc <> 0.
          MODIFY TABLE rt_variables FROM ls_variable.
        ENDIF.
      ENDWHILE.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
