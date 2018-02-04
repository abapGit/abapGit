CLASS zcl_abapgit_object_form DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PRIVATE SECTION.
    CONSTANTS: c_objectname_form    TYPE thead-tdobject VALUE 'FORM' ##NO_TEXT.
    CONSTANTS: c_objectname_tdlines TYPE thead-tdobject VALUE 'TDLINES' ##NO_TEXT.
    CONSTANTS: c_extension_xml      TYPE string         VALUE 'xml' ##NO_TEXT.

    TYPES: BEGIN OF tys_form_data,
             form_header   TYPE itcta,
             text_header   TYPE thead,
             orig_language TYPE sy-langu,
             pages         TYPE STANDARD TABLE OF itctg WITH DEFAULT KEY,
             page_windows  TYPE STANDARD TABLE OF itcth WITH DEFAULT KEY,
             paragraphs    TYPE STANDARD TABLE OF itcdp WITH DEFAULT KEY,
             strings       TYPE STANDARD TABLE OF itcds WITH DEFAULT KEY,
             tabs          TYPE STANDARD TABLE OF itcdq WITH DEFAULT KEY,
             windows       TYPE STANDARD TABLE OF itctw WITH DEFAULT KEY,
           END OF tys_form_data,
           tyt_form_data   TYPE STANDARD TABLE OF tys_form_data WITH DEFAULT KEY,
           tyt_form_header TYPE STANDARD TABLE OF itcta WITH DEFAULT KEY,
           tys_form_header TYPE LINE OF tyt_form_header,
           tyt_text_header TYPE STANDARD TABLE OF thead WITH DEFAULT KEY,
           tys_text_header TYPE LINE OF tyt_text_header,
           tyt_lines       TYPE tline_tab.

    METHODS _get_last_changes
      IMPORTING
        iv_form_name           TYPE zif_abapgit_definitions=>ty_item-obj_name
      RETURNING
        VALUE(es_last_changed) TYPE tys_form_header.

    METHODS _build_extra_from_header
      IMPORTING
        ls_header       TYPE tys_form_header
      RETURNING
        VALUE(r_result) TYPE string.

    METHODS _save_form
      IMPORTING
        it_lines     TYPE zcl_abapgit_object_form=>tyt_lines
      CHANGING
        cs_form_data TYPE zcl_abapgit_object_form=>tys_form_data.

    METHODS _extract_tdlines
      IMPORTING
        is_form_data    TYPE zcl_abapgit_object_form=>tys_form_data
      RETURNING
        VALUE(et_lines) TYPE zcl_abapgit_object_form=>tyt_lines
      RAISING
        zcx_abapgit_exception.

    METHODS _clear_changed_fields
      CHANGING
        cs_form_data TYPE zcl_abapgit_object_form=>tys_form_data.

    METHODS _compress_lines
      IMPORTING
        is_form_data TYPE zcl_abapgit_object_form=>tys_form_data
        it_lines     TYPE zcl_abapgit_object_form=>tyt_lines
      RAISING
        zcx_abapgit_exception.

    METHODS _find_form
      IMPORTING
        iv_object_name        TYPE zif_abapgit_definitions=>ty_item-obj_name
      RETURNING
        VALUE(et_text_header) TYPE zcl_abapgit_object_form=>tyt_text_header.

    METHODS _read_form
      IMPORTING
        is_text_header TYPE zcl_abapgit_object_form=>tys_text_header
      EXPORTING
        ev_form_found  TYPE flag
        es_form_data   TYPE zcl_abapgit_object_form=>tys_form_data
        et_lines       TYPE zcl_abapgit_object_form=>tyt_lines.

ENDCLASS.

CLASS zcl_abapgit_object_form IMPLEMENTATION.

  METHOD zif_abapgit_object~has_changed_since.

    DATA: ls_last_changed    TYPE tys_form_header.
    DATA: lv_last_changed_ts TYPE timestamp.

    ls_last_changed = _get_last_changes( ms_item-obj_name ).

    CONVERT DATE ls_last_changed-tdldate TIME ls_last_changed-tdltime
            INTO TIME STAMP lv_last_changed_ts TIME ZONE sy-zonlo.

    rv_changed = boolc( sy-subrc <> 0 OR lv_last_changed_ts > iv_timestamp ).

  ENDMETHOD.

  METHOD zif_abapgit_object~changed_by.

    DATA: ls_last_changed TYPE tys_form_header.

    ls_last_changed = _get_last_changes( ms_item-obj_name ).

    IF ls_last_changed-tdluser IS NOT INITIAL.
      rv_user = ls_last_changed-tdluser.
    ELSE.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.

  METHOD zif_abapgit_object~get_metadata.

    rs_metadata = get_metadata( ).
    rs_metadata-delete_tadir = abap_true.

  ENDMETHOD.

  METHOD zif_abapgit_object~exists.

    DATA: lv_form_name TYPE thead-tdform.

    lv_form_name = ms_item-obj_name.

    CALL FUNCTION 'READ_FORM'
      EXPORTING
        form             = lv_form_name
        read_only_header = abap_true
      IMPORTING
        found            = rv_bool.

  ENDMETHOD.

  METHOD zif_abapgit_object~jump.

    DATA: lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPMSSCF' ##NO_TEXT.
    <ls_bdcdata>-dynpro   = '1102' ##NO_TEXT.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE' ##NO_TEXT.
    <ls_bdcdata>-fval = '=SHOW' ##NO_TEXT.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'RSSCF-TDFORM' ##NO_TEXT.
    <ls_bdcdata>-fval = ms_item-obj_name.

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      STARTING NEW TASK 'GIT'
      EXPORTING
        tcode     = 'SE71'
        mode_val  = 'E'
      TABLES
        using_tab = lt_bdcdata
      EXCEPTIONS
        OTHERS    = 1
        ##fm_subrc_ok.                                                   "#EC CI_SUBRC

  ENDMETHOD.

  METHOD zif_abapgit_object~delete.

    DATA: lv_name TYPE itcta-tdform.

    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DELETE_FORM'
      EXPORTING
        form     = lv_name
        language = '*'.

  ENDMETHOD.

  METHOD zif_abapgit_object~serialize.

    DATA: lt_form_data              TYPE tyt_form_data.
    DATA: ls_form_data              TYPE tys_form_data.
    DATA: lt_text_header            TYPE tyt_text_header.
    DATA: lt_lines                  TYPE tyt_lines.
*    DATA: lo_xml                    TYPE REF TO zcl_abapgit_xml_output.
    DATA: lv_form_found             TYPE flag.
    FIELD-SYMBOLS: <ls_text_header> LIKE LINE OF lt_text_header.

    lt_text_header = _find_form( ms_item-obj_name ).

    LOOP AT lt_text_header ASSIGNING <ls_text_header>.
      CLEAR lt_lines.
      CLEAR ls_form_data.
*      FREE lo_xml.

      _read_form( EXPORTING is_text_header = <ls_text_header>
                  IMPORTING ev_form_found = lv_form_found
                            es_form_data  = ls_form_data
                            et_lines      = lt_lines ).

      IF lv_form_found = abap_true.

        _clear_changed_fields( CHANGING cs_form_data = ls_form_data ).

        _compress_lines( is_form_data = ls_form_data
                         it_lines     = lt_lines ).

        INSERT ls_form_data INTO TABLE lt_form_data.

      ENDIF.

    ENDLOOP.

    IF lt_form_data IS NOT INITIAL.

      io_xml->add( iv_name = c_objectname_form
                   ig_data = lt_form_data ).

    ENDIF.

  ENDMETHOD.

  METHOD zif_abapgit_object~deserialize.

    DATA: lt_form_data            TYPE tyt_form_data.
    DATA: lt_lines                TYPE tyt_lines.
    FIELD-SYMBOLS: <ls_form_data> TYPE LINE OF tyt_form_data.

    io_xml->read( EXPORTING iv_name = c_objectname_form
                  CHANGING  cg_data = lt_form_data ).

    LOOP AT lt_form_data ASSIGNING <ls_form_data>.

      lt_lines = _extract_tdlines( <ls_form_data> ).

      _save_form( EXPORTING it_lines     = lt_lines
                  CHANGING  cs_form_data = <ls_form_data> ).

    ENDLOOP.

    CALL FUNCTION 'SAPSCRIPT_DELETE_LOAD'
      EXPORTING
        delete = abap_true
        form   = '*'
        write  = space.

    tadir_insert( iv_package ).

  ENDMETHOD.

  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
  ENDMETHOD.

  METHOD _build_extra_from_header.
    r_result = c_objectname_tdlines && '_' && ls_header-tdspras.
  ENDMETHOD.

  METHOD _get_last_changes.

    DATA: lv_form_name         TYPE thead-tdform.

    CLEAR es_last_changed.

    lv_form_name = iv_form_name.

    CALL FUNCTION 'READ_FORM'
      EXPORTING
        form             = lv_form_name
        read_only_header = abap_true
      IMPORTING
        form_header      = es_last_changed.

  ENDMETHOD.


  METHOD _save_form.

    CALL FUNCTION 'SAVE_FORM'
      EXPORTING
        form_header  = cs_form_data-form_header
      TABLES
        form_lines   = it_lines
        pages        = cs_form_data-pages
        page_windows = cs_form_data-page_windows
        paragraphs   = cs_form_data-paragraphs
        strings      = cs_form_data-strings
        tabs         = cs_form_data-tabs
        windows      = cs_form_data-windows.

    CALL FUNCTION 'SAPSCRIPT_CHANGE_OLANGUAGE'
      EXPORTING
        forced    = abap_true
        name      = cs_form_data-text_header-tdname
        object    = cs_form_data-text_header-tdobject
        olanguage = cs_form_data-orig_language
      EXCEPTIONS
        OTHERS    = 1
        ##fm_subrc_ok.                                                   "#EC CI_SUBRC

  ENDMETHOD.


  METHOD _extract_tdlines.

    DATA lv_string TYPE string.
    DATA lo_xml TYPE REF TO zcl_abapgit_xml_input.

    lv_string = mo_files->read_string( iv_extra =
                               _build_extra_from_header( is_form_data-form_header )
                                       iv_ext   = c_extension_xml ).

    CREATE OBJECT lo_xml EXPORTING iv_xml = lv_string.
    lo_xml->read( EXPORTING iv_name = c_objectname_tdlines
                  CHANGING  cg_data = et_lines ).

  ENDMETHOD.


  METHOD _clear_changed_fields.

    CLEAR: cs_form_data-form_header-tdfuser,
           cs_form_data-form_header-tdfdate,
           cs_form_data-form_header-tdftime,
           cs_form_data-form_header-tdfreles,
           cs_form_data-form_header-tdluser,
           cs_form_data-form_header-tdldate,
           cs_form_data-form_header-tdltime,
           cs_form_data-form_header-tdlreles.
    CLEAR: cs_form_data-text_header-tdfuser,
           cs_form_data-text_header-tdfdate,
           cs_form_data-text_header-tdftime,
           cs_form_data-text_header-tdfreles,
           cs_form_data-text_header-tdluser,
           cs_form_data-text_header-tdldate,
           cs_form_data-text_header-tdltime,
           cs_form_data-text_header-tdlreles.

  ENDMETHOD.


  METHOD _compress_lines.

    DATA lv_string TYPE string.
    DATA lo_xml TYPE REF TO zcl_abapgit_xml_output.

    CREATE OBJECT lo_xml.
    lo_xml->add( iv_name = c_objectname_tdlines
                 ig_data = it_lines ).
    lv_string = lo_xml->render( ).
    IF lv_string IS NOT INITIAL.
      mo_files->add_string( iv_extra  =
                    _build_extra_from_header( is_form_data-form_header )
                            iv_ext    = c_extension_xml
                            iv_string = lv_string ).
    ENDIF.

  ENDMETHOD.


  METHOD _find_form.

    DATA: lv_text_name TYPE thead-tdname.

    lv_text_name = iv_object_name.

    CALL FUNCTION 'SELECT_TEXT'
      EXPORTING
        database_only = abap_true
        id            = 'TXT'
        language      = '*'
        name          = lv_text_name
        object        = c_objectname_form
      TABLES
        selections    = et_text_header
      EXCEPTIONS
        OTHERS        = 1
        ##fm_subrc_ok ##NO_TEXT.  "#EC CI_SUBRC

  ENDMETHOD.


  METHOD _read_form.

    CLEAR es_form_data.

    CALL FUNCTION 'READ_FORM'
      EXPORTING
        form         = is_text_header-tdform
        language     = is_text_header-tdspras
        status       = ' '
      IMPORTING
        form_header  = es_form_data-form_header
        found        = ev_form_found
        header       = es_form_data-text_header
        olanguage    = es_form_data-orig_language
      TABLES
        form_lines   = et_lines
        pages        = es_form_data-pages
        page_windows = es_form_data-page_windows
        paragraphs   = es_form_data-paragraphs
        strings      = es_form_data-strings
        tabs         = es_form_data-tabs
        windows      = es_form_data-windows.

  ENDMETHOD.

ENDCLASS.                    "zcl_abapgit_object_FORM IMPLEMENTATION
