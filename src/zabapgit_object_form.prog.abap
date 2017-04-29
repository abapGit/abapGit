*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_FORM
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS lcl_object_form DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_object_form DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

  PRIVATE SECTION.
    CONSTANTS: c_objectname_form TYPE thead-tdobject VALUE 'FORM' ##NO_TEXT.
    CONSTANTS: c_extension_xml   TYPE string         VALUE 'xml' ##NO_TEXT.
    CONSTANTS: c_tdid_default    TYPE thead-tdid     VALUE 'DEF' ##NO_TEXT.

    TYPES: tyt_header TYPE STANDARD TABLE OF thead WITH DEFAULT KEY.
    TYPES: tyt_lines  TYPE tline_tab.

    METHODS _get_last_changes
      IMPORTING iv_form_name           TYPE ty_item-obj_name
      RETURNING VALUE(es_last_changed) TYPE thead.
    METHODS _build_extr_from_header
      IMPORTING
        ls_header       TYPE LINE OF tyt_header
      RETURNING
        VALUE(r_result) TYPE string.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_object_form IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_object_form IMPLEMENTATION.

  METHOD lif_object~has_changed_since.

    DATA: ls_last_changed TYPE thead.
    DATA: lv_last_changed_ts TYPE timestamp.

    ls_last_changed = _get_last_changes( ms_item-obj_name ).

    CONVERT DATE ls_last_changed-tdldate TIME ls_last_changed-tdltime
            INTO TIME STAMP lv_last_changed_ts TIME ZONE sy-zonlo.

    rv_changed = boolc( sy-subrc <> 0 OR lv_last_changed_ts > iv_timestamp ).

  ENDMETHOD.

  METHOD lif_object~changed_by.

    DATA: ls_last_changed TYPE thead.

    ls_last_changed = _get_last_changes( ms_item-obj_name ).

    IF ls_last_changed-tdluser IS NOT INITIAL.
      rv_user = ls_last_changed-tdluser.
    ELSE.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.

  METHOD lif_object~get_metadata.

    rs_metadata = get_metadata( ).
    rs_metadata-delete_tadir = abap_true.

  ENDMETHOD.

  METHOD lif_object~exists.

    DATA: lv_form_name TYPE thead-tdform.

    lv_form_name = ms_item-obj_name.

    CALL FUNCTION 'READ_FORM'
      EXPORTING
        form  = lv_form_name
      IMPORTING
        found = rv_bool.

  ENDMETHOD.

  METHOD lif_object~jump.

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

  METHOD lif_object~delete.

    DATA: lv_name TYPE thead-tdname.

    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DELETE_TEXT'
      EXPORTING
        id       = 'TXT'
        language = '*'
        name     = lv_name
        object   = c_objectname_form
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from DELETE_TEXT TXT, FORM' ) ##NO_TEXT.
    ENDIF.

    lv_name+16(3) = '*  '.
    CALL FUNCTION 'DELETE_TEXT'
      EXPORTING
        id       = c_tdid_default
        language = '*'
        name     = lv_name
        object   = c_objectname_form
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from DELETE_TEXT DEF, FORM' ) ##NO_TEXT.
    ENDIF.

  ENDMETHOD.

  METHOD lif_object~serialize.

    DATA: lt_form              TYPE tyt_header.
    DATA: ls_header            TYPE LINE OF tyt_header.
    DATA: lt_lines             TYPE tyt_lines.
    DATA: lv_name              TYPE thead-tdname.
    DATA: lt_header            TYPE tyt_header.
    DATA: lv_string            TYPE string.
    DATA: lo_xml               TYPE REF TO lcl_xml_output.
    FIELD-SYMBOLS: <ls_header> TYPE LINE OF tyt_header.

    lv_name = ms_item-obj_name.

    CALL FUNCTION 'SELECT_TEXT'
      EXPORTING
        database_only = abap_true
        id            = '*'
        language      = '*'
        name          = lv_name
        object        = c_objectname_form
      TABLES
        selections    = lt_header
      EXCEPTIONS
        OTHERS        = 1
        ##fm_subrc_ok.                                                   "#EC CI_SUBRC

    LOOP AT lt_header ASSIGNING <ls_header>.
      CLEAR ls_header.
      CLEAR lt_lines.
      FREE lo_xml.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id       = <ls_header>-tdid
          language = <ls_header>-tdspras
          name     = <ls_header>-tdname
          object   = <ls_header>-tdobject
        IMPORTING
          header   = ls_header
        TABLES
          lines    = lt_lines
        EXCEPTIONS
          OTHERS   = 1
          ##fm_subrc_ok.                                                   "#EC CI_SUBRC

      CLEAR: ls_header-tdfuser,
             ls_header-tdfdate,
             ls_header-tdftime,
             ls_header-tdfreles,
             ls_header-tdluser,
             ls_header-tdldate,
             ls_header-tdltime,
             ls_header-tdlreles.

      CREATE OBJECT lo_xml.
      lo_xml->add( iv_name = 'TLINES'
                   ig_data = lt_lines ).
      lv_string = lo_xml->render( ).
      IF lv_string IS NOT INITIAL.
        mo_files->add_string( iv_extra  = _build_extr_from_header( ls_header )
                              iv_ext    = c_extension_xml
                              iv_string = lv_string ).
      ENDIF.

      INSERT ls_header INTO TABLE lt_form.

    ENDLOOP.

    IF lt_form IS NOT INITIAL.
      io_xml->add( iv_name = c_objectname_form
                   ig_data = lt_form ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_object~deserialize.

    DATA: lt_form              TYPE tyt_header.
    DATA: lt_lines             TYPE tyt_lines.
    DATA: lv_string            TYPE string.
    DATA: lo_xml               TYPE REF TO lcl_xml_input.
    FIELD-SYMBOLS: <ls_header> TYPE LINE OF tyt_header.

    io_xml->read( EXPORTING iv_name = c_objectname_form
                  CHANGING  cg_data = lt_form ).

    LOOP AT lt_form ASSIGNING <ls_header>.

      lv_string = mo_files->read_string( iv_extra = _build_extr_from_header( <ls_header> )
                                         iv_ext   = c_extension_xml ).

      CREATE OBJECT lo_xml EXPORTING iv_xml = lv_string.
      lo_xml->read( EXPORTING iv_name = 'TLINES'
                    CHANGING  cg_data = lt_lines ).

      IF <ls_header>-tdid = c_tdid_default.

        CALL FUNCTION 'SAPSCRIPT_CHANGE_OLANGUAGE'
          EXPORTING
            forced    = abap_true
            name      = <ls_header>-tdname
            object    = <ls_header>-tdobject
            olanguage = <ls_header>-tdspras
          EXCEPTIONS
            OTHERS    = 1
            ##fm_subrc_ok.                                                   "#EC CI_SUBRC

      ENDIF.
      CALL FUNCTION 'SAVE_TEXT'
        EXPORTING
          header          = <ls_header>
          savemode_direct = abap_true
        TABLES
          lines           = lt_lines
        EXCEPTIONS
          OTHERS          = 1.
      IF sy-subrc <> 0.
        lcx_exception=>raise( 'error from SAVE_TEXT, FORM' ) ##NO_TEXT.
      ENDIF.

    ENDLOOP.

    CALL FUNCTION 'SAPSCRIPT_DELETE_LOAD'
      EXPORTING
        delete = abap_true
        form   = '*'
        write  = space.

    tadir_insert( iv_package ).

  ENDMETHOD.

  METHOD lif_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_null_comparison_result.
  ENDMETHOD.

  METHOD _build_extr_from_header.
    r_result = ls_header-tdid && '_' && ls_header-tdspras.
  ENDMETHOD.

  METHOD _get_last_changes.

    DATA: lv_name              TYPE thead-tdname.
    DATA: lt_header            TYPE tyt_header.
    FIELD-SYMBOLS: <ls_header> TYPE LINE OF tyt_header.

    CLEAR es_last_changed.

    lv_name = ms_item-obj_name.

    CALL FUNCTION 'SELECT_TEXT'
      EXPORTING
        database_only = abap_true
        id            = '*'
        language      = '*'
        name          = lv_name
        object        = c_objectname_form
      TABLES
        selections    = lt_header
      EXCEPTIONS
        OTHERS        = 1
        ##fm_subrc_ok.                                                   "#EC CI_SUBRC

    LOOP AT lt_header ASSIGNING <ls_header>.

      IF <ls_header>-tdldate > es_last_changed-tdldate OR
         <ls_header>-tdldate = es_last_changed-tdldate AND
         <ls_header>-tdltime > es_last_changed-tdltime.
        es_last_changed = <ls_header>.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.                    "lcl_object_FORM IMPLEMENTATION
