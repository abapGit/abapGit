CLASS zcl_abapgit_object_form DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

    METHODS constructor
      IMPORTING
        is_item     TYPE zif_abapgit_definitions=>ty_item
        iv_language TYPE spras.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: c_objectname_form    TYPE thead-tdobject VALUE 'FORM' ##NO_TEXT.
    CONSTANTS: c_objectname_tdlines TYPE thead-tdobject VALUE 'TDLINES' ##NO_TEXT.
    CONSTANTS: c_extension_xml      TYPE string         VALUE 'xml' ##NO_TEXT.
    DATA: mv_form_name  TYPE itcta-tdform.

    TYPES: BEGIN OF ty_s_form_data,
             form_header   TYPE itcta,
             text_header   TYPE thead,
             orig_language TYPE sy-langu,
             pages         TYPE STANDARD TABLE OF itctg WITH DEFAULT KEY,
             page_windows  TYPE STANDARD TABLE OF itcth WITH DEFAULT KEY,
             paragraphs    TYPE STANDARD TABLE OF itcdp WITH DEFAULT KEY,
             strings       TYPE STANDARD TABLE OF itcds WITH DEFAULT KEY,
             tabs          TYPE STANDARD TABLE OF itcdq WITH DEFAULT KEY,
             windows       TYPE STANDARD TABLE OF itctw WITH DEFAULT KEY,
           END OF ty_s_form_data,
           ty_t_form_data   TYPE STANDARD TABLE OF ty_s_form_data WITH DEFAULT KEY,
           ty_t_form_header TYPE STANDARD TABLE OF itcta WITH DEFAULT KEY,
           ty_s_form_header TYPE LINE OF ty_t_form_header,
           ty_t_text_header TYPE STANDARD TABLE OF thead WITH DEFAULT KEY,
           ty_s_text_header TYPE LINE OF ty_t_text_header,
           ty_t_lines       TYPE tline_tab.

    METHODS get_last_changes
      IMPORTING
        iv_form_name           TYPE zif_abapgit_definitions=>ty_item-obj_name
      RETURNING
        VALUE(rs_last_changed) TYPE ty_s_form_header.

    METHODS build_extra_from_header
      IMPORTING
        is_header        TYPE ty_s_form_header
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS build_extra_from_header_old
      IMPORTING
        is_header        TYPE ty_s_form_header
      RETURNING
        VALUE(rv_result) TYPE string.

    METHODS _save_form
      IMPORTING
        it_lines     TYPE ty_t_lines
      CHANGING
        cs_form_data TYPE ty_s_form_data.

    METHODS extract_tdlines
      IMPORTING
        is_form_data    TYPE ty_s_form_data
      RETURNING
        VALUE(rt_lines) TYPE ty_t_lines
      RAISING
        zcx_abapgit_exception.

    METHODS _clear_changed_fields
      CHANGING
        cs_form_data TYPE ty_s_form_data.

    METHODS compress_lines
      IMPORTING
        is_form_data TYPE ty_s_form_data
        it_lines     TYPE ty_t_lines
      RAISING
        zcx_abapgit_exception.

    METHODS find_form
      IMPORTING
        iv_object_name        TYPE zif_abapgit_definitions=>ty_item-obj_name
      RETURNING
        VALUE(rt_text_header) TYPE ty_t_text_header.

    METHODS _read_form
      IMPORTING
        is_text_header TYPE ty_s_text_header
      EXPORTING
        ev_form_found  TYPE abap_bool
        es_form_data   TYPE ty_s_form_data
        et_lines       TYPE ty_t_lines.

    METHODS _sort_tdlines_by_windows
      CHANGING
        ct_form_windows TYPE ty_s_form_data-windows
        ct_lines        TYPE ty_t_lines.

    METHODS order_check_and_insert
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS zcl_abapgit_object_form IMPLEMENTATION.


  METHOD build_extra_from_header.

    DATA: lv_tdspras TYPE laiso.

    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
      EXPORTING
        input  = is_header-tdspras
      IMPORTING
        output = lv_tdspras.

    rv_result = c_objectname_tdlines && '_' && lv_tdspras.

  ENDMETHOD.


  METHOD build_extra_from_header_old.
    rv_result = c_objectname_tdlines && '_' && is_header-tdspras.
  ENDMETHOD.


  METHOD compress_lines.

    DATA lv_string TYPE string.
    DATA li_xml TYPE REF TO zif_abapgit_xml_output.

    CREATE OBJECT li_xml TYPE zcl_abapgit_xml_output.
    li_xml->add( iv_name = c_objectname_tdlines
                 ig_data = it_lines ).
    lv_string = li_xml->render( ).
    IF lv_string IS NOT INITIAL.
      zif_abapgit_object~mo_files->add_string( iv_extra  =
                    build_extra_from_header( is_form_data-form_header )
                            iv_ext    = c_extension_xml
                            iv_string = lv_string ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_form_name = ms_item-obj_name.

  ENDMETHOD.


  METHOD extract_tdlines.

    DATA lv_string TYPE string.
    DATA li_xml TYPE REF TO zif_abapgit_xml_input.

    TRY.
        lv_string = zif_abapgit_object~mo_files->read_string( iv_extra =
                                   build_extra_from_header( is_form_data-form_header )
                                           iv_ext   = c_extension_xml ).
      CATCH zcx_abapgit_exception.

        lv_string = zif_abapgit_object~mo_files->read_string( iv_extra =
                               build_extra_from_header_old( is_form_data-form_header )
                                           iv_ext   = c_extension_xml ).

    ENDTRY.

    CREATE OBJECT li_xml TYPE zcl_abapgit_xml_input EXPORTING iv_xml = lv_string.
    li_xml->read( EXPORTING iv_name = c_objectname_tdlines
                  CHANGING  cg_data = rt_lines ).

  ENDMETHOD.


  METHOD find_form.

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
        selections    = rt_text_header
      EXCEPTIONS
        OTHERS        = 1 ##FM_SUBRC_OK.  "#EC CI_SUBRC

  ENDMETHOD.


  METHOD get_last_changes.

    DATA: lv_form_name         TYPE thead-tdform.

    CLEAR rs_last_changed.

    lv_form_name = iv_form_name.

    CALL FUNCTION 'READ_FORM'
      EXPORTING
        form             = lv_form_name
        read_only_header = abap_true
      IMPORTING
        form_header      = rs_last_changed.

  ENDMETHOD.


  METHOD order_check_and_insert.

    DATA: lv_order TYPE e071k-trkorr.

    CALL FUNCTION 'SAPSCRIPT_ORDER_CHECK'
      EXPORTING
        objecttype           = ms_item-obj_type
        form                 = mv_form_name
      EXCEPTIONS
        invalid_input        = 1
        object_locked        = 2
        object_not_available = 3
        OTHERS               = 4.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'SAPSCRIPT_ORDER_INSERT'
      EXPORTING
        objecttype     = ms_item-obj_type
        form           = mv_form_name
        masterlang     = mv_language
      CHANGING
        order          = lv_order
      EXCEPTIONS
        invalid_input  = 1
        order_canceled = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA: ls_last_changed TYPE ty_s_form_header.

    ls_last_changed = get_last_changes( ms_item-obj_name ).

    IF ls_last_changed-tdluser IS NOT INITIAL.
      rv_user = ls_last_changed-tdluser.
    ELSE.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    CALL FUNCTION 'DELETE_FORM'
      EXPORTING
        form     = mv_form_name
        language = '*'.

    order_check_and_insert( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lt_form_data            TYPE ty_t_form_data.
    DATA: lt_lines TYPE ty_t_lines.
    FIELD-SYMBOLS: <ls_form_data> TYPE LINE OF ty_t_form_data.

    io_xml->read( EXPORTING iv_name = c_objectname_form
                  CHANGING  cg_data = lt_form_data ).

    LOOP AT lt_form_data ASSIGNING <ls_form_data>.

      lt_lines = extract_tdlines( <ls_form_data> ).

      _save_form( EXPORTING it_lines     = lt_lines
                  CHANGING  cs_form_data = <ls_form_data> ).

    ENDLOOP.

    CALL FUNCTION 'SAPSCRIPT_DELETE_LOAD'
      EXPORTING
        delete = abap_true
        form   = '*'
        write  = space.

    tadir_insert( iv_package ).

    order_check_and_insert( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    CALL FUNCTION 'READ_FORM'
      EXPORTING
        form             = mv_form_name
        read_only_header = abap_true
      IMPORTING
        found            = rv_bool.

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
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    DATA: lv_object TYPE seqg3-garg.

    " example lock entry
    "'001FORM      ZTEST_SAPSCRIPT                                                       TXT'
    lv_object = |{ sy-mandt }{ ms_item-obj_type }      { ms_item-obj_name }|.
    OVERLAY lv_object WITH '                                                                                   '.
    lv_object = lv_object && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESSFORM'
                                            iv_argument    = lv_object ).


  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    DATA: lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPMSSCF'.
    <ls_bdcdata>-dynpro   = '1102'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=SHOW'.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'RSSCF-TDFORM'.
    <ls_bdcdata>-fval = ms_item-obj_name.

    zcl_abapgit_ui_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SE71'
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

    DATA: lt_form_data              TYPE ty_t_form_data.
    DATA: ls_form_data              TYPE ty_s_form_data.
    DATA: lt_text_header            TYPE ty_t_text_header.
    DATA: lt_lines                  TYPE ty_t_lines.
    DATA: lv_form_found             TYPE abap_bool.
    FIELD-SYMBOLS: <ls_text_header> LIKE LINE OF lt_text_header.

    lt_text_header = find_form( ms_item-obj_name ).

    LOOP AT lt_text_header ASSIGNING <ls_text_header>.
      CLEAR lt_lines.
      CLEAR ls_form_data.

      _read_form( EXPORTING is_text_header = <ls_text_header>
                  IMPORTING ev_form_found = lv_form_found
                            es_form_data  = ls_form_data
                            et_lines      = lt_lines ).

      IF lv_form_found = abap_true.

        _clear_changed_fields( CHANGING cs_form_data = ls_form_data ).

        compress_lines( is_form_data = ls_form_data
                        it_lines     = lt_lines ).

        INSERT ls_form_data INTO TABLE lt_form_data.

      ENDIF.

    ENDLOOP.

    IF lt_form_data IS NOT INITIAL.

      io_xml->add( iv_name = c_objectname_form
                   ig_data = lt_form_data ).

    ENDIF.

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

    _sort_tdlines_by_windows( CHANGING ct_form_windows  = es_form_data-windows
                                       ct_lines         = et_lines ).

    es_form_data-form_header-tdversion = '00001'.
    es_form_data-text_header-tdversion = '00001'.

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
        OTHERS    = 1 ##FM_SUBRC_OK.                                                   "#EC CI_SUBRC

  ENDMETHOD.


  METHOD _sort_tdlines_by_windows.
    DATA lt_lines        TYPE ty_t_lines.
    DATA ls_lines        LIKE LINE OF lt_lines.
    DATA ls_form_windows LIKE LINE OF ct_form_windows.
    DATA lv_elt_windows  TYPE tdformat VALUE '/W'.
    DATA lv_firstloop    TYPE abap_bool.

    lt_lines = ct_lines.
    CLEAR ct_lines.

    SORT ct_form_windows BY tdwindow.

    LOOP AT ct_form_windows INTO ls_form_windows.
      lv_firstloop = abap_true.
      READ TABLE lt_lines INTO ls_lines WITH KEY tdformat = lv_elt_windows
                                                 tdline   = ls_form_windows-tdwindow.
      IF sy-subrc <> 0.
        CONTINUE. " current loop
      ENDIF.
      LOOP AT lt_lines INTO ls_lines FROM sy-tabix.
        IF lv_firstloop = abap_false AND
           ls_lines-tdformat = lv_elt_windows.
          EXIT.
        ENDIF.
        APPEND ls_lines TO ct_lines.
        lv_firstloop = abap_false.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
