*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_PROG
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_object_prog DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_prog DEFINITION INHERITING FROM lcl_objects_program FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

  PRIVATE SECTION.
    TYPES:  BEGIN OF ty_tpool_i18n,
              language TYPE langu,
              textpool TYPE textpool_table,
            END OF ty_tpool_i18n,
            tt_tpool_i18n TYPE STANDARD TABLE OF ty_tpool_i18n.

    METHODS:
    serialize_texts
      IMPORTING io_xml TYPE REF TO lcl_xml_output
      RAISING lcx_exception,
    deserialize_texts
      IMPORTING io_xml TYPE REF TO lcl_xml_input
      RAISING lcx_exception.

ENDCLASS.                    "lcl_object_prog DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_prog IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_prog IMPLEMENTATION.

  METHOD lif_object~has_changed_since.

    rv_changed = check_prog_changed_since(
      iv_program   = ms_item-obj_name
      iv_timestamp = iv_timestamp ).

  ENDMETHOD.  "lif_object~has_changed_since

  METHOD lif_object~changed_by.
    SELECT SINGLE unam FROM reposrc INTO rv_user
      WHERE progname = ms_item-obj_name
      AND r3state = 'A'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.                    "lif_object~changed_by

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: lv_progname TYPE reposrc-progname.

    SELECT SINGLE progname FROM reposrc INTO lv_progname
      WHERE progname = ms_item-obj_name
      AND r3state = 'A'.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'PROG'
        in_new_window = abap_true.

  ENDMETHOD.                    "jump

  METHOD lif_object~delete.

    DATA: lv_program LIKE sy-repid.


    lv_program = ms_item-obj_name.

    CALL FUNCTION 'RS_DELETE_PROGRAM'
      EXPORTING
        program            = lv_program
        suppress_popup     = abap_true
      EXCEPTIONS
        enqueue_lock       = 1
        object_not_found   = 2
        permission_failure = 3
        reject_deletion    = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from RS_DELETE_PROGRAM' ).
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD lif_object~serialize.

    serialize_program( io_xml   = io_xml
                       is_item  = ms_item
                       io_files = mo_files ).

    " Texts serializing (translations)
    serialize_texts( io_xml ).

  ENDMETHOD.                    "lif_serialize~serialize

  METHOD lif_object~deserialize.

    DATA: lv_program_name TYPE programm,
          ls_progdir      TYPE ty_progdir,
          lt_tpool        TYPE textpool_table,
          lt_dynpros      TYPE ty_dynpro_tt,
          lt_tpool_ext    TYPE ty_tpool_tt,
          ls_cua          TYPE ty_cua,
          lt_source       TYPE abaptxt255_tab.

    lv_program_name = ms_item-obj_name.

    lt_source = mo_files->read_abap( ).

    io_xml->read( EXPORTING iv_name = 'TPOOL'
                  CHANGING cg_data = lt_tpool_ext ).
    lt_tpool = read_tpool( lt_tpool_ext ).

    io_xml->read( EXPORTING iv_name = 'PROGDIR'
                  CHANGING cg_data  = ls_progdir ).
    deserialize_program( is_progdir = ls_progdir
                         it_source  = lt_source
                         it_tpool   = lt_tpool
                         iv_package = iv_package ).

    io_xml->read( EXPORTING iv_name = 'DYNPROS'
                  CHANGING cg_data  = lt_dynpros ).
    deserialize_dynpros( it_dynpros = lt_dynpros ).

    io_xml->read( EXPORTING iv_name = 'CUA'
                  CHANGING cg_data  = ls_cua ).
    deserialize_cua( iv_program_name = lv_program_name
                     is_cua = ls_cua ).

    " Texts deserializing (English)
    deserialize_textpool( iv_program = lv_program_name
                          it_tpool   = lt_tpool ).

    " Texts deserializing (translations)
    deserialize_texts( io_xml ).

  ENDMETHOD.                    "lif_serialize~deserialize

  METHOD lif_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_null_comparison_result.
  ENDMETHOD.                    "lif_object~compare_to_remote_version

  METHOD serialize_texts.

    DATA  lt_tpool_i18n TYPE tt_tpool_i18n.

    FIELD-SYMBOLS <tpool> LIKE LINE OF lt_tpool_i18n.

    " Table d010tinf stores info. on languages in which program is maintained
    " Select all active translations of program texts
    " Skip master language - it was already serialized
    SELECT DISTINCT language
      INTO CORRESPONDING FIELDS OF TABLE lt_tpool_i18n
      FROM d010tinf
      WHERE r3state = 'A'
      AND   prog = ms_item-obj_name
      AND   language <> mv_language.

    SORT lt_tpool_i18n BY language ASCENDING.
    LOOP AT lt_tpool_i18n ASSIGNING <tpool>.
      READ TEXTPOOL ms_item-obj_name
        LANGUAGE <tpool>-language
        INTO <tpool>-textpool.
    ENDLOOP.

    IF lines( lt_tpool_i18n ) > 0.
      io_xml->add( iv_name = 'I18N_TPOOL'
                   ig_data = lt_tpool_i18n ).
    ENDIF.

  ENDMETHOD.                    "serialize_texts

  METHOD deserialize_texts.

    DATA lt_tpool_i18n  TYPE tt_tpool_i18n.
    FIELD-SYMBOLS <tpool> LIKE LINE OF lt_tpool_i18n.

    io_xml->read( EXPORTING iv_name = 'I18N_TPOOL'
                  CHANGING  cg_data = lt_tpool_i18n ).

    LOOP AT lt_tpool_i18n ASSIGNING <tpool>.
      deserialize_textpool( iv_program  = ms_item-obj_name
                            iv_language = <tpool>-language
                            it_tpool    = <tpool>-textpool ).
    ENDLOOP.

  ENDMETHOD.                    "deserialize_texts

ENDCLASS.                    "lcl_object_prog IMPLEMENTATION
