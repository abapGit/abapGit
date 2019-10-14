CLASS zcl_abapgit_longtexts DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      serialize
        IMPORTING
          iv_object_name TYPE sobj_name
          iv_longtext_id TYPE dokil-id
          it_dokil       TYPE zif_abapgit_definitions=>tty_dokil
          io_xml         TYPE REF TO zcl_abapgit_xml_output
        RAISING
          zcx_abapgit_exception,

      deserialize
        IMPORTING
          io_xml             TYPE REF TO zcl_abapgit_xml_input
          iv_master_language TYPE langu
        RAISING
          zcx_abapgit_exception,

      delete
        IMPORTING
          iv_object_name TYPE sobj_name
          iv_longtext_id TYPE dokil-id
        RAISING
          zcx_abapgit_exception.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_longtext,
        dokil TYPE dokil,
        head  TYPE thead,
        lines TYPE tline_tab,
      END OF ty_longtext,
      tty_longtexts TYPE STANDARD TABLE OF ty_longtext
                         WITH NON-UNIQUE DEFAULT KEY.
    CONSTANTS:
      c_longtexts_name    TYPE string   VALUE 'LONGTEXTS' ##NO_TEXT,
      c_docu_state_active TYPE dokstate VALUE 'A' ##NO_TEXT.

ENDCLASS.



CLASS ZCL_ABAPGIT_LONGTEXTS IMPLEMENTATION.


  METHOD delete.

    DATA: lt_dokil TYPE zif_abapgit_definitions=>tty_dokil.
    FIELD-SYMBOLS: <ls_dokil> TYPE dokil.

    SELECT * FROM dokil
      INTO TABLE lt_dokil
      WHERE id = iv_longtext_id
      AND object = iv_longtext_id.

    LOOP AT lt_dokil ASSIGNING <ls_dokil>.

      CALL FUNCTION 'DOCU_DEL'
        EXPORTING
          id       = <ls_dokil>-id
          langu    = <ls_dokil>-langu
          object   = <ls_dokil>-object
          typ      = <ls_dokil>-typ
        EXCEPTIONS
          ret_code = 1
          OTHERS   = 2.

      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD deserialize.

    DATA: lt_longtexts     TYPE tty_longtexts,
          lv_no_masterlang TYPE dokil-masterlang.
    FIELD-SYMBOLS: <ls_longtext> TYPE ty_longtext.

    io_xml->read(
      EXPORTING
        iv_name = c_longtexts_name
      CHANGING
        cg_data = lt_longtexts ).

    LOOP AT lt_longtexts ASSIGNING <ls_longtext>.

      lv_no_masterlang = boolc( iv_master_language <> <ls_longtext>-dokil-langu ).

      CALL FUNCTION 'DOCU_UPDATE'
        EXPORTING
          head          = <ls_longtext>-head
          state         = c_docu_state_active
          typ           = <ls_longtext>-dokil-typ
          version       = <ls_longtext>-dokil-version
          no_masterlang = lv_no_masterlang
        TABLES
          line          = <ls_longtext>-lines.

    ENDLOOP.

  ENDMETHOD.


  METHOD serialize.

    DATA: ls_longtext  TYPE ty_longtext,
          lt_longtexts TYPE tty_longtexts,
          lt_dokil     TYPE zif_abapgit_definitions=>tty_dokil.

    FIELD-SYMBOLS: <ls_dokil> LIKE LINE OF lt_dokil.


    IF lines( it_dokil ) > 0.

      lt_dokil = it_dokil.

    ELSEIF iv_longtext_id IS NOT INITIAL.

      SELECT * FROM dokil
              INTO TABLE lt_dokil
              WHERE id     = iv_longtext_id
              AND   object = iv_object_name.

    ELSE.

      zcx_abapgit_exception=>raise( |serialize_longtexts parameter error| ).

    ENDIF.

    LOOP AT lt_dokil ASSIGNING <ls_dokil>
                     WHERE txtlines > 0.

      CLEAR: ls_longtext.

      ls_longtext-dokil = <ls_dokil>.

      CALL FUNCTION 'DOCU_READ'
        EXPORTING
          id      = <ls_dokil>-id
          langu   = <ls_dokil>-langu
          object  = <ls_dokil>-object
          typ     = <ls_dokil>-typ
          version = <ls_dokil>-version
        IMPORTING
          head    = ls_longtext-head
        TABLES
          line    = ls_longtext-lines.

      CLEAR: ls_longtext-head-tdfuser,
             ls_longtext-head-tdfreles,
             ls_longtext-head-tdfdate,
             ls_longtext-head-tdftime,
             ls_longtext-head-tdluser,
             ls_longtext-head-tdlreles,
             ls_longtext-head-tdldate,
             ls_longtext-head-tdltime.

      INSERT ls_longtext INTO TABLE lt_longtexts.

    ENDLOOP.

    io_xml->add( iv_name = c_longtexts_name
                 ig_data = lt_longtexts ).

  ENDMETHOD.
ENDCLASS.
