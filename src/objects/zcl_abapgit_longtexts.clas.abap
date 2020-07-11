CLASS zcl_abapgit_longtexts DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_factory .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_longtexts .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_longtext,
        dokil TYPE dokil,
        head  TYPE thead,
        lines TYPE tline_tab,
      END OF ty_longtext .
    TYPES:
      tty_longtexts TYPE STANDARD TABLE OF ty_longtext
                           WITH NON-UNIQUE DEFAULT KEY .

    METHODS read
      IMPORTING
        !iv_object_name      TYPE sobj_name
        !iv_longtext_id      TYPE dokil-id
        !it_dokil            TYPE zif_abapgit_definitions=>tty_dokil
        !iv_master_lang_only TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_longtexts)  TYPE tty_longtexts
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.

    CONSTANTS c_docu_state_active TYPE dokstate VALUE 'A' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_ABAPGIT_LONGTEXTS IMPLEMENTATION.


  METHOD read.

    DATA: ls_longtext TYPE ty_longtext,
          lt_dokil    TYPE zif_abapgit_definitions=>tty_dokil.

    FIELD-SYMBOLS: <ls_dokil> LIKE LINE OF lt_dokil.

    IF lines( it_dokil ) > 0.

      lt_dokil = it_dokil.

    ELSEIF iv_longtext_id IS NOT INITIAL.
      IF iv_master_lang_only = abap_true.
        SELECT * FROM dokil
                 INTO TABLE lt_dokil
                 WHERE id     = iv_longtext_id
                 AND   object = iv_object_name
                 AND masterlang = abap_true
                 ORDER BY PRIMARY KEY.
      ELSE.
        SELECT * FROM dokil
                 INTO TABLE lt_dokil
                 WHERE id     = iv_longtext_id
                 AND   object = iv_object_name
                 ORDER BY PRIMARY KEY.
      ENDIF.
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

      INSERT ls_longtext INTO TABLE rt_longtexts.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_longtexts~changed_by.

    DATA: lt_longtexts TYPE tty_longtexts.
    FIELD-SYMBOLS: <ls_longtext> TYPE ty_longtext.

    lt_longtexts = read( iv_object_name = iv_object_name
                         iv_longtext_id = iv_longtext_id
                         it_dokil       = it_dokil ).

    READ TABLE lt_longtexts INDEX 1 ASSIGNING <ls_longtext>.
    IF sy-subrc = 0.
      rv_user = <ls_longtext>-head-tdluser.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_longtexts~delete.

    DATA: lt_dokil TYPE zif_abapgit_definitions=>tty_dokil.
    FIELD-SYMBOLS: <ls_dokil> TYPE dokil.

    SELECT * FROM dokil
      INTO TABLE lt_dokil
      WHERE id     = iv_longtext_id
      AND   object = iv_object_name.

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


  METHOD zif_abapgit_longtexts~deserialize.

    DATA: lt_longtexts     TYPE tty_longtexts,
          lv_no_masterlang TYPE dokil-masterlang.
    FIELD-SYMBOLS: <ls_longtext> TYPE ty_longtext.

    io_xml->read(
      EXPORTING
        iv_name = iv_longtext_name
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


  METHOD zif_abapgit_longtexts~serialize.

    DATA lt_longtexts TYPE tty_longtexts.
    DATA lt_dokil LIKE it_dokil.
    DATA lv_master_lang_only TYPE abap_bool.

    lt_dokil = it_dokil.
    lv_master_lang_only = io_xml->i18n_params( )-serialize_master_lang_only.
    IF lv_master_lang_only = abap_true.
      DELETE lt_dokil WHERE masterlang <> abap_true.
    ENDIF.

    lt_longtexts = read( iv_object_name = iv_object_name
                         iv_longtext_id = iv_longtext_id
                         it_dokil       = lt_dokil
                         iv_master_lang_only = lv_master_lang_only ).

    io_xml->add( iv_name = iv_longtext_name
                 ig_data = lt_longtexts ).

  ENDMETHOD.
ENDCLASS.
