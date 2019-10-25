CLASS zcl_abapgit_object_msag DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_t100_texts,
        sprsl TYPE t100-sprsl,
        msgnr TYPE t100-msgnr,
        text  TYPE t100-text,
      END OF ty_t100_texts .
    TYPES:
      tt_t100_texts TYPE STANDARD TABLE OF ty_t100_texts .
    TYPES:
      tty_t100      TYPE STANDARD TABLE OF t100
                           WITH NON-UNIQUE DEFAULT KEY .

    METHODS serialize_texts
      IMPORTING
        !io_xml TYPE REF TO zcl_abapgit_xml_output
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_texts
      IMPORTING
        !io_xml TYPE REF TO zcl_abapgit_xml_input
      RAISING
        zcx_abapgit_exception .
    METHODS serialize_longtexts_msag
      IMPORTING
        !it_t100 TYPE zcl_abapgit_object_msag=>tty_t100
        !io_xml  TYPE REF TO zcl_abapgit_xml_output
      RAISING
        zcx_abapgit_exception .
    METHODS delete_msgid
      IMPORTING
        !iv_message_id TYPE arbgb .
    METHODS free_access_permission
      IMPORTING
        !iv_message_id TYPE arbgb .
    METHODS delete_documentation
      IMPORTING
        !iv_message_id TYPE arbgb .
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_MSAG IMPLEMENTATION.


  METHOD delete_documentation.
    DATA: lv_key_s TYPE dokhl-object.

    CLEAR lv_key_s.
    CALL FUNCTION 'DOCU_OBJECT_NAME_CONCATENATE'
      EXPORTING
        docu_id  = 'NA'
        element  = iv_message_id
        addition = '   '
      IMPORTING
        object   = lv_key_s
      EXCEPTIONS
        OTHERS   = 0.

    CALL FUNCTION 'DOKU_DELETE_ALL'
      EXPORTING
        doku_id                        = 'NA'
        doku_object                    = lv_key_s
        generic_use                    = 'X'
        suppress_authority             = space
        suppress_enqueue               = space
        suppress_transport             = space
      EXCEPTIONS
        header_without_text            = 01
        index_without_header           = 02
        no_authority_for_devclass_xxxx = 03
        no_docu_found                  = 04
        object_is_already_enqueued     = 05
        object_is_enqueued_by_corr     = 06
        user_break                     = 07.

  ENDMETHOD.


  METHOD delete_msgid.

    delete_documentation( iv_message_id ).

    DELETE FROM t100a WHERE arbgb = iv_message_id.
    IF sy-subrc = 0 OR sy-subrc = 4.
      CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
        EXPORTING
          object    = iv_message_id
          operation = 'DELETE'
          program   = space
          type      = 'CN'.
      DELETE FROM t100o WHERE arbgb = iv_message_id.
      DELETE FROM t100t WHERE arbgb = iv_message_id.    "#EC CI_NOFIRST
      DELETE FROM t100u WHERE arbgb = iv_message_id.
      DELETE FROM t100x WHERE arbgb = iv_message_id.
      DELETE FROM t100 WHERE arbgb = iv_message_id.
    ENDIF.


  ENDMETHOD.


  METHOD deserialize_texts.

    DATA: lv_msg_id     TYPE rglif-message_id,
          ls_t100       TYPE t100,
          lt_t100t      TYPE TABLE OF t100t,
          lt_t100_texts TYPE tt_t100_texts,
          lt_t100u      TYPE TABLE OF t100u.

    FIELD-SYMBOLS: <ls_t100_text> TYPE ty_t100_texts.


    lv_msg_id = ms_item-obj_name.

    SELECT * FROM t100u INTO TABLE lt_t100u
      WHERE arbgb = lv_msg_id ORDER BY PRIMARY KEY.     "#EC CI_GENBUFF

    io_xml->read( EXPORTING iv_name = 'T100_TEXTS'
                  CHANGING  cg_data = lt_t100_texts ).

    io_xml->read( EXPORTING iv_name = 'T100T'
                  CHANGING  cg_data = lt_t100t ).

    MODIFY t100t FROM TABLE lt_t100t.                     "#EC CI_SUBRC

    LOOP AT lt_t100_texts ASSIGNING <ls_t100_text>.
      "check if message exists
      READ TABLE lt_t100u TRANSPORTING NO FIELDS
        WITH KEY arbgb = lv_msg_id msgnr = <ls_t100_text>-msgnr BINARY SEARCH.
      CHECK sy-subrc = 0. "if original message doesn't exist no translations added

      MOVE-CORRESPONDING <ls_t100_text> TO ls_t100.
      ls_t100-arbgb = lv_msg_id.
      MODIFY t100 FROM ls_t100.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'MSAG: Table T100 modify failed' ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD free_access_permission.
    CALL FUNCTION 'RS_ACCESS_PERMISSION'
      EXPORTING
        mode         = 'FREE'
        object       = iv_message_id
        object_class = 'T100'.
  ENDMETHOD.


  METHOD serialize_longtexts_msag.

    DATA: lv_object  TYPE dokhl-object,
          lt_objects TYPE STANDARD TABLE OF dokhl-object
                          WITH NON-UNIQUE DEFAULT KEY,
          lt_dokil   TYPE zif_abapgit_definitions=>tty_dokil,
          ls_dokil   LIKE LINE OF lt_dokil.

    FIELD-SYMBOLS: <ls_t100>  TYPE t100.

    IF lines( it_t100 ) = 0.
      RETURN.
    ENDIF.

    LOOP AT it_t100 ASSIGNING <ls_t100>.

      lv_object = <ls_t100>-arbgb && <ls_t100>-msgnr.
      INSERT lv_object INTO TABLE lt_objects.

    ENDLOOP.

    SELECT * FROM dokil
      INTO TABLE lt_dokil
      FOR ALL ENTRIES IN lt_objects
      WHERE id = 'NA'
      AND object = lt_objects-table_line.

    CLEAR ls_dokil-dokstate.
    MODIFY lt_dokil FROM ls_dokil TRANSPORTING dokstate WHERE dokstate IS NOT INITIAL.

    IF lines( lt_dokil ) > 0.
      serialize_longtexts( io_xml   = io_xml
                           it_dokil = lt_dokil ).
    ENDIF.

  ENDMETHOD.


  METHOD serialize_texts.

    DATA: lv_msg_id     TYPE rglif-message_id,
          lt_t100_texts TYPE tt_t100_texts,
          lt_t100t      TYPE TABLE OF t100t,
          lt_i18n_langs TYPE TABLE OF langu.

    lv_msg_id = ms_item-obj_name.

    IF io_xml->i18n_params( )-serialize_master_lang_only = abap_true.
      RETURN. " skip
    ENDIF.

    " Collect additional languages
    " Skip master lang - it has been already serialized
    SELECT DISTINCT sprsl AS langu INTO TABLE lt_i18n_langs
      FROM t100t
      WHERE arbgb = lv_msg_id
      AND sprsl <> mv_language.       "#EC CI_BYPASS "#EC CI_GENBUFF

    SORT lt_i18n_langs ASCENDING.

    IF lines( lt_i18n_langs ) > 0.

      SELECT * FROM t100t INTO CORRESPONDING FIELDS OF TABLE lt_t100t
        WHERE sprsl <> mv_language
        AND arbgb = lv_msg_id.                          "#EC CI_GENBUFF

      SELECT * FROM t100 INTO CORRESPONDING FIELDS OF TABLE lt_t100_texts
        FOR ALL ENTRIES IN lt_i18n_langs
        WHERE sprsl = lt_i18n_langs-table_line
        AND arbgb = lv_msg_id
        ORDER BY PRIMARY KEY.             "#EC CI_SUBRC "#EC CI_GENBUFF

      SORT lt_t100t BY sprsl ASCENDING.
      SORT lt_t100_texts BY sprsl msgnr ASCENDING.

      io_xml->add( iv_name = 'I18N_LANGS'
                   ig_data = lt_i18n_langs ).

      io_xml->add( iv_name = 'T100T'
                   ig_data = lt_t100t ).

      io_xml->add( iv_name = 'T100_TEXTS'
                   ig_data = lt_t100_texts ).

    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE lastuser FROM t100a INTO rv_user
      WHERE arbgb = ms_item-obj_name.                   "#EC CI_GENBUFF
    IF sy-subrc <> 0 OR rv_user = ''.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.
    DATA: lv_t100a          TYPE t100a,
          lv_frozen         TYPE abap_bool,
          lv_message_id     TYPE arbgb,
          lv_access_granted TYPE abap_bool.

* parameter SUPPRESS_DIALOG doesnt exist in all versions of FM RS_DELETE_MESSAGE_ID
* replaced with a copy
    lv_message_id = ms_item-obj_name.
    IF ms_item-obj_name = space.
      zcx_abapgit_exception=>raise( 'Error from (copy of) RS_DELETE_MESSAGE_ID' )."blank message id
    ENDIF.

    SELECT SINGLE * FROM t100a INTO lv_t100a WHERE arbgb = ms_item-obj_name.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error from (copy of) RS_DELETE_MESSAGE_ID' )."not found
    ENDIF.

    CLEAR lv_frozen.
    CALL FUNCTION 'RS_ACCESS_PERMISSION'
      EXPORTING
        authority_check = 'X'
        global_lock     = 'X'
        mode            = 'MODIFY'
        object          = lv_message_id
        object_class    = 'T100'
      IMPORTING
        frozen          = lv_frozen
      EXCEPTIONS
        OTHERS          = 1.

    IF sy-subrc <> 0 OR lv_frozen <> space.
      zcx_abapgit_exception=>raise( 'Error from (copy of) RS_DELETE_MESSAGE_ID' )."can't access
    ENDIF.

    lv_access_granted = abap_true.

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        global_lock        = 'X'
        object             = lv_message_id
        object_class       = 'MSAG'
        mode               = 'D'
        suppress_dialog    = abap_true
      EXCEPTIONS
        cancelled          = 01
        permission_failure = 02.

    IF sy-subrc <> 0.
      IF lv_access_granted = abap_true.
        free_access_permission( lv_message_id ).
      ENDIF.
      zcx_abapgit_exception=>raise( 'Error from (copy of) RS_DELETE_MESSAGE_ID' )."can't access
    ENDIF.

    delete_msgid( lv_message_id ).

    IF lv_access_granted = abap_true.
      free_access_permission( lv_message_id ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.
* fm RPY_MESSAGE_ID_INSERT almost works, but not in older versions

    DATA: ls_t100a  TYPE t100a,
          ls_t100t  TYPE t100t,
          ls_t100u  TYPE t100u,
          lt_t100   TYPE TABLE OF t100,
          lt_before TYPE TABLE OF t100u.

    FIELD-SYMBOLS: <ls_t100> LIKE LINE OF lt_t100.


    io_xml->read( EXPORTING iv_name = 'T100A'
                  CHANGING cg_data = ls_t100a ).
    io_xml->read( EXPORTING iv_name = 'T100'
                  CHANGING cg_data = lt_t100 ).

    corr_insert( iv_package ).

    SELECT * FROM t100u INTO TABLE lt_before
      WHERE arbgb = ls_t100a-arbgb ORDER BY msgnr. "#EC CI_GENBUFF "#EC CI_BYPASS

    LOOP AT lt_t100 ASSIGNING <ls_t100>.
      DELETE lt_before WHERE msgnr = <ls_t100>-msgnr.
      MODIFY t100 FROM <ls_t100>.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'MSAG: Table T100 modify failed' ).
      ENDIF.
      CLEAR ls_t100u.
      MOVE-CORRESPONDING <ls_t100> TO ls_t100u ##enh_ok.
      ls_t100u-name    = sy-uname.
      ls_t100u-datum   = sy-datum.
      ls_t100u-selfdef = '3'.
      MODIFY t100u FROM ls_t100u.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'MSAG: Table T100U modify failed' ).
      ENDIF.
    ENDLOOP.

    ls_t100a-masterlang = mv_language.
    ls_t100a-lastuser = sy-uname.
    ls_t100a-respuser = sy-uname.
    ls_t100a-ldate = sy-datum.
    ls_t100a-ltime = sy-uzeit.
    MODIFY t100a FROM ls_t100a.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'MSAG: Table T100A modify failed' ).
    ENDIF.

    ls_t100t-sprsl = mv_language.
    ls_t100t-arbgb = ls_t100a-arbgb.
    ls_t100t-stext = ls_t100a-stext.
    MODIFY t100t FROM ls_t100t.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'MSAG: Table T100T modify failed' ).
    ENDIF.

    LOOP AT lt_before INTO ls_t100u.
      DELETE FROM t100 WHERE arbgb = ls_t100u-arbgb
        AND msgnr = ls_t100u-msgnr.                       "#EC CI_SUBRC

      DELETE FROM t100u WHERE arbgb = ls_t100u-arbgb
        AND msgnr = ls_t100u-msgnr.                       "#EC CI_SUBRC
    ENDLOOP.

    deserialize_longtexts( io_xml ).

    deserialize_texts( io_xml ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_arbgb TYPE t100a-arbgb.


    SELECT SINGLE arbgb FROM t100a INTO lv_arbgb
      WHERE arbgb = ms_item-obj_name.                   "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
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

    DATA: lv_argument TYPE seqg3-garg.

    lv_argument   = |{ ms_item-obj_name }|.
    OVERLAY lv_argument WITH '                     '.
    lv_argument = lv_argument && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = |ES_MSGSI|
                                            iv_argument    = lv_argument ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'MSAG'
        in_new_window = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lv_msg_id TYPE rglif-message_id,
          ls_inf    TYPE t100a,
          lt_source TYPE tty_t100.


    lv_msg_id = ms_item-obj_name.

    SELECT SINGLE * FROM t100a INTO ls_inf
      WHERE arbgb = lv_msg_id.                          "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    CLEAR ls_inf-respuser.

    SELECT * FROM t100 INTO TABLE lt_source
      WHERE sprsl = mv_language
      AND arbgb = lv_msg_id
      ORDER BY PRIMARY KEY.               "#EC CI_SUBRC "#EC CI_GENBUFF

    CLEAR: ls_inf-lastuser,
           ls_inf-ldate,
           ls_inf-ltime.

    io_xml->add( iv_name = 'T100A'
                 ig_data = ls_inf ).
    io_xml->add( ig_data = lt_source
                 iv_name = 'T100' ).

    serialize_longtexts_msag( it_t100 = lt_source
                              io_xml  = io_xml ).

    serialize_texts( io_xml ).

  ENDMETHOD.
ENDCLASS.
