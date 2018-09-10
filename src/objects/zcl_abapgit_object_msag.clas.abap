class zcl_abapgit_object_msag definition public inheriting from zcl_abapgit_objects_super final.

  public section.
    interfaces zif_abapgit_object.
    aliases mo_files for zif_abapgit_object~mo_files.

  private section.
    types: begin of ty_t100_texts,
             sprsl type t100-sprsl,
             msgnr type t100-msgnr,
             text  type t100-text,
           end of ty_t100_texts,
           tt_t100_texts type standard table of ty_t100_texts,
           tty_t100      type standard table of t100
                         with non-unique default key,
           begin of ty_longtext,
             dokil type dokil,
             head  type thead,
             lines type tline_tab,
           end of ty_longtext,
           tty_longtexts type standard table of ty_longtext
                              with non-unique default key.

    methods:
      serialize_texts
        importing io_xml type ref to zcl_abapgit_xml_output
        raising   zcx_abapgit_exception,
      deserialize_texts
        importing io_xml type ref to zcl_abapgit_xml_input
        raising   zcx_abapgit_exception,
      serialize_longtexts_msag
        importing it_t100 type zcl_abapgit_object_msag=>tty_t100
                  io_xml  type ref to zcl_abapgit_xml_output
        raising   zcx_abapgit_exception,
      erase_msgid importing message_id        type arbgb.

endclass.



class zcl_abapgit_object_msag implementation.


  method deserialize_texts.

    data: lv_msg_id     type rglif-message_id,
          ls_t100       type t100,
          lt_t100t      type table of t100t,
          lt_t100_texts type tt_t100_texts,
          lt_t100u      type table of t100u.

    field-symbols: <ls_t100_text> type ty_t100_texts.


    lv_msg_id = ms_item-obj_name.

    select * from t100u into table lt_t100u
      where arbgb = lv_msg_id order by primary key.     "#EC CI_GENBUFF

    io_xml->read( exporting iv_name = 'T100_TEXTS'
                  changing  cg_data = lt_t100_texts ).

    io_xml->read( exporting iv_name = 'T100T'
                  changing  cg_data = lt_t100t ).

    modify t100t from table lt_t100t.                     "#EC CI_SUBRC

    loop at lt_t100_texts assigning <ls_t100_text>.
      "check if message exists
      read table lt_t100u transporting no fields
        with key arbgb = lv_msg_id msgnr = <ls_t100_text>-msgnr binary search.
      check sy-subrc = 0. "if original message doesn't exist no translations added

      move-corresponding <ls_t100_text> to ls_t100.
      ls_t100-arbgb = lv_msg_id.
      modify t100 from ls_t100.                           "#EC CI_SUBRC
      if sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'MSAG: Table T100 modify failed' ).
      endif.
    endloop.

  endmethod.


  method serialize_longtexts_msag.

    data: lv_object  type dokhl-object,
          lt_objects type standard table of dokhl-object
                          with non-unique default key,
          lt_dokil   type zif_abapgit_definitions=>tty_dokil.

    field-symbols: <ls_t100>  type t100.

    if lines( it_t100 ) = 0.
      return.
    endif.

    loop at it_t100 assigning <ls_t100>.

      lv_object = <ls_t100>-arbgb && <ls_t100>-msgnr.
      insert lv_object into table lt_objects.

    endloop.

    select * from dokil
             into table lt_dokil
             for all entries in lt_objects
             where id     = 'NA'
             and   object = lt_objects-table_line.

    if lines( lt_dokil ) > 0.
      serialize_longtexts( io_xml   = io_xml
                           it_dokil = lt_dokil ).
    endif.

  endmethod.


  method serialize_texts.

    data: lv_msg_id     type rglif-message_id,
          lt_t100_texts type tt_t100_texts,
          lt_t100t      type table of t100t,
          lt_i18n_langs type table of langu.

    lv_msg_id = ms_item-obj_name.

    " Collect additional languages
    " Skip master lang - it has been already serialized
    select distinct sprsl as langu into table lt_i18n_langs
      from t100t
      where arbgb = lv_msg_id
      and   sprsl <> mv_language.       "#EC CI_BYPASS "#EC CI_GENBUFF.

    sort lt_i18n_langs ascending.

    if lines( lt_i18n_langs ) > 0.

      select * from t100t into corresponding fields of table lt_t100t
        where sprsl <> mv_language
        and arbgb = lv_msg_id.                          "#EC CI_GENBUFF

      select * from t100 into corresponding fields of table lt_t100_texts
        for all entries in lt_i18n_langs
        where sprsl = lt_i18n_langs-table_line
        and arbgb = lv_msg_id
        order by primary key.             "#EC CI_SUBRC "#EC CI_GENBUFF

      sort lt_t100t by sprsl ascending.
      sort lt_t100_texts by sprsl msgnr ascending.

      io_xml->add( iv_name = 'I18N_LANGS'
                   ig_data = lt_i18n_langs ).

      io_xml->add( iv_name = 'T100T'
                   ig_data = lt_t100t ).

      io_xml->add( iv_name = 'T100_TEXTS'
                   ig_data = lt_t100_texts ).

    endif.

  endmethod.


  method zif_abapgit_object~changed_by.

    select single lastuser from t100a into rv_user
      where arbgb = ms_item-obj_name.                   "#EC CI_GENBUFF
    if sy-subrc <> 0 or rv_user = ''.
      rv_user = c_user_unknown.
    endif.

  endmethod.


  method zif_abapgit_object~compare_to_remote_version.
    create object ro_comparison_result type zcl_abapgit_comparison_null.
  endmethod.


  method zif_abapgit_object~delete.
    data: lv_t100a          type t100a,
          lv_error          type abap_bool,
          frozen            type flag,
          message_id        type arbgb,
          trkey1            type trkey,
          lv_access_granted type abap_bool.

* parameter SUPPRESS_DIALOG doesnt exist in all versions of FM RS_DELETE_MESSAGE_ID
* replaced with a copy
    message_id = ms_item-obj_name.
    if ms_item-obj_name eq space.
      lv_error = abap_true."blank message id
    else.
      select single * from t100a into lv_t100a where arbgb = ms_item-obj_name.
      if sy-subrc ne 0.
        lv_error = abap_true."not found
      endif.
    endif.

    if lv_error = abap_false.
      clear frozen.
      call function 'RS_ACCESS_PERMISSION'
        exporting
          authority_check = 'X'
          global_lock     = 'X'
          mode            = 'MODIFY'
          object          = message_id
          object_class    = 'T100'
        importing
          frozen          = frozen
        exceptions
          others          = 1.

      if sy-subrc ne 0 or frozen ne space.
        lv_error = abap_true."can't access
      else.
        lv_access_granted = abap_true.
      endif.

    endif.

    if lv_error = abap_false.

      call function 'RS_CORR_INSERT'
        exporting
          global_lock        = 'X'
          object             = message_id
          object_class       = 'MSAG'
          mode               = 'D'
        importing
          transport_key      = trkey1
        exceptions
          cancelled          = 01
          permission_failure = 02.
      if sy-subrc ne 0.
        lv_error = abap_true."can't access
      endif.

    endif.

    if lv_error = abap_false.
      erase_msgid( message_id ).
      trkey1-sub_type = '*'.

    endif.

    if    lv_access_granted = abap_true.
      call function 'RS_ACCESS_PERMISSION'
        exporting
          mode         = 'FREE'
          object       = message_id
          object_class = 'T100'.

    endif.

    if lv_error = abap_true.
      zcx_abapgit_exception=>raise( 'Error from RS_DELETE_MESSAGE_ID' ).
    endif.

  endmethod.


  method zif_abapgit_object~deserialize.
* fm RPY_MESSAGE_ID_INSERT almost works, but not in older versions

    data: ls_t100a  type t100a,
          ls_t100t  type t100t,
          ls_t100u  type t100u,
          lt_t100   type table of t100,
          lt_before type table of t100u.

    field-symbols: <ls_t100> like line of lt_t100.


    io_xml->read( exporting iv_name = 'T100A'
                  changing cg_data = ls_t100a ).
    io_xml->read( exporting iv_name = 'T100'
                  changing cg_data = lt_t100 ).

    call function 'RS_CORR_INSERT'
      exporting
        global_lock         = abap_true
        devclass            = iv_package
        object              = ls_t100a-arbgb
        object_class        = 'T100'
        mode                = 'INSERT'
      exceptions
        cancelled           = 01
        permission_failure  = 02
        unknown_objectclass = 03.
    if sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error from RS_CORR_INSERT' ).
    endif.

    select * from t100u into table lt_before
      where arbgb = ls_t100a-arbgb order by msgnr. "#EC CI_GENBUFF "#EC CI_BYPASS

    loop at lt_t100 assigning <ls_t100>.
      delete lt_before where msgnr = <ls_t100>-msgnr.
      modify t100 from <ls_t100>.                         "#EC CI_SUBRC
      if sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'MSAG: Table T100 modify failed' ).
      endif.
      clear ls_t100u.
      move-corresponding <ls_t100> to ls_t100u ##enh_ok.
      ls_t100u-name    = sy-uname.
      ls_t100u-datum   = sy-datum.
      ls_t100u-selfdef = '3'.
      modify t100u from ls_t100u.                         "#EC CI_SUBRC
      if sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'MSAG: Table T100U modify failed' ).
      endif.
    endloop.

    ls_t100a-masterlang = mv_language.
    ls_t100a-lastuser = sy-uname.
    ls_t100a-respuser = sy-uname.
    ls_t100a-ldate = sy-datum.
    ls_t100a-ltime = sy-uzeit.
    modify t100a from ls_t100a.                           "#EC CI_SUBRC
    if sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'MSAG: Table T100A modify failed' ).
    endif.

    ls_t100t-sprsl = mv_language.
    ls_t100t-arbgb = ls_t100a-arbgb.
    ls_t100t-stext = ls_t100a-stext.
    modify t100t from ls_t100t.                           "#EC CI_SUBRC
    if sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'MSAG: Table T100T modify failed' ).
    endif.

    loop at lt_before into ls_t100u.
      delete from t100 where arbgb = ls_t100u-arbgb
        and msgnr = ls_t100u-msgnr.                       "#EC CI_SUBRC

      delete from t100u where arbgb = ls_t100u-arbgb
        and msgnr = ls_t100u-msgnr.                       "#EC CI_SUBRC
    endloop.

    deserialize_longtexts( io_xml ).

    deserialize_texts( io_xml = io_xml ).

  endmethod.


  method zif_abapgit_object~exists.

    data: lv_arbgb type t100a-arbgb.


    select single arbgb from t100a into lv_arbgb
      where arbgb = ms_item-obj_name.                   "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  endmethod.


  method zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  endmethod.


  method zif_abapgit_object~has_changed_since.
    rv_changed = abap_true.
  endmethod.


  method zif_abapgit_object~is_locked.

    data: lv_argument type seqg3-garg.

    lv_argument   = |{ ms_item-obj_name }|.
    overlay lv_argument with '                     '.
    lv_argument = lv_argument && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = |ES_MSGSI|
                                            iv_argument    = lv_argument ).

  endmethod.


  method zif_abapgit_object~jump.

    call function 'RS_TOOL_ACCESS'
      exporting
        operation     = 'SHOW'
        object_name   = ms_item-obj_name
        object_type   = 'MSAG'
        in_new_window = abap_true.

  endmethod.


  method zif_abapgit_object~serialize.

    data: lv_msg_id type rglif-message_id,
          ls_inf    type t100a,
          lt_source type tty_t100.


    lv_msg_id = ms_item-obj_name.

    select single * from t100a into ls_inf
      where arbgb = lv_msg_id.                          "#EC CI_GENBUFF
    if sy-subrc <> 0.
      return.
    endif.
    clear ls_inf-respuser.

    select * from t100 into table lt_source
      where sprsl = mv_language
      and arbgb = lv_msg_id
      order by primary key.               "#EC CI_SUBRC "#EC CI_GENBUFF

    clear: ls_inf-lastuser,
           ls_inf-ldate,
           ls_inf-ltime.

    io_xml->add( iv_name = 'T100A'
                 ig_data = ls_inf ).
    io_xml->add( ig_data = lt_source
                 iv_name = 'T100' ).

    serialize_longtexts_msag( it_t100 = lt_source
                              io_xml  = io_xml ).

    serialize_texts( io_xml ).

  endmethod.

  method erase_msgid.
    data: d_key_s type dokhl-object.

    clear d_key_s.
    call function 'DOCU_OBJECT_NAME_CONCATENATE'
      exporting
        docu_id  = 'NA'
        element  = message_id
        addition = '   '
      importing
        object   = d_key_s
      exceptions
        others   = 0.

    call function 'DOKU_DELETE_ALL'
      exporting
        doku_id                        = 'NA'
        doku_object                    = d_key_s
        generic_use                    = 'X'
        suppress_authority             = space
        suppress_enqueue               = space
        suppress_transport             = space
      exceptions
        header_without_text            = 01
        index_without_header           = 02
        no_authority_for_devclass_xxxx = 03
        no_docu_found                  = 04
        object_is_already_enqueued     = 05
        object_is_enqueued_by_corr     = 06
        user_break                     = 07.
    if sy-subrc ne 0 and sy-subrc ne 4.
      message s044(e4) with sy-msgid.
*    Dokumentation zu Nachrichten der Nachrichtenklasse & nicht gelöscht
    endif.
    delete from t100a where arbgb = message_id.
    if sy-subrc <> 0 and sy-subrc <> 4.
      message s018(e4) with 'T100A'.
*    Fehler beim Löschen eines Eintrags von &. Bitte überprüfen
    else.
      call function 'RS_TREE_OBJECT_PLACEMENT'
        exporting
          object    = message_id
          operation = 'DELETE'
          program   = space
          type      = 'CN'.
      delete from t100o where arbgb = message_id.
      delete from t100t where arbgb = message_id.       "#EC CI_NOFIRST
      delete from t100u where arbgb = message_id.
      delete from t100x where arbgb = message_id.
      delete from t100 where arbgb = message_id.
      if sy-subrc <> 0 and sy-subrc <> 4.
        message s018(e4) with 'T100'.
*     Fehler beim Löschen &. Bitte überprüfen
      else.
        message s022(e4) with message_id.
*     Nachrichtenklasse & wurde gelöscht
      endif.
    endif.


  endmethod.

endclass.
