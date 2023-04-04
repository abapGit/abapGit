CLASS zcl_abapgit_object_tran DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object .
  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES:
      ty_param_values TYPE STANDARD TABLE OF rsparam
                                     WITH NON-UNIQUE DEFAULT KEY ,
      ty_tstca        TYPE STANDARD TABLE OF tstca
                                     WITH DEFAULT KEY.

    CONSTANTS:
      c_oo_program   TYPE c LENGTH 9 VALUE '\PROGRAM=' ##NO_TEXT,
      c_oo_class     TYPE c LENGTH 7 VALUE '\CLASS=' ##NO_TEXT,
      c_oo_method    TYPE c LENGTH 8 VALUE '\METHOD=' ##NO_TEXT,
      c_oo_tcode     TYPE tcode VALUE 'OS_APPLICATION' ##NO_TEXT,
      c_oo_frclass   TYPE c LENGTH 30 VALUE 'CLASS' ##NO_TEXT,
      c_oo_frmethod  TYPE c LENGTH 30 VALUE 'METHOD' ##NO_TEXT,
      c_oo_frupdtask TYPE c LENGTH 30 VALUE 'UPDATE_MODE' ##NO_TEXT,
      c_oo_synchron  TYPE c VALUE 'S' ##NO_TEXT,
      c_oo_asynchron TYPE c VALUE 'U' ##NO_TEXT,
      c_true         TYPE c VALUE 'X' ##NO_TEXT,
      c_false        TYPE c VALUE space ##NO_TEXT,
      BEGIN OF c_variant_type,
        dialog     TYPE rglif-docutype VALUE 'D' ##NO_TEXT,
        report     TYPE rglif-docutype VALUE 'R' ##NO_TEXT,
        variant    TYPE rglif-docutype VALUE 'V' ##NO_TEXT,
        parameters TYPE rglif-docutype VALUE 'P' ##NO_TEXT,
        object     TYPE rglif-docutype VALUE 'O' ##NO_TEXT,
      END OF c_variant_type.

    DATA:
      mt_bcdata TYPE STANDARD TABLE OF bdcdata .

    METHODS transaction_read
      IMPORTING
        iv_transaction TYPE tcode
      EXPORTING
        es_transaction TYPE tstc
        es_gui_attr    TYPE tstcc
      RAISING
        zcx_abapgit_exception.
    METHODS shift_param
      CHANGING
        !ct_rsparam TYPE s_param
        !cs_tstcp   TYPE tstcp .
    METHODS add_data
      IMPORTING
        !iv_fnam TYPE bdcdata-fnam
        !iv_fval TYPE clike .
    METHODS call_se93
      RAISING
        zcx_abapgit_exception .
    METHODS set_oo_parameters
      IMPORTING
        !it_rsparam TYPE s_param
      CHANGING
        !cs_rsstcd  TYPE rsstcd .
    METHODS split_parameters
      CHANGING
        !ct_rsparam TYPE s_param
        !cs_rsstcd  TYPE rsstcd
        !cs_tstcp   TYPE tstcp
        !cs_tstc    TYPE tstc .
    METHODS split_parameters_comp
      IMPORTING
        !ig_type  TYPE any
        !ig_param TYPE any
      CHANGING
        !cg_value TYPE any .
    METHODS serialize_texts
      IMPORTING
        !ii_xml TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_texts
      IMPORTING
        !ii_xml TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_oo_transaction
      IMPORTING
        !iv_package TYPE devclass
        !is_tstc    TYPE tstc
        !is_tstcc   TYPE tstcc
        !is_tstct   TYPE tstct
        !is_rsstcd  TYPE rsstcd
      RAISING
        zcx_abapgit_exception .
    METHODS save_authorizations
      IMPORTING
        iv_transaction    TYPE tstc-tcode
        it_authorizations TYPE ty_tstca
      RAISING
        zcx_abapgit_exception.
    METHODS clear_functiongroup_globals.
    METHODS is_variant_transaction IMPORTING is_tstcp                      TYPE tstcp
                                   RETURNING VALUE(rv_variant_transaction) TYPE abap_bool.
ENDCLASS.



CLASS zcl_abapgit_object_tran IMPLEMENTATION.


  METHOD add_data.

    DATA: ls_bcdata LIKE LINE OF mt_bcdata.

    ls_bcdata-fnam = iv_fnam.
    ls_bcdata-fval = iv_fval.
    APPEND ls_bcdata TO mt_bcdata.

  ENDMETHOD.


  METHOD call_se93.

    DATA: lt_message TYPE STANDARD TABLE OF bdcmsgcoll.
    DATA lv_msg TYPE string.

    FIELD-SYMBOLS: <ls_message> TYPE bdcmsgcoll.


    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      EXPORTING
        tcode     = 'SE93'
        mode_val  = 'N'
      TABLES
        using_tab = mt_bcdata
        mess_tab  = lt_message
      EXCEPTIONS
        OTHERS    = 1.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error deserializing { ms_item-obj_type } { ms_item-obj_name }| ).
    ENDIF.

    LOOP AT lt_message ASSIGNING <ls_message> WHERE msgtyp CA 'EAX'.
      MESSAGE ID <ls_message>-msgid
        TYPE <ls_message>-msgtyp
        NUMBER <ls_message>-msgnr
        WITH <ls_message>-msgv1 <ls_message>-msgv2 <ls_message>-msgv3 <ls_message>-msgv4
        INTO lv_msg.
      zcx_abapgit_exception=>raise_t100( ).
    ENDLOOP.

  ENDMETHOD.


  METHOD clear_functiongroup_globals.
    TYPES ty_param_vari TYPE abap_bool.

    DATA lt_error_list TYPE STANDARD TABLE OF rsmp_check WITH DEFAULT KEY.
    FIELD-SYMBOLS <lv_param_vari> TYPE ty_param_vari.

    " only way to clear global fields in function group
    CALL FUNCTION 'RS_TRANSACTION_INCONSISTENCIES'
      EXPORTING
        transaction_code = 'ZTHISTCODENEVEREXIST'
      TABLES
        error_list       = lt_error_list
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      "Expected - fine

      " but there is no other way to clear this field
      ASSIGN ('(SAPLSEUK)PARAM_VARI') TO <lv_param_vari>.
      IF sy-subrc = 0.
        CLEAR <lv_param_vari>.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD deserialize_oo_transaction.

    " You should remember that we don't use batch input just for fun,
    " but because FM RPY_TRANSACTION_INSERT doesn't support OO transactions.

    DATA: ls_bcdata  TYPE bdcdata.


    CLEAR mt_bcdata.

    ls_bcdata-program  = 'SAPLSEUK'.
    ls_bcdata-dynpro   = '0390'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO mt_bcdata.

    add_data( iv_fnam = 'TSTC-TCODE'
              iv_fval = is_tstc-tcode ).

    IF zif_abapgit_object~exists( ) = abap_true.

      add_data( iv_fnam = 'BDC_OKCODE'
                iv_fval = '=CHNG' ).

    ELSE.

      add_data( iv_fnam = 'BDC_OKCODE'
                iv_fval = '=ADD' ).

    ENDIF.

    ls_bcdata-program  = 'SAPLSEUK'.
    ls_bcdata-dynpro   = '0300'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO mt_bcdata.

    add_data( iv_fnam     = 'TSTCT-TTEXT'
              iv_fval     = is_tstct-ttext ).

    add_data( iv_fnam     = 'RSSTCD-S_CLASS'
              iv_fval     = 'X' ).

    add_data( iv_fnam = 'BDC_OKCODE'
              iv_fval = '=ENTR' ).

    ls_bcdata-program  = 'SAPLSEUK'.
    ls_bcdata-dynpro   = '0360'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO mt_bcdata.

    add_data( iv_fnam     = 'RSSTCD-S_TRFRAME'
              iv_fval     = is_rsstcd-s_trframe ).

    add_data( iv_fnam     = 'RSSTCD-S_UPDTASK'
              iv_fval     = is_rsstcd-s_updtask ).

    add_data( iv_fnam = 'BDC_OKCODE'
              iv_fval = '=TR_FRAMEWORK' ).

    ls_bcdata-program  = 'SAPLSEUK'.
    ls_bcdata-dynpro   = '0360'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO mt_bcdata.

    add_data( iv_fnam     = 'RSSTCD-CLASSNAME'
              iv_fval     = is_rsstcd-classname ).

    add_data( iv_fnam     = 'RSSTCD-METHOD'
              iv_fval     = is_rsstcd-method ).

    IF is_rsstcd-s_local IS NOT INITIAL.
      add_data( iv_fnam     = 'RSSTCD-S_LOCAL'
                iv_fval     = is_rsstcd-s_local ).
    ENDIF.

    IF is_rsstcd-s_updlok IS NOT INITIAL.
      add_data( iv_fnam     = 'RSSTCD-S_UPDLOK'
                iv_fval     = is_rsstcd-s_updlok ).
    ENDIF.

    add_data( iv_fnam     = 'TSTC-PGMNA'
              iv_fval     = is_tstc-pgmna ).

    IF is_tstcc-s_webgui = '2'.

      add_data( iv_fnam     = 'G_IAC_EWT'
                iv_fval     = abap_true ).

      add_data( iv_fnam = 'BDC_OKCODE'
                iv_fval = 'MAKE_PROFI' ).

      ls_bcdata-program  = 'SAPLSEUK'.
      ls_bcdata-dynpro   = '0360'.
      ls_bcdata-dynbegin = 'X'.
      APPEND ls_bcdata TO mt_bcdata.

    ELSEIF is_tstcc-s_webgui IS NOT INITIAL.

      add_data( iv_fnam     = 'TSTCC-S_WEBGUI'
                iv_fval     = is_tstcc-s_webgui ).

    ENDIF.

    IF is_tstcc-s_pervas IS NOT INITIAL.
      add_data( iv_fnam     = 'TSTCC-S_PERVAS'
                iv_fval     = is_tstcc-s_pervas ).
    ENDIF.

    IF is_tstcc-s_service IS NOT INITIAL.
      add_data( iv_fnam     = 'TSTCC-S_SERVICE'
                iv_fval     = is_tstcc-s_service ).
    ENDIF.

    IF is_tstcc-s_platin IS NOT INITIAL.
      add_data( iv_fnam     = 'TSTCC-S_PLATIN'
                iv_fval     = is_tstcc-s_platin ).
    ENDIF.

    IF is_tstcc-s_win32 IS NOT INITIAL.
      add_data( iv_fnam     = 'TSTCC-S_WIN32'
                iv_fval     = is_tstcc-s_win32 ).
    ENDIF.

    add_data( iv_fnam = 'BDC_OKCODE'
              iv_fval = '=WB_SAVE' ).

    ls_bcdata-program  = 'SAPLSTRD'.
    ls_bcdata-dynpro   = '0100'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO mt_bcdata.

    add_data( iv_fnam     = 'KO007-L_DEVCLASS'
              iv_fval     = iv_package ).

    add_data( iv_fnam = 'BDC_OKCODE'
              iv_fval = '=ADD' ).

    ls_bcdata-program  = 'BDC_OKCODE'.
    ls_bcdata-dynpro   = '0360'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO mt_bcdata.

    add_data( iv_fnam = 'BDC_OKCODE'
              iv_fval = '=WB_BACK' ).

    ls_bcdata-program  = 'BDC_OKCODE'.
    ls_bcdata-dynpro   = '0360'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO mt_bcdata.

    add_data( iv_fnam = 'BDC_OKCODE'
              iv_fval = '=WB_BACK' ).

    call_se93( ).

  ENDMETHOD.


  METHOD deserialize_texts.

    DATA lt_tpool_i18n TYPE TABLE OF tstct.

    FIELD-SYMBOLS <ls_tpool> LIKE LINE OF lt_tpool_i18n.

    " Read XML-files data
    ii_xml->read( EXPORTING iv_name = 'I18N_TPOOL'
                  CHANGING  cg_data = lt_tpool_i18n ).

    zcl_abapgit_lxe_texts=>trim_tab_w_saplang_by_iso(
      EXPORTING
        it_iso_filter = ii_xml->i18n_params( )-translation_languages
        iv_lang_field_name = 'SPRSL'
      CHANGING
        ct_tab = lt_tpool_i18n ).

    " Force t-code name (security reasons)
    LOOP AT lt_tpool_i18n ASSIGNING <ls_tpool>.
      <ls_tpool>-tcode = ms_item-obj_name.
    ENDLOOP.

    IF lines( lt_tpool_i18n ) > 0.
      MODIFY tstct FROM TABLE lt_tpool_i18n.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Update of t-code translations failed' ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD is_variant_transaction.

    rv_variant_transaction = boolc( is_tstcp-param(1) = '@' ).

  ENDMETHOD.


  METHOD save_authorizations.

    CONSTANTS: lc_hex_chk TYPE x VALUE '04'.
    DATA: ls_transaction TYPE tstc.

    transaction_read( EXPORTING iv_transaction = iv_transaction
                      IMPORTING es_transaction = ls_transaction ).

    DELETE FROM tstca WHERE tcode = iv_transaction.

    IF ls_transaction IS NOT INITIAL.
      INSERT tstca FROM TABLE it_authorizations.
      ls_transaction-cinfo = ls_transaction-cinfo + lc_hex_chk.
      UPDATE tstc SET cinfo = ls_transaction-cinfo WHERE tcode = ls_transaction-tcode.
    ENDIF.

  ENDMETHOD.


  METHOD serialize_texts.

    DATA lt_tpool_i18n TYPE TABLE OF tstct.

    IF ii_xml->i18n_params( )-main_language_only = abap_true.
      RETURN.
    ENDIF.

    " Skip main language - it was already serialized
    " Don't serialize t-code itself
    SELECT sprsl ttext
      INTO CORRESPONDING FIELDS OF TABLE lt_tpool_i18n
      FROM tstct
      WHERE sprsl <> mv_language
      AND   tcode = ms_item-obj_name ##TOO_MANY_ITAB_FIELDS. "#EC CI_GENBUFF

    zcl_abapgit_lxe_texts=>trim_tab_w_saplang_by_iso(
      EXPORTING
        it_iso_filter = ii_xml->i18n_params( )-translation_languages
        iv_lang_field_name = 'SPRSL'
      CHANGING
        ct_tab = lt_tpool_i18n ).

    IF lines( lt_tpool_i18n ) > 0.
      SORT lt_tpool_i18n BY sprsl ASCENDING.
      ii_xml->add( iv_name = 'I18N_TPOOL'
                   ig_data = lt_tpool_i18n ).
    ENDIF.

  ENDMETHOD.


  METHOD set_oo_parameters.

    DATA: ls_param LIKE LINE OF it_rsparam.

    IF cs_rsstcd-call_tcode = c_oo_tcode.
      cs_rsstcd-s_trframe = c_true.
      LOOP AT it_rsparam INTO ls_param.
        CASE ls_param-field.
          WHEN c_oo_frclass.
            cs_rsstcd-classname = ls_param-value.
          WHEN c_oo_frmethod.
            cs_rsstcd-method   = ls_param-value.
          WHEN c_oo_frupdtask.
            IF ls_param-value = c_oo_synchron.
              cs_rsstcd-s_upddir  = c_true.
              cs_rsstcd-s_updtask = c_false.
              cs_rsstcd-s_updlok  = c_false.
            ELSEIF ls_param-value = c_oo_asynchron.
              cs_rsstcd-s_upddir  = c_false.
              cs_rsstcd-s_updtask = c_true.
              cs_rsstcd-s_updlok  = c_false.
            ELSE.
              cs_rsstcd-s_upddir  = c_false.
              cs_rsstcd-s_updtask = c_false.
              cs_rsstcd-s_updlok  = c_true.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD shift_param.

    DATA: ls_param  LIKE LINE OF ct_rsparam,
          lv_fdpos  TYPE sy-fdpos,
          lv_length TYPE i.

    FIELD-SYMBOLS <lg_f> TYPE any.


    DO 254 TIMES.
      IF cs_tstcp-param = space.
        EXIT.
      ENDIF.
      CLEAR ls_param.
      IF cs_tstcp-param CA '='.
        CHECK sy-fdpos <> 0.
        ASSIGN cs_tstcp-param(sy-fdpos) TO <lg_f>.
        ls_param-field = <lg_f>.
        IF ls_param-field(1) = space.
          SHIFT ls_param-field.
        ENDIF.
        lv_fdpos = sy-fdpos + 1.
        SHIFT cs_tstcp-param BY lv_fdpos PLACES.
        IF cs_tstcp-param CA ';'.
          IF sy-fdpos <> 0.
            ASSIGN cs_tstcp-param(sy-fdpos) TO <lg_f>.
            ls_param-value = <lg_f>.
            IF ls_param-value(1) = space.
              SHIFT ls_param-value.
            ENDIF.
          ENDIF.
          lv_fdpos = sy-fdpos + 1.
          SHIFT cs_tstcp-param BY lv_fdpos PLACES.
          APPEND ls_param TO ct_rsparam.
        ELSE.
          lv_length = strlen( cs_tstcp-param ).
          CHECK lv_length > 0.
          ASSIGN cs_tstcp-param(lv_length) TO <lg_f>.
          ls_param-value = <lg_f>.
          IF ls_param-value(1) = space.
            SHIFT ls_param-value.
          ENDIF.
          lv_length = lv_length + 1.
          SHIFT cs_tstcp-param BY lv_length PLACES.
          APPEND ls_param TO ct_rsparam.
        ENDIF.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD split_parameters.
* see subroutine split_parameters in include LSEUKF01

    DATA: lv_off       TYPE i,
          lv_fdpos     TYPE sy-fdpos,
          lv_param_beg TYPE i.


    CLEAR cs_rsstcd-s_vari.

    IF cs_tstcp-param(1) = '\'.             " OO-Transaktion ohne FR
      split_parameters_comp( EXPORTING ig_type = c_oo_program
                                       ig_param = cs_tstcp-param
                             CHANGING  cg_value = cs_tstc-pgmna ).
      split_parameters_comp( EXPORTING ig_type = c_oo_class
                                       ig_param = cs_tstcp-param
                             CHANGING  cg_value = cs_rsstcd-classname ).
      split_parameters_comp( EXPORTING ig_type = c_oo_method
                                       ig_param = cs_tstcp-param
                             CHANGING  cg_value = cs_rsstcd-method ).

      IF NOT cs_tstc-pgmna IS INITIAL.
        cs_rsstcd-s_local = c_true.
      ENDIF.
      RETURN.
    ELSEIF cs_tstcp-param(1) = '@'.         " Transaktionsvariante
      cs_rsstcd-s_vari = c_true.
      IF cs_tstcp-param(2) = '@@'.
        cs_rsstcd-s_ind_vari = c_true.
        lv_off = 2.
      ELSE.
        CLEAR cs_rsstcd-s_ind_vari.
        lv_off = 1.
      ENDIF.
      IF cs_tstcp-param CA ' '.
      ENDIF.
      lv_fdpos = sy-fdpos - lv_off.
      IF lv_fdpos > 0.
        cs_rsstcd-call_tcode = cs_tstcp-param+lv_off(sy-fdpos).
        lv_fdpos = lv_fdpos + 1 + lv_off.
        cs_rsstcd-variant = cs_tstcp-param+lv_fdpos.
      ENDIF.
    ELSEIF cs_tstcp-param(1) = '/'.
      cs_rsstcd-st_tcode = c_true.
      cs_rsstcd-st_prog  = space.
      IF cs_tstcp-param+1(1) = '*'.
        cs_rsstcd-st_skip_1 = c_true.
      ELSE.
        CLEAR cs_rsstcd-st_skip_1.
      ENDIF.
      IF cs_tstcp-param CA ' '.
      ENDIF.
      lv_param_beg = sy-fdpos + 1.
      lv_fdpos = sy-fdpos - 2.
      IF lv_fdpos > 0.
        cs_rsstcd-call_tcode = cs_tstcp-param+2(lv_fdpos).
      ENDIF.
      SHIFT cs_tstcp-param BY lv_param_beg PLACES.
    ELSE.
      cs_rsstcd-st_tcode = space.
      cs_rsstcd-st_prog  = c_true.
    ENDIF.

    shift_param(
      CHANGING ct_rsparam = ct_rsparam
               cs_tstcp   = cs_tstcp ).

    set_oo_parameters(
      EXPORTING it_rsparam = ct_rsparam
      CHANGING cs_rsstcd = cs_rsstcd ).

  ENDMETHOD.


  METHOD split_parameters_comp.
    DATA: lv_off TYPE i.

    IF ig_param CS ig_type.
      lv_off = sy-fdpos + strlen( ig_type ).
      cg_value = ig_param+lv_off.
      IF cg_value CA '\'.
        CLEAR cg_value+sy-fdpos.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD transaction_read.

    DATA: lt_tcodes   TYPE TABLE OF tstc,
          lt_gui_attr TYPE TABLE OF tstcc.

    CLEAR: es_transaction, es_gui_attr.

    CALL FUNCTION 'RPY_TRANSACTION_READ'
      EXPORTING
        transaction      = iv_transaction
      TABLES
        tcodes           = lt_tcodes
        gui_attributes   = lt_gui_attr
      EXCEPTIONS
        permission_error = 1
        cancelled        = 2
        not_found        = 3
        object_not_found = 4
        OTHERS           = 5.
    IF sy-subrc = 4 OR sy-subrc = 3.
      RETURN.
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    READ TABLE lt_tcodes INDEX 1 INTO es_transaction.
    ASSERT sy-subrc = 0.
    READ TABLE lt_gui_attr INDEX 1 INTO es_gui_attr.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
* looks like "changed by user" is not stored in the database
    rv_user = c_user_unknown.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lv_transaction TYPE tstc-tcode.


    lv_transaction = ms_item-obj_name.

    CALL FUNCTION 'RPY_TRANSACTION_DELETE'
      EXPORTING
        transaction      = lv_transaction
      EXCEPTIONS
        not_excecuted    = 1
        object_not_found = 0
        OTHERS           = 3.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    CONSTANTS: lc_hex_tra TYPE x VALUE '00',
*               lc_hex_men TYPE x VALUE '01',
               lc_hex_par TYPE x VALUE '02',
               lc_hex_rep TYPE x VALUE '80',
*               lc_hex_rpv TYPE x VALUE '10',
               lc_hex_obj TYPE x VALUE '08'.

    DATA: lv_dynpro       TYPE d020s-dnum,
          ls_tstc         TYPE tstc,
          lv_type         TYPE rglif-docutype,
          ls_tstct        TYPE tstct,
          ls_tstcc        TYPE tstcc,
          ls_tstcp        TYPE tstcp,
          lt_tstca        TYPE ty_tstca,
          lt_param_values TYPE ty_param_values,
          ls_rsstcd       TYPE rsstcd.


    IF zif_abapgit_object~exists( ) = abap_true.
      zif_abapgit_object~delete( iv_package   = iv_package
                                 iv_transport = iv_transport ).
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'TSTC'
                  CHANGING cg_data = ls_tstc ).
    io_xml->read( EXPORTING iv_name = 'TSTCC'
                  CHANGING cg_data = ls_tstcc ).
    io_xml->read( EXPORTING iv_name = 'TSTCT'
                  CHANGING cg_data = ls_tstct ).
    io_xml->read( EXPORTING iv_name = 'TSTCP'
                  CHANGING cg_data = ls_tstcp ).
    io_xml->read( EXPORTING iv_name = 'AUTHORIZATIONS'
                  CHANGING cg_data = lt_tstca ).

    lv_dynpro = ls_tstc-dypno.

    IF     ls_tstc-cinfo O lc_hex_rep.
      lv_type = c_variant_type-report.
    ELSEIF ls_tstc-cinfo O lc_hex_obj.
      lv_type = c_variant_type-object.
    ELSEIF ls_tstc-cinfo O lc_hex_par.
      IF is_variant_transaction( ls_tstcp ) = abap_true.
        lv_type = c_variant_type-variant.
      ELSE.
        lv_type = c_variant_type-parameters.
      ENDIF.
    ELSEIF ls_tstc-cinfo O lc_hex_tra.
      lv_type = c_variant_type-dialog.
    ELSE.
      zcx_abapgit_exception=>raise( 'Transaction, unknown CINFO' ).
    ENDIF.

    IF ls_tstcp IS NOT INITIAL.
      split_parameters( CHANGING ct_rsparam = lt_param_values
                                 cs_rsstcd  = ls_rsstcd
                                 cs_tstcp   = ls_tstcp
                                 cs_tstc    = ls_tstc ).
    ENDIF.

    CASE lv_type.
      WHEN c_variant_type-object.

        deserialize_oo_transaction( iv_package      = iv_package
                                    is_tstc         = ls_tstc
                                    is_tstcc        = ls_tstcc
                                    is_tstct        = ls_tstct
                                    is_rsstcd       = ls_rsstcd ).

      WHEN OTHERS.

        clear_functiongroup_globals( ).

        corr_insert( iv_package ).

        CALL FUNCTION 'RPY_TRANSACTION_INSERT'
          EXPORTING
            transaction             = ls_tstc-tcode
            program                 = ls_tstc-pgmna
            dynpro                  = lv_dynpro
            language                = mv_language
            development_class       = iv_package
            transaction_type        = lv_type
            shorttext               = ls_tstct-ttext
            called_transaction      = ls_rsstcd-call_tcode
            called_transaction_skip = ls_rsstcd-st_skip_1
            variant                 = ls_rsstcd-variant
            cl_independend          = ls_rsstcd-s_ind_vari
            html_enabled            = ls_tstcc-s_webgui
            java_enabled            = ls_tstcc-s_platin
            wingui_enabled          = ls_tstcc-s_win32
            suppress_corr_insert    = abap_true
          TABLES
            param_values            = lt_param_values
          EXCEPTIONS
            cancelled               = 1
            already_exist           = 2
            permission_error        = 3
            name_not_allowed        = 4
            name_conflict           = 5
            illegal_type            = 6
            object_inconsistent     = 7
            db_access_error         = 8
            OTHERS                  = 9.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise_t100( ).
        ENDIF.

    ENDCASE.

    IF lt_tstca IS NOT INITIAL.
      save_authorizations( iv_transaction    = ls_tstc-tcode
                           it_authorizations = lt_tstca ).
    ENDIF.

    IF io_xml->i18n_params( )-translation_languages IS INITIAL OR io_xml->i18n_params( )-use_lxe = abap_false.
      deserialize_texts( io_xml ).
    ELSE.
      deserialize_lxe_texts( io_xml ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_tcode TYPE tstc-tcode.


    SELECT SINGLE tcode FROM tstc INTO lv_tcode
      WHERE tcode = ms_item-obj_name.                   "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

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

    DATA: lv_object TYPE eqegraarg.

    lv_object = |TN{ ms_item-obj_name }|.
    OVERLAY lv_object WITH '                                          '.
    lv_object = lv_object && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'EEUDB'
                                            iv_argument    = lv_object ).


  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    DATA: lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.


    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPLSEUK'.
    <ls_bdcdata>-dynpro   = '0390'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=SHOW'.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'TSTC-TCODE'.
    <ls_bdcdata>-fval = ms_item-obj_name.

    zcl_abapgit_ui_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode      = 'SE93'
      it_bdcdata    = lt_bdcdata ).

    rv_exit = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lv_transaction TYPE tstc-tcode,
          ls_tcode       TYPE tstc,
          ls_tstct       TYPE tstct,
          ls_tstcp       TYPE tstcp,
          lt_tstca       TYPE ty_tstca,
          ls_gui_attr    TYPE tstcc.


    lv_transaction = ms_item-obj_name.

    transaction_read( EXPORTING iv_transaction = lv_transaction
                      IMPORTING es_transaction = ls_tcode
                                es_gui_attr    = ls_gui_attr ).
    IF ls_tcode IS INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM tstct INTO ls_tstct
      WHERE sprsl = mv_language
      AND tcode = lv_transaction.         "#EC CI_SUBRC "#EC CI_GENBUFF

    SELECT SINGLE * FROM tstcp INTO ls_tstcp
      WHERE tcode = lv_transaction.       "#EC CI_SUBRC "#EC CI_GENBUFF

    SELECT * FROM tstca INTO TABLE lt_tstca
      WHERE tcode = lv_transaction.
    IF sy-subrc <> 0.
      CLEAR: lt_tstca.
    ENDIF.

    io_xml->add( iv_name = 'TSTC'
                 ig_data = ls_tcode ).
    io_xml->add( iv_name = 'TSTCC'
                 ig_data = ls_gui_attr ).
    io_xml->add( iv_name = 'TSTCT'
                 ig_data = ls_tstct ).
    IF ls_tstcp IS NOT INITIAL.
      io_xml->add( iv_name = 'TSTCP'
                   ig_data = ls_tstcp ).
    ENDIF.
    io_xml->add( iv_name = 'AUTHORIZATIONS'
                 ig_data = lt_tstca ).

    IF io_xml->i18n_params( )-translation_languages IS INITIAL OR io_xml->i18n_params( )-use_lxe = abap_false.
      serialize_texts( io_xml ).
    ELSE.
      serialize_lxe_texts( io_xml ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
