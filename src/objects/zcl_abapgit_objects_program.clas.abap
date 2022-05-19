CLASS zcl_abapgit_objects_program DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_progdir,
             name    TYPE progdir-name,
             state   TYPE progdir-state,
             sqlx    TYPE progdir-sqlx,
             edtx    TYPE progdir-edtx,
             varcl   TYPE progdir-varcl,
             dbapl   TYPE progdir-dbapl,
             dbna    TYPE progdir-dbna,
             clas    TYPE progdir-clas,
             type    TYPE progdir-type,
             occurs  TYPE progdir-occurs,
             subc    TYPE progdir-subc,
             appl    TYPE progdir-appl,
             secu    TYPE progdir-secu,
             cnam    TYPE progdir-cnam,
             cdat    TYPE progdir-cdat,
             unam    TYPE progdir-unam,
             udat    TYPE progdir-udat,
             vern    TYPE progdir-vern,
             levl    TYPE progdir-levl,
             rstat   TYPE progdir-rstat,
             rmand   TYPE progdir-rmand,
             rload   TYPE progdir-rload,
             fixpt   TYPE progdir-fixpt,
             sset    TYPE progdir-sset,
             sdate   TYPE progdir-sdate,
             stime   TYPE progdir-stime,
             idate   TYPE progdir-idate,
             itime   TYPE progdir-itime,
             ldbname TYPE progdir-ldbname,
             uccheck TYPE progdir-uccheck,
           END OF ty_progdir.

    METHODS serialize_program
      IMPORTING io_xml     TYPE REF TO zif_abapgit_xml_output OPTIONAL
                is_item    TYPE zif_abapgit_definitions=>ty_item
                io_files   TYPE REF TO zcl_abapgit_objects_files
                iv_program TYPE programm OPTIONAL
                iv_extra   TYPE clike OPTIONAL
      RAISING   zcx_abapgit_exception.

    METHODS read_progdir
      IMPORTING iv_program        TYPE programm
      RETURNING VALUE(rs_progdir) TYPE ty_progdir.

    METHODS deserialize_program
      IMPORTING is_progdir TYPE ty_progdir
                it_source  TYPE abaptxt255_tab
                it_tpool   TYPE textpool_table
                iv_package TYPE devclass
      RAISING   zcx_abapgit_exception.

protected section.

  types:
    ty_spaces_tt TYPE STANDARD TABLE OF i WITH DEFAULT KEY .
  types:
    BEGIN OF ty_dynpro,
        header     TYPE rpy_dyhead,
        containers TYPE dycatt_tab,
        fields     TYPE dyfatc_tab,
        flow_logic TYPE swydyflow,
        spaces     TYPE ty_spaces_tt,
      END OF ty_dynpro .
  types:
    ty_dynpro_tt TYPE STANDARD TABLE OF ty_dynpro WITH DEFAULT KEY .
  types:
    BEGIN OF ty_cua,
        adm TYPE rsmpe_adm,
        sta TYPE STANDARD TABLE OF rsmpe_stat WITH DEFAULT KEY,
        fun TYPE STANDARD TABLE OF rsmpe_funt WITH DEFAULT KEY,
        men TYPE STANDARD TABLE OF rsmpe_men WITH DEFAULT KEY,
        mtx TYPE STANDARD TABLE OF rsmpe_mnlt WITH DEFAULT KEY,
        act TYPE STANDARD TABLE OF rsmpe_act WITH DEFAULT KEY,
        but TYPE STANDARD TABLE OF rsmpe_but WITH DEFAULT KEY,
        pfk TYPE STANDARD TABLE OF rsmpe_pfk WITH DEFAULT KEY,
        set TYPE STANDARD TABLE OF rsmpe_staf WITH DEFAULT KEY,
        doc TYPE STANDARD TABLE OF rsmpe_atrt WITH DEFAULT KEY,
        tit TYPE STANDARD TABLE OF rsmpe_titt WITH DEFAULT KEY,
        biv TYPE STANDARD TABLE OF rsmpe_buts WITH DEFAULT KEY,
      END OF ty_cua .
  types:
    BEGIN OF ty_oo_docu_cat,
    id   TYPE DOKU_ID,
    comp TYPE SEOCMPNAME,
  END OF ty_oo_docu_cat .
  types:
    ty_oo_docu_cat_tt type STANDARD TABLE OF ty_oo_docu_cat .

  methods STRIP_GENERATION_COMMENTS
    changing
      !CT_SOURCE type ABAPTXT255_TAB .
  methods SERIALIZE_DYNPROS
    importing
      !IV_PROGRAM_NAME type PROGRAMM
    returning
      value(RT_DYNPRO) type TY_DYNPRO_TT
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods SERIALIZE_CUA
    importing
      !IV_PROGRAM_NAME type PROGRAMM
    returning
      value(RS_CUA) type TY_CUA
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods DESERIALIZE_DYNPROS
    importing
      !IT_DYNPROS type TY_DYNPRO_TT
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods DESERIALIZE_TEXTPOOL
    importing
      !IV_PROGRAM type PROGRAMM
      !IT_TPOOL type TEXTPOOL_TABLE
      !IV_LANGUAGE type SY-LANGU optional
      !IV_IS_INCLUDE type ABAP_BOOL default ABAP_FALSE
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods DESERIALIZE_CUA
    importing
      !IV_PROGRAM_NAME type PROGRAMM
      !IS_CUA type TY_CUA
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods IS_ANY_DYNPRO_LOCKED
    importing
      !IV_PROGRAM type PROGRAMM
    returning
      value(RV_IS_ANY_DYNPRO_LOCKED) type ABAP_BOOL
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods IS_CUA_LOCKED
    importing
      !IV_PROGRAM type PROGRAMM
    returning
      value(RV_IS_CUA_LOCKED) type ABAP_BOOL
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods IS_TEXT_LOCKED
    importing
      !IV_PROGRAM type PROGRAMM
    returning
      value(RV_IS_TEXT_LOCKED) type ABAP_BOOL
    raising
      ZCX_ABAPGIT_EXCEPTION .
  class-methods ADD_TPOOL
    importing
      !IT_TPOOL type TEXTPOOL_TABLE
    returning
      value(RT_TPOOL) type ZIF_ABAPGIT_DEFINITIONS=>TY_TPOOL_TT .
  class-methods READ_TPOOL
    importing
      !IT_TPOOL type ZIF_ABAPGIT_DEFINITIONS=>TY_TPOOL_TT
    returning
      value(RT_TPOOL) type ZIF_ABAPGIT_DEFINITIONS=>TY_TPOOL_TT .
  methods SERIALIZE_ALL_DOCU
    importing
      !II_XML type ref to ZIF_ABAPGIT_XML_OUTPUT
      !IV_CLSNAME type SEOCLSNAME
      !II_OBJECT_ORIENTED_OBJECT_FCT type ref to ZIF_ABAPGIT_OO_OBJECT_FNC
      !IV_ID type DOKU_ID
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods DESERIALIZE_ALL_DOCU
    importing
      !II_OBJECT_ORIENTED_OBJECT_FCT type ref to ZIF_ABAPGIT_OO_OBJECT_FNC
      !II_XML type ref to ZIF_ABAPGIT_XML_INPUT
      !IV_ID type DOKU_ID
    raising
      ZCX_ABAPGIT_EXCEPTION .
private section.

  methods SERIALIZE_DOCU
    importing
      !II_XML type ref to ZIF_ABAPGIT_XML_OUTPUT
      !IT_LANGU_ADDITIONAL type ZIF_ABAPGIT_LANG_DEFINITIONS=>TY_LANGUS
      !IV_OBJ type DOKHL-OBJECT
      !IV_ID type DOKHL-ID
      !II_OBJECT_ORIENTED_OBJECT_FCT type ref to ZIF_ABAPGIT_OO_OBJECT_FNC
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods DESERIALIZE_DOCU
    importing
      !II_XML type ref to ZIF_ABAPGIT_XML_INPUT
      !IV_ID type DOKU_ID
      !IV_COMP type CLIKE optional
      !IV_CLSNAME type CLIKE
      !II_OBJECT_ORIENTED_OBJECT_FCT type ref to ZIF_ABAPGIT_OO_OBJECT_FNC
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods MAKE_DOCU_TAG
    importing
      !IV_PREFIX type CLIKE
      !IV_ID type DOKU_ID
      !IV_COMP type CLIKE
    returning
      value(OV_TAG) type STRING .
  methods UNCONDENSE_FLOW
    importing
      !IT_FLOW type SWYDYFLOW
      !IT_SPACES type TY_SPACES_TT
    returning
      value(RT_FLOW) type SWYDYFLOW .
  class-methods AUTO_CORRECT_CUA_ADM
    importing
      !IS_CUA type TY_CUA
    changing
      !CS_ADM type RSMPE_ADM .
  methods GET_PROGRAM_TITLE
    importing
      !IT_TPOOL type TEXTPOOL_TABLE
    returning
      value(RV_TITLE) type REPTI .
  methods INSERT_PROGRAM
    importing
      !IS_PROGDIR type TY_PROGDIR
      !IT_SOURCE type ABAPTXT255_TAB
      !IV_TITLE type REPTI
      !IV_PACKAGE type DEVCLASS
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods UPDATE_PROGRAM
    importing
      !IS_PROGDIR type TY_PROGDIR
      !IT_SOURCE type ABAPTXT255_TAB
      !IV_TITLE type REPTI
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods UPDATE_PROGDIR
    importing
      !IS_PROGDIR type TY_PROGDIR
    raising
      ZCX_ABAPGIT_EXCEPTION .
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECTS_PROGRAM IMPLEMENTATION.


  METHOD add_tpool.

    FIELD-SYMBOLS: <ls_tpool_in>  LIKE LINE OF it_tpool,
                   <ls_tpool_out> LIKE LINE OF rt_tpool.


    LOOP AT it_tpool ASSIGNING <ls_tpool_in>.
      APPEND INITIAL LINE TO rt_tpool ASSIGNING <ls_tpool_out>.
      MOVE-CORRESPONDING <ls_tpool_in> TO <ls_tpool_out>.
      IF <ls_tpool_out>-id = 'S'.
        <ls_tpool_out>-split = <ls_tpool_out>-entry.
        <ls_tpool_out>-entry = <ls_tpool_out>-entry+8.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD auto_correct_cua_adm.
    " issue #1807 automatic correction of CUA interfaces saved incorrectly in the past (ADM was not saved in the XML)

    CONSTANTS:
      lc_num_n_space TYPE string VALUE ' 0123456789',
      lc_num_only    TYPE string VALUE '0123456789'.

    FIELD-SYMBOLS:
      <ls_pfk> TYPE rsmpe_pfk,
      <ls_act> TYPE rsmpe_act,
      <ls_men> TYPE rsmpe_men.

    IF cs_adm IS NOT INITIAL
        AND cs_adm-actcode CO lc_num_n_space
        AND cs_adm-mencode CO lc_num_n_space
        AND cs_adm-pfkcode CO lc_num_n_space. "Check performed in form check_adm of include LSMPIF03
      RETURN.
    ENDIF.

    LOOP AT is_cua-act ASSIGNING <ls_act>.
      IF <ls_act>-code+6(14) IS INITIAL AND <ls_act>-code(6) CO lc_num_only.
        cs_adm-actcode = <ls_act>-code.
      ENDIF.
    ENDLOOP.

    LOOP AT is_cua-men ASSIGNING <ls_men>.
      IF <ls_men>-code+6(14) IS INITIAL AND <ls_men>-code(6) CO lc_num_only.
        cs_adm-mencode = <ls_men>-code.
      ENDIF.
    ENDLOOP.

    LOOP AT is_cua-pfk ASSIGNING <ls_pfk>.
      IF <ls_pfk>-code+6(14) IS INITIAL AND <ls_pfk>-code(6) CO lc_num_only.
        cs_adm-pfkcode = <ls_pfk>-code.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  method DESERIALIZE_ALL_DOCU.
    data: lt_cat type ty_oo_docu_cat_tt.
    data: ls_cat type ty_oo_docu_cat.

    deserialize_docu(
      ii_xml                        = ii_xml
      iv_id = IV_ID
      iv_clsname                    = ms_item-obj_name
      ii_object_oriented_object_fct = ii_object_oriented_object_fct ).

    ii_xml->read( EXPORTING iv_name = 'DOCU_CAT'
                  CHANGING cg_data  = lt_cat  ).

    loop at lt_cat into ls_cat.
      deserialize_docu(
        ii_xml                        = ii_xml
        iv_id                         = ls_cat-id
        iv_clsname                    = ms_item-obj_name
        iv_comp                       = ls_cat-comp
        ii_object_oriented_object_fct = ii_object_oriented_object_fct ).
    endloop.
  endmethod.


  METHOD deserialize_cua.

    DATA: ls_tr_key TYPE trkey,
          ls_adm    TYPE rsmpe_adm.


    IF lines( is_cua-sta ) = 0
        AND lines( is_cua-fun ) = 0
        AND lines( is_cua-men ) = 0
        AND lines( is_cua-mtx ) = 0
        AND lines( is_cua-act ) = 0
        AND lines( is_cua-but ) = 0
        AND lines( is_cua-pfk ) = 0
        AND lines( is_cua-set ) = 0
        AND lines( is_cua-doc ) = 0
        AND lines( is_cua-tit ) = 0
        AND lines( is_cua-biv ) = 0.
      RETURN.
    ENDIF.

    SELECT SINGLE devclass INTO ls_tr_key-devclass
      FROM tadir
      WHERE pgmid = 'R3TR'
      AND object = ms_item-obj_type
      AND obj_name = ms_item-obj_name.                  "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'not found in tadir' ).
    ENDIF.

    ls_tr_key-obj_type = ms_item-obj_type.
    ls_tr_key-obj_name = ms_item-obj_name.
    ls_tr_key-sub_type = 'CUAD'.
    ls_tr_key-sub_name = iv_program_name.

    ls_adm = is_cua-adm.
    auto_correct_cua_adm( EXPORTING is_cua = is_cua CHANGING cs_adm = ls_adm ).

    sy-tcode = 'SE41' ##WRITE_OK. " evil hack, workaround to handle fixes in note 2159455
    CALL FUNCTION 'RS_CUA_INTERNAL_WRITE'
      EXPORTING
        program   = iv_program_name
        language  = mv_language
        tr_key    = ls_tr_key
        adm       = ls_adm
        state     = 'I'
      TABLES
        sta       = is_cua-sta
        fun       = is_cua-fun
        men       = is_cua-men
        mtx       = is_cua-mtx
        act       = is_cua-act
        but       = is_cua-but
        pfk       = is_cua-pfk
        set       = is_cua-set
        doc       = is_cua-doc
        tit       = is_cua-tit
        biv       = is_cua-biv
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
* if moving code from SAPlink, see https://github.com/abapGit/abapGit/issues/562
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    zcl_abapgit_objects_activation=>add(
      iv_type = 'CUAD'
      iv_name = iv_program_name ).

  ENDMETHOD.


  METHOD deserialize_docu.
    DATA lv_name TYPE string.
    DATA lv_i18n_name  TYPE string.

    DATA: lt_lines      TYPE tlinetab,
          lv_object     TYPE dokhl-object,
          lt_i18n_lines TYPE zif_abapgit_lang_definitions=>ty_i18n_lines,
          ls_i18n_lines TYPE zif_abapgit_lang_definitions=>ty_i18n_line.

    lv_name      = make_docu_tag( iv_prefix = 'LINES'       iv_id = iv_id     iv_comp = iv_comp ).
    lv_i18n_name = make_docu_tag( iv_prefix = 'I18N_LINES'  iv_id = iv_id     iv_comp = iv_comp ).

    ii_xml->read( EXPORTING iv_name = lv_name
                  CHANGING cg_data = lt_lines ).

    lv_object = iv_clsname.
    lv_object+30(30) = iv_comp.

    IF lines( lt_lines ) = 0.
      ii_object_oriented_object_fct->delete_documentation(
        iv_id          = iv_id
        iv_object_name = lv_object
        iv_language    = mv_language ).
      RETURN.
    ENDIF.

    ii_object_oriented_object_fct->create_documentation(
      it_lines       = lt_lines
      iv_id          = iv_id
      iv_object_name = lv_object
      iv_language    = mv_language ).

    ii_xml->read( EXPORTING iv_name = lv_i18n_name
                  CHANGING cg_data = lt_i18n_lines ).

    LOOP AT lt_i18n_lines INTO ls_i18n_lines.
      ii_object_oriented_object_fct->create_documentation(
        it_lines         = ls_i18n_lines-lines
        iv_id            = iv_id
        iv_object_name   = lv_object
        iv_language      = ls_i18n_lines-language
        iv_no_masterlang = abap_true ).
    ENDLOOP.
  ENDMETHOD.


  METHOD deserialize_dynpros.

    CONSTANTS lc_rpyty_force_off TYPE c LENGTH 1 VALUE '/'.

    DATA: lv_name   TYPE dwinactiv-obj_name,
          lt_d020s_to_delete TYPE TABLE OF d020s,
          ls_d020s  LIKE LINE OF lt_d020s_to_delete,
          ls_dynpro LIKE LINE OF it_dynpros.

    FIELD-SYMBOLS: <ls_field> TYPE rpy_dyfatc.

    " Delete DYNPROs which are not in the list
    CALL FUNCTION 'RS_SCREEN_LIST'
      EXPORTING
        dynnr     = ''
        progname  = ms_item-obj_name
      TABLES
        dynpros   = lt_d020s_to_delete
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc = 2.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    SORT lt_d020s_to_delete BY dnum ASCENDING.

* ls_dynpro is changed by the function module, a field-symbol will cause
* the program to dump since it_dynpros cannot be changed
    LOOP AT it_dynpros INTO ls_dynpro.

      READ TABLE lt_d020s_to_delete WITH KEY dnum = ls_dynpro-header-screen
        TRANSPORTING NO FIELDS
        BINARY SEARCH.
      IF sy-subrc = 0.
        DELETE lt_d020s_to_delete INDEX sy-tabix.
      ENDIF.

      " todo: kept for compatibility, remove after grace period #3680
      ls_dynpro-flow_logic = uncondense_flow(
        it_flow = ls_dynpro-flow_logic
        it_spaces = ls_dynpro-spaces ).


      LOOP AT ls_dynpro-fields ASSIGNING <ls_field>.
* if the DDIC element has a PARAMETER_ID and the flag "from_dict" is active
* the import will enable the SET-/GET_PARAM flag. In this case: "force off"
        IF <ls_field>-param_id IS NOT INITIAL
            AND <ls_field>-from_dict = abap_true.
          IF <ls_field>-set_param IS INITIAL.
            <ls_field>-set_param = lc_rpyty_force_off.
          ENDIF.
          IF <ls_field>-get_param IS INITIAL.
            <ls_field>-get_param = lc_rpyty_force_off.
          ENDIF.
        ENDIF.

* If the previous conditions are met the value 'F' will be taken over
* during de-serialization potentially overlapping other fields in the screen,
* we set the tag to the correct value 'X'
        IF <ls_field>-type = 'CHECK'
            AND <ls_field>-from_dict = abap_true
            AND <ls_field>-text IS INITIAL
            AND <ls_field>-modific IS INITIAL.
          <ls_field>-modific = 'X'.
        ENDIF.

        "fix for issue #2747:
        IF <ls_field>-foreignkey IS INITIAL.
          <ls_field>-foreignkey = lc_rpyty_force_off.
        ENDIF.

      ENDLOOP.

      CALL FUNCTION 'RPY_DYNPRO_INSERT'
        EXPORTING
          header                 = ls_dynpro-header
          suppress_exist_checks  = abap_true
        TABLES
          containers             = ls_dynpro-containers
          fields_to_containers   = ls_dynpro-fields
          flow_logic             = ls_dynpro-flow_logic
        EXCEPTIONS
          cancelled              = 1
          already_exists         = 2
          program_not_exists     = 3
          not_executed           = 4
          missing_required_field = 5
          illegal_field_value    = 6
          field_not_allowed      = 7
          not_generated          = 8
          illegal_field_position = 9
          OTHERS                 = 10.
      IF sy-subrc <> 2 AND sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.
* todo, RPY_DYNPRO_UPDATE?

      CONCATENATE ls_dynpro-header-program ls_dynpro-header-screen
        INTO lv_name RESPECTING BLANKS.
      ASSERT NOT lv_name IS INITIAL.

      zcl_abapgit_objects_activation=>add(
        iv_type = 'DYNP'
        iv_name = lv_name ).

    ENDLOOP.

    " Delete obsolete screens
    LOOP AT lt_d020s_to_delete INTO ls_d020s.

      CALL FUNCTION 'RS_SCRP_DELETE'
        EXPORTING
          dynnr      = ls_d020s-dnum
          progname   = ms_item-obj_name
          with_popup = abap_false
        EXCEPTIONS
          enqueued_by_user       = 1
          enqueue_system_failure = 2
          not_executed           = 3
          not_exists             = 4
          no_modify_permission   = 5
          popup_canceled         = 6.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD deserialize_program.

    DATA:
      lv_progname TYPE reposrc-progname,
      lv_title    TYPE rglif-title.

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = is_progdir-name
        object_class        = 'ABAP'
        devclass            = iv_package
        master_language     = mv_language
        mode                = 'I'
        suppress_dialog     = abap_true
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    lv_title = get_program_title( it_tpool ).

    " Check if program already exists
    SELECT SINGLE progname FROM reposrc INTO lv_progname
      WHERE progname = is_progdir-name
      AND r3state = 'A'.

    IF sy-subrc = 0.
      update_program(
        is_progdir = is_progdir
        it_source  = it_source
        iv_title   = lv_title ).
    ELSE.
      insert_program(
        is_progdir = is_progdir
        it_source  = it_source
        iv_title   = lv_title
        iv_package = iv_package ).
    ENDIF.

    update_progdir( is_progdir ).

    zcl_abapgit_objects_activation=>add(
      iv_type = 'REPS'
      iv_name = is_progdir-name ).

  ENDMETHOD.


  METHOD deserialize_textpool.

    DATA lv_language TYPE sy-langu.
    DATA lv_state    TYPE c.
    DATA lv_delete   TYPE abap_bool.

    IF iv_language IS INITIAL.
      lv_language = mv_language.
    ELSE.
      lv_language = iv_language.
    ENDIF.

    IF lv_language = mv_language.
      lv_state = 'I'. "Textpool in main language needs to be activated
    ELSE.
      lv_state = 'A'. "Translations are always active
    ENDIF.

    IF it_tpool IS INITIAL.
      IF iv_is_include = abap_false OR lv_state = 'A'.
        DELETE TEXTPOOL iv_program "Remove initial description from textpool if
          LANGUAGE lv_language     "original program does not have a textpool
          STATE lv_state.

        lv_delete = abap_true.
      ELSE.
        INSERT TEXTPOOL iv_program "In case of includes: Deletion of textpool in
          FROM it_tpool            "main language cannot be activated because
          LANGUAGE lv_language     "this woul activate the deletion of the textpool
          STATE lv_state.          "of the mail program -> insert empty textpool
      ENDIF.
    ELSE.
      INSERT TEXTPOOL iv_program
        FROM it_tpool
        LANGUAGE lv_language
        STATE lv_state.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'error from INSERT TEXTPOOL' ).
      ENDIF.
    ENDIF.

    IF lv_state = 'I'. "Textpool in main language needs to be activated
      zcl_abapgit_objects_activation=>add(
        iv_type   = 'REPT'
        iv_name   = iv_program
        iv_delete = lv_delete ).
    ENDIF.
  ENDMETHOD.


  METHOD get_program_title.

    DATA ls_tpool LIKE LINE OF it_tpool.

    FIELD-SYMBOLS <lg_any> TYPE any.

    READ TABLE it_tpool INTO ls_tpool WITH KEY id = 'R'.
    IF sy-subrc = 0.
      " there is a bug in RPY_PROGRAM_UPDATE, the header line of TTAB is not
      " cleared, so the title length might be inherited from a different program.
      ASSIGN ('(SAPLSIFP)TTAB') TO <lg_any>.
      IF sy-subrc = 0.
        CLEAR <lg_any>.
      ENDIF.

      rv_title = ls_tpool-entry.
    ENDIF.

  ENDMETHOD.


  METHOD insert_program.

    CALL FUNCTION 'RPY_PROGRAM_INSERT'
      EXPORTING
        development_class = iv_package
        program_name      = is_progdir-name
        program_type      = is_progdir-subc
        title_string      = iv_title
        save_inactive     = 'I'
        suppress_dialog   = abap_true
      TABLES
        source_extended   = it_source
      EXCEPTIONS
        already_exists    = 1
        cancelled         = 2
        name_not_allowed  = 3
        permission_error  = 4
        OTHERS            = 5.
    IF sy-subrc = 3.

      " For cases that standard function does not handle (like FUGR),
      " we save active and inactive version of source with the given PROGRAM TYPE.
      " Without the active version, the code will not be visible in case of activation errors.
      INSERT REPORT is_progdir-name
        FROM it_source
        STATE 'A'
        PROGRAM TYPE is_progdir-subc.
      INSERT REPORT is_progdir-name
        FROM it_source
        STATE 'I'
        PROGRAM TYPE is_progdir-subc.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Error from INSERT REPORT .. PROGRAM TYPE' ).
      ENDIF.

    ELSEIF sy-subrc > 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD is_any_dynpro_locked.

    DATA: lt_dynpros TYPE ty_dynpro_tt,
          lv_object  TYPE seqg3-garg.

    FIELD-SYMBOLS: <ls_dynpro> TYPE ty_dynpro.

    lt_dynpros = serialize_dynpros( iv_program ).

    LOOP AT lt_dynpros ASSIGNING <ls_dynpro>.

      lv_object = |{ <ls_dynpro>-header-screen }{ <ls_dynpro>-header-program }|.

      IF exists_a_lock_entry_for( iv_lock_object = 'ESCRP'
                                  iv_argument    = lv_object ) = abap_true.
        rv_is_any_dynpro_locked = abap_true.
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD is_cua_locked.

    DATA: lv_object TYPE eqegraarg.

    lv_object = |CU{ iv_program }|.
    OVERLAY lv_object WITH '                                          '.
    lv_object = lv_object && '*'.

    rv_is_cua_locked = exists_a_lock_entry_for( iv_lock_object = 'ESCUAPAINT'
                                                iv_argument    = lv_object ).

  ENDMETHOD.


  METHOD is_text_locked.

    DATA: lv_object TYPE eqegraarg.

    lv_object = |*{ iv_program }|.

    rv_is_text_locked = exists_a_lock_entry_for( iv_lock_object = 'EABAPTEXTE'
                                                 iv_argument    = lv_object ).

  ENDMETHOD.


  method MAKE_DOCU_TAG.
    if iv_id = 'IF' or iv_id = 'CL'.
      ov_tag = iv_prefix.  " LINES or I18N_LINES
    else.
      CONCATENATE iv_prefix '_' iv_id '_' iv_comp INTO ov_tag.
    endif.
  endmethod.


  METHOD read_progdir.

    DATA: ls_sapdir TYPE progdir.


    CALL FUNCTION 'READ_PROGDIR'
      EXPORTING
        i_progname = iv_program
        i_state    = 'A'
      IMPORTING
        e_progdir  = ls_sapdir.
    MOVE-CORRESPONDING ls_sapdir TO rs_progdir.

    CLEAR: rs_progdir-edtx,
           rs_progdir-cnam,
           rs_progdir-cdat,
           rs_progdir-unam,
           rs_progdir-udat,
           rs_progdir-levl,
           rs_progdir-vern,
           rs_progdir-rmand,
           rs_progdir-sdate,
           rs_progdir-stime,
           rs_progdir-idate,
           rs_progdir-itime,
           rs_progdir-varcl,
           rs_progdir-state.

  ENDMETHOD.


  METHOD read_tpool.

    FIELD-SYMBOLS: <ls_tpool_in>  LIKE LINE OF it_tpool,
                   <ls_tpool_out> LIKE LINE OF rt_tpool.


    LOOP AT it_tpool ASSIGNING <ls_tpool_in>.
      APPEND INITIAL LINE TO rt_tpool ASSIGNING <ls_tpool_out>.
      MOVE-CORRESPONDING <ls_tpool_in> TO <ls_tpool_out>.
      IF <ls_tpool_out>-id = 'S'.
        CONCATENATE <ls_tpool_in>-split <ls_tpool_in>-entry
          INTO <ls_tpool_out>-entry
          RESPECTING BLANKS.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  method SERIALIZE_ALL_DOCU.
    data: lt_docu_cat type ty_oo_docu_cat_tt. " catalog of component docu

    data lv_pattern TYPE string.
    data lv_object type DOKU_OBJ.
    data lv_id type DOKU_ID.
    data lt_langu_additional TYPE zif_abapgit_lang_definitions=>ty_langus.
    data lr_id type range of dokhl-id.
    data wa_id like LINE OF lr_id.

    wa_id-sign = 'I'.
    wa_id-option = 'EQ'.
    wa_id-low = IV_ID.
    append wa_id to lr_id.

    if iv_id = 'CL'.
      wa_id-low = 'CA'.  append wa_id to lr_id.
      wa_id-low = 'CE'.  append wa_id to lr_id.
      wa_id-low = 'CO'.  append wa_id to lr_id.
    else.
      wa_id-low = 'IA'.  append wa_id to lr_id.
      wa_id-low = 'IE'.  append wa_id to lr_id.
      wa_id-low = 'IO'.  append wa_id to lr_id.
    endif.

    concatenate iv_clsname '%' into lv_pattern RESPECTING BLANKS.


    SELECT DISTINCT OBJECT, ID FROM DOKHL INTO (@LV_OBJECT, @LV_ID)
      WHERE id       IN   @lr_id
        AND object   LIKE @lv_pattern
        and langu    =    @mv_language.

      SELECT DISTINCT langu
        INTO TABLE lt_langu_additional
        FROM dokhl
        WHERE id     =  lv_id
          AND object =  lv_object
          AND langu  <> mv_language.
        SERIALIZE_DOCU( EXPORTING
                          ii_xml              = ii_xml
                          iv_obj              = lv_object
                          iv_id               = lv_id
                          it_langu_additional = lt_langu_additional
                          ii_object_oriented_object_fct = ii_object_oriented_object_fct ).

        if lv_id <> 'CL' and lv_id <> 'IF'.
          data ls_docu_cat type ty_oo_docu_cat.
          ls_docu_cat-id = lv_id.
          ls_docu_cat-comp = lv_object+30(30).
          APPEND ls_docu_cat to lt_docu_cat.
        endif.
      ENDSELECT.
      ii_xml->add( iv_name = 'DOCU_CAT'
                   ig_data = lt_docu_cat ).

  endmethod.


  METHOD serialize_cua.

    CALL FUNCTION 'RS_CUA_INTERNAL_FETCH'
      EXPORTING
        program         = iv_program_name
        language        = mv_language
        state           = 'A'
      IMPORTING
        adm             = rs_cua-adm
      TABLES
        sta             = rs_cua-sta
        fun             = rs_cua-fun
        men             = rs_cua-men
        mtx             = rs_cua-mtx
        act             = rs_cua-act
        but             = rs_cua-but
        pfk             = rs_cua-pfk
        set             = rs_cua-set
        doc             = rs_cua-doc
        tit             = rs_cua-tit
        biv             = rs_cua-biv
      EXCEPTIONS
        not_found       = 1
        unknown_version = 2
        OTHERS          = 3.
    IF sy-subrc > 1.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  method SERIALIZE_DOCU.
    DATA: lt_lines      TYPE tlinetab,
          lv_langu      TYPE sy-langu,
          lt_i18n_lines TYPE zif_abapgit_lang_definitions=>ty_i18n_lines,
          ls_i18n_lines TYPE zif_abapgit_lang_definitions=>ty_i18n_line,
          lv_name       TYPE string,
          lv_i18n_name  TYPE string.

    " append the object and component name if we're serializing
    " attribute, method or event documentation
    lv_name      = make_docu_tag( iv_prefix = 'LINES'       iv_id = iv_id iv_comp = iv_obj+30(30) ).
    lv_i18n_name = make_docu_tag( iv_prefix = 'I18N_LINES'  iv_id = iv_id iv_comp = iv_obj+30(30) ).

    lt_lines = ii_object_oriented_object_fct->read_documentation(
      iv_id          = iv_id
      iv_object_name = iv_obj
      iv_language    = mv_language ).

    IF lines( lt_lines ) > 0.
      ii_xml->add( iv_name = lv_name
                   ig_data = lt_lines ).
    ENDIF.

    IF ii_xml->i18n_params( )-main_language_only = abap_true.
      RETURN.
    ENDIF.

    LOOP AT it_langu_additional INTO lv_langu.
      lt_lines = ii_object_oriented_object_fct->read_documentation(
        iv_id          = iv_id
        iv_object_name = iv_obj
        iv_language    = lv_langu ).

      IF lines( lt_lines ) > 0.
        CLEAR ls_i18n_lines.
        ls_i18n_lines-language = lv_langu.
        ls_i18n_lines-lines    = lt_lines.
        INSERT ls_i18n_lines INTO TABLE lt_i18n_lines.
      ENDIF.
    ENDLOOP.

    IF lines( lt_i18n_lines ) > 0.
      ii_xml->add( iv_name = lv_i18n_name
                   ig_data = lt_i18n_lines ).
    ENDIF.
  endmethod.


  METHOD serialize_dynpros.
    DATA: ls_header               TYPE rpy_dyhead,
          lt_containers           TYPE dycatt_tab,
          lt_fields_to_containers TYPE dyfatc_tab,
          lt_flow_logic           TYPE swydyflow,
          lt_d020s                TYPE TABLE OF d020s,
          lt_fieldlist_int        TYPE TABLE OF d021s. "internal format

    FIELD-SYMBOLS: <ls_d020s>       LIKE LINE OF lt_d020s,
                   <lv_outputstyle> TYPE scrpostyle,
                   <ls_container>   LIKE LINE OF lt_containers,
                   <ls_field>       LIKE LINE OF lt_fields_to_containers,
                   <ls_dynpro>      LIKE LINE OF rt_dynpro,
                   <ls_field_int>   LIKE LINE OF lt_fieldlist_int.

    "#2746: relevant flag values (taken from include MSEUSBIT)
    CONSTANTS: lc_flg1ddf TYPE x VALUE '20',
               lc_flg3fku TYPE x VALUE '08',
               lc_flg3for TYPE x VALUE '04',
               lc_flg3fdu TYPE x VALUE '02'.


    CALL FUNCTION 'RS_SCREEN_LIST'
      EXPORTING
        dynnr     = ''
        progname  = iv_program_name
      TABLES
        dynpros   = lt_d020s
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc = 2.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    SORT lt_d020s BY dnum ASCENDING.

* loop dynpros and skip generated selection screens
    LOOP AT lt_d020s ASSIGNING <ls_d020s>
        WHERE type <> 'S' AND type <> 'W' AND type <> 'J'
        AND NOT dnum IS INITIAL.

      CALL FUNCTION 'RPY_DYNPRO_READ'
        EXPORTING
          progname             = iv_program_name
          dynnr                = <ls_d020s>-dnum
        IMPORTING
          header               = ls_header
        TABLES
          containers           = lt_containers
          fields_to_containers = lt_fields_to_containers
          flow_logic           = lt_flow_logic
        EXCEPTIONS
          cancelled            = 1
          not_found            = 2
          permission_error     = 3
          OTHERS               = 4.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      "#2746: we need the dynpro fields in internal format:
      FREE lt_fieldlist_int.

      CALL FUNCTION 'RPY_DYNPRO_READ_NATIVE'
        EXPORTING
          progname  = iv_program_name
          dynnr     = <ls_d020s>-dnum
        TABLES
          fieldlist = lt_fieldlist_int.


      LOOP AT lt_fields_to_containers ASSIGNING <ls_field>.
* output style is a NUMC field, the XML conversion will fail if it contains invalid value
* field does not exist in all versions
        ASSIGN COMPONENT 'OUTPUTSTYLE' OF STRUCTURE <ls_field> TO <lv_outputstyle>.
        IF sy-subrc = 0 AND <lv_outputstyle> = '  '.
          CLEAR <lv_outputstyle>.
        ENDIF.

        "2746: we apply the same logic as in SAPLWBSCREEN
        "for setting or unsetting the foreignkey field:
        UNASSIGN <ls_field_int>.
        READ TABLE lt_fieldlist_int ASSIGNING <ls_field_int> WITH KEY fnam = <ls_field>-name.
        IF <ls_field_int> IS ASSIGNED.
          IF <ls_field_int>-flg1 O lc_flg1ddf AND
              <ls_field_int>-flg3 O lc_flg3for AND
              <ls_field_int>-flg3 Z lc_flg3fdu AND
              <ls_field_int>-flg3 Z lc_flg3fku.
            <ls_field>-foreignkey = 'X'.
          ELSE.
            CLEAR <ls_field>-foreignkey.
          ENDIF.
        ENDIF.

        IF <ls_field>-from_dict = abap_true AND
           <ls_field>-modific   <> 'F' AND
           <ls_field>-modific   <> 'X'.
          CLEAR <ls_field>-text.
        ENDIF.
      ENDLOOP.

      LOOP AT lt_containers ASSIGNING <ls_container>.
        IF <ls_container>-c_resize_v = abap_false.
          CLEAR <ls_container>-c_line_min.
        ENDIF.
        IF <ls_container>-c_resize_h = abap_false.
          CLEAR <ls_container>-c_coln_min.
        ENDIF.
      ENDLOOP.

      APPEND INITIAL LINE TO rt_dynpro ASSIGNING <ls_dynpro>.
      <ls_dynpro>-header     = ls_header.
      <ls_dynpro>-containers = lt_containers.
      <ls_dynpro>-fields     = lt_fields_to_containers.

      <ls_dynpro>-flow_logic = lt_flow_logic.

    ENDLOOP.

  ENDMETHOD.


  METHOD serialize_program.

    DATA: ls_progdir      TYPE ty_progdir,
          lv_program_name TYPE programm,
          lt_dynpros      TYPE ty_dynpro_tt,
          ls_cua          TYPE ty_cua,
          lt_source       TYPE TABLE OF abaptxt255,
          lt_tpool        TYPE textpool_table,
          ls_tpool        LIKE LINE OF lt_tpool,
          li_xml          TYPE REF TO zif_abapgit_xml_output.

    IF iv_program IS INITIAL.
      lv_program_name = is_item-obj_name.
    ELSE.
      lv_program_name = iv_program.
    ENDIF.

    zcl_abapgit_language=>set_current_language( mv_language ).

    CALL FUNCTION 'RPY_PROGRAM_READ'
      EXPORTING
        program_name     = lv_program_name
        with_includelist = abap_false
        with_lowercase   = abap_true
      TABLES
        source_extended  = lt_source
        textelements     = lt_tpool
      EXCEPTIONS
        cancelled        = 1
        not_found        = 2
        permission_error = 3
        OTHERS           = 4.

    IF sy-subrc = 2.
      zcl_abapgit_language=>restore_login_language( ).
      RETURN.
    ELSEIF sy-subrc <> 0.
      zcl_abapgit_language=>restore_login_language( ).
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    zcl_abapgit_language=>restore_login_language( ).

    ls_progdir = read_progdir( lv_program_name ).

    IF io_xml IS BOUND.
      li_xml = io_xml.
    ELSE.
      CREATE OBJECT li_xml TYPE zcl_abapgit_xml_output.
    ENDIF.

    li_xml->add( iv_name = 'PROGDIR'
                 ig_data = ls_progdir ).
    IF ls_progdir-subc = '1' OR ls_progdir-subc = 'M'.
      lt_dynpros = serialize_dynpros( lv_program_name ).
      li_xml->add( iv_name = 'DYNPROS'
                   ig_data = lt_dynpros ).

      ls_cua = serialize_cua( lv_program_name ).
      IF NOT ls_cua IS INITIAL.
        li_xml->add( iv_name = 'CUA'
                     ig_data = ls_cua ).
      ENDIF.
    ENDIF.

    READ TABLE lt_tpool WITH KEY id = 'R' INTO ls_tpool.
    IF sy-subrc = 0 AND ls_tpool-key = '' AND ls_tpool-length = 0.
      DELETE lt_tpool INDEX sy-tabix.
    ENDIF.

    li_xml->add( iv_name = 'TPOOL'
                 ig_data = add_tpool( lt_tpool ) ).

    IF NOT io_xml IS BOUND.
      io_files->add_xml( iv_extra = iv_extra
                         ii_xml   = li_xml ).
    ENDIF.

    strip_generation_comments( CHANGING ct_source = lt_source ).

    io_files->add_abap( iv_extra = iv_extra
                        it_abap  = lt_source ).

  ENDMETHOD.


  METHOD strip_generation_comments.

    FIELD-SYMBOLS <lv_line> LIKE LINE OF ct_source.

    IF ms_item-obj_type <> 'FUGR'.
      RETURN.
    ENDIF.

    IF lines( ct_source ) < 5. " Generation header length
      RETURN.
    ENDIF.

    READ TABLE ct_source INDEX 1 ASSIGNING <lv_line>.
    ASSERT sy-subrc = 0.
    IF NOT <lv_line> CP '#*---*'.
      RETURN.
    ENDIF.

    READ TABLE ct_source INDEX 2 ASSIGNING <lv_line>.
    ASSERT sy-subrc = 0.
    IF NOT <lv_line> CP '#**'.
      RETURN.
    ENDIF.

    READ TABLE ct_source INDEX 3 ASSIGNING <lv_line>.
    ASSERT sy-subrc = 0.
    IF NOT <lv_line> CP '#**generation date:*'.
      RETURN.
    ENDIF.

    READ TABLE ct_source INDEX 4 ASSIGNING <lv_line>.
    ASSERT sy-subrc = 0.
    IF NOT <lv_line> CP '#**generator version:*'.
      RETURN.
    ENDIF.

    READ TABLE ct_source INDEX 5 ASSIGNING <lv_line>.
    ASSERT sy-subrc = 0.
    IF NOT <lv_line> CP '#*---*'.
      RETURN.
    ENDIF.

    DELETE ct_source INDEX 4.
    DELETE ct_source INDEX 3.

  ENDMETHOD.


  METHOD uncondense_flow.

    DATA: lv_spaces LIKE LINE OF it_spaces.

    FIELD-SYMBOLS: <ls_flow>   LIKE LINE OF it_flow,
                   <ls_output> LIKE LINE OF rt_flow.


    LOOP AT it_flow ASSIGNING <ls_flow>.
      APPEND INITIAL LINE TO rt_flow ASSIGNING <ls_output>.
      <ls_output>-line = <ls_flow>-line.

      READ TABLE it_spaces INDEX sy-tabix INTO lv_spaces.
      IF sy-subrc = 0.
        SHIFT <ls_output>-line RIGHT BY lv_spaces PLACES IN CHARACTER MODE.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD update_progdir.

    DATA ls_progdir_new TYPE progdir.

    CALL FUNCTION 'READ_PROGDIR'
      EXPORTING
        i_progname = is_progdir-name
        i_state    = 'I'
      IMPORTING
        e_progdir  = ls_progdir_new
      EXCEPTIONS
        not_exists = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error reading program directory' ).
    ENDIF.

    ls_progdir_new-ldbname = is_progdir-ldbname.
    ls_progdir_new-dbna    = is_progdir-dbna.
    ls_progdir_new-dbapl   = is_progdir-dbapl.
    ls_progdir_new-rload   = is_progdir-rload.
    ls_progdir_new-fixpt   = is_progdir-fixpt.
    ls_progdir_new-varcl   = is_progdir-varcl.
    ls_progdir_new-appl    = is_progdir-appl.
    ls_progdir_new-rstat   = is_progdir-rstat.
    ls_progdir_new-sqlx    = is_progdir-sqlx.
    ls_progdir_new-uccheck = is_progdir-uccheck.
    ls_progdir_new-clas    = is_progdir-clas.

    CALL FUNCTION 'UPDATE_PROGDIR'
      EXPORTING
        i_progdir    = ls_progdir_new
        i_progname   = ls_progdir_new-name
        i_state      = ls_progdir_new-state
      EXCEPTIONS
        not_executed = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Error updating program directory' ).
    ENDIF.

    " function UPDATE_PROGDIR does not update VARCL, so we do it here
    SELECT SINGLE * FROM progdir INTO ls_progdir_new
      WHERE name  = ls_progdir_new-name
        AND state = ls_progdir_new-state.
    IF sy-subrc = 0 AND is_progdir-varcl <> ls_progdir_new-varcl.
      UPDATE progdir SET varcl = is_progdir-varcl
        WHERE name  = ls_progdir_new-name
          AND state = ls_progdir_new-state.               "#EC CI_SUBRC
    ENDIF.

  ENDMETHOD.


  METHOD update_program.

    zcl_abapgit_language=>set_current_language( mv_language ).

    CALL FUNCTION 'RPY_PROGRAM_UPDATE'
      EXPORTING
        program_name     = is_progdir-name
        title_string     = iv_title
        save_inactive    = 'I'
      TABLES
        source_extended  = it_source
      EXCEPTIONS
        cancelled        = 1
        permission_error = 2
        not_found        = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      zcl_abapgit_language=>restore_login_language( ).

      IF sy-msgid = 'EU' AND sy-msgno = '510'.
        zcx_abapgit_exception=>raise( 'User is currently editing program' ).
      ELSEIF sy-msgid = 'EU' AND sy-msgno = '522'.
        " for generated table maintenance function groups, the author is set to SAP* instead of the user which
        " generates the function group. This hits some standard checks, pulling new code again sets the author
        " to the current user which avoids the check
        zcx_abapgit_exception=>raise( |Delete function group and pull again, { is_progdir-name } (EU522)| ).
      ELSE.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

    zcl_abapgit_language=>restore_login_language( ).

  ENDMETHOD.
ENDCLASS.
