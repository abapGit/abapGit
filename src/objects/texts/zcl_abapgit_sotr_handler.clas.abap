CLASS zcl_abapgit_sotr_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS read_sotr
      IMPORTING
        !iv_pgmid    TYPE pgmid DEFAULT 'R3TR'
        !iv_object   TYPE trobjtype
        !iv_obj_name TYPE csequence
        !io_xml      TYPE REF TO zif_abapgit_xml_output OPTIONAL
        !iv_language TYPE spras OPTIONAL
      EXPORTING
        !et_sotr     TYPE zif_abapgit_definitions=>ty_sotr_tt
        !et_sotr_use TYPE zif_abapgit_definitions=>ty_sotr_use_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS create_sotr
      IMPORTING
        !iv_package TYPE devclass
        !io_xml     TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS create_sotr_from_data
      IMPORTING
        !iv_package  TYPE devclass
        !it_sotr     TYPE zif_abapgit_definitions=>ty_sotr_tt
        !it_sotr_use TYPE zif_abapgit_definitions=>ty_sotr_use_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS delete_sotr
      IMPORTING
        !iv_pgmid    TYPE pgmid DEFAULT 'R3TR'
        !iv_object   TYPE trobjtype
        !iv_obj_name TYPE csequence
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS delete_sotr_package
      IMPORTING
        !iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
    CLASS-METHODS get_sotr_usage
      IMPORTING
        !iv_pgmid          TYPE pgmid
        !iv_object         TYPE trobjtype
        !iv_obj_name       TYPE csequence
      RETURNING
        VALUE(rt_sotr_use) TYPE zif_abapgit_definitions=>ty_sotr_use_tt.

    CLASS-METHODS get_sotr_4_concept
      IMPORTING
        !iv_concept    TYPE sotr_conc
      RETURNING
        VALUE(rs_sotr) TYPE zif_abapgit_definitions=>ty_sotr .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_sotr_handler IMPLEMENTATION.


  METHOD create_sotr.

    DATA:
      lt_sotr     TYPE zif_abapgit_definitions=>ty_sotr_tt,
      lt_sotr_use TYPE zif_abapgit_definitions=>ty_sotr_use_tt.

    io_xml->read( EXPORTING iv_name = 'SOTR'
                  CHANGING cg_data = lt_sotr ).
    io_xml->read( EXPORTING iv_name = 'SOTR_USE'
                  CHANGING cg_data = lt_sotr_use ).

    create_sotr_from_data(
      iv_package  = iv_package
      it_sotr     = lt_sotr
      it_sotr_use = lt_sotr_use ).

  ENDMETHOD.


  METHOD create_sotr_from_data.

    DATA:
      lt_objects TYPE sotr_objects,
      ls_paket   TYPE sotr_pack,
      lv_object  LIKE LINE OF lt_objects.

    FIELD-SYMBOLS: <ls_sotr> LIKE LINE OF it_sotr.

    LOOP AT it_sotr ASSIGNING <ls_sotr>.
      CALL FUNCTION 'SOTR_OBJECT_GET_OBJECTS'
        EXPORTING
          object_vector    = <ls_sotr>-header-objid_vec
        IMPORTING
          objects          = lt_objects
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      READ TABLE lt_objects INDEX 1 INTO lv_object.
      ASSERT sy-subrc = 0.

      ls_paket-paket = iv_package.

      CALL FUNCTION 'SOTR_CREATE_CONCEPT'
        EXPORTING
          paket                         = ls_paket
          crea_lan                      = <ls_sotr>-header-crea_lan
          alias_name                    = <ls_sotr>-header-alias_name
          object                        = lv_object
          entries                       = <ls_sotr>-entries
          concept_default               = <ls_sotr>-header-concept
        EXCEPTIONS
          package_missing               = 1
          crea_lan_missing              = 2
          object_missing                = 3
          paket_does_not_exist          = 4
          alias_already_exist           = 5
          object_type_not_found         = 6
          langu_missing                 = 7
          identical_context_not_allowed = 8
          text_too_long                 = 9
          error_in_update               = 10
          no_master_langu               = 11
          error_in_concept_id           = 12
          alias_not_allowed             = 13
          tadir_entry_creation_failed   = 14
          internal_error                = 15
          error_in_correction           = 16
          user_cancelled                = 17
          no_entry_found                = 18
          OTHERS                        = 19.
      IF sy-subrc <> 0 AND sy-subrc <> 5.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'SOTR_USAGE_MODIFY'
      EXPORTING
        sotr_usage = it_sotr_use.

  ENDMETHOD.


  METHOD delete_sotr.

    DATA lt_sotr_use TYPE zif_abapgit_definitions=>ty_sotr_use_tt.

    FIELD-SYMBOLS <ls_sotr_use> LIKE LINE OF lt_sotr_use.

    lt_sotr_use = get_sotr_usage( iv_pgmid    = iv_pgmid
                                  iv_object   = iv_object
                                  iv_obj_name = iv_obj_name ).

    LOOP AT lt_sotr_use ASSIGNING <ls_sotr_use> WHERE concept IS NOT INITIAL.

      CALL FUNCTION 'SOTR_DELETE_CONCEPT'
        EXPORTING
          concept             = <ls_sotr_use>-concept
        EXCEPTIONS
          no_entry_found      = 1
          text_not_found      = 2
          invalid_package     = 3
          text_not_changeable = 4
          text_enqueued       = 5
          no_correction       = 6
          parameter_error     = 7
          OTHERS              = 8.
      IF sy-subrc > 2.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD delete_sotr_package.

    DATA lt_sotr_head TYPE STANDARD TABLE OF sotr_head WITH DEFAULT KEY.
    DATA lv_obj_name TYPE tadir-obj_name.

    FIELD-SYMBOLS <ls_sotr_head> LIKE LINE OF lt_sotr_head.

    SELECT * FROM sotr_head INTO TABLE lt_sotr_head WHERE paket = iv_package.

    LOOP AT lt_sotr_head ASSIGNING <ls_sotr_head> WHERE concept IS NOT INITIAL.

      CALL FUNCTION 'SOTR_DELETE_CONCEPT'
        EXPORTING
          concept             = <ls_sotr_head>-concept
        EXCEPTIONS
          no_entry_found      = 1
          text_not_found      = 2
          invalid_package     = 3
          text_not_changeable = 4
          text_enqueued       = 5
          no_correction       = 6
          parameter_error     = 7
          OTHERS              = 8.
      IF sy-subrc > 2.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ENDLOOP.

    " Nothing left, then delete SOTR from TADIR
    SELECT * FROM sotr_head INTO TABLE lt_sotr_head WHERE paket = iv_package.
    IF sy-subrc <> 0.
      SELECT SINGLE obj_name FROM tadir INTO lv_obj_name
        WHERE pgmid = 'R3TR' AND object = 'SOTR' AND obj_name = iv_package.
      IF sy-subrc = 0.
        CALL FUNCTION 'TR_TADIR_INTERFACE'
          EXPORTING
            wi_delete_tadir_entry = abap_true
            wi_test_modus         = abap_false
            wi_tadir_pgmid        = 'R3TR'
            wi_tadir_object       = 'SOTR'
            wi_tadir_obj_name     = lv_obj_name
          EXCEPTIONS
            OTHERS                = 1 ##FM_SUBRC_OK.

        IF zcl_abapgit_factory=>get_sap_package( iv_package )->are_changes_recorded_in_tr_req( ) = abap_true.
          CALL FUNCTION 'RS_CORR_INSERT'
            EXPORTING
              object              = lv_obj_name
              object_class        = 'SOTR'
              mode                = 'D'
              global_lock         = abap_true
              devclass            = iv_package
              suppress_dialog     = abap_true
            EXCEPTIONS
              cancelled           = 1
              permission_failure  = 2
              unknown_objectclass = 3
              OTHERS              = 4.
          IF sy-subrc <> 0.
            zcx_abapgit_exception=>raise_t100( ).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_sotr_4_concept.

    DATA: ls_header  TYPE zif_abapgit_definitions=>ty_sotr-header,
          lt_entries TYPE zif_abapgit_definitions=>ty_sotr-entries.

    FIELD-SYMBOLS: <ls_entry> LIKE LINE OF lt_entries.

    CALL FUNCTION 'SOTR_GET_CONCEPT'
      EXPORTING
        concept        = iv_concept
      IMPORTING
        header         = ls_header
      TABLES
        entries        = lt_entries
      EXCEPTIONS
        no_entry_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CLEAR: ls_header-paket,
           ls_header-crea_name,
           ls_header-crea_tstut,
           ls_header-chan_name,
           ls_header-chan_tstut,
           ls_header-system_id.

    LOOP AT lt_entries ASSIGNING <ls_entry>.
      CLEAR: <ls_entry>-version,
             <ls_entry>-crea_name,
             <ls_entry>-crea_tstut,
             <ls_entry>-chan_name,
             <ls_entry>-chan_tstut.
    ENDLOOP.

    rs_sotr-header  = ls_header.
    rs_sotr-entries = lt_entries.

  ENDMETHOD.


  METHOD get_sotr_usage.

    DATA: lv_obj_name TYPE trobj_name.

    lv_obj_name = iv_obj_name.

    " Objects with multiple components
    IF iv_pgmid = 'LIMU' AND ( iv_object CP 'WDY*' OR iv_object = 'WAPP' ).
      lv_obj_name+30 = '%'.
    ENDIF.

    CALL FUNCTION 'SOTR_USAGE_READ'
      EXPORTING
        pgmid          = iv_pgmid
        object         = iv_object
        obj_name       = lv_obj_name
      IMPORTING
        sotr_usage     = rt_sotr_use
      EXCEPTIONS
        no_entry_found = 1
        error_in_pgmid = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      SORT rt_sotr_use.
    ENDIF.

  ENDMETHOD.


  METHOD read_sotr.

    FIELD-SYMBOLS <ls_sotr_use> LIKE LINE OF et_sotr_use.

    DATA: lv_sotr            TYPE zif_abapgit_definitions=>ty_sotr,
          lt_language_filter TYPE zif_abapgit_environment=>ty_system_language_filter.

    " SOTR usage (see LSOTR_SYSTEM_SETTINGSF01, FORM GET_OBJECT_TABLE)
    " LIMU: CPUB, WAPP, WDYC, WDYD, WDYV
    " R3TR: ENHC, ENHD, ENHO, ENHS, ENSC, SCGR, SICF, WDCA, WDCC, WDYA, WEBI, WEBS, XSLT

    et_sotr_use = get_sotr_usage( iv_pgmid    = iv_pgmid
                                  iv_object   = iv_object
                                  iv_obj_name = iv_obj_name ).

    LOOP AT et_sotr_use ASSIGNING <ls_sotr_use> WHERE concept IS NOT INITIAL.
      lv_sotr = get_sotr_4_concept( <ls_sotr_use>-concept ).

      IF io_xml IS BOUND AND
         io_xml->i18n_params( )-main_language_only = abap_true AND
         iv_language IS SUPPLIED.
        DELETE lv_sotr-entries WHERE langu <> iv_language.
        CHECK lv_sotr-entries IS NOT INITIAL.
      ENDIF.
      lt_language_filter = zcl_abapgit_factory=>get_environment( )->get_system_language_filter( ).
      DELETE lv_sotr-entries WHERE NOT langu IN lt_language_filter.
      CHECK lv_sotr-entries IS NOT INITIAL.

      INSERT lv_sotr INTO TABLE et_sotr.
    ENDLOOP.

    IF io_xml IS BOUND.
      io_xml->add( iv_name = 'SOTR'
                   ig_data = et_sotr ).
      io_xml->add( iv_name = 'SOTR_USE'
                   ig_data = et_sotr_use ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
