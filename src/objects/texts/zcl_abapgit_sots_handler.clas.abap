CLASS zcl_abapgit_sots_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_sots,
        header  TYPE sotr_headu,
        entries TYPE sotr_textl_tt,
      END OF ty_sots.
    TYPES:
      ty_sots_tt TYPE STANDARD TABLE OF ty_sots WITH DEFAULT KEY.
    TYPES:
      ty_sots_use_tt TYPE STANDARD TABLE OF sotr_useu WITH DEFAULT KEY.

    CLASS-METHODS read_sots
      IMPORTING
        !iv_pgmid    TYPE pgmid DEFAULT 'R3TR'
        !iv_object   TYPE trobjtype
        !iv_obj_name TYPE csequence
        !io_xml      TYPE REF TO zif_abapgit_xml_output OPTIONAL
        !iv_language TYPE spras OPTIONAL
      EXPORTING
        !et_sots     TYPE ty_sots_tt
        !et_sots_use TYPE ty_sots_use_tt
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS create_sots
      IMPORTING
        !iv_package TYPE devclass
        !io_xml     TYPE REF TO zif_abapgit_xml_input OPTIONAL
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS create_sots_from_data
      IMPORTING
        !iv_package  TYPE devclass
        !it_sots     TYPE ty_sots_tt OPTIONAL
        !it_sots_use TYPE ty_sots_use_tt OPTIONAL
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS delete_sots
      IMPORTING
        !iv_pgmid    TYPE pgmid DEFAULT 'R3TR'
        !iv_object   TYPE trobjtype
        !iv_obj_name TYPE csequence
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.

    CLASS-METHODS get_sots_usage
      IMPORTING
        !iv_pgmid          TYPE pgmid
        !iv_object         TYPE trobjtype
        !iv_obj_name       TYPE csequence
      RETURNING
        VALUE(rt_sots_use) TYPE ty_sots_use_tt.

    CLASS-METHODS get_sots_4_concept
      IMPORTING
        !iv_concept    TYPE sotr_conc
      RETURNING
        VALUE(rs_sots) TYPE ty_sots.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_abapgit_sots_handler IMPLEMENTATION.


  METHOD create_sots.

    DATA:
      lt_sots     TYPE ty_sots_tt,
      lt_sots_use TYPE ty_sots_use_tt.

    io_xml->read( EXPORTING iv_name = 'SOTS'
                  CHANGING  cg_data = lt_sots ).
    io_xml->read( EXPORTING iv_name = 'SOTS_USE'
                  CHANGING  cg_data = lt_sots_use ).

    create_sots_from_data(
      iv_package  = iv_package
      it_sots     = lt_sots
      it_sots_use = lt_sots_use ).

  ENDMETHOD.


  METHOD create_sots_from_data.

    DATA:
      lt_objects         TYPE sotr_objects,
      lv_object          LIKE LINE OF lt_objects,
      lv_subrc           TYPE sy-subrc,
      ls_header          TYPE btfr_head,
      lt_text_tab        TYPE sotr_text_tt,
      lt_string_tab      TYPE sotr_textl_tt,
      ls_entry           LIKE LINE OF lt_string_tab,
      lv_concept         TYPE sotr_conc,
      lv_concept_default TYPE sotr_conc.

    FIELD-SYMBOLS <ls_sots> LIKE LINE OF it_sots.

    LOOP AT it_sots ASSIGNING <ls_sots>.

      CALL FUNCTION 'SOTR_OBJECT_GET_OBJECTS'
        EXPORTING
          object_vector    = <ls_sots>-header-objid_vec
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

      " Reimplementation of SOTR_STRING_CREATE_CONCEPT because we can't supply
      " concept and it would then be generated.

      LOOP AT <ls_sots>-entries INTO ls_entry.
        ls_entry-langu   = <ls_sots>-header-crea_lan.
        ls_entry-concept = <ls_sots>-header-concept.
        INSERT ls_entry INTO TABLE lt_string_tab.
      ENDLOOP.

      MOVE-CORRESPONDING <ls_sots>-header TO ls_header.
      ls_header-paket = iv_package.

      lv_concept = <ls_sots>-header-concept.

      PERFORM btfr_create IN PROGRAM saplsotr_db_string
        USING    lv_object
                 sy-langu
                 abap_false
                 abap_true
        CHANGING lt_text_tab
                 lt_string_tab
                 ls_header
                 lv_concept
                 lv_concept_default
                 lv_subrc.

      CASE lv_subrc.
        WHEN 1.
          MESSAGE e100(sotr_mess) INTO zcx_abapgit_exception=>null.
        WHEN 2.
          MESSAGE e101(sotr_mess) INTO zcx_abapgit_exception=>null.
        WHEN 3.
          MESSAGE i305(sotr_mess) INTO zcx_abapgit_exception=>null.
        WHEN 4.
          " The concept will be created in the non-original system (not an error)
        WHEN 5.
          MESSAGE e504(sotr_mess) INTO zcx_abapgit_exception=>null.
        WHEN 6.
          MESSAGE e035(sotr_mess) INTO zcx_abapgit_exception=>null.
        WHEN 7.
          MESSAGE e170(sotr_mess) INTO zcx_abapgit_exception=>null.
        WHEN 9.
          MESSAGE e102(sotr_mess) INTO zcx_abapgit_exception=>null.
      ENDCASE.

      IF lv_subrc <> 0 AND lv_subrc <> 4.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ENDLOOP.

    CALL FUNCTION 'SOTR_USAGE_STRING_MODIFY'
      EXPORTING
        sotr_usage = it_sots_use.

  ENDMETHOD.


  METHOD delete_sots.

    DATA:
      ls_sots     TYPE ty_sots,
      lt_sots_use TYPE ty_sots_use_tt.

    FIELD-SYMBOLS <ls_sots_use> LIKE LINE OF lt_sots_use.

    lt_sots_use = get_sots_usage( iv_pgmid    = iv_pgmid
                                  iv_object   = iv_object
                                  iv_obj_name = iv_obj_name ).

    " Remove any usage to ensure deletion, see function module BTFR_CHECK
    DELETE sotr_useu FROM TABLE lt_sots_use ##SUBRC_OK.

    LOOP AT lt_sots_use ASSIGNING <ls_sots_use> WHERE concept IS NOT INITIAL.

      CALL FUNCTION 'BTFR_DELETE_SINGLE_TEXT'
        EXPORTING
          concept             = <ls_sots_use>-concept
          flag_string         = abap_true
        EXCEPTIONS
          text_not_found      = 1 "ok
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


  METHOD get_sots_4_concept.

    DATA: ls_header  TYPE ty_sots-header,
          lt_entries TYPE ty_sots-entries.

    FIELD-SYMBOLS <ls_entry> LIKE LINE OF lt_entries.

    CALL FUNCTION 'SOTR_STRING_GET_CONCEPT'
      EXPORTING
        concept        = iv_concept
      IMPORTING
        header         = ls_header
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

    rs_sots-header  = ls_header.
    rs_sots-entries = lt_entries.

  ENDMETHOD.


  METHOD get_sots_usage.

    DATA: lv_obj_name TYPE trobj_name.

    lv_obj_name = iv_obj_name.

    " Objects with multiple components
    IF iv_pgmid = 'LIMU' AND ( iv_object CP 'WDY*' OR iv_object = 'WAPP' ).
      lv_obj_name+30 = '%'.
    ENDIF.

    CALL FUNCTION 'SOTR_USAGE_STRING_READ'
      EXPORTING
        pgmid          = iv_pgmid
        object         = iv_object
        obj_name       = lv_obj_name
      IMPORTING
        sotr_usage     = rt_sots_use
      EXCEPTIONS
        no_entry_found = 1
        error_in_pgmid = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      SORT rt_sots_use.
    ENDIF.

  ENDMETHOD.


  METHOD read_sots.

    FIELD-SYMBOLS <ls_sots_use> LIKE LINE OF et_sots_use.

    DATA ls_sots TYPE ty_sots.

    " OTR long text (string) usage: see TABLE BTFR_OBJ_IDS
    " LIMU: CPUB, WAPP
    " R3TR: SICF, SMIF, XSLT

    et_sots_use = get_sots_usage( iv_pgmid    = iv_pgmid
                                  iv_object   = iv_object
                                  iv_obj_name = iv_obj_name ).

    LOOP AT et_sots_use ASSIGNING <ls_sots_use> WHERE concept IS NOT INITIAL.
      ls_sots = get_sots_4_concept( <ls_sots_use>-concept ).

      IF io_xml IS BOUND AND
         io_xml->i18n_params( )-main_language_only = abap_true AND
         iv_language IS SUPPLIED.
        DELETE ls_sots-entries WHERE langu <> iv_language.
        CHECK ls_sots-entries IS NOT INITIAL.
      ENDIF.

      INSERT ls_sots INTO TABLE et_sots.
    ENDLOOP.

    IF io_xml IS BOUND.
      io_xml->add( iv_name = 'SOTS'
                   ig_data = et_sots ).
      io_xml->add( iv_name = 'SOTS_USE'
                   ig_data = et_sots_use ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
