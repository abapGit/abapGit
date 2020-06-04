CLASS zcl_abapgit_sotr_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: yt_sotr_use   TYPE STANDARD TABLE OF sotr_use WITH DEFAULT KEY,
           yt_seocompodf TYPE STANDARD TABLE OF seocompodf WITH DEFAULT KEY.

    CLASS-METHODS read_sotr_wda
      IMPORTING
        iv_object_name TYPE sobj_name
      RETURNING
        VALUE(rt_sotr) TYPE zif_abapgit_definitions=>ty_sotr_tt
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS read_sotr_seocomp
      IMPORTING
        iv_object_name TYPE sobj_name
      RETURNING
        VALUE(rt_sotr) TYPE zif_abapgit_definitions=>ty_sotr_tt
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS create_sotr
      IMPORTING
        iv_package TYPE devclass
        it_sotr    TYPE zif_abapgit_definitions=>ty_sotr_tt
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
    CLASS-METHODS get_sotr_4_concept
      IMPORTING
        iv_concept     TYPE sotr_conc
      RETURNING
        VALUE(rt_sotr) TYPE zif_abapgit_definitions=>ty_sotr_tt.

ENDCLASS.



CLASS ZCL_ABAPGIT_SOTR_HANDLER IMPLEMENTATION.


  METHOD create_sotr.
    DATA: lt_sotr    TYPE zif_abapgit_definitions=>ty_sotr_tt,
          lt_objects TYPE sotr_objects,
          ls_paket   TYPE sotr_pack,
          lv_object  LIKE LINE OF lt_objects.

    FIELD-SYMBOLS: <ls_sotr> LIKE LINE OF lt_sotr.

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
        zcx_abapgit_exception=>raise( |error from SOTR_OBJECT_GET_OBJECTS. Subrc = { sy-subrc }| ).
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
        zcx_abapgit_exception=>raise( |Error from SOTR_CREATE_CONCEPT. Subrc = { sy-subrc }| ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_sotr_4_concept.
    DATA: ls_header   TYPE sotr_head,
          lt_entries  TYPE sotr_text_tt,
          lv_obj_name TYPE trobj_name.

    FIELD-SYMBOLS: <ls_sotr>  LIKE LINE OF rt_sotr,
                   <ls_entry> LIKE LINE OF lt_entries.

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
           ls_header-chan_tstut.

    LOOP AT lt_entries ASSIGNING <ls_entry>.
      CLEAR: <ls_entry>-version,
             <ls_entry>-crea_name,
             <ls_entry>-crea_tstut,
             <ls_entry>-chan_name,
             <ls_entry>-chan_tstut.
    ENDLOOP.

    APPEND INITIAL LINE TO rt_sotr ASSIGNING <ls_sotr>.
    <ls_sotr>-header = ls_header.
    <ls_sotr>-entries = lt_entries.

  ENDMETHOD.


  METHOD read_sotr_seocomp.
    DATA: lv_concept    TYPE sotr_head-concept.
    DATA: lt_seocompodf TYPE yt_seocompodf.
    FIELD-SYMBOLS <ls_seocompodf> TYPE seocompodf.

    SELECT * FROM seocompodf
      INTO TABLE lt_seocompodf
      WHERE clsname = iv_object_name
      AND version = '1'
      AND exposure = '2'
      AND attdecltyp = '2'
      AND type = 'SOTR_CONC'
      ORDER BY PRIMARY KEY.                               "#EC CI_SUBRC

    IF sy-subrc = 0.
      LOOP AT lt_seocompodf ASSIGNING <ls_seocompodf>.
        lv_concept = translate( val = <ls_seocompodf>-attvalue
                                from = ''''
                                to = '' ).
        rt_sotr = get_sotr_4_concept( lv_concept ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD read_sotr_wda.
    DATA: lv_concept  TYPE sotr_head-concept.
    DATA: lt_sotr_use TYPE yt_sotr_use.
    DATA: lv_obj_name TYPE trobj_name.
    FIELD-SYMBOLS <ls_sotr_use> TYPE sotr_use.

    lv_obj_name = |{ iv_object_name }%|. "Existence check via WDR_REPOSITORY_INFO should have been done earlier
    CALL FUNCTION 'SOTR_USAGE_READ'
      EXPORTING
        pgmid          = 'LIMU'                 " Program ID in requests and tasks
        object         = 'WDYV'                 " Object Type
        obj_name       = lv_obj_name
      IMPORTING
        sotr_usage     = lt_sotr_use
      EXCEPTIONS
        no_entry_found = 1
        error_in_pgmid = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      LOOP AT lt_sotr_use ASSIGNING <ls_sotr_use>.
        lv_concept = translate( val = <ls_sotr_use>-concept
                                from = ''''
                                to = '' ).
        rt_sotr = get_sotr_4_concept( lv_concept ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
