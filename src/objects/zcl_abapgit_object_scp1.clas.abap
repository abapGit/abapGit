CLASS zcl_abapgit_object_scp1 DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_scp1,
        scprattr TYPE scprattr,
        scprtext TYPE STANDARD TABLE OF scprtext WITH DEFAULT KEY,
        scprvals TYPE STANDARD TABLE OF scprvals WITH DEFAULT KEY,
        scprvall TYPE STANDARD TABLE OF scprvall WITH DEFAULT KEY,
        scprreca TYPE STANDARD TABLE OF scprreca WITH DEFAULT KEY,
        scprfldv TYPE STANDARD TABLE OF scprfldv WITH DEFAULT KEY,
      END OF ty_scp1 .

    METHODS dequeue .
    METHODS enqueue
      RAISING
        zcx_abapgit_exception .
    METHODS save
      IMPORTING
        !is_scp1 TYPE ty_scp1
      RAISING
        zcx_abapgit_exception .
    METHODS adjust_inbound
      CHANGING
        !cs_scp1 TYPE ty_scp1 .
    METHODS adjust_outbound
      CHANGING
        !cs_scp1 TYPE ty_scp1 .
    METHODS load
      CHANGING
        !cs_scp1 TYPE ty_scp1 .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_object_scp1 IMPLEMENTATION.


  METHOD adjust_inbound.

    FIELD-SYMBOLS: <ls_scprvals> TYPE scprvals,
                   <ls_scprreca> TYPE scprreca.

* back to internal format
    LOOP AT cs_scp1-scprvals ASSIGNING <ls_scprvals>.
      SHIFT <ls_scprvals>-recnumber RIGHT DELETING TRAILING space.
    ENDLOOP.
    LOOP AT cs_scp1-scprreca ASSIGNING <ls_scprreca>.
      SHIFT <ls_scprreca>-recnumber RIGHT DELETING TRAILING space.
    ENDLOOP.

  ENDMETHOD.


  METHOD adjust_outbound.

    FIELD-SYMBOLS: <ls_scprvals> TYPE scprvals,
                   <ls_scprreca> TYPE scprreca.

* normalize the XML
    LOOP AT cs_scp1-scprvals ASSIGNING <ls_scprvals>.
      CONDENSE <ls_scprvals>-recnumber.
    ENDLOOP.
    LOOP AT cs_scp1-scprreca ASSIGNING <ls_scprreca>.
      CONDENSE <ls_scprreca>-recnumber.
    ENDLOOP.

  ENDMETHOD.


  METHOD dequeue.

    DATA: lv_id TYPE scpr_id.


    lv_id = ms_item-obj_name.

    CALL FUNCTION 'SCPR_SV_DEQUEUE_BCSET'
      EXPORTING
        bcset_id = lv_id.

  ENDMETHOD.


  METHOD enqueue.

    DATA: lv_id TYPE scpr_id.

    lv_id = ms_item-obj_name.

    CALL FUNCTION 'SCPR_SV_ENQUEUE_BCSET'
      EXPORTING
        bcset_id          = lv_id
      EXCEPTIONS
        is_already_locked = 1
        system_failure    = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |SCP1 locking error| ).
    ENDIF.

  ENDMETHOD.


  METHOD load.

    CALL FUNCTION 'SCPR_TEXT_GET'
      EXPORTING
        profid        = cs_scp1-scprattr-id
        category      = cs_scp1-scprattr-category
      TABLES
        texts         = cs_scp1-scprtext
      EXCEPTIONS
        no_text_found = 1.

    CALL FUNCTION 'SCPR_TEMPL_DB_VALS_GET_DETAIL'
      EXPORTING
        profid   = cs_scp1-scprattr-id
        category = cs_scp1-scprattr-category
      TABLES
        values   = cs_scp1-scprvals
        valuesl  = cs_scp1-scprvall
        recattr  = cs_scp1-scprreca.

    CALL FUNCTION 'SCPR_TEMPL_DB_FLDTXTVAR_GET'
      EXPORTING
        bcset_id = cs_scp1-scprattr-id
        category = cs_scp1-scprattr-category
      TABLES
        it_fldv  = cs_scp1-scprfldv.

  ENDMETHOD.


  METHOD save.

    DATA: ls_scp1 TYPE ty_scp1,
          ls_text TYPE scprtext.


* copy everything to local, the function module changes the values
    ls_scp1 = is_scp1.

    READ TABLE ls_scp1-scprtext INTO ls_text WITH KEY langu = sy-langu.

    CALL FUNCTION 'SCPR_TEMPL_MN_TEMPLATE_SAVE'
      EXPORTING
        profid                    = ls_scp1-scprattr-id
        proftext                  = ls_text-text
        category                  = ls_scp1-scprattr-category
        cli_dep                   = ls_scp1-scprattr-cli_dep
        cli_cas                   = ls_scp1-scprattr-cli_cas
        reftype                   = ls_scp1-scprattr-reftype
        refname                   = ls_scp1-scprattr-refname
        orgid                     = ls_scp1-scprattr-orgid
        component                 = ls_scp1-scprattr-component
        minrelease                = ls_scp1-scprattr-minrelease
        maxrelease                = ls_scp1-scprattr-maxrelease
        act_info                  = ls_scp1-scprattr-act_info
        bcset_type                = ls_scp1-scprattr-type
        fldtxtvar_supplied        = 'YES'
        with_transp_insert        = abap_false
        with_progress_indicator   = abap_true
        remove_denied_data        = abap_true
        ask_for_cont_after_remove = abap_true
      TABLES
        values                    = ls_scp1-scprvals
        valuesl                   = ls_scp1-scprvall
        recattr                   = ls_scp1-scprreca
        it_fldv                   = ls_scp1-scprfldv
        texts                     = ls_scp1-scprtext
      EXCEPTIONS
        user_abort                = 1
        error_in_transport_layer  = 2
        inconsistent_data         = 3
        database_error            = 4
        OTHERS                    = 5.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error saving SCP1, { sy-tabix }| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE modifier INTO rv_user FROM scprattr
      WHERE id = ms_item-obj_name
      AND version = 'N'.
    IF sy-subrc <> 0 OR rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~compare_to_remote_version.

    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lv_id TYPE scpr_id.

    lv_id = ms_item-obj_name.

    enqueue( ).

* todo, this gives a popup
    CALL FUNCTION 'SCPR_CTRL_DELETE'
      EXPORTING
        profid             = lv_id
      EXCEPTIONS
        user_abort         = 1
        profile_dont_exist = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |error while deleting SCP1, { sy-subrc }| ).
    ENDIF.

    dequeue( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_scp1 TYPE ty_scp1.


    io_xml->read(
      EXPORTING iv_name = 'SCP1'
      CHANGING  cg_data = ls_scp1 ).

    adjust_inbound( CHANGING cs_scp1 = ls_scp1 ).

    IF ls_scp1-scprattr-type = 'TMP'.
* todo, function module SCPR_PRSET_MN_BCSET_SAVE
      zcx_abapgit_exception=>raise( |todo, SCP1| ).
    ELSE.
      save( ls_scp1 ).
    ENDIF.

    dequeue( ).

    tadir_insert( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_rc     TYPE sy-subrc,
          lv_profid TYPE scprattr-id.


    lv_profid = ms_item-obj_name.

    CALL FUNCTION 'SCPR_BCSET_EXISTS'
      EXPORTING
        profid = lv_profid
      IMPORTING
        rc     = lv_rc.
    rv_bool = boolc( lv_rc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.

    rs_metadata = get_metadata( ).

  ENDMETHOD.


  METHOD zif_abapgit_object~has_changed_since.

    rv_changed = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    DATA: lv_display_only TYPE scpr_txt20,
          lv_bcset_id     TYPE scpr_id.

    lv_display_only = abap_false.
    lv_bcset_id     = ms_item-obj_name.

    EXPORT scpr3_display_only = lv_display_only
           scpr3_bcset_id     = lv_bcset_id
        TO MEMORY ID 'SCPR3_PARAMETER'.

    SUBMIT scpr3 AND RETURN.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: ls_scp1 TYPE ty_scp1.


    ls_scp1-scprattr-id = ms_item-obj_name.

    CALL FUNCTION 'SCPR_DB_ATTR_GET_DETAIL'
      EXPORTING
        profid     = ls_scp1-scprattr-id
      IMPORTING
        proftype   = ls_scp1-scprattr-type
        cli_dep    = ls_scp1-scprattr-cli_dep
        cli_cas    = ls_scp1-scprattr-cli_cas
        reftype    = ls_scp1-scprattr-reftype
        refname    = ls_scp1-scprattr-refname
        component  = ls_scp1-scprattr-component
        minrelease = ls_scp1-scprattr-minrelease
        maxrelease = ls_scp1-scprattr-maxrelease
        orgid      = ls_scp1-scprattr-orgid
        act_info   = ls_scp1-scprattr-act_info.

    IF ls_scp1-scprattr-type = 'TMP'.
* todo, Hierarchical, fm SCPR_PRSET_DB_SUBP_GET_DETAIL, recursive?
      zcx_abapgit_exception=>raise( |todo, SCP1| ).
    ELSE.
      load( CHANGING cs_scp1 = ls_scp1 ).
    ENDIF.

    adjust_outbound( CHANGING cs_scp1 = ls_scp1 ).

    io_xml->add(
      iv_name = 'SCP1'
      ig_data  = ls_scp1 ).

  ENDMETHOD.
ENDCLASS.
