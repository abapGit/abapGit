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

    METHODS get
      CHANGING
        !cs_scp1 TYPE ty_scp1 .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_SCP1 IMPLEMENTATION.


  METHOD get.

    FIELD-SYMBOLS: <ls_scprvals> TYPE scprvals,
                   <ls_scprreca> TYPE scprreca.


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

* normalize the XML
    LOOP AT cs_scp1-scprvals ASSIGNING <ls_scprvals>.
      CONDENSE <ls_scprvals>-recnumber.
    ENDLOOP.
    LOOP AT cs_scp1-scprreca ASSIGNING <ls_scprreca>.
      CONDENSE <ls_scprreca>-recnumber.
    ENDLOOP.

    CALL FUNCTION 'SCPR_TEMPL_DB_FLDTXTVAR_GET'
      EXPORTING
        bcset_id = cs_scp1-scprattr-id
        category = cs_scp1-scprattr-category
      TABLES
        it_fldv  = cs_scp1-scprfldv.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    rv_user = c_user_unknown.

  ENDMETHOD.


  METHOD zif_abapgit_object~compare_to_remote_version.

    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

* todo

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

* todo

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_profid TYPE scprattr-id.


    lv_profid = ms_item-obj_name.

    CALL FUNCTION 'SCPR_DB_ATTR_GET_DETAIL'
      EXPORTING
        profid             = lv_profid
      EXCEPTIONS
        profile_dont_exist = 1
        OTHERS             = 2.
    rv_bool = boolc( sy-subrc = 0 ).

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

    zcx_abapgit_exception=>raise( |TODO: Jump| ).

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
* todo, Hierarchical
      BREAK-POINT.
    ELSE.
      get( CHANGING cs_scp1 = ls_scp1 ).
    ENDIF.


    io_xml->add(
      iv_name = 'SCP1'
      ig_data  = ls_scp1 ).

  ENDMETHOD.
ENDCLASS.
