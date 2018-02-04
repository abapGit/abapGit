CLASS zcl_abapgit_object_shlp DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

ENDCLASS.

CLASS zcl_abapgit_object_shlp IMPLEMENTATION.

  METHOD zif_abapgit_object~has_changed_since.

    DATA: lv_date TYPE dats,
          lv_time TYPE tims.

    SELECT SINGLE as4date as4time FROM dd30l
       INTO (lv_date, lv_time)
       WHERE shlpname = ms_item-obj_name
       AND as4local = 'A'.                              "#EC CI_GENBUFF

    rv_changed = check_timestamp(
     iv_timestamp = iv_timestamp
     iv_date      = lv_date
     iv_time      = lv_time ).

  ENDMETHOD.  "zif_abapgit_object~has_changed_since

  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE as4user FROM dd30l INTO rv_user
      WHERE shlpname = ms_item-obj_name
      AND as4local = 'A'.                               "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.

  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-ddic = abap_true.
  ENDMETHOD.                    "zif_abapgit_object~get_metadata

  METHOD zif_abapgit_object~exists.

    DATA: lv_shlpname TYPE dd30l-shlpname.


    SELECT SINGLE shlpname FROM dd30l INTO lv_shlpname
      WHERE shlpname = ms_item-obj_name
      AND as4local = 'A'.                               "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "zif_abapgit_object~exists

  METHOD zif_abapgit_object~jump.

    jump_se11( iv_radio = 'RSRD1-SHMA'
               iv_field = 'RSRD1-SHMA_VAL' ).

  ENDMETHOD.                    "jump

  METHOD zif_abapgit_object~delete.

    DATA: lv_objname TYPE rsedd0-ddobjname.


    lv_objname = ms_item-obj_name.

    CALL FUNCTION 'RS_DD_DELETE_OBJ'
      EXPORTING
        no_ask               = abap_true
        objname              = lv_objname
        objtype              = 'H'
      EXCEPTIONS
        not_executed         = 1
        object_not_found     = 2
        object_not_specified = 3
        permission_failure   = 4.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from RS_DD_DELETE_OBJ, SHLP' ).
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD zif_abapgit_object~serialize.

    DATA: lv_name  TYPE ddobjname,
          ls_dd30v TYPE dd30v,
          lt_dd31v TYPE TABLE OF dd31v,
          lt_dd32p TYPE TABLE OF dd32p,
          lt_dd33v TYPE TABLE OF dd33v.

    FIELD-SYMBOLS: <ls_dd32p> LIKE LINE OF lt_dd32p.


    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_SHLP_GET'
      EXPORTING
        name          = lv_name
        state         = 'A'
        langu         = mv_language
      IMPORTING
        dd30v_wa      = ls_dd30v
      TABLES
        dd31v_tab     = lt_dd31v
        dd32p_tab     = lt_dd32p
        dd33v_tab     = lt_dd33v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from DDIF_SHLP_GET' ).
    ENDIF.
    IF ls_dd30v IS INITIAL.
      RETURN. " does not exist in system
    ENDIF.

    CLEAR: ls_dd30v-as4user,
           ls_dd30v-as4date,
           ls_dd30v-as4time.

    LOOP AT lt_dd32p ASSIGNING <ls_dd32p>.
* clear information inherited from domain
      CLEAR: <ls_dd32p>-domname,
        <ls_dd32p>-headlen,
        <ls_dd32p>-scrlen1,
        <ls_dd32p>-scrlen2,
        <ls_dd32p>-datatype,
        <ls_dd32p>-leng,
        <ls_dd32p>-outputlen,
        <ls_dd32p>-decimals,
        <ls_dd32p>-lowercase,
        <ls_dd32p>-signflag,
        <ls_dd32p>-convexit.
    ENDLOOP.

    io_xml->add( iv_name = 'DD30V'
                 ig_data = ls_dd30v ).
    io_xml->add( ig_data = lt_dd31v
                 iv_name = 'DD31V_TABLE' ).
    io_xml->add( ig_data = lt_dd32p
                 iv_name = 'DD32P_TABLE' ).
    io_xml->add( ig_data = lt_dd33v
                 iv_name = 'DD33V_TABLE' ).

  ENDMETHOD.                    "serialize

  METHOD zif_abapgit_object~deserialize.

    DATA: lv_name  TYPE ddobjname,
          ls_dd30v TYPE dd30v,
          lt_dd31v TYPE TABLE OF dd31v,
          lt_dd32p TYPE TABLE OF dd32p,
          lt_dd33v TYPE TABLE OF dd33v.


    io_xml->read( EXPORTING iv_name = 'DD30V'
                  CHANGING cg_data = ls_dd30v ).
    io_xml->read( EXPORTING iv_name = 'DD31V_TABLE'
                  CHANGING cg_data = lt_dd31v ).
    io_xml->read( EXPORTING iv_name = 'DD32P_TABLE'
                  CHANGING cg_data = lt_dd32p ).
    io_xml->read( EXPORTING iv_name = 'DD33V_TABLE'
                  CHANGING cg_data = lt_dd33v ).

    corr_insert( iv_package ).

    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_SHLP_PUT'
      EXPORTING
        name              = lv_name
        dd30v_wa          = ls_dd30v
      TABLES
        dd31v_tab         = lt_dd31v
        dd32p_tab         = lt_dd32p
        dd33v_tab         = lt_dd33v
      EXCEPTIONS
        shlp_not_found    = 1
        name_inconsistent = 2
        shlp_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from DDIF_SHLP_PUT' ).
    ENDIF.

    zcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
  ENDMETHOD.

ENDCLASS.                    "zcl_abapgit_object_shlp IMPLEMENTATION
