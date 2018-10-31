CLASS zcl_abapgit_object_view DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

ENDCLASS.



CLASS zcl_abapgit_object_view IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE as4user FROM dd25l INTO rv_user
      WHERE viewname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE zcl_abapgit_comparison_null.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lv_objname TYPE rsedd0-ddobjname.


    lv_objname = ms_item-obj_name.

    CALL FUNCTION 'RS_DD_DELETE_OBJ'
      EXPORTING
        no_ask               = abap_true
        objname              = lv_objname
        objtype              = 'V'
      EXCEPTIONS
        not_executed         = 1
        object_not_found     = 2
        object_not_specified = 3
        permission_failure   = 4.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from RS_DD_DELETE_OBJ, VIEW' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lv_name  TYPE ddobjname,
          ls_dd25v TYPE dd25v,
          ls_dd09l TYPE dd09l,
          lt_dd26v TYPE TABLE OF dd26v,
          lt_dd27p TYPE TABLE OF dd27p,
          lt_dd28j TYPE TABLE OF dd28j,
          lt_dd28v TYPE TABLE OF dd28v.


    io_xml->read( EXPORTING iv_name = 'DD25V'
                  CHANGING cg_data = ls_dd25v ).
    io_xml->read( EXPORTING iv_name = 'DD09L'
                  CHANGING cg_data = ls_dd09l ).
    io_xml->read( EXPORTING iv_name = 'DD26V_TABLE'
                  CHANGING cg_data = lt_dd26v ).
    io_xml->read( EXPORTING iv_name = 'DD27P_TABLE'
                  CHANGING cg_data = lt_dd27p ).
    io_xml->read( EXPORTING iv_name = 'DD28J_TABLE'
                  CHANGING cg_data = lt_dd28j ).
    io_xml->read( EXPORTING iv_name = 'DD28V_TABLE'
                  CHANGING cg_data = lt_dd28v ).

    corr_insert( iv_package ).

    lv_name = ms_item-obj_name. " type conversion

    CALL FUNCTION 'DDIF_VIEW_PUT'
      EXPORTING
        name              = lv_name
        dd25v_wa          = ls_dd25v
        dd09l_wa          = ls_dd09l
      TABLES
        dd26v_tab         = lt_dd26v
        dd27p_tab         = lt_dd27p
        dd28j_tab         = lt_dd28j
        dd28v_tab         = lt_dd28v
      EXCEPTIONS
        view_not_found    = 1
        name_inconsistent = 2
        view_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from DDIF_VIEW_PUT' ).
    ENDIF.

    zcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_viewname TYPE dd25l-viewname,
          lv_ddl_view TYPE abap_bool.


    SELECT SINGLE viewname FROM dd25l INTO lv_viewname
      WHERE viewname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    rv_bool = boolc( sy-subrc = 0 ).

    IF rv_bool = abap_true.
      TRY.
          CALL METHOD ('CL_DD_DDL_UTILITIES')=>('CHECK_FOR_DDL_VIEW')
            EXPORTING
              objname     = lv_viewname
            RECEIVING
              is_ddl_view = lv_ddl_view.

          IF lv_ddl_view = abap_true.
            rv_bool = abap_false.
          ENDIF.
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-ddic = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~has_changed_since.

    DATA: lv_date TYPE dats,
          lv_time TYPE tims.

    SELECT SINGLE as4date as4time FROM dd25l
      INTO (lv_date, lv_time)
      WHERE viewname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers  = '0000'.

    rv_changed = check_timestamp(
      iv_timestamp = iv_timestamp
      iv_date      = lv_date
      iv_time      = lv_time ).
    IF rv_changed = abap_true.
      RETURN.
    ENDIF.

    SELECT SINGLE as4date as4time FROM dd09l
      INTO (lv_date, lv_time)
      WHERE tabname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers  = '0000'.

    rv_changed = check_timestamp(
      iv_timestamp = iv_timestamp
      iv_date      = lv_date
      iv_time      = lv_time ).

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    jump_se11( iv_radio = 'RSRD1-VIMA'
               iv_field = 'RSRD1-VIMA_VAL' ).

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lv_name  TYPE ddobjname,
          ls_dd25v TYPE dd25v,
          ls_dd09l TYPE dd09l,
          lt_dd26v TYPE TABLE OF dd26v,
          lt_dd27p TYPE TABLE OF dd27p,
          lt_dd28j TYPE TABLE OF dd28j,
          lt_dd28v TYPE TABLE OF dd28v.

    FIELD-SYMBOLS: <ls_dd27p> LIKE LINE OF lt_dd27p.


    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_VIEW_GET'
      EXPORTING
        name          = lv_name
        state         = 'A'
        langu         = mv_language
      IMPORTING
        dd25v_wa      = ls_dd25v
        dd09l_wa      = ls_dd09l
      TABLES
        dd26v_tab     = lt_dd26v
        dd27p_tab     = lt_dd27p
        dd28j_tab     = lt_dd28j
        dd28v_tab     = lt_dd28v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from DDIF_VIEW_GET' ).
    ENDIF.
    IF ls_dd25v IS INITIAL.
      RETURN. " does not exist in system
    ENDIF.

    CLEAR: ls_dd25v-as4user,
           ls_dd25v-as4date,
           ls_dd25v-as4time.

    CLEAR: ls_dd09l-as4user,
           ls_dd09l-as4date,
           ls_dd09l-as4time.

    LOOP AT lt_dd27p ASSIGNING <ls_dd27p>.
      CLEAR: <ls_dd27p>-ddtext,
             <ls_dd27p>-reptext,
             <ls_dd27p>-scrtext_s,
             <ls_dd27p>-scrtext_m,
             <ls_dd27p>-scrtext_l,
             <ls_dd27p>-outputlen,
             <ls_dd27p>-decimals,
             <ls_dd27p>-lowercase,
             <ls_dd27p>-convexit,
             <ls_dd27p>-signflag,
             <ls_dd27p>-flength,
             <ls_dd27p>-domname,
             <ls_dd27p>-datatype,
             <ls_dd27p>-entitytab,
             <ls_dd27p>-inttype,
             <ls_dd27p>-intlen,
             <ls_dd27p>-headlen,
             <ls_dd27p>-scrlen1,
             <ls_dd27p>-scrlen2,
             <ls_dd27p>-scrlen3,
             <ls_dd27p>-memoryid.
    ENDLOOP.

    io_xml->add( iv_name = 'DD25V'
                 ig_data = ls_dd25v ).
    io_xml->add( iv_name = 'DD09L'
                 ig_data = ls_dd09l ).
    io_xml->add( ig_data = lt_dd26v
                 iv_name = 'DD26V_TABLE' ).
    io_xml->add( ig_data = lt_dd27p
                 iv_name = 'DD27P_TABLE' ).
    io_xml->add( ig_data = lt_dd28j
                 iv_name = 'DD28J_TABLE' ).
    io_xml->add( ig_data = lt_dd28v
                 iv_name = 'DD28V_TABLE' ).

  ENDMETHOD.

  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
ENDCLASS.
