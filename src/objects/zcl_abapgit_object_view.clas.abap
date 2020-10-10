CLASS zcl_abapgit_object_view DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
    ALIASES mo_files FOR zif_abapgit_object~mo_files.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: ty_dd26v TYPE STANDARD TABLE OF dd26v
                          WITH NON-UNIQUE DEFAULT KEY,
           ty_dd27p TYPE STANDARD TABLE OF dd27p
                          WITH NON-UNIQUE DEFAULT KEY,
           ty_dd28j TYPE STANDARD TABLE OF dd28j
                          WITH NON-UNIQUE DEFAULT KEY,
           ty_dd28v TYPE STANDARD TABLE OF dd28v
                          WITH NON-UNIQUE DEFAULT KEY.
    CONSTANTS: BEGIN OF co_viewclass,
                 help         TYPE viewclass VALUE 'H',
                 database     TYPE viewclass VALUE 'D',
                 projection   TYPE viewclass VALUE 'P',
                 structure    TYPE viewclass VALUE 'S',
                 maintenance  TYPE viewclass VALUE 'C',
                 entity       TYPE viewclass VALUE 'E',
                 view_variant TYPE viewclass VALUE 'V',
                 append       TYPE viewclass VALUE 'A',
                 external     TYPE viewclass VALUE 'X',
                 replication  TYPE viewclass VALUE 'R',
               END OF co_viewclass.

    METHODS:
      read_view
        EXPORTING
          es_dd25v TYPE dd25v
          es_dd09l TYPE dd09l
          et_dd26v TYPE ty_dd26v
          et_dd27p TYPE ty_dd27p
          et_dd28j TYPE ty_dd28j
          et_dd28v TYPE ty_dd28v
        RAISING
          zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_VIEW IMPLEMENTATION.


  METHOD read_view.

    DATA: lv_name TYPE ddobjname.

    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_VIEW_GET'
      EXPORTING
        name          = lv_name
        state         = 'A'
        langu         = mv_language
      IMPORTING
        dd25v_wa      = es_dd25v
        dd09l_wa      = es_dd09l
      TABLES
        dd26v_tab     = et_dd26v
        dd27p_tab     = et_dd27p
        dd28j_tab     = et_dd28j
        dd28v_tab     = et_dd28v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from DDIF_VIEW_GET' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    SELECT SINGLE as4user FROM dd25l INTO rv_user
      WHERE viewname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    delete_ddic( 'V' ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lv_name  TYPE ddobjname,
          ls_dd25v TYPE dd25v,
          ls_dd09l TYPE dd09l,
          lt_dd26v TYPE TABLE OF dd26v,
          lt_dd27p TYPE TABLE OF dd27p,
          lt_dd28j TYPE TABLE OF dd28j,
          lt_dd28v TYPE TABLE OF dd28v.

    FIELD-SYMBOLS: <ls_dd27p> LIKE LINE OF lt_dd27p.

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

    lv_name = ms_item-obj_name. " type conversion

    LOOP AT lt_dd27p ASSIGNING <ls_dd27p>.
      <ls_dd27p>-objpos = sy-tabix.
      <ls_dd27p>-viewname = lv_name.
* rollname seems to be mandatory in the API, but is typically not defined in the VIEW
      SELECT SINGLE rollname FROM dd03l INTO <ls_dd27p>-rollname
        WHERE tabname = <ls_dd27p>-tabname
        AND fieldname = <ls_dd27p>-fieldname.
      IF <ls_dd27p>-rollnamevi IS INITIAL.
        <ls_dd27p>-rollnamevi = <ls_dd27p>-rollname.
      ENDIF.
    ENDLOOP.

    corr_insert( iv_package = iv_package
                 ig_object_class = 'DICT' ).

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
      WHERE viewname = ms_item-obj_name.
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


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-ddic TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
    rs_metadata-ddic = abap_true.
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    DATA: ls_dd25v TYPE dd25v.

    read_view( IMPORTING es_dd25v = ls_dd25v ).

    CASE ls_dd25v-viewclass.
      WHEN co_viewclass-view_variant.

        CALL FUNCTION 'RS_TOOL_ACCESS'
          EXPORTING
            operation           = 'SHOW'
            object_name         = ms_item-obj_name
            object_type         = ms_item-obj_type
            in_new_window       = abap_true
          EXCEPTIONS
            not_executed        = 1
            invalid_object_type = 2
            OTHERS              = 3.

        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( |Error from RS_TOOL_ACCESS. Subrc={ sy-subrc }| ).
        ENDIF.

      WHEN OTHERS.

        jump_se11( iv_radio = 'RSRD1-VIMA'
                   iv_field = 'RSRD1-VIMA_VAL' ).

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: ls_dd25v TYPE dd25v,
          ls_dd09l TYPE dd09l,
          lt_dd26v TYPE ty_dd26v,
          lt_dd27p TYPE ty_dd27p,
          lt_dd28j TYPE ty_dd28j,
          lt_dd28v TYPE ty_dd28v.

    FIELD-SYMBOLS: <ls_dd27p> LIKE LINE OF lt_dd27p.

    read_view(
      IMPORTING
        es_dd25v = ls_dd25v
        es_dd09l = ls_dd09l
        et_dd26v = lt_dd26v
        et_dd27p = lt_dd27p
        et_dd28j = lt_dd28j
        et_dd28v = lt_dd28v ).

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
      IF <ls_dd27p>-rollchange = abap_false.
        CLEAR <ls_dd27p>-rollnamevi.
      ENDIF.
      CLEAR <ls_dd27p>-ddlanguage.
      CLEAR <ls_dd27p>-rollname.
      CLEAR <ls_dd27p>-viewname.
      CLEAR <ls_dd27p>-objpos.
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
ENDCLASS.
