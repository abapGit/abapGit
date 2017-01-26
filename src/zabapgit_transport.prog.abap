*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_TRANSPORT
*&---------------------------------------------------------------------*

CLASS lcl_transport DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      zip RAISING lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS:
      popup
        RETURNING VALUE(rt_trkorr) TYPE trwbo_request_headers,
      read_requests
        IMPORTING it_trkorr          TYPE trwbo_request_headers
        RETURNING VALUE(rt_requests) TYPE trwbo_requests
        RAISING   lcx_exception,
      find_top_package
        IMPORTING it_tadir          TYPE scts_tadir
        RETURNING VALUE(rv_package) TYPE devclass,
      resolve
        IMPORTING it_requests     TYPE trwbo_requests
        RETURNING VALUE(rt_tadir) TYPE scts_tadir
        RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_transport IMPLEMENTATION.

  METHOD zip.

    DATA: lt_requests   TYPE trwbo_requests,
          lt_tadir      TYPE scts_tadir,
          lv_package    TYPE devclass,
          ls_data       TYPE lcl_persistence_repo=>ty_repo,
          lo_repo       TYPE REF TO lcl_repo_offline,
          lt_trkorr TYPE trwbo_request_headers.

    lt_trkorr = popup( ).
    IF lines( lt_trkorr ) = 0.
      RETURN.
    ENDIF.

    lt_requests = read_requests( lt_trkorr ).
    lt_tadir = resolve( lt_requests ).
    IF lines( lt_tadir ) = 0.
      lcx_exception=>raise( 'empty transport' ).
    ENDIF.

    lv_package = find_top_package( lt_tadir ).
    IF lv_package IS INITIAL.
      lcx_exception=>raise( 'error finding super package' ).
    ENDIF.

    ls_data-key             = 'TZIP'.
    ls_data-package         = lv_package.
    ls_data-master_language = sy-langu.

    CREATE OBJECT lo_repo
      EXPORTING
        is_data = ls_data.

    lcl_zip=>export( io_repo = lo_repo
                     it_filter = lt_tadir ).

  ENDMETHOD.

  METHOD find_top_package.
* assumption: all objects in transport share a common super package

    DATA: lt_obj   TYPE lcl_sap_package=>ty_devclass_tt,
          lt_super TYPE lcl_sap_package=>ty_devclass_tt,
          lv_super LIKE LINE OF lt_super,
          lv_index TYPE i.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF it_tadir.


    READ TABLE it_tadir INDEX 1 ASSIGNING <ls_tadir>.
    ASSERT sy-subrc = 0.
    lt_super = lcl_sap_package=>list_superpackages( <ls_tadir>-devclass ).

    LOOP AT it_tadir ASSIGNING <ls_tadir>.
      lt_obj = lcl_sap_package=>list_superpackages( <ls_tadir>-devclass ).

* filter out possibilities from lt_super
      LOOP AT lt_super INTO lv_super.
        lv_index = sy-tabix.
        READ TABLE lt_obj FROM lv_super TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          DELETE lt_super INDEX lv_index.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    SORT lt_super.
    READ TABLE lt_super INDEX 1 INTO rv_package.

  ENDMETHOD.

  METHOD popup.
    DATA: lrs_trfunction TYPE trsel_trs_function,
          lv_types       TYPE string,
          ls_ranges      TYPE trsel_ts_ranges.

    " Fill all request types
    lv_types = 'KWTCOEMPDRSXQFG'.
    lrs_trfunction-sign   = 'I'.
    lrs_trfunction-option = 'EQ'.
    WHILE lv_types NE space.
      lrs_trfunction-low = lv_types(1).
      APPEND lrs_trfunction TO ls_ranges-request_funcs.
      SHIFT lv_types.
    ENDWHILE.

    CALL FUNCTION 'TRINT_SELECT_REQUESTS'
      EXPORTING
        iv_username_pattern    = sy-uname
        iv_via_selscreen       = 'X'
        iv_complete_projects   = ''
*       is_popup               =
        iv_title               = 'abapGit: Transport Request Selection'
      IMPORTING
        et_requests            = rt_trkorr
      CHANGING
        cs_ranges              = ls_ranges
      EXCEPTIONS
        action_aborted_by_user = 1
        OTHERS                 = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD read_requests.

    DATA lt_requests LIKE rt_requests.
    FIELD-SYMBOLS <fs_trkorr> LIKE LINE OF it_trkorr.

    LOOP AT it_trkorr ASSIGNING <fs_trkorr>.
      CALL FUNCTION 'TR_READ_REQUEST_WITH_TASKS'
        EXPORTING
          iv_trkorr     = <fs_trkorr>-trkorr
        IMPORTING
          et_requests   = lt_requests
        EXCEPTIONS
          invalid_input = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        lcx_exception=>raise( 'error from TR_READ_REQUEST_WITH_TASKS' ).
      ENDIF.

      APPEND LINES OF lt_requests TO rt_requests.
    ENDLOOP.

  ENDMETHOD.

  METHOD resolve.

    DATA: lv_object     TYPE tadir-object,
          lv_obj_name   TYPE tadir-obj_name,
          lv_trobj_name TYPE trobj_name,
          ls_tadir      TYPE tadir.

    FIELD-SYMBOLS: <ls_request> LIKE LINE OF it_requests,
                   <ls_object>  LIKE LINE OF <ls_request>-objects.


    LOOP AT it_requests ASSIGNING <ls_request>.
      LOOP AT <ls_request>-objects ASSIGNING <ls_object>.
        IF <ls_object>-pgmid = 'LIMU'.
          CALL FUNCTION 'GET_R3TR_OBJECT_FROM_LIMU_OBJ'
            EXPORTING
              p_limu_objtype = <ls_object>-object
              p_limu_objname = <ls_object>-obj_name
            IMPORTING
              p_r3tr_objtype = lv_object
              p_r3tr_objname = lv_trobj_name
            EXCEPTIONS
              no_mapping     = 1
              OTHERS         = 2.
          IF sy-subrc <> 0.
            lcx_exception=>raise( 'error from GET_R3TR_OBJECT_FROM_LIMU_OBJ' ).
          ENDIF.
          lv_obj_name = lv_trobj_name.
        ELSE.
          lv_object   = <ls_object>-object.
          lv_obj_name = <ls_object>-obj_name.
        ENDIF.

        ls_tadir = lcl_tadir=>read_single(
          iv_object   = lv_object
          iv_obj_name = lv_obj_name ).

        APPEND ls_tadir TO rt_tadir.
      ENDLOOP.
    ENDLOOP.

    SORT rt_tadir BY object ASCENDING obj_name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM rt_tadir COMPARING object obj_name.
    DELETE rt_tadir WHERE table_line IS INITIAL.
  ENDMETHOD.

ENDCLASS.
