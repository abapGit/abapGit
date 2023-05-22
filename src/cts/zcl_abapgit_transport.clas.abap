CLASS zcl_abapgit_transport DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
* todo, add interfaces for this class, consider merging zcl_abapgit_transport_mass into this class?
    CLASS-METHODS zip
      IMPORTING
        !iv_show_log_popup TYPE abap_bool DEFAULT abap_true
        !iv_logic          TYPE string OPTIONAL
        !is_trkorr         TYPE trwbo_request_header OPTIONAL
      RETURNING
        VALUE(rv_xstr)     TYPE xstring
      RAISING
        zcx_abapgit_exception .

    CLASS-METHODS to_tadir
      IMPORTING
        !it_transport_headers TYPE trwbo_request_headers
        !iv_deleted_objects   TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_tadir)       TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .

    CLASS-METHODS add_all_objects_to_trans_req
      IMPORTING
        iv_key TYPE zif_abapgit_persistence=>ty_value
      RAISING
        zcx_abapgit_exception .

    CLASS-METHODS read
      IMPORTING
        !is_trkorr        TYPE trwbo_request_header OPTIONAL
      RETURNING
        VALUE(rs_request) TYPE trwbo_request
      RAISING
        zcx_abapgit_exception .

    CLASS-METHODS validate_transport_request
      IMPORTING
        iv_transport_request TYPE trkorr
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.

    CLASS-METHODS read_requests
      IMPORTING
        !it_trkorr         TYPE trwbo_request_headers
      RETURNING
        VALUE(rt_requests) TYPE trwbo_requests
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS find_top_package
      IMPORTING
        !it_tadir         TYPE zif_abapgit_definitions=>ty_tadir_tt
      RETURNING
        VALUE(rv_package) TYPE devclass
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS resolve
      IMPORTING
        !it_requests        TYPE trwbo_requests
        !iv_deleted_objects TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_tadir)     TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.
    CLASS-METHODS collect_all_objects
      IMPORTING
        iv_key            TYPE zif_abapgit_persistence=>ty_value
      RETURNING
        VALUE(rt_objects) TYPE tr_objects
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS show_log
      IMPORTING
        it_log   TYPE sprot_u_tab
        iv_title TYPE string.
ENDCLASS.



CLASS zcl_abapgit_transport IMPLEMENTATION.


  METHOD add_all_objects_to_trans_req.

    DATA:
      ls_request      TYPE trwbo_request_header,
      lt_e071         TYPE tr_objects,
      lv_text         TYPE string,
      lv_answer       TYPE c LENGTH 1,
      lv_lock_objects TYPE trparflag,
      lt_log          TYPE sprot_u_tab.

    lv_answer = zcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
                    iv_titlebar              = `Lock objects?`
                    iv_text_question         = `Shall all objects be locked in the transport request?`
                    iv_display_cancel_button = abap_true ).

    CASE lv_answer.
      WHEN '1'.
        lv_lock_objects = abap_true.
      WHEN '2'.
        lv_lock_objects = abap_false.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    lt_e071 = collect_all_objects( iv_key ).

    " We used TR_REQUEST_CHOICE before, but it issues its error log with
    " write lists which are not compatible with abapGit.
    " There we user TRINT_REQUEST_CHOICE which returns the error log
    " and display the log ourselve.
    CALL FUNCTION 'TRINT_REQUEST_CHOICE'
      EXPORTING
        iv_request_types     = 'FTCOK'
        iv_lock_objects      = lv_lock_objects
        iv_with_error_log    = abap_false
      IMPORTING
        es_request           = ls_request
        et_log               = lt_log
      TABLES
        it_e071              = lt_e071
      EXCEPTIONS
        invalid_request      = 1
        invalid_request_type = 2
        user_not_owner       = 3
        no_objects_appended  = 4
        enqueue_error        = 5
        cancelled_by_user    = 6
        recursive_call       = 7
        OTHERS               = 8.
    IF sy-subrc = 0.
      lv_text = |Objects successfully added to { ls_request-trkorr }|.
      MESSAGE lv_text TYPE 'S'.
      RETURN.
    ENDIF.

    IF lines( lt_log ) > 0.
      show_log(
          it_log   = lt_log
          iv_title = `Error log` ).
    ELSE.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD collect_all_objects.

    DATA:
      lt_objects     TYPE scts_tadir,
      lt_objects_all LIKE lt_objects,
      ls_e071        LIKE LINE OF rt_objects,
      lo_repo        TYPE REF TO zcl_abapgit_repo,
      lv_package     TYPE zif_abapgit_persistence=>ty_repo-package,
      lt_packages    TYPE zif_abapgit_sap_package=>ty_devclass_tt.

    FIELD-SYMBOLS:
      <lv_package> TYPE devclass,
      <ls_object>  TYPE tadir.

    lo_repo    ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
    lv_package  = lo_repo->get_package( ).
    lt_packages = zcl_abapgit_factory=>get_sap_package( lv_package )->list_subpackages( ).
    INSERT lv_package INTO TABLE lt_packages.

    LOOP AT lt_packages ASSIGNING <lv_package>.

      CLEAR: lt_objects.

      CALL FUNCTION 'TRINT_SELECT_OBJECTS'
        EXPORTING
          iv_devclass       = <lv_package>
          iv_via_selscreen  = abap_false
        IMPORTING
          et_objects_tadir  = lt_objects
        EXCEPTIONS
          cancelled_by_user = 1
          invalid_input     = 2
          OTHERS            = 3.

      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      INSERT LINES OF lt_objects INTO TABLE lt_objects_all.

    ENDLOOP.

    IF lines( lt_objects_all ) = 0.
      zcx_abapgit_exception=>raise( |No objects found| ).
    ENDIF.

    LOOP AT lt_objects_all ASSIGNING <ls_object>.

      CLEAR: ls_e071.

      MOVE-CORRESPONDING <ls_object> TO ls_e071.
      INSERT ls_e071 INTO TABLE rt_objects.

    ENDLOOP.

  ENDMETHOD.


  METHOD find_top_package.
* assumption: all objects in transport share a common super package

    DATA: lt_obj   TYPE zif_abapgit_sap_package=>ty_devclass_tt,
          lt_super TYPE zif_abapgit_sap_package=>ty_devclass_tt,
          lv_super LIKE LINE OF lt_super,
          lv_index TYPE i.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF it_tadir.


    READ TABLE it_tadir INDEX 1 ASSIGNING <ls_tadir>.
    ASSERT sy-subrc = 0.
    lt_super = zcl_abapgit_factory=>get_sap_package( <ls_tadir>-devclass )->list_superpackages( ).

    LOOP AT it_tadir ASSIGNING <ls_tadir>.
      lt_obj = zcl_abapgit_factory=>get_sap_package( <ls_tadir>-devclass )->list_superpackages( ).

* filter out possibilities from lt_super
      LOOP AT lt_super INTO lv_super.
        lv_index = sy-tabix.
        READ TABLE lt_obj FROM lv_super TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          DELETE lt_super INDEX lv_index.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    READ TABLE lt_super INDEX lines( lt_super ) INTO rv_package.
  ENDMETHOD.


  METHOD read.

    rs_request-h-trkorr = is_trkorr-trkorr.

    CALL FUNCTION 'TRINT_READ_REQUEST'
      EXPORTING
        iv_read_e070       = abap_true
        iv_read_e07t       = abap_true
        iv_read_e070c      = abap_true
        iv_read_e070m      = abap_true
        iv_read_objs_keys  = abap_true
        iv_read_objs       = abap_true
        iv_read_attributes = abap_true
      CHANGING
        cs_request         = rs_request
      EXCEPTIONS
        error_occured      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD read_requests.
    DATA lt_requests LIKE rt_requests.
    FIELD-SYMBOLS <ls_trkorr> LIKE LINE OF it_trkorr.

    LOOP AT it_trkorr ASSIGNING <ls_trkorr>.
      CALL FUNCTION 'TR_READ_REQUEST_WITH_TASKS'
        EXPORTING
          iv_trkorr     = <ls_trkorr>-trkorr
        IMPORTING
          et_requests   = lt_requests
        EXCEPTIONS
          invalid_input = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      APPEND LINES OF lt_requests TO rt_requests.
    ENDLOOP.
  ENDMETHOD.


  METHOD resolve.
    DATA: lv_object    TYPE tadir-object,
          lv_obj_name  TYPE tadir-obj_name,
          ls_tadir     TYPE zif_abapgit_definitions=>ty_tadir,
          lv_result    TYPE trpari-s_checked,
          ls_tadir_sap TYPE tadir.

    FIELD-SYMBOLS: <ls_request> LIKE LINE OF it_requests,
                   <ls_object>  LIKE LINE OF <ls_request>-objects.


    LOOP AT it_requests ASSIGNING <ls_request>.
      LOOP AT <ls_request>-objects ASSIGNING <ls_object>.
        " VARX, see https://github.com/abapGit/abapGit/issues/3107
        IF <ls_object>-pgmid = 'LIMU' AND <ls_object>-object <> 'VARX'.
          CALL FUNCTION 'TR_CHECK_TYPE'
            EXPORTING
              wi_e071   = <ls_object>
            IMPORTING
              we_tadir  = ls_tadir_sap
              pe_result = lv_result.
          IF lv_result NA 'TL' OR ls_tadir_sap IS INITIAL.
            zcx_abapgit_exception=>raise( 'error from TR_CHECK_TYPE' ).
          ENDIF.
          lv_object   = ls_tadir_sap-object.
          lv_obj_name = ls_tadir_sap-obj_name.
        ELSE.
          lv_object   = <ls_object>-object.
          lv_obj_name = <ls_object>-obj_name.
        ENDIF.

        ls_tadir = zcl_abapgit_factory=>get_tadir( )->read_single(
          iv_object   = lv_object
          iv_obj_name = lv_obj_name ).

        IF ls_tadir-delflag IS INITIAL OR iv_deleted_objects = abap_true.
          APPEND ls_tadir TO rt_tadir.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    SORT rt_tadir BY object ASCENDING obj_name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM rt_tadir COMPARING object obj_name.
    DELETE rt_tadir WHERE table_line IS INITIAL.
  ENDMETHOD.


  METHOD show_log.

    DATA: li_log     TYPE REF TO zif_abapgit_log,
          lv_message TYPE string.
    FIELD-SYMBOLS: <ls_log> TYPE sprot_u.

    CREATE OBJECT li_log TYPE zcl_abapgit_log
      EXPORTING
        iv_title = iv_title.

    LOOP AT it_log ASSIGNING <ls_log>.

      MESSAGE ID <ls_log>-ag TYPE <ls_log>-severity NUMBER <ls_log>-msgnr
       WITH <ls_log>-var1 <ls_log>-var2 <ls_log>-var3 <ls_log>-var4
       INTO lv_message.

      li_log->add(
          iv_msg  = lv_message
          iv_type = <ls_log>-severity ).

    ENDLOOP.

    zcl_abapgit_log_viewer=>show_log( li_log ).

  ENDMETHOD.


  METHOD to_tadir.
    DATA: lt_requests TYPE trwbo_requests.


    IF lines( it_transport_headers ) = 0.
      RETURN.
    ENDIF.

    lt_requests = read_requests( it_transport_headers ).
    rt_tadir = resolve(
      it_requests        = lt_requests
      iv_deleted_objects = iv_deleted_objects ).

  ENDMETHOD.


  METHOD validate_transport_request.

    CONSTANTS:
      BEGIN OF c_tr_status,
        modifiable                   TYPE trstatus VALUE 'D',
        modifiable_protected         TYPE trstatus VALUE 'L',
        release_started              TYPE trstatus VALUE 'O',
        released                     TYPE trstatus VALUE 'R',
        released_with_import_protect TYPE trstatus VALUE 'N', " Released (with Import Protection for Repaired Objects)
      END OF c_tr_status.

    DATA:
      ls_trkorr  TYPE trwbo_request_header,
      ls_request TYPE trwbo_request.

    ls_trkorr-trkorr = iv_transport_request.

    ls_request = read( ls_trkorr ).

    IF  ls_request-h-trstatus <> c_tr_status-modifiable
    AND ls_request-h-trstatus <> c_tr_status-modifiable_protected.
      " Task/request &1 has already been released
      MESSAGE e064(tk) WITH iv_transport_request INTO zcx_abapgit_exception=>null.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zip.

    DATA: lt_requests       TYPE trwbo_requests,
          lt_tadir          TYPE zif_abapgit_definitions=>ty_tadir_tt,
          lv_package        TYPE devclass,
          lo_dot_abapgit    TYPE REF TO zcl_abapgit_dot_abapgit,
          ls_local_settings TYPE zif_abapgit_persistence=>ty_repo-local_settings,
          lt_trkorr         TYPE trwbo_request_headers.


    IF is_trkorr IS SUPPLIED.
      APPEND is_trkorr TO lt_trkorr.
    ELSE.
      lt_trkorr = zcl_abapgit_ui_factory=>get_popups( )->popup_to_select_transports( ).
    ENDIF.

    IF lines( lt_trkorr ) = 0.
      RETURN.
    ENDIF.

    lt_requests = read_requests( lt_trkorr ).
    lt_tadir = resolve( lt_requests ).
    IF lines( lt_tadir ) = 0.
      zcx_abapgit_exception=>raise( 'empty transport' ).
    ENDIF.

    lv_package = find_top_package( lt_tadir ).
    IF lv_package IS INITIAL.
      zcx_abapgit_exception=>raise( 'error finding super package' ).
    ENDIF.

    lo_dot_abapgit = zcl_abapgit_dot_abapgit=>build_default( ).
    IF iv_logic IS SUPPLIED AND iv_logic IS NOT INITIAL.
      lo_dot_abapgit->set_folder_logic( iv_logic ).
    ELSE.
      lo_dot_abapgit->set_folder_logic( zcl_abapgit_ui_factory=>get_popups( )->popup_folder_logic( ) ).
    ENDIF.

    rv_xstr = zcl_abapgit_zip=>export(
      iv_package        = lv_package
      io_dot_abapgit    = lo_dot_abapgit
      is_local_settings = ls_local_settings
      it_filter         = lt_tadir
      iv_show_log       = iv_show_log_popup ).

  ENDMETHOD.
ENDCLASS.
