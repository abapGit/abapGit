CLASS zcl_abapgit_gui_pre_filter DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_transports
      RETURNING VALUE(et_r_trkorr) TYPE zif_abapgit_repo_pre_filter=>ty_trrngtrkor_tt
      RAISING
                zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_gui_pre_filter IMPLEMENTATION.

  METHOD get_transports.
    DATA ls_selection  TYPE trwbo_selection.

    DATA ls_r_trkorr TYPE line of zif_abapgit_repo_pre_filter=>ty_trrngtrkor_tt.
    DATA lr_request TYPE REF TO trwbo_request_header.
    DATA lt_request TYPE trwbo_request_headers.

    ls_selection-trkorrpattern = space.
    ls_selection-connect_req_task_conditions = 'X'.
    ls_selection-reqfunctions = 'KTRXS'.
    ls_selection-reqstatus = 'RNODL'.
    ls_selection-taskstatus = 'RNODL'.
    CONDENSE ls_selection-reqfunctions NO-GAPS.
    ls_selection-taskfunctions = 'QRSX'.
    CONCATENATE sy-sysid '*' INTO ls_selection-trkorrpattern.

    CALL FUNCTION 'TRINT_SELECT_REQUESTS'
      EXPORTING
        iv_username_pattern    = '*'
        is_selection           = ls_selection
        iv_complete_projects   = abap_false
        iv_via_selscreen       = 'X'
        iv_title               = 'Select Transports / Tasks'
      IMPORTING
        et_requests            = lt_request
      EXCEPTIONS
        action_aborted_by_user = 1
        OTHERS                 = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Pre-Filter Canceled' ).
    ENDIF.

    IF lt_request IS INITIAL.
      zcx_abapgit_exception=>raise( 'No Request Found' ).
    ENDIF.

    LOOP AT lt_request REFERENCE INTO lr_request.
      ls_r_trkorr-sign = 'I'.
      ls_r_trkorr-option = 'EQ'.
      ls_r_trkorr-low = lr_request->trkorr.
      INSERT ls_r_trkorr INTO TABLE et_r_trkorr.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
