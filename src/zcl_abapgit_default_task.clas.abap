CLASS zcl_abapgit_default_task DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance
        RETURNING
          VALUE(ro_instance) TYPE REF TO zcl_abapgit_default_task
        RAISING
          zcx_abapgit_exception.

    METHODS:
      constructor
        RAISING
          zcx_abapgit_exception,

      set
        IMPORTING
          iv_package TYPE devclass
        RAISING
          zcx_abapgit_exception,

      reset
        RAISING
          zcx_abapgit_exception.

  PRIVATE SECTION.

    CLASS-DATA go_instance TYPE REF TO zcl_abapgit_default_task .
    DATA mv_task_is_set_by_abapgit TYPE abap_bool .
    DATA ms_save_default_task TYPE e070use .

    METHODS store_current_default_task
      RAISING
        zcx_abapgit_exception .
    METHODS restore_saved_default_task
      RAISING
        zcx_abapgit_exception .
    METHODS get_default_task
      RETURNING
        VALUE(rs_default_task) TYPE e070use
      RAISING
        zcx_abapgit_exception .
    METHODS set_default_task
      IMPORTING
        !iv_order TYPE trkorr
        !iv_task  TYPE trkorr
      RAISING
        zcx_abapgit_exception .
    METHODS call_transport_order_popup
      EXPORTING
        !ev_order TYPE trkorr
        !ev_task  TYPE trkorr
      RAISING
        zcx_abapgit_exception .
    METHODS are_objects_recorded_in_tr_req
      IMPORTING
        !iv_package                    TYPE devclass
      RETURNING
        VALUE(rv_are_objects_recorded) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS clear_default_task
      IMPORTING
        !is_default_task TYPE e070use
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_DEFAULT_TASK IMPLEMENTATION.


  METHOD are_objects_recorded_in_tr_req.

    DATA: li_package TYPE REF TO if_package.

    cl_package_factory=>load_package(
      EXPORTING
        i_package_name             = iv_package
      IMPORTING
        e_package                  = li_package
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        no_access                  = 4
        object_locked_and_modified = 5
        OTHERS                     = 6 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from CL_PACKAGE_FACTORY=>LOAD_PACKAGE { sy-subrc }| ).
    ENDIF.

    rv_are_objects_recorded = li_package->wbo_korr_flag.

  ENDMETHOD.


  METHOD call_transport_order_popup.

    DATA: lt_e071  TYPE STANDARD TABLE OF e071,
          lt_e071k TYPE STANDARD TABLE OF e071k.

    CLEAR: ev_order,
           ev_task.

    CALL FUNCTION 'TRINT_ORDER_CHOICE'
      IMPORTING
        we_order               = ev_order
        we_task                = ev_task
      TABLES
        wt_e071                = lt_e071
        wt_e071k               = lt_e071k
      EXCEPTIONS
        no_correction_selected = 1
        display_mode           = 2
        object_append_error    = 3
        recursive_call         = 4
        wrong_order_type       = 5
        OTHERS                 = 6.

    IF sy-subrc = 1.
      zcx_abapgit_exception=>raise( 'cancelled' ).
    ELSEIF sy-subrc > 1.
      zcx_abapgit_exception=>raise( |Error from TRINT_ORDER_CHOICE { sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD clear_default_task.

    CALL FUNCTION 'TR_TASK_RESET'
      EXPORTING
        iv_username      = is_default_task-username
        iv_order         = is_default_task-ordernum
        iv_task          = is_default_task-tasknum
        iv_dialog        = abap_false
      EXCEPTIONS
        invalid_username = 1
        invalid_order    = 2
        invalid_task     = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from TR_TASK_RESET { sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    store_current_default_task( ).

  ENDMETHOD.


  METHOD get_default_task.

    DATA: lt_e070use TYPE STANDARD TABLE OF e070use.

    CALL FUNCTION 'TR_TASK_GET'
      TABLES
        tt_e070use       = lt_e070use
      EXCEPTIONS
        invalid_username = 1
        invalid_category = 2
        invalid_client   = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from TR_TASK_GET { sy-subrc }| ).
    ENDIF.

    READ TABLE lt_e070use INTO rs_default_task
                          INDEX 1.

  ENDMETHOD.


  METHOD get_instance.

    IF go_instance IS NOT BOUND.
      CREATE OBJECT go_instance.
    ENDIF.

    ro_instance = go_instance.

  ENDMETHOD.


  METHOD reset.

    DATA: ls_default_task TYPE e070use.

    IF mv_task_is_set_by_abapgit = abap_false.
      " if the default transport request task isn't set
      " by us there is nothing to do.
      RETURN.
    ENDIF.

    CLEAR mv_task_is_set_by_abapgit.

    ls_default_task = get_default_task( ).

    IF ls_default_task IS NOT INITIAL.

      clear_default_task( ls_default_task ).

    ENDIF.

    restore_saved_default_task( ).

  ENDMETHOD.


  METHOD restore_saved_default_task.

    IF ms_save_default_task IS INITIAL.
      " There wasn't a default transport request before
      " so we needn't restore anything.
      RETURN.
    ENDIF.

    CALL FUNCTION 'TR_TASK_SET'
      EXPORTING
        iv_order          = ms_save_default_task-ordernum
        iv_task           = ms_save_default_task-tasknum
      EXCEPTIONS
        invalid_username  = 1
        invalid_category  = 2
        invalid_client    = 3
        invalid_validdays = 4
        invalid_order     = 5
        invalid_task      = 6
        OTHERS            = 7.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from TR_TASK_SET { sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD set.

    " checks whether object changes of the package are rerorded in transport
    " requests. If true then we set the default task, so that no annoying
    " transport request popups are shown while deserializing.

    DATA: lv_order TYPE trkorr,
          lv_task  TYPE trkorr.

    IF mv_task_is_set_by_abapgit = abap_true.
      " the default transport request task is already set by us
      " -> no reason to do it again.
      RETURN.
    ENDIF.

    IF are_objects_recorded_in_tr_req( iv_package ) = abap_false.
      " Objects of package are not recorded in transport request,
      " no need to proceed.
      RETURN.
    ENDIF.

    call_transport_order_popup(
      IMPORTING
        ev_order = lv_order
        ev_task  = lv_task ).

    set_default_task( iv_order = lv_order
                      iv_task  = lv_task ).

    mv_task_is_set_by_abapgit = abap_true.

  ENDMETHOD.


  METHOD set_default_task.

    CALL FUNCTION 'TR_TASK_SET'
      EXPORTING
        iv_order          = iv_order
        iv_task           = iv_task
      EXCEPTIONS
        invalid_username  = 1
        invalid_category  = 2
        invalid_client    = 3
        invalid_validdays = 4
        invalid_order     = 5
        invalid_task      = 6
        OTHERS            = 7.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from TR_TASK_SET { sy-subrc }| ).
    ENDIF.

  ENDMETHOD.


  METHOD store_current_default_task.

    ms_save_default_task = get_default_task( ).

  ENDMETHOD.
ENDCLASS.
