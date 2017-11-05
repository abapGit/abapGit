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
    CLASS-DATA: mo_instance TYPE REF TO zcl_abapgit_default_task.

    DATA: mv_task_is_set_by_abapgit TYPE abap_bool,
          ms_save_default_task      TYPE e070use.

    METHODS restore_old_default_task
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_default_task IMPLEMENTATION.

  METHOD constructor.

    DATA: lt_e070use TYPE STANDARD TABLE OF e070use.

    " Save the current default task to restore it later...
    CALL FUNCTION 'TR_TASK_GET'
      TABLES
        tt_e070use       = lt_e070use     " Table of current settings
      EXCEPTIONS
        invalid_username = 1
        invalid_category = 2
        invalid_client   = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from TR_TASK_GET { sy-subrc }| ).
    ENDIF.

    IF lines( lt_e070use ) = 0.
      RETURN.
    ENDIF.

    READ TABLE lt_e070use INTO ms_save_default_task
                          INDEX 1.

  ENDMETHOD.


  METHOD get_instance.

    IF mo_instance IS NOT BOUND.

      CREATE OBJECT mo_instance.

    ENDIF.

    ro_instance = mo_instance.

  ENDMETHOD.


  METHOD reset.

    DATA: lt_e070use TYPE STANDARD TABLE OF e070use.

    IF mv_task_is_set_by_abapgit = abap_false.
      " if the default transport request task isn't set
      " by us there is nothing to do.
      RETURN.
    ENDIF.

    CALL FUNCTION 'TR_TASK_GET'
      TABLES
        tt_e070use       = lt_e070use    " Table of current settings
      EXCEPTIONS
        invalid_username = 1
        invalid_category = 2
        invalid_client   = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from TR_TASK_GET { sy-subrc }| ).
    ENDIF.

    READ TABLE lt_e070use ASSIGNING FIELD-SYMBOL(<ls_e070use>)
                          INDEX 1.
    ASSERT sy-subrc = 0.

    CALL FUNCTION 'TR_TASK_RESET'
      EXPORTING
        iv_username      = <ls_e070use>-username
        iv_order         = <ls_e070use>-ordernum
        iv_task          = <ls_e070use>-tasknum
        iv_dialog        = abap_false
      EXCEPTIONS
        invalid_username = 1
        invalid_order    = 2
        invalid_task     = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from TR_TASK_RESET { sy-subrc }| ).
    ENDIF.

    restore_old_default_task( ).

  ENDMETHOD.


  METHOD set.

    " checks whether object changes of the package are rerorded in transport
    " requests. If true then we set the default task, so that no annoying
    " transport request popups are shown while deserializing.

    DATA: li_package TYPE REF TO if_package,
          lt_e071    TYPE STANDARD TABLE OF e071,
          lt_e071k   TYPE STANDARD TABLE OF e071k,
          lv_order   TYPE trkorr,
          lv_task    TYPE trkorr.

    IF mv_task_is_set_by_abapgit = abap_true.
      " the default transport request task is already set by us
      " -> no reason to do it again.
      RETURN.
    ENDIF.

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

    IF li_package->wbo_korr_flag = abap_false.
      " Objects of package are not recorded in transport request,
      " no need to proceed.
      RETURN.
    ENDIF.

    CALL FUNCTION 'TRINT_ORDER_CHOICE'
      IMPORTING
        we_order               = lv_order     " Selected request
        we_task                = lv_task    " Selected task
      TABLES
        wt_e071                = lt_e071    " Object table to be edited (for mass editing)
        wt_e071k               = lt_e071k    " Key table to be edited (for mass editing)
      EXCEPTIONS
        no_correction_selected = 1
        display_mode           = 2
        object_append_error    = 3
        recursive_call         = 4
        wrong_order_type       = 5
        OTHERS                 = 6.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from TRINT_ORDER_CHOICE { sy-subrc }| ).
    ENDIF.

    CALL FUNCTION 'TR_TASK_SET'
      EXPORTING
        iv_order          = lv_order    " Request to be s et
        iv_task           = lv_task    " Task to be set
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

    mv_task_is_set_by_abapgit = abap_true.

  ENDMETHOD.

  METHOD restore_old_default_task.

    IF ms_save_default_task IS INITIAL.
      " There wasn't a default transport request before
      " so we needn't restore anything.
      RETURN.
    ENDIF.

    CALL FUNCTION 'TR_TASK_SET'
      EXPORTING
        iv_order          = ms_save_default_task-ordernum    " Request to be s et
        iv_task           = ms_save_default_task-tasknum    " Task to be set
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

ENDCLASS.
