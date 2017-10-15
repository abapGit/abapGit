*&---------------------------------------------------------------------*
*& Include zabapgit_callbacks
*&---------------------------------------------------------------------*

INTERFACE lif_callback_listener.
  METHODS:
    on_after_pull IMPORTING iv_package     TYPE devclass
                            iv_old_version TYPE string
                            iv_new_version TYPE string.
ENDINTERFACE.

CLASS lcl_dummy_callback_listener DEFINITION.
  PUBLIC SECTION.
    INTERFACES:
      lif_callback_listener.
    ALIASES:
      on_after_pull FOR lif_callback_listener~on_after_pull.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_dummy_callback_listener IMPLEMENTATION.
  METHOD lif_callback_listener~on_after_pull.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_callback_adapter DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    INTERFACES:
      lif_callback_listener.
    ALIASES:
      on_after_pull FOR lif_callback_listener~on_after_pull.
    CONSTANTS:
      gc_methname_on_after_pull TYPE abap_methname VALUE 'ON_AFTER_PULL'.
    CLASS-METHODS:
      get_instance IMPORTING io_repo            TYPE REF TO lcl_repo
                             iv_force_new       TYPE abap_bool DEFAULT abap_false
                   RETURNING VALUE(ro_instance) TYPE REF TO lcl_callback_adapter
                   RAISING   zcx_abapgit_exception.
    METHODS:
      check_execution_allowed IMPORTING iv_methname       TYPE abap_methname
                              RETURNING VALUE(rv_allowed) TYPE abap_bool
                              RAISING   zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS:
      dyn_call_method IMPORTING io_object     TYPE REF TO object
                                iv_methname   TYPE abap_methname
                                it_parameters TYPE abap_parmbind_tab.
    METHODS:
      constructor IMPORTING io_repo TYPE REF TO lcl_repo
                  RAISING   zcx_abapgit_exception,
      init_listener RAISING cx_sy_create_object_error,
      is_dummy_listener RETURNING VALUE(rv_is_dummy) TYPE abap_bool.
    DATA:
      mo_repository         TYPE REF TO lcl_repo,
      mo_listener           TYPE REF TO object,
      mv_callback_classname TYPE abap_classname.
ENDCLASS.

CLASS lcl_callback_adapter IMPLEMENTATION.
  METHOD get_instance.
    TYPES: BEGIN OF lty_cache,
             key      TYPE lcl_persistence_db=>ty_value,
             instance TYPE REF TO lcl_callback_adapter,
           END OF lty_cache.
    STATICS: st_cache TYPE SORTED TABLE OF lty_cache WITH UNIQUE KEY key.
    DATA: lr_cache TYPE REF TO lty_cache.

    ASSERT io_repo IS BOUND AND io_repo->get_key( ) IS NOT INITIAL.

    IF iv_force_new = abap_false.
      READ TABLE st_cache WITH TABLE KEY key = io_repo->get_key( ) REFERENCE INTO lr_cache.
      IF sy-subrc = 0 AND lr_cache IS BOUND.
        ro_instance = lr_cache->instance.
        ASSERT ro_instance IS BOUND.
        FREE lr_cache.
      ENDIF.
    ENDIF.

    IF ro_instance IS NOT BOUND.
      CREATE OBJECT ro_instance
        EXPORTING
          io_repo = io_repo.

      CREATE DATA lr_cache.
      lr_cache->instance = ro_instance.
      lr_cache->key = io_repo->get_key( ).

      TRY.
          INSERT lr_cache->* INTO TABLE st_cache.
        CATCH cx_sy_itab_duplicate_key ##NO_HANDLER.
          " Can occur if iv_force_new was set to true
      ENDTRY.
      FREE lr_cache.
    ENDIF.
  ENDMETHOD.

  METHOD constructor.
    DATA: lx_ex TYPE REF TO cx_sy_create_object_error.

    ASSERT io_repo IS BOUND.
    mo_repository = io_repo.

    TRY.
        init_listener( ).
      CATCH cx_sy_create_object_error INTO lx_ex.
        RAISE EXCEPTION TYPE zcx_abapgit_exception
          EXPORTING
            previous = lx_ex
            text     = lx_ex->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD init_listener.
    mv_callback_classname = mo_repository->get_dot_abapgit( )->get_callback_classname( ).

    IF mv_callback_classname IS NOT INITIAL.
      CREATE OBJECT mo_listener TYPE (mv_callback_classname).
    ELSE.
      CREATE OBJECT mo_listener TYPE lcl_dummy_callback_listener.
    ENDIF.
  ENDMETHOD.

  METHOD lif_callback_listener~on_after_pull.
    CONSTANTS: lc_parmname_package     TYPE abap_parmname VALUE 'IV_PACKAGE',
               lc_parmname_old_version TYPE abap_parmname VALUE 'IV_OLD_VERSION',
               lc_parmname_new_version TYPE abap_parmname VALUE 'IV_NEW_VERSION'.
    DATA: lt_parameters  TYPE abap_parmbind_tab,
          ls_parameter   TYPE abap_parmbind,
          lr_package     TYPE REF TO devclass,
          lr_old_version TYPE REF TO string,
          lr_new_version TYPE REF TO string.

    CREATE DATA: lr_package, lr_old_version, lr_new_version.

    ls_parameter-name = lc_parmname_package.
    ls_parameter-kind = cl_abap_objectdescr=>exporting.
    lr_package->* = iv_package.
    ls_parameter-value = lr_package.
    INSERT ls_parameter INTO TABLE lt_parameters.

    ls_parameter-name = lc_parmname_old_version.
    ls_parameter-kind = cl_abap_objectdescr=>exporting.
    lr_old_version->* = iv_old_version.
    ls_parameter-value = lr_old_version.
    INSERT ls_parameter INTO TABLE lt_parameters.

    ls_parameter-name = lc_parmname_new_version.
    ls_parameter-kind = cl_abap_objectdescr=>exporting.
    lr_new_version->* = iv_new_version.
    ls_parameter-value = lr_old_version.
    INSERT ls_parameter INTO TABLE lt_parameters.

    dyn_call_method( io_object     = mo_listener
                     iv_methname   = gc_methname_on_after_pull
                     it_parameters = lt_parameters ).
  ENDMETHOD.

  METHOD check_execution_allowed.
    IF is_dummy_listener( ) = abap_true.
      rv_allowed = abap_true.
      RETURN.
    ENDIF.

    " Prevent arbitrary code execution by allowing the user to take a look at the (possibly just
    " pulled) callback implementation.
    rv_allowed = lcl_popups=>popup_to_decide_callback_exec(
                   iv_methname           = iv_methname
                   iv_callback_classname = mv_callback_classname ).
  ENDMETHOD.

  METHOD is_dummy_listener.
    DATA: lo_dummy       TYPE REF TO lcl_dummy_callback_listener ##NEEDED,
          lo_class_descr TYPE REF TO cl_abap_classdescr,
          lo_ref_descr   TYPE REF TO cl_abap_refdescr.

    lo_ref_descr ?= cl_abap_typedescr=>describe_by_data( lo_dummy ).
    lo_class_descr ?= lo_ref_descr->get_referenced_type( ).
    rv_is_dummy = lo_class_descr->applies_to( mo_listener ).
  ENDMETHOD.

  METHOD dyn_call_method.
    DATA: lx_ex TYPE REF TO cx_sy_dyn_call_error.

    ASSERT: io_object IS BOUND,
            iv_methname IS NOT INITIAL.

    TRY.
        CALL METHOD io_object->(iv_methname) PARAMETER-TABLE it_parameters.
      CATCH cx_sy_dyn_call_error INTO lx_ex.
        " If a short dump occurs here the listener object does not implement the callback methods
        " correctly, see LIF_CALLBACK_LISTENER for the method signatures.
        RAISE EXCEPTION lx_ex.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
