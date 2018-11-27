CLASS zcl_abapgit_adhoc_code_insp DEFINITION
  PUBLIC
  CREATE PRIVATE
  INHERITING FROM zcl_abapgit_code_inspector
  GLOBAL FRIENDS zcl_abapgit_factory.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          iv_package   TYPE devclass
          iv_test_name TYPE sci_tstval-testname
        RAISING
          zcx_abapgit_exception.

  PROTECTED SECTION.
    METHODS:
      create_variant REDEFINITION,

      cleanup REDEFINITION.

  PRIVATE SECTION.
    DATA:
      mo_variant   TYPE REF TO cl_ci_checkvariant,
      mv_test_name TYPE sci_tstval-testname.

ENDCLASS.



CLASS zcl_abapgit_adhoc_code_insp IMPLEMENTATION.


  METHOD cleanup.

    super->cleanup( io_set ).

    IF mo_variant IS BOUND.

      mo_variant->delete(
        EXCEPTIONS
          exists_in_insp   = 1
          locked           = 2
          error_in_enqueue = 3
          not_authorized   = 4
          transport_error  = 5
          OTHERS           = 6 ).

      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Couldn't delete variant. Subrc = { sy-subrc }| ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    DATA: lv_check_variant_name TYPE sci_chkv.

    IF iv_test_name IS INITIAL.
      zcx_abapgit_exception=>raise( |Please supply a test name| ).
    ENDIF.

    " we supply a dummy name for the check variant,
    " because we have to persists it to be able to run in parallel.
    " Afterwards it's deleted.

    lv_check_variant_name = |{ sy-uname }_{ sy-datum }_{ sy-uzeit }|.

    super->constructor( iv_package            = iv_package
                        iv_check_variant_name = lv_check_variant_name ).

    mv_test_name = iv_test_name.

  ENDMETHOD.


  METHOD create_variant.

    DATA: lt_variant TYPE sci_tstvar,
          ls_variant LIKE LINE OF lt_variant.

    cl_ci_checkvariant=>create(
      EXPORTING
        p_user              = sy-uname
        p_name              = mv_check_variant_name
      RECEIVING
        p_ref               = mo_variant
      EXCEPTIONS
        chkv_already_exists = 1
        locked              = 2
        error_in_enqueue    = 3
        not_authorized      = 4
        OTHERS              = 5 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Couldn't create variant. Subrc = { sy-subrc }| ).
    ENDIF.

    ls_variant-testname = mv_test_name.
    INSERT ls_variant INTO TABLE lt_variant.

    mo_variant->set_variant(
      EXPORTING
        p_variant    = lt_variant
      EXCEPTIONS
        not_enqueued = 1
        OTHERS       = 2 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Couldn't set variant. Subrc = { sy-subrc }| ).
    ENDIF.

    mo_variant->save(
      EXPORTING
        p_variant         = mo_variant->variant
      EXCEPTIONS
        empty_variant     = 1
        transport_error   = 2
        not_authorized    = 3
        OTHERS            = 4 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Couldn't save variant. Subrc = { sy-subrc }| ).
    ENDIF.

    ro_variant = mo_variant.

  ENDMETHOD.
ENDCLASS.
