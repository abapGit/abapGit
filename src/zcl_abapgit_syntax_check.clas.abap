CLASS zcl_abapgit_syntax_check DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_code_inspector
  GLOBAL FRIENDS zcl_abapgit_factory.

  PROTECTED SECTION.
    METHODS:
      create_variant REDEFINITION.

ENDCLASS.



CLASS ZCL_ABAPGIT_SYNTAX_CHECK IMPLEMENTATION.


  METHOD create_variant.

    DATA: lt_variant TYPE sci_tstvar,
          ls_variant LIKE LINE OF lt_variant.

    cl_ci_checkvariant=>create(
      EXPORTING
        p_user              = sy-uname
      RECEIVING
        p_ref               = ro_variant
      EXCEPTIONS
        chkv_already_exists = 1
        locked              = 2
        error_in_enqueue    = 3
        not_authorized      = 4
        OTHERS              = 5 ).
    ASSERT sy-subrc = 0.

    ls_variant-testname = 'CL_CI_TEST_SYNTAX_CHECK'.
    INSERT ls_variant INTO TABLE lt_variant.

    ro_variant->set_variant(
      EXPORTING
        p_variant    = lt_variant
      EXCEPTIONS
        not_enqueued = 1
        OTHERS       = 2 ).
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.
