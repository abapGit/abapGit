CLASS zcl_abapgit_pr_enumerator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    class-methods is_platform_supported
      importing
        iv_url type string
      returning
        value(rv_yes) type abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_PR_ENUMERATOR IMPLEMENTATION.


  method is_platform_supported.
  endmethod.
ENDCLASS.
