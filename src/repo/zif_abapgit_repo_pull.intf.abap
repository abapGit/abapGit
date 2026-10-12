INTERFACE zif_abapgit_repo_pull
  PUBLIC .

  " The steps of a pull, without any UI. The caller gets the checks,
  " fills in the decisions (popups, form, or "yes to all") and pulls.
  METHODS checks
    RETURNING
      VALUE(rs_checks) TYPE zif_abapgit_definitions=>ty_deserialize_checks
    RAISING
      zcx_abapgit_exception .
  METHODS pull
    IMPORTING
      !is_checks    TYPE zif_abapgit_definitions=>ty_deserialize_checks
      !ii_log       TYPE REF TO zif_abapgit_log OPTIONAL
    RETURNING
      VALUE(ri_log) TYPE REF TO zif_abapgit_log
    RAISING
      zcx_abapgit_exception .

ENDINTERFACE.
