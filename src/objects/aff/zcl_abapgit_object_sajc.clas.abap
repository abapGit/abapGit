CLASS zcl_abapgit_object_sajc DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_object_common_aff
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS zif_abapgit_object~changed_by
        REDEFINITION .
    METHODS zif_abapgit_object~get_deserialize_steps REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_SAJC IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.

    CONSTANTS lc_table_name TYPE tabname VALUE 'APJ_W_JCE_ROOT'.

    SELECT SINGLE lst_ch_user_acct
      FROM (lc_table_name)
      INTO rv_user
      WHERE job_catalog_entry_name = ms_item-obj_name
        AND job_catalog_entry_version = 'I'.

    IF rv_user IS INITIAL.
      SELECT SINGLE lst_ch_user_acct
        FROM (lc_table_name)
        INTO rv_user
        WHERE job_catalog_entry_name = ms_item-obj_name
          AND job_catalog_entry_version = 'A'.
    ENDIF.
  ENDMETHOD.

  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-late TO rt_steps.
  ENDMETHOD.
ENDCLASS.
