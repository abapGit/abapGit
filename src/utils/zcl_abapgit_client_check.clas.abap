CLASS zcl_abapgit_client_check DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS
      get_instance
        RETURNING VALUE(ri_client_check) TYPE REF TO zcl_abapgit_client_check.

    METHODS:
      constructor,

      is_repo_object_changes_allowed
        RETURNING VALUE(rv_allowed) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: gi_client_check      TYPE REF TO zcl_abapgit_client_check,
                gv_client_modifiable TYPE abap_bool.
ENDCLASS.


CLASS zcl_abapgit_client_check IMPLEMENTATION.
  METHOD constructor.
    DATA lv_ind TYPE t000-ccnocliind.

    SELECT SINGLE ccnocliind FROM t000 INTO lv_ind
           WHERE mandt = sy-mandt.
    IF sy-subrc = 0
        AND ( lv_ind = ' ' OR lv_ind = '1' ). "check changes allowed
      gv_client_modifiable = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD get_instance.
    IF gi_client_check IS INITIAL.
      CREATE OBJECT gi_client_check TYPE zcl_abapgit_client_check.
    ENDIF.

    ri_client_check = gi_client_check.
  ENDMETHOD.

  METHOD is_repo_object_changes_allowed.
    rv_allowed = gv_client_modifiable.
  ENDMETHOD.
ENDCLASS.
