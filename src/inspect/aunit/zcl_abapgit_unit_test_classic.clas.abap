CLASS zcl_abapgit_unit_test_classic DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_unit_test_runner.

    "! Runs the tests via CL_AUCV_TEST_RUNNER_STANDARD.
    "! Raises an exception if not available in the system, either SAPLSAUCV_GUI_RUNNER
    "! does not exist in old releases or passport=>get is private in releases NW >= 756
    METHODS constructor
      IMPORTING
        !ii_repo TYPE REF TO zif_abapgit_repo
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_key,
        obj_name TYPE tadir-obj_name,
        obj_type TYPE tadir-object,
      END OF ty_key,
      ty_keys_tt TYPE STANDARD TABLE OF ty_key WITH DEFAULT KEY.

    DATA mi_repo TYPE REF TO zif_abapgit_repo.

    METHODS build_keys
      RETURNING
        VALUE(rt_keys) TYPE ty_keys_tt
      RAISING
        zcx_abapgit_exception.

    METHODS create_runner
      RETURNING
        VALUE(ro_runner) TYPE REF TO object
      RAISING
        zcx_abapgit_exception.

    "! Returns an object of type IF_SAUNIT_INTERNAL_RESULT
    METHODS run_aunit
      IMPORTING
        !it_keys        TYPE ty_keys_tt
      RETURNING
        VALUE(ro_aunit) TYPE REF TO object
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_unit_test_classic IMPLEMENTATION.


  METHOD build_keys.

    DATA lt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA ls_tadir LIKE LINE OF lt_tadir.
    DATA ls_key   LIKE LINE OF rt_keys.

    lt_tadir = mi_repo->get_tadir_objects( ).

    LOOP AT lt_tadir INTO ls_tadir.
      CLEAR ls_key.
      ls_key-obj_type = ls_tadir-object.
      ls_key-obj_name = ls_tadir-obj_name.
      APPEND ls_key TO rt_keys.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    mi_repo = ii_repo.

    TRY.
        CALL METHOD ('\PROGRAM=SAPLSAUCV_GUI_RUNNER\CLASS=PASSPORT')=>get.
      CATCH cx_root.
        zcx_abapgit_exception=>raise( |Not supported in your NW release| ).
    ENDTRY.

  ENDMETHOD.


  METHOD create_runner.

    DATA lo_passport TYPE REF TO object.

    TRY.
        CALL METHOD ('\PROGRAM=SAPLSAUCV_GUI_RUNNER\CLASS=PASSPORT')=>get
          RECEIVING
            result = lo_passport.

        CALL METHOD ('CL_AUCV_TEST_RUNNER_STANDARD')=>create
          EXPORTING
            i_passport = lo_passport
          RECEIVING
            result     = ro_runner.
      CATCH cx_root.
        zcx_abapgit_exception=>raise( |Not supported in your NW release| ).
    ENDTRY.

  ENDMETHOD.


  METHOD run_aunit.

    DATA lo_runner TYPE REF TO object.
    DATA li_result TYPE REF TO data.

    FIELD-SYMBOLS <li_result> TYPE any.

    lo_runner = create_runner( ).

    CREATE DATA li_result TYPE REF TO ('IF_SAUNIT_INTERNAL_RESULT').
    ASSIGN li_result->* TO <li_result>.

    CALL METHOD lo_runner->('RUN_FOR_PROGRAM_KEYS')
      EXPORTING
        i_limit_on_duration_category = '36' " long
        i_limit_on_risk_level        = '33' " critical
        i_program_keys               = it_keys
      IMPORTING
        e_aunit_result               = <li_result>.

    ro_aunit = <li_result>.

  ENDMETHOD.


  METHOD zif_abapgit_unit_test_runner~run.

    DATA lo_aunit   TYPE REF TO object.
    DATA lo_timer   TYPE REF TO zcl_abapgit_timer.
    DATA lt_keys    TYPE ty_keys_tt.
    DATA lv_summary TYPE string.

    lt_keys = build_keys( ).

    lo_timer = zcl_abapgit_timer=>create( iv_count = lines( lt_keys ) )->start( ).

    lo_aunit = run_aunit( lt_keys ).

    lv_summary = lo_timer->end( ).

    rs_result = zcl_abapgit_unit_test_result=>from_aunit( lo_aunit ).
    rs_result-summary = lv_summary.

  ENDMETHOD.
ENDCLASS.
