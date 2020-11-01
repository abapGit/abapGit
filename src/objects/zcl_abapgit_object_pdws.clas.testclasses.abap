CLASS ltc_ci DEFINITION FINAL FOR TESTING
  DURATION MEDIUM
  RISK LEVEL CRITICAL.

  PRIVATE SECTION.
    METHODS run_ci FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltc_ci IMPLEMENTATION.

  METHOD run_ci.

    DATA lv_repo_url TYPE string.

    "Use STVARV to optionally override repo in local system
    SELECT SINGLE low
      INTO lv_repo_url
      FROM tvarvc
      WHERE name = 'ABAPGIT_TEST_URL_PDWS'  ##WARN_OK.

    IF sy-subrc = 0.   "Todo: Remove once we have a test repo
      zcl_abapgit_objects_ci_tests=>run(
          iv_object = 'PDWS'
          iv_url  = lv_repo_url ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
