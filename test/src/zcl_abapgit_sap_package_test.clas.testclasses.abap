*"* use this source file for your ABAP unit test classes
CLASS ltcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION MEDIUM FINAL.
  PRIVATE SECTION.
    CLASS-DATA gi_environment TYPE REF TO if_osql_test_environment.
    CLASS-METHODS class_setup.
    CLASS-METHODS class_teardown.
    CLASS-METHODS create_envoirment.
    METHODS check_list_subpackages FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.
CLASS ltcl_test IMPLEMENTATION.

  METHOD create_envoirment.

    DATA lt_tables TYPE if_osql_test_environment=>ty_t_sobjnames.
    DATA ls_tadir TYPE tadir.
    DATA lt_tadir TYPE STANDARD TABLE OF tadir.
    DATA ls_tdevc TYPE tdevc.
    DATA lt_tdevc TYPE STANDARD TABLE OF tdevc.

    APPEND 'TADIR' TO lt_tables.
    APPEND 'TDEVC' TO lt_tables.
    APPEND 'SOTR_HEADU' TO lt_tables.
    gi_environment = cl_osql_test_environment=>create( lt_tables ).

    ls_tadir-pgmid = 'R3TR'.
    ls_tadir-object = 'DEVC'.
    ls_tadir-obj_name = 'BASIS'.
    ls_tadir-devclass = 'BASIS'.
    INSERT ls_tadir INTO TABLE lt_tadir.

    ls_tadir-pgmid = 'R3TR'.
    ls_tadir-object = 'DEVC'.
    ls_tadir-obj_name = '/STMC/FND'.
    ls_tadir-devclass = '/STMC/FND'.
    INSERT ls_tadir INTO TABLE lt_tadir.

    ls_tadir-pgmid = 'R3TR'.
    ls_tadir-object = 'DTEL'.
    ls_tadir-obj_name = 'DEVCLASS'.
    ls_tadir-devclass = 'BASIS'. "not in reality
    INSERT ls_tadir INTO TABLE lt_tadir.

    ls_tadir-pgmid = 'R3TR'.
    ls_tadir-object = 'DTEL'.
    ls_tadir-obj_name = 'MATNR'.
    ls_tadir-devclass = 'BASIS'. "not in reality
    INSERT ls_tadir INTO TABLE lt_tadir.

    gi_environment->insert_test_data( lt_tadir ).

    ls_tdevc-devclass = 'BASIS'.
    INSERT ls_tdevc INTO TABLE lt_tdevc.

    ls_tdevc-devclass = '$SWF_RUN_CNT'.
    ls_tdevc-parentcl = '/STMC/FND'. "not in reality
    INSERT ls_tdevc INTO TABLE lt_tdevc.

    ls_tdevc-devclass = '/STMC/FND'.
    ls_tdevc-parentcl = 'BASIS'. "not in reality
    INSERT ls_tdevc INTO TABLE lt_tdevc.

    gi_environment->insert_test_data( lt_tdevc ).

  ENDMETHOD.


  METHOD check_list_subpackages.
    DATA lt_packages  TYPE zif_abapgit_sap_package=>ty_devclass_tt.

    DATA lt_list TYPE zif_abapgit_sap_package=>ty_devclass_tt.
    DATA lt_list2 TYPE zif_abapgit_sap_package=>ty_devclass_tt.
    DATA lt_list3 TYPE zif_abapgit_sap_package=>ty_devclass_tt.

    SELECT devclass FROM tdevc
      INTO TABLE lt_list
      WHERE parentcl = 'BASIS'
      ORDER BY PRIMARY KEY.                 "#EC CI_SUBRC "#EC CI_GENBUFF

    READ TABLE lt_list TRANSPORTING NO FIELDS
      WITH KEY table_line = '/STMC/FND'.
    IF sy-subrc <> 0.
      cl_abap_unit_assert=>fail( |Package /STMC/FND not found as sub package of BASIS| ).
    ENDIF.

    SELECT devclass FROM tdevc
      INTO TABLE lt_list2
      WHERE parentcl = '/STMC/FND'
      ORDER BY PRIMARY KEY.                 "#EC CI_SUBRC "#EC CI_GENBUFF

    READ TABLE lt_list2 TRANSPORTING NO FIELDS
      WITH KEY table_line = '$SWF_RUN_CNT'.
    IF sy-subrc <> 0.
      cl_abap_unit_assert=>fail( |Package $SWF_RUN_CNT not found as sub package of /STMC/FND| ).
    ENDIF.

    SELECT devclass FROM tdevc
      INTO TABLE lt_list3
      FOR ALL ENTRIES IN lt_list
      WHERE parentcl = lt_list-table_line
      ORDER BY PRIMARY KEY.           "#EC CI_SUBRC "#EC CI_GENBUFF

    READ TABLE lt_list3 TRANSPORTING NO FIELDS
      WITH KEY table_line = '$SWF_RUN_CNT'.
    IF sy-subrc <> 0.
      cl_abap_unit_assert=>fail( |Package $SWF_RUN_CNT not found as sub package of /STMC/FND with for all entries| ).
    ENDIF.

    lt_packages = zcl_abapgit_factory=>get_sap_package( 'BASIS' )->list_subpackages( ).

    READ TABLE lt_packages TRANSPORTING NO FIELDS
      WITH KEY table_line = '$SWF_RUN_CNT'.
    IF sy-subrc <> 0.
      cl_abap_unit_assert=>fail( |Package $SWF_RUN_CNT not found as sub package with method list_subpackages| ).
    ENDIF.

  ENDMETHOD.

  METHOD class_setup.
    create_envoirment( ).
  ENDMETHOD.

  METHOD class_teardown.
    IF NOT gi_environment IS INITIAL.
      gi_environment->destroy( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
