*"* use this source file for your ABAP unit test classes

CLASS ltcl_adjust_filter DEFINITION FINAL FOR TESTING INHERITING FROM zcl_abapgit_repo_pre_filter
  DURATION SHORT
  RISK LEVEL HARMLESS.


  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO ltcl_adjust_filter.
    METHODS setup.
    METHODS teardown.
    METHODS adjust_local_filter_r3tr_clas FOR TESTING RAISING cx_static_check.
    METHODS adjust_local_filter_limu_fm FOR TESTING RAISING cx_static_check.
    METHODS adjust_local_filter_limu_meth FOR TESTING RAISING cx_static_check.
    METHODS adjust_local_filter_lang FOR TESTING RAISING cx_static_check.

    METHODS adjust_local_filter_test
      IMPORTING is_e071_filter TYPE zif_abapgit_repo_pre_filter=>ty_e071_filter
                is_filter_adj  TYPE zif_abapgit_definitions=>ty_tadir
                iv_msg         TYPE csequence.
ENDCLASS.


CLASS ltcl_adjust_filter IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD teardown.
    CLEAR mo_cut.
  ENDMETHOD.

  METHOD adjust_local_filter_test.

    DATA lt_e071_filter TYPE zif_abapgit_repo_pre_filter=>ty_e071_filter_tt.
    DATA lt_filter_adj TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA lr_filter_adj TYPE REF TO zif_abapgit_definitions=>ty_tadir.
    DATA lr_ex TYPE REF TO zcx_abapgit_exception.

    INSERT is_e071_filter INTO TABLE lt_e071_filter.

    TRY.
        lt_filter_adj =  mo_cut->adjust_local_filter( it_e071_filter = lt_e071_filter  ).

        READ TABLE lt_filter_adj REFERENCE INTO lr_filter_adj INDEX 1.
        IF sy-subrc = 0.
          cl_aunit_assert=>assert_equals(
              exp                  = is_filter_adj-obj_name
              act                  = lr_filter_adj->obj_name
              msg                  = iv_msg ).

          cl_aunit_assert=>assert_equals(
            exp                  = is_filter_adj-object
            act                  = lr_filter_adj->object
            msg                  = iv_msg ).

          cl_aunit_assert=>assert_equals(
            exp                  = is_filter_adj-pgmid
            act                  = lr_filter_adj->pgmid
            msg                  = iv_msg ).
        ELSE.
          cl_aunit_assert=>fail( msg = iv_msg ).
        ENDIF.

        IF lines( lt_filter_adj ) > 1.
          cl_aunit_assert=>fail( msg = 'To many entries' ).
        ENDIF.

      CATCH zcx_abapgit_exception INTO lr_ex.
        cl_aunit_assert=>fail( msg = lr_ex->get_text( ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD adjust_local_filter_limu_fm.
    DATA ls_e071_filter TYPE zif_abapgit_repo_pre_filter=>ty_e071_filter.
    DATA ls_filter_adj TYPE zif_abapgit_definitions=>ty_tadir.
    DATA lv_msg TYPE zif_abapgit_repo_pre_filter=>ty_msg.
    ls_e071_filter-pgmid = 'LIMU'.
    ls_e071_filter-object = 'FUNC'.
    ls_e071_filter-obj_name = 'GET_R3TR_OBJECT_FROM_LIMU_OBJ'.

    ls_filter_adj-pgmid = 'R3TR'.
    ls_filter_adj-object = 'FUGR'.
    ls_filter_adj-obj_name = 'SEWB'.
    lv_msg = 'Object (R3TR FUGR SEWB) for FM GET_R3TR_OBJECT_FROM_LIMU_OBJ not found'.

    adjust_local_filter_test(
        is_e071_filter     = ls_e071_filter
        is_filter_adj = ls_filter_adj
        iv_msg        = lv_msg ).

  ENDMETHOD.
  METHOD adjust_local_filter_limu_meth.
    DATA ls_e071_filter TYPE zif_abapgit_repo_pre_filter=>ty_e071_filter.
    DATA ls_filter_adj TYPE zif_abapgit_definitions=>ty_tadir.
    DATA lv_msg TYPE zif_abapgit_repo_pre_filter=>ty_msg.
    ls_e071_filter-pgmid = 'LIMU'.
    ls_e071_filter-object = 'METH'.
    ls_e071_filter-obj_name = 'CL_GUI_ALV_GRID               CONSTRUCTOR'.

    ls_filter_adj-pgmid = 'R3TR'.
    ls_filter_adj-object = 'CLAS'.
    ls_filter_adj-obj_name = 'CL_GUI_ALV_GRID'.
    lv_msg = 'Object (R3TR CLAS CL_GUI_ALV_GRID) for Meth CL_GUI_ALV_GRID->CONSTRUCTOR not found'.

    adjust_local_filter_test(
        is_e071_filter = ls_e071_filter
        is_filter_adj = ls_filter_adj
        iv_msg        = lv_msg ).

  ENDMETHOD.

  METHOD adjust_local_filter_r3tr_clas.
    DATA ls_e071_filter TYPE zif_abapgit_repo_pre_filter=>ty_e071_filter.
    DATA ls_filter_adj TYPE zif_abapgit_definitions=>ty_tadir.
    DATA lv_msg TYPE zif_abapgit_repo_pre_filter=>ty_msg.
    ls_e071_filter-pgmid = 'R3TR'.
    ls_e071_filter-object = 'CLAS'.
    ls_e071_filter-obj_name = 'CL_GUI_ALV_GRID'.

    ls_filter_adj-pgmid = 'R3TR'.
    ls_filter_adj-object = 'CLAS'.
    ls_filter_adj-obj_name = 'CL_GUI_ALV_GRID'.
    lv_msg = 'Object (R3TR CLAS CL_GUI_ALV_GRID) for CLAS CL_GUI_ALV_GRID not found'.

    adjust_local_filter_test(
        is_e071_filter = ls_e071_filter
        is_filter_adj = ls_filter_adj
        iv_msg        = lv_msg ).

  ENDMETHOD.

  METHOD adjust_local_filter_lang.
    DATA ls_e071_filter TYPE zif_abapgit_repo_pre_filter=>ty_e071_filter.
    DATA ls_filter_adj TYPE zif_abapgit_definitions=>ty_tadir.
    DATA lt_e071_filter TYPE zif_abapgit_repo_pre_filter=>ty_e071_filter_tt.
    DATA lt_filter_adj TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA lt_filter_adj_res TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA lr_filter_adj TYPE REF TO zif_abapgit_definitions=>ty_tadir.
    DATA lr_ex TYPE REF TO zcx_abapgit_exception.
    DATA lv_msg TYPE zif_abapgit_repo_pre_filter=>ty_msg.
    ls_e071_filter-pgmid = 'LANG'.
    ls_e071_filter-object = 'METH'.
    ls_e071_filter-obj_name = 'CL_GUI_ALV_GRID               CONSTRUCTOR'.
    INSERT ls_e071_filter INTO TABLE lt_e071_filter.

    ls_filter_adj-pgmid = 'R3TR'.
    ls_filter_adj-object = 'CLAS'.
    ls_filter_adj-obj_name = 'CL_GUI_ALV_GRID'.
    INSERT ls_filter_adj INTO TABLE lt_filter_adj.

    ls_filter_adj-pgmid = 'R3TR'.
    ls_filter_adj-object = 'METH'.
    ls_filter_adj-obj_name = 'CL_GUI_ALV_GRID               CONSTRUCTO'. "Has only 40 Chars so the R will be cut
    INSERT ls_filter_adj INTO TABLE lt_filter_adj.

    ls_filter_adj-pgmid = 'LANG'.
    ls_filter_adj-object = 'METH'.
    ls_filter_adj-obj_name = 'CL_GUI_ALV_GRID               CONSTRUCTO'. "Has only 40 Chars so the R will be cut
    INSERT ls_filter_adj INTO TABLE lt_filter_adj.

    SORT lt_filter_adj.

    lv_msg = 'Object (R3TR CLAS CL_GUI_ALV_GRID) for LANG METH CL_GUI_ALV_GRID->CONSTRUCTOR not found'.

    TRY.
        lt_filter_adj_res =  mo_cut->adjust_local_filter( it_e071_filter = lt_e071_filter  ).

        cl_aunit_assert=>assert_equals(
                   exp                  = lt_filter_adj
                   act                  = lt_filter_adj_res
                   msg                  = lv_msg ).

      CATCH zcx_abapgit_exception INTO lr_ex.
        cl_aunit_assert=>fail( msg = lr_ex->get_text( ) ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
