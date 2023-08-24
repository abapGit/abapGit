CLASS ltcl_build DEFINITION DEFERRED.

CLASS ltcl_build DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
  PRIVATE SECTION.

    CLASS-DATA gi_environment TYPE REF TO if_osql_test_environment.
    CLASS-METHODS class_setup.
    CLASS-METHODS class_teardown.
    CLASS-METHODS create_envoirment.
    METHODS check_build_wo_filter FOR TESTING RAISING zcx_abapgit_exception.
    METHODS check_build_w_filter FOR TESTING RAISING zcx_abapgit_exception.
ENDCLASS.
CLASS ltcl_build IMPLEMENTATION.

  METHOD create_envoirment.


    DATA lt_tables TYPE if_osql_test_environment=>ty_t_sobjnames.
    DATA ls_tadir TYPE tadir.
    DATA lt_tadir TYPE STANDARD TABLE OF tadir.
    DATA ls_tdevc TYPE tdevc.
    DATA lt_tdevc TYPE STANDARD TABLE OF tdevc.

    APPEND 'TADIR' TO lt_tables.
    APPEND 'TDEVC' TO lt_tables.
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

  METHOD check_build_wo_filter.

    DATA lo_dot TYPE REF TO zcl_abapgit_dot_abapgit.
    DATA li_log TYPE REF TO zif_abapgit_log.
    DATA lt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA lv_runtime TYPE i.
    DATA lr_ex TYPE REF TO zcx_abapgit_exception.
    DATA li_tadir TYPE REF TO zif_abapgit_tadir.
    DATA lo_ex TYPE REF TO zcx_abapgit_exception.
    DATA lo_ex_cast TYPE REF TO cx_sy_move_cast_error.
    DATA lv_top_package TYPE devclass.

    lv_top_package = 'BASIS'.
    TRY.
        CREATE OBJECT li_log TYPE zcl_abapgit_log.

        lo_dot = zcl_abapgit_dot_abapgit=>build_default( ).
        lo_dot->set_folder_logic( iv_logic = zif_abapgit_dot_abapgit=>c_folder_logic-full ).

        lt_tadir = zcl_abapgit_factory=>get_tadir( )->read(
            iv_package = lv_top_package
            io_dot     = lo_dot
            iv_check_exists       = abap_false ).

        cl_abap_unit_assert=>assert_not_initial( lt_tadir ).


      CATCH zcx_abapgit_exception  INTO lo_ex.
        cl_abap_unit_assert=>fail( msg = lo_ex->get_text( ) ).

      CATCH cx_sy_move_cast_error INTO lo_ex_cast.
        cl_abap_unit_assert=>fail( msg = lo_ex_cast->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD check_build_w_filter.

    DATA lo_dot TYPE REF TO zcl_abapgit_dot_abapgit.
    DATA li_log TYPE REF TO zif_abapgit_log.
    DATA lv_runtime TYPE i.
    DATA lr_ex TYPE REF TO zcx_abapgit_exception.
    DATA lo_tadir TYPE REF TO zcl_abapgit_tadir.
    DATA lo_ex_cast TYPE REF TO cx_sy_move_cast_error.
    DATA lo_ex TYPE REF TO zcx_abapgit_exception.
    DATA lv_top_package TYPE devclass.
    DATA lt_filter TYPE zif_abapgit_definitions=>ty_obj_tt.
    DATA ls_filter TYPE  zif_abapgit_definitions=>ty_obj.
    DATA lt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt.

    lv_top_package = 'BASIS'.

    ls_filter-obj_type = 'NSPC'.
    ls_filter-obj_name = '/STMC/'.
    INSERT ls_filter INTO TABLE lt_filter.

    ls_filter-obj_type = 'DEVC'.
    ls_filter-obj_name = '/STMC/FND'.
    INSERT ls_filter INTO TABLE lt_filter.

    ls_filter-obj_type = 'DTEL'.
    ls_filter-obj_name = 'DEVCLASS'.
    INSERT ls_filter INTO TABLE lt_filter.

    ls_filter-obj_type = 'DEVC'.
    ls_filter-obj_name = 'BASIS'.
    INSERT ls_filter INTO TABLE lt_filter.

    ls_filter-obj_type = 'DEVC'.
    ls_filter-obj_name = '$SWF_RUN_CNT'.
    INSERT ls_filter INTO TABLE lt_filter.


    TRY.
        CREATE OBJECT li_log TYPE zcl_abapgit_log.

        lo_dot = zcl_abapgit_dot_abapgit=>build_default( ).
        lo_dot->set_folder_logic( iv_logic = zif_abapgit_dot_abapgit=>c_folder_logic-full ).

        lt_tadir = zcl_abapgit_factory=>get_tadir( )->read(
                   iv_package = lv_top_package
                   io_dot     = lo_dot
                   iv_check_exists       = abap_false ).

        cl_abap_unit_assert=>assert_not_initial( lt_tadir ).

        cl_abap_unit_assert=>assert_equals(
            act = lines( lt_tadir )
            exp = lines( lt_filter ) ).

      CATCH zcx_abapgit_exception  INTO lo_ex.
        cl_abap_unit_assert=>fail( msg = lo_ex->get_text( ) ).

      CATCH cx_sy_move_cast_error INTO lo_ex_cast.
        cl_abap_unit_assert=>fail( msg = lo_ex_cast->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD class_setup.
    create_envoirment(  ).
  ENDMETHOD.

  METHOD class_teardown.
    gi_environment->destroy(  ).
  ENDMETHOD.

ENDCLASS.
