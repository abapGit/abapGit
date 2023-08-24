CLASS ltcl_build DEFINITION DEFERRED.
CLASS zcl_abapgit_tadir DEFINITION LOCAL FRIENDS ltcl_build.

CLASS ltcl_build DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
  PRIVATE SECTION.

    METHODS check_build_wo_filter FOR TESTING RAISING zcx_abapgit_exception.
ENDCLASS.
CLASS ltcl_build IMPLEMENTATION.

  METHOD check_build_wo_filter.

    DATA lo_dot TYPE REF TO zcl_abapgit_dot_abapgit.
    DATA li_log TYPE REF TO zif_abapgit_log.
    DATA lt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA lv_runtime TYPE i.
    DATA lr_ex TYPE REF TO zcx_abapgit_exception.
    DATA lo_tadir TYPE REF TO zcl_abapgit_tadir.
    DATA lo_ex TYPE REF TO zcx_abapgit_exception.
    TRY.
        CREATE OBJECT li_log TYPE zcl_abapgit_log.

        lo_dot = zcl_abapgit_dot_abapgit=>build_default( ).
        lo_dot->set_folder_logic( iv_logic = zif_abapgit_dot_abapgit=>c_folder_logic-full ).

        lo_tadir ?= zcl_abapgit_factory=>get_tadir( ).
        lt_tadir = lo_tadir->build(
                               iv_package = 'BASIS'
                                   io_dot =  lo_dot ).

        cl_abap_unit_assert=>assert_not_initial( lt_tadir ).

      CATCH zcx_abapgit_exception INTO lo_ex.

        cl_abap_unit_assert=>fail( msg = lo_ex->get_text( ) ).

    ENDTRY.
  ENDMETHOD.

ENDCLASS.
