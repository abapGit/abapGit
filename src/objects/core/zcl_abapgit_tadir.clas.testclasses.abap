CLASS ltcl_build DEFINITION DEFERRED.
CLASS zcl_abapgit_tadir DEFINITION LOCAL FRIENDS ltcl_build.

CLASS ltcl_build DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
  PRIVATE SECTION.

    METHODS:

      check_build_wo_filter FOR TESTING RAISING zcx_abapgit_exception.
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
        lo_tadir->build(
          EXPORTING
            iv_package            = 'BASIS'
            io_dot                =  lo_dot
*          iv_ignore_subpackages = abap_false
*          iv_only_local_objects = abap_false
*          ii_log                =
*          it_filter             =
          RECEIVING
            rt_tadir              = lt_tadir
        ).

        cl_abap_unit_assert=>assert_not_initial( lt_tadir ).

      CATCH zcx_abapgit_exception INTO lo_ex.

        cl_abap_unit_assert=>fail(
          EXPORTING
            msg    = lo_ex->get_text( ) ).

    ENDTRY.
  ENDMETHOD.

ENDCLASS.
