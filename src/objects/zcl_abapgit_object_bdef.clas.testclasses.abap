CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS:
      serialize FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD serialize.
    DATA: ls_item TYPE zif_abapgit_definitions=>ty_item,
          lo_cut  TYPE REF TO zcl_abapgit_object_bdef.

    ls_item-obj_type = 'BDEF'.
    ls_item-obj_name = 'ZBDEF_TEST'.

    CREATE OBJECT lo_cut
      EXPORTING
        is_item     = ls_item
        iv_language = 'E'.

    cl_abap_unit_assert=>assert_bound( lo_cut ).
  ENDMETHOD.
ENDCLASS.
