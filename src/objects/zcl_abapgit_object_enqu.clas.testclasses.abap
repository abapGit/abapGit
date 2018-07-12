CLASS ltcl_serialize DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS:
      serialize FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_serialize IMPLEMENTATION.

  METHOD serialize.

    DATA: ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_item-obj_type = 'ENQU'.
    ls_item-obj_name = 'E_USR04'.

    zcl_abapgit_test_serialize=>check( ls_item ).

  ENDMETHOD.

ENDCLASS.
