CLASS ltcl_flow_logic DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
  PUBLIC SECTION.
    METHODS test1 FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_flow_logic IMPLEMENTATION.

  METHOD test1.

    DATA lt_features TYPE zif_abapgit_flow_logic=>ty_features.

    lt_features = zcl_abapgit_flow_logic=>get( ).

* todo

  ENDMETHOD.

ENDCLASS.
