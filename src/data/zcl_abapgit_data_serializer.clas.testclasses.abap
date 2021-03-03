CLASS ltcl_test DEFINITION DEFERRED.
CLASS zcl_abapgit_data_serializer DEFINITION LOCAL FRIENDS ltcl_test.

CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA mi_cut TYPE REF TO zif_abapgit_data_serializer.
    DATA mi_config TYPE REF TO zif_abapgit_data_config.

    METHODS:
      setup,
      serialize FOR TESTING RAISING zcx_abapgit_exception.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mi_cut TYPE zcl_abapgit_data_serializer.
    CREATE OBJECT mi_config TYPE zcl_abapgit_data_config.
  ENDMETHOD.

  METHOD serialize.

    DATA ls_config TYPE zif_abapgit_data_config=>ty_config.
    DATA lt_files TYPE zif_abapgit_definitions=>ty_files_tt.
    DATA lv_where LIKE LINE OF ls_config-where.

    ls_config-type = 'TABU'.
    ls_config-name = 'T000'.
    lv_where = |MANDT = '{ sy-mandt }'|.
    APPEND lv_where TO ls_config-where.

    mi_config->add_config( ls_config ).

    lt_files = mi_cut->serialize( mi_config ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files )
      exp = 1 ).

  ENDMETHOD.

ENDCLASS.
