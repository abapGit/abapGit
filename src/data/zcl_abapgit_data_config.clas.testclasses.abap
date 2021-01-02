CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA mv_json TYPE string.
    METHODS setup.
    METHODS: to_json FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.

    mv_json = |\{\n| &&
      |  "name": "DUMMY",\n| &&
      |  "type": "TABU",\n| &&
      |  "where": [\n| &&
      |    "DUMMY"\n| &&
      |  ]\n| &&
      |\}|.

  ENDMETHOD.

  METHOD to_json.

    DATA li_config TYPE REF TO zif_abapgit_data_config.
    DATA ls_config TYPE zif_abapgit_data_config=>ty_config.
    DATA lt_files TYPE zif_abapgit_definitions=>ty_files_tt.
    DATA ls_file LIKE LINE OF lt_files.
    DATA lv_json TYPE string.

    ls_config-type = zif_abapgit_data_config=>c_data_type-tabu.
    ls_config-name  = 'DUMMY'.
    APPEND 'DUMMY' TO ls_config-where.

    CREATE OBJECT li_config TYPE zcl_abapgit_data_config.
    li_config->add_config( ls_config ).

    lt_files = li_config->to_json( ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files )
      exp = 1 ).

    READ TABLE lt_files INDEX 1 INTO ls_file.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_not_initial( ls_file-data ).

    lv_json = zcl_abapgit_convert=>xstring_to_string_utf8( ls_file-data ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_json
      exp = mv_json ).

  ENDMETHOD.

ENDCLASS.
