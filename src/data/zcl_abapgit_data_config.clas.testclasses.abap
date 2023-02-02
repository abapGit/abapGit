CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA mv_json TYPE string.
    DATA ms_config TYPE zif_abapgit_data_config=>ty_config.

    METHODS setup.
    METHODS double_add_config FOR TESTING RAISING cx_static_check.
    METHODS to_json FOR TESTING RAISING cx_static_check.
    METHODS from_json
      IMPORTING it_files TYPE zif_abapgit_git_definitions=>ty_files_tt
      RAISING   cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.

    mv_json = |\{\n| &&
      |  "name": "DUMMY",\n| &&
      |  "skip_initial": false,\n| &&
      |  "type": "TABU",\n| &&
      |  "where": [\n| &&
      |    "DUMMY"\n| &&
      |  ]\n| &&
      |\}|.

    ms_config-type = zif_abapgit_data_config=>c_data_type-tabu.
    ms_config-name  = 'DUMMY'.
    APPEND 'DUMMY' TO ms_config-where.

  ENDMETHOD.

  METHOD double_add_config.

    DATA li_config TYPE REF TO zif_abapgit_data_config.
    DATA ls_config TYPE zif_abapgit_data_config=>ty_config.

    CREATE OBJECT li_config TYPE zcl_abapgit_data_config.

    ls_config-name = 'HELLO'.
    ls_config-type = zif_abapgit_data_config=>c_data_type-tabu.
    APPEND 'foo' TO ls_config-where.

    li_config->add_config( ls_config ).

    CLEAR ls_config-where.
    APPEND 'bar' TO ls_config-where.
    li_config->add_config( ls_config ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( li_config->get_configs( ) )
      exp = 1 ).

  ENDMETHOD.

  METHOD to_json.

    DATA li_config TYPE REF TO zif_abapgit_data_config.
    DATA lt_files TYPE zif_abapgit_git_definitions=>ty_files_tt.
    DATA ls_file LIKE LINE OF lt_files.
    DATA lv_json TYPE string.


    CREATE OBJECT li_config TYPE zcl_abapgit_data_config.
    li_config->add_config( ms_config ).

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

    from_json( lt_files ).

  ENDMETHOD.

  METHOD from_json.

    DATA li_config TYPE REF TO zif_abapgit_data_config.

    CREATE OBJECT li_config TYPE zcl_abapgit_data_config.

    li_config->from_json( it_files ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( li_config->get_configs( ) )
      exp = 1 ).

  ENDMETHOD.

ENDCLASS.
