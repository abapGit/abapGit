CLASS ltcl_table_compare_tests DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA mi_cut TYPE REF TO zif_abapgit_comparator.
    DATA ms_tabl TYPE zif_abapgit_object_tabl=>ty_internal.

    METHODS:
      setup,
      compare
        IMPORTING
          is_local         TYPE zif_abapgit_object_tabl=>ty_internal
          is_remote        TYPE zif_abapgit_object_tabl=>ty_internal
        RETURNING
          VALUE(rs_result) TYPE zif_abapgit_comparator=>ty_result
        RAISING
          zcx_abapgit_exception,
      identical_tables FOR TESTING,
      add_field FOR TESTING,
      remove_field FOR TESTING,
      change_field FOR TESTING.

ENDCLASS.

CLASS ltcl_table_compare_tests IMPLEMENTATION.

  METHOD setup.

    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.

    ls_item-obj_type = 'TABL'.
    ls_item-obj_name = 'T100'.

    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = 'T100'
      IMPORTING
        dd02v_wa      = ms_tabl-dd02v
      TABLES
        dd03p_tab     = ms_tabl-dd03p
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    ASSERT sy-subrc = 0.

    CREATE OBJECT mi_cut TYPE zcl_abapgit_object_tabl_compar
      EXPORTING
        is_item = ls_item.

  ENDMETHOD.

  METHOD compare.

    DATA:
      li_local_xml      TYPE REF TO zif_abapgit_xml_output,
      li_remote_xml     TYPE REF TO zif_abapgit_xml_output,
      li_local_version  TYPE REF TO zif_abapgit_xml_input,
      li_remote_version TYPE REF TO zif_abapgit_xml_input,
      li_log            TYPE REF TO zif_abapgit_log.

    TRY.
        CREATE OBJECT li_local_xml TYPE zcl_abapgit_xml_output.

        li_local_xml->add( iv_name = 'DD02V'
                           ig_data = is_local-dd02v ).
        li_local_xml->add( iv_name = 'DD03P_TABLE'
                           ig_data = is_local-dd03p ).

        CREATE OBJECT li_local_version TYPE zcl_abapgit_xml_input
          EXPORTING
            iv_xml      = li_local_xml->render( )
            iv_filename = 't100.tabl.xml'.

        CREATE OBJECT li_remote_xml TYPE zcl_abapgit_xml_output.

        li_remote_xml->add( iv_name = 'DD02V'
                            ig_data = is_remote-dd02v ).
        li_remote_xml->add( iv_name = 'DD03P_TABLE'
                            ig_data = is_remote-dd03p ).

        CREATE OBJECT li_remote_version TYPE zcl_abapgit_xml_input
          EXPORTING
            iv_xml      = li_remote_xml->render( )
            iv_filename = 't100.tabl.xml'.

      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( 'Error initializing XML' ).
    ENDTRY.

    CREATE OBJECT li_log TYPE zcl_abapgit_log.

    rs_result = mi_cut->compare(
      ii_local  = li_local_version
      ii_remote = li_remote_version
      ii_log    = li_log ).

  ENDMETHOD.

  METHOD identical_tables.

    DATA ls_result TYPE zif_abapgit_comparator=>ty_result.

    TRY.
        ls_result = compare(
          is_local  = ms_tabl
          is_remote = ms_tabl ).

        cl_abap_unit_assert=>assert_initial( ls_result-text ).
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.

  METHOD add_field.

    DATA ls_result TYPE zif_abapgit_comparator=>ty_result.
    DATA ls_local TYPE zif_abapgit_object_tabl=>ty_internal.

    ls_local = ms_tabl.
    DELETE ls_local-dd03p WHERE fieldname = 'MSGNR'. " meaning it will be added by remote

    TRY.
        ls_result = compare(
          is_local  = ls_local
          is_remote = ms_tabl ).

        cl_abap_unit_assert=>assert_initial( ls_result-text ).
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.

  METHOD remove_field.

    DATA ls_result TYPE zif_abapgit_comparator=>ty_result.
    DATA ls_remote TYPE zif_abapgit_object_tabl=>ty_internal.

    ls_remote = ms_tabl.
    DELETE ls_remote-dd03p WHERE fieldname = 'MSGNR'. " meaning it will be removed by remote

    TRY.
        ls_result = compare(
          is_local  = ms_tabl
          is_remote = ls_remote ).

        cl_abap_unit_assert=>assert_not_initial( ls_result-text ).
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.

  METHOD change_field.

    DATA ls_result TYPE zif_abapgit_comparator=>ty_result.
    DATA ls_remote TYPE zif_abapgit_object_tabl=>ty_internal.

    FIELD-SYMBOLS <ls_dd03p> TYPE dd03p.

    ls_remote = ms_tabl.

    READ TABLE ls_remote-dd03p ASSIGNING <ls_dd03p> WITH KEY fieldname = 'MSGNR'.
    ASSERT sy-subrc = 0.

    <ls_dd03p>-rollname = 'CHAR1'. " replace with shorter data element

    TRY.
        ls_result = compare(
          is_local  = ms_tabl
          is_remote = ls_remote ).

        cl_abap_unit_assert=>assert_not_initial( ls_result-text ).
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
