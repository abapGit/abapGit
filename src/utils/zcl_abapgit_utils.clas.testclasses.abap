CLASS ltcl_utils_test DEFINITION FINAL
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS extract_author_data1 FOR TESTING RAISING cx_static_check.
    METHODS extract_author_data2 FOR TESTING RAISING cx_static_check.
    METHODS extract_author_data3 FOR TESTING RAISING cx_static_check.
    METHODS extract_author_data4 FOR TESTING RAISING cx_static_check.
    METHODS extract_author_data5 FOR TESTING RAISING cx_static_check.
    METHODS extract_author_data6 FOR TESTING RAISING cx_static_check.
    METHODS extract_author_data7 FOR TESTING RAISING cx_static_check.
    METHODS extract_author_data8 FOR TESTING RAISING cx_static_check.
    METHODS extract_author_data9 FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_utils_test IMPLEMENTATION.

  METHOD extract_author_data1.
    TRY.
        zcl_abapgit_utils=>extract_author_data( 'Volker Jägle äÖüß <github@beimir.net> 1573216988 +0000' ).
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( |Language-related special characters in author name are allowed.| ).
    ENDTRY.
  ENDMETHOD.

  METHOD extract_author_data2.
    TRY.
        zcl_abapgit_utils=>extract_author_data(
          'pull[bot&%#$] <39814207+pull[bot]@users.noreply.github.com> 1573216988 +0000' ).
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( |Special characters in author name are allowed.| ).
    ENDTRY.
  ENDMETHOD.

  METHOD extract_author_data3.
    TRY.
        zcl_abapgit_utils=>extract_author_data(
          'pull[bot] <39814207+pull[bot]@users.noreply.github.com> 1573216988 +00001' ).
        cl_abap_unit_assert=>fail( |+00001 shouldn't be valid cause it's too long.| ).
      CATCH zcx_abapgit_exception.
    ENDTRY.
  ENDMETHOD.

  METHOD extract_author_data4.
    TRY.
        zcl_abapgit_utils=>extract_author_data(
          'pull[bot] <39814207+pull[bot]@users.noreply.github.com> 15732169881 +0000' ).
        cl_abap_unit_assert=>fail( |Value shouldn't be valid cause time is invalid.| ).
      CATCH zcx_abapgit_exception.
    ENDTRY.
  ENDMETHOD.

  METHOD extract_author_data5.
    TRY.
        zcl_abapgit_utils=>extract_author_data(
          '<39814207+pull[bot]@users.noreply.github.com> 1573216988 +0000' ).
        cl_abap_unit_assert=>fail( |Value shouldn't be valid cause no author name was supplied.| ).
      CATCH zcx_abapgit_exception.
    ENDTRY.
  ENDMETHOD.

  METHOD extract_author_data6.
    TRY.
        zcl_abapgit_utils=>extract_author_data( 'pull[bot] 1573216988 +0000' ).
        cl_abap_unit_assert=>fail( |Value shouldn't be valid cause no email was supplied.| ).
      CATCH zcx_abapgit_exception.
    ENDTRY.
  ENDMETHOD.

  METHOD extract_author_data7.
    TRY.
        zcl_abapgit_utils=>extract_author_data( 'pull[bot] <39814207+pull[bot]@users.noreply.github.com> +0000' ).
        cl_abap_unit_assert=>fail( |Value shouldn't be valid cause no time was supplied.| ).
      CATCH zcx_abapgit_exception.
    ENDTRY.
  ENDMETHOD.

  METHOD extract_author_data8.
    TRY.
        zcl_abapgit_utils=>extract_author_data(
          'pull[bot] <39814207+pull[bot]@users.noreply.github.com> 1573216988' ).
        cl_abap_unit_assert=>fail( |Value shouldn't be valid cause +0000 wasn't supplied.| ).
      CATCH zcx_abapgit_exception.
    ENDTRY.
  ENDMETHOD.

  METHOD extract_author_data9.
    TRY.
        zcl_abapgit_utils=>extract_author_data(
          '<pull[bot]()> <39814207+pull[bot]@users.noreply.github.com> 1573216988 +0000' ).
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( |Value should be valid though brackets are in author name.| ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
