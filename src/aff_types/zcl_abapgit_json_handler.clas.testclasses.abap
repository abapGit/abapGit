CLASS ltc_json_handler DEFINITION FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: exp             TYPE zif_abapgit_aff_intf_v1=>ty_main,
          json_as_xstring TYPE xstring.
    METHODS setup.
    METHODS first_test_name FOR TESTING RAISING cx_static_check.

ENDCLASS.



CLASS ltc_json_handler IMPLEMENTATION.

  METHOD setup.
    exp = VALUE zif_abapgit_aff_intf_v1=>ty_main(
      format_version = `1`
      header = VALUE #( description = `Example interface for ABAP file formats`
                        original_language = 'E'
                        abap_language_version = 'X' )
    ).

    DATA(json_str_tabl) = VALUE string_table(
      ( `{` )
      ( `  "formatVersion": "1",` )
      ( `  "header": {` )
      ( `    "description": "Example interface for ABAP file formats",` )
      ( `    "originalLanguage": "en"` )
      ( `  }` )
      ( `}` ) ).
    DATA json_string TYPE string.
    json_string = concat_lines_of( table = json_str_tabl sep = cl_abap_char_utilities=>newline ).
    json_as_xstring = cl_abap_codepage=>convert_to( json_string ).
  ENDMETHOD.


  METHOD first_test_name.
    DATA lo_ajson TYPE REF TO zcl_abapgit_json_handler.
    CREATE OBJECT lo_ajson.

    DATA act TYPE zif_abapgit_aff_intf_v1=>ty_main.

    TRY.
        CALL METHOD lo_ajson->('DESERIALIZE')
          EXPORTING
            iv_content = json_as_xstring
          IMPORTING
            ev_data    = act.
      CATCH cx_static_check.
    ENDTRY.

    cl_abap_unit_assert=>assert_equals( act = act
                                        exp = exp ).
  ENDMETHOD.

ENDCLASS.
