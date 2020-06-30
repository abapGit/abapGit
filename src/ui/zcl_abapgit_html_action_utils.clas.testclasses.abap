CLASS ltcl_html_action_utils DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PUBLIC SECTION.

    CLASS-METHODS class_constructor.
    METHODS get_field FOR TESTING.
    METHODS parse_fields_simple_case FOR TESTING.
    METHODS parse_fields_advanced_case FOR TESTING.
    METHODS parse_fields_unescape FOR TESTING.
    METHODS parse_fields_german_umlauts FOR TESTING.

  PRIVATE SECTION.

    CONSTANTS: BEGIN OF c_german_umlaut_as_hex,
                 lower_case_ae TYPE xstring VALUE 'C3A4',
                 lower_case_oe TYPE xstring VALUE 'C3B6',
                 lower_case_ue TYPE xstring VALUE 'C3BC',
               END OF c_german_umlaut_as_hex.

    CLASS-DATA: BEGIN OF gs_german_umlaut_as_char,
                  lower_case_ae TYPE string,
                  lower_case_oe TYPE string,
                  lower_case_ue TYPE string,
                END OF gs_german_umlaut_as_char.

    DATA mv_given_parse_string TYPE string.
    DATA mt_parsed_fields TYPE tihttpnvp.

    METHODS _given_string_is
      IMPORTING
        iv_string TYPE string.
    METHODS _when_fields_are_parsed.
    METHODS _then_fields_should_be
      IMPORTING
        iv_index TYPE i
        iv_name  TYPE string
        iv_value TYPE string.

    CLASS-METHODS _hex_to_char
      IMPORTING
        iv_x        TYPE xstring
      RETURNING
        VALUE(rv_s) TYPE string.

ENDCLASS.

CLASS ltcl_html_action_utils IMPLEMENTATION.

  METHOD class_constructor.

    gs_german_umlaut_as_char-lower_case_ae = _hex_to_char( c_german_umlaut_as_hex-lower_case_ae ).
    gs_german_umlaut_as_char-lower_case_oe = _hex_to_char( c_german_umlaut_as_hex-lower_case_oe ).
    gs_german_umlaut_as_char-lower_case_ue = _hex_to_char( c_german_umlaut_as_hex-lower_case_ue ).

  ENDMETHOD.

  METHOD get_field.

    DATA: lt_fields TYPE tihttpnvp,
          ls_answer LIKE LINE OF lt_fields,
          ls_field  LIKE LINE OF lt_fields.

    ls_answer-name  = 'NAME'.
    ls_answer-value = 'TEST'.
    APPEND ls_answer TO lt_fields.

    zcl_abapgit_html_action_utils=>get_field( EXPORTING iv_name = 'NAME'
                                                        it_field = lt_fields
                                      CHANGING  cg_field   = ls_field-value ).
    zcl_abapgit_html_action_utils=>get_field( EXPORTING iv_name = 'NAME'
                                                        it_field = lt_fields
                                      CHANGING  cg_field   = ls_field ).

    ls_answer-name  = 'TEST'.
    ls_answer-value = 'TEST'.

    cl_abap_unit_assert=>assert_equals(
      act = ls_field
      exp = ls_answer ). " Both field are filled!

  ENDMETHOD.

  METHOD parse_fields_simple_case.

    _given_string_is( `committer_name=Gustav Gans` ).

    _when_fields_are_parsed( ).

    _then_fields_should_be( iv_index = 1
                            iv_name = `COMMITTER_NAME`
                            iv_value = `Gustav Gans` ).

  ENDMETHOD.

  METHOD parse_fields_advanced_case.

    _given_string_is( `committer_name=Albert Schweitzer&`
                   && `committer_email=albert.schweitzer@googlemail.com&`
                   && `comment=dummy comment&`
                   && `body=Message body<<new>><<new>>with line break<<new>>&`
                   && `author_name=Karl Klammer&`
                   && `author_email=karl@klammer.com` ).

    _when_fields_are_parsed( ).

    _then_fields_should_be( iv_index = 1
                            iv_name  = `COMMITTER_NAME`
                            iv_value = `Albert Schweitzer` ).

    _then_fields_should_be( iv_index = 2
                            iv_name  = `COMMITTER_EMAIL`
                            iv_value = `albert.schweitzer@googlemail.com` ).

    _then_fields_should_be( iv_index = 3
                            iv_name  = `COMMENT`
                            iv_value = `dummy comment` ).

    _then_fields_should_be( iv_index = 4
                            iv_name  = `BODY`
                            iv_value = `Message body<<new>><<new>>with line break<<new>>` ).

    _then_fields_should_be( iv_index = 5
                            iv_name  = `AUTHOR_NAME`
                            iv_value = `Karl Klammer` ).

    _then_fields_should_be( iv_index = 6
                            iv_name  = `AUTHOR_EMAIL`
                            iv_value = `karl@klammer.com` ).

  ENDMETHOD.

  METHOD parse_fields_unescape.
* file status = '?', used in staging page

    _given_string_is( '/SRC/ZFOOBAR.PROG.ABAP=%3F' ).

    _when_fields_are_parsed( ).

    _then_fields_should_be( iv_index = 1
                            iv_name  = '/SRC/ZFOOBAR.PROG.ABAP'
                            iv_value = '?' ).

  ENDMETHOD.

  METHOD parse_fields_german_umlauts.

    DATA: lv_ae       TYPE string,
          lv_oe       TYPE string,
          lv_ue       TYPE string,
          lv_ae_oe_ue TYPE string.


    lv_ae = gs_german_umlaut_as_char-lower_case_ae.
    lv_oe = gs_german_umlaut_as_char-lower_case_oe.
    lv_ue = gs_german_umlaut_as_char-lower_case_ue.

    lv_ae_oe_ue = lv_ae && lv_oe && lv_ue.

    _given_string_is( |committer_name=Christian G{ lv_ue }nter&|
                   && |committer_email=guenne@googlemail.com&|
                   && |comment={ lv_ae_oe_ue }&|
                   && |body=Message body<<new>><<new>>with line break<<new>>and umlauts. { lv_ae_oe_ue }&|
                   && |author_name=Gerd Schr{ lv_oe }der&|
                   && |author_email=gerd@schroeder.com| ).

    _when_fields_are_parsed( ).

    _then_fields_should_be( iv_index = 1
                            iv_name  = `COMMITTER_NAME`
                            iv_value = |Christian G{ lv_ue }nter| ).

    _then_fields_should_be( iv_index = 2
                            iv_name  = `COMMITTER_EMAIL`
                            iv_value = `guenne@googlemail.com` ).

    _then_fields_should_be( iv_index = 3
                            iv_name  = `COMMENT`
                            iv_value = lv_ae_oe_ue ).

    _then_fields_should_be( iv_index = 4
                            iv_name  = `BODY`
                            iv_value = |Message body<<new>><<new>>with line break<<new>>and umlauts. { lv_ae_oe_ue }| ).

    _then_fields_should_be( iv_index = 5
                            iv_name  = `AUTHOR_NAME`
                            iv_value = |Gerd Schr{ lv_oe }der| ).

    _then_fields_should_be( iv_index = 6
                            iv_name  = `AUTHOR_EMAIL`
                            iv_value = `gerd@schroeder.com` ).

  ENDMETHOD.

  METHOD _given_string_is.

    mv_given_parse_string = iv_string.

  ENDMETHOD.

  METHOD _when_fields_are_parsed.

    mt_parsed_fields = zcl_abapgit_html_action_utils=>parse_fields_upper_case_name( mv_given_parse_string ).

  ENDMETHOD.

  METHOD _then_fields_should_be.

    FIELD-SYMBOLS: <ls_parsed_field> LIKE LINE OF mt_parsed_fields.

    READ TABLE mt_parsed_fields ASSIGNING <ls_parsed_field>
                                INDEX iv_index.

    cl_abap_unit_assert=>assert_subrc( exp = 0
                                       msg = |No parsed field found at index { iv_index }| ).

    cl_abap_unit_assert=>assert_equals( act = <ls_parsed_field>-name
                                        exp = iv_name
                                        msg = |Name at index { iv_index } should be { iv_name }| ).

    cl_abap_unit_assert=>assert_equals( act = <ls_parsed_field>-value
                                        exp = iv_value
                                        msg = |Value at index { iv_index } should be { iv_value }| ).

  ENDMETHOD.

  METHOD _hex_to_char.

    DATA lo_conv TYPE REF TO cl_abap_conv_in_ce.

    lo_conv = cl_abap_conv_in_ce=>create( ).
    lo_conv->convert( EXPORTING input = iv_x IMPORTING data = rv_s ).

  ENDMETHOD.

ENDCLASS.
