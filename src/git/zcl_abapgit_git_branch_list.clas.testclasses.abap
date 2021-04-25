CLASS ltcl_parse DEFINITION DEFERRED.
CLASS zcl_abapgit_git_branch_list DEFINITION LOCAL FRIENDS ltcl_parse.

CLASS ltcl_parse DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      parse
        IMPORTING
          iv_expected_lines TYPE i DEFAULT 2
        RAISING
          zcx_abapgit_exception.

    METHODS:
      parse_ok_without_first_lf FOR TESTING RAISING zcx_abapgit_exception,
      parse_works FOR TESTING RAISING zcx_abapgit_exception,
      captcha_response_is_caught FOR TESTING RAISING zcx_abapgit_exception,
      use_refs_if_head_is_missing FOR TESTING RAISING cx_static_check.

    DATA: mt_data TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

ENDCLASS.


CLASS ltcl_parse IMPLEMENTATION.

  METHOD parse.

    DATA: lv_data TYPE string,
          lt_list TYPE zif_abapgit_definitions=>ty_git_branch_list_tt.

    CONCATENATE LINES OF mt_data INTO lv_data SEPARATED BY zif_abapgit_definitions=>c_newline.

    zcl_abapgit_git_branch_list=>parse_branch_list(
      EXPORTING
        iv_data = lv_data
      IMPORTING
        et_list = lt_list ).

    cl_abap_unit_assert=>assert_not_initial( lt_list ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_list )
      exp = iv_expected_lines ).

    READ TABLE lt_list WITH KEY name = zif_abapgit_definitions=>c_git_branch-main TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_subrc( ).

  ENDMETHOD.

  METHOD parse_ok_without_first_lf.

* without linefeed after first pkt-line
*
* see https://github.com/git/git/blob/master/Documentation/technical/pack-protocol.txt
*
* "unless otherwise noted the usual pkt-line LF rules apply: the sender SHOULD include a
* LF, but the receiver MUST NOT complain if it is not present"

    APPEND '001d# service=git-upload-pack000000d2b5d5f1f84ebcaeb8a299edd14c959518e9d81bb5 HEAD#asdf' TO mt_data.
    APPEND '003fb5d5f1f84ebcaeb8a299edd14c959518e9d81bb5 refs/heads/main' TO mt_data.
    APPEND '0000' TO mt_data.

    parse( ).

  ENDMETHOD.

  METHOD parse_works.

    APPEND '001e# service=git-upload-pack' TO mt_data.
    APPEND '000001080e6fe6b311f789ccbac6c5122702d4f48a4f6bda HEAD#asdf' TO mt_data.
    APPEND '003f0e6fe6b311f789ccbac6c5122702d4f48a4f6bda refs/heads/main' TO mt_data.

    parse( ).

  ENDMETHOD.

  METHOD captcha_response_is_caught.

* https://github.com/abapGit/abapGit/issues/4523

    DATA lx_error TYPE REF TO zcx_abapgit_exception.
    DATA lv_data TYPE string.

    lv_data = |001e\n service=git-upload-pack\n00000230ERR CAPTCHA required\nYour Bitbucket account etc..|.
    APPEND lv_data TO mt_data.

    TRY.
        parse( ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_error.
        cl_abap_unit_assert=>assert_char_cp(
          act = lx_error->get_text( )
          exp = '*CAPTCHA*' ).
    ENDTRY.

  ENDMETHOD.

  METHOD use_refs_if_head_is_missing.

    " https://github.com/abapGit/abapGit/issues/4703
    " In case HEAD is missing, it should use refs/heads/xxxxx as the default branch

    APPEND '001e# service=git-upload-pack' TO mt_data.
    APPEND '000000f7e6e5b066fb4177bf0780bf343ab2de06368dd973 refs/heads/main' TO mt_data.
    APPEND '0000' TO mt_data.

    parse( iv_expected_lines = 1 ).

  ENDMETHOD.

ENDCLASS.
