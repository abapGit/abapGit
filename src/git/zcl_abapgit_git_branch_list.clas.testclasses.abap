CLASS ltcl_parse DEFINITION DEFERRED.
CLASS zcl_abapgit_git_branch_list DEFINITION LOCAL FRIENDS ltcl_parse.

CLASS ltcl_parse DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      parse
        RAISING
          zcx_abapgit_exception.

    METHODS:
      test01 FOR TESTING RAISING zcx_abapgit_exception,
      test02 FOR TESTING RAISING zcx_abapgit_exception.

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
      exp = 2 ).

    READ TABLE lt_list WITH KEY name = zif_abapgit_definitions=>c_git_branch-master TRANSPORTING NO FIELDS.
    cl_abap_unit_assert=>assert_subrc( ).

  ENDMETHOD.

  METHOD test01.

* without linefeed after first pkt-line
*
* see https://github.com/git/git/blob/master/Documentation/technical/pack-protocol.txt
*
* "unless otherwise noted the usual pkt-line LF rules apply: the sender SHOULD include a
* LF, but the receiver MUST NOT complain if it is not present"

    APPEND '001d# service=git-upload-pack000000d2b5d5f1f84ebcaeb8a299edd14c959518e9d81bb5 HEAD#asdf' TO mt_data.
    APPEND '003fb5d5f1f84ebcaeb8a299edd14c959518e9d81bb5 refs/heads/master' TO mt_data.
    APPEND '0000' TO mt_data.

    parse( ).

  ENDMETHOD.

  METHOD test02.

    APPEND '001e# service=git-upload-pack' TO mt_data.
    APPEND '000001080e6fe6b311f789ccbac6c5122702d4f48a4f6bda HEAD#asdf' TO mt_data.
    APPEND '003f0e6fe6b311f789ccbac6c5122702d4f48a4f6bda refs/heads/master' TO mt_data.

    parse( ).

  ENDMETHOD.

ENDCLASS.
