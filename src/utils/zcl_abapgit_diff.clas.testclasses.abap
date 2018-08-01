
CLASS ltcl_diff DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: mt_new      TYPE TABLE OF string,
          mt_old      TYPE TABLE OF string,
          mt_expected TYPE zif_abapgit_definitions=>ty_diffs_tt,
          ms_expected LIKE LINE OF mt_expected.

    METHODS: setup.
    METHODS: test.

    METHODS:
      diff01 FOR TESTING,
      diff02 FOR TESTING,
      diff03 FOR TESTING,
      diff04 FOR TESTING,
      diff05 FOR TESTING,
      diff06 FOR TESTING.

ENDCLASS.


CLASS ltcl_diff IMPLEMENTATION.

  DEFINE _new.
    APPEND &1 TO mt_new.
  END-OF-DEFINITION.

  DEFINE _old.
    APPEND &1 TO mt_old.
  END-OF-DEFINITION.

  DEFINE _expected.
    CLEAR ms_expected.
    ms_expected-new_num = &1.
    ms_expected-new     = &2.
    ms_expected-result  = &3.
    ms_expected-old_num = &4.
    ms_expected-old     = &5.
    APPEND ms_expected TO mt_expected.
  END-OF-DEFINITION.

  METHOD setup.
    CLEAR mt_new.
    CLEAR mt_old.
    CLEAR mt_expected.
  ENDMETHOD.

  METHOD test.

    DATA: lv_new  TYPE string,
          lv_xnew TYPE xstring,
          lv_old  TYPE string,
          lv_xold TYPE xstring,
          lo_diff TYPE REF TO zcl_abapgit_diff,
          lt_diff TYPE zif_abapgit_definitions=>ty_diffs_tt.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF lt_diff.


    CONCATENATE LINES OF mt_new INTO lv_new SEPARATED BY zif_abapgit_definitions=>gc_newline.
    CONCATENATE LINES OF mt_old INTO lv_old SEPARATED BY zif_abapgit_definitions=>gc_newline.

    lv_xnew = zcl_abapgit_convert=>string_to_xstring_utf8( lv_new ).
    lv_xold = zcl_abapgit_convert=>string_to_xstring_utf8( lv_old ).

    CREATE OBJECT lo_diff
      EXPORTING
        iv_new = lv_xnew
        iv_old = lv_xold.

    lt_diff = lo_diff->get( ).

    LOOP AT lt_diff ASSIGNING <ls_diff>.
      CLEAR <ls_diff>-short.
    ENDLOOP.

    cl_abap_unit_assert=>assert_equals( act = lt_diff
                                        exp = mt_expected ).


  ENDMETHOD.

  METHOD diff01.

* insert
    _new 'A'.

    "         " NEW  " STATUS                 " OLD
    _expected 1 'A'  zif_abapgit_definitions=>c_diff-insert  '' ''.
    test( ).

  ENDMETHOD.

  METHOD diff02.

* identical
    _new 'A'.
    _old 'A'.

    "         " NEW  " STATUS  " OLD
    _expected 1 'A'  ''        1 'A'.
    test( ).

  ENDMETHOD.

  METHOD diff03.

* delete
    _old 'A'.

    "         " NEW  " STATUS                 " OLD
    _expected '' ''  zif_abapgit_definitions=>c_diff-delete  1 'A'.
    test( ).

  ENDMETHOD.

  METHOD diff04.

* update
    _new 'A+'.
    _old 'A'.

    "         " NEW   " STATUS                 " OLD
    _expected 1 'A+'  zif_abapgit_definitions=>c_diff-update  1 'A'.
    test( ).

  ENDMETHOD.

  METHOD diff05.

* identical
    _new 'A'.
    _new 'B'.
    _old 'A'.
    _old 'B'.

    "         " NEW  " STATUS  " OLD
    _expected 1 'A'  ''        1 'A'.
    _expected 2 'B'  ''        2 'B'.
    test( ).

  ENDMETHOD.

  METHOD diff06.

    _new 'A'.
    _new 'B'.
    _new 'inserted'.
    _new 'C'.
    _new 'D update'.

    _old 'A'.
    _old 'B'.
    _old 'C'.
    _old 'D'.

    "         " NEW         " STATUS                        " OLD
    _expected 1 'A'         ''                                1 'A'.
    _expected 2 'B'         ''                                2 'B'.
    _expected 3 'inserted'  zif_abapgit_definitions=>c_diff-insert   '' ''.
    _expected 4 'C'         ''                                3 'C'.
    _expected 5 'D update'  zif_abapgit_definitions=>c_diff-update   4 'D'.

    test( ).

  ENDMETHOD.

ENDCLASS.
