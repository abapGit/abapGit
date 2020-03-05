
CLASS ltcl_diff DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: mt_new      TYPE TABLE OF string,
          mt_old      TYPE TABLE OF string,
          mt_expected TYPE zif_abapgit_definitions=>ty_diffs_tt.

    METHODS:
      add_new IMPORTING iv_new TYPE string,
      add_old IMPORTING iv_old TYPE string,
      add_expected IMPORTING iv_new_num TYPE zif_abapgit_definitions=>ty_diff-new_num
                             iv_new     TYPE zif_abapgit_definitions=>ty_diff-new
                             iv_result  TYPE zif_abapgit_definitions=>ty_diff-result
                             iv_old_num TYPE zif_abapgit_definitions=>ty_diff-old_num
                             iv_old     TYPE zif_abapgit_definitions=>ty_diff-old.

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

  METHOD add_new.
    DATA ls_new LIKE LINE OF mt_new.

    ls_new = iv_new.
    APPEND ls_new TO mt_new.
  ENDMETHOD.

  METHOD add_old.
    DATA ls_old LIKE LINE OF mt_old.

    ls_old = iv_old.
    APPEND ls_old TO mt_old.
  ENDMETHOD.

  METHOD add_expected.
    DATA ls_expected LIKE LINE OF mt_expected.

    ls_expected-new_num = iv_new_num.
    ls_expected-new     = iv_new.
    ls_expected-result  = iv_result.
    ls_expected-old_num = iv_old_num.
    ls_expected-old     = iv_old.
    ls_expected-beacon  = zcl_abapgit_diff=>co_starting_beacon.
    APPEND ls_expected TO mt_expected.
  ENDMETHOD.

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


    CONCATENATE LINES OF mt_new INTO lv_new SEPARATED BY zif_abapgit_definitions=>c_newline.
    CONCATENATE LINES OF mt_old INTO lv_old SEPARATED BY zif_abapgit_definitions=>c_newline.

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

    "insert
    add_new( iv_new = 'A' ).

    add_expected( iv_new_num = '    1'
                  iv_new     = 'A'
                  iv_result  = zif_abapgit_definitions=>c_diff-insert
                  iv_old_num = ''
                  iv_old     = '' ).
    test( ).

  ENDMETHOD.

  METHOD diff02.

    " identical
    add_new( iv_new = 'A' ).
    add_old( iv_old = 'A' ).

    add_expected( iv_new_num = '    1'
                  iv_new     = 'A'
                  iv_result  = ''
                  iv_old_num = '    1'
                  iv_old     = 'A' ).
    test( ).

  ENDMETHOD.

  METHOD diff03.

    " delete
    add_old( iv_old = 'A' ).

    add_expected( iv_new_num = ''
                  iv_new     = ''
                  iv_result  = zif_abapgit_definitions=>c_diff-delete
                  iv_old_num = '    1'
                  iv_old     = 'A' ).
    test( ).

  ENDMETHOD.

  METHOD diff04.

    " update
    add_new( iv_new = 'A+' ).

    add_old( iv_old = 'A' ).

    add_expected( iv_new_num = '    1'
                  iv_new     = 'A+'
                  iv_result  = zif_abapgit_definitions=>c_diff-update
                  iv_old_num = '    1'
                  iv_old     = 'A' ).
    test( ).

  ENDMETHOD.

  METHOD diff05.

    " identical
    add_new( iv_new = 'A' ).
    add_new( iv_new = 'B' ).

    add_old( iv_old = 'A' ).
    add_old( iv_old = 'B' ).

    add_expected( iv_new_num = '    1'
                  iv_new     = 'A'
                  iv_result  = ''
                  iv_old_num = '    1'
                  iv_old     = 'A' ).
    add_expected( iv_new_num = '    2'
                  iv_new     = 'B'
                  iv_result  = ''
                  iv_old_num = '    2'
                  iv_old     = 'B' ).
    test( ).

  ENDMETHOD.

  METHOD diff06.


    add_new( iv_new = 'A' ).
    add_new( iv_new = 'B' ).
    add_new( iv_new = 'inserted' ).
    add_new( iv_new = 'C' ).
    add_new( iv_new = 'D update' ).

    add_old( iv_old = 'A' ).
    add_old( iv_old = 'B' ).
    add_old( iv_old = 'C' ).
    add_old( iv_old = 'D' ).

    add_expected( iv_new_num = '    1'
                  iv_new     = 'A'
                  iv_result  = ''
                  iv_old_num = '    1'
                  iv_old     = 'A' ).
    add_expected( iv_new_num = '    2'
                  iv_new     = 'B'
                  iv_result  = ''
                  iv_old_num = '    2'
                  iv_old     = 'B' ).
    add_expected( iv_new_num = '    3'
                  iv_new     = 'inserted'
                  iv_result  = zif_abapgit_definitions=>c_diff-insert
                  iv_old_num = ''
                  iv_old     = '' ).
    add_expected( iv_new_num = '    4'
                  iv_new     = 'C'
                  iv_result  = ''
                  iv_old_num = '    3'
                  iv_old     = 'C' ).
    add_expected( iv_new_num = '    5'
                  iv_new     = 'D update'
                  iv_result  = zif_abapgit_definitions=>c_diff-update
                  iv_old_num = '    4'
                  iv_old     = 'D' ).

    test( ).

  ENDMETHOD.

ENDCLASS.
