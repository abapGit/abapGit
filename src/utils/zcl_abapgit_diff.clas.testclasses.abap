
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
                             iv_old     TYPE zif_abapgit_definitions=>ty_diff-old
                             iv_beacon  TYPE zif_abapgit_definitions=>ty_diff-beacon
                               DEFAULT zcl_abapgit_diff=>co_starting_beacon.

    METHODS: setup.

    METHODS: test
      IMPORTING
        !iv_ignore_indentation TYPE abap_bool DEFAULT abap_false
        !iv_ignore_comments    TYPE abap_bool DEFAULT abap_false
        !iv_ignore_case        TYPE abap_bool DEFAULT abap_false.

    METHODS:
      diff01 FOR TESTING,
      diff02 FOR TESTING,
      diff03 FOR TESTING,
      diff04 FOR TESTING,
      diff05 FOR TESTING,
      diff06 FOR TESTING,
      diff07 FOR TESTING,
      diff08 FOR TESTING,
      diff09 FOR TESTING,
      diff10 FOR TESTING,
      diff11 FOR TESTING,
      diff12 FOR TESTING,
      diff13 FOR TESTING.

ENDCLASS.


CLASS ltcl_diff IMPLEMENTATION.

  METHOD add_new.
    DATA lv_new LIKE LINE OF mt_new.

    lv_new = iv_new.
    APPEND lv_new TO mt_new.
  ENDMETHOD.

  METHOD add_old.
    DATA lv_old LIKE LINE OF mt_old.

    lv_old = iv_old.
    APPEND lv_old TO mt_old.
  ENDMETHOD.

  METHOD add_expected.
    DATA ls_expected LIKE LINE OF mt_expected.

    ls_expected-new_num = iv_new_num.
    ls_expected-new     = iv_new.
    ls_expected-result  = iv_result.
    ls_expected-old_num = iv_old_num.
    ls_expected-old     = iv_old.
    ls_expected-beacon  = iv_beacon.
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
        iv_new                = lv_xnew
        iv_old                = lv_xold
        iv_ignore_indentation = iv_ignore_indentation
        iv_ignore_comments    = iv_ignore_comments
        iv_ignore_case        = iv_ignore_case.


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

    " mixed
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

  METHOD diff07.

    " ignore indentation
    add_new( iv_new = 'A' ).
    add_new( iv_new = ' B' ). " changed indent
    add_new( iv_new = 'C' ).
    add_new( iv_new = '    D' ). " changed indent

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
                  iv_new     = ' B'
                  iv_result  = '' " no diff!
                  iv_old_num = '    2'
                  iv_old     = 'B' ).
    add_expected( iv_new_num = '    3'
                  iv_new     = 'C'
                  iv_result  = ''
                  iv_old_num = '    3'
                  iv_old     = 'C' ).
    add_expected( iv_new_num = '    4'
                  iv_new     = '    D'
                  iv_result  = '' " no diff!
                  iv_old_num = '    4'
                  iv_old     = 'D' ).

    test( iv_ignore_indentation = abap_true ).

  ENDMETHOD.

  METHOD diff08.

    " ignore comments
    add_new( iv_new = 'A' ).
    add_new( iv_new = '* X' ). " changed comment
    add_new( iv_new = 'C' ).
    add_new( iv_new = 'D " new' ). " changed comment

    add_old( iv_old = 'A' ).
    add_old( iv_old = '* B' ).
    add_old( iv_old = 'C' ).
    add_old( iv_old = 'D " old' ).

    add_expected( iv_new_num = '    1'
                  iv_new     = 'A'
                  iv_result  = ''
                  iv_old_num = '    1'
                  iv_old     = 'A' ).
    add_expected( iv_new_num = '    2'
                  iv_new     = '* X'
                  iv_result  = '' " no diff!
                  iv_old_num = '    2'
                  iv_old     = '* B' ).
    add_expected( iv_new_num = '    3'
                  iv_new     = 'C'
                  iv_result  = ''
                  iv_old_num = '    3'
                  iv_old     = 'C' ).
    add_expected( iv_new_num = '    4'
                  iv_new     = 'D " new'
                  iv_result  = '' " no diff!
                  iv_old_num = '    4'
                  iv_old     = 'D " old' ).

    test( iv_ignore_comments = abap_true ).

  ENDMETHOD.

  METHOD diff09.

    " ignore case
    add_new( iv_new = 'A' ).
    add_new( iv_new = 'b' ). " changed case
    add_new( iv_new = 'c' ).
    add_new( iv_new = 'D' ). " changed case

    add_old( iv_old = 'A' ).
    add_old( iv_old = 'B' ).
    add_old( iv_old = 'c' ).
    add_old( iv_old = 'd' ).

    add_expected( iv_new_num = '    1'
                  iv_new     = 'A'
                  iv_result  = ''
                  iv_old_num = '    1'
                  iv_old     = 'A' ).
    add_expected( iv_new_num = '    2'
                  iv_new     = 'b'
                  iv_result  = '' " no diff!
                  iv_old_num = '    2'
                  iv_old     = 'B' ).
    add_expected( iv_new_num = '    3'
                  iv_new     = 'c'
                  iv_result  = ''
                  iv_old_num = '    3'
                  iv_old     = 'c' ).
    add_expected( iv_new_num = '    4'
                  iv_new     = 'D'
                  iv_result  = '' " no diff!
                  iv_old_num = '    4'
                  iv_old     = 'd' ).

    test( iv_ignore_case = abap_true ).

  ENDMETHOD.

  METHOD diff10.

    " ignore case should NOT ignore changed literals
    add_new( iv_new = `WRITE 'TEST'` ).

    add_old( iv_old = `WRITE 'test'` ).

    add_expected( iv_new_num = '    1'
                  iv_new     = `WRITE 'TEST'`
                  iv_result  = 'U'
                  iv_old_num = '    1'
                  iv_old     = `WRITE 'test'` ).

    test( iv_ignore_case = abap_true ).

  ENDMETHOD.

  METHOD diff11.

    " ignore case should ignore changed keywords, variables, types
    add_new( iv_new = `write 'test'` ).
    add_new( iv_new = `DATA FOO TYPE I.` ).

    add_old( iv_old = `WRITE 'test'` ).
    add_old( iv_old = `DATA foo TYPE i.` ).

    add_expected( iv_new_num = '    1'
                  iv_new     = `write 'test'`
                  iv_result  = '' " no diff!
                  iv_old_num = '    1'
                  iv_old     = `WRITE 'test'` ).
    add_expected( iv_new_num = '    2'
                  iv_new     = `DATA FOO TYPE I.`
                  iv_result  = '' " no diff!
                  iv_old_num = '    2'
                  iv_old     = `DATA foo TYPE i.` ).

    test( iv_ignore_case = abap_true ).

  ENDMETHOD.

  METHOD diff12.

    " adjusted diffs for insert (workaround for kernel issue)
    add_new( iv_new = `REPORT zprog_diff.` ).
    add_new( iv_new = `*` ).
    add_new( iv_new = `FORM t_1.` ).
    add_new( iv_new = `ENDFORM.` ).
    add_new( iv_new = `FORM t_2.` ).
    add_new( iv_new = `ENDFORM.` ).

    add_old( iv_old = `REPORT zprog_diff.` ).
    add_old( iv_old = `FORM t_1.` ).
    add_old( iv_old = `ENDFORM.` ).

    add_expected( iv_new_num = '    1'
                  iv_new     = `REPORT zprog_diff.`
                  iv_result  = '' " no diff!
                  iv_old_num = '    1'
                  iv_old     = `REPORT zprog_diff.`
                  iv_beacon  = 1 ).
    add_expected( iv_new_num = '    2'
                  iv_new     = `*`
                  iv_result  = 'I'
                  iv_old_num = '     '
                  iv_old     = ``
                  iv_beacon  = 1 ).
    add_expected( iv_new_num = '    3'
                  iv_new     = `FORM t_1.`
                  iv_result  = '' " no diff!
                  iv_old_num = '    2'
                  iv_old     = `FORM t_1.`
                  iv_beacon  = 2 ).
    add_expected( iv_new_num = '    4'
                  iv_new     = `ENDFORM.`
                  iv_result  = '' " no diff!
                  iv_old_num = '    3'
                  iv_old     = `ENDFORM.`
                  iv_beacon  = 2 ).
    add_expected( iv_new_num = '    5'
                  iv_new     = `FORM t_2.`
                  iv_result  = 'I'
                  iv_old_num = '     '
                  iv_old     = ``
                  iv_beacon  = 3 ).
    add_expected( iv_new_num = '    6'
                  iv_new     = `ENDFORM.`
                  iv_result  = 'I'
                  iv_old_num = '     '
                  iv_old     = ``
                  iv_beacon  = 3 ).

    test( ).

  ENDMETHOD.

  METHOD diff13.

    " adjusted diffs for delete (workaround for kernel issue)
    add_old( iv_old = `REPORT zprog_diff.` ).
    add_old( iv_old = `*` ).
    add_old( iv_old = `FORM t_1.` ).
    add_old( iv_old = `ENDFORM.` ).
    add_old( iv_old = `FORM t_2.` ).
    add_old( iv_old = `ENDFORM.` ).

    add_new( iv_new = `REPORT zprog_diff.` ).
    add_new( iv_new = `FORM t_1.` ).
    add_new( iv_new = `ENDFORM.` ).

    add_expected( iv_old_num = '    1'
                  iv_old     = `REPORT zprog_diff.`
                  iv_result  = '' " no diff!
                  iv_new_num = '    1'
                  iv_new     = `REPORT zprog_diff.`
                  iv_beacon  = 1 ).
    add_expected( iv_old_num = '    2'
                  iv_old     = `*`
                  iv_result  = 'D'
                  iv_new_num = '     '
                  iv_new     = ``
                  iv_beacon  = 1 ).
    add_expected( iv_old_num = '    3'
                  iv_old     = `FORM t_1.`
                  iv_result  = '' " no diff!
                  iv_new_num = '    2'
                  iv_new     = `FORM t_1.`
                  iv_beacon  = 2 ).
    add_expected( iv_old_num = '    4'
                  iv_old     = `ENDFORM.`
                  iv_result  = '' " no diff!
                  iv_new_num = '    3'
                  iv_new     = `ENDFORM.`
                  iv_beacon  = 2 ).
    add_expected( iv_old_num = '    5'
                  iv_old     = `FORM t_2.`
                  iv_result  = 'D'
                  iv_new_num = '     '
                  iv_new     = ``
                  iv_beacon  = 2 ).
    add_expected( iv_old_num = '    6'
                  iv_old     = `ENDFORM.`
                  iv_result  = 'D'
                  iv_new_num = '     '
                  iv_new     = ``
                  iv_beacon  = 2 ).

    test( ).

  ENDMETHOD.

ENDCLASS.
