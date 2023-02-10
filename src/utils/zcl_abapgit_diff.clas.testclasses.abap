CLASS ltcl_diff DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: mt_new      TYPE TABLE OF string,
          mt_old      TYPE TABLE OF string,
          mt_beacons  TYPE zif_abapgit_definitions=>ty_string_tt,
          mt_expected TYPE zif_abapgit_definitions=>ty_diffs_tt.

    METHODS:
      add_new IMPORTING iv_new TYPE string,
      add_old IMPORTING iv_old TYPE string,
      add_beacon   IMPORTING iv_beacon TYPE string,
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
        !iv_ignore_case        TYPE abap_bool DEFAULT abap_false
        !iv_check_beacons      TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception.

    METHODS:
      diff01 FOR TESTING RAISING zcx_abapgit_exception,
      diff02 FOR TESTING RAISING zcx_abapgit_exception,
      diff03 FOR TESTING RAISING zcx_abapgit_exception,
      diff04 FOR TESTING RAISING zcx_abapgit_exception,
      diff05 FOR TESTING RAISING zcx_abapgit_exception,
      diff06 FOR TESTING RAISING zcx_abapgit_exception,
      diff07 FOR TESTING RAISING zcx_abapgit_exception,
      diff08 FOR TESTING RAISING zcx_abapgit_exception,
      diff09 FOR TESTING RAISING zcx_abapgit_exception,
      diff10 FOR TESTING RAISING zcx_abapgit_exception,
      diff11 FOR TESTING RAISING zcx_abapgit_exception,
      diff12 FOR TESTING RAISING zcx_abapgit_exception,
      diff13 FOR TESTING RAISING zcx_abapgit_exception,
      diff14 FOR TESTING RAISING zcx_abapgit_exception,
      diff15 FOR TESTING RAISING zcx_abapgit_exception,
      diff16 FOR TESTING RAISING zcx_abapgit_exception,
      map_beacons FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_diff IMPLEMENTATION.

  METHOD add_new.
    APPEND iv_new TO mt_new.
  ENDMETHOD.

  METHOD add_old.
    APPEND iv_old TO mt_old.
  ENDMETHOD.

  METHOD add_beacon.
    APPEND iv_beacon TO mt_beacons.
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
    CLEAR mt_beacons.
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


    CONCATENATE LINES OF mt_new INTO lv_new SEPARATED BY cl_abap_char_utilities=>newline.
    CONCATENATE LINES OF mt_old INTO lv_old SEPARATED BY cl_abap_char_utilities=>newline.

    lv_xnew = zcl_abapgit_convert=>string_to_xstring_utf8( lv_new ).
    lv_xold = zcl_abapgit_convert=>string_to_xstring_utf8( lv_old ).

    CREATE OBJECT lo_diff
      EXPORTING
        iv_new                = lv_xnew
        iv_old                = lv_xold
        iv_ignore_indentation = iv_ignore_indentation
        iv_ignore_comments    = iv_ignore_comments
        iv_ignore_case        = iv_ignore_case.

    IF iv_check_beacons = abap_true.
      cl_abap_unit_assert=>assert_equals(
        act = lo_diff->get_beacons( )
        exp = mt_beacons ).
      RETURN.
    ENDIF.

    lt_diff = lo_diff->get( ).

    LOOP AT lt_diff ASSIGNING <ls_diff>.
      CLEAR <ls_diff>-short.
    ENDLOOP.

    cl_abap_unit_assert=>assert_equals( act = lt_diff
                                        exp = mt_expected ).

  ENDMETHOD.

  METHOD diff01.

    "insert
    add_new( 'A' ).

    add_expected( iv_new_num = '    1'
                  iv_new     = 'A'
                  iv_result  = zif_abapgit_definitions=>c_diff-insert
                  iv_old_num = ''
                  iv_old     = '' ).
    test( ).

  ENDMETHOD.

  METHOD diff02.

    " identical
    add_new( 'A' ).
    add_old( 'A' ).

    add_expected( iv_new_num = '    1'
                  iv_new     = 'A'
                  iv_result  = zif_abapgit_definitions=>c_diff-unchanged
                  iv_old_num = '    1'
                  iv_old     = 'A' ).
    test( ).

  ENDMETHOD.

  METHOD diff03.

    " delete
    add_old( 'A' ).

    add_expected( iv_new_num = ''
                  iv_new     = ''
                  iv_result  = zif_abapgit_definitions=>c_diff-delete
                  iv_old_num = '    1'
                  iv_old     = 'A' ).
    test( ).

  ENDMETHOD.

  METHOD diff04.

    " update
    add_new( 'A+' ).

    add_old( 'A' ).

    add_expected( iv_new_num = '    1'
                  iv_new     = 'A+'
                  iv_result  = zif_abapgit_definitions=>c_diff-update
                  iv_old_num = '    1'
                  iv_old     = 'A' ).
    test( ).

  ENDMETHOD.

  METHOD diff05.

    " identical
    add_new( 'A' ).
    add_new( 'B' ).

    add_old( 'A' ).
    add_old( 'B' ).

    add_expected( iv_new_num = '    1'
                  iv_new     = 'A'
                  iv_result  = zif_abapgit_definitions=>c_diff-unchanged
                  iv_old_num = '    1'
                  iv_old     = 'A' ).
    add_expected( iv_new_num = '    2'
                  iv_new     = 'B'
                  iv_result  = zif_abapgit_definitions=>c_diff-unchanged
                  iv_old_num = '    2'
                  iv_old     = 'B' ).
    test( ).

  ENDMETHOD.

  METHOD diff06.

    " mixed
    add_new( 'A' ).
    add_new( 'B' ).
    add_new( 'inserted' ).
    add_new( 'C' ).
    add_new( 'D update' ).

    add_old( 'A' ).
    add_old( 'B' ).
    add_old( 'C' ).
    add_old( 'D' ).

    add_expected( iv_new_num = '    1'
                  iv_new     = 'A'
                  iv_result  = zif_abapgit_definitions=>c_diff-unchanged
                  iv_old_num = '    1'
                  iv_old     = 'A' ).
    add_expected( iv_new_num = '    2'
                  iv_new     = 'B'
                  iv_result  = zif_abapgit_definitions=>c_diff-unchanged
                  iv_old_num = '    2'
                  iv_old     = 'B' ).
    add_expected( iv_new_num = '    3'
                  iv_new     = 'inserted'
                  iv_result  = zif_abapgit_definitions=>c_diff-insert
                  iv_old_num = ''
                  iv_old     = '' ).
    add_expected( iv_new_num = '    4'
                  iv_new     = 'C'
                  iv_result  = zif_abapgit_definitions=>c_diff-unchanged
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
    add_new( 'A' ).
    add_new( ' B' ). " changed indent
    add_new( 'C' ).
    add_new( '    D' ). " changed indent

    add_old( 'A' ).
    add_old( 'B' ).
    add_old( 'C' ).
    add_old( 'D' ).

    add_expected( iv_new_num = '    1'
                  iv_new     = 'A'
                  iv_result  = zif_abapgit_definitions=>c_diff-unchanged
                  iv_old_num = '    1'
                  iv_old     = 'A' ).
    add_expected( iv_new_num = '    2'
                  iv_new     = ' B'
                  iv_result  = zif_abapgit_definitions=>c_diff-unchanged
                  iv_old_num = '    2'
                  iv_old     = 'B' ).
    add_expected( iv_new_num = '    3'
                  iv_new     = 'C'
                  iv_result  = zif_abapgit_definitions=>c_diff-unchanged
                  iv_old_num = '    3'
                  iv_old     = 'C' ).
    add_expected( iv_new_num = '    4'
                  iv_new     = '    D'
                  iv_result  = zif_abapgit_definitions=>c_diff-unchanged
                  iv_old_num = '    4'
                  iv_old     = 'D' ).

    test( iv_ignore_indentation = abap_true ).

  ENDMETHOD.

  METHOD diff08.

    " ignore comments
    add_new( 'A' ).
    add_new( '* X' ). " changed comment
    add_new( 'C' ).
    add_new( 'D " new' ). " changed comment

    add_old( 'A' ).
    add_old( '* B' ).
    add_old( 'C' ).
    add_old( 'D " old' ).

    add_expected( iv_new_num = '    1'
                  iv_new     = 'A'
                  iv_result  = zif_abapgit_definitions=>c_diff-unchanged
                  iv_old_num = '    1'
                  iv_old     = 'A' ).
    add_expected( iv_new_num = '    2'
                  iv_new     = '* X'
                  iv_result  = zif_abapgit_definitions=>c_diff-unchanged
                  iv_old_num = '    2'
                  iv_old     = '* B' ).
    add_expected( iv_new_num = '    3'
                  iv_new     = 'C'
                  iv_result  = zif_abapgit_definitions=>c_diff-unchanged
                  iv_old_num = '    3'
                  iv_old     = 'C' ).
    add_expected( iv_new_num = '    4'
                  iv_new     = 'D " new'
                  iv_result  = zif_abapgit_definitions=>c_diff-unchanged
                  iv_old_num = '    4'
                  iv_old     = 'D " old' ).

    test( iv_ignore_comments = abap_true ).

  ENDMETHOD.

  METHOD diff09.

    " ignore case
    add_new( 'A' ).
    add_new( 'b' ). " changed case
    add_new( 'c' ).
    add_new( 'D' ). " changed case

    add_old( 'A' ).
    add_old( 'B' ).
    add_old( 'c' ).
    add_old( 'd' ).

    add_expected( iv_new_num = '    1'
                  iv_new     = 'A'
                  iv_result  = zif_abapgit_definitions=>c_diff-unchanged
                  iv_old_num = '    1'
                  iv_old     = 'A' ).
    add_expected( iv_new_num = '    2'
                  iv_new     = 'b'
                  iv_result  = zif_abapgit_definitions=>c_diff-unchanged
                  iv_old_num = '    2'
                  iv_old     = 'B' ).
    add_expected( iv_new_num = '    3'
                  iv_new     = 'c'
                  iv_result  = zif_abapgit_definitions=>c_diff-unchanged
                  iv_old_num = '    3'
                  iv_old     = 'c' ).
    add_expected( iv_new_num = '    4'
                  iv_new     = 'D'
                  iv_result  = zif_abapgit_definitions=>c_diff-unchanged
                  iv_old_num = '    4'
                  iv_old     = 'd' ).

    test( iv_ignore_case = abap_true ).

  ENDMETHOD.

  METHOD diff10.

    " ignore case should NOT ignore changed literals
    add_new( `WRITE 'TEST'` ).

    add_old( `WRITE 'test'` ).

    add_expected( iv_new_num = '    1'
                  iv_new     = `WRITE 'TEST'`
                  iv_result  = zif_abapgit_definitions=>c_diff-update
                  iv_old_num = '    1'
                  iv_old     = `WRITE 'test'` ).

    test( iv_ignore_case = abap_true ).

  ENDMETHOD.

  METHOD diff11.

    " ignore case should ignore changed keywords, variables, types
    add_new( `write 'test'` ).
    add_new( `DATA FOO TYPE I.` ).

    add_old( `WRITE 'test'` ).
    add_old( `DATA foo TYPE i.` ).

    add_expected( iv_new_num = '    1'
                  iv_new     = `write 'test'`
                  iv_result  = zif_abapgit_definitions=>c_diff-unchanged
                  iv_old_num = '    1'
                  iv_old     = `WRITE 'test'` ).
    add_expected( iv_new_num = '    2'
                  iv_new     = `DATA FOO TYPE I.`
                  iv_result  = zif_abapgit_definitions=>c_diff-unchanged
                  iv_old_num = '    2'
                  iv_old     = `DATA foo TYPE i.` ).

    test( iv_ignore_case = abap_true ).

  ENDMETHOD.

  METHOD diff12.

    " adjusted diffs for insert (workaround for kernel issue)
    add_new( `REPORT zprog_diff.` ).
    add_new( `*` ).
    add_new( `FORM t_1.` ).
    add_new( `ENDFORM.` ).
    add_new( `FORM t_2.` ).
    add_new( `ENDFORM.` ).

    add_old( `REPORT zprog_diff.` ).
    add_old( `FORM t_1.` ).
    add_old( `ENDFORM.` ).

    add_expected( iv_new_num = '    1'
                  iv_new     = `REPORT zprog_diff.`
                  iv_result  = zif_abapgit_definitions=>c_diff-unchanged
                  iv_old_num = '    1'
                  iv_old     = `REPORT zprog_diff.`
                  iv_beacon  = 1 ).
    add_expected( iv_new_num = '    2'
                  iv_new     = `*`
                  iv_result  = zif_abapgit_definitions=>c_diff-insert
                  iv_old_num = '     '
                  iv_old     = ``
                  iv_beacon  = 1 ).
    add_expected( iv_new_num = '    3'
                  iv_new     = `FORM t_1.`
                  iv_result  = zif_abapgit_definitions=>c_diff-unchanged
                  iv_old_num = '    2'
                  iv_old     = `FORM t_1.`
                  iv_beacon  = 2 ).
    add_expected( iv_new_num = '    4'
                  iv_new     = `ENDFORM.`
                  iv_result  = zif_abapgit_definitions=>c_diff-unchanged
                  iv_old_num = '    3'
                  iv_old     = `ENDFORM.`
                  iv_beacon  = 2 ).
    add_expected( iv_new_num = '    5'
                  iv_new     = `FORM t_2.`
                  iv_result  = zif_abapgit_definitions=>c_diff-insert
                  iv_old_num = '     '
                  iv_old     = ``
                  iv_beacon  = 3 ).
    add_expected( iv_new_num = '    6'
                  iv_new     = `ENDFORM.`
                  iv_result  = zif_abapgit_definitions=>c_diff-insert
                  iv_old_num = '     '
                  iv_old     = ``
                  iv_beacon  = 3 ).

    test( ).

  ENDMETHOD.

  METHOD diff13.

    " adjusted diffs for delete (workaround for kernel issue)
    add_old( `REPORT zprog_diff.` ).
    add_old( `*` ).
    add_old( `FORM t_1.` ).
    add_old( `ENDFORM.` ).
    add_old( `FORM t_2.` ).
    add_old( `ENDFORM.` ).

    add_new( `REPORT zprog_diff.` ).
    add_new( `FORM t_1.` ).
    add_new( `ENDFORM.` ).

    add_expected( iv_old_num = '    1'
                  iv_old     = `REPORT zprog_diff.`
                  iv_result  = zif_abapgit_definitions=>c_diff-unchanged
                  iv_new_num = '    1'
                  iv_new     = `REPORT zprog_diff.`
                  iv_beacon  = 1 ).
    add_expected( iv_old_num = '    2'
                  iv_old     = `*`
                  iv_result  = zif_abapgit_definitions=>c_diff-delete
                  iv_new_num = '     '
                  iv_new     = ``
                  iv_beacon  = 1 ).
    add_expected( iv_old_num = '    3'
                  iv_old     = `FORM t_1.`
                  iv_result  = zif_abapgit_definitions=>c_diff-unchanged
                  iv_new_num = '    2'
                  iv_new     = `FORM t_1.`
                  iv_beacon  = 2 ).
    add_expected( iv_old_num = '    4'
                  iv_old     = `ENDFORM.`
                  iv_result  = zif_abapgit_definitions=>c_diff-unchanged
                  iv_new_num = '    3'
                  iv_new     = `ENDFORM.`
                  iv_beacon  = 2 ).
    add_expected( iv_old_num = '    5'
                  iv_old     = `FORM t_2.`
                  iv_result  = zif_abapgit_definitions=>c_diff-delete
                  iv_new_num = '     '
                  iv_new     = ``
                  iv_beacon  = 2 ).
    add_expected( iv_old_num = '    6'
                  iv_old     = `ENDFORM.`
                  iv_result  = zif_abapgit_definitions=>c_diff-delete
                  iv_new_num = '     '
                  iv_new     = ``
                  iv_beacon  = 2 ).

    test( ).

  ENDMETHOD.

  METHOD diff14.

    " lines with different whitespace
    add_new( 'A' ).
    add_new( `` ). " empty line
    add_new( ` ` ). " one space
    add_new( `   ` ). " some spaces
    add_new( 'E' ).

    add_old( 'A' ).
    add_old( `     ` ). " some spaces
    add_old( `  ` ). " two spaces
    add_old( `` ). " empty line
    add_old( 'E' ).

    add_expected( iv_new_num = '    1'
                  iv_new     = 'A'
                  iv_result  = zif_abapgit_definitions=>c_diff-unchanged
                  iv_old_num = '    1'
                  iv_old     = 'A' ).
    add_expected( iv_new_num = '    2'
                  iv_new     = ''
                  iv_result  = zif_abapgit_definitions=>c_diff-update
                  iv_old_num = '    2'
                  iv_old     = `     ` ).
    add_expected( iv_new_num = '    3'
                  iv_new     = ` `
                  iv_result  = zif_abapgit_definitions=>c_diff-update
                  iv_old_num = '    3'
                  iv_old     = `  ` ).
    add_expected( iv_new_num = '    4'
                  iv_new     = `   `
                  iv_result  = zif_abapgit_definitions=>c_diff-update
                  iv_old_num = '    4'
                  iv_old     = '' ).
    add_expected( iv_new_num = '    5'
                  iv_new     = 'E'
                  iv_result  = zif_abapgit_definitions=>c_diff-unchanged
                  iv_old_num = '    5'
                  iv_old     = 'E' ).

    test( ).

  ENDMETHOD.


  METHOD diff15.

    " ignore comments - edge case new comment
    add_new( `*/` ).

    add_old( '' ).

    add_expected( iv_new_num = '    1'
                  iv_new     = '*/'
                  iv_result  = zif_abapgit_definitions=>c_diff-unchanged
                  iv_old_num = '     '
                  iv_old     = '' ).

    test( iv_ignore_comments = abap_true ).

  ENDMETHOD.


  METHOD diff16.

    " ignore comments - edge case deleted comment
    add_new( `` ).

    add_old( `* " problem` ).

    add_expected( iv_new_num = '     '
                  iv_new     = ''
                  iv_result  = zif_abapgit_definitions=>c_diff-unchanged
                  iv_old_num = '    1'
                  iv_old     = `* " problem` ).

    test( iv_ignore_comments = abap_true ).

  ENDMETHOD.


  METHOD map_beacons.

    add_new( `REPORT ztest_beacon.` ).
    add_new( `` ).
    add_new( `DATA report TYPE string.` ).
    add_new( `report = 'TEST'.` ).
    add_new( `` ).
    add_new( `CLASS lcl_test DEFINITION.` ).
    add_new( `  PUBLIC SECTION.` ).
    add_new( `    CLASS-METHODS test.` ).
    add_new( `ENDCLASS.` ).
    add_new( `` ).
    add_new( `CLASS lcl_test IMPLEMENTATION.` ).
    add_new( `  METHOD test.` ).
    add_new( `    DATA method TYPE i.` ).
    add_new( `    method = 10.` ).
    add_new( `  ENDMETHOD.` ).
    add_new( `ENDCLASS.` ).
    add_new( `` ).
    add_new( `TYPE-POOLS abap.` ).
    add_new( `` ).
    add_new( `START-OF-SELECTION.` ).
    add_new( `  CALL METHOD lcl_test=>test.` ).
    add_new( `` ).
    add_new( `AT SELECTION-SCREEN.` ).
    add_new( `  BREAK-POINT.` ).

    add_old( '' ).

    add_beacon( `REPORT ztest_beacon` ).
    add_beacon( `CLASS lcl_test DEFINITION` ).
    add_beacon( `CLASS lcl_test PUBLIC SECTION` ).
    add_beacon( `CLASS lcl_test IMPLEMENTATION` ).
    add_beacon( `CLASS lcl_test => METHOD test` ).
    add_beacon( `START-OF-SELECTION` ).
    add_beacon( `AT SELECTION-SCREEN` ).

    test( iv_check_beacons = abap_true ).

  ENDMETHOD.
ENDCLASS.
