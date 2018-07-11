*"* use this source file for your ABAP unit test classes
CLASS ltcl_is_ignored DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      m_path           TYPE string,
      m_filename       TYPE string,
      mt_gitignore     TYPE stringtab,
      m_act_is_ignored TYPE abap_bool.

    METHODS:
      single_file_positive
        FOR TESTING RAISING cx_static_check,

      single_file_negative
        FOR TESTING RAISING cx_static_check,

      single_file_sub_positive
        FOR TESTING RAISING cx_static_check,

      single_file_sub_negative
        FOR TESTING RAISING cx_static_check,

      star_at_the_end_positive
        FOR TESTING RAISING cx_static_check,

      star_at_the_end_positive2
        FOR TESTING RAISING cx_static_check,

      star_at_the_end_negative
        FOR TESTING RAISING cx_static_check,

      star_at_the_end_sub_positive
        FOR TESTING RAISING cx_static_check,

      star_at_the_end_sub_negative
        FOR TESTING RAISING cx_static_check,

      repository_root_positive
        FOR TESTING RAISING cx_static_check,

      repository_root_negative
        FOR TESTING RAISING cx_static_check,

      repository_root_negative2
        FOR TESTING RAISING cx_static_check,

      wildcard_positive
        FOR TESTING RAISING cx_static_check,

      wildcard_negative
        FOR TESTING RAISING cx_static_check,

      given_file
        IMPORTING
          iv_path     TYPE string
          iv_filename TYPE string,

      given_gitignore
        IMPORTING
          iv_gitignore TYPE string,

      when_is_ignored_checked,

      then_file_is_ignored
        IMPORTING
          iv_exp_is_ignored TYPE abap_bool.

ENDCLASS.


CLASS ltcl_is_ignored IMPLEMENTATION.

  METHOD single_file_positive.

    given_file( iv_path     = |/|
                iv_filename = |ZTEST.ABAP| ).
    given_gitignore( |ZTEST.ABAP| ).
    when_is_ignored_checked( ).
    then_file_is_ignored( abap_true ).

  ENDMETHOD.


  METHOD single_file_negative.

    given_file( iv_path     = |/|
                iv_filename = |ZTEST2.ABAP| ).
    given_gitignore( |ZTEST.ABAP| ).
    when_is_ignored_checked( ).
    then_file_is_ignored( abap_false ).

  ENDMETHOD.


  METHOD single_file_sub_positive.

    given_file( iv_path     = |/sub/|
                iv_filename = |ZTEST.xml| ).
    given_gitignore( |ZTEST.xml| ).
    when_is_ignored_checked( ).
    then_file_is_ignored( abap_true ).

  ENDMETHOD.


  METHOD single_file_sub_negative.

    given_file( iv_path     = |/sub/|
                iv_filename = |ZTEST.xml| ).
    given_gitignore( |ZZTEST.xml| ).
    when_is_ignored_checked( ).
    then_file_is_ignored( abap_false ).

  ENDMETHOD.


  METHOD star_at_the_end_positive.

    given_file( iv_path     = |/|
                iv_filename = |ZTEST.ABAP| ).
    given_gitignore( |ZTEST*| ).
    when_is_ignored_checked( ).
    then_file_is_ignored( abap_true ).

  ENDMETHOD.

  METHOD star_at_the_end_positive2.

    given_file( iv_path     = |/|
                iv_filename = |ZZTEST.ABAP| ).
    given_gitignore( |ZTEST*| ).
    when_is_ignored_checked( ).
    then_file_is_ignored( abap_true ).

  ENDMETHOD.


  METHOD star_at_the_end_negative.

    given_file( iv_path     = |/|
                iv_filename = |ZZTEST.xml| ).
    given_gitignore( |ZTEST2*| ).
    when_is_ignored_checked( ).
    then_file_is_ignored( abap_false ).

  ENDMETHOD.


  METHOD star_at_the_end_sub_positive.

    given_file( iv_path     = |/sub/|
                iv_filename = |ZTEST.ABAP| ).
    given_gitignore( |ZTEST*| ).
    when_is_ignored_checked( ).
    then_file_is_ignored( abap_true ).

  ENDMETHOD.


  METHOD star_at_the_end_sub_negative.

    given_file( iv_path     = |/sub/|
                iv_filename = |ZTEST.ABAP| ).
    given_gitignore( |ZZTEST*| ).
    when_is_ignored_checked( ).
    then_file_is_ignored( abap_false ).

  ENDMETHOD.


  METHOD repository_root_positive.

    given_file( iv_path     = |/|
                iv_filename = |ZTEST.ABAP| ).
    given_gitignore( |/ZTEST*| ).
    when_is_ignored_checked( ).
    then_file_is_ignored( abap_true ).

  ENDMETHOD.


  METHOD repository_root_negative.

    given_file( iv_path     = |/|
                iv_filename = |Z_TEST.ABAP| ).
    given_gitignore( |/ZTEST*| ).
    when_is_ignored_checked( ).
    then_file_is_ignored( abap_false ).

  ENDMETHOD.


  METHOD repository_root_negative2.

    given_file( iv_path     = |/sub/|
                iv_filename = |ZTEST2.ABAP| ).
    given_gitignore( |/ZTEST*| ).
    when_is_ignored_checked( ).
    then_file_is_ignored( abap_false ).

  ENDMETHOD.


  METHOD wildcard_positive.

    given_file( iv_path     = |/|
                iv_filename = |ZTEST.ABAP| ).
    given_gitignore( |*.ABAP| ).
    when_is_ignored_checked( ).
    then_file_is_ignored( abap_true ).

  ENDMETHOD.


  METHOD wildcard_negative.

    given_file( iv_path     = |/|
                iv_filename = |ZTEST.xml| ).
    given_gitignore( |*.ABAP| ).
    when_is_ignored_checked( ).
    then_file_is_ignored( abap_false ).

  ENDMETHOD.


  METHOD given_file.

    m_path = iv_path.
    m_filename = iv_filename.

  ENDMETHOD.


  METHOD given_gitignore.

    INSERT iv_gitignore INTO TABLE mt_gitignore.

  ENDMETHOD.


  METHOD when_is_ignored_checked.

    DATA: lo_dot_gitignore TYPE REF TO zcl_abapgit_dot_gitignore.

    CREATE OBJECT lo_dot_gitignore
      EXPORTING
        it_gitignore = mt_gitignore.

    m_act_is_ignored = lo_dot_gitignore->is_ignored(
                           iv_path     = m_path
                           iv_filename = m_filename ).

  ENDMETHOD.


  METHOD then_file_is_ignored.

    cl_abap_unit_assert=>assert_equals(
      exp = iv_exp_is_ignored
      act = m_act_is_ignored ).

  ENDMETHOD.

ENDCLASS.
