CLASS ltcl_test_checksums DEFINITION FINAL
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PUBLIC SECTION.
    METHODS serialize FOR TESTING.
    METHODS deserialize FOR TESTING.

    METHODS get_mock
      EXPORTING
        et_checksums TYPE zif_abapgit_persistence=>ty_local_checksum_tt
        ev_str       TYPE string.
ENDCLASS.

CLASS ltcl_test_checksums IMPLEMENTATION.

  METHOD get_mock.

    FIELD-SYMBOLS <ls_cs> LIKE LINE OF et_checksums.
    FIELD-SYMBOLS <ls_file> LIKE LINE OF <ls_cs>-files.

    CLEAR et_checksums.

    APPEND INITIAL LINE TO et_checksums ASSIGNING <ls_cs>.
    <ls_cs>-item-devclass = '$PKG'.
    <ls_cs>-item-obj_type = 'PROG'.
    <ls_cs>-item-obj_name = 'ZHELLO'.
    <ls_cs>-item-inactive = 'X'. " should not affect
    APPEND INITIAL LINE TO <ls_cs>-files ASSIGNING <ls_file>.
    <ls_file>-path     = '/'.
    <ls_file>-filename = 'zhello.prog.abap'.
    <ls_file>-sha1     = 'hash1'.
    APPEND INITIAL LINE TO <ls_cs>-files ASSIGNING <ls_file>.
    <ls_file>-path     = '/'.
    <ls_file>-filename = 'zhello.prog.xml'.
    <ls_file>-sha1     = 'hash2'.

    APPEND INITIAL LINE TO et_checksums ASSIGNING <ls_cs>.
    <ls_cs>-item-devclass = '$PKG'.
    <ls_cs>-item-obj_type = 'DEVC'.
    <ls_cs>-item-obj_name = '$PKG'.
    APPEND INITIAL LINE TO <ls_cs>-files ASSIGNING <ls_file>.
    <ls_file>-path     = '/'.
    <ls_file>-filename = '$pkg.devc.xml'.
    <ls_file>-sha1     = 'hash3'.

    ev_str =
      |PROG ZHELLO $PKG\n| &&
      |/ zhello.prog.abap hash1\n| &&
      |/ zhello.prog.xml hash2\n| &&
      |DEVC $PKG $PKG\n| &&
      |/ $pkg.devc.xml hash3|.

    ev_str = replace( val = ev_str sub = ` ` with = `|` occ = 0 ).
    " This way it's easier to read and adjust ¯\_(ツ)_/¯

  ENDMETHOD.

  METHOD serialize.

    DATA lt_checksums TYPE zif_abapgit_persistence=>ty_local_checksum_tt.
    DATA lo_cs TYPE REF TO zcl_abapgit_repo_checksums.
    DATA lv_act TYPE string.
    DATA lv_exp TYPE string.

    get_mock(
      IMPORTING
        et_checksums = lt_checksums
        ev_str       = lv_exp ).

    CREATE OBJECT lo_cs.
    lv_act = lo_cs->serialize( lt_checksums ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  ENDMETHOD.

  METHOD deserialize.

    DATA lt_checksums_exp TYPE zif_abapgit_persistence=>ty_local_checksum_tt.
    DATA lt_checksums_act TYPE zif_abapgit_persistence=>ty_local_checksum_tt.
    DATA lo_cs TYPE REF TO zcl_abapgit_repo_checksums.
    DATA lv_str TYPE string.

    FIELD-SYMBOLS <ls_cs> LIKE LINE OF lt_checksums_exp.

    get_mock(
      IMPORTING
        et_checksums = lt_checksums_exp
        ev_str       = lv_str ).

    LOOP AT lt_checksums_exp ASSIGNING <ls_cs>.
      CLEAR <ls_cs>-item-inactive.
    ENDLOOP.

    CREATE OBJECT lo_cs.
    lt_checksums_act = lo_cs->deserialize( lv_str ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_checksums_act
      exp = lt_checksums_exp ).

  ENDMETHOD.

ENDCLASS.
