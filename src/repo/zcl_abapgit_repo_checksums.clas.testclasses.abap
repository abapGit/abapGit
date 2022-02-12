**********************************************************************
* SERIALIZER
**********************************************************************

CLASS ltcl_test_checksum_serializer DEFINITION FINAL
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PUBLIC SECTION.
    METHODS serialize FOR TESTING.
    METHODS deserialize FOR TESTING.

    CLASS-METHODS get_mock
      EXPORTING
        et_checksums TYPE zif_abapgit_persistence=>ty_local_checksum_tt
        ev_str       TYPE string.
    CLASS-METHODS space_to_separator
      IMPORTING
        iv_str TYPE string
      RETURNING
        VALUE(rv_str) TYPE string.
ENDCLASS.

CLASS ltcl_test_checksum_serializer IMPLEMENTATION.

  METHOD get_mock.

    FIELD-SYMBOLS <ls_cs> LIKE LINE OF et_checksums.
    FIELD-SYMBOLS <ls_file> LIKE LINE OF <ls_cs>-files.

    CLEAR et_checksums.

    APPEND INITIAL LINE TO et_checksums ASSIGNING <ls_cs>.
    <ls_cs>-item-devclass = '$PKG'.
    <ls_cs>-item-obj_type = 'DEVC'.
    <ls_cs>-item-obj_name = '$PKG'.
    APPEND INITIAL LINE TO <ls_cs>-files ASSIGNING <ls_file>.
    <ls_file>-path     = '/'.
    <ls_file>-filename = '$pkg.devc.xml'.
    <ls_file>-sha1     = 'hash3'.

    APPEND INITIAL LINE TO et_checksums ASSIGNING <ls_cs>.
    <ls_cs>-item-devclass = '$PKG'.
    <ls_cs>-item-obj_type = 'PROG'.
    <ls_cs>-item-obj_name = 'ZHELLO'.
    APPEND INITIAL LINE TO <ls_cs>-files ASSIGNING <ls_file>.
    <ls_file>-path     = '/'.
    <ls_file>-filename = 'zhello.prog.abap'.
    <ls_file>-sha1     = 'hash1'.
    APPEND INITIAL LINE TO <ls_cs>-files ASSIGNING <ls_file>.
    <ls_file>-path     = '/'.
    <ls_file>-filename = 'zhello.prog.xml'.
    <ls_file>-sha1     = 'hash2'.

    ev_str = space_to_separator(
      |DEVC $PKG $PKG\n| &&
      |/ $pkg.devc.xml hash3\n| &&
      |PROG ZHELLO $PKG\n| &&
      |/ zhello.prog.abap hash1\n| &&
      |/ zhello.prog.xml hash2|
    ).

  ENDMETHOD.

  METHOD space_to_separator.

    rv_str = replace(
      val  = iv_str
      sub  = ` `
      with = `|`
      occ  = 0 ).
    " This way it's easier to read and adjust :)

  ENDMETHOD.

  METHOD serialize.

    DATA lt_checksums TYPE zif_abapgit_persistence=>ty_local_checksum_tt.
    DATA lv_act TYPE string.
    DATA lv_exp TYPE string.

    get_mock(
      IMPORTING
        et_checksums = lt_checksums
        ev_str       = lv_exp ).

    lv_act = lcl_checksum_serializer=>serialize( lt_checksums ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  ENDMETHOD.

  METHOD deserialize.

    DATA lt_checksums_exp TYPE zif_abapgit_persistence=>ty_local_checksum_tt.
    DATA lt_checksums_act TYPE zif_abapgit_persistence=>ty_local_checksum_tt.
    DATA lv_str TYPE string.

    get_mock(
      IMPORTING
        et_checksums = lt_checksums_exp
        ev_str       = lv_str ).

    lt_checksums_act = lcl_checksum_serializer=>deserialize( lv_str ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_checksums_act
      exp = lt_checksums_exp ).

  ENDMETHOD.

ENDCLASS.

**********************************************************************
* CHECKSUMS
**********************************************************************

CLASS ltcl_test_checksums DEFINITION FINAL
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PUBLIC SECTION.

    INTERFACES zif_abapgit_persist_repo_cs.

    DATA mv_last_update_key TYPE zif_abapgit_persistence=>ty_repo-key.
    DATA mv_last_update_cs_blob TYPE zif_abapgit_persistence=>ty_content-data_str.

    METHODS get FOR TESTING.
    METHODS rebuild_simple FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

**********************************************************************
* HELPERS
**********************************************************************

CLASS ltcl_repo_mock DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_repo.
    INTERFACES zif_abapgit_repo_srv.
    DATA mt_local_files TYPE zif_abapgit_definitions=>ty_files_item_tt.
    DATA mt_remote_files TYPE zif_abapgit_definitions=>ty_files_tt.
ENDCLASS.

CLASS ltcl_repo_mock IMPLEMENTATION.

  METHOD zif_abapgit_repo_srv~get.
    IF iv_key = '1'.
      ri_repo = me.
    ENDIF.
  ENDMETHOD.

  METHOD zif_abapgit_repo~get_files_local.
    rt_files = mt_local_files.
  ENDMETHOD.

  METHOD zif_abapgit_repo~get_files_remote.
    rt_files = mt_remote_files.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_local_file_builder DEFINITION FINAL.
  PUBLIC SECTION.
    DATA mt_tab TYPE zif_abapgit_definitions=>ty_files_item_tt.
    METHODS add IMPORTING iv_str TYPE STRING.
ENDCLASS.

CLASS ltcl_local_file_builder IMPLEMENTATION.
  METHOD add.
    DATA ls_item LIKE LINE OF mt_tab.
    DATA lv_tmp TYPE string.
    lv_tmp = iv_str.
    condense lv_tmp.
    SPLIT lv_tmp AT space INTO
      ls_item-item-devclass
      ls_item-item-obj_type
      ls_item-item-obj_name
      ls_item-file-path
      ls_item-file-filename
      ls_item-file-sha1.
    APPEND ls_item TO mt_tab.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_remote_file_builder DEFINITION FINAL.
  PUBLIC SECTION.
    DATA mt_tab TYPE zif_abapgit_definitions=>ty_files_tt.
    METHODS add IMPORTING iv_str TYPE STRING.
ENDCLASS.

CLASS ltcl_remote_file_builder IMPLEMENTATION.
  METHOD add.
    DATA ls_item LIKE LINE OF mt_tab.
    DATA lv_tmp TYPE string.
    lv_tmp = iv_str.
    condense lv_tmp.
    SPLIT lv_tmp AT space INTO
      ls_item-path
      ls_item-filename
      ls_item-sha1.
    APPEND ls_item TO mt_tab.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
* CHECKSUMS UT
**********************************************************************

CLASS ltcl_test_checksums IMPLEMENTATION.

  METHOD get.

    DATA li_cut TYPE REF TO zif_abapgit_repo_checksums.
    DATA lt_checksums_exp TYPE zif_abapgit_persistence=>ty_local_checksum_tt.

    zcl_abapgit_persist_injector=>set_repo_cs( me ).

    ltcl_test_checksum_serializer=>get_mock( IMPORTING et_checksums = lt_checksums_exp ).

    CREATE OBJECT li_cut TYPE zcl_abapgit_repo_checksums
      EXPORTING
        iv_repo_key = '1'.

    cl_abap_unit_assert=>assert_equals(
      act = li_cut->get( )
      exp = lt_checksums_exp ).

  ENDMETHOD.

  METHOD rebuild_simple.

    DATA lo_mock TYPE REF TO ltcl_repo_mock.
    DATA li_cut TYPE REF TO zif_abapgit_repo_checksums.
    DATA lv_cs_exp TYPE string.
    DATA lo_l_builder TYPE REF TO ltcl_local_file_builder.
    DATA lo_r_builder TYPE REF TO ltcl_remote_file_builder.

    CREATE OBJECT lo_mock.

    zcl_abapgit_repo_srv=>inject_instance( lo_mock ).
    zcl_abapgit_persist_injector=>set_repo_cs( me ).

    " Local
    CREATE OBJECT lo_l_builder.
    lo_l_builder->add( '$PKG PROG ZHELLO / zhello.prog.abap hash1' ).
    lo_l_builder->add( '$PKG PROG ZHELLO / zhello.prog.xml  hash2' ).
    lo_l_builder->add( '$PKG DEVC $PKG   / $pkg.devc.xml    hash3' ).
    lo_mock->mt_local_files = lo_l_builder->mt_tab.

    " Remote
    CREATE OBJECT lo_r_builder.
    lo_r_builder->add( '/ zhello.prog.abap hash1' ).
    lo_r_builder->add( '/ zhello.prog.xml  hash2' ).
    lo_r_builder->add( '/ $pkg.devc.xml    hash3' ).
    lo_mock->mt_remote_files = lo_r_builder->mt_tab.

    lv_cs_exp = ltcl_test_checksum_serializer=>space_to_separator(
      |DEVC $PKG $PKG\n| &&
      |/ $pkg.devc.xml hash3\n| &&
      |PROG ZHELLO $PKG\n| &&
      |/ zhello.prog.abap hash1\n| &&
      |/ zhello.prog.xml hash2|
    ).

    CREATE OBJECT li_cut TYPE zcl_abapgit_repo_checksums
      EXPORTING
        iv_repo_key = '1'.

    li_cut->rebuild( ).

    cl_abap_unit_assert=>assert_equals(
      act = mv_last_update_key
      exp = '1' ).
    cl_abap_unit_assert=>assert_equals(
      act = mv_last_update_cs_blob
      exp = lv_cs_exp ).

  ENDMETHOD.

  METHOD zif_abapgit_persist_repo_cs~delete.

  ENDMETHOD.

  METHOD zif_abapgit_persist_repo_cs~read.

    IF iv_key = '1'.
      ltcl_test_checksum_serializer=>get_mock( IMPORTING ev_str = rv_cs_blob ).
    ENDIF.

  ENDMETHOD.

  METHOD zif_abapgit_persist_repo_cs~update.
    mv_last_update_key     = iv_key.
    mv_last_update_cs_blob = iv_cs_blob.
  ENDMETHOD.

ENDCLASS.
