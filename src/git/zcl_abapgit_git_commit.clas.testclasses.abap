CLASS ltcl_test DEFINITION DEFERRED.
CLASS zcl_abapgit_git_commit DEFINITION LOCAL FRIENDS ltcl_test.


CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.

    METHODS:
      parse_commits FOR TESTING RAISING zcx_abapgit_exception,
      check_author_regex FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD parse_commits.

    DATA: lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt,
          lt_commits TYPE zif_abapgit_definitions=>ty_commit_tt,
          ls_commit  LIKE LINE OF lt_commits,
          ls_object  LIKE LINE OF lt_objects.


    ls_object-type = zif_abapgit_definitions=>c_type-commit.
    ls_object-data = '7472656520396335376238613931336465306539' &&
      '3735333630633261306330643638363037306162' &&
      '61343965650A706172656E742036393532346462' &&
      '3139363263383839366566343364323861616131' &&
      '396536366533373263653364620A617574686F72' &&
      '206C6172736870203C6C617273687040686F746D' &&
      '61696C2E636F6D3E203135333236313133353020' &&
      '2B303030300A636F6D6D6974746572206C617273' &&
      '6870203C6C617273687040686F746D61696C2E63' &&
      '6F6D3E2031353332363131333530202B30303030' &&
      '0A0A56494557'.
    APPEND ls_object TO lt_objects.

    lt_commits = zcl_abapgit_git_commit=>parse_commits( lt_objects ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_commits )
      exp = 1 ).

    READ TABLE lt_commits INTO ls_commit INDEX 1.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_commit-message
      exp = 'VIEW' ).

  ENDMETHOD.

  METHOD check_author_regex.

    DATA: lt_objects         TYPE zif_abapgit_definitions=>ty_objects_tt,
          lt_commits         TYPE zif_abapgit_definitions=>ty_commit_tt,
          ls_object          LIKE LINE OF lt_objects,
          ls_git_pack_commit TYPE zcl_abapgit_git_pack=>ty_commit.


    ls_object-type = zif_abapgit_definitions=>c_type-commit.
    ls_git_pack_commit-tree      = 'dummy'.
    ls_git_pack_commit-committer = 'dummy'.

    ls_git_pack_commit-author = 'Lastname, Firstname <mail@mail.com> 1532611350 +0000'.
    ls_object-data = zcl_abapgit_git_pack=>encode_commit( ls_git_pack_commit ).
    APPEND ls_object TO lt_objects.

    ls_git_pack_commit-author = 'Soeren Mueller <mail@mail.com> 1532611350 +0000'.
    ls_object-data = zcl_abapgit_git_pack=>encode_commit( ls_git_pack_commit ).
    APPEND ls_object TO lt_objects.

    ls_git_pack_commit-author = 'S#ren M#ller <mail@mail.com> 1532611350 +0000'.
    ls_object-data = zcl_abapgit_git_pack=>encode_commit( ls_git_pack_commit ).
    APPEND ls_object TO lt_objects.

    ls_git_pack_commit-author = 'Dr. Werner Mueller <mail@mail.com> 1532611350 +0000'.
    ls_object-data = zcl_abapgit_git_pack=>encode_commit( ls_git_pack_commit ).
    APPEND ls_object TO lt_objects.

    ls_git_pack_commit-author = 'SAP*........ <SAP*........@localhost> 1535375483 +0200'.
    ls_object-data = zcl_abapgit_git_pack=>encode_commit( ls_git_pack_commit ).
    APPEND ls_object TO lt_objects.

    ls_git_pack_commit-author = |Soeren %26 Sascha <soerenschlegel87@gmail.com> 1557574255 +0000|.
    ls_object-data = zcl_abapgit_git_pack=>encode_commit( ls_git_pack_commit ).
    APPEND ls_object TO lt_objects.

    lt_commits = zcl_abapgit_git_commit=>parse_commits( lt_objects ).

    cl_abap_unit_assert=>assert_equals(
       act = lines( lt_commits )
       exp = 6 ).

  ENDMETHOD.

ENDCLASS.

CLASS ltc_parent_handling DEFINITION DEFERRED.
CLASS zcl_abapgit_git_commit DEFINITION LOCAL FRIENDS ltc_parent_handling.
CLASS ltc_parent_handling DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mt_commits TYPE zif_abapgit_definitions=>ty_commit_tt.
    METHODS blank_not_missing FOR TESTING RAISING cx_static_check.
    METHODS parent_is_missing FOR TESTING RAISING cx_static_check.
    METHODS parent_is_found FOR TESTING RAISING cx_static_check.
    METHODS missing_parent1_cleared FOR TESTING RAISING cx_static_check.
    METHODS missing_parent2_cleared FOR TESTING RAISING cx_static_check.
    METHODS matched_parent1_remains FOR TESTING RAISING cx_static_check.
    METHODS given_commit_sha1 IMPORTING iv_sha1 TYPE zif_abapgit_git_definitions=>ty_sha1.
    METHODS parent_should_be_missing IMPORTING iv_sha1 TYPE zif_abapgit_git_definitions=>ty_sha1.
    METHODS parent_should_not_be_missing IMPORTING iv_sha1 TYPE zif_abapgit_git_definitions=>ty_sha1.
    METHODS given_commit IMPORTING iv_sha1    TYPE zif_abapgit_git_definitions=>ty_sha1
                                   iv_parent1 TYPE zif_abapgit_git_definitions=>ty_sha1 OPTIONAL
                                   iv_parent2 TYPE zif_abapgit_git_definitions=>ty_sha1 OPTIONAL.
ENDCLASS.


CLASS ltc_parent_handling IMPLEMENTATION.

  METHOD blank_not_missing.
    given_commit_sha1( 'F00' ).
    parent_should_not_be_missing( '' ).
  ENDMETHOD.

  METHOD parent_is_missing.
    given_commit_sha1( 'F00' ).
    parent_should_be_missing( 'BA5' ).
  ENDMETHOD.

  METHOD parent_is_found.
    given_commit_sha1( 'F00' ).
    parent_should_not_be_missing( 'F00' ).
  ENDMETHOD.

  METHOD missing_parent1_cleared.

    DATA ls_commit TYPE zif_abapgit_definitions=>ty_commit.

    given_commit( iv_sha1 = 'F00'
                  iv_parent1 = 'BA5' ).
    zcl_abapgit_git_commit=>clear_missing_parents( CHANGING ct_commits = mt_commits ).

    READ TABLE mt_commits INDEX 1 INTO ls_commit.
    cl_abap_unit_assert=>assert_equals( act = ls_commit-parent1
                                        exp = '' ).
  ENDMETHOD.

  METHOD missing_parent2_cleared.

    DATA ls_commit TYPE zif_abapgit_definitions=>ty_commit.

    given_commit( iv_sha1 = 'F00'
                  iv_parent2 = 'BA5' ).
    zcl_abapgit_git_commit=>clear_missing_parents( CHANGING ct_commits = mt_commits ).

    READ TABLE mt_commits INDEX 1 INTO ls_commit.
    cl_abap_unit_assert=>assert_equals( act = ls_commit-parent2
                                        exp = '' ).

  ENDMETHOD.

  METHOD matched_parent1_remains.

    DATA ls_commit TYPE zif_abapgit_definitions=>ty_commit.

    given_commit( iv_sha1 = 'F00' ).
    given_commit( iv_sha1 = 'BA5'
                  iv_parent1 = 'F00' ).

    zcl_abapgit_git_commit=>clear_missing_parents( CHANGING ct_commits = mt_commits ).

    READ TABLE mt_commits INDEX 2 INTO ls_commit.
    cl_abap_unit_assert=>assert_equals( act = ls_commit-parent1
                                        exp = 'F00' ).

  ENDMETHOD.

  METHOD given_commit_sha1.
    DATA ls_commit TYPE zif_abapgit_definitions=>ty_commit.

    ls_commit-sha1 = iv_sha1.
    APPEND ls_commit TO mt_commits.

  ENDMETHOD.


  METHOD parent_should_be_missing.
    cl_abap_unit_assert=>assert_true(
        zcl_abapgit_git_commit=>is_missing( it_commits = mt_commits
                                            iv_sha1    = iv_sha1 ) ).
  ENDMETHOD.

  METHOD parent_should_not_be_missing.
    cl_abap_unit_assert=>assert_false(
        zcl_abapgit_git_commit=>is_missing( it_commits = mt_commits
                                            iv_sha1    = iv_sha1 ) ).
  ENDMETHOD.

  METHOD given_commit.
    FIELD-SYMBOLS: <ls_commit> TYPE zif_abapgit_definitions=>ty_commit.
    APPEND INITIAL LINE TO mt_commits ASSIGNING <ls_commit>.
    <ls_commit>-sha1 = iv_sha1.
    <ls_commit>-parent1 = iv_parent1.
    <ls_commit>-parent2 = iv_parent2.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_extract_author_data DEFINITION DEFERRED.
CLASS zcl_abapgit_git_commit DEFINITION LOCAL FRIENDS ltcl_extract_author_data.

CLASS ltcl_extract_author_data DEFINITION FINAL
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS extract_author_data1 FOR TESTING RAISING cx_static_check.
    METHODS extract_author_data2 FOR TESTING RAISING cx_static_check.
    METHODS extract_author_data3 FOR TESTING RAISING cx_static_check.
    METHODS extract_author_data4 FOR TESTING RAISING cx_static_check.
    METHODS extract_author_data5 FOR TESTING RAISING cx_static_check.
    METHODS extract_author_data6 FOR TESTING RAISING cx_static_check.
    METHODS extract_author_data7 FOR TESTING RAISING cx_static_check.
    METHODS extract_author_data8 FOR TESTING RAISING cx_static_check.
    METHODS extract_author_data9 FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_extract_author_data IMPLEMENTATION.

  METHOD extract_author_data1.
    TRY.
        zcl_abapgit_git_commit=>extract_author_data( 'Volker Jägle äÖüß <github@beimir.net> 1573216988 +0000' ).
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( |Language-related special characters in author name are allowed.| ).
    ENDTRY.
  ENDMETHOD.

  METHOD extract_author_data2.
    TRY.
        zcl_abapgit_git_commit=>extract_author_data(
          'pull[bot&%#$] <39814207+pull[bot]@users.noreply.github.com> 1573216988 +0000' ).
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( |Special characters in author name are allowed.| ).
    ENDTRY.
  ENDMETHOD.

  METHOD extract_author_data3.
    TRY.
        zcl_abapgit_git_commit=>extract_author_data(
          'pull[bot] <39814207+pull[bot]@users.noreply.github.com> 1573216988 +00001' ).
        cl_abap_unit_assert=>fail( |+00001 shouldn't be valid cause it's too long.| ).
      CATCH zcx_abapgit_exception.
    ENDTRY.
  ENDMETHOD.

  METHOD extract_author_data4.
    TRY.
        zcl_abapgit_git_commit=>extract_author_data(
          'pull[bot] <39814207+pull[bot]@users.noreply.github.com> 15732169881 +0000' ).
        cl_abap_unit_assert=>fail( |Value shouldn't be valid cause time is invalid.| ).
      CATCH zcx_abapgit_exception.
    ENDTRY.
  ENDMETHOD.

  METHOD extract_author_data5.
    TRY.
        zcl_abapgit_git_commit=>extract_author_data(
          '<39814207+pull[bot]@users.noreply.github.com> 1573216988 +0000' ).
        cl_abap_unit_assert=>fail( |Value shouldn't be valid cause no author name was supplied.| ).
      CATCH zcx_abapgit_exception.
    ENDTRY.
  ENDMETHOD.

  METHOD extract_author_data6.
    TRY.
        zcl_abapgit_git_commit=>extract_author_data( 'pull[bot] 1573216988 +0000' ).
        cl_abap_unit_assert=>fail( |Value shouldn't be valid cause no email was supplied.| ).
      CATCH zcx_abapgit_exception.
    ENDTRY.
  ENDMETHOD.

  METHOD extract_author_data7.
    TRY.
        zcl_abapgit_git_commit=>extract_author_data( 'pull[bot] <39814207+pull[bot]@users.noreply.github.com> +0000' ).
        cl_abap_unit_assert=>fail( |Value shouldn't be valid cause no time was supplied.| ).
      CATCH zcx_abapgit_exception.
    ENDTRY.
  ENDMETHOD.

  METHOD extract_author_data8.
    TRY.
        zcl_abapgit_git_commit=>extract_author_data(
          'pull[bot] <39814207+pull[bot]@users.noreply.github.com> 1573216988' ).
        cl_abap_unit_assert=>fail( |Value shouldn't be valid cause +0000 wasn't supplied.| ).
      CATCH zcx_abapgit_exception.
    ENDTRY.
  ENDMETHOD.

  METHOD extract_author_data9.
    TRY.
        zcl_abapgit_git_commit=>extract_author_data(
          '<pull[bot]()> <39814207+pull[bot]@users.noreply.github.com> 1573216988 +0000' ).
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( |Value should be valid though brackets are in author name.| ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
