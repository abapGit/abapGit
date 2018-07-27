
CLASS ltcl_test DEFINITION DEFERRED.
CLASS zcl_abapgit_branch_overview DEFINITION LOCAL FRIENDS ltcl_test.


CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.

    METHODS:
      parse_commits FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD parse_commits.

    DATA: lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt,
          lt_commits TYPE zcl_abapgit_branch_overview=>ty_commits,
          ls_commit  LIKE LINE OF lt_commits,
          ls_object  LIKE LINE OF lt_objects.


    ls_object-type = zif_abapgit_definitions=>gc_type-commit.
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

    lt_commits = zcl_abapgit_branch_overview=>parse_commits( lt_objects ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_commits )
      exp = 1 ).

    READ TABLE lt_commits INTO ls_commit INDEX 1.
    cl_abap_unit_assert=>assert_subrc( ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_commit-message
      exp = 'VIEW' ).

  ENDMETHOD.

ENDCLASS.
