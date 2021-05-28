CLASS ltcl_git_transport DEFINITION DEFERRED.
CLASS zcl_abapgit_git_transport DEFINITION LOCAL FRIENDS ltcl_git_transport.

CLASS ltcl_git_transport DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS:
      all_ok FOR TESTING,
      unpack_error FOR TESTING,
      command_error FOR TESTING,
      flush_error FOR TESTING,
      gitlab_flush_all_ok FOR TESTING.

ENDCLASS.

CLASS ltcl_git_transport IMPLEMENTATION.

  METHOD all_ok.

    DATA lv_status TYPE string.

    lv_status = `000eunpack ok` &&
      zif_abapgit_definitions=>c_newline &&
      `0017ok refs/heads/main` &&
      zif_abapgit_definitions=>c_newline &&
      `00000000`.
    TRY.
        zcl_abapgit_git_transport=>check_report_status( lv_status ).
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.

  METHOD unpack_error.

    DATA lv_status TYPE string.
    DATA lx_error TYPE REF TO zcx_abapgit_exception.

    lv_status = `0012unpack failed` &&
      zif_abapgit_definitions=>c_newline &&
      `00000000`.

    TRY.
        zcl_abapgit_git_transport=>check_report_status( lv_status ).

        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_error.
        cl_abap_unit_assert=>assert_equals(
          act = lx_error->get_text( )
          exp = 'Git protocol error: Unpack not ok (failed)' ).
    ENDTRY.

  ENDMETHOD.

  METHOD command_error.

    DATA lv_status TYPE string.
    DATA lx_error TYPE REF TO zcx_abapgit_exception.

    lv_status = `000eunpack ok` &&
      zif_abapgit_definitions=>c_newline &&
      `009fng refs/heads/main cannot lock ref 'refs/heads/main':` &&
      ` is at 625990aec9a9ebacbb99021804ce07bb6c350d05 but expected 77d2cfba9595de4c247d26b12cd40ce6cb75d61e` &&
      zif_abapgit_definitions=>c_newline &&
      `00000000`.

    TRY.
        zcl_abapgit_git_transport=>check_report_status( lv_status ).

        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_error.
        cl_abap_unit_assert=>assert_equals(
          act = lx_error->get_text( )
          exp = 'Git protocol error: Branch cannot be locked' ).
    ENDTRY.

  ENDMETHOD.

  METHOD flush_error.

    DATA lv_status TYPE string.
    DATA lx_error TYPE REF TO zcx_abapgit_exception.

    lv_status = `000eunpack ok` &&
      zif_abapgit_definitions=>c_newline &&
      `0017ok refs/heads/main` &&
      zif_abapgit_definitions=>c_newline &&
      '0020ok refs/heads/main'. " example for more data instead of flush

    TRY.
        zcl_abapgit_git_transport=>check_report_status( lv_status ).

        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception INTO lx_error.
        cl_abap_unit_assert=>assert_equals(
          act = lx_error->get_text( )
          exp = 'Git protocol error: Unexpected end of status (flush-pkt)' ).
    ENDTRY.

  ENDMETHOD.

  METHOD gitlab_flush_all_ok.

    " GitLab and Bitbucket use 0000 opposed to GitHub which uses 00000000

    DATA lv_status TYPE string.
    DATA lx_error TYPE REF TO zcx_abapgit_exception.

    lv_status = `000eunpack ok` &&
      zif_abapgit_definitions=>c_newline &&
      `0019ok refs/heads/master` &&
      zif_abapgit_definitions=>c_newline &&
      `0000`.

    TRY.
        zcl_abapgit_git_transport=>check_report_status( lv_status ).
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
