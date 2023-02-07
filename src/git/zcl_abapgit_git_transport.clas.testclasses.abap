CLASS ltcl_git_transport DEFINITION DEFERRED.
CLASS zcl_abapgit_git_transport DEFINITION LOCAL FRIENDS ltcl_git_transport.

CLASS ltcl_git_transport DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS:
      all_ok FOR TESTING RAISING cx_static_check,
      unpack_error FOR TESTING RAISING cx_static_check,
      command_error FOR TESTING RAISING cx_static_check,
      flush_error FOR TESTING RAISING cx_static_check,
      parse FOR TESTING RAISING cx_static_check,
      gitlab_flush_all_ok FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_git_transport IMPLEMENTATION.

  METHOD parse.

    DATA lv_data TYPE xstring.
    DATA lv_pack TYPE xstring.

    lv_data = '303033347368616C6C6F77206639656332336436'
      && '6439333561613764633236656531343163376234'
      && '3666656564396434363835653030303030303038'
      && '4E414B0A30323564015041434B00000002000000'
      && '059E0C789C958BC109C3300C00FF9EC2FF428965'
      && '59B2A194AE22273209244D70D4FD1BE804BDCFC1'
      && 'C15957F543454488A47168A9317023168446B932'
      && 'C53171014E44EA0EE9FA361F529BA4D594558443'
      && '4C5827E0C20C5090545498931664271F9BF7EE57'
      && 'E9E77CF8C7CFAF79B74D96F53EEEDBD307CC9902'
      && '9610FC6DB87057DD1633FD7373A6A77D0161323D'
      && '31A307789C33343030333151D04B4C4A2C48CF2C'
      && 'D1ABC8CD61A85FBB4172EFBBF6E6BC39974DD41E'
      && 'BFEE08889FB0DA10A22EC8D5D1C5D7552F378561'
      && 'F3E315727BDDAF7059C5DEE7EFF415E369E4D2EF'
      && '842A4ACBCF4F4A2CD22BA928617877E244D7E46F'
      && 'EF44457F377B7C3AA874FE9633D35900D7032E45'
      && 'B31E789C6D91D16A83301486EF7D8AE0FD92ED6E'
      && '484C093515C1EA5007BB93D45A974D8D9854DDB3'
      && 'ED628FB45798D5C26CF1EEFCFFF7710827BFDF3F'
      && '78335425E8F2560959DBE6137C34415E67F228EA'
      && 'C236CFFAF4F06C6E8881B91A2C7EE00D18F55A59'
      && '63B4CD77AD1B0BA1BEEFA1E20DCC64852ECA6898'
      && 'B71B8901A6051D2FCFB91A13C00E4DE86500784F'
      && 'E38445A94F03F795BA8C308CEEABC98B131A255E'
      && 'E0A6BBD07758441046F7D5E4CD73EA87AEB7252F'
      && '11DB796F18DD9493E6B94118CDAB01163AAF0882'
      && '85D0A2A8659B6334354BE87B5B16C46C85448C3A'
      && '7B06ABE30A6B78F6C98B1C7E2859AF60A85BDE09'
      && '05BFAA72859EA43CF016EA412F2046FF2FC7E87A'
      && '468C96E79DD3E52B88F10759BF90313D789C5356'
      && '70CD2D28A9E42A492D2EE102001BCB042734789C'
      && 'CBCDCFE7020003ED0156C43280EA381436A38EE5'
      && 'E78491FF99CEAAC43A30303036017530303030'.

    zcl_abapgit_git_transport=>parse(
      IMPORTING
        ev_pack = lv_pack
      CHANGING
        cv_data = lv_data ).

    cl_abap_unit_assert=>assert_equals(
      act = xstrlen( lv_data )
      exp = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = xstrlen( lv_pack )
      exp = 601 ).

  ENDMETHOD.

  METHOD all_ok.

    DATA lv_status TYPE string.

    lv_status = `000eunpack ok` &&
      cl_abap_char_utilities=>newline &&
      `0017ok refs/heads/main` &&
      cl_abap_char_utilities=>newline &&
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
      cl_abap_char_utilities=>newline &&
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
      cl_abap_char_utilities=>newline &&
      `009fng refs/heads/main cannot lock ref 'refs/heads/main':` &&
      ` is at 625990aec9a9ebacbb99021804ce07bb6c350d05 but expected 77d2cfba9595de4c247d26b12cd40ce6cb75d61e` &&
      cl_abap_char_utilities=>newline &&
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
      cl_abap_char_utilities=>newline &&
      `0017ok refs/heads/main` &&
      cl_abap_char_utilities=>newline &&
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

    lv_status = `000eunpack ok` &&
      cl_abap_char_utilities=>newline &&
      `0019ok refs/heads/master` &&
      cl_abap_char_utilities=>newline &&
      `0000`.

    TRY.
        zcl_abapgit_git_transport=>check_report_status( lv_status ).
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
