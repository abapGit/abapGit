
CLASS ltcl_path DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS is_root FOR TESTING.
    METHODS split_file_location FOR TESTING.
    METHODS is_subdir FOR TESTING.
    METHODS change_dir FOR TESTING.
    METHODS get_filename_from_syspath FOR TESTING.

ENDCLASS.


CLASS ltcl_path IMPLEMENTATION.

  METHOD is_root.

    cl_abap_unit_assert=>assert_equals(
      exp = abap_true
      act = zcl_abapgit_path=>is_root( '/' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = abap_false
      act = zcl_abapgit_path=>is_root( '' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = abap_false
      act = zcl_abapgit_path=>is_root( 'somedir' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = abap_false
      act = zcl_abapgit_path=>is_root( '/somedir' ) ).

  ENDMETHOD.

  METHOD split_file_location.

    DATA: lv_path TYPE string,
          lv_name TYPE string.

    zcl_abapgit_path=>split_file_location(
      EXPORTING iv_fullpath = ''
      IMPORTING ev_path     = lv_path ev_filename = lv_name ).
    cl_abap_unit_assert=>assert_equals( act = lv_path
                                        exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = lv_name
                                        exp = '' ).

    zcl_abapgit_path=>split_file_location(
      EXPORTING iv_fullpath = 'somefile'
      IMPORTING ev_path     = lv_path ev_filename = lv_name ).
    cl_abap_unit_assert=>assert_equals( act = lv_path
                                        exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = lv_name
                                        exp = 'somefile' ).

    zcl_abapgit_path=>split_file_location(
      EXPORTING iv_fullpath = '/'
      IMPORTING ev_path     = lv_path ev_filename = lv_name ).
    cl_abap_unit_assert=>assert_equals( act = lv_path
                                        exp = '/' ).
    cl_abap_unit_assert=>assert_equals( act = lv_name
                                        exp = '' ).

    zcl_abapgit_path=>split_file_location(
      EXPORTING iv_fullpath = '/somefile'
      IMPORTING ev_path     = lv_path ev_filename = lv_name ).
    cl_abap_unit_assert=>assert_equals( act = lv_path
                                        exp = '/' ).
    cl_abap_unit_assert=>assert_equals( act = lv_name
                                        exp = 'somefile' ).

    zcl_abapgit_path=>split_file_location(
      EXPORTING iv_fullpath = '/somedir/'
      IMPORTING ev_path     = lv_path ev_filename = lv_name ).
    cl_abap_unit_assert=>assert_equals( act = lv_path
                                        exp = '/somedir/' ).
    cl_abap_unit_assert=>assert_equals( act = lv_name
                                        exp = '' ).

    zcl_abapgit_path=>split_file_location(
      EXPORTING iv_fullpath = '/somedir/somefile'
      IMPORTING ev_path     = lv_path ev_filename = lv_name ).
    cl_abap_unit_assert=>assert_equals( act = lv_path
                                        exp = '/somedir/' ).
    cl_abap_unit_assert=>assert_equals( act = lv_name
                                        exp = 'somefile' ).


  ENDMETHOD.

  METHOD is_subdir.

    DATA lv_yes TYPE abap_bool.

    lv_yes = zcl_abapgit_path=>is_subdir( iv_path   = '/dir/subdir'
                                          iv_parent = '/dir' ).
    cl_abap_unit_assert=>assert_equals( act = lv_yes
                                        exp = abap_true ).

    lv_yes = zcl_abapgit_path=>is_subdir( iv_path   = '/dir/subdir'
                                          iv_parent = '/dir/' ).
    cl_abap_unit_assert=>assert_equals( act = lv_yes
                                        exp = abap_true ).

    lv_yes = zcl_abapgit_path=>is_subdir( iv_path   = '/another'
                                          iv_parent = '/dir' ).
    cl_abap_unit_assert=>assert_equals( act = lv_yes
                                        exp = abap_false ).

    lv_yes = zcl_abapgit_path=>is_subdir( iv_path   = '/dir'
                                          iv_parent = '/dir' ).
    cl_abap_unit_assert=>assert_equals( act = lv_yes
                                        exp = abap_false ).

    lv_yes = zcl_abapgit_path=>is_subdir( iv_path   = '/dir'
                                          iv_parent = '/' ).
    cl_abap_unit_assert=>assert_equals( act = lv_yes
                                        exp = abap_true ).

    lv_yes = zcl_abapgit_path=>is_subdir( iv_path   = '/dir2'
                                          iv_parent = '/dir' ).
    cl_abap_unit_assert=>assert_equals( act = lv_yes
                                        exp = abap_false ).

  ENDMETHOD.

  METHOD change_dir.

    DATA lv_path TYPE string.

    lv_path = zcl_abapgit_path=>change_dir( iv_cur_dir = ''
                                            iv_cd      = '' ).
    cl_abap_unit_assert=>assert_equals( act = lv_path
                                        exp = '' ).

    lv_path = zcl_abapgit_path=>change_dir( iv_cur_dir = '/dir'
                                            iv_cd      = '' ).
    cl_abap_unit_assert=>assert_equals( act = lv_path
                                        exp = '/dir' ).

    lv_path = zcl_abapgit_path=>change_dir( iv_cur_dir = '/dir'
                                            iv_cd      = '.' ).
    cl_abap_unit_assert=>assert_equals( act = lv_path
                                        exp = '/dir' ).

    lv_path = zcl_abapgit_path=>change_dir( iv_cur_dir = '/dir'
                                            iv_cd      = '..' ).
    cl_abap_unit_assert=>assert_equals( act = lv_path
                                        exp = '/' ).

    lv_path = zcl_abapgit_path=>change_dir( iv_cur_dir = '/dir/sub'
                                            iv_cd      = '..' ).
    cl_abap_unit_assert=>assert_equals( act = lv_path
                                        exp = '/dir/' ).

    lv_path = zcl_abapgit_path=>change_dir( iv_cur_dir = '/dir/'
                                            iv_cd      = 'sub' ).
    cl_abap_unit_assert=>assert_equals( act = lv_path
                                        exp = '/dir/sub' ).

    lv_path = zcl_abapgit_path=>change_dir( iv_cur_dir = '/dir'
                                            iv_cd      = 'sub' ).
    cl_abap_unit_assert=>assert_equals( act = lv_path
                                        exp = '/dir/sub' ).

  ENDMETHOD.

  METHOD get_filename_from_syspath.

    DATA lv_filename TYPE string.

    lv_filename = zcl_abapgit_path=>get_filename_from_syspath( 'file.txt' ).
    cl_abap_unit_assert=>assert_equals( act = lv_filename
                                        exp = 'file.txt' ).

    lv_filename = zcl_abapgit_path=>get_filename_from_syspath( 'c:\dir\file.txt' ).
    cl_abap_unit_assert=>assert_equals( act = lv_filename
                                        exp = 'file.txt' ).

    lv_filename = zcl_abapgit_path=>get_filename_from_syspath( 'c:\file.txt' ).
    cl_abap_unit_assert=>assert_equals( act = lv_filename
                                        exp = 'file.txt' ).

    lv_filename = zcl_abapgit_path=>get_filename_from_syspath( '/dir/file.txt' ).
    cl_abap_unit_assert=>assert_equals( act = lv_filename
                                        exp = 'file.txt' ).

    lv_filename = zcl_abapgit_path=>get_filename_from_syspath( '/file.txt' ).
    cl_abap_unit_assert=>assert_equals( act = lv_filename
                                        exp = 'file.txt' ).

    lv_filename = zcl_abapgit_path=>get_filename_from_syspath( '\\server$\file.txt' ).
    cl_abap_unit_assert=>assert_equals( act = lv_filename
                                        exp = 'file.txt' ).

    lv_filename = zcl_abapgit_path=>get_filename_from_syspath(
      'C:\foo\bar\moo.boo\dev\qas\_blah\goog\muuh\sap\hello\world\lorem\ipsum\s_foo.gif' ).
    cl_abap_unit_assert=>assert_equals( act = lv_filename
                                        exp = 's_foo.gif' ).

  ENDMETHOD.

ENDCLASS.
