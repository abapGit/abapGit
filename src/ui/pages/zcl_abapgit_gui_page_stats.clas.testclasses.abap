CLASS ltcl_size_format DEFINITION DEFERRED.
CLASS zcl_abapgit_gui_page_stats DEFINITION LOCAL FRIENDS ltcl_size_format.

CLASS ltcl_size_format DEFINITION FINAL FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS unit_boundaries FOR TESTING.
    METHODS fractional_sizes FOR TESTING.
    METHODS large_sizes FOR TESTING.
    METHODS favorites_then_description FOR TESTING.
    METHODS numeric_sort FOR TESTING.
ENDCLASS.

CLASS ltcl_size_format IMPLEMENTATION.
  METHOD unit_boundaries.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_gui_page_stats=>format_size( 0 )
      exp = '0 Bytes' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_gui_page_stats=>format_size( 1023 )
      exp = '1023 Bytes' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_gui_page_stats=>format_size( 1024 )
      exp = '1.00 KB' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_gui_page_stats=>format_size( 1048576 )
      exp = '1.00 MB' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_gui_page_stats=>format_size( 1073741824 )
      exp = '1.00 GB' ).
  ENDMETHOD.

  METHOD fractional_sizes.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_gui_page_stats=>format_size( 1536 )
      exp = '1.50 KB' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_gui_page_stats=>format_size( 1572864 )
      exp = '1.50 MB' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_gui_page_stats=>format_size( 1610612736 )
      exp = '1.50 GB' ).
  ENDMETHOD.

  METHOD large_sizes.
    DATA lv_size TYPE zcl_abapgit_statistics=>ty_size.
    lv_size = '3221225472'.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_gui_page_stats=>format_size( lv_size )
      exp = '3.00 GB' ).
  ENDMETHOD.

  METHOD favorites_then_description.
    DATA lt_repos TYPE zcl_abapgit_statistics=>ty_repos.
    DATA ls_repo LIKE LINE OF lt_repos.
    DATA ls_sorting TYPE zif_abapgit_html_table=>ty_sorting_state.

    ls_repo-name = 'Zulu'.
    APPEND ls_repo TO lt_repos.
    ls_repo-favorite = abap_true.
    APPEND ls_repo TO lt_repos.
    ls_repo-name = 'Alpha'.
    APPEND ls_repo TO lt_repos.
    ls_repo-favorite = abap_false.
    APPEND ls_repo TO lt_repos.
    ls_sorting-column_id = 'favorite'.
    ls_sorting-descending = abap_true.
    lt_repos = zcl_abapgit_gui_page_stats=>sort_repositories(
      it_repos = lt_repos
      is_sorting = ls_sorting ).

    READ TABLE lt_repos INTO ls_repo INDEX 1.
    cl_abap_unit_assert=>assert_equals( act = ls_repo-name
      exp = 'Alpha' ).
    cl_abap_unit_assert=>assert_equals( act = ls_repo-favorite
      exp = abap_true ).
    READ TABLE lt_repos INTO ls_repo INDEX 2.
    cl_abap_unit_assert=>assert_equals( act = ls_repo-name
      exp = 'Zulu' ).
    cl_abap_unit_assert=>assert_equals( act = ls_repo-favorite
      exp = abap_true ).
    READ TABLE lt_repos INTO ls_repo INDEX 3.
    cl_abap_unit_assert=>assert_equals( act = ls_repo-name
      exp = 'Alpha' ).
    cl_abap_unit_assert=>assert_equals( act = ls_repo-favorite
      exp = abap_false ).
    READ TABLE lt_repos INTO ls_repo INDEX 4.
    cl_abap_unit_assert=>assert_equals( act = ls_repo-name
      exp = 'Zulu' ).
    cl_abap_unit_assert=>assert_equals( act = ls_repo-favorite
      exp = abap_false ).
  ENDMETHOD.

  METHOD numeric_sort.
    DATA lt_repos TYPE zcl_abapgit_statistics=>ty_repos.
    DATA ls_repo LIKE LINE OF lt_repos.
    DATA ls_sorting TYPE zif_abapgit_html_table=>ty_sorting_state.

    ls_repo-name = 'Small'.
    ls_repo-size = 1024.
    APPEND ls_repo TO lt_repos.
    ls_repo-name = 'Large'.
    ls_repo-size = 1048576.
    APPEND ls_repo TO lt_repos.
    ls_sorting-column_id = 'size'.
    lt_repos = zcl_abapgit_gui_page_stats=>sort_repositories(
      it_repos = lt_repos
      is_sorting = ls_sorting ).
    READ TABLE lt_repos INTO ls_repo INDEX 1.
    cl_abap_unit_assert=>assert_equals( act = ls_repo-name
      exp = 'Small' ).
    ls_sorting-descending = abap_true.
    lt_repos = zcl_abapgit_gui_page_stats=>sort_repositories(
      it_repos = lt_repos
      is_sorting = ls_sorting ).
    READ TABLE lt_repos INTO ls_repo INDEX 1.
    cl_abap_unit_assert=>assert_equals( act = ls_repo-name
      exp = 'Large' ).
  ENDMETHOD.
ENDCLASS.
