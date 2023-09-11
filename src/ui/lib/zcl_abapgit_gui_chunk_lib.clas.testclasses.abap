*"* use this source file for your ABAP unit test classes
CLASS ltcl_normalize_program_name DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_chunk_lib TYPE REF TO zcl_abapgit_gui_chunk_lib.

    METHODS:
      setup,
      class FOR TESTING RAISING cx_static_check,
      program FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltd_repo DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_repo PARTIALLY IMPLEMENTED.

    DATA ms_data TYPE zif_abapgit_persistence=>ty_repo READ-ONLY.

    METHODS set_display_name
      IMPORTING !display_name TYPE csequence.
ENDCLASS.


CLASS ltd_repo_srv DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_repo_srv PARTIALLY IMPLEMENTED.

    METHODS add_repository
      IMPORTING !display_name TYPE csequence.

  PRIVATE SECTION.
    DATA repositories TYPE STANDARD TABLE OF REF TO ltd_repo.
ENDCLASS.


CLASS ltcl_render_repo DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_chunk_lib TYPE REF TO zcl_abapgit_gui_chunk_lib.
    DATA repo_srv TYPE REF TO ltd_repo_srv.

    METHODS:
      setup,
      render_repo_palette FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS zcl_abapgit_gui_chunk_lib DEFINITION
  LOCAL FRIENDS ltcl_normalize_program_name
                ltcl_render_repo.


CLASS ltcl_normalize_program_name IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT mo_chunk_lib.

  ENDMETHOD.


  METHOD class.

    cl_abap_unit_assert=>assert_equals(
      act = mo_chunk_lib->normalize_program_name( 'ZCL_ABAPGIT_FRONTEND_SERVICES=CP' )
      exp = `ZCL_ABAPGIT_FRONTEND_SERVICES` ).

  ENDMETHOD.


  METHOD program.

    cl_abap_unit_assert=>assert_equals(
      act = mo_chunk_lib->normalize_program_name( 'ZABAPGIT_FULL' )
      exp = `ZABAPGIT_FULL` ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_chunk_lib->normalize_program_name( 'ZSOME_PROG_ENDING_WITH_CP' )
      exp = `ZSOME_PROG_ENDING_WITH_CP` ).

  ENDMETHOD.

ENDCLASS.


CLASS ltd_repo_srv IMPLEMENTATION.

  METHOD add_repository.
    DATA(new_repo) = NEW ltd_repo( ).
    new_repo->set_display_name( display_name ).

    APPEND new_repo TO repositories.
  ENDMETHOD.

  METHOD zif_abapgit_repo_srv~list.
    LOOP AT repositories INTO DATA(repo).
      APPEND CAST #( repo ) TO rt_list.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.


CLASS ltd_repo IMPLEMENTATION.

  METHOD set_display_name.
    ms_data-local_settings-display_name = display_name.
  ENDMETHOD.

  METHOD zif_abapgit_repo~get_name.
    rv_name = ms_data-local_settings-display_name.
  ENDMETHOD.

ENDCLASS.


CLASS ltcl_render_repo IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT repo_srv.
    zcl_abapgit_repo_srv=>inject_instance( ii_srv = repo_srv ).
    CREATE OBJECT mo_chunk_lib.
  ENDMETHOD.


  METHOD render_repo_palette.
    DATA ag_exception TYPE REF TO zcx_abapgit_exception.
    DATA html_chunk TYPE REF TO zif_abapgit_html.
    DATA html_string TYPE string.

    repo_srv->add_repository( display_name = |Simple test| ).
    TRY.
        mo_chunk_lib->render_repo_palette(
          EXPORTING
            iv_action = zif_abapgit_definitions=>c_action-go_repo
          RECEIVING
            ri_html   = html_chunk ).
        html_chunk->render(
          RECEIVING
            rv_html = html_string ).
        cl_abap_unit_assert=>assert_char_cp(
            act = html_string
            exp = |*displayName: "Simple test"*| ).
      CATCH zcx_abapgit_exception INTO ag_exception.
        cl_abap_unit_assert=>fail(
            msg    = 'abapGit exception'
            detail = ag_exception->get_text( ) ).
    ENDTRY.

    repo_srv->add_repository( display_name = |'Single' quotation marks| ).
    TRY.
        mo_chunk_lib->render_repo_palette(
          EXPORTING
            iv_action = zif_abapgit_definitions=>c_action-go_repo
          RECEIVING
            ri_html   = html_chunk ).
        html_chunk->render(
          RECEIVING
            rv_html = html_string ).
        cl_abap_unit_assert=>assert_char_cp(
            act = html_string
            exp = |*displayName: "\\'Single\\' quotation marks"*| ).
      CATCH zcx_abapgit_exception INTO ag_exception.
        cl_abap_unit_assert=>fail(
            msg    = 'abapGit exception'
            detail = ag_exception->get_text( ) ).
    ENDTRY.

    repo_srv->add_repository( display_name = |"Double quotation marks"| ).
    TRY.
        mo_chunk_lib->render_repo_palette(
          EXPORTING
            iv_action = zif_abapgit_definitions=>c_action-go_repo
          RECEIVING
            ri_html   = html_chunk ).
        html_chunk->render(
          RECEIVING
            rv_html = html_string ).
        cl_abap_unit_assert=>assert_char_cp(
            act = html_string
            exp = |*displayName: "\\"Double quotation marks\\""*| ).
      CATCH zcx_abapgit_exception INTO ag_exception.
        cl_abap_unit_assert=>fail(
            msg    = 'abapGit exception'
            detail = ag_exception->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
