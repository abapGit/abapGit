CLASS zcl_abapgit_gui_page_diff_file DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page_diff_base
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        !is_local      TYPE zif_abapgit_git_definitions=>ty_file
        !is_remote     TYPE zif_abapgit_git_definitions=>ty_file
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
    DATA ms_local TYPE zif_abapgit_git_definitions=>ty_file.
    DATA ms_remote TYPE zif_abapgit_git_definitions=>ty_file.

    METHODS do_diff
      RAISING zcx_abapgit_exception.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_DIFF_FILE IMPLEMENTATION.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_diff_file.

    ASSERT is_local IS NOT INITIAL.
    ASSERT is_remote IS NOT INITIAL.

    CREATE OBJECT lo_component.
    lo_component->ms_local  = is_local.
    lo_component->ms_remote = is_remote.

    lo_component->do_diff( ).

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Diff File'
      iv_page_layout        = zcl_abapgit_gui_page=>c_page_layout-full_width
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.


  METHOD do_diff.

    DATA lt_remote TYPE zif_abapgit_git_definitions=>ty_files_tt.
    DATA lt_local TYPE zif_abapgit_definitions=>ty_files_item_tt.
    DATA ls_local LIKE LINE OF lt_local.
    DATA ls_status TYPE zif_abapgit_definitions=>ty_result.

    ls_status-path = ms_remote-path.
    ls_status-filename = ms_remote-filename.

    INSERT ms_remote INTO TABLE lt_remote.

    ls_local-file = ms_local.
* todo? ls_local-item
    INSERT ls_local INTO TABLE lt_local.

    append_diff(
      it_remote = lt_remote
      it_local  = lt_local
      is_status = ls_status ).

  ENDMETHOD.
ENDCLASS.
