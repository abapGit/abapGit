CLASS zcl_abapgit_gui_page_diff_file DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page_diff_base
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        iv_obj_type    TYPE tadir-object
        iv_obj_name    TYPE tadir-obj_name
        !is_local      TYPE zif_abapgit_git_definitions=>ty_file
        !is_remote     TYPE zif_abapgit_git_definitions=>ty_file
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.

    METHODS do_diff
      IMPORTING
        iv_obj_type TYPE tadir-object
        iv_obj_name TYPE tadir-obj_name
        !is_local   TYPE zif_abapgit_git_definitions=>ty_file
        !is_remote  TYPE zif_abapgit_git_definitions=>ty_file
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_DIFF_FILE IMPLEMENTATION.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_diff_file.

    ASSERT is_local IS NOT INITIAL.
    ASSERT is_remote IS NOT INITIAL.

    CREATE OBJECT lo_component.

    lo_component->do_diff(
      iv_obj_type  = iv_obj_type
      iv_obj_name  = iv_obj_name
      is_local     = is_local
      is_remote    = is_remote ).

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

    ls_status-path = is_remote-path.
    ls_status-filename = is_remote-filename.
    ls_status-obj_type = iv_obj_type.
    ls_status-obj_name = iv_obj_name.

    INSERT is_remote INTO TABLE lt_remote.

    ls_local-file = is_local.
    ls_local-item-obj_type = iv_obj_type.
    ls_local-item-obj_name = iv_obj_name.
    INSERT ls_local INTO TABLE lt_local.

    append_diff(
      it_remote = lt_remote
      it_local  = lt_local
      is_status = ls_status ).

  ENDMETHOD.
ENDCLASS.
