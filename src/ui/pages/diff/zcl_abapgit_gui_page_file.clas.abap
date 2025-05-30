CLASS zcl_abapgit_gui_page_file DEFINITION
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

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_abapgit_gui_page_file IMPLEMENTATION.

  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_patch.

    ASSERT is_local IS NOT INITIAL.
    ASSERT is_remote IS NOT INITIAL.

    CREATE OBJECT lo_component.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Diff File'
      iv_page_layout        = zcl_abapgit_gui_page=>c_page_layout-full_width
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

* todo, call method append_diff ?

  ENDMETHOD.

ENDCLASS.
