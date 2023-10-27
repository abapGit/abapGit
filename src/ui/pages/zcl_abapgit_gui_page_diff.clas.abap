CLASS zcl_abapgit_gui_page_diff DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page_diff_base
  CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        !iv_key        TYPE zif_abapgit_persistence=>ty_repo-key
        !is_file       TYPE zif_abapgit_git_definitions=>ty_file OPTIONAL
        !is_object     TYPE zif_abapgit_definitions=>ty_item OPTIONAL
        !it_files      TYPE zif_abapgit_definitions=>ty_stage_tt OPTIONAL
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_gui_page_diff IMPLEMENTATION.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_diff.

    CREATE OBJECT lo_component
      EXPORTING
        iv_key    = iv_key
        is_file   = is_file
        is_object = is_object
        it_files  = it_files.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Diff'
      iv_page_layout        = get_page_layout( )
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.
ENDCLASS.
