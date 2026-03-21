CLASS zcl_abapgit_html_toolbar_lib DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS render_repo_settings_dropdown
      IMPORTING
        io_toolbar  TYPE REF TO zcl_abapgit_html_toolbar
        iv_key      TYPE zif_abapgit_persistence=>ty_value
        iv_opt      TYPE c OPTIONAL
        iv_class    TYPE string OPTIONAL
        iv_li_class TYPE string OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS build_repo_settings_dropdown
      IMPORTING
        iv_key            TYPE zif_abapgit_persistence=>ty_value
        iv_class          TYPE string OPTIONAL
      RETURNING
        VALUE(ro_toolbar) TYPE REF TO zcl_abapgit_html_toolbar.
ENDCLASS.



CLASS zcl_abapgit_html_toolbar_lib IMPLEMENTATION.

  METHOD render_repo_settings_dropdown.

    ASSERT io_toolbar IS BOUND.

    io_toolbar->add( iv_txt      = 'Repo Settings'
                     io_sub      = build_repo_settings_dropdown( iv_key   = iv_key
                                                                 iv_class = iv_class )
                     iv_opt      = iv_opt
                     iv_title    = `Repository Settings`
                     iv_class    = iv_class
                     iv_li_class = iv_li_class ).

  ENDMETHOD.


  METHOD build_repo_settings_dropdown.

    CREATE OBJECT ro_toolbar.

    ro_toolbar->add(
      iv_txt   = 'Repository'
      iv_act   = |{ zif_abapgit_definitions=>c_action-repo_settings }?key={ iv_key }|
      iv_class = iv_class
      iv_title = `Repository Settings`
    )->add(
      iv_txt   = 'Local'
      iv_act   = |{ zif_abapgit_definitions=>c_action-repo_local_settings }?key={ iv_key }|
      iv_class = iv_class
      iv_title = `Local Settings`
    )->add(
      iv_txt   = 'Remote'
      iv_act   = |{ zif_abapgit_definitions=>c_action-repo_remote_settings }?key={ iv_key }|
      iv_class = iv_class
      iv_title = `Remote Settings`
    )->add(
      iv_txt   = 'Background'
      iv_act   = |{ zif_abapgit_definitions=>c_action-repo_background }?key={ iv_key }|
      iv_class = iv_class
      iv_title = `Background Settings`
    )->add(
      iv_txt   = 'Stats'
      iv_act   = |{ zif_abapgit_definitions=>c_action-repo_infos }?key={ iv_key }|
      iv_class = iv_class
      iv_title = `Statistics` ).

  ENDMETHOD.

ENDCLASS.
