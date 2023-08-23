CLASS zcl_abapgit_web_objects_api DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS serialize_objects
      IMPORTING
        iv_top_package TYPE devclass
        it_objs        TYPE zif_abapgit_definitions=>ty_obj_tt
        is_dot_data    TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit OPTIONAL
      EXPORTING
        ei_log         TYPE REF TO zif_abapgit_log
        et_files       TYPE zif_abapgit_definitions=>ty_files_item_tt
      RAISING
        zcx_abapgit_exception .


  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS get_dot
      IMPORTING is_dot_data   TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit OPTIONAL
      RETURNING VALUE(ro_dot) TYPE REF TO zcl_abapgit_dot_abapgit.


    CLASS-METHODS get_filter
      IMPORTING it_objs          TYPE zif_abapgit_definitions=>ty_obj_tt
      RETURNING VALUE(rt_filter) TYPE zif_abapgit_definitions=>ty_tadir_tt.


ENDCLASS.


CLASS zcl_abapgit_web_objects_api IMPLEMENTATION.

  METHOD serialize_objects.

    DATA lo_dot TYPE REF TO zcl_abapgit_dot_abapgit.
    DATA lt_filter TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA lo_serialize TYPE REF TO zcl_abapgit_serialize.
    DATA lt_files  TYPE zif_abapgit_definitions=>ty_files_item_tt.

    CLEAR ei_log.
    CLEAR et_files.

    IF iv_top_package IS INITIAL.
      zcx_abapgit_exception=>raise( |Please enter top package| ).
    ENDIF.

    lo_dot = get_dot( is_dot_data ).

    lt_filter = get_filter( it_objs = it_objs  ).

    CREATE OBJECT ei_log TYPE zcl_abapgit_log.

    CREATE OBJECT lo_serialize
      EXPORTING
        io_dot_abapgit = lo_dot.

    lt_files =  lo_serialize->files_local(
      EXPORTING
        iv_package     = iv_top_package
        ii_log         = ei_log
        it_filter      = lt_filter ).

    "We want to handle only objects so delete the not relevant files
    DELETE et_files WHERE file-filename = zif_abapgit_definitions=>c_dot_abapgit.
    DELETE et_files WHERE file-filename = zif_abapgit_apack_definitions=>c_dot_apack_manifest.
  ENDMETHOD.

  METHOD get_dot.
    "Get abapGit. Dot use default values if a field is not provided

    DATA ls_dot_new_data TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit.
    DATA lo_dot_default TYPE REF TO zcl_abapgit_dot_abapgit.
    lo_dot_default = zcl_abapgit_dot_abapgit=>build_default( ).

    ls_dot_new_data = lo_dot_default->get_data( ).

    IF NOT is_dot_data-master_language IS INITIAL.
      ls_dot_new_data-master_language = is_dot_data-master_language.
    ENDIF.

    ls_dot_new_data-use_lxe = is_dot_data-use_lxe.

    IF NOT is_dot_data-i18n_languages IS INITIAL.
      ls_dot_new_data-i18n_languages = is_dot_data-i18n_languages.
    ENDIF.

    IF NOT is_dot_data-starting_folder IS INITIAL.
      ls_dot_new_data-starting_folder = is_dot_data-starting_folder.
    ENDIF.

    IF NOT is_dot_data-folder_logic IS INITIAL.
      ls_dot_new_data-folder_logic = is_dot_data-folder_logic.
    ENDIF.

    IF NOT is_dot_data-ignore IS INITIAL.
      ls_dot_new_data-ignore = is_dot_data-ignore.
    ENDIF.

    IF NOT is_dot_data-requirements IS INITIAL.
      ls_dot_new_data-requirements = is_dot_data-requirements.
    ENDIF.

    IF NOT is_dot_data-version_constant IS INITIAL.
      ls_dot_new_data-version_constant = is_dot_data-version_constant.
    ENDIF.

    IF NOT is_dot_data-abap_language_version IS INITIAL.
      ls_dot_new_data-abap_language_version = is_dot_data-abap_language_version.
    ENDIF.

    CREATE OBJECT ro_dot
      EXPORTING
        is_data = ls_dot_new_data.

  ENDMETHOD.

  METHOD get_filter.
    DATA lr_obj TYPE REF TO  zif_abapgit_definitions=>ty_obj.
    DATA ls_filter TYPE zif_abapgit_definitions=>ty_tadir.

    LOOP AT it_objs REFERENCE INTO lr_obj.
      ls_filter-object = lr_obj->obj_type.
      ls_filter-obj_name = lr_obj->obj_name.
      INSERT ls_filter INTO TABLE rt_filter.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
