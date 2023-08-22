CLASS zcl_abapgit_deserialize_repo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS deserialize
      IMPORTING
        !io_repo                 TYPE REF TO zcl_abapgit_repo
        !is_checks               TYPE zif_abapgit_definitions=>ty_deserialize_checks
        !ii_log                  TYPE REF TO zif_abapgit_log
      RETURNING
        VALUE(rt_accessed_files) TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt
      RAISING
        zcx_abapgit_exception .


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_deserialize_repo IMPLEMENTATION.

  METHOD deserialize.

    DATA lt_remote  TYPE zif_abapgit_git_definitions=>ty_files_tt.
    DATA lv_package TYPE devclass.
    DATA lv_transport TYPE trkorr.
    DATA lt_results  TYPE zif_abapgit_definitions=>ty_results_tt.
    DATA lr_result TYPE REF TO zif_abapgit_definitions=>ty_result.
    DATA lt_deserialized_objects  TYPE zcl_abapgit_objects=>ty_deserialization_tt.
    DATA ls_deserialized_object  TYPE zcl_abapgit_objects=>ty_deserialization.
    DATA lo_files    TYPE REF TO zcl_abapgit_objects_files.
    DATA ls_item     TYPE zif_abapgit_definitions=>ty_item.
    DATA lv_path TYPE string.

    lv_package = io_repo->get_package( ).

    IF is_checks-transport-required = abap_true.
      lv_transport = is_checks-transport-transport.
    ENDIF.

    lt_remote = io_repo->get_files_remote( iv_ignore_files = abap_true ).

    lt_results = zcl_abapgit_file_deserialize=>get_results(
      io_repo = io_repo
      ii_log = ii_log ).

    IF lt_results IS INITIAL.
      RETURN.
    ENDIF.

    zcl_abapgit_objects_check=>checks_adjust(
      EXPORTING
        io_dot    = io_repo->get_dot_abapgit( )
        iv_top_package    = io_repo->get_package( )
        is_checks  = is_checks
      CHANGING
        ct_results = lt_results ).

    LOOP AT lt_results REFERENCE INTO lr_result.
      CLEAR ls_deserialized_object.

      ls_deserialized_object-item-obj_type = lr_result->obj_type.
      ls_deserialized_object-item-obj_name = lr_result->obj_name.
      ls_deserialized_object-main_filename = lr_result->filename.
      ls_deserialized_object-only_package_move = lr_result->packmove.
      ls_deserialized_object-top_package = lv_package.

      CLEAR ls_item.
      ls_item-obj_type = lr_result->obj_type.
      ls_item-obj_name = lr_result->obj_name.

      CLEAR lv_path.
      IF ls_item-obj_type = 'DEVC'.
        " Packages have the same filename across different folders. The path needs to be supplied
        " to find the correct file.
        lv_path = lr_result->path.
      ENDIF.

      CREATE OBJECT lo_files
        EXPORTING
          is_item = ls_item
          iv_path = lv_path.

      lo_files->set_files( lt_remote ).

      ls_deserialized_object-files = lo_files->get_files( ).
      INSERT ls_deserialized_object INTO TABLE lt_deserialized_objects.
    ENDLOOP.

    zcl_abapgit_objects=>deserialize_central(
      EXPORTING
        iv_transport            = lv_transport
        iv_main_language_only   = io_repo->get_local_settings( )-main_language_only
        io_dot                  = io_repo->get_dot_abapgit( )
        ii_log                  = ii_log
*    iv_conf_diff_wo_warn    = abap_false
      IMPORTING
        et_accessed_files       = rt_accessed_files
      CHANGING
        ct_deserialized_objects = lt_deserialized_objects ).

  ENDMETHOD.

ENDCLASS.
