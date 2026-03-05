CLASS zcl_abapgit_objects_compare DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS get_comparator
      IMPORTING
        is_item              TYPE zif_abapgit_definitions=>ty_item
      RETURNING
        VALUE(ri_comparator) TYPE REF TO zif_abapgit_comparator.

    CLASS-METHODS get_result
      IMPORTING
        ii_comparator    TYPE REF TO zif_abapgit_comparator
        iv_filename      TYPE string
        it_local         TYPE zif_abapgit_definitions=>ty_files_item_tt
        it_remote        TYPE zif_abapgit_git_definitions=>ty_files_tt
      RETURNING
        VALUE(rv_result) TYPE string
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_objects_compare IMPLEMENTATION.


  METHOD get_comparator.

    DATA lv_class_name TYPE seoclsname.

    CONCATENATE 'ZCL_ABAPGIT_OBJECT_' is_item-obj_type '_COMPAR' INTO lv_class_name.

    IF zcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_true.
      " Prevent accidental usage of object handlers in the developer version
      lv_class_name = |\\PROGRAM={ sy-repid }\\CLASS={ lv_class_name }|.
    ENDIF.

    TRY.
        CREATE OBJECT ri_comparator TYPE (lv_class_name)
          EXPORTING
            is_item = is_item.
      CATCH cx_sy_create_object_error ##NO_HANDLER.
        " No instance, no comparator for this object type
    ENDTRY.

  ENDMETHOD.


  METHOD get_result.

    " this method is used for comparing local with remote objects
    " before pull, this is useful eg. when overwriting a TABL object.
    " only the main XML file is used for comparison

    DATA:
      ls_remote_file    TYPE zif_abapgit_git_definitions=>ty_file,
      ls_local_file     TYPE zif_abapgit_definitions=>ty_file_item,
      li_remote_version TYPE REF TO zif_abapgit_xml_input,
      li_local_version  TYPE REF TO zif_abapgit_xml_input,
      li_log            TYPE REF TO zif_abapgit_log,
      ls_msg            TYPE zif_abapgit_log=>ty_log_out,
      lt_msg            TYPE zif_abapgit_log=>ty_log_outs,
      ls_result         TYPE zif_abapgit_comparator=>ty_result.

    " REMOTE
    " if file does not exist in remote, we don't need to validate
    READ TABLE it_remote WITH KEY file COMPONENTS filename = iv_filename INTO ls_remote_file.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CREATE OBJECT li_remote_version TYPE zcl_abapgit_xml_input
      EXPORTING
        iv_xml      = zcl_abapgit_convert=>xstring_to_string_utf8( ls_remote_file-data )
        iv_filename = iv_filename.

    " LOCAL
    " if file does not exist locally, we don't need to validate
    READ TABLE it_local WITH KEY file-filename = iv_filename INTO ls_local_file.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CREATE OBJECT li_local_version TYPE zcl_abapgit_xml_input
      EXPORTING
        iv_xml      = zcl_abapgit_convert=>xstring_to_string_utf8( ls_local_file-file-data )
        iv_filename = iv_filename.

    " COMPARE
    CREATE OBJECT li_log TYPE zcl_abapgit_log.

    ls_result = ii_comparator->compare(
      ii_local  = li_local_version
      ii_remote = li_remote_version
      ii_log    = li_log ).

    rv_result = ls_result-text.

    " To keep it simple, append the log messages to the result
    lt_msg = li_log->get_messages( ).

    LOOP AT lt_msg INTO ls_msg.
      rv_result = |{ rv_result }, { ls_msg-text }|.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
