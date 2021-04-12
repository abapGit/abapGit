CLASS zcl_abapgit_filename_logic DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF c_package_file,
        obj_name  TYPE c LENGTH 7 VALUE 'package',
        sep1      TYPE c LENGTH 1 VALUE '.',
        obj_type  TYPE c LENGTH 4 VALUE 'devc',
        sep2      TYPE c LENGTH 1 VALUE '.',
        extension TYPE c LENGTH 3 VALUE 'xml',
      END OF c_package_file.

    CLASS-METHODS file_to_object
      IMPORTING
        !iv_filename TYPE string
        !iv_path     TYPE string
        !iv_devclass TYPE devclass OPTIONAL
        !io_dot      TYPE REF TO zcl_abapgit_dot_abapgit
      EXPORTING
        !es_item     TYPE zif_abapgit_definitions=>ty_item
        !ev_is_xml   TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS object_to_file
      IMPORTING
        !is_item           TYPE zif_abapgit_definitions=>ty_item
        !iv_ext            TYPE string
        !iv_extra          TYPE clike OPTIONAL
      RETURNING
        VALUE(rv_filename) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_filename_logic IMPLEMENTATION.


  METHOD file_to_object.

    DATA:
      lv_name TYPE string,
      lv_type TYPE string,
      lv_ext  TYPE string.

    " Guess object type and name
    SPLIT to_upper( iv_filename ) AT '.' INTO lv_name lv_type lv_ext.

    " Handle namespaces
    REPLACE ALL OCCURRENCES OF '#' IN lv_name WITH '/'.
    REPLACE ALL OCCURRENCES OF '#' IN lv_type WITH '/'.
    REPLACE ALL OCCURRENCES OF '#' IN lv_ext WITH '/'.

    " The counter part to this logic must be maintained in OBJECT_TO_FILE
    IF lv_type = to_upper( c_package_file-obj_type ).
      " Try to get a unique package name for DEVC by using the path
      ASSERT lv_name = to_upper( c_package_file-obj_name ).
      lv_name = zcl_abapgit_folder_logic=>get_instance( )->path_to_package(
        iv_top                  = iv_devclass
        io_dot                  = io_dot
        iv_create_if_not_exists = abap_false
        iv_path                 = iv_path ).
    ELSE.
      " Get original object name
      lv_name = cl_http_utility=>unescape_url( lv_name ).
    ENDIF.

    CLEAR es_item.
    es_item-obj_type = lv_type.
    es_item-obj_name = lv_name.
    ev_is_xml        = boolc( lv_ext = to_upper( c_package_file-extension ) AND strlen( lv_type ) = 4 ).

  ENDMETHOD.


  METHOD object_to_file.

    DATA lv_obj_name TYPE string.

    lv_obj_name = is_item-obj_name.

    " The counter part to this logic must be maintained in FILE_TO_OBJECT
    IF is_item-obj_type = to_upper( c_package_file-obj_type ).
      " Packages have a fixed filename so that the repository can be installed to a different
      " package(-hierarchy) on the client and not show up as a different package in the repo.
      lv_obj_name = c_package_file-obj_name.
    ELSE.
      " Some characters in object names cause problems when identifying the object later
      " -> we escape these characters here
      " cl_http_utility=>escape_url doesn't do dots but escapes slash which we use for namespaces
      " -> we escape just some selected characters
      REPLACE ALL OCCURRENCES OF `%` IN lv_obj_name WITH '%25'.
      REPLACE ALL OCCURRENCES OF `#` IN lv_obj_name WITH '%23'.
      REPLACE ALL OCCURRENCES OF `.` IN lv_obj_name WITH '%2e'.
      REPLACE ALL OCCURRENCES OF `=` IN lv_obj_name WITH '%3d'.
      REPLACE ALL OCCURRENCES OF `?` IN lv_obj_name WITH '%3f'.
      REPLACE ALL OCCURRENCES OF `<` IN lv_obj_name WITH '%3c'.
      REPLACE ALL OCCURRENCES OF `>` IN lv_obj_name WITH '%3e'.
    ENDIF.

    IF iv_extra IS INITIAL.
      CONCATENATE lv_obj_name '.' is_item-obj_type INTO rv_filename.
    ELSE.
      CONCATENATE lv_obj_name '.' is_item-obj_type '.' iv_extra INTO rv_filename.
    ENDIF.

    IF iv_ext IS NOT INITIAL.
      CONCATENATE rv_filename '.' iv_ext INTO rv_filename.
    ENDIF.

    " handle namespaces
    REPLACE ALL OCCURRENCES OF '/' IN rv_filename WITH '#'.
    TRANSLATE rv_filename TO LOWER CASE.

  ENDMETHOD.
ENDCLASS.
