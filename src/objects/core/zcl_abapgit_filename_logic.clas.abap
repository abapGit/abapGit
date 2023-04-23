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

    CONSTANTS:
      BEGIN OF c_json_file,
        extension TYPE c LENGTH 4 VALUE 'json',
      END OF c_json_file.

    CLASS-METHODS detect_obj_definition
      IMPORTING
        !iv_type    TYPE string
        !iv_ext     TYPE string
      EXPORTING
        !ev_is_xml  TYPE abap_bool
        !ev_is_json TYPE abap_bool.

    CLASS-METHODS is_obj_definition_file
      IMPORTING
        !iv_filename  TYPE string
      RETURNING
        VALUE(rv_yes) TYPE abap_bool.

    CLASS-METHODS file_to_object
      IMPORTING
        !iv_filename TYPE string
        !iv_path     TYPE string
        !iv_devclass TYPE devclass OPTIONAL
        !io_dot      TYPE REF TO zcl_abapgit_dot_abapgit
      EXPORTING
        !es_item     TYPE zif_abapgit_definitions=>ty_item
        !ev_is_xml   TYPE abap_bool
        !ev_is_json  TYPE abap_bool
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

    CLASS-DATA:
      go_aff_registry TYPE REF TO zif_abapgit_aff_registry.

    CLASS-METHODS name_escape
      IMPORTING
        !iv_name       TYPE csequence
      RETURNING
        VALUE(rv_name) TYPE string.

    CLASS-METHODS name_unescape
      IMPORTING
        !iv_name       TYPE csequence
      RETURNING
        VALUE(rv_name) TYPE string.

    CLASS-METHODS map_filename_to_object
      IMPORTING
        !iv_filename TYPE string
        !iv_path     TYPE string
        !iv_package  TYPE devclass
        !io_dot      TYPE REF TO zcl_abapgit_dot_abapgit
      CHANGING
        cs_item      TYPE zif_abapgit_definitions=>ty_item
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS map_object_to_filename
      IMPORTING
        !is_item    TYPE zif_abapgit_definitions=>ty_item
      CHANGING
        cv_filename TYPE string
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_filename_logic IMPLEMENTATION.


  METHOD detect_obj_definition.

    ev_is_xml  = boolc( iv_ext = to_upper( c_package_file-extension ) AND strlen( iv_type ) = 4 ).
    ev_is_json = boolc( iv_ext = to_upper( c_json_file-extension ) AND strlen( iv_type ) = 4 ).

  ENDMETHOD.


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

    " Assume AFF namespace convention
    CREATE OBJECT go_aff_registry TYPE zcl_abapgit_aff_registry.

    IF go_aff_registry->is_supported_object_type( |{ lv_type }| ) = abap_true.
      REPLACE ALL OCCURRENCES OF '(' IN lv_name WITH '/'.
      REPLACE ALL OCCURRENCES OF ')' IN lv_name WITH '/'.
    ENDIF.

    " Get original object name
    lv_name = name_unescape( lv_name ).

    CLEAR es_item.
    es_item-obj_type = lv_type.
    es_item-obj_name = lv_name.

    " Get mapping specific to object type
    map_filename_to_object(
      EXPORTING
        iv_filename = iv_filename
        iv_path     = iv_path
        io_dot      = io_dot
        iv_package  = iv_devclass
      CHANGING
        cs_item     = es_item ).

    detect_obj_definition(
      EXPORTING
        iv_ext     = lv_ext
        iv_type    = lv_type
      IMPORTING
        ev_is_xml  = ev_is_xml
        ev_is_json = ev_is_json ).

  ENDMETHOD.


  METHOD is_obj_definition_file.

    DATA:
      lv_xml  TYPE abap_bool,
      lv_json TYPE abap_bool,
      lv_name TYPE string,
      lv_type TYPE string,
      lv_ext  TYPE string.

    SPLIT to_upper( iv_filename ) AT '.' INTO lv_name lv_type lv_ext.

    detect_obj_definition(
      EXPORTING
        iv_ext     = lv_ext
        iv_type    = lv_type
      IMPORTING
        ev_is_xml  = lv_xml
        ev_is_json = lv_json ).

    rv_yes = boolc( lv_json = abap_true OR lv_xml = abap_true ).

  ENDMETHOD.


  METHOD map_filename_to_object.

    DATA lv_class TYPE seoclsname.

    " TODO: Add check for supported object types to avoid calls to non-existing classes
    " zcl_abapgit_objects=>is_type_supported( is_item-obj_type )
    " This will trigger class constructor of zcl_abapgit_objects_bridge reading table seometarel
    " which is currently not supported by abaplint test runner

    TRY.
        lv_class = 'ZCL_ABAPGIT_OBJECT_' && cs_item-obj_type.

        CALL METHOD (lv_class)=>('ZIF_ABAPGIT_OBJECT~MAP_FILENAME_TO_OBJECT')
          EXPORTING
            iv_filename = iv_filename
            iv_path     = iv_path
            io_dot      = io_dot
            iv_package  = iv_package
          CHANGING
            cs_item     = cs_item.
      CATCH cx_sy_dyn_call_illegal_class ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD map_object_to_filename.

    DATA lv_class TYPE seoclsname.

    " TODO: Add check for supported object types to avoid calls to non-existing classes
    " zcl_abapgit_objects=>is_type_supported( is_item-obj_type )
    " This will trigger class constructor of zcl_abapgit_objects_bridge reading table seometarel
    " which is currently not supported by abaplint test runner

    TRY.
        lv_class = 'ZCL_ABAPGIT_OBJECT_' && is_item-obj_type.

        CALL METHOD (lv_class)=>('ZIF_ABAPGIT_OBJECT~MAP_OBJECT_TO_FILENAME')
          EXPORTING
            is_item     = is_item
          CHANGING
            cv_filename = cv_filename.
      CATCH cx_sy_dyn_call_illegal_class ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD name_escape.
    " Some characters in object names cause problems when identifying the object later
    " -> we escape these characters here
    " cl_http_utility=>escape_url doesn't do dots but escapes slash which we use for namespaces
    " -> we escape just some selected characters
    rv_name = iv_name.
    REPLACE ALL OCCURRENCES OF `#` IN rv_name WITH '%23'.
    REPLACE ALL OCCURRENCES OF `%` IN rv_name WITH '%25'.
    REPLACE ALL OCCURRENCES OF `.` IN rv_name WITH '%2e'.
    REPLACE ALL OCCURRENCES OF `<` IN rv_name WITH '%3c'.
    REPLACE ALL OCCURRENCES OF `=` IN rv_name WITH '%3d'.
    REPLACE ALL OCCURRENCES OF `>` IN rv_name WITH '%3e'.
    REPLACE ALL OCCURRENCES OF `?` IN rv_name WITH '%3f'.
  ENDMETHOD.


  METHOD name_unescape.
    " Replace all %xy with encoded character
    rv_name = cl_http_utility=>unescape_url( iv_name ).
  ENDMETHOD.


  METHOD object_to_file.

    DATA lv_obj_name TYPE string.
    DATA lv_nb_of_slash TYPE string.

    " Get escaped object name
    lv_obj_name = to_lower( name_escape( is_item-obj_name ) ).

    IF iv_extra IS INITIAL.
      CONCATENATE lv_obj_name '.' is_item-obj_type INTO rv_filename.
    ELSE.
      CONCATENATE lv_obj_name '.' is_item-obj_type '.' iv_extra INTO rv_filename.
    ENDIF.

    IF iv_ext IS NOT INITIAL.
      CONCATENATE rv_filename '.' iv_ext INTO rv_filename.
    ENDIF.

    " Get mapping specific to object type
    TRY.
        map_object_to_filename(
          EXPORTING
            is_item     = is_item
          CHANGING
            cv_filename = rv_filename ).
      CATCH zcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

    " Handle namespaces
    CREATE OBJECT go_aff_registry TYPE zcl_abapgit_aff_registry.

    IF go_aff_registry->is_supported_object_type( is_item-obj_type ) = abap_true.
      FIND ALL OCCURRENCES OF `/` IN rv_filename MATCH COUNT lv_nb_of_slash.
      IF lv_nb_of_slash = 2.
        REPLACE FIRST OCCURRENCE OF `/` IN rv_filename WITH `(`.
        REPLACE `/` IN rv_filename WITH `)`.
      ENDIF.
    ELSE.
      REPLACE ALL OCCURRENCES OF '/' IN rv_filename WITH '#'.
    ENDIF.

    TRANSLATE rv_filename TO LOWER CASE.

  ENDMETHOD.
ENDCLASS.
