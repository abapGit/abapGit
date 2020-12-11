CLASS zcl_abapgit_folder_logic DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS package_to_path
      IMPORTING
        !iv_top        TYPE devclass
        !io_dot        TYPE REF TO zcl_abapgit_dot_abapgit
        !iv_package    TYPE devclass
      RETURNING
        VALUE(rv_path) TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS path_to_package
      IMPORTING
        !iv_top                  TYPE devclass
        !io_dot                  TYPE REF TO zcl_abapgit_dot_abapgit
        !iv_path                 TYPE string
        !iv_create_if_not_exists TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rv_package)        TYPE devclass
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abapgit_folder_logic .
  PROTECTED SECTION.

    METHODS get_parent
      IMPORTING
        !iv_package      TYPE devclass
      RETURNING
        VALUE(rv_parent) TYPE devclass
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_devclass_info,
        devclass  TYPE devclass,
        namespace TYPE namespace,
        parentcl  TYPE parentcl,
      END OF ty_devclass_info .
    TYPES:
      ty_devclass_info_tt TYPE SORTED TABLE OF ty_devclass_info
        WITH UNIQUE KEY devclass .
    DATA mt_parent TYPE ty_devclass_info_tt .
ENDCLASS.



CLASS ZCL_ABAPGIT_FOLDER_LOGIC IMPLEMENTATION.


  METHOD get_instance.
    CREATE OBJECT ro_instance.
  ENDMETHOD.


  METHOD get_parent.
    DATA: ls_parent LIKE LINE OF mt_parent.

    "Determine Parent Package
    READ TABLE mt_parent INTO ls_parent
      WITH TABLE KEY devclass = iv_package.
    IF sy-subrc <> 0.
      rv_parent = zcl_abapgit_factory=>get_sap_package( iv_package )->read_parent( ).
      ls_parent-devclass = iv_package.
      ls_parent-parentcl = rv_parent.
      INSERT ls_parent INTO TABLE mt_parent.
    ELSE.
      rv_parent = ls_parent-parentcl.
    ENDIF.
  ENDMETHOD.


  METHOD package_to_path.

    DATA: lv_len          TYPE i,
          lv_path         TYPE string,
          lv_message      TYPE string,
          lv_parentcl     TYPE tdevc-parentcl,
          lv_folder_logic TYPE string.

    IF iv_top = iv_package.
      rv_path = io_dot->get_starting_folder( ).
    ELSE.
      lv_parentcl = get_parent( iv_package ).

      IF lv_parentcl IS INITIAL.
        zcx_abapgit_exception=>raise( |error, expected parent package, { iv_package }| ).
      ELSE.
        lv_folder_logic = io_dot->get_folder_logic( ).
        CASE lv_folder_logic.
          WHEN zif_abapgit_dot_abapgit=>c_folder_logic-full.
            lv_len = 0.
            IF iv_package(1) = '$'.
              lv_len = 1.
            ENDIF.
          WHEN zif_abapgit_dot_abapgit=>c_folder_logic-prefix.
            lv_len = strlen( lv_parentcl ).

            IF iv_package(lv_len) <> lv_parentcl.
* if abapGit project is installed in package ZZZ, all subpackages should be named
* ZZZ_something. This will define the folder name in the zip file to be "something",
* similarily with online projects. Alternatively change to FULL folder logic
              lv_message = 'PREFIX: Unexpected package naming (' && iv_package && ')'
                           && 'you might switch to FULL folder logic'.
              zcx_abapgit_exception=>raise( lv_message ).
            ENDIF.
          WHEN OTHERS.
            zcx_abapgit_exception=>raise( |Invalid folder logic: { lv_folder_logic }| ).
        ENDCASE.

        lv_path = iv_package+lv_len.
        IF strlen( lv_path ) = 0.
          zcx_abapgit_exception=>raise( |Folder logic: length = 0, parent: {
            lv_parentcl }, child: { iv_package }| ).
        ENDIF.

        IF lv_path(1) = '_'.
          lv_path = lv_path+1.
        ENDIF.
        IF strlen( lv_path ) = 0.
          zcx_abapgit_exception=>raise( |Folder logic: length = 0, parent: {
            lv_parentcl }, child: { iv_package }| ).
        ENDIF.

        TRANSLATE lv_path USING '/#'.
        TRANSLATE lv_path TO LOWER CASE.
        CONCATENATE lv_path '/' INTO lv_path.

        rv_path = package_to_path( iv_top     = iv_top
                                   io_dot     = io_dot
                                   iv_package = lv_parentcl ).

        CONCATENATE rv_path lv_path INTO rv_path.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD path_to_package.

    DATA: lv_length               TYPE i,
          lv_parent               TYPE devclass,
          lv_new                  TYPE string,
          lv_path                 TYPE string,
          lv_absolute_name        TYPE string,
          lv_top                  TYPE devclass,
          lt_unique_package_names TYPE HASHED TABLE OF devclass WITH UNIQUE KEY table_line.

    lv_top = iv_top.

    lv_length  = strlen( io_dot->get_starting_folder( ) ).
    IF lv_length > strlen( iv_path ).
* treat as not existing locally
      RETURN.
    ENDIF.
    lv_path    = iv_path+lv_length.
    lv_parent  = lv_top.
    rv_package = lv_top.

    INSERT iv_top INTO TABLE lt_unique_package_names.

    WHILE lv_path CA '/'.
      SPLIT lv_path AT '/' INTO lv_new lv_path.

      CASE io_dot->get_folder_logic( ).
        WHEN zif_abapgit_dot_abapgit=>c_folder_logic-full.
          lv_absolute_name = lv_new.
          TRANSLATE lv_absolute_name USING '#/'.
          IF iv_top(1) = '$'.
            CONCATENATE '$' lv_absolute_name INTO lv_absolute_name.
          ENDIF.
        WHEN zif_abapgit_dot_abapgit=>c_folder_logic-prefix.
          CONCATENATE rv_package '_' lv_new INTO lv_absolute_name.
        WHEN OTHERS.
          ASSERT 0 = 1.
      ENDCASE.

      TRANSLATE lv_absolute_name TO UPPER CASE.

      IF strlen( lv_absolute_name ) > 30.
        zcx_abapgit_exception=>raise( |Package { lv_absolute_name } exceeds ABAP 30-characters-name limit| ).
      ENDIF.

      rv_package = lv_absolute_name.
      READ TABLE lt_unique_package_names TRANSPORTING NO FIELDS
        WITH TABLE KEY table_line = rv_package.
      IF sy-subrc = 0.
        zcx_abapgit_exception=>raise( |Package { rv_package } has a subpackage with the same name| ).
      ELSE.
        INSERT rv_package INTO TABLE lt_unique_package_names.
      ENDIF.

      IF zcl_abapgit_factory=>get_sap_package( rv_package )->exists( ) = abap_false AND
          iv_create_if_not_exists = abap_true.

        zcl_abapgit_factory=>get_sap_package( lv_parent )->create_child( rv_package ).
      ENDIF.

      lv_parent = rv_package.
    ENDWHILE.

  ENDMETHOD.
ENDCLASS.
