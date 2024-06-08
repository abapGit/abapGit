CLASS lcl_package_to_path DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS get
      IMPORTING
        !iv_top        TYPE devclass
        !io_dot        TYPE REF TO zcl_abapgit_dot_abapgit
        !iv_package    TYPE devclass
      RETURNING
        VALUE(rv_path) TYPE string.

    CLASS-METHODS add
      IMPORTING
        !iv_top     TYPE devclass
        !io_dot     TYPE REF TO zcl_abapgit_dot_abapgit
        !iv_package TYPE devclass
        !iv_path    TYPE string.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_buffer,
        top             TYPE devclass,
        starting_folder TYPE string,
        folder_logic    TYPE string,
        package         TYPE devclass,
        path            TYPE string,
      END OF ty_buffer.

    CLASS-DATA gt_buffer
      TYPE HASHED TABLE OF ty_buffer
      WITH UNIQUE KEY top starting_folder folder_logic package.

ENDCLASS.

CLASS lcl_package_to_path IMPLEMENTATION.

  METHOD get.

    FIELD-SYMBOLS <ls_buffer> LIKE LINE OF gt_buffer.

    READ TABLE gt_buffer ASSIGNING <ls_buffer> WITH TABLE KEY
      top             = iv_top
      starting_folder = io_dot->get_starting_folder( )
      folder_logic    = io_dot->get_folder_logic( )
      package         = iv_package.
    IF sy-subrc = 0.
      rv_path = <ls_buffer>-path.
    ENDIF.

  ENDMETHOD.

  METHOD add.

    DATA ls_buffer LIKE LINE OF gt_buffer.

    CLEAR ls_buffer.
    ls_buffer-top             = iv_top.
    ls_buffer-starting_folder = io_dot->get_starting_folder( ).
    ls_buffer-folder_logic    = io_dot->get_folder_logic( ).
    ls_buffer-package         = iv_package.
    ls_buffer-path            = iv_path.
    INSERT ls_buffer INTO TABLE gt_buffer.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_path_to_package DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS get
      IMPORTING
        !iv_top           TYPE devclass
        !io_dot           TYPE REF TO zcl_abapgit_dot_abapgit
        !iv_path          TYPE string
      RETURNING
        VALUE(rv_package) TYPE devclass.

    CLASS-METHODS add
      IMPORTING
        !iv_top     TYPE devclass
        !io_dot     TYPE REF TO zcl_abapgit_dot_abapgit
        !iv_path    TYPE string
        !iv_package TYPE devclass.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_buffer,
        top             TYPE devclass,
        starting_folder TYPE string,
        folder_logic    TYPE string,
        path            TYPE string,
        package         TYPE devclass,
      END OF ty_buffer.

    CLASS-DATA gt_buffer
      TYPE HASHED TABLE OF ty_buffer
      WITH UNIQUE KEY top starting_folder folder_logic path.

ENDCLASS.

CLASS lcl_path_to_package IMPLEMENTATION.

  METHOD get.

    FIELD-SYMBOLS <ls_buffer> LIKE LINE OF gt_buffer.

    READ TABLE gt_buffer ASSIGNING <ls_buffer> WITH TABLE KEY
      top             = iv_top
      starting_folder = io_dot->get_starting_folder( )
      folder_logic    = io_dot->get_folder_logic( )
      path            = iv_path.
    IF sy-subrc = 0.
      rv_package = <ls_buffer>-package.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD add.

    DATA ls_buffer LIKE LINE OF gt_buffer.

    CLEAR ls_buffer.
    ls_buffer-top             = iv_top.
    ls_buffer-starting_folder = io_dot->get_starting_folder( ).
    ls_buffer-folder_logic    = io_dot->get_folder_logic( ).
    ls_buffer-path            = iv_path.
    ls_buffer-package         = iv_package.
    INSERT ls_buffer INTO TABLE gt_buffer.

  ENDMETHOD.

ENDCLASS.
