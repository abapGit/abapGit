class ZCL_ABAPGIT_FOLDER_LOGIC definition
  public
  create public .

  public section.

    methods CONSTRUCTOR
      importing
        !IV_BUFFERED type ABAP_BOOL default ABAP_FALSE .
    methods PACKAGE_TO_PATH
      importing
        !IV_TOP type DEVCLASS
        !IO_DOT type ref to ZCL_ABAPGIT_DOT_ABAPGIT
        !IV_PACKAGE type DEVCLASS
      returning
        value(RV_PATH) type STRING
      raising
        ZCX_ABAPGIT_EXCEPTION .
    methods PATH_TO_PACKAGE
      importing
        !IV_TOP type DEVCLASS
        !IO_DOT type ref to ZCL_ABAPGIT_DOT_ABAPGIT
        !IV_PATH type STRING
        !IV_CREATE_IF_NOT_EXISTS type ABAP_BOOL default ABAP_TRUE
      returning
        value(RV_PACKAGE) type DEVCLASS
      raising
        ZCX_ABAPGIT_EXCEPTION .
    class-methods GET_INSTANCE
      importing
        !IV_BUFFERED type ABAP_BOOL default ABAP_FALSE
      returning
        value(RO_INSTANCE) type ref to ZCL_ABAPGIT_FOLDER_LOGIC .
  protected section.

    data MV_BUFFERED type ABAP_BOOL .
  private section.

    data MT_DEVC_BUFFER type ZIF_ABAPGIT_DEFINITIONS=>TT_DEVC_BUFFER .
ENDCLASS.



CLASS ZCL_ABAPGIT_FOLDER_LOGIC IMPLEMENTATION.


  METHOD constructor.
    mv_buffered = iv_buffered.

    IF mv_buffered = abap_true.
      SELECT devclass parentcl
        FROM tdevc
        INTO CORRESPONDING FIELDS OF TABLE mt_devc_buffer.
    ENDIF.

  ENDMETHOD.


  METHOD get_instance.

    CREATE OBJECT ro_instance
      EXPORTING
        iv_buffered = iv_buffered.

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
      IF mv_buffered = abap_true.
        lv_parentcl = zcl_abapgit_factory=>get_sap_package( iv_package )->read_parent( mt_devc_buffer ).
      ELSE.
        lv_parentcl = zcl_abapgit_factory=>get_sap_package( iv_package )->read_parent( ).
      ENDIF.

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
                           && 'you might switch to FULL folder logic' ##no_text.
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

  ENDMETHOD.                    "class_to_path


  METHOD path_to_package.

    DATA: lv_length TYPE i,
          lv_parent TYPE devclass,
          lv_new    TYPE string,
          lv_path   TYPE string,
          lv_top    TYPE devclass.

    lv_top = iv_top.

    lv_length  = strlen( io_dot->get_starting_folder( ) ).
    IF lv_length > strlen( iv_path ).
* treat as not existing locally
      RETURN.
    ENDIF.
    lv_path    = iv_path+lv_length.
    lv_parent  = lv_top.
    rv_package = lv_top.

    WHILE lv_path CA '/'.
      SPLIT lv_path AT '/' INTO lv_new lv_path.

      CASE io_dot->get_folder_logic( ).
        WHEN zif_abapgit_dot_abapgit=>c_folder_logic-full.
          rv_package = lv_new.
          TRANSLATE rv_package USING '#/'.
          IF iv_top(1) = '$'.
            CONCATENATE '$' rv_package INTO rv_package.
          ENDIF.
        WHEN zif_abapgit_dot_abapgit=>c_folder_logic-prefix.
          CONCATENATE rv_package '_' lv_new INTO rv_package.
        WHEN OTHERS.
          ASSERT 0 = 1.
      ENDCASE.

      TRANSLATE rv_package TO UPPER CASE.

      IF zcl_abapgit_factory=>get_sap_package( rv_package )->exists( ) = abap_false AND
          iv_create_if_not_exists = abap_true.

        zcl_abapgit_factory=>get_sap_package( lv_parent )->create_child( rv_package ).
      ENDIF.

      lv_parent = rv_package.
    ENDWHILE.

  ENDMETHOD.
ENDCLASS.
