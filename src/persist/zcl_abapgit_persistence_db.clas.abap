CLASS zcl_abapgit_persistence_db DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.
    CONSTANTS c_tabname TYPE c LENGTH 30 VALUE 'ZABAPGIT' ##NO_TEXT.
    CONSTANTS c_lock TYPE c LENGTH 30 VALUE 'EZABAPGIT' ##NO_TEXT.

    CONSTANTS:
      c_type_settings   TYPE zif_abapgit_persistence=>ty_type VALUE 'SETTINGS' ##NO_TEXT,
      c_type_repo       TYPE zif_abapgit_persistence=>ty_type VALUE 'REPO' ##NO_TEXT,
      c_type_repo_csum  TYPE zif_abapgit_persistence=>ty_type VALUE 'REPO_CS' ##NO_TEXT,
      c_type_background TYPE zif_abapgit_persistence=>ty_type VALUE 'BACKGROUND' ##NO_TEXT,
      c_type_packages   TYPE zif_abapgit_persistence=>ty_type VALUE 'PACKAGES' ##NO_TEXT,
      c_type_user       TYPE zif_abapgit_persistence=>ty_type VALUE 'USER' ##NO_TEXT.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_db) TYPE REF TO zcl_abapgit_persistence_db .
    METHODS add
      IMPORTING
        !iv_type  TYPE zif_abapgit_persistence=>ty_type
        !iv_value TYPE zif_abapgit_persistence=>ty_content-value
        !iv_data  TYPE zif_abapgit_persistence=>ty_content-data_str
      RAISING
        zcx_abapgit_exception .
    METHODS delete
      IMPORTING
        !iv_type  TYPE zif_abapgit_persistence=>ty_type
        !iv_value TYPE zif_abapgit_persistence=>ty_content-value
      RAISING
        zcx_abapgit_exception .
    METHODS list
      RETURNING
        VALUE(rt_content) TYPE zif_abapgit_persistence=>ty_contents .
    METHODS list_by_type
      IMPORTING
        !iv_type          TYPE zif_abapgit_persistence=>ty_type
      RETURNING
        VALUE(rt_content) TYPE zif_abapgit_persistence=>ty_contents .
    METHODS list_by_keys
      IMPORTING it_keys            TYPE zif_abapgit_persistence=>ty_repo_keys
                iv_type            TYPE zif_abapgit_persistence=>ty_type
      RETURNING VALUE(rt_contents) TYPE zif_abapgit_persistence=>ty_contents.
    METHODS lock
      IMPORTING
        !iv_mode  TYPE enqmode DEFAULT 'E'
        !iv_type  TYPE zif_abapgit_persistence=>ty_type
        !iv_value TYPE zif_abapgit_persistence=>ty_content-value
      RAISING
        zcx_abapgit_exception .
    METHODS modify
      IMPORTING
        !iv_type  TYPE zif_abapgit_persistence=>ty_type
        !iv_value TYPE zif_abapgit_persistence=>ty_content-value
        !iv_data  TYPE zif_abapgit_persistence=>ty_content-data_str
      RAISING
        zcx_abapgit_exception .
    METHODS read
      IMPORTING
        !iv_type       TYPE zif_abapgit_persistence=>ty_type
        !iv_value      TYPE zif_abapgit_persistence=>ty_content-value
      RETURNING
        VALUE(rv_data) TYPE zif_abapgit_persistence=>ty_content-data_str
      RAISING
        zcx_abapgit_not_found .
    METHODS update
      IMPORTING
        !iv_type  TYPE zif_abapgit_persistence=>ty_type
        !iv_value TYPE zif_abapgit_persistence=>ty_content-value
        !iv_data  TYPE zif_abapgit_persistence=>ty_content-data_str
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS validate_entry_type
      IMPORTING
        !iv_type  TYPE zif_abapgit_persistence=>ty_type
      RAISING
        zcx_abapgit_exception .

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA go_db TYPE REF TO zcl_abapgit_persistence_db .
    DATA mv_update_function TYPE funcname .

    METHODS get_update_function
      RETURNING
        VALUE(rv_funcname) TYPE funcname .
    METHODS validate_and_unprettify_xml
      IMPORTING
        !iv_xml       TYPE string
      RETURNING
        VALUE(rv_xml) TYPE string
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_PERSISTENCE_DB IMPLEMENTATION.


  METHOD add.

    DATA ls_table TYPE zif_abapgit_persistence=>ty_content.

    validate_entry_type( iv_type ).
    ls_table-type  = iv_type.
    ls_table-value = iv_value.
    ls_table-data_str = iv_data.

    INSERT (c_tabname) FROM ls_table.                     "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD delete.

    lock( iv_type  = iv_type
          iv_value = iv_value ).

    " Ignore errors since record might not exist
    DELETE FROM (c_tabname)
      WHERE type = iv_type
      AND value = iv_value.

  ENDMETHOD.


  METHOD get_instance.

    IF go_db IS NOT BOUND.
      CREATE OBJECT go_db.
    ENDIF.
    ro_db = go_db.

  ENDMETHOD.


  METHOD get_update_function.
    IF mv_update_function IS INITIAL.
      mv_update_function = 'CALL_V1_PING'.
      CALL FUNCTION 'FUNCTION_EXISTS'
        EXPORTING
          funcname = mv_update_function
        EXCEPTIONS
          OTHERS   = 2.

      IF sy-subrc <> 0.
        mv_update_function = 'BANK_OBJ_WORKL_RELEASE_LOCKS'.
      ENDIF.
    ENDIF.
    rv_funcname = mv_update_function.

  ENDMETHOD.


  METHOD list.
    SELECT * FROM (c_tabname)
      INTO TABLE rt_content.                              "#EC CI_SUBRC
  ENDMETHOD.


  METHOD list_by_keys.
    FIELD-SYMBOLS: <ls_key> LIKE LINE OF it_keys.
    LOOP AT it_keys ASSIGNING <ls_key>.
      SELECT * FROM (c_tabname)
      APPENDING TABLE rt_contents
      WHERE value = <ls_key> AND
            type  = iv_type.
    ENDLOOP.
  ENDMETHOD.


  METHOD list_by_type.
    SELECT * FROM (c_tabname)
      INTO TABLE rt_content
      WHERE type = iv_type
      ORDER BY PRIMARY KEY.                               "#EC CI_SUBRC
  ENDMETHOD.


  METHOD lock.
    DATA: lv_dummy_update_function TYPE funcname.

    CALL FUNCTION 'ENQUEUE_EZABAPGIT'
      EXPORTING
        mode_zabapgit  = iv_mode
        type           = iv_type
        value          = iv_value
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    lv_dummy_update_function = get_update_function( ).

* trigger dummy update task to automatically release locks at commit
    CALL FUNCTION lv_dummy_update_function
      IN UPDATE TASK.

  ENDMETHOD.


  METHOD modify.

    DATA: ls_content TYPE zif_abapgit_persistence=>ty_content.

    lock( iv_type  = iv_type
          iv_value = iv_value ).

    ls_content-type  = iv_type.
    ls_content-value = iv_value.
    ls_content-data_str = iv_data.

    MODIFY (c_tabname) FROM ls_content.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'DB modify failed' ).
    ENDIF.

  ENDMETHOD.


  METHOD read.

    SELECT SINGLE data_str FROM (c_tabname) INTO rv_data
      WHERE type = iv_type
      AND value = iv_value.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abapgit_not_found.
    ENDIF.

  ENDMETHOD.


  METHOD update.

    DATA lv_data LIKE iv_data.

    IF iv_data CS '<?xml'.
      lv_data = validate_and_unprettify_xml( iv_data ).
    ELSE.
      lv_data = iv_data.
    ENDIF.

    lock( iv_type  = iv_type
          iv_value = iv_value ).

    UPDATE (c_tabname) SET data_str = lv_data
      WHERE type  = iv_type
      AND value = iv_value.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'DB update failed' ).
    ENDIF.

  ENDMETHOD.


  METHOD validate_and_unprettify_xml.

    rv_xml = zcl_abapgit_xml_pretty=>print(
      iv_xml           = iv_xml
      iv_unpretty      = abap_true
      iv_ignore_errors = abap_false ).

  ENDMETHOD.


  METHOD validate_entry_type.

    IF NOT (
      iv_type = c_type_repo OR
      iv_type = c_type_repo_csum OR
      iv_type = c_type_user OR
      iv_type = c_type_settings OR
      iv_type = c_type_background OR
      iv_type = c_type_packages ).
      zcx_abapgit_exception=>raise( |Invalid DB entry type [{ iv_type }]| ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
