CLASS zcl_abapgit_persistence_db DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.
    CONSTANTS c_tabname TYPE tabname VALUE 'ZABAPGIT' ##NO_TEXT.
    CONSTANTS c_lock TYPE viewname VALUE 'EZABAPGIT' ##NO_TEXT.

    CONSTANTS:
      c_type_settings   TYPE zif_abapgit_persistence=>ty_type VALUE 'SETTINGS' ##NO_TEXT,
      c_type_repo       TYPE zif_abapgit_persistence=>ty_type VALUE 'REPO' ##NO_TEXT,
      c_type_background TYPE zif_abapgit_persistence=>ty_type VALUE 'BACKGROUND' ##NO_TEXT,
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
        VALUE(rt_content) TYPE zif_abapgit_persistence=>tt_content .
    METHODS list_by_type
      IMPORTING
        !iv_type          TYPE zif_abapgit_persistence=>ty_type
      RETURNING
        VALUE(rt_content) TYPE zif_abapgit_persistence=>tt_content .
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
  PRIVATE SECTION.

    CLASS-DATA go_db TYPE REF TO zcl_abapgit_persistence_db .

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

    ls_table-type  = iv_type.
    ls_table-value = iv_value.
    ls_table-data_str = iv_data.

    INSERT (c_tabname) FROM ls_table.                     "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD delete.

    lock( iv_type  = iv_type
          iv_value = iv_value ).

    DELETE FROM (c_tabname)
      WHERE type = iv_type
      AND value = iv_value.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'DB Delete failed' ).
    ENDIF.

  ENDMETHOD.


  METHOD get_instance.

    IF go_db IS NOT BOUND.
      CREATE OBJECT go_db.
    ENDIF.
    ro_db = go_db.

  ENDMETHOD.


  METHOD list.
    SELECT * FROM (c_tabname)
      INTO TABLE rt_content.                              "#EC CI_SUBRC
  ENDMETHOD.


  METHOD list_by_type.
    SELECT * FROM (c_tabname)
      INTO TABLE rt_content
      WHERE type = iv_type
      ORDER BY PRIMARY KEY.                               "#EC CI_SUBRC
  ENDMETHOD.


  METHOD lock.

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
      zcx_abapgit_exception=>raise( |Could not aquire lock { iv_type } { iv_value }| ).
    ENDIF.

* trigger dummy update task to automatically release locks at commit
    CALL FUNCTION 'BANK_OBJ_WORKL_RELEASE_LOCKS'
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
      AND value = iv_value.                               "#EC CI_SUBRC
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abapgit_not_found.
    ENDIF.

  ENDMETHOD.


  METHOD update.

    DATA lv_data LIKE iv_data.

    lv_data = validate_and_unprettify_xml( iv_data ).

    lock( iv_type  = iv_type
          iv_value = iv_value ).

    UPDATE (c_tabname) SET data_str = lv_data
      WHERE type  = iv_type
      AND   value = iv_value.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'DB update failed' ).
    ENDIF.

  ENDMETHOD.  "update


  METHOD validate_and_unprettify_xml.

    rv_xml = zcl_abapgit_xml_pretty=>print(
      iv_xml           = iv_xml
      iv_unpretty      = abap_true
      iv_ignore_errors = abap_false ).

  ENDMETHOD.  " validate_and_unprettify_xml
ENDCLASS.
