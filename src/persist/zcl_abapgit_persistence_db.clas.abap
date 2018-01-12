CLASS zcl_abapgit_persistence_db DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS:
      c_tabname TYPE tabname VALUE 'ZABAPGIT',
      c_lock    TYPE viewname VALUE 'EZABAPGIT'.

    TYPES: ty_type  TYPE c LENGTH 12.
    TYPES: ty_value TYPE c LENGTH 12.

    TYPES: BEGIN OF ty_content,
             type     TYPE ty_type,
             value    TYPE ty_value,
             data_str TYPE string,
           END OF ty_content,
           tt_content TYPE SORTED TABLE OF ty_content WITH UNIQUE KEY type value.

    CONSTANTS:
      c_type_settings   TYPE ty_type VALUE 'SETTINGS',
      c_type_repo       TYPE ty_type VALUE 'REPO',
      c_type_background TYPE ty_type VALUE 'BACKGROUND',
      c_type_user       TYPE ty_type VALUE 'USER'.

    METHODS:
      list_by_type
        IMPORTING iv_type           TYPE ty_type
        RETURNING VALUE(rt_content) TYPE tt_content,
      list
        RETURNING VALUE(rt_content) TYPE tt_content,
      add
        IMPORTING iv_type  TYPE ty_type
                  iv_value TYPE ty_content-value
                  iv_data  TYPE ty_content-data_str
        RAISING   zcx_abapgit_exception,
      delete
        IMPORTING iv_type  TYPE ty_type
                  iv_value TYPE ty_content-value
        RAISING   zcx_abapgit_exception,
      update
        IMPORTING iv_type  TYPE ty_type
                  iv_value TYPE ty_content-value
                  iv_data  TYPE ty_content-data_str
        RAISING   zcx_abapgit_exception,
      modify
        IMPORTING iv_type  TYPE ty_type
                  iv_value TYPE ty_content-value
                  iv_data  TYPE ty_content-data_str
        RAISING   zcx_abapgit_exception,
      read
        IMPORTING iv_type        TYPE ty_type
                  iv_value       TYPE ty_content-value
        RETURNING VALUE(rv_data) TYPE ty_content-data_str
        RAISING   zcx_abapgit_not_found,
      lock
        IMPORTING iv_mode  TYPE enqmode DEFAULT 'E'
                  iv_type  TYPE ty_type
                  iv_value TYPE ty_content-value
        RAISING   zcx_abapgit_exception.

  PRIVATE SECTION.
    METHODS: validate_and_unprettify_xml
      IMPORTING iv_xml        TYPE string
      RETURNING VALUE(rv_xml) TYPE string
      RAISING   zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_PERSISTENCE_DB IMPLEMENTATION.


  METHOD add.

* todo, change instantiation back to private? make sure this class is a singleton?

    DATA ls_table TYPE ty_content.

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


  METHOD list.
    SELECT * FROM (c_tabname)
      INTO TABLE rt_content.                              "#EC CI_SUBRC
  ENDMETHOD.


  METHOD list_by_type.
    SELECT * FROM (c_tabname)
      INTO TABLE rt_content
      WHERE type = iv_type.                               "#EC CI_SUBRC
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

    DATA: ls_content TYPE ty_content.

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
