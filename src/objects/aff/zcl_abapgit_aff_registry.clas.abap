CLASS zcl_abapgit_aff_registry DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_aff_registry.

    METHODS:
      constructor
        IMPORTING
          io_settings TYPE REF TO zcl_abapgit_settings OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_registry_entry,
        obj_type     TYPE tadir-object,
        experimental TYPE abap_bool,
      END OF ty_registry_entry.

    CLASS-DATA:
      gt_registry TYPE HASHED TABLE OF ty_registry_entry WITH UNIQUE KEY obj_type.

    DATA:
      mo_settings TYPE REF TO zcl_abapgit_settings.
    METHODS initialize_registry_table.

    CLASS-METHODS:
      register
        IMPORTING
          iv_obj_type     TYPE tadir-object
          iv_experimental TYPE abap_bool DEFAULT abap_false.

ENDCLASS.



CLASS zcl_abapgit_aff_registry IMPLEMENTATION.


  METHOD constructor.
    IF io_settings IS SUPPLIED.
      mo_settings = io_settings.
    ELSE.
      mo_settings = zcl_abapgit_persist_factory=>get_settings( )->read( ).
    ENDIF.
  ENDMETHOD.


  METHOD initialize_registry_table.
    register( iv_obj_type = 'CHKC' ).
    register( iv_obj_type = 'CHKO' ).
    register( iv_obj_type = 'CHKV' ).
    register( iv_obj_type = 'EVTB' ).
    register( iv_obj_type = 'GSMP' ).
    register( iv_obj_type     = 'INTF'
              iv_experimental = abap_true ).
    register( iv_obj_type = 'SMBC' ).
    register( iv_obj_type = 'NONT' ).
    register( iv_obj_type = 'RONT' ).
  ENDMETHOD.


  METHOD register.
    DATA ls_registry_entry TYPE ty_registry_entry.

    ls_registry_entry-obj_type = iv_obj_type.
    ls_registry_entry-experimental = iv_experimental.
    INSERT ls_registry_entry INTO TABLE gt_registry.
  ENDMETHOD.


  METHOD zif_abapgit_aff_registry~is_supported_object_type.

    DATA ls_registry_entry TYPE ty_registry_entry.

    IF gt_registry IS INITIAL.
      initialize_registry_table( ).
    ENDIF.

    READ TABLE gt_registry WITH TABLE KEY obj_type = iv_obj_type INTO ls_registry_entry.
    IF sy-subrc = 0 AND ls_registry_entry-experimental = abap_false.
      rv_result = abap_true.
    ELSEIF sy-subrc = 0 AND mo_settings->get_experimental_features( ) = abap_true.
      rv_result = abap_true.
    ELSE.
      rv_result = abap_false.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
