CLASS zcl_abapgit_aff_registry DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_aff_registry.

    CONSTANTS c_aff_feature TYPE string VALUE 'AFF'.

    METHODS constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_registry_entry,
        obj_type     TYPE tadir-object,
        experimental TYPE abap_bool,
      END OF ty_registry_entry.

    CLASS-DATA:
      gt_registry TYPE HASHED TABLE OF ty_registry_entry WITH UNIQUE KEY obj_type.

    DATA mv_aff_enabled TYPE abap_bool.

    CLASS-METHODS initialize_registry_table.

    CLASS-METHODS:
      register
        IMPORTING
          iv_obj_type     TYPE tadir-object
          iv_experimental TYPE abap_bool DEFAULT abap_false.

ENDCLASS.



CLASS zcl_abapgit_aff_registry IMPLEMENTATION.


  METHOD constructor.
    mv_aff_enabled = zcl_abapgit_feature=>is_enabled( c_aff_feature ).
  ENDMETHOD.


  METHOD initialize_registry_table.
    register( 'CHKC' ).
    register( 'CHKO' ).
    register( 'CHKV' ).
    register( 'DRTY' ).
    register( 'DTEB' ).
    register( 'EVTB' ).
    register( 'EEEC' ).
    register( 'GSMP' ).
    register( iv_obj_type     = 'INTF'
              iv_experimental = abap_true ).
    register( 'SMBC' ).
    register( 'NONT' ).
    register( 'RONT' ).
    register( 'UIPG' ).
    register( 'UIST' ).
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
    ELSEIF sy-subrc = 0 AND mv_aff_enabled = abap_true.
      rv_result = abap_true.
    ELSE.
      rv_result = abap_false.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
