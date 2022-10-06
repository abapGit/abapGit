CLASS zcl_abapgit_aff_registry DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_aff_registry.
    CLASS-METHODS:
      class_constructor.
    METHODS:
      constructor
        IMPORTING
          settings TYPE REF TO zcl_abapgit_settings OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_registry_entry,
        obj_type     TYPE tadir-object,
        experimental TYPE abap_bool,
      END OF ty_registry_entry.

    CLASS-DATA:
      registry TYPE HASHED TABLE OF ty_registry_entry WITH UNIQUE KEY obj_type.

    DATA:
      settings TYPE REF TO zcl_abapgit_settings.

    CLASS-METHODS:
      register
        IMPORTING
          obj_type     TYPE tadir-object
          experimental TYPE abap_bool DEFAULT abap_false.

ENDCLASS.



CLASS zcl_abapgit_aff_registry IMPLEMENTATION.

  METHOD class_constructor.
    register( obj_type = 'CHKC' ).
    register( obj_type = 'CHKO' ).
    register( obj_type = 'CHKV' ).
    register( obj_type = 'EVTB' ).
    register( obj_type = 'INTF' experimental = abap_true ).
  ENDMETHOD.

  METHOD constructor.
    IF settings IS SUPPLIED.
      me->settings = settings.
    ELSE.
      me->settings = zcl_abapgit_persist_factory=>get_settings( )->read( ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_abapgit_aff_registry~is_supported_object_type.
    DATA registry_entry TYPE ty_registry_entry.

    READ TABLE registry WITH TABLE KEY obj_type = obj_type INTO registry_entry.
    IF sy-subrc = 0 AND ( registry_entry-experimental = abap_false OR
                          settings->get_experimental_features( ) = abap_true ).
      result = abap_true.
    ELSE.
      result = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD register.
    DATA registry_entry TYPE ty_registry_entry.

    registry_entry-obj_type = obj_type.
    registry_entry-experimental = experimental.
    INSERT registry_entry INTO TABLE registry.
  ENDMETHOD.

ENDCLASS.
