CLASS zcl_abapgit_apack_provider_prx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_serializable_object.
    INTERFACES if_apack_manifest.

    ALIASES descriptor FOR if_apack_manifest~descriptor.

    CLASS-METHODS get_provider
      IMPORTING
        iv_clsname         TYPE seoclsname
      RETURNING
        VALUE(ro_provider) TYPE REF TO zcl_abapgit_apack_provider_prx.

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS copy_manifest_descriptor
      IMPORTING
        !io_manifest_provider TYPE REF TO object.

ENDCLASS.



CLASS zcl_abapgit_apack_provider_prx IMPLEMENTATION.


  METHOD copy_manifest_descriptor.

    DATA: ls_my_manifest_wo_deps TYPE zif_abapgit_apack_definitions=>ty_descriptor_wo_dependencies,
          ls_my_dependency       TYPE zif_abapgit_apack_definitions=>ty_dependency,
          ls_dependency          TYPE if_apack_manifest=>ty_dependency,
          lv_descriptor_cust     TYPE string,
          lv_descriptor_sap      TYPE string.

    FIELD-SYMBOLS: <lg_descriptor>   TYPE any,
                   <lt_dependencies> TYPE ANY TABLE,
                   <lg_dependency>   TYPE any.

    lv_descriptor_cust = zif_abapgit_apack_definitions=>c_apack_interface_cust && '~DESCRIPTOR'.
    lv_descriptor_sap  = zif_abapgit_apack_definitions=>c_apack_interface_sap && '~DESCRIPTOR'.

    ASSIGN io_manifest_provider->(lv_descriptor_cust) TO <lg_descriptor>.
    IF <lg_descriptor> IS NOT ASSIGNED.
      ASSIGN io_manifest_provider->(lv_descriptor_sap) TO <lg_descriptor>.
    ENDIF.
    IF <lg_descriptor> IS ASSIGNED.
      " A little more complex than a normal MOVE-CORRSPONDING
      " to avoid dumps in case of future updates to the dependencies table structure
      ASSIGN COMPONENT 'DEPENDENCIES' OF STRUCTURE <lg_descriptor> TO <lt_dependencies>.
      IF <lt_dependencies> IS ASSIGNED.
        LOOP AT <lt_dependencies> ASSIGNING <lg_dependency>.
          MOVE-CORRESPONDING <lg_dependency> TO ls_my_dependency.
          MOVE-CORRESPONDING ls_my_dependency TO ls_dependency.
          INSERT ls_dependency INTO TABLE descriptor-dependencies.
        ENDLOOP.
        MOVE-CORRESPONDING <lg_descriptor> TO ls_my_manifest_wo_deps.
        MOVE-CORRESPONDING ls_my_manifest_wo_deps TO descriptor.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_provider.

    DATA: lo_manifest_provider TYPE REF TO object.

    TRY.
        CREATE OBJECT lo_manifest_provider TYPE (iv_clsname).
      CATCH cx_sy_create_object_error.
        RETURN.
    ENDTRY.

    CREATE OBJECT ro_provider.
    ro_provider->copy_manifest_descriptor( lo_manifest_provider ).

  ENDMETHOD.


ENDCLASS.
