CLASS zcl_abapgit_object_samc DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_object_saxx_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        is_item        TYPE zif_abapgit_definitions=>ty_item
        iv_language    TYPE spras
        io_files       TYPE REF TO zcl_abapgit_objects_files OPTIONAL
        io_i18n_params TYPE REF TO zcl_abapgit_i18n_params OPTIONAL
      RAISING
        zcx_abapgit_type_not_supported.
  PROTECTED SECTION.

    METHODS get_data_class_name
        REDEFINITION .
    METHODS get_data_structure_name
        REDEFINITION .
    METHODS get_persistence_class_name
        REDEFINITION .
    METHODS get_lock_object
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_object_samc IMPLEMENTATION.

  METHOD constructor.

    super->constructor(
        is_item        = is_item
        iv_language    = iv_language
        io_files       = io_files
        io_i18n_params = io_i18n_params ).

    create_channel_objects( ).

  ENDMETHOD.


  METHOD get_data_class_name.

    rv_data_class_name = 'CL_AMC_APPLICATION_OBJ_DATA'.

  ENDMETHOD.


  METHOD get_data_structure_name.

    rv_data_structure_name = 'AMC_APPLICATION_COMPLETE'.

  ENDMETHOD.


  METHOD get_persistence_class_name.

    rv_persistence_class_name = 'CL_AMC_APPLICATION_OBJ_PERS'.

  ENDMETHOD.


  METHOD get_lock_object.

    rv_lock_object = 'E_AMC_APPL'.

  ENDMETHOD.

ENDCLASS.
