CLASS zcl_abapgit_data_config DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_data_config .

    METHODS constructor .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_path TYPE string .
ENDCLASS.



CLASS ZCL_ABAPGIT_DATA_CONFIG IMPLEMENTATION.


  METHOD constructor.

    mv_path = zif_abapgit_data_config=>c_default_path.

  ENDMETHOD.


  METHOD zif_abapgit_data_config~add_config.

* todo, give exception if it already exists

  ENDMETHOD.


  METHOD zif_abapgit_data_config~from_json.
  ENDMETHOD.


  METHOD zif_abapgit_data_config~get_configs.
  ENDMETHOD.


  METHOD zif_abapgit_data_config~get_path.

    rv_path = mv_path.

  ENDMETHOD.


  METHOD zif_abapgit_data_config~remove_config.

* todo, give exception if it does not exist

  ENDMETHOD.


  METHOD zif_abapgit_data_config~set_path.

* todo, validate format

    mv_path = iv_path.

  ENDMETHOD.


  METHOD zif_abapgit_data_config~to_json.
* todo
  ENDMETHOD.


  METHOD zif_abapgit_data_config~update_config.
* todo
  ENDMETHOD.
ENDCLASS.
