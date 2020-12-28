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
    DATA mt_config TYPE zif_abapgit_data_config=>ty_config_tt .
ENDCLASS.



CLASS ZCL_ABAPGIT_DATA_CONFIG IMPLEMENTATION.


  METHOD constructor.

    mv_path = zif_abapgit_data_config=>c_default_path.

  ENDMETHOD.


  METHOD zif_abapgit_data_config~add_config.

    ASSERT NOT is_config-type IS INITIAL.
    ASSERT NOT is_config-name IS INITIAL.

    INSERT is_config INTO TABLE mt_config.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Already in table' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_data_config~from_json.
* todo
    ASSERT 0 = 1.
  ENDMETHOD.


  METHOD zif_abapgit_data_config~get_configs.
    rt_configs = mt_config.
  ENDMETHOD.


  METHOD zif_abapgit_data_config~get_path.

    rv_path = mv_path.

  ENDMETHOD.


  METHOD zif_abapgit_data_config~remove_config.

* todo, give exception if it does not exist

    DELETE mt_config WHERE name = is_config-name AND type = is_config-type.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Not found' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_data_config~set_path.

* todo, validate format

    mv_path = iv_path.

  ENDMETHOD.


  METHOD zif_abapgit_data_config~to_json.
* todo
    ASSERT 0 = 1.
  ENDMETHOD.


  METHOD zif_abapgit_data_config~update_config.

    zif_abapgit_data_config~remove_config( is_config ).
    zif_abapgit_data_config~add_config( is_config ).

  ENDMETHOD.
ENDCLASS.
