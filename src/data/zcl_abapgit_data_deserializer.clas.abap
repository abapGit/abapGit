CLASS zcl_abapgit_data_deserializer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_data_deserializer .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_DATA_DESERIALIZER IMPLEMENTATION.


  METHOD zif_abapgit_data_deserializer~deserialize.

    DATA lt_configs TYPE zif_abapgit_data_config=>ty_config_tt.
    DATA ls_config LIKE LINE OF lt_configs.
    DATA lr_data  TYPE REF TO data.
    FIELD-SYMBOLS <lg_tab> TYPE ANY TABLE.


    lt_configs = ii_config->get_configs( ).

    LOOP AT lt_configs INTO ls_config.
* todo

      lr_data = zcl_abapgit_data_utils=>build_table_itab( ls_config-name ).
      ASSIGN lr_data->* TO <lg_tab>.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
