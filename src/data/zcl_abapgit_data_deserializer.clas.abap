CLASS zcl_abapgit_data_deserializer DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_data_factory .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_data_deserializer .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS read_json
      IMPORTING
        !is_file TYPE zif_abapgit_definitions=>ty_file
        !ir_data TYPE REF TO data
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_DATA_DESERIALIZER IMPLEMENTATION.


  METHOD read_json.

    DATA lo_ajson TYPE REF TO zcl_abapgit_ajson.
    DATA lx_ajson TYPE REF TO zcx_abapgit_ajson_error.
    FIELD-SYMBOLS <lg_tab> TYPE ANY TABLE.


    ASSIGN ir_data->* TO <lg_tab>.

    TRY.
        lo_ajson = zcl_abapgit_ajson=>parse( zcl_abapgit_convert=>xstring_to_string_utf8( is_file-data ) ).
        lo_ajson->zif_abapgit_ajson_reader~to_abap( IMPORTING ev_container = <lg_tab> ).
      CATCH zcx_abapgit_ajson_error INTO lx_ajson.
        zcx_abapgit_exception=>raise( lx_ajson->get_text( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_data_deserializer~actualize.

* todo, this method will update the database

  ENDMETHOD.


  METHOD zif_abapgit_data_deserializer~deserialize.

* this method does not persist any changes to the database

    DATA lt_configs TYPE zif_abapgit_data_config=>ty_config_tt.
    DATA ls_config LIKE LINE OF lt_configs.
    DATA lr_data  TYPE REF TO data.
    DATA ls_file LIKE LINE OF it_files.


    lt_configs = ii_config->get_configs( ).

    LOOP AT lt_configs INTO ls_config.

      lr_data = zcl_abapgit_data_utils=>build_table_itab( ls_config-name ).

      READ TABLE it_files INTO ls_file WITH KEY
        path = zif_abapgit_data_config=>c_default_path
        filename = zcl_abapgit_data_utils=>build_filename( ls_config ).
      IF sy-subrc = 0.
        read_json(
          ir_data = lr_data
          is_file = ls_file ).
      ENDIF.

* todo

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
