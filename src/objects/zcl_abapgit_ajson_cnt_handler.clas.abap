CLASS zcl_abapgit_ajson_cnt_handler DEFINITION
  public
  final
  create public .

  public section.

    "! Serializes data to xstring. Type of data is specified in the
    "! implementing class.
    "!
    "! @parameter data | data to be serialized
    "! @parameter result | serialized data
    methods serialize
      importing data          type data
      returning value(result) type xstring
      raising   cx_aff_root.

    "! Deserializes xstring into data. The type of data is specified in
    "! the implementing class
    "!
    "! @parameter content | xstring to be deserialized
    "! @parameter data | data of the xstring
    methods deserialize
      importing content type xstring
      exporting data    type data
      raising   cx_aff_root.

  protected section.

  private section.

    methods handle_exception
      importing exception type ref to cx_root
      raising   cx_aff_root.

ENDCLASS.



CLASS ZCL_ABAPGIT_AJSON_CNT_HANDLER IMPLEMENTATION.


method handle_exception.
    raise exception type cx_aff_without_message
      exporting
        previous = exception.
  endmethod.


  method deserialize.

    clear data.

    try.
        data(lo_ajson) = zcl_abapgit_ajson=>parse( zcl_abapgit_convert=>xstring_to_string_utf8( content ) ).

        lo_ajson->zif_abapgit_ajson~to_abap( importing ev_container = data ).

      catch zcx_abapgit_ajson_error  into data(ajson_error) .
        me->handle_exception( ajson_error ).
    endtry.

  endmethod.


  method serialize.
    data st_source type abap_trans_srcbind_tab.
    field-symbols: <st_source> like line of st_source.

    append initial line to st_source assigning <st_source>.
    get reference of data into <st_source>-value.

    try.
        data(li_ajson) = zcl_abapgit_ajson=>create_empty( ii_custom_mapping = new lcl_mapping( ) ).
        li_ajson->keep_item_order( ).
        li_ajson->set(
          iv_path = '/'
          iv_val  = data ).
        data(json) = li_ajson->stringify( 2 ).

*        result =  cl_abap_conv_codepage=>create_out(  )->convert( json ).
        result = zcl_abapgit_convert=>string_to_xstring_utf8( iv_string = json ).

      catch zcx_abapgit_ajson_error  into data(ajson_error) .
        me->handle_exception( ajson_error ).
    endtry.
  endmethod.
ENDCLASS.
