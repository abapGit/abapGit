*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
class lcl_mapping definition.
  public section.
    interfaces zif_abapgit_ajson_mapping.
endclass.

class lcl_mapping implementation.
  method zif_abapgit_ajson_mapping~to_abap.
  endmethod.

  method zif_abapgit_ajson_mapping~to_json.
    types ty_token type c length 255.
    data lt_tokens type standard table of ty_token.
    field-symbols <token> like line of lt_tokens.

    rv_result = iv_name.
    if iv_path = '/' and iv_name = 'schema'.
      rv_result = '$schema'.
    else.
      split rv_result at `_` into table lt_tokens.
      loop at lt_tokens assigning <token> from 2.
        translate <token>(1) to upper case.
      endloop.
      concatenate lines of lt_tokens into rv_result.
    endif.
  endmethod.
endclass.
