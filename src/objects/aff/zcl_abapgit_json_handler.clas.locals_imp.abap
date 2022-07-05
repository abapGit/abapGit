*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_mapping DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_ajson_mapping.
ENDCLASS.

CLASS lcl_mapping IMPLEMENTATION.
  METHOD zif_abapgit_ajson_mapping~to_abap.
  ENDMETHOD.

  METHOD zif_abapgit_ajson_mapping~to_json.
    TYPES ty_token TYPE c LENGTH 255.
    DATA lt_tokens TYPE STANDARD TABLE OF ty_token.
    FIELD-SYMBOLS <lg_token> LIKE LINE OF lt_tokens.

    rv_result = iv_name.

    SPLIT rv_result AT `_` INTO TABLE lt_tokens.
    LOOP AT lt_tokens ASSIGNING <lg_token> FROM 2.
      TRANSLATE <lg_token>(1) TO UPPER CASE.
    ENDLOOP.
    CONCATENATE LINES OF lt_tokens INTO rv_result.

  ENDMETHOD.
ENDCLASS.
