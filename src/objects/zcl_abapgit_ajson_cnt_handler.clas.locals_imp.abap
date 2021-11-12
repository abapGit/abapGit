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
    FIELD-SYMBOLS <token> LIKE LINE OF lt_tokens.

    rv_result = iv_name.
    IF iv_path = '/' AND iv_name = 'schema'.
      rv_result = '$schema'.
    ELSE.
      SPLIT rv_result AT `_` INTO TABLE lt_tokens.
      LOOP AT lt_tokens ASSIGNING <token> FROM 2.
        TRANSLATE <token>(1) TO UPPER CASE.
      ENDLOOP.
      CONCATENATE LINES OF lt_tokens INTO rv_result.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
