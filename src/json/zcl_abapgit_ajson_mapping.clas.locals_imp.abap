CLASS lcl_mapping_fields IMPLEMENTATION.


  METHOD constructor.

    DATA ls_mapping_field LIKE LINE OF mt_mapping_fields.

    LOOP AT it_mapping_fields INTO ls_mapping_field.
      ls_mapping_field-abap = to_upper( ls_mapping_field-abap ).
      INSERT ls_mapping_field INTO TABLE mt_mapping_fields.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_ajson_mapping~to_abap.

    DATA ls_mapping_field LIKE LINE OF mt_mapping_fields.

    READ TABLE mt_mapping_fields INTO ls_mapping_field
      WITH KEY json COMPONENTS json = iv_name.
    IF sy-subrc = 0.
      rv_result = ls_mapping_field-abap.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_ajson_mapping~to_json.

    DATA lv_field TYPE string.
    DATA ls_mapping_field LIKE LINE OF mt_mapping_fields.

    lv_field = to_upper( iv_name ).

    READ TABLE mt_mapping_fields INTO ls_mapping_field
      WITH KEY abap COMPONENTS abap = lv_field.
    IF sy-subrc = 0.
      rv_result = ls_mapping_field-json.
    ENDIF.

  ENDMETHOD.


ENDCLASS.


CLASS lcl_mapping_to_upper IMPLEMENTATION.


  METHOD constructor.

    mi_mapping_fields = zcl_abapgit_ajson_mapping=>create_field_mapping( it_mapping_fields ).

  ENDMETHOD.


  METHOD zif_abapgit_ajson_mapping~to_abap.

    rv_result = mi_mapping_fields->to_abap( iv_path = iv_path
                                            iv_name = iv_name ).

  ENDMETHOD.


  METHOD zif_abapgit_ajson_mapping~to_json.

    rv_result = mi_mapping_fields->to_json( iv_path = iv_path
                                            iv_name = iv_name ).

    IF rv_result IS NOT INITIAL. " Mapping found
      RETURN.
    ENDIF.

    rv_result = to_upper( iv_name ).

  ENDMETHOD.


ENDCLASS.


CLASS lcl_mapping_to_lower IMPLEMENTATION.


  METHOD constructor.

    mi_mapping_fields = zcl_abapgit_ajson_mapping=>create_field_mapping( it_mapping_fields ).

  ENDMETHOD.


  METHOD zif_abapgit_ajson_mapping~to_abap.

    rv_result = mi_mapping_fields->to_abap( iv_path = iv_path
                                            iv_name = iv_name ).

  ENDMETHOD.


  METHOD zif_abapgit_ajson_mapping~to_json.

    rv_result = mi_mapping_fields->to_json( iv_path = iv_path
                                            iv_name = iv_name ).

    IF rv_result IS NOT INITIAL. " Mapping found
      RETURN.
    ENDIF.

    rv_result = to_lower( iv_name ).

  ENDMETHOD.


ENDCLASS.


CLASS lcl_mapping_camel IMPLEMENTATION.


  METHOD constructor.

    mi_mapping_fields   = zcl_abapgit_ajson_mapping=>create_field_mapping( it_mapping_fields ).
    mv_first_json_upper = iv_first_json_upper.

  ENDMETHOD.


  METHOD zif_abapgit_ajson_mapping~to_abap.

    rv_result = mi_mapping_fields->to_abap( iv_path = iv_path
                                            iv_name = iv_name ).

    IF rv_result IS NOT INITIAL. " Mapping found
      RETURN.
    ENDIF.

    rv_result = iv_name.

    REPLACE ALL OCCURRENCES OF REGEX `([a-z])([A-Z])` IN rv_result WITH `$1_$2`.

  ENDMETHOD.


  METHOD zif_abapgit_ajson_mapping~to_json.

    TYPES ty_token TYPE c LENGTH 255.
    DATA lt_tokens TYPE STANDARD TABLE OF ty_token.
    DATA lv_from TYPE i.
    FIELD-SYMBOLS <token> LIKE LINE OF lt_tokens.

    rv_result = mi_mapping_fields->to_json( iv_path = iv_path
                                            iv_name = iv_name ).

    IF rv_result IS NOT INITIAL. " Mapping found
      RETURN.
    ENDIF.

    rv_result = iv_name.

    REPLACE ALL OCCURRENCES OF `__` IN rv_result WITH `*`.

    TRANSLATE rv_result TO LOWER CASE.
    TRANSLATE rv_result USING `/_:_~_`.

    IF mv_first_json_upper = abap_true.
      lv_from = 1.
    ELSE.
      lv_from = 2.
    ENDIF.

    SPLIT rv_result AT `_` INTO TABLE lt_tokens.
    LOOP AT lt_tokens ASSIGNING <token> FROM lv_from.
      TRANSLATE <token>(1) TO UPPER CASE.
    ENDLOOP.

    CONCATENATE LINES OF lt_tokens INTO rv_result.
    REPLACE ALL OCCURRENCES OF `*` IN rv_result WITH `_`.

  ENDMETHOD.


ENDCLASS.
