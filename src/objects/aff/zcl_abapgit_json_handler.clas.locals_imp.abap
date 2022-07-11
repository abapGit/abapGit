CLASS lcl_aff_filter DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_ajson_filter.
    METHODS constructor
      RAISING zcx_abapgit_ajson_error.
    TYPES:
      BEGIN OF ty_key_value,
        key   TYPE string,
        value TYPE string,
      END OF ty_key_value.

  PRIVATE SECTION.
    DATA mt_skip_paths TYPE STANDARD TABLE OF ty_key_value WITH KEY key.
ENDCLASS.

CLASS lcl_aff_filter IMPLEMENTATION.

  METHOD zif_abapgit_ajson_filter~keep_node.

    DATA lv_path TYPE string.
    DATA lv_line_exists TYPE abap_bool.

    lv_path = is_node-path && is_node-name.

    READ TABLE mt_skip_paths WITH KEY key = lv_path value = is_node-value TRANSPORTING NO FIELDS.
    IF boolc( sy-subrc = 0 ) = abap_true
      AND iv_visit = zif_abapgit_ajson_filter=>visit_type-value.
      rv_keep = abap_false.
      RETURN.
    ELSE.
      READ TABLE mt_skip_paths WITH KEY key = lv_path TRANSPORTING NO FIELDS.
      IF boolc( sy-subrc = 0 ) = abap_true
        AND iv_visit = zif_abapgit_ajson_filter=>visit_type-value.
        rv_keep = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    IF is_node-type = 'bool' AND is_node-value = 'false' AND iv_visit = zif_abapgit_ajson_filter=>visit_type-value.
      rv_keep = abap_false.
      RETURN.
    ENDIF.

    IF NOT ( ( iv_visit = zif_abapgit_ajson_filter=>visit_type-value AND is_node-value IS NOT INITIAL ) OR
         ( iv_visit <> zif_abapgit_ajson_filter=>visit_type-value AND is_node-children > 0 ) ).
      rv_keep = abap_false.
      RETURN.
    ENDIF.

    rv_keep = abap_true.

  ENDMETHOD.

  METHOD constructor.
    " extract annotations and build table for values to be skipped ( path/name | value )
    DATA lo_abap_language_pair TYPE ty_key_value.
    lo_abap_language_pair-key = `/header/abapLanguageVersion`.
    lo_abap_language_pair-value = 'X'.

    APPEND lo_abap_language_pair TO mt_skip_paths.
  ENDMETHOD.

ENDCLASS.
