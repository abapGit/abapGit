CLASS lcl_aff_filter DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_ajson_filter.
    TYPES:
      BEGIN OF ty_path_value_pair,
        path  TYPE string,
        value TYPE string,
      END OF ty_path_value_pair,
      ty_skip_paths TYPE STANDARD TABLE OF ty_path_value_pair WITH KEY path.

    METHODS constructor
      IMPORTING iv_skip_paths TYPE ty_skip_paths OPTIONAL
      RAISING   zcx_abapgit_ajson_error.
  PRIVATE SECTION.
    DATA mt_skip_paths TYPE ty_skip_paths.
ENDCLASS.

CLASS lcl_aff_filter IMPLEMENTATION.

  METHOD zif_abapgit_ajson_filter~keep_node.

    DATA lv_path TYPE string.

    lv_path = is_node-path && is_node-name.

    READ TABLE mt_skip_paths WITH KEY path = lv_path value = is_node-value TRANSPORTING NO FIELDS.
    IF boolc( sy-subrc = 0 ) = abap_true
      AND iv_visit = zif_abapgit_ajson_filter=>visit_type-value.
      rv_keep = abap_false.
      RETURN.
    ELSE.
      READ TABLE mt_skip_paths WITH KEY path = lv_path TRANSPORTING NO FIELDS.
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

    " AFF: if INTEGER type is initial (0) then is will be skipped
    "      However, if type is $required, it should be serialized.
    IF NOT ( ( iv_visit = zif_abapgit_ajson_filter=>visit_type-value AND is_node-value IS NOT INITIAL ) OR
         ( iv_visit <> zif_abapgit_ajson_filter=>visit_type-value AND is_node-children > 0 ) ).
      rv_keep = abap_false.
      RETURN.
    ENDIF.

    rv_keep = abap_true.

  ENDMETHOD.

  METHOD constructor.
    " extract annotations and build table for values to be skipped ( path/name | value )
    DATA lo_abap_language_pair TYPE ty_path_value_pair.
    lo_abap_language_pair-path = `/header/abapLanguageVersion`.
    lo_abap_language_pair-value = 'standard'.

    APPEND lo_abap_language_pair TO mt_skip_paths.

    IF iv_skip_paths IS NOT INITIAL.
      APPEND LINES OF iv_skip_paths TO mt_skip_paths.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
