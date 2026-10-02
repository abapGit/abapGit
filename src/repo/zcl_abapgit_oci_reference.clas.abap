CLASS zcl_abapgit_oci_reference DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_reference,
        registry   TYPE string,
        repository TYPE string,
        reference  TYPE string,
        is_digest  TYPE abap_bool,
        canonical  TYPE string,
      END OF ty_reference .

    CLASS-METHODS parse
      IMPORTING
        iv_reference        TYPE string
      RETURNING
        VALUE(rs_reference) TYPE ty_reference
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS build
      IMPORTING
        iv_registry         TYPE string
        iv_repository       TYPE string
        iv_reference        TYPE string
      RETURNING
        VALUE(rv_reference) TYPE string
      RAISING
        zcx_abapgit_exception .

  PRIVATE SECTION.
    CLASS-METHODS validate_registry
      IMPORTING
        iv_registry        TYPE string
      RETURNING
        VALUE(rv_registry) TYPE string
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_oci_reference IMPLEMENTATION.

  METHOD parse.

    DATA: lv_rest           TYPE string,
          lv_name           TYPE string,
          lv_leaf            TYPE string,
          lv_tag             TYPE string,
          lv_registry        TYPE string,
          lv_repository      TYPE string,
          lv_separator       TYPE string,
          lv_at_offset       TYPE i,
          lv_tail_offset     TYPE i,
          lt_components      TYPE string_table,
          lt_leaf            TYPE string_table,
          lv_component       TYPE string,
          lv_tail            TYPE string,
          lv_is_digest       TYPE abap_bool.

    FIELD-SYMBOLS <lv_component> LIKE LINE OF lt_components.

    IF strlen( iv_reference ) < 7.
      zcx_abapgit_exception=>raise( 'OCI reference must use the oci://registry/repository:tag form' ).
    ENDIF.
    IF iv_reference+0(6) <> 'oci://'.
      zcx_abapgit_exception=>raise( 'OCI reference must use the oci://registry/repository:tag form' ).
    ENDIF.

    IF iv_reference CS '?' OR iv_reference CS '#' OR iv_reference CS '\' OR
       iv_reference CS '%' OR iv_reference CS ` `.
      zcx_abapgit_exception=>raise(
        'OCI reference must not contain query, fragment, escape, or whitespace characters' ).
    ENDIF.

    lv_rest = iv_reference+6.
    IF lv_rest IS INITIAL.
      zcx_abapgit_exception=>raise( 'OCI reference is missing its registry and repository' ).
    ENDIF.

    FIND FIRST OCCURRENCE OF '@' IN lv_rest MATCH OFFSET lv_at_offset.
    IF sy-subrc = 0.
      lv_tail_offset = lv_at_offset + 1.
      IF lv_tail_offset >= strlen( lv_rest ).
        zcx_abapgit_exception=>raise( 'OCI digest reference is empty' ).
      ENDIF.
      lv_tail = lv_rest+lv_tail_offset.
      FIND FIRST OCCURRENCE OF '@' IN lv_tail.
      IF sy-subrc = 0.
        zcx_abapgit_exception=>raise( 'OCI reference contains multiple @ separators' ).
      ENDIF.

      lv_name = lv_rest(lv_at_offset).
      lv_tag = lv_tail.
      FIND REGEX '^sha256:[0-9a-f]{64}$' IN lv_tag.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise(
          'OCI digest must be sha256 followed by 64 lowercase hexadecimal characters' ).
      ENDIF.
      lv_is_digest = abap_true.
      lv_separator = '@'.
    ELSE.
      lv_name = lv_rest.
      lv_is_digest = abap_false.
      lv_separator = ':'.
    ENDIF.

    SPLIT lv_name AT '/' INTO TABLE lt_components.
    IF lines( lt_components ) < 2.
      zcx_abapgit_exception=>raise( 'OCI reference must include a registry and repository path' ).
    ENDIF.

    READ TABLE lt_components INDEX 1 INTO lv_registry.
    rs_reference-registry = validate_registry( lv_registry ).

    IF lv_is_digest = abap_false.
      READ TABLE lt_components INDEX lines( lt_components ) INTO lv_leaf.
      SPLIT lv_leaf AT ':' INTO TABLE lt_leaf.
      IF lines( lt_leaf ) <> 2.
        zcx_abapgit_exception=>raise( 'OCI tag reference is required; latest is not implicit' ).
      ENDIF.

      READ TABLE lt_leaf INDEX 1 INTO lv_component.
      IF lv_component IS INITIAL.
        zcx_abapgit_exception=>raise( 'OCI repository name is empty' ).
      ENDIF.
      MODIFY lt_components FROM lv_component INDEX lines( lt_components ).

      READ TABLE lt_leaf INDEX 2 INTO lv_tag.
      FIND REGEX '^[A-Za-z0-9_][A-Za-z0-9_.-]{0,127}$' IN lv_tag.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'OCI tag is malformed or longer than 128 characters' ).
      ENDIF.
    ENDIF.

    LOOP AT lt_components ASSIGNING <lv_component> FROM 2.
      IF <lv_component> IS INITIAL.
        zcx_abapgit_exception=>raise( 'OCI repository contains an empty path segment' ).
      ENDIF.
      FIND REGEX '^[a-z0-9]+([._-][a-z0-9]+)*$' IN <lv_component>.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise(
          'OCI repository path segments must use lowercase alphanumeric names and separators' ).
      ENDIF.
      IF lv_repository IS INITIAL.
        lv_repository = <lv_component>.
      ELSE.
        CONCATENATE lv_repository <lv_component> INTO lv_repository SEPARATED BY '/'.
      ENDIF.
    ENDLOOP.

    IF lv_repository IS INITIAL.
      zcx_abapgit_exception=>raise( 'OCI repository path is empty' ).
    ENDIF.

    rs_reference-repository = lv_repository.
    rs_reference-reference = lv_tag.
    rs_reference-is_digest = lv_is_digest.
    rs_reference-canonical = |oci://{ rs_reference-registry }/{ lv_repository }{ lv_separator }{ lv_tag }|.

  ENDMETHOD.


  METHOD build.

    DATA: lv_separator TYPE string,
          ls_reference TYPE ty_reference.

    IF iv_reference CP 'sha256:*'.
      lv_separator = '@'.
    ELSE.
      lv_separator = ':'.
    ENDIF.

    rv_reference = |oci://{ iv_registry }/{ iv_repository }{ lv_separator }{ iv_reference }|.
    ls_reference = parse( rv_reference ).
    rv_reference = ls_reference-canonical.

  ENDMETHOD.


  METHOD validate_registry.

    DATA: lv_host       TYPE string,
          lv_port       TYPE string,
          lv_port_value TYPE i,
          lv_host_last  TYPE i,
          lt_parts      TYPE string_table.

    IF iv_registry IS INITIAL.
      zcx_abapgit_exception=>raise( 'OCI registry hostname is missing' ).
    ENDIF.

    SPLIT iv_registry AT ':' INTO TABLE lt_parts.
    IF lines( lt_parts ) = 1.
      lv_host = iv_registry.
    ELSEIF lines( lt_parts ) = 2.
      READ TABLE lt_parts INDEX 1 INTO lv_host.
      READ TABLE lt_parts INDEX 2 INTO lv_port.
      IF lv_port IS INITIAL OR strlen( lv_port ) > 5.
        zcx_abapgit_exception=>raise( 'OCI registry port is malformed' ).
      ENDIF.
      FIND REGEX '^[0-9]+$' IN lv_port.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'OCI registry port must be a decimal number' ).
      ENDIF.
      lv_port_value = lv_port.
      IF lv_port_value < 1 OR lv_port_value > 65535.
        zcx_abapgit_exception=>raise( 'OCI registry port is outside the TCP port range' ).
      ENDIF.
    ELSE.
      zcx_abapgit_exception=>raise( 'OCI registry hostname or port is malformed' ).
    ENDIF.

    IF lv_host IS INITIAL.
      zcx_abapgit_exception=>raise( 'OCI registry hostname is malformed' ).
    ENDIF.

    FIND REGEX '^[A-Za-z0-9.-]+$' IN lv_host.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'OCI registry hostname is malformed' ).
    ENDIF.

    IF lv_host+0(1) = '.' OR lv_host+0(1) = '-'.
      zcx_abapgit_exception=>raise( 'OCI registry hostname is malformed' ).
    ENDIF.

    lv_host_last = strlen( lv_host ) - 1.
    IF lv_host+lv_host_last(1) = '.' OR lv_host+lv_host_last(1) = '-'.
      zcx_abapgit_exception=>raise( 'OCI registry hostname is malformed' ).
    ENDIF.

    IF lv_host CS '..' OR lv_host CS '.-' OR lv_host CS '-.'.
      zcx_abapgit_exception=>raise( 'OCI registry hostname is malformed' ).
    ENDIF.

    TRANSLATE lv_host TO LOWER CASE.
    IF lv_port IS INITIAL.
      rv_registry = lv_host.
    ELSE.
      rv_registry = |{ lv_host }:{ lv_port }|.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
