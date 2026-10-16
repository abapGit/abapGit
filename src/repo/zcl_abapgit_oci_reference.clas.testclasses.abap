CLASS ltcl_oci_reference DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS parse_tag_with_registry_port FOR TESTING RAISING zcx_abapgit_exception.
    METHODS parse_digest FOR TESTING RAISING zcx_abapgit_exception.
    METHODS reject_implicit_tag FOR TESTING RAISING zcx_abapgit_exception.
    METHODS reject_user_information FOR TESTING RAISING zcx_abapgit_exception.
    METHODS reject_query_and_fragment FOR TESTING RAISING zcx_abapgit_exception.
    METHODS reject_malformed_paths FOR TESTING RAISING zcx_abapgit_exception.
    METHODS assert_rejected IMPORTING iv_reference TYPE string.
ENDCLASS.



CLASS ltcl_oci_reference IMPLEMENTATION.

  METHOD parse_tag_with_registry_port.

    DATA ls_reference TYPE zcl_abapgit_oci_reference=>ty_reference.

    ls_reference = zcl_abapgit_oci_reference=>parse( 'oci://Registry.Example.com:5000/team/library:1.2.3' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_reference-registry
      exp = 'registry.example.com:5000' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_reference-repository
      exp = 'team/library' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_reference-reference
      exp = '1.2.3' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_reference-canonical
      exp = 'oci://registry.example.com:5000/team/library:1.2.3' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_reference-is_digest
      exp = abap_false ).

  ENDMETHOD.


  METHOD parse_digest.

    DATA: ls_reference TYPE zcl_abapgit_oci_reference=>ty_reference,
          lv_digest    TYPE string.

    lv_digest = 'sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'.
    ls_reference = zcl_abapgit_oci_reference=>parse( |oci://registry.example.com/team/library@{ lv_digest }| ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_reference-reference
      exp = lv_digest ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_reference-is_digest
      exp = abap_true ).

  ENDMETHOD.


  METHOD reject_implicit_tag.

    assert_rejected( 'oci://registry.example.com/team/library' ).

  ENDMETHOD.


  METHOD reject_user_information.

    assert_rejected( 'oci://user:secret@registry.example.com/team/library:1' ).

  ENDMETHOD.


  METHOD reject_query_and_fragment.

    assert_rejected( 'oci://registry.example.com/team/library:1?x=1' ).
    assert_rejected( 'oci://registry.example.com/team/library:1#fragment' ).

  ENDMETHOD.


  METHOD reject_malformed_paths.

    assert_rejected( 'oci://registry.example.com/Team/library:1' ).
    assert_rejected( 'oci://registry.example.com/team//library:1' ).
    assert_rejected( 'oci://registry.example.com/team/library@sha256:ABCD' ).

  ENDMETHOD.


  METHOD assert_rejected.

    TRY.
        zcl_abapgit_oci_reference=>parse( iv_reference ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception.
        RETURN.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
