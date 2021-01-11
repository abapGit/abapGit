*"* use this source file for your ABAP unit test classes
CLASS ltcl_apack_manifest_reader DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS:
      setup RAISING cx_static_check,
      manifest_descriptor FOR TESTING RAISING cx_static_check,
      verify_own_descriptor IMPORTING is_manifest_descriptor TYPE zif_abapgit_apack_definitions=>ty_descriptor.

    DATA: mo_manifest_reader TYPE REF TO zcl_abapgit_apack_reader.
ENDCLASS.

CLASS ltcl_apack_manifest_reader IMPLEMENTATION.

  METHOD manifest_descriptor.
    verify_own_descriptor( mo_manifest_reader->get_manifest_descriptor( ) ).
  ENDMETHOD.

  METHOD setup.
    DATA: ls_apack_manifest_descriptor TYPE zif_abapgit_apack_definitions=>ty_descriptor.

    ls_apack_manifest_descriptor-group_id = 'github.com/abapGit'.
    ls_apack_manifest_descriptor-artifact_id = 'abapGit'.
    ls_apack_manifest_descriptor-version = '1.42'.
    ls_apack_manifest_descriptor-git_url = 'https://github.com/abapGit/abapGit.git'.

    mo_manifest_reader = zcl_abapgit_apack_reader=>create_instance( '$TMP' ).
    mo_manifest_reader->set_manifest_descriptor( ls_apack_manifest_descriptor ).

  ENDMETHOD.

  METHOD verify_own_descriptor.
    cl_abap_unit_assert=>assert_not_initial( is_manifest_descriptor ).
    cl_abap_unit_assert=>assert_equals( exp = 'github.com/abapGit'
                                        act = is_manifest_descriptor-group_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'abapGit'
                                        act = is_manifest_descriptor-artifact_id ).
    cl_abap_unit_assert=>assert_equals( exp = '1.42'
                                        act = is_manifest_descriptor-version ).
    " Repository type is added automatically by serializer later
    cl_abap_unit_assert=>assert_initial( is_manifest_descriptor-repository_type ).
    cl_abap_unit_assert=>assert_equals( exp = 'https://github.com/abapGit/abapGit.git'
                                        act = is_manifest_descriptor-git_url ).
    cl_abap_unit_assert=>assert_initial( is_manifest_descriptor-dependencies ).
  ENDMETHOD.

ENDCLASS.
