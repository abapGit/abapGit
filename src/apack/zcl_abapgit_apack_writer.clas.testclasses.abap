*"* use this source file for your ABAP unit test classes
CLASS ltcl_apack_manifest_writer DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS: the_serializator FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_apack_manifest_writer IMPLEMENTATION.

  METHOD the_serializator.

    DATA: ls_apack_manifest_descriptor TYPE zif_abapgit_apack_definitions=>ty_descriptor,
          lo_manifest_writer           TYPE REF TO zcl_abapgit_apack_writer,
          lv_bool                      TYPE abap_bool,
          lv_actual_xml                TYPE string.

    ls_apack_manifest_descriptor-group_id = 'github.com/larshp'.
    ls_apack_manifest_descriptor-artifact_id = 'abapGit'.
    ls_apack_manifest_descriptor-version = '1.8'.
    ls_apack_manifest_descriptor-git_url = 'https://github.com/larshp/abapGit.git'.

    lo_manifest_writer = zcl_abapgit_apack_writer=>create_instance( ls_apack_manifest_descriptor ).
    lv_actual_xml = lo_manifest_writer->serialize( ).
    cl_abap_unit_assert=>assert_not_initial( lv_actual_xml ).

    lv_bool = boolc( contains( val = lv_actual_xml
                               sub = '<ARTIFACT_ID>abapGit</ARTIFACT_ID>' ) ).
    cl_abap_unit_assert=>assert_equals( act = lv_bool
                                        exp = abap_true ).
    lv_bool = boolc( contains( val = lv_actual_xml
                               sub = '<GROUP_ID>github.com/larshp</GROUP_ID>' ) ).
    cl_abap_unit_assert=>assert_equals( act = lv_bool
                                        exp = abap_true ).
    lv_bool = boolc( contains( val = lv_actual_xml
                               sub = '<REPOSITORY_TYPE>abapGit</REPOSITORY_TYPE>' ) ).
    cl_abap_unit_assert=>assert_equals( act = lv_bool
                                        exp = abap_true ).
  ENDMETHOD.

ENDCLASS.
