CLASS ltcl_frontend_services DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abapgit_frontend_services.

    METHODS:
      setup,
      path_windows FOR TESTING,
      path_unix FOR TESTING,
      no_path FOR TESTING.

ENDCLASS.

CLASS zcl_abapgit_frontend_services DEFINITION LOCAL FRIENDS ltcl_frontend_services.

CLASS ltcl_frontend_services IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD path_windows.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_path_from_fullname( 'C:\SAPworkdir\hello\world.abap' )
      exp = 'C:\SAPworkdir\hello\' ).
  ENDMETHOD.

  METHOD path_unix.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_path_from_fullname( '/var/tmp/world.abap' )
      exp = '/var/tmp/' ).
  ENDMETHOD.

  METHOD no_path.
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_path_from_fullname( 'toronto.abap' )
      exp = '' ).
  ENDMETHOD.

ENDCLASS.
