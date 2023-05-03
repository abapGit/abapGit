CLASS ltcl_test DEFINITION DEFERRED.
CLASS zcl_abapgit_data_deserializer DEFINITION LOCAL FRIENDS ltcl_test.

CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS test1 FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD test1.

    DATA li_cut TYPE REF TO zif_abapgit_data_deserializer.
    DATA li_config TYPE REF TO zif_abapgit_data_config.
    DATA lt_files TYPE zif_abapgit_git_definitions=>ty_files_tt.
    DATA ls_config TYPE zif_abapgit_data_config=>ty_config.

    CREATE OBJECT li_cut TYPE zcl_abapgit_data_deserializer.
    CREATE OBJECT li_config TYPE zcl_abapgit_data_config.

    ls_config-type = zif_abapgit_data_config=>c_data_type-tabu.
    ls_config-name = 'T100'.

    li_config->add_config( ls_config ).

    " this does not change the database. it just gives a preview of changes
    li_cut->deserialize(
      ii_config = li_config
      it_files  = lt_files ).

  ENDMETHOD.

ENDCLASS.
