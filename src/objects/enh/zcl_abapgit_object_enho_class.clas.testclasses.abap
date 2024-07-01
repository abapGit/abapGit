CLASS ltcl_enho_class DEFINITION FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS adjust_generated_comments FOR TESTING.

ENDCLASS.

CLASS zcl_abapgit_object_enho_class DEFINITION LOCAL FRIENDS ltcl_enho_class.

CLASS ltcl_enho_class IMPLEMENTATION.

  METHOD adjust_generated_comments.

    DATA lt_source_act TYPE rswsourcet.
    DATA lt_source_exp TYPE rswsourcet.

    INSERT `  METHOD run.` INTO TABLE lt_source_act.
    INSERT `*"------------------------------------------------------------------------*` INTO TABLE lt_source_act.
    INSERT `*"METHODS run .` INTO TABLE lt_source_act.
    INSERT `*"` INTO TABLE lt_source_act.
    INSERT `*"METHODS test.` INTO TABLE lt_source_act.
    INSERT `*"------------------------------------------------------------------------*` INTO TABLE lt_source_act.
    INSERT `    BREAK-POINT .` INTO TABLE lt_source_act.
    INSERT `  ENDMETHOD.` INTO TABLE lt_source_act.

    lt_source_exp = lt_source_act.

    DELETE lt_source_exp INDEX 3.
    INSERT `*"METHODS run.` INTO lt_source_exp INDEX 3.  " <<< change only this

    zcl_abapgit_object_enho_class=>adjust_generated_comments( CHANGING ct_source = lt_source_act ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_source_act
      exp = lt_source_exp ).

  ENDMETHOD.

ENDCLASS.
