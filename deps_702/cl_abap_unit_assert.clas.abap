* The methods of CL_ABAP_UNIT_ASSERT that abapGit uses and that exist on 7.02,
* so that abaplint reports methods added in later releases (ASSERT_TRUE, ...).
* Add a method here only if it exists on 7.02.
* Only read by abaplint, see abaplint.json. Based on open-abap.
CLASS cl_abap_unit_assert DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS
      assert_equals
        IMPORTING
          act                     TYPE any
          exp                     TYPE any
          msg                     TYPE csequence OPTIONAL
          tol                     TYPE f OPTIONAL
          quit                    TYPE i OPTIONAL
          level                   TYPE i OPTIONAL
        RETURNING
          VALUE(assertion_failed) TYPE abap_bool.

    CLASS-METHODS
      assert_number_between
        IMPORTING
          lower                   TYPE numeric
          upper                   TYPE numeric
          number                  TYPE numeric
          msg                     TYPE csequence OPTIONAL
          quit                    TYPE i OPTIONAL
          level                   TYPE i OPTIONAL
        RETURNING
          VALUE(assertion_failed) TYPE abap_bool.

    CLASS-METHODS
      assert_not_initial
        IMPORTING
          act                     TYPE any
          msg                     TYPE csequence OPTIONAL
          quit                    TYPE i OPTIONAL
          level                   TYPE i OPTIONAL
        RETURNING
          VALUE(assertion_failed) TYPE abap_bool.

    CLASS-METHODS
      assert_initial
        IMPORTING
          act                     TYPE any
          msg                     TYPE csequence OPTIONAL
          quit                    TYPE i OPTIONAL
          level                   TYPE i OPTIONAL
        RETURNING
          VALUE(assertion_failed) TYPE abap_bool.

    CLASS-METHODS
      fail
        IMPORTING
          msg    TYPE csequence OPTIONAL
          quit   TYPE i OPTIONAL
          level  TYPE i OPTIONAL
          detail TYPE csequence OPTIONAL
        PREFERRED PARAMETER msg.

    CLASS-METHODS
      assert_subrc
        IMPORTING
          exp                     TYPE i DEFAULT 0
          act                     TYPE i DEFAULT sy-subrc
          msg                     TYPE csequence OPTIONAL
          quit                    TYPE i OPTIONAL
          level                   TYPE i OPTIONAL
        PREFERRED PARAMETER act
        RETURNING
          VALUE(assertion_failed) TYPE abap_bool.

    CLASS-METHODS
      assert_char_cp
        IMPORTING
          act                     TYPE clike
          exp                     TYPE clike
          msg                     TYPE string OPTIONAL
          quit                    TYPE i OPTIONAL
          level                   TYPE i OPTIONAL
        RETURNING
          VALUE(assertion_failed) TYPE abap_bool.

    CLASS-METHODS
      assert_bound
        IMPORTING
          act                     TYPE any
          msg                     TYPE csequence OPTIONAL
          quit                    TYPE i OPTIONAL
          level                   TYPE i OPTIONAL
        RETURNING
          VALUE(assertion_failed) TYPE abap_bool.

    CLASS-METHODS
      assert_not_bound
        IMPORTING
          act                     TYPE any
          msg                     TYPE csequence OPTIONAL
          quit                    TYPE i OPTIONAL
          level                   TYPE i OPTIONAL
        RETURNING
          VALUE(assertion_failed) TYPE abap_bool.

    CLASS-METHODS
      assert_text_matches
        IMPORTING
          pattern                 TYPE csequence
          text                    TYPE csequence
          msg                     TYPE csequence OPTIONAL
          quit                    TYPE i OPTIONAL
          level                   TYPE i OPTIONAL
        RETURNING
          VALUE(assertion_failed) TYPE abap_bool.

ENDCLASS.

CLASS cl_abap_unit_assert IMPLEMENTATION.
  METHOD assert_equals.
  ENDMETHOD.
  METHOD assert_number_between.
  ENDMETHOD.
  METHOD assert_not_initial.
  ENDMETHOD.
  METHOD assert_initial.
  ENDMETHOD.
  METHOD fail.
  ENDMETHOD.
  METHOD assert_subrc.
  ENDMETHOD.
  METHOD assert_char_cp.
  ENDMETHOD.
  METHOD assert_bound.
  ENDMETHOD.
  METHOD assert_not_bound.
  ENDMETHOD.
  METHOD assert_text_matches.
  ENDMETHOD.
ENDCLASS.
