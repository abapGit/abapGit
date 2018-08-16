CLASS zcl_abapgit_state DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS:
      reduce
        IMPORTING
          !iv_cur  TYPE char1
        CHANGING
          !cv_prev TYPE char1 .

ENDCLASS.



CLASS ZCL_ABAPGIT_STATE IMPLEMENTATION.


  METHOD reduce.

    IF cv_prev = iv_cur OR iv_cur IS INITIAL.
      RETURN. " No change
    ELSEIF cv_prev IS INITIAL.
      cv_prev = iv_cur.
    ELSE.
      cv_prev = zif_abapgit_definitions=>c_state-mixed.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
