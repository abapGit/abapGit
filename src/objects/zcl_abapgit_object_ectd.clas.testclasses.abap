*"* use this source file for your ABAP unit test classes

CLASS ltcl_changed_by DEFINITION DEFERRED.

CLASS zcl_abapgit_object_ectd DEFINITION LOCAL FRIENDS ltcl_changed_by.

CLASS ltcl_changed_by DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      ms_given_current_changed  TYPE zcl_abapgit_object_ectd=>ty_last_changed,
      ms_given_last_changed     TYPE zcl_abapgit_object_ectd=>ty_last_changed,
      mo_cut                    TYPE REF TO zcl_abapgit_object_ectd,
      mv_act_change_more_recent TYPE abap_bool.

    METHODS:
      setup,

      current_more_recent_than_last FOR TESTING RAISING cx_static_check,

      last_more_recent_than_current FOR TESTING RAISING cx_static_check,

      same_day_current_more_recent FOR TESTING RAISING cx_static_check,

      same_day_last_more_recent FOR TESTING RAISING cx_static_check,

      given_currently_changed
        IMPORTING
          iv_ldate TYPE d
          iv_ltime TYPE t,

      given_last_changed
        IMPORTING
          iv_ldate TYPE d
          iv_ltime TYPE t,

      when_changed_is_checked,

      then_should_be
        IMPORTING
          iv_exp_change_more_recent TYPE abap_bool.

ENDCLASS.


CLASS ltcl_changed_by IMPLEMENTATION.

  METHOD setup.

    DATA: ls_dummy_item TYPE zif_abapgit_definitions=>ty_item.

    ls_dummy_item-obj_name = 'DUMMY'.
    ls_dummy_item-obj_type = 'ECTD'.

    CREATE OBJECT mo_cut
      EXPORTING
        is_item     = ls_dummy_item
        iv_language = sy-langu.

  ENDMETHOD.

  METHOD current_more_recent_than_last.

    given_currently_changed( iv_ldate = '20180403'
                             iv_ltime = '090101' ).

    given_last_changed( iv_ldate = '20180402'
                        iv_ltime = '080101' ).


    when_changed_is_checked( ).

    then_should_be( iv_exp_change_more_recent = abap_true ).

  ENDMETHOD.


  METHOD last_more_recent_than_current.

    given_currently_changed( iv_ldate = '20180402'
                             iv_ltime = '080101' ).

    given_last_changed( iv_ldate = '20180403'
                        iv_ltime = '090101' ).

    when_changed_is_checked( ).

    then_should_be( iv_exp_change_more_recent = abap_false ).

  ENDMETHOD.


  METHOD same_day_current_more_recent.

    given_currently_changed( iv_ldate = '20180402'
                             iv_ltime = '090103' ).

    given_last_changed( iv_ldate = '20180402'
                        iv_ltime = '090101' ).

    when_changed_is_checked( ).

    then_should_be( iv_exp_change_more_recent = abap_true ).

  ENDMETHOD.


  METHOD same_day_last_more_recent.

    given_currently_changed( iv_ldate = '20180402'
                             iv_ltime = '090103' ).

    given_last_changed( iv_ldate = '20180402'
                        iv_ltime = '090104' ).

    when_changed_is_checked( ).

    then_should_be( iv_exp_change_more_recent = abap_false ).

  ENDMETHOD.


  METHOD given_currently_changed.

    ms_given_current_changed-ldate = iv_ldate.
    ms_given_current_changed-ltime = iv_ltime.

  ENDMETHOD.


  METHOD given_last_changed.

    ms_given_last_changed-ldate = iv_ldate.
    ms_given_last_changed-ltime = iv_ltime.

  ENDMETHOD.


  METHOD when_changed_is_checked.

    mv_act_change_more_recent = mo_cut->is_change_more_recent_than(
                                  is_currently_changed = ms_given_current_changed
                                  is_last_changed    = ms_given_last_changed ).

  ENDMETHOD.


  METHOD then_should_be.

    cl_abap_unit_assert=>assert_equals(
      exp = iv_exp_change_more_recent
      act = mv_act_change_more_recent ).

  ENDMETHOD.

ENDCLASS.
