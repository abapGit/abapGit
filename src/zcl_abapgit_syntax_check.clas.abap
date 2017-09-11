class ZCL_ABAPGIT_SYNTAX_CHECK definition
  public
  create public .

public section.

  class-methods RUN
    importing
      !IV_PACKAGE type DEVCLASS .
protected section.

  class-methods CREATE_OBJECTSET
    importing
      !IV_PACKAGE type DEVCLASS
    returning
      value(RO_SET) type ref to CL_CI_OBJECTSET .
private section.
ENDCLASS.



CLASS ZCL_ABAPGIT_SYNTAX_CHECK IMPLEMENTATION.


  METHOD create_objectset.

    CONSTANTS: lc_anonymous TYPE sci_objs VALUE ''.

    DATA: lt_objects TYPE scistadir,
          ls_flags   TYPE sci_flgl.

    FIELD-SYMBOLS: <ls_package> LIKE LINE OF lt_objects-sodevc.


    ro_set = cl_ci_objectset=>create(
      p_user = sy-uname
      p_name = lc_anonymous ).

    APPEND INITIAL LINE TO lt_objects-sodevc ASSIGNING <ls_package>.
    <ls_package>-sign = 'I'.
    <ls_package>-option = 'CP'.
    CONCATENATE iv_package '*' INTO <ls_package>-low.
* todo, find all subpackages

    ls_flags-class = abap_true.
    ls_flags-fugrs = abap_true.
    ls_flags-repos = abap_true.
    ls_flags-wdyns = abap_true.
    ls_flags-ddics = abap_true.
    ls_flags-typps = abap_true.

    ro_set->save_objectset(
      p_tadir     = lt_objects
      p_sel_flags = ls_flags ).

  ENDMETHOD.


  METHOD run.

    create_objectset( iv_package ).

* todo: add returning parameter

  ENDMETHOD.
ENDCLASS.
