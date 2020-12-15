CLASS zcl_abapgit_object_w3mi DEFINITION PUBLIC INHERITING FROM zcl_abapgit_object_w3xx_super FINAL.

  PROTECTED SECTION.
    METHODS: change_bdc_jump_data REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_object_w3mi IMPLEMENTATION.


  METHOD change_bdc_jump_data.

    DATA: ls_bdcdata LIKE LINE OF ct_bdcdata.

    ls_bdcdata-fnam = 'RADIO_HT'.
    ls_bdcdata-fval = ' '.
    APPEND ls_bdcdata TO ct_bdcdata.

    CLEAR ls_bdcdata.
    ls_bdcdata-fnam = 'RADIO_MI'.
    ls_bdcdata-fval = 'X'.
    APPEND ls_bdcdata TO ct_bdcdata.

  ENDMETHOD.
ENDCLASS.
