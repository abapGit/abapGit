INTERFACE zif_abapgit_handle_customizing
  PUBLIC .


  METHODS stage_customizing_content
    IMPORTING
      !iv_devclass             TYPE devclass
    RETURNING
      VALUE(ro_staged_content) TYPE REF TO zcl_abapgit_stage
    RAISING
      zcx_abapgit_exception .
ENDINTERFACE.
