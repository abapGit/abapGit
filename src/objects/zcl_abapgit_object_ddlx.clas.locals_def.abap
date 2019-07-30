INTERFACE lif_tadir.
  METHODS tadir_insert
    IMPORTING
      iv_package TYPE devclass
    RAISING
      zcx_abapgit_exception.
ENDINTERFACE.
