INTERFACE zif_abapgit_persist_packages PUBLIC.

  TYPES:
    BEGIN OF ty_package,
      devclass   TYPE scompkdtln-devclass,
      component  TYPE scompkdtln-component,
      comp_posid TYPE scompkdtln-comp_posid,
    END OF ty_package,
    ty_packages TYPE HASHED TABLE OF ty_package WITH UNIQUE KEY devclass.

  METHODS modify
    IMPORTING
      !iv_package    TYPE scompkdtln-devclass
      !iv_component  TYPE scompkdtln-component OPTIONAL
      !iv_comp_posid TYPE scompkdtln-comp_posid OPTIONAL
    RAISING
      zcx_abapgit_exception.

  METHODS read
    IMPORTING
      !iv_package       TYPE scompkdtln-devclass
    RETURNING
      VALUE(rs_package) TYPE ty_package.

ENDINTERFACE.
