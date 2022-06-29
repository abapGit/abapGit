INTERFACE zif_abapgit_intf
  PUBLIC .

    TYPES:
      BEGIN OF ty_docu,
        lines      TYPE tlinetab,
        i18n_lines TYPE zif_abapgit_lang_definitions=>ty_i18n_lines,
      END OF ty_docu.

    TYPES:
      BEGIN OF ty_intf,
        vseointerf  TYPE vseointerf,
        docu        TYPE ty_docu,
        description TYPE zif_abapgit_oo_object_fnc=>ty_seocompotx_tt,
      END OF TY_INTf.
ENDINTERFACE.
