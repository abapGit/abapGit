class ZCL_ABAPGIT_SAP definition
  public
  final
  create public .

public section.

  class-methods IS_SAP_NAMESPACE
    importing
      !TYPE type TROBJTYPE optional
      !OBJECT type CSEQUENCE
    exporting
      !NAMESPACE type NAMESPACE
    returning
      value(R_IS_SAP_NAMESPACE) type ABAP_BOOL .
  class-methods GET_NAMESPACE
    importing
      !TYPE type TROBJTYPE optional
      !OBJECT type CSEQUENCE
    returning
      value(NAMESPACE) type NAMESPACE .
protected section.

  class-data MT_NAMESPACE_SETTINGS type SORTED TABLE OF trnspacet
    with UNIQUE key namespace .
private section.
ENDCLASS.



CLASS ZCL_ABAPGIT_SAP IMPLEMENTATION.


  METHOD get_namespace.

    CLEAR: namespace.

    "**********************************
    "Sanity Check
    "**********************************
    IF object IS INITIAL.
      RETURN.
    ENDIF.

    "**********************************
    "Namespace Determination
    "**********************************
    CASE type.

***Special Cases can be handled here

      WHEN OTHERS.
        "Default Handling

        "Determine Namespace
        IF object(1) <> '/'.
          "Object has no namespace
          RETURN.
        ELSE.
          DATA(offset) = find( val = object+1 sub = '/' ) + 2.
          namespace = object(offset).
        ENDIF.

        IF namespace NP '/*/'.
          CLEAR: namespace.
          RETURN.
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD is_sap_namespace.

    r_is_sap_namespace = abap_true.

    "**********************************
    "Sanity Check
    "**********************************
    IF object IS INITIAL.
      RETURN.
    ENDIF.

    "**********************************
    "Namespace Evaluation
    "**********************************
    CASE type.

***Special Cases can be handled here

      WHEN OTHERS.
        "Default Handling

        "Determine Namespace
        namespace = get_namespace( type   = type
                                   object = object ).
        IF namespace IS INITIAL.
          IF namespace(1) = 'Z' OR
             namespace(1) = 'Y'.
            "Customer Namespace for Objects w/o Namespace
            r_is_sap_namespace = abap_false.
            RETURN.
          ENDIF.
        ENDIF.

        "Check Namespace Settings
        READ TABLE mt_namespace_settings ASSIGNING FIELD-SYMBOL(<st_info>)
          WITH TABLE KEY namespace = namespace.
        IF sy-subrc <> 0.
          SELECT SINGLE * FROM trnspacet
            WHERE namespace = @namespace
            INTO  @DATA(st_info).
          IF sy-subrc <> 0.
            "Unknown Namespace: Setup a Default that behaves like a SAP Namespace
            st_info-namespace = namespace.
            st_info-role      = 'C'.
            st_info-sapflag   = abap_true.
          ENDIF.
          INSERT st_info INTO TABLE mt_namespace_settings ASSIGNING <st_info>.
        ENDIF.

        IF <st_info> IS ASSIGNED          AND
           <st_info>-sapflag = abap_false AND
           <st_info>-role    = 'P'.
          r_is_sap_namespace = abap_false.
        ENDIF.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
