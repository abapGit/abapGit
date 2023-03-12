CLASS zcl_abapgit_xml_input DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_xml
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_xml_input.

    METHODS constructor
      IMPORTING
        !iv_xml      TYPE clike
        !iv_filename TYPE string OPTIONAL
      RAISING
        zcx_abapgit_exception .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA ms_i18n_params TYPE zif_abapgit_definitions=>ty_i18n_params.

    METHODS fix_xml.

ENDCLASS.



CLASS ZCL_ABAPGIT_XML_INPUT IMPLEMENTATION.


  METHOD constructor.

    super->constructor( iv_filename ).
    parse( iv_xml ).
    fix_xml( ).

  ENDMETHOD.


  METHOD fix_xml.

    DATA: li_git  TYPE REF TO if_ixml_element,
          li_abap TYPE REF TO if_ixml_node.


    li_git ?= mi_xml_doc->find_from_name_ns( depth = 0
                                             name = c_abapgit_tag ).
    li_abap = li_git->get_first_child( ).

    mi_xml_doc->get_root( )->remove_child( li_git ).
    mi_xml_doc->get_root( )->append_child( li_abap ).

  ENDMETHOD.


  METHOD zif_abapgit_xml_input~get_metadata.
    rs_metadata = ms_metadata.
  ENDMETHOD.


  METHOD zif_abapgit_xml_input~get_raw.
    ri_raw = mi_xml_doc.
  ENDMETHOD.


  METHOD zif_abapgit_xml_input~i18n_params.

    IF is_i18n_params IS SUPPLIED.
      ms_i18n_params = is_i18n_params.
    ENDIF.

    rs_i18n_params = ms_i18n_params.

  ENDMETHOD.


  METHOD zif_abapgit_xml_input~read.

    DATA: lx_error TYPE REF TO cx_transformation_error,
          lt_rtab  TYPE abap_trans_resbind_tab.

    FIELD-SYMBOLS: <ls_rtab> LIKE LINE OF lt_rtab.

    ASSERT NOT iv_name IS INITIAL.

    CLEAR cg_data. "Initialize result to avoid problems with empty values

    APPEND INITIAL LINE TO lt_rtab ASSIGNING <ls_rtab>.
    <ls_rtab>-name = iv_name.
    GET REFERENCE OF cg_data INTO <ls_rtab>-value.

    TRY.
        CALL TRANSFORMATION id
          OPTIONS value_handling = 'accept_data_loss'
          SOURCE XML mi_xml_doc
          RESULT (lt_rtab).
      CATCH cx_transformation_error INTO lx_error.
        IF mv_filename IS INITIAL.
          zcx_abapgit_exception=>raise( lx_error->if_message~get_text( ) ).
        ELSE.
          zcx_abapgit_exception=>raise( |File { mv_filename }: { lx_error->if_message~get_text( ) }| ).
        ENDIF.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
