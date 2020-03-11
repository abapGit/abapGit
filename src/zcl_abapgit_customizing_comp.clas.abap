class ZCL_ABAPGIT_CUSTOMIZING_COMP definition
  public
  final
  create private .

public section.

  interfaces ZIF_ABAPGIT_CUSTOMIZING_COMP .

  class-methods GET_INSTANCE
    returning
      value(RO_CUSTOMIZING_COMPARE) type ref to ZIF_ABAPGIT_CUSTOMIZING_COMP .
protected section.
private section.

  types:
    BEGIN OF ty_bcset_metadata,
      scprattr TYPE scprattr,
      scprtext TYPE STANDARD TABLE OF scprtext WITH DEFAULT KEY,
      scprvals TYPE STANDARD TABLE OF scprvals WITH DEFAULT KEY,
      scprvall TYPE STANDARD TABLE OF scprvall WITH DEFAULT KEY,
      scprreca TYPE STANDARD TABLE OF scprreca WITH DEFAULT KEY,
      scprfldv TYPE STANDARD TABLE OF scprfldv WITH DEFAULT KEY,
      subprofs TYPE STANDARD TABLE OF scprpprl WITH DEFAULT KEY,
    END OF ty_bcset_metadata .

  data MT_FIELDDESCRS type SCPR_RECORDS .
  data MT_RECATTR type SCPRRECATAB .
  data MT_STATUS_LIST type SCPR_TRANSP_ENTRIES .
  data MT_VALUES type SCPRVALSTAB .
  class-data MO_CUSTOMIZING_COMPARE type ref to ZCL_ABAPGIT_CUSTOMIZING_COMP .
  data MS_BCSET_METADATA type TY_BCSET_METADATA .

  methods READ_BCSET_METADATA
    importing
      !IS_ITEM type ZIF_ABAPGIT_DEFINITIONS=>TY_ITEM
      !IS_FILE_DETAILS type ZIF_ABAPGIT_DEFINITIONS=>TY_FILE .
  methods IS_A2G_TYPE_BCSET
    returning
      value(RV_IS_A2G_TYPE_BCSET) type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_ABAPGIT_CUSTOMIZING_COMP IMPLEMENTATION.


  METHOD get_instance.

    IF mo_customizing_compare IS NOT BOUND.

      mo_customizing_compare = NEW #( ).

    ENDIF. " IF mo_customizing_compare IS NOT BOUND

    ro_customizing_compare ?= mo_customizing_compare.

  ENDMETHOD.


  METHOD is_a2g_type_bcset.

    rv_is_a2g_type_bcset = SWITCH #( ms_bcset_metadata-scprattr-type WHEN 'A2G' THEN abap_true
                                                                                ELSE abap_false
                                   ).
  ENDMETHOD.


  METHOD read_bcset_metadata.

*   Instantiate object files object
    DATA(lo_object_files) = NEW zcl_abapgit_objects_files( is_item = is_item ).

    lo_object_files->add( is_file = is_file_details ).

    DATA(lo_xml_file) = lo_object_files->read_xml( ).

    lo_xml_file->read(
      EXPORTING
        iv_name = is_item-obj_type
      CHANGING
        cg_data = ms_bcset_metadata
    ).

  ENDMETHOD.


  METHOD zif_abapgit_customizing_comp~compare_customizing_with_table.

    DATA: lv_is_data_equal TYPE char1.

    read_bcset_metadata( is_item         = is_item
                         is_file_details = is_file_details
                       ).

    IF is_a2g_type_bcset( ) = abap_false. " ' '
      RETURN.
    ENDIF. " IF is_a2g_type_bcset( ) = abap_false

    cl_scpr_runtime_bcset_factory=>set_bcset_runtime_metadata(
      EXPORTING
        is_attributes  = ms_bcset_metadata-scprattr                     " BC Set: Attributes
        is_bcset_text  = VALUE #( id = ms_bcset_metadata-scprattr-id langu = sy-langu )
        it_record_attr = ms_bcset_metadata-scprreca[]                   " BC Set Data Record Information Table Type
        it_values      = ms_bcset_metadata-scprvals[]                   " BC Set Data Table Type
        it_values_lang = ms_bcset_metadata-scprvall[]                   " Language-Dependent BC Set Data Table Type
    ).

*   Compare BC set content with customizing tables
    CALL FUNCTION 'SCPR_PRSET_MN_COMP_CUST_TABLE'
      EXPORTING
        bcset_id           = ms_bcset_metadata-scprattr-id
        with_dialog        = abap_false
      IMPORTING
        flag_data_equal    = lv_is_data_equal
      EXCEPTIONS
        no_bcset           = 1
        bcset_selection    = 2
        no_data            = 3
        wrong_parameters   = 4
        no_authority       = 5
        no_authority_activ = 6
        internal_error     = 7
        OTHERS             = 8.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from SCPR_PRSET_MN_COMP_CUST_TABLE' ).
    ENDIF.

    cs_result-match = SWITCH #( lv_is_data_equal WHEN 'Y' THEN abap_true
                                                          ELSE abap_false ).

    cs_result-lstate = SWITCH #( lv_is_data_equal WHEN 'Y' THEN zif_abapgit_definitions=>c_state-unchanged
                                                           ELSE zif_abapgit_definitions=>c_state-modified ).

  ENDMETHOD.


  METHOD zif_abapgit_customizing_comp~create_local_file.

*   Declaration of local internal table
    DATA: lt_object TYPE STANDARD TABLE OF e071,
          lt_keys   TYPE STANDARD TABLE OF e071k.

    DATA: ls_bcset_metadata TYPE ty_bcset_metadata,
          ls_item           TYPE zif_abapgit_definitions=>ty_item,
          ls_file           TYPE zif_abapgit_definitions=>ty_file_item.

    LOOP AT rs_file-status[] ASSIGNING FIELD-SYMBOL(<ls_status>)
                             WHERE obj_type = 'SCP1'.

      READ TABLE rs_file-remote[] ASSIGNING FIELD-SYMBOL(<ls_remote>)
                                  WITH KEY path     = <ls_status>-path
                                           filename = <ls_status>-filename.
      CHECK sy-subrc = 0.

      read_bcset_metadata(
        EXPORTING
          is_item         = VALUE #( obj_type = <ls_status>-obj_type obj_name = <ls_status>-obj_name devclass = <ls_status>-package )
          is_file_details = <ls_remote>
      ).

      IF is_a2g_type_bcset( ) = abap_false. " ' '
        RETURN.
      ENDIF. " IF is_a2g_type_bcset( ) = abap_false

      cl_scpr_runtime_bcset_factory=>set_bcset_runtime_metadata(
        EXPORTING
          is_attributes  = ms_bcset_metadata-scprattr                     " BC Set: Attributes
          is_bcset_text  = VALUE #( id = ms_bcset_metadata-scprattr-id langu = sy-langu )
          it_record_attr = ms_bcset_metadata-scprreca[]                   " BC Set Data Record Information Table Type
          it_values      = ms_bcset_metadata-scprvals[]                   " BC Set Data Table Type
          it_values_lang = ms_bcset_metadata-scprvall[]                   " Language-Dependent BC Set Data Table Type
      ).

      CALL FUNCTION 'SCPR_BCSET_DATA_TO_E071'
        EXPORTING
          bcset_id                = ms_bcset_metadata-scprattr-id
        TABLES
          e071                    = lt_object[]
          e071k                   = lt_keys[]
        EXCEPTIONS
          bcset_dont_exist        = 1
          bcset_is_empty          = 2
          bcset_internal_error    = 3
          wrong_ddic_informations = 4
          OTHERS                  = 5.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'error from SCPR_BCSET_DATA_TO_E071' ).
      ENDIF.

*     Read data based on transport request table keys
      CALL FUNCTION 'SCPR_TR_READ_DATA'
        IMPORTING
          tab_fielddescrs = mt_fielddescrs
          tab_recattr     = mt_recattr[]
          tab_values      = mt_values[]
        TABLES
          tab_e071        = lt_object[]
          tab_e071k       = lt_keys[]
        EXCEPTIONS
          no_data_found   = 1
          OTHERS          = 2.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'error from SCPR_TR_READ_DATA' ).
      ENDIF. " IF sy-subrc <> 0

*     Instantiate the XML object
      DATA(lo_xml_output) = NEW zcl_abapgit_xml_output( ).

      ls_bcset_metadata-scprattr   = ms_bcset_metadata-scprattr.
      ls_bcset_metadata-scprtext   = ms_bcset_metadata-scprtext.
      ls_bcset_metadata-scprreca[] = mt_recattr[].
      ls_bcset_metadata-scprvals[] = mt_values[].

*     Add metadata to XML
      lo_xml_output->add(
        EXPORTING
          iv_name = 'SCP1'
          ig_data = ls_bcset_metadata
      ).

      ls_item = VALUE #( obj_type = 'SCP1' obj_name = ms_bcset_metadata-scprattr-id ).

*     Instantiate object files object
      DATA(lo_object_files) = NEW zcl_abapgit_objects_files( is_item = ls_item ).

*     Add XML data to file
      lo_object_files->add_xml(
        EXPORTING
          io_xml = lo_xml_output
      ).

      LOOP AT lo_object_files->get_files( ) ASSIGNING FIELD-SYMBOL(<ls_file>).

        ls_file-file = CORRESPONDING #( <ls_file> ).
        ls_file-file-filename = <ls_status>-filename.
        ls_file-file-sha1 = zcl_abapgit_hash=>sha1( iv_type = zif_abapgit_definitions=>c_type-blob
                                                    iv_data = ls_file-file-data
                                                  ).

      ENDLOOP.

      ls_file-item = ls_item.
      APPEND ls_file TO rs_file-local[].

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
