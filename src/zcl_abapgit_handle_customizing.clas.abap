class ZCL_ABAPGIT_HANDLE_CUSTOMIZING definition
  public
  final
  create private .

public section.

  interfaces ZIF_ABAPGIT_HANDLE_CUSTOMIZING .

  methods CONSTRUCTOR
    importing
      !IV_TRANSPORT_REQUEST type TRKORR
    raising
      ZCX_ABAPGIT_EXCEPTION .
  class-methods GET_INSTANCE
    importing
      !IV_TRANSPORT_REQUEST type TRKORR
    returning
      value(RO_HANDLE_CUSTOMIZING) type ref to ZIF_ABAPGIT_HANDLE_CUSTOMIZING .
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
  types:
    BEGIN OF ty_handle_customizing_instance,
      transport_request TYPE trkorr,
      instance          TYPE REF TO zcl_abapgit_handle_customizing,
    END OF ty_handle_customizing_instance .
  types:
    ty_t_handle_customizing TYPE HASHED TABLE OF ty_handle_customizing_instance
                            WITH UNIQUE KEY transport_request .

  data MT_FIELDDESCRS type SCPR_RECORDS .
  data MT_RECATTR type SCPRRECATAB .
  data MT_STATUS_LIST type SCPR_TRANSP_ENTRIES .
  data MT_VALUES type SCPRVALSTAB .
  class-data MT_HANDLE_CUSTOMIZING_INSTANCE type TY_T_HANDLE_CUSTOMIZING .
  data MS_REQUEST_DETAILS type TRWBO_REQUEST .

  methods CREATE_BCSET_DATA_FROM_TR
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods CREATE_XML
    importing
      !IV_BCSET_IDX type XSTRING
      !IT_RECORD_ATTRIBUTE type SCPRRECATAB
      !IT_BCSET_VALUES type SCPRVALSTAB
    returning
      value(RO_XML_OUTPUT) type ref to ZCL_ABAPGIT_XML_OUTPUT
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods CREATE_OBJECT_FILES
    importing
      !IV_BCSET_IDX type XSTRING
      !IO_XML type ref to ZCL_ABAPGIT_XML_OUTPUT
    returning
      value(RO_OBJECT_FILES) type ref to ZCL_ABAPGIT_OBJECTS_FILES
    raising
      ZCX_ABAPGIT_EXCEPTION .
ENDCLASS.



CLASS ZCL_ABAPGIT_HANDLE_CUSTOMIZING IMPLEMENTATION.


  METHOD constructor.

*   Read request details
    CALL FUNCTION 'TR_READ_REQUEST'
      EXPORTING
        iv_read_objs_keys = abap_true
        iv_trkorr         = iv_transport_request
      CHANGING
        cs_request        = ms_request_details
      EXCEPTIONS
        error_occured     = 1
        no_authorization  = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from TR_READ_REQUEST' ).
    ENDIF. " IF sy-subrc <> 0

  ENDMETHOD.


  METHOD create_bcset_data_from_tr.

    DATA: lt_status_list TYPE scpr_transp_entries.

*   Read data based on transport request table keys
    CALL FUNCTION 'SCPR_TR_READ_DATA'
      IMPORTING
        tab_fielddescrs = mt_fielddescrs
        tab_recattr     = mt_recattr[]
        tab_values      = mt_values[]
      TABLES
        tab_e071        = ms_request_details-objects[]
        tab_e071k       = ms_request_details-keys[]
        tab_statuslist  = mt_status_list[]
      EXCEPTIONS
        no_data_found   = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from SCPR_TR_READ_DATA' ).
    ENDIF. " IF sy-subrc <> 0

  ENDMETHOD.


  METHOD get_instance.

    READ TABLE mt_handle_customizing_instance[] ASSIGNING FIELD-SYMBOL(<ls_handle_customizing>)
                                                WITH TABLE KEY transport_request = iv_transport_request.
    IF sy-subrc = 0.

      ro_handle_customizing ?= <ls_handle_customizing>-instance.

    ELSE.

      TRY.

          DATA(lo_handle_customizing) = NEW zcl_abapgit_handle_customizing( iv_transport_request = iv_transport_request ).

          mt_handle_customizing_instance[] = VALUE #( BASE mt_handle_customizing_instance ( transport_request = iv_transport_request
                                                                                            instance          = lo_handle_customizing
                                                                                          ) ).

          ro_handle_customizing ?= lo_handle_customizing.

        CATCH zcx_abapgit_exception ##NO_HANDLER.

      ENDTRY.

    ENDIF. " IF sy-subrc = 0

  ENDMETHOD.


  METHOD zif_abapgit_handle_customizing~stage_customizing_content.

*   Declaration of local internal table
    DATA: lt_recattr TYPE scprrecatab,
          lt_values  TYPE scprvalstab.

*   Declaration of local workarea
    DATA: ls_files_and_item TYPE zcl_abapgit_objects=>ty_serialization.

*   Create BC set metadata from transport request
    create_bcset_data_from_tr( ).

*   Process one table/customizing object
    LOOP AT mt_recattr[] ASSIGNING FIELD-SYMBOL(<ls_recattr>)
                         GROUP BY ( tablename  = <ls_recattr>-tablename
                                    objectname = <ls_recattr>-objectname
                                    objecttype = <ls_recattr>-objecttype
                                    clustname  = <ls_recattr>-clustname
                                  ) ASSIGNING FIELD-SYMBOL(<ls_recattr_group>).

*     For one table/customizing metadata
      lt_recattr[] = VALUE #( FOR ls_recattr IN GROUP <ls_recattr_group> ( ls_recattr ) ).

*     Retrieve the relevant customizing content
      lt_values[] = VALUE #( FOR ls_recattr_tmp IN lt_recattr[]
                             FOR ls_values      IN mt_values[] WHERE (     tablename = ls_recattr_tmp-tablename
                                                                       AND recnumber = ls_recattr_tmp-recnumber )
                             ( ls_values ) ).

*     Retrieve valid transport object details
      IF <ls_recattr_group>-clustname IS NOT INITIAL.

        DATA(ls_status_list) = mt_status_list[ cust_objname = <ls_recattr_group>-clustname
                                               cust_objtype = 'C' ].

      ELSEIF <ls_recattr_group>-objectname IS NOT INITIAL
      AND    <ls_recattr_group>-objecttype IS NOT INITIAL.

        ls_status_list = mt_status_list[ cust_objname = <ls_recattr_group>-objectname
                                         cust_objtype = <ls_recattr_group>-objecttype ].

      ELSE.

        ls_status_list = mt_status_list[ obj_name = <ls_recattr_group>-tablename ].

      ENDIF. " IF <ls_recattr_group>-clustname IS NOT INITIAL

*     Generate ID
      CONCATENATE ls_status_list-pgmid
                  ls_status_list-object
                  ls_status_list-obj_name
      INTO DATA(lv_id).

*     Get hash key value
      cl_abap_message_digest=>calculate_hash_for_char(
        EXPORTING
          if_data        = lv_id              " Data
        IMPORTING
          ef_hashxstring = DATA(lv_bcset_idx) " Binary hash value as XString
      ).

*     Create XML file
      DATA(lo_xml_data) = create_xml( iv_bcset_idx        = lv_bcset_idx
                                      it_record_attribute = lt_recattr[]
                                      it_bcset_values     = lt_values[]
                                    ).

*     Create object files
      DATA(lo_object_files) = create_object_files( iv_bcset_idx = lv_bcset_idx
                                                   io_xml       = lo_xml_data  " XML Output
      ).

      ls_files_and_item-item  = VALUE #( obj_type = 'SCP1' obj_name = lv_bcset_idx ).
      ls_files_and_item-files = lo_object_files->get_files( ).

      LOOP AT ls_files_and_item-files[] ASSIGNING FIELD-SYMBOL(<ls_file>).

        <ls_file>-sha1 = zcl_abapgit_hash=>sha1( iv_type = zif_abapgit_definitions=>c_type-blob
                                                 iv_data = <ls_file>-data
                                               ).

      ENDLOOP. " LOOP AT ls_files_and_item-files[] ASSIGNING FIELD-SYMBOL(<ls_file>)

    ENDLOOP. " LOOP AT mt_recattr[] ASSIGNING FIELD-SYMBOL(<ls_recattr>)

  ENDMETHOD.


  METHOD create_xml.

*   Declaration of local workarea
    DATA: ls_bcset_metadata TYPE ty_bcset_metadata.

*   Declaration of local variable
    DATA: lv_system_type TYPE sy-sysid.

*   Instantiate the XML object
    ro_xml_output = NEW #( ).

*   Get system type
    CALL FUNCTION 'TR_SYS_PARAMS'
      IMPORTING
        systemtype    = lv_system_type
      EXCEPTIONS
        no_systemname = 1
        no_systemtype = 2
        OTHERS        = 3.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from TR_SYS_PARAMS' ).
    ENDIF. " IF sy-subrc <> 0

*   Populate BC set header data
    ls_bcset_metadata = VALUE #( scprattr-id      = iv_bcset_idx
                                 scprattr-type    = 'A2G'
                                 scprattr-reftype = 'TRAN'
                                 scprattr-refname = ms_request_details-h-trkorr
                                 scprattr-orgid   = SWITCH #( lv_system_type WHEN 'SAP' THEN '/0SAP/'
                                                                                        ELSE '/CUSTOMER/' )
                               ).

*   Populate records and values metadata
    ls_bcset_metadata-scprreca[] = it_record_attribute[].
    ls_bcset_metadata-scprvals[] = it_bcset_values[].

*   Add metadata to XML
    ro_xml_output->add(
      EXPORTING
        iv_name = 'SCP1'
        ig_data = ls_bcset_metadata
    ).

  ENDMETHOD.


  METHOD create_object_files.

*  Declaration of local workarea
    DATA: ls_bcset_metadata TYPE ty_bcset_metadata,
          ls_item           TYPE zif_abapgit_definitions=>ty_item.

    ls_item = VALUE #( obj_type = 'SCP1' obj_name = iv_bcset_idx ).

*   Instantiate object files object
    ro_object_files = NEW #( is_item = ls_item ).

*   Add XML data to file
    ro_object_files->add_xml(
      EXPORTING
        io_xml = io_xml
    ).

  ENDMETHOD.
ENDCLASS.
