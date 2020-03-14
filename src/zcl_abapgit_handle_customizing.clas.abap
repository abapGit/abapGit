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

  constants MC_BCSET type TROBJTYPE value 'SCP1' ##NO_TEXT.
  data MO_STAGED_FILES type ref to ZCL_ABAPGIT_STAGE .
  data MT_FIELDDESCRS type SCPR_RECORDS .
  data MT_RECATTR type SCPRRECATAB .
  data MT_VALUES type SCPRVALSTAB .
  class-data MT_HANDLE_CUSTOMIZING_INSTANCE type TY_T_HANDLE_CUSTOMIZING .
  data MS_REQUEST_DETAILS type TRWBO_REQUEST .
  constants MC_VERSION type SCPR_VERS value 'N' ##NO_TEXT.
  constants MC_BCSET_TYPE type SCPR_TYPE value 'A2G' ##NO_TEXT.

  methods CREATE_BCSET_DATA_FROM_TR
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods CREATE_XML
    importing
      !IV_BCSET_ID type SCPR_ID
      !IT_RECORD_ATTRIBUTE type SCPRRECATAB
      !IT_BCSET_VALUES type SCPRVALSTAB
    returning
      value(RO_XML_OUTPUT) type ref to ZCL_ABAPGIT_XML_OUTPUT
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods CREATE_OBJECT_FILES
    importing
      !IV_BCSET_ID type SCPR_ID
      !IV_DEVCLASS type DEVCLASS
      !IO_XML type ref to ZCL_ABAPGIT_XML_OUTPUT
    returning
      value(RO_OBJECT_FILES) type ref to ZCL_ABAPGIT_OBJECTS_FILES
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods STAGE_FILES
    importing
      !IV_BCSET_ID type SCPR_ID
      !IV_DEVCLASS type DEVCLASS
      !IO_OBJECT_FILES type ref to ZCL_ABAPGIT_OBJECTS_FILES
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
      zcx_abapgit_exception=>raise( 'error from TR_READ_REQUEST'(001) ).
    ENDIF. " IF sy-subrc <> 0

  ENDMETHOD.


  METHOD create_bcset_data_from_tr.

*   Read data based on transport request table keys
    CALL FUNCTION 'SCPR_TR_READ_DATA'
      IMPORTING
        tab_fielddescrs = mt_fielddescrs
        tab_recattr     = mt_recattr[]
        tab_values      = mt_values[]
      TABLES
        tab_e071        = ms_request_details-objects[]
        tab_e071k       = ms_request_details-keys[]
      EXCEPTIONS
        no_data_found   = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from SCPR_TR_READ_DATA'(002) ).
    ENDIF. " IF sy-subrc <> 0

  ENDMETHOD.


  METHOD get_instance.

*   Declaration of local object reference
    DATA: lo_handle_customizing TYPE REF TO zcl_abapgit_handle_customizing.

*   Declaration of local workarea
    DATA: ls_handle_customizing TYPE ty_handle_customizing_instance.

    READ TABLE mt_handle_customizing_instance[] INTO ls_handle_customizing
                                                WITH TABLE KEY transport_request = iv_transport_request.
    IF sy-subrc = 0.

      ro_handle_customizing ?= ls_handle_customizing-instance.

    ELSE.

      TRY.

          CREATE OBJECT lo_handle_customizing
            EXPORTING
              iv_transport_request = iv_transport_request.

          ls_handle_customizing-transport_request = iv_transport_request.
          ls_handle_customizing-instance          = lo_handle_customizing.
          INSERT ls_handle_customizing INTO TABLE mt_handle_customizing_instance[].

          ro_handle_customizing ?= lo_handle_customizing.

        CATCH zcx_abapgit_exception ##NO_HANDLER.

      ENDTRY.

    ENDIF. " IF sy-subrc = 0

  ENDMETHOD.


  METHOD zif_abapgit_handle_customizing~stage_customizing_content.

*   Declaration of local object reference
    DATA: lo_xml_data     TYPE REF TO zcl_abapgit_xml_output,
          lo_object_files TYPE REF TO zcl_abapgit_objects_files.

*   Declaration of local internal table
    DATA: lt_recattr TYPE scprrecatab,
          lt_values  TYPE scprvalstab,
          lt_objects TYPE scp1_act_objects.

*   Declaration of local variable
    DATA: lv_bcset_id TYPE scpr_id.

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

      CALL FUNCTION 'SCPR_ACTIV_EXTRACT_OBJECTS'
        IMPORTING
          act_objects = lt_objects[]
        TABLES
          recattr     = lt_recattr[].

      READ TABLE lt_objects[] ASSIGNING FIELD-SYMBOL(<ls_object>) INDEX 1.

*     Generate ID
      CONCATENATE <ls_object>-objectname
                  <ls_object>-objecttype
      INTO lv_bcset_id
      SEPARATED BY '_'.

*     Create XML file
      lo_xml_data = create_xml( iv_bcset_id         = lv_bcset_id
                                it_record_attribute = lt_recattr[]
                                it_bcset_values     = lt_values[] ).

*     Create object files
      lo_object_files = create_object_files( iv_bcset_id = lv_bcset_id
                                             iv_devclass = iv_devclass
                                             io_xml      = lo_xml_data ). " XML Output

*     Stage created files
      stage_files( iv_bcset_id     = lv_bcset_id
                   iv_devclass     = iv_devclass
                   io_object_files = lo_object_files ).

    ENDLOOP. " LOOP AT mt_recattr[] ASSIGNING <ls_recattr>

    ro_staged_content = mo_staged_files.

  ENDMETHOD.


  METHOD create_xml.

*   Declaration of local workarea
    DATA: ls_bcset_metadata TYPE ty_bcset_metadata,
          ls_bcset_text     TYPE scprtext.

*   Declaration of local variable
    DATA: lv_system_type TYPE sy-sysid,
          lv_org_id      TYPE scpr_orgid.

*   Declaration of local field symbols
    FIELD-SYMBOLS: <ls_recattr> TYPE scprreca,
                   <ls_values>  TYPE scprvals.

*   Instantiate the XML object
    CREATE OBJECT ro_xml_output.

*   Get system type
    CALL FUNCTION 'TR_SYS_PARAMS'
      IMPORTING
        systemtype    = lv_system_type
      EXCEPTIONS
        no_systemname = 1
        no_systemtype = 2
        OTHERS        = 3.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error from TR_SYS_PARAMS'(003) ).
    ENDIF. " IF sy-subrc <> 0

    IF lv_system_type = 'SAP'.
      lv_org_id = '/0SAP/'.

    ELSE.
      lv_org_id = '/CUSTOMER/'.

    ENDIF. " IF lv_system_type = 'SAP'

*   Populate BC set header data
    ls_bcset_metadata-scprattr-id      = iv_bcset_id.
    ls_bcset_metadata-scprattr-version = mc_version.    " N
    ls_bcset_metadata-scprattr-type    = mc_bcset_type. " A2G
    ls_bcset_metadata-scprattr-reftype = 'TRAN'.
    ls_bcset_metadata-scprattr-refname = ms_request_details-h-trkorr.
    ls_bcset_metadata-scprattr-orgid   = lv_org_id.

*   Populate short text
    ls_bcset_text-id      = ls_bcset_metadata-scprattr-id.
    ls_bcset_text-version = ls_bcset_metadata-scprattr-version.
    ls_bcset_text-langu   = sy-langu.
    ls_bcset_text-text    = 'Generated via ABAPGIT'(004).
    APPEND ls_bcset_text TO ls_bcset_metadata-scprtext[].

*   Populate records and values metadata
    ls_bcset_metadata-scprreca[] = it_record_attribute[].
    LOOP AT ls_bcset_metadata-scprreca[] ASSIGNING <ls_recattr>.
      <ls_recattr>-id = iv_bcset_id.
    ENDLOOP. " LOOP AT ls_bcset_metadata-scprreca[] ASSIGNING <ls_recattr>

    ls_bcset_metadata-scprvals[] = it_bcset_values[].
    LOOP AT ls_bcset_metadata-scprvals[] ASSIGNING <ls_values>.
      <ls_values>-id = iv_bcset_id.
    ENDLOOP. " LOOP AT ls_bcset_metadata-scprvals[] ASSIGNING <ls_values>

*   Get language values
    CALL FUNCTION 'SCPR_TEMPL_CT_LANG_ALL_GET'
      EXPORTING
        bcset_id    = ls_bcset_metadata-scprattr-id
      TABLES
        values      = ls_bcset_metadata-scprvals[]
        valuesl     = ls_bcset_metadata-scprvall[]
        recattr     = ls_bcset_metadata-scprreca[]
        fielddescrs = mt_fielddescrs[].

*   Add metadata to XML
    ro_xml_output->add( iv_name = mc_bcset          " SCP1
                        ig_data = ls_bcset_metadata ).

  ENDMETHOD.


  METHOD create_object_files.

*   Declaration of local workarea
    DATA: ls_item TYPE zif_abapgit_definitions=>ty_item.

    ls_item-obj_type = mc_bcset.    " SCP1
    ls_item-obj_name = iv_bcset_id.
    ls_item-devclass = iv_devclass.

*   Instantiate object files object
    CREATE OBJECT ro_object_files
      EXPORTING
        is_item = ls_item.

*   Add XML data to file
    ro_object_files->add_xml( io_xml ).

  ENDMETHOD.


  METHOD stage_files.

*   Declaration of local workarea
    DATA: ls_files_and_item TYPE zcl_abapgit_objects=>ty_serialization.

*   Declaration of local field symbols
    FIELD-SYMBOLS: <ls_file> TYPE zif_abapgit_definitions=>ty_file.

    ls_files_and_item-item-obj_type = mc_bcset.    " SCP1
    ls_files_and_item-item-obj_name = iv_bcset_id.
    ls_files_and_item-item-devclass = iv_devclass.

    ls_files_and_item-files = io_object_files->get_files( ).

    IF mo_staged_files IS NOT BOUND.

*     Instantiate the stage object
      CREATE OBJECT mo_staged_files.

    ENDIF. " IF mo_staged_files IS NOT BOUND

    LOOP AT ls_files_and_item-files[] ASSIGNING <ls_file>.

      <ls_file>-sha1 = zcl_abapgit_hash=>sha1( iv_type = zif_abapgit_definitions=>c_type-blob
                                               iv_data = <ls_file>-data ).

*     Add files to stage
      mo_staged_files->add( iv_path     = <ls_file>-path
                            iv_filename = <ls_file>-filename
                            iv_data     = <ls_file>-data ).

    ENDLOOP. " LOOP AT ls_files_and_item-files[] ASSIGNING <ls_file>

  ENDMETHOD.
ENDCLASS.
