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

  constants MC_BCSET type TROBJTYPE value 'SCP1' ##NO_TEXT.
  class-data MO_CUSTOMIZING_COMPARE type ref to ZCL_ABAPGIT_CUSTOMIZING_COMP .
  data MS_BCSET_METADATA type TY_BCSET_METADATA .
  constants MC_BCSET_TYPE type SCPR_TYPE value 'A2G' ##NO_TEXT.

  methods READ_BCSET_METADATA
    importing
      !IS_ITEM type ZIF_ABAPGIT_DEFINITIONS=>TY_ITEM
      !IS_FILE_DETAILS type ZIF_ABAPGIT_DEFINITIONS=>TY_FILE
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods IS_A2G_TYPE_BCSET
    returning
      value(RV_IS_A2G_TYPE_BCSET) type ABAP_BOOL .
  methods CREATE_CONTAINER
    returning
      value(RO_CONTAINER) type ref to CL_BCFG_BCSET_CONFIG_CONTAINER .
  methods ADD_FIELD_VALUES_TO_CONTAINER
    importing
      !IO_CONTAINER type ref to IF_BCFG_CONFIG_CONTAINER .
ENDCLASS.



CLASS ZCL_ABAPGIT_CUSTOMIZING_COMP IMPLEMENTATION.


  METHOD get_instance.

    IF mo_customizing_compare IS NOT BOUND.

      CREATE OBJECT mo_customizing_compare.

    ENDIF. " IF mo_customizing_compare IS NOT BOUND

    ro_customizing_compare ?= mo_customizing_compare.

  ENDMETHOD.


  METHOD is_a2g_type_bcset.

    IF ms_bcset_metadata-scprattr-type = mc_bcset_type. " A2G

      rv_is_a2g_type_bcset = abap_true. " X

    ENDIF. " IF ms_bcset_metadata-scprattr-type = mc_bcset_type

  ENDMETHOD.


  METHOD read_bcset_metadata.

*   Declaration of local object reference
    DATA: lo_object_files TYPE REF TO zcl_abapgit_objects_files,
          lo_xml_file     TYPE REF TO zcl_abapgit_xml_input.

*   Instantiate object files object
    CREATE OBJECT lo_object_files
      EXPORTING
        is_item = is_item.

    lo_object_files->add( is_file = is_file_details ).

    lo_xml_file = lo_object_files->read_xml( ).

    lo_xml_file->read(
      EXPORTING
        iv_name = is_item-obj_type
      CHANGING
        cg_data = ms_bcset_metadata
    ).

  ENDMETHOD.


  METHOD zif_abapgit_customizing_comp~compare_customizing_with_table.

*   Declaration of local object reference
    DATA: lo_remote_container TYPE REF TO cl_bcfg_bcset_config_container,
          lo_local_container  TYPE REF TO cl_bcfg_bcset_config_container,
          lo_key_container    TYPE REF TO if_bcfg_key_container,
          lo_operation_failed TYPE REF TO cx_bcfg_operation_failed.

*   Declaration of local variables
    DATA: lv_is_a2g_type_bcset TYPE abap_bool,
          lv_is_equal          TYPE abap_bool.

    read_bcset_metadata( is_item         = is_item
                         is_file_details = is_file_details ).

    lv_is_a2g_type_bcset = is_a2g_type_bcset( ).
    IF lv_is_a2g_type_bcset = abap_false. " ' '
      RETURN.
    ENDIF. " IF lv_is_a2g_type_bcset = abap_false

    TRY.

*       Create configuration container for remote file
        lo_remote_container = create_container( ).

        add_field_values_to_container( lo_remote_container ).

        lo_key_container = lo_remote_container->if_bcfg_config_container~extract_key_container( ).

*       Create configuration container for local file
        lo_local_container = create_container( ).

*       Get local data
        lo_local_container->if_bcfg_config_container~add_current_config( io_keys = lo_key_container ). " Option 1: specify keys to be read from the database

        lv_is_equal = lo_local_container->if_bcfg_config_container~equals( io_other = lo_remote_container ).

      CATCH cx_bcfg_operation_failed INTO lo_operation_failed.
        zcx_abapgit_exception=>raise(
          EXPORTING
            iv_text     = 'Operation Failed'(001)
            ix_previous = lo_operation_failed ).

    ENDTRY.

    cs_result-match = lv_is_equal.

    IF lv_is_equal = abap_true. " X

      cs_result-lstate = zif_abapgit_definitions=>c_state-unchanged.

    ELSE.

      cs_result-lstate = zif_abapgit_definitions=>c_state-modified.

    ENDIF. " IF lv_is_equal = abap_true

  ENDMETHOD.


  METHOD zif_abapgit_customizing_comp~create_local_file.

*   Declaration of local object reference
    DATA: lo_container        TYPE REF TO cl_bcfg_bcset_config_container,
          lo_key_container    TYPE REF TO if_bcfg_key_container,
          lo_operation_failed TYPE REF TO cx_bcfg_operation_failed,
          lo_xml_output       TYPE REF TO zcl_abapgit_xml_output,
          lo_object_files     TYPE REF TO zcl_abapgit_objects_files.

*   Declaration of local internal table
    DATA: lt_files TYPE zif_abapgit_definitions=>ty_files_tt.

*   Declaration of local workarea
    DATA: ls_bcset_metadata TYPE ty_bcset_metadata,
          ls_item           TYPE zif_abapgit_definitions=>ty_item,
          ls_file           TYPE zif_abapgit_definitions=>ty_file_item.

*   Declaration of local variables
    DATA: lv_is_a2g_type_bcset TYPE abap_bool.

*   Declaration of lcoal field symbols
    FIELD-SYMBOLS: <ls_status>  TYPE zif_abapgit_definitions=>ty_result,
                   <ls_remote>  TYPE zif_abapgit_definitions=>ty_file,
                   <ls_recattr> TYPE scprreca,
                   <ls_values>  TYPE scprvals,
                   <ls_valuel>  TYPE scprvall.

    LOOP AT cs_file-status[] ASSIGNING <ls_status>
                             WHERE obj_type = mc_bcset. " SCP1

      READ TABLE cs_file-remote[] ASSIGNING <ls_remote>
                                  WITH KEY path     = <ls_status>-path
                                           filename = <ls_status>-filename.
      IF sy-subrc = 0.

        MOVE-CORRESPONDING <ls_status> TO ls_item.

        read_bcset_metadata( is_item         = ls_item
                             is_file_details = <ls_remote> ).

        lv_is_a2g_type_bcset = is_a2g_type_bcset( ).
        IF lv_is_a2g_type_bcset = abap_false. " ' '
          RETURN.
        ENDIF. " IF lv_is_a2g_type_bcset = abap_false

        TRY.

*           Create configuration container
            lo_container = create_container( ).

            add_field_values_to_container( lo_container ).

            lo_key_container = lo_container->if_bcfg_config_container~extract_key_container( ).

            lo_container->if_bcfg_config_container~remove_all( ).

            lo_container->if_bcfg_config_container~add_current_config( lo_key_container ).

            lo_container->if_bcfg_has_data_manager~get_data_manager( )->convert_to_bcset(
              EXPORTING
                iv_id   = lo_container->if_bcfg_config_container~get_id( )
              CHANGING
                ct_reca = ls_bcset_metadata-scprreca[]
                ct_vals = ls_bcset_metadata-scprvals[]
                ct_vall = ls_bcset_metadata-scprvall[] ).

          CATCH cx_bcfg_operation_failed INTO lo_operation_failed.
            zcx_abapgit_exception=>raise( iv_text     = 'Operation Failed'(001)
                                          ix_previous = lo_operation_failed ).

        ENDTRY.

*       Instantiate the XML object
        CREATE OBJECT lo_xml_output.

        ls_bcset_metadata-scprattr   = ms_bcset_metadata-scprattr.
        ls_bcset_metadata-scprtext   = ms_bcset_metadata-scprtext.

        LOOP AT ls_bcset_metadata-scprreca[] ASSIGNING <ls_recattr>.
          <ls_recattr>-id = ms_bcset_metadata-scprattr-id.
        ENDLOOP. " LOOP AT ls_bcset_metadata-scprreca[] ASSIGNING <ls_recattr>

        LOOP AT ls_bcset_metadata-scprvals[] ASSIGNING <ls_values>.
          <ls_values>-id = ms_bcset_metadata-scprattr-id.
        ENDLOOP. " LOOP AT ls_bcset_metadata-scprreca[] ASSIGNING <ls_recattr>

        LOOP AT ls_bcset_metadata-scprvall[] ASSIGNING <ls_valuel>.
          <ls_valuel>-id = ms_bcset_metadata-scprattr-id.
        ENDLOOP. " LOOP AT ls_bcset_metadata-scprreca[] ASSIGNING <ls_recattr>

*       Add metadata to XML
        lo_xml_output->add( iv_name = mc_bcset          " SCP1
                            ig_data = ls_bcset_metadata ).

        ls_item-obj_type = mc_bcset. " SCP1
        ls_item-obj_name = ms_bcset_metadata-scprattr-id.

*       Instantiate object files object
        CREATE OBJECT lo_object_files
          EXPORTING
            is_item = ls_item.

*       Add XML data to file
        lo_object_files->add_xml( io_xml = lo_xml_output ).

        lt_files[] = lo_object_files->get_files( ).

        LOOP AT lt_files[] ASSIGNING <ls_remote>.

          MOVE-CORRESPONDING <ls_remote> TO ls_file-file.
          ls_file-file-filename = <ls_status>-filename.
          ls_file-file-sha1 = zcl_abapgit_hash=>sha1( iv_type = zif_abapgit_definitions=>c_type-blob
                                                      iv_data = ls_file-file-data ).

        ENDLOOP.

        ls_file-item = ls_item.
        APPEND ls_file TO cs_file-local[].

      ENDIF. " IF sy-subrc = 0

    ENDLOOP. " LOOP AT cs_file-status[] ASSIGNING <ls_status>

  ENDMETHOD.


  METHOD zif_abapgit_customizing_comp~apply_customizing_content.

*   Declaration of local object reference
    DATA: lo_container        TYPE REF TO cl_bcfg_bcset_config_container,
          lo_result           TYPE REF TO if_bcfg_result_apply,
          lo_operation_failed TYPE REF TO cx_bcfg_operation_failed.

*   Declaration of local internal table
    DATA: lt_return TYPE bapiret2_t.

*   Declaration of local field symbols
    FIELD-SYMBOLS: <ls_return> TYPE bapiret2.

    ms_bcset_metadata = is_bcset_metadata.

    TRY.

*       Create configuration container using the mappings
        lo_container = create_container( ).

        add_field_values_to_container( lo_container ).

        lo_result = lo_container->if_bcfg_config_container~apply( ).

      CATCH cx_bcfg_operation_failed INTO lo_operation_failed.
        zcx_abapgit_exception=>raise( iv_text     = 'Operation Failed'(001)
                                      ix_previous = lo_operation_failed ).

    ENDTRY.

    lt_return[] = lo_result->get_log_messages( ).

    LOOP AT lt_return[] ASSIGNING <ls_return>.

      io_log->add( iv_msg  = <ls_return>-message
                   iv_type = <ls_return>-type ).

    ENDLOOP. " LOOP AT lo_result->get_log_messages( ) ASSIGNING <ls_return>

  ENDMETHOD.


  METHOD add_field_values_to_container.

*   Declaration of local internal table
    DATA: lt_field_values    TYPE if_bcfg_config_container=>ty_t_field_values,
          lt_field_value_tmp TYPE STANDARD TABLE OF if_bcfg_config_container=>ty_s_field_value.

*   Declaration of local workarea
    DATA: ls_field_value TYPE if_bcfg_config_container=>ty_s_field_value.

*   Declaration of local field symbols
    FIELD-SYMBOLS: <ls_values> TYPE scprvals,
                   <ls_valuel> TYPE scprvall.

*   Convert BC set field values to configuration container format
    LOOP AT ms_bcset_metadata-scprvals[] ASSIGNING <ls_values>.

      MOVE-CORRESPONDING <ls_values> TO ls_field_value.
      ls_field_value-rec_id = <ls_values>-recnumber.
      INSERT ls_field_value INTO TABLE lt_field_values[].

      CLEAR ls_field_value.

    ENDLOOP. " LOOP AT ms_bcset_metadata-scprvals[] ASSIGNING <ls_values>

    LOOP AT ms_bcset_metadata-scprvall[] ASSIGNING <ls_valuel>.

      MOVE-CORRESPONDING <ls_valuel> TO ls_field_value.
      ls_field_value-rec_id = <ls_values>-recnumber.
      INSERT ls_field_value INTO TABLE lt_field_values[].

      CLEAR ls_field_value.

    ENDLOOP. " LOOP AT ms_bcset_metadata-scprvall[] ASSIGNING <ls_valuel>

    io_container->add_lines_by_fields( it_fields = lt_field_values[] ).

  ENDMETHOD.


  METHOD create_container.

*   Declaration of local internal table
    DATA: lt_objects     TYPE scp1_act_objects,
          lt_mappings    TYPE if_bcfg_config_container=>ty_t_mapping_info,
          lt_field_value TYPE STANDARD TABLE OF if_bcfg_config_container=>ty_s_field_value,
          lt_languages   TYPE if_bcfg_config_container=>ty_t_languages.

*   Declaration of local workarea
    DATA: ls_mapping     TYPE if_bcfg_config_container=>ty_s_mapping_info,
          ls_field_value TYPE if_bcfg_config_container=>ty_s_field_value.

*   Declaration of local field symbol
    FIELD-SYMBOLS: <ls_object>      TYPE scp1_act_object,
                   <ls_valuel>      TYPE scprvall,
                   <ls_field_value> TYPE if_bcfg_config_container=>ty_s_field_value.

    CALL FUNCTION 'SCPR_ACTIV_EXTRACT_OBJECTS'
      IMPORTING
        act_objects = lt_objects[]
      TABLES
        recattr     = ms_bcset_metadata-scprreca[].

    LOOP AT lt_objects[] ASSIGNING <ls_object>.

      MOVE-CORRESPONDING <ls_object> TO ls_mapping.
      APPEND ls_mapping TO lt_mappings[].

      CLEAR ls_mapping.

    ENDLOOP. " LOOP AT lt_objects[] ASSIGNING <ls_object>

    LOOP AT ms_bcset_metadata-scprvall[] ASSIGNING <ls_valuel>.

      MOVE-CORRESPONDING <ls_valuel> TO ls_field_value.
      ls_field_value-rec_id = <ls_valuel>-recnumber.
      APPEND ls_field_value TO lt_field_value[].

      CLEAR ls_field_value.

    ENDLOOP. " LOOP AT ms_bcset_metadata-scprvall[] ASSIGNING <ls_valuel>

    SORT lt_field_value[] BY langu.

    DELETE ADJACENT DUPLICATES FROM lt_field_value[]
    COMPARING langu.

    LOOP AT lt_field_value[] ASSIGNING <ls_field_value>.

      APPEND <ls_field_value>-langu TO lt_languages[].

    ENDLOOP.
    IF sy-subrc NE 0.
      APPEND sy-langu TO lt_languages[].
    ENDIF.

*   Create configuration container using the mappings
    ro_container ?= cl_bcfg_config_manager=>create_container( io_container_type  = cl_bcfg_enum_container_type=>classic  " CLASSIC
                                                              it_object_mappings = lt_mappings[]
                                                              it_langus          = lt_languages[]
                                                              io_commit_mode     = cl_bcfg_enum_commit_mode=>auto_commit " AUTO_COMMIT
                                                            ).

  ENDMETHOD.
ENDCLASS.
