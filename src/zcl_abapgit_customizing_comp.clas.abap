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

      mo_customizing_compare = NEW #( ).

    ENDIF. " IF mo_customizing_compare IS NOT BOUND

    ro_customizing_compare ?= mo_customizing_compare.

  ENDMETHOD.


  METHOD is_a2g_type_bcset.

    rv_is_a2g_type_bcset = SWITCH #( ms_bcset_metadata-scprattr-type WHEN mc_bcset_type THEN abap_true
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

    read_bcset_metadata( is_item         = is_item
                         is_file_details = is_file_details
                       ).

    IF is_a2g_type_bcset( ) = abap_false. " ' '
      RETURN.
    ENDIF. " IF is_a2g_type_bcset( ) = abap_false

    TRY.

*       Create configuration container for remote file
        DATA(lo_remote_container) = create_container( ).

        add_field_values_to_container( lo_remote_container ).

        DATA(lo_key_cobtainer) = lo_remote_container->if_bcfg_config_container~extract_key_container( ).

*       Create configuration container for local file
        DATA(lo_local_container) = create_container( ).

        lo_local_container->if_bcfg_config_container~add_current_config( io_keys = lo_key_cobtainer ). " Option 1: specify keys to be read from the database

        DATA(lv_is_equal) = lo_local_container->if_bcfg_config_container~equals( io_other = lo_remote_container ).

      CATCH cx_bcfg_operation_failed INTO DATA(lo_operation_failed).
        zcx_abapgit_exception=>raise(
          EXPORTING
            iv_text     = 'Operation Failed'(001)
            ix_previous = lo_operation_failed
        ).

    ENDTRY.

    cs_result-match = lv_is_equal.

    IF lv_is_equal = abap_true. " X

      cs_result-lstate = zif_abapgit_definitions=>c_state-unchanged.

    ELSE.

      cs_result-lstate = zif_abapgit_definitions=>c_state-modified.

    ENDIF. " IF lv_is_equal = abap_true

  ENDMETHOD.


  METHOD zif_abapgit_customizing_comp~create_local_file.

    DATA: ls_bcset_metadata TYPE ty_bcset_metadata,
          ls_item           TYPE zif_abapgit_definitions=>ty_item,
          ls_file           TYPE zif_abapgit_definitions=>ty_file_item.

    LOOP AT rs_file-status[] ASSIGNING FIELD-SYMBOL(<ls_status>)
                             WHERE obj_type = mc_bcset. " SCP1

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

      TRY.

*         Create configuration container
          DATA(lo_container) = create_container( ).

          add_field_values_to_container( lo_container ).

          DATA(lo_key_container) = lo_container->if_bcfg_config_container~extract_key_container( ).

          lo_container->if_bcfg_config_container~remove_all( ).

          lo_container->if_bcfg_config_container~add_current_config( lo_key_container ).

          lo_container->if_bcfg_has_data_manager~get_data_manager( )->convert_to_bcset(
            EXPORTING
              iv_id   = lo_container->if_bcfg_config_container~get_id( )
            CHANGING
              ct_reca = ls_bcset_metadata-scprreca[]
              ct_vals = ls_bcset_metadata-scprvals[]
              ct_vall = ls_bcset_metadata-scprvall[]
          ).

        CATCH cx_bcfg_operation_failed INTO DATA(lo_operation_failed).
          zcx_abapgit_exception=>raise(
            EXPORTING
              iv_text     = 'Operation Failed'(001)
              ix_previous = lo_operation_failed
          ).

      ENDTRY.

*     Instantiate the XML object
      DATA(lo_xml_output) = NEW zcl_abapgit_xml_output( ).

      ls_bcset_metadata-scprattr   = ms_bcset_metadata-scprattr.
      ls_bcset_metadata-scprtext   = ms_bcset_metadata-scprtext.

      LOOP AT ls_bcset_metadata-scprreca[] ASSIGNING FIELD-SYMBOL(<ls_recattr>).
        <ls_recattr>-id = ms_bcset_metadata-scprattr-id.
      ENDLOOP. " LOOP AT ls_bcset_metadata-scprreca[] ASSIGNING FIELD-SYMBOL(<ls_recattr>)

      LOOP AT ls_bcset_metadata-scprvals[] ASSIGNING FIELD-SYMBOL(<ls_values>).
        <ls_values>-id = ms_bcset_metadata-scprattr-id.
      ENDLOOP. " LOOP AT ls_bcset_metadata-scprreca[] ASSIGNING FIELD-SYMBOL(<ls_recattr>)

      LOOP AT ls_bcset_metadata-scprvall[] ASSIGNING FIELD-SYMBOL(<ls_valuel>).
        <ls_valuel>-id = ms_bcset_metadata-scprattr-id.
      ENDLOOP. " LOOP AT ls_bcset_metadata-scprreca[] ASSIGNING FIELD-SYMBOL(<ls_recattr>)

*     Add metadata to XML
      lo_xml_output->add(
        EXPORTING
          iv_name = mc_bcset          " SCP1
          ig_data = ls_bcset_metadata
      ).

      ls_item = VALUE #( obj_type = mc_bcset obj_name = ms_bcset_metadata-scprattr-id ).

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


  METHOD zif_abapgit_customizing_comp~apply_customizing_content.

    ms_bcset_metadata = is_bcset_metadata.

    TRY.

*       Create configuration container using the mappings
        DATA(lo_container) = create_container( ).

        add_field_values_to_container( lo_container ).

        DATA(lo_result) = lo_container->if_bcfg_config_container~apply( ).

      CATCH cx_bcfg_operation_failed INTO DATA(lo_operation_failed).
        zcx_abapgit_exception=>raise(
          EXPORTING
            iv_text     = 'Operation Failed'(001)
            ix_previous = lo_operation_failed
        ).

    ENDTRY.

    LOOP AT lo_result->get_log_messages( ) ASSIGNING FIELD-SYMBOL(<ls_return>).

      io_log->add( iv_msg  = <ls_return>-message
                   iv_type = <ls_return>-type
                 ).

    ENDLOOP. " LOOP AT lo_result->get_log_messages( ) ASSIGNING FIELD-SYMBOL(<ls_return>)

  ENDMETHOD.


  METHOD add_field_values_to_container.

*   Declaration of local internal table
    DATA: lt_field_values    TYPE if_bcfg_config_container=>ty_t_field_values,
          lt_field_value_tmp TYPE STANDARD TABLE OF if_bcfg_config_container=>ty_s_field_value.

    lt_field_value_tmp[] = CORRESPONDING #( ms_bcset_metadata-scprvals[] MAPPING rec_id = recnumber ).
    lt_field_values[] = CORRESPONDING #( ms_bcset_metadata-scprvall[] MAPPING rec_id = recnumber ).

    LOOP AT lt_field_values[] ASSIGNING FIELD-SYMBOL(<ls_field_value>).

      APPEND <ls_field_value> TO lt_field_value_tmp[].

    ENDLOOP.

    lt_field_values[] = lt_field_value_tmp[].

    io_container->add_lines_by_fields( it_fields = lt_field_values[] ).

  ENDMETHOD.


  METHOD create_container.

*   Declaration of local internal table
    DATA: lt_objects         TYPE scp1_act_objects,
          lt_mappings        TYPE if_bcfg_config_container=>ty_t_mapping_info,
          lt_field_value_tmp TYPE STANDARD TABLE OF if_bcfg_config_container=>ty_s_field_value,
          lt_languages       TYPE if_bcfg_config_container=>ty_t_languages.

    CALL FUNCTION 'SCPR_ACTIV_EXTRACT_OBJECTS'
      IMPORTING
        act_objects = lt_objects[]
      TABLES
        recattr     = ms_bcset_metadata-scprreca[].

    lt_mappings[] = CORRESPONDING #( lt_objects[] ).

    lt_field_value_tmp[] = CORRESPONDING #( ms_bcset_metadata-scprvall[] MAPPING rec_id = recnumber ).

    SORT lt_field_value_tmp[] BY langu.

    DELETE ADJACENT DUPLICATES FROM lt_field_value_tmp[]
    COMPARING langu.

    LOOP AT lt_field_value_tmp[] ASSIGNING FIELD-SYMBOL(<ls_field_value>).

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


  METHOD zif_abapgit_customizing_comp~display_differences.

    DATA(lo_repo) = zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
    DATA(lt_status) = lo_repo->status( ).
    DATA(lt_remote) = lo_repo->get_files_remote( ).

    READ TABLE lt_status[] ASSIGNING FIELD-SYMBOL(<ls_status>)
                           WITH KEY path     = is_file-path
                                    filename = is_file-filename.
    IF sy-subrc NE 0 OR <ls_status>-obj_type NE mc_bcset. " SCP1
      RETURN.
    ENDIF. " IF sy-subrc NE 0 OR <ls_status>-obj_type NE 'SCP1'

    READ TABLE lt_remote[] ASSIGNING FIELD-SYMBOL(<ls_file_details>)
                           WITH KEY path     = <ls_status>-path
                                    filename = <ls_status>-filename.

    read_bcset_metadata(
      EXPORTING
        is_item         = CORRESPONDING #( <ls_status> )
        is_file_details = <ls_file_details>
    ).

    IF is_a2g_type_bcset( ) = abap_false. " ' '
      RETURN.
    ENDIF. " IF is_a2g_type_bcset( ) = abap_false

  ENDMETHOD.
ENDCLASS.
