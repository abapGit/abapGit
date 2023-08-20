CLASS zcl_abapgit_deserialize_repo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS deserialize
      IMPORTING
        !io_repo                 TYPE REF TO zcl_abapgit_repo
        !is_checks               TYPE zif_abapgit_definitions=>ty_deserialize_checks
        !ii_log                  TYPE REF TO zif_abapgit_log
      RETURNING
        VALUE(rt_accessed_files) TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt
      RAISING
        zcx_abapgit_exception .


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_deserialize_repo IMPLEMENTATION.

METHOD deserialize.

*    DATA: ls_item     TYPE zif_abapgit_definitions=>ty_item,
*          li_obj      TYPE REF TO zif_abapgit_object,
*          lt_remote   TYPE zif_abapgit_git_definitions=>ty_files_tt,
*          lv_package  TYPE devclass,
*          lo_files    TYPE REF TO zcl_abapgit_objects_files,
*          ls_metadata TYPE zif_abapgit_definitions=>ty_metadata,
*          lo_xml      TYPE REF TO zif_abapgit_xml_input,
*          lt_results  TYPE zif_abapgit_definitions=>ty_results_tt,
*          li_progress TYPE REF TO zif_abapgit_progress,
*          lv_path     TYPE string,
*          lt_items    TYPE zif_abapgit_definitions=>ty_items_tt,
*          lt_steps_id TYPE zif_abapgit_definitions=>ty_deserialization_step_tt,
*          lt_steps    TYPE zif_abapgit_objects=>ty_step_data_tt,
*          lx_exc      TYPE REF TO zcx_abapgit_exception.
*    DATA lo_folder_logic TYPE REF TO zcl_abapgit_folder_logic.
*    DATA lo_i18n_params TYPE REF TO zcl_abapgit_i18n_params.
*    DATA lo_timer TYPE REF TO zcl_abapgit_timer.
*    DATA lo_abap_language_vers TYPE REF TO zcl_abapgit_abap_language_vers.
*
*    FIELD-SYMBOLS: <ls_result>  TYPE zif_abapgit_definitions=>ty_result,
*                   <lv_step_id> TYPE LINE OF zif_abapgit_definitions=>ty_deserialization_step_tt,
*                   <ls_step>    TYPE LINE OF zif_abapgit_objects=>ty_step_data_tt,
*                   <ls_deser>   TYPE LINE OF zif_abapgit_objects=>ty_deserialization_tt.
*
*    lt_steps = get_deserialize_steps( ).
*
*    lv_package = io_repo->get_package( ).
*
*    IF is_checks-transport-required = abap_true.
*      zcl_abapgit_default_transport=>get_instance( )->set( is_checks-transport-transport ).
*    ENDIF.
*
*    zcl_abapgit_objects_activation=>clear( ).
*
*    lt_remote = io_repo->get_files_remote( iv_ignore_files = abap_true ).
*
*    lt_results = zcl_abapgit_file_deserialize=>get_results(
*      io_repo = io_repo
*      ii_log = ii_log ).
*
*    IF lt_results IS INITIAL.
*      RETURN.
*    ENDIF.
*
*    zcl_abapgit_objects_check=>checks_adjust(
*      EXPORTING
*        io_repo    = io_repo
*        is_checks  = is_checks
*      CHANGING
*        ct_results = lt_results ).
*
*    li_progress = zcl_abapgit_progress=>get_instance( lines( lt_results ) ).
*
*    lt_items = map_results_to_items( lt_results ).
*
*    lo_timer = zcl_abapgit_timer=>create(
*      iv_text  = 'Deserialize:'
*      iv_count = lines( lt_items ) )->start( ).
*
*    zcl_abapgit_factory=>get_cts_api( )->confirm_transport_messages( ).
*
*    check_objects_locked( iv_language = io_repo->get_dot_abapgit( )->get_main_language( )
*                          it_items    = lt_items ).
*
*    lo_i18n_params = zcl_abapgit_i18n_params=>new( is_params = determine_i18n_params(
*      io_dot                = io_repo->get_dot_abapgit( )
*      iv_main_language_only = io_repo->get_local_settings( )-main_language_only ) ).
*
*    IF lines( lt_items ) = 1.
*      ii_log->add_info( |>>> Deserializing 1 object| ).
*    ELSE.
*      ii_log->add_info( |>>> Deserializing { lines( lt_items ) } objects| ).
*    ENDIF.
*
*    CREATE OBJECT lo_abap_language_vers.
*
*    lo_folder_logic = zcl_abapgit_folder_logic=>get_instance( ).
*    LOOP AT lt_results ASSIGNING <ls_result>.
*      li_progress->show( iv_current = sy-tabix
*                         iv_text    = |Prepare Deserialize: { <ls_result>-obj_type } { <ls_result>-obj_name }| ).
*
*      CLEAR ls_item.
*      ls_item-obj_type = <ls_result>-obj_type.
*      ls_item-obj_name = <ls_result>-obj_name.
*
*      "error handling & logging added
*      TRY.
*          IF ls_item-obj_type <> 'NSPC'.
*            " If package does not exist yet, it will be created with this call
*            lv_package = lo_folder_logic->path_to_package(
*              iv_top  = io_repo->get_package( )
*              io_dot  = io_repo->get_dot_abapgit( )
*              iv_path = <ls_result>-path ).
*
*            check_main_package(
*              iv_package  = lv_package
*              iv_obj_type = ls_item-obj_type ).
*          ENDIF.
*
*          IF ls_item-obj_type = 'DEVC'.
*            " Packages have the same filename across different folders. The path needs to be supplied
*            " to find the correct file.
*            lv_path = <ls_result>-path.
*          ENDIF.
*
*          ls_item-devclass = lv_package.
*          ls_item-abap_language_version = lo_abap_language_vers->get_abap_language_vers_by_objt(
*                                                                    iv_object_type = ls_item-obj_type
*                                                                    iv_package = lv_package ).
*
*          IF <ls_result>-packmove = abap_true.
*            " Move object to new package
*            change_package_assignments( is_item = ls_item
*                                        ii_log  = ii_log ).
*            " No other changes required
*            CONTINUE.
*          ENDIF.
*
*          " Create or update object
*          CREATE OBJECT lo_files
*            EXPORTING
*              is_item = ls_item
*              iv_path = lv_path.
*
*          lo_files->set_files( lt_remote ).
*
*          IF lo_files->is_json_metadata( ) = abap_false.
*            "analyze XML in order to instantiate the proper serializer
*            lo_xml = lo_files->read_xml( ).
*            ls_metadata = lo_xml->get_metadata( ).
*          ELSE.
*            " there's no XML and metadata for JSON format
*            CLEAR: lo_xml, ls_metadata.
*          ENDIF.
*
*          li_obj = create_object(
*            is_item        = ls_item
*            is_metadata    = ls_metadata
*            io_i18n_params = lo_i18n_params ).
*
*          compare_remote_to_local(
*            ii_object = li_obj
*            it_remote = lt_remote
*            is_result = <ls_result>
*            ii_log    = ii_log ).
*
*          li_obj->mo_files = lo_files.
*
*          "get required steps for deserialize the object
*          lt_steps_id = li_obj->get_deserialize_steps( ).
*
*          LOOP AT lt_steps_id ASSIGNING <lv_step_id>.
*            READ TABLE lt_steps WITH KEY step_id = <lv_step_id> ASSIGNING <ls_step>.
*            ASSERT sy-subrc = 0.
*            IF <lv_step_id> = zif_abapgit_object=>gc_step_id-ddic AND
*               zcl_abapgit_objects_activation=>is_ddic_type( ls_item-obj_type ) = abap_false.
*              " DDIC only for DDIC objects
*              zcx_abapgit_exception=>raise( |Step { <lv_step_id> } is only for DDIC objects| ).
*            ENDIF.
*            APPEND INITIAL LINE TO <ls_step>-objects ASSIGNING <ls_deser>.
*            <ls_deser>-item    = ls_item.
*            <ls_deser>-obj     = li_obj.
*            <ls_deser>-xml     = lo_xml.
*            <ls_deser>-package = lv_package.
*          ENDLOOP.
*
*          " LXE, TODO refactor and move below activation
*          IF lo_i18n_params->is_lxe_applicable( ) = abap_true.
*            zcl_abapgit_factory=>get_lxe_texts( )->deserialize(
*              iv_object_type = ls_item-obj_type
*              iv_object_name = ls_item-obj_name
*              io_i18n_params = lo_i18n_params
*              ii_xml         = lo_xml
*              io_files       = lo_files ).
*          ENDIF.
*
*          CLEAR: lv_path, lv_package.
*
*        CATCH zcx_abapgit_exception INTO lx_exc.
*          ii_log->add_exception( ix_exc = lx_exc
*                                 is_item = ls_item ).
*          ii_log->add_error( iv_msg = |Import of object { ls_item-obj_name } failed|
*                             is_item = ls_item ).
*          "object should not be part of any deserialization step
*          CONTINUE.
*      ENDTRY.
*
*    ENDLOOP.
*
*    li_progress->off( ).
*
*    "run deserialize for all steps and its objects
*    deserialize_steps(
*      EXPORTING
*        it_steps     = lt_steps
*        ii_log       = ii_log
*        iv_transport = is_checks-transport-transport
*      CHANGING
*        ct_files     = rt_accessed_files ).
*
*    " TODO: LXE translations (objects has been activated by now)
*
*    update_package_tree( io_repo->get_package( ) ).
*
*    zcl_abapgit_default_transport=>get_instance( )->reset( ).
*
*    lo_timer->end( abap_true ).

  ENDMETHOD.

ENDCLASS.
