CLASS zcl_abapgit_repo DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_repo.

    ALIASES ms_data FOR zif_abapgit_repo~ms_data.
    ALIASES:
      get_key FOR zif_abapgit_repo~get_key,
      get_name FOR zif_abapgit_repo~get_name,
      is_offline FOR zif_abapgit_repo~is_offline,
      get_package FOR zif_abapgit_repo~get_package,
      get_files_local FOR zif_abapgit_repo~get_files_local,
      get_files_remote FOR zif_abapgit_repo~get_files_remote,
      get_local_settings FOR zif_abapgit_repo~get_local_settings,
      refresh FOR zif_abapgit_repo~refresh,
      get_dot_abapgit FOR zif_abapgit_repo~get_dot_abapgit,
      set_dot_abapgit FOR zif_abapgit_repo~set_dot_abapgit,
      deserialize FOR zif_abapgit_repo~deserialize,
      deserialize_checks FOR zif_abapgit_repo~deserialize_checks.

    METHODS constructor
      IMPORTING
        !is_data TYPE zif_abapgit_persistence=>ty_repo .

    METHODS bind_listener
      IMPORTING
        !ii_listener TYPE REF TO zif_abapgit_repo_listener .
    METHODS delete_checks
      RETURNING
        VALUE(rs_checks) TYPE zif_abapgit_definitions=>ty_delete_checks
      RAISING
        zcx_abapgit_exception .
    METHODS get_dot_apack
      RETURNING
        VALUE(ro_dot_apack) TYPE REF TO zcl_abapgit_apack_reader .
    METHODS get_data_config
      RETURNING
        VALUE(ri_config) TYPE REF TO zif_abapgit_data_config
      RAISING
        zcx_abapgit_exception .
    METHODS find_remote_dot_abapgit
      RETURNING
        VALUE(ro_dot) TYPE REF TO zcl_abapgit_dot_abapgit
      RAISING
        zcx_abapgit_exception .
    METHODS set_files_remote
      IMPORTING
        !it_files TYPE zif_abapgit_git_definitions=>ty_files_tt .
    METHODS set_local_settings
      IMPORTING
        !is_settings TYPE zif_abapgit_persistence=>ty_repo-local_settings
      RAISING
        zcx_abapgit_exception .
    METHODS has_remote_source
      ABSTRACT
      RETURNING
        VALUE(rv_yes) TYPE abap_bool .
    METHODS status
      IMPORTING
        !ii_log           TYPE REF TO zif_abapgit_log OPTIONAL
      RETURNING
        VALUE(rt_results) TYPE zif_abapgit_definitions=>ty_results_tt
      RAISING
        zcx_abapgit_exception .
    METHODS switch_repo_type
      IMPORTING
        !iv_offline TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS create_new_log
      IMPORTING
        !iv_title     TYPE string OPTIONAL
      RETURNING
        VALUE(ri_log) TYPE REF TO zif_abapgit_log .
    METHODS get_log
      RETURNING
        VALUE(ri_log) TYPE REF TO zif_abapgit_log .
    METHODS refresh_local_object
      IMPORTING
        !iv_obj_type TYPE tadir-object
        !iv_obj_name TYPE tadir-obj_name
      RAISING
        zcx_abapgit_exception .
    METHODS refresh_local_objects
      RAISING
        zcx_abapgit_exception .
    METHODS reset_status .
    METHODS get_unsupported_objects_local
      RETURNING
        VALUE(rt_objects) TYPE zif_abapgit_definitions=>ty_items_tt
      RAISING
        zcx_abapgit_exception .
    METHODS remove_ignored_files
      CHANGING
        ct_files TYPE zif_abapgit_git_definitions=>ty_files_tt
      RAISING
        zcx_abapgit_exception .
    METHODS check_and_create_package
      IMPORTING
        iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.

    DATA mt_local TYPE zif_abapgit_definitions=>ty_files_item_tt .
    DATA mt_remote TYPE zif_abapgit_git_definitions=>ty_files_tt .
    DATA mv_request_local_refresh TYPE abap_bool .
    DATA mv_request_remote_refresh TYPE abap_bool .
    DATA mt_status TYPE zif_abapgit_definitions=>ty_results_tt .
    DATA mi_log TYPE REF TO zif_abapgit_log .
    DATA mi_listener TYPE REF TO zif_abapgit_repo_listener .
    DATA mo_apack_reader TYPE REF TO zcl_abapgit_apack_reader .
    DATA mi_data_config TYPE REF TO zif_abapgit_data_config .

    METHODS find_remote_dot_apack
      RETURNING
        VALUE(ro_dot) TYPE REF TO zcl_abapgit_apack_reader
      RAISING
        zcx_abapgit_exception .
    METHODS set_dot_apack
      IMPORTING
        !io_dot_apack TYPE REF TO zcl_abapgit_apack_reader
      RAISING
        zcx_abapgit_exception .
    METHODS set
      IMPORTING
        !iv_url             TYPE zif_abapgit_persistence=>ty_repo-url OPTIONAL
        !iv_branch_name     TYPE zif_abapgit_persistence=>ty_repo-branch_name OPTIONAL
        !iv_selected_commit TYPE zif_abapgit_persistence=>ty_repo-selected_commit OPTIONAL
        !iv_head_branch     TYPE zif_abapgit_persistence=>ty_repo-head_branch OPTIONAL
        !iv_offline         TYPE zif_abapgit_persistence=>ty_repo-offline OPTIONAL
        !is_dot_abapgit     TYPE zif_abapgit_persistence=>ty_repo-dot_abapgit OPTIONAL
        !is_local_settings  TYPE zif_abapgit_persistence=>ty_repo-local_settings OPTIONAL
        !iv_deserialized_at TYPE zif_abapgit_persistence=>ty_repo-deserialized_at OPTIONAL
        !iv_deserialized_by TYPE zif_abapgit_persistence=>ty_repo-deserialized_by OPTIONAL
        !iv_switched_origin TYPE zif_abapgit_persistence=>ty_repo-switched_origin OPTIONAL
      RAISING
        zcx_abapgit_exception .
    METHODS reset_remote .
  PRIVATE SECTION.

    METHODS deserialize_dot_abapgit
      CHANGING
        ct_files TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt
      RAISING
        zcx_abapgit_exception.

    METHODS deserialize_objects
      IMPORTING
        !is_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks
        !ii_log    TYPE REF TO zif_abapgit_log
      CHANGING
        ct_files   TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt
      RAISING
        zcx_abapgit_exception.

    METHODS deserialize_data
      IMPORTING
        !is_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks
      CHANGING
        ct_files   TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt
      RAISING
        zcx_abapgit_exception.

    METHODS notify_listener
      IMPORTING
        !is_change_mask TYPE zif_abapgit_persistence=>ty_repo_meta_mask
      RAISING
        zcx_abapgit_exception .
    METHODS update_last_deserialize
      RAISING
        zcx_abapgit_exception .
    METHODS check_for_restart .
    METHODS check_write_protect
      RAISING
        zcx_abapgit_exception .
    METHODS check_language
      RAISING
        zcx_abapgit_exception .
    METHODS normalize_local_settings
      CHANGING
        cs_local_settings TYPE zif_abapgit_persistence=>ty_local_settings.
ENDCLASS.



CLASS zcl_abapgit_repo IMPLEMENTATION.


  METHOD bind_listener.
    mi_listener = ii_listener.
  ENDMETHOD.


  METHOD check_and_create_package.

    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.
    DATA lv_package TYPE devclass.

    ls_item-obj_type = 'DEVC'.
    ls_item-obj_name = iv_package.

    IF zcl_abapgit_objects=>exists( ls_item ) = abap_false.
      " Check if any package is included in remote
      READ TABLE mt_remote TRANSPORTING NO FIELDS
        WITH KEY file
        COMPONENTS filename = zcl_abapgit_filename_logic=>c_package_file.
      IF sy-subrc <> 0.
        " If not, prompt to create it
        lv_package = zcl_abapgit_services_basis=>create_package( iv_package ).
        IF lv_package IS NOT INITIAL.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD check_for_restart.

    CONSTANTS:
      lc_abapgit_prog TYPE progname VALUE `ZABAPGIT`.

    " If abapGit was used to update itself, then restart to avoid LOAD_PROGRAM_&_MISMATCH dumps
    " because abapGit code was changed at runtime
    IF zcl_abapgit_ui_factory=>get_frontend_services( )->gui_is_available( ) = abap_true AND
       zcl_abapgit_url=>is_abapgit_repo( ms_data-url ) = abap_true AND
       sy-batch = abap_false AND
       sy-cprog = lc_abapgit_prog.

      IF zcl_abapgit_persist_factory=>get_settings( )->read( )->get_show_default_repo( ) = abap_false.
        MESSAGE 'abapGit was updated and will restart itself' TYPE 'I'.
      ENDIF.

      SUBMIT (sy-cprog).

    ENDIF.

  ENDMETHOD.


  METHOD check_language.

    DATA:
      lv_main_language  TYPE spras,
      lv_error_message  TYPE string,
      lv_error_longtext TYPE string.

    " for deserialize, assumes find_remote_dot_abapgit has been called before (or language won't be defined)
    lv_main_language = get_dot_abapgit( )->get_main_language( ).

    IF lv_main_language <> sy-langu.

      lv_error_message = |Current login language |
                      && |'{ zcl_abapgit_convert=>conversion_exit_isola_output( sy-langu ) }'|
                      && | does not match main language |
                      && |'{ zcl_abapgit_convert=>conversion_exit_isola_output( lv_main_language ) }'.|.

      " Feature open in main language only exists if abapGit tcode is present
      IF zcl_abapgit_services_abapgit=>get_abapgit_tcode( ) IS INITIAL.
        lv_error_message = lv_error_message && | Please logon in main language and retry.|.
        lv_error_longtext = |For the Advanced menu option 'Open in Main Language' to be available a transaction code| &&
                            | must be assigned to report { sy-cprog }.|.
      ELSE.
        lv_error_message = lv_error_message && | Select 'Advanced' > 'Open in Main Language'|.
      ENDIF.

      zcx_abapgit_exception=>raise( iv_text     = lv_error_message
                                    iv_longtext = lv_error_longtext ).

    ENDIF.

  ENDMETHOD.


  METHOD check_write_protect.

    IF get_local_settings( )-write_protected = abap_true.
      zcx_abapgit_exception=>raise( 'Cannot deserialize. Local code is write-protected by repo config' ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    ASSERT NOT is_data-key IS INITIAL.

    ms_data = is_data.
    mv_request_remote_refresh = abap_true.

  ENDMETHOD.


  METHOD create_new_log.

    CREATE OBJECT mi_log TYPE zcl_abapgit_log.
    mi_log->set_title( iv_title ).

    ri_log = mi_log.

  ENDMETHOD.


  METHOD delete_checks.

    DATA: li_package TYPE REF TO zif_abapgit_sap_package.

    check_write_protect( ).
    check_language( ).

    li_package = zcl_abapgit_factory=>get_sap_package( get_package( ) ).
    rs_checks-transport-required = li_package->are_changes_recorded_in_tr_req( ).

  ENDMETHOD.


  METHOD deserialize_data.

    DATA:
      lt_updated_files TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt,
      lt_result        TYPE zif_abapgit_data_deserializer=>ty_results.

    "Deserialize data
    lt_result = zcl_abapgit_data_factory=>get_deserializer( )->deserialize(
      ii_config  = get_data_config( )
      it_files   = get_files_remote( ) ).

    "Save deserialized data to DB and add entries to transport requests
    lt_updated_files = zcl_abapgit_data_factory=>get_deserializer( )->actualize(
      it_result = lt_result
      is_checks = is_checks ).

    INSERT LINES OF lt_updated_files INTO TABLE ct_files.

  ENDMETHOD.


  METHOD deserialize_dot_abapgit.
    INSERT get_dot_abapgit( )->get_signature( ) INTO TABLE ct_files.
  ENDMETHOD.


  METHOD deserialize_objects.

    DATA:
      lt_updated_files TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt,
      lx_error         TYPE REF TO zcx_abapgit_exception.

    TRY.
        lt_updated_files = zcl_abapgit_objects=>deserialize(
          io_repo   = me
          is_checks = is_checks
          ii_log    = ii_log ).
      CATCH zcx_abapgit_exception INTO lx_error.
        " Ensure to reset default transport request task
        zcl_abapgit_default_transport=>get_instance( )->reset( ).
        refresh( iv_drop_log = abap_false ).
        RAISE EXCEPTION lx_error.
    ENDTRY.

    INSERT LINES OF lt_updated_files INTO TABLE ct_files.

  ENDMETHOD.


  METHOD find_remote_dot_abapgit.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF mt_remote.

    get_files_remote( ).

    READ TABLE mt_remote ASSIGNING <ls_remote>
      WITH KEY file_path
      COMPONENTS path     = zif_abapgit_definitions=>c_root_dir
                 filename = zif_abapgit_definitions=>c_dot_abapgit.
    IF sy-subrc = 0.
      ro_dot = zcl_abapgit_dot_abapgit=>deserialize( <ls_remote>-data ).
      set_dot_abapgit( ro_dot ).
      COMMIT WORK AND WAIT. " to release lock
    ENDIF.

  ENDMETHOD.


  METHOD find_remote_dot_apack.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF mt_remote.

    get_files_remote( ).

    READ TABLE mt_remote ASSIGNING <ls_remote>
      WITH KEY file_path
      COMPONENTS path     = zif_abapgit_definitions=>c_root_dir
                 filename = zif_abapgit_apack_definitions=>c_dot_apack_manifest.
    IF sy-subrc = 0.
      ro_dot = zcl_abapgit_apack_reader=>deserialize( iv_package_name = ms_data-package
                                                      iv_xstr         = <ls_remote>-data ).
      set_dot_apack( ro_dot ).
    ENDIF.

  ENDMETHOD.


  METHOD get_data_config.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF mt_remote.

    IF mi_data_config IS BOUND.
      ri_config = mi_data_config.
      RETURN.
    ENDIF.

    CREATE OBJECT ri_config TYPE zcl_abapgit_data_config.
    mi_data_config = ri_config.

    " Assume remote data has been loaded already
    READ TABLE mt_remote ASSIGNING <ls_remote>
      WITH KEY file_path
      COMPONENTS path = zif_abapgit_data_config=>c_default_path.
    IF sy-subrc = 0.
      ri_config->from_json( mt_remote ).
    ENDIF.

  ENDMETHOD.


  METHOD get_dot_apack.
    IF mo_apack_reader IS NOT BOUND.
      mo_apack_reader = zcl_abapgit_apack_reader=>create_instance( ms_data-package ).
    ENDIF.

    ro_dot_apack = mo_apack_reader.

  ENDMETHOD.


  METHOD get_log.
    ri_log = mi_log.
  ENDMETHOD.


  METHOD get_unsupported_objects_local.

    DATA: lt_tadir           TYPE zif_abapgit_definitions=>ty_tadir_tt,
          lt_supported_types TYPE zcl_abapgit_objects=>ty_types_tt.

    FIELD-SYMBOLS: <ls_tadir>  LIKE LINE OF lt_tadir,
                   <ls_object> LIKE LINE OF rt_objects.

    lt_tadir = zcl_abapgit_factory=>get_tadir( )->read(
                      iv_package            = ms_data-package
                      iv_ignore_subpackages = ms_data-local_settings-ignore_subpackages
                      iv_only_local_objects = ms_data-local_settings-only_local_objects
                      io_dot                = get_dot_abapgit( ) ).

    lt_supported_types = zcl_abapgit_objects=>supported_list( ).
    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      READ TABLE lt_supported_types WITH KEY table_line = <ls_tadir>-object TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO rt_objects ASSIGNING <ls_object>.
        MOVE-CORRESPONDING <ls_tadir> TO <ls_object>.
        <ls_object>-obj_type = <ls_tadir>-object.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD normalize_local_settings.

    cs_local_settings-labels = zcl_abapgit_repo_labels=>normalize( cs_local_settings-labels ).

    " TODO: more validation and normalization ?

  ENDMETHOD.


  METHOD notify_listener.

    DATA ls_meta_slug TYPE zif_abapgit_persistence=>ty_repo_xml.

    IF mi_listener IS BOUND.
      MOVE-CORRESPONDING ms_data TO ls_meta_slug.
      mi_listener->on_meta_change(
        iv_key         = ms_data-key
        is_meta        = ls_meta_slug
        is_change_mask = is_change_mask ).
    ENDIF.

  ENDMETHOD.


  METHOD refresh_local_object.

    DATA:
      ls_tadir           TYPE zif_abapgit_definitions=>ty_tadir,
      lt_tadir           TYPE zif_abapgit_definitions=>ty_tadir_tt,
      lt_new_local_files TYPE zif_abapgit_definitions=>ty_files_item_tt,
      lo_serialize       TYPE REF TO zcl_abapgit_serialize.

    lt_tadir = zcl_abapgit_factory=>get_tadir( )->read(
                   iv_package = ms_data-package
                   io_dot     = get_dot_abapgit( ) ).

    DELETE mt_local WHERE item-obj_type = iv_obj_type
                    AND   item-obj_name = iv_obj_name.

    READ TABLE lt_tadir INTO ls_tadir
                        WITH KEY object   = iv_obj_type
                                 obj_name = iv_obj_name.
    IF sy-subrc <> 0 OR ls_tadir-delflag = abap_true.
      " object doesn't exist anymore, nothing todo here
      RETURN.
    ENDIF.

    CLEAR lt_tadir.
    INSERT ls_tadir INTO TABLE lt_tadir.

    CREATE OBJECT lo_serialize.
    lt_new_local_files = lo_serialize->serialize(
      iv_package = ms_data-package
      it_tadir   = lt_tadir ).

    INSERT LINES OF lt_new_local_files INTO TABLE mt_local.

  ENDMETHOD.


  METHOD refresh_local_objects.

    mv_request_local_refresh = abap_true.
    get_files_local( ).

  ENDMETHOD.


  METHOD remove_ignored_files.

    DATA lo_dot TYPE REF TO zcl_abapgit_dot_abapgit.
    DATA lv_index TYPE sy-index.

    FIELD-SYMBOLS <ls_files> LIKE LINE OF ct_files.

    lo_dot = get_dot_abapgit( ).

    " Skip ignored files
    LOOP AT ct_files ASSIGNING <ls_files>.
      lv_index = sy-tabix.
      IF lo_dot->is_ignored( iv_path     = <ls_files>-path
                             iv_filename = <ls_files>-filename ) = abap_true.
        DELETE ct_files INDEX lv_index.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD reset_remote.
    CLEAR mt_remote.
    mv_request_remote_refresh = abap_true.
    reset_status( ).
  ENDMETHOD.


  METHOD reset_status.
    CLEAR mt_status.
  ENDMETHOD.


  METHOD set.

* TODO: refactor, maybe use zcl_abapgit_string_map ?

    DATA: ls_mask TYPE zif_abapgit_persistence=>ty_repo_meta_mask.


    ASSERT iv_url IS SUPPLIED
      OR iv_branch_name IS SUPPLIED
      OR iv_selected_commit IS SUPPLIED
      OR iv_head_branch IS SUPPLIED
      OR iv_offline IS SUPPLIED
      OR is_dot_abapgit IS SUPPLIED
      OR is_local_settings IS SUPPLIED
      OR iv_deserialized_by IS SUPPLIED
      OR iv_deserialized_at IS SUPPLIED
      OR iv_switched_origin IS SUPPLIED.


    IF iv_url IS SUPPLIED.
      ms_data-url = iv_url.
      ls_mask-url = abap_true.
    ENDIF.

    IF iv_branch_name IS SUPPLIED.
      ms_data-branch_name = iv_branch_name.
      ls_mask-branch_name = abap_true.
    ENDIF.

    IF iv_selected_commit IS SUPPLIED.
      ms_data-selected_commit = iv_selected_commit.
      ls_mask-selected_commit = abap_true.
    ENDIF.

    IF iv_head_branch IS SUPPLIED.
      ms_data-head_branch = iv_head_branch.
      ls_mask-head_branch = abap_true.
    ENDIF.

    IF iv_offline IS SUPPLIED.
      ms_data-offline = iv_offline.
      ls_mask-offline = abap_true.
    ENDIF.

    IF is_dot_abapgit IS SUPPLIED.
      ms_data-dot_abapgit = is_dot_abapgit.
      ls_mask-dot_abapgit = abap_true.
    ENDIF.

    IF is_local_settings IS SUPPLIED.
      ms_data-local_settings = is_local_settings.
      ls_mask-local_settings = abap_true.
      normalize_local_settings( CHANGING cs_local_settings = ms_data-local_settings ).
    ENDIF.

    IF iv_deserialized_at IS SUPPLIED OR iv_deserialized_by IS SUPPLIED.
      ms_data-deserialized_at = iv_deserialized_at.
      ms_data-deserialized_by = iv_deserialized_by.
      ls_mask-deserialized_at = abap_true.
      ls_mask-deserialized_by = abap_true.
    ENDIF.

    IF iv_switched_origin IS SUPPLIED.
      ms_data-switched_origin = iv_switched_origin.
      ls_mask-switched_origin = abap_true.
    ENDIF.

    notify_listener( ls_mask ).

  ENDMETHOD.


  METHOD set_dot_apack.
    get_dot_apack( ).
    mo_apack_reader->set_manifest_descriptor( io_dot_apack->get_manifest_descriptor( ) ).
  ENDMETHOD.


  METHOD set_files_remote.

    mt_remote = it_files.
    mv_request_remote_refresh = abap_false.

  ENDMETHOD.


  METHOD set_local_settings.

    set( is_local_settings = is_settings ).

  ENDMETHOD.


  METHOD status.

    IF lines( mt_status ) = 0.
      mt_status = zcl_abapgit_file_status=>status( io_repo = me
                                                   ii_log  = ii_log ).

    ENDIF.

    rt_results = mt_status.

  ENDMETHOD.


  METHOD switch_repo_type.

    IF iv_offline = ms_data-offline.
      zcx_abapgit_exception=>raise( |Cannot switch_repo_type, offline already = "{ ms_data-offline }"| ).
    ENDIF.

    IF iv_offline = abap_true. " On-line -> OFFline
      set( iv_url             = zcl_abapgit_url=>name( ms_data-url )
           iv_branch_name     = ''
           iv_selected_commit = ''
           iv_head_branch     = ''
           iv_offline         = abap_true ).
    ELSE. " OFFline -> On-line
      set( iv_offline = abap_false ).
    ENDIF.

  ENDMETHOD.


  METHOD update_last_deserialize.

    DATA: lv_deserialized_at TYPE zif_abapgit_persistence=>ty_repo-deserialized_at,
          lv_deserialized_by TYPE zif_abapgit_persistence=>ty_repo-deserialized_by.

    GET TIME STAMP FIELD lv_deserialized_at.
    lv_deserialized_by = sy-uname.

    set( iv_deserialized_at = lv_deserialized_at
         iv_deserialized_by = lv_deserialized_by ).

  ENDMETHOD.


  METHOD zif_abapgit_repo~checksums.

    CREATE OBJECT ri_checksums TYPE zcl_abapgit_repo_checksums
      EXPORTING
        iv_repo_key = ms_data-key.

  ENDMETHOD.


  METHOD zif_abapgit_repo~deserialize.

    DATA lt_updated_files TYPE zif_abapgit_git_definitions=>ty_file_signatures_tt.

    find_remote_dot_abapgit( ).
    find_remote_dot_apack( ).

    check_write_protect( ).
    check_language( ).

    IF is_checks-requirements-met = zif_abapgit_definitions=>c_no AND is_checks-requirements-decision IS INITIAL.
      zcx_abapgit_exception=>raise( 'Requirements not met and undecided' ).
    ENDIF.

    IF is_checks-dependencies-met = zif_abapgit_definitions=>c_no.
      zcx_abapgit_exception=>raise( 'APACK dependencies not met' ).
    ENDIF.

    IF is_checks-transport-required = abap_true AND is_checks-transport-transport IS INITIAL.
      zcx_abapgit_exception=>raise( |No transport request was supplied| ).
    ENDIF.

    deserialize_dot_abapgit( CHANGING ct_files = lt_updated_files ).

    deserialize_objects(
      EXPORTING
        is_checks = is_checks
        ii_log    = ii_log
      CHANGING
        ct_files  = lt_updated_files ).

    deserialize_data(
      EXPORTING
        is_checks = is_checks
      CHANGING
        ct_files  = lt_updated_files ).

    CLEAR mt_local. " Should be before CS update which uses NEW local

    zif_abapgit_repo~checksums( )->update( lt_updated_files ).

    update_last_deserialize( ).
    reset_status( ).

    COMMIT WORK AND WAIT.

    check_for_restart( ).

  ENDMETHOD.


  METHOD zif_abapgit_repo~deserialize_checks.

    DATA: lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
          lt_dependencies TYPE zif_abapgit_apack_definitions=>ty_dependencies.

    find_remote_dot_abapgit( ).
    find_remote_dot_apack( ).

    check_write_protect( ).
    check_language( ).

    rs_checks = zcl_abapgit_objects=>deserialize_checks( me ).

    lt_requirements = get_dot_abapgit( )->get_data( )-requirements.
    rs_checks-requirements-met = zcl_abapgit_requirement_helper=>is_requirements_met( lt_requirements ).

    lt_dependencies = get_dot_apack( )->get_manifest_descriptor( )-dependencies.
    rs_checks-dependencies-met = zcl_abapgit_apack_helper=>are_dependencies_met( lt_dependencies ).

    rs_checks-customizing = zcl_abapgit_data_factory=>get_deserializer( )->deserialize_check(
      io_repo   = me
      ii_config = get_data_config( ) ).

  ENDMETHOD.


  METHOD zif_abapgit_repo~get_dot_abapgit.
    CREATE OBJECT ro_dot_abapgit
      EXPORTING
        is_data = ms_data-dot_abapgit.
  ENDMETHOD.


  METHOD zif_abapgit_repo~get_files_local.

    DATA lo_serialize TYPE REF TO zcl_abapgit_serialize.
    DATA lt_filter TYPE zif_abapgit_definitions=>ty_tadir_tt.
    " Serialization happened before and no refresh request
    IF lines( mt_local ) > 0 AND mv_request_local_refresh = abap_false.
      rt_files = mt_local.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_serialize
      EXPORTING
        io_dot_abapgit    = get_dot_abapgit( )
        is_local_settings = get_local_settings( ).

    IF ii_obj_filter IS NOT INITIAL.
      lt_filter = ii_obj_filter->get_filter( ).
    ENDIF.

    rt_files = lo_serialize->files_local(
      iv_package     = get_package( )
      ii_data_config = get_data_config( )
      ii_log         = ii_log
      it_filter      = lt_filter ).

    mt_local                 = rt_files.
    mv_request_local_refresh = abap_false. " Fulfill refresh

  ENDMETHOD.


  METHOD zif_abapgit_repo~get_files_remote.
    DATA lt_filter TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA lr_filter TYPE REF TO zcl_abapgit_repo_filter.

    rt_files = mt_remote.
    IF ii_obj_filter IS NOT INITIAL.
      lt_filter = ii_obj_filter->get_filter( ).

      CREATE OBJECT lr_filter.
      lr_filter->apply_object_filter(
        EXPORTING
          it_filter   = lt_filter
          io_dot      = get_dot_abapgit( )
          iv_devclass = get_package( )
        CHANGING
          ct_files    = rt_files ).

    ENDIF.

    IF iv_ignore_files = abap_true.
      remove_ignored_files( CHANGING ct_files = rt_files ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_repo~get_key.
    rv_key = ms_data-key.
  ENDMETHOD.


  METHOD zif_abapgit_repo~get_local_settings.

    rs_settings = ms_data-local_settings.

  ENDMETHOD.


  METHOD zif_abapgit_repo~get_name.

    rv_name = ms_data-local_settings-display_name.

  ENDMETHOD.


  METHOD zif_abapgit_repo~get_package.
    rv_package = ms_data-package.
  ENDMETHOD.


  METHOD zif_abapgit_repo~is_offline.
    rv_offline = ms_data-offline.
  ENDMETHOD.


  METHOD zif_abapgit_repo~refresh.

    mv_request_local_refresh = abap_true.
    reset_remote( ).

    IF iv_drop_log = abap_true.
      CLEAR mi_log.
    ENDIF.

    IF iv_drop_cache = abap_true.
      CLEAR mt_local.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_repo~set_dot_abapgit.
    set( is_dot_abapgit = io_dot_abapgit->get_data( ) ).
  ENDMETHOD.
ENDCLASS.
