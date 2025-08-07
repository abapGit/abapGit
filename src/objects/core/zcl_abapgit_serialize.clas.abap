CLASS zcl_abapgit_serialize DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !io_dot_abapgit    TYPE REF TO zcl_abapgit_dot_abapgit OPTIONAL
        !is_local_settings TYPE zif_abapgit_persistence=>ty_repo-local_settings OPTIONAL
      RAISING
        zcx_abapgit_exception .
    METHODS on_end_of_task
      IMPORTING
        !p_task TYPE clike ##NEEDED.
    METHODS serialize
      IMPORTING
        !iv_package          TYPE devclass OPTIONAL
        !it_tadir            TYPE zif_abapgit_definitions=>ty_tadir_tt
        !ii_log              TYPE REF TO zif_abapgit_log OPTIONAL
        !iv_force_sequential TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_files)      TYPE zif_abapgit_definitions=>ty_files_item_tt
      RAISING
        zcx_abapgit_exception .
    METHODS files_local
      IMPORTING
        !iv_package     TYPE devclass
        !ii_log         TYPE REF TO zif_abapgit_log
        !it_filter      TYPE zif_abapgit_definitions=>ty_tadir_tt OPTIONAL
        !ii_data_config TYPE REF TO zif_abapgit_data_config OPTIONAL
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_files_item_tt
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_unsupported_count,
        obj_type TYPE tadir-object,
        obj_name TYPE tadir-obj_name,
        count    TYPE i,
      END OF ty_unsupported_count .
    TYPES:
      ty_unsupported_count_tt TYPE HASHED TABLE OF ty_unsupported_count WITH UNIQUE KEY obj_type .
    TYPES:
      ty_char32 TYPE c LENGTH 32 .

    CLASS-DATA gv_max_processes TYPE i .
    DATA mt_files TYPE zif_abapgit_definitions=>ty_files_item_tt .
    DATA mv_free TYPE i .
    DATA mi_log TYPE REF TO zif_abapgit_log .
    DATA mv_group TYPE rzlli_apcl .
    DATA mo_dot_abapgit TYPE REF TO zcl_abapgit_dot_abapgit.
    DATA ms_local_settings TYPE zif_abapgit_persistence=>ty_repo-local_settings.
    DATA ms_i18n_params TYPE zif_abapgit_definitions=>ty_i18n_params.
    DATA mo_abap_language_version TYPE REF TO zcl_abapgit_abap_language_vers.
    DATA mt_wo_translation_patterns TYPE string_table.

    METHODS add_apack
      IMPORTING
        !iv_package TYPE devclass
      CHANGING
        !ct_files   TYPE zif_abapgit_definitions=>ty_files_item_tt
      RAISING
        zcx_abapgit_exception .
    METHODS add_data
      IMPORTING
        !ii_data_config TYPE REF TO zif_abapgit_data_config
      CHANGING
        !ct_files       TYPE zif_abapgit_definitions=>ty_files_item_tt
      RAISING
        zcx_abapgit_exception .
    METHODS add_dot_abapgit
      CHANGING
        !ct_files TYPE zif_abapgit_definitions=>ty_files_item_tt
      RAISING
        zcx_abapgit_exception .
    METHODS add_to_return
      IMPORTING
        !iv_path      TYPE string
        !is_file_item TYPE zif_abapgit_objects=>ty_serialization .
    METHODS run_parallel
      IMPORTING
        !is_tadir TYPE zif_abapgit_definitions=>ty_tadir
        !iv_task  TYPE ty_char32
      RAISING
        zcx_abapgit_exception .
    METHODS run_sequential
      IMPORTING
        !is_tadir TYPE zif_abapgit_definitions=>ty_tadir
      RAISING
        zcx_abapgit_exception .
    METHODS add_objects
      IMPORTING
        !iv_package     TYPE devclass
        !ii_log         TYPE REF TO zif_abapgit_log
        !it_filter      TYPE zif_abapgit_definitions=>ty_tadir_tt OPTIONAL
      CHANGING
        VALUE(ct_files) TYPE zif_abapgit_definitions=>ty_files_item_tt
      RAISING
        zcx_abapgit_exception .
    METHODS determine_rfc_server_group
      RETURNING
        VALUE(rv_group) TYPE rzlli_apcl
      RAISING
        zcx_abapgit_exception.
    METHODS determine_max_processes
      IMPORTING
        !iv_force_sequential TYPE abap_bool DEFAULT abap_false
        iv_package           TYPE devclass
      RETURNING
        VALUE(rv_processes)  TYPE i
      RAISING
        zcx_abapgit_exception .
    METHODS filter_unsupported_objects
      CHANGING
        !ct_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt .
    METHODS filter_ignored_objects
      IMPORTING
        !iv_package TYPE devclass
      CHANGING
        !ct_tadir   TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.

    METHODS is_parallelization_possible
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS is_no_parallel
      IMPORTING
        !iv_object_type  TYPE tadir-object
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

ENDCLASS.



CLASS ZCL_ABAPGIT_SERIALIZE IMPLEMENTATION.


  METHOD add_apack.

    DATA ls_apack_file TYPE zif_abapgit_git_definitions=>ty_file.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF ct_files.


    ls_apack_file = zcl_abapgit_apack_helper=>to_file( iv_package ).
    IF ls_apack_file IS NOT INITIAL.
      APPEND INITIAL LINE TO ct_files ASSIGNING <ls_file>.
      <ls_file>-file = ls_apack_file.
    ENDIF.

  ENDMETHOD.


  METHOD add_data.

    DATA lt_files TYPE zif_abapgit_git_definitions=>ty_files_tt.
    DATA ls_file LIKE LINE OF lt_files.

    FIELD-SYMBOLS <ls_return> LIKE LINE OF ct_files.

    IF ii_data_config IS INITIAL.
      RETURN.
    ENDIF.

    lt_files = ii_data_config->to_json( ).
    LOOP AT lt_files INTO ls_file.
      APPEND INITIAL LINE TO ct_files ASSIGNING <ls_return>.
      <ls_return>-file = ls_file.

      " Derive object from config filename (namespace + escaping)
      zcl_abapgit_filename_logic=>file_to_object(
        EXPORTING
          iv_filename = <ls_return>-file-filename
          iv_path     = <ls_return>-file-path
          io_dot      = mo_dot_abapgit
        IMPORTING
          es_item     = <ls_return>-item ).

      <ls_return>-item-obj_type = zif_abapgit_data_config=>c_data_type-tabu. " todo
    ENDLOOP.

    lt_files = zcl_abapgit_data_factory=>get_serializer( )->serialize( ii_data_config ).
    LOOP AT lt_files INTO ls_file.
      APPEND INITIAL LINE TO ct_files ASSIGNING <ls_return>.
      <ls_return>-file = ls_file.

      " Derive object from data filename (namespace + escaping)
      zcl_abapgit_filename_logic=>file_to_object(
        EXPORTING
          iv_filename = <ls_return>-file-filename
          iv_path     = <ls_return>-file-path
          io_dot      = mo_dot_abapgit
        IMPORTING
          es_item     = <ls_return>-item ).
    ENDLOOP.

  ENDMETHOD.


  METHOD add_dot_abapgit.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF ct_files.

    APPEND INITIAL LINE TO ct_files ASSIGNING <ls_file>.
    <ls_file>-file = mo_dot_abapgit->to_file( ).

  ENDMETHOD.


  METHOD add_objects.

    DATA: lo_filter TYPE REF TO zcl_abapgit_repo_filter,
          lv_force  TYPE abap_bool,
          lt_found  LIKE ct_files,
          lt_tadir  TYPE zif_abapgit_definitions=>ty_tadir_tt.

    lt_tadir = zcl_abapgit_factory=>get_tadir( )->read(
      iv_package            = iv_package
      iv_ignore_subpackages = ms_local_settings-ignore_subpackages
      iv_only_local_objects = ms_local_settings-only_local_objects
      io_dot                = mo_dot_abapgit
      ii_log                = ii_log
      it_filter             = it_filter ).

    CREATE OBJECT lo_filter.

    lo_filter->apply( EXPORTING it_filter = it_filter
                      CHANGING  ct_tadir  = lt_tadir ).

* if there are less than 10 objects run in single thread
* this helps a lot when debugging, plus performance gain
* with low number of objects does not matter much
    lv_force = boolc( lines( lt_tadir ) < 10 ).

    lt_found = serialize(
      iv_package          = iv_package
      it_tadir            = lt_tadir
      ii_log              = ii_log
      iv_force_sequential = lv_force ).
    APPEND LINES OF lt_found TO ct_files.

  ENDMETHOD.


  METHOD add_to_return.

    FIELD-SYMBOLS: <ls_file>   LIKE LINE OF is_file_item-files,
                   <ls_return> LIKE LINE OF mt_files.


    LOOP AT is_file_item-files ASSIGNING <ls_file>.
      APPEND INITIAL LINE TO mt_files ASSIGNING <ls_return>.
      <ls_return>-file = <ls_file>.
      <ls_return>-file-path = iv_path.
      <ls_return>-item = is_file_item-item.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    mv_group = determine_rfc_server_group( ).

    mo_dot_abapgit = io_dot_abapgit.
    ms_local_settings = is_local_settings.

    IF io_dot_abapgit IS BOUND.
      ms_i18n_params = io_dot_abapgit->determine_i18n_parameters( is_local_settings-main_language_only ).
      mt_wo_translation_patterns =
        zcl_abapgit_i18n_params=>normalize_obj_patterns( io_dot_abapgit->get_objs_without_translation( ) ).
    ELSE.
      ms_i18n_params-main_language      = sy-langu.
      ms_i18n_params-main_language_only = is_local_settings-main_language_only.
    ENDIF.
    ms_i18n_params-suppress_po_comments = is_local_settings-suppress_lxe_po_comments.

    IF mo_dot_abapgit IS NOT INITIAL.
      CREATE OBJECT mo_abap_language_version
        EXPORTING
          io_dot_abapgit = mo_dot_abapgit.
    ENDIF.

  ENDMETHOD.


  METHOD determine_max_processes.
    DATA: li_exit TYPE REF TO zif_abapgit_exit.
    DATA lv_available_sessions TYPE i.

    IF iv_force_sequential = abap_true.
      rv_processes = 1.
      RETURN.
    ENDIF.

    IF gv_max_processes IS INITIAL AND is_parallelization_possible( ) = abap_true.

      gv_max_processes = zcl_abapgit_factory=>get_environment( )->init_parallel_processing( mv_group ).

      IF gv_max_processes > 1.
        gv_max_processes = gv_max_processes - 1.
      ENDIF.

      IF gv_max_processes > 32.
        " https://en.wikipedia.org/wiki/Amdahl%27s_law
        gv_max_processes = 32.
      ENDIF.

    ENDIF.

    IF gv_max_processes IS INITIAL.
      " fallback to running sequentially.
      gv_max_processes = 1.
    ENDIF.

    rv_processes = gv_max_processes.

    ASSERT rv_processes >= 1.

    " Avoid going over the maximum available user sessions
    IF sy-batch IS INITIAL.
      lv_available_sessions = zcl_abapgit_factory=>get_environment( )->get_available_user_sessions( ).

      IF lv_available_sessions = 0.
        " No available session -> disable parallel processing
        rv_processes = 1.
      ELSEIF rv_processes > lv_available_sessions.
        rv_processes = lv_available_sessions.
      ENDIF.
    ENDIF.

    ASSERT rv_processes >= 1.

    " Exit setting has highest priority to change maximum sessions
    li_exit = zcl_abapgit_exit=>get_instance( ).
    li_exit->change_max_parallel_processes(
      EXPORTING
        iv_package       = iv_package
      CHANGING
        cv_max_processes = rv_processes ).

    ASSERT rv_processes >= 1.

  ENDMETHOD.


  METHOD determine_rfc_server_group.

    DATA:
      li_exit   TYPE REF TO zif_abapgit_exit,
      lv_exists TYPE abap_bool.

    " According to SAP Note 3215918 it's recommended NOT to use this group anymore.
    " However, we keep it for compatibility. If it does not exist, we switch to the
    " recommended DEFAULT behaviour.
    rv_group = 'parallel_generators'.

    li_exit = zcl_abapgit_exit=>get_instance( ).
    li_exit->change_rfc_server_group( CHANGING cv_group = rv_group ).

    " Check if RFC server group exists and fallback to the default
    lv_exists = zcl_abapgit_factory=>get_environment( )->check_parallel_processing( rv_group ).
    IF lv_exists = abap_false.
      rv_group = ''.
    ENDIF.

  ENDMETHOD.


  METHOD files_local.

* serializes objects, including .abapgit.xml, apack, and takes into account local settings

    add_dot_abapgit( CHANGING ct_files = rt_files ).

    add_apack(
      EXPORTING
        iv_package = iv_package
      CHANGING
        ct_files   = rt_files ).

    add_data(
      EXPORTING
        ii_data_config = ii_data_config
      CHANGING
        ct_files       = rt_files ).

    add_objects(
      EXPORTING
        iv_package = iv_package
        ii_log     = ii_log
        it_filter  = it_filter
      CHANGING
        ct_files   = rt_files ).

  ENDMETHOD.


  METHOD filter_ignored_objects.

    DATA:
      ls_ignored_count TYPE ty_unsupported_count,
      lt_ignored_count TYPE ty_unsupported_count_tt,
      lo_folder_logic  TYPE REF TO zcl_abapgit_folder_logic,
      ls_item          TYPE zif_abapgit_definitions=>ty_item,
      lv_path          TYPE string,
      lv_filename      TYPE string.

    FIELD-SYMBOLS:
      <ls_tadir>         LIKE LINE OF ct_tadir,
      <ls_ignored_count> TYPE ty_unsupported_count.

    " Ignore logic requires .abapGit.xml
    IF mo_dot_abapgit IS INITIAL OR iv_package IS INITIAL OR mi_log IS INITIAL.
      RETURN.
    ENDIF.

    lo_folder_logic = zcl_abapgit_folder_logic=>get_instance( ).

    LOOP AT ct_tadir ASSIGNING <ls_tadir>.
      CLEAR: ls_ignored_count.

      ls_item-obj_type = <ls_tadir>-object.
      ls_item-obj_name = <ls_tadir>-obj_name.

      IF <ls_tadir>-devclass IS NOT INITIAL.
        lv_path = lo_folder_logic->package_to_path(
          iv_top     = iv_package
          io_dot     = mo_dot_abapgit
          iv_package = <ls_tadir>-devclass ).
      ELSE.
        lv_path = mo_dot_abapgit->get_starting_folder( ).
      ENDIF.

      lv_filename = zcl_abapgit_filename_logic=>object_to_file(
        is_item  = ls_item
        iv_ext   = '*' ).

      IF mo_dot_abapgit->is_ignored(
        iv_path     = lv_path
        iv_filename = lv_filename ) = abap_false.
        CONTINUE.
      ENDIF.

      READ TABLE lt_ignored_count ASSIGNING <ls_ignored_count> WITH TABLE KEY obj_type = <ls_tadir>-object.
      IF sy-subrc <> 0.
        ls_ignored_count-obj_type = <ls_tadir>-object.
        ls_ignored_count-count    = 1.
        ls_ignored_count-obj_name = <ls_tadir>-obj_name.
        INSERT ls_ignored_count INTO TABLE lt_ignored_count ASSIGNING <ls_ignored_count>.
      ELSE.
        CLEAR: <ls_ignored_count>-obj_name.
        <ls_ignored_count>-count = <ls_ignored_count>-count + 1.
      ENDIF.
      " init object so we can remove these entries afterward
      CLEAR <ls_tadir>-object.
    ENDLOOP.
    IF lt_ignored_count IS INITIAL.
      RETURN.
    ENDIF.

    " remove ignored objects
    DELETE ct_tadir WHERE object IS INITIAL.

    LOOP AT lt_ignored_count ASSIGNING <ls_ignored_count>.
      IF <ls_ignored_count>-count = 1.
        mi_log->add_warning( |Object { <ls_ignored_count>-obj_type } { <ls_ignored_count>-obj_name } ignored| ).
      ELSE.
        mi_log->add_warning( |Object type { <ls_ignored_count>-obj_type } with | &&
                             |{ <ls_ignored_count>-count } objects ignored| ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD filter_unsupported_objects.

    DATA: ls_unsupported_count TYPE ty_unsupported_count,
          lt_supported_types   TYPE zif_abapgit_objects=>ty_types_tt,
          lt_unsupported_count TYPE ty_unsupported_count_tt.

    FIELD-SYMBOLS: <ls_tadir>             LIKE LINE OF ct_tadir,
                   <ls_unsupported_count> TYPE ty_unsupported_count.

    lt_supported_types = zcl_abapgit_objects=>supported_list( ).
    LOOP AT ct_tadir ASSIGNING <ls_tadir>.
      CLEAR: ls_unsupported_count.
      READ TABLE lt_supported_types WITH KEY table_line = <ls_tadir>-object TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      READ TABLE lt_unsupported_count ASSIGNING <ls_unsupported_count>
                                      WITH TABLE KEY obj_type = <ls_tadir>-object.
      IF sy-subrc <> 0.
        ls_unsupported_count-obj_type = <ls_tadir>-object.
        ls_unsupported_count-count    = 1.
        ls_unsupported_count-obj_name = <ls_tadir>-obj_name.
        INSERT ls_unsupported_count INTO TABLE lt_unsupported_count ASSIGNING <ls_unsupported_count>.
      ELSE.
        CLEAR: <ls_unsupported_count>-obj_name.
        <ls_unsupported_count>-count = <ls_unsupported_count>-count + 1.
      ENDIF.
      CLEAR: <ls_tadir>-object.
    ENDLOOP.
    IF lt_unsupported_count IS INITIAL.
      RETURN.
    ENDIF.

    DELETE ct_tadir WHERE object IS INITIAL.
    IF mi_log IS BOUND.
      LOOP AT lt_unsupported_count ASSIGNING <ls_unsupported_count>.
        IF <ls_unsupported_count>-count = 1.
          mi_log->add_error( |Object type { <ls_unsupported_count>-obj_type } not supported, | &&
                             |{ <ls_unsupported_count>-obj_name } ignored| ).
        ELSE.
          mi_log->add_error( |Object type { <ls_unsupported_count>-obj_type } not supported, | &&
                             |{ <ls_unsupported_count>-count } objects ignored| ).
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD is_no_parallel.

    " Currently know object types that do not support parallel processing:
    " ECTC/ECTD - ECATT Test Config/Data (see #7148)
    " Should this list become longer, it should become a flag of the object type serializer
    IF iv_object_type = 'ECTC' OR iv_object_type = 'ECTD'.
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD is_parallelization_possible.

    rv_result = boolc( zcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_false
                   AND zcl_abapgit_persist_factory=>get_settings( )->read( )->get_parallel_proc_disabled( ) = abap_false
                   " The function module below should always exist here as is_merged evaluated to false above.
                   " It does however not exist in the transpiled version which then causes unit tests to fail.
                   " Therefore the check needs to stay.
                   AND zcl_abapgit_factory=>get_function_module(
                                         )->function_exists( 'Z_ABAPGIT_SERIALIZE_PARALLEL' ) = abap_true ).

  ENDMETHOD.


  METHOD on_end_of_task.

* this method will be called from the parallel processing, thus it must be public

    DATA: lv_result    TYPE xstring,
          lv_path      TYPE string,
          lv_mess      TYPE c LENGTH 200,
          ls_file_item TYPE zif_abapgit_objects=>ty_serialization.


    RECEIVE RESULTS FROM FUNCTION 'Z_ABAPGIT_SERIALIZE_PARALLEL'
      IMPORTING
        ev_result             = lv_result
        ev_path               = lv_path
      EXCEPTIONS
        error                 = 1
        system_failure        = 2 MESSAGE lv_mess
        communication_failure = 3 MESSAGE lv_mess
        OTHERS = 4.
    IF sy-subrc <> 0.
      IF NOT mi_log IS INITIAL.
        IF NOT lv_mess IS INITIAL.
          mi_log->add_error( lv_mess ).
        ELSE.
          mi_log->add_error( |{ sy-msgv1 }{ sy-msgv2 }{ sy-msgv3 }{ sy-msgv3 }| ).
        ENDIF.
      ENDIF.
    ELSE.
      IMPORT data = ls_file_item FROM DATA BUFFER lv_result. "#EC CI_SUBRC
      ASSERT sy-subrc = 0.
      add_to_return( is_file_item = ls_file_item
                     iv_path      = lv_path ).
    ENDIF.

    mv_free = mv_free + 1.

  ENDMETHOD.


  METHOD run_parallel.

    DATA: lv_msg  TYPE c LENGTH 100,
          lv_task TYPE c LENGTH 32,
          lv_free LIKE mv_free.
    DATA lv_abap_language_version TYPE zif_abapgit_aff_types_v1=>ty_abap_language_version.
    DATA lv_main_language_only TYPE abap_bool.

    ASSERT mv_free > 0.

    lv_abap_language_version = mo_abap_language_version->get_repo_abap_language_version( ).

    lv_main_language_only = ms_i18n_params-main_language_only.
    IF lv_main_language_only = abap_false AND mt_wo_translation_patterns IS NOT INITIAL.
      lv_main_language_only = zcl_abapgit_i18n_params=>match_obj_patterns(
        is_tadir                   = is_tadir
        it_wo_translation_patterns = mt_wo_translation_patterns ).
    ENDIF.

    DO.
      lv_task = |{ iv_task }-{ sy-index }|.
      " An initial server group is handled like DEFAULT meaning all instances are used
      CALL FUNCTION 'Z_ABAPGIT_SERIALIZE_PARALLEL'
        STARTING NEW TASK lv_task
        DESTINATION IN GROUP mv_group
        CALLING on_end_of_task ON END OF TASK
        EXPORTING
          iv_obj_type           = is_tadir-object
          iv_obj_name           = is_tadir-obj_name
          iv_devclass           = is_tadir-devclass
          iv_path               = is_tadir-path
          iv_srcsystem          = is_tadir-srcsystem
          iv_abap_language_vers = lv_abap_language_version
          iv_language           = ms_i18n_params-main_language
          iv_main_language_only = lv_main_language_only
          iv_suppress_po_comments = ms_i18n_params-suppress_po_comments
          it_translation_langs  = ms_i18n_params-translation_languages
          iv_use_lxe            = ms_i18n_params-use_lxe
        EXCEPTIONS
          system_failure        = 1 MESSAGE lv_msg
          communication_failure = 2 MESSAGE lv_msg
          resource_failure      = 3
          OTHERS                = 4.
      IF sy-subrc = 3.
        lv_free = mv_free.
        WAIT UNTIL mv_free <> lv_free UP TO 1 SECONDS.
        CONTINUE.
      ELSEIF sy-subrc <> 0.
        ASSERT lv_msg = '' AND 0 = 1.
      ENDIF.
      EXIT.
    ENDDO.

    mv_free = mv_free - 1.

  ENDMETHOD.


  METHOD run_sequential.

    DATA: lx_error     TYPE REF TO zcx_abapgit_exception,
          ls_i18n_params LIKE ms_i18n_params,
          ls_file_item TYPE zif_abapgit_objects=>ty_serialization.

    ls_file_item-item-obj_type  = is_tadir-object.
    ls_file_item-item-obj_name  = is_tadir-obj_name.
    ls_file_item-item-devclass  = is_tadir-devclass.
    ls_file_item-item-srcsystem = is_tadir-srcsystem.
    ls_file_item-item-abap_language_version = mo_abap_language_version->get_repo_abap_language_version( ).

    ls_i18n_params = ms_i18n_params.
    IF ls_i18n_params-main_language_only = abap_false AND mt_wo_translation_patterns IS NOT INITIAL.
      ls_i18n_params-main_language_only = zcl_abapgit_i18n_params=>match_obj_patterns(
        is_tadir                   = is_tadir
        it_wo_translation_patterns = mt_wo_translation_patterns ).
    ENDIF.

    TRY.
        ls_file_item = zcl_abapgit_objects=>serialize(
          is_item        = ls_file_item-item
          io_i18n_params = zcl_abapgit_i18n_params=>new( is_params = ls_i18n_params ) ).

        add_to_return( is_file_item = ls_file_item
                       iv_path      = is_tadir-path ).
      CATCH zcx_abapgit_exception INTO lx_error.
        IF NOT mi_log IS INITIAL.
          mi_log->add_exception(
              ix_exc  = lx_error
              is_item = ls_file_item-item ).
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD serialize.

* serializes only objects

    DATA: lv_max      TYPE i,
          lv_count    TYPE i,
          li_progress TYPE REF TO zif_abapgit_progress,
          li_exit     TYPE REF TO zif_abapgit_exit,
          lo_timer    TYPE REF TO zcl_abapgit_timer,
          lt_tadir    TYPE zif_abapgit_definitions=>ty_tadir_tt.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF it_tadir.


    CLEAR mt_files.

    lv_max = determine_max_processes( iv_force_sequential = iv_force_sequential
                                      iv_package          = iv_package ).
    mv_free = lv_max.
    mi_log = ii_log.

    lt_tadir = it_tadir.
    filter_unsupported_objects( CHANGING ct_tadir = lt_tadir ).

    filter_ignored_objects(
      EXPORTING
        iv_package = iv_package
      CHANGING
        ct_tadir   = lt_tadir ).

    lv_count = lines( lt_tadir ).

    li_progress = zcl_abapgit_progress=>get_instance( lv_count ).

    lo_timer = zcl_abapgit_timer=>create(
      iv_text  = 'Serialize:'
      iv_count = lv_count )->start( ).

    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      IF lv_max = 1 OR is_no_parallel( <ls_tadir>-object ) = abap_true.
        li_progress->show(
          iv_current = sy-tabix
          iv_text    = |Serialize { <ls_tadir>-obj_name }, { lv_max } thread| ).
        run_sequential( <ls_tadir> ).
      ELSE.
        li_progress->show(
          iv_current = sy-tabix
          iv_text    = |Serialize { <ls_tadir>-obj_name }, { lv_max } threads| ).
        run_parallel(
          is_tadir = <ls_tadir>
          iv_task  = |{ sy-tabix }| ).
        WAIT UNTIL mv_free > 0 UP TO 120 SECONDS.
      ENDIF.
    ENDLOOP.

    li_progress->off( ).

    WAIT UNTIL mv_free = lv_max UP TO 120 SECONDS.
    rt_files = mt_files.
    FREE mt_files.

*   Call postprocessing
    li_exit = zcl_abapgit_exit=>get_instance( ).

    li_exit->serialize_postprocess(
      EXPORTING
        iv_package = iv_package
        ii_log     = ii_log
      CHANGING
        ct_files   = rt_files ).

    lo_timer->end( abap_true ).

  ENDMETHOD.
ENDCLASS.
