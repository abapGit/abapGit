CLASS zcl_abapgit_remote_2_branch DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

************************************************************************
* abapGit - Remote (e.g. PRD) snapshot to branch
*
* WHY THIS FEATURE EXISTS
* ----------------------------------------------------------------------
* When the main branch has already moved many steps ahead of what is
* currently live in production, it is hard to "go back" and prepare a
* small, isolated hot-fix that matches the production state. Developers
* end up guessing which objects already diverged from PRD.
*
* This feature creates, in one click and from within DEV, a dedicated
* branch that:
*   1) captures the CURRENT serialized state of the repository, so it
*      can be used as a stable base to branch a targeted hot-fix from,
*   2) attaches a PRD-vs-DEV "divergence manifest" that lists, per
*      object, the latest version / transport recorded in the remote
*      (production) system next to the one in DEV, flagging the objects
*      that have already moved past production, and
*   3) optionally PULLS THE ACTUAL PRODUCTION SOURCE of source-code
*      objects (programs / interfaces) into the branch, so the branch
*      content itself is the live PRD baseline - not just a report.
*
* HOW IT TALKS TO PRODUCTION (no abapGit / no custom code in PRD)
* ----------------------------------------------------------------------
* abapGit is intentionally NOT installed in the PRD environment, so the
* feature only uses *standard* version-management function modules - the
* very same building blocks behind SE38 -> Utilities -> Versions when
* retrieving a remote version:
*   - SVRS_GET_VERSION_DIRECTORY_46 (remote-enabled) builds the
*     divergence manifest. It is called once with the RFC DESTINATION
*     pointing to PRD (remote directory) and once locally
*     (DESTINATION = space, DEV directory).
*   - SVRS_GET_VERSION_REMOTE (addressed by TMS target system, optional)
*     retrieves the production source for the content pull.
* No RFC_READ_TABLE and no Z code on the production side are required.
*
* SCOPE / LIMITATIONS (be honest)
* ----------------------------------------------------------------------
* SVRS_GET_VERSION_DIRECTORY_46 keys on version-management object types
* (VRSD-OBJTYPE), not on R3TR/TADIR types. A clean, reliable 1:1
* mapping exists for source programs and the common DDIC objects; those
* are compared in the manifest. Composite objects (classes, function
* groups, ...) are versioned as many internal parts and are therefore
* listed in the manifest as "not version-compared" - they are still
* fully contained in the branch snapshot, only the automatic divergence
* flag is skipped for them.
*
* The actual content pull (step 3) is deliberately limited to objects
* that map to a single, unambiguous abapGit source file - programs
* (*.prog.abap) and interfaces (*.intf.abap). The production source is
* read from the ABAPTEXT table of the deep SVRS2_VERSIONABLE_OBJECT
* structure using fully defensive, dynamic component access. Composite
* objects keep their DEV serialization. The version-management FMs used
* here are not officially released by SAP, so every call is wrapped
* defensively: any object that cannot be analyzed or retrieved is
* skipped gracefully, the branch snapshot is never broken, and an object
* is NEVER overwritten with empty or unverified content.
************************************************************************

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_result,
        branch_name  TYPE string,
        total_count  TYPE i,
        ahead_count  TYPE i,
        pulled_count TYPE i,
      END OF ty_result .

    METHODS create
      IMPORTING
        !ii_repo_online   TYPE REF TO zif_abapgit_repo_online
        !iv_destination   TYPE rfcdest
        !iv_target_system TYPE tmscsys-sysnam OPTIONAL
        !iv_branch_name   TYPE string
      RETURNING
        VALUE(rs_result)  TYPE ty_result
      RAISING
        zcx_abapgit_exception .

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_manifest_filename TYPE string VALUE 'PRD_DIVERGENCE.md' ##NO_TEXT.

    TYPES:
      BEGIN OF ty_manifest_line,
        obj_type       TYPE tadir-object,
        obj_name       TYPE tadir-obj_name,
        dev_versno     TYPE c LENGTH 5,
        dev_date       TYPE d,
        dev_trkorr     TYPE c LENGTH 20,
        prd_versno     TYPE c LENGTH 5,
        prd_date       TYPE d,
        prd_trkorr     TYPE c LENGTH 20,
        status         TYPE string,
        content_pulled TYPE string,
      END OF ty_manifest_line .
    TYPES:
      ty_manifest_tt TYPE STANDARD TABLE OF ty_manifest_line WITH DEFAULT KEY .

    METHODS stage_current_state
      IMPORTING
        !io_stage         TYPE REF TO zcl_abapgit_stage
        !is_stage_objects TYPE zif_abapgit_definitions=>ty_stage_files
      RAISING
        zcx_abapgit_exception .

    METHODS build_manifest
      IMPORTING
        !ii_repo_online    TYPE REF TO zif_abapgit_repo_online
        !iv_destination    TYPE rfcdest
      RETURNING
        VALUE(rt_manifest) TYPE ty_manifest_tt
      RAISING
        zcx_abapgit_exception .

    METHODS read_latest_version
      IMPORTING
        !iv_destination TYPE rfcdest
        !iv_objtype     TYPE vrsd-objtype
        !iv_objname     TYPE vrsd-objname
      EXPORTING
        !ev_found       TYPE abap_bool
        !es_version     TYPE vrsd .

    METHODS map_vrs_objtype
      IMPORTING
        !iv_object        TYPE tadir-object
      RETURNING
        VALUE(rv_objtype) TYPE vrsd-objtype .

    METHODS pull_prd_source
      IMPORTING
        !ii_repo_online   TYPE REF TO zif_abapgit_repo_online
        !iv_target_system TYPE tmscsys-sysnam
        !io_stage         TYPE REF TO zcl_abapgit_stage
        !is_stage_objects TYPE zif_abapgit_definitions=>ty_stage_files
      CHANGING
        !ct_manifest      TYPE ty_manifest_tt
      RAISING
        zcx_abapgit_exception .

    METHODS read_remote_source
      IMPORTING
        !iv_target_system TYPE tmscsys-sysnam
        !iv_objtype       TYPE vrsd-objtype
        !iv_objname       TYPE vrsd-objname
      EXPORTING
        !ev_found         TYPE abap_bool
        !et_source        TYPE string_table .

    METHODS override_source_file
      IMPORTING
        !io_stage         TYPE REF TO zcl_abapgit_stage
        !is_stage_objects TYPE zif_abapgit_definitions=>ty_stage_files
        !iv_object        TYPE tadir-object
        !iv_obj_name      TYPE tadir-obj_name
        !it_source        TYPE string_table
      RETURNING
        VALUE(rv_done)    TYPE abap_bool
      RAISING
        zcx_abapgit_exception .

    METHODS render_manifest
      IMPORTING
        !iv_destination   TYPE rfcdest
        !iv_target_system TYPE tmscsys-sysnam
        !it_manifest      TYPE ty_manifest_tt
      RETURNING
        VALUE(rv_content) TYPE string .

    METHODS add_manifest_to_stage
      IMPORTING
        !io_stage   TYPE REF TO zcl_abapgit_stage
        !iv_content TYPE string
      RAISING
        zcx_abapgit_exception .

    METHODS generate_commit_message
      IMPORTING
        !iv_destination   TYPE rfcdest
        !iv_target_system TYPE tmscsys-sysnam
      RETURNING
        VALUE(rs_comment) TYPE zif_abapgit_git_definitions=>ty_comment .

ENDCLASS.



CLASS zcl_abapgit_remote_2_branch IMPLEMENTATION.


  METHOD create.

    DATA:
      lv_branch_name   TYPE string,
      ls_comment       TYPE zif_abapgit_git_definitions=>ty_comment,
      lo_stage         TYPE REF TO zcl_abapgit_stage,
      ls_stage_objects TYPE zif_abapgit_definitions=>ty_stage_files,
      lt_manifest      TYPE ty_manifest_tt,
      ls_line          TYPE ty_manifest_line,
      lv_content       TYPE string.

    lv_branch_name = zcl_abapgit_git_branch_utils=>complete_heads_branch_name(
        zcl_abapgit_git_branch_utils=>normalize_branch_name( iv_branch_name ) ).

    ii_repo_online->create_branch( lv_branch_name ).

    CREATE OBJECT lo_stage.

    " 1) Snapshot the current serialized state of the repository
    ls_stage_objects = zcl_abapgit_stage_logic=>get_stage_logic( )->get( ii_repo_online ).
    stage_current_state(
      io_stage         = lo_stage
      is_stage_objects = ls_stage_objects ).

    " 2) Build PRD-vs-DEV divergence manifest via standard version management
    lt_manifest = build_manifest(
      ii_repo_online = ii_repo_online
      iv_destination = iv_destination ).

    " 2b) Optionally pull the actual PRD source for source-code objects
    "     (PROG / INTF) via standard remote version retrieval. When a TMS
    "     target system is supplied, the staged DEV source is replaced with
    "     the production source so the branch reflects the live PRD baseline.
    IF iv_target_system IS NOT INITIAL.
      pull_prd_source(
        EXPORTING
          ii_repo_online   = ii_repo_online
          iv_target_system = iv_target_system
          io_stage         = lo_stage
          is_stage_objects = ls_stage_objects
        CHANGING
          ct_manifest      = lt_manifest ).
    ENDIF.

    lv_content = render_manifest(
      iv_destination   = iv_destination
      iv_target_system = iv_target_system
      it_manifest      = lt_manifest ).
    add_manifest_to_stage(
      io_stage   = lo_stage
      iv_content = lv_content ).

    " 3) Push everything to the new branch
    ls_comment = generate_commit_message(
      iv_destination   = iv_destination
      iv_target_system = iv_target_system ).
    ii_repo_online->push(
      is_comment = ls_comment
      io_stage   = lo_stage ).

    " Report what happened so the caller can tell the user where the
    " changes went (which branch) and how much diverged / was pulled.
    rs_result-branch_name = zcl_abapgit_git_branch_utils=>get_display_name( lv_branch_name ).
    rs_result-total_count = lines( lt_manifest ).
    LOOP AT lt_manifest INTO ls_line.
      IF ls_line-content_pulled IS NOT INITIAL.
        rs_result-pulled_count = rs_result-pulled_count + 1.
      ENDIF.
      IF ls_line-status CS 'AHEAD'.
        rs_result-ahead_count = rs_result-ahead_count + 1.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD stage_current_state.

    DATA:
      ls_status TYPE zif_abapgit_definitions=>ty_result,
      ls_local  TYPE zif_abapgit_definitions=>ty_file_item.

    LOOP AT is_stage_objects-status INTO ls_status WHERE NOT lstate IS INITIAL.
      CASE ls_status-lstate.
        WHEN zif_abapgit_definitions=>c_state-added
          OR zif_abapgit_definitions=>c_state-modified.
          READ TABLE is_stage_objects-local INTO ls_local
            WITH KEY file-path     = ls_status-path
                     file-filename = ls_status-filename.
          IF sy-subrc = 0.
            io_stage->add(
              iv_path     = ls_local-file-path
              iv_filename = ls_local-file-filename
              iv_data     = ls_local-file-data ).
          ENDIF.
        WHEN zif_abapgit_definitions=>c_state-deleted.
          io_stage->rm(
            iv_path     = ls_status-path
            iv_filename = ls_status-filename ).
        WHEN OTHERS.
          " mixed / unchanged are not staged
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD build_manifest.

    DATA:
      li_repo     TYPE REF TO zif_abapgit_repo,
      lt_tadir    TYPE zif_abapgit_definitions=>ty_tadir_tt,
      ls_tadir    TYPE zif_abapgit_definitions=>ty_tadir,
      lv_objtype  TYPE vrsd-objtype,
      lv_objname  TYPE vrsd-objname,
      lv_found    TYPE abap_bool,
      ls_dev      TYPE vrsd,
      ls_prd      TYPE vrsd,
      ls_line     TYPE ty_manifest_line.

    li_repo ?= ii_repo_online.
    lt_tadir = li_repo->get_tadir_objects( ).

    LOOP AT lt_tadir INTO ls_tadir.
      CLEAR ls_line.
      ls_line-obj_type = ls_tadir-object.
      ls_line-obj_name = ls_tadir-obj_name.

      lv_objtype = map_vrs_objtype( ls_tadir-object ).
      IF lv_objtype IS INITIAL.
        ls_line-status = 'Not version-compared (composite/unsupported type)'.
        APPEND ls_line TO rt_manifest.
        CONTINUE.
      ENDIF.

      lv_objname = ls_tadir-obj_name.

      " DEV (local) latest version
      read_latest_version(
        EXPORTING
          iv_destination = space
          iv_objtype     = lv_objtype
          iv_objname     = lv_objname
        IMPORTING
          ev_found       = lv_found
          es_version     = ls_dev ).
      IF lv_found = abap_true.
        ls_line-dev_versno = ls_dev-versno.
        ls_line-dev_date   = ls_dev-datum.
        ls_line-dev_trkorr = ls_dev-korrnum.
      ENDIF.

      " PRD (remote) latest version
      read_latest_version(
        EXPORTING
          iv_destination = iv_destination
          iv_objtype     = lv_objtype
          iv_objname     = lv_objname
        IMPORTING
          ev_found       = lv_found
          es_version     = ls_prd ).
      IF lv_found = abap_true.
        ls_line-prd_versno = ls_prd-versno.
        ls_line-prd_date   = ls_prd-datum.
        ls_line-prd_trkorr = ls_prd-korrnum.
      ENDIF.

      IF ls_line-prd_versno IS INITIAL AND ls_line-dev_versno IS INITIAL.
        ls_line-status = 'No version information available'.
      ELSEIF ls_line-prd_versno IS INITIAL.
        ls_line-status = 'Not found in PRD (no recorded version)'.
      ELSEIF ls_line-dev_versno IS INITIAL.
        ls_line-status = 'No DEV version recorded'.
      ELSEIF ls_line-dev_trkorr = ls_line-prd_trkorr.
        ls_line-status = 'IN-SYNC with PRD'.
      ELSE.
        ls_line-status = 'DEV-AHEAD (diverged from PRD)'.
      ENDIF.

      APPEND ls_line TO rt_manifest.
    ENDLOOP.

  ENDMETHOD.


  METHOD read_latest_version.

    DATA:
      lt_vrsd TYPE STANDARD TABLE OF vrsd,
      lt_vrsn TYPE STANDARD TABLE OF vrsn.

    CLEAR: ev_found, es_version.

    CALL FUNCTION 'SVRS_GET_VERSION_DIRECTORY_46'
      EXPORTING
        destination           = iv_destination
        objname               = iv_objname
        objtype               = iv_objtype
      TABLES
        lversno_list          = lt_vrsn
        version_list          = lt_vrsd
      EXCEPTIONS
        no_entry              = 1
        communication_failure = 2
        system_failure        = 3
        OTHERS                = 4.
    IF sy-subrc <> 0.
      " Defensive: object not versioned, type not applicable, or RFC
      " problem - simply report "not found" and let the caller continue.
      RETURN.
    ENDIF.

    SORT lt_vrsd BY versno DESCENDING.
    READ TABLE lt_vrsd INTO es_version INDEX 1.
    IF sy-subrc = 0.
      ev_found = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD map_vrs_objtype.

    " Maps the abapGit/TADIR object type to the version-management
    " object type (VRSD-OBJTYPE). Only reliable 1:1 mappings are listed;
    " everything else is intentionally left blank and reported as
    " "not version-compared".
    CASE iv_object.
      WHEN 'PROG'.
        rv_objtype = 'REPS'.
      WHEN 'INTF'.
        rv_objtype = 'INTF'.
      WHEN 'TABL'.
        rv_objtype = 'TABD'.
      WHEN 'DTEL'.
        rv_objtype = 'DTED'.
      WHEN 'DOMA'.
        rv_objtype = 'DOMD'.
      WHEN 'VIEW'.
        rv_objtype = 'VIED'.
      WHEN 'TTYP'.
        rv_objtype = 'TTYD'.
      WHEN 'ENQU'.
        rv_objtype = 'ENQD'.
      WHEN 'SHLP'.
        rv_objtype = 'SHLD'.
      WHEN OTHERS.
        CLEAR rv_objtype.
    ENDCASE.

  ENDMETHOD.


  METHOD render_manifest.

    DATA:
      ls_line    TYPE ty_manifest_line,
      lv_ahead   TYPE i,
      lv_insync  TYPE i,
      lv_skipped TYPE i,
      lv_pulled  TYPE i.

    LOOP AT it_manifest INTO ls_line.
      IF ls_line-content_pulled IS NOT INITIAL.
        lv_pulled = lv_pulled + 1.
      ENDIF.
      IF ls_line-status CS 'DEV-AHEAD'.
        lv_ahead = lv_ahead + 1.
      ELSEIF ls_line-status CS 'IN-SYNC'.
        lv_insync = lv_insync + 1.
      ELSE.
        lv_skipped = lv_skipped + 1.
      ENDIF.
    ENDLOOP.

    rv_content = |# PRD Divergence Manifest\n\n|.
    rv_content = rv_content && |Remote (production) RFC destination: `{ iv_destination }`\n|.
    IF iv_target_system IS NOT INITIAL.
      rv_content = rv_content && |PRD content retrieved from TMS target system: `{ iv_target_system }`\n|.
    ELSE.
      rv_content = rv_content && |PRD content pull: not requested (divergence manifest only)\n|.
    ENDIF.
    rv_content = rv_content && |Generated by { sy-uname } on { sy-datum+0(4) }-{ sy-datum+4(2) }-{ sy-datum+6(2) }\n\n|.
    rv_content = rv_content && |## Summary\n\n|.
    rv_content = rv_content && |- Objects diverged from PRD (DEV-AHEAD): **{ lv_ahead }**\n|.
    rv_content = rv_content && |- Objects in sync with PRD: **{ lv_insync }**\n|.
    rv_content = rv_content && |- Objects not version-compared: **{ lv_skipped }**\n|.
    rv_content = rv_content && |- Objects whose PRD source was pulled into this branch: **{ lv_pulled }**\n\n|.
    rv_content = rv_content && |> A "DEV-AHEAD" object is one whose latest recorded transport in DEV differs |.
    rv_content = rv_content && |from the one currently active in PRD - i.e. exactly the objects you must |.
    rv_content = rv_content && |review when preparing a production hot-fix.\n\n|.
    rv_content = rv_content && |> Where "PRD source pulled" is shown, the file content in this branch is the |.
    rv_content = rv_content && |production source retrieved via standard remote version management |.
    rv_content = rv_content && |(no PRD-side abapGit/Z-code required). All other objects keep their DEV |.
    rv_content = rv_content && |serialization.\n\n|.
    rv_content = rv_content && |## Objects\n\n|.
    rv_content = rv_content &&
      |\| Type \| Name \| DEV ver \| DEV transport \| PRD ver \| PRD transport \| | &&
      |Status \| PRD content \|\n|.
    rv_content = rv_content &&
      |\|------\|------\|---------\|---------------\|---------\|---------------\| | &&
      |--------\|-------------\|\n|.

    LOOP AT it_manifest INTO ls_line.
      rv_content = rv_content &&
        |\| { ls_line-obj_type } \| { ls_line-obj_name } \| { ls_line-dev_versno } | &&
        |\| { ls_line-dev_trkorr } \| { ls_line-prd_versno } \| { ls_line-prd_trkorr } | &&
        |\| { ls_line-status } \| { ls_line-content_pulled } \|\n|.
    ENDLOOP.

  ENDMETHOD.


  METHOD add_manifest_to_stage.

    DATA lv_data TYPE xstring.

    lv_data = zcl_abapgit_convert=>string_to_xstring_utf8( iv_content ).

    io_stage->add(
      iv_path     = zif_abapgit_definitions=>c_root_dir
      iv_filename = c_manifest_filename
      iv_data     = lv_data ).

  ENDMETHOD.


  METHOD generate_commit_message.

    rs_comment-committer-name  = sy-uname.
    rs_comment-committer-email = |{ rs_comment-committer-name }@localhost|.
    IF iv_target_system IS INITIAL.
      rs_comment-comment = |Remote snapshot to branch (reference { iv_destination })|.
    ELSE.
      rs_comment-comment = |Remote snapshot to branch (reference { iv_destination }, content { iv_target_system })|.
    ENDIF.

  ENDMETHOD.


  METHOD pull_prd_source.

    DATA:
      li_repo    TYPE REF TO zif_abapgit_repo,
      lt_tadir   TYPE zif_abapgit_definitions=>ty_tadir_tt,
      ls_tadir   TYPE zif_abapgit_definitions=>ty_tadir,
      lv_objtype TYPE vrsd-objtype,
      lv_found   TYPE abap_bool,
      lt_source  TYPE string_table,
      lv_done    TYPE abap_bool.

    FIELD-SYMBOLS <ls_line> TYPE ty_manifest_line.

    li_repo ?= ii_repo_online.
    lt_tadir = li_repo->get_tadir_objects( ).

    LOOP AT lt_tadir INTO ls_tadir.
      " Only source-code objects with a single, unambiguous abapGit source
      " file are pulled. Composite objects (CLAS, FUGR, DDIC, ...) keep
      " their DEV serialization and are reported as such in the manifest.
      IF ls_tadir-object <> 'PROG' AND ls_tadir-object <> 'INTF'.
        CONTINUE.
      ENDIF.

      lv_objtype = map_vrs_objtype( ls_tadir-object ).
      IF lv_objtype IS INITIAL.
        CONTINUE.
      ENDIF.

      CLEAR lt_source.
      read_remote_source(
        EXPORTING
          iv_target_system = iv_target_system
          iv_objtype       = lv_objtype
          iv_objname       = ls_tadir-obj_name
        IMPORTING
          ev_found         = lv_found
          et_source        = lt_source ).

      IF lv_found = abap_false OR lt_source IS INITIAL.
        " Nothing reliable retrieved - never overwrite with empty content.
        CONTINUE.
      ENDIF.

      lv_done = override_source_file(
        io_stage         = io_stage
        is_stage_objects = is_stage_objects
        iv_object        = ls_tadir-object
        iv_obj_name      = ls_tadir-obj_name
        it_source        = lt_source ).

      IF lv_done = abap_true.
        READ TABLE ct_manifest ASSIGNING <ls_line>
          WITH KEY obj_type = ls_tadir-object
                   obj_name = ls_tadir-obj_name.
        IF sy-subrc = 0.
          <ls_line>-content_pulled = 'PRD source pulled'.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD read_remote_source.

    " Retrieves the production source of a source-code object via the
    " standard remote version-management function module. No abapGit or
    " custom Z-code has to exist on the production system; the call is the
    " programmatic equivalent of SE38 -> Utilities -> Versions -> Retrieve
    " from remote system.
    "
    " The returned SVRS2_VERSIONABLE_OBJECT is a deep, type-specific
    " structure. Source code is carried in its ABAPTEXT table. Access is
    " fully defensive (dynamic ASSIGN COMPONENT, exception-guarded call) so
    " that an unsupported object, a missing component or any RFC/TMS issue
    " simply results in EV_FOUND = abap_false and the caller keeping the
    " DEV serialization - it can never push wrong or empty content.

    DATA ls_object TYPE svrs2_versionable_object.

    FIELD-SYMBOLS:
      <lv_comp>   TYPE any,
      <lt_source> TYPE ANY TABLE,
      <ls_row>    TYPE any,
      <lv_line>   TYPE any.

    CLEAR: ev_found, et_source.

    ASSIGN COMPONENT 'OBJTYPE' OF STRUCTURE ls_object TO <lv_comp>.
    IF <lv_comp> IS ASSIGNED.
      <lv_comp> = iv_objtype.
    ENDIF.
    UNASSIGN <lv_comp>.

    ASSIGN COMPONENT 'OBJNAME' OF STRUCTURE ls_object TO <lv_comp>.
    IF <lv_comp> IS ASSIGNED.
      <lv_comp> = iv_objname.
    ENDIF.

    CALL FUNCTION 'SVRS_GET_VERSION_REMOTE'
      EXPORTING
        p_tarsystem         = iv_target_system
      CHANGING
        object              = ls_object
      EXCEPTIONS
        no_version          = 1
        system_error        = 2
        communication_error = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ASSIGN COMPONENT 'ABAPTEXT' OF STRUCTURE ls_object TO <lt_source>.
    IF <lt_source> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    LOOP AT <lt_source> ASSIGNING <ls_row>.
      ASSIGN COMPONENT 'LINE' OF STRUCTURE <ls_row> TO <lv_line>.
      IF <lv_line> IS ASSIGNED.
        APPEND <lv_line> TO et_source.
      ENDIF.
    ENDLOOP.

    IF et_source IS NOT INITIAL.
      ev_found = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD override_source_file.

    DATA:
      lv_filename TYPE string,
      lv_source   TYPE string,
      lv_line     TYPE string,
      lv_data     TYPE xstring,
      ls_local    TYPE zif_abapgit_definitions=>ty_file_item.

    CASE iv_object.
      WHEN 'PROG'.
        lv_filename = |{ to_lower( iv_obj_name ) }.prog.abap|.
      WHEN 'INTF'.
        lv_filename = |{ to_lower( iv_obj_name ) }.intf.abap|.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    " Locate the matching staged DEV file so we reuse its exact path.
    READ TABLE is_stage_objects-local INTO ls_local
      WITH KEY item-obj_type = iv_object
               item-obj_name = iv_obj_name
               file-filename = lv_filename.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT it_source INTO lv_line.
      IF lv_source IS INITIAL.
        lv_source = lv_line.
      ELSE.
        lv_source = lv_source && cl_abap_char_utilities=>newline && lv_line.
      ENDIF.
    ENDLOOP.
    lv_source = lv_source && cl_abap_char_utilities=>newline.

    lv_data = zcl_abapgit_convert=>string_to_xstring_utf8( lv_source ).

    " add( ) with the same path + filename replaces the previously staged
    " DEV version, so the branch now carries the production source.
    io_stage->add(
      iv_path     = ls_local-file-path
      iv_filename = ls_local-file-filename
      iv_data     = lv_data ).

    rv_done = abap_true.

  ENDMETHOD.

ENDCLASS.
