CLASS zcl_abapgit_stage_changed_by DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_changed_by,
        obj_type TYPE zif_abapgit_definitions=>ty_item-obj_type,
        obj_name TYPE zif_abapgit_definitions=>ty_item-obj_name,
        filename TYPE string,
        name     TYPE syuname,
      END OF ty_changed_by .
    TYPES:
      ty_changed_by_tt TYPE SORTED TABLE OF ty_changed_by WITH UNIQUE KEY obj_type obj_name filename.
    TYPES:
      BEGIN OF ty_file_user,
        path     TYPE string,
        filename TYPE string,
        name     TYPE syuname,
      END OF ty_file_user .
    TYPES:
      ty_file_users_tt TYPE SORTED TABLE OF ty_file_user WITH UNIQUE KEY path filename.

    CLASS-METHODS find_transports
      IMPORTING
        !it_files            TYPE zif_abapgit_definitions=>ty_stage_files
        !io_dot              TYPE REF TO zcl_abapgit_dot_abapgit
      RETURNING
        VALUE(rt_transports) TYPE zif_abapgit_cts_api=>ty_transport_list .
    CLASS-METHODS find_changed_by
      IMPORTING
        !it_files            TYPE zif_abapgit_definitions=>ty_stage_files
        !io_dot              TYPE REF TO zcl_abapgit_dot_abapgit
        !it_transports       TYPE zif_abapgit_cts_api=>ty_transport_list
      RETURNING
        VALUE(rt_changed_by) TYPE ty_changed_by_tt .
    CLASS-METHODS get_name
      IMPORTING
        !it_changed_by TYPE ty_changed_by_tt
        !is_item       TYPE zif_abapgit_definitions=>ty_item
        !iv_filename   TYPE string
      RETURNING
        VALUE(rv_name) TYPE syuname .
    "! Users of files that exist only in the remote repository (deleted in the system):
    "! the owner of the transport the object is recorded in, as on the staging page
    CLASS-METHODS get_users_of_remote_files
      IMPORTING
        !it_remote      TYPE zif_abapgit_git_definitions=>ty_files_tt
        !io_dot         TYPE REF TO zcl_abapgit_dot_abapgit
      RETURNING
        VALUE(rt_users) TYPE ty_file_users_tt .

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS remote_to_object
      IMPORTING
        !is_remote     TYPE zif_abapgit_git_definitions=>ty_file
        !io_dot        TYPE REF TO zcl_abapgit_dot_abapgit
      RETURNING
        VALUE(rs_item) TYPE zif_abapgit_definitions=>ty_item
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_stage_changed_by IMPLEMENTATION.


  METHOD find_changed_by.

    DATA: ls_local             LIKE LINE OF it_files-local,
          ls_remote            LIKE LINE OF it_files-remote,
          ls_changed_by        LIKE LINE OF rt_changed_by,
          lt_changed_by_remote LIKE rt_changed_by,
          ls_item              TYPE zif_abapgit_definitions=>ty_item,
          lv_transport         LIKE LINE OF it_transports,
          lv_user              TYPE uname.

    TYPES:
      BEGIN OF ty_owner,
        trkorr TYPE trkorr,
        user   TYPE uname,
      END OF ty_owner.
    DATA lt_owners TYPE SORTED TABLE OF ty_owner WITH UNIQUE KEY trkorr.
    DATA ls_owner LIKE LINE OF lt_owners.

    FIELD-SYMBOLS <ls_changed_by> LIKE LINE OF lt_changed_by_remote.

    LOOP AT it_files-local INTO ls_local WHERE NOT item IS INITIAL.
      ls_changed_by-obj_type = ls_local-item-obj_type.
      ls_changed_by-obj_name = ls_local-item-obj_name.
      ls_changed_by-filename = ls_local-file-filename.
      ls_changed_by-name = zcl_abapgit_objects=>changed_by(
        is_item     = ls_local-item
        iv_filename = ls_local-file-filename ).
      INSERT ls_changed_by INTO TABLE rt_changed_by.
    ENDLOOP.

    LOOP AT it_files-remote INTO ls_remote WHERE filename IS NOT INITIAL.
      " Start empty: a remote file must not inherit the user of the last local file
      CLEAR ls_changed_by.
      TRY.
          ls_item = remote_to_object( is_remote = ls_remote
                                      io_dot    = io_dot ).
          ls_changed_by-obj_type = ls_item-obj_type.
          ls_changed_by-obj_name = ls_item-obj_name.
          ls_changed_by-filename = ls_remote-filename.
          INSERT ls_changed_by INTO TABLE lt_changed_by_remote.
        CATCH zcx_abapgit_exception ##NO_HANDLER.
      ENDTRY.
    ENDLOOP.

    LOOP AT lt_changed_by_remote ASSIGNING <ls_changed_by>.
      " deleted files might still be in a transport
      CLEAR lv_transport.
      READ TABLE it_transports WITH KEY
        obj_type = <ls_changed_by>-obj_type
        obj_name = <ls_changed_by>-obj_name
        INTO lv_transport.
      IF sy-subrc = 0.
        " The files of an object share its transport, read each owner once
        READ TABLE lt_owners INTO ls_owner WITH TABLE KEY trkorr = lv_transport-trkorr.
        IF sy-subrc = 0.
          lv_user = ls_owner-user.
        ELSE.
          lv_user = zcl_abapgit_factory=>get_cts_api( )->read_user( lv_transport-trkorr ).
          ls_owner-trkorr = lv_transport-trkorr.
          ls_owner-user   = lv_user.
          INSERT ls_owner INTO TABLE lt_owners.
        ENDIF.
        IF lv_user IS NOT INITIAL.
          <ls_changed_by>-name = lv_user.
        ENDIF.
      ENDIF.
      IF <ls_changed_by>-name IS INITIAL.
        <ls_changed_by>-name = zcl_abapgit_objects_super=>c_user_unknown.
      ENDIF.
    ENDLOOP.

    INSERT LINES OF lt_changed_by_remote INTO TABLE rt_changed_by.

  ENDMETHOD.


  METHOD find_transports.

    DATA li_cts_api TYPE REF TO zif_abapgit_cts_api.
    DATA lt_items TYPE zif_abapgit_definitions=>ty_items_tt.
    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.
    FIELD-SYMBOLS <ls_local> LIKE LINE OF it_files-local.
    FIELD-SYMBOLS <ls_remote> LIKE LINE OF it_files-remote.


    li_cts_api = zcl_abapgit_factory=>get_cts_api( ).

    TRY.
        LOOP AT it_files-local ASSIGNING <ls_local> WHERE item IS NOT INITIAL.
          IF li_cts_api->is_chrec_possible_for_package( <ls_local>-item-devclass ) = abap_false.
            RETURN. " Assume all other objects are also in packages without change recording
          ENDIF.
          APPEND <ls_local>-item TO lt_items.
        ENDLOOP.

        LOOP AT it_files-remote ASSIGNING <ls_remote> WHERE filename IS NOT INITIAL.
          ls_item = remote_to_object( is_remote = <ls_remote>
                                      io_dot    = io_dot ).
          IF ls_item IS INITIAL.
            CONTINUE.
          ENDIF.
          APPEND ls_item TO lt_items.
        ENDLOOP.

        SORT lt_items BY obj_type obj_name.
        DELETE ADJACENT DUPLICATES FROM lt_items COMPARING obj_type obj_name.

        rt_transports = li_cts_api->get_transports_for_list( lt_items ).

      CATCH zcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD get_name.

    DATA ls_changed_by LIKE LINE OF it_changed_by.

    READ TABLE it_changed_by INTO ls_changed_by WITH TABLE KEY
      obj_type = is_item-obj_type
      obj_name = is_item-obj_name
      filename = iv_filename.
    IF sy-subrc = 0.
      rv_name = ls_changed_by-name.
    ENDIF.

  ENDMETHOD.


  METHOD get_users_of_remote_files.

    DATA ls_files      TYPE zif_abapgit_definitions=>ty_stage_files.
    DATA lt_changed_by TYPE ty_changed_by_tt.
    DATA ls_item       TYPE zif_abapgit_definitions=>ty_item.
    DATA ls_user       LIKE LINE OF rt_users.

    FIELD-SYMBOLS <ls_remote> LIKE LINE OF it_remote.

    IF it_remote IS INITIAL.
      RETURN.
    ENDIF.

    ls_files-remote = it_remote.
    lt_changed_by = find_changed_by(
      it_files      = ls_files
      io_dot        = io_dot
      it_transports = find_transports( it_files = ls_files
                                       io_dot   = io_dot ) ).

    LOOP AT it_remote ASSIGNING <ls_remote>.
      TRY.
          ls_item = remote_to_object( is_remote = <ls_remote>
                                      io_dot    = io_dot ).
        CATCH zcx_abapgit_exception.
          CONTINUE.
      ENDTRY.

      ls_user-path     = <ls_remote>-path.
      ls_user-filename = <ls_remote>-filename.
      ls_user-name     = get_name(
        it_changed_by = lt_changed_by
        is_item       = ls_item
        iv_filename   = <ls_remote>-filename ).
      INSERT ls_user INTO TABLE rt_users.
    ENDLOOP.

  ENDMETHOD.


  METHOD remote_to_object.

    zcl_abapgit_filename_logic=>file_to_object(
      EXPORTING
        iv_filename = is_remote-filename
        iv_path     = is_remote-path
        io_dot      = io_dot
      IMPORTING
        es_item     = rs_item ).

  ENDMETHOD.
ENDCLASS.
