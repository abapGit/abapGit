CLASS zcl_abapgit_gitv2_porcelain DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_git_factory .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gitv2_porcelain.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_service,
        receive TYPE string VALUE 'receive',                "#EC NOTEXT
        upload  TYPE string VALUE 'upload',                 "#EC NOTEXT
      END OF c_service .

    CONSTANTS c_flush_pkt TYPE c LENGTH 4 VALUE '0000'.
    CONSTANTS c_delim_pkt TYPE c LENGTH 4 VALUE '0001'.

    CLASS-METHODS get_request_uri
      IMPORTING
        iv_url        TYPE string
        iv_service    TYPE string
      RETURNING
        VALUE(rv_uri) TYPE string
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS send_command
      IMPORTING
        iv_url             TYPE string
        iv_service         TYPE string
        iv_command         TYPE string
        it_arguments       TYPE string_table OPTIONAL
      RETURNING
        VALUE(rv_response) TYPE xstring
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS decode_pack
      IMPORTING
        iv_xstring        TYPE xstring
      RETURNING
        VALUE(rt_objects) TYPE zif_abapgit_definitions=>ty_objects_tt
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS path_needed
      IMPORTING
        !iv_path         TYPE string
        !it_wanted_paths TYPE string_table OPTIONAL
      RETURNING
        VALUE(rv_needed) TYPE abap_bool.

    CLASS-METHODS compute_max_depth
      IMPORTING
        !it_wanted_paths TYPE string_table OPTIONAL
      RETURNING
        VALUE(rv_depth)  TYPE i.

    CLASS-METHODS fetch_trees_at_depth
      IMPORTING
        !iv_url           TYPE string
        !iv_commit        TYPE zif_abapgit_git_definitions=>ty_sha1
        !iv_max_depth     TYPE i
      RETURNING
        VALUE(rt_objects) TYPE zif_abapgit_definitions=>ty_objects_tt
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS walk_tree_from_objects
      IMPORTING
        !it_objects      TYPE zif_abapgit_definitions=>ty_objects_tt
        !iv_base         TYPE string
        !it_wanted_paths TYPE string_table OPTIONAL
      CHANGING
        !ct_expanded     TYPE zif_abapgit_git_definitions=>ty_expanded_tt
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS walk_tree_level
      IMPORTING
        !it_objects      TYPE zif_abapgit_definitions=>ty_objects_tt
        !iv_tree_sha1    TYPE zif_abapgit_git_definitions=>ty_sha1
        !iv_base         TYPE string
        !it_wanted_paths TYPE string_table OPTIONAL
      CHANGING
        !ct_expanded     TYPE zif_abapgit_git_definitions=>ty_expanded_tt
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_gitv2_porcelain IMPLEMENTATION.


  METHOD decode_pack.

    DATA lv_xstring TYPE xstring.
    DATA lv_contents  TYPE xstring.
    DATA lv_pack      TYPE xstring.
    DATA lv_pktlen    TYPE i.
    DATA lv_hex4      TYPE xstring.

    lv_xstring = iv_xstring.

* The data transfer of the packfile is always multiplexed, using the same semantics of the
* side-band-64k capability from protocol version 1
    WHILE xstrlen( lv_xstring ) > 0.
      lv_hex4 = lv_xstring(4).
      lv_pktlen = zcl_abapgit_git_utils=>length_utf8_hex( lv_hex4 ).
      IF lv_pktlen = 0.
        EXIT.
      ELSEIF lv_pktlen = 1.
* its a delimiter package
        lv_xstring = lv_xstring+4.
        CONTINUE.
      ENDIF.
      lv_contents = lv_xstring(lv_pktlen).
      IF lv_contents+4(1) = '01'.
        CONCATENATE lv_pack lv_contents+5 INTO lv_pack IN BYTE MODE.
      ENDIF.
      lv_xstring = lv_xstring+lv_pktlen.
    ENDWHILE.

    rt_objects = zcl_abapgit_git_pack=>decode( lv_pack ).

  ENDMETHOD.


  METHOD get_request_uri.
    rv_uri = zcl_abapgit_url=>path_name( iv_url ) && |/info/refs?service=git-{ iv_service }-pack|.
  ENDMETHOD.


  METHOD send_command.

    CONSTANTS lc_content_regex TYPE string VALUE '^[0-9a-f]{4}#'.

    DATA lo_client   TYPE REF TO zcl_abapgit_http_client.
    DATA lv_cmd_pkt  TYPE string.
    DATA lt_headers  TYPE zcl_abapgit_http=>ty_headers.
    DATA ls_header   LIKE LINE OF lt_headers.
    DATA lv_argument TYPE string.


    ls_header-key   = 'Git-Protocol'.
    ls_header-value = 'version=2'.
    APPEND ls_header TO lt_headers.
    ls_header-key   = '~request_uri'.
    ls_header-value = get_request_uri( iv_url     = iv_url
                                       iv_service = iv_service ).
    APPEND ls_header TO lt_headers.

    lo_client = zcl_abapgit_http=>create_by_url(
      iv_url     = iv_url
      it_headers = lt_headers ).

    lo_client->check_smart_response(
      iv_expected_content_type = |application/x-git-{ iv_service }-pack-advertisement|
      iv_content_regex         = lc_content_regex ).

    lv_cmd_pkt = zcl_abapgit_git_utils=>pkt_string( |command={ iv_command }\n| )
      && zcl_abapgit_git_utils=>pkt_string( |agent={ zcl_abapgit_http=>get_agent( ) }\n| ).
    IF lines( it_arguments ) > 0.
      lv_cmd_pkt = lv_cmd_pkt && c_delim_pkt.
      LOOP AT it_arguments INTO lv_argument.
        lv_cmd_pkt = lv_cmd_pkt && zcl_abapgit_git_utils=>pkt_string( lv_argument ).
      ENDLOOP.
    ENDIF.
    lv_cmd_pkt = lv_cmd_pkt && c_flush_pkt.

    lo_client->set_header(
      iv_key   = '~request_uri'
      iv_value = zcl_abapgit_url=>path_name( iv_url ) && |/git-{ iv_service }-pack| ).

    lo_client->set_header(
      iv_key   = '~request_method'
      iv_value = 'POST' ).

    lo_client->set_header(
      iv_key   = 'Content-Type'
      iv_value = |application/x-git-{ iv_service }-pack-request| ).

    lo_client->set_header(
      iv_key   = 'Accept'
      iv_value = |application/x-git-{ iv_service }-pack-result| ).

    rv_response = lo_client->send_receive_close( zcl_abapgit_convert=>string_to_xstring_utf8( lv_cmd_pkt ) ).

  ENDMETHOD.


  METHOD zif_abapgit_gitv2_porcelain~commits_last_year.
* including trees
    DATA lv_xstring   TYPE xstring.
    DATA lt_arguments TYPE string_table.
    DATA lv_argument  TYPE string.
    DATA lv_sha1      LIKE LINE OF it_sha1.


    ASSERT lines( it_sha1 ) > 0.

    lv_argument = |deepen-since { zcl_abapgit_git_time=>get_one_year_ago( ) }|.
    APPEND lv_argument TO lt_arguments.
    LOOP AT it_sha1 INTO lv_sha1.
      lv_argument = |want { lv_sha1 }|.
      APPEND lv_argument TO lt_arguments.
    ENDLOOP.
* 'filter object:type=commit' doesn't work on github
    APPEND 'filter blob:none' TO lt_arguments.
    APPEND 'no-progress' TO lt_arguments.
    APPEND 'done' TO lt_arguments.

    lv_xstring = send_command(
      iv_url       = iv_url
      iv_service   = c_service-upload
      iv_command   = |fetch|
      it_arguments = lt_arguments ).

    rt_objects = decode_pack( lv_xstring ).

  ENDMETHOD.


  METHOD zif_abapgit_gitv2_porcelain~fetch_blob.

    DATA lv_xstring   TYPE xstring.
    DATA lt_arguments TYPE string_table.
    DATA lv_argument  TYPE string.
    DATA lt_objects   TYPE zif_abapgit_definitions=>ty_objects_tt.
    DATA ls_object    LIKE LINE OF lt_objects.


    ASSERT iv_sha1 IS NOT INITIAL.

    lv_argument = |want { iv_sha1 }|.
    APPEND lv_argument TO lt_arguments.
    APPEND 'no-progress' TO lt_arguments.
    APPEND 'done' TO lt_arguments.

    lv_xstring = send_command(
      iv_url       = iv_url
      iv_service   = c_service-upload
      iv_command   = |fetch|
      it_arguments = lt_arguments ).

    lt_objects = decode_pack( lv_xstring ).
    IF lines( lt_objects ) <> 1.
      zcx_abapgit_exception=>raise( |Blob { iv_sha1 } not found in response.| ).
    ENDIF.

    READ TABLE lt_objects INTO ls_object INDEX 1.
    ASSERT sy-subrc = 0.
    rv_blob = ls_object-data.

  ENDMETHOD.


  METHOD zif_abapgit_gitv2_porcelain~fetch_blobs.

    DATA lv_xstring   TYPE xstring.
    DATA lt_arguments TYPE string_table.
    DATA lv_argument  TYPE string.
    DATA lv_sha1      LIKE LINE OF it_sha1.


    ASSERT lines( it_sha1 ) > 0.

    LOOP AT it_sha1 INTO lv_sha1.
      lv_argument = |want { lv_sha1 }|.
      APPEND lv_argument TO lt_arguments.
    ENDLOOP.
    APPEND 'no-progress' TO lt_arguments.
    APPEND 'done' TO lt_arguments.

    lv_xstring = send_command(
      iv_url       = iv_url
      iv_service   = c_service-upload
      iv_command   = |fetch|
      it_arguments = lt_arguments ).

    rt_objects = decode_pack( lv_xstring ).

  ENDMETHOD.


  METHOD zif_abapgit_gitv2_porcelain~list_branches.
    DATA lv_xstring   TYPE xstring.
    DATA lt_arguments TYPE string_table.
    DATA lv_argument  TYPE string.
    DATA lv_data      TYPE string.

    IF iv_prefix IS NOT INITIAL.
      lv_argument = |ref-prefix { iv_prefix }|.
      APPEND lv_argument TO lt_arguments.
    ENDIF.

    lv_xstring = send_command(
      iv_url       = iv_url
      iv_service   = c_service-upload
      iv_command   = |ls-refs|
      it_arguments = lt_arguments ).

    " add dummy packet so the v1 branch parsing can be reused
    lv_data = |0004\n{ zcl_abapgit_convert=>xstring_to_string_utf8_raw( lv_xstring ) }|.

    CREATE OBJECT ro_list TYPE zcl_abapgit_git_branch_list
      EXPORTING
        iv_data = lv_data.

  ENDMETHOD.


  METHOD zif_abapgit_gitv2_porcelain~list_trees_for_paths.

    " Fetch only the trees needed to reach the wanted paths.
    " Uses filter tree:<depth> so the server sends only commit + trees at
    " depth <= max_depth - a tiny response even for large repos.
    " Then walks the cached objects locally to produce the file listing.

    DATA lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt.
    DATA lv_max_depth TYPE i.

    " Determine fetch depth: count '/' chars in deepest wanted path.
    " e.g. /src/pkg/ has 3 slashes -> depth 3. filter tree:<N> sends
    " commit + trees at depth <= N levels from the commit root tree.
    lv_max_depth = compute_max_depth( it_wanted_paths ).

    " Fetch the commit + all trees up to lv_max_depth in one request.
    " filter tree:<N> sends commit + trees at depth <= N from commit.
    lt_objects = fetch_trees_at_depth(
      iv_url        = iv_url
      iv_commit     = iv_sha1
      iv_max_depth  = lv_max_depth ).

    " Walk tree locally - all needed objects are already in lt_objects
    walk_tree_from_objects(
      EXPORTING
        it_objects      = lt_objects
        iv_base         = '/'
        it_wanted_paths = it_wanted_paths
      CHANGING
        ct_expanded     = rt_expanded ).

  ENDMETHOD.


  METHOD zif_abapgit_gitv2_porcelain~list_no_blobs.

    DATA lt_sha1    TYPE zif_abapgit_git_definitions=>ty_sha1_tt.
    DATA lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt.

    ASSERT iv_sha1 IS NOT INITIAL.
    APPEND iv_sha1 TO lt_sha1.

    lt_objects = zif_abapgit_gitv2_porcelain~list_no_blobs_multi(
      iv_url  = iv_url
      it_sha1 = lt_sha1 ).

    rt_expanded = zcl_abapgit_git_porcelain=>full_tree(
      it_objects = lt_objects
      iv_parent  = iv_sha1 ).

  ENDMETHOD.


  METHOD zif_abapgit_gitv2_porcelain~list_no_blobs_multi.

    DATA lv_xstring   TYPE xstring.
    DATA lt_arguments TYPE string_table.
    DATA lv_argument  TYPE string.
    DATA lv_sha1      LIKE LINE OF it_sha1.


    ASSERT lines( it_sha1 ) > 0.

    APPEND 'deepen 1' TO lt_arguments.
    LOOP AT it_sha1 INTO lv_sha1.
      lv_argument = |want { lv_sha1 }|.
      APPEND lv_argument TO lt_arguments.
    ENDLOOP.
    APPEND 'filter blob:none' TO lt_arguments.
    APPEND 'no-progress' TO lt_arguments.
    APPEND 'done' TO lt_arguments.

    lv_xstring = send_command(
      iv_url       = iv_url
      iv_service   = c_service-upload
      iv_command   = |fetch|
      it_arguments = lt_arguments ).

    rt_objects = decode_pack( lv_xstring ).

  ENDMETHOD.


  METHOD path_needed.

    DATA lv_wanted TYPE string.

    " No filter - all paths are needed
    IF it_wanted_paths IS INITIAL.
      rv_needed = abap_true.
      RETURN.
    ENDIF.

    " A directory iv_path is needed if:
    " (a) it is an ancestor of a wanted path  (iv_path is prefix of wanted)
    " (b) it is the wanted path itself or a descendant (wanted is prefix of iv_path)
    " Examples with iv_path = '/src/' and wanted = '/src/pkg/sub/':
    "   (a) '/src/' CP '/src/*'  - true  (iv_path is ancestor of wanted)
    " Examples with iv_path = '/src/pkg/sub/' and wanted = '/src/':
    "   (b) '/src/pkg/sub/' CP '/src/*' - true  (wanted is ancestor of iv_path)
    LOOP AT it_wanted_paths INTO lv_wanted.
      IF lv_wanted CP iv_path && '*'   " iv_path is prefix of wanted
          OR iv_path CP lv_wanted && '*'.  " wanted is prefix of iv_path
        rv_needed = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD compute_max_depth.

    " Count the number of '/' separators in the deepest wanted path.
    " e.g. '/src/pkg/' has 3 slashes -> depth 3, '/src/' has 2 -> depth 2.
    " This matches filter tree:<N> semantics: N = number of tree levels
    " to send from the commit root (root tree = 0, its children = 1, etc.).
    " Returns 0 (no filter -> use filter blob:none full fetch).

    DATA lv_wanted    TYPE string.
    DATA lv_depth     TYPE i.
    DATA lv_pos       TYPE i.
    DATA lv_len       TYPE i.
    DATA lv_char      TYPE c LENGTH 1.

    IF it_wanted_paths IS INITIAL.
      rv_depth = 0.
      RETURN.
    ENDIF.

    LOOP AT it_wanted_paths INTO lv_wanted.
      lv_depth = 0.
      lv_len   = strlen( lv_wanted ).
      DO lv_len TIMES.
        lv_pos  = sy-index - 1.
        lv_char = lv_wanted+lv_pos(1).
        IF lv_char = '/'.
          lv_depth = lv_depth + 1.
        ENDIF.
      ENDDO.
      IF lv_depth > rv_depth.
        rv_depth = lv_depth.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD fetch_trees_at_depth.

    " Fetch the commit + all tree objects up to iv_max_depth levels deep.
    " Uses filter tree:<N> which sends only trees at depth <= N from commit.
    " iv_max_depth=0 falls back to filter blob:none (full tree).
    "
    " Note: servers advertising 'filter' capability may not support filter tree:<N>
    " (a newer extension). If the server returns an empty/malformed pack, fall back
    " to filter blob:none which all partial-clone-capable servers support.

    DATA lv_xstring   TYPE xstring.
    DATA lt_arguments TYPE string_table.
    DATA lv_filter    TYPE string.
    DATA lv_argument  TYPE string.
    DATA lx_exc       TYPE REF TO zcx_abapgit_exception.

    lv_argument = |want { iv_commit }|.
    APPEND lv_argument   TO lt_arguments.
    APPEND 'deepen 1'    TO lt_arguments.

    IF iv_max_depth = 0.
      lv_filter = 'filter blob:none'.
    ELSE.
      lv_filter = |filter tree:{ iv_max_depth }|.
    ENDIF.
    APPEND lv_filter     TO lt_arguments.
    APPEND 'no-progress' TO lt_arguments.
    APPEND 'done'        TO lt_arguments.

    lv_xstring = send_command(
                   iv_url        = iv_url
                   iv_service    = c_service-upload
                   iv_command    = |fetch|
                   it_arguments  = lt_arguments ).

    TRY.
        rt_objects = decode_pack( lv_xstring ).
      CATCH zcx_abapgit_exception INTO lx_exc.
        " filter tree:<N> is not universally supported - fall back to filter blob:none
        IF iv_max_depth > 0.
          CLEAR lt_arguments.
          lv_argument = |want { iv_commit }|.
          APPEND lv_argument         TO lt_arguments.
          APPEND 'deepen 1'          TO lt_arguments.
          APPEND 'filter blob:none'  TO lt_arguments.
          APPEND 'no-progress'       TO lt_arguments.
          APPEND 'done'              TO lt_arguments.
          lv_xstring = send_command(
                         iv_url       = iv_url
                         iv_service   = c_service-upload
                         iv_command   = |fetch|
                         it_arguments = lt_arguments ).
          rt_objects = decode_pack( lv_xstring ).
        ELSE.
          zcx_abapgit_exception=>raise_with_text( lx_exc ).
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD walk_tree_from_objects.

    " Walk the tree structure from a set of already-fetched objects.
    " Avoids additional HTTP requests by looking up trees in it_objects.

    DATA ls_commit   TYPE zcl_abapgit_git_pack=>ty_commit.
    DATA ls_object   LIKE LINE OF it_objects.

    " Get the root tree SHA1 from the commit
    READ TABLE it_objects INTO ls_object
      WITH KEY type COMPONENTS type = zif_abapgit_git_definitions=>c_type-commit.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Commit not found in object list' ).
    ENDIF.
    ls_commit = zcl_abapgit_git_pack=>decode_commit( ls_object-data ).

    " Delegate to recursive tree walker using the commit's root tree
    walk_tree_level(
      EXPORTING
        it_objects      = it_objects
        iv_tree_sha1    = ls_commit-tree
        iv_base         = iv_base
        it_wanted_paths = it_wanted_paths
      CHANGING
        ct_expanded     = ct_expanded ).

  ENDMETHOD.


  METHOD walk_tree_level.

    " Recursively walk a tree within the cached object list.

    DATA ls_object   LIKE LINE OF it_objects.
    DATA lt_nodes    TYPE zcl_abapgit_git_pack=>ty_nodes_tt.
    DATA ls_exp      LIKE LINE OF ct_expanded.
    DATA lv_sub_path TYPE string.
    DATA lv_needed   TYPE abap_bool.

    FIELD-SYMBOLS: <ls_node> LIKE LINE OF lt_nodes.

    READ TABLE it_objects INTO ls_object
      WITH KEY type COMPONENTS
        type = zif_abapgit_git_definitions=>c_type-tree
        sha1 = iv_tree_sha1.
    IF sy-subrc <> 0.
      " Tree not in cache - depth-limited fetch may have excluded it; skip
      RETURN.
    ENDIF.

    lt_nodes = zcl_abapgit_git_pack=>decode_tree( ls_object-data ).

    LOOP AT lt_nodes ASSIGNING <ls_node>.
      CASE <ls_node>-chmod.
        WHEN zif_abapgit_git_definitions=>c_chmod-dir.
          lv_sub_path = iv_base && <ls_node>-name && '/'.
          lv_needed = path_needed(
            iv_path         = lv_sub_path
            it_wanted_paths = it_wanted_paths ).
          IF lv_needed = abap_true.
            walk_tree_level(
              EXPORTING
                it_objects      = it_objects
                iv_tree_sha1    = <ls_node>-sha1
                iv_base         = lv_sub_path
                it_wanted_paths = it_wanted_paths
              CHANGING
                ct_expanded     = ct_expanded ).
          ENDIF.
        WHEN OTHERS.
          CLEAR ls_exp.
          ls_exp-path  = iv_base.
          ls_exp-name  = <ls_node>-name.
          ls_exp-sha1  = <ls_node>-sha1.
          ls_exp-chmod = <ls_node>-chmod.
          APPEND ls_exp TO ct_expanded.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
