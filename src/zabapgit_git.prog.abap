*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_GIT
*&---------------------------------------------------------------------*

CLASS ltcl_git_pack DEFINITION DEFERRED.

*----------------------------------------------------------------------*
*       CLASS lcl_transport DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_git_transport DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_branch_list,
             sha1 TYPE ty_sha1,
             name TYPE string,
           END OF ty_branch_list.
    TYPES: ty_branch_list_tt TYPE STANDARD TABLE OF ty_branch_list WITH DEFAULT KEY.

* remote to local
    CLASS-METHODS upload_pack
      IMPORTING io_repo     TYPE REF TO lcl_repo_online
                iv_deepen   TYPE abap_bool DEFAULT abap_true
                it_branches TYPE ty_branch_list_tt OPTIONAL
      EXPORTING et_objects  TYPE ty_objects_tt
                ev_branch   TYPE ty_sha1
      RAISING   lcx_exception.

* local to remote
    CLASS-METHODS receive_pack
      IMPORTING io_repo        TYPE REF TO lcl_repo_online
                iv_old         TYPE ty_sha1
                iv_new         TYPE ty_sha1
                iv_branch_name TYPE string
                iv_pack        TYPE xstring
      RAISING   lcx_exception.

    CLASS-METHODS branches
      IMPORTING iv_url                TYPE string
      RETURNING VALUE(rt_branch_list) TYPE ty_branch_list_tt
      RAISING   lcx_exception.

    CLASS-METHODS class_constructor.

  PRIVATE SECTION.
    CLASS-DATA: gv_agent TYPE string.

    CONSTANTS: BEGIN OF c_service,
                 receive TYPE string VALUE 'receive',       "#EC NOTEXT
                 upload  TYPE string VALUE 'upload',        "#EC NOTEXT
               END OF c_service.

    CLASS-METHODS branch_list
      IMPORTING iv_url         TYPE string
                iv_service     TYPE string
      EXPORTING ei_client      TYPE REF TO if_http_client
                et_branch_list TYPE ty_branch_list_tt
      RAISING   lcx_exception.

    CLASS-METHODS pkt_string
      IMPORTING iv_string     TYPE string
      RETURNING VALUE(rv_pkt) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS find_branch
      IMPORTING io_repo    TYPE REF TO lcl_repo_online
                iv_service TYPE string
      EXPORTING ei_client  TYPE REF TO if_http_client
                ev_branch  TYPE ty_sha1
      RAISING   lcx_exception.

    CLASS-METHODS parse
      EXPORTING ev_pack TYPE xstring
      CHANGING  cv_data TYPE xstring
      RAISING   lcx_exception.

    CLASS-METHODS length_utf8_hex
      IMPORTING iv_data       TYPE xstring
      RETURNING VALUE(rv_len) TYPE i
      RAISING   lcx_exception.

    CLASS-METHODS parse_branch_list
      IMPORTING iv_data        TYPE string
      RETURNING VALUE(rt_list) TYPE ty_branch_list_tt
      RAISING   lcx_exception.

    CLASS-METHODS set_headers
      IMPORTING io_repo    TYPE REF TO lcl_repo_online
                iv_service TYPE string
                ii_client  TYPE REF TO if_http_client
      RAISING   lcx_exception.

    CLASS-METHODS check_http_200
      IMPORTING ii_client TYPE REF TO if_http_client
      RAISING   lcx_exception.

    CLASS-METHODS get_null
      RETURNING VALUE(rv_c) TYPE char1.

ENDCLASS.                    "lcl_transport DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_pack DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_git_pack DEFINITION FINAL FRIENDS ltcl_git_pack.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_node,
             chmod TYPE ty_chmod,
             name  TYPE string,
             sha1  TYPE ty_sha1,
           END OF ty_node.
    TYPES: ty_nodes_tt TYPE STANDARD TABLE OF ty_node WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_commit,
             tree      TYPE ty_sha1,
             parent    TYPE ty_sha1,
             parent2   TYPE ty_sha1,
             author    TYPE string,
             committer TYPE string,
             body      TYPE string,
           END OF ty_commit.

    CLASS-METHODS decode
      IMPORTING iv_data           TYPE xstring
      RETURNING VALUE(rt_objects) TYPE ty_objects_tt
      RAISING   lcx_exception.

    CLASS-METHODS decode_tree
      IMPORTING iv_data         TYPE xstring
      RETURNING VALUE(rt_nodes) TYPE ty_nodes_tt
      RAISING   lcx_exception.

    CLASS-METHODS decode_commit
      IMPORTING iv_data          TYPE xstring
      RETURNING VALUE(rs_commit) TYPE ty_commit
      RAISING   lcx_exception.

    CLASS-METHODS encode
      IMPORTING it_objects     TYPE ty_objects_tt
      RETURNING VALUE(rv_data) TYPE xstring
      RAISING   lcx_exception.

    CLASS-METHODS encode_tree
      IMPORTING it_nodes       TYPE ty_nodes_tt
      RETURNING VALUE(rv_data) TYPE xstring.

    CLASS-METHODS encode_commit
      IMPORTING is_commit      TYPE ty_commit
      RETURNING VALUE(rv_data) TYPE xstring.

  PRIVATE SECTION.
    CONSTANTS: c_pack_start TYPE x LENGTH 4 VALUE '5041434B', " PACK
               c_zlib       TYPE x LENGTH 2 VALUE '789C',
               c_zlib_hmm   TYPE x LENGTH 2 VALUE '7801',
               c_version    TYPE x LENGTH 4 VALUE '00000002'.

    CLASS-METHODS decode_deltas
      CHANGING ct_objects TYPE ty_objects_tt
      RAISING  lcx_exception.

    CLASS-METHODS type_and_length
      IMPORTING is_object         TYPE ty_object
      RETURNING VALUE(rv_xstring) TYPE xstring
      RAISING   lcx_exception.

    CLASS-METHODS delta
      IMPORTING is_object  TYPE ty_object
      CHANGING  ct_objects TYPE ty_objects_tt
      RAISING   lcx_exception.

    CLASS-METHODS delta_header
      EXPORTING ev_header TYPE i
      CHANGING  cv_delta  TYPE xstring.

    CLASS-METHODS sort_tree
      IMPORTING it_nodes        TYPE ty_nodes_tt
      RETURNING VALUE(rt_nodes) TYPE ty_nodes_tt.

    CLASS-METHODS get_type
      IMPORTING iv_x           TYPE x
      RETURNING VALUE(rv_type) TYPE ty_type
      RAISING   lcx_exception.

    CLASS-METHODS get_length
      EXPORTING ev_length TYPE i
      CHANGING  cv_data   TYPE xstring.

ENDCLASS.                    "lcl_pack DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_transport IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_git_transport IMPLEMENTATION.

  METHOD class_constructor.

* bitbucket require agent prefix = "git/"
    gv_agent = 'git/abapGit ' && gc_abap_version.

  ENDMETHOD.                    "class_constructor

  METHOD set_headers.

    DATA: lv_value TYPE string.


    ii_client->request->set_header_field(
        name  = '~request_method'
        value = 'POST' ).

    lv_value = lcl_url=>path_name( io_repo->get_url( ) ) &&
      '.git/git-' &&
      iv_service &&
      '-pack'.
    ii_client->request->set_header_field(
        name  = '~request_uri'
        value = lv_value ).

    lv_value = 'application/x-git-'
                  && iv_service && '-pack-request'.         "#EC NOTEXT
    ii_client->request->set_header_field(
        name  = 'Content-Type'
        value = lv_value ).                                 "#EC NOTEXT

    lv_value = 'application/x-git-'
                  && iv_service && '-pack-result'.          "#EC NOTEXT
    ii_client->request->set_header_field(
        name  = 'Accept'
        value = lv_value ).                                 "#EC NOTEXT

  ENDMETHOD.                    "set_headers

  METHOD get_null.

    DATA: lv_x(4) TYPE x VALUE '00000000',
          lv_z(2) TYPE c.

    FIELD-SYMBOLS <lv_y> TYPE c.


    ASSIGN lv_x TO <lv_y> CASTING.
    lv_z = <lv_y>.
    rv_c = lv_z(1).

  ENDMETHOD.                    "get_null

  METHOD check_http_200.

    DATA: lv_code TYPE i.


    ii_client->response->get_status(
      IMPORTING
        code   = lv_code ).
    CASE lv_code.
      WHEN 200.
        RETURN.
      WHEN 302.
        _raise 'HTTP redirect, check URL'.
      WHEN 401.
        _raise 'HTTP 401, unauthorized'.
      WHEN 403.
        _raise 'HTTP 403, forbidden'.
      WHEN 404.
        _raise 'HTTP 404, not found'.
      WHEN 415.
        _raise 'HTTP 415, unsupported media type'.
      WHEN OTHERS.
        _raise 'HTTP error code'.
    ENDCASE.

  ENDMETHOD.                                                "http_200

  METHOD parse_branch_list.

    DATA: lt_result TYPE TABLE OF string,
          lv_hash   TYPE ty_sha1,
          lv_name   TYPE string,
          lv_foo    TYPE string ##needed,
          lv_char   TYPE c,
          lv_data   LIKE LINE OF lt_result.

    FIELD-SYMBOLS: <ls_branch> LIKE LINE OF rt_list.


    SPLIT iv_data AT gc_newline INTO TABLE lt_result.
    LOOP AT lt_result INTO lv_data.
      IF sy-tabix = 1.
        CONTINUE. " current loop
      ELSEIF sy-tabix = 2 AND strlen( lv_data ) > 49.
        lv_hash = lv_data+8.
        lv_name = lv_data+49.
        lv_char = get_null( ).
        SPLIT lv_name AT lv_char INTO lv_name lv_foo.
      ELSEIF sy-tabix > 2 AND strlen( lv_data ) > 45.
        lv_hash = lv_data+4.
        lv_name = lv_data+45.
      ELSEIF sy-tabix = 2 AND strlen( lv_data ) = 8 AND lv_data(8) = '00000000'.
        _raise 'No branches, create branch manually by adding file'.
      ELSE.
        CONTINUE.
      ENDIF.

      IF lv_name CP 'refs/pull/*'.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO rt_list ASSIGNING <ls_branch>.
      <ls_branch>-sha1 = lv_hash.
      <ls_branch>-name = lv_name.
    ENDLOOP.

  ENDMETHOD.                    "parse_branch_list

  METHOD find_branch.

    DATA: lt_branch_list TYPE ty_branch_list_tt,
          ls_branch_list LIKE LINE OF lt_branch_list.


    branch_list(
      EXPORTING
        iv_url          = io_repo->get_url( )
        iv_service      = iv_service
      IMPORTING
        ei_client       = ei_client
        et_branch_list  = lt_branch_list ).

    IF io_repo->get_branch_name( ) IS INITIAL.
      _raise 'branch empty'.
    ENDIF.

    READ TABLE lt_branch_list INTO ls_branch_list
      WITH KEY name = io_repo->get_branch_name( ).
    IF sy-subrc <> 0.
      _raise 'Branch not found'.
    ENDIF.

    ev_branch = ls_branch_list-sha1.

  ENDMETHOD.                    "find_branch

  METHOD branches.

    DATA: li_client TYPE REF TO if_http_client.


    lcl_git_transport=>branch_list(
      EXPORTING
        iv_url         = iv_url
        iv_service     = c_service-upload
      IMPORTING
        ei_client      = li_client
        et_branch_list = rt_branch_list ).
    li_client->close( ).

  ENDMETHOD.                    "branches

  METHOD branch_list.

    DATA: lv_data TYPE string,
          lv_uri  TYPE string,
          lv_text TYPE string.


    cl_http_client=>create_by_url(
      EXPORTING
        url    = lcl_url=>host( iv_url )
        ssl_id = 'ANONYM'
      IMPORTING
        client = ei_client ).

    ei_client->request->set_cdata( '' ).
    ei_client->request->set_header_field(
        name  = '~request_method'
        value = 'GET' ).
    ei_client->request->set_header_field(
        name  = 'user-agent'
        value = gv_agent ).                                 "#EC NOTEXT
    lv_uri = lcl_url=>path_name( iv_url ) &&
             '.git/info/refs?service=git-' &&
             iv_service &&
             '-pack'.
    ei_client->request->set_header_field(
        name  = '~request_uri'
        value = lv_uri ).

    lcl_login_manager=>load( iv_uri    = iv_url
                             ii_client = ei_client ).

    ei_client->send( ).
    ei_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
* make sure:
* a) SSL is setup properly in STRUST
* b) no firewalls
* check trace file in transaction SMICM
          lv_text = 'HTTP Communication Failure'.           "#EC NOTEXT
        WHEN 2.
          lv_text = 'HTTP Invalid State'.                   "#EC NOTEXT
        WHEN 3.
          lv_text = 'HTTP Processing failed'.               "#EC NOTEXT
        WHEN OTHERS.
          lv_text = 'Another error occured'.                "#EC NOTEXT
      ENDCASE.
      RAISE EXCEPTION TYPE lcx_exception
        EXPORTING
          iv_text = lv_text.
    ENDIF.

    check_http_200( ei_client ).

    lcl_login_manager=>save( iv_uri    = iv_url
                             ii_client = ei_client ).

    lv_data = ei_client->response->get_cdata( ).
    et_branch_list = parse_branch_list( lv_data ).

  ENDMETHOD.                    "ref_discovery

  METHOD receive_pack.

    DATA: li_client   TYPE REF TO if_http_client,
          lv_cmd_pkt  TYPE string,
          lv_line     TYPE string,
          lv_tmp      TYPE xstring,
          lv_xstring  TYPE xstring,
          lv_string   TYPE string,
          lv_cap_list TYPE string,
          lv_buffer   TYPE string.


    find_branch(
      EXPORTING
        io_repo    = io_repo
        iv_service = c_service-receive
      IMPORTING
        ei_client  = li_client ).

    set_headers(
      io_repo    = io_repo
      iv_service = c_service-receive
      ii_client  = li_client ).

    lv_cap_list = 'report-status agent=' && gv_agent ##NO_TEXT.

    lv_line = iv_old &&
              ` ` &&
              iv_new &&
              ` ` &&
              iv_branch_name &&
              get_null( ) &&
              ` ` &&
              lv_cap_list &&
              gc_newline.                                   "#EC NOTEXT
    lv_cmd_pkt = pkt_string( lv_line ).

    lv_buffer = lv_cmd_pkt && '0000'.
    lv_tmp = lcl_convert=>string_to_xstring_utf8( lv_buffer ).

    CONCATENATE lv_tmp iv_pack INTO lv_xstring IN BYTE MODE.

    li_client->request->set_data( lv_xstring ).

    li_client->send( ).
    li_client->receive( ).
    check_http_200( li_client ).

    lv_xstring = li_client->response->get_data( ).
    li_client->close( ).

    lv_string = lcl_convert=>xstring_to_string_utf8( lv_xstring ).
    IF NOT lv_string CP '*unpack ok*'.
      _raise 'unpack not ok'.
    ELSEIF lv_string CP '*pre-receive hook declined*'.
      _raise 'pre-receive hook declined'.
    ENDIF.

  ENDMETHOD.                    "receive_pack

  METHOD length_utf8_hex.

    DATA: lv_xstring TYPE xstring,
          lv_string  TYPE string,
          lv_char4   TYPE c LENGTH 4,
          lv_x       TYPE x LENGTH 2,
          lo_obj     TYPE REF TO cl_abap_conv_in_ce,
          lv_len     TYPE int4.

* hmm, can this be done easier?

    lv_xstring = iv_data(4).

    lo_obj = cl_abap_conv_in_ce=>create(
        input    = lv_xstring
        encoding = 'UTF-8' ).
    lv_len = xstrlen( lv_xstring ).

    TRY.
        lo_obj->read( EXPORTING n    = lv_len
                      IMPORTING data = lv_string ).
      CATCH cx_sy_conversion_codepage.
        _raise 'error converting to hex, LENGTH_UTF8_HEX'.
    ENDTRY.

    lv_char4 = lv_string.
    TRANSLATE lv_char4 TO UPPER CASE.
    lv_x = lv_char4.
    rv_len = lv_x.

  ENDMETHOD.                    "length_utf8_hex

  METHOD parse.

    CONSTANTS: lc_band1 TYPE x VALUE '01'.

    DATA: lv_len      TYPE i,
          lv_contents TYPE xstring,
          lv_pack     TYPE xstring.


    WHILE xstrlen( cv_data ) >= 4.
      lv_len = length_utf8_hex( cv_data ).

      IF lv_len > xstrlen( cv_data ).
        _raise 'parse, string length too large'.
      ENDIF.

      lv_contents = cv_data(lv_len).
      IF lv_len = 0.
        cv_data = cv_data+4.
        CONTINUE.
      ELSE.
        cv_data = cv_data+lv_len.
      ENDIF.

      lv_contents = lv_contents+4.

      IF xstrlen( lv_contents ) > 1 AND lv_contents(1) = lc_band1.
        CONCATENATE lv_pack lv_contents+1 INTO lv_pack IN BYTE MODE.
      ENDIF.

    ENDWHILE.

    ev_pack = lv_pack.

  ENDMETHOD.                    "parse

  METHOD upload_pack.

    DATA: li_client   TYPE REF TO if_http_client,
          lv_buffer   TYPE string,
          lv_xstring  TYPE xstring,
          lv_line     TYPE string,
          lv_pack     TYPE xstring,
          lt_branches TYPE ty_branch_list_tt,
          lv_capa     TYPE string,
          lv_pkt1     TYPE string.

    FIELD-SYMBOLS: <ls_branch> LIKE LINE OF lt_branches.


    find_branch(
      EXPORTING
        io_repo    = io_repo
        iv_service = c_service-upload
      IMPORTING
        ei_client  = li_client
        ev_branch  = ev_branch ).

    IF it_branches IS INITIAL.
      APPEND INITIAL LINE TO lt_branches ASSIGNING <ls_branch>.
      <ls_branch>-sha1 = ev_branch.
    ELSE.
      lt_branches = it_branches.
    ENDIF.

    set_headers( io_repo    = io_repo
                 iv_service = c_service-upload
                 ii_client  = li_client ).

    LOOP AT lt_branches FROM 1 ASSIGNING <ls_branch>.
      IF sy-tabix = 1.
        lv_capa = 'side-band-64k no-progress agent=' && gv_agent.
        lv_line = 'want' && ` ` && <ls_branch>-sha1
          && ` ` && lv_capa && gc_newline.                  "#EC NOTEXT
      ELSE.
        lv_line = 'want' && ` ` && <ls_branch>-sha1
          && gc_newline.                                    "#EC NOTEXT
      ENDIF.
      lv_buffer = lv_buffer && pkt_string( lv_line ).
    ENDLOOP.

    IF iv_deepen = abap_true.
      lv_buffer = lv_buffer && pkt_string( 'deepen 1' && gc_newline ). "#EC NOTEXT
    ENDIF.

    lv_buffer = lv_buffer
             && '0000'
             && '0009done' && gc_newline.

* do not use set_cdata as it modifies the Content-Type header field
    li_client->request->set_data( lcl_convert=>string_to_xstring_utf8( lv_buffer ) ).
    li_client->send( ).
    li_client->receive( ).
    check_http_200( li_client ).
    lv_xstring = li_client->response->get_data( ).
    li_client->close( ).

    parse( IMPORTING ev_pack = lv_pack
           CHANGING cv_data = lv_xstring ).

    IF lv_pack IS INITIAL.
      _raise 'empty pack'.
    ENDIF.

    et_objects = lcl_git_pack=>decode( lv_pack ).

  ENDMETHOD.                    "upload_pack

  METHOD pkt_string.

    DATA: lv_x   TYPE x,
          lv_len TYPE i.


    lv_len = strlen( iv_string ).

    IF lv_len >= 255.
      _raise 'PKT, todo'.
    ENDIF.

    lv_x = lv_len + 4.

    rv_pkt = rv_pkt && '00' && lv_x && iv_string.

  ENDMETHOD.                    "pkt

ENDCLASS.                    "lcl_transport IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_pack IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_git_pack IMPLEMENTATION.

  METHOD sort_tree.

    TYPES: BEGIN OF ty_sort,
             sort TYPE string,
             node TYPE ty_node,
           END OF ty_sort.

    DATA: lt_sort TYPE STANDARD TABLE OF ty_sort WITH DEFAULT KEY.

    FIELD-SYMBOLS: <ls_sort> LIKE LINE OF lt_sort,
                   <ls_node> LIKE LINE OF it_nodes.


    LOOP AT it_nodes ASSIGNING <ls_node>.
      APPEND INITIAL LINE TO lt_sort ASSIGNING <ls_sort>.
      IF <ls_node>-chmod = gc_chmod-dir.
        CONCATENATE <ls_node>-name '/' INTO <ls_sort>-sort.
      ELSE.
        <ls_sort>-sort = <ls_node>-name.
      ENDIF.
      <ls_sort>-node = <ls_node>.
    ENDLOOP.

* following has to be done, or unpack will fail on server side
    SORT lt_sort BY sort ASCENDING.

    LOOP AT lt_sort ASSIGNING <ls_sort>.
      APPEND <ls_sort>-node TO rt_nodes.
    ENDLOOP.

  ENDMETHOD.

  METHOD type_and_length.

    DATA: lv_bits   TYPE string,
          lv_type   TYPE string,
          lv_result TYPE string,
          lv_c      TYPE c,
          lv_offset TYPE i,
          lv_x4     TYPE x LENGTH 4,
          lv_x      TYPE x LENGTH 1.


    CASE is_object-type.
      WHEN gc_type-commit.
        lv_type = '001'.
      WHEN gc_type-tree.
        lv_type = '010'.
      WHEN gc_type-blob.
        lv_type = '011'.
      WHEN gc_type-ref_d.
        lv_type = '111'.
      WHEN OTHERS.
        _raise 'Unexpected object type while encoding pack'.
    ENDCASE.

    lv_x4 = xstrlen( is_object-data ).
    DO 32 TIMES.
      GET BIT sy-index OF lv_x4 INTO lv_c.
      CONCATENATE lv_bits lv_c INTO lv_bits.
    ENDDO.

    IF lv_bits(28) = '0000000000000000000000000000'.
      CONCATENATE '0' lv_type lv_bits+28(4) INTO lv_result.
    ELSEIF lv_bits(21) = '000000000000000000000'.
      CONCATENATE '1' lv_type lv_bits+28(4) INTO lv_result.
      CONCATENATE lv_result '0' lv_bits+21(7) INTO lv_result.
    ELSEIF lv_bits(14) = '00000000000000'.
      CONCATENATE '1' lv_type lv_bits+28(4) INTO lv_result.
      CONCATENATE lv_result '1' lv_bits+21(7) INTO lv_result.
      CONCATENATE lv_result '0' lv_bits+14(7) INTO lv_result.
    ELSEIF lv_bits(7) = '0000000'.
      CONCATENATE '1' lv_type lv_bits+28(4) INTO lv_result.
      CONCATENATE lv_result '1' lv_bits+21(7) INTO lv_result.
      CONCATENATE lv_result '1' lv_bits+14(7) INTO lv_result.
      CONCATENATE lv_result '0' lv_bits+7(7) INTO lv_result.
    ELSE.
* this IF can be refactored, use shifting?
      _raise 'Todo, encoding length'.
    ENDIF.

* convert bit string to xstring
    CLEAR lv_x.
    DO strlen( lv_result ) TIMES.
      lv_offset = sy-index - 1.
      IF lv_result+lv_offset(1) = '1'.
        SET BIT ( lv_offset MOD 8 ) + 1 OF lv_x.
      ENDIF.
      IF ( lv_offset + 1 ) MOD 8 = 0.
        CONCATENATE rv_xstring lv_x INTO rv_xstring IN BYTE MODE.
        CLEAR lv_x.
      ENDIF.
    ENDDO.

  ENDMETHOD.                    "type_and_length

  METHOD get_length.

    DATA: lv_x           TYPE x,
          lv_length_bits TYPE string,
          lv_bitbyte     TYPE ty_bitbyte.


    lv_x = cv_data(1).
    lv_bitbyte = lcl_convert=>x_to_bitbyte( lv_x ).

    cv_data = cv_data+1.
    lv_length_bits = lv_bitbyte+4.

    WHILE lv_bitbyte(1) <> '0'.
      lv_x = cv_data(1).
      lv_bitbyte = lcl_convert=>x_to_bitbyte( lv_x ).
      cv_data = cv_data+1.
      CONCATENATE lv_bitbyte+1 lv_length_bits INTO lv_length_bits.
    ENDWHILE.

    ev_length = lcl_convert=>bitbyte_to_int( lv_length_bits ).

  ENDMETHOD.                    "get_length

  METHOD encode_tree.

    CONSTANTS: lc_null TYPE x VALUE '00'.

    DATA: lv_string  TYPE string,
          lt_nodes   LIKE it_nodes,
          lv_hex20   TYPE x LENGTH 20,
          lv_xstring TYPE xstring.

    FIELD-SYMBOLS: <ls_node> LIKE LINE OF it_nodes.


    lt_nodes = sort_tree( it_nodes ).

    LOOP AT lt_nodes ASSIGNING <ls_node>.
      ASSERT NOT <ls_node>-chmod IS INITIAL.
      ASSERT NOT <ls_node>-name IS INITIAL.
      ASSERT NOT <ls_node>-sha1 IS INITIAL.

      CONCATENATE <ls_node>-chmod <ls_node>-name INTO lv_string SEPARATED BY space.
      lv_xstring = lcl_convert=>string_to_xstring_utf8( lv_string ).

      lv_hex20 = to_upper( <ls_node>-sha1 ).
      CONCATENATE rv_data lv_xstring lc_null lv_hex20 INTO rv_data IN BYTE MODE.
    ENDLOOP.

  ENDMETHOD.                    "encode_tree

  METHOD encode_commit.

    DATA: lv_string       TYPE string,
          lv_tmp          TYPE string,
          lv_tree_lower   TYPE string,
          lv_parent_lower TYPE string.


    lv_tree_lower = is_commit-tree.
    TRANSLATE lv_tree_lower TO LOWER CASE.

    lv_parent_lower = is_commit-parent.
    TRANSLATE lv_parent_lower TO LOWER CASE.

    lv_string = ''.

    CONCATENATE 'tree' lv_tree_lower INTO lv_tmp SEPARATED BY space. "#EC NOTEXT
    CONCATENATE lv_string lv_tmp gc_newline INTO lv_string.

    IF NOT is_commit-parent IS INITIAL.
      CONCATENATE 'parent' lv_parent_lower
        INTO lv_tmp SEPARATED BY space.                     "#EC NOTEXT
      CONCATENATE lv_string lv_tmp gc_newline INTO lv_string.
    ENDIF.

    CONCATENATE 'author' is_commit-author
      INTO lv_tmp SEPARATED BY space.                       "#EC NOTEXT
    CONCATENATE lv_string lv_tmp gc_newline INTO lv_string.

    CONCATENATE 'committer' is_commit-committer
      INTO lv_tmp SEPARATED BY space.                       "#EC NOTEXT
    CONCATENATE lv_string lv_tmp gc_newline INTO lv_string.

    CONCATENATE lv_string gc_newline is_commit-body INTO lv_string.

    rv_data = lcl_convert=>string_to_xstring_utf8( lv_string ).

  ENDMETHOD.                    "encode_commit

  METHOD get_type.

    DATA: lv_char3   TYPE c LENGTH 3,
          lv_bitbyte TYPE ty_bitbyte.


    lv_bitbyte = lcl_convert=>x_to_bitbyte( iv_x ).
    lv_char3 = lv_bitbyte+1.

    CASE lv_char3.
      WHEN '001'.
        rv_type = gc_type-commit.
      WHEN '010'.
        rv_type = gc_type-tree.
      WHEN '011'.
        rv_type = gc_type-blob.
      WHEN '111'.
        rv_type = gc_type-ref_d.
      WHEN OTHERS.
        _raise 'Todo, unknown type'.
    ENDCASE.

  ENDMETHOD.                    "get_type

  METHOD decode_commit.

    DATA: lv_string TYPE string,
          lv_len    TYPE i,
          lv_word   TYPE string,
          lv_trash  TYPE string,
          lt_string TYPE TABLE OF string.

    FIELD-SYMBOLS: <lv_string> LIKE LINE OF lt_string.


    lv_string = lcl_convert=>xstring_to_string_utf8( iv_data ).

    SPLIT lv_string AT gc_newline INTO TABLE lt_string.

    LOOP AT lt_string ASSIGNING <lv_string>.
      IF NOT rs_commit-committer IS INITIAL.
        CONCATENATE rs_commit-body <lv_string> INTO rs_commit-body
          SEPARATED BY gc_newline.
      ELSE.
        SPLIT <lv_string> AT space INTO lv_word lv_trash.
        CASE lv_word.
          WHEN 'tree'.
            rs_commit-tree = <lv_string>+5.
          WHEN 'parent'.
            IF rs_commit-parent IS INITIAL.
              rs_commit-parent = <lv_string>+7.
            ELSE.
              rs_commit-parent2 = <lv_string>+7.
            ENDIF.
          WHEN 'author'.
            rs_commit-author = <lv_string>+7.
          WHEN 'committer'.
            rs_commit-committer = <lv_string>+10.
          WHEN OTHERS.
            ASSERT 0 = 1.
        ENDCASE.
      ENDIF.
    ENDLOOP.

* strip first newline
    IF strlen( rs_commit-body ) >= 2.
      rs_commit-body = rs_commit-body+2.
    ENDIF.

    IF rs_commit-author IS INITIAL
        OR rs_commit-committer IS INITIAL
        OR rs_commit-tree IS INITIAL.
      _raise 'multiple parents? not supported'.
    ENDIF.

  ENDMETHOD.                    "decode_commit

  METHOD delta_header.

    DATA: lv_bitbyte TYPE ty_bitbyte,
          lv_bits    TYPE string,
          lv_x       TYPE x.


    lv_bits = ''.
    DO.
      lv_x = cv_delta(1).
      cv_delta = cv_delta+1.
      lv_bitbyte = lcl_convert=>x_to_bitbyte( lv_x ).
      CONCATENATE lv_bitbyte+1 lv_bits INTO lv_bits.
      IF lv_bitbyte(1) = '0'.
        EXIT. " current loop
      ENDIF.
    ENDDO.
    ev_header = lcl_convert=>bitbyte_to_int( lv_bits ).

  ENDMETHOD.                    "delta_header

  METHOD delta.

    DATA: lv_delta   TYPE xstring,
          lv_base    TYPE xstring,
          lv_result  TYPE xstring,
          lv_bitbyte TYPE ty_bitbyte,
          lv_offset  TYPE i,
          lv_message TYPE string,
          lv_sha1    TYPE ty_sha1,
          ls_object  LIKE LINE OF ct_objects,
          lv_len     TYPE i,
          lv_x       TYPE x.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF ct_objects.


    lv_delta = is_object-data.

* find base
    READ TABLE ct_objects ASSIGNING <ls_object> WITH KEY sha1 = is_object-sha1.
    IF sy-subrc <> 0.
      CONCATENATE 'Base not found,' is_object-sha1 INTO lv_message
        SEPARATED BY space.                                 "#EC NOTEXT
      _raise lv_message.
    ELSE.
      lv_base = <ls_object>-data.
    ENDIF.

* sanity check
    IF <ls_object>-type = gc_type-ref_d.
      _raise 'Delta, base eq delta'.
    ENDIF.

* skip the 2 headers
    delta_header( CHANGING cv_delta = lv_delta ).
    delta_header( CHANGING cv_delta = lv_delta ).

    WHILE xstrlen( lv_delta ) > 0.

      lv_x = lv_delta(1).
      lv_delta = lv_delta+1.
      lv_bitbyte = lcl_convert=>x_to_bitbyte( lv_x ).

      IF lv_bitbyte(1) = '1'. " MSB

        lv_offset = 0.
        IF lv_bitbyte+7(1) = '1'.
          lv_x = lv_delta(1).
          lv_delta = lv_delta+1.
          lv_offset = lv_x.
        ENDIF.
        IF lv_bitbyte+6(1) = '1'.
          lv_x = lv_delta(1).
          lv_delta = lv_delta+1.
          lv_offset = lv_offset + lv_x * 256.
        ENDIF.
        IF lv_bitbyte+5(1) = '1'.
          lv_x = lv_delta(1).
          lv_delta = lv_delta+1.
          lv_offset = lv_offset + lv_x * 65536.
        ENDIF.
        IF lv_bitbyte+4(1) = '1'.
          lv_x = lv_delta(1).
          lv_delta = lv_delta+1.
          lv_offset = lv_offset + lv_x * 16777216. " hmm, overflow?
        ENDIF.

        lv_len = 0.
        IF lv_bitbyte+3(1) = '1'.
          lv_x = lv_delta(1).
          lv_delta = lv_delta+1.
          lv_len = lv_x.
        ENDIF.
        IF lv_bitbyte+2(1) = '1'.
          lv_x = lv_delta(1).
          lv_delta = lv_delta+1.
          lv_len = lv_len + lv_x * 256.
        ENDIF.
        IF lv_bitbyte+1(1) = '1'.
          lv_x = lv_delta(1).
          lv_delta = lv_delta+1.
          lv_len = lv_len + lv_x * 65536.
        ENDIF.

        IF lv_len = 0.
          lv_len = 65536.
        ENDIF.

        CONCATENATE lv_result lv_base+lv_offset(lv_len)
          INTO lv_result IN BYTE MODE.
      ELSE. " lv_bitbyte(1) = '0'
* insert from delta
        lv_len = lv_x.
        CONCATENATE lv_result lv_delta(lv_len) INTO lv_result IN BYTE MODE.
        lv_delta = lv_delta+lv_len.
      ENDIF.

    ENDWHILE.

    lv_sha1 = lcl_hash=>sha1( iv_type = <ls_object>-type iv_data = lv_result ).

    CLEAR ls_object.
    ls_object-sha1 = lv_sha1.
    ls_object-type = <ls_object>-type.
    ls_object-data = lv_result.
    APPEND ls_object TO ct_objects.

  ENDMETHOD.                    "delta

  METHOD decode_deltas.

    DATA: ls_object LIKE LINE OF ct_objects,
          lt_deltas LIKE ct_objects.


    LOOP AT ct_objects INTO ls_object WHERE type = gc_type-ref_d.
      DELETE ct_objects INDEX sy-tabix.
      APPEND ls_object TO lt_deltas.
    ENDLOOP.

    LOOP AT lt_deltas INTO ls_object.
      delta( EXPORTING is_object = ls_object
             CHANGING ct_objects = ct_objects ).
    ENDLOOP.

  ENDMETHOD.                    "decode_deltas

  METHOD decode_tree.

    CONSTANTS: lc_sha_length TYPE i VALUE 20,
               lc_null       TYPE x VALUE '00'.

    DATA: lv_xstring TYPE xstring,
          lv_chmod   TYPE ty_chmod,
          lv_name    TYPE string,
          lv_string  TYPE string,
          lv_len     TYPE i,
          lv_offset  TYPE i,
          lv_cursor  TYPE i,
          ls_node    TYPE ty_node,
          lv_start   TYPE i.


    DO.
      IF lv_cursor >= xstrlen( iv_data ).
        EXIT. " current loop
      ENDIF.

      IF iv_data+lv_cursor(1) = lc_null.
        lv_len = lv_cursor - lv_start.
        lv_xstring = iv_data+lv_start(lv_len).

        lv_string = lcl_convert=>xstring_to_string_utf8( lv_xstring ).
        SPLIT lv_string AT space INTO lv_chmod lv_name.

        lv_offset = lv_cursor + 1.

        CLEAR ls_node.
        ls_node-chmod = lv_chmod.
        IF ls_node-chmod <> gc_chmod-dir
            AND ls_node-chmod <> gc_chmod-file
            AND ls_node-chmod <> gc_chmod-executable.
          _raise 'Unknown chmod'.
        ENDIF.

        ls_node-name = lv_name.
        ls_node-sha1 = iv_data+lv_offset(lc_sha_length).
        TRANSLATE ls_node-sha1 TO LOWER CASE.
        APPEND ls_node TO rt_nodes.

        lv_start = lv_cursor + 1 + lc_sha_length.
        lv_cursor = lv_start.
      ELSE.
        lv_cursor = lv_cursor + 1.
      ENDIF.
    ENDDO.

  ENDMETHOD.                    "decode_tree

  METHOD decode.

    DATA: lv_x              TYPE x,
          lv_data           TYPE xstring,
          lv_type           TYPE c LENGTH 6,
          lv_zlib           TYPE x LENGTH 2,
          lv_objects        TYPE i,
          lv_len            TYPE i,
          lv_sha1           TYPE ty_sha1,
          lv_ref_delta      TYPE ty_sha1,
          lv_adler32        TYPE lcl_hash=>ty_adler32,
          lv_compressed     TYPE xstring,
          lv_compressed_len TYPE i,
          lv_decompress_len TYPE i,
          lv_decompressed   TYPE xstring,
          lv_xstring        TYPE xstring,
          lv_expected       TYPE i,
          ls_object         LIKE LINE OF rt_objects,
          ls_data           TYPE lcl_zlib=>ty_decompress.


    lv_data = iv_data.

* header
    IF NOT xstrlen( lv_data ) > 4 OR lv_data(4) <> c_pack_start.
      _raise 'Unexpected pack header'.
    ENDIF.
    lv_data = lv_data+4.

* version
    IF lv_data(4) <> c_version.
      _raise 'Version not supported'.
    ENDIF.
    lv_data = lv_data+4.

* number of objects
    lv_xstring = lv_data(4).
    lv_objects = lcl_convert=>xstring_to_int( lv_xstring ).
    lv_data = lv_data+4.


    DO lv_objects TIMES.

      lv_x = lv_data(1).
      lv_type = get_type( lv_x ).

      get_length( IMPORTING ev_length = lv_expected
                  CHANGING cv_data = lv_data ).

      IF lv_type = gc_type-ref_d.
        lv_ref_delta = lv_data(20).
        lv_data = lv_data+20.
      ENDIF.

* strip header, '789C', CMF + FLG
      lv_zlib = lv_data(2).
      IF lv_zlib <> c_zlib AND lv_zlib <> c_zlib_hmm.
        _raise 'Unexpected zlib header'.
      ENDIF.
      lv_data = lv_data+2.

*******************************

      IF lv_zlib = c_zlib.
        cl_abap_gzip=>decompress_binary(
          EXPORTING
            gzip_in     = lv_data
          IMPORTING
            raw_out     = lv_decompressed
            raw_out_len = lv_decompress_len ).

        IF lv_expected <> lv_decompress_len.
          _raise 'Decompression falied'.
        ENDIF.

        cl_abap_gzip=>compress_binary(
          EXPORTING
            raw_in         = lv_decompressed
          IMPORTING
            gzip_out       = lv_compressed
            gzip_out_len   = lv_compressed_len ).

        IF lv_compressed(lv_compressed_len) <> lv_data(lv_compressed_len).
          _raise 'Compressed data doesnt match'.
        ENDIF.

        lv_data = lv_data+lv_compressed_len.

      ELSEIF lv_zlib = c_zlib_hmm.
* cl_abap_gzip copmression works for header '789C', but does not work for
* '7801', call custom implementation of DEFLATE algorithm.
* The custom implementation could handle both, but most likely the kernel
* implementation runs faster than the custom ABAP.
        ls_data = lcl_zlib=>decompress( lv_data ).
        lv_compressed_len = ls_data-compressed_len.
        lv_decompressed = ls_data-raw.

        IF lv_compressed_len IS INITIAL.
          _raise 'Decompression falied :o/'.
        ENDIF.

        lv_data = lv_data+lv_compressed_len.

        lv_adler32 = lcl_hash=>adler32( lv_decompressed ).
        IF lv_data(4) <> lv_adler32.
          lv_data = lv_data+1.
        ENDIF.
        IF lv_data(4) <> lv_adler32.
          lv_data = lv_data+1.
        ENDIF.
        IF lv_data(4) <> lv_adler32.
          _raise 'Wrong Adler checksum'.
        ENDIF.

      ENDIF.

      lv_data = lv_data+4. " skip adler checksum

*************************

      CLEAR ls_object.
      IF lv_type = gc_type-ref_d.
        ls_object-sha1 = lv_ref_delta.
        TRANSLATE ls_object-sha1 TO LOWER CASE.
      ELSE.
        ls_object-sha1 = lcl_hash=>sha1(
          iv_type = lv_type
          iv_data = lv_decompressed ).
      ENDIF.
      ls_object-type = lv_type.
      ls_object-data = lv_decompressed.
      APPEND ls_object TO rt_objects.
    ENDDO.

* check SHA1 at end of pack
    lv_len = xstrlen( iv_data ) - 20.
    lv_xstring = iv_data(lv_len).
    lv_sha1 = lcl_hash=>sha1_raw( lv_xstring ).
    IF to_upper( lv_sha1 ) <> lv_data.
      _raise 'SHA1 at end of pack doesnt match'.
    ENDIF.

    decode_deltas( CHANGING ct_objects = rt_objects ).

  ENDMETHOD.                    "decode

  METHOD encode.

    DATA: lv_sha1       TYPE x LENGTH 20,
          lv_adler32    TYPE lcl_hash=>ty_adler32,
          lv_len        TYPE i,
          lv_compressed TYPE xstring,
          lv_xstring    TYPE xstring.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF it_objects.


    rv_data = c_pack_start.

    CONCATENATE rv_data c_version INTO rv_data IN BYTE MODE.

    lv_len = lines( it_objects ).
    lv_xstring = lcl_convert=>int_to_xstring( iv_i      = lv_len
                                              iv_length = 4 ).
    CONCATENATE rv_data lv_xstring INTO rv_data IN BYTE MODE.

    LOOP AT it_objects ASSIGNING <ls_object>.
      lv_xstring = type_and_length( <ls_object> ).
      CONCATENATE rv_data lv_xstring INTO rv_data IN BYTE MODE.

      cl_abap_gzip=>compress_binary(
        EXPORTING
          raw_in   = <ls_object>-data
        IMPORTING
          gzip_out = lv_compressed ).

      CONCATENATE rv_data c_zlib lv_compressed INTO rv_data IN BYTE MODE.

      lv_adler32 = lcl_hash=>adler32( <ls_object>-data ).
      CONCATENATE rv_data lv_adler32 INTO rv_data IN BYTE MODE.

    ENDLOOP.

    lv_sha1 = to_upper( lcl_hash=>sha1_raw( rv_data ) ).
    CONCATENATE rv_data lv_sha1 INTO rv_data IN BYTE MODE.

  ENDMETHOD.                    "encode

ENDCLASS.                    "lcl_pack IMPLEMENTATION

CLASS ltcl_git_porcelain DEFINITION DEFERRED.

*----------------------------------------------------------------------*
*       CLASS lcl_porcelain DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_git_porcelain DEFINITION FINAL FRIENDS ltcl_git_porcelain.

  PUBLIC SECTION.

    CLASS-METHODS pull
      IMPORTING io_repo    TYPE REF TO lcl_repo_online
      EXPORTING et_files   TYPE ty_files_tt
                et_objects TYPE ty_objects_tt
                ev_branch  TYPE ty_sha1
      RAISING   lcx_exception.

    CLASS-METHODS push
      IMPORTING io_repo          TYPE REF TO lcl_repo_online
                is_comment       TYPE ty_comment
                io_stage         TYPE REF TO lcl_stage
      RETURNING VALUE(rv_branch) TYPE ty_sha1
      RAISING   lcx_exception.

    CLASS-METHODS create_branch
      IMPORTING io_repo TYPE REF TO lcl_repo_online
                iv_name TYPE string
                iv_from TYPE ty_sha1
      RAISING   lcx_exception.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_expanded,
             path  TYPE string,
             name  TYPE string,
             sha1  TYPE ty_sha1,
             chmod TYPE ty_chmod,
           END OF ty_expanded.

    TYPES: ty_expanded_tt TYPE STANDARD TABLE OF ty_expanded WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_tree,
             path TYPE string,
             data TYPE xstring,
             sha1 TYPE ty_sha1,
           END OF ty_tree.

    TYPES: ty_trees_tt TYPE STANDARD TABLE OF ty_tree WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_folder,
             path  TYPE string,
             count TYPE i,
             sha1  TYPE ty_sha1,
           END OF ty_folder.

    TYPES: ty_folders_tt TYPE STANDARD TABLE OF ty_folder WITH DEFAULT KEY.

    CLASS-METHODS build_trees
      IMPORTING it_expanded     TYPE ty_expanded_tt
      RETURNING VALUE(rt_trees) TYPE ty_trees_tt
      RAISING   lcx_exception.

    CLASS-METHODS find_folders
      IMPORTING it_expanded       TYPE ty_expanded_tt
      RETURNING VALUE(rt_folders) TYPE ty_folders_tt.

    CLASS-METHODS walk
      IMPORTING it_objects TYPE ty_objects_tt
                iv_sha1    TYPE ty_sha1
                iv_path    TYPE string
      CHANGING  ct_files   TYPE ty_files_tt
      RAISING   lcx_exception.

    CLASS-METHODS walk_tree
      IMPORTING it_objects         TYPE ty_objects_tt
                iv_tree            TYPE ty_sha1
                iv_base            TYPE string
      RETURNING VALUE(rt_expanded) TYPE ty_expanded_tt
      RAISING   lcx_exception.

    CLASS-METHODS full_tree
      IMPORTING it_objects         TYPE ty_objects_tt
                iv_branch          TYPE ty_sha1
      RETURNING VALUE(rt_expanded) TYPE ty_expanded_tt
      RAISING   lcx_exception.

    CLASS-METHODS receive_pack
      IMPORTING is_comment       TYPE ty_comment
                io_repo          TYPE REF TO lcl_repo_online
                it_trees         TYPE ty_trees_tt
                it_blobs         TYPE ty_files_tt
                iv_branch        TYPE ty_sha1
      RETURNING VALUE(rv_branch) TYPE ty_sha1
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_porcelain DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_porcelain IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_git_porcelain IMPLEMENTATION.

  METHOD receive_pack.

    DATA: lv_time    TYPE lcl_time=>ty_unixtime,
          lv_commit  TYPE xstring,
          lt_objects TYPE ty_objects_tt,
          lv_pack    TYPE xstring,
          ls_object  LIKE LINE OF lt_objects,
          ls_commit  TYPE lcl_git_pack=>ty_commit.

    FIELD-SYMBOLS: <ls_tree> LIKE LINE OF it_trees,
                   <ls_blob> LIKE LINE OF it_blobs.


    lv_time = lcl_time=>get( ).

    READ TABLE it_trees ASSIGNING <ls_tree> WITH KEY path = '/'.
    ASSERT sy-subrc = 0.

* new commit
    ls_commit-tree      = <ls_tree>-sha1.
    ls_commit-parent    = iv_branch.
    CONCATENATE is_comment-username space '<' is_comment-email '>' space lv_time
      INTO ls_commit-author RESPECTING BLANKS.
    ls_commit-committer = ls_commit-author.
    ls_commit-body      = is_comment-comment.
    lv_commit = lcl_git_pack=>encode_commit( ls_commit ).

    CLEAR ls_object.
    ls_object-sha1 = lcl_hash=>sha1( iv_type = gc_type-commit iv_data = lv_commit ).
    ls_object-type = gc_type-commit.
    ls_object-data = lv_commit.
    APPEND ls_object TO lt_objects.

    LOOP AT it_trees ASSIGNING <ls_tree>.
      CLEAR ls_object.
      ls_object-sha1 = <ls_tree>-sha1.
      ls_object-type = gc_type-tree.
      ls_object-data = <ls_tree>-data.
      APPEND ls_object TO lt_objects.
    ENDLOOP.

    LOOP AT it_blobs ASSIGNING <ls_blob>.
      CLEAR ls_object.
      ls_object-sha1 = lcl_hash=>sha1( iv_type = gc_type-blob iv_data = <ls_blob>-data ).
      ls_object-type = gc_type-blob.
      ls_object-data = <ls_blob>-data.
      APPEND ls_object TO lt_objects.
    ENDLOOP.

    lv_pack = lcl_git_pack=>encode( lt_objects ).

    rv_branch = lcl_hash=>sha1(
      iv_type = gc_type-commit
      iv_data = lv_commit ).

    lcl_git_transport=>receive_pack(
      io_repo        = io_repo
      iv_old         = io_repo->get_sha1_local( )
      iv_new         = rv_branch
      iv_branch_name = io_repo->get_branch_name( )
      iv_pack        = lv_pack ).

  ENDMETHOD.                    "receive_pack

  METHOD create_branch.

    DATA: lv_zero    TYPE ty_sha1,
          lt_objects TYPE ty_objects_tt,
          lv_pack    TYPE xstring.


    lv_zero = '0000000000000000000000000000000000000000'.

* "client MUST send an empty packfile"
* https://github.com/git/git/blob/master/Documentation/technical/pack-protocol.txt#L514
    lv_pack = lcl_git_pack=>encode( lt_objects ).

    lcl_git_transport=>receive_pack(
      io_repo        = io_repo
      iv_old         = lv_zero
      iv_new         = iv_from
      iv_branch_name = iv_name
      iv_pack        = lv_pack ).

  ENDMETHOD.

  METHOD push.

    DATA: lt_expanded TYPE ty_expanded_tt,
          lt_blobs    TYPE ty_files_tt,
          lv_sha1     TYPE ty_sha1,
          lt_trees    TYPE ty_trees_tt,
          lt_stage    TYPE lcl_stage=>ty_stage_tt.

    FIELD-SYMBOLS: <ls_stage> LIKE LINE OF lt_stage,
                   <ls_exp>   LIKE LINE OF lt_expanded.


    lt_expanded = full_tree( it_objects = io_repo->get_objects( )
                             iv_branch  = io_repo->get_sha1_remote( ) ).

    lt_stage = io_stage->get_all( ).
    LOOP AT lt_stage ASSIGNING <ls_stage>.
      CASE <ls_stage>-method.
        WHEN lcl_stage=>c_method-add.
          APPEND <ls_stage>-file TO lt_blobs.

          READ TABLE lt_expanded ASSIGNING <ls_exp> WITH KEY
            name = <ls_stage>-file-filename
            path = <ls_stage>-file-path.
          IF sy-subrc <> 0. " new files
            APPEND INITIAL LINE TO lt_expanded ASSIGNING <ls_exp>.
            <ls_exp>-name  = <ls_stage>-file-filename.
            <ls_exp>-path  = <ls_stage>-file-path.
            <ls_exp>-chmod = gc_chmod-file.
          ENDIF.

          lv_sha1 = lcl_hash=>sha1( iv_type = gc_type-blob iv_data = <ls_stage>-file-data ).
          IF <ls_exp>-sha1 <> lv_sha1.
            <ls_exp>-sha1 = lv_sha1.
          ENDIF.
        WHEN lcl_stage=>c_method-rm.
          DELETE lt_expanded
            WHERE name = <ls_stage>-file-filename
            AND path = <ls_stage>-file-path.
          ASSERT sy-subrc = 0.
        WHEN OTHERS.
          _raise 'stage method not supported, todo'.
      ENDCASE.
    ENDLOOP.

    lt_trees = build_trees( lt_expanded ).

    rv_branch = receive_pack( is_comment = is_comment
                              io_repo    = io_repo
                              it_trees   = lt_trees
                              it_blobs   = lt_blobs
                              iv_branch  = io_repo->get_sha1_remote( ) ).

  ENDMETHOD.                    "push

  METHOD walk_tree.

    DATA: ls_object   LIKE LINE OF it_objects,
          lt_expanded LIKE rt_expanded,
          lt_nodes    TYPE lcl_git_pack=>ty_nodes_tt.

    FIELD-SYMBOLS: <ls_exp>  LIKE LINE OF rt_expanded,
                   <ls_node> LIKE LINE OF lt_nodes.


    READ TABLE it_objects INTO ls_object
      WITH KEY sha1 = iv_tree
      type = gc_type-tree.
    IF sy-subrc <> 0.
      _raise 'tree not found'.
    ENDIF.
    lt_nodes = lcl_git_pack=>decode_tree( ls_object-data ).

    LOOP AT lt_nodes ASSIGNING <ls_node>.
      CASE <ls_node>-chmod.
        WHEN gc_chmod-file
            OR gc_chmod-executable.
          APPEND INITIAL LINE TO rt_expanded ASSIGNING <ls_exp>.
          <ls_exp>-path  = iv_base.
          <ls_exp>-name  = <ls_node>-name.
          <ls_exp>-sha1  = <ls_node>-sha1.
          <ls_exp>-chmod = <ls_node>-chmod.
        WHEN gc_chmod-dir.
          lt_expanded = walk_tree(
            it_objects = it_objects
            iv_tree    = <ls_node>-sha1
            iv_base    = iv_base && <ls_node>-name && '/' ).
          APPEND LINES OF lt_expanded TO rt_expanded.
        WHEN OTHERS.
          _raise 'walk_tree: unknown chmod'.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD full_tree.

    DATA: ls_object LIKE LINE OF it_objects,
          ls_commit TYPE lcl_git_pack=>ty_commit.


    READ TABLE it_objects INTO ls_object WITH KEY sha1 = iv_branch type = gc_type-commit.
    IF sy-subrc <> 0.
      _raise 'commit not found'.
    ENDIF.
    ls_commit = lcl_git_pack=>decode_commit( ls_object-data ).

    rt_expanded = walk_tree( it_objects = it_objects
                             iv_tree    = ls_commit-tree
                             iv_base    = '/' ).

  ENDMETHOD.                    "root_tree

  METHOD pull.

    DATA: ls_object LIKE LINE OF et_objects,
          ls_commit TYPE lcl_git_pack=>ty_commit,
          lv_pack   TYPE xstring.


    CLEAR et_files.
    CLEAR et_objects.
    CLEAR ev_branch.

    lcl_git_transport=>upload_pack( EXPORTING io_repo = io_repo
                                    IMPORTING et_objects = et_objects
                                              ev_branch = ev_branch ).

    READ TABLE et_objects INTO ls_object WITH KEY sha1 = ev_branch type = gc_type-commit.
    IF sy-subrc <> 0.
      _raise 'Commit/branch not found'.
    ENDIF.
    ls_commit = lcl_git_pack=>decode_commit( ls_object-data ).

    walk( EXPORTING it_objects = et_objects
                    iv_sha1 = ls_commit-tree
                    iv_path = '/'
          CHANGING ct_files = et_files ).

  ENDMETHOD.                    "pull

  METHOD find_folders.

    DATA: lt_paths TYPE TABLE OF string,
          lv_split TYPE string,
          lv_path  TYPE string.

    FIELD-SYMBOLS: <ls_folder> LIKE LINE OF rt_folders,
                   <ls_new>    LIKE LINE OF rt_folders,
                   <ls_exp>    LIKE LINE OF it_expanded.


    LOOP AT it_expanded ASSIGNING <ls_exp>.
      READ TABLE rt_folders WITH KEY path = <ls_exp>-path TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO rt_folders ASSIGNING <ls_folder>.
        <ls_folder>-path = <ls_exp>-path.
      ENDIF.
    ENDLOOP.

* add empty folders
    LOOP AT rt_folders ASSIGNING <ls_folder>.
      SPLIT <ls_folder>-path AT '/' INTO TABLE lt_paths.

      CLEAR lv_path.
      LOOP AT lt_paths INTO lv_split.
        CONCATENATE lv_path lv_split '/' INTO lv_path.
        READ TABLE rt_folders WITH KEY path = lv_path TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO rt_folders ASSIGNING <ls_new>.
          <ls_new>-path = lv_path.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    LOOP AT rt_folders ASSIGNING <ls_folder>.
      FIND ALL OCCURRENCES OF '/' IN <ls_folder>-path MATCH COUNT <ls_folder>-count.
    ENDLOOP.

  ENDMETHOD.

  METHOD build_trees.

    DATA: lt_nodes   TYPE lcl_git_pack=>ty_nodes_tt,
          ls_tree    LIKE LINE OF rt_trees,
          lv_sub     TYPE string,
          lv_len     TYPE i,
          lt_folders TYPE ty_folders_tt.

    FIELD-SYMBOLS: <ls_folder> LIKE LINE OF lt_folders,
                   <ls_node>   LIKE LINE OF lt_nodes,
                   <ls_sub>    LIKE LINE OF lt_folders,
                   <ls_exp>    LIKE LINE OF it_expanded.


    lt_folders = find_folders( it_expanded ).

* start with the deepest folders
    SORT lt_folders BY count DESCENDING.

    LOOP AT lt_folders ASSIGNING <ls_folder>.
      CLEAR lt_nodes.

* files
      LOOP AT it_expanded ASSIGNING <ls_exp> WHERE path = <ls_folder>-path.
        APPEND INITIAL LINE TO lt_nodes ASSIGNING <ls_node>.
        <ls_node>-chmod = <ls_exp>-chmod.
        <ls_node>-name  = <ls_exp>-name.
        <ls_node>-sha1  = <ls_exp>-sha1.
      ENDLOOP.

* folders
      lv_sub = <ls_folder>-path && '+*'.
      LOOP AT lt_folders ASSIGNING <ls_sub>
          WHERE count = <ls_folder>-count + 1 AND path CP lv_sub.
        APPEND INITIAL LINE TO lt_nodes ASSIGNING <ls_node>.
        <ls_node>-chmod = gc_chmod-dir.

* extract folder name, this can probably be done easier using regular expressions
        lv_len = strlen( <ls_folder>-path ).
        <ls_node>-name = <ls_sub>-path+lv_len.
        lv_len = strlen( <ls_node>-name ) - 1.
        <ls_node>-name = <ls_node>-name(lv_len).

        <ls_node>-sha1 = <ls_sub>-sha1.
      ENDLOOP.

      CLEAR ls_tree.
      ls_tree-path = <ls_folder>-path.
      ls_tree-data = lcl_git_pack=>encode_tree( lt_nodes ).
      ls_tree-sha1 = lcl_hash=>sha1( iv_type = gc_type-tree iv_data = ls_tree-data ).
      APPEND ls_tree TO rt_trees.

      <ls_folder>-sha1 = ls_tree-sha1.
    ENDLOOP.

  ENDMETHOD.

  METHOD walk.

    DATA: lv_path  TYPE string,
          ls_file  LIKE LINE OF ct_files,
          lt_nodes TYPE lcl_git_pack=>ty_nodes_tt.

    FIELD-SYMBOLS: <ls_tree> LIKE LINE OF it_objects,
                   <ls_blob> LIKE LINE OF it_objects,
                   <ls_node> LIKE LINE OF lt_nodes.


    READ TABLE it_objects ASSIGNING <ls_tree> WITH KEY sha1 = iv_sha1 type = gc_type-tree.
    IF sy-subrc <> 0.
      _raise 'Walk, tree not found'.
    ENDIF.

    lt_nodes = lcl_git_pack=>decode_tree( <ls_tree>-data ).

    LOOP AT lt_nodes ASSIGNING <ls_node>.
      IF <ls_node>-chmod = gc_chmod-file.
        READ TABLE it_objects ASSIGNING <ls_blob>
          WITH KEY sha1 = <ls_node>-sha1 type = gc_type-blob.
        IF sy-subrc <> 0.
          _raise 'Walk, blob not found'.
        ENDIF.

        CLEAR ls_file.
        ls_file-path     = iv_path.
        ls_file-filename = <ls_node>-name.
        ls_file-data     = <ls_blob>-data.
        APPEND ls_file TO ct_files.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_nodes ASSIGNING <ls_node> WHERE chmod = gc_chmod-dir.
      CONCATENATE iv_path <ls_node>-name '/' INTO lv_path.
      walk( EXPORTING it_objects = it_objects
                      iv_sha1 = <ls_node>-sha1
                      iv_path = lv_path
            CHANGING ct_files = ct_files ).
    ENDLOOP.

  ENDMETHOD.                    "walk

ENDCLASS.                    "lcl_porcelain IMPLEMENTATION