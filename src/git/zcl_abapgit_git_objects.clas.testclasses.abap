CLASS ltcl_git_objects DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA: mt_objects TYPE zif_abapgit_definitions=>ty_objects_tt,
          mv_commit  TYPE zif_abapgit_git_definitions=>ty_sha1,
          mv_blob_a  TYPE zif_abapgit_git_definitions=>ty_sha1,
          mv_blob_b  TYPE zif_abapgit_git_definitions=>ty_sha1.

    METHODS:
      setup
        RAISING zcx_abapgit_exception,
      add_object
        IMPORTING iv_type        TYPE zif_abapgit_git_definitions=>ty_type
                  iv_data        TYPE xstring
        RETURNING VALUE(rv_sha1) TYPE zif_abapgit_git_definitions=>ty_sha1
        RAISING   zcx_abapgit_exception,
      build_files FOR TESTING
        RAISING zcx_abapgit_exception,
      build_files_no_commit FOR TESTING,
      full_tree FOR TESTING
        RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_git_objects IMPLEMENTATION.

  METHOD add_object.

    DATA ls_object LIKE LINE OF mt_objects.

    CASE iv_type.
      WHEN zif_abapgit_git_definitions=>c_type-blob.
        rv_sha1 = zcl_abapgit_hash=>sha1_blob( iv_data ).
      WHEN zif_abapgit_git_definitions=>c_type-tree.
        rv_sha1 = zcl_abapgit_hash=>sha1_tree( iv_data ).
      WHEN OTHERS.
        rv_sha1 = zcl_abapgit_hash=>sha1_commit( iv_data ).
    ENDCASE.

    ls_object-sha1 = rv_sha1.
    ls_object-type = iv_type.
    ls_object-data = iv_data.
    INSERT ls_object INTO TABLE mt_objects.

  ENDMETHOD.

  METHOD setup.

    " Repository with /a.txt and /src/b.txt
    DATA: lt_nodes  TYPE zcl_abapgit_git_pack=>ty_nodes_tt,
          ls_node   LIKE LINE OF lt_nodes,
          lv_sub    TYPE zif_abapgit_git_definitions=>ty_sha1,
          ls_commit TYPE zcl_abapgit_git_pack=>ty_commit.

    mv_blob_a = add_object( iv_type = zif_abapgit_git_definitions=>c_type-blob
                            iv_data = zcl_abapgit_convert=>string_to_xstring_utf8( 'a' ) ).
    mv_blob_b = add_object( iv_type = zif_abapgit_git_definitions=>c_type-blob
                            iv_data = zcl_abapgit_convert=>string_to_xstring_utf8( 'b' ) ).

    ls_node-chmod = zif_abapgit_git_definitions=>c_chmod-file.
    ls_node-name  = 'b.txt'.
    ls_node-sha1  = mv_blob_b.
    APPEND ls_node TO lt_nodes.
    lv_sub = add_object( iv_type = zif_abapgit_git_definitions=>c_type-tree
                         iv_data = zcl_abapgit_git_pack=>encode_tree( lt_nodes ) ).

    CLEAR lt_nodes.
    ls_node-chmod = zif_abapgit_git_definitions=>c_chmod-file.
    ls_node-name  = 'a.txt'.
    ls_node-sha1  = mv_blob_a.
    APPEND ls_node TO lt_nodes.
    ls_node-chmod = zif_abapgit_git_definitions=>c_chmod-dir.
    ls_node-name  = 'src'.
    ls_node-sha1  = lv_sub.
    APPEND ls_node TO lt_nodes.
    ls_commit-tree = add_object( iv_type = zif_abapgit_git_definitions=>c_type-tree
                                 iv_data = zcl_abapgit_git_pack=>encode_tree( lt_nodes ) ).

    ls_commit-author    = 'test <test@example.com> 1700000000 +0000'.
    ls_commit-committer = ls_commit-author.
    ls_commit-body      = 'test'.
    mv_commit = add_object( iv_type = zif_abapgit_git_definitions=>c_type-commit
                            iv_data = zcl_abapgit_git_pack=>encode_commit( ls_commit ) ).

  ENDMETHOD.

  METHOD build_files.

    DATA: lt_files TYPE zif_abapgit_git_definitions=>ty_files_tt,
          ls_file  LIKE LINE OF lt_files.

    lt_files = zcl_abapgit_git_objects=>build_files( iv_commit  = mv_commit
                                                     it_objects = mt_objects ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files )
      exp = 2 ).

    READ TABLE lt_files INTO ls_file WITH TABLE KEY file_path COMPONENTS path = '/' filename = 'a.txt'.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_file-sha1
      exp = mv_blob_a ).

    READ TABLE lt_files INTO ls_file WITH TABLE KEY file_path COMPONENTS path = '/src/' filename = 'b.txt'.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_file-data
      exp = zcl_abapgit_convert=>string_to_xstring_utf8( 'b' ) ).

  ENDMETHOD.

  METHOD build_files_no_commit.

    TRY.
        zcl_abapgit_git_objects=>build_files( iv_commit  = mv_blob_a
                                              it_objects = mt_objects ).
        cl_abap_unit_assert=>fail( 'Exception expected' ).
      CATCH zcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD full_tree.

    DATA: lt_expanded TYPE zif_abapgit_git_definitions=>ty_expanded_tt,
          ls_expanded LIKE LINE OF lt_expanded.

    lt_expanded = zcl_abapgit_git_objects=>full_tree( it_objects = mt_objects
                                                      iv_parent  = mv_commit ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_expanded )
      exp = 2 ).

    READ TABLE lt_expanded INTO ls_expanded WITH TABLE KEY path_name COMPONENTS path = '/src/' name = 'b.txt'.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_expanded-sha1
      exp = mv_blob_b ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_expanded-chmod
      exp = zif_abapgit_git_definitions=>c_chmod-file ).

  ENDMETHOD.

ENDCLASS.
