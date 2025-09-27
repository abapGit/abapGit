INTERFACE zif_abapgit_diff3 PUBLIC.


************************************************************************
* ABAP Diff3 Interface
*
* https://github.com/Marc-Bernard-Tools/ABAP-Diff3
*
* This is a port of JavaScript (https://github.com/bhousel/node-diff3, MIT license)
* https://github.com/bhousel/node-diff3/blob/main/index.d.ts as of 2021-05-04
*
* Copyright 2022 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: MIT
************************************************************************

  TYPES ty_number TYPE i.

  TYPES:
    ty_numbers TYPE STANDARD TABLE OF ty_number WITH DEFAULT KEY.

  TYPES:
    BEGIN OF ty_number_number,
      key TYPE ty_number,
      len TYPE ty_number,
    END OF ty_number_number.

  TYPES:
    BEGIN OF ty_lcs_result,
      key          TYPE i,
      buffer1index TYPE ty_number,
      buffer2index TYPE ty_number,
      chain        TYPE i, " ref to ilcsresult-key
    END OF ty_lcs_result.
  TYPES:
    ty_lcs_result_t TYPE SORTED TABLE OF ty_lcs_result WITH UNIQUE KEY key.

  TYPES:
    BEGIN OF ty_comm_result,
      common TYPE string_table,
      BEGIN OF diff,
        buffer1 TYPE string_table,
        buffer2 TYPE string_table,
      END OF diff,
    END OF ty_comm_result.
  TYPES:
    ty_comm_result_t TYPE STANDARD TABLE OF ty_comm_result WITH DEFAULT KEY.

  TYPES:
    BEGIN OF ty_diff_indices_result,
      buffer1        TYPE ty_number_number,
      buffer1content TYPE string_table,
      buffer2        TYPE ty_number_number,
      buffer2content TYPE string_table,
    END OF ty_diff_indices_result.
  TYPES:
    ty_diff_indices_result_t TYPE STANDARD TABLE OF ty_diff_indices_result WITH DEFAULT KEY.

  TYPES:
    BEGIN OF ty_chunk,
      offset TYPE ty_number,
      length TYPE ty_number,
      chunk  TYPE string_table,
    END OF ty_chunk.

  TYPES:
    BEGIN OF ty_patch_result,
      buffer1 TYPE ty_chunk,
      buffer2 TYPE ty_chunk,
    END OF ty_patch_result.
  TYPES:
    ty_patch_result_t TYPE STANDARD TABLE OF ty_patch_result WITH DEFAULT KEY.

  TYPES:
    BEGIN OF ty_stable_region,
      buffer         TYPE c LENGTH 1,
      buffer_start   TYPE ty_number,
      buffer_length  TYPE ty_number,
      buffer_content TYPE string_table,
    END OF ty_stable_region.
  TYPES:
    BEGIN OF ty_unstable_region,
      a_start   TYPE ty_number,
      a_length  TYPE ty_number,
      a_content TYPE string_table,
      b_start   TYPE ty_number,
      b_length  TYPE ty_number,
      b_content TYPE string_table,
      o_start   TYPE ty_number,
      o_length  TYPE ty_number,
      o_content TYPE string_table,
    END OF ty_unstable_region.

  TYPES:
    BEGIN OF ty_region,
      stable          TYPE abap_bool,
      stable_region   TYPE ty_stable_region,
      unstable_region TYPE ty_unstable_region,
    END OF ty_region.
  TYPES:
    ty_region_t TYPE STANDARD TABLE OF ty_region WITH DEFAULT KEY.

  TYPES:
    BEGIN OF ty_merge_region,
      ok TYPE string_table,
      BEGIN OF conflict,
        a       TYPE string_table,
        a_index TYPE ty_number,
        o       TYPE string_table,
        o_index TYPE ty_number,
        b       TYPE string_table,
        b_index TYPE ty_number,
      END OF conflict,
    END OF ty_merge_region.
  TYPES:
    ty_merge_region_t TYPE STANDARD TABLE OF ty_merge_region WITH DEFAULT KEY.

  TYPES:
    BEGIN OF ty_merge_result,
      conflict TYPE abap_bool,
      result   TYPE string_table,
    END OF ty_merge_result.

  TYPES:
    BEGIN OF ty_labels,
      a TYPE string,
      o TYPE string,
      x TYPE string,
      b TYPE string,
    END OF ty_labels.

  "! Expects two arrays, finds longest common sequence
  METHODS lcs
    IMPORTING
      !it_buffer1      TYPE string_table
      !it_buffer2      TYPE string_table
    RETURNING
      VALUE(rt_result) TYPE ty_lcs_result_t.

  "! We apply the LCS to build a 'comm'-style picture of the
  "! differences between buffer1 and buffer2.
  METHODS diff_comm
    IMPORTING
      !it_buffer1      TYPE string_table
      !it_buffer2      TYPE string_table
    RETURNING
      VALUE(rt_result) TYPE ty_comm_result_t.

  "! We apply the LCS to give a simple representation of the
  "! offsets and lengths of mismatched chunks in the input
  "! buffers. This is used by diff3MergeRegions.
  METHODS diff_indices
    IMPORTING
      !it_buffer1      TYPE string_table
      !it_buffer2      TYPE string_table
    RETURNING
      VALUE(rt_result) TYPE ty_diff_indices_result_t.

  "! We apply the LCS to build a JSON representation of a
  "! diff(1)-style patch.
  METHODS diff_patch
    IMPORTING
      !it_buffer1      TYPE string_table
      !it_buffer2      TYPE string_table
    RETURNING
      VALUE(rt_result) TYPE ty_patch_result_t.

  METHODS patch
    IMPORTING
      !it_buffer       TYPE string_table
      !it_patchres     TYPE ty_patch_result_t
    RETURNING
      VALUE(rt_result) TYPE string_table.

  "! Takes the output of diffPatch(), and removes extra information from it.
  "! It can still be used by patch(), below, but can no longer be inverted.
  METHODS strip_patch
    IMPORTING
      !it_patchres     TYPE ty_patch_result_t
    RETURNING
      VALUE(rt_result) TYPE ty_patch_result_t.

  "! Takes the output of diffPatch(), and inverts the sense of it, so that it
  "! can be applied to buffer2 to give buffer1 rather than the other way around.
  METHODS invert_patch
    IMPORTING
      !it_patchres     TYPE ty_patch_result_t
    RETURNING
      VALUE(rt_result) TYPE ty_patch_result_t.

  "! Given three buffers, A, O, and B, where both A and B are
  "! independently derived from O, returns a fairly complicated
  "! internal representation of merge decisions it's taken. The
  "! interested reader may wish to consult
  "!
  "! Sanjeev Khanna, Keshav Kunal, and Benjamin C. Pierce.
  "! 'A Formal Investigation of ' In Arvind and Prasad,
  "! editors, Foundations of Software Technology and Theoretical
  "! Computer Science (FSTTCS), December 2007.
  "!
  "! (http://www.cis.upenn.edu/~bcpierce/papers/diff3-short.pdf)
  METHODS diff3_merge_regions
    IMPORTING
      !it_a            TYPE string_table
      !it_o            TYPE string_table
      !it_b            TYPE string_table
    RETURNING
      VALUE(rt_result) TYPE ty_region_t.

  "! Applies the output of diff3MergeRegions to actually
  "! construct the merged buffer; the returned result alternates
  "! between 'ok' and 'conflict' blocks.
  "! A "false conflict" is where `a` and `b` both change the same from `o`
  METHODS diff3_merge
    IMPORTING
      !it_a                       TYPE string_table
      !it_o                       TYPE string_table
      !it_b                       TYPE string_table
      !iv_exclude_false_conflicts TYPE abap_bool DEFAULT abap_true
    RETURNING
      VALUE(rt_result)            TYPE ty_merge_region_t.

  METHODS merge
    IMPORTING
      !it_a                       TYPE string_table
      !it_o                       TYPE string_table
      !it_b                       TYPE string_table
      !iv_exclude_false_conflicts TYPE abap_bool DEFAULT abap_true
      !is_labels                  TYPE ty_labels OPTIONAL
    RETURNING
      VALUE(rs_result)            TYPE ty_merge_result.

  METHODS merge_diff3
    IMPORTING
      !it_a                       TYPE string_table
      !it_o                       TYPE string_table
      !it_b                       TYPE string_table
      !iv_exclude_false_conflicts TYPE abap_bool DEFAULT abap_true
      !is_labels                  TYPE ty_labels OPTIONAL
    RETURNING
      VALUE(rs_result)            TYPE ty_merge_result.

  METHODS merge_dig_in
    IMPORTING
      !it_a                       TYPE string_table
      !it_o                       TYPE string_table
      !it_b                       TYPE string_table
      !iv_exclude_false_conflicts TYPE abap_bool DEFAULT abap_true
      !is_labels                  TYPE ty_labels OPTIONAL
    RETURNING
      VALUE(rs_result)            TYPE ty_merge_result.

ENDINTERFACE.
