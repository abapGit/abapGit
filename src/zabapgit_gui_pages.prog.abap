*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_GUI_PAGES
*&---------------------------------------------------------------------*

* All UI pages

* Super class & common html chunks
INCLUDE zabapgit_html_chunks.
INCLUDE zabapgit_page.

* Utils and helpers
INCLUDE zabapgit_html_action_utils.
INCLUDE zabapgit_repo_browser_util.
INCLUDE zabapgit_syntax_highlighter.

* Components and templates
INCLUDE zabapgit_view_repo.
INCLUDE zabapgit_view_tutorial.

* Pages
INCLUDE zabapgit_page_commit.
INCLUDE zabapgit_page_merge.
INCLUDE zabapgit_page_background.
INCLUDE zabapgit_page_branch_overview.
INCLUDE zabapgit_page_db.
INCLUDE zabapgit_page_diff.
INCLUDE zabapgit_page_explore.
INCLUDE zabapgit_page_main.
INCLUDE zabapgit_page_stage.
INCLUDE zabapgit_page_debug.
INCLUDE zabapgit_page_settings.
