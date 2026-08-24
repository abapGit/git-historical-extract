CLASS zcl_abapgit_historical_git DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS push
      IMPORTING
        iv_trkorr TYPE e070-trkorr
        it_files  TYPE zif_abapgit_historical_extract=>ty_files_tt
        iv_url    TYPE string
        iv_branch TYPE string
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

* todo, the files should be placed according to the package structure, see README
    CONSTANTS c_path TYPE string VALUE '/src/' ##NO_TEXT.

    CLASS-METHODS build_comment
      IMPORTING
        iv_trkorr         TYPE e070-trkorr
      RETURNING
        VALUE(rs_comment) TYPE zif_abapgit_git_definitions=>ty_comment .

    CLASS-METHODS build_stage
      IMPORTING
        it_files        TYPE zif_abapgit_historical_extract=>ty_files_tt
      RETURNING
        VALUE(ro_stage) TYPE REF TO zcl_abapgit_stage
      RAISING
        zcx_abapgit_exception .
ENDCLASS.

CLASS zcl_abapgit_historical_git IMPLEMENTATION.
  METHOD build_comment.

    DATA(li_cts) = zcl_abapgit_factory=>get_cts_api( ).

    DATA(lv_description) = li_cts->read_description( iv_trkorr ).
    IF lv_description IS INITIAL.
* released transports without a short text still deserve a subject line
      lv_description = |Transport { iv_trkorr }|.
    ENDIF.
    rs_comment-comment = |{ lv_description }\n\nTransport: { iv_trkorr }|.

* the owner of the transport is the author, the user running the extract is
* the committer
* todo, abapGit always stamps the commit with the current time, the release
* date of the transport (e070-as4date and as4time) should be used instead,
* this needs an optional "time" in zif_abapgit_git_definitions=>ty_comment
    DATA(lv_owner) = li_cts->read_user( iv_trkorr ).
    IF lv_owner IS INITIAL.
      lv_owner = sy-uname.
    ENDIF.

* use the raw user name from the transport, the user record is typically
* long gone for old transports
    rs_comment-author-name = lv_owner.
    rs_comment-author-email = |{ lv_owner }@localhost|.

    rs_comment-committer-name = sy-uname.
    rs_comment-committer-email = |{ sy-uname }@localhost|.

  ENDMETHOD.

  METHOD build_stage.

    ro_stage = NEW zcl_abapgit_stage( ).

    LOOP AT it_files INTO DATA(ls_file).
      ro_stage->add(
        iv_path     = c_path
        iv_filename = ls_file-filename
        iv_data     = zcl_abapgit_convert=>string_to_xstring_utf8( ls_file-source ) ).
    ENDLOOP.

  ENDMETHOD.

  METHOD push.
    ASSERT iv_url IS NOT INITIAL.
    ASSERT iv_branch IS NOT INITIAL.

    IF lines( it_files ) = 0.
* a transport can be empty after filtering on object type, nothing to commit
      RETURN.
    ENDIF.

    DATA(lv_branch) = zcl_abapgit_git_branch_utils=>complete_heads_branch_name(
      zcl_abapgit_git_branch_utils=>normalize_branch_name( iv_branch ) ).

* the branch must already exist with at least one commit, abapGit cannot
* build the tree of the first commit of a branch
    DATA(ls_pull) = zcl_abapgit_git_porcelain=>pull_by_branch(
      iv_url         = iv_url
      iv_branch_name = lv_branch ).

    zcl_abapgit_git_porcelain=>push(
      is_comment     = build_comment( iv_trkorr )
      io_stage       = build_stage( it_files )
      it_old_objects = ls_pull-objects
      iv_parent      = ls_pull-commit
      iv_url         = iv_url
      iv_branch_name = lv_branch ).

  ENDMETHOD.

ENDCLASS.
