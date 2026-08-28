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
        VALUE(rs_comment) TYPE zif_abapgit_git_definitions=>ty_comment
      RAISING
        zcx_abapgit_exception .

    CLASS-METHODS build_stage
      IMPORTING
        it_files        TYPE zif_abapgit_historical_extract=>ty_files_tt
        it_old_files    TYPE zif_abapgit_git_definitions=>ty_files_tt
      RETURNING
        VALUE(ro_stage) TYPE REF TO zcl_abapgit_stage
      RAISING
        zcx_abapgit_exception .
ENDCLASS.

CLASS zcl_abapgit_historical_git IMPLEMENTATION.
  METHOD build_comment.

    DATA lt_task_lines TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    DATA(li_cts) = zcl_abapgit_factory=>get_cts_api( ).
    DATA(lt_request_and_tasks) = li_cts->read_request_and_tasks( iv_trkorr ).

    DATA(lv_description) = li_cts->read_description( iv_trkorr ).
    lv_description = |{ iv_trkorr } - { lv_description }|.
    rs_comment-comment = |{ lv_description }\n\nTransport: { iv_trkorr }|.

    READ TABLE lt_request_and_tasks INTO DATA(ls_transport)
      WITH KEY trkorr = iv_trkorr.
    IF sy-subrc = 0.
      IF ls_transport-as4user IS NOT INITIAL.
        rs_comment-comment = |{ rs_comment-comment }\nReleased by: { ls_transport-as4user }|.
      ENDIF.
      IF ls_transport-as4date IS NOT INITIAL.
        rs_comment-comment = |{ rs_comment-comment }\nReleased on: { ls_transport-as4date } { ls_transport-as4time }|.
      ENDIF.
    ENDIF.

    LOOP AT lt_request_and_tasks INTO DATA(ls_task)
        WHERE trkorr <> iv_trkorr.
      DATA(lv_task_line) = |- { ls_task-trkorr }|.
      DATA(lv_task_description) = li_cts->read_description( ls_task-trkorr ).
      IF lv_task_description IS NOT INITIAL.
        lv_task_line = |{ lv_task_line }: { lv_task_description }|.
      ENDIF.
      DATA(lv_task_user) = li_cts->read_user( ls_task-trkorr ).
      IF lv_task_user IS NOT INITIAL.
        lv_task_line = |{ lv_task_line } ({ lv_task_user })|.
      ENDIF.
      APPEND lv_task_line TO lt_task_lines.
    ENDLOOP.

    IF lines( lt_task_lines ) > 0.
      rs_comment-comment = |{ rs_comment-comment }\n\nTasks:\n{ concat_lines_of(
        table = lt_task_lines
        sep   = |\n| ) }|.
    ENDIF.

* the owner of the transport is the author, the user running the extract is
* the committer
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

    IF ls_transport-as4date IS NOT INITIAL.
      rs_comment-time = zcl_abapgit_git_time=>get_unix_from_local(
        iv_date = ls_transport-as4date
        iv_time = ls_transport-as4time ).
    ENDIF.

  ENDMETHOD.

  METHOD build_stage.

    ro_stage = NEW zcl_abapgit_stage( ).

    LOOP AT it_files INTO DATA(ls_file).
      IF ls_file-deleted = abap_true.
        IF line_exists( it_old_files[ path = c_path filename = ls_file-filename ] ).
          ro_stage->rm(
            iv_path     = c_path
            iv_filename = ls_file-filename ).
        ENDIF.
        CONTINUE.
      ENDIF.

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

    DATA(lo_stage) = build_stage(
      it_files     = it_files
      it_old_files = ls_pull-files ).
    IF lo_stage->count( ) = 0.
      RETURN.
    ENDIF.

    zcl_abapgit_git_porcelain=>push(
      is_comment     = build_comment( iv_trkorr )
      io_stage       = lo_stage
      it_old_objects = ls_pull-objects
      iv_parent      = ls_pull-commit
      iv_url         = iv_url
      iv_branch_name = lv_branch ).

  ENDMETHOD.

ENDCLASS.
