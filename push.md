  METHOD git_push.

* some inspiration in https://github.com/abaplint/abaplint-sci-client/blob/main/src/zabaplint_dependencies.prog.abap

    IF ms_remote IS INITIAL.
* create new branch from default branch
      ms_remote = zcl_abapgit_git_porcelain=>pull_by_branch(
        iv_url         = mv_url
        iv_branch_name = zcl_abapgit_git_transport=>branches( mv_url )->get_head_symref( ) ).
      zcl_abapgit_git_porcelain=>create_branch(
        iv_url  = mv_url
        iv_name = mv_branch_name
        iv_from = ms_remote-commit ).
    ENDIF.

    LOOP AT it_files INTO DATA(ls_file).
*  push
      DATA(ls_comment) = VALUE zif_abapgit_git_definitions=>ty_comment(
        committer = VALUE #( name = 'asdf' email = 'asdf@localhost' )
        author    = VALUE #( name = 'asdf' email = 'asdf@localhost' )
        comment   = |{ ls_file-filename }{ ls_file-timestamp }| ).
      DATA(lo_stage) = NEW zcl_abapgit_stage( ).
      lo_stage->add( iv_path     = '/'
                     iv_filename = ls_file-filename
                     iv_data     = zcl_abapgit_convert=>string_to_xstring_utf8( ls_file-source ) ).

      DATA(ls_push_result) = zcl_abapgit_git_porcelain=>push(
        is_comment     = ls_comment
        io_stage       = lo_stage
        it_old_objects = ms_remote-objects
        iv_parent      = ms_remote-commit
        iv_url         = mv_url
        iv_branch_name = mv_branch_name ).

* prepare for next push
      ms_remote-files = ls_push_result-new_files.
      ms_remote-objects = ls_push_result-new_objects.
      ms_remote-commit = ls_push_result-branch.
    ENDLOOP.

  ENDMETHOD.