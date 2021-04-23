CLASS zcl_abapgit_historical_extract DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_devc_range TYPE RANGE OF tadir-devclass .

    METHODS run
      IMPORTING
        !it_packages TYPE ty_devc_range
        !iv_skip_git TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.

    TYPES:
      ty_timestamp TYPE c LENGTH 14 .
    TYPES:
      BEGIN OF ty_parts,
        objtype  TYPE vrsd-objtype,
        objname  TYPE vrsd-objname,
        type     TYPE tadir-object,
        name     TYPE tadir-obj_name,
        devclass TYPE tadir-devclass,
      END OF ty_parts .
    TYPES:
      ty_parts_tt TYPE STANDARD TABLE OF ty_parts WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_vrsd,
        objtype TYPE vrsd-objtype,
        objname TYPE vrsd-objname,
        versno  TYPE vrsd-versno,
        author  TYPE vrsd-author,
        datum   TYPE vrsd-datum,
        zeit    TYPE vrsd-zeit,
        source  TYPE string,
      END OF ty_vrsd .
    TYPES:
      ty_vrsd_tt TYPE STANDARD TABLE OF ty_vrsd WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_extended.
        INCLUDE TYPE ty_vrsd.
    TYPES: from TYPE ty_timestamp,
        to   TYPE ty_timestamp,
      END OF ty_extended .
    TYPES:
      ty_extended_tt TYPE STANDARD TABLE OF ty_extended WITH EMPTY KEY .
    TYPES:
      ty_timestamps_tt TYPE SORTED TABLE OF ty_timestamp WITH UNIQUE DEFAULT KEY .
    TYPES:
      BEGIN OF ty_file,
        filename  TYPE string,
        source    TYPE string,
        timestamp TYPE ty_timestamp,
      END OF ty_file .
    TYPES:
      ty_files_tt TYPE STANDARD TABLE OF ty_file WITH EMPTY KEY .

    DATA mv_url TYPE string .
    DATA mv_branch_name TYPE string .
    DATA ms_remote TYPE zcl_abapgit_git_porcelain=>ty_pull_result.

    METHODS build
      IMPORTING
        !is_tadir       TYPE zif_abapgit_definitions=>ty_tadir
        !it_vrsd        TYPE ty_vrsd_tt
      RETURNING
        VALUE(rt_files) TYPE ty_files_tt .
    METHODS build_timestamps
      IMPORTING
        !it_extended         TYPE ty_extended_tt
      RETURNING
        VALUE(rt_timestamps) TYPE ty_timestamps_tt .
    METHODS determine_parts
      IMPORTING
        !is_tadir       TYPE zif_abapgit_definitions=>ty_tadir
      RETURNING
        VALUE(rt_parts) TYPE ty_parts_tt .
    METHODS extend
      IMPORTING
        !it_vrsd           TYPE ty_vrsd_tt
      RETURNING
        VALUE(rt_extended) TYPE ty_extended_tt .
    METHODS git_push
      IMPORTING
        !it_files TYPE ty_files_tt
      RAISING
        zcx_abapgit_exception .
    METHODS read_sources
      CHANGING
        !ct_vrsd TYPE ty_vrsd_tt .
    METHODS read_tadir
      IMPORTING
        !it_packages    TYPE ty_devc_range
      RETURNING
        VALUE(rt_tadir) TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .
    METHODS read_versions
      IMPORTING
        !it_parts      TYPE ty_parts_tt
      RETURNING
        VALUE(rt_vrsd) TYPE ty_vrsd_tt .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_HISTORICAL_EXTRACT IMPLEMENTATION.


  METHOD build.

    DATA ls_file LIKE LINE OF rt_files.

    DATA(lt_extended) = extend( it_vrsd ).
    DATA ls_extended LIKE LINE OF lt_extended.
    DATA(lt_timestamps) = build_timestamps( lt_extended ).

    LOOP AT lt_timestamps INTO DATA(lv_timestamp).
      CLEAR ls_file.
      DATA(lt_filtered) = lt_extended.
      DELETE lt_filtered WHERE to <= lv_timestamp.
      DELETE lt_filtered WHERE from > lv_timestamp.

      CASE is_tadir-object.
        WHEN 'CLAS'.
          READ TABLE lt_filtered INTO ls_extended WITH KEY objtype = 'CPUB'.
          IF sy-subrc = 0.
            ls_file-source = |{ ls_extended-source }\n|.
          ENDIF.

          READ TABLE lt_filtered INTO ls_extended WITH KEY objtype = 'CPRO'.
          IF sy-subrc = 0.
            ls_file-source = |{ ls_file-source }{ ls_extended-source }\n|.
          ENDIF.

          READ TABLE lt_filtered INTO ls_extended WITH KEY objtype = 'CPRI'.
          IF sy-subrc = 0.
            ls_file-source = |{ ls_file-source }{ ls_extended-source }\n|.
          ENDIF.

          ls_file-source = |{ ls_file-source }CLASS { to_lower( is_tadir-obj_name ) } IMPLEMENTATION.\n|.
          LOOP AT lt_filtered INTO ls_extended WHERE objtype = 'METH'.
* todo, this seems wrong, the LOOP might find too much?
            ls_file-source = |{ ls_file-source }{ ls_extended-source }\n|.
          ENDLOOP.

          ls_file-source = |{ ls_file-source }ENDCLASS.|.
        WHEN 'INTF'.
          READ TABLE lt_filtered INTO ls_extended WITH KEY objtype = 'INTF'.
          IF sy-subrc = 0.
            ls_file-source = ls_extended-source.
          ENDIF.
        WHEN 'PROG'.
          READ TABLE lt_filtered INTO ls_extended WITH KEY objtype = 'REPS'.
          IF sy-subrc = 0.
            ls_file-source = ls_extended-source.
          ENDIF.
      ENDCASE.

      ls_file-filename = 'todo.txt'.
      ls_file-timestamp = lv_timestamp.
      APPEND ls_file TO rt_files.
    ENDLOOP.

  ENDMETHOD.


  METHOD build_timestamps.

    LOOP AT it_extended ASSIGNING FIELD-SYMBOL(<ls_vrsd>).
      INSERT <ls_vrsd>-from INTO TABLE rt_timestamps.
    ENDLOOP.

  ENDMETHOD.


  METHOD determine_parts.

    CASE is_tadir-object.
      WHEN 'PROG'.
        APPEND VALUE #(
          objtype  = 'REPS'
          objname  = is_tadir-obj_name
          type     = is_tadir-object
          name     = is_tadir-obj_name
          devclass = is_tadir-devclass ) TO rt_parts.
      WHEN 'INTF'.
        APPEND VALUE #(
          objtype  = 'INTF'
          objname  = is_tadir-obj_name
          type     = is_tadir-object
          name     = is_tadir-obj_name
          devclass = is_tadir-devclass ) TO rt_parts.
      WHEN 'CLAS'.
* note that the CLSD is not needed
* 4 x CINC, dont serialize if empty, CCAU + CCDEF + CCIMP + CCMAC
        APPEND VALUE #(
          objtype  = 'CINC'
          objname  = cl_oo_classname_service=>get_ccau_name( CONV #( is_tadir-obj_name ) )
          type     = is_tadir-object
          name     = is_tadir-obj_name
          devclass = is_tadir-devclass ) TO rt_parts.
        APPEND VALUE #(
          objtype  = 'CINC'
          objname  = cl_oo_classname_service=>get_ccimp_name( CONV #( is_tadir-obj_name ) )
          type     = is_tadir-object
          name     = is_tadir-obj_name
          devclass = is_tadir-devclass ) TO rt_parts.
        APPEND VALUE #(
          objtype  = 'CINC'
          objname  = cl_oo_classname_service=>get_ccdef_name( CONV #( is_tadir-obj_name ) )
          type     = is_tadir-object
          name     = is_tadir-obj_name
          devclass = is_tadir-devclass ) TO rt_parts.
        APPEND VALUE #(
          objtype  = 'CINC'
          objname  = cl_oo_classname_service=>get_ccmac_name( CONV #( is_tadir-obj_name ) )
          type     = is_tadir-object
          name     = is_tadir-obj_name
          devclass = is_tadir-devclass ) TO rt_parts.
        APPEND VALUE #(
          objtype  = 'CPUB'
          objname  = is_tadir-obj_name
          type     = is_tadir-object
          name     = is_tadir-obj_name
          devclass = is_tadir-devclass ) TO rt_parts.
        APPEND VALUE #(
          objtype  = 'CPRO'
          objname  = is_tadir-obj_name
          type     = is_tadir-object
          name     = is_tadir-obj_name
          devclass = is_tadir-devclass ) TO rt_parts.
        APPEND VALUE #(
          objtype  = 'CPRI'
          objname  = is_tadir-obj_name
          type     = is_tadir-object
          name     = is_tadir-obj_name
          devclass = is_tadir-devclass ) TO rt_parts.
* ? x METH
        DATA(lv_objname) = |{ is_tadir-obj_name WIDTH = 30 }%|.
        SELECT DISTINCT objtype, objname FROM vrsd INTO TABLE @DATA(lt_methods)
          WHERE objtype = 'METH'
          AND objname LIKE @lv_objname
          ORDER BY objtype, objname.
        LOOP AT lt_methods INTO DATA(ls_method).
          APPEND VALUE #(
            objtype  = ls_method-objtype
            objname  = ls_method-objname
            type     = is_tadir-object
            name     = is_tadir-obj_name
            devclass = is_tadir-devclass ) TO rt_parts.
        ENDLOOP.
      WHEN OTHERS.
        ASSERT 1 = 'todo'.
    ENDCASE.

  ENDMETHOD.


  METHOD extend.

    DATA lv_next TYPE vrsd-versno.

    LOOP AT it_vrsd ASSIGNING FIELD-SYMBOL(<ls_vrsd>).
      DATA(ls_extended) = CORRESPONDING ty_extended( <ls_vrsd> ).
      ls_extended-from = |{ <ls_vrsd>-datum }{ <ls_vrsd>-zeit }|.
      APPEND ls_extended TO rt_extended.
    ENDLOOP.

    LOOP AT rt_extended ASSIGNING FIELD-SYMBOL(<ls_extended>).
      lv_next = <ls_extended>-versno + 1.
      READ TABLE rt_extended INTO DATA(ls_to) WITH KEY
        objtype = <ls_extended>-objtype
        objname = <ls_extended>-objname
        versno = lv_next.
      IF sy-subrc = 0.
        <ls_extended>-to = ls_to-from.
      ELSE.
        <ls_extended>-to = '99991231235959'.
      ENDIF.
    ENDLOOP.

    SORT rt_extended BY objtype objname versno.

  ENDMETHOD.


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
      DATA(ls_comment) = VALUE zif_abapgit_definitions=>ty_comment(
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


  METHOD read_sources.

    DATA lt_repos TYPE STANDARD TABLE OF abaptxt255 WITH EMPTY KEY.
    DATA lt_trdir TYPE STANDARD TABLE OF trdir WITH EMPTY KEY.


    LOOP AT ct_vrsd ASSIGNING FIELD-SYMBOL(<ls_vrsd>).
      CASE <ls_vrsd>-objtype.
        WHEN 'REPS' OR 'INTF' OR 'METH' OR 'CPRI' OR 'CPRO' OR 'CPUB' OR 'CINC'.
* note that this function module returns the full 255 character width source code
* plus works for multiple object types
          CALL FUNCTION 'SVRS_GET_REPS_FROM_OBJECT'
            EXPORTING
              object_name = <ls_vrsd>-objname
              object_type = <ls_vrsd>-objtype
              versno      = <ls_vrsd>-versno
            TABLES
              repos_tab   = lt_repos
              trdir_tab   = lt_trdir
            EXCEPTIONS
              no_version  = 1
              OTHERS      = 2.
          IF sy-subrc = 0.
            <ls_vrsd>-source = concat_lines_of( table = lt_repos
                                                sep   = |\n| ).
          ENDIF.
        WHEN OTHERS.
          ASSERT 1 = 'todo'.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD read_tadir.

    SELECT devclass, parentcl
      FROM tdevc INTO TABLE @DATA(lt_tdevc)
      WHERE devclass IN @it_packages
      AND parentcl = ''
      ORDER BY PRIMARY KEY.

    DATA(lo_dot) = zcl_abapgit_dot_abapgit=>build_default( ).
    lo_dot->set_folder_logic( zif_abapgit_dot_abapgit=>c_folder_logic-full ).

    LOOP AT lt_tdevc INTO DATA(ls_tdevc).
      APPEND LINES OF zcl_abapgit_factory=>get_tadir( )->read(
        io_dot     = lo_dot
        iv_package = ls_tdevc-devclass ) TO rt_tadir.
    ENDLOOP.

    DELETE rt_tadir WHERE object <> 'PROG'
      AND object <> 'INTF'
      AND object <> 'CLAS'.

  ENDMETHOD.


  METHOD read_versions.

    IF lines( it_parts ) = 0.
      RETURN.
    ENDIF.

    SELECT objtype, objname, versno, author, datum, zeit
      FROM vrsd INTO CORRESPONDING FIELDS OF TABLE @rt_vrsd
      FOR ALL ENTRIES IN @it_parts
      WHERE objtype = @it_parts-objtype
      AND objname = @it_parts-objname
      ORDER BY PRIMARY KEY.

    read_sources( CHANGING ct_vrsd = rt_vrsd ).

  ENDMETHOD.


  METHOD run.

    CLEAR ms_remote.
    mv_url = |https://github.com/larshp/test-hist.git|.
    mv_branch_name = |refs/heads/{ sy-sysid }_{ sy-datum }_{ sy-uzeit }|.

    DATA(lt_tadir) = read_tadir( it_packages ).

    LOOP AT lt_tadir INTO DATA(ls_tadir).
      cl_progress_indicator=>progress_indicate(
        i_text               = |Processing objects, { sy-tabix }/{ lines( lt_tadir ) }|
        i_processed          = sy-tabix
        i_total              = lines( lt_tadir )
        i_output_immediately = abap_true ).

      DATA(lt_parts) = determine_parts( ls_tadir ).
      DATA(lt_vrsd) = read_versions( lt_parts ).

      DATA(lt_files) = build(
        is_tadir = ls_tadir
        it_vrsd  = lt_vrsd ).

      git_push( lt_files ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
