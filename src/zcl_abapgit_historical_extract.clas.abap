CLASS zcl_abapgit_historical_extract DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_devc_range TYPE RANGE OF tadir-devclass .

    METHODS run
      IMPORTING
        !it_packages TYPE ty_devc_range
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.

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
    TYPES: BEGIN OF ty_vrsd,
             objtype TYPE vrsd-objtype,
             objname TYPE vrsd-objname,
             versno  TYPE vrsd-versno,
             author  TYPE vrsd-author,
             datum   TYPE vrsd-datum,
             zeit    TYPE vrsd-zeit,
           END OF ty_vrsd.
    TYPES:
      ty_vrsd_tt TYPE STANDARD TABLE OF ty_vrsd WITH EMPTY KEY .

    TYPES: BEGIN OF ty_sources,
             objtype TYPE vrsd-objtype,
             objname TYPE vrsd-objname,
             versno  TYPE vrsd-versno,
             source  TYPE string,
           END OF ty_sources.
    TYPES ty_sources_tt TYPE SORTED TABLE OF ty_sources WITH UNIQUE KEY objtype objname versno.

    METHODS read_tadir
      IMPORTING
        !it_packages    TYPE ty_devc_range
      RETURNING
        VALUE(rt_tadir) TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .
    METHODS determine_parts
      IMPORTING
        !is_tadir       TYPE zif_abapgit_definitions=>ty_tadir
      RETURNING
        VALUE(rt_parts) TYPE ty_parts_tt .
    METHODS read_sources
      IMPORTING
        !it_vrsd          TYPE ty_vrsd_tt
      RETURNING
        VALUE(rt_sources) TYPE ty_sources_tt .
    METHODS read_versions
      IMPORTING
        !it_parts      TYPE ty_parts_tt
      RETURNING
        VALUE(rt_vrsd) TYPE ty_vrsd_tt .
    METHODS build.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_HISTORICAL_EXTRACT IMPLEMENTATION.


  METHOD build.
* todo
    RETURN.
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


  METHOD read_sources.

    DATA lt_repos TYPE STANDARD TABLE OF abaptxt255 WITH EMPTY KEY.
    DATA lt_trdir TYPE STANDARD TABLE OF trdir WITH EMPTY KEY.
    DATA lv_source TYPE string.

    LOOP AT it_vrsd INTO DATA(ls_vrsd).
      CASE ls_vrsd-objtype.
        WHEN 'REPS' OR 'INTF' OR 'METH' OR 'CPRI' OR 'CPRO' OR 'CPUB' OR 'CINC'.
* note that this function module returns the full 255 character width source code
* plus works for multiple object types
          CALL FUNCTION 'SVRS_GET_REPS_FROM_OBJECT'
            EXPORTING
              object_name = ls_vrsd-objname
              object_type = ls_vrsd-objtype
              versno      = ls_vrsd-versno
            TABLES
              repos_tab   = lt_repos
              trdir_tab   = lt_trdir
            EXCEPTIONS
              no_version  = 1
              OTHERS      = 2.
          IF sy-subrc <> 0.
            CLEAR lv_source.
          ELSE.
            lv_source = concat_lines_of( table = lt_repos
                                         sep   = |\n| ).
          ENDIF.
          INSERT VALUE #(
            objtype = ls_vrsd-objname
            objname = ls_vrsd-objtype
            versno  = ls_vrsd-versno
            source  = lv_source ) INTO TABLE rt_sources.
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
      FROM vrsd INTO TABLE @rt_vrsd
      FOR ALL ENTRIES IN @it_parts
      WHERE objtype = @it_parts-objtype
      AND objname = @it_parts-objname
      ORDER BY PRIMARY KEY.

  ENDMETHOD.


  METHOD run.

    DATA(lt_tadir) = read_tadir( it_packages ).

    LOOP AT lt_tadir INTO DATA(ls_tadir).
      IF sy-tabix MOD 10 = 0.
        cl_progress_indicator=>progress_indicate(
          i_text               = |Processing objects, { sy-tabix }/{ lines( lt_tadir ) }|
          i_processed          = sy-tabix
          i_total              = lines( lt_tadir )
          i_output_immediately = abap_true ).
      ENDIF.

      DATA(lt_parts) = determine_parts( ls_tadir ).
      DATA(lt_vrsd) = read_versions( lt_parts ).
      DATA(lt_sources) = read_sources( lt_vrsd ).

      build( ).

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
