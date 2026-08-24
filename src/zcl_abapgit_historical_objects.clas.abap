CLASS zcl_abapgit_historical_objects DEFINITION PUBLIC.
  PUBLIC SECTION.

    CLASS-METHODS read
      IMPORTING
        iv_objtype      TYPE vrsd-objtype
        iv_objname      TYPE vrsd-objname
        iv_korrnum      TYPE vrsd-korrnum
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_historical_extract=>ty_files_tt.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_parts,
        objtype  TYPE vrsd-objtype,
        objname  TYPE vrsd-objname,
        type     TYPE tadir-object,
        name     TYPE tadir-obj_name,
        devclass TYPE tadir-devclass,
      END OF ty_parts .
    TYPES
      ty_parts_tt TYPE STANDARD TABLE OF ty_parts WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_vrsd,
        objtype TYPE vrsd-objtype,
        objname TYPE vrsd-objname,
        versno  TYPE vrsd-versno,
        korrnum TYPE vrsd-korrnum,
        author  TYPE vrsd-author,
        datum   TYPE vrsd-datum,
        zeit    TYPE vrsd-zeit,
        source  TYPE string,
      END OF ty_vrsd .
    TYPES
      ty_vrsd_tt TYPE STANDARD TABLE OF ty_vrsd WITH EMPTY KEY .

    CLASS-METHODS build
      IMPORTING
        is_tadir        TYPE zif_abapgit_definitions=>ty_tadir
        it_vrsd         TYPE ty_vrsd_tt
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_historical_extract=>ty_files_tt .

    CLASS-METHODS determine_parts
      IMPORTING
        is_tadir        TYPE zif_abapgit_definitions=>ty_tadir
      RETURNING
        VALUE(rt_parts) TYPE ty_parts_tt .

    CLASS-METHODS read_versions
      IMPORTING
        it_parts       TYPE ty_parts_tt
        iv_korrnum     TYPE vrsd-korrnum
      RETURNING
        VALUE(rt_vrsd) TYPE ty_vrsd_tt .

    CLASS-METHODS read_sources
      CHANGING
        ct_vrsd TYPE ty_vrsd_tt .
ENDCLASS.



CLASS ZCL_ABAPGIT_HISTORICAL_OBJECTS IMPLEMENTATION.


  METHOD build.

    DATA ls_file     LIKE LINE OF rt_files.
    DATA ls_extended LIKE LINE OF it_vrsd.


    CASE is_tadir-object.
      WHEN 'CLAS'.
        ls_file-filename = |{ to_lower( is_tadir-obj_name ) }.clas.abap|.

        READ TABLE it_vrsd INTO ls_extended WITH KEY objtype = 'CPUB'.
        IF sy-subrc = 0.
          ls_file-source = |{ ls_extended-source }\n|.
        ENDIF.

        READ TABLE it_vrsd INTO ls_extended WITH KEY objtype = 'CPRO'.
        IF sy-subrc = 0.
          ls_file-source = |{ ls_file-source }{ ls_extended-source }\n|.
        ENDIF.

        READ TABLE it_vrsd INTO ls_extended WITH KEY objtype = 'CPRI'.
        IF sy-subrc = 0.
          ls_file-source = |{ ls_file-source }{ ls_extended-source }\n|.
        ENDIF.

        ls_file-source = |{ ls_file-source }CLASS { to_lower( is_tadir-obj_name ) } IMPLEMENTATION.\n|.
        LOOP AT it_vrsd INTO ls_extended WHERE objtype = 'METH'.
* todo, this seems wrong, the LOOP might find too much?
          ls_file-source = |{ ls_file-source }{ ls_extended-source }\n|.
        ENDLOOP.

        ls_file-source = |{ ls_file-source }ENDCLASS.|.
      WHEN 'INTF'.
        ls_file-filename = |{ to_lower( is_tadir-obj_name ) }.intf.abap|.

        READ TABLE it_vrsd INTO ls_extended WITH KEY objtype = 'INTF'.
        IF sy-subrc = 0.
          ls_file-source = ls_extended-source.
        ENDIF.
      WHEN 'PROG'.
        ls_file-filename = |{ to_lower( is_tadir-obj_name ) }.prog.abap|.

        READ TABLE it_vrsd INTO ls_extended WITH KEY objtype = 'REPS'.
        IF sy-subrc = 0.
          ls_file-source = ls_extended-source.
        ENDIF.
    ENDCASE.

    INSERT ls_file INTO TABLE rt_files.

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
      WHEN 'DOMD'.
        APPEND VALUE #(
          objtype  = 'DOMD'
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


  METHOD read.

    DATA ls_vrsd TYPE vrsd.

    DATA(lv_objtype) = iv_objtype.
* translate
    IF lv_objtype = 'DOMA'.
      BREAK-POINT.
      lv_objtype = 'DOMD'.
    ENDIF.

    SELECT SINGLE * FROM vrsd INTO @ls_vrsd
      WHERE objtype = @lv_objtype
      AND objname = @iv_objname
      AND korrnum = @iv_korrnum.
    IF sy-subrc <> 0.
      " non handled object types, todo
      " then its a deletion? maybe?
      RETURN.
    ENDIF.

    DATA(ls_tadir) = VALUE zif_abapgit_definitions=>ty_tadir(
      obj_name = ls_vrsd-objname
      object   = ls_vrsd-objtype
      devclass = 'TODO' ).
    DATA(lt_parts) = determine_parts( ls_tadir ).

    DATA(lt_vrsd) = read_versions(
      it_parts   = lt_parts
      iv_korrnum = iv_korrnum ).

    rt_files = build(
      is_tadir = ls_tadir
      it_vrsd  = lt_vrsd ).

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
        WHEN 'DOMD'.
          DATA dd07v_tab TYPE STANDARD TABLE OF dd07v WITH DEFAULT KEY.
          DATA dd01v_tab TYPE STANDARD TABLE OF dd01v WITH DEFAULT KEY.
          DATA dd07tv_tab TYPE STANDARD TABLE OF dd07tv WITH DEFAULT KEY.
          DATA dd01tv_tab TYPE STANDARD TABLE OF dd01tv WITH DEFAULT KEY.
          break-point.
          CALL FUNCTION 'SVRS_GET_VERSION_DOMD_40'
            EXPORTING
              object_name = <ls_vrsd>-objname
              versno      = <ls_vrsd>-versno
            TABLES
              dd01v_tab   = dd01v_tab
              dd07v_tab   = dd07v_tab
              dd01tv_tab  = dd01tv_tab
              dd07tv_tab  = dd07tv_tab
            EXCEPTIONS
              no_version  = 01.
        WHEN OTHERS.
          ASSERT 1 = 'todo'.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD read_versions.

    IF lines( it_parts ) = 0.
      RETURN.
    ENDIF.

    SELECT objtype, objname, versno, korrnum, author, datum, zeit
      FROM vrsd INTO CORRESPONDING FIELDS OF TABLE @rt_vrsd
      FOR ALL ENTRIES IN @it_parts
      WHERE objtype = @it_parts-objtype
      AND objname = @it_parts-objname
      AND korrnum = @iv_korrnum
      ORDER BY PRIMARY KEY.

    read_sources( CHANGING ct_vrsd = rt_vrsd ).

  ENDMETHOD.
ENDCLASS.
