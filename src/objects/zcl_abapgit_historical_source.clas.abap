CLASS zcl_abapgit_historical_source DEFINITION PUBLIC.
  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_program,
        source TYPE string,
        trdir  TYPE trdir,
      END OF ty_program .
    TYPES:
      BEGIN OF ty_release,
        trkorr  TYPE e070-trkorr,
        as4date TYPE e070-as4date,
        as4time TYPE e070-as4time,
      END OF ty_release .

    CLASS-METHODS read_versions
      IMPORTING
        it_parts       TYPE zif_abapgit_historical_object=>ty_parts_tt
        iv_korrnum     TYPE vrsd-korrnum
      RETURNING
        VALUE(rt_vrsd) TYPE zif_abapgit_historical_object=>ty_vrsd_tt .

* the newest version of each part as of the release of the transport, for objects whose
* transports only carry the parts that changed
    CLASS-METHODS read_snapshot
      IMPORTING
        it_parts       TYPE zif_abapgit_historical_object=>ty_parts_tt
        iv_korrnum     TYPE vrsd-korrnum
      RETURNING
        VALUE(rt_vrsd) TYPE zif_abapgit_historical_object=>ty_vrsd_tt .

    CLASS-METHODS read_reps
      IMPORTING
        is_vrsd          TYPE zif_abapgit_historical_object=>ty_vrsd
      RETURNING
        VALUE(rv_source) TYPE string .

* the source and the attributes of a program version, initial when the version does not exist
    CLASS-METHODS read_program
      IMPORTING
        is_vrsd           TYPE zif_abapgit_historical_object=>ty_vrsd
      RETURNING
        VALUE(rs_program) TYPE ty_program
      RAISING
        zcx_abapgit_exception .

* the release of a request, initial when the request does not exist
    CLASS-METHODS read_release
      IMPORTING
        iv_trkorr         TYPE e070-trkorr
      RETURNING
        VALUE(rs_release) TYPE ty_release .

* true when the request is released after the reference, in the order the extraction
* processes transports: by release date, time, and request number
    CLASS-METHODS is_released_after
      IMPORTING
        is_release      TYPE ty_release
        is_reference    TYPE ty_release
      RETURNING
        VALUE(rv_after) TYPE abap_bool .

  PROTECTED SECTION.
ENDCLASS.

CLASS zcl_abapgit_historical_source IMPLEMENTATION.
  METHOD read_reps.

    DATA lt_repos TYPE STANDARD TABLE OF abaptxt255 WITH EMPTY KEY.
    DATA lt_trdir TYPE STANDARD TABLE OF trdir WITH EMPTY KEY.

* note that this function module returns the full 255 character width source code
* plus works for multiple object types
    CALL FUNCTION 'SVRS_GET_REPS_FROM_OBJECT'
      EXPORTING
        object_name = is_vrsd-objname
        object_type = is_vrsd-objtype
        versno      = is_vrsd-versno
      TABLES
        repos_tab   = lt_repos
        trdir_tab   = lt_trdir
      EXCEPTIONS
        no_version  = 1
        OTHERS      = 2.
    IF sy-subrc = 0.
      rv_source = concat_lines_of( table = lt_repos
                                   sep   = |\n| ).
    ENDIF.

  ENDMETHOD.

  METHOD read_program.

    DATA lt_repos TYPE STANDARD TABLE OF abaptxt255 WITH EMPTY KEY.
    DATA lt_trdir TYPE STANDARD TABLE OF trdir WITH EMPTY KEY.

* unlike read_reps( ) a reader failure is not taken as a missing version
    CALL FUNCTION 'SVRS_GET_REPS_FROM_OBJECT'
      EXPORTING
        object_name = is_vrsd-objname
        object_type = is_vrsd-objtype
        versno      = is_vrsd-versno
      TABLES
        repos_tab   = lt_repos
        trdir_tab   = lt_trdir
      EXCEPTIONS
        no_version  = 1
        OTHERS      = 2.
    IF sy-subrc = 1.
      RETURN.
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Unable to read historical { is_vrsd-objtype } { is_vrsd-objname } | &&
        |version { is_vrsd-versno }, subrc { sy-subrc }| ).
    ENDIF.

    READ TABLE lt_trdir INTO rs_program-trdir INDEX 1.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise(
        |Program attributes missing in historical { is_vrsd-objtype } { is_vrsd-objname } version { is_vrsd-versno }| ).
    ENDIF.

    rs_program-source = concat_lines_of( table = lt_repos
                                         sep   = |\n| ).

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

  ENDMETHOD.

  METHOD read_release.

    SELECT SINGLE trkorr, as4date, as4time FROM e070 INTO @rs_release
      WHERE trkorr = @iv_trkorr.
    IF sy-subrc <> 0.
      CLEAR rs_release.
    ENDIF.

  ENDMETHOD.

  METHOD is_released_after.

    rv_after = xsdbool( is_release-as4date > is_reference-as4date
      OR ( is_release-as4date = is_reference-as4date AND is_release-as4time > is_reference-as4time )
      OR ( is_release-as4date = is_reference-as4date AND is_release-as4time = is_reference-as4time
        AND is_release-trkorr > is_reference-trkorr ) ).

  ENDMETHOD.

  METHOD read_snapshot.

    TYPES:
      BEGIN OF ty_row,
        objtype TYPE vrsd-objtype,
        objname TYPE vrsd-objname,
        versno  TYPE vrsd-versno,
        korrnum TYPE vrsd-korrnum,
        author  TYPE vrsd-author,
        datum   TYPE vrsd-datum,
        zeit    TYPE vrsd-zeit,
        as4date TYPE e070-as4date,
        as4time TYPE e070-as4time,
      END OF ty_row.
    DATA lt_rows TYPE SORTED TABLE OF ty_row WITH NON-UNIQUE KEY objtype objname versno.

    IF lines( it_parts ) = 0.
      RETURN.
    ENDIF.

    DATA(ls_transport) = read_release( iv_korrnum ).
    IF ls_transport IS INITIAL.
      RETURN.
    ENDIF.

* versions without a released request, like local ones, never reached the repository
    SELECT vrsd~objtype, vrsd~objname, vrsd~versno, vrsd~korrnum, vrsd~author, vrsd~datum, vrsd~zeit,
        e070~as4date, e070~as4time
      FROM vrsd INNER JOIN e070 ON e070~trkorr = vrsd~korrnum
      INTO TABLE @lt_rows
      FOR ALL ENTRIES IN @it_parts
      WHERE vrsd~objtype = @it_parts-objtype
      AND vrsd~objname = @it_parts-objname.

* same order as the extraction processes transports in, so later transports never leak in
    LOOP AT lt_rows INTO DATA(ls_row).
      IF is_released_after(
          is_release   = VALUE #(
            trkorr  = ls_row-korrnum
            as4date = ls_row-as4date
            as4time = ls_row-as4time )
          is_reference = ls_transport ) = abap_true.
        CONTINUE.
      ENDIF.
* rows come in ascending version order, so the last row kept per part is its newest version
      READ TABLE rt_vrsd ASSIGNING FIELD-SYMBOL(<ls_vrsd>)
        WITH KEY objtype = ls_row-objtype objname = ls_row-objname.
      IF sy-subrc <> 0.
        INSERT INITIAL LINE INTO TABLE rt_vrsd ASSIGNING <ls_vrsd>.
      ENDIF.
      <ls_vrsd> = CORRESPONDING #( ls_row ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
