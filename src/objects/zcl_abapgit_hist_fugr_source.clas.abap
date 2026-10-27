CLASS zcl_abapgit_hist_fugr_source DEFINITION
  PUBLIC
  CREATE PUBLIC .

* assembles the parts of a historical function group, kept free of database access so the
* logic can be tested without a system
  PUBLIC SECTION.

    TYPES ty_names TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line .

* a row of the parameter table the function module version reader returns
    TYPES:
      BEGIN OF ty_parameter,
        parameter  TYPE string,
        paramtype  TYPE string,
        pposition  TYPE i,
        structure  TYPE string,
        defaultval TYPE string,
        reference  TYPE string,
        optional   TYPE string,
        type       TYPE string,
        class      TYPE string,
        ref_class  TYPE string,
        line_of    TYPE string,
        table_of   TYPE string,
      END OF ty_parameter .
    TYPES ty_parameters TYPE STANDARD TABLE OF ty_parameter WITH EMPTY KEY .
* a parameter short text in the original language
    TYPES:
      BEGIN OF ty_text,
        parameter TYPE string,
        kind      TYPE string,
        stext     TYPE string,
      END OF ty_text .
    TYPES ty_texts TYPE STANDARD TABLE OF ty_text WITH EMPTY KEY .

* the function module interface as abapGit writes it, components named as in RSIMP, RSCHA,
* RSEXP, RSTBL, RSEXC and RSFDO
    TYPES:
      BEGIN OF ty_importing,
        parameter TYPE string,
        dbfield   TYPE string,
        default   TYPE string,
        types     TYPE string,
        optional  TYPE string,
        reference TYPE string,
        typ       TYPE string,
        class     TYPE string,
        ref_class TYPE string,
        line_of   TYPE string,
        table_of  TYPE string,
      END OF ty_importing .
    TYPES ty_importing_tt TYPE STANDARD TABLE OF ty_importing WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_exporting,
        parameter TYPE string,
        dbfield   TYPE string,
        types     TYPE string,
        reference TYPE string,
        typ       TYPE string,
        class     TYPE string,
        ref_class TYPE string,
        line_of   TYPE string,
        table_of  TYPE string,
      END OF ty_exporting .
    TYPES ty_exporting_tt TYPE STANDARD TABLE OF ty_exporting WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_tables,
        parameter TYPE string,
        dbstruct  TYPE string,
        types     TYPE string,
        optional  TYPE string,
        typ       TYPE string,
        class     TYPE string,
        ref_class TYPE string,
        line_of   TYPE string,
        table_of  TYPE string,
      END OF ty_tables .
    TYPES ty_tables_tt TYPE STANDARD TABLE OF ty_tables WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_exception,
        exception TYPE string,
      END OF ty_exception .
    TYPES ty_exception_tt TYPE STANDARD TABLE OF ty_exception WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_documentation,
        parameter TYPE string,
        kind      TYPE string,
        stext     TYPE string,
      END OF ty_documentation .
    TYPES ty_documentation_tt TYPE STANDARD TABLE OF ty_documentation WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_interface,
        import        TYPE ty_importing_tt,
        changing      TYPE ty_importing_tt,
        export        TYPE ty_exporting_tt,
        tables        TYPE ty_tables_tt,
        exception     TYPE ty_exception_tt,
        documentation TYPE ty_documentation_tt,
      END OF ty_interface .

* SAPL followed by the group name, behind the namespace of a namespaced group
    CLASS-METHODS get_main_program
      IMPORTING
        iv_group          TYPE csequence
      RETURNING
        VALUE(rv_program) TYPE string .

* the L every include name of the group starts with, behind the namespace of a namespaced group
    CLASS-METHODS get_include_prefix
      IMPORTING
        iv_group         TYPE csequence
      RETURNING
        VALUE(rv_prefix) TYPE string .

* the programs named by INCLUDE statements, upper case
    CLASS-METHODS parse_includes
      IMPORTING
        iv_source          TYPE string
      RETURNING
        VALUE(rt_includes) TYPE ty_names .

* false for includes abapGit does not write as include files of the group: function module
* includes and their UXX directory, generated maintenance includes of other groups, simple
* transformation includes, and includes of other function groups
    CLASS-METHODS is_group_include
      IMPORTING
        iv_group         TYPE csequence
        iv_include       TYPE csequence
      RETURNING
        VALUE(rv_member) TYPE abap_bool .

* drops the generation timestamps of maintenance view sources, as abapGit does
    CLASS-METHODS strip_generation_comments
      IMPORTING
        iv_source        TYPE string
      RETURNING
        VALUE(rv_source) TYPE string .

    CLASS-METHODS map_interface
      IMPORTING
        it_parameters       TYPE ty_parameters
        it_texts            TYPE ty_texts
      RETURNING
        VALUE(rs_interface) TYPE ty_interface .

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS split_namespace
      IMPORTING
        iv_group     TYPE csequence
      EXPORTING
        ev_namespace TYPE string
        ev_name      TYPE string .
ENDCLASS.



CLASS ZCL_ABAPGIT_HIST_FUGR_SOURCE IMPLEMENTATION.


  METHOD get_include_prefix.

    split_namespace( EXPORTING iv_group     = iv_group
                     IMPORTING ev_namespace = DATA(lv_namespace)
                               ev_name      = DATA(lv_name) ).
    rv_prefix = |{ lv_namespace }L{ lv_name }|.

  ENDMETHOD.


  METHOD get_main_program.

    split_namespace( EXPORTING iv_group     = iv_group
                     IMPORTING ev_namespace = DATA(lv_namespace)
                               ev_name      = DATA(lv_name) ).
    rv_program = |{ lv_namespace }SAPL{ lv_name }|.

  ENDMETHOD.


  METHOD is_group_include.

    DATA(lv_include) = to_upper( iv_include ).
    DATA(lv_prefix) = get_include_prefix( to_upper( iv_group ) ).

    IF lv_include CP 'LSVIM*'.
      RETURN.
    ELSEIF strlen( lv_include ) = 33 AND substring( val = lv_include
                                                    off = 30
                                                    len = 3 ) = 'XTI'.
      RETURN.
    ELSEIF strlen( lv_include ) >= strlen( lv_prefix )
        AND substring( val = lv_include
                       len = strlen( lv_prefix ) ) = lv_prefix.
* the includes of the function modules are named U and a two character number, the UXX include
* lists them, both are generated from the function modules
      rv_member = xsdbool( NOT ( strlen( lv_include ) = strlen( lv_prefix ) + 3
        AND substring( val = lv_include
                       off = strlen( lv_prefix )
                       len = 1 ) = 'U' ) ).
    ELSE.
* an include named like the includes of a function group belongs to another group
      rv_member = xsdbool( NOT ( lv_include CP 'L*' OR lv_include CP '/*/L*' ) ).
    ENDIF.

  ENDMETHOD.


  METHOD map_interface.

    DATA lt_parameters TYPE ty_parameters.


    lt_parameters = it_parameters.
    SORT lt_parameters BY pposition parameter.

* the documentation lists every parameter, importing, exporting, changing, tables and then the
* exceptions, which is the order abapGit writes for nearly every function module
    LOOP AT VALUE string_table( ( `I` ) ( `E` ) ( `C` ) ( `T` ) ( `X` ) ) INTO DATA(lv_paramtype).
      LOOP AT lt_parameters INTO DATA(ls_parameter) WHERE paramtype = lv_paramtype.
* LIKE references a dictionary field, TYPE a type, flagged by the version reader
        DATA(lv_like) = xsdbool( ls_parameter-type IS INITIAL ).
        DATA(lv_types) = COND string( WHEN ls_parameter-type <> 'X' THEN ls_parameter-type ).

        CASE lv_paramtype.
          WHEN 'I' OR 'C'.
            DATA(ls_importing) = VALUE ty_importing(
              parameter = ls_parameter-parameter
              dbfield   = COND #( WHEN lv_like = abap_true THEN ls_parameter-structure )
              default   = ls_parameter-defaultval
              types     = lv_types
              optional  = ls_parameter-optional
              reference = ls_parameter-reference
              typ       = COND #( WHEN lv_like = abap_false THEN ls_parameter-structure )
              class     = ls_parameter-class
              ref_class = ls_parameter-ref_class
              line_of   = ls_parameter-line_of
              table_of  = ls_parameter-table_of ).
            IF lv_paramtype = 'I'.
              INSERT ls_importing INTO TABLE rs_interface-import.
            ELSE.
              INSERT ls_importing INTO TABLE rs_interface-changing.
            ENDIF.
          WHEN 'E'.
            INSERT VALUE #(
              parameter = ls_parameter-parameter
              dbfield   = COND #( WHEN lv_like = abap_true THEN ls_parameter-structure )
              types     = lv_types
              reference = ls_parameter-reference
              typ       = COND #( WHEN lv_like = abap_false THEN ls_parameter-structure )
              class     = ls_parameter-class
              ref_class = ls_parameter-ref_class
              line_of   = ls_parameter-line_of
              table_of  = ls_parameter-table_of ) INTO TABLE rs_interface-export.
          WHEN 'T'.
            INSERT VALUE #(
              parameter = ls_parameter-parameter
              dbstruct  = COND #( WHEN lv_like = abap_true THEN ls_parameter-structure )
              types     = lv_types
              optional  = ls_parameter-optional
              typ       = COND #( WHEN lv_like = abap_false THEN ls_parameter-structure )
              class     = ls_parameter-class
              ref_class = ls_parameter-ref_class
              line_of   = ls_parameter-line_of
              table_of  = ls_parameter-table_of ) INTO TABLE rs_interface-tables.
          WHEN 'X'.
            INSERT VALUE #( exception = ls_parameter-parameter ) INTO TABLE rs_interface-exception.
        ENDCASE.

        DATA(lv_kind) = COND string( WHEN lv_paramtype = 'X' THEN `X` ELSE `P` ).
        READ TABLE it_texts INTO DATA(ls_text)
          WITH KEY parameter = ls_parameter-parameter kind = lv_kind.
        IF sy-subrc <> 0.
          CLEAR ls_text.
        ENDIF.
        INSERT VALUE #(
          parameter = ls_parameter-parameter
          kind      = lv_kind
          stext     = ls_text-stext ) INTO TABLE rs_interface-documentation.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD parse_includes.

    DATA lv_prefix TYPE string.
    DATA lv_chain  TYPE string.
    DATA lt_tokens TYPE STANDARD TABLE OF string WITH EMPTY KEY.

* without comments and literals every period ends a statement and every comma a chain element
    SPLIT zcl_abapgit_hist_clas_source=>remove_comments_and_literals( iv_source )
      AT '.' INTO TABLE DATA(lt_statements).

    LOOP AT lt_statements INTO DATA(lv_statement).
      IF lv_statement CA ':'.
        SPLIT lv_statement AT ':' INTO lv_prefix lv_chain.
      ELSE.
        CLEAR lv_prefix.
        lv_chain = lv_statement.
      ENDIF.

      SPLIT lv_chain AT ',' INTO TABLE DATA(lt_elements).
      LOOP AT lt_elements INTO DATA(lv_element).
        DATA(lv_single) = to_upper( condense( |{ lv_prefix } { lv_element }| ) ).
        SPLIT lv_single AT space INTO TABLE lt_tokens.
        IF lines( lt_tokens ) < 2 OR lt_tokens[ 1 ] <> 'INCLUDE'.
          CONTINUE.
        ENDIF.
* INCLUDE STRUCTURE and INCLUDE TYPE take components into a structure, not a program
        DATA(lv_name) = lt_tokens[ 2 ].
        IF lv_name <> 'STRUCTURE' AND lv_name <> 'TYPE'.
          INSERT lv_name INTO TABLE rt_includes.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD split_namespace.

    CLEAR: ev_namespace, ev_name.

    ev_name = iv_group.
    IF strlen( ev_name ) > 1 AND ev_name(1) = '/'.
      DATA(lv_end) = find( val = ev_name
                           sub = '/'
                           off = 1 ).
      IF lv_end > 0.
        ev_namespace = substring( val = ev_name
                                  len = lv_end + 1 ).
        ev_name = substring( val = ev_name
                             off = lv_end + 1 ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD strip_generation_comments.

    rv_source = iv_source.
    SPLIT iv_source AT |\n| INTO TABLE DATA(lt_lines).

* the main program and the TOP include of a maintenance view start with a regeneration stamp
    IF lines( lt_lines ) >= 1 AND lt_lines[ 1 ] CP '#**regenerated at *'.
      DELETE lt_lines INDEX 1.
      rv_source = concat_lines_of( table = lt_lines
                                   sep   = |\n| ).
      RETURN.
    ENDIF.

* the other generated includes carry the generation date and generator version in a header
    IF lines( lt_lines ) < 5.
      RETURN.
    ENDIF.
    IF lt_lines[ 1 ] CP '#*---*'
        AND lt_lines[ 2 ] CP '#**'
        AND lt_lines[ 3 ] CP '#**generation date:*'
        AND lt_lines[ 4 ] CP '#**generator version:*'
        AND lt_lines[ 5 ] CP '#*---*'.
      DELETE lt_lines FROM 3 TO 4.
      rv_source = concat_lines_of( table = lt_lines
                                   sep   = |\n| ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
