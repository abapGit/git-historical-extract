CLASS zcl_abapgit_hist_clas_source DEFINITION
  PUBLIC
  CREATE PUBLIC .

* assembles the abapGit class files from the historical class includes, kept free of
* database access so the logic can be tested without a system
  PUBLIC SECTION.

    TYPES ty_names TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line .
    TYPES:
      BEGIN OF ty_declarations,
        methods    TYPE ty_names,
        interfaces TYPE ty_names,
      END OF ty_declarations .
    TYPES:
      BEGIN OF ty_method,
        name   TYPE string,
        source TYPE string,
      END OF ty_method .
    TYPES ty_methods TYPE SORTED TABLE OF ty_method WITH UNIQUE KEY name .

* the names declared by METHODS, CLASS-METHODS and INTERFACES statements, upper case
    CLASS-METHODS parse_declarations
      IMPORTING
        iv_source              TYPE string
      RETURNING
        VALUE(rs_declarations) TYPE ty_declarations .

* true if the method is declared in the class itself or belongs to a declared interface
    CLASS-METHODS is_declared
      IMPORTING
        is_declarations    TYPE ty_declarations
        iv_method          TYPE string
      RETURNING
        VALUE(rv_declared) TYPE abap_bool .

* false for includes that only hold the comments SAP generates into every class include
    CLASS-METHODS has_content
      IMPORTING
        iv_source         TYPE string
      RETURNING
        VALUE(rv_content) TYPE abap_bool .

    CLASS-METHODS build_main_source
      IMPORTING
        iv_class_name    TYPE string
        iv_public        TYPE string
        iv_protected     TYPE string
        iv_private       TYPE string
        it_methods       TYPE ty_methods
      RETURNING
        VALUE(rv_source) TYPE string .

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS remove_comments_and_literals
      IMPORTING
        iv_source       TYPE string
      RETURNING
        VALUE(rv_clean) TYPE string .
ENDCLASS.



CLASS ZCL_ABAPGIT_HIST_CLAS_SOURCE IMPLEMENTATION.


  METHOD build_main_source.

* the includes of the three sections hold the definition, the class pool adds ENDCLASS
    DATA(lv_definition) = iv_public.
    IF iv_protected IS NOT INITIAL.
      lv_definition = |{ lv_definition }\n{ iv_protected }|.
    ENDIF.
    IF iv_private IS NOT INITIAL.
      lv_definition = |{ lv_definition }\n{ iv_private }|.
    ENDIF.

* tolerate a release that keeps ENDCLASS in the private section include
    SPLIT lv_definition AT |\n| INTO TABLE DATA(lt_lines).
    DATA(lv_ends_with_endclass) = abap_false.
    DATA(lv_index) = lines( lt_lines ).
    WHILE lv_index > 0.
      DATA(lv_line) = to_upper( condense( lt_lines[ lv_index ] ) ).
      IF lv_line IS NOT INITIAL.
        lv_ends_with_endclass = xsdbool( lv_line CP 'ENDCLASS.*' ).
        EXIT.
      ENDIF.
      lv_index = lv_index - 1.
    ENDWHILE.

    IF lv_ends_with_endclass = abap_true.
      rv_source = |{ lv_definition }\n|.
    ELSE.
      rv_source = |{ lv_definition }\nENDCLASS.\n|.
    ENDIF.

* the layout SE24 and abapGit give a class, methods in alphabetical order
    rv_source = |{ rv_source }\n\n\nCLASS { iv_class_name } IMPLEMENTATION.\n|.
    LOOP AT it_methods INTO DATA(ls_method).
      DATA(lv_method) = shift_right( val = ls_method-source
                                     sub = |\n| ).
      rv_source = |{ rv_source }\n\n{ lv_method }\n|.
    ENDLOOP.
    rv_source = |{ rv_source }ENDCLASS.\n|.

  ENDMETHOD.


  METHOD has_content.

* same rule as abapGit's serializer uses to skip the local includes
    SPLIT iv_source AT |\n| INTO TABLE DATA(lt_lines).
    LOOP AT lt_lines INTO DATA(lv_line).
      IF strlen( lv_line ) >= 3 AND lv_line(3) <> '*"*'.
        rv_content = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD is_declared.

    IF line_exists( is_declarations-methods[ table_line = iv_method ] ).
      rv_declared = abap_true.
    ELSEIF iv_method CS '~'.
      DATA(lv_interface) = segment( val   = iv_method
                                    sep   = '~'
                                    index = 1 ).
      rv_declared = xsdbool( line_exists( is_declarations-interfaces[ table_line = lv_interface ] ) ).
    ENDIF.

  ENDMETHOD.


  METHOD parse_declarations.

    DATA lv_prefix TYPE string.
    DATA lv_chain  TYPE string.
    DATA lt_tokens TYPE STANDARD TABLE OF string WITH EMPTY KEY.

* without comments and literals every period ends a statement and every comma a chain element
    SPLIT remove_comments_and_literals( iv_source ) AT '.' INTO TABLE DATA(lt_statements).

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
        IF lines( lt_tokens ) < 2.
          CONTINUE.
        ENDIF.
        DATA(lv_name) = lt_tokens[ 2 ].
        IF strlen( lv_name ) > 1 AND lv_name(1) = '!'.
          lv_name = lv_name+1.
        ENDIF.
        CASE lt_tokens[ 1 ].
          WHEN 'METHODS' OR 'CLASS-METHODS'.
            INSERT lv_name INTO TABLE rs_declarations-methods.
          WHEN 'INTERFACES'.
            INSERT lv_name INTO TABLE rs_declarations-interfaces.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD remove_comments_and_literals.

    DATA lv_quote TYPE string.

    SPLIT iv_source AT |\n| INTO TABLE DATA(lt_lines).
    LOOP AT lt_lines INTO DATA(lv_line).
      IF strlen( lv_line ) > 0 AND lv_line(1) = '*'.
        CONTINUE.
      ENDIF.

      CLEAR lv_quote.
      DO strlen( lv_line ) TIMES.
        DATA(lv_char) = substring(
          val = lv_line
          off = sy-index - 1
          len = 1 ).
        IF lv_quote IS NOT INITIAL.
          IF lv_char = lv_quote.
            CLEAR lv_quote.
          ENDIF.
          CONTINUE.
        ENDIF.
        CASE lv_char.
          WHEN `'` OR '`' OR '|'.
            lv_quote = lv_char.
            rv_clean = |{ rv_clean } LITERAL |.
          WHEN '"'.
            EXIT.
          WHEN cl_abap_char_utilities=>horizontal_tab.
            rv_clean = |{ rv_clean } |.
          WHEN OTHERS.
            rv_clean = rv_clean && lv_char.
        ENDCASE.
      ENDDO.
      rv_clean = |{ rv_clean } |.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
