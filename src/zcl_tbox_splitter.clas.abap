CLASS zcl_tbox_splitter DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_serializable_object.

    METHODS get_part
      IMPORTING i_part   TYPE i
      CHANGING  ct_table TYPE table.

    METHODS split_by_part
      IMPORTING i_parts TYPE i.

    METHODS split_by_size
      IMPORTING i_size TYPE i.

    METHODS count
      RETURNING VALUE(r_count) TYPE i.

    METHODS constructor
      IMPORTING i_main_table TYPE table OPTIONAL.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_parts_ix,
        part       TYPE i,
        table_part TYPE REF TO data,
      END OF ty_parts_ix.
    TYPES ty_parts_ix_t TYPE HASHED TABLE OF ty_parts_ix WITH UNIQUE KEY part.

    DATA main_table  TYPE REF TO data.
    DATA parts_ix    TYPE ty_parts_ix_t.
    DATA segment     TYPE i.
    DATA parts       TYPE i.
    DATA split_type  TYPE c LENGTH 1.
    DATA total_lines TYPE i.

    CONSTANTS c_split_by_part TYPE c LENGTH 1 VALUE 'P' ##NO_TEXT.
    CONSTANTS c_split_by_size TYPE c LENGTH 1 VALUE 'S' ##NO_TEXT.

    METHODS _split_table.

    METHODS _get_position
      IMPORTING i_index      TYPE int4
      RETURNING VALUE(r_pos) TYPE int4.
ENDCLASS.


CLASS zcl_tbox_splitter IMPLEMENTATION.
  METHOD constructor.
    CREATE DATA main_table LIKE i_main_table.
    main_table = REF #( i_main_table ).

    total_lines = lines( i_main_table ).
  ENDMETHOD.

  METHOD count.
    r_count = lines( parts_ix ).
  ENDMETHOD.

  METHOD get_part.
    DATA(part) = VALUE #( parts_ix[ part = i_part ] OPTIONAL ).
    IF part IS INITIAL.
      RETURN.
    ENDIF.

    IF part-table_part IS NOT BOUND.
      RETURN.
    ENDIF.
    ASSIGN part-table_part->* TO FIELD-SYMBOL(<part>).

    IF <part> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    ct_table = <part>.
  ENDMETHOD.

  METHOD split_by_part.
    CHECK i_parts BETWEEN 1 AND total_lines.

    parts      = i_parts.
    segment    = total_lines DIV parts.
    split_type = c_split_by_part.

    _split_table( ).
  ENDMETHOD.

  METHOD split_by_size.
    CHECK i_size BETWEEN 1 AND total_lines.

    segment    = i_size.
    split_type = c_split_by_size.

    _split_table( ).
  ENDMETHOD.

  METHOD _get_position.
    CASE split_type.

      WHEN c_split_by_part.

        r_pos = COND #(
          WHEN i_index MOD segment = 0
          THEN nmin( val1 = i_index DIV segment     val2 = parts )
          ELSE nmin( val1 = i_index DIV segment + 1 val2 = parts ) ).

      WHEN c_split_by_size.

        r_pos = COND #(
          WHEN i_index MOD segment = 0
          THEN i_index DIV segment
          ELSE i_index DIV segment + 1 ).

    ENDCASE.
  ENDMETHOD.

  METHOD _split_table.
    CHECK main_table IS BOUND.

    FIELD-SYMBOLS <part> TYPE ANY TABLE.
    FIELD-SYMBOLS <main> TYPE ANY TABLE.
    ASSIGN main_table->* TO <main>.

    CLEAR parts_ix.
    LOOP AT <main> ASSIGNING FIELD-SYMBOL(<row>).

      DATA(ix) = _get_position( sy-tabix ).

      READ TABLE parts_ix ASSIGNING FIELD-SYMBOL(<part_ix>) WITH TABLE KEY part = ix.
      CASE sy-subrc.

        WHEN 0.

          ASSIGN <part_ix>-table_part->* TO <part>.
          INSERT <row> INTO TABLE <part>.

        WHEN OTHERS.

          DATA(part) = VALUE ty_parts_ix( part = ix ).
          CREATE DATA part-table_part LIKE <main>.
          ASSIGN part-table_part->* TO <part>.

          INSERT <row> INTO TABLE <part>.
          INSERT part  INTO TABLE parts_ix.

      ENDCASE.

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
