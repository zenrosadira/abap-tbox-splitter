class ZTBOX_CL_SPLITTER definition
  public
  final
  create public .

public section.

  interfaces IF_SERIALIZABLE_OBJECT .

  methods GET_PART
    importing
      !I_PART type I
    changing
      !CT_TABLE type TABLE .
  methods SPLIT_BY_PART
    importing
      !I_PARTS type I .
  methods SPLIT_BY_SIZE
    importing
      !I_SIZE type I .
  methods COUNT
    returning
      value(R_COUNT) type I .
  methods CONSTRUCTOR
    importing
      !I_MAIN_TABLE type TABLE optional .
protected section.
private section.

  types:
    BEGIN OF ty_parts_ix,
      part       TYPE i,
      table_part TYPE REF TO data,
    END OF ty_parts_ix .
  types:
    ty_parts_ix_t TYPE HASHED TABLE OF ty_parts_ix WITH UNIQUE KEY part .

  data MAIN_TABLE type ref to DATA .
  data PARTS_IX type TY_PARTS_IX_T .
  data SEGMENT type I .
  data PARTS type I .
  data SPLIT_TYPE type C .
  data TOTAL_LINES type I .
  constants C_SPLIT_BY_PART type C value 'P' ##NO_TEXT.
  constants C_SPLIT_BY_SIZE type C value 'S' ##NO_TEXT.

  methods _SPLIT_TABLE .
  methods _GET_POSITION
    importing
      !I_INDEX type INT4
    returning
      value(R_POS) type INT4 .
ENDCLASS.



CLASS ZTBOX_CL_SPLITTER IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    CREATE DATA main_table LIKE i_main_table.
    GET REFERENCE OF i_main_table INTO main_table.

    total_lines = lines( i_main_table ).

  ENDMETHOD.


  METHOD get_part.

    DATA(part) = VALUE #( parts_ix[ part = i_part ] OPTIONAL ).
    CHECK part IS NOT INITIAL.

    CHECK part-table_part IS BOUND.
    ASSIGN part-table_part->* TO FIELD-SYMBOL(<part>).

    CHECK <part> IS ASSIGNED.

    ct_table = <part>.

  ENDMETHOD.


  METHOD split_by_part.

    CHECK i_parts BETWEEN 1 AND total_lines.

    parts       = i_parts.
    segment     = total_lines DIV parts.
    split_type  = c_split_by_part.

    _split_table( ).

  ENDMETHOD.


  METHOD split_by_size.

    CHECK i_size BETWEEN 1 AND total_lines.

    segment     = i_size.
    split_type  = c_split_by_size.

    _split_table( ).

  ENDMETHOD.


  METHOD _get_position.

    CASE split_type.

      WHEN c_split_by_part.

        r_pos = COND #(
          WHEN i_index MOD segment EQ 0
            THEN nmin( val1 = i_index DIV segment     val2 = parts )
            ELSE nmin( val1 = i_index DIV segment + 1 val2 = parts ) ).

      WHEN c_split_by_size.

        r_pos = COND #(
          WHEN i_index MOD segment EQ 0
            THEN i_index DIV segment
            ELSE i_index DIV segment + 1 ).

    ENDCASE.

  ENDMETHOD.


  METHOD _split_table.

    CHECK main_table IS BOUND.

    CLEAR parts_ix.
    LOOP AT main_table->* ASSIGNING FIELD-SYMBOL(<row>).

      DATA(ix)   = _get_position( sy-tabix ).

      READ TABLE parts_ix ASSIGNING FIELD-SYMBOL(<part_ix>) WITH TABLE KEY part = ix.
      CASE sy-subrc.

        WHEN 0.

          INSERT <row> INTO TABLE <part_ix>-table_part->*.

        WHEN OTHERS.

          DATA(part) = VALUE ty_parts_ix( part = ix ).
          CREATE DATA part-table_part LIKE main_table->*.

          INSERT <row>  INTO TABLE part-table_part->*.
          INSERT part   INTO TABLE parts_ix.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD count.

    r_count = lines( parts_ix ).

  ENDMETHOD.
ENDCLASS.
