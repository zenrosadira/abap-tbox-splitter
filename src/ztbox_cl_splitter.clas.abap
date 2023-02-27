class ZTBOX_CL_SPLITTER definition
  public
  final
  create public .

public section.

  interfaces IF_SERIALIZABLE_OBJECT .

  methods GET_PART
    importing
      !IV_PART type INT4
    changing
      !CT_TABLE type TABLE .
  methods SPLIT_BY_PART
    importing
      !I_PARTS type INT4 .
  methods SPLIT_BY_SIZE
    importing
      !I_SIZE type INT4 .
  methods COUNT
    returning
      value(R_COUNT) type I .
  methods CONSTRUCTOR
    importing
      !I_MAIN_TABLE type TABLE optional .
protected section.
private section.

  types:
    BEGIN OF ty_parts,
      part TYPE REF TO data,
    END OF ty_parts .
  types:
    ty_parts_t TYPE TABLE OF ty_parts .
  types:
    BEGIN OF ty_parts_guid,
      guid TYPE sysuuid_c36,
    END OF ty_parts_guid .
  types:
    ty_parts_guid_t TYPE TABLE OF ty_parts_guid .
  types:
    BEGIN OF ty_parts_ix,
      part       TYPE i,
      table_part TYPE REF TO data,
    END OF ty_parts_ix .
  types:
    ty_parts_ix_t TYPE HASHED TABLE OF ty_parts_ix WITH UNIQUE KEY part .

  data MAIN_TABLE type ref to DATA .
  data TABLE_PARTS type TY_PARTS_T .
  data CLASS_PERFORMER type SEOCLSNAME .
  data METHOD_PERFORMER type SEOCMPNAME .
  data PARTS_GUID type TY_PARTS_GUID_T .
  data CURRENT_PART type INT4 .
  data PARTS_IX type TY_PARTS_IX_T .
  data SEGMENT type INT4 .
  data PARTS type INT4 .
  data SPLIT_TYPE type C .
  data TOTAL_LINES type INT4 .
  data COMPUTIME type INT4 .

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

    DATA(part) = VALUE #( parts_ix[ part = iv_part ] OPTIONAL ).
    CHECK part IS NOT INITIAL.

    CHECK part-table_part IS BOUND.
    ASSIGN part-table_part->* TO FIELD-SYMBOL(<part>).

    CHECK <part> IS ASSIGNED.

    ct_table = <part>.

  ENDMETHOD.


  METHOD SPLIT_BY_PART.

    CLEAR parts_ix.

    parts       = i_parts.
    segment     = total_lines DIV parts.
    split_type  = 'P'.

    _split_table( ).

  ENDMETHOD.


  METHOD SPLIT_BY_SIZE.

    CLEAR parts_ix.

    segment     = i_size.
    split_type  = 'S'.

    _split_table( ).

  ENDMETHOD.


  METHOD _GET_POSITION.

    CASE split_type.

      WHEN 'P'. " Split by part

        r_pos = COND #(
          WHEN i_index MOD segment EQ 0
            THEN nmin( val1 = i_index DIV segment     val2 = parts )
            ELSE nmin( val1 = i_index DIV segment + 1 val2 = parts ) ).

      WHEN 'S'. " Split by package size

        r_pos = COND #(
          WHEN i_index MOD segment EQ 0
            THEN i_index DIV segment
            ELSE i_index DIV segment + 1 ).

    ENDCASE.

  ENDMETHOD.


  METHOD _split_table.

    GET TIME STAMP FIELD DATA(start).

    FIELD-SYMBOLS <main> TYPE ANY TABLE.
    FIELD-SYMBOLS <part> TYPE ANY TABLE.

    DATA part LIKE LINE OF parts_ix.

    CHECK main_table IS BOUND.
    ASSIGN main_table->* TO <main>.

    CHECK <main> IS ASSIGNED.

    LOOP AT <main> ASSIGNING FIELD-SYMBOL(<row>).

      DATA(ix)   = _get_position( sy-tabix ).

      READ TABLE parts_ix ASSIGNING FIELD-SYMBOL(<part_ix>) WITH TABLE KEY part = ix.
      CASE sy-subrc.

        WHEN 0.

          CHECK <part_ix>-table_part IS BOUND.
          ASSIGN <part_ix>-table_part->* TO <part>.
          CHECK <part> IS ASSIGNED.

          INSERT <row> INTO TABLE <part>.

        WHEN OTHERS.

          IF <part> IS ASSIGNED.
            UNASSIGN <part>.
          ENDIF.

          CLEAR part.
          CREATE DATA part-table_part LIKE <main>.
          ASSIGN part-table_part->* TO <part>.
          CHECK <part> IS ASSIGNED.

          INSERT <row> INTO TABLE <part>.

          part-part = ix.
          INSERT part INTO TABLE parts_ix.

      ENDCASE.

    ENDLOOP.

    GET TIME STAMP FIELD DATA(end).

    computime = end - start.

  ENDMETHOD.


  METHOD count.

    r_count = lines( parts_ix ).

  ENDMETHOD.
ENDCLASS.
