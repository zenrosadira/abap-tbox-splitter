# ABAP Splitter

This utility allows you to easily split the rows of a table into multiple tables by performing a split by size (i.e. number of rows) or by parts (i.e. number of output tables)

## Quick Start
```abap
* First, create an instance passing an internal table
DATA(splitter) = NEW zcl_tbox_splitter( t_vbak ).

* Call method SPLIT_BY_PART to split the table into a known number of sub-tables
splitter->split_by_part( 4 ).

* Call method GET_PART to get a single sub-table
splitter->get_part( EXPORTING i_part = 1 CHANGING ct_table  = t_vbak_part_1 ).

* Call method SPLIT_BY_SIZE to split the table into many sub-tables with the same number of rows, 
* and a possible additional sub-table with the remaining rows.
splitter->split_by_size( 10 ).

* Call method COUNT to get the number of parts in which the main table has been splitted.
DATA(parts) = splitter->count( ).
```

If a table with $N$ rows is splitted into $d$ parts, the first $d-1$ sub-tables will have $\left\lfloor\frac{N}{d}\right\rfloor$ records, and the last one $N - (d-1)\cdot\left\lfloor\frac{N}{d}\right\rfloor$.

For both `split_by_part( )` and `split_by_size( )` method the parameter must be an integer between 1 and the lines of the main table.

## Example
```abap
DATA t_vbak_part TYPE TABLE OF vbak.
SELECT * FROM vbak INTO TABLE @DATA(t_vbak).

IF lines( t_vbak ) < 1000.
  PERFORM process_orders USING t_vbak.
  RETURN.
ENDIF.

DATA(splitter) = NEW ztbox_cl_splitter( t_vbak ).

splitter->split_by_size( 1000 ).

DO splitter->count( ) TIMES.
  
  CLEAR t_vbak_part.
  splitter->get_part(
    EXPORTING
      i_part    = sy-index
    CHANGING
      ct_table  = t_vbak_part ).
      
  PERFORM process_orders USING t_vbak_part.

ENDDO.
```

## Installation
Install this project using [abapGit](https://abapgit.org/)
