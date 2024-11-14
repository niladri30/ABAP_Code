REPORT z_alv_demo.

" Data declaration
TABLES: mara.

TYPES: BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
         ersda TYPE mara-ersda,
         mtart TYPE mara-mtart,
         meins TYPE mara-meins,
         maktx TYPE makt-maktx,
       END OF ty_mara.

DATA: it_mara TYPE TABLE OF ty_mara,
      wa_mara TYPE ty_mara.

DATA: gr_alv TYPE REF TO cl_salv_table.

START-OF-SELECTION.

  " Select data from MARA table
  SELECT matnr ersda mtart meins
    INTO CORRESPONDING FIELDS OF TABLE it_mara
    FROM mara
    UP TO 100 ROWS.

  IF it_mara IS NOT INITIAL.
    " Get descriptions from MAKT table for each material
    LOOP AT it_mara INTO wa_mara.
      SELECT SINGLE maktx INTO wa_mara-maktx
        FROM makt
        WHERE matnr = wa_mara-matnr
          AND spras = sy-langu.
      MODIFY it_mara FROM wa_mara TRANSPORTING maktx WHERE matnr = wa_mara-matnr.
    ENDLOOP.
  ENDIF.

  " Display ALV
  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = gr_alv
                              CHANGING  t_table      = it_mara ).

      gr_alv->display( ).
    CATCH cx_salv_msg INTO DATA(lx_msg).
      MESSAGE lx_msg->get_text( ) TYPE 'E'.
  ENDTRY.
