*&---------------------------------------------------------------------*
*& Report ZSDCRM_INIT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsdcrm_init.
DATA:lt_head TYPE TABLE OF ztcrm_so_head,
     lt_item TYPE TABLE OF ztcrm_so_item.

START-OF-SELECTION.

  SELECT *
    FROM ztcrm_so_init AS zinit
    WHERE NOT EXISTS ( SELECT * FROM ztcrm_so_item WHERE new_contractid = zinit~new_contractid AND new_contractdetailid = zinit~new_contractdetailid )
    INTO TABLE @DATA(lt_init)
  .
  IF lt_init IS INITIAL.
    MESSAGE '没有可初始化的数据' TYPE 'E'.
  ENDIF.

  CLEAR:lt_head,lt_item.
  LOOP AT lt_init ASSIGNING FIELD-SYMBOL(<lt_init>).
    <lt_init>-new_contractid = to_lower( <lt_init>-new_contractid ).
    <lt_init>-new_contractdetailid = to_lower( <lt_init>-new_contractdetailid ).
    <lt_init>-vbeln = |{ <lt_init>-vbeln ALPHA = IN }|.
    <lt_init>-posnr = |{ <lt_init>-posnr ALPHA = IN }|.
    INSERT INITIAL LINE INTO TABLE lt_head ASSIGNING FIELD-SYMBOL(<lt_head>).
    <lt_head>-new_contractid = <lt_init>-new_contractid.
    <lt_head>-vbeln = <lt_init>-vbeln.
    INSERT INITIAL LINE INTO TABLE lt_item ASSIGNING FIELD-SYMBOL(<lt_item>).
    <lt_item>-new_contractid = <lt_init>-new_contractid.
    <lt_item>-new_contractdetailid = <lt_init>-new_contractdetailid.
    <lt_item>-posnr = <lt_init>-posnr.
    UPDATE vbak SET new_contractid = <lt_init>-new_contractid WHERE vbeln = <lt_init>-vbeln.
    UPDATE vbap SET new_contractdetailid = <lt_init>-new_contractdetailid WHERE vbeln = <lt_init>-vbeln AND posnr = <lt_init>-posnr.
  ENDLOOP.
  IF sy-subrc EQ 0.
    MODIFY ztcrm_so_init FROM TABLE lt_init.
    MODIFY ztcrm_so_head FROM TABLE lt_head.
    MODIFY ztcrm_so_item FROM TABLE lt_item.
  ENDIF.
  COMMIT WORK.
  WRITE:/ |本次处理条目数{ lines( lt_init ) }条|.
