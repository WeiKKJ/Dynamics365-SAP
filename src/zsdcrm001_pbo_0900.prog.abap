*----------------------------------------------------------------------*
***INCLUDE ZSDCRM001_PBO_0900.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0900 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0900 OUTPUT.
  DATA extab TYPE slis_t_extab.
  CLEAR extab.
  IF gs_out-state = '已处理'.
    APPEND 'SO' TO extab.
  ENDIF.
  SET PF-STATUS 'STA900' EXCLUDING extab.
  SET TITLEBAR 'TIT900'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module INIT_WARNING OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE init_warning OUTPUT.
  IF o_container_warning IS INITIAL.
    o_container_warning = NEW cl_gui_custom_container( container_name = 'CONWARNING' ).
    CREATE OBJECT o_textedit_warning
      EXPORTING
        parent                     = o_container_warning
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_position          = 45
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true
      EXCEPTIONS
        OTHERS                     = 1.
    o_textedit_warning->set_readonly_mode( readonly_mode = 1 ).
    o_textedit_warning->set_statusbar_mode( statusbar_mode = 0 ).
    o_textedit_warning->set_toolbar_mode( toolbar_mode = 0 ).
    DATA(text) = `明细行物料编码的真实物料属性可能与所展示的不一致，严格以VA03界面展示为准！！！`
    && `深红色底色：未选择物料号，深黄色底色：注意核对物料号，深绿色底色：规格匹配，无底色：已生成了合同明细。`.
    o_textedit_warning->set_textstream( text = text ).
  ENDIF.
ENDMODULE.
