*&---------------------------------------------------------------------*
*& Report ZSDCRM001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsdcrm001.
TABLES:tvgrt,vbkd.
INCLUDE zsdcrm001_top.
INCLUDE zlongtext.
INCLUDE zsdcrm001_define.
INCLUDE zsdcrm001_class.



SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE btxt1.
  PARAMETERS p_vkorg TYPE tvko-vkorg DEFAULT '2000' OBLIGATORY.
  SELECT-OPTIONS:s_spart FOR gs_out-spart,
                 s_vkgrp FOR tvgrt-vkgrp,
                 s_bstkd FOR vbkd-bstkd.
  PARAMETERS p_s AS CHECKBOX TYPE char1 DEFAULT 'X' USER-COMMAND ss1.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.
  btxt1 = '数据筛选'(t01).

AT SELECTION-SCREEN. "PAI
  CASE sy-ucomm.
    WHEN 'ONLI'.
      PERFORM auth_check.
  ENDCASE.

INITIALIZATION.

START-OF-SELECTION.
  PERFORM savelog(zreplog) USING sy-repid '' IF FOUND.
  PERFORM init.
  PERFORM getdata.
  PERFORM updatelog(zreplog) IF FOUND.
  PERFORM outdata.

*&---------------------------------------------------------------------*
*&      Form  auth_check
*&---------------------------------------------------------------------*
FORM auth_check.
  AUTHORITY-CHECK OBJECT 'S_TCODE'
  ID 'TCD' FIELD sy-tcode.
  IF sy-subrc <> 0.
    MESSAGE e000(oo) WITH '无事务码权限:'(m01) sy-tcode.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form init
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM init .

ENDFORM.
*&---------------------------------------------------------------------*
*& getdata
*&---------------------------------------------------------------------*
FORM getdata.
  CLEAR gt_out.
  IF p_s = 'X'.
    SELECT
      z~*,
      ag~name1 AS name_ag,
      we~name1 AS name_we,
      tvgrt~bezei AS vkgrp_des
      FROM ztcrm_so_head AS z
      LEFT JOIN kna1 AS ag ON z~kunnr_ag = ag~kunnr
      LEFT JOIN kna1 AS we ON z~kunnr_we = we~kunnr
      LEFT JOIN tvgrt ON z~vkgrp = tvgrt~vkgrp AND tvgrt~spras = @sy-langu
      WHERE z~vkorg = @p_vkorg
      AND z~spart IN @s_spart
      AND z~vkgrp IN @s_vkgrp
      AND z~bstkd IN @s_bstkd
      AND EXISTS ( SELECT * FROM ztcrm_so_item WHERE new_contractid = z~new_contractid AND action NE '' )
      ORDER BY z~new_contractid
      INTO CORRESPONDING FIELDS OF TABLE @gt_out.
  ELSE.
    SELECT
      z~*,
      ag~name1 AS name_ag,
      we~name1 AS name_we,
      tvgrt~bezei AS vkgrp_des
      FROM ztcrm_so_head AS z
      LEFT JOIN kna1 AS ag ON z~kunnr_ag = ag~kunnr
      LEFT JOIN kna1 AS we ON z~kunnr_we = we~kunnr
      LEFT JOIN tvgrt ON z~vkgrp = tvgrt~vkgrp AND tvgrt~spras = @sy-langu
      WHERE z~vkorg = @p_vkorg
      AND z~spart IN @s_spart
      AND z~vkgrp IN @s_vkgrp
      AND z~bstkd IN @s_bstkd
      ORDER BY z~new_contractid
      INTO CORRESPONDING FIELDS OF TABLE @gt_out.
  ENDIF.
  IF gt_out IS INITIAL.
    MESSAGE s000(oo) WITH '无数据'.
    EXIT.
  ENDIF.
*  SELECT
*    domname,
*    domvalue_l,
*    ddtext
*    FROM dd07t
*    WHERE domname IN ( 'ZD_ZISDXS','ZD_ZHTJGFS','ZD_ZHWLX','ZD_ZCPYT','ZD_ZCPSX','ZD_ZISJB' )
*    AND ddlanguage = @sy-langu
*    AND as4local = 'A'
*    ORDER BY domname,domvalue_l
*    INTO TABLE @lt_domdes
*    .
*
*  SELECT vkorg,vtext
*    FROM tvkot
*    WHERE spras = @sy-langu
*    INTO TABLE @DATA(lt_tvkot).
*  SELECT vtweg,vtext
*    FROM tvtwt
*    WHERE spras = @sy-langu
*    INTO TABLE @DATA(lt_tvtwt).
*  SELECT vkbur,bezei
*    FROM tvkbt
*    WHERE spras = @sy-langu
*    INTO TABLE @DATA(lt_tvkbt).
*  SELECT vkgrp,bezei
*    FROM tvgrt
*    WHERE spras = @sy-langu
*    INTO TABLE @DATA(lt_tvgrt).
*  SELECT spart,vtext
*    FROM tspat
*    WHERE spras = @sy-langu
*    INTO TABLE @DATA(lt_tspat).
*  SELECT kdgrp,ktext
*    FROM t151t
*    WHERE spras = @sy-langu
*    INTO TABLE @DATA(lt_t151t).
*  SELECT zid,zname
*    FROM ztsd226
*    FOR ALL ENTRIES IN @gt_out
*    WHERE zid = @gt_out-province
*    OR zid = @gt_out-city
*    OR zid = @gt_out-county
*    INTO TABLE @DATA(lt_226).
*  SELECT auart,bezei
*    FROM tvakt
*    WHERE spras = @sy-langu
*  INTO TABLE @DATA(lt_tvakt).
*  LOOP AT gt_out INTO gs_out.
*    getdomdes 'ZD_ZISDXS' gs_out-zisdxs gs_out-zisdxs_des.
*    getdomdes 'ZD_ZHTJGFS' gs_out-zhtjgfs gs_out-zhtjgfs_des.
*    getdomdes 'ZD_ZHWLX' gs_out-zhwlx gs_out-zhwlx_des.
*    getdomdes 'ZD_ZCPYT' gs_out-zcpyt gs_out-zcpyt_des.
*    getdomdes 'ZD_ZCPSX' gs_out-zcpsx gs_out-zcpsx_des.
*    getdomdes 'ZD_ZISJB' gs_out-zISJB gs_out-zisjb_des.
*    READ TABLE lt_tvkot INTO DATA(wa1) WITH KEY vkorg = gs_out-vkorg.
*    IF sy-subrc EQ 0.
*      gs_out-vkorg_des = wa1-vtext.
*    ENDIF.
*    READ TABLE lt_tvtwt INTO DATA(wa2) WITH KEY vtweg = gs_out-vtweg.
*    IF sy-subrc EQ 0.
*      gs_out-vtweg_des = wa2-vtext.
*    ENDIF.
*    READ TABLE lt_tvkbt INTO DATA(wa3) WITH KEY vkbur = gs_out-vkbur.
*    IF sy-subrc EQ 0.
*      gs_out-vkbur_des = wa3-bezei.
*    ENDIF.
*    READ TABLE lt_tvgrt INTO DATA(wa4) WITH KEY vkgrp = gs_out-vkgrp.
*    IF sy-subrc EQ 0.
*      gs_out-vkgrp_des = wa4-bezei.
*    ENDIF.
*    READ TABLE lt_tspat INTO DATA(wa5) WITH KEY spart = gs_out-spart.
*    IF sy-subrc EQ 0.
*      gs_out-spart_des = wa5-vtext.
*    ENDIF.
*    READ TABLE lt_t151t INTO DATA(wa6) WITH KEY kdgrp = gs_out-kdgrp.
*    IF sy-subrc EQ 0.
*      gs_out-kdgrp_des = wa6-ktext.
*    ENDIF.
*    READ TABLE lt_226 INTO DATA(wa226) WITH KEY zid = gs_out-province.
*    IF sy-subrc EQ 0.
*      gs_out-province_des = wa226-zname.
*    ENDIF.
*    READ TABLE lt_226 INTO wa226 WITH KEY zid = gs_out-city.
*    IF sy-subrc EQ 0.
*      gs_out-city_des = wa226-zname.
*    ENDIF.
*    READ TABLE lt_226 INTO wa226 WITH KEY zid = gs_out-county.
*    IF sy-subrc EQ 0.
*      gs_out-county_des = wa226-zname.
*    ENDIF.
*    READ TABLE lt_tvakt INTO DATA(wa7) WITH KEY auart = gs_out-auart.
*    IF sy-subrc EQ 0.
*      gs_out-auart_des = wa7-bezei.
*    ENDIF.
*    IF gs_out-zisck = 'X'.
*      gs_out-zisck_des = '出口'.
*    ELSE.
*      gs_out-zisck_des = '非出口'.
*    ENDIF.
*    IF gs_out-ztt = 'X'.
*      gs_out-ztt_des = '是'.
*    ELSE.
*      gs_out-ztt_des = '否'.
*    ENDIF.
*    IF gs_out-ziscj = 'X'.
*      gs_out-ziscj_des = '是'.
*    ELSE.
*      gs_out-ziscj_des = '否'.
*    ENDIF.
*    IF gs_out-yl1 = 'X'.
*      gs_out-yl1_des = '是'.
*    ELSE.
*      gs_out-yl1_des = '否'.
*    ENDIF.
*    MODIFY gt_out FROM gs_out.
*  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
* outdata
*---------------------------------------------------------------------*
FORM outdata.
  CHECK gt_out IS NOT INITIAL.
  gv_repid        = sy-repid.
  gs_slayt-zebra  = 'X'.
  gs_slayt-box_fname  = 'SEL'.
  gs_varnt-report = sy-repid.
  gs_varnt-handle = 1.
  DATA dfies_tab LIKE TABLE OF dfies WITH HEADER LINE.
  CLEAR:gt_fldct.
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname   = 'ZTCRM_SO_HEAD'
*     FIELDNAME = ' '
      langu     = sy-langu
*     LFIELDNAME           = ' '
*     ALL_TYPES = ' '
*     GROUP_NAMES          = ' '
*     UCLEN     =
*     DO_NOT_WRITE         = ' '
* IMPORTING
*     X030L_WA  =
*     DDOBJTYPE =
*     DFIES_WA  =
*     LINES_DESCR          =
    TABLES
      dfies_tab = dfies_tab[]
*     FIXED_VALUES         =
* EXCEPTIONS
*     NOT_FOUND = 1
*     INTERNAL_ERROR       = 2
*     OTHERS    = 3
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
  LOOP AT dfies_tab.
    IF dfies_tab-fieldname = 'MANDT' OR dfies_tab-fieldname = 'NEW_CONTRACTID'.
      CONTINUE.
    ENDIF.
    CASE dfies_tab-fieldname.
      WHEN 'VBELN'.
        PERFORM catset TABLES gt_fldct USING:
              dfies_tab-fieldname dfies_tab-tabname dfies_tab-fieldname '合同号'.
      WHEN OTHERS.
        PERFORM catset TABLES gt_fldct USING:
              dfies_tab-fieldname dfies_tab-tabname dfies_tab-fieldname dfies_tab-fieldtext.
    ENDCASE.
  ENDLOOP.
  PERFORM catset TABLES gt_fldct USING:
        'VKGRP_DES'  '' '' '销售组描述'   .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      it_fieldcat_lvc          = gt_fldct
      i_save                   = 'A'
      is_variant               = gs_varnt
      is_layout_lvc            = gs_slayt
      i_callback_program       = gv_repid
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_pf_status_set = 'SET_STATUS'
      i_callback_top_of_page   = 'TOP_OF_PAGEA'
    TABLES
      t_outtab                 = gt_out.
ENDFORM.

*抬头框
FORM top_of_pagea.
  DATA:it_list_commentary TYPE slis_t_listheader,
       wa_list_commentary TYPE slis_listheader,
       sjtms              TYPE i.
  CLEAR:wa_list_commentary,sjtms.
  REFRESH:it_list_commentary.

  sjtms = lines( gt_out ).

  wa_list_commentary-typ = 'S'.
  wa_list_commentary-key = 'CRM销售合同列表条目数:'.
  wa_list_commentary-info = sjtms.
  APPEND wa_list_commentary TO it_list_commentary.


  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = it_list_commentary[]
    EXCEPTIONS
      OTHERS             = 1.
ENDFORM.

*&---------------------------------------------------------------------*
*& set_status
*&---------------------------------------------------------------------*
FORM set_status USING pt_extab TYPE slis_t_extab ##CALLED.
  SET PF-STATUS 'STD_FULL' EXCLUDING pt_extab.
ENDFORM.

*&--------------------------------------------------------------------*
*& ALV user_command
*&--------------------------------------------------------------------*
FORM user_command USING pv_ucomm TYPE sy-ucomm ##CALLED
      pv_field TYPE slis_selfield.
  READ TABLE gt_out INTO gs_out INDEX pv_field-tabindex.
  CASE pv_ucomm.
    WHEN '&IC1'.
      CASE pv_field-fieldname.
        WHEN 'MATNR' OR 'MAKTX'.
*          SET PARAMETER ID 'MXX' FIELD 'K'.
*          SET PARAMETER ID 'MAT' FIELD gs_out-matnr.
*          SET PARAMETER ID 'WRK' FIELD p_werks.
*          CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'REFRE'.
      PERFORM getdata.
      pv_field-row_stable = 'X'.
      pv_field-col_stable = 'X'.
      pv_field-refresh    = 'X'.
    WHEN 'ITEMS'.

      destroy_control_object o_container          .
      destroy_control_object o_splitter           .
      destroy_control_object o_container_tdid     .
      destroy_control_object o_container_longtext .
      destroy_control_object o_tree_longtext      .
      destroy_control_object o_textedit           .
      destroy_control_object o_tree_repository    .
      destroy_control_object o_customcontainer    .
      destroy_control_object item_container    .
      destroy_control_object alv_grid_item     .
      destroy_control_object o_container_warning.
      destroy_control_object o_textedit_warning.
      FREE o_handle_event     .
      FREE o_handle_simple_event     .
      PERFORM getitems.
  ENDCASE.
ENDFORM.

*---------------------------------------------------------------------*
* set fieldcat
*---------------------------------------------------------------------*
FORM catset TABLES t_fldcat
USING pv_field pv_reftab pv_reffld pv_text.
  DATA: ls_fldcat TYPE lvc_s_fcat.

  ls_fldcat-fieldname =  pv_field.    "字段名
  ls_fldcat-scrtext_l =  pv_text.     "长描述
  ls_fldcat-scrtext_s =  pv_text.     "短描述
  ls_fldcat-scrtext_m =  pv_text.     "中描述
  ls_fldcat-coltext   =  pv_text.     "列描述
  ls_fldcat-ref_table =  pv_reftab.   "参考表名
  ls_fldcat-ref_field =  pv_reffld.   "参考字段名
  ls_fldcat-col_opt   = 'A'.          "自动优化列宽

  CASE ls_fldcat-fieldname.
    WHEN 'GSMNG'.
      ls_fldcat-qfieldname = 'MEINS'.
      ls_fldcat-no_zero    = 'X'.
    WHEN 'MENGE'.
      ls_fldcat-qfieldname = 'MEINS'.
      ls_fldcat-no_zero    = 'X'.
    WHEN 'WRBTR'.
      ls_fldcat-cfieldname = 'WAERS'.
    WHEN 'LIFNR' OR 'AUFNR' OR 'KUNNR'.
      ls_fldcat-edit_mask = '==ALPHA'.
    WHEN 'MATNR' OR 'IDNRK'.
      ls_fldcat-edit_mask = '==MATN1'.
    WHEN 'MEINS' .
      ls_fldcat-edit_mask = '==CUNIT'.
  ENDCASE.

  APPEND ls_fldcat TO t_fldcat.
  CLEAR ls_fldcat.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form getitems
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM getitems .
  DATA:posnr TYPE posnr.
  CLEAR:l_ref_alv,msg.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = l_ref_alv.
  CALL METHOD l_ref_alv->get_selected_rows
    IMPORTING
*     et_index_rows =
      et_row_no = DATA(rows).
  IF lines( rows ) NE 1.
    MESSAGE '请选择一行数据' TYPE 'E'.
  ENDIF.
  READ TABLE rows ASSIGNING FIELD-SYMBOL(<row>) INDEX 1.
  READ TABLE gt_out INTO gs_out INDEX <row>-row_id.
  PERFORM get_head USING gs_out-new_contractid.
  CLEAR:gt_item.
  SELECT
    z~*
    FROM ztcrm_so_item AS z
    WHERE z~new_contractid = @gs_out-new_contractid
    AND zhtmxzf = ''
    ORDER BY z~zcrm_num,z~posnr,z~new_contractdetailid
    INTO CORRESPONDING FIELDS OF TABLE @gt_item
    .
  IF gt_item IS INITIAL.
    MESSAGE '无明细数据' TYPE 'E'.
  ENDIF.
  SELECT
    *
    FROM vbak
    WHERE new_contractid = @gs_out-new_contractid
    AND vbtyp = 'G'
    INTO TABLE @DATA(lt_countvn)
  .
  IF lines( lt_countvn ) GT 1.
    msg = |CRM合同ID[{ gs_out-new_contractid }]存在于[{ lines( lt_countvn ) }]个合同！！！|.
    MESSAGE msg TYPE 'E'.
  ENDIF.
  IF lt_countvn IS NOT INITIAL.
    READ TABLE lt_countvn INTO DATA(wa_countvn) INDEX 1.
    gs_out-vbeln = wa_countvn-vbeln.
    gs_out-auart = wa_countvn-auart.
  ENDIF.
  IF msg IS NOT INITIAL.
    MESSAGE msg TYPE 'E'.
  ENDIF.
  LOOP AT gt_item TRANSPORTING NO FIELDS WHERE action NE ''.
    EXIT.
  ENDLOOP.
  IF sy-subrc EQ 0.
    gs_out-state = '待处理'.
  ELSE.
    gs_out-state = '已处理'.
    PERFORM va03 IN PROGRAM zpubform IF FOUND USING gs_out-vbeln.
    EXIT.
  ENDIF.

  SELECT
    ttxit~*
    FROM tvak
    JOIN ttxern ON tvak~txtgr = ttxern~txtgr
    JOIN ttxit ON ttxern~tdid = ttxit~tdid AND ttxern~tdobject = ttxit~tdobject
    WHERE tvak~auart = @gs_out-auart
    AND ttxern~tdobject IN ( 'VBBK','VBBP' )
    AND ttxern~tdid LIKE 'Z%'
    AND ttxit~tdspras = @sy-langu
    ORDER BY ttxit~tdobject,ttxit~tdid
    INTO TABLE @lt_ttxit.

  CLEAR:gt_longtext_im.
  LOOP AT lt_ttxit ASSIGNING FIELD-SYMBOL(<lt_ttxit>) WHERE tdobject = 'VBBK'.
    CLEAR gs_longtext_im.
    gs_longtext_im-sapmk = 'SD'.
    gs_longtext_im-tdid  = <lt_ttxit>-tdid.
    gs_longtext_im-sapno = gs_out-new_contractid.
    APPEND gs_longtext_im TO gt_longtext_im.
  ENDLOOP.

  SELECT MAX( posnr ) FROM vbap WHERE vbeln = @gs_out-vbeln INTO @posnr.
  SELECT
    posnr,
    zhtmxzf
    FROM vbap
    WHERE vbeln = @gs_out-vbeln
    ORDER BY posnr
    INTO TABLE @DATA(t_apdel)
    .

  LOOP AT gt_item ASSIGNING FIELD-SYMBOL(<gt_item>).
    IF <gt_item>-posnr IS INITIAL.
      ADD 10 TO posnr.
      <gt_item>-posnr = posnr.
    ENDIF.
    READ TABLE t_apdel ASSIGNING FIELD-SYMBOL(<t_apdel>) WITH KEY posnr = <gt_item>-posnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      <gt_item>-zhtmxzf = <t_apdel>-zhtmxzf.
    ENDIF.
    PERFORM checkmenge USING <gt_item>-houdu CHANGING <gt_item>-houdus.
    PERFORM checkmenge USING <gt_item>-width CHANGING <gt_item>-widths.
    PERFORM set_style USING <gt_item>-matnr CHANGING <gt_item>.
    LOOP AT lt_ttxit ASSIGNING <lt_ttxit> WHERE tdobject = 'VBBP'.
      gs_longtext_im-sapmk = 'SD'.
      gs_longtext_im-tdid  = <lt_ttxit>-tdid.
      gs_longtext_im-sapno = |{ gs_out-new_contractid }{ <gt_item>-new_contractdetailid }|.
      APPEND gs_longtext_im TO gt_longtext_im.
    ENDLOOP.
  ENDLOOP.
  PERFORM get_long_text_img TABLES gt_longtext_im.
  IF o_tree_repository IS BOUND.
*  IF o_tree_longtext IS BOUND.
*    PERFORM repo_fill.
    PERFORM repo_fill_simple .
    o_textedit->delete_text( ).
  ENDIF.

  LOOP AT lt_ttxit ASSIGNING <lt_ttxit> WHERE tdobject = 'VBBK'.
    READ TABLE gt_longtext_im INTO gs_longtext_im WITH KEY sapno = gs_out-new_contractid tdid = <lt_ttxit>-tdid BINARY SEARCH.
    IF sy-subrc EQ 0.
      ASSIGN COMPONENT <lt_ttxit>-tdid OF STRUCTURE gs_out TO FIELD-SYMBOL(<tdidv>).
      IF sy-subrc EQ 0.
        <tdidv> = gs_longtext_im-longtext.
      ENDIF.
    ENDIF.
  ENDLOOP.
  LOOP AT gt_item ASSIGNING <gt_item>.
    LOOP AT lt_ttxit ASSIGNING <lt_ttxit> WHERE tdobject = 'VBBP'.
      DATA(sapno) = CONV ztlongtext-sapno( |{ <gt_item>-new_contractid }{ <gt_item>-new_contractdetailid }| ).
      READ TABLE gt_longtext_im INTO gs_longtext_im WITH KEY sapno = sapno tdid = <lt_ttxit>-tdid BINARY SEARCH.
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT <lt_ttxit>-tdid OF STRUCTURE <gt_item> TO FIELD-SYMBOL(<tdidev>).
        IF sy-subrc EQ 0.
          <tdidev> = gs_longtext_im-longtext.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
  MODIFY gt_out FROM gs_out INDEX <row>-row_id.
  PERFORM fillfldct.
  PERFORM ezsdr IN PROGRAM saplzfg_crm USING '' gs_out-new_contractid CHANGING msg.
  CALL SCREEN 900.
ENDFORM.

INCLUDE zsdcrm001_pbo_0900.

INCLUDE zsdcrm001_pai_0900.

*&SPWizard: Data incl. inserted by SP Wizard. DO NOT CHANGE THIS LINE!
INCLUDE zsdcrm001_sub_data .
*&SPWizard: Include inserted by SP Wizard. DO NOT CHANGE THIS LINE!
INCLUDE zsdcrm001_sub_pbo .
INCLUDE zsdcrm001_sub_pai .
*&---------------------------------------------------------------------*
*& Form fillfldct
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fillfldct .
  CLEAR gt_fldct_item.
  CASE gs_out-spart.
    WHEN '11' OR '12' OR '13'.
      PERFORM catset TABLES gt_fldct_item USING:
            'POSNR '  'ZTCRM_SO_ITEM' 'POSNR ' '项目     '   ,
            'MATNR '  'ZTCRM_SO_ITEM' 'MATNR ' '物料编码  '   ,
            'MAKTX ' 'MAKT' 'MAKTX ' '物料描述   '    ,
            'GROES '  'ZTCRM_SO_ITEM' 'GROES ' '品名     '   ,
            'CHANDI'  'ZTCRM_SO_ITEM' 'CHANDI' '产地     '   ,
            'HOUDU '  'ZTCRM_SO_ITEM' 'HOUDU ' '厚度     '   ,
            'WIDTH '  'ZTCRM_SO_ITEM' 'WIDTH ' '宽度     '   ,
            'CAIZHI'  'ZTCRM_SO_ITEM' 'CAIZHI' '材质     '   ,
            'ZHDGC '  'ZTCRM_SO_ITEM' 'ZHDGC ' '厚度公差  '   ,
            'ZKDGC '  'ZTCRM_SO_ITEM' 'ZKDGC ' '宽度公差  '   ,
            'YL9   '  'ZTCRM_SO_ITEM' 'YL9   ' '包装方式  '   ,
            'VRKME '  'ZTCRM_SO_ITEM' 'VRKME ' '销售单位  '   ,
            'KWMENG'  'ZTCRM_SO_ITEM' 'KWMENG' '订单数量  '   ,
            'KBETR '  'ZTCRM_SO_ITEM' 'KBETR ' '金额     '   ,
            'Z001  '  'ZTCRM_SO_ITEM' 'Z001  ' '合同项目备注'   ,
            'WERKS '  'ZTCRM_SO_ITEM' 'WERKS ' '工厂     '   ,
            'ACTION'  'ZTCRM_SO_ITEM' 'ACTION' '行状态   '   .
    WHEN '14'.
      PERFORM catset TABLES gt_fldct_item USING:
            'POSNR ' 'ZTCRM_SO_ITEM' 'POSNR ' '项目       '    ,
            'MATNR ' 'ZTCRM_SO_ITEM' 'MATNR ' '物料编码   '    ,
            'MAKTX ' 'MAKT' 'MAKTX ' '物料描述   '    ,
            'GROES ' 'ZTCRM_SO_ITEM' 'GROES ' '品名       '    ,
            'CHANDI' 'ZTCRM_SO_ITEM' 'CHANDI' '产地       '    ,
            'HOUDU ' 'ZTCRM_SO_ITEM' 'HOUDU ' '厚度       '    ,
            'WIDTH ' 'ZTCRM_SO_ITEM' 'WIDTH ' '宽度       '    ,
            'YL3   ' 'ZTCRM_SO_ITEM' 'YL3   ' '镀层含量   '    ,
            'CAIZHI' 'ZTCRM_SO_ITEM' 'CAIZHI' '材质       '    ,
            'YL4   ' 'ZTCRM_SO_ITEM' 'YL4   ' '表面处理   '    ,
            'HCL   ' 'ZTCRM_SO_ITEM' 'HCL   ' '后处理     '    ,
            'ZHDGC ' 'ZTCRM_SO_ITEM' 'ZHDGC ' '厚度公差   '    ,
            'ZKDGC ' 'ZTCRM_SO_ITEM' 'ZKDGC ' '宽度公差   '    ,
            'YL9   ' 'ZTCRM_SO_ITEM' 'YL9   ' '包装方式   '    ,
            'VRKME ' 'ZTCRM_SO_ITEM' 'VRKME ' '销售单位   '    ,
            'KWMENG' 'ZTCRM_SO_ITEM' 'KWMENG' '订单数量   '    ,
            'KBETR ' 'ZTCRM_SO_ITEM' 'KBETR ' '金额       '    ,
            'Z001  ' 'ZTCRM_SO_ITEM' 'Z001  ' '合同项目备注'    ,
            'WERKS ' 'ZTCRM_SO_ITEM' 'WERKS ' '工厂       '    ,
            'YL6 ' 'ZTCRM_SO_ITEM' 'YL6' '套筒类型       '    ,
            'YL11 ' 'ZTCRM_SO_ITEM' 'YL11' '单卷重量       '    ,
            'YL12 ' 'ZTCRM_SO_ITEM' 'YL12' '成品用途       '    ,
            'ACTION' 'ZTCRM_SO_ITEM' 'ACTION' '行状态     '    ,
            'ZDCGC' 'ZTCRM_SO_ITEM' 'ZDCGC' '镀层公差'    .
    WHEN '15'.
      PERFORM catset TABLES gt_fldct_item USING:
            'POSNR ' 'ZTCRM_SO_ITEM' 'POSNR ' '项目     '  ,
            'MATNR ' 'ZTCRM_SO_ITEM' 'MATNR ' '物料编码 '   ,
            'MAKTX ' 'MAKT' 'MAKTX ' '物料描述   '    ,
            'GROES ' 'ZTCRM_SO_ITEM' 'GROES ' '品名     '  ,
            'CHANDI' 'ZTCRM_SO_ITEM' 'CHANDI' '产地     '  ,
            'HOUDU ' 'ZTCRM_SO_ITEM' 'HOUDU ' '厚度     '  ,
            'WIDTH ' 'ZTCRM_SO_ITEM' 'WIDTH ' '宽度     '  ,
            'YL3   ' 'ZTCRM_SO_ITEM' 'YL3   ' '镀层含量 '   ,
            'CAIZHI' 'ZTCRM_SO_ITEM' 'CAIZHI' '材质     '  ,
            'ZSXQB ' 'ZTCRM_SO_ITEM' 'ZSXQB ' '涂层种类 '   ,
            'YL2   ' 'ZTCRM_SO_ITEM' 'YL2   ' '颜色     '  ,
            'ZHDGC ' 'ZTCRM_SO_ITEM' 'ZHDGC ' '厚度公差 '   ,
            'ZKDGC ' 'ZTCRM_SO_ITEM' 'ZKDGC ' '宽度公差 '   ,
            'YL9   ' 'ZTCRM_SO_ITEM' 'YL9   ' '包装方式 '   ,
            'VRKME ' 'ZTCRM_SO_ITEM' 'VRKME ' '销售单位 '   ,
            'KWMENG' 'ZTCRM_SO_ITEM' 'KWMENG' '订单数量 '   ,
            'KBETR ' 'ZTCRM_SO_ITEM' 'KBETR ' '金额     '  ,
            'Z001  ' 'ZTCRM_SO_ITEM' 'Z001  ' '合同项目备注'  ,
            'WERKS ' 'ZTCRM_SO_ITEM' 'WERKS ' '工厂     '  ,
            'ACTION' 'ZTCRM_SO_ITEM' 'ACTION' '行状态   '  ,
            'ZDCGC' 'ZTCRM_SO_ITEM' 'ZDCGC' '镀层公差'    .
    WHEN OTHERS.
      PERFORM catset TABLES gt_fldct_item USING:
            'POSNR ' 'ZTCRM_SO_ITEM' 'POSNR ' '项目     '  ,
            'MATNR ' 'ZTCRM_SO_ITEM' 'MATNR ' '物料编码 '   ,
            'MAKTX ' 'MAKT' 'MAKTX ' '物料描述   '    .
  ENDCASE.
  PERFORM catset TABLES gt_fldct_item USING:
        'BMTHOUDU ' 'ZTCRM_SO_ITEM' 'BMTHOUDU' 'BMT厚度'  ,
        'TCTHOUDU ' 'ZTCRM_SO_ITEM' 'TCTHOUDU' 'TCT厚度'  ,
        'TCTPHOUDU ' 'ZTCRM_SO_ITEM' 'TCTPHOUDU' 'TCTP厚度'  ,
        'HOUDULX ' 'ZTCRM_SO_ITEM' 'HOUDULX' '厚度类型'  ,
        'ZBQHT' 'ZTCRM_SO_ITEM' 'ZBQHT' '标签厚度'  ,
        'ZISXB' 'ZTCRM_SO_ITEM' 'ZISXB' '是否修边'  ,
        'ZFT_WIDTH' 'ZTCRM_SO_ITEM' 'ZFT_WIDTH' '分条宽度'  .
  LOOP AT gt_fldct_item ASSIGNING FIELD-SYMBOL(<fldct>).
    CASE <fldct>-fieldname.
      WHEN 'MATNR'.
        <fldct>-edit = 'X'.
        <fldct>-f4availabl = 'X'.
      WHEN 'YL6' OR 'YL11' OR 'YL12'.
        <fldct>-edit = 'X'.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_style
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- <GT_ITEM>
*&---------------------------------------------------------------------*
FORM set_style USING VALUE(p_matnr) CHANGING p_item LIKE gs_item.
  CLEAR:p_item-field_style,p_item-cellcolor.
  IF p_item-action = ''.
    LOOP AT gt_fldct_item ASSIGNING FIELD-SYMBOL(<fldct>) WHERE edit = 'X'.
      CLEAR stylelin.
      stylelin-fieldname = <fldct>-fieldname.
      stylelin-style = cl_gui_alv_grid=>mc_style_disabled.
      INSERT stylelin INTO TABLE p_item-field_style.
    ENDLOOP.
    SELECT SINGLE
      vbap~matnr,
      makt~maktx
      FROM vbak
      JOIN vbap ON vbak~vbeln = vbap~vbeln
      JOIN makt ON vbap~matnr = makt~matnr AND makt~spras = @sy-langu
      WHERE vbap~new_contractdetailid = @p_item-new_contractdetailid
      AND vbak~new_contractid = @gs_out-new_contractid
      INTO ( @p_item-matnr,@p_item-maktx )
      .
    IF sy-subrc NE 0.
      INSERT INITIAL LINE INTO TABLE p_item-cellcolor ASSIGNING FIELD-SYMBOL(<cellcolor>).
      <cellcolor>-fname = 'MATNR'.
      <cellcolor>-color-col = 6 .
      <cellcolor>-color-int = 1 .
      <cellcolor>-color-inv = 0 .
      INSERT INITIAL LINE INTO TABLE p_item-cellcolor ASSIGNING <cellcolor>.
      <cellcolor>-fname = 'MAKTX'.
      <cellcolor>-color-col = 6 .
      <cellcolor>-color-int = 1 .
      <cellcolor>-color-inv = 0 .
    ENDIF.
  ELSE."action
    LOOP AT gt_fldct_item ASSIGNING <fldct> WHERE edit = 'X'.
      CLEAR stylelin.
      stylelin-fieldname = <fldct>-fieldname.
      stylelin-style = cl_gui_alv_grid=>mc_style_enabled.
      INSERT stylelin INTO TABLE p_item-field_style.
    ENDLOOP.
    CASE gs_out-spart.
      WHEN '11' OR '12' OR '13'.
        SELECT SINGLE
          mara~matnr,
          makt~maktx
          FROM mara
          LEFT JOIN makt ON mara~matnr = makt~matnr AND spras = @sy-langu
          WHERE mara~spart = @gs_out-spart
          AND mara~groes = @p_item-groes
          AND mara~houdus = @p_item-houdus
          AND mara~widths = @p_item-widths
          AND mara~chandi = @p_item-chandi
          AND mara~caizhi = @p_item-caizhi
          INTO ( @DATA(matnr),@DATA(maktx) )
          .
      WHEN '14'.
        SELECT SINGLE
          mara~matnr,
          makt~maktx
          FROM mara
          LEFT JOIN makt ON mara~matnr = makt~matnr AND spras = @sy-langu
          WHERE mara~spart = @gs_out-spart
          AND mara~groes = @p_item-groes
          AND mara~houdus = @p_item-houdus
          AND mara~widths = @p_item-widths
*          AND mara~chandi = @p_item-chandi
          AND mara~chandi = '自产'"  https://zentao.shinmade.com/task-view-31640.html 20250113修改： 13.01.2025 13:29:25 by kkw
          AND mara~caizhi = @p_item-caizhi
          AND mara~yl3 = @p_item-yl3
          AND mara~yl4 = @p_item-yl4
          AND mara~hcl = @p_item-hcl
          INTO ( @matnr,@maktx )
          .
      WHEN '15'.
        SELECT SINGLE
          mara~matnr,
          makt~maktx
          FROM mara
          LEFT JOIN makt ON mara~matnr = makt~matnr AND spras = @sy-langu
          WHERE mara~spart = @gs_out-spart
          AND mara~groes = @p_item-groes
          AND mara~houdus = @p_item-houdus
          AND mara~widths = @p_item-widths
          AND mara~chandi = @p_item-chandi
          AND mara~caizhi = @p_item-caizhi
          AND mara~yl2 = @p_item-yl2
          AND mara~yl3 = @p_item-yl3
          AND mara~yl5 = @p_item-zsxqb
          INTO ( @matnr,@maktx )
          .
    ENDCASE.

    IF p_matnr IS NOT INITIAL.
      IF p_matnr = matnr.
        p_item-maktx = maktx.
        INSERT INITIAL LINE INTO TABLE p_item-cellcolor ASSIGNING <cellcolor>.
        <cellcolor>-fname = 'MATNR'.
        <cellcolor>-color-col = 5 .
        <cellcolor>-color-int = 1 .
        <cellcolor>-color-inv = 0 .
        INSERT INITIAL LINE INTO TABLE p_item-cellcolor ASSIGNING <cellcolor>.
        <cellcolor>-fname = 'MAKTX'.
        <cellcolor>-color-col = 5 .
        <cellcolor>-color-int = 1 .
        <cellcolor>-color-inv = 0 .
      ELSE.
        SELECT SINGLE
          makt~maktx
          FROM makt
          WHERE makt~matnr = @p_matnr
          AND makt~spras = @sy-langu
          INTO @p_item-maktx
          .
        INSERT INITIAL LINE INTO TABLE p_item-cellcolor ASSIGNING <cellcolor>.
        <cellcolor>-fname = 'MATNR'.
        <cellcolor>-color-col = 3 .
        <cellcolor>-color-int = 1 .
        <cellcolor>-color-inv = 0 .
        INSERT INITIAL LINE INTO TABLE p_item-cellcolor ASSIGNING <cellcolor>.
        <cellcolor>-fname = 'MAKTX'.
        <cellcolor>-color-col = 3 .
        <cellcolor>-color-int = 1 .
        <cellcolor>-color-inv = 0 .
      ENDIF.
    ELSE.
      IF matnr IS NOT INITIAL.
        p_item-matnr = matnr.
        p_item-maktx = maktx.
        INSERT INITIAL LINE INTO TABLE p_item-cellcolor ASSIGNING <cellcolor>.
        <cellcolor>-fname = 'MATNR'.
        <cellcolor>-color-col = 5 .
        <cellcolor>-color-int = 1 .
        <cellcolor>-color-inv = 0 .
        INSERT INITIAL LINE INTO TABLE p_item-cellcolor ASSIGNING <cellcolor>.
        <cellcolor>-fname = 'MAKTX'.
        <cellcolor>-color-col = 5 .
        <cellcolor>-color-int = 1 .
        <cellcolor>-color-inv = 0 .
      ELSE.
        INSERT INITIAL LINE INTO TABLE p_item-cellcolor ASSIGNING <cellcolor>.
        <cellcolor>-fname = 'MATNR'.
        <cellcolor>-color-col = 6 .
        <cellcolor>-color-int = 1 .
        <cellcolor>-color-inv = 0 .
        INSERT INITIAL LINE INTO TABLE p_item-cellcolor ASSIGNING <cellcolor>.
        <cellcolor>-fname = 'MAKTX'.
        <cellcolor>-color-col = 6 .
        <cellcolor>-color-int = 1 .
        <cellcolor>-color-inv = 0 .
      ENDIF.
    ENDIF.
  ENDIF."action
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_head
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_head USING VALUE(p_new_contractid).
  CLEAR gs_out.
  SELECT SINGLE
    z~*,
    ag~name1 AS name_ag,
    we~name1 AS name_we
    FROM ztcrm_so_head AS z
    LEFT JOIN kna1 AS ag ON z~kunnr_ag = ag~kunnr
    LEFT JOIN kna1 AS we ON z~kunnr_we = we~kunnr
    WHERE z~new_contractid = @p_new_contractid
    INTO CORRESPONDING FIELDS OF @gs_out.

  SELECT
    domname,
    domvalue_l,
    ddtext
    FROM dd07t
    WHERE domname IN ( 'ZD_ZISDXS','ZD_ZHTJGFS','ZD_ZHWLX','ZD_ZCPYT','ZD_ZCPSX','ZD_ZISJB' )
    AND ddlanguage = @sy-langu
    AND as4local = 'A'
    ORDER BY domname,domvalue_l
    INTO TABLE @lt_domdes
    .

  SELECT vkorg,vtext
    FROM tvkot
    WHERE spras = @sy-langu
    INTO TABLE @DATA(lt_tvkot).
  SELECT vtweg,vtext
    FROM tvtwt
    WHERE spras = @sy-langu
    INTO TABLE @DATA(lt_tvtwt).
  SELECT vkbur,bezei
    FROM tvkbt
    WHERE spras = @sy-langu
    INTO TABLE @DATA(lt_tvkbt).
  SELECT vkgrp,bezei
    FROM tvgrt
    WHERE spras = @sy-langu
    INTO TABLE @DATA(lt_tvgrt).
  SELECT spart,vtext
    FROM tspat
    WHERE spras = @sy-langu
    INTO TABLE @DATA(lt_tspat).
  SELECT kdgrp,ktext
    FROM t151t
    WHERE spras = @sy-langu
    INTO TABLE @DATA(lt_t151t).
  SELECT zid,zname
    FROM ztsd226
    FOR ALL ENTRIES IN @gt_out
    WHERE zid = @gt_out-province
    OR zid = @gt_out-city
    OR zid = @gt_out-county
    INTO TABLE @DATA(lt_226).
  SELECT auart,bezei
    FROM tvakt
    WHERE spras = @sy-langu
  INTO TABLE @DATA(lt_tvakt).

  getdomdes 'ZD_ZISDXS' gs_out-zisdxs gs_out-zisdxs_des.
  getdomdes 'ZD_ZHTJGFS' gs_out-zhtjgfs gs_out-zhtjgfs_des.
  getdomdes 'ZD_ZHWLX' gs_out-zhwlx gs_out-zhwlx_des.
  getdomdes 'ZD_ZCPYT' gs_out-zcpyt gs_out-zcpyt_des.
  getdomdes 'ZD_ZCPSX' gs_out-zcpsx gs_out-zcpsx_des.
  getdomdes 'ZD_ZISJB' gs_out-zISJB gs_out-zisjb_des.
  READ TABLE lt_tvkot INTO DATA(wa1) WITH KEY vkorg = gs_out-vkorg.
  IF sy-subrc EQ 0.
    gs_out-vkorg_des = wa1-vtext.
  ENDIF.
  READ TABLE lt_tvtwt INTO DATA(wa2) WITH KEY vtweg = gs_out-vtweg.
  IF sy-subrc EQ 0.
    gs_out-vtweg_des = wa2-vtext.
  ENDIF.
  READ TABLE lt_tvkbt INTO DATA(wa3) WITH KEY vkbur = gs_out-vkbur.
  IF sy-subrc EQ 0.
    gs_out-vkbur_des = wa3-bezei.
  ENDIF.
  READ TABLE lt_tvgrt INTO DATA(wa4) WITH KEY vkgrp = gs_out-vkgrp.
  IF sy-subrc EQ 0.
    gs_out-vkgrp_des = wa4-bezei.
  ENDIF.
  READ TABLE lt_tspat INTO DATA(wa5) WITH KEY spart = gs_out-spart.
  IF sy-subrc EQ 0.
    gs_out-spart_des = wa5-vtext.
  ENDIF.
  READ TABLE lt_t151t INTO DATA(wa6) WITH KEY kdgrp = gs_out-kdgrp.
  IF sy-subrc EQ 0.
    gs_out-kdgrp_des = wa6-ktext.
  ENDIF.
  READ TABLE lt_226 INTO DATA(wa226) WITH KEY zid = gs_out-province.
  IF sy-subrc EQ 0.
    gs_out-province_des = wa226-zname.
  ENDIF.
  READ TABLE lt_226 INTO wa226 WITH KEY zid = gs_out-city.
  IF sy-subrc EQ 0.
    gs_out-city_des = wa226-zname.
  ENDIF.
  READ TABLE lt_226 INTO wa226 WITH KEY zid = gs_out-county.
  IF sy-subrc EQ 0.
    gs_out-county_des = wa226-zname.
  ENDIF.
  READ TABLE lt_tvakt INTO DATA(wa7) WITH KEY auart = gs_out-auart.
  IF sy-subrc EQ 0.
    gs_out-auart_des = wa7-bezei.
  ENDIF.
  IF gs_out-zisck = 'X'.
    gs_out-zisck_des = '出口'.
  ELSE.
    gs_out-zisck_des = '非出口'.
  ENDIF.
  IF gs_out-ztt = 'X'.
    gs_out-ztt_des = '是'.
  ELSE.
    gs_out-ztt_des = '否'.
  ENDIF.
  IF gs_out-ziscj = 'X'.
    gs_out-ziscj_des = '是'.
  ELSE.
    gs_out-ziscj_des = '否'.
  ENDIF.
  IF gs_out-yl1 = 'X'.
    gs_out-yl1_des = '是'.
  ELSE.
    gs_out-yl1_des = '否'.
  ENDIF.

ENDFORM.
