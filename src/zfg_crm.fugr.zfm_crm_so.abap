FUNCTION zfm_crm_so.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(ACTION) TYPE  ZE_SO_ACTION DEFAULT 'S'
*"  EXPORTING
*"     VALUE(RTYPE) TYPE  BAPI_MTYPE
*"     VALUE(RTMSG) TYPE  BAPI_MSG
*"     VALUE(VBELN) TYPE  VBELN
*"  CHANGING
*"     VALUE(DATA) TYPE  ZSCRM_SO
*"----------------------------------------------------------------------
  zfmdatasave1 'ZFM_CRM_SO'.
  zfmdatasave2 'B'.
  COMMIT WORK.
  IF data IS INITIAL.
    fillmsg 'E' '请传入DATA数据后再调用本接口'.
  ENDIF.
  CLEAR:rtype,rtmsg,vbeln.

  TYPES:BEGIN OF zsvbap,
          vbeln  TYPE vbak-vbeln,
          posnr  TYPE vbap-posnr,
          matnr  TYPE vbap-matnr,
          werks  TYPE vbap-werks,
          pstyv  TYPE vbap-pstyv,
          bstkd  TYPE vbkd-bstkd,
          znodel TYPE char1,
          zmatnr TYPE char1,
        END OF zsvbap.
  DATA:it_vbap TYPE TABLE OF zsvbap WITH HEADER LINE.

  DATA:sales_header_in      TYPE bapisdhd1,
       sales_header_inx     TYPE bapisdhd1x,
       order_header_in      TYPE bapisdh1,
       order_header_inx     TYPE bapisdh1x,
       salesdocument_ex     TYPE bapivbeln-vbeln,
*        SALES_TEXT           TYPE TABLE OF BAPISDTEXT WITH HEADER LINE,
       it_knvp              TYPE TABLE OF knvp WITH HEADER LINE,
       return               TYPE STANDARD TABLE OF bapiret2 WITH HEADER LINE,
       sales_items_in       TYPE STANDARD TABLE OF bapisditm WITH HEADER LINE,
       sales_items_inx      TYPE STANDARD TABLE OF bapisditmx WITH HEADER LINE,
       sales_partners       TYPE STANDARD TABLE OF bapiparnr WITH HEADER LINE,
       sales_schedules_in   TYPE STANDARD TABLE OF bapischdl WITH HEADER LINE,
       sales_schedules_inx  TYPE STANDARD TABLE OF bapischdlx WITH HEADER LINE,
       sales_conditions_in  TYPE STANDARD TABLE OF bapicond WITH HEADER LINE,
       sales_conditions_inx TYPE STANDARD TABLE OF bapicondx WITH HEADER LINE,
       extensionin          TYPE TABLE OF bapiparex WITH HEADER LINE,
       extensionex          TYPE TABLE OF bapiparex WITH HEADER LINE.
  DATA:order_header    TYPE bapisdh1,
       order_headerx   TYPE bapisdh1x,
       salesdocument   TYPE bapivbeln-vbeln,
       order_item_in   TYPE TABLE OF bapisditm  WITH HEADER LINE,
       order_item_inx  TYPE TABLE OF bapisditmx WITH HEADER LINE,
       schedule_lines  TYPE TABLE OF bapischdl  WITH HEADER LINE,
       schedule_linesx TYPE TABLE OF bapischdlx WITH HEADER LINE,
       order_text      TYPE TABLE OF bapisdtext WITH HEADER LINE,
       conditions_in   TYPE TABLE OF bapicond   WITH HEADER LINE,
       conditions_inx  TYPE TABLE OF bapicondx  WITH HEADER LINE.

  DATA:BEGIN OF text_stream OCCURS 0,
         text TYPE char2048,
       END OF text_stream,
       lines TYPE TABLE OF tline WITH HEADER LINE.
  DATA:wa_extk  TYPE bape_vbak,
       wa_extkx TYPE bape_vbakx,
       wa_extp  TYPE bape_vbap,
       wa_extpx TYPE bape_vbapx,
       subrc    TYPE sy-subrc,
       mess     TYPE char200,
       mes      TYPE string.
  DATA:posnr TYPE vbap-posnr.
**********************************
  DATA:wa_head TYPE ztcrm_so_head,
       lt_item TYPE TABLE OF ZTCRM_SO_item.

  RANGES:s_zcolname FOR ztsd203-zcolname.
  RANGES:s_matnr    FOR vbap-matnr.

  PERFORM checkdom USING 'ZD_SO_ACTION' action CHANGING rtmsg.
  IF rtmsg IS NOT INITIAL.
    fillmsg 'E' rtmsg.
  ENDIF.

  CLEAR:s_zcolname[],s_matnr[].
  CLEAR:s_zcolname.s_zcolname = 'IEQJQJ'. APPEND s_zcolname.
  CLEAR:s_zcolname.s_zcolname = 'IEQKD' . APPEND s_zcolname.
  CLEAR:s_zcolname.s_zcolname = 'IEQXG' . APPEND s_zcolname.

  SELECT
     *
    FROM ztsd203
    WHERE ztabname =  'MATNR'
    AND  zcolname IN @s_zcolname
    ORDER BY zseloption
    INTO TABLE @DATA(it_ztsd203)
     .
  LOOP AT it_ztsd203 INTO DATA(wa_ztsd203).
    CLEAR:s_matnr.
    s_matnr(3) = 'IEQ'.
    s_matnr-low = wa_ztsd203-zseloption.
    COLLECT s_matnr.
  ENDLOOP.

  CASE action.
    WHEN 'S'.
      MOVE-CORRESPONDING data TO wa_head.
      MOVE-CORRESPONDING data-items TO lt_item.
      LOOP AT lt_item ASSIGNING FIELD-SYMBOL(<lt_item>).
        <lt_item>-new_contractid = data-new_contractid.
      ENDLOOP.
      MODIFY ZTCRM_SO_item FROM TABLE lt_item.
      IF sy-subrc NE 0.
        subrc = 4.
      ENDIF.
      MODIFY ztcrm_so_head FROM wa_head.
      IF sy-subrc NE 0.
        subrc = 4.
      ENDIF.
      IF subrc EQ 0.
        COMMIT WORK.
        rtmsg = 'S:数据保存成功'.
        zfmdatasave2 'R'.
      ELSE.
        ROLLBACK WORK.
        rtmsg = 'E:数据保存失败'.
      ENDIF.
      fillmsg rtmsg(1) rtmsg.
      RETURN.
    WHEN 'I'.
      SELECT
        COUNT(*)
        FROM vbkd
        WHERE bstkd = @data-bstkd
        .
      IF sy-subrc = 0.
        rtmsg = |外部合同号：{ data-bstkd }已创建销售订单，请确认！|.
        fillmsg 'E' rtmsg.
      ENDIF.

      checkinitial data-bstkd       '客户参考'            .
      checkinitial data-auart       '销售凭证类型'          .
      checkinitial data-kunnr_we    '售达方'             .
      checkinitial data-kunnr_ag    '送达方'             .
      checkinitial data-vkorg       '销售组织'            .
      checkinitial data-vtweg       '分销渠道'            .
      checkinitial data-prsdt       '凭证日期'            .
      checkinitial data-guebg       '有效期自'            .
      checkinitial data-gueen       '有效期至'            .
      checkinitial data-zhtyf       '合同月份'            .
      checkinitial data-spart       '产品组'             .
      checkinitial data-zhwlx       '货物类型'            .
      checkinitial data-vkbur       '销售办事处'           .
      checkinitial data-vkgrp       '销售组'             .
      checkinitial data-waerk       '凭证货币'            .
      checkinitial data-kursk       '汇率'              .
      checkinitial data-zterm       '付款条件'            .
      checkinitial data-zhtjgfs     '合同加工方式'          .
      checkinitial data-zdjbl       '定金比例'            .
      checkinitial data-zisdxs      '销售类型'            .
      checkinitial data-zcpyt       '产品用途'            .

      "补0
      PERFORM addzero(zpubform) CHANGING data-kunnr_we.
      PERFORM addzero(zpubform) CHANGING data-kunnr_ag.

      IF data-items IS INITIAL.
        fillmsg 'E' '合同明细不能为空'.
      ENDIF.
      LOOP AT data-items ASSIGNING FIELD-SYMBOL(<item>).
*        mes = '第' && sy-tabix && '行,' && '物料'           .
*        checkinitial <item>-matnr mes.
        mes = '第' && sy-tabix && '行,' && '品名'           .
        checkinitial <item>-groes mes.
        mes = '第' && sy-tabix && '行,' && '厚度'           .
        checkinitial <item>-houdu mes.
        mes = '第' && sy-tabix && '行,' && '宽度'           .
        checkinitial <item>-width mes.
        mes = '第' && sy-tabix && '行,' && '材质'           .
        checkinitial <item>-caizhi mes.
        mes = '第' && sy-tabix && '行,' && '工厂'           .
        checkinitial <item>-werks mes.
        mes = '第' && sy-tabix && '行,' && '销售单位'         .
        checkinitial <item>-vrkme mes.
        mes = '第' && sy-tabix && '行,' && '订单数量'         .
        checkinitial <item>-kwmeng mes.
        mes = '第' && sy-tabix && '行,' && '价格'           .
        checkinitial <item>-kbetr mes.
        mes = '第' && sy-tabix && '行,' && '税率'           .
        checkinitial <item>-mwskz mes.

*单位转换
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
          EXPORTING
            input          = <item>-vrkme
            language       = sy-langu
          IMPORTING
            output         = <item>-vrkme
          EXCEPTIONS
            unit_not_found = 1
            OTHERS         = 2.

*        SELECT SINGLE
*          matnr
*          INTO @DATA(matnr)
*          FROM mara
*          WHERE matnr = @<item>-matnr
*          .
*        SELECT SINGLE
*          meinh
*          INTO @DATA(meinh)
*          FROM marm
*          WHERE matnr = @<item>-matnr
*          AND meinh = @<item>-vrkme
*          .
*        IF matnr IS NOT INITIAL AND meinh IS INITIAL.
*          CLEAR mes.
*          PERFORM addunit USING <item>-matnr <item>-vrkme CHANGING mess.
*          IF mess+0(1) = 'E'.
*            mes = '第' && sy-tabix && '行，物料：' && <item>-matnr && '单位:' && <item>-vrkme && '扩充失败！:' && mess.
*            rtmsg = mes.
*            fillmsg 'E' rtmsg.
*          ENDIF.
*        ENDIF.
*        CLEAR :matnr,meinh.
      ENDLOOP.

      CLEAR  : sales_header_in        .
      CLEAR  : sales_header_inx       .
      CLEAR  : salesdocument_ex       .
      CLEAR  : return                 ,    return[]               .
      CLEAR  : sales_items_in         ,    sales_items_in[]       .
      CLEAR  : sales_items_inx        ,    sales_items_inx[]      .
      CLEAR  : sales_partners         ,    sales_partners[]       .
      CLEAR  : sales_schedules_in     ,    sales_schedules_in[]   .
      CLEAR  : sales_schedules_inx    ,    sales_schedules_inx[]  .
      CLEAR  : sales_conditions_in    ,    sales_conditions_in[]  .
      CLEAR  : sales_conditions_inx   ,    sales_conditions_inx[] .
      CLEAR  : order_text             ,    order_text[]           .
      CLEAR  : extensionin            ,    extensionin[]          .

      "抬头
*******************************************************20230214
      sales_header_in-doc_type   = data-auart ."订单类型
      sales_header_in-sales_org  = data-vkorg ."销售组织
*      sales_header_in-ord_reason = data-augru ."补差原因
      sales_header_in-purch_no_c = data-bstkd ."外部合同号
      sales_header_in-division   = data-spart ."产品组
      sales_header_in-sales_grp  = data-vkgrp ."销售组
      sales_header_in-distr_chan = data-vtweg ."分销渠道
      sales_header_in-sales_off  = data-vkbur ."销售办事处
*      sales_header_in-wbs_elem   = data-posid.
      sales_header_in-currency   = data-waerk.
      sales_header_in-doc_date   = data-prsdt."凭证日期 (接收/发送日期)
*      IF data-bstkd_e IS NOT INITIAL.
*        sales_header_in-purch_no_s  = data-bstkd_e."运达方的采购订单编号
*      ENDIF.
      sales_header_in-incoterms1 = data-inco1."*  国际贸易条款（第 1 部分）
      sales_header_in-incoterms2 = data-inco2_l."*  国际贸易条款（第 2 部分）
      sales_header_in-price_date = data-prsdt.
      sales_header_in-ct_valid_f = data-guebg.
      sales_header_in-ct_valid_t = data-gueen.
      sales_header_in-cust_group = data-kdgrp.
*      sales_header_in-price_grp  = data-kdgrp."客户价格组
      PERFORM setbapix USING sales_header_in CHANGING sales_header_inx.

*增强字段
      CLEAR wa_extk.
      MOVE-CORRESPONDING data TO wa_extk.
      CLEAR:extensionin.
      extensionin-structure = 'BAPE_VBAK'.
      extensionin+30(960) = wa_extk.
*      CALL METHOD CL_ABAP_CONTAINER_UTILITIES=>FILL_CONTAINER_C
*        EXPORTING
*          IM_VALUE     = WA_EXTK
*        IMPORTING
*          EX_CONTAINER = EXTENSIONIN-VALUEPART1.
*      CALL METHOD CL_ABAP_CONTAINER_UTILITIES=>FILL_CONTAINER_C
*        EXPORTING
*          IM_VALUE     = WA_EXTK+240
*        IMPORTING
*          EX_CONTAINER = EXTENSIONIN-VALUEPART2.
*      CALL METHOD CL_ABAP_CONTAINER_UTILITIES=>FILL_CONTAINER_C
*        EXPORTING
*          IM_VALUE     = WA_EXTK+480
*        IMPORTING
*          EX_CONTAINER = EXTENSIONIN-VALUEPART3.
*      CALL METHOD CL_ABAP_CONTAINER_UTILITIES=>FILL_CONTAINER_C
*        EXPORTING
*          IM_VALUE     = WA_EXTK+720
*        IMPORTING
*          EX_CONTAINER = EXTENSIONIN-VALUEPART4.
      APPEND extensionin.
      CLEAR:extensionin.
      PERFORM setbapix USING wa_extk CHANGING wa_extkx.
      extensionin-structure = 'BAPE_VBAKX'.
      extensionin-valuepart1 = wa_extkx.
      APPEND extensionin.
**********************************
      "合作伙伴
      CLEAR sales_partners.
      sales_partners-partn_role = 'AG'.
      sales_partners-partn_numb = data-kunnr_ag.
      PERFORM addzero(zpubform) CHANGING sales_partners-partn_numb.
      APPEND sales_partners.
      CLEAR sales_partners.
      sales_partners-partn_role = 'WE'.
      sales_partners-partn_numb = data-kunnr_we.
      PERFORM addzero(zpubform) CHANGING sales_partners-partn_numb.
      APPEND sales_partners.

      CLEAR:order_text[].
      IF data-z001 IS NOT INITIAL.
        "切割文本
        CLEAR:text_stream,text_stream[],lines[],lines.
        text_stream-text = data-z001.
        APPEND text_stream.
        CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
          TABLES
            text_stream = text_stream
            itf_text    = lines.
        LOOP AT lines.
          CLEAR:order_text.
          "抬头备注
          order_text-text_id    = 'Z001'.
          order_text-langu      = sy-langu.
          order_text-format_col = '*' .
          order_text-text_line  = lines-tdline.
          APPEND order_text.
        ENDLOOP.
      ENDIF.
      IF data-z002 IS NOT INITIAL.
        "切割文本
        CLEAR:text_stream,text_stream[],lines[],lines.
        text_stream-text = data-z002.
        APPEND text_stream.
        CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
          TABLES
            text_stream = text_stream
            itf_text    = lines.
        LOOP AT lines.
          CLEAR:order_text.
          "合同抬头备注
          order_text-text_id    = 'Z002'.
          order_text-langu      = sy-langu.
          order_text-format_col = '*' .
          order_text-text_line  = lines-tdline.
          APPEND order_text.
        ENDLOOP.
      ENDIF.
      IF data-z006 IS NOT INITIAL.
        "切割文本
        CLEAR:text_stream,text_stream[],lines[],lines.
        text_stream-text = data-z006.
        APPEND text_stream.
        CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
          TABLES
            text_stream = text_stream
            itf_text    = lines.
        LOOP AT lines.
          CLEAR:order_text.
          "质量要求标准
          order_text-text_id    = 'Z006'.
          order_text-langu      = sy-langu.
          order_text-format_col = '*' .
          order_text-text_line  = lines-tdline.
          APPEND order_text.
        ENDLOOP.
      ENDIF.
      IF data-z007 IS NOT INITIAL.
        "切割文本
        CLEAR:text_stream,text_stream[],lines[],lines.
        text_stream-text = data-z007.
        APPEND text_stream.
        CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
          TABLES
            text_stream = text_stream
            itf_text    = lines.
        LOOP AT lines.
          CLEAR:order_text.
          "运输方式以及运费承担
          order_text-text_id    = 'Z007'.
          order_text-langu      = sy-langu.
          order_text-format_col = '*' .
          order_text-text_line  = lines-tdline.
          APPEND order_text.
        ENDLOOP.
      ENDIF.
      IF data-z008 IS NOT INITIAL.
        "切割文本
        CLEAR:text_stream,text_stream[],lines[],lines.
        text_stream-text = data-z008.
        APPEND text_stream.
        CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
          TABLES
            text_stream = text_stream
            itf_text    = lines.
        LOOP AT lines.
          CLEAR:order_text.
          "支付方式
          order_text-text_id    = 'Z008'.
          order_text-langu      = sy-langu.
          order_text-format_col = '*' .
          order_text-text_line  = lines-tdline.
          APPEND order_text.
        ENDLOOP.
      ENDIF.
      IF data-z009 IS NOT INITIAL.
        "切割文本
        CLEAR:text_stream,text_stream[],lines[],lines.
        text_stream-text = data-z009.
        APPEND text_stream.
        CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
          TABLES
            text_stream = text_stream
            itf_text    = lines.
        LOOP AT lines.
          CLEAR:order_text.
          "备注
          order_text-text_id    = 'Z009'.
          order_text-langu      = sy-langu.
          order_text-format_col = '*' .
          order_text-text_line  = lines-tdline.
          APPEND order_text.
        ENDLOOP.
      ENDIF.
**********************************
      "行项目
      posnr = 0 .
      LOOP AT data-items ASSIGNING <item> .
        CLEAR:sales_items_in,sales_items_inx,sales_schedules_in,sales_schedules_inx .
        ADD 10 TO posnr.
        <item>-posnr = posnr.
        sales_items_in-itm_number     = posnr. "使用ERP给的行号
        sales_items_in-material       = <item>-matnr.
        PERFORM addzero(zpubform) CHANGING sales_items_in-material.
*        sales_items_in-prc_group2     = <item>-mvgr2.
*        sales_items_in-prc_group3     = <item>-mvgr3.
        sales_items_in-target_qty     = <item>-kwmeng.
        sales_items_in-target_qu      = <item>-vrkme.
        sales_items_in-sales_unit     = <item>-vrkme.
*        sales_items_in-reason_rej     = <item>-abgru.
*        sales_items_in-item_categ     = <item>-pstyv."项目类别
*        sales_items_in-hg_lv_item     = <item>-uepos."上一层项目
*        IF data-auart = 'ZWM0' OR data-vkorg = '3210'.     "只有外贸允许修改税码 "增加马来西亚公司修改税码逻辑 By Qidawei On 20240805
*          sales_items_in-tax_class1     = data-zsl.    "税码调整 add by yangyousheng 20221209
*        ENDIF.
        PERFORM transunit_to_inside(zpubform) CHANGING sales_items_in-target_qu.
        PERFORM transunit_to_inside(zpubform) CHANGING sales_items_in-sales_unit.
        sales_items_in-plant          =  <item>-werks.
*        sales_items_in-store_loc      =  <item>-lgort.
*        sales_items_in-wbs_elem       =  data-posid. "WBS只放抬头
*        sales_items_in-ref_doc_ca     =  ''. "G
*        SALES_ITEMS_IN-CURRENCY       = data-WAERK.
        PERFORM setbapix USING sales_items_in CHANGING sales_items_inx.
        APPEND: sales_items_in,sales_items_inx.
        "计划行
        sales_schedules_in-itm_number = posnr ."
        sales_schedules_in-sched_line = '0001'."
        sales_schedules_in-req_qty    = <item>-kwmeng.
        sales_schedules_in-req_date   = sy-datum .
*       SALES_SCHEDULES_IN-REQ_DATE = KETDAT.
        PERFORM setbapix USING sales_schedules_in CHANGING sales_schedules_inx.
        APPEND: sales_schedules_in,sales_schedules_inx.


*增强字段
        CLEAR:wa_extp.
        CLEAR:extensionin.
        MOVE-CORRESPONDING <item> TO wa_extp.
        extensionin-structure = 'BAPE_VBAP'.
        CALL METHOD cl_abap_container_utilities=>fill_container_c
          EXPORTING
            im_value     = wa_extp
          IMPORTING
            ex_container = extensionin-valuepart1.
        CALL METHOD cl_abap_container_utilities=>fill_container_c
          EXPORTING
            im_value     = wa_extp+240
          IMPORTING
            ex_container = extensionin-valuepart2.
        CALL METHOD cl_abap_container_utilities=>fill_container_c
          EXPORTING
            im_value     = wa_extp+480
          IMPORTING
            ex_container = extensionin-valuepart3.
*        CALL METHOD cl_abap_container_utilities=>fill_container_c
*          EXPORTING
*            im_value     = wa_extp+720
*          IMPORTING
*            ex_container = extensionin-valuepart4.
        APPEND extensionin.
        CLEAR:extensionin ,wa_extpx .
        PERFORM setbapix USING wa_extp CHANGING wa_extpx.
        extensionin-structure = 'BAPE_VBAPX'.
        extensionin-valuepart1 = wa_extpx.
        APPEND extensionin.
        PERFORM setbapix USING sales_items_in CHANGING sales_items_inx.
        PERFORM setbapix USING sales_schedules_in CHANGING sales_schedules_inx.
*        APPEND: SALES_ITEMS_IN,SALES_ITEMS_INX,SALES_SCHEDULES_IN,SALES_SCHEDULES_INX.

*文本
        IF <item>-z001 IS NOT INITIAL.
          "切割文本
          CLEAR:text_stream,text_stream[],lines[],lines.
          text_stream-text = <item>-z001.
          APPEND text_stream.
          CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
            TABLES
              text_stream = text_stream
              itf_text    = lines.
          LOOP AT lines.
            CLEAR:order_text.
            "合同项目备注
            CLEAR:order_text.
            order_text-itm_number = posnr.
            order_text-text_id    = 'Z001'.
            order_text-langu      = sy-langu .
            order_text-format_col = '*' .
            order_text-text_line  = lines-tdline.
            APPEND order_text.
          ENDLOOP.
        ENDIF.
        "EDIT BY DONGPZ AT 26.11.2022 13:08:41 价格
        CLEAR:sales_conditions_in,sales_conditions_inx.
        sales_conditions_in-itm_number = posnr.
        sales_conditions_in-cond_type  = 'ZPR0'.
        sales_conditions_in-cond_value = <item>-kbetr.
        sales_conditions_in-cond_p_unt = 1.
        sales_conditions_in-currency   = data-waerk.
*        SALES_CONDITIONS_IN-CONDVALUE  = <item>-ZQYDJ * <item>-KWMENG.
*        SALES_CONDITIONS_IN-CURRENCY_2 = data-WAERK.
        PERFORM setbapix USING sales_conditions_in CHANGING sales_conditions_inx.
        APPEND:sales_conditions_in,sales_conditions_inx.
      ENDLOOP.
**********************************

      SET UPDATE TASK LOCAL.
      CALL FUNCTION 'SD_SALESDOCUMENT_CREATE'
        EXPORTING
          sales_header_in      = sales_header_in
          sales_header_inx     = sales_header_inx
        IMPORTING
          salesdocument_ex     = salesdocument_ex
        TABLES
          return               = return
          sales_items_in       = sales_items_in
          sales_items_inx      = sales_items_inx
          sales_partners       = sales_partners
          sales_schedules_in   = sales_schedules_in
          sales_schedules_inx  = sales_schedules_inx
          sales_conditions_in  = sales_conditions_in
          sales_conditions_inx = sales_conditions_inx
          sales_text           = order_text
          extensionin          = extensionin
        EXCEPTIONS
          OTHERS               = 1.

      IF sy-subrc NE 0.
        PERFORM msgtotext(zpubform) USING '' '' '' '' '' '' CHANGING rtmsg.
        ADD sy-subrc TO subrc.
      ENDIF.
      LOOP AT return WHERE type CA 'AEX'.
        CONCATENATE return-message rtmsg INTO rtmsg SEPARATED BY '/'.
      ENDLOOP.
      IF sy-subrc EQ 0.
        ADD 4 TO subrc.
      ENDIF.

      IF subrc = 0 AND salesdocument_ex IS NOT INITIAL.
        PERFORM bapirun(zpubform)  USING 'S'.
        rtype = 'S'.
        CONCATENATE '创建销售合同成功，单号：'  salesdocument_ex INTO rtmsg.
        vbeln = salesdocument_ex.
        fillmsg 'S' rtmsg.
      ELSE.
        PERFORM bapirun(zpubform) USING 'E'.
        fillmsg 'E' rtmsg.
      ENDIF.
    WHEN 'U'.
      checkinitial data-bstkd       '客户参考'            .
      checkinitial data-auart       '销售凭证类型'          .
      checkinitial data-kunnr_we    '售达方'             .
      checkinitial data-kunnr_ag    '送达方'             .
      checkinitial data-vkorg       '销售组织'            .
      checkinitial data-vtweg       '分销渠道'            .
      checkinitial data-prsdt       '凭证日期'            .
      checkinitial data-guebg       '有效期自'            .
      checkinitial data-gueen       '有效期至'            .
      checkinitial data-zhtyf       '合同月份'            .
      checkinitial data-spart       '产品组'             .
      checkinitial data-zhwlx       '货物类型'            .
      checkinitial data-vkbur       '销售办事处'           .
      checkinitial data-vkgrp       '销售组'             .
      checkinitial data-waerk       '凭证货币'            .
      checkinitial data-kursk       '汇率'              .
      checkinitial data-zterm       '付款条件'            .
      checkinitial data-zhtjgfs     '合同加工方式'          .
      checkinitial data-zdjbl       '定金比例'            .
      checkinitial data-zisdxs      '销售类型'            .
      checkinitial data-zcpyt       '产品用途'            .

      "补0
      PERFORM addzero(zpubform) CHANGING data-kunnr_we.
      PERFORM addzero(zpubform) CHANGING data-kunnr_we.
      PERFORM addzero(zpubform) CHANGING data-vbeln.

      IF data-items IS INITIAL.
        fillmsg 'E' '合同明细不能为空'.
      ENDIF.
      LOOP AT data-items ASSIGNING <item>.
*        mes = '第' && sy-tabix && '行,' && '物料'           .
*        checkinitial <item>-matnr mes.
        mes = '第' && sy-tabix && '行,' && '品名'           .
        checkinitial <item>-groes mes.
        mes = '第' && sy-tabix && '行,' && '厚度'           .
        checkinitial <item>-houdu mes.
        mes = '第' && sy-tabix && '行,' && '宽度'           .
        checkinitial <item>-width mes.
        mes = '第' && sy-tabix && '行,' && '材质'           .
        checkinitial <item>-caizhi mes.
        mes = '第' && sy-tabix && '行,' && '工厂'           .
        checkinitial <item>-werks mes.
        mes = '第' && sy-tabix && '行,' && '销售单位'         .
        checkinitial <item>-vrkme mes.
        mes = '第' && sy-tabix && '行,' && '订单数量'         .
        checkinitial <item>-kwmeng mes.
        mes = '第' && sy-tabix && '行,' && '价格'           .
        checkinitial <item>-kbetr mes.
        mes = '第' && sy-tabix && '行,' && '税率'           .
        checkinitial <item>-mwskz mes.

*单位转换
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
          EXPORTING
            input          = <item>-vrkme
            language       = sy-langu
          IMPORTING
            output         = <item>-vrkme
          EXCEPTIONS
            unit_not_found = 1
            OTHERS         = 2.
*        SELECT SINGLE
*          matnr
*          INTO @DATA(matnr)
*          FROM mara
*          WHERE matnr = @<item>-matnr
*          .
*        SELECT SINGLE
*          meinh
*          INTO @DATA(meinh)
*          FROM marm
*          WHERE matnr = @<item>-matnr
*          AND meinh = @<item>-vrkme
*          .
*        IF matnr IS NOT INITIAL AND meinh IS INITIAL.
*          CLEAR mes.
*          PERFORM addunit USING <item>-matnr <item>-vrkme CHANGING mess.
*          IF mess+0(1) = 'E'.
*            mes = '第' && sy-tabix && '行，物料：' && <item>-matnr && '单位:' && <item>-vrkme && '扩充失败！:' && mess.
*            rtmsg = mes.
*            fillmsg 'E' rtmsg.
*          ENDIF.
*        ENDIF.
*        CLEAR :matnr,meinh.
      ENDLOOP.

      CLEAR:salesdocument,order_item_in,order_item_inx,schedule_lines,schedule_linesx,
      order_header,order_headerx,order_text,conditions_in,conditions_inx,subrc.
      REFRESH:
      order_item_in,order_item_inx,schedule_lines,schedule_linesx,
      order_text,conditions_in,conditions_inx.

      SELECT
        vbak~vbeln
        vbap~posnr
        vbap~matnr
        vbap~werks
        vbap~pstyv
        vbkd~bstkd
        INTO CORRESPONDING FIELDS OF TABLE it_vbap
        FROM vbak
        JOIN vbap ON vbak~vbeln = vbap~vbeln
        JOIN vbkd ON vbak~vbeln = vbkd~vbeln AND vbkd~posnr = '000000'
        WHERE vbkd~bstkd = data-bstkd
        AND vbak~vbeln = data-vbeln
        AND vbak~vbtyp = 'G' "合同
        .
      IF sy-subrc NE 0.
        fillmsg 'E' '只允许创建或修改一个合同！' .
      ENDIF.
      READ TABLE it_vbap INDEX 1.
      "抬头
      salesdocument = data-vbeln.

*      order_header-doc_type   = data-auart ."订单类型
      order_header-sales_org  = data-vkorg ."销售组织
*      order_header-ord_reason = data-augru ."补差原因
      order_header-purch_no_c = data-bstkd ."外部合同号
      order_header-division   = data-spart ."产品组
      order_header-sales_grp  = data-vkgrp ."销售组
      order_header-distr_chan = data-vtweg ."分销渠道
      order_header-sales_off  = data-vkbur ."销售办事处
      order_header-doc_date   = data-prsdt."凭证日期 (接收/发送日期)
      order_header-currency   = data-waerk."货币
      order_header-ct_valid_f = data-guebg.
      order_header-ct_valid_t = data-gueen.

      order_headerx-updateflag = 'U'.
*      ORDER_HEADERX-DOC_TYPE   = 'X' .
      order_headerx-sales_org  = 'X' .
      order_headerx-ord_reason = 'X' .
      order_headerx-purch_no_c = 'X' .
      order_headerx-division   = 'X' .
      order_headerx-sales_grp  = 'X' .
      order_headerx-distr_chan = 'X' .
      order_headerx-doc_date   = 'X' .
      order_headerx-sales_off  = 'X' .
      order_headerx-ct_valid_f = 'X' .
      order_headerx-ct_valid_t = 'X' .
*增强字段
      CLEAR wa_extk.
      MOVE-CORRESPONDING data TO wa_extk.
      CLEAR:extensionin.
      extensionin-structure = 'BAPE_VBAK'.
      extensionin+30(960) = wa_extk.
*      CALL METHOD CL_ABAP_CONTAINER_UTILITIES=>FILL_CONTAINER_C
*        EXPORTING
*          IM_VALUE     = WA_EXTK
*        IMPORTING
*          EX_CONTAINER = EXTENSIONIN-VALUEPART1.
*      CALL METHOD CL_ABAP_CONTAINER_UTILITIES=>FILL_CONTAINER_C
*        EXPORTING
*          IM_VALUE     = WA_EXTK+240
*        IMPORTING
*          EX_CONTAINER = EXTENSIONIN-VALUEPART2.
*      CALL METHOD CL_ABAP_CONTAINER_UTILITIES=>FILL_CONTAINER_C
*        EXPORTING
*          IM_VALUE     = WA_EXTK+480
*        IMPORTING
*          EX_CONTAINER = EXTENSIONIN-VALUEPART3.
*      CALL METHOD CL_ABAP_CONTAINER_UTILITIES=>FILL_CONTAINER_C
*        EXPORTING
*          IM_VALUE     = WA_EXTK+720
*        IMPORTING
*          EX_CONTAINER = EXTENSIONIN-VALUEPART4.
      APPEND extensionin.
      CLEAR:extensionin.
      PERFORM setbapix USING wa_extk CHANGING wa_extkx.
      extensionin-structure = 'BAPE_VBAKX'.
      extensionin-valuepart1 = wa_extkx.
      APPEND extensionin.

      CLEAR:order_text[].
      IF data-z001 IS NOT INITIAL.
        "切割文本
        CLEAR:text_stream,text_stream[],lines[],lines.
        text_stream-text = data-z001.
        APPEND text_stream.
        CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
          TABLES
            text_stream = text_stream
            itf_text    = lines.
        LOOP AT lines.
          CLEAR:order_text.
          "抬头备注
          order_text-text_id    = 'Z001'.
          order_text-langu      = sy-langu.
          order_text-format_col = '*' .
          order_text-text_line  = lines-tdline.
          APPEND order_text.
        ENDLOOP.
      ENDIF.
      IF data-z002 IS NOT INITIAL.
        "切割文本
        CLEAR:text_stream,text_stream[],lines[],lines.
        text_stream-text = data-z002.
        APPEND text_stream.
        CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
          TABLES
            text_stream = text_stream
            itf_text    = lines.
        LOOP AT lines.
          CLEAR:order_text.
          "合同抬头备注
          order_text-text_id    = 'Z002'.
          order_text-langu      = sy-langu.
          order_text-format_col = '*' .
          order_text-text_line  = lines-tdline.
          APPEND order_text.
        ENDLOOP.
      ENDIF.
      IF data-z006 IS NOT INITIAL.
        "切割文本
        CLEAR:text_stream,text_stream[],lines[],lines.
        text_stream-text = data-z006.
        APPEND text_stream.
        CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
          TABLES
            text_stream = text_stream
            itf_text    = lines.
        LOOP AT lines.
          CLEAR:order_text.
          "质量要求标准
          order_text-text_id    = 'Z006'.
          order_text-langu      = sy-langu.
          order_text-format_col = '*' .
          order_text-text_line  = lines-tdline.
          APPEND order_text.
        ENDLOOP.
      ENDIF.
      IF data-z007 IS NOT INITIAL.
        "切割文本
        CLEAR:text_stream,text_stream[],lines[],lines.
        text_stream-text = data-z007.
        APPEND text_stream.
        CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
          TABLES
            text_stream = text_stream
            itf_text    = lines.
        LOOP AT lines.
          CLEAR:order_text.
          "运输方式以及运费承担
          order_text-text_id    = 'Z007'.
          order_text-langu      = sy-langu.
          order_text-format_col = '*' .
          order_text-text_line  = lines-tdline.
          APPEND order_text.
        ENDLOOP.
      ENDIF.
      IF data-z008 IS NOT INITIAL.
        "切割文本
        CLEAR:text_stream,text_stream[],lines[],lines.
        text_stream-text = data-z008.
        APPEND text_stream.
        CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
          TABLES
            text_stream = text_stream
            itf_text    = lines.
        LOOP AT lines.
          CLEAR:order_text.
          "支付方式
          order_text-text_id    = 'Z008'.
          order_text-langu      = sy-langu.
          order_text-format_col = '*' .
          order_text-text_line  = lines-tdline.
          APPEND order_text.
        ENDLOOP.
      ENDIF.
      IF data-z009 IS NOT INITIAL.
        "切割文本
        CLEAR:text_stream,text_stream[],lines[],lines.
        text_stream-text = data-z009.
        APPEND text_stream.
        CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
          TABLES
            text_stream = text_stream
            itf_text    = lines.
        LOOP AT lines.
          CLEAR:order_text.
          "备注
          order_text-text_id    = 'Z009'.
          order_text-langu      = sy-langu.
          order_text-format_col = '*' .
          order_text-text_line  = lines-tdline.
          APPEND order_text.
        ENDLOOP.
      ENDIF.

**********************************
*      "合作伙伴
*      SALES_PARTNERS-PARTN_ROLE = 'AG'.
*      SALES_PARTNERS-PARTN_NUMB = data-KUNNR.
*      PERFORM ADDZERO(ZPUBFORM) CHANGING SALES_PARTNERS-PARTN_NUMB.
*      APPEND SALES_PARTNERS.
*      CLEAR SALES_PARTNERS.
**********************************
      "行项目
      "新增行取最大行项目
      SELECT MAX( vbap~posnr )
        INTO posnr
        FROM vbap
        INNER JOIN vbkd ON vbkd~vbeln = vbap~vbeln
        WHERE bstkd = data-bstkd
        AND vbkd~posnr = '000000'.

      SORT it_vbap BY bstkd posnr.
      LOOP AT data-items ASSIGNING <item> .
        "洁净 虚拟物料 不参于更新
        " modify 20230721 by hanwq
*        IF <item>-matnr IN s_matnr.
*          CONTINUE.
*        ENDIF.
*        IF <item>-uepos IS NOT INITIAL.
*          "下层行项目 且 上层行项目更改物料 则 删除 ，不更新 IT_VBAP-ZNODEL
*          READ TABLE it_vbap WITH KEY posnr = <item>-uepos zmatnr = 'X'.
*          IF sy-subrc = 0 .
*            CONTINUE.
*          ENDIF.
*        ENDIF.
        READ TABLE it_vbap WITH KEY bstkd = data-bstkd posnr = <item>-posnr BINARY SEARCH.
        IF sy-subrc = 0 .
          "更新未被删除标志
          it_vbap-znodel = 'X'.
          MODIFY it_vbap INDEX sy-tabix TRANSPORTING znodel.

          CLEAR:order_item_in, order_item_inx, schedule_lines,schedule_linesx,wa_extp,wa_extpx.
          order_item_in-itm_number  = it_vbap-posnr.
          order_item_inx-itm_number = it_vbap-posnr.
          IF <item>-matnr <> it_vbap-matnr OR it_vbap-pstyv <> <item>-pstyv OR it_vbap-werks <> <item>-werks."项目类别.
            it_vbap-zmatnr = 'X'.
            MODIFY it_vbap INDEX sy-tabix TRANSPORTING zmatnr.
            order_item_inx-updateflag = 'U'.
            APPEND: order_item_in,order_item_inx.
          ELSE.
            order_item_in-target_qu    = <item>-vrkme.
            order_item_in-sales_unit   = <item>-vrkme.
*            PERFORM transunit_to_inside(zpubform) CHANGING order_item_in-target_qu.
*            PERFORM transunit_to_inside(zpubform) CHANGING order_item_in-sales_unit.
            order_item_inx-target_qu   = 'X' .
            order_item_inx-sales_unit  = 'X' .
*            order_item_in-prc_group2  = <item>-mvgr2.
*            order_item_in-prc_group3  = <item>-mvgr3.
*            order_item_in-reason_rej  = <item>-abgru.
*            order_item_in-hg_lv_item  = <item>-uepos."上一层项目
*            IF data-auart = 'ZWM0' OR data-vkorg = '3210'.     "只有外贸允许修改税码 "增加马来西亚公司修改税码逻辑 By Qidawei On 20240805
*              order_item_in-tax_class1     = data-zsl.    "税码调整 add by yangyousheng 20221209
*              order_item_inx-tax_class1  = 'X' . "20221230_likun,修改时因税率变更为空。
*            ENDIF.
            order_item_in-target_qty  = <item>-kwmeng.
*            ORDER_ITEM_IN-PLANT       =  <item>-WERKS.

*          ORDER_ITEM_INX-MATERIAL    = 'X' .
*            order_item_inx-prc_group2  = 'X' .
*            order_item_inx-prc_group3  = 'X' .
*            order_item_inx-hg_lv_item  = 'X' ."上一层项目
*          ORDER_ITEM_INX-TAX_CLASS1  = 'X' .
*            order_item_inx-reason_rej  = 'X' .
            order_item_inx-target_qty  = 'X' .
*            ORDER_ITEM_INX-PLANT       = 'X' .
            order_item_inx-updateflag = 'U'.
            APPEND: order_item_in,order_item_inx.
            "计划行
            schedule_lines-itm_number = <item>-posnr.
            schedule_lines-sched_line = '0001'.
            schedule_lines-req_qty    = order_item_in-target_qty.
            schedule_lines-req_date   = sy-datum .
            PERFORM setbapix USING schedule_lines CHANGING schedule_linesx.
            schedule_linesx-updateflag = 'U'.
            APPEND:schedule_lines,schedule_linesx.
          ENDIF.

*文本
          IF <item>-z001 IS NOT INITIAL.
            "切割文本
            CLEAR:text_stream,text_stream[],lines[],lines.
            text_stream-text = <item>-z001.
            APPEND text_stream.
            CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
              TABLES
                text_stream = text_stream
                itf_text    = lines.
            LOOP AT lines.
              CLEAR:order_text.
              "合同项目备注
              CLEAR:order_text.
              order_text-itm_number = posnr.
              order_text-text_id    = 'Z001'.
              order_text-langu      = sy-langu .
              order_text-format_col = '*' .
              order_text-text_line  = lines-tdline.
              APPEND order_text.
            ENDLOOP.
          ENDIF.
*增强字段
          CLEAR:wa_extp.
          MOVE-CORRESPONDING <item> TO wa_extp.
*          WA_EXTP-VBELN  = data-VBELN.
          CLEAR:extensionin.
          extensionin-structure = 'BAPE_VBAP'.
          CALL METHOD cl_abap_container_utilities=>fill_container_c
            EXPORTING
              im_value     = wa_extp
            IMPORTING
              ex_container = extensionin-valuepart1.
          CALL METHOD cl_abap_container_utilities=>fill_container_c
            EXPORTING
              im_value     = wa_extp+240
            IMPORTING
              ex_container = extensionin-valuepart2.
          CALL METHOD cl_abap_container_utilities=>fill_container_c
            EXPORTING
              im_value     = wa_extp+480
            IMPORTING
              ex_container = extensionin-valuepart3.
*          CALL METHOD cl_abap_container_utilities=>fill_container_c
*            EXPORTING
*              im_value     = wa_extp+720
*            IMPORTING
*              ex_container = extensionin-valuepart4.

          APPEND extensionin.
          CLEAR:extensionin,wa_extpx .
          PERFORM setbapix_initial USING wa_extp CHANGING wa_extpx.
          extensionin-structure = 'BAPE_VBAPX'.
          extensionin-valuepart1 = wa_extpx.
          APPEND extensionin.

          SELECT SINGLE
            vbak~*
            INTO @DATA(vbak)
            FROM vbak
            WHERE vbak~vbeln = @data-vbeln.

          CLEAR:conditions_in,conditions_inx.
          PERFORM fillcond USING it_vbap-posnr 'ZPR0'  <item>-kbetr vbak
                CHANGING conditions_in conditions_inx.
          APPEND:conditions_in,conditions_inx.
        ELSE.
          ADD 10 TO posnr.
          <item>-posnr = posnr.
          CLEAR:order_item_in, order_item_inx, schedule_lines,schedule_linesx,wa_extp,wa_extpx.
          order_item_in-itm_number  = posnr.
          order_item_in-material    = <item>-matnr.
*          order_item_in-prc_group2  = <item>-mvgr2.
*          order_item_in-prc_group3  = <item>-mvgr3.
*          order_item_in-reason_rej  = <item>-abgru.
*          order_item_in-wbs_elem    = data-posid.
          order_item_in-target_qu   = <item>-vrkme.
          order_item_in-sales_unit  = <item>-vrkme.
          PERFORM transunit_to_inside(zpubform) CHANGING order_item_in-target_qu.
          PERFORM transunit_to_inside(zpubform) CHANGING order_item_in-sales_unit.
          order_item_in-plant       = <item>-werks.
          PERFORM addzero(zpubform) CHANGING order_item_in-material.
          order_item_in-item_categ  = <item>-pstyv."项目类别
*          order_item_in-hg_lv_item  = <item>-uepos."上一层项目
          order_item_in-plant       = <item>-werks.
*          IF data-auart = 'ZWM0' OR data-vkorg = '3210'.     "只有外贸允许修改税码 "增加马来西亚公司修改税码逻辑 By Qidawei On 20240805
*            order_item_in-tax_class1     = data-zsl.    "税码调整 add by yangyousheng 20221209
*          ENDIF.
          order_item_in-target_qty  = <item>-kwmeng.
          schedule_lines-itm_number = <item>-posnr.
          schedule_lines-sched_line = '0001'.
          schedule_lines-req_qty    = order_item_in-target_qty.
          schedule_lines-req_date   = sy-datum .
          PERFORM setbapix USING order_item_in CHANGING order_item_inx.
          PERFORM setbapix USING schedule_lines CHANGING schedule_linesx.
          order_item_inx-updateflag  = 'I'.
          schedule_linesx-updateflag = 'I'.
          APPEND: order_item_in,order_item_inx,schedule_lines,schedule_linesx.

*文本
          IF <item>-z001 IS NOT INITIAL.
            "切割文本
            CLEAR:text_stream,text_stream[],lines[],lines.
            text_stream-text = <item>-z001.
            APPEND text_stream.
            CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
              TABLES
                text_stream = text_stream
                itf_text    = lines.
            LOOP AT lines.
              CLEAR:order_text.
              "合同项目备注
              CLEAR:order_text.
              order_text-itm_number = posnr.
              order_text-text_id    = 'Z001'.
              order_text-langu      = sy-langu .
              order_text-format_col = '*' .
              order_text-text_line  = lines-tdline.
              APPEND order_text.
            ENDLOOP.
          ENDIF.
*增强字段
          CLEAR:wa_extp.
          MOVE-CORRESPONDING <item> TO wa_extp.
          wa_extp-vbeln  = data-vbeln.
          CLEAR:extensionin.
          extensionin-structure = 'BAPE_VBAP'.
          CALL METHOD cl_abap_container_utilities=>fill_container_c
            EXPORTING
              im_value     = wa_extp
            IMPORTING
              ex_container = extensionin-valuepart1.
          CALL METHOD cl_abap_container_utilities=>fill_container_c
            EXPORTING
              im_value     = wa_extp+240
            IMPORTING
              ex_container = extensionin-valuepart2.
          CALL METHOD cl_abap_container_utilities=>fill_container_c
            EXPORTING
              im_value     = wa_extp+480
            IMPORTING
              ex_container = extensionin-valuepart3.
*          CALL METHOD cl_abap_container_utilities=>fill_container_c
*            EXPORTING
*              im_value     = wa_extp+720
*            IMPORTING
*              ex_container = extensionin-valuepart4.
          APPEND extensionin.
          CLEAR:extensionin ,wa_extpx .
          PERFORM setbapix USING wa_extp CHANGING wa_extpx.
          extensionin-structure = 'BAPE_VBAPX'.
          extensionin-valuepart1 = wa_extpx.
          APPEND extensionin.

          PERFORM setbapix USING sales_items_in CHANGING sales_items_inx.
          PERFORM setbapix USING sales_schedules_in CHANGING sales_schedules_inx.
          APPEND: sales_items_in,sales_items_inx,sales_schedules_in,sales_schedules_inx.

          "价格
          CLEAR:conditions_in,conditions_inx.
          conditions_in-itm_number = <item>-posnr.
          conditions_in-cond_type  = 'ZPR0'.
          conditions_in-cond_value =  <item>-kbetr .
          conditions_in-cond_p_unt = 1.
*          CONDITIONS_IN-CURRENCY   = 'CNY'.
          conditions_in-currency   = data-waerk.
          PERFORM setbapix USING conditions_in CHANGING conditions_inx.
          conditions_inx-updateflag = 'I'.
          APPEND:conditions_in,conditions_inx.
        ENDIF.
      ENDLOOP.
*      "先删除 行项目WBS号再删除
*      LOOP AT IT_VBAP WHERE ZNODEL = ''.
*        CLEAR:ORDER_ITEM_IN, ORDER_ITEM_INX.
*        ORDER_ITEM_IN-ITM_NUMBER  = IT_VBAP-POSNR.
*        ORDER_ITEM_IN-WBS_ELEM    = ''.
*        PERFORM SETBAPIX USING ORDER_ITEM_IN CHANGING ORDER_ITEM_INX.
*        ORDER_ITEM_INX-UPDATEFLAG  = 'U'.
*        ORDER_ITEM_INX-WBS_ELEM    = 'X'.
*        APPEND: ORDER_ITEM_IN,ORDER_ITEM_INX.
*      ENDLOOP.
      LOOP AT it_vbap WHERE znodel = ''.
        "洁净 虚拟物料 不参于更新
        " modify 20230721 by hanwq
*        IF it_vbap-matnr IN s_matnr.
*          CONTINUE.
*        ENDIF.
        CLEAR:order_item_in, order_item_inx.
        order_item_in-itm_number  = it_vbap-posnr.
        PERFORM setbapix USING order_item_in CHANGING order_item_inx.
        order_item_inx-updateflag  = 'D'.
        APPEND: order_item_in,order_item_inx.
      ENDLOOP.
*调用BAPI_SALESORDER_CHANGE bapi
*增强字段内存传值更新
*      EXPORT I_VBAK = VBAKENH
*             T_VBAP = VBAPENH[]
*             TO MEMORY ID 'MEMO_ZSSDVBAK_VBAP'.
      SET UPDATE TASK LOCAL.
      CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
        EXPORTING
          salesdocument    = salesdocument
          order_header_in  = order_header
          order_header_inx = order_headerx
        TABLES
          return           = return
          order_item_in    = order_item_in
          order_item_inx   = order_item_inx
          schedule_lines   = schedule_lines
          schedule_linesx  = schedule_linesx
          order_text       = order_text
          conditions_in    = conditions_in
          conditions_inx   = conditions_inx
          extensionin      = extensionin
        EXCEPTIONS
          OTHERS           = 1.

      IF sy-subrc NE 0.
        PERFORM msgtotext(zpubform) USING '' '' '' '' '' '' CHANGING rtmsg.
        ADD sy-subrc TO subrc.
      ENDIF.
      LOOP AT return WHERE type CA 'AEX'.
        CONCATENATE return-message rtmsg INTO rtmsg SEPARATED BY '/'.
      ENDLOOP.
      IF sy-subrc NE 0.
        SET UPDATE TASK LOCAL.
        PERFORM bapirun(zpubform)  USING 'S'.
        CONCATENATE '更改销售合同成功，单号：' salesdocument  INTO rtmsg.
        vbeln = salesdocument.
        fillmsg 'S' rtmsg.
      ELSE.
        PERFORM bapirun(zpubform) USING 'E'.
        fillmsg 'E' rtmsg.
      ENDIF.
    WHEN 'D'.
      SELECT SINGLE
        vbak~vbeln
        INTO @vbeln
        FROM vbak
        JOIN vbkd ON vbak~vbeln = vbkd~vbeln AND vbkd~posnr = '000000'
        WHERE vbak~vbeln = @data-vbeln
        AND vbkd~bstkd = @data-bstkd
        .
      IF sy-subrc <> 0.
        fillmsg 'E' '未查到合同！' .
      ENDIF.
      salesdocument = vbeln.
      CLEAR:order_header_in, order_header_inx.
*      ORDER_HEADER_IN-COLLECT_NO = VBELN.
      order_header_inx-updateflag  = 'D'.

*调用BAPI_SALESORDER_CHANGE bapi
      CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
        EXPORTING
          salesdocument    = salesdocument
          order_header_in  = order_header_in
          order_header_inx = order_header_inx
        TABLES
          return           = return
        EXCEPTIONS
          OTHERS           = 1.

      IF sy-subrc NE 0.
        PERFORM msgtotext(zpubform) USING '' '' '' '' '' '' CHANGING rtmsg.
        ADD sy-subrc TO subrc.
      ENDIF.
      LOOP AT return WHERE type CA 'AEX'.
        CONCATENATE return-message rtmsg INTO rtmsg SEPARATED BY '/'.
      ENDLOOP.
      IF sy-subrc = 0 .
        PERFORM bapirun(zpubform) USING 'E'.
        fillmsg 'E' rtmsg.
      ELSE.
        PERFORM bapirun(zpubform)  USING 'S'.
        CONCATENATE '删除销售合同成功，单号：'  salesdocument_ex INTO rtmsg.
        fillmsg 'S' rtmsg.
      ENDIF.
    WHEN OTHERS.
      rtmsg = '不支持保存、新增、更新、删除以外其他功能'.
      fillmsg 'E' rtmsg.
  ENDCASE.


  zfmdatasave2 'R'.


ENDFUNCTION.


FORM fillcond USING p_posnr
                    p_kschl
*                    P_UNT
                    p_kbetr
                    p_vbak TYPE vbak
              CHANGING o_cond STRUCTURE bapicond
                       o_condx STRUCTURE bapicondx.
  TABLES:t683s.

  DATA:kalsm TYPE t683v-kalsm.
  CLEAR:o_cond,o_condx,t683s.
*  CHECK P_KSCHL IS NOT INITIAL
*  AND P_KBETR IS NOT INITIAL.

  PERFORM getkalsm(zpubform) USING p_vbak p_kschl
        CHANGING kalsm.

  SELECT SINGLE * FROM t683s
   WHERE kvewe = 'A'
     AND kappl = 'V'
     AND kalsm = kalsm
     AND kschl = p_kschl.
  o_cond-itm_number  = p_posnr.
  o_condx-itm_number = p_posnr.
  o_cond-cond_st_no  = t683s-stunr.
  o_condx-cond_st_no = t683s-stunr.
  o_cond-cond_count  = '01'.
  o_condx-cond_count = '01'.
  o_cond-cond_type   = p_kschl.
  o_condx-cond_type  = p_kschl.
  o_cond-cond_p_unt = '1'.
  o_condx-cond_p_unt = 'X'.
  o_cond-cond_value  = p_kbetr.
  o_condx-cond_value = 'X'.
*  O_COND-CONDVALUE   = P_KBETR * 10.
  o_cond-currency    = p_vbak-waerk.
  IF t683s IS NOT INITIAL.
    o_condx-updateflag = 'U'.
  ELSE.
    o_condx-updateflag = 'I'.
  ENDIF.
ENDFORM.


FORM setbapix USING fs CHANGING fsx    .
  FIELD-SYMBOLS : <fs>,<fsx>.
  DATA: outfieldcat TYPE slis_t_fieldcat_alv,
        wa_field    TYPE slis_fieldcat_alv.
  CLEAR:outfieldcat,wa_field,fsx.

  PERFORM gettabstru_se11(zpubform) USING fsx CHANGING outfieldcat.

  LOOP AT outfieldcat INTO wa_field.
    ASSIGN COMPONENT wa_field-fieldname OF STRUCTURE fs TO <fs>.
    IF sy-subrc <> 0 .
      CONTINUE.
    ENDIF.
    IF <fs> IS NOT INITIAL.
      ASSIGN COMPONENT wa_field-fieldname OF STRUCTURE fsx TO <fsx>.
      IF sy-subrc <> 0 .
        EXIT.
      ENDIF.
*对于长度为1，但是不是赋值X的字段特殊处理
      IF wa_field-rollname = 'CHAR1' OR wa_field-rollname = 'BAPIUPDATE' .
        <fsx> = 'X'.
      ELSE.
        <fsx> = <fs>.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.

FORM setbapix_initial USING fs CHANGING fsx .

  FIELD-SYMBOLS : <fs>,<fsx>.
  DATA: outfieldcat TYPE slis_t_fieldcat_alv,
        wa_field    TYPE slis_fieldcat_alv.
  CLEAR:outfieldcat,wa_field,fsx.

  PERFORM gettabstru_se11(zpubform) USING fsx CHANGING outfieldcat.

  LOOP AT outfieldcat INTO wa_field.
    ASSIGN COMPONENT wa_field-fieldname OF STRUCTURE fs TO <fs>.
    IF sy-subrc <> 0 .
      CONTINUE.
    ENDIF.
*    IF <FS> IS NOT INITIAL.
    ASSIGN COMPONENT wa_field-fieldname OF STRUCTURE fsx TO <fsx>.
    IF sy-subrc <> 0 .
      EXIT.
    ENDIF.
*对于长度为1，但是不是赋值X的字段特殊处理
    IF wa_field-rollname = 'CHAR1' OR wa_field-rollname = 'BAPIUPDATE' .
      <fsx> = 'X'.
    ELSE.
      <fsx> = <fs>.
    ENDIF.
*    ENDIF.
  ENDLOOP.

ENDFORM.

FORM addunit USING matnr wsmei CHANGING message.

  DATA: headdata        TYPE bapimathead,
        return          TYPE bapiret2,
        clientdata      TYPE bapi_mara,
        clientdatax     TYPE bapi_marax,
        returnmessages  TYPE TABLE OF bapi_matreturn2 WITH HEADER LINE,
        unitsofmeasure  TYPE TABLE OF bapi_marm WITH HEADER LINE,
        unitsofmeasurex TYPE TABLE OF bapi_marmx WITH HEADER LINE.

  headdata-material = matnr.
  clientdata-uomusage = 'B'.
  clientdatax-uomusage = 'X'.
*维护比例产量单位
  IF wsmei IS NOT INITIAL.
    CLEAR:unitsofmeasure,unitsofmeasurex.
    unitsofmeasure-alt_unit = wsmei.
    unitsofmeasure-numerator = 1.
    unitsofmeasure-denominatr = 1.
    unitsofmeasurex-alt_unit = wsmei.
    unitsofmeasurex-numerator = 'X'.
    unitsofmeasurex-denominatr = 'X'.
    APPEND:unitsofmeasure,unitsofmeasurex.
  ENDIF.

  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
    EXPORTING
      headdata        = headdata
      clientdata      = clientdata
      clientdatax     = clientdatax
    IMPORTING
      return          = return
    TABLES
      returnmessages  = returnmessages
      unitsofmeasure  = unitsofmeasure
      unitsofmeasurex = unitsofmeasurex.

  LOOP AT returnmessages WHERE type CA 'AEX'.
    EXIT.
  ENDLOOP.
  IF sy-subrc EQ 0 OR return-type CA 'AEX'.
    message = 'E'.
    LOOP AT returnmessages WHERE type CA 'AEX'.
      message = message && returnmessages-message && '/' .
    ENDLOOP.
    message = message && return-message && '/' .
    PERFORM bapirun(zpubform) USING ''.
  ELSE.
    SET UPDATE TASK LOCAL.
    PERFORM bapirun(zpubform) USING 'X'.
    message = 'S:成功'.
  ENDIF.

ENDFORM.
