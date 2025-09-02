FUNCTION zfm_crm_bp_change.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(DATA) TYPE  ZSCRM_BP
*"  EXPORTING
*"     VALUE(RTYPE) TYPE  BAPI_MTYPE
*"     VALUE(RTMSG) TYPE  BAPI_MSG
*"----------------------------------------------------------------------
  zfmdatasave1 'ZFM_CRM_BP_CHANGE'.
  zfmdatasave2 'B'.
  COMMIT WORK.
  IF data IS INITIAL.
    fillmsg 'E' '请传入DATA数据后再调用本接口'.
  ENDIF.
  CLEAR:ls_partner          ,
        ls_customer         ,
        lt_addresses        ,
        ls_addresses        ,
        lt_phone            ,
        ls_phone            ,
        lt_remarks_a        ,
        ls_remarks_a        ,
        lt_bank             ,
        ls_bank             ,
        it_taxnumbers       ,
        wa_taxnumbers       ,
        lt_tax_ind          ,
        ls_tax_ind          ,
        ls_header           ,
        ls_central_data     ,
        ls_company_data     ,
        lt_company          ,
        ls_company          ,
        ls_sales_data       ,
        lt_sales            ,
        ls_sales            ,
        ls_data             ,
        lt_data             ,
        lt_return           ,
        ls_return           ,
        ls_msg              ,
        lt_partnerguid_list ,
        ls_partnerguid_list ,
        lt_smtp             ,
        ls_smtp             ,
        lt_roles            ,
        ls_roles            .
  CLEAR:mdm_intab,mdm_intab[],mdm_outtab,mdm_outtab[],mdm_rtype,mdm_rtmsg.
  CLEAR:rtype,rtmsg.

  DATA : lv_msg TYPE string.
  DATA : lv_msg1 TYPE string.

  DATA:lt_return_ukmbp1 TYPE ukm_t_monitor_return,
       lt_return_frg1   TYPE TABLE OF bapiret2 WITH HEADER LINE.

*补前导零
  PERFORM addzero(zpubform) CHANGING data-kunnr.
  SELECT
    partner,
    partner_guid,
    name_org1
    FROM but000
    WHERE partner = @data-kunnr
    INTO TABLE @DATA(it_but000)
    .
  IF sy-subrc NE 0.
    rtmsg = |客户编码：{ data-kunnr }不存在，请核实数据后再试|.
    fillmsg 'E' rtmsg.
  ENDIF.
  SELECT
    kunnr,
    eikto,
    vsort
    FROM knvv
    WHERE kunnr = @data-kunnr
    INTO TABLE @DATA(it_knvv)
    .
  SELECT
    kna1~kunnr,
    kna1~land1,
    kna1~regio,
    t005t~landx,
    t005u~bezei AS bezei20,
    kna1~ort01,
    kna1~telf1,
    adrc~str_suppl3,
    kna1~name4
    FROM kna1
    JOIN t005t ON kna1~land1 = t005t~land1
    JOIN t005u ON kna1~regio = t005u~bland AND t005u~spras = @sy-langu AND kna1~land1 = t005u~land1
    JOIN adrc ON adrc~addrnumber = kna1~adrnr
    WHERE kunnr = @data-kunnr
    AND t005t~spras = @sy-langu
    INTO TABLE @DATA(it_kna1)
    .
  SELECT
    kunnr
    FROM kna1
    WHERE kunnr = @data-kunnr
    INTO TABLE @DATA(it_kna2)
    .
  SELECT *
    FROM but020
    WHERE partner = @data-kunnr
    INTO TABLE @DATA(it_but020)
    .
  SELECT *
    FROM tb003i
    WHERE rltgr = 'ZWSDCU'
    INTO TABLE @DATA(it_tb003i)
    .
  SELECT
    bankl,
    bankn,
    partner,
    banks,
    bkext,
    accname
    FROM but0bk
    WHERE partner = @data-kunnr
    INTO TABLE @DATA(it_but0bk)
    .

*  SORT it_but000.
  READ TABLE it_but000 INTO DATA(wa_but000) WITH KEY partner = data-kunnr.
  IF sy-subrc = 0.
    ls_partner-header-object_task = 'U'.
    ls_partner-header-object_instance-bpartnerguid = wa_but000-partner_guid.
    ls_partner-header-object_instance-bpartner = wa_but000-partner.

    ls_customer-header-object_instance-kunnr = ''.
    ls_customer-header-object_task = 'U'.
  ENDIF.

  "扩展角色
  DATA memo(200).
  LOOP AT it_tb003i ASSIGNING FIELD-SYMBOL(<it_tb003i>).
    PERFORM create_role IN PROGRAM zsdb002 USING data-kunnr <it_tb003i>-role CHANGING memo.
  ENDLOOP.

  IF data-ktokd IS NOT INITIAL.
    ls_partner-central_data-common-data-bp_control-grouping = data-ktokd.  " 分组
  ENDIF.

  IF data-name1 IS NOT INITIAL.
    ls_partner-central_data-common-data-bp_organization-name1 = data-name1. " 名称1
    ls_partner-central_data-common-datax-bp_organization-name1 = abap_true.
  ENDIF.

  IF data-name2 IS NOT INITIAL.
    ls_partner-central_data-common-data-bp_organization-name2 = data-name2.  " 名称2
    ls_partner-central_data-common-datax-bp_organization-name2 = abap_true.
  ENDIF.

  IF data-name_org4 IS NOT INITIAL.
    ls_partner-central_data-common-data-bp_organization-name4 = data-name_org4.  " 法人
    ls_partner-central_data-common-datax-bp_organization-name4 = abap_true.
  ENDIF.

  IF data-found_dat IS NOT INITIAL.
    ls_partner-central_data-common-data-bp_organization-foundationdate = data-found_dat.   "组织成立日期
    ls_partner-central_data-common-datax-bp_organization-foundationdate = abap_true.
  ENDIF.

  IF data-sortl IS NOT INITIAL.
    ls_partner-central_data-common-data-bp_centraldata-searchterm1 = data-sortl. "业务伙伴的搜索词 1（简称）
    ls_partner-central_data-common-datax-bp_centraldata-searchterm1 = abap_true.
  ENDIF.

  IF data-natpers IS NOT INITIAL.
    ls_partner-central_data-taxnumber-common-data-nat_person = data-natpers.
    ls_partner-central_data-taxnumber-common-datax-nat_person = abap_true.

  ENDIF.

  ls_partner-central_data-common-data-bp_centraldata-partnerlanguage = sy-langu.
  ls_partner-central_data-common-datax-bp_centraldata-partnerlanguage = abap_true.


  "银行基本户逻辑
  LOOP AT data-banks ASSIGNING FIELD-SYMBOL(<bank>).
    IF <bank>-xezer = 'X'.
      CLEAR:lt_bank,ls_bank.
      LOOP AT it_but0bk INTO DATA(wa_but0bk) WHERE partner = data-kunnr.

        ls_bank-task = 'U'.
        ls_bank-data-coll_auth  = ''.    "基本户
        ls_bank-datax-coll_auth  = abap_true.

        ls_bank-data-bankaccountname =  wa_but0bk-accname. "银行名称
        ls_bank-datax-bankaccountname  = abap_true.

        ls_bank-data-bank_ctry =  wa_but0bk-banks.    "银行国家代码
        ls_bank-datax-bank_ctry = abap_true.

        ls_bank-data-bank_key  =  wa_but0bk-bankl.    "联行号
        ls_bank-datax-bank_key  = abap_true.

        ls_bank-data-bank_acct  =  wa_but0bk-bankn.
        ls_bank-datax-bank_acct  = abap_true.

        ls_bank-data-externalbankid  =  wa_but0bk-bkext.    "账号2
        ls_bank-datax-externalbankid  = abap_true.

        APPEND ls_bank TO lt_bank.
        ls_partner-central_data-bankdetail-bankdetails = lt_bank.

      ENDLOOP.

      CLEAR:ls_data,lt_data.
      ls_data-partner = ls_partner.
      ls_data-customer = ls_customer.

      APPEND ls_data TO lt_data.
      "删除基本户标识
      CLEAR:lt_return,lv_msg.
      CALL FUNCTION 'CVI_EI_INBOUND_MAIN'
        EXPORTING
          i_data   = lt_data
*         I_EXT_DATA       =
        IMPORTING
          e_return = lt_return.

      LOOP AT lt_return INTO ls_return.
        LOOP AT ls_return-object_msg INTO ls_msg WHERE type = 'E' OR type = 'A' OR type = 'X'.
          CONCATENATE lv_msg ls_msg-message INTO lv_msg.
        ENDLOOP.
      ENDLOOP.

      IF lv_msg IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.
        "新增一条银行
        ls_bank-task = 'I'.
        ls_bank-data-coll_auth  = <bank>-xezer.    "基本户
        ls_bank-data-bank_ctry = <bank>-banks.    "银行国家代码
        ls_bank-data-bank_key  = <bank>-bankl.    "联行号
        ls_bank-data-bankaccountname = <bank>-accname. "银行名称
        ls_bank-data-accountholder  = <bank>-koinh.    "户主
        ls_bank-data-bank_acct  = <bank>-bankn.    "账号1
        ls_bank-data-externalbankid  = <bank>-bkext.    "账号2

        ls_bank-datax-coll_auth  = abap_true.
        ls_bank-datax-bank_ctry = abap_true.
        ls_bank-datax-bank_key  = abap_true.
        ls_bank-datax-bankaccountname  = abap_true.
        ls_bank-datax-accountholder  = abap_true.
        ls_bank-datax-bank_acct  = abap_true.
        ls_bank-datax-externalbankid  = abap_true.

        APPEND ls_bank TO lt_bank.
        ls_partner-central_data-bankdetail-bankdetails = lt_bank.
      ENDIF.
    ELSE.
      ls_bank-task = 'I'.
      ls_bank-data-coll_auth  = <bank>-xezer.    "基本户
      ls_bank-datax-coll_auth  = abap_true.

      ls_bank-data-bankaccountname =  <bank>-accname. "银行名称
      ls_bank-datax-bankaccountname  = abap_true.

      ls_bank-data-bank_ctry =  <bank>-banks.    "银行国家代码
      ls_bank-datax-bank_ctry = abap_true.

      ls_bank-data-bank_key  =  <bank>-bankl.    "联行号
      ls_bank-datax-bank_key  = abap_true.

      ls_bank-data-accountholder  = <bank>-koinh.    "户主
      ls_bank-datax-accountholder  = abap_true.

      ls_bank-data-bank_acct  =  <bank>-bankn.
      ls_bank-datax-bank_acct  = abap_true.

      ls_bank-data-externalbankid  =  <bank>-bkext.    "账号2
      ls_bank-datax-externalbankid  = abap_true.

      APPEND ls_bank TO lt_bank.
      ls_partner-central_data-bankdetail-bankdetails = lt_bank.
    ENDIF.
  ENDLOOP.


  CLEAR:ls_addresses,lt_addresses.
  ls_addresses-task = 'M'.
  "ls_addresses-data-postal-data-move_addr_guid = lv_bpartnerguid2.
  READ TABLE it_but020 INTO DATA(wa_but020)  WITH KEY partner = data-kunnr.
  IF sy-subrc = 0.
    ls_addresses-data_key-guid = wa_but020-address_guid.
  ENDIF.


  ls_addresses-data-postal-data-langu = sy-langu."语言
  ls_addresses-data-postal-datax-langu = abap_true."语言

  IF data-pstlz IS NOT INITIAL.
    ls_addresses-data-postal-data-postl_cod1 = data-pstlz.  " 邮政编码
    ls_addresses-data-postal-datax-postl_cod1 = abap_true.       " 邮政编码
  ENDIF.

  IF data-land1 IS NOT INITIAL.
    ls_addresses-data-postal-data-country = data-land1.     " 国家代码
    ls_addresses-data-postal-datax-country = abap_true.     " 国家代码
  ENDIF.

  IF data-regio IS NOT INITIAL.
    ls_addresses-data-postal-data-region = |{ data-regio ALPHA = IN }|.     " 地区代码
    ls_addresses-data-postal-datax-region = abap_true.     " 地区代码
  ENDIF.

  IF data-ort01 IS NOT INITIAL.
    ls_addresses-data-postal-data-city = data-ort01.     " 地区代码
    ls_addresses-data-postal-datax-city = abap_true.     " 地区代码
  ENDIF.

  IF data-strsuppl3 IS NOT INITIAL.
    ls_addresses-data-postal-data-str_suppl3 = data-strsuppl3.     " 街道2 WD
    ls_addresses-data-postal-datax-str_suppl3 = abap_true.     " 街道3 WD
  ENDIF.

  IF data-location IS NOT INITIAL.
    ls_addresses-data-postal-data-location = data-location.     " 街道3 WD
    ls_addresses-data-postal-datax-location = abap_true.     " 街道4
  ENDIF.


  ls_addresses-data-postal-data-country = 'CN'.
  ls_addresses-data-postal-datax-country = abap_true.

  CLEAR:ls_phone,lt_phone.
  READ TABLE it_kna2 INTO DATA(wa_kna2) WITH KEY kunnr = data-kunnr.
  IF sy-subrc = 0.
    ls_phone-contact-task = 'U'.
  ELSE.
    ls_phone-contact-task = 'I'.
  ENDIF.

  IF data-telf1 IS NOT INITIAL.
    ls_phone-contact-data-telephone = data-telf1. "手机
    ls_phone-contact-datax-telephone = abap_true.

    ls_phone-contact-data-r_3_user = 3.    "3表示移动电话
    ls_phone-contact-datax-r_3_user = abap_true.

  ENDIF.

  APPEND ls_phone TO lt_phone.
  ls_addresses-data-communication-phone-phone = lt_phone.

*--地址注释 WD(联系人)
  REFRESH lt_remarks_a.
  CLEAR:ls_remarks_a.
  ls_remarks_a-task = 'M'.
  ls_remarks_a-data-langu = sy-langu.
  ls_remarks_a-datax-langu = abap_true.
  IF data-vsort IS NOT INITIAL.
    ls_remarks_a-data-adr_notes = data-vsort.
    ls_remarks_a-datax-adr_notes = abap_true.
  ENDIF.

  APPEND ls_remarks_a TO lt_remarks_a .
  ls_addresses-data-remark-remarks = lt_remarks_a.

*电子邮件存储CRM客户ID
  CLEAR:lt_smtp,ls_smtp.
  ls_smtp-contact-task = 'M'.
  ls_smtp-contact-data-e_mail = |{ data-accountid }@crm.com|.
  ls_smtp-contact-datax-e_mail = abap_true.
  APPEND ls_smtp TO lt_smtp.
  ls_addresses-data-communication-smtp-smtp = lt_smtp.

  APPEND ls_addresses TO lt_addresses.
  ls_partner-central_data-address-addresses = lt_addresses.


****更新公司代码数据************************************************
  CLEAR:ls_company,lt_company.
  IF data-bukrs IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'   " 统驭科目补零
      EXPORTING
        input  = data-akont
      IMPORTING
        output = data-akont.

    ls_company-task = 'M'.

    ls_company-data_key-bukrs = data-bukrs. " 公司代码
    ls_company-data-akont = data-akont. " 统驭科目
    ls_company-data-zuawa = data-zuawa. " 排序码
    ls_company-data-zterm = data-zterm1. " 付款条款

    ls_company-datax-akont = abap_true.
    ls_company-datax-zuawa = abap_true.
    ls_company-datax-zterm = abap_true.
    APPEND ls_company TO lt_company.
    ls_company_data-company = lt_company.
    ls_customer-company_data = ls_company_data.

  ENDIF.

****税分类*******************************************************
  CLEAR:ls_tax_ind,lt_tax_ind.
  ls_tax_ind-task = 'M'.
  ls_tax_ind-data_key-aland = 'CN'.
  ls_tax_ind-data_key-tatyp = 'MWST'.
  ls_tax_ind-data-taxkd = data-taxkd.
  ls_tax_ind-datax-taxkd = 'X'.
  APPEND ls_tax_ind TO  lt_tax_ind.

  CLEAR:ls_central_data.
  ls_central_data-tax_ind-tax_ind = lt_tax_ind.
  ls_customer-central_data = ls_central_data.


*****销售数据****************************************************

  CLEAR:ls_sales,lt_sales.
  IF  data-vkorg IS NOT INITIAL
      AND data-vtweg IS NOT INITIAL
      AND data-spart IS NOT INITIAL.
    ls_sales-task = 'M'.
    ls_sales-data_key-vkorg = data-vkorg.
    ls_sales-data_key-vtweg = data-vtweg.
    ls_sales-data_key-spart = data-spart.
    ls_sales-data-vkbur = data-vkbur.
    ls_sales-data-vkgrp = data-vkgrp.
    "ls_sales-data-kvgr1 = '01'.  "客户组
    ls_sales-data-waers = data-waers.
    ls_sales-data-kalks = 1.
    ls_sales-data-vsbed = '01'.
    ls_sales-data-vwerk = data-vwerk.
    ls_sales-data-pltyp = '01'.
    ls_sales-data-podkz = data-podkz.
    ls_sales-data-zterm = data-zterm1.
    ls_sales-data-ktgrd = data-ktgrd.
    ls_sales-data-konda = data-konda.
    ls_sales-data-vsort = data-vsort.
    ls_sales-data-eikto = data-eikto.
    "IF data-vkbur <> '2020'.
    ls_sales-data-inco1 = 'EXW'.
    ls_sales-data-inco2_l = '博兴'.
    ls_sales-datax-inco1 = abap_true.
    ls_sales-datax-inco2_l = abap_true.
    "ENDIF.

    ls_sales-datax-vkbur = abap_true.
    ls_sales-datax-vkgrp = abap_true.
    "ls_sales-datax-kvgr1 = abap_true.  "客户组
    ls_sales-datax-waers = abap_true.
    ls_sales-datax-kalks = abap_true.
    ls_sales-datax-vsbed = abap_true.
    ls_sales-datax-vwerk = abap_true.
    ls_sales-datax-pltyp = abap_true.
    ls_sales-datax-podkz = abap_true.
    ls_sales-datax-zterm = abap_true.
    ls_sales-datax-ktgrd = abap_true.
    ls_sales-datax-konda = abap_true.
    ls_sales-datax-vsort = abap_true.
    ls_sales-datax-eikto = abap_true.

    APPEND ls_sales TO lt_sales.
    ls_sales_data-sales = lt_sales.
    ls_customer-sales_data = ls_sales_data.
  ENDIF.

  CLEAR:ls_data,lt_data.
  ls_data-partner = ls_partner.
  ls_data-customer = ls_customer.

  APPEND ls_data TO lt_data.

  CALL FUNCTION 'CVI_EI_INBOUND_MAIN'
    EXPORTING
      i_data   = lt_data
*     I_EXT_DATA       =
    IMPORTING
      e_return = lt_return.

  CLEAR lv_msg.
  LOOP AT lt_return INTO ls_return.
    LOOP AT ls_return-object_msg INTO ls_msg WHERE type = 'E' OR type = 'A' OR type = 'X'.
      CONCATENATE lv_msg ls_msg-message INTO lv_msg.
    ENDLOOP.
  ENDLOOP.

  IF lv_msg IS INITIAL.
    CLEAR:lt_partnerguid_list,ls_partnerguid_list.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.
    CONCATENATE '客户' wa_but000-partner '更新成功！' INTO rtmsg.
*--维护信用段数据
    CLEAR:lv_msg1.
    REFRESH lt_return_ukmbp1[].
    PERFORM frm_add_ukmbp USING data wa_but000-partner CHANGING lt_return_ukmbp1.
    LOOP AT lt_return_frg1 WHERE type = 'E' OR type = 'A' OR type = 'X'.
      CONCATENATE lv_msg1 lt_return_frg1-message INTO lv_msg1.
    ENDLOOP.
    IF lv_msg1 IS INITIAL .
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
      fillmsg 'S' rtmsg.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*       IMPORTING
*         RETURN        =
        .
      rtmsg = '客户更新成功，但是信用相关数据更新失败'.
      fillmsg 'E' rtmsg.
    ENDIF.
  ELSE.
    rtmsg = lv_msg.
    fillmsg 'E' rtmsg.
  ENDIF.



  zfmdatasave2 'R'.




ENDFUNCTION.
