REPORT zdeal_memoreport.

DATA : lv_dmdt     TYPE /ibscms/d_dmhd-dmdt,
       lv_dmno     TYPE /ibscms/d_dmhd-dmno,
       lv_contract TYPE /ibscms/d_dmco-contno,
       lv_po_no    TYPE /ibscms/d_dmco-ebeln,
       lv_po_date  TYPE ekko-aedat,
       lv_chnid    TYPE /ibscms/d_dmhd-chnlid.
DATA : gt_fieldcatalog TYPE slis_t_fieldcat_alv,
       gs_fieldcatalog TYPE slis_fieldcat_alv,
       gs_layout       TYPE slis_layout_alv.
DATA : lv_count  TYPE i.



TYPES : BEGIN OF ty_display,
          channel_id                  TYPE /ibscms/d_dmhd-chnlid,
          tentid                      TYPE /ibscms/d_dmhd-tentid,
          dealmemo_number             TYPE /ibscms/d_dmhd-dmno,
          dealmemo_version            TYPE /ibscms/d_dmhd-dmver,
          contract_no                 TYPE /ibscms/d_dmco-contno,
          contract_version            TYPE /ibscms/d_dmco-contver,
          contract_type               TYPE /ibscms/d_dmco-conttp,
          episode_id                  TYPE /ibscms/d_dmce-epiid,
          cost_code                   TYPE /ibscms/d_dmce-costcd,
          transaction_type            TYPE /ibscms/d_dmhd-transtp,
          deal_memo_date              TYPE /ibscms/d_dmhd-dmdt,
          deal_memo_status            TYPE /ibscms/d_dmhd-dmst,
          record_status               TYPE /ibscms/d_dmhd-recst,
          content_id                  TYPE /ibscms/d_dmhd-cntid,
          content_id_description      TYPE /ibscms/m_coidt-cntnm,
          rights_start_date           TYPE /ibscms/d_dmvr-rhtfrdt,
          rights_end_date             TYPE /ibscms/d_dmvr-rhttodt,
          cont_status                 TYPE /ibscms/d_dmco-contstat,
          contractrecord              TYPE /ibscms/d_dmco-recst,
          currency                    TYPE /ibscms/d_dmhd-waers,
          contract_currency           TYPE /ibscms/d_dmco-waers,
          deal_memo_exchange_rate     TYPE /ibscms/d_dmhd-exchrt,
          deal_memo_val_loc_curr      TYPE /ibscms/d_dmct-prdhsamt,
          episode_amount              TYPE /ibscms/d_dmce-coepiamt,
          deal_memo_value             TYPE /ibscms/d_dmct-prdhsamt,
          contract_typ_desc(40),
          cost_code_desc(40),
          cost_code_currency          TYPE /ibscms/d_dmco-waers,
          cost_code_exchange_rate     TYPE /ibscms/d_dmhd-exchrt,
          po_number                   TYPE ekko-ebeln,
          po_item                     TYPE ekpo-ebelp,
          requisitioner               TYPE ekpo-afnam,
          vc_latest_contract          TYPE ekko-zcnt_vc,
          vc_latest_version           TYPE ekko-zcnt_vc_vr,
          po_creation_date            TYPE ekko-aedat,
          vendor_code                 TYPE ekko-lifnr,
          vendor_name                 TYPE lfa1-name1,
          po_currency                 TYPE EKKo-waers,
          contract_value              TYPE /ibscms/d_dmce-coepiamt,
          contract_exchange_rate      TYPE /ibscms/d_dmhd-exchrt,
          po_value                    TYPE ekpo-netwr,
          grn_value                   TYPE ekbe-wrbtr,
          invoice_value               TYPE ekbe-wrbtr,
          pending_invoice_value       TYPE ekbe-wrbtr,
          open_po_value               TYPE ekpo-netwr,
          total_contract_val_loc_curr TYPE /ibscms/d_dmce-coepiamt,
          cost_code_val_loc_curr      TYPE /ibscms/d_dmce-coepiamt,
          po_exchange_rate            TYPE ekko-wkurs,
          po_value_in_loc_curr        TYPE ekpo-netwr,
          commitment_item             TYPE /ibscms/m_funds-rcmmtitem,
          mpm_level3                  TYPE /ibscms/d_dmed-mpmid,
          asset_no                    TYPE /ibscms/d_dmed-anln1,
          sub_asset_no                TYPE /ibscms/d_dmed-anln2,
          wbs                         TYPE /ibscms/d_dmed-pspnr,
          mpm_level3_descrpt          TYPE makt-maktx,
          delivery_date               TYPE /ibscms/D_DMVD-delvdt,
        END OF ty_display.

DATA : gs_display TYPE ty_display,
       gt_display TYPE STANDARD TABLE OF ty_display.
SELECTION-SCREEN: BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_dmno FOR lv_dmno,
                  s_dmdt FOR lv_dmdt OBLIGATORY,
                  s_cont FOR lv_contract,
                  s_po_no FOR lv_po_no,
                  s_po_dt FOR lv_po_date,
                  s_chnid FOR lv_chnid.
SELECTION-SCREEN :END OF BLOCK a1.



START-OF-SELECTION.

  PERFORM get_data_display_email.
  IF gt_display IS NOT INITIAL.
    PERFORM display.
    PERFORM email.
    PERFORM alv_display.
  ENDIF.
*&---------------------------------------------------------------------*
*& Form get_data_display_email
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data_display_email.

  DATA : lc_transtp(1)   VALUE 'D',
         lc_dmst(2)      VALUE '04',
         lc_RECST(1)     VALUE 'A',
         lc_ibs          TYPE /ibscms/d_dmct-tentid VALUE 'IBS',
         lc_bewtp_e      TYPE ekbe-bewtp VALUE 'E',
         lc_bewtp_q      TYPE ekbe-bewtp VALUE 'Q',
         gr_bwart        TYPE RANGE OF bwart,
         lc_shkzg_s      TYPE  ekbe-shkzg VALUE 'S',
         lc_shkzg_h      TYPE  ekbe-shkzg VALUE 'H',
         lc_bstyp_f      TYPE ekko-bstyp VALUE 'F',
         lc_bstyp_k      TYPE ekko-bstyp VALUE 'K',
         lv_po_total_val TYPE ekpo-netwr,
         lc_currency_inr TYPE waers VALUE 'INR',
         lv_channel_id   TYPE /ibscms/chnlid,
         lv_message      TYPE string.


  DATA : lv_item TYPE ekpo-ebelp,
         lv_po   TYPE ekko-ebeln,
         gt_ZP2P_DEVI_APPR TYPE STANDARD TABLE OF ZP2P_DEVI_APPR.

  CONSTANTS : lc_zpr_to_po_bwart  TYPE tvarvc-name VALUE 'ZPR_TO_PO_BWART'.

  SELECT  tentid,
          dealmemo_number,
          dealmemo_version,
          contract_no,
          contract_version,
          contract_type,
          episode_id,
          cost_code,
          channel_id,
          transaction_type,
          deal_memo_exchange_rate,
          deal_memo_date,
          deal_memo_status,
          record_status,
          content_id,
          content_id_description,
          rights_start_date,
          rights_end_date,
          cont_status,
          contractrecord,
          commitment_item,
          currency,
          contract_currency,
          cost_code_currency,
          episode_amount,
          deal_memo_value,
          contract_typ_desc,
          cost_code_desc,
          po_number,
          mpm_level3,
          mpm_level3_descrpt,
          asset_no,
          sub_asset_no,
          wbs,
          po_item,
          requisitioner,
          vc_latest_contract,
          vc_latest_version,
          po_creation_date,
          delivery_date,
          vendor_code,
          vendor_name,
          contract_exchange_rate,
          cost_code_exchange_rate,
          po_exchange_rate,
          po_currency,
          contract_value,
          po_value,
          grn_value,
          invoice_value,
          pending_invoice_value,
          open_po_value,
          deal_memo_val_loc_curr,
          total_contract_val_loc_curr,
          cost_code_val_loc_curr,
          po_value_in_loc_curr
  FROM zcf_deal_memo_to_po
  INTO TABLE @DATA(gt_final)
        WHERE dealmemo_number IN @s_dmno
        AND  transaction_type EQ @lc_transtp
        AND  deal_memo_date   IN @s_dmdt
        AND  deal_memo_status EQ @lc_dmst
        AND  channel_id       IN @s_chnid
        AND  record_status    EQ @lc_recst
        AND  cont_status      EQ @lc_dmst
        AND  contractrecord   EQ @lc_RECST
  ORDER BY   dealmemo_number,
             dealmemo_version,
             contract_no,
             contract_version,
             episode_id.

  IF sy-subrc EQ 0.

    IF s_cont IS NOT INITIAL.

      DELETE gt_final WHERE contract_no NOT IN s_cont.

    ENDIF.

    IF s_po_no IS NOT INITIAL.

      DELETE gt_final WHERE po_number NOT IN s_po_no.

    ENDIF.



    SELECT DISTINCT
           a~tentid,
           a~dmno,
           a~dmver,
           a~epiid,
           a~epitp,
           a~mpmid,
           b~ebeln,
           b~ebelp,
           b~zekkn,
           b~zzmpm
      FROM /ibscms/d_dmed   AS a
      INNER JOIN @gt_final  AS final
      ON    a~tentid EQ final~tentid
      AND   a~dmno   EQ final~dealmemo_number
      AND   a~dmver  EQ final~dealmemo_version
      INNER JOIN ekkn       AS b
       ON b~zzmpm  EQ a~mpmid
      INTO TABLE @DATA(gt_final_po_item).

    SORT gt_final_po_item BY tentid dmno dmver epiid ebeln.

    LOOP AT gt_final ASSIGNING FIELD-SYMBOL(<fs_final_po_item>).
      READ TABLE gt_final_po_item INTO DATA(ls_po_item) WITH KEY   tentid = <fs_final_po_item>-tentid
                                                                   dmno   = <fs_final_po_item>-dealmemo_number
                                                                   dmver  = <fs_final_po_item>-dealmemo_version
                                                                   epiid  = <fs_final_po_item>-episode_id
                                                                   ebeln  = <fs_final_po_item>-po_number
                                                                  BINARY SEARCH.
      IF sy-subrc EQ 0.

        <fs_final_po_item>-po_item = ls_po_item-ebelp.

      ENDIF.

    ENDLOOP.



********* for po line item as per ekkn ends


    SELECT DISTINCT
           a~tentid,
           a~dmno,
           a~dmver,
           a~epiid,
           a~costcd,
           a~scostcd,
           a~budgetver,
           a~prdhsamt,
           a~inhsamt ,
           a~inhouseamt
    FROM /ibscms/d_dmct         AS a
    INNER JOIN @gt_final        AS b
    ON   a~tentid EQ b~tentid
    AND  a~dmno   EQ b~dealmemo_number
    AND  a~dmver  EQ b~dealmemo_version
    WHERE a~epiid  EQ @space
  ORDER BY  a~tentid,
            a~dmno,
            a~dmver
    INTO TABLE @DATA(gt_deal_memo_value).
    IF sy-subrc EQ 0.

      SELECT a~tentid,
      a~dmno,
      a~dmver,
      SUM( a~prdhsamt ) AS prdhsamt , SUM( a~inhsamt ) AS inhsamt, SUM( a~inhouseamt ) AS inhouseamt
      FROM @gt_deal_memo_value AS a                                                                                                  ##ITAB_DB_SELECT
      WHERE  tentid EQ @lc_ibs
      GROUP BY a~tentid,
      a~dmno,
      a~dmver
      ORDER BY a~tentid,
      a~dmno,
      a~dmver
      INTO TABLE @DATA(gt_deal_memo_value2).

      IF sy-subrc EQ 0.

        SELECT a~tentid,
               a~dmno,
               a~dmver,
            SUM( a~prdhsamt ) + SUM( a~inhsamt ) + SUM( a~inhouseamt ) AS final_deal_memo_amount
            FROM @gt_deal_memo_value2 AS a                                                                                              ##ITAB_DB_SELECT
            WHERE  tentid EQ @lc_ibs
            GROUP BY a~tentid,
                     a~dmno,
                     a~dmver
            INTO TABLE @DATA(gt_final_deal_memo_amt).

        IF sy-subrc EQ 0.

          SORT gt_final_deal_memo_amt BY tentid dmno dmver.

        ENDIF.
*

      ENDIF.

    ENDIF.


    SELECT a~tentid,
           a~dealmemo_number,
           a~dealmemo_version,
           a~contract_no,
           a~contract_version,
           SUM( a~episode_amount ) AS contract_value
           FROM @gt_final AS a                                                                                                          ##ITAB_DB_SELECT
           WHERE  tentid EQ @lc_ibs
           GROUP BY a~tentid,
                    a~dealmemo_number,
                    a~dealmemo_version,
                    a~contract_no,
                    a~contract_version
           INTO TABLE @DATA(gt_contract_value_final).

    IF sy-subrc EQ 0.

      SORT gt_contract_value_final BY tentid
                                      dealmemo_number.


    ENDIF.
****/////////////// PO Logic //////////////////////////////////////////
******* Starting PO logic
*BREAK-POINT.
    SELECT DISTINCT
           a~ebeln,
           b~ebelp,
           a~bstyp,
           a~aedat,
           a~lifnr,
           c~name1 AS vendor_name,
           a~waers,
           a~wkurs,
           a~zcnt_vc,
           a~zcnt_vc_vr,
           b~netwr,
           b~zwert,
           b~afnam
      FROM ekko AS a
      INNER JOIN @gt_final AS final
      ON a~ebeln = final~po_number
      INNER JOIN ekpo   AS b
      ON  b~ebeln = final~po_number
      AND b~ebelp = final~po_item
      INNER JOIN lfa1   AS c
      ON c~lifnr = a~lifnr
      ORDER BY a~ebeln,
               b~ebelp
      INTO TABLE @DATA(gt_ekko_ekpo).


    IF sy-subrc EQ 0.

      SORT gt_ekko_ekpo BY ebeln ebelp.

    ENDIF.

    REFRESH gr_bwart[].
    SELECT name,
           type,
           numb,
           sign,
           opti,
           low,
           high
    FROM tvarvc
    INTO TABLE @DATA(lt_tvarvc)
          WHERE name EQ @lc_zpr_to_po_bwart.

    IF sy-subrc EQ 0.

      gr_bwart[] = CORRESPONDING #( lt_tvarvc MAPPING
      sign  = sign
      option = opti
      low = low   ).

    ENDIF.

    SELECT ebeln,
           ebelp,
           zekkn,
           vgabe,
           gjahr,
           belnr,
           buzei,
           bewtp,
           shkzg,
           bwart,
           wrbtr,
           waers
      FROM ekbe
      INTO TABLE @DATA(gt_ekbe_grn_value)
      FOR ALL ENTRIES IN @gt_final
          WHERE ebeln EQ @gt_final-po_number
            AND ebelp EQ @gt_final-po_item
            AND bewtp EQ @lc_bewtp_e
            AND bwart IN @gr_bwart.
    IF sy-subrc EQ 0.

      SORT gt_ekbe_grn_value BY ebeln
                                ebelp
                                zekkn
                                vgabe
                                gjahr
                                belnr
                                buzei.

      IF gt_ekbe_grn_value IS NOT INITIAL.

        SELECT a~ebeln,
        a~ebelp,
        SUM( CASE
        WHEN   a~shkzg EQ @lc_shkzg_s
        THEN   a~wrbtr

        END
        ) AS grn_value_s,
        SUM( CASE
        WHEN   a~shkzg EQ @lc_shkzg_h
        THEN    a~wrbtr
        END
        ) AS grn_value_h
        FROM @gt_ekbe_grn_value AS a
        GROUP BY a~ebeln,
        a~ebelp
        ORDER BY a~ebeln,
        a~ebelp
        INTO TABLE @DATA(gt_final_grn_value).
        IF sy-subrc EQ 0.

          SORT gt_final_grn_value BY ebeln ebelp.

        ENDIF.


      ENDIF.

    ENDIF.


    SELECT ebeln,
           ebelp,
           zekkn,
           vgabe,
           gjahr,
           belnr,
           buzei,
           bewtp,
           shkzg,
           bwart,
           wrbtr,
           waers
    FROM ekbe
    INTO TABLE @DATA(gt_invoice_value)
    FOR ALL ENTRIES IN @gt_final
    WHERE ebeln EQ @gt_final-po_number
    AND ebelp EQ @gt_final-po_item
    AND bewtp EQ @lc_bewtp_q.

    IF sy-subrc EQ 0.

      SORT gt_invoice_value BY ebeln ebelp.

    ENDIF.


    IF gt_invoice_value IS NOT INITIAL.


      SELECT a~ebeln,
      a~ebelp,
      SUM( CASE
      WHEN   a~shkzg EQ @lc_shkzg_s
      THEN   a~wrbtr
      END
      ) AS inv_value_s,
      SUM( CASE
      WHEN   a~shkzg EQ @lc_shkzg_h
      THEN    a~wrbtr
      END
      ) AS inv_value_h
      FROM @gt_invoice_value AS a
      GROUP BY a~ebeln,
      a~ebelp
      ORDER BY a~ebeln,
      a~ebelp
      INTO TABLE @DATA(gt_final_invoice_value).

      IF sy-subrc EQ 0.

        SORT gt_final_invoice_value BY ebeln ebelp.

      ENDIF.


    ENDIF.

******* Ending PO logic


************For fetching MPM LEVEL3,Asset,sub-asset,WBS starts

    SELECT tentid,
           dmno,
           dmver,
           epiid,
           epitp,
           pspnr,
           anln1,
           anln2,
           mpmid
      FROM /ibscms/d_dmed
      FOR ALL ENTRIES IN @gt_final
      WHERE tentid EQ @gt_final-tentid
        AND dmno   EQ @gt_final-dealmemo_number
        AND dmver  EQ @gt_final-dealmemo_version
        AND epiid  EQ @gt_final-episode_id
      INTO TABLE @DATA(gt_D_DMED).

    IF sy-subrc EQ 0.

      SORT gt_D_DMED BY tentid
                        dmno
                        dmver
                        epiid .

      SELECT matnr,
             maktx
        FROM makt
        INTO TABLE @DATA(gt_makt_level3)
        FOR ALL ENTRIES IN @gt_d_dmed
        WHERE matnr EQ @gt_d_dmed-mpmid.
      IF sy-subrc EQ 0.
        SORT gt_makt_level3 BY matnr.

      ENDIF.
    ENDIF.

  ENDIF.

  SELECT  tentid,
            dmno,
            dmver,
            contno,
            contver,
            epiid,
            delvcd,
            delvdt,
            delvpay
      FROM /ibscms/d_dmvd
      INTO TABLE @DATA(gt_delivery_date)
      FOR ALL ENTRIES IN @gt_final
      WHERE tentid EQ @gt_final-tentid
        AND  dmno   EQ @gt_final-dealmemo_number
        AND  dmver  EQ @gt_final-dealmemo_version
        AND  contno EQ @gt_final-contract_no
        AND contver EQ @gt_final-contract_version
        AND delvpay EQ @abap_true.

  IF sy-subrc EQ 0.

    SORT gt_delivery_date BY tentid
                             dmno
                             dmver
                             contno
                             contver.
  ENDIF.

************** processing logic starts

  IF gt_final IS NOT INITIAL.

    LOOP AT gt_final ASSIGNING FIELD-SYMBOL(<fs_final>).
      CLEAR : lv_channel_id,
              lv_message.
      IF <fs_final>-channel_id IS NOT INITIAL.

        lv_channel_id = <fs_final>-channel_id.

        AUTHORITY-CHECK OBJECT 'ZCHNL'
         ID 'ZCHNL' FIELD <fs_final>-channel_id
         ID 'ACTVT' FIELD '03'.
        IF sy-subrc <> 0.


          CONCATENATE TEXT-104
                      lv_channel_id
                      TEXT-105
                      INTO lv_message SEPARATED BY space.

          WRITE :/ lv_message.

          EXIT.

        ENDIF.


      ENDIF.

      READ TABLE gt_final_deal_memo_amt INTO DATA(ls_deal_memo_value)
                                          WITH KEY tentid = <fs_final>-tentid
                                                   dmno   = <fs_final>-dealmemo_number
                                                   dmver  = <fs_final>-dealmemo_version BINARY SEARCH.
      IF sy-subrc EQ 0 AND ls_deal_memo_value IS NOT INITIAL.
        <fs_final>-deal_memo_value = ls_deal_memo_value-final_deal_memo_amount.
      ENDIF.
      CLEAR : ls_deal_memo_value.

      READ TABLE gt_contract_value_final INTO DATA(ls_contract_value)
                               WITH KEY tentid           = <fs_final>-tentid
                                        dealmemo_number  = <fs_final>-dealmemo_number
                                        dealmemo_version = <fs_final>-dealmemo_version
                                        contract_no      = <fs_final>-contract_no
                                        contract_version = <fs_final>-contract_version BINARY SEARCH.

      IF sy-subrc EQ 0.

        READ TABLE gt_final INTO DATA(ls_contract_check) WITH KEY tentid           = <fs_final>-tentid
                                                                  dealmemo_number  = <fs_final>-dealmemo_number
                                                                  dealmemo_version = <fs_final>-dealmemo_version
                                                                  contract_no      = <fs_final>-contract_no
                                                                  contract_version = <fs_final>-contract_version.

        IF sy-subrc EQ 0 AND ls_contract_check-contract_value IS INITIAL.
          <fs_final>-contract_value = ls_contract_value-contract_value.
        ENDIF.


      ENDIF.
      CLEAR : ls_contract_value.


      READ TABLE gt_final_grn_value INTO DATA(ls_grn_value) WITH KEY ebeln = <fs_final>-po_number
                                                                     ebelp = <fs_final>-po_item BINARY SEARCH.
      IF sy-subrc EQ 0.

        <fs_final>-grn_value = ls_grn_value-grn_value_s - ls_grn_value-grn_value_h.

      ENDIF.
      CLEAR : ls_grn_value.

      READ TABLE gt_final_invoice_value INTO DATA(ls_inv_value) WITH KEY ebeln = <fs_final>-po_number
                                                                         ebelp = <fs_final>-po_item BINARY SEARCH.
      IF sy-subrc EQ 0.


        <fs_final>-invoice_value = ls_inv_value-inv_value_s - ls_inv_value-inv_value_h.

      ENDIF.
      CLEAR : ls_inv_value.


      <fs_final>-pending_invoice_value = <fs_final>-grn_value - <fs_final>-invoice_value.
      CLEAR : lv_po_total_val.

      LOOP AT gt_ekko_ekpo INTO DATA(ls_ekko_ekpo) WHERE ebeln EQ  <fs_final>-po_number
                                                     AND ebelp EQ <fs_final>-po_item.
        MOVE : ls_ekko_ekpo-zcnt_vc     TO <fs_final>-vc_latest_contract,
               ls_ekko_ekpo-zcnt_vc_vr  TO <fs_final>-vc_latest_version,
               ls_ekko_ekpo-aedat       TO <fs_final>-po_creation_date,
               ls_ekko_ekpo-lifnr       TO <fs_final>-vendor_code,
               ls_ekko_ekpo-vendor_name TO <fs_final>-vendor_name,
               ls_ekko_ekpo-waers       TO <fs_final>-po_currency,
               ls_ekko_ekpo-wkurs       TO <fs_final>-po_exchange_rate,
               ls_ekko_ekpo-afnam       TO <fs_final>-requisitioner.

        IF ls_ekko_ekpo-bstyp EQ lc_bstyp_f.

          MOVE : ls_ekko_ekpo-netwr TO <fs_final>-po_value.

        ENDIF.

        IF ls_ekko_ekpo-bstyp EQ lc_bstyp_f.

          MOVE : ls_ekko_ekpo-netwr TO lv_po_total_val.

        ELSEIF ls_ekko_ekpo-bstyp EQ lc_bstyp_k.
          MOVE : ls_ekko_ekpo-zwert TO lv_po_total_val.
        ENDIF.


        <fs_final>-open_po_value = lv_po_total_val - <fs_final>-grn_value.

      ENDLOOP.



      MOVE-CORRESPONDING <fs_final> TO gs_display.
      gs_display-deal_memo_val_loc_curr = <fs_final>-deal_memo_value * <fs_final>-deal_memo_exchange_rate.


      IF gs_display-contract_currency EQ lc_currency_inr.

        gs_display-contract_exchange_rate = 1.

      ELSE.

        gs_display-contract_exchange_rate = gs_display-deal_memo_exchange_rate.

      ENDIF.

      gs_display-total_contract_val_loc_curr = gs_display-contract_value * gs_display-contract_exchange_rate.

      IF gs_display-cost_code_currency EQ lc_currency_inr.

        gs_display-cost_code_exchange_rate = 1.

      ELSE.

        gs_display-cost_code_exchange_rate = gs_display-deal_memo_exchange_rate.

      ENDIF.

      gs_display-cost_code_val_loc_curr = gs_display-episode_amount * gs_display-cost_code_exchange_rate.

      gs_display-po_value_in_loc_curr =  gs_display-po_value * gs_display-po_exchange_rate.


**********addition for mpmlevel3,wbs,asset and sub-asset
      READ TABLE gt_D_DMED INTO DATA(ls_dmed) WITH KEY tentid = <fs_final>-tentid
                                                       dmno   = <fs_final>-dealmemo_number
                                                       dmver  = <fs_final>-dealmemo_version
                                                       epiid  = <fs_final>-episode_id BINARY SEARCH.
      IF sy-subrc EQ 0.

        MOVE : ls_dmed-mpmid TO gs_display-mpm_level3,
               ls_dmed-anln1 TO gs_display-asset_no,
               ls_dmed-anln2 TO gs_display-sub_asset_no,
               ls_dmed-pspnr TO gs_display-wbs.

        READ TABLE gt_makt_level3 INTO DATA(ls_maktx) WITH KEY matnr = ls_dmed-mpmid BINARY SEARCH.
        IF sy-subrc EQ 0.

          MOVE : ls_maktx-maktx TO gs_display-mpm_level3_descrpt.

        ENDIF.



      ENDIF.
      CALL FUNCTION 'ENQUEUE_E_TABLE'.
      IF sy-subrc EQ 0.
         update zp2p_devi_appr FROM TABLE gt_ZP2P_DEVI_APPR.

**********end for mpmlevel3,wbs,asset and sub-asset
        CALL FUNCTION 'DEQUEUE_E_TABLE'.
      ENDIF.



      READ TABLE gt_delivery_date INTO DATA(ls_del_date) WITH KEY tentid  = <fs_final>-tentid
                                                                  dmno    = <fs_final>-dealmemo_number
                                                                  dmver   = <fs_final>-dealmemo_version
                                                                  contno  = <fs_final>-contract_no
                                                                  contver = <fs_final>-contract_version BINARY SEARCH.
      IF sy-subrc EQ 0.

        MOVE : ls_del_date-delvdt TO gs_display-delivery_date.

      ENDIF.

      APPEND gs_display TO gt_display.
      CLEAR : gs_display.



    ENDLOOP.



  ENDIF.
************** processing logic ends

  IF s_po_dt IS NOT INITIAL.

    DELETE gt_display WHERE po_creation_date NOT IN s_po_dt.

  ENDIF.


ENDFORM.


*&---------------------------------------------------------------------*
*& Form display
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display .
************* Display


  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-061.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 10.
  gs_fieldcatalog-seltext_l = TEXT-062.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.


  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-063.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 10.
  gs_fieldcatalog-seltext_l = TEXT-064.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-019.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 40.
  gs_fieldcatalog-seltext_l = TEXT-020.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-021.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 40.
  gs_fieldcatalog-seltext_l = TEXT-022.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-065.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 40.
  gs_fieldcatalog-seltext_l = TEXT-066.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-067.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 40.
  gs_fieldcatalog-seltext_l = TEXT-068.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-069.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 12.
  gs_fieldcatalog-seltext_l = TEXT-070.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-071.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 06.
  gs_fieldcatalog-seltext_l = TEXT-072.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-073.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 08.
  gs_fieldcatalog-seltext_l = TEXT-073.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.


  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-003.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 15.
  gs_fieldcatalog-seltext_l = TEXT-004.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-005.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 05.
  gs_fieldcatalog-seltext_l = TEXT-006.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-074.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 05.
  gs_fieldcatalog-seltext_l = TEXT-075.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-007.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 15.
  gs_fieldcatalog-seltext_l = TEXT-008.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-076.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 07.
  gs_fieldcatalog-seltext_l = TEXT-077.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-078.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 18.
  gs_fieldcatalog-seltext_l = TEXT-079.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.


  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-009.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 10.
  gs_fieldcatalog-seltext_l = TEXT-010.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.


  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-011.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 05.
  gs_fieldcatalog-seltext_l = TEXT-012.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-013.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 05.
  gs_fieldcatalog-seltext_l = TEXT-014.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-015.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 20.
  gs_fieldcatalog-seltext_l = TEXT-016.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.


  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-080.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 20.
  gs_fieldcatalog-seltext_l = TEXT-081.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.


  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-082.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 20.
  gs_fieldcatalog-seltext_l = TEXT-083.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-084.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 5.
  gs_fieldcatalog-seltext_l = TEXT-085.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.



  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-086.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 18.
  gs_fieldcatalog-seltext_l = TEXT-087.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.


  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-029.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 05.
  gs_fieldcatalog-seltext_l = TEXT-030.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-031..
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 4.
  gs_fieldcatalog-seltext_l = TEXT-032.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-033.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen = 40.
  gs_fieldcatalog-seltext_l = TEXT-034.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-088.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen = 04.
  gs_fieldcatalog-seltext_l = TEXT-089.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.


  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-035.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 20.
  gs_fieldcatalog-seltext_l = TEXT-036.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-090.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 05.
  gs_fieldcatalog-seltext_l = TEXT-091.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-092.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 20.
  gs_fieldcatalog-seltext_l = TEXT-093.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

*******8

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-023.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 10.
  gs_fieldcatalog-seltext_l = TEXT-024.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.


  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-025.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 10.
  gs_fieldcatalog-seltext_l = TEXT-026.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.


  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-094.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 10.
  gs_fieldcatalog-seltext_l = TEXT-095.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.


  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-037.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 10.
  gs_fieldcatalog-seltext_l = TEXT-038.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-039.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 5.
  gs_fieldcatalog-seltext_l = TEXT-040.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-096.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 24.
  gs_fieldcatalog-seltext_l = TEXT-097.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-098.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 24.
  gs_fieldcatalog-seltext_l = TEXT-099.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-041.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 20.
  gs_fieldcatalog-seltext_l = TEXT-042.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-043.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen   = 20.
  gs_fieldcatalog-seltext_l = TEXT-044.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-045.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen = 10.
  gs_fieldcatalog-seltext_l = TEXT-046.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-047.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen = 12.
  gs_fieldcatalog-seltext_l = TEXT-048.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-049.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen = 40.
  gs_fieldcatalog-seltext_l = TEXT-050.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-051.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen = 05.
  gs_fieldcatalog-seltext_l = TEXT-052.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-053.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen = 22.
  gs_fieldcatalog-seltext_l = TEXT-054.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.


  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-100.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen = 04.
  gs_fieldcatalog-seltext_l = TEXT-101.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-102.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen = 20.
  gs_fieldcatalog-seltext_l = TEXT-103.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-055.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen = 22.
  gs_fieldcatalog-seltext_l = TEXT-056.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-057.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen = 22.
  gs_fieldcatalog-seltext_l = TEXT-058.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

  lv_count = lv_count + 1.
  gs_fieldcatalog-col_pos   = lv_count.
  gs_fieldcatalog-fieldname = TEXT-059.
  gs_fieldcatalog-tabname   = 'GT_DISPLAY'.
  gs_fieldcatalog-outputlen = 22.
  gs_fieldcatalog-seltext_l = TEXT-060.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
  CLEAR gs_fieldcatalog.

*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
*      i_callback_program      = sy-repid
*      i_callback_user_command = 'USER_COMMAND'
*      is_layout               = gs_layout
*      it_fieldcat             = gt_fieldcatalog
*      i_save                  = 'A'
*    TABLES
*      t_outtab                = gt_display
*    EXCEPTIONS
*      program_error           = 1
*      OTHERS                  = 2.
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.

ENDFORM.


FORM user_command USING r_ucomm TYPE sy-ucomm
                        rs_selfield TYPE slis_selfield.

  CONSTANTS : lc_po_number TYPE slis_fieldname VALUE 'PO_NUMBER',
              lc_bes(3)    VALUE 'BES'.
  CASE r_ucomm.
*User clicks a transaction code and that tcode is called from ALV
    WHEN '&IC1'.

      IF rs_selfield-fieldname = lc_po_number.

        READ TABLE gt_display INTO DATA(ls_display_hotspot) INDEX rs_selfield-tabindex.
        IF sy-subrc EQ 0.

          SET PARAMETER ID lc_bes FIELD ls_display_hotspot-po_number.

          CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.

        ENDIF.

      ENDIF.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form email
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM email .

**select option  maintained ZDEALMEMO_TO_PO via tcode stvarv

  CONSTANTS : lc_tvarvc   TYPE tvarvc-name VALUE 'ZDEALMEMO_TO_PO'.

  DATA: l_send_request    TYPE REF TO cl_bcs,         " Send request
        l_body            TYPE bcsy_text,                " Mail body
        l_success         TYPE bcsy_text,                " Atchmnt for success
        l_error           TYPE bcsy_text,                " Atchmnt for error
        wa_text           TYPE soli,                     " Work area for attach
        l_document        TYPE REF TO cl_document_bcs,   " Mail body
        l_sender          TYPE REF TO if_sender_bcs,     " Sender address
        l_recipient       TYPE REF TO if_recipient_bcs,  " Recipient
        l_size            TYPE sood-objlen,              " Size of Attachment
        l_lines           TYPE i,
        lc_today_date(10),
        lc_type           TYPE so_obj_tp VALUE 'RAW',
        lc_att_type       TYPE soodk-objtp VALUE 'xls',
        lt_list_of_emails TYPE STANDARD TABLE OF tvarvc,
        ls_list_of_emails TYPE tvarvc,
        lv_xstring_output TYPE xstring,
        lv_subject        TYPE so_obj_des,
        lv_TVARVC_name    TYPE tvarvc-name VALUE 'ZDEALMEMO_TO_PO',
        lv_mailto         TYPE ad_smtpadr.


  CONSTANTS : lc_underscore(1) VALUE '_',
              lc_sender(50)    VALUE 'SAP_WFRT@SETINDIA.COM',
              lc_typee         TYPE tvarvc-type VALUE 'S',
              lc_sign          TYPE tvarvc-sign VALUE 'I',
              lc_opti          TYPE tvarvc-opti VALUE 'EQ'.

  CONSTANTS : lc_fieldcat1(30) VALUE 'TENTID',
              lc_fieldcat2(30) VALUE 'TRANSACTION_TYPE',
              lc_fieldcat3(30) VALUE 'DEAL_MEMO_STATUS',
              lc_fieldcat4(30) VALUE 'RECORD_STATUS',
              lc_fieldcat5(30) VALUE 'CONT_STATUS',
              lc_fieldcat6(30) VALUE 'CONTRACTRECORD',
              lc_fieldcat7(30) VALUE 'CURRENCY',
              lc_fieldcat8(30) VALUE 'OPEN_PO_VALUE'.



  FIELD-SYMBOLS : <fs_output>     TYPE ANY TABLE.

  IF gt_display IS NOT INITIAL.

    SELECT
          name,
          type,
          numb,
          sign,
          opti,
          low,
          high,
          clie_indep
    FROM tvarvc
    INTO TABLE @DATA(gt_recipients)
          WHERE name EQ @lv_tvarvc_name.

    IF sy-subrc NE 0.

      WRITE :/ TEXT-002.

    ENDIF.


    GET REFERENCE OF gt_display INTO DATA(lo_data_output_display).

    ASSIGN lo_data_output_display->* TO <fs_output>.


    TRY."Factory method
        cl_salv_table=>factory(
        IMPORTING r_salv_table = DATA(lo_table_output)
        CHANGING  t_table      = <fs_output> ).

        DATA(lt_fcat_output) =
              cl_salv_controller_metadata=>get_lvc_fieldcatalog(
              r_columns      = lo_table_output->get_columns( )
              r_aggregations = lo_table_output->get_aggregations( ) ).


        IF lt_fcat_output IS NOT INITIAL.

          DELETE lt_fcat_output WHERE fieldname EQ lc_fieldcat1.
          DELETE lt_fcat_output WHERE fieldname EQ lc_fieldcat2.
          DELETE lt_fcat_output WHERE fieldname EQ lc_fieldcat3.
          DELETE lt_fcat_output WHERE fieldname EQ lc_fieldcat4.
          DELETE lt_fcat_output WHERE fieldname EQ lc_fieldcat5.
          DELETE lt_fcat_output WHERE fieldname EQ lc_fieldcat6.
          DELETE lt_fcat_output WHERE fieldname EQ lc_fieldcat7.
          DELETE lt_fcat_output WHERE fieldname EQ lc_fieldcat8.

          LOOP AT lt_fcat_output ASSIGNING FIELD-SYMBOL(<fs_fcat_output>).

            IF <fs_fcat_output>-fieldname EQ TEXT-003.

              <fs_fcat_output>-reptext = TEXT-004.

            ELSEIF <fs_fcat_output>-fieldname EQ TEXT-005.

              <fs_fcat_output>-reptext = TEXT-006.

            ELSEIF <fs_fcat_output>-fieldname EQ TEXT-007.

              <fs_fcat_output>-reptext = TEXT-008.

            ELSEIF <fs_fcat_output>-fieldname EQ TEXT-009.

              <fs_fcat_output>-reptext = TEXT-010.

            ELSEIF <fs_fcat_output>-fieldname EQ TEXT-011.

              <fs_fcat_output>-reptext = TEXT-012.

            ELSEIF <fs_fcat_output>-fieldname EQ TEXT-013.

              <fs_fcat_output>-reptext = TEXT-014.

            ELSEIF <fs_fcat_output>-fieldname EQ TEXT-015.

              <fs_fcat_output>-reptext = TEXT-016.

            ELSEIF <fs_fcat_output>-fieldname EQ TEXT-017.

              <fs_fcat_output>-reptext = TEXT-018.

            ELSEIF <fs_fcat_output>-fieldname EQ TEXT-019.

              <fs_fcat_output>-reptext = TEXT-020.

            ELSEIF <fs_fcat_output>-fieldname EQ TEXT-021.

              <fs_fcat_output>-reptext = TEXT-022.

            ELSEIF <fs_fcat_output>-fieldname EQ TEXT-023.

              <fs_fcat_output>-reptext = TEXT-024.

            ELSEIF <fs_fcat_output>-fieldname EQ TEXT-025.

              <fs_fcat_output>-reptext = TEXT-026.

            ELSEIF <fs_fcat_output>-fieldname EQ TEXT-027.

              <fs_fcat_output>-reptext = TEXT-028.

            ELSEIF <fs_fcat_output>-fieldname EQ TEXT-029.

              <fs_fcat_output>-reptext = TEXT-030.

            ELSEIF <fs_fcat_output>-fieldname EQ TEXT-031.

              <fs_fcat_output>-reptext = TEXT-032.

            ELSEIF <fs_fcat_output>-fieldname EQ TEXT-033.

              <fs_fcat_output>-reptext = TEXT-034.
            ELSEIF <fs_fcat_output>-fieldname EQ TEXT-035.

              <fs_fcat_output>-reptext = TEXT-036.

            ELSEIF <fs_fcat_output>-fieldname EQ TEXT-037.

              <fs_fcat_output>-reptext = TEXT-038.

            ELSEIF <fs_fcat_output>-fieldname EQ TEXT-039.

              <fs_fcat_output>-reptext = TEXT-040.

            ELSEIF <fs_fcat_output>-fieldname EQ TEXT-041.

              <fs_fcat_output>-reptext = TEXT-042.

            ELSEIF <fs_fcat_output>-fieldname EQ TEXT-043.

              <fs_fcat_output>-reptext = TEXT-044.

            ELSEIF <fs_fcat_output>-fieldname EQ TEXT-045.

              <fs_fcat_output>-reptext = TEXT-046.

            ELSEIF <fs_fcat_output>-fieldname EQ TEXT-047.

              <fs_fcat_output>-reptext = TEXT-048.

            ELSEIF <fs_fcat_output>-fieldname EQ TEXT-049.

              <fs_fcat_output>-reptext = TEXT-050.

            ELSEIF <fs_fcat_output>-fieldname EQ TEXT-051.

              <fs_fcat_output>-reptext = TEXT-052.

            ELSEIF <fs_fcat_output>-fieldname EQ TEXT-053.

              <fs_fcat_output>-reptext = TEXT-054.

            ELSEIF <fs_fcat_output>-fieldname EQ TEXT-055.

              <fs_fcat_output>-reptext = TEXT-056.

            ELSEIF <fs_fcat_output>-fieldname EQ TEXT-057.

              <fs_fcat_output>-reptext = TEXT-058.

            ELSEIF <fs_fcat_output>-fieldname EQ TEXT-059.

              <fs_fcat_output>-reptext = TEXT-060.

            ELSEIF <fs_fcat_output>-fieldname EQ TEXT-086.

              <fs_fcat_output>-reptext = TEXT-087.

            ELSEIF <fs_fcat_output>-fieldname EQ TEXT-092.

              <fs_fcat_output>-reptext = TEXT-093.

            ENDIF.

          ENDLOOP.
        ENDIF.

        DATA(lo_result_output) =
              cl_salv_ex_util=>factory_result_data_table(
              r_data         = lo_data_output_display
              t_fieldcatalog = lt_fcat_output ).

        cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform(
        EXPORTING
          xml_type      = if_salv_bs_xml=>c_type_xlsx
          xml_version   = cl_salv_bs_a_xml_base=>get_version( )
          r_result_data = lo_result_output
          xml_flavour   = if_salv_bs_c_tt=>c_tt_xml_flavour_export
          gui_type      = if_salv_bs_xml=>c_gui_type_gui
        IMPORTING
          xml           = lv_xstring_output ).



    ENDTRY.


    TRY.
        "Create send request
        DATA(lo_send_request) = cl_bcs=>create_persistent( ).

        DATA(lt_body) = VALUE bcsy_text(
              ( line = TEXT-106 ) ( )
              ( line = TEXT-107 ) ( )
              ( line = space    ) ( )
              ( line = TEXT-108 ) ( ) ).

        CONCATENATE TEXT-109
                    sy-datum+6(2)
                    sy-datum+4(2)
                    sy-datum+0(4) INTO lv_subject SEPARATED BY '_' .

        DATA(lo_document) = cl_document_bcs=>create_document(
              i_type = lc_type
              i_text = lt_body
              i_subject = lv_subject ).

        IF lv_xstring_output IS NOT INITIAL.


          lo_document->add_attachment(
          i_attachment_type    = lc_att_type
          i_attachment_size    = CONV #( xstrlen( lv_xstring_output ) )
          i_attachment_subject = TEXT-110
          i_attachment_header  = VALUE #( ( line = |output.xlsx| ) ) "not possible to avoid hardcode as per syntax
          i_att_content_hex    = cl_bcs_convert=>xstring_to_solix( lv_xstring_output ) ).

        ENDIF.

        "Add document to send request
        lo_send_request->set_document( lo_document ).

        lo_send_request->set_sender(
        cl_cam_address_bcs=>create_internet_address(
        i_address_string = CONV #( lc_sender ) ) ).


        LOOP AT gt_recipients INTO DATA(ls_rec).
          IF ls_rec IS NOT INITIAL.

            lo_send_request->add_recipient(
            i_recipient = cl_cam_address_bcs=>create_internet_address(
            i_address_string = CONV #( |{ ls_rec-low }| ) )
            i_express   = abap_true ).

          ENDIF.

        ENDLOOP.


        "Send Email
        DATA(lv_sent_to_all) = lo_send_request->send( ).

        IF lv_sent_to_all IS INITIAL.
          MESSAGE i500(sbcoms) WITH lv_mailto.
        ELSE.
          MESSAGE s022(so).
        ENDIF.





    ENDTRY.

  ENDIF.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form alv_display
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM alv_display .

  IF gt_display IS NOT INITIAL.

    gs_layout-zebra = abap_true.
    gs_layout-colwidth_optimize = abap_true.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program      = sy-repid
        i_callback_user_command = 'USER_COMMAND'
        is_layout               = gs_layout
        it_fieldcat             = gt_fieldcatalog
        i_save                  = 'A'
      TABLES
        t_outtab                = gt_display
      EXCEPTIONS
        program_error           = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ELSE.

    MESSAGE TEXT-111 TYPE 'I'.

  ENDIF.

ENDFORM.