  SELECT FROM LFBW
         FIELDS MANDT,
                LIFNR,
                BUKRS,
                WITHT,
                WT_SUBJCT,
                QSREC,
                WT_WTSTCD,
                WT_WITHCD,
                WT_EXNR,
                WT_EXRT,
                WT_EXDF,
                WT_EXDT,
                WT_WTEXRS
          WHERE LIFNR = @IV_KONZS
            AND BUKRS = @IV_BUKRS
             INTO TABLE @ET_LFBW_V2.

    EV_COUNT_V2 = LINES( ET_LFBW_V2 ).
  ENDMETHOD.