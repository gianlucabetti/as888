000100940224     H*--------------------------------------------------------------*
000200171019     Ffnblp31l  iF A E           k DISK
000201171019     D* DS RICHIAMO PROGRAMMA CANCELLAZIONE BOLLA SINGOLA
000202171019     D DSLS63        E DS                  EXTNAME(FNLS63DS)
000203171019     D kpjba         E DS
001200900131     C*---------------------------------------------------------------*
010700060512     c
010701171019     C     *entry        plist
010702171019     C                   parm                    kpjba
010703171019         setll (26:2017:1019) fnblp31l   ;
010704171019         reade (26:2017:1019) fnblp31l   ;
010705171019         dow not %eof(fnblp31l);
010706171019         if blpksc=0262145;
010707171019             clear dsls63;
010708171019     C                   MOVE      BLPAAS        I63AAS
010709171019     C                   MOVE      BLPLNP        I63LNP
010710171019     C                   MOVE      BLPNRS        I63NRS
010711171019     C                   MOVE      BLPNSP        I63NSP
010712171019     C                   MOVEL     DSLS63        KPJBU
010713171019     c                   call      'FNLS63R'
010714171019     C                   PARM                    KPJBA
010715171019         endif;
010716171019            reade (26:2017:1019) fnblp31l   ;
010717171019         enddo;
010800030205      *
010900000000     C                   SETON                                        LR
