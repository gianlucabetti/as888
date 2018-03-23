000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200070111     H* FICNA1R *----------------------------------------------------*
000300000000     H*-----------------*                                            *
000400090723     H*         -  ELIMINA CONTEGGI AUTISTI   s i m u l a t i        *
000500000000     H*--------------------------------------------------------------*
000600090723     FficnS1D   CF   E             WORKSTN
000700070112     Ffiapd01L  iF   E           K DISK
000800070112      *
000900090723     Ffifst01L  iF   E           K DISK
001000090723     Ffifst02L  uF   E           K DISK    rename(FIFTT000:FIFTT02)
001100090723     Ffifsd05L  UF   E           K DISK
001200070112     H*--------------------------------------------------------------*
001300930707     F*
001400931126     D WLBDAT          DS
001500950120     D  G02DAT                 1      8  0
001600950120     D  G02INV                 9     16  0
001700950120     D  G02ERR                17     17
001800950120     D  G02TGI                18     22  0
001900070112      *
002000931126     D WGIDAT          DS
002100950120     D  GIODAT                 1      8  0
002200950120     D  GIOINV                 9     16  0
002300950120     D  GIOTGI                17     21  0
002400070112      *
002500131007     D tibs42ds      E DS
002501131007     D uteautds      E DS
002600070111     D CNCR80        E DS
002700070111     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002800091104     D* DS PER TRUL06R - CARICAMENTO £X
002900091104     D DSUL06        E DS                  EXTNAME(TRUL06DS)
003000091104     D  LIN                    1     90  0
003100091104     D                                     DIM(30)                              FIL. COMODO
003400920810     D KPJBA         E DS
003500900521     C*****************************************************************
005400091104     C*
005500070111     C     FOR01         TAG
005600070111     c                   setoff                                       99
005700070111     C                   EXFMT     ficna01
005800070111     C** CMD3 -
005900070111     C   KC              GOTO      FINE
006000070111     C* CONTROLLI
006100070111     C                   EXSR      CONTR1
006200070111     C  NKF
006300070111     COR 99              GOTO      FOR01
006400070111     C*
006500070111     C                   EXSR      aggiorna
006600070111     C*
006700070111     C     FINE          TAG
006800070111     C                   SETON                                        LR
006900070111     C*---------------------------------------------------------------*
007000070112     C*- - CONTROLLI video    -                                      -*
007100070112     C*---------------------------------------------------------------*
007200070112     C     CONTR1        BEGSR
007300070112     C*
007400070112     C                   SETOFF                                       99
007500070112     C* Autista Conteggi
007600070112     C                   clear                   VIDDA1
007700070112     C     kapd          CHAIN     fiapd01L
007800070112     C                   IF        not %Found(fiapd01L)
007900070112     C                   SETON                                        2299
008000070112     C                   GOTO      ENDCT1
008100070112    2C                   END
008200100223     C                   MOVEL     APDRSf        VIDDA1
008201130808     C*  Sotto autorizzazione solo chi valorizza può utilizzarlo oppure EDP
008202130808     c                   if        %subst(knmus:1:3) <>'EDP'
008203130808     C*
008204130808     C* Controllo autorizzazione all'utilizzo del programma
008205130808     c* Controllo se abilitato
008206131007     c                   clear                   tibs42ds
008207131007     c                   movel     vidau1        i42pge
008209131007     c                   call      'TIBS42R'
008210131007     c                   parm                    tibs42ds
008211131007     c                   movel     o42uni        uteautds
008212130808     c* 98 indicatore che protegge tutto
008213130808     c     §autcon       ifne      'S'
008214130808     C                   exfmt     NONAUT
008215130808     C                   GOTO      FINE
008216130808     C                   endif
008217130808      *
008218130808     c                   end
008300070112     C*
008400070112     C* D A T A   D I S T I N T A
008500070112     C                   MOVE      VIDDDC        G02DAT
008600070112     C                   MOVEL     *BLANK        G02ERR
008700070112     C                   CALL      'XSRDA8'
008800070112     C                   PARM                    WLBDAT
008900070112    2C     G02ERR        IFEQ      '1'
009000070112     C                   SETON                                        2399
009100070112     C                   GOTO      ENDCT1
009200070112    2C                   END
009300070112     C                   Z-ADD     G02INV        W01DDC
009400070112     C                   Z-ADD     G02DAT        VIDDDC
009500070112     C*
009600070112     C* CONTROLLO SE ESISTE LA SELEZIONE
009700090723     C     KFTT          chain(n)  fifst02L
009800090723     c                   if        not %Found(fifst02L)
009900070112     C                   SETON                                        2699
010000070112     C                   GOTO      ENDCT1
010100070112     c                   end
010200070112      *
010300070112     C* CONTROLLO SE ESISTE LA SELEZIONE del documento se immesso
010400070112     c                   if        vidndc > 0
010500070112     c                   move      'C'           W01TSR
010600090723     C     KFTT01        chain     fifst01L
010700090723     c                   if        not %Found(fifst01L)
010800070112     c                   move      'R'           W01TSR
010900090723     C     KFTT01        chain     fifst01L
011000090723     c                   if        not %Found(fifst01L)
011100070112     C                   SETON                                        262799
011200070112     C                   GOTO      ENDCT1
011300070112     c                   end
011400070112     c                   end
011500070112      *
011600070112     c                   else
011700070112      *
011800070112     c                   end
011900070112     C*
012000070112     C     ENDCT1        ENDSR
012100070112     C*---------------------------------------------------------------*
012200070112     C*   Aggiorna gli archivi
012300070112     C*---------------------------------------------------------------*
012400070112     C     Aggiorna      BegSR
012500070112     C*
012600090723     C     kFTT          setll     Fifst02L
012700090723     C     kFTT          reade     Fifst02L
012800090723     c                   dow       not %EoF(Fifst02l)
012900070112     C*
013000070112     C* Se mirato sul documento
013100070112     c                   if        vidndc > 0  and vidndc <> fttndc
013200070112     c                   goto      salta
013300070112     c                   end
013400070112     C*
013500090723     C* Se mirato alla data
013600090723     c                   if        W01DDC > 0  and W01DDC <> fttddc
013700090723     c                   goto      salta
013800090723     c                   end
013900090723     C*
014000070112     C*  prima di cancellare deve impostare FIFCE x generare la spia
014100070112     C*   delle competenze
014200090723     c     kFTD          setll     Fifsd05l
014300090723     c     kftd          Reade     Fifsd05l
014400070112      *
014500090723     c                   dow       not %EoF(Fifsd05l)
014600090723     c                   delete    fiftd000
014700090723     c     kFTD          Reade     Fifsd05l
014800070112     c                   enddo
014900070111     C*
015000070112     c                   delete    fiFTT02
015100070112     C*
015200070112     c     salta         tag
015300070112     C*
015400090723     C     kFTT          reade     Fifst02L
015500070112     c                   enddo
015600070112     C*
015700070112     C                   EndSR
015800070112     C*****************************************************************
015900070112     C     *inzsr        BegSR
016000070112      *
016100070112     C     *ENTRY        PLIST
016200070112     C                   PARM                    KPJBA
016300070112      *
016400091104     c                   seton                                        33
016500091104     c                   if        %subst(knmus:1:3) = 'EDP'
016600091104     c                   setoff                                       33
016700091104     c                   end
016800091104      *
016900070112     C                   Z-ADD     1             CODUT
017000070112     C                   CALL      'X§PARUT'
017100070112     C                   PARM                    UT§DSE
017200070112     C                   MOVEL     RAGUT         vidrsu
017300070112     C                   MOVEL     REC80         CNCR80
017400091104     C***
018700070112     C*---------------------------------------------------------------*
018800070112     C     kFTT          KLIST
018900070112     C                   KFLD                    VIDAU1
019000070112      *
019100070112     C     kFTT01        KLIST
019200070112     C                   KFLD                    VIDAU1
019300070112     C                   KFLD                    W01TSR
019400070112     C                   KFLD                    VIDNDC
019500070112     C                   KFLD                    W01DDC
019600070112      *
019700070112     C     kFTD          KLIST
019800070112     C                   KFLD                    FTTPDR
019900070112     C                   KFLD                    FTTTSR
020000070112     C                   KFLD                    FTTNDC
020100070112     C                   KFLD                    FTTDDC
020200070112     C*
020300070112     C     kapd          KLIST
020400070112     C                   KFLD                    Ktip              1
020500070112     C                   KFLD                    VIDAU1
020600070112      *
020700070112     C                   move      'A'           Ktip              1
020800070112     C*
020900070112     C* DEFINIZIONE CAMPI
021000070112     C     *LIKE         DEFINE    FTTDDC        W01DDC
021100070112     C     *LIKE         DEFINE    FTTTSR        W01TSR
021200070112     C*
021300070112     C* LA DATA DEL GIORNO LA PRENDO DA TIME
021400070112     C                   TIME                    W0140            14 0
021500070112     C                   MOVE      W0140         UDATE8            8 0
021600070112     C*
021700070112     C                   EndSR
021800070112     C*---------------------------------------------------------------*
