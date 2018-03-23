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
002600131007     D uteautds      E DS
002700070111     D CNCR80        E DS
002800070111     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002900091104     D* DS PER TRUL06R - CARICAMENTO £X
003000091104     D DSUL06        E DS                  EXTNAME(TRUL06DS)
003100091104     D  LIN                    1     90  0
003200091104     D                                     DIM(30)                              FIL. COMODO
003300920810     D KPJBA         E DS
003400900521     C*****************************************************************
003500091104     C*
003600070111     C     FOR01         TAG
003700070111     c                   setoff                                       99
003800070111     C                   EXFMT     ficna01
003900070111     C** CMD3 -
004000070111     C   KC              GOTO      FINE
004100070111     C* CONTROLLI
004200070111     C                   EXSR      CONTR1
004300070111     C  NKF
004400070111     COR 99              GOTO      FOR01
004500070111     C*
004600070111     C                   EXSR      aggiorna
004700070111     C*
004800070111     C     FINE          TAG
004900070111     C                   SETON                                        LR
005000070111     C*---------------------------------------------------------------*
005100070112     C*- - CONTROLLI video    -                                      -*
005200070112     C*---------------------------------------------------------------*
005300070112     C     CONTR1        BEGSR
005400070112     C*
005500070112     C                   SETOFF                                       99
005600070112     C* Autista Conteggi
005700070112     C                   clear                   VIDDA1
005800070112     C     kapd          CHAIN     fiapd01L
005900070112     C                   IF        not %Found(fiapd01L)
006000070112     C                   SETON                                        2299
006100070112     C                   GOTO      ENDCT1
006200070112    2C                   END
006300100223     C                   MOVEL     APDRSf        VIDDA1
006400160909     C*  Sotto autorizzazione solo chi valorizza può utilizzarlo
006500130808     C*
006600130808     C* Controllo autorizzazione all'utilizzo del programma
006700130808     c* Controllo se abilitato
006800131007     c                   clear                   tibs42ds
006900131007     c                   movel     vidau1        i42pge
007000131007     c                   call      'TIBS42R'
007100131007     c                   parm                    tibs42ds
007200131007     c                   movel     o42uni        uteautds
007300130808     c* 98 indicatore che protegge tutto
007400130808     c     §autcon       ifne      'S'
007500130808     C                   exfmt     NONAUT
007600130808     C                   GOTO      FINE
007700130808     C                   endif
007800070112     C*
007900070112     C* D A T A   D I S T I N T A
008000070112     C                   MOVE      VIDDDC        G02DAT
008100070112     C                   MOVEL     *BLANK        G02ERR
008200070112     C                   CALL      'XSRDA8'
008300070112     C                   PARM                    WLBDAT
008400070112    2C     G02ERR        IFEQ      '1'
008500070112     C                   SETON                                        2399
008600070112     C                   GOTO      ENDCT1
008700070112    2C                   END
008800070112     C                   Z-ADD     G02INV        W01DDC
008900070112     C                   Z-ADD     G02DAT        VIDDDC
009000070112     C*
009100070112     C* CONTROLLO SE ESISTE LA SELEZIONE
009200090723     C     KFTT          chain(n)  fifst02L
009300090723     c                   if        not %Found(fifst02L)
009400070112     C                   SETON                                        2699
009500070112     C                   GOTO      ENDCT1
009600070112     c                   end
009700070112      *
009800070112     C* CONTROLLO SE ESISTE LA SELEZIONE del documento se immesso
009900070112     c                   if        vidndc > 0
010000070112     c                   move      'C'           W01TSR
010100090723     C     KFTT01        chain     fifst01L
010200090723     c                   if        not %Found(fifst01L)
010300070112     c                   move      'R'           W01TSR
010400090723     C     KFTT01        chain     fifst01L
010500090723     c                   if        not %Found(fifst01L)
010600070112     C                   SETON                                        262799
010700070112     C                   GOTO      ENDCT1
010800070112     c                   end
010900070112     c                   end
011000070112      *
011100070112     c                   else
011200070112      *
011300070112     c                   end
011400070112     C*
011500070112     C     ENDCT1        ENDSR
011600070112     C*---------------------------------------------------------------*
011700070112     C*   Aggiorna gli archivi
011800070112     C*---------------------------------------------------------------*
011900070112     C     Aggiorna      BegSR
012000070112     C*
012100090723     C     kFTT          setll     Fifst02L
012200090723     C     kFTT          reade     Fifst02L
012300090723     c                   dow       not %EoF(Fifst02l)
012400070112     C*
012500070112     C* Se mirato sul documento
012600070112     c                   if        vidndc > 0  and vidndc <> fttndc
012700070112     c                   goto      salta
012800070112     c                   end
012900070112     C*
013000090723     C* Se mirato alla data
013100090723     c                   if        W01DDC > 0  and W01DDC <> fttddc
013200090723     c                   goto      salta
013300090723     c                   end
013400090723     C*
013500070112     C*  prima di cancellare deve impostare FIFCE x generare la spia
013600070112     C*   delle competenze
013700090723     c     kFTD          setll     Fifsd05l
013800090723     c     kftd          Reade     Fifsd05l
013900070112      *
014000090723     c                   dow       not %EoF(Fifsd05l)
014100090723     c                   delete    fiftd000
014200090723     c     kFTD          Reade     Fifsd05l
014300070112     c                   enddo
014400070111     C*
014500070112     c                   delete    fiFTT02
014600070112     C*
014700070112     c     salta         tag
014800070112     C*
014900090723     C     kFTT          reade     Fifst02L
015000070112     c                   enddo
015100070112     C*
015200070112     C                   EndSR
015300070112     C*****************************************************************
015400070112     C     *inzsr        BegSR
015500070112      *
015600070112     C     *ENTRY        PLIST
015700070112     C                   PARM                    KPJBA
015800070112      *
015900091104     c                   seton                                        33
016000091104     c                   if        %subst(knmus:1:3) = 'EDP'
016100091104     c                   setoff                                       33
016200091104     c                   end
016300091104      *
016400070112     C                   Z-ADD     1             CODUT
016500070112     C                   CALL      'X§PARUT'
016600070112     C                   PARM                    UT§DSE
016700070112     C                   MOVEL     RAGUT         vidrsu
016800070112     C                   MOVEL     REC80         CNCR80
016900091104     C***
017000070112     C*---------------------------------------------------------------*
017100070112     C     kFTT          KLIST
017200070112     C                   KFLD                    VIDAU1
017300070112      *
017400070112     C     kFTT01        KLIST
017500070112     C                   KFLD                    VIDAU1
017600070112     C                   KFLD                    W01TSR
017700070112     C                   KFLD                    VIDNDC
017800070112     C                   KFLD                    W01DDC
017900070112      *
018000070112     C     kFTD          KLIST
018100070112     C                   KFLD                    FTTPDR
018200070112     C                   KFLD                    FTTTSR
018300070112     C                   KFLD                    FTTNDC
018400070112     C                   KFLD                    FTTDDC
018500070112     C*
018600070112     C     kapd          KLIST
018700070112     C                   KFLD                    Ktip              1
018800070112     C                   KFLD                    VIDAU1
018900070112      *
019000070112     C                   move      'A'           Ktip              1
019100070112     C*
019200070112     C* DEFINIZIONE CAMPI
019300070112     C     *LIKE         DEFINE    FTTDDC        W01DDC
019400070112     C     *LIKE         DEFINE    FTTTSR        W01TSR
019500070112     C*
019600070112     C* LA DATA DEL GIORNO LA PRENDO DA TIME
019700070112     C                   TIME                    W0140            14 0
019800070112     C                   MOVE      W0140         UDATE8            8 0
019900070112     C*
020000070112     C                   EndSR
020100070112     C*---------------------------------------------------------------*
