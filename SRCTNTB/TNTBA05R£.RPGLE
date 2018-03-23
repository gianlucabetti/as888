000100001006     H DECEDIT('0,') DATEDIT(*YMD.)
000200980521      ****************************************************************
000300980521      *  RIEPILOGO INDICATORI
000400981008      ****************************************************************
000500980521      * 28 - ERRORE GENERICO DSPF
000600010604      * 31 - GENERICO
000700980624      * 90 - riemissione videata
000800990317      ****************************************************************
000900161125     Fkfazn11L  IF   E           K DISK
001000161125     FTNTBE01L  IF   E           K DISK
001100161125     FTNTBa05D  CF   E             WORKSTN
001200980521      *------------------------------------------------------------------------*
001300010604     D KCOD            S                   LIKE(TBECOD)
001400010604     D KKE1            S                   LIKE(TBEKE1)
001500980521      *   S C H I E R E
001600161125     D MSG             S             78    DIM(4) CTDATA PERRCD(1)              MSG VIDEO
001700010608     D datoggi         S              8  0
001800010608
001900010608     D W0140           S             14  0
002000010608     D Wdtgio          S              8  0
002100010608     D dateu           S              8  0
002200020603
002300980521      *   D S   I N T E R N E / E S T E R N E
002400001009     D WLBDAT          DS                  INZ
002500001009     D  G02DAT                 1      8  0
002600001009     D  G02INV                 9     16  0
002700001009     D  G02ERR                17     17
002800001009     D  G02TGI                18     22  0
002900010308
003000001016     D TIBS02DS      E DS
003100980529     D UT§DSE0F      E DS
003200980601     D CNCR80        E DS
003300161125     D DBIR          E DS
003400980521      *
003500980521     D KPJBA         E DS
003600010608      *
003700001006      *------------------------------------------------------------------------*
003800001006     C                   do        *hival
003900001009      * Pulizia 1° Videata
004000010601     C                   if        (*in28 = *off and *in90 = *off)
004100161125     C                   MOVE      *BLANKS       V1Caz
004200161125     C                   MOVE      *BLANKS       V1Daz
004300161125     C                   MOVE      '?'           V1Caz
004400001009     C                   endif
004500001011
004600010608      * Emissione 1° Videata
004700160715     C                   exfmt     TNTB151
004800001006      * F3=Fine
004900001006     C   kc              leave
005000010405
005100001006      * Controlli 1° videata
005200010604     C                   SETOFF                                       28
005300001006     C                   exsr      Sr_Contr01
005400980623
005500010601     C   28              iter
005600001009     C   90              iter
005700001009
005800010608      * Caricamento e pulizia seconda videata
005900010608     C                   exsr      Sr_carvid
006000010608
006100010608      * Emissione 2° Videata
006200010608     C     riemetto      tag
006300160715     C                   exfmt     TNTB152
006400010611
006500010611     C                   eval      *in28 = *off
006600010608
006700010606      * F3=Fine
006800010606     C   kc              LEAVE
006900010606
007000010608      * F12=Ritorno
007100161125     C   kl              clear                   v2caz
007200161125     c   kl              clear                   v2daz
007300010608     C   kl              ITER
007400010608
007500010608     C                   exsr      Sr_Contr02
007600010608     C   28              goto      riemetto
007700010608
007800010608      * F6=Aggiornamento
007900161125     C   kf              exsr      Sr_Lancia
008000010608     C   kf              iter
008100010608
008200010608     C                   goto      riemetto
008300010608
008400001006     C                   enddo
008500981204
008600001006     C                   eval      *inlr = *on
008700980521      **********************************************************************
008800980521      * CONTROLLI VIDEO 1
008900980521      **********************************************************************
009000001006     C     Sr_Contr01    begsr
009100001009      * Reset indicatori
009200010612     C                   setoff                                       90
009300010601
009400161125     C     '?'           scan      v1caz                                  31
009500010601     C     *in31         ifeq      *on
009600010601     C                   eval      *in90 = *on
009700010601     C                   clear                   TIBS02DS
009800010601     C                   movel     'R'           t02mod
009900010601     C                   movel     knsif         t02sif
010000161125     C                   movel     'BIR'         t02cod
010100010601     C                   call      'TIBS02R'
010200010601     C                   parm                    KPJBA
010300010601     C                   parm                    TIBS02DS
010400010601     C                   if        t02err =  *blanks
010500161125     C                   movel     t02ke1        v1caz
010600010601     C                   SETON                                        90
010700010601     C                   ELSE
010800010612     C                   SETON                                        28
010900010612     C                   MOVEL     MSG(1)        V1CMSG
011000010601     C                   GOTO      ENDSRCONTR
011100010601     C                   endif
011200010604     C                   ENDIF
011300010601
011400010611      * controllo se campo valorizzato
011500161125     C                   IF        V1Caz  = *BLANKS
011600161125     C                   MOVE      *BLANKS       V1Daz
011700010612     C                   SETON                                        28        controllo P.O.emi.
011800010612     C                   MOVEL     MSG(1)        V1CMSG
011900010601     C                   GOTO      ENDSRCONTR                                   normale
012000010601     C                   ENDIF
012100010212
012200010611      * reperisco le descrizioni
012300161125     C     v1caz         CHAIN     kfazn11l                           31
012400010611     C                   IF        *IN31 = *OFF
012500161125     C                   MOVEL     KDSAZ         V2Daz
012600161125     C                   MOVEL     kdsaz         V1Daz
012700010611     C                   ELSE
012800161115     C                   SETON                                        28
012900161125     C                   MOVEL     MSG(4)        V1CMSG
013000161115     C                   GOTO      ENDSRCONTR
013100161125     C                   CLEAR                   V1Daz
013200161125     C                   CLEAR                   V2Daz
013300010611     C                   ENDIF
013400060503     C                   ENDIF
013500010611
013600010611     C     ENDSRCONTR    endsr
013700010608      **********************************************************************
013800010608      * CONTROLLI VIDEO 2
013900010608      **********************************************************************
014000010608     C     Sr_Contr02    begsr
014100010608      * Reset indicatori
014200161125     C                   setoff                                       2842
014300161019     c                   if        *in10
014400161019     c                   seton                                        28
014500161019     c                   end
014600010608
014700160715      * data
014800160715     c                   if        v2cdat <> 0
014900160715     C                   Z-ADD     v2cdat        G02DAT
015000160715     C                   MOVEL     *BLANK        G02ERR
015100160715     C                   CALL      'XSRDA8'
015200160715     C                   PARM                    WLBDAT
015300160715     c                   if        g02err = '0'
015400160715     C                   MOVEL     G02INV        v2cdatg           8 0
015500160718     C                   MOVEL     G02dat        v2cdat
015600160715     c                   else
015700160715     C                   SETON                                        2842
015800161125     C                   MOVEL     MSG(3)        V1CMSG
015900160715     C                   GOTO      ENDSRCONT2
016000160715     c                   endif
016100160715     c                   endif
016200010608
016300010608     C     ENDSRCONT2    endsr
016400161125      **********************************************************************
016500161125      * CARICAMENTO SECONDA VIDEATA
016600161125      **********************************************************************
016700161125     C     Sr_Carvid     begsr
016800161125     C                   movel     v1caz         kke1
016900161125     C                   movel     'BIR'         kcod
017000161125     C     ktntbe        chain     tntbe01l
017100161125     C                   if        %found(tntbe01l)
017200161125     C                   movel     tbeke2        v2csif
017300161125     C                   movel     tbeuni        v2cdes
017400161125     C                   endif
017500161125
017600161125     C                   endsr
017700010608      **********************************************************************
017800010608      * CARICAMENTO SECONDA VIDEATA
017900010608      **********************************************************************
018000161125     C     Sr_Lancia     begsr
018100161125     c                   movel     kpjba         kpjbas
018200161125     C                   MOVEL     v2caz         KCOAZ
018300161125     C                   MOVEL     v2csif        Knsif
018400161125     C                   MOVEL     v2cdatg       KPJBU
018500161125     C                   CALL      'BCH10'
018600161125     C                   PARM                    KPJBA
018700161125     c                   movel     kpjbas        kpjba
018800010608
018900010608     C                   endsr
019000980923      *****************************************************************
019100980923      * ROUTINE INIZIALE
019200980923      *****************************************************************
019300980923     C     *INZSR        BEGSR
019400980923      *
019500980521     C     *ENTRY        PLIST
019600980521     C                   PARM                    KPJBA
019700001030
019800980521     C                   Z-ADD     1             CODUT
019900980521     C                   CALL      'X§PARUT'
020000980521     C                   PARM                    UT§DSE0F
020100980521     C                   MOVEL     RAGUT         RSUT
020200980521     C                   MOVEL     REC80         CNCR80
020300001221
020400010608      * reperisco data e ora
020500010608     C                   TIME                    W0140
020600010608      * UDATE IN GGMMAAAA
020700010608     C                   MOVE      W0140         WDTGIO
020800010608      * UDATE IN AAAAMMGG
020900010608     C                   Z-ADD     WDTGIO        G02DAT
021000010608     C                   MOVEL     *BLANK        G02ERR
021100010608     C                   CALL      'XSRDA8'
021200010608     C                   PARM                    WLBDAT
021300010608     C                   MOVEL     G02INV        DATEU
021400010608     C                   MOVEL     DATEU         DAToggi
021500010608
021600010601     C     KTNTBE        KLIST
021700010604     C                   KFLD                    KCOD
021800010601     C                   KFLD                    KKE1
021900010611
022000001006     C                   endsr
022100980521      *---------------------------------------------------------------------------------------------
022200980521** MSG  Lungh. 78                                                            *
022300161125Inserire Azione
022400161125Azione  non gestibile
022500160715Data errata
022600161125Azione  inesistente
