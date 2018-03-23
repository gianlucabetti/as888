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
001500161125     D kpjbas          S                   LIKE(kpjba)
001600980521      *   S C H I E R E
001700161125     D MSG             S             78    DIM(4) CTDATA PERRCD(1)              MSG VIDEO
001800010608     D datoggi         S              8  0
001900010608
002000010608     D W0140           S             14  0
002100010608     D Wdtgio          S              8  0
002200010608     D dateu           S              8  0
002300020603
002400980521      *   D S   I N T E R N E / E S T E R N E
002500001009     D WLBDAT          DS                  INZ
002600001009     D  G02DAT                 1      8  0
002700001009     D  G02INV                 9     16  0
002800001009     D  G02ERR                17     17
002900001009     D  G02TGI                18     22  0
003000010308
003100001016     D TIBS02DS      E DS
003200980529     D UT§DSE0F      E DS
003300980601     D CNCR80        E DS
003400980521      *
003500980521     D KPJBA         E DS
003600161125     D dbir          E DS
003700010608      *
003800001006      *------------------------------------------------------------------------*
003900001006     C                   do        *hival
004000001009      * Pulizia 1° Videata
004100010601     C                   if        (*in28 = *off and *in90 = *off)
004200161125     C                   MOVE      *BLANKS       V1Caz
004300161125     C                   MOVE      *BLANKS       V1Daz
004400161125     C                   MOVE      '?'           V1Caz
004500001009     C                   endif
004600001011
004700010608      * Emissione 1° Videata
004800160715     C                   exfmt     TNTB151
004900001006      * F3=Fine
005000001006     C   kc              leave
005100010405
005200001006      * Controlli 1° videata
005300010604     C                   SETOFF                                       28
005400001006     C                   exsr      Sr_Contr01
005500980623
005600010601     C   28              iter
005700001009     C   90              iter
005800001009
005900010608      * Caricamento e pulizia seconda videata
006000010608     C                   exsr      Sr_carvid
006100010608
006200010608      * Emissione 2° Videata
006300010608     C     riemetto      tag
006400160715     C                   exfmt     TNTB152
006500010611
006600010611     C                   eval      *in28 = *off
006700010608
006800010606      * F3=Fine
006900010606     C   kc              LEAVE
007000010606
007100010608      * F12=Ritorno
007200161125     C   kl              clear                   v2caz
007300161125     c   kl              clear                   v2daz
007400010608     C   kl              ITER
007500010608
007600010608     C                   exsr      Sr_Contr02
007700010608     C   28              goto      riemetto
007800010608
007900010608      * F6=Aggiornamento
008000161125     C   kf              exsr      Sr_Lancia
008100010608     C   kf              iter
008200010608
008300010608     C                   goto      riemetto
008400010608
008500001006     C                   enddo
008600981204
008700001006     C                   eval      *inlr = *on
008800980521      **********************************************************************
008900980521      * CONTROLLI VIDEO 1
009000980521      **********************************************************************
009100001006     C     Sr_Contr01    begsr
009200001009      * Reset indicatori
009300010612     C                   setoff                                       90
009400010601
009500161125     C     '?'           scan      v1caz                                  31
009600010601     C     *in31         ifeq      *on
009700010601     C                   eval      *in90 = *on
009800010601     C                   clear                   TIBS02DS
009900010601     C                   movel     'R'           t02mod
010000010601     C                   movel     knsif         t02sif
010100161125     C                   movel     'BIR'         t02cod
010200010601     C                   call      'TIBS02R'
010300010601     C                   parm                    KPJBA
010400010601     C                   parm                    TIBS02DS
010500010601     C                   if        t02err =  *blanks
010600161125     C                   movel     t02ke1        v1caz
010700010601     C                   SETON                                        90
010800010601     C                   ELSE
010900010612     C                   SETON                                        28
011000010612     C                   MOVEL     MSG(1)        V1CMSG
011100010601     C                   GOTO      ENDSRCONTR
011200010601     C                   endif
011300010604     C                   ENDIF
011400010601
011500010611      * controllo se campo valorizzato
011600161125     C                   IF        V1Caz  = *BLANKS
011700161125     C                   MOVE      *BLANKS       V1Daz
011800010612     C                   SETON                                        28        controllo P.O.emi.
011900010612     C                   MOVEL     MSG(1)        V1CMSG
012000010601     C                   GOTO      ENDSRCONTR                                   normale
012100010601     C                   ENDIF
012200010212
012300161125     C                   movel     v1caz         kke1
012400161125     C                   movel     'BIR'         kcod
012500161125     C     ktntbe        chain     tntbe01l
012600161125     C                   if        not%found(tntbe01l)
012700161125     C                   SETON                                        28        controllo P.O.emi.
012800161125     C                   MOVEL     MSG(4)        V1CMSG
012900161125     C                   GOTO      ENDSRCONTR                                   normale
013000161125     C                   endif
013100010611      * reperisco le descrizioni
013200161125     C     v1caz         CHAIN     kfazn11l                           31
013300010611     C                   IF        *IN31 = *OFF
013400161125     C                   MOVEL     KDSAZ         V2Daz
013500161125     C                   MOVEL     kdsaz         V1Daz
013600010611     C                   ELSE
013700161115     C                   SETON                                        28
013800161125     C                   MOVEL     MSG(2)        V1CMSG
013900161115     C                   GOTO      ENDSRCONTR
014000161125     C                   CLEAR                   V1Daz
014100161125     C                   CLEAR                   V2Daz
014200010611     C                   ENDIF
014300010611
014400010611     C     ENDSRCONTR    endsr
014500010608      **********************************************************************
014600010608      * CONTROLLI VIDEO 2
014700010608      **********************************************************************
014800010608     C     Sr_Contr02    begsr
014900010608      * Reset indicatori
015000161125     C                   setoff                                       2842
015100010608
015200160715      * data
015300161125     c                   if        not *in43
015400160715     c                   if        v2cdat <> 0
015500160715     C                   Z-ADD     v2cdat        G02DAT
015600160715     C                   MOVEL     *BLANK        G02ERR
015700160715     C                   CALL      'XSRDA8'
015800160715     C                   PARM                    WLBDAT
015900160715     c                   if        g02err = '0'
016000160715     C                   MOVEL     G02INV        v2cdatg           8 0
016100160718     C                   MOVEL     G02dat        v2cdat
016200160715     c                   else
016300160715     C                   SETON                                        2842
016400161125     C                   MOVEL     MSG(3)        V1CMSG
016500160715     C                   GOTO      ENDSRCONT2
016600160715     c                   endif
016700161125     c                   else
016800161125     C                   SETON                                        2842
016900161125     C                   MOVEL     MSG(3)        V1CMSG
017000161125     C                   GOTO      ENDSRCONT2
017100161125     c                   endif
017200160715     c                   endif
017300010608
017400010608     C     ENDSRCONT2    endsr
017500161125      **********************************************************************
017600161125      * CARICAMENTO SECONDA VIDEATA
017700161125      **********************************************************************
017800161125     C     Sr_Carvid     begsr
017900161125     C                   clear                   dbir
018000161125     C                   clear                   v2csif
018100161125     C                   clear                   v2cdes
018200161125     C                   movel     v1caz         kke1
018300161125     C                   movel     'BIR'         kcod
018400161125     C     ktntbe        chain     tntbe01l
018500161125     C                   if        %found(tntbe01l)
018600161125     C                   movel     tbeke1        v2caz
018700161125     C                   movel     tbeke2        v2csif
018800161125     C                   movel     tbeuni        dbir
018900161125     C                   movel     §birdes       v2cdes
019000161125     C                   endif
019100161125     C     §birpjbu      comp      'N'                                    43
019200161125
019300161125     C                   endsr
019400010608      **********************************************************************
019500010608      * CARICAMENTO SECONDA VIDEATA
019600010608      **********************************************************************
019700161125     C     Sr_Lancia     begsr
019800161125     c                   movel     kpjba         kpjbas
019900161125     C                   MOVEL     v2caz         KCOAZ
020000161125      *se c'è forzatura sistema informativo
020100161125     c                   if        v2csif <> *blank
020200161125     C                   MOVEL     v2csif        Knsif
020300161125     c                   endif
020400161125      *
020500161128     C  n43              MOVEL     v2cdatg       KPJBU
020600161125     C                   CALL      'BCH10'
020700161125     C                   PARM                    KPJBA
020800161125     c                   movel     kpjbas        kpjba
020900010608
021000010608     C                   endsr
021100980923      *****************************************************************
021200980923      * ROUTINE INIZIALE
021300980923      *****************************************************************
021400980923     C     *INZSR        BEGSR
021500980923      *
021600980521     C     *ENTRY        PLIST
021700980521     C                   PARM                    KPJBA
021800001030
021900980521     C                   Z-ADD     1             CODUT
022000980521     C                   CALL      'X§PARUT'
022100980521     C                   PARM                    UT§DSE0F
022200980521     C                   MOVEL     RAGUT         RSUT
022300980521     C                   MOVEL     REC80         CNCR80
022400001221
022500010608      * reperisco data e ora
022600010608     C                   TIME                    W0140
022700010608      * UDATE IN GGMMAAAA
022800010608     C                   MOVE      W0140         WDTGIO
022900010608      * UDATE IN AAAAMMGG
023000010608     C                   Z-ADD     WDTGIO        G02DAT
023100010608     C                   MOVEL     *BLANK        G02ERR
023200010608     C                   CALL      'XSRDA8'
023300010608     C                   PARM                    WLBDAT
023400010608     C                   MOVEL     G02INV        DATEU
023500010608     C                   MOVEL     DATEU         DAToggi
023600010608
023700010601     C     KTNTBE        KLIST
023800010604     C                   KFLD                    KCOD
023900010601     C                   KFLD                    KKE1
024000010611
024100001006     C                   endsr
024200980521      *---------------------------------------------------------------------------------------------
024300980521** MSG  Lungh. 78                                                            *
024400161125Inserire Azione
024500161125Azione non presente a sistema
024600161125Data errata o non inserita
024700161125Azione non presente in tabella BIR
