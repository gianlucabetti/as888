000100001006     H DECEDIT('0,') DATEDIT(*YMD.)
000200980521      ****************************************************************
000300980521      *  RIEPILOGO INDICATORI
000400981008      ****************************************************************
000500980521      * 28 - ERRORE GENERICO DSPF
000600010604      * 31 - GENERICO
000700980624      * 90 - riemissione videata
000800990317      ****************************************************************
000900010611     FAZORG05L  IF   E           K DISK
001000010611     FAZORG01L  IF   E           K DISK    RENAME(AZORG:AZORG000)
001100010604     FTNTBE01L  UF A E           K DISK
001200010608     FTNTB12D   CF   E             WORKSTN
001300980521      *------------------------------------------------------------------------*
001400010604     D KCOD            S                   LIKE(TBECOD)
001500010604     D KKE1            S                   LIKE(TBEKE1)
001600010611     D WFIL            S              3
001700980521      *   S C H I E R E
001800010612     D MSG             S             78    DIM(3) CTDATA PERRCD(1)              MSG VIDEO
001900010608     D datoggi         S              8  0
002000010608
002100010608     D W0140           S             14  0
002200010608     D Wdtgio          S              8  0
002300010608     D dateu           S              8  0
002400001006
002500010611     D SKDPD           S              3    dim(50)
002600010611
002700980521      *   D S   I N T E R N E / E S T E R N E
002800001009     D WLBDAT          DS                  INZ
002900001009     D  G02DAT                 1      8  0
003000001009     D  G02INV                 9     16  0
003100001009     D  G02ERR                17     17
003200001009     D  G02TGI                18     22  0
003300010308
003400001016     D TIBS02DS      E DS
003500980529     D UT§DSE0F      E DS
003600980601     D CNCR80        E DS
003700010608     D DOES          E DS
003800980521      *
003900980521     D KPJBA         E DS
004000980521      *
004100010608     D OG143         E DS
004200010608      *
004300001006      *------------------------------------------------------------------------*
004400001006     C                   do        *hival
004500001009      * Pulizia 1° Videata
004600010601     C                   if        (*in28 = *off and *in90 = *off)
004700010604     C                   MOVE      *BLANKS       V1CFIL
004800010611     C                   MOVE      *BLANKS       V1DFIL
004900010611     C                   MOVE      '?'           V1CFIL
005000001009     C                   endif
005100001011
005200010608      * Emissione 1° Videata
005300010608     C                   exfmt     TNTB121
005400001006      * F3=Fine
005500001006     C   kc              leave
005600010405
005700001006      * Controlli 1° videata
005800010604     C                   SETOFF                                       28
005900001006     C                   exsr      Sr_Contr01
006000980623
006100010601     C   28              iter
006200001009     C   90              iter
006300001009
006400010608      * Caricamento e pulizia seconda videata
006500010608     C                   exsr      Sr_carvid
006600010608
006700010608      * Emissione 2° Videata
006800010608     C     riemetto      tag
006900010608     C                   exfmt     TNTB122
007000010611
007100010611     C                   eval      *in28 = *off
007200010608
007300010606      * F3=Fine
007400010606     C   kc              LEAVE
007500010606
007600010608      * F12=Ritorno
007700010608     C   kl              ITER
007800010608
007900010608     C                   exsr      Sr_Contr02
008000010608     C   28              goto      riemetto
008100010608
008200010608      * F6=Aggiornamento
008300010608     C   kf              exsr      Sr_Registra
008400010608     C   kf              iter
008500010608
008600010608     C                   goto      riemetto
008700010608
008800001006     C                   enddo
008900981204
009000001006     C                   eval      *inlr = *on
009100980521      **********************************************************************
009200980521      * CONTROLLI VIDEO 1
009300980521      **********************************************************************
009400001006     C     Sr_Contr01    begsr
009500001009      * Reset indicatori
009600010612     C                   setoff                                       90
009700010601
009800010601     C     '?'           scan      v1cfil                                 31
009900010601     C     *in31         ifeq      *on
010000010601     C                   eval      *in90 = *on
010100010601     C                   clear                   TIBS02DS
010200010601     C                   movel     'R'           t02mod
010300010601     C                   movel     knsif         t02sif
010400010608     C                   movel     'OES'         t02cod
010500010601     C                   call      'TIBS02R'
010600010601     C                   parm                    KPJBA
010700010601     C                   parm                    TIBS02DS
010800010601     C                   if        t02err =  *blanks
010900010601     C                   movel     t02ke1        v1cfil
011000010601     C                   SETON                                        90
011100010601     C                   ELSE
011200010612     C                   SETON                                        28
011300010612     C                   MOVEL     MSG(1)        V1CMSG
011400010601     C                   GOTO      ENDSRCONTR
011500010601     C                   endif
011600010604     C                   ENDIF
011700010601
011800010611      * controllo se campo valorizzato
011900010608     C                   IF        V1CFIL = *ZEROS OR V1CFIL = *BLANKS
012000010612     C                   MOVE      *BLANKS       V1DFIL
012100010612     C                   SETON                                        28        controllo P.O.emi.
012200010612     C                   MOVEL     MSG(1)        V1CMSG
012300010601     C                   GOTO      ENDSRCONTR                                   normale
012400010601     C                   ENDIF
012500010212
012600010611      * reperisco le descrizioni
012700010611     C                   MOVEL     V1CFIL        W3CAR             3 0
012800010611     C     W3CAR         CHAIN     AZORG01L                           31
012900010611     C                   IF        *IN31 = *OFF
013000010611     C                   MOVEL     ORGDES        V2DFIL
013100010611     C                   MOVEL     ORGDES        V1DFIL
013200010611     C                   ELSE
013300010611     C                   CLEAR                   V1DFIL
013400010611     C                   ENDIF
013500010611
013600010611      * controllo se P.O. gestibile
013700020207      * solo se sono in filiale
013800020207     c                   if        simfel <> 0
013900010611     C     V1CFIL        LOOKUP    SKDPD                                  31
014000010611     C                   IF        *IN31 = *OFF
014100010612     C                   SETON                                        28
014200010612     C                   MOVEL     MSG(2)        V1CMSG
014300010611     C                   GOTO      ENDSRCONTR
014400010611     C                   ENDIF
014500020207     c                   endif
014600010611
014700010611     C     ENDSRCONTR    endsr
014800010608      **********************************************************************
014900010608      * CONTROLLI VIDEO 2
015000010608      **********************************************************************
015100010608     C     Sr_Contr02    begsr
015200010608      * Reset indicatori
015300010608     C                   setoff                                       28
015400010608
015500010608     C                   if        (v2cdes = *blanks and v2cfax = *blanks) and
015600010608     C                             (v2cnom = *blanks)
015700010608      * se sono tutti vuoti
015800010612     C                   seton                                        28
015900010612     C                   MOVEL     MSG(3)        V2CMSG
016000010608     C                   goto      endsrcont2
016100010608     C                   else
016200010608     C                   if        (v2cdes <> *blanks and v2cfax <> *blanks) and
016300010608     C                             (v2cnom <> *blanks)
016400010608     C                   else
016500010608      * se non sono tutti pieni
016600010612     C                   seton                                        28
016700010612     C                   MOVEL     MSG(3)        V2CMSG
016800010608     C                   goto      endsrcont2
016900010608     C                   endif
017000010608     C                   endif
017100010608
017200010608     C     ENDSRCONT2    endsr
017300010608      **********************************************************************
017400010608      * CARICAMENTO SECONDA VIDEATA
017500010608      **********************************************************************
017600010608     C     Sr_Carvid     begsr
017700010608
017800010608     C                   movel     v1cfil        v2cfil
017900010608     C                   movel     *blanks       v2cdes
018000010608     C                   movel     *blanks       v2cfax
018100010608     C                   movel     *blanks       v2cnom
018200010608     C                   movel     v1cfil        kke1
018300010608     C                   movel     'OES'         kcod
018400010608     C     ktntbe        chain     tntbe01l                           31
018500010608     C                   if        *in31 = *off
018600010608     C                   movel     tbeuni        does
018700010608     C                   movel     d§oesdes      v2cdes
018800010608     C                   movel     d§oesfax      v2cfax
018900010608     C                   movel     d§oesnom      v2cnom
019000010608     C                   endif
019100010608
019200010608     C                   endsr
019300010608      **********************************************************************
019400010608      * REGISTRAZIONE DATI IMMESSI
019500010608      **********************************************************************
019600010608     C     Sr_Registra   begsr
019700010608
019800010608     C                   movel     'OES'         kcod
019900010608     C                   movel     v2cfil        kke1
020000010608     C     ktntbe        chain     tntbe01l                           31
020100010608     C                   if        *in31 = *on
020200010608     C                   movel     'OES'         tbecod
020300010608     C                   movel     v2cfil        tbeke1
020400010608     C                   movel     v2cfil        tbeflt
020500010608     C                   move      *blanks       tbeftt
020600010608     C                   move      *blanks       tbeftr
020700010608     C                   move      datoggi       tbedtr
020800010608     C                   movel     v2cdes        d§oesdes
020900010608     C                   movel     v2cfax        d§oesfax
021000010608     C                   movel     v2cnom        d§oesnom
021100010608     C                   movel     does          tbeuni
021200010608     C                   write     tntbe000
021300010608     C                   else
021400010608     C                   movel     v2cfil        tbeflt
021500010608     C                   move      *blanks       tbeftt
021600010608     C                   move      *blanks       tbeftr
021700010608     C                   move      datoggi       tbedtr
021800010608     C                   movel     v2cdes        d§oesdes
021900010608     C                   movel     v2cfax        d§oesfax
022000010608     C                   movel     v2cnom        d§oesnom
022100010608     C                   movel     does          tbeuni
022200010608     C                   update    tntbe000
022300010608     C                   endif
022400010608     C     ENDREGISTRA   endsr
022500980923      *****************************************************************
022600980923      * ROUTINE INIZIALE
022700980923      *****************************************************************
022800980923     C     *INZSR        BEGSR
022900980923      *
023000980521     C     *ENTRY        PLIST
023100980521     C                   PARM                    KPJBA
023200001030
023300980521     C                   Z-ADD     1             CODUT
023400980521     C                   CALL      'X§PARUT'
023500980521     C                   PARM                    UT§DSE0F
023600980521     C                   MOVEL     RAGUT         RSUT
023700980521     C                   MOVEL     REC80         CNCR80
023800001221
023900010608      * reperisco data e ora
024000010608     C                   TIME                    W0140
024100010608      * UDATE IN GGMMAAAA
024200010608     C                   MOVE      W0140         WDTGIO
024300010608      * UDATE IN AAAAMMGG
024400010608     C                   Z-ADD     WDTGIO        G02DAT
024500010608     C                   MOVEL     *BLANK        G02ERR
024600010608     C                   CALL      'XSRDA8'
024700010608     C                   PARM                    WLBDAT
024800010608     C                   MOVEL     G02INV        DATEU
024900010608     C                   MOVEL     DATEU         DAToggi
025000010608
025100010601     C     KTNTBE        KLIST
025200010604     C                   KFLD                    KCOD
025300010601     C                   KFLD                    KKE1
025400010510
025500010611      * Carico la schiera con tutti i punti operativi gestibili
025600010611     C                   Z-ADD     *ZEROS        X                 2 0
025700010611     C     SIMFEL        SETLL     AZORG05L
025800010628     C     SIMFEL        READE     AZORG05L
025900010628      *
026000010628     C                   DOW       Not %EOF(AZORG05L)
026100010611
026200010628     C                   IF        (ORGFAG = 'A' or ORGFAG = 'F') and
026300010628     C                             ORGFVA = *BLANKS
026400020207
026500020207     C                   MOVEL     ORGDE3        OG143
026600010628
026700020207     C                   IF        ORGFL1 = 'E' or §ogntw = 'DPD'
026800010628     C                   ADD       1             X
026900010628     C                   MOVEL     ORGFIL        WFIL
027000020207     c                   if        §ogntw = 'DPD'
027100020207     c                   movel     §ogdpt        wfil
027200020207     c                   endif
027300010628     C                   MOVEA     WFIL          SKDPD(X)
027400010611
027500020207     c**!!!              ELSE
027600010628
027700020207     C**!!!              MOVEL     ORGDE3        OG143
027800020207     C**!!!              IF        §OGDPD = 'S'
027900020207     C**!!!              ADD       1             X
028000020207     C**!!!              MOVEA     §OGDPT        SKDPD(X)
028100020207     C**!!!              ENDIF
028200010611
028300010611     C                   ENDIF
028400010628     C                   ENDIF
028500010611
028600010628     C     SIMFEL        READE     AZORG05L
028700010611     C                   ENDDO
028800010611
028900001006     C                   endsr
029000980521      *---------------------------------------------------------------------------------------------
029100980521** MSG  Lungh. 78                                                            *
029200010612Inserire Punto Operativo
029300010612Punto Operativo non gestibile
029400010612Inserire tutti i dati
