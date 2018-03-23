000100001006     H DECEDIT('0,') DATEDIT(*YMD.)
000200980521      ****************************************************************
000300980521      *  RIEPILOGO INDICATORI
000400981008      ****************************************************************
000500980521      * 28 - ERRORE GENERICO DSPF
000600010604      * 31 - GENERICO
000700980624      * 90 - riemissione videata
000800990317      ****************************************************************
000900020603     F**!!!AZORG05L  IF   E           K DISK
001000010611     FAZORG01L  IF   E           K DISK    RENAME(AZORG:AZORG000)
001100010604     FTNTBE01L  UF A E           K DISK
001200010608     FTNTB12D   CF   E             WORKSTN
001300980521      *------------------------------------------------------------------------*
001400010604     D KCOD            S                   LIKE(TBECOD)
001500010604     D KKE1            S                   LIKE(TBEKE1)
001600060502     D WFIL            S              4
001700980521      *   S C H I E R E
001800010612     D MSG             S             78    DIM(3) CTDATA PERRCD(1)              MSG VIDEO
001900010608     D datoggi         S              8  0
002000010608
002100010608     D W0140           S             14  0
002200010608     D Wdtgio          S              8  0
002300010608     D dateu           S              8  0
002400001006
002500060502     D SKDPD           S              4    dim(50)
002600020603
002700020603     d xx              s              2  0
002800020603     d yy              s              2  0
002900010611
003000980521      *   D S   I N T E R N E / E S T E R N E
003100001009     D WLBDAT          DS                  INZ
003200001009     D  G02DAT                 1      8  0
003300001009     D  G02INV                 9     16  0
003400001009     D  G02ERR                17     17
003500001009     D  G02TGI                18     22  0
003600010308
003700001016     D TIBS02DS      E DS
003800980529     D UT§DSE0F      E DS
003900980601     D CNCR80        E DS
004000010608     D DOES          E DS
004100020603     d trul06ds      e ds
004200020603     d  lin                    1     90  0 dim(30)
004300980521      *
004400980521     D KPJBA         E DS
004500980521      *
004600010608     D OG143         E DS
004700010608      *
004800001006      *------------------------------------------------------------------------*
004900001006     C                   do        *hival
005000001009      * Pulizia 1° Videata
005100010601     C                   if        (*in28 = *off and *in90 = *off)
005200010604     C                   MOVE      *BLANKS       V1CFIL
005300010611     C                   MOVE      *BLANKS       V1DFIL
005400010611     C                   MOVE      '?'           V1CFIL
005500001009     C                   endif
005600001011
005700010608      * Emissione 1° Videata
005800010608     C                   exfmt     TNTB121
005900001006      * F3=Fine
006000001006     C   kc              leave
006100010405
006200001006      * Controlli 1° videata
006300010604     C                   SETOFF                                       28
006400001006     C                   exsr      Sr_Contr01
006500980623
006600010601     C   28              iter
006700001009     C   90              iter
006800001009
006900010608      * Caricamento e pulizia seconda videata
007000010608     C                   exsr      Sr_carvid
007100010608
007200010608      * Emissione 2° Videata
007300010608     C     riemetto      tag
007400010608     C                   exfmt     TNTB122
007500010611
007600010611     C                   eval      *in28 = *off
007700010608
007800010606      * F3=Fine
007900010606     C   kc              LEAVE
008000010606
008100010608      * F12=Ritorno
008200010608     C   kl              ITER
008300010608
008400010608     C                   exsr      Sr_Contr02
008500010608     C   28              goto      riemetto
008600010608
008700010608      * F6=Aggiornamento
008800010608     C   kf              exsr      Sr_Registra
008900010608     C   kf              iter
009000010608
009100010608     C                   goto      riemetto
009200010608
009300001006     C                   enddo
009400981204
009500001006     C                   eval      *inlr = *on
009600980521      **********************************************************************
009700980521      * CONTROLLI VIDEO 1
009800980521      **********************************************************************
009900001006     C     Sr_Contr01    begsr
010000001009      * Reset indicatori
010100010612     C                   setoff                                       90
010200010601
010300010601     C     '?'           scan      v1cfil                                 31
010400010601     C     *in31         ifeq      *on
010500010601     C                   eval      *in90 = *on
010600010601     C                   clear                   TIBS02DS
010700010601     C                   movel     'R'           t02mod
010800010601     C                   movel     knsif         t02sif
010900010608     C                   movel     'OES'         t02cod
011000010601     C                   call      'TIBS02R'
011100010601     C                   parm                    KPJBA
011200010601     C                   parm                    TIBS02DS
011300010601     C                   if        t02err =  *blanks
011400010601     C                   movel     t02ke1        v1cfil
011500010601     C                   SETON                                        90
011600010601     C                   ELSE
011700010612     C                   SETON                                        28
011800010612     C                   MOVEL     MSG(1)        V1CMSG
011900010601     C                   GOTO      ENDSRCONTR
012000010601     C                   endif
012100010604     C                   ENDIF
012200010601
012300010611      * controllo se campo valorizzato
012400010608     C                   IF        V1CFIL = *ZEROS OR V1CFIL = *BLANKS
012500010612     C                   MOVE      *BLANKS       V1DFIL
012600010612     C                   SETON                                        28        controllo P.O.emi.
012700010612     C                   MOVEL     MSG(1)        V1CMSG
012800010601     C                   GOTO      ENDSRCONTR                                   normale
012900010601     C                   ENDIF
013000010212
013100010611      * reperisco le descrizioni
013200060503     c* la descrizione la devo reperire solo se inserito numero con primo by
013300060503     c* te = 0
013400060503     C                   TESTN                   v1cfil               31
013500060503     C                   MOVE      v1cfil        W001A             1
013600060503    2C     *IN31         IFEQ      *On
013700060503     C     W001A         andge     '0'
013800060502     C                   MOVE      V1CFIL        W3CAR             3 0
013900010611     C     W3CAR         CHAIN     AZORG01L                           31
014000010611     C                   IF        *IN31 = *OFF
014100010611     C                   MOVEL     ORGDES        V2DFIL
014200010611     C                   MOVEL     ORGDES        V1DFIL
014300010611     C                   ELSE
014400010611     C                   CLEAR                   V1DFIL
014500060503     C                   CLEAR                   V2DFIL
014600010611     C                   ENDIF
014700060503     C                   ENDIF
014800010611
014900010611      * controllo se P.O. gestibile
015000020207      * solo se sono in filiale
015100020207     c                   if        simfel <> 0
015200010611     C     V1CFIL        LOOKUP    SKDPD                                  31
015300010611     C                   IF        *IN31 = *OFF
015400010612     C                   SETON                                        28
015500010612     C                   MOVEL     MSG(2)        V1CMSG
015600010611     C                   GOTO      ENDSRCONTR
015700010611     C                   ENDIF
015800020207     c                   endif
015900010611
016000010611     C     ENDSRCONTR    endsr
016100010608      **********************************************************************
016200010608      * CONTROLLI VIDEO 2
016300010608      **********************************************************************
016400010608     C     Sr_Contr02    begsr
016500010608      * Reset indicatori
016600010608     C                   setoff                                       28
016700010608
016800010608     C                   if        (v2cdes = *blanks and v2cfax = *blanks) and
016900010608     C                             (v2cnom = *blanks)
017000010608      * se sono tutti vuoti
017100010612     C                   seton                                        28
017200010612     C                   MOVEL     MSG(3)        V2CMSG
017300010608     C                   goto      endsrcont2
017400010608     C                   else
017500010608     C                   if        (v2cdes <> *blanks and v2cfax <> *blanks) and
017600010608     C                             (v2cnom <> *blanks)
017700010608     C                   else
017800010608      * se non sono tutti pieni
017900010612     C                   seton                                        28
018000010612     C                   MOVEL     MSG(3)        V2CMSG
018100010608     C                   goto      endsrcont2
018200010608     C                   endif
018300010608     C                   endif
018400010608
018500010608     C     ENDSRCONT2    endsr
018600010608      **********************************************************************
018700010608      * CARICAMENTO SECONDA VIDEATA
018800010608      **********************************************************************
018900010608     C     Sr_Carvid     begsr
019000010608
019100010608     C                   movel     v1cfil        v2cfil
019200010608     C                   movel     *blanks       v2cdes
019300010608     C                   movel     *blanks       v2cfax
019400010608     C                   movel     *blanks       v2cnom
019500010608     C                   movel     v1cfil        kke1
019600010608     C                   movel     'OES'         kcod
019700010608     C     ktntbe        chain     tntbe01l                           31
019800010608     C                   if        *in31 = *off
019900010608     C                   movel     tbeuni        does
020000010608     C                   movel     d§oesdes      v2cdes
020100010608     C                   movel     d§oesfax      v2cfax
020200010608     C                   movel     d§oesnom      v2cnom
020300010608     C                   endif
020400010608
020500010608     C                   endsr
020600010608      **********************************************************************
020700010608      * REGISTRAZIONE DATI IMMESSI
020800010608      **********************************************************************
020900010608     C     Sr_Registra   begsr
021000010608
021100010608     C                   movel     'OES'         kcod
021200010608     C                   movel     v2cfil        kke1
021300010608     C     ktntbe        chain     tntbe01l                           31
021400010608     C                   if        *in31 = *on
021500010608     C                   movel     'OES'         tbecod
021600010608     C                   movel     v2cfil        tbeke1
021700060502     C***                movel     v2cfil        tbeflt
021800060502     c                   clear                   tbeflt
021900010608     C                   move      *blanks       tbeftt
022000010608     C                   move      *blanks       tbeftr
022100010608     C                   move      datoggi       tbedtr
022200010608     C                   movel     v2cdes        d§oesdes
022300010608     C                   movel     v2cfax        d§oesfax
022400010608     C                   movel     v2cnom        d§oesnom
022500010608     C                   movel     does          tbeuni
022600010608     C                   write     tntbe000
022700010608     C                   else
022800060502     C***                movel     v2cfil        tbeflt
022900060502     C                   clear                   tbeflt
023000010608     C                   move      *blanks       tbeftt
023100010608     C                   move      *blanks       tbeftr
023200010608     C                   move      datoggi       tbedtr
023300010608     C                   movel     v2cdes        d§oesdes
023400010608     C                   movel     v2cfax        d§oesfax
023500010608     C                   movel     v2cnom        d§oesnom
023600010608     C                   movel     does          tbeuni
023700010608     C                   update    tntbe000
023800010608     C                   endif
023900010608     C     ENDREGISTRA   endsr
024000980923      *****************************************************************
024100980923      * ROUTINE INIZIALE
024200980923      *****************************************************************
024300980923     C     *INZSR        BEGSR
024400980923      *
024500980521     C     *ENTRY        PLIST
024600980521     C                   PARM                    KPJBA
024700001030
024800980521     C                   Z-ADD     1             CODUT
024900980521     C                   CALL      'X§PARUT'
025000980521     C                   PARM                    UT§DSE0F
025100980521     C                   MOVEL     RAGUT         RSUT
025200980521     C                   MOVEL     REC80         CNCR80
025300001221
025400010608      * reperisco data e ora
025500010608     C                   TIME                    W0140
025600010608      * UDATE IN GGMMAAAA
025700010608     C                   MOVE      W0140         WDTGIO
025800010608      * UDATE IN AAAAMMGG
025900010608     C                   Z-ADD     WDTGIO        G02DAT
026000010608     C                   MOVEL     *BLANK        G02ERR
026100010608     C                   CALL      'XSRDA8'
026200010608     C                   PARM                    WLBDAT
026300010608     C                   MOVEL     G02INV        DATEU
026400010608     C                   MOVEL     DATEU         DAToggi
026500010608
026600010601     C     KTNTBE        KLIST
026700010604     C                   KFLD                    KCOD
026800010601     C                   KFLD                    KKE1
026900010510
027000020603     c                   clear                   trul06ds
027100020603     c                   eval      d06tla = 'L'
027200020603     c                   eval      d06cod = '£1'
027300020603     c                   movel     simfel        d06key
027400020603     c                   movel(p)  trul06ds      kpjbu
027500020603     c                   call      'TRUL06R'
027600020603     c                   parm                    kpjba
027700020603     c                   movel     kpjbu         trul06ds
027800020603
027900010611      * Carico la schiera con tutti i punti operativi gestibili
028000020603     C                   Z-ADD     *ZEROS        yy
028100020603     C**!!!SIMFEL        SETLL     AZORG05L
028200020603     C**!!!SIMFEL        READE     AZORG05L
028300010628      *
028400020603     C**!!!              DOW       Not %EOF(AZORG05L)
028500020603 1b  c                   do        30            xx
028600020603     c                   if        lin(xx) = *zeros
028700020603     c                   leave
028800020603     c                   endif
028900020603     c     lin(xx)       chain     azorg01l
029000020603 2b  c                   if        %found(azorg01l) and
029100020603     C                             (ORGFAG = 'A' or ORGFAG = 'F') and
029200010628     C                             ORGFVA = *BLANKS
029300020207
029400020207     C                   MOVEL     ORGDE3        OG143
029500010628
029600020603     C**!!!              IF        ORGFL1 = 'E' or §ogntw = 'DPD'
029700020603 3b  C                   IF        §ogntw = 'DPD' or §ogntw = 'EEX' or
029800020603     C                             §ogntw = 'FED' or §ogntw = 'EUP'
029900020603     C                   ADD       1             yy
030000060502     C                   MOVE      ORGFIL        WFIL
030100020207     c                   if        §ogntw = 'DPD'
030200060502     c                   movel     §ogdp1        wfil
030300020207     c                   endif
030400020603     C                   MOVEA     WFIL          SKDPD(yy)
030500010611
030600020603 3e  C                   ENDIF
030700020603 2e  C                   ENDIF
030800010611
030900020603     C**!!!SIMFEL        READE     AZORG05L
031000020603 1e  C                   ENDDO
031100010611
031200001006     C                   endsr
031300980521      *---------------------------------------------------------------------------------------------
031400980521** MSG  Lungh. 78                                                            *
031500010612Inserire Punto Operativo
031600010612Punto Operativo non gestibile
031700010612Inserire tutti i dati
