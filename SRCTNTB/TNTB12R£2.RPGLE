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
013200010611     C                   MOVEL     V1CFIL        W3CAR             3 0
013300010611     C     W3CAR         CHAIN     AZORG01L                           31
013400010611     C                   IF        *IN31 = *OFF
013500010611     C                   MOVEL     ORGDES        V2DFIL
013600010611     C                   MOVEL     ORGDES        V1DFIL
013700010611     C                   ELSE
013800010611     C                   CLEAR                   V1DFIL
013900010611     C                   ENDIF
014000010611
014100010611      * controllo se P.O. gestibile
014200020207      * solo se sono in filiale
014300020207     c                   if        simfel <> 0
014400010611     C     V1CFIL        LOOKUP    SKDPD                                  31
014500010611     C                   IF        *IN31 = *OFF
014600010612     C                   SETON                                        28
014700010612     C                   MOVEL     MSG(2)        V1CMSG
014800010611     C                   GOTO      ENDSRCONTR
014900010611     C                   ENDIF
015000020207     c                   endif
015100010611
015200010611     C     ENDSRCONTR    endsr
015300010608      **********************************************************************
015400010608      * CONTROLLI VIDEO 2
015500010608      **********************************************************************
015600010608     C     Sr_Contr02    begsr
015700010608      * Reset indicatori
015800010608     C                   setoff                                       28
015900010608
016000010608     C                   if        (v2cdes = *blanks and v2cfax = *blanks) and
016100010608     C                             (v2cnom = *blanks)
016200010608      * se sono tutti vuoti
016300010612     C                   seton                                        28
016400010612     C                   MOVEL     MSG(3)        V2CMSG
016500010608     C                   goto      endsrcont2
016600010608     C                   else
016700010608     C                   if        (v2cdes <> *blanks and v2cfax <> *blanks) and
016800010608     C                             (v2cnom <> *blanks)
016900010608     C                   else
017000010608      * se non sono tutti pieni
017100010612     C                   seton                                        28
017200010612     C                   MOVEL     MSG(3)        V2CMSG
017300010608     C                   goto      endsrcont2
017400010608     C                   endif
017500010608     C                   endif
017600010608
017700010608     C     ENDSRCONT2    endsr
017800010608      **********************************************************************
017900010608      * CARICAMENTO SECONDA VIDEATA
018000010608      **********************************************************************
018100010608     C     Sr_Carvid     begsr
018200010608
018300010608     C                   movel     v1cfil        v2cfil
018400010608     C                   movel     *blanks       v2cdes
018500010608     C                   movel     *blanks       v2cfax
018600010608     C                   movel     *blanks       v2cnom
018700010608     C                   movel     v1cfil        kke1
018800010608     C                   movel     'OES'         kcod
018900010608     C     ktntbe        chain     tntbe01l                           31
019000010608     C                   if        *in31 = *off
019100010608     C                   movel     tbeuni        does
019200010608     C                   movel     d§oesdes      v2cdes
019300010608     C                   movel     d§oesfax      v2cfax
019400010608     C                   movel     d§oesnom      v2cnom
019500010608     C                   endif
019600010608
019700010608     C                   endsr
019800010608      **********************************************************************
019900010608      * REGISTRAZIONE DATI IMMESSI
020000010608      **********************************************************************
020100010608     C     Sr_Registra   begsr
020200010608
020300010608     C                   movel     'OES'         kcod
020400010608     C                   movel     v2cfil        kke1
020500010608     C     ktntbe        chain     tntbe01l                           31
020600010608     C                   if        *in31 = *on
020700010608     C                   movel     'OES'         tbecod
020800010608     C                   movel     v2cfil        tbeke1
020900010608     C                   movel     v2cfil        tbeflt
021000010608     C                   move      *blanks       tbeftt
021100010608     C                   move      *blanks       tbeftr
021200010608     C                   move      datoggi       tbedtr
021300010608     C                   movel     v2cdes        d§oesdes
021400010608     C                   movel     v2cfax        d§oesfax
021500010608     C                   movel     v2cnom        d§oesnom
021600010608     C                   movel     does          tbeuni
021700010608     C                   write     tntbe000
021800010608     C                   else
021900010608     C                   movel     v2cfil        tbeflt
022000010608     C                   move      *blanks       tbeftt
022100010608     C                   move      *blanks       tbeftr
022200010608     C                   move      datoggi       tbedtr
022300010608     C                   movel     v2cdes        d§oesdes
022400010608     C                   movel     v2cfax        d§oesfax
022500010608     C                   movel     v2cnom        d§oesnom
022600010608     C                   movel     does          tbeuni
022700010608     C                   update    tntbe000
022800010608     C                   endif
022900010608     C     ENDREGISTRA   endsr
023000980923      *****************************************************************
023100980923      * ROUTINE INIZIALE
023200980923      *****************************************************************
023300980923     C     *INZSR        BEGSR
023400980923      *
023500980521     C     *ENTRY        PLIST
023600980521     C                   PARM                    KPJBA
023700001030
023800980521     C                   Z-ADD     1             CODUT
023900980521     C                   CALL      'X§PARUT'
024000980521     C                   PARM                    UT§DSE0F
024100980521     C                   MOVEL     RAGUT         RSUT
024200980521     C                   MOVEL     REC80         CNCR80
024300001221
024400010608      * reperisco data e ora
024500010608     C                   TIME                    W0140
024600010608      * UDATE IN GGMMAAAA
024700010608     C                   MOVE      W0140         WDTGIO
024800010608      * UDATE IN AAAAMMGG
024900010608     C                   Z-ADD     WDTGIO        G02DAT
025000010608     C                   MOVEL     *BLANK        G02ERR
025100010608     C                   CALL      'XSRDA8'
025200010608     C                   PARM                    WLBDAT
025300010608     C                   MOVEL     G02INV        DATEU
025400010608     C                   MOVEL     DATEU         DAToggi
025500010608
025600010601     C     KTNTBE        KLIST
025700010604     C                   KFLD                    KCOD
025800010601     C                   KFLD                    KKE1
025900010510
026000020603     c                   clear                   trul06ds
026100020603     c                   eval      d06tla = 'L'
026200020603     c                   eval      d06cod = '£1'
026300020603     c                   movel     simfel        d06key
026400020603     c                   movel(p)  trul06ds      kpjbu
026500020603     c                   call      'TRUL06R'
026600020603     c                   parm                    kpjba
026700020603     c                   movel     kpjbu         trul06ds
026800020603
026900010611      * Carico la schiera con tutti i punti operativi gestibili
027000020603     C                   Z-ADD     *ZEROS        yy
027100020603     C**!!!SIMFEL        SETLL     AZORG05L
027200020603     C**!!!SIMFEL        READE     AZORG05L
027300010628      *
027400020603     C**!!!              DOW       Not %EOF(AZORG05L)
027500020603 1b  c                   do        30            xx
027600020603     c                   if        lin(xx) = *zeros
027700020603     c                   leave
027800020603     c                   endif
027900020603     c     lin(xx)       chain     azorg01l
028000020603 2b  c                   if        %found(azorg01l) and
028100020603     C                             (ORGFAG = 'A' or ORGFAG = 'F') and
028200010628     C                             ORGFVA = *BLANKS
028300020207
028400020207     C                   MOVEL     ORGDE3        OG143
028500010628
028600020603     C**!!!              IF        ORGFL1 = 'E' or §ogntw = 'DPD'
028700020603 3b  C                   IF        §ogntw = 'DPD' or §ogntw = 'EEX' or
028800020603     C                             §ogntw = 'FED' or §ogntw = 'EUP'
028900020603     C                   ADD       1             yy
029000010628     C                   MOVEL     ORGFIL        WFIL
029100020207     c                   if        §ogntw = 'DPD'
029200020207     c                   movel     §ogdpt        wfil
029300020207     c                   endif
029400020603     C                   MOVEA     WFIL          SKDPD(yy)
029500010611
029600020603 3e  C                   ENDIF
029700020603 2e  C                   ENDIF
029800010611
029900020603     C**!!!SIMFEL        READE     AZORG05L
030000020603 1e  C                   ENDDO
030100010611
030200001006     C                   endsr
030300980521      *---------------------------------------------------------------------------------------------
030400980521** MSG  Lungh. 78                                                            *
030500010612Inserire Punto Operativo
030600010612Punto Operativo non gestibile
030700010612Inserire tutti i dati
