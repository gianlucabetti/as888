000100001006     H DECEDIT('0,') DATEDIT(*YMD.)
000200980521      ****************************************************************
000300980521      *  RIEPILOGO INDICATORI
000400981008      ****************************************************************
000500980521      * 28 - ERRORE GENERICO DSPF
000600010604      * 31 - GENERICO
000700980624      * 90 - riemissione videata
000800990317      ****************************************************************
000900010611     FAZORG01L  IF   E           K DISK    RENAME(AZORG:AZORG000)
001000010604     FTNTBE01L  UF A E           K DISK
001100160715     FTNTB15D   CF   E             WORKSTN
001200980521      *------------------------------------------------------------------------*
001300010604     D KCOD            S                   LIKE(TBECOD)
001400010604     D KKE1            S                   LIKE(TBEKE1)
001500060502     D WFIL            S              4
001600980521      *   S C H I E R E
001700160718     D MSG             S             78    DIM(8) CTDATA PERRCD(1)              MSG VIDEO
001800010608     D datoggi         S              8  0
001900010608
002000010608     D W0140           S             14  0
002100010608     D Wdtgio          S              8  0
002200010608     D dateu           S              8  0
002300001006
002400060502     D SKDPD           S              4    dim(50)
002500020603
002600020603     d xx              s              2  0
002700020603     d yy              s              2  0
002800010611
002900980521      *   D S   I N T E R N E / E S T E R N E
003000001009     D WLBDAT          DS                  INZ
003100001009     D  G02DAT                 1      8  0
003200001009     D  G02INV                 9     16  0
003300001009     D  G02ERR                17     17
003400001009     D  G02TGI                18     22  0
003500010308
003600001016     D TIBS02DS      E DS
003700980529     D UT§DSE0F      E DS
003800980601     D CNCR80        E DS
003900160715     D Dbon          E DS
004000160715     D Dtce          E DS
004100980521      *
004200980521     D KPJBA         E DS
004300010608      *
004400001006      *------------------------------------------------------------------------*
004500001006     C                   do        *hival
004600001009      * Pulizia 1° Videata
004700010601     C                   if        (*in28 = *off and *in90 = *off)
004800010604     C                   MOVE      *BLANKS       V1CFIL
004900010611     C                   MOVE      *BLANKS       V1DFIL
005000010611     C                   MOVE      '?'           V1CFIL
005100001009     C                   endif
005200001011
005300010608      * Emissione 1° Videata
005400160715     C                   exfmt     TNTB151
005500001006      * F3=Fine
005600001006     C   kc              leave
005700010405
005800001006      * Controlli 1° videata
005900010604     C                   SETOFF                                       28
006000001006     C                   exsr      Sr_Contr01
006100980623
006200010601     C   28              iter
006300001009     C   90              iter
006400001009
006500010608      * Caricamento e pulizia seconda videata
006600010608     C                   exsr      Sr_carvid
006700010608
006800010608      * Emissione 2° Videata
006900010608     C     riemetto      tag
007000160715     C                   exfmt     TNTB152
007100010611
007200010611     C                   eval      *in28 = *off
007300010608
007400010606      * F3=Fine
007500010606     C   kc              LEAVE
007600010606
007700010608      * F12=Ritorno
007800010608     C   kl              ITER
007900010608
008000010608     C                   exsr      Sr_Contr02
008100010608     C   28              goto      riemetto
008200010608
008300010608      * F6=Aggiornamento
008400010608     C   kf              exsr      Sr_Registra
008500010608     C   kf              iter
008600010608
008700010608     C                   goto      riemetto
008800010608
008900001006     C                   enddo
009000981204
009100001006     C                   eval      *inlr = *on
009200980521      **********************************************************************
009300980521      * CONTROLLI VIDEO 1
009400980521      **********************************************************************
009500001006     C     Sr_Contr01    begsr
009600001009      * Reset indicatori
009700010612     C                   setoff                                       90
009800010601
009900010601     C     '?'           scan      v1cfil                                 31
010000010601     C     *in31         ifeq      *on
010100010601     C                   eval      *in90 = *on
010200010601     C                   clear                   TIBS02DS
010300010601     C                   movel     'R'           t02mod
010400010601     C                   movel     knsif         t02sif
010500160715     C                   movel     'BON'         t02cod
010600010601     C                   call      'TIBS02R'
010700010601     C                   parm                    KPJBA
010800010601     C                   parm                    TIBS02DS
010900010601     C                   if        t02err =  *blanks
011000010601     C                   movel     t02ke1        v1cfil
011100010601     C                   SETON                                        90
011200010601     C                   ELSE
011300010612     C                   SETON                                        28
011400010612     C                   MOVEL     MSG(1)        V1CMSG
011500010601     C                   GOTO      ENDSRCONTR
011600010601     C                   endif
011700010604     C                   ENDIF
011800010601
011900010611      * controllo se campo valorizzato
012000010608     C                   IF        V1CFIL = *ZEROS OR V1CFIL = *BLANKS
012100010612     C                   MOVE      *BLANKS       V1DFIL
012200010612     C                   SETON                                        28        controllo P.O.emi.
012300010612     C                   MOVEL     MSG(1)        V1CMSG
012400010601     C                   GOTO      ENDSRCONTR                                   normale
012500010601     C                   ENDIF
012600010212
012700010611      * reperisco le descrizioni
012800060503     c* la descrizione la devo reperire solo se inserito numero con primo by
012900060503     c* te = 0
013000060503     C                   TESTN                   v1cfil               31
013100060503     C                   MOVE      v1cfil        W001A             1
013200060503    2C     *IN31         IFEQ      *On
013300060503     C     W001A         andge     '0'
013400060502     C                   MOVE      V1CFIL        W3CAR             3 0
013500010611     C     W3CAR         CHAIN     AZORG01L                           31
013600010611     C                   IF        *IN31 = *OFF
013700010611     C                   MOVEL     ORGDES        V2DFIL
013800010611     C                   MOVEL     ORGDES        V1DFIL
013900010611     C                   ELSE
014000010611     C                   CLEAR                   V1DFIL
014100060503     C                   CLEAR                   V2DFIL
014200010611     C                   ENDIF
014300060503     C                   ENDIF
014400010611
014500010611     C     ENDSRCONTR    endsr
014600010608      **********************************************************************
014700010608      * CONTROLLI VIDEO 2
014800010608      **********************************************************************
014900010608     C     Sr_Contr02    begsr
015000010608      * Reset indicatori
015100160715     C                   setoff                                       284041
015200160715     C                   setoff                                       4243
015300010608
015400160715      * ricerca causale
015500160715     C     '?'           scan      v2ccau                                 31
015600160715     C                   clear                   TIBS02DS
015700160715     C                   movel     'TCE'         t02cod
015800160715     C                   movel     knsif         t02sif
015900160715     C     *in31         ifeq      *on
016000160715     C                   movel     'R'           t02mod
016100160715     c                   else
016200160715     C                   movel     'C'           t02mod
016300160715     C                   movel(p)  v2ccau        t02ke1
016400160715     c                   endif
016500160715     C                   call      'TIBS02R'
016600160715     C                   parm                    KPJBA
016700160715     C                   parm                    TIBS02DS
016800160715     C                   if        t02err =  *blanks
016900160715     C                   movel     t02ke1        v2ccau
017000160715     c                   movel     t02uni        dtce
017100160715     c                   movel     §tcedes       v2ccaud
017200160715     C                   ELSE
017300160715     C                   clear                   v2ccau
017400160715     c                   clear                   dtce
017500160715     c                   clear                   v2ccaud
017600160715     C                   SETON                                        2840
017700160715     C                   MOVEL     MSG(6)        V1CMSG
017800160715     C                   GOTO      ENDSRCONT2
017900160715     C                   endif
018000160715      * centro di costo
018100160715     c                   if        v2ccdc <> 0
018200160715     c     v2ccdc        chain     azorg01l
018300160715     c                   if        %found(azorg01l)
018400160715     c                   movel     orgdes        v2ccdcd
018500160715     c                   else
018600160715     C                   SETON                                        2841
018700160715     c                   clear                   v2ccdcd
018800160715     C                   MOVEL     MSG(4)        V1CMSG
018900160715     C                   GOTO      ENDSRCONT2
019000160715     c                   endif
019100160715     c                   endif
019200160715      * data
019300160715     c                   if        v2cdat <> 0
019400160715     C                   Z-ADD     v2cdat        G02DAT
019500160715     C                   MOVEL     *BLANK        G02ERR
019600160715     C                   CALL      'XSRDA8'
019700160715     C                   PARM                    WLBDAT
019800160715     c                   if        g02err = '0'
019900160715     C                   MOVEL     G02INV        v2cdatg           8 0
020000160718     C                   MOVEL     G02dat        v2cdat
020100160718     c                   if        v2cdatins = 0
020200160718     c                   if        v2cdatg < datoggi
020300160718     C                   SETON                                        2842
020400160718     C                   MOVEL     MSG(7)        V1CMSG
020500160718     C                   GOTO      ENDSRCONT2
020600160718     c                   endif
020700160718     c                   else
020800160718     c                   if        v2cdatg < v2cdatins
020900160718     C                   SETON                                        2842
021000160718     C                   MOVEL     MSG(8)        V1CMSG
021100160718     C                   GOTO      ENDSRCONT2
021200160718     c                   endif
021300160718     c                   endif
021400160715     c                   else
021500160715     C                   SETON                                        2842
021600160715     C                   MOVEL     MSG(5)        V1CMSG
021700160715     C                   GOTO      ENDSRCONT2
021800160715     c                   endif
021900160715     c                   endif
022000160715      * se uno è vuoto
022100160715     c                   select
022200160715     C                   when      v2ccau = *blanks
022300160715     c                   seton                                        4028
022400160715     C                   when      v2cimp = 0
022500160715     C                   seton                                        4328
022600160715     C                   when      v2ccdc = 0
022700160715     C                   seton                                        4128
022800160715     C                   when      v2cdat = 0
022900160715     C                   seton                                        4228
023000160715     C                   endsl
023100160715     C   28              MOVEL     MSG(3)        V2CMSG
023200010608
023300010608     C     ENDSRCONT2    endsr
023400010608      **********************************************************************
023500010608      * CARICAMENTO SECONDA VIDEATA
023600010608      **********************************************************************
023700010608     C     Sr_Carvid     begsr
023800010608
023900010608     C                   movel     v1cfil        v2cfil
024000160715     C                   movel     *blanks       v2ccau
024100160715     C                   movel     *zeros        v2ccdc
024200160715     C                   movel     *zeros        v2cimp
024300160715     C                   movel     *zeros        v2cdat
024400160718     C                   movel     *zeros        v2cdatins         8 0
024500010608     C                   movel     v1cfil        kke1
024600160715     C                   movel     'BON'         kcod
024700160715     C     ktntbe        chain     tntbe01l
024800160715     C                   if        %found(tntbe01l)
024900160715     C                   movel     tbeuni        dbon
025000160715     C                   movel     §BONCAUS      v2ccau
025100160715     C                   z-add     §BONIMP       v2cimp
025200160715     C                   z-add     §BONFILD      v2ccdc
025300160715     C                   move      §BONDTI       g02inv
025400160715     C                   MOVEL     '3'           G02ERR
025500160715     C                   CALL      'XSRDA8'
025600160715     C                   PARM                    WLBDAT
025700160715     C                   move      g02dat        v2cdat
025800160718     C                   move      g02inv        v2cdatins
025900160715     c                   exsr      sr_contr02
026000010608     C                   endif
026100010608
026200010608     C                   endsr
026300010608      **********************************************************************
026400010608      * REGISTRAZIONE DATI IMMESSI
026500010608      **********************************************************************
026600010608     C     Sr_Registra   begsr
026700010608
026800160715     C                   movel     'BON'         kcod
026900010608     C                   movel     v2cfil        kke1
027000160715     C     ktntbe        chain     tntbe01l
027100160718     C                   if        not %found(tntbe01l)
027200160715     C                   movel     'BON'         tbecod
027300010608     C                   movel     v2cfil        tbeke1
027400060502     c                   clear                   tbeflt
027500010608     C                   move      *blanks       tbeftt
027600010608     C                   move      *blanks       tbeftr
027700010608     C                   move      datoggi       tbedtr
027800160715     C                   movel     v2ccau        §BONCAUS
027900160715     C                   movel     v2cimp        §BONIMP
028000160715     C                   movel     v2ccdc        §BONFILD
028100160715     C                   movel     v2cdatg       §bondti
028200160715     C                   movel     dbon          tbeuni
028300010608     C                   write     tntbe000
028400010608     C                   else
028500060502     C                   clear                   tbeflt
028600010608     C                   move      *blanks       tbeftt
028700010608     C                   move      *blanks       tbeftr
028800010608     C                   move      datoggi       tbedtr
028900160715     C                   movel     v2ccau        §BONCAUS
029000160715     C                   movel     v2cimp        §BONIMP
029100160715     C                   movel     v2ccdc        §BONFILD
029200160715     C                   movel     v2cdatg       §bondti
029300160715     C                   movel     dbon          tbeuni
029400010608     C                   update    tntbe000
029500010608     C                   endif
029600010608     C     ENDREGISTRA   endsr
029700980923      *****************************************************************
029800980923      * ROUTINE INIZIALE
029900980923      *****************************************************************
030000980923     C     *INZSR        BEGSR
030100980923      *
030200980521     C     *ENTRY        PLIST
030300980521     C                   PARM                    KPJBA
030400001030
030500980521     C                   Z-ADD     1             CODUT
030600980521     C                   CALL      'X§PARUT'
030700980521     C                   PARM                    UT§DSE0F
030800980521     C                   MOVEL     RAGUT         RSUT
030900980521     C                   MOVEL     REC80         CNCR80
031000001221
031100010608      * reperisco data e ora
031200010608     C                   TIME                    W0140
031300010608      * UDATE IN GGMMAAAA
031400010608     C                   MOVE      W0140         WDTGIO
031500010608      * UDATE IN AAAAMMGG
031600010608     C                   Z-ADD     WDTGIO        G02DAT
031700010608     C                   MOVEL     *BLANK        G02ERR
031800010608     C                   CALL      'XSRDA8'
031900010608     C                   PARM                    WLBDAT
032000010608     C                   MOVEL     G02INV        DATEU
032100010608     C                   MOVEL     DATEU         DAToggi
032200010608
032300010601     C     KTNTBE        KLIST
032400010604     C                   KFLD                    KCOD
032500010601     C                   KFLD                    KKE1
032600010611
032700001006     C                   endsr
032800980521      *---------------------------------------------------------------------------------------------
032900980521** MSG  Lungh. 78                                                            *
033000010612Inserire Punto Operativo
033100010612Punto Operativo non gestibile
033200010612Inserire tutti i dati
033300160715Centro di costo inesistente
033400160715Data errata
033500160715Causale errata
033600160718La Data deve essere superiore alla Odierna in inserimento
033700160718La Data in modifica non può essere inferiore a quella di inserimento
