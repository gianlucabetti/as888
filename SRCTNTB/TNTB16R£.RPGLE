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
001100161011     FTNTB16D   CF   E             WORKSTN
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
003900161011     D Dmga          E DS
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
005400161011     C                   exfmt     TNTB161
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
007000161011     C                   exfmt     TNTB162
007100010611
007200010611     C                   eval      *in28 = *off
007300010608
007400010606      * F3=Fine
007500010606     C   kc              LEAVE
007600010606
007700010608      * F12=Ritorno
007800160915     C   kl              clear                   v2ccdcd
007900160915     c   kl              clear                   v2ccaud
008000010608     C   kl              ITER
008100010608
008200010608     C                   exsr      Sr_Contr02
008300010608     C   28              goto      riemetto
008400010608
008500010608      * F6=Aggiornamento
008600010608     C   kf              exsr      Sr_Registra
008700010608     C   kf              iter
008800010608
008900010608     C                   goto      riemetto
009000010608
009100001006     C                   enddo
009200981204
009300001006     C                   eval      *inlr = *on
009400980521      **********************************************************************
009500980521      * CONTROLLI VIDEO 1
009600980521      **********************************************************************
009700001006     C     Sr_Contr01    begsr
009800001009      * Reset indicatori
009900010612     C                   setoff                                       90
010000010601
010100010601     C     '?'           scan      v1cfil                                 31
010200010601     C     *in31         ifeq      *on
010300010601     C                   eval      *in90 = *on
010400010601     C                   clear                   TIBS02DS
010500010601     C                   movel     'R'           t02mod
010600010601     C                   movel     knsif         t02sif
010700161011     C                   movel     'MGA'         t02cod
010800010601     C                   call      'TIBS02R'
010900010601     C                   parm                    KPJBA
011000010601     C                   parm                    TIBS02DS
011100010601     C                   if        t02err =  *blanks
011200010601     C                   movel     t02ke1        v1cfil
011300010601     C                   SETON                                        90
011400010601     C                   ELSE
011500010612     C                   SETON                                        28
011600010612     C                   MOVEL     MSG(1)        V1CMSG
011700010601     C                   GOTO      ENDSRCONTR
011800010601     C                   endif
011900010604     C                   ENDIF
012000010601
012100010611      * controllo se campo valorizzato
012200010608     C                   IF        V1CFIL = *ZEROS OR V1CFIL = *BLANKS
012300010612     C                   MOVE      *BLANKS       V1DFIL
012400010612     C                   SETON                                        28        controllo P.O.emi.
012500010612     C                   MOVEL     MSG(1)        V1CMSG
012600010601     C                   GOTO      ENDSRCONTR                                   normale
012700010601     C                   ENDIF
012800010212
012900010611      * reperisco le descrizioni
013000060503     c* la descrizione la devo reperire solo se inserito numero con primo by
013100060503     c* te = 0
013200060503     C                   TESTN                   v1cfil               31
013300060503     C                   MOVE      v1cfil        W001A             1
013400060503    2C     *IN31         IFEQ      *On
013500060503     C     W001A         andge     '0'
013600060502     C                   MOVE      V1CFIL        W3CAR             3 0
013700010611     C     W3CAR         CHAIN     AZORG01L                           31
013800010611     C                   IF        *IN31 = *OFF
013900010611     C                   MOVEL     ORGDES        V2DFIL
014000010611     C                   MOVEL     ORGDES        V1DFIL
014100010611     C                   ELSE
014200010611     C                   CLEAR                   V1DFIL
014300060503     C                   CLEAR                   V2DFIL
014400010611     C                   ENDIF
014500060503     C                   ENDIF
014600010611
014700010611     C     ENDSRCONTR    endsr
014800010608      **********************************************************************
014900010608      * CONTROLLI VIDEO 2
015000010608      **********************************************************************
015100010608     C     Sr_Contr02    begsr
015200010608      * Reset indicatori
015300160715     C                   setoff                                       284041
015400160715     C                   setoff                                       4243
015500010608
015600160715      * ricerca causale
015700160715     C     '?'           scan      v2ccau                                 31
015800160715     C                   clear                   TIBS02DS
015900160715     C                   movel     'TCE'         t02cod
016000160715     C                   movel     knsif         t02sif
016100160715     C     *in31         ifeq      *on
016200160715     C                   movel     'R'           t02mod
016300160715     c                   else
016400160715     C                   movel     'C'           t02mod
016500160715     C                   movel(p)  v2ccau        t02ke1
016600160715     c                   endif
016700160715     C                   call      'TIBS02R'
016800160715     C                   parm                    KPJBA
016900160715     C                   parm                    TIBS02DS
017000160715     C                   if        t02err =  *blanks
017100160715     C                   movel     t02ke1        v2ccau
017200160715     c                   movel     t02uni        dtce
017300160715     c                   movel     §tcedes       v2ccaud
017400160715     C                   ELSE
017500160715     C                   clear                   v2ccau
017600160715     c                   clear                   dtce
017700160715     c                   clear                   v2ccaud
017800160715     C                   SETON                                        2840
017900160715     C                   MOVEL     MSG(6)        V1CMSG
018000160715     C                   GOTO      ENDSRCONT2
018100160715     C                   endif
018200160715      * centro di costo
018300160715     c                   if        v2ccdc <> 0
018400160715     c     v2ccdc        chain     azorg01l
018500160715     c                   if        %found(azorg01l)
018600160715     c                   movel     orgdes        v2ccdcd
018700160715     c                   else
018800160715     C                   SETON                                        2841
018900160715     c                   clear                   v2ccdcd
019000160715     C                   MOVEL     MSG(4)        V1CMSG
019100160715     C                   GOTO      ENDSRCONT2
019200160715     c                   endif
019300160715     c                   endif
019400160715      * data
019500160715     c                   if        v2cdat <> 0
019600160715     C                   Z-ADD     v2cdat        G02DAT
019700160715     C                   MOVEL     *BLANK        G02ERR
019800160715     C                   CALL      'XSRDA8'
019900160715     C                   PARM                    WLBDAT
020000160715     c                   if        g02err = '0'
020100160715     C                   MOVEL     G02INV        v2cdatg           8 0
020200160718     C                   MOVEL     G02dat        v2cdat
020300160718     c                   if        v2cdatins = 0
020400160718     c                   if        v2cdatg < datoggi
020500160718     C                   SETON                                        2842
020600160718     C                   MOVEL     MSG(7)        V1CMSG
020700160718     C                   GOTO      ENDSRCONT2
020800160718     c                   endif
020900160718     c                   else
021000160718     c                   if        v2cdatg < v2cdatins
021100160718     C                   SETON                                        2842
021200160718     C                   MOVEL     MSG(8)        V1CMSG
021300160718     C                   GOTO      ENDSRCONT2
021400160718     c                   endif
021500160718     c                   endif
021600160715     c                   else
021700160715     C                   SETON                                        2842
021800160715     C                   MOVEL     MSG(5)        V1CMSG
021900160715     C                   GOTO      ENDSRCONT2
022000160715     c                   endif
022100160715     c                   endif
022200160715      * se uno è vuoto
022300160715     c                   select
022400160715     C                   when      v2ccau = *blanks
022500160715     c                   seton                                        4028
022600160715     C                   when      v2cimp = 0
022700160715     C                   seton                                        4328
022800160715     C                   when      v2ccdc = 0
022900160715     C                   seton                                        4128
023000160715     C                   when      v2cdat = 0
023100160715     C                   seton                                        4228
023200160715     C                   endsl
023300160715     C   28              MOVEL     MSG(3)        V2CMSG
023400010608
023500010608     C     ENDSRCONT2    endsr
023600010608      **********************************************************************
023700010608      * CARICAMENTO SECONDA VIDEATA
023800010608      **********************************************************************
023900010608     C     Sr_Carvid     begsr
024000010608
024100010608     C                   movel     v1cfil        v2cfil
024200160715     C                   movel     *blanks       v2ccau
024300160715     C                   movel     *zeros        v2ccdc
024400160715     C                   movel     *zeros        v2cimp
024500160715     C                   movel     *zeros        v2cdat
024600160718     C                   movel     *zeros        v2cdatins         8 0
024700010608     C                   movel     v1cfil        kke1
024800161011     C                   movel     'MGA'         kcod
024900160715     C     ktntbe        chain     tntbe01l
025000160715     C                   if        %found(tntbe01l)
025100161011     C                   movel     tbeuni        dmga
025200161011     C                   movel     §mgaCAUS      v2ccau
025300161011     C                   z-add     §mgaIMP       v2cimp
025400161011     C                   z-add     §mgaFILD      v2ccdc
025500161011     C                   move      §mgaDTI       g02inv
025600160715     C                   MOVEL     '3'           G02ERR
025700160715     C                   CALL      'XSRDA8'
025800160715     C                   PARM                    WLBDAT
025900160715     C                   move      g02dat        v2cdat
026000160718     C                   move      g02inv        v2cdatins
026100160715     c                   exsr      sr_contr02
026200010608     C                   endif
026300010608
026400010608     C                   endsr
026500010608      **********************************************************************
026600010608      * REGISTRAZIONE DATI IMMESSI
026700010608      **********************************************************************
026800010608     C     Sr_Registra   begsr
026900010608
027000161011     C                   movel     'MGA'         kcod
027100010608     C                   movel     v2cfil        kke1
027200160715     C     ktntbe        chain     tntbe01l
027300160718     C                   if        not %found(tntbe01l)
027400161011     C                   movel     'MGA'         tbecod
027500010608     C                   movel     v2cfil        tbeke1
027600060502     c                   clear                   tbeflt
027700010608     C                   move      *blanks       tbeftt
027800010608     C                   move      *blanks       tbeftr
027900010608     C                   move      datoggi       tbedtr
028000161011     C                   movel     v2ccau        §mgaCAUS
028100161011     C                   movel     v2cimp        §mgaIMP
028200161011     C                   movel     v2ccdc        §mgaFILD
028300161011     C                   movel     v2cdatg       §mgadti
028400161011     C                   movel     dmga          tbeuni
028500010608     C                   write     tntbe000
028600010608     C                   else
028700060502     C                   clear                   tbeflt
028800010608     C                   move      *blanks       tbeftt
028900010608     C                   move      *blanks       tbeftr
029000010608     C                   move      datoggi       tbedtr
029100161011     C                   movel     v2ccau        §mgaCAUS
029200161011     C                   movel     v2cimp        §mgaIMP
029300161011     C                   movel     v2ccdc        §mgaFILD
029400161011     C                   movel     v2cdatg       §mgadti
029500161011     C                   movel     dmga          tbeuni
029600010608     C                   update    tntbe000
029700010608     C                   endif
029800010608     C     ENDREGISTRA   endsr
029900980923      *****************************************************************
030000980923      * ROUTINE INIZIALE
030100980923      *****************************************************************
030200980923     C     *INZSR        BEGSR
030300980923      *
030400980521     C     *ENTRY        PLIST
030500980521     C                   PARM                    KPJBA
030600001030
030700980521     C                   Z-ADD     1             CODUT
030800980521     C                   CALL      'X§PARUT'
030900980521     C                   PARM                    UT§DSE0F
031000980521     C                   MOVEL     RAGUT         RSUT
031100980521     C                   MOVEL     REC80         CNCR80
031200001221
031300010608      * reperisco data e ora
031400010608     C                   TIME                    W0140
031500010608      * UDATE IN GGMMAAAA
031600010608     C                   MOVE      W0140         WDTGIO
031700010608      * UDATE IN AAAAMMGG
031800010608     C                   Z-ADD     WDTGIO        G02DAT
031900010608     C                   MOVEL     *BLANK        G02ERR
032000010608     C                   CALL      'XSRDA8'
032100010608     C                   PARM                    WLBDAT
032200010608     C                   MOVEL     G02INV        DATEU
032300010608     C                   MOVEL     DATEU         DAToggi
032400010608
032500010601     C     KTNTBE        KLIST
032600010604     C                   KFLD                    KCOD
032700010601     C                   KFLD                    KKE1
032800010611
032900001006     C                   endsr
033000980521      *---------------------------------------------------------------------------------------------
033100980521** MSG  Lungh. 78                                                            *
033200010612Inserire Punto Operativo
033300010612Punto Operativo non gestibile
033400010612Inserire tutti i dati
033500160715Centro di costo inesistente
033600160715Data errata
033700160715Causale errata
033800160718La Data deve essere superiore alla Odierna in inserimento
033900160718La Data in modifica non può essere inferiore a quella di inserimento
