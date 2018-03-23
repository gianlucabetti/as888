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
001100170109     FTNTB16D   CF   E             WORKSTN
001200980521      *------------------------------------------------------------------------*
001300010604     D KCOD            S                   LIKE(TBECOD)
001400010604     D KKE1            S                   LIKE(TBEKE1)
001500060502     D WFIL            S              4
001600980521      *   S C H I E R E
001700170104     D MSG             S             78    DIM(10) CTDATA PERRCD(1)              MSG VIDEO
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
003900170109     D Dmga          E DS
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
005400170109     C                   exfmt     TNTB161
005500170104      *
005600170104     c                   clear                   inkj
005700001006      * F3=Fine
005800001006     C   kc              leave
005900010405
006000001006      * Controlli 1° videata
006100010604     C                   SETOFF                                       28
006200001006     C                   exsr      Sr_Contr01
006300980623
006400010601     C   28              iter
006500001009     C   90              iter
006600001009
006700010608      * Caricamento e pulizia seconda videata
006800010608     C                   exsr      Sr_carvid
006900010608
007000010608      * Emissione 2° Videata
007100010608     C     riemetto      tag
007200170109     C                   exfmt     TNTB162
007300010611
007400010611     C                   eval      *in28 = *off
007500010608
007600010606      * F3=Fine
007700010606     C   kc              LEAVE
007800010606
007900010608      * F12=Ritorno
008000160915     C   kl              clear                   v2ccdcd
008100160915     c   kl              clear                   v2ccaud
008200010608     C   kl              ITER
008300170104      *immetto nuovo rekord
008400170104     C                   if        *inkj
008500170104     C                   movel     v1cfil        v2cfil
008600170104     C                   movel     *blanks       v2ccau
008700170109     C                   movel     *blanks       v2ccaud
008800170109     C                   movel     *blanks       v2ccdcd
008900170104     C                   movel     *zeros        v2ccdc
009000170104     C                   movel     *zeros        v2cimp
009100170104     C                   movel     *zeros        v2cdat
009200170104     C                   movel     *zeros        v2cdatf
009300170104     C                   movel     *zeros        v2cdatins         8 0
009400170104     c                   move      'X'           inkj              1
009500170109     c                   setoff                                       9899
009600170109     C                   goto      riemetto
009700170104     c                   endif
009800010608
009900010608     C                   exsr      Sr_Contr02
010000010608     C   28              goto      riemetto
010100010608
010200010608      * F6=Aggiornamento
010300170104     C   kf              exsr      Sr_Registra
010400170104     C   28              goto      riemetto
010500010608     C   kf              iter
010600010608
010700010608     C                   goto      riemetto
010800010608
010900001006     C                   enddo
011000981204
011100001006     C                   eval      *inlr = *on
011200980521      **********************************************************************
011300980521      * CONTROLLI VIDEO 1
011400980521      **********************************************************************
011500001006     C     Sr_Contr01    begsr
011600001009      * Reset indicatori
011700161013     C                   setoff                                       90
011800010601
011900010601     C     '?'           scan      v1cfil                                 31
012000010601     C     *in31         ifeq      *on
012100010601     C                   eval      *in90 = *on
012200010601     C                   clear                   TIBS02DS
012300010601     C                   movel     'R'           t02mod
012400010601     C                   movel     knsif         t02sif
012500170109     C                   movel     'MGA'         t02cod
012600010601     C                   call      'TIBS02R'
012700010601     C                   parm                    KPJBA
012800010601     C                   parm                    TIBS02DS
012900010601     C                   if        t02err =  *blanks
013000010601     C                   movel     t02ke1        v1cfil
013100010601     C                   SETON                                        90
013200010601     C                   ELSE
013300010612     C                   SETON                                        28
013400010612     C                   MOVEL     MSG(1)        V1CMSG
013500010601     C                   GOTO      ENDSRCONTR
013600010601     C                   endif
013700010604     C                   ENDIF
013800010601
013900010611      * controllo se campo valorizzato
014000010608     C                   IF        V1CFIL = *ZEROS OR V1CFIL = *BLANKS
014100010612     C                   MOVE      *BLANKS       V1DFIL
014200010612     C                   SETON                                        28        controllo P.O.emi.
014300010612     C                   MOVEL     MSG(1)        V1CMSG
014400010601     C                   GOTO      ENDSRCONTR                                   normale
014500010601     C                   ENDIF
014600010212
014700010611      * reperisco le descrizioni
014800060503     c* la descrizione la devo reperire solo se inserito numero con primo by
014900060503     c* te = 0
015000060503     C                   TESTN                   v1cfil               31
015100060503     C                   MOVE      v1cfil        W001A             1
015200060503    2C     *IN31         IFEQ      *On
015300060503     C     W001A         andge     '0'
015400060502     C                   MOVE      V1CFIL        W3CAR             3 0
015500010611     C     W3CAR         CHAIN     AZORG01L                           31
015600010611     C                   IF        *IN31 = *OFF
015700010611     C                   MOVEL     ORGDES        V2DFIL
015800010611     C                   MOVEL     ORGDES        V1DFIL
015900010611     C                   ELSE
016000010611     C                   CLEAR                   V1DFIL
016100060503     C                   CLEAR                   V2DFIL
016200010611     C                   ENDIF
016300060503     C                   ENDIF
016400010611
016500010611     C     ENDSRCONTR    endsr
016600010608      **********************************************************************
016700010608      * CONTROLLI VIDEO 2
016800010608      **********************************************************************
016900010608     C     Sr_Contr02    begsr
017000010608      * Reset indicatori
017100160715     C                   setoff                                       284041
017200170104     C                   setoff                                       424399
017300170104     C                   setoff                                       98
017400160715      * ricerca causale
017500160715     C     '?'           scan      v2ccau                                 31
017600160715     C                   clear                   TIBS02DS
017700160715     C                   movel     'TCE'         t02cod
017800160715     C                   movel     knsif         t02sif
017900160715     C     *in31         ifeq      *on
018000160715     C                   movel     'R'           t02mod
018100160715     c                   else
018200160715     C                   movel     'C'           t02mod
018300160715     C                   movel(p)  v2ccau        t02ke1
018400160715     c                   endif
018500160715     C                   call      'TIBS02R'
018600160715     C                   parm                    KPJBA
018700160715     C                   parm                    TIBS02DS
018800160715     C                   if        t02err =  *blanks
018900160715     C                   movel     t02ke1        v2ccau
019000160715     c                   movel     t02uni        dtce
019100160715     c                   movel     §tcedes       v2ccaud
019200160715     C                   ELSE
019300160715     C                   clear                   v2ccau
019400160715     c                   clear                   dtce
019500160715     c                   clear                   v2ccaud
019600160715     C                   SETON                                        2840
019700160715     C                   MOVEL     MSG(6)        V1CMSG
019800160715     C                   GOTO      ENDSRCONT2
019900160715     C                   endif
020000160715      * centro di costo
020100160715     c                   if        v2ccdc <> 0
020200160715     c     v2ccdc        chain     azorg01l
020300160715     c                   if        %found(azorg01l)
020400160715     c                   movel     orgdes        v2ccdcd
020500160715     c                   else
020600160715     C                   SETON                                        2841
020700160715     c                   clear                   v2ccdcd
020800160715     C                   MOVEL     MSG(4)        V1CMSG
020900160715     C                   GOTO      ENDSRCONT2
021000160715     c                   endif
021100160715     c                   endif
021200160715      * data
021300160715     c                   if        v2cdat <> 0
021400160715     C                   Z-ADD     v2cdat        G02DAT
021500160715     C                   MOVEL     *BLANK        G02ERR
021600160715     C                   CALL      'XSRDA8'
021700160715     C                   PARM                    WLBDAT
021800160715     c                   if        g02err = '0'
021900160715     C                   MOVEL     G02INV        v2cdatg           8 0
022000160718     C                   MOVEL     G02dat        v2cdat
022100160718     c                   if        v2cdatins = 0
022200160718     c                   if        v2cdatg < datoggi
022300160718     C                   SETON                                        2842
022400160718     C                   MOVEL     MSG(7)        V1CMSG
022500160718     C                   GOTO      ENDSRCONT2
022600160718     c                   endif
022700160718     c                   else
022800160718     c                   if        v2cdatg < v2cdatins
022900160718     C                   SETON                                        2842
023000160718     C                   MOVEL     MSG(8)        V1CMSG
023100160718     C                   GOTO      ENDSRCONT2
023200160718     c                   endif
023300160718     c                   endif
023400160715     c                   else
023500160715     C                   SETON                                        2842
023600160715     C                   MOVEL     MSG(5)        V1CMSG
023700160715     C                   GOTO      ENDSRCONT2
023800160715     c                   endif
023900160715     c                   endif
024000161223      * data fine
024100161223     c                   if        v2cdatf <> 0
024200161223     C                   Z-ADD     v2cdatf       G02DAT
024300161223     C                   MOVEL     *BLANK        G02ERR
024400161223     C                   CALL      'XSRDA8'
024500161223     C                   PARM                    WLBDAT
024600161223     c                   if        g02err = '0'
024700161223     C                   MOVEL     G02INV        v2cdatfg          8 0
024800161223     C                   MOVEL     G02dat        v2cdatf
024900161223     c                   if        v2cdatfg < v2cdatg
025000161223     C                   SETON                                        2844
025100161223     C                   MOVEL     MSG(9)        V1CMSG
025200161223     C                   GOTO      ENDSRCONT2
025300161223     c                   endif
025400161223     c                   else
025500161223     C                   SETON                                        2844
025600161223     C                   MOVEL     MSG(5)        V1CMSG
025700161223     C                   GOTO      ENDSRCONT2
025800161223     c                   endif
025900170104     c                   if        v2cdatfg < datoggi
026000170104     C                   SETON                                        9998
026100170104     C                   GOTO      ENDSRCONT2
026200170104     c                   else
026300170104     C                   SETON                                        98
026400170104     c                   endif
026500161223     c                   endif
026600170105      *verifico se in inserimento il rekord non si sovrappone all'ultimo esistente
026700170105     c                   if        inkj <> *blank
026800170109     C                   movel     tbeuni        dmga
026900170111     c                   if        v2cdatg < §mgadtf or
027000170111     c                             v2cdatg = §mgadti
027100170105     C                   SETON                                        2844
027200170105     C                   MOVEL     MSG(10)       V1CMSG
027300170105     C                   SETOff                                       98
027400170105     C                   leavesr
027500170105     c                   endif
027600170105     c                   endif
027700160715      * se uno è vuoto
027800160715     c                   select
027900160715     C                   when      v2ccau = *blanks
028000160715     c                   seton                                        4028
028100160715     C                   when      v2cimp = 0
028200160715     C                   seton                                        4328
028300160715     C                   when      v2ccdc = 0
028400160715     C                   seton                                        4128
028500160715     C                   when      v2cdat = 0
028600160715     C                   seton                                        4228
028700161223     C                   when      v2cdatf = 0
028800161223     C                   seton                                        4428
028900160715     C                   endsl
029000160715     C   28              MOVEL     MSG(3)        V2CMSG
029100010608
029200010608     C     ENDSRCONT2    endsr
029300010608      **********************************************************************
029400010608      * CARICAMENTO SECONDA VIDEATA
029500010608      **********************************************************************
029600010608     C     Sr_Carvid     begsr
029700010608
029800161013     c*
029900010608     C                   movel     v1cfil        v2cfil
030000160715     C                   movel     *blanks       v2ccau
030100160715     C                   movel     *zeros        v2ccdc
030200160715     C                   movel     *zeros        v2cimp
030300160715     C                   movel     *zeros        v2cdat
030400161223     C                   movel     *zeros        v2cdatf
030500160718     C                   movel     *zeros        v2cdatins         8 0
030600010608     C                   movel     v1cfil        kke1
030700170109     C                   movel     'MGA'         kcod
030800170104     C     ktntbe        setgt     tntbe01l
030900170104     C     ktntbe        readpe    tntbe01l
031000170104     C                   if        %eof(tntbe01l)
031100170104      *non trovato pulisco i campi
031200170104     C                   clear                   v2ccau
031300170104     C                   clear                   v2cimp
031400170104     C                   clear                   v2ccdc
031500170104     C                   clear                   v2cdat
031600170104     C                   clear                   v2cdatf
031700170104     c                   leavesr
031800170104     c                   else
031900170104      *per i rekord già esistenti e senza data impostata forzo chiusura anno
032000170104     c                   if        %subst(tbeuni:24:8) = *blank
032100170104     c                   eval      %subst(tbeuni:24:8) = '20161231'
032200170104     c                   endif
032300170109     C                   movel     tbeuni        dmga
032400170109     C                   movel     §mgaCAUS      v2ccau
032500170109     C                   z-add     §mgaIMP       v2cimp
032600170109     C                   z-add     §mgaFILD      v2ccdc
032700170104      *data inizio
032800170109     C                   move      §mgaDTI       g02inv
032900160715     C                   MOVEL     '3'           G02ERR
033000160715     C                   CALL      'XSRDA8'
033100160715     C                   PARM                    WLBDAT
033200160715     C                   move      g02dat        v2cdat
033300160718     C                   move      g02inv        v2cdatins
033400170104      *data fine
033500170109     C                   move      §mgaDTf       g02inv
033600170104     C                   MOVEL     '3'           G02ERR
033700170104     C                   CALL      'XSRDA8'
033800170104     C                   PARM                    WLBDAT
033900170104     C                   move      g02dat        v2cdatf
034000170104     C                   move      g02inv        v2cdatfg          8 0
034100160715     c                   exsr      sr_contr02
034200010608     C                   endif
034300010608
034400010608     C                   endsr
034500010608      **********************************************************************
034600010608      * REGISTRAZIONE DATI IMMESSI
034700010608      **********************************************************************
034800010608     C     Sr_Registra   begsr
034900010608
035000170109     C                   movel     'MGA'         kcod
035100010608     C                   movel     v2cfil        kke1
035200170104     C     ktntbe        setgt     tntbe01l
035300170104     C     ktntbe        readpe    tntbe01l
035400170104     C                   if        %eof(tntbe01l)
035500170109     C                   movel     'MGA'         tbecod
035600010608     C                   movel     v2cfil        tbeke1
035700170104     C                   movel     v2cdatg       tbeke2
035800060502     c                   clear                   tbeflt
035900010608     C                   move      *blanks       tbeftt
036000010608     C                   move      *blanks       tbeftr
036100010608     C                   move      datoggi       tbedtr
036200170109     C                   movel     v2ccau        §mgaCAUS
036300170109     C                   movel     v2cimp        §mgaIMP
036400170109     C                   movel     v2ccdc        §mgaFILD
036500170109     C                   movel     v2cdatg       §mgadti
036600170109     C                   movel     v2cdatfg      §mgadtf
036700170109     C                   movel     dmga          tbeuni
036800010608     C                   write     tntbe000
036900010608     C                   else
037000170104      * per inserimento esegue write
037100170104     c                   if        inkj <> *blank
037200170109     C                   movel     'MGA'         tbecod
037300170104     C                   movel     v2cfil        tbeke1
037400170104     C                   movel     v2cdatg       tbeke2
037500170104     c                   clear                   tbeflt
037600170104     C                   move      *blanks       tbeftt
037700170104     C                   move      *blanks       tbeftr
037800170104     C                   move      datoggi       tbedtr
037900170109     C                   movel     v2ccau        §mgaCAUS
038000170109     C                   movel     v2cimp        §mgaIMP
038100170109     C                   movel     v2ccdc        §mgaFILD
038200170109     C                   movel     v2cdatg       §mgadti
038300170109     C                   movel     v2cdatfg      §mgadtf
038400170109     C                   movel     dmga          tbeuni
038500170104     C                   write     tntbe000
038600170104      *aggiorna esistente
038700170104     c                   else
038800060502     C                   clear                   tbeflt
038900010608     C                   move      *blanks       tbeftt
039000010608     C                   move      *blanks       tbeftr
039100010608     C                   move      datoggi       tbedtr
039200170109     C                   movel     v2ccau        §mgaCAUS
039300170109     C                   movel     v2cimp        §mgaIMP
039400170109     C                   movel     v2ccdc        §mgaFILD
039500170109     C                   movel     v2cdatg       §mgadti
039600170110     C                   movel     v2cdatg       tbeke2
039700170109     C                   movel     v2cdatfg      §mgadtf
039800170109     C                   movel     dmga          tbeuni
039900010608     C                   update    tntbe000
040000170104     C                   endif
040100010608     C                   endif
040200010608     C     ENDREGISTRA   endsr
040300980923      *****************************************************************
040400980923      * ROUTINE INIZIALE
040500980923      *****************************************************************
040600980923     C     *INZSR        BEGSR
040700980923      *
040800980521     C     *ENTRY        PLIST
040900980521     C                   PARM                    KPJBA
041000001030
041100980521     C                   Z-ADD     1             CODUT
041200980521     C                   CALL      'X§PARUT'
041300980521     C                   PARM                    UT§DSE0F
041400980521     C                   MOVEL     RAGUT         RSUT
041500980521     C                   MOVEL     REC80         CNCR80
041600001221
041700010608      * reperisco data e ora
041800010608     C                   TIME                    W0140
041900010608      * UDATE IN GGMMAAAA
042000010608     C                   MOVE      W0140         WDTGIO
042100010608      * UDATE IN AAAAMMGG
042200010608     C                   Z-ADD     WDTGIO        G02DAT
042300010608     C                   MOVEL     *BLANK        G02ERR
042400010608     C                   CALL      'XSRDA8'
042500010608     C                   PARM                    WLBDAT
042600010608     C                   MOVEL     G02INV        DATEU
042700010608     C                   MOVEL     DATEU         DAToggi
042800010608
042900010601     C     KTNTBE        KLIST
043000010604     C                   KFLD                    KCOD
043100010601     C                   KFLD                    KKE1
043200010611
043300001006     C                   endsr
043400980521      *---------------------------------------------------------------------------------------------
043500980521** MSG  Lungh. 78                                                            *
043600161013Inserire Filiale
043700161013Filiale non gestibile
043800010612Inserire tutti i dati
043900160715Centro di costo inesistente
044000160715Data errata
044100160715Causale errata
044200160718La Data deve essere superiore alla Odierna in inserimento
044300160718La Data in modifica non può essere inferiore a quella di inserimento
044400161223La Data fine deve essere superiore alla data decorrenza
044500170104Il Rekord inserito si sovrappone al precedente verificare data inizio scrittura
