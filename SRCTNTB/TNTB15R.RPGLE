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
001001180208     FTNTBE02L  iF   E           K DISK    rename(tntbe000:tntbe2) prefix(x)
001100170109     FTNTB15D   CF   E             WORKSTN
001200980521      *------------------------------------------------------------------------*
001300010604     D KCOD            S                   LIKE(TBECOD)
001400010604     D KKE1            S                   LIKE(TBEKE1)
001401180208     D xCOD            S                   LIKE(TBECOD)
001403180208     D xKE1            S                   LIKE(TBEKE1)
001404180208     D xsif            S                   LIKE(TBEsif)  inz
001405180208     D xlin            S                   LIKE(TBElin)  inz
001500060502     D WFIL            S              4
001600980521      *   S C H I E R E
001700180208     D MSG             S             78    DIM(11) CTDATA PERRCD(1)              MSG VIDEO
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
003900170109     D Dbon          E DS
003901180208     D Dboi          E DS
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
007200160715     C                   exfmt     TNTB152
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
008700170104     C                   movel     *zeros        v2ccdc
008800170109     C                   movel     *blanks       v2ccaud
008900170109     C                   movel     *blanks       v2ccdcd
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
010400180208     C   28              goto      riemetto
010401180208     C   kf              movel     *blanks       v2ccaud
010402180208     C   kf              movel     *blanks       v2ccdcd
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
012500170109     C                   movel     'BON'         t02cod
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
017400161019     c                   if        *in10
017500161019     c                   seton                                        28
017600161019     c                   end
017700160715      * ricerca causale
017800160715     C     '?'           scan      v2ccau                                 31
017900160715     C                   clear                   TIBS02DS
018000160715     C                   movel     'TCE'         t02cod
018100160715     C                   movel     knsif         t02sif
018200160715     C     *in31         ifeq      *on
018300160715     C                   movel     'R'           t02mod
018400160715     c                   else
018500160715     C                   movel     'C'           t02mod
018600160715     C                   movel(p)  v2ccau        t02ke1
018700160715     c                   endif
018800160715     C                   call      'TIBS02R'
018900160715     C                   parm                    KPJBA
019000160715     C                   parm                    TIBS02DS
019100160715     C                   if        t02err =  *blanks
019200160715     C                   movel     t02ke1        v2ccau
019300160715     c                   movel     t02uni        dtce
019400160715     c                   movel     §tcedes       v2ccaud
019500160715     C                   ELSE
019600160715     C                   clear                   v2ccau
019700160715     c                   clear                   dtce
019800160715     c                   clear                   v2ccaud
019900160715     C                   SETON                                        2840
020000160715     C                   MOVEL     MSG(6)        V1CMSG
020100160715     C                   GOTO      ENDSRCONT2
020200160715     C                   endif
020300160715      * centro di costo
020400160715     c                   if        v2ccdc <> 0
020500160715     c     v2ccdc        chain     azorg01l
020600160715     c                   if        %found(azorg01l)
020700160715     c                   movel     orgdes        v2ccdcd
020800160715     c                   else
020900160715     C                   SETON                                        2841
021000160715     c                   clear                   v2ccdcd
021100160715     C                   MOVEL     MSG(4)        V1CMSG
021200160715     C                   GOTO      ENDSRCONT2
021300160715     c                   endif
021400160715     c                   endif
021500160715      * data
021600160715     c                   if        v2cdat <> 0
021700160715     C                   Z-ADD     v2cdat        G02DAT
021800160715     C                   MOVEL     *BLANK        G02ERR
021900160715     C                   CALL      'XSRDA8'
022000160715     C                   PARM                    WLBDAT
022100160715     c                   if        g02err = '0'
022200160715     C                   MOVEL     G02INV        v2cdatg           8 0
022300160718     C                   MOVEL     G02dat        v2cdat
022400160718     c                   if        v2cdatins = 0
022500160718     c                   if        v2cdatg < datoggi
022600160718     C                   SETON                                        2842
022700160718     C                   MOVEL     MSG(7)        V1CMSG
022800160718     C                   GOTO      ENDSRCONT2
022900160718     c                   endif
023000160718     c                   else
023100160718     c                   if        v2cdatg < v2cdatins
023200160718     C                   SETON                                        2842
023300160718     C                   MOVEL     MSG(8)        V1CMSG
023400160718     C                   GOTO      ENDSRCONT2
023500160718     c                   endif
023600160718     c                   endif
023700160715     c                   else
023800160715     C                   SETON                                        2842
023900160715     C                   MOVEL     MSG(5)        V1CMSG
024000160715     C                   GOTO      ENDSRCONT2
024100160715     c                   endif
024200160715     c                   endif
024300161223      * data fine
024400161223     c                   if        v2cdatf <> 0
024500161223     C                   Z-ADD     v2cdatf       G02DAT
024600161223     C                   MOVEL     *BLANK        G02ERR
024700161223     C                   CALL      'XSRDA8'
024800161223     C                   PARM                    WLBDAT
024900161223     c                   if        g02err = '0'
025000161223     C                   MOVEL     G02INV        v2cdatfg          8 0
025100161223     C                   MOVEL     G02dat        v2cdatf
025200161223     c                   if        v2cdatfg < v2cdatg
025300161223     C                   SETON                                        2844
025400161223     C                   MOVEL     MSG(9)        V1CMSG
025500161223     C                   GOTO      ENDSRCONT2
025600161223     c                   endif
025700161223     c                   else
025800161223     C                   SETON                                        2844
025900161223     C                   MOVEL     MSG(5)        V1CMSG
026000161223     C                   GOTO      ENDSRCONT2
026100161223     c                   endif
026200170104     c                   if        v2cdatfg < datoggi
026300170104     C                   SETON                                        9998
026400170104     C                   GOTO      ENDSRCONT2
026500170104     c                   else
026600170104     C                   SETON                                        98
026700170104     c                   endif
026800161223     c                   endif
026900170105      *verifico se in inserimento il rekord non si sovrappone all'ultimo esistente
027000170105     c                   if        inkj <> *blank
027100170109     C                   movel     tbeuni        dbon
027200170111     c                   if        v2cdatg < §bondtf or
027300170111     c                             v2cdatg = §bondti
027400170105     C                   SETON                                        2844
027500170105     C                   MOVEL     MSG(10)       V1CMSG
027600170105     C                   SETOff                                       98
027700170105     C                   leavesr
027800170105     c                   endif
027900170105     c                   endif
028000160715      * se uno è vuoto
028100160715     c                   select
028200160715     C                   when      v2ccau = *blanks
028300160715     c                   seton                                        4028
028400160715     C                   when      v2cimp = 0
028500160715     C                   seton                                        4328
028600160715     C                   when      v2ccdc = 0
028700160715     C                   seton                                        4128
028800160715     C                   when      v2cdat = 0
028900160715     C                   seton                                        4228
029000161223     C                   when      v2cdatf = 0
029100161223     C                   seton                                        4428
029200160715     C                   endsl
029300160715     C   28              MOVEL     MSG(3)        V2CMSG
029301180208     c                   exsr      verificaBOI
029302180208     c
029400010608
029500010608     C     ENDSRCONT2    endsr
029501180208      **********************************************************************
029502180208      * verifica se esiste record in tabella BOI nello stesso periodo
029504180208      **********************************************************************
029515180208     C     VerificaBOI   begsr
029516180208     C                   SETOff                                       1028
029517180208     C                   clear                   V1CMSG
029519180208     c* controllo se presente anche la tabella BOI
029520180208     C                   movel     v1cfil        xke1
029521180208     C                   movel     'BOI'         xcod
029522180208     C     ktntbe2       setll     tntbe02l
029523180208     c                   do        *hival
029524180208     c     ktntbe2       reade     tntbe02l
029525180208     c                   if        %eof(tntbe02l)
029526180208     c                   leave
029527180208     c                   endif
029528180208     c                   if        xtbeatb <> *blank
029529180208     c                   iter
029530180208     c                   endif
029531180208     c                   movel     xtbeuni       dboi
029532180208     c                   if        v2cdatg < §boidtf and
029533180208     c                             v2cdatg >= §boidti or
029535180208     c                             v2cdatfg <= §boidtf and
029536180208     c                             v2cdatfg >= §boidti  or
029537180208     c                             v2cdatfg >= §boidtf and
029538180208     c                             v2cdatg <= §boidti
029541180208     C                   SETON                                        2810
029542180208     C                   MOVEL     MSG(11)       V1CMSG
029543180208     C                   SETOff                                       98
029544180208     C                   leavesr
029545180208     c                   endif
029546180208     c                   enddo
029547180208      *
029548180208     C                   endsr
029549180208     c
029600010608      **********************************************************************
029700010608      * CARICAMENTO SECONDA VIDEATA
029800010608      **********************************************************************
029900010608     C     Sr_Carvid     begsr
030000010608
030100170109     c* controllo se presente anche la tabella BOI
030200180208     C*                  movel     v1cfil        kke1
030300180208     C*                  movel     'BOI'         kcod
030400180208     C*    ktntbe        setll     tntbe01l                               10
030500161013     c*
030600010608     C                   movel     v1cfil        v2cfil
030700160715     C                   movel     *blanks       v2ccau
030800160715     C                   movel     *zeros        v2ccdc
030900160715     C                   movel     *zeros        v2cimp
031000160715     C                   movel     *zeros        v2cdat
031100161223     C                   movel     *zeros        v2cdatf
031200160718     C                   movel     *zeros        v2cdatins         8 0
031300010608     C                   movel     v1cfil        kke1
031400170109     C                   movel     'BON'         kcod
031500170104     C     ktntbe        setgt     tntbe01l
031600170104     C     ktntbe        readpe    tntbe01l
031700170104     C                   if        %eof(tntbe01l)
031800170104      *non trovato pulisco i campi
031900170104     C                   clear                   v2ccau
032000170104     C                   clear                   v2cimp
032100170104     C                   clear                   v2ccdc
032200170104     C                   clear                   v2cdat
032300170104     C                   clear                   v2cdatf
032400170104     c                   leavesr
032500170104     c                   else
032600170104      *per i rekord già esistenti e senza data impostata forzo chiusura anno
032700170104     c                   if        %subst(tbeuni:24:8) = *blank
032800170104     c                   eval      %subst(tbeuni:24:8) = '20161231'
032900170104     c                   endif
033000170109     C                   movel     tbeuni        dbon
033100170109     C                   movel     §BOnCAUS      v2ccau
033200170109     C                   z-add     §BOnIMP       v2cimp
033300170109     C                   z-add     §BOnFILD      v2ccdc
033400170104      *data inizio
033500170109     C                   move      §BOnDTI       g02inv
033600160715     C                   MOVEL     '3'           G02ERR
033700160715     C                   CALL      'XSRDA8'
033800160715     C                   PARM                    WLBDAT
033900160715     C                   move      g02dat        v2cdat
034000160718     C                   move      g02inv        v2cdatins
034100170104      *data fine
034200170109     C                   move      §BOnDTf       g02inv
034300170104     C                   MOVEL     '3'           G02ERR
034400170104     C                   CALL      'XSRDA8'
034500170104     C                   PARM                    WLBDAT
034600170104     C                   move      g02dat        v2cdatf
034700170104     C                   move      g02inv        v2cdatfg          8 0
034800160715     c                   exsr      sr_contr02
034900010608     C                   endif
035000010608
035100010608     C                   endsr
035200010608      **********************************************************************
035300010608      * REGISTRAZIONE DATI IMMESSI
035400010608      **********************************************************************
035500010608     C     Sr_Registra   begsr
035600010608
035700170109     C                   movel     'BON'         kcod
035800010608     C                   movel     v2cfil        kke1
035900170104     C     ktntbe        setgt     tntbe01l
036000170104     C     ktntbe        readpe    tntbe01l
036100170104     C                   if        %eof(tntbe01l)
036200170109     C                   movel     'BON'         tbecod
036300010608     C                   movel     v2cfil        tbeke1
036400170104     C                   movel     v2cdatg       tbeke2
036500060502     c                   clear                   tbeflt
036600010608     C                   move      *blanks       tbeftt
036700010608     C                   move      *blanks       tbeftr
036800010608     C                   move      datoggi       tbedtr
036900170109     C                   movel     v2ccau        §BONCAUS
037000170109     C                   movel     v2cimp        §BOnIMP
037100170109     C                   movel     v2ccdc        §BOnFILD
037200170109     C                   movel     v2cdatg       §bondti
037300170109     C                   movel     v2cdatfg      §bondtf
037400170109     C                   movel     dbon          tbeuni
037500010608     C                   write     tntbe000
037600010608     C                   else
037700170104      * per inserimento esegue write
037800170104     c                   if        inkj <> *blank
037900170109     C                   movel     'BON'         tbecod
038000170104     C                   movel     v2cfil        tbeke1
038100170104     C                   movel     v2cdatg       tbeke2
038200170104     c                   clear                   tbeflt
038300170104     C                   move      *blanks       tbeftt
038400170104     C                   move      *blanks       tbeftr
038500170104     C                   move      datoggi       tbedtr
038600170109     C                   movel     v2ccau        §BOnCAUS
038700170109     C                   movel     v2cimp        §BOnIMP
038800170109     C                   movel     v2ccdc        §BOnFILD
038900170109     C                   movel     v2cdatg       §bondti
039000170110     C                   movel     v2cdatg       tbeke2
039100170109     C                   movel     v2cdatfg      §bondtf
039200170109     C                   movel     dbon          tbeuni
039300170104     C                   write     tntbe000
039400170104      *aggiorna esistente
039500170104     c                   else
039600060502     C                   clear                   tbeflt
039700010608     C                   move      *blanks       tbeftt
039800010608     C                   move      *blanks       tbeftr
039900010608     C                   move      datoggi       tbedtr
040000170109     C                   movel     v2ccau        §BonCAUS
040100170109     C                   movel     v2cimp        §BOnIMP
040200170109     C                   movel     v2ccdc        §BOnFILD
040300170109     C                   movel     v2cdatg       §bondti
040400170110     C                   movel     v2cdatg       tbeke2
040500170109     C                   movel     v2cdatfg      §bondtf
040600170109     C                   movel     dbon          tbeuni
040700010608     C                   update    tntbe000
040800170104     C                   endif
040900010608     C                   endif
041000010608     C     ENDREGISTRA   endsr
041100980923      *****************************************************************
041200980923      * ROUTINE INIZIALE
041300980923      *****************************************************************
041400980923     C     *INZSR        BEGSR
041500980923      *
041600980521     C     *ENTRY        PLIST
041700980521     C                   PARM                    KPJBA
041800001030
041900980521     C                   Z-ADD     1             CODUT
042000980521     C                   CALL      'X§PARUT'
042100980521     C                   PARM                    UT§DSE0F
042200980521     C                   MOVEL     RAGUT         RSUT
042300980521     C                   MOVEL     REC80         CNCR80
042400001221
042500010608      * reperisco data e ora
042600010608     C                   TIME                    W0140
042700010608      * UDATE IN GGMMAAAA
042800010608     C                   MOVE      W0140         WDTGIO
042900010608      * UDATE IN AAAAMMGG
043000010608     C                   Z-ADD     WDTGIO        G02DAT
043100010608     C                   MOVEL     *BLANK        G02ERR
043200010608     C                   CALL      'XSRDA8'
043300010608     C                   PARM                    WLBDAT
043400010608     C                   MOVEL     G02INV        DATEU
043500010608     C                   MOVEL     DATEU         DAToggi
043600010608
043700010601     C     KTNTBE        KLIST
043800010604     C                   KFLD                    KCOD
043900010601     C                   KFLD                    KKE1
044000010611
044001180208     C     KTNTBE2       KLIST
044002180208     C                   KFLD                    xCOD
044003180208     C                   KFLD                    xlin
044005180208     C                   KFLD                    xsif
044008180208     C                   KFLD                    xKE1
044100001006     C                   endsr
044200980521      *---------------------------------------------------------------------------------------------
044300980521** MSG  Lungh. 78                                                            *
044400161013Inserire Filiale
044500161013Filiale non gestibile
044600010612Inserire tutti i dati
044700160715Centro di costo inesistente
044800160715Data errata
044900160715Causale errata
045000160718La Data deve essere superiore alla Odierna in inserimento
045100160718La Data in modifica non può essere inferiore a quella di inserimento
045200161223La Data fine deve essere superiore alla data decorrenza
045201180208Il Rekord inserito si sovrappone al precedente verificare data inizio scrittura
045300180208Il Rekord inserito si sovrappone ad un record presente nella tabella BOI
