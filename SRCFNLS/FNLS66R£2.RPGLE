000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941007     H* FNLS66R *-----------------------------------------------------*
000300970515     H*         CONFERMA BOLLA CLIENTI
000400930609     H*---------------------------------------------------------------*
000500030611     FFNLS66D   CF   E             WORKSTN    INFDS(CA02DS)
000600030611     F                                        SFILE(LS66DSF:NRR)
000700960103     FAZCLN01L  IF   E           K DISK
000800060221     FFNBLP01L  iF   E           K DISK
000900021203     FFIAPD01L  IF   E           K DISK
001000900827     FTABEL00F  IF   E           K DISK
001100020219     FTNTBE01L  IF   E           K DISK
001200030611     FTNTBE02L  IF   E           K DISK    RENAME(TNTBE000:TNTBE02)
001300020927     FFIVAB01L  UF   E           K DISK    INFDS(TARDS1)
001400020927     FFIVAB02L  IF   E           K DISK    RENAME(FIVAB000:FIVAB02)
001500020927     F                                     INFDS(TARDS2)
001600020927     FFIVAB03L  IF   E           K DISK    RENAME(FIVAB000:FIVAB03)
001700020927     F                                     INFDS(TARDS3)
001800130121     FFIVAB10L  uF   E           K DISK    RENAME(FIVAB000:FIVAB10)
001900130121     F                                     INFDS(TARDS10)
002000020927     FFIVAB06L  IF   E           K DISK    RENAME(FIVAB000:FIVAB06)
002100020927     F                                     INFDS(TARDS6)
002200080623     FFIVAB09L  uF   E           K DISK    RENAME(FIVAB000:FIVAB09)
002300080623     F                                     INFDS(TARDS9)
002400020927     FFIVAB00F  UF   E             DISK    RENAME(FIVAB000:FIVAB0FF)
002500020910     FFIVAD02L  UF   E           K DISK
002600040604     FFIVAX01L  UF   E           K DISK
002700020910     FFIVAT01L  UF   E           K DISK
002800900807     FCNACO00F  IF   E           K DISK
002900011126     FFIFTT02L  IF   E           K DISK
003000000512     FAZORG01L  IF   E           K DISK
003100010219     FFNORM01L  IF   E           K DISK
003200120607     FFIDST01L  IF   E           K DISK
003300010220     FFNFVV01L  IF   E           K DISK
003400061122     FFIDPO06L  IF   E           K DISK
003500081017     ffirab02l  uf   e           k disk
003600950112     FFNLV22P   O    F  132        PRINTER USROPN
003700030611
003800950407     D CTM             S              2    DIM(70)
003900120710     D CTMF            S             13    DIM(70)
004000120710     D CTM16           S              1    DIM(70)
004100960103     D K78             S              1    DIM(78)
004200960103     D K31             S              1    DIM(31)
004300120903     D*CLI             S              7  0 DIM(100)
004400030611
004500000518     D TES             S             33    DIM(3) CTDATA PERRCD(1)
004600991201     D CM3             S              1    DIM(122) CTDATA PERRCD(61)
004700030611      * le schiere per OVRPRTF dei moduli LASER sono + lunghe
004800030611     D CMA4            S              1    DIM(130) CTDATA PERRCD(65)
004900030611     D CMA5            S              1    DIM(130) CTDATA PERRCD(65)
005000120709     D ERR             S             78    DIM(33) CTDATA PERRCD(1)
005100030409
005200030409     d Dataiso         s               d   datfmt(*iso)
005300051215     d*woggi5          s              8  0
005400051215     d woggidcgm1      s              8  0
005500051215     d woggidcgm2      s              8  0
005600030409     d wdcr            s              8  0
005700051215     d s_woggidcg      s                   like(woggidcgm1)
005800051215     d s_vpodcg        s                   like(§vpodcgm1)
005900071218     d s_vi4ksc        s                   like(vi4ksc)
006000071219     d s_vi4tcr        s                   like(vi4tcr)
006100071219     d s_vi4dcr        s                   like(vi4dcr)
006200071219     d s_vi4hcr        s                   like(vi4hcr)
006300071219     d s_vi4ore        s                   like(vi4ore)
006400071218     d $conta          s              1  0 inz
006500120709     d s_vi4ctm        s                   like(vi4ctm)
006600040301
006700040301     d wtxt            S           2048
006800040301     d TxtInOut        S           2048
006900040301     d ElencoChar      S            256
007000040301     d TipoElenco      S              1
007100040301     d CharSost        S              1
007200040301     d UpperCase       S              1
007300040301     d ChkNull         S              1    Inz('1')
007400040301     d CharNull        S              1
007500040301     d Esito           S              1
007600041222
007700041222     d savctr          s                   Like(vi4ctr)
007800041222     d wfctr           s              1    Inz(*Off)
007900030409
008000030409     d Vi4Hcr          ds                  Inz
008100030409     d  Vi4Hcro
008200030409     d  Vi4Hcrm
008300030409
008400021203      * Ds esterna per ricerca codice padroncino
008500021203     D FNLV24DS      E DS
008600060926     D* DS PER FNLV55R - RICERCA TERMINAL DI PARTENZA/ARRIVO
008700060926     D DSLV55        E DS                  EXTNAME(FNLV55DS)
008800000626      * PASSAGGIO A CHIUSURA FV                     -FNLSA2R-
008900000626     D PARAM3          DS
009000000626     D* CATEGORIA FOGLIO I.M.P.
009100000626     D  PA3NPG                 1      1  0
009200000626     D* NUMERO    FOGLIO I.M.P.
009300000626     D  PA3NFV                 2      4P 0
009400000626     D  PA3DFV                 5      9P 0
009500000626     D* FLAG DI CREAZIONE SPUNTE DA ORGANIGRAMMA DESCR 148
009600000626     D  PA3AGS                10     10
009700000626     D  PA3FGV                11     15
009800000626      * INDICHERA'IL NUMERO DI VOLTE CHE E' STATA SOTTOMESSA DI SEGUITO
009900000626      *  LA CHIUSURA DEL FV: ALL'INIZIO E' 0
010000000626     D  PA3NUM                17     17  0 INZ
010100000626     D* PA2FLG = "M" ---> RICHIAMATO DALLA GESTIONE F.VIAGGIO
010200000626     D  PA3FLG                19     19    INZ
010300000626     D  L1                    78    137P 0 INZ
010400941007     D                                     DIM(30)
010500000629     D  PA3SUT               138    157
010600000626     D  PA3FEL               158    160  0
010700000626     D  PA3FGS               164    166  0
010800000626     D  PA3CML               167    167
010900010911     D  PA3AGP               168    168
011000040121     D  PA3spv               169    169
011100040121     D  PA3spp               170    170
011200000626     D*
011300020926     D  POG                    1     60P 0 INZ     DIM(30)
011400020916     D*
011500960103     D                 DS
011600960103     D  WAA                    1      4  0
011700960103     D  WMM                    5      6  0
011800960103     D  WGG                    7      8  0
011900960103     D  WDATA                  1      8  0
012000970512     D MMGG            DS
012100900516     D  MM                     1      2  0
012200900516     D  GG                     3      4  0
012300900828     D CA02DS          DS
012400900828     D  PRIMA                378    379B 0
012500941007     D***
012600941007     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
012700941007     D***
012800941007     D WLBDAT          DS                  INZ
012900941007     D  G02DAT                 1      8  0
013000941007     D  G02INV                 9     16  0
013100941007     D  G02ERR                17     17
013200941007     D  G02TGI                18     22  0
013300941007     D***
013400941007     D* PARAMETRI PER TRASFORM GIORNI IN DATA             - XSRDGI -
013500941007     D***
013600941007     D WGIDAT          DS                  INZ
013700941007     D  GIODAT                 1      8  0
013800941007     D  GIOINV                 9     16  0
013900941007     D  GIOTGI                17     21  0
014000020927      * NUMERO RELATIVO RECORD
014100020927     D TARDS1          DS
014200020927     D  VABNR1               397    400B 0
014300020927     D TARDS2          DS
014400020927     D  VABNR2               397    400B 0
014500020927     D TARDS3          DS
014600020927     D  VABNR3               397    400B 0
014700020927     D TARDS6          DS
014800020927     D  VABNR6               397    400B 0
014900080623     D TARDS9          DS
015000080623     D  VABNR9               397    400B 0
015100130121     D TARDS10         DS
015200130121     D  VABNR10              397    400B 0
015300021220
015400021220     d                 ds
015500021220     d VabTba                  1      2  0
015600021220     d VabNba                  3      4  0
015700021220     d VabTb2                  5      6  0
015800021220     d VabNb2                  7      8  0
015900021220     d VabAnt                  1      9  0
016000020927      *
016100900514     D KPJBA         E DS
016200030611      * DS PER TRUL06R - CARICAMENTO £X
016300941007     D DSUL06        E DS                  EXTNAME(TRUL06DS)
016400020926     D  LIN                    1     90  0 DIM(30)
016500030611      * DS PER TRUL90DS - RICHIESTA STAMPANTI
016600030611     D TRUL90DS      E DS
016700991026     D DBLP          E DS
016800060306     D DCLI          E DS
016900911031     D DS3C          E DS
017000970512     D DS3N          E DS
017100020926     D  P3N                   33     89  0 DIM(19)
017200960103     D DS3I          E DS
017300910311     D DS1B          E DS
017400100604     D DS7R          E DS
017500000512     D OG143         E DS
017600000626     D OG148         E DS
017700001006     D OG150         E DS
017800060221      * DS PER FNLV19R - PER SCRITTURA Fiar400F
017900060221     D*** DSLV19        E DS                  EXTNAME(FNLV19DS)
018000030611      * DS PER TIBS02R - GESTIONE TNTBE00F
018100000525     D DSBS02        E DS                  EXTNAME(TIBS02DS)
018200991026     D DTASV         E DS                  INZ
018300020910     d FNLSE5DS      e ds
018400051207     d tntb50ds      e ds
018500100517     d trulNRSds     e ds
018600080110      * - Aggiunta/Sottrazione di n giorni lavorativi ad una data
018700080110     d TRUL52ds      e ds
018800031008     d Fnls80ds      e ds
018900031008     d  dPog                   1     90  0 Dim(30)
019000020910      *------------
019100020910      * DS DATI UTENTE
019200020910      *------------
019300020910     d tibs34ds      e ds                  inz
019400020910     d ddatiute      e ds
019500020910     d azuteds       e ds                  extname(AZUTE00F)
019600020910      *
019700010219     D DFAR          E DS
019800010219     D  §FARDS       E                     EXTFLD(D§FARDES)
019900010219     D  §FARFB       E                     EXTFLD(D§FARFFB)
020000010716     D  §FARNF       E                     EXTFLD(D§FARNONF)
020100010716     D  §FARMA       E                     EXTFLD(D§FARMAN)
020200010716     D  §FARST       E                     EXTFLD(D§FARSTP)
020300010716     D  §FARAF       E                     EXTFLD(D§FARAFVV)
020400010716     D  §FARCF       E                     EXTFLD(D§FARCFVV)
020500011025     D  §FARNO       E                     EXTFLD(D§FARNOO)
020600011025     D  §FARUV       E                     EXTFLD(D§FARUVA)
020700011025     D  §FARDV       E                     EXTFLD(D§FARDVA)
020800041202     D Dvpo          E DS
020900070803     D Dvpodeco      E DS
021000110419     d dVpols01      e ds
021100110419     D  fio                    1     60  0 dim(20)
021200061122     D Dvate         E DS
021300010219     D* DS RICHIAMO PROGRAMMA RICERCA O.R.M.
021400010219     D DSOR04        E DS                  EXTNAME(FIOR04DS)
021500110419     D fior58ds      E DS
021600060130      * ds x richiamo pgm FNLS25R
021700060130     d Fnls25ds      e ds
021800010219     D*
021900010219     D* DS PER RICHIAMO FIOR07R PER VISUALIZZAZIONE ORM
022000010219     D FIOR07          DS
022100010219     D  D07POE                 1      3  0
022200010219     D  D07NOR                 4     10  0
022300010219     D  D07NSR                11     12  0
022400010219     D  D07NRV                13     14  0
022500010219     D  D07SCE                15     15
022600010219     D  D07FGS                16     18  0
022700010219     D  D07POR                19     21  0
022800010219     D  D07DTR                22     29  0
022900010219     D  D07MDB                30     39
023000010219     D  D07PRB                40     49
023100010219     D  D07DTS                50     57  0
023200010219     D  D07RMP                58     58
023300010219     D  D07BRC                59     59
023400010219     D  D07REF                60     61
023500010219     D  D07MIO                62     62
023600010219     D                 DS
023700010219     D  VI4POE                 1      3  0
023800010219     D  VI4NSR                 4      5  0
023900010223     D  VI4NO2                 6     12  0
024000010223     D  VI4NOR                 4     12  0
024100010219     D  VI4NRV                13     14  0
024200010227     D  WIDORM                 4     14  0
024300010227     D  VIDORM                 1     14  0
024400021203
024500021203     D kpdpdr          S                   LIKE(APDpdr)
024600021203     D kpdtip          S                   LIKE(APDtip)
024700070906     d s_drt           s                   like(vi4drt)
024800081215     d ntwpor          s                   like(§ogntw)
024900111216     D preccm          S                   LIKE(vabccm)
025000111216     D prelnp          S                   LIKE(vablnp)
025100111216     D wctrs           S              1
025200120710     D wctrctm         S              1
025300120710     D w1b             S             13
025400120831     d wconf           s              1    inz(*Off)
025500120903     d w0030           s              3  0
025501140110     d posl            s              2  0
025502140110     d posr            s              2  0
025503140110     d len             s              2  0
025504140110
025505140110     d charnook        C                   const('.:,;+<> ')
025600930609     C*---------------------------------------------------------------*
025700900518     C*  RIEPILOGO INDICATORI
025800930609     C*---------------------------------------------------------------*
025900930609     C* 01    - CLIENTE CON PIU' DI 9900 BOLLE
026000900807     C* 02-03 - INDICANO IMMISSIONE CODICE CLIENTE E RIF,MITT IN 1 VID.
026100901116     C* 04    - POSIZIONAMENTO CURSORE PRIMO GIRO SU KSC
026200910916     C* 05    - KSC+RIFERIM MA PIU' DI UNA BOLLA
026300920410     C* 06    - NON ESISTE TABELLA DEL CLIENTE
026400950321     C* 07    - NON VISUALIZZO  TUTTA LA BOLLA SE CONFERMA CON F6
026500000704     C* 08    - INDICA IMMISSIONE NUMERO SERIE IN CONFERMA TOTALE BOLLE
026600911031     C* 10-11 - USATO A VIDEO
026700911031     C* 12    - PRESENTI DEGLI ERRORI
026800001128     C* 16    - DISABILITO I COMANDI DI SELEZIONE E ANNULLAMENTO TOTALE
026900900518     C* 17-19 - INDICANO IL TIPO DI ARCHIVIO(IN PART.;PART.;ENTRAM)SCEL
027000900518     C* 20    - PULISCO SCOCOLLO AL SE RIEMPITO SOLO PER CONFRONTO
027100040604     c* 27    - Errore
027200900518     C* 30    - FINE LETTURA FILE
027300900518     C* 31    - COMODO
027400900518     C* 32    - BOLLA LETTA DA SCARTARE
027500960319     C* 33    - COMODO
027600920203     C* 34    - LETTURA SUBFILE PER RECORD SCELTI
027700080204     C* 37    - COMODO
027800060306     C* 39-79 - ERRORI
027900900518     C* 90    - INDICA LA PRESENZA DI UN ERRORE
028000900808     C* 91    - TORNO A VIDEATA UNO SE CMD12 DA RICHIESTA DI BOLLA SING
028100900808     C* 98-99 - ERRORE
028200930609     C*---------------------------------------------------------------*
028300941010     C* RICHIESTA STAMPANTI
028400030611     C                   CLEAR                   trul90ds
028500941010     C*
028600030611     C                   MOVEL     'S'           D90RSE
028700030611     C                   MOVEL     'S'           D90RSB
028800030611     C                   CALL      'TRUL90R'
028900941010     C                   PARM                    KPJBA
029000030611     C                   PARM                    trul90ds
029100941010     C**
029200941010     C* F3 - FINE
029300030611     C     D90F3         IFEQ      '1'
029400941010     C                   GOTO      FINE
029500941010     C                   ENDIF
029600030611      *
029700030611      * OVRPRTF BOLLE A4
029800030611     C                   MOVEA     D90PRB4       CMA4(30)
029900030611     C                   MOVEA     D90MDB4       CMA4(52)
030000030611     C*
030100080710     C                   Z-ADD     110           LUNG             15 5
030200080710     C                   CLEAR                   COMMAN          110
030300030611     C                   MOVEA     CMA4          COMMAN
030400030611     C                   CALL      'QCMDEXC'                            91
030500030611     C                   PARM                    COMMAN
030600030611     C                   PARM                    LUNG
030700030611     C*
030800030611     C   91              GOTO      FINE
030900030611      *
031000030611      * OVRPRTF BOLLE A5
031100030611     C                   MOVEA     D90PRB5       CMA5(30)
031200030611     C                   MOVEA     D90MDB5       CMA5(52)
031300030611     C*
031400030611     C                   Z-ADD     110           LUNG
031500030611     C                   CLEAR                   COMMAN
031600030611     C                   MOVEA     CMA5          COMMAN
031700030611     C                   CALL      'QCMDEXC'                            91
031800030611     C                   PARM                    COMMAN
031900030611     C                   PARM                    LUNG
032000030611     C*
032100030611     C   91              GOTO      FINE
032200991201     C*
032300991201     C* OVRPRTF SEGNACOLLI
032400030611     C                   MOVEA     D90PRE        CM3(28)
032500030611     C                   MOVEA     D90MDE        CM3(50)
032600941010     C*
032700030611     C                   Z-ADD     103           LUNG
032800941010     C                   CLEAR                   COMMAN
032900991201     C                   MOVEA     CM3           COMMAN
033000941011     C                   CALL      'QCMDEXC'                            91
033100941010     C                   PARM                    COMMAN
033200941010     C                   PARM                    LUNG
033300941010     C*
033400950112     C  N91              OPEN      FNLV22P
033500941010     C   91              GOTO      FINE
033600950407     C*
033700941010     C**
033800901116     C     INIZIO        TAG
033900900827     C                   Z-ADD     0             VI4RMN
034000080623     C******             ADD       1             VI4NSP
034100950419     C                   CLEAR                   WSANRR
034200000515     C                   CLEAR                   TOTA
034300030409     c                   Clear                   wdcr
034400071218
034500071218      * se deve mantenere la data consegna richiesta non devo pulire
034600071218     c                   if        §clidcr <> 'S'
034700071218     c                   Clear                   Vi4Tcr
034800071218     c                   Clear                   Vi4Dcr
034900071218     c                   Clear                   Vi4Hcr
035000071218     c                   Clear                   Vi4Ore
035100071219     c                   eval      s_vi4tcr = vi4tcr
035200071219     c                   eval      s_vi4dcr = vi4dcr
035300071219     c                   eval      s_vi4hcr = vi4hcr
035400071219     c                   eval      s_vi4ore = vi4ore
035500071218     c                   endif
035600071218
035700071218     c                   clear                   $conta
035800941011     C*
035900010221     C                   EXSR      ORMOBB
036000010221     C* SE ATTIVATA PROCEDURA ORM MI POSIZIONO SU NUMERO ORM
036100010228     C* SE NON IMMESSO
036200010221     C     WOBBL         IFEQ      'S'
036300010228     C     VIDORM        ANDEQ     *ZEROS
036400010221     C                   SETON                                        82
036500010221     C                   ELSE
036600010221     C* SE NON IMMESSO PADRONCINO, MI RIPOSIZIONO LI
036700941011     C     VI4PDR        IFEQ      *ZEROS
036800941011     C     VI4PDR        OREQ      *BLANKS
036900941011     C                   SETON                                        04
037000941011     C                   ENDIF
037100010221     C                   ENDIF
037200921005     C*
037300900509     C     FOR01         TAG
037400941007     C                   EXFMT     LS66D01
037500990107     C                   SETOFF                                       285364
037600110419     C                   SETOFF                                       6104
037700920324     C** CMD3 - FINE LAVORO
037800900509     C   KC              GOTO      FINE
037900000522     C** CMD18- CAMBIO P.O. GESTIONE
038000060221     C** KS              MOVEL     'S'           FLGF18
038100060221     C** KS              Z-ADD     VI4FGS        POGES
038200060221     C** KS              GOTO      FINE
038300010219     C* CMD14 - INTERROGAZIONE O.R.M.
038400010223     C     *INKH         IFEQ      *ON
038500010220     C     VI4NOR        ANDEQ     *ZEROS
038600110419    2c                   if        %lookup(vi4lpd:fio)=0
038700110505     c                             and %lookup(999:fio)=0
038800010219     C                   CLEAR                   DSOR04
038900010219     C                   MOVEL     '1'           I04RIC
039000010219     C                   Z-ADD     VI4LPD        I04POR
039100010219     C                   CALL      'FIOR04R'
039200010219     C                   PARM                    KPJBA
039300010219     C                   PARM                    DSOR04
039400010219     C     O04CM3        IFEQ      *BLANKS
039500010219     C                   Z-ADD     O04POE        VI4POE
039600010219     C                   Z-ADD     O04NSR        VI4NSR
039700010223     C                   Z-ADD     O04NOR        VI4NO2
039800010219     C                   Z-ADD     O04NRV        VI4NRV
039900010226     C                   ENDIF
040000110419   x2c                   else
040100110419    3c                   if        vi4pdr<=*zeros
040200110419     c                   seton                                        0428
040300110419     c                   setoff                                       82
040400110419     c                   movel     err(28)       vidmsg
040500110419   x3c                   else
040600110419     c                   clear                   fior58ds
040700110419     c                   movel     vi4lpd        ior58pdr
040800110419     c                   move      vi4pdr        IOR58PDR
040900110419     c                   CALL      'FIOR58R'
041000110419     c                   PARM                    KPJBA
041100110419     c                   PARM                    fior58ds
041200110419    4c     OOR58F03      IFEQ      *BLANKS
041300110419     c                   Z-ADD     OOR58POE      VI4POE
041400110419     c                   z-add     OOR58NSR      VI4Nsr
041500110419     c                   z-add     OOR58NOR      VI4no2
041600110419     c                   Z-ADD     OOR58NRV      VI4NRV
041700110419    4c                   endif
041800110419    3c                   endif
041900110419    2c                   endif
042000010219     C                   GOTO      FOR01
042100010219     C                   ENDIF
042200900509     C* CONTROLLI FORMATO1
042300941010     C                   EXSR      CTRD01
042400110419     C   90              SETOFF                                       82
042500900509     C   90              GOTO      FOR01
042600071218
042700071218      * se cliente modificato pulisco i dati relativi alla
042800071218      * data consegna richiesta
042900071219      * se rimasta uguale a quella precedente
043000071219      * e quindi riemetto il video
043100071218     c                   if        vi4ksc <> s_vi4ksc
043200071219     c                   eval      s_vi4ksc = vi4ksc
043300071219     c                   if        vi4tcr = s_vi4tcr and
043400071219     c                             vi4dcr = s_vi4dcr and
043500071219     c                             vi4hcr = s_vi4hcr and
043600071219     c                             vi4ore = s_vi4ore and
043700071219     c                             (vi4dcr > 0 or vi4hcro > 0 or
043800071219     c                              vi4hcrm > 0 or vi4ore > 0)
043900071218     c                   Clear                   Vi4Tcr
044000071218     c                   Clear                   Vi4Dcr
044100071218     c                   Clear                   Vi4Hcr
044200071218     c                   Clear                   Vi4Ore
044300071218     c                   goto      for01
044400071219     c                   endif
044500071219      * mi salvo la data consegna richiesta
044600071219     c                   eval      s_vi4tcr = vi4tcr
044700071219     c                   eval      s_vi4dcr = vi4dcr
044800071219     c                   eval      s_vi4hcr = vi4hcr
044900071219     c                   eval      s_vi4ore = vi4ore
045000071219     c                   endif
045100031008
045200031008      * F10 - Riepilogo
045300031008     c                   If        *InKj
045400031008     c                   Clear                   Fnls80ds
045500031008     c                   Movea     Pog           dpog
045600031008     c                   Movel     Vi4Nrs        d80Nsr
045700031008     c                   Movel     Vi4Ksc        d80Ccm
045800050923     c                   Movel     Vi4lna        d80lna
045900031008     c                   Call      'FNLS80R'
046000031008     c                   Parm                    Kpjba
046100031008     c                   Parm                    Fnls80ds
046200031008     c                   GoTo      For01
046300031008     c                   EndIf
046400020909      *
046500000512     C* CMD6 CONFERMA BOLLE CLIENTE E CMD16 ANNULLAMENTO BOLLE CLIENTE
046600000512     C* ESEGUO LE ROUTINE E TORNO ALLA RIEMISSIONE DEL FORMATO
046700000512     C     *INKF         IFEQ      *ON
046800000512     C     *INKQ         OREQ      *ON
046900000515     C*
047000000515     C                   MOVEL     'S'           TOTA              1
047100000512     C* ROUTINE CHE LEGGE DAL FILE VAB ED ESEGUE O CONFERMNA O ANNULLAMENTO
047200000512     C                   EXSR      SUBTOT
047300000718     C                   CLEAR                   VI4KSC
047400000512     C*
047500000512     C* SE CI SONO ERRORI GESTIONE DEL SUBFILE ERRORI
047600000518     C   12              SETON                                        05
047700000512     C   12              GOTO      FORCT2
047800000512     C* SE NON CI SONO ERRORI RIEMETTO LA PRIMA VIDEATA
047900000512     C                   GOTO      INIZIO
048000000512     C*
048100000515     C                   ELSE
048200000515     C                   CLEAR                   TOTA
048300000512     C                   ENDIF
048400900511     C* ELABORAZIONE
048500900808     C     ELATAG        TAG
048600900515     C                   EXSR      ELAB
048700111215     c                   if        *in28=*on
048800111215     C                   GOTO      FOR01
048900111215     c                   endif
049000900808     C*
049100930909     C* 43 ON - SE NON CI SONO RECORD DA CARICA ERRORE
049200900516     C     NRR           IFEQ      0
049300900808     C                   SETON                                        43
049400900516     C                   GOTO      FOR01
049500900516     C                   END
049600941010     C* POSIZIONAMENTO CURSORE
049700110419     C                   SETOFF                                       82
049800900828     C*
049900930909     C* SE RMN  E CE N'E' SOLO UNO VADO SUBITO NEL DETTAGLIO
050000930909     C   03NRR           IFEQ      1
050100950905     C     *IN05         ANDEQ     *OFF
050200910227     C*
050300050407     C                   MOVEL     VI4SCE        VI5SCE
050400900828     C                   EXSR      CONF
050500950206     C*
050600950206     C* SE C'E' ERRORE RITORNO CON LA "V"
050700950419     C* QUESTO CASI NON SI DOVREBBE PIU' VERIFICARE
050800950206     C     *IN90         IFEQ      *ON
050900950407     C* SE ERRORE PRIMA DI ENTRARE NELLA BOLLA --> VISUALIZZO IL MESSAG
051000950407     C     WBOL          IFEQ      ' '
051100950407     C                   GOTO      FOR01
051200950407     C                   ELSE
051300050407     C                   MOVEL     'V'           VI5SCE
051400950206     C                   EXSR      CONF
051500950206     C                   ENDIF
051600950407     C                   ENDIF
051700911031     C*
051800900828     C                   GOTO      INIZIO
051900900828     C                   END
052000911031     C*
052100911031     C                   SETON                                        05
052200950419     C*
052300950419     C* SE IMPOSTATO LO TENGO
052400950419     C     WSANRR        IFGT      0
052500951002     C     WSANRR        IFLE      SAVNRR
052600950419     C                   Z-ADD     WSANRR        REC
052700951002     C                   ELSE
052800951002     C                   Z-ADD     SAVNRR        REC
052900951002     C                   ENDIF
053000951002     C                   CLEAR                   WSANRR
053100950419     C                   ENDIF
053200900828     C*
053300900515     C** CARICAMENTO SUBFILE
053400900518     C     FORCT2        TAG
053500020917      *
053600020917      * Prima di caricare il sfl richiamo il Pgm per pulizia bolle obsolete
053700020917      * solo se la prima chiamata allo stesso pgm è andata a buon fine
053800020918     C                   IF        OE5ERR <> 'E'
053900020917     C                   CLEAR                   FNLSE5DS
054000020918     C                   Z-ADD     VI4FGS        IE5POU
054100020917     C                   CALL      'FNLSE5R'
054200020917     C                   PARM                    FNLSE5DS
054300020917     C                   ENDIF
054400020917      *
054500000518     C                   CLEAR                   TOTA
054600001128     C* SE CODICE CLIENTE A ZERO OPPURE NON È STATA INSERITA LA ZONA
054700001128     C* DISABILITO IL CMD 6 E CMD 16
054800001128     C                   SETOFF                                       16
054900001128     C*
055000001128     C     VI4KSC        IFEQ      *ZEROS
055100001128     C     VI4NRS        IFEQ      99
055200111221     C     VI4NRS        oreq      00
055300001128     C                   SETON                                        16
055400001128     C                   ENDIF
055500001128     C                   ENDIF
055600001128     C*
055700941007     C                   WRITE     LS66D02
055800900515     C** COMANDI SUBFILE
055900900515     C     FORCT         TAG
056000941007     C                   EXFMT     LS66DCT
056100950112     C* F3 - FINE
056200900515     C   KC              GOTO      FINE
056300950112     C* F12 - RITORNO
056400900517     C   KL              GOTO      INIZIO
056500950112     C* F16 - CONFERMA ANNULLAMENTO
056600910225     C   KQ              EXSR      ANNULL
056700910225     C   KQ              GOTO      ELATAG
056800900808     C*
056900950112     C* F6 - SELEZIONE TOTALE
057000950112     C   KF              DO
057100941010     C                   EXSR      CTRD02
057200930909     C   90              GOTO      FORCT
057300930909     C*
057400900827     C     SUB           TAG
057500941007     C                   READC     LS66DSF                                34
057600900827     C  N34VI5SCE        IFNE      ' '
057700900827     C                   SETON                                        98
057800900827     C                   GOTO      FORCT
057900900827     C                   ELSE
058000900827     C                   GOTO      SUB
058100900827     C                   END
058200900808     C*
058300900808     C                   EXSR      SELEZ
058400911031     C* ERRORI
058500911031     C   12              GOTO      FORCT2
058600911031     C*
058700900808     C* SE FINITO TUTTO OK VADO A VIDEATA 1
058800900808     C                   GOTO      INIZIO
058900900808     C                   END
059000900824     C*
059100900516     C** CONTROLLO SE EFFETTUATA UNA SCELTA
059200941007     C                   READC     LS66DSF                                34
059300911031     C   34              Z-ADD     PRIMA         REC
059400950419     C                   SETOFF                                       90
059500911031     C*
059600930909    1C  N34VI5SCE        IFNE      *BLANKS
059700941010     C                   EXSR      CTRD02
059800930909     C                   ENDIF
059900930909     C   90
060000911031     COR 34              GOTO      FORCT
060100911031     C*
060200930909     C* VISUALIZZO BOLLA PER BOLLA
060300911031     C     *IN34         DOWEQ     '0'
060400950419     C*
060500950419     C* SALVO IL NUMERO DI RECORD CHE HO
060600950419     C                   Z-ADD     PRIMA         WSANRR
060700911031     C*
060800910222     C     VI5SCE        IFEQ      'X'
060900910222     C     VI5SCE        OREQ      'V'
061000900808     C                   EXSR      CONF
061100900808     C                   END
061200950419     C*
061300000517     C* SE E' STATO DATO KC NEL PGM FNLS01R VADO FUORI PROGRAMMA
061400000517     C     §LPFPR        IFNE      '3'
061500941007     C                   READC     LS66DSF                                34
061600000517     C                   ELSE
061700000517     C                   SETON                                            34
061800000517     C                   END
061900000517     C*
062000911031     C                   END
062100950419     C*
062200911031     C* ERRORI EVIDENZIO QUELLE ERRATE
062300911031     C   12              GOTO      FORCT2
062400911031     C*
062500911031     C* FINE LETTURA RIEMETTO SUBFILE AGGIORNATO
062600930909     C   03              GOTO      FORCT2
062700911122     C                   GOTO      ELATAG
062800000000     C     FINE          TAG
062900930628     C*
063000930628     C     RICH          IFEQ      '1'
063100930628     C                   MOVEL     'C'           §LPFPR
063200991026     C                   CLEAR                   DTASV
063300020910     C* P.O DI GESTIONE
063400020917     C                   MOVE      VI4FGS        §LPFGS
063500020910      *
063600941011     C                   CALL      'FNLS01R'
063700930628     C                   PARM                    KPJBA
063800991026     C                   PARM                    DBLP
063900991026     C                   PARM                    DTASV
064000030718     C                   PARM                    trul90ds
064100930628     C                   END
064200000626     C*
064300060221     C***  RICLSA        IFEQ      'S'
064400000629     C*
064500060221     C***                CLEAR                   KPJBU
064600060221     C***                MOVEL     '99999'       PA3FGV
064700000626     C*
064800060221     C****               MOVEL     SIMFEL        WORG
064900000626     C*
065000060221     C****               EXSR      CTRORG
065100060221     c***                eval      og150=orgdf0
065200000626     C*
065300060221     C*                  TESTN                   §ogvol               30
065400000626     C*
065500060221     C*    *IN30         IFEQ      *ON
065600060221     C*    §ogvol        ANDGT     *ZEROS
065700060221     C*                  MOVEL     'S'           PA3CML
065800060221     C*                  ENDIF
065900040121     c*
066000060221     c*                  eval      pa3spv=§ogspv
066100060221     c*                  eval      pa3spp=§ogspp
066200000626     C*
066300060221     C*                  MOVEL     ORGDE8        OG148
066400000626     C*
066500060221     C*    §OGAGS        IFEQ      'E'
066600060221     C*                  MOVEL     'S'           PA3AGS
066700060221     C*                  MOVEL     §OGAGP        PA3AGP
066800060221     C*                  ELSE
066900060221     C*                  MOVEL     ' '           PA3AGS
067000060221     C*                  CLEAR                   PA3AGP
067100060221     C*                  ENDIF
067200000626     C*
067300060221     C*                  MOVEL     SIMFEL        PA3FEL
067400060221     C*                  MOVEL     VIDDSA        PA3SUT
067500000626     C*
067600060221     C*                  Z-ADD     *ZEROS        PA3NFV
067700060221     C*                  Z-ADD     *ZEROS        PA3DFV
067800000626     C*
067900060221     C*                  MOVEL     'KSDIFFE'     KQEBI
068000060221     C*                  MOVE      'PT '         KQEBI
068100060221     C*                  MOVEL     'KSDIFFE'     KQIEB
068200060221     C*                  MOVE      'PT '         KQIEB
068300060221     C*                  MOVEL     PARAM3        KPJBU
068400060221     C*                  MOVEL     'LSA2'        KCOAZ
068500060221     C*                  CALL      'BCH10'
068600060221     C*                  PARM                    KPJBA
068700000824     C* INVIO FILE TRASMISSIONE
068800060221     C*                  CLEAR                   KPJBU
068900060221     C*                  MOVEL     'LSA3'        KCOAZ
069000060221     C*                  CALL      'BCH10'
069100060221     C*                  PARM                    KPJBA
069200000626     C*
069300060221     C*                  ENDIF
069400020909      *
069500020909      * Prima di fine richiamo il Pgm per pulizia bolle obsolete
069600020909      * il modalità 'C'
069700020918     C                   IF        OE5ERR <> 'E'
069800020909     C                   CLEAR                   FNLSE5DS
069900020909     C                   MOVE      'C'           IE5TLA
070000020909     C                   CALL      'FNLSE5R'
070100020909     C                   PARM                    FNLSE5DS
070200020909     C                   ENDIF
070300060926     C*
070400060926     C                   CLEAR                   DSLV55
070500060926     C                   MOVEL     'C'           D55TLA
070600060926     C                   CALL      'FNLV55R'
070700060926     C                   PARM                    DSLV55
070800000000     C                   SETON                                        LR
070900940201     C*
071000941007     C* SUBROUTINE INIZIALE ------------------------------------------*
071100941007     C     *INZSR        BEGSR
071200950127     C*
071300950127     C     *ENTRY        PLIST
071400950127     C                   PARM                    KPJBA
071500950127     C**
071600950127     C* KLIST
071700950127     C**
071800941007     C     KTAB          KLIST
071900941007     C                   KFLD                    CODUT
072000941007     C                   KFLD                    COD               2
072100941007     C                   KFLD                    KEY               8
072200950407     C     KTAB2         KLIST
072300950407     C                   KFLD                    CODUT
072400950407     C                   KFLD                    COD               2
072500960103     C* ACCESSO   AZCLN
072600960103     C     KCLN          KLIST
072700960103     C                   KFLD                    SIMFEL
072800960103     C                   KFLD                    W0030
072900960103     C                   KFLD                    WAA               4 0
073000960103     C                   KFLD                    WMM               2 0
073100941007     C     KBLP          KLIST
073200960103     C                   KFLD                    AAS
073300941007     C                   KFLD                    VI5LNP
073400941007     C                   KFLD                    VI5NRS
073500941007     C                   KFLD                    VI5NSP
073600941007     C     KACO          KLIST
073700941007     C                   KFLD                    CODUT
073800020910     C                   KFLD                    DUTKCI
073900941007     C                   KFLD                    KSC               7 0
074000941010     C     KVAB          KLIST
074100941007     C                   KFLD                    VI4KSC
074200941007     C                   KFLD                    VI4RMN
074300080623     C***  KVAB2         KLIST
074400080623     C***                KFLD                    VI5FGS
074500080623     C***                KFLD                    VI5KSC
074600080623     C***                KFLD                    VI5RMN
074700941007     C     KBLD          KLIST
074800020910     C                   KFLD                    VABFGS
074900020910     C                   KFLD                    VABCCM
075000941010     C                   KFLD                    VABAAS
075100941010     C                   KFLD                    VABLNP
075200941010     C                   KFLD                    VABNRS
075300941010     C                   KFLD                    VABNSP
075400941007     C     KBLD2         KLIST
075500020917     C                   KFLD                    VI5FGS
075600941007     C                   KFLD                    VI5KSC
075700941007     C                   KFLD                    VI5AAS
075800941007     C                   KFLD                    VI5LNP
075900941007     C                   KFLD                    VI5NRS
076000941007     C                   KFLD                    VI5NSP
076001130313     C     Kvat2         KLIST
076002130313     C                   KFLD                    VI5FGS
076003130313     C                   KFLD                    VI5KSC
076004130313     C                   KFLD                    VI5AAS
076005130313     C                   KFLD                    VI5LNP
076006130313     C                   KFLD                    VI5NRS
076007130313     C                   KFLD                    VI5NSP
076008130313     C                   KFLD                    WTRC
076100950127     C     KFTT          KLIST
076200950127     C                   KFLD                    W01PDR
076300950127     C                   KFLD                    W01FVL
076400950127     C                   KFLD                    COMDRT
076500950127     C                   KFLD                    W01TSR
076600060221     C***  KBL4          KLIST
076700060221     C***                KFLD                    VABAAS
076800060221     C***                KFLD                    VABLNP
076900060221     C***                KFLD                    VABNRS
077000060221     C***                KFLD                    VABNSP
077100060221     C***                KFLD                    WTRC
077200060221     C***  KBLPPT        KLIST
077300060221     C***                KFLD                    BLTAAS
077400060221     C***                KFLD                    BLTLNP
077500060221     C***                KFLD                    BLTNRS
077600060221     C***                KFLD                    BLTNSP
077700060221     C***  KBL4PT        KLIST
077800060221     C***                KFLD                    BLTAAS
077900060221     C***                KFLD                    BLTLNP
078000060221     C***                KFLD                    BLTNRS
078100060221     C***                KFLD                    BLTNSP
078200060221     C***                KFLD                    WPTTRC
078300951010     C     KBLP2         KLIST
078400951010     C                   KFLD                    VABAAS
078500951010     C                   KFLD                    VABLNP
078600951010     C                   KFLD                    VABNRS
078700951010     C                   KFLD                    VABNSP
078800000523     C     KBLT          KLIST
078900000523     C                   KFLD                    VABLNP
079000000523     C                   KFLD                    VABNRS
079100000523     C                   KFLD                    VABNCD
079200001214     C*
079300010219     C     KORM          KLIST
079400010219     C                   KFLD                    VI4POE
079500010219     C                   KFLD                    VI4NSR
079600010223     C                   KFLD                    VI4NO2
079700010219     C                   KFLD                    VI4NRV
079800061127     C     KORM_dpo      KLIST
079900061127     C                   KFLD                    dpopoe
080000061127     C                   KFLD                    dponrs
080100061127     C                   KFLD                    dponor
080200061127     C                   KFLD                    dponrv
080300010219     C     KDST          KLIST
080400120607     C                   KFLD                    ormnpg
080500120607     C                   KFLD                    ormndc
080600010219     C                   KFLD                    ORMPOR
080700010220     C     KFVV          KLIST
080800010220     C                   KFLD                    ORMNPG
080900010220     C                   KFLD                    W0050
081000010220     C                   KFLD                    ORMPOR
081100020219      *
081200020219     C     K01TBE        KLIST
081300020219     C                   KFLD                    TBECOD
081400020219     C                   KFLD                    TBEKE1
081500020219      *
081600020219     C     K02TBE        KLIST
081700020219     C                   KFLD                    $COD
081800020219     C                   KFLD                    $LIN
081900020219     C                   KFLD                    $SIF
082000020219     C                   KFLD                    TBEKE1
082100020219      *
082200020219     C     K03TBE        KLIST
082300020219     C                   KFLD                    $COD
082400020219     C                   KFLD                    $LIN
082500020219     C                   KFLD                    $SIF
082600020219     C                   KFLD                    TBEKE1
082700020219     C                   KFLD                    TBEKE2
082800021203     C     Kfapd         klist
082900021203     C                   kfld                    kpdtip
083000021203     C                   kfld                    kpdpdr
083100061122     C     KVAT          KLIST
083200061124     C                   KFLD                    vabfgs
083300061122     C                   KFLD                    §LPCCM
083400061122     C                   KFLD                    §LPAAC
083500061122     C                   KFLD                    §LPLNP
083600061122     C                   KFLD                    §LPNRS
083700061122     C                   KFLD                    §LPNSP
083800061122     C                   KFLD                    WTRC
083900941007     C*
084000060221     C***  *LIKE         DEFINE    BL4TRC        WTRC
084100060221     C**** *LIKE         DEFINE    BL4TRC        WPTTRC
084200061122     C     *LIKE         DEFINE    vattrc        WTRC
084300951010     C     *LIKE         DEFINE    TBLKUT        §KUT
084400941010     C     *LIKE         DEFINE    TBLCOD        §COD
084500941007     C     *LIKE         DEFINE    TBLKEY        §KEY
084600941007     C     *LIKE         DEFINE    VI4KSC        SAVKSC
084700990107     C     *LIKE         DEFINE    VI4NRS        SAVNRS
084800950407     C     *LIKE         DEFINE    VI4KSC        WKSC
084900941007     C     *LIKE         DEFINE    VI5CTM        SAVCTM
085000990107     C     *LIKE         DEFINE    VI5CTM        SA2CTM
085100941007     C     *LIKE         DEFINE    BLPAAS        COMAAS
085200950127     C     *LIKE         DEFINE    FTTTSR        W01TSR
085300950127     C     *LIKE         DEFINE    FTTPDR        W01PDR
085400950127     C     *LIKE         DEFINE    FTTFVL        W01FVL
085500950419     C     *LIKE         DEFINE    REC           WSANRR
085600001006     C     *LIKE         DEFINE    §1BCTC        WWCTC
085700010219     C     *LIKE         DEFINE    VIDORM        SAVORM
085800010219     C     *LIKE         DEFINE    VI4PDR        SA1PDR
085900010219     C     *LIKE         DEFINE    VI4FPP        SA1FPP
086000010219     C     *LIKE         DEFINE    VI4DRT        SA1DRT
086100010219     C     *LIKE         DEFINE    VI4KSC        SA1KSC
086200010219     C     *LIKE         DEFINE    VI4PDR        WDPDR
086300950127     C*
086400950127     C                   MOVEL     'R'           W01TSR
086500950127     C                   MOVEL     'C'           W01FVL
086600950127     C                   Z-ADD     1             CODUT
086700950407     C                   CLEAR                   WKSC
086800060221     C******             MOVEL     'E'           WTRC
086900060221     C******             MOVEL     PT            WRKPT             1
087000060221     C                   clear                   WRKPT             1
087100020910     C                   Z-ADD     1             CODUT             1 0
087200020910      *
087300020910     c     *dtaara       define    §azute        azuteds
087400020910     c     *dtaara       define    §datiute      ddatiute
087500020910     c                   in(E)     *dtaara
087600020910     c                   IF        %error  or RSUT = *blanks
087700020910     c                   CLEAR                   tibs34ds
087800020910     c                   CALL      'TIBS34R'
087900020910     c                   PARM                    tibs34ds
088000020910     c                   in        *dtaara
088100020910     c                   endif
088200020910     C                   MOVEL     RSUT          VIDDSA
088300060221     c*
088400060221     C                   MOVEL     dutpou        VI4FGS
088500020910      *
088600001006     C* MI MEMORIZZO L'AS PER NUMERATORE CHI SONO DPD
088700001006     C                   Z-ADD     SIMFEL        W0030
088800001006     C     W0030         CHAIN     AZORG01L                           31
088900001006     C                   MOVEL     ORGDF0        OG150
089000001006     C                   MOVEL     §OGDKC        WASDKC            1
089100941007     C***
089200941007     C* CARICO TABELLA FILIALI GESTITE £1
089300941007     C***
089400941007     C                   CLEAR                   DSUL06
089500021007     C                   MOVE      'PP'          D06COD
089600000522     C     VI4FGS        IFEQ      *ZEROS
089700941007     C                   MOVEL     SIMFEL        D06KEY
089800000522     C                   ELSE
089900000522     C                   MOVEL     VI4FGS        D06KEY
090000000522     C                   ENDIF
090100941007     C                   MOVEL     DSUL06        KPJBU
090200941007     C*
090300941007     C                   CALL      'TRUL06R'
090400941007     C                   PARM                    KPJBA
090500941007     C                   MOVEL     KPJBU         DSUL06
090600941007     C                   MOVEA     LIN           L1
090700020916     C***
090800020926     C* COSTRUISCO SCHIERA CON P.O GESTITI
090900020926     C* SE P.O. GESTIONE POSTE USO LA SUA £6
091000021007     C* SE NON POSTE TOLGO DALLA £1 I P.O. POSTE
091100020916     C***
091200020926     C     WRKPT         IFNE      'S'
091300020916     C                   z-add     1             Y                 2 0
091400020916     C     Y             DOWLT     30
091500020916     C     L1(Y)         ANDNE     *zeros
091600020916     C                   MOVE      L1(Y)         WORG
091700020916     C*
091800020916     C                   EXSR      CTRORG
091900020918     C     §OGNTW        IFNE      'PPT'
092000020926     C                   add       1             XX                3 0
092100020926     C                   z-add     L1(Y)         POG(xx)
092200020916     c                   ENDIF
092300020918     C                   add       1             Y
092400020916     C                   ENDDO
092500020923      *
092600020923     C                   ELSE
092700020916     C                   CLEAR                   DSUL06
092800020916     C                   MOVE      '£6'          D06COD
092900020916     C     VI4FGS        IFEQ      *ZEROS
093000020916     C                   MOVEL     SIMFEL        D06KEY
093100020916     C                   ELSE
093200020916     C                   MOVEL     VI4FGS        D06KEY
093300020916     C                   ENDIF
093400020916     C                   MOVEL     DSUL06        KPJBU
093500020916     C*
093600020916     C                   CALL      'TRUL06R'
093700020916     C                   PARM                    KPJBA
093800020916     C                   MOVEL     KPJBU         DSUL06
093900020926     C                   MOVEA     LIN           POG
094000020916     C                   ENDIF
094100960103     C***
094200960103     C* CARICO DATI VARIABILI PARTENZE
094300960103     C***
094400960103     C                   MOVEL     '3I'          COD
094500960103     C                   MOVEL     '1       '    KEY
094600960103     C     KTAB          CHAIN     TABEL                              30
094700960103     C  N30              MOVEL     TBLUNI        DS3I
094800960103     C   30              CLEAR                   DS3I
094900941007     C*
095000960103     C* DATA BOLLETTAZIONE:  TRA UDATE E 1 GIORNO LAVORATIVO MAGGIORE
095100960103     C*  SOLO PER LE SPEDIZ.DI RECUPERO E' TRA DATA G. - 3  E  + 1 LAV
095200941007     C                   TIME                    W0140            14 0
095300941007     C* UDATE IN GGMMAAAA
095400941007     C                   MOVE      W0140         WDTGIO            8 0
095500941007     C*
095600941007     C* UDATE IN AAAAMMGG
095700941007     C                   Z-ADD     WDTGIO        G02DAT
095800941007     C                   MOVEL     *BLANK        G02ERR
095900941007     C                   CALL      'XSRDA8'
096000941007     C                   PARM                    WLBDAT
096100941007     C                   MOVEL     G02INV        DATEU             8 0
096200960103     C* SE NON FOSSE PERMESSO BOLLETTARE IN SOSPENSIONE
096300960103     C*  SI PUO' BOLLETTARE SOLO FINO A OGGI
096400960103     C                   Z-ADD     G02INV        DATAPA            8 0
096500960103     C                   MOVEL     G02DAT        DATAPG            8
096600950127     C* SOTTRAGGO  3
096700950127     C     G02TGI        SUB       3             GIOTGI
096800950127     C*
096900950127     C                   CALL      'XSRGI8'
097000950127     C                   PARM                    WGIDAT
097100950127     C*
097200950127     C                   Z-ADD     GIOINV        DATAM             8 0
097300080110     C* SOTTRAGGO  giorni per ottenere data minima per data ritiro
097400080110     c                   clear                   TRUL52ds
097500080110     c                   eval      d52gio=§3iggb
097600080110     c                   eval      d52sgn='-'
097700080110     c                   z-add     dateu         D52din
097800080110     c                   call      'TRUL52R'
097900080110     c                   parm                    TRUL52ds
098000080110     C*
098100080110     C                   Z-ADD     d52dfi        DATAmin           8 0
098200960103     C*
098300960103     C* ADDIZIONO 1 FINTANTO CHE NON TROVO IL PRIMO GIORNO LAVORATIVO
098400020917     C                   Z-ADD     1             X                 2 0
098500960103    1C     X             DOWLE     §3IGGS
098600960103     C*
098700960103     C                   MOVEL     'F'           WFEST             1
098800960103     C*
098900960103    2C     WFEST         DOWNE     ' '
099000960103     C     G02TGI        ADD       1             GIOTGI
099100960103     C                   ADD       1             G02TGI
099200960103     C                   CALL      'XSRGI8'
099300960103     C                   PARM                    WGIDAT
099400960103     C*
099500960103     C                   Z-ADD     GIOINV        DATAPA            8 0
099600960103     C                   MOVEL     GIODAT        DATAPG            8
099700960103     C*
099800960103     C* IMPOSTO ANNO E MESE DELLA DATA TROVATA
099900960103     C                   Z-ADD     GIOINV        WDATA
100000960103     C                   CLEAR                   W0030
100100960103     C* CHAIN SUL CALENDARIO BARTOLINI
100200960103     C     KCLN          CHAIN     AZCLN01L                           30
100300960103     C   30              CLEAR                   CLNPOM
100400960103     C                   MOVEA     CLNPOM        K31
100500960103     C                   MOVEL     K31(WGG)      WFEST
100600960103    2C                   ENDDO
100700960103     C*
100800960103     C                   ADD       1             X
100900960103    2C                   ENDDO
101000020430     C* TESTO SE SONO primo o secondo livello
101100020430     C*                  MOVEL     SIMFEL        VI4LPD
101200020917     c                   z-add     dutpou        vi4lpd
101300000522     C* ABILITO F18
101400060221     C****               SETON                                        09
101500060221     c**** DutLpo        ifeq      '2'
101600060221     c**** DutLpo        oreq      *blanks
101700000522     C* DISABILITO F18
101800060221     C****               SETOFF                                       09
101900000522     C*
102000060221     C*****              END
102100000523     C* SE 09 È SPENTO LO ACCENDO SE SONO UN PO POSTE PER ABILITARE F18
102200060221     C***  *IN09         IFEQ      *OFF
102300060221     C***  WRKPT         ANDEQ     'S'
102400060221     C****               SETON                                        09
102500060221     C****               ENDIF
102600000522     C* SE PO IN GESTIONE È VALORIZZATO SARA' LUI CHE COMANDERA'
102700000522     C     VI4FGS        IFNE      *ZEROS
102800000522     C                   MOVEL     VI4FGS        VI4LPD
102900000523     C                   ELSE
103000000523     C                   MOVEL     VI4LPD        VI4FGS
103100000522     C                   ENDIF
103200000522     C* DECODIFICO
103300000523     C     VI4FGS        CHAIN     AZORG01L                           37
103400000522     C     *IN37         IFEQ      *OFF
103500000522     C     ORGFVA        ANDEQ     ' '
103600000522     C                   MOVEL     ORGDES        DESFGS
103700000522     C                   ENDIF
103800000512     C*
103900940201     C* PULIZIA CAMPI CHE RIMANGONO INVARIATI IL SECONDO GIRO
104000940201     C                   Z-ADD     0             VI4KSC
104100071207     c                   Clear                   Vi4Tcr
104200071207     c                   Clear                   Vi4Dcr
104300071207     c                   Clear                   Vi4Hcr
104400071207     c                   Clear                   Vi4Ore
104500970513     C                   Z-ADD     99            VI4NRS
104600041122     C                   Z-ADD     999           VI4lna
104700941007     C                   MOVEL     *BLANKS       VI4PDR
104800941007     C                   MOVEL     *BLANKS       DESPDR
104900941007     C                   Z-ADD     WDTGIO        VI4DRT
105000941007     C                   MOVEL     'P'           VI4FPP
105100941007     C                   MOVEL     0             VI4NRT
105200941007     C                   Z-ADD     WDTGIO        VI4DSP
105300941007     C                   MOVE      WDTGIO        COMAAS
105400940201     C                   Z-ADD     0             PREKSC
105500080623     C                   clear                   PRERMN
105600940201     C                   MOVEL     '  '          VI4CTM
105700120709     C                   MOVEL     '  '          s_VI4CTM
105800950407     C                   MOVEL     'X'           VI4SCE
105900010219     C                   CLEAR                   VI4POE
106000010219     C                   CLEAR                   VI4NOR
106100010219     C                   CLEAR                   VI4NRV
106200041222     c                   Clear                   vi4ctr
106300080623     c                   movel     'N'           vi4ord
106400010219     C*
106500010219     C* IMPOSTO NUMERO ORM FITTIZIO
106600010227     C                   MOVE      *ALL'9'       ORMFIT           11 0
106700940201     C*
106800000518     C* SE WRKPT = 'S' VENGO DA CONFERMA BOLLE POSTE
106900000518     C     WRKPT         IFEQ      'S'
107000000518     C                   MOVE      TES(3)        VIDTES
107100000518     C                   ELSE
107200940201     C                   MOVE      TES(1)        VIDTES
107300940201     C                   END
107400940201     C*
107500080623     C******             Z-ADD     -1            VI4NSP
107600950407     C***
107700950407     C* CARICO TUTTI I CODIDI TRATTAMENTO MERCE CHE SI ASSEGNANO
107800950407     C*  IL NUMERO DI SPEDIZIONE
107900950407     C**
108000950407     C                   MOVEL     '1B'          COD
108100950407     C                   Z-ADD     1             X
108200950407     C*
108300950407     C     KTAB2         SETLL     TABEL
108400950407     C     KTAB2         READE     TABEL                                  30
108500950407     C*
108600950407     C     *IN30         DOWEQ     *OFF
108700950407     C                   MOVEL     TBLUNI        DS1B
108800950407     C     §1BFG2        IFEQ      'S'
108900950407     C                   MOVEL     TBLKEY        CTM(X)
109000120710     C                   eval      ctmf(X)=%subst(ds1b:26:13)
109100120710     C                   eval      ctm16(X)=§1bf16
109200950407     C                   ADD       1             X
109300950407     C                   ENDIF
109400950407     C*
109500950407     C     KTAB2         READE     TABEL                                  30
109600950407     C                   ENDDO
109700940201     C*
109800950503     C                   CLEAR                   DS1B
109900950503     C                   CLEAR                   SAVCTM
110000020219     C*
110100020219     C* RECUPERO SISTEMA INFORMATIVO DELLA TABELLA 3NN
110200020219     C                   MOVE      *BLANKS       TBECOD
110300020219     C                   MOVE      *ZEROS        TBEKE1
110400020219     C                   MOVE      '3NN'         $COD              3
110500020219     C                   MOVE      $COD          TBEKE1
110600020219     C     K01TBE        CHAIN     TNTBE01L                           31
110700020219     C                   MOVEL     TBESIF        $SIF             10
110800020219     C                   MOVEL     TBELIN        $LIN              1
110900020219     C                   MOVEL     TBEAPL        $APL              2
111000020219     C*
111100020909     C* Richiamo Pgm per pulizia bolle obsolete, passo il P.O di gestione
111200020909     C                   CLEAR                   FNLSE5DS
111300020918     C                   Z-ADD     VI4FGS        IE5POU
111400020909     C                   CALL      'FNLSE5R'
111500020909     C                   PARM                    FNLSE5DS
111600070803      * Aggancio tabella "VPODECO" per reperimento data attivazione
111700070803      * data ritiro da orm
111800070803     C                   CLEAR                   dsbs02
111900070803     c                   clear                   dvpodeco
112000070803     C                   MOVEL     'C'           T02MOD
112100070803     C                   MOVEL     KNSIF         T02SIF
112200070803     C                   MOVEL     'VPO'         T02COD
112300070803     c                   movel(P)  'DECO'        t02ke1
112400070803     C                   CALL      'TIBS02R'
112500070803     C                   PARM                    KPJBA
112600070803     C                   PARM                    dsbs02
112700070803    2C                   IF        T02ERR = *BLANKS
112800070803     C                   MOVEL     T02uni        dvpodeco
112900070803    2C                   ENDIF
113000041202     C* reperisco da tabella vpo il numero giorni da sommare a udate per
113100041202     c* determinare data max per data consegna richiesta
113200041202     C                   CLEAR                   DSBS02
113300041202     C                   MOVEL     'C'           T02MOD
113400041202     C                   MOVEL     KNSIF         T02SIF
113500041202     C                   MOVEL     'VPO'         T02COD
113600041202     C                   MOVEL(p)  'VPO'         T02KE1
113700041202     C                   CALL      'TIBS02R'
113800041202     C                   PARM                    KPJBA
113900041202     C                   PARM                    DSBS02
114000041202    4C     T02ERR        IFEQ      *BLANKS
114100041202     C                   MOVEL     T02UNI        DVPO
114200041202     C                   ELSE
114300041202     C                   CLEAR                   DVPO
114400041202    4C                   ENDIF
114500030415      * Calcolo oggi + 5 gg.
114600051215     c**                 Move      Dateu         Dataiso
114700051215     c**                 Adddur    §vpodcg:*d    Dataiso
114800051215     c***                Move      Dataiso       woggi5
114900051215     c*
115000051215     c                   Move      Dateu         Dataiso
115100051215     c                   Adddur    §vpodcgm1:*d  Dataiso
115200051215     c                   Move      Dataiso       woggidcgm1
115300051215     c*
115400051215     c                   Move      Dateu         Dataiso
115500051215     c                   Adddur    §vpodcgm2:*d  Dataiso
115600051215     c                   Move      Dataiso       woggidcgm2
115700110419      * Aggancio tabella "VPOLS01" per reperimento filiali che utilizzano
115800110419      * nuova ricerca O.R.M.
115900110419     C                   CLEAR                   DSBS02
116000110419     C                   MOVEL     'C'           T02MOD
116100110419     C                   MOVEL     KNSIF         T02SIF
116200110419     C                   MOVEL     'VPO'         T02COD
116300110419     C                   MOVEL(p)  'LS01'        T02KE1
116400110419     C                   CALL      'TIBS02R'
116500110419     C                   PARM                    KPJBA
116600110419     C                   PARM                    DSBS02
116700110419     C     T02ERR        IFEQ      *BLANKS
116800110419     C                   MOVEL     T02UNI        DVPOls01
116900110419     C                   ELSE
117000110419     C                   CLEAR                   DVPOls01
117100110419     C                   ENDIF
117200030409
117300940201     C                   ENDSR
117400900509     C*
117500900509     C*--- CONTROLLI FORMATO1 ----------------------------------------*
117600941010     C     CTRD01        BEGSR
117700900807     C                   SETOFF                                       900203
117800000717     C                   SETOFF                                       051208
117900030411     C                   SETOFF                                       588586
118000030415     C                   SETOFF                                       878889
118100080110     c                   setoff                                       6752
118200930909     C                   CLEAR                   COMDSP
118300930909     C                   CLEAR                   COMDRT
118400941007     C                   CLEAR                   DESPDR
118500000718     C                   CLEAR                   W0070
118600120903     C***                CLEAR                   CLI
118700120903     C***                CLEAR                   C
118800120903     C***                CLEAR                   Z
118900060306     c                   clear                   dcli
119000120710     c                   clear                   wctrctm
119100000512     C* SE E' STATO PREMUTO F6 O F16 FACCIO DEI CONTROLLI AGGIUNTIVI
119200000512     C* OBBLIGATORIETA' DEL CODICE CLIENTE SEMPRE
119300000512     C     *INKF         IFEQ      *ON
119400000512     C     *INKQ         OREQ      *ON
119500000705     C* SE IL P.O. GESTIONE NON E' POSTE OBBLIGATORIO IL CLIENTE
119600000512     C     VI4KSC        IFEQ      *ZEROS
119700000705     C     WRKPT         ANDNE     'S'
119800000512     C                   SETON                                        6990
119900000512     C                   GOTO      ENDCTR
120000000512     C                   ENDIF
120100000705     C* SE INVECE SONO UN P.O. POSTE POSSO ESEGUIRE ANCHE CON LA SOLA SERIE
120200120903     C*    WRKPT         IFEQ      'S'
120300120903     C*                  SELECT
120400000705     C*
120500120903     C*    VI4KSC        WHENGT    *ZEROS
120600000705     C*
120700120903     C*    VI4NRS        WHENNE    99
120800120903     C*    VI4NRS        ANDNE     *ZEROS
120900000705     C* CONTROLLO I CLIENTI CHE SONO LEGATI A QUELLA SERIE CON P.O. UGUALE
121000000705     C* AL P.O. PADRONCINO
121100120903     C*                  SETON                                        08
121200120903     C*                  EXSR      CTRNRS
121300120903     C*  90              GOTO      ENDCTR
121400000705     C* ALTRIMENTI  ERRORE
121500120903     C*                  OTHER
121600000705     C* ERRORE
121700120903     C*                  SETON                                        7590
121800120903     C*                  GOTO      ENDCTR
121900000705     C*
122000120903     C**                 ENDSL
122100000718     C*
122200120903     C**                 ENDIF
122300000512     C*
122400000512     C                   ENDIF
122500000512     C* OBBLIGATORIETA' DEL CODICE PADRONCINO SOLO IN CASO DI CONFERMA BOLLE
122600000512     C*
122700000512     C     *INKF         IFEQ      *ON
122800000512     C*
122900000512     C     VI4PDR        IFEQ      *ZEROS
123000000512     C     VI4PDR        OREQ      *BLANKS
123100000515     C     VI4DRT        OREQ      *ZEROS
123200000512     C                   SETON                                        9068
123300000512     C                   GOTO      ENDCTR
123400000512     C                   ENDIF
123500010219     C* SE IN CONFERMA E P.O. PADRONCINO ATTIVATO A PROCEDURA O.R.M.
123600010219     C* È OBBLIGATORIO IL N.O.R.M.
123700010219     C                   EXSR      ORMOBB
123800010219     C     VIDORM        IFEQ      *ZEROS
123900010219     C     WOBBL         ANDEQ     'S'
124000010220     C                   SETON                                        905828
124100010219     C                   MOVEL     ERR(6)        VIDMSG
124200010219     C                   GOTO      ENDCTR
124300010219     C                   ENDIF
124400000512     C                   ENDIF
124500921005     C*
124600921005     C* D A T A   S P E D I Z I O N E
124700941007     C                   Z-ADD     VI4DSP        G02DAT
124800941007     C                   MOVEL     *BLANK        G02ERR
124900941007     C                   CALL      'XSRDA8'
125000941007     C                   PARM                    WLBDAT
125100941007     C* ERRATA
125200941007    1C     G02ERR        IFEQ      '1'
125300960103     C                   SETON                                        532890
125400960103     C* PREPARO L'ERRORE
125500960103     C                   MOVEA     ERR(1)        K78
125600960103     C     2             SUBST     DATAPG:1      W002A
125700960103     C                   MOVEA     W002A         K78(69)
125800960103     C                   MOVEL     '/'           K78(71)
125900960103     C     2             SUBST     DATAPG:3      W002A
126000960103     C                   MOVEA     W002A         K78(72)
126100960103     C                   MOVEL     '/'           K78(74)
126200960103     C     2             SUBST     DATAPG:7      W002A
126300960103     C                   MOVEA     W002A         K78(75)
126400960103     C                   MOVEA     K78           VIDMSG
126500911031     C                   GOTO      ENDCTR
126600941007    1C                   ENDIF
126700960103     C                   Z-ADD     G02DAT        VI4DSP
126800941007     C                   Z-ADD     G02INV        COMDSP            8 0
126900960103     C* DATE SPEDIZIONE NO > DATA MAX DI SOSPENSIONE
127000960103     C* ANNO SPEDIZIONE NO < ANNO IN CORSO
127100960103     C                   MOVEL     COMDSP        AAS               4 0
127200960103    1C     AAS           IFLT      COMAAS
127300960103     C     COMDSP        ORGT      DATAPA
127400960103     C     COMDSP        ORLT      DATAM
127500960103     C                   SETON                                        539028
127600960103     C                   MOVEA     ERR(1)        K78
127700960103     C     2             SUBST     DATAPG:1      W002A             2
127800960103     C                   MOVEA     W002A         K78(69)
127900960103     C                   MOVEL     '/'           K78(71)
128000960103     C     2             SUBST     DATAPG:3      W002A
128100960103     C                   MOVEA     W002A         K78(72)
128200960103     C                   MOVEL     '/'           K78(74)
128300960103     C     2             SUBST     DATAPG:7      W002A
128400960103     C                   MOVEA     W002A         K78(75)
128500960103     C                   MOVEA     K78           VIDMSG
128600960103     C                   GOTO      ENDCTR
128700960103    1C                   ENDIF
128800911031     C*
128900921005     C* C O D I C E   P A D R O N C I N O
129000941007    1C     VI4PDR        IFNE      *BLANKS
129100941007     C     VI4PDR        ANDNE     *ZEROS
129200941007     C     '?'           SCAN      VI4PDR                                 31
129300930909     C*
129400930909    2C     *IN31         IFEQ      *ON
129500021203      *
129600021203      * imposto i campi della ds esterna fnlv24ds
129700021203     C                   clear                   fnlv24ds
129800021203     C                   eval      d24fil   = VI4LPD
129900021203     C                   eval      d24tip   = 'A'
130000021203     C                   eval      d24flg   = 'R'
130100021203     C                   movel     fnlv24ds      kpjbu
130200941011     C                   CALL      'FNLV24R'
130300930331     C                   PARM                    KPJBA
130400021203      *
130500021203      * valorizzo i campi a video
130600021203     C                   movel     kpjbu         fnlv24ds
130700021203     C                   if        d24pdr   >  *zeros
130800021203     C                   move      d24pdr        VI4PDR
130900021203     C                   movel     d24pdr        VI4LPD
131000021203     C                   else
131100021203     C                   clear                   VI4PDR
131200021203     C                   endif
131300021203     C                   movel     d24rsc        DESPDR
131400910201     C                   SETON                                        90
131500910201     C                   GOTO      ENDCTR
131600930909    2C                   END
131700910201     C*
131800941007     C* ESEGUO TESTN
131900941007     C                   TESTN                   VI4PDR               30
132000941010     C                   MOVE      VI4PDR        W001A             1
132100941007     C     *IN30         IFEQ      *OFF
132200941007     C     W001A         ORLT      '0'
132300941007     C                   SETON                                        4490
132400941007     C                   GOTO      ENDCTR
132500941007     C                   ENDIF
132600000518     C* TESTO SE IL PADRONCINO APPARTIENE AD UNA FILIALE POSTA
132700000524     C                   MOVEL     VI4LPD        WORG
132800000524     C                   EXSR      CTRORG
132900020219     C     WRKPT         IFEQ      'S'
133000020219     C     §OGNTW        ANDNE     'PPT'
133100020219     C     WRKPT         ORNE      'S'
133200020219     C     §OGNTW        ANDEQ     'PPT'
133300000518     C                   SETON                                        7090
133400000518     C                   GOTO      ENDCTR
133500000518     C                   ENDIF
133600941007     C*
133700941010     C                   MOVE      VI4PDR        W0070             7 0
133800950321     C                   MOVEL     VI4LPD        W0070             7 0
133900021203     C*    W0070         CHAIN     FNAPD000                           30
134000021205     C                   move      w0070         kpdpdr
134100021203     C                   move      'A'           kpdtip
134200021203     C     kfapd         chain     fiapd01l                           30
134300941007     C     *IN30         IFEQ      *ON
134400941007     C     *IN30         OREQ      *OFF
134500941007     C     APDATB        ANDNE     *BLANKS
134600941007     C                   SETON                                        4490
134700941007     C                   GOTO      ENDCTR
134800941007    1C                   ENDIF
134900941007     C* DESCRIZIONE
135000941010     C                   MOVEL     APDRSC        DESPDR
135100930909    1C                   ENDIF
135200010219     C*
135300010219     C* NUMERO O.R.M.
135400010219     C*
135500010220     C                   EXSR      ORMOBB
135600010219     C* Se immesso lo controllo solo se p.o. attivato a procedura ORM
135700010219    1C     VIDORM        IFGT      *ZEROS
135800060926     C**** WOBBL         ANDEQ     'S'
135900010227    3C     WIDORM        IFNE      ORMFIT
136000010219     C     KORM          CHAIN     FNORM01L                           30
136100010219    4C     *IN30         IFEQ      *ON
136200040819     c     ormatb        orne      *blanks
136300010220     C                   SETON                                        905828
136400010219     C                   MOVEL     ERR(7)        VIDMSG
136500010219     C                   GOTO      ENDCTR
136600010219    4C                   ENDIF
136700010220     C* CMD14 - INTERROGAZIONE ORM
136800010223     C     *INKH         IFEQ      *ON
136900010220     C                   CLEAR                   FIOR07
137000010220     C                   Z-ADD     VI4POE        D07POE
137100010220     C                   Z-ADD     VI4NSR        D07NSR
137200010223     C                   Z-ADD     VI4NO2        D07NOR
137300010220     C                   Z-ADD     VI4NRV        D07NRV
137400010220     C                   Z-ADD     VI4FGS        D07FGS
137500010220     C                   MOVEL     FIOR07        KPJBU
137600010220     C                   CALL      'FIOR07R'
137700010220     C                   PARM                    KPJBA
137800010226     C                   SETON                                        9058
137900010220     C                   GOTO      ENDCTR
138000010220     C                   ENDIF
138100010219     C* ERRORE SE P.O. RITIRO DIVERSO DA P.O. PADRONCINO
138200010219    4C     ORMPOR        IFNE      VI4LPD
138300060926     c* verifico se p.o. ritiro gestito dalla lnp bolla
138400060926     c* perchè in tal caso non devo dare errore se p.o. ritiro <> da
138500060926     c* lnp bolla
138600060926     c                   clear                   dslv55
138700060926     c                   eval      d55tpt = '6'
138800060926     c                   z-add     ormpor        d55lin
138900060926     c                   z-add     comdsp        d55drf
139000060926     c                   call      'FNLV55R'
139100060926     c                   parm                    dslv55
139200060926    5c                   if        d55err <> *blanks or d55tfa <> vi4lpd
139300060926     c* Verifico se si tratta di dati edi trasferiti: in questo caso
139400060926     c*  non do errore
139500060926    6c                   if        vi4ksc>0
139600060926     c                   clear                   tntb50ds
139700060926     C                   MOVEL     vi4ksc        b50ke2
139800060926     C*
139900060926     c                   movel     b50ke2        b50ke1
140000060926     c* controllo linea e p.o. orm
140100060926     c                   movel     'O'           b50opz
140200060926     c                   movel     ormpor        b50ormreso
140300060926     c                   movel     vi4lpd        b50poa
140400060926     c                   call      'TNTB51R'
140500060926     c                   parm                    kpjba
140600060926     c                   parm                    tntb50ds
140700060926     c                   endif
140800060926     c
140900060926    6c                   if        b50err<>' '  or vi4ksc=0
141000081215     c                   movel     ormpor        worg
141100081215     c                   exsr      ctrorg
141200081215     c                   move      §ogntw        ntwpor
141300081215     c                   movel     vi4lpd        worg
141400081215     c                   exsr      ctrorg
141500081215     c                   if        ntwpor<>'DPD' or §ogntw<>'DPD'
141600010220     C                   SETON                                        905828
141700010219     C                   MOVEL     ERR(8)        VIDMSG
141800010219     C                   GOTO      ENDCTR
141900081215     c                   ENDIF
142000060926    6C                   ENDIF
142100060926    5C                   ENDIF
142200060926    4C                   ENDIF
142300010219     C*
142400010219     C* ERRORE SE ORM PREPAGATO
142500010219    4C     ORMTOR        IFEQ      'P'
142600080110     C                   SETON                                        905828
142700010219     C                   MOVEL     ERR(9)        VIDMSG
142800010219     C                   GOTO      ENDCTR
142900010219    4C                   ENDIF
143000010219     C* ERRORE SE ORM IN CUI PAGA IL DESTINATARIO
143100010219    4C     ORMPAG        IFEQ      'D'
143200010220     C                   SETON                                        905828
143300010219     C                   MOVEL     ERR(10)       VIDMSG
143400010219     C                   GOTO      ENDCTR
143500010219    4C                   ENDIF
143600010223     C* ERRORE SE ORM CON LINEA DI ARRIVO FORZATA
143700010223    4C     ORMPOC        IFGT      *ZEROS
143800010223     C                   SETON                                        905828
143900010223     C                   MOVEL     ERR(13)       VIDMSG
144000010223     C                   GOTO      ENDCTR
144100010223    4C                   ENDIF
144200010219     C* ERRORE FORZABILE SE ORM IN FASE NON BOLLETTABILE
144300010219   4AC     VIDORM        IFNE      SAVORM
144400010219     C                   Z-ADD     VIDORM        SAVORM
144500010219     C* A CAMBIO NUMERO ORM DEVO PULIRE CAMPI PER CONTROLLO FORZATURE
144600010219     C* SU INCONGRUENZE CON ORM
144700010219     C                   CLEAR                   SA1PDR
144800010219     C                   CLEAR                   SA1DRT
144900010219     C                   CLEAR                   SA1FPP
145000010219     C                   CLEAR                   SA1KSC
145100070906     c                   clear                   s_drt
145200070906     c                   setoff                                       93
145300010219     C*
145400010219     C                   CLEAR                   DSBS02
145500010219     C                   MOVEL     'C'           T02MOD
145600010219     C                   MOVEL     KNSIF         T02SIF
145700010219     C                   MOVEL     'FAR'         T02COD
145800010219     C                   MOVEL     ORMFAO        T02KE1
145900010219      *
146000010219     C                   CALL      'TIBS02R'
146100010219     C                   PARM                    KPJBA
146200010219     C                   PARM                    DSBS02
146300010219    4C     T02ERR        IFEQ      *BLANKS
146400010219     C                   MOVEL     T02UNI        DFAR
146500010219     C                   ELSE
146600010219     C                   CLEAR                   DFAR
146700010219    4C                   ENDIF
146800010219    4C     §FARFB        IFNE      'S'
146900010219     C                   MOVEL     ERR(11)       VIDMSG
147000010220     C                   SETON                                        905828
147100010219     C                   GOTO      ENDCTR
147200010219    4C                   ENDIF
147300010219   4AC                   ENDIF
147400010226   X3C                   ELSE
147500070829     c* se orm fittizio a cambio numero orm reimposto udate nella data
147600070829     c* ritiro
147700070829     c                   if        vidorm<>savorm and vi4drt<>wdtgio
147800071107     c*****                        and vi4drt=s_drt and dateu>=§vpodrt
147900071107     c                             and vi4drt=s_drt
148000070829     C                   Z-ADD     VIDORM        SAVORM
148100070829     c                   z-add     wdtgio        vi4drt
148200070906     c                   eval      s_drt=vi4drt
148300070829     c                   seton                                        90
148400070831     c                   setoff                                       93
148500070829     C                   GOTO      ENDCTR
148600070829     c                   endif
148700070829     c*
148800010226     C   KH              SETON                                        9058
148900010226     C   KH              GOTO      ENDCTR
149000010219    3C                   ENDIF
149100010226   X1C                   ELSE
149200010226     C   KH              SETON                                        9058
149300010226     C   KH              GOTO      ENDCTR
149400010219    1C                   ENDIF
149500950127     C*
149600950127     C* D A T A   R I T I R O   M E R C E
149700070803     c* se immesso orm e bolla non import forzo come data ritiro la data
149800070803     c* ritiro dell'orm
149900070803    1C     WIDORM        IFNE      ORMFIT
150000070904     c     widorm        andgt     0
150100071107     c**** dateu         andge     §vpodrt
150200070803     C                   MOVEL     VI4LPD        WORG
150300070803     C                   EXSR      CTRORG
150400070803     C                   CLEAR                   WLBDAT
150500070803     C                   MOVEL     '3'           G02ERR
150600070803     c* se fase utilizzabile propongo la data della fase
150700070803    3C     d§fardar      ifeq      'S'
150800071112     c                   if        ormfao=400
150900071112     c                   z-add     ormddc        g02inv
151000071112     c                   else
151100070803     C                   Z-ADD     ORMDFO        G02INV
151200071112     c                   endif
151300070803   x3c                   else
151400070803     c* se l'orm non è in una fase utilizzabile per la data ritiro prendo
151500070803     c* la data ritiro presente sull'orm
151600070803     C                   Z-ADD     ORMDAR        G02INV
151700070803    3c                   endif
151800070803     c* se la data ritiro da proporre è maggiore della data odierna non
151900070803     c* propongo nulla
152000070905    3c                   if        g02inv<=dateu and
152100070905     c                             §ogntw<>'DPD' and §ogntw<>'FED' and
152200070905     c                             §ogntw<>'EEX'
152300070803     C                   CALL      'XSRDA8'
152400070803     C                   PARM                    WLBDAT
152500070803    4c                   if        g02dat<>vi4drt
152600070803     C                   Z-ADD     G02DAT        VI4DRT
152700070906     C                   Z-ADD     G02DAT        s_drt
152800070906     C***                Z-ADD     g02inv        comdrt
152900070906     c***                z-add     g02inv        sa1drt
153000070831     C                   seton                                        9093
153100070803     c                   goto      endctr
153200070803    4c                   endif
153300070905     c                   else
153400070906     c* se non propongo la data perchè > data giorno o bolla import
153500070906     c* propongo udate
153600071107     c****               if        s_drt=0 or dateu<§vpodrt
153700071107     c                   if        s_drt=0
153800070906     c                   z-add     wdtgio        vi4drt
153900070906     c                   z-add     wdtgio        s_drt
154000070906     c**                 z-add     dateu         comdrt
154100070906     c**                 z-add     dateu         sa1drt
154200070906     c                   seton                                        90
154300070906     c                   goto      endctr
154400070906     c                   endif
154500070905     c
154600070803    3c                   endif
154700070803    1c                   endif
154800070803     c*
154900950127    1C     VI4DRT        IFGT      0
155000950127     C                   Z-ADD     VI4DRT        G02DAT
155100950127     C                   MOVEL     *BLANK        G02ERR
155200950127     C                   CALL      'XSRDA8'
155300950127     C                   PARM                    WLBDAT
155400070904    2C*    G02ERR        IFEQ      '1'
155500070904     C*    G02INV        ORGT      DATEU
155600070904     C*    G02INV        ORLT      DATAM
155700070904     c                   if        g02err='1' or g02inv>dateu or
155800080110     c                             g02inv<datamin
155900080110     c****                         (g02inv<datam and (widorm=ormfit or
156000071107     c****                         dateu<§vpodrt or s_drt<>vi4drt))
156100080110     c****                         s_drt<>vi4drt))
156200080110     C                   MOVEL     ERR(24)       VIDMSG
156300080110     C                   SETON                                        522890
156400080110     c* se errore data ritiro in quanto viene forzata data orm aggiungo
156500080110     c* nel messaggio di verificare l'orm
156600080110     c                   if        g02inv<datamin and widorm<>ormfit
156700080110     c                   eval      vidmsg=%trim(vidmsg)
156800080110     c                             + ': verificare l''O.R.M.'
156900080110     c                   endif
157000950127     C                   GOTO      ENDCTR
157100950127    2C                   ENDIF
157200950127     C*
157300950127     C                   Z-ADD     G02INV        COMDRT            8 0
157400070906     C                   Z-ADD     vi4drt        s_drt
157500950127     C* SE IMMESSO COD.PADRONCINO CONTROLLO CHE NON SIANO STATO GIA'
157600950127     C*  CONFERMATI I CONTEGGI
157700950127    2C     W0070         IFGT      0
157800950127     C                   Z-ADD     W0070         W01PDR
157900011126     C     KFTT          SETLL     FIFTT000                               30
158000950127    3C     *IN30         IFEQ      *ON
158100080110     C                   MOVEL     ERR(25)       VIDMSG
158200080110     C                   SETON                                        522890
158300950127     C                   GOTO      ENDCTR
158400950127    3C                   ENDIF
158500950127    2C                   ENDIF
158600950206    1C                   ENDIF
158700990107     c* se modificata serie e cod cliente rido messaggio di frozatura
158800990107     c*  cod trattamento merce
158900990107     C     VI4KSC        IFNE      SAVKSC
159000990107     C     VI4NRS        ORNE      SAVNRS
159100990107     C     VI4CTM        ORNE      SA2CTM
159200990107     C                   CLEAR                   WDUE
159300051207     C                   MOVEL     VI4KSC        SAvKSC
159400990107     C                   MOVEL     VI4NRS        SAVNRS
159500990107     C                   MOVEL     VI4CTM        SA2CTM
159600990107     C                   ENDIF
159700041222
159800041222      * se modificata serie e cod cliente riemetto messaggio di forzatura
159900041222      * codice tariffa
160000041222     c                   If        vi4ksc <> savksc or
160100041222     c                             vi4nrs <> savnrs or
160200041222     c                             vi4ctr <> savctr
160300041222     c                   Reset                   wfctr
160400041222     c                   Eval      savksc = vi4ksc
160500041222     c                   Eval      savnrs = vi4nrs
160600041222     c                   Eval      savctr = vi4ctr
160700041222     c                   EndIf
160800041222
160900920324     C*
161000921005     C* C O D I C E   T R A T T A M E N T O   M E R C E
161100930909    1C     VI4CTM        IFNE      *BLANKS
161200941007     C     '?'           SCAN      VI4CTM                                 90
161300930909     C*
161400941007    2C     *IN90         IFEQ      *ON
161500920324     C                   MOVEL     CODUT         §KUT
161600920324     C                   MOVEL     '1B'          §COD
161700920324     C                   MOVE      *BLANKS       VI4CTM
161800920324     C                   CALL      'X§TABER'
161900920324     C                   PARM                    §KUT
162000920324     C                   PARM                    §COD
162100920324     C                   PARM                    §KEY
162200920324     C                   PARM                    §DES             30
162300920324     C                   MOVEL     §KEY          VI4CTM
162400920324     C                   GOTO      ENDCTR
162500930909    2C                   END
162600920324     C*
162700000518     C* IMPOSSIBILE IMMETTERE QUALSIASI CODICE TRATTAMENTO MERCE IN CONFERMA
162800000518     C* BOLLE POSTE
162900000518     C     WRKPT         IFEQ      'S'
163000000518     C                   MOVEL     ERR(5)        VIDMSG
163100000518     C                   SETON                                        289064
163200000518     C                   GOTO      ENDCTR
163300000518     C                   ENDIF
163400000518     C**
163500920324     C                   MOVEL     '1B'          COD
163600941007     C                   MOVEL(P)  VI4CTM        KEY
163700920324     C     KTAB          CHAIN     TABEL                              30
163800920324     C* ANNULLATO
163900930909    2C  N30TBLFLG        IFNE      ' '
164000920324     C                   SETON                                        30
164100930909    2C                   END
164200920324     C* ERRORE
164300920324     C   30              SETON                                        5490
164400920324     C   30              GOTO      ENDCTR
164500020801
164600020801     C                   MOVEL     TBLUNI        DS1B
164700990107     C*
164800020801     C* Controllo se possibile utilizzare il codice trattamento merce
164900020801     c                   if        §1bf15 = 'S'
165000990107     C                   MOVEL     ERR(2)        VIDMSG
165100020801     c                   eval      %subst(vidmsg:47:2) = vi4ctm
165200990107     C                   SETON                                        289064
165300990107     C                   GOTO      ENDCTR
165400990107    2C                   ENDIF
165500120709     c*
165600120709     c* Non è sol "A" e non è autospunta:
165700120709     c* se prevede VAX --> errore
165800120709     c* non prevede VAX--> messaggio di avviso
165900120709     C     §1BFG1        IFne      'N'
166000120709     c     §1bfg5        andne     'N'
166100120709     c                   if        §1bf16='S'
166200120709     C                   MOVEL     ERR(32)       VIDMSG
166300120709     C                   SETON                                        289064
166400120709     C                   GOTO      ENDCTR
166500120709     c                   endif
166600120709     c                   if        vi4ctm<>s_vi4ctm
166700120709     C                   MOVEL     ERR(33)       VIDMSG
166800120709     C                   SETON                                        289064
166900120709     c                   eval      s_vi4ctm=vi4ctm
167000120709     C                   GOTO      ENDCTR
167100120709     c                   endif
167200120710     c* memorizzazione in un flag per controllare che il cod.trattam merce
167300120710     c* forzato sia utilizzabile per la bolla da confermare
167400120710     c                   eval      wctrctm='1'
167500120710     c* memorizzo i flag del ctm dall'1 al 13 che mi servirà per confrontarli
167600120710     c* con i flag del ctm del vab
167700120710     c                   eval      w1b=%subst(ds1b:26:13)
167800120709     c*
167900120709     c                   endif
168000990204     C*
168100990204     C* POSSO UTILIZZARE SOLO CTM CON SOLUZIONE "A"
168200090803     c* o  di autospunta
168300090803     C***  §1BFG1        IFNE      'N'
168400090803     c
168500120709     C***  §1BFG1        IFeq      'N'
168600120709     c***  §1bfg1        oreq      'S'
168700120709     c***  §1bfg5        andeq     'N'
168800120709     c***                else
168900120709     C***                MOVEL     ERR(4)        VIDMSG
169000120709     C***                SETON                                        289064
169100120709     C***                GOTO      ENDCTR
169200120709     C***                ENDIF
169300990107     C* CHIEDO FORZATURA DEL COD TRATTAMANTO MERCE
169400990107    2C     WDUE          IFEQ      ' '
169500990107     C                   MOVEL     '1'           WDUE              1
169600990107     C                   MOVEL     ERR(3)        VIDMSG
169700990107     C                   SETON                                        289064
169800990107     C                   GOTO      ENDCTR
169900990107    2C                   ENDIF
170000990107     C**
170100990107    1C                   ENDIF
170200041222
170300041222      * C O D I C E   T A R I F F A
170400041222if  1c                   If        vi4ctr <> *Blanks
170500041222      * Solo campi numerico
170600041222     c                   If        %subst(vi4ctr:1:1) < '0' or
170700041222     c                             %subst(vi4ctr:1:1) = ' ' or
170800041222     c                             %subst(vi4ctr:2:1) < '0' or
170900041222     c                             %subst(vi4ctr:2:1) = ' ' or
171000041222     c                             %subst(vi4ctr:3:1) < '0' or
171100041222     c                             %subst(vi4ctr:3:1) = ' '
171200041222     c                   Eval      *In28 = *On
171300041222     c                   Eval      *In67 = *On
171400041222     c                   Eval      *In90 = *On
171500041222     c                   Eval      vidmsg = err(21)
171600041222     c                   Goto      endctr
171700041222     c                   EndIf
171800041222
171900041222      * Msg info x forzatura codice tariffa
172000041222if  2c                   If        wfctr = *Off
172100041222     c                   Eval      *In28 = *On
172200041222     c                   Eval      *In67 = *On
172300041222     c                   Eval      *In90 = *On
172400041222     c                   Eval      vidmsg = err(22)
172500041222     c                   Eval      wfctr = *On
172600041222     c                   Goto      endctr
172700041222e   2c                   EndIf
172800041222e   1c                   EndIf
172900041222
173000910201     C*
173100921005     C* C O D I C E   C L I E N T E
173200930909    1C     VI4KSC        IFNE      0
173300941010     C                   MOVEL     VI4KSC        KSC
173400941010     C                   EXSR      CTRCCM
173500941010     C* 90 ON - ERRORI
173600941010     C   90              GOTO      ENDCTR
173700941010     C* 02 ON - IMMESSO CODICE CLIENTE MITTENTE
173800941010     C                   SETON                                        02
173900941010     C                   ELSE
174000941010     C                   CLEAR                   WCCMN
174100941010     C                   ENDIF
174200000613     C* SE INSERITO CODICE CLIENTE VERIFICARE SE CLIENTE APPARTIENE A POSTE
174300000613     C* E NON STO SELEZIONANDO DELLE POSTE
174400000613     C*
174500000613     C     VI4KSC        IFNE      0
174600000613     C                   MOVEL     VI4KSC        WORG
174700000613     C                   EXSR      CTRORG
174800020219     C     WRKPT         IFEQ      'S'
174900020219     C     §OGNTW        ANDNE     'PPT'
175000020219     C     WRKPT         ORNE      'S'
175100020219     C     §OGNTW        ANDEQ     'PPT'
175200000614     C                   SETON                                        7290
175300000613     C                   GOTO      ENDCTR
175400000613     C                   ENDIF
175500001207     C* SE CLIENTE DPD IL P.O. AUTOTRASPORTATORE DEVE ESSERE = AL P.O.
175600051207     C* CLIENTE  oppure in tabella 3TR
175700020219     C     §OGNTW        IFEQ      'DPD'
175800001207     C     VI4PDR        ANDGT     *ZEROS
175900001207     C     VI4LPD        ANDNE     WORG
176000051207     c
176100051207     c* Controllo da tabella 3TR se dati inviabili ad altro p.o.
176200051207     c                   clear                   tntb50ds
176300051207     c                   if        vi4ksc>0
176400051207     C                   MOVEL     vi4ksc        b50ke2
176500051207     c
176600051207     c                   movel     b50ke2        b50ke1
176700051207     c                   movel     'C'           b50opz
176800051207     c                   movel     vi4lpd        b50poa
176900051207     c                   call      'TNTB51R'
177000051207     c                   parm                    kpjba
177100051207     c                   parm                    tntb50ds
177200051207     c                   endif
177300051207     c
177400051207     c                   if        b50err<>' '  or vi4ksc=0
177500001207     C                   SETON                                        7690
177600001207     C                   GOTO      ENDCTR
177700001207     C                   ENDIF
177800051207     C                   ENDIF
177900000613     C                   ENDIF
178000041122     C
178100041122     c* L I N E A   D I   A R R I V O
178200041122     C                   IF        VI4LNA<>999
178300041122    2C     vi4lna        IFGT      *ZEROS
178400041122     C     vi4lna        CHAIN     AZORG01L                           30
178500041122     C                   ELSE
178600041122     C                   SETON                                        30
178700041122    2C                   END
178800041122     C*
178900041122     C* CONTROLLO SE ANNULLATO
179000041122     C* CONSIDERO SOLO I TIPI "F" E "A"
179100041122    2C  N30ORGFVA        IFNE      *BLANKS
179200041122    2C     ORGFAG        ORNE      'F'
179300041122     C     ORGFAG        ANDNE     'A'
179400041122     C                   SETON                                        30
179500041122    2C                   END
179600041122     C* CONTROLLO CHE NON SIA UNA LINEA BLOCCATA
179700041122     C                   MOVEL     ORGDE8        OG148
179800041122     C     §OGLBL        IFEQ      'N'
179900041122     C                   SETON                                        30
180000041122    2C                   END
180100041122     C*
180200041122    2C     *IN30         IFEQ      *ON
180300041123     C                   MOVEL     ERR(20)       VIDMSG
180400041122     C                   SETON                                        909228
180500041122     C                   GOTO      endctr
180600041122    2C                   ENDIF
180700060306     c                   else
180800060306     c* Se codice che prevede obbligo lna segnalo errore
180900060306    2c                   if        not *in08  and §clilna='S'
181000060306     C                   MOVEL     ERR(23)       VIDMSG
181100060306     C                   SETON                                        909228
181200060306     C                   GOTO      endctr
181300060306    2C                   ENDIF
181400060306     c
181500060306    1C                   ENDIF
181600041122     C
181700111214     C* L I N E A   D I    P A R T E N Z A   S E G N A C O L L O
181800111214     c                   if        vi4lnp>0
181900111214     C     vi4lnp        CHAIN     AZORG01L
182000111220     c                   if        not %found(azorg01l) or orgfva<>*blanks
182100111220     c                             or (orgfag <> 'F' and orgfag <>'A')
182200111214     C                   MOVEL     ERR(29)       VIDMSG
182300111214     C                   SETON                                        909428
182400111214     C                   GOTO      endctr
182500111214     c                   endif
182600111214     c                   endif
182700111214     c
182800900807     C*
182900921005     C* R I F E R I M E N T O   M I T T E N T E
183000930909    1C     VI4RMN        IFNE      0
183100080623     c* Se richiesto ordinamento per rif, alfabetico, errore
183200130121     c                   if        vi4ord='A' or vi4ord='S'
183300080623     C                   MOVEL     ERR(26)       VIDMSG
183400080623     C                   SETON                                        906128
183500080623     C                   GOTO      endctr
183600080623     c                   endif
183700911031     C*
183800900807     C* CONTROLLO ESISTENZA DELLA BOLLA CON QUEL RIFERIMENTO
183900930909     C* SOLO RIFERIMENTO SENZA CLIENTE
184000020916     C                   SETOFF                                       3031
184100020917     C  N02VI4RMN        SETLL     FIVAB03L                               31
184200930909     C*
184300930909     C* RIFERIMENTO CON CLIENTE
184400020917     C   02KVAB          SETLL     FIVAB01L                               31
184500930909     C*
184600020917     C  N02VI4RMN        READE     FIVAB03L                               31
184700020917     C   02KVAB          READE     FIVAB01L                               31
184800020916      *
184900020916     C     *IN31         DOWEQ     *OFF
185000020916     C                   MOVEL     VABFGS        WFIL              3 0
185100020916      *
185200020926      * VERIFICO IL P.O. gestione
185300020926     C     WFIL          LOOKUP    POG                                    30
185400020918     C   30              LEAVE
185500020916      *
185600020917     C  N02VI4RMN        READE     FIVAB03L                               31
185700020917     C   02KVAB          READE     FIVAB01L                               31
185800020916     C                   ENDDO
185900020917     C   31              SETON                                        4190
186000020917     C   31              GOTO      ENDCTR
186100930909     C*
186200930909     C* OBBLIGATORIO PADRONCINO E DATA RITIRO MERCE
186300941010    2C     VI4PDR        IFEQ      *ZEROS
186400941010     C     VI4PDR        OREQ      *BLANKS
186500930909     C                   SETON                                        4290
186600930909     C                   GOTO      ENDCTR
186700930909    2C                   ENDIF
186800930909     C*
186900930909    2C     VI4DRT        IFEQ      0
187000930909     C                   SETON                                        4590
187100930909     C                   GOTO      ENDCTR
187200930909    2C                   ENDIF
187300010220     C* OBBLIGATORIO ANCHE IL N.ORM SE ATTIVATA PROCEDURA
187400010220     C     WOBBL         IFEQ      'S'
187500010220     C     VIDORM        ANDEQ     *ZEROS
187600010220     C                   SETON                                        589028
187700010220     C                   MOVEL     ERR(12)       VIDMSG
187800010220     C                   GOTO      ENDCTR
187900010220     C                   ENDIF
188000930909     C*
188100930909     C                   SETON                                        03
188200930909    1C                   ENDIF
188300010220     C*
188400010220     C* SEGNALAZIONE INCONGRUENZE CAMPI DEL VIDEO CON DATI DELL'ORM
188500010220     C*
188600010220   1AC     VIDORM        IFGT      *ZEROS
188700060926     C***  WOBBL         ANDEQ     'S'
188800010227     C     WIDORM        ANDNE     ORMFIT
188900010220     C* INCONGRUENZA CON CODICE AUTOTRASPORTATORE
189000010220    1C     ORMNDC        IFGT      *ZEROS
189100010220     C     VI4PDR        ANDNE     SA1PDR
189200010221     C     VI4PDR        ANDGT     *ZEROS
189300010220     C                   MOVE      VI4PDR        SA1PDR
189400120607     C     KDST          CHAIN     FIDST01L                           30
189500010220     C   30              CLEAR                   DSTPDR
189600010220     C                   MOVE      DSTPDR        WDPDR
189700010220    2C     WDPDR         IFNE      VI4PDR
189800010220     C                   SETON                                        9077
189900010220     C                   GOTO      ENDCTR
190000010220    2C                   ENDIF
190100010220    1C                   ENDIF
190200010220     C* Il flag mattino/pomeriggio lo controllo solo se il PDR è =
190300010220    2C     ORMNDC        IFGT      *ZEROS
190400010220     C     VI4PDR        ANDEQ     WDPDR
190500010221     C     VI4PDR        ANDGT     *ZEROS
190600010220     C     VI4FPP        ANDNE     SA1FPP
190700010220     C                   MOVE      VI4FPP        SA1FPP
190800120607     C                   MOVE      ORMNDC        W0050             5 0
190900010220     C     KFVV          CHAIN     FNFVV01L                           30
191000010220    3C     FVVFPP        IFNE      VI4FPP
191100010220     C                   SETON                                        9081
191200010220     C                   GOTO      ENDCTR
191300010220    3C                   ENDIF
191400010220    2C                   ENDIF
191500010220     C* INCONGRUENZA CON DATA RITIRO
191600071107     c***                if        dateu<§vpodrt
191700071107    1C***  d§fardar      IFEQ      'S'
191800071107     C***  COMDRT        ANDNE     SA1DRT
191900071107     C***                Z-ADD     COMDRT        SA1DRT
192000071107    2C***  ORMDFO        IFNE      COMDRT
192100071107     C***                SETON                                        9078
192200071107     C***                GOTO      ENDCTR
192300071107    2C***                ENDIF
192400071107    1C***                ENDIF
192500071107     c****               endif
192600010220     C* INCONGRUENZA CON CODICE CLIENTE
192700010220    1C     ORMKSC        IFGT      *ZEROS
192800010220     C     ORMKSC        ANDNE     VI4KSC
192900010220     C     VI4KSC        ANDNE     SA1KSC
193000010220     C                   Z-ADD     VI4KSC        SA1KSC
193100010220     C                   SETON                                        9079
193200010220     C                   GOTO      ENDCTR
193300010220    1C                   ENDIF
193400010220     C*
193500010220   1AC                   ENDIF
193600080623     C*
193700080623     C* O R D I N A M E N T O   P E R   R I F . A L F A B E T I C O
193800130121    1C                   IF        VI4ORD='A' or VI4ORD='S'
193900080623     C* CODICE CLIENTE obbligatorio
194000080623     c                   if        vi4ksc=0
194100080623     c                   Eval      *In28 = *On
194200080623     c                   Eval      *In88 = *On
194300080623     c                   Eval      *In90 = *On
194400080623     c                   Eval      VidMsg = Err(27)
194500080623     c                   GoTo      EndCtr
194600080623     c                   endif
194700080623     c                   endif
194800030409
194900030409      * C O N S E G N A   R I C H I E S T A
195000030409
195100030411      * immettere i dati della consegna richiesta solo se è indicato il
195200030411      * cliente o la serie
195300030411     c                   If        (Vi4Dcr > *Zeros or Vi4Hcr > *Zeros or
195400030411     c                              Vi4Ore > *Zeros) and
195500030411     c                             (Vi4Ksc = *Zeros and
195600030411     c                             (Vi4Nrs = *Zeros or Vi4Nrs = 99))
195700030411     c                   Eval      *In28 = *On
195800030411     c                   Eval      *In88 = *On
195900030411     c                   Eval      *In90 = *On
196000030411     c                   Eval      VidMsg = Err(18)
196100030411     c                   GoTo      EndCtr
196200030411     c                   EndIf
196300030409      * tipo
196400030411     c                   If        Vi4Tcr <> *Blanks
196500030411      * o il tipo o le ore da sommare alla delivery
196600030411     c                   If        Vi4Ore > *Zeros
196700030411     c                   Eval      *In28 = *On
196800030411     c                   Eval      *In87 = *On
196900030411     c                   Eval      *In90 = *On
197000030411     c                   Eval      VidMsg = Err(16)
197100030411     c                   GoTo      EndCtr
197200030411     c                   EndIf
197300030411     c                   If        Vi4Dcr = *Zeros and Vi4Hcr = *Zeros
197400030409     c                   Eval      *In28 = *On
197500030409     c                   Eval      *In87 = *On
197600030409     c                   Eval      *In90 = *On
197700030409     c                   Eval      VidMsg = Err(17)
197800030409     c                   GoTo      EndCtr
197900030409     c                   EndIf
198000030411     c                   EndIf
198100030409      * data
198200030409     c                   If        Vi4Dcr > *Zeros
198300030409      * controllo formale della data
198400030409     c                   Clear                   wlbdat
198500030409     c                   Z-Add     Vi4Dcr        G02Dat
198600030409     c                   Call      'XSRDA8'
198700030409     c                   Parm                    wlbdat
198800030409    1c                   If        G02Err = '1'
198900030409     c                   Eval      *In28 = *On
199000030409     c                   Eval      *In85 = *On
199100030409     c                   Eval      *In90 = *On
199200030409     c                   Eval      VidMsg = Err(14)
199300030409     c                   GoTo      EndCtr
199400030409     c                   EndIf
199500030409     c                   Movel     G02Dat        Vi4Dcr
199600030409     c                   Movel     G02Inv        wdcr
199700030409      * Non deve essere inferiore/uguale a oggi
199800030409     c                   If        wdcr <= dateu
199900030409     c                   Eval      *In28 = *On
200000030409     c                   Eval      *In85 = *On
200100030409     c                   Eval      *In90 = *On
200200030409     c                   Eval      VidMsg = Err(14)
200300030409     c                   GoTo      EndCtr
200400030409     c                   EndIf
200500030415      * Non deve essere superiore a oggi + 5 gg.
200600051215     c                   if        vi4tcr = 'P'
200700051215     c                   z-add     woggidcgm1    s_woggidcg
200800051215     c                   z-add     §vpodcgm1     s_vpodcg
200900051215     c                   else
201000051215     c                   z-add     woggidcgm2    s_woggidcg
201100051215     c                   z-add     §vpodcgm2     s_vpodcg
201200051215     c                   endif
201300051215     c                   If        wdcr > s_woggidcg
201400030409     c                   Eval      *In28 = *On
201500030409     c                   Eval      *In85 = *On
201600030409     c                   Eval      *In90 = *On
201700041202     c                   Eval      VidMsg = Err(15)
201800041202     c                   Eval      VidMsg = %trim(vidmsg) + ' ' +
201900051215     c                             %trim(%editc(s_vpodcg:'Z'))  +
202000041202     c                             ' giorni dalla data odierna'
202100030409     c                   GoTo      EndCtr
202200030409     c                   EndIf
202300030409      * o la data o le ore da sommare alla delivery
202400030409     c                   If        Vi4Ore > *Zeros
202500030409     c                   Eval      *In28 = *On
202600030409     c                   Eval      *In85 = *On
202700030409     c                   Eval      *In90 = *On
202800030409     c                   Eval      VidMsg = Err(16)
202900030409     c                   GoTo      EndCtr
203000030409     c                   EndIf
203100030409     c                   EndIf
203200030411      * ora
203300030411      * o l'ora o le ore da sommare alla delivery
203400030411     c                   If        Vi4Hcr > *Zeros and
203500030411     c                             Vi4Ore > *Zeros
203600030411     c                   Eval      *In28 = *On
203700030411     c                   Eval      *In86 = *On
203800030411     c                   Eval      *In90 = *On
203900030411     c                   Eval      VidMsg = Err(16)
204000030411     c                   GoTo      EndCtr
204100030411     c                   EndIf
204200030415
204300030415      * Ore da sommare alla delivery
204400030415     c                   If        Vi4Ore > *Zeros and
204500030415     c                             Vi4Ore <> 24 and Vi4Ore <> 48 and
204600030415     c                             Vi4Ore <> 72
204700030415     c                   Eval      *In28 = *On
204800030415     c                   Eval      *In89 = *On
204900030415     c                   Eval      *In90 = *On
205000030415     c                   Eval      VidMsg = Err(19)
205100030415     c                   GoTo      EndCtr
205200030415     c                   EndIf
205300900514     C*
205400900514     C     ENDCTR        ENDSR
205500010219     C*
205600010219     C* CONTROLLO OBBLIGATORIETA' NUMERO ORM   ----------------------*
205700010219     C     ORMOBB        BEGSR
205800010219     C                   CLEAR                   WOBBL
205900010219     C     VI4LPD        IFGT      *ZEROS
206000010219     C                   MOVEL     VI4LPD        WORG
206100010219     C                   EXSR      CTRORG
206200010219     C                   MOVEL     ORGDE8        OG148
206300010219     C     §OGORM        IFEQ      'S'
206400010219     C                   MOVEL     'S'           WOBBL             1
206500010219     C                   ENDIF
206600010219     C                   ENDIF
206700010219     C                   ENDSR
206800941010     C*
206900941010     C* CONTROLLO CODICE CLIENTE ------------------------------------*
207000941010     C     CTRCCM        BEGSR
207100941010     C                   SETOFF                                       06
207200970512     C**
207300970512     C     KACO          CHAIN     CNACO000                           31
207400970512     C*
207500990518    1C     *IN31         IFEQ      *ON
207600970512     C     *IN31         OREQ      *OFF
207700970512     C     ACOFLG        ANDNE     *BLANKS
207800970512     C                   SETON                                        4090
207900970512     C                   GOTO      ENDCCM
208000990518    1C                   ENDIF
208100120906     c*
208200120906     c* Il seguente controllo non lo ripeto se provengo da conferma totale
208300120906     c* La conferma totale può essere fatta solo se immesso il cliente e in
208400120906     c* questo caso il controllo è gia stato fatto sempre da questa routine
208500120906     c* quando richiamata dalla CTRD01
208600120906     c* Se lo ripetesse si sposizionerebbe la lettura sul fivab fatta nella rout
208700120906     c* di conferma totale con conseguente errore dopo la call fnls01r
208800120906     c                   if        wconf=*off or tota<>'S'
208900970512     C*
209000970512     C* CONTROLLO SE ESISTONO BOLLE PER IL CODICE DIGITATO
209100020917     C* APPARTENENTI A £1/£6
209200020916     C                   SETOFF                                       3031
209300020927     C     KSC           SETLL     FIVAB01L
209400020927     C     KSC           READE(N)  FIVAB01L                               31
209500020916      *
209600020916     C     *IN31         DOWEQ     *OFF
209700020916     C                   MOVEL     VABFGS        WFIL              3 0
209800020916      *
209900020926      * VERIFICO IL P.O DI GESTIONE
210000020926     C     WFIL          LOOKUP    POG                                    30
210100020918     C   30              LEAVE
210200020916      *
210300020927     C     KSC           READE(N)  FIVAB01L                               31
210400020916     C                   ENDDO
210500020916     C   31              SETON                                        4390
210600020916     C   31              GOTO      ENDCCM
210700120906     c                   endif
210800970512     C*
210900970512     C* IMPOSTO IL CODICE DA UTILIZZARE NELLA CONFERMA
211000970512     C*  = AL CODICE DEL DISK
211100970512     C                   MOVEL     KSC           WCCMN
211200970512     C**
211300020219     C* CONTROLLI SU TABELLA 3NN
211400020405     C**
211500020405     C* Flag che indica l'esistenza solo del record valido '999'
211600020405     C                   CLEAR                   WES3N_S999        1
211700970512     C                   CLEAR                   WES3N
211800970516     C                   MOVEL     KSC           W0030
211900020219      *
212000020220      * VERIFICO SE ESISTE ALMENO UN RECORD VALIDO DELLA 3NN
212100020220     C                   MOVE      *BLANKS       TBEKE1
212200020220     C                   MOVEL     KSC           TBEKE1
212300020220     C     K02TBE        SETLL     TNTBE02L
212400020220     C     K02TBE        READE     TNTBE02L                               31
212500020220    1C     *IN31         DOWEQ     *OFF
212600020405     C     TBEATB        IFEQ      ' '
212700020405     C     TbeKe2        ifeq      '999'
212800020405     C     WEs3N         andne     'S'
212900020405     C                   MOVEL     'S'           WES3N_S999
213000020405     C                   else
213100020405     C                   MOVEL     ' '           WES3N_S999
213200020405     C                   endif
213300020405     C                   MOVEL     'S'           WES3N
213400020405     C                   ENDIF
213500020220     C     K02TBE        READE     TNTBE02L                               31
213600020320   1-C                   ENDDO
213700020320    1C     WES3N         IFEQ      'S'
213800020220      *
213900020219      * PROVO PRIMA A LEGGERE TABELLA 3NN CON P.O = P.O CODICE
214000020220     C                   MOVEL     VI4LPD        TBEKE2
214100020220     C     K03TBE        CHAIN     TNTBE02L                           32
214200020220    2C     *IN32         IFEQ      *OFF
214300020219     C     TBEATB        ANDEQ     ' '
214400020219      *
214500020220      * RECUPERO NUOVO CODICE
214600070914    3C***  TBEUNI        IFEQ      *BLANKS
214700070914     c                   if        %subst(tbeuni:1:7)<=*zeros
214800020219     C                   MOVEL     TBEKE1        WCCMN
214900020219     C                   ELSE
215000020219     C                   MOVEL     TBEUNI        WCCMN
215100020220   3-C                   ENDIF
215200020220   2EC                   ELSE
215300020219      *
215400020219      * PROVO POI A LEGGERE TABELLA 3NN CON P.O = 999
215500020219     C                   MOVEL     '999'         TBEKE2
215600020220     C     K03TBE        CHAIN     TNTBE02L                           32
215700020220    3C     *IN32         IFEQ      *OFF
215800020219     C     TBEATB        ANDEQ     ' '
215900020220      *
216000020220      * SE IL P.O. DEL NUOVO CODICE E' UGUALE AL P.O DI CONFERMA
216100020220      * USO NUOVO CODICE
216200070914    4C***  TBEUNI        IFEQ      *BLANKS
216300070914     c                   if        %subst(tbeuni:1:7)<=*zeros
216400020220     C                   MOVEL     TBEKE1        WTBEC             3 0
216500020220    5C     VI4LPD        IFEQ      WTBEC
216600020320     C     VI4LPD        ORNE      W0030
216700020220     C                   MOVEL     TBEKE1        WCCMN
216800020220   5-C                   ENDIF
216900020220   4EC                   ELSE
217000020220     C                   MOVEL     TBEUNI        WTBEC             3 0
217100020220    5C     VI4LPD        IFEQ      WTBEC
217200020320     C     VI4LPD        ORNE      W0030
217300020220     C                   MOVEL     TBEUNI        WCCMN
217400020220   5-C                   ENDIF
217500020220   4-C                   ENDIF
217600020220   3-C                   ENDIF
217700020219   2-C                   ENDIF
217800020219   1-C                   ENDIF
217900120830     c*
218000120830     c* I controlli seguenti li eseguo solo se non è stato richiesto l'annullam.
218100120831     c                   if        *inkf or wconf=*on
218200120831
218300120831     c                   if        wconf=*on
218400120831    1C     WES3N         IFEQ      ' '
218500120831    1C     WES3N_S999    OREQ      'S'
218600120831      * SE NON ESISTE NEPPURE UN RECORD VALIDO DELLA TABELLA
218700120831      * OPPURE ESISTE SOLO IL '999'
218800120831      * SE IL CLIENTE HA TRATTAMENTO DISK 'B' E SE L'AREA DI
218900120831      * APPARTENENZA E' DIVERSA DALLA FILIALE DEL CLIENTE
219000120831      * SEGNALO L'ERRORE
219100120831    2C     §1BFG1        IFEQ      'S'
219200120831     C                   MOVEL     KSC           WFIL              3 0
219300120831     C     WFIL          LOOKUP    pog                                    31
219400120831     C  N31              SETON                                        8490
219500120831     C  N31              GOTO      ENDCCM
219600120831   2-C                   ENDIF
219700120831   1-C                   ENDIF
219800120831     c                   endif
219900020219      *
220000020220      * SE IL P.O DI CONFERMA E' DIVERSO DA QUELLO DEL CODICE CLIENTE
220100020220      * ED ESISTE ALMENO UN RECORD VALIDO SULLA TABELLA 3NN CHE NON
220200020220      * SIA 999 , O P.O DI CONFERNA SEGNALO ERRORE
220300120830    1C***  VI4LPD        IFNE      W0030
220400120830     c                   if        vi4lpd<>w0030 and
220500120830     c                             %lookup(w0030:pog)=0
220600020220    2C     WES3N         IFEQ      'S'
220700020220     C                   MOVE      *BLANKS       TBEKE1
220800020220     C                   MOVEL     KSC           TBEKE1
220900020220     C                   MOVEL     '999'         TBEKE2
221000020220     C     K03TBE        CHAIN     TNTBE02L                           31
221100020220    3C     *IN31         IFEQ      *ON
221200020220     C     TBEATB        ORNE      ' '
221300020220     C                   MOVEL     VI4LPD        TBEKE2
221400020220     C     K03TBE        CHAIN     TNTBE02L                           32
221500020220    4C     *IN32         IFEQ      *ON
221600020220     C     TBEATB        ORNE      ' '
221700020219     C                   SETON                                        6390
221800020219     C                   GOTO      ENDCCM
221900020220    4C                   ENDIF
222000020220    3C                   ENDIF
222100020220    2C                   ENDIF
222200020220    1C                   ENDIF
222300040318     C*
222400040318     C* SE CLIENTE MANTIENE NUMERO DI SPEDIZIONE E SE LA TABELLA 3NN
222500040318     C* NON E' ABILITATA PER IL MIO P.O, E SE IL P.O DI CONFERMA
222600040318     C* E' DIVERSO DA QUELLO DEL CODICE CLIENTE SEGNALO ERRORE.
222700040318     C                   MOVEL     '3C'          COD
222800040318     C                   MOVEL(P)  KSC           KEY
222900040318     C*
223000040318     C     KTAB          CHAIN     TABEL                              31
223100040318     C* SOLO SE ESISTE
223200040318    1C  N31TBLFLG        IFEQ      ' '
223300040318     C                   MOVEL     TBLUNI        DS3C
223400040318    2C     §3CFSP        IFEQ      'S'
223500040318     C                   MOVE      *BLANKS       TBEKE1
223600040318     C                   MOVEL     KSC           TBEKE1
223700040318     C                   MOVEL     VI4LPD        TBEKE2
223800040318     C     K03TBE        CHAIN     TNTBE02L                           31
223900040318    3C     *IN31         IFEQ      *ON
224000040318     C     TBEATB        ORNE      ' '
224100040318    4C     VI4LPD        IFNE      W0030
224200040318     C                   SETON                                        8390
224300040318     C                   GOTO      ENDCCM
224400040318    4C                   ENDIF
224500040318    3C                   ENDIF
224600040318    2C                   ENDIF
224700040318    1C                   ENDIF
224800120830     c                   endif
224900970512     C**
225000970512     C* PRENDO TABELLA CLIENTE CON DISCHETTO
225100000705     C*
225200000717     C* SE PROVENGO DAL CARICAMENTO SCHIERA CLIENTI DALLA TABELLA 3C NON
225300000717     C* RIFACCIO IL CONTROLLO
225400970512     C**
225500000717     C     *IN08         IFEQ      *OFF
225600970512     C                   MOVEL     '3C'          COD
225700970512     C                   MOVEL(P)  WCCMN         KEY
225800970512     C* NON ESISTE NELLA TABELLA 3C QUEL CLIENTE
225900970512     C     KTAB          CHAIN     TABEL                              06
226000970512     C* ANNULLATO
226100990518    1C  N06TBLFLG        IFNE      ' '
226200970512     C                   SETON                                        06
226300990518    1C                   END
226400970512     C*
226500970512     C  N06              MOVEL     TBLUNI        DS3C
226600980805     C* SE NON HA L'* E' COME SE NON CI FOSSE LA 3C
226700990518    1C  N06§3COKD        IFNE      '*'
226800980805     C                   SETON                                        06
226900990518    1C                   ENDIF
227000060306     c* Prendo anche tabella CLI
227100060306     C                   CLEAR                   DSBS02
227200060306     C                   MOVEL     'C'           T02MOD
227300060306     C                   MOVEL     KNSIF         T02SIF
227400060306     C                   MOVEL     'CLI'         T02COD
227500060306     C                   MOVEL(p)  WCCMN         T02KE1
227600060306     C                   CALL      'TIBS02R'
227700060306     C                   PARM                    KPJBA
227800060306     C                   PARM                    DSBS02
227900060306     c
228000060306    4C     T02ERR        IFEQ      *BLANKS
228100060306     C                   MOVEL     T02UNI        Dcli
228200060306     c                   else
228300060306     c                   clear                   dcli
228400060306     c                   endif
228500000717     C*
228600000717     C                   ENDIF
228700941010     C*
228800941010     C     ENDCCM        ENDSR
228900910404     C*
229000900515     C*--- ELABORA BOLLE DA VISUALIZZARE -----------------------------*
229100900515     C     ELAB          BEGSR
229200020917     C*
229300020917     C* PULIZIA SUBFILE
229400020917     C                   EXSR      PULSF
229500020917     C                   Z-ADD     1             REC
229600020917     C                   Z-ADD     0             NRR
229700020927     C                   CLEAR                   WVABNR            1 0
229800020918     C                   SETOFF                                       1101
229900020923     C                   SETOFF                                       303133
230000111220     c* pulizia campi di comodo per controllo serie
230100111220     c                   clear                   preccm
230200111220     c                   clear                   prelnp
230300020917     C*
230400020917     C* 02 OFF  03 OFF - CARICO TUTTO
230500020917    1C     *IN02         IFEQ      *OFF
230600020917    2C     *IN03         IFEQ      *OFF
230700020917      *
230800020917     C* SE RICHIESTA LA SERIE LEGGO PER SERIE E POI SCARTO
230900020917    3C     VI4NRS        IFNE      99
231000020927     C                   EVAL      WVABNR = 6
231100020917     C     VI4NRS        SETLL     FIVAB06L
231200020917     C     VI4NRS        READE     FIVAB06L                               30
231300020917      *
231400020917   X3C                   ELSE
231500020926      *
231600020926      * ELABORO UN P.O ALLA VOLTA.
231700020927     C                   EVAL      *IN31 = *ON
231800020926     C                   Z-ADD     1             Y
231900020917    4C     *IN31         DOWEQ     *ON
232000020917     C     Y             ANDLT     30
232100020926      *   a fine schiera accendo IN31 ed esco dal ciclo
232200020926    6C                   IF        POG(Y) = *ZEROS
232300020926     c                   eval      *in31 = *on
232400020926     C                   leave
232500020926     C                   ELSE
232600020926     C                   move      POG(Y)        $FGS              3 0
232700020926    6C                   ENDIF
232800020926      *
232900021003     C                   EVAL      WVABNR = 2
233000020917     C     $FGS          SETLL     FIVAB02L
233100020917     C     $FGS          READE     FIVAB02L                               31
233200020917     C*
233300020926     C   31              ADD       1             Y
233400020917   4-C                   ENDDO
233500020917     C*
233600020917   3-C                   ENDIF
233700020917     C*
233800020917   X2C                   ELSE
233900020917     C* 02 OFF  03 ON  - CARICO SOLO IL RIFERIMENTO RICHIESTO
234000020927     C                   EVAL      WVABNR = 3
234100020917     C     VI4RMN        SETLL     FIVAB03L
234200020917     C     VI4RMN        READE     FIVAB03L                               30
234300020917    2C                   ENDIF
234400020917     C*
234500020917   X1C                   ELSE
234600020917     C*
234700020917     C* 02 ON   03 OFF - CARICO TUTTE LE BOLLE DEL CLIENTE DIGITATO
234800020917    2C     *IN03         IFEQ      *OFF
234900080623     C* PER CLIENTE rif numerico
235000130121     c                   select
235100130121     c                   when      vi4ord='N'
235200021003     C                   EVAL      WVABNR = 1
235300020917     C     VI4KSC        SETLL     FIVAB01L
235400020917     C     VI4KSC        READE(N)  FIVAB01L                               30
235500130121     c
235600130121     c                   when      vi4ord='S'
235700130121     C                   EVAL      WVABNR = 0
235800130121     C     VI4KSC        SETLL     FIVAB10L
235900130121     C     VI4KSC        READE(N)  FIVAB10L                               30
236000130121     C*
236100130121     c                   other
236200080623     C* PER CLIENTE rif alfanumerico
236300080623     C                   EVAL      WVABNR = 9
236400080623     C     VI4KSC        SETLL     FIVAB09L
236500080623     C     VI4KSC        READE(N)  FIVAB09L                               30
236600130121     c                   endsl
236700020917     C*
236800020917   X2C                   ELSE
236900020917     C* 02 ON   03 ON  - CARICO SOLO IL CLIENTE E RIFERIMENTO DIGIATATI
237000021003     C                   EVAL      WVABNR = 1
237100020917     C     KVAB          SETLL     FIVAB01L
237200050407     C     KVAB          READE(N)  FIVAB01L                               30
237300020917     C*
237400020917    2C                   ENDIF
237500020917    1C                   ENDIF
237600020917     C*
237700020917    1C     *IN30         DOWEQ     *OFF
237800020917     C     *IN31         ANDEQ     *OFF
237900020917     C*
238000020926     C* SCARTO I RECORD NON GESTITI
238100020926     C* TRANNE SE CCM=0 e RMN=0 e NRS=99
238200020926     C* PERCHE' IN QUESTO CASO STO GIA' LEGGENDO PER P.O.
238300020923     C                   SETOFF                                       33
238400020926    2C                   IF        *IN02 = *ON or *in03 = *ON
238500020926     C                             or  VI4NRS <> 99
238600020926     C     VABFGS        LOOKUP    POG                                    33
238700020918   X2C                   ELSE
238800020923     C                   MOVE      *on           *in33
238900020917   2-C                   ENDIF
239000020917     C* PROSEGUO SE LA LETTURA E' VALIDA ..
239100020923    2C     *IN33         IFEQ      *ON
239200020917     C*
239300020917     C* CARICO SOLO 9900 RECORD
239400020917     C* SE RICHIESTA LA SERIE SELEZIONO SOLO LE BOLLE CON QUELLA SERIE
239500020917    3C     VI4NRS        IFEQ      99
239600020917     C     VI4NRS        OREQ      VABNRS
239700041123   3aC     VI4lna        IFEQ      999
239800041123     C     VI4lna        OREQ      VABlna
239900020917     C*
240000020917     C* SE CLIENTE NON SELEZIONATO IN PRIMA VIDEATA E NON SONO UN P.O. POSTE
240100020917     C* NON CARICO CLIENTI POSTE IN SUBFILE
240200020917     C*
240300020917    4C     *IN02         IFEQ      *OFF
240400020917     C     WRKPT         ANDNE     'S'
240500020917     C*
240600020917     C                   MOVEL     VABCCM        WORG
240700020917     C* CONTROLLO A CHI APPARTIENE IL CLIENTE
240800020917     C                   EXSR      CTRORG
240900020917     C* SE CLIENTE POSTA PASSO AL RECORD SUCCESSIVO
241000020917    5C     §OGNTW        IFEQ      'PPT'
241100020917     C                   GOTO      NEXT
241200020917   5-C                   ENDIF
241300020917   4-C                   ENDIF
241400020917     C* SE CLIENTE NON SELEZIONATO IN PRIMA VIDEATA E SONO UN P.O. POSTE
241500020917     C* CARICO CLIENTI POSTE IN SUBFILE
241600020917    4C     *IN02         IFEQ      *OFF
241700020917     C     WRKPT         ANDEQ     'S'
241800020917     C*
241900020917     C                   MOVEL     VABCCM        WORG
242000020917     C* CONTROLLO A CHI APPARTIENE IL CLIENTE
242100020917     C                   EXSR      CTRORG
242200020917     C* SE CLIENTE POSTA PASSO AL RECORD SUCCESSIVO
242300020917    5C     §OGNTW        IFNE      'PPT'
242400020917     C                   GOTO      NEXT
242500020917   5-C                   ENDIF
242600020917   4-C                   ENDIF
242700130121     c
242800111215     c* Se effettuata selezione per linea di partenza segnacollo
242900111215     c* scarto il record se lnp diversa
243000111215     c                   if        vi4lnp=0 or  vi4lnp=vablnp
243100111215     c* se specificata la serie e non il codice cliente richiamo
243200111215     c* routine per controllare se è possibile proseguire con il
243300111215     c* caricamento del subfile o se deve essere richiesta l'immissione
243400111215     c* del codice cliente
243500111215     c                   clear                   wctrs
243600111219    4c                   if        vi4nrs<>99 and vi4nrs>00 and vi4ksc=0
243700111215     c                   exsr      ctr_serie
243800111215    4c                   endif
243900111215     c                   if        wctrs=*blanks
244000020917     C*
244100020917     C     NRR           COMP      9900                               30  30
244200020917     C  N30              EXSR      EMISS
244300111215     c                   else
244400111216     c                   seton                                        302888
244500111220     c                   if        vi4lnp=0
244600111216     c                   movel     err(30)       vidmsg
244700111220     c                   else
244800111220     c                   movel     err(31)       vidmsg
244900111220     c                   endif
245000111215     c                   endif
245100111215     c                   endif
245200041123   3aC                   ENDIF
245300041123   3-C                   ENDIF
245400020917   2-C                   ENDIF
245500020917     C*
245600020917     C* RECORD SUCCESSIVO
245700020917     C     NEXT          TAG
245800020917     C* SENZA CLIENTE
245900020917    2C  N30*IN02         IFEQ      *OFF
246000020917    3C     *IN03         IFEQ      *OFF
246100020917      *
246200020917     C* SE RICHIESTA LA SERIE LEGGO PER SERIE E POI SCARTO
246300020917    4C     VI4NRS        IFNE      99
246400020917     C     VI4NRS        READE     FIVAB06L                               30
246500020917     C*
246600020917   X4C                   ELSE
246700020926      *
246800020926      * ELABORO UM P.O ALLA VOLTA.
246900020918     C     $FGS          READE     FIVAB02L                               31
247000020917    5C     *IN31         DOWEQ     *ON
247100020917     C     Y             ANDLT     30
247200020917     C                   ADD       1             Y
247300020926      *   a fine schiera accendo IN31 ed esco dal ciclo
247400020926    6C                   IF        POG(Y) = *ZEROS
247500020926     c                   eval      *in31 = *on
247600020926     C                   leave
247700020926     C                   ELSE
247800020926     C                   move      POG(Y)        $FGS
247900020926    6C                   ENDIF
248000020926      *
248100020918     C     $FGS          SETLL     FIVAB02L                               31
248200020918     C     $FGS          READE     FIVAB02L                               31
248300020917   5-C                   ENDDO
248400020917   4-C                   ENDIF
248500020917      *
248600020917   X3C                   ELSE
248700020917     C     VI4RMN        READE     FIVAB03L                               30
248800020917    3C                   ENDIF
248900020917     C* CON CLIENTE
249000020917   X2C                   ELSE
249100130121   c
249200020917    3C     *IN03         IFEQ      *OFF
249300130121     c                   select
249400130121     c                   when      vi4ord='N'
249500020917     C     VI4KSC        READE(N)  FIVAB01L                               30
249600130121     c
249700130121     c                   when      vi4ord='S'
249800130121     C     VI4KSC        READE(N)  FIVAB10L                               30
249900130121     c
250000130121     c                   other
250100080623     C     VI4KSC        READE(N)  FIVAB09L                               30
250200130121     c                   endsl
250300080623     c
250400020917   X3C                   ELSE
250500020917     C* 02 ON   03 ON  - CARICO SOLO IL CLIENTE E RIFERIMENTO DIGIATATI
250600050407     C     KVAB          READE(N)  FIVAB01L                               30
250700020917    3C                   ENDIF
250800020917    2C                   ENDIF
250900020917    1C                   ENDDO
251000020917     C*
251100020917     C                   Z-ADD     NRR           SAVNRR            4 0
251200020917     C*
251300020917     C* SE IL CLIENTE HA PIU DI 9900 BOLLE EMETTO UN MESSAGGIO
251400020917IF  1C     NRR           IFGE      9900
251500020917     C                   SETON                                        01
251600020917E   1C                   ENDIF
251700930609     C*
251800930909     C                   ENDSR
251900111215     C* CONTROLLO per serie parziale ---------------------------------*
252000111215     C     CTR_serie     BEGSR
252100111215    1c                   if        vabccm<>preccm
252200111215     c* Presente più di un cliente con la stessa serie
252300111215    2c                   if        preccm>0
252400111215     c* . . . stessa linea di partenza
252500111215     c*       errore solo se si tratta di serie parziale
252600111215    3c                   if        vablnp=prelnp
252700111216     c                   clear                   trulnrsds
252800111219     c                   movel     vabccm        inrsCCM
252900111219     c                   movel     vablnp        inrsFLS
253000111216     c                   movel     vabnrs        inrsNRS
253100111216     c                   call      'TRULNRSR'
253200111216     c                   parm                    trulnrsds
253300111216     c                   if        onrsprz='P'
253400111216     c                   eval      wctrs='E'
253500111216     c                   endif
253600111215     c
253700111215   x3c                   else
253800111216     c* . . . diversa linea di partenza: sempre errore
253900111216     c                   eval      wctrs='E'
254000111215    3c                   endif
254100111215   x2c                   else
254200111215     c                   eval      preccm=vabccm
254300111215     c                   eval      prelnp=vablnp
254400111215    2c                   endif
254500111215    1c                   endif
254600111215     c
254700111215     c                   endsr
254800930909     C*
254900930909     C* CONTROLLO SE SI PUO' CONFERMARE LA BOLLA ---------------------*
255000941010     C     CTRD02        BEGSR
255100930909     C                   SETOFF                                       90
255200930909     C*
255300930909     C* SE BOLLA NORMALE DEVE ESSERCI COD PADRONCINO E DATA RITIRO
255400010220     C* E NUMERO ORM SE ATTIVATA PROCEDURA ORM
255500010220    2C     WOBBL         IFEQ      *BLANKS
255600010220    3C     VI4PDR        IFEQ      *BLANKS
255700941010     C     VI4PDR        OREQ      *ZEROS
255800000515     C     VI4DRT        OREQ      *ZEROS
255900010220     C                   SETON                                        513690
256000930909     C                   Z-ADD     NRR           REC
256100950112     C  NKF              UPDATE    LS66DSF
256200930909     C                   SETOFF                                       36
256300010220    3C                   ENDIF
256400010220   X2C                   ELSE
256500010220    3C     VIDORM        IFEQ      *ZEROS
256600010220     C     VI4PDR        OREQ      *BLANKS
256700010220     C     VI4PDR        OREQ      *ZEROS
256800010220     C     VI4DRT        OREQ      *ZEROS
256900010220     C                   SETON                                        803690
257000010220     C                   Z-ADD     NRR           REC
257100010220     C  NKF              UPDATE    LS66DSF
257200010220     C                   SETOFF                                       36
257300010220    3C                   ENDIF
257400010220    2C                   ENDIF
257500930909     C*
257600930909     C                   ENDSR
257700900515     C*
257800900515     C* EMISSIONE SUBFILE -------------------------------------------
257900900515     C     EMISS         BEGSR
258000970513     C**
258100900808     C* SALVO UNA VOLTA LA LINEA PARTENZA PER IL VIDEO
258200950321     C                   SETOFF                                       1007
258300900515     C                   MOVEL     *BLANKS       VI5SCE
258400941010     C                   MOVEL     VABLNA        VI5LNA
258500941010     C                   MOVEL     VABLNP        VI5LNP
258600941010     C                   MOVEL     VABAAS        VI5AAS
258700941010     C                   MOVEL     VABMGS        MMGG
258800900516     C                   MOVEL     GG            VI5DSP
258900900516     C                   MOVE      MM            VI5DSP
259000941010     C                   MOVE      VABCCM        VI5KSC
259100020917     C                   MOVE      VABFGS        VI5FGS
259200900827     C                   MOVEL     VI5KSC        KSC
259300970512     C**
259400970512     C* SE NON IMMESSO COD CLIENTE LO CERCO
259500970512    0C     *IN02         IFEQ      *OFF
259600020219     C                   MOVEL     VI5KSC        WCCMN
259700970512     C                   CLEAR                   WES3N
259800970512     C*
259900941011     C* DECODIFICA RAGIONE SOCIALE
260000970512     C     KACO          CHAIN     CNACO000                           33
260100970512    1C     *IN33         IFEQ      *OFF
260200970512     C     ACOFLG        ANDEQ     ' '
260300970512     C                   MOVEL     ACORAG        VI5RAG
260400970512     C                   ELSE
260500970512     C* SE NON C'E' LO CARICO MA PROTETTO SENZA POSSIBILITA' DI CONFERM
260600970512     C                   SETON                                        10
260700970512     C                   MOVEL     'C'           VI5SCE
260800970512     C                   CLEAR                   VI5RAG
260900960319    1C                   ENDIF
261000970512     C**
261100970512   X0C                   ELSE
261200970512     C**
261300970512     C* WCCMN E WES3N SONO GIA' IMPOSTATI DALL 1 VIDEATA
261400970512     C                   MOVEL     ACORAG        VI5RAG
261500970512    0C                   ENDIF
261600020927      * IMPOSTO NUMERO RELATIVO RECORD
261700020927     C                   SELECT
261800020927     C                   WHEN      WVABNR = 1
261900020927     C                   Z-ADD     VABNR1        VI5NRR
262000020927     C                   WHEN      WVABNR = 2
262100020927     C                   Z-ADD     VABNR2        VI5NRR
262200020927     C                   WHEN      WVABNR = 3
262300020927     C                   Z-ADD     VABNR3        VI5NRR
262400020927     C                   WHEN      WVABNR = 6
262500020927     C                   Z-ADD     VABNR6        VI5NRR
262600080623     C                   WHEN      WVABNR = 9
262700080623     C                   Z-ADD     VABNR9        VI5NRR
262800130121     C                   WHEN      WVABNR = 0
262900130121     C                   Z-ADD     VABNR10       VI5NRR
263000020927     C                   ENDSL
263100020927     C*
263200020927     C                   Z-ADD     VABNSP        VI5NSP
263300080526     c                   Eval      wtxt = VabRsd
263400080526     c                   ExSr      Sr_Chk
263500080526     c                   Movel     wtxt          vi5rsd
263600080526     C***                MOVEL     VABRSD        VI5RSD
263700121126     c*** visulizzo in DROP i riferimenti  mittente
263800121126     C                   MOVEL     VABRMN        VI5_rmn
263900121126     C                   MOVEL     VABRMa        VI5_rma
264000121126     c
264100130121     c                   if        vi4ord<>'A'
264200121126     C                   MOVEL     VABRMN        VI5RMN
264300080623     c                   z-add     vabrmn        w0090             9 0
264400080623     c                   eval      vi5rm2=%editc(w0090:'Z')
264500080623     c                   else
264600080623     c                   movel     vabrma        vi5rmn
264700080623     c                   movel     vabrma        vi5rm2
264800080623     c                   endif
264900080623     C******             MOVE      VABRMN        VI5RM2
265000941010     C                   MOVEL     VABNRS        VI5NRS
265100941010     C                   MOVEL     VABNCD        VI5SCD
265200941010     C                   MOVEL     VABNCA        VI5SCA
265300941010     C                   MOVEL     VABTSP        VI5TSP
265400920325     C     VI4CTM        IFNE      '  '
265500920325     C                   MOVEL     VI4CTM        VI5CTM
265600920325     C                   ELSE
265700941010     C                   MOVEL     VABCTM        VI5CTM
265800920325     C                   END
265900950407     C                   MOVEL     VABCTM        VH5CTM
266000000515     C* SE NON E' STATA RICHIESTA LA CONFERMA TOTALE DALLA PRIMA VIDEATA
266100000515     C* SCRIVO IL SUBFILE
266200000515     C     TOTA          IFNE      'S'
266300900518     C                   ADD       1             NRR               4 0
266400941007     C                   WRITE     LS66DSF
266500000515     C                   ENDIF
266600000515     C*
266700020219     C     ENDEMI        ENDSR
266800900515     C*
266900900515     C* PULIZIA SUBFILE ---------------------------------------------
267000900515     C     PULSF         BEGSR
267100900515     C                   SETON                                        35
267200941007     C                   WRITE     LS66DCT
267300900515     C                   SETOFF                                       35
267400900515     C                   ENDSR
267500900808     C*
267600900808     C*--- CONFERMA ANNULLAMENTO BOLLE INDICATE CON LA 'A' -----------*
267700900808     C     ANNULL        BEGSR
267800941010     C*
267900941007     C                   READC     LS66DSF                                35
268000941010     C*
268100941010    1C     *IN35         DOWEQ     *OFF
268200900808     C* ANNULLATO
268300941010    2C     VI5SCE        IFEQ      'A'
268400080623     c
268500020917     C* CANCELLO FIVAB
268600080623     C**** KVAB2         CHAIN     FIVAB02L                           32
268700080623     c     vi5nrr        chain     fivab0ff                           3290
268800970821     C     *IN32         IFEQ      *OFF
268900080623     C     *IN90         andeq     *OFF
269000060130      * Prima di annullare la spedizione se bolla RC vado ad annullare
269100060130      * il relativo ORM legato
269200111010     c                   exsr      annorm
269300000512     C* ANNULLO I RECORDS
269400080623     C                   movel     vi5nrr        WNRR              9 0
269500000512     C                   EXSR      ANNREC
269600000512     C*
269700000512     C                   ENDIF
269800910222     C*
269900941010    2C                   ENDIF
270000900808     C*
270100941010     C                   READC     LS66DSF                                35
270200941010     C                   ENDDO
270300900808     C*
270400900808     C                   ENDSR
270500900808     C*
270600900808     C*--- CONFERMA BOLLA VADO NEL DETTAGLIO -------------------------*
270700900808     C     CONF          BEGSR
270800911031     C                   SETOFF                                       90
270900950407     C                   CLEAR                   WBOL
271000120831
271100120831     C* CONTROLLO ESISTENZA CODICE TRATTAMENTO MERCE
271200120831    1C     VI5CTM        IFNE      *BLANKS
271300120831     C* SOLO SE DIVERSO
271400120831    2C     VI5CTM        IFNE      SAVCTM
271500120831     C                   MOVEL     '1B'          COD
271600120831     C                   MOVEL(P)  VI5CTM        KEY
271700120831     C     KTAB          CHAIN     TABEL                              33
271800120831     C* ANNULLATO
271900120831    3C  N33TBLFLG        IFNE      ' '
272000120831     C                   SETON                                        33
272100120831    3C                   END
272200120831     C*
272300120831     C   33              SETON                                        5090
272400120831     C   33              GOTO      ENDCON
272500120831     C*
272600120831     C                   MOVEL     TBLUNI        DS1B
272700120831     C                   MOVEL     VI5CTM        SAVCTM
272800120831    2C                   ENDIF
272900120831     C*
273000120831     C* SE NON C'E' IL CTM AZZERO LA DS E IL CAMPO DI SALVATAGGIO
273100120831   X1C                   ELSE
273200120831     C                   CLEAR                   DS1B
273300120831     C                   CLEAR                   SAVCTM
273400120831    1C                   ENDIF
273500950407     C*
273600941010     C* 02 OFF - NON IMMESSO CODICE CLIENTE IN 1 VIDEATA
273700120831    1C***  *IN02         IFEQ      *OFF
273800941010     C                   MOVEL     VI5KSC        KSC
273900120831     c                   eval      wconf=*on
274000941010     C                   EXSR      CTRCCM
274100120831     c                   eval      wconf=*off
274200941010     C* 90 ON - ERRORI
274300941010     C   90              GOTO      ENDCON
274400120831     C***                ENDIF
274500950407     C*
274600950407     C* SE CAMBIATO IL CODICE CLIENTE PULISCO IL CAMPO DEL CONTROLLO
274700950407     C*  DELLA SERIE
274800950407     C     VI5KSC        IFNE      WKSC
274900950407     C                   CLEAR                   WUNO
275000950407     C                   MOVEL     VI5KSC        WKSC
275100950407     C                   ENDIF
275200120710     c* se è stato forzato un codice trattamento merce no sol."A" e no
275300120710     c* autospunta che non prevede il FIVAX verifico che per la bolla
275400120710     c* selezionata sia utilizzabile
275500120710    0c                   if        wctrctm='1'
275600120710     c                   clear                   x
275700120710     c                   if        vh5ctm<>*blanks
275800120710     c                   eval      x=%lookup(vh5ctm:ctm)
275900120710     c                   endif
276000120710    1c                   select
276100120710     c* bolla con ctm che non prevede il packing list
276200120710     c                   when      x>0 and ctm16(x)<>'S'
276300120710     C                   SETON                                        2590
276400120710     C                   GOTO      ENDCON
276500120710     c* bolla con cod trattamento merce incongruente con quello forzato
276600120710     c                   when      x=0 or ctmf(x)<>w1b
276700120710     C                   SETON                                        6590
276800120710     C                   GOTO      ENDCON
276900120711     c                   other
277002130314     c* Errore se:
277003130314     c* 1) la bolla ha il VAX e lui solo (no vat record "P")
277004130314     c* 2) la bolla ha il VAT record "P" e il cliente è abilitato
277005130314     c* in questi casi non gli si deve dare la possibilità di forzare il ctm
277100120711     C     KBLD2         SETLL     FIVAX000
277101130313     C                   MOVEL     'P'           WTRC
277102130313     C     Kvat2         SETLL     FIVAt01l
277200130314     c                   if        (%equal(fivax01l) and not %equal(fivat01l))
277201130314     c                             or (%equal(fivat01l) and §clipdfp<>*blank)
277300120711     C                   SETON                                        1590
277400120711     C                   GOTO      ENDCON
277500120711     c                   endif
277600120710    1c                   endsl
277700120710   x0c                   else
277800950407     C*
277900950407     C* SE C'E' LA SERIE NON POSSO FORZARE COD TRATTAM.MERCE
278000950407     C*  --> ERRORE PERO' FORZABILE
278100950504    1C     VH5CTM        IFNE      '  '
278200950407     C     VI4CTM        ANDNE     '  '
278300950407     C     WUNO          ANDEQ     ' '
278400950504     C     VI5NRS        ANDNE     0
278500950407     C*
278600950407     C     VH5CTM        LOOKUP    CTM                                    33
278700950504    2C     *IN33         IFEQ      *ON
278800950407     C                   SETON                                        5590
278900950407     C                   MOVEL     '1'           WUNO              1
279000950407     C                   GOTO      ENDCON
279100950504    2C                   ENDIF
279200950407    1C                   ENDIF
279300120710    0c                   endif
279400941010     C*
279500941010     C* SE C'E' LA SERIE CTM DI BOLLA DEVE ESSERCI
279600941010    1C     VI5NRS        IFNE      0
279700941010     C     VI5CTM        ANDEQ     '  '
279800941010     C                   SETON                                        4190
279900941010     C                   GOTO      ENDCON
280000941010    1C                   ENDIF
280100120831     c* Il prossimo controlll lo sposto all'inizio della routine
280200120831     c* perchè mi serve la ds1b valorizzata per la sbr CTRCCM
280300930909     C*
280400900828     C* CONTROLLO ESISTENZA CODICE TRATTAMENTO MERCE
280500120831    1C***  VI5CTM        IFNE      *BLANKS
280600930909     C* SOLO SE DIVERSO
280700120831    2C***  VI5CTM        IFNE      SAVCTM
280800120831     C***                MOVEL     '1B'          COD
280900120831     C***                MOVEL(P)  VI5CTM        KEY
281000120831     C***  KTAB          CHAIN     TABEL                              33
281100910226     C* ANNULLATO
281200120831    3C**N33TBLFLG        IFNE      ' '
281300120831     C**                 SETON                                        33
281400120831    3C**                 END
281500941010     C*
281600120831     C** 33              SETON                                        5090
281700120831     C** 33              GOTO      ENDCON
281800900828     C*
281900120831     C**                 MOVEL     TBLUNI        DS1B
282000120831     C**                 MOVEL     VI5CTM        SAVCTM
282100120831    2C**                 ENDIF
282200950504     C*
282300950504     C* SE NON C'E' IL CTM AZZERO LA DS E IL CAMPO DI SALVATAGGIO
282400120831   X1C**                 ELSE
282500120831     C**                 CLEAR                   DS1B
282600120831     C**                 CLEAR                   SAVCTM
282700120831    1C**                 ENDIF
282800060306     c
282900060306     c* Se cliente con obbligo parzializzazione lna errore se non immes
283000060306     c                   if        §clilna='S' and vi4lna=999
283100060306     C                   SETON                                        3990
283200060306     C                   GOTO      ENDCON
283300060306     c                   endif
283400060306     C*
283500900808     C*
283600900828     C* CONTROLLO SE IL CLIENTE SI ASSEGNA IL NUMERO DI SPEDIZIONE
283700941010    1C     §1BFG2        IFEQ      'S'
283800941010     C* NON TROVATO RECORD DEL CLIENTE NELLA DS3C: ERRORE
283900920410     C   06              SETON                                        4690
284000920410     C   06              GOTO      ENDCON
284100900808     C*
284200941010     C* CONTROLLO SERIE SE NON CORRISPONDE A QUELLA ASSEGNATA AL CLIENT
284300020221     C*  SE LA LINEA DI PARTENZA NON E'ESTERA
284400100517     C                   MOVEL     VI5LNP        WORG              3 0
284500100517     C                   EXSR      CTRORG
284600100517     C**   §OGNTW        IFNE      'EEX'
284700100517     C**   §OGNTW        ANDNE     'EUP'
284800990603     C*
284900100517     C**   §3CNRS        IFNE      VI5NRS
285000100517     C**                 SETON                                        4790
285100100517     C**                 GOTO      ENDCON
285200100517     C**                 ENDIF
285300100517     C**                 ENDIF
285400000515     C*
285500100517     c                   clear                   trulnrsds
285600100517     c                   movel     vi5ksc        inrsCCM
285700100517     c                   movel     vi5nrs        inrsNRS
285800100517     c
285900100517     c                   call      'TRULNRSR'
286000100517     c                   parm                    trulnrsds
286100100517     c                   if        onrserr<>' '
286200100517     C                   SETON                                        4790
286300100517     C                   GOTO      ENDCON
286400100517     C                   endif
286500941010     C*
286600900808     C* CONTROLLO CHE IL NUMERO DI SPEDIZIONE NON SIA DOPPIO
286700920424     C* SE NON LO RIASSEGNO
286800941010    2C     §3CFSP        IFEQ      'S'
286900950220    1C     §1BFG3        OREQ      'S'
287000080204     c* Controllo prima che anno del vab=anno corrente altrimenti errore
287100080204     c                   if        vi5aas<>comaas
287200080204     C                   SETON                                        3890
287300080204     C                   GOTO      ENDCON
287400080204     c                   endif
287500941010     C     KBLP          SETLL     FNBLP01L                               33
287600921005     C   33              SETON                                        4890
287700921005     C   33              GOTO      ENDCON
287800941010    2C                   ENDIF
287900921005     C*
288000910311     C* SE FLAG6 "C" E NON C'E NUMERO DI SPEDIZIONE
288100910311     C* E NON POSSO BOLLETTARE CON LA SERIE DEL CLIENTE
288200941010    2C     VI5NSP        IFEQ      0
288300941010    3C     §3CFG1        IFEQ      'N'
288400910311     C                   SETON                                        5990
288500910311     C                   GOTO      ENDCON
288600941010    3C                   ENDIF
288700910311     C* SE STAMPA ANCHE LA BOLLA ERRORE VINCOLANTE
288800941010    3C     §1BFG3        IFEQ      'S'
288900910311     C                   SETON                                        5990
289000910311     C                   GOTO      ENDCON
289100941010    3C                   ENDIF
289200941010    2C                   END
289300941010     C*
289400941010    1C                   END
289500941010     C*
289600020910     C* SE FLAG7= S DEVE ESISTERE FILE FIVAD
289700991213    1C     §1BFG7        IFEQ      'S'
289800020910     C     KBLD2         SETLL     FIVAD000                               33
289900941010     C  N33              SETON                                        6090
290000941010     C  N33              GOTO      ENDCON
290100991213    1C                   ENDIF
290200130313     C* SE FLAG16= S DEVE ESISTERE FILE FIVAX o FIVAT record "P"
290300040604    1C     §1BF16        IFEQ      'S'
290400130313     C*    KBLD2         SETLL     FIVAX000                               33
290500130313     C* N33              SETON                                        2790
290600130313     C* N33              GOTO      ENDCON
290601130313     C     KBLD2         SETLL     FIVAX000
290602130313     C                   MOVEL     'P'           WTRC
290603130313     C     Kvat2         SETLL     FIVAt000
290604130313     c                   if        not %equal(fivax01l) and not %equal(fivat01l)
290605130313     c                   seton                                        2790
290606130313     c                   goto      endcon
290607130313     c                   endif
290608130314     c* cliente non abilitato al PDF
290609130314     c                   if        %equal(fivat01l) and §clipdfp=*blanks
290611130314     C                   SETON                                        2690
290612130314     C                   GOTO      ENDcon
290613130314     c                   endif
290700040604    1C                   ENDIF
290800900808     C*
290900941010     C* CANCELLO VAB
291000991213     C                   CLEAR                   §LPFPR
291100000515     C* SE VENGO DA CONFERMA TOTALE 1° VIDEATA NON AGGANCIO IL VAB
291200000515     C     TOTA          IFNE      'S'
291300020917     C     VI5NRR        CHAIN     FIVAB0FF                           3290
291400000515     C                   ELSE
291500000515     C                   SETOFF                                       3290
291600000515     C                   ENDIF
291700000523     C* SE STO CONFERMANO BOLLE POSTE VERIFICO SE ESISTE GIA' UNA BOLLA
291800000523     C* GIA' CREATA
291900060221     C                   CLEAR                   TROVA             1
292000060221     C                   CLEAR                   WDOPP             1
292100060221     C***  WRKPT         IFEQ      'S'
292200060221     C***                EXSR      CERBLP
292300060221     C***                ENDIF
292400000526     C* SE E' STATO TROVATO IL RECORD IN BLP ED E' UN AUTOGENERATO LEGGO IL
292500000526     C* SUCCESSIVO DOPO AVER DELETATO IL RECORD
292600000526     C     TROVA         IFEQ      'S'
292700000627     C                   GOTO      DOPO01
292800000526     C                   ENDIF
292900950407     C*
293000950407     C* 90 ON - NON RIUSCITA ALLOCAZIONE DEL RECORD
293100991213    1C     *IN90         IFEQ      *ON
293200950407     C                   SETON                                        66
293300950407     C                   GOTO      ENDCON
293400991213   X1C                   ELSE
293500911114     C*
293600991210     C*  SOLO SE NEL VAB C'E' ANCORA LA BOLLA DA CONFERMARE,
293700991210     C*  ADO AVANTI: POTREBBE AVERLA GIA' CONFERMATA UN ALTRO
293800991213    2C     *IN32         IFEQ      *OFF
293900001006     C* SE AS400 E CODICE CLIENTE CON DISK C, SE NON E' STATO FORZATO
294000020917     C* UN CODICE TRATTAMENTO MERCE E SE IL CTM DI FIVAB E' DISK B
294100001006     C* PASSO IL CTM SUO "FRATELLINO" AL PGM DI BOLLETTAZIONE.
294200001006     C* SE QUEST'ULTIMO MANCA --> ERRORE
294300001006     C                   CLEAR                   WWCTC
294400001006     C     WASDKC        IFEQ      'S'
294500001006     C     §3CDKC        ANDEQ     'S'
294600001006     C     VI4CTM        ANDEQ     '  '
294700001006     C     §1BFG1        ANDEQ     'S'
294800001006     C     §1BCTC        IFEQ      '  '
294900001006     C                   SETON                                        5790
295000001006     C                   GOTO      ENDCON
295100001006     C                   ELSE
295200001006     C                   MOVE      §1BCTC        WWCTC
295300001006     C                   ENDIF
295400001006     C                   ENDIF
295500911031     C                   EXSR      CRTDS
295600950112     C*
295700950112     C                   CALL      'FNLS01R'
295800950112     C                   PARM                    KPJBA
295900991026     C                   PARM                    DBLP
296000991026     C                   PARM                    DTASV
296100030611     C                   PARM                    trul90ds
296200030611      *
296300930628     C                   MOVEL     '1'           RICH              1
296400991213    2C                   ENDIF
296500000627     C*
296600000627     C     DOPO01        TAG
296700900808     C*
296800911031     C* SE FLAG=4 CMD12 NON CONFERMATA
296900991213    2C     §LPFPR        IFNE      '4'
297000000517     C* SE FLAG=3 CMD03 NON CONFERMATA
297100000517     C     §LPFPR        ANDNE     '3'
297200950123     C* SE E' = A 5 ERRORE
297300991213    3C     §LPFPR        IFEQ      '5'
297400000516     C* NON VENGO DA SELEZIONE TOTALE
297500000516     C     TOTA          IFNE      'S'
297600020917     C  N32              UNLOCK    FIVAB00F
297700000516     C                   ELSE
297800020917     C  N32              UNLOCK    FIVAB01L
297900000516     C                   ENDIF
298000950123     C                   SETON                                        90
298100991213   X3C                   ELSE
298200000526     C*
298300111010     C     *IN32         IFEQ      *OFF
298400111010     c* se flag ='2' --> cancellare dal vab e suoi collegati
298500111010     c                   if        §lpfpr='2'
298600111010     c                   exsr      ges_ann
298700111010     c                   else
298800910404     C                   Z-ADD     VI5KSC        PREKSC
298900080623     C***                Z-ADD     VI5RMN        PRERMN
299000130121     c                   if        vi4ord<>'A'
299100080623     c                   movel     vi5rmn        w0150            15 0
299200080623     c                   eval      prermn=%editc(w0150:'Z')
299300080623     c                   else
299400080623     c                   movel     vi5rmn        prermn
299500080623     c                   endif
299600000516     C* NON VENGO DA SELEZIONE TOTALE
299700000516     C     TOTA          IFNE      'S'
299800020917     C                   DELETE    FIVAB0FF
299900000516     C                   ELSE
300000130121     c                   select
300100130121     c                   when      vi4ord='N'
300200020917     C                   DELETE    FIVAB000
300300130121     c                   when      vi4ord='S'
300400130121     C                   DELETE    FIVAB10
300500130121     c                   other
300600080623     C                   DELETE    FIVAB09
300700130121     c                   endsl
300800080623     c
300900000516     C                   ENDIF
301000111010     c                   endif
301100111010     C                   ENDIF
301200911031     C*
301300000515     C* PROTEGGO IL CAMPO DI SCELTA SE NON VENGO DA CONFERMA TOTALE 1° VIDEATA
301400000515    4C   05TOTA          IFNE      'S'
301500900808     C                   SETON                                        10
301600950321     C                   MOVEL     'C'           VI5SCE
301700950321     C* SE F6 NON VISUALIZZO TUTTA LA BOLLA
301800991213    5C     WF6           IFEQ      '1'
301900950321     C                   SETON                                        07
302000950321     C                   ELSE
302100950321     C                   SETOFF                                       07
302200991213    5C                   ENDIF
302300950321     C*
302400941007     C                   UPDATE    LS66DSF
302500900808     C                   SETOFF                                       10
302600991213    4C                   END
302700991213    3C                   END
302800950321     C*
302900991213   X2C                   ELSE
303000020917     C  N32              UNLOCK    FIVAB00F
303100991213    2C                   END
303200950407    1C                   END
303300900828     C*
303400900828     C     ENDCON        TAG
303500900828     C* POSIZIONAMENTO SU RECORD SBAGLIATO
303600950123     C   05
303700950123    1CAN 90              DO
303800911031     C                   MOVEL     'E'           VI5SCE
303900950321     C                   SETOFF                                       07
304000950321     C                   SETON                                        113612
304100941007     C                   UPDATE    LS66DSF
304200900828     C                   SETOFF                                       1136
304300900828     C                   Z-ADD     NRR           REC
304400950123    1C                   END
304500000515     C* PER 90 E SELEZIONE TOTALE SCRIVO IL RECORD ERRATA NEL SUBFILE
304600000515     C   90TOTA          IFEQ      'S'
304700000515     C                   ADD       1             NRR
304800000516     C                   MOVEL     'E'           VI5SCE
304900000516     C                   SETOFF                                       07
305000000516     C                   SETON                                        113612
305100000515     C                   WRITE     LS66DSF
305200000516     C                   SETOFF                                       1136
305300000516     C                   Z-ADD     NRR           REC
305400000515     C                   ENDIF
305500000515     C*
305600911031     C                   ENDSR
305700900808     C*
305800900808     C*--- SELEZIONE TOTALE BOLLE IN SUBFILE -------------------------*
305900900808     C     SELEZ         BEGSR
306000900824     C*
306100900824     C                   Z-ADD     0             B                 5 0
306200950321     C                   MOVEL     '1'           WF6               1
306300950321     C*
306400900828     C     B             DOWLT     SAVNRR
306500900824     C                   ADD       1             B
306600941007     C     B             CHAIN     LS66DSF                            31
306700950321     C*
306800950321     C* SE L'HO GIA' CONFERMATA NON LA RILEGGO
306900950321     C  N31VI5SCE        IFNE      'C'
307000950419     C                   MOVEL     'W'           VI5SCE
307100950321     C                   EXSR      CONF
307200000517     C* SE DATO IN FNLS01R IL KC VADO A FIE LETTURA
307300000517     C     §LPFPR        IFEQ      '3'
307400000517     C                   LEAVE
307500000517     C                   ENDIF
307600000517     C*
307700950321     C                   ENDIF
307800911031     C                   END
307900900808     C*
308000900808     C                   ENDSR
308100000512     C*-------------------------------------------------------------*
308200000512     C* -- ROUTINE DI LETTURA TOTALE BOLLE CLIENTE PER CONFERMA --  *
308300000512     C* -- O ANNULLAMENTO COMPLETO                              --  *
308400000512     C*-------------------------------------------------------------*
308500000512     C     SUBTOT        BEGSR
308600000512     C* PULIZIA SUBFILE  PER GLI ERRORI
308700000512     C                   EXSR      PULSF
308800000512     C                   Z-ADD     1             REC
308900000512     C                   Z-ADD     0             NRR
309000000512     C                   SETOFF                                       1101
309100000718     C* SE E' STATO RICHIESTO IL NUEMRO DI SERIE LEGGO E CONFERMO IL VAB
309200000718     C* PER OGNI CLIENTE PRESENTE NELLA SCHIERA CLI PRECEDENTEMENTE
309300000718     C* CARICATA DALLA TABELLA 3C
309400120903    1C*    C             IFEQ      *ZEROS
309500120903     C*    *IN08         ANDEQ     *OFF
309600120903     C*                  Z-ADD     1             C
309700120903     C*                  MOVEA     VI4KSC        CLI(C)
309800120903   1-C*                  ENDIF
309900120903     C*                  Z-ADD     1             Z                 3 0
310000120903     C*                  SETOFF                                       08
310100000718     C*
310200120903    1C*    Z             DOWLE     C
310300120903     C*                  MOVE      CLI(Z)        VI4KSC
310400000718     C* QUESTO CONTROLLO LO FACCIO QUANDO VENGO DA UNA SELEZIONE TOTALE PER
310500000718     C* SERIE
310600000718     C                   MOVEL     VI4KSC        KSC
310700000718     C                   EXSR      CTRCCM
310800000718     C*
310900000718     C                   SETON                                        02
311000000512     C*
311100000512     C* RICHIESTO A VIDEO IL CODICE CLIENTE SENZA RIFERIMENTO MITTENTE
311200020917    2C     *IN03         IFEQ      *OFF
311300080623     C* PER CLIENTE e rif numerico
311400130121     c                   select
311500130121     c                   when      vi4ord='N'
311600021004     C                   EVAL      WVABNR = 1
311700020917     C     VI4KSC        SETLL     FIVAB01L
311800020917     C     VI4KSC        READE     FIVAB01L                               30
311900130121     c                   when      vi4ord='S'
312000130121     C                   EVAL      WVABNR = 0
312100130121     C     VI4KSC        SETLL     FIVAB10L
312200130121     C     VI4KSC        READE     FIVAB10L                               30
312300080623     C* PER CLIENTE e rif alfa numerico
312400130121     c                   other
312500080623     C                   EVAL      WVABNR = 9
312600080623     C     VI4KSC        SETLL     FIVAB09L
312700080623     C     VI4KSC        READE     FIVAB09L                               30
312800130121     c                   endsl
312900020917     C*
313000020917   X2C                   ELSE
313100000512     C* RICHIESTO A VIDEO SIA IL CODICE CHE IL RIFERIMENTO MITTENTE
313200021004     C                   EVAL      WVABNR = 1
313300020917     C     KVAB          SETLL     FIVAB01L
313400020917     C     KVAB          READE     FIVAB01L                               30
313500020917   2-C                   ENDIF
313600000512     C*
313700020917    2C     *IN30         DOWEQ     *OFF
313800020917      *
313900020926     C     VABFGS        LOOKUP    POG                                    33
314000020917     C* PROSEGUO SE LA LETTURA E' VALIDA ..
314100020923    3C     *IN33         IFEQ      *ON
314200020917     C*
314300000512     C* ANNULLAMENTO
314400091007      * Prima di annullare la spedizione se bolla RC vado ad annullare
314500091007      * il relativo ORM legato
314600100604    4c                   if        *inkq
314700111010     c                   exsr      annorm
314800100604     C                   clear                   WNRR
314900100604     C                   EXSR      ANNREC
315000100604    4c                   EndIf
315100100604
315200000512     C* CONFERMA
315300020917    4C     *INKF         IFEQ      *ON
315400000515     C* SE RICHIESTA LA SERIE SELEZIONO SOLO LE BOLLE CON QUELLA SERIE
315500020917    5C     VI4NRS        IFEQ      99
315600000515     C     VI4NRS        OREQ      VABNRS
315700041123     C* SE RICHIESTA LA lin arrivo selezSOLO LE BOLLE CON QUELLA lna
315800041123   5aC     VI4lna        IFEQ      999
315900041123     C     VI4lna        OREQ      VABlna
316000111223     C* SE RICHIESTA LA lin parten selezSOLO LE BOLLE CON QUELLA lnp
316100111223   6aC     VI4lnp        IFEQ      *zeros
316200111223     C     VI4lnp        OREQ      VABlnp
316300000515     C                   EXSR      CONREC
316400000524     C* SE DATO IN FNLS01R IL KC VADO A FINE LETTURA
316500020917    6C     §LPFPR        IFEQ      '3'
316600000517     C                   LEAVE
316700020917   6-C                   ENDIF
316800000515     C* SE ARRIVATI A 9900 RECORD DEL SUBFILE VADO A FINE LETTURA
316900020917    6C     NRR           IFGE      9900
317000000515     C                   LEAVE
317100020917   6-C                   ENDIF
317200000515     C*
317300111223   6ac                   endif
317400041123   5aC                   ENDIF
317500041123   5-C                   ENDIF
317600000515     C*
317700020917   4-C                   ENDIF
317800020917   3-C                   ENDIF
317900000512     C*
318000020917    3C     *IN03         IFEQ      *OFF
318100130121     c                   select
318200130121     c                   when      vi4ord='N'
318300020917     C     VI4KSC        READE     FIVAB01L                               30
318400130121     c                   when      vi4ord='S'
318500130121     C     VI4KSC        READE     FIVAB10L                               30
318600130121     c                   other
318700080623     C     VI4KSC        READE     FIVAB09L                               30
318800130121     c                   endsl
318900080623     c
319000020917   X3C                   ELSE
319100020917     C     KVAB          READE     FIVAB01L                               30
319200020917    3C                   ENDIF
319300000512     C*
319400020917    2C                   ENDDO
319500000718     C*
319600120903     C*                  ADD       1             Z
319700120903    1C*                  ENDDO
319800000512     C*
319900000512     C                   ENDSR
320000000512     C*
320100000512     C*-------------------------------------------------------------*
320200000512     C*     -- ROUTINE DI ANNULLAMENTO RECORD VAB/VAD       --      *
320300000512     C*-------------------------------------------------------------*
320400000512     C     ANNREC        BEGSR
320500911031     C*
320600000512     C*
320700020910     C* CANCELLO RECORD IN FIVAD000
320800020910     C     KBLD          SETLL     FIVAD000
320900020910     C     KBLD          READE     FIVAD000                               32
321000000512     C*
321100000512    3C     *IN32         DOWEQ     *OFF
321200020910     C                   DELETE    FIVAD000
321300020910     C     KBLD          READE     FIVAD000                               32
321400000512    3C                   ENDDO
321500020108     C*
321600020910     C* CANCELLO RECORD IN FIVAT000
321700020910     C     KBLD          SETLL     FIVAT000
321800020910     C     KBLD          READE     FIVAT000                               32
321900020108     C*
322000020108    3C     *IN32         DOWEQ     *OFF
322100020910     C                   DELETE    FIVAT000
322200020910     C     KBLD          READE     FIVAT000                               32
322300020108    3C                   ENDDO
322400040604     c*
322500040604     C* CANCELLO RECORD IN FIVAX000
322600040604     C     KBLD          SETLL     FIVAX000
322700040604     C     KBLD          READE     FIVAX000                               32
322800040604     C*
322900040604    3C     *IN32         DOWEQ     *OFF
323000040604     C                   DELETE    FIVAX000
323100040604     C     KBLD          READE     FIVAX000                               32
323200040604    3C                   ENDDO
323300081017      * CANCELLO RECORD IN FIRAB000
323400081020     c     kbld          setll     firab02l
323500081017     c                   do        *hival
323600081020     c     kbld          reade     firab02l
323700081017    3c                   if        %eof(firab02l)
323800081017     c                   leave
323900081017     c                   endif
324000081017     c                   delete    firab000
324100081017    3c                   enddo
324200000512     C*
324300000512     C* CANCELLO EVENTUALE RECORD DEL RIFERIMENTO PARTNER ESTERO
324400060221     C***                MOVEL     VABLNP        WORG
324500060221     C***                EXSR      CTRORG
324600000512     C*
324700060221     C***  §OGNTW        IFEQ      'EEX'
324800060221     C***  §OGNTW        OREQ      'EUP'
324900000512     C* SOLO SE NON ESISTE GIA' LA BOLLA IN FNBLP
325000060221     C***  KBLP2         SETLL     FNBLP000                               32
325100060221     C**N32KBL4          DELETE    Fiar4000                           32
325200060221     C***                ENDIF
325300080623      *
325400021003      * CANCELLO FIVAB
325500021003      *  Se cancellazione singola ho letto FIVAB02L e cancello FIVAB00F
325600021003      *   con il num. relativo record che mi sono memorizzato
325700021003      *  Se annullamento totale ho letto FIVAB01L e cancello FIVAB01L
325800021003     C                   IF        Wnrr > *zeros
325900021003     C     Wnrr          DELETE    FIVAB0FF
326000021003     C                   else
326100130121     c                   select
326200130121     c                   when      vi4ord='N'
326300021003     c                   DELETE    FIVAB000
326400130121     c                   when      vi4ord='S'
326500130121     c                   DELETE    FIVAB10
326600130121     c                   other
326700080623     c                   DELETE    FIVAB09
326800130121     c                   endsl
326900021003     c                   endif
327000000512     C*
327100000512     C                   ENDSR
327200000512     C*
327300000512     C*-------------------------------------------------------------*
327400000512     C*     -- ROUTINE DI CONFERMA     RECORD VAB/VAD       --      *
327500000512     C*-------------------------------------------------------------*
327600000512     C     CONREC        BEGSR
327700000515     C* CARICO I CAMPI PER IL CONTROLLO DEL SINGOLO RECORD
327800000515     C                   EXSR      EMISS
327900000515     C* CONTROLLO DEL SINGOLO RECORD
328000000515     C                   EXSR      CONF
328100000512     C*
328200000512     C                   ENDSR
328300000512     C*-------------------------------------------------------------*
328400000512     C*     -- AGGANCIO AZORG --                                    *
328500000512     C*-------------------------------------------------------------*
328600000512     C     CTRORG        BEGSR
328700000512     C*
328800000524     C     WORG          CHAIN     AZORG01L                           37
328900000512     C     *IN37         IFEQ      *ON
329000000512     C     ORGFVA        ORNE      ' '
329100000512     C                   MOVEL     *BLANK        ORGFL1
329200000524     C                   MOVEL     *BLANK        ORGDES
329300000524     C                   MOVEL     *BLANK        ORGDE3
329400010219     C                   MOVEL     *BLANK        ORGDE8
329500040121     C                   MOVEL     *BLANK        ORGDF0
329600000512     C                   ENDIF
329700000524     C                   MOVEL     ORGDE3        OG143
329800000512     C*
329900000512     C                   ENDSR
330000000523     C*-------------------------------------------------------------*
330100000523     C*     -- ROUTINE DI RICERCA BLP PER RECORD BOLLA POSTA --     *
330200000523     C*-------------------------------------------------------------*
330300060221     C**** CERBLP        BEGSR
330400000523     C*
330500060221     C*****              CLEAR                   WESCI
330600001214     C*
330700001207     C* AGGANCIO CON IL SEGNACOLLO (FLS NRS NSC) DETTAGLIO SEGNACOLLI
330800001214     C* DEVO FARE UN CICLO DI LETTURA PERCHE' POTREI AVERNE ANCHE
330900001214     C* PIU' DI UNA DI BOLLE CON STESSO SEGNACOLLO MA CON <> LNA SE
331000041123     C* PER ESEMPIO ERANO PRESENTI DUE O PIU' ANOMALIE 10 SU STESSO
331100001214     C* SEGNACOLLO MA CON LNA <>.
331200000523     C*
331300060221     C**** KBLT          SETLL     FNBLT29L
331400060221     C**** KBLT          READE     FNBLT29L                               37
331500000523     C*
331600060221     C**** *IN37         DOWEQ     *OFF
331700060221     C**** WESCI         ANDEQ     *BLANKS
331800001214     C* SE COLLO DOPPIO E LNA <> MEMORIZZO ED ESCO DA QUESTA ROUTINE
331900060221    2C**** VABLNA        IFNE      BLTLNA
332000060221     C****               MOVEL     'S'           WDOPP             1
332100060221   X2C****               ELSE
332200001214     C* SE COLLO DOPPIO E STESSA LNA AGGANCIO FNBLP PER VEDERE SE SI
332300001214     C* TRATTA DI BOLLA AUTOGENERATA
332400060221     C****               CLEAR                   WDOPP
332500060221     C****               MOVE      '1'           WESCI             1
332600000523     C* AGGANCIO FNBLP
332700060221     C**** KBLPPT        CHAIN     FNBLP01L                           38
332800060221     C**** *IN38         IFEQ      *OFF
332900000523     C* AUTOGENERATA  AGGIORNO E DELETO
333000060221     C**** BLPSOP        IFEQ      '&'
333100060221     c**** BlpDcm        Andeq     *Zeros
333200060221     C****               EXSR      AGGBLP
333300060221     C****               MOVEL     'S'           TROVA             1
333400060221     C****               ELSE
333500060221     C****               UNLOCK    FNBLP01L
333600060221     C****               ENDIF
333700000523     C*
333800060221     C*****              ENDIF
333900060221    2C*****              ENDIF
334000000526     C*
334100060221     C**** KBLT          READE     FNBLT29L                               37
334200060221     C****               ENDDO
334300000523     C*
334400060221     C****               ENDSR
334500000523     C*
334600000523     C*-------------------------------------------------------------*
334700000523     C*     -- ROUTINE DI RICERCA BLP PER RECORD BOLLA POSTA --     *
334800000523     C*-------------------------------------------------------------*
334900060221     C**** AGGBLP        BEGSR
335000021220
335100021220      * In questa routine non aggiorno FIAR5 xchè per le bolle poste
335200021220      * non sono previsti i bancali
335300000531     C*
335400060221     C*****              MOVEL     'J'           WPTTRC
335500000531     C*
335600000523     C* VERIFCO SE BOLLA GIA' AGGIORNATA IN UNA FASE PRECEDENTE
335700060221     C**** KBL4PT        CHAIN     Fiar401L                           37
335800000523     C*
335900000526     C*
336000060221     C**** *IN37         IFEQ      *ON
336100060221     C****               CLEAR                   Fiar4000
336200060221     C****               ENDIF
336300000523     C* VERIFICO:SE BOLLA GIA' AGGIORNATA PROSEGUO SENZA AGGIORNARE BLP
336400000523     C*
336500060221     C****               MOVEL     BL4NOT        DSBL4J
336600000523     C*
336700060221    2C***  §B4BAU        IFEQ      'S'
336800060221     C**N37              UNLOCK    Fiar401L
336900060221     C****               ELSE
337000000627     C*
337100060221     C****               UNLOCK    FNBLP01L
337200000627     C* CARICO LA DS CHE MI SERVE PER RICHIAMARE IL PGM FNLS01R (IMMISSIONE
337300000627     C* BOLLE) SOLO PER I CONTROLLI
337400060221     C*****              CLEAR                   WWCTC
337500060221     C******             EXSR      CRTDS
337600000627     C* MODFICO ALCUNI CAMPI
337700060221     C*****              MOVEL     BLPLNP        §LPLNP
337800060221     C******             MOVEL     BLPNRS        §LPNRS
337900060221     C******             MOVEL     BLPNSP        §LPNSP
338000060221     C******             MOVEL     'C'           §LPFLG
338100060221     C*****              MOVEL     '&'           §LPSOP
338200060221     C*****              MOVEL     VABCCM        §LPCCM
338300060221     C****               MOVEL     VABCCM        §LPCCN
338400000627     C* PULISCO QUESTI CAMPI PERCHÈ LI AGGIORNO IO DAL VAB  IN BLP E ASSOCIATI
338500060221     C*****              CLEAR                   §LPIAS
338600060221     C****               CLEAR                   §LPVAS
338700050114     C**!!!              CLEAR                   §LPXCO
338800030611      *
338900060221     C*****              CALL      'FNLS01R'
339000060221     C*****              PARM                    KPJBA
339100060221     C*****              PARM                    DBLP
339200060221     C*****              PARM                    DTASV
339300060221     C*****              PARM                    trul90ds
339400030611      *
339500060221     C****               MOVEL     '1'           RICH              1
339600000627     C* SE FLAG=4 CMD12 NON CONFERMABILE
339700000627     C* SE FLAG=3 CMD03 NON CONFERMABILE
339800000627     C* SE FLAG=5 NON COMFERMABILE ERRORE
339900000523     C*
340000060221     C***  §LPFPR        IFNE      '4'
340100060221     C**** §LPFPR        ANDNE     '3'
340200060221     C**** §LPFPR        ANDNE     '5'
340300000627     C* AGGIORNO
340400060221     C*****              Z-ADD     VABIAS        §B4IAS
340500060221     C**** VABVAS        IFEQ      *BLANKS
340600060221     C***  VABIAS        ANDGT     *ZEROS
340700060221     C*****              MOVEL     'ITL'         §B4VAS
340800060221     C*****              ELSE
340900060221     C****               MOVEL     VABVAS        §B4VAS
341000060221     C*****              ENDIF
341100050114     C**!!!              MOVE      VABXCO        §B4XCO
341200060221     C*****              Clear                   §B4XCO
341300060221     C******             MOVEL     'S'           §B4BAU
341400060221     C* SE DA CLIENTE RICEVO F TOLGO LA 'S' DA ar4
341500060221     C**** §LPTC1        IFEQ      'F'
341600060221     C**** §LPTC2        OREQ      'F'
341700060221     C****               CLEAR                   §B4BAI
341800060221     C******             ENDIF
341900001221     C* MEMORIZZO ANCHE LA DATA SPEDIZIONE DEL CLIENTE
342000060221     C****               MOVEL     VABAAS        §B4DSC
342100060221     C****               MOVE      VABMGS        §B4DSC
342200010118     C* RICEVO IL CONTRASSEGNO DAL CLIENTE:
342300010118     C* SE BOLLA GIA' CONSEGNATA CON CONSEGNA ANOMALA DEVO MEMORIZZARE
342400010118     C* CHE NON RICEVERO' MAI DALL'ARRIVO IL RECORD DELL'INCASSO DEL
342500010118     C* CONTRASSEGNO. QUESTO FLAG SERVE IN PULIZIA BOLLE PARTENZE PER
342600010118     C* POTER LO STESSO PULIRE LA BOLLA CON CONTRASSEGNO NON INCASSATO
342700060221     C**** §LPCAS        IFGT      *ZEROS
342800060221     C***  BLPDCM        ANDGT     *ZEROS
342900060221     C***  BLPCCA        ANDNE     *BLANKS
343000060221     C****               MOVEL     'S'           §B4CSB
343100060221     C****               ENDIF
343200001221     C*
343300060221     C*****              MOVEL     DSBL4J        BL4NOT
343400000526     C*
343500060221     C***  *IN37         IFEQ      *ON
343600060221     C***                MOVEL     BLTAAS        BL4AAS
343700060221     C****               MOVEL     BLTLNP        BL4LNP
343800060221     C****               MOVEL     BLTNRS        BL4NRS
343900060221     C****               MOVEL     BLTNSP        BL4NSP
344000060221     C****               MOVEL     'J'           BL4TRC
344100060221     C****               MOVEL     BLTAAS        BL4AAS
344200060221     C****               WRITE     Fiar4000
344300000526     C*
344400060221     C*****              ELSE
344500000523     C*
344600060221     C*****              UPDATE    Fiar4000
344700000526     C*
344800060221     C*****              ENDIF
344900000524     C*
345000000523     C* AGGIORNO BLP
345100000615     C* SE IMPORTO CONTRASSEGNO E' A ZEROS NON AGGIORNO IL CODICE BOLLA
345200000615     C* PERCHE' POTREBBE ESSERE STATO MODIFICATO ED INCASSATO IN ARRIVO
345300000615     C*
345400060221     C***  KBLPPT        CHAIN     FNBLP01L                           38
345500000627     C*
345600060221     C**** §LPCAS        IFNE      *ZEROS
345700060221     C****               MOVE      §LPCBO        BLPCBO
345800060221     C****               ENDIF
345900000627     C* MITTENTE
346000060221     C*****              Z-ADD     §LPCCN        BLPKSC
346100060221     c****               Eval      wtxt = §lpRsm
346200060221     c****               ExSr      Sr_Chk
346300060221     c****               Movel     wtxt          BlpRsm
346400040301     C**!!!              MOVEL     §LPRSM        BLPRSM
346500060221     c****               Eval      wtxt = §lpInm
346600060221     c****               ExSr      Sr_Chk
346700060221     c*****              Movel     wtxt          BlpInm
346800040301     C**!!!              MOVEL     §LPINM        BLPINM
346900060221     c****               Eval      wtxt = §lpLom
347000060221     c*                  ExSr      Sr_Chk
347100060221     c*                  Movel     wtxt          BlpLom
347200040301     C**!!!              MOVEL     §LPLOM        BLPLOM
347300060221     c*                  Eval      wtxt = §lpCam
347400060221     c*                  ExSr      Sr_Chk
347500060221     c*                  Movel     wtxt          BlpCam
347600040301     C**!!!              MOVEL     §LPCAM        BLPCAM
347700060221     C*                  MOVEL     §LPPRM        BLPPRM
347800060221     C*                  MOVEL     §LPNZM        BLPNZM
347900060221     C*                  MOVEL     §LPCPI        BLPCPI
348000000627     C* DESTINATARIO
348100060221     c*                  Eval      wtxt = §lpRsd
348200060221     c*                  ExSr      Sr_Chk
348300060221     c*                  Movel     wtxt          BlpRsd
348400040301     C**!!!              MOVEL     §LPRSD        BLPRSD
348500060221     c*                  Eval      wtxt = §lpInd
348600060221     c*                  ExSr      Sr_Chk
348700060221     c*                  Movel     wtxt          BlpInd
348800040301     C**!!!              MOVEL     §LPIND        BLPIND
348900060221     c*                  Eval      wtxt = §lpLod
349000060221     c*                  ExSr      Sr_Chk
349100060221     c*                  Movel     wtxt          BlpLod
349200040301     C**!!!              MOVEL     §LPLOD        BLPLOD
349300060221     c*                  Eval      wtxt = §lpCad
349400060221     c*                  ExSr      Sr_Chk
349500060221     c*                  Movel     wtxt          BlpCad
349600040301     C**!!!              MOVEL     §LPCAD        BLPCAD
349700060221     C*                  MOVEL     §LPPRD        BLPPRD
349800060221     C*                  MOVEL     §LPNZD        BLPNZD
349900000627     C*
350000030924     C**!!!              MOVEL     §LPFLO        DBLP01
350100060221     C*    BLPFDN        IFNE      'S'
350200060221     C*                  MOVEL     §LPFAP        BLPFAP
350300060221     C*                  ENDIF
350400060221     C*                  MOVEL     §LPFIN        BLPFIN
350500000627     C*
350600000627     C* VISTO CHE I GIORNI DI CHIUSURA POSSONO ESSERE AGGIORNATI DALL'ARRIVO
350700000627     C* PRIMA DI AGGIORNARE QUELLI CHE MI HA DATO IL CLIENTE VERRIFICO
350800000627     C* CHE SIANO PIENI
350900060221     C*    §LPGC1        IFNE      *BLANKS
351000060221     C*    §LPGC1        ANDNE     BLPGC1
351100060221     C*    §LPGC1        ANDNE     BLPGC2
351200060221     C*    BLPGC1        ANDEQ     *BLANKS
351300060221     C*                  MOVEL     §LPGC1        BLPGC1
351400060221     C*                  ENDIF
351500060221     C*    §LPGC1        IFNE      *BLANKS
351600060221     C*    §LPGC1        ANDNE     BLPGC1
351700060221     C*    §LPGC1        ANDNE     BLPGC2
351800060221     C*    BLPGC2        ANDEQ     *BLANKS
351900060221     C*                  MOVEL     §LPGC1        BLPGC2
352000060221     C*                  ENDIF
352100060221     C*    §LPGC2        IFNE      *BLANKS
352200060221     C*    §LPGC2        ANDNE     BLPGC1
352300060221     C*    §LPGC2        ANDNE     BLPGC2
352400060221     C*    BLPGC1        ANDEQ     *BLANKS
352500060221     C*                  MOVEL     §LPGC2        BLPGC1
352600060221     C*                  ENDIF
352700060221     C*    §LPGC2        IFNE      *BLANKS
352800060221     C*    §LPGC2        ANDNE     BLPGC1
352900060221     C*    §LPGC2        ANDNE     BLPGC2
353000060221     C*    BLPGC2        ANDEQ     *BLANKS
353100060221     C*                  MOVEL     §LPGC2        BLPGC2
353200060221     C*                  ENDIF
353300000627     C*
353400060221     C*                  MOVEL     §LPCTR        BLPCTR
353500060221     C*                  MOVEL     §LPCTS        BLPCTS
353600060221     C*                  MOVEL     §LPTSP        BLPTSP
353700060221     C*                  MOVEL     §LPNAS        BLPNAS
353800060221     C*                  MOVEL     §LPNCL        BLPNCL
353900060221     C*                  MOVEL     §LPPKB        BLPPKB
354000060221     C*                  MOVEL     §LPPKB        BLPPKF
354100000627     C* VOLUME
354200060221     C*    §LPVLB        IFGT      0
354300060221     C*                  MOVEL     §LPVLB        BLPVLB
354400060221     C*                  MOVEL     §LPFVB        BLPFVB
354500000607     C* VERIFICO IL VOLUME FATTURATO
354600060221     C*    BLPFVF        IFNE      'T'
354700060221     C*    BLPFVF        ANDEQ     'Z'
354800060221     C*    §LPVLB        ANDGT     BLPVLF
354900060221     C*                  MOVEL     §LPVLB        BLPVLF
355000060221     C*                  MOVEL     §LPFVB        BLPFVF
355100060221     C*                  ENDIF
355200060221     C*    BLPFVF        IFNE      'T'
355300060221     C*    BLPFVF        ANDNE     'Z'
355400060221     C*                  MOVEL     §LPVLB        BLPVLF
355500060221     C*                  MOVEL     §LPFVB        BLPFVF
355600060221     C*                  ENDIF
355700060221     C*                  ENDIF
355800000607     C*
355900060221     c*                  Eval      wtxt = §lpRma
356000060221     c*                  ExSr      Sr_Chk
356100060221     c*                  Movel     wtxt          BlpRma
356200040301     C**!!!              MOVEL     §LPRMA        BLPRMA
356300060221     C*                  MOVEL     §LPRMN        BLPRMN
356400060221     C*                  MOVEL     §LPCTM        BLPCTM
356500060221     C*                  MOVEL     §LPGGA        BLPGGA
356600000627     C*******************  MOVEL§LPZNC    BLPZNC *******************************
356700060221     C*                  MOVEL     §LPGMA        BLPGMA
356800060221     C*                  MOVEL     §LPGVA        BLPGVA
356900060221     C*                  MOVEL     §LPFFD        BLPFFD
357000060221     C*                  Z-ADD     §LPQFT        BLPQFT
357100000627     C* SE DATA CONSEGNA RICHIESTA NON VALORIZZATA
357200060221     C*    BLPDCR        IFEQ      *ZEROS
357300060221     C*                  Z-ADD     §LPDCR        BLPDCR
357400060221     C*                  Z-ADD     §LPHCR        BLPHCR
357500060221     C*                  MOVEL     §LPTCR        BLPTCR
357600060221     C*                  ENDIF
357700000615     C* VISTO CHE I TIPI CONSEGNA POSSO ESSERE AGGIORNATI DALL'ARRIVO
357800000615     C* PRIMA DI AGGIORNARE QUELLI CHE MI HA DATO IL CLIENTE VERRIFICO
357900000615     C* CHE SIANO PIENI
358000060221     C*    §LPTC1        IFNE      *BLANKS
358100060221     C*    §LPTC1        ANDNE     BLPTC1
358200060221     C*    §LPTC1        ANDNE     BLPTC2
358300060221     C*    BLPTC1        ANDEQ     *BLANKS
358400060221     C*                  MOVEL     §LPTC1        BLPTC1
358500060221     C*                  ENDIF
358600060221     C*    §LPTC1        IFNE      *BLANKS
358700060221     C*    §LPTC1        ANDNE     BLPTC1
358800060221     C*    §LPTC1        ANDNE     BLPTC2
358900060221     C*    BLPTC2        ANDEQ     *BLANKS
359000060221     C*                  MOVEL     §LPTC1        BLPTC2
359100060221     C*                  ENDIF
359200060221     C*    §LPTC2        IFNE      *BLANKS
359300060221     C*    §LPTC2        ANDNE     BLPTC1
359400060221     C*    §LPTC2        ANDNE     BLPTC2
359500060221     C*    BLPTC1        ANDEQ     *BLANKS
359600060221     C*                  MOVEL     §LPTC2        BLPTC1
359700060221     C*                  ENDIF
359800060221     C*    §LPTC2        IFNE      *BLANKS
359900060221     C*    §LPTC2        ANDNE     BLPTC1
360000060221     C*    §LPTC2        ANDNE     BLPTC2
360100060221     C*    BLPTC2        ANDEQ     *BLANKS
360200060221     C*                  MOVEL     §LPTC2        BLPTC2
360300060221     C*                  ENDIF
360400001116     C* SE IL CLIENTE MI PASSA "F"=FUORI MISURA E SULLA BOLLA TC1 E TC2
360500001116     C* SONO PIENI E DIVERSI DA 'F' PERDO TC2 E METTO 'F'
360600060221     C*    §LPTC1        IFEQ      'F'
360700060221     C*    §LPTC2        OREQ      'F'
360800060221     C*    BLPTC1        IFNE      'F'
360900060221     C*    BLPTC2        ANDNE     'F'
361000060221     C*                  MOVEL     'F'           BLPTC2
361100060221     C*                  ENDIF
361200060221     C*                  ENDIF
361300000615     C*
361400060221     C*                  CLEAR                   BLPFT1
361500060221     C*                  CLEAR                   BLPFT2
361600060221     C*                  CLEAR                   BLPDT1
361700060221     C*                  CLEAR                   BLPDT2
361800060221     C*                  Z-ADD     99999         BLPNFV
361900060221     C*                  Z-ADD     DATEU         BLPDBR
362000000523     C*
362100060221     C*                  UPDATE    FNBLP000
362200000626     C*
362300060221     C*                  MOVEL     'S'           RICLSA            1
362400000524     C* ESTENSIONE RAGIONE SOCIALE
362500060221    3C*    §LPRD2        IFNE      *BLANKS
362600060221     C*                  MOVEL     'P'           D19TBO
362700060221     C*                  MOVEL     BLPAAS        D19AAS
362800060221     C*                  MOVEL     BLPLNP        D19LNP
362900060221     C*                  MOVEL     BLPNRS        D19NRS
363000060221     C*                  MOVEL     BLPNSP        D19NSP
363100060221     C*                  MOVEL     'D'           D19TRC
363200060221     c*                  Eval      wtxt = §lpRd2
363300060221     c*                  ExSr      Sr_Chk
363400060221     c*                  Movel(p)  wtxt          d19Nt1
363500040301     C**!!!              MOVEL(P)  §LPRD2        D19NT1
363600060221     C*                  CLEAR                   D19NT2
363700060221     C*                  CALL      'FNLV19R'
363800060221     C*                  PARM                    DSLV19
363900060221    3C*                  ENDIF
364000000524     C* NOTE DEL MITTENTE PER IL DESTINATARIO
364100060221    3C*    §LPNOT        IFNE      *BLANKS
364200060221     C*    §LPNT2        ORNE      *BLANKS
364300060221     C*                  MOVEL     'P'           D19TBO
364400060221     C*                  MOVEL     BLPAAS        D19AAS
364500060221     C*                  MOVEL     BLPLNP        D19LNP
364600060221     C*                  MOVEL     BLPNRS        D19NRS
364700060221     C*                  MOVEL     BLPNSP        D19NSP
364800060221     C*                  MOVEL     '8'           D19TRC
364900060221     c*                  Eval      wtxt = §lpNot
365000060221     c*                  ExSr      Sr_Chk
365100060221     c*                  Movel(p)  wtxt          d19Nt1
365200040301     C**!!!              MOVEL(P)  §LPNOT        D19NT1
365300060221     c*                  Eval      wtxt = §lpNt2
365400060221     c*                  ExSr      Sr_Chk
365500060221     c*                  Movel(p)  wtxt          d19Nt2
365600040301     C**!!!              MOVEL(P)  §LPNT2        D19NT2
365700060221     C*                  CALL      'FNLV19R'
365800060221     C*                  PARM                    DSLV19
365900060221    3C*                  ENDIF
366000000524     C* C/A     VALORIZZATO
366100060221    3C*    §LPCAS        IFGT      *ZEROS
366200051115     C* AGGANCIARE IL ar9
366300060221     C*    KBLPPT        CHAIN     Fiar901L                           39
366400000525     C*
366500060221     C*                  Z-ADD     §LPCAS        ar9CAS
366600060221     C*                  MOVEL     §LPVCA        ar9VCA
366700060221     C*                  Z-ADD     §LPCMB        ar9CMB
366800000525     C*
366900060221     C*                  MOVEL     §LPGCA        ar9GCA
367000060221     C*                  MOVEL     §LPTIC        ar9TIC
367100060221     c*                  movel     dateu         ar9duv
367200000525     C*
367300060221     C*    *IN39         IFEQ      *ON
367400060221     C*                  MOVEL     BLPAAS        ar9AAS
367500060221     C*                  MOVEL     BLPLNP        ar9LNP
367600060221     C*                  MOVEL     BLPNRS        ar9NRS
367700060221     C*                  MOVEL     BLPNSP        ar9NSP
367800060221     C*                  WRITE     Fiar9000
367900060221     C*                  ELSE
368000060221     C*                  UPDATE    Fiar9000
368100060221     C*                  ENDIF
368200000525     C*
368300060221    3C*                  ENDIF
368400000524     C*
368500060221    2C*                  ENDIF
368600060221     C* AGGIORNO IL TIPO RECORD W DEL ar4 PER TRASMISSIONE VAB AL CLIENTE
368700000531     C*
368800060221     C*                  MOVEL     'W'           WPTTRC
368900000531     C*
369000060221     C*    KBL4PT        CHAIN     Fiar401L                           37
369100000531     C*
369200060221     C*    *IN37         IFEQ      *OFF
369300060221     C*                  MOVEL     BL4NOT        DSBL4W
369400060221     C*                  MOVEL     *ZEROS        §B4DAT
369500060221     C*                  MOVEL     DSBL4W        BL4NOT
369600000531     C*
369700060221     C*                  UPDATE    Fiar4000
369800000531     C*
369900060221     C*                  ENDIF
370000000627     C*
370100060221     C*                  ENDIF
370200000525     C*
370300060221     C*                  ENDSR
370400000705     C*
370500000705     C*--- CONTROLLO I CODICI CLIENTI CHE SONO LEGATI AL NR.SERIE-----*
370600120903     C*    CTRNRS        BEGSR
370700000717     C*
370800120903     C*                  Z-ADD     *ZEROS        C                 3 0
370900000717     C*
371000120903     C*                  MOVEL     '3C'          COD
371100120903     C*                  MOVEL     *BLANKS       KEY
371200120903     C*                  MOVEL     VI4FGS        KEY
371300000705     C*
371400120903     C*    KTAB          SETLL     TABEL00F
371500000705     C*
371600120903     C*                  DO        *HIVAL
371700000705     C*
371800120903     C*    KTAB2         READE     TABEL00F                               30
371900120903     C*
372000120903     C*  30              LEAVE
372100120903     C* CONTROLLO SE LA FILIALE DEL CLIENTE È GIUSTA
372200120903     C*                  MOVEL     TBLKEY        W0030             3 0
372300120903     C*    W0030         IFNE      VI4FGS
372400120903     C*                  LEAVE
372500120903     C*                  ENDIF
372600120903     C*
372700120903     C*                  MOVEL     TBLUNI        DS3C
372800000705     C* E LA SERIE È GIUSTA
372900000825     C*  ESCLUDO GLI ANNULLATI
373000120903     C*    §3CNRS        IFNE      VI4NRS
373100120903     C*    TBLFLG        ORNE      ' '
373200120903     C*                  ITER
373300120903     C*                  ENDIF
373400120903     C* CONTROLLO SE IL CLIENTE È  GIUSTO
373500120903     C*                  MOVEL     TBLKEY        KSC
373600120903     C*
373700120903     C*                  EXSR      CTRCCM
373800120903     C* SE NON HA L'* E' COME SE NON CI FOSSE LA 3C
373900120903    1C* N90§3COKD        IFNE      '*'
374000120903     C*                  SETON                                        06
374100120903    1C*                  ENDIF
374200120903     C* SE C'È ERRORE OPPURE E' ACCESO 06 (CLIENTE ANCORA IN ATTESA DI PARTIRE
374300120903     C* CON LA PROCEDURA) NON CARICO IN SCHIERA
374400120903     C*    *IN90         IFEQ      *OFF
374500120903     C*    *IN06         ANDEQ     *OFF
374600120903     C*                  ADD       1             C
374700120903     C*                  MOVE      WCCMN         CLI(C)
374800120903     C*                  ENDIF
374900120903     C* SPENGO INDICATORI DI ERRORE
375000120903     C*                  SETOFF                                       904043
375100120903     C*                  SETOFF                                       6306
375200120903     C*                  MOVEL     '3C'          COD
375300120903     C*                  MOVEL     KSC           KEY
375400120903     C*
375500120903     C*    KTAB          SETGT     TABEL00F
375600000718     C*
375700120903     c*                  ENDDO
375800000705     C*
375900120903     C*                  ENDSR
376000911031     C*--- CREO DS DA PASSARE ----------------------------------------*
376100911114     C     CRTDS         BEGSR
376200991026     C                   CLEAR                   DBLP
376300991026     C                   CLEAR                   DTASV
376400950407     C                   MOVEL     'B'           WBOL              1
376500911031     C*
376600020910     C* P.O DI GESTIONE
376700020917     C                   MOVE      VABFGS        §LPFGS
376800020910      *
376900941010     C                   MOVEL     VABLNP        §LPLNP
377000001221     C* SE STO CONFERMANDO BOLLA POSTE PASSO ANCHE LA DATA SPEDIZIONE
377100001221     C* DEL CLIENTE
377200001221     C     WRKPT         IFEQ      'S'
377300010219     C                   MOVEL     VABAAS        §LPDSK
377400010219     C                   MOVE      VABMGS        §LPDSK
377500001221     C                   ENDIF
377600001221     C                   MOVEL     COMDSP        §LPAAS
377700911114     C                   MOVE      COMDSP        §LPMGS
377800941010     C                   MOVEL     VABNRS        §LPNRS
377900941010     C                   MOVEL     VABNSP        §LPNSP
378000970326     C                   MOVEL     VABAAS        §LPAAC
378100911031     C*
378200941010     C                   MOVEL     VI5KSC        §LPCCM
378300941010     C                   MOVEL     WCCMN         §LPCCN
378400941010     C                   MOVEL     VABCBO        §LPCBO
378500911031     C*
378600011206     C                   MOVEL     VABLNA        §LPLNA
378700011206     C                   MOVEL     VABZNC        §LPZNC
378800040301     c                   Eval      wtxt = VabRsd
378900040301     c                   ExSr      Sr_Chk
379000040301     c                   Movel     wtxt          §LpRsd
379100040301     C**!!!              MOVEL     VABRSD        §LPRSD
379200040301     c                   Eval      wtxt = VabRd2
379300040301     c                   ExSr      Sr_Chk
379400040301     c                   Movel     wtxt          §LpRd2
379500070403     c                   if        §lprd2=§lprsd
379600070403     c                   clear                   §lprd2
379700070403     c                   endif
379800040301     C**!!!              MOVEL     VABRD2        §LPRD2
379900040301     c                   Eval      wtxt = VabInd
380000040301     c                   ExSr      Sr_Chk
380100040301     c                   Movel     wtxt          §LpInd
380200040301     C**!!!              MOVEL     VABIND        §LPIND
380300040301     c                   Eval      wtxt = VabLod
380400040301     c                   ExSr      Sr_Chk
380401140110     c* eseguo routine per eliminare caratteri di punteggiatura precedenti e
380402140110     c* seguenti la località
380403140207     c****               exsr      chkstringa
380500040301     c                   Movel     wtxt          §LpLod
380600040301     C**!!!              MOVEL     VABLOD        §LPLOD
380700040301     c                   Eval      wtxt = VabCad
380800040301     c                   ExSr      Sr_Chk
380900040301     c                   Movel     wtxt          §LpCad
381000040301     C**!!!              MOVEL     VABCAD        §LPCAD
381100941010     C                   MOVEL     VABPRD        §LPPRD
381200941010     C                   MOVEL     VABNZD        §LPNZD
381300941010     C                   MOVEL     VABGC1        §LPGC1
381400941010     C                   MOVEL     VABGC2        §LPGC2
381500041222     C**!!!              MOVEL     VABCTR        §LPCTR
381600941010     C                   MOVEL     VABTSP        §LPTSP
381700941010     C                   MOVEL     VABCTS        §LPCTS
381800941010     C                   MOVEL     VABFTM        §LPFTM
381900941010     C                   MOVEL     VABIAS        §LPIAS
382000941010     C                   MOVEL     VABVAS        §LPVAS
382100011030      * Se importo valorizzato e divisa no, la imposto 'ITL'
382200011030     C     VABIAS        IFGT      0
382300011030     C     VABVAS        ANDEQ     *BLANKS
382400011030     C                   MOVEL     'ITL'         §LPVAS
382500011030     C                   ENDIF
382600011030      *
382700941010     C                   MOVEL     VABVMD        §LPVMD
382800011030     C                   MOVEL     VABVAD        §LPVAD
382900011030      * Se importo valorizzato e divisa no, la imposto 'ITL'
383000011030     C     VABVMD        IFGT      0
383100011030     C     VABVAD        ANDEQ     *BLANKS
383200011030     C                   MOVEL     'ITL'         §LPVAD
383300011030     C                   ENDIF
383400011030      *
383500941010     C                   MOVEL     VABQFT        §LPQFT
383600941010     C                   MOVEL     VABNAS        §LPNAS
383700941010     C                   MOVEL     VABNCL        §LPNCL
383800941010     C                   MOVEL     VABPKB        §LPPKB
383900941010     C                   MOVEL     VABVLB        §LPVLB
384000950301     C     VABVLB        IFGT      0
384100950301     C                   MOVEL     'R'           §LPFVB
384200950301     C                   ELSE
384300950301     C                   CLEAR                   §LPFVB
384400950301     C                   ENDIF
384500941010     C                   MOVEL     VABCAS        §LPCAS
384600941010     C                   MOVEL     VABVCA        §LPVCA
384700011030      * Se importo valorizzato e divisa no, la imposto 'ITL'
384800011030     C     VABCAS        IFGT      0
384900011030     C     VABVCA        ANDEQ     *BLANKS
385000011030     C                   MOVEL     'ITL'         §LPVCA
385100011030     C                   ENDIF
385200011030      *
385300941010     C                   MOVEL     VABTIC        §LPTIC
385400941010     C                   MOVEL     VABTC1        §LPTC1
385500000606     C                   MOVEL     VABTC2        §LPTC2
385600941010     C                   MOVEL     VABRMN        §LPRMN
385700040301     c                   Eval      wtxt = VabRma
385800040301     c                   ExSr      Sr_Chk
385900040301     c                   Movel     wtxt          §LpRma
386000040301     C**!!!              MOVEL     VABRMA        §LPRMA
386100040301     c                   Eval      wtxt = VabRmo
386200040301     c                   ExSr      Sr_Chk
386300040301     c                   Movel     wtxt          §LpRmo
386400040301     C**!!!              MOVEL     VABRMO        §LPRMO
386500040301     c                   Eval      wtxt = VabCmo
386600040301     c                   ExSr      Sr_Chk
386700040301     c                   Movel     wtxt          §LpCam
386800040301     C**!!!              MOVEL     VABCMO        §LPCAM
386900941010     C                   MOVEL     VABNMO        §LPNZM
387000941010     C                   MOVEL     VABNCA        §LPNCA
387100941010     C                   MOVEL     VABNCD        §LPNCD
387200941010     C                   MOVEL     VABXCO        §LPXCO
387300040301     c                   Eval      wtxt = VabNot
387400040301     c                   ExSr      Sr_Chk
387500040301     c                   Movel     wtxt          §LpNot
387600040301     C**!!!              MOVEL     VABNOT        §LPNOT
387700040301     c                   Eval      wtxt = VabNt2
387800040301     c                   ExSr      Sr_Chk
387900040301     c                   Movel     wtxt          §LpNt2
388000040301     C**!!!              MOVEL     VABNT2        §LPNT2
388100011121      *
388200011121      * CODICE TRATTAMENTO MERCE: IMPOSTO DA VIDEO SE PRESENTE
388300011121      *        OPPURE SE DISK B CHE DIVENTA DISK C DA TABELLA "1B"
388400020917      *        OPPURE DA FIVAB
388500011121      *
388600920324     C     VI4CTM        IFNE      '  '
388700920324     C                   MOVEL     VI4CTM        §LPCTM
388800920325     C                   ELSE
388900001006     C     WWCTC         IFNE      '  '
389000001006     C                   MOVEL     WWCTC         §LPCTM
389100001006     C                   ELSE
389200941010     C                   MOVEL     VABCTM        §LPCTM
389300001006     C                   ENDIF
389400920324     C                   END
389500041222      * Codice tariffa
389600041222     c                   If        vi4ctr <> *Blanks
389700041222     c                   Move      vi4ctr        §lpctr
389800041222     c                   Else
389900041222     c                   Move      vabctr        §lpctr
390000041222     c                   EndIf
390100950407     C* SE SI TRATTA DI CLIENTE CON    SERIE --> DISK "B" METTO "%"
390200950407     C* SE SI TRATTA DI CLIENTE SENZA  SERIE --> DISK "A" METTO "§"
390300950407     C                   MOVEL     '§'           §LPSOP
390400950407     C     §LPCTM        IFNE      *BLANKS
390500950407     C     §1BFG1        ANDEQ     'S'
390600950407     C                   MOVEL     '%'           §LPSOP
390700950407     C                   ENDIF
390800941010     C                   MOVEL     VABFFD        §LPFFD
390900941010     C                   MOVEL     VABDCR        §LPDCR
391000941010     C                   MOVEL     VABHCR        §LPHCR
391100941010     C     VABTCR        IFNE      'D'
391200941010     C     VABTCR        ANDNE     'P'
391300930528     C                   MOVEL     ' '           §LPTCR
391400930528     C                   ELSE
391500941010     C                   MOVEL     VABTCR        §LPTCR
391600930528     C                   END
391700030409      * se non c'è la consegna richiesta nel VAB
391800030411     c                   If        VabDcr = *Zeros and VabHcr = *Zeros
391900030409      * se il p.o. gestione non è un p.o. poste
392000030409     c                             and wrkpt <> 'S'
392100030409      * passo i dati della consegna richiesta (tipo-data-ora)
392200030409     c                   Eval      §lpTcr = Vi4Tcr
392300030409     c                   Eval      §lpDcr = wdcr
392400030409     c                   Movel     Vi4Hcr        §lpHcr
392500030410      * passo le ore da sommare alla delivery
392600030409     c                   Move      Vi4Ore        §LpOre
392700030409     c                   EndIf
392800930528     C*
392900950321     C                   MOVE      VI4PDR        §LPPDR
393000950321     C                   MOVEL     VI4LPD        §LPPDR
393100941010     C                   MOVEL     VI4FPP        §LPFPP
393200941010     C                   MOVEL     VI4NRT        §LPNRT
393300911031     C                   MOVEL     COMDRT        §LPDRT
393400000516     C     VI5SCE        IFNE      *BLANKS
393500050407     C                   MOVEL     VI5SCE        §LPFPR
393600000516     C                   ELSE
393700000516     C     VI4SCE        IFEQ      'X'
393800000516     C     TOTA          ANDEQ     'S'
393900050407     C                   MOVEL     'W'           §LPFPR
394000000516     C                   ELSE
394100050407     C                   MOVEL     VI4SCE        §LPFPR
394200000516     C                   ENDIF
394300000516     C                   ENDIF
394400941010     C                   MOVEL     VABGCA        §LPGCA
394500941010     C                   MOVEL     VABGGA        §LPGGA
394600941010     C                   MOVEL     VABGMA        §LPGMA
394700941010     C                   MOVEL     VABGVA        §LPGVA
394800970407     C* RICHIAMO DA CONFERMA BOLLA
394900970407     C                   MOVEL     'C'           §LPRIC
395000010219     C* PASSO NUMERO ORM
395100060926     C***  WOBBL         IFEQ      'S'
395200060926     C     vidorm        IFgt      0
395300010219     C                   Z-ADD     VI4POE        §LPPOE
395400010227     C* SE PO EMITTENTE = 0 SIGNIFICA CHE L'ROM E FITTIZIO: IN QUESTO
395500010227     C* CASO FORZO IL PO DEL PADRONCINO
395600010227     C     §LPPOE        IFEQ      *ZEROS
395700010227     C                   MOVEL     §LPPDR        §LPPOE
395800010227     C                   ENDIF
395900010219     C                   Z-ADD     VI4NSR        §LPNSR
396000010223     C                   Z-ADD     VI4NO2        §LPNOR
396100010219     C                   Z-ADD     VI4NRV        §LPNRV
396200010220     C                   ELSE
396300010220     C                   CLEAR                   §LPPOE
396400010220     C                   CLEAR                   §LPNSR
396500010220     C                   CLEAR                   §LPNOR
396600010220     C                   CLEAR                   §LPNRV
396700010220     C                   ENDIF
396800061122     c* Bolla import dpd: cerco su FIDPO00F se presente numero parcel
396900061121     c* se lo trovo passo a fnls01r l'orm recuperato da questo archivio
397000061122     c* e forzo codice bolla = '2'
397100061121     c                   move      §lplnp        worg
397200061121     c                   exsr      ctrorg
397300061122    1c                   if        §ogntw='DPD'
397400061122     c* Cerco numero parcel
397500061122     C                   MOVEL     'E'           WTRC
397600061122     C     KVAT          chain(N)  FIVAT01L
397700061122    2c                   if        %found(fivat01l)
397800061122     c                   movel     vatnot        dvate
397900061122     c     §vateprn      chain     fidpo06l
398000061122     c                   if        %found(fidpo06l)
398100061127     c     korm_dpo      chain     fnorm01l
398200061127     c                   if        %found(fnorm01l)  and ormfcs='F'
398300061122     C                   z-add     dpopoe        §LPPOE
398400061122     C                   z-add     dponrs        §LPNSR
398500061122     C                   z-add     dponor        §LPNOR
398600061122     C                   z-add     dponrv        §LPNRV
398700061122     c                   movel     '2 '          §lpcbo
398800061127     c                   endif
398900061122     c                   endif
399000061122    2c                   endif
399100061122    1c                   endif
399200030409
399300021220      * Imposto i campi dei bancali
399400021220     c                   Eval      §LpTba = VabTba
399500021220     c                   Eval      §LpNba = VabNba
399600021220     c                   Eval      §LpTb2 = VabTb2
399700021220     c                   Eval      §LpNb2 = VabNb2
399800021220
399900911031     C                   ENDSR
400000040301      *---------------------------------------------------------
400100040301      * Controllo i caratteri dei campi alfanumerici
400200040301      *---------------------------------------------------------
400300040301     c     Sr_Chk        BegSr
400400040301
400500040301     c                   Eval      TxtInOut = wtxt
400600040301     c                   Call      'XCHKCHAR'
400700040301     c                   Parm                    TxtInOut
400800040301     c                   Parm                    ElencoChar
400900040301     c                   Parm                    TipoElenco
401000040301     c                   Parm                    CharSost
401100040301     c                   Parm                    UpperCase
401200040301     c                   Parm                    ChkNull
401300040301     c                   Parm                    CharNull
401400040301     c                   Parm                    Esito
401500040301     c                   If        Esito = '1'
401600040301     c                   Move      TxtInOut      wtxt
401700040301     c                   EndIf
401800040301
401900040301     c                   EndSr
401901140110      *---------------------------------------------------------
401902140110      * Controllo caratteri di punteggiatura precedenti e seguenti
401903140110      *---------------------------------------------------------
401904140110     c     chkstringa    BegSr
401905140110
401906140110     c* trovo la prima posizione "buona" da utilizzare
401907140110     c                   eval      posl=%check(charnook:wtxt)
401908140110     c* trovo l'ultima posizione "buona" da utilizzare
401909140110     c                   eval      posr=%checkr(charnook:wtxt)
401910140110     c                   if        posl=0
401911140110     c                   clear                   wtxt
401912140110     c                   else
401913140110     c* determino la lunghezza della stringa "buona"
401914140110     c                   eval      len=(posr-posl)+1
401915140110     c                   eval      wtxt=%subst(wtxt:posl:len)
401916140110     c                   endif
401917140110
401918140110     c                   EndSr
402000111010     C*
402100111010     C*-------------------------------------------------------------*
402200111010     C*     -- Annullamento vab/vad richiesto da fnls01r    --      *
402300111010     C*-------------------------------------------------------------*
402400111010     C     ges_ann       BEGSR
402500111010     C*
402600111010     c                   exsr      annorm
402700111010     C                   movel     vi5nrr        WNRR              9 0
402800111010     c                   exsr      annrec
402900111010     c                   endsr
403000111010     C*-------------------------------------------------------------*
403100111010     C*     -- Annullamento orm legato se annullata bolla RC --     *
403200111010     C*-------------------------------------------------------------*
403300111010     C     annorm        BEGSR
403400111010      * Prima di annullare la spedizione se bolla RC vado ad annullare
403500111010      * il relativo ORM legato
403600111010     C                   CLEAR                   DS7R
403700111010    3c                   If        vabgma <>*blanks
403800111010     C                   MOVEL     '7R'          COD
403900111010     C                   MOVEL(P)  vabgma        KEY
404000111010     C     KTAB          CHAIN     TABEL                              30
404100111010     C  N30              MOVEL     TBLUNI        DS7R
404200111010    3c                   endif
404300111010     C*
404400111010    3c                   If        §7rRC='S'
404500111010     c                   Clear                   fnls25ds
404600111010     c                   Eval      i25Ksc = vabccm
404700111010     c                   Move      vabaas        i25aas
404800111010     c                   Eval      i25spe = %editc(vablnp:'X') +
404900111010     c                                      %editc(vabnrs:'X') +
405000111010     c                                      %editc(vabnsp:'X')
405100111010     c                   Call      'FNLS25R'
405200111010     c                   Parm                    Fnls25ds
405300111010    3c                   EndIf
405400111010     c                   endsr
405500030611      *---------------------------------------------------------
405600950112     OFNLV22P   T    LRNLR
405700030611      *---------------------------------------------------------
405800030611**  tes
405900950407 ** CONFERMA BOLLE  DA CLIENTI **
406000950407 ** RECUPERO BOLLE  DA CLIENTI **
406100000518 * CONFERMA BOLLE CLIENTI POSTE *
406200030611**  cm3
406300950112OVRPRTF FILE(FNLV22P) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
406400941011 SHARE(*YES)          USRDTA('Segnacolli')
406500030611**  cmA4
406600030611OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
406700030611 SHARE(*YES)          USRDTA('Boll.Dk A4')
406800030611**  cmA5
406900030611OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
407000030611 SHARE(*YES)          USRDTA('Boll.Dk A5')
407100030611**  err
407200960103Data Spedizione formalmente errata  o  superiore a data sospensione xx/xx/xx
407300020801Impossibile utilizzare cod trattamento merce "xx"  !!
407400990107E' stato forzato il cod trattamento merce: controllare e enter per proseguire
407500990205Codice trattamento merce non forzabile:utilizzare solo codici per soluzioni"A"
407600000518Codice trattamento merce non forzabile
407700010221Immettere il numero O.R.M.
407800010219Numero O.R.M. errato
407900010220Il p.o. di ritiro definito sull'O.R.M. è diverso dal p.o. autotrasportatore    8
408000010220Il numero O.R.M. non deve essere un prepagato                                  9
408100010220Immettere un O.R.M. il cui pagamento non sia a carico del destinatario         0
408200010220O.R.M. in fase non bollettabile. ENTER PER FORZARE                             1
408300010220Se immesso riferimento immettere anche numero O.R.M.                           2
408400010223O.R.M. non utilizzabile in conferma bolle: presente forzatura linea di arrivo  3
408500030409Data cons.richiesta formalmente errata o inferiore/uguale alla data odierna    4
408600051215Data consegna richiesta non ammessa: oltre                                     5
408700030411Immettere o i dati della consegna richiesta o il numero delle ore              6
408800030409Se immesso tipo immettere anche la data o l'ora Consegna Richiesta             7
408900030411Immettere il cliente o la serie se immessi i dati della Consegna Richiesta     8
409000030415Le ore da aggiungere alla delivery devono essere:  24 - 48 - 72                9
409100041122LINEA DI ARRIVO errata                                                         0
409200041222Codice tariffa non valido                                                     21
409300041222E' stato forzato il codice tariffa: controllare e ENTER per proseguire        22
409400060306Cliente con obbligo parzializzazione linea di arrivo                          23
409500080110Data Ritiro Merce errata                                                      24
409600080110Già confermati i conteggi autotrasportatore nella data ritiro richiesta       25
409700130121Non inserire rif.mittente se richiesto ordinamento diverso                    26
409800130121Codice cliente obbligatorio se richiesto ordinamento per RIF.ALFA o NUM.SPED  27
409900110419Immettere l'Autotrasportatore per interrogare l'O.R.M.                        28
410000111214Linea di Partenza Segnacollo errata                                           29
410100111222Presenti più clienti da confermare per la serie richiesta                     30
410200111220Presenti più clienti da confermare per la serie/linea par:immettere Cd.Cliente31
410300130313Utilizzare solo Cod.Tratt.Merce per soluzione "A" o senza Packing List/PDF    32
410400130313Cd.forzabile solo x sped. di clienti con PackingList/PDF non fornito:EnterForz33
