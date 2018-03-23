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
007801170623     d s_inkf          s              1    Inz(*Off)
007802170623     d s_inkq          s              1    Inz(*Off)
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
016800150909     D DBLP01        E DS
016900060306     D DCLI          E DS
017000911031     D DS3C          E DS
017100970512     D DS3N          E DS
017200020926     D  P3N                   33     89  0 DIM(19)
017300960103     D DS3I          E DS
017400910311     D DS1B          E DS
017500100604     D DS7R          E DS
017600000512     D OG143         E DS
017700000626     D OG148         E DS
017800001006     D OG150         E DS
017900060221      * DS PER FNLV19R - PER SCRITTURA Fiar400F
018000060221     D*** DSLV19        E DS                  EXTNAME(FNLV19DS)
018100030611      * DS PER TIBS02R - GESTIONE TNTBE00F
018200000525     D DSBS02        E DS                  EXTNAME(TIBS02DS)
018300991026     D DTASV         E DS                  INZ
018400020910     d FNLSE5DS      e ds
018500051207     d tntb50ds      e ds
018600100517     d trulNRSds     e ds
018700080110      * - Aggiunta/Sottrazione di n giorni lavorativi ad una data
018800080110     d TRUL52ds      e ds
018900031008     d Fnls80ds      e ds
019000031008     d  dPog                   1     90  0 Dim(30)
019100020910      *------------
019200020910      * DS DATI UTENTE
019300020910      *------------
019400020910     d tibs34ds      e ds                  inz
019500020910     d ddatiute      e ds
019600020910     d azuteds       e ds                  extname(AZUTE00F)
019700020910      *
019800010219     D DFAR          E DS
019900010219     D  §FARDS       E                     EXTFLD(D§FARDES)
020000010219     D  §FARFB       E                     EXTFLD(D§FARFFB)
020100010716     D  §FARNF       E                     EXTFLD(D§FARNONF)
020200010716     D  §FARMA       E                     EXTFLD(D§FARMAN)
020300010716     D  §FARST       E                     EXTFLD(D§FARSTP)
020400010716     D  §FARAF       E                     EXTFLD(D§FARAFVV)
020500010716     D  §FARCF       E                     EXTFLD(D§FARCFVV)
020600011025     D  §FARNO       E                     EXTFLD(D§FARNOO)
020700011025     D  §FARUV       E                     EXTFLD(D§FARUVA)
020800011025     D  §FARDV       E                     EXTFLD(D§FARDVA)
020900041202     D Dvpo          E DS
021000070803     D Dvpodeco      E DS
021100110419     d dVpols01      e ds
021200110419     D  fio                    1     60  0 dim(20)
021300061122     D Dvate         E DS
021400010219     D* DS RICHIAMO PROGRAMMA RICERCA O.R.M.
021500150929     D*DSOR04        E DS                  EXTNAME(FIOR04DS)
021600110419     D fior58ds      E DS
021700060130      * ds x richiamo pgm FNLS25R
021800060130     d Fnls25ds      e ds
021900010219     D*
022000010219     D* DS PER RICHIAMO FIOR07R PER VISUALIZZAZIONE ORM
022100010219     D FIOR07          DS
022200010219     D  D07POE                 1      3  0
022300010219     D  D07NOR                 4     10  0
022400010219     D  D07NSR                11     12  0
022500010219     D  D07NRV                13     14  0
022600010219     D  D07SCE                15     15
022700010219     D  D07FGS                16     18  0
022800010219     D  D07POR                19     21  0
022900010219     D  D07DTR                22     29  0
023000010219     D  D07MDB                30     39
023100010219     D  D07PRB                40     49
023200010219     D  D07DTS                50     57  0
023300010219     D  D07RMP                58     58
023400010219     D  D07BRC                59     59
023500010219     D  D07REF                60     61
023600010219     D  D07MIO                62     62
023700010219     D                 DS
023800010219     D  VI4POE                 1      3  0
023900010219     D  VI4NSR                 4      5  0
024000010223     D  VI4NO2                 6     12  0
024100010223     D  VI4NOR                 4     12  0
024200010219     D  VI4NRV                13     14  0
024300010227     D  WIDORM                 4     14  0
024400010227     D  VIDORM                 1     14  0
024500021203
024600021203     D kpdpdr          S                   LIKE(APDpdr)
024700021203     D kpdtip          S                   LIKE(APDtip)
024800070906     d s_drt           s                   like(vi4drt)
024900081215     d ntwpor          s                   like(§ogntw)
025000111216     D preccm          S                   LIKE(vabccm)
025100111216     D prelnp          S                   LIKE(vablnp)
025200111216     D wctrs           S              1
025300120710     D wctrctm         S              1
025400120710     D w1b             S             13
025500120831     d wconf           s              1    inz(*Off)
025600120903     d w0030           s              3  0
025700140110     d posl            s              2  0
025800140110     d posr            s              2  0
025900140110     d len             s              2  0
026000140110
026100140110     d charnook        C                   const('.:,;+<> ')
026200930609     C*---------------------------------------------------------------*
026300900518     C*  RIEPILOGO INDICATORI
026400930609     C*---------------------------------------------------------------*
026500930609     C* 01    - CLIENTE CON PIU' DI 9900 BOLLE
026600900807     C* 02-03 - INDICANO IMMISSIONE CODICE CLIENTE E RIF,MITT IN 1 VID.
026700901116     C* 04    - POSIZIONAMENTO CURSORE PRIMO GIRO SU KSC
026800910916     C* 05    - KSC+RIFERIM MA PIU' DI UNA BOLLA
026900920410     C* 06    - NON ESISTE TABELLA DEL CLIENTE
027000950321     C* 07    - NON VISUALIZZO  TUTTA LA BOLLA SE CONFERMA CON F6
027100000704     C* 08    - INDICA IMMISSIONE NUMERO SERIE IN CONFERMA TOTALE BOLLE
027200911031     C* 10-11 - USATO A VIDEO
027300911031     C* 12    - PRESENTI DEGLI ERRORI
027400001128     C* 16    - DISABILITO I COMANDI DI SELEZIONE E ANNULLAMENTO TOTALE
027500900518     C* 17-19 - INDICANO IL TIPO DI ARCHIVIO(IN PART.;PART.;ENTRAM)SCEL
027600900518     C* 20    - PULISCO SCOCOLLO AL SE RIEMPITO SOLO PER CONFRONTO
027700040604     c* 27    - Errore
027800900518     C* 30    - FINE LETTURA FILE
027900900518     C* 31    - COMODO
028000900518     C* 32    - BOLLA LETTA DA SCARTARE
028100960319     C* 33    - COMODO
028200920203     C* 34    - LETTURA SUBFILE PER RECORD SCELTI
028300080204     C* 37    - COMODO
028400060306     C* 39-79 - ERRORI
028500900518     C* 90    - INDICA LA PRESENZA DI UN ERRORE
028600900808     C* 91    - TORNO A VIDEATA UNO SE CMD12 DA RICHIESTA DI BOLLA SING
028700900808     C* 98-99 - ERRORE
028800930609     C*---------------------------------------------------------------*
028900941010     C* RICHIESTA STAMPANTI
029000030611     C                   CLEAR                   trul90ds
029100941010     C*
029200030611     C                   MOVEL     'S'           D90RSE
029300030611     C                   MOVEL     'S'           D90RSB
029400030611     C                   CALL      'TRUL90R'
029500941010     C                   PARM                    KPJBA
029600030611     C                   PARM                    trul90ds
029700941010     C**
029800941010     C* F3 - FINE
029900030611     C     D90F3         IFEQ      '1'
030000941010     C                   GOTO      FINE
030100941010     C                   ENDIF
030200030611      *
030300030611      * OVRPRTF BOLLE A4
030400030611     C                   MOVEA     D90PRB4       CMA4(30)
030500030611     C                   MOVEA     D90MDB4       CMA4(52)
030600030611     C*
030700080710     C                   Z-ADD     110           LUNG             15 5
030800080710     C                   CLEAR                   COMMAN          110
030900030611     C                   MOVEA     CMA4          COMMAN
031000030611     C                   CALL      'QCMDEXC'                            91
031100030611     C                   PARM                    COMMAN
031200030611     C                   PARM                    LUNG
031300030611     C*
031400030611     C   91              GOTO      FINE
031500030611      *
031600030611      * OVRPRTF BOLLE A5
031700030611     C                   MOVEA     D90PRB5       CMA5(30)
031800030611     C                   MOVEA     D90MDB5       CMA5(52)
031900030611     C*
032000030611     C                   Z-ADD     110           LUNG
032100030611     C                   CLEAR                   COMMAN
032200030611     C                   MOVEA     CMA5          COMMAN
032300030611     C                   CALL      'QCMDEXC'                            91
032400030611     C                   PARM                    COMMAN
032500030611     C                   PARM                    LUNG
032600030611     C*
032700030611     C   91              GOTO      FINE
032800991201     C*
032900991201     C* OVRPRTF SEGNACOLLI
033000030611     C                   MOVEA     D90PRE        CM3(28)
033100030611     C                   MOVEA     D90MDE        CM3(50)
033200941010     C*
033300030611     C                   Z-ADD     103           LUNG
033400941010     C                   CLEAR                   COMMAN
033500991201     C                   MOVEA     CM3           COMMAN
033600941011     C                   CALL      'QCMDEXC'                            91
033700941010     C                   PARM                    COMMAN
033800941010     C                   PARM                    LUNG
033900941010     C*
034000950112     C  N91              OPEN      FNLV22P
034100941010     C   91              GOTO      FINE
034200950407     C*
034300941010     C**
034400901116     C     INIZIO        TAG
034500900827     C                   Z-ADD     0             VI4RMN
034600080623     C******             ADD       1             VI4NSP
034700950419     C                   CLEAR                   WSANRR
034800000515     C                   CLEAR                   TOTA
034900030409     c                   Clear                   wdcr
035000071218
035100071218      * se deve mantenere la data consegna richiesta non devo pulire
035200071218     c                   if        §clidcr <> 'S'
035300071218     c                   Clear                   Vi4Tcr
035400071218     c                   Clear                   Vi4Dcr
035500071218     c                   Clear                   Vi4Hcr
035600071218     c                   Clear                   Vi4Ore
035700071219     c                   eval      s_vi4tcr = vi4tcr
035800071219     c                   eval      s_vi4dcr = vi4dcr
035900071219     c                   eval      s_vi4hcr = vi4hcr
036000071219     c                   eval      s_vi4ore = vi4ore
036100071218     c                   endif
036200071218
036300071218     c                   clear                   $conta
036400941011     C*
036500010221     C                   EXSR      ORMOBB
036600010221     C* SE ATTIVATA PROCEDURA ORM MI POSIZIONO SU NUMERO ORM
036700010228     C* SE NON IMMESSO
036800010221     C     WOBBL         IFEQ      'S'
036900010228     C     VIDORM        ANDEQ     *ZEROS
037000010221     C                   SETON                                        82
037100010221     C                   ELSE
037200010221     C* SE NON IMMESSO PADRONCINO, MI RIPOSIZIONO LI
037300941011     C     VI4PDR        IFEQ      *ZEROS
037400941011     C     VI4PDR        OREQ      *BLANKS
037500941011     C                   SETON                                        04
037600941011     C                   ENDIF
037700010221     C                   ENDIF
037800921005     C*
037900900509     C     FOR01         TAG
038000941007     C                   EXFMT     LS66D01
038100990107     C                   SETOFF                                       285364
038200110419     C                   SETOFF                                       6104
038300920324     C** CMD3 - FINE LAVORO
038400900509     C   KC              GOTO      FINE
038500000522     C** CMD18- CAMBIO P.O. GESTIONE
038600060221     C** KS              MOVEL     'S'           FLGF18
038700060221     C** KS              Z-ADD     VI4FGS        POGES
038800060221     C** KS              GOTO      FINE
038900010219     C* CMD14 - INTERROGAZIONE O.R.M.
039000010223     C     *INKH         IFEQ      *ON
039100010220     C     VI4NOR        ANDEQ     *ZEROS
039200150929    2c***                if        %lookup(vi4lpd:fio)=0
039300150929     c***                          and %lookup(999:fio)=0
039400150929     C***                CLEAR                   DSOR04
039500150929     C***                MOVEL     '1'           I04RIC
039600150929     C***                Z-ADD     VI4LPD        I04POR
039700150929     C***                CALL      'FIOR04R'
039800150929     C***                PARM                    KPJBA
039900150929     C***                PARM                    DSOR04
040000150929     C***  O04CM3        IFEQ      *BLANKS
040100150929     C***                Z-ADD     O04POE        VI4POE
040200150929     C***                Z-ADD     O04NSR        VI4NSR
040300150929     C***                Z-ADD     O04NOR        VI4NO2
040400150929     C***                Z-ADD     O04NRV        VI4NRV
040500150929     C***                ENDIF
040600150929   x2c***                else
040700110419    3c                   if        vi4pdr<=*zeros
040800110419     c                   seton                                        0428
040900110419     c                   setoff                                       82
041000110419     c                   movel     err(28)       vidmsg
041100110419   x3c                   else
041200110419     c                   clear                   fior58ds
041300110419     c                   movel     vi4lpd        ior58pdr
041400110419     c                   move      vi4pdr        IOR58PDR
041500110419     c                   CALL      'FIOR58R'
041600110419     c                   PARM                    KPJBA
041700110419     c                   PARM                    fior58ds
041800110419    4c     OOR58F03      IFEQ      *BLANKS
041900110419     c                   Z-ADD     OOR58POE      VI4POE
042000110419     c                   z-add     OOR58NSR      VI4Nsr
042100110419     c                   z-add     OOR58NOR      VI4no2
042200110419     c                   Z-ADD     OOR58NRV      VI4NRV
042300110419    4c                   endif
042400110419    3c                   endif
042500150929    2c***                endif
042600010219     C                   GOTO      FOR01
042700010219     C                   ENDIF
042800900509     C* CONTROLLI FORMATO1
042900941010     C                   EXSR      CTRD01
043000110419     C   90              SETOFF                                       82
043100900509     C   90              GOTO      FOR01
043200071218
043300071218      * se cliente modificato pulisco i dati relativi alla
043400071218      * data consegna richiesta
043500071219      * se rimasta uguale a quella precedente
043600071219      * e quindi riemetto il video
043700071218     c                   if        vi4ksc <> s_vi4ksc
043800071219     c                   eval      s_vi4ksc = vi4ksc
043900071219     c                   if        vi4tcr = s_vi4tcr and
044000071219     c                             vi4dcr = s_vi4dcr and
044100071219     c                             vi4hcr = s_vi4hcr and
044200071219     c                             vi4ore = s_vi4ore and
044300071219     c                             (vi4dcr > 0 or vi4hcro > 0 or
044400071219     c                              vi4hcrm > 0 or vi4ore > 0)
044500071218     c                   Clear                   Vi4Tcr
044600071218     c                   Clear                   Vi4Dcr
044700071218     c                   Clear                   Vi4Hcr
044800071218     c                   Clear                   Vi4Ore
044900071218     c                   goto      for01
045000071219     c                   endif
045100071219      * mi salvo la data consegna richiesta
045200071219     c                   eval      s_vi4tcr = vi4tcr
045300071219     c                   eval      s_vi4dcr = vi4dcr
045400071219     c                   eval      s_vi4hcr = vi4hcr
045500071219     c                   eval      s_vi4ore = vi4ore
045600071219     c                   endif
045700031008
045800031008      * F10 - Riepilogo
045900031008     c                   If        *InKj
046000031008     c                   Clear                   Fnls80ds
046100031008     c                   Movea     Pog           dpog
046200031008     c                   Movel     Vi4Nrs        d80Nsr
046300031008     c                   Movel     Vi4Ksc        d80Ccm
046400050923     c                   Movel     Vi4lna        d80lna
046500031008     c                   Call      'FNLS80R'
046600031008     c                   Parm                    Kpjba
046700031008     c                   Parm                    Fnls80ds
046800031008     c                   GoTo      For01
046900031008     c                   EndIf
047000020909      *
047100000512     C* CMD6 CONFERMA BOLLE CLIENTE E CMD16 ANNULLAMENTO BOLLE CLIENTE
047200000512     C* ESEGUO LE ROUTINE E TORNO ALLA RIEMISSIONE DEL FORMATO
047300000512     C     *INKF         IFEQ      *ON
047400000512     C     *INKQ         OREQ      *ON
047401170623     c                   eval      s_inkf=*inkf
047402170623     c                   eval      s_inkq=*inkq
047403170626     c                   if        *inkq or vi4sce ='X'
047404170526
047405170608     c                   clear                   w01sino
047406170608     c                   if        *inkf
047407170608     c                   evaL      W1CCOS='Conferma'
047408170608     c                   else
047409170608     c                   evaL      W1CCOS='Annullam.'
047410170608     c                   endif
047411170608     c                   eval      w1ccos=%trim(w1ccos) + ' totale codice'+
047412170614     c                             ' ' + %editc(vi4ksc:'Z') + '?'
047413170608     c
047414170526     C                   dou       *inkf=*on  and *in95=*off
047415170526
047416170526     C                   exfmt     ls66w01
047417170526          *in95=*off;
047418170526          if w01sino=*blanks ;
047419170526             *in95=*on;
047420170526             w01msg='Effettuare una scelta';
047421170526          ENDIF;
047422170526
047423170526
047424170526     C                   ENDDO
047425170526     C
047426170526     C                   if        w01sino='N'
047427170526     C                   goto      for01
047428170623     c                   else
047429170623     c                   eval      *inkf=s_inkf
047430170623     c                   eval      *inkq=s_inkq
047431170526     C                   ENDIF
047432170526     c                   endif
047500000515     C*
047600000515     C                   MOVEL     'S'           TOTA              1
047700000512     C* ROUTINE CHE LEGGE DAL FILE VAB ED ESEGUE O CONFERMNA O ANNULLAMENTO
047800000512     C                   EXSR      SUBTOT
047900000718     C                   CLEAR                   VI4KSC
048000000512     C*
048100000512     C* SE CI SONO ERRORI GESTIONE DEL SUBFILE ERRORI
048200000518     C   12              SETON                                        05
048300000512     C   12              GOTO      FORCT2
048400000512     C* SE NON CI SONO ERRORI RIEMETTO LA PRIMA VIDEATA
048500000512     C                   GOTO      INIZIO
048600000512     C*
048700000515     C                   ELSE
048800000515     C                   CLEAR                   TOTA
048900000512     C                   ENDIF
049000900511     C* ELABORAZIONE
049100900808     C     ELATAG        TAG
049200900515     C                   EXSR      ELAB
049300111215     c                   if        *in28=*on
049400111215     C                   GOTO      FOR01
049500111215     c                   endif
049600900808     C*
049700930909     C* 43 ON - SE NON CI SONO RECORD DA CARICA ERRORE
049800900516     C     NRR           IFEQ      0
049900900808     C                   SETON                                        43
050000900516     C                   GOTO      FOR01
050100900516     C                   END
050200941010     C* POSIZIONAMENTO CURSORE
050300110419     C                   SETOFF                                       82
050400900828     C*
050500930909     C* SE RMN  E CE N'E' SOLO UNO VADO SUBITO NEL DETTAGLIO
050600930909     C   03NRR           IFEQ      1
050700950905     C     *IN05         ANDEQ     *OFF
050800910227     C*
050900050407     C                   MOVEL     VI4SCE        VI5SCE
051000900828     C                   EXSR      CONF
051100950206     C*
051200950206     C* SE C'E' ERRORE RITORNO CON LA "V"
051300950419     C* QUESTO CASI NON SI DOVREBBE PIU' VERIFICARE
051400950206     C     *IN90         IFEQ      *ON
051500950407     C* SE ERRORE PRIMA DI ENTRARE NELLA BOLLA --> VISUALIZZO IL MESSAG
051600950407     C     WBOL          IFEQ      ' '
051700950407     C                   GOTO      FOR01
051800950407     C                   ELSE
051900050407     C                   MOVEL     'V'           VI5SCE
052000950206     C                   EXSR      CONF
052100950206     C                   ENDIF
052200950407     C                   ENDIF
052300911031     C*
052400900828     C                   GOTO      INIZIO
052500900828     C                   END
052600911031     C*
052700911031     C                   SETON                                        05
052800950419     C*
052900950419     C* SE IMPOSTATO LO TENGO
053000950419     C     WSANRR        IFGT      0
053100951002     C     WSANRR        IFLE      SAVNRR
053200950419     C                   Z-ADD     WSANRR        REC
053300951002     C                   ELSE
053400951002     C                   Z-ADD     SAVNRR        REC
053500951002     C                   ENDIF
053600951002     C                   CLEAR                   WSANRR
053700950419     C                   ENDIF
053800900828     C*
053900900515     C** CARICAMENTO SUBFILE
054000900518     C     FORCT2        TAG
054100020917      *
054200020917      * Prima di caricare il sfl richiamo il Pgm per pulizia bolle obsolete
054300020917      * solo se la prima chiamata allo stesso pgm è andata a buon fine
054400020918     C                   IF        OE5ERR <> 'E'
054500020917     C                   CLEAR                   FNLSE5DS
054600020918     C                   Z-ADD     VI4FGS        IE5POU
054700020917     C                   CALL      'FNLSE5R'
054800020917     C                   PARM                    FNLSE5DS
054900020917     C                   ENDIF
055000020917      *
055100000518     C                   CLEAR                   TOTA
055200001128     C* SE CODICE CLIENTE A ZERO OPPURE NON È STATA INSERITA LA ZONA
055300001128     C* DISABILITO IL CMD 6 E CMD 16
055400001128     C                   SETOFF                                       16
055500001128     C*
055600001128     C     VI4KSC        IFEQ      *ZEROS
055700001128     C     VI4NRS        IFEQ      99
055800111221     C     VI4NRS        oreq      00
055900001128     C                   SETON                                        16
056000001128     C                   ENDIF
056100001128     C                   ENDIF
056200001128     C*
056300941007     C                   WRITE     LS66D02
056400900515     C** COMANDI SUBFILE
056500900515     C     FORCT         TAG
056600941007     C                   EXFMT     LS66DCT
056700950112     C* F3 - FINE
056800900515     C   KC              GOTO      FINE
056900950112     C* F12 - RITORNO
057000900517     C   KL              GOTO      INIZIO
057100950112     C* F16 - CONFERMA ANNULLAMENTO
057200910225     C   KQ              EXSR      ANNULL
057300910225     C   KQ              GOTO      ELATAG
057400900808     C*
057500950112     C* F6 - SELEZIONE TOTALE
057600950112     C   KF              DO
057700941010     C                   EXSR      CTRD02
057800930909     C   90              GOTO      FORCT
057900930909     C*
058000900827     C     SUB           TAG
058100941007     C                   READC     LS66DSF                                34
058200900827     C  N34VI5SCE        IFNE      ' '
058300900827     C                   SETON                                        98
058400900827     C                   GOTO      FORCT
058500900827     C                   ELSE
058600900827     C                   GOTO      SUB
058700900827     C                   END
058800900808     C*
058900900808     C                   EXSR      SELEZ
059000911031     C* ERRORI
059100911031     C   12              GOTO      FORCT2
059200911031     C*
059300900808     C* SE FINITO TUTTO OK VADO A VIDEATA 1
059400900808     C                   GOTO      INIZIO
059500900808     C                   END
059600900824     C*
059700900516     C** CONTROLLO SE EFFETTUATA UNA SCELTA
059800941007     C                   READC     LS66DSF                                34
059900911031     C   34              Z-ADD     PRIMA         REC
060000950419     C                   SETOFF                                       90
060100911031     C*
060200930909    1C  N34VI5SCE        IFNE      *BLANKS
060300941010     C                   EXSR      CTRD02
060400930909     C                   ENDIF
060500930909     C   90
060600911031     COR 34              GOTO      FORCT
060700911031     C*
060800930909     C* VISUALIZZO BOLLA PER BOLLA
060900911031     C     *IN34         DOWEQ     '0'
061000950419     C*
061100950419     C* SALVO IL NUMERO DI RECORD CHE HO
061200950419     C                   Z-ADD     PRIMA         WSANRR
061300911031     C*
061400910222     C     VI5SCE        IFEQ      'X'
061500910222     C     VI5SCE        OREQ      'V'
061600900808     C                   EXSR      CONF
061700900808     C                   END
061800950419     C*
061900000517     C* SE E' STATO DATO KC NEL PGM FNLS01R VADO FUORI PROGRAMMA
062000000517     C     §LPFPR        IFNE      '3'
062100941007     C                   READC     LS66DSF                                34
062200000517     C                   ELSE
062300000517     C                   SETON                                            34
062400000517     C                   END
062500000517     C*
062600911031     C                   END
062700950419     C*
062800911031     C* ERRORI EVIDENZIO QUELLE ERRATE
062900911031     C   12              GOTO      FORCT2
063000911031     C*
063100911031     C* FINE LETTURA RIEMETTO SUBFILE AGGIORNATO
063200930909     C   03              GOTO      FORCT2
063300911122     C                   GOTO      ELATAG
063400000000     C     FINE          TAG
063500930628     C*
063600930628     C     RICH          IFEQ      '1'
063700930628     C                   MOVEL     'C'           §LPFPR
063800991026     C                   CLEAR                   DTASV
063900020910     C* P.O DI GESTIONE
064000020917     C                   MOVE      VI4FGS        §LPFGS
064100020910      *
064200941011     C                   CALL      'FNLS01R'
064300930628     C                   PARM                    KPJBA
064400991026     C                   PARM                    DBLP
064500991026     C                   PARM                    DTASV
064600030718     C                   PARM                    trul90ds
064700930628     C                   END
064800000626     C*
064900060221     C***  RICLSA        IFEQ      'S'
065000000629     C*
065100060221     C***                CLEAR                   KPJBU
065200060221     C***                MOVEL     '99999'       PA3FGV
065300000626     C*
065400060221     C****               MOVEL     SIMFEL        WORG
065500000626     C*
065600060221     C****               EXSR      CTRORG
065700060221     c***                eval      og150=orgdf0
065800000626     C*
065900060221     C*                  TESTN                   §ogvol               30
066000000626     C*
066100060221     C*    *IN30         IFEQ      *ON
066200060221     C*    §ogvol        ANDGT     *ZEROS
066300060221     C*                  MOVEL     'S'           PA3CML
066400060221     C*                  ENDIF
066500040121     c*
066600060221     c*                  eval      pa3spv=§ogspv
066700060221     c*                  eval      pa3spp=§ogspp
066800000626     C*
066900060221     C*                  MOVEL     ORGDE8        OG148
067000000626     C*
067100060221     C*    §OGAGS        IFEQ      'E'
067200060221     C*                  MOVEL     'S'           PA3AGS
067300060221     C*                  MOVEL     §OGAGP        PA3AGP
067400060221     C*                  ELSE
067500060221     C*                  MOVEL     ' '           PA3AGS
067600060221     C*                  CLEAR                   PA3AGP
067700060221     C*                  ENDIF
067800000626     C*
067900060221     C*                  MOVEL     SIMFEL        PA3FEL
068000060221     C*                  MOVEL     VIDDSA        PA3SUT
068100000626     C*
068200060221     C*                  Z-ADD     *ZEROS        PA3NFV
068300060221     C*                  Z-ADD     *ZEROS        PA3DFV
068400000626     C*
068500060221     C*                  MOVEL     'KSDIFFE'     KQEBI
068600060221     C*                  MOVE      'PT '         KQEBI
068700060221     C*                  MOVEL     'KSDIFFE'     KQIEB
068800060221     C*                  MOVE      'PT '         KQIEB
068900060221     C*                  MOVEL     PARAM3        KPJBU
069000060221     C*                  MOVEL     'LSA2'        KCOAZ
069100060221     C*                  CALL      'BCH10'
069200060221     C*                  PARM                    KPJBA
069300000824     C* INVIO FILE TRASMISSIONE
069400060221     C*                  CLEAR                   KPJBU
069500060221     C*                  MOVEL     'LSA3'        KCOAZ
069600060221     C*                  CALL      'BCH10'
069700060221     C*                  PARM                    KPJBA
069800000626     C*
069900060221     C*                  ENDIF
070000020909      *
070100020909      * Prima di fine richiamo il Pgm per pulizia bolle obsolete
070200020909      * il modalità 'C'
070300020918     C                   IF        OE5ERR <> 'E'
070400020909     C                   CLEAR                   FNLSE5DS
070500020909     C                   MOVE      'C'           IE5TLA
070600020909     C                   CALL      'FNLSE5R'
070700020909     C                   PARM                    FNLSE5DS
070800020909     C                   ENDIF
070900060926     C*
071000060926     C                   CLEAR                   DSLV55
071100060926     C                   MOVEL     'C'           D55TLA
071200060926     C                   CALL      'FNLV55R'
071300060926     C                   PARM                    DSLV55
071400000000     C                   SETON                                        LR
071500940201     C*
071600941007     C* SUBROUTINE INIZIALE ------------------------------------------*
071700941007     C     *INZSR        BEGSR
071800950127     C*
071900950127     C     *ENTRY        PLIST
072000950127     C                   PARM                    KPJBA
072100950127     C**
072200950127     C* KLIST
072300950127     C**
072400941007     C     KTAB          KLIST
072500941007     C                   KFLD                    CODUT
072600941007     C                   KFLD                    COD               2
072700941007     C                   KFLD                    KEY               8
072800950407     C     KTAB2         KLIST
072900950407     C                   KFLD                    CODUT
073000950407     C                   KFLD                    COD               2
073100960103     C* ACCESSO   AZCLN
073200960103     C     KCLN          KLIST
073300960103     C                   KFLD                    SIMFEL
073400960103     C                   KFLD                    W0030
073500960103     C                   KFLD                    WAA               4 0
073600960103     C                   KFLD                    WMM               2 0
073700941007     C     KBLP          KLIST
073800960103     C                   KFLD                    AAS
073900941007     C                   KFLD                    VI5LNP
074000941007     C                   KFLD                    VI5NRS
074100941007     C                   KFLD                    VI5NSP
074200941007     C     KACO          KLIST
074300941007     C                   KFLD                    CODUT
074400020910     C                   KFLD                    DUTKCI
074500941007     C                   KFLD                    KSC               7 0
074600941010     C     KVAB          KLIST
074700941007     C                   KFLD                    VI4KSC
074800941007     C                   KFLD                    VI4RMN
074900080623     C***  KVAB2         KLIST
075000080623     C***                KFLD                    VI5FGS
075100080623     C***                KFLD                    VI5KSC
075200080623     C***                KFLD                    VI5RMN
075300941007     C     KBLD          KLIST
075400020910     C                   KFLD                    VABFGS
075500020910     C                   KFLD                    VABCCM
075600941010     C                   KFLD                    VABAAS
075700941010     C                   KFLD                    VABLNP
075800941010     C                   KFLD                    VABNRS
075900941010     C                   KFLD                    VABNSP
076000941007     C     KBLD2         KLIST
076100020917     C                   KFLD                    VI5FGS
076200941007     C                   KFLD                    VI5KSC
076300941007     C                   KFLD                    VI5AAS
076400941007     C                   KFLD                    VI5LNP
076500941007     C                   KFLD                    VI5NRS
076600941007     C                   KFLD                    VI5NSP
076700130313     C     Kvat2         KLIST
076800130313     C                   KFLD                    VI5FGS
076900130313     C                   KFLD                    VI5KSC
077000130313     C                   KFLD                    VI5AAS
077100130313     C                   KFLD                    VI5LNP
077200130313     C                   KFLD                    VI5NRS
077300130313     C                   KFLD                    VI5NSP
077400130313     C                   KFLD                    WTRC
077500950127     C     KFTT          KLIST
077600950127     C                   KFLD                    W01PDR
077700950127     C                   KFLD                    W01FVL
077800950127     C                   KFLD                    COMDRT
077900950127     C                   KFLD                    W01TSR
078000060221     C***  KBL4          KLIST
078100060221     C***                KFLD                    VABAAS
078200060221     C***                KFLD                    VABLNP
078300060221     C***                KFLD                    VABNRS
078400060221     C***                KFLD                    VABNSP
078500060221     C***                KFLD                    WTRC
078600060221     C***  KBLPPT        KLIST
078700060221     C***                KFLD                    BLTAAS
078800060221     C***                KFLD                    BLTLNP
078900060221     C***                KFLD                    BLTNRS
079000060221     C***                KFLD                    BLTNSP
079100060221     C***  KBL4PT        KLIST
079200060221     C***                KFLD                    BLTAAS
079300060221     C***                KFLD                    BLTLNP
079400060221     C***                KFLD                    BLTNRS
079500060221     C***                KFLD                    BLTNSP
079600060221     C***                KFLD                    WPTTRC
079700951010     C     KBLP2         KLIST
079800951010     C                   KFLD                    VABAAS
079900951010     C                   KFLD                    VABLNP
080000951010     C                   KFLD                    VABNRS
080100951010     C                   KFLD                    VABNSP
080200000523     C     KBLT          KLIST
080300000523     C                   KFLD                    VABLNP
080400000523     C                   KFLD                    VABNRS
080500000523     C                   KFLD                    VABNCD
080600001214     C*
080700010219     C     KORM          KLIST
080800010219     C                   KFLD                    VI4POE
080900010219     C                   KFLD                    VI4NSR
081000010223     C                   KFLD                    VI4NO2
081100010219     C                   KFLD                    VI4NRV
081200061127     C     KORM_dpo      KLIST
081300061127     C                   KFLD                    dpopoe
081400061127     C                   KFLD                    dponrs
081500061127     C                   KFLD                    dponor
081600061127     C                   KFLD                    dponrv
081700010219     C     KDST          KLIST
081800120607     C                   KFLD                    ormnpg
081900120607     C                   KFLD                    ormndc
082000010219     C                   KFLD                    ORMPOR
082100010220     C     KFVV          KLIST
082200010220     C                   KFLD                    ORMNPG
082300010220     C                   KFLD                    W0050
082400010220     C                   KFLD                    ORMPOR
082500020219      *
082600020219     C     K01TBE        KLIST
082700020219     C                   KFLD                    TBECOD
082800020219     C                   KFLD                    TBEKE1
082900020219      *
083000020219     C     K02TBE        KLIST
083100020219     C                   KFLD                    $COD
083200020219     C                   KFLD                    $LIN
083300020219     C                   KFLD                    $SIF
083400020219     C                   KFLD                    TBEKE1
083500020219      *
083600020219     C     K03TBE        KLIST
083700020219     C                   KFLD                    $COD
083800020219     C                   KFLD                    $LIN
083900020219     C                   KFLD                    $SIF
084000020219     C                   KFLD                    TBEKE1
084100020219     C                   KFLD                    TBEKE2
084200021203     C     Kfapd         klist
084300021203     C                   kfld                    kpdtip
084400021203     C                   kfld                    kpdpdr
084500061122     C     KVAT          KLIST
084600061124     C                   KFLD                    vabfgs
084700061122     C                   KFLD                    §LPCCM
084800061122     C                   KFLD                    §LPAAC
084900061122     C                   KFLD                    §LPLNP
085000061122     C                   KFLD                    §LPNRS
085100061122     C                   KFLD                    §LPNSP
085200061122     C                   KFLD                    WTRC
085300941007     C*
085400060221     C***  *LIKE         DEFINE    BL4TRC        WTRC
085500060221     C**** *LIKE         DEFINE    BL4TRC        WPTTRC
085600061122     C     *LIKE         DEFINE    vattrc        WTRC
085700951010     C     *LIKE         DEFINE    TBLKUT        §KUT
085800941010     C     *LIKE         DEFINE    TBLCOD        §COD
085900941007     C     *LIKE         DEFINE    TBLKEY        §KEY
086000941007     C     *LIKE         DEFINE    VI4KSC        SAVKSC
086100990107     C     *LIKE         DEFINE    VI4NRS        SAVNRS
086200950407     C     *LIKE         DEFINE    VI4KSC        WKSC
086300941007     C     *LIKE         DEFINE    VI5CTM        SAVCTM
086400990107     C     *LIKE         DEFINE    VI5CTM        SA2CTM
086500941007     C     *LIKE         DEFINE    BLPAAS        COMAAS
086600950127     C     *LIKE         DEFINE    FTTTSR        W01TSR
086700950127     C     *LIKE         DEFINE    FTTPDR        W01PDR
086800950127     C     *LIKE         DEFINE    FTTFVL        W01FVL
086900950419     C     *LIKE         DEFINE    REC           WSANRR
087000001006     C     *LIKE         DEFINE    §1BCTC        WWCTC
087100010219     C     *LIKE         DEFINE    VIDORM        SAVORM
087200010219     C     *LIKE         DEFINE    VI4PDR        SA1PDR
087300010219     C     *LIKE         DEFINE    VI4FPP        SA1FPP
087400010219     C     *LIKE         DEFINE    VI4DRT        SA1DRT
087500010219     C     *LIKE         DEFINE    VI4KSC        SA1KSC
087600010219     C     *LIKE         DEFINE    VI4PDR        WDPDR
087700950127     C*
087800950127     C                   MOVEL     'R'           W01TSR
087900950127     C                   MOVEL     'C'           W01FVL
088000950127     C                   Z-ADD     1             CODUT
088100950407     C                   CLEAR                   WKSC
088200060221     C******             MOVEL     'E'           WTRC
088300060221     C******             MOVEL     PT            WRKPT             1
088400060221     C                   clear                   WRKPT             1
088500020910     C                   Z-ADD     1             CODUT             1 0
088600020910      *
088700020910     c     *dtaara       define    §azute        azuteds
088800020910     c     *dtaara       define    §datiute      ddatiute
088900020910     c                   in(E)     *dtaara
089000020910     c                   IF        %error  or RSUT = *blanks
089100020910     c                   CLEAR                   tibs34ds
089200020910     c                   CALL      'TIBS34R'
089300020910     c                   PARM                    tibs34ds
089400020910     c                   in        *dtaara
089500020910     c                   endif
089600020910     C                   MOVEL     RSUT          VIDDSA
089700060221     c*
089800060221     C                   MOVEL     dutpou        VI4FGS
089900020910      *
090000001006     C* MI MEMORIZZO L'AS PER NUMERATORE CHI SONO DPD
090100001006     C                   Z-ADD     SIMFEL        W0030
090200001006     C     W0030         CHAIN     AZORG01L                           31
090300001006     C                   MOVEL     ORGDF0        OG150
090400001006     C                   MOVEL     §OGDKC        WASDKC            1
090500941007     C***
090600941007     C* CARICO TABELLA FILIALI GESTITE £1
090700941007     C***
090800941007     C                   CLEAR                   DSUL06
090900021007     C                   MOVE      'PP'          D06COD
091000000522     C     VI4FGS        IFEQ      *ZEROS
091100941007     C                   MOVEL     SIMFEL        D06KEY
091200000522     C                   ELSE
091300000522     C                   MOVEL     VI4FGS        D06KEY
091400000522     C                   ENDIF
091500941007     C                   MOVEL     DSUL06        KPJBU
091600941007     C*
091700941007     C                   CALL      'TRUL06R'
091800941007     C                   PARM                    KPJBA
091900941007     C                   MOVEL     KPJBU         DSUL06
092000941007     C                   MOVEA     LIN           L1
092100020916     C***
092200020926     C* COSTRUISCO SCHIERA CON P.O GESTITI
092300020926     C* SE P.O. GESTIONE POSTE USO LA SUA £6
092400021007     C* SE NON POSTE TOLGO DALLA £1 I P.O. POSTE
092500020916     C***
092600020926     C     WRKPT         IFNE      'S'
092700020916     C                   z-add     1             Y                 2 0
092800020916     C     Y             DOWLT     30
092900020916     C     L1(Y)         ANDNE     *zeros
093000020916     C                   MOVE      L1(Y)         WORG
093100020916     C*
093200020916     C                   EXSR      CTRORG
093300020918     C     §OGNTW        IFNE      'PPT'
093400020926     C                   add       1             XX                3 0
093500020926     C                   z-add     L1(Y)         POG(xx)
093600020916     c                   ENDIF
093700020918     C                   add       1             Y
093800020916     C                   ENDDO
093900020923      *
094000020923     C                   ELSE
094100020916     C                   CLEAR                   DSUL06
094200020916     C                   MOVE      '£6'          D06COD
094300020916     C     VI4FGS        IFEQ      *ZEROS
094400020916     C                   MOVEL     SIMFEL        D06KEY
094500020916     C                   ELSE
094600020916     C                   MOVEL     VI4FGS        D06KEY
094700020916     C                   ENDIF
094800020916     C                   MOVEL     DSUL06        KPJBU
094900020916     C*
095000020916     C                   CALL      'TRUL06R'
095100020916     C                   PARM                    KPJBA
095200020916     C                   MOVEL     KPJBU         DSUL06
095300020926     C                   MOVEA     LIN           POG
095400020916     C                   ENDIF
095500960103     C***
095600960103     C* CARICO DATI VARIABILI PARTENZE
095700960103     C***
095800960103     C                   MOVEL     '3I'          COD
095900960103     C                   MOVEL     '1       '    KEY
096000960103     C     KTAB          CHAIN     TABEL                              30
096100960103     C  N30              MOVEL     TBLUNI        DS3I
096200960103     C   30              CLEAR                   DS3I
096300941007     C*
096400960103     C* DATA BOLLETTAZIONE:  TRA UDATE E 1 GIORNO LAVORATIVO MAGGIORE
096500960103     C*  SOLO PER LE SPEDIZ.DI RECUPERO E' TRA DATA G. - 3  E  + 1 LAV
096600941007     C                   TIME                    W0140            14 0
096700941007     C* UDATE IN GGMMAAAA
096800941007     C                   MOVE      W0140         WDTGIO            8 0
096900941007     C*
097000941007     C* UDATE IN AAAAMMGG
097100941007     C                   Z-ADD     WDTGIO        G02DAT
097200941007     C                   MOVEL     *BLANK        G02ERR
097300941007     C                   CALL      'XSRDA8'
097400941007     C                   PARM                    WLBDAT
097500941007     C                   MOVEL     G02INV        DATEU             8 0
097600960103     C* SE NON FOSSE PERMESSO BOLLETTARE IN SOSPENSIONE
097700960103     C*  SI PUO' BOLLETTARE SOLO FINO A OGGI
097800960103     C                   Z-ADD     G02INV        DATAPA            8 0
097900960103     C                   MOVEL     G02DAT        DATAPG            8
098000950127     C* SOTTRAGGO  3
098100950127     C     G02TGI        SUB       3             GIOTGI
098200950127     C*
098300950127     C                   CALL      'XSRGI8'
098400950127     C                   PARM                    WGIDAT
098500950127     C*
098600950127     C                   Z-ADD     GIOINV        DATAM             8 0
098700080110     C* SOTTRAGGO  giorni per ottenere data minima per data ritiro
098800080110     c                   clear                   TRUL52ds
098900080110     c                   eval      d52gio=§3iggb
099000080110     c                   eval      d52sgn='-'
099100080110     c                   z-add     dateu         D52din
099200080110     c                   call      'TRUL52R'
099300080110     c                   parm                    TRUL52ds
099400080110     C*
099500080110     C                   Z-ADD     d52dfi        DATAmin           8 0
099600960103     C*
099700960103     C* ADDIZIONO 1 FINTANTO CHE NON TROVO IL PRIMO GIORNO LAVORATIVO
099800020917     C                   Z-ADD     1             X                 2 0
099900960103    1C     X             DOWLE     §3IGGS
100000960103     C*
100100960103     C                   MOVEL     'F'           WFEST             1
100200960103     C*
100300960103    2C     WFEST         DOWNE     ' '
100400960103     C     G02TGI        ADD       1             GIOTGI
100500960103     C                   ADD       1             G02TGI
100600960103     C                   CALL      'XSRGI8'
100700960103     C                   PARM                    WGIDAT
100800960103     C*
100900960103     C                   Z-ADD     GIOINV        DATAPA            8 0
101000960103     C                   MOVEL     GIODAT        DATAPG            8
101100960103     C*
101200960103     C* IMPOSTO ANNO E MESE DELLA DATA TROVATA
101300960103     C                   Z-ADD     GIOINV        WDATA
101400960103     C                   CLEAR                   W0030
101500960103     C* CHAIN SUL CALENDARIO BARTOLINI
101600960103     C     KCLN          CHAIN     AZCLN01L                           30
101700960103     C   30              CLEAR                   CLNPOM
101800960103     C                   MOVEA     CLNPOM        K31
101900960103     C                   MOVEL     K31(WGG)      WFEST
102000960103    2C                   ENDDO
102100960103     C*
102200960103     C                   ADD       1             X
102300960103    2C                   ENDDO
102400020430     C* TESTO SE SONO primo o secondo livello
102500020430     C*                  MOVEL     SIMFEL        VI4LPD
102600020917     c                   z-add     dutpou        vi4lpd
102700000522     C* ABILITO F18
102800060221     C****               SETON                                        09
102900060221     c**** DutLpo        ifeq      '2'
103000060221     c**** DutLpo        oreq      *blanks
103100000522     C* DISABILITO F18
103200060221     C****               SETOFF                                       09
103300000522     C*
103400060221     C*****              END
103500000523     C* SE 09 È SPENTO LO ACCENDO SE SONO UN PO POSTE PER ABILITARE F18
103600060221     C***  *IN09         IFEQ      *OFF
103700060221     C***  WRKPT         ANDEQ     'S'
103800060221     C****               SETON                                        09
103900060221     C****               ENDIF
104000000522     C* SE PO IN GESTIONE È VALORIZZATO SARA' LUI CHE COMANDERA'
104100000522     C     VI4FGS        IFNE      *ZEROS
104200000522     C                   MOVEL     VI4FGS        VI4LPD
104300000523     C                   ELSE
104400000523     C                   MOVEL     VI4LPD        VI4FGS
104500000522     C                   ENDIF
104600000522     C* DECODIFICO
104700000523     C     VI4FGS        CHAIN     AZORG01L                           37
104800000522     C     *IN37         IFEQ      *OFF
104900000522     C     ORGFVA        ANDEQ     ' '
105000000522     C                   MOVEL     ORGDES        DESFGS
105100000522     C                   ENDIF
105200000512     C*
105300940201     C* PULIZIA CAMPI CHE RIMANGONO INVARIATI IL SECONDO GIRO
105400940201     C                   Z-ADD     0             VI4KSC
105500071207     c                   Clear                   Vi4Tcr
105600071207     c                   Clear                   Vi4Dcr
105700071207     c                   Clear                   Vi4Hcr
105800071207     c                   Clear                   Vi4Ore
105900970513     C                   Z-ADD     99            VI4NRS
106000041122     C                   Z-ADD     999           VI4lna
106100941007     C                   MOVEL     *BLANKS       VI4PDR
106200941007     C                   MOVEL     *BLANKS       DESPDR
106300941007     C                   Z-ADD     WDTGIO        VI4DRT
106400941007     C                   MOVEL     'P'           VI4FPP
106500941007     C                   MOVEL     0             VI4NRT
106600941007     C                   Z-ADD     WDTGIO        VI4DSP
106700941007     C                   MOVE      WDTGIO        COMAAS
106800940201     C                   Z-ADD     0             PREKSC
106900080623     C                   clear                   PRERMN
107000940201     C                   MOVEL     '  '          VI4CTM
107100120709     C                   MOVEL     '  '          s_VI4CTM
107200950407     C                   MOVEL     'X'           VI4SCE
107300010219     C                   CLEAR                   VI4POE
107400010219     C                   CLEAR                   VI4NOR
107500010219     C                   CLEAR                   VI4NRV
107600041222     c                   Clear                   vi4ctr
107700080623     c                   movel     'N'           vi4ord
107800010219     C*
107900010219     C* IMPOSTO NUMERO ORM FITTIZIO
108000010227     C                   MOVE      *ALL'9'       ORMFIT           11 0
108100940201     C*
108200000518     C* SE WRKPT = 'S' VENGO DA CONFERMA BOLLE POSTE
108300000518     C     WRKPT         IFEQ      'S'
108400000518     C                   MOVE      TES(3)        VIDTES
108500000518     C                   ELSE
108600940201     C                   MOVE      TES(1)        VIDTES
108700940201     C                   END
108800940201     C*
108900080623     C******             Z-ADD     -1            VI4NSP
109000950407     C***
109100950407     C* CARICO TUTTI I CODIDI TRATTAMENTO MERCE CHE SI ASSEGNANO
109200950407     C*  IL NUMERO DI SPEDIZIONE
109300950407     C**
109400950407     C                   MOVEL     '1B'          COD
109500950407     C                   Z-ADD     1             X
109600950407     C*
109700950407     C     KTAB2         SETLL     TABEL
109800950407     C     KTAB2         READE     TABEL                                  30
109900950407     C*
110000950407     C     *IN30         DOWEQ     *OFF
110100950407     C                   MOVEL     TBLUNI        DS1B
110200950407     C     §1BFG2        IFEQ      'S'
110300950407     C                   MOVEL     TBLKEY        CTM(X)
110400120710     C                   eval      ctmf(X)=%subst(ds1b:26:13)
110500120710     C                   eval      ctm16(X)=§1bf16
110600950407     C                   ADD       1             X
110700950407     C                   ENDIF
110800950407     C*
110900950407     C     KTAB2         READE     TABEL                                  30
111000950407     C                   ENDDO
111100940201     C*
111200950503     C                   CLEAR                   DS1B
111300950503     C                   CLEAR                   SAVCTM
111400020219     C*
111500020219     C* RECUPERO SISTEMA INFORMATIVO DELLA TABELLA 3NN
111600020219     C                   MOVE      *BLANKS       TBECOD
111700020219     C                   MOVE      *ZEROS        TBEKE1
111800020219     C                   MOVE      '3NN'         $COD              3
111900020219     C                   MOVE      $COD          TBEKE1
112000020219     C     K01TBE        CHAIN     TNTBE01L                           31
112100020219     C                   MOVEL     TBESIF        $SIF             10
112200020219     C                   MOVEL     TBELIN        $LIN              1
112300020219     C                   MOVEL     TBEAPL        $APL              2
112400020219     C*
112500020909     C* Richiamo Pgm per pulizia bolle obsolete, passo il P.O di gestione
112600020909     C                   CLEAR                   FNLSE5DS
112700020918     C                   Z-ADD     VI4FGS        IE5POU
112800020909     C                   CALL      'FNLSE5R'
112900020909     C                   PARM                    FNLSE5DS
113000070803      * Aggancio tabella "VPODECO" per reperimento data attivazione
113100070803      * data ritiro da orm
113200070803     C                   CLEAR                   dsbs02
113300070803     c                   clear                   dvpodeco
113400070803     C                   MOVEL     'C'           T02MOD
113500070803     C                   MOVEL     KNSIF         T02SIF
113600070803     C                   MOVEL     'VPO'         T02COD
113700070803     c                   movel(P)  'DECO'        t02ke1
113800070803     C                   CALL      'TIBS02R'
113900070803     C                   PARM                    KPJBA
114000070803     C                   PARM                    dsbs02
114100070803    2C                   IF        T02ERR = *BLANKS
114200070803     C                   MOVEL     T02uni        dvpodeco
114300070803    2C                   ENDIF
114400041202     C* reperisco da tabella vpo il numero giorni da sommare a udate per
114500041202     c* determinare data max per data consegna richiesta
114600041202     C                   CLEAR                   DSBS02
114700041202     C                   MOVEL     'C'           T02MOD
114800041202     C                   MOVEL     KNSIF         T02SIF
114900041202     C                   MOVEL     'VPO'         T02COD
115000041202     C                   MOVEL(p)  'VPO'         T02KE1
115100041202     C                   CALL      'TIBS02R'
115200041202     C                   PARM                    KPJBA
115300041202     C                   PARM                    DSBS02
115400041202    4C     T02ERR        IFEQ      *BLANKS
115500041202     C                   MOVEL     T02UNI        DVPO
115600041202     C                   ELSE
115700041202     C                   CLEAR                   DVPO
115800041202    4C                   ENDIF
115900030415      * Calcolo oggi + 5 gg.
116000051215     c**                 Move      Dateu         Dataiso
116100051215     c**                 Adddur    §vpodcg:*d    Dataiso
116200051215     c***                Move      Dataiso       woggi5
116300051215     c*
116400051215     c                   Move      Dateu         Dataiso
116500051215     c                   Adddur    §vpodcgm1:*d  Dataiso
116600051215     c                   Move      Dataiso       woggidcgm1
116700051215     c*
116800051215     c                   Move      Dateu         Dataiso
116900051215     c                   Adddur    §vpodcgm2:*d  Dataiso
117000051215     c                   Move      Dataiso       woggidcgm2
117100110419      * Aggancio tabella "VPOLS01" per reperimento filiali che utilizzano
117200110419      * nuova ricerca O.R.M.
117300110419     C                   CLEAR                   DSBS02
117400110419     C                   MOVEL     'C'           T02MOD
117500110419     C                   MOVEL     KNSIF         T02SIF
117600110419     C                   MOVEL     'VPO'         T02COD
117700110419     C                   MOVEL(p)  'LS01'        T02KE1
117800110419     C                   CALL      'TIBS02R'
117900110419     C                   PARM                    KPJBA
118000110419     C                   PARM                    DSBS02
118100110419     C     T02ERR        IFEQ      *BLANKS
118200110419     C                   MOVEL     T02UNI        DVPOls01
118300110419     C                   ELSE
118400110419     C                   CLEAR                   DVPOls01
118500110419     C                   ENDIF
118600030409
118700940201     C                   ENDSR
118800900509     C*
118900900509     C*--- CONTROLLI FORMATO1 ----------------------------------------*
119000941010     C     CTRD01        BEGSR
119100900807     C                   SETOFF                                       900203
119200000717     C                   SETOFF                                       051208
119300030411     C                   SETOFF                                       588586
119400030415     C                   SETOFF                                       878889
119500080110     c                   setoff                                       6752
119600930909     C                   CLEAR                   COMDSP
119700930909     C                   CLEAR                   COMDRT
119800941007     C                   CLEAR                   DESPDR
119900000718     C                   CLEAR                   W0070
120000120903     C***                CLEAR                   CLI
120100120903     C***                CLEAR                   C
120200120903     C***                CLEAR                   Z
120300060306     c                   clear                   dcli
120400120710     c                   clear                   wctrctm
120500000512     C* SE E' STATO PREMUTO F6 O F16 FACCIO DEI CONTROLLI AGGIUNTIVI
120600000512     C* OBBLIGATORIETA' DEL CODICE CLIENTE SEMPRE
120700000512     C     *INKF         IFEQ      *ON
120800000512     C     *INKQ         OREQ      *ON
120900000705     C* SE IL P.O. GESTIONE NON E' POSTE OBBLIGATORIO IL CLIENTE
121000000512     C     VI4KSC        IFEQ      *ZEROS
121100000705     C     WRKPT         ANDNE     'S'
121200000512     C                   SETON                                        6990
121300000512     C                   GOTO      ENDCTR
121400000512     C                   ENDIF
121500000705     C* SE INVECE SONO UN P.O. POSTE POSSO ESEGUIRE ANCHE CON LA SOLA SERIE
121600120903     C*    WRKPT         IFEQ      'S'
121700120903     C*                  SELECT
121800000705     C*
121900120903     C*    VI4KSC        WHENGT    *ZEROS
122000000705     C*
122100120903     C*    VI4NRS        WHENNE    99
122200120903     C*    VI4NRS        ANDNE     *ZEROS
122300000705     C* CONTROLLO I CLIENTI CHE SONO LEGATI A QUELLA SERIE CON P.O. UGUALE
122400000705     C* AL P.O. PADRONCINO
122500120903     C*                  SETON                                        08
122600120903     C*                  EXSR      CTRNRS
122700120903     C*  90              GOTO      ENDCTR
122800000705     C* ALTRIMENTI  ERRORE
122900120903     C*                  OTHER
123000000705     C* ERRORE
123100120903     C*                  SETON                                        7590
123200120903     C*                  GOTO      ENDCTR
123300000705     C*
123400120903     C**                 ENDSL
123500000718     C*
123600120903     C**                 ENDIF
123700000512     C*
123800000512     C                   ENDIF
123900000512     C* OBBLIGATORIETA' DEL CODICE PADRONCINO SOLO IN CASO DI CONFERMA BOLLE
124000000512     C*
124100000512     C     *INKF         IFEQ      *ON
124200000512     C*
124300000512     C     VI4PDR        IFEQ      *ZEROS
124400000512     C     VI4PDR        OREQ      *BLANKS
124500000515     C     VI4DRT        OREQ      *ZEROS
124600000512     C                   SETON                                        9068
124700000512     C                   GOTO      ENDCTR
124800000512     C                   ENDIF
124900010219     C* SE IN CONFERMA E P.O. PADRONCINO ATTIVATO A PROCEDURA O.R.M.
125000010219     C* È OBBLIGATORIO IL N.O.R.M.
125100010219     C                   EXSR      ORMOBB
125200010219     C     VIDORM        IFEQ      *ZEROS
125300010219     C     WOBBL         ANDEQ     'S'
125400010220     C                   SETON                                        905828
125500010219     C                   MOVEL     ERR(6)        VIDMSG
125600010219     C                   GOTO      ENDCTR
125700010219     C                   ENDIF
125800000512     C                   ENDIF
125900921005     C*
126000921005     C* D A T A   S P E D I Z I O N E
126100941007     C                   Z-ADD     VI4DSP        G02DAT
126200941007     C                   MOVEL     *BLANK        G02ERR
126300941007     C                   CALL      'XSRDA8'
126400941007     C                   PARM                    WLBDAT
126500941007     C* ERRATA
126600941007    1C     G02ERR        IFEQ      '1'
126700960103     C                   SETON                                        532890
126800960103     C* PREPARO L'ERRORE
126900960103     C                   MOVEA     ERR(1)        K78
127000960103     C     2             SUBST     DATAPG:1      W002A
127100960103     C                   MOVEA     W002A         K78(69)
127200960103     C                   MOVEL     '/'           K78(71)
127300960103     C     2             SUBST     DATAPG:3      W002A
127400960103     C                   MOVEA     W002A         K78(72)
127500960103     C                   MOVEL     '/'           K78(74)
127600960103     C     2             SUBST     DATAPG:7      W002A
127700960103     C                   MOVEA     W002A         K78(75)
127800960103     C                   MOVEA     K78           VIDMSG
127900911031     C                   GOTO      ENDCTR
128000941007    1C                   ENDIF
128100960103     C                   Z-ADD     G02DAT        VI4DSP
128200941007     C                   Z-ADD     G02INV        COMDSP            8 0
128300960103     C* DATE SPEDIZIONE NO > DATA MAX DI SOSPENSIONE
128400960103     C* ANNO SPEDIZIONE NO < ANNO IN CORSO
128500960103     C                   MOVEL     COMDSP        AAS               4 0
128600960103    1C     AAS           IFLT      COMAAS
128700960103     C     COMDSP        ORGT      DATAPA
128800960103     C     COMDSP        ORLT      DATAM
128900960103     C                   SETON                                        539028
129000960103     C                   MOVEA     ERR(1)        K78
129100960103     C     2             SUBST     DATAPG:1      W002A             2
129200960103     C                   MOVEA     W002A         K78(69)
129300960103     C                   MOVEL     '/'           K78(71)
129400960103     C     2             SUBST     DATAPG:3      W002A
129500960103     C                   MOVEA     W002A         K78(72)
129600960103     C                   MOVEL     '/'           K78(74)
129700960103     C     2             SUBST     DATAPG:7      W002A
129800960103     C                   MOVEA     W002A         K78(75)
129900960103     C                   MOVEA     K78           VIDMSG
130000960103     C                   GOTO      ENDCTR
130100960103    1C                   ENDIF
130200911031     C*
130300921005     C* C O D I C E   P A D R O N C I N O
130400941007    1C     VI4PDR        IFNE      *BLANKS
130500941007     C     VI4PDR        ANDNE     *ZEROS
130600941007     C     '?'           SCAN      VI4PDR                                 31
130700930909     C*
130800930909    2C     *IN31         IFEQ      *ON
130900021203      *
131000021203      * imposto i campi della ds esterna fnlv24ds
131100021203     C                   clear                   fnlv24ds
131200021203     C                   eval      d24fil   = VI4LPD
131300021203     C                   eval      d24tip   = 'A'
131400021203     C                   eval      d24flg   = 'R'
131500021203     C                   movel     fnlv24ds      kpjbu
131600941011     C                   CALL      'FNLV24R'
131700930331     C                   PARM                    KPJBA
131800021203      *
131900021203      * valorizzo i campi a video
132000021203     C                   movel     kpjbu         fnlv24ds
132100021203     C                   if        d24pdr   >  *zeros
132200021203     C                   move      d24pdr        VI4PDR
132300021203     C                   movel     d24pdr        VI4LPD
132400021203     C                   else
132500021203     C                   clear                   VI4PDR
132600021203     C                   endif
132700021203     C                   movel     d24rsc        DESPDR
132800910201     C                   SETON                                        90
132900910201     C                   GOTO      ENDCTR
133000930909    2C                   END
133100910201     C*
133200941007     C* ESEGUO TESTN
133300941007     C                   TESTN                   VI4PDR               30
133400941010     C                   MOVE      VI4PDR        W001A             1
133500941007     C     *IN30         IFEQ      *OFF
133600941007     C     W001A         ORLT      '0'
133700941007     C                   SETON                                        4490
133800941007     C                   GOTO      ENDCTR
133900941007     C                   ENDIF
134000000518     C* TESTO SE IL PADRONCINO APPARTIENE AD UNA FILIALE POSTA
134100000524     C                   MOVEL     VI4LPD        WORG
134200000524     C                   EXSR      CTRORG
134300020219     C     WRKPT         IFEQ      'S'
134400020219     C     §OGNTW        ANDNE     'PPT'
134500020219     C     WRKPT         ORNE      'S'
134600020219     C     §OGNTW        ANDEQ     'PPT'
134700000518     C                   SETON                                        7090
134800000518     C                   GOTO      ENDCTR
134900000518     C                   ENDIF
135000941007     C*
135100941010     C                   MOVE      VI4PDR        W0070             7 0
135200950321     C                   MOVEL     VI4LPD        W0070             7 0
135300021203     C*    W0070         CHAIN     FNAPD000                           30
135400021205     C                   move      w0070         kpdpdr
135500021203     C                   move      'A'           kpdtip
135600021203     C     kfapd         chain     fiapd01l                           30
135700941007     C     *IN30         IFEQ      *ON
135800941007     C     *IN30         OREQ      *OFF
135900941007     C     APDATB        ANDNE     *BLANKS
136000941007     C                   SETON                                        4490
136100941007     C                   GOTO      ENDCTR
136200941007    1C                   ENDIF
136300941007     C* DESCRIZIONE
136400941010     C                   MOVEL     APDRSC        DESPDR
136500930909    1C                   ENDIF
136600010219     C*
136700010219     C* NUMERO O.R.M.
136800010219     C*
136900010220     C                   EXSR      ORMOBB
137000010219     C* Se immesso lo controllo solo se p.o. attivato a procedura ORM
137100010219    1C     VIDORM        IFGT      *ZEROS
137200060926     C**** WOBBL         ANDEQ     'S'
137300010227    3C     WIDORM        IFNE      ORMFIT
137400010219     C     KORM          CHAIN     FNORM01L                           30
137500010219    4C     *IN30         IFEQ      *ON
137600040819     c     ormatb        orne      *blanks
137700010220     C                   SETON                                        905828
137800010219     C                   MOVEL     ERR(7)        VIDMSG
137900010219     C                   GOTO      ENDCTR
138000010219    4C                   ENDIF
138100010220     C* CMD14 - INTERROGAZIONE ORM
138200010223     C     *INKH         IFEQ      *ON
138300010220     C                   CLEAR                   FIOR07
138400010220     C                   Z-ADD     VI4POE        D07POE
138500010220     C                   Z-ADD     VI4NSR        D07NSR
138600010223     C                   Z-ADD     VI4NO2        D07NOR
138700010220     C                   Z-ADD     VI4NRV        D07NRV
138800010220     C                   Z-ADD     VI4FGS        D07FGS
138900010220     C                   MOVEL     FIOR07        KPJBU
139000010220     C                   CALL      'FIOR07R'
139100010220     C                   PARM                    KPJBA
139200010226     C                   SETON                                        9058
139300010220     C                   GOTO      ENDCTR
139400010220     C                   ENDIF
139500010219     C* ERRORE SE P.O. RITIRO DIVERSO DA P.O. PADRONCINO
139600010219    4C     ORMPOR        IFNE      VI4LPD
139700060926     c* verifico se p.o. ritiro gestito dalla lnp bolla
139800060926     c* perchè in tal caso non devo dare errore se p.o. ritiro <> da
139900060926     c* lnp bolla
140000060926     c                   clear                   dslv55
140100060926     c                   eval      d55tpt = '6'
140200060926     c                   z-add     ormpor        d55lin
140300060926     c                   z-add     comdsp        d55drf
140400060926     c                   call      'FNLV55R'
140500060926     c                   parm                    dslv55
140600060926    5c                   if        d55err <> *blanks or d55tfa <> vi4lpd
140700060926     c* Verifico se si tratta di dati edi trasferiti: in questo caso
140800060926     c*  non do errore
140900150929    6c                   if        vi4ksc>0
141000150929     c                   clear                   tntb50ds
141100150929     C                   MOVEL     vi4ksc        b50ke2
141200060926     C*
141300150929     c                   movel     b50ke2        b50ke1
141400060926     c* controllo linea e p.o. orm
141500150929     c                   movel     'O'           b50opz
141600150929     c                   movel     ormpor        b50ormreso
141700150929     c                   movel     vi4lpd        b50poa
141800150929     c                   call      'TNTB51R'
141900150929     c                   parm                    kpjba
142000150929     c                   parm                    tntb50ds
142100150929     c                   endif
142200060926     c
142300150929    6c                   if        vi4ksc=0 or b50err<>' '
142400081215     c                   movel     ormpor        worg
142500081215     c                   exsr      ctrorg
142600081215     c                   move      §ogntw        ntwpor
142700081215     c                   movel     vi4lpd        worg
142800081215     c                   exsr      ctrorg
142900081215     c                   if        ntwpor<>'DPD' or §ogntw<>'DPD'
143000010220     C                   SETON                                        905828
143100010219     C                   MOVEL     ERR(8)        VIDMSG
143200010219     C                   GOTO      ENDCTR
143300081215     c                   ENDIF
143400150929    6C                   ENDIF
143500060926    5C                   ENDIF
143600060926    4C                   ENDIF
143700010219     C*
143800010219     C* ERRORE SE ORM PREPAGATO
143900010219    4C     ORMTOR        IFEQ      'P'
144000080110     C                   SETON                                        905828
144100010219     C                   MOVEL     ERR(9)        VIDMSG
144200010219     C                   GOTO      ENDCTR
144300010219    4C                   ENDIF
144400010219     C* ERRORE SE ORM IN CUI PAGA IL DESTINATARIO
144500010219    4C     ORMPAG        IFEQ      'D'
144600010220     C                   SETON                                        905828
144700010219     C                   MOVEL     ERR(10)       VIDMSG
144800010219     C                   GOTO      ENDCTR
144900010219    4C                   ENDIF
145000010223     C* ERRORE SE ORM CON LINEA DI ARRIVO FORZATA
145100010223    4C     ORMPOC        IFGT      *ZEROS
145200010223     C                   SETON                                        905828
145300010223     C                   MOVEL     ERR(13)       VIDMSG
145400010223     C                   GOTO      ENDCTR
145500010223    4C                   ENDIF
145600010219     C* ERRORE FORZABILE SE ORM IN FASE NON BOLLETTABILE
145700010219   4AC     VIDORM        IFNE      SAVORM
145800010219     C                   Z-ADD     VIDORM        SAVORM
145900010219     C* A CAMBIO NUMERO ORM DEVO PULIRE CAMPI PER CONTROLLO FORZATURE
146000010219     C* SU INCONGRUENZE CON ORM
146100010219     C                   CLEAR                   SA1PDR
146200010219     C                   CLEAR                   SA1DRT
146300010219     C                   CLEAR                   SA1FPP
146400010219     C                   CLEAR                   SA1KSC
146500070906     c                   clear                   s_drt
146600070906     c                   setoff                                       93
146700010219     C*
146800010219     C                   CLEAR                   DSBS02
146900010219     C                   MOVEL     'C'           T02MOD
147000010219     C                   MOVEL     KNSIF         T02SIF
147100010219     C                   MOVEL     'FAR'         T02COD
147200010219     C                   MOVEL     ORMFAO        T02KE1
147300010219      *
147400010219     C                   CALL      'TIBS02R'
147500010219     C                   PARM                    KPJBA
147600010219     C                   PARM                    DSBS02
147700010219    4C     T02ERR        IFEQ      *BLANKS
147800010219     C                   MOVEL     T02UNI        DFAR
147900010219     C                   ELSE
148000010219     C                   CLEAR                   DFAR
148100010219    4C                   ENDIF
148200010219    4C     §FARFB        IFNE      'S'
148300010219     C                   MOVEL     ERR(11)       VIDMSG
148400010220     C                   SETON                                        905828
148500010219     C                   GOTO      ENDCTR
148600010219    4C                   ENDIF
148700010219   4AC                   ENDIF
148800010226   X3C                   ELSE
148900070829     c* se orm fittizio a cambio numero orm reimposto udate nella data
149000070829     c* ritiro
149100070829     c                   if        vidorm<>savorm and vi4drt<>wdtgio
149200071107     c*****                        and vi4drt=s_drt and dateu>=§vpodrt
149300071107     c                             and vi4drt=s_drt
149400070829     C                   Z-ADD     VIDORM        SAVORM
149500070829     c                   z-add     wdtgio        vi4drt
149600070906     c                   eval      s_drt=vi4drt
149700070829     c                   seton                                        90
149800070831     c                   setoff                                       93
149900070829     C                   GOTO      ENDCTR
150000070829     c                   endif
150100070829     c*
150200010226     C   KH              SETON                                        9058
150300010226     C   KH              GOTO      ENDCTR
150400010219    3C                   ENDIF
150500010226   X1C                   ELSE
150600010226     C   KH              SETON                                        9058
150700010226     C   KH              GOTO      ENDCTR
150800010219    1C                   ENDIF
150900950127     C*
151000950127     C* D A T A   R I T I R O   M E R C E
151100070803     c* se immesso orm e bolla non import forzo come data ritiro la data
151200070803     c* ritiro dell'orm
151300070803    1C     WIDORM        IFNE      ORMFIT
151400070904     c     widorm        andgt     0
151500071107     c**** dateu         andge     §vpodrt
151600070803     C                   MOVEL     VI4LPD        WORG
151700070803     C                   EXSR      CTRORG
151800070803     C                   CLEAR                   WLBDAT
151900070803     C                   MOVEL     '3'           G02ERR
152000070803     c* se fase utilizzabile propongo la data della fase
152100070803    3C     d§fardar      ifeq      'S'
152200071112     c                   if        ormfao=400
152300071112     c                   z-add     ormddc        g02inv
152400071112     c                   else
152500070803     C                   Z-ADD     ORMDFO        G02INV
152600071112     c                   endif
152700070803   x3c                   else
152800070803     c* se l'orm non è in una fase utilizzabile per la data ritiro prendo
152900070803     c* la data ritiro presente sull'orm
153000070803     C                   Z-ADD     ORMDAR        G02INV
153100070803    3c                   endif
153200070803     c* se la data ritiro da proporre è maggiore della data odierna non
153300070803     c* propongo nulla
153400070905    3c                   if        g02inv<=dateu and
153500070905     c                             §ogntw<>'DPD' and §ogntw<>'FED' and
153600070905     c                             §ogntw<>'EEX'
153700070803     C                   CALL      'XSRDA8'
153800070803     C                   PARM                    WLBDAT
153900070803    4c                   if        g02dat<>vi4drt
154000070803     C                   Z-ADD     G02DAT        VI4DRT
154100070906     C                   Z-ADD     G02DAT        s_drt
154200070906     C***                Z-ADD     g02inv        comdrt
154300070906     c***                z-add     g02inv        sa1drt
154400070831     C                   seton                                        9093
154500070803     c                   goto      endctr
154600070803    4c                   endif
154700070905     c                   else
154800070906     c* se non propongo la data perchè > data giorno o bolla import
154900070906     c* propongo udate
155000071107     c****               if        s_drt=0 or dateu<§vpodrt
155100071107     c                   if        s_drt=0
155200070906     c                   z-add     wdtgio        vi4drt
155300070906     c                   z-add     wdtgio        s_drt
155400070906     c**                 z-add     dateu         comdrt
155500070906     c**                 z-add     dateu         sa1drt
155600070906     c                   seton                                        90
155700070906     c                   goto      endctr
155800070906     c                   endif
155900070905     c
156000070803    3c                   endif
156100070803    1c                   endif
156200070803     c*
156300950127    1C     VI4DRT        IFGT      0
156400950127     C                   Z-ADD     VI4DRT        G02DAT
156500950127     C                   MOVEL     *BLANK        G02ERR
156600950127     C                   CALL      'XSRDA8'
156700950127     C                   PARM                    WLBDAT
156800070904    2C*    G02ERR        IFEQ      '1'
156900070904     C*    G02INV        ORGT      DATEU
157000070904     C*    G02INV        ORLT      DATAM
157100070904     c                   if        g02err='1' or g02inv>dateu or
157200080110     c                             g02inv<datamin
157300080110     c****                         (g02inv<datam and (widorm=ormfit or
157400071107     c****                         dateu<§vpodrt or s_drt<>vi4drt))
157500080110     c****                         s_drt<>vi4drt))
157600080110     C                   MOVEL     ERR(24)       VIDMSG
157700080110     C                   SETON                                        522890
157800080110     c* se errore data ritiro in quanto viene forzata data orm aggiungo
157900080110     c* nel messaggio di verificare l'orm
158000080110     c                   if        g02inv<datamin and widorm<>ormfit
158100080110     c                   eval      vidmsg=%trim(vidmsg)
158200080110     c                             + ': verificare l''O.R.M.'
158300080110     c                   endif
158400950127     C                   GOTO      ENDCTR
158500950127    2C                   ENDIF
158600950127     C*
158700950127     C                   Z-ADD     G02INV        COMDRT            8 0
158800070906     C                   Z-ADD     vi4drt        s_drt
158900950127     C* SE IMMESSO COD.PADRONCINO CONTROLLO CHE NON SIANO STATO GIA'
159000950127     C*  CONFERMATI I CONTEGGI
159100950127    2C     W0070         IFGT      0
159200950127     C                   Z-ADD     W0070         W01PDR
159300011126     C     KFTT          SETLL     FIFTT000                               30
159400950127    3C     *IN30         IFEQ      *ON
159500080110     C                   MOVEL     ERR(25)       VIDMSG
159600080110     C                   SETON                                        522890
159700950127     C                   GOTO      ENDCTR
159800950127    3C                   ENDIF
159900950127    2C                   ENDIF
160000950206    1C                   ENDIF
160100990107     c* se modificata serie e cod cliente rido messaggio di frozatura
160200990107     c*  cod trattamento merce
160300990107     C     VI4KSC        IFNE      SAVKSC
160400990107     C     VI4NRS        ORNE      SAVNRS
160500990107     C     VI4CTM        ORNE      SA2CTM
160600990107     C                   CLEAR                   WDUE
160700051207     C                   MOVEL     VI4KSC        SAvKSC
160800990107     C                   MOVEL     VI4NRS        SAVNRS
160900990107     C                   MOVEL     VI4CTM        SA2CTM
161000990107     C                   ENDIF
161100041222
161200041222      * se modificata serie e cod cliente riemetto messaggio di forzatura
161300041222      * codice tariffa
161400041222     c                   If        vi4ksc <> savksc or
161500041222     c                             vi4nrs <> savnrs or
161600041222     c                             vi4ctr <> savctr
161700041222     c                   Reset                   wfctr
161800041222     c                   Eval      savksc = vi4ksc
161900041222     c                   Eval      savnrs = vi4nrs
162000041222     c                   Eval      savctr = vi4ctr
162100041222     c                   EndIf
162200041222
162300920324     C*
162400921005     C* C O D I C E   T R A T T A M E N T O   M E R C E
162500930909    1C     VI4CTM        IFNE      *BLANKS
162600941007     C     '?'           SCAN      VI4CTM                                 90
162700930909     C*
162800941007    2C     *IN90         IFEQ      *ON
162900920324     C                   MOVEL     CODUT         §KUT
163000920324     C                   MOVEL     '1B'          §COD
163100920324     C                   MOVE      *BLANKS       VI4CTM
163200920324     C                   CALL      'X§TABER'
163300920324     C                   PARM                    §KUT
163400920324     C                   PARM                    §COD
163500920324     C                   PARM                    §KEY
163600920324     C                   PARM                    §DES             30
163700920324     C                   MOVEL     §KEY          VI4CTM
163800920324     C                   GOTO      ENDCTR
163900930909    2C                   END
164000920324     C*
164100000518     C* IMPOSSIBILE IMMETTERE QUALSIASI CODICE TRATTAMENTO MERCE IN CONFERMA
164200000518     C* BOLLE POSTE
164300000518     C     WRKPT         IFEQ      'S'
164400000518     C                   MOVEL     ERR(5)        VIDMSG
164500000518     C                   SETON                                        289064
164600000518     C                   GOTO      ENDCTR
164700000518     C                   ENDIF
164800000518     C**
164900920324     C                   MOVEL     '1B'          COD
165000941007     C                   MOVEL(P)  VI4CTM        KEY
165100920324     C     KTAB          CHAIN     TABEL                              30
165200920324     C* ANNULLATO
165300930909    2C  N30TBLFLG        IFNE      ' '
165400920324     C                   SETON                                        30
165500930909    2C                   END
165600920324     C* ERRORE
165700920324     C   30              SETON                                        5490
165800920324     C   30              GOTO      ENDCTR
165900020801
166000020801     C                   MOVEL     TBLUNI        DS1B
166100990107     C*
166200020801     C* Controllo se possibile utilizzare il codice trattamento merce
166300020801     c                   if        §1bf15 = 'S'
166400990107     C                   MOVEL     ERR(2)        VIDMSG
166500020801     c                   eval      %subst(vidmsg:47:2) = vi4ctm
166600990107     C                   SETON                                        289064
166700990107     C                   GOTO      ENDCTR
166800990107    2C                   ENDIF
166900120709     c*
167000120709     c* Non è sol "A" e non è autospunta:
167100120709     c* se prevede VAX --> errore
167200120709     c* non prevede VAX--> messaggio di avviso
167300120709     C     §1BFG1        IFne      'N'
167400120709     c     §1bfg5        andne     'N'
167500120709     c                   if        §1bf16='S'
167600120709     C                   MOVEL     ERR(32)       VIDMSG
167700120709     C                   SETON                                        289064
167800120709     C                   GOTO      ENDCTR
167900120709     c                   endif
168000120709     c                   if        vi4ctm<>s_vi4ctm
168100120709     C                   MOVEL     ERR(33)       VIDMSG
168200120709     C                   SETON                                        289064
168300120709     c                   eval      s_vi4ctm=vi4ctm
168400120709     C                   GOTO      ENDCTR
168500120709     c                   endif
168600120710     c* memorizzazione in un flag per controllare che il cod.trattam merce
168700120710     c* forzato sia utilizzabile per la bolla da confermare
168800120710     c                   eval      wctrctm='1'
168900120710     c* memorizzo i flag del ctm dall'1 al 13 che mi servirà per confrontarli
169000120710     c* con i flag del ctm del vab
169100120710     c                   eval      w1b=%subst(ds1b:26:13)
169200120709     c*
169300120709     c                   endif
169400990204     C*
169500990204     C* POSSO UTILIZZARE SOLO CTM CON SOLUZIONE "A"
169600090803     c* o  di autospunta
169700090803     C***  §1BFG1        IFNE      'N'
169800090803     c
169900120709     C***  §1BFG1        IFeq      'N'
170000120709     c***  §1bfg1        oreq      'S'
170100120709     c***  §1bfg5        andeq     'N'
170200120709     c***                else
170300120709     C***                MOVEL     ERR(4)        VIDMSG
170400120709     C***                SETON                                        289064
170500120709     C***                GOTO      ENDCTR
170600120709     C***                ENDIF
170700990107     C* CHIEDO FORZATURA DEL COD TRATTAMANTO MERCE
170800990107    2C     WDUE          IFEQ      ' '
170900990107     C                   MOVEL     '1'           WDUE              1
171000990107     C                   MOVEL     ERR(3)        VIDMSG
171100990107     C                   SETON                                        289064
171200990107     C                   GOTO      ENDCTR
171300990107    2C                   ENDIF
171400990107     C**
171500990107    1C                   ENDIF
171600041222
171700041222      * C O D I C E   T A R I F F A
171800041222if  1c                   If        vi4ctr <> *Blanks
171900041222      * Solo campi numerico
172000041222     c                   If        %subst(vi4ctr:1:1) < '0' or
172100041222     c                             %subst(vi4ctr:1:1) = ' ' or
172200041222     c                             %subst(vi4ctr:2:1) < '0' or
172300041222     c                             %subst(vi4ctr:2:1) = ' ' or
172400041222     c                             %subst(vi4ctr:3:1) < '0' or
172500041222     c                             %subst(vi4ctr:3:1) = ' '
172600041222     c                   Eval      *In28 = *On
172700041222     c                   Eval      *In67 = *On
172800041222     c                   Eval      *In90 = *On
172900041222     c                   Eval      vidmsg = err(21)
173000041222     c                   Goto      endctr
173100041222     c                   EndIf
173200041222
173300041222      * Msg info x forzatura codice tariffa
173400041222if  2c                   If        wfctr = *Off
173500041222     c                   Eval      *In28 = *On
173600041222     c                   Eval      *In67 = *On
173700041222     c                   Eval      *In90 = *On
173800041222     c                   Eval      vidmsg = err(22)
173900041222     c                   Eval      wfctr = *On
174000041222     c                   Goto      endctr
174100041222e   2c                   EndIf
174200041222e   1c                   EndIf
174300041222
174400910201     C*
174500921005     C* C O D I C E   C L I E N T E
174600930909    1C     VI4KSC        IFNE      0
174700941010     C                   MOVEL     VI4KSC        KSC
174800941010     C                   EXSR      CTRCCM
174900941010     C* 90 ON - ERRORI
175000941010     C   90              GOTO      ENDCTR
175100941010     C* 02 ON - IMMESSO CODICE CLIENTE MITTENTE
175200941010     C                   SETON                                        02
175300941010     C                   ELSE
175400941010     C                   CLEAR                   WCCMN
175500941010     C                   ENDIF
175600000613     C* SE INSERITO CODICE CLIENTE VERIFICARE SE CLIENTE APPARTIENE A POSTE
175700000613     C* E NON STO SELEZIONANDO DELLE POSTE
175800000613     C*
175900000613     C     VI4KSC        IFNE      0
176000000613     C                   MOVEL     VI4KSC        WORG
176100000613     C                   EXSR      CTRORG
176200020219     C     WRKPT         IFEQ      'S'
176300020219     C     §OGNTW        ANDNE     'PPT'
176400020219     C     WRKPT         ORNE      'S'
176500020219     C     §OGNTW        ANDEQ     'PPT'
176600000614     C                   SETON                                        7290
176700000613     C                   GOTO      ENDCTR
176800000613     C                   ENDIF
176900001207     C* SE CLIENTE DPD IL P.O. AUTOTRASPORTATORE DEVE ESSERE = AL P.O.
177000051207     C* CLIENTE  oppure in tabella 3TR
177100150929     c* NON più: se cliente dpd il p.o. aut deve corrsipondere a p.o. DPD
177200020219     C     §OGNTW        IFEQ      'DPD'
177300001207     C     VI4PDR        ANDGT     *ZEROS
177400001207     C     VI4LPD        ANDNE     WORG
177500051207     c
177600051207     c* Controllo da tabella 3TR se dati inviabili ad altro p.o.
177700150929     c***                clear                   tntb50ds
177800150929     c***                if        vi4ksc>0
177900150929     C***                MOVEL     vi4ksc        b50ke2
178000051207     c
178100150929     c***                movel     b50ke2        b50ke1
178200150929     c***                movel     'C'           b50opz
178300150929     c***                movel     vi4lpd        b50poa
178400150929     c***                call      'TNTB51R'
178500150929     c***                parm                    kpjba
178600150929     c***                parm                    tntb50ds
178700150929     c***                endif
178800051207     c
178900150929     c***                if        b50err<>' '  or vi4ksc=0
179000150929     c                   movel     vi4lpd        worg
179100150929     C                   EXSR      CTRORG
179200150929     c                   if        §ogntw<>'DPD'
179300001207     C                   SETON                                        7690
179400001207     C                   GOTO      ENDCTR
179500001207     C                   ENDIF
179600051207     C                   ENDIF
179700000613     C                   ENDIF
179800041122     C
179900041122     c* L I N E A   D I   A R R I V O
180000041122     C                   IF        VI4LNA<>999
180100041122    2C     vi4lna        IFGT      *ZEROS
180200041122     C     vi4lna        CHAIN     AZORG01L                           30
180300041122     C                   ELSE
180400041122     C                   SETON                                        30
180500041122    2C                   END
180600041122     C*
180700041122     C* CONTROLLO SE ANNULLATO
180800041122     C* CONSIDERO SOLO I TIPI "F" E "A"
180900041122    2C  N30ORGFVA        IFNE      *BLANKS
181000041122    2C     ORGFAG        ORNE      'F'
181100041122     C     ORGFAG        ANDNE     'A'
181200041122     C                   SETON                                        30
181300041122    2C                   END
181400041122     C* CONTROLLO CHE NON SIA UNA LINEA BLOCCATA
181500041122     C                   MOVEL     ORGDE8        OG148
181600041122     C     §OGLBL        IFEQ      'N'
181700041122     C                   SETON                                        30
181800041122    2C                   END
181900041122     C*
182000041122    2C     *IN30         IFEQ      *ON
182100041123     C                   MOVEL     ERR(20)       VIDMSG
182200041122     C                   SETON                                        909228
182300041122     C                   GOTO      endctr
182400041122    2C                   ENDIF
182500060306     c                   else
182600060306     c* Se codice che prevede obbligo lna segnalo errore
182700060306    2c                   if        not *in08  and §clilna='S'
182800060306     C                   MOVEL     ERR(23)       VIDMSG
182900060306     C                   SETON                                        909228
183000060306     C                   GOTO      endctr
183100060306    2C                   ENDIF
183200060306     c
183300060306    1C                   ENDIF
183400041122     C
183500111214     C* L I N E A   D I    P A R T E N Z A   S E G N A C O L L O
183600111214     c                   if        vi4lnp>0
183700111214     C     vi4lnp        CHAIN     AZORG01L
183800111220     c                   if        not %found(azorg01l) or orgfva<>*blanks
183900111220     c                             or (orgfag <> 'F' and orgfag <>'A')
184000111214     C                   MOVEL     ERR(29)       VIDMSG
184100111214     C                   SETON                                        909428
184200111214     C                   GOTO      endctr
184300111214     c                   endif
184400111214     c                   endif
184500111214     c
184600900807     C*
184700921005     C* R I F E R I M E N T O   M I T T E N T E
184800930909    1C     VI4RMN        IFNE      0
184900080623     c* Se richiesto ordinamento per rif, alfabetico, errore
185000130121     c                   if        vi4ord='A' or vi4ord='S'
185100080623     C                   MOVEL     ERR(26)       VIDMSG
185200080623     C                   SETON                                        906128
185300080623     C                   GOTO      endctr
185400080623     c                   endif
185500911031     C*
185600900807     C* CONTROLLO ESISTENZA DELLA BOLLA CON QUEL RIFERIMENTO
185700930909     C* SOLO RIFERIMENTO SENZA CLIENTE
185800020916     C                   SETOFF                                       3031
185900020917     C  N02VI4RMN        SETLL     FIVAB03L                               31
186000930909     C*
186100930909     C* RIFERIMENTO CON CLIENTE
186200020917     C   02KVAB          SETLL     FIVAB01L                               31
186300930909     C*
186400020917     C  N02VI4RMN        READE     FIVAB03L                               31
186500020917     C   02KVAB          READE     FIVAB01L                               31
186600020916      *
186700020916     C     *IN31         DOWEQ     *OFF
186800020916     C                   MOVEL     VABFGS        WFIL              3 0
186900020916      *
187000020926      * VERIFICO IL P.O. gestione
187100020926     C     WFIL          LOOKUP    POG                                    30
187200170901     C** 30              LEAVE
187201170901     c* Verifico anche VABATB
187202170901     c                   if        *in30 and VABATB <> 'A'
187203170901     c                   LEAVE
187205170901     c                   ENDIF
187300020916      *
187400020917     C  N02VI4RMN        READE     FIVAB03L                               31
187500020917     C   02KVAB          READE     FIVAB01L                               31
187600020916     C                   ENDDO
187700020917     C   31              SETON                                        4190
187800020917     C   31              GOTO      ENDCTR
187900930909     C*
188000930909     C* OBBLIGATORIO PADRONCINO E DATA RITIRO MERCE
188100941010    2C     VI4PDR        IFEQ      *ZEROS
188200941010     C     VI4PDR        OREQ      *BLANKS
188300930909     C                   SETON                                        4290
188400930909     C                   GOTO      ENDCTR
188500930909    2C                   ENDIF
188600930909     C*
188700930909    2C     VI4DRT        IFEQ      0
188800930909     C                   SETON                                        4590
188900930909     C                   GOTO      ENDCTR
189000930909    2C                   ENDIF
189100010220     C* OBBLIGATORIO ANCHE IL N.ORM SE ATTIVATA PROCEDURA
189200010220     C     WOBBL         IFEQ      'S'
189300010220     C     VIDORM        ANDEQ     *ZEROS
189400010220     C                   SETON                                        589028
189500010220     C                   MOVEL     ERR(12)       VIDMSG
189600010220     C                   GOTO      ENDCTR
189700010220     C                   ENDIF
189800930909     C*
189900930909     C                   SETON                                        03
190000930909    1C                   ENDIF
190100010220     C*
190200010220     C* SEGNALAZIONE INCONGRUENZE CAMPI DEL VIDEO CON DATI DELL'ORM
190300010220     C*
190400010220   1AC     VIDORM        IFGT      *ZEROS
190500060926     C***  WOBBL         ANDEQ     'S'
190600010227     C     WIDORM        ANDNE     ORMFIT
190700010220     C* INCONGRUENZA CON CODICE AUTOTRASPORTATORE
190800010220    1C     ORMNDC        IFGT      *ZEROS
190900010220     C     VI4PDR        ANDNE     SA1PDR
191000010221     C     VI4PDR        ANDGT     *ZEROS
191100010220     C                   MOVE      VI4PDR        SA1PDR
191200120607     C     KDST          CHAIN     FIDST01L                           30
191300010220     C   30              CLEAR                   DSTPDR
191400010220     C                   MOVE      DSTPDR        WDPDR
191500010220    2C     WDPDR         IFNE      VI4PDR
191600010220     C                   SETON                                        9077
191700010220     C                   GOTO      ENDCTR
191800010220    2C                   ENDIF
191900010220    1C                   ENDIF
192000010220     C* Il flag mattino/pomeriggio lo controllo solo se il PDR è =
192100010220    2C     ORMNDC        IFGT      *ZEROS
192200010220     C     VI4PDR        ANDEQ     WDPDR
192300010221     C     VI4PDR        ANDGT     *ZEROS
192400010220     C     VI4FPP        ANDNE     SA1FPP
192500010220     C                   MOVE      VI4FPP        SA1FPP
192600120607     C                   MOVE      ORMNDC        W0050             5 0
192700010220     C     KFVV          CHAIN     FNFVV01L                           30
192800010220    3C     FVVFPP        IFNE      VI4FPP
192900010220     C                   SETON                                        9081
193000010220     C                   GOTO      ENDCTR
193100010220    3C                   ENDIF
193200010220    2C                   ENDIF
193300010220     C* INCONGRUENZA CON DATA RITIRO
193400071107     c***                if        dateu<§vpodrt
193500071107    1C***  d§fardar      IFEQ      'S'
193600071107     C***  COMDRT        ANDNE     SA1DRT
193700071107     C***                Z-ADD     COMDRT        SA1DRT
193800071107    2C***  ORMDFO        IFNE      COMDRT
193900071107     C***                SETON                                        9078
194000071107     C***                GOTO      ENDCTR
194100071107    2C***                ENDIF
194200071107    1C***                ENDIF
194300071107     c****               endif
194400010220     C* INCONGRUENZA CON CODICE CLIENTE
194500010220    1C     ORMKSC        IFGT      *ZEROS
194600010220     C     ORMKSC        ANDNE     VI4KSC
194700010220     C     VI4KSC        ANDNE     SA1KSC
194800010220     C                   Z-ADD     VI4KSC        SA1KSC
194900010220     C                   SETON                                        9079
195000010220     C                   GOTO      ENDCTR
195100010220    1C                   ENDIF
195200010220     C*
195300010220   1AC                   ENDIF
195400080623     C*
195500080623     C* O R D I N A M E N T O   P E R   R I F . A L F A B E T I C O
195600130121    1C                   IF        VI4ORD='A' or VI4ORD='S'
195700080623     C* CODICE CLIENTE obbligatorio
195800080623     c                   if        vi4ksc=0
195900080623     c                   Eval      *In28 = *On
196000080623     c                   Eval      *In88 = *On
196100080623     c                   Eval      *In90 = *On
196200080623     c                   Eval      VidMsg = Err(27)
196300080623     c                   GoTo      EndCtr
196400080623     c                   endif
196500080623     c                   endif
196600030409
196700030409      * C O N S E G N A   R I C H I E S T A
196800030409
196900030411      * immettere i dati della consegna richiesta solo se è indicato il
197000030411      * cliente o la serie
197100030411     c                   If        (Vi4Dcr > *Zeros or Vi4Hcr > *Zeros or
197200030411     c                              Vi4Ore > *Zeros) and
197300030411     c                             (Vi4Ksc = *Zeros and
197400030411     c                             (Vi4Nrs = *Zeros or Vi4Nrs = 99))
197500030411     c                   Eval      *In28 = *On
197600030411     c                   Eval      *In88 = *On
197700030411     c                   Eval      *In90 = *On
197800030411     c                   Eval      VidMsg = Err(18)
197900030411     c                   GoTo      EndCtr
198000030411     c                   EndIf
198100030409      * tipo
198200030411     c                   If        Vi4Tcr <> *Blanks
198300030411      * o il tipo o le ore da sommare alla delivery
198400030411     c                   If        Vi4Ore > *Zeros
198500030411     c                   Eval      *In28 = *On
198600030411     c                   Eval      *In87 = *On
198700030411     c                   Eval      *In90 = *On
198800030411     c                   Eval      VidMsg = Err(16)
198900030411     c                   GoTo      EndCtr
199000030411     c                   EndIf
199100030411     c                   If        Vi4Dcr = *Zeros and Vi4Hcr = *Zeros
199200030409     c                   Eval      *In28 = *On
199300030409     c                   Eval      *In87 = *On
199400030409     c                   Eval      *In90 = *On
199500030409     c                   Eval      VidMsg = Err(17)
199600030409     c                   GoTo      EndCtr
199700030409     c                   EndIf
199800030411     c                   EndIf
199900030409      * data
200000030409     c                   If        Vi4Dcr > *Zeros
200100030409      * controllo formale della data
200200030409     c                   Clear                   wlbdat
200300030409     c                   Z-Add     Vi4Dcr        G02Dat
200400030409     c                   Call      'XSRDA8'
200500030409     c                   Parm                    wlbdat
200600030409    1c                   If        G02Err = '1'
200700030409     c                   Eval      *In28 = *On
200800030409     c                   Eval      *In85 = *On
200900030409     c                   Eval      *In90 = *On
201000030409     c                   Eval      VidMsg = Err(14)
201100030409     c                   GoTo      EndCtr
201200030409     c                   EndIf
201300030409     c                   Movel     G02Dat        Vi4Dcr
201400030409     c                   Movel     G02Inv        wdcr
201500030409      * Non deve essere inferiore/uguale a oggi
201600030409     c                   If        wdcr <= dateu
201700030409     c                   Eval      *In28 = *On
201800030409     c                   Eval      *In85 = *On
201900030409     c                   Eval      *In90 = *On
202000030409     c                   Eval      VidMsg = Err(14)
202100030409     c                   GoTo      EndCtr
202200030409     c                   EndIf
202300030415      * Non deve essere superiore a oggi + 5 gg.
202400051215     c                   if        vi4tcr = 'P'
202500051215     c                   z-add     woggidcgm1    s_woggidcg
202600051215     c                   z-add     §vpodcgm1     s_vpodcg
202700051215     c                   else
202800051215     c                   z-add     woggidcgm2    s_woggidcg
202900051215     c                   z-add     §vpodcgm2     s_vpodcg
203000051215     c                   endif
203100051215     c                   If        wdcr > s_woggidcg
203200030409     c                   Eval      *In28 = *On
203300030409     c                   Eval      *In85 = *On
203400030409     c                   Eval      *In90 = *On
203500041202     c                   Eval      VidMsg = Err(15)
203600041202     c                   Eval      VidMsg = %trim(vidmsg) + ' ' +
203700051215     c                             %trim(%editc(s_vpodcg:'Z'))  +
203800041202     c                             ' giorni dalla data odierna'
203900030409     c                   GoTo      EndCtr
204000030409     c                   EndIf
204100030409      * o la data o le ore da sommare alla delivery
204200030409     c                   If        Vi4Ore > *Zeros
204300030409     c                   Eval      *In28 = *On
204400030409     c                   Eval      *In85 = *On
204500030409     c                   Eval      *In90 = *On
204600030409     c                   Eval      VidMsg = Err(16)
204700030409     c                   GoTo      EndCtr
204800030409     c                   EndIf
204900030409     c                   EndIf
205000030411      * ora
205100030411      * o l'ora o le ore da sommare alla delivery
205200030411     c                   If        Vi4Hcr > *Zeros and
205300030411     c                             Vi4Ore > *Zeros
205400030411     c                   Eval      *In28 = *On
205500030411     c                   Eval      *In86 = *On
205600030411     c                   Eval      *In90 = *On
205700030411     c                   Eval      VidMsg = Err(16)
205800030411     c                   GoTo      EndCtr
205900030411     c                   EndIf
206000030415
206100030415      * Ore da sommare alla delivery
206200030415     c                   If        Vi4Ore > *Zeros and
206300030415     c                             Vi4Ore <> 24 and Vi4Ore <> 48 and
206400030415     c                             Vi4Ore <> 72
206500030415     c                   Eval      *In28 = *On
206600030415     c                   Eval      *In89 = *On
206700030415     c                   Eval      *In90 = *On
206800030415     c                   Eval      VidMsg = Err(19)
206900030415     c                   GoTo      EndCtr
207000030415     c                   EndIf
207100900514     C*
207200900514     C     ENDCTR        ENDSR
207300010219     C*
207400010219     C* CONTROLLO OBBLIGATORIETA' NUMERO ORM   ----------------------*
207500010219     C     ORMOBB        BEGSR
207600010219     C                   CLEAR                   WOBBL
207700010219     C     VI4LPD        IFGT      *ZEROS
207800010219     C                   MOVEL     VI4LPD        WORG
207900010219     C                   EXSR      CTRORG
208000010219     C                   MOVEL     ORGDE8        OG148
208100010219     C     §OGORM        IFEQ      'S'
208200010219     C                   MOVEL     'S'           WOBBL             1
208300010219     C                   ENDIF
208400010219     C                   ENDIF
208500010219     C                   ENDSR
208600941010     C*
208700941010     C* CONTROLLO CODICE CLIENTE ------------------------------------*
208800941010     C     CTRCCM        BEGSR
208900941010     C                   SETOFF                                       06
209000970512     C**
209100970512     C     KACO          CHAIN     CNACO000                           31
209200970512     C*
209300990518    1C     *IN31         IFEQ      *ON
209400970512     C     *IN31         OREQ      *OFF
209500970512     C     ACOFLG        ANDNE     *BLANKS
209600970512     C                   SETON                                        4090
209700970512     C                   GOTO      ENDCCM
209800990518    1C                   ENDIF
209900120906     c*
210000120906     c* Il seguente controllo non lo ripeto se provengo da conferma totale
210100120906     c* La conferma totale può essere fatta solo se immesso il cliente e in
210200120906     c* questo caso il controllo è gia stato fatto sempre da questa routine
210300120906     c* quando richiamata dalla CTRD01
210400120906     c* Se lo ripetesse si sposizionerebbe la lettura sul fivab fatta nella rout
210500120906     c* di conferma totale con conseguente errore dopo la call fnls01r
210600120906     c                   if        wconf=*off or tota<>'S'
210700970512     C*
210800970512     C* CONTROLLO SE ESISTONO BOLLE PER IL CODICE DIGITATO
210900020917     C* APPARTENENTI A £1/£6
211000020916     C                   SETOFF                                       3031
211100020927     C     KSC           SETLL     FIVAB01L
211200020927     C     KSC           READE(N)  FIVAB01L                               31
211300020916      *
211400020916     C     *IN31         DOWEQ     *OFF
211500020916     C                   MOVEL     VABFGS        WFIL              3 0
211600020916      *
211700020926      * VERIFICO IL P.O DI GESTIONE
211800161121      * se devo andare nel riepilogo, questo test deve dire sempre ok
211900161121     C                   IF        not *inkj
212000020926     C     WFIL          LOOKUP    POG                                    30
212100161121     C                   ELSE
212200161121     C                   SETON                                            30
212300161121     C                   ENDIF
212301170901     C                   if        *in30 and vabatb<>'A'
212302170901     C                   leave
212303170901     C                   ENDIF
212400170901     C*  30              LEAVE
212500020916      *
212600020927     C     KSC           READE(N)  FIVAB01L                               31
212700020916     C                   ENDDO
212800020916     C   31              SETON                                        4390
212900020916     C   31              GOTO      ENDCCM
213000120906     c                   endif
213100970512     C*
213200970512     C* IMPOSTO IL CODICE DA UTILIZZARE NELLA CONFERMA
213300970512     C*  = AL CODICE DEL DISK
213400970512     C                   MOVEL     KSC           WCCMN
213500970512     C**
213600020219     C* CONTROLLI SU TABELLA 3NN
213700020405     C**
213800020405     C* Flag che indica l'esistenza solo del record valido '999'
213900020405     C                   CLEAR                   WES3N_S999        1
214000970512     C                   CLEAR                   WES3N
214100970516     C                   MOVEL     KSC           W0030
214200020219      *
214300020220      * VERIFICO SE ESISTE ALMENO UN RECORD VALIDO DELLA 3NN
214400020220     C                   MOVE      *BLANKS       TBEKE1
214500020220     C                   MOVEL     KSC           TBEKE1
214600020220     C     K02TBE        SETLL     TNTBE02L
214700020220     C     K02TBE        READE     TNTBE02L                               31
214800020220    1C     *IN31         DOWEQ     *OFF
214900020405     C     TBEATB        IFEQ      ' '
215000020405     C     TbeKe2        ifeq      '999'
215100020405     C     WEs3N         andne     'S'
215200020405     C                   MOVEL     'S'           WES3N_S999
215300020405     C                   else
215400020405     C                   MOVEL     ' '           WES3N_S999
215500020405     C                   endif
215600020405     C                   MOVEL     'S'           WES3N
215700020405     C                   ENDIF
215800020220     C     K02TBE        READE     TNTBE02L                               31
215900020320   1-C                   ENDDO
216000020320    1C     WES3N         IFEQ      'S'
216100020220      *
216200020219      * PROVO PRIMA A LEGGERE TABELLA 3NN CON P.O = P.O CODICE
216300020220     C                   MOVEL     VI4LPD        TBEKE2
216400020220     C     K03TBE        CHAIN     TNTBE02L                           32
216500020220    2C     *IN32         IFEQ      *OFF
216600020219     C     TBEATB        ANDEQ     ' '
216700020219      *
216800020220      * RECUPERO NUOVO CODICE
216900070914    3C***  TBEUNI        IFEQ      *BLANKS
217000070914     c                   if        %subst(tbeuni:1:7)<=*zeros
217100020219     C                   MOVEL     TBEKE1        WCCMN
217200020219     C                   ELSE
217300020219     C                   MOVEL     TBEUNI        WCCMN
217400020220   3-C                   ENDIF
217500020220   2EC                   ELSE
217600020219      *
217700020219      * PROVO POI A LEGGERE TABELLA 3NN CON P.O = 999
217800020219     C                   MOVEL     '999'         TBEKE2
217900020220     C     K03TBE        CHAIN     TNTBE02L                           32
218000020220    3C     *IN32         IFEQ      *OFF
218100020219     C     TBEATB        ANDEQ     ' '
218200020220      *
218300020220      * SE IL P.O. DEL NUOVO CODICE E' UGUALE AL P.O DI CONFERMA
218400020220      * USO NUOVO CODICE
218500070914    4C***  TBEUNI        IFEQ      *BLANKS
218600070914     c                   if        %subst(tbeuni:1:7)<=*zeros
218700020220     C                   MOVEL     TBEKE1        WTBEC             3 0
218800020220    5C     VI4LPD        IFEQ      WTBEC
218900020320     C     VI4LPD        ORNE      W0030
219000020220     C                   MOVEL     TBEKE1        WCCMN
219100020220   5-C                   ENDIF
219200020220   4EC                   ELSE
219300020220     C                   MOVEL     TBEUNI        WTBEC             3 0
219400020220    5C     VI4LPD        IFEQ      WTBEC
219500020320     C     VI4LPD        ORNE      W0030
219600020220     C                   MOVEL     TBEUNI        WCCMN
219700020220   5-C                   ENDIF
219800020220   4-C                   ENDIF
219900020220   3-C                   ENDIF
220000020219   2-C                   ENDIF
220100020219   1-C                   ENDIF
220200120830     c*
220300120830     c* I controlli seguenti li eseguo solo se non è stato richiesto l'annullam.
220400120831     c                   if        *inkf or wconf=*on
220500120831
220600120831     c                   if        wconf=*on
220700120831    1C     WES3N         IFEQ      ' '
220800120831    1C     WES3N_S999    OREQ      'S'
220900120831      * SE NON ESISTE NEPPURE UN RECORD VALIDO DELLA TABELLA
221000120831      * OPPURE ESISTE SOLO IL '999'
221100120831      * SE IL CLIENTE HA TRATTAMENTO DISK 'B' E SE L'AREA DI
221200120831      * APPARTENENZA E' DIVERSA DALLA FILIALE DEL CLIENTE
221300120831      * SEGNALO L'ERRORE
221400120831    2C     §1BFG1        IFEQ      'S'
221500120831     C                   MOVEL     KSC           WFIL              3 0
221600120831     C     WFIL          LOOKUP    pog                                    31
221700120831     C  N31              SETON                                        8490
221800120831     C  N31              GOTO      ENDCCM
221900120831   2-C                   ENDIF
222000120831   1-C                   ENDIF
222100120831     c                   endif
222200020219      *
222300020220      * SE IL P.O DI CONFERMA E' DIVERSO DA QUELLO DEL CODICE CLIENTE
222400020220      * ED ESISTE ALMENO UN RECORD VALIDO SULLA TABELLA 3NN CHE NON
222500020220      * SIA 999 , O P.O DI CONFERNA SEGNALO ERRORE
222600120830    1C***  VI4LPD        IFNE      W0030
222700120830     c                   if        vi4lpd<>w0030 and
222800120830     c                             %lookup(w0030:pog)=0
222900020220    2C     WES3N         IFEQ      'S'
223000020220     C                   MOVE      *BLANKS       TBEKE1
223100020220     C                   MOVEL     KSC           TBEKE1
223200020220     C                   MOVEL     '999'         TBEKE2
223300020220     C     K03TBE        CHAIN     TNTBE02L                           31
223400020220    3C     *IN31         IFEQ      *ON
223500020220     C     TBEATB        ORNE      ' '
223600020220     C                   MOVEL     VI4LPD        TBEKE2
223700020220     C     K03TBE        CHAIN     TNTBE02L                           32
223800020220    4C     *IN32         IFEQ      *ON
223900020220     C     TBEATB        ORNE      ' '
224000020219     C                   SETON                                        6390
224100020219     C                   GOTO      ENDCCM
224200020220    4C                   ENDIF
224300020220    3C                   ENDIF
224400020220    2C                   ENDIF
224500020220    1C                   ENDIF
224600040318     C*
224700040318     C* SE CLIENTE MANTIENE NUMERO DI SPEDIZIONE E SE LA TABELLA 3NN
224800040318     C* NON E' ABILITATA PER IL MIO P.O, E SE IL P.O DI CONFERMA
224900040318     C* E' DIVERSO DA QUELLO DEL CODICE CLIENTE SEGNALO ERRORE.
225000040318     C                   MOVEL     '3C'          COD
225100040318     C                   MOVEL(P)  KSC           KEY
225200040318     C*
225300040318     C     KTAB          CHAIN     TABEL                              31
225400040318     C* SOLO SE ESISTE
225500040318    1C  N31TBLFLG        IFEQ      ' '
225600040318     C                   MOVEL     TBLUNI        DS3C
225700040318    2C     §3CFSP        IFEQ      'S'
225800040318     C                   MOVE      *BLANKS       TBEKE1
225900040318     C                   MOVEL     KSC           TBEKE1
226000040318     C                   MOVEL     VI4LPD        TBEKE2
226100040318     C     K03TBE        CHAIN     TNTBE02L                           31
226200040318    3C     *IN31         IFEQ      *ON
226300040318     C     TBEATB        ORNE      ' '
226400040318    4C     VI4LPD        IFNE      W0030
226500040318     C                   SETON                                        8390
226600040318     C                   GOTO      ENDCCM
226700040318    4C                   ENDIF
226800040318    3C                   ENDIF
226900040318    2C                   ENDIF
227000040318    1C                   ENDIF
227100120830     c                   endif
227200970512     C**
227300970512     C* PRENDO TABELLA CLIENTE CON DISCHETTO
227400000705     C*
227500000717     C* SE PROVENGO DAL CARICAMENTO SCHIERA CLIENTI DALLA TABELLA 3C NON
227600000717     C* RIFACCIO IL CONTROLLO
227700970512     C**
227800000717     C     *IN08         IFEQ      *OFF
227900970512     C                   MOVEL     '3C'          COD
228000970512     C                   MOVEL(P)  WCCMN         KEY
228100970512     C* NON ESISTE NELLA TABELLA 3C QUEL CLIENTE
228200970512     C     KTAB          CHAIN     TABEL                              06
228300970512     C* ANNULLATO
228400990518    1C  N06TBLFLG        IFNE      ' '
228500970512     C                   SETON                                        06
228600990518    1C                   END
228700970512     C*
228800970512     C  N06              MOVEL     TBLUNI        DS3C
228900980805     C* SE NON HA L'* E' COME SE NON CI FOSSE LA 3C
229000990518    1C  N06§3COKD        IFNE      '*'
229100980805     C                   SETON                                        06
229200990518    1C                   ENDIF
229300060306     c* Prendo anche tabella CLI
229400060306     C                   CLEAR                   DSBS02
229500060306     C                   MOVEL     'C'           T02MOD
229600060306     C                   MOVEL     KNSIF         T02SIF
229700060306     C                   MOVEL     'CLI'         T02COD
229800060306     C                   MOVEL(p)  WCCMN         T02KE1
229900060306     C                   CALL      'TIBS02R'
230000060306     C                   PARM                    KPJBA
230100060306     C                   PARM                    DSBS02
230200060306     c
230300060306    4C     T02ERR        IFEQ      *BLANKS
230400060306     C                   MOVEL     T02UNI        Dcli
230500060306     c                   else
230600060306     c                   clear                   dcli
230700060306     c                   endif
230800000717     C*
230900000717     C                   ENDIF
231000941010     C*
231100941010     C     ENDCCM        ENDSR
231200910404     C*
231300900515     C*--- ELABORA BOLLE DA VISUALIZZARE -----------------------------*
231400900515     C     ELAB          BEGSR
231500020917     C*
231600020917     C* PULIZIA SUBFILE
231700020917     C                   EXSR      PULSF
231800020917     C                   Z-ADD     1             REC
231900020917     C                   Z-ADD     0             NRR
232000020927     C                   CLEAR                   WVABNR            1 0
232100020918     C                   SETOFF                                       1101
232200020923     C                   SETOFF                                       303133
232300111220     c* pulizia campi di comodo per controllo serie
232400111220     c                   clear                   preccm
232500111220     c                   clear                   prelnp
232600020917     C*
232700020917     C* 02 OFF  03 OFF - CARICO TUTTO
232800020917    1C     *IN02         IFEQ      *OFF
232900020917    2C     *IN03         IFEQ      *OFF
233000020917      *
233100020917     C* SE RICHIESTA LA SERIE LEGGO PER SERIE E POI SCARTO
233200020917    3C     VI4NRS        IFNE      99
233300020927     C                   EVAL      WVABNR = 6
233400020917     C     VI4NRS        SETLL     FIVAB06L
233500020917     C     VI4NRS        READE     FIVAB06L                               30
233600020917      *
233700020917   X3C                   ELSE
233800020926      *
233900020926      * ELABORO UN P.O ALLA VOLTA.
234000020927     C                   EVAL      *IN31 = *ON
234100020926     C                   Z-ADD     1             Y
234200020917    4C     *IN31         DOWEQ     *ON
234300020917     C     Y             ANDLT     30
234400020926      *   a fine schiera accendo IN31 ed esco dal ciclo
234500020926    6C                   IF        POG(Y) = *ZEROS
234600020926     c                   eval      *in31 = *on
234700020926     C                   leave
234800020926     C                   ELSE
234900020926     C                   move      POG(Y)        $FGS              3 0
235000020926    6C                   ENDIF
235100020926      *
235200021003     C                   EVAL      WVABNR = 2
235300020917     C     $FGS          SETLL     FIVAB02L
235400020917     C     $FGS          READE     FIVAB02L                               31
235500020917     C*
235600020926     C   31              ADD       1             Y
235700020917   4-C                   ENDDO
235800020917     C*
235900020917   3-C                   ENDIF
236000020917     C*
236100020917   X2C                   ELSE
236200020917     C* 02 OFF  03 ON  - CARICO SOLO IL RIFERIMENTO RICHIESTO
236300020927     C                   EVAL      WVABNR = 3
236400020917     C     VI4RMN        SETLL     FIVAB03L
236500020917     C     VI4RMN        READE     FIVAB03L                               30
236600020917    2C                   ENDIF
236700020917     C*
236800020917   X1C                   ELSE
236900020917     C*
237000020917     C* 02 ON   03 OFF - CARICO TUTTE LE BOLLE DEL CLIENTE DIGITATO
237100020917    2C     *IN03         IFEQ      *OFF
237200080623     C* PER CLIENTE rif numerico
237300130121     c                   select
237400130121     c                   when      vi4ord='N'
237500021003     C                   EVAL      WVABNR = 1
237600020917     C     VI4KSC        SETLL     FIVAB01L
237700020917     C     VI4KSC        READE(N)  FIVAB01L                               30
237800130121     c
237900130121     c                   when      vi4ord='S'
238000130121     C                   EVAL      WVABNR = 0
238100130121     C     VI4KSC        SETLL     FIVAB10L
238200130121     C     VI4KSC        READE(N)  FIVAB10L                               30
238300130121     C*
238400130121     c                   other
238500080623     C* PER CLIENTE rif alfanumerico
238600080623     C                   EVAL      WVABNR = 9
238700080623     C     VI4KSC        SETLL     FIVAB09L
238800080623     C     VI4KSC        READE(N)  FIVAB09L                               30
238900130121     c                   endsl
239000020917     C*
239100020917   X2C                   ELSE
239200020917     C* 02 ON   03 ON  - CARICO SOLO IL CLIENTE E RIFERIMENTO DIGIATATI
239300021003     C                   EVAL      WVABNR = 1
239400020917     C     KVAB          SETLL     FIVAB01L
239500050407     C     KVAB          READE(N)  FIVAB01L                               30
239600020917     C*
239700020917    2C                   ENDIF
239800020917    1C                   ENDIF
239900020917     C*
240000020917    1C     *IN30         DOWEQ     *OFF
240100020917     C     *IN31         ANDEQ     *OFF
240200020917     C*
240300020926     C* SCARTO I RECORD NON GESTITI
240400020926     C* TRANNE SE CCM=0 e RMN=0 e NRS=99
240500020926     C* PERCHE' IN QUESTO CASO STO GIA' LEGGENDO PER P.O.
240600020923     C                   SETOFF                                       33
240700020926    2C                   IF        *IN02 = *ON or *in03 = *ON
240800020926     C                             or  VI4NRS <> 99
240900020926     C     VABFGS        LOOKUP    POG                                    33
241000020918   X2C                   ELSE
241100020923     C                   MOVE      *on           *in33
241200020917   2-C                   ENDIF
241201170901     c* Se record annullato scarto il record
241202170901     C                   if        vabatb='A'
241203170901     C                   setoff                                       33
241204170901     C                   ENDIF
241300020917     C* PROSEGUO SE LA LETTURA E' VALIDA ..
241400020923    2C     *IN33         IFEQ      *ON
241500020917     C*
241600020917     C* CARICO SOLO 9900 RECORD
241700020917     C* SE RICHIESTA LA SERIE SELEZIONO SOLO LE BOLLE CON QUELLA SERIE
241800020917    3C     VI4NRS        IFEQ      99
241900020917     C     VI4NRS        OREQ      VABNRS
242000041123   3aC     VI4lna        IFEQ      999
242100041123     C     VI4lna        OREQ      VABlna
242200020917     C*
242300020917     C* SE CLIENTE NON SELEZIONATO IN PRIMA VIDEATA E NON SONO UN P.O. POSTE
242400020917     C* NON CARICO CLIENTI POSTE IN SUBFILE
242500020917     C*
242600020917    4C     *IN02         IFEQ      *OFF
242700020917     C     WRKPT         ANDNE     'S'
242800020917     C*
242900020917     C                   MOVEL     VABCCM        WORG
243000020917     C* CONTROLLO A CHI APPARTIENE IL CLIENTE
243100020917     C                   EXSR      CTRORG
243200020917     C* SE CLIENTE POSTA PASSO AL RECORD SUCCESSIVO
243300020917    5C     §OGNTW        IFEQ      'PPT'
243400020917     C                   GOTO      NEXT
243500020917   5-C                   ENDIF
243600020917   4-C                   ENDIF
243700020917     C* SE CLIENTE NON SELEZIONATO IN PRIMA VIDEATA E SONO UN P.O. POSTE
243800020917     C* CARICO CLIENTI POSTE IN SUBFILE
243900020917    4C     *IN02         IFEQ      *OFF
244000020917     C     WRKPT         ANDEQ     'S'
244100020917     C*
244200020917     C                   MOVEL     VABCCM        WORG
244300020917     C* CONTROLLO A CHI APPARTIENE IL CLIENTE
244400020917     C                   EXSR      CTRORG
244500020917     C* SE CLIENTE POSTA PASSO AL RECORD SUCCESSIVO
244600020917    5C     §OGNTW        IFNE      'PPT'
244700020917     C                   GOTO      NEXT
244800020917   5-C                   ENDIF
244900020917   4-C                   ENDIF
245000130121     c
245100111215     c* Se effettuata selezione per linea di partenza segnacollo
245200111215     c* scarto il record se lnp diversa
245300111215     c                   if        vi4lnp=0 or  vi4lnp=vablnp
245400111215     c* se specificata la serie e non il codice cliente richiamo
245500111215     c* routine per controllare se è possibile proseguire con il
245600111215     c* caricamento del subfile o se deve essere richiesta l'immissione
245700111215     c* del codice cliente
245800111215     c                   clear                   wctrs
245900111219    4c                   if        vi4nrs<>99 and vi4nrs>00 and vi4ksc=0
246000111215     c                   exsr      ctr_serie
246100111215    4c                   endif
246200111215     c                   if        wctrs=*blanks
246300020917     C*
246400020917     C     NRR           COMP      9900                               30  30
246500020917     C  N30              EXSR      EMISS
246600111215     c                   else
246700111216     c                   seton                                        302888
246800111220     c                   if        vi4lnp=0
246900111216     c                   movel     err(30)       vidmsg
247000111220     c                   else
247100111220     c                   movel     err(31)       vidmsg
247200111220     c                   endif
247300111215     c                   endif
247400111215     c                   endif
247500041123   3aC                   ENDIF
247600041123   3-C                   ENDIF
247700020917   2-C                   ENDIF
247800020917     C*
247900020917     C* RECORD SUCCESSIVO
248000020917     C     NEXT          TAG
248100020917     C* SENZA CLIENTE
248200020917    2C  N30*IN02         IFEQ      *OFF
248300020917    3C     *IN03         IFEQ      *OFF
248400020917      *
248500020917     C* SE RICHIESTA LA SERIE LEGGO PER SERIE E POI SCARTO
248600020917    4C     VI4NRS        IFNE      99
248700020917     C     VI4NRS        READE     FIVAB06L                               30
248800020917     C*
248900020917   X4C                   ELSE
249000020926      *
249100020926      * ELABORO UM P.O ALLA VOLTA.
249200020918     C     $FGS          READE     FIVAB02L                               31
249300020917    5C     *IN31         DOWEQ     *ON
249400020917     C     Y             ANDLT     30
249500020917     C                   ADD       1             Y
249600020926      *   a fine schiera accendo IN31 ed esco dal ciclo
249700020926    6C                   IF        POG(Y) = *ZEROS
249800020926     c                   eval      *in31 = *on
249900020926     C                   leave
250000020926     C                   ELSE
250100020926     C                   move      POG(Y)        $FGS
250200020926    6C                   ENDIF
250300020926      *
250400020918     C     $FGS          SETLL     FIVAB02L                               31
250500020918     C     $FGS          READE     FIVAB02L                               31
250600020917   5-C                   ENDDO
250700020917   4-C                   ENDIF
250800020917      *
250900020917   X3C                   ELSE
251000020917     C     VI4RMN        READE     FIVAB03L                               30
251100020917    3C                   ENDIF
251200020917     C* CON CLIENTE
251300020917   X2C                   ELSE
251400130121   c
251500020917    3C     *IN03         IFEQ      *OFF
251600130121     c                   select
251700130121     c                   when      vi4ord='N'
251800020917     C     VI4KSC        READE(N)  FIVAB01L                               30
251900130121     c
252000130121     c                   when      vi4ord='S'
252100130121     C     VI4KSC        READE(N)  FIVAB10L                               30
252200130121     c
252300130121     c                   other
252400080623     C     VI4KSC        READE(N)  FIVAB09L                               30
252500130121     c                   endsl
252600080623     c
252700020917   X3C                   ELSE
252800020917     C* 02 ON   03 ON  - CARICO SOLO IL CLIENTE E RIFERIMENTO DIGIATATI
252900050407     C     KVAB          READE(N)  FIVAB01L                               30
253000020917    3C                   ENDIF
253100020917    2C                   ENDIF
253200020917    1C                   ENDDO
253300020917     C*
253400020917     C                   Z-ADD     NRR           SAVNRR            4 0
253500020917     C*
253600020917     C* SE IL CLIENTE HA PIU DI 9900 BOLLE EMETTO UN MESSAGGIO
253700020917IF  1C     NRR           IFGE      9900
253800020917     C                   SETON                                        01
253900020917E   1C                   ENDIF
254000930609     C*
254100930909     C                   ENDSR
254200111215     C* CONTROLLO per serie parziale ---------------------------------*
254300111215     C     CTR_serie     BEGSR
254400111215    1c                   if        vabccm<>preccm
254500111215     c* Presente più di un cliente con la stessa serie
254600111215    2c                   if        preccm>0
254700111215     c* . . . stessa linea di partenza
254800111215     c*       errore solo se si tratta di serie parziale
254900111215    3c                   if        vablnp=prelnp
255000111216     c                   clear                   trulnrsds
255100111219     c                   movel     vabccm        inrsCCM
255200111219     c                   movel     vablnp        inrsFLS
255300111216     c                   movel     vabnrs        inrsNRS
255400111216     c                   call      'TRULNRSR'
255500111216     c                   parm                    trulnrsds
255600111216     c                   if        onrsprz='P'
255700111216     c                   eval      wctrs='E'
255800111216     c                   endif
255900111215     c
256000111215   x3c                   else
256100111216     c* . . . diversa linea di partenza: sempre errore
256200111216     c                   eval      wctrs='E'
256300111215    3c                   endif
256400111215   x2c                   else
256500111215     c                   eval      preccm=vabccm
256600111215     c                   eval      prelnp=vablnp
256700111215    2c                   endif
256800111215    1c                   endif
256900111215     c
257000111215     c                   endsr
257100930909     C*
257200930909     C* CONTROLLO SE SI PUO' CONFERMARE LA BOLLA ---------------------*
257300941010     C     CTRD02        BEGSR
257400930909     C                   SETOFF                                       90
257500930909     C*
257600930909     C* SE BOLLA NORMALE DEVE ESSERCI COD PADRONCINO E DATA RITIRO
257700010220     C* E NUMERO ORM SE ATTIVATA PROCEDURA ORM
257800010220    2C     WOBBL         IFEQ      *BLANKS
257900010220    3C     VI4PDR        IFEQ      *BLANKS
258000941010     C     VI4PDR        OREQ      *ZEROS
258100000515     C     VI4DRT        OREQ      *ZEROS
258200010220     C                   SETON                                        513690
258300930909     C                   Z-ADD     NRR           REC
258400950112     C  NKF              UPDATE    LS66DSF
258500930909     C                   SETOFF                                       36
258600010220    3C                   ENDIF
258700010220   X2C                   ELSE
258800010220    3C     VIDORM        IFEQ      *ZEROS
258900010220     C     VI4PDR        OREQ      *BLANKS
259000010220     C     VI4PDR        OREQ      *ZEROS
259100010220     C     VI4DRT        OREQ      *ZEROS
259200010220     C                   SETON                                        803690
259300010220     C                   Z-ADD     NRR           REC
259400010220     C  NKF              UPDATE    LS66DSF
259500010220     C                   SETOFF                                       36
259600010220    3C                   ENDIF
259700010220    2C                   ENDIF
259800930909     C*
259900930909     C                   ENDSR
260000900515     C*
260100900515     C* EMISSIONE SUBFILE -------------------------------------------
260200900515     C     EMISS         BEGSR
260300970513     C**
260400900808     C* SALVO UNA VOLTA LA LINEA PARTENZA PER IL VIDEO
260500950321     C                   SETOFF                                       1007
260600900515     C                   MOVEL     *BLANKS       VI5SCE
260700941010     C                   MOVEL     VABLNA        VI5LNA
260800941010     C                   MOVEL     VABLNP        VI5LNP
260900941010     C                   MOVEL     VABAAS        VI5AAS
261000941010     C                   MOVEL     VABMGS        MMGG
261100900516     C                   MOVEL     GG            VI5DSP
261200900516     C                   MOVE      MM            VI5DSP
261300941010     C                   MOVE      VABCCM        VI5KSC
261400020917     C                   MOVE      VABFGS        VI5FGS
261500900827     C                   MOVEL     VI5KSC        KSC
261600970512     C**
261700970512     C* SE NON IMMESSO COD CLIENTE LO CERCO
261800970512    0C     *IN02         IFEQ      *OFF
261900020219     C                   MOVEL     VI5KSC        WCCMN
262000970512     C                   CLEAR                   WES3N
262100970512     C*
262200941011     C* DECODIFICA RAGIONE SOCIALE
262300970512     C     KACO          CHAIN     CNACO000                           33
262400970512    1C     *IN33         IFEQ      *OFF
262500970512     C     ACOFLG        ANDEQ     ' '
262600970512     C                   MOVEL     ACORAG        VI5RAG
262700970512     C                   ELSE
262800970512     C* SE NON C'E' LO CARICO MA PROTETTO SENZA POSSIBILITA' DI CONFERM
262900970512     C                   SETON                                        10
263000970512     C                   MOVEL     'C'           VI5SCE
263100970512     C                   CLEAR                   VI5RAG
263200960319    1C                   ENDIF
263300970512     C**
263400970512   X0C                   ELSE
263500970512     C**
263600970512     C* WCCMN E WES3N SONO GIA' IMPOSTATI DALL 1 VIDEATA
263700970512     C                   MOVEL     ACORAG        VI5RAG
263800970512    0C                   ENDIF
263900020927      * IMPOSTO NUMERO RELATIVO RECORD
264000020927     C                   SELECT
264100020927     C                   WHEN      WVABNR = 1
264200020927     C                   Z-ADD     VABNR1        VI5NRR
264300020927     C                   WHEN      WVABNR = 2
264400020927     C                   Z-ADD     VABNR2        VI5NRR
264500020927     C                   WHEN      WVABNR = 3
264600020927     C                   Z-ADD     VABNR3        VI5NRR
264700020927     C                   WHEN      WVABNR = 6
264800020927     C                   Z-ADD     VABNR6        VI5NRR
264900080623     C                   WHEN      WVABNR = 9
265000080623     C                   Z-ADD     VABNR9        VI5NRR
265100130121     C                   WHEN      WVABNR = 0
265200130121     C                   Z-ADD     VABNR10       VI5NRR
265300020927     C                   ENDSL
265400020927     C*
265500020927     C                   Z-ADD     VABNSP        VI5NSP
265600080526     c                   Eval      wtxt = VabRsd
265700080526     c                   ExSr      Sr_Chk
265800080526     c                   Movel     wtxt          vi5rsd
265900080526     C***                MOVEL     VABRSD        VI5RSD
266000121126     c*** visulizzo in DROP i riferimenti  mittente
266100121126     C                   MOVEL     VABRMN        VI5_rmn
266200121126     C                   MOVEL     VABRMa        VI5_rma
266300121126     c
266400130121     c                   if        vi4ord<>'A'
266500121126     C                   MOVEL     VABRMN        VI5RMN
266600080623     c                   z-add     vabrmn        w0090             9 0
266700080623     c                   eval      vi5rm2=%editc(w0090:'Z')
266800080623     c                   else
266900080623     c                   movel     vabrma        vi5rmn
267000080623     c                   movel     vabrma        vi5rm2
267100080623     c                   endif
267200080623     C******             MOVE      VABRMN        VI5RM2
267300941010     C                   MOVEL     VABNRS        VI5NRS
267400941010     C                   MOVEL     VABNCD        VI5SCD
267500941010     C                   MOVEL     VABNCA        VI5SCA
267600941010     C                   MOVEL     VABTSP        VI5TSP
267700920325     C     VI4CTM        IFNE      '  '
267800920325     C                   MOVEL     VI4CTM        VI5CTM
267900920325     C                   ELSE
268000941010     C                   MOVEL     VABCTM        VI5CTM
268100920325     C                   END
268200950407     C                   MOVEL     VABCTM        VH5CTM
268300000515     C* SE NON E' STATA RICHIESTA LA CONFERMA TOTALE DALLA PRIMA VIDEATA
268400000515     C* SCRIVO IL SUBFILE
268500000515     C     TOTA          IFNE      'S'
268600900518     C                   ADD       1             NRR               4 0
268700941007     C                   WRITE     LS66DSF
268800000515     C                   ENDIF
268900000515     C*
269000020219     C     ENDEMI        ENDSR
269100900515     C*
269200900515     C* PULIZIA SUBFILE ---------------------------------------------
269300900515     C     PULSF         BEGSR
269400900515     C                   SETON                                        35
269500941007     C                   WRITE     LS66DCT
269600900515     C                   SETOFF                                       35
269700900515     C                   ENDSR
269800900808     C*
269900900808     C*--- CONFERMA ANNULLAMENTO BOLLE INDICATE CON LA 'A' -----------*
270000900808     C     ANNULL        BEGSR
270100941010     C*
270200941007     C                   READC     LS66DSF                                35
270300941010     C*
270400941010    1C     *IN35         DOWEQ     *OFF
270500900808     C* ANNULLATO
270600941010    2C     VI5SCE        IFEQ      'A'
270700080623     c
270800020917     C* CANCELLO FIVAB
270900080623     C**** KVAB2         CHAIN     FIVAB02L                           32
271000080623     c     vi5nrr        chain     fivab0ff                           3290
271100970821     C     *IN32         IFEQ      *OFF
271200080623     C     *IN90         andeq     *OFF
271300060130      * Prima di annullare la spedizione se bolla RC vado ad annullare
271400060130      * il relativo ORM legato
271500111010     c                   exsr      annorm
271600000512     C* ANNULLO I RECORDS
271700080623     C                   movel     vi5nrr        WNRR              9 0
271800000512     C                   EXSR      ANNREC
271900000512     C*
272000000512     C                   ENDIF
272100910222     C*
272200941010    2C                   ENDIF
272300900808     C*
272400941010     C                   READC     LS66DSF                                35
272500941010     C                   ENDDO
272600900808     C*
272700900808     C                   ENDSR
272800900808     C*
272900900808     C*--- CONFERMA BOLLA VADO NEL DETTAGLIO -------------------------*
273000900808     C     CONF          BEGSR
273100911031     C                   SETOFF                                       90
273200950407     C                   CLEAR                   WBOL
273300120831
273400120831     C* CONTROLLO ESISTENZA CODICE TRATTAMENTO MERCE
273500120831    1C     VI5CTM        IFNE      *BLANKS
273600120831     C* SOLO SE DIVERSO
273700120831    2C     VI5CTM        IFNE      SAVCTM
273800120831     C                   MOVEL     '1B'          COD
273900120831     C                   MOVEL(P)  VI5CTM        KEY
274000120831     C     KTAB          CHAIN     TABEL                              33
274100120831     C* ANNULLATO
274200120831    3C  N33TBLFLG        IFNE      ' '
274300120831     C                   SETON                                        33
274400120831    3C                   END
274500120831     C*
274600120831     C   33              SETON                                        5090
274700120831     C   33              GOTO      ENDCON
274800120831     C*
274900120831     C                   MOVEL     TBLUNI        DS1B
275000120831     C                   MOVEL     VI5CTM        SAVCTM
275100120831    2C                   ENDIF
275200120831     C*
275300120831     C* SE NON C'E' IL CTM AZZERO LA DS E IL CAMPO DI SALVATAGGIO
275400120831   X1C                   ELSE
275500120831     C                   CLEAR                   DS1B
275600120831     C                   CLEAR                   SAVCTM
275700120831    1C                   ENDIF
275800950407     C*
275900941010     C* 02 OFF - NON IMMESSO CODICE CLIENTE IN 1 VIDEATA
276000120831    1C***  *IN02         IFEQ      *OFF
276100941010     C                   MOVEL     VI5KSC        KSC
276200120831     c                   eval      wconf=*on
276300941010     C                   EXSR      CTRCCM
276400120831     c                   eval      wconf=*off
276500941010     C* 90 ON - ERRORI
276600941010     C   90              GOTO      ENDCON
276700120831     C***                ENDIF
276800950407     C*
276900950407     C* SE CAMBIATO IL CODICE CLIENTE PULISCO IL CAMPO DEL CONTROLLO
277000950407     C*  DELLA SERIE
277100950407     C     VI5KSC        IFNE      WKSC
277200950407     C                   CLEAR                   WUNO
277300950407     C                   MOVEL     VI5KSC        WKSC
277400950407     C                   ENDIF
277500120710     c* se è stato forzato un codice trattamento merce no sol."A" e no
277600120710     c* autospunta che non prevede il FIVAX verifico che per la bolla
277700120710     c* selezionata sia utilizzabile
277800120710    0c                   if        wctrctm='1'
277900120710     c                   clear                   x
278000120710     c                   if        vh5ctm<>*blanks
278100120710     c                   eval      x=%lookup(vh5ctm:ctm)
278200120710     c                   endif
278300120710    1c                   select
278400120710     c* bolla con ctm che non prevede il packing list
278500120710     c                   when      x>0 and ctm16(x)<>'S'
278600120710     C                   SETON                                        2590
278700120710     C                   GOTO      ENDCON
278800120710     c* bolla con cod trattamento merce incongruente con quello forzato
278900120710     c                   when      x=0 or ctmf(x)<>w1b
279000120710     C                   SETON                                        6590
279100120710     C                   GOTO      ENDCON
279200120711     c                   other
279300130314     c* Errore se:
279400130314     c* 1) la bolla ha il VAX e lui solo (no vat record "P")
279500130314     c* 2) la bolla ha il VAT record "P" e il cliente è abilitato
279600130314     c* in questi casi non gli si deve dare la possibilità di forzare il ctm
279700120711     C     KBLD2         SETLL     FIVAX000
279800130313     C                   MOVEL     'P'           WTRC
279900130313     C     Kvat2         SETLL     FIVAt01l
280000130314     c                   if        (%equal(fivax01l) and not %equal(fivat01l))
280100130314     c                             or (%equal(fivat01l) and §clipdfp<>*blank)
280200120711     C                   SETON                                        1590
280300120711     C                   GOTO      ENDCON
280400120711     c                   endif
280500120710    1c                   endsl
280600120710   x0c                   else
280700950407     C*
280800950407     C* SE C'E' LA SERIE NON POSSO FORZARE COD TRATTAM.MERCE
280900950407     C*  --> ERRORE PERO' FORZABILE
281000950504    1C     VH5CTM        IFNE      '  '
281100950407     C     VI4CTM        ANDNE     '  '
281200950407     C     WUNO          ANDEQ     ' '
281300950504     C     VI5NRS        ANDNE     0
281400950407     C*
281500950407     C     VH5CTM        LOOKUP    CTM                                    33
281600950504    2C     *IN33         IFEQ      *ON
281700950407     C                   SETON                                        5590
281800950407     C                   MOVEL     '1'           WUNO              1
281900950407     C                   GOTO      ENDCON
282000950504    2C                   ENDIF
282100950407    1C                   ENDIF
282200120710    0c                   endif
282300941010     C*
282400941010     C* SE C'E' LA SERIE CTM DI BOLLA DEVE ESSERCI
282500941010    1C     VI5NRS        IFNE      0
282600941010     C     VI5CTM        ANDEQ     '  '
282700941010     C                   SETON                                        4190
282800941010     C                   GOTO      ENDCON
282900941010    1C                   ENDIF
283000120831     c* Il prossimo controlll lo sposto all'inizio della routine
283100120831     c* perchè mi serve la ds1b valorizzata per la sbr CTRCCM
283200930909     C*
283300900828     C* CONTROLLO ESISTENZA CODICE TRATTAMENTO MERCE
283400120831    1C***  VI5CTM        IFNE      *BLANKS
283500930909     C* SOLO SE DIVERSO
283600120831    2C***  VI5CTM        IFNE      SAVCTM
283700120831     C***                MOVEL     '1B'          COD
283800120831     C***                MOVEL(P)  VI5CTM        KEY
283900120831     C***  KTAB          CHAIN     TABEL                              33
284000910226     C* ANNULLATO
284100120831    3C**N33TBLFLG        IFNE      ' '
284200120831     C**                 SETON                                        33
284300120831    3C**                 END
284400941010     C*
284500120831     C** 33              SETON                                        5090
284600120831     C** 33              GOTO      ENDCON
284700900828     C*
284800120831     C**                 MOVEL     TBLUNI        DS1B
284900120831     C**                 MOVEL     VI5CTM        SAVCTM
285000120831    2C**                 ENDIF
285100950504     C*
285200950504     C* SE NON C'E' IL CTM AZZERO LA DS E IL CAMPO DI SALVATAGGIO
285300120831   X1C**                 ELSE
285400120831     C**                 CLEAR                   DS1B
285500120831     C**                 CLEAR                   SAVCTM
285600120831    1C**                 ENDIF
285700060306     c
285800060306     c* Se cliente con obbligo parzializzazione lna errore se non immes
285900060306     c                   if        §clilna='S' and vi4lna=999
286000060306     C                   SETON                                        3990
286100060306     C                   GOTO      ENDCON
286200060306     c                   endif
286300060306     C*
286400900808     C*
286500900828     C* CONTROLLO SE IL CLIENTE SI ASSEGNA IL NUMERO DI SPEDIZIONE
286600941010    1C     §1BFG2        IFEQ      'S'
286700941010     C* NON TROVATO RECORD DEL CLIENTE NELLA DS3C: ERRORE
286800920410     C   06              SETON                                        4690
286900920410     C   06              GOTO      ENDCON
287000900808     C*
287100941010     C* CONTROLLO SERIE SE NON CORRISPONDE A QUELLA ASSEGNATA AL CLIENT
287200020221     C*  SE LA LINEA DI PARTENZA NON E'ESTERA
287300100517     C                   MOVEL     VI5LNP        WORG              3 0
287400100517     C                   EXSR      CTRORG
287500100517     C**   §OGNTW        IFNE      'EEX'
287600100517     C**   §OGNTW        ANDNE     'EUP'
287700990603     C*
287800100517     C**   §3CNRS        IFNE      VI5NRS
287900100517     C**                 SETON                                        4790
288000100517     C**                 GOTO      ENDCON
288100100517     C**                 ENDIF
288200100517     C**                 ENDIF
288300000515     C*
288400100517     c                   clear                   trulnrsds
288500100517     c                   movel     vi5ksc        inrsCCM
288600100517     c                   movel     vi5nrs        inrsNRS
288700100517     c
288800100517     c                   call      'TRULNRSR'
288900100517     c                   parm                    trulnrsds
289000100517     c                   if        onrserr<>' '
289100100517     C                   SETON                                        4790
289200100517     C                   GOTO      ENDCON
289300100517     C                   endif
289400941010     C*
289500900808     C* CONTROLLO CHE IL NUMERO DI SPEDIZIONE NON SIA DOPPIO
289600920424     C* SE NON LO RIASSEGNO
289700941010    2C     §3CFSP        IFEQ      'S'
289800950220    1C     §1BFG3        OREQ      'S'
289900080204     c* Controllo prima che anno del vab=anno corrente altrimenti errore
290000080204     c                   if        vi5aas<>comaas
290100080204     C                   SETON                                        3890
290200080204     C                   GOTO      ENDCON
290300080204     c                   endif
290400941010     C     KBLP          SETLL     FNBLP01L                               33
290500921005     C   33              SETON                                        4890
290600921005     C   33              GOTO      ENDCON
290700941010    2C                   ENDIF
290800921005     C*
290900910311     C* SE FLAG6 "C" E NON C'E NUMERO DI SPEDIZIONE
291000910311     C* E NON POSSO BOLLETTARE CON LA SERIE DEL CLIENTE
291100941010    2C     VI5NSP        IFEQ      0
291200941010    3C     §3CFG1        IFEQ      'N'
291300910311     C                   SETON                                        5990
291400910311     C                   GOTO      ENDCON
291500941010    3C                   ENDIF
291600910311     C* SE STAMPA ANCHE LA BOLLA ERRORE VINCOLANTE
291700941010    3C     §1BFG3        IFEQ      'S'
291800910311     C                   SETON                                        5990
291900910311     C                   GOTO      ENDCON
292000941010    3C                   ENDIF
292100941010    2C                   END
292200941010     C*
292300941010    1C                   END
292400941010     C*
292500020910     C* SE FLAG7= S DEVE ESISTERE FILE FIVAD
292600991213    1C     §1BFG7        IFEQ      'S'
292700020910     C     KBLD2         SETLL     FIVAD000                               33
292800941010     C  N33              SETON                                        6090
292900941010     C  N33              GOTO      ENDCON
293000991213    1C                   ENDIF
293100130313     C* SE FLAG16= S DEVE ESISTERE FILE FIVAX o FIVAT record "P"
293200040604    1C     §1BF16        IFEQ      'S'
293300130313     C*    KBLD2         SETLL     FIVAX000                               33
293400130313     C* N33              SETON                                        2790
293500130313     C* N33              GOTO      ENDCON
293600130313     C     KBLD2         SETLL     FIVAX000
293700130313     C                   MOVEL     'P'           WTRC
293800130313     C     Kvat2         SETLL     FIVAt000
293900130313     c                   if        not %equal(fivax01l) and not %equal(fivat01l)
294000130313     c                   seton                                        2790
294100130313     c                   goto      endcon
294200130313     c                   endif
294300130314     c* cliente non abilitato al PDF
294400130314     c                   if        %equal(fivat01l) and §clipdfp=*blanks
294500130314     C                   SETON                                        2690
294600130314     C                   GOTO      ENDcon
294700130314     c                   endif
294800040604    1C                   ENDIF
294900900808     C*
295000941010     C* CANCELLO VAB
295100991213     C                   CLEAR                   §LPFPR
295200000515     C* SE VENGO DA CONFERMA TOTALE 1° VIDEATA NON AGGANCIO IL VAB
295300000515     C     TOTA          IFNE      'S'
295400020917     C     VI5NRR        CHAIN     FIVAB0FF                           3290
295500000515     C                   ELSE
295600000515     C                   SETOFF                                       3290
295700000515     C                   ENDIF
295800000523     C* SE STO CONFERMANO BOLLE POSTE VERIFICO SE ESISTE GIA' UNA BOLLA
295900000523     C* GIA' CREATA
296000060221     C                   CLEAR                   TROVA             1
296100060221     C                   CLEAR                   WDOPP             1
296200060221     C***  WRKPT         IFEQ      'S'
296300060221     C***                EXSR      CERBLP
296400060221     C***                ENDIF
296500000526     C* SE E' STATO TROVATO IL RECORD IN BLP ED E' UN AUTOGENERATO LEGGO IL
296600000526     C* SUCCESSIVO DOPO AVER DELETATO IL RECORD
296700000526     C     TROVA         IFEQ      'S'
296800000627     C                   GOTO      DOPO01
296900000526     C                   ENDIF
297000950407     C*
297100950407     C* 90 ON - NON RIUSCITA ALLOCAZIONE DEL RECORD
297200991213    1C     *IN90         IFEQ      *ON
297300950407     C                   SETON                                        66
297400950407     C                   GOTO      ENDCON
297500991213   X1C                   ELSE
297600911114     C*
297700991210     C*  SOLO SE NEL VAB C'E' ANCORA LA BOLLA DA CONFERMARE,
297800991210     C*  ADO AVANTI: POTREBBE AVERLA GIA' CONFERMATA UN ALTRO
297900991213    2C     *IN32         IFEQ      *OFF
298000001006     C* SE AS400 E CODICE CLIENTE CON DISK C, SE NON E' STATO FORZATO
298100020917     C* UN CODICE TRATTAMENTO MERCE E SE IL CTM DI FIVAB E' DISK B
298200001006     C* PASSO IL CTM SUO "FRATELLINO" AL PGM DI BOLLETTAZIONE.
298300001006     C* SE QUEST'ULTIMO MANCA --> ERRORE
298400001006     C                   CLEAR                   WWCTC
298500001006     C     WASDKC        IFEQ      'S'
298600001006     C     §3CDKC        ANDEQ     'S'
298700001006     C     VI4CTM        ANDEQ     '  '
298800001006     C     §1BFG1        ANDEQ     'S'
298900001006     C     §1BCTC        IFEQ      '  '
299000001006     C                   SETON                                        5790
299100001006     C                   GOTO      ENDCON
299200001006     C                   ELSE
299300001006     C                   MOVE      §1BCTC        WWCTC
299400001006     C                   ENDIF
299500001006     C                   ENDIF
299600911031     C                   EXSR      CRTDS
299700950112     C*
299800950112     C                   CALL      'FNLS01R'
299900950112     C                   PARM                    KPJBA
300000991026     C                   PARM                    DBLP
300100991026     C                   PARM                    DTASV
300200030611     C                   PARM                    trul90ds
300300030611      *
300400930628     C                   MOVEL     '1'           RICH              1
300500991213    2C                   ENDIF
300600000627     C*
300700000627     C     DOPO01        TAG
300800900808     C*
300900911031     C* SE FLAG=4 CMD12 NON CONFERMATA
301000991213    2C     §LPFPR        IFNE      '4'
301100000517     C* SE FLAG=3 CMD03 NON CONFERMATA
301200000517     C     §LPFPR        ANDNE     '3'
301300950123     C* SE E' = A 5 ERRORE
301400991213    3C     §LPFPR        IFEQ      '5'
301500000516     C* NON VENGO DA SELEZIONE TOTALE
301600000516     C     TOTA          IFNE      'S'
301700020917     C  N32              UNLOCK    FIVAB00F
301800000516     C                   ELSE
301900020917     C  N32              UNLOCK    FIVAB01L
302000000516     C                   ENDIF
302100950123     C                   SETON                                        90
302200991213   X3C                   ELSE
302300000526     C*
302400111010     C     *IN32         IFEQ      *OFF
302500111010     c* se flag ='2' --> cancellare dal vab e suoi collegati
302600111010     c                   if        §lpfpr='2'
302700111010     c                   exsr      ges_ann
302800111010     c                   else
302900910404     C                   Z-ADD     VI5KSC        PREKSC
303000080623     C***                Z-ADD     VI5RMN        PRERMN
303100130121     c                   if        vi4ord<>'A'
303200080623     c                   movel     vi5rmn        w0150            15 0
303300080623     c                   eval      prermn=%editc(w0150:'Z')
303400080623     c                   else
303500080623     c                   movel     vi5rmn        prermn
303600080623     c                   endif
303700000516     C* NON VENGO DA SELEZIONE TOTALE
303800000516     C     TOTA          IFNE      'S'
303900020917     C                   DELETE    FIVAB0FF
304000000516     C                   ELSE
304100130121     c                   select
304200130121     c                   when      vi4ord='N'
304300020917     C                   DELETE    FIVAB000
304400130121     c                   when      vi4ord='S'
304500130121     C                   DELETE    FIVAB10
304600130121     c                   other
304700080623     C                   DELETE    FIVAB09
304800130121     c                   endsl
304900080623     c
305000000516     C                   ENDIF
305100111010     c                   endif
305200111010     C                   ENDIF
305300911031     C*
305400000515     C* PROTEGGO IL CAMPO DI SCELTA SE NON VENGO DA CONFERMA TOTALE 1° VIDEATA
305500000515    4C   05TOTA          IFNE      'S'
305600900808     C                   SETON                                        10
305700950321     C                   MOVEL     'C'           VI5SCE
305800950321     C* SE F6 NON VISUALIZZO TUTTA LA BOLLA
305900991213    5C     WF6           IFEQ      '1'
306000950321     C                   SETON                                        07
306100950321     C                   ELSE
306200950321     C                   SETOFF                                       07
306300991213    5C                   ENDIF
306400950321     C*
306500941007     C                   UPDATE    LS66DSF
306600900808     C                   SETOFF                                       10
306700991213    4C                   END
306800991213    3C                   END
306900950321     C*
307000991213   X2C                   ELSE
307100020917     C  N32              UNLOCK    FIVAB00F
307200991213    2C                   END
307300950407    1C                   END
307400900828     C*
307500900828     C     ENDCON        TAG
307600900828     C* POSIZIONAMENTO SU RECORD SBAGLIATO
307700950123     C   05
307800950123    1CAN 90              DO
307900911031     C                   MOVEL     'E'           VI5SCE
308000950321     C                   SETOFF                                       07
308100950321     C                   SETON                                        113612
308200941007     C                   UPDATE    LS66DSF
308300900828     C                   SETOFF                                       1136
308400900828     C                   Z-ADD     NRR           REC
308500950123    1C                   END
308600000515     C* PER 90 E SELEZIONE TOTALE SCRIVO IL RECORD ERRATA NEL SUBFILE
308700000515     C   90TOTA          IFEQ      'S'
308800000515     C                   ADD       1             NRR
308900000516     C                   MOVEL     'E'           VI5SCE
309000000516     C                   SETOFF                                       07
309100000516     C                   SETON                                        113612
309200000515     C                   WRITE     LS66DSF
309300000516     C                   SETOFF                                       1136
309400000516     C                   Z-ADD     NRR           REC
309500000515     C                   ENDIF
309600000515     C*
309700911031     C                   ENDSR
309800900808     C*
309900900808     C*--- SELEZIONE TOTALE BOLLE IN SUBFILE -------------------------*
310000900808     C     SELEZ         BEGSR
310100900824     C*
310200900824     C                   Z-ADD     0             B                 5 0
310300950321     C                   MOVEL     '1'           WF6               1
310400950321     C*
310500900828     C     B             DOWLT     SAVNRR
310600900824     C                   ADD       1             B
310700941007     C     B             CHAIN     LS66DSF                            31
310800950321     C*
310900950321     C* SE L'HO GIA' CONFERMATA NON LA RILEGGO
311000950321     C  N31VI5SCE        IFNE      'C'
311100950419     C                   MOVEL     'W'           VI5SCE
311200950321     C                   EXSR      CONF
311300000517     C* SE DATO IN FNLS01R IL KC VADO A FIE LETTURA
311400000517     C     §LPFPR        IFEQ      '3'
311500000517     C                   LEAVE
311600000517     C                   ENDIF
311700000517     C*
311800950321     C                   ENDIF
311900911031     C                   END
312000900808     C*
312100900808     C                   ENDSR
312200000512     C*-------------------------------------------------------------*
312300000512     C* -- ROUTINE DI LETTURA TOTALE BOLLE CLIENTE PER CONFERMA --  *
312400000512     C* -- O ANNULLAMENTO COMPLETO                              --  *
312500000512     C*-------------------------------------------------------------*
312600000512     C     SUBTOT        BEGSR
312700000512     C* PULIZIA SUBFILE  PER GLI ERRORI
312800000512     C                   EXSR      PULSF
312900000512     C                   Z-ADD     1             REC
313000000512     C                   Z-ADD     0             NRR
313100000512     C                   SETOFF                                       1101
313200000718     C* SE E' STATO RICHIESTO IL NUEMRO DI SERIE LEGGO E CONFERMO IL VAB
313300000718     C* PER OGNI CLIENTE PRESENTE NELLA SCHIERA CLI PRECEDENTEMENTE
313400000718     C* CARICATA DALLA TABELLA 3C
313500120903    1C*    C             IFEQ      *ZEROS
313600120903     C*    *IN08         ANDEQ     *OFF
313700120903     C*                  Z-ADD     1             C
313800120903     C*                  MOVEA     VI4KSC        CLI(C)
313900120903   1-C*                  ENDIF
314000120903     C*                  Z-ADD     1             Z                 3 0
314100120903     C*                  SETOFF                                       08
314200000718     C*
314300120903    1C*    Z             DOWLE     C
314400120903     C*                  MOVE      CLI(Z)        VI4KSC
314500000718     C* QUESTO CONTROLLO LO FACCIO QUANDO VENGO DA UNA SELEZIONE TOTALE PER
314600000718     C* SERIE
314700000718     C                   MOVEL     VI4KSC        KSC
314800000718     C                   EXSR      CTRCCM
314900000718     C*
315000000718     C                   SETON                                        02
315100000512     C*
315200000512     C* RICHIESTO A VIDEO IL CODICE CLIENTE SENZA RIFERIMENTO MITTENTE
315300020917    2C     *IN03         IFEQ      *OFF
315400080623     C* PER CLIENTE e rif numerico
315500130121     c                   select
315600130121     c                   when      vi4ord='N'
315700021004     C                   EVAL      WVABNR = 1
315800020917     C     VI4KSC        SETLL     FIVAB01L
315900020917     C     VI4KSC        READE     FIVAB01L                               30
316000130121     c                   when      vi4ord='S'
316100130121     C                   EVAL      WVABNR = 0
316200130121     C     VI4KSC        SETLL     FIVAB10L
316300130121     C     VI4KSC        READE     FIVAB10L                               30
316400080623     C* PER CLIENTE e rif alfa numerico
316500130121     c                   other
316600080623     C                   EVAL      WVABNR = 9
316700080623     C     VI4KSC        SETLL     FIVAB09L
316800080623     C     VI4KSC        READE     FIVAB09L                               30
316900130121     c                   endsl
317000020917     C*
317100020917   X2C                   ELSE
317200000512     C* RICHIESTO A VIDEO SIA IL CODICE CHE IL RIFERIMENTO MITTENTE
317300021004     C                   EVAL      WVABNR = 1
317400020917     C     KVAB          SETLL     FIVAB01L
317500020917     C     KVAB          READE     FIVAB01L                               30
317600020917   2-C                   ENDIF
317700000512     C*
317800020917    2C     *IN30         DOWEQ     *OFF
317900020917      *
318000020926     C     VABFGS        LOOKUP    POG                                    33
318100020917     C* PROSEGUO SE LA LETTURA E' VALIDA ..
318200020923    3C     *IN33         IFEQ      *ON
318201170901     C     vabatb        andne     'A'
318300020917     C*
318400000512     C* ANNULLAMENTO
318500091007      * Prima di annullare la spedizione se bolla RC vado ad annullare
318600091007      * il relativo ORM legato
318700100604    4c                   if        *inkq
318800111010     c                   exsr      annorm
318900100604     C                   clear                   WNRR
319000100604     C                   EXSR      ANNREC
319100100604    4c                   EndIf
319200100604
319300000512     C* CONFERMA
319400020917    4C     *INKF         IFEQ      *ON
319500000515     C* SE RICHIESTA LA SERIE SELEZIONO SOLO LE BOLLE CON QUELLA SERIE
319600020917    5C     VI4NRS        IFEQ      99
319700000515     C     VI4NRS        OREQ      VABNRS
319800041123     C* SE RICHIESTA LA lin arrivo selezSOLO LE BOLLE CON QUELLA lna
319900041123   5aC     VI4lna        IFEQ      999
320000041123     C     VI4lna        OREQ      VABlna
320100111223     C* SE RICHIESTA LA lin parten selezSOLO LE BOLLE CON QUELLA lnp
320200111223   6aC     VI4lnp        IFEQ      *zeros
320300111223     C     VI4lnp        OREQ      VABlnp
320400000515     C                   EXSR      CONREC
320500000524     C* SE DATO IN FNLS01R IL KC VADO A FINE LETTURA
320600020917    6C     §LPFPR        IFEQ      '3'
320700000517     C                   LEAVE
320800020917   6-C                   ENDIF
320900000515     C* SE ARRIVATI A 9900 RECORD DEL SUBFILE VADO A FINE LETTURA
321000020917    6C     NRR           IFGE      9900
321100000515     C                   LEAVE
321200020917   6-C                   ENDIF
321300000515     C*
321400111223   6ac                   endif
321500041123   5aC                   ENDIF
321600041123   5-C                   ENDIF
321700000515     C*
321800020917   4-C                   ENDIF
321900020917   3-C                   ENDIF
322000000512     C*
322100020917    3C     *IN03         IFEQ      *OFF
322200130121     c                   select
322300130121     c                   when      vi4ord='N'
322400020917     C     VI4KSC        READE     FIVAB01L                               30
322500130121     c                   when      vi4ord='S'
322600130121     C     VI4KSC        READE     FIVAB10L                               30
322700130121     c                   other
322800080623     C     VI4KSC        READE     FIVAB09L                               30
322900130121     c                   endsl
323000080623     c
323100020917   X3C                   ELSE
323200020917     C     KVAB          READE     FIVAB01L                               30
323300020917    3C                   ENDIF
323400000512     C*
323500020917    2C                   ENDDO
323600000718     C*
323700120903     C*                  ADD       1             Z
323800120903    1C*                  ENDDO
323900000512     C*
324000000512     C                   ENDSR
324100000512     C*
324200000512     C*-------------------------------------------------------------*
324300000512     C*     -- ROUTINE DI ANNULLAMENTO RECORD VAB/VAD       --      *
324400000512     C*-------------------------------------------------------------*
324500000512     C     ANNREC        BEGSR
324600911031     C*
324700000512     C*
324800020910     C* CANCELLO RECORD IN FIVAD000
324900020910     C     KBLD          SETLL     FIVAD000
325000020910     C     KBLD          READE     FIVAD000                               32
325100000512     C*
325200000512    3C     *IN32         DOWEQ     *OFF
325300020910     C                   DELETE    FIVAD000
325400020910     C     KBLD          READE     FIVAD000                               32
325500000512    3C                   ENDDO
325600020108     C*
325700020910     C* CANCELLO RECORD IN FIVAT000
325800020910     C     KBLD          SETLL     FIVAT000
325900020910     C     KBLD          READE     FIVAT000                               32
326000020108     C*
326100020108    3C     *IN32         DOWEQ     *OFF
326200020910     C                   DELETE    FIVAT000
326300020910     C     KBLD          READE     FIVAT000                               32
326400020108    3C                   ENDDO
326500040604     c*
326600040604     C* CANCELLO RECORD IN FIVAX000
326700040604     C     KBLD          SETLL     FIVAX000
326800040604     C     KBLD          READE     FIVAX000                               32
326900040604     C*
327000040604    3C     *IN32         DOWEQ     *OFF
327100040604     C                   DELETE    FIVAX000
327200040604     C     KBLD          READE     FIVAX000                               32
327300040604    3C                   ENDDO
327400081017      * CANCELLO RECORD IN FIRAB000
327500081020     c     kbld          setll     firab02l
327600081017     c                   do        *hival
327700081020     c     kbld          reade     firab02l
327800081017    3c                   if        %eof(firab02l)
327900081017     c                   leave
328000081017     c                   endif
328100081017     c                   delete    firab000
328200081017    3c                   enddo
328300000512     C*
328400000512     C* CANCELLO EVENTUALE RECORD DEL RIFERIMENTO PARTNER ESTERO
328500060221     C***                MOVEL     VABLNP        WORG
328600060221     C***                EXSR      CTRORG
328700000512     C*
328800060221     C***  §OGNTW        IFEQ      'EEX'
328900060221     C***  §OGNTW        OREQ      'EUP'
329000000512     C* SOLO SE NON ESISTE GIA' LA BOLLA IN FNBLP
329100060221     C***  KBLP2         SETLL     FNBLP000                               32
329200060221     C**N32KBL4          DELETE    Fiar4000                           32
329300060221     C***                ENDIF
329400080623      *
329500021003      * CANCELLO FIVAB
329600021003      *  Se cancellazione singola ho letto FIVAB02L e cancello FIVAB00F
329700021003      *   con il num. relativo record che mi sono memorizzato
329800021003      *  Se annullamento totale ho letto FIVAB01L e cancello FIVAB01L
329900021003     C                   IF        Wnrr > *zeros
330000021003     C     Wnrr          DELETE    FIVAB0FF
330100021003     C                   else
330200130121     c                   select
330300130121     c                   when      vi4ord='N'
330400021003     c                   DELETE    FIVAB000
330500130121     c                   when      vi4ord='S'
330600130121     c                   DELETE    FIVAB10
330700130121     c                   other
330800080623     c                   DELETE    FIVAB09
330900130121     c                   endsl
331000021003     c                   endif
331100000512     C*
331200000512     C                   ENDSR
331300000512     C*
331400000512     C*-------------------------------------------------------------*
331500000512     C*     -- ROUTINE DI CONFERMA     RECORD VAB/VAD       --      *
331600000512     C*-------------------------------------------------------------*
331700000512     C     CONREC        BEGSR
331800000515     C* CARICO I CAMPI PER IL CONTROLLO DEL SINGOLO RECORD
331900000515     C                   EXSR      EMISS
332000000515     C* CONTROLLO DEL SINGOLO RECORD
332100000515     C                   EXSR      CONF
332200000512     C*
332300000512     C                   ENDSR
332400000512     C*-------------------------------------------------------------*
332500000512     C*     -- AGGANCIO AZORG --                                    *
332600000512     C*-------------------------------------------------------------*
332700000512     C     CTRORG        BEGSR
332800000512     C*
332900000524     C     WORG          CHAIN     AZORG01L                           37
333000000512     C     *IN37         IFEQ      *ON
333100000512     C     ORGFVA        ORNE      ' '
333200000512     C                   MOVEL     *BLANK        ORGFL1
333300000524     C                   MOVEL     *BLANK        ORGDES
333400000524     C                   MOVEL     *BLANK        ORGDE3
333500010219     C                   MOVEL     *BLANK        ORGDE8
333600040121     C                   MOVEL     *BLANK        ORGDF0
333700000512     C                   ENDIF
333800000524     C                   MOVEL     ORGDE3        OG143
333900000512     C*
334000000512     C                   ENDSR
334100000523     C*-------------------------------------------------------------*
334200000523     C*     -- ROUTINE DI RICERCA BLP PER RECORD BOLLA POSTA --     *
334300000523     C*-------------------------------------------------------------*
334400060221     C**** CERBLP        BEGSR
334500000523     C*
334600060221     C*****              CLEAR                   WESCI
334700001214     C*
334800001207     C* AGGANCIO CON IL SEGNACOLLO (FLS NRS NSC) DETTAGLIO SEGNACOLLI
334900001214     C* DEVO FARE UN CICLO DI LETTURA PERCHE' POTREI AVERNE ANCHE
335000001214     C* PIU' DI UNA DI BOLLE CON STESSO SEGNACOLLO MA CON <> LNA SE
335100041123     C* PER ESEMPIO ERANO PRESENTI DUE O PIU' ANOMALIE 10 SU STESSO
335200001214     C* SEGNACOLLO MA CON LNA <>.
335300000523     C*
335400060221     C**** KBLT          SETLL     FNBLT29L
335500060221     C**** KBLT          READE     FNBLT29L                               37
335600000523     C*
335700060221     C**** *IN37         DOWEQ     *OFF
335800060221     C**** WESCI         ANDEQ     *BLANKS
335900001214     C* SE COLLO DOPPIO E LNA <> MEMORIZZO ED ESCO DA QUESTA ROUTINE
336000060221    2C**** VABLNA        IFNE      BLTLNA
336100060221     C****               MOVEL     'S'           WDOPP             1
336200060221   X2C****               ELSE
336300001214     C* SE COLLO DOPPIO E STESSA LNA AGGANCIO FNBLP PER VEDERE SE SI
336400001214     C* TRATTA DI BOLLA AUTOGENERATA
336500060221     C****               CLEAR                   WDOPP
336600060221     C****               MOVE      '1'           WESCI             1
336700000523     C* AGGANCIO FNBLP
336800060221     C**** KBLPPT        CHAIN     FNBLP01L                           38
336900060221     C**** *IN38         IFEQ      *OFF
337000000523     C* AUTOGENERATA  AGGIORNO E DELETO
337100060221     C**** BLPSOP        IFEQ      '&'
337200060221     c**** BlpDcm        Andeq     *Zeros
337300060221     C****               EXSR      AGGBLP
337400060221     C****               MOVEL     'S'           TROVA             1
337500060221     C****               ELSE
337600060221     C****               UNLOCK    FNBLP01L
337700060221     C****               ENDIF
337800000523     C*
337900060221     C*****              ENDIF
338000060221    2C*****              ENDIF
338100000526     C*
338200060221     C**** KBLT          READE     FNBLT29L                               37
338300060221     C****               ENDDO
338400000523     C*
338500060221     C****               ENDSR
338600000523     C*
338700000523     C*-------------------------------------------------------------*
338800000523     C*     -- ROUTINE DI RICERCA BLP PER RECORD BOLLA POSTA --     *
338900000523     C*-------------------------------------------------------------*
339000060221     C**** AGGBLP        BEGSR
339100021220
339200021220      * In questa routine non aggiorno FIAR5 xchè per le bolle poste
339300021220      * non sono previsti i bancali
339400000531     C*
339500060221     C*****              MOVEL     'J'           WPTTRC
339600000531     C*
339700000523     C* VERIFCO SE BOLLA GIA' AGGIORNATA IN UNA FASE PRECEDENTE
339800060221     C**** KBL4PT        CHAIN     Fiar401L                           37
339900000523     C*
340000000526     C*
340100060221     C**** *IN37         IFEQ      *ON
340200060221     C****               CLEAR                   Fiar4000
340300060221     C****               ENDIF
340400000523     C* VERIFICO:SE BOLLA GIA' AGGIORNATA PROSEGUO SENZA AGGIORNARE BLP
340500000523     C*
340600060221     C****               MOVEL     BL4NOT        DSBL4J
340700000523     C*
340800060221    2C***  §B4BAU        IFEQ      'S'
340900060221     C**N37              UNLOCK    Fiar401L
341000060221     C****               ELSE
341100000627     C*
341200060221     C****               UNLOCK    FNBLP01L
341300000627     C* CARICO LA DS CHE MI SERVE PER RICHIAMARE IL PGM FNLS01R (IMMISSIONE
341400000627     C* BOLLE) SOLO PER I CONTROLLI
341500060221     C*****              CLEAR                   WWCTC
341600060221     C******             EXSR      CRTDS
341700000627     C* MODFICO ALCUNI CAMPI
341800060221     C*****              MOVEL     BLPLNP        §LPLNP
341900060221     C******             MOVEL     BLPNRS        §LPNRS
342000060221     C******             MOVEL     BLPNSP        §LPNSP
342100060221     C******             MOVEL     'C'           §LPFLG
342200060221     C*****              MOVEL     '&'           §LPSOP
342300060221     C*****              MOVEL     VABCCM        §LPCCM
342400060221     C****               MOVEL     VABCCM        §LPCCN
342500000627     C* PULISCO QUESTI CAMPI PERCHÈ LI AGGIORNO IO DAL VAB  IN BLP E ASSOCIATI
342600060221     C*****              CLEAR                   §LPIAS
342700060221     C****               CLEAR                   §LPVAS
342800050114     C**!!!              CLEAR                   §LPXCO
342900030611      *
343000060221     C*****              CALL      'FNLS01R'
343100060221     C*****              PARM                    KPJBA
343200060221     C*****              PARM                    DBLP
343300060221     C*****              PARM                    DTASV
343400060221     C*****              PARM                    trul90ds
343500030611      *
343600060221     C****               MOVEL     '1'           RICH              1
343700000627     C* SE FLAG=4 CMD12 NON CONFERMABILE
343800000627     C* SE FLAG=3 CMD03 NON CONFERMABILE
343900000627     C* SE FLAG=5 NON COMFERMABILE ERRORE
344000000523     C*
344100060221     C***  §LPFPR        IFNE      '4'
344200060221     C**** §LPFPR        ANDNE     '3'
344300060221     C**** §LPFPR        ANDNE     '5'
344400000627     C* AGGIORNO
344500060221     C*****              Z-ADD     VABIAS        §B4IAS
344600060221     C**** VABVAS        IFEQ      *BLANKS
344700060221     C***  VABIAS        ANDGT     *ZEROS
344800060221     C*****              MOVEL     'ITL'         §B4VAS
344900060221     C*****              ELSE
345000060221     C****               MOVEL     VABVAS        §B4VAS
345100060221     C*****              ENDIF
345200050114     C**!!!              MOVE      VABXCO        §B4XCO
345300060221     C*****              Clear                   §B4XCO
345400060221     C******             MOVEL     'S'           §B4BAU
345500060221     C* SE DA CLIENTE RICEVO F TOLGO LA 'S' DA ar4
345600060221     C**** §LPTC1        IFEQ      'F'
345700060221     C**** §LPTC2        OREQ      'F'
345800060221     C****               CLEAR                   §B4BAI
345900060221     C******             ENDIF
346000001221     C* MEMORIZZO ANCHE LA DATA SPEDIZIONE DEL CLIENTE
346100060221     C****               MOVEL     VABAAS        §B4DSC
346200060221     C****               MOVE      VABMGS        §B4DSC
346300010118     C* RICEVO IL CONTRASSEGNO DAL CLIENTE:
346400010118     C* SE BOLLA GIA' CONSEGNATA CON CONSEGNA ANOMALA DEVO MEMORIZZARE
346500010118     C* CHE NON RICEVERO' MAI DALL'ARRIVO IL RECORD DELL'INCASSO DEL
346600010118     C* CONTRASSEGNO. QUESTO FLAG SERVE IN PULIZIA BOLLE PARTENZE PER
346700010118     C* POTER LO STESSO PULIRE LA BOLLA CON CONTRASSEGNO NON INCASSATO
346800060221     C**** §LPCAS        IFGT      *ZEROS
346900060221     C***  BLPDCM        ANDGT     *ZEROS
347000060221     C***  BLPCCA        ANDNE     *BLANKS
347100060221     C****               MOVEL     'S'           §B4CSB
347200060221     C****               ENDIF
347300001221     C*
347400060221     C*****              MOVEL     DSBL4J        BL4NOT
347500000526     C*
347600060221     C***  *IN37         IFEQ      *ON
347700060221     C***                MOVEL     BLTAAS        BL4AAS
347800060221     C****               MOVEL     BLTLNP        BL4LNP
347900060221     C****               MOVEL     BLTNRS        BL4NRS
348000060221     C****               MOVEL     BLTNSP        BL4NSP
348100060221     C****               MOVEL     'J'           BL4TRC
348200060221     C****               MOVEL     BLTAAS        BL4AAS
348300060221     C****               WRITE     Fiar4000
348400000526     C*
348500060221     C*****              ELSE
348600000523     C*
348700060221     C*****              UPDATE    Fiar4000
348800000526     C*
348900060221     C*****              ENDIF
349000000524     C*
349100000523     C* AGGIORNO BLP
349200000615     C* SE IMPORTO CONTRASSEGNO E' A ZEROS NON AGGIORNO IL CODICE BOLLA
349300000615     C* PERCHE' POTREBBE ESSERE STATO MODIFICATO ED INCASSATO IN ARRIVO
349400000615     C*
349500060221     C***  KBLPPT        CHAIN     FNBLP01L                           38
349600000627     C*
349700060221     C**** §LPCAS        IFNE      *ZEROS
349800060221     C****               MOVE      §LPCBO        BLPCBO
349900060221     C****               ENDIF
350000000627     C* MITTENTE
350100060221     C*****              Z-ADD     §LPCCN        BLPKSC
350200060221     c****               Eval      wtxt = §lpRsm
350300060221     c****               ExSr      Sr_Chk
350400060221     c****               Movel     wtxt          BlpRsm
350500040301     C**!!!              MOVEL     §LPRSM        BLPRSM
350600060221     c****               Eval      wtxt = §lpInm
350700060221     c****               ExSr      Sr_Chk
350800060221     c*****              Movel     wtxt          BlpInm
350900040301     C**!!!              MOVEL     §LPINM        BLPINM
351000060221     c****               Eval      wtxt = §lpLom
351100060221     c*                  ExSr      Sr_Chk
351200060221     c*                  Movel     wtxt          BlpLom
351300040301     C**!!!              MOVEL     §LPLOM        BLPLOM
351400060221     c*                  Eval      wtxt = §lpCam
351500060221     c*                  ExSr      Sr_Chk
351600060221     c*                  Movel     wtxt          BlpCam
351700040301     C**!!!              MOVEL     §LPCAM        BLPCAM
351800060221     C*                  MOVEL     §LPPRM        BLPPRM
351900060221     C*                  MOVEL     §LPNZM        BLPNZM
352000060221     C*                  MOVEL     §LPCPI        BLPCPI
352100000627     C* DESTINATARIO
352200060221     c*                  Eval      wtxt = §lpRsd
352300060221     c*                  ExSr      Sr_Chk
352400060221     c*                  Movel     wtxt          BlpRsd
352500040301     C**!!!              MOVEL     §LPRSD        BLPRSD
352600060221     c*                  Eval      wtxt = §lpInd
352700060221     c*                  ExSr      Sr_Chk
352800060221     c*                  Movel     wtxt          BlpInd
352900040301     C**!!!              MOVEL     §LPIND        BLPIND
353000060221     c*                  Eval      wtxt = §lpLod
353100060221     c*                  ExSr      Sr_Chk
353200060221     c*                  Movel     wtxt          BlpLod
353300040301     C**!!!              MOVEL     §LPLOD        BLPLOD
353400060221     c*                  Eval      wtxt = §lpCad
353500060221     c*                  ExSr      Sr_Chk
353600060221     c*                  Movel     wtxt          BlpCad
353700040301     C**!!!              MOVEL     §LPCAD        BLPCAD
353800060221     C*                  MOVEL     §LPPRD        BLPPRD
353900060221     C*                  MOVEL     §LPNZD        BLPNZD
354000000627     C*
354100030924     C**!!!              MOVEL     §LPFLO        DBLP01
354200060221     C*    BLPFDN        IFNE      'S'
354300060221     C*                  MOVEL     §LPFAP        BLPFAP
354400060221     C*                  ENDIF
354500060221     C*                  MOVEL     §LPFIN        BLPFIN
354600000627     C*
354700000627     C* VISTO CHE I GIORNI DI CHIUSURA POSSONO ESSERE AGGIORNATI DALL'ARRIVO
354800000627     C* PRIMA DI AGGIORNARE QUELLI CHE MI HA DATO IL CLIENTE VERRIFICO
354900000627     C* CHE SIANO PIENI
355000060221     C*    §LPGC1        IFNE      *BLANKS
355100060221     C*    §LPGC1        ANDNE     BLPGC1
355200060221     C*    §LPGC1        ANDNE     BLPGC2
355300060221     C*    BLPGC1        ANDEQ     *BLANKS
355400060221     C*                  MOVEL     §LPGC1        BLPGC1
355500060221     C*                  ENDIF
355600060221     C*    §LPGC1        IFNE      *BLANKS
355700060221     C*    §LPGC1        ANDNE     BLPGC1
355800060221     C*    §LPGC1        ANDNE     BLPGC2
355900060221     C*    BLPGC2        ANDEQ     *BLANKS
356000060221     C*                  MOVEL     §LPGC1        BLPGC2
356100060221     C*                  ENDIF
356200060221     C*    §LPGC2        IFNE      *BLANKS
356300060221     C*    §LPGC2        ANDNE     BLPGC1
356400060221     C*    §LPGC2        ANDNE     BLPGC2
356500060221     C*    BLPGC1        ANDEQ     *BLANKS
356600060221     C*                  MOVEL     §LPGC2        BLPGC1
356700060221     C*                  ENDIF
356800060221     C*    §LPGC2        IFNE      *BLANKS
356900060221     C*    §LPGC2        ANDNE     BLPGC1
357000060221     C*    §LPGC2        ANDNE     BLPGC2
357100060221     C*    BLPGC2        ANDEQ     *BLANKS
357200060221     C*                  MOVEL     §LPGC2        BLPGC2
357300060221     C*                  ENDIF
357400000627     C*
357500060221     C*                  MOVEL     §LPCTR        BLPCTR
357600060221     C*                  MOVEL     §LPCTS        BLPCTS
357700060221     C*                  MOVEL     §LPTSP        BLPTSP
357800060221     C*                  MOVEL     §LPNAS        BLPNAS
357900060221     C*                  MOVEL     §LPNCL        BLPNCL
358000060221     C*                  MOVEL     §LPPKB        BLPPKB
358100060221     C*                  MOVEL     §LPPKB        BLPPKF
358200000627     C* VOLUME
358300060221     C*    §LPVLB        IFGT      0
358400060221     C*                  MOVEL     §LPVLB        BLPVLB
358500060221     C*                  MOVEL     §LPFVB        BLPFVB
358600000607     C* VERIFICO IL VOLUME FATTURATO
358700060221     C*    BLPFVF        IFNE      'T'
358800060221     C*    BLPFVF        ANDEQ     'Z'
358900060221     C*    §LPVLB        ANDGT     BLPVLF
359000060221     C*                  MOVEL     §LPVLB        BLPVLF
359100060221     C*                  MOVEL     §LPFVB        BLPFVF
359200060221     C*                  ENDIF
359300060221     C*    BLPFVF        IFNE      'T'
359400060221     C*    BLPFVF        ANDNE     'Z'
359500060221     C*                  MOVEL     §LPVLB        BLPVLF
359600060221     C*                  MOVEL     §LPFVB        BLPFVF
359700060221     C*                  ENDIF
359800060221     C*                  ENDIF
359900000607     C*
360000060221     c*                  Eval      wtxt = §lpRma
360100060221     c*                  ExSr      Sr_Chk
360200060221     c*                  Movel     wtxt          BlpRma
360300040301     C**!!!              MOVEL     §LPRMA        BLPRMA
360400060221     C*                  MOVEL     §LPRMN        BLPRMN
360500060221     C*                  MOVEL     §LPCTM        BLPCTM
360600060221     C*                  MOVEL     §LPGGA        BLPGGA
360700000627     C*******************  MOVEL§LPZNC    BLPZNC *******************************
360800060221     C*                  MOVEL     §LPGMA        BLPGMA
360900060221     C*                  MOVEL     §LPGVA        BLPGVA
361000060221     C*                  MOVEL     §LPFFD        BLPFFD
361100060221     C*                  Z-ADD     §LPQFT        BLPQFT
361200000627     C* SE DATA CONSEGNA RICHIESTA NON VALORIZZATA
361300060221     C*    BLPDCR        IFEQ      *ZEROS
361400060221     C*                  Z-ADD     §LPDCR        BLPDCR
361500060221     C*                  Z-ADD     §LPHCR        BLPHCR
361600060221     C*                  MOVEL     §LPTCR        BLPTCR
361700060221     C*                  ENDIF
361800000615     C* VISTO CHE I TIPI CONSEGNA POSSO ESSERE AGGIORNATI DALL'ARRIVO
361900000615     C* PRIMA DI AGGIORNARE QUELLI CHE MI HA DATO IL CLIENTE VERRIFICO
362000000615     C* CHE SIANO PIENI
362100060221     C*    §LPTC1        IFNE      *BLANKS
362200060221     C*    §LPTC1        ANDNE     BLPTC1
362300060221     C*    §LPTC1        ANDNE     BLPTC2
362400060221     C*    BLPTC1        ANDEQ     *BLANKS
362500060221     C*                  MOVEL     §LPTC1        BLPTC1
362600060221     C*                  ENDIF
362700060221     C*    §LPTC1        IFNE      *BLANKS
362800060221     C*    §LPTC1        ANDNE     BLPTC1
362900060221     C*    §LPTC1        ANDNE     BLPTC2
363000060221     C*    BLPTC2        ANDEQ     *BLANKS
363100060221     C*                  MOVEL     §LPTC1        BLPTC2
363200060221     C*                  ENDIF
363300060221     C*    §LPTC2        IFNE      *BLANKS
363400060221     C*    §LPTC2        ANDNE     BLPTC1
363500060221     C*    §LPTC2        ANDNE     BLPTC2
363600060221     C*    BLPTC1        ANDEQ     *BLANKS
363700060221     C*                  MOVEL     §LPTC2        BLPTC1
363800060221     C*                  ENDIF
363900060221     C*    §LPTC2        IFNE      *BLANKS
364000060221     C*    §LPTC2        ANDNE     BLPTC1
364100060221     C*    §LPTC2        ANDNE     BLPTC2
364200060221     C*    BLPTC2        ANDEQ     *BLANKS
364300060221     C*                  MOVEL     §LPTC2        BLPTC2
364400060221     C*                  ENDIF
364500001116     C* SE IL CLIENTE MI PASSA "F"=FUORI MISURA E SULLA BOLLA TC1 E TC2
364600001116     C* SONO PIENI E DIVERSI DA 'F' PERDO TC2 E METTO 'F'
364700060221     C*    §LPTC1        IFEQ      'F'
364800060221     C*    §LPTC2        OREQ      'F'
364900060221     C*    BLPTC1        IFNE      'F'
365000060221     C*    BLPTC2        ANDNE     'F'
365100060221     C*                  MOVEL     'F'           BLPTC2
365200060221     C*                  ENDIF
365300060221     C*                  ENDIF
365400000615     C*
365500060221     C*                  CLEAR                   BLPFT1
365600060221     C*                  CLEAR                   BLPFT2
365700060221     C*                  CLEAR                   BLPDT1
365800060221     C*                  CLEAR                   BLPDT2
365900060221     C*                  Z-ADD     99999         BLPNFV
366000060221     C*                  Z-ADD     DATEU         BLPDBR
366100000523     C*
366200060221     C*                  UPDATE    FNBLP000
366300000626     C*
366400060221     C*                  MOVEL     'S'           RICLSA            1
366500000524     C* ESTENSIONE RAGIONE SOCIALE
366600060221    3C*    §LPRD2        IFNE      *BLANKS
366700060221     C*                  MOVEL     'P'           D19TBO
366800060221     C*                  MOVEL     BLPAAS        D19AAS
366900060221     C*                  MOVEL     BLPLNP        D19LNP
367000060221     C*                  MOVEL     BLPNRS        D19NRS
367100060221     C*                  MOVEL     BLPNSP        D19NSP
367200060221     C*                  MOVEL     'D'           D19TRC
367300060221     c*                  Eval      wtxt = §lpRd2
367400060221     c*                  ExSr      Sr_Chk
367500060221     c*                  Movel(p)  wtxt          d19Nt1
367600040301     C**!!!              MOVEL(P)  §LPRD2        D19NT1
367700060221     C*                  CLEAR                   D19NT2
367800060221     C*                  CALL      'FNLV19R'
367900060221     C*                  PARM                    DSLV19
368000060221    3C*                  ENDIF
368100000524     C* NOTE DEL MITTENTE PER IL DESTINATARIO
368200060221    3C*    §LPNOT        IFNE      *BLANKS
368300060221     C*    §LPNT2        ORNE      *BLANKS
368400060221     C*                  MOVEL     'P'           D19TBO
368500060221     C*                  MOVEL     BLPAAS        D19AAS
368600060221     C*                  MOVEL     BLPLNP        D19LNP
368700060221     C*                  MOVEL     BLPNRS        D19NRS
368800060221     C*                  MOVEL     BLPNSP        D19NSP
368900060221     C*                  MOVEL     '8'           D19TRC
369000060221     c*                  Eval      wtxt = §lpNot
369100060221     c*                  ExSr      Sr_Chk
369200060221     c*                  Movel(p)  wtxt          d19Nt1
369300040301     C**!!!              MOVEL(P)  §LPNOT        D19NT1
369400060221     c*                  Eval      wtxt = §lpNt2
369500060221     c*                  ExSr      Sr_Chk
369600060221     c*                  Movel(p)  wtxt          d19Nt2
369700040301     C**!!!              MOVEL(P)  §LPNT2        D19NT2
369800060221     C*                  CALL      'FNLV19R'
369900060221     C*                  PARM                    DSLV19
370000060221    3C*                  ENDIF
370100000524     C* C/A     VALORIZZATO
370200060221    3C*    §LPCAS        IFGT      *ZEROS
370300051115     C* AGGANCIARE IL ar9
370400060221     C*    KBLPPT        CHAIN     Fiar901L                           39
370500000525     C*
370600060221     C*                  Z-ADD     §LPCAS        ar9CAS
370700060221     C*                  MOVEL     §LPVCA        ar9VCA
370800060221     C*                  Z-ADD     §LPCMB        ar9CMB
370900000525     C*
371000060221     C*                  MOVEL     §LPGCA        ar9GCA
371100060221     C*                  MOVEL     §LPTIC        ar9TIC
371200060221     c*                  movel     dateu         ar9duv
371300000525     C*
371400060221     C*    *IN39         IFEQ      *ON
371500060221     C*                  MOVEL     BLPAAS        ar9AAS
371600060221     C*                  MOVEL     BLPLNP        ar9LNP
371700060221     C*                  MOVEL     BLPNRS        ar9NRS
371800060221     C*                  MOVEL     BLPNSP        ar9NSP
371900060221     C*                  WRITE     Fiar9000
372000060221     C*                  ELSE
372100060221     C*                  UPDATE    Fiar9000
372200060221     C*                  ENDIF
372300000525     C*
372400060221    3C*                  ENDIF
372500000524     C*
372600060221    2C*                  ENDIF
372700060221     C* AGGIORNO IL TIPO RECORD W DEL ar4 PER TRASMISSIONE VAB AL CLIENTE
372800000531     C*
372900060221     C*                  MOVEL     'W'           WPTTRC
373000000531     C*
373100060221     C*    KBL4PT        CHAIN     Fiar401L                           37
373200000531     C*
373300060221     C*    *IN37         IFEQ      *OFF
373400060221     C*                  MOVEL     BL4NOT        DSBL4W
373500060221     C*                  MOVEL     *ZEROS        §B4DAT
373600060221     C*                  MOVEL     DSBL4W        BL4NOT
373700000531     C*
373800060221     C*                  UPDATE    Fiar4000
373900000531     C*
374000060221     C*                  ENDIF
374100000627     C*
374200060221     C*                  ENDIF
374300000525     C*
374400060221     C*                  ENDSR
374500000705     C*
374600000705     C*--- CONTROLLO I CODICI CLIENTI CHE SONO LEGATI AL NR.SERIE-----*
374700120903     C*    CTRNRS        BEGSR
374800000717     C*
374900120903     C*                  Z-ADD     *ZEROS        C                 3 0
375000000717     C*
375100120903     C*                  MOVEL     '3C'          COD
375200120903     C*                  MOVEL     *BLANKS       KEY
375300120903     C*                  MOVEL     VI4FGS        KEY
375400000705     C*
375500120903     C*    KTAB          SETLL     TABEL00F
375600000705     C*
375700120903     C*                  DO        *HIVAL
375800000705     C*
375900120903     C*    KTAB2         READE     TABEL00F                               30
376000120903     C*
376100120903     C*  30              LEAVE
376200120903     C* CONTROLLO SE LA FILIALE DEL CLIENTE È GIUSTA
376300120903     C*                  MOVEL     TBLKEY        W0030             3 0
376400120903     C*    W0030         IFNE      VI4FGS
376500120903     C*                  LEAVE
376600120903     C*                  ENDIF
376700120903     C*
376800120903     C*                  MOVEL     TBLUNI        DS3C
376900000705     C* E LA SERIE È GIUSTA
377000000825     C*  ESCLUDO GLI ANNULLATI
377100120903     C*    §3CNRS        IFNE      VI4NRS
377200120903     C*    TBLFLG        ORNE      ' '
377300120903     C*                  ITER
377400120903     C*                  ENDIF
377500120903     C* CONTROLLO SE IL CLIENTE È  GIUSTO
377600120903     C*                  MOVEL     TBLKEY        KSC
377700120903     C*
377800120903     C*                  EXSR      CTRCCM
377900120903     C* SE NON HA L'* E' COME SE NON CI FOSSE LA 3C
378000120903    1C* N90§3COKD        IFNE      '*'
378100120903     C*                  SETON                                        06
378200120903    1C*                  ENDIF
378300120903     C* SE C'È ERRORE OPPURE E' ACCESO 06 (CLIENTE ANCORA IN ATTESA DI PARTIRE
378400120903     C* CON LA PROCEDURA) NON CARICO IN SCHIERA
378500120903     C*    *IN90         IFEQ      *OFF
378600120903     C*    *IN06         ANDEQ     *OFF
378700120903     C*                  ADD       1             C
378800120903     C*                  MOVE      WCCMN         CLI(C)
378900120903     C*                  ENDIF
379000120903     C* SPENGO INDICATORI DI ERRORE
379100120903     C*                  SETOFF                                       904043
379200120903     C*                  SETOFF                                       6306
379300120903     C*                  MOVEL     '3C'          COD
379400120903     C*                  MOVEL     KSC           KEY
379500120903     C*
379600120903     C*    KTAB          SETGT     TABEL00F
379700000718     C*
379800120903     c*                  ENDDO
379900000705     C*
380000120903     C*                  ENDSR
380100911031     C*--- CREO DS DA PASSARE ----------------------------------------*
380200911114     C     CRTDS         BEGSR
380300991026     C                   CLEAR                   DBLP
380400150909     C                   CLEAR                   DBLP01
380500991026     C                   CLEAR                   DTASV
380600950407     C                   MOVEL     'B'           WBOL              1
380700911031     C*
380800020910     C* P.O DI GESTIONE
380900020917     C                   MOVE      VABFGS        §LPFGS
381000020910      *
381100941010     C                   MOVEL     VABLNP        §LPLNP
381200001221     C* SE STO CONFERMANDO BOLLA POSTE PASSO ANCHE LA DATA SPEDIZIONE
381300001221     C* DEL CLIENTE
381400001221     C     WRKPT         IFEQ      'S'
381500010219     C                   MOVEL     VABAAS        §LPDSK
381600010219     C                   MOVE      VABMGS        §LPDSK
381700001221     C                   ENDIF
381800001221     C                   MOVEL     COMDSP        §LPAAS
381900911114     C                   MOVE      COMDSP        §LPMGS
382000941010     C                   MOVEL     VABNRS        §LPNRS
382100941010     C                   MOVEL     VABNSP        §LPNSP
382200970326     C                   MOVEL     VABAAS        §LPAAC
382300911031     C*
382400941010     C                   MOVEL     VI5KSC        §LPCCM
382500941010     C                   MOVEL     WCCMN         §LPCCN
382600941010     C                   MOVEL     VABCBO        §LPCBO
382700911031     C*
382800011206     C                   MOVEL     VABLNA        §LPLNA
382900011206     C                   MOVEL     VABZNC        §LPZNC
383000040301     c                   Eval      wtxt = VabRsd
383100040301     c                   ExSr      Sr_Chk
383200040301     c                   Movel     wtxt          §LpRsd
383300040301     C**!!!              MOVEL     VABRSD        §LPRSD
383400040301     c                   Eval      wtxt = VabRd2
383500040301     c                   ExSr      Sr_Chk
383600040301     c                   Movel     wtxt          §LpRd2
383700070403     c                   if        §lprd2=§lprsd
383800070403     c                   clear                   §lprd2
383900070403     c                   endif
384000040301     C**!!!              MOVEL     VABRD2        §LPRD2
384100040301     c                   Eval      wtxt = VabInd
384200040301     c                   ExSr      Sr_Chk
384300040301     c                   Movel     wtxt          §LpInd
384400040301     C**!!!              MOVEL     VABIND        §LPIND
384500040301     c                   Eval      wtxt = VabLod
384600040301     c                   ExSr      Sr_Chk
384700140110     c* eseguo routine per eliminare caratteri di punteggiatura precedenti e
384800140110     c* seguenti la località
384900140207     c****               exsr      chkstringa
385000040301     c                   Movel     wtxt          §LpLod
385100040301     C**!!!              MOVEL     VABLOD        §LPLOD
385200040301     c                   Eval      wtxt = VabCad
385300040301     c                   ExSr      Sr_Chk
385400040301     c                   Movel     wtxt          §LpCad
385500040301     C**!!!              MOVEL     VABCAD        §LPCAD
385600941010     C                   MOVEL     VABPRD        §LPPRD
385700941010     C                   MOVEL     VABNZD        §LPNZD
385800941010     C                   MOVEL     VABGC1        §LPGC1
385900941010     C                   MOVEL     VABGC2        §LPGC2
386000041222     C**!!!              MOVEL     VABCTR        §LPCTR
386100941010     C                   MOVEL     VABTSP        §LPTSP
386200941010     C                   MOVEL     VABCTS        §LPCTS
386300941010     C                   MOVEL     VABFTM        §LPFTM
386400941010     C                   MOVEL     VABIAS        §LPIAS
386500941010     C                   MOVEL     VABVAS        §LPVAS
386600011030      * Se importo valorizzato e divisa no, la imposto 'ITL'
386700011030     C     VABIAS        IFGT      0
386800011030     C     VABVAS        ANDEQ     *BLANKS
386900011030     C                   MOVEL     'ITL'         §LPVAS
387000011030     C                   ENDIF
387100011030      *
387200941010     C                   MOVEL     VABVMD        §LPVMD
387300011030     C                   MOVEL     VABVAD        §LPVAD
387400011030      * Se importo valorizzato e divisa no, la imposto 'ITL'
387500011030     C     VABVMD        IFGT      0
387600011030     C     VABVAD        ANDEQ     *BLANKS
387700011030     C                   MOVEL     'ITL'         §LPVAD
387800011030     C                   ENDIF
387900011030      *
388000941010     C                   MOVEL     VABQFT        §LPQFT
388100941010     C                   MOVEL     VABNAS        §LPNAS
388200941010     C                   MOVEL     VABNCL        §LPNCL
388300941010     C                   MOVEL     VABPKB        §LPPKB
388400941010     C                   MOVEL     VABVLB        §LPVLB
388500950301     C     VABVLB        IFGT      0
388600950301     C                   MOVEL     'R'           §LPFVB
388700950301     C                   ELSE
388800950301     C                   CLEAR                   §LPFVB
388900950301     C                   ENDIF
389000941010     C                   MOVEL     VABCAS        §LPCAS
389100941010     C                   MOVEL     VABVCA        §LPVCA
389200011030      * Se importo valorizzato e divisa no, la imposto 'ITL'
389300011030     C     VABCAS        IFGT      0
389400011030     C     VABVCA        ANDEQ     *BLANKS
389500011030     C                   MOVEL     'ITL'         §LPVCA
389600011030     C                   ENDIF
389700011030      *
389800941010     C                   MOVEL     VABTIC        §LPTIC
389900941010     C                   MOVEL     VABTC1        §LPTC1
390000000606     C                   MOVEL     VABTC2        §LPTC2
390100941010     C                   MOVEL     VABRMN        §LPRMN
390200040301     c                   Eval      wtxt = VabRma
390300040301     c                   ExSr      Sr_Chk
390400040301     c                   Movel     wtxt          §LpRma
390500040301     C**!!!              MOVEL     VABRMA        §LPRMA
390600040301     c                   Eval      wtxt = VabRmo
390700040301     c                   ExSr      Sr_Chk
390800040301     c                   Movel     wtxt          §LpRmo
390900040301     C**!!!              MOVEL     VABRMO        §LPRMO
391000040301     c                   Eval      wtxt = VabCmo
391100040301     c                   ExSr      Sr_Chk
391200040301     c                   Movel     wtxt          §LpCam
391300040301     C**!!!              MOVEL     VABCMO        §LPCAM
391400941010     C                   MOVEL     VABNMO        §LPNZM
391500941010     C                   MOVEL     VABNCA        §LPNCA
391600941010     C                   MOVEL     VABNCD        §LPNCD
391700941010     C                   MOVEL     VABXCO        §LPXCO
391800040301     c                   Eval      wtxt = VabNot
391900040301     c                   ExSr      Sr_Chk
392000040301     c                   Movel     wtxt          §LpNot
392100040301     C**!!!              MOVEL     VABNOT        §LPNOT
392200040301     c                   Eval      wtxt = VabNt2
392300040301     c                   ExSr      Sr_Chk
392400040301     c                   Movel     wtxt          §LpNt2
392500040301     C**!!!              MOVEL     VABNT2        §LPNT2
392600011121      *
392700011121      * CODICE TRATTAMENTO MERCE: IMPOSTO DA VIDEO SE PRESENTE
392800011121      *        OPPURE SE DISK B CHE DIVENTA DISK C DA TABELLA "1B"
392900020917      *        OPPURE DA FIVAB
393000011121      *
393100920324     C     VI4CTM        IFNE      '  '
393200920324     C                   MOVEL     VI4CTM        §LPCTM
393300920325     C                   ELSE
393400001006     C     WWCTC         IFNE      '  '
393500001006     C                   MOVEL     WWCTC         §LPCTM
393600001006     C                   ELSE
393700941010     C                   MOVEL     VABCTM        §LPCTM
393800001006     C                   ENDIF
393900920324     C                   END
394000041222      * Codice tariffa
394100041222     c                   If        vi4ctr <> *Blanks
394200041222     c                   Move      vi4ctr        §lpctr
394300041222     c                   Else
394400041222     c                   Move      vabctr        §lpctr
394500041222     c                   EndIf
394600950407     C* SE SI TRATTA DI CLIENTE CON    SERIE --> DISK "B" METTO "%"
394700950407     C* SE SI TRATTA DI CLIENTE SENZA  SERIE --> DISK "A" METTO "§"
394800950407     C                   MOVEL     '§'           §LPSOP
394900950407     C     §LPCTM        IFNE      *BLANKS
395000950407     C     §1BFG1        ANDEQ     'S'
395100950407     C                   MOVEL     '%'           §LPSOP
395200950407     C                   ENDIF
395300941010     C                   MOVEL     VABFFD        §LPFFD
395400941010     C                   MOVEL     VABDCR        §LPDCR
395500941010     C                   MOVEL     VABHCR        §LPHCR
395600941010     C     VABTCR        IFNE      'D'
395700941010     C     VABTCR        ANDNE     'P'
395800930528     C                   MOVEL     ' '           §LPTCR
395900930528     C                   ELSE
396000941010     C                   MOVEL     VABTCR        §LPTCR
396100930528     C                   END
396200030409      * se non c'è la consegna richiesta nel VAB
396300030411     c                   If        VabDcr = *Zeros and VabHcr = *Zeros
396400030409      * se il p.o. gestione non è un p.o. poste
396500030409     c                             and wrkpt <> 'S'
396600030409      * passo i dati della consegna richiesta (tipo-data-ora)
396700030409     c                   Eval      §lpTcr = Vi4Tcr
396800030409     c                   Eval      §lpDcr = wdcr
396900030409     c                   Movel     Vi4Hcr        §lpHcr
397000030410      * passo le ore da sommare alla delivery
397100030409     c                   Move      Vi4Ore        §LpOre
397200030409     c                   EndIf
397300930528     C*
397400950321     C                   MOVE      VI4PDR        §LPPDR
397500950321     C                   MOVEL     VI4LPD        §LPPDR
397600941010     C                   MOVEL     VI4FPP        §LPFPP
397700941010     C                   MOVEL     VI4NRT        §LPNRT
397800911031     C                   MOVEL     COMDRT        §LPDRT
397900000516     C     VI5SCE        IFNE      *BLANKS
398000050407     C                   MOVEL     VI5SCE        §LPFPR
398100000516     C                   ELSE
398200000516     C     VI4SCE        IFEQ      'X'
398300000516     C     TOTA          ANDEQ     'S'
398400050407     C                   MOVEL     'W'           §LPFPR
398500000516     C                   ELSE
398600050407     C                   MOVEL     VI4SCE        §LPFPR
398700000516     C                   ENDIF
398800000516     C                   ENDIF
398900941010     C                   MOVEL     VABGCA        §LPGCA
399000941010     C                   MOVEL     VABGGA        §LPGGA
399100941010     C                   MOVEL     VABGMA        §LPGMA
399200941010     C                   MOVEL     VABGVA        §LPGVA
399300970407     C* RICHIAMO DA CONFERMA BOLLA
399400970407     C                   MOVEL     'C'           §LPRIC
399500010219     C* PASSO NUMERO ORM
399600060926     C***  WOBBL         IFEQ      'S'
399700060926     C     vidorm        IFgt      0
399800010219     C                   Z-ADD     VI4POE        §LPPOE
399900010227     C* SE PO EMITTENTE = 0 SIGNIFICA CHE L'ROM E FITTIZIO: IN QUESTO
400000010227     C* CASO FORZO IL PO DEL PADRONCINO
400100010227     C     §LPPOE        IFEQ      *ZEROS
400200010227     C                   MOVEL     §LPPDR        §LPPOE
400300010227     C                   ENDIF
400400010219     C                   Z-ADD     VI4NSR        §LPNSR
400500010223     C                   Z-ADD     VI4NO2        §LPNOR
400600010219     C                   Z-ADD     VI4NRV        §LPNRV
400700010220     C                   ELSE
400800010220     C                   CLEAR                   §LPPOE
400900010220     C                   CLEAR                   §LPNSR
401000010220     C                   CLEAR                   §LPNOR
401100010220     C                   CLEAR                   §LPNRV
401200010220     C                   ENDIF
401300061122     c* Bolla import dpd: cerco su FIDPO00F se presente numero parcel
401400061121     c* se lo trovo passo a fnls01r l'orm recuperato da questo archivio
401500061122     c* e forzo codice bolla = '2'
401600061121     c                   move      §lplnp        worg
401700061121     c                   exsr      ctrorg
401800061122    1c                   if        §ogntw='DPD'
401900061122     c* Cerco numero parcel
402000061122     C                   MOVEL     'E'           WTRC
402100061122     C     KVAT          chain(N)  FIVAT01L
402200061122    2c                   if        %found(fivat01l)
402300061122     c                   movel     vatnot        dvate
402400061122     c     §vateprn      chain     fidpo06l
402500061122     c                   if        %found(fidpo06l)
402600061127     c     korm_dpo      chain     fnorm01l
402700061127     c                   if        %found(fnorm01l)  and ormfcs='F'
402800061122     C                   z-add     dpopoe        §LPPOE
402900061122     C                   z-add     dponrs        §LPNSR
403000061122     C                   z-add     dponor        §LPNOR
403100061122     C                   z-add     dponrv        §LPNRV
403200061122     c                   movel     '2 '          §lpcbo
403300061127     c                   endif
403400061122     c                   endif
403500061122    2c                   endif
403600150909      * Imposto flag traduttore Vecchio/Nuovo se Import Dpd (DBLP01 - §lptrd)
403700150909     c                   movel     vabscl        §lptrd
403800061122    1c                   endif
403900030409
404000021220      * Imposto i campi dei bancali
404100021220     c                   Eval      §LpTba = VabTba
404200021220     c                   Eval      §LpNba = VabNba
404300021220     c                   Eval      §LpTb2 = VabTb2
404400021220     c                   Eval      §LpNb2 = VabNb2
404500150909
404600150909     c                   eval      §lpflo = dblp01
404700021220
404800911031     C                   ENDSR
404900040301      *---------------------------------------------------------
405000040301      * Controllo i caratteri dei campi alfanumerici
405100040301      *---------------------------------------------------------
405200040301     c     Sr_Chk        BegSr
405300040301
405400040301     c                   Eval      TxtInOut = wtxt
405500040301     c                   Call      'XCHKCHAR'
405600040301     c                   Parm                    TxtInOut
405700040301     c                   Parm                    ElencoChar
405800040301     c                   Parm                    TipoElenco
405900040301     c                   Parm                    CharSost
406000040301     c                   Parm                    UpperCase
406100040301     c                   Parm                    ChkNull
406200040301     c                   Parm                    CharNull
406300040301     c                   Parm                    Esito
406400040301     c                   If        Esito = '1'
406500040301     c                   Move      TxtInOut      wtxt
406600040301     c                   EndIf
406700040301
406800040301     c                   EndSr
406900140110      *---------------------------------------------------------
407000140110      * Controllo caratteri di punteggiatura precedenti e seguenti
407100140110      *---------------------------------------------------------
407200140110     c     chkstringa    BegSr
407300140110
407400140110     c* trovo la prima posizione "buona" da utilizzare
407500140110     c                   eval      posl=%check(charnook:wtxt)
407600140110     c* trovo l'ultima posizione "buona" da utilizzare
407700140110     c                   eval      posr=%checkr(charnook:wtxt)
407800140110     c                   if        posl=0
407900140110     c                   clear                   wtxt
408000140110     c                   else
408100140110     c* determino la lunghezza della stringa "buona"
408200140110     c                   eval      len=(posr-posl)+1
408300140110     c                   eval      wtxt=%subst(wtxt:posl:len)
408400140110     c                   endif
408500140110
408600140110     c                   EndSr
408700111010     C*
408800111010     C*-------------------------------------------------------------*
408900111010     C*     -- Annullamento vab/vad richiesto da fnls01r    --      *
409000111010     C*-------------------------------------------------------------*
409100111010     C     ges_ann       BEGSR
409200111010     C*
409300111010     c                   exsr      annorm
409400111010     C                   movel     vi5nrr        WNRR              9 0
409500111010     c                   exsr      annrec
409600111010     c                   endsr
409700111010     C*-------------------------------------------------------------*
409800111010     C*     -- Annullamento orm legato se annullata bolla RC --     *
409900111010     C*-------------------------------------------------------------*
410000111010     C     annorm        BEGSR
410100111010      * Prima di annullare la spedizione se bolla RC vado ad annullare
410200111010      * il relativo ORM legato
410300111010     C                   CLEAR                   DS7R
410400111010    3c                   If        vabgma <>*blanks
410500111010     C                   MOVEL     '7R'          COD
410600111010     C                   MOVEL(P)  vabgma        KEY
410700111010     C     KTAB          CHAIN     TABEL                              30
410800111010     C  N30              MOVEL     TBLUNI        DS7R
410900111010    3c                   endif
411000111010     C*
411100111010    3c                   If        §7rRC='S'
411200111010     c                   Clear                   fnls25ds
411300111010     c                   Eval      i25Ksc = vabccm
411400111010     c                   Move      vabaas        i25aas
411500111010     c                   Eval      i25spe = %editc(vablnp:'X') +
411600111010     c                                      %editc(vabnrs:'X') +
411700111010     c                                      %editc(vabnsp:'X')
411800111010     c                   Call      'FNLS25R'
411900111010     c                   Parm                    Fnls25ds
412000111010    3c                   EndIf
412100111010     c                   endsr
412200030611      *---------------------------------------------------------
412300950112     OFNLV22P   T    LRNLR
412400030611      *---------------------------------------------------------
412500030611**  tes
412600950407 ** CONFERMA BOLLE  DA CLIENTI **
412700950407 ** RECUPERO BOLLE  DA CLIENTI **
412800000518 * CONFERMA BOLLE CLIENTI POSTE *
412900030611**  cm3
413000950112OVRPRTF FILE(FNLV22P) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
413100941011 SHARE(*YES)          USRDTA('Segnacolli')
413200030611**  cmA4
413300030611OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
413400030611 SHARE(*YES)          USRDTA('Boll.Dk A4')
413500030611**  cmA5
413600030611OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
413700030611 SHARE(*YES)          USRDTA('Boll.Dk A5')
413800030611**  err
413900960103Data Spedizione formalmente errata  o  superiore a data sospensione xx/xx/xx
414000020801Impossibile utilizzare cod trattamento merce "xx"  !!
414100990107E' stato forzato il cod trattamento merce: controllare e enter per proseguire
414200990205Codice trattamento merce non forzabile:utilizzare solo codici per soluzioni"A"
414300000518Codice trattamento merce non forzabile
414400010221Immettere il numero O.R.M.
414500010219Numero O.R.M. errato
414600010220Il p.o. di ritiro definito sull'O.R.M. è diverso dal p.o. autotrasportatore    8
414700010220Il numero O.R.M. non deve essere un prepagato                                  9
414800010220Immettere un O.R.M. il cui pagamento non sia a carico del destinatario         0
414900010220O.R.M. in fase non bollettabile. ENTER PER FORZARE                             1
415000010220Se immesso riferimento immettere anche numero O.R.M.                           2
415100010223O.R.M. non utilizzabile in conferma bolle: presente forzatura linea di arrivo  3
415200030409Data cons.richiesta formalmente errata o inferiore/uguale alla data odierna    4
415300051215Data consegna richiesta non ammessa: oltre                                     5
415400030411Immettere o i dati della consegna richiesta o il numero delle ore              6
415500030409Se immesso tipo immettere anche la data o l'ora Consegna Richiesta             7
415600030411Immettere il cliente o la serie se immessi i dati della Consegna Richiesta     8
415700030415Le ore da aggiungere alla delivery devono essere:  24 - 48 - 72                9
415800041122LINEA DI ARRIVO errata                                                         0
415900041222Codice tariffa non valido                                                     21
416000041222E' stato forzato il codice tariffa: controllare e ENTER per proseguire        22
416100060306Cliente con obbligo parzializzazione linea di arrivo                          23
416200080110Data Ritiro Merce errata                                                      24
416300080110Già confermati i conteggi autotrasportatore nella data ritiro richiesta       25
416400130121Non inserire rif.mittente se richiesto ordinamento diverso                    26
416500130121Codice cliente obbligatorio se richiesto ordinamento per RIF.ALFA o NUM.SPED  27
416600110419Immettere l'Autotrasportatore per interrogare l'O.R.M.                        28
416700111214Linea di Partenza Segnacollo errata                                           29
416800111222Presenti più clienti da confermare per la serie richiesta                     30
416900111220Presenti più clienti da confermare per la serie/linea par:immettere Cd.Cliente31
417000130313Utilizzare solo Cod.Tratt.Merce per soluzione "A" o senza Packing List/PDF    32
417100130313Cd.forzabile solo x sped. di clienti con PackingList/PDF non fornito:EnterForz33
