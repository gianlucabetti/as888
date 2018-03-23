000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200130314      * FNLST3R *-----------------------------------------------------*
000300130314      *          Gestione/INTERROGAZIONE FOGLI DI VIAGGIO
000400020806      *---------------------------------------------------------------*
000500130314     FFNLST3D   CF   E             WORKSTN
000600130315     F                                     SFILE(LST3S02:NRR4)
000700130503     FTABEL00F  IF   E           K DISK
000800130503     FAZCLN01L  IF   E           K DISK
000900130319     FTNTLZ01L  IF   E           K DISK
001000130315     FFNFGV01L  IF   E           K DISK
001100130315     FFNFGV02L  IF   E           K DISK
001200940913     F                                     RENAME(FNFGV000:FGV02)
001300020806     FFNFGV06L  IF   E           K DISK
001400020806     F                                     RENAME(FNFGV000:FGV06)
001500951031     F*
001600900515     FAZORG01L  IF   E           K DISK
001700020806     FFNBLP79L  IF   E           K DISK
001800130314     FFNbrv01L  IF   E           K DISK
001900130322     FFNFV201L  IF   E           K DISK
002000130429     FFNFV401L  IF   E           K DISK
002100130322     FFNFV203L  IF   E           K DISK
002200130322     F                                     RENAME(FNFV2000:FV203)
002300020806      *
002400020806      * DEFINIZIONE SCHIERE
002500020806      *
002600960313     D K78             S              1    DIM(78)
002700960313     D K05             S              1    DIM(5)
002800130319     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002900130426     D TES             S             36    DIM(4) CTDATA PERRCD(1)
003000020806      *
003100020806      * DEFINIZIONE STRUTTURE DATI ESTERNE
003200020806      *
003300020806     D KPJBA         E DS
003400020806     D UT§DSE0F      E DS
003500020806     D CNCR80        E DS
003600130319      * DS per TNTL17R - Interrogazione Traini
003700130319     D TNTL00DS      E DS
003800130430     D dvpodecofi    E DS
003900130430     d TIBS02ds      e ds                  inz
004000130503     d ds1Y          e ds
004001130611     d fnlst9ds      e ds
004002130611     d trul90ds      e ds
004100130319     D*-----------------------------------------------------
004200130319      * Parametri per la ricerca trazionisti     - TRUL16R -
004300130319     D PARAM3          DS
004400130319     D* Tipologia T_=TRAZIONISTA
004500130319     D  PA3TIP                 1      2
004600130319     D* Ragione sociale trazionista
004700130319     D  PA3RSC                 3     37
004800130319     D* Sistema inform. trazionista
004900130319     D  PA3CSF                38     40
005000130319     D* Codice trazionista
005100130319     D  PA3PDR                41     47  0
005200130319     D* PA3FLG = "3" --> restituisce al pgm chiamante il messaggio che
005300130319     D*   non esistono records per la chiave alfabetica richiesta
005400130319     D  PA3FLG                48     48
005500020806      *
005600020806      * DEFINIZIONE STRUTTURE DATI INTERNE
005700020806      *
005800130319     D*-----------------------------------------------------
005900020806      * Parametri ricevuti dai pgm chiamanti
006000130314     D fnlst3ds        DS
006100130314     D* PARFLG  = "R" ---> PGM RICHIAMATO per scelta fogli viaggio
006200130314     D* PARFLG  = "I" ---> interrogazione fogli  viaggio
006300130314     D* PARFLG  = "G" ---> Gestione       fogli  viaggio
006400130426     D* PARFLG  = "C" ---> Chiusura       fogli  viaggio
006500130506     D* PARFLG  = "P" ---> interrogazione fogli  viaggio con scelta
006600130314     d*
006700951031     D  PARFLG                 1      1
006800130314     d
006900951123     D* PARFL2  = "F" ---> PASSO AL PGM CHIAMANTE I FV  SELEZIONATI
007000951123     D*                    NELLA SKIERA FVA
007100951123     D*                    SE NE VOGLIO SELEZIONARE SOLO 1 :
007200951123     D*                    PARFL2=" " E VIENE PASSATO IN PARNFV
007300951102     D  PARFL2                 2      2
007400020806     D* Numero Foglio Viaggio: se è pieno significa che è richiesta
007500020806     D*   la visualizzazione di quello specifico foglio
007600951031     D  PARNFV                 3      5P 0
007700020806     D* Data iniziale e finale in GG/MM/AAAA
007800951031     D  PARDIN                 6     10P 0
007900951031     D  PARDFI                11     15P 0
008000020806     D* PARTFV è il tipo FV da visualizzare:  " " = FV TUTTI
008100951031     D*                                       "1" = FV PARTITI (CHIUSI)
008200951031     D*                                       "2" = FV NON PARTITI (APERTI)
008300951031     D  PARTFV                16     16
008400020806     D* FVA è la schiera contenente tutti i FV selezionati
008500020806     D*        (da riempire solo se PARFL2 = "F")
008600951031     D  FVA                   17    241P 0
008700951031     D                                     DIM(45)
008800000609     D  PARFGS               242    244  0
008900020806      *
009000130319     D*-----------------------------------------------------
009100130318      * Parametri per pgm FNLST4R
009200130318     D fnlst4ds        DS
009300130318     D* ilst4gES = M - MANUTENZIONE FV RICHIAMATA
009400130318     D*          B - DA BORDERO'
009500130318     D*          A - apertura foglio
009600130318     D*          C - DA CHIUSURA FV SINGOLA
009700130318     D*          I - DA CHIUSURA DA IMP
009800130318     D*          N - DA INTERROGAZIONE  FV
009900130419     D*          M - Gestione fogli viaggio
010000130419     D*          P - Gestione fogli viaggio con videata piombi
010100130603     d
010200130603     d* solo per un po' N=nuova chiusura
010300130603     D  ilst4NEWC              2      2
010400130319     c* La data serve solo per immissione
010500130319     D  ilst4dfv               3     10  0
010600130319     d
010700130319     D  ilst4nfv              11     15
010800130318     D  IlsT4GES              16     16
010900130318     D* Se PARMSG pieno --> c'è stato ERRORE
011000130318     D  olst4msg              17     94
011100130429     D  iolsT4FOR             95     97
011200130429     D  IlsT4OLD              98     98
011300130318
011400130318     D* F12 = 'S'    --> premuto F12
011500130318     D  olst4F12             102    102
011600130318     D  ilst4fgs             103    105
011700130319 i   D  ilst4TRN             106    112  0
011800130318     D  ilst4NRRP            113    125
011900130319     D* se non immessa il numero relativo record della proposta
012000130319     d*  si può immettere a modo vecchio il trazionista
012100130319     D  ilst4PDR             126    132
012200130315     d*
012300130319     D*-----------------------------------------------------
012400130315     d* Richiamo a pgm di proposte traini
012500130315     D FNLST1DS        DS
012600130319     d* 'A' --> ricerca proposte per apertura FGV
012700130315     D  Ilst1GES               1      1
012800130315     D  Ilst1DAT               2      9
012900130315     D* 'G' --> caricare solo le proposte traini del giorno della settimana
013000130315     D*         corrispondente alla data;
013100130315     D* ' ' --> caricare tutte le proposte/traini alla data decorrenza
013200130315     D  Ilst1TUT              10     10
013300130315     D  Ilst1fgs              11     13
013400130417     d* scelta automatica del programma senza proposta videata
013500130417     D  Ilst1aut              14     14
013600020806      *
013700900515     D WLBDAT          DS
013800940913     D  G02DAT                 1      8  0
013900940913     D  G02INV                 9     16  0
014000940913     D  G02ERR                17     17
014100940913     D  G02TGI                18     22  0
014200040611      *
014300040611      * STATUS DS
014400040611      *
014500040611     d Status         sds
014600040611     d   SDSprm          *parms
014700020806      *
014800130503     D                 DS
014900130503     D  CLNPOM                 1     31
015000130503     D  POM                    1     31
015100130503     D                                     DIM(31)
015200130503     D                 DS
015300130503     D  CLNMAT                 1     31
015400130503     D  MAT                    1     31
015500130503     D                                     DIM(31)
015600130503     D                 DS
015700130503     D  GG                     1      2  0
015800130503     D  MM                     3      4  0
015900130503     D  AA                     5      8  0
016000130503     D  WDATA                  1      8  0
016100020806      * VARIABILI
016200020806      *
016300130503     D COD             S                   LIKE(TBLCOD)
016400130503     D KEY             S                   LIKE(TBLKEY)
016500130503     D KTFP            S                   LIKE(FGVLNA)
016600130503     D KTFA            S                   LIKE(FGVLNA)
016700020806     D WDATE           S                   LIKE(FGVDFV)
016800130426     D Knfv6           S                   LIKE(brvnfv) INZ
016900130426     D Knfv5           S                   LIKE(fgvnfv) INZ
017000020806     D DATFIW          S                   LIKE(FGVDFV) INZ
017100020806     D DATINW          S                   LIKE(FGVDFV) INZ
017200130314     D savFIW          S                   LIKE(FGVDFV) INZ
017300130314     D savINW          S                   LIKE(FGVDFV) INZ
017400130322     D savrtp          S                   LIKE(vidrtp) INZ
017500130322     D savrta          S                   LIKE(vidrta) INZ
017600130426     D savtfog         S                   LIKE(tipfog) INZ
017700130503     D KANN            S                   LIKE(CLNANN)
017800130503     D KMES            S                   LIKE(CLNMES)
017900130314     D knpg            S                   LIKE(brvnpg) INZ(1)
018000130322     D KEPA            S                   LIKE(FV2EPA)
018100130322     D KTDH            S                   LIKE(FV2TDH)
018200130322     D KLAI            S                   LIKE(FV2LAI)
018300130429     D KTRC            S                   LIKE(FV4trc)
018400020806     D WTIME           S             14  0
018500020806     D NRR4            S              4  0 INZ
018600020806     D XZ              S              3  0 INZ
018700020806     D W0020           S              2  0 INZ
018800020806     D WRTA            S              1    INZ
018900041129     D Werrore         S              1    INZ
019000130430     D alfaFGS         S              3    INZ
019100130319     D KTLTIP          S                   LIKE(TLZTIP) INZ('T ')
019200130319     D KTLPDR          S                   LIKE(TLZPDR)
019300130319     D KPDR            S                   LIKE(fgvPDR)
019400130503     d Datasys         s               d   datfmt(*eur) inz(*sys)
019500130503     d wdtaeur         s               d   datfmt(*eur)
019600130503     d wdtaiso         s               d   datfmt(*iso)
019700130503     d wdtameno1       s              8  0
019800130503     d wdtameno2       s              8  0
019900130503     d wdtapiu1        s              8  0
020000130503     d wgiorno         s              2  0
020001130611     D wrichtrul90     S              1
020002130612     D ww02            S              1
020100040611      *
020200040611      * Parametri aggiuntivi:
020300040611      * - P.O. di cui selezionare i F.V. (da FIMS03R)
020400040611     d DLSpo           s              3
020500040630     d DLSpo2          s              3
020600040611     d wDLSpo          s              3  0
020700040630     d wDLSpo2         s              3  0
020800020806
020900130430      //---------------------------------------------------------------
021000130430      /copy gaitrasrc/srcprotopr,tibs02r
021100130430
021200020806      *****************************************************************
021300020806      *  RIEPILOGO INDICATORI
021400020806      *****************************************************************
021500130314      * 02    - interrogazione   OFF - gestione
021600130426      * 05    - Chiusura foglio viaggio singola
021700130314      * 06    - FOGLIO VIAGGIO IN GESTIONE AD ALTRO P.O.
021800020806      * 10    - ON SELEZIONE PER DATA
021900020806      * 11    - RICHIESTA LA VISUALIZZAZIONE DEL FOGLIO VIAGGIO PASSATO
022000020806      *         DAL PGM CHIAMANTE
022100020806      * 13    - VISUALIZZO SOLO I F.V. APERTI
022200020806      * 14    - P.O. GESTIONE ERRATO
022300130314      * 20    - PROGRAMMA RICHIAMATO PER SCELTA FOGLI VIAGGIO
022400130523      * 21    - proteggo scelte nel sfl ctl
022500020806      * 22    - DEVO PASSARE AL PGM CHIAMANTE TUTTI I F.V. SELEZIONATI
022600130429      * 25    - Reverse immage per pimobi non inseriti
022700130523      * 26    - Reverse immage per targa rimorchio  non inserita
022800020806      * 28    - INDICATORE DI EMISSIONE MESSAGGIO DI ERRORE
022900020806      * 29    - READC SUBFILE
023000020806      * 31/33 - DI COMODO
023100020806      * 35    - TIPO UTENTE
023200020806      * 36    - SFLNXTCHG
023300020806      * 40    - ERRORE
023400020806      * 48    - ERRORE
023500020806      * 72/73 - SFLCLR SFLDSP E SFLDSPCTLM           SFL04
023600020806      * 76/77 - ERRORE
023700020806      * 90    - INDICATORE DI ERRORE GENERICO
023800020806      * 95    - COMODO
023900130430      * 99    - Errore grave: esco dal pgm
024000020806      *****************************************************************
024100130314     c
024200130314     C                   EXSR      PULIZ
024300130314     c
024400130426    1c                   select
024500130426     c* INTERROGAZIONE
024600130506     c                   when      parflg='I' or parflg='P'
024700130314     c                   seton                                        02
024800130417     C                   MOVE      TES(2)        VIDTES
024900130506     c                   if        parflg='P'
025000130506     C                   SETON                                        20
025100130506     c                   endif
025200130314     C
025300130426     c* CHIUSURA FOGLI VIAGGIO
025400130426     c                   when      parflg='C'
025500130426     c                   seton                                        0205
025600130426     C                   MOVE      TES(4)        VIDTES
025700130426     c                   eval      tipfog='2'
025800130603     c* se parfl2='N' significa che devo sottomettere il nuovo pgm di
025900130603     c*  chiusura FGV
026000130426     C
026100130314     c                   when      parflg='G'
026200130314     c                   setoff                                       02
026300130314     c
026400130506     c* RICERCA FOGLI
026500130314     c                   when      parflg='R'
026600130417     C                   MOVE      TES(3)        VIDTES
026700940323     C                   SETON                                        20
026800951106     C                   Z-ADD     PARDIN        VIDDIN
026900951106     C                   Z-ADD     PARDFI        VIDDFI
027000951102     C                   MOVEL     PARTFV        TIPFOG
027100000609     C                   Z-ADD     PARFGS        VIDFGS
027200130523     c
027300130523     c* se ho passato partfv = "2" proteggo le scelte perchè non mi servono
027400130523     c                   if        partfv='2'
027500130523     c                   seton                                        21
027600130523     c                   endif
027700130523     c
027800130314     c* controllo le date passate
027900130314     c                   exsr      ctrd02
028000130314     c* se errore --> esco
028100130314     c                   if        *in90
028200130314     c                   goto      fine
028300130314     c                   endif
028400130314     C*
028500130314     c* CONTROLLO SE DEVO PASSARE AL PGM CHIAMANTE TUTTI I FOGLI DI
028600130314     C*   VIAGGIO SELEZIONATI (PARFL2 = "F")
028700130314    2C     PARFL2        IFEQ      'F'
028800130314     C                   SETON                                        22
028900130314     C                   MOVEA     *ZEROS        FVA
029000130314    2C                   ENDIF
029100130314     C
029200130314    1c                   ENDSL
029300951031     C*
029400130314     C     INIZIO        TAG
029500130314     C                   EXSR      RIES04
029600951031     C*
029700951031     C* VISUALIZZAZIONE SFL E SCELTA FOGLIO
029800020806     C                   Z-ADD     1             XZ
029900960313     C                   Z-ADD     1             REC4
030000020806     C*    =================
030100951031     C     FORCT         TAG
030200020806     C*    =================
030300130314     C                   WRITE     LST3T01
030400130314     C                   WRITE     LST3Z02
030500130314     c* se non caricati fogli emetto videata
030600130315     C  n73              WRITE     LST3d03
030700130314
030701130612     c* se immissione multipla con f15 emissione window per richiesta
030702130612     c* stampa etichette
030703130612     c                   if        ww02='S'
030704130612     c                   clear                   ww02
030705130612     C                   write     LST3C02
030706130612     c                   exsr      geswdw02
030707130612     c                   endif
030708130612     c*
030800130314     C                   EXFMT     LST3C02
030900130419     C                   SETOFF                                       284048
031000960313     C                   CLEAR                   VIDMSG
031100951031     C*
031200130314     C* F3 o F12 - Fine/Ritorno
031300130430     c*            o per errore grave esco
031400130314    1c                   if        *inKC or *inKL
031500130430     c                             or *in99
031600130314     C                   GOTO      FINE
031700130314    1c                   endif
031800130503     c* controllo le selezioni immesse
031900130503     c                   exsr      Ctrd02
032000130503     c* Errore
032100130503    1c                   if        *in90
032200130503     c                   goto      forct
032300130503    1c                   endif
032400130430    c
032500130419     c* F13 - Immissione multipla con   videata
032600130419     c* F15 - Immissione multipla SENZA videata
032700130419    1c                   if        *inKP or *inkM
032800130314     c                   exsr      RicercaTRN
032801130612     c                   if        *inkp
032802130612     c                   eval      ww02='S'
032803130612     c                   endif
032900130314     c                   goto      inizio
033000130314    1c                   endif
033100130419    c
033200130417     c* F10 - Immissione singola  non dovrebbe servire ...
033300130417    1c                   if        *inKJ and not *inkj
033400130314     c                   exsr      Immissione
033500130314     c                   goto      inizio
033600130314    1c                   endif
033700130419    c
033800130419     c* F10 - Immissione singola  senza copia traino
033900130419    1c                   if        *inKJ
034000130419     c                   clear                   fnlst4ds
034100130419     c                   eval      ilst4ges='A'
034200130419     c                   eval      ilst4fgs =%editc(vidfgs:'X')
034300130419     c                   eval      ilst4dfv=datinw
034400130419     c                   eval      ilst4old='S'
034500130419     c                   eval      kpjbu=fnlst4ds
034600130419     c                   call      'FNLST4R'
034700130419     c                   parm                    kpjba
034800130419     c                   eval      fnlst4ds=kpjbu
034900130419     c
035000130419     c                   if        olst4msg<>*blanks
035100130419     c                   movel     olst4msg      vidmsg
035200130419     c                   seton                                        2848
035300130419     c                   goto      forct
035400130419     c                   else
035500130419     c                   goto      inizio
035600130419     c                   endif
035700130419     c
035800130419    1c                   endif
035900130314     c
036000130314     c
036100130314     c* se scelto un foglio lo visualizzo
036200130314    1c                   select
036300130314     c*  se cambiate date --> ricarico
036400130314     c                   when      vidnfv>0
036500130419     c                   if        not *in20 and not *in02
036600130315     c                   eval      vidsce='2'
036700130419     c                   endif
036800130506     c                   if        *in02     and not *in05
036900130419     c                   eval      vidsce='5'
037000130419     c                   endif
037100130506     c                   if        *in05
037200130506     c                   eval      vidsce='2'
037300130506     c                   endif
037400130314     c                   exsr      VisualFGV
037500130419     c                   if        vidmsg<>*blanks
037600130419     c                   seton                                        289048
037700130419     c                   else
037800130314     c                   clear                   vidnfv
037900130419     c                   endif
038000130314     c                   goto      forct
038100130314
038200130314     c
038300130314     c                   when      datfiw<>savfiw or
038400130314     c                             datinw<>savinw
038500130314     c                   goto      inizio
038600130426     c
038700130322     c                   when      vidrtp<>savrtp or
038800130322     c                             vidrta<>savrta
038900130322     c                   goto      inizio
039000130426
039100130426     c                   when      tipfog<>savtfog
039200130426     c                   goto      inizio
039300130314     c
039400130314     c* Se immesso numero foglio --> lo visualizzo
039500951031     C*
039600130314   x1c                   other
039700130315     c                   z-add     1             nrr4
039800130430     c
039900130430     c* e non ho caricato fogli non eseguo la lettura
040000130430   1dc                   if        *in73
040100951031     C     LEGGI         TAG
040200130315     C                   READC     LST3S02                                29
040300951031     C*
040400951031     C* SE SELEZIONATO ALMENO UNO ESCO ALTRIMENTI RIEMETTO SUBFILE
040500130315    1c                   if        *in29
040600130315    2C                   IF        *in22  and XZ >  1
040700951031     C                   GOTO      FINE
040800951031    2C                   ENDIF
040900951031     C*
041000130419     C                   GOTO      forct
041100130315    1c                   endif
041200960126     C**
041300130419     c* Per gestione ammessa solo opzione 2 / 5 / P
041400130315   1ac                   if        vidsce<>' '
041500130430
041600130430     c* Se chiusura foglio e scelta con "2" --> controllo se ha bolla
041700130430     c**   o spunte il foglio
041800130426     c
041900130426     c                   if        not *inkn and vidsce='2' and *in05
042000130426     c                   eval      knfv5=fgvnfv
042100130426     c                   exsr      contrbolle
042200130426     c                   if        *in90
042300130426     c                   seton                                        3640
042400130426     C                   Z-ADD     NRR4          REC4
042500130429     c                   exsr      AggioSFL
042600130426     C                   GOTO      forct
042700130426     c                   endif
042800130426     c                   endif
042900130502     c
043000130502     c                   if        *in05 and vidsce<>'2' and vidsce<>'5'
043100130502     c                   seton                                        369040
043200130502     c                   seton                                        28
043300130502     C                   Z-ADD     NRR4          REC4
043400130502     C                   MOVEA     MSG(12)       VIDMSG
043500130502     c                   exsr      AggioSFL
043600130502     C                   GOTO      forct
043700130502     c                   endif
043800130419     c
043900130314     c                   if        parflg='G' and vidsce<>'2'
044000130315     c                             and vidsce<>'5'and vidsce<>' '
044100130419     c                             and vidsce<>'P'
044200130314     c                   seton                                        369040
044300130314     c                   seton                                        28
044400130315     C                   Z-ADD     NRR4          REC4
044500130315     C                   MOVEA     MSG(12)       VIDMSG
044600130429     c                   exsr      AggioSFL
044700130314     C                   GOTO      forct
044800130314     c                   endif
044900130419     c
045000130314     c* Per Interrogazione solo opzione 5
045100130314     c                   if        parflg='I' and
045200130315     c                                 vidsce<>'5'  and vidsce<>' '
045300130314     c                   seton                                        369040
045400130314     c                   seton                                        28
045500130315     C                   MOVEA     MSG(12)       VIDMSG
045600130315     C                   Z-ADD     NRR4          REC4
045700130429     c                   exsr      AggioSFL
045800130314     C                   GOTO      forct
045900130314     c                   endif
046000130506     c                   if        parflg='P' and   vidsce<>'1' and
046100130506     c                                 vidsce<>'5'  and vidsce<>' '
046200130506     c                   seton                                        369040
046300130506     c                   seton                                        28
046400130506     C                   MOVEA     MSG(12)       VIDMSG
046500130506     C                   Z-ADD     NRR4          REC4
046600130506     c                   exsr      AggioSFL
046700130506     C                   GOTO      forct
046800130506     c                   endif
046900130315     c
047000130315     c* Per Ricerca        solo opzione 1
047100130314     c                   if        parflg='R' and
047200130315     c                                 vidsce<>'1'  and vidsce<>' '
047300130314     c                   seton                                        369040
047400130314     c                   seton                                        28
047500130315     C                   Z-ADD     NRR4          REC4
047600130315     C                   MOVEA     MSG(12)       VIDMSG
047700130429     c                   exsr      AggioSFL
047800130314     C                   GOTO      forct
047900130314     c                   endif
048000130314     c*
048100130314    2c                   if        vidsce='2' or  vidsce='5'
048200130419    2c                             or vidsce='P'
048300130314     c                   exsr      visualFGV
048400130419     c
048500130315     C                   Z-ADD     NRR4          REC4
048600130314     c* Se ce' errore devo riemettere sfl
048700130429     c* se non c'e' errore riaggiorno sfl con i dati del foglio
048800130419     c                   if        not *in90
048900130419     C                   MOVEL     ' '           VIDSCE
049000130419     c     kfgvsfl       chain     fnfgv01l
049100130419     c                   exsr      rigaSFL
049200130419     c                   endif
049300130419     c
049400130314     c                   seton                                        3640
049500130429     c                   exsr      AggioSFL
049600130315     c                   if        *in90
049700130314     C                   GOTO      forct
049800130315     c                   endif
049900130314     C*
050000130314     C                   GOTO      LEGGI
050100130314    2C                   ENDIF
050200960318     C*
050300130314     C* SE scelta    SELEZIONO IL FOGLIO CON L'OPZIONE "1"
050400130506    2c                   if        vidsce='1'
050500960126     C* PULISCO IL CAMPO DI SCELTA
050600020806     C                   CLEAR                   VIDSCE
050700130429     c                   exsr      AggioSFL
050800130314     c
050900960126     C* 22 OFF - PASSO UN SOLO NUMERO DI FV
051000130314    3C     *IN22         IFEQ      *OFF
051100960126     C                   Z-ADD     FGVNFV        PARNFV
051200020806     C                   z-add     VIDfgs        PARfgs
051300960126     C                   GOTO      FINE
051400960313     C*
051500130314   X3C                   ELSE
051600951031     C* 22 ON  - MEMORIZZO TUTTI I FV SELEZIONATI
051700020806    5C     XZ            IFLE      45
051800020806     C                   z-add     VIDfgs        PARfgs
051900020806     C                   Z-ADD     FGVLNA        FVA(XZ)
052000020806     C                   MOVEL     FGVNFV        FVA(XZ)
052100020806     C                   ADD       1             XZ
052200960126     C                   GOTO      LEGGI
052300960313   X5C                   ELSE
052400960313     C* SE SONO STATI SELEZIONATI PIU' DI 45 F.VIAGGIO: ERRORE
052500020806     C                   SUB       1             XZ
052600960313     C*
052700960313     C                   MOVEA     MSG(5)        K78
052800020806     C                   MOVEL     XZ            W003A             3
052900960313     C                   MOVEA     W003A         K05(3)
053000960313     C* ABBLANCO ZERO NON SIGNIFICATIVI
053100020806     C                   Z-ADD     3             XZ
053200020806    6C     K05(XZ)       DOWEQ     '0'
053300020806     C     XZ            ANDLT     5
053400020806     C                   CLEAR                   K05(XZ)
053500020806     C                   ADD       1             XZ
053600960313    6C                   ENDDO
053700960313     C*
053800960313     C                   MOVEA     K05(3)        K78(35)
053900960313     C                   MOVEA     K78           VIDMSG
054000960313     C                   SETON                                        4028
054100960313     C                   Z-ADD     NRR4          REC4
054200960313     C*
054300130315     C     NRR4          CHAIN     LST3S02                            33
054400960313     C                   MOVEL     '1'           VIDSCE
054500130429     c                   exsr      AggioSFL
054600960313     C                   GOTO      FORCT
054700960313    5C                   ENDIF
054800960313     C*
054900960126    3C                   ENDIF
055000960126    2C                   ENDIF
055100130430   1aC                   ENDIF
055200951031     C*
055300951102     C* PULISCO IL CAMPO DI SCELTA
055400130419     c                   if        vidsce<>' '
055500130419     C                   SETON                                        4036
055600130419     c                   else
055700130419     C                   SETOff                                       40
055800130419     c                   endif
055900130419     c
056000130419     C                   CLEAR                   VIDSCE
056100130315     c                   setoff                                       36
056200130429     c                   exsr      AggioSFL
056300130430   1bC                   ENDIF
056400130314    1C                   ENDsl
056500951031     C*
056600130430     c   73              goto      leggi
056700130430     c  n73              goto      forct
056800130314     c
056900000000     C     FINE          TAG
057000130315     C   20              MOVEL     fnlst3ds      KPJBU
057001130611
057002130611     C                   Z-ADD     130           LUNG             15 5
057003130611     c                   eval      comman='DLTOVR FILE(*ALL)'
057004130611     C                   CALL      'QCMDEXC'                            91
057005130611     C                   PARM                    COMMAN
057006130611     C                   PARM                    LUNG
057007130611
057100000000     C                   SETON                                        LR
057200951031     C*-----------------------------------------------------***********
057300951031     C* PULIZIA CAMPI VIDEO                                 *  PULIZ  *
057400951031     C*-----------------------------------------------------***********
057500900515     C     PULIZ         BEGSR
057600000620     C                   SETOFF                                       10  15
057700951102     C                   CLEAR                   VIDNFV
057800041129     C                   z-add     wdate         VIDDIN
057900940913     C                   Z-ADD     WDATE         VIDDFI
058000951102     C                   CLEAR                   TIPFOG
058100130322     C                   CLEAR                   VIDRTP
058200130322     C                   CLEAR                   VIDRTA
058300130319     c                   exsr      ctrd02
058400900514     C                   ENDSR
058500130429     C*-----------------------------------------------------***********
058600130429     C* Aggioranemto SFL                                    *  PULIZ  *
058700130429     C*-----------------------------------------------------***********
058800130429     C     AggioSFL      BEGSR
058900130429     c                   if        vidpr_p='NO'
059000130429     c                   seton                                        25
059100130429     c                   endif
059200130523     c                   if        vidpr_t='NO'
059300130523     c                   seton                                        26
059400130523     c                   endif
059500130429     c
059600130429     C                   UPDATE    LST3S02
059700130429     c                   setoff                                       364025
059800130523     c                   setoff                                       26
059900130523     c
060000130429     c                   ENDSR
060100951031     C*-----------------------------------------------------***********
060200130314     C* CARICAMENTO  FOGLIO VIAGGIO
060300951031     C*-----------------------------------------------------***********
060400951031     C     RIES04        BEGSR
060500951031     C*
060600940912     C                   Z-ADD     1             NRR4
060700900517     C                   SETON                                        72
060800130315     C                   WRITE     LST3C02
060900911205     C                   SETOFF                                       7213
061000130430     c*
061100130430     c* Se errore grave --> esco dalla routine
061200130430     c                   if        *in99
061300130430     c                   leavesr
061400130430     c                   endif
061500130430     c
061600130430     C                   SETON                                        73
061700911205     C*
061800951031     C* 13 ON  - VISUALIZZO SOLO I FV APERTI
061900951102    1C     TIPFOG        IFEQ      '2'
062000911205     C                   SETON                                        13
062100951031    1C                   ENDIF
062200951031     C*
062300951031     C* CARICO IL SUBFILE
062400130314    1c                   if        not *in13
062500130314     c     KFGV02        setll     FNFGV02L
062600130314     c     vidfgs        reade     FNFGV02L                               31
062700020806     c                   else
062800130314     c     vidfgs        setll     FNFGV06L
062900130314     c     vidfgs        reade     FNFGV06L                               31
063000130314    1c                   endif
063100951031     C*
063200951031    1C     *IN31         DOWEQ     *OFF
063300041130     c     nrr4          andle     9990
063400951031     C*
063500951031    2C     FGVDFV        IFLE      DATFIW
063600900529     C                   SETOFF                                       95
063700951031     C*
063800951031    3C   13FGVDFV        IFGE      DATINW
063900911205     C                   SETON                                        95
064000951031    3C                   ENDIF
064100951031     C*
064200960219     C                   SELECT
064300960219     C     TIPFOG        WHENEQ    '1'
064400020806    3C     FGVFCF        ANDNE     *BLANKS
064500900529     C                   SETON                                        95
064600951031     C*
064700020806    3C     TIPFOG        WHENEQ    *BLANKS
064800900529     C                   SETON                                        95
064900960219    3C                   ENDSL
065000040611      *
065100040611      * Verifico P.O. arrivo
065200040630    3c                   if             *in95   =  *on
065300040630     c                             AND (wDLSpo  <> *zeros
065400040630     c                             and  wDLSpo  <> FGVlna
065500040630     c                             and  wDLSpo2 =  *zeros
065600040630     c                              OR  wDLSpo2 <> *zeros
065700040630     c                             and  wDLSpo  <> FGVlna
065800040630     c                             and  wDLSpo2 <> FGVlna)
065900040611     c                   eval      *in95 = *off
066000040611    3c                   endif
066100130322     c
066200130322     C* VERIFICO CON IL RITARDO PARTENZA
066300130322    3C   95VIDRTP        IFNE      *BLANKS
066400130322     C* CHAIN SU FV2
066500130322     C                   MOVEL     'P'           KEPA
066600130322     C                   CLEAR                   KLAI
066700130322     C                   MOVEL     'R'           KTDH
066800130322     C     KFV2          CHAIN     FNFV201L                           32
066900130322    4C     *IN32         IFEQ      *OFF
067000130322     C     FV2ATB        ANDEQ     *BLANKS
067100130322     C     FV2RTC        ANDNE     *BLANKS
067200130322     C* SE HO CHIESTO I FV SENZA RITARDO ERRORE
067300130322    5C     VIDRTP        IFEQ      'N'
067400130322     C                   SETOFF                                       95
067500130322    5C                   ENDIF
067600130322   X4C                   ELSE
067700130322     C*
067800130322     C* SE HO CHIESTO I FV CON   RITARDO ERRORE
067900130322    5C     VIDRTP        IFEQ      'R'
068000130322     C                   SETOFF                                       95
068100130322    5C                   ENDIF
068200130322    4C                   ENDIF
068300130322    3C                   ENDIF
068400130322     C**
068500130322     C* VERIFICO CON IL RITARDO ARRIVO
068600130322    3C   95VIDRTA        IFNE      *BLANKS
068700130322     C* CHAIN SU FV2
068800130322     C                   CLEAR                   WRTA
068900130322     C                   MOVEL     'A'           KEPA
069000130322     C                   MOVEL     'R'           KTDH
069100130322     C     KFV3          CHAIN     FNFV203L                           32
069200130322    4C     *IN32         DOWEQ     *OFF
069300130322     C*
069400130322    5C     FV2ATB        IFEQ      *BLANKS
069500130322     C     FV2RTC        ANDNE     *BLANKS
069600130322     C                   SETON                                        32
069700130322     C* SE HO CHIESTO I FV SENZA RITARDO ERRORE
069800130322    6C     VIDRTA        IFEQ      'N'
069900130322     C                   SETOFF                                       95
070000130322   X6C                   ELSE
070100130322     C* SE HO CHIESTO I FV CON RITARDO SOLO A POSTO
070200130322     C                   MOVEL     'S'           WRTA
070300130322    6C                   ENDIF
070400130322    5C                   ENDIF
070500130322     C*
070600130322     C  N32KFV3          READE     FNFV203L                               32
070700130322    4C                   ENDDO
070800130322     C*
070900130322    4C     VIDRTA        IFEQ      'R'
071000130322     C     WRTA          ANDEQ     *BLANKS
071100130322     C                   SETOFF                                       95
071200130322    4C                   ENDIF
071300130322    3C                   ENDIF
071400960219     C**
071500960219     C**
071600951031     C* 95 ON  - TUTTO OK: RECORD DA VISUALIZZARE
071700951031    3C     *IN95         IFEQ      *ON
071800951031     C*
071900951031     C* CAMPO SELEZIONE
072000020806     C                   CLEAR                   VIDSCE
072100130314     C
072200130419     c                   exsr      RigaSFL
072300130315     c
072400130315     c* La prima volta accedo indicatore di posizionamento ccursore
072500130419     c                   if        nrr4=1
072600130419     c                   seton                                        4036
072700130419     c                   endif
072800951031     C*
072900130315     C                   WRITE     LST3S02
073000940912     C                   ADD       1             NRR4
073100130430     c                   setoff                                       403625
073200130523     c                   setoff                                       26
073300951031    3C                   ENDIF
073400951031     C*
073500020806     c                   if        not *in13
073600130314     c     vidfgs        reade     FGV02                                  31
073700020806     c                   else
073800130314     c     vidfgs        reade     FGV06                                  31
073900020806     c                   endif
074000951031     C*
074100951031   X2C                   ELSE
074200020806     C*
074300020806     c                   if        not *in13
074400020806     c                   eval      *in31 = *on
074500020806     c                   else
074600130314     c     vidfgs        reade     FGV06                                  31
074700020806     c                   endif
074800020806     C*
074900951031    2C                   ENDIF
075000951031     C*
075100951031    1C                   ENDDO
075200130314
075300041129     c                   eval      werrore='N'
075400130314     c* Se caricati troppi record --> segnalo l'errore
075500041130     c                   if        *in31 =*off and nrr4 >9990
075600041129     c                   eval      werrore='S'
075700130314     C                   MOVEL     MSG(11)       VIDMSG
075800130314     C                   SETON                                        28
075900041129     c                   endif
076000951031     C*
076100130426     c* salvo i campi di ricaricamento sfl
076200130314     c                   eval      savfiw=datfiW
076300130314     c                   eval      savinw=datinW
076400130322     c                   eval      savrtp=vidrtp
076500130322     c                   eval      savrta=vidrta
076600130426     c                   eval      savtfog=tipfog
076700130315    1c                   if        nrr4=1
076800130315     c                   setoff                                       73
076900130315     c                   endif
077000130315     c
077100951031     C                   ENDSR
077200130419     C*-----------------------------------------------------***********
077300130419     C* Carico dati nella riga del sfl
077400130419     C*-----------------------------------------------------***********
077500130419     c     RigaSFL       BEGSR
077600130523     c                   setoff                                       2526
077700130419     C* GIRO DATA FOGLIO VIAGGIO
077800130419     C                   Z-ADD     FGVDFV        G02INV
077900130419     C                   MOVEL     *ZERO         G02DAT
078000130419     C                   MOVEL     '3'           G02ERR
078100130419     C                   CALL      'XSRDA8'
078200130419     C                   PARM                    WLBDAT
078300130419     C                   Z-ADD     G02DAT        W0020
078400130419     C                   MOVEL     G02DAT        VIDDEL
078500130419     C                   MOVE      W0020         VIDDEL
078600130419     C* DECODIFICO LINEA DI ARRIVO
078700130419     C     FGVLNA        CHAIN     AZORG                              31
078800130419     C  N31              MOVEL     ORGDES        DE2LNA
078900130419     C   31              MOVEL     *BLANKS       DE2LNA
079000130419     C* NUMERO TRAINO
079100130419     C                   MOVEL     FGVTRN        VIDTRN
079200130419     C*
079300130419     C                   MOVEL     *BLANKS       VIDABP
079400130419     C                   MOVEL     *BLANKS       VIDimp
079500130419     c
079600130419     c                   clear                   vidabp
079700130419     C* ANNULLATO
079800130419    4C     FGVATB        IFEQ      '*'
079900130429     C                   MOVEL     'Ann   '      VIDABP
080000130419    4C                   ENDIF
080100130419     C*
080200130419     C* BORDERO'
080300130419     C     K02BLP79      setll     FNBLP79L                               31
080400130429     C   31              MOVE      'B     '      VIDABP
080500130419     c* Verifico se presenti spunte
080600130426     c                   eval      knfv6=fgvnfv
080700130419     C     Kbrv          setll     FNBrv01l                               31
080800130429     c   31              eval      %subst(vidabp:3:2)='Sp'
080900130419     C*
081000130419     C* PARTITO
081100130419    4C     FGVFCF        IFNE      *BLANKS
081200130419     C                   MOVE      'P'           VIDABP
081300130419    4C                   ENDIF
081400130419     c* Verifico che chiusura da IMP
081500130419     c                   if        fgvnfa>0
081600130419     c                   eval      vidimp='DaIMP'
081700130419     c                   endif
081800130429     c* Verifico la presenza dei piombi
081900130429     c* Escludo fogli defluenza e locali
082000130429     c                   eval      vidpr_p='  '
082100130523     c                   eval      vidpr_t='  '
082200130429     c                   if        fgvttr= ' ' or fgvttr='S'
082300130429     c                                         or fgvttr='H'
082400130429     C                   MOVEL     'P'           KEPA
082500130429     C                   CLEAR                   KLAI
082600130429     C                   MOVEL     'P'           KTrc
082700130429     C     KFV4          CHAIN     FNFV401L
082800130429    4C                   IF        %found(fnfv401l) and fv4atb=' '
082900130429     c                             and fv4not<>*blanks
083000130429     c                   else
083100130429     c                   eval      vidpr_p='NO'
083200130429     c                   seton                                        25
083300130429     c                   endif
083400130523     c
083500130523     c* Targa Rimorchio
083600130523     c                   if        fgvtrr=*blanks
083700130523     c                   eval      vidpr_t='NO'
083800130523     c                   seton                                        26
083900130523     c                   endif
084000130523     c
084100130429     c                   endif
084200130419     C*
084300130419     C* TARGHE MOTRICE E RIMORCHIO
084400130419     C                   MOVEL     FGVTRM        VIDTRM
084500130523     C*****              MOVEL     FGVTrr        VIDTrr
084600130419     c                   ENDSR
084700951031     C*-----------------------------------------------------***********
084800130314     C* CONTROLLI dati di parzializzazione
084900951031     C*-----------------------------------------------------***********
085000130314     C     CTRD02        BEGSR
085100940913     C                   SETOFF                                       907677
085200000612     C                   SETOFF                                       1406
085300000609     C*
085400000609     C* CONTROLLO DATA INIZIALE
085500951031    1C     VIDDIN        IFNE      0
085600900517     C                   MOVE      VIDDIN        G02DAT
085700900517     C                   MOVEL     *BLANK        G02ERR
085800940913     C                   CALL      'XSRDA8'
085900900517     C                   PARM                    WLBDAT
086000951031    2C     G02ERR        IFEQ      '1'
086100940913     C                   MOVEL     MSG(2)        VIDMSG
086200940913     C                   SETON                                        762890
086300130503     C                   GOTO      ENDCT2
086400951031    2C                   ENDIF
086500130503     c
086600940913     C                   Z-ADD     G02DAT        VIDDIN
086700130503     c                   if        g02inv<>datinw
086800130503     c                   clear                   wforzb            1
086900130503     c                   endif
087000130503     C                   Z-ADD     G02INV        DATINW            8 0
087100130503     C                   Z-ADD     G02DAT        WDATA
087200130503     C                   MOVEL     MM            KMES
087300130503     c
087400130503     c* In caso di apertura fogli viaggio faccio il controllo
087500130503    2c                   if        *inkj   or *inkm  or *inkp
087600130503     c
087700130503      * non può essere minore o maggiore di 1 gg. lavorativo
087800130503    3c                   If        datinw < wdtameno1  or
087900130503     c                             datinw > wdtapiu1
088000130503     c                   Eval      VidMsg = Msg(9)
088100130503     c                   Eval      *in28 = *On
088200130503     c                   Eval      *in76 = *On
088300130503     c                   Eval      *in90 = *On
088400130503     C                   GOTO      ENDCT2
088500130503    3c                   EndIf
088600130503     c
088700130503     C* Se DATA DFV CADE IN GIORNO FESTIVO PER
088800130503     C*  LA TRAINI, AVVISO. ERRORE FORZABILE
088900130503     C                   CLEAR                   KTFA
089000130503     C                   Z-ADD     68            KTFP
089100130503     C     KCLN1         CHAIN     AZCLN01L                           83
089200130503    3C  N83POM(GG)       IFNE      *BLANKS
089300130503     C                   MOVEL     '1Y'          COD
089400130503     C                   MOVEL(P)  POM(GG)       KEY
089500130503     C     KTAB          CHAIN     TABEL                              83
089600130503     C                   MOVEL     TBLUNI        DS1Y
089700130503     C*
089800130503    4C  N83§1YFET        IFNE      *BLANKS
089900130503     C     WFORZB        ANDEQ     *BLANKS
090000130503     C                   MOVEL     MSG(17)       VIDMSG
090100130503     C                   SETON                                        287690
090200130503     C                   MOVEL     '1'           WFORZB            1
090300130503    4C                   ENDIF
090400130503    3c                   endif
090500130503    2c                   endif
090600951031     C*
090700951031   X1C                   ELSE
090800940913     C                   Z-ADD     0             DATINW
090900041130     c* solo se non sono stati richiesti solo gli aperti
091000041130     c     tipfog        ifne      '2'
091100041129     C                   MOVEL     MSG(10)       VIDMSG
091200041129     C                   SETON                                        762890
091300041130     c                   endif
091400951031    1C                   ENDIF
091500130314     C   90              GOTO      ENDCT2
091600951031     C*
091700951031     C* CONTROLLO DATA FINALE
091800951031    1C     VIDDFI        IFEQ      0
091900951031    2C     VIDDIN        IFEQ      *ZERO
092000940913     C                   Z-ADD     WDATE         VIDDFI
092100951031   X2C                   ELSE
092200940921     C                   Z-ADD     VIDDIN        VIDDFI
092300951031    2C                   ENDIF
092400951031    1C                   ENDIF
092500951031     C*
092600900517     C                   MOVE      VIDDFI        G02DAT
092700900517     C                   MOVEL     *BLANK        G02ERR
092800940913     C                   CALL      'XSRDA8'
092900900517     C                   PARM                    WLBDAT
093000951031    1C     G02ERR        IFEQ      '1'
093100940913     C                   MOVEL     MSG(2)        VIDMSG
093200940913     C                   SETON                                        772890
093300951031    1C                   ENDIF
093400130314     C   90              GOTO      ENDCT2
093500940913     C                   Z-ADD     G02DAT        VIDDFI
093600940913     C                   Z-ADD     G02INV        DATFIW            8 0
093700951031     C*
093800951031     C* DATA INIZIALE MAGGIORE DATA FINALE
093900951031    1C     DATINW        IFGT      DATFIW
094000940913     C                   MOVEL     MSG(1)        VIDMSG
094100940913     C                   SETON                                        762890
094200130314     C                   GOTO      ENDCT2
094300951031    1C                   ENDIF
094400960126     C**
094500960126     C* CONTROLLO NUMERO FOGLIO VIAGGIO
094600960710    1C     VIDNFV        IFGT      0
094700130314     C     kfgv          CHAIN     FNFGV01L                           31
094800960126    2C     *IN31         IFEQ      *ON
094900960126     C                   MOVEL     MSG(4)        VIDMSG
095000960201     C                   SETON                                        284890
095100130314     C                   GOTO      ENDCT2
095200960126    2C                   ENDIF
095300130426     c* Per chiusura fogli viaggio controllo se ha bolle altrimenti
095400130426     c*  si forza con F14
095500130426     c                   if        not *inkn and parflg='C'
095600130426     c                   eval      Knfv5=vidnfv
095700130426     c                   exsr      contrBolle
095800130426     c                   if        *in90
095900130426     c                   seton                                        48
096000130426     c                   goto      endct2
096100130426     c                   endif
096200130426     c
096300130426     c                   endif
096400130426     c
096500130315    2C                   ENDIF
096600951031     C*
096700130314     C     ENDCT2        ENDSR
096800130426     C*****************************************************************
096900130426     c*   controllo se il foglio ha elle bolle per chiusrua FGV
097000130426     C*****************************************************************
097100130426     C     contrBolle    BEGSR
097200130607     c                   if        parfl2<>'N'
097300130426     C     Kblp2         SETLL     FNBLP000                               83
097400130426     c                   if        *in83
097500130426     c                   z-add     knfv5         knfv6
097600130426     C     KBRV          SETLL     FNBRV01L                               84
097700130426     c                   endif
097800130607     c
097900130607     c                   else
098000130607     c* Per la nuova borderizzazine controllo solo se ci sono spunte
098100130607     c                   seton                                        83
098200130607     C     KBRV          SETLL     FNBRV01L                               84
098300130607     c                   endif
098400130426     C*
098500130426     C* BOLLE O SPUNTE NON TROVATE --> FORZATURA
098600130426     C     *IN83         IFEQ      *OFF
098700130426     C     *IN84         OREQ      *OFF
098800130426     C                   SETON                                        9028
098900130426     C  N84              MOVEL     MSG(16)       VIDMSG
099000130426     C  N83              MOVEL     MSG(15)       VIDMSG
099100130426     C                   ENDIF
099200130426     c
099300130426     c                   ENDSR
099400130314     C*****************************************************************
099500130314     C*   Visualizza singolo foglio
099600130314     C*****************************************************************
099700130314     C     VisualFGV     BEGSR
099800130426     c                   setoff                                       90
099900130426     c* Se sono in chiusura fogli viaggio emetto finestra se si tratta
100000130426     c*  di foglio NON bis
100100130502     c                   if        vidsce<>'5'  and
100200130502     c                             parflg='C' and fgvSNP=' '
100300130426     c                             and fgvttr <>'D' and fgvttr<>'H'
100400130426     c                             and fgvttr<>'I'
100500130426    3c                   do        *hival
100600130426     c                   exfmt     lst3w01
100700130426     c* Premuto F12 riemetto videata iniziale
100800130426    4c                   if        *inkl
100900130426     C                   SETON                                        90
101000130426     c                   leave
101100130426    4c                   endif
101200130426     c
101300130426     c   kf              leave
101400130426    3c                   enddo
101500130426
101600130426     c                   endif
101700130426     c
101800130426     c                   if        not *in90
101900130318     C                   CLEAR                   fnlst4ds
102000130318     C                   MOVEL     FGVNFV        ilst4NFV
102100130318     C                   move      VIDfgs        ilst4FGS
102200130314     c* Per gestione flag =' '
102300130315     c                   if        vidsce='5'
102400130318     C                   MOVEL     'N'           Ilst4GES
102500130314     c                   endif
102600130426     c                   if        not *in05 and vidsce='2'
102700130318     C                   MOVEL     'M'           ilst4GES
102800130315     c                   endif
102900130426     c                   if        *in05 and vidsce='2'
103000130426     C                   MOVEL     'C'           ilst4GES
103100130603     c                   movel     parfl2        ilst4newc
103200130426     c                   endif
103300130419     c                   if        vidsce='P'
103400130419     C                   MOVEL     'P'           ilst4GES
103500130419     c                   endif
103600130318     C                   MOVEL     fnlst4ds      KPJBU
103700130318     C                   CALL      'FNLST4R'
103800130314     C                   PARM                    KPJBA
103900130318     C                   MOVEL     KPJBU         fnlst4ds
104000130314     C* MESSAGGIO DI ERRORE
104100130318    1C     olst4MSG      IFNE      *BLANKS
104200130318     C                   MOVEL     olst4MSG      VIDMSG
104300130314     C                   SETON                                        2890
104400130314    1C                   ENDIF
104500130426    1C                   ENDIF
104600130426     c
104700130314     C                   ENDSR
104800130314     C*****************************************************************
104900130314     C*   Ricerca Proposte per apertura FGV
105000130314     C*****************************************************************
105100130314     C     RicercaTRN    BEGSR
105200130315     c                   clear                   fnlst1ds
105300130319     c                   eval      ilst1GES='A'
105400130315     c                   movel     datinw        ilst1dat
105500130315     c                   eval      ilst1TUT='G'
105600130315     c                   movel     vidfgs        ilst1fgs
105700130417     c                   if        *inkp
105800130417     c                   eval      ilst1aut='S'
105900130417     c                   endif
106000130315     c
106100130315     c                   movel(p)  fnlst1ds      kpjbu
106200130315     c                   call      'FNLST1R'
106300130315     c                   parm                    kpjba
106400130315     c*
106500130314     C                   ENDSR
106600130314     C*****************************************************************
106700130314     C*   Immissione singolo foglio da traino
106800130314     C*****************************************************************
106900130314     C     Immissione    BEGSR
107000130319     C                   CLEAR                   coptrn
107100130319     C                   CLEAR                   vidpdr
107200130319     C                   CLEAR                   vidpdr
107300130319     c     emforw4       tag
107400130319     c                   exfmt     lst3w04
107500130319
107600130319     c                   setoff                                       289054
107700130319     c                   setoff                                       47
107800130319     c
107900130319     c                   if        *inkl
108000130319     c                   leavesr
108100130319     c                   endif
108200130319     c
108300130319     c                   exsr      ctrImm
108400130319
108500130319     c                   if        not *in90
108600130319     c
108700130319     c                   clear                   fnlst4ds
108800130319     c                   eval      ilst4ges='A'
108900130319     c                   eval      ilst4fgs =%editc(vidfgs:'X')
109000130319     c                   if        coptrn>*zeros
109100130319     c                   eval      ilst4TRN =%int(copTRN)
109200130319     c                   endif
109300130319     c                   eval      ilst4dfv=datinw
109400130319     c                   eval      ilst4pdr=vidpdr
109500130319     c                   eval      ilst4old='S'
109600130319     c                   eval      kpjbu=fnlst4ds
109700130319     c                   call      'FNLST4R'
109800130319     c                   parm                    kpjba
109900130319     c                   eval      fnlst4ds=kpjbu
110000130319     c
110100130319     c                   if        olst4msg<>*blanks
110200130319     c                   movel     olst4msg      w4cmsg
110300130319     c                   seton                                        2890
110400130319     c                   goto      emforw4
110500130319     c                   endif
110600130319     c
110700130319     c                   else
110800130319     c                   goto      emforw4
110900130319     c                   endif
111000130319     c
111100130314     C                   ENDSR
111200951031     C*****************************************************************
111300130319     C*   controlla immissione
111400951031     C*****************************************************************
111500130319     C     CtrImm        BEGSR
111600130319     c
111700130319     C     '?'           SCAN      COPTRN                                 90
111800130319    1C     *IN90         IFEQ      *ON
111900130319     C                   CLEAR                   TNTL00DS
112000130319     C                   Z-ADD     datinw        D00DDE
112100130319     C*
112200130319     C                   MOVE      VIDFGS        D00FIL
112300130319     C                   MOVEL     'C'           D00SCA
112400130319     C                   MOVEL     TNTL00DS      KPJBU
112500130319     C                   CALL      'TNTL17R'
112600130319     C                   PARM                    KPJBA
112700130319     C                   MOVEL     KPJBU         TNTL00DS
112800130319     C*
112900130319     C                   MOVEL     D00TRN        COPTRN
113000130319     C                   SETON                                        54
113100130319     C                   leavesr
113200130319    1C                   ENDIF
113300130319     C*
113400130319    1c                   if        coptrn<>*blanks and coptrn<>*zeros
113500130319     C                   TESTN                   COPTRN               83
113600130319     C                   MOVE      COPTRN        W001              1
113700130319    2C     *IN83         IFEQ      *OFF
113800130319     C     COPTRN        ORLT      *ZEROS
113900130319     C     W001          ORLT      '0'
114000130319     C                   MOVEL     MSG(13)       w4cmsg
114100130319     C                   SETON                                        549028
114200130319     C                   leavesr
114300130319    2C                   ENDIF
114400130319    1C                   ENDIF
114500130319     c
114600130319     C* RICERCA CODICE TRAZIONISTA
114700130319     C     '?'           SCAN      VIDPDR                                 83
114800130319    1C     *IN83         IFEQ      *ON
114900130319     C                   MOVEL     'T '          PA3TIP
115000130319     C                   MOVEL(P)  VIDDPD        PA3RSC
115100130319     C                   CLEAR                   PA3CSF
115200130319     C                   CLEAR                   PA3PDR
115300130319     C                   CLEAR                   PA3FLG
115400130319     C                   MOVEL     PARAM3        KPJBU
115500130319     C                   CALL      'TRUL16R'
115600130319     C                   PARM                    KPJBA
115700130319     C                   MOVEL     KPJBU         PARAM3
115800130319     C                   MOVEL     PA3PDR        VIDPDR
115900130319     C                   MOVEL     PA3RSC        VIDDPD
116000130319     C                   SETON                                        9047
116100130319     C                   LEAVESR
116200130319     C*
116300130319    1C                   ENDIF
116400130319     C*
116500130319     C* CONTROLLO VALIDITA' CODICE TRAZIONISTA
116600130319    1C     VIDPDR        IFNE      *BLANKS
116700130319     C     VIDPDR        ANDNE     *ZEROS
116800130319     C*
116900130319     C                   TESTN                   VIDPDR               31
117000130319     C  N31              MOVEL     MSG(14)       w4cmsg
117100130319     C  N31              SETON                                        902847
117200130319     C  N31              leavesr
117300130319     C*
117400130319     C                   MOVE      VIDPDR        KPDR
117500130319     C                   Z-ADD     KPDR          KTLPDR
117600130319     C     KTNTLZ        CHAIN     TNTLZ01L                           31
117700130319     C*
117800130319    2C     *IN31         IFEQ      *ON
117900130319     C     TLZATB        ORNE      ' '
118000130319     C                   MOVEL     MSG(14)       w4cmsg
118100130319     C                   SETON                                        904728
118200130319     C                   leavesr
118300130319   X2C                   ELSE
118400130319     C*
118500130319     C* IMPOSTO LA RAGIONE SOCIALE SOLO SE NON IMMESSA SE E' IL FASULLO
118600130319    3C     VIDPDR        IFNE      '0465000'
118700130319     C     VIDDPD        OREQ      *BLANKS
118800130319     C                   MOVEL     TLZRSC        VIDDPD
118900130319    3C                   ENDIF
119000130319    2C                   ENDIF
119100130319    1C                   ENDIF
119200130319     c
119300130319     c                   ENDSR
119400130319     C*****************************************************************
119500130319     C*   R O U T I N E      I N I Z I A L E
119600130319     C*****************************************************************
119700130319     C     *INZSR        BEGSR
119800020806      *
119900020806      ****  KLIST  ****
120000130503     C     KTAB          KLIST
120100130503     C                   KFLD                    CODUT
120200130503     C                   KFLD                    COD
120300130503     C                   KFLD                    KEY
120400951102     C* ACCESSO FNFGV02L
120500130319     C     KTNTLZ        KLIST
120600130319     C                   KFLD                    KTLTIP
120700130319     C                   KFLD                    KTLPDR
120800130314     C     KFGV02        KLIST
120900130314     C                   KFLD                    vidfgs
121000941121     C                   KFLD                    DATINW
121100130314     C     KFGV          KLIST
121200130314     C                   KFLD                    vidnfv
121300130315     C                   KFLD                    vidfgs
121400130419     c     kfgvsfl       klist
121500130419     c                   kfld                    FGVnfv
121600130419     c                   kfld                    vidfgs
121700020806     c* File FNBLP29L
121800020806     c     K02BLP79      klist
121900020806     c                   kfld                    FGVlnp
122000020806     c                   kfld                    FGVnfv
122100130426     c* File FNBLP29L
122200130426     c     Kblp2         klist
122300130426     c                   kfld                    vidfgs
122400130426     c                   kfld                    knfv5
122500130314     c     Kbrv          klist
122600130314     c                   kfld                    knpg
122700130426     c                   kfld                    Knfv6
122800130314     c                   kfld                    FGVlnp
122900130322     C     KFV2          KLIST
123000130322     C                   KFLD                    FGVLNP
123100130322     C                   KFLD                    FGVNFV
123200130322     C                   KFLD                    KEPA
123300130322     C                   KFLD                    KLAI
123400130322     C                   KFLD                    KTDH
123500130429     C     KFV4          KLIST
123600130429     C                   KFLD                    FGVLNP
123700130429     C                   KFLD                    FGVNFV
123800130429     C                   KFLD                    KEPA
123900130429     C                   KFLD                    KLAI
124000130429     C                   KFLD                    Ktrc
124100130322     C     KFV3          KLIST
124200130322     C                   KFLD                    FGVLNP
124300130322     C                   KFLD                    FGVNFV
124400130322     C                   KFLD                    KEPA
124500130322     C                   KFLD                    KTDH
124600130503     C     KCLN1         KLIST
124700130503     C                   KFLD                    KTFP
124800130503     C                   KFLD                    KTFA
124900130503     C                   KFLD                    KANN
125000130503     C                   KFLD                    KMES
125100020806      *
125200020806      ****  IMPOSTO CAMPI FISSI  ****
125300951031     C                   SETOFF                                       01
125400130314     C                   MOVEL     'FNLST3R '    VIDPGM
125500951108     C* DATA DEL GIORNO
125600020806     C                   TIME                    WTIME
125700951108     C                   MOVE      WTIME         WDATE
125800020806      ****
125900951031     C     *ENTRY        PLIST
126000951031     C                   PARM                    KPJBA
126100040611     c                   parm                    DLSpo
126200040630     c                   parm                    DLSpo2
126300040611      *
126400130314     C                   MOVEL     KPJBU         FNLST3DS
126500040611     c                   clear                   wDLSpo
126600040630     c                   clear                   wDLSpo2
126700040611if  1c                   if        SDSprm > 1
126800040611     c                   move      DLSpo         wDLSpo
126900040611e   1c                   endif
127000040630if  1c                   if        SDSprm > 2
127100040630     c                   move      DLSpo2        wDLSpo2
127200040630e   1c                   endif
127300000609     C* TESTATA PGM
127400951031     C                   MOVE      TES(1)        VIDTES
127500951031     C*
127600951031     C                   Z-ADD     1             CODUT
127700951031     C                   CALL      'X§PARUT'
127800020806     C                   PARM                    UT§DSE0F
127900951031     C                   MOVEL     RAGUT         VIDRSU
128000951031     C                   MOVEL     REC80         CNCR80
128100000609     C                   SETON                                        35
128200000609      *
128300130314      * terminal partenza in gestione
128400130314     c                   if        parfgs>*zeros
128500130314     c                   movel     parfgs        vidfgs
128600130314     c                   else
128700130314     c                   movel     simfel        vidfgs
128800130314     c                   endif
128900130314     c
129000130314     c                   clear                   viddfgs
129100130314     c     vidfgs        chain     AZORG01L
129200020806     c                   if        %found(AZORG01L)
129300130315     c                   movel     ORGdes        viddfgs
129400020806     c                   endif
129500130430     c*
129600130503     c* Carico limiti date per immissione
129700130503     c                   exsr      sr_MinMax
129800130430      /free
129900130430
130000130430        // Carico filiali di test abilitate
130100130430         clear TIBS02ds;
130200130430         T02Mod = 'C';
130300130430         T02sif = knsif;
130400130430         T02cod = 'VPO';
130500130430         T02ke1 ='DECOFI'   ;
130600130430         TNTBE_RicercaControllo  (kpjba : tibs02ds);
130700130430         if  T02err  = *blank;
130800130430           dvpodecofi = T02uni;
130900130430         endif;
131000130430
131100130430       if  §vpofgvf1<>'999'  ;
131200130430           eval alfafgs=%editc(vidfgs:'X')  ;
131300130430       if  alfafgs<>§vpofgvf1 and alfafgs<>§vpofgvf2 and
131400130430           alfafgs<>§vpofgvf3 and alfafgs<>§vpofgvf4 and
131500130430           alfafgs<>§vpofgvf5 ;
131600130430          Vidmsg = Msg(08);
131700130430          *in90=*on  ;
131800130430          *in28=*on  ;
131900130430          *in99=*on  ;
132000130430           leavesr;
132100130430         endif         ;
132200130430         endif         ;
132300130430      /end-free
132400130314     c
132500940912     C                   ENDSR
132600130503      **************************************************************************
132700130503      *  Calcolo meno 1 gg e più 1 gg lavorativi da data foglio viaggio
132800130503      **************************************************************************
132900130503     c     Sr_MinMax     BegSr
133000130503
133100130503     c                   Clear                   Ktfa
133200130503     c                   Z-Add     VidFgs        Ktfp
133300130503     c                   Move      datasys       wdtaeur
133400130503      * meno 1 gg. lavorativo
133500130503     c                   Do        *Hival
133600130503     c                   Subdur    1:*d          wdtaeur
133700130503     c                   extrct    wdtaeur:*d    wgiorno
133800130503     c                   extrct    wdtaeur:*m    kmes
133900130503     c                   extrct    wdtaeur:*y    kann
134000130503     c     kcln1         Chain     Azcln01l
134100130503     c                   If        Not %Found(azcln01l)
134200130503     c                   Leave
134300130503     c                   EndIf
134400130503     c                   If        POM(wgiorno) = 'F'
134500130503     c                   Iter
134600130503     c                   EndIf
134700130503     c                   Leave
134800130503     c                   EndDo
134900130503     c                   Move      wdtaeur       wdtaiso
135000130503     c                   Move      wdtaiso       wdtameno1
135100130503
135200130503      * meno 2 gg. lavorativ1 (non imposto wdataeur perchè già impostato
135300130503     c*  con data foglio-1)
135400130503     c                   Do        *Hival
135500130503     c                   Subdur    1:*d          wdtaeur
135600130503     c                   extrct    wdtaeur:*d    wgiorno
135700130503     c                   extrct    wdtaeur:*m    kmes
135800130503     c                   extrct    wdtaeur:*y    kann
135900130503     c     kcln1         Chain     Azcln01l
136000130503     c                   If        Not %Found(azcln01l)
136100130503     c                   Leave
136200130503     c                   EndIf
136300130503     c                   If        POM(wgiorno) = 'F'
136400130503     c                   Iter
136500130503     c                   EndIf
136600130503     c                   Leave
136700130503     c                   EndDo
136800130503     c                   Move      wdtaeur       wdtaiso
136900130503     c                   Move      wdtaiso       wdtameno2
137000130503      * più 1 gg. lavorativo
137100130503     c                   Move      datasys       wdtaeur
137200130503     c                   Do        *Hival
137300130503     c                   Adddur    1:*d          wdtaeur
137400130503     c                   extrct    wdtaeur:*d    wgiorno
137500130503     c                   extrct    wdtaeur:*m    kmes
137600130503     c                   extrct    wdtaeur:*y    kann
137700130503     c     kcln1         Chain     Azcln01l
137800130503     c                   If        Not %Found(azcln01l)
137900130503     c                   Leave
138000130503     c                   EndIf
138100130503     c                   If        POM(wgiorno) = 'F'
138200130503     c                   Iter
138300130503     c                   EndIf
138400130503     c                   Leave
138500130503     c                   EndDo
138600130503     c                   Move      wdtaeur       wdtaiso
138700130503     c                   Move      wdtaiso       wdtapiu1
138800130503
138900130503     c                   EndSr
138901130611      **************************************************************************
138902130611      * Window di richiesta stampa etichette borderò
138903130611      **************************************************************************
138904130611     c     geswdw02      BegSr
138905130611     c                   eval      wideti='S'
138906130611     c                   do        *hival
138907130611     c                   exfmt     lst3w02
138908130611     c                   if        *inkf
138909130611     c                   leave
138910130611     c                   endif
138911130611     c                   enddo
138912130611     c                   if        wideti='S'
138913130611     c* richiesta stampanti se non ancora fatta
138914130611     c                   if        wrichtrul90=' '
138915130611     c                   exsr      richstampan
138916130611     c                   if        d90f3<>'1' and not *in91
138917130611     c                   eval      wrichtrul90='S'
138918130611     c                   endif
138919130611     c                   if        *in91
138920130611     c                   endif
138921130611     c                   endif
138922130611     c                   if        wrichtrul90='S'
138923130611     c                   clear                   fnlst9ds
138924130611     c                   eval      LST9DFV=%dec(ilst1dat:8:0)
138925130611     c                   eval      LST9FOG='S'
138926130611     c                   eval      LST9PGMSE=d90psb
138927130611     c                   movel     fnlst9ds      kpjbu
138928130611     c                   call      'FNLST9R1'
138929130611     c                   parm                    kpjba
138930130611     c                   endif
138931130611     c                   endif
138932130611     c                   endsr
138933130611     C**************************************************************************
138934130611      * RICHIEDO LE STAMPANTI per stampa etichette
138935130611     C**************************************************************************
138936130611     c     RICHSTAMPAN   BEGSR
138937130611     C                   CLEAR                   TRUL90DS
138938130611      *
138939130611     C                   MOVEL     'S'           d90RSe
138940130611     C                   MOVEL     'ETBOR'       d90mde
138941130611     C                   CALL      'TRUL90R'
138942130611     C                   PARM                    KPJBA
138943130611     C                   PARM                    trul90DS
138944130611      *
138945130611     C* F3 - FINE
138946130611     c                   if        d90f3<>'1'
138947130611      *
138948130611      * OVRPRTF
138954130611     C*
138955130611     C                   Z-ADD     130           LUNG             15 5
138956130611     C                   CLEAR                   COMMAN          130
138957130611     c                   eval      comman='OVRPRTF FILE(FNLV22P) OUTQ('
138958130611     c                             + d90pre + ') FORMTYPE('
138959130611     c                             + d90mde + ') USRDTA(' + '''' + 'Etich.Bord'
138960130611     c                             + '''' + ') share(*yes)'
138961130611     C                   CALL      'QCMDEXC'                            91
138962130611     C                   PARM                    COMMAN
138963130611     C                   PARM                    LUNG
138964130611      *
138982130611     c                   endif
138983130611      *
138984130611     c                   ENDSR
139000130503
139100951031     C*****************************************************************
139200951031** MESSAGGI DI ERRORE
139300940913Immettere data iniziale minore o uguale a quella finale                        1
139400940913Immettere data formalmente valida                                              2
139500940913Non trovati fogli di viaggio per la selezione richiesta                        3
139600951031Immettere un foglio di viaggio esistente                                       4
139700960313Verranno selezionati solo i primi xxx Fogli Viaggio: enter x continuare        5
139800130430Opzione "5" non ammessa: immettere l'opzione "1" per selezionare e modificare  6
139900130430Interrogazione non ammessa: premere F3 e riproporre la richiesta in gestione   7
140000130430Utente di filiale non abilitata all'uso di questa funzione.Enter per terminare 8
140100130503Data DAL usata in APERTURA errata: non compresa nei limiti previsti            9
140200130314Data DAL obbligatoria
140300130314Non è possibile visualizzare tutti i fogli richiesti: superano i 9990!!!
140400130502Opzione non ammessa                                                           12
140500130319Immettere un numero di TRAINO esistente                                       13
140600130319Immettere codice TRAZIONISTA esistente                                        14
140700130426Non sono presenti BOLLE BORDERIZZATE!!  Premere F14 per forzare               15
140800130426Non sono presenti SPUNTE!!  Premere F14 per forzare                           16
140900130503La data Foglio Viaggio e' un giorno festivo: enter per forzare                17
141000951031**
141100130417 * GESTIONE FOGLI VIAGGIO PARTENZA *
141200130417 ** INTERROGAZIONE FOGLI VIAGGIO  **
141300130417 ** S C E L T A    FOGLI VIAGGIO  **
141400130426 * CHIUSURA FOGLI VIAGGIO PARTENZA *
