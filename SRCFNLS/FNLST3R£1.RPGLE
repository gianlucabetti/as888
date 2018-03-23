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
010100130319     c* La data serve solo per immissione
010200130319     D  ilst4dfv               3     10  0
010300130319     d
010400130319     D  ilst4nfv              11     15
010500130318     D  IlsT4GES              16     16
010600130318     D* Se PARMSG pieno --> c'è stato ERRORE
010700130318     D  olst4msg              17     94
010800130429     D  iolsT4FOR             95     97
010900130429     D  IlsT4OLD              98     98
011000130318
011100130318     D* F12 = 'S'    --> premuto F12
011200130318     D  olst4F12             102    102
011300130318     D  ilst4fgs             103    105
011400130319 i   D  ilst4TRN             106    112  0
011500130318     D  ilst4NRRP            113    125
011600130319     D* se non immessa il numero relativo record della proposta
011700130319     d*  si può immettere a modo vecchio il trazionista
011800130319     D  ilst4PDR             126    132
011900130315     d*
012000130319     D*-----------------------------------------------------
012100130315     d* Richiamo a pgm di proposte traini
012200130315     D FNLST1DS        DS
012300130319     d* 'A' --> ricerca proposte per apertura FGV
012400130315     D  Ilst1GES               1      1
012500130315     D  Ilst1DAT               2      9
012600130315     D* 'G' --> caricare solo le proposte traini del giorno della settimana
012700130315     D*         corrispondente alla data;
012800130315     D* ' ' --> caricare tutte le proposte/traini alla data decorrenza
012900130315     D  Ilst1TUT              10     10
013000130315     D  Ilst1fgs              11     13
013100130417     d* scelta automatica del programma senza proposta videata
013200130417     D  Ilst1aut              14     14
013300020806      *
013400900515     D WLBDAT          DS
013500940913     D  G02DAT                 1      8  0
013600940913     D  G02INV                 9     16  0
013700940913     D  G02ERR                17     17
013800940913     D  G02TGI                18     22  0
013900040611      *
014000040611      * STATUS DS
014100040611      *
014200040611     d Status         sds
014300040611     d   SDSprm          *parms
014400020806      *
014500130503     D                 DS
014600130503     D  CLNPOM                 1     31
014700130503     D  POM                    1     31
014800130503     D                                     DIM(31)
014900130503     D                 DS
015000130503     D  CLNMAT                 1     31
015100130503     D  MAT                    1     31
015200130503     D                                     DIM(31)
015300130503     D                 DS
015400130503     D  GG                     1      2  0
015500130503     D  MM                     3      4  0
015600130503     D  AA                     5      8  0
015700130503     D  WDATA                  1      8  0
015800020806      * VARIABILI
015900020806      *
016000130503     D COD             S                   LIKE(TBLCOD)
016100130503     D KEY             S                   LIKE(TBLKEY)
016200130503     D KTFP            S                   LIKE(FGVLNA)
016300130503     D KTFA            S                   LIKE(FGVLNA)
016400020806     D WDATE           S                   LIKE(FGVDFV)
016500130426     D Knfv6           S                   LIKE(brvnfv) INZ
016600130426     D Knfv5           S                   LIKE(fgvnfv) INZ
016700020806     D DATFIW          S                   LIKE(FGVDFV) INZ
016800020806     D DATINW          S                   LIKE(FGVDFV) INZ
016900130314     D savFIW          S                   LIKE(FGVDFV) INZ
017000130314     D savINW          S                   LIKE(FGVDFV) INZ
017100130322     D savrtp          S                   LIKE(vidrtp) INZ
017200130322     D savrta          S                   LIKE(vidrta) INZ
017300130426     D savtfog         S                   LIKE(tipfog) INZ
017400130503     D KANN            S                   LIKE(CLNANN)
017500130503     D KMES            S                   LIKE(CLNMES)
017600130314     D knpg            S                   LIKE(brvnpg) INZ(1)
017700130322     D KEPA            S                   LIKE(FV2EPA)
017800130322     D KTDH            S                   LIKE(FV2TDH)
017900130322     D KLAI            S                   LIKE(FV2LAI)
018000130429     D KTRC            S                   LIKE(FV4trc)
018100020806     D WTIME           S             14  0
018200020806     D NRR4            S              4  0 INZ
018300020806     D XZ              S              3  0 INZ
018400020806     D W0020           S              2  0 INZ
018500020806     D WRTA            S              1    INZ
018600041129     D Werrore         S              1    INZ
018700130430     D alfaFGS         S              3    INZ
018800130319     D KTLTIP          S                   LIKE(TLZTIP) INZ('T ')
018900130319     D KTLPDR          S                   LIKE(TLZPDR)
019000130319     D KPDR            S                   LIKE(fgvPDR)
019100130503     d Datasys         s               d   datfmt(*eur) inz(*sys)
019200130503     d wdtaeur         s               d   datfmt(*eur)
019300130503     d wdtaiso         s               d   datfmt(*iso)
019400130503     d wdtameno1       s              8  0
019500130503     d wdtameno2       s              8  0
019600130503     d wdtapiu1        s              8  0
019700130503     d wgiorno         s              2  0
019800040611      *
019900040611      * Parametri aggiuntivi:
020000040611      * - P.O. di cui selezionare i F.V. (da FIMS03R)
020100040611     d DLSpo           s              3
020200040630     d DLSpo2          s              3
020300040611     d wDLSpo          s              3  0
020400040630     d wDLSpo2         s              3  0
020500020806
020600130430      //---------------------------------------------------------------
020700130430      /copy gaitrasrc/srcprotopr,tibs02r
020800130430
020900020806      *****************************************************************
021000020806      *  RIEPILOGO INDICATORI
021100020806      *****************************************************************
021200130314      * 02    - interrogazione   OFF - gestione
021300130426      * 05    - Chiusura foglio viaggio singola
021400130314      * 06    - FOGLIO VIAGGIO IN GESTIONE AD ALTRO P.O.
021500020806      * 10    - ON SELEZIONE PER DATA
021600020806      * 11    - RICHIESTA LA VISUALIZZAZIONE DEL FOGLIO VIAGGIO PASSATO
021700020806      *         DAL PGM CHIAMANTE
021800020806      * 13    - VISUALIZZO SOLO I F.V. APERTI
021900020806      * 14    - P.O. GESTIONE ERRATO
022000130314      * 20    - PROGRAMMA RICHIAMATO PER SCELTA FOGLI VIAGGIO
022100130523      * 21    - proteggo scelte nel sfl ctl
022200020806      * 22    - DEVO PASSARE AL PGM CHIAMANTE TUTTI I F.V. SELEZIONATI
022300130429      * 25    - Reverse immage per pimobi non inseriti
022400130523      * 26    - Reverse immage per targa rimorchio  non inserita
022500020806      * 28    - INDICATORE DI EMISSIONE MESSAGGIO DI ERRORE
022600020806      * 29    - READC SUBFILE
022700020806      * 31/33 - DI COMODO
022800020806      * 35    - TIPO UTENTE
022900020806      * 36    - SFLNXTCHG
023000020806      * 40    - ERRORE
023100020806      * 48    - ERRORE
023200020806      * 72/73 - SFLCLR SFLDSP E SFLDSPCTLM           SFL04
023300020806      * 76/77 - ERRORE
023400020806      * 90    - INDICATORE DI ERRORE GENERICO
023500020806      * 95    - COMODO
023600130430      * 99    - Errore grave: esco dal pgm
023700020806      *****************************************************************
023800130314     c
023900130314     C                   EXSR      PULIZ
024000130314     c
024100130426    1c                   select
024200130426     c* INTERROGAZIONE
024300130506     c                   when      parflg='I' or parflg='P'
024400130314     c                   seton                                        02
024500130417     C                   MOVE      TES(2)        VIDTES
024600130506     c                   if        parflg='P'
024700130506     C                   SETON                                        20
024800130506     c                   endif
024900130314     C
025000130426     c* CHIUSURA FOGLI VIAGGIO
025100130426     c                   when      parflg='C'
025200130426     c                   seton                                        0205
025300130426     C                   MOVE      TES(4)        VIDTES
025400130426     c                   eval      tipfog='2'
025500130426     C
025600130314     c                   when      parflg='G'
025700130314     c                   setoff                                       02
025800130314     c
025900130506     c* RICERCA FOGLI
026000130314     c                   when      parflg='R'
026100130417     C                   MOVE      TES(3)        VIDTES
026200940323     C                   SETON                                        20
026300951106     C                   Z-ADD     PARDIN        VIDDIN
026400951106     C                   Z-ADD     PARDFI        VIDDFI
026500951102     C                   MOVEL     PARTFV        TIPFOG
026600000609     C                   Z-ADD     PARFGS        VIDFGS
026700130523     c
026800130523     c* se ho passato partfv = "2" proteggo le scelte perchè non mi servono
026900130523     c                   if        partfv='2'
027000130523     c                   seton                                        21
027100130523     c                   endif
027200130523     c
027300130314     c* controllo le date passate
027400130314     c                   exsr      ctrd02
027500130314     c* se errore --> esco
027600130314     c                   if        *in90
027700130314     c                   goto      fine
027800130314     c                   endif
027900130314     C*
028000130314     c* CONTROLLO SE DEVO PASSARE AL PGM CHIAMANTE TUTTI I FOGLI DI
028100130314     C*   VIAGGIO SELEZIONATI (PARFL2 = "F")
028200130314    2C     PARFL2        IFEQ      'F'
028300130314     C                   SETON                                        22
028400130314     C                   MOVEA     *ZEROS        FVA
028500130314    2C                   ENDIF
028600130314     C
028700130314    1c                   ENDSL
028800951031     C*
028900130314     C     INIZIO        TAG
029000130314     C                   EXSR      RIES04
029100951031     C*
029200951031     C* VISUALIZZAZIONE SFL E SCELTA FOGLIO
029300020806     C                   Z-ADD     1             XZ
029400960313     C                   Z-ADD     1             REC4
029500020806     C*    =================
029600951031     C     FORCT         TAG
029700020806     C*    =================
029800130314     C                   WRITE     LST3T01
029900130314     C                   WRITE     LST3Z02
030000130314     c* se non caricati fogli emetto videata
030100130315     C  n73              WRITE     LST3d03
030200130314
030300130314     C                   EXFMT     LST3C02
030400130419     C                   SETOFF                                       284048
030500960313     C                   CLEAR                   VIDMSG
030600951031     C*
030700130314     C* F3 o F12 - Fine/Ritorno
030800130430     c*            o per errore grave esco
030900130314    1c                   if        *inKC or *inKL
031000130430     c                             or *in99
031100130314     C                   GOTO      FINE
031200130314    1c                   endif
031300130503     c* controllo le selezioni immesse
031400130503     c                   exsr      Ctrd02
031500130503     c* Errore
031600130503    1c                   if        *in90
031700130503     c                   goto      forct
031800130503    1c                   endif
031900130430    c
032000130419     c* F13 - Immissione multipla con   videata
032100130419     c* F15 - Immissione multipla SENZA videata
032200130419    1c                   if        *inKP or *inkM
032300130314     c                   exsr      RicercaTRN
032400130314     c                   goto      inizio
032500130314    1c                   endif
032600130419    c
032700130417     c* F10 - Immissione singola  non dovrebbe servire ...
032800130417    1c                   if        *inKJ and not *inkj
032900130314     c                   exsr      Immissione
033000130314     c                   goto      inizio
033100130314    1c                   endif
033200130419    c
033300130419     c* F10 - Immissione singola  senza copia traino
033400130419    1c                   if        *inKJ
033500130419     c                   clear                   fnlst4ds
033600130419     c                   eval      ilst4ges='A'
033700130419     c                   eval      ilst4fgs =%editc(vidfgs:'X')
033800130419     c                   eval      ilst4dfv=datinw
033900130419     c                   eval      ilst4old='S'
034000130419     c                   eval      kpjbu=fnlst4ds
034100130419     c                   call      'FNLST4R'
034200130419     c                   parm                    kpjba
034300130419     c                   eval      fnlst4ds=kpjbu
034400130419     c
034500130419     c                   if        olst4msg<>*blanks
034600130419     c                   movel     olst4msg      vidmsg
034700130419     c                   seton                                        2848
034800130419     c                   goto      forct
034900130419     c                   else
035000130419     c                   goto      inizio
035100130419     c                   endif
035200130419     c
035300130419    1c                   endif
035400130314     c
035500130314     c
035600130314     c* se scelto un foglio lo visualizzo
035700130314    1c                   select
035800130314     c*  se cambiate date --> ricarico
035900130314     c                   when      vidnfv>0
036000130419     c                   if        not *in20 and not *in02
036100130315     c                   eval      vidsce='2'
036200130419     c                   endif
036300130506     c                   if        *in02     and not *in05
036400130419     c                   eval      vidsce='5'
036500130419     c                   endif
036600130506     c                   if        *in05
036700130506     c                   eval      vidsce='2'
036800130506     c                   endif
036900130314     c                   exsr      VisualFGV
037000130419     c                   if        vidmsg<>*blanks
037100130419     c                   seton                                        289048
037200130419     c                   else
037300130314     c                   clear                   vidnfv
037400130419     c                   endif
037500130314     c                   goto      forct
037600130314
037700130314     c
037800130314     c                   when      datfiw<>savfiw or
037900130314     c                             datinw<>savinw
038000130314     c                   goto      inizio
038100130426     c
038200130322     c                   when      vidrtp<>savrtp or
038300130322     c                             vidrta<>savrta
038400130322     c                   goto      inizio
038500130426
038600130426     c                   when      tipfog<>savtfog
038700130426     c                   goto      inizio
038800130314     c
038900130314     c* Se immesso numero foglio --> lo visualizzo
039000951031     C*
039100130314   x1c                   other
039200130315     c                   z-add     1             nrr4
039300130430     c
039400130430     c* e non ho caricato fogli non eseguo la lettura
039500130430   1dc                   if        *in73
039600951031     C     LEGGI         TAG
039700130315     C                   READC     LST3S02                                29
039800951031     C*
039900951031     C* SE SELEZIONATO ALMENO UNO ESCO ALTRIMENTI RIEMETTO SUBFILE
040000130315    1c                   if        *in29
040100130315    2C                   IF        *in22  and XZ >  1
040200951031     C                   GOTO      FINE
040300951031    2C                   ENDIF
040400951031     C*
040500130419     C                   GOTO      forct
040600130315    1c                   endif
040700960126     C**
040800130419     c* Per gestione ammessa solo opzione 2 / 5 / P
040900130315   1ac                   if        vidsce<>' '
041000130430
041100130430     c* Se chiusura foglio e scelta con "2" --> controllo se ha bolla
041200130430     c**   o spunte il foglio
041300130426     c
041400130426     c                   if        not *inkn and vidsce='2' and *in05
041500130426     c                   eval      knfv5=fgvnfv
041600130426     c                   exsr      contrbolle
041700130426     c                   if        *in90
041800130426     c                   seton                                        3640
041900130426     C                   Z-ADD     NRR4          REC4
042000130429     c                   exsr      AggioSFL
042100130426     C                   GOTO      forct
042200130426     c                   endif
042300130426     c                   endif
042400130502     c
042500130502     c                   if        *in05 and vidsce<>'2' and vidsce<>'5'
042600130502     c                   seton                                        369040
042700130502     c                   seton                                        28
042800130502     C                   Z-ADD     NRR4          REC4
042900130502     C                   MOVEA     MSG(12)       VIDMSG
043000130502     c                   exsr      AggioSFL
043100130502     C                   GOTO      forct
043200130502     c                   endif
043300130419     c
043400130314     c                   if        parflg='G' and vidsce<>'2'
043500130315     c                             and vidsce<>'5'and vidsce<>' '
043600130419     c                             and vidsce<>'P'
043700130314     c                   seton                                        369040
043800130314     c                   seton                                        28
043900130315     C                   Z-ADD     NRR4          REC4
044000130315     C                   MOVEA     MSG(12)       VIDMSG
044100130429     c                   exsr      AggioSFL
044200130314     C                   GOTO      forct
044300130314     c                   endif
044400130419     c
044500130314     c* Per Interrogazione solo opzione 5
044600130314     c                   if        parflg='I' and
044700130315     c                                 vidsce<>'5'  and vidsce<>' '
044800130314     c                   seton                                        369040
044900130314     c                   seton                                        28
045000130315     C                   MOVEA     MSG(12)       VIDMSG
045100130315     C                   Z-ADD     NRR4          REC4
045200130429     c                   exsr      AggioSFL
045300130314     C                   GOTO      forct
045400130314     c                   endif
045500130506     c                   if        parflg='P' and   vidsce<>'1' and
045600130506     c                                 vidsce<>'5'  and vidsce<>' '
045700130506     c                   seton                                        369040
045800130506     c                   seton                                        28
045900130506     C                   MOVEA     MSG(12)       VIDMSG
046000130506     C                   Z-ADD     NRR4          REC4
046100130506     c                   exsr      AggioSFL
046200130506     C                   GOTO      forct
046300130506     c                   endif
046400130315     c
046500130315     c* Per Ricerca        solo opzione 1
046600130314     c                   if        parflg='R' and
046700130315     c                                 vidsce<>'1'  and vidsce<>' '
046800130314     c                   seton                                        369040
046900130314     c                   seton                                        28
047000130315     C                   Z-ADD     NRR4          REC4
047100130315     C                   MOVEA     MSG(12)       VIDMSG
047200130429     c                   exsr      AggioSFL
047300130314     C                   GOTO      forct
047400130314     c                   endif
047500130314     c*
047600130314    2c                   if        vidsce='2' or  vidsce='5'
047700130419    2c                             or vidsce='P'
047800130314     c                   exsr      visualFGV
047900130419     c
048000130315     C                   Z-ADD     NRR4          REC4
048100130314     c* Se ce' errore devo riemettere sfl
048200130429     c* se non c'e' errore riaggiorno sfl con i dati del foglio
048300130419     c                   if        not *in90
048400130419     C                   MOVEL     ' '           VIDSCE
048500130419     c     kfgvsfl       chain     fnfgv01l
048600130419     c                   exsr      rigaSFL
048700130419     c                   endif
048800130419     c
048900130314     c                   seton                                        3640
049000130429     c                   exsr      AggioSFL
049100130315     c                   if        *in90
049200130314     C                   GOTO      forct
049300130315     c                   endif
049400130314     C*
049500130314     C                   GOTO      LEGGI
049600130314    2C                   ENDIF
049700960318     C*
049800130314     C* SE scelta    SELEZIONO IL FOGLIO CON L'OPZIONE "1"
049900130506    2c                   if        vidsce='1'
050000960126     C* PULISCO IL CAMPO DI SCELTA
050100020806     C                   CLEAR                   VIDSCE
050200130429     c                   exsr      AggioSFL
050300130314     c
050400960126     C* 22 OFF - PASSO UN SOLO NUMERO DI FV
050500130314    3C     *IN22         IFEQ      *OFF
050600960126     C                   Z-ADD     FGVNFV        PARNFV
050700020806     C                   z-add     VIDfgs        PARfgs
050800960126     C                   GOTO      FINE
050900960313     C*
051000130314   X3C                   ELSE
051100951031     C* 22 ON  - MEMORIZZO TUTTI I FV SELEZIONATI
051200020806    5C     XZ            IFLE      45
051300020806     C                   z-add     VIDfgs        PARfgs
051400020806     C                   Z-ADD     FGVLNA        FVA(XZ)
051500020806     C                   MOVEL     FGVNFV        FVA(XZ)
051600020806     C                   ADD       1             XZ
051700960126     C                   GOTO      LEGGI
051800960313   X5C                   ELSE
051900960313     C* SE SONO STATI SELEZIONATI PIU' DI 45 F.VIAGGIO: ERRORE
052000020806     C                   SUB       1             XZ
052100960313     C*
052200960313     C                   MOVEA     MSG(5)        K78
052300020806     C                   MOVEL     XZ            W003A             3
052400960313     C                   MOVEA     W003A         K05(3)
052500960313     C* ABBLANCO ZERO NON SIGNIFICATIVI
052600020806     C                   Z-ADD     3             XZ
052700020806    6C     K05(XZ)       DOWEQ     '0'
052800020806     C     XZ            ANDLT     5
052900020806     C                   CLEAR                   K05(XZ)
053000020806     C                   ADD       1             XZ
053100960313    6C                   ENDDO
053200960313     C*
053300960313     C                   MOVEA     K05(3)        K78(35)
053400960313     C                   MOVEA     K78           VIDMSG
053500960313     C                   SETON                                        4028
053600960313     C                   Z-ADD     NRR4          REC4
053700960313     C*
053800130315     C     NRR4          CHAIN     LST3S02                            33
053900960313     C                   MOVEL     '1'           VIDSCE
054000130429     c                   exsr      AggioSFL
054100960313     C                   GOTO      FORCT
054200960313    5C                   ENDIF
054300960313     C*
054400960126    3C                   ENDIF
054500960126    2C                   ENDIF
054600130430   1aC                   ENDIF
054700951031     C*
054800951102     C* PULISCO IL CAMPO DI SCELTA
054900130419     c                   if        vidsce<>' '
055000130419     C                   SETON                                        4036
055100130419     c                   else
055200130419     C                   SETOff                                       40
055300130419     c                   endif
055400130419     c
055500130419     C                   CLEAR                   VIDSCE
055600130315     c                   setoff                                       36
055700130429     c                   exsr      AggioSFL
055800130430   1bC                   ENDIF
055900130314    1C                   ENDsl
056000951031     C*
056100130430     c   73              goto      leggi
056200130430     c  n73              goto      forct
056300130314     c
056400000000     C     FINE          TAG
056500130315     C   20              MOVEL     fnlst3ds      KPJBU
056600000000     C                   SETON                                        LR
056700951031     C*-----------------------------------------------------***********
056800951031     C* PULIZIA CAMPI VIDEO                                 *  PULIZ  *
056900951031     C*-----------------------------------------------------***********
057000900515     C     PULIZ         BEGSR
057100000620     C                   SETOFF                                       10  15
057200951102     C                   CLEAR                   VIDNFV
057300041129     C                   z-add     wdate         VIDDIN
057400940913     C                   Z-ADD     WDATE         VIDDFI
057500951102     C                   CLEAR                   TIPFOG
057600130322     C                   CLEAR                   VIDRTP
057700130322     C                   CLEAR                   VIDRTA
057800130319     c                   exsr      ctrd02
057900900514     C                   ENDSR
058000130429     C*-----------------------------------------------------***********
058100130429     C* Aggioranemto SFL                                    *  PULIZ  *
058200130429     C*-----------------------------------------------------***********
058300130429     C     AggioSFL      BEGSR
058400130429     c                   if        vidpr_p='NO'
058500130429     c                   seton                                        25
058600130429     c                   endif
058700130523     c                   if        vidpr_t='NO'
058800130523     c                   seton                                        26
058900130523     c                   endif
059000130429     c
059100130429     C                   UPDATE    LST3S02
059200130429     c                   setoff                                       364025
059300130523     c                   setoff                                       26
059400130523     c
059500130429     c                   ENDSR
059600951031     C*-----------------------------------------------------***********
059700130314     C* CARICAMENTO  FOGLIO VIAGGIO
059800951031     C*-----------------------------------------------------***********
059900951031     C     RIES04        BEGSR
060000951031     C*
060100940912     C                   Z-ADD     1             NRR4
060200900517     C                   SETON                                        72
060300130315     C                   WRITE     LST3C02
060400911205     C                   SETOFF                                       7213
060500130430     c*
060600130430     c* Se errore grave --> esco dalla routine
060700130430     c                   if        *in99
060800130430     c                   leavesr
060900130430     c                   endif
061000130430     c
061100130430     C                   SETON                                        73
061200911205     C*
061300951031     C* 13 ON  - VISUALIZZO SOLO I FV APERTI
061400951102    1C     TIPFOG        IFEQ      '2'
061500911205     C                   SETON                                        13
061600951031    1C                   ENDIF
061700951031     C*
061800951031     C* CARICO IL SUBFILE
061900130314    1c                   if        not *in13
062000130314     c     KFGV02        setll     FNFGV02L
062100130314     c     vidfgs        reade     FNFGV02L                               31
062200020806     c                   else
062300130314     c     vidfgs        setll     FNFGV06L
062400130314     c     vidfgs        reade     FNFGV06L                               31
062500130314    1c                   endif
062600951031     C*
062700951031    1C     *IN31         DOWEQ     *OFF
062800041130     c     nrr4          andle     9990
062900951031     C*
063000951031    2C     FGVDFV        IFLE      DATFIW
063100900529     C                   SETOFF                                       95
063200951031     C*
063300951031    3C   13FGVDFV        IFGE      DATINW
063400911205     C                   SETON                                        95
063500951031    3C                   ENDIF
063600951031     C*
063700960219     C                   SELECT
063800960219     C     TIPFOG        WHENEQ    '1'
063900020806    3C     FGVFCF        ANDNE     *BLANKS
064000900529     C                   SETON                                        95
064100951031     C*
064200020806    3C     TIPFOG        WHENEQ    *BLANKS
064300900529     C                   SETON                                        95
064400960219    3C                   ENDSL
064500040611      *
064600040611      * Verifico P.O. arrivo
064700040630    3c                   if             *in95   =  *on
064800040630     c                             AND (wDLSpo  <> *zeros
064900040630     c                             and  wDLSpo  <> FGVlna
065000040630     c                             and  wDLSpo2 =  *zeros
065100040630     c                              OR  wDLSpo2 <> *zeros
065200040630     c                             and  wDLSpo  <> FGVlna
065300040630     c                             and  wDLSpo2 <> FGVlna)
065400040611     c                   eval      *in95 = *off
065500040611    3c                   endif
065600130322     c
065700130322     C* VERIFICO CON IL RITARDO PARTENZA
065800130322    3C   95VIDRTP        IFNE      *BLANKS
065900130322     C* CHAIN SU FV2
066000130322     C                   MOVEL     'P'           KEPA
066100130322     C                   CLEAR                   KLAI
066200130322     C                   MOVEL     'R'           KTDH
066300130322     C     KFV2          CHAIN     FNFV201L                           32
066400130322    4C     *IN32         IFEQ      *OFF
066500130322     C     FV2ATB        ANDEQ     *BLANKS
066600130322     C     FV2RTC        ANDNE     *BLANKS
066700130322     C* SE HO CHIESTO I FV SENZA RITARDO ERRORE
066800130322    5C     VIDRTP        IFEQ      'N'
066900130322     C                   SETOFF                                       95
067000130322    5C                   ENDIF
067100130322   X4C                   ELSE
067200130322     C*
067300130322     C* SE HO CHIESTO I FV CON   RITARDO ERRORE
067400130322    5C     VIDRTP        IFEQ      'R'
067500130322     C                   SETOFF                                       95
067600130322    5C                   ENDIF
067700130322    4C                   ENDIF
067800130322    3C                   ENDIF
067900130322     C**
068000130322     C* VERIFICO CON IL RITARDO ARRIVO
068100130322    3C   95VIDRTA        IFNE      *BLANKS
068200130322     C* CHAIN SU FV2
068300130322     C                   CLEAR                   WRTA
068400130322     C                   MOVEL     'A'           KEPA
068500130322     C                   MOVEL     'R'           KTDH
068600130322     C     KFV3          CHAIN     FNFV203L                           32
068700130322    4C     *IN32         DOWEQ     *OFF
068800130322     C*
068900130322    5C     FV2ATB        IFEQ      *BLANKS
069000130322     C     FV2RTC        ANDNE     *BLANKS
069100130322     C                   SETON                                        32
069200130322     C* SE HO CHIESTO I FV SENZA RITARDO ERRORE
069300130322    6C     VIDRTA        IFEQ      'N'
069400130322     C                   SETOFF                                       95
069500130322   X6C                   ELSE
069600130322     C* SE HO CHIESTO I FV CON RITARDO SOLO A POSTO
069700130322     C                   MOVEL     'S'           WRTA
069800130322    6C                   ENDIF
069900130322    5C                   ENDIF
070000130322     C*
070100130322     C  N32KFV3          READE     FNFV203L                               32
070200130322    4C                   ENDDO
070300130322     C*
070400130322    4C     VIDRTA        IFEQ      'R'
070500130322     C     WRTA          ANDEQ     *BLANKS
070600130322     C                   SETOFF                                       95
070700130322    4C                   ENDIF
070800130322    3C                   ENDIF
070900960219     C**
071000960219     C**
071100951031     C* 95 ON  - TUTTO OK: RECORD DA VISUALIZZARE
071200951031    3C     *IN95         IFEQ      *ON
071300951031     C*
071400951031     C* CAMPO SELEZIONE
071500020806     C                   CLEAR                   VIDSCE
071600130314     C
071700130419     c                   exsr      RigaSFL
071800130315     c
071900130315     c* La prima volta accedo indicatore di posizionamento ccursore
072000130419     c                   if        nrr4=1
072100130419     c                   seton                                        4036
072200130419     c                   endif
072300951031     C*
072400130315     C                   WRITE     LST3S02
072500940912     C                   ADD       1             NRR4
072600130430     c                   setoff                                       403625
072700130523     c                   setoff                                       26
072800951031    3C                   ENDIF
072900951031     C*
073000020806     c                   if        not *in13
073100130314     c     vidfgs        reade     FGV02                                  31
073200020806     c                   else
073300130314     c     vidfgs        reade     FGV06                                  31
073400020806     c                   endif
073500951031     C*
073600951031   X2C                   ELSE
073700020806     C*
073800020806     c                   if        not *in13
073900020806     c                   eval      *in31 = *on
074000020806     c                   else
074100130314     c     vidfgs        reade     FGV06                                  31
074200020806     c                   endif
074300020806     C*
074400951031    2C                   ENDIF
074500951031     C*
074600951031    1C                   ENDDO
074700130314
074800041129     c                   eval      werrore='N'
074900130314     c* Se caricati troppi record --> segnalo l'errore
075000041130     c                   if        *in31 =*off and nrr4 >9990
075100041129     c                   eval      werrore='S'
075200130314     C                   MOVEL     MSG(11)       VIDMSG
075300130314     C                   SETON                                        28
075400041129     c                   endif
075500951031     C*
075600130426     c* salvo i campi di ricaricamento sfl
075700130314     c                   eval      savfiw=datfiW
075800130314     c                   eval      savinw=datinW
075900130322     c                   eval      savrtp=vidrtp
076000130322     c                   eval      savrta=vidrta
076100130426     c                   eval      savtfog=tipfog
076200130315    1c                   if        nrr4=1
076300130315     c                   setoff                                       73
076400130315     c                   endif
076500130315     c
076600951031     C                   ENDSR
076700130419     C*-----------------------------------------------------***********
076800130419     C* Carico dati nella riga del sfl
076900130419     C*-----------------------------------------------------***********
077000130419     c     RigaSFL       BEGSR
077100130523     c                   setoff                                       2526
077200130419     C* GIRO DATA FOGLIO VIAGGIO
077300130419     C                   Z-ADD     FGVDFV        G02INV
077400130419     C                   MOVEL     *ZERO         G02DAT
077500130419     C                   MOVEL     '3'           G02ERR
077600130419     C                   CALL      'XSRDA8'
077700130419     C                   PARM                    WLBDAT
077800130419     C                   Z-ADD     G02DAT        W0020
077900130419     C                   MOVEL     G02DAT        VIDDEL
078000130419     C                   MOVE      W0020         VIDDEL
078100130419     C* DECODIFICO LINEA DI ARRIVO
078200130419     C     FGVLNA        CHAIN     AZORG                              31
078300130419     C  N31              MOVEL     ORGDES        DE2LNA
078400130419     C   31              MOVEL     *BLANKS       DE2LNA
078500130419     C* NUMERO TRAINO
078600130419     C                   MOVEL     FGVTRN        VIDTRN
078700130419     C*
078800130419     C                   MOVEL     *BLANKS       VIDABP
078900130419     C                   MOVEL     *BLANKS       VIDimp
079000130419     c
079100130419     c                   clear                   vidabp
079200130419     C* ANNULLATO
079300130419    4C     FGVATB        IFEQ      '*'
079400130429     C                   MOVEL     'Ann   '      VIDABP
079500130419    4C                   ENDIF
079600130419     C*
079700130419     C* BORDERO'
079800130419     C     K02BLP79      setll     FNBLP79L                               31
079900130429     C   31              MOVE      'B     '      VIDABP
080000130419     c* Verifico se presenti spunte
080100130426     c                   eval      knfv6=fgvnfv
080200130419     C     Kbrv          setll     FNBrv01l                               31
080300130429     c   31              eval      %subst(vidabp:3:2)='Sp'
080400130419     C*
080500130419     C* PARTITO
080600130419    4C     FGVFCF        IFNE      *BLANKS
080700130419     C                   MOVE      'P'           VIDABP
080800130419    4C                   ENDIF
080900130419     c* Verifico che chiusura da IMP
081000130419     c                   if        fgvnfa>0
081100130419     c                   eval      vidimp='DaIMP'
081200130419     c                   endif
081300130429     c* Verifico la presenza dei piombi
081400130429     c* Escludo fogli defluenza e locali
081500130429     c                   eval      vidpr_p='  '
081600130523     c                   eval      vidpr_t='  '
081700130429     c                   if        fgvttr= ' ' or fgvttr='S'
081800130429     c                                         or fgvttr='H'
081900130429     C                   MOVEL     'P'           KEPA
082000130429     C                   CLEAR                   KLAI
082100130429     C                   MOVEL     'P'           KTrc
082200130429     C     KFV4          CHAIN     FNFV401L
082300130429    4C                   IF        %found(fnfv401l) and fv4atb=' '
082400130429     c                             and fv4not<>*blanks
082500130429     c                   else
082600130429     c                   eval      vidpr_p='NO'
082700130429     c                   seton                                        25
082800130429     c                   endif
082900130523     c
083000130523     c* Targa Rimorchio
083100130523     c                   if        fgvtrr=*blanks
083200130523     c                   eval      vidpr_t='NO'
083300130523     c                   seton                                        26
083400130523     c                   endif
083500130523     c
083600130429     c                   endif
083700130419     C*
083800130419     C* TARGHE MOTRICE E RIMORCHIO
083900130419     C                   MOVEL     FGVTRM        VIDTRM
084000130523     C*****              MOVEL     FGVTrr        VIDTrr
084100130419     c                   ENDSR
084200951031     C*-----------------------------------------------------***********
084300130314     C* CONTROLLI dati di parzializzazione
084400951031     C*-----------------------------------------------------***********
084500130314     C     CTRD02        BEGSR
084600940913     C                   SETOFF                                       907677
084700000612     C                   SETOFF                                       1406
084800000609     C*
084900000609     C* CONTROLLO DATA INIZIALE
085000951031    1C     VIDDIN        IFNE      0
085100900517     C                   MOVE      VIDDIN        G02DAT
085200900517     C                   MOVEL     *BLANK        G02ERR
085300940913     C                   CALL      'XSRDA8'
085400900517     C                   PARM                    WLBDAT
085500951031    2C     G02ERR        IFEQ      '1'
085600940913     C                   MOVEL     MSG(2)        VIDMSG
085700940913     C                   SETON                                        762890
085800130503     C                   GOTO      ENDCT2
085900951031    2C                   ENDIF
086000130503     c
086100940913     C                   Z-ADD     G02DAT        VIDDIN
086200130503     c                   if        g02inv<>datinw
086300130503     c                   clear                   wforzb            1
086400130503     c                   endif
086500130503     C                   Z-ADD     G02INV        DATINW            8 0
086600130503     C                   Z-ADD     G02DAT        WDATA
086700130503     C                   MOVEL     MM            KMES
086800130503     c
086900130503     c* In caso di apertura fogli viaggio faccio il controllo
087000130503    2c                   if        *inkj   or *inkm  or *inkp
087100130503     c
087200130503      * non può essere minore o maggiore di 1 gg. lavorativo
087300130503    3c                   If        datinw < wdtameno1  or
087400130503     c                             datinw > wdtapiu1
087500130503     c                   Eval      VidMsg = Msg(9)
087600130503     c                   Eval      *in28 = *On
087700130503     c                   Eval      *in76 = *On
087800130503     c                   Eval      *in90 = *On
087900130503     C                   GOTO      ENDCT2
088000130503    3c                   EndIf
088100130503     c
088200130503     C* Se DATA DFV CADE IN GIORNO FESTIVO PER
088300130503     C*  LA TRAINI, AVVISO. ERRORE FORZABILE
088400130503     C                   CLEAR                   KTFA
088500130503     C                   Z-ADD     68            KTFP
088600130503     C     KCLN1         CHAIN     AZCLN01L                           83
088700130503    3C  N83POM(GG)       IFNE      *BLANKS
088800130503     C                   MOVEL     '1Y'          COD
088900130503     C                   MOVEL(P)  POM(GG)       KEY
089000130503     C     KTAB          CHAIN     TABEL                              83
089100130503     C                   MOVEL     TBLUNI        DS1Y
089200130503     C*
089300130503    4C  N83§1YFET        IFNE      *BLANKS
089400130503     C     WFORZB        ANDEQ     *BLANKS
089500130503     C                   MOVEL     MSG(17)       VIDMSG
089600130503     C                   SETON                                        287690
089700130503     C                   MOVEL     '1'           WFORZB            1
089800130503    4C                   ENDIF
089900130503    3c                   endif
090000130503    2c                   endif
090100951031     C*
090200951031   X1C                   ELSE
090300940913     C                   Z-ADD     0             DATINW
090400041130     c* solo se non sono stati richiesti solo gli aperti
090500041130     c     tipfog        ifne      '2'
090600041129     C                   MOVEL     MSG(10)       VIDMSG
090700041129     C                   SETON                                        762890
090800041130     c                   endif
090900951031    1C                   ENDIF
091000130314     C   90              GOTO      ENDCT2
091100951031     C*
091200951031     C* CONTROLLO DATA FINALE
091300951031    1C     VIDDFI        IFEQ      0
091400951031    2C     VIDDIN        IFEQ      *ZERO
091500940913     C                   Z-ADD     WDATE         VIDDFI
091600951031   X2C                   ELSE
091700940921     C                   Z-ADD     VIDDIN        VIDDFI
091800951031    2C                   ENDIF
091900951031    1C                   ENDIF
092000951031     C*
092100900517     C                   MOVE      VIDDFI        G02DAT
092200900517     C                   MOVEL     *BLANK        G02ERR
092300940913     C                   CALL      'XSRDA8'
092400900517     C                   PARM                    WLBDAT
092500951031    1C     G02ERR        IFEQ      '1'
092600940913     C                   MOVEL     MSG(2)        VIDMSG
092700940913     C                   SETON                                        772890
092800951031    1C                   ENDIF
092900130314     C   90              GOTO      ENDCT2
093000940913     C                   Z-ADD     G02DAT        VIDDFI
093100940913     C                   Z-ADD     G02INV        DATFIW            8 0
093200951031     C*
093300951031     C* DATA INIZIALE MAGGIORE DATA FINALE
093400951031    1C     DATINW        IFGT      DATFIW
093500940913     C                   MOVEL     MSG(1)        VIDMSG
093600940913     C                   SETON                                        762890
093700130314     C                   GOTO      ENDCT2
093800951031    1C                   ENDIF
093900960126     C**
094000960126     C* CONTROLLO NUMERO FOGLIO VIAGGIO
094100960710    1C     VIDNFV        IFGT      0
094200130314     C     kfgv          CHAIN     FNFGV01L                           31
094300960126    2C     *IN31         IFEQ      *ON
094400960126     C                   MOVEL     MSG(4)        VIDMSG
094500960201     C                   SETON                                        284890
094600130314     C                   GOTO      ENDCT2
094700960126    2C                   ENDIF
094800130426     c* Per chiusura fogli viaggio controllo se ha bolle altrimenti
094900130426     c*  si forza con F14
095000130426     c                   if        not *inkn and parflg='C'
095100130426     c                   eval      Knfv5=vidnfv
095200130426     c                   exsr      contrBolle
095300130426     c                   if        *in90
095400130426     c                   seton                                        48
095500130426     c                   goto      endct2
095600130426     c                   endif
095700130426     c
095800130426     c                   endif
095900130426     c
096000130315    2C                   ENDIF
096100951031     C*
096200130314     C     ENDCT2        ENDSR
096300130426     C*****************************************************************
096400130426     c*   controllo se il foglio ha elle bolle per chiusrua FGV
096500130426     C*****************************************************************
096600130426     C     contrBolle    BEGSR
096700130426     C     Kblp2         SETLL     FNBLP000                               83
096800130426     c                   if        *in83
096900130426     c                   z-add     knfv5         knfv6
097000130426     C     KBRV          SETLL     FNBRV01L                               84
097100130426     c                   endif
097200130426     C*
097300130426     C* BOLLE O SPUNTE NON TROVATE --> FORZATURA
097400130426     C     *IN83         IFEQ      *OFF
097500130426     C     *IN84         OREQ      *OFF
097600130426     C                   SETON                                        9028
097700130426     C  N84              MOVEL     MSG(16)       VIDMSG
097800130426     C  N83              MOVEL     MSG(15)       VIDMSG
097900130426     C                   ENDIF
098000130426     c
098100130426     c                   ENDSR
098200130314     C*****************************************************************
098300130314     C*   Visualizza singolo foglio
098400130314     C*****************************************************************
098500130314     C     VisualFGV     BEGSR
098600130426     c                   setoff                                       90
098700130426     c* Se sono in chiusura fogli viaggio emetto finestra se si tratta
098800130426     c*  di foglio NON bis
098900130502     c                   if        vidsce<>'5'  and
099000130502     c                             parflg='C' and fgvSNP=' '
099100130426     c                             and fgvttr <>'D' and fgvttr<>'H'
099200130426     c                             and fgvttr<>'I'
099300130426    3c                   do        *hival
099400130426     c                   exfmt     lst3w01
099500130426     c* Premuto F12 riemetto videata iniziale
099600130426    4c                   if        *inkl
099700130426     C                   SETON                                        90
099800130426     c                   leave
099900130426    4c                   endif
100000130426     c
100100130426     c   kf              leave
100200130426    3c                   enddo
100300130426
100400130426     c                   endif
100500130426     c
100600130426     c                   if        not *in90
100700130318     C                   CLEAR                   fnlst4ds
100800130318     C                   MOVEL     FGVNFV        ilst4NFV
100900130318     C                   move      VIDfgs        ilst4FGS
101000130314     c* Per gestione flag =' '
101100130315     c                   if        vidsce='5'
101200130318     C                   MOVEL     'N'           Ilst4GES
101300130314     c                   endif
101400130426     c                   if        not *in05 and vidsce='2'
101500130318     C                   MOVEL     'M'           ilst4GES
101600130315     c                   endif
101700130426     c                   if        *in05 and vidsce='2'
101800130426     C                   MOVEL     'C'           ilst4GES
101900130426     c                   endif
102000130419     c                   if        vidsce='P'
102100130419     C                   MOVEL     'P'           ilst4GES
102200130419     c                   endif
102300130318     C                   MOVEL     fnlst4ds      KPJBU
102400130318     C                   CALL      'FNLST4R'
102500130314     C                   PARM                    KPJBA
102600130318     C                   MOVEL     KPJBU         fnlst4ds
102700130314     C* MESSAGGIO DI ERRORE
102800130318    1C     olst4MSG      IFNE      *BLANKS
102900130318     C                   MOVEL     olst4MSG      VIDMSG
103000130314     C                   SETON                                        2890
103100130314    1C                   ENDIF
103200130426    1C                   ENDIF
103300130426     c
103400130314     C                   ENDSR
103500130314     C*****************************************************************
103600130314     C*   Ricerca Proposte per apertura FGV
103700130314     C*****************************************************************
103800130314     C     RicercaTRN    BEGSR
103900130315     c                   clear                   fnlst1ds
104000130319     c                   eval      ilst1GES='A'
104100130315     c                   movel     datinw        ilst1dat
104200130315     c                   eval      ilst1TUT='G'
104300130315     c                   movel     vidfgs        ilst1fgs
104400130417     c                   if        *inkp
104500130417     c                   eval      ilst1aut='S'
104600130417     c                   endif
104700130315     c
104800130315     c                   movel(p)  fnlst1ds      kpjbu
104900130315     c                   call      'FNLST1R'
105000130315     c                   parm                    kpjba
105100130315     c*
105200130314     C                   ENDSR
105300130314     C*****************************************************************
105400130314     C*   Immissione singolo foglio da traino
105500130314     C*****************************************************************
105600130314     C     Immissione    BEGSR
105700130319     C                   CLEAR                   coptrn
105800130319     C                   CLEAR                   vidpdr
105900130319     C                   CLEAR                   vidpdr
106000130319     c     emforw4       tag
106100130319     c                   exfmt     lst3w04
106200130319
106300130319     c                   setoff                                       289054
106400130319     c                   setoff                                       47
106500130319     c
106600130319     c                   if        *inkl
106700130319     c                   leavesr
106800130319     c                   endif
106900130319     c
107000130319     c                   exsr      ctrImm
107100130319
107200130319     c                   if        not *in90
107300130319     c
107400130319     c                   clear                   fnlst4ds
107500130319     c                   eval      ilst4ges='A'
107600130319     c                   eval      ilst4fgs =%editc(vidfgs:'X')
107700130319     c                   if        coptrn>*zeros
107800130319     c                   eval      ilst4TRN =%int(copTRN)
107900130319     c                   endif
108000130319     c                   eval      ilst4dfv=datinw
108100130319     c                   eval      ilst4pdr=vidpdr
108200130319     c                   eval      ilst4old='S'
108300130319     c                   eval      kpjbu=fnlst4ds
108400130319     c                   call      'FNLST4R'
108500130319     c                   parm                    kpjba
108600130319     c                   eval      fnlst4ds=kpjbu
108700130319     c
108800130319     c                   if        olst4msg<>*blanks
108900130319     c                   movel     olst4msg      w4cmsg
109000130319     c                   seton                                        2890
109100130319     c                   goto      emforw4
109200130319     c                   endif
109300130319     c
109400130319     c                   else
109500130319     c                   goto      emforw4
109600130319     c                   endif
109700130319     c
109800130314     C                   ENDSR
109900951031     C*****************************************************************
110000130319     C*   controlla immissione
110100951031     C*****************************************************************
110200130319     C     CtrImm        BEGSR
110300130319     c
110400130319     C     '?'           SCAN      COPTRN                                 90
110500130319    1C     *IN90         IFEQ      *ON
110600130319     C                   CLEAR                   TNTL00DS
110700130319     C                   Z-ADD     datinw        D00DDE
110800130319     C*
110900130319     C                   MOVE      VIDFGS        D00FIL
111000130319     C                   MOVEL     'C'           D00SCA
111100130319     C                   MOVEL     TNTL00DS      KPJBU
111200130319     C                   CALL      'TNTL17R'
111300130319     C                   PARM                    KPJBA
111400130319     C                   MOVEL     KPJBU         TNTL00DS
111500130319     C*
111600130319     C                   MOVEL     D00TRN        COPTRN
111700130319     C                   SETON                                        54
111800130319     C                   leavesr
111900130319    1C                   ENDIF
112000130319     C*
112100130319    1c                   if        coptrn<>*blanks and coptrn<>*zeros
112200130319     C                   TESTN                   COPTRN               83
112300130319     C                   MOVE      COPTRN        W001              1
112400130319    2C     *IN83         IFEQ      *OFF
112500130319     C     COPTRN        ORLT      *ZEROS
112600130319     C     W001          ORLT      '0'
112700130319     C                   MOVEL     MSG(13)       w4cmsg
112800130319     C                   SETON                                        549028
112900130319     C                   leavesr
113000130319    2C                   ENDIF
113100130319    1C                   ENDIF
113200130319     c
113300130319     C* RICERCA CODICE TRAZIONISTA
113400130319     C     '?'           SCAN      VIDPDR                                 83
113500130319    1C     *IN83         IFEQ      *ON
113600130319     C                   MOVEL     'T '          PA3TIP
113700130319     C                   MOVEL(P)  VIDDPD        PA3RSC
113800130319     C                   CLEAR                   PA3CSF
113900130319     C                   CLEAR                   PA3PDR
114000130319     C                   CLEAR                   PA3FLG
114100130319     C                   MOVEL     PARAM3        KPJBU
114200130319     C                   CALL      'TRUL16R'
114300130319     C                   PARM                    KPJBA
114400130319     C                   MOVEL     KPJBU         PARAM3
114500130319     C                   MOVEL     PA3PDR        VIDPDR
114600130319     C                   MOVEL     PA3RSC        VIDDPD
114700130319     C                   SETON                                        9047
114800130319     C                   LEAVESR
114900130319     C*
115000130319    1C                   ENDIF
115100130319     C*
115200130319     C* CONTROLLO VALIDITA' CODICE TRAZIONISTA
115300130319    1C     VIDPDR        IFNE      *BLANKS
115400130319     C     VIDPDR        ANDNE     *ZEROS
115500130319     C*
115600130319     C                   TESTN                   VIDPDR               31
115700130319     C  N31              MOVEL     MSG(14)       w4cmsg
115800130319     C  N31              SETON                                        902847
115900130319     C  N31              leavesr
116000130319     C*
116100130319     C                   MOVE      VIDPDR        KPDR
116200130319     C                   Z-ADD     KPDR          KTLPDR
116300130319     C     KTNTLZ        CHAIN     TNTLZ01L                           31
116400130319     C*
116500130319    2C     *IN31         IFEQ      *ON
116600130319     C     TLZATB        ORNE      ' '
116700130319     C                   MOVEL     MSG(14)       w4cmsg
116800130319     C                   SETON                                        904728
116900130319     C                   leavesr
117000130319   X2C                   ELSE
117100130319     C*
117200130319     C* IMPOSTO LA RAGIONE SOCIALE SOLO SE NON IMMESSA SE E' IL FASULLO
117300130319    3C     VIDPDR        IFNE      '0465000'
117400130319     C     VIDDPD        OREQ      *BLANKS
117500130319     C                   MOVEL     TLZRSC        VIDDPD
117600130319    3C                   ENDIF
117700130319    2C                   ENDIF
117800130319    1C                   ENDIF
117900130319     c
118000130319     c                   ENDSR
118100130319     C*****************************************************************
118200130319     C*   R O U T I N E      I N I Z I A L E
118300130319     C*****************************************************************
118400130319     C     *INZSR        BEGSR
118500020806      *
118600020806      ****  KLIST  ****
118700130503     C     KTAB          KLIST
118800130503     C                   KFLD                    CODUT
118900130503     C                   KFLD                    COD
119000130503     C                   KFLD                    KEY
119100951102     C* ACCESSO FNFGV02L
119200130319     C     KTNTLZ        KLIST
119300130319     C                   KFLD                    KTLTIP
119400130319     C                   KFLD                    KTLPDR
119500130314     C     KFGV02        KLIST
119600130314     C                   KFLD                    vidfgs
119700941121     C                   KFLD                    DATINW
119800130314     C     KFGV          KLIST
119900130314     C                   KFLD                    vidnfv
120000130315     C                   KFLD                    vidfgs
120100130419     c     kfgvsfl       klist
120200130419     c                   kfld                    FGVnfv
120300130419     c                   kfld                    vidfgs
120400020806     c* File FNBLP29L
120500020806     c     K02BLP79      klist
120600020806     c                   kfld                    FGVlnp
120700020806     c                   kfld                    FGVnfv
120800130426     c* File FNBLP29L
120900130426     c     Kblp2         klist
121000130426     c                   kfld                    vidfgs
121100130426     c                   kfld                    knfv5
121200130314     c     Kbrv          klist
121300130314     c                   kfld                    knpg
121400130426     c                   kfld                    Knfv6
121500130314     c                   kfld                    FGVlnp
121600130322     C     KFV2          KLIST
121700130322     C                   KFLD                    FGVLNP
121800130322     C                   KFLD                    FGVNFV
121900130322     C                   KFLD                    KEPA
122000130322     C                   KFLD                    KLAI
122100130322     C                   KFLD                    KTDH
122200130429     C     KFV4          KLIST
122300130429     C                   KFLD                    FGVLNP
122400130429     C                   KFLD                    FGVNFV
122500130429     C                   KFLD                    KEPA
122600130429     C                   KFLD                    KLAI
122700130429     C                   KFLD                    Ktrc
122800130322     C     KFV3          KLIST
122900130322     C                   KFLD                    FGVLNP
123000130322     C                   KFLD                    FGVNFV
123100130322     C                   KFLD                    KEPA
123200130322     C                   KFLD                    KTDH
123300130503     C     KCLN1         KLIST
123400130503     C                   KFLD                    KTFP
123500130503     C                   KFLD                    KTFA
123600130503     C                   KFLD                    KANN
123700130503     C                   KFLD                    KMES
123800020806      *
123900020806      ****  IMPOSTO CAMPI FISSI  ****
124000951031     C                   SETOFF                                       01
124100130314     C                   MOVEL     'FNLST3R '    VIDPGM
124200951108     C* DATA DEL GIORNO
124300020806     C                   TIME                    WTIME
124400951108     C                   MOVE      WTIME         WDATE
124500020806      ****
124600951031     C     *ENTRY        PLIST
124700951031     C                   PARM                    KPJBA
124800040611     c                   parm                    DLSpo
124900040630     c                   parm                    DLSpo2
125000040611      *
125100130314     C                   MOVEL     KPJBU         FNLST3DS
125200040611     c                   clear                   wDLSpo
125300040630     c                   clear                   wDLSpo2
125400040611if  1c                   if        SDSprm > 1
125500040611     c                   move      DLSpo         wDLSpo
125600040611e   1c                   endif
125700040630if  1c                   if        SDSprm > 2
125800040630     c                   move      DLSpo2        wDLSpo2
125900040630e   1c                   endif
126000000609     C* TESTATA PGM
126100951031     C                   MOVE      TES(1)        VIDTES
126200951031     C*
126300951031     C                   Z-ADD     1             CODUT
126400951031     C                   CALL      'X§PARUT'
126500020806     C                   PARM                    UT§DSE0F
126600951031     C                   MOVEL     RAGUT         VIDRSU
126700951031     C                   MOVEL     REC80         CNCR80
126800000609     C                   SETON                                        35
126900000609      *
127000130314      * terminal partenza in gestione
127100130314     c                   if        parfgs>*zeros
127200130314     c                   movel     parfgs        vidfgs
127300130314     c                   else
127400130314     c                   movel     simfel        vidfgs
127500130314     c                   endif
127600130314     c
127700130314     c                   clear                   viddfgs
127800130314     c     vidfgs        chain     AZORG01L
127900020806     c                   if        %found(AZORG01L)
128000130315     c                   movel     ORGdes        viddfgs
128100020806     c                   endif
128200130430     c*
128300130503     c* Carico limiti date per immissione
128400130503     c                   exsr      sr_MinMax
128500130430      /free
128600130430
128700130430        // Carico filiali di test abilitate
128800130430         clear TIBS02ds;
128900130430         T02Mod = 'C';
129000130430         T02sif = knsif;
129100130430         T02cod = 'VPO';
129200130430         T02ke1 ='DECOFI'   ;
129300130430         TNTBE_RicercaControllo  (kpjba : tibs02ds);
129400130430         if  T02err  = *blank;
129500130430           dvpodecofi = T02uni;
129600130430         endif;
129700130430
129800130430       if  §vpofgvf1<>'999'  ;
129900130430           eval alfafgs=%editc(vidfgs:'X')  ;
130000130430       if  alfafgs<>§vpofgvf1 and alfafgs<>§vpofgvf2 and
130100130430           alfafgs<>§vpofgvf3 and alfafgs<>§vpofgvf4 and
130200130430           alfafgs<>§vpofgvf5 ;
130300130430          Vidmsg = Msg(08);
130400130430          *in90=*on  ;
130500130430          *in28=*on  ;
130600130430          *in99=*on  ;
130700130430           leavesr;
130800130430         endif         ;
130900130430         endif         ;
131000130430      /end-free
131100130314     c
131200940912     C                   ENDSR
131300130503      **************************************************************************
131400130503      *  Calcolo meno 1 gg e più 1 gg lavorativi da data foglio viaggio
131500130503      **************************************************************************
131600130503     c     Sr_MinMax     BegSr
131700130503
131800130503     c                   Clear                   Ktfa
131900130503     c                   Z-Add     VidFgs        Ktfp
132000130503     c                   Move      datasys       wdtaeur
132100130503      * meno 1 gg. lavorativo
132200130503     c                   Do        *Hival
132300130503     c                   Subdur    1:*d          wdtaeur
132400130503     c                   extrct    wdtaeur:*d    wgiorno
132500130503     c                   extrct    wdtaeur:*m    kmes
132600130503     c                   extrct    wdtaeur:*y    kann
132700130503     c     kcln1         Chain     Azcln01l
132800130503     c                   If        Not %Found(azcln01l)
132900130503     c                   Leave
133000130503     c                   EndIf
133100130503     c                   If        POM(wgiorno) = 'F'
133200130503     c                   Iter
133300130503     c                   EndIf
133400130503     c                   Leave
133500130503     c                   EndDo
133600130503     c                   Move      wdtaeur       wdtaiso
133700130503     c                   Move      wdtaiso       wdtameno1
133800130503
133900130503      * meno 2 gg. lavorativ1 (non imposto wdataeur perchè già impostato
134000130503     c*  con data foglio-1)
134100130503     c                   Do        *Hival
134200130503     c                   Subdur    1:*d          wdtaeur
134300130503     c                   extrct    wdtaeur:*d    wgiorno
134400130503     c                   extrct    wdtaeur:*m    kmes
134500130503     c                   extrct    wdtaeur:*y    kann
134600130503     c     kcln1         Chain     Azcln01l
134700130503     c                   If        Not %Found(azcln01l)
134800130503     c                   Leave
134900130503     c                   EndIf
135000130503     c                   If        POM(wgiorno) = 'F'
135100130503     c                   Iter
135200130503     c                   EndIf
135300130503     c                   Leave
135400130503     c                   EndDo
135500130503     c                   Move      wdtaeur       wdtaiso
135600130503     c                   Move      wdtaiso       wdtameno2
135700130503      * più 1 gg. lavorativo
135800130503     c                   Move      datasys       wdtaeur
135900130503     c                   Do        *Hival
136000130503     c                   Adddur    1:*d          wdtaeur
136100130503     c                   extrct    wdtaeur:*d    wgiorno
136200130503     c                   extrct    wdtaeur:*m    kmes
136300130503     c                   extrct    wdtaeur:*y    kann
136400130503     c     kcln1         Chain     Azcln01l
136500130503     c                   If        Not %Found(azcln01l)
136600130503     c                   Leave
136700130503     c                   EndIf
136800130503     c                   If        POM(wgiorno) = 'F'
136900130503     c                   Iter
137000130503     c                   EndIf
137100130503     c                   Leave
137200130503     c                   EndDo
137300130503     c                   Move      wdtaeur       wdtaiso
137400130503     c                   Move      wdtaiso       wdtapiu1
137500130503
137600130503     c                   EndSr
137700130503
137800951031     C*****************************************************************
137900951031** MESSAGGI DI ERRORE
138000940913Immettere data iniziale minore o uguale a quella finale                        1
138100940913Immettere data formalmente valida                                              2
138200940913Non trovati fogli di viaggio per la selezione richiesta                        3
138300951031Immettere un foglio di viaggio esistente                                       4
138400960313Verranno selezionati solo i primi xxx Fogli Viaggio: enter x continuare        5
138500130430Opzione "5" non ammessa: immettere l'opzione "1" per selezionare e modificare  6
138600130430Interrogazione non ammessa: premere F3 e riproporre la richiesta in gestione   7
138700130430Utente di filiale non abilitata all'uso di questa funzione.Enter per terminare 8
138800130503Data DAL usata in APERTURA errata: non compresa nei limiti previsti            9
138900130314Data DAL obbligatoria
139000130314Non è possibile visualizzare tutti i fogli richiesti: superano i 9990!!!
139100130502Opzione non ammessa                                                           12
139200130319Immettere un numero di TRAINO esistente                                       13
139300130319Immettere codice TRAZIONISTA esistente                                        14
139400130426Non sono presenti BOLLE BORDERIZZATE!!  Premere F14 per forzare               15
139500130426Non sono presenti SPUNTE!!  Premere F14 per forzare                           16
139600130503La data Foglio Viaggio e' un giorno festivo: enter per forzare                17
139700951031**
139800130417 * GESTIONE FOGLI VIAGGIO PARTENZA *
139900130417 ** INTERROGAZIONE FOGLI VIAGGIO  **
140000130417 ** S C E L T A    FOGLI VIAGGIO  **
140100130426 * CHIUSURA FOGLI VIAGGIO PARTENZA *
