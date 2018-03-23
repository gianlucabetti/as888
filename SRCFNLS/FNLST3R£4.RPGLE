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
004100130611     d fnlst9ds      e ds
004200130611     d trul90ds      e ds
004300130319     D*-----------------------------------------------------
004400130319      * Parametri per la ricerca trazionisti     - TRUL16R -
004500130319     D PARAM3          DS
004600130319     D* Tipologia T_=TRAZIONISTA
004700130319     D  PA3TIP                 1      2
004800130319     D* Ragione sociale trazionista
004900130319     D  PA3RSC                 3     37
005000130319     D* Sistema inform. trazionista
005100130319     D  PA3CSF                38     40
005200130319     D* Codice trazionista
005300130319     D  PA3PDR                41     47  0
005400130319     D* PA3FLG = "3" --> restituisce al pgm chiamante il messaggio che
005500130319     D*   non esistono records per la chiave alfabetica richiesta
005600130319     D  PA3FLG                48     48
005700020806      *
005800020806      * DEFINIZIONE STRUTTURE DATI INTERNE
005900020806      *
006000130319     D*-----------------------------------------------------
006100020806      * Parametri ricevuti dai pgm chiamanti
006200130314     D fnlst3ds        DS
006300130314     D* PARFLG  = "R" ---> PGM RICHIAMATO per scelta fogli viaggio
006400130314     D* PARFLG  = "I" ---> interrogazione fogli  viaggio
006500130314     D* PARFLG  = "G" ---> Gestione       fogli  viaggio
006600130426     D* PARFLG  = "C" ---> Chiusura       fogli  viaggio
006700130506     D* PARFLG  = "P" ---> interrogazione fogli  viaggio con scelta
006800130314     d*
006900951031     D  PARFLG                 1      1
007000130314     d
007100951123     D* PARFL2  = "F" ---> PASSO AL PGM CHIAMANTE I FV  SELEZIONATI
007200951123     D*                    NELLA SKIERA FVA
007300951123     D*                    SE NE VOGLIO SELEZIONARE SOLO 1 :
007400951123     D*                    PARFL2=" " E VIENE PASSATO IN PARNFV
007500951102     D  PARFL2                 2      2
007600020806     D* Numero Foglio Viaggio: se è pieno significa che è richiesta
007700020806     D*   la visualizzazione di quello specifico foglio
007800951031     D  PARNFV                 3      5P 0
007900020806     D* Data iniziale e finale in GG/MM/AAAA
008000951031     D  PARDIN                 6     10P 0
008100951031     D  PARDFI                11     15P 0
008200020806     D* PARTFV è il tipo FV da visualizzare:  " " = FV TUTTI
008300951031     D*                                       "1" = FV PARTITI (CHIUSI)
008400951031     D*                                       "2" = FV NON PARTITI (APERTI)
008500951031     D  PARTFV                16     16
008600020806     D* FVA è la schiera contenente tutti i FV selezionati
008700020806     D*        (da riempire solo se PARFL2 = "F")
008800951031     D  FVA                   17    241P 0
008900951031     D                                     DIM(45)
009000000609     D  PARFGS               242    244  0
009100020806      *
009200130319     D*-----------------------------------------------------
009300130318      * Parametri per pgm FNLST4R
009400130318     D fnlst4ds        DS
009500130318     D* ilst4gES = M - MANUTENZIONE FV RICHIAMATA
009600130318     D*          B - DA BORDERO'
009700130318     D*          A - apertura foglio
009800130318     D*          C - DA CHIUSURA FV SINGOLA
009900130318     D*          I - DA CHIUSURA DA IMP
010000130318     D*          N - DA INTERROGAZIONE  FV
010100130419     D*          M - Gestione fogli viaggio
010200130419     D*          P - Gestione fogli viaggio con videata piombi
010300130603     d
010400130603     d* solo per un po' N=nuova chiusura
010500130603     D  ilst4NEWC              2      2
010600130319     c* La data serve solo per immissione
010700130319     D  ilst4dfv               3     10  0
010800130319     d
010900130319     D  ilst4nfv              11     15
011000130318     D  IlsT4GES              16     16
011100130318     D* Se PARMSG pieno --> c'è stato ERRORE
011200130318     D  olst4msg              17     94
011300130429     D  iolsT4FOR             95     97
011400130429     D  IlsT4OLD              98     98
011500130318
011600130318     D* F12 = 'S'    --> premuto F12
011700130318     D  olst4F12             102    102
011800130318     D  ilst4fgs             103    105
011900130319 i   D  ilst4TRN             106    112  0
012000130318     D  ilst4NRRP            113    125
012100130319     D* se non immessa il numero relativo record della proposta
012200130319     d*  si può immettere a modo vecchio il trazionista
012300130319     D  ilst4PDR             126    132
012400130315     d*
012500130319     D*-----------------------------------------------------
012600130315     d* Richiamo a pgm di proposte traini
012700130315     D FNLST1DS        DS
012800130319     d* 'A' --> ricerca proposte per apertura FGV
012900130315     D  Ilst1GES               1      1
013000130315     D  Ilst1DAT               2      9
013100130315     D* 'G' --> caricare solo le proposte traini del giorno della settimana
013200130315     D*         corrispondente alla data;
013300130315     D* ' ' --> caricare tutte le proposte/traini alla data decorrenza
013400130315     D  Ilst1TUT              10     10
013500130315     D  Ilst1fgs              11     13
013600130417     d* scelta automatica del programma senza proposta videata
013700130417     D  Ilst1aut              14     14
013800020806      *
013900900515     D WLBDAT          DS
014000940913     D  G02DAT                 1      8  0
014100940913     D  G02INV                 9     16  0
014200940913     D  G02ERR                17     17
014300940913     D  G02TGI                18     22  0
014400040611      *
014500040611      * STATUS DS
014600040611      *
014700040611     d Status         sds
014800040611     d   SDSprm          *parms
014900020806      *
015000130503     D                 DS
015100130503     D  CLNPOM                 1     31
015200130503     D  POM                    1     31
015300130503     D                                     DIM(31)
015400130503     D                 DS
015500130503     D  CLNMAT                 1     31
015600130503     D  MAT                    1     31
015700130503     D                                     DIM(31)
015800130503     D                 DS
015900130503     D  GG                     1      2  0
016000130503     D  MM                     3      4  0
016100130503     D  AA                     5      8  0
016200130503     D  WDATA                  1      8  0
016300020806      * VARIABILI
016400020806      *
016500130503     D COD             S                   LIKE(TBLCOD)
016600130503     D KEY             S                   LIKE(TBLKEY)
016700130503     D KTFP            S                   LIKE(FGVLNA)
016800130503     D KTFA            S                   LIKE(FGVLNA)
016900020806     D WDATE           S                   LIKE(FGVDFV)
017000130426     D Knfv6           S                   LIKE(brvnfv) INZ
017100130426     D Knfv5           S                   LIKE(fgvnfv) INZ
017200020806     D DATFIW          S                   LIKE(FGVDFV) INZ
017300020806     D DATINW          S                   LIKE(FGVDFV) INZ
017400130314     D savFIW          S                   LIKE(FGVDFV) INZ
017500130314     D savINW          S                   LIKE(FGVDFV) INZ
017600130322     D savrtp          S                   LIKE(vidrtp) INZ
017700130322     D savrta          S                   LIKE(vidrta) INZ
017800130426     D savtfog         S                   LIKE(tipfog) INZ
017900130503     D KANN            S                   LIKE(CLNANN)
018000130503     D KMES            S                   LIKE(CLNMES)
018100130314     D knpg            S                   LIKE(brvnpg) INZ(1)
018200130322     D KEPA            S                   LIKE(FV2EPA)
018300130322     D KTDH            S                   LIKE(FV2TDH)
018400130322     D KLAI            S                   LIKE(FV2LAI)
018500130429     D KTRC            S                   LIKE(FV4trc)
018600020806     D WTIME           S             14  0
018700020806     D NRR4            S              4  0 INZ
018800020806     D XZ              S              3  0 INZ
018900020806     D W0020           S              2  0 INZ
019000020806     D WRTA            S              1    INZ
019100041129     D Werrore         S              1    INZ
019200130430     D alfaFGS         S              3    INZ
019300130319     D KTLTIP          S                   LIKE(TLZTIP) INZ('T ')
019400130319     D KTLPDR          S                   LIKE(TLZPDR)
019500130319     D KPDR            S                   LIKE(fgvPDR)
019600130503     d Datasys         s               d   datfmt(*eur) inz(*sys)
019700130503     d wdtaeur         s               d   datfmt(*eur)
019800130503     d wdtaiso         s               d   datfmt(*iso)
019900130503     d wdtameno1       s              8  0
020000130503     d wdtameno2       s              8  0
020100130503     d wdtapiu1        s              8  0
020200130503     d wgiorno         s              2  0
020300130611     D wrichtrul90     S              1
020400130612     D ww02            S              1
020500040611      *
020600040611      * Parametri aggiuntivi:
020700040611      * - P.O. di cui selezionare i F.V. (da FIMS03R)
020800040611     d DLSpo           s              3
020900040630     d DLSpo2          s              3
021000040611     d wDLSpo          s              3  0
021100040630     d wDLSpo2         s              3  0
021200020806
021300130430      //---------------------------------------------------------------
021400130430      /copy gaitrasrc/srcprotopr,tibs02r
021500130430
021600020806      *****************************************************************
021700020806      *  RIEPILOGO INDICATORI
021800020806      *****************************************************************
021900130314      * 02    - interrogazione   OFF - gestione
022000130426      * 05    - Chiusura foglio viaggio singola
022100130314      * 06    - FOGLIO VIAGGIO IN GESTIONE AD ALTRO P.O.
022200020806      * 10    - ON SELEZIONE PER DATA
022300020806      * 11    - RICHIESTA LA VISUALIZZAZIONE DEL FOGLIO VIAGGIO PASSATO
022400020806      *         DAL PGM CHIAMANTE
022500020806      * 13    - VISUALIZZO SOLO I F.V. APERTI
022600020806      * 14    - P.O. GESTIONE ERRATO
022700130314      * 20    - PROGRAMMA RICHIAMATO PER SCELTA FOGLI VIAGGIO
022800130523      * 21    - proteggo scelte nel sfl ctl
022900020806      * 22    - DEVO PASSARE AL PGM CHIAMANTE TUTTI I F.V. SELEZIONATI
023000130429      * 25    - Reverse immage per pimobi non inseriti
023100130523      * 26    - Reverse immage per targa rimorchio  non inserita
023200020806      * 28    - INDICATORE DI EMISSIONE MESSAGGIO DI ERRORE
023300020806      * 29    - READC SUBFILE
023400020806      * 31/33 - DI COMODO
023500020806      * 35    - TIPO UTENTE
023600020806      * 36    - SFLNXTCHG
023700020806      * 40    - ERRORE
023800020806      * 48    - ERRORE
023900020806      * 72/73 - SFLCLR SFLDSP E SFLDSPCTLM           SFL04
024000020806      * 76/77 - ERRORE
024100020806      * 90    - INDICATORE DI ERRORE GENERICO
024200020806      * 95    - COMODO
024300130430      * 99    - Errore grave: esco dal pgm
024400020806      *****************************************************************
024500130314     c
024600130314     C                   EXSR      PULIZ
024700130314     c
024800130426    1c                   select
024900130426     c* INTERROGAZIONE
025000130506     c                   when      parflg='I' or parflg='P'
025100130314     c                   seton                                        02
025200130417     C                   MOVE      TES(2)        VIDTES
025300130506     c                   if        parflg='P'
025400130506     C                   SETON                                        20
025500130506     c                   endif
025600130314     C
025700130426     c* CHIUSURA FOGLI VIAGGIO
025800130426     c                   when      parflg='C'
025900130426     c                   seton                                        0205
026000130426     C                   MOVE      TES(4)        VIDTES
026100130426     c                   eval      tipfog='2'
026200130612     c                   clear                   viddin
026300130612     c                   exsr      ctrd02
026400130603     c* se parfl2='N' significa che devo sottomettere il nuovo pgm di
026500130603     c*  chiusura FGV
026600130612      /free
026700130612           eval alfafgs=%editc(vidfgs:'X')  ;
026800130612       if parfl2='N'       ;
026900130612
027000130612       if  §vpofgvf1<>'999'  ;
027100130612       if  alfafgs<>§vpofgvf1 and alfafgs<>§vpofgvf2 and
027200130612           alfafgs<>§vpofgvf3 and alfafgs<>§vpofgvf4 and
027300130612           alfafgs<>§vpofgvf5 ;
027400130612          Vidmsg = Msg(08);
027500130612          *in90=*on  ;
027600130612          *in28=*on  ;
027700130612          *in99=*on  ;
027800130612         endif         ;
027900130612         endif         ;
028000130612
028100130612         else          ;
028200130612       if  §vpofgvf1= '999'  or
028300130612           alfafgs= §vpofgvf1 or  alfafgs= §vpofgvf2 or
028400130612           alfafgs= §vpofgvf3 or  alfafgs= §vpofgvf4 or
028500130612           alfafgs= §vpofgvf5 ;
028600130612          *in90=*on  ;
028700130612          *in28=*on  ;
028800130612          *in99=*on  ;
028900130612          vidmsg=msg(18)  ;
029000130612         endif         ;
029100130612         endif         ;
029200130612      /end-free
029300130426     C
029400130314     c                   when      parflg='G'
029500130314     c                   setoff                                       02
029600130314     c
029700130506     c* RICERCA FOGLI
029800130314     c                   when      parflg='R'
029900130417     C                   MOVE      TES(3)        VIDTES
030000940323     C                   SETON                                        20
030100951106     C                   Z-ADD     PARDIN        VIDDIN
030200951106     C                   Z-ADD     PARDFI        VIDDFI
030300951102     C                   MOVEL     PARTFV        TIPFOG
030400000609     C                   Z-ADD     PARFGS        VIDFGS
030500130523     c
030600130523     c* se ho passato partfv = "2" proteggo le scelte perchè non mi servono
030700130523     c                   if        partfv='2'
030800130523     c                   seton                                        21
030900130523     c                   endif
031000130523     c
031100130314     c* controllo le date passate
031200130314     c                   exsr      ctrd02
031300130314     c* se errore --> esco
031400130314     c                   if        *in90
031500130314     c                   goto      fine
031600130314     c                   endif
031700130314     C*
031800130314     c* CONTROLLO SE DEVO PASSARE AL PGM CHIAMANTE TUTTI I FOGLI DI
031900130314     C*   VIAGGIO SELEZIONATI (PARFL2 = "F")
032000130314    2C     PARFL2        IFEQ      'F'
032100130314     C                   SETON                                        22
032200130314     C                   MOVEA     *ZEROS        FVA
032300130314    2C                   ENDIF
032400130314     C
032500130314    1c                   ENDSL
032600951031     C*
032700130314     C     INIZIO        TAG
032800130314     C                   EXSR      RIES04
032900951031     C*
033000951031     C* VISUALIZZAZIONE SFL E SCELTA FOGLIO
033100020806     C                   Z-ADD     1             XZ
033200960313     C                   Z-ADD     1             REC4
033300020806     C*    =================
033400951031     C     FORCT         TAG
033500020806     C*    =================
033600130314     C                   WRITE     LST3T01
033700130314     C                   WRITE     LST3Z02
033800130314     c* se non caricati fogli emetto videata
033900130315     C  n73              WRITE     LST3d03
034000130314
034100130612     c* se immissione multipla con f15 emissione window per richiesta
034200130612     c* stampa etichette
034300130612     c                   if        ww02='S'
034400130612     c                   clear                   ww02
034500130612     C                   write     LST3C02
034600130612     c                   exsr      geswdw02
034700130612     c                   endif
034800130612     c*
034900130314     C                   EXFMT     LST3C02
035000130419     C                   SETOFF                                       284048
035100960313     C                   CLEAR                   VIDMSG
035200951031     C*
035300130314     C* F3 o F12 - Fine/Ritorno
035400130430     c*            o per errore grave esco
035500130314    1c                   if        *inKC or *inKL
035600130430     c                             or *in99
035700130314     C                   GOTO      FINE
035800130314    1c                   endif
035900130503     c* controllo le selezioni immesse
036000130503     c                   exsr      Ctrd02
036100130503     c* Errore
036200130503    1c                   if        *in90
036300130503     c                   goto      forct
036400130503    1c                   endif
036500130430    c
036600130419     c* F13 - Immissione multipla con   videata
036700130419     c* F15 - Immissione multipla SENZA videata
036800130419    1c                   if        *inKP or *inkM
036900130314     c                   exsr      RicercaTRN
037000130612     c                   if        *inkp
037100130612     c                   eval      ww02='S'
037200130612     c                   endif
037300130314     c                   goto      inizio
037400130314    1c                   endif
037500130419    c
037600130417     c* F10 - Immissione singola  non dovrebbe servire ...
037700130417    1c                   if        *inKJ and not *inkj
037800130314     c                   exsr      Immissione
037900130314     c                   goto      inizio
038000130314    1c                   endif
038100130419    c
038200130419     c* F10 - Immissione singola  senza copia traino
038300130419    1c                   if        *inKJ
038400130419     c                   clear                   fnlst4ds
038500130419     c                   eval      ilst4ges='A'
038600130419     c                   eval      ilst4fgs =%editc(vidfgs:'X')
038700130419     c                   eval      ilst4dfv=datinw
038800130419     c                   eval      ilst4old='S'
038900130419     c                   eval      kpjbu=fnlst4ds
039000130419     c                   call      'FNLST4R'
039100130419     c                   parm                    kpjba
039200130419     c                   eval      fnlst4ds=kpjbu
039300130419     c
039400130419     c                   if        olst4msg<>*blanks
039500130419     c                   movel     olst4msg      vidmsg
039600130419     c                   seton                                        2848
039700130419     c                   goto      forct
039800130419     c                   else
039900130419     c                   goto      inizio
040000130419     c                   endif
040100130419     c
040200130419    1c                   endif
040300130314     c
040400130314     c
040500130314     c* se scelto un foglio lo visualizzo
040600130314    1c                   select
040700130314     c*  se cambiate date --> ricarico
040800130314     c                   when      vidnfv>0
040900130419     c                   if        not *in20 and not *in02
041000130315     c                   eval      vidsce='2'
041100130419     c                   endif
041200130506     c                   if        *in02     and not *in05
041300130419     c                   eval      vidsce='5'
041400130419     c                   endif
041500130506     c                   if        *in05
041600130506     c                   eval      vidsce='2'
041700130506     c                   endif
041800130314     c                   exsr      VisualFGV
041900130419     c                   if        vidmsg<>*blanks
042000130419     c                   seton                                        289048
042100130419     c                   else
042200130314     c                   clear                   vidnfv
042300130419     c                   endif
042400130314     c                   goto      forct
042500130314
042600130314     c
042700130314     c                   when      datfiw<>savfiw or
042800130314     c                             datinw<>savinw
042900130314     c                   goto      inizio
043000130426     c
043100130322     c                   when      vidrtp<>savrtp or
043200130322     c                             vidrta<>savrta
043300130322     c                   goto      inizio
043400130426
043500130426     c                   when      tipfog<>savtfog
043600130426     c                   goto      inizio
043700130314     c
043800130314     c* Se immesso numero foglio --> lo visualizzo
043900951031     C*
044000130314   x1c                   other
044100130315     c                   z-add     1             nrr4
044200130430     c
044300130430     c* e non ho caricato fogli non eseguo la lettura
044400130430   1dc                   if        *in73
044500951031     C     LEGGI         TAG
044600130315     C                   READC     LST3S02                                29
044700951031     C*
044800951031     C* SE SELEZIONATO ALMENO UNO ESCO ALTRIMENTI RIEMETTO SUBFILE
044900130315    1c                   if        *in29
045000130315    2C                   IF        *in22  and XZ >  1
045100951031     C                   GOTO      FINE
045200951031    2C                   ENDIF
045300951031     C*
045400130419     C                   GOTO      forct
045500130315    1c                   endif
045600960126     C**
045700130419     c* Per gestione ammessa solo opzione 2 / 5 / P
045800130315   1ac                   if        vidsce<>' '
045900130430
046000130430     c* Se chiusura foglio e scelta con "2" --> controllo se ha bolla
046100130430     c**   o spunte il foglio
046200130426     c
046300130426     c                   if        not *inkn and vidsce='2' and *in05
046400130426     c                   eval      knfv5=fgvnfv
046500130426     c                   exsr      contrbolle
046600130426     c                   if        *in90
046700130426     c                   seton                                        3640
046800130426     C                   Z-ADD     NRR4          REC4
046900130429     c                   exsr      AggioSFL
047000130426     C                   GOTO      forct
047100130426     c                   endif
047200130426     c                   endif
047300130502     c
047400130502     c                   if        *in05 and vidsce<>'2' and vidsce<>'5'
047500130502     c                   seton                                        369040
047600130502     c                   seton                                        28
047700130502     C                   Z-ADD     NRR4          REC4
047800130502     C                   MOVEA     MSG(12)       VIDMSG
047900130502     c                   exsr      AggioSFL
048000130502     C                   GOTO      forct
048100130502     c                   endif
048200130419     c
048300130314     c                   if        parflg='G' and vidsce<>'2'
048400130315     c                             and vidsce<>'5'and vidsce<>' '
048500130419     c                             and vidsce<>'P'
048600130314     c                   seton                                        369040
048700130314     c                   seton                                        28
048800130315     C                   Z-ADD     NRR4          REC4
048900130315     C                   MOVEA     MSG(12)       VIDMSG
049000130429     c                   exsr      AggioSFL
049100130314     C                   GOTO      forct
049200130314     c                   endif
049300130419     c
049400130314     c* Per Interrogazione solo opzione 5
049500130314     c                   if        parflg='I' and
049600130315     c                                 vidsce<>'5'  and vidsce<>' '
049700130314     c                   seton                                        369040
049800130314     c                   seton                                        28
049900130315     C                   MOVEA     MSG(12)       VIDMSG
050000130315     C                   Z-ADD     NRR4          REC4
050100130429     c                   exsr      AggioSFL
050200130314     C                   GOTO      forct
050300130314     c                   endif
050400130506     c                   if        parflg='P' and   vidsce<>'1' and
050500130506     c                                 vidsce<>'5'  and vidsce<>' '
050600130506     c                   seton                                        369040
050700130506     c                   seton                                        28
050800130506     C                   MOVEA     MSG(12)       VIDMSG
050900130506     C                   Z-ADD     NRR4          REC4
051000130506     c                   exsr      AggioSFL
051100130506     C                   GOTO      forct
051200130506     c                   endif
051300130315     c
051400130315     c* Per Ricerca        solo opzione 1
051500130314     c                   if        parflg='R' and
051600130315     c                                 vidsce<>'1'  and vidsce<>' '
051700130314     c                   seton                                        369040
051800130314     c                   seton                                        28
051900130315     C                   Z-ADD     NRR4          REC4
052000130315     C                   MOVEA     MSG(12)       VIDMSG
052100130429     c                   exsr      AggioSFL
052200130314     C                   GOTO      forct
052300130314     c                   endif
052400130314     c*
052500130314    2c                   if        vidsce='2' or  vidsce='5'
052600130419    2c                             or vidsce='P'
052700130314     c                   exsr      visualFGV
052800130419     c
052900130315     C                   Z-ADD     NRR4          REC4
053000130314     c* Se ce' errore devo riemettere sfl
053100130429     c* se non c'e' errore riaggiorno sfl con i dati del foglio
053200130419     c                   if        not *in90
053300130419     C                   MOVEL     ' '           VIDSCE
053400130419     c     kfgvsfl       chain     fnfgv01l
053500130419     c                   exsr      rigaSFL
053600130419     c                   endif
053700130419     c
053800130314     c                   seton                                        3640
053900130429     c                   exsr      AggioSFL
054000130315     c                   if        *in90
054100130314     C                   GOTO      forct
054200130315     c                   endif
054300130314     C*
054400130314     C                   GOTO      LEGGI
054500130314    2C                   ENDIF
054600960318     C*
054700130314     C* SE scelta    SELEZIONO IL FOGLIO CON L'OPZIONE "1"
054800130506    2c                   if        vidsce='1'
054900960126     C* PULISCO IL CAMPO DI SCELTA
055000020806     C                   CLEAR                   VIDSCE
055100130429     c                   exsr      AggioSFL
055200130314     c
055300960126     C* 22 OFF - PASSO UN SOLO NUMERO DI FV
055400130314    3C     *IN22         IFEQ      *OFF
055500960126     C                   Z-ADD     FGVNFV        PARNFV
055600020806     C                   z-add     VIDfgs        PARfgs
055700960126     C                   GOTO      FINE
055800960313     C*
055900130314   X3C                   ELSE
056000951031     C* 22 ON  - MEMORIZZO TUTTI I FV SELEZIONATI
056100020806    5C     XZ            IFLE      45
056200020806     C                   z-add     VIDfgs        PARfgs
056300020806     C                   Z-ADD     FGVLNA        FVA(XZ)
056400020806     C                   MOVEL     FGVNFV        FVA(XZ)
056500020806     C                   ADD       1             XZ
056600960126     C                   GOTO      LEGGI
056700960313   X5C                   ELSE
056800960313     C* SE SONO STATI SELEZIONATI PIU' DI 45 F.VIAGGIO: ERRORE
056900020806     C                   SUB       1             XZ
057000960313     C*
057100960313     C                   MOVEA     MSG(5)        K78
057200020806     C                   MOVEL     XZ            W003A             3
057300960313     C                   MOVEA     W003A         K05(3)
057400960313     C* ABBLANCO ZERO NON SIGNIFICATIVI
057500020806     C                   Z-ADD     3             XZ
057600020806    6C     K05(XZ)       DOWEQ     '0'
057700020806     C     XZ            ANDLT     5
057800020806     C                   CLEAR                   K05(XZ)
057900020806     C                   ADD       1             XZ
058000960313    6C                   ENDDO
058100960313     C*
058200960313     C                   MOVEA     K05(3)        K78(35)
058300960313     C                   MOVEA     K78           VIDMSG
058400960313     C                   SETON                                        4028
058500960313     C                   Z-ADD     NRR4          REC4
058600960313     C*
058700130315     C     NRR4          CHAIN     LST3S02                            33
058800960313     C                   MOVEL     '1'           VIDSCE
058900130429     c                   exsr      AggioSFL
059000960313     C                   GOTO      FORCT
059100960313    5C                   ENDIF
059200960313     C*
059300960126    3C                   ENDIF
059400960126    2C                   ENDIF
059500130430   1aC                   ENDIF
059600951031     C*
059700951102     C* PULISCO IL CAMPO DI SCELTA
059800130419     c                   if        vidsce<>' '
059900130419     C                   SETON                                        4036
060000130419     c                   else
060100130419     C                   SETOff                                       40
060200130419     c                   endif
060300130419     c
060400130419     C                   CLEAR                   VIDSCE
060500130315     c                   setoff                                       36
060600130429     c                   exsr      AggioSFL
060700130430   1bC                   ENDIF
060800130314    1C                   ENDsl
060900951031     C*
061000130430     c   73              goto      leggi
061100130430     c  n73              goto      forct
061200130314     c
061300000000     C     FINE          TAG
061400130315     C   20              MOVEL     fnlst3ds      KPJBU
061500130611
061600130611     C                   Z-ADD     130           LUNG             15 5
061700130611     c                   eval      comman='DLTOVR FILE(*ALL)'
061800130611     C                   CALL      'QCMDEXC'                            91
061900130611     C                   PARM                    COMMAN
062000130611     C                   PARM                    LUNG
062100130611
062200000000     C                   SETON                                        LR
062300951031     C*-----------------------------------------------------***********
062400951031     C* PULIZIA CAMPI VIDEO                                 *  PULIZ  *
062500951031     C*-----------------------------------------------------***********
062600900515     C     PULIZ         BEGSR
062700000620     C                   SETOFF                                       10  15
062800951102     C                   CLEAR                   VIDNFV
062900041129     C                   z-add     wdate         VIDDIN
063000940913     C                   Z-ADD     WDATE         VIDDFI
063100951102     C                   CLEAR                   TIPFOG
063200130322     C                   CLEAR                   VIDRTP
063300130322     C                   CLEAR                   VIDRTA
063400130319     c                   exsr      ctrd02
063500900514     C                   ENDSR
063600130429     C*-----------------------------------------------------***********
063700130429     C* Aggioranemto SFL                                    *  PULIZ  *
063800130429     C*-----------------------------------------------------***********
063900130429     C     AggioSFL      BEGSR
064000130429     c                   if        vidpr_p='NO'
064100130429     c                   seton                                        25
064200130429     c                   endif
064300130523     c                   if        vidpr_t='NO'
064400130523     c                   seton                                        26
064500130523     c                   endif
064600130429     c
064700130429     C                   UPDATE    LST3S02
064800130429     c                   setoff                                       364025
064900130523     c                   setoff                                       26
065000130523     c
065100130429     c                   ENDSR
065200951031     C*-----------------------------------------------------***********
065300130314     C* CARICAMENTO  FOGLIO VIAGGIO
065400951031     C*-----------------------------------------------------***********
065500951031     C     RIES04        BEGSR
065600951031     C*
065700940912     C                   Z-ADD     1             NRR4
065800900517     C                   SETON                                        72
065900130315     C                   WRITE     LST3C02
066000911205     C                   SETOFF                                       7213
066100130430     c*
066200130430     c* Se errore grave --> esco dalla routine
066300130430     c                   if        *in99
066400130430     c                   leavesr
066500130430     c                   endif
066600130430     c
066700130430     C                   SETON                                        73
066800911205     C*
066900951031     C* 13 ON  - VISUALIZZO SOLO I FV APERTI
067000951102    1C     TIPFOG        IFEQ      '2'
067100911205     C                   SETON                                        13
067200951031    1C                   ENDIF
067300951031     C*
067400951031     C* CARICO IL SUBFILE
067500130314    1c                   if        not *in13
067600130314     c     KFGV02        setll     FNFGV02L
067700130314     c     vidfgs        reade     FNFGV02L                               31
067800020806     c                   else
067900130314     c     vidfgs        setll     FNFGV06L
068000130314     c     vidfgs        reade     FNFGV06L                               31
068100130314    1c                   endif
068200951031     C*
068300951031    1C     *IN31         DOWEQ     *OFF
068400041130     c     nrr4          andle     9990
068500951031     C*
068600951031    2C     FGVDFV        IFLE      DATFIW
068700900529     C                   SETOFF                                       95
068800951031     C*
068900951031    3C   13FGVDFV        IFGE      DATINW
069000911205     C                   SETON                                        95
069100951031    3C                   ENDIF
069200951031     C*
069300960219     C                   SELECT
069400960219     C     TIPFOG        WHENEQ    '1'
069500020806    3C     FGVFCF        ANDNE     *BLANKS
069600900529     C                   SETON                                        95
069700951031     C*
069800020806    3C     TIPFOG        WHENEQ    *BLANKS
069900900529     C                   SETON                                        95
070000960219    3C                   ENDSL
070100040611      *
070200040611      * Verifico P.O. arrivo
070300040630    3c                   if             *in95   =  *on
070400040630     c                             AND (wDLSpo  <> *zeros
070500040630     c                             and  wDLSpo  <> FGVlna
070600040630     c                             and  wDLSpo2 =  *zeros
070700040630     c                              OR  wDLSpo2 <> *zeros
070800040630     c                             and  wDLSpo  <> FGVlna
070900040630     c                             and  wDLSpo2 <> FGVlna)
071000040611     c                   eval      *in95 = *off
071100040611    3c                   endif
071200130322     c
071300130322     C* VERIFICO CON IL RITARDO PARTENZA
071400130322    3C   95VIDRTP        IFNE      *BLANKS
071500130322     C* CHAIN SU FV2
071600130322     C                   MOVEL     'P'           KEPA
071700130322     C                   CLEAR                   KLAI
071800130322     C                   MOVEL     'R'           KTDH
071900130322     C     KFV2          CHAIN     FNFV201L                           32
072000130322    4C     *IN32         IFEQ      *OFF
072100130322     C     FV2ATB        ANDEQ     *BLANKS
072200130322     C     FV2RTC        ANDNE     *BLANKS
072300130322     C* SE HO CHIESTO I FV SENZA RITARDO ERRORE
072400130322    5C     VIDRTP        IFEQ      'N'
072500130322     C                   SETOFF                                       95
072600130322    5C                   ENDIF
072700130322   X4C                   ELSE
072800130322     C*
072900130322     C* SE HO CHIESTO I FV CON   RITARDO ERRORE
073000130322    5C     VIDRTP        IFEQ      'R'
073100130322     C                   SETOFF                                       95
073200130322    5C                   ENDIF
073300130322    4C                   ENDIF
073400130322    3C                   ENDIF
073500130322     C**
073600130322     C* VERIFICO CON IL RITARDO ARRIVO
073700130322    3C   95VIDRTA        IFNE      *BLANKS
073800130322     C* CHAIN SU FV2
073900130322     C                   CLEAR                   WRTA
074000130322     C                   MOVEL     'A'           KEPA
074100130322     C                   MOVEL     'R'           KTDH
074200130322     C     KFV3          CHAIN     FNFV203L                           32
074300130322    4C     *IN32         DOWEQ     *OFF
074400130322     C*
074500130322    5C     FV2ATB        IFEQ      *BLANKS
074600130322     C     FV2RTC        ANDNE     *BLANKS
074700130322     C                   SETON                                        32
074800130322     C* SE HO CHIESTO I FV SENZA RITARDO ERRORE
074900130322    6C     VIDRTA        IFEQ      'N'
075000130322     C                   SETOFF                                       95
075100130322   X6C                   ELSE
075200130322     C* SE HO CHIESTO I FV CON RITARDO SOLO A POSTO
075300130322     C                   MOVEL     'S'           WRTA
075400130322    6C                   ENDIF
075500130322    5C                   ENDIF
075600130322     C*
075700130322     C  N32KFV3          READE     FNFV203L                               32
075800130322    4C                   ENDDO
075900130322     C*
076000130322    4C     VIDRTA        IFEQ      'R'
076100130322     C     WRTA          ANDEQ     *BLANKS
076200130322     C                   SETOFF                                       95
076300130322    4C                   ENDIF
076400130322    3C                   ENDIF
076500960219     C**
076600960219     C**
076700951031     C* 95 ON  - TUTTO OK: RECORD DA VISUALIZZARE
076800951031    3C     *IN95         IFEQ      *ON
076900951031     C*
077000951031     C* CAMPO SELEZIONE
077100020806     C                   CLEAR                   VIDSCE
077200130314     C
077300130419     c                   exsr      RigaSFL
077400130315     c
077500130315     c* La prima volta accedo indicatore di posizionamento ccursore
077600130419     c                   if        nrr4=1
077700130419     c                   seton                                        4036
077800130419     c                   endif
077900951031     C*
078000130315     C                   WRITE     LST3S02
078100940912     C                   ADD       1             NRR4
078200130430     c                   setoff                                       403625
078300130523     c                   setoff                                       26
078400951031    3C                   ENDIF
078500951031     C*
078600020806     c                   if        not *in13
078700130314     c     vidfgs        reade     FGV02                                  31
078800020806     c                   else
078900130314     c     vidfgs        reade     FGV06                                  31
079000020806     c                   endif
079100951031     C*
079200951031   X2C                   ELSE
079300020806     C*
079400020806     c                   if        not *in13
079500020806     c                   eval      *in31 = *on
079600020806     c                   else
079700130314     c     vidfgs        reade     FGV06                                  31
079800020806     c                   endif
079900020806     C*
080000951031    2C                   ENDIF
080100951031     C*
080200951031    1C                   ENDDO
080300130314
080400041129     c                   eval      werrore='N'
080500130314     c* Se caricati troppi record --> segnalo l'errore
080600041130     c                   if        *in31 =*off and nrr4 >9990
080700041129     c                   eval      werrore='S'
080800130314     C                   MOVEL     MSG(11)       VIDMSG
080900130314     C                   SETON                                        28
081000041129     c                   endif
081100951031     C*
081200130426     c* salvo i campi di ricaricamento sfl
081300130314     c                   eval      savfiw=datfiW
081400130314     c                   eval      savinw=datinW
081500130322     c                   eval      savrtp=vidrtp
081600130322     c                   eval      savrta=vidrta
081700130426     c                   eval      savtfog=tipfog
081800130315    1c                   if        nrr4=1
081900130315     c                   setoff                                       73
082000130315     c                   endif
082100130315     c
082200951031     C                   ENDSR
082300130419     C*-----------------------------------------------------***********
082400130419     C* Carico dati nella riga del sfl
082500130419     C*-----------------------------------------------------***********
082600130419     c     RigaSFL       BEGSR
082700130523     c                   setoff                                       2526
082800130419     C* GIRO DATA FOGLIO VIAGGIO
082900130419     C                   Z-ADD     FGVDFV        G02INV
083000130419     C                   MOVEL     *ZERO         G02DAT
083100130419     C                   MOVEL     '3'           G02ERR
083200130419     C                   CALL      'XSRDA8'
083300130419     C                   PARM                    WLBDAT
083400130419     C                   Z-ADD     G02DAT        W0020
083500130419     C                   MOVEL     G02DAT        VIDDEL
083600130419     C                   MOVE      W0020         VIDDEL
083700130419     C* DECODIFICO LINEA DI ARRIVO
083800130419     C     FGVLNA        CHAIN     AZORG                              31
083900130419     C  N31              MOVEL     ORGDES        DE2LNA
084000130419     C   31              MOVEL     *BLANKS       DE2LNA
084100130419     C* NUMERO TRAINO
084200130419     C                   MOVEL     FGVTRN        VIDTRN
084300130419     C*
084400130419     C                   MOVEL     *BLANKS       VIDABP
084500130419     C                   MOVEL     *BLANKS       VIDimp
084600130419     c
084700130419     c                   clear                   vidabp
084800130419     C* ANNULLATO
084900130419    4C     FGVATB        IFEQ      '*'
085000130429     C                   MOVEL     'Ann   '      VIDABP
085100130419    4C                   ENDIF
085200130419     C*
085300130419     C* BORDERO'
085400130419     C     K02BLP79      setll     FNBLP79L                               31
085500130429     C   31              MOVE      'B     '      VIDABP
085600130419     c* Verifico se presenti spunte
085700130426     c                   eval      knfv6=fgvnfv
085800130419     C     Kbrv          setll     FNBrv01l                               31
085900130429     c   31              eval      %subst(vidabp:3:2)='Sp'
086000130419     C*
086100130419     C* PARTITO
086200130419    4C     FGVFCF        IFNE      *BLANKS
086300130419     C                   MOVE      'P'           VIDABP
086400130419    4C                   ENDIF
086500130419     c* Verifico che chiusura da IMP
086600130419     c                   if        fgvnfa>0
086700130419     c                   eval      vidimp='DaIMP'
086800130419     c                   endif
086900130429     c* Verifico la presenza dei piombi
087000130429     c* Escludo fogli defluenza e locali
087100130429     c                   eval      vidpr_p='  '
087200130523     c                   eval      vidpr_t='  '
087300130429     c                   if        fgvttr= ' ' or fgvttr='S'
087400130429     c                                         or fgvttr='H'
087500130429     C                   MOVEL     'P'           KEPA
087600130429     C                   CLEAR                   KLAI
087700130429     C                   MOVEL     'P'           KTrc
087800130429     C     KFV4          CHAIN     FNFV401L
087900130429    4C                   IF        %found(fnfv401l) and fv4atb=' '
088000130429     c                             and fv4not<>*blanks
088100130429     c                   else
088200130429     c                   eval      vidpr_p='NO'
088300130429     c                   seton                                        25
088400130429     c                   endif
088500130523     c
088600130523     c* Targa Rimorchio
088700130523     c                   if        fgvtrr=*blanks
088800130523     c                   eval      vidpr_t='NO'
088900130523     c                   seton                                        26
089000130523     c                   endif
089100130523     c
089200130429     c                   endif
089300130419     C*
089400130419     C* TARGHE MOTRICE E RIMORCHIO
089500130419     C                   MOVEL     FGVTRM        VIDTRM
089600130523     C*****              MOVEL     FGVTrr        VIDTrr
089700130419     c                   ENDSR
089800951031     C*-----------------------------------------------------***********
089900130314     C* CONTROLLI dati di parzializzazione
090000951031     C*-----------------------------------------------------***********
090100130314     C     CTRD02        BEGSR
090200940913     C                   SETOFF                                       907677
090300000612     C                   SETOFF                                       1406
090400000609     C*
090500000609     C* CONTROLLO DATA INIZIALE
090600951031    1C     VIDDIN        IFNE      0
090700900517     C                   MOVE      VIDDIN        G02DAT
090800900517     C                   MOVEL     *BLANK        G02ERR
090900940913     C                   CALL      'XSRDA8'
091000900517     C                   PARM                    WLBDAT
091100951031    2C     G02ERR        IFEQ      '1'
091200940913     C                   MOVEL     MSG(2)        VIDMSG
091300940913     C                   SETON                                        762890
091400130503     C                   GOTO      ENDCT2
091500951031    2C                   ENDIF
091600130503     c
091700940913     C                   Z-ADD     G02DAT        VIDDIN
091800130503     c                   if        g02inv<>datinw
091900130503     c                   clear                   wforzb            1
092000130503     c                   endif
092100130503     C                   Z-ADD     G02INV        DATINW            8 0
092200130503     C                   Z-ADD     G02DAT        WDATA
092300130503     C                   MOVEL     MM            KMES
092400130503     c
092500130503     c* In caso di apertura fogli viaggio faccio il controllo
092600130503    2c                   if        *inkj   or *inkm  or *inkp
092700130503     c
092800130503      * non può essere minore o maggiore di 1 gg. lavorativo
092900130503    3c                   If        datinw < wdtameno1  or
093000130503     c                             datinw > wdtapiu1
093100130503     c                   Eval      VidMsg = Msg(9)
093200130503     c                   Eval      *in28 = *On
093300130503     c                   Eval      *in76 = *On
093400130503     c                   Eval      *in90 = *On
093500130503     C                   GOTO      ENDCT2
093600130503    3c                   EndIf
093700130503     c
093800130503     C* Se DATA DFV CADE IN GIORNO FESTIVO PER
093900130503     C*  LA TRAINI, AVVISO. ERRORE FORZABILE
094000130503     C                   CLEAR                   KTFA
094100130503     C                   Z-ADD     68            KTFP
094200130503     C     KCLN1         CHAIN     AZCLN01L                           83
094300130503    3C  N83POM(GG)       IFNE      *BLANKS
094400130503     C                   MOVEL     '1Y'          COD
094500130503     C                   MOVEL(P)  POM(GG)       KEY
094600130503     C     KTAB          CHAIN     TABEL                              83
094700130503     C                   MOVEL     TBLUNI        DS1Y
094800130503     C*
094900130503    4C  N83§1YFET        IFNE      *BLANKS
095000130503     C     WFORZB        ANDEQ     *BLANKS
095100130503     C                   MOVEL     MSG(17)       VIDMSG
095200130503     C                   SETON                                        287690
095300130503     C                   MOVEL     '1'           WFORZB            1
095400130503    4C                   ENDIF
095500130503    3c                   endif
095600130503    2c                   endif
095700951031     C*
095800951031   X1C                   ELSE
095900940913     C                   Z-ADD     0             DATINW
096000041130     c* solo se non sono stati richiesti solo gli aperti
096100041130     c     tipfog        ifne      '2'
096200041129     C                   MOVEL     MSG(10)       VIDMSG
096300041129     C                   SETON                                        762890
096400041130     c                   endif
096500951031    1C                   ENDIF
096600130314     C   90              GOTO      ENDCT2
096700951031     C*
096800951031     C* CONTROLLO DATA FINALE
096900951031    1C     VIDDFI        IFEQ      0
097000951031    2C     VIDDIN        IFEQ      *ZERO
097100940913     C                   Z-ADD     WDATE         VIDDFI
097200951031   X2C                   ELSE
097300940921     C                   Z-ADD     VIDDIN        VIDDFI
097400951031    2C                   ENDIF
097500951031    1C                   ENDIF
097600951031     C*
097700900517     C                   MOVE      VIDDFI        G02DAT
097800900517     C                   MOVEL     *BLANK        G02ERR
097900940913     C                   CALL      'XSRDA8'
098000900517     C                   PARM                    WLBDAT
098100951031    1C     G02ERR        IFEQ      '1'
098200940913     C                   MOVEL     MSG(2)        VIDMSG
098300940913     C                   SETON                                        772890
098400951031    1C                   ENDIF
098500130314     C   90              GOTO      ENDCT2
098600940913     C                   Z-ADD     G02DAT        VIDDFI
098700940913     C                   Z-ADD     G02INV        DATFIW            8 0
098800951031     C*
098900951031     C* DATA INIZIALE MAGGIORE DATA FINALE
099000951031    1C     DATINW        IFGT      DATFIW
099100940913     C                   MOVEL     MSG(1)        VIDMSG
099200940913     C                   SETON                                        762890
099300130314     C                   GOTO      ENDCT2
099400951031    1C                   ENDIF
099500960126     C**
099600960126     C* CONTROLLO NUMERO FOGLIO VIAGGIO
099700960710    1C     VIDNFV        IFGT      0
099800130314     C     kfgv          CHAIN     FNFGV01L                           31
099900960126    2C     *IN31         IFEQ      *ON
100000960126     C                   MOVEL     MSG(4)        VIDMSG
100100960201     C                   SETON                                        284890
100200130314     C                   GOTO      ENDCT2
100300960126    2C                   ENDIF
100400130426     c* Per chiusura fogli viaggio controllo se ha bolle altrimenti
100500130426     c*  si forza con F14
100600130426     c                   if        not *inkn and parflg='C'
100700130426     c                   eval      Knfv5=vidnfv
100800130426     c                   exsr      contrBolle
100900130426     c                   if        *in90
101000130426     c                   seton                                        48
101100130426     c                   goto      endct2
101200130426     c                   endif
101300130426     c
101400130426     c                   endif
101500130426     c
101600130315    2C                   ENDIF
101700951031     C*
101800130314     C     ENDCT2        ENDSR
101900130426     C*****************************************************************
102000130426     c*   controllo se il foglio ha elle bolle per chiusrua FGV
102100130426     C*****************************************************************
102200130426     C     contrBolle    BEGSR
102300130607     c                   if        parfl2<>'N'
102400130426     C     Kblp2         SETLL     FNBLP000                               83
102500130426     c                   if        *in83
102600130426     c                   z-add     knfv5         knfv6
102700130426     C     KBRV          SETLL     FNBRV01L                               84
102800130426     c                   endif
102900130607     c
103000130607     c                   else
103100130607     c* Per la nuova borderizzazine controllo solo se ci sono spunte
103200130607     c                   seton                                        83
103300130607     C     KBRV          SETLL     FNBRV01L                               84
103400130607     c                   endif
103500130426     C*
103600130426     C* BOLLE O SPUNTE NON TROVATE --> FORZATURA
103700130426     C     *IN83         IFEQ      *OFF
103800130426     C     *IN84         OREQ      *OFF
103900130426     C                   SETON                                        9028
104000130426     C  N84              MOVEL     MSG(16)       VIDMSG
104100130426     C  N83              MOVEL     MSG(15)       VIDMSG
104200130426     C                   ENDIF
104300130426     c
104400130426     c                   ENDSR
104500130314     C*****************************************************************
104600130314     C*   Visualizza singolo foglio
104700130314     C*****************************************************************
104800130314     C     VisualFGV     BEGSR
104900130426     c                   setoff                                       90
105000130426     c* Se sono in chiusura fogli viaggio emetto finestra se si tratta
105100130426     c*  di foglio NON bis
105200130502     c                   if        vidsce<>'5'  and
105300130502     c                             parflg='C' and fgvSNP=' '
105400130426     c                             and fgvttr <>'D' and fgvttr<>'H'
105500130426     c                             and fgvttr<>'I'
105600130426    3c                   do        *hival
105700130426     c                   exfmt     lst3w01
105800130426     c* Premuto F12 riemetto videata iniziale
105900130426    4c                   if        *inkl
106000130426     C                   SETON                                        90
106100130426     c                   leave
106200130426    4c                   endif
106300130426     c
106400130426     c   kf              leave
106500130426    3c                   enddo
106600130426
106700130426     c                   endif
106800130426     c
106900130426     c                   if        not *in90
107000130318     C                   CLEAR                   fnlst4ds
107100130318     C                   MOVEL     FGVNFV        ilst4NFV
107200130318     C                   move      VIDfgs        ilst4FGS
107300130314     c* Per gestione flag =' '
107400130315     c                   if        vidsce='5'
107500130318     C                   MOVEL     'N'           Ilst4GES
107600130314     c                   endif
107700130426     c                   if        not *in05 and vidsce='2'
107800130318     C                   MOVEL     'M'           ilst4GES
107900130315     c                   endif
108000130426     c                   if        *in05 and vidsce='2'
108100130426     C                   MOVEL     'C'           ilst4GES
108200130603     c                   movel     parfl2        ilst4newc
108300130426     c                   endif
108400130419     c                   if        vidsce='P'
108500130419     C                   MOVEL     'P'           ilst4GES
108600130419     c                   endif
108700130318     C                   MOVEL     fnlst4ds      KPJBU
108800130318     C                   CALL      'FNLST4R'
108900130314     C                   PARM                    KPJBA
109000130318     C                   MOVEL     KPJBU         fnlst4ds
109100130314     C* MESSAGGIO DI ERRORE
109200130318    1C     olst4MSG      IFNE      *BLANKS
109300130318     C                   MOVEL     olst4MSG      VIDMSG
109400130314     C                   SETON                                        2890
109500130314    1C                   ENDIF
109600130426    1C                   ENDIF
109700130426     c
109800130314     C                   ENDSR
109900130314     C*****************************************************************
110000130314     C*   Ricerca Proposte per apertura FGV
110100130314     C*****************************************************************
110200130314     C     RicercaTRN    BEGSR
110300130315     c                   clear                   fnlst1ds
110400130319     c                   eval      ilst1GES='A'
110500130315     c                   movel     datinw        ilst1dat
110600130315     c                   eval      ilst1TUT='G'
110700130315     c                   movel     vidfgs        ilst1fgs
110800130417     c                   if        *inkp
110900130417     c                   eval      ilst1aut='S'
111000130417     c                   endif
111100130315     c
111200130315     c                   movel(p)  fnlst1ds      kpjbu
111300130315     c                   call      'FNLST1R'
111400130315     c                   parm                    kpjba
111500130315     c*
111600130314     C                   ENDSR
111700130314     C*****************************************************************
111800130314     C*   Immissione singolo foglio da traino
111900130314     C*****************************************************************
112000130314     C     Immissione    BEGSR
112100130319     C                   CLEAR                   coptrn
112200130319     C                   CLEAR                   vidpdr
112300130319     C                   CLEAR                   vidpdr
112400130319     c     emforw4       tag
112500130319     c                   exfmt     lst3w04
112600130319
112700130319     c                   setoff                                       289054
112800130319     c                   setoff                                       47
112900130319     c
113000130319     c                   if        *inkl
113100130319     c                   leavesr
113200130319     c                   endif
113300130319     c
113400130319     c                   exsr      ctrImm
113500130319
113600130319     c                   if        not *in90
113700130319     c
113800130319     c                   clear                   fnlst4ds
113900130319     c                   eval      ilst4ges='A'
114000130319     c                   eval      ilst4fgs =%editc(vidfgs:'X')
114100130319     c                   if        coptrn>*zeros
114200130319     c                   eval      ilst4TRN =%int(copTRN)
114300130319     c                   endif
114400130319     c                   eval      ilst4dfv=datinw
114500130319     c                   eval      ilst4pdr=vidpdr
114600130319     c                   eval      ilst4old='S'
114700130319     c                   eval      kpjbu=fnlst4ds
114800130319     c                   call      'FNLST4R'
114900130319     c                   parm                    kpjba
115000130319     c                   eval      fnlst4ds=kpjbu
115100130319     c
115200130319     c                   if        olst4msg<>*blanks
115300130319     c                   movel     olst4msg      w4cmsg
115400130319     c                   seton                                        2890
115500130319     c                   goto      emforw4
115600130319     c                   endif
115700130319     c
115800130319     c                   else
115900130319     c                   goto      emforw4
116000130319     c                   endif
116100130319     c
116200130314     C                   ENDSR
116300951031     C*****************************************************************
116400130319     C*   controlla immissione
116500951031     C*****************************************************************
116600130319     C     CtrImm        BEGSR
116700130319     c
116800130319     C     '?'           SCAN      COPTRN                                 90
116900130319    1C     *IN90         IFEQ      *ON
117000130319     C                   CLEAR                   TNTL00DS
117100130319     C                   Z-ADD     datinw        D00DDE
117200130319     C*
117300130319     C                   MOVE      VIDFGS        D00FIL
117400130319     C                   MOVEL     'C'           D00SCA
117500130319     C                   MOVEL     TNTL00DS      KPJBU
117600130319     C                   CALL      'TNTL17R'
117700130319     C                   PARM                    KPJBA
117800130319     C                   MOVEL     KPJBU         TNTL00DS
117900130319     C*
118000130319     C                   MOVEL     D00TRN        COPTRN
118100130319     C                   SETON                                        54
118200130319     C                   leavesr
118300130319    1C                   ENDIF
118400130319     C*
118500130319    1c                   if        coptrn<>*blanks and coptrn<>*zeros
118600130319     C                   TESTN                   COPTRN               83
118700130319     C                   MOVE      COPTRN        W001              1
118800130319    2C     *IN83         IFEQ      *OFF
118900130319     C     COPTRN        ORLT      *ZEROS
119000130319     C     W001          ORLT      '0'
119100130319     C                   MOVEL     MSG(13)       w4cmsg
119200130319     C                   SETON                                        549028
119300130319     C                   leavesr
119400130319    2C                   ENDIF
119500130319    1C                   ENDIF
119600130319     c
119700130319     C* RICERCA CODICE TRAZIONISTA
119800130319     C     '?'           SCAN      VIDPDR                                 83
119900130319    1C     *IN83         IFEQ      *ON
120000130319     C                   MOVEL     'T '          PA3TIP
120100130319     C                   MOVEL(P)  VIDDPD        PA3RSC
120200130319     C                   CLEAR                   PA3CSF
120300130319     C                   CLEAR                   PA3PDR
120400130319     C                   CLEAR                   PA3FLG
120500130319     C                   MOVEL     PARAM3        KPJBU
120600130319     C                   CALL      'TRUL16R'
120700130319     C                   PARM                    KPJBA
120800130319     C                   MOVEL     KPJBU         PARAM3
120900130319     C                   MOVEL     PA3PDR        VIDPDR
121000130319     C                   MOVEL     PA3RSC        VIDDPD
121100130319     C                   SETON                                        9047
121200130319     C                   LEAVESR
121300130319     C*
121400130319    1C                   ENDIF
121500130319     C*
121600130319     C* CONTROLLO VALIDITA' CODICE TRAZIONISTA
121700130319    1C     VIDPDR        IFNE      *BLANKS
121800130319     C     VIDPDR        ANDNE     *ZEROS
121900130319     C*
122000130319     C                   TESTN                   VIDPDR               31
122100130319     C  N31              MOVEL     MSG(14)       w4cmsg
122200130319     C  N31              SETON                                        902847
122300130319     C  N31              leavesr
122400130319     C*
122500130319     C                   MOVE      VIDPDR        KPDR
122600130319     C                   Z-ADD     KPDR          KTLPDR
122700130319     C     KTNTLZ        CHAIN     TNTLZ01L                           31
122800130319     C*
122900130319    2C     *IN31         IFEQ      *ON
123000130319     C     TLZATB        ORNE      ' '
123100130319     C                   MOVEL     MSG(14)       w4cmsg
123200130319     C                   SETON                                        904728
123300130319     C                   leavesr
123400130319   X2C                   ELSE
123500130319     C*
123600130319     C* IMPOSTO LA RAGIONE SOCIALE SOLO SE NON IMMESSA SE E' IL FASULLO
123700130319    3C     VIDPDR        IFNE      '0465000'
123800130319     C     VIDDPD        OREQ      *BLANKS
123900130319     C                   MOVEL     TLZRSC        VIDDPD
124000130319    3C                   ENDIF
124100130319    2C                   ENDIF
124200130319    1C                   ENDIF
124300130319     c
124400130319     c                   ENDSR
124500130319     C*****************************************************************
124600130319     C*   R O U T I N E      I N I Z I A L E
124700130319     C*****************************************************************
124800130319     C     *INZSR        BEGSR
124900020806      *
125000020806      ****  KLIST  ****
125100130503     C     KTAB          KLIST
125200130503     C                   KFLD                    CODUT
125300130503     C                   KFLD                    COD
125400130503     C                   KFLD                    KEY
125500951102     C* ACCESSO FNFGV02L
125600130319     C     KTNTLZ        KLIST
125700130319     C                   KFLD                    KTLTIP
125800130319     C                   KFLD                    KTLPDR
125900130314     C     KFGV02        KLIST
126000130314     C                   KFLD                    vidfgs
126100941121     C                   KFLD                    DATINW
126200130314     C     KFGV          KLIST
126300130314     C                   KFLD                    vidnfv
126400130315     C                   KFLD                    vidfgs
126500130419     c     kfgvsfl       klist
126600130419     c                   kfld                    FGVnfv
126700130419     c                   kfld                    vidfgs
126800020806     c* File FNBLP29L
126900020806     c     K02BLP79      klist
127000020806     c                   kfld                    FGVlnp
127100020806     c                   kfld                    FGVnfv
127200130426     c* File FNBLP29L
127300130426     c     Kblp2         klist
127400130426     c                   kfld                    vidfgs
127500130426     c                   kfld                    knfv5
127600130314     c     Kbrv          klist
127700130314     c                   kfld                    knpg
127800130426     c                   kfld                    Knfv6
127900130314     c                   kfld                    FGVlnp
128000130322     C     KFV2          KLIST
128100130322     C                   KFLD                    FGVLNP
128200130322     C                   KFLD                    FGVNFV
128300130322     C                   KFLD                    KEPA
128400130322     C                   KFLD                    KLAI
128500130322     C                   KFLD                    KTDH
128600130429     C     KFV4          KLIST
128700130429     C                   KFLD                    FGVLNP
128800130429     C                   KFLD                    FGVNFV
128900130429     C                   KFLD                    KEPA
129000130429     C                   KFLD                    KLAI
129100130429     C                   KFLD                    Ktrc
129200130322     C     KFV3          KLIST
129300130322     C                   KFLD                    FGVLNP
129400130322     C                   KFLD                    FGVNFV
129500130322     C                   KFLD                    KEPA
129600130322     C                   KFLD                    KTDH
129700130503     C     KCLN1         KLIST
129800130503     C                   KFLD                    KTFP
129900130503     C                   KFLD                    KTFA
130000130503     C                   KFLD                    KANN
130100130503     C                   KFLD                    KMES
130200020806      *
130300020806      ****  IMPOSTO CAMPI FISSI  ****
130400951031     C                   SETOFF                                       01
130500130314     C                   MOVEL     'FNLST3R '    VIDPGM
130600951108     C* DATA DEL GIORNO
130700020806     C                   TIME                    WTIME
130800951108     C                   MOVE      WTIME         WDATE
130900020806      ****
131000951031     C     *ENTRY        PLIST
131100951031     C                   PARM                    KPJBA
131200040611     c                   parm                    DLSpo
131300040630     c                   parm                    DLSpo2
131400040611      *
131500130314     C                   MOVEL     KPJBU         FNLST3DS
131600040611     c                   clear                   wDLSpo
131700040630     c                   clear                   wDLSpo2
131800040611if  1c                   if        SDSprm > 1
131900040611     c                   move      DLSpo         wDLSpo
132000040611e   1c                   endif
132100040630if  1c                   if        SDSprm > 2
132200040630     c                   move      DLSpo2        wDLSpo2
132300040630e   1c                   endif
132400000609     C* TESTATA PGM
132500951031     C                   MOVE      TES(1)        VIDTES
132600951031     C*
132700951031     C                   Z-ADD     1             CODUT
132800951031     C                   CALL      'X§PARUT'
132900020806     C                   PARM                    UT§DSE0F
133000951031     C                   MOVEL     RAGUT         VIDRSU
133100951031     C                   MOVEL     REC80         CNCR80
133200000609     C                   SETON                                        35
133300000609      *
133400130314      * terminal partenza in gestione
133500130314     c                   if        parfgs>*zeros
133600130314     c                   movel     parfgs        vidfgs
133700130314     c                   else
133800130314     c                   movel     simfel        vidfgs
133900130314     c                   endif
134000130314     c
134100130314     c                   clear                   viddfgs
134200130314     c     vidfgs        chain     AZORG01L
134300020806     c                   if        %found(AZORG01L)
134400130315     c                   movel     ORGdes        viddfgs
134500020806     c                   endif
134600130430     c*
134700130503     c* Carico limiti date per immissione
134800130503     c                   exsr      sr_MinMax
134900130430      /free
135000130430
135100130430        // Carico filiali di test abilitate
135200130430         clear TIBS02ds;
135300130430         T02Mod = 'C';
135400130430         T02sif = knsif;
135500130430         T02cod = 'VPO';
135600130430         T02ke1 ='DECOFI'   ;
135700130430         TNTBE_RicercaControllo  (kpjba : tibs02ds);
135800130430         if  T02err  = *blank;
135900130430           dvpodecofi = T02uni;
136000130430         endif;
136100130430
136200130430      /end-free
136300130314     c
136400940912     C                   ENDSR
136500130503      **************************************************************************
136600130503      *  Calcolo meno 1 gg e più 1 gg lavorativi da data foglio viaggio
136700130503      **************************************************************************
136800130503     c     Sr_MinMax     BegSr
136900130503
137000130503     c                   Clear                   Ktfa
137100130503     c                   Z-Add     VidFgs        Ktfp
137200130503     c                   Move      datasys       wdtaeur
137300130503      * meno 1 gg. lavorativo
137400130503     c                   Do        *Hival
137500130503     c                   Subdur    1:*d          wdtaeur
137600130503     c                   extrct    wdtaeur:*d    wgiorno
137700130503     c                   extrct    wdtaeur:*m    kmes
137800130503     c                   extrct    wdtaeur:*y    kann
137900130503     c     kcln1         Chain     Azcln01l
138000130503     c                   If        Not %Found(azcln01l)
138100130503     c                   Leave
138200130503     c                   EndIf
138300130503     c                   If        POM(wgiorno) = 'F'
138400130503     c                   Iter
138500130503     c                   EndIf
138600130503     c                   Leave
138700130503     c                   EndDo
138800130503     c                   Move      wdtaeur       wdtaiso
138900130503     c                   Move      wdtaiso       wdtameno1
139000130503
139100130503      * meno 2 gg. lavorativ1 (non imposto wdataeur perchè già impostato
139200130503     c*  con data foglio-1)
139300130503     c                   Do        *Hival
139400130503     c                   Subdur    1:*d          wdtaeur
139500130503     c                   extrct    wdtaeur:*d    wgiorno
139600130503     c                   extrct    wdtaeur:*m    kmes
139700130503     c                   extrct    wdtaeur:*y    kann
139800130503     c     kcln1         Chain     Azcln01l
139900130503     c                   If        Not %Found(azcln01l)
140000130503     c                   Leave
140100130503     c                   EndIf
140200130503     c                   If        POM(wgiorno) = 'F'
140300130503     c                   Iter
140400130503     c                   EndIf
140500130503     c                   Leave
140600130503     c                   EndDo
140700130503     c                   Move      wdtaeur       wdtaiso
140800130503     c                   Move      wdtaiso       wdtameno2
140900130503      * più 1 gg. lavorativo
141000130503     c                   Move      datasys       wdtaeur
141100130503     c                   Do        *Hival
141200130503     c                   Adddur    1:*d          wdtaeur
141300130503     c                   extrct    wdtaeur:*d    wgiorno
141400130503     c                   extrct    wdtaeur:*m    kmes
141500130503     c                   extrct    wdtaeur:*y    kann
141600130503     c     kcln1         Chain     Azcln01l
141700130503     c                   If        Not %Found(azcln01l)
141800130503     c                   Leave
141900130503     c                   EndIf
142000130503     c                   If        POM(wgiorno) = 'F'
142100130503     c                   Iter
142200130503     c                   EndIf
142300130503     c                   Leave
142400130503     c                   EndDo
142500130503     c                   Move      wdtaeur       wdtaiso
142600130503     c                   Move      wdtaiso       wdtapiu1
142700130503
142800130503     c                   EndSr
142900130611      **************************************************************************
143000130611      * Window di richiesta stampa etichette borderò
143100130611      **************************************************************************
143200130611     c     geswdw02      BegSr
143300130611     c                   eval      wideti='S'
143400130611     c                   do        *hival
143500130611     c                   exfmt     lst3w02
143600130611     c                   if        *inkf
143700130611     c                   leave
143800130611     c                   endif
143900130611     c                   enddo
144000130611     c                   if        wideti='S'
144100130611     c* richiesta stampanti se non ancora fatta
144200130611     c                   if        wrichtrul90=' '
144300130611     c                   exsr      richstampan
144400130611     c                   if        d90f3<>'1' and not *in91
144500130611     c                   eval      wrichtrul90='S'
144600130611     c                   endif
144700130611     c                   if        *in91
144800130611     c                   endif
144900130611     c                   endif
145000130611     c                   if        wrichtrul90='S'
145100130611     c                   clear                   fnlst9ds
145200130611     c                   eval      LST9DFV=%dec(ilst1dat:8:0)
145300130611     c                   eval      LST9FOG='S'
145400130611     c                   eval      LST9PGMSE=d90psb
145500130611     c                   movel     fnlst9ds      kpjbu
145600130611     c                   call      'FNLST9R1'
145700130611     c                   parm                    kpjba
145800130611     c                   endif
145900130611     c                   endif
146000130611     c                   endsr
146100130611     C**************************************************************************
146200130611      * RICHIEDO LE STAMPANTI per stampa etichette
146300130611     C**************************************************************************
146400130611     c     RICHSTAMPAN   BEGSR
146500130611     C                   CLEAR                   TRUL90DS
146600130611      *
146700130611     C                   MOVEL     'S'           d90RSe
146800130611     C                   MOVEL     'ETBOR'       d90mde
146900130611     C                   CALL      'TRUL90R'
147000130611     C                   PARM                    KPJBA
147100130611     C                   PARM                    trul90DS
147200130611      *
147300130611     C* F3 - FINE
147400130611     c                   if        d90f3<>'1'
147500130611      *
147600130611      * OVRPRTF
147700130611     C*
147800130611     C                   Z-ADD     130           LUNG             15 5
147900130611     C                   CLEAR                   COMMAN          130
148000130611     c                   eval      comman='OVRPRTF FILE(FNLV22P) OUTQ('
148100130611     c                             + d90pre + ') FORMTYPE('
148200130611     c                             + d90mde + ') USRDTA(' + '''' + 'Etich.Bord'
148300130611     c                             + '''' + ') share(*yes)'
148400130611     C                   CALL      'QCMDEXC'                            91
148500130611     C                   PARM                    COMMAN
148600130611     C                   PARM                    LUNG
148700130611      *
148800130611     c                   endif
148900130611      *
149000130611     c                   ENDSR
149100130503
149200951031     C*****************************************************************
149300951031** MESSAGGI DI ERRORE
149400940913Immettere data iniziale minore o uguale a quella finale                        1
149500940913Immettere data formalmente valida                                              2
149600940913Non trovati fogli di viaggio per la selezione richiesta                        3
149700951031Immettere un foglio di viaggio esistente                                       4
149800960313Verranno selezionati solo i primi xxx Fogli Viaggio: enter x continuare        5
149900130430Opzione "5" non ammessa: immettere l'opzione "1" per selezionare e modificare  6
150000130430Interrogazione non ammessa: premere F3 e riproporre la richiesta in gestione   7
150100130430Utente di filiale non abilitata all'uso di questa funzione.Enter per terminare 8
150200130503Data DAL usata in APERTURA errata: non compresa nei limiti previsti            9
150300130314Data DAL obbligatoria
150400130314Non è possibile visualizzare tutti i fogli richiesti: superano i 9990!!!
150500130502Opzione non ammessa                                                           12
150600130319Immettere un numero di TRAINO esistente                                       13
150700130319Immettere codice TRAZIONISTA esistente                                        14
150800130426Non sono presenti BOLLE BORDERIZZATE!!  Premere F14 per forzare               15
150900130426Non sono presenti SPUNTE!!  Premere F14 per forzare                           16
151000130503La data Foglio Viaggio e' un giorno festivo: enter per forzare                17
151100130612Utente di filiale non + abilitata all'uso.Utilizzare la nuova chiusura F.viagg18
151200951031**
151300130417 * GESTIONE FOGLI VIAGGIO PARTENZA *
151400130417 ** INTERROGAZIONE FOGLI VIAGGIO  **
151500130417 ** S C E L T A    FOGLI VIAGGIO  **
151600130426 * CHIUSURA FOGLI VIAGGIO PARTENZA *
