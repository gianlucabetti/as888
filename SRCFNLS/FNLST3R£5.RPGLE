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
064000130613     c                   if        vidpr_p='N'
064100130429     c                   seton                                        25
064200130429     c                   endif
064300130613     c                   if        vidpr_m='N'
064400130523     c                   seton                                        26
064500130523     c                   endif
064600130613     c                   if        vidpr_r='N'
064700130613     c                   seton                                        27
064800130613     c                   endif
064900130429     c
065000130429     C                   UPDATE    LST3S02
065100130429     c                   setoff                                       364025
065200130613     c                   setoff                                       2627
065300130523     c
065400130429     c                   ENDSR
065500951031     C*-----------------------------------------------------***********
065600130314     C* CARICAMENTO  FOGLIO VIAGGIO
065700951031     C*-----------------------------------------------------***********
065800951031     C     RIES04        BEGSR
065900951031     C*
066000940912     C                   Z-ADD     1             NRR4
066100900517     C                   SETON                                        72
066200130315     C                   WRITE     LST3C02
066300911205     C                   SETOFF                                       7213
066400130430     c*
066500130430     c* Se errore grave --> esco dalla routine
066600130430     c                   if        *in99
066700130430     c                   leavesr
066800130430     c                   endif
066900130430     c
067000130430     C                   SETON                                        73
067100911205     C*
067200951031     C* 13 ON  - VISUALIZZO SOLO I FV APERTI
067300951102    1C     TIPFOG        IFEQ      '2'
067400911205     C                   SETON                                        13
067500951031    1C                   ENDIF
067600951031     C*
067700951031     C* CARICO IL SUBFILE
067800130314    1c                   if        not *in13
067900130314     c     KFGV02        setll     FNFGV02L
068000130314     c     vidfgs        reade     FNFGV02L                               31
068100020806     c                   else
068200130314     c     vidfgs        setll     FNFGV06L
068300130314     c     vidfgs        reade     FNFGV06L                               31
068400130314    1c                   endif
068500951031     C*
068600951031    1C     *IN31         DOWEQ     *OFF
068700041130     c     nrr4          andle     9990
068800951031     C*
068900951031    2C     FGVDFV        IFLE      DATFIW
069000900529     C                   SETOFF                                       95
069100951031     C*
069200951031    3C   13FGVDFV        IFGE      DATINW
069300911205     C                   SETON                                        95
069400951031    3C                   ENDIF
069500951031     C*
069600960219     C                   SELECT
069700960219     C     TIPFOG        WHENEQ    '1'
069800020806    3C     FGVFCF        ANDNE     *BLANKS
069900900529     C                   SETON                                        95
070000951031     C*
070100020806    3C     TIPFOG        WHENEQ    *BLANKS
070200900529     C                   SETON                                        95
070300960219    3C                   ENDSL
070400040611      *
070500040611      * Verifico P.O. arrivo
070600040630    3c                   if             *in95   =  *on
070700040630     c                             AND (wDLSpo  <> *zeros
070800040630     c                             and  wDLSpo  <> FGVlna
070900040630     c                             and  wDLSpo2 =  *zeros
071000040630     c                              OR  wDLSpo2 <> *zeros
071100040630     c                             and  wDLSpo  <> FGVlna
071200040630     c                             and  wDLSpo2 <> FGVlna)
071300040611     c                   eval      *in95 = *off
071400040611    3c                   endif
071500130322     c
071600130322     C* VERIFICO CON IL RITARDO PARTENZA
071700130322    3C   95VIDRTP        IFNE      *BLANKS
071800130322     C* CHAIN SU FV2
071900130322     C                   MOVEL     'P'           KEPA
072000130322     C                   CLEAR                   KLAI
072100130322     C                   MOVEL     'R'           KTDH
072200130322     C     KFV2          CHAIN     FNFV201L                           32
072300130322    4C     *IN32         IFEQ      *OFF
072400130322     C     FV2ATB        ANDEQ     *BLANKS
072500130322     C     FV2RTC        ANDNE     *BLANKS
072600130322     C* SE HO CHIESTO I FV SENZA RITARDO ERRORE
072700130322    5C     VIDRTP        IFEQ      'N'
072800130322     C                   SETOFF                                       95
072900130322    5C                   ENDIF
073000130322   X4C                   ELSE
073100130322     C*
073200130322     C* SE HO CHIESTO I FV CON   RITARDO ERRORE
073300130322    5C     VIDRTP        IFEQ      'R'
073400130322     C                   SETOFF                                       95
073500130322    5C                   ENDIF
073600130322    4C                   ENDIF
073700130322    3C                   ENDIF
073800130322     C**
073900130322     C* VERIFICO CON IL RITARDO ARRIVO
074000130322    3C   95VIDRTA        IFNE      *BLANKS
074100130322     C* CHAIN SU FV2
074200130322     C                   CLEAR                   WRTA
074300130322     C                   MOVEL     'A'           KEPA
074400130322     C                   MOVEL     'R'           KTDH
074500130322     C     KFV3          CHAIN     FNFV203L                           32
074600130322    4C     *IN32         DOWEQ     *OFF
074700130322     C*
074800130322    5C     FV2ATB        IFEQ      *BLANKS
074900130322     C     FV2RTC        ANDNE     *BLANKS
075000130322     C                   SETON                                        32
075100130322     C* SE HO CHIESTO I FV SENZA RITARDO ERRORE
075200130322    6C     VIDRTA        IFEQ      'N'
075300130322     C                   SETOFF                                       95
075400130322   X6C                   ELSE
075500130322     C* SE HO CHIESTO I FV CON RITARDO SOLO A POSTO
075600130322     C                   MOVEL     'S'           WRTA
075700130322    6C                   ENDIF
075800130322    5C                   ENDIF
075900130322     C*
076000130322     C  N32KFV3          READE     FNFV203L                               32
076100130322    4C                   ENDDO
076200130322     C*
076300130322    4C     VIDRTA        IFEQ      'R'
076400130322     C     WRTA          ANDEQ     *BLANKS
076500130322     C                   SETOFF                                       95
076600130322    4C                   ENDIF
076700130322    3C                   ENDIF
076800960219     C**
076900960219     C**
077000951031     C* 95 ON  - TUTTO OK: RECORD DA VISUALIZZARE
077100951031    3C     *IN95         IFEQ      *ON
077200951031     C*
077300951031     C* CAMPO SELEZIONE
077400020806     C                   CLEAR                   VIDSCE
077500130314     C
077600130419     c                   exsr      RigaSFL
077700130315     c
077800130315     c* La prima volta accedo indicatore di posizionamento ccursore
077900130419     c                   if        nrr4=1
078000130419     c                   seton                                        4036
078100130419     c                   endif
078200951031     C*
078300130315     C                   WRITE     LST3S02
078400940912     C                   ADD       1             NRR4
078500130430     c                   setoff                                       403625
078600130523     c                   setoff                                       26
078700951031    3C                   ENDIF
078800951031     C*
078900020806     c                   if        not *in13
079000130314     c     vidfgs        reade     FGV02                                  31
079100020806     c                   else
079200130314     c     vidfgs        reade     FGV06                                  31
079300020806     c                   endif
079400951031     C*
079500951031   X2C                   ELSE
079600020806     C*
079700020806     c                   if        not *in13
079800020806     c                   eval      *in31 = *on
079900020806     c                   else
080000130314     c     vidfgs        reade     FGV06                                  31
080100020806     c                   endif
080200020806     C*
080300951031    2C                   ENDIF
080400951031     C*
080500951031    1C                   ENDDO
080600130314
080700041129     c                   eval      werrore='N'
080800130314     c* Se caricati troppi record --> segnalo l'errore
080900041130     c                   if        *in31 =*off and nrr4 >9990
081000041129     c                   eval      werrore='S'
081100130314     C                   MOVEL     MSG(11)       VIDMSG
081200130314     C                   SETON                                        28
081300041129     c                   endif
081400951031     C*
081500130426     c* salvo i campi di ricaricamento sfl
081600130314     c                   eval      savfiw=datfiW
081700130314     c                   eval      savinw=datinW
081800130322     c                   eval      savrtp=vidrtp
081900130322     c                   eval      savrta=vidrta
082000130426     c                   eval      savtfog=tipfog
082100130315    1c                   if        nrr4=1
082200130315     c                   setoff                                       73
082300130315     c                   endif
082400130315     c
082500951031     C                   ENDSR
082600130419     C*-----------------------------------------------------***********
082700130419     C* Carico dati nella riga del sfl
082800130419     C*-----------------------------------------------------***********
082900130419     c     RigaSFL       BEGSR
083000130613     c                   setoff                                       252627
083100130419     C* GIRO DATA FOGLIO VIAGGIO
083200130419     C                   Z-ADD     FGVDFV        G02INV
083300130419     C                   MOVEL     *ZERO         G02DAT
083400130419     C                   MOVEL     '3'           G02ERR
083500130419     C                   CALL      'XSRDA8'
083600130419     C                   PARM                    WLBDAT
083700130419     C                   Z-ADD     G02DAT        W0020
083800130419     C                   MOVEL     G02DAT        VIDDEL
083900130419     C                   MOVE      W0020         VIDDEL
084000130419     C* DECODIFICO LINEA DI ARRIVO
084100130419     C     FGVLNA        CHAIN     AZORG                              31
084200130419     C  N31              MOVEL     ORGDES        DE2LNA
084300130419     C   31              MOVEL     *BLANKS       DE2LNA
084400130419     C* NUMERO TRAINO
084500130613    1c                   if        fgvtrn>999999
084600130613     C                   z-add     999999        VIDTRN
084700130613     c                   else
084800130419     C                   MOVEL     FGVTRN        VIDTRN
084900130613    1c                   endif
085000130419     C*
085100130419     C                   MOVEL     *BLANKS       VIDABP
085200130419     C                   MOVEL     *BLANKS       VIDimp
085300130613     C                   clear                   VIDdtp
085400130613     C                   clear                   VIDhmp
085500130613     c* e c'e' data/ora reale --> prendo questa altrimenti cerco la teorica
085600130613     c*   solo se non annullato
085700130613    0c                   if        fgvatb=' '
085800130613    1c                   if        fgvdtp>0
085900130613     C                   Z-ADD     FGVDtp        G02INV
086000130613     C                   MOVEL     *ZERO         G02DAT
086100130613     C                   MOVEL     '3'           G02ERR
086200130613     C                   CALL      'XSRDA8'
086300130613     C                   PARM                    WLBDAT
086400130613     C                   MOVEL     G02DAT        viddtp
086500130613     c*
086600130613     c                   eval      vidhmp=%editw(fgvhmp:'0 :  ')
086700130613   x1c                   else
086800130613     c* cerco la teorica
086900130613     C                   MOVEL     'P'           KEPA
087000130613     C                   CLEAR                   KLAI
087100130613     C                   MOVEL     'T'           KTDH
087200130613     C     KFV2          CHAIN     FNFV201L                           32
087300130613    2c                   if        %found(fnfv201l) and fv2atb=' '
087400130613     C                   Z-ADD     Fv2dpa        G02INV
087500130613     C                   MOVEL     *ZERO         G02DAT
087600130613     C                   MOVEL     '3'           G02ERR
087700130613     C                   CALL      'XSRDA8'
087800130613     C                   PARM                    WLBDAT
087900130613     C                   MOVEL     G02DAT        viddtp
088000130613     c*
088100130613     c                   eval      vidhmp=%editw(fv2hpa:'0 :  ')
088200130613    2c                   endif
088300130613    1c                   endif
088400130613    0c                   endif
088500130419     c
088600130419     c                   clear                   vidabp
088700130419     C* ANNULLATO
088800130419    4C     FGVATB        IFEQ      '*'
088900130429     C                   MOVEL     'Ann   '      VIDABP
089000130419    4C                   ENDIF
089100130419     C*
089200130419     C* BORDERO'
089300130419     C     K02BLP79      setll     FNBLP79L                               31
089400130429     C   31              MOVE      'B     '      VIDABP
089500130419     c* Verifico se presenti spunte
089600130426     c                   eval      knfv6=fgvnfv
089700130419     C     Kbrv          setll     FNBrv01l                               31
089800130429     c   31              eval      %subst(vidabp:3:2)='Sp'
089900130419     C*
090000130419     C* PARTITO
090100130419    4C     FGVFCF        IFNE      *BLANKS
090200130419     C                   MOVE      'P'           VIDABP
090300130419    4C                   ENDIF
090400130419     c* Verifico che chiusura da IMP
090500130419     c                   if        fgvnfa>0
090600130613     c                   eval      vidimp='IMP'
090700130419     c                   endif
090800130429     c* Verifico la presenza dei piombi
090900130429     c* Escludo fogli defluenza e locali
091000130429     c                   eval      vidpr_p='  '
091100130613     c                   eval      vidpr_r='  '
091200130613     c                   eval      vidpr_m='  '
091300130618     c                   if        fgvatb=' '  and fgvfcf=' '
091400130618     c
091500130429     c                   if        fgvttr= ' ' or fgvttr='S'
091600130429     c                                         or fgvttr='H'
091700130429     C                   MOVEL     'P'           KEPA
091800130429     C                   CLEAR                   KLAI
091900130429     C                   MOVEL     'P'           KTrc
092000130429     C     KFV4          CHAIN     FNFV401L
092100130429    4C                   IF        %found(fnfv401l) and fv4atb=' '
092200130429     c                             and fv4not<>*blanks
092300130429     c                   else
092400130613     c                   eval      vidpr_p='N'
092500130429     c                   seton                                        25
092600130429     c                   endif
092700130523     c
092800130613     c* Targa motrice
092900130613     c                   if        fgvtrm=*blanks
093000130613     c                   eval      vidpr_m='N'
093100130523     c                   seton                                        26
093200130523     c                   endif
093300130613     c* Targa Rimorchio
093400130613     c                   if        fgvtrr=*blanks
093500130613     c                   eval      vidpr_r='N'
093600130613     c                   seton                                        27
093700130613     c                   endif
093800130523     c
093900130429     c                   endif
094000130613     c                   endif
094100130419     C*
094200130419     C* TARGHE MOTRICE E RIMORCHIO
094300130613     C*****              MOVEL     FGVTRM        VIDTRM
094400130523     C*****              MOVEL     FGVTrr        VIDTrr
094500130419     c                   ENDSR
094600951031     C*-----------------------------------------------------***********
094700130314     C* CONTROLLI dati di parzializzazione
094800951031     C*-----------------------------------------------------***********
094900130314     C     CTRD02        BEGSR
095000940913     C                   SETOFF                                       907677
095100000612     C                   SETOFF                                       1406
095200000609     C*
095300000609     C* CONTROLLO DATA INIZIALE
095400951031    1C     VIDDIN        IFNE      0
095500900517     C                   MOVE      VIDDIN        G02DAT
095600900517     C                   MOVEL     *BLANK        G02ERR
095700940913     C                   CALL      'XSRDA8'
095800900517     C                   PARM                    WLBDAT
095900951031    2C     G02ERR        IFEQ      '1'
096000940913     C                   MOVEL     MSG(2)        VIDMSG
096100940913     C                   SETON                                        762890
096200130503     C                   GOTO      ENDCT2
096300951031    2C                   ENDIF
096400130503     c
096500940913     C                   Z-ADD     G02DAT        VIDDIN
096600130503     c                   if        g02inv<>datinw
096700130503     c                   clear                   wforzb            1
096800130503     c                   endif
096900130503     C                   Z-ADD     G02INV        DATINW            8 0
097000130503     C                   Z-ADD     G02DAT        WDATA
097100130503     C                   MOVEL     MM            KMES
097200130503     c
097300130503     c* In caso di apertura fogli viaggio faccio il controllo
097400130503    2c                   if        *inkj   or *inkm  or *inkp
097500130503     c
097600130503      * non può essere minore o maggiore di 1 gg. lavorativo
097700130503    3c                   If        datinw < wdtameno1  or
097800130503     c                             datinw > wdtapiu1
097900130503     c                   Eval      VidMsg = Msg(9)
098000130503     c                   Eval      *in28 = *On
098100130503     c                   Eval      *in76 = *On
098200130503     c                   Eval      *in90 = *On
098300130503     C                   GOTO      ENDCT2
098400130503    3c                   EndIf
098500130503     c
098600130503     C* Se DATA DFV CADE IN GIORNO FESTIVO PER
098700130503     C*  LA TRAINI, AVVISO. ERRORE FORZABILE
098800130503     C                   CLEAR                   KTFA
098900130503     C                   Z-ADD     68            KTFP
099000130503     C     KCLN1         CHAIN     AZCLN01L                           83
099100130503    3C  N83POM(GG)       IFNE      *BLANKS
099200130503     C                   MOVEL     '1Y'          COD
099300130503     C                   MOVEL(P)  POM(GG)       KEY
099400130503     C     KTAB          CHAIN     TABEL                              83
099500130503     C                   MOVEL     TBLUNI        DS1Y
099600130503     C*
099700130503    4C  N83§1YFET        IFNE      *BLANKS
099800130503     C     WFORZB        ANDEQ     *BLANKS
099900130503     C                   MOVEL     MSG(17)       VIDMSG
100000130503     C                   SETON                                        287690
100100130503     C                   MOVEL     '1'           WFORZB            1
100200130503    4C                   ENDIF
100300130503    3c                   endif
100400130503    2c                   endif
100500951031     C*
100600951031   X1C                   ELSE
100700940913     C                   Z-ADD     0             DATINW
100800041130     c* solo se non sono stati richiesti solo gli aperti
100900041130     c     tipfog        ifne      '2'
101000041129     C                   MOVEL     MSG(10)       VIDMSG
101100041129     C                   SETON                                        762890
101200041130     c                   endif
101300951031    1C                   ENDIF
101400130314     C   90              GOTO      ENDCT2
101500951031     C*
101600951031     C* CONTROLLO DATA FINALE
101700951031    1C     VIDDFI        IFEQ      0
101800951031    2C     VIDDIN        IFEQ      *ZERO
101900940913     C                   Z-ADD     WDATE         VIDDFI
102000951031   X2C                   ELSE
102100940921     C                   Z-ADD     VIDDIN        VIDDFI
102200951031    2C                   ENDIF
102300951031    1C                   ENDIF
102400951031     C*
102500900517     C                   MOVE      VIDDFI        G02DAT
102600900517     C                   MOVEL     *BLANK        G02ERR
102700940913     C                   CALL      'XSRDA8'
102800900517     C                   PARM                    WLBDAT
102900951031    1C     G02ERR        IFEQ      '1'
103000940913     C                   MOVEL     MSG(2)        VIDMSG
103100940913     C                   SETON                                        772890
103200951031    1C                   ENDIF
103300130314     C   90              GOTO      ENDCT2
103400940913     C                   Z-ADD     G02DAT        VIDDFI
103500940913     C                   Z-ADD     G02INV        DATFIW            8 0
103600951031     C*
103700951031     C* DATA INIZIALE MAGGIORE DATA FINALE
103800951031    1C     DATINW        IFGT      DATFIW
103900940913     C                   MOVEL     MSG(1)        VIDMSG
104000940913     C                   SETON                                        762890
104100130314     C                   GOTO      ENDCT2
104200951031    1C                   ENDIF
104300960126     C**
104400960126     C* CONTROLLO NUMERO FOGLIO VIAGGIO
104500960710    1C     VIDNFV        IFGT      0
104600130314     C     kfgv          CHAIN     FNFGV01L                           31
104700960126    2C     *IN31         IFEQ      *ON
104800960126     C                   MOVEL     MSG(4)        VIDMSG
104900960201     C                   SETON                                        284890
105000130314     C                   GOTO      ENDCT2
105100960126    2C                   ENDIF
105200130426     c* Per chiusura fogli viaggio controllo se ha bolle altrimenti
105300130426     c*  si forza con F14
105400130426     c                   if        not *inkn and parflg='C'
105500130426     c                   eval      Knfv5=vidnfv
105600130426     c                   exsr      contrBolle
105700130426     c                   if        *in90
105800130426     c                   seton                                        48
105900130426     c                   goto      endct2
106000130426     c                   endif
106100130426     c
106200130426     c                   endif
106300130426     c
106400130315    2C                   ENDIF
106500951031     C*
106600130314     C     ENDCT2        ENDSR
106700130426     C*****************************************************************
106800130613     c*   controllo se il foglio ha delle bolle per chiusura FGV
106900130426     C*****************************************************************
107000130426     C     contrBolle    BEGSR
107100130613     c                   z-add     knfv5         knfv6
107200130607     c                   if        parfl2<>'N'
107300130426     C     Kblp2         SETLL     FNBLP000                               83
107400130426     c                   if        *in83
107500130426     C     KBRV          SETLL     FNBRV01L                               84
107600130426     c                   endif
107700130607     c
107800130607     c                   else
107900130607     c* Per la nuova borderizzazine controllo solo se ci sono spunte
108000130607     c                   seton                                        83
108100130607     C     KBRV          SETLL     FNBRV01L                               84
108200130607     c                   endif
108300130426     C*
108400130426     C* BOLLE O SPUNTE NON TROVATE --> FORZATURA
108500130426     C     *IN83         IFEQ      *OFF
108600130426     C     *IN84         OREQ      *OFF
108700130426     C                   SETON                                        9028
108800130426     C  N84              MOVEL     MSG(16)       VIDMSG
108900130426     C  N83              MOVEL     MSG(15)       VIDMSG
109000130426     C                   ENDIF
109100130426     c
109200130426     c                   ENDSR
109300130314     C*****************************************************************
109400130314     C*   Visualizza singolo foglio
109500130314     C*****************************************************************
109600130314     C     VisualFGV     BEGSR
109700130426     c                   setoff                                       90
109800130426     c* Se sono in chiusura fogli viaggio emetto finestra se si tratta
109900130426     c*  di foglio NON bis
110000130502     c                   if        vidsce<>'5'  and
110100130502     c                             parflg='C' and fgvSNP=' '
110200130426     c                             and fgvttr <>'D' and fgvttr<>'H'
110300130426     c                             and fgvttr<>'I'
110400130426    3c                   do        *hival
110500130426     c                   exfmt     lst3w01
110600130426     c* Premuto F12 riemetto videata iniziale
110700130426    4c                   if        *inkl
110800130426     C                   SETON                                        90
110900130426     c                   leave
111000130426    4c                   endif
111100130426     c
111200130426     c   kf              leave
111300130426    3c                   enddo
111400130426
111500130426     c                   endif
111600130426     c
111700130426     c                   if        not *in90
111800130318     C                   CLEAR                   fnlst4ds
111900130318     C                   MOVEL     FGVNFV        ilst4NFV
112000130318     C                   move      VIDfgs        ilst4FGS
112100130314     c* Per gestione flag =' '
112200130315     c                   if        vidsce='5'
112300130318     C                   MOVEL     'N'           Ilst4GES
112400130314     c                   endif
112500130426     c                   if        not *in05 and vidsce='2'
112600130318     C                   MOVEL     'M'           ilst4GES
112700130315     c                   endif
112800130426     c                   if        *in05 and vidsce='2'
112900130426     C                   MOVEL     'C'           ilst4GES
113000130603     c                   movel     parfl2        ilst4newc
113100130426     c                   endif
113200130419     c                   if        vidsce='P'
113300130419     C                   MOVEL     'P'           ilst4GES
113400130419     c                   endif
113500130318     C                   MOVEL     fnlst4ds      KPJBU
113600130318     C                   CALL      'FNLST4R'
113700130314     C                   PARM                    KPJBA
113800130318     C                   MOVEL     KPJBU         fnlst4ds
113900130314     C* MESSAGGIO DI ERRORE
114000130318    1C     olst4MSG      IFNE      *BLANKS
114100130318     C                   MOVEL     olst4MSG      VIDMSG
114200130314     C                   SETON                                        2890
114300130314    1C                   ENDIF
114400130426    1C                   ENDIF
114500130426     c
114600130314     C                   ENDSR
114700130314     C*****************************************************************
114800130314     C*   Ricerca Proposte per apertura FGV
114900130314     C*****************************************************************
115000130314     C     RicercaTRN    BEGSR
115100130315     c                   clear                   fnlst1ds
115200130319     c                   eval      ilst1GES='A'
115300130315     c                   movel     datinw        ilst1dat
115400130315     c                   eval      ilst1TUT='G'
115500130315     c                   movel     vidfgs        ilst1fgs
115600130417     c                   if        *inkp
115700130417     c                   eval      ilst1aut='S'
115800130417     c                   endif
115900130315     c
116000130315     c                   movel(p)  fnlst1ds      kpjbu
116100130315     c                   call      'FNLST1R'
116200130315     c                   parm                    kpjba
116300130315     c*
116400130314     C                   ENDSR
116500130314     C*****************************************************************
116600130314     C*   Immissione singolo foglio da traino
116700130314     C*****************************************************************
116800130314     C     Immissione    BEGSR
116900130319     C                   CLEAR                   coptrn
117000130319     C                   CLEAR                   vidpdr
117100130319     C                   CLEAR                   vidpdr
117200130319     c     emforw4       tag
117300130319     c                   exfmt     lst3w04
117400130319
117500130319     c                   setoff                                       289054
117600130319     c                   setoff                                       47
117700130319     c
117800130319     c                   if        *inkl
117900130319     c                   leavesr
118000130319     c                   endif
118100130319     c
118200130319     c                   exsr      ctrImm
118300130319
118400130319     c                   if        not *in90
118500130319     c
118600130319     c                   clear                   fnlst4ds
118700130319     c                   eval      ilst4ges='A'
118800130319     c                   eval      ilst4fgs =%editc(vidfgs:'X')
118900130319     c                   if        coptrn>*zeros
119000130319     c                   eval      ilst4TRN =%int(copTRN)
119100130319     c                   endif
119200130319     c                   eval      ilst4dfv=datinw
119300130319     c                   eval      ilst4pdr=vidpdr
119400130319     c                   eval      ilst4old='S'
119500130319     c                   eval      kpjbu=fnlst4ds
119600130319     c                   call      'FNLST4R'
119700130319     c                   parm                    kpjba
119800130319     c                   eval      fnlst4ds=kpjbu
119900130319     c
120000130319     c                   if        olst4msg<>*blanks
120100130319     c                   movel     olst4msg      w4cmsg
120200130319     c                   seton                                        2890
120300130319     c                   goto      emforw4
120400130319     c                   endif
120500130319     c
120600130319     c                   else
120700130319     c                   goto      emforw4
120800130319     c                   endif
120900130319     c
121000130314     C                   ENDSR
121100951031     C*****************************************************************
121200130319     C*   controlla immissione
121300951031     C*****************************************************************
121400130319     C     CtrImm        BEGSR
121500130319     c
121600130319     C     '?'           SCAN      COPTRN                                 90
121700130319    1C     *IN90         IFEQ      *ON
121800130319     C                   CLEAR                   TNTL00DS
121900130319     C                   Z-ADD     datinw        D00DDE
122000130319     C*
122100130319     C                   MOVE      VIDFGS        D00FIL
122200130319     C                   MOVEL     'C'           D00SCA
122300130319     C                   MOVEL     TNTL00DS      KPJBU
122400130319     C                   CALL      'TNTL17R'
122500130319     C                   PARM                    KPJBA
122600130319     C                   MOVEL     KPJBU         TNTL00DS
122700130319     C*
122800130319     C                   MOVEL     D00TRN        COPTRN
122900130319     C                   SETON                                        54
123000130319     C                   leavesr
123100130319    1C                   ENDIF
123200130319     C*
123300130319    1c                   if        coptrn<>*blanks and coptrn<>*zeros
123400130319     C                   TESTN                   COPTRN               83
123500130319     C                   MOVE      COPTRN        W001              1
123600130319    2C     *IN83         IFEQ      *OFF
123700130319     C     COPTRN        ORLT      *ZEROS
123800130319     C     W001          ORLT      '0'
123900130319     C                   MOVEL     MSG(13)       w4cmsg
124000130319     C                   SETON                                        549028
124100130319     C                   leavesr
124200130319    2C                   ENDIF
124300130319    1C                   ENDIF
124400130319     c
124500130319     C* RICERCA CODICE TRAZIONISTA
124600130319     C     '?'           SCAN      VIDPDR                                 83
124700130319    1C     *IN83         IFEQ      *ON
124800130319     C                   MOVEL     'T '          PA3TIP
124900130319     C                   MOVEL(P)  VIDDPD        PA3RSC
125000130319     C                   CLEAR                   PA3CSF
125100130319     C                   CLEAR                   PA3PDR
125200130319     C                   CLEAR                   PA3FLG
125300130319     C                   MOVEL     PARAM3        KPJBU
125400130319     C                   CALL      'TRUL16R'
125500130319     C                   PARM                    KPJBA
125600130319     C                   MOVEL     KPJBU         PARAM3
125700130319     C                   MOVEL     PA3PDR        VIDPDR
125800130319     C                   MOVEL     PA3RSC        VIDDPD
125900130319     C                   SETON                                        9047
126000130319     C                   LEAVESR
126100130319     C*
126200130319    1C                   ENDIF
126300130319     C*
126400130319     C* CONTROLLO VALIDITA' CODICE TRAZIONISTA
126500130319    1C     VIDPDR        IFNE      *BLANKS
126600130319     C     VIDPDR        ANDNE     *ZEROS
126700130319     C*
126800130319     C                   TESTN                   VIDPDR               31
126900130319     C  N31              MOVEL     MSG(14)       w4cmsg
127000130319     C  N31              SETON                                        902847
127100130319     C  N31              leavesr
127200130319     C*
127300130319     C                   MOVE      VIDPDR        KPDR
127400130319     C                   Z-ADD     KPDR          KTLPDR
127500130319     C     KTNTLZ        CHAIN     TNTLZ01L                           31
127600130319     C*
127700130319    2C     *IN31         IFEQ      *ON
127800130319     C     TLZATB        ORNE      ' '
127900130319     C                   MOVEL     MSG(14)       w4cmsg
128000130319     C                   SETON                                        904728
128100130319     C                   leavesr
128200130319   X2C                   ELSE
128300130319     C*
128400130319     C* IMPOSTO LA RAGIONE SOCIALE SOLO SE NON IMMESSA SE E' IL FASULLO
128500130319    3C     VIDPDR        IFNE      '0465000'
128600130319     C     VIDDPD        OREQ      *BLANKS
128700130319     C                   MOVEL     TLZRSC        VIDDPD
128800130319    3C                   ENDIF
128900130319    2C                   ENDIF
129000130319    1C                   ENDIF
129100130319     c
129200130319     c                   ENDSR
129300130319     C*****************************************************************
129400130319     C*   R O U T I N E      I N I Z I A L E
129500130319     C*****************************************************************
129600130319     C     *INZSR        BEGSR
129700020806      *
129800020806      ****  KLIST  ****
129900130503     C     KTAB          KLIST
130000130503     C                   KFLD                    CODUT
130100130503     C                   KFLD                    COD
130200130503     C                   KFLD                    KEY
130300951102     C* ACCESSO FNFGV02L
130400130319     C     KTNTLZ        KLIST
130500130319     C                   KFLD                    KTLTIP
130600130319     C                   KFLD                    KTLPDR
130700130314     C     KFGV02        KLIST
130800130314     C                   KFLD                    vidfgs
130900941121     C                   KFLD                    DATINW
131000130314     C     KFGV          KLIST
131100130314     C                   KFLD                    vidnfv
131200130315     C                   KFLD                    vidfgs
131300130419     c     kfgvsfl       klist
131400130419     c                   kfld                    FGVnfv
131500130419     c                   kfld                    vidfgs
131600020806     c* File FNBLP29L
131700020806     c     K02BLP79      klist
131800020806     c                   kfld                    FGVlnp
131900020806     c                   kfld                    FGVnfv
132000130426     c* File FNBLP29L
132100130426     c     Kblp2         klist
132200130426     c                   kfld                    vidfgs
132300130426     c                   kfld                    knfv5
132400130314     c     Kbrv          klist
132500130314     c                   kfld                    knpg
132600130426     c                   kfld                    Knfv6
132700130314     c                   kfld                    FGVlnp
132800130322     C     KFV2          KLIST
132900130322     C                   KFLD                    FGVLNP
133000130322     C                   KFLD                    FGVNFV
133100130322     C                   KFLD                    KEPA
133200130322     C                   KFLD                    KLAI
133300130322     C                   KFLD                    KTDH
133400130429     C     KFV4          KLIST
133500130429     C                   KFLD                    FGVLNP
133600130429     C                   KFLD                    FGVNFV
133700130429     C                   KFLD                    KEPA
133800130429     C                   KFLD                    KLAI
133900130429     C                   KFLD                    Ktrc
134000130322     C     KFV3          KLIST
134100130322     C                   KFLD                    FGVLNP
134200130322     C                   KFLD                    FGVNFV
134300130322     C                   KFLD                    KEPA
134400130322     C                   KFLD                    KTDH
134500130503     C     KCLN1         KLIST
134600130503     C                   KFLD                    KTFP
134700130503     C                   KFLD                    KTFA
134800130503     C                   KFLD                    KANN
134900130503     C                   KFLD                    KMES
135000020806      *
135100020806      ****  IMPOSTO CAMPI FISSI  ****
135200951031     C                   SETOFF                                       01
135300130314     C                   MOVEL     'FNLST3R '    VIDPGM
135400951108     C* DATA DEL GIORNO
135500020806     C                   TIME                    WTIME
135600951108     C                   MOVE      WTIME         WDATE
135700020806      ****
135800951031     C     *ENTRY        PLIST
135900951031     C                   PARM                    KPJBA
136000040611     c                   parm                    DLSpo
136100040630     c                   parm                    DLSpo2
136200040611      *
136300130314     C                   MOVEL     KPJBU         FNLST3DS
136400040611     c                   clear                   wDLSpo
136500040630     c                   clear                   wDLSpo2
136600040611if  1c                   if        SDSprm > 1
136700040611     c                   move      DLSpo         wDLSpo
136800040611e   1c                   endif
136900040630if  1c                   if        SDSprm > 2
137000040630     c                   move      DLSpo2        wDLSpo2
137100040630e   1c                   endif
137200000609     C* TESTATA PGM
137300951031     C                   MOVE      TES(1)        VIDTES
137400951031     C*
137500951031     C                   Z-ADD     1             CODUT
137600951031     C                   CALL      'X§PARUT'
137700020806     C                   PARM                    UT§DSE0F
137800951031     C                   MOVEL     RAGUT         VIDRSU
137900951031     C                   MOVEL     REC80         CNCR80
138000000609     C                   SETON                                        35
138100000609      *
138200130314      * terminal partenza in gestione
138300130314     c                   if        parfgs>*zeros
138400130314     c                   movel     parfgs        vidfgs
138500130314     c                   else
138600130314     c                   movel     simfel        vidfgs
138700130314     c                   endif
138800130314     c
138900130314     c                   clear                   viddfgs
139000130314     c     vidfgs        chain     AZORG01L
139100020806     c                   if        %found(AZORG01L)
139200130315     c                   movel     ORGdes        viddfgs
139300020806     c                   endif
139400130430     c*
139500130503     c* Carico limiti date per immissione
139600130503     c                   exsr      sr_MinMax
139700130430      /free
139800130430
139900130430        // Carico filiali di test abilitate
140000130430         clear TIBS02ds;
140100130430         T02Mod = 'C';
140200130430         T02sif = knsif;
140300130430         T02cod = 'VPO';
140400130613         T02ke1 ='DECOFI1'   ;
140500130430         TNTBE_RicercaControllo  (kpjba : tibs02ds);
140600130430         if  T02err  = *blank;
140700130430           dvpodecofi = T02uni;
140800130430         endif;
140900130430
141000130430      /end-free
141100130314     c
141200940912     C                   ENDSR
141300130503      **************************************************************************
141400130503      *  Calcolo meno 1 gg e più 1 gg lavorativi da data foglio viaggio
141500130503      **************************************************************************
141600130503     c     Sr_MinMax     BegSr
141700130503
141800130503     c                   Clear                   Ktfa
141900130503     c                   Z-Add     VidFgs        Ktfp
142000130503     c                   Move      datasys       wdtaeur
142100130503      * meno 1 gg. lavorativo
142200130503     c                   Do        *Hival
142300130503     c                   Subdur    1:*d          wdtaeur
142400130503     c                   extrct    wdtaeur:*d    wgiorno
142500130503     c                   extrct    wdtaeur:*m    kmes
142600130503     c                   extrct    wdtaeur:*y    kann
142700130503     c     kcln1         Chain     Azcln01l
142800130503     c                   If        Not %Found(azcln01l)
142900130503     c                   Leave
143000130503     c                   EndIf
143100130503     c                   If        POM(wgiorno) = 'F'
143200130503     c                   Iter
143300130503     c                   EndIf
143400130503     c                   Leave
143500130503     c                   EndDo
143600130503     c                   Move      wdtaeur       wdtaiso
143700130503     c                   Move      wdtaiso       wdtameno1
143800130503
143900130503      * meno 2 gg. lavorativ1 (non imposto wdataeur perchè già impostato
144000130503     c*  con data foglio-1)
144100130503     c                   Do        *Hival
144200130503     c                   Subdur    1:*d          wdtaeur
144300130503     c                   extrct    wdtaeur:*d    wgiorno
144400130503     c                   extrct    wdtaeur:*m    kmes
144500130503     c                   extrct    wdtaeur:*y    kann
144600130503     c     kcln1         Chain     Azcln01l
144700130503     c                   If        Not %Found(azcln01l)
144800130503     c                   Leave
144900130503     c                   EndIf
145000130503     c                   If        POM(wgiorno) = 'F'
145100130503     c                   Iter
145200130503     c                   EndIf
145300130503     c                   Leave
145400130503     c                   EndDo
145500130503     c                   Move      wdtaeur       wdtaiso
145600130503     c                   Move      wdtaiso       wdtameno2
145700130503      * più 1 gg. lavorativo
145800130503     c                   Move      datasys       wdtaeur
145900130503     c                   Do        *Hival
146000130503     c                   Adddur    1:*d          wdtaeur
146100130503     c                   extrct    wdtaeur:*d    wgiorno
146200130503     c                   extrct    wdtaeur:*m    kmes
146300130503     c                   extrct    wdtaeur:*y    kann
146400130503     c     kcln1         Chain     Azcln01l
146500130503     c                   If        Not %Found(azcln01l)
146600130503     c                   Leave
146700130503     c                   EndIf
146800130503     c                   If        POM(wgiorno) = 'F'
146900130503     c                   Iter
147000130503     c                   EndIf
147100130503     c                   Leave
147200130503     c                   EndDo
147300130503     c                   Move      wdtaeur       wdtaiso
147400130503     c                   Move      wdtaiso       wdtapiu1
147500130503
147600130503     c                   EndSr
147700130611      **************************************************************************
147800130611      * Window di richiesta stampa etichette borderò
147900130611      **************************************************************************
148000130611     c     geswdw02      BegSr
148100130611     c                   eval      wideti='S'
148200130611     c                   do        *hival
148300130611     c                   exfmt     lst3w02
148400130611     c                   if        *inkf
148500130611     c                   leave
148600130611     c                   endif
148700130611     c                   enddo
148800130611     c                   if        wideti='S'
148900130611     c* richiesta stampanti se non ancora fatta
149000130611     c                   if        wrichtrul90=' '
149100130611     c                   exsr      richstampan
149200130611     c                   if        d90f3<>'1' and not *in91
149300130611     c                   eval      wrichtrul90='S'
149400130611     c                   endif
149500130611     c                   if        *in91
149600130611     c                   endif
149700130611     c                   endif
149800130611     c                   if        wrichtrul90='S'
149900130611     c                   clear                   fnlst9ds
150000130611     c                   eval      LST9DFV=%dec(ilst1dat:8:0)
150100130611     c                   eval      LST9FOG='S'
150200130611     c                   eval      LST9PGMSE=d90psb
150300130611     c                   movel     fnlst9ds      kpjbu
150400130611     c                   call      'FNLST9R1'
150500130611     c                   parm                    kpjba
150600130611     c                   endif
150700130611     c                   endif
150800130611     c                   endsr
150900130611     C**************************************************************************
151000130611      * RICHIEDO LE STAMPANTI per stampa etichette
151100130611     C**************************************************************************
151200130611     c     RICHSTAMPAN   BEGSR
151300130611     C                   CLEAR                   TRUL90DS
151400130611      *
151500130611     C                   MOVEL     'S'           d90RSe
151600130611     C                   MOVEL     'ETBOR'       d90mde
151700130611     C                   CALL      'TRUL90R'
151800130611     C                   PARM                    KPJBA
151900130611     C                   PARM                    trul90DS
152000130611      *
152100130611     C* F3 - FINE
152200130611     c                   if        d90f3<>'1'
152300130611      *
152400130611      * OVRPRTF
152500130611     C*
152600130611     C                   Z-ADD     130           LUNG             15 5
152700130611     C                   CLEAR                   COMMAN          130
152800130611     c                   eval      comman='OVRPRTF FILE(FNLV22P) OUTQ('
152900130611     c                             + d90pre + ') FORMTYPE('
153000130611     c                             + d90mde + ') USRDTA(' + '''' + 'Etich.Bord'
153100130611     c                             + '''' + ') share(*yes)'
153200130611     C                   CALL      'QCMDEXC'                            91
153300130611     C                   PARM                    COMMAN
153400130611     C                   PARM                    LUNG
153500130611      *
153600130611     c                   endif
153700130611      *
153800130611     c                   ENDSR
153900130503
154000951031     C*****************************************************************
154100951031** MESSAGGI DI ERRORE
154200940913Immettere data iniziale minore o uguale a quella finale                        1
154300940913Immettere data formalmente valida                                              2
154400940913Non trovati fogli di viaggio per la selezione richiesta                        3
154500951031Immettere un foglio di viaggio esistente                                       4
154600960313Verranno selezionati solo i primi xxx Fogli Viaggio: enter x continuare        5
154700130430Opzione "5" non ammessa: immettere l'opzione "1" per selezionare e modificare  6
154800130430Interrogazione non ammessa: premere F3 e riproporre la richiesta in gestione   7
154900130430Utente di filiale non abilitata all'uso di questa funzione.Enter per terminare 8
155000130503Data DAL usata in APERTURA errata: non compresa nei limiti previsti            9
155100130314Data DAL obbligatoria
155200130314Non è possibile visualizzare tutti i fogli richiesti: superano i 9990!!!
155300130502Opzione non ammessa                                                           12
155400130319Immettere un numero di TRAINO esistente                                       13
155500130319Immettere codice TRAZIONISTA esistente                                        14
155600130426Non sono presenti BOLLE BORDERIZZATE!!  Premere F14 per forzare               15
155700130426Non sono presenti SPUNTE!!  Premere F14 per forzare                           16
155800130503La data Foglio Viaggio e' un giorno festivo: enter per forzare                17
155900130612Utente di filiale non + abilitata all'uso.Utilizzare la nuova chiusura F.viagg18
156000951031**
156100130417 * GESTIONE FOGLI VIAGGIO PARTENZA *
156200130417 ** INTERROGAZIONE FOGLI VIAGGIO  **
156300130417 ** S C E L T A    FOGLI VIAGGIO  **
156400130426 * CHIUSURA FOGLI VIAGGIO PARTENZA *
