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
003900130430     d TIBS02ds      e ds                  inz
004000130503     d ds1Y          e ds
004100130611     d fnlst9ds      e ds
004200130611     d trul90ds      e ds
004300130619     d dppa          e ds                  inz
004400130319     D*-----------------------------------------------------
004500130319      * Parametri per la ricerca trazionisti     - TRUL16R -
004600130319     D PARAM3          DS
004700130319     D* Tipologia T_=TRAZIONISTA
004800130319     D  PA3TIP                 1      2
004900130319     D* Ragione sociale trazionista
005000130319     D  PA3RSC                 3     37
005100130319     D* Sistema inform. trazionista
005200130319     D  PA3CSF                38     40
005300130319     D* Codice trazionista
005400130319     D  PA3PDR                41     47  0
005500130319     D* PA3FLG = "3" --> restituisce al pgm chiamante il messaggio che
005600130319     D*   non esistono records per la chiave alfabetica richiesta
005700130319     D  PA3FLG                48     48
005800020806      *
005900020806      * DEFINIZIONE STRUTTURE DATI INTERNE
006000020806      *
006100130319     D*-----------------------------------------------------
006200020806      * Parametri ricevuti dai pgm chiamanti
006300130314     D fnlst3ds        DS
006400130314     D* PARFLG  = "R" ---> PGM RICHIAMATO per scelta fogli viaggio
006500130314     D* PARFLG  = "I" ---> interrogazione fogli  viaggio
006600130314     D* PARFLG  = "G" ---> Gestione       fogli  viaggio
006700130426     D* PARFLG  = "C" ---> Chiusura       fogli  viaggio
006800130506     D* PARFLG  = "P" ---> interrogazione fogli  viaggio con scelta
006900130314     d*
007000951031     D  PARFLG                 1      1
007100130314     d
007200951123     D* PARFL2  = "F" ---> PASSO AL PGM CHIAMANTE I FV  SELEZIONATI
007300951123     D*                    NELLA SKIERA FVA
007400951123     D*                    SE NE VOGLIO SELEZIONARE SOLO 1 :
007500951123     D*                    PARFL2=" " E VIENE PASSATO IN PARNFV
007600951102     D  PARFL2                 2      2
007700020806     D* Numero Foglio Viaggio: se è pieno significa che è richiesta
007800020806     D*   la visualizzazione di quello specifico foglio
007900951031     D  PARNFV                 3      5P 0
008000020806     D* Data iniziale e finale in GG/MM/AAAA
008100951031     D  PARDIN                 6     10P 0
008200951031     D  PARDFI                11     15P 0
008300020806     D* PARTFV è il tipo FV da visualizzare:  " " = FV TUTTI
008400951031     D*                                       "1" = FV PARTITI (CHIUSI)
008500951031     D*                                       "2" = FV NON PARTITI (APERTI)
008600951031     D  PARTFV                16     16
008700020806     D* FVA è la schiera contenente tutti i FV selezionati
008800020806     D*        (da riempire solo se PARFL2 = "F")
008900951031     D  FVA                   17    241P 0
009000951031     D                                     DIM(45)
009100000609     D  PARFGS               242    244  0
009200020806      *
009300130319     D*-----------------------------------------------------
009400130318      * Parametri per pgm FNLST4R
009500130318     D fnlst4ds        DS
009600130318     D* ilst4gES = M - MANUTENZIONE FV RICHIAMATA
009700130318     D*          B - DA BORDERO'
009800130318     D*          A - apertura foglio
009900130318     D*          C - DA CHIUSURA FV SINGOLA
010000130318     D*          I - DA CHIUSURA DA IMP
010100130318     D*          N - DA INTERROGAZIONE  FV
010200130419     D*          M - Gestione fogli viaggio
010300130419     D*          P - Gestione fogli viaggio con videata piombi
010400130603     d
010500130603     d* solo per un po' N=nuova chiusura
010600130603     D  ilst4NEWC              2      2
010700130319     c* La data serve solo per immissione
010800130319     D  ilst4dfv               3     10  0
010900130319     d
011000130319     D  ilst4nfv              11     15
011100130318     D  IlsT4GES              16     16
011200130318     D* Se PARMSG pieno --> c'è stato ERRORE
011300130318     D  olst4msg              17     94
011400130429     D  iolsT4FOR             95     97
011500130429     D  IlsT4OLD              98     98
011600130318
011700130318     D* F12 = 'S'    --> premuto F12
011800130318     D  olst4F12             102    102
011900130318     D  ilst4fgs             103    105
012000130319 i   D  ilst4TRN             106    112  0
012100130318     D  ilst4NRRP            113    125
012200130319     D* se non immessa il numero relativo record della proposta
012300130319     d*  si può immettere a modo vecchio il trazionista
012400130319     D  ilst4PDR             126    132
012500130315     d*
012600130319     D*-----------------------------------------------------
012700130315     d* Richiamo a pgm di proposte traini
012800130315     D FNLST1DS        DS
012900130319     d* 'A' --> ricerca proposte per apertura FGV
013000130315     D  Ilst1GES               1      1
013100130315     D  Ilst1DAT               2      9
013200130315     D* 'G' --> caricare solo le proposte traini del giorno della settimana
013300130315     D*         corrispondente alla data;
013400130315     D* ' ' --> caricare tutte le proposte/traini alla data decorrenza
013500130315     D  Ilst1TUT              10     10
013600130315     D  Ilst1fgs              11     13
013700130417     d* scelta automatica del programma senza proposta videata
013800130417     D  Ilst1aut              14     14
013900130619
014000130619     d SflRcd          DS
014100130619     d  fgvnfv
014200130619     d  h02dtp
014300130619     d  h02hmp
014400130619     d  fgvlna
014500130619     d  viddel
014600130619     d  fgvsnp
014700130619     d  de2lna
014800130619     d  vidtrn
014900130619     d  viddtp
015000130619     d  vidhmp
015100130619     d  vidabp
015200130619     d  vidimp
015300130619     d  vidpr_m
015400130619     d  vidpr_r
015500130619     d  vidpr_p
015600130619     d  Selected                      1A
015700020806      *
015800900515     D WLBDAT          DS
015900940913     D  G02DAT                 1      8  0
016000940913     D  G02INV                 9     16  0
016100940913     D  G02ERR                17     17
016200940913     D  G02TGI                18     22  0
016300040611      *
016400040611      * STATUS DS
016500040611      *
016600040611     d Status         sds
016700040611     d   SDSprm          *parms
016800020806      *
016900130503     D                 DS
017000130503     D  CLNPOM                 1     31
017100130503     D  POM                    1     31
017200130503     D                                     DIM(31)
017300130503     D                 DS
017400130503     D  CLNMAT                 1     31
017500130503     D  MAT                    1     31
017600130503     D                                     DIM(31)
017700130503     D                 DS
017800130503     D  GG                     1      2  0
017900130503     D  MM                     3      4  0
018000130503     D  AA                     5      8  0
018100130503     D  WDATA                  1      8  0
018200020806      * VARIABILI
018300020806      *
018400130503     D COD             S                   LIKE(TBLCOD)
018500130503     D KEY             S                   LIKE(TBLKEY)
018600130503     D KTFP            S                   LIKE(FGVLNA)
018700130503     D KTFA            S                   LIKE(FGVLNA)
018800020806     D WDATE           S                   LIKE(FGVDFV)
018900130426     D Knfv6           S                   LIKE(brvnfv) INZ
019000130426     D Knfv5           S                   LIKE(fgvnfv) INZ
019100020806     D DATFIW          S                   LIKE(FGVDFV) INZ
019200020806     D DATINW          S                   LIKE(FGVDFV) INZ
019300130314     D savFIW          S                   LIKE(FGVDFV) INZ
019400130314     D savINW          S                   LIKE(FGVDFV) INZ
019500130322     D savrtp          S                   LIKE(vidrtp) INZ
019600130322     D savrta          S                   LIKE(vidrta) INZ
019700130426     D savtfog         S                   LIKE(tipfog) INZ
019800130503     D KANN            S                   LIKE(CLNANN)
019900130503     D KMES            S                   LIKE(CLNMES)
020000130314     D knpg            S                   LIKE(brvnpg) INZ(1)
020100130322     D KEPA            S                   LIKE(FV2EPA)
020200130322     D KTDH            S                   LIKE(FV2TDH)
020300130322     D KLAI            S                   LIKE(FV2LAI)
020400130429     D KTRC            S                   LIKE(FV4trc)
020500020806     D WTIME           S             14  0
020600020806     D NRR4            S              4  0 INZ
020700020806     D XZ              S              3  0 INZ
020800020806     D W0020           S              2  0 INZ
020900020806     D WRTA            S              1    INZ
021000041129     D Werrore         S              1    INZ
021100130430     D alfaFGS         S              3    INZ
021200130319     D KTLTIP          S                   LIKE(TLZTIP) INZ('T ')
021300130319     D KTLPDR          S                   LIKE(TLZPDR)
021400130319     D KPDR            S                   LIKE(fgvPDR)
021500130503     d Datasys         s               d   datfmt(*eur) inz(*sys)
021600130503     d wdtaeur         s               d   datfmt(*eur)
021700130503     d wdtaiso         s               d   datfmt(*iso)
021800130503     d wdtameno1       s              8  0
021900130503     d wdtameno2       s              8  0
022000130503     d wdtapiu1        s              8  0
022100130503     d wgiorno         s              2  0
022200130611     D wrichtrul90     S              1
022300130612     D ww02            S              1
022400040611      *
022500040611      * Parametri aggiuntivi:
022600040611      * - P.O. di cui selezionare i F.V. (da FIMS03R)
022700040611     d DLSpo           s              3
022800040630     d DLSpo2          s              3
022900040611     d wDLSpo          s              3  0
023000040630     d wDLSpo2         s              3  0
023100130619
023200130619     d ord_dtaorp      c                   'F11=Ord x Dt/OraPart'
023300130621     d ord_nfv         c                   'F11=Ord x N.Foglio  '
023400130621     d ord_tfa         c                   'F11=Ord x Fil.Arr.  '
023500130619     d w_nfv           C                   1
023600130619     d w_dtaorp        C                   2
023700130619     d w_tfa           C                   3
023800130619     d W_ord           S              1  0
023900130619      *---------------------------------------------------------------------
024000130619      * specifiche per ordinamento forzato sfl
024100130619     d NotUsed         S             16A
024200130619     d ReturnSize      S              9B 0
024300130619     d SizeList        S              9B 0
024400130619     D RrnLast         S              5I 0
024500130619     d*---
024600130619     d MaxKey          C                   2
024700130619     d Ascendente      C                   1
024800130619     d Discendente     C                   2
024900130619     d Carattere       C                   6
025000130619     d Put             C                   1
025100130619     d EndPut          C                   2
025200130619     d Get             C                   3
025300130619     d Numerico        C                   8
025400020806
025500130430      //---------------------------------------------------------------
025600130430      /copy gaitrasrc/srcprotopr,tibs02r
025700130619
025800130619      /COPY QSYSINC/QRPGLESRC,QLGSORT
025900130619     D QLGKL                         16    DIM(MaxKey)
026000130619     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
026100130619     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
026200130619     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
026300130619     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
026400130619
026500130619      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
026600130619      /COPY QSYSINC/QRPGLESRC,QUSEC
026700130430
026800020806      *****************************************************************
026900020806      *  RIEPILOGO INDICATORI
027000020806      *****************************************************************
027100130314      * 02    - interrogazione   OFF - gestione
027200130426      * 05    - Chiusura foglio viaggio singola
027300130314      * 06    - FOGLIO VIAGGIO IN GESTIONE AD ALTRO P.O.
027400020806      * 10    - ON SELEZIONE PER DATA
027500020806      * 11    - RICHIESTA LA VISUALIZZAZIONE DEL FOGLIO VIAGGIO PASSATO
027600020806      *         DAL PGM CHIAMANTE
027700020806      * 13    - VISUALIZZO SOLO I F.V. APERTI
027800020806      * 14    - P.O. GESTIONE ERRATO
027900130314      * 20    - PROGRAMMA RICHIAMATO PER SCELTA FOGLI VIAGGIO
028000130523      * 21    - proteggo scelte nel sfl ctl
028100020806      * 22    - DEVO PASSARE AL PGM CHIAMANTE TUTTI I F.V. SELEZIONATI
028200130429      * 25    - Reverse immage per pimobi non inseriti
028300130628      * 26    - Reverse immage per targa motrice    non inserita
028400130628      * 27    - Reverse immage per targa rimorchio  non inserita
028500020806      * 28    - INDICATORE DI EMISSIONE MESSAGGIO DI ERRORE
028600020806      * 29    - READC SUBFILE
028700020806      * 31/33 - DI COMODO
028800020806      * 35    - TIPO UTENTE
028900020806      * 36    - SFLNXTCHG
029000020806      * 40    - ERRORE
029100020806      * 48    - ERRORE
029200020806      * 72/73 - SFLCLR SFLDSP E SFLDSPCTLM           SFL04
029300020806      * 76/77 - ERRORE
029400020806      * 90    - INDICATORE DI ERRORE GENERICO
029500020806      * 95    - COMODO
029600130430      * 99    - Errore grave: esco dal pgm
029700020806      *****************************************************************
029800130314     c
029900130314     C                   EXSR      PULIZ
030000130314     c
030100130426    1c                   select
030200130426     c* INTERROGAZIONE
030300130506     c                   when      parflg='I' or parflg='P'
030400130314     c                   seton                                        02
030500130417     C                   MOVE      TES(2)        VIDTES
030600130506     c                   if        parflg='P'
030700130506     C                   SETON                                        20
030800130506     c                   endif
030900130314     C
031000130426     c* CHIUSURA FOGLI VIAGGIO
031100130426     c                   when      parflg='C'
031200130426     c                   seton                                        0205
031300130426     C                   MOVE      TES(4)        VIDTES
031400130426     c                   eval      tipfog='2'
031500130612     c                   clear                   viddin
031600130612     c                   exsr      ctrd02
034600130426     C
034700130314     c                   when      parflg='G'
034800130314     c                   setoff                                       02
034900130314     c
035000130506     c* RICERCA FOGLI
035100130314     c                   when      parflg='R'
035200130417     C                   MOVE      TES(3)        VIDTES
035300940323     C                   SETON                                        20
035400951106     C                   Z-ADD     PARDIN        VIDDIN
035500951106     C                   Z-ADD     PARDFI        VIDDFI
035600951102     C                   MOVEL     PARTFV        TIPFOG
035700000609     C                   Z-ADD     PARFGS        VIDFGS
035800130523     c
035900130523     c* se ho passato partfv = "2" proteggo le scelte perchè non mi servono
036000130523     c                   if        partfv='2'
036100130523     c                   seton                                        21
036200130523     c                   endif
036300130523     c
036400130314     c* controllo le date passate
036500130314     c                   exsr      ctrd02
036600130314     c* se errore --> esco
036700130314     c                   if        *in90
036800130314     c                   goto      fine
036900130314     c                   endif
037000130314     C*
037100130314     c* CONTROLLO SE DEVO PASSARE AL PGM CHIAMANTE TUTTI I FOGLI DI
037200130314     C*   VIAGGIO SELEZIONATI (PARFL2 = "F")
037300130314    2C     PARFL2        IFEQ      'F'
037400130314     C                   SETON                                        22
037500130314     C                   MOVEA     *ZEROS        FVA
037600130314    2C                   ENDIF
037700130314     C
037800130314    1c                   ENDSL
037900951031     C*
038000130314     C     INIZIO        TAG
038100130314     C                   EXSR      RIES04
038200951031     C*
038300951031     C* VISUALIZZAZIONE SFL E SCELTA FOGLIO
038400020806     C                   Z-ADD     1             XZ
038500960313     C                   Z-ADD     1             REC4
038600020806     C*    =================
038700951031     C     FORCT         TAG
038800020806     C*    =================
038900130314     C                   WRITE     LST3T01
039000130314     C                   WRITE     LST3Z02
039100130314     c* se non caricati fogli emetto videata
039200130315     C  n73              WRITE     LST3d03
039300130314
039400130612     c* se immissione multipla con f15 emissione window per richiesta
039500130612     c* stampa etichette
039600130711     c                   if        ww02='S'  and ww02<>'S'
039700130612     c                   clear                   ww02
039800130612     C                   write     LST3C02
039900130612     c                   exsr      geswdw02
040000130612     c                   endif
040100130612     c*
040200130314     C                   EXFMT     LST3C02
040300130419     C                   SETOFF                                       284048
040400960313     C                   CLEAR                   VIDMSG
040500951031     C*
040600130314     C* F3 o F12 - Fine/Ritorno
040800130314    1c                   if        *inKC or *inKL
041000130314     C                   GOTO      FINE
041100130314    1c                   endif
041200130503     c* controllo le selezioni immesse
041300130503     c                   exsr      Ctrd02
041400130503     c* Errore
041500130503    1c                   if        *in90
041600130503     c                   goto      forct
041700130503    1c                   endif
041800130619     c* F11=Ordinamento
041900130620     c                   if        *inkk=*on  and *in73
042000130619     c                   select
042100130619     c                   when      w_ord=w_nfv
042200130619     c                   eval      w_ord=w_dtaorp
042300130619     c                   eval      v2zf11=ord_tfa
042400130619     c                   eval      v2zf11b=ord_tfa
042500130619     c                   when      w_ord=w_dtaorp
042600130619     c                   eval      w_ord=w_tfa
042700130619     c                   eval      v2zf11=ord_nfv
042800130619     c                   eval      v2zf11b=ord_nfv
042900130619     c                   when      w_ord=w_tfa
043000130619     c                   eval      w_ord=w_nfv
043100130619     c                   eval      v2zf11=ord_dtaorp
043200130619     c                   eval      v2zf11b=ord_dtaorp
043300130619     c                   endsl
043400130619     c                   exsr      sr_ordina
043500130619     c                   goto      forct
043600130619     c                   endif
043700130430    c
043800130419     c* F13 - Immissione multipla con   videata
043900130419     c* F15 - Immissione multipla SENZA videata
044000130419    1c                   if        *inKP or *inkM
044100130314     c                   exsr      RicercaTRN
044200130619     c                   if        *inkp and §ppaeti=*blanks
044300130612     c                   eval      ww02='S'
044400130612     c                   endif
044500130314     c                   goto      inizio
044600130314    1c                   endif
044700130419    c
044800130417     c* F10 - Immissione singola  non dovrebbe servire ...
044900130417    1c                   if        *inKJ and not *inkj
045000130314     c                   exsr      Immissione
045100130314     c                   goto      inizio
045200130314    1c                   endif
045300130419    c
045400130419     c* F10 - Immissione singola  senza copia traino
045500130419    1c                   if        *inKJ
045600130419     c                   clear                   fnlst4ds
045700130419     c                   eval      ilst4ges='A'
045800130419     c                   eval      ilst4fgs =%editc(vidfgs:'X')
045900130419     c                   eval      ilst4dfv=datinw
046000130419     c                   eval      ilst4old='S'
046100130419     c                   eval      kpjbu=fnlst4ds
046200130419     c                   call      'FNLST4R'
046300130419     c                   parm                    kpjba
046400130419     c                   eval      fnlst4ds=kpjbu
046500130419     c
046600130419     c                   if        olst4msg<>*blanks
046700130419     c                   movel     olst4msg      vidmsg
046800130419     c                   seton                                        2848
046900130419     c                   goto      forct
047000130419     c                   else
047100130419     c                   goto      inizio
047200130419     c                   endif
047300130419     c
047400130419    1c                   endif
047500130314     c
047600130314     c
047700130314     c* se scelto un foglio lo visualizzo
047800130314    1c                   select
047900130314     c*  se cambiate date --> ricarico
048000130314     c                   when      vidnfv>0
048100130419     c                   if        not *in20 and not *in02
048200130315     c                   eval      vidsce='2'
048300130419     c                   endif
048400130506     c                   if        *in02     and not *in05
048500130419     c                   eval      vidsce='5'
048600130419     c                   endif
048700130506     c                   if        *in05
048800130506     c                   eval      vidsce='2'
048900130506     c                   endif
049000130314     c                   exsr      VisualFGV
049100130419     c                   if        vidmsg<>*blanks
049200130419     c                   seton                                        289048
049300130419     c                   else
049400130314     c                   clear                   vidnfv
049500130419     c                   endif
049600130314     c                   goto      forct
049700130314
049800130314     c
049900130314     c                   when      datfiw<>savfiw or
050000130314     c                             datinw<>savinw
050100130314     c                   goto      inizio
050200130426     c
050300130322     c                   when      vidrtp<>savrtp or
050400130322     c                             vidrta<>savrta
050500130322     c                   goto      inizio
050600130426
050700130426     c                   when      tipfog<>savtfog
050800130426     c                   goto      inizio
050900130314     c
051000130314     c* Se immesso numero foglio --> lo visualizzo
051100951031     C*
051200130314   x1c                   other
051300130315     c                   z-add     1             nrr4
051400130430     c
051500130430     c* e non ho caricato fogli non eseguo la lettura
051600130430   1dc                   if        *in73
051700951031     C     LEGGI         TAG
051800130315     C                   READC     LST3S02                                29
051900951031     C*
052000951031     C* SE SELEZIONATO ALMENO UNO ESCO ALTRIMENTI RIEMETTO SUBFILE
052100130315    1c                   if        *in29
052200130315    2C                   IF        *in22  and XZ >  1
052300951031     C                   GOTO      FINE
052400951031    2C                   ENDIF
052500951031     C*
052600130419     C                   GOTO      forct
052700130315    1c                   endif
052800960126     C**
052900130419     c* Per gestione ammessa solo opzione 2 / 5 / P
053000130315   1ac                   if        vidsce<>' '
053100130430
053200130430     c* Se chiusura foglio e scelta con "2" --> controllo se ha bolla
053300130430     c**   o spunte il foglio
053400130426     c
053500130426     c                   if        not *inkn and vidsce='2' and *in05
053600130426     c                   eval      knfv5=fgvnfv
053700130426     c                   exsr      contrbolle
053800130426     c                   if        *in90
053900130426     c                   seton                                        3640
054000130426     C                   Z-ADD     NRR4          REC4
054100130429     c                   exsr      AggioSFL
054200130426     C                   GOTO      forct
054300130426     c                   endif
054400130426     c                   endif
054500130502     c
054600130502     c                   if        *in05 and vidsce<>'2' and vidsce<>'5'
054700130502     c                   seton                                        369040
054800130502     c                   seton                                        28
054900130502     C                   Z-ADD     NRR4          REC4
055000130502     C                   MOVEA     MSG(12)       VIDMSG
055100130502     c                   exsr      AggioSFL
055200130502     C                   GOTO      forct
055300130502     c                   endif
055400130419     c
055500130314     c                   if        parflg='G' and vidsce<>'2'
055600130315     c                             and vidsce<>'5'and vidsce<>' '
055700130419     c                             and vidsce<>'P'
055800130314     c                   seton                                        369040
055900130314     c                   seton                                        28
056000130315     C                   Z-ADD     NRR4          REC4
056100130315     C                   MOVEA     MSG(12)       VIDMSG
056200130429     c                   exsr      AggioSFL
056300130314     C                   GOTO      forct
056400130314     c                   endif
056500130419     c
056600130314     c* Per Interrogazione solo opzione 5
056700130314     c                   if        parflg='I' and
056800130315     c                                 vidsce<>'5'  and vidsce<>' '
056900130314     c                   seton                                        369040
057000130314     c                   seton                                        28
057100130315     C                   MOVEA     MSG(12)       VIDMSG
057200130315     C                   Z-ADD     NRR4          REC4
057300130429     c                   exsr      AggioSFL
057400130314     C                   GOTO      forct
057500130314     c                   endif
057600130506     c                   if        parflg='P' and   vidsce<>'1' and
057700130506     c                                 vidsce<>'5'  and vidsce<>' '
057800130506     c                   seton                                        369040
057900130506     c                   seton                                        28
058000130506     C                   MOVEA     MSG(12)       VIDMSG
058100130506     C                   Z-ADD     NRR4          REC4
058200130506     c                   exsr      AggioSFL
058300130506     C                   GOTO      forct
058400130506     c                   endif
058500130315     c
058600130315     c* Per Ricerca        solo opzione 1
058700130314     c                   if        parflg='R' and
058800130315     c                                 vidsce<>'1'  and vidsce<>' '
058900130314     c                   seton                                        369040
059000130314     c                   seton                                        28
059100130315     C                   Z-ADD     NRR4          REC4
059200130315     C                   MOVEA     MSG(12)       VIDMSG
059300130429     c                   exsr      AggioSFL
059400130314     C                   GOTO      forct
059500130314     c                   endif
059600130314     c*
059700130314    2c                   if        vidsce='2' or  vidsce='5'
059800130419    2c                             or vidsce='P'
059900130314     c                   exsr      visualFGV
060000130419     c
060100130315     C                   Z-ADD     NRR4          REC4
060200130314     c* Se ce' errore devo riemettere sfl
060300130429     c* se non c'e' errore riaggiorno sfl con i dati del foglio
060400130419     c                   if        not *in90
060500130419     C                   MOVEL     ' '           VIDSCE
060600130419     c     kfgvsfl       chain     fnfgv01l
060700130419     c                   exsr      rigaSFL
060800130419     c                   endif
060900130419     c
061000130314     c                   seton                                        3640
061100130429     c                   exsr      AggioSFL
061200130315     c                   if        *in90
061300130314     C                   GOTO      forct
061400130315     c                   endif
061500130314     C*
061600130314     C                   GOTO      LEGGI
061700130314    2C                   ENDIF
061800960318     C*
061900130314     C* SE scelta    SELEZIONO IL FOGLIO CON L'OPZIONE "1"
062000130506    2c                   if        vidsce='1'
062100960126     C* PULISCO IL CAMPO DI SCELTA
062200020806     C                   CLEAR                   VIDSCE
062300130429     c                   exsr      AggioSFL
062400130314     c
062500960126     C* 22 OFF - PASSO UN SOLO NUMERO DI FV
062600130314    3C     *IN22         IFEQ      *OFF
062700960126     C                   Z-ADD     FGVNFV        PARNFV
062800020806     C                   z-add     VIDfgs        PARfgs
062900960126     C                   GOTO      FINE
063000960313     C*
063100130314   X3C                   ELSE
063200951031     C* 22 ON  - MEMORIZZO TUTTI I FV SELEZIONATI
063300020806    5C     XZ            IFLE      45
063400020806     C                   z-add     VIDfgs        PARfgs
063500020806     C                   Z-ADD     FGVLNA        FVA(XZ)
063600020806     C                   MOVEL     FGVNFV        FVA(XZ)
063700020806     C                   ADD       1             XZ
063800960126     C                   GOTO      LEGGI
063900960313   X5C                   ELSE
064000960313     C* SE SONO STATI SELEZIONATI PIU' DI 45 F.VIAGGIO: ERRORE
064100020806     C                   SUB       1             XZ
064200960313     C*
064300960313     C                   MOVEA     MSG(5)        K78
064400020806     C                   MOVEL     XZ            W003A             3
064500960313     C                   MOVEA     W003A         K05(3)
064600960313     C* ABBLANCO ZERO NON SIGNIFICATIVI
064700020806     C                   Z-ADD     3             XZ
064800020806    6C     K05(XZ)       DOWEQ     '0'
064900020806     C     XZ            ANDLT     5
065000020806     C                   CLEAR                   K05(XZ)
065100020806     C                   ADD       1             XZ
065200960313    6C                   ENDDO
065300960313     C*
065400960313     C                   MOVEA     K05(3)        K78(35)
065500960313     C                   MOVEA     K78           VIDMSG
065600960313     C                   SETON                                        4028
065700960313     C                   Z-ADD     NRR4          REC4
065800960313     C*
065900130315     C     NRR4          CHAIN     LST3S02                            33
066000960313     C                   MOVEL     '1'           VIDSCE
066100130429     c                   exsr      AggioSFL
066200960313     C                   GOTO      FORCT
066300960313    5C                   ENDIF
066400960313     C*
066500960126    3C                   ENDIF
066600960126    2C                   ENDIF
066700130430   1aC                   ENDIF
066800951031     C*
066900951102     C* PULISCO IL CAMPO DI SCELTA
067000130419     c                   if        vidsce<>' '
067100130419     C                   SETON                                        4036
067200130419     c                   else
067300130419     C                   SETOff                                       40
067400130419     c                   endif
067500130419     c
067600130419     C                   CLEAR                   VIDSCE
067700130315     c                   setoff                                       36
067800130429     c                   exsr      AggioSFL
067900130430   1bC                   ENDIF
068000130314    1C                   ENDsl
068100951031     C*
068200130430     c   73              goto      leggi
068300130430     c  n73              goto      forct
068400130314     c
068500000000     C     FINE          TAG
068600130315     C   20              MOVEL     fnlst3ds      KPJBU
068700130611
068800130611     C                   Z-ADD     130           LUNG             15 5
068900130611     c                   eval      comman='DLTOVR FILE(*ALL)'
069000130611     C                   CALL      'QCMDEXC'                            91
069100130611     C                   PARM                    COMMAN
069200130611     C                   PARM                    LUNG
069300130611
069400000000     C                   SETON                                        LR
069500951031     C*-----------------------------------------------------***********
069600951031     C* PULIZIA CAMPI VIDEO                                 *  PULIZ  *
069700951031     C*-----------------------------------------------------***********
069800900515     C     PULIZ         BEGSR
069900000620     C                   SETOFF                                       10  15
070000951102     C                   CLEAR                   VIDNFV
070100041129     C                   z-add     wdate         VIDDIN
070200940913     C                   Z-ADD     WDATE         VIDDFI
070300951102     C                   CLEAR                   TIPFOG
070400130322     C                   CLEAR                   VIDRTP
070500130322     C                   CLEAR                   VIDRTA
070600130319     c                   exsr      ctrd02
070700900514     C                   ENDSR
070800130429     C*-----------------------------------------------------***********
070900130429     C* Aggioranemto SFL                                    *  PULIZ  *
071000130429     C*-----------------------------------------------------***********
071100130429     C     AggioSFL      BEGSR
071200130613     c                   if        vidpr_p='N'
071300130429     c                   seton                                        25
071400130429     c                   endif
071500130613     c                   if        vidpr_m='N'
071600130523     c                   seton                                        26
071700130523     c                   endif
071800130613     c                   if        vidpr_r='N'
071900130613     c                   seton                                        27
072000130613     c                   endif
072100130429     c
072200130429     C                   UPDATE    LST3S02
072300130429     c                   setoff                                       364025
072400130613     c                   setoff                                       2627
072500130523     c
072600130429     c                   ENDSR
072700951031     C*-----------------------------------------------------***********
072800130314     C* CARICAMENTO  FOGLIO VIAGGIO
072900951031     C*-----------------------------------------------------***********
073000951031     C     RIES04        BEGSR
073100951031     C*
073200940912     C                   Z-ADD     1             NRR4
073300900517     C                   SETON                                        72
073400130315     C                   WRITE     LST3C02
073500911205     C                   SETOFF                                       7213
073600130430     c*
074200130430     C                   SETON                                        73
074300911205     C*
074400951031     C* 13 ON  - VISUALIZZO SOLO I FV APERTI
074500951102    1C     TIPFOG        IFEQ      '2'
074600911205     C                   SETON                                        13
074700951031    1C                   ENDIF
074800951031     C*
074900951031     C* CARICO IL SUBFILE
075000130314    1c                   if        not *in13
075100130314     c     KFGV02        setll     FNFGV02L
075200130314     c     vidfgs        reade     FNFGV02L                               31
075300020806     c                   else
075400130314     c     vidfgs        setll     FNFGV06L
075500130314     c     vidfgs        reade     FNFGV06L                               31
075600130314    1c                   endif
075700951031     C*
075800951031    1C     *IN31         DOWEQ     *OFF
075900041130     c     nrr4          andle     9990
076000951031     C*
076100951031    2C     FGVDFV        IFLE      DATFIW
076200900529     C                   SETOFF                                       95
076300951031     C*
076400951031    3C   13FGVDFV        IFGE      DATINW
076500911205     C                   SETON                                        95
076600951031    3C                   ENDIF
076700951031     C*
076800960219     C                   SELECT
076900960219     C     TIPFOG        WHENEQ    '1'
077000020806    3C     FGVFCF        ANDNE     *BLANKS
077100900529     C                   SETON                                        95
077200951031     C*
077300020806    3C     TIPFOG        WHENEQ    *BLANKS
077400900529     C                   SETON                                        95
077500960219    3C                   ENDSL
077600040611      *
077700040611      * Verifico P.O. arrivo
077800040630    3c                   if             *in95   =  *on
077900040630     c                             AND (wDLSpo  <> *zeros
078000040630     c                             and  wDLSpo  <> FGVlna
078100040630     c                             and  wDLSpo2 =  *zeros
078200040630     c                              OR  wDLSpo2 <> *zeros
078300040630     c                             and  wDLSpo  <> FGVlna
078400040630     c                             and  wDLSpo2 <> FGVlna)
078500040611     c                   eval      *in95 = *off
078600040611    3c                   endif
078700130322     c
078800130322     C* VERIFICO CON IL RITARDO PARTENZA
078900130322    3C   95VIDRTP        IFNE      *BLANKS
079000130322     C* CHAIN SU FV2
079100130322     C                   MOVEL     'P'           KEPA
079200130322     C                   CLEAR                   KLAI
079300130322     C                   MOVEL     'R'           KTDH
079400130322     C     KFV2          CHAIN     FNFV201L                           32
079500130322    4C     *IN32         IFEQ      *OFF
079600130322     C     FV2ATB        ANDEQ     *BLANKS
079700130322     C     FV2RTC        ANDNE     *BLANKS
079800130322     C* SE HO CHIESTO I FV SENZA RITARDO ERRORE
079900130322    5C     VIDRTP        IFEQ      'N'
080000130322     C                   SETOFF                                       95
080100130322    5C                   ENDIF
080200130322   X4C                   ELSE
080300130322     C*
080400130322     C* SE HO CHIESTO I FV CON   RITARDO ERRORE
080500130322    5C     VIDRTP        IFEQ      'R'
080600130322     C                   SETOFF                                       95
080700130322    5C                   ENDIF
080800130322    4C                   ENDIF
080900130322    3C                   ENDIF
081000130322     C**
081100130322     C* VERIFICO CON IL RITARDO ARRIVO
081200130322    3C   95VIDRTA        IFNE      *BLANKS
081300130322     C* CHAIN SU FV2
081400130322     C                   CLEAR                   WRTA
081500130322     C                   MOVEL     'A'           KEPA
081600130322     C                   MOVEL     'R'           KTDH
081700130322     C     KFV3          CHAIN     FNFV203L                           32
081800130322    4C     *IN32         DOWEQ     *OFF
081900130322     C*
082000130322    5C     FV2ATB        IFEQ      *BLANKS
082100130322     C     FV2RTC        ANDNE     *BLANKS
082200130322     C                   SETON                                        32
082300130322     C* SE HO CHIESTO I FV SENZA RITARDO ERRORE
082400130322    6C     VIDRTA        IFEQ      'N'
082500130322     C                   SETOFF                                       95
082600130322   X6C                   ELSE
082700130322     C* SE HO CHIESTO I FV CON RITARDO SOLO A POSTO
082800130322     C                   MOVEL     'S'           WRTA
082900130322    6C                   ENDIF
083000130322    5C                   ENDIF
083100130322     C*
083200130322     C  N32KFV3          READE     FNFV203L                               32
083300130322    4C                   ENDDO
083400130322     C*
083500130322    4C     VIDRTA        IFEQ      'R'
083600130322     C     WRTA          ANDEQ     *BLANKS
083700130322     C                   SETOFF                                       95
083800130322    4C                   ENDIF
083900130322    3C                   ENDIF
084000960219     C**
084100960219     C**
084200951031     C* 95 ON  - TUTTO OK: RECORD DA VISUALIZZARE
084300951031    3C     *IN95         IFEQ      *ON
084400951031     C*
084500951031     C* CAMPO SELEZIONE
084600020806     C                   CLEAR                   VIDSCE
084700130314     C
084800130419     c                   exsr      RigaSFL
084900130315     c
085000130315     c* La prima volta accedo indicatore di posizionamento ccursore
085100130419     c                   if        nrr4=1
085200130419     c                   seton                                        4036
085300130419     c                   endif
085400951031     C*
085500130315     C                   WRITE     LST3S02
085600940912     C                   ADD       1             NRR4
085700130430     c                   setoff                                       403625
085800130628     c                   setoff                                       2627
085900951031    3C                   ENDIF
086000951031     C*
086100020806     c                   if        not *in13
086200130314     c     vidfgs        reade     FGV02                                  31
086300020806     c                   else
086400130314     c     vidfgs        reade     FGV06                                  31
086500020806     c                   endif
086600951031     C*
086700951031   X2C                   ELSE
086800020806     C*
086900020806     c                   if        not *in13
087000020806     c                   eval      *in31 = *on
087100020806     c                   else
087200130314     c     vidfgs        reade     FGV06                                  31
087300020806     c                   endif
087400020806     C*
087500951031    2C                   ENDIF
087600951031     C*
087700951031    1C                   ENDDO
087800130619
087900130619     c                   clear                   rrnlast
088000130619     c                   if        nrr4>*zero
088100130711     c                   eval      rrnlast=nrr4-1
088200130619     c                   endif
088300130314
088400041129     c                   eval      werrore='N'
088500130314     c* Se caricati troppi record --> segnalo l'errore
088600041130     c                   if        *in31 =*off and nrr4 >9990
088700041129     c                   eval      werrore='S'
088800130314     C                   MOVEL     MSG(11)       VIDMSG
088900130314     C                   SETON                                        28
089000041129     c                   endif
089100951031     C*
089200130426     c* salvo i campi di ricaricamento sfl
089300130314     c                   eval      savfiw=datfiW
089400130314     c                   eval      savinw=datinW
089500130322     c                   eval      savrtp=vidrtp
089600130322     c                   eval      savrta=vidrta
089700130426     c                   eval      savtfog=tipfog
089800130315    1c                   if        nrr4=1
089900130315     c                   setoff                                       73
090000130315     c                   endif
090100130619
090200130619     c* vedo se devo ordinare il sfl
090300130620     c                   if        w_ord<>w_nfv  and *in73
090400130619     c                   exsr      sr_ordina
090500130619     c                   endif
090600130315     c
090700951031     C                   ENDSR
090800130419     C*-----------------------------------------------------***********
090900130419     C* Carico dati nella riga del sfl
091000130419     C*-----------------------------------------------------***********
091100130419     c     RigaSFL       BEGSR
091200130613     c                   setoff                                       252627
091300130419     C* GIRO DATA FOGLIO VIAGGIO
091400130419     C                   Z-ADD     FGVDFV        G02INV
091500130419     C                   MOVEL     *ZERO         G02DAT
091600130419     C                   MOVEL     '3'           G02ERR
091700130419     C                   CALL      'XSRDA8'
091800130419     C                   PARM                    WLBDAT
091900130419     C                   Z-ADD     G02DAT        W0020
092000130419     C                   MOVEL     G02DAT        VIDDEL
092100130419     C                   MOVE      W0020         VIDDEL
092200130419     C* DECODIFICO LINEA DI ARRIVO
092300130419     C     FGVLNA        CHAIN     AZORG                              31
092400130419     C  N31              MOVEL     ORGDES        DE2LNA
092500130419     C   31              MOVEL     *BLANKS       DE2LNA
092600130419     C* NUMERO TRAINO
092700130613    1c                   if        fgvtrn>999999
092800130613     C                   z-add     999999        VIDTRN
092900130613     c                   else
093000130620     C                   MOVE      FGVTRN        VIDTRN
093100130613    1c                   endif
093200130419     C*
093300130419     C                   MOVEL     *BLANKS       VIDABP
093400130419     C                   MOVEL     *BLANKS       VIDimp
093500130613     C                   clear                   VIDdtp
093600130613     C                   clear                   VIDhmp
093700130619     C                   clear                   h02dtp
093800130619     C                   clear                   h02hmp
093900130613     c* e c'e' data/ora reale --> prendo questa altrimenti cerco la teorica
094000130613     c*   solo se non annullato
094100130613    0c                   if        fgvatb=' '
094200130613    1c                   if        fgvdtp>0
094300130613     C                   Z-ADD     FGVDtp        G02INV
094400130613     C                   MOVEL     *ZERO         G02DAT
094500130613     C                   MOVEL     '3'           G02ERR
094600130613     C                   CALL      'XSRDA8'
094700130613     C                   PARM                    WLBDAT
094800130613     C                   MOVEL     G02DAT        viddtp
094900130613     c*
095000130613     c                   eval      vidhmp=%editw(fgvhmp:'0 :  ')
095100130619     c                   eval      h02dtp=fgvdtp
095200130619     c                   eval      h02hmp=fgvhmp
095300130613   x1c                   else
095400130613     c* cerco la teorica
095500130613     C                   MOVEL     'P'           KEPA
095600130613     C                   CLEAR                   KLAI
095700130613     C                   MOVEL     'T'           KTDH
095800130613     C     KFV2          CHAIN     FNFV201L                           32
095900130613    2c                   if        %found(fnfv201l) and fv2atb=' '
096000130613     C                   Z-ADD     Fv2dpa        G02INV
096100130613     C                   MOVEL     *ZERO         G02DAT
096200130613     C                   MOVEL     '3'           G02ERR
096300130613     C                   CALL      'XSRDA8'
096400130613     C                   PARM                    WLBDAT
096500130613     C                   MOVEL     G02DAT        viddtp
096600130613     c*
096700130613     c                   eval      vidhmp=%editw(fv2hpa:'0 :  ')
096800130619     c                   eval      h02dtp=fv2dpa
096900130619     c                   eval      h02hmp=fv2hpa
097000130613    2c                   endif
097100130613    1c                   endif
097200130613    0c                   endif
097300130419     c
097400130419     c                   clear                   vidabp
097500130419     C* ANNULLATO
097600130419    4C     FGVATB        IFEQ      '*'
097700130429     C                   MOVEL     'Ann   '      VIDABP
097800130419    4C                   ENDIF
097900130419     C*
098000130419     C* BORDERO'
098100130419     C     K02BLP79      setll     FNBLP79L                               31
098200130429     C   31              MOVE      'B     '      VIDABP
098300130419     c* Verifico se presenti spunte
098400130426     c                   eval      knfv6=fgvnfv
098500130419     C     Kbrv          setll     FNBrv01l                               31
098600130429     c   31              eval      %subst(vidabp:3:2)='Sp'
098700130419     C*
098800130419     C* PARTITO
098900130419    4C     FGVFCF        IFNE      *BLANKS
099000130419     C                   MOVE      'P'           VIDABP
099100130419    4C                   ENDIF
099200130419     c* Verifico che chiusura da IMP
099300130419     c                   if        fgvnfa>0
099400130613     c                   eval      vidimp='IMP'
099500130419     c                   endif
099600130429     c* Verifico la presenza dei piombi
099700130429     c* Escludo fogli defluenza e locali
099800130429     c                   eval      vidpr_p='  '
099900130613     c                   eval      vidpr_r='  '
100000130613     c                   eval      vidpr_m='  '
100100130618     c                   if        fgvatb=' '  and fgvfcf=' '
100200130618     c
100300130429     c                   if        fgvttr= ' ' or fgvttr='S'
100400130429     c                                         or fgvttr='H'
100500130429     C                   MOVEL     'P'           KEPA
100600130429     C                   CLEAR                   KLAI
100700130429     C                   MOVEL     'P'           KTrc
100800130429     C     KFV4          CHAIN     FNFV401L
100900130429    4C                   IF        %found(fnfv401l) and fv4atb=' '
101000130429     c                             and fv4not<>*blanks
101100130429     c                   else
101200130613     c                   eval      vidpr_p='N'
101300130429     c                   seton                                        25
101400130429     c                   endif
101500130523     c
101600130613     c* Targa motrice
101700130613     c                   if        fgvtrm=*blanks
101800130613     c                   eval      vidpr_m='N'
101900130523     c                   seton                                        26
102000130523     c                   endif
102100130613     c* Targa Rimorchio
102200130613     c                   if        fgvtrr=*blanks
102300130613     c                   eval      vidpr_r='N'
102400130613     c                   seton                                        27
102500130613     c                   endif
102600130523     c
102700130429     c                   endif
102800130613     c                   endif
102900130419     C*
103000130419     C* TARGHE MOTRICE E RIMORCHIO
103100130613     C*****              MOVEL     FGVTRM        VIDTRM
103200130523     C*****              MOVEL     FGVTrr        VIDTrr
103300130419     c                   ENDSR
103400951031     C*-----------------------------------------------------***********
103500130314     C* CONTROLLI dati di parzializzazione
103600951031     C*-----------------------------------------------------***********
103700130314     C     CTRD02        BEGSR
103800940913     C                   SETOFF                                       907677
103900000612     C                   SETOFF                                       1406
104000000609     C*
104100000609     C* CONTROLLO DATA INIZIALE
104200951031    1C     VIDDIN        IFNE      0
104300900517     C                   MOVE      VIDDIN        G02DAT
104400900517     C                   MOVEL     *BLANK        G02ERR
104500940913     C                   CALL      'XSRDA8'
104600900517     C                   PARM                    WLBDAT
104700951031    2C     G02ERR        IFEQ      '1'
104800940913     C                   MOVEL     MSG(2)        VIDMSG
104900940913     C                   SETON                                        762890
105000130503     C                   GOTO      ENDCT2
105100951031    2C                   ENDIF
105200130503     c
105300940913     C                   Z-ADD     G02DAT        VIDDIN
105400130503     c                   if        g02inv<>datinw
105500130503     c                   clear                   wforzb            1
105600130503     c                   endif
105700130503     C                   Z-ADD     G02INV        DATINW            8 0
105800130503     C                   Z-ADD     G02DAT        WDATA
105900130503     C                   MOVEL     MM            KMES
106000130503     c
106100130503     c* In caso di apertura fogli viaggio faccio il controllo
106200130503    2c                   if        *inkj   or *inkm  or *inkp
106300130503     c
106400130503      * non può essere minore o maggiore di 1 gg. lavorativo
106500130503    3c                   If        datinw < wdtameno1  or
106600130503     c                             datinw > wdtapiu1
106700130503     c                   Eval      VidMsg = Msg(9)
106800130503     c                   Eval      *in28 = *On
106900130503     c                   Eval      *in76 = *On
107000130503     c                   Eval      *in90 = *On
107100130503     C                   GOTO      ENDCT2
107200130503    3c                   EndIf
107300130503     c
107400130503     C* Se DATA DFV CADE IN GIORNO FESTIVO PER
107500130503     C*  LA TRAINI, AVVISO. ERRORE FORZABILE
107600130503     C                   CLEAR                   KTFA
107700130503     C                   Z-ADD     68            KTFP
107800130503     C     KCLN1         CHAIN     AZCLN01L                           83
107900130503    3C  N83POM(GG)       IFNE      *BLANKS
108000130503     C                   MOVEL     '1Y'          COD
108100130503     C                   MOVEL(P)  POM(GG)       KEY
108200130503     C     KTAB          CHAIN     TABEL                              83
108300130503     C                   MOVEL     TBLUNI        DS1Y
108400130503     C*
108500130503    4C  N83§1YFET        IFNE      *BLANKS
108600130503     C     WFORZB        ANDEQ     *BLANKS
108700130503     C                   MOVEL     MSG(17)       VIDMSG
108800130503     C                   SETON                                        287690
108900130503     C                   MOVEL     '1'           WFORZB            1
109000130503    4C                   ENDIF
109100130503    3c                   endif
109200130503    2c                   endif
109300951031     C*
109400951031   X1C                   ELSE
109500940913     C                   Z-ADD     0             DATINW
109600041130     c* solo se non sono stati richiesti solo gli aperti
109700041130     c     tipfog        ifne      '2'
109800041129     C                   MOVEL     MSG(10)       VIDMSG
109900041129     C                   SETON                                        762890
110000041130     c                   endif
110100951031    1C                   ENDIF
110200130314     C   90              GOTO      ENDCT2
110300951031     C*
110400951031     C* CONTROLLO DATA FINALE
110500951031    1C     VIDDFI        IFEQ      0
110600951031    2C     VIDDIN        IFEQ      *ZERO
110700940913     C                   Z-ADD     WDATE         VIDDFI
110800951031   X2C                   ELSE
110900940921     C                   Z-ADD     VIDDIN        VIDDFI
111000951031    2C                   ENDIF
111100951031    1C                   ENDIF
111200951031     C*
111300900517     C                   MOVE      VIDDFI        G02DAT
111400900517     C                   MOVEL     *BLANK        G02ERR
111500940913     C                   CALL      'XSRDA8'
111600900517     C                   PARM                    WLBDAT
111700951031    1C     G02ERR        IFEQ      '1'
111800940913     C                   MOVEL     MSG(2)        VIDMSG
111900940913     C                   SETON                                        772890
112000951031    1C                   ENDIF
112100130314     C   90              GOTO      ENDCT2
112200940913     C                   Z-ADD     G02DAT        VIDDFI
112300940913     C                   Z-ADD     G02INV        DATFIW            8 0
112400951031     C*
112500951031     C* DATA INIZIALE MAGGIORE DATA FINALE
112600951031    1C     DATINW        IFGT      DATFIW
112700940913     C                   MOVEL     MSG(1)        VIDMSG
112800940913     C                   SETON                                        762890
112900130314     C                   GOTO      ENDCT2
113000951031    1C                   ENDIF
113100960126     C**
113200960126     C* CONTROLLO NUMERO FOGLIO VIAGGIO
113300960710    1C     VIDNFV        IFGT      0
113400130314     C     kfgv          CHAIN     FNFGV01L                           31
113500960126    2C     *IN31         IFEQ      *ON
113600960126     C                   MOVEL     MSG(4)        VIDMSG
113700960201     C                   SETON                                        284890
113800130314     C                   GOTO      ENDCT2
113900960126    2C                   ENDIF
114000130426     c* Per chiusura fogli viaggio controllo se ha bolle altrimenti
114100130426     c*  si forza con F14
114200130426     c                   if        not *inkn and parflg='C'
114300130426     c                   eval      Knfv5=vidnfv
114400130426     c                   exsr      contrBolle
114500130426     c                   if        *in90
114600130426     c                   seton                                        48
114700130426     c                   goto      endct2
114800130426     c                   endif
114900130426     c
115000130426     c                   endif
115100130426     c
115200130315    2C                   ENDIF
115300951031     C*
115400130314     C     ENDCT2        ENDSR
115500130426     C*****************************************************************
115600130613     c*   controllo se il foglio ha delle bolle per chiusura FGV
115700130426     C*****************************************************************
115800130426     C     contrBolle    BEGSR
115900130613     c                   z-add     knfv5         knfv6
116000130607     c                   if        parfl2<>'N'
116500130607     c
116600130607     c                   else
116700130607     c* Per la nuova borderizzazine controllo solo se ci sono spunte
116900130607     C     KBRV          SETLL     FNBRV01L                               84
117000130607     c                   endif
117100130426     C*
117200130426     C* BOLLE O SPUNTE NON TROVATE --> FORZATURA
117400130902     C     *IN84         ifEQ      *OFF
117500130426     C                   SETON                                        9028
117600130902     C                   MOVEL     MSG(16)       VIDMSG
117800130426     C                   ENDIF
117900130426     c
118000130426     c                   ENDSR
118100130314     C*****************************************************************
118200130314     C*   Visualizza singolo foglio
118300130314     C*****************************************************************
118400130314     C     VisualFGV     BEGSR
118500130426     c                   setoff                                       90
118600130426     c* Se sono in chiusura fogli viaggio emetto finestra se si tratta
118700130426     c*  di foglio NON bis
118800130502     c                   if        vidsce<>'5'  and
118900130502     c                             parflg='C' and fgvSNP=' '
119000130426     c                             and fgvttr <>'D' and fgvttr<>'H'
119100130426     c                             and fgvttr<>'I'
119200130426    3c                   do        *hival
119300130426     c                   exfmt     lst3w01
119400130426     c* Premuto F12 riemetto videata iniziale
119500130426    4c                   if        *inkl
119600130426     C                   SETON                                        90
119700130426     c                   leave
119800130426    4c                   endif
119900130426     c
120000130426     c   kf              leave
120100130426    3c                   enddo
120200130426
120300130426     c                   endif
120400130426     c
120500130426     c                   if        not *in90
120600130318     C                   CLEAR                   fnlst4ds
120700130318     C                   MOVEL     FGVNFV        ilst4NFV
120800130318     C                   move      VIDfgs        ilst4FGS
120900130314     c* Per gestione flag =' '
121000130315     c                   if        vidsce='5'
121100130318     C                   MOVEL     'N'           Ilst4GES
121200130314     c                   endif
121300130426     c                   if        not *in05 and vidsce='2'
121400130318     C                   MOVEL     'M'           ilst4GES
121500130315     c                   endif
121600130426     c                   if        *in05 and vidsce='2'
121700130426     C                   MOVEL     'C'           ilst4GES
121800130603     c                   movel     parfl2        ilst4newc
121900130426     c                   endif
122000130419     c                   if        vidsce='P'
122100130419     C                   MOVEL     'P'           ilst4GES
122200130419     c                   endif
122300130318     C                   MOVEL     fnlst4ds      KPJBU
122400130318     C                   CALL      'FNLST4R'
122500130314     C                   PARM                    KPJBA
122600130318     C                   MOVEL     KPJBU         fnlst4ds
122700130314     C* MESSAGGIO DI ERRORE
122800130318    1C     olst4MSG      IFNE      *BLANKS
122900130318     C                   MOVEL     olst4MSG      VIDMSG
123000130314     C                   SETON                                        2890
123100130314    1C                   ENDIF
123200130426    1C                   ENDIF
123300130426     c
123400130314     C                   ENDSR
123500130314     C*****************************************************************
123600130314     C*   Ricerca Proposte per apertura FGV
123700130314     C*****************************************************************
123800130314     C     RicercaTRN    BEGSR
123900130315     c                   clear                   fnlst1ds
124000130319     c                   eval      ilst1GES='A'
124100130315     c                   movel     datinw        ilst1dat
124200130315     c                   eval      ilst1TUT='G'
124300130315     c                   movel     vidfgs        ilst1fgs
124400130417     c                   if        *inkp
124500130417     c                   eval      ilst1aut='S'
124600130417     c                   endif
124700130315     c
124800130315     c                   movel(p)  fnlst1ds      kpjbu
124900130315     c                   call      'FNLST1R'
125000130315     c                   parm                    kpjba
125100130315     c*
125200130314     C                   ENDSR
125300130314     C*****************************************************************
125400130314     C*   Immissione singolo foglio da traino
125500130314     C*****************************************************************
125600130314     C     Immissione    BEGSR
125700130319     C                   CLEAR                   coptrn
125800130319     C                   CLEAR                   vidpdr
125900130319     C                   CLEAR                   vidpdr
126000130319     c     emforw4       tag
126100130319     c                   exfmt     lst3w04
126200130319
126300130319     c                   setoff                                       289054
126400130319     c                   setoff                                       47
126500130319     c
126600130319     c                   if        *inkl
126700130319     c                   leavesr
126800130319     c                   endif
126900130319     c
127000130319     c                   exsr      ctrImm
127100130319
127200130319     c                   if        not *in90
127300130319     c
127400130319     c                   clear                   fnlst4ds
127500130319     c                   eval      ilst4ges='A'
127600130319     c                   eval      ilst4fgs =%editc(vidfgs:'X')
127700130319     c                   if        coptrn>*zeros
127800130319     c                   eval      ilst4TRN =%int(copTRN)
127900130319     c                   endif
128000130319     c                   eval      ilst4dfv=datinw
128100130319     c                   eval      ilst4pdr=vidpdr
128200130319     c                   eval      ilst4old='S'
128300130319     c                   eval      kpjbu=fnlst4ds
128400130319     c                   call      'FNLST4R'
128500130319     c                   parm                    kpjba
128600130319     c                   eval      fnlst4ds=kpjbu
128700130319     c
128800130319     c                   if        olst4msg<>*blanks
128900130319     c                   movel     olst4msg      w4cmsg
129000130319     c                   seton                                        2890
129100130319     c                   goto      emforw4
129200130319     c                   endif
129300130319     c
129400130319     c                   else
129500130319     c                   goto      emforw4
129600130319     c                   endif
129700130319     c
129800130314     C                   ENDSR
129900951031     C*****************************************************************
130000130319     C*   controlla immissione
130100951031     C*****************************************************************
130200130319     C     CtrImm        BEGSR
130300130319     c
130400130319     C     '?'           SCAN      COPTRN                                 90
130500130319    1C     *IN90         IFEQ      *ON
130600130319     C                   CLEAR                   TNTL00DS
130700130319     C                   Z-ADD     datinw        D00DDE
130800130319     C*
130900130319     C                   MOVE      VIDFGS        D00FIL
131000130319     C                   MOVEL     'C'           D00SCA
131100130319     C                   MOVEL     TNTL00DS      KPJBU
131200130319     C                   CALL      'TNTL17R'
131300130319     C                   PARM                    KPJBA
131400130319     C                   MOVEL     KPJBU         TNTL00DS
131500130319     C*
131600130319     C                   MOVEL     D00TRN        COPTRN
131700130319     C                   SETON                                        54
131800130319     C                   leavesr
131900130319    1C                   ENDIF
132000130319     C*
132100130319    1c                   if        coptrn<>*blanks and coptrn<>*zeros
132200130319     C                   TESTN                   COPTRN               83
132300130319     C                   MOVE      COPTRN        W001              1
132400130319    2C     *IN83         IFEQ      *OFF
132500130319     C     COPTRN        ORLT      *ZEROS
132600130319     C     W001          ORLT      '0'
132700130319     C                   MOVEL     MSG(13)       w4cmsg
132800130319     C                   SETON                                        549028
132900130319     C                   leavesr
133000130319    2C                   ENDIF
133100130319    1C                   ENDIF
133200130319     c
133300130319     C* RICERCA CODICE TRAZIONISTA
133400130319     C     '?'           SCAN      VIDPDR                                 83
133500130319    1C     *IN83         IFEQ      *ON
133600130319     C                   MOVEL     'T '          PA3TIP
133700130319     C                   MOVEL(P)  VIDDPD        PA3RSC
133800130319     C                   CLEAR                   PA3CSF
133900130319     C                   CLEAR                   PA3PDR
134000130319     C                   CLEAR                   PA3FLG
134100130319     C                   MOVEL     PARAM3        KPJBU
134200130319     C                   CALL      'TRUL16R'
134300130319     C                   PARM                    KPJBA
134400130319     C                   MOVEL     KPJBU         PARAM3
134500130319     C                   MOVEL     PA3PDR        VIDPDR
134600130319     C                   MOVEL     PA3RSC        VIDDPD
134700130319     C                   SETON                                        9047
134800130319     C                   LEAVESR
134900130319     C*
135000130319    1C                   ENDIF
135100130319     C*
135200130319     C* CONTROLLO VALIDITA' CODICE TRAZIONISTA
135300130319    1C     VIDPDR        IFNE      *BLANKS
135400130319     C     VIDPDR        ANDNE     *ZEROS
135500130319     C*
135600130319     C                   TESTN                   VIDPDR               31
135700130319     C  N31              MOVEL     MSG(14)       w4cmsg
135800130319     C  N31              SETON                                        902847
135900130319     C  N31              leavesr
136000130319     C*
136100130319     C                   MOVE      VIDPDR        KPDR
136200130319     C                   Z-ADD     KPDR          KTLPDR
136300130319     C     KTNTLZ        CHAIN     TNTLZ01L                           31
136400130319     C*
136500130319    2C     *IN31         IFEQ      *ON
136600130319     C     TLZATB        ORNE      ' '
136700130319     C                   MOVEL     MSG(14)       w4cmsg
136800130319     C                   SETON                                        904728
136900130319     C                   leavesr
137000130319   X2C                   ELSE
137100130319     C*
137200130319     C* IMPOSTO LA RAGIONE SOCIALE SOLO SE NON IMMESSA SE E' IL FASULLO
137300130319    3C     VIDPDR        IFNE      '0465000'
137400130319     C     VIDDPD        OREQ      *BLANKS
137500130319     C                   MOVEL     TLZRSC        VIDDPD
137600130319    3C                   ENDIF
137700130319    2C                   ENDIF
137800130319    1C                   ENDIF
137900130319     c
138000130319     c                   ENDSR
138100130319     C*****************************************************************
138200130319     C*   R O U T I N E      I N I Z I A L E
138300130319     C*****************************************************************
138400130319     C     *INZSR        BEGSR
138500020806      *
138600020806      ****  KLIST  ****
138700130503     C     KTAB          KLIST
138800130503     C                   KFLD                    CODUT
138900130503     C                   KFLD                    COD
139000130503     C                   KFLD                    KEY
139100951102     C* ACCESSO FNFGV02L
139200130319     C     KTNTLZ        KLIST
139300130319     C                   KFLD                    KTLTIP
139400130319     C                   KFLD                    KTLPDR
139500130314     C     KFGV02        KLIST
139600130314     C                   KFLD                    vidfgs
139700941121     C                   KFLD                    DATINW
139800130314     C     KFGV          KLIST
139900130314     C                   KFLD                    vidnfv
140000130315     C                   KFLD                    vidfgs
140100130419     c     kfgvsfl       klist
140200130419     c                   kfld                    FGVnfv
140300130419     c                   kfld                    vidfgs
140400020806     c* File FNBLP29L
140500020806     c     K02BLP79      klist
140600020806     c                   kfld                    FGVlnp
140700020806     c                   kfld                    FGVnfv
141200130314     c     Kbrv          klist
141300130314     c                   kfld                    knpg
141400130426     c                   kfld                    Knfv6
141500130314     c                   kfld                    FGVlnp
141600130322     C     KFV2          KLIST
141700130322     C                   KFLD                    FGVLNP
141800130322     C                   KFLD                    FGVNFV
141900130322     C                   KFLD                    KEPA
142000130322     C                   KFLD                    KLAI
142100130322     C                   KFLD                    KTDH
142200130429     C     KFV4          KLIST
142300130429     C                   KFLD                    FGVLNP
142400130429     C                   KFLD                    FGVNFV
142500130429     C                   KFLD                    KEPA
142600130429     C                   KFLD                    KLAI
142700130429     C                   KFLD                    Ktrc
142800130322     C     KFV3          KLIST
142900130322     C                   KFLD                    FGVLNP
143000130322     C                   KFLD                    FGVNFV
143100130322     C                   KFLD                    KEPA
143200130322     C                   KFLD                    KTDH
143300130503     C     KCLN1         KLIST
143400130503     C                   KFLD                    KTFP
143500130503     C                   KFLD                    KTFA
143600130503     C                   KFLD                    KANN
143700130503     C                   KFLD                    KMES
143800020806      *
143900020806      ****  IMPOSTO CAMPI FISSI  ****
144000951031     C                   SETOFF                                       01
144100130314     C                   MOVEL     'FNLST3R '    VIDPGM
144200951108     C* DATA DEL GIORNO
144300020806     C                   TIME                    WTIME
144400951108     C                   MOVE      WTIME         WDATE
144500020806      ****
144600951031     C     *ENTRY        PLIST
144700951031     C                   PARM                    KPJBA
144800040611     c                   parm                    DLSpo
144900040630     c                   parm                    DLSpo2
145000040611      *
145100130314     C                   MOVEL     KPJBU         FNLST3DS
145200040611     c                   clear                   wDLSpo
145300040630     c                   clear                   wDLSpo2
145400040611if  1c                   if        SDSprm > 1
145500040611     c                   move      DLSpo         wDLSpo
145600040611e   1c                   endif
145700040630if  1c                   if        SDSprm > 2
145800040630     c                   move      DLSpo2        wDLSpo2
145900040630e   1c                   endif
146000000609     C* TESTATA PGM
146100951031     C                   MOVE      TES(1)        VIDTES
146200951031     C*
146300951031     C                   Z-ADD     1             CODUT
146400951031     C                   CALL      'X§PARUT'
146500020806     C                   PARM                    UT§DSE0F
146600951031     C                   MOVEL     RAGUT         VIDRSU
146700951031     C                   MOVEL     REC80         CNCR80
146800000609     C                   SETON                                        35
146900000609      *
147000130314      * terminal partenza in gestione
147100130314     c                   if        parfgs>*zeros
147200130314     c                   movel     parfgs        vidfgs
147300130314     c                   else
147400130314     c                   movel     simfel        vidfgs
147500130314     c                   endif
147600130314     c
147700130314     c                   clear                   viddfgs
147800130314     c     vidfgs        chain     AZORG01L
147900020806     c                   if        %found(AZORG01L)
148000130315     c                   movel     ORGdes        viddfgs
148100020806     c                   endif
148200130430     c*
148300130503     c* Carico limiti date per immissione
148400130503     c                   exsr      sr_MinMax
148500130619
148600130430      /free
148700130619
148800130619        //Imposto l'ordinamento sfl di default: per numero foglio
148900130619         w_ord=w_nfv ;
149000130619         v2zf11=ord_dtaorp;
149100130619         v2zf11b=ord_dtaorp;
149200130430
150300130619        // Carico personalizzazione partenze per sapere se terminal
150400130619        // prevede la stampa delle etichette borderò
150500130619         clear TIBS02ds;
150600130619         clear dppa;
150700130619         T02Mod = 'C';
150800130619         T02sif = knsif;
150900130619         T02cod = 'PPA';
151000130619         T02ke1 =%editc(simfel:'X');
151100130619         TNTBE_RicercaControllo  (kpjba : tibs02ds);
151200130619         if  T02err  = *blank;
151300130619           dppa= T02uni;
151400130619         endif;
151500130430
151600130430      /end-free
151700130314     c
151800940912     C                   ENDSR
151900130503      **************************************************************************
152000130503      *  Calcolo meno 1 gg e più 1 gg lavorativi da data foglio viaggio
152100130503      **************************************************************************
152200130503     c     Sr_MinMax     BegSr
152300130503
152400130503     c                   Clear                   Ktfa
152500130503     c                   Z-Add     VidFgs        Ktfp
152600130503     c                   Move      datasys       wdtaeur
152700130503      * meno 1 gg. lavorativo
152800130503     c                   Do        *Hival
152900130503     c                   Subdur    1:*d          wdtaeur
153000130503     c                   extrct    wdtaeur:*d    wgiorno
153100130503     c                   extrct    wdtaeur:*m    kmes
153200130503     c                   extrct    wdtaeur:*y    kann
153300130503     c     kcln1         Chain     Azcln01l
153400130503     c                   If        Not %Found(azcln01l)
153500130503     c                   Leave
153600130503     c                   EndIf
153700130503     c                   If        POM(wgiorno) = 'F'
153800130503     c                   Iter
153900130503     c                   EndIf
154000130503     c                   Leave
154100130503     c                   EndDo
154200130503     c                   Move      wdtaeur       wdtaiso
154300130503     c                   Move      wdtaiso       wdtameno1
154400130503
154500130503      * meno 2 gg. lavorativ1 (non imposto wdataeur perchè già impostato
154600130503     c*  con data foglio-1)
154700130503     c                   Do        *Hival
154800130503     c                   Subdur    1:*d          wdtaeur
154900130503     c                   extrct    wdtaeur:*d    wgiorno
155000130503     c                   extrct    wdtaeur:*m    kmes
155100130503     c                   extrct    wdtaeur:*y    kann
155200130503     c     kcln1         Chain     Azcln01l
155300130503     c                   If        Not %Found(azcln01l)
155400130503     c                   Leave
155500130503     c                   EndIf
155600130503     c                   If        POM(wgiorno) = 'F'
155700130503     c                   Iter
155800130503     c                   EndIf
155900130503     c                   Leave
156000130503     c                   EndDo
156100130503     c                   Move      wdtaeur       wdtaiso
156200130503     c                   Move      wdtaiso       wdtameno2
156300130503      * più 1 gg. lavorativo
156400130503     c                   Move      datasys       wdtaeur
156500130503     c                   Do        *Hival
156600130503     c                   Adddur    1:*d          wdtaeur
156700130503     c                   extrct    wdtaeur:*d    wgiorno
156800130503     c                   extrct    wdtaeur:*m    kmes
156900130503     c                   extrct    wdtaeur:*y    kann
157000130503     c     kcln1         Chain     Azcln01l
157100130503     c                   If        Not %Found(azcln01l)
157200130503     c                   Leave
157300130503     c                   EndIf
157400130503     c                   If        POM(wgiorno) = 'F'
157500130503     c                   Iter
157600130503     c                   EndIf
157700130503     c                   Leave
157800130503     c                   EndDo
157900130503     c                   Move      wdtaeur       wdtaiso
158000130503     c                   Move      wdtaiso       wdtapiu1
158100130503
158200130503     c                   EndSr
158300130611      **************************************************************************
158400130611      * Window di richiesta stampa etichette borderò
158500130611      **************************************************************************
158600130611     c     geswdw02      BegSr
158700130611     c                   eval      wideti='S'
158800130611     c                   do        *hival
158900130611     c                   exfmt     lst3w02
159000130611     c                   if        *inkf
159100130611     c                   leave
159200130611     c                   endif
159300130611     c                   enddo
159400130611     c                   if        wideti='S'
159500130611     c* richiesta stampanti se non ancora fatta
159600130611     c                   if        wrichtrul90=' '
159700130611     c                   exsr      richstampan
159800130611     c                   if        d90f3<>'1' and not *in91
159900130611     c                   eval      wrichtrul90='S'
160000130611     c                   endif
160100130611     c                   if        *in91
160200130611     c                   endif
160300130611     c                   endif
160400130611     c                   if        wrichtrul90='S'
160500130611     c                   clear                   fnlst9ds
160600130611     c                   eval      LST9DFV=%dec(ilst1dat:8:0)
160700130611     c                   eval      LST9FOG='S'
160800130611     c                   eval      LST9PGMSE=d90psb
160900130611     c                   movel     fnlst9ds      kpjbu
161000130611     c                   call      'FNLST9R1'
161100130611     c                   parm                    kpjba
161200130611     c                   endif
161300130611     c                   endif
161400130611     c                   endsr
161500130611     C**************************************************************************
161600130611      * RICHIEDO LE STAMPANTI per stampa etichette
161700130611     C**************************************************************************
161800130611     c     RICHSTAMPAN   BEGSR
161900130611     C                   CLEAR                   TRUL90DS
162000130611      *
162100130611     C                   MOVEL     'S'           d90RSe
162200130611     C                   MOVEL     'ETBOR'       d90mde
162300130611     C                   CALL      'TRUL90R'
162400130611     C                   PARM                    KPJBA
162500130611     C                   PARM                    trul90DS
162600130611      *
162700130611     C* F3 - FINE
162800130611     c                   if        d90f3<>'1'
162900130611      *
163000130611      * OVRPRTF
163100130611     C*
163200130611     C                   Z-ADD     130           LUNG             15 5
163300130611     C                   CLEAR                   COMMAN          130
163400130611     c                   eval      comman='OVRPRTF FILE(FNLV22P) OUTQ('
163500130611     c                             + d90pre + ') FORMTYPE('
163600130611     c                             + d90mde + ') USRDTA(' + '''' + 'Etich.Bord'
163700130611     c                             + '''' + ') share(*yes)'
163800130611     C                   CALL      'QCMDEXC'                            91
163900130611     C                   PARM                    COMMAN
164000130611     C                   PARM                    LUNG
164100130611      *
164200130611     c                   endif
164300130611      *
164400130611     c                   ENDSR
164500130619      *-------------------------------------------------------------------------
164600130619      * ordinamento sfl in base alle richieste
164700130619      *-------------------------------------------------------------------------
164800130619     C     sr_ordina     BEGSR
164900130619      * IMPOSTO ENTRATA AD 'A'
165000130619     C                   EVAL      nrr4   = 0
165100130619      *
165200130619     C                   CLEAR                   QLGSCB
165300130619     C                   CLEAR                   QLGSCB00
165400130619
165500130619     c                   select
165600130619     c* ordinamento per n.foglio
165700130619     c                   when      w_ord=1
165800130619     ** 1 campo chiave.
165900130619     C                   EVAL      QLGNBRK    = 1
166000130619     C                   EVAL      QLGSP      = 1
166100130619     C                   EVAL      QLGSS      = %SIZE(fgvnfv)
166200130619     C                   EVAL      QLGDT      = carattere
166300130619     C                   EVAL      QLGSO      = Ascendente
166400130619     C                   EVAL      QLGKL(1)   = QLGSKL
166500130619     c* ordinamento per data/ora partenza
166600130619     c                   when      w_ord=2
166700130619     ** 2 campi chiave.
166800130619     C                   EVAL      QLGNBRK    = 2
166900130619     C                   EVAL      QLGSP      = 1 + %size(fgvnfv)
167000130619     C                   EVAL      QLGSS      = %SIZE(h02dtp)
167100130619     C                   EVAL      QLGDT      = carattere
167200130619     C                   EVAL      QLGSO      = Ascendente
167300130619     C                   EVAL      QLGKL(1)   = QLGSKL
167400130619     ** ora partenza
167500130619     C                   EVAL      QLGSP      = 1 + %size(fgvnfv) +
167600130619     c                                          %size(h02dtp)
167700130619     C                   EVAL      QLGSS      = %SIZE(h02hmp)
167800130619     C                   EVAL      QLGDT      = carattere
167900130619     C                   EVAL      QLGSO      = Ascendente
168000130619     C                   EVAL      QLGKL(2)   = QLGSKL
168100130619     c* ordinamento per filiale finale di arrivo
168200130619     c                   when      w_ord=3
168300130619     ** 1 campo chiave.
168400130619     C                   EVAL      QLGNBRK    = 1
168500130619     C                   EVAL      QLGSP      = 1 + %size(fgvnfv) +
168600130619     c                                          %size(h02dtp) +%size(h02hmp)
168700130619     C                   EVAL      QLGSS      = %SIZE(fgvlna)
168800130619     C                   EVAL      QLGDT      = carattere
168900130619     C                   EVAL      QLGSO      = Ascendente
169000130619     C                   EVAL      QLGKL(1)   = QLGSKL
169100130619     c                   endsl
169200130619
169300130619      * Load other sort parameters.
169400130619     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
169500130619     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
169600130619     C                   EVAL      QLGRT     = 8
169700130619     C                   EVAL      QLGOKL    = 80
169800130619     C                   EVAL      QLGLKE    = 16
169900130619     C                   EVAL      QLGLSS    = 290
170000130619
170100130619      * Initialize Sort I/O API fields.
170200130619     C                   EVAL      QLGRL00  = QLGRL
170300130619     C                   EVAL      QLGRC00  = 1
170400130619     C                   CLEAR                   QUSEI
170500130619     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
170600130619
170700130619      * First step - Initialize the sort routine.
170800130619     C                   CALL      'QLGSORT'
170900130619     C                   PARM                    QLGSCB
171000130619     C                   PARM                    NotUsed
171100130619     C                   PARM                    NotUsed
171200130619     C                   PARM                    SizeList
171300130619     C                   PARM                    ReturnSize
171400130619     C                   PARM                    QUSEC
171500130619
171600130619      * Next step - Write records to I/O routine.
171700130619     C                   EVAL      QLGRT00 = Put
171800130619
171900130619     C                   DO        RrnLast       nrr4
172000130619
172100130619     C     nrr4          CHAIN     lst3s02
172200130619     c                   if        not %found
172300130619     c                   leave
172400130619     c                   endif
172500130619     ** Solo le righe con Selected = 'Y' sono riordinate,
172600130619     ** quindi per fare un ordinamento di tutte le righe
172700130619     ** metto 'Y' sempre.
172800130619     C                   EVAL      Selected = 'Y'
172900130619
173000130619     C                   CLEAR                   QUSEI
173100130619     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
173200130619     C                   CALL      'QLGSRTIO'
173300130619     C                   PARM                    QLGSCB00
173400130619     C                   PARM                    SflRcd
173500130619     C                   PARM                    NotUsed
173600130619     C                   PARM                    SizeList
173700130619     C                   PARM                    NotUsed
173800130619     C                   PARM                    QUSEC
173900130619
174000130619     C                   ENDDO
174100130619
174200130619      * Next step - Signal end of input, clear subfile for reload.
174300130619     C                   EVAL      QLGRT00 = EndPut
174400130619     C                   CLEAR                   QUSEI
174500130619     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
174600130619
174700130619     C                   CALL      'QLGSRTIO'
174800130619     C                   PARM                    QLGSCB00
174900130619     C                   PARM                    SflRcd
175000130619     C                   PARM                    NotUsed
175100130619     C                   PARM                    SizeList
175200130619     C                   PARM                    NotUsed
175300130619     C                   PARM                    QUSEC
175400130619
175500130619     c                   seton                                        72
175600130619     C                   WRITE     lst3c02
175700130619     c                   setoff                                       7213
175800130619     c                   write     lst3z02
175900130619
176000130619      * Final step - Write the records back to the subfile.
176100130619     C                   EVAL      QLGRT00 = Get
176200130619     C                   DO        RrnLast       nrr4
176300130619     C                   CLEAR                   QUSEI
176400130619     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
176500130619     C                   CALL      'QLGSRTIO'
176600130619     C                   PARM                    QLGSCB00
176700130619     C                   PARM                    NotUsed
176800130619     C                   PARM                    SflRcd
176900130619     C                   PARM                    QLGRL00
177000130619     C                   PARM                    NotUsed
177100130619     C                   PARM                    QUSEC
177200130619      *
177300130619     c                   setoff                                       252627
177400130619     c                   setoff                                       40
177500130619     c                   if        vidpr_p='N'
177600130619     c                   seton                                        25
177700130619     c                   endif
177800130619     c                   if        vidpr_m='N'
177900130619     c                   seton                                        26
178000130619     c                   endif
178100130619     c                   if        vidpr_r='N'
178200130619     c                   seton                                        27
178300130619     c                   endif
178400130619     c                   if        nrr4=1
178500130619     c                   seton                                        40
178600130619     c                   endif
178700130619     C                   WRITE     lst3s02
178800130619     C                   ENDDO
178900130619
179000130619      *
179100130619     C                   ENDSR
179200130503
179300951031     C*****************************************************************
179400951031** MESSAGGI DI ERRORE
179500940913Immettere data iniziale minore o uguale a quella finale                        1
179600940913Immettere data formalmente valida                                              2
179700940913Non trovati fogli di viaggio per la selezione richiesta                        3
179800951031Immettere un foglio di viaggio esistente                                       4
179900960313Verranno selezionati solo i primi xxx Fogli Viaggio: enter x continuare        5
180000130430Opzione "5" non ammessa: immettere l'opzione "1" per selezionare e modificare  6
180100130430Interrogazione non ammessa: premere F3 e riproporre la richiesta in gestione   7
180200130902                                                                               8
180300130503Data DAL usata in APERTURA errata: non compresa nei limiti previsti            9
180400130314Data DAL obbligatoria
180500130314Non è possibile visualizzare tutti i fogli richiesti: superano i 9990!!!
180600130502Opzione non ammessa                                                           12
180700130319Immettere un numero di TRAINO esistente                                       13
180800130319Immettere codice TRAZIONISTA esistente                                        14
180900130902                                                                              15
181000130426Non sono presenti SPUNTE!!  Premere F14 per forzare                           16
181100130503La data Foglio Viaggio e' un giorno festivo: enter per forzare                17
181200130902                                                                              18
181300951031**
181400130417 * GESTIONE FOGLI VIAGGIO PARTENZA *
181500130417 ** INTERROGAZIONE FOGLI VIAGGIO  **
181600130417 ** S C E L T A    FOGLI VIAGGIO  **
181700130628 *CHIUSURA FOGLI VIAGGIO DA SPUNTE *
