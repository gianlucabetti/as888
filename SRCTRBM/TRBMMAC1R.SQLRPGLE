000100970526     H*------------------------------------------------------------------------*
000200161110     H DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP('QILE')
000300161110     H bnddir('UBBNDDIR')
000400970526     F*------------------------------------------------------------------------*
000500970526     F* DATA BASE
000600970526     F*------------------------------------------------------------------------*
000700161108     FTRBMMAC1D CF   E             WORKSTN INFDS(DEVDS1)
000800161108     F                                     SFILE(BMC1D05:SFLULT)
000900161108     F                                     SFILE(BMC1DMS:MSGULT)
001000970526     D*------------------------------------------------------------------------*
001100970526     D* SCHIERE
001200970526     D*------------------------------------------------------------------------*
001300970526     D*------------
001400970526     D* DECODIFICHE
001500970526     D*------------
001600970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
001700970526     D*------------
001800970526     D* MESSAGGI
001900970526     D*------------
002000970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002100970526     D*------------
002200970526     D* COMANDI
002300970526     D*------------
002400970526     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
002500970526     D*------------
002600970526     D* DESCRIZIONI OPZIONI
002700970526     D*------------
002800970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
002900970526     D*------------
003000970526     D* DESCRIZIONI TASTI FUNZIONALI
003100970526     D*------------
003200970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003300970526     D*------------
003400970526     D* OPZIONI DA CARICARE A VIDEO
003500970526     D*------------
003600970526     D OPV             S             19    DIM(24)
003700970526     D*------------
003800970526     D* FUNZIONI DA CARICARE A VIDEO
003900970526     D*------------
004000970526     D FUV             S             19    DIM(24)
004100970526     D*------------------------------------------------------------------------*
004200970526     D* INPUT
004300970526     D*------------------------------------------------------------------------*
004400970526     D*------------
004500970526     D* RIDEFINIZIONE OPZIONI VIDEO
004600970526     D*------------
004700970526     D OPDDS           DS
004800970526     D  OPDOPZ                 1      2
004900970526     D  OPDDE1                 3     19
005000970526     D  OPDOP1                21     22
005100970526     D  OPDFI0                23     34
005200970526     D  OPDATT                35     35
005300970526     D  OPDFI1                36     80
005400970526     D  OPDDES                 1     19
005500970526     D  OPDRIG                 1     80
005600970526     D*------------
005700970526     D* RIDEFINIZIONE FUNZIONI VIDEO
005800970526     D*------------
005900970526     D FUDDS           DS
006000970526     D  FUDDE1                 1      1
006100970526     D  FUDFUN                 2      3
006200970526     D  FUDDE2                 4     19
006300970526     D  FUDFI0                20     34
006400970526     D  FUDATT                35     35
006500970526     D  FUDFI1                36     80
006600970526     D  FUDDES                 1     19
006700970526     D  FUDRIG                 1     80
006800970526     D*------------
006900970526     D* DS GESTIONE TASTI FUNZIONALI
007000970526     D*------------
007100970526     D DSKEY         E DS
007200970526     D DEVDS1          DS
007300970526     D  KEY                  369    369
007400970526     D*------------
007500970526     D* DS "XMSG" - GESTIONE MESSAGGI
007600970526     D*------------
007700970526     D DSMSG         E DS
007800970526     D*------------
007900020919     D* DS REPERIMENTO DATI UTENTE
008000970526     D*------------
008100020919     D TIBS34DS      E DS                                                       *Profili utente
008200020919     D DDATIUTE      E DS                                                       *Dati utente
008300020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008400970526     D*------------------
008500970526     D* DS "XSRDA8" - CONTROLLA DATA (8)
008600970526     D*------------------
008700970526     D WLBDA8          DS                  INZ
008800970526     D  G08DAT                 1      8  0
008900970526     D  G08INV                 9     16  0
009000970526     D  G08ERR                17     17
009100970526     D  G08TGI                18     22  0
009200970526     D*------------
009300970526     D* DS DI PROCEDURA
009400970526     D*------------
009500161108     D TRBMMAC1DS    E DS
009600161108     D WTRBMMAC1DS     DS                  LIKEDS(TRBMMAC1DS)
009700970526     D*------------
009800970526     D* ARCHITETTURA
009900970526     D*------------
010000970526     D KPJBA         E DS
010100970715     D  UTENTE                 1      3
010200160713     D*------------
010300160713     D* campi di appoggio
010400160713     D*------------
010500161130     D DEPLivDrill     S              1s 0 inz
010600161115     D DEPCompara      S              1a   inz(*off)
010700170117     D DEPDtD          S              8s 0 inz
010800170117     D DEPDtA          S              8s 0 inz
010900170117     D DEPHHD          S              2s 0 inz
011000170117     D DEPHHA          S              2s 0 inz
011100170117     D DEPServerD      S             10a   inz
011200170117     D DEPServerA      S             10a   inz
011300161130     d wLivDrill       s              1s 0 inz(2)
011400161116     d wCompara        s               n   inz(*off)
011500170117     D* campi tipo data per le date a video
011600170117     D DtDISO          s               d
011700170117     D DtAISO          s               d
011800161116
011900161115     d wDtChk          s               d
012000161115     d wDatePrevious   s               d
012100161115     d wTimePrevious   s               t
012200161115     d wDC_Day_NaI     s              9a
012300161117     d wCntOggi        s              7s 0
012400161117     d wCntPrevious    s              7s 0
012500161115     d wTrovato        s               n   inz(*off)
012600161115     d wNoncene        s               n   inz(*off)
012700170118     D wD03HhD         S              2s 0 inz
012800170118     D wD03HhA         S              2s 0 inz
012900161111
013000160831       // - Stringa SQL da eseguire
013100160831     d wSQl            s           1024    inz  varying
013200161130     d TIMAC_fetch3    ds                  qualified
013300161111     d  DtChk                          d
013400161111     d  TmChk                          t
013500161130     d  Server                       10a
013600161111     d  Cnt1                          6s 0
013700161111     d  Dif1                         12s 3
013800161111     d  Sto1                          1
013900161130     d TIMAC_fetch2    ds                  qualified
014000161130     d  DtChk                          d
014100161130     d  TmChk                          t
014200161130     d  Cnt1                          6s 0
014300161130     d  Dif1                         12s 3
014400161130     d  Sto1                          1
014500161130     d TIMAC_fetch1    ds                  qualified
014600161111     d  DtChk                          d
014700161111     d  Cnt1                          6s 0
014800161111     d  Dif1                         12s 3
014900160916
015000161110     D*-------------------
015100161110     D* parametri UBMACHKR
015200161110     D*-------------------
015300161110       // Periodo DA
015400161110     d MACHKTSDa       s               z
015500161110       // Periodo AL
015600161110     d MACHKTSAl       s               z
015700161110       // Esito
015800161110     d MACHKEsito      s              1a
015900161110
016000161110       /copy gaitrasrc/srcProtoPR,UBMACHK1R
016100161110
016200970526     C*------------------------------------------------------------------------*
016300970526     C* MAIN LINES
016400970526     C*------------------------------------------------------------------------*
016500970526     C*
016600970526     C* SE NON CI SONO ERRORI, PROSEGUE
016700970526IF  1C     ERRL00        IFNE      '1'
016800970526     C*
016900970526     C* CARICA IL SFILE
017000970526     C                   EXSR      POSSFL
017100970526     C*
017200970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
017300970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
017400970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
017500970526     C     FINL00        OREQ      '1'                                          *FINE PGM
017600970526     C*
017700970526     C* REIMPOSTA ERRORE DI PROCERURA
017800970526     C                   MOVEL     '0'           ERRL00
017900970526     C*
018000970526     C* EMETTE IL SFILE
018100970526     C                   EXSR      OUTSFL
018200970526     C*
018300970526     C* GESTIONE TASTI FUNZIONALI
018400970526     C                   EXSR      FUNCON
018500970526     C*
018600970526E   2C                   ENDDO
018700970526E   1C                   ENDIF
018800970526     C* FINE PGM
018900970526     C                   SETON                                        LR
019000970526     C*------------------------------------------------------------------------*
019100970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
019200970526     C*------------------------------------------------------------------------*
019300970526     C     F03GES        BEGSR
019400970526     C*
019500970526     C* IMPOSTA "PREMUTO F03"
019600970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
019700161108     C                   MOVEL     '1'           MAC1F03                         *DI PROCEDURA
019800160916     C*
019900970526     C                   ENDSR
020000970526     C*------------------------------------------------------------------------*
020100970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
020200970526     C*------------------------------------------------------------------------*
020300970526     C     F05GES        BEGSR
020400160923     C*
020500161110
020600161110       // aggiorna TIMAC00F prima di caricare il SFL
020700161110       exsr sr_AggTIMAC;
020800161110
020900970526     C*
021000970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
021100970526     C*
021200970526     C                   ENDSR
021300970526     C*------------------------------------------------------------------------*
021400970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
021500970526     C*------------------------------------------------------------------------*
021600970526     C     F06GES        BEGSR
021700970526     C*
021800970526     C* INSERIMENTO CONSENTITO SOLO IN SEDE
021900020919IF  1C     DUTLPO        IFNE      'S'
022000970526     C                   MOVEL     '1'           ERRL00
022100970526     C                   MOVEL     MSG(3)        DSMSMS
022200970526E   1C                   ENDIF
022300970526     C*
022400970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
022500161108     C***                CLEAR                   DPROC
022600970529     C                   MOVEL     DEPOP0        WOP0
022700970529     C                   MOVEL     'F06'         WOP1
022800161108     C                   MOVEL     'TRBMMAC2R'   WPGM             10
022900970526     C                   EXSR      LANPGM                                       *LANCIA PGM
023000970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
023100970526E   1C                   ENDIF
023200970526     C*
023300970526     C                   ENDSR
023400161130     C*------------------------------------------------------------------------*
023500161201     C* F08GES - GESTIONE F08 -> Drill-up
023600161130     C*------------------------------------------------------------------------*
023700161201     C     F08GES        BEGSR
023800161130     C*
023900161201     C* a oggi 30/11/16 i livelli di Drill sono 3, per cui se siamo già al 1 non posso salire più
024000161201     C* (si va in ordine inverso)
024100161130     C                   if        wLivDrill= 1
024200161130     C                   MOVEL     '1'           ERRL00
024300161201     C                   MOVEL     MSG(13)       DSMSMS
024400161130     C                   LEAVESR
024500161130     C                   endif
024600161130
024700161130     C* diminuisce il livello di Drill
024800161130     C                   eval      wLivDrill -= 1
024900161130     C* disattivo la comparazione (se c'è)
025000161130     C                   eval      wCompara = *off
025100161130     C                   eval      *in03 = wCompara
025200161130     C* cambio il tasto di funzione F09 in Comparazione
025300161130     c                   eval      FUDRig = FUD(9)
025400161130     C                   MOVEL     'S'           FUDATT
025500161130     c                   eval      %subst(FUDRig:1:19) = 'F09=Comparazione'
025600161130     c                   eval      FUD(9) = FUDRig
025700161130     C*
025800161130     c                   eval      FUNGru = 0
025900161130     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
026000161130     C*
026100161130     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
026200161130     C                   EXSR      NEWPOS
026300161130     C*
026400161130     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
026500161130IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026600161130IF  2C     SWPOSI        IFNE      0
026700161130     C                   EXSR      POSSFL                                       *RICARICA SFILE
026800161130X   2C                   ELSE
026900161130IF  3C     SFLULT        IFNE      0
027000161130     C                   EXSR      ENTATT                                       *GESTIONE ENTER
027100161130     C*
027200161130     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
027300161130IF  4C     SWMODI        IFNE      0
027400161130     C                   EXSR      POSSFL
027500161130E   4C                   ENDIF
027600161130E   3C                   ENDIF
027700161130E   2C                   ENDIF
027800161130E   1C                   ENDIF
027900161130     C*
028000161130     C                   ENDSR
028100161111     C*------------------------------------------------------------------------*
028200161201     C* F07GES - GESTIONE F07 -> Drill-down
028300161111     C*------------------------------------------------------------------------*
028400161201     C     F07GES        BEGSR
028500161111     C*
028600161201     C* a oggi 30/11/16 i livelli di Drill sono 3, per cui se siamo già al 3 non posso scendere più
028700161201     C* (si va in ordine inverso)
028800161130     C                   if        wLivDrill= 3
028900161130     C                   MOVEL     '1'           ERRL00
029000161201     C                   MOVEL     MSG(14)       DSMSMS
029100161130     C                   LEAVESR
029200161130     C                   endif
029300161130
029400161130     C* aumento il livello di Drill
029500161130     C                   eval      wLivDrill += 1
029600161115     C* disattivo la comparazione (se c'è)
029700161115     C                   eval      wCompara = *off
029800161116     C                   eval      *in03 = wCompara
029900161115     C* cambio il tasto di funzione F09 in Comparazione
030000161115     c                   eval      FUDRig = FUD(9)
030100161115     C                   MOVEL     'S'           FUDATT
030200161115     c                   eval      %subst(FUDRig:1:19) = 'F09=Comparazione'
030300161115     c                   eval      FUD(9) = FUDRig
030400161111     C*
030500161111     c                   eval      FUNGru = 0
030600161111     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
030700161111     C*
030800161111     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
030900161111     C                   EXSR      NEWPOS
031000161111     C*
031100161111     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
031200161111IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
031300161111IF  2C     SWPOSI        IFNE      0
031400161111     C                   EXSR      POSSFL                                       *RICARICA SFILE
031500161111X   2C                   ELSE
031600161111IF  3C     SFLULT        IFNE      0
031700161111     C                   EXSR      ENTATT                                       *GESTIONE ENTER
031800161111     C*
031900161111     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
032000161111IF  4C     SWMODI        IFNE      0
032100161111     C                   EXSR      POSSFL
032200161111E   4C                   ENDIF
032300161111E   3C                   ENDIF
032400161111E   2C                   ENDIF
032500161111E   1C                   ENDIF
032600161111     C*
032700161111     C                   ENDSR
032800161115     C*------------------------------------------------------------------------*
032900161123     C* F09GES - GESTIONE F09 -> Comparazione/Senza comparazione
033000161115     C*------------------------------------------------------------------------*
033100161115     C     F09GES        BEGSR
033200161115     C*
033300161115     C                   if        wCompara = *off
033400161115     C* attivo la Comparazione
033500161115     C                   eval      wCompara = *on
033600161116     C                   eval      *in03 = wCompara
033700161123     C* cambio il tasto di funzione F09 in Senza comparazione
033800161115     c                   eval      FUDRig = FUD(9)
033900161115     C                   MOVEL     'S'           FUDATT
034000161123     c                   eval      %subst(FUDRig:1:19) = 'F09=Senza comp.'
034100161115     c                   eval      FUD(9) = FUDRig
034200161115     c                   else
034300161115     C* disattivo la comparazione
034400161115     C                   eval      wCompara = *off
034500161116     C                   eval      *in03 = wCompara
034600161115     C* cambio il tasto di funzione F09 in Comparazione
034700161115     c                   eval      FUDRig = FUD(9)
034800161115     C                   MOVEL     'S'           FUDATT
034900161115     c                   eval      %subst(FUDRig:1:19) = 'F09=Comparazione'
035000161115     c                   eval      FUD(9) = FUDRig
035100161115     c                   endif
035200161115     C*
035300161115     c                   eval      FUNGru = 0
035400161115     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
035500161115     C*
035600161115     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
035700161115     C                   EXSR      NEWPOS
035800161115     C*
035900161115     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
036000161115IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
036100161115IF  2C     SWPOSI        IFNE      0
036200161115     C                   EXSR      POSSFL                                       *RICARICA SFILE
036300161115X   2C                   ELSE
036400161115IF  3C     SFLULT        IFNE      0
036500161115     C                   EXSR      ENTATT                                       *GESTIONE ENTER
036600161115     C*
036700161115     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
036800161115IF  4C     SWMODI        IFNE      0
036900161115     C                   EXSR      POSSFL
037000161115E   4C                   ENDIF
037100161115E   3C                   ENDIF
037200161115E   2C                   ENDIF
037300161115E   1C                   ENDIF
037400161115     C*
037500161115     C                   ENDSR
037600970526     C*------------------------------------------------------------------------*
037700970526     C* F12GES - GESTIONE F12 -> RITORNO
037800970526     C*------------------------------------------------------------------------*
037900970526     C     F12GES        BEGSR
038000970526     C*
038100970526     C* IMPOSTA "PREMUTO F12"
038200970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
038300161108     C                   MOVEL     '1'           MAC1F12                         *DI PROCEDURA
038400970526     C*
038500970526     C                   ENDSR
038600970526     C*------------------------------------------------------------------------*
038700970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
038800970526     C*------------------------------------------------------------------------*
038900970526     C     F23GES        BEGSR
039000970526     C*
039100970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
039200970526     C*
039300970526     C                   ENDSR
039400970526     C*------------------------------------------------------------------------*
039500970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
039600970526     C*------------------------------------------------------------------------*
039700970526     C     F24GES        BEGSR
039800970526     C*
039900970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
040000970526     C*
040100970526     C                   ENDSR
040200970526     C*------------------------------------------------------------------------*
040300970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
040400970526     C*------------------------------------------------------------------------*
040500970526     C     RUPGES        BEGSR
040600970526     C*
040700970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
040800970526     C*
040900970526     C                   ENDSR
041000970526     C*------------------------------------------------------------------------*
041100970526     C* ENTGES - GESTIONE ENTER -> INVIO
041200970526     C*------------------------------------------------------------------------*
041300970526     C     ENTGES        BEGSR
041400970526     C*
041500970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
041600970526     C                   EXSR      NEWPOS
041700970526     C*
041800970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
041900970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
042000970526IF  2C     SWPOSI        IFNE      0
042100970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
042200970526X   2C                   ELSE
042300970526IF  3C     SFLULT        IFNE      0
042400970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
042500970526     C*
042600970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
042700970526IF  4C     SWMODI        IFNE      0
042800970526     C                   EXSR      POSSFL
042900970526E   4C                   ENDIF
043000970526E   3C                   ENDIF
043100970526E   2C                   ENDIF
043200970526E   1C                   ENDIF
043300970526     C*
043400970526     C                   ENDSR
043500970526     C*------------------------------------------------------------------------*
043600970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
043700970526     C*------------------------------------------------------------------------*
043800970526     C     O01GES        BEGSR
043900970526     C*
044000160923     C*
044100160923     C* simulo l'enter
044200160923     C                   exsr      NEWPOS
044300160923     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
044400160923     C*
044500970526     C                   ENDSR
044600970526     C*------------------------------------------------------------------------*
044700970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
044800970526     C*------------------------------------------------------------------------*
044900970526     C     O02GES        BEGSR
045000160927     C*
045100160927     C*
045200161108     C                   ENDSR
045300970526     C*------------------------------------------------------------------------*
045400161108     C* O03GES - GESTIONE OPZIONE 3=COPIA
045500970526     C*------------------------------------------------------------------------*
045600161108     C     O03GES        BEGSR
045700160927     C*
045800970526     C*
045900970526     C                   ENDSR
046000161108     C*------------------------------------------------------------------------*
046100161108     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
046200161108     C*------------------------------------------------------------------------*
046300161108     C     O04GES        BEGSR
046400161108     C*
046500161108     C*
046600161108     C                   ENDSR
046700970526     C*------------------------------------------------------------------------*
046800970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
046900970526     C*------------------------------------------------------------------------*
047000970526     C     O05GES        BEGSR
047100970526     C*
047200970526     C*
047300970526     C                   ENDSR
047400970526     C*------------------------------------------------------------------------*
047500970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
047600970526     C*------------------------------------------------------------------------*
047700970526     C     O07GES        BEGSR
047800970526     C*
047900970526     C*
048000970526     C                   ENDSR
048100970526     C*------------------------------------------------------------------------*
048200970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
048300970526     C*------------------------------------------------------------------------*
048400970526     C     NEWPOS        BEGSR
048500970526     C*
048600970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
048700970526     C*         1= RICHIESTO POSIZIONAMENTO
048800970526     C                   Z-ADD     0             SWPOSI            7 0
048900970526     C*
049000970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
049100970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
049200970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
049300970526     C                   Z-ADD     0             SWMODI            1 0
049400970526     C*
049500970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
049600161130     C     wLivDrill     IFNE      DEPLivDrill
049700161115     C     wCompara      ORNE      DEPCompara
049800170117     C     D03DtD        ORNE      DEPDtD
049900170117     C     D03DtA        ORNE      DEPDtA
050000170117     C     D03HhD        ORNE      DEPHhD
050100170117     C     D03HhA        ORNE      DEPHhA
050200170117     C     D03ServerD    ORNE      DEPServerD
050300170117     C     D03ServerA    ORNE      DEPServerA
050400970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
050500970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
050600970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
050700161130     C                   EVAL      DEPLivDrill = wLivDrill
050800161115     C                   EVAL      DEPCompara = wCompara
050900170117     C                   EVAL      DEPDtD = D03DtD
051000170117     C                   EVAL      DEPDtA = D03DtA
051100170117     C                   EVAL      DEPHhD = D03HhD
051200170117     C                   EVAL      DEPHhA = D03HhA
051300170117     C                   EVAL      DEPServerD = D03ServerD
051400170117     C                   EVAL      DEPServerA = D03ServerA
051500970526E   2C                   ENDIF
051600970526E   1C                   ENDIF
051700970526     C*
051800970526     C                   ENDSR
051900970526     C*------------------------------------------------------------------------*
052000970526     C* SETFIL - POSIZIONAMENTO RECORD
052100970526     C*------------------------------------------------------------------------*
052200970526     C     SETFIL        BEGSR
052300970526     C*
052400970526     C* POSIZIONAMENTO DEL FILE E PRIMA LETTURA IN BASE AL TIPO DI RICERCA
052500160830     C                   SELECT
052600160831     C*
052700161108IF  1C     WTIPRI        WHENEQ    *blank
052800160831     C* i dati li reperisce mediante SQL
052900160831     C                   EXSR      Prepara_SQL
053000160830     C*
053100160830E   1C                   ENDSL
053200970526     C* LETTURA RECORD
053300970526IF  1C     *IN99         IFEQ      *OFF
053400970526     C                   EXSR      LETFIL
053500970526E   1C                   ENDIF
053600970526     C*
053700970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
053800970526IF  1C     *IN99         IFEQ      *ON
053900970526     C                   Z-ADD     1             SFLEOF
054000970526E   1C                   ENDIF
054100970526     C*
054200970526     C                   ENDSR
054300970526     C*------------------------------------------------------------------------*
054400970526     C* LETFIL - LETTURA RECORD
054500970526     C*------------------------------------------------------------------------*
054600970526     C     LETFIL        BEGSR
054700970526     C*
054800970526     C* LEGGE FINO A:
054900970526     C*     - FINE FILE
055000970526     C*     - TROVATO RECORD VALIDO
055100970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
055200970526DO  1C     *IN99         DOWEQ     *OFF
055300970526     C     WRECOK        ANDEQ     'N'
055400970526     C*
055500970526     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO DI RICERCA
055600160830     C                   SELECT
055700160831     C*
055800161108IF  1C     WTIPRI        WHENEQ    *blank
055900160831     C* i dati li reperisce mediante SQL
056000160901     C                   EXSR      Legge_SQL
056100160830     C*
056200160830E   2C                   ENDSL
056300970526     C*
056400970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
056500970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
056600970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
056700970530X   2C                   ELSE
056800970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
056900970530E   2C                   ENDIF
057000970526E   1C                   ENDDO
057100970526     C*
057200970526     C                   ENDSR
057300970526     C*------------------------------------------------------------------------*
057400970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
057500970526     C*------------------------------------------------------------------------*
057600970526     C     CHKREC        BEGSR
057700970526     C*
057800970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
057900160913     C*
058000160919     C* Proc diversa
058100161108IF  1C*                  if        wDrillUp  =  *off and
058200161108IF  1C*                            D03Proc<> *blank and
058300161108IF  1C*                            D03Proc<> VMSProc
058400161108     C*                  MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
058500161108     C*                  leavesr
058600161108     C*                  endif
058700161108
058800160713
058900970526     C                   ENDSR
059000970526     C*------------------------------------------------------------------------*
059100160830     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE            §
059200970526     C*------------------------------------------------------------------------*
059300970526     C     IRESFL        BEGSR
059400970526     C*
059500970526     C                   MOVEL     *BLANKS       OPZKEY
059600161111
059700161130         select;
059800161130
059900161130          // liv.3 Giorno-Fascia oraria-Server
060000161130          when wLivDrill = 3;
060100161130           D05DtChk  = %char(TIMAC_fetch3.DtChk);
060200161130           D05TmChk  = %char(TIMAC_fetch3.TmChk);
060300161130           D05HHChk  = %subst(%char(TIMAC_fetch3.TmChk):1:2);
060400161130           D05Server = TIMAC_fetch3.Server;
060500161130           D05Cnt1   = TIMAC_fetch3.Cnt1;
060600161130           D05Dif1   = TIMAC_fetch3.Dif1;
060700161130           D05Sto1   = TIMAC_fetch3.Sto1;
060800161130           *in01 = *off;
060900161130           if TIMAC_fetch3.Sto1 <> 'S';
061000161130             *in01 = *on;
061100161130           endif;
061200161130           // se richiesta la comparazione, recupero il rcd stessa fascia ma del primo giorno
061300161130           // precedente che non è né sabato né domenica e che
061400161130           // il nr.totale di email inviate è almeno il 70% di quello di oggi
061500161130           // (per escludere i festivi)
061600161130           if wCompara = *on;
061700161130             exsr RepTIMACGgPrec;
061800161130           endif;
061900161130
062000161130          // liv.2 Giorno-Fascia oraria
062100161130          when wLivDrill = 2;
062200161130           D05DtChk  = %char(TIMAC_fetch2.DtChk);
062300161130           D05TmChk  = %char(TIMAC_fetch2.TmChk);
062400161130           D05HHChk  = %subst(%char(TIMAC_fetch2.TmChk):1:2);
062500161130           D05Server = *blank;
062600161130           D05Cnt1   = TIMAC_fetch2.Cnt1;
062700161130           D05Dif1   = TIMAC_fetch2.Dif1;
062800161130           D05Sto1   = TIMAC_fetch2.Sto1;
062900161111           *in01 = *off;
063000161130           if TIMAC_fetch2.Sto1 <> 'S';
063100161111             *in01 = *on;
063200161111           endif;
063300161115           // se richiesta la comparazione, recupero il rcd stessa fascia ma del primo giorno
063400161115           // precedente che non è né sabato né domenica e che
063500161116           // il nr.totale di email inviate è almeno il 70% di quello di oggi
063600161116           // (per escludere i festivi)
063700161115           if wCompara = *on;
063800161115             exsr RepTIMACGgPrec;
063900161115           endif;
064000161130           D05ServerP= *blank;
064100161115
064200161130          // liv.1 Giorno
064300161130          // non eseguibile la comparazione
064400161130          when wLivDrill = 1;
064500161130           D05DtChk  = %char(TIMAC_fetch1.DtChk);
064600161116           D05TmChk  = *blank;
064700161130           D05Server = *blank;
064800161116           D05HHChk  = *blank;
064900161130           D05Cnt1   = TIMAC_fetch1.Cnt1;
065000161130           D05Dif1   = TIMAC_fetch1.Dif1;
065100161111           *in01 = *off;
065200161111           D05Sto1 = *blank;
065300161115           *in02 = *off;
065400161115           D05Sto1P= *blank;
065500161115           D05DtChkP = *blank;
065600161115           D05HHChkP = *blank;
065700161130           D05ServerP= *blank;
065800161115           D05Cnt1P  = 0;
065900161115           D05Dif1P  = 0;
066000161130
066100161130         endsl;
066200160901     C*
066300970526     C                   ENDSR
066400161115       //----------------------------------------------------------------------*
066500161115       // Reperisco rcd TIMAC stesso orario ma giorno precedente, no sabato no domenica
066600161116       // e che il nr.totale di email inviate è almeno il 70% di quello di oggi
066700161116       // (per escludere i festivi)
066800161130       // La comparazione si fa solo per i liv.3 e 2
066900161115       //----------------------------------------------------------------------*
067000161130       begsr RepTIMACGgPrec;
067100161223
067200161223         clear d05dtchkp;
067300161223         clear d05hhchkp;
067400161223         clear d05serverp;
067500161223         clear d05cnt1p;
067600161223         clear d05dif1p;
067700161223         clear d05sto1p;
067800161115
067900161130         // ciclo finché non ho trovato il giorno precedente oppure è chiaro che
068000161130         // non ce ne è
068100161130         select;
068200161130         // liv.3 Giorno-Fascia oraria-Server
068300161130          when wLivDrill = 3;
068400161130           wDtChk = TIMAC_fetch3.DtChk;
068500161130         // liv.2 Giorno-Fascia oraria
068600161130          when wLivDrill = 2;
068700161130           wDtChk = TIMAC_fetch2.DtChk;
068800161130         endsl;
068900161130         wTrovato = *off;
069000161130         wNoncene = *off;
069100161130         dou wTrovato = *on or
069200161130             wNoncene = *on;
069300161130           exsr RepGgPrec;
069400161130         enddo;
069500161115
069600161130         // se ho trovato il giorno precedente giusto
069700161130         if wTrovato = *on;
069800161130           // leggo i dati
069900161130           select;
070000161130            // liv.3 Giorno-Fascia oraria-Server
070100161130            when wLivDrill = 3;
070200161130             exec sql
070300161130             select char(macdtchk), substr(char(mactmchk) , 1 , 2),
070400161130             macserver,
070500161130             maccnt1, macdif1, macsto1
070600161130             into
070700161130             :d05dtchkp, :d05hhchkp, :d05serverp, :d05cnt1p, :d05dif1p,
070800161130             :d05sto1p
070900161130             from timac00f
071000161130             where macdtchk = :wDatePrevious and
071100161130                   mactmchk = time(:d05tmchk) and
071200161130                   macserver= :d05server;
071300161130            // liv.2 Giorno-Fascia oraria
071400161130            when wLivDrill = 2;
071500161130             exec sql
071600161130             select char(macdtchk), substr(char(mactmchk) , 1 , 2),
071700161130             ' ',
071800161130             sum(maccnt1), avg(macdif1), max(macsto1)
071900161130             into
072000161130             :d05dtchkp, :d05hhchkp, :d05serverp, :d05cnt1p, :d05dif1p,
072100161130             :d05sto1p
072200161130             from timac00f
072300161130             where macdtchk = :wDatePrevious and
072400161130                   mactmchk = time(:d05tmchk)
072500161130             group by macdtchk, mactmchk;
072600161130           endsl;
072700161116
072800161116             // controllo il risultato
072900161116             select;
073000161116              when sqlcod < 0;
073100161116               // errore SQL
073200161116               clear d05dtchkp;
073300161116               clear d05hhchkp;
073400161201               clear d05serverp;
073500161116               clear d05cnt1p;
073600161116               clear d05dif1p;
073700161116               clear d05sto1p;
073800161201              // non c'è il rcd precedente (può capitare con l'aggiunta del server)
073900161116              when sqlcod = 100;
074000161201               clear d05dtchkp;
074100161201               clear d05hhchkp;
074200161201               clear d05serverp;
074300161201               clear d05cnt1p;
074400161201               clear d05dif1p;
074500161201               clear d05sto1p;
074600161116              other;
074700161116               // proseguo
074800161116             endsl;
074900161116
075000161116           endif;
075100161115
075200161115         endsr;
075300161115       //----------------------------------------------------------------------*
075400161115       // Reperisco il giorno precedente di una data passata, no sabato no domenica
075500161116       // e che il nr.totale di email inviate è almeno il 70% di quello di oggi
075600161116       // (per escludere i festivi)
075700161115       //----------------------------------------------------------------------*
075800161115         begsr RepGgPrec;
075900161115
076000161115         // controllo che giorno è oggi
076100161115         exec sql
076200161115         set :wDC_DAY_NAI = (select DC_DAY_NAI from DATECNV0F
076300161115                             where DC_Date = :wDtChk);
076400161115
076500161115         select;
076600161115          // se è lunedì, come precedente devo andare indietro di 3 gg (a venerdì)
076700161115          when wDC_DAY_NAI = 'Lunedì';
076800161115           wDatePrevious = wDtChk - %days(3);
076900161115          // se è domenica, come precedente devo andare indietro di 2 gg (a venerdì)
077000161117          when wDC_DAY_NAI = 'Domenica';
077100161115           wDatePrevious = wDtChk - %days(2);
077200161117          // se NON è né lunedì né domenica, vado indietro di 1 gg
077300161115          other;
077400161115           wDatePrevious = wDtChk - %days(1);
077500161115         endsl;
077600161115
077700161115         // controllo il totale differenza media del giorno definito
077800161115         // sia almeno il 70% di quella di oggi
077900161115         exec sql
078000161116         set :wCntOggi = (
078100161116                   select sum(maccnt1)
078200161115                   from timac00f
078300161115                   where macdtchk = :wDtChk
078400161115                   group by macdtchk);
078500161115
078600161115         // controllo il risultato
078700161115         select;
078800161115          when sqlcod < 0;
078900161115           // errore SQL
079000161115           wNoncene = *on;
079100161115           leavesr;
079200161115          when sqlcod = 100;
079300161115           // non c'è questo precedente per cui non ci sono rcd
079400161115           wNoncene = *on;
079500161115           leavesr;
079600161115          other;
079700161115           // proseguo
079800161115         endsl;
079900161115
080000161115         exec sql
080100161116         set :wCntPrevious = (
080200161116                   select sum(maccnt1)
080300161115                   from timac00f
080400161115                   where macdtchk = :wDatePrevious
080500161115                   group by macdtchk);
080600161115
080700161115         // controllo il risultato
080800161115         select;
080900161115          when sqlcod < 0;
081000161115           // errore SQL
081100161115           wNoncene = *on;
081200161115           leavesr;
081300161115          when sqlcod = 100;
081400161115           // non c'è questo precedente per cui non ci sono rcd
081500161115           wNoncene = *on;
081600161115           leavesr;
081700161115          other;
081800161115           // proseguo
081900161115         endsl;
082000161115
082100161116         if wCntPrevious >= wCntOggi * 70/100;
082200161115           // questa data va bene e leggo il rcd di TIMAC00F
082300161115           wTrovato = *on;
082400161115         else;
082500161115           // devo leggere un nuovo precedente
082600161115           wDtChk = wDatePrevious;
082700161115         endif;
082800161115
082900161115         endsr;
083000161115     C*------------------------------------------------------------------------*
083100161115     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
083200161115     C*------------------------------------------------------------------------*
083300161115     C     CHKPAR        BEGSR
083400161115     C*
083500970526     C* REIMPOSTO FLAG EVIDENZAZIONE ERRORI SUI PARAMETRI
083600970526     C                   SETOFF                                       808182
083700160913     C                   SETOFF                                       838485
083800970526     C*--------------------
083900970526     C* ORDINAMENTO
084000970526     C*--------------------
084100970526     C*--------------------
084200970526     C* SELEZIONI
084300970526     C*--------------------
084400970526     C*
084500160831     C* SE NESSUN ORDINAMENTO RICHIESTO, utilizzo il caricamento completo per Procedura
084600160831     C                   MOVEL     'ALL'         WTIPRI            3
084700170117     C                   EVAL      DtDISO = *loval
084800170117     C                   EVAL      DtAISO = *hival
084900170117     C*
085000170117     C* imposto il periodo
085100170117     C     ERRL00        IFEQ      '0'
085200170118       // se valorizzato solo Da, imposto Al con Da
085300170117       if D03DtD <> 0 and D03DtA = 0;
085400170117         D03DtA = D03DtD;
085500170117       endif;
085600170117       if D03HhD <> 0 and D03HhA = 0;
085700170117         D03HhA = D03HhD;
085800170117       endif;
085900170117       if D03ServerD <> *blank and D03ServerA = *blank;
086000170117         D03ServerA = D03ServerD;
086100170117       endif;
086200170117     C                   ENDIF
086300170117     C*
086400170117     C* CONTROLLA che il giorno da sia corretto formalmente
086500170117     C     ERRL00        IFEQ      '0'
086600170117     C                   IF        D03DtD   <> 0
086700170117     C                   MONITOR
086800170117     C* se il valore della data è > 6 cifre
086900170117     C                   IF        %len(%trim(%char(D03DtD  ))) > 6
087000170117     C* presumo sia scritta in ggmmaaaa
087100170117IF  5C                   EVAL      DtDISO = %date(D03DtD:*eur)
087200170117     C* se il valore della data è <= 6 cifre
087300170117     C                   ELSE
087400170117     C* presumo sia scritta in ggmmaa
087500170117IF  5C                   EVAL      DtDISO = %date(D03DtD:*dmy)
087600170117     C                   ENDIF
087700170117IF  2C                   ON-ERROR
087800170117     C                   MOVEL     '1'           ERRL00
087900170117     C                   SETON                                        80
088000170117     C                   MOVEL     MSG(8)        DSMSMS
088100170117     C                   LEAVESR
088200170117     C                   ENDMON
088300170117     C* rivalorizzo il video con la data in ggmmaaaa
088400170117     C                   EVAL      D03DtD = %dec(DtDISO:*eur)
088500170117     C                   ENDIF
088600170117     C                   ENDIF
088700170117     C*
088800170117     C* CONTROLLA che il giorno  a sia corretto formalmente
088900170117     C     ERRL00        IFEQ      '0'
089000170117     C                   IF        D03DtA   <> 0
089100170117     C                   MONITOR
089200170117     C* se il valore della data è > 6 cifre
089300170117     C                   IF        %len(%trim(%char(D03DtA  ))) > 6
089400170117     C* presumo sia scritta in ggmmaaaa
089500170118IF  5C                   EVAL      DtAISO = %date(D03DtA:*eur)
089600170117     C* se il valore della data è <= 6 cifre
089700170117     C                   ELSE
089800170117     C* presumo sia scritta in ggmmaa
089900170117IF  5C                   EVAL      DtAISO = %date(D03DtA:*dmy)
090000170117     C                   ENDIF
090100170117IF  2C                   ON-ERROR
090200170117     C                   MOVEL     '1'           ERRL00
090300170117     C                   SETON                                        81
090400170117     C                   MOVEL     MSG(8)        DSMSMS
090500170117     C                   LEAVESR
090600170117     C                   ENDMON
090700170117     C* rivalorizzo il video con la data in ggmmaaaa
090800170117     C                   EVAL      D03DtA = %dec(DtAISO:*eur)
090900170117     C                   ENDIF
091000170117     C                   ENDIF
091100170117     C*
091200170117     C* Ctrl che Da sia <= Al
091300170117     C     ERRL00        IFEQ      '0'
091400170117       // se valorizzato solo Da, imposto Al don Da
091500170117       if DtDISO > DtAISO;
091600170117     C                   MOVEL     '1'           ERRL00
091700170117     C                   SETON                                        8180
091800170117     C                   MOVEL     MSG(9)        DSMSMS
091900170117     C                   LEAVESR
092000170117       endif;
092100170117     C                   ENDIF
092200170117     C*
092300170117     C* Ctrl che Da sia <= Al
092400170117     C     ERRL00        IFEQ      '0'
092500170118       // se valorizzato solo Da, imposto Al con Da
092600170117       if D03HHD > D03HHA;
092700170117     C                   MOVEL     '1'           ERRL00
092800170117     C                   SETON                                        8283
092900170117     C                   MOVEL     MSG(9)        DSMSMS
093000170117     C                   LEAVESR
093100170117       endif;
093200170117     C                   ENDIF
093300170117     C*
093400170117     C* Ctrl che Da sia <= Al
093500170117     C     ERRL00        IFEQ      '0'
093600170118       // se valorizzato solo Da, imposto Al con Da
093700170117       if D03ServerD > D03ServerA;
093800170117     C                   MOVEL     '1'           ERRL00
093900170117     C                   SETON                                        8485
094000170117     C                   MOVEL     MSG(9)        DSMSMS
094100170117     C                   LEAVESR
094200170117       endif;
094300170117     C                   ENDIF
094400160830     C*
094500161108     C                   EVAL      WTIPRI = *blank
094600160830IF  1C     ERRL00        IFEQ      '0'
094700161108     C*                  SELECT
094800161108E   2C*                  ENDSL
094900970526E   1C                   ENDIF
095000970526     C*
095100970526     C                   ENDSR
095200970526     C*------------------------------------------------------------------------*
095300970526     C* LANPGM - LANCIA UN PROGRAMMA
095400970526     C*------------------------------------------------------------------------*
095500970526     C     LANPGM        BEGSR
095600970526     C*
095700970526     C* IMPOSTA LA DS DI PROCEDURA
095800161108     C                   MOVEL     WOP0          MAC1OP0                         *LIVELLO PROCEDURA
095900161108     C                   MOVEL     WOP1          MAC1OP1                         *LIVELLO PGM
096000161108     C                   MOVEL     '0'           MAC1F03                         *PREMUTO F03=FINE
096100161108     C                   MOVEL     '0'           MAC1F12                         *PREMUTO F12=RIT
096200161108     C                   MOVEL     '0'           MAC1ERR                         *'1'=ERRORE
096300161108     C                   MOVEL     *BLANKS       MAC1MSG                         *MESSAGGIO DI RIT
096400970526     C*
096500970526     C                   CALL      WPGM
096600970612     C                   PARM                    KPJBA
096700161108     C                   PARM                    TRBMMAC1DS
096800970526     C*
096900970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
097000161108IF  1C     MAC1F03       IFEQ      '1'                                          *PREMUTO F03
097100970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
097200970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
097300970526X   1C                   ELSE                                                   *NON PREMUTO F03
097400161108     C                   MOVEL     MAC1MSG       DSMSMS                         *MESSAGGIO DI RIT
097500970526E   1C                   ENDIF
097600970526     C*
097700970526     C                   ENDSR
097800970527     C*------------------------------------------------------------------------*
097900970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
098000970527     C*------------------------------------------------------------------------*
098100970527     C     CARTAB        BEGSR
098200970527     C*
098300970527     C                   ENDSR
098400970526     C*------------------------------------------------------------------------*
098500970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
098600970526     C*------------------------------------------------------------------------*
098700970526     C     CHKFOP        BEGSR
098800970626     C*
098900970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
099000970626IF  1C     DEPOP0        IFEQ      'R01'
099100161111     C***                MOVEL     OPD(1)        OPDRIG                         *SCELTA
099200161111     C***                MOVEL     'S'           OPDATT
099300161111     C***                MOVEL     OPDRIG        OPD(1)
099400970626X   1C                   ELSE
099500970626     C* OPZIONI
099600161111     C***                MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
099700161111     C***                MOVEL     'S'           OPDATT
099800161111     C***                MOVEL     OPDRIG        OPD(5)
099900970626     C* FUNZIONI
100000970626E   1C                   ENDIF
100100970626     C*
100200000602     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
100300970626IF  1C     DEPOP0        IFEQ      'R02'
100400161111     C***                MOVEL     OPD(1)        OPDRIG                         *MODIFICA
100500161111     C***                MOVEL     'S'           OPDATT
100600161111     C***                MOVEL     OPDRIG        OPD(1)
100700161111     C***                MOVEL     OPD(2)        OPDRIG                         *MODIFICA
100800161111     C***                MOVEL     'S'           OPDATT
100900161111     C***                MOVEL     OPDRIG        OPD(2)
101000161111     C***                MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
101100161111     C***                MOVEL     'S'           OPDATT
101200161111     C***                MOVEL     OPDRIG        OPD(4)
101300970715     C*
101400161111     C* SOLO SE EDP ABILITA  INSERIMENTO
101500970715IF  2C     UTENTE        IFEQ      'EDP'
101600161111     C***                MOVEL     OPD(3)        OPDRIG                         *COPIA
101700161111     C***                MOVEL     'S'           OPDATT
101800161111     C***                MOVEL     OPDRIG        OPD(3)
101900161111     C***                MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
102000161111     C***                MOVEL     'S'           FUDATT
102100161111     C***                MOVEL     FUDRIG        FUD(6)
102200970715E   2C                   ENDIF
102300970715E   1C                   ENDIF
102400970626     C*
102500000602     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO, ALLINEAMENTO
102600020919IF  1C     DUTLPO        IFNE      'S'                                          *FILIALE
102700970626     C                   MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
102800970626     C                   MOVEL     'N'           OPDATT
102900970626     C                   MOVEL     OPDRIG        OPD(4)
103000970626E   1C                   ENDIF
103100970526     C*
103200970526     C* GESTIONE OPZIONI A VIDEO
103300970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
103400970526     C                   EXSR      OPZGES
103500970526     C*
103600970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
103700970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
103800970526     C                   EXSR      FUNGES
103900970526     C*
104000970526     C                   ENDSR
104100020919     C*--------------------------------------------------------------------------------------------*
104200020919     C* REPERISCE I DATI UTENTE
104300020919     C*--------------------------------------------------------------------------------------------*
104400020919     C     REPDATIUTE    BEGSR
104500020919     C*
104600020919     C* INIZIALIZZA VARIABILI DI WRK
104700020919     C                   CLEAR                   TIBS34DS
104800020919     C                   CLEAR                   AZUTEDS
104900020919     C                   CLEAR                   DDATIUTE
105000020919     C*
105100020919     C     *DTAARA       DEFINE    §azute        azuteds
105200020919     C     *DTAARA       DEFINE    §datiute      ddatiute
105300020919     C                   IN(E)     *DTAARA
105400020919if  1C                   IF        %Error
105500020919     c                   EVAL      I34Tla = 'L'
105600020919     C                   CALL      'TIBS34R'
105700020919     C                   PARM                    Tibs34Ds
105800020919     C                   IN        *DTAARA
105900020919e   1C                   ENDIF
106000020919     C*
106100020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
106200020919     C                   MOVEL(P)  RSUT          DSFIRS           20
106300020919     C*
106400020919     C                   ENDSR
106500970526     C*------------------------------------------------------------------------*
106600970526     C* *INZSR - OPERAZIONI INIZIALI
106700970526     C*------------------------------------------------------------------------*
106800970526     C     *INZSR        BEGSR
106900970526     C*---
107000970526     C* RICEVIMENTO PARAMETRI
107100970526     C*---
107200970526     C     *ENTRY        PLIST
107300970612     C                   PARM                    KPJBA
107400160714     C*
107500161108     C                   EVAL      TRBMMAC1DS = kpjbu
107600160913     c                   if        kpjbu = *blank
107700161108     c                   clear                   TRBMMAC1DS
107800160913     c                   endif
107900970526     C*
108000970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
108100970526     C     XMSGPA        PLIST
108200970526     C                   PARM                    DSMSMS
108300970526     C                   PARM                    DSMSPG
108400970526     C                   PARM                    DSMSKE
108500970526     C                   PARM                    DSMSER
108600970526     C*
108700970526     C* IMPOSTA LA DS -TASTI PREMUTI-
108800970526     C                   CALL      'XKEY'
108900970526     C                   PARM                    DSKEY
109000970526     C*---
109100970526     C* VARIABILI NON RIFERITE AL DATA BASE
109200970526     C*---
109300970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
109400970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
109500970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
109600970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
109700970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
109800970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
109900970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
110000970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
110100970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
110200161108     C     *LIKE         DEFINE    MAC1OP0       DEPOP0                         *DI DEPOSITO
110300161108     C     *LIKE         DEFINE    MAC1OP1       DEPOP1
110400161108     C     *LIKE         DEFINE    MAC1OP0       WOP0                           *DI LAVORO
110500161108     C     *LIKE         DEFINE    MAC1OP1       WOP1
110600970526     C*---
110700970526     C* VARIABILI DI CONTROLLO
110800970526     C*---
110900970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
111000970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
111100970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
111200970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
111300970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
111400160714     C                   Z-ADD     14            SFLPAG                         *N°RIGHE X PAG
111500970526     C*
111600970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
111700161108     C                   MOVEL     'TRBMMAC1R'   DSMSPG
111800161108     C                   MOVEL     'TRBMMAC1R'   MSGQUE
111900970526     C                   MOVEL     *BLANKS       MSGKEY
112000970526     C                   MOVEL     *BLANKS       DSMSMS
112100970526     C                   MOVEL     *BLANKS       DSMSKE
112200970526     C                   Z-ADD     *ZEROS        DSMSER
112300970526     C*---
112400970526     C* VARIABILI RIFERITE AL DATA BASE
112500970526     C*---
112600970526     C*---
112700970526     C* CHIAVI DI LETTURA
112800970526     C*---
112900970527     C*
113000970527     C* REPERIMENTO DATI UTENTE
113100020919     C                   EXSR      REPDATIUTE
113200970527     C*
113300970527     C* CARICA LE TABELLE OCCORRENTI
113400970527     C                   EXSR      CARTAB
113500970526     C*
113600970526     C* DEPOSITA LE OPZIONI DI ENTRATA
113700161108     C                   MOVEL     MAC1OP0       DEPOP0                         *LIVELLO PROCEDURA
113800161108     C                   MOVEL     MAC1OP1       DEPOP1                         *LIVELLO PROGRAMMA
113900970526     C*
114000970526     C* CALCOLA LA DATA CORRENTE
114100970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
114200970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
114300970526     C                   Z-ADD     WN8           G08DAT
114400970526     C                   Z-ADD     *ZEROS        G08INV
114500970526     C                   MOVEL     '0'           G08ERR
114600970526     C                   CALL      'XSRDA8'
114700970526     C                   PARM                    WLBDA8
114800970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
114900970526     C*
115000970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
115100970526     C                   EXSR      CHKFOP
115200970526     C*
115300970526     C* IMPOSTA I PARAMETRI DI INGRESSO
115400970526     C*
115500161108     C*
115600970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
115700970530IF  1C     DEPOP0        IFEQ      'R01'
115800970530     C                   MOVEL     DEC(3)        VIDING
115900970530E   1C                   ENDIF
116000970530IF  1C     DEPOP0        IFEQ      'R02'
116100970530     C                   MOVEL     DEC(1)        VIDING
116200970530E   1C                   ENDIF
116300970530IF  1C     DEPOP0        IFEQ      'R05'
116400970530     C                   MOVEL     DEC(2)        VIDING
116500970530E   1C                   ENDIF
116600161110
116700161110       // aggiorna TIMAC00F prima di caricare il SFL
116800161110       exsr sr_AggTIMAC;
116900161110
117000970526     C*
117100970526     C                   ENDSR
117200161110
117300161110       //----------------------------------------------------------------------*
117400161110       // aggiorna TIMAC00F
117500161110       //----------------------------------------------------------------------*
117600161110       BEGSR sr_AggTIMAC;
117700161110
117800161110         // passo come periodo tutto (dal *loval a oggi)
117900161110         MACHKTSDa = *loval;
118000161110         MACHKTSAl = %timestamp();
118100161110
118200161110         // chiamo
118300161110         UBMACHK1R_S( *blank :
118400161110                      MACHKTSDa :
118500161110                      MACHKTSAl :
118600161110                      MACHKEsito );
118700161110
118800161110         // se l'esito non è 1=OK
118900161110         if MACHKEsito <>'1';
119000161110         // DEVO CREARE UN MSG MA PER ORA NON SO COME
119100161110
119200161110         endif;
119300161110
119400161110       ENDSR;
119500161110
119600970526     C*------------------------------------------------------------------------*
119700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
119800970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
119900970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
120000970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
120100970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
120200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
120300970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
120400970526     C*------------------------------------------------------------------------*
120500970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
120600970526     C*------------------------------------------------------------------------*
120700970526     C     POSSFL        BEGSR
120800970526     C*
120900970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
121000970526     C                   EXSR      CLRSFL
121100970526     C*
121200170118     C**************************************************************************
121300170118     C* siccome dentro la routine CARSFL c'è il richiamo alla routine SETFIL se
121400170118     C* non sono stati caricati rcd sul sfl (che vengono caricati dalla routine
121500170118     C* CARSFL dopo il richiamo alla SETFIL),
121600170118     C* non ha senso chiamare prima la SETFIL, faccio fare tutto alla CARSFL
121700170118     C**************************************************************************
121800970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
121900170118     C***                EXSR      SETFIL
122000970526     C*
122100970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
122200170118     C***  SFLEOF        IFEQ      0
122300170118     C***                EXSR      CARSFL
122400170118     C***                END
122500170118     C**************************************************************************
122600170118     C*
122700170118     C                   EXSR      CARSFL
122800970526     C*
122900970526     C                   ENDSR
123000970526     C*------------------------------------------------------------------------*
123100970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
123200970526     C*------------------------------------------------------------------------*
123300970526     C     ENTATT        BEGSR
123400970526     C*
123500970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
123600970526     C                   Z-ADD     0             OPZOK             1 0
123700970526     C*
123800970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
123900970526     C                   Z-ADD     0             SFLVAR            5 0
124000970526     C*
124100970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
124200970526     C                   Z-ADD     0             SFLERR
124300970526     C*
124400970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
124500970526     C                   Z-ADD     SFLULT        WFLULT
124600970526     C*
124700970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
124800161108     C                   READC     BMC1D05                                98
124900970526     C     *IN98         DOWEQ     '0'
125000970526     C*
125100970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
125200970526     C                   ADD       1             SFLVAR
125300970526     C*
125400970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
125500970526     C     OPZOK         IFEQ      0
125600970526     C*
125700970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
125800970526     C     OPZKEY        IFNE      *BLANKS
125900970526     C                   EXSR      OPZCON
126000970526     C                   END
126100970526     C*
126200970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
126300970526     C*            2. PULIZIA SCELTA IN SFL
126400970526     C     OPZOK         IFEQ      0
126500970526     C                   MOVEL     '0'           *IN41
126600970526     C                   MOVEL     *BLANKS       OPZKEY
126700970526     C                   ELSE
126800970526     C*
126900970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
127000970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
127100970526     C                   MOVEL     '1'           *IN41
127200970526     C                   Z-ADD     SFLULT        SFLERR
127300970526     C                   END
127400970526     C                   ELSE
127500970526     C*
127600970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
127700970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
127800970526     C                   MOVEL     '1'           *IN41
127900970526     C                   END                                                    OPZOK=0
128000970526     C*
128100970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
128200970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
128300970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
128400970526     C     OPZOK         IFNE      0
128500970526     C                   Z-ADD     SFLERR        SFLNBR
128600970526     C                   ELSE
128700970526     C                   Z-ADD     SFLULT        SFLNBR
128800970526     C                   END
128900970526     C*
129000970526     C* AGGIORNO RECORD SFL
129100160902     C*  imposto "effetti speciali"
129200160902     C                   EXSR      SFXD05
129300161108     C                   UPDATE    BMC1D05
129400970526     C*
129500161108     C                   READC     BMC1D05                                98
129600970526     C                   END                                                    DO WHILE
129700970526     C*
129800970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
129900970526     C                   Z-ADD     WFLULT        SFLULT
130000970526     C*
130100970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
130200970526     C     SFLVAR        IFEQ      0
130300970526     C*---                 MOVEL'1'       FINL00
130400970526     C                   END
130500970526     C*
130600970526     C                   ENDSR
130700970526     C*------------------------------------------------------------------------*
130800970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
130900970526     C*------------------------------------------------------------------------*
131000970526     C     CLRSFL        BEGSR
131100970526     C*
131200970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
131300970526     C* (0=NON FINE FILE  1=FINE FILE)
131400970526     C                   Z-ADD     0             SFLEOF
131500970526     C*
131600970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
131700970526     C                   Z-ADD     0             SFLULT
131800970526     C*
131900970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
132000970526     C                   MOVEL     '1'           *IN40
132100161108     C                   WRITE     BMC1D06
132200970526     C                   MOVEL     '0'           *IN40
132300970526     C*
132400970526     C                   ENDSR
132500970526     C*------------------------------------------------------------------------*
132600970526     C* CARSFL - CARICAMENTO SFL
132700970526     C*------------------------------------------------------------------------*
132800970526     C     CARSFL        BEGSR
132900970526     C*
133000970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
133100970526     C     SFLULT        IFEQ      0
133200970526     C                   EXSR      SETFIL
133300970526     C                   END
133400970526     C*
133500970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
133600970526     C                   Z-ADD     SFLULT        WFLULT
133700970526     C*
133800970526     C* CICLO:
133900970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
134000970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
134100970526     C                   Z-ADD     1             SFLRIG
134200970526     C     SFLRIG        DOWLE     SFLPAG
134300970526     C     SFLEOF        ANDEQ     0
134400970526     C*
134500970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
134600970526     C                   MOVEL     *BLANKS       OPZKEY
134700970526     C*
134800970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
134900970526     C                   EXSR      IRESFL
135000970526     C*
135100970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
135200970526     C                   ADD       1             SFLULT
135300970526     C*
135400970526     C* SCRITTURA RECORD SFL
135500160902     C*  imposto "effetti speciali"
135600160902     C                   EXSR      SFXD05
135700161108     C                   WRITE     BMC1D05
135800970526     C                   ADD       1             SFLRIG
135900970526     C*
136000970526     C* LETTURA FILE DA CARICARE
136100970526     C                   EXSR      LETFIL
136200970526     C                   END                                                    DO WHILE
136300970526     C*
136400970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
136500970526     C     WFLULT        IFNE      SFLULT
136600970526     C                   Z-ADD     WFLULT        SFLNBR
136700970526     C                   ADD       1             SFLNBR
136800970526     C                   END
136900970526     C*
137000970526     C                   ENDSR
137100970526     C*------------------------------------------------------------------------*
137200160902     C* SFXD05 - effetti speciali sfl D05 (RI, ND, BL, ...)
137300160902     C*          a seconda dei dati presenti in riga
137400970526     C*------------------------------------------------------------------------*
137500160902     C     SFXD05        BEGSR
137600161110     C*
137700161111     C* HI campi conteggio e media se storicizzato
137800161130     C* nel caso in cui siamo in un livello con fascia oraria
137900161110     C                   SETOFF                                       01
138000161130     C                   if        wLivDrill > 1
138100161110     C                   IF        D05Sto1 <>'S'
138200161110     C                   SETON                                        01
138300161110     C                   ENDIF
138400161111     C                   ENDIF
138500161115     C*
138600161115     C* HI campi giorno precedente conteggio e media se storicizzato
138700161130     C* nel caso in cui siamo in comparazione e a livello 2
138800161115     C                   SETOFF                                       02
138900161130     C                   if        wLivDrill= 2    and
139000161115     C                             wCompara = *on
139100161115     C                   IF        D05Sto1P <>'S'
139200161115     C                   SETON                                        02
139300161115     C                   ENDIF
139400161115     C                   ENDIF
139500160902     C*
139600160902     C                   ENDSR
139700160902     C*------------------------------------------------------------------------*
139800160902     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
139900160902     C*------------------------------------------------------------------------*
140000160902     C     OUTSFL        BEGSR
140100160902     C*
140200970526     C* WRITE RECORD TESTATA VIDEO
140300161108     C                   WRITE     BMC1D01
140400970526     C*
140500970526     C* VISUALIZZAZIONE MESSAGGIO
140600970526     C                   EXSR      SNDMSG
140700970526     C*
140800970526     C* WRITE RECORD OPZIONI VIDEO
140900161108     C                   WRITE     BMC1D02
141000970526     C*
141100970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
141200161108     C                   WRITE     BMC1D03
141300970526     C*
141400970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
141500161108     C                   WRITE     BMC1D04
141600970526     C*
141700970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
141800970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
141900970526     C     SFLEOF        IFEQ      0
142000970526     C                   MOVEL     '0'           *IN42
142100970526     C                   ELSE
142200970526     C*
142300970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
142400970526     C                   MOVEL     '1'           *IN42
142500970526     C                   END
142600970526     C*
142700970526     C* WRITE RECORD TASTI FUNZIONALI
142800161108     C                   WRITE     BMC1D09
142900970526     C*
143000970526     C* CASO DI SFL VUOTO
143100970526     C     SFLULT        IFLE      0
143200970526     C*
143300970526     C*    - WRITE RECORD VUOTO
143400161108     C                   WRITE     BMC1D0A
143500161108     C                   READ      BMC1D0A                                99
143600970526     C*
143700970526     C                   ELSE
143800970526     C*
143900970526     C* CASO DI SFL CON RECORDS
144000970526     C*
144100970526     C*    - WRITE RECORD CONTROLLO SFL
144200161108     C                   WRITE     BMC1D06
144300161108     C                   READ      BMC1D06                                99
144400970526     C                   END
144500970526     C*
144600970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
144700161108     C                   READ      BMC1D03                                99
144800970526     C*
144900970526     C                   ENDSR
145000970526     C*------------------------------------------------------------------------*
145100970526     C* SNDMSG - MANDA MESSAGGIO
145200970526     C*------------------------------------------------------------------------*
145300970526     C     SNDMSG        BEGSR
145400970526     C*
145500970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
145600970526     C                   MOVEL     '1'           *IN40
145700161108     C                   WRITE     BMC1DMC
145800970526     C                   MOVEL     '0'           *IN40
145900970526     C*
146000970526     C* VISUALIZZAZIONE MESSAGGIO
146100970526     C                   Z-ADD     0             MSGULT
146200970526     C                   CALL      'XMSG'        XMSGPA
146300970526     C     DSMSER        IFEQ      0
146400970526     C                   MOVEL     DSMSKE        MSGKEY
146500970526     C                   END
146600970526     C                   Z-ADD     1             MSGULT
146700161108     C                   WRITE     BMC1DMS                                      SFL MESS
146800161108     C                   WRITE     BMC1DMC                                      CONTR MESS
146900970526     C*
147000970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
147100970526     C                   MOVEL     *BLANKS       DSMSMS
147200970526     C*
147300970526     C                   ENDSR
147400970526     C*------------------------------------------------------------------------*
147500970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
147600970526     C*------------------------------------------------------------------------*
147700970526     C     FUNCON        BEGSR
147800970526     C*
147900970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
148000970526     C                   EXSR      FUNVAR
148100970526     C*
148200970526     C* PULISCO CAMPI DI LAVORO
148300970526     C                   Z-ADD     0             I                 5 0
148400970526     C                   Z-ADD     0             W                 5 0
148500970526     C*
148600970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
148700970526     C     FUNKEY        IFNE      *BLANKS
148800970526     C     1             DO        24            I
148900970526     C                   MOVEL     FUD(I)        FUDRIG
149000970526     C*
149100970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
149200970526     C     FUNKEY        IFEQ      FUDFUN
149300970526     C     FUDATT        ANDEQ     'S'
149400970526     C                   Z-ADD     I             W
149500970526     C                   END
149600970526     C                   END                                                    DO WHILE
149700970526     C*
149800970526     C* FUNZIONE NON ATTIVA (ERRORE)
149900970526     C     W             IFEQ      0
150000970526     C*
150100970526     C* EMISSIONE MESSAGGIO DI ERRORE
150200970526     C                   MOVEL     MSG(2)        DSMSMS
150300970526     C                   END                                                    W=0
150400970526     C                   END                                                    FUNKEY NE *BLANKS
150500970526     C*
150600970526     C* FUNZIONE ATTIVA (PROCEDI ...)
150700970526     C     W             IFNE      0
150800970526     C     FUNKEY        OREQ      *BLANKS
150900970526     C*
151000970526     C     FUNKEY        IFEQ      *BLANKS
151100970526     C                   EXSR      ENTGES
151200970526     C                   END
151300970526     C     FUNKEY        IFEQ      '03'
151400970526     C                   EXSR      F03GES
151500970526     C                   END
151600970526     C     FUNKEY        IFEQ      '05'
151700970526     C                   EXSR      F05GES
151800970526     C                   END
151900970526     C     FUNKEY        IFEQ      '06'
152000970526     C                   EXSR      F06GES
152100970526     C                   END
152200161130     C     FUNKEY        IFEQ      '07'
152300161130     C                   EXSR      F07GES
152400161111     C                   END
152500161130     C     FUNKEY        IFEQ      '08'
152600161130     C                   EXSR      F08GES
152700161130     C                   END
152800161115     C     FUNKEY        IFEQ      '09'
152900161115     C                   EXSR      F09GES
153000161115     C                   END
153100160913     C     FUNKEY        IFEQ      '12'
153200160913     C                   EXSR      F12GES
153300160913     C                   END
153400970526     C     FUNKEY        IFEQ      '23'
153500970526     C                   EXSR      F23GES
153600970526     C                   END
153700970526     C     FUNKEY        IFEQ      '24'
153800970526     C                   EXSR      F24GES
153900970526     C                   END
154000970526     C     FUNKEY        IFEQ      'RU'
154100970526     C                   EXSR      RUPGES
154200970526     C                   END
154300970526     C*
154400970526     C                   END
154500970526     C*
154600970526     C                   ENDSR
154700970526     C*------------------------------------------------------------------------*
154800970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
154900970526     C*------------------------------------------------------------------------*
155000970526     C     FUNVAR        BEGSR
155100970526     C*
155200970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
155300970526     C     KEY           IFEQ      ENTER
155400970526     C                   MOVEL     *BLANKS       FUNKEY
155500970526     C                   END
155600970526     C     KEY           IFEQ      F1
155700970526     C                   MOVEL     '01'          FUNKEY
155800970526     C                   END
155900970526     C     KEY           IFEQ      F2
156000970526     C                   MOVEL     '02'          FUNKEY
156100970526     C                   END
156200970526     C     KEY           IFEQ      F3
156300970526     C                   MOVEL     '03'          FUNKEY
156400970526     C                   END
156500970526     C     KEY           IFEQ      F4
156600970526     C                   MOVEL     '04'          FUNKEY
156700970526     C                   END
156800970526     C     KEY           IFEQ      F5
156900970526     C                   MOVEL     '05'          FUNKEY
157000970526     C                   END
157100970526     C     KEY           IFEQ      F6
157200970526     C                   MOVEL     '06'          FUNKEY
157300970526     C                   END
157400970526     C     KEY           IFEQ      F7
157500970526     C                   MOVEL     '07'          FUNKEY
157600970526     C                   END
157700970526     C     KEY           IFEQ      F8
157800970526     C                   MOVEL     '08'          FUNKEY
157900970526     C                   END
158000970526     C     KEY           IFEQ      F9
158100970526     C                   MOVEL     '09'          FUNKEY
158200970526     C                   END
158300970526     C     KEY           IFEQ      F10
158400970526     C                   MOVEL     '10'          FUNKEY
158500970526     C                   END
158600970526     C     KEY           IFEQ      F11
158700970526     C                   MOVEL     '11'          FUNKEY
158800970526     C                   END
158900970526     C     KEY           IFEQ      F12
159000970526     C                   MOVEL     '12'          FUNKEY
159100970526     C                   END
159200970526     C     KEY           IFEQ      F13
159300970526     C                   MOVEL     '13'          FUNKEY
159400970526     C                   END
159500970526     C     KEY           IFEQ      F14
159600970526     C                   MOVEL     '14'          FUNKEY
159700970526     C                   END
159800970526     C     KEY           IFEQ      F15
159900970526     C                   MOVEL     '15'          FUNKEY
160000970526     C                   END
160100970526     C     KEY           IFEQ      F16
160200970526     C                   MOVEL     '16'          FUNKEY
160300970526     C                   END
160400970526     C     KEY           IFEQ      F17
160500970526     C                   MOVEL     '17'          FUNKEY
160600970526     C                   END
160700970526     C     KEY           IFEQ      F18
160800970526     C                   MOVEL     '18'          FUNKEY
160900970526     C                   END
161000970526     C     KEY           IFEQ      F19
161100970526     C                   MOVEL     '19'          FUNKEY
161200970526     C                   END
161300970526     C     KEY           IFEQ      F20
161400970526     C                   MOVEL     '20'          FUNKEY
161500970526     C                   END
161600970526     C     KEY           IFEQ      F21
161700970526     C                   MOVEL     '21'          FUNKEY
161800970526     C                   END
161900970526     C     KEY           IFEQ      F22
162000970526     C                   MOVEL     '22'          FUNKEY
162100970526     C                   END
162200970526     C     KEY           IFEQ      F23
162300970526     C                   MOVEL     '23'          FUNKEY
162400970526     C                   END
162500970526     C     KEY           IFEQ      F24
162600970526     C                   MOVEL     '24'          FUNKEY
162700970526     C                   END
162800970526     C     KEY           IFEQ      ROLLUP
162900970526     C                   MOVEL     'RU'          FUNKEY
163000970526     C                   END
163100970526     C     KEY           IFEQ      HELP
163200970526     C                   MOVEL     'HP'          FUNKEY
163300970526     C                   END
163400970526     C*
163500970526     C                   ENDSR
163600970526     C*------------------------------------------------------------------------*
163700970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
163800970526     C*------------------------------------------------------------------------*
163900970526     C     FUNGES        BEGSR
164000970526     C*
164100970526     C* PULISCO CAMPI DI LAVORO
164200970526     C                   Z-ADD     0             W                 5 0
164300970526     C*
164400970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
164500970526     C* E CARICO TABELLA FUNZIONI ATTIVE
164600970526     C     1             DO        24            I
164700970526     C                   MOVEL     FUD(I)        FUDRIG
164800970526     C*
164900970526     C* PULIZIA FUNZIONI PRECEDENTI
165000970526     C                   MOVEL     *BLANKS       FUV(I)
165100970526     C*
165200970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
165300970526     C* (SALTA F23 E F24)
165400970526     C     FUDATT        IFEQ      'S'
165500970526     C     FUDFUN        ANDNE     '23'
165600970526     C     FUDFUN        ANDNE     '24'
165700970526     C                   ADD       1             W
165800970526     C                   MOVEL     FUDDES        FUV(W)
165900970526     C                   END
166000970526     C                   END                                                    DO WHILE
166100970526     C*
166200970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
166300970526     C* ALTRIMENTI DISATTIVO F24
166400970526     C     1             DO        24            I
166500970526     C                   MOVEL     FUD(I)        FUDRIG
166600970526     C*
166700970526     C     FUDFUN        IFEQ      '24'
166800970526     C     W             IFGT      6
166900970526     C                   MOVEL     'S'           FUDATT
167000970526     C                   ELSE
167100970526     C                   MOVEL     *BLANKS       FUDATT
167200970526     C                   END                                                    W GT 8
167300970526     C                   MOVEL     FUDRIG        FUD(I)
167400970526     C                   END
167500970526     C                   END                                                    DO WHILE
167600970526     C*
167700970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
167800970526     C     W             DIV       6             I
167900970526     C                   MVR                     Z
168000970526     C*
168100970526     C     Z             IFNE      0
168200970526     C     I             ADD       1             Z
168300970526     C                   ELSE
168400970526     C                   Z-ADD     I             Z
168500970526     C                   END
168600970526     C*
168700970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
168800970526     C                   ADD       1             FUNGRU
168900970526     C     FUNGRU        IFGT      Z
169000970526     C     FUNGRU        ORLT      1
169100970526     C                   Z-ADD     1             FUNGRU
169200970526     C                   END
169300970526     C*
169400970526     C* PRIMO GRUPPO DI 6 FUNZIONI
169500970526     C     FUNGRU        IFEQ      1
169600970526     C                   MOVEL     FUV(1)        FUN001
169700970526     C                   MOVEL     FUV(2)        FUN002
169800970526     C                   MOVEL     FUV(3)        FUN003
169900970526     C                   MOVEL     FUV(4)        FUN004
170000970526     C                   MOVEL     FUV(5)        FUN005
170100970526     C                   MOVEL     FUV(6)        FUN006
170200970526     C                   END
170300970526     C*
170400970526     C* SECONDO GRUPPO DI 6 FUNZIONI
170500970526     C     FUNGRU        IFEQ      2
170600970526     C                   MOVEL     FUV(7)        FUN001
170700970526     C                   MOVEL     FUV(8)        FUN002
170800970526     C                   MOVEL     FUV(9)        FUN003
170900970526     C                   MOVEL     FUV(10)       FUN004
171000970526     C                   MOVEL     FUV(11)       FUN005
171100970526     C                   MOVEL     FUV(12)       FUN006
171200970526     C                   END
171300970526     C*
171400970526     C* TERZO GRUPPO DI 6 FUNZIONI
171500970526     C     FUNGRU        IFEQ      3
171600970526     C                   MOVEL     FUV(13)       FUN001
171700970526     C                   MOVEL     FUV(14)       FUN002
171800970526     C                   MOVEL     FUV(15)       FUN003
171900970526     C                   MOVEL     FUV(16)       FUN004
172000970526     C                   MOVEL     FUV(17)       FUN005
172100970526     C                   MOVEL     FUV(18)       FUN006
172200970526     C                   END
172300970526     C*
172400970526     C* QUARTO GRUPPO DI 6 FUNZIONI
172500970526     C     FUNGRU        IFEQ      4
172600970526     C                   MOVEL     FUV(19)       FUN001
172700970526     C                   MOVEL     FUV(20)       FUN002
172800970526     C                   MOVEL     FUV(21)       FUN003
172900970526     C                   MOVEL     FUV(22)       FUN004
173000970526     C                   MOVEL     *BLANKS       FUN005
173100970526     C                   MOVEL     *BLANKS       FUN006
173200970526     C                   END
173300970526     C*
173400970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
173500970526     C                   MOVEL     *BLANKS       FUN007
173600970526     C                   MOVEL     *BLANKS       FUN008
173700970526     C*
173800970526     C     1             DO        24            I
173900970526     C                   MOVEL     FUD(I)        FUDRIG
174000970526     C     FUDFUN        IFEQ      '23'
174100970526     C     FUDATT        ANDEQ     'S'
174200970526     C                   MOVEL     FUDDES        FUN007
174300970526     C                   END
174400970526     C     FUDFUN        IFEQ      '24'
174500970526     C     FUDATT        ANDEQ     'S'
174600970526     C                   MOVEL     FUDDES        FUN008
174700970526     C                   END
174800970526     C                   END                                                    DO
174900970526     C*
175000970526     C                   ENDSR
175100970526     C*------------------------------------------------------------------------*
175200970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
175300970526     C*------------------------------------------------------------------------*
175400970526     C     OPZGES        BEGSR
175500970526     C*
175600970526     C* PULISCO CAMPI DI LAVORO
175700970526     C                   Z-ADD     0             W                 5 0
175800970526     C*
175900970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
176000970526     C* E CARICO TABELLA OPZIONI ATTIVE
176100970526     C     1             DO        24            I
176200970526     C                   MOVEL     OPD(I)        OPDRIG
176300970526     C*
176400970526     C* PULIZIA OPZIONI PRECEDENTI
176500970526     C                   MOVEL     *BLANKS       OPV(I)
176600970526     C*
176700970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
176800970526     C     OPDATT        IFEQ      'S'
176900970526     C                   ADD       1             W
177000970526     C                   MOVEL     OPDDES        OPV(W)
177100970526     C                   END
177200970526     C                   END                                                    DO WHILE
177300970526     C*
177400970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
177500970526     C* ALTRIMENTI DISATTIVO F23
177600970526     C     1             DO        24            I
177700970526     C                   MOVEL     FUD(I)        FUDRIG
177800970526     C*
177900970526     C     FUDFUN        IFEQ      '23'
178000970526     C     W             IFGT      8
178100970526     C                   MOVEL     'S'           FUDATT
178200970526     C                   ELSE
178300970526     C                   MOVEL     *BLANKS       FUDATT
178400970526     C                   END                                                    W GT 8
178500970526     C                   MOVEL     FUDRIG        FUD(I)
178600970526     C                   END
178700970526     C                   END                                                    DO WHILE
178800970526     C*
178900970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
179000970526     C     W             DIV       8             I
179100970526     C                   MVR                     Z                 5 0
179200970526     C*
179300970526     C     Z             IFNE      0
179400970526     C     I             ADD       1             Z
179500970526     C                   ELSE
179600970526     C                   Z-ADD     I             Z
179700970526     C                   END
179800970526     C*
179900970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
180000970526     C                   ADD       1             OPZGRU
180100970526     C     OPZGRU        IFGT      Z
180200970526     C     OPZGRU        ORLT      1
180300970526     C                   Z-ADD     1             OPZGRU
180400970526     C                   END
180500970526     C*
180600970526     C* PRIMO GRUPPO DI 8 OPZIONI
180700970526     C     OPZGRU        IFEQ      1
180800970526     C                   MOVEL     OPV(1)        OPZ001
180900970526     C                   MOVEL     OPV(2)        OPZ002
181000970526     C                   MOVEL     OPV(3)        OPZ003
181100970526     C                   MOVEL     OPV(4)        OPZ004
181200970526     C                   MOVEL     OPV(5)        OPZ005
181300970526     C                   MOVEL     OPV(6)        OPZ006
181400970526     C                   MOVEL     OPV(7)        OPZ007
181500970526     C                   MOVEL     OPV(8)        OPZ008
181600970526     C                   END
181700970526     C*
181800970526     C* SECONDO GRUPPO DI 8 OPZIONI
181900970526     C     OPZGRU        IFEQ      2
182000970526     C                   MOVEL     OPV(9)        OPZ001
182100970526     C                   MOVEL     OPV(10)       OPZ002
182200970526     C                   MOVEL     OPV(11)       OPZ003
182300970526     C                   MOVEL     OPV(12)       OPZ004
182400970526     C                   MOVEL     OPV(13)       OPZ005
182500970526     C                   MOVEL     OPV(14)       OPZ006
182600970526     C                   MOVEL     OPV(15)       OPZ007
182700970526     C                   MOVEL     OPV(16)       OPZ008
182800970526     C                   END
182900970526     C*
183000970526     C* TERZO GRUPPO DI 8 OPZIONI
183100970526     C     OPZGRU        IFEQ      3
183200970526     C                   MOVEL     OPV(17)       OPZ001
183300970526     C                   MOVEL     OPV(18)       OPZ002
183400970526     C                   MOVEL     OPV(19)       OPZ003
183500970526     C                   MOVEL     OPV(20)       OPZ004
183600970526     C                   MOVEL     OPV(21)       OPZ005
183700970526     C                   MOVEL     OPV(22)       OPZ006
183800970526     C                   MOVEL     OPV(23)       OPZ007
183900970526     C                   MOVEL     OPV(24)       OPZ008
184000970526     C                   END
184100970526     C*
184200970526     C                   ENDSR
184300970526     C*------------------------------------------------------------------------*
184400970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
184500970526     C*------------------------------------------------------------------------*
184600970526     C     OPZCON        BEGSR
184700970526     C*
184800970526     C* PULISCO CAMPI DI LAVORO
184900970526     C                   Z-ADD     0             I                 5 0
185000970526     C                   Z-ADD     0             W                 5 0
185100970526     C*
185200970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
185300970526     C     1             DO        24            I
185400970526     C                   MOVEL     OPD(I)        OPDRIG
185500970526     C*
185600970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
185700970526     C     OPZKEY        IFEQ      OPDOPZ
185800970526     C     OPZKEY        OREQ      OPDOP1
185900970526     C*
186000970526     C     OPDATT        IFEQ      'S'
186100970526     C                   MOVEL     OPDOPZ        OPZKEY
186200970526     C                   Z-ADD     I             W
186300970526     C                   END
186400970526     C                   END
186500970526     C*
186600970526     C                   END                                                    DO WHILE
186700970526     C*
186800970526     C* OPZIONE NON ATTIVA (ERRORE)
186900970526     C     W             IFEQ      0
187000970526     C*
187100970526     C* OPZIONE NON OK
187200970526     C                   Z-ADD     1             OPZOK
187300970526     C*
187400970526     C* EMISSIONE MESSAGGIO DI ERRORE
187500970526     C                   MOVEL     MSG(1)        DSMSMS
187600970526     C                   ELSE
187700970526     C*
187800970526     C* OPZIONE ATTIVA (PROCEDI ...)
187900970526     C*
188000970526     C     OPZKEY        IFEQ      ' 1'
188100970526     C                   EXSR      O01GES
188200970526     C                   END
188300970526     C     OPZKEY        IFEQ      ' 2'
188400970526     C                   EXSR      O02GES
188500970526     C                   END
188600970603     C     OPZKEY        IFEQ      ' 3'
188700970603     C                   EXSR      O03GES
188800970603     C                   END
188900970526     C     OPZKEY        IFEQ      ' 4'
189000970526     C                   EXSR      O04GES
189100970526     C                   END
189200970526     C     OPZKEY        IFEQ      ' 5'
189300970526     C                   EXSR      O05GES
189400970526     C                   END
189500970526     C     OPZKEY        IFEQ      ' 7'
189600970526     C                   END
189700970528     C     OPZKEY        IFEQ      ' 8'
189800970528     C                   END
189900970528     C     OPZKEY        IFEQ      ' 9'
190000970528     C                   END
190100970528     C     OPZKEY        IFEQ      '10'
190200970528     C                   END
190300970528     C     OPZKEY        IFEQ      '11'
190400970528     C                   END
190500970526     C                   END                                                    OPZ.NON ATTIVA
190600970526     C*
190700970526     C                   ENDSR
190800160831
190900160831       /free
191000160831
191100160831       //--------------------------------------------------------------
191200160831       //?Preparazione stringa SQL                                     ?
191300160831       //--------------------------------------------------------------
191400160831       BEGSR  Prepara_SQL;
191500160831
191600160901         // Chiudo il cursore (lo faccio in anticipo perché non saprei quando chiuderlo
191700160901         exec sql  close C1;
191800160901
191900161130         select;
192000161130
192100161130          // liv.3 Giorno-Fascia oraria-Server
192200161130          when wLivDrill = 3;
192300161130           wSQL = 'select macdtchk, mactmchk, macserver, maccnt1, macdif1, +
192400161130                   macsto1 +
192500161122                   from timac00f';
192600170117           // costruisce la where
192700170117           exsr CreateWhere;
192800161223           //if wCompara = *on;
192900161223           //  wSQL = %trim(wSQL) +
193000161223           //       ' where macdtchk = current_date';
193100161223           //endif;
193200161111           wSQL = %trim(wSQL) +
193300161201                  ' order by macdtchk desc , mactmchk desc, macserver';
193400161130
193500161130          // liv.2 Giorno-Fascia oraria
193600161130          when wLivDrill = 2;
193700161130           wSQL = 'select macdtchk, mactmchk, +
193800161130                   sum(maccnt1), avg(macdif1), max(macsto1) +
193900161130                   from timac00f';
194000170118           // costruisce la where
194100170118           exsr CreateWhere;
194200161223           //if wCompara = *on;
194300161223           //  wSQL = %trim(wSQL) +
194400161223           //       ' where macdtchk = current_date';
194500161223           //endif;
194600161130           wSQL = %trim(wSQL) +
194700161130                  ' group by macdtchk , mactmchk';
194800161130           wSQL = %trim(wSQL) +
194900161130                  ' order by macdtchk desc , mactmchk desc';
195000161115
195100170118          // liv.1 Giorno
195200161130          // e leggo tutti i giorni
195300161130          when wLivDrill = 1;
195400161111           wSQL = 'select macdtchk, sum(maccnt1), avg(macdif1) +
195500161111                   from timac00f';
195600170118           // costruisce la where
195700170118           exsr CreateWhere;
195800161111           wSQL = %trim(wSQL) +
195900161111                  ' group by macdtchk';
196000161111           wSQL = %trim(wSQL) +
196100161111                  ' order by macdtchk desc';
196200161130         endsl;
196300160831
196400160831         // Dichiarazione cursore
196500160831         exec sql   prepare S1   from :wSQL;
196600160831         exec sql   declare C1  asensitive   cursor for S1;
196700160831
196800160831         // Apertura del cursore
196900160831         exec sql   open C1;
197000160831
197100160901         *in99 = *off;
197200160901         if SQLCode < 0;
197300160901           *in99 = *on;
197400160831         endif;
197500160831
197600160831       ENDSR;
197700160831
197800160831       //--------------------------------------------------------------
197900170117       //?Costruisce la where a seconda delle selezioni                ?
198000160831       //--------------------------------------------------------------
198100170117       BEGSR  CreateWhere;
198200160831
198300170117         // se è stato indicato almeno una selezione, allora avrò una where
198400170117         if D03dtd <> 0
198500170117         or D03dta <> 0
198600170117         or D03hhd <> 0
198700170117         or D03hha <> 0
198800170117         or D03serverd <> *blank
198900170117         or D03servera <> *blank;
199000170117           wSQL = %trim(wSQL) +
199100170117                  ' where';
199200170117           if D03dtd <> 0
199300170117           or D03dta <> 0;
199400170117           wSQL = %trim(wSQL) +
199500170118                  ' MACDTCHK between ''' +
199600170117                  %char(DTDISO) +
199700170117                  ''' and ''' +
199800170118                  %char(DTAISO) +
199900170118                  '''';
200000170117           endif;
200100170118           if D03hhd <> 0
200200170118           or D03hha <> 0;
200300170118             wD03hhd = D03hhd;
200400170118             wD03hha = D03hha;
200500170118             if D03dtd <> 0
200600170118             or D03dta <> 0;
200700170118               wSQL = %trim(wSQL) +
200800170118               ' and';
200900170118             endif;
201000170118             // siccome la fascia 0 esiste ma non posso richiederla con lo 0, uso il 24
201100170118             if D03hhd = 24;
201200170118               wD03hhd = 0;
201300170118             endif;
201400170118             if D03hha = 24;
201500170118               wD03hha = 0;
201600170118             endif;
201700170118           wSQL = %trim(wSQL) +
201800170118                  ' hour(MACTMCHK) between ' +
201900170118                  %char(wD03HhD) +
202000170118                  ' and ' +
202100170118                  %char(wD03HhA) +
202200170118                  '';
202300170118           endif;
202400170118           if D03serverd <> *blank
202500170118           or D03servera <> *blank;
202600170118             if D03dtd <> 0
202700170118             or D03dta <> 0
202800170118             or D03hhd <> 0
202900170118             or D03hha <> 0;
203000170118               wSQL = %trim(wSQL) +
203100170118               ' and';
203200170118             endif;
203300170118           wSQL = %trim(wSQL) +
203400170118                  ' MACSERVER between ''' +
203500170118                  D03ServerD +
203600170118                  ''' and ''' +
203700170118                  D03ServerA +
203800170118                  '''';
203900170118           endif;
204000170117         endif;
204100170117
204200170117       ENDSR;
204300170117
204400170117       //--------------------------------------------------------------
204500170117       //?Lettura occorrenza                                           ?
204600170117       //--------------------------------------------------------------
204700170117       BEGSR  Legge_SQL;
204800170117
204900161130         select;
205000161130
205100161130          // liv.3 Giorno-Fascia oraria-Server
205200161130          when wLivDrill = 3;
205300161130           exec sql  fetch next  from C1  into :TIMAC_fetch3;
205400161130          // liv.2 Giorno-Fascia oraria
205500161130          when wLivDrill = 2;
205600161111           exec sql  fetch next  from C1  into :TIMAC_fetch2;
205700161130          // liv.1 Giorno
205800161130          when wLivDrill = 1;
205900161130           exec sql  fetch next  from C1  into :TIMAC_fetch1;
206000161130         endsl;
206100160901
206200160901         *in99 = *off;
206300160901         if SQLCode = 100 or SQLCode < 0;
206400160901           *in99 = *on;
206500160901         endif;
206600160831
206700160831       ENDSR;
206800160831
206900160831       /end-free
207000160831
207100970526     C*------------------------------------------------------------------------*
207200970526     C* NOTE COSTRUZIONE SCHIERE
207300970526     C*------------------------------------------------------------------------*
207400970526     C*
207500970526     C* -MSG-MESSAGGI
207600970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
207700970526     C* (NON TOCCARE MSG,1 E MSG,2)
207800970526     C*
207900970526     C* -CMD-COMANDI
208000970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
208100970526     C*
208200970526     C* -OPD-OPZIONI
208300970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
208400970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
208500970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
208600970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
208700970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
208800970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
208900970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
209000970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
209100970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
209200970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
209300970526     C*
209400970526     C* -FUD-FUNZIONI
209500970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
209600970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
209700970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
209800970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
209900970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
210000970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
210100970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
210200970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
210300970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
210400970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
210500970526     O*------------------------------------------------------------------------*
210600970526** DEC - DECODIFICHE
210700970526    GESTIONE                                                     1
210800970526INTERROGAZIONE                                                   2
210900970530     SCELTA                                                      3
2110009705264
2111009705265
211200970526** MSG - MESSAGGI
211300161108TRBMMAC1R- Opzione non gestita                                                 1
211400161108TRBMMAC1R- Tasto funzionale non gestito                                        2
211500161108TRBMMAC1R-
211600161108TRBMMAC1R- Record annullato.                                                   4
211700161108TRBMMAC1R- Record non annullato. Impossibile ripristinare                      5
211800161108TRBMMAC1R- Accettato solo un parametro di inizio lista                         6
211900161108TRBMMAC1R- Errori nei parametri di ingresso                                    7
212000161108TRBMMAC1R- Data errata                                                         8
212100170118TRBMMAC1R- Valore DA > valore AL                                               9
212200161108TRBMMAC1R- Errore SQL nel reperimento dati Validi al                          10
212300161108TRBMMAC1R- Copia totale (opz. 23) eseguita correttamente                      11
212400161108TRBMMAC1R- Copia totale (opz. 23) non eseguita                                12
212500161130TRBMMAC1R- Non esiste un livello superiore per il DrillUp                     13
212600161130TRBMMAC1R- Non esiste un livello inferiore per il DrillDown                   14
212700161108TRBMMAC1R- 15                                                                 15
212800161108TRBMMAC1R- 16                                                                 16
212900161108TRBMMAC1R- 17                                                                 17
213000161108TRBMMAC1R- 18                                                                 18
213100161108TRBMMAC1R- 19                                                                 19
213200161108TRBMMAC1R- 20                                                                 20
213300970526** CMD - COMANDI CL
213400161108
213500970526
213600970526
213700970526
213800970526
213900970526** OPD - OPZIONI
214000160923 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
214100970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
214200160914 3=Copia            3 -     (S/N)=N                        opz. 03¬
214300970526 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
214400970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
214500160713                   -  -     (S/N)=N                        opz. 06¬
214600160713                   -  -     (S/N)=N                        opz. 07¬
214700160713                   -  -     (S/N)=N                        opz. 08¬
214800160713                   -  -     (S/N)=N                        opz. 09¬
214900160713                   -  -     (S/N)=N                        opz. 10¬
215000160713                   -  -     (S/N)=N                        opz. 11¬
215100970526                   -  -     (S/N)=N                        opz. 12¬
215200161108                   -  -     (S/N)=N                        opz. 13¬
215300970526                   -  -     (S/N)=N                        opz. 14¬
215400970526                   -  -     (S/N)=N                        opz. 15¬
215500970526                   -  -     (S/N)=N                        opz. 16¬
215600970526                   -  -     (S/N)=N                        opz. 17¬
215700970526                   -  -     (S/N)=N                        opz. 18¬
215800970526                   -  -     (S/N)=N                        opz. 19¬
215900970526                   -  -     (S/N)=N                        opz. 20¬
216000970526                   -  -     (S/N)=N                        opz. 21¬
216100970526                   -  -     (S/N)=N                        opz. 22¬
216200161108                   -  -     (S/N)=N                        opz. 23¬
216300970526                   -  -     (S/N)=N                        opz. 24¬
216400970526** FUD - FUNZIONI
216500970526                   -        (S/N)=N                        funz.01¬
216600970526                   -        (S/N)=N                        funz.02¬
216700970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
216800970526                   -        (S/N)=N                        funz.04¬
216900970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
217000970526F06=Inserimento    -        (S/N)=N                        funz.06¬
217100161130F07=Drill-down     -        (S/N)=S                        funz.07¬
217200161111F08=Drill-up       -        (S/N)=S                        funz.08¬
217300161115F09=Comparazione   -        (S/N)=S                        funz.09¬
217400160713                   -        (S/N)=N                        funz.10¬
217500160713                   -        (S/N)=N                        funz.11¬
217600160923F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
217700160713                   -        (S/N)=N                        funz.13¬
217800970603                   -        (S/N)=S                        funz.14¬
217900970526                   -        (S/N)=N                        funz.15¬
218000970526                   -        (S/N)=N                        funz.16¬
218100970526                   -        (S/N)=N                        funz.17¬
218200970526                   -        (S/N)=N                        funz.18¬
218300970627 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
218400970627 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
218500970526                   -        (S/N)=N                        funz.21¬
218600160713                   -        (S/N)=S                        funz.22¬
218700970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
218800970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
