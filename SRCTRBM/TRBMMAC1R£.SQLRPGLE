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
010700161130     d wLivDrill       s              1s 0 inz(2)
010800161116     d wCompara        s               n   inz(*off)
010900161116
011000161115     d wDtChk          s               d
011100161115     d wDatePrevious   s               d
011200161115     d wTimePrevious   s               t
011300161115     d wDC_Day_NaI     s              9a
011400161117     d wCntOggi        s              7s 0
011500161117     d wCntPrevious    s              7s 0
011600161115     d wTrovato        s               n   inz(*off)
011700161115     d wNoncene        s               n   inz(*off)
011800161111
011900160831       // - Stringa SQL da eseguire
012000160831     d wSQl            s           1024    inz  varying
012100161130     d TIMAC_fetch3    ds                  qualified
012200161111     d  DtChk                          d
012300161111     d  TmChk                          t
012400161130     d  Server                       10a
012500161111     d  Cnt1                          6s 0
012600161111     d  Dif1                         12s 3
012700161111     d  Sto1                          1
012800161130     d TIMAC_fetch2    ds                  qualified
012900161130     d  DtChk                          d
013000161130     d  TmChk                          t
013100161130     d  Cnt1                          6s 0
013200161130     d  Dif1                         12s 3
013300161130     d  Sto1                          1
013400161130     d TIMAC_fetch1    ds                  qualified
013500161111     d  DtChk                          d
013600161111     d  Cnt1                          6s 0
013700161111     d  Dif1                         12s 3
013800160916
013900161110     D*-------------------
014000161110     D* parametri UBMACHKR
014100161110     D*-------------------
014200161110       // Periodo DA
014300161110     d MACHKTSDa       s               z
014400161110       // Periodo AL
014500161110     d MACHKTSAl       s               z
014600161110       // Esito
014700161110     d MACHKEsito      s              1a
014800161110
014900161110       /copy gaitrasrc/srcProtoPR,UBMACHK1R
015000161110
015100970526     C*------------------------------------------------------------------------*
015200970526     C* MAIN LINES
015300970526     C*------------------------------------------------------------------------*
015400970526     C*
015500970526     C* SE NON CI SONO ERRORI, PROSEGUE
015600970526IF  1C     ERRL00        IFNE      '1'
015700970526     C*
015800970526     C* CARICA IL SFILE
015900970526     C                   EXSR      POSSFL
016000970526     C*
016100970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
016200970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
016300970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
016400970526     C     FINL00        OREQ      '1'                                          *FINE PGM
016500970526     C*
016600970526     C* REIMPOSTA ERRORE DI PROCERURA
016700970526     C                   MOVEL     '0'           ERRL00
016800970526     C*
016900970526     C* EMETTE IL SFILE
017000970526     C                   EXSR      OUTSFL
017100970526     C*
017200970526     C* GESTIONE TASTI FUNZIONALI
017300970526     C                   EXSR      FUNCON
017400970526     C*
017500970526E   2C                   ENDDO
017600970526E   1C                   ENDIF
017700970526     C* FINE PGM
017800970526     C                   SETON                                        LR
017900970526     C*------------------------------------------------------------------------*
018000970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
018100970526     C*------------------------------------------------------------------------*
018200970526     C     F03GES        BEGSR
018300970526     C*
018400970526     C* IMPOSTA "PREMUTO F03"
018500970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
018600161108     C                   MOVEL     '1'           MAC1F03                         *DI PROCEDURA
018700160916     C*
018800970526     C                   ENDSR
018900970526     C*------------------------------------------------------------------------*
019000970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
019100970526     C*------------------------------------------------------------------------*
019200970526     C     F05GES        BEGSR
019300160923     C*
019400161110
019500161110       // aggiorna TIMAC00F prima di caricare il SFL
019600161110       exsr sr_AggTIMAC;
019700161110
019800970526     C*
019900970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
020000970526     C*
020100970526     C                   ENDSR
020200970526     C*------------------------------------------------------------------------*
020300970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
020400970526     C*------------------------------------------------------------------------*
020500970526     C     F06GES        BEGSR
020600970526     C*
020700970526     C* INSERIMENTO CONSENTITO SOLO IN SEDE
020800020919IF  1C     DUTLPO        IFNE      'S'
020900970526     C                   MOVEL     '1'           ERRL00
021000970526     C                   MOVEL     MSG(3)        DSMSMS
021100970526E   1C                   ENDIF
021200970526     C*
021300970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
021400161108     C***                CLEAR                   DPROC
021500970529     C                   MOVEL     DEPOP0        WOP0
021600970529     C                   MOVEL     'F06'         WOP1
021700161108     C                   MOVEL     'TRBMMAC2R'   WPGM             10
021800970526     C                   EXSR      LANPGM                                       *LANCIA PGM
021900970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
022000970526E   1C                   ENDIF
022100970526     C*
022200970526     C                   ENDSR
022300161130     C*------------------------------------------------------------------------*
022400161201     C* F08GES - GESTIONE F08 -> Drill-up
022500161130     C*------------------------------------------------------------------------*
022600161201     C     F08GES        BEGSR
022700161130     C*
022800161201     C* a oggi 30/11/16 i livelli di Drill sono 3, per cui se siamo già al 1 non posso salire più
022900161201     C* (si va in ordine inverso)
023000161130     C                   if        wLivDrill= 1
023100161130     C                   MOVEL     '1'           ERRL00
023200161201     C                   MOVEL     MSG(13)       DSMSMS
023300161130     C                   LEAVESR
023400161130     C                   endif
023500161130
023600161130     C* diminuisce il livello di Drill
023700161130     C                   eval      wLivDrill -= 1
023800161130     C* disattivo la comparazione (se c'è)
023900161130     C                   eval      wCompara = *off
024000161130     C                   eval      *in03 = wCompara
024100161130     C* cambio il tasto di funzione F09 in Comparazione
024200161130     c                   eval      FUDRig = FUD(9)
024300161130     C                   MOVEL     'S'           FUDATT
024400161130     c                   eval      %subst(FUDRig:1:19) = 'F09=Comparazione'
024500161130     c                   eval      FUD(9) = FUDRig
024600161130     C*
024700161130     c                   eval      FUNGru = 0
024800161130     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
024900161130     C*
025000161130     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
025100161130     C                   EXSR      NEWPOS
025200161130     C*
025300161130     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
025400161130IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
025500161130IF  2C     SWPOSI        IFNE      0
025600161130     C                   EXSR      POSSFL                                       *RICARICA SFILE
025700161130X   2C                   ELSE
025800161130IF  3C     SFLULT        IFNE      0
025900161130     C                   EXSR      ENTATT                                       *GESTIONE ENTER
026000161130     C*
026100161130     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
026200161130IF  4C     SWMODI        IFNE      0
026300161130     C                   EXSR      POSSFL
026400161130E   4C                   ENDIF
026500161130E   3C                   ENDIF
026600161130E   2C                   ENDIF
026700161130E   1C                   ENDIF
026800161130     C*
026900161130     C                   ENDSR
027000161111     C*------------------------------------------------------------------------*
027100161201     C* F07GES - GESTIONE F07 -> Drill-down
027200161111     C*------------------------------------------------------------------------*
027300161201     C     F07GES        BEGSR
027400161111     C*
027500161201     C* a oggi 30/11/16 i livelli di Drill sono 3, per cui se siamo già al 3 non posso scendere più
027600161201     C* (si va in ordine inverso)
027700161130     C                   if        wLivDrill= 3
027800161130     C                   MOVEL     '1'           ERRL00
027900161201     C                   MOVEL     MSG(14)       DSMSMS
028000161130     C                   LEAVESR
028100161130     C                   endif
028200161130
028300161130     C* aumento il livello di Drill
028400161130     C                   eval      wLivDrill += 1
028500161115     C* disattivo la comparazione (se c'è)
028600161115     C                   eval      wCompara = *off
028700161116     C                   eval      *in03 = wCompara
028800161115     C* cambio il tasto di funzione F09 in Comparazione
028900161115     c                   eval      FUDRig = FUD(9)
029000161115     C                   MOVEL     'S'           FUDATT
029100161115     c                   eval      %subst(FUDRig:1:19) = 'F09=Comparazione'
029200161115     c                   eval      FUD(9) = FUDRig
029300161111     C*
029400161111     c                   eval      FUNGru = 0
029500161111     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
029600161111     C*
029700161111     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
029800161111     C                   EXSR      NEWPOS
029900161111     C*
030000161111     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
030100161111IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030200161111IF  2C     SWPOSI        IFNE      0
030300161111     C                   EXSR      POSSFL                                       *RICARICA SFILE
030400161111X   2C                   ELSE
030500161111IF  3C     SFLULT        IFNE      0
030600161111     C                   EXSR      ENTATT                                       *GESTIONE ENTER
030700161111     C*
030800161111     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
030900161111IF  4C     SWMODI        IFNE      0
031000161111     C                   EXSR      POSSFL
031100161111E   4C                   ENDIF
031200161111E   3C                   ENDIF
031300161111E   2C                   ENDIF
031400161111E   1C                   ENDIF
031500161111     C*
031600161111     C                   ENDSR
031700161115     C*------------------------------------------------------------------------*
031800161123     C* F09GES - GESTIONE F09 -> Comparazione/Senza comparazione
031900161115     C*------------------------------------------------------------------------*
032000161115     C     F09GES        BEGSR
032100161115     C*
032200161115     C                   if        wCompara = *off
032300161115     C* attivo la Comparazione
032400161115     C                   eval      wCompara = *on
032500161116     C                   eval      *in03 = wCompara
032600161123     C* cambio il tasto di funzione F09 in Senza comparazione
032700161115     c                   eval      FUDRig = FUD(9)
032800161115     C                   MOVEL     'S'           FUDATT
032900161123     c                   eval      %subst(FUDRig:1:19) = 'F09=Senza comp.'
033000161115     c                   eval      FUD(9) = FUDRig
033100161115     c                   else
033200161115     C* disattivo la comparazione
033300161115     C                   eval      wCompara = *off
033400161116     C                   eval      *in03 = wCompara
033500161115     C* cambio il tasto di funzione F09 in Comparazione
033600161115     c                   eval      FUDRig = FUD(9)
033700161115     C                   MOVEL     'S'           FUDATT
033800161115     c                   eval      %subst(FUDRig:1:19) = 'F09=Comparazione'
033900161115     c                   eval      FUD(9) = FUDRig
034000161115     c                   endif
034100161115     C*
034200161115     c                   eval      FUNGru = 0
034300161115     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
034400161115     C*
034500161115     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
034600161115     C                   EXSR      NEWPOS
034700161115     C*
034800161115     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
034900161115IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
035000161115IF  2C     SWPOSI        IFNE      0
035100161115     C                   EXSR      POSSFL                                       *RICARICA SFILE
035200161115X   2C                   ELSE
035300161115IF  3C     SFLULT        IFNE      0
035400161115     C                   EXSR      ENTATT                                       *GESTIONE ENTER
035500161115     C*
035600161115     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
035700161115IF  4C     SWMODI        IFNE      0
035800161115     C                   EXSR      POSSFL
035900161115E   4C                   ENDIF
036000161115E   3C                   ENDIF
036100161115E   2C                   ENDIF
036200161115E   1C                   ENDIF
036300161115     C*
036400161115     C                   ENDSR
036500970526     C*------------------------------------------------------------------------*
036600970526     C* F12GES - GESTIONE F12 -> RITORNO
036700970526     C*------------------------------------------------------------------------*
036800970526     C     F12GES        BEGSR
036900970526     C*
037000970526     C* IMPOSTA "PREMUTO F12"
037100970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
037200161108     C                   MOVEL     '1'           MAC1F12                         *DI PROCEDURA
037300970526     C*
037400970526     C                   ENDSR
037500970526     C*------------------------------------------------------------------------*
037600970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
037700970526     C*------------------------------------------------------------------------*
037800970526     C     F23GES        BEGSR
037900970526     C*
038000970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
038100970526     C*
038200970526     C                   ENDSR
038300970526     C*------------------------------------------------------------------------*
038400970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
038500970526     C*------------------------------------------------------------------------*
038600970526     C     F24GES        BEGSR
038700970526     C*
038800970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
038900970526     C*
039000970526     C                   ENDSR
039100970526     C*------------------------------------------------------------------------*
039200970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
039300970526     C*------------------------------------------------------------------------*
039400970526     C     RUPGES        BEGSR
039500970526     C*
039600970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
039700970526     C*
039800970526     C                   ENDSR
039900970526     C*------------------------------------------------------------------------*
040000970526     C* ENTGES - GESTIONE ENTER -> INVIO
040100970526     C*------------------------------------------------------------------------*
040200970526     C     ENTGES        BEGSR
040300970526     C*
040400970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
040500970526     C                   EXSR      NEWPOS
040600970526     C*
040700970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
040800970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
040900970526IF  2C     SWPOSI        IFNE      0
041000970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
041100970526X   2C                   ELSE
041200970526IF  3C     SFLULT        IFNE      0
041300970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
041400970526     C*
041500970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
041600970526IF  4C     SWMODI        IFNE      0
041700970526     C                   EXSR      POSSFL
041800970526E   4C                   ENDIF
041900970526E   3C                   ENDIF
042000970526E   2C                   ENDIF
042100970526E   1C                   ENDIF
042200970526     C*
042300970526     C                   ENDSR
042400970526     C*------------------------------------------------------------------------*
042500970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
042600970526     C*------------------------------------------------------------------------*
042700970526     C     O01GES        BEGSR
042800970526     C*
042900160923     C*
043000160923     C* simulo l'enter
043100160923     C                   exsr      NEWPOS
043200160923     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
043300160923     C*
043400970526     C                   ENDSR
043500970526     C*------------------------------------------------------------------------*
043600970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
043700970526     C*------------------------------------------------------------------------*
043800970526     C     O02GES        BEGSR
043900160927     C*
044000160927     C*
044100161108     C                   ENDSR
044200970526     C*------------------------------------------------------------------------*
044300161108     C* O03GES - GESTIONE OPZIONE 3=COPIA
044400970526     C*------------------------------------------------------------------------*
044500161108     C     O03GES        BEGSR
044600160927     C*
044700970526     C*
044800970526     C                   ENDSR
044900161108     C*------------------------------------------------------------------------*
045000161108     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
045100161108     C*------------------------------------------------------------------------*
045200161108     C     O04GES        BEGSR
045300161108     C*
045400161108     C*
045500161108     C                   ENDSR
045600970526     C*------------------------------------------------------------------------*
045700970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
045800970526     C*------------------------------------------------------------------------*
045900970526     C     O05GES        BEGSR
046000970526     C*
046100970526     C*
046200970526     C                   ENDSR
046300970526     C*------------------------------------------------------------------------*
046400970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
046500970526     C*------------------------------------------------------------------------*
046600970526     C     O07GES        BEGSR
046700970526     C*
046800970526     C*
046900970526     C                   ENDSR
047000970526     C*------------------------------------------------------------------------*
047100970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
047200970526     C*------------------------------------------------------------------------*
047300970526     C     NEWPOS        BEGSR
047400970526     C*
047500970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
047600970526     C*         1= RICHIESTO POSIZIONAMENTO
047700970526     C                   Z-ADD     0             SWPOSI            7 0
047800970526     C*
047900970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
048000970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
048100970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
048200970526     C                   Z-ADD     0             SWMODI            1 0
048300970526     C*
048400970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
048500161130     C     wLivDrill     IFNE      DEPLivDrill
048600161115     C     wCompara      ORNE      DEPCompara
048700970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
048800970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
048900970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
049000161130     C                   EVAL      DEPLivDrill = wLivDrill
049100161115     C                   EVAL      DEPCompara = wCompara
049200970526E   2C                   ENDIF
049300970526E   1C                   ENDIF
049400970526     C*
049500970526     C                   ENDSR
049600970526     C*------------------------------------------------------------------------*
049700970526     C* SETFIL - POSIZIONAMENTO RECORD
049800970526     C*------------------------------------------------------------------------*
049900970526     C     SETFIL        BEGSR
050000970526     C*
050100970526     C* POSIZIONAMENTO DEL FILE E PRIMA LETTURA IN BASE AL TIPO DI RICERCA
050200160830     C                   SELECT
050300160831     C*
050400161108IF  1C     WTIPRI        WHENEQ    *blank
050500160831     C* i dati li reperisce mediante SQL
050600160831     C                   EXSR      Prepara_SQL
050700160830     C*
050800160830E   1C                   ENDSL
050900970526     C* LETTURA RECORD
051000970526IF  1C     *IN99         IFEQ      *OFF
051100970526     C                   EXSR      LETFIL
051200970526E   1C                   ENDIF
051300970526     C*
051400970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
051500970526IF  1C     *IN99         IFEQ      *ON
051600970526     C                   Z-ADD     1             SFLEOF
051700970526E   1C                   ENDIF
051800970526     C*
051900970526     C                   ENDSR
052000970526     C*------------------------------------------------------------------------*
052100970526     C* LETFIL - LETTURA RECORD
052200970526     C*------------------------------------------------------------------------*
052300970526     C     LETFIL        BEGSR
052400970526     C*
052500970526     C* LEGGE FINO A:
052600970526     C*     - FINE FILE
052700970526     C*     - TROVATO RECORD VALIDO
052800970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
052900970526DO  1C     *IN99         DOWEQ     *OFF
053000970526     C     WRECOK        ANDEQ     'N'
053100970526     C*
053200970526     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO DI RICERCA
053300160830     C                   SELECT
053400160831     C*
053500161108IF  1C     WTIPRI        WHENEQ    *blank
053600160831     C* i dati li reperisce mediante SQL
053700160901     C                   EXSR      Legge_SQL
053800160830     C*
053900160830E   2C                   ENDSL
054000970526     C*
054100970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
054200970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
054300970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
054400970530X   2C                   ELSE
054500970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
054600970530E   2C                   ENDIF
054700970526E   1C                   ENDDO
054800970526     C*
054900970526     C                   ENDSR
055000970526     C*------------------------------------------------------------------------*
055100970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
055200970526     C*------------------------------------------------------------------------*
055300970526     C     CHKREC        BEGSR
055400970526     C*
055500970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
055600160913     C*
055700160919     C* Proc diversa
055800161108IF  1C*                  if        wDrillUp  =  *off and
055900161108IF  1C*                            D03Proc<> *blank and
056000161108IF  1C*                            D03Proc<> VMSProc
056100161108     C*                  MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
056200161108     C*                  leavesr
056300161108     C*                  endif
056400161108
056500160713
056600970526     C                   ENDSR
056700970526     C*------------------------------------------------------------------------*
056800160830     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE            §
056900970526     C*------------------------------------------------------------------------*
057000970526     C     IRESFL        BEGSR
057100970526     C*
057200970526     C                   MOVEL     *BLANKS       OPZKEY
057300161111
057400161130         select;
057500161130
057600161130          // liv.3 Giorno-Fascia oraria-Server
057700161130          when wLivDrill = 3;
057800161130           D05DtChk  = %char(TIMAC_fetch3.DtChk);
057900161130           D05TmChk  = %char(TIMAC_fetch3.TmChk);
058000161130           D05HHChk  = %subst(%char(TIMAC_fetch3.TmChk):1:2);
058100161130           D05Server = TIMAC_fetch3.Server;
058200161130           D05Cnt1   = TIMAC_fetch3.Cnt1;
058300161130           D05Dif1   = TIMAC_fetch3.Dif1;
058400161130           D05Sto1   = TIMAC_fetch3.Sto1;
058500161130           *in01 = *off;
058600161130           if TIMAC_fetch3.Sto1 <> 'S';
058700161130             *in01 = *on;
058800161130           endif;
058900161130           // se richiesta la comparazione, recupero il rcd stessa fascia ma del primo giorno
059000161130           // precedente che non è né sabato né domenica e che
059100161130           // il nr.totale di email inviate è almeno il 70% di quello di oggi
059200161130           // (per escludere i festivi)
059300161130           if wCompara = *on;
059400161130             exsr RepTIMACGgPrec;
059500161130           endif;
059600161130
059700161130          // liv.2 Giorno-Fascia oraria
059800161130          when wLivDrill = 2;
059900161130           D05DtChk  = %char(TIMAC_fetch2.DtChk);
060000161130           D05TmChk  = %char(TIMAC_fetch2.TmChk);
060100161130           D05HHChk  = %subst(%char(TIMAC_fetch2.TmChk):1:2);
060200161130           D05Server = *blank;
060300161130           D05Cnt1   = TIMAC_fetch2.Cnt1;
060400161130           D05Dif1   = TIMAC_fetch2.Dif1;
060500161130           D05Sto1   = TIMAC_fetch2.Sto1;
060600161111           *in01 = *off;
060700161130           if TIMAC_fetch2.Sto1 <> 'S';
060800161111             *in01 = *on;
060900161111           endif;
061000161115           // se richiesta la comparazione, recupero il rcd stessa fascia ma del primo giorno
061100161115           // precedente che non è né sabato né domenica e che
061200161116           // il nr.totale di email inviate è almeno il 70% di quello di oggi
061300161116           // (per escludere i festivi)
061400161115           if wCompara = *on;
061500161115             exsr RepTIMACGgPrec;
061600161115           endif;
061700161130           D05ServerP= *blank;
061800161115
061900161130          // liv.1 Giorno
062000161130          // non eseguibile la comparazione
062100161130          when wLivDrill = 1;
062200161130           D05DtChk  = %char(TIMAC_fetch1.DtChk);
062300161116           D05TmChk  = *blank;
062400161130           D05Server = *blank;
062500161116           D05HHChk  = *blank;
062600161130           D05Cnt1   = TIMAC_fetch1.Cnt1;
062700161130           D05Dif1   = TIMAC_fetch1.Dif1;
062800161111           *in01 = *off;
062900161111           D05Sto1 = *blank;
063000161115           *in02 = *off;
063100161115           D05Sto1P= *blank;
063200161115           D05DtChkP = *blank;
063300161115           D05HHChkP = *blank;
063400161130           D05ServerP= *blank;
063500161115           D05Cnt1P  = 0;
063600161115           D05Dif1P  = 0;
063700161130
063800161130         endsl;
063900160901     C*
064000970526     C                   ENDSR
064100161115       //----------------------------------------------------------------------*
064200161115       // Reperisco rcd TIMAC stesso orario ma giorno precedente, no sabato no domenica
064300161116       // e che il nr.totale di email inviate è almeno il 70% di quello di oggi
064400161116       // (per escludere i festivi)
064500161130       // La comparazione si fa solo per i liv.3 e 2
064600161115       //----------------------------------------------------------------------*
064700161130       begsr RepTIMACGgPrec;
064800161223
064900161223         clear d05dtchkp;
065000161223         clear d05hhchkp;
065100161223         clear d05serverp;
065200161223         clear d05cnt1p;
065300161223         clear d05dif1p;
065400161223         clear d05sto1p;
065500161115
065600161130         // ciclo finché non ho trovato il giorno precedente oppure è chiaro che
065700161130         // non ce ne è
065800161130         select;
065900161130         // liv.3 Giorno-Fascia oraria-Server
066000161130          when wLivDrill = 3;
066100161130           wDtChk = TIMAC_fetch3.DtChk;
066200161130         // liv.2 Giorno-Fascia oraria
066300161130          when wLivDrill = 2;
066400161130           wDtChk = TIMAC_fetch2.DtChk;
066500161130         endsl;
066600161130         wTrovato = *off;
066700161130         wNoncene = *off;
066800161130         dou wTrovato = *on or
066900161130             wNoncene = *on;
067000161130           exsr RepGgPrec;
067100161130         enddo;
067200161115
067300161130         // se ho trovato il giorno precedente giusto
067400161130         if wTrovato = *on;
067500161130           // leggo i dati
067600161130           select;
067700161130            // liv.3 Giorno-Fascia oraria-Server
067800161130            when wLivDrill = 3;
067900161130             exec sql
068000161130             select char(macdtchk), substr(char(mactmchk) , 1 , 2),
068100161130             macserver,
068200161130             maccnt1, macdif1, macsto1
068300161130             into
068400161130             :d05dtchkp, :d05hhchkp, :d05serverp, :d05cnt1p, :d05dif1p,
068500161130             :d05sto1p
068600161130             from timac00f
068700161130             where macdtchk = :wDatePrevious and
068800161130                   mactmchk = time(:d05tmchk) and
068900161130                   macserver= :d05server;
069000161130            // liv.2 Giorno-Fascia oraria
069100161130            when wLivDrill = 2;
069200161130             exec sql
069300161130             select char(macdtchk), substr(char(mactmchk) , 1 , 2),
069400161130             ' ',
069500161130             sum(maccnt1), avg(macdif1), max(macsto1)
069600161130             into
069700161130             :d05dtchkp, :d05hhchkp, :d05serverp, :d05cnt1p, :d05dif1p,
069800161130             :d05sto1p
069900161130             from timac00f
070000161130             where macdtchk = :wDatePrevious and
070100161130                   mactmchk = time(:d05tmchk)
070200161130             group by macdtchk, mactmchk;
070300161130           endsl;
070400161116
070500161116             // controllo il risultato
070600161116             select;
070700161116              when sqlcod < 0;
070800161116               // errore SQL
070900161116               clear d05dtchkp;
071000161116               clear d05hhchkp;
071100161201               clear d05serverp;
071200161116               clear d05cnt1p;
071300161116               clear d05dif1p;
071400161116               clear d05sto1p;
071500161201              // non c'è il rcd precedente (può capitare con l'aggiunta del server)
071600161116              when sqlcod = 100;
071700161201               clear d05dtchkp;
071800161201               clear d05hhchkp;
071900161201               clear d05serverp;
072000161201               clear d05cnt1p;
072100161201               clear d05dif1p;
072200161201               clear d05sto1p;
072300161116              other;
072400161116               // proseguo
072500161116             endsl;
072600161116
072700161116           endif;
072800161115
072900161115         endsr;
073000161115       //----------------------------------------------------------------------*
073100161115       // Reperisco il giorno precedente di una data passata, no sabato no domenica
073200161116       // e che il nr.totale di email inviate è almeno il 70% di quello di oggi
073300161116       // (per escludere i festivi)
073400161115       //----------------------------------------------------------------------*
073500161115         begsr RepGgPrec;
073600161115
073700161115         // controllo che giorno è oggi
073800161115         exec sql
073900161115         set :wDC_DAY_NAI = (select DC_DAY_NAI from DATECNV0F
074000161115                             where DC_Date = :wDtChk);
074100161115
074200161115         select;
074300161115          // se è lunedì, come precedente devo andare indietro di 3 gg (a venerdì)
074400161115          when wDC_DAY_NAI = 'Lunedì';
074500161115           wDatePrevious = wDtChk - %days(3);
074600161115          // se è domenica, come precedente devo andare indietro di 2 gg (a venerdì)
074700161117          when wDC_DAY_NAI = 'Domenica';
074800161115           wDatePrevious = wDtChk - %days(2);
074900161117          // se NON è né lunedì né domenica, vado indietro di 1 gg
075000161115          other;
075100161115           wDatePrevious = wDtChk - %days(1);
075200161115         endsl;
075300161115
075400161115         // controllo il totale differenza media del giorno definito
075500161115         // sia almeno il 70% di quella di oggi
075600161115         exec sql
075700161116         set :wCntOggi = (
075800161116                   select sum(maccnt1)
075900161115                   from timac00f
076000161115                   where macdtchk = :wDtChk
076100161115                   group by macdtchk);
076200161115
076300161115         // controllo il risultato
076400161115         select;
076500161115          when sqlcod < 0;
076600161115           // errore SQL
076700161115           wNoncene = *on;
076800161115           leavesr;
076900161115          when sqlcod = 100;
077000161115           // non c'è questo precedente per cui non ci sono rcd
077100161115           wNoncene = *on;
077200161115           leavesr;
077300161115          other;
077400161115           // proseguo
077500161115         endsl;
077600161115
077700161115         exec sql
077800161116         set :wCntPrevious = (
077900161116                   select sum(maccnt1)
078000161115                   from timac00f
078100161115                   where macdtchk = :wDatePrevious
078200161115                   group by macdtchk);
078300161115
078400161115         // controllo il risultato
078500161115         select;
078600161115          when sqlcod < 0;
078700161115           // errore SQL
078800161115           wNoncene = *on;
078900161115           leavesr;
079000161115          when sqlcod = 100;
079100161115           // non c'è questo precedente per cui non ci sono rcd
079200161115           wNoncene = *on;
079300161115           leavesr;
079400161115          other;
079500161115           // proseguo
079600161115         endsl;
079700161115
079800161116         if wCntPrevious >= wCntOggi * 70/100;
079900161115           // questa data va bene e leggo il rcd di TIMAC00F
080000161115           wTrovato = *on;
080100161115         else;
080200161115           // devo leggere un nuovo precedente
080300161115           wDtChk = wDatePrevious;
080400161115         endif;
080500161115
080600161115         endsr;
080700161115     C*------------------------------------------------------------------------*
080800161115     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
080900161115     C*------------------------------------------------------------------------*
081000161115     C     CHKPAR        BEGSR
081100161115     C*
081200970526     C* REIMPOSTO FLAG EVIDENZAZIONE ERRORI SUI PARAMETRI
081300970526     C                   SETOFF                                       808182
081400160913     C                   SETOFF                                       838485
081500970526     C*--------------------
081600970526     C* ORDINAMENTO
081700970526     C*--------------------
081800970526     C*--------------------
081900970526     C* SELEZIONI
082000970526     C*--------------------
082100970526     C*
082200160831     C* SE NESSUN ORDINAMENTO RICHIESTO, utilizzo il caricamento completo per Procedura
082300160831     C                   MOVEL     'ALL'         WTIPRI            3
082400970526     C*
082500970526     C* ACCETTA SOLO UN PARAMETRO DI INIZIO RICERCA
082600970526IF  1C     ERRL00        IFEQ      '0'
082700160913     C* SE gestiremo l'Inizio elenco mettere qui l'elenco controllando che solo 1 sia valorizzato
082800160713     C                   if        1 <> 1
082900970526     C                   MOVEL     MSG(6)        DSMSMS
083000970526     C                   MOVEL     '1'           ERRL00
083100160830     C                   SETON                                        808182
083200970526E   1C                   ENDIF
083300160713E   1C                   ENDIF
083400160830     C*
083500161108     C                   EVAL      WTIPRI = *blank
083600160830IF  1C     ERRL00        IFEQ      '0'
083700161108     C*                  SELECT
083800161108E   2C*                  ENDSL
083900970526E   1C                   ENDIF
084000970526     C*
084100970526     C                   ENDSR
084200970526     C*------------------------------------------------------------------------*
084300970526     C* LANPGM - LANCIA UN PROGRAMMA
084400970526     C*------------------------------------------------------------------------*
084500970526     C     LANPGM        BEGSR
084600970526     C*
084700970526     C* IMPOSTA LA DS DI PROCEDURA
084800161108     C                   MOVEL     WOP0          MAC1OP0                         *LIVELLO PROCEDURA
084900161108     C                   MOVEL     WOP1          MAC1OP1                         *LIVELLO PGM
085000161108     C                   MOVEL     '0'           MAC1F03                         *PREMUTO F03=FINE
085100161108     C                   MOVEL     '0'           MAC1F12                         *PREMUTO F12=RIT
085200161108     C                   MOVEL     '0'           MAC1ERR                         *'1'=ERRORE
085300161108     C                   MOVEL     *BLANKS       MAC1MSG                         *MESSAGGIO DI RIT
085400970526     C*
085500970526     C                   CALL      WPGM
085600970612     C                   PARM                    KPJBA
085700161108     C                   PARM                    TRBMMAC1DS
085800970526     C*
085900970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
086000161108IF  1C     MAC1F03       IFEQ      '1'                                          *PREMUTO F03
086100970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
086200970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
086300970526X   1C                   ELSE                                                   *NON PREMUTO F03
086400161108     C                   MOVEL     MAC1MSG       DSMSMS                         *MESSAGGIO DI RIT
086500970526E   1C                   ENDIF
086600970526     C*
086700970526     C                   ENDSR
086800970527     C*------------------------------------------------------------------------*
086900970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
087000970527     C*------------------------------------------------------------------------*
087100970527     C     CARTAB        BEGSR
087200970527     C*
087300970527     C                   ENDSR
087400970526     C*------------------------------------------------------------------------*
087500970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
087600970526     C*------------------------------------------------------------------------*
087700970526     C     CHKFOP        BEGSR
087800970626     C*
087900970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
088000970626IF  1C     DEPOP0        IFEQ      'R01'
088100161111     C***                MOVEL     OPD(1)        OPDRIG                         *SCELTA
088200161111     C***                MOVEL     'S'           OPDATT
088300161111     C***                MOVEL     OPDRIG        OPD(1)
088400970626X   1C                   ELSE
088500970626     C* OPZIONI
088600161111     C***                MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
088700161111     C***                MOVEL     'S'           OPDATT
088800161111     C***                MOVEL     OPDRIG        OPD(5)
088900970626     C* FUNZIONI
089000970626E   1C                   ENDIF
089100970626     C*
089200000602     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
089300970626IF  1C     DEPOP0        IFEQ      'R02'
089400161111     C***                MOVEL     OPD(1)        OPDRIG                         *MODIFICA
089500161111     C***                MOVEL     'S'           OPDATT
089600161111     C***                MOVEL     OPDRIG        OPD(1)
089700161111     C***                MOVEL     OPD(2)        OPDRIG                         *MODIFICA
089800161111     C***                MOVEL     'S'           OPDATT
089900161111     C***                MOVEL     OPDRIG        OPD(2)
090000161111     C***                MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
090100161111     C***                MOVEL     'S'           OPDATT
090200161111     C***                MOVEL     OPDRIG        OPD(4)
090300970715     C*
090400161111     C* SOLO SE EDP ABILITA  INSERIMENTO
090500970715IF  2C     UTENTE        IFEQ      'EDP'
090600161111     C***                MOVEL     OPD(3)        OPDRIG                         *COPIA
090700161111     C***                MOVEL     'S'           OPDATT
090800161111     C***                MOVEL     OPDRIG        OPD(3)
090900161111     C***                MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
091000161111     C***                MOVEL     'S'           FUDATT
091100161111     C***                MOVEL     FUDRIG        FUD(6)
091200970715E   2C                   ENDIF
091300970715E   1C                   ENDIF
091400970626     C*
091500000602     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO, ALLINEAMENTO
091600020919IF  1C     DUTLPO        IFNE      'S'                                          *FILIALE
091700970626     C                   MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
091800970626     C                   MOVEL     'N'           OPDATT
091900970626     C                   MOVEL     OPDRIG        OPD(4)
092000970626E   1C                   ENDIF
092100970526     C*
092200970526     C* GESTIONE OPZIONI A VIDEO
092300970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
092400970526     C                   EXSR      OPZGES
092500970526     C*
092600970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
092700970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
092800970526     C                   EXSR      FUNGES
092900970526     C*
093000970526     C                   ENDSR
093100020919     C*--------------------------------------------------------------------------------------------*
093200020919     C* REPERISCE I DATI UTENTE
093300020919     C*--------------------------------------------------------------------------------------------*
093400020919     C     REPDATIUTE    BEGSR
093500020919     C*
093600020919     C* INIZIALIZZA VARIABILI DI WRK
093700020919     C                   CLEAR                   TIBS34DS
093800020919     C                   CLEAR                   AZUTEDS
093900020919     C                   CLEAR                   DDATIUTE
094000020919     C*
094100020919     C     *DTAARA       DEFINE    §azute        azuteds
094200020919     C     *DTAARA       DEFINE    §datiute      ddatiute
094300020919     C                   IN(E)     *DTAARA
094400020919if  1C                   IF        %Error
094500020919     c                   EVAL      I34Tla = 'L'
094600020919     C                   CALL      'TIBS34R'
094700020919     C                   PARM                    Tibs34Ds
094800020919     C                   IN        *DTAARA
094900020919e   1C                   ENDIF
095000020919     C*
095100020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
095200020919     C                   MOVEL(P)  RSUT          DSFIRS           20
095300020919     C*
095400020919     C                   ENDSR
095500970526     C*------------------------------------------------------------------------*
095600970526     C* *INZSR - OPERAZIONI INIZIALI
095700970526     C*------------------------------------------------------------------------*
095800970526     C     *INZSR        BEGSR
095900970526     C*---
096000970526     C* RICEVIMENTO PARAMETRI
096100970526     C*---
096200970526     C     *ENTRY        PLIST
096300970612     C                   PARM                    KPJBA
096400160714     C*
096500161108     C                   EVAL      TRBMMAC1DS = kpjbu
096600160913     c                   if        kpjbu = *blank
096700161108     c                   clear                   TRBMMAC1DS
096800160913     c                   endif
096900970526     C*
097000970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
097100970526     C     XMSGPA        PLIST
097200970526     C                   PARM                    DSMSMS
097300970526     C                   PARM                    DSMSPG
097400970526     C                   PARM                    DSMSKE
097500970526     C                   PARM                    DSMSER
097600970526     C*
097700970526     C* IMPOSTA LA DS -TASTI PREMUTI-
097800970526     C                   CALL      'XKEY'
097900970526     C                   PARM                    DSKEY
098000970526     C*---
098100970526     C* VARIABILI NON RIFERITE AL DATA BASE
098200970526     C*---
098300970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
098400970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
098500970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
098600970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
098700970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
098800970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
098900970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
099000970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
099100970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
099200161108     C     *LIKE         DEFINE    MAC1OP0       DEPOP0                         *DI DEPOSITO
099300161108     C     *LIKE         DEFINE    MAC1OP1       DEPOP1
099400161108     C     *LIKE         DEFINE    MAC1OP0       WOP0                           *DI LAVORO
099500161108     C     *LIKE         DEFINE    MAC1OP1       WOP1
099600970526     C*---
099700970526     C* VARIABILI DI CONTROLLO
099800970526     C*---
099900970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
100000970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
100100970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
100200970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
100300970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
100400160714     C                   Z-ADD     14            SFLPAG                         *N°RIGHE X PAG
100500970526     C*
100600970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
100700161108     C                   MOVEL     'TRBMMAC1R'   DSMSPG
100800161108     C                   MOVEL     'TRBMMAC1R'   MSGQUE
100900970526     C                   MOVEL     *BLANKS       MSGKEY
101000970526     C                   MOVEL     *BLANKS       DSMSMS
101100970526     C                   MOVEL     *BLANKS       DSMSKE
101200970526     C                   Z-ADD     *ZEROS        DSMSER
101300970526     C*---
101400970526     C* VARIABILI RIFERITE AL DATA BASE
101500970526     C*---
101600970526     C*---
101700970526     C* CHIAVI DI LETTURA
101800970526     C*---
101900970527     C*
102000970527     C* REPERIMENTO DATI UTENTE
102100020919     C                   EXSR      REPDATIUTE
102200970527     C*
102300970527     C* CARICA LE TABELLE OCCORRENTI
102400970527     C                   EXSR      CARTAB
102500970526     C*
102600970526     C* DEPOSITA LE OPZIONI DI ENTRATA
102700161108     C                   MOVEL     MAC1OP0       DEPOP0                         *LIVELLO PROCEDURA
102800161108     C                   MOVEL     MAC1OP1       DEPOP1                         *LIVELLO PROGRAMMA
102900970526     C*
103000970526     C* CALCOLA LA DATA CORRENTE
103100970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
103200970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
103300970526     C                   Z-ADD     WN8           G08DAT
103400970526     C                   Z-ADD     *ZEROS        G08INV
103500970526     C                   MOVEL     '0'           G08ERR
103600970526     C                   CALL      'XSRDA8'
103700970526     C                   PARM                    WLBDA8
103800970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
103900970526     C*
104000970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
104100970526     C                   EXSR      CHKFOP
104200970526     C*
104300970526     C* IMPOSTA I PARAMETRI DI INGRESSO
104400970526     C*
104500161108     C*
104600970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
104700970530IF  1C     DEPOP0        IFEQ      'R01'
104800970530     C                   MOVEL     DEC(3)        VIDING
104900970530E   1C                   ENDIF
105000970530IF  1C     DEPOP0        IFEQ      'R02'
105100970530     C                   MOVEL     DEC(1)        VIDING
105200970530E   1C                   ENDIF
105300970530IF  1C     DEPOP0        IFEQ      'R05'
105400970530     C                   MOVEL     DEC(2)        VIDING
105500970530E   1C                   ENDIF
105600161110
105700161110       // aggiorna TIMAC00F prima di caricare il SFL
105800161110       exsr sr_AggTIMAC;
105900161110
106000970526     C*
106100970526     C                   ENDSR
106200161110
106300161110       //----------------------------------------------------------------------*
106400161110       // aggiorna TIMAC00F
106500161110       //----------------------------------------------------------------------*
106600161110       BEGSR sr_AggTIMAC;
106700161110
106800161110         // passo come periodo tutto (dal *loval a oggi)
106900161110         MACHKTSDa = *loval;
107000161110         MACHKTSAl = %timestamp();
107100161110
107200161110         // chiamo
107300161110         UBMACHK1R_S( *blank :
107400161110                      MACHKTSDa :
107500161110                      MACHKTSAl :
107600161110                      MACHKEsito );
107700161110
107800161110         // se l'esito non è 1=OK
107900161110         if MACHKEsito <>'1';
108000161110         // DEVO CREARE UN MSG MA PER ORA NON SO COME
108100161110
108200161110         endif;
108300161110
108400161110       ENDSR;
108500161110
108600970526     C*------------------------------------------------------------------------*
108700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
108800970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
108900970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
109000970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
109100970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
109200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
109300970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
109400970526     C*------------------------------------------------------------------------*
109500970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
109600970526     C*------------------------------------------------------------------------*
109700970526     C     POSSFL        BEGSR
109800970526     C*
109900970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
110000970526     C                   EXSR      CLRSFL
110100970526     C*
110200970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
110300970526     C                   EXSR      SETFIL
110400970526     C*
110500970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
110600970526     C     SFLEOF        IFEQ      0
110700970526     C                   EXSR      CARSFL
110800970526     C                   END
110900970526     C*
111000970526     C                   ENDSR
111100970526     C*------------------------------------------------------------------------*
111200970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
111300970526     C*------------------------------------------------------------------------*
111400970526     C     ENTATT        BEGSR
111500970526     C*
111600970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
111700970526     C                   Z-ADD     0             OPZOK             1 0
111800970526     C*
111900970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
112000970526     C                   Z-ADD     0             SFLVAR            5 0
112100970526     C*
112200970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
112300970526     C                   Z-ADD     0             SFLERR
112400970526     C*
112500970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
112600970526     C                   Z-ADD     SFLULT        WFLULT
112700970526     C*
112800970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
112900161108     C                   READC     BMC1D05                                98
113000970526     C     *IN98         DOWEQ     '0'
113100970526     C*
113200970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
113300970526     C                   ADD       1             SFLVAR
113400970526     C*
113500970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
113600970526     C     OPZOK         IFEQ      0
113700970526     C*
113800970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
113900970526     C     OPZKEY        IFNE      *BLANKS
114000970526     C                   EXSR      OPZCON
114100970526     C                   END
114200970526     C*
114300970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
114400970526     C*            2. PULIZIA SCELTA IN SFL
114500970526     C     OPZOK         IFEQ      0
114600970526     C                   MOVEL     '0'           *IN41
114700970526     C                   MOVEL     *BLANKS       OPZKEY
114800970526     C                   ELSE
114900970526     C*
115000970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
115100970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
115200970526     C                   MOVEL     '1'           *IN41
115300970526     C                   Z-ADD     SFLULT        SFLERR
115400970526     C                   END
115500970526     C                   ELSE
115600970526     C*
115700970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
115800970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
115900970526     C                   MOVEL     '1'           *IN41
116000970526     C                   END                                                    OPZOK=0
116100970526     C*
116200970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
116300970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
116400970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
116500970526     C     OPZOK         IFNE      0
116600970526     C                   Z-ADD     SFLERR        SFLNBR
116700970526     C                   ELSE
116800970526     C                   Z-ADD     SFLULT        SFLNBR
116900970526     C                   END
117000970526     C*
117100970526     C* AGGIORNO RECORD SFL
117200160902     C*  imposto "effetti speciali"
117300160902     C                   EXSR      SFXD05
117400161108     C                   UPDATE    BMC1D05
117500970526     C*
117600161108     C                   READC     BMC1D05                                98
117700970526     C                   END                                                    DO WHILE
117800970526     C*
117900970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
118000970526     C                   Z-ADD     WFLULT        SFLULT
118100970526     C*
118200970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
118300970526     C     SFLVAR        IFEQ      0
118400970526     C*---                 MOVEL'1'       FINL00
118500970526     C                   END
118600970526     C*
118700970526     C                   ENDSR
118800970526     C*------------------------------------------------------------------------*
118900970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
119000970526     C*------------------------------------------------------------------------*
119100970526     C     CLRSFL        BEGSR
119200970526     C*
119300970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
119400970526     C* (0=NON FINE FILE  1=FINE FILE)
119500970526     C                   Z-ADD     0             SFLEOF
119600970526     C*
119700970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
119800970526     C                   Z-ADD     0             SFLULT
119900970526     C*
120000970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
120100970526     C                   MOVEL     '1'           *IN40
120200161108     C                   WRITE     BMC1D06
120300970526     C                   MOVEL     '0'           *IN40
120400970526     C*
120500970526     C                   ENDSR
120600970526     C*------------------------------------------------------------------------*
120700970526     C* CARSFL - CARICAMENTO SFL
120800970526     C*------------------------------------------------------------------------*
120900970526     C     CARSFL        BEGSR
121000970526     C*
121100970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
121200970526     C     SFLULT        IFEQ      0
121300970526     C                   EXSR      SETFIL
121400970526     C                   END
121500970526     C*
121600970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
121700970526     C                   Z-ADD     SFLULT        WFLULT
121800970526     C*
121900970526     C* CICLO:
122000970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
122100970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
122200970526     C                   Z-ADD     1             SFLRIG
122300970526     C     SFLRIG        DOWLE     SFLPAG
122400970526     C     SFLEOF        ANDEQ     0
122500970526     C*
122600970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
122700970526     C                   MOVEL     *BLANKS       OPZKEY
122800970526     C*
122900970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
123000970526     C                   EXSR      IRESFL
123100970526     C*
123200970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
123300970526     C                   ADD       1             SFLULT
123400970526     C*
123500970526     C* SCRITTURA RECORD SFL
123600160902     C*  imposto "effetti speciali"
123700160902     C                   EXSR      SFXD05
123800161108     C                   WRITE     BMC1D05
123900970526     C                   ADD       1             SFLRIG
124000970526     C*
124100970526     C* LETTURA FILE DA CARICARE
124200970526     C                   EXSR      LETFIL
124300970526     C                   END                                                    DO WHILE
124400970526     C*
124500970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
124600970526     C     WFLULT        IFNE      SFLULT
124700970526     C                   Z-ADD     WFLULT        SFLNBR
124800970526     C                   ADD       1             SFLNBR
124900970526     C                   END
125000970526     C*
125100970526     C                   ENDSR
125200970526     C*------------------------------------------------------------------------*
125300160902     C* SFXD05 - effetti speciali sfl D05 (RI, ND, BL, ...)
125400160902     C*          a seconda dei dati presenti in riga
125500970526     C*------------------------------------------------------------------------*
125600160902     C     SFXD05        BEGSR
125700161110     C*
125800161111     C* HI campi conteggio e media se storicizzato
125900161130     C* nel caso in cui siamo in un livello con fascia oraria
126000161110     C                   SETOFF                                       01
126100161130     C                   if        wLivDrill > 1
126200161110     C                   IF        D05Sto1 <>'S'
126300161110     C                   SETON                                        01
126400161110     C                   ENDIF
126500161111     C                   ENDIF
126600161115     C*
126700161115     C* HI campi giorno precedente conteggio e media se storicizzato
126800161130     C* nel caso in cui siamo in comparazione e a livello 2
126900161115     C                   SETOFF                                       02
127000161130     C                   if        wLivDrill= 2    and
127100161115     C                             wCompara = *on
127200161115     C                   IF        D05Sto1P <>'S'
127300161115     C                   SETON                                        02
127400161115     C                   ENDIF
127500161115     C                   ENDIF
127600160902     C*
127700160902     C                   ENDSR
127800160902     C*------------------------------------------------------------------------*
127900160902     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
128000160902     C*------------------------------------------------------------------------*
128100160902     C     OUTSFL        BEGSR
128200160902     C*
128300970526     C* WRITE RECORD TESTATA VIDEO
128400161108     C                   WRITE     BMC1D01
128500970526     C*
128600970526     C* VISUALIZZAZIONE MESSAGGIO
128700970526     C                   EXSR      SNDMSG
128800970526     C*
128900970526     C* WRITE RECORD OPZIONI VIDEO
129000161108     C                   WRITE     BMC1D02
129100970526     C*
129200970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
129300161108     C                   WRITE     BMC1D03
129400970526     C*
129500970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
129600161108     C                   WRITE     BMC1D04
129700970526     C*
129800970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
129900970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
130000970526     C     SFLEOF        IFEQ      0
130100970526     C                   MOVEL     '0'           *IN42
130200970526     C                   ELSE
130300970526     C*
130400970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
130500970526     C                   MOVEL     '1'           *IN42
130600970526     C                   END
130700970526     C*
130800970526     C* WRITE RECORD TASTI FUNZIONALI
130900161108     C                   WRITE     BMC1D09
131000970526     C*
131100970526     C* CASO DI SFL VUOTO
131200970526     C     SFLULT        IFLE      0
131300970526     C*
131400970526     C*    - WRITE RECORD VUOTO
131500161108     C                   WRITE     BMC1D0A
131600161108     C                   READ      BMC1D0A                                99
131700970526     C*
131800970526     C                   ELSE
131900970526     C*
132000970526     C* CASO DI SFL CON RECORDS
132100970526     C*
132200970526     C*    - WRITE RECORD CONTROLLO SFL
132300161108     C                   WRITE     BMC1D06
132400161108     C                   READ      BMC1D06                                99
132500970526     C                   END
132600970526     C*
132700970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
132800161108     C                   READ      BMC1D03                                99
132900970526     C*
133000970526     C                   ENDSR
133100970526     C*------------------------------------------------------------------------*
133200970526     C* SNDMSG - MANDA MESSAGGIO
133300970526     C*------------------------------------------------------------------------*
133400970526     C     SNDMSG        BEGSR
133500970526     C*
133600970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
133700970526     C                   MOVEL     '1'           *IN40
133800161108     C                   WRITE     BMC1DMC
133900970526     C                   MOVEL     '0'           *IN40
134000970526     C*
134100970526     C* VISUALIZZAZIONE MESSAGGIO
134200970526     C                   Z-ADD     0             MSGULT
134300970526     C                   CALL      'XMSG'        XMSGPA
134400970526     C     DSMSER        IFEQ      0
134500970526     C                   MOVEL     DSMSKE        MSGKEY
134600970526     C                   END
134700970526     C                   Z-ADD     1             MSGULT
134800161108     C                   WRITE     BMC1DMS                                      SFL MESS
134900161108     C                   WRITE     BMC1DMC                                      CONTR MESS
135000970526     C*
135100970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
135200970526     C                   MOVEL     *BLANKS       DSMSMS
135300970526     C*
135400970526     C                   ENDSR
135500970526     C*------------------------------------------------------------------------*
135600970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
135700970526     C*------------------------------------------------------------------------*
135800970526     C     FUNCON        BEGSR
135900970526     C*
136000970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
136100970526     C                   EXSR      FUNVAR
136200970526     C*
136300970526     C* PULISCO CAMPI DI LAVORO
136400970526     C                   Z-ADD     0             I                 5 0
136500970526     C                   Z-ADD     0             W                 5 0
136600970526     C*
136700970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
136800970526     C     FUNKEY        IFNE      *BLANKS
136900970526     C     1             DO        24            I
137000970526     C                   MOVEL     FUD(I)        FUDRIG
137100970526     C*
137200970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
137300970526     C     FUNKEY        IFEQ      FUDFUN
137400970526     C     FUDATT        ANDEQ     'S'
137500970526     C                   Z-ADD     I             W
137600970526     C                   END
137700970526     C                   END                                                    DO WHILE
137800970526     C*
137900970526     C* FUNZIONE NON ATTIVA (ERRORE)
138000970526     C     W             IFEQ      0
138100970526     C*
138200970526     C* EMISSIONE MESSAGGIO DI ERRORE
138300970526     C                   MOVEL     MSG(2)        DSMSMS
138400970526     C                   END                                                    W=0
138500970526     C                   END                                                    FUNKEY NE *BLANKS
138600970526     C*
138700970526     C* FUNZIONE ATTIVA (PROCEDI ...)
138800970526     C     W             IFNE      0
138900970526     C     FUNKEY        OREQ      *BLANKS
139000970526     C*
139100970526     C     FUNKEY        IFEQ      *BLANKS
139200970526     C                   EXSR      ENTGES
139300970526     C                   END
139400970526     C     FUNKEY        IFEQ      '03'
139500970526     C                   EXSR      F03GES
139600970526     C                   END
139700970526     C     FUNKEY        IFEQ      '05'
139800970526     C                   EXSR      F05GES
139900970526     C                   END
140000970526     C     FUNKEY        IFEQ      '06'
140100970526     C                   EXSR      F06GES
140200970526     C                   END
140300161130     C     FUNKEY        IFEQ      '07'
140400161130     C                   EXSR      F07GES
140500161111     C                   END
140600161130     C     FUNKEY        IFEQ      '08'
140700161130     C                   EXSR      F08GES
140800161130     C                   END
140900161115     C     FUNKEY        IFEQ      '09'
141000161115     C                   EXSR      F09GES
141100161115     C                   END
141200160913     C     FUNKEY        IFEQ      '12'
141300160913     C                   EXSR      F12GES
141400160913     C                   END
141500970526     C     FUNKEY        IFEQ      '23'
141600970526     C                   EXSR      F23GES
141700970526     C                   END
141800970526     C     FUNKEY        IFEQ      '24'
141900970526     C                   EXSR      F24GES
142000970526     C                   END
142100970526     C     FUNKEY        IFEQ      'RU'
142200970526     C                   EXSR      RUPGES
142300970526     C                   END
142400970526     C*
142500970526     C                   END
142600970526     C*
142700970526     C                   ENDSR
142800970526     C*------------------------------------------------------------------------*
142900970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
143000970526     C*------------------------------------------------------------------------*
143100970526     C     FUNVAR        BEGSR
143200970526     C*
143300970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
143400970526     C     KEY           IFEQ      ENTER
143500970526     C                   MOVEL     *BLANKS       FUNKEY
143600970526     C                   END
143700970526     C     KEY           IFEQ      F1
143800970526     C                   MOVEL     '01'          FUNKEY
143900970526     C                   END
144000970526     C     KEY           IFEQ      F2
144100970526     C                   MOVEL     '02'          FUNKEY
144200970526     C                   END
144300970526     C     KEY           IFEQ      F3
144400970526     C                   MOVEL     '03'          FUNKEY
144500970526     C                   END
144600970526     C     KEY           IFEQ      F4
144700970526     C                   MOVEL     '04'          FUNKEY
144800970526     C                   END
144900970526     C     KEY           IFEQ      F5
145000970526     C                   MOVEL     '05'          FUNKEY
145100970526     C                   END
145200970526     C     KEY           IFEQ      F6
145300970526     C                   MOVEL     '06'          FUNKEY
145400970526     C                   END
145500970526     C     KEY           IFEQ      F7
145600970526     C                   MOVEL     '07'          FUNKEY
145700970526     C                   END
145800970526     C     KEY           IFEQ      F8
145900970526     C                   MOVEL     '08'          FUNKEY
146000970526     C                   END
146100970526     C     KEY           IFEQ      F9
146200970526     C                   MOVEL     '09'          FUNKEY
146300970526     C                   END
146400970526     C     KEY           IFEQ      F10
146500970526     C                   MOVEL     '10'          FUNKEY
146600970526     C                   END
146700970526     C     KEY           IFEQ      F11
146800970526     C                   MOVEL     '11'          FUNKEY
146900970526     C                   END
147000970526     C     KEY           IFEQ      F12
147100970526     C                   MOVEL     '12'          FUNKEY
147200970526     C                   END
147300970526     C     KEY           IFEQ      F13
147400970526     C                   MOVEL     '13'          FUNKEY
147500970526     C                   END
147600970526     C     KEY           IFEQ      F14
147700970526     C                   MOVEL     '14'          FUNKEY
147800970526     C                   END
147900970526     C     KEY           IFEQ      F15
148000970526     C                   MOVEL     '15'          FUNKEY
148100970526     C                   END
148200970526     C     KEY           IFEQ      F16
148300970526     C                   MOVEL     '16'          FUNKEY
148400970526     C                   END
148500970526     C     KEY           IFEQ      F17
148600970526     C                   MOVEL     '17'          FUNKEY
148700970526     C                   END
148800970526     C     KEY           IFEQ      F18
148900970526     C                   MOVEL     '18'          FUNKEY
149000970526     C                   END
149100970526     C     KEY           IFEQ      F19
149200970526     C                   MOVEL     '19'          FUNKEY
149300970526     C                   END
149400970526     C     KEY           IFEQ      F20
149500970526     C                   MOVEL     '20'          FUNKEY
149600970526     C                   END
149700970526     C     KEY           IFEQ      F21
149800970526     C                   MOVEL     '21'          FUNKEY
149900970526     C                   END
150000970526     C     KEY           IFEQ      F22
150100970526     C                   MOVEL     '22'          FUNKEY
150200970526     C                   END
150300970526     C     KEY           IFEQ      F23
150400970526     C                   MOVEL     '23'          FUNKEY
150500970526     C                   END
150600970526     C     KEY           IFEQ      F24
150700970526     C                   MOVEL     '24'          FUNKEY
150800970526     C                   END
150900970526     C     KEY           IFEQ      ROLLUP
151000970526     C                   MOVEL     'RU'          FUNKEY
151100970526     C                   END
151200970526     C     KEY           IFEQ      HELP
151300970526     C                   MOVEL     'HP'          FUNKEY
151400970526     C                   END
151500970526     C*
151600970526     C                   ENDSR
151700970526     C*------------------------------------------------------------------------*
151800970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
151900970526     C*------------------------------------------------------------------------*
152000970526     C     FUNGES        BEGSR
152100970526     C*
152200970526     C* PULISCO CAMPI DI LAVORO
152300970526     C                   Z-ADD     0             W                 5 0
152400970526     C*
152500970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
152600970526     C* E CARICO TABELLA FUNZIONI ATTIVE
152700970526     C     1             DO        24            I
152800970526     C                   MOVEL     FUD(I)        FUDRIG
152900970526     C*
153000970526     C* PULIZIA FUNZIONI PRECEDENTI
153100970526     C                   MOVEL     *BLANKS       FUV(I)
153200970526     C*
153300970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
153400970526     C* (SALTA F23 E F24)
153500970526     C     FUDATT        IFEQ      'S'
153600970526     C     FUDFUN        ANDNE     '23'
153700970526     C     FUDFUN        ANDNE     '24'
153800970526     C                   ADD       1             W
153900970526     C                   MOVEL     FUDDES        FUV(W)
154000970526     C                   END
154100970526     C                   END                                                    DO WHILE
154200970526     C*
154300970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
154400970526     C* ALTRIMENTI DISATTIVO F24
154500970526     C     1             DO        24            I
154600970526     C                   MOVEL     FUD(I)        FUDRIG
154700970526     C*
154800970526     C     FUDFUN        IFEQ      '24'
154900970526     C     W             IFGT      6
155000970526     C                   MOVEL     'S'           FUDATT
155100970526     C                   ELSE
155200970526     C                   MOVEL     *BLANKS       FUDATT
155300970526     C                   END                                                    W GT 8
155400970526     C                   MOVEL     FUDRIG        FUD(I)
155500970526     C                   END
155600970526     C                   END                                                    DO WHILE
155700970526     C*
155800970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
155900970526     C     W             DIV       6             I
156000970526     C                   MVR                     Z
156100970526     C*
156200970526     C     Z             IFNE      0
156300970526     C     I             ADD       1             Z
156400970526     C                   ELSE
156500970526     C                   Z-ADD     I             Z
156600970526     C                   END
156700970526     C*
156800970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
156900970526     C                   ADD       1             FUNGRU
157000970526     C     FUNGRU        IFGT      Z
157100970526     C     FUNGRU        ORLT      1
157200970526     C                   Z-ADD     1             FUNGRU
157300970526     C                   END
157400970526     C*
157500970526     C* PRIMO GRUPPO DI 6 FUNZIONI
157600970526     C     FUNGRU        IFEQ      1
157700970526     C                   MOVEL     FUV(1)        FUN001
157800970526     C                   MOVEL     FUV(2)        FUN002
157900970526     C                   MOVEL     FUV(3)        FUN003
158000970526     C                   MOVEL     FUV(4)        FUN004
158100970526     C                   MOVEL     FUV(5)        FUN005
158200970526     C                   MOVEL     FUV(6)        FUN006
158300970526     C                   END
158400970526     C*
158500970526     C* SECONDO GRUPPO DI 6 FUNZIONI
158600970526     C     FUNGRU        IFEQ      2
158700970526     C                   MOVEL     FUV(7)        FUN001
158800970526     C                   MOVEL     FUV(8)        FUN002
158900970526     C                   MOVEL     FUV(9)        FUN003
159000970526     C                   MOVEL     FUV(10)       FUN004
159100970526     C                   MOVEL     FUV(11)       FUN005
159200970526     C                   MOVEL     FUV(12)       FUN006
159300970526     C                   END
159400970526     C*
159500970526     C* TERZO GRUPPO DI 6 FUNZIONI
159600970526     C     FUNGRU        IFEQ      3
159700970526     C                   MOVEL     FUV(13)       FUN001
159800970526     C                   MOVEL     FUV(14)       FUN002
159900970526     C                   MOVEL     FUV(15)       FUN003
160000970526     C                   MOVEL     FUV(16)       FUN004
160100970526     C                   MOVEL     FUV(17)       FUN005
160200970526     C                   MOVEL     FUV(18)       FUN006
160300970526     C                   END
160400970526     C*
160500970526     C* QUARTO GRUPPO DI 6 FUNZIONI
160600970526     C     FUNGRU        IFEQ      4
160700970526     C                   MOVEL     FUV(19)       FUN001
160800970526     C                   MOVEL     FUV(20)       FUN002
160900970526     C                   MOVEL     FUV(21)       FUN003
161000970526     C                   MOVEL     FUV(22)       FUN004
161100970526     C                   MOVEL     *BLANKS       FUN005
161200970526     C                   MOVEL     *BLANKS       FUN006
161300970526     C                   END
161400970526     C*
161500970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
161600970526     C                   MOVEL     *BLANKS       FUN007
161700970526     C                   MOVEL     *BLANKS       FUN008
161800970526     C*
161900970526     C     1             DO        24            I
162000970526     C                   MOVEL     FUD(I)        FUDRIG
162100970526     C     FUDFUN        IFEQ      '23'
162200970526     C     FUDATT        ANDEQ     'S'
162300970526     C                   MOVEL     FUDDES        FUN007
162400970526     C                   END
162500970526     C     FUDFUN        IFEQ      '24'
162600970526     C     FUDATT        ANDEQ     'S'
162700970526     C                   MOVEL     FUDDES        FUN008
162800970526     C                   END
162900970526     C                   END                                                    DO
163000970526     C*
163100970526     C                   ENDSR
163200970526     C*------------------------------------------------------------------------*
163300970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
163400970526     C*------------------------------------------------------------------------*
163500970526     C     OPZGES        BEGSR
163600970526     C*
163700970526     C* PULISCO CAMPI DI LAVORO
163800970526     C                   Z-ADD     0             W                 5 0
163900970526     C*
164000970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
164100970526     C* E CARICO TABELLA OPZIONI ATTIVE
164200970526     C     1             DO        24            I
164300970526     C                   MOVEL     OPD(I)        OPDRIG
164400970526     C*
164500970526     C* PULIZIA OPZIONI PRECEDENTI
164600970526     C                   MOVEL     *BLANKS       OPV(I)
164700970526     C*
164800970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
164900970526     C     OPDATT        IFEQ      'S'
165000970526     C                   ADD       1             W
165100970526     C                   MOVEL     OPDDES        OPV(W)
165200970526     C                   END
165300970526     C                   END                                                    DO WHILE
165400970526     C*
165500970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
165600970526     C* ALTRIMENTI DISATTIVO F23
165700970526     C     1             DO        24            I
165800970526     C                   MOVEL     FUD(I)        FUDRIG
165900970526     C*
166000970526     C     FUDFUN        IFEQ      '23'
166100970526     C     W             IFGT      8
166200970526     C                   MOVEL     'S'           FUDATT
166300970526     C                   ELSE
166400970526     C                   MOVEL     *BLANKS       FUDATT
166500970526     C                   END                                                    W GT 8
166600970526     C                   MOVEL     FUDRIG        FUD(I)
166700970526     C                   END
166800970526     C                   END                                                    DO WHILE
166900970526     C*
167000970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
167100970526     C     W             DIV       8             I
167200970526     C                   MVR                     Z                 5 0
167300970526     C*
167400970526     C     Z             IFNE      0
167500970526     C     I             ADD       1             Z
167600970526     C                   ELSE
167700970526     C                   Z-ADD     I             Z
167800970526     C                   END
167900970526     C*
168000970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
168100970526     C                   ADD       1             OPZGRU
168200970526     C     OPZGRU        IFGT      Z
168300970526     C     OPZGRU        ORLT      1
168400970526     C                   Z-ADD     1             OPZGRU
168500970526     C                   END
168600970526     C*
168700970526     C* PRIMO GRUPPO DI 8 OPZIONI
168800970526     C     OPZGRU        IFEQ      1
168900970526     C                   MOVEL     OPV(1)        OPZ001
169000970526     C                   MOVEL     OPV(2)        OPZ002
169100970526     C                   MOVEL     OPV(3)        OPZ003
169200970526     C                   MOVEL     OPV(4)        OPZ004
169300970526     C                   MOVEL     OPV(5)        OPZ005
169400970526     C                   MOVEL     OPV(6)        OPZ006
169500970526     C                   MOVEL     OPV(7)        OPZ007
169600970526     C                   MOVEL     OPV(8)        OPZ008
169700970526     C                   END
169800970526     C*
169900970526     C* SECONDO GRUPPO DI 8 OPZIONI
170000970526     C     OPZGRU        IFEQ      2
170100970526     C                   MOVEL     OPV(9)        OPZ001
170200970526     C                   MOVEL     OPV(10)       OPZ002
170300970526     C                   MOVEL     OPV(11)       OPZ003
170400970526     C                   MOVEL     OPV(12)       OPZ004
170500970526     C                   MOVEL     OPV(13)       OPZ005
170600970526     C                   MOVEL     OPV(14)       OPZ006
170700970526     C                   MOVEL     OPV(15)       OPZ007
170800970526     C                   MOVEL     OPV(16)       OPZ008
170900970526     C                   END
171000970526     C*
171100970526     C* TERZO GRUPPO DI 8 OPZIONI
171200970526     C     OPZGRU        IFEQ      3
171300970526     C                   MOVEL     OPV(17)       OPZ001
171400970526     C                   MOVEL     OPV(18)       OPZ002
171500970526     C                   MOVEL     OPV(19)       OPZ003
171600970526     C                   MOVEL     OPV(20)       OPZ004
171700970526     C                   MOVEL     OPV(21)       OPZ005
171800970526     C                   MOVEL     OPV(22)       OPZ006
171900970526     C                   MOVEL     OPV(23)       OPZ007
172000970526     C                   MOVEL     OPV(24)       OPZ008
172100970526     C                   END
172200970526     C*
172300970526     C                   ENDSR
172400970526     C*------------------------------------------------------------------------*
172500970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
172600970526     C*------------------------------------------------------------------------*
172700970526     C     OPZCON        BEGSR
172800970526     C*
172900970526     C* PULISCO CAMPI DI LAVORO
173000970526     C                   Z-ADD     0             I                 5 0
173100970526     C                   Z-ADD     0             W                 5 0
173200970526     C*
173300970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
173400970526     C     1             DO        24            I
173500970526     C                   MOVEL     OPD(I)        OPDRIG
173600970526     C*
173700970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
173800970526     C     OPZKEY        IFEQ      OPDOPZ
173900970526     C     OPZKEY        OREQ      OPDOP1
174000970526     C*
174100970526     C     OPDATT        IFEQ      'S'
174200970526     C                   MOVEL     OPDOPZ        OPZKEY
174300970526     C                   Z-ADD     I             W
174400970526     C                   END
174500970526     C                   END
174600970526     C*
174700970526     C                   END                                                    DO WHILE
174800970526     C*
174900970526     C* OPZIONE NON ATTIVA (ERRORE)
175000970526     C     W             IFEQ      0
175100970526     C*
175200970526     C* OPZIONE NON OK
175300970526     C                   Z-ADD     1             OPZOK
175400970526     C*
175500970526     C* EMISSIONE MESSAGGIO DI ERRORE
175600970526     C                   MOVEL     MSG(1)        DSMSMS
175700970526     C                   ELSE
175800970526     C*
175900970526     C* OPZIONE ATTIVA (PROCEDI ...)
176000970526     C*
176100970526     C     OPZKEY        IFEQ      ' 1'
176200970526     C                   EXSR      O01GES
176300970526     C                   END
176400970526     C     OPZKEY        IFEQ      ' 2'
176500970526     C                   EXSR      O02GES
176600970526     C                   END
176700970603     C     OPZKEY        IFEQ      ' 3'
176800970603     C                   EXSR      O03GES
176900970603     C                   END
177000970526     C     OPZKEY        IFEQ      ' 4'
177100970526     C                   EXSR      O04GES
177200970526     C                   END
177300970526     C     OPZKEY        IFEQ      ' 5'
177400970526     C                   EXSR      O05GES
177500970526     C                   END
177600970526     C     OPZKEY        IFEQ      ' 7'
177700970526     C                   END
177800970528     C     OPZKEY        IFEQ      ' 8'
177900970528     C                   END
178000970528     C     OPZKEY        IFEQ      ' 9'
178100970528     C                   END
178200970528     C     OPZKEY        IFEQ      '10'
178300970528     C                   END
178400970528     C     OPZKEY        IFEQ      '11'
178500970528     C                   END
178600970526     C                   END                                                    OPZ.NON ATTIVA
178700970526     C*
178800970526     C                   ENDSR
178900160831
179000160831       /free
179100160831
179200160831       //--------------------------------------------------------------
179300160831       //?Preparazione stringa SQL                                     ?
179400160831       //--------------------------------------------------------------
179500160831       BEGSR  Prepara_SQL;
179600160831
179700160901         // Chiudo il cursore (lo faccio in anticipo perché non saprei quando chiuderlo
179800160901         exec sql  close C1;
179900160901
180000161130         select;
180100161130
180200161130          // liv.3 Giorno-Fascia oraria-Server
180300161130          when wLivDrill = 3;
180400161130           wSQL = 'select macdtchk, mactmchk, macserver, maccnt1, macdif1, +
180500161130                   macsto1 +
180600161122                   from timac00f';
180700161223           //if wCompara = *on;
180800161223           //  wSQL = %trim(wSQL) +
180900161223           //       ' where macdtchk = current_date';
181000161223           //endif;
181100161111           wSQL = %trim(wSQL) +
181200161201                  ' order by macdtchk desc , mactmchk desc, macserver';
181300161130
181400161130          // liv.2 Giorno-Fascia oraria
181500161130          when wLivDrill = 2;
181600161130           wSQL = 'select macdtchk, mactmchk, +
181700161130                   sum(maccnt1), avg(macdif1), max(macsto1) +
181800161130                   from timac00f';
181900161223           //if wCompara = *on;
182000161223           //  wSQL = %trim(wSQL) +
182100161223           //       ' where macdtchk = current_date';
182200161223           //endif;
182300161130           wSQL = %trim(wSQL) +
182400161130                  ' group by macdtchk , mactmchk';
182500161130           wSQL = %trim(wSQL) +
182600161130                  ' order by macdtchk desc , mactmchk desc';
182700161115
182800161130          // liv.2 Giorno
182900161130          // e leggo tutti i giorni
183000161130          when wLivDrill = 1;
183100161111           wSQL = 'select macdtchk, sum(maccnt1), avg(macdif1) +
183200161111                   from timac00f';
183300161111           wSQL = %trim(wSQL) +
183400161111                  ' group by macdtchk';
183500161111           wSQL = %trim(wSQL) +
183600161111                  ' order by macdtchk desc';
183700161130         endsl;
183800160831
183900160831         // Dichiarazione cursore
184000160831         exec sql   prepare S1   from :wSQL;
184100160831         exec sql   declare C1  asensitive   cursor for S1;
184200160831
184300160831         // Apertura del cursore
184400160831         exec sql   open C1;
184500160831
184600160901         *in99 = *off;
184700160901         if SQLCode < 0;
184800160901           *in99 = *on;
184900160831         endif;
185000160831
185100160831       ENDSR;
185200160831
185300160831       //--------------------------------------------------------------
185400160831       //?Lettura occorrenza                                           ?
185500160831       //--------------------------------------------------------------
185600160901       BEGSR  Legge_SQL;
185700160831
185800161130         select;
185900161130
186000161130          // liv.3 Giorno-Fascia oraria-Server
186100161130          when wLivDrill = 3;
186200161130           exec sql  fetch next  from C1  into :TIMAC_fetch3;
186300161130          // liv.2 Giorno-Fascia oraria
186400161130          when wLivDrill = 2;
186500161111           exec sql  fetch next  from C1  into :TIMAC_fetch2;
186600161130          // liv.1 Giorno
186700161130          when wLivDrill = 1;
186800161130           exec sql  fetch next  from C1  into :TIMAC_fetch1;
186900161130         endsl;
187000160901
187100160901         *in99 = *off;
187200160901         if SQLCode = 100 or SQLCode < 0;
187300160901           *in99 = *on;
187400160901         endif;
187500160831
187600160831       ENDSR;
187700160831
187800160831       /end-free
187900160831
188000970526     C*------------------------------------------------------------------------*
188100970526     C* NOTE COSTRUZIONE SCHIERE
188200970526     C*------------------------------------------------------------------------*
188300970526     C*
188400970526     C* -MSG-MESSAGGI
188500970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
188600970526     C* (NON TOCCARE MSG,1 E MSG,2)
188700970526     C*
188800970526     C* -CMD-COMANDI
188900970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
189000970526     C*
189100970526     C* -OPD-OPZIONI
189200970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
189300970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
189400970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
189500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
189600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
189700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
189800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
189900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
190000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
190100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
190200970526     C*
190300970526     C* -FUD-FUNZIONI
190400970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
190500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
190600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
190700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
190800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
190900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
191000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
191100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
191200970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
191300970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
191400970526     O*------------------------------------------------------------------------*
191500970526** DEC - DECODIFICHE
191600970526    GESTIONE                                                     1
191700970526INTERROGAZIONE                                                   2
191800970530     SCELTA                                                      3
1919009705264
1920009705265
192100970526** MSG - MESSAGGI
192200161108TRBMMAC1R- Opzione non gestita                                                 1
192300161108TRBMMAC1R- Tasto funzionale non gestito                                        2
192400161108TRBMMAC1R-
192500161108TRBMMAC1R- Record annullato.                                                   4
192600161108TRBMMAC1R- Record non annullato. Impossibile ripristinare                      5
192700161108TRBMMAC1R- Accettato solo un parametro di inizio lista                         6
192800161108TRBMMAC1R- Errori nei parametri di ingresso                                    7
192900161108TRBMMAC1R- Data errata                                                         8
193000161108TRBMMAC1R-                                                                     9
193100161108TRBMMAC1R- Errore SQL nel reperimento dati Validi al                          10
193200161108TRBMMAC1R- Copia totale (opz. 23) eseguita correttamente                      11
193300161108TRBMMAC1R- Copia totale (opz. 23) non eseguita                                12
193400161130TRBMMAC1R- Non esiste un livello superiore per il DrillUp                     13
193500161130TRBMMAC1R- Non esiste un livello inferiore per il DrillDown                   14
193600161108TRBMMAC1R- 15                                                                 15
193700161108TRBMMAC1R- 16                                                                 16
193800161108TRBMMAC1R- 17                                                                 17
193900161108TRBMMAC1R- 18                                                                 18
194000161108TRBMMAC1R- 19                                                                 19
194100161108TRBMMAC1R- 20                                                                 20
194200970526** CMD - COMANDI CL
194300161108
194400970526
194500970526
194600970526
194700970526
194800970526** OPD - OPZIONI
194900160923 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
195000970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
195100160914 3=Copia            3 -     (S/N)=N                        opz. 03¬
195200970526 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
195300970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
195400160713                   -  -     (S/N)=N                        opz. 06¬
195500160713                   -  -     (S/N)=N                        opz. 07¬
195600160713                   -  -     (S/N)=N                        opz. 08¬
195700160713                   -  -     (S/N)=N                        opz. 09¬
195800160713                   -  -     (S/N)=N                        opz. 10¬
195900160713                   -  -     (S/N)=N                        opz. 11¬
196000970526                   -  -     (S/N)=N                        opz. 12¬
196100161108                   -  -     (S/N)=N                        opz. 13¬
196200970526                   -  -     (S/N)=N                        opz. 14¬
196300970526                   -  -     (S/N)=N                        opz. 15¬
196400970526                   -  -     (S/N)=N                        opz. 16¬
196500970526                   -  -     (S/N)=N                        opz. 17¬
196600970526                   -  -     (S/N)=N                        opz. 18¬
196700970526                   -  -     (S/N)=N                        opz. 19¬
196800970526                   -  -     (S/N)=N                        opz. 20¬
196900970526                   -  -     (S/N)=N                        opz. 21¬
197000970526                   -  -     (S/N)=N                        opz. 22¬
197100161108                   -  -     (S/N)=N                        opz. 23¬
197200970526                   -  -     (S/N)=N                        opz. 24¬
197300970526** FUD - FUNZIONI
197400970526                   -        (S/N)=N                        funz.01¬
197500970526                   -        (S/N)=N                        funz.02¬
197600970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
197700970526                   -        (S/N)=N                        funz.04¬
197800970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
197900970526F06=Inserimento    -        (S/N)=N                        funz.06¬
198000161130F07=Drill-down     -        (S/N)=S                        funz.07¬
198100161111F08=Drill-up       -        (S/N)=S                        funz.08¬
198200161115F09=Comparazione   -        (S/N)=S                        funz.09¬
198300160713                   -        (S/N)=N                        funz.10¬
198400160713                   -        (S/N)=N                        funz.11¬
198500160923F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
198600160713                   -        (S/N)=N                        funz.13¬
198700970603                   -        (S/N)=S                        funz.14¬
198800970526                   -        (S/N)=N                        funz.15¬
198900970526                   -        (S/N)=N                        funz.16¬
199000970526                   -        (S/N)=N                        funz.17¬
199100970526                   -        (S/N)=N                        funz.18¬
199200970627 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
199300970627 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
199400970526                   -        (S/N)=N                        funz.21¬
199500160713                   -        (S/N)=S                        funz.22¬
199600970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
199700970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
