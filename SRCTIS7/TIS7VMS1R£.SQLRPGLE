000100970526     H*------------------------------------------------------------------------*
000200970526     H DECEDIT('0,') DATEDIT(*DMY.)
000300970526     F*------------------------------------------------------------------------*
000400970526     F* DATA BASE
000500970526     F*------------------------------------------------------------------------*
000600160712     FTIS7VMS1D CF   E             WORKSTN INFDS(DEVDS1)
000700160712     F                                     SFILE(S7V1D05:SFLULT)
000800160712     F                                     SFILE(S7V1DMS:MSGULT)
000900161202     F*------------
001000970526     F* VERSIONI
001100970526     F*------------
001200160831     FTIVMS04I  IF   E           K DISK    rename(TIVMS000:TIVMS04)
001300160830     FTIVMS01I  IF   E           K DISK    rename(TIVMS000:TIVMS01)
001400970526     D*------------------------------------------------------------------------*
001500970526     D* SCHIERE
001600970526     D*------------------------------------------------------------------------*
001700970526     D*------------
001800970526     D* DECODIFICHE
001900970526     D*------------
002000970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002100970526     D*------------
002200970526     D* MESSAGGI
002300970526     D*------------
002400970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002500970526     D*------------
002600970526     D* COMANDI
002700970526     D*------------
002800970526     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
002900970526     D*------------
003000970526     D* DESCRIZIONI OPZIONI
003100970526     D*------------
003200970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003300970526     D*------------
003400970526     D* DESCRIZIONI TASTI FUNZIONALI
003500970526     D*------------
003600970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003700970526     D*------------
003800970526     D* OPZIONI DA CARICARE A VIDEO
003900970526     D*------------
004000970526     D OPV             S             19    DIM(24)
004100970526     D*------------
004200970526     D* FUNZIONI DA CARICARE A VIDEO
004300970526     D*------------
004400970526     D FUV             S             19    DIM(24)
004500970526     D*------------------------------------------------------------------------*
004600970526     D* INPUT
004700970526     D*------------------------------------------------------------------------*
004800970526     D*------------
004900970526     D* RIDEFINIZIONE OPZIONI VIDEO
005000970526     D*------------
005100970526     D OPDDS           DS
005200970526     D  OPDOPZ                 1      2
005300970526     D  OPDDE1                 3     19
005400970526     D  OPDOP1                21     22
005500970526     D  OPDFI0                23     34
005600970526     D  OPDATT                35     35
005700970526     D  OPDFI1                36     80
005800970526     D  OPDDES                 1     19
005900970526     D  OPDRIG                 1     80
006000970526     D*------------
006100970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006200970526     D*------------
006300970526     D FUDDS           DS
006400970526     D  FUDDE1                 1      1
006500970526     D  FUDFUN                 2      3
006600970526     D  FUDDE2                 4     19
006700970526     D  FUDFI0                20     34
006800970526     D  FUDATT                35     35
006900970526     D  FUDFI1                36     80
007000970526     D  FUDDES                 1     19
007100970526     D  FUDRIG                 1     80
007200970526     D*------------
007300970526     D* DS GESTIONE TASTI FUNZIONALI
007400970526     D*------------
007500970526     D DSKEY         E DS
007600970526     D DEVDS1          DS
007700970526     D  KEY                  369    369
007800970526     D*------------
007900970526     D* DS "XMSG" - GESTIONE MESSAGGI
008000970526     D*------------
008100970526     D DSMSG         E DS
008200970526     D*------------
008300020919     D* DS REPERIMENTO DATI UTENTE
008400970526     D*------------
008500020919     D TIBS34DS      E DS                                                       *Profili utente
008600020919     D DDATIUTE      E DS                                                       *Dati utente
008700020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008800970526     D*------------------
008900970526     D* DS "XSRDA8" - CONTROLLA DATA (8)
009000970526     D*------------------
009100970526     D WLBDA8          DS                  INZ
009200970526     D  G08DAT                 1      8  0
009300970526     D  G08INV                 9     16  0
009400970526     D  G08ERR                17     17
009500970526     D  G08TGI                18     22  0
009600970526     D*------------
009700161123     D* DS DI PROCEDURA
009800970526     D*------------
009900160713     D TIS7VMS2DS    E DS
010000160714     D WTIS7VMS2DS     DS                  LIKEDS(TIS7VMS2DS)
010100160927     D TIS7VMS5DS    E DS
010200161123     d Param6DS        ds                  inz qualified
010300161123     d  Proc                         10
010400161123     d  KSU                           8
010500161123     d  DDE                           8s 0
010600161123     d  CauBRT                       15
010700161123     d  StsCli                       15
010800161123     d  DrillUp                        n
010900161123     d  Squad                          n
011000970526     D*------------
011100970526     D* ARCHITETTURA
011200970526     D*------------
011300970526     D KPJBA         E DS
011400970715     D  UTENTE                 1      3
011500160713     D*------------
011600160713     D* campi di appoggio
011700160713     D*------------
011800160713     D DATB            S                   like(VMSATB   )
011900160713     D DProc           S                   like(VMSProc  )
012000160713     D DKSU            S                   like(VMSKSU   )
012100160831     D DCauTip         S                   like(VMSCauTip)
012200160831     D DCauBRT         S                   like(VMSCauBRT)
012300160713     D DStsCli         S                   like(VMSStsCli)
012400160916     D DDDE            S              8s 0
012500160713     D DEPProc         S                   like(D03Proc  )
012600160713     D DEPKSU          S                   like(D03KSU   )
012700160913     D DEPDDE          S                   like(D03DDE   )
012800160913     D DEPCAUBRT       S                   like(D03CAUBRT)
012900160926     D DEPSTSCLI       S                   like(D03STSCLI)
013000160919     D DEPDrill        S              1a   inz(*off)
013100161122     D DEPSquad        S              1a   inz(*off)
013200170207     D DEPConfr        S              1a   inz(*off)
013300160831     D*------------
013400160831     D* campi tipo data per le date a video
013500160831     D ValidISO        s               d
013600160913     D DDEISO          s               d
013700160831       // - Stringa SQL da eseguire
013800160831     d wSQl            s           1024    inz  varying
013900161122     d VMS_Drill       ds                  qualified
014000161122     d  Proc                         10
014100161122     d  KSU                           8
014200161122     d  Count                         9s 0
014300161122     d  DDE                           8s 0
014400161122     d  DSC                           8s 0
014500161122     d VMS_Squad       ds                  qualified
014600161122     d  Proc                         10
014700161122     d  KSU                           8
014800161122     d  CauTip                        3
014900161122     d  CauBRT                       15
015000161122     d  CauTT                        15
015100161122     d  StsCli                       15
015200161122     d  DDE                           8s 0
015300161122     d  DSC                           8s 0
015400161122     d  DescIT                       28
015500160831     d TIVMSOccurs     s              3s 0
015600170207     d VMS_Confr       ds                  qualified
015700170207     d  Proc                         10
015800170207     d  KSU                           8
015900170207     d  CauTip                        3
016000170207     d  CauBRT                       15
016100170207     d  CauTT                        15
016200170207     d  StsCli                       15
016300170207     d  DDE                           8s 0
016400170207     d  DSC                           8s 0
016500170207     d  DescIT                       28
016600160916
016700160916       // - Parametri chiamata TIS7VMS4R
016800160916       // opz: blank=elabora  C=chiudi
016900160916     d pIn_Opz_S4      s              1a
017000160916       // Procedura
017100160916     d pIn_Proc_S4     s                   like(VMSProc)
017200160916       // Cliente unificante
017300160916     d pIn_KSU_S4      s                   like(VMSKSU)
017400160916       // Data decorrenza
017500160916     d pIn_DDE_S4      s              8s 0
017600160916       // Tipo causale
017700160916     d pIn_CauTip_S4   s                   like(VMSCAUTIP)
017800160916       // Causale BRT
017900160916     d pIn_CauBRT_S4   s                   like(VMSCAUBRT)
018000160916       // Data scadenza
018100160916     d pIn_DSC_S4      s              8s 0
018200160916       // esito: 1=OK  I=Err parametri input  2=KO SQL  3=non trovato periodo  4=trovati + periodi
018300160916     d pOut_Esito_S4   s              1a
018400160916
018500160916     d wDrillUp        s               n   inz(*off)
018600161122     d wSquad          s               n   inz(*off)
018700170207     d wConfr          s               n   inz(*off)
018800161205     D wD03CauBRT      S                   like(D03CauBRT)
018900161205     D wD03StsCli      S                   like(D03StsCli)
019000161205     d wDummy          s              1
019100170207
019200170207        // parametri TIS7VMS8R
019300170207     d Exfmt           s               n   inz(*off)
019400170207     d Proc1           s                   like(D03Proc)
019500170207     d Proc2           s                   like(D05Proc)
019600170207     D ExecSQL         S               n   inz(*off)
019700970526     C*------------------------------------------------------------------------*
019800970526     C* MAIN LINES
019900970526     C*------------------------------------------------------------------------*
020000970526     C*
020100970526     C* SE NON CI SONO ERRORI, PROSEGUE
020200970526IF  1C     ERRL00        IFNE      '1'
020300970526     C*
020400970526     C* CARICA IL SFILE
020500970526     C                   EXSR      POSSFL
020600970526     C*
020700970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
020800970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
020900970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
021000970526     C     FINL00        OREQ      '1'                                          *FINE PGM
021100970526     C*
021200970526     C* REIMPOSTA ERRORE DI PROCERURA
021300970526     C                   MOVEL     '0'           ERRL00
021400970526     C*
021500970526     C* EMETTE IL SFILE
021600970526     C                   EXSR      OUTSFL
021700970526     C*
021800970526     C* GESTIONE TASTI FUNZIONALI
021900970526     C                   EXSR      FUNCON
022000970526     C*
022100970526E   2C                   ENDDO
022200970526E   1C                   ENDIF
022300970526     C* FINE PGM
022400970526     C                   SETON                                        LR
022500970526     C*------------------------------------------------------------------------*
022600970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
022700970526     C*------------------------------------------------------------------------*
022800970526     C     F03GES        BEGSR
022900970526     C*
023000970526     C* IMPOSTA "PREMUTO F03"
023100970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
023200160713     C                   MOVEL     '1'           VMS2F03                         *DI PROCEDURA
023300160916     C*
023400970526     C                   ENDSR
023500970526     C*------------------------------------------------------------------------*
023600970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
023700970526     C*------------------------------------------------------------------------*
023800970526     C     F05GES        BEGSR
023900160923     C*
024000160923     C* se il Drill-up è attivo, con L'F5 lo disattivo
024100161123     C* se la ricerca Squadrature è attivo, con L'F5 lo disattivo
024200161123     C*
024300160923     C* disattivo il Drill-up
024400160923     C                   eval      wDrillUp = *off
024500161123     C* disattivo la ricerca squadrature
024600161123     C                   eval      wSquad   = *off
024700160923     C* siccome con l'F5 non ctrl il posizionamento, la variabile di appoggio la devo valorizzare
024800160923     C                   EVAL      DEPDrill   = wDrillUp
024900161123     C                   EVAL      DEPSquad   = wSquad
025000160923     C* gli chiedo di caricare tutto
025100160923     C                   MOVEL     'ALL'         WTIPRI
025200160923     C* cambio il tasto di funzione in Drill-up
025300160923     c                   eval      FUDRig = FUD(8)
025400160923     C                   MOVEL     'S'           FUDATT
025500160923     c                   eval      %subst(FUDRig:1:19) = 'F08=Drill-up'
025600160923     c                   eval      FUD(8) = FUDRig
025700161123     C* cambio il tasto di funzione in Squadrature
025800161123     c                   eval      FUDRig = FUD(15)
025900161123     C                   MOVEL     'S'           FUDATT
026000161123     c                   eval      %subst(FUDRig:1:19) = 'F15=Squadrature'
026100161123     c                   eval      FUD(15) = FUDRig
026200160923     C*
026300160923     c                   eval      FUNGru = 0
026400160923     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
026500970526     C*
026600970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
026700970526     C*
026800970526     C                   ENDSR
026900970526     C*------------------------------------------------------------------------*
027000970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
027100970526     C*------------------------------------------------------------------------*
027200970526     C     F06GES        BEGSR
027300970526     C*
027400970526     C* INSERIMENTO CONSENTITO SOLO IN SEDE
027500020919IF  1C     DUTLPO        IFNE      'S'
027600970526     C                   MOVEL     '1'           ERRL00
027700970526     C                   MOVEL     MSG(3)        DSMSMS
027800970526E   1C                   ENDIF
027900970526     C*
028000970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
028100160713     C                   CLEAR                   DATB
028200160713     C                   CLEAR                   DPROC
028300160713     C                   CLEAR                   DKSU
028400160831     C                   CLEAR                   DCAUTip
028500160831     C                   CLEAR                   DCAUBRT
028600160713     C                   CLEAR                   DSTSCLI
028700160713     C                   CLEAR                   DDDE
028800970529     C                   MOVEL     DEPOP0        WOP0
028900970529     C                   MOVEL     'F06'         WOP1
029000160714     C                   MOVEL     'TIS7VMS2R'   WPGM             10
029100970526     C                   EXSR      LANPGM                                       *LANCIA PGM
029200970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
029300970526E   1C                   ENDIF
029400970526     C*
029500970526     C                   ENDSR
029600160916     C*------------------------------------------------------------------------*
029700160923     C* F08GES - GESTIONE F08 -> Drill-up/down
029800160916     C*------------------------------------------------------------------------*
029900160916     C     F08GES        BEGSR
030000160916     C*
030100160923     C                   if        wDrillUp = *off
030200160916     C* attivo il Drill-up
030300160916     C                   eval      wDrillUp = *on
030400160923     C* cambio il tasto di funzione in Drill-down
030500160923     c                   eval      FUDRig = FUD(8)
030600160923     C                   MOVEL     'S'           FUDATT
030700160923     c                   eval      %subst(FUDRig:1:19) = 'F08=Drill-down'
030800160923     c                   eval      FUD(8) = FUDRig
030900161123     C* disattivo la ricerca squadrature
031000161123     C                   eval      wSquad   = *off
031100161123     C* cambio il tasto di funzione in Squadrature
031200161123     c                   eval      FUDRig = FUD(15)
031300161123     C                   MOVEL     'S'           FUDATT
031400161123     c                   eval      %subst(FUDRig:1:19) = 'F15=Squadrature'
031500161123     c                   eval      FUD(15) = FUDRig
031600160923     c                   else
031700160923     C* disattivo il Drill-up
031800160923     C                   eval      wDrillUp = *off
031900160923     C* cambio il tasto di funzione in Drill-up
032000160923     c                   eval      FUDRig = FUD(8)
032100160923     C                   MOVEL     'S'           FUDATT
032200160923     c                   eval      %subst(FUDRig:1:19) = 'F08=Drill-up'
032300160923     c                   eval      FUD(8) = FUDRig
032400160923     c                   endif
032500160923     C*
032600160923     c                   eval      FUNGru = 0
032700160923     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
032800160916     C*
032900160916     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
033000160916     C                   EXSR      NEWPOS
033100160916     C*
033200160916     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
033300160916IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033400160916IF  2C     SWPOSI        IFNE      0
033500160916     C                   EXSR      POSSFL                                       *RICARICA SFILE
033600160916X   2C                   ELSE
033700160916IF  3C     SFLULT        IFNE      0
033800160916     C                   EXSR      ENTATT                                       *GESTIONE ENTER
033900160916     C*
034000160916     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
034100160916IF  4C     SWMODI        IFNE      0
034200160916     C                   EXSR      POSSFL
034300160916E   4C                   ENDIF
034400160916E   3C                   ENDIF
034500160916E   2C                   ENDIF
034600160916E   1C                   ENDIF
034700160919     C*
034800160916     C                   ENDSR
034900970526     C*------------------------------------------------------------------------*
035000970526     C* F12GES - GESTIONE F12 -> RITORNO
035100970526     C*------------------------------------------------------------------------*
035200970526     C     F12GES        BEGSR
035300970526     C*
035400970526     C* IMPOSTA "PREMUTO F12"
035500970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
035600160713     C                   MOVEL     '1'           VMS2F12                         *DI PROCEDURA
035700970526     C*
035800970526     C                   ENDSR
035900161121     C*------------------------------------------------------------------------*
036000170208     C* F15GES - GESTIONE F15 -> SQUADRATURE
036100161121     C*------------------------------------------------------------------------*
036200161121     C     F15GES        BEGSR
036300161121     C*
036400161122     C                   if        wSquad = *off
036500161122     C* attivo le squadrature
036600161122     C                   eval      wSquad = *on
036700161122     C* cambio il tasto di funzione in Normale
036800170208     c                   eval      FUDRig = FUD(15)
036900161122     C                   MOVEL     'S'           FUDATT
037000161122     c                   eval      %subst(FUDRig:1:19) = 'F15=Normale'
037100161122     c                   eval      FUD(15) = FUDRig
037200161123     C* disattivo il Drill-up
037300161123     C                   eval      wDrillUp = *off
037400161123     C* cambio il tasto di funzione in Drill-up
037500161123     c                   eval      FUDRig = FUD(8)
037600161123     C                   MOVEL     'S'           FUDATT
037700161123     c                   eval      %subst(FUDRig:1:19) = 'F08=Drill-up'
037800161123     c                   eval      FUD(8) = FUDRig
037900161122     c                   else
038000161122     C* disattivo la ricerca squadrature
038100161122     C                   eval      wSquad   = *off
038200161122     C* cambio il tasto di funzione in Squadrature
038300161122     c                   eval      FUDRig = FUD(15)
038400161122     C                   MOVEL     'S'           FUDATT
038500161122     c                   eval      %subst(FUDRig:1:19) = 'F15=Squadrature'
038600161122     c                   eval      FUD(15) = FUDRig
038700161122     c                   endif
038800161122     C*
038900161122     c                   eval      FUNGru = 0
039000161122     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
039100161122     C*
039200161122     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
039300161122     C                   EXSR      NEWPOS
039400161122     C*
039500161122     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
039600161122IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
039700161122IF  2C     SWPOSI        IFNE      0
039800161122     C                   EXSR      POSSFL                                       *RICARICA SFILE
039900161122X   2C                   ELSE
040000161122IF  3C     SFLULT        IFNE      0
040100161122     C                   EXSR      ENTATT                                       *GESTIONE ENTER
040200161122     C*
040300161122     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
040400161122IF  4C     SWMODI        IFNE      0
040500161122     C                   EXSR      POSSFL
040600161122E   4C                   ENDIF
040700161122E   3C                   ENDIF
040800161122E   2C                   ENDIF
040900161122E   1C                   ENDIF
041000161121     C*
041100161121     C                   ENDSR
041200170208     C*------------------------------------------------------------------------*
041300170208     C* F16GES - GESTIONE F16 CONFRONTA
041400170208     C*------------------------------------------------------------------------*
041500170208     C     F16GES        BEGSR
041600170208     C*
0417001702081    C                   if        wConfr = *off
041800170208
041900170208     C* emetto finestra per conferma richiesta confronto e tra chi
042000170208
042100170208     C* siccome questo pgm si chiama tramite un comando, è ovvio che ci sia stata una exfmt del sfl
042200170208     C                   EVAL      Exfmt = *on
042300170208     C* richiedi procedura da confrontare
042400170208     C                   CALL      'TIS7VMS8R'
042500170208     C                   PARM                    Kpjba
042600170208     C                   PARM                    Exfmt
042700170208     C                   PARM                    Proc1
042800170208     C                   PARM                    Proc2
042900170208     C                   PARM                    ExecSQL
043000170208     C*
043100170208     C* tolgo memo che c'è stata una exfmt del sfl
043200170208     C                   EVAL      Exfmt = *off
043300170208     C* CONFRONTA
0434001702082    C                   IF        ExecSQL = *on
043500170208     C* attivo il confrontore
043600170208     C                   eval      wConfr = *on
043700170208     C* cambio il tasto di funzione in Lista
043800170208     c                   eval      FUDRig = FUD(16)
043900170208     C                   MOVEL     'S'           FUDATT
044000170208     c                   eval      %subst(FUDRig:1:13) = 'F16=Lista    '
044100170208     c                   eval      FUD(16) = FUDRig
0442001702082e   c                   endif
044300170208
0444001702081x   c                   else
044500170208     C* disattivo il confronto
044600170208     C                   eval      wConfr   = *off
044700170208     C* cambio il tasto di funzione in Squadrature
044800170208     c                   eval      FUDRig = FUD(16)
044900170208     C                   MOVEL     'S'           FUDATT
045000170208     c                   eval      %subst(FUDRig:1:13) = 'F16=Confronto'
045100170208     c                   eval      FUD(16) = FUDRig
0452001702081e   c                   endif
045300170208     C*
045400170208     c                   eval      FUNGru = 0
045500170208     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
045600170208     C*
045700170208     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
045800170208     C                   EXSR      NEWPOS
045900170208     C*
046000170208     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
0461001702081   1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
0462001702082   2C     SWPOSI        IFNE      0
046300170208     C                   EXSR      POSSFL                                       *RICARICA SFILE
0464001702082x  2C                   ELSE
0465001702083    C     SFLULT        IFNE      0
046600170208     C                   EXSR      ENTATT                                       *GESTIONE ENTER
046700170208     C*
046800170208     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
0469001702084    C     SWMODI        IFNE      0
047000170208     C                   EXSR      POSSFL
0471001702084e   C                   ENDIF
0472001702083e   C                   ENDIF
0473001702082e   C                   ENDIF
0474001702081e   C                   ENDIF
047500170208     C*
047600170208     C                   ENDSR
047700970526     C*------------------------------------------------------------------------*
047800161121     C* F21GES - GESTIONE F21 -> GESTIONE STAMPE
047900970526     C*------------------------------------------------------------------------*
048000161121     C     F21GES        BEGSR
048100970526     C*
048200161123     C                   eval      Param6DS.Proc   = D03Proc
048300161123     C                   eval      Param6DS.Ksu    = D03Ksu
048400161123     C                   if        DDEISO <> *loval
048500161123     C                   eval      Param6DS.DDE    = %dec(DDEISO)
048600161123     C                   else
048700161123     C                   eval      Param6DS.DDE    = 0
048800161123     C                   endif
048900161123     C                   eval      Param6DS.CauBRT = D03CauBRT
049000161123     C                   eval      Param6DS.StsCli = D03StsCli
049100161123     C                   eval      Param6DS.DrillUp = wDrillUp
049200161123     C                   eval      Param6DS.Squad   = wSquad
049300170207     C                   clear                   kpjbu
049400161123     C                   eval      kpjbu = Param6DS
049500970526     C*
049600161123     C                   call      'TIS7VMS6R'
049700161123     C                   parm                    kpjba
049800161123     C*
049900161123     C                   MOVEL     MSG(13)       DSMSMS
050000161123     C*
050100970526     C                   ENDSR
050200161121     C*------------------------------------------------------------------------*
050300161121     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
050400161121     C*------------------------------------------------------------------------*
050500161121     C     F22GES        BEGSR
050600161121     C*
050700161121     C                   Z-ADD     80            LENGH            15 5
050800161121     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
050900161121     C                   PARM                    CMD(1)
051000161121     C                   PARM                    LENGH
051100161121     C*
051200161121     C                   ENDSR
051300970526     C*------------------------------------------------------------------------*
051400970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
051500970526     C*------------------------------------------------------------------------*
051600970526     C     F23GES        BEGSR
051700970526     C*
051800970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
051900970526     C*
052000970526     C                   ENDSR
052100970526     C*------------------------------------------------------------------------*
052200970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
052300970526     C*------------------------------------------------------------------------*
052400970526     C     F24GES        BEGSR
052500970526     C*
052600970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
052700970526     C*
052800970526     C                   ENDSR
052900970526     C*------------------------------------------------------------------------*
053000970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
053100970526     C*------------------------------------------------------------------------*
053200970526     C     RUPGES        BEGSR
053300970526     C*
053400970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
053500970526     C*
053600970526     C                   ENDSR
053700970526     C*------------------------------------------------------------------------*
053800970526     C* ENTGES - GESTIONE ENTER -> INVIO
053900970526     C*------------------------------------------------------------------------*
054000970526     C     ENTGES        BEGSR
054100970526     C*
054200970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
054300970526     C                   EXSR      NEWPOS
054400970526     C*
054500970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
054600970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
054700970526IF  2C     SWPOSI        IFNE      0
054800970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
054900970526X   2C                   ELSE
055000970526IF  3C     SFLULT        IFNE      0
055100970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
055200970526     C*
055300970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
055400970526IF  4C     SWMODI        IFNE      0
055500970526     C                   EXSR      POSSFL
055600970526E   4C                   ENDIF
055700970526E   3C                   ENDIF
055800970526E   2C                   ENDIF
055900970526E   1C                   ENDIF
056000970526     C*
056100970526     C                   ENDSR
056200970526     C*------------------------------------------------------------------------*
056300970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
056400970526     C*------------------------------------------------------------------------*
056500970526     C     O01GES        BEGSR
056600970526     C*
056700160923     C* valorizzo Procedura, Unificante, Decorrenza con quelli del rcd scelto
056800160923     C* ed eseguo l'enter (in pratica vado in dettaglio)
056900160923     c                   eval      D03Proc = D05Proc
057000160923     c                   eval      D03Ksu  = D05Ksu
057100160923     c                   eval      D03DDE  = D05DDE
057200160923     C*
057300160923     C* se il Drill-up è attivo, lo disattivo
057400160923     C                   if        wDrillUp = *on
057500160923     C* disattivo il Drill-up
057600160923     C                   eval      wDrillUp = *off
057700160923     C* cambio il tasto di funzione in Drill-up
057800160923     c                   eval      FUDRig = FUD(8)
057900160923     C                   MOVEL     'S'           FUDATT
058000160923     c                   eval      %subst(FUDRig:1:19) = 'F08=Drill-up'
058100160923     c                   eval      FUD(8) = FUDRig
058200160923     C*
058300160923     c                   eval      FUNGru = 0
058400160923     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
058500160923     C*
058600160923     C* simulo l'enter
058700160923     C                   exsr      NEWPOS
058800160923     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
058900160923     C*
059000160923     c                   endif
059100160923     C*
059200970526     C                   ENDSR
059300970526     C*------------------------------------------------------------------------*
059400970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
059500970526     C*------------------------------------------------------------------------*
059600970526     C     O02GES        BEGSR
059700160927     C*
059800160927     C* se il Drill-up è attivo, il pgm da chiamare è un altro
059900160927     C                   if        wDrillUp = *on
060000160927     C                   exsr      O02GesDrillUp
060100160927     c                   leavesr
060200160927     c                   endif
060300970526     C*
060400970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
060500160712IF  1C     D05ATB        IFEQ      'A'
060600970526     C                   MOVEL     '1'           ERRL00
060700970526     C                   MOVEL     MSG(4)        DSMSMS
060800970526E   1C                   ENDIF
060900970526     C* MODIFICA
061000970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
061100160713     C                   EVAL      DATB        = D05ATB
061200160713     C                   EVAL      DProc       = D05Proc
061300160713     C                   EVAL      DKSU        = D05KSU
061400160831     C                   EVAL      DCauTip     = D05CauTip
061500160831     C                   EVAL      DCauBRT     = D05CauBRT
061600160713     C                   EVAL      DStsCli     = D05StsCli
061700160914     C                   EVAL      DDDE        = %dec(%date(D05DDE:*dmy))
061800970529     C                   MOVEL     DEPOP0        WOP0
061900970526     C                   MOVEL     'O02'         WOP1
062000160715     C                   MOVEL     'TIS7VMS2R'   WPGM
062100970526     C                   EXSR      LANPGM
062200970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
062300970526E   1C                   ENDIF
062400970526     C*
062500160927     C                   ENDSR
062600160927     C*
062700160914     C*------------------------------------------------------------------------*
062800160927     C* O02GesDrillUp - GESTIONE OPZIONE 2 per DrillUp
062900160914     C*------------------------------------------------------------------------*
063000160927     C     O02GesDrillUp BEGSR
063100160914     C*
063200160927     C* IMPOSTA LA DS DI PROCEDURA
063300160927     C                   MOVEL     '2'           VMS5Rich                        *Richiesta
063400160927     C                   MOVEL     '0'           VMS2F03                         *PREMUTO F03=FINE
063500160927     C                   MOVEL     '0'           VMS2F12                         *PREMUTO F12=RIT
063600160927     C                   MOVEL     '0'           VMS2ERR                         *'1'=ERRORE
063700160927     C                   MOVEL     *BLANKS       VMS2MSG                         *MESSAGGIO DI RIT
063800160927     C                   EVAL      VMS5Proc   = D05Proc
063900160927     C                   EVAL      VMS5KSU    = D05KSU
064000160927     C                   EVAL      VMS5DDE    = %dec(%date(D05DDE:*dmy))
064100160927     C                   EVAL      VMS5DSC    = %dec(%date(D05DSC:*dmy))
064200160927     C*
064300160927     C                   CALL      'TIS7VMS5R'
064400160927     C                   PARM                    KPJBA
064500160927     C                   PARM                    TIS7VMS5DS
064600160927     C*
064700160927     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
064800160927IF  1C     VMS5F03       IFEQ      '1'                                          *PREMUTO F03
064900160927     C                   MOVEL     '1'           ERRL00                         *ERRORE
065000160927     C                   MOVEL     '1'           F03L00                         *FINE PGM
065100160927X   1C                   ELSE                                                   *NON PREMUTO F03
065200160927     C                   MOVEL     VMS5MSG       DSMSMS                         *MESSAGGIO DI RIT
065300160927E   1C                   ENDIF
065400160927     C*
065500160927     C                   ENDSR
065600160927     C*------------------------------------------------------------------------*
065700160927     C* O03GES - GESTIONE OPZIONE 3=DUPLICA
065800160927     C*------------------------------------------------------------------------*
065900160927     C     O03GES        BEGSR
066000160927     C*
066100160914     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
066200160914IF  1C     D05ATB        IFEQ      'A'
066300160914     C                   MOVEL     '1'           ERRL00
066400160914     C                   MOVEL     MSG(4)        DSMSMS
066500160914E   1C                   ENDIF
066600160914     C* COPIA
066700160914IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
066800160914     C                   EVAL      DATB        = D05ATB
066900160914     C                   EVAL      DProc       = D05Proc
067000160914     C                   EVAL      DKSU        = D05KSU
067100160914     C                   EVAL      DCauTip     = D05CauTip
067200160914     C                   EVAL      DCauBRT     = D05CauBRT
067300160914     C                   EVAL      DStsCli     = D05StsCli
067400160914     C                   EVAL      DDDE        = %dec(%date(D05DDE:*dmy))
067500160914     C                   MOVEL     DEPOP0        WOP0
067600160914     C                   MOVEL     'O03'         WOP1
067700160914     C                   MOVEL     'TIS7VMS2R'   WPGM
067800160914     C                   EXSR      LANPGM
067900160914     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
068000160914E   1C                   ENDIF
068100160914     C*
068200160914     C                   ENDSR
068300970603     C*------------------------------------------------------------------------*
068400160914     C* O13GES - GESTIONE OPZIONE 13=DUPLICA
068500970603     C*------------------------------------------------------------------------*
068600160914     C     O13GES        BEGSR
068700970603     C*
068800970603     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
068900160713IF  1C     D05ATB        IFEQ      'A'
069000160713     C                   MOVEL     '1'           ERRL00
069100160713     C                   MOVEL     MSG(4)        DSMSMS
069200160713E   1C                   ENDIF
069300970603     C* DUPLICA
069400970603IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
069500160713     C                   EVAL      DATB        = D05ATB
069600160713     C                   EVAL      DProc       = D05Proc
069700160713     C                   EVAL      DKSU        = D05KSU
069800160831     C                   EVAL      DCauTip     = D05CauTip
069900160713     C                   EVAL      DCauBRT     = D05CauBRT
070000160713     C                   EVAL      DStsCli     = D05StsCli
070100160914     C                   EVAL      DDDE        = %dec(%date(D05DDE:*dmy))
070200970603     C                   MOVEL     DEPOP0        WOP0
070300970603     C                   MOVEL     'O02'         WOP1
070400160715     C                   MOVEL     'TIS7VMS3R'   WPGM
070500970603     C                   EXSR      LANPGM
070600970603     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
070700970603E   1C                   ENDIF
070800970603     C*
070900970603     C                   ENDSR
071000160915     C*------------------------------------------------------------------------*
071100160915     C* O23GES - GESTIONE OPZIONE 23=COPIA SU TUTTE
071200160915     C*------------------------------------------------------------------------*
071300160915     C     O23GES        BEGSR
071400160915     C*
071500160915     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
071600160915IF  1C     D05ATB        IFEQ      'A'
071700160915     C                   MOVEL     '1'           ERRL00
071800160915     C                   MOVEL     MSG(4)        DSMSMS
071900160915E   1C                   ENDIF
072000160915     C* copia su tutte le procedure/clienti unificante
072100160915IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
072200160916     C                   EVAL      pIn_Proc_S4   = D05Proc
072300160916     C                   EVAL      pIn_KSU_S4    = D05KSU
072400160916     C                   EVAL      pIn_DDE_S4    = %dec(%date(D05DDE:*dmy))
072500160916     C                   EVAL      pIn_CauTip_S4 = D05CauTip
072600160916     C                   EVAL      pIn_CauBRT_S4 = D05CauBRT
072700160916     C                   EVAL      pIn_DSC_S4    = %dec(%date(D05DSC:*dmy))
072800160916     c                   call      'TIS7VMS4R'
072900160916     c                   parm      *blank        pIn_Opz_S4
073000160916     c                   parm                    pIn_Proc_S4
073100160916     c                   parm                    pIn_KSU_S4
073200160916     c                   parm                    pIn_DDE_S4
073300160916     c                   parm                    pIn_CauTip_S4
073400160916     c                   parm                    pIn_CauBRT_S4
073500160916     c                   parm                    pIn_DSC_S4
073600160916     c                   parm                    pOut_Esito_S4
073700160916
073800160916     c                   if        pOut_Esito_S4 <> '1'
073900160916     C                   MOVEL     MSG(12)       DSMSMS
074000160916     C                   MOVEL     '1'           ERRL00
074100160916     c                   else
074200160916     C                   MOVEL     MSG(11)       DSMSMS
074300160915     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
074400160916     c                   endif
074500160916
074600160915E   1C                   ENDIF
074700160915     C*
074800160915     C                   ENDSR
074900970526     C*------------------------------------------------------------------------*
075000970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
075100970526     C*------------------------------------------------------------------------*
075200970526     C     O04GES        BEGSR
075300160927     C*
075400160927     C* se il Drill-up è attivo, il pgm da chiamare è un altro
075500160927     C                   if        wDrillUp = *on
075600160927     C                   exsr      O04GesDrillUp
075700160927     c                   leavesr
075800160927     c                   endif
075900160902     C*
076000160902     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
076100160902IF  1C     D05ATB        IFEQ      'A'
076200160902     C                   MOVEL     '1'           ERRL00
076300160902     C                   MOVEL     MSG(4)        DSMSMS
076400160902E   1C                   ENDIF
076500970526     C*
076600970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
076700160902IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
076800160713     C                   EVAL      DATB        = D05ATB
076900160713     C                   EVAL      DProc       = D05Proc
077000160713     C                   EVAL      DKSU        = D05KSU
077100160831     C                   EVAL      DCauTip     = D05CauTip
077200160713     C                   EVAL      DCauBRT     = D05CauBRT
077300160713     C                   EVAL      DStsCli     = D05StsCli
077400160914     C                   EVAL      DDDE        = %dec(%date(D05DDE:*dmy))
077500970529     C                   MOVEL     DEPOP0        WOP0
077600970526     C                   MOVEL     'O04'         WOP1
077700160714     C                   MOVEL     'TIS7VMS2R'   WPGM
077800970526     C                   EXSR      LANPGM
077900160919     C***                Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
078000970526E   1C                   ENDIF
078100970526     C*
078200970526     C                   ENDSR
078300160927     C*------------------------------------------------------------------------*
078400160927     C* O04GesDrillUp - GESTIONE OPZIONE 4 per DrillUp
078500160927     C*------------------------------------------------------------------------*
078600160927     C     O04GesDrillUp BEGSR
078700160927     C*
078800160927     C* IMPOSTA LA DS DI PROCEDURA
078900160927     C                   MOVEL     '4'           VMS5Rich                        *Richiesta
079000160927     C                   MOVEL     '0'           VMS2F03                         *PREMUTO F03=FINE
079100160927     C                   MOVEL     '0'           VMS2F12                         *PREMUTO F12=RIT
079200160927     C                   MOVEL     '0'           VMS2ERR                         *'1'=ERRORE
079300160927     C                   MOVEL     *BLANKS       VMS2MSG                         *MESSAGGIO DI RIT
079400160927     C                   EVAL      VMS5Proc   = D05Proc
079500160927     C                   EVAL      VMS5KSU    = D05KSU
079600160927     C                   EVAL      VMS5DDE    = %dec(%date(D05DDE:*dmy))
079700160927     C                   EVAL      VMS5DSC    = %dec(%date(D05DSC:*dmy))
079800160927     C*
079900160927     C                   CALL      'TIS7VMS5R'
080000160927     C                   PARM                    KPJBA
080100160927     C                   PARM                    TIS7VMS5DS
080200160927     C*
080300160927     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
080400160927IF  1C     VMS5F03       IFEQ      '1'                                          *PREMUTO F03
080500160927     C                   MOVEL     '1'           ERRL00                         *ERRORE
080600160927     C                   MOVEL     '1'           F03L00                         *FINE PGM
080700160927X   1C                   ELSE                                                   *NON PREMUTO F03
080800160927     C                   MOVEL     VMS5MSG       DSMSMS                         *MESSAGGIO DI RIT
080900160927E   1C                   ENDIF
081000160927     C*
081100160927     C                   ENDSR
081200970526     C*------------------------------------------------------------------------*
081300970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
081400970526     C*------------------------------------------------------------------------*
081500970526     C     O05GES        BEGSR
081600970526     C*
081700160713     C                   EVAL      DATB        = D05ATB
081800160713     C                   EVAL      DProc       = D05Proc
081900160713     C                   EVAL      DKSU        = D05KSU
082000160831     C                   EVAL      DCauTip     = D05CauTip
082100160713     C                   EVAL      DCauBRT     = D05CauBRT
082200160713     C                   EVAL      DStsCli     = D05StsCli
082300160914     C                   EVAL      DDDE        = %dec(%date(D05DDE:*dmy))
082400970529     C                   MOVEL     DEPOP0        WOP0
082500970526     C                   MOVEL     'O05'         WOP1
082600160714     C                   MOVEL     'TIS7VMS2R'   WPGM
082700970526     C                   EXSR      LANPGM
082800970526     C*
082900970526     C                   ENDSR
083000970526     C*------------------------------------------------------------------------*
083100970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
083200970526     C*------------------------------------------------------------------------*
083300970526     C     O07GES        BEGSR
083400970526     C*
083500970526     C*
083600970526     C                   ENDSR
083700970526     C*------------------------------------------------------------------------*
083800970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
083900970526     C*------------------------------------------------------------------------*
084000970526     C     NEWPOS        BEGSR
084100970526     C*
084200970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
084300970526     C*         1= RICHIESTO POSIZIONAMENTO
084400970526     C                   Z-ADD     0             SWPOSI            7 0
084500970526     C*
084600970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
084700970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
084800970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
084900970526     C                   Z-ADD     0             SWMODI            1 0
085000970526     C*
085100970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
085200160713IF  1C     D03Proc       IFNE      DEPProc
085300160713     C     D03KSU        ORNE      DEPKSU
085400160913     C     D03DDE        ORNE      DEPDDE
085500160913     C     D03CAUBRT     ORNE      DEPCAUBRT
085600160926     C     D03STSCLI     ORNE      DEPSTSCLI
085700160919     C     wDrillUp      ORNE      DEPDrill
085800161122     C     wSquad        ORNE      DEPSquad
085900170207     C     wConfr        ORNE      DEPConfr
086000970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
086100970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
086200970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
086300160713     C                   EVAL      DEPProc = D03Proc
086400160713     C                   EVAL      DEPKSU  = D03KSU
086500160913     C                   EVAL      DEPDDE  = D03DDE
086600160913     C                   EVAL      DEPCAUBRT  = D03CAUBRT
086700160926     C                   EVAL      DEPSTSCLI  = D03STSCLI
086800160919     C                   EVAL      DEPDrill   = wDrillUp
086900161122     C                   EVAL      DEPSquad   = wSquad
087000170207     C                   EVAL      DEPConfr   = wConfr
087100970526E   2C                   ENDIF
087200970526E   1C                   ENDIF
087300970526     C*
087400970526     C                   ENDSR
087500970526     C*------------------------------------------------------------------------*
087600970526     C* SETFIL - POSIZIONAMENTO RECORD
087700970526     C*------------------------------------------------------------------------*
087800970526     C     SETFIL        BEGSR
087900970526     C*
088000970526     C* POSIZIONAMENTO DEL FILE E PRIMA LETTURA IN BASE AL TIPO DI RICERCA
088100160830     C                   SELECT
088200160831IF  1C     WTIPRI        WHENEQ    'ALL'
088300160831     C     *loval        SETLL     TIVMS01I                           99
088400970526     C*
088500160831IF  1C     WTIPRI        WHENEQ    'PRO'
088600160831     C                   EVAL      VMSProc = D03Proc
088700160831     C     K01VMS01      SETLL     TIVMS01I                           99
088800160831     C*
088900160831IF  1C     WTIPRI        WHENEQ    'KSU'
089000160830     C                   EVAL      VMSKSU  = D03KSU
089100160830     C     K01VMS04      SETLL     TIVMS04I                           99
089200160831     C*
089300160831IF  1C     WTIPRI        WHENEQ    'VAL'
089400160831     C* i dati li reperisce mediante SQL
089500161122     C                   EXSR      PrepSQLDrill
089600161122     C*
089700161122IF  1C     WTIPRI        WHENEQ    'SQD'
089800161122     C* i dati li reperisce mediante SQL
089900161122     C                   EXSR      PrepSQLSquad
090000170207     C*
090100170207IF  1C     WTIPRI        WHENEQ    'CFR'
090200170207     C* i dati li reperisce mediante SQL
090300170207     C                   EXSR      PrepSQLConfr
090400160830     C*
090500160830E   1C                   ENDSL
090600970526     C* LETTURA RECORD
090700970526IF  1C     *IN99         IFEQ      *OFF
090800970526     C                   EXSR      LETFIL
090900970526E   1C                   ENDIF
091000970526     C*
091100970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
091200970526IF  1C     *IN99         IFEQ      *ON
091300970526     C                   Z-ADD     1             SFLEOF
091400970526E   1C                   ENDIF
091500970526     C*
091600970526     C                   ENDSR
091700970526     C*------------------------------------------------------------------------*
091800970526     C* LETFIL - LETTURA RECORD
091900970526     C*------------------------------------------------------------------------*
092000970526     C     LETFIL        BEGSR
092100970526     C*
092200970526     C* LEGGE FINO A:
092300970526     C*     - FINE FILE
092400970526     C*     - TROVATO RECORD VALIDO
092500970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
092600970526DO  1C     *IN99         DOWEQ     *OFF
092700970526     C     WRECOK        ANDEQ     'N'
092800970526     C*
092900970526     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO DI RICERCA
093000160830     C                   SELECT
093100160831IF  1C     WTIPRI        WHENEQ    'ALL'
093200160831     C                   READ      TIVMS01I                               99
093300160830     C*
093400160830IF  2C     WTIPRI        WHENEQ    'PRO'
093500160830     C     K01VMS01      READE     TIVMS01I                               99
093600160830     C*
093700160830IF  2C     WTIPRI        WHENEQ    'KSU'
093800160830     C     K01VMS04      READE     TIVMS04I                               99
093900160831     C*
094000160831IF  1C     WTIPRI        WHENEQ    'VAL'
094100160831     C* i dati li reperisce mediante SQL
094200161122     C                   EXSR      LeggeSQLDrill
094300161122     C*
094400161122IF  1C     WTIPRI        WHENEQ    'SQD'
094500161122     C* i dati li reperisce mediante SQL
094600161122     C                   EXSR      LeggeSQLSquad
094700170207     C*
094800170207IF  1C     WTIPRI        WHENEQ    'CFR'
094900170207     C* i dati li reperisce mediante SQL
095000170207     C                   EXSR      LeggeSQLConfr
095100160830     C*
095200160830E   2C                   ENDSL
095300970526     C*
095400970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
095500970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
095600970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
095700970530X   2C                   ELSE
095800970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
095900970530E   2C                   ENDIF
096000970526E   1C                   ENDDO
096100970526     C*
096200970526     C                   ENDSR
096300970526     C*------------------------------------------------------------------------*
096400970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
096500970526     C*------------------------------------------------------------------------*
096600970526     C     CHKREC        BEGSR
096700970526     C*
096800970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
096900161205     C* se sono in DrillUp o in ricerca squadrature, il file lo leggo mediante SQL, per cui sono
097000161202     C* certo del risultato
097100161202     C* (i test sotto li lascio nel caso passi da sql a lettura)
097200161202IF  1C                   if        wDrillUp  =  *on  or
097300170207IF  1C                             wSquad    =  *on  or
097400170207IF  1C                             wConfr    =  *on
097500161202     C                   leavesr
097600161202     C                   endif
097700160913     C*
097800160919     C* Proc diversa
097900161102IF  1C                   if        wDrillUp  =  *off and
098000161122IF  1C                             wSquad    =  *off and
098100160927IF  1C                             D03Proc<> *blank and
098200160927IF  1C                             D03Proc<> VMSProc
098300970526     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
098400161102     C                   leavesr
098500161102     C                   endif
098600160927     C*
098700160927     C* Proc diversa
098800160927     C* se sono in DrillUp, il file lo leggo mediante SQL
098900161102IF  1C                   if        wDrillUp  =  *on  and
099000160927IF  1C                             D03Proc<> *blank and
099100161122IF  1C                             D03Proc<> VMS_Drill.Proc
099200160927     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
099300161102     C                   leavesr
099400161102     C                   endif
099500161122     C*
099600161122     C* Proc diversa
099700161122     C* se sono in ricerca Squadrature, il file lo leggo mediante SQL
099800161122IF  1C                   if        wSquad    =  *on  and
099900161122IF  1C                             D03Proc<> *blank and
100000161122IF  1C                             D03Proc<> VMS_Squad.Proc
100100161122     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
100200161122     C                   leavesr
100300161122     C                   endif
100400170207     C*
100500170207     C* Proc diversa
100600170207     C* se sono in confronto, il file lo leggo mediante SQL
100700170207IF  1C                   if        wConfr    =  *on  and
100800170207IF  1C                             D03Proc<> *blank and
100900170207IF  1C                             D03Proc<> VMS_Confr.Proc
101000170207     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
101100170207     C                   leavesr
101200170207     C                   endif
101300160919     C*
101400160919     C* KSU diverso
101500161102IF  1C                   if        wDrillUp  =  *off and
101600161122IF  1C                             wSquad    =  *off and
101700160927IF  1C                             D03KSU <> *blank and
101800160919IF  1C                             D03KSU <> VMSKSU
101900160919     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
102000161102     C                   leavesr
102100161102     C                   endif
102200160927     C*
102300160927     C* KSU diverso
102400161102IF  1C                   if        wDrillUp  =  *on  and
102500160927IF  1C                             D03KSU <> *blank and
102600161122IF  1C                             D03KSU <> VMS_Drill.KSU
102700160927     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
102800161102     C                   leavesr
102900161102     C                   endif
103000161122     C*
103100161122     C* KSU diverso
103200161122IF  1C                   if        wSquad    =  *on  and
103300161122IF  1C                             D03KSU <> *blank and
103400161122IF  1C                             D03KSU <> VMS_Squad.KSU
103500161122     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
103600161122     C                   leavesr
103700161122     C                   endif
103800170207     C*
103900170207     C* KSU diverso
104000170207IF  1C                   if        wConfr    =  *on  and
104100170207IF  1C                             D03KSU <> *blank and
104200170207IF  1C                             D03KSU <> VMS_Confr.KSU
104300170207     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
104400170207     C                   leavesr
104500170207     C                   endif
104600160919     C*
104700160919     C* RECORD CON DECORRENZA MINORE DEL GIORNO RICHIESTO, ESCLUDE
104800161102IF  1C                   if        D03DDE   > *ZEROS
104900161122     C                   SELECT
105000160919     C* se sono in DrillUp, il file lo leggo mediante SQL
105100161122IF  1C                   WHEN      wDrillUp  =  *on
105200160919     C* quindi i campi non sono quelli del file ma quelli della fetch
105300160919     C                   IF        (%date(d03DDE:*eur) <
105400161122     C                              %date(VMS_Drill.DDE)               OR
105500160919     C                              %date(d03DDE:*eur) >
105600161122     C                              %date(VMS_Drill.DSC))
105700160919     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
105800161102     C                   leavesr
105900160919E   2C                   ENDIF
106000161122     C* se NON sono in DrillUp né in ricerca Squadrature
106100161122IF  1C                   WHEN      wDrillUp  =  *off and
106200161122IF  1C                             wSquad    =  *off
106300160919     C                   IF        (%date(d03DDE:*eur) <
106400160919     C                              %date(VMSDDE)               OR
106500160919     C                              %date(d03DDE:*eur) >
106600160919     C                              %date(VMSDSC))
106700160919     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
106800161102     C                   leavesr
106900160919E   2C                   ENDIF
107000161122     C* se sono in ricerca Squadrature
107100161122IF  1C                   WHEN      wSquad    =  *on
107200161122     C                   IF        (%date(d03DDE:*eur) <
107300161122     C                              %date(VMS_Squad.DDE)               OR
107400161122     C                              %date(d03DDE:*eur) >
107500161122     C                              %date(VMS_Squad.DSC))
107600161122     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
107700161122     C                   leavesr
107800161122E   2C                   ENDIF
107900170207     C* se sono in Confronto
108000170207IF  1C                   WHEN      wConfr    =  *on
108100170207     C                   IF        (%date(d03DDE:*eur) <
108200170207     C                              %date(VMS_Confr.DDE)               OR
108300170207     C                              %date(d03DDE:*eur) >
108400170207     C                              %date(VMS_Confr.DSC))
108500170207     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
108600170207     C                   leavesr
108700170207E   2C                   ENDIF
108800161122E   2C                   ENDSL
108900161102E   2C                   ENDIF
109000160913     C*
109100161122     C* CAUBRT diversa (in like)
109200161205IF  1C                   if        D03CAUBRT <> *blank
109300161205           wD03CauBrt = D03CauBRT;
109400161205           wD03CauBRT = %xlate('*':'%':wD03CauBRT);
109500161206           exec sql
109600161206           set :wDummy = (case
109700161206           when trim(:VMSCAUBRT) like trim(:wD03CAUBRT)
109800161206           then '1'
109900161206           else '0'
110000161206           end);
110100161206IF  1C                   if        wDummy = '0'
110200161205     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
110300161205     C                   leavesr
110400161205     C                   endif
110500161205     C                   endif
110600160926     C*
110700160926     C* STSCLI diversa (in like)
110800161205IF  1C                   if        D03STSCLI <> *blank
110900161205           wD03StsCli = D03StsCli;
111000161205           wD03StsCli = %xlate('*':'%':wD03StsCli);
111100161206           exec sql
111200161206           set :wDummy = (case
111300161206           when trim(:VMSSTSCLi) like trim(:wD03StsCli)
111400161206           then '1'
111500161206           else '0'
111600161206           end);
111700161206IF  1C                   if        wDummy = '0'
111800161206     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
111900161206     C                   leavesr
112000161206     C                   endif
112100161205     C                   endif
112200160713
112300970526     C                   ENDSR
112400970526     C*------------------------------------------------------------------------*
112500160830     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE            §
112600970526     C*------------------------------------------------------------------------*
112700970526     C     IRESFL        BEGSR
112800970526     C*
112900970526     C                   MOVEL     *BLANKS       OPZKEY
113000160901     C*
113100161122     C                   SELECT
113200161122     C* se NON è il drill-up né è la ricerca squadrature
113300161122     C                   WHEN      WTIPRI <> 'VAL' and
113400170207     C                             WTIPRI <> 'SQD' and
113500170207     C                             WTIPRI <> 'CFR'
113600160713     C                   EVAL      D05ATB    = VMSATB
113700160831     C                   SETOFF                                       01
113800160831     C                   IF        D05ATB = 'A'
113900160831     C                   SETON                                        01
114000160831     C                   ENDIF
114100160713     C                   EVAL      D05Proc   = VMSProc
114200160713     C                   EVAL      D05KSU    = VMSKSU
114300160830     C                   EVAL      D05CauTip = VMSCauTip
114400160830     C                   EVAL      D05CauBRT = VMSCauBRT
114500160715     C                   EVAL      D05CauTT  = VMSCauTT
114600160713     C                   EVAL      D05StsCli = VMSStsCli
114700160914     C                   EVAL      D05DDE    = %dec(%date(VMSDDE):*dmy)
114800160914     C                   EVAL      D05DSC    = %dec(%date(VMSDSC):*dmy)
114900160715     C                   EVAL      D05DescIT = VMSDescIT
115000970526     C*
115100161122     C* se è il drill-up
115200161122     C                   WHEN      WTIPRI =  'VAL'
115300160901     C                   CLEAR                   D05ATB
115400160901     C                   SETOFF                                       01
115500161122     C                   EVAL      D05Proc   = VMS_Drill.Proc
115600161122     C                   EVAL      D05KSU    = VMS_Drill.KSU
115700160901     C                   EVAL      D05CauTip = *blank
115800160923     C                   EVALR     D05CauBRT = 'Tot:'
115900161122     C                   EVALR     D05CauTT  = %char(VMS_Drill.Count)
116000160901     C                   EVAL      D05StsCli = *blank
116100161122     C                   EVAL      D05DDE    = %dec(%date(VMS_Drill.DDE):*dmy)
116200161122     C                   EVAL      D05DSC    = %dec(%date(VMS_Drill.DSC):*dmy)
116300160901     C                   EVAL      D05DescIT = *blank
116400161122
116500161122     C* se è la ricerca squadrature
116600161122     C                   WHEN      WTIPRI =  'SQD'
116700161122     C                   EVAL      D05ATB    = *blank
116800161122     C                   SETOFF                                       01
116900161122     C                   EVAL      D05Proc   = VMS_Squad.Proc
117000161122     C                   EVAL      D05KSU    = VMS_Squad.KSU
117100161122     C                   EVAL      D05CauTip = VMS_Squad.CauTip
117200161122     C                   EVAL      D05CauBRT = VMS_Squad.CauBRT
117300161122     C                   EVAL      D05CauTT  = VMS_Squad.CauTT
117400161122     C                   EVAL      D05StsCli = VMS_Squad.StsCli
117500161122     C                   EVAL      D05DDE    = %dec(%date(VMS_Squad.DDE):*dmy)
117600161122     C                   EVAL      D05DSC    = %dec(%date(VMS_Squad.DSC):*dmy)
117700161122     C                   EVAL      D05DescIT = VMS_Squad.DescIT
117800170207
117900170207     C* se è il confronto
118000170207     C                   WHEN      WTIPRI =  'CFR'
118100170207     C                   EVAL      D05ATB    = *blank
118200170207     C                   SETOFF                                       01
118300170207     C                   EVAL      D05Proc   = VMS_Confr.Proc
118400170207     C                   EVAL      D05KSU    = VMS_Confr.KSU
118500170207     C                   EVAL      D05CauTip = VMS_Confr.CauTip
118600170207     C                   EVAL      D05CauBRT = VMS_Confr.CauBRT
118700170207     C                   EVAL      D05CauTT  = VMS_Confr.CauTT
118800170207     C                   EVAL      D05StsCli = VMS_Confr.StsCli
118900170207     C                   EVAL      D05DDE    = %dec(%date(VMS_Confr.DDE):*dmy)
119000170207     C                   EVAL      D05DSC    = %dec(%date(VMS_Confr.DSC):*dmy)
119100170207     C                   EVAL      D05DescIT = VMS_Confr.DescIT
119200161122
119300161122     C                   ENDSL
119400160901     C*
119500970526     C                   ENDSR
119600970526     C*------------------------------------------------------------------------*
119700970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
119800970526     C*------------------------------------------------------------------------*
119900970526     C     CHKPAR        BEGSR
120000970526     C*
120100970526     C* REIMPOSTO FLAG EVIDENZAZIONE ERRORI SUI PARAMETRI
120200970526     C                   SETOFF                                       808182
120300160913     C                   SETOFF                                       838485
120400970526     C*--------------------
120500970526     C* ORDINAMENTO
120600970526     C*--------------------
120700970526     C*--------------------
120800970526     C* SELEZIONI
120900970526     C*--------------------
121000970526     C*
121100160831     C* SE NESSUN ORDINAMENTO RICHIESTO, utilizzo il caricamento completo per Procedura
121200160831     C                   MOVEL     'ALL'         WTIPRI            3
121300161123IF  5C                   EVAL      DDEISO = *loval
121400970526     C*
121500970526     C* ACCETTA SOLO UN PARAMETRO DI INIZIO RICERCA
121600970526IF  1C     ERRL00        IFEQ      '0'
121700160913     C* SE gestiremo l'Inizio elenco mettere qui l'elenco controllando che solo 1 sia valorizzato
121800160713     C                   if        1 <> 1
121900970526     C                   MOVEL     MSG(6)        DSMSMS
122000970526     C                   MOVEL     '1'           ERRL00
122100160830     C                   SETON                                        808182
122200970526E   1C                   ENDIF
122300160713E   1C                   ENDIF
122400160831     C*
122500160913     C* CONTROLLA che la data decorrenza  sia corretta formalmente
122600160831     C     ERRL00        IFEQ      '0'
122700160913     C                   IF        D03DDE   <> 0
122800160831     C                   MONITOR
122900160831     C* se il valore della data è > 6 cifre
123000160913     C                   IF        %len(%trim(%char(D03DDE  ))) > 6
123100160831     C* presumo sia scritta in ggmmaaaa
123200160913IF  5C                   EVAL      DDEISO = %date(D03DDE:*eur)
123300160831     C* se il valore della data è <= 6 cifre
123400160831     C                   ELSE
123500160831     C* presumo sia scritta in ggmmaa
123600160919IF  5C                   EVAL      DDEISO = %date(D03DDE:*dmy)
123700160831     C                   ENDIF
123800160831IF  2C                   ON-ERROR
123900160831     C                   MOVEL     '1'           ERRL00
124000160913     C                   SETON                                        83
124100160831     C                   MOVEL     MSG(8)        DSMSMS
124200160831     C                   LEAVESR
124300160831     C                   ENDMON
124400160831     C* rivalorizzo il video con la data in ggmmaaaa
124500160913     C                   EVAL      D03DDE = %dec(DDEISO:*eur)
124600160831     C                   ENDIF
124700160831     C                   ENDIF
124800160830     C*
124900160830IF  1C     ERRL00        IFEQ      '0'
125000160830     C                   SELECT
125100160916IF  2C     wDrillUp      WHENEQ    *on
125200170207IF  2C     wConfr        ANDNE     *on
125300160916     C                   MOVEL     'VAL'         WTIPRI
125400161122IF  2C     wSquad        WHENEQ    *on
125500161122     C                   MOVEL     'SQD'         WTIPRI
125600170207IF  2C     wConfr        WHENEQ    *on
125700170207     C                   MOVEL     'CFR'         WTIPRI
125800160830IF  2C     D03Proc       WHENNE    *blank
125900160830     C                   MOVEL     'PRO'         WTIPRI
126000160830IF  2C     D03KSU        WHENNE    *blank
126100160830     C                   MOVEL     'KSU'         WTIPRI
126200160831     C                   OTHER
126300160831     C                   MOVEL     'ALL'         WTIPRI
126400160830E   2C                   ENDSL
126500970526E   1C                   ENDIF
126600970526     C*
126700970526     C                   ENDSR
126800970526     C*------------------------------------------------------------------------*
126900970526     C* LANPGM - LANCIA UN PROGRAMMA
127000970526     C*------------------------------------------------------------------------*
127100970526     C     LANPGM        BEGSR
127200970526     C*
127300970526     C* IMPOSTA LA DS DI PROCEDURA
127400160713     C                   MOVEL     WOP0          VMS2OP0                         *LIVELLO PROCEDURA
127500160713     C                   MOVEL     WOP1          VMS2OP1                         *LIVELLO PGM
127600160713     C                   MOVEL     '0'           VMS2F03                         *PREMUTO F03=FINE
127700160713     C                   MOVEL     '0'           VMS2F12                         *PREMUTO F12=RIT
127800160713     C                   MOVEL     '0'           VMS2ERR                         *'1'=ERRORE
127900160713     C                   MOVEL     *BLANKS       VMS2MSG                         *MESSAGGIO DI RIT
128000160713     C                   EVAL      VMS2ATB    = DATB
128100160713     C                   EVAL      VMS2Proc   = DProc
128200160713     C                   EVAL      VMS2KSU    = DKSU
128300160831     C                   EVAL      VMS2CauTip = DCauTip
128400160831     C                   EVAL      VMS2CauBRT = DCauBRT
128500160713     C                   EVAL      VMS2StsCli = DStsCli
128600160913     C                   EVAL      VMS2DDE    = %char(DDDE)
128700970526     C*
128800970526     C                   CALL      WPGM
128900970612     C                   PARM                    KPJBA
129000160713     C                   PARM                    TIS7VMS2DS
129100970526     C*
129200970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
129300160713IF  1C     VMS2F03       IFEQ      '1'                                          *PREMUTO F03
129400970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
129500970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
129600970526X   1C                   ELSE                                                   *NON PREMUTO F03
129700160713     C                   MOVEL     VMS2MSG       DSMSMS                         *MESSAGGIO DI RIT
129800970526E   1C                   ENDIF
129900970526     C*
130000970526     C                   ENDSR
130100970527     C*------------------------------------------------------------------------*
130200970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
130300970527     C*------------------------------------------------------------------------*
130400970527     C     CARTAB        BEGSR
130500970527     C*
130600970527     C                   ENDSR
130700970526     C*------------------------------------------------------------------------*
130800970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
130900970526     C*------------------------------------------------------------------------*
131000970526     C     CHKFOP        BEGSR
131100970626     C*
131200970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
131300970626IF  1C     DEPOP0        IFEQ      'R01'
131400970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
131500970626     C                   MOVEL     'S'           OPDATT
131600970626     C                   MOVEL     OPDRIG        OPD(1)
131700970626X   1C                   ELSE
131800970626     C* OPZIONI
131900970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
132000970626     C                   MOVEL     'S'           OPDATT
132100970626     C                   MOVEL     OPDRIG        OPD(5)
132200970626     C* FUNZIONI
132300970626E   1C                   ENDIF
132400970626     C*
132500000602     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
132600970626IF  1C     DEPOP0        IFEQ      'R02'
132700160923     C                   MOVEL     OPD(1)        OPDRIG                         *MODIFICA
132800970626     C                   MOVEL     'S'           OPDATT
132900160923     C                   MOVEL     OPDRIG        OPD(1)
133000160923     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
133100160923     C                   MOVEL     'S'           OPDATT
133200160923     C                   MOVEL     OPDRIG        OPD(2)
133300970626     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
133400970626     C                   MOVEL     'S'           OPDATT
133500970626     C                   MOVEL     OPDRIG        OPD(4)
133600010702     C*                    MOVELOPD,6     OPDRIG           *ALLINEAMENTO
133700010702     C*                    MOVEL'S'       OPDATT
133800010702     C*                    MOVELOPDRIG    OPD,6
133900970626     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
134000970626     C                   MOVEL     'S'           OPDATT
134100970626     C                   MOVEL     OPDRIG        OPD(7)
134200970715     C*
134300970715     C* SOLO SE EDP ABILITA DUPLICA E INSERIMENTO
134400161121     C* SQUADRATURE, STAMPA
134500170207     C* CONFRONTO
134600970715IF  2C     UTENTE        IFEQ      'EDP'
134700970715     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
134800970715     C                   MOVEL     'S'           OPDATT
134900970715     C                   MOVEL     OPDRIG        OPD(3)
135000160915     C                   MOVEL     OPD(13)       OPDRIG                         *DUPLICA
135100160914     C                   MOVEL     'S'           OPDATT
135200160914     C                   MOVEL     OPDRIG        OPD(13)
135300160915     C                   MOVEL     OPD(23)       OPDRIG                         *DUPLICA
135400160915     C                   MOVEL     'S'           OPDATT
135500160915     C                   MOVEL     OPDRIG        OPD(23)
135600970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
135700970626     C                   MOVEL     'S'           FUDATT
135800970626     C                   MOVEL     FUDRIG        FUD(6)
135900160916     C                   MOVEL     FUD(8)        FUDRIG                         *INSERIMENTO
136000160916     C                   MOVEL     'S'           FUDATT
136100160916     C                   MOVEL     FUDRIG        FUD(8)
136200161121     C                   MOVEL     FUD(15)       FUDRIG                         *INSERIMENTO
136300161121     C                   MOVEL     'S'           FUDATT
136400161121     C                   MOVEL     FUDRIG        FUD(15)
136500170208     C                   MOVEL     FUD(16)       FUDRIG                         *INSERIMENTO
136600170208     C                   MOVEL     'S'           FUDATT
136700170208     C                   MOVEL     FUDRIG        FUD(16)
136800161121     C                   MOVEL     FUD(21)       FUDRIG                         *INSERIMENTO
136900161121     C                   MOVEL     'S'           FUDATT
137000161121     C                   MOVEL     FUDRIG        FUD(21)
137100970715E   2C                   ENDIF
137200970715E   1C                   ENDIF
137300970626     C*
137400000602     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO, ALLINEAMENTO
137500161121     C* SQUADRATURE, STAMPA
137600020919IF  1C     DUTLPO        IFNE      'S'                                          *FILIALE
137700970626     C                   MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
137800970626     C                   MOVEL     'N'           OPDATT
137900970626     C                   MOVEL     OPDRIG        OPD(4)
138000010702     C*                    MOVELOPD,6     OPDRIG           *NO ALLINEAMENTO
138100010702     C*                    MOVEL'N'       OPDATT
138200010702     C*                    MOVELOPDRIG    OPD,6
138300970626     C                   MOVEL     OPD(7)        OPDRIG                         *NO RIPRISTINO
138400970626     C                   MOVEL     'N'           OPDATT
138500970626     C                   MOVEL     OPDRIG        OPD(7)
138600160916     C                   MOVEL     OPD(13)       OPDRIG                         *NO RIPRISTINO
138700160916     C                   MOVEL     'N'           OPDATT
138800160916     C                   MOVEL     OPDRIG        OPD(13)
138900160916     C                   MOVEL     OPD(23)       OPDRIG                         *NO RIPRISTINO
139000160916     C                   MOVEL     'N'           OPDATT
139100160916     C                   MOVEL     OPDRIG        OPD(23)
139200970626     C                   MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
139300970626     C                   MOVEL     'N'           FUDATT
139400970626     C                   MOVEL     FUDRIG        FUD(6)
139500160916     C                   MOVEL     FUD(8)        FUDRIG                         *NO INSERIMENTO
139600160916     C                   MOVEL     'N'           FUDATT
139700160916     C                   MOVEL     FUDRIG        FUD(8)
139800161121     C                   MOVEL     FUD(15)       FUDRIG                         *INSERIMENTO
139900161121     C                   MOVEL     'N'           FUDATT
140000161121     C                   MOVEL     FUDRIG        FUD(15)
140100170208     C                   MOVEL     FUD(16)       FUDRIG                         *INSERIMENTO
140200170208     C                   MOVEL     'N'           FUDATT
140300170208     C                   MOVEL     FUDRIG        FUD(16)
140400161121     C                   MOVEL     FUD(21)       FUDRIG                         *INSERIMENTO
140500161121     C                   MOVEL     'N'           FUDATT
140600161121     C                   MOVEL     FUDRIG        FUD(21)
140700970626E   1C                   ENDIF
140800970526     C*
140900970526     C* GESTIONE OPZIONI A VIDEO
141000970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
141100970526     C                   EXSR      OPZGES
141200970526     C*
141300970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
141400970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
141500970526     C                   EXSR      FUNGES
141600970526     C*
141700970526     C                   ENDSR
141800020919     C*--------------------------------------------------------------------------------------------*
141900020919     C* REPERISCE I DATI UTENTE
142000020919     C*--------------------------------------------------------------------------------------------*
142100020919     C     REPDATIUTE    BEGSR
142200020919     C*
142300020919     C* INIZIALIZZA VARIABILI DI WRK
142400020919     C                   CLEAR                   TIBS34DS
142500020919     C                   CLEAR                   AZUTEDS
142600020919     C                   CLEAR                   DDATIUTE
142700020919     C*
142800020919     C     *DTAARA       DEFINE    §azute        azuteds
142900020919     C     *DTAARA       DEFINE    §datiute      ddatiute
143000020919     C                   IN(E)     *DTAARA
143100020919if  1C                   IF        %Error
143200020919     c                   EVAL      I34Tla = 'L'
143300020919     C                   CALL      'TIBS34R'
143400020919     C                   PARM                    Tibs34Ds
143500020919     C                   IN        *DTAARA
143600020919e   1C                   ENDIF
143700020919     C*
143800020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
143900020919     C                   MOVEL(P)  RSUT          DSFIRS           20
144000020919     C*
144100020919     C                   ENDSR
144200970526     C*------------------------------------------------------------------------*
144300970526     C* *INZSR - OPERAZIONI INIZIALI
144400970526     C*------------------------------------------------------------------------*
144500970526     C     *INZSR        BEGSR
144600970526     C*---
144700970526     C* RICEVIMENTO PARAMETRI
144800970526     C*---
144900970526     C     *ENTRY        PLIST
145000970612     C                   PARM                    KPJBA
145100160714     C*
145200160714     C                   EVAL      TIS7VMS2DS = kpjbu
145300160913     c                   if        kpjbu = *blank
145400160913     c                   clear                   TIS7VMS2DS
145500160913     c                   endif
145600970526     C*
145700970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
145800970526     C     XMSGPA        PLIST
145900970526     C                   PARM                    DSMSMS
146000970526     C                   PARM                    DSMSPG
146100970526     C                   PARM                    DSMSKE
146200970526     C                   PARM                    DSMSER
146300970526     C*
146400970526     C* IMPOSTA LA DS -TASTI PREMUTI-
146500970526     C                   CALL      'XKEY'
146600970526     C                   PARM                    DSKEY
146700970526     C*---
146800970526     C* VARIABILI NON RIFERITE AL DATA BASE
146900970526     C*---
147000970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
147100970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
147200970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
147300970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
147400970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
147500970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
147600970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
147700970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
147800970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
147900160713     C     *LIKE         DEFINE    VMS2OP0       DEPOP0                         *DI DEPOSITO
148000160713     C     *LIKE         DEFINE    VMS2OP1       DEPOP1
148100160713     C     *LIKE         DEFINE    VMS2OP0       WOP0                           *DI LAVORO
148200160713     C     *LIKE         DEFINE    VMS2OP1       WOP1
148300970526     C*---
148400970526     C* VARIABILI DI CONTROLLO
148500970526     C*---
148600970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
148700970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
148800970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
148900970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
149000970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
149100160714     C                   Z-ADD     14            SFLPAG                         *N°RIGHE X PAG
149200970526     C*
149300970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
149400160712     C                   MOVEL     'TIS7VMS1R'   DSMSPG
149500160712     C                   MOVEL     'TIS7VMS1R'   MSGQUE
149600970526     C                   MOVEL     *BLANKS       MSGKEY
149700970526     C                   MOVEL     *BLANKS       DSMSMS
149800970526     C                   MOVEL     *BLANKS       DSMSKE
149900970526     C                   Z-ADD     *ZEROS        DSMSER
150000970526     C*---
150100970526     C* VARIABILI RIFERITE AL DATA BASE
150200970526     C*---
150300970526     C*---
150400970526     C* CHIAVI DI LETTURA
150500970526     C*---
150600160713     C* LETTURA TIVMS01I - COMPLETA
150700160914     C     K05VMS01      KLIST
150800160713     C                   KFLD                    VMSProc
150900160713     C                   KFLD                    VMSKSU
151000160830     C                   KFLD                    VMSDDE
151100160830     C                   KFLD                    VMSCauTip
151200160912     C                   KFLD                    VMSCauBRT
151300160713     C* LETTURA TIVMS01I - parziale
151400160830     C     K01VMS01      KLIST
151500160713     C                   KFLD                    VMSProc
151600160913     C* LETTURA TIVMS01I - parziale
151700160913     C     K02VMS01      KLIST
151800160913     C                   KFLD                    VMSProc
151900160913     C                   KFLD                    VMSKSU
152000160913     C* LETTURA TIVMS01I - parziale
152100160913     C     K03VMS01      KLIST
152200160913     C                   KFLD                    VMSProc
152300160913     C                   KFLD                    VMSKSU
152400160913     C                   KFLD                    VMSDDE
152500160830     C* LETTURA TIVMS04I - parziale
152600160830     C     K01VMS04      KLIST
152700160830     C                   KFLD                    VMSKSU
152800970527     C*
152900970527     C* REPERIMENTO DATI UTENTE
153000020919     C                   EXSR      REPDATIUTE
153100970527     C*
153200970527     C* CARICA LE TABELLE OCCORRENTI
153300970527     C                   EXSR      CARTAB
153400970526     C*
153500970526     C* DEPOSITA LE OPZIONI DI ENTRATA
153600160713     C                   MOVEL     VMS2OP0       DEPOP0                         *LIVELLO PROCEDURA
153700160713     C                   MOVEL     VMS2OP1       DEPOP1                         *LIVELLO PROGRAMMA
153800970526     C*
153900970526     C* CALCOLA LA DATA CORRENTE
154000970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
154100970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
154200970526     C                   Z-ADD     WN8           G08DAT
154300970526     C                   Z-ADD     *ZEROS        G08INV
154400970526     C                   MOVEL     '0'           G08ERR
154500970526     C                   CALL      'XSRDA8'
154600970526     C                   PARM                    WLBDA8
154700970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
154800970526     C*
154900970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
155000970526     C                   EXSR      CHKFOP
155100970526     C*
155200970526     C* IMPOSTA I PARAMETRI DI INGRESSO
155300160912     C*  intanto imposto le selezioni con i dati passati
155400160912     C                   EVAL      D03Proc = VMS2Proc
155500160913     C                   EVAL      D03KSU  = VMS2KSU
155600170302     C                   EVAL      D03DDE  =
155700170302     c                              %dec(%date():*eur)
155800160913     C                   if        VMS2DDE <> *blank and
155900160913     C                             VMS2DDE <> *zeros
156000160913     C                   EVAL      D03DDE  =
156100160913     c                              %dec(%date(%dec(VMS2DDE:8:0)):*eur)
156200160913     c                   endif
156300170302     C                   EVAL      DEPProc = D03Proc
156400170302     C                   EVAL      DEPKSU  = D03KSU
156500170302     C                   EVAL      DEPDDE  = D03DDE
156600160714     C                   EXSR      CHKPAR
156700160714IF  1C     ERRL00        IFEQ      '1'
156800160714     C                   MOVEL     '1'           VMS2ERR
156900160714     C                   MOVEL     DSMSMS        VMS2MSG
157000160714X   1C                   ELSE
157100160714E   1C                   ENDIF
157200980225     C*
157300970526     C*
157400970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
157500970530IF  1C     DEPOP0        IFEQ      'R01'
157600970530     C                   MOVEL     DEC(3)        VIDING
157700970530E   1C                   ENDIF
157800970530IF  1C     DEPOP0        IFEQ      'R02'
157900970530     C                   MOVEL     DEC(1)        VIDING
158000970530E   1C                   ENDIF
158100970530IF  1C     DEPOP0        IFEQ      'R05'
158200970530     C                   MOVEL     DEC(2)        VIDING
158300970530E   1C                   ENDIF
158400970526     C*
158500970526     C                   ENDSR
158600970526     C*------------------------------------------------------------------------*
158700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
158800970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
158900970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
159000970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
159100970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
159200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
159300970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
159400970526     C*------------------------------------------------------------------------*
159500970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
159600970526     C*------------------------------------------------------------------------*
159700970526     C     POSSFL        BEGSR
159800970526     C*
159900970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
160000970526     C                   EXSR      CLRSFL
160100970526     C*
160200970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
160300970526     C                   EXSR      SETFIL
160400970526     C*
160500970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
160600970526     C     SFLEOF        IFEQ      0
160700970526     C                   EXSR      CARSFL
160800970526     C                   END
160900970526     C*
161000970526     C                   ENDSR
161100970526     C*------------------------------------------------------------------------*
161200970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
161300970526     C*------------------------------------------------------------------------*
161400970526     C     ENTATT        BEGSR
161500970526     C*
161600970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
161700970526     C                   Z-ADD     0             OPZOK             1 0
161800970526     C*
161900970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
162000970526     C                   Z-ADD     0             SFLVAR            5 0
162100970526     C*
162200970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
162300970526     C                   Z-ADD     0             SFLERR
162400970526     C*
162500970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
162600970526     C                   Z-ADD     SFLULT        WFLULT
162700970526     C*
162800970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
162900160712     C                   READC     S7V1D05                                98
163000970526     C     *IN98         DOWEQ     '0'
163100970526     C*
163200970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
163300970526     C                   ADD       1             SFLVAR
163400970526     C*
163500970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
163600970526     C     OPZOK         IFEQ      0
163700970526     C*
163800970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
163900970526     C     OPZKEY        IFNE      *BLANKS
164000970526     C                   EXSR      OPZCON
164100970526     C                   END
164200970526     C*
164300970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
164400970526     C*            2. PULIZIA SCELTA IN SFL
164500970526     C     OPZOK         IFEQ      0
164600970526     C                   MOVEL     '0'           *IN41
164700970526     C                   MOVEL     *BLANKS       OPZKEY
164800970526     C                   ELSE
164900970526     C*
165000970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
165100970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
165200970526     C                   MOVEL     '1'           *IN41
165300970526     C                   Z-ADD     SFLULT        SFLERR
165400970526     C                   END
165500970526     C                   ELSE
165600970526     C*
165700970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
165800970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
165900970526     C                   MOVEL     '1'           *IN41
166000970526     C                   END                                                    OPZOK=0
166100970526     C*
166200970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
166300970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
166400970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
166500970526     C     OPZOK         IFNE      0
166600970526     C                   Z-ADD     SFLERR        SFLNBR
166700970526     C                   ELSE
166800970526     C                   Z-ADD     SFLULT        SFLNBR
166900970526     C                   END
167000970526     C*
167100970526     C* AGGIORNO RECORD SFL
167200160902     C*  imposto "effetti speciali"
167300160902     C                   EXSR      SFXD05
167400160712     C                   UPDATE    S7V1D05
167500970526     C*
167600160712     C                   READC     S7V1D05                                98
167700970526     C                   END                                                    DO WHILE
167800970526     C*
167900970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
168000970526     C                   Z-ADD     WFLULT        SFLULT
168100970526     C*
168200970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
168300970526     C     SFLVAR        IFEQ      0
168400970526     C*---                 MOVEL'1'       FINL00
168500970526     C                   END
168600970526     C*
168700970526     C                   ENDSR
168800970526     C*------------------------------------------------------------------------*
168900970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
169000970526     C*------------------------------------------------------------------------*
169100970526     C     CLRSFL        BEGSR
169200970526     C*
169300970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
169400970526     C* (0=NON FINE FILE  1=FINE FILE)
169500970526     C                   Z-ADD     0             SFLEOF
169600970526     C*
169700970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
169800970526     C                   Z-ADD     0             SFLULT
169900970526     C*
170000970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
170100970526     C                   MOVEL     '1'           *IN40
170200160712     C                   WRITE     S7V1D06
170300970526     C                   MOVEL     '0'           *IN40
170400970526     C*
170500970526     C                   ENDSR
170600970526     C*------------------------------------------------------------------------*
170700970526     C* CARSFL - CARICAMENTO SFL
170800970526     C*------------------------------------------------------------------------*
170900970526     C     CARSFL        BEGSR
171000970526     C*
171100970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
171200970526     C     SFLULT        IFEQ      0
171300970526     C                   EXSR      SETFIL
171400970526     C                   END
171500970526     C*
171600970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
171700970526     C                   Z-ADD     SFLULT        WFLULT
171800970526     C*
171900970526     C* CICLO:
172000970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
172100970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
172200970526     C                   Z-ADD     1             SFLRIG
172300970526     C     SFLRIG        DOWLE     SFLPAG
172400970526     C     SFLEOF        ANDEQ     0
172500970526     C*
172600970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
172700970526     C                   MOVEL     *BLANKS       OPZKEY
172800970526     C*
172900970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
173000970526     C                   EXSR      IRESFL
173100970526     C*
173200970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
173300970526     C                   ADD       1             SFLULT
173400970526     C*
173500970526     C* SCRITTURA RECORD SFL
173600160902     C*  imposto "effetti speciali"
173700160902     C                   EXSR      SFXD05
173800160712     C                   WRITE     S7V1D05
173900970526     C                   ADD       1             SFLRIG
174000970526     C*
174100970526     C* LETTURA FILE DA CARICARE
174200970526     C                   EXSR      LETFIL
174300970526     C                   END                                                    DO WHILE
174400970526     C*
174500970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
174600970526     C     WFLULT        IFNE      SFLULT
174700970526     C                   Z-ADD     WFLULT        SFLNBR
174800970526     C                   ADD       1             SFLNBR
174900970526     C                   END
175000970526     C*
175100970526     C                   ENDSR
175200970526     C*------------------------------------------------------------------------*
175300160902     C* SFXD05 - effetti speciali sfl D05 (RI, ND, BL, ...)
175400160902     C*          a seconda dei dati presenti in riga
175500970526     C*------------------------------------------------------------------------*
175600160902     C     SFXD05        BEGSR
175700970526     C*
175800160902     C* RI campi chiave per rcd annullato
175900160902     C                   SETOFF                                       01
176000160902     C                   IF        D05ATB = 'A'
176100160902     C                   SETON                                        01
176200160902     C                   ENDIF
176300160902     C*
176400160902     C                   ENDSR
176500160902     C*------------------------------------------------------------------------*
176600160902     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
176700160902     C*------------------------------------------------------------------------*
176800160902     C     OUTSFL        BEGSR
176900160902     C*
177000970526     C* WRITE RECORD TESTATA VIDEO
177100160712     C                   WRITE     S7V1D01
177200970526     C*
177300970526     C* VISUALIZZAZIONE MESSAGGIO
177400970526     C                   EXSR      SNDMSG
177500970526     C*
177600970526     C* WRITE RECORD OPZIONI VIDEO
177700160712     C                   WRITE     S7V1D02
177800970526     C*
177900970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
178000160712     C                   WRITE     S7V1D03
178100970526     C*
178200970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
178300160712     C                   WRITE     S7V1D04
178400970526     C*
178500970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
178600970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
178700970526     C     SFLEOF        IFEQ      0
178800970526     C                   MOVEL     '0'           *IN42
178900970526     C                   ELSE
179000970526     C*
179100970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
179200970526     C                   MOVEL     '1'           *IN42
179300970526     C                   END
179400970526     C*
179500970526     C* WRITE RECORD TASTI FUNZIONALI
179600160712     C                   WRITE     S7V1D09
179700970526     C*
179800970526     C* CASO DI SFL VUOTO
179900970526     C     SFLULT        IFLE      0
180000970526     C*
180100970526     C*    - WRITE RECORD VUOTO
180200160712     C                   WRITE     S7V1D0A
180300160712     C                   READ      S7V1D0A                                99
180400970526     C*
180500970526     C                   ELSE
180600970526     C*
180700970526     C* CASO DI SFL CON RECORDS
180800970526     C*
180900970526     C*    - WRITE RECORD CONTROLLO SFL
181000160712     C                   WRITE     S7V1D06
181100160712     C                   READ      S7V1D06                                99
181200970526     C                   END
181300970526     C*
181400970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
181500160712     C                   READ      S7V1D03                                99
181600970526     C*
181700970526     C                   ENDSR
181800970526     C*------------------------------------------------------------------------*
181900970526     C* SNDMSG - MANDA MESSAGGIO
182000970526     C*------------------------------------------------------------------------*
182100970526     C     SNDMSG        BEGSR
182200970526     C*
182300970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
182400970526     C                   MOVEL     '1'           *IN40
182500160712     C                   WRITE     S7V1DMC
182600970526     C                   MOVEL     '0'           *IN40
182700970526     C*
182800970526     C* VISUALIZZAZIONE MESSAGGIO
182900970526     C                   Z-ADD     0             MSGULT
183000970526     C                   CALL      'XMSG'        XMSGPA
183100970526     C     DSMSER        IFEQ      0
183200970526     C                   MOVEL     DSMSKE        MSGKEY
183300970526     C                   END
183400970526     C                   Z-ADD     1             MSGULT
183500160712     C                   WRITE     S7V1DMS                                      SFL MESS
183600160712     C                   WRITE     S7V1DMC                                      CONTR MESS
183700970526     C*
183800970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
183900970526     C                   MOVEL     *BLANKS       DSMSMS
184000970526     C*
184100970526     C                   ENDSR
184200970526     C*------------------------------------------------------------------------*
184300970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
184400970526     C*------------------------------------------------------------------------*
184500970526     C     FUNCON        BEGSR
184600970526     C*
184700970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
184800970526     C                   EXSR      FUNVAR
184900970526     C*
185000970526     C* PULISCO CAMPI DI LAVORO
185100970526     C                   Z-ADD     0             I                 5 0
185200970526     C                   Z-ADD     0             W                 5 0
185300970526     C*
185400970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
185500970526     C     FUNKEY        IFNE      *BLANKS
185600970526     C     1             DO        24            I
185700970526     C                   MOVEL     FUD(I)        FUDRIG
185800970526     C*
185900970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
186000970526     C     FUNKEY        IFEQ      FUDFUN
186100970526     C     FUDATT        ANDEQ     'S'
186200970526     C                   Z-ADD     I             W
186300970526     C                   END
186400970526     C                   END                                                    DO WHILE
186500970526     C*
186600970526     C* FUNZIONE NON ATTIVA (ERRORE)
186700970526     C     W             IFEQ      0
186800970526     C*
186900970526     C* EMISSIONE MESSAGGIO DI ERRORE
187000970526     C                   MOVEL     MSG(2)        DSMSMS
187100970526     C                   END                                                    W=0
187200970526     C                   END                                                    FUNKEY NE *BLANKS
187300970526     C*
187400970526     C* FUNZIONE ATTIVA (PROCEDI ...)
187500970526     C     W             IFNE      0
187600970526     C     FUNKEY        OREQ      *BLANKS
187700970526     C*
187800970526     C     FUNKEY        IFEQ      *BLANKS
187900970526     C                   EXSR      ENTGES
188000970526     C                   END
188100970526     C     FUNKEY        IFEQ      '03'
188200970526     C                   EXSR      F03GES
188300970526     C                   END
188400970526     C     FUNKEY        IFEQ      '05'
188500970526     C                   EXSR      F05GES
188600970526     C                   END
188700970526     C     FUNKEY        IFEQ      '06'
188800970526     C                   EXSR      F06GES
188900970526     C                   END
189000160916     C     FUNKEY        IFEQ      '08'
189100160916     C                   EXSR      F08GES
189200160916     C                   END
189300160913     C     FUNKEY        IFEQ      '12'
189400160913     C                   EXSR      F12GES
189500160913     C                   END
189600161121     C     FUNKEY        IFEQ      '15'
189700161121     C                   EXSR      F15GES
189800161121     C                   END
189900170208     C     FUNKEY        IFEQ      '16'
190000170208     C                   EXSR      F16GES
190100170208     C                   END
190200161121     C     FUNKEY        IFEQ      '21'
190300161121     C                   EXSR      F21GES
190400161121     C                   END
190500970526     C     FUNKEY        IFEQ      '22'
190600970526     C                   EXSR      F22GES
190700970526     C                   END
190800970526     C     FUNKEY        IFEQ      '23'
190900970526     C                   EXSR      F23GES
191000970526     C                   END
191100970526     C     FUNKEY        IFEQ      '24'
191200970526     C                   EXSR      F24GES
191300970526     C                   END
191400970526     C     FUNKEY        IFEQ      'RU'
191500970526     C                   EXSR      RUPGES
191600970526     C                   END
191700970526     C*
191800970526     C                   END
191900970526     C*
192000970526     C                   ENDSR
192100970526     C*------------------------------------------------------------------------*
192200970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
192300970526     C*------------------------------------------------------------------------*
192400970526     C     FUNVAR        BEGSR
192500970526     C*
192600970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
192700970526     C     KEY           IFEQ      ENTER
192800970526     C                   MOVEL     *BLANKS       FUNKEY
192900970526     C                   END
193000970526     C     KEY           IFEQ      F1
193100970526     C                   MOVEL     '01'          FUNKEY
193200970526     C                   END
193300970526     C     KEY           IFEQ      F2
193400970526     C                   MOVEL     '02'          FUNKEY
193500970526     C                   END
193600970526     C     KEY           IFEQ      F3
193700970526     C                   MOVEL     '03'          FUNKEY
193800970526     C                   END
193900970526     C     KEY           IFEQ      F4
194000970526     C                   MOVEL     '04'          FUNKEY
194100970526     C                   END
194200970526     C     KEY           IFEQ      F5
194300970526     C                   MOVEL     '05'          FUNKEY
194400970526     C                   END
194500970526     C     KEY           IFEQ      F6
194600970526     C                   MOVEL     '06'          FUNKEY
194700970526     C                   END
194800970526     C     KEY           IFEQ      F7
194900970526     C                   MOVEL     '07'          FUNKEY
195000970526     C                   END
195100970526     C     KEY           IFEQ      F8
195200970526     C                   MOVEL     '08'          FUNKEY
195300970526     C                   END
195400970526     C     KEY           IFEQ      F9
195500970526     C                   MOVEL     '09'          FUNKEY
195600970526     C                   END
195700970526     C     KEY           IFEQ      F10
195800970526     C                   MOVEL     '10'          FUNKEY
195900970526     C                   END
196000970526     C     KEY           IFEQ      F11
196100970526     C                   MOVEL     '11'          FUNKEY
196200970526     C                   END
196300970526     C     KEY           IFEQ      F12
196400970526     C                   MOVEL     '12'          FUNKEY
196500970526     C                   END
196600970526     C     KEY           IFEQ      F13
196700970526     C                   MOVEL     '13'          FUNKEY
196800970526     C                   END
196900970526     C     KEY           IFEQ      F14
197000970526     C                   MOVEL     '14'          FUNKEY
197100970526     C                   END
197200970526     C     KEY           IFEQ      F15
197300970526     C                   MOVEL     '15'          FUNKEY
197400970526     C                   END
197500970526     C     KEY           IFEQ      F16
197600970526     C                   MOVEL     '16'          FUNKEY
197700970526     C                   END
197800970526     C     KEY           IFEQ      F17
197900970526     C                   MOVEL     '17'          FUNKEY
198000970526     C                   END
198100970526     C     KEY           IFEQ      F18
198200970526     C                   MOVEL     '18'          FUNKEY
198300970526     C                   END
198400970526     C     KEY           IFEQ      F19
198500970526     C                   MOVEL     '19'          FUNKEY
198600970526     C                   END
198700970526     C     KEY           IFEQ      F20
198800970526     C                   MOVEL     '20'          FUNKEY
198900970526     C                   END
199000970526     C     KEY           IFEQ      F21
199100970526     C                   MOVEL     '21'          FUNKEY
199200970526     C                   END
199300970526     C     KEY           IFEQ      F22
199400970526     C                   MOVEL     '22'          FUNKEY
199500970526     C                   END
199600970526     C     KEY           IFEQ      F23
199700970526     C                   MOVEL     '23'          FUNKEY
199800970526     C                   END
199900970526     C     KEY           IFEQ      F24
200000970526     C                   MOVEL     '24'          FUNKEY
200100970526     C                   END
200200970526     C     KEY           IFEQ      ROLLUP
200300970526     C                   MOVEL     'RU'          FUNKEY
200400970526     C                   END
200500970526     C     KEY           IFEQ      HELP
200600970526     C                   MOVEL     'HP'          FUNKEY
200700970526     C                   END
200800970526     C*
200900970526     C                   ENDSR
201000970526     C*------------------------------------------------------------------------*
201100970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
201200970526     C*------------------------------------------------------------------------*
201300970526     C     FUNGES        BEGSR
201400970526     C*
201500970526     C* PULISCO CAMPI DI LAVORO
201600970526     C                   Z-ADD     0             W                 5 0
201700970526     C*
201800970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
201900970526     C* E CARICO TABELLA FUNZIONI ATTIVE
202000970526     C     1             DO        24            I
202100970526     C                   MOVEL     FUD(I)        FUDRIG
202200970526     C*
202300970526     C* PULIZIA FUNZIONI PRECEDENTI
202400970526     C                   MOVEL     *BLANKS       FUV(I)
202500970526     C*
202600970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
202700970526     C* (SALTA F23 E F24)
202800970526     C     FUDATT        IFEQ      'S'
202900970526     C     FUDFUN        ANDNE     '23'
203000970526     C     FUDFUN        ANDNE     '24'
203100970526     C                   ADD       1             W
203200970526     C                   MOVEL     FUDDES        FUV(W)
203300970526     C                   END
203400970526     C                   END                                                    DO WHILE
203500970526     C*
203600970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
203700970526     C* ALTRIMENTI DISATTIVO F24
203800970526     C     1             DO        24            I
203900970526     C                   MOVEL     FUD(I)        FUDRIG
204000970526     C*
204100970526     C     FUDFUN        IFEQ      '24'
204200970526     C     W             IFGT      6
204300970526     C                   MOVEL     'S'           FUDATT
204400970526     C                   ELSE
204500970526     C                   MOVEL     *BLANKS       FUDATT
204600970526     C                   END                                                    W GT 8
204700970526     C                   MOVEL     FUDRIG        FUD(I)
204800970526     C                   END
204900970526     C                   END                                                    DO WHILE
205000970526     C*
205100970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
205200970526     C     W             DIV       6             I
205300970526     C                   MVR                     Z
205400970526     C*
205500970526     C     Z             IFNE      0
205600970526     C     I             ADD       1             Z
205700970526     C                   ELSE
205800970526     C                   Z-ADD     I             Z
205900970526     C                   END
206000970526     C*
206100970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
206200970526     C                   ADD       1             FUNGRU
206300970526     C     FUNGRU        IFGT      Z
206400970526     C     FUNGRU        ORLT      1
206500970526     C                   Z-ADD     1             FUNGRU
206600970526     C                   END
206700970526     C*
206800970526     C* PRIMO GRUPPO DI 6 FUNZIONI
206900970526     C     FUNGRU        IFEQ      1
207000970526     C                   MOVEL     FUV(1)        FUN001
207100970526     C                   MOVEL     FUV(2)        FUN002
207200970526     C                   MOVEL     FUV(3)        FUN003
207300970526     C                   MOVEL     FUV(4)        FUN004
207400970526     C                   MOVEL     FUV(5)        FUN005
207500970526     C                   MOVEL     FUV(6)        FUN006
207600970526     C                   END
207700970526     C*
207800970526     C* SECONDO GRUPPO DI 6 FUNZIONI
207900970526     C     FUNGRU        IFEQ      2
208000970526     C                   MOVEL     FUV(7)        FUN001
208100970526     C                   MOVEL     FUV(8)        FUN002
208200970526     C                   MOVEL     FUV(9)        FUN003
208300970526     C                   MOVEL     FUV(10)       FUN004
208400970526     C                   MOVEL     FUV(11)       FUN005
208500970526     C                   MOVEL     FUV(12)       FUN006
208600970526     C                   END
208700970526     C*
208800970526     C* TERZO GRUPPO DI 6 FUNZIONI
208900970526     C     FUNGRU        IFEQ      3
209000970526     C                   MOVEL     FUV(13)       FUN001
209100970526     C                   MOVEL     FUV(14)       FUN002
209200970526     C                   MOVEL     FUV(15)       FUN003
209300970526     C                   MOVEL     FUV(16)       FUN004
209400970526     C                   MOVEL     FUV(17)       FUN005
209500970526     C                   MOVEL     FUV(18)       FUN006
209600970526     C                   END
209700970526     C*
209800970526     C* QUARTO GRUPPO DI 6 FUNZIONI
209900970526     C     FUNGRU        IFEQ      4
210000970526     C                   MOVEL     FUV(19)       FUN001
210100970526     C                   MOVEL     FUV(20)       FUN002
210200970526     C                   MOVEL     FUV(21)       FUN003
210300970526     C                   MOVEL     FUV(22)       FUN004
210400970526     C                   MOVEL     *BLANKS       FUN005
210500970526     C                   MOVEL     *BLANKS       FUN006
210600970526     C                   END
210700970526     C*
210800970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
210900970526     C                   MOVEL     *BLANKS       FUN007
211000970526     C                   MOVEL     *BLANKS       FUN008
211100970526     C*
211200970526     C     1             DO        24            I
211300970526     C                   MOVEL     FUD(I)        FUDRIG
211400970526     C     FUDFUN        IFEQ      '23'
211500970526     C     FUDATT        ANDEQ     'S'
211600970526     C                   MOVEL     FUDDES        FUN007
211700970526     C                   END
211800970526     C     FUDFUN        IFEQ      '24'
211900970526     C     FUDATT        ANDEQ     'S'
212000970526     C                   MOVEL     FUDDES        FUN008
212100970526     C                   END
212200970526     C                   END                                                    DO
212300970526     C*
212400970526     C                   ENDSR
212500970526     C*------------------------------------------------------------------------*
212600970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
212700970526     C*------------------------------------------------------------------------*
212800970526     C     OPZGES        BEGSR
212900970526     C*
213000970526     C* PULISCO CAMPI DI LAVORO
213100970526     C                   Z-ADD     0             W                 5 0
213200970526     C*
213300970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
213400970526     C* E CARICO TABELLA OPZIONI ATTIVE
213500970526     C     1             DO        24            I
213600970526     C                   MOVEL     OPD(I)        OPDRIG
213700970526     C*
213800970526     C* PULIZIA OPZIONI PRECEDENTI
213900970526     C                   MOVEL     *BLANKS       OPV(I)
214000970526     C*
214100970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
214200970526     C     OPDATT        IFEQ      'S'
214300970526     C                   ADD       1             W
214400970526     C                   MOVEL     OPDDES        OPV(W)
214500970526     C                   END
214600970526     C                   END                                                    DO WHILE
214700970526     C*
214800970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
214900970526     C* ALTRIMENTI DISATTIVO F23
215000970526     C     1             DO        24            I
215100970526     C                   MOVEL     FUD(I)        FUDRIG
215200970526     C*
215300970526     C     FUDFUN        IFEQ      '23'
215400970526     C     W             IFGT      8
215500970526     C                   MOVEL     'S'           FUDATT
215600970526     C                   ELSE
215700970526     C                   MOVEL     *BLANKS       FUDATT
215800970526     C                   END                                                    W GT 8
215900970526     C                   MOVEL     FUDRIG        FUD(I)
216000970526     C                   END
216100970526     C                   END                                                    DO WHILE
216200970526     C*
216300970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
216400970526     C     W             DIV       8             I
216500970526     C                   MVR                     Z                 5 0
216600970526     C*
216700970526     C     Z             IFNE      0
216800970526     C     I             ADD       1             Z
216900970526     C                   ELSE
217000970526     C                   Z-ADD     I             Z
217100970526     C                   END
217200970526     C*
217300970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
217400970526     C                   ADD       1             OPZGRU
217500970526     C     OPZGRU        IFGT      Z
217600970526     C     OPZGRU        ORLT      1
217700970526     C                   Z-ADD     1             OPZGRU
217800970526     C                   END
217900970526     C*
218000970526     C* PRIMO GRUPPO DI 8 OPZIONI
218100970526     C     OPZGRU        IFEQ      1
218200970526     C                   MOVEL     OPV(1)        OPZ001
218300970526     C                   MOVEL     OPV(2)        OPZ002
218400970526     C                   MOVEL     OPV(3)        OPZ003
218500970526     C                   MOVEL     OPV(4)        OPZ004
218600970526     C                   MOVEL     OPV(5)        OPZ005
218700970526     C                   MOVEL     OPV(6)        OPZ006
218800970526     C                   MOVEL     OPV(7)        OPZ007
218900970526     C                   MOVEL     OPV(8)        OPZ008
219000970526     C                   END
219100970526     C*
219200970526     C* SECONDO GRUPPO DI 8 OPZIONI
219300970526     C     OPZGRU        IFEQ      2
219400970526     C                   MOVEL     OPV(9)        OPZ001
219500970526     C                   MOVEL     OPV(10)       OPZ002
219600970526     C                   MOVEL     OPV(11)       OPZ003
219700970526     C                   MOVEL     OPV(12)       OPZ004
219800970526     C                   MOVEL     OPV(13)       OPZ005
219900970526     C                   MOVEL     OPV(14)       OPZ006
220000970526     C                   MOVEL     OPV(15)       OPZ007
220100970526     C                   MOVEL     OPV(16)       OPZ008
220200970526     C                   END
220300970526     C*
220400970526     C* TERZO GRUPPO DI 8 OPZIONI
220500970526     C     OPZGRU        IFEQ      3
220600970526     C                   MOVEL     OPV(17)       OPZ001
220700970526     C                   MOVEL     OPV(18)       OPZ002
220800970526     C                   MOVEL     OPV(19)       OPZ003
220900970526     C                   MOVEL     OPV(20)       OPZ004
221000970526     C                   MOVEL     OPV(21)       OPZ005
221100970526     C                   MOVEL     OPV(22)       OPZ006
221200970526     C                   MOVEL     OPV(23)       OPZ007
221300970526     C                   MOVEL     OPV(24)       OPZ008
221400970526     C                   END
221500970526     C*
221600970526     C                   ENDSR
221700970526     C*------------------------------------------------------------------------*
221800970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
221900970526     C*------------------------------------------------------------------------*
222000970526     C     OPZCON        BEGSR
222100970526     C*
222200970526     C* PULISCO CAMPI DI LAVORO
222300970526     C                   Z-ADD     0             I                 5 0
222400970526     C                   Z-ADD     0             W                 5 0
222500970526     C*
222600970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
222700970526     C     1             DO        24            I
222800970526     C                   MOVEL     OPD(I)        OPDRIG
222900970526     C*
223000970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
223100970526     C     OPZKEY        IFEQ      OPDOPZ
223200970526     C     OPZKEY        OREQ      OPDOP1
223300970526     C*
223400970526     C     OPDATT        IFEQ      'S'
223500970526     C                   MOVEL     OPDOPZ        OPZKEY
223600970526     C                   Z-ADD     I             W
223700970526     C                   END
223800970526     C                   END
223900970526     C*
224000970526     C                   END                                                    DO WHILE
224100970526     C*
224200970526     C* OPZIONE NON ATTIVA (ERRORE)
224300970526     C     W             IFEQ      0
224400970526     C*
224500970526     C* OPZIONE NON OK
224600970526     C                   Z-ADD     1             OPZOK
224700970526     C*
224800970526     C* EMISSIONE MESSAGGIO DI ERRORE
224900970526     C                   MOVEL     MSG(1)        DSMSMS
225000970526     C                   ELSE
225100970526     C*
225200970526     C* OPZIONE ATTIVA (PROCEDI ...)
225300970526     C*
225400970526     C     OPZKEY        IFEQ      ' 1'
225500970526     C                   EXSR      O01GES
225600970526     C                   END
225700970526     C     OPZKEY        IFEQ      ' 2'
225800970526     C                   EXSR      O02GES
225900970526     C                   END
226000970603     C     OPZKEY        IFEQ      ' 3'
226100970603     C                   EXSR      O03GES
226200970603     C                   END
226300970526     C     OPZKEY        IFEQ      ' 4'
226400970526     C                   EXSR      O04GES
226500970526     C                   END
226600970526     C     OPZKEY        IFEQ      ' 5'
226700970526     C                   EXSR      O05GES
226800970526     C                   END
226900970526     C     OPZKEY        IFEQ      ' 7'
227000970526     C                   END
227100970528     C     OPZKEY        IFEQ      ' 8'
227200970528     C                   END
227300970528     C     OPZKEY        IFEQ      ' 9'
227400970528     C                   END
227500970528     C     OPZKEY        IFEQ      '10'
227600970528     C                   END
227700970528     C     OPZKEY        IFEQ      '11'
227800970528     C                   END
227900970603     C     OPZKEY        IFEQ      '13'
228000160914     C                   EXSR      O13GES
228100970603     C                   END
228200160916     C     OPZKEY        IFEQ      '23'
228300160916     C                   EXSR      O23GES
228400160916     C                   END
228500970526     C                   END                                                    OPZ.NON ATTIVA
228600970526     C*
228700970526     C                   ENDSR
228800160831
228900160831       /free
229000160831
229100160831       //--------------------------------------------------------------
229200161122       //?Preparazione stringa SQL per Drill up                        ?
229300160831       //--------------------------------------------------------------
229400161122       BEGSR  PrepSQLDrill;
229500160831
229600161122         // Chiudo il cursore (lo faccio in anticipo perché non saprei quando chiuderlo)
229700160901         exec sql  close C1;
229800160901
229900160923         wSQL = 'select vmsproc, vmsksu, count(*), vmsdde, vmsdsc +
230000160831                 from tivms00f +
230100160916                 where vmsatb = '' ''' ;
230200160916         if D03Proc <> *blank;
230300160916           wSQL = %trim(wSQL) +
230400160919                ' and vmsproc = ''' + D03Proc + '''';
230500160916         endif;
230600160916         if D03Ksu  <> *blank;
230700160916           wSQL = %trim(wSQL) +
230800160919                ' and vmsksu = ''' + D03Ksu  + '''';
230900160916         endif;
231000160916         if D03DDE    <> 0     ;
231100160916           wSQL = %trim(wSQL) +
231200160919                ' and vmsdde <= ' + %char(%dec(%date(d03DDE:*eur))) + ' and +
231300160919                  vmsdsc >= ' + %char(%dec(%date(d03DDE:*eur)));
231400160916         endif;
231500160916         if D03CauBRT <> *blank;
231600161202           D03CauBRT = %xlate('*':'%':D03CauBRT);
231700161202           if %scan('%':D03CauBRT) > 0;
231800161202             wSQL = %trim(wSQL) +
231900161205                ' and trim(vmscaubrt) like ''' + %trim(D03CauBRT) + '''';
232000161202           else;
232100161202             wSQL = %trim(wSQL) +
232200161202                ' and vmscaubrt =    ''' + %trim(D03CauBRT) + '''';
232300161202           endif;
232400161202         endif;
232500160926         if D03StsCli <> *blank;
232600161202           D03StsCli = %xlate('*':'%':D03StsCli);
232700161202           if %scan('%':D03StsCli) > 0;
232800161202             wSQL = %trim(wSQL) +
232900161205                ' and trim(vmsstscli) like ''' + %trim(D03StsCli) + '''';
233000161202           else;
233100161202             wSQL = %trim(wSQL) +
233200161202                ' and vmsstscli =    ''' + %trim(D03StsCli) + '''';
233300161202           endif;
233400160926         endif;
233500160916         wSQL = %trim(wSQL) +
233600160926                ' group by vmsproc, vmsksu, vmsdde, vmsdsc' +
233700160926                ' order by vmsproc, vmsksu, vmsdde, vmsdsc';
233800160831
233900160831         // Dichiarazione cursore
234000160831         exec sql   prepare S1   from :wSQL;
234100160831         exec sql   declare C1  asensitive   cursor for S1;
234200160831
234300160831         // Apertura del cursore
234400160831         exec sql   open C1;
234500160831
234600160901         *in99 = *off;
234700160901         if SQLCode < 0;
234800160901           *in99 = *on;
234900160831         endif;
235000160831
235100160831       ENDSR;
235200161122
235300161122       //--------------------------------------------------------------
235400161122       //?Preparazione stringa SQL per ricerca squadrature             ?
235500161122       // le Squadrature sono quei rcd il cui valore di CauBRT non è presente in un altro rcd con
235600161122       // procedura standard (*STD)
235700161122       //--------------------------------------------------------------
235800161122       BEGSR  PrepSQLSquad;
235900161122
236000161122         // Chiudo il cursore (lo faccio in anticipo perché non saprei quando chiuderlo)
236100161122         exec sql  close C2;
236200161122
236300161122         wSQL = 'select vmsproc, vmsksu, vmscautip, vmscaubrt, vmscautt , +
236400161122                 vmsstscli , vmsdde, vmsdsc, vmsdescriz +
236500161122                 from tivms00f +
236600161122                 where vmsatb = '' '' +
236700161202                 and vmscaubrt concat vmscautip not in +
236800161202                 (select vmscaubrt concat vmscautip from tivms00f +
236900161122                 where vmsatb = '' '' +
237000161122                 and vmsproc=''*STD'')';
237100161122         if D03Proc <> *blank;
237200161122           wSQL = %trim(wSQL) +
237300161122                ' and vmsproc = ''' + D03Proc + '''';
237400161122         endif;
237500161122         if D03Ksu  <> *blank;
237600161122           wSQL = %trim(wSQL) +
237700161122                ' and vmsksu = ''' + D03Ksu  + '''';
237800161122         endif;
237900161122         if D03DDE    <> 0     ;
238000161122           wSQL = %trim(wSQL) +
238100161122                ' and vmsdde <= ' + %char(%dec(%date(d03DDE:*eur))) + ' and +
238200161122                  vmsdsc >= ' + %char(%dec(%date(d03DDE:*eur)));
238300161122         endif;
238400161122         if D03CauBRT <> *blank;
238500161202           D03CauBRT = %xlate('*':'%':D03CauBRT);
238600161202           if %scan('%':D03CauBRT) > 0;
238700161202             wSQL = %trim(wSQL) +
238800161205                ' and trim(vmscaubrt) like ''' + %trim(D03CauBRT) + '''';
238900161202           else;
239000161202             wSQL = %trim(wSQL) +
239100161202                ' and vmscaubrt =    ''' + %trim(D03CauBRT) + '''';
239200161202           endif;
239300161202         endif;
239400161202         if D03StsCli <> *blank;
239500161202           D03StsCli = %xlate('*':'%':D03StsCli);
239600161202           if %scan('%':D03StsCli) > 0;
239700161202             wSQL = %trim(wSQL) +
239800161205                ' and trim(vmsstscli) like ''' + %trim(D03StsCli) + '''';
239900161202           else;
240000161202             wSQL = %trim(wSQL) +
240100161202                ' and vmsstscli =    ''' + %trim(D03StsCli) + '''';
240200161202           endif;
240300161122         endif;
240400161122         wSQL = %trim(wSQL) +
240500161122                ' order by vmsproc, vmsksu, vmsdde, vmscautip, vmscaubrt';
240600161122
240700161122         // Dichiarazione cursore
240800161122         exec sql   prepare S2   from :wSQL;
240900161122         exec sql   declare C2  asensitive   cursor for S2;
241000161122
241100161122         // Apertura del cursore
241200161122         exec sql   open C2;
241300161122
241400161122         *in99 = *off;
241500161122         if SQLCode < 0;
241600161122           *in99 = *on;
241700161122         endif;
241800161122
241900161122       ENDSR;
242000170207
242100170207       //--------------------------------------------------------------
242200170207       //?Preparazione stringa SQL per confronto
242300170207       // Mostro quei rcd delle procedure 1 (su cui ho messo l'opzione 8) e 2 (selezionata in
242400170207       // finestra, che non hanno stato clienti O tipo causale O causale BRT
242500170207       //--------------------------------------------------------------
242600170207       BEGSR  PrepSQLConfr;
242700170207
242800170207         // Chiudo il cursore (lo faccio in anticipo perché non saprei quando chiuderlo)
242900170207         exec sql  close C3;
243000170207
243100170207         wSQL = 'select vmsproc, vmsksu, vmscautip, vmscaubrt, vmscautt , +
243200170207                 vmsstscli , vmsdde, vmsdsc, vmsdescriz +
243300170207                 from tivms00f +
243400170207                 where vmsatb = '' '' +
243500170207                 and vmsproc = ''' + proc1 + ''' +
243600170207                 and vmsstscli concat vmscaubrt concat vmscautip not in +
243700170207                 (select vmsstscli concat vmscaubrt concat vmscautip +
243800170207                 from tivms00f +
243900170207                 where vmsatb = '' '' +
244000170208                 and vmsproc= + ''' + proc2 + ''')';
244100170208         if D03Proc <> *blank;
244200170208           wSQL = %trim(wSQL) +
244300170208                ' and vmsproc = ''' + D03Proc + '''';
244400170208         endif;
244500170208         if D03Ksu  <> *blank;
244600170208           wSQL = %trim(wSQL) +
244700170208                ' and vmsksu = ''' + D03Ksu  + '''';
244800170208         endif;
244900170208         if D03DDE    <> 0     ;
245000170208           wSQL = %trim(wSQL) +
245100170208                ' and vmsdde <= ' + %char(%dec(%date(d03DDE:*eur))) + ' and +
245200170208                  vmsdsc >= ' + %char(%dec(%date(d03DDE:*eur)));
245300170208         endif;
245400170208         if D03CauBRT <> *blank;
245500170208           D03CauBRT = %xlate('*':'%':D03CauBRT);
245600170208           if %scan('%':D03CauBRT) > 0;
245700170208             wSQL = %trim(wSQL) +
245800170208                ' and trim(vmscaubrt) like ''' + %trim(D03CauBRT) + '''';
245900170208           else;
246000170208             wSQL = %trim(wSQL) +
246100170208                ' and vmscaubrt =    ''' + %trim(D03CauBRT) + '''';
246200170208           endif;
246300170208         endif;
246400170208         if D03StsCli <> *blank;
246500170208           D03StsCli = %xlate('*':'%':D03StsCli);
246600170208           if %scan('%':D03StsCli) > 0;
246700170208             wSQL = %trim(wSQL) +
246800170208                ' and trim(vmsstscli) like ''' + %trim(D03StsCli) + '''';
246900170208           else;
247000170208             wSQL = %trim(wSQL) +
247100170208                ' and vmsstscli =    ''' + %trim(D03StsCli) + '''';
247200170208           endif;
247300170208         endif;
247400170208           wSQL = %trim(wSQL) +
247500170208                ' union all +
247600170207                 select vmsproc, vmsksu, vmscautip, vmscaubrt, vmscautt , +
247700170207                 vmsstscli , vmsdde, vmsdsc, vmsdescriz +
247800170207                 from tivms00f +
247900170207                 where vmsatb = '' '' +
248000170207                 and vmsproc = ''' + proc2 + ''' +
248100170207                 and vmsstscli concat vmscaubrt concat vmscautip not in +
248200170207                 (select vmsstscli concat vmscaubrt concat vmscautip +
248300170207                 from tivms00f +
248400170207                 where vmsatb = '' '' +
248500170207                 and vmsproc= ''' + proc1 + ''')';
248600170207         if D03Proc <> *blank;
248700170207           wSQL = %trim(wSQL) +
248800170207                ' and vmsproc = ''' + D03Proc + '''';
248900170207         endif;
249000170207         if D03Ksu  <> *blank;
249100170207           wSQL = %trim(wSQL) +
249200170207                ' and vmsksu = ''' + D03Ksu  + '''';
249300170207         endif;
249400170207         if D03DDE    <> 0     ;
249500170207           wSQL = %trim(wSQL) +
249600170207                ' and vmsdde <= ' + %char(%dec(%date(d03DDE:*eur))) + ' and +
249700170207                  vmsdsc >= ' + %char(%dec(%date(d03DDE:*eur)));
249800170207         endif;
249900170207         if D03CauBRT <> *blank;
250000170207           D03CauBRT = %xlate('*':'%':D03CauBRT);
250100170207           if %scan('%':D03CauBRT) > 0;
250200170207             wSQL = %trim(wSQL) +
250300170207                ' and trim(vmscaubrt) like ''' + %trim(D03CauBRT) + '''';
250400170207           else;
250500170207             wSQL = %trim(wSQL) +
250600170207                ' and vmscaubrt =    ''' + %trim(D03CauBRT) + '''';
250700170207           endif;
250800170207         endif;
250900170207         if D03StsCli <> *blank;
251000170207           D03StsCli = %xlate('*':'%':D03StsCli);
251100170207           if %scan('%':D03StsCli) > 0;
251200170207             wSQL = %trim(wSQL) +
251300170207                ' and trim(vmsstscli) like ''' + %trim(D03StsCli) + '''';
251400170207           else;
251500170207             wSQL = %trim(wSQL) +
251600170207                ' and vmsstscli =    ''' + %trim(D03StsCli) + '''';
251700170207           endif;
251800170207         endif;
251900170207         wSQL = %trim(wSQL) +
252000170207                ' order by vmsproc, vmsksu, vmsdde, vmscautip, vmscaubrt';
252100170207
252200170207         // Dichiarazione cursore
252300170207         exec sql   prepare S3   from :wSQL;
252400170207         exec sql   declare C3  asensitive   cursor for S3;
252500170207
252600170207         // Apertura del cursore
252700170207         exec sql   open C3;
252800170207
252900170207         *in99 = *off;
253000170207         if SQLCode < 0;
253100170207           *in99 = *on;
253200170207         endif;
253300170207
253400170207       ENDSR;
253500160831
253600160831       //--------------------------------------------------------------
253700161122       //?Lettura occorrenza per drill up                              ?
253800160831       //--------------------------------------------------------------
253900161122       BEGSR  LeggeSQLDrill;
254000160831
254100161122         exec sql  fetch next  from C1  into :VMS_Drill;
254200160901
254300160901         *in99 = *off;
254400160901         if SQLCode = 100 or SQLCode < 0;
254500160901           *in99 = *on;
254600160901         endif;
254700160831
254800160831       ENDSR;
254900161122
255000161122       //--------------------------------------------------------------
255100161122       //?Lettura occorrenza per ricerca squadrature                   ?
255200161122       //--------------------------------------------------------------
255300161122       BEGSR  LeggeSQLSquad;
255400161122
255500161122         exec sql  fetch next  from C2  into :VMS_Squad;
255600161122
255700161122         *in99 = *off;
255800161122         if SQLCode = 100 or SQLCode < 0;
255900161122           *in99 = *on;
256000161122         endif;
256100161122
256200161122       ENDSR;
256300160831
256400160831       /end-free
256500170207
256600170207       //--------------------------------------------------------------
256700170207       //?Lettura occorrenza per confronto                             ?
256800170207       //--------------------------------------------------------------
256900170207       BEGSR  LeggeSQLConfr;
257000170207
257100170207         exec sql  fetch next  from C3  into :VMS_Confr;
257200170207
257300170207         *in99 = *off;
257400170207         if SQLCode = 100 or SQLCode < 0;
257500170207           *in99 = *on;
257600170207         endif;
257700170207
257800170207       ENDSR;
257900170207
258000170207       /end-free
258100160831
258200970526     C*------------------------------------------------------------------------*
258300970526     C* NOTE COSTRUZIONE SCHIERE
258400970526     C*------------------------------------------------------------------------*
258500970526     C*
258600970526     C* -MSG-MESSAGGI
258700970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
258800970526     C* (NON TOCCARE MSG,1 E MSG,2)
258900970526     C*
259000970526     C* -CMD-COMANDI
259100970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
259200970526     C*
259300970526     C* -OPD-OPZIONI
259400970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
259500970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
259600970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
259700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
259800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
259900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
260000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
260100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
260200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
260300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
260400970526     C*
260500970526     C* -FUD-FUNZIONI
260600970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
260700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
260800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
260900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
261000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
261100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
261200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
261300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
261400970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
261500970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
261600970526     O*------------------------------------------------------------------------*
261700970526** DEC - DECODIFICHE
261800970526    GESTIONE                                                     1
261900970526INTERROGAZIONE                                                   2
262000970530     SCELTA                                                      3
2621009705264
2622009705265
262300970526** MSG - MESSAGGI
262400160715TIS7VMS1R- Opzione non gestita                                                 1
262500160715TIS7VMS1R- Tasto funzionale non gestito                                        2
262600161123TIS7VMS1R-                                                                     3
262700160831TIS7VMS1R- Record annullato.                                                   4
262800160715TIS7VMS1R- Record non annullato. Impossibile ripristinare                      5
262900160715TIS7VMS1R- Accettato solo un parametro di inizio lista                         6
263000160715TIS7VMS1R- Errori nei parametri di ingresso                                    7
263100160715TIS7VMS1R- Data errata                                                         8
263200160919TIS7VMS1R-                                                                     9
263300160831TIS7VMS1R- Errore SQL nel reperimento dati Validi al                          10
263400160916TIS7VMS1R- Copia totale (opz. 23) eseguita correttamente                      11
263500160916TIS7VMS1R- Copia totale (opz. 23) non eseguita                                12
263600161123TIS7VMS1R- Stampa effettuata                                                  13
263700160715TIS7VMS1R- 14                                                                 14
263800160715TIS7VMS1R- 15                                                                 15
263900160715TIS7VMS1R- 16                                                                 16
264000160715TIS7VMS1R- 17                                                                 17
264100160715TIS7VMS1R- 18                                                                 18
264200160715TIS7VMS1R- 19                                                                 19
264300160715TIS7VMS1R- 20                                                                 20
264400970526** CMD - COMANDI CL
264500970526WRKSPLF
264600970526
264700970526
264800970526
264900970526
265000970526** OPD - OPZIONI
265100160923 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
265200970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
265300160914 3=Copia            3 -     (S/N)=N                        opz. 03¬
265400970526 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
265500970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
265600160713                   -  -     (S/N)=N                        opz. 06¬
265700160713                   -  -     (S/N)=N                        opz. 07¬
265800170208                   -  -     (S/N)=N                        opz. 08¬
265900160713                   -  -     (S/N)=N                        opz. 09¬
266000160713                   -  -     (S/N)=N                        opz. 10¬
266100160713                   -  -     (S/N)=N                        opz. 11¬
266200970526                   -  -     (S/N)=N                        opz. 12¬
26630016092313=Duplica         -13-     (S/N)=N                        opz. 13¬
266400970526                   -  -     (S/N)=N                        opz. 14¬
266500970526                   -  -     (S/N)=N                        opz. 15¬
266600970526                   -  -     (S/N)=N                        opz. 16¬
266700970526                   -  -     (S/N)=N                        opz. 17¬
266800970526                   -  -     (S/N)=N                        opz. 18¬
266900970526                   -  -     (S/N)=N                        opz. 19¬
267000970526                   -  -     (S/N)=N                        opz. 20¬
267100970526                   -  -     (S/N)=N                        opz. 21¬
267200970526                   -  -     (S/N)=N                        opz. 22¬
26730016092323=Copia su tutte  -23-     (S/N)=N                        opz. 23¬
267400970526                   -  -     (S/N)=N                        opz. 24¬
267500970526** FUD - FUNZIONI
267600970526                   -        (S/N)=N                        funz.01¬
267700970526                   -        (S/N)=N                        funz.02¬
267800970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
267900970526                   -        (S/N)=N                        funz.04¬
268000970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
268100970526F06=Inserimento    -        (S/N)=N                        funz.06¬
268200970526                   -        (S/N)=N                        funz.07¬
268300160916F08=Drill-up       -        (S/N)=N                        funz.08¬
268400160713                   -        (S/N)=N                        funz.09¬
268500160713                   -        (S/N)=N                        funz.10¬
268600160713                   -        (S/N)=N                        funz.11¬
268700160923F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
268800160713                   -        (S/N)=N                        funz.13¬
268900161121                   -        (S/N)=N                        funz.14¬
269000161121F15=Squadrature    -        (S/N)=N                        funz.15¬
269100170208F16=Confronto      -        (S/N)=N                        funz.16¬
269200970526                   -        (S/N)=N                        funz.17¬
269300970526                   -        (S/N)=N                        funz.18¬
269400970627 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
269500970627 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
269600161121F21=Stampa         -        (S/N)=N                        funz.21¬
269700161121                   -        (S/N)=N                        funz.22¬
269800970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
269900970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
