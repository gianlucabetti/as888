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
014500170523     d  DesProc                      30
014600161122     d VMS_Squad       ds                  qualified
014700161122     d  Proc                         10
014800161122     d  KSU                           8
014900161122     d  CauTip                        3
015000161122     d  CauBRT                       15
015100161122     d  CauTT                        15
015200161122     d  StsCli                       15
015300161122     d  DDE                           8s 0
015400161122     d  DSC                           8s 0
015500161122     d  DescIT                       28
015600160831     d TIVMSOccurs     s              3s 0
015700170207     d VMS_Confr       ds                  qualified
015800170207     d  Proc                         10
015900170207     d  KSU                           8
016000170207     d  CauTip                        3
016100170207     d  CauBRT                       15
016200170207     d  CauTT                        15
016300170207     d  StsCli                       15
016400170207     d  DDE                           8s 0
016500170207     d  DSC                           8s 0
016600170207     d  DescIT                       28
016700160916
016800160916       // - Parametri chiamata TIS7VMS4R
016900160916       // opz: blank=elabora  C=chiudi
017000160916     d pIn_Opz_S4      s              1a
017100160916       // Procedura
017200160916     d pIn_Proc_S4     s                   like(VMSProc)
017300160916       // Cliente unificante
017400160916     d pIn_KSU_S4      s                   like(VMSKSU)
017500160916       // Data decorrenza
017600160916     d pIn_DDE_S4      s              8s 0
017700160916       // Tipo causale
017800160916     d pIn_CauTip_S4   s                   like(VMSCAUTIP)
017900160916       // Causale BRT
018000160916     d pIn_CauBRT_S4   s                   like(VMSCAUBRT)
018100160916       // Data scadenza
018200160916     d pIn_DSC_S4      s              8s 0
018300160916       // esito: 1=OK  I=Err parametri input  2=KO SQL  3=non trovato periodo  4=trovati + periodi
018400160916     d pOut_Esito_S4   s              1a
018500160916
018600160916     d wDrillUp        s               n   inz(*off)
018700161122     d wSquad          s               n   inz(*off)
018800170207     d wConfr          s               n   inz(*off)
018900161205     D wD03CauBRT      S                   like(D03CauBRT)
019000161205     D wD03StsCli      S                   like(D03StsCli)
019100161205     d wDummy          s              1
019200170207
019300170207        // parametri TIS7VMS8R
019400170207     d Exfmt           s               n   inz(*off)
019500170207     d Proc1           s                   like(D03Proc)
019600170207     d Proc2           s                   like(D05Proc)
019700170207     D ExecSQL         S               n   inz(*off)
019800970526     C*------------------------------------------------------------------------*
019900970526     C* MAIN LINES
020000970526     C*------------------------------------------------------------------------*
020100970526     C*
020200970526     C* SE NON CI SONO ERRORI, PROSEGUE
020300970526IF  1C     ERRL00        IFNE      '1'
020400970526     C*
020500970526     C* CARICA IL SFILE
020600970526     C                   EXSR      POSSFL
020700970526     C*
020800970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
020900970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
021000970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
021100970526     C     FINL00        OREQ      '1'                                          *FINE PGM
021200970526     C*
021300970526     C* REIMPOSTA ERRORE DI PROCERURA
021400970526     C                   MOVEL     '0'           ERRL00
021500970526     C*
021600970526     C* EMETTE IL SFILE
021700970526     C                   EXSR      OUTSFL
021800970526     C*
021900970526     C* GESTIONE TASTI FUNZIONALI
022000970526     C                   EXSR      FUNCON
022100970526     C*
022200970526E   2C                   ENDDO
022300970526E   1C                   ENDIF
022400970526     C* FINE PGM
022500970526     C                   SETON                                        LR
022600970526     C*------------------------------------------------------------------------*
022700970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
022800970526     C*------------------------------------------------------------------------*
022900970526     C     F03GES        BEGSR
023000970526     C*
023100970526     C* IMPOSTA "PREMUTO F03"
023200970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
023300160713     C                   MOVEL     '1'           VMS2F03                         *DI PROCEDURA
023400160916     C*
023500970526     C                   ENDSR
023600970526     C*------------------------------------------------------------------------*
023700970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
023800970526     C*------------------------------------------------------------------------*
023900970526     C     F05GES        BEGSR
024000160923     C*
024100160923     C* se il Drill-up è attivo, con L'F5 lo disattivo
024200161123     C* se la ricerca Squadrature è attivo, con L'F5 lo disattivo
024300161123     C*
024400160923     C* disattivo il Drill-up
024500160923     C                   eval      wDrillUp = *off
024600161123     C* disattivo la ricerca squadrature
024700161123     C                   eval      wSquad   = *off
024800160923     C* siccome con l'F5 non ctrl il posizionamento, la variabile di appoggio la devo valorizzare
024900160923     C                   EVAL      DEPDrill   = wDrillUp
025000161123     C                   EVAL      DEPSquad   = wSquad
025100160923     C* gli chiedo di caricare tutto
025200160923     C                   MOVEL     'ALL'         WTIPRI
025300160923     C* cambio il tasto di funzione in Drill-up
025400160923     c                   eval      FUDRig = FUD(8)
025500160923     C                   MOVEL     'S'           FUDATT
025600160923     c                   eval      %subst(FUDRig:1:19) = 'F08=Drill-up'
025700160923     c                   eval      FUD(8) = FUDRig
025800161123     C* cambio il tasto di funzione in Squadrature
025900161123     c                   eval      FUDRig = FUD(15)
026000161123     C                   MOVEL     'S'           FUDATT
026100161123     c                   eval      %subst(FUDRig:1:19) = 'F15=Squadrature'
026200161123     c                   eval      FUD(15) = FUDRig
026300160923     C*
026400160923     c                   eval      FUNGru = 0
026500160923     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
026600970526     C*
026700970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
026800970526     C*
026900970526     C                   ENDSR
027000970526     C*------------------------------------------------------------------------*
027100970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
027200970526     C*------------------------------------------------------------------------*
027300970526     C     F06GES        BEGSR
027400970526     C*
027500970526     C* INSERIMENTO CONSENTITO SOLO IN SEDE
027600020919IF  1C     DUTLPO        IFNE      'S'
027700970526     C                   MOVEL     '1'           ERRL00
027800970526     C                   MOVEL     MSG(3)        DSMSMS
027900970526E   1C                   ENDIF
028000970526     C*
028100970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
028200160713     C                   CLEAR                   DATB
028300160713     C                   CLEAR                   DPROC
028400160713     C                   CLEAR                   DKSU
028500160831     C                   CLEAR                   DCAUTip
028600160831     C                   CLEAR                   DCAUBRT
028700160713     C                   CLEAR                   DSTSCLI
028800160713     C                   CLEAR                   DDDE
028900970529     C                   MOVEL     DEPOP0        WOP0
029000970529     C                   MOVEL     'F06'         WOP1
029100160714     C                   MOVEL     'TIS7VMS2R'   WPGM             10
029200970526     C                   EXSR      LANPGM                                       *LANCIA PGM
029300970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
029400970526E   1C                   ENDIF
029500970526     C*
029600970526     C                   ENDSR
029700160916     C*------------------------------------------------------------------------*
029800160923     C* F08GES - GESTIONE F08 -> Drill-up/down
029900160916     C*------------------------------------------------------------------------*
030000160916     C     F08GES        BEGSR
030100160916     C*
030200160923     C                   if        wDrillUp = *off
030300160916     C* attivo il Drill-up
030400160916     C                   eval      wDrillUp = *on
030500160923     C* cambio il tasto di funzione in Drill-down
030600160923     c                   eval      FUDRig = FUD(8)
030700160923     C                   MOVEL     'S'           FUDATT
030800160923     c                   eval      %subst(FUDRig:1:19) = 'F08=Drill-down'
030900160923     c                   eval      FUD(8) = FUDRig
031000161123     C* disattivo la ricerca squadrature
031100161123     C                   eval      wSquad   = *off
031200161123     C* cambio il tasto di funzione in Squadrature
031300161123     c                   eval      FUDRig = FUD(15)
031400161123     C                   MOVEL     'S'           FUDATT
031500161123     c                   eval      %subst(FUDRig:1:19) = 'F15=Squadrature'
031600161123     c                   eval      FUD(15) = FUDRig
031700160923     c                   else
031800160923     C* disattivo il Drill-up
031900160923     C                   eval      wDrillUp = *off
032000160923     C* cambio il tasto di funzione in Drill-up
032100160923     c                   eval      FUDRig = FUD(8)
032200160923     C                   MOVEL     'S'           FUDATT
032300160923     c                   eval      %subst(FUDRig:1:19) = 'F08=Drill-up'
032400160923     c                   eval      FUD(8) = FUDRig
032500160923     c                   endif
032600160923     C*
032700160923     c                   eval      FUNGru = 0
032800160923     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
032900160916     C*
033000160916     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
033100160916     C                   EXSR      NEWPOS
033200160916     C*
033300160916     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
033400160916IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033500160916IF  2C     SWPOSI        IFNE      0
033600160916     C                   EXSR      POSSFL                                       *RICARICA SFILE
033700160916X   2C                   ELSE
033800160916IF  3C     SFLULT        IFNE      0
033900160916     C                   EXSR      ENTATT                                       *GESTIONE ENTER
034000160916     C*
034100160916     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
034200160916IF  4C     SWMODI        IFNE      0
034300160916     C                   EXSR      POSSFL
034400160916E   4C                   ENDIF
034500160916E   3C                   ENDIF
034600160916E   2C                   ENDIF
034700160916E   1C                   ENDIF
034800160919     C*
034900160916     C                   ENDSR
035000970526     C*------------------------------------------------------------------------*
035100970526     C* F12GES - GESTIONE F12 -> RITORNO
035200970526     C*------------------------------------------------------------------------*
035300970526     C     F12GES        BEGSR
035400970526     C*
035500970526     C* IMPOSTA "PREMUTO F12"
035600970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
035700160713     C                   MOVEL     '1'           VMS2F12                         *DI PROCEDURA
035800970526     C*
035900970526     C                   ENDSR
036000161121     C*------------------------------------------------------------------------*
036100170208     C* F15GES - GESTIONE F15 -> SQUADRATURE
036200161121     C*------------------------------------------------------------------------*
036300161121     C     F15GES        BEGSR
036400161121     C*
036500161122     C                   if        wSquad = *off
036600161122     C* attivo le squadrature
036700161122     C                   eval      wSquad = *on
036800161122     C* cambio il tasto di funzione in Normale
036900170208     c                   eval      FUDRig = FUD(15)
037000161122     C                   MOVEL     'S'           FUDATT
037100161122     c                   eval      %subst(FUDRig:1:19) = 'F15=Normale'
037200161122     c                   eval      FUD(15) = FUDRig
037300161123     C* disattivo il Drill-up
037400161123     C                   eval      wDrillUp = *off
037500161123     C* cambio il tasto di funzione in Drill-up
037600161123     c                   eval      FUDRig = FUD(8)
037700161123     C                   MOVEL     'S'           FUDATT
037800161123     c                   eval      %subst(FUDRig:1:19) = 'F08=Drill-up'
037900161123     c                   eval      FUD(8) = FUDRig
038000161122     c                   else
038100161122     C* disattivo la ricerca squadrature
038200161122     C                   eval      wSquad   = *off
038300161122     C* cambio il tasto di funzione in Squadrature
038400161122     c                   eval      FUDRig = FUD(15)
038500161122     C                   MOVEL     'S'           FUDATT
038600161122     c                   eval      %subst(FUDRig:1:19) = 'F15=Squadrature'
038700161122     c                   eval      FUD(15) = FUDRig
038800161122     c                   endif
038900161122     C*
039000161122     c                   eval      FUNGru = 0
039100161122     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
039200161122     C*
039300161122     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
039400161122     C                   EXSR      NEWPOS
039500161122     C*
039600161122     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
039700161122IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
039800161122IF  2C     SWPOSI        IFNE      0
039900161122     C                   EXSR      POSSFL                                       *RICARICA SFILE
040000161122X   2C                   ELSE
040100161122IF  3C     SFLULT        IFNE      0
040200161122     C                   EXSR      ENTATT                                       *GESTIONE ENTER
040300161122     C*
040400161122     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
040500161122IF  4C     SWMODI        IFNE      0
040600161122     C                   EXSR      POSSFL
040700161122E   4C                   ENDIF
040800161122E   3C                   ENDIF
040900161122E   2C                   ENDIF
041000161122E   1C                   ENDIF
041100161121     C*
041200161121     C                   ENDSR
041300170208     C*------------------------------------------------------------------------*
041400170208     C* F16GES - GESTIONE F16 CONFRONTA
041500170208     C*------------------------------------------------------------------------*
041600170208     C     F16GES        BEGSR
041700170208     C*
0418001702081    C                   if        wConfr = *off
041900170208
042000170208     C* emetto finestra per conferma richiesta confronto e tra chi
042100170208
042200170208     C* siccome questo pgm si chiama tramite un comando, è ovvio che ci sia stata una exfmt del sfl
042300170208     C                   EVAL      Exfmt = *on
042400170208     C* richiedi procedura da confrontare
042500170208     C                   CALL      'TIS7VMS8R'
042600170208     C                   PARM                    Kpjba
042700170208     C                   PARM                    Exfmt
042800170208     C                   PARM                    Proc1
042900170208     C                   PARM                    Proc2
043000170208     C                   PARM                    ExecSQL
043100170208     C*
043200170208     C* tolgo memo che c'è stata una exfmt del sfl
043300170208     C                   EVAL      Exfmt = *off
043400170208     C* CONFRONTA
0435001702082    C                   IF        ExecSQL = *on
043600170208     C* attivo il confrontore
043700170208     C                   eval      wConfr = *on
043800170208     C* cambio il tasto di funzione in Lista
043900170208     c                   eval      FUDRig = FUD(16)
044000170208     C                   MOVEL     'S'           FUDATT
044100170208     c                   eval      %subst(FUDRig:1:13) = 'F16=Lista    '
044200170208     c                   eval      FUD(16) = FUDRig
0443001702082e   c                   endif
044400170208
0445001702081x   c                   else
044600170208     C* disattivo il confronto
044700170208     C                   eval      wConfr   = *off
044800170208     C* cambio il tasto di funzione in Squadrature
044900170208     c                   eval      FUDRig = FUD(16)
045000170208     C                   MOVEL     'S'           FUDATT
045100170208     c                   eval      %subst(FUDRig:1:13) = 'F16=Confronto'
045200170208     c                   eval      FUD(16) = FUDRig
0453001702081e   c                   endif
045400170208     C*
045500170208     c                   eval      FUNGru = 0
045600170208     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
045700170208     C*
045800170208     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
045900170208     C                   EXSR      NEWPOS
046000170208     C*
046100170208     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
0462001702081   1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
0463001702082   2C     SWPOSI        IFNE      0
046400170208     C                   EXSR      POSSFL                                       *RICARICA SFILE
0465001702082x  2C                   ELSE
0466001702083    C     SFLULT        IFNE      0
046700170208     C                   EXSR      ENTATT                                       *GESTIONE ENTER
046800170208     C*
046900170208     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
0470001702084    C     SWMODI        IFNE      0
047100170208     C                   EXSR      POSSFL
0472001702084e   C                   ENDIF
0473001702083e   C                   ENDIF
0474001702082e   C                   ENDIF
0475001702081e   C                   ENDIF
047600170208     C*
047700170208     C                   ENDSR
047800970526     C*------------------------------------------------------------------------*
047900161121     C* F21GES - GESTIONE F21 -> GESTIONE STAMPE
048000970526     C*------------------------------------------------------------------------*
048100161121     C     F21GES        BEGSR
048200970526     C*
048300161123     C                   eval      Param6DS.Proc   = D03Proc
048400161123     C                   eval      Param6DS.Ksu    = D03Ksu
048500161123     C                   if        DDEISO <> *loval
048600161123     C                   eval      Param6DS.DDE    = %dec(DDEISO)
048700161123     C                   else
048800161123     C                   eval      Param6DS.DDE    = 0
048900161123     C                   endif
049000161123     C                   eval      Param6DS.CauBRT = D03CauBRT
049100161123     C                   eval      Param6DS.StsCli = D03StsCli
049200161123     C                   eval      Param6DS.DrillUp = wDrillUp
049300161123     C                   eval      Param6DS.Squad   = wSquad
049400170207     C                   clear                   kpjbu
049500161123     C                   eval      kpjbu = Param6DS
049600970526     C*
049700161123     C                   call      'TIS7VMS6R'
049800161123     C                   parm                    kpjba
049900161123     C*
050000161123     C                   MOVEL     MSG(13)       DSMSMS
050100161123     C*
050200970526     C                   ENDSR
050300161121     C*------------------------------------------------------------------------*
050400161121     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
050500161121     C*------------------------------------------------------------------------*
050600161121     C     F22GES        BEGSR
050700161121     C*
050800161121     C                   Z-ADD     80            LENGH            15 5
050900161121     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
051000161121     C                   PARM                    CMD(1)
051100161121     C                   PARM                    LENGH
051200161121     C*
051300161121     C                   ENDSR
051400970526     C*------------------------------------------------------------------------*
051500970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
051600970526     C*------------------------------------------------------------------------*
051700970526     C     F23GES        BEGSR
051800970526     C*
051900970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
052000970526     C*
052100970526     C                   ENDSR
052200970526     C*------------------------------------------------------------------------*
052300970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
052400970526     C*------------------------------------------------------------------------*
052500970526     C     F24GES        BEGSR
052600970526     C*
052700970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
052800970526     C*
052900970526     C                   ENDSR
053000970526     C*------------------------------------------------------------------------*
053100970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
053200970526     C*------------------------------------------------------------------------*
053300970526     C     RUPGES        BEGSR
053400970526     C*
053500970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
053600970526     C*
053700970526     C                   ENDSR
053800970526     C*------------------------------------------------------------------------*
053900970526     C* ENTGES - GESTIONE ENTER -> INVIO
054000970526     C*------------------------------------------------------------------------*
054100970526     C     ENTGES        BEGSR
054200970526     C*
054300970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
054400970526     C                   EXSR      NEWPOS
054500970526     C*
054600970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
054700970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
054800970526IF  2C     SWPOSI        IFNE      0
054900970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
055000970526X   2C                   ELSE
055100970526IF  3C     SFLULT        IFNE      0
055200970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
055300970526     C*
055400970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
055500970526IF  4C     SWMODI        IFNE      0
055600970526     C                   EXSR      POSSFL
055700970526E   4C                   ENDIF
055800970526E   3C                   ENDIF
055900970526E   2C                   ENDIF
056000970526E   1C                   ENDIF
056100970526     C*
056200970526     C                   ENDSR
056300970526     C*------------------------------------------------------------------------*
056400970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
056500970526     C*------------------------------------------------------------------------*
056600970526     C     O01GES        BEGSR
056700970526     C*
056800160923     C* valorizzo Procedura, Unificante, Decorrenza con quelli del rcd scelto
056900160923     C* ed eseguo l'enter (in pratica vado in dettaglio)
057000160923     c                   eval      D03Proc = D05Proc
057100160923     c                   eval      D03Ksu  = D05Ksu
057200160923     c                   eval      D03DDE  = D05DDE
057300160923     C*
057400160923     C* se il Drill-up è attivo, lo disattivo
057500160923     C                   if        wDrillUp = *on
057600160923     C* disattivo il Drill-up
057700160923     C                   eval      wDrillUp = *off
057800160923     C* cambio il tasto di funzione in Drill-up
057900160923     c                   eval      FUDRig = FUD(8)
058000160923     C                   MOVEL     'S'           FUDATT
058100160923     c                   eval      %subst(FUDRig:1:19) = 'F08=Drill-up'
058200160923     c                   eval      FUD(8) = FUDRig
058300160923     C*
058400160923     c                   eval      FUNGru = 0
058500160923     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
058600160923     C*
058700160923     C* simulo l'enter
058800160923     C                   exsr      NEWPOS
058900160923     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
059000160923     C*
059100160923     c                   endif
059200160923     C*
059300970526     C                   ENDSR
059400970526     C*------------------------------------------------------------------------*
059500970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
059600970526     C*------------------------------------------------------------------------*
059700970526     C     O02GES        BEGSR
059800160927     C*
059900160927     C* se il Drill-up è attivo, il pgm da chiamare è un altro
060000160927     C                   if        wDrillUp = *on
060100160927     C                   exsr      O02GesDrillUp
060200160927     c                   leavesr
060300160927     c                   endif
060400970526     C*
060500970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
060600160712IF  1C     D05ATB        IFEQ      'A'
060700970526     C                   MOVEL     '1'           ERRL00
060800970526     C                   MOVEL     MSG(4)        DSMSMS
060900970526E   1C                   ENDIF
061000970526     C* MODIFICA
061100970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
061200160713     C                   EVAL      DATB        = D05ATB
061300160713     C                   EVAL      DProc       = D05Proc
061400160713     C                   EVAL      DKSU        = D05KSU
061500160831     C                   EVAL      DCauTip     = D05CauTip
061600160831     C                   EVAL      DCauBRT     = D05CauBRT
061700160713     C                   EVAL      DStsCli     = D05StsCli
061800160914     C                   EVAL      DDDE        = %dec(%date(D05DDE:*dmy))
061900970529     C                   MOVEL     DEPOP0        WOP0
062000970526     C                   MOVEL     'O02'         WOP1
062100160715     C                   MOVEL     'TIS7VMS2R'   WPGM
062200970526     C                   EXSR      LANPGM
062300970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
062400970526E   1C                   ENDIF
062500970526     C*
062600160927     C                   ENDSR
062700160927     C*
062800160914     C*------------------------------------------------------------------------*
062900160927     C* O02GesDrillUp - GESTIONE OPZIONE 2 per DrillUp
063000160914     C*------------------------------------------------------------------------*
063100160927     C     O02GesDrillUp BEGSR
063200160914     C*
063300160927     C* IMPOSTA LA DS DI PROCEDURA
063400160927     C                   MOVEL     '2'           VMS5Rich                        *Richiesta
063500160927     C                   MOVEL     '0'           VMS2F03                         *PREMUTO F03=FINE
063600160927     C                   MOVEL     '0'           VMS2F12                         *PREMUTO F12=RIT
063700160927     C                   MOVEL     '0'           VMS2ERR                         *'1'=ERRORE
063800160927     C                   MOVEL     *BLANKS       VMS2MSG                         *MESSAGGIO DI RIT
063900160927     C                   EVAL      VMS5Proc   = D05Proc
064000160927     C                   EVAL      VMS5KSU    = D05KSU
064100160927     C                   EVAL      VMS5DDE    = %dec(%date(D05DDE:*dmy))
064200160927     C                   EVAL      VMS5DSC    = %dec(%date(D05DSC:*dmy))
064300170523     C                   EVAL      VMS5DesPro   = D05DesProc
064400160927     C*
064500160927     C                   CALL      'TIS7VMS5R'
064600160927     C                   PARM                    KPJBA
064700160927     C                   PARM                    TIS7VMS5DS
064800160927     C*
064900160927     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
065000160927IF  1C     VMS5F03       IFEQ      '1'                                          *PREMUTO F03
065100160927     C                   MOVEL     '1'           ERRL00                         *ERRORE
065200160927     C                   MOVEL     '1'           F03L00                         *FINE PGM
065300160927X   1C                   ELSE                                                   *NON PREMUTO F03
065400160927     C                   MOVEL     VMS5MSG       DSMSMS                         *MESSAGGIO DI RIT
065500160927E   1C                   ENDIF
065600160927     C*
065700160927     C                   ENDSR
065800160927     C*------------------------------------------------------------------------*
065900160927     C* O03GES - GESTIONE OPZIONE 3=DUPLICA
066000160927     C*------------------------------------------------------------------------*
066100160927     C     O03GES        BEGSR
066200160927     C*
066300160914     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
066400160914IF  1C     D05ATB        IFEQ      'A'
066500160914     C                   MOVEL     '1'           ERRL00
066600160914     C                   MOVEL     MSG(4)        DSMSMS
066700160914E   1C                   ENDIF
066800160914     C* COPIA
066900160914IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
067000160914     C                   EVAL      DATB        = D05ATB
067100160914     C                   EVAL      DProc       = D05Proc
067200160914     C                   EVAL      DKSU        = D05KSU
067300160914     C                   EVAL      DCauTip     = D05CauTip
067400160914     C                   EVAL      DCauBRT     = D05CauBRT
067500160914     C                   EVAL      DStsCli     = D05StsCli
067600160914     C                   EVAL      DDDE        = %dec(%date(D05DDE:*dmy))
067700160914     C                   MOVEL     DEPOP0        WOP0
067800160914     C                   MOVEL     'O03'         WOP1
067900160914     C                   MOVEL     'TIS7VMS2R'   WPGM
068000160914     C                   EXSR      LANPGM
068100160914     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
068200160914E   1C                   ENDIF
068300160914     C*
068400160914     C                   ENDSR
068500970603     C*------------------------------------------------------------------------*
068600160914     C* O13GES - GESTIONE OPZIONE 13=DUPLICA
068700970603     C*------------------------------------------------------------------------*
068800160914     C     O13GES        BEGSR
068900970603     C*
069000970603     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
069100160713IF  1C     D05ATB        IFEQ      'A'
069200160713     C                   MOVEL     '1'           ERRL00
069300160713     C                   MOVEL     MSG(4)        DSMSMS
069400160713E   1C                   ENDIF
069500970603     C* DUPLICA
069600970603IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
069700160713     C                   EVAL      DATB        = D05ATB
069800160713     C                   EVAL      DProc       = D05Proc
069900160713     C                   EVAL      DKSU        = D05KSU
070000160831     C                   EVAL      DCauTip     = D05CauTip
070100160713     C                   EVAL      DCauBRT     = D05CauBRT
070200160713     C                   EVAL      DStsCli     = D05StsCli
070300160914     C                   EVAL      DDDE        = %dec(%date(D05DDE:*dmy))
070400970603     C                   MOVEL     DEPOP0        WOP0
070500970603     C                   MOVEL     'O02'         WOP1
070600160715     C                   MOVEL     'TIS7VMS3R'   WPGM
070700970603     C                   EXSR      LANPGM
070800970603     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
070900970603E   1C                   ENDIF
071000970603     C*
071100970603     C                   ENDSR
071200160915     C*------------------------------------------------------------------------*
071300160915     C* O23GES - GESTIONE OPZIONE 23=COPIA SU TUTTE
071400160915     C*------------------------------------------------------------------------*
071500160915     C     O23GES        BEGSR
071600160915     C*
071700160915     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
071800160915IF  1C     D05ATB        IFEQ      'A'
071900160915     C                   MOVEL     '1'           ERRL00
072000160915     C                   MOVEL     MSG(4)        DSMSMS
072100160915E   1C                   ENDIF
072200160915     C* copia su tutte le procedure/clienti unificante
072300160915IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
072400160916     C                   EVAL      pIn_Proc_S4   = D05Proc
072500160916     C                   EVAL      pIn_KSU_S4    = D05KSU
072600160916     C                   EVAL      pIn_DDE_S4    = %dec(%date(D05DDE:*dmy))
072700160916     C                   EVAL      pIn_CauTip_S4 = D05CauTip
072800160916     C                   EVAL      pIn_CauBRT_S4 = D05CauBRT
072900160916     C                   EVAL      pIn_DSC_S4    = %dec(%date(D05DSC:*dmy))
073000160916     c                   call      'TIS7VMS4R'
073100160916     c                   parm      *blank        pIn_Opz_S4
073200160916     c                   parm                    pIn_Proc_S4
073300160916     c                   parm                    pIn_KSU_S4
073400160916     c                   parm                    pIn_DDE_S4
073500160916     c                   parm                    pIn_CauTip_S4
073600160916     c                   parm                    pIn_CauBRT_S4
073700160916     c                   parm                    pIn_DSC_S4
073800160916     c                   parm                    pOut_Esito_S4
073900160916
074000160916     c                   if        pOut_Esito_S4 <> '1'
074100160916     C                   MOVEL     MSG(12)       DSMSMS
074200160916     C                   MOVEL     '1'           ERRL00
074300160916     c                   else
074400160916     C                   MOVEL     MSG(11)       DSMSMS
074500160915     C                   Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
074600160916     c                   endif
074700160916
074800160915E   1C                   ENDIF
074900160915     C*
075000160915     C                   ENDSR
075100970526     C*------------------------------------------------------------------------*
075200970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
075300970526     C*------------------------------------------------------------------------*
075400970526     C     O04GES        BEGSR
075500160927     C*
075600160927     C* se il Drill-up è attivo, il pgm da chiamare è un altro
075700160927     C                   if        wDrillUp = *on
075800160927     C                   exsr      O04GesDrillUp
075900160927     c                   leavesr
076000160927     c                   endif
076100160902     C*
076200160902     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
076300160902IF  1C     D05ATB        IFEQ      'A'
076400160902     C                   MOVEL     '1'           ERRL00
076500160902     C                   MOVEL     MSG(4)        DSMSMS
076600160902E   1C                   ENDIF
076700970526     C*
076800970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
076900160902IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
077000160713     C                   EVAL      DATB        = D05ATB
077100160713     C                   EVAL      DProc       = D05Proc
077200160713     C                   EVAL      DKSU        = D05KSU
077300160831     C                   EVAL      DCauTip     = D05CauTip
077400160713     C                   EVAL      DCauBRT     = D05CauBRT
077500160713     C                   EVAL      DStsCli     = D05StsCli
077600160914     C                   EVAL      DDDE        = %dec(%date(D05DDE:*dmy))
077700970529     C                   MOVEL     DEPOP0        WOP0
077800970526     C                   MOVEL     'O04'         WOP1
077900160714     C                   MOVEL     'TIS7VMS2R'   WPGM
078000970526     C                   EXSR      LANPGM
078100160919     C***                Z-ADD     1             SWMODI                         *FLAG RICARICA SFL
078200970526E   1C                   ENDIF
078300970526     C*
078400970526     C                   ENDSR
078500160927     C*------------------------------------------------------------------------*
078600160927     C* O04GesDrillUp - GESTIONE OPZIONE 4 per DrillUp
078700160927     C*------------------------------------------------------------------------*
078800160927     C     O04GesDrillUp BEGSR
078900160927     C*
079000160927     C* IMPOSTA LA DS DI PROCEDURA
079100160927     C                   MOVEL     '4'           VMS5Rich                        *Richiesta
079200160927     C                   MOVEL     '0'           VMS2F03                         *PREMUTO F03=FINE
079300160927     C                   MOVEL     '0'           VMS2F12                         *PREMUTO F12=RIT
079400160927     C                   MOVEL     '0'           VMS2ERR                         *'1'=ERRORE
079500160927     C                   MOVEL     *BLANKS       VMS2MSG                         *MESSAGGIO DI RIT
079600160927     C                   EVAL      VMS5Proc   = D05Proc
079700160927     C                   EVAL      VMS5KSU    = D05KSU
079800160927     C                   EVAL      VMS5DDE    = %dec(%date(D05DDE:*dmy))
079900160927     C                   EVAL      VMS5DSC    = %dec(%date(D05DSC:*dmy))
080000170523     C                   EVAL      VMS5DesPro   = D05DesProc
080100160927     C*
080200160927     C                   CALL      'TIS7VMS5R'
080300160927     C                   PARM                    KPJBA
080400160927     C                   PARM                    TIS7VMS5DS
080500160927     C*
080600160927     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
080700160927IF  1C     VMS5F03       IFEQ      '1'                                          *PREMUTO F03
080800160927     C                   MOVEL     '1'           ERRL00                         *ERRORE
080900160927     C                   MOVEL     '1'           F03L00                         *FINE PGM
081000160927X   1C                   ELSE                                                   *NON PREMUTO F03
081100160927     C                   MOVEL     VMS5MSG       DSMSMS                         *MESSAGGIO DI RIT
081200160927E   1C                   ENDIF
081300160927     C*
081400160927     C                   ENDSR
081500970526     C*------------------------------------------------------------------------*
081600970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
081700970526     C*------------------------------------------------------------------------*
081800970526     C     O05GES        BEGSR
081900970526     C*
082000160713     C                   EVAL      DATB        = D05ATB
082100160713     C                   EVAL      DProc       = D05Proc
082200160713     C                   EVAL      DKSU        = D05KSU
082300160831     C                   EVAL      DCauTip     = D05CauTip
082400160713     C                   EVAL      DCauBRT     = D05CauBRT
082500160713     C                   EVAL      DStsCli     = D05StsCli
082600160914     C                   EVAL      DDDE        = %dec(%date(D05DDE:*dmy))
082700970529     C                   MOVEL     DEPOP0        WOP0
082800970526     C                   MOVEL     'O05'         WOP1
082900160714     C                   MOVEL     'TIS7VMS2R'   WPGM
083000970526     C                   EXSR      LANPGM
083100970526     C*
083200970526     C                   ENDSR
083300970526     C*------------------------------------------------------------------------*
083400970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
083500970526     C*------------------------------------------------------------------------*
083600970526     C     O07GES        BEGSR
083700970526     C*
083800970526     C*
083900970526     C                   ENDSR
084000970526     C*------------------------------------------------------------------------*
084100970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
084200970526     C*------------------------------------------------------------------------*
084300970526     C     NEWPOS        BEGSR
084400970526     C*
084500970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
084600970526     C*         1= RICHIESTO POSIZIONAMENTO
084700970526     C                   Z-ADD     0             SWPOSI            7 0
084800970526     C*
084900970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
085000970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
085100970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
085200970526     C                   Z-ADD     0             SWMODI            1 0
085300970526     C*
085400970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
085500160713IF  1C     D03Proc       IFNE      DEPProc
085600160713     C     D03KSU        ORNE      DEPKSU
085700160913     C     D03DDE        ORNE      DEPDDE
085800160913     C     D03CAUBRT     ORNE      DEPCAUBRT
085900160926     C     D03STSCLI     ORNE      DEPSTSCLI
086000160919     C     wDrillUp      ORNE      DEPDrill
086100161122     C     wSquad        ORNE      DEPSquad
086200170207     C     wConfr        ORNE      DEPConfr
086300970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
086400970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
086500970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
086600160713     C                   EVAL      DEPProc = D03Proc
086700160713     C                   EVAL      DEPKSU  = D03KSU
086800160913     C                   EVAL      DEPDDE  = D03DDE
086900160913     C                   EVAL      DEPCAUBRT  = D03CAUBRT
087000160926     C                   EVAL      DEPSTSCLI  = D03STSCLI
087100160919     C                   EVAL      DEPDrill   = wDrillUp
087200161122     C                   EVAL      DEPSquad   = wSquad
087300170207     C                   EVAL      DEPConfr   = wConfr
087400970526E   2C                   ENDIF
087500970526E   1C                   ENDIF
087600970526     C*
087700970526     C                   ENDSR
087800970526     C*------------------------------------------------------------------------*
087900970526     C* SETFIL - POSIZIONAMENTO RECORD
088000970526     C*------------------------------------------------------------------------*
088100970526     C     SETFIL        BEGSR
088200970526     C*
088300970526     C* POSIZIONAMENTO DEL FILE E PRIMA LETTURA IN BASE AL TIPO DI RICERCA
088400160830     C                   SELECT
088500160831IF  1C     WTIPRI        WHENEQ    'ALL'
088600160831     C     *loval        SETLL     TIVMS01I                           99
088700970526     C*
088800160831IF  1C     WTIPRI        WHENEQ    'PRO'
088900160831     C                   EVAL      VMSProc = D03Proc
089000160831     C     K01VMS01      SETLL     TIVMS01I                           99
089100160831     C*
089200160831IF  1C     WTIPRI        WHENEQ    'KSU'
089300160830     C                   EVAL      VMSKSU  = D03KSU
089400160830     C     K01VMS04      SETLL     TIVMS04I                           99
089500160831     C*
089600160831IF  1C     WTIPRI        WHENEQ    'VAL'
089700160831     C* i dati li reperisce mediante SQL
089800161122     C                   EXSR      PrepSQLDrill
089900161122     C*
090000161122IF  1C     WTIPRI        WHENEQ    'SQD'
090100161122     C* i dati li reperisce mediante SQL
090200161122     C                   EXSR      PrepSQLSquad
090300170207     C*
090400170207IF  1C     WTIPRI        WHENEQ    'CFR'
090500170207     C* i dati li reperisce mediante SQL
090600170207     C                   EXSR      PrepSQLConfr
090700160830     C*
090800160830E   1C                   ENDSL
090900970526     C* LETTURA RECORD
091000970526IF  1C     *IN99         IFEQ      *OFF
091100970526     C                   EXSR      LETFIL
091200970526E   1C                   ENDIF
091300970526     C*
091400970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
091500970526IF  1C     *IN99         IFEQ      *ON
091600970526     C                   Z-ADD     1             SFLEOF
091700970526E   1C                   ENDIF
091800970526     C*
091900970526     C                   ENDSR
092000970526     C*------------------------------------------------------------------------*
092100970526     C* LETFIL - LETTURA RECORD
092200970526     C*------------------------------------------------------------------------*
092300970526     C     LETFIL        BEGSR
092400970526     C*
092500970526     C* LEGGE FINO A:
092600970526     C*     - FINE FILE
092700970526     C*     - TROVATO RECORD VALIDO
092800970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
092900970526DO  1C     *IN99         DOWEQ     *OFF
093000970526     C     WRECOK        ANDEQ     'N'
093100970526     C*
093200970526     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO DI RICERCA
093300160830     C                   SELECT
093400160831IF  1C     WTIPRI        WHENEQ    'ALL'
093500160831     C                   READ      TIVMS01I                               99
093600160830     C*
093700160830IF  2C     WTIPRI        WHENEQ    'PRO'
093800160830     C     K01VMS01      READE     TIVMS01I                               99
093900160830     C*
094000160830IF  2C     WTIPRI        WHENEQ    'KSU'
094100160830     C     K01VMS04      READE     TIVMS04I                               99
094200160831     C*
094300160831IF  1C     WTIPRI        WHENEQ    'VAL'
094400160831     C* i dati li reperisce mediante SQL
094500161122     C                   EXSR      LeggeSQLDrill
094600161122     C*
094700161122IF  1C     WTIPRI        WHENEQ    'SQD'
094800161122     C* i dati li reperisce mediante SQL
094900161122     C                   EXSR      LeggeSQLSquad
095000170207     C*
095100170207IF  1C     WTIPRI        WHENEQ    'CFR'
095200170207     C* i dati li reperisce mediante SQL
095300170207     C                   EXSR      LeggeSQLConfr
095400160830     C*
095500160830E   2C                   ENDSL
095600970526     C*
095700970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
095800970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
095900970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
096000970530X   2C                   ELSE
096100970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
096200970530E   2C                   ENDIF
096300970526E   1C                   ENDDO
096400970526     C*
096500970526     C                   ENDSR
096600970526     C*------------------------------------------------------------------------*
096700970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
096800970526     C*------------------------------------------------------------------------*
096900970526     C     CHKREC        BEGSR
097000970526     C*
097100970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
097200161205     C* se sono in DrillUp o in ricerca squadrature, il file lo leggo mediante SQL, per cui sono
097300161202     C* certo del risultato
097400161202     C* (i test sotto li lascio nel caso passi da sql a lettura)
097500161202IF  1C                   if        wDrillUp  =  *on  or
097600170207IF  1C                             wSquad    =  *on  or
097700170207IF  1C                             wConfr    =  *on
097800161202     C                   leavesr
097900161202     C                   endif
098000160913     C*
098100160919     C* Proc diversa
098200161102IF  1C                   if        wDrillUp  =  *off and
098300161122IF  1C                             wSquad    =  *off and
098400160927IF  1C                             D03Proc<> *blank and
098500160927IF  1C                             D03Proc<> VMSProc
098600970526     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
098700161102     C                   leavesr
098800161102     C                   endif
098900160927     C*
099000160927     C* Proc diversa
099100160927     C* se sono in DrillUp, il file lo leggo mediante SQL
099200161102IF  1C                   if        wDrillUp  =  *on  and
099300160927IF  1C                             D03Proc<> *blank and
099400161122IF  1C                             D03Proc<> VMS_Drill.Proc
099500160927     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
099600161102     C                   leavesr
099700161102     C                   endif
099800161122     C*
099900161122     C* Proc diversa
100000161122     C* se sono in ricerca Squadrature, il file lo leggo mediante SQL
100100161122IF  1C                   if        wSquad    =  *on  and
100200161122IF  1C                             D03Proc<> *blank and
100300161122IF  1C                             D03Proc<> VMS_Squad.Proc
100400161122     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
100500161122     C                   leavesr
100600161122     C                   endif
100700170207     C*
100800170207     C* Proc diversa
100900170207     C* se sono in confronto, il file lo leggo mediante SQL
101000170207IF  1C                   if        wConfr    =  *on  and
101100170207IF  1C                             D03Proc<> *blank and
101200170207IF  1C                             D03Proc<> VMS_Confr.Proc
101300170207     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
101400170207     C                   leavesr
101500170207     C                   endif
101600160919     C*
101700160919     C* KSU diverso
101800161102IF  1C                   if        wDrillUp  =  *off and
101900161122IF  1C                             wSquad    =  *off and
102000160927IF  1C                             D03KSU <> *blank and
102100160919IF  1C                             D03KSU <> VMSKSU
102200160919     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
102300161102     C                   leavesr
102400161102     C                   endif
102500160927     C*
102600160927     C* KSU diverso
102700161102IF  1C                   if        wDrillUp  =  *on  and
102800160927IF  1C                             D03KSU <> *blank and
102900161122IF  1C                             D03KSU <> VMS_Drill.KSU
103000160927     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
103100161102     C                   leavesr
103200161102     C                   endif
103300161122     C*
103400161122     C* KSU diverso
103500161122IF  1C                   if        wSquad    =  *on  and
103600161122IF  1C                             D03KSU <> *blank and
103700161122IF  1C                             D03KSU <> VMS_Squad.KSU
103800161122     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
103900161122     C                   leavesr
104000161122     C                   endif
104100170207     C*
104200170207     C* KSU diverso
104300170207IF  1C                   if        wConfr    =  *on  and
104400170207IF  1C                             D03KSU <> *blank and
104500170207IF  1C                             D03KSU <> VMS_Confr.KSU
104600170207     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
104700170207     C                   leavesr
104800170207     C                   endif
104900160919     C*
105000160919     C* RECORD CON DECORRENZA MINORE DEL GIORNO RICHIESTO, ESCLUDE
105100161102IF  1C                   if        D03DDE   > *ZEROS
105200161122     C                   SELECT
105300160919     C* se sono in DrillUp, il file lo leggo mediante SQL
105400161122IF  1C                   WHEN      wDrillUp  =  *on
105500160919     C* quindi i campi non sono quelli del file ma quelli della fetch
105600160919     C                   IF        (%date(d03DDE:*eur) <
105700161122     C                              %date(VMS_Drill.DDE)               OR
105800160919     C                              %date(d03DDE:*eur) >
105900161122     C                              %date(VMS_Drill.DSC))
106000160919     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
106100161102     C                   leavesr
106200160919E   2C                   ENDIF
106300161122     C* se NON sono in DrillUp né in ricerca Squadrature
106400161122IF  1C                   WHEN      wDrillUp  =  *off and
106500161122IF  1C                             wSquad    =  *off
106600160919     C                   IF        (%date(d03DDE:*eur) <
106700160919     C                              %date(VMSDDE)               OR
106800160919     C                              %date(d03DDE:*eur) >
106900160919     C                              %date(VMSDSC))
107000160919     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
107100161102     C                   leavesr
107200160919E   2C                   ENDIF
107300161122     C* se sono in ricerca Squadrature
107400161122IF  1C                   WHEN      wSquad    =  *on
107500161122     C                   IF        (%date(d03DDE:*eur) <
107600161122     C                              %date(VMS_Squad.DDE)               OR
107700161122     C                              %date(d03DDE:*eur) >
107800161122     C                              %date(VMS_Squad.DSC))
107900161122     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
108000161122     C                   leavesr
108100161122E   2C                   ENDIF
108200170207     C* se sono in Confronto
108300170207IF  1C                   WHEN      wConfr    =  *on
108400170207     C                   IF        (%date(d03DDE:*eur) <
108500170207     C                              %date(VMS_Confr.DDE)               OR
108600170207     C                              %date(d03DDE:*eur) >
108700170207     C                              %date(VMS_Confr.DSC))
108800170207     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
108900170207     C                   leavesr
109000170207E   2C                   ENDIF
109100161122E   2C                   ENDSL
109200161102E   2C                   ENDIF
109300160913     C*
109400161122     C* CAUBRT diversa (in like)
109500161205IF  1C                   if        D03CAUBRT <> *blank
109600161205           wD03CauBrt = D03CauBRT;
109700161205           wD03CauBRT = %xlate('*':'%':wD03CauBRT);
109800161206           exec sql
109900161206           set :wDummy = (case
110000161206           when trim(:VMSCAUBRT) like trim(:wD03CAUBRT)
110100161206           then '1'
110200161206           else '0'
110300161206           end);
110400161206IF  1C                   if        wDummy = '0'
110500161205     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
110600161205     C                   leavesr
110700161205     C                   endif
110800161205     C                   endif
110900160926     C*
111000160926     C* STSCLI diversa (in like)
111100161205IF  1C                   if        D03STSCLI <> *blank
111200161205           wD03StsCli = D03StsCli;
111300161205           wD03StsCli = %xlate('*':'%':wD03StsCli);
111400161206           exec sql
111500161206           set :wDummy = (case
111600161206           when trim(:VMSSTSCLi) like trim(:wD03StsCli)
111700161206           then '1'
111800161206           else '0'
111900161206           end);
112000161206IF  1C                   if        wDummy = '0'
112100161206     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
112200161206     C                   leavesr
112300161206     C                   endif
112400161205     C                   endif
112500160713
112600970526     C                   ENDSR
112700970526     C*------------------------------------------------------------------------*
112800160830     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE            §
112900970526     C*------------------------------------------------------------------------*
113000970526     C     IRESFL        BEGSR
113100970526     C*
113200970526     C                   MOVEL     *BLANKS       OPZKEY
113300160901     C*
113400161122     C                   SELECT
113500161122     C* se NON è il drill-up né è la ricerca squadrature
113600161122     C                   WHEN      WTIPRI <> 'VAL' and
113700170207     C                             WTIPRI <> 'SQD' and
113800170207     C                             WTIPRI <> 'CFR'
113900160713     C                   EVAL      D05ATB    = VMSATB
114000160831     C                   SETOFF                                       01
114100160831     C                   IF        D05ATB = 'A'
114200160831     C                   SETON                                        01
114300160831     C                   ENDIF
114400160713     C                   EVAL      D05Proc   = VMSProc
114500160713     C                   EVAL      D05KSU    = VMSKSU
114600160830     C                   EVAL      D05CauTip = VMSCauTip
114700160830     C                   EVAL      D05CauBRT = VMSCauBRT
114800160715     C                   EVAL      D05CauTT  = VMSCauTT
114900160713     C                   EVAL      D05StsCli = VMSStsCli
115000160914     C                   EVAL      D05DDE    = %dec(%date(VMSDDE):*dmy)
115100160914     C                   EVAL      D05DSC    = %dec(%date(VMSDSC):*dmy)
115200160715     C                   EVAL      D05DescIT = VMSDescIT
115300170523     C                   EVAL      D05DesProc= VMSDesProc
115400170523     C                   EVAL      D05Descr  = VMSDescIT
115500970526     C*
115600161122     C* se è il drill-up
115700161122     C                   WHEN      WTIPRI =  'VAL'
115800160901     C                   CLEAR                   D05ATB
115900160901     C                   SETOFF                                       01
116000161122     C                   EVAL      D05Proc   = VMS_Drill.Proc
116100161122     C                   EVAL      D05KSU    = VMS_Drill.KSU
116200160901     C                   EVAL      D05CauTip = *blank
116300160923     C                   EVALR     D05CauBRT = 'Tot:'
116400161122     C                   EVALR     D05CauTT  = %char(VMS_Drill.Count)
116500160901     C                   EVAL      D05StsCli = *blank
116600161122     C                   EVAL      D05DDE    = %dec(%date(VMS_Drill.DDE):*dmy)
116700161122     C                   EVAL      D05DSC    = %dec(%date(VMS_Drill.DSC):*dmy)
116800170523     C                   EVAL      D05DescIT = *blank
116900170523     C                   EVAL      D05DesProc= VMS_Drill.DesProc
117000170523     C                   EVAL      D05Descr  = VMS_Drill.DesProc
117100161122
117200161122     C* se è la ricerca squadrature
117300161122     C                   WHEN      WTIPRI =  'SQD'
117400161122     C                   EVAL      D05ATB    = *blank
117500161122     C                   SETOFF                                       01
117600161122     C                   EVAL      D05Proc   = VMS_Squad.Proc
117700161122     C                   EVAL      D05KSU    = VMS_Squad.KSU
117800161122     C                   EVAL      D05CauTip = VMS_Squad.CauTip
117900161122     C                   EVAL      D05CauBRT = VMS_Squad.CauBRT
118000161122     C                   EVAL      D05CauTT  = VMS_Squad.CauTT
118100161122     C                   EVAL      D05StsCli = VMS_Squad.StsCli
118200161122     C                   EVAL      D05DDE    = %dec(%date(VMS_Squad.DDE):*dmy)
118300161122     C                   EVAL      D05DSC    = %dec(%date(VMS_Squad.DSC):*dmy)
118400161122     C                   EVAL      D05DescIT = VMS_Squad.DescIT
118500170523     C                   EVAL      D05DesProc= *blank
118600170523     C                   EVAL      D05Descr  = D05DescIT
118700170207
118800170207     C* se è il confronto
118900170207     C                   WHEN      WTIPRI =  'CFR'
119000170207     C                   EVAL      D05ATB    = *blank
119100170207     C                   SETOFF                                       01
119200170207     C                   EVAL      D05Proc   = VMS_Confr.Proc
119300170207     C                   EVAL      D05KSU    = VMS_Confr.KSU
119400170207     C                   EVAL      D05CauTip = VMS_Confr.CauTip
119500170207     C                   EVAL      D05CauBRT = VMS_Confr.CauBRT
119600170207     C                   EVAL      D05CauTT  = VMS_Confr.CauTT
119700170207     C                   EVAL      D05StsCli = VMS_Confr.StsCli
119800170207     C                   EVAL      D05DDE    = %dec(%date(VMS_Confr.DDE):*dmy)
119900170207     C                   EVAL      D05DSC    = %dec(%date(VMS_Confr.DSC):*dmy)
120000170523     C                   EVAL      D05DesProc= *blank
120100170207     C                   EVAL      D05DescIT = VMS_Confr.DescIT
120200170523     C                   EVAL      D05Descr  = D05DescIT
120300161122
120400161122     C                   ENDSL
120500160901     C*
120600970526     C                   ENDSR
120700970526     C*------------------------------------------------------------------------*
120800970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
120900970526     C*------------------------------------------------------------------------*
121000970526     C     CHKPAR        BEGSR
121100970526     C*
121200970526     C* REIMPOSTO FLAG EVIDENZAZIONE ERRORI SUI PARAMETRI
121300970526     C                   SETOFF                                       808182
121400160913     C                   SETOFF                                       838485
121500970526     C*--------------------
121600970526     C* ORDINAMENTO
121700970526     C*--------------------
121800970526     C*--------------------
121900970526     C* SELEZIONI
122000970526     C*--------------------
122100970526     C*
122200160831     C* SE NESSUN ORDINAMENTO RICHIESTO, utilizzo il caricamento completo per Procedura
122300160831     C                   MOVEL     'ALL'         WTIPRI            3
122400161123IF  5C                   EVAL      DDEISO = *loval
122500970526     C*
122600970526     C* ACCETTA SOLO UN PARAMETRO DI INIZIO RICERCA
122700970526IF  1C     ERRL00        IFEQ      '0'
122800160913     C* SE gestiremo l'Inizio elenco mettere qui l'elenco controllando che solo 1 sia valorizzato
122900160713     C                   if        1 <> 1
123000970526     C                   MOVEL     MSG(6)        DSMSMS
123100970526     C                   MOVEL     '1'           ERRL00
123200160830     C                   SETON                                        808182
123300970526E   1C                   ENDIF
123400160713E   1C                   ENDIF
123500160831     C*
123600160913     C* CONTROLLA che la data decorrenza  sia corretta formalmente
123700160831     C     ERRL00        IFEQ      '0'
123800160913     C                   IF        D03DDE   <> 0
123900160831     C                   MONITOR
124000160831     C* se il valore della data è > 6 cifre
124100160913     C                   IF        %len(%trim(%char(D03DDE  ))) > 6
124200160831     C* presumo sia scritta in ggmmaaaa
124300160913IF  5C                   EVAL      DDEISO = %date(D03DDE:*eur)
124400160831     C* se il valore della data è <= 6 cifre
124500160831     C                   ELSE
124600160831     C* presumo sia scritta in ggmmaa
124700160919IF  5C                   EVAL      DDEISO = %date(D03DDE:*dmy)
124800160831     C                   ENDIF
124900160831IF  2C                   ON-ERROR
125000160831     C                   MOVEL     '1'           ERRL00
125100160913     C                   SETON                                        83
125200160831     C                   MOVEL     MSG(8)        DSMSMS
125300160831     C                   LEAVESR
125400160831     C                   ENDMON
125500160831     C* rivalorizzo il video con la data in ggmmaaaa
125600160913     C                   EVAL      D03DDE = %dec(DDEISO:*eur)
125700160831     C                   ENDIF
125800160831     C                   ENDIF
125900160830     C*
126000160830IF  1C     ERRL00        IFEQ      '0'
126100160830     C                   SELECT
126200160916IF  2C     wDrillUp      WHENEQ    *on
126300170207IF  2C     wConfr        ANDNE     *on
126400160916     C                   MOVEL     'VAL'         WTIPRI
126500161122IF  2C     wSquad        WHENEQ    *on
126600161122     C                   MOVEL     'SQD'         WTIPRI
126700170207IF  2C     wConfr        WHENEQ    *on
126800170207     C                   MOVEL     'CFR'         WTIPRI
126900160830IF  2C     D03Proc       WHENNE    *blank
127000160830     C                   MOVEL     'PRO'         WTIPRI
127100160830IF  2C     D03KSU        WHENNE    *blank
127200160830     C                   MOVEL     'KSU'         WTIPRI
127300160831     C                   OTHER
127400160831     C                   MOVEL     'ALL'         WTIPRI
127500160830E   2C                   ENDSL
127600970526E   1C                   ENDIF
127700970526     C*
127800970526     C                   ENDSR
127900970526     C*------------------------------------------------------------------------*
128000970526     C* LANPGM - LANCIA UN PROGRAMMA
128100970526     C*------------------------------------------------------------------------*
128200970526     C     LANPGM        BEGSR
128300970526     C*
128400970526     C* IMPOSTA LA DS DI PROCEDURA
128500160713     C                   MOVEL     WOP0          VMS2OP0                         *LIVELLO PROCEDURA
128600160713     C                   MOVEL     WOP1          VMS2OP1                         *LIVELLO PGM
128700160713     C                   MOVEL     '0'           VMS2F03                         *PREMUTO F03=FINE
128800160713     C                   MOVEL     '0'           VMS2F12                         *PREMUTO F12=RIT
128900160713     C                   MOVEL     '0'           VMS2ERR                         *'1'=ERRORE
129000160713     C                   MOVEL     *BLANKS       VMS2MSG                         *MESSAGGIO DI RIT
129100160713     C                   EVAL      VMS2ATB    = DATB
129200160713     C                   EVAL      VMS2Proc   = DProc
129300160713     C                   EVAL      VMS2KSU    = DKSU
129400160831     C                   EVAL      VMS2CauTip = DCauTip
129500160831     C                   EVAL      VMS2CauBRT = DCauBRT
129600160713     C                   EVAL      VMS2StsCli = DStsCli
129700160913     C                   EVAL      VMS2DDE    = %char(DDDE)
129800970526     C*
129900970526     C                   CALL      WPGM
130000970612     C                   PARM                    KPJBA
130100160713     C                   PARM                    TIS7VMS2DS
130200970526     C*
130300970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
130400160713IF  1C     VMS2F03       IFEQ      '1'                                          *PREMUTO F03
130500970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
130600970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
130700970526X   1C                   ELSE                                                   *NON PREMUTO F03
130800160713     C                   MOVEL     VMS2MSG       DSMSMS                         *MESSAGGIO DI RIT
130900970526E   1C                   ENDIF
131000970526     C*
131100970526     C                   ENDSR
131200970527     C*------------------------------------------------------------------------*
131300970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
131400970527     C*------------------------------------------------------------------------*
131500970527     C     CARTAB        BEGSR
131600970527     C*
131700970527     C                   ENDSR
131800970526     C*------------------------------------------------------------------------*
131900970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
132000970526     C*------------------------------------------------------------------------*
132100970526     C     CHKFOP        BEGSR
132200970626     C*
132300970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
132400970626IF  1C     DEPOP0        IFEQ      'R01'
132500970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
132600970626     C                   MOVEL     'S'           OPDATT
132700970626     C                   MOVEL     OPDRIG        OPD(1)
132800970626X   1C                   ELSE
132900970626     C* OPZIONI
133000970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
133100970626     C                   MOVEL     'S'           OPDATT
133200970626     C                   MOVEL     OPDRIG        OPD(5)
133300970626     C* FUNZIONI
133400970626E   1C                   ENDIF
133500970626     C*
133600000602     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
133700970626IF  1C     DEPOP0        IFEQ      'R02'
133800160923     C                   MOVEL     OPD(1)        OPDRIG                         *MODIFICA
133900970626     C                   MOVEL     'S'           OPDATT
134000160923     C                   MOVEL     OPDRIG        OPD(1)
134100160923     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
134200160923     C                   MOVEL     'S'           OPDATT
134300160923     C                   MOVEL     OPDRIG        OPD(2)
134400970626     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
134500970626     C                   MOVEL     'S'           OPDATT
134600970626     C                   MOVEL     OPDRIG        OPD(4)
134700010702     C*                    MOVELOPD,6     OPDRIG           *ALLINEAMENTO
134800010702     C*                    MOVEL'S'       OPDATT
134900010702     C*                    MOVELOPDRIG    OPD,6
135000970626     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
135100970626     C                   MOVEL     'S'           OPDATT
135200970626     C                   MOVEL     OPDRIG        OPD(7)
135300970715     C*
135400970715     C* SOLO SE EDP ABILITA DUPLICA E INSERIMENTO
135500161121     C* SQUADRATURE, STAMPA
135600170207     C* CONFRONTO
135700970715IF  2C     UTENTE        IFEQ      'EDP'
135800970715     C                   MOVEL     OPD(3)        OPDRIG                         *DUPLICA
135900970715     C                   MOVEL     'S'           OPDATT
136000970715     C                   MOVEL     OPDRIG        OPD(3)
136100160915     C                   MOVEL     OPD(13)       OPDRIG                         *DUPLICA
136200160914     C                   MOVEL     'S'           OPDATT
136300160914     C                   MOVEL     OPDRIG        OPD(13)
136400160915     C                   MOVEL     OPD(23)       OPDRIG                         *DUPLICA
136500160915     C                   MOVEL     'S'           OPDATT
136600160915     C                   MOVEL     OPDRIG        OPD(23)
136700970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
136800970626     C                   MOVEL     'S'           FUDATT
136900970626     C                   MOVEL     FUDRIG        FUD(6)
137000160916     C                   MOVEL     FUD(8)        FUDRIG                         *INSERIMENTO
137100160916     C                   MOVEL     'S'           FUDATT
137200160916     C                   MOVEL     FUDRIG        FUD(8)
137300161121     C                   MOVEL     FUD(15)       FUDRIG                         *INSERIMENTO
137400161121     C                   MOVEL     'S'           FUDATT
137500161121     C                   MOVEL     FUDRIG        FUD(15)
137600170208     C                   MOVEL     FUD(16)       FUDRIG                         *INSERIMENTO
137700170208     C                   MOVEL     'S'           FUDATT
137800170208     C                   MOVEL     FUDRIG        FUD(16)
137900161121     C                   MOVEL     FUD(21)       FUDRIG                         *INSERIMENTO
138000161121     C                   MOVEL     'S'           FUDATT
138100161121     C                   MOVEL     FUDRIG        FUD(21)
138200970715E   2C                   ENDIF
138300970715E   1C                   ENDIF
138400970626     C*
138500000602     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO, ALLINEAMENTO
138600161121     C* SQUADRATURE, STAMPA
138700020919IF  1C     DUTLPO        IFNE      'S'                                          *FILIALE
138800970626     C                   MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
138900970626     C                   MOVEL     'N'           OPDATT
139000970626     C                   MOVEL     OPDRIG        OPD(4)
139100010702     C*                    MOVELOPD,6     OPDRIG           *NO ALLINEAMENTO
139200010702     C*                    MOVEL'N'       OPDATT
139300010702     C*                    MOVELOPDRIG    OPD,6
139400970626     C                   MOVEL     OPD(7)        OPDRIG                         *NO RIPRISTINO
139500970626     C                   MOVEL     'N'           OPDATT
139600970626     C                   MOVEL     OPDRIG        OPD(7)
139700160916     C                   MOVEL     OPD(13)       OPDRIG                         *NO RIPRISTINO
139800160916     C                   MOVEL     'N'           OPDATT
139900160916     C                   MOVEL     OPDRIG        OPD(13)
140000160916     C                   MOVEL     OPD(23)       OPDRIG                         *NO RIPRISTINO
140100160916     C                   MOVEL     'N'           OPDATT
140200160916     C                   MOVEL     OPDRIG        OPD(23)
140300970626     C                   MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
140400970626     C                   MOVEL     'N'           FUDATT
140500970626     C                   MOVEL     FUDRIG        FUD(6)
140600160916     C                   MOVEL     FUD(8)        FUDRIG                         *NO INSERIMENTO
140700160916     C                   MOVEL     'N'           FUDATT
140800160916     C                   MOVEL     FUDRIG        FUD(8)
140900161121     C                   MOVEL     FUD(15)       FUDRIG                         *INSERIMENTO
141000161121     C                   MOVEL     'N'           FUDATT
141100161121     C                   MOVEL     FUDRIG        FUD(15)
141200170208     C                   MOVEL     FUD(16)       FUDRIG                         *INSERIMENTO
141300170208     C                   MOVEL     'N'           FUDATT
141400170208     C                   MOVEL     FUDRIG        FUD(16)
141500161121     C                   MOVEL     FUD(21)       FUDRIG                         *INSERIMENTO
141600161121     C                   MOVEL     'N'           FUDATT
141700161121     C                   MOVEL     FUDRIG        FUD(21)
141800970626E   1C                   ENDIF
141900970526     C*
142000970526     C* GESTIONE OPZIONI A VIDEO
142100970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
142200970526     C                   EXSR      OPZGES
142300970526     C*
142400970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
142500970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
142600970526     C                   EXSR      FUNGES
142700970526     C*
142800970526     C                   ENDSR
142900020919     C*--------------------------------------------------------------------------------------------*
143000020919     C* REPERISCE I DATI UTENTE
143100020919     C*--------------------------------------------------------------------------------------------*
143200020919     C     REPDATIUTE    BEGSR
143300020919     C*
143400020919     C* INIZIALIZZA VARIABILI DI WRK
143500020919     C                   CLEAR                   TIBS34DS
143600020919     C                   CLEAR                   AZUTEDS
143700020919     C                   CLEAR                   DDATIUTE
143800020919     C*
143900020919     C     *DTAARA       DEFINE    §azute        azuteds
144000020919     C     *DTAARA       DEFINE    §datiute      ddatiute
144100020919     C                   IN(E)     *DTAARA
144200020919if  1C                   IF        %Error
144300020919     c                   EVAL      I34Tla = 'L'
144400020919     C                   CALL      'TIBS34R'
144500020919     C                   PARM                    Tibs34Ds
144600020919     C                   IN        *DTAARA
144700020919e   1C                   ENDIF
144800020919     C*
144900020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
145000020919     C                   MOVEL(P)  RSUT          DSFIRS           20
145100020919     C*
145200020919     C                   ENDSR
145300970526     C*------------------------------------------------------------------------*
145400970526     C* *INZSR - OPERAZIONI INIZIALI
145500970526     C*------------------------------------------------------------------------*
145600970526     C     *INZSR        BEGSR
145700970526     C*---
145800970526     C* RICEVIMENTO PARAMETRI
145900970526     C*---
146000970526     C     *ENTRY        PLIST
146100970612     C                   PARM                    KPJBA
146200160714     C*
146300160714     C                   EVAL      TIS7VMS2DS = kpjbu
146400160913     c                   if        kpjbu = *blank
146500160913     c                   clear                   TIS7VMS2DS
146600160913     c                   endif
146700970526     C*
146800970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
146900970526     C     XMSGPA        PLIST
147000970526     C                   PARM                    DSMSMS
147100970526     C                   PARM                    DSMSPG
147200970526     C                   PARM                    DSMSKE
147300970526     C                   PARM                    DSMSER
147400970526     C*
147500970526     C* IMPOSTA LA DS -TASTI PREMUTI-
147600970526     C                   CALL      'XKEY'
147700970526     C                   PARM                    DSKEY
147800970526     C*---
147900970526     C* VARIABILI NON RIFERITE AL DATA BASE
148000970526     C*---
148100970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
148200970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
148300970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
148400970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
148500970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
148600970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
148700970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
148800970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
148900970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
149000160713     C     *LIKE         DEFINE    VMS2OP0       DEPOP0                         *DI DEPOSITO
149100160713     C     *LIKE         DEFINE    VMS2OP1       DEPOP1
149200160713     C     *LIKE         DEFINE    VMS2OP0       WOP0                           *DI LAVORO
149300160713     C     *LIKE         DEFINE    VMS2OP1       WOP1
149400970526     C*---
149500970526     C* VARIABILI DI CONTROLLO
149600970526     C*---
149700970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
149800970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
149900970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
150000970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
150100970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
150200160714     C                   Z-ADD     14            SFLPAG                         *N°RIGHE X PAG
150300970526     C*
150400970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
150500160712     C                   MOVEL     'TIS7VMS1R'   DSMSPG
150600160712     C                   MOVEL     'TIS7VMS1R'   MSGQUE
150700970526     C                   MOVEL     *BLANKS       MSGKEY
150800970526     C                   MOVEL     *BLANKS       DSMSMS
150900970526     C                   MOVEL     *BLANKS       DSMSKE
151000970526     C                   Z-ADD     *ZEROS        DSMSER
151100970526     C*---
151200970526     C* VARIABILI RIFERITE AL DATA BASE
151300970526     C*---
151400970526     C*---
151500970526     C* CHIAVI DI LETTURA
151600970526     C*---
151700160713     C* LETTURA TIVMS01I - COMPLETA
151800160914     C     K05VMS01      KLIST
151900160713     C                   KFLD                    VMSProc
152000160713     C                   KFLD                    VMSKSU
152100160830     C                   KFLD                    VMSDDE
152200160830     C                   KFLD                    VMSCauTip
152300160912     C                   KFLD                    VMSCauBRT
152400160713     C* LETTURA TIVMS01I - parziale
152500160830     C     K01VMS01      KLIST
152600160713     C                   KFLD                    VMSProc
152700160913     C* LETTURA TIVMS01I - parziale
152800160913     C     K02VMS01      KLIST
152900160913     C                   KFLD                    VMSProc
153000160913     C                   KFLD                    VMSKSU
153100160913     C* LETTURA TIVMS01I - parziale
153200160913     C     K03VMS01      KLIST
153300160913     C                   KFLD                    VMSProc
153400160913     C                   KFLD                    VMSKSU
153500160913     C                   KFLD                    VMSDDE
153600160830     C* LETTURA TIVMS04I - parziale
153700160830     C     K01VMS04      KLIST
153800160830     C                   KFLD                    VMSKSU
153900970527     C*
154000970527     C* REPERIMENTO DATI UTENTE
154100020919     C                   EXSR      REPDATIUTE
154200970527     C*
154300970527     C* CARICA LE TABELLE OCCORRENTI
154400970527     C                   EXSR      CARTAB
154500970526     C*
154600970526     C* DEPOSITA LE OPZIONI DI ENTRATA
154700160713     C                   MOVEL     VMS2OP0       DEPOP0                         *LIVELLO PROCEDURA
154800160713     C                   MOVEL     VMS2OP1       DEPOP1                         *LIVELLO PROGRAMMA
154900970526     C*
155000970526     C* CALCOLA LA DATA CORRENTE
155100970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
155200970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
155300970526     C                   Z-ADD     WN8           G08DAT
155400970526     C                   Z-ADD     *ZEROS        G08INV
155500970526     C                   MOVEL     '0'           G08ERR
155600970526     C                   CALL      'XSRDA8'
155700970526     C                   PARM                    WLBDA8
155800970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
155900970526     C*
156000970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
156100970526     C                   EXSR      CHKFOP
156200970526     C*
156300970526     C* IMPOSTA I PARAMETRI DI INGRESSO
156400160912     C*  intanto imposto le selezioni con i dati passati
156500160912     C                   EVAL      D03Proc = VMS2Proc
156600160913     C                   EVAL      D03KSU  = VMS2KSU
156700170302     C                   EVAL      D03DDE  =
156800170302     c                              %dec(%date():*eur)
156900160913     C                   if        VMS2DDE <> *blank and
157000160913     C                             VMS2DDE <> *zeros
157100160913     C                   EVAL      D03DDE  =
157200160913     c                              %dec(%date(%dec(VMS2DDE:8:0)):*eur)
157300160913     c                   endif
157400170302     C                   EVAL      DEPProc = D03Proc
157500170302     C                   EVAL      DEPKSU  = D03KSU
157600170302     C                   EVAL      DEPDDE  = D03DDE
157700160714     C                   EXSR      CHKPAR
157800160714IF  1C     ERRL00        IFEQ      '1'
157900160714     C                   MOVEL     '1'           VMS2ERR
158000160714     C                   MOVEL     DSMSMS        VMS2MSG
158100160714X   1C                   ELSE
158200160714E   1C                   ENDIF
158300980225     C*
158400970526     C*
158500970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
158600970530IF  1C     DEPOP0        IFEQ      'R01'
158700970530     C                   MOVEL     DEC(3)        VIDING
158800970530E   1C                   ENDIF
158900970530IF  1C     DEPOP0        IFEQ      'R02'
159000970530     C                   MOVEL     DEC(1)        VIDING
159100970530E   1C                   ENDIF
159200970530IF  1C     DEPOP0        IFEQ      'R05'
159300970530     C                   MOVEL     DEC(2)        VIDING
159400970530E   1C                   ENDIF
159500970526     C*
159600970526     C                   ENDSR
159700970526     C*------------------------------------------------------------------------*
159800970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
159900970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
160000970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
160100970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
160200970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
160300970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
160400970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
160500970526     C*------------------------------------------------------------------------*
160600970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
160700970526     C*------------------------------------------------------------------------*
160800970526     C     POSSFL        BEGSR
160900970526     C*
161000970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
161100970526     C                   EXSR      CLRSFL
161200970526     C*
161300970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
161400970526     C                   EXSR      SETFIL
161500970526     C*
161600970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
161700970526     C     SFLEOF        IFEQ      0
161800970526     C                   EXSR      CARSFL
161900970526     C                   END
162000970526     C*
162100970526     C                   ENDSR
162200970526     C*------------------------------------------------------------------------*
162300970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
162400970526     C*------------------------------------------------------------------------*
162500970526     C     ENTATT        BEGSR
162600970526     C*
162700970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
162800970526     C                   Z-ADD     0             OPZOK             1 0
162900970526     C*
163000970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
163100970526     C                   Z-ADD     0             SFLVAR            5 0
163200970526     C*
163300970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
163400970526     C                   Z-ADD     0             SFLERR
163500970526     C*
163600970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
163700970526     C                   Z-ADD     SFLULT        WFLULT
163800970526     C*
163900970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
164000160712     C                   READC     S7V1D05                                98
164100970526     C     *IN98         DOWEQ     '0'
164200970526     C*
164300970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
164400970526     C                   ADD       1             SFLVAR
164500970526     C*
164600970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
164700970526     C     OPZOK         IFEQ      0
164800970526     C*
164900970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
165000970526     C     OPZKEY        IFNE      *BLANKS
165100970526     C                   EXSR      OPZCON
165200970526     C                   END
165300970526     C*
165400970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
165500970526     C*            2. PULIZIA SCELTA IN SFL
165600970526     C     OPZOK         IFEQ      0
165700970526     C                   MOVEL     '0'           *IN41
165800970526     C                   MOVEL     *BLANKS       OPZKEY
165900970526     C                   ELSE
166000970526     C*
166100970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
166200970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
166300970526     C                   MOVEL     '1'           *IN41
166400970526     C                   Z-ADD     SFLULT        SFLERR
166500970526     C                   END
166600970526     C                   ELSE
166700970526     C*
166800970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
166900970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
167000970526     C                   MOVEL     '1'           *IN41
167100970526     C                   END                                                    OPZOK=0
167200970526     C*
167300970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
167400970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
167500970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
167600970526     C     OPZOK         IFNE      0
167700970526     C                   Z-ADD     SFLERR        SFLNBR
167800970526     C                   ELSE
167900970526     C                   Z-ADD     SFLULT        SFLNBR
168000970526     C                   END
168100970526     C*
168200970526     C* AGGIORNO RECORD SFL
168300160902     C*  imposto "effetti speciali"
168400160902     C                   EXSR      SFXD05
168500160712     C                   UPDATE    S7V1D05
168600970526     C*
168700160712     C                   READC     S7V1D05                                98
168800970526     C                   END                                                    DO WHILE
168900970526     C*
169000970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
169100970526     C                   Z-ADD     WFLULT        SFLULT
169200970526     C*
169300970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
169400970526     C     SFLVAR        IFEQ      0
169500970526     C*---                 MOVEL'1'       FINL00
169600970526     C                   END
169700970526     C*
169800970526     C                   ENDSR
169900970526     C*------------------------------------------------------------------------*
170000970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
170100970526     C*------------------------------------------------------------------------*
170200970526     C     CLRSFL        BEGSR
170300970526     C*
170400970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
170500970526     C* (0=NON FINE FILE  1=FINE FILE)
170600970526     C                   Z-ADD     0             SFLEOF
170700970526     C*
170800970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
170900970526     C                   Z-ADD     0             SFLULT
171000970526     C*
171100970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
171200970526     C                   MOVEL     '1'           *IN40
171300160712     C                   WRITE     S7V1D06
171400970526     C                   MOVEL     '0'           *IN40
171500970526     C*
171600970526     C                   ENDSR
171700970526     C*------------------------------------------------------------------------*
171800970526     C* CARSFL - CARICAMENTO SFL
171900970526     C*------------------------------------------------------------------------*
172000970526     C     CARSFL        BEGSR
172100970526     C*
172200970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
172300970526     C     SFLULT        IFEQ      0
172400970526     C                   EXSR      SETFIL
172500970526     C                   END
172600970526     C*
172700970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
172800970526     C                   Z-ADD     SFLULT        WFLULT
172900970526     C*
173000970526     C* CICLO:
173100970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
173200970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
173300970526     C                   Z-ADD     1             SFLRIG
173400970526     C     SFLRIG        DOWLE     SFLPAG
173500970526     C     SFLEOF        ANDEQ     0
173600970526     C*
173700970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
173800970526     C                   MOVEL     *BLANKS       OPZKEY
173900970526     C*
174000970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
174100970526     C                   EXSR      IRESFL
174200970526     C*
174300970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
174400970526     C                   ADD       1             SFLULT
174500970526     C*
174600970526     C* SCRITTURA RECORD SFL
174700160902     C*  imposto "effetti speciali"
174800160902     C                   EXSR      SFXD05
174900160712     C                   WRITE     S7V1D05
175000970526     C                   ADD       1             SFLRIG
175100970526     C*
175200970526     C* LETTURA FILE DA CARICARE
175300970526     C                   EXSR      LETFIL
175400970526     C                   END                                                    DO WHILE
175500970526     C*
175600970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
175700970526     C     WFLULT        IFNE      SFLULT
175800970526     C                   Z-ADD     WFLULT        SFLNBR
175900970526     C                   ADD       1             SFLNBR
176000970526     C                   END
176100970526     C*
176200970526     C                   ENDSR
176300970526     C*------------------------------------------------------------------------*
176400160902     C* SFXD05 - effetti speciali sfl D05 (RI, ND, BL, ...)
176500160902     C*          a seconda dei dati presenti in riga
176600970526     C*------------------------------------------------------------------------*
176700160902     C     SFXD05        BEGSR
176800970526     C*
176900160902     C* RI campi chiave per rcd annullato
177000160902     C                   SETOFF                                       01
177100160902     C                   IF        D05ATB = 'A'
177200160902     C                   SETON                                        01
177300160902     C                   ENDIF
177400160902     C*
177500160902     C                   ENDSR
177600160902     C*------------------------------------------------------------------------*
177700160902     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
177800160902     C*------------------------------------------------------------------------*
177900160902     C     OUTSFL        BEGSR
178000160902     C*
178100970526     C* WRITE RECORD TESTATA VIDEO
178200160712     C                   WRITE     S7V1D01
178300970526     C*
178400970526     C* VISUALIZZAZIONE MESSAGGIO
178500970526     C                   EXSR      SNDMSG
178600970526     C*
178700970526     C* WRITE RECORD OPZIONI VIDEO
178800160712     C                   WRITE     S7V1D02
178900970526     C*
179000970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
179100160712     C                   WRITE     S7V1D03
179200970526     C*
179300970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
179400160712     C                   WRITE     S7V1D04
179500970526     C*
179600970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
179700970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
179800970526     C     SFLEOF        IFEQ      0
179900970526     C                   MOVEL     '0'           *IN42
180000970526     C                   ELSE
180100970526     C*
180200970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
180300970526     C                   MOVEL     '1'           *IN42
180400970526     C                   END
180500970526     C*
180600970526     C* WRITE RECORD TASTI FUNZIONALI
180700160712     C                   WRITE     S7V1D09
180800970526     C*
180900970526     C* CASO DI SFL VUOTO
181000970526     C     SFLULT        IFLE      0
181100970526     C*
181200970526     C*    - WRITE RECORD VUOTO
181300160712     C                   WRITE     S7V1D0A
181400160712     C                   READ      S7V1D0A                                99
181500970526     C*
181600970526     C                   ELSE
181700970526     C*
181800970526     C* CASO DI SFL CON RECORDS
181900970526     C*
182000970526     C*    - WRITE RECORD CONTROLLO SFL
182100160712     C                   WRITE     S7V1D06
182200160712     C                   READ      S7V1D06                                99
182300970526     C                   END
182400970526     C*
182500970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
182600160712     C                   READ      S7V1D03                                99
182700970526     C*
182800970526     C                   ENDSR
182900970526     C*------------------------------------------------------------------------*
183000970526     C* SNDMSG - MANDA MESSAGGIO
183100970526     C*------------------------------------------------------------------------*
183200970526     C     SNDMSG        BEGSR
183300970526     C*
183400970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
183500970526     C                   MOVEL     '1'           *IN40
183600160712     C                   WRITE     S7V1DMC
183700970526     C                   MOVEL     '0'           *IN40
183800970526     C*
183900970526     C* VISUALIZZAZIONE MESSAGGIO
184000970526     C                   Z-ADD     0             MSGULT
184100970526     C                   CALL      'XMSG'        XMSGPA
184200970526     C     DSMSER        IFEQ      0
184300970526     C                   MOVEL     DSMSKE        MSGKEY
184400970526     C                   END
184500970526     C                   Z-ADD     1             MSGULT
184600160712     C                   WRITE     S7V1DMS                                      SFL MESS
184700160712     C                   WRITE     S7V1DMC                                      CONTR MESS
184800970526     C*
184900970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
185000970526     C                   MOVEL     *BLANKS       DSMSMS
185100970526     C*
185200970526     C                   ENDSR
185300970526     C*------------------------------------------------------------------------*
185400970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
185500970526     C*------------------------------------------------------------------------*
185600970526     C     FUNCON        BEGSR
185700970526     C*
185800970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
185900970526     C                   EXSR      FUNVAR
186000970526     C*
186100970526     C* PULISCO CAMPI DI LAVORO
186200970526     C                   Z-ADD     0             I                 5 0
186300970526     C                   Z-ADD     0             W                 5 0
186400970526     C*
186500970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
186600970526     C     FUNKEY        IFNE      *BLANKS
186700970526     C     1             DO        24            I
186800970526     C                   MOVEL     FUD(I)        FUDRIG
186900970526     C*
187000970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
187100970526     C     FUNKEY        IFEQ      FUDFUN
187200970526     C     FUDATT        ANDEQ     'S'
187300970526     C                   Z-ADD     I             W
187400970526     C                   END
187500970526     C                   END                                                    DO WHILE
187600970526     C*
187700970526     C* FUNZIONE NON ATTIVA (ERRORE)
187800970526     C     W             IFEQ      0
187900970526     C*
188000970526     C* EMISSIONE MESSAGGIO DI ERRORE
188100970526     C                   MOVEL     MSG(2)        DSMSMS
188200970526     C                   END                                                    W=0
188300970526     C                   END                                                    FUNKEY NE *BLANKS
188400970526     C*
188500970526     C* FUNZIONE ATTIVA (PROCEDI ...)
188600970526     C     W             IFNE      0
188700970526     C     FUNKEY        OREQ      *BLANKS
188800970526     C*
188900970526     C     FUNKEY        IFEQ      *BLANKS
189000970526     C                   EXSR      ENTGES
189100970526     C                   END
189200970526     C     FUNKEY        IFEQ      '03'
189300970526     C                   EXSR      F03GES
189400970526     C                   END
189500970526     C     FUNKEY        IFEQ      '05'
189600970526     C                   EXSR      F05GES
189700970526     C                   END
189800970526     C     FUNKEY        IFEQ      '06'
189900970526     C                   EXSR      F06GES
190000970526     C                   END
190100160916     C     FUNKEY        IFEQ      '08'
190200160916     C                   EXSR      F08GES
190300160916     C                   END
190400160913     C     FUNKEY        IFEQ      '12'
190500160913     C                   EXSR      F12GES
190600160913     C                   END
190700161121     C     FUNKEY        IFEQ      '15'
190800161121     C                   EXSR      F15GES
190900161121     C                   END
191000170208     C     FUNKEY        IFEQ      '16'
191100170208     C                   EXSR      F16GES
191200170208     C                   END
191300161121     C     FUNKEY        IFEQ      '21'
191400161121     C                   EXSR      F21GES
191500161121     C                   END
191600970526     C     FUNKEY        IFEQ      '22'
191700970526     C                   EXSR      F22GES
191800970526     C                   END
191900970526     C     FUNKEY        IFEQ      '23'
192000970526     C                   EXSR      F23GES
192100970526     C                   END
192200970526     C     FUNKEY        IFEQ      '24'
192300970526     C                   EXSR      F24GES
192400970526     C                   END
192500970526     C     FUNKEY        IFEQ      'RU'
192600970526     C                   EXSR      RUPGES
192700970526     C                   END
192800970526     C*
192900970526     C                   END
193000970526     C*
193100970526     C                   ENDSR
193200970526     C*------------------------------------------------------------------------*
193300970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
193400970526     C*------------------------------------------------------------------------*
193500970526     C     FUNVAR        BEGSR
193600970526     C*
193700970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
193800970526     C     KEY           IFEQ      ENTER
193900970526     C                   MOVEL     *BLANKS       FUNKEY
194000970526     C                   END
194100970526     C     KEY           IFEQ      F1
194200970526     C                   MOVEL     '01'          FUNKEY
194300970526     C                   END
194400970526     C     KEY           IFEQ      F2
194500970526     C                   MOVEL     '02'          FUNKEY
194600970526     C                   END
194700970526     C     KEY           IFEQ      F3
194800970526     C                   MOVEL     '03'          FUNKEY
194900970526     C                   END
195000970526     C     KEY           IFEQ      F4
195100970526     C                   MOVEL     '04'          FUNKEY
195200970526     C                   END
195300970526     C     KEY           IFEQ      F5
195400970526     C                   MOVEL     '05'          FUNKEY
195500970526     C                   END
195600970526     C     KEY           IFEQ      F6
195700970526     C                   MOVEL     '06'          FUNKEY
195800970526     C                   END
195900970526     C     KEY           IFEQ      F7
196000970526     C                   MOVEL     '07'          FUNKEY
196100970526     C                   END
196200970526     C     KEY           IFEQ      F8
196300970526     C                   MOVEL     '08'          FUNKEY
196400970526     C                   END
196500970526     C     KEY           IFEQ      F9
196600970526     C                   MOVEL     '09'          FUNKEY
196700970526     C                   END
196800970526     C     KEY           IFEQ      F10
196900970526     C                   MOVEL     '10'          FUNKEY
197000970526     C                   END
197100970526     C     KEY           IFEQ      F11
197200970526     C                   MOVEL     '11'          FUNKEY
197300970526     C                   END
197400970526     C     KEY           IFEQ      F12
197500970526     C                   MOVEL     '12'          FUNKEY
197600970526     C                   END
197700970526     C     KEY           IFEQ      F13
197800970526     C                   MOVEL     '13'          FUNKEY
197900970526     C                   END
198000970526     C     KEY           IFEQ      F14
198100970526     C                   MOVEL     '14'          FUNKEY
198200970526     C                   END
198300970526     C     KEY           IFEQ      F15
198400970526     C                   MOVEL     '15'          FUNKEY
198500970526     C                   END
198600970526     C     KEY           IFEQ      F16
198700970526     C                   MOVEL     '16'          FUNKEY
198800970526     C                   END
198900970526     C     KEY           IFEQ      F17
199000970526     C                   MOVEL     '17'          FUNKEY
199100970526     C                   END
199200970526     C     KEY           IFEQ      F18
199300970526     C                   MOVEL     '18'          FUNKEY
199400970526     C                   END
199500970526     C     KEY           IFEQ      F19
199600970526     C                   MOVEL     '19'          FUNKEY
199700970526     C                   END
199800970526     C     KEY           IFEQ      F20
199900970526     C                   MOVEL     '20'          FUNKEY
200000970526     C                   END
200100970526     C     KEY           IFEQ      F21
200200970526     C                   MOVEL     '21'          FUNKEY
200300970526     C                   END
200400970526     C     KEY           IFEQ      F22
200500970526     C                   MOVEL     '22'          FUNKEY
200600970526     C                   END
200700970526     C     KEY           IFEQ      F23
200800970526     C                   MOVEL     '23'          FUNKEY
200900970526     C                   END
201000970526     C     KEY           IFEQ      F24
201100970526     C                   MOVEL     '24'          FUNKEY
201200970526     C                   END
201300970526     C     KEY           IFEQ      ROLLUP
201400970526     C                   MOVEL     'RU'          FUNKEY
201500970526     C                   END
201600970526     C     KEY           IFEQ      HELP
201700970526     C                   MOVEL     'HP'          FUNKEY
201800970526     C                   END
201900970526     C*
202000970526     C                   ENDSR
202100970526     C*------------------------------------------------------------------------*
202200970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
202300970526     C*------------------------------------------------------------------------*
202400970526     C     FUNGES        BEGSR
202500970526     C*
202600970526     C* PULISCO CAMPI DI LAVORO
202700970526     C                   Z-ADD     0             W                 5 0
202800970526     C*
202900970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
203000970526     C* E CARICO TABELLA FUNZIONI ATTIVE
203100970526     C     1             DO        24            I
203200970526     C                   MOVEL     FUD(I)        FUDRIG
203300970526     C*
203400970526     C* PULIZIA FUNZIONI PRECEDENTI
203500970526     C                   MOVEL     *BLANKS       FUV(I)
203600970526     C*
203700970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
203800970526     C* (SALTA F23 E F24)
203900970526     C     FUDATT        IFEQ      'S'
204000970526     C     FUDFUN        ANDNE     '23'
204100970526     C     FUDFUN        ANDNE     '24'
204200970526     C                   ADD       1             W
204300970526     C                   MOVEL     FUDDES        FUV(W)
204400970526     C                   END
204500970526     C                   END                                                    DO WHILE
204600970526     C*
204700970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
204800970526     C* ALTRIMENTI DISATTIVO F24
204900970526     C     1             DO        24            I
205000970526     C                   MOVEL     FUD(I)        FUDRIG
205100970526     C*
205200970526     C     FUDFUN        IFEQ      '24'
205300970526     C     W             IFGT      6
205400970526     C                   MOVEL     'S'           FUDATT
205500970526     C                   ELSE
205600970526     C                   MOVEL     *BLANKS       FUDATT
205700970526     C                   END                                                    W GT 8
205800970526     C                   MOVEL     FUDRIG        FUD(I)
205900970526     C                   END
206000970526     C                   END                                                    DO WHILE
206100970526     C*
206200970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
206300970526     C     W             DIV       6             I
206400970526     C                   MVR                     Z
206500970526     C*
206600970526     C     Z             IFNE      0
206700970526     C     I             ADD       1             Z
206800970526     C                   ELSE
206900970526     C                   Z-ADD     I             Z
207000970526     C                   END
207100970526     C*
207200970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
207300970526     C                   ADD       1             FUNGRU
207400970526     C     FUNGRU        IFGT      Z
207500970526     C     FUNGRU        ORLT      1
207600970526     C                   Z-ADD     1             FUNGRU
207700970526     C                   END
207800970526     C*
207900970526     C* PRIMO GRUPPO DI 6 FUNZIONI
208000970526     C     FUNGRU        IFEQ      1
208100970526     C                   MOVEL     FUV(1)        FUN001
208200970526     C                   MOVEL     FUV(2)        FUN002
208300970526     C                   MOVEL     FUV(3)        FUN003
208400970526     C                   MOVEL     FUV(4)        FUN004
208500970526     C                   MOVEL     FUV(5)        FUN005
208600970526     C                   MOVEL     FUV(6)        FUN006
208700970526     C                   END
208800970526     C*
208900970526     C* SECONDO GRUPPO DI 6 FUNZIONI
209000970526     C     FUNGRU        IFEQ      2
209100970526     C                   MOVEL     FUV(7)        FUN001
209200970526     C                   MOVEL     FUV(8)        FUN002
209300970526     C                   MOVEL     FUV(9)        FUN003
209400970526     C                   MOVEL     FUV(10)       FUN004
209500970526     C                   MOVEL     FUV(11)       FUN005
209600970526     C                   MOVEL     FUV(12)       FUN006
209700970526     C                   END
209800970526     C*
209900970526     C* TERZO GRUPPO DI 6 FUNZIONI
210000970526     C     FUNGRU        IFEQ      3
210100970526     C                   MOVEL     FUV(13)       FUN001
210200970526     C                   MOVEL     FUV(14)       FUN002
210300970526     C                   MOVEL     FUV(15)       FUN003
210400970526     C                   MOVEL     FUV(16)       FUN004
210500970526     C                   MOVEL     FUV(17)       FUN005
210600970526     C                   MOVEL     FUV(18)       FUN006
210700970526     C                   END
210800970526     C*
210900970526     C* QUARTO GRUPPO DI 6 FUNZIONI
211000970526     C     FUNGRU        IFEQ      4
211100970526     C                   MOVEL     FUV(19)       FUN001
211200970526     C                   MOVEL     FUV(20)       FUN002
211300970526     C                   MOVEL     FUV(21)       FUN003
211400970526     C                   MOVEL     FUV(22)       FUN004
211500970526     C                   MOVEL     *BLANKS       FUN005
211600970526     C                   MOVEL     *BLANKS       FUN006
211700970526     C                   END
211800970526     C*
211900970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
212000970526     C                   MOVEL     *BLANKS       FUN007
212100970526     C                   MOVEL     *BLANKS       FUN008
212200970526     C*
212300970526     C     1             DO        24            I
212400970526     C                   MOVEL     FUD(I)        FUDRIG
212500970526     C     FUDFUN        IFEQ      '23'
212600970526     C     FUDATT        ANDEQ     'S'
212700970526     C                   MOVEL     FUDDES        FUN007
212800970526     C                   END
212900970526     C     FUDFUN        IFEQ      '24'
213000970526     C     FUDATT        ANDEQ     'S'
213100970526     C                   MOVEL     FUDDES        FUN008
213200970526     C                   END
213300970526     C                   END                                                    DO
213400970526     C*
213500970526     C                   ENDSR
213600970526     C*------------------------------------------------------------------------*
213700970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
213800970526     C*------------------------------------------------------------------------*
213900970526     C     OPZGES        BEGSR
214000970526     C*
214100970526     C* PULISCO CAMPI DI LAVORO
214200970526     C                   Z-ADD     0             W                 5 0
214300970526     C*
214400970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
214500970526     C* E CARICO TABELLA OPZIONI ATTIVE
214600970526     C     1             DO        24            I
214700970526     C                   MOVEL     OPD(I)        OPDRIG
214800970526     C*
214900970526     C* PULIZIA OPZIONI PRECEDENTI
215000970526     C                   MOVEL     *BLANKS       OPV(I)
215100970526     C*
215200970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
215300970526     C     OPDATT        IFEQ      'S'
215400970526     C                   ADD       1             W
215500970526     C                   MOVEL     OPDDES        OPV(W)
215600970526     C                   END
215700970526     C                   END                                                    DO WHILE
215800970526     C*
215900970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
216000970526     C* ALTRIMENTI DISATTIVO F23
216100970526     C     1             DO        24            I
216200970526     C                   MOVEL     FUD(I)        FUDRIG
216300970526     C*
216400970526     C     FUDFUN        IFEQ      '23'
216500970526     C     W             IFGT      8
216600970526     C                   MOVEL     'S'           FUDATT
216700970526     C                   ELSE
216800970526     C                   MOVEL     *BLANKS       FUDATT
216900970526     C                   END                                                    W GT 8
217000970526     C                   MOVEL     FUDRIG        FUD(I)
217100970526     C                   END
217200970526     C                   END                                                    DO WHILE
217300970526     C*
217400970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
217500970526     C     W             DIV       8             I
217600970526     C                   MVR                     Z                 5 0
217700970526     C*
217800970526     C     Z             IFNE      0
217900970526     C     I             ADD       1             Z
218000970526     C                   ELSE
218100970526     C                   Z-ADD     I             Z
218200970526     C                   END
218300970526     C*
218400970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
218500970526     C                   ADD       1             OPZGRU
218600970526     C     OPZGRU        IFGT      Z
218700970526     C     OPZGRU        ORLT      1
218800970526     C                   Z-ADD     1             OPZGRU
218900970526     C                   END
219000970526     C*
219100970526     C* PRIMO GRUPPO DI 8 OPZIONI
219200970526     C     OPZGRU        IFEQ      1
219300970526     C                   MOVEL     OPV(1)        OPZ001
219400970526     C                   MOVEL     OPV(2)        OPZ002
219500970526     C                   MOVEL     OPV(3)        OPZ003
219600970526     C                   MOVEL     OPV(4)        OPZ004
219700970526     C                   MOVEL     OPV(5)        OPZ005
219800970526     C                   MOVEL     OPV(6)        OPZ006
219900970526     C                   MOVEL     OPV(7)        OPZ007
220000970526     C                   MOVEL     OPV(8)        OPZ008
220100970526     C                   END
220200970526     C*
220300970526     C* SECONDO GRUPPO DI 8 OPZIONI
220400970526     C     OPZGRU        IFEQ      2
220500970526     C                   MOVEL     OPV(9)        OPZ001
220600970526     C                   MOVEL     OPV(10)       OPZ002
220700970526     C                   MOVEL     OPV(11)       OPZ003
220800970526     C                   MOVEL     OPV(12)       OPZ004
220900970526     C                   MOVEL     OPV(13)       OPZ005
221000970526     C                   MOVEL     OPV(14)       OPZ006
221100970526     C                   MOVEL     OPV(15)       OPZ007
221200970526     C                   MOVEL     OPV(16)       OPZ008
221300970526     C                   END
221400970526     C*
221500970526     C* TERZO GRUPPO DI 8 OPZIONI
221600970526     C     OPZGRU        IFEQ      3
221700970526     C                   MOVEL     OPV(17)       OPZ001
221800970526     C                   MOVEL     OPV(18)       OPZ002
221900970526     C                   MOVEL     OPV(19)       OPZ003
222000970526     C                   MOVEL     OPV(20)       OPZ004
222100970526     C                   MOVEL     OPV(21)       OPZ005
222200970526     C                   MOVEL     OPV(22)       OPZ006
222300970526     C                   MOVEL     OPV(23)       OPZ007
222400970526     C                   MOVEL     OPV(24)       OPZ008
222500970526     C                   END
222600970526     C*
222700970526     C                   ENDSR
222800970526     C*------------------------------------------------------------------------*
222900970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
223000970526     C*------------------------------------------------------------------------*
223100970526     C     OPZCON        BEGSR
223200970526     C*
223300970526     C* PULISCO CAMPI DI LAVORO
223400970526     C                   Z-ADD     0             I                 5 0
223500970526     C                   Z-ADD     0             W                 5 0
223600970526     C*
223700970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
223800970526     C     1             DO        24            I
223900970526     C                   MOVEL     OPD(I)        OPDRIG
224000970526     C*
224100970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
224200970526     C     OPZKEY        IFEQ      OPDOPZ
224300970526     C     OPZKEY        OREQ      OPDOP1
224400970526     C*
224500970526     C     OPDATT        IFEQ      'S'
224600970526     C                   MOVEL     OPDOPZ        OPZKEY
224700970526     C                   Z-ADD     I             W
224800970526     C                   END
224900970526     C                   END
225000970526     C*
225100970526     C                   END                                                    DO WHILE
225200970526     C*
225300970526     C* OPZIONE NON ATTIVA (ERRORE)
225400970526     C     W             IFEQ      0
225500970526     C*
225600970526     C* OPZIONE NON OK
225700970526     C                   Z-ADD     1             OPZOK
225800970526     C*
225900970526     C* EMISSIONE MESSAGGIO DI ERRORE
226000970526     C                   MOVEL     MSG(1)        DSMSMS
226100970526     C                   ELSE
226200970526     C*
226300970526     C* OPZIONE ATTIVA (PROCEDI ...)
226400970526     C*
226500970526     C     OPZKEY        IFEQ      ' 1'
226600970526     C                   EXSR      O01GES
226700970526     C                   END
226800970526     C     OPZKEY        IFEQ      ' 2'
226900970526     C                   EXSR      O02GES
227000970526     C                   END
227100970603     C     OPZKEY        IFEQ      ' 3'
227200970603     C                   EXSR      O03GES
227300970603     C                   END
227400970526     C     OPZKEY        IFEQ      ' 4'
227500970526     C                   EXSR      O04GES
227600970526     C                   END
227700970526     C     OPZKEY        IFEQ      ' 5'
227800970526     C                   EXSR      O05GES
227900970526     C                   END
228000970526     C     OPZKEY        IFEQ      ' 7'
228100970526     C                   END
228200970528     C     OPZKEY        IFEQ      ' 8'
228300970528     C                   END
228400970528     C     OPZKEY        IFEQ      ' 9'
228500970528     C                   END
228600970528     C     OPZKEY        IFEQ      '10'
228700970528     C                   END
228800970528     C     OPZKEY        IFEQ      '11'
228900970528     C                   END
229000970603     C     OPZKEY        IFEQ      '13'
229100160914     C                   EXSR      O13GES
229200970603     C                   END
229300160916     C     OPZKEY        IFEQ      '23'
229400160916     C                   EXSR      O23GES
229500160916     C                   END
229600970526     C                   END                                                    OPZ.NON ATTIVA
229700970526     C*
229800970526     C                   ENDSR
229900160831
230000160831       /free
230100160831
230200160831       //--------------------------------------------------------------
230300161122       //?Preparazione stringa SQL per Drill up                        ?
230400160831       //--------------------------------------------------------------
230500161122       BEGSR  PrepSQLDrill;
230600160831
230700161122         // Chiudo il cursore (lo faccio in anticipo perché non saprei quando chiuderlo)
230800160901         exec sql  close C1;
230900160901
231000170523         wSQL = 'select vmsproc, vmsksu, count(*), vmsdde, vmsdsc, vmsdesproc +
231100160831                 from tivms00f +
231200160916                 where vmsatb = '' ''' ;
231300160916         if D03Proc <> *blank;
231400160916           wSQL = %trim(wSQL) +
231500160919                ' and vmsproc = ''' + D03Proc + '''';
231600160916         endif;
231700160916         if D03Ksu  <> *blank;
231800160916           wSQL = %trim(wSQL) +
231900160919                ' and vmsksu = ''' + D03Ksu  + '''';
232000160916         endif;
232100160916         if D03DDE    <> 0     ;
232200160916           wSQL = %trim(wSQL) +
232300160919                ' and vmsdde <= ' + %char(%dec(%date(d03DDE:*eur))) + ' and +
232400160919                  vmsdsc >= ' + %char(%dec(%date(d03DDE:*eur)));
232500160916         endif;
232600160916         if D03CauBRT <> *blank;
232700161202           D03CauBRT = %xlate('*':'%':D03CauBRT);
232800161202           if %scan('%':D03CauBRT) > 0;
232900161202             wSQL = %trim(wSQL) +
233000161205                ' and trim(vmscaubrt) like ''' + %trim(D03CauBRT) + '''';
233100161202           else;
233200161202             wSQL = %trim(wSQL) +
233300161202                ' and vmscaubrt =    ''' + %trim(D03CauBRT) + '''';
233400161202           endif;
233500161202         endif;
233600160926         if D03StsCli <> *blank;
233700161202           D03StsCli = %xlate('*':'%':D03StsCli);
233800161202           if %scan('%':D03StsCli) > 0;
233900161202             wSQL = %trim(wSQL) +
234000161205                ' and trim(vmsstscli) like ''' + %trim(D03StsCli) + '''';
234100161202           else;
234200161202             wSQL = %trim(wSQL) +
234300161202                ' and vmsstscli =    ''' + %trim(D03StsCli) + '''';
234400161202           endif;
234500160926         endif;
234600160916         wSQL = %trim(wSQL) +
234700170523                ' group by vmsproc, vmsksu, vmsdde, vmsdsc, vmsdesproc' +
234800170523                ' order by vmsproc, vmsksu, vmsdde, vmsdsc, vmsdesproc';
234900160831
235000160831         // Dichiarazione cursore
235100160831         exec sql   prepare S1   from :wSQL;
235200160831         exec sql   declare C1  asensitive   cursor for S1;
235300160831
235400160831         // Apertura del cursore
235500160831         exec sql   open C1;
235600160831
235700160901         *in99 = *off;
235800160901         if SQLCode < 0;
235900160901           *in99 = *on;
236000160831         endif;
236100160831
236200160831       ENDSR;
236300161122
236400161122       //--------------------------------------------------------------
236500161122       //?Preparazione stringa SQL per ricerca squadrature             ?
236600161122       // le Squadrature sono quei rcd il cui valore di CauBRT non è presente in un altro rcd con
236700161122       // procedura standard (*STD)
236800161122       //--------------------------------------------------------------
236900161122       BEGSR  PrepSQLSquad;
237000161122
237100161122         // Chiudo il cursore (lo faccio in anticipo perché non saprei quando chiuderlo)
237200161122         exec sql  close C2;
237300161122
237400161122         wSQL = 'select vmsproc, vmsksu, vmscautip, vmscaubrt, vmscautt , +
237500161122                 vmsstscli , vmsdde, vmsdsc, vmsdescriz +
237600161122                 from tivms00f +
237700161122                 where vmsatb = '' '' +
237800161202                 and vmscaubrt concat vmscautip not in +
237900161202                 (select vmscaubrt concat vmscautip from tivms00f +
238000161122                 where vmsatb = '' '' +
238100161122                 and vmsproc=''*STD'')';
238200161122         if D03Proc <> *blank;
238300161122           wSQL = %trim(wSQL) +
238400161122                ' and vmsproc = ''' + D03Proc + '''';
238500161122         endif;
238600161122         if D03Ksu  <> *blank;
238700161122           wSQL = %trim(wSQL) +
238800161122                ' and vmsksu = ''' + D03Ksu  + '''';
238900161122         endif;
239000161122         if D03DDE    <> 0     ;
239100161122           wSQL = %trim(wSQL) +
239200161122                ' and vmsdde <= ' + %char(%dec(%date(d03DDE:*eur))) + ' and +
239300161122                  vmsdsc >= ' + %char(%dec(%date(d03DDE:*eur)));
239400161122         endif;
239500161122         if D03CauBRT <> *blank;
239600161202           D03CauBRT = %xlate('*':'%':D03CauBRT);
239700161202           if %scan('%':D03CauBRT) > 0;
239800161202             wSQL = %trim(wSQL) +
239900161205                ' and trim(vmscaubrt) like ''' + %trim(D03CauBRT) + '''';
240000161202           else;
240100161202             wSQL = %trim(wSQL) +
240200161202                ' and vmscaubrt =    ''' + %trim(D03CauBRT) + '''';
240300161202           endif;
240400161202         endif;
240500161202         if D03StsCli <> *blank;
240600161202           D03StsCli = %xlate('*':'%':D03StsCli);
240700161202           if %scan('%':D03StsCli) > 0;
240800161202             wSQL = %trim(wSQL) +
240900161205                ' and trim(vmsstscli) like ''' + %trim(D03StsCli) + '''';
241000161202           else;
241100161202             wSQL = %trim(wSQL) +
241200161202                ' and vmsstscli =    ''' + %trim(D03StsCli) + '''';
241300161202           endif;
241400161122         endif;
241500161122         wSQL = %trim(wSQL) +
241600161122                ' order by vmsproc, vmsksu, vmsdde, vmscautip, vmscaubrt';
241700161122
241800161122         // Dichiarazione cursore
241900161122         exec sql   prepare S2   from :wSQL;
242000161122         exec sql   declare C2  asensitive   cursor for S2;
242100161122
242200161122         // Apertura del cursore
242300161122         exec sql   open C2;
242400161122
242500161122         *in99 = *off;
242600161122         if SQLCode < 0;
242700161122           *in99 = *on;
242800161122         endif;
242900161122
243000161122       ENDSR;
243100170207
243200170207       //--------------------------------------------------------------
243300170207       //?Preparazione stringa SQL per confronto
243400170207       // Mostro quei rcd delle procedure 1 (su cui ho messo l'opzione 8) e 2 (selezionata in
243500170207       // finestra, che non hanno stato clienti O tipo causale O causale BRT
243600170207       //--------------------------------------------------------------
243700170207       BEGSR  PrepSQLConfr;
243800170207
243900170207         // Chiudo il cursore (lo faccio in anticipo perché non saprei quando chiuderlo)
244000170207         exec sql  close C3;
244100170207
244200170207         wSQL = 'select vmsproc, vmsksu, vmscautip, vmscaubrt, vmscautt , +
244300170207                 vmsstscli , vmsdde, vmsdsc, vmsdescriz +
244400170207                 from tivms00f +
244500170207                 where vmsatb = '' '' +
244600170207                 and vmsproc = ''' + proc1 + ''' +
244700170207                 and vmsstscli concat vmscaubrt concat vmscautip not in +
244800170207                 (select vmsstscli concat vmscaubrt concat vmscautip +
244900170207                 from tivms00f +
245000170207                 where vmsatb = '' '' +
245100170208                 and vmsproc= + ''' + proc2 + ''')';
245200170208         if D03Proc <> *blank;
245300170208           wSQL = %trim(wSQL) +
245400170208                ' and vmsproc = ''' + D03Proc + '''';
245500170208         endif;
245600170208         if D03Ksu  <> *blank;
245700170208           wSQL = %trim(wSQL) +
245800170208                ' and vmsksu = ''' + D03Ksu  + '''';
245900170208         endif;
246000170208         if D03DDE    <> 0     ;
246100170208           wSQL = %trim(wSQL) +
246200170208                ' and vmsdde <= ' + %char(%dec(%date(d03DDE:*eur))) + ' and +
246300170208                  vmsdsc >= ' + %char(%dec(%date(d03DDE:*eur)));
246400170208         endif;
246500170208         if D03CauBRT <> *blank;
246600170208           D03CauBRT = %xlate('*':'%':D03CauBRT);
246700170208           if %scan('%':D03CauBRT) > 0;
246800170208             wSQL = %trim(wSQL) +
246900170208                ' and trim(vmscaubrt) like ''' + %trim(D03CauBRT) + '''';
247000170208           else;
247100170208             wSQL = %trim(wSQL) +
247200170208                ' and vmscaubrt =    ''' + %trim(D03CauBRT) + '''';
247300170208           endif;
247400170208         endif;
247500170208         if D03StsCli <> *blank;
247600170208           D03StsCli = %xlate('*':'%':D03StsCli);
247700170208           if %scan('%':D03StsCli) > 0;
247800170208             wSQL = %trim(wSQL) +
247900170208                ' and trim(vmsstscli) like ''' + %trim(D03StsCli) + '''';
248000170208           else;
248100170208             wSQL = %trim(wSQL) +
248200170208                ' and vmsstscli =    ''' + %trim(D03StsCli) + '''';
248300170208           endif;
248400170208         endif;
248500170208           wSQL = %trim(wSQL) +
248600170208                ' union all +
248700170207                 select vmsproc, vmsksu, vmscautip, vmscaubrt, vmscautt , +
248800170207                 vmsstscli , vmsdde, vmsdsc, vmsdescriz +
248900170207                 from tivms00f +
249000170207                 where vmsatb = '' '' +
249100170207                 and vmsproc = ''' + proc2 + ''' +
249200170207                 and vmsstscli concat vmscaubrt concat vmscautip not in +
249300170207                 (select vmsstscli concat vmscaubrt concat vmscautip +
249400170207                 from tivms00f +
249500170207                 where vmsatb = '' '' +
249600170207                 and vmsproc= ''' + proc1 + ''')';
249700170207         if D03Proc <> *blank;
249800170207           wSQL = %trim(wSQL) +
249900170207                ' and vmsproc = ''' + D03Proc + '''';
250000170207         endif;
250100170207         if D03Ksu  <> *blank;
250200170207           wSQL = %trim(wSQL) +
250300170207                ' and vmsksu = ''' + D03Ksu  + '''';
250400170207         endif;
250500170207         if D03DDE    <> 0     ;
250600170207           wSQL = %trim(wSQL) +
250700170207                ' and vmsdde <= ' + %char(%dec(%date(d03DDE:*eur))) + ' and +
250800170207                  vmsdsc >= ' + %char(%dec(%date(d03DDE:*eur)));
250900170207         endif;
251000170207         if D03CauBRT <> *blank;
251100170207           D03CauBRT = %xlate('*':'%':D03CauBRT);
251200170207           if %scan('%':D03CauBRT) > 0;
251300170207             wSQL = %trim(wSQL) +
251400170207                ' and trim(vmscaubrt) like ''' + %trim(D03CauBRT) + '''';
251500170207           else;
251600170207             wSQL = %trim(wSQL) +
251700170207                ' and vmscaubrt =    ''' + %trim(D03CauBRT) + '''';
251800170207           endif;
251900170207         endif;
252000170207         if D03StsCli <> *blank;
252100170207           D03StsCli = %xlate('*':'%':D03StsCli);
252200170207           if %scan('%':D03StsCli) > 0;
252300170207             wSQL = %trim(wSQL) +
252400170207                ' and trim(vmsstscli) like ''' + %trim(D03StsCli) + '''';
252500170207           else;
252600170207             wSQL = %trim(wSQL) +
252700170207                ' and vmsstscli =    ''' + %trim(D03StsCli) + '''';
252800170207           endif;
252900170207         endif;
253000170207         wSQL = %trim(wSQL) +
253100170207                ' order by vmsproc, vmsksu, vmsdde, vmscautip, vmscaubrt';
253200170207
253300170207         // Dichiarazione cursore
253400170207         exec sql   prepare S3   from :wSQL;
253500170207         exec sql   declare C3  asensitive   cursor for S3;
253600170207
253700170207         // Apertura del cursore
253800170207         exec sql   open C3;
253900170207
254000170207         *in99 = *off;
254100170207         if SQLCode < 0;
254200170207           *in99 = *on;
254300170207         endif;
254400170207
254500170207       ENDSR;
254600160831
254700160831       //--------------------------------------------------------------
254800161122       //?Lettura occorrenza per drill up                              ?
254900160831       //--------------------------------------------------------------
255000161122       BEGSR  LeggeSQLDrill;
255100160831
255200161122         exec sql  fetch next  from C1  into :VMS_Drill;
255300160901
255400160901         *in99 = *off;
255500160901         if SQLCode = 100 or SQLCode < 0;
255600160901           *in99 = *on;
255700160901         endif;
255800160831
255900160831       ENDSR;
256000161122
256100161122       //--------------------------------------------------------------
256200161122       //?Lettura occorrenza per ricerca squadrature                   ?
256300161122       //--------------------------------------------------------------
256400161122       BEGSR  LeggeSQLSquad;
256500161122
256600161122         exec sql  fetch next  from C2  into :VMS_Squad;
256700161122
256800161122         *in99 = *off;
256900161122         if SQLCode = 100 or SQLCode < 0;
257000161122           *in99 = *on;
257100161122         endif;
257200161122
257300161122       ENDSR;
257400160831
257500160831       /end-free
257600170207
257700170207       //--------------------------------------------------------------
257800170207       //?Lettura occorrenza per confronto                             ?
257900170207       //--------------------------------------------------------------
258000170207       BEGSR  LeggeSQLConfr;
258100170207
258200170207         exec sql  fetch next  from C3  into :VMS_Confr;
258300170207
258400170207         *in99 = *off;
258500170207         if SQLCode = 100 or SQLCode < 0;
258600170207           *in99 = *on;
258700170207         endif;
258800170207
258900170207       ENDSR;
259000170207
259100170207       /end-free
259200160831
259300970526     C*------------------------------------------------------------------------*
259400970526     C* NOTE COSTRUZIONE SCHIERE
259500970526     C*------------------------------------------------------------------------*
259600970526     C*
259700970526     C* -MSG-MESSAGGI
259800970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
259900970526     C* (NON TOCCARE MSG,1 E MSG,2)
260000970526     C*
260100970526     C* -CMD-COMANDI
260200970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
260300970526     C*
260400970526     C* -OPD-OPZIONI
260500970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
260600970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
260700970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
260800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
260900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
261000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
261100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
261200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
261300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
261400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
261500970526     C*
261600970526     C* -FUD-FUNZIONI
261700970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
261800970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
261900970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
262000970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
262100970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
262200970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
262300970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
262400970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
262500970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
262600970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
262700970526     O*------------------------------------------------------------------------*
262800970526** DEC - DECODIFICHE
262900970526    GESTIONE                                                     1
263000970526INTERROGAZIONE                                                   2
263100970530     SCELTA                                                      3
2632009705264
2633009705265
263400970526** MSG - MESSAGGI
263500160715TIS7VMS1R- Opzione non gestita                                                 1
263600160715TIS7VMS1R- Tasto funzionale non gestito                                        2
263700161123TIS7VMS1R-                                                                     3
263800160831TIS7VMS1R- Record annullato.                                                   4
263900160715TIS7VMS1R- Record non annullato. Impossibile ripristinare                      5
264000160715TIS7VMS1R- Accettato solo un parametro di inizio lista                         6
264100160715TIS7VMS1R- Errori nei parametri di ingresso                                    7
264200160715TIS7VMS1R- Data errata                                                         8
264300160919TIS7VMS1R-                                                                     9
264400160831TIS7VMS1R- Errore SQL nel reperimento dati Validi al                          10
264500160916TIS7VMS1R- Copia totale (opz. 23) eseguita correttamente                      11
264600160916TIS7VMS1R- Copia totale (opz. 23) non eseguita                                12
264700161123TIS7VMS1R- Stampa effettuata                                                  13
264800160715TIS7VMS1R- 14                                                                 14
264900160715TIS7VMS1R- 15                                                                 15
265000160715TIS7VMS1R- 16                                                                 16
265100160715TIS7VMS1R- 17                                                                 17
265200160715TIS7VMS1R- 18                                                                 18
265300160715TIS7VMS1R- 19                                                                 19
265400160715TIS7VMS1R- 20                                                                 20
265500970526** CMD - COMANDI CL
265600970526WRKSPLF
265700970526
265800970526
265900970526
266000970526
266100970526** OPD - OPZIONI
266200160923 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
266300970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
266400160914 3=Copia            3 -     (S/N)=N                        opz. 03¬
266500970526 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
266600970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
266700160713                   -  -     (S/N)=N                        opz. 06¬
266800160713                   -  -     (S/N)=N                        opz. 07¬
266900170208                   -  -     (S/N)=N                        opz. 08¬
267000160713                   -  -     (S/N)=N                        opz. 09¬
267100160713                   -  -     (S/N)=N                        opz. 10¬
267200160713                   -  -     (S/N)=N                        opz. 11¬
267300970526                   -  -     (S/N)=N                        opz. 12¬
26740016092313=Duplica         -13-     (S/N)=N                        opz. 13¬
267500970526                   -  -     (S/N)=N                        opz. 14¬
267600970526                   -  -     (S/N)=N                        opz. 15¬
267700970526                   -  -     (S/N)=N                        opz. 16¬
267800970526                   -  -     (S/N)=N                        opz. 17¬
267900970526                   -  -     (S/N)=N                        opz. 18¬
268000970526                   -  -     (S/N)=N                        opz. 19¬
268100970526                   -  -     (S/N)=N                        opz. 20¬
268200970526                   -  -     (S/N)=N                        opz. 21¬
268300970526                   -  -     (S/N)=N                        opz. 22¬
26840016092323=Copia su tutte  -23-     (S/N)=N                        opz. 23¬
268500970526                   -  -     (S/N)=N                        opz. 24¬
268600970526** FUD - FUNZIONI
268700970526                   -        (S/N)=N                        funz.01¬
268800970526                   -        (S/N)=N                        funz.02¬
268900970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
269000970526                   -        (S/N)=N                        funz.04¬
269100970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
269200970526F06=Inserimento    -        (S/N)=N                        funz.06¬
269300970526                   -        (S/N)=N                        funz.07¬
269400160916F08=Drill-up       -        (S/N)=N                        funz.08¬
269500160713                   -        (S/N)=N                        funz.09¬
269600160713                   -        (S/N)=N                        funz.10¬
269700160713                   -        (S/N)=N                        funz.11¬
269800160923F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
269900160713                   -        (S/N)=N                        funz.13¬
270000161121                   -        (S/N)=N                        funz.14¬
270100161121F15=Squadrature    -        (S/N)=N                        funz.15¬
270200170208F16=Confronto      -        (S/N)=N                        funz.16¬
270300970526                   -        (S/N)=N                        funz.17¬
270400970526                   -        (S/N)=N                        funz.18¬
270500970627 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
270600970627 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
270700161121F21=Stampa         -        (S/N)=N                        funz.21¬
270800161121                   -        (S/N)=N                        funz.22¬
270900970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
271000970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
