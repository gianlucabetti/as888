000100981214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(pnota)
000200981214     H*PARMS BNDDIR(PJXBND PJCBND)
000300970611     H*PARMS  cvtopt(*NONE)
000400970611     H DECEDIT('0,') DATEDIT(*DMY.)
000500970611     F*
000600970611     F* Contabilizzazione HOME BANKING
000700970611     F* Scrive dati ndbhm*
000800970611     F* opz = '01' contabilizza remote banking (file NDRI100F)
000900970611     F* opz = '02' contabilizza movimenti banca(file NDBMB00F)
001000020902c1574F* opz = '03' contabilizza movimenti primanota tesoreria
001100040513c1758F* opz = '04' contabilizza movimenti CASH POOLING
001200970611     F*
001300970611     FNDBHM00F  O    E           K DISK    COMMIT
001400970611     F                                     USROPN
001500030401c1642FNDBHP00F  O    E           K DISK    COMMIT
001600030401c1642F                                     USROPN
001700970611     FNDRI102L  UF   E           K DISK    usropn
001800970617     FNDBMB02L  UF   E           K DISK    usropn
001900970611     F*
002000970611     D*-----------
002100970611     D* Passaggio Parametri
002200970611     D KPJBA         E DS
002300970611     D*-----------
002400970611      * Reperimento nome PGM
002500970611     D STATUS         SDS           333
002600970611     D  DSPGM            *PROC
002700970611     D  PARMS            *PARMS
002800970611     D*-----------
002900970611     D* Dati di ambiente ottenuti da XSOC
003000970611    >D SOC001        E DS                  EXTNAME(XSOC001DS)
003100970611     D*-------------
003200970611     D* DS Interna per dati di output di XSOC
003300970611     D XSOCDS          DS          1000
003400980303     D*-------------
003500980303     D* Passaggio Parametri al PGM XATB
003600980303     D XATBDS        E DS                  INZ
003700051122     D*-------------
003800051123D1900D* parametri per NDC252R1
003900051122  "  D BHMDS         E DS                  inz EXTNAME(NDBHM00F)
004000051122D1900D BHPDS         E DS                  inz EXTNAME(NDBHP00F)
004100970611     D*-----------
004200970611     D*Parametri in entrata/uscita
004300970611     D NDC252DS      E DS
004400021014     D*-------------
004500021014c1574D* Parametri gestione tesoreria
004600021014  "  D ANP021DS      E DS
004700021014  "  D* DS gestione parametri modulo
004800021014  "  D XPAOUT          DS          2000
004900021014  "  D* Causali operative
005000021014  "  D ANOPE         E DS                  EXTNAME(ANOPE00F)
005100021014  "  D* ------ Definizioni parametri standard
005200021014  "  D  Esito          S              1
005300021014  "  D  Gesmsg         S              1
005400021014  "  D  StrutturO      S             10
005500021014  "  D  LenOut         S              9B 0
005600021014  "  D  Operaz         S              2
005700021014c1574D  Refresh        S              1
005800030401c1642D StrutturaO      S             10
005900030401     D*-----------
006000030401c1642D* controllo conto
006100030401  " >D ND002DS       E DS
006200030401c1642D*-----------
006300980303     D*Parametri chiamata saldaconto
006400051014D1879X*NDC049DS      E DS
006500051014D1879D NDC049DS      E DS                  inz
006600970611     D*-------------
006700051010C1866D* Parametri parametro modulo HBPARBAS
006800051010C1866D NdCk90ds      E DS
006900051010     D*-------------
007000051019C1875D* Parametri contabile d'addebito
007100051019C1875D NdCH15ds      E DS                  inz
007200051019     D*-------------
007300970611     D* Parametri per gestione p.nota interattiva
007400970611     D NDC071DS      E DS                  INZ
007500970611    >D  ERR071       E                     INZ('0')
007600970611     D*-----------
007700970611     D £NDREG        E DS                  EXTNAME(NDREG00F) import
007800970611     D*-----------
007900980303     D* serve solo per il REFFLD delle variabili
008000980303     D NDPAS00F      E DS
008100980303     D*-----------
008200970611     D* DS Esterna gestione messaggi
008300970611     D XMSGDS        E DS
008400970611     D  STS          E                     EXTFLD(MSGSTS)
008500970611     D                                     DIM(333)
008600970611     D  JBA          E                     EXTFLD(MSGJBA)
008700980303     D                                     DIM(502)
008800980303     D*-------------
008900980303     D* Tabella saldaconto
009000980303     D ANG0IPDS      E DS                  based(P0IP)
009100980303     D ANG1IPDS      E DS                  based(P1IP)
009200980303     D ANG2IPDS      E DS                  based(P2IP)
009300980303     D DSAut           DS           512    based(PDSAut)
009400980303     D*-------------
009500980303     D* Schiera per multicodice (dettaglio per conto)
009600070717D2236X*Sk1Conto        S             14    dim(100)
009700070717  "  X*Sk1ImportD      S                   dim(100) like(BHMImportD)
009800070717  "  D $DimSk1         C                   Const(500)
009900070717  "  D Sk1Conto        S             14    dim($dimsk1)
010000070717D2236D Sk1ImportD      S                   dim($dimsk1) like(BhmImportD)
010100980303     D*-------------
010200980303     D* DS multipla per saldaconto vero e proprio
010300070717D2236x*Sk2Partite      DS                  occurs(100)
010400070717  "  D $DimSk          C                   Const(3000)
010500070717D2236D Sk2Partite      DS                  occurs($DimSK)
010600980303     D  Sk2Kcc                             like(PASKcc)
010700980303     D  Sk2Ksc                             like(PASKsc)
010800980303     D  Sk2ImportD                         like(PASImportD)
010900980303     D  Sk2DtPar                           like(PASDtPar)
011000980303     D  Sk2NrPar                           like(PASNrPar)
011100980303     D  Sk2SerPar                          like(PASSerPar)
011200980303     D  Sk2DtSca                           like(PASDtSca)
011300980303     D  Sk2ImpDivC                         like(PASImportD)
011400980303     D  Sk2ImpAbbA                         like(PASImportD)
011500980303     D  Sk2ImpAbbP                         like(PASImportD)
011600980303     D  Sk2Sconto                          like(PASImportD)
011700980303     D*-------------
011800980303     D* DS per fleggaggio effetti al dopo incasso
011900980303     D Sk3DInc         DS
012000980303     D* Schiere per cancellazione registrazioni provv. in elaborazione
012100070717D2236X**Sk3Sys                             dim(100) like(PASSys    )
012200070717D2236X**Sk3NrAsReg                         dim(100) like(PASNrAsReg)
012300070717D2236D  Sk3Sys                             Dim($DimSK) Like(PASSys    )
012400070717D2236D  Sk3NrAsReg                         Dim($DimSK) Like(PASNrAsReg)
012500040513     D*-----------
012600980303     D* indici
012700070717D2236X*y               S              3s 0
012800070717D2236D y               S              5s 0
012900040513     D*-----------
013000040513c1758D* Campi di lavoro
013100040513  "  D $SubNum         S                   Like(BhmSubNum) Inz(0)
013200040518  "  D SavRifInt       S                   Like(BhmRifInt) Inz
013300040519c1758D Wrksocieta      S                   Like(BhmSocieta)Inz
013400051122D1900D SAVsocieta      S                   Like(BhmSocieta)Inz
013500980303     D*-----------
013600040513c1574D  ImpSpeseD      S                   like(PASImportD)
013700021113  "  D* Prototipazione procedure EURO
013800021113c1574D/COPY *LIBL/SRCCPY,PJX002PR
013900970611     c/EJECT
014000970611      *----------------------------------------------------*
014100970611      *                MAIN LINE PROGRAM
014200970611      *----------------------------------------------------*
014300970611     C* Inizializza variabili
014400980303     C                   EXSR      InzVAr
014500040518c1758c* se Cash pooling
014600040518  "  c                   if        Opz252 = '04'
014700040518  "  c                   exsr      CashPool
014800040518  "  c                   exsr      Endpgm
014900040518C1758C                   endif
015000980303     C*
015100980309     C* se la tabella saldaconto non è stata impostata in proposta
015200980309     C                   IF        TabSalC252 = *blank
015300980303     C*
015400970611     C* Scrittura archivio contabilizzazione
015500970611     C                   EXSR      ScriveBhm
015600970612     C* Richiamo programma prima nota
015700040518c1758c                   eval      WrkSocieta = Societa252
015800970612     C                   EXSR      CallPnota
015900970613     c* se non errore, ripasso avvenuta conferma
016000970612     c                   if        err071 = *off
016100970612     c                   eval      opr252 = *on
016200970612     c                   else
016300970613     c* se errore, ma non digitato F3 o F12,richiamo con apertura prima nota
016400970613     c* 2a chiamata
016500970613     c                   if        Ret071<> '1' and Ret071 <> '2'
016600970612     C                   EXSR      ScriveBhm
016700970612     c                   eval      societa071 = Societa252
016800970612     c                   move      knraz         RifInt071
016900970612     c                   eval      dtregda071 = *loval
017000970612     c                   eval      dtrega071  = *hival
017100970612     c                   eval      subnumd071 = *loval
017200970612     c                   eval      subnuma071 = *hival
017300970612     c                   eval      interat071 = *on
017400970612     c                   movel     ndc071ds      KPJBU
017500970612     C                   CALL      'NDC071R'
017600970612     C                   PARM                    KPJBA
017700970612     c                   movel     kpjbu         NDC071DS
017800970612     c                   if        err071 = *off
017900970612     c                   eval      opr252 = *on
018000970613     c                   endif
018100970612     c                   endif
018200970612     c                   endif
018300980303     C*
018400980309     C* se la tabella saldaconto è stata impostata in proposta
018500980303     C                   ELSE
018600051019C1875c* se non richiesta contabile d'addebito
018700051019C1875C                   IF        TabSalC252 <> '*PP0'
018800980303     C* richiamo direttamente il saldaconto
018900980303     C                   EXSR      CallSaldaC
019000051019C1875c                   else
019100051019  "  C                   EXSR      CallContAd
019200051019C1875c                   endif
019300980303     C                   ENDIF
019400970611     c*
019500970611     C* Agggiornamento archivi
019600970611     c                   if        opr252 = *on
019700970611     c*
019800970611     c                   select
019900970611     c                   when      Opz252 = '01'
020000970611     c                   exsr      AggNdRi1
020100970611     c*
020200991210     c                   when      Opz252 = '02'
020300970611     c                   exsr      AggNdBmb
020400040518     c                   endsl
020500040514     c*
020600970611     c                   endif
020700040514     C*
020800970611     c                   exsr      endpgm
020900970611     c/EJECT
021000970611      ************************************************************
021100970611      * fine programma                                           *
021200970611      ************************************************************
021300970611     C     Endpgm        BEGSR
021400970611     c*
021500970617     c                   unlock    ndRi102l                             21
021600970617     c                   unlock    ndbmb02l                             21
021700970617     c*
021800970611     C                   SETON                                        RT
021900970617     C                   return
022000980303     C*
022100980303     C                   endsr
022200040518     C/EJECT
022300040518c1758C************************************************************
022400040518  "  C* Contabilizzazione cash pooling                          *
022500040518c1758C************************************************************
022600040518     C     CashPool      BEGSR
022700040518     c*
022800040519     C* Scrittura archivio contabilizzazione per le due società
022900040519     c                   if        GiroCp252='1' or GiroCp252='2'
023000040518     C                   EXSR      ScriveBhm
023100040518     c* do commit per lasciare i file negli archivi BH
023200040518     C                   COMMIT
023300040519     c                   endif
023400040519     c*
023500040519     c* Al secondo giro aggiorno NDRI100F
023600040519     c                   if        GiroCp252='2'
023700040519     c                   exsr      AggNdRi1
023800040519     c                   endif
023900040519     C*
024000040519     C* Alla fine richiamo pnota per le due società
024100040519     c                   if        GiroCp252='X'
024200040519     c                   eval      WrkSocieta = SocCp252
024300040518     c                   exsr      CallPnota
024400040518     c                   eval      Err252=Err071
024500040519     c                   eval      WrkSocieta = Societa252
024600040519     c                   exsr      CallPnota
024700040519     c                   eval      Err252=Err071
024800040518     C                   endif
024900040518     c*
025000040518     C                   endsr
025100040518c1758C/EJECT
025200980303     C************************************************************
025300980303     C* Scrittura dati di output                                 *
025400980303     C************************************************************
025500980303     C     ScriveBhm     BEGSR
025600040513     c*
025700040513c1758c* se Cash Pooling
025800040513  "  c* Aggiorno SubNum al primo giro
025900090313c1758c                   IF        GiroCp252 ='1'
026000090313D2497c                             or (SavSocieta = Societa252  and
026100090313D2497c                             GiroCp252  = '2')
026200090313C1758c                   Eval      $SubNum = $SubNum + 1
026300040513c1758c                   Endif
026400980303     c* 1a riga
026500040513c1758c                   if        Opz252 = '04'
026600040513  "  c                   Move      SavRifInt     BhmRifInt
026700050404  "  c                   eval      BhmSubNum  =  $SubNum
026800040513c1758c                   else
026900970611     c                   move      knraz         BhmRifInt
027000040513     c                   eval      BhmsubNum = 1
027100040513c1758c                   endif
027200040518c1758c                   move      knraz         BhmKnraz
027300970611     c                   eval      Bhmsubrig = 1
027400970611     c                   eval      BhmCambio  =  Cambio252
027500970611     c                   eval      BhmCausTes =  CauCont252
027600970611     c                   eval      BhmClasse  =  Classe252
027700970611     c                   eval      BhmCtb     =  Ctb252
027800970611     c                   eval      BhmDivisa  =  Divisa252
027900970611     c                   eval      BhmDtdoc   =  DtDoc252
028000970611     c                   eval      BhmNrDoc   =  NrDoc252
028100970611     c                   eval      BhmSerDoc  =  SerDoc252
028200970611     c                   eval      BhmDtReg   =  DtReg252
028300970611     c                   eval      BhmNrReg   =  NrReg252
028400970611     c                   eval      BhmSerReg  =  SerReg252
028500040518c1758C                   IF        GiroCp252='1'
028600051010c1758c                   eval      BhmSocieta =  SocCp252
028700051010c1866c* reperisco unità soc. cash pooling da param. modulo
028800051010c1866c                   exsr      RepUniCP
028900051010c1866c                   eval      BhmUnita   =  UnitaK90
029000040518c1758c                   else
029100970611     c                   eval      BhmSocieta =  Societa252
029200051010c1866c                   eval      BhmUnita   =  Unita252
029300040518c1758c                   endif
029400970611     c                   eval      BhmTpRegz  =  Tpregz252
029500051010c1866x****               eval      BhmUnita   =  Unita252
029600021015c1574c                   if        opz252 <> '03'
029700021015     c                   eval      BhmFungen  =  'H'
029800021015c1574c                   else
029900021015  "  c                   eval      BhmFungen  =  '1'
030000021113  "  C                   eval      BhmDivisaM= Divisa252
030100021113  "  C                   eval      BhmCambioM= Cambio252
030200021015c1574c                   endif
030300970611     c                   eval      BhmBanca   =  Banca252
030400970611     c                   eval      BhmCausRig =  CausRi1252
030500970611     c                   eval      BhmDesBrev =  Descr252
030600970611     c                   eval      BhmImportD =  ImportD252
030700970611     c                   eval      BhmImportO =  Importo252
030800970611     c                   eval      BhmKcc     =  kcc252
030900970611     c                   eval      Bhmksc     =  ksc252
031000050525D1789c* contropartita la metto solo se le spese non sono incluse
031100050525D1789c                   if        incluse252<>*on
031200970611     c                   eval      BhmKcccpu  =  kccCpu252
031300970611     c                   eval      Bhmksccpu  =  kscCpu252
031400050525D1789C                   else
031500050525  "  c                   clear                   BhmKccCpu
031600050525  "  c                   clear                   BhmKscCpu
031700050525D1789C                   endif
031800970611     c                   eval      BhmSegno    = Segno252
031900970611     c* campi di tesoreria
032000970611     c                   eval      BhmCauBan   = CauTeso252
032100970611     c                   eval      BhmdtOpe    = Dtope252
032200970611     c                   eval      BhmdtVal    = Dtval252
032300970611     c                   eval      BhmLinea    = Linea252
032400970611     c                   eval      BhmNrTeso   = NrTeso252
032500051006D1876c                   if        Spunta252 = *on
032600051006  "  c                   eval      BhmCertif   = *on
032700051006  "  c                   else
032800051006  "  c                   eval      BhmCertif   = *off
032900051006D1876C                   endif
033000021030     c*
033100021105c1574c* se le spese sono incluse le devo
033200021030  "  c                   if        incluse252=*on
033300021113  "  c* converte importo se divisa <>
033400021113  "  c                   if        BhmDivisa <>xscdiv
033500021113  "  c                   exsr      ConverteIm
033600021113  "  c                   else
033700021113  "  c                   z-add     ImpSpes252    ImpSpeseD
033800021113  "  c                   endif
033900021105  "  c* aggiungere all'importo banca se si tratta di un accredito
034000021105  "  c                   if        Segno252='A'
034100021113  "  c                   add       ImpSpeseD     BhmimportD
034200021105  "  c                   add       ImpSpes252    Bhmimporto
034300021105  "  c                   else
034400021105  "  c* togliere  all'importo banca se si tratta di un addebito
034500021113  "  c                   sub       ImpSpeseD     BhmimportD
034600021105  "  c                   sub       ImpSpes252    Bhmimporto
034700021105  "  c                   endif
034800021030c1574c                   endif
034900970611     c*
035000051123D1900c* se societa di NDBHM = SOCIETA' DI COLLEGAMENTO, scrivo
035100051122D1900c                   if        BhmSocieta =  SavSocieta
035200970611     c                   write     NDBHM000
035300051122D1900C                   else
035400051123  "  c* altrimenti cambio job anche se stesso sistema informativo
035500051123  "  c* perchè la pn batch non può essere chiamata successivamente per
035600051123  "  c* 2 società differenti: 1) ndmsg00w non si pulisce 2) alcuni modu
035700051123  "  c* li chiamano xsoc nella *inszr, quindi rimangono i dati società
035800051123  "  c* precedente
035900051122  "  C                   movel     BhmSocieta    Kpjbu
036000051122  "  c                   call      'NDC252R1'
036100051122  "  C                   PARM                    KPJBA
036200051122  "  C                   PARM                    BhmDs
036300051122  "  C                   PARM                    BhPDs
036400051123  "  C                   PARM      'M'           Tipo              1
036500051122D1900C                   endif
036600051122     c*
036700030401c1642c* se contabilizzazione '01'
036800030401  "  c                   if        Opz252 = '01'
036900030401  "  C* vedo se conto gestito a partite
037000030401  "  C                   RESET                   ND002DS
037100030401  "  C                   EVAL      LenOut = %Size(ND002DS)
037200030401  "  C                   CallB     'NDMVC002'
037300030401  "  C                   PARM                    societa252
037400030401  "  C                   PARM                    bhmkcc
037500030401  "  C                   PARM                    bhmksc
037600030401  "  C                   PARM                    *omit
037700030401  "  C                   PARM      *OFF          GesMsg
037800030401  "  C                   PARM      *OFF          Esito
037900030401  "  C                   PARM      'ND002DS'     StrutturaO
038000030401  "  C                   PARM                    ND002DS
038100030401  "  C                   PARM                    LenOut
038200030401  "  C                   IF        Esito=*off and
038300030401  "  c* e se conto gestito a partite
038400030401  "  C                             TpGest002  <> '0'
038500030401  "  C                   EXSR      ScriveBhp
038600030401  "  c                   endif
038700030401c1642c                   endif
038800970611     c* 2a riga
038900970611     c                   eval      Bhmsubrig = 2
039000970611     c                   eval      BhmKcc     =  kcccpu252
039100970611     c                   eval      Bhmksc     =  ksccpu252
039200050525D1789X*                  clear                   BhmKccCpu
039300050525  "  X*                  clear                   BhmKscCpu
039400050525  "  c                   eval      BhmKccCpu  =  kcc252
039500050525D1789c                   eval      BhmKscCpu  =  ksc252
039600970611     c                   eval      BhmCausrig =  Causri2252
039700970611     c                   if        Segno252 = 'D'
039800970611     c                   eval      BhmSegno    = 'A'
039900970611     c                   else
040000970611     c                   eval      BhmSegno    = 'D'
040100970611     C                   endif
040200021030     c                   eval      BhmImportD =  ImportD252
040300021030     c                   eval      BhmImportO =  Importo252
040400030403C1642c* campi di tesoreria
040500030403  "  c                   if        CauCcp252 <> *blanks
040600030403  "  c                   eval      BhmBanca   =  BancaCp252
040700030403  "  c                   eval      Bhmlinea   =  LineaCp252
040800030401  "  c                   eval      BhmCauBan   = CauCcp252
040900030401c1642c                   endif
041000970611     c*
041100051122D1900c                   if        BhmSocieta =  SavSocieta
041200970611     c                   write     NDBHM000
041300051122D1900C                   else
041400051122  "  C                   movel     BhmSocieta    Kpjbu
041500051122  "  c                   call      'NDC252R1'
041600051122  "  C                   PARM                    KPJBA
041700051122  "  C                   PARM                    BhmDs
041800051122  "  C                   PARM                    BhPDs
041900051123  "  C                   PARM      'M'           Tipo
042000051122D1900C                   endif
042100030401     c*
042200030401c1642c* se contabilizzazione '01'
042300030401  "  c                   if        Opz252 = '01'
042400030401  "  C* vedo se conto gestito a partite
042500030401  "  C                   RESET                   ND002DS
042600030401  "  C                   EVAL      LenOut = %Size(ND002DS)
042700030401  "  C                   CallB     'NDMVC002'
042800030401  "  C                   PARM                    societa252
042900030401  "  C                   PARM                    bhmkcc
043000030401  "  C                   PARM                    bhmksc
043100030401  "  C                   PARM                    *omit
043200030401  "  C                   PARM      *OFF          GesMsg
043300030401  "  C                   PARM      *OFF          Esito
043400030401  "  C                   PARM      'ND002DS'     StrutturaO
043500030401  "  C                   PARM                    ND002DS
043600030401  "  C                   PARM                    LenOut
043700030401  "  C                   IF        Esito=*off and
043800030401  "  c* e se conto gestito a partite
043900030401  "  C                             TpGest002  <> '0'
044000030401  "  C                   EXSR      ScriveBhp
044100030401  "  c                   endif
044200030401c1642c                   endif
044300021014     c*
044400021014c1574c* se chieste le spese, le scrive
044500021014  "  c                   if        Spese252=*on
044600021014  "  C                   EXSR      ScriveSpe
044700021014c1574c                   endif
044800970611     c*
044900970611     C                   ENDSR
045000021014     c/EJECT
045100030401c1642 ************************************************************
045200030401  "   * Scrive partite
045300030401c1642 ************************************************************
045400030401     C     ScriveBhp     BEGSR
045500030401     C*
045600030401     C                   Reset                   NdBhp000
045700030401     c                   eval      BhpRifInt   = BhmRifInt
045800030401     c                   eval      BhpSubNum   = BhmSubNum
045900030401     c                   eval      BhpSubRig   = BhmSubRig
046000030401     c                   eval      BhpSubSubR  = 1
046100030401      *
046200030401     c                   eval      BhpSocieta  = Societa252
046300061017D2067C* se richiesto da F18=Default
046400061017D2067c                   If        RifPar252 = *on
046500030401     C                   Eval      BhpDtPar    = DataCar252
046600030401     c                   Eval      BhpNrPar    = NrCAr252
046700030401     c                   Eval      BhpSerPar   = *blanks
046800061017D2067C                   endif
046900030401     C                   eval      BhpDtSca    = DataCar252
047000030401     C                   eval      BhpDtFlu    = *loval
047100030401     C                   Clear                   BhpNrRata
047200030401      *
047300030401     C                   Eval      BhpSegno    = BhmSegno
047400030401     c                   eval      BhpDivisa   = Divisa252
047500030401      *
047600030401     c                   eval      BhpImporto  = BhmImporto
047700030401     c                   eval      BhpImportD  = BhmImportD
047800030401      *
047900030401     c                   eval      BhpImpDivM  = BhpImportD
048000030401     c                   eval      BhpImpDivC  = BhpImportD
048100030401     c                   eval      BhpMovInv   = *off
048200030401     C                   Clear                   BhpMdRd
048300030401     C                   Clear                   BhpMdPag
048400030401     c                   eval      BhpPagMan   = *off
048500030401     C                   Clear                   BhpBkAmm
048600030401     c                   eval      BhpCliSoll  = *off
048700030401     C                   Clear                   BhpCtgAna1
048800030401     C                   Clear                   BhpCtgAna2
048900030401     C                   Clear                   BhpCtgAna3
049000030401     C                   Clear                   BhpKccFact
049100030401     C                   Clear                   BhpKscFact
049200030401     C                   Clear                   BhpKscAge
049300030401     c                   eval      BhpStCred   = *off
049400030401     C                   Clear                   BhpTpAppo
049500030401      *
049600030401     C                   Clear                   BhpRitAcc
049700030401     C                   Clear                   BhpPercen
049800030401     C*
049900051122D1900c                   if        BhmSocieta =  SavSocieta
050000030401     c                   write     NdBhp000
050100051122D1900c                   else
050200051122  "  C                   movel     BhmSocieta    Kpjbu
050300051122  "  c                   call      'NDC252R1'
050400051122  "  C                   PARM                    KPJBA
050500051122  "  C                   PARM                    BhmDs
050600051122  "  C                   PARM                    BhPDs
050700051123  "  C                   PARM      'P'           Tipo
050800051122D1900C                   endif
050900030401     C*
051000030401C1642C                   ENDSR
051100021014c1574 ************************************************************
051200021014  "   * Scrive le spese
051300021014c1574 ************************************************************
051400021014     C     ScriveSpe     BEGSR
051500021014     c*
051600021014     c* 3a riga spese
051700021105     c* vanno sempre in dare alla banca
051800021014     c                   eval      Bhmsubrig = 3
051900021014     c                   eval      BhmImportD =  ImpSpes252
052000021014     c                   eval      BhmImportO =  ImpSpes252
052100021014     c                   eval      BhmKcc     =  KccCps021
052200021014     c                   eval      Bhmksc     =  KscCps021
052300021014     c                   eval      Bhmkcccpu  =  kcc252
052400021014     c                   eval      Bhmksccpu  =  ksc252
052500021014     c                   eval      BhmCausrig =  CauSpes021
052600021105     c                   eval      BhmSegno    = 'D'
052700021113     C                   eval      BhmDivisaM= xscdiv
052800021113     C                   eval      BhmCambio = 1
052900051122D1900c                   if        BhmSocieta =  SavSocieta
053000021014     c                   write     NDBHM000
053100051122D1900c                   else
053200051122  "  C                   movel     BhmSocieta    Kpjbu
053300051122  "  C                   call      'NDC252R1'
053400051122  "  C                   PARM                    KPJBA
053500051122  "  C                   PARM                    BhmDs
053600051122  "  C                   PARM                    BhPDs
053700051123  "  C                   PARM      'M'           Tipo
053800051122D1900C                   endif
053900021014     c*
054000021105     c* se spese non incluse, deve scriver anche le riga averedi spese
054100021014     c* 4a riga spese
054200021014     c                   if        incluse252=*off
054300021014     c                   eval      Bhmsubrig = 4
054400021014     c                   eval      BhmImportD =  ImpSpes252
054500021014     c                   eval      BhmImportO =  ImpSpes252
054600021014     c                   eval      BhmKcc     =  Kcc252
054700021014     c                   eval      Bhmksc     =  Ksc252
054800050525D1789X*                  clear                   Bhmkcccpu
054900050525  "  X*                  clear                   Bhmksccpu
055000050525  "  C                   eval      Bhmkcccpu  =  kcccps021
055100050525D1789C                   eval      Bhmksccpu  =  ksccps021
055200021014     c                   eval      BhmCausrig =  CauSpes021
055300021105     c                   eval      BhmSegno    = 'A'
055400021014     c*
055500021014     c* reperisco causale operazione bancaria
055600021014     C                   EVAL      LenOut = %Size(Anope)
055700021014     C                   CALLB     'NDMVC003'
055800021014     C                   PARM                    Societa252
055900021014     C                   PARM                    CauSpes021
056000021014     C                   PARM                    *omit
056100021014     C                   PARM      '0'           Uso               1
056200021014     C                   PARM      *off          GesMsg
056300021014     C                   PARM                    Esito
056400021014     C                   PARM      'ANOPE'       StrutturO
056500021014     C                   PARM                    Anope
056600021014     C                   PARM                    Lenout
056700021014     C                   PARM      *off          Refresh
056800021014     C* se non errore
056900021014 b2  C                   IF        Esito = *off
057000021014     c                   eval      BhmCauBan   = OpeCauban
057100021014     c                   endif
057200021014     c*
057300051122D1900c                   if        BhmSocieta =  SavSocieta
057400021014     c                   write     NDBHM000
057500051122D1900c                   else
057600051122  "  C                   movel     BhmSocieta    Kpjbu
057700051122  "  C                   call      'NDC252R1'
057800051122  "  C                   PARM                    KPJBA
057900051122  "  C                   PARM                    BhmDs
058000051122  "  C                   PARM                    BhPDs
058100051123  "  C                   PARM      'M'           Tipo
058200051122D1900C                   endif
058300021014     c                   endif
058400021014     c*
058500021014c1574C                   ENDSR
058600021113c1574C************************************************************
058700021113     C* Converte importo spese in divisa
058800021113c1574C************************************************************
058900021113     C     ConverteIm    BEGSR
059000021113     C*
059100021113     c                   clear                   ImpSpeseD
059200021113     C* controllo congruenza importi/cambio
059300021113     C                   RESET                   PJX00202DS
059400021113     C                   MOVEL     BhmDivisa     Divisa0202
059500021113     C                   MOVEL     XscDiv        DivMC0202
059600021113     C                   Z-ADD     0             ImporD0202
059700021113     C                   Z-ADD     ImpSpes252    ImporM0202
059800021115     C                   Z-ADD     bhmCambio     Cambio0202
059900021113     C                   MOVEL     BhmDtVal      DtRif0202
060000021113     C*
060100021113     C                   MOVEL     *off          GesMsg
060200021113     C*
060300021113     C                   CALLP     X0202TestC(PJX00202DS :
060400021113     C                                        %size(PJX00202DS) :
060500021113     C                                        GesMsg : Esito :
060600021113     C                                        *omit  : *omit)
060700021113   B1C                   IF        Esito = *off
060800021113     C                   EVAL      ImpSpeseD = ImporD0202
060900021113     c                   endif
061000021113     C*
061100021113c1574C                   ENDSR
061200970611     c/EJECT
061300970611      ************************************************************
061400970611      * Chiama contabilizzazione in interattivo                  *
061500970611      ************************************************************
061600970612     C     CallPNota     BEGSR
061700051122     c
061800970612     c* 1a chiamata
061900970611     C                   RESET                   NDC071DS
062000040518C1758X****               eval      societa071 = Societa252
062100040518C1758c                   eval      Societa071 = WrkSocieta
062200970611     c                   move      knraz         RifInt071
062300970611     c                   eval      dtregda071 = *loval
062400970611     c                   eval      dtrega071  = *hival
062500970611     c                   eval      subnumd071 = *loval
062600970611     c                   eval      subnuma071 = *hival
062700970612     c* Se richiesta apertura primanota
062800970612     c                   if        ApriPn252 = *on
062900970611     c                   eval      interat071 = *on
063000970612     c                   else
063100970612     c                   eval      interat071 = *off
063200970612     c                   endif
063300970611     c                   movel     ndc071ds      KPJBU
063400051122     C*
063500051122D1900c                   if        WrkSocieta= SavSocieta
063600051122     C*
063700040514C1758C* Se cash pooling chiamo il cl per avere la stampa degli
063800040514  "  c* errori
063900051123  "  c* no cash pooling
064000040514c1758c                   if        Opz252 <> '04'
064100970611     C                   CALL      'NDC071R'
064200040514     C                   PARM                    KPJBA
064300040514c1758c                   ELSE
064400051123  "  c* si cash pooling
064500040514  "  C                   CALL      'NDC071C'
064600040514  "  C                   PARM                    KPJBA
064700040514C1758C                   endif
064800051122D1900C                   else
064900051123  "  c* società differente da collegamento, quindi solo cash pooling
065000051122  "  c                   call      'NDC252R1'
065100051122  "  C                   PARM                    KPJBA
065200051122  "  C                   PARM                    BhmDs
065300051122  "  C                   PARM                    BhPDs
065400051122  "  C                   PARM      'C'           Tipo              1
065500051122D1900C                   endif
065600051122     c*
065700051122     c                   movel     kpjbu         NDC071DS
065800970611     c*
065900970611     C                   ENDSR
066000980303     C/EJECT
066100980303     C************************************************************
066200980303     C* Richiamo saldaconto
066300980303     C************************************************************
066400980303     C     CallSaldaC    BEGSR
066500980303     C*
066600980303     C* riempie la DS di passaggio + gli altri parms
066700980303     C                   EXSR      Rie049DS
066800980303     C*
066900980303     C                   MOVEL     NDC049DS      Kpjbu
067000980303     C* non passo né le DS delle tabelle saldaconto né la DS delle
067100980303     C* autorizzazioni in modo che il pgm chiamato se le reperisca da sé
067200980303     C                   EVAL      P0IP = *null
067300980303     C                   EVAL      P1IP = *null
067400980303     C                   EVAL      P2IP = *null
067500980303     C                   EVAL      PDSAut = *null
067600980303     C*
067700980303     C                   CALL      'NDC049R'
067800980303     C                   PARM                    Kpjba
067900980303     C                   PARM                    ANG0IPDS
068000980303     C                   PARM                    ANG1IPDS
068100980303     C                   PARM                    ANG2IPDS
068200980303     C                   PARM                    DSAUT
068300980303     C                   PARM                    Sk1Conto
068400980303     C                   PARM                    Sk1ImportD
068500980303     C                   PARM                    Sk2Partite
068600980303     C                   PARM                    Sk3DInc
068700980303     C*
068800980303     C                   MOVEL     Kpjbu         NDC049DS
068900980303     C*
069000980303     C                   EXSR      Rtn049
069100980303     C*
069200980303     C                   ENDSR
069300980303     C/EJECT
069400980303     C************************************************************
069500980303     C* Riempie la DS di passaggio + tutti gli altri paramentri
069600980303     C************************************************************
069700980303     C     Rie049DS      BEGSR
069800980303     C*
069900980303     C                   RESET                   NDC049DS
070000980303     C* pulisco le schiere e tabelle di passaggio
070100980303     C                   CLEAR                   Sk1Conto
070200980303     C                   CLEAR                   Sk1ImportD
070300070717D2236X***                DO        100           Y
070400070717D2236C                   DO        $DimSk        Y
070500980303     C     Y             OCCUR     Sk2Partite
070600980303     C                   CLEAR                   Sk2Partite
070700980303     C                   ENDDO
070800980303     C                   CLEAR                   Sk3Sys
070900980303     C                   CLEAR                   Sk3NrAsReg
071000980303     C*
071100980303     C* campi che arrivano dal preambolo
071200060116c1891c                   add       SpeEst252     ImportD252
071300060116  "  c* Se moneta di conto, aggiungo le spese
071400060116  "  c                   if        divisa252 =xscdiv
071500060116  "  c                   add       SpeEst252     Importo252
071600051213  "  c                   add       ImpSpes252    Importo252
071700051213  "  c                   add       ImpSpes252    ImportD252
071800060116  "  c                   endif
071900060116c1891c                   eval      SpeEstD049=SpeEst252
072000980303     C                   EVAL      Ctb049 = Ctb252
072100980303     C                   EVAL      TpRegz049 = TpRegz252
072200980309     C                   EVAL      TabSald049 = TabSalC252
072300980303     C                   EVAL      UnitaS049 = *blank
072400980303     C                   EVAL      UnitaR049 = Unita252
072500980303     C                   EVAL      Banca049 = Banca252
072600980303     C                   EVAL      Linea049 = Linea252
072700980303     C                   EVAL      Kcc049 = Kcc252
072800980303     C                   EVAL      Ksc049 = Ksc252
072900980303     C                   EVAL      ImportD049 = ImportD252
073000980303     C                   EVAL      Divisa049 = Divisa252
073100980303     C                   EVAL      SerReg049 = SerReg252
073200980303     C                   EVAL      NrReg049 = NrReg252
073300980303     C                   EVAL      DtReg049 = DtReg252
073400980303     C                   EVAL      SerDoc049 = SerDoc252
073500980303     C                   EVAL      NrDoc049 = NrDoc252
073600980303     C                   EVAL      DtDoc049 = DtDoc252
073700980303     C                   EVAL      DtOpe049 = DtOpe252
073800980304     C                   EVAL      DtVal049 = DtVal252
073900021014c1574c                   eval      gesspes049=spese252
074000021014  "  c                   eval      spese049  =impspes252
074100021030  "  c                   eval      incspe1049=incluse252
074200021030c1574c                   eval      fungen049 =fungen252
074300051014D1879c                   eval      Spunta049 =Spunta252
074400980304     C**********************************************************************
074500980304     C* questi sono gli altri dati che questo pgm passa alla pnota batch e
074600980304     C* che non passa al saldaconto
074700980304     C**********************************************************************
074800980303     c*                  eval      BhmCausTes =  CauCont252
074900980303     c*                  eval      BhmCausRig =  CausRi1252
075000980303     c*                  eval      BhmFungen  =  'H'
075100980303     c*                  eval      BhmCauBan   = CauTeso252
075200980304     C**********************************************************************
075300980303     C* imposto la schiera del conto da saldacontare
075400980303     C                   MOVEL     KccCpU252     Sk1Conto(1)
075500980303     C                   MOVE      KscCpU252     Sk1Conto(1)
075600980303     C                   EVAL      Sk1ImportD(1) = ImportD252
075700980303     C*
075800980303     C                   ENDSR
075900980303     C/EJECT
076000980303     C************************************************************
076100980303     C* Ritorno da decisore saldaconto
076200980303     C************************************************************
076300980303     C     Rtn049        BEGSR
076400980303     C*
076500980303     C* modo di ritorno
076600980303     C*
076700980303   B1C                   SELECT
076800980303     C* questi modi di utilizzo dei valori di ritorno dal
076900980303     C* pgm di manutenzione rcd di anagrafica sono delle
077000980303     C* proposte, normalmente sempre valide, ma modificabili
077100980303     C* per situazioni particolari
077200980303     C* 1 = F3
077300980303     C     RET049        WHENEQ    '1'
077400980303     C***                MOVE      '1'           RET252
077500980303     C* 2 = F12
077600980303     C     RET049        WHENEQ    '2'
077700980303     C***                MOVE      '1'           RET252
077800980303     C*
077900980303   E1C                   ENDSL
078000980303     C*
078100980303     C* operazione eseguita dal pgm chiamato
078200980303     C*
078300980303     C                   SELECT
078400980303     C* 1 = operazione eseguita
078500980303     C     Opr049        WHENEQ    '1'
078600980303     C                   EVAL      Opr252 = '1'
078700980303     C*
078800980303     C                   OTHER
078900980303     C                   EVAL      Opr252 = '0'
079000980303     C*
079100980303     C                   ENDSL
079200980303     C*
079300980303     C* funzione non eseguibile per errore :
079400980303     C*
079500980303     C                   SELECT
079600980303     C* 1 = funzione richiamata chiusa in errore
079700980303     C     ERR049        WHENEQ    '1'
079800980303     C                   EVAL      Err252 = '1'
079900980303     C*
080000980303     C                   ENDSL
080100980303     C*
080200980303     C                   ENDSR
080300980303     c/EJECT
080400051019c1875C************************************************************
080500051019  "  C* Richiamo contabile addebito
080600051019  "  C************************************************************
080700051019  "  C     CallContAd    BEGSR
080800051019  "  C*
080900051019  "  C* riempie la DS di passaggio + gli altri parms
081000051019  "  C                   EXSR      RieH15DS
081100051019  "  C*
081200051019  "  C                   MOVEL     NDCH15DS      Kpjbu
081300051019  "  C*
081400051019  "  C                   CALL      'NDCH15R'
081500051019  "  C                   PARM                    Kpjba
081600051019  "  C*
081700051019  "  C                   MOVEL     Kpjbu         NDCH15DS
081800051019  "  C*
081900051019  "  C                   EXSR      RtnH15
082000051019  "  C*
082100051019  "  C                   ENDSR
082200051019  "  C/EJECT
082300051019  "  C************************************************************
082400051019  "  C* Riempie la DS di passaggio contabile addebito
082500051019  "  C************************************************************
082600051019  "  C     RieH15DS      BEGSR
082700051019  "  C*
082800051019  "  C                   RESET                   NDCh15DS
082900051019  "  c                   eval      opzH15    ='02'
083000051019  "  c                   eval      societaH15=societa252
083100051019  "  c                   eval      CtbH15    =Ctb252
083200051019  "  c                   eval      UnitaH15  =Unita252
083300051019  "  c                   eval      BancaH15  =Banca252
083400051019  "  c                   eval      LineaH15  =Linea252
083500051019  "  c                   eval      DtOpeH15  =DtOpe252
083600051019  "  c                   eval      DtValH15  =DtVal252
083700051019  "  c                   eval      DtRegH15  =DtReg252
083800051019  "  c                   eval      DtDocH15  =DtDoc252
083900051019  "  c                   eval      NrDocH15  =NrDoc252
084000051019  "  c                   eval      SerDocH15 =SerDoc252
084100051213c1875c                   eval      DivisaH15 =Divisa252
084200051213c1891x***                eval      ImportoH15=Importo252
084300051213  "  x***                eval      ImportDH15=ImportD252
084400051213  "  c                   eval      ImportoH15=Importo252 - Impspes252
084500051213c1891c                   eval      ImportDH15=ImportD252 - impSpes252
084600051213c1875c                   eval      SpuntaH15 =Spunta252
084700051019  "  c                   eval      SpeseMcH15=ImpSpes252
084800051019  "  C*
084900051019  "  C                   ENDSR
085000051019  "  C/EJECT
085100051019  "  C************************************************************
085200051019  "  C* Ritorno da contabile addebito
085300051019  "  C************************************************************
085400051019  "  C     RtnH15        BEGSR
085500051019  "  C*
085600051019  "  C* modo di ritorno
085700051019  "  C*
085800051019  "B1C                   SELECT
085900051019  "  C* questi modi di utilizzo dei valori di ritorno dal
086000051019  "  C* pgm di manutenzione rcd di anagrafica sono delle
086100051019  "  C* proposte, normalmente sempre valide, ma modificabili
086200051019  "  C* per situazioni particolari
086300051019  "  C* 1 = F3
086400051019  "  C     RETH15        WHENEQ    '1'
086500051019  "  C***                MOVE      '1'           RET252
086600051019  "  C* 2 = F12
086700051019  "  C     RETH15        WHENEQ    '2'
086800051019  "  C***                MOVE      '1'           RET252
086900051019  "  C*
087000051019  "E1C                   ENDSL
087100051019  "  C*
087200051019  "  C* operazione eseguita dal pgm chiamato
087300051019  "  C*
087400051019  "  C                   SELECT
087500051019  "  C* 1 = operazione eseguita
087600051019  "  C     OprH15        WHENEQ    '1'
087700051019  "  C                   EVAL      Opr252 = '1'
087800051019  "  C*
087900051019  "  C                   OTHER
088000051019  "  C                   EVAL      Opr252 = '0'
088100051019  "  C*
088200051019  "  C                   ENDSL
088300051019  "  C*
088400051019  "  C* funzione non eseguibile per errore :
088500051019  "  C*
088600051019  "  C                   SELECT
088700051019  "  C* 1 = funzione richiamata chiusa in errore
088800051019  "  C     ERRH15        WHENEQ    '1'
088900051019  "  C                   EVAL      Err252 = '1'
089000051019  "  C*
089100051019  "  C                   ENDSL
089200051019  "  C*
089300051019c1875C                   ENDSR
089400051019     c/EJECT
089500051019      ************************************************************
089600980303      * aggiorna archivio remote banking
089700980303      ************************************************************
089800980303     C     AggNdRi1      BEGSR
089900980303     c*
090000970611     C                   z-add     NrCar252      Ri1NrCar
090100970611     C                   z-add     ProgCar252    Ri1ProgCar
090200970611     c     K02Ri102      chain     NdRi102l                           2122
090300970611      *
090400970611      * Gestione RCD allocato
090500970611   B3C     *IN22         IFEQ      *ON
090600970611     C                   MOVEL     'NDRI100F'    MSGFIL
090700970611     C                   EXSR      MSGPGM
090800970611     c                   move      '1'           ERR252
090900970611     C                   exsr      endpgm
091000970611   E3C                   ENDIF
091100970611     c* Record non trovato
091200970611     c                   if        *in21 = *on
091300970611     c                   move      '1'           ERR252
091400970611     C                   exsr      endpgm
091500970611     c                   endif
091600970611     c*
091700970611     c* Aggiorna
091800040519c1758c* Per il momento non gestisco i valori di contabilizzazione in quanto
091900040519  "  c*  la contabilizzazione viene fatta in momenti successivi per CP
092000040519C1758c                   if        opz252 <> '04'
092100970611     c                   eval      Ri1Sysm = regsys
092200970611     c                   eval      Ri1Ncb  = regnrasreg
092300970611     c                   eval      Ri1Drg  = regdtreg
092400970611     c                   eval      Ri1Nrg  = regnrreg
092500060420C1924c                   eval      Ri1Sta='2'
092600040519C1758C                   else
092700040519  "  c                   Eval      Ri1Sysm = *zeros
092800040519  "  c                   Eval      Ri1Ncb  = $SubNum
092900040519  "  c     *dmy          Move      Udate         Ri1Drg
093000060420C1758c                   Eval      Ri1Nrg  = $SubNum
093100060420C1924c                   eval      Ri1Sta='2'
093200040519C1758c                   endif
093300040513     c*
093400060420C1924X*****              if        Spunta252 = *on
093500060420  "  X*****              eval      Ri1Sta = *on
093600060420C1924X*****              endif
093700060428D1987x***                eval      Ri1Div   = Regdivisa
093800060428D1987c                   eval      Ri1Div   = Divisa252
093900060628D2014c                   sub       SpeEst252     ImportD252
094000060628  "  c* Se moneta di conto, tolgo le spese
094100060628  "  c                   if        divisa252 =xscdiv
094200060628  "  c                   sub       ImpSpes252    ImportD252
094300060628D2014c                   endif
094400991213D0347C                   If        AggHB252  = *off
094500970620     C                   eval      Ri1Imp   = Importd252
094600991213D0347c                   endif
094700970620     C                   eval      Ri1Da    = Segno252
094800970611     c                   update    NdRi1000
094900970611     c*
095000970611     c                   endsr
095100970611     c/EJECT
095200970611      ************************************************************
095300970611      * aggiorna archivio movimenti banca
095400970611      ************************************************************
095500970611     C     AggNdBmb      BEGSR
095600970617     c*
095700970617     C                   z-add     SysN252       BmbSysn
095800970617     C                   z-add     Num252        BmbNum
095900970617     C                   z-add     NrRiga252     BmbNrRiga
096000970617     c     K03Bmb02      chain     NdBmb02l                           2122
096100970617      *
096200970617      * Gestione RCD allocato
096300970617   B3C     *IN22         IFEQ      *ON
096400970617     C                   MOVEL     'NDBMB00F'    MSGFIL
096500970617     C                   EXSR      MSGPGM
096600970617     c                   move      '1'           ERR252
096700970617     C                   exsr      endpgm
096800970617   E3C                   ENDIF
096900970617     c* Record non trovato
097000970617     c                   if        *in21 = *on
097100970617     c                   move      '1'           ERR252
097200970617     C                   exsr      endpgm
097300970617     c                   endif
097400970617     c*
097500970617     c* Aggiorna
097600970617     c                   eval      BmbSys  = regsys
097700970617     c                   eval      BmbNrasReg= regnrasreg
097800970617     c                   if        Spunta252 = *on
097900970617     c                   eval      BmbSpunta= *on
098000970617     c                   endif
098100970620     C                   eval      BmbDivisa= Regdivisa
098200991213D0347C                   If        AggHB252  = *off
098300970620     C                   eval      bmbimportd= Importd252
098400970620     C                   eval      bmbimporto= Importo252
098500991210D0347c                   endif
098600970620     C                   eval      bmbCambio = RegCambio
098700970620     c                   if        segno252 ='D'
098800970620     C                   eval      BmbDare  = 1
098900970620     C                   eval      BmbAvere = 0
099000970620     c                   else
099100970620     C                   eval      BmbDare  = 0
099200970620     C                   eval      BmbAvere = 1
099300970620     c                   endif
099400970617     c                   update    NdBmb000
099500970611     c*
099600970611     c                   endsr
099700970611     c/EJECT
099800970611      ************************************************************
099900970611      * INizializza variabili                                    *
100000970611      ************************************************************
100100970611     C     inzvAr        BEGSR
100200970611     c*
100300970611     c                   eval      Opr252 = '0'
100400970611     c                   eval      Err252 = '0'
100500970611     c*
100600040513C1758C* Operazioni in caso di CASH POOLING
100700040513  "  c                   if        Opz252 = '04'
100800040513  "  C* Reperimento dati società
100900051123C1758c                   if        societa252 <> xscsoc
101000051123D1900X*                  MOVEL     'SOC001'      TIPXSC
101100051123D1900C                   MOVEL     'SOC002'      TIPXSC
101200051123C1758C                   MOVEL     Societa252    SOCXSC
101300040513  "  C                   EXSR      REPSOC
101400040513  "  C     RTNXSC        IFNE      '1'
101500040513  "  C                   MOVEL     XSOCDS        SOC001
101600040513  "  C                   ELSE
101700040513  "  c                   Eval      Err252 = '3'
101800040513  "  C                   Exsr      EndPgm
101900040513  "  C                   ENDIF
102000040513  "  c                   ENDIF
102100040513  "  C*
102200040513  "  C* Preparo il riferimento interno
102300040513  "  c                   Move      knraz         SavRifInt
102400040513c1758c                   endif
102500040513     c*
102600970611     c                   endsr
102700970611     c/EJECT
102800970611      ************************************************************
102900970611      * operazioni iniziali                                      *
103000970611      ************************************************************
103100970611     C     *Inzsr        BEGSR
103200970611     c*
103300970611     C     *entry        PLIST
103400970611     C                   parm                    kpjba
103500970611     C                   parm                    ndc252ds
103600051122D1900C* SALVO società di collegamento
103700051122D1900c                   movel     Kpjbu         SavSocieta
103800970611     C*
103900970611     C* Reperimento dati società
104000970611     C*
104100051123D1900X**                 MOVEL     'SOC001'      TIPXSC
104200051123D1900C                   MOVEL     'SOC002'      TIPXSC
104300970611     C                   MOVEL     Societa252    SOCXSC
104400970611     C                   EXSR      REPSOC
104500970611     C     RTNXSC        IFNE      '1'
104600970611     C                   MOVEL     XSOCDS        SOC001
104700970611     C                   ELSE
104800970611     C                   RETURN
104900970611     C                   ENDIF
105000021014c1574C*
105100021014  "  C* Reperimento parametri gestione tesoreria
105200021014  "  C*
105300021014  "  C                   MOVEL     *BLANK        XPARIC
105400021014  "  C                   MOVEL     XSCSOC        XPASOC
105500021014  "  C                   MOVEL     'STRTESO '    XPAPAR
105600021014  "  C                   MOVEL     *BLANK        XPAOUT
105700021014  "  C                   EXSR      REPPAR
105800021014  "  C     XPAERR        IFEQ      '0'
105900021014  "  C                   MOVEL     XPAOUT        ANP021DS
106000021014  "  C                   ELSE
106100021014  "  C                   SETON                                        LR
106200021014  "  C                   RETURN
106300021014c1574C                   ENDIF
106400970611     C* Eseguo strcmtctl
106500970611     C                   CALLb     'XSTRCMT'
106600970611     C                   OPEN      NDBHM00f
106700030401c1642C                   OPEN      NDBHP00f
106800020902c1574x*                  if        opz252 = '01'
106900020902  "  x*                  open      NDRI102l
107000020902  "  x*                  else
107100020902  "  x*                  open      NDBMB02L
107200020902  "  x*                  endif
107300020902  "  c*
107400020902  "  c                   select
107500040519C1574c                   when      Opz252 = '01'
107600040519C1758c                             OR Opz252 = '04'
107700040519C1574c                   open      NDRI102l
107800020902  "  c*
107900020902  "  c                   when      Opz252 = '02'
108000020902  "  c                   open      NDBMB02L
108100020902c1574c                   endsl
108200970611     c*
108300970611     c                   endsr
108400970611     c/EJECT
108500970611      ************************************************************
108600970611      * DEFINIZIONE KLIST                                        *
108700970611      ************************************************************
108800970611     C     DefKlist      BEGSR
108900970611      *
109000970611      * klist
109100970611      *
109200970611      * ndri102L
109300970611     C     K02ri102      KLIST
109400970611     C                   KFLD                    Ri1NrCar
109500970611     C                   KFLD                    Ri1ProgCar
109600970617      * ndBMB02L
109700970617     C     K03BMB02      KLIST
109800970617     C                   KFLD                    BmbSysn
109900970617     C                   KFLD                    BmbNum
110000970617     C                   KFLD                    BmbNrRiga
110100970611     C*
110200970611     C                   ENDSR
110300970611     C/EJECT
110400970611     C************************************************************
110500970611     C* REPERIMENTO DATI SOCIETA'
110600970611     C************************************************************
110700970611     C     REPSOC        BEGSR
110800970611     C*
110900970611     C                   CALLB     'XSOC'
111000970611     C                   PARM                    TIPXSC            6
111100970611     C                   PARM                    SOCXSC            3
111200970611     C                   PARM                    CDSXSC            9 0
111300970611     C                   PARM                    MODXSC            3
111400970611     C                   PARM      *BLANKS       RTNXSC            1
111500970611     C                   PARM                    XSOCDS
111600970611     C                   PARM                    KPJBA
111700970611     C*
111800970611     C                   ENDSR
111900021014     C/EJECT
112000021014c1574C************************************************************
112100021014  "  C* REPERIMENTO PARAMETRI GESTIONE
112200021014c1574C************************************************************
112300021014     C     REPPAR        BEGSR
112400021014     C*
112500021014     C                   CALLB     'XPAR'
112600021014     C                   PARM                    XPASOC            3
112700021014     C                   PARM                    XPAPAR            8
112800021014     C                   PARM                    XPAOUT
112900021014     C                   PARM                    XPAERR            1
113000021014     C                   PARM                    XPARIC            1
113100021014     C*
113200021014c1574C                   ENDSR
113300970611     C/EJECT
113400051010c1866C************************************************************
113500051010  "  C* REPERIMENTO unità società cash pooling
113600051010c1866C************************************************************
113700051010     C     RepUniCp      BEGSR
113800051010     C*
113900051010     C                   MOVEL     *BLANK        XPARIC
114000051010     C                   MOVEL     SocCp252      XPASOC
114100051010     C                   MOVEL     'HBPARBAS'    XPAPAR
114200051010     C                   MOVEL     *BLANK        XPAOUT
114300051010     C                   CALLB     'XPAR'
114400051010     C                   PARM                    XPASOC            3
114500051010     C                   PARM                    XPAPAR            8
114600051010     C                   PARM                    XPAOUT
114700051010     C                   PARM                    XPAERR            1
114800051010     C                   PARM                    XPARIC            1
114900051010     C     XPAERR        IFEQ      '0'
115000051010     C                   MOVEL     XPAOUT        NdCk90ds
115100051010     c                   else
115200051010     c                   clear                   NdCk90DS
115300051010     c                   endif
115400051010     C*
115500051010c1574C                   ENDSR
115600051010     C/EJECT
115700970611      ******************************************************
115800970611      * PGM PER GESTIONE MESSAGGI ERRORE                   *
115900970611      ******************************************************
116000970611     C     MSGPGM        BEGSR
116100970611      *
116200970611     C                   Z-ADD     2             MSGMSG
116300970611     C                   MOVEL     'S'           MSGRSP
116400970611     C                   MOVEL     'CR'          MSGTPR
116500970611     C                   MOVEL     'S'           MSGEMV
116600970611     C                   Z-ADD     10            MSGRGP
116700970611     C                   MOVEL     'S'           MSGLCK
116800970611     C                   MOVEA     STATUS        STS
116900970611     C                   MOVEL     'N'           MSGCNL
117000970611     C                   MOVEL     'N'           MSGVID
117100970611     C                   MOVEL     'N'           MSGOPE
117200970611     C                   MOVEL     'N'           MSGSTP
117300970611     C                   CALL      'XMSGR'
117400970611     C                   PARM                    XMSGDS
117500970611      *
117600970611     C                   ENDSR
