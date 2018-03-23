000100170927>>>>>H DFTACTGRP(*NO) BNDDIR('PJXBND':'PJCBND') ACTGRP('QILE')
000200970102     H*PARMS CVTOPT(*NONE)
000300950608     H DECEDIT('0,') DATEDIT(*DMY.)
000400950831     F*---------------------------------------------------------------*
000500950831     F* Stampa mastrino per un conto                                  *
000600950831     F*---------------------------------------------------------------*
000700950619    >FANOPE01L  IF   E           K DISK
000800951103    >FNDDES01L  IF   E           K DISK
000900950615    >FNDMOV05L  IF   E           K DISK    rename(ndmov000:ndmov05)
001000950615    >FNDMOV06L  IF   E           K DISK    rename(ndmov000:ndmov06)
001100960108    >FNDREG01L  IF   E           K DISK
001200950615    >FNDC132P   O    E             PRINTER OFLIND(*IN39)
001201170927>>>>>F                                     INFDS(yPrtInf)
001300950615     F*---------------------------------------------------------------
001400990421     D*-------------
001500900731     D KPJBA         E DS
001600950605     D*-------------
001700950130     D* Reperimento nome PGM
001800950130     D STATUS         SDS           333
001900950130     D  DSPGM            *PROC
001901170927>>>>>D  psJobName            244    253                                         Nome lavoro
001902170927>>>>>D  psJobUser            254    263                                         Utente
001903170927>>>>>D  psJobNbr             264    269                                         Numero lavoro
002000950605     D*-------------
002100950615     D* Parametri ricevuti dal PGM
002200950615     D C132DS        E DS                  EXTNAME(NDC132DS)
002300950405     D*-------------
002400941103     D* Dati per pgm XSOC
002500941103     D SOC001        E DS                  EXTNAME(XSOC001DS)
002600961009     D    XscDare                     1    overlay(xscFgo:7)                    Scadenz. retroattivo
002700961009     D    XscAvere                    1    overlay(xscFgo:8)                    Scadenz. retroattivo
002800941103     D*-------------
002900950605     D* DS Interna per dati utente
003000941103     D XSOCDS          DS          1000
003100950615     D*-------------
003200950615     D* Controllo conti
003300990421     D ANRCO         E DS                  EXTNAME(ANRCO00F)
003400950621     D ND002DS       E DS
003500950619     D* Definizioni parametri standard chiamata modulo controllo conti
003600950619     D$Esito           S              1
003700950619     D$gesmsg          S              1
003800950619     D$lenout          S              9B 0
003900950619     D$struttura       S             10
004000950621     D*-------------
004100950621     D* DS esterna per dati soggetti
004200950621     D DVASOG        E DS                  INZ
004300950621     D*-------------
004400950621     D* Definizioni parametri driver soggetti
004500950621     DDvaric           S              1
004600950621     DDvaSocieta       S              3
004700950621     DDvaUnita         S              8
004800950621     DDvaStrutt        S             10
004900950621     DDvaDtRif         S               D
005000950621     DDvaLenOut        S              9B 0
005100950621     DDvaSnatura       S              1
005200950621     DDvaCodice        S              8
005300950621     DDvaLineav        S              3
005400950621     DDvaFiliale       S              3  0
005500950621     DDvaSogg          S              8
005600950621     DDvaTpInd         S              2
005700950621     DDvaCdInd         S              3
005800950621     DDvaErrore        S              1
005900950621     DDvaOutput        S           4000
006000950621     D*-------------
006100990421     D* Ds per output variazioni estremi fiscali
006200990421     D ANCPI         E DS                  EXTNAME(ANCPI00F)
006300990421     D*-------------
006400990421     D* Definizioni parametri driver estremi fiscali
006500990421     D dvcRic          S              1
006600990421     D dvcSocieta      S              3
006700990421     D dvcUnita        S              8
006800990421     D dvcStrutt       S             10
006900990421     D dvcDtRif        S               D
007000990421     D dvcLenOut       S              9B 0
007100990421     D dvcSogg         S              8
007200990421     D dvcErrore       S              1
007300990421     D dvcOutput       S           4000
007400990421     D*-------------
007500950621     D* DS esterna per dati fornitori
007600950621     D DVAFOR        E DS                  INZ
007700950606     D*-------------
007800950616     D* Controllo saldi
007900960104     D X26SALDS      E DS
008000980323     D X26SalDs1     E DS                        Occurs(30)
008100950616     D*-------------
008200950616     D* Controllo date rispetto all'esercizio
008300950616     D X10ESEDS      E DS
008400961121     D ANOPE         E DS                  EXTNAME(ANOPE00F)
008500950619     D*-------------
008600950619     D* campi di lavoro
008700950619     D wdate           S               D
008800991220D0350D wdateLoval      S               D   Inz(*Loval)
008900961121     D Causale         S                   like(opecausale)
009000961121     D Uso             S              1
009100961121     D Esito           S              1
009200961121     D GesMsg          S              1
009300961121     D LenOut          S              9B 0
009400961121     D Refresh         S              1
009500961121     D StrutturaO      S             10
009600981221     D $DataDoc        S                    like(DataDoc)
009700981221     D $NrDoc          S                    like(MovNrDoc)
009800981221     D $SerieDoc       S                    like(MovserDoc)
009900000118C1309D ZDateISO        S               D
010000000914D0703 *-----------------------------------------------------
010100000914  "   *Inizializza che per il conto non c'è alcun movimento
010200000914  "   *(se richiesta stampa solo movimentati e il conto
010300000914  "   *non ha movimenti, non stampo neanche le variazioni fiscali)
010400000914D0703D $ExistMov       S              1    Inz('0')
010500070613D2217D $RegOk          S              1    Inz('1')
010501170927>>>>>
010502170927>>>>>D yPrtInf         DS                  QUALIFIED
010503170927>>>>>D  spoolNum             160    163I 0                                      6 digit Spool Nbr
010504170927>>>>> /COPY GAITRASRC/SRCPROTOPR,TRBMAF05R
010505170927>>>>>
010600950405     C*----------------------------------------------------*
010700950405     C*                MAIN LINE PROGRAM
010800950405     C*----------------------------------------------------*
010900980612     C*
011000950606     C* Inizializzazione variabili
011100950606     C                   EXSR      INZVAR
011200980612     C*
011300980612     C* fine elaborazione ?
011400960112   1 C                   if        fin132=*off
011500950615     C* Lettura NDMOV
011600950615     C                   EXSR      LEGMOV
011700990423     C* Stampa eventuali variazioni dati fiscali del conto
011800990422     C                   ExSr      VarDatFis
011900960112   1xC                   else
012000950615     C* stampa totali finali e legenda causali
012100950619     C                   EXSR      STATOT
012200960112   1-C                   endif
012201170927>>>>>** Creo il file.spl per DocFlow.
012202170927>>>>>C                   IF        SendToDocFlow() < *ZERO
012205170927>>>>>C                   EXSR      EndPgm
012206170927>>>>>C                   ENDIF
012300900731     C*
012400950606     C* Fine programma
012500950606     C                   EXSR      ENDPGM
012600950606     C*
012700950606     C/EJECT
012800950606     C************************************************************
012900950606     C* FINE PROGRAMMA
013000950606     C************************************************************
013100950606     C     ENDPGM        BEGSR
013200950606     C*
013300960112     c                   if            lr132 = *on  or fin132 = *on
013400960112     C                   SETON                                        LR
013500950619     C                   else
013600960112     C                   SETON                                        RT
013700950619     C                   endif
013800960112     C*
013900950606     C                   RETURN
014000950606     C*
014100950606     C                   ENDSR
014200950606     C/EJECT
014300950405     C************************************************************
014400900731     C* Definizioni e inizializzazioni
014500950405     C************************************************************
014600940831     C     *INZSR        BEGSR
014700000118C1309C     *DMY          Move      Udate         ZDateISO
014800000118C1309C     *JobRun       Move      ZDateISO      ZDate             6 0
014900900731     C*
015000900731     C     *ENTRY        PLIST                                                  riceve
015100900731     C                   PARM                    KPJBA                          parametro
015200950616     C* definizione campi
015300950616     C                   move      *off          $states           1
015400950622     C                   move      *off          $statot           1
015500130625>>>>>C                   seton                                        39
015600950615     C* definizione campi x chiavi
015700950615     C     *like         define    movsocieta    soc
015800950615     C     *like         define    movctb        ctb
015900950615     C     *like         define    movkcc        kcc
016000950615     C     *like         define    movksc        ksc
016100950615     C     *like         define    movdtreg      dtreg
016200950615     C     *like         define    movdtlgio     dtlgio
016300950616     C     *like         define    movimporto    dare
016400950616     C     *like         define    movimporto    avere
016500950622     C     *like         define    movimporto    darein
016600950622     C     *like         define    movimporto    averein
016700950619     C     *like         define    movimporto    dareme
016800950619     C     *like         define    movimporto    avereme
016900950622     C     *like         define    movimporto    darepr
017000950622     C     *like         define    movimporto    averepr
017100950622     C     *like         define    movimporto    daretoin
017200950622     C     *like         define    movimporto    averetoin
017300950622     C     *like         define    movimporto    daretopr
017400950622     C     *like         define    movimporto    averetopr
017500950619     C     *like         define    movimporto    saldo
017600950616     C     *like         define    movimporto    saldodare
017700950616     C     *like         define    movimporto    saldoavere
017800960723     C     *like         define    movimporto    salprog
017900970715     C     *like         define    movimporto    wrkdare
018000970715     C     *like         define    movimporto    wrkavere
018100980619     C*
018200950615     C* KLIST PER NDMOV05L
018300980619     C     K5MOV05       KLIST
018400950615     C                   KFLD                    SOC
018500950615     C                   KFLD                    CTB
018600950615     C                   KFLD                    KCC
018700950615     C                   KFLD                    KSC
018800950615     C                   KFLD                    DTREG
018900950615     C* KLIST PER NDMOV06L
019000950616     C     K5MOV06       KLIST
019100950616     C                   KFLD                    SOC
019200950616     C                   KFLD                    CTB
019300950616     C                   KFLD                    KCC
019400950616     C                   KFLD                    KSC
019500950616     C                   KFLD                    DTLGIO
019600950615     C* KLIST PER NDMOV
019700950615     C     K4MOV         KLIST
019800950615     C                   KFLD                    SOC
019900950615     C                   KFLD                    CTB
020000950615     C                   KFLD                    KCC
020100950615     C                   KFLD                    KSC
020200951103     C*
020300951103     C* KLIST PER NDDES
020400951103     C     K04Des01      KLIST
020500951103     C                   KFLD                    DESSYS
020600951103     C                   KFLD                    DESNRASREG
020700951103     C                   KFLD                    DESTipDes
020800951103     C                   KFLD                    DESNRRigaM
020900960108     C*
021000960108     C* KLIST PER NDREG
021100960108     C     K2reg1        KLIST
021200960108     C                   KFLD                    regSYS
021300960108     C                   KFLD                    regNRASREG
021400950621     C*
021500950621     C* PLIST per NDVASOG
021600950621     C*
021700950621     C     PDVASOG       PLIST
021800950621     C                   PARM                    DVARIC
021900950621     C                   PARM                    DVASOCIETA
022000950621     C                   PARM                    DVAUNITA
022100950621     C                   PARM                    DVASTRUTT
022200950621     C                   PARM                    DVADTRIF
022300950621     C                   PARM                    DVALENOUT
022400950621     C                   PARM                    *omit
022500950621     C                   PARM                    *omit
022600950621     C                   PARM                    *omit
022700950621     C                   PARM                    *omit
022800950621     C                   PARM                    DVSSOGG
022900950621     C                   PARM                    DVSTPIND
023000950621     C                   PARM                    DVSCDIND
023100950621     C                   PARM                    DVAERRORE
023200950621     C                   PARM                    DVAOUTPUT
023300990421     C*
023400990421     C* PLIST per NDDVACPI
023500990421     C*
023600990421     C     PDVACPI       PLIST
023700990421     C                   PARM                    DVCRIC
023800990421     C                   PARM                    DVCSOCIETA
023900990421     C                   PARM                    DVCUNITA
024000990421     C                   PARM                    DVCSTRUTT
024100990421     C                   PARM                    DVCDTRIF
024200990421     C                   PARM                    DVCLENOUT
024300990421     C                   PARM                    DVCSOGG
024400990421     C                   PARM                    DVCERRORE
024500990421     C                   PARM                    DVCOUTPUT
024600950615     C*
024700940831     C                   ENDSR
024800950606      /EJECT
024900950606     C************************************************************
025000950606     C* INIZIALIZZAZIONE VARIABILI
025100950606     C************************************************************
025200950606     C     INZVAR        BEGSR
025300950606     C*
025400950615     C                   MOVEL     KPJBU         C132DS
025500130625>>>>>C* Imposto valore assunto.
025600130625>>>>>C                   IF        Ycd132 = *BLANK
025700130625>>>>>C                   EVAL      Ycd132 = *OFF
025800130625>>>>>C                   ENDIF
025900950606     C*
026000950606     C* Reperimento azienda
026100950615     C                   MOVE      SOC132        SOCXSC
026200950606     C                   MOVEL     'SOC001'      TIPXSC
026300950606     C                   EXSR      REPSOC
026400950606     C     RTNXSC        IFNE      '1'
026500950606     C                   MOVEL     XSOCDS        SOC001
026600950606     C                   ELSE
026700950606     C                   EXSR      ENDPGM
026800950606     C                   ENDIF
026900950616     C*
027000950616     C* Inizializzazione KLIST lettura NDMOV
027100950616     C                   movel     soc132        soc
027200950616     C                   movel     ctb132        ctb
027300950616     C                   movel     kcc132        kcc
027400950616     C                   movel     ksc132        ksc
027500950615     C*
027600950619     C                   move      *all'-'       t2tra1
027700950627     C* Valorizzazione campi univoci testate
027800950704     C*  reperisco nome sistema informatico
027900960104     C                   CALL      'XNETA'                              11
028000950704     C                   PARM                    NOMSYS
028100950627     C                   MOVEL     KNSIF         NOMSIF
028200950627     C                   MOVEL     XSCDSI        NOMDIT
028300950627     C                   MOVEL     DSPGM         NOMPGM
028400950615     C*
028500950616     C* Stampa testata conto, preparazione dati
028600950615     C                   movel     kcc132        t2kcc
028700950615     C                   movel     ksc132        t2ksc
028800950615     C                   clear                   t2rag
028900990422     C                   clear                   t2rag2
029000950616     C*
029100950615     C* Richiamo routine per controllare conto
029200950621     C                   CLEAR                   ANRCO
029300950615     C     *dmy          move      udate         wdate
029400950621     C                   EVAL      $LenOut = %Size(ANRCO)
029500950615     C                   CallB     'NDMVC002'
029600950615     C                   PARM                    xscsoc
029700950615     C                   PARM                    t2kcc
029800950615     C                   PARM                    t2ksc
029900040308D0424x***                PARM                    Wdate
030000000201D0424C                   PARM                    WDateLoval
030100950619     C                   PARM      *OFF          $GesMsg
030200950619     C                   PARM      *OFF          $Esito
030300950621     C* con ANRCO ....
030400950621     C                   PARM      'ANRCO'       $Struttura
030500950621     C                   PARM                    ANRCO
030600950615     C                   PARM                    $LenOut
030700950615     C*
030800950616   1 C     $Esito        ifeq      *ON
030900950622     C                   CLEAR                   ANRCO
031000950616   1-C                   endif
031100950616     C*
031200970130   1 C     dti132        ifne      *loval
031300040308C1309x******dmy          move      dti132        t2dti
031400000118C1309C     *JobRun       move      dti132        t2dti
031500950627     C                   else
031600950627     C                   move      *zeros        t2dti
031700970130   1-C                   endif
031800980612     C     dtf132        ifne      *hival
031900040308C1309x******dmy          move      dtf132        t2dtf
032000000118C1309C     *JobRun       move      dtf132        t2dtf
032100950627     C                   else
032200980612     C                   move      *zeros        t2dtf
032300950627     C                   endif
032400990421     C*
032500030325D1309X* se rcosnatura C o F reperire altri dati anagrafici soggetto
032600030804D1390x*                  if        rcosnatura='C' or rcosnatura='F'
032700030804D1416C                   if        rcosnatura='C' or rcosnatura='F'
032800040308D1309C* Se conto gestito a soggetti
032900030325D1309c                   if        rcokccsogg = *on
033000990421     C* ... dati da variazioni estremi fiscali
033100990421     C                   move      '2'           DVCRIC
033200990421     C                   move      soc132        DVCSOCIETA
033300990421     C                   move      uni132        DVCUNITA
033400990421     C                   movel     'ANCPI'       DVCSTRUTT
033500990421     C                   move      dtf132        DVCDTRIF
033600990421     C                   eval      DVCLENOUT = %Size(ANCPI)
033700990421     C                   move      rcosogg       DVCSOGG
033800990421     C                   CALLB     'NDDVACPI'    PDVACPI
033900990421     C     dvcerrore     ifeq      *off
034000990421     C                   movel     dvcoutput     ancpi
034100990421     C                   movel     cpidessog1    t2rag
034200990422     C                   movel     cpidessog2    t2rag2
034300990421     C                   movel     cpiindriz     t2via
034400990421     C                   movel     cpilocalit    t2cit
034500990421     C* ... dati da anagrafica soggetti
034600990421     C                   else
034700950621     C                   MOVE      '1'           DVARIC
034800950621     C                   MOVE      soc132        DVASOCIETA
034900960313     C                   MOVE      uni132        DVAUnita
035000950621     C                   MOVE      rcosogg       DVssogg
035100950622     C                   MOVE      *blank        DVstpind
035200950622     C                   MOVE      *blank        DVscdind
035300950621     C                   MOVEL     'DVASOG'      DVastrutt
035400950621     C                   EVAL      DVALenOut = %Size(DVASOG)
035500950621     C                   CALLB     'NDDVASOG'    PDVASOG
035600950621   2 C                   if        DVaerrore<>*ON
035700950621     C                   eval      %subst(DVASOG:1:DVALENOUT)
035800950621     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
035900950622     C                   eval      t2rag=dvsdes
036000990422     C                   eval      t2rag2=dvsdesagg
036100950622     C                   eval      t2via=dvsindriz
036200950622     C                   eval      t2cit=dvslocalit
036300950621   2xC                   else
036400950621     C                   clear                   c132t21
036500950621   2-C                   endif
036600990421   2-C                   endif
036700030918D1447c                   endif
036800990421     C* descrizione se non cliente/fornitore
036900950627   1xC                   else
037000950627     C                   Clear                   ND002DS
037100950627     C                   EVAL      $LenOut = %Size(ND002DS)
037200950627     C                   CallB     'NDMVC002'
037300950627     C                   PARM                    xscsoc
037400950627     C                   PARM                    t2kcc
037500950627     C                   PARM                    t2ksc
037600991220D0350 *se il conto è annullato alla data Wdate, la descrizione
037700991220  "   *del conto stesse non viene stampata
037800991220  "  C****************** PARM                    Wdate
037900991220D0350C                   PARM                    WDateLoval
038000950627     C                   PARM      *OFF          $GesMsg
038100950627     C                   PARM      *OFF          $Esito
038200950627     C                   PARM      'ND002DS'     $Struttura
038300950627     C                   PARM                    ND002DS
038400950627     C                   PARM                    $LenOut
0385009904213  2 C     $Esito        ifne      *ON
038600990422     C                   movel     descr002      t2rag
038700990422     C                   movel     desagg002     t2rag2
0388009506273- 2-C                   endif
038900950622   1-C                   endif
039000030918D1447x*** ex d1416       endif
039100990421     C*
039200950622     C* inizializzazione campi dare/avere di lavoro del mastrino
039300950622     C                   clear                   darein
039400950622     C                   clear                   averein
039500950622     C                   clear                   dareme
039600950622     C                   clear                   avereme
039700950622     C                   clear                   darepr
039800950622     C                   clear                   averepr
039900950623     C                   clear                   saldo
040000960723     C                   clear                   salprog
040100950622     C* resetta tutti i campi utilizzati per ogni conto
040200950622     C                   clear                   periodo
040300950622     C                   clear                   meseanno
040400000914D0703 *Inizializzo che per il conto non esiste alcun movimento
040500000914  "   *(se richiesta stampa solo movimentati e il conto
040600000914  "   *non ha movimenti, non stampo neanche le variazioni fiscali)
040700000914  "  C                   Eval      $ExistMov   = *Off
040800000914D0703 *
040900950616     C* saldi iniziali ; reperimento data apertura esercizio
041000951103   1 C     dti132        ifne      *loval
041100950616     C                   CLEAR                   X10ESEDS
041200950616     C                   EVAL      X10Societa = soc132
041300950616     C                   EVAL      X10Ctb = ctb132
041400950616     C                   EVAL      X10Data = dti132
041500950616     C                   CALL      'X10ESER'
041600950616     C                   PARM                    X10EseDS
041700951103     C                   else
041800951103     C                   move      *loval        x10dtines
041900970130   1-C                   endif
042000980605     C* saldi iniziali; un richiamo per ogni tipo registrazione
042100970130   1 C     dti132        ifne      *loval
042200960104     C                   clear                   x26salds
042300960104     C                   movel     soc132        x26societa
042400960104     C                   movel     uni132        x26unita
042500960104     C                   movel     ctb132        x26ctb
042600960104     C                   movel     kcc132        x26kcc
042700960104     C                   movel     ksc132        x26ksc
042800960104     C                   movel     'R'           x26tipdat
042900970213     C                   move      dti132        x26dtinpe
043000970213     C                   move      dti132        x26dtfipe
043100970130     C*
043200970130     C                   MOVE      def132        x26flgdef
043300970130     C                   MOVE      pro132        x26flgpro
043400970130     C                   MOVE      ges132        x26flgges
043500950616     C*
043600950622     C* RCONATURA se 1,2,5=S se 3,4=N
043700970130   3 C                   if        rconatura='1' or
043800950622     C                             rconatura='2' or
043900950622     C                             rconatura='5'
044000960104     C                   MOVE      *on           x26flgape
044100970130   3-C                   endif
044200970130   3 C                   if        rconatura='3' or
044300950622     C                             rconatura='4'
044400960104     C                   MOVE      *off          x26flgape
044500970130   3-C                   endif
044600960104     C                   move      *on           x26flgpre
044700970130     C                   move      *off          x26flgper
044800950616     C*
044900960104     C                   CALL      'X26SALR'
045000960104     C                   parm                    x26salds
045100980323     C                   parm                    x26salds1
045200950616     C* i saldi....
045300970130   3 C                   if        x26esito<>*on
045400970130     C                   z-add     x26predarD    darein
045500970130     C                   add       x26predarP    darein
045600970130     C                   add       x26predarG    darein
045700970130     C                   z-add     x26preaveD    averein
045800970130     C                   add       x26preaveP    averein
045900970130     C                   add       x26preaveG    averein
046000970130     c*
046100990423   4 c                   if        x10bilap <> *on or
046200990423     c                             x10bilap = *on  and x10dtregap <dti132 or
046300990423     c                             x10bilap = *on  and x10dtregap >dtf132
046400970130     C                   add       x26apedarD    darein
046500970130     C                   add       x26apedarP    darein
046600970130     C                   add       x26apedarG    darein
046700970130     C                   add       x26apeaveD    averein
046800970130     C                   add       x26apeaveP    averein
046900970130     C                   add       x26apeaveG    averein
047000950616   4-C                   endif
047100970130   3-c                   endif
047200950616     C*
047300970130   2-C                   endif
047400950616     C*
047500950622     C* valorizza i totali saldi iniziali e li stampa al momento
047600950622     C* della stampa testata
047700950623   1 C                   if        darein>=averein
047800950622     C                   eval      saldodare=darein-averein
047900950622     C                   eval      saldoavere=0
048000950622     C                   else
048100950622     C                   eval      saldoavere=averein-darein
048200950622     C                   eval      saldodare=0
048300950622   1-C                   endif
048400950622     C* i valori iniziali di tutti i mastrini
048500950622     C                   eval      daretoin=darein+daretoin
048600950622     C                   eval      averetoin=averein+averetoin
048700950623     C* segnala testata da stampare
048800950623     C                   move      *on           $states
048900970110     C* accendo indicatore 50 per inizializzare il nr. pagina
049000970110     C                   move      *on           *in50
049100130625>>>>>C* Se è una stampa per il CD
049200130625>>>>>C* il numero pagina parte da 1 per ogni conto.
049300130625>>>>>C                   IF        Ycd132 = *ON
049400130625>>>>>C                   CLEAR                   ypagnbr
049500130625>>>>>C                   ENDIF
049600950622     C*
049700000914D0703X*******************ENDSR
049800000914D0703C     XInzVar       ENDSR
049900950606      ************************************************************
050000950615      * Lettura NDMOV
050100950606      ************************************************************
050200950615     C     LEGMOV        BEGSR
050300950616     C* se anche conti non movimentati, stampo testata
050400960112   1 C                   if        cnm132=*on
050500950616     C                   EXSR      STATES
050600950616   1xC                   else
050700950616     C                   move      *on           $states
050800950616   1-C                   endif
050900950621     C* reimposta le date chiave
051000980619     C                   movel     *loval        dtlgio
051100950616     C                   movel     dti132        dtreg
051200950621     C* posizionamento iniziale
051300950615   1 C                   if        seo132='R'
051400980619     C     K5MOV05       SETLL     NDMOV05L
051500950615   1xC                   else
051600980619     C     K5MOV06       SETLL     NDMOV06L
051700950615   1-C                   endif
051800950615     C*
051900950622     C                   move      *off          *in21
052000960104   1 C                   do        *HIVAL
052100950615     C* lettura per società/contabilità/conto
052200950615   2 C                   if        seo132='R'
052300950616     C     K4MOV         READE     NDMOV05L                               21
052400950615   2xC                   else
052500950615     C     K4MOV         READE     NDMOV06L                               21
052600950615   2-C                   endif
052700960104     C                   if        *in21=*on     or
052800980612     C* se ord. per dt reg. ed è > limite superiore --> fine lettura
052900980605   4 C                             seo132='R' and movdtreg>dtf132
053000960104     C                   leave
053100960104     C                   endif
053200981221     C*
053300981221     C                   if        slp132=*on
053400981221     C* se richiesta stampa saldi progressivi accendo l'indicatore per
053500981221     C* stamparli
053600981221     C                   SetOn                                        46
053700981221     C                   Else
053800981221     C* se non richiesta stampo gli estremi del documento
053900981221     C                   SetOff                                       46
054000981221     C                   if        MovDtDoc <> *LoVal
054100000118C1309C******dmy          Move      MovDtDoc      $DataDoc
054200000118C1309C     *JobRun       Move      MovDtDoc      $DataDoc
054300981221     C                   Else
054400981221     C                   z-add     0             $DataDoc
054500981221     C                   EndIf
054600981221     C                   MoveL     MovNrDoc      $NrDoc
054700981221     C                   MoveL     MovSerDoc     $SerieDoc
054800981221     C                   EndIf
054900070613D2217 * reperimento ndreg
055000070613  "  C                   exsr      RepNDREG
055100070613D2217 *
055200980612     C* se ord. per dt libro gior. e dt reg. > limite sup. --> scarto rcd
055300980605     C                   if        seo132 = 'G' and
055400980605     C                             movdtreg >= dti132 and
055500980619     C                             movdtreg <= dtf132 or
055600980612     C                             seo132 = 'R'
055700960104     C* se unità coerente
055800960104   2 C                   if        uni132=*blank or
055900960104     C                             uni132<>*blank and
056000980622     C                             uni132=movunita
056100950831     C* se tipo registrazione = a quello/i richiesto/i
056200960112   3 C                   if        def132=*on   and movtpregz='D' or
056300960112     C                             pro132=*on   and movtpregz='P' or
056400960112     C                             ges132=*on   and movtpregz='G'
056500070613D2217 * se registrazione esiste
056600070613D2217C                   if        $regOk=*on
056700950622     C* se solo_conti_movimentati stampo testata alla 1° movimentazione
056800960112   5 C                   if        cnm132=*off   and $states=*ON
056900950616     C                   EXSR      STATES
057000950616   5-C                   endif
057100950622     C* c'è almeno un movimento stampabile
057200950622     C                   move      *on           $statot
057300981218     C                   clear                   RigP1
057400981221     C* se devo stampare i dati del documento li imposto
057500981221     C                   If        slp132 = *Off
057600981221     C                   z-add     $DataDoc      DataDoc
057700981221     C                   MoveL     $NrDoc        NrDoc
057800981221     C                   MoveL     $SerieDoc     SerieDoc
057900981221     C                   Else
058000981221     C                   SetOn                                        46
058100981221     C                   EndIf
058200950616     C* comunque calcola importi
058300950616     C     movimporto    mult      movdare       dare
058400950616     C     movimporto    mult      movavere      avere
058500950619     C* alimenta i totali per periodo
058600950619     C                   add       dare          darepr
058700950619     c                   add       avere         averepr
058800950619     C* alimenta i totali per mese
058900960112   5 C                   if        slm132=*on
059000950831   6 C                   if        seo132='R'
059100950619     C* estrae mese/anno da dtreg
059200950619     C                   extrct    movdtreg:*y   anno              4
059300950619     C                   extrct    movdtreg:*m   mese              2
059400950619   6xC                   else
059500950619     C* estrae mese/anno da dtlgio
059600950619     C                   extrct    movdtlgio:*y  anno
059700950619     C                   extrct    movdtlgio:*m  mese
059800950619   6-C                   endif
059900950619     C                   clear                   meseanno          7
060000950619     C                   eval      meseanno=mese+'/'+anno
060100950622     C* stampa totali di periodo
060200950619   6 C                   if        meseanno<>periodo
060300950619   7 C                   if        periodo<>*blank
060400950622     C* ... se presenti
060500950622   8 C                   if        dareme>0 or avereme>0
060600981218     C                   clear                   ValoriTot
060700970721     c                   setoff                                       414243
060800970721     c                   setoff                                       4445
060900970721     c                   seton                                        42
061000970721     C*****              movel     totmes        rtdestot
061100950621     C                   move      periodo       rtdestot
061200960723     C* saldo progressivo
061300960723     C                   if        slp132=*on
061400960723     C                   eval      salprog = salprog + dareme - avereme
061500960723     C                   endif
061600960723     C                   eval      rtsalprog=salprog
061700960723     C                   if        salprog>0
061800961009     C                   move      XscDare       rtdaprog
061900960723     C                   else
062000960723     C                   if        salprog<0
062100961009     C                   move      XscAvere      rtdaprog
062200960723     C                   else
062300960723     C                   move      *blank        rtdaprog
062400960723     C                   endif
062500960723   2-C                   endif
062600950619     C* i valori
062700950619     C                   eval      rtdare=dareme
062800950619     C                   eval      rtavere=avereme
062900981218     C                   write     ValoriTot
063000981218     C                   write     RigaVuota
063100950622   8-C                   endif
063200950619     C                   move      meseanno      periodo
063300950619     C                   eval      dareme=0
063400950619     C                   eval      avereme=0
063500950619   7xC                   else
063600950619     C                   move      meseanno      periodo           7
063700950619   7-C                   endif
063800950619   6-C                   endif
063900950619     C*
064000950619     C                   add       dare          dareme
064100950622     C                   add       avere         avereme
064200950619   5-C                   endif
064300950622     C*
064400950616     C* richiesti saldi progressivi
064500960112   5 C                   if        slp132=*on
064600970715     c                   z-add     darepr        wrkdare
064700970715     c                   z-add     averepr       wrkavere
064800970715     c                   add       darein        wrkdare
064900970715     c                   add       averein       wrkavere
065000970715     C                   eval      saldo=(wrkdare-wrkavere)
065100960110     C                   eval      rdsalprog=saldo
065200970715   6 C                   if        wrkdare>wrkavere
065300961009     C                   eval      rddaprog= XscDare
065400950619   6xC                   else
065500970715   6 C                   if        wrkdare<wrkavere
065600961009     C                   eval      rddaprog= XscAvere
065700960110     C                   else
065800960110     C                   eval      rddaprog=' '
065900950619   6-C                   endif
066000950619   5-C                   endif
066100960110   5-C                   endif
066200950622     C*
066300950615     C* se richiesto dettaglio ....
066400960112   5 C                   if        det132=*on
066500960110     C*
066600960110     C                   if        movtpregz <> 'D'
066700950615     C                   move      movtpregz     rdtpre
066800960110     C                   else
066900960110     C                   move      *blank        rdtpre
067000960110     C                   endif
067100960110     C*
067200000118C1309C******dmy          move      movdtreg      rddtre
067300000118C1309C     *JobRun       move      movdtreg      rddtre
067400980622     C* se data reg. <> data libro giornale --> highlight data reg.
067500980622     C                   if        movdtreg <> movdtlgio
067600980622     C                   seton                                        51
067700980622     C                   else
067800980622     C                   setoff                                       51
067900980622     C                   endif
068000960108     C*
068100070613D2217X****************** z-add     movsys        regsys
068200070613  "  X****************** z-add     movnrasreg    regnrasreg
068300070613  "  X**** k2reg1 ****** chain     ndreg01l                           22
068400070613D2217X**** *in22 ******* ifeq      *off
068500070613D2217 * se registrazione esiste
068600070613D2217C                   if        $regOk=*on
068700960108     C                   z-add     regnrreg      rdnreg
068800960108     C                   else
068900960108     C                   z-add     0             rdnreg
069000960108     C                   endif
069100950831     C*
069200960108     C                   movel     movserreg     rdserreg
069300960108     C                   movel     movcausrig    rdcaus
069400961121     C*
069500961121     C                   clear                   rddesbre
069600961121     C     movdesbrev    ifeq      *blanks
069700961121     C                   eval      wdate         = movdtreg
069800961121     C                   eval      causale       = movcausrig
069900961121     C                   exsr      callmvc003
070000961121     C     Esito         ifeq      *off
070100961121     C                   movel     opedesbrev    rddesbre
070200961121     C                   endif
070300961121     C                   else
070400950619     C                   movel     movdesbrev    rddesbre
070500961121     C                   endif
070600960110     C*
070700960912     C                   clear                   rdccpu
070800960912     C                   clear                   rdscpu
070900960912     C                   clear                   rddcpu
071000960110     C                   if        movkcccpu<>*blank
071100960912     C                   move      movkcccpu     rdccpu
071200960912     C                   move      movksccpu     rdscpu
071300960110     C                   else
071400960110     C                   if        *in22=*off
071500960110     C                   if        movdare=1
071600960912     C                   move      regkcccpa     rdccpu
071700960912     C                   move      regksccpa     rdscpu
071800960110     C                   else
071900960110     C                   if        movavere=1
072000960912     C                   move      regkcccpd     rdccpu
072100960912     C                   move      regksccpd     rdscpu
072200960110     C                   endif
072300960110     C                   endif
072400960110     C                   endif
072500960110     C                   endif
072600960110     C*
072700960110     C                   if        rdccpu<>*blank
072800950616     C* Richiamo routine per controllare conto
072900950616     C                   CLEAR                   ND002DS
073000950616     C     *dmy          move      udate         wdate
073100950616     C                   EVAL      $LenOut = %Size(ND002DS)
073200950616     C                   CallB     'NDMVC002'
073300950616     C                   PARM                    xscsoc
073400950616     C                   PARM                    rdccpu
073500950616     C                   PARM                    rdscpu
073600991221D0350 *se il conto è annullato alla data Wdate, la descrizione
073700991221  "   *del conto stesse non viene stampata
073800991221  "  C****************** PARM                    Wdate
073900991221D0350C                   PARM                    WDateLoval
074000950616     C                   PARM      *OFF          $GesMsg
074100950616     C                   PARM      *OFF          $Esito
074200950616     C                   PARM      'ND002DS'     $Struttura
074300950616     C                   PARM                    ND002DS
074400950616     C                   PARM                    $LenOut
074500950616   6 C     $Esito        ifeq      *ON
074600960912     C                   clear                   rddcpu
074700950616   6xC                   else
074800950619     C                   movel     desbrev002    rddcpu
074900060206D1940C* vedo se alla data di registr. del movimento ci sono state
075000060206  "  C* delle variazioni di ragione sociale
075100060206D1940C                   exsr      VarDatFis1
075200950616   6-C                   endif
075300960110   6-C                   endif
075400950622     C*
075500950616   6 C                   if        movdivisa<>xscdiv
075600950615     C                   movel     movdivisa     rddivi
075700950616     C                   z-add     movimportd    rdimpod
075800950616   6-C                   endif
075900950619     C*
076000950616     C                   z-add     dare          rddare
076100950616     C                   z-add     avere         rdavere
076200950614     C*
076300960110     C                   z-add     movnrlgio     rdnrlgio
076400980622     C*
076500950616   6 C                   if        *in39=*on
076600950616     C                   EXSR      STATES
076700950622     C                   move      *off          *in39
076800950616   6-C                   endif
076900981218     C                   write     RigP1
077000000914D0703 *Memorizzo che almeno un movimento esiste per il conto
077100000914  "   *(se richiesta stampa solo movimentati e il conto
077200000914  "   *non hamovimenti, non stampo neanche le variazioni fiscali)
077300000914  "  C                   Eval      $ExistMov   = *On
077400000914D0703 *
077500951103     c* stampa descrizioni aggiuntive riga
077600951103     C                   Eval      DesTipDes = 'M'
077700951103     C                   Eval      DesNrRigaM = MovNrRigaM
077800951103     C                   exsr      stadesagg
077900951103     c* stampa descrizioni aggiuntive testata
078000951103     C                   Eval      DesTipDes = 'T'
078100951103     C                   Eval      DesNrRigaM = 0
078200951103     C                   exsr      stadesagg
078300950616     C*
078400950616   5-C                   endif
078500950615     C*
078600070613D2217C                   endif
078700950615   3-C                   endif
078800950606     C*
078900950615   2-C                   endif
079000980605   2-C                   endif
079100950615     C*
079200950615   1-C                   enddo
079300950622     C*
079400950622     C* totali del mastrino
079500950622     C* se c'è un totale da stampare
079600950622   1 C                   if        $statot=*on
079700950621     C* ultimo totale mese
079800960112   2 C                   if        slm132=*on
079900950622   3 C                   if        dareme>0 or avereme>0
080000981218     C                   clear                   ValoriTot
080100970721     c                   setoff                                       414243
080200970721     c                   setoff                                       4445
080300970721     c                   seton                                        42
080400970721     C******             movel     totmes        rtdestot
080500950621     C                   move      meseanno      rtdestot
080600960723     C* saldo progressivo
080700960723     C                   if        slp132=*on
080800960723     C                   eval      salprog = salprog + dareme - avereme
080900960723     C                   endif
081000960723     C                   eval      rtsalprog=salprog
081100960723     C                   if        salprog>0
081200961009     C                   move      XscDare       rtdaprog
081300960723     C                   else
081400960723     C                   if        salprog<0
081500961009     C                   move      XscAvere      rtdaprog
081600960723     C                   else
081700960723     C                   move      *blank        rtdaprog
081800960723     C                   endif
081900960723   2-C                   endif
082000950621     C* i valori
082100950621     C                   eval      rtdare=dareme
082200950621     C                   eval      rtavere=avereme
082300950622     C* ripulisco i campi.........
082400950622     C                   clear                   periodo
082500950622     C                   eval      dareme=0
082600950622     C                   eval      avereme=0
082700981218     C                   write     ValoriTot
082800981218     C                   write     RigaVuota
082900950622   3-C                   endif
083000950622   2-C                   endif
083100950622     C* i valori del periodo del mastrino
083200981218     C                   clear                   ValoriTot
083300970721     c                   setoff                                       414243
083400970721     c                   setoff                                       4445
083500970721     c                   seton                                        43
083600970721     C*******            movel     totper        rtdestot
083700950622     C*
083800950621     C                   eval      rtdare=darepr
083900950621     C                   eval      rtavere=averepr
084000950622     C* alimenta i valori del periodo di tutti i mastrini
084100950622     C                   eval      daretopr=darepr+daretopr
084200950622     C                   eval      averetopr=averepr+averetopr
084300981218     C                   write     ValoriTot
084400950622     C* i valori iniziali + il periodo del mastrino
084500981218     C                   clear                   ValoriTot
084600970721     c                   setoff                                       414243
084700970721     c                   setoff                                       4445
084800970721     c                   seton                                        44
084900970721     C******             movel     totatt        rtdestot
085000950622     C*
085100950622     C                   eval      rtdare=darepr+darein
085200950622     C                   eval      rtavere=averepr+averein
085300981218     C                   write     ValoriTot
085400950622     C* il saldo del mastrino
085500970721     c                   setoff                                       414243
085600970721     c                   setoff                                       4445
085700970721     c                   seton                                        45
085800970721     C******             movel     totsal        rtdestot
085900950622     C*
086000950622   1 C                   if        rtdare>=rtavere
086100950622     C                   eval      rtdare=rtdare-rtavere
086200950622     C                   eval      rtavere=0
086300950622   1xC                   else
086400950622     C                   eval      rtavere=rtavere-rtdare
086500950622     C                   eval      rtdare=0
086600950622   1-C                   endif
086700981218     C                   write     ValoriTot
086800981218     C                   clear                   ValoriTot
086900950622     C                   move      *off          $statot
087000950622   1-C                   endif
087100950622     C*
087200000914D0703X*******************ENDSR
087300000914D0703C     XLegMov       ENDSR
087400070613D2217 ************************************************************
087500070613  "   * reperimento ndreg
087600070613  "   ************************************************************
087700070613  "  C     RepNDREG      begsr
087800070613  "   *
087900070613  "  C                   eval      $regOk=*on
088000070613  "  C                   z-add     movsys        regsys
088100070613  "  C                   z-add     movnrasreg    regnrasreg
088200070613  "  C     k2reg1        chain     ndreg01l                           22
088300070613  "  C                   if        *in22=*on or regnrreg=0
088400070613  "  C                   eval      $regOk=*off
088500070613  "  C                   endif
088600070613  "   *
088700070613D2217C     XRepNDREG     endsr
088800990422     C************************************************************
088900990422     C* Stampa eventuali variazioni dati fiscali
089000990422     C************************************************************
089100990422     C     VarDatFis     BEGSR
089200000914D0703 *
089300000914  "   * se richiesta stampa conti non movimentati     oppure
089400000914  "   * richiesta stampa solo conti movimentati e il conto ha movimenti
089500000914  "  C                   If        CNM132 = *On       OR
089600000914  "  C                             (CNM132 = *Off AND
089700000914  "  C                              $ExistMov = *On)
089800000914D0703 *
089900990422     C*
090000030325D1309x***                If        RCOSNatura = 'C' or RCOSNatura = 'F'
090100030804D1416C                   If        RCOSNatura = 'C' or RCOSNatura = 'F'
090200030804D1309C* Se conto gestito a soggetti
090300030325D1309c                   if        rcokccsogg = *on
090400990423     C*
090500990423     C                   Move      *Off          $TestaFis         1
090600000118C1309C******DMY          Move      DtI132        F0DtI
090700000118C1309C     *JobRun       Move      DtI132        F0DtI
090800000118C1309C******DMY          Move      DtF132        F0DtF
090900000118C1309C     *JobRun       Move      DtF132        F0DtF
091000990423     C*
091100990423     C                   Move      '2'           DVCRIC
091200990423     C                   Move      Soc132        DVCSOCIETA
091300990423     C                   Move      Uni132        DVCUNITA
091400990423     C                   MoveL     'ANCPI'       DVCSTRUTT
091500990423     C                   Move      DtI132        DVCDTRIF
091600990423     C                   Eval      DVCLENOUT = %Size(ANCPI)
091700990423     C                   Move      RCOSogg       DVCSOGG
091800990423     C                   CALLB     'NDDVACPI'    PDVACPI
091900990423     C                   If        DVCERRORE = '0'
092000990423     C                   MoveL     DVCOUTPUT     ANCPI
092100990423     C                   EndIf
092200990422     C*
092300990423     C                   DoW       DVCERRORE = '0'
092400990423     C                             and CPIDtFiVl <= DtF132
092500990423     C*
092600990423     C                   If        *In39 = *On
092700040401c1744c                   add       1             PagTot
092800130625>>>>>C                   add       1             ypagnbr
092900990423     C                   Write     TESTATA
093000990423     C                   Write     C132T20
093100990423     C                   Write     C132T21
093200990423     C                   Write     DATFIS0
093300990423     C                   SetOff                                       39
093400990423     C                   Move      *On           $TestaFis
093500990423     C                   Else
093600990423     C                   If        $TestaFis = *Off
093700990423     C                   Write     DATFIS0
093800990423     C                   Move      *On           $TestaFis
093900990423     C                   EndIf
094000990423     C                   EndIf
094100990423     C*
094200000118C1309C******DMY          Move      CPIDtFiVl     F1DtFiVl
094300000118C1309C     *JobRun       Move      CPIDtFiVl     F1DtFiVl
094400990423     C                   MoveL     CPIDesSog1    F1DesSog1
094500990423     C                   MoveL     CPIDesSog2    F1DesSog2
094600990423     C                   MoveL     CPIIndriz     F1Indriz
094700990423     C                   MoveL     CPILocalit    F1Localit
094800990423     C*
094900990423     C                   Write     DATFIS1
095000990423     C*
095100990423     C                   Move      '3'           DVCRIC
095200990423     C                   CALLB     'NDDVACPI'    PDVACPI
095300990423     C                   If        DVCERRORE = '0'
095400990423     C                   MoveL     DVCOUTPUT     ANCPI
095500990423     C                   EndIf
095600990423     C                   EndDo
095700990423     C*
095800990423     C                   EndIf
095900030804D1416C                   EndIf
096000000914     C*
096100000914D0703 *
096200000914  "  C                   EndIf
096300000914  "   *
096400000914D0703C     XVarDatFis    ENDSR
096500000914D0703X*******************ENDSR
096600060206D1940C************************************************************
096700060206     C* Stampa la descrizione del soggetto troncata se ci sono
096800060206     c* state delle variazioni alla data di registrazione del movimento
096900060206D1940C************************************************************
097000060206     C     VarDatFis1    BEGSR
097100060206     C*
097200060206     C* solo per clienti e fornitori
097300060206     C                   If        Snatura002 = 'C' or Snatura002 = 'F'
097400060206     C* se conto gestito a soggetti
097500060206     c                   if        kccsogg002 = *on
097600060206     C*
097700060206     C                   Move      '2'           DVCRIC
097800060206     C                   Move      Soc132        DVCSOCIETA
097900060206     C                   Move      Uni132        DVCUNITA
098000060206     C                   MoveL     'ANCPI'       DVCSTRUTT
098100060206     C                   Move      MovDtReg      DVCDTRIF
098200060206     C                   Eval      DVCLENOUT = %Size(ANCPI)
098300060206     C                   Move      sogg002       DVCSOGG
098400060206     C                   CALLB     'NDDVACPI'    PDVACPI
098500060206     C                   If        DVCERRORE = '0'
098600060206     C                   MoveL     DVCOUTPUT     ANCPI
098700060206     C                   movel     cpidessog1    RddCpu
098800060206     C                   EndIf
098900060206     C*
099000060206     C                   EndIf
099100060206     C                   EndIf
099200060206     C*
099300060206D1940c                   ENDSR
099400060206      *
099500060206     C************************************************************
099600951103     C* Stampa descrizioni aggiuntive
099700951103     C************************************************************
099800951103     C     STADesAgg     BEGSR
099900951103     C*
100000951103     C                   Eval      DesSYS     =  MOVSYS
100100951103     C                   eval      DesNRASREG =  MOVNRASREG
100200951103     C     K04Des01      SETLL     NDDes01L
100300951103     C                   DO        *hival
100400951103     C     K04des01      READE     NDdes000                               21
100500951103     C   21              LEAVE
100600951103     C*
100700951103     C                   IF        DesUsoCiv = *on
100800951103     C*
100900951103     C                   SELECT
101000951103     C                   WHEN      DesTipDes = 'T'
101100951103     C                   eval      TDesAgg = DesTesRig
101200951103   6 C                   if        *in39=*on
101300951103     C                   EXSR      STATES
101400951103     C                   move      *off          *in39
101500951103   6-C                   endif
101600951103     C                   write     C132DEST
101700951103     C*
101800951103     C                   WHEN      DesTipDes = 'M'
101900951103     C                   eval      MDesAgg = DesTesRig
102000951103   6 C                   if        *in39=*on
102100951103     C                   EXSR      STATES
102200951103     C                   move      *off          *in39
102300951103   6-C                   endif
102400951103     C                   write     C132DESM
102500951103     C                   ENDSL
102600951103     C*
102700951103     C                   ENDIF
102800951103     C*
102900951103     C                   ENDDO
103000951103     C*
103100951103     C                   ENDSR
103200950405     C/EJECT
103300941103      ************************************************************
103400950616      * Stampa Testata
103500941103      ************************************************************
103600950616     C     STATES        BEGSR
103700130625>>>>>C* Salto pagina se:
103800130625>>>>>C* - overflow.
103900130625>>>>>C* - stampa per il CD.
104000130625>>>>>C                   if        *in39
104100130625>>>>>C                             OR
104200130625>>>>>C                             Ycd132 = *ON
104300130625>>>>>C                   add       1             ypagnbr
104400040401c1744c                   add       1             PagTot
104500981218     C                   write     Testata
104600130625>>>>>C                   setoff                                       39
104700130625>>>>>C                   endif
104800970110     C* spengo indicatore 50 (se era acceso ho inizializzato il nr. pagina)
104900970110     C                   move      *off          *in50
105000121112D2758C* spengo il salto pagina altrimenti lo farebbe di nuovo
105100121112D2758C   39              move      *off          *in39
105200970110     C*
105300950616     C                   write     c132t20
105400950622   1 C                   if        rcosnatura='C' or rcosnatura='F'
105500950616     C                   write     c132t21
105600950619   1-C                   endif
105700950619     C* colonne campi
105800981218     C                   write     RigaVuota
105900981221     C                   If        Slp132 = *On
106000981221     C                   setOn                                        46
106100981221     C                   Else
106200981221     C                   setOff                                       46
106300981221     C                   EndIf
106400950619     C                   write     c132t22
106500950621     C*
106600981218     C                   clear                   ValoriTot
106700950622     C* se richiesti saldi iniziali, solo la 1° volta
106800950622   1 C                   if        $states=*ON
106900970721     c                   setoff                                       414243
107000970721     c                   setoff                                       4445
107100970721     c                   seton                                        41
107200970721     C****               movel     totpre        rtdestot
107300950619     C* totali
107400950622     C                   eval      rtdare=darein
107500950622     C                   eval      rtavere=averein
107600950622     C* se richiesti saldi progressivi
107700960112   2 C                   if        slp132=*on
107800950623     C                   eval      saldo=saldodare-saldoavere
107900960110     C                   eval      rtsalprog=saldo
108000960110     C                   if        saldodare>saldoavere
108100961009     C                   move      XscDare       rtdaprog
108200960110     C                   else
108300960110     C                   if        saldodare<saldoavere
108400961009     C                   move      XscAvere      rtdaprog
108500960110     C                   else
108600960110     C                   move      *blank        rtdaprog
108700960110     C                   endif
108800950622   2-C                   endif
108900960110   2-C                   endif
109000950622     C*
109100981218     C                   write     ValoriTot
109200981218     C                   write     RigaVuota
109300950622   1-C                   endif
109400950616     C*
109500950623     C                   move      *off          $states
109600950623     C*
109700950616     C                   ENDSR
109800950616     C/EJECT
109900950616      ************************************************************
110000950619      * Stampa totali e legenda causali
110100950616      ************************************************************
110200950619     C     STATOT        BEGSR
110300950831     C* stampa totali elaborazione a nuova pagina
110400040401c1744c                   add       1             PagTot
110500130625>>>>>C                   add       1             ypagnbr
110600990423     C                   write     Testata
110700981218     C                   write     RigaVuota
110800990426     C                   write     TestaTot
110900990426     C                   write     RigaVuota
111000950622     C* alimenta i valori del periodo di tutti i mastrini
111100981218     C                   clear                   ValoriTot
111200970721     c                   setoff                                       414243
111300970721     c                   setoff                                       4445
111400970721     c                   seton                                        43
111500970721     C******             movel     totper        rtdestot
111600950622     C*
111700950622     C                   eval      rtdare=daretopr
111800950622     C                   eval      rtavere=averetopr
111900981218     C                   write     ValoriTot
112000950622     C* i valori iniziali + il periodo di tutti i mastrini
112100981218     C                   clear                   ValoriTot
112200970721     c                   setoff                                       414243
112300970721     c                   setoff                                       4445
112400970721     c                   seton                                        44
112500970721     C******             movel     totatt        rtdestot
112600950622     C*
112700950622     C                   eval      rtdare=daretopr+daretoin
112800950622     C                   eval      rtavere=averetopr+averetoin
112900981218     C                   write     ValoriTot
113000950622     C* il saldo dei tutti i mastrini
113100970721     c                   setoff                                       414243
113200970721     c                   setoff                                       4445
113300970721     c                   seton                                        45
113400970721     C******             movel     totsal        rtdestot
113500950622     C*
113600950622   1 C                   if        rtdare>=rtavere
113700950622     C                   eval      rtdare=rtdare-rtavere
113800950622     C                   eval      rtavere=0
113900950622   1xC                   else
114000950622     C                   eval      rtavere=rtavere-rtdare
114100950622     C                   eval      rtdare=0
114200950622   1-C                   endif
114300981218     C                   write     ValoriTot
114400981218     C                   write     RigaVuota
114500981218     C                   clear                   ValoriTot
114600950622     C                   move      *off          $statot
114700950622     C*
114800950619     C* stampa legenda cusali
114900960112     C                   if        cau132=*on
115000950619     C                   EXSR      STALISCAU
115100950619     C                   endif
115200950619     C*
115300950619     C                   ENDSR
115400950619     C/EJECT
115500950619     C************************************************************
115600950619     C* RICHIAMA PGM stampa lista causali
115700950619     C************************************************************
115800950619     C     STALISCAU     BEGSR
115900950619     C*
116000950619     C                   WRITE     C106TS
116100950619     C*
116200950619     C                   MOVE      SOC132        OPESOCIETA
116300950619     C*
116400950619     C     OPESOCIETA    SETLL     ANOPE01L
116500950619     C                   DO        *HIVAL
116600950619     C     OPESOCIETA    READE     ANOPE01L                               21
116700950619     C   21              LEAVE
116800950619     C                   ADD       1             $CT               1 0
116900950619     C     $ct           ifeq      4
117000950619     C                   CLEAR                   C106P1
117100950619     C                   Z-add     1             $ct
117200950619     C                   ENDIF
117300950619     C                   SELECT
117400950619     C     $ct           WHENEQ    1
117500950619     C                   EVAL      P1cau1 =opecausale
117600950619     C                   EVAL      P1des1 =opedescr
117700950619     C     $ct           WHENEQ    2
117800950619     C                   EVAL      P1cau2 =opecausale
117900950619     C                   EVAL      P1DES2 =opedescr
118000950619     C     $ct           WHENEQ    3
118100950619     C                   EVAL      P1cau3 =opecausale
118200950619     C                   EVAL      P1DES3 =opedescr
118300950619     C*
118400970721     C   39              WRITE     C106TS
118500970721     C   39              SETOFF                                       39
118600950619     C                   WRITE     C106P1
118700950619     C*
118800950619     C                   ENDSL
118900950619     C*
119000950619   E1C                   ENDDO
119100950619     C* stampa ultima riga se non stampata
119200950619     C     $ct           ifne      3
119300970721     C   39              WRITE     C106TS
119400970721     C   39              SETOFF                                       39
119500950619     C                   WRITE     C106P1
119600950619     c                   endif
119700950619     C*
119800950619     C                   ENDSR
119900950619     C/EJECT
120000950619      ************************************************************
120100950619      * Reperimento dati società
120200950619      ************************************************************
120300950619     C     REPSOC        BEGSR
120400950619      *
120500961009     C                   CALLB     'XSOC'
120600941103     C                   PARM                    TIPXSC            6
120700941103     C                   PARM                    SOCXSC            3
120800941103     C                   PARM                    CDSXSC            9 0
120900941103     C                   PARM                    MODXSC            3
121000941103     C                   PARM      *BLANKS       RTNXSC            1
121100941103     C                   PARM                    XSOCDS
121200941103     C                   PARM                    KPJBA
121300941103      *
121400941103     C                   ENDSR
121500950615     C/EJECT
121600961121      ************************************************************
121700961121      * Decodifica causale contabile
121800961121      ************************************************************
121900961121     C     CALLMVC003    BEGSR
122000961121      *
122100961121     C                   clear                   anope
122200961121     C                   EVAL      LenOut = %Size(Anope)
122300961121     C                   CALLB     'NDMVC003'
122400961121     C                   PARM                    XscSoc
122500961121     C                   PARM                    Causale
122600961121     C                   PARM                    wdate
122700961121     C                   PARM      '1'           Uso
122800961121     C                   PARM      *off          GesMsg
122900961121     C                   PARM      *off          Esito
123000961121     C                   PARM      'ANOPE'       StrutturaO
123100961121     C                   PARM                    Anope
123200961121     C                   PARM                    LenOut
123300961121     C                   PARM      *off          Refresh
123400961121      *
123500961121     C                   ENDSR
123600170927>>>>>C/EJECT
123601170927>>>>>
123602170927>>>>>  // --------------------------------------------------
123603170927>>>>>  // Procedure name: SendToDocFlow
123604170927>>>>>  // Purpose:        Invia lo spool a DocFlow
123605170927>>>>>  // Returns:        Esito.
123606170927>>>>>  // --------------------------------------------------
123607170927>>>>>
123608170927>>>>>  DCL-PROC SendToDocFlow ;
123609170927>>>>>
123610170927>>>>>    DCL-PI *N INT(10);
123611170927>>>>>    END-PI ;
123612170927>>>>>
123613170927>>>>>    DCL-DS local QUALIFIED INZ;
123614170927>>>>>      aplKey CHAR(15) INZ('PJMASTRINI');
123615170927>>>>>      filNam CHAR(10) INZ('NDC132P');
123616170927>>>>>      numFil CHAR(6);
123617170927>>>>>      fileOut CHAR(255) INZ('SCON');
123618170927>>>>>      subFldr CHAR(50) INZ('SCON');
123619170927>>>>>      esito CHAR(1);
123620170927>>>>>    END-DS;
123621170927>>>>>
123622170927>>>>>    // Non creo il file per DocFlow quando non è una stampa definitiva.
123623170927>>>>>
123624170927>>>>>    IF ycd132 = *OFF OR fin132 = *OFF;
123625170927>>>>>      RETURN *ZERO;
123626170927>>>>>    ENDIF;
123627170927>>>>>
123628170927>>>>>    local.numFil = %EDITC(%DEC(yPrtInf.spoolNum:6:0):'X');
123629170927>>>>>
123630170927>>>>>    local.fileOut = %TRIMR(local.fileOut) + soc132 + %CHAR(dtf132)
123631170927>>>>>                  + '.spl';
123632170927>>>>>
123633170927>>>>>    CLOSE ndc132p;
123634170927>>>>>
123635170927>>>>>    TRBM_SendSpoolFileToIfs( local.aplKey
123636170927>>>>>                           : *BLANK
123637170927>>>>>                           : local.filNam
123638170927>>>>>                           : psJobName : psJobUser : psJobNbr
123639170927>>>>>                           : local.numFil
123640170927>>>>>                           : *BLANK
123641170927>>>>>                           : local.fileOut
123642170927>>>>>                           : local.subFldr
123643170927>>>>>                           : 'S'
123644170927>>>>>                           : 'S'
123645170927>>>>>                           : 'S'
123646170927>>>>>                           : local.esito
123647170927>>>>>                           );
123648170927>>>>>
123649170927>>>>>    IF local.esito = '2';
123650170927>>>>>      DUMP(A);
123651170927>>>>>      RETURN -1;
123652170927>>>>>    ENDIF;
123653170927>>>>>
123654170927>>>>>    RETURN *ZERO;
123655170927>>>>>
123656170927>>>>>  END-PROC ;
