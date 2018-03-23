000100950127     H DECEDIT('0,') DATEDIT(*DMY.)
000200030821     H* FICNB1R1 *----------------------------------------------------*
000300030821     H*         - conta le Bolle appartnenenti a stesso STOP
000400950127     H*--------------------------------------------------------------*
000500011113     FFIFTD01L  IF   E           K DISK
000600030821     Ffnarb01l  IF   E           K DISK
000700030821     Ffnblp01l  IF   E           K DISK
000800090909     FTntbe01L  IF   E           K DISK
000900090909     F*--------------------------------------------------------------*
001000090909     D   DCTP        E DS
001100090909     D KCliPar         S              7s 0 DIM(9999)                            Tar.Part.sul cliente
001200090909     D KCliParTSR      S              8a   DIM(9999)                            Tar.Part.sul cliente
001300090909     D KTarStdNew      S              7A   DIM(9999)                            Tar.Part.sul cliente
001400090909     D KTarStd         S              4A   DIM(9999)                            Tar.Part.sul cliente
001500090909     D KTarNew         S              3A   DIM(9999)                            Tar.Part.sul cliente
001600090909     D KTarObb         S              1A   DIM(9999)                            Tar.Part.sul cliente
001700090909     D KTarUgu         S              1A   DIM(9999)                            Tar.Part.sul cliente
001800090911     D KCliTrovato     S              7s 0 DIM(9999)                            Tar.Part.sul cliente
001900090911     D KStdNewCli      S             14A   DIM(9999) DESCEND                    Tar.Part.sul cliente
002000090911     D KxILcliente     S              1A
002100090909     D Cli_TAR_Part...                                                          Cliente con tariffe
002200090909     D                 S              1A   INZ('S')                             Particolari
002300090909     D*
002400020319     F*--------------------------------------------------------------*
002500030821     D FICNB1        E DS                  EXTNAME(FICNB1DS)
002600950127     C*****************************************************************
002700020319     C     *ENTRY        PLIST
002800030821     C                   PARM                    ficnb1
002900090911      *
003000090911      *  reset dei parametri per indicare pagamenti con tariffe particolari
003100090911     c                   eval      §T1PARTIC = 'N'
003200090911     c                   eval      §T1CLIPAR = 0
003300020319      *
003400020319     C     KFTD          KLIST
003500030821     C                   KFLD                    ftdPDR
003600030821     C                   KFLD                    ftdTSR
003700030821     C                   KFLD                    ftdNDC
003800030821     C                   KFLD                    ftdDDC
003900030821     C                   KFLD                    ftdSTP
004000020319     C*
004100030821     C     KBolla        KLIST
004200030821     C                   kfld                    ftdAAS
004300030821     C                   kfld                    ftdLNP
004400030821     C                   kfld                    ftdNRS
004500030821     C                   kfld                    ftdNSP
004600090909      *----
004700090909     c                   reset                   Cli_TAR_Part
004800090911     C                   Z-ADD     0             C                 5 0
004900090909     C                   movel(P)  'CTP'         tbeCOD
005000090911     c                   clear                   KCliTrovato
005100090909     c                   clear                   KCliPar
005200090914     c                   clear                   KCliParTSR
005300090909     c                   clear                   KTarStdNew
005400090909     c                   clear                   KTarStd
005500090909     c                   clear                   KTarNew
005600090909     c                   clear                   KTarObb
005700090909     c                   clear                   KTarUgu
005800090911     c                   clear                   KxILcliente
005900090911      *----
006000090909     C     tbeCOD        setll     tntbe01l
006100090909     C     tbeCOD        reade     tntbe01l
006200090909      *
006300090909     c                   dow       not %eof(tntbe01l)
006400090909     C                   add       1             C
006500090909     C                   MOVEL     TBeKE1        KCliPar(C)
006600090909     C                   MOVEL     TBeKE1        KCliParTSR(C)
006700090909     C                   MOVEL     TBeKE2        primoByte         1
006800090911      * il tipo servizio alla destra del codice cliente
006900090909     C                   MOVE      primoByte     KCliParTSR(C)
007000090911     C                   MOVEL     TBeKE2        KTarStd(C)
007100090909     C                   movel     tbeuni        dCTP
007200090911     C                   MOVEL     TBeKE2        KTarStdNew(C)
007300090911     C                   MOVE      §CTPCTD       KTarStdNew(C)
007400090911     C                   MOVEL     §CTPCTD       KTarnew(C)
007500090911     C                   MOVEL     §CTPOBB       KTarobb(C)
007600090911     C                   MOVEL     §CTPUGU       KTarugu(C)
007700090909     C     tbeCOD        reade     tntbe01l
007800090909     c                   enddo
007900090911      *
008000090911     C                   z-add     C             SavC              5 0
008100090911      *-----------------------------------------------------------------
008200090911      *   Caricate tariffe particolari procede con il ciclo
008300020319      *-----------------------------------------------------------------
008400030821     C                   z-add     0             conta_Sped        5 0
008500030821     C                   z-add     0             conta_FW          5 0
008600030821     C                   z-add     0             conta_RCC         5 0
008700090909     C                   z-add     0             codCLIpartic      7 0
008800090911     C                   Z-ADD     0             TotClienti        5 0
008900020319     C*
009000030821     C                   z-add     §t1PDR        ftdPDR
009100030821     C                   move      §t1TSR        ftdTSR
009200030821     C                   z-add     §t1NDC        ftdNDC
009300030821     C                   z-add     §t1DDC        ftdDDC
009400030821     C                   z-add     §t1STP        ftdSTP
009500030821     C*
009600030821     C     KFTD          chain     fiftd01l                           21
009700030821     C     *in21         DOweq     *Off
009800030821     c                   add       1             conta_Sped
009900030821     C*
010000030821     C* Se è un Ritiro vado su Bolle in Partenza
010100030821     c                   if        ftdTSR = 'R'
010200030821     C     KBolla        chain     fnblp01l
010300090909     c                   if        %Found(fnblp01l)
010400090909      *
010500090909      * se non è in tabella particolarità
010600090909     c                   if        Cli_TAR_Part = 'S'
010700090911     c* il tipo servizio alla destra del codice cliente
010800090911     c                   movel     blpksc        cliTSR            8
010900090911     c                   move      'R'           cliTSR
011000090911     c     cliTSR        lookup    KcliParTSR                             33
011100090909     c                   if        not *in33
011200090909     c                   movel     'N'           Cli_TAR_Part
011300090909     c                   else
011400090909     C                   z-add     blpksc        codCLIpartic
011500090911     c     blpksc        lookup    KcliTrovato                            34
011600090911     C  N34              Z-ADD     1             C
011700090911     C  N34              add       1             TotClienti
011800090911     c  N34*ZERO         lookup    KcliTrovato(C)                         34
011900090911     C   34              z-add     blpksc        KcliTrovato(C)
012000090909     c                   end
012100090909     c                   end
012200090909      *
012300090909     c                   if        blpCBO = '2R'
012400030821     c                   add       1             conta_RCC
012500030821     c                   end
012600090909      *
012700090909     c                   end
012800030821     c                   end
012900030821     C*
013000030821     C* Se è una Consegna vado su Bolle in Arrivo
013100030821     c                   if        ftdTSR = 'C'
013200030821     C     KBolla        chain     fnarb01l
013300090909     c                   if        %Found(fnarb01l)
013400090909      *
013500090909      * se non è in tabella particolarità
013600090909     c                   if        Cli_TAR_Part = 'S'
013700090911     c* il tipo servizio alla destra del codice cliente
013800090909     c                   movel     arbksc        cliTSR            8
013900090909     c                   move      'C'           cliTSR
014000090911     c     cliTSR        lookup    KcliParTSR                             33
014100090909     c                   if        not *in33
014200090909     c                   movel     'N'           Cli_TAR_Part
014300090909     c                   else
014400090911     C                   z-add     arbksc        codCLIpartic
014500090911     c     ARBksc        lookup    KcliTrovato                            34
014600090911     C  N34              Z-ADD     1             C
014700090911     C  N34              add       1             TotClienti
014800090911     c  N34*ZERO         lookup    KcliTrovato(C)                         34
014900090911     C   34              z-add     ARBksc        KcliTrovato(C)
015000090909     c                   end
015100090909     c                   end
015200090909     C*
015300090909     c                   if        arbCBO = 'FW'
015400030821     c                   add       1             conta_FW
015500030821     c                   end
015600090909     C*
015700030821     c                   end
015800090909     c                   end
015900030821     C*
016000030821     C     KFTD          reade     fiftd01l                               21
016100030821     C                   ENDDO
016200030821     C*
016300030821     c                   eval      §t1NSS  =  conta_Sped
016400030821     c                   eval      §t1RCC  =  conta_RCC
016500030821     c                   eval      §t1FW   =  conta_FW
016600090911     C*
016700090911     C*---------------------------------------------------------------*
016800090911     C*  Se presente lo stop per solo clienti Particolari
016900090911     C*   deve controllare
017000090911     C*  che si tratti del solo cliente oppure occorre verificare
017100090911     C*   fra i vari clienti che i clienti selezionati abbiano le stesse
017200090911     C*   tariffe altrimenti non si deve permettere l'applicazione delle
017300090911     C*   tariffe particolari.
017400090911     C*---------------------------------------------------------------*
017500090911     c                   if        Cli_TAR_Part = 'S'
017600090911     C*
017700090911     c                   if        TotClienti = 1
017800090911     C*
017900090911      * qui significa che tutte le bolle dello stop sono sotto lo stesso cliente
018000090911     c                   eval      §T1PARTIC = 'S'
018100090911     c                   eval      §T1CLIPAR = KcliTrovato(1)
018200090911     C*
018300090911     c                   else
018400090911     C*
018500090911      * se presenti più clienti con tariffe particolari occorre verificare
018600090911      *  che le tariffe siano omogenee sui 2 clienti altrimenti si deve procedere
018700090911      *  con le tariffe standard.
018800090911     c                   z-add     0             A                 5 0
018900090911     c                   clear                   KStdNewCli
019000090911     C*
019100090911      *  Controllo incrociato fra tariffe dei clienti
019200090911     c                   Do        TotClienti    Z                 5 0
019300090911     c                   exsr      Carica_SKFinal
019400090911     c                   endDo
019500090911      *
019600090911      *  Riorganizzando la SKiera Finale riordinandola x tariffa
019700090911      *   verifica che x i clienti selezionati vi siano le stessa tariffe
019800090911      *   altrimenti non deve applicare le tariffe particolari
019900090911     c                   exsr      CHK_incrociato
020000090911      *
020100090911     c                   if        tarUguali = 'N'
020200090911     c                   eval      §T1PARTIC = 'N'
020300090911     c                   eval      §T1CLIPAR = 0
020400090911     c                   else
020500090911      * qui significa che tutte le bolle dello stop sono particolari x + clienti
020600090911     c                   eval      §T1PARTIC = 'S'
020700090911     c                   eval      §T1CLIPAR = *all'9'
020800090911     c                   end
020900090911     C*
021000090911     c                   end
021100090911     C*
021200090911     c                   end
021300090911     C*
021400020320     c                   RETURN
021500020319     C*---------------------------------------------------------------*
021600090911     C*  Carica una schiera con Tar.STD + Tar.NEW + Cliente da riordinare
021700090911     C*  e confrontare.
021800090911     C*---------------------------------------------------------------*
021900090911     c     Carica_SKFinalBegsr
022000090911     C*
022100090911     c                   z-add     1             Cl                5 0
022200090911     c     KCliTrovato(Z)lookup    KCliPar(Cl)                            33
022300090911     c   33              dow       KCliTrovato(Z) = KCliPar(Cl)
022400090911     C*   incrementa sempre per ottenere un'unica schiera finale da confrontare
022500090911     c                   add       1             A
022600090911     c                   movel     KTarStdNew(Cl)KStdNewCli(A)
022700090911     c                   move      KCliPar(Cl)   KStdNewCli(A)
022800090911      *
022900090911     c                   add       1             Cl
023000090911      *  se oltre gli elementi esce x non far schiantare il pgm
023100090911     c                   if        Cl > %Elem(KcliPar)
023200090911     c                   Leave
023300090911     c                   end
023400090911      *
023500090911     c                   endDo
023600090911      *
023700090911     c                   endSR
023800090911     C*---------------------------------------------------------------*
023900090911      *  esce con tariffe Uguali oppure Diverse (quindi applicare Standard)
024000090911     C*---------------------------------------------------------------*
024100090911     c     CHK_incrociatoBegsR
024200090911      *
024300090911      *  Riordina in modo Discendente
024400090911     c                   SORTA     KStdNewCli
024500090911     c                   clear                   savTariffe        7
024600090911     c                   clear                   contaTAR          5 0
024700090911     c                   clear                   contaSAVE         5 0
024800090911     c                   move      'S'           tarUguali         1
024900090911     c                   z-add     1             C
025000090911      *
025100090911      * Ciclo di elementi riordinati in modo DIscendente
025200090911     c                   doW       C <= %elem(KStdNewCli)
025300090911      *
025400090911      *  Verifica che il totale delle tariffe precedentemente conteggiate
025500090911      *   sia numericamente identico a quello seguente altrimenti esce non
025600090911      *   permettendo il calcolo con le tariffe particolari.
025700090911      *    oppure x
025800090911      *  Fine lettura elementi validi caricati in schiera
025900090911      *
026000090911     c                   if        savTariffe <> %subst(KStdNewCli(C):1:7)
026100090911     c                             or  KStdNewCli(C) =  *blank
026200090911      *  Esegue il controllo
026300090911      * =====================
026400090911     c                   IF        ContaTAR  <> ContaSAVE and ContaSAVE >0
026500090911     c                   move      'N'           tarUguali         1
026600090911     c                   end
026700090911      *
026800090911      * Fine ciclo o per problemi fra tariffe clienti
026900090911     c                   if        KStdNewCli(C) =  *blank  or
027000090911     c                             tarUguali = 'N'
027100090911     c                   leave
027200090911     c                   end
027300090911      *
027400090911     c                   z-add     ContaTAR      ContaSAVE
027500090911     c                   eval      savTariffe =  %subst(KStdNewCli(C):1:7)
027600090911     c                   z-add     0             ContaTAR
027700090911     c                   end
027800090911      *
027900090911      *  conta il numero di tariffe di stessa tipologia
028000090911     c                   add       1             ContaTAR
028100090911      *
028200090911      *   incrementa il contatore di schiera da leggere
028300090911     c                   eval      C = C + 1
028400090911     c                   endDo
028500090911      *
028600090911     c                   endSR
028700090911     C*---------------------------------------------------------------*
