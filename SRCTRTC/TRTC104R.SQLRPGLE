000100050131     H DECEDIT('0,') DATEDIT(*YMD.)
000200050127      ***********************************************************************
000300050225      * - Il programma deve prendere tutti i Files non spediti e reimpostare
000400050225      *   nuovamente la trasmissione.
000500050225      *   Il programma rileverà quali sono i files e imposterà le azioni da
000600050225      *   sottomettere.
000700930128      ***********************************************************************
000800050225     FSNDIFCSUM1iF   E           k DISK    usropn rename(SNDDATI:SNDSUM)
000900050225     f                                            prefix(sum)
001000050127      *
001100050225     FSNDIFTSTA1iF   E           k DISK    usropn rename(SNDDATI:SNDSTA)
001200050225     f                                            prefix(sta)
001300000119      *---------------------------------------------------------------------*
001400000119      * DS
001500000119      *---------------------------------------------------------------------*
001600960528     D KPJBA         E DS
001700050127      *
001800050225     D Volte           s              3  0
001900050225     D Stato_Elab      s              2
002000041201     D Almeno_Uno      s              2    INZ('  ')
002100050131      *
002200050131     D data_iso        s               d   datfmt(*iso)
002300050131     D data_old        s               d   datfmt(*iso)
002400050131      *
002500050225     D savElabora      s                   like(SumSSelabora)
002600050225     D savLinea        s                   like(SumSSLinea)
002700050225     D savNumero       s                   like(SumSSNumero)
002800050225      *
002900050131     D                SDS
003000050131     D  §PGMname               1     10
003100050131     D  §USRname             254    263
003200980225      *---------------------------------------------------------------------*
003300050131     D Digits          C                   CONST('0123456789')
003400050131      *---------------------------------------------------------------------*
003500980225      * Ciclo principale
003600980225      *---------------------------------------------------------------------*
003700050127     C     *ENTRY        PLIST
003800050127     C                   PARM                    KPJBA
003900050225     C*
004000050225     C*  il pgm viene eseguito solo in ambiente di produzione
004100050131     C*
004200050131     C* RECUPERO DATA E ORA
004300050131     C                   TIME                    WHHD12           12 0
004400050131     C                   MOVE      WHHD12        DATA6             6 0
004500050131     C                   MOVEL     DATA6         DATA              6 0
004600050131     C                   TIME                    WORA              6 0
004700050127      *
004800050127     c     *like         define    kpjbu         kpjbu_sav
004900050127     c                   move      kpjbu         kpjbu_sav
005000050127     c                   moveL     kpjbu         Stato_Elab
005100050127      *
005200050127     C* Lettura file x chiavi
005300050225     c     SUM_Dopo      klist
005400050225     c                   kfld                    sumSSelabora
005500050225     c                   kfld                    sumSSlinea
005600050225     c                   kfld                    sumSSnumero
005700050225      *
005800050225     c     STA_Dopo      klist
005900050225     c                   kfld                    staSSelabora
006000050225     c                   kfld                    staSSlinea
006100050225     c                   kfld                    staSSnumero
006200050225      *
006300050225      *  Controlla prima gli IFCSUM
006400050225     c                   EXSR      SND_IFCSUM
006500050225      *
006600050225      *  Poi Controlla gli IFTSTA
006700050225     c                   EXSR      SND_IFTSTA
006800050127      *
006900980225      * Fine pgm.
007000040604     c     fine          TAG
007100041201      *
007200041201      *      Imposta se ci sono dei records oppure no
007300041203     c                   move      kpjbu_sav     kpjbu
007400041201     c                   movel     Almeno_Uno    kpjbu
007500040604      *
007600960531     C                   SETON                                        LR
007700041201      *----------------------------------------------------------------
007800050225      *  Ciclo x Bolle
007900050127      *----------------------------------------------------------------
008000050225     c     SND_IFCSUM    Begsr
008100050127      *
008200091104      *  controlla se il file è allocato
008300091104     c                   clear                   allocato          1
008400091104     c                   call      'TRTC104C1'
008500091104     c                   parm                    allocato
008600091104      *
008700091104      *  Se NON allocato
008800091104     c                   if         allocato = *blank
008900050225     C/EXEC SQL
009000050225     C+ UPDATE SNDIFCSUM  Set  SSELABORA = 'OK'
009100050225     C+                 Where  SSELABORA = 'EF'
009200050225     C/END-EXEC
009300050127     c                   open      SNDIFCSUM1
009400050225      *
009500050225     c                   eval      Stato_Elab = 'OK'
009600050225     C     Stato_Elab    SETLL     SNDIFCSUM1
009700050225     C     Stato_Elab    READe     SNDIFCSUM1
009800050225      *
009900050225     c                   Dow       not %eof(SNDIFCSUM1)
010000050225     **  Prepara l'Azione da sottomettere
010100050225     c                   exsr      SUM_Azione
010200050225      *
010300050225     c     Sum_Dopo      SETgt     SNDIFCSUM1
010400050225     C     Stato_Elab    READe     SNDIFCSUM1
010500050225     c                   endDO
010600050225      *
010700050225     c                   close     SNDIFCSUM1
010800050225      *
010900091104     c                   End
011000091104      *
011100091104     c                   Endsr
011200050225      *----------------------------------------------------------------
011300050225      *  Ciclo x Stati
011400050225      *----------------------------------------------------------------
011500050225     c     SND_IFTSTA    Begsr
011600050225      *
011700050225     C/EXEC SQL
011800050225     C+ UPDATE SNDIFTSTA  Set  SSELABORA = 'OK'
011900050225     C+                 Where  SSELABORA = 'EF'
012000050225     C/END-EXEC
012100050225     c                   open      SNDIFTSTA1
012200050225      *
012300050225     c                   eval      Stato_Elab = 'OK'
012400050225     C     Stato_Elab    SETLL     SNDIFTSTA1
012500050225     C     Stato_Elab    READe     SNDIFTSTA1
012600050225      *
012700050225     c                   Dow       not %eof(SNDIFTSTA1)
012800050225     **  Prepara l'Azione da sottomettere
012900050225     c                   exsr      STA_Azione
013000050225      *
013100050225     c     Sta_Dopo      SETgt     SNDIFTSTA1
013200050225     C     Stato_Elab    READe     SNDIFTSTA1
013300050225     c                   endDO
013400050225      *
013500050225     c                   close     SNDIFTSTA1
013600050225      *
013700050225     c                   Endsr
013800050225      *----------------------------------------------------------------
013900050225      *  Imposta Parametri x sottomissione Azione
014000050225      *----------------------------------------------------------------
014100050225     c     SUM_Azione    Begsr
014200050225      *
014300050225     c                   move      sumSSLINEA    Lnp_alf           3
014400050225     c                   move      sumSSLINEA    Hub_alf           3
014500050225     c                   move      sumSSNUMERO   Num_alf           7
014600050225     c                   move      sumSSTIPO     Tip_alf           3
014700050225     c                   movel     'IFCSUM    '  Fil_alf          10
014800050225      *
014900050225     c                   EXSR      SND_Azione
015000050225      *
015100050225     c                   Endsr
015200050225      *----------------------------------------------------------------
015300050225      *  Imposta Parametri x sottomissione Azione
015400050225      *----------------------------------------------------------------
015500050225     c     STA_Azione    Begsr
015600050225      *
015700050225     c                   move      staSSLINEA    Lnp_alf           3
015800050225     c                   move      staSSLINEA    Hub_alf           3
015900050225     c                   move      staSSNUMERO   Num_alf           7
016000050225     c                   move      staSSTIPO     Tip_alf           3
016100050225     c                   movel     'IFTSTA    '  Fil_alf          10
016200050225      *
016300050225     c                   EXSR      SND_Azione
016400050225      *
016500050225     c                   Endsr
016600050225      *----------------------------------------------------------------
016700050225      *   Sottomissione azione impostat
016800050225      *----------------------------------------------------------------
016900050225     c     SND_Azione    Begsr
017000050225      *
017100050225     c                   clear                   kpjbu
017200060117     c                   movel     'TCC8'        kcoaz
017300051110      **
017400050225     c                   movel     'GAITRA201 '  knsif
017500050225     c                   eval      kqebi = 'KEDI'
017600050225     c                   eval      knmus = 'EDPCED'
017700050225     c                   eval      knmtd = 'EDPCED'
017800050225     c                   movel     'QPRINTS   '  kqopr
017900050225     c                   eval      %subst(kpjbu:227:10) =Fil_alf
018000050225     c                   eval      %subst(kpjbu:237:3) = Tip_alf
018100050225     c                   eval      %subst(kpjbu:240:7) = Num_alf
018200050225     c                   eval      %subst(kpjbu:247:1) = 'M'
018300050225     c                   eval      %subst(kpjbu:248:3) = Lnp_alf
018400050225     c                   eval      %subst(kpjbu:251:3) = '046'
018500050225     c                   eval      %subst(kpjbu:254:3) = Hub_alf
018600050225      *
018700050225      *  x provare la procedura mettevo in Hold il lavoro
018800050225     c*****              eval      KSTEB = 'H'
018900050225     c*****              eval      KSTJO = 'H'
019000050225     c                   call      'BCH10'
019100050225     c                   parm                    kpjba
019200050225      *
019300050225     c                   Endsr
019400050225      *----------------------------------------------------------------
