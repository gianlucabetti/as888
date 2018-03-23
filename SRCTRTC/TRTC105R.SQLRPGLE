000100050131     H DECEDIT('0,') DATEDIT(*YMD.)
000200050127      ***********************************************************************
000300050302      * - Il programma deve prendere tutti i Files non ricevuti e reimpostare
000400050302      *   nuovamente la ricezione.
000500050225      *   Il programma rileverà quali sono i files e imposterà le azioni da
000600050225      *   sottomettere.
000700930128      ***********************************************************************
000800050302     FRCVIFCSUM1iF   E           k DISK    usropn rename(RCVDATI:RCVSUM)
000900050225     f                                            prefix(sum)
001000050127      *
001100050302     FRCVIFTSTA1iF   E           k DISK    usropn rename(RCVDATI:RCVSTA)
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
002500050302     D savElabora      s                   like(SumRRelabora)
002600050302     D savLinea        s                   like(SumRRLinea)
002700050302     D savNumero       s                   like(SumRRNumero)
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
003900050302     C*
004000050302     C*  i primi 10 bytes sono x lo USRMSG
004100050302     C                   eval      usrmsg = %Subst(kpjbu:1:10)
004200050302      *  l'undicesimo definisce se siamo in test
004300050302     C                   eval      flgtst = %Subst(kpjbu:11:1)
004400050131     C*
004500050131     C* RECUPERO DATA E ORA
004600050131     C                   TIME                    WHHD12           12 0
004700050131     C                   MOVE      WHHD12        DATA6             6 0
004800050131     C                   MOVEL     DATA6         DATA              6 0
004900050131     C                   TIME                    WORA              6 0
005000050127      *
005100050127     c     *like         define    kpjbu         kpjbu_sav
005200050127     c                   move      kpjbu         kpjbu_sav
005300050127     c                   moveL     kpjbu         Stato_Elab
005400050127      *
005500050127     C* Lettura file x chiavi
005600050302     c     SUM_Dopo      klist
005700050302     c                   kfld                    sumRRelabora
005800050302     c                   kfld                    sumRRlinea
005900050302     c                   kfld                    sumRRnumero
006000050225      *
006100050302     c     STA_Dopo      klist
006200050302     c                   kfld                    staRRelabora
006300050302     c                   kfld                    staRRlinea
006400050302     c                   kfld                    staRRnumero
006500050225      *
006600050225      *  Controlla prima gli IFCSUM
006700050302     c                   EXSR      RCV_IFCSUM
006800050225      *
006900050225      *  Poi Controlla gli IFTSTA
007000050302     c                   EXSR      RCV_IFTSTA
007100050127      *
007200980225      * Fine pgm.
007300040604     c     fine          TAG
007400041201      *
007500041201      *      Imposta se ci sono dei records oppure no
007600041203     c                   move      kpjbu_sav     kpjbu
007700041201     c                   movel     Almeno_Uno    kpjbu
007800040604      *
007900960531     C                   SETON                                        LR
008000041201      *----------------------------------------------------------------
008100050225      *  Ciclo x Bolle
008200050127      *----------------------------------------------------------------
008300050302     c     RCV_IFCSUM    Begsr
008400050127      *
008500050225     C/EXEC SQL
008600050302     C+ UPDATE RCVIFCSUM  Set  RRELABORA = '  '
008700050302     C+                 Where  RRELABORA = 'EF'
008800050225     C/END-EXEC
008900050302     c                   open      RCVIFCSUM1
009000050225      *
009100050302     c                   eval      Stato_Elab = *blank
009200050302     C     Stato_Elab    SETLL     RCVIFCSUM1
009300050302     C     Stato_Elab    READe     RCVIFCSUM1
009400050225      *
009500050302     c                   Dow       not %eof(RCVIFCSUM1)
009600050302     c                   movel     'IFCSUM    '  Fil_alf          10
009700050302     c                   exsr      Ri_Ricevi
009800050225      *
009900050302     c     Sum_Dopo      SETgt     RCVIFCSUM1
010000050302     C     Stato_Elab    READe     RCVIFCSUM1
010100050225     c                   endDO
010200050225      *
010300050302     c                   close     RCVIFCSUM1
010400050225      *
010500050225     c                   Endsr
010600050225      *----------------------------------------------------------------
010700050225      *  Ciclo x Stati
010800050225      *----------------------------------------------------------------
010900050302     c     RCV_IFTSTA    Begsr
011000050225      *
011100050225     C/EXEC SQL
011200050302     C+ UPDATE RCVIFTSTA  Set  RRELABORA = '  '
011300050302     C+                 Where  RRELABORA = 'EF'
011400050225     C/END-EXEC
011500050302     c                   open      RCVIFTSTA1
011600050225      *
011700050302     c                   eval      Stato_Elab = *blank
011800050302     C     Stato_Elab    SETLL     RCVIFTSTA1
011900050302     C     Stato_Elab    READe     RCVIFTSTA1
012000050225      *
012100050302     c                   Dow       not %eof(RCVIFTSTA1)
012200050302     c                   movel     'IFTSTA    '  Fil_alf          10
012300050302     c                   exsr      Ri_Ricevi
012400050225      *
012500050302     c     Sta_Dopo      SETgt     RCVIFTSTA1
012600050302     C     Stato_Elab    READe     RCVIFTSTA1
012700050225     c                   endDO
012800050225      *
012900050302     c                   close     RCVIFTSTA1
013000050225      *
013100050225     c                   Endsr
013200050225      *----------------------------------------------------------------
013300050302      *  Lancia Nuovamente la ricezione
013400050225      *----------------------------------------------------------------
013500050302     c     Ri_Ricevi     Begsr
013600050302      *
013700050302     c                   call      'TRTC100R'
013800050302     c                   parm                    flgtst            1
013900050302     c                   parm                    usrmsg           10
014000050302     c                   parm                    Fil_alf
014100050225      *
014200050225     c                   Endsr
014300050225      *----------------------------------------------------------------
