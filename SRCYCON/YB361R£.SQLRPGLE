000100021127      /TITLE RICONCILIAZIONE MANUALE MOVIMENTI BANCA
000200021127      *PARMS DFTACTGRP(*NO) ACTGRP(*CALLER)
000300021127      *PARMS COMMIT(*NONE) DATFMT(*ISO)
000400021127      *PARMS DYNUSRPRF(*OWNER)
000500021127     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000600021127      *---------------------------------------------------------------------
000700021127      *
000800021127     Fndbmv00f  UF   E             DISK     commit
000900021127      *---------------------------------------------------------------------
001000021127     D KPJBA         E DS
001100021128     D YB361DS       E DS
001200021127      *
001300021127     d nrRecord        S             10I 0
001400021127     C*---------------------------------------------------------------*
001500021127     C*
001600021127     C     *ENTRY        PLIST
001700021127     C                   PARM                    KPJBA
001800021128     C                   movel     kpjbu         yb361ds
001900021127     C*
002000021127     C/EXEC SQL
002100021127     C+ Declare A1 Cursor for
002200021127     c+ SELECT  RRN(NDBMV00F)
002300021127     c+ BMVSTATUS
002400021127     c+ FROM NDCPR00F
002500021127     c+ JOIN NDBMV00F
002600021127     c+ ON  BMVSYS = CPRSYSPRO
002700021127     c+ AND BMVNRASREG = CPRNRASPRO
002800021127     c+ AND BMVNRRIGAM = CPRNRRIPRO
002900021127     c+ WHERE CPRANN = ' '
003000021127     c+ AND   CPRSYSPRO  = :Y36SYS
003100021127     c+ AND   CPRNRASPRO = :Y36NRASREG
003200021127     c+ AND   CPRNRRIPRO = :Y36NRRIGAM
003300021127     c+ AND   BMVSTATUS = ' '
003400021127     c+ UNION
003500021127     c+ SELECT  RRN(NDBMV00F)
003600021127     c+ FROM NDCPR00F
003700021127     c+ JOIN NDBMV00F
003800021127     c+ ON  BMVSYS = CPRSYSDES
003900021127     c+ AND BMVNRASREG = CPRNRASDES
004000021127     c+ WHERE CPRANN = ' '
004100021127     c+ AND   CPRSYSPRO  = :Y36SYS
004200021127     c+ AND   CPRNRASPRO = :Y36NRASREG
004300021127     c+ AND   CPRNRRIPRO = :Y36NRRIGAM
004400021127     C+ AND   BMVSTATUS = ' '
004500021127     C/END-EXEC
004600021127      *          apertura cursore
004700021127     C/EXEC SQL
004800021127     C+ OPEN A1
004900021127     C/END-EXEC
005000021127     C                   DOU       SqlCod = 100 OR SqlCod < 0
005100021127      *          lettura cursore
005200021127     C/EXEC SQL
005300021127     C+ Fetch Next From A1 Into :nrrecord
005400021127     C/END-EXEC
005500021127      *
005600021127     C                   SELECT
005700021127     C                   WHEN      SqlCod = 100
005800021127      *
005900021127     C                   WHEN      SqlCod >= 0
006000021127     C                   Exsr      Update_Mov
006100021127      *
006200021127     C                   OTHER
006300021127     C                   EXSR      SqlError
006400021127     C                   ENDSL
006500021127      *
006600021127     C                   ENDDO
006700021127     C/EXEC SQL
006800021127     C+ Close A1
006900021127     C/END-EXEC
007000021127      *
007100021127     C                   Seton                                        RT
007200021127     C*---------------------------------------------------------------*
007300021127     C* Aggiorna il Movimento x nr. record sul file fisico
007400021127     C*---------------------------------------------------------------*
007500021127     C     Update_Mov    BEGSR
007600021127      *
007700021127     c     NrRecord      chain     ndbmv00f
007800021127      *  solo se c'è lo
007900021127      *    aggiorna
008000021127     c                   if        %found(ndbmv00f)
008100021127      *
008200021127     c                   eval      BMVnrTeso = Y36NRTESO
008300021127     c                   eval      BMVstatus = '1'
008400021127      *                           ----------
008500021127     c                   update    ndbmv000
008600021127      *                           ----------
008700021127     c                   end
008800021127      *
008900021127     C                   ENDSR
009000021127     C*---------------------------------------------------------------*
009100021127     C* per emissione errore su SQL
009200021127     C*---------------------------------------------------------------*
009300021127     C     SqlError      BEGSR
009400021127      *
009500021127      * Forzo la stampa del JOBLOG.
009600021127      *   e chiude il pgm forzatamente
009700021127     C                   CALL      'X66CHGJOB'
009800021127     C                   EVAL      *INLR = *ON
009900021127     C                   RETURN
010000021127      *
010100021127     C                   ENDSR
010200021127     C*---------------------------------------------------------------*
