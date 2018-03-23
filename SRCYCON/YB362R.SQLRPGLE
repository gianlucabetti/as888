000100021127      /TITLE RICONCILIAZIONE MANUALE MOVIMENTI BANCA
000200021127      *PARMS DFTACTGRP(*NO) ACTGRP(*CALLER)
000300021127      *PARMS COMMIT(*NONE) DATFMT(*ISO)
000400021127      *PARMS DYNUSRPRF(*OWNER)
000500021127     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000600021127      *---------------------------------------------------------------------
000700021127      *
000800021128     Fndbmb00f  UF   E             DISK     commit
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
002200021128     c+ SELECT RRN(B)
002300021128     c+ FROM
002400021128     c+ ((NDCPR00F JOIN NDBMV00F
002500021128     c+ ON  BMVSYS     = CPRSYSDES
002600021128     c+ AND BMVNRASREG = CPRNRASDES)
002700021128     c+ JOIN NDBMB00F A
002800021128     c+ ON  BMVSOCIETA = A.BMBSOCIETA
002900021128     c+ AND BMVBANCA   = A.BMBBANCA
003000021128     c+ AND BMVLINEA   = A.BMBLINEA
003100021128     c+ AND BMVNRTESO  = A.BMBNRRIC)
003200021128     c+ JOIN NDBMB00F B
003300021128     c+ ON  A.BMBSOCIETA = B.BMBSOCIETA
003400021128     c+ AND A.BMBBANCA   = B.BMBBANCA
003500021128     c+ AND A.BMBDTOPE   = B.BMBDTOPE
003600021128     c+ AND A.BMBDTVAL   = B.BMBDTVAL
003700021128     c+ AND A.BMBIMPORTO = B.BMBIMPORTO
003800021128     c+ WHERE CPRANN = ' '
003900021128     c+ AND   CPRSYSPRO  = :Y36SYS
004000021128     c+ AND   CPRNRASPRO = :Y36NRASREG
004100021128     c+ AND   CPRNRRIPRO = :Y36NRRIGAM
004200021128     c+ AND   A.BMBSTATUS = '1'
004300021128     c+ AND   B.BMBSTATUS = ' '
004400021128     c+ AND ((B.BMBLINEA <> 'CCO' AND A.BMBDARE = B.BMBDARE
004500021128     c+       AND B.BMBCAUBAN <> '  Z1')
004600021128     c+      OR
004700021128     c+      (B.BMBLINEA <> 'CCO' AND A.BMBDARE <> B.BMBDARE
004800021128     c+       AND B.BMBCAUBAN = '  Z1'))
004900021128     c+ UNION
005000021128     c+ SELECT RRN(B)
005100021128     c+ FROM
005200021128     c+ ((NDCPR00F JOIN NDBMV00F
005300021128     c+ ON  BMVSYS     = CPRSYSPRO
005400021128     c+ AND BMVNRASREG = CPRNRASPRO
005500021128     c+ AND BMVNRRIGAM = CPRNRRIPRO)
005600021128     c+ JOIN NDBMB00F A
005700021128     c+ ON  BMVSOCIETA = A.BMBSOCIETA
005800021128     c+ AND BMVBANCA   = A.BMBBANCA
005900021128     c+ AND BMVLINEA   = A.BMBLINEA
006000021128     c+ AND BMVNRTESO  = A.BMBNRRIC)
006100021128     c+ JOIN NDBMB00F B
006200021128     c+ ON  A.BMBSOCIETA = B.BMBSOCIETA
006300021128     c+ AND A.BMBBANCA   = B.BMBBANCA
006400021128     c+ AND A.BMBDTOPE   = B.BMBDTOPE
006500021128     c+ AND A.BMBDTVAL   = B.BMBDTVAL
006600021128     c+ AND A.BMBIMPORTO = B.BMBIMPORTO
006700021128     c+ WHERE CPRANN = ' '
006800021128     c+ AND   CPRSYSPRO  = :Y36SYS
006900021128     c+ AND   CPRNRASPRO = :Y36NRASREG
007000021128     c+ AND   CPRNRRIPRO = :Y36NRRIGAM
007100021128     c+ AND   A.BMBSTATUS = '1'
007200021128     c+ AND   B.BMBSTATUS = ' '
007300021128     c+ AND ((B.BMBLINEA = 'CCO' AND A.BMBDARE = B.BMBDARE)
007400021128     c+      OR
007500021128     c+      (B.BMBLINEA <> 'CCO' AND A.BMBDARE <> B.BMBDARE
007600021128     c+       AND B.BMBCAUBAN = '  Z1'))
007700021127     C/END-EXEC
007800021127      *          apertura cursore
007900021127     C/EXEC SQL
008000021127     C+ OPEN A1
008100021127     C/END-EXEC
008200021127     C                   DOU       SqlCod = 100 OR SqlCod < 0
008300021127      *          lettura cursore
008400021127     C/EXEC SQL
008500021127     C+ Fetch Next From A1 Into :nrrecord
008600021127     C/END-EXEC
008700021127      *
008800021127     C                   SELECT
008900021127     C                   WHEN      SqlCod = 100
009000021127      *
009100021127     C                   WHEN      SqlCod >= 0
009200021128     C                   Exsr      Update_MovB
009300021127      *
009400021127     C                   OTHER
009500021127     C                   EXSR      SqlError
009600021127     C                   ENDSL
009700021127      *
009800021127     C                   ENDDO
009900021127     C/EXEC SQL
010000021127     C+ Close A1
010100021127     C/END-EXEC
010200021127      *
010300021127     C                   Seton                                        RT
010400021127     C*---------------------------------------------------------------*
010500021127     C* Aggiorna il Movimento x nr. record sul file fisico
010600021127     C*---------------------------------------------------------------*
010700021128     C     Update_MovB   BEGSR
010800021127      *
010900021128     c     NrRecord      chain     ndbmb00f
011000021127      *  solo se c'è lo
011100021127      *    aggiorna
011200021128     c                   if        %found(ndbmb00f)
011300021127      *
011400021128     c                   eval      BMBnrRIC  = Y36NRTESO
011500021128     c                   eval      BMBstatus = '1'
011600021127      *                           ----------
011700021128     c                   update    ndbmb000
011800021127      *                           ----------
011900021127     c                   end
012000021127      *
012100021127     C                   ENDSR
012200021127     C*---------------------------------------------------------------*
012300021127     C* per emissione errore su SQL
012400021127     C*---------------------------------------------------------------*
012500021127     C     SqlError      BEGSR
012600021127      *
012700021127      * Forzo la stampa del JOBLOG.
012800021127      *   e chiude il pgm forzatamente
012900021127     C                   CALL      'X66CHGJOB'
013000021127     C                   EVAL      *INLR = *ON
013100021127     C                   RETURN
013200021127      *
013300021127     C                   ENDSR
013400021127     C*---------------------------------------------------------------*
