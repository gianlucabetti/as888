000100050520     H DFTACTGRP(*NO)
000200050519
000300050519     D NuovaRichiesta  C                   '1'
000400050519     D DatiTrovati     C                   '2'
000500050519     D Continua        C                   '2'
000600050519     D EOF             C                   '9'
000700050519
000800050519     D tis7711r        PR
000900050520     D                                     EXTPGM('TIS7711R')
001000050519     D  a                          9999                                         Dati richiesta
001100050519     D  a                             1                                         Operazione
001200050519     D  s                             5S 0                                      Righe richieste
001300050519     D  a                          9999                                         Dati risposta
001400050519     D  s                             5S 0                                      Righe restituite
001500050519     D  a                             1                                         Esito
001600050519
001700050519     D tis1711dsi    E DS
001800050519     D                                     PREFIX(rqs)
001900050520     D                                     INZ
002000050520     D  ksu          E
002100050520     D                                     INZ('00430839')
002200050520     D  sun          E
002300050520     D                                     INZ(3130)
002400050520     D  dspin        E
002500050524     D                                     INZ('01.01.2004')
002600050520     D  dspfi        E
002700050524     D                                     INZ('31.12.2004')
002800050520     D  dft          E
002900050524     D                                     INZ('01.12.2004')
003000050524     D  nft          E
003100050524     D                                     INZ(36626)
003200050519     D tis173dso     E DS
003300050519     D                                     PREFIX(rpy)
003400050520     D                                     INZ
003500050519
003600050520     D righe           S
003700050520     D                                     LIKE(tis173dso)
003800050520     D                                     DIM(20)
003900050520
004000050520     D rqsdta          S           9999                                         Dati richiesta
004100050520     D operazione      S              1                                         Operazione
004200050520     D righeric        S              5S 0                                      Righe richieste
004300050520     D                                     INZ(20)
004400050520     D rpydta          S           9999                                         Dati risposta
004500050520     D righeout        S              5S 0                                      Righe restituite
004600050520     D esito           S              1                                         Esito
004700050520     D rmn             S
004800050520     D                                     LIKE(rqsrmn)
004900050520
005000050520     C     *ENTRY        PLIST
005100050520     C                   PARM                    rmn
005200050519
005300050519     ***********************************************************************************************
005400050519     **?
005500050519     **?Main.
005600050519     **?
005700050519     ***********************************************************************************************
005800050520     C                   EVAL      rqsrmn = rmn
005900050520     C                   EVAL      rqsdta = tis1711dsi
006000050520     C                   EVAL      operazione = NuovaRichiesta
006100050520     C                   CALLP     tis7711r(rqsdta:operazione:righeric:rpydta
006200050520     C                             :righeout:esito)
006300050520
006400050524     C                   IF        operazione = Continua
006500050524     C                   DO        49
006600050520     C                   CALLP     tis7711r(rqsdta:operazione:righeric:rpydta
006700050520     C                             :righeout:esito)
006800050524     C                   IF        operazione = EOF
006900050524     C                   LEAVE
007000050524     C                   ENDIF
007100050520     C                   ENDDO
007200050524     C                   ENDIF
007300050519
007400050520     C                   EXSR      uscita
007500050519
007600050519     ***********************************************************************************************
007700050519     **?
007800050519     **?Uscita.
007900050519     **?
008000050519     ***********************************************************************************************
008100050520     C     uscita        BEGSR
008200050519
008300050520     C                   EVAL      *INLR = *ON
008400050520     C                   RETURN
008500050519
008600050520     C                   ENDSR
