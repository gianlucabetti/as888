000100030617     D Eseguito        C                   '0'
000200030617     D Errore          C                   '1'
000300030616     D RichiestaNuova  C                   '1'
000400030616     D RichiestaSuccessiva...
000500030616     D                 C                   '2'
000600030616     D FineDati        C                   '9'
000700030616     D Elementi        C                   20
000800030617
000900030617     D NullPtr         S               *
001000030617     D TiOri00F      E DS
001100030617     D                                     BASED(NullPtr)
001200030617     D FnOrm00F      E DS
001300030617     D                                     BASED(NullPtr)
001400030617     D DFAR          E DS
001500030617     D                                     BASED(NullPtr)
001600030617
001700030616     D TIO1G2IDS     E DS
001800030616     D                                     PREFIX(Inp)
001900040729
002000040729     D TIO1G2PDS     E DS
002100040729     D                                     PREFIX(Pag)
002200040729     D  RSR                          30
002300040729     D                                     DIM(Elementi)
002400040729     D                                     OVERLAY(PagRsr)
002500040729     D  RSC                          30
002600040729     D                                     DIM(Elementi)
002700040729     D                                     OVERLAY(PagRsc)
002800040729     D  DER                          10
002900040729     D                                     DIM(Elementi)
003000040729     D                                     OVERLAY(PagDer)
003100040729     D  DAR                          10
003200040729     D                                     DIM(Elementi)
003300040729     D                                     OVERLAY(PagDar)
003400040729     D  RFA
003500040729     D                                     LIKE(OrmRfa)
003600040729     D                                     DIM(Elementi)
003700040729     D                                     OVERLAY(PagRfa)
003800040729     D  ORM                          14S 0
003900040729     D                                     DIM(Elementi)
004000040729     D                                     OVERLAY(PagOrm)
004100040729     D  FARDES
004200040729     D                                     LIKE(D§FARDES)
004300040729     D                                     DIM(Elementi)
004400040729     D                                     OVERLAY(PagFarDes)
004500030616
004600030616     D Operazione      S              1
004700030616     D RigheOut        S              5S 0
004800030616     D RigheRic        S              5S 0
004900030616     D Esito           S              1
005000030616
005100030616     ***********************************************************************
005200030616     **
005300030616     ** Main
005400030616     **
005500030616     ***********************************************************************
005600030619     C                   EVAL      InpRQSCID = *ZERO
005700040729     C                   EVAL      InpKSC    = '00892318'
005800040729     C                   EVAL      InpRSR    = 'tlc spa'
005900040729     C*                  EVAL      InpRSC    = 'PANORAMA'
006000040729     C                   EVAL      InpDERI   = '01.01.0001'
006100040729     C                   EVAL      InpDERF   = '31.12.9999'
006200040729     C                   EVAL      InpDARI   = '01.01.0001'
006300040729     C                   EVAL      InpDARF   = '31.12.9999'
006400040729     C*                  EVAL      InpRFA    = '         N12245'
006500030617     C                   EVAL      InpNRIGHE = Elementi
006600030617
006700030617     C                   EVAL      Operazione = RichiestaNuova
006800030617     C                   EXSR      CalTIO7G2R
006900030617
007000030617     C                   EVAL      Operazione = RichiestaSuccessiva
007100030617     C                   DO        49
007200030617     C                   EXSR      CalTIO7G2R
007300030617     C                   IF        Operazione = FineDati
007400030617     C                   LEAVE
007500030617     C                   ENDIF
007600030617     C                   ENDDO
007700030616
007800030617     C                   EXSR      Uscita
007900030617
008000030617     ***********************************************************************
008100030617     **
008200030617     **
008300030617     **
008400030617     ***********************************************************************
008500030617     C     CalTIO7G2R    BEGSR
008600030617
008700030617     C                   CLEAR                   TIO1G2PDS
008800030617
008900030619     C                   CALL      'TIO7G21R'
009000030617     C                   PARM                    TIO1G2IDS
009100030617     C                   PARM                    Operazione
009200030617     C                   PARM      InpNRighe     RigheRic
009300030617     C                   PARM                    TIO1G2PDS
009400030617     C                   PARM      0             RigheOut
009500030617     C                   PARM      *BLANK        Esito
009600030617
009700030617     C                   IF        Esito = Errore OR
009800030617     C                             Operazione = FineDati
009900030617     C                   EXSR      Uscita
010000030617     C                   ENDIF
010100030617
010200030617     C                   ENDSR
010300030617
010400030617     ***********************************************************************
010500030617     **
010600030617     ** Operazioni finali.
010700030617     **
010800030617     ***********************************************************************
010900030617     C     Uscita        BEGSR
011000030617
011100030617     C                   EVAL      *INLR = (Operazione = FineDati)
011200030617     C                   RETURN
011300030617
011400030617     C                   ENDSR
