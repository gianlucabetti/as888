000100030307      *PARMS DFTACTGRP(*YES)
000200030305      *PARMS COMMIT(*NONE) DATFMT(*ISO)
000300030305      *PARMS CLOSQLCSR(*ENDMOD)
000400030305      *PARMS DYNUSRPRF(*OWNER)
000500030305
000600030305      /TITLE Pulizia anagrafico trazionisti TNTLZ00F.
000700030305
000800030305     D KPJBA         E DS
000900030307     D WrkDTrMin       S              8S 0
001000030307     D WrkDtCur        S               D
001100030307
001200030307     D PSDS           SDS
001300030307     D  PSJobName            244    253                                         Nome lavoro
001400030307     D  PSJobUser            254    263                                         Utente
001500030307     D  PSJobNbr             264    269                                         Numero lavoro
001600030307
001700030307     D SndMg01         S             10                                         Message type
001800030307     D                                     INZ('*INFO')
001900030307     D SndMg02         S             10                                         Deluvery mode
002000030311     D                                     INZ('*NORMAL')
002100030307     D SndMg03         S            256                                         Message text
002200030307     D SndMg04         S              9B 0                                      Length of text
002300030307     D                                     INZ(%SIZE(SndMg03))
002400030307     D SndMg05         S             10                                         User profile
002500030307     D                                     DIM(299)
002600030307     D SndMg06         S              9B 0                                      Number of user
002700030307     D SndMg07         S              9B 0                                      Message sent indic.
002800030307     D SndMg08         S              9B 0                                      Function requested
002900030307     D SndMg10         S              1                                         Show display
003000030307     D                                     INZ('N')
003100030307     D SndMg11         S             20                                         Qualified MSGQ name
003200030307     D SndMg12         S              4                                         Name type indicator
003300030307     D                                     INZ('*USR')
003400030307     D/COPY QSYSINC/QRPGLESRC,QUSEC
003500030305
003600030305     C     *ENTRY        PLIST
003700030305     C                   PARM                    KPJBA
003800030307
003900030307     C/EXEC SQL
004000030307     C+ WHENEVER SQLERROR GOTO SQLInErrore
004100030307     C/END-EXEC
004200030305
004300030305     C                   EXSR      AnnTnTlz
004400030311     C                   EXSR      DltTnTlz
004500030310
004600030307     C                   GOTO      SQLEseguito
004700030307     C     SQLInErrore   TAG
004800030307     C                   EVAL      *INH1 = *ON
004900030307     C     SQLEseguito   TAG
005000030305     C                   EXSR      Uscita
005100030305
005200030305     ***********************************************************************
005300030305     **
005400030311     ** Annullamento codici non più usati.
005500030305     **
005600030305     ***********************************************************************
005700030305     C     AnnTnTlz      BEGSR
005800030307
005900030307     ** Per annullare il record la data trasmissione deve essere
006000030307     ** passata da almeno 1 mese, in questo modo evito di annullare
006100030307     ** i records appena immessi.
006200030307     C                   TIME                    WrkDtCur
006300030307     C                   SUBDUR    1:*M          WrkDtCur
006400030307     C                   MOVE      WrkDtCur      WrkDTrMin
006500030305
006600030307     C/EXEC SQL
006700030307     C+ UPDATE TNTLZ00F SET TLZATB = '*', TLZFTR = ' ', TLZDTR
006800030307     C+ = 0 WHERE TLZTIP = 'T' AND TLZATB = ' ' AND TLZFTR = 'T' AND
006900030307     C+ TLZDTR < :WrkDTrMin AND TLZPDR NOT IN (SELECT DISTINCT FVSPDR
007000030307     C+ FROM TNFVS00F) AND TLZPDR NOT IN (SELECT DISTINCT TSTPDR FROM
007100030307     C+ TNTST00F)
007200030307     C/END-EXEC
007300030311
007400030311     C                   EVAL      SndMg03 = 'Pulizia anagrafico trazionisti: '
007500030311     C                             + 'annullati '
007600030311     C                             + %TRIML(%EDITC(SQLEr3:'3'))
007700030311     C                             + ' codici non usati.'
007800030311     C                   EXSR      CallQEZSNDMG
007900030311
008000030311     ** Trasmetto i records aggiornati.
008100030311     C                   CALL      'TRTR65R'
008200030311     C                   PARM                    KPJBA
008300030311
008400030311     C                   EVAL      SndMg03 = 'Pulizia anagrafico trazionisti: '
008500030311     C                             + 'sottomessa trasmissione dei records '
008600030311     C                             + 'annullati.'
008700030311     C                   EXSR      CallQEZSNDMG
008800030307
008900030305     C                   ENDSR
009000030311
009100030311     ***********************************************************************
009200030311     **
009300030311     ** Cancellazione codici non più usati, annullati e trasmessi.
009400030311     **
009500030311     ***********************************************************************
009600030311     C     DltTnTlz      BEGSR
009700030311
009800030311     C/EXEC SQL
009900030311     C+ DELETE FROM TNTLZ00F WHERE TLZTIP = 'T' AND TLZATB = '*'
010000030311     C+ AND TLZFTR = 'T' AND TLZPDR NOT IN (SELECT DISTINCT FVSPDR FROM
010100030311     C+ TNFVS00F) AND TLZPDR NOT IN (SELECT DISTINCT TSTPDR FROM TNTST00F)
010200030311     C/END-EXEC
010300030311
010400030311     C                   EVAL      SndMg03 = 'Pulizia anagrafico trazionisti: '
010500030311     C                             + 'cancellati '
010600030311     C                             + %TRIML(%EDITC(SQLEr3:'3'))
010700030311     C                             + ' codici non usati.'
010800030311     C                   EXSR      CallQEZSNDMG
010900030311
011000030311     C                   ENDSR
011100030307
011200030307     ***********************************************************************
011300030307     **
011400030307     **
011500030307     **
011600030307     ***********************************************************************
011700030307     C     CallQEZSNDMG  BEGSR
011800030307
011900030307     C                   EVAL      SndMg05(1) = PSJobUser
012000030307     C                   EVAL      SndMg06 = 1
012100030307     C                   CLEAR                   Qusec
012200030307     C                   EVAL      Qusbprv = %SIZE(Qusec)
012300030307
012400030307     C                   CALL      'QEZSNDMG'
012500030307     C                   PARM                    SndMg01
012600030307     C                   PARM                    SndMg02
012700030307     C                   PARM                    SndMg03
012800030307     C                   PARM                    SndMg04
012900030307     C                   PARM                    SndMg05
013000030307     C                   PARM                    SndMg06
013100030307     C                   PARM                    SndMg07
013200030307     C                   PARM                    SndMg08
013300030307     C                   PARM                    Qusec
013400030307     C                   PARM                    SndMg10
013500030307     C                   PARM                    SndMg11
013600030307     C                   PARM                    SndMg12
013700030307
013800030307     C                   ENDSR
013900030305
014000030305     ***********************************************************************
014100030305     **
014200030305     **
014300030305     **
014400030305     ***********************************************************************
014500030305     C     Uscita        BEGSR
014600030305
014700030305     C                   EVAL      *INLR = *ON
014800030305     C                   RETURN
014900030305
015000030305     C                   ENDSR
015100030305
015200030305     ***********************************************************************
015300030305     **
015400030305     **
015500030305     **
015600030305     ***********************************************************************
015700030305     C     *INZSR        BEGSR
015800030305
015900030305
016000030305     C                   ENDSR
