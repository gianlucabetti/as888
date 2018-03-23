000100030717      *PARMS DFTACTGRP(*NO) ACTGRP(QILE) BNDDIR(TIO7)
000200030717      *PARMS DYNUSRPRF(*OWNER) TGTRLS(*CURRENT)
000300030717      *PARMS
000400030717      *PARMS
000500030717      *PARMS
000600030717
000700030718     FTiOri03l  IF   E           K DISK
000800030718     FFnOrm01l  IF   E           K DISK
000900030717
001000030717     D A14             S             14
001100030717     D Esito           S              1
001200030717     D TIO1G1IDS     E DS                  PREFIX(Inp)
001300030717     D TIO1G1ODS     E DS                  PREFIX(Out)
001400030717
001500030718     d KriRfa          S                   LIKE(VaoRfa)
001600030717     d KrmPoe          S                   LIKE(OrmPoe)
001700030717     d KrmNsr          S                   LIKE(OrmNsr)
001800030717     d KrmNor          S                   LIKE(OrmNor)
001900030717     d KrmNrv          S                   LIKE(OrmNrv)
002000000000     c*--------------------------------------------------------------------------------------------*
002100000000     c* Main lines
002200000000     c*--------------------------------------------------------------------------------------------*
002300000000     c*
002400000000     c* operazioni iniziali -da fare sempre-
002500000000     c                   EXSR      rutinz
002600030717     c*
002700030717     c* controlla esistenza del riferimento ORM immesso in TIORI
002800030717     c                   EVAL      KriRfa = InpO1GRfa
002900030722     C     KeyOri        CHAIN     TiOri03L                           98
003000030722if  1C                   IF        *in98 OR
003100030722     c                             oriidc <> '00000000'
003200030717     C                   EVAL      Esito = '1'
003300030717     c*
003400030717     c                   ELSE
003500030717     c*
003600030717     c* controlla esistenza del riferimento ORM immesso in FNORM
003700030717     C                   EVAL      KrmPoe = OriPoe
003800030717     C                   EVAL      KrmNsr = OriNsr
003900030717     C                   EVAL      KrmNor = OriNor
004000030717     C                   EVAL      KrmNrv = OriNrv
004100030718     C     KeyOrm        CHAIN     FnOrm01L
004200030717if  2C                   IF        NOT %FOUND
004300030717     C                   EVAL      Esito = '1'
004400030717     c*
004500030717     c                   ELSE
004600030718     C                   EVAL      a14 =   %EDITC(OriPoe:'X')
004700030718     C                                   + %EDITC(OriNsr:'X')
004800030718     C                                   + %EDITC(OriNor:'X')
004900030718     C                                   + %EDITC(OriNrv:'X')
005000030718     C                   MOVEL     a14           OutO1GOrm
005100030717e   2c                   ENDIF
005200030717e   1C                   ENDIF
005300000000     c*
005400000000     C                   RETURN
005500030718     C*--------------------------------------------------------------------------------------------*
005600030718     C* operazioni iniziali -da fare sempre-
005700030718     C*--------------------------------------------------------------------------------------------*
005800030718     C     rutinz        BEGSR
005900030718     C*
006000030718     C* azzera schiere di memorizzazione
006100030722     C                   MOVEL     '0'           Esito
006200030718     C                   CLEAR                   TIO1G1ODS
006300030718     C*
006400030718     C                   ENDSR
006500000000     c*--------------------------------------------------------------------------------------------*
006600000000     c* Operazioni iniziali -solo la prima volta-
006700000000     c*--------------------------------------------------------------------------------------------*
006800000000     C     *InzSr        BEGSR
006900000000     c*
007000000000     c* ricevimento parametri
007100000000     c     *ENTRY        PLIST
007200030717     C                   PARM                    Esito
007300030718     c                   PARM                    TIO1G1IDS
007400030718     c                   PARM                    TIO1G1ODS
007500000000     c*
007600030717     C     KeyOri        KLIST
007700030717     C                   KFLD                    KriRfa
007800030717     C     KeyOrm        KLIST
007900030717     C                   KFLD                    KrmPoe
008000030717     C                   KFLD                    KrmNsr
008100030717     C                   KFLD                    KrmNor
008200030717     C                   KFLD                    KrmNrv
008300000000     c*
008400000000     c                   ENDSR
