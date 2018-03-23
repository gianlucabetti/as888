000100060626     H DFTACTGRP(*NO) BNDDIR('TIO7')
000200030717
000300030718     FTiOri03l  IF   E           K DISK
000400030718     FFnOrm01l  IF   E           K DISK
000500030717
000600030717     D A14             S             14
000700030717     D Esito           S              1
000800030717     D TIO1G1IDS     E DS                  PREFIX(Inp)
000900030717     D TIO1G1ODS     E DS                  PREFIX(Out)
001000030717
001100030718     d KriRfa          S                   LIKE(VaoRfa)
001200030717     d KrmPoe          S                   LIKE(OrmPoe)
001300030717     d KrmNsr          S                   LIKE(OrmNsr)
001400030717     d KrmNor          S                   LIKE(OrmNor)
001500030717     d KrmNrv          S                   LIKE(OrmNrv)
001600000000     c*--------------------------------------------------------------------------------------------*
001700000000     c* Main lines
001800000000     c*--------------------------------------------------------------------------------------------*
001900000000     c*
002000000000     c* operazioni iniziali -da fare sempre-
002100000000     c                   EXSR      rutinz
002200030717     c*
002300030717     c* controlla esistenza del riferimento ORM immesso in TIORI
002400030717     c                   EVAL      KriRfa = InpO1GRfa
002500030722     C     KeyOri        CHAIN     TiOri03L                           98
002600030722if  1C                   IF        *in98 OR
002700030722     c                             oriidc <> '00000000'
002800030717     C                   EVAL      Esito = '1'
002900030717     c*
003000030717     c                   ELSE
003100030717     c*
003200030717     c* controlla esistenza del riferimento ORM immesso in FNORM
003300030717     C                   EVAL      KrmPoe = OriPoe
003400030717     C                   EVAL      KrmNsr = OriNsr
003500030717     C                   EVAL      KrmNor = OriNor
003600030717     C                   EVAL      KrmNrv = OriNrv
003700030718     C     KeyOrm        CHAIN     FnOrm01L
003800030717if  2C                   IF        NOT %FOUND
003900030717     C                   EVAL      Esito = '1'
004000030717     c*
004100030717     c                   ELSE
004200030718     C                   EVAL      a14 =   %EDITC(OriPoe:'X')
004300030718     C                                   + %EDITC(OriNsr:'X')
004400030718     C                                   + %EDITC(OriNor:'X')
004500030718     C                                   + %EDITC(OriNrv:'X')
004600030718     C                   MOVEL     a14           OutO1GOrm
004700030717e   2c                   ENDIF
004800030717e   1C                   ENDIF
004900000000     c*
005000000000     C                   RETURN
005100030718     C*--------------------------------------------------------------------------------------------*
005200030718     C* operazioni iniziali -da fare sempre-
005300030718     C*--------------------------------------------------------------------------------------------*
005400030718     C     rutinz        BEGSR
005500030718     C*
005600030718     C* azzera schiere di memorizzazione
005700030722     C                   MOVEL     '0'           Esito
005800030718     C                   CLEAR                   TIO1G1ODS
005900030718     C*
006000030718     C                   ENDSR
006100000000     c*--------------------------------------------------------------------------------------------*
006200000000     c* Operazioni iniziali -solo la prima volta-
006300000000     c*--------------------------------------------------------------------------------------------*
006400000000     C     *InzSr        BEGSR
006500000000     c*
006600000000     c* ricevimento parametri
006700000000     c     *ENTRY        PLIST
006800030717     C                   PARM                    Esito
006900030718     c                   PARM                    TIO1G1IDS
007000030718     c                   PARM                    TIO1G1ODS
007100000000     c*
007200030717     C     KeyOri        KLIST
007300030717     C                   KFLD                    KriRfa
007400030717     C     KeyOrm        KLIST
007500030717     C                   KFLD                    KrmPoe
007600030717     C                   KFLD                    KrmNsr
007700030717     C                   KFLD                    KrmNor
007800030717     C                   KFLD                    KrmNrv
007900000000     c*
008000000000     c                   ENDSR
