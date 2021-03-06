000100120223     ***************************************************************************
000200120223     **
000300120224     ** Tester di TIS7ST20R.
000400120223     **
000500120223     ***************************************************************************
000600120224     H DFTACTGRP(*NO) ACTGRP(*NEW)
000700120223
000800120223     ***************************************************************************
000900120223     **
001000120223     ** Costanti.
001100120223     **
001200120223     ***************************************************************************
001300120223     D ESITO_ERROR...
001400120223     D                 C                   -1
001500120223     D ESITO_OK...
001600120223     D                 C                   0
001700120223     D RPYOPCODE_DONE...
001800120223     D                 C                   'DONE'
001900120223     D RPYOPCODE_ERROR...
002000120223     D                 C                   'ERROR'
002100120223     D RQSOPCODE_GETKSUFAMI...
002200120223     D                 C                   'GETKSUFAMI'
002300120223
002400120223     ***************************************************************************
002500120223     **
002600120223     ** Strutture dati.
002700120223     **
002800120223     ***************************************************************************
002900120223     D tis7st20i     E DS                  INZ(*EXTDFT) QUALIFIED
003000120223     D tis7st20o     E DS                  INZ(*EXTDFT) QUALIFIED
003100120223     D  idCliente_ary                 7A   DIM(99) OVERLAY(idCliente)
003200120224     D  dzCliente_ary                79A   DIM(99) OVERLAY(dzCliente)
003300120223
003400120223     ***************************************************************************
003500120223     **
003600120223     ** Campi.
003700120223     **
003800120223     ***************************************************************************
003900120223     D rqsOpCode       S             10A
004000120223     D rqsData         S           9999A
004100120223     D rqsDataLen      S              4P 0
004200120223     D rpyOpCode       S             10A
004300120223     D rpyData         S           9999A
004400120223     D rpyDataLen      S              4P 0
004500120223
004600120223     ***************************************************************************
004700120223     **
004800120223     ** Prototipi.
004900120223     **
005000120223     ***************************************************************************
005100120224     D Tis7st20r       PR                  EXTPGM('TIS7ST20R')
005200120224     D  rqsOpCode                    10A
005300120224     D  rqsData                    9999A
005400120224     D  rqsDataLen                    4P 0
005500120224     D  rpyOpCode                    10A
005600120224     D  rpyData                    9999A
005700120224     D  rpyDataLen                    4P 0
005800120223
005900120223     ***************************************************************************
006000120223     **
006100120223     ** Main.
006200120223     **
006300120223     ***************************************************************************
006400120223
006500120223      /FREE
006600120224
006700120224       *INLR = *ON;
006800120224
006900120224       rqsOpCode = RQSOPCODE_GETKSUFAMI;
007000120224
007100120224       RESET tis7st20i;
007200120224       tis7st20i.ksu = '00430839';
007300120224       tis7st20i.sun = 7;
007400120224
007500120224       DOU tis7st20o.datiFiniti = *ON;
007600120224
007700120224         rqsData = tis7st20i;
007800120224         rqsDataLen = %SIZE(tis7st20i);
007900120224
008000120224         Tis7st20r( rqsOpCode : rqsData : rqsDataLen
008100120224                  : rpyOpCode : rpyData : rpyDataLen
008200120224                  );
008300120224
008400120224         IF rpyOpCode = RPYOPCODE_ERROR;
008500120224           DUMP(A);
008600120224           LEAVE;
008700120224         ENDIF;
008800120224
008900120224         tis7st20o = rpyData;
009000120224
009100120224         IF tis7st20o.datiFiniti = *ON;
009200120224           LEAVE;
009300120224         ENDIF;
009400120224
009500120224         tis7st20i.usrSpcName = tis7st20o.usrSpcName;
009600120224         tis7st20i.prgCall += 1;
009700120224
009800120224       ENDDO;
009900120224
010000120224       RESET tis7st20i;
010100120224       tis7st20i.ksu = '00390537';
010200120224       tis7st20i.sun = 5112;
010300120224
010400120224       DOU tis7st20o.datiFiniti = *ON;
010500120224
010600120224         rqsData = tis7st20i;
010700120224         rqsDataLen = %SIZE(tis7st20i);
010800120224
010900120224         Tis7st20r( rqsOpCode : rqsData : rqsDataLen
011000120224                  : rpyOpCode : rpyData : rpyDataLen
011100120224                  );
011200120224
011300120224         IF rpyOpCode = RPYOPCODE_ERROR;
011400120224           DUMP(A);
011500120224           LEAVE;
011600120224         ENDIF;
011700120224
011800120224         tis7st20o = rpyData;
011900120224
012000120224         IF tis7st20o.datiFiniti = *ON;
012100120224           LEAVE;
012200120224         ENDIF;
012300120224
012400120224         tis7st20i.usrSpcName = tis7st20o.usrSpcName;
012500120224         tis7st20i.prgCall += 1;
012600120224
012700120224       ENDDO;
012800120224
012900120224      /END-FREE
