000100070221     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200070221
000300070221      //********************************************************************************************
000400070221      //
000500070221      // Definizione files database.
000600070221      //
000700070221      //********************************************************************************************
000800070222     Fazorg01l  IF   E           K DISK    USROPN
000900070222
001000070222      //********************************************************************************************
001100070222      //
001200070222      // Definizione costanti.
001300070222      //
001400070222      //********************************************************************************************
001500070222     D ESITO_ERRORE...
001600070222     D                 C                   -1
001700070222     D CODICE_OPERATIVO_ERRORE...
001800070222     D                 C                   'ERRORE'
001900070222     D CODICE_OPERATIVO_ELABORAZIONE_ESEGUITA...
002000070222     D                 C                   'DONE'
002100070222     D ERRORE_PARAMETRO_FIL...
002200070222     D                 C                   1
002300070222     D ERRORE_PARAMETRO_POSTEL...
002400070222     D                 C                   2
002500070222     D ERRORE_FIL_NONTROVATO...
002600070222     D                 C                   3
002700070221
002800070221      //********************************************************************************************
002900070221      //
003000070221      // Definizione strutture dati.
003100070221      //
003200070221      //********************************************************************************************
003300070222     D tibsPinfS     E DS                  QUALIFIED
003400070222     D prmAzorg00f   E DS                  EXTNAME(azorg00f) QUALIFIED
003500070222     D azorg00f      E DS                  INZ
003600070222
003700070222      //********************************************************************************************
003800070222      //
003900070222      // Dichiarazione parametri.
004000070222      //
004100070222      //********************************************************************************************
004200070222     C     *ENTRY        PLIST
004300070222     C                   PARM                    tibsPinfS
004400070222     C                   PARM                    prmAzorg00f                    Facoltativo
004500070222      /FREE
004600070222
004700070222       EXSR chkParametri;
004800070222       EXSR setInformazioni;
004900070222       EXSR uscita;
005000070222
005100070222       //*******************************************************************************************
005200070222       //
005300070222       // Controllo parametri ricevuti.
005400070222       //
005500070222       //*******************************************************************************************
005600070222       BEGSR chkParametri;
005700070222
005800070222         CLEAR tibsPinfS.rpyEsito;
005900070222         CLEAR tibsPinfS.rpyOpCode;
006000070222         CLEAR tibsPinfS.rpyStatus;
006100070222         CLEAR tibsPinfS.poInfo1;
006200070222         CLEAR tibsPinfS.poInfo2;
006300070222         CLEAR tibsPinfS.poInfo3;
006400070222         CLEAR tibsPinfS.poInfo4;
006500070222         CLEAR tibsPinfS.poInfo5;
006600070222         CLEAR tibsPinfS.poInfo6;
006700070222         CLEAR tibsPinfS.poInfo7;
006800070222         CLEAR tibsPinfS.poInfo8;
006900070222         CLEAR tibsPinfS.poInfo9;
007000070222
007100070222         IF tibsPinfS.fil = *ZERO
007200070222         OR (%PARMS() > 1 AND tibsPinfS.fil <> prmAzorg00f.orgFil);
007300070222           tibsPinfS.rpyEsito = ESITO_ERRORE;
007400070222           tibsPinfS.rpyOpCode = CODICE_OPERATIVO_ERRORE;
007500070222           tibsPinfS.rpyStatus = ERRORE_PARAMETRO_FIL;
007600070222           EXSR uscita;
007700070222         ENDIF;
007800070222
007900070222         SELECT;
008000070222           WHEN tibsPinfS.postel = *BLANK;
008100070222             tibsPinfS.postel = *OFF;
008200070222           WHEN tibsPinfS.postel <> *OFF AND tibsPinfS.postel <> *ON;
008300070222             tibsPinfS.rpyEsito = ESITO_ERRORE;
008400070222             tibsPinfS.rpyOpCode = CODICE_OPERATIVO_ERRORE;
008500070222             tibsPinfS.rpyStatus = ERRORE_PARAMETRO_POSTEL;
008600070222             EXSR uscita;
008700070222         ENDSL;
008800070222
008900070222       ENDSR;
009000070222
009100070222       //*******************************************************************************************
009200070222       //
009300070222       // Impostazione informazioni punto operativo.
009400070222       //
009500070222       //*******************************************************************************************
009600070222       BEGSR setInformazioni;
009700070222
009800070222         IF %PARMS() < 2;
009900070222           IF tibsPinfS.fil <> orgFil;
010000070222             IF NOT %OPEN(azorg01l);
010100070222               OPEN azorg01l;
010200070222             ENDIF;
010300070222             CHAIN tibsPinfS.fil azorg01l;
010400070222             IF NOT %FOUND;
010500070222               tibsPinfS.rpyEsito = ESITO_ERRORE;
010600070222               tibsPinfS.rpyOpCode = CODICE_OPERATIVO_ERRORE;
010700070222               tibsPinfS.rpyStatus = ERRORE_FIL_NONTROVATO;
010800070222               EXSR uscita;
010900070222             ENDIF;
011000070222           ENDIF;
011100070222         ELSE;
011200070222           IF %OPEN(azorg01l);
011300070222             CLOSE azorg01l;
011400070222           ENDIF;
011500070222           azorg00f = prmAzorg00f;
011600070222         ENDIF;
011700070222
011800070222         tibsPinfS.poInfo1 =
011900070301         'Per informazioni rivolgersi a:';
012000070222         tibsPinfS.poInfo2 = orgDes;
012100070222         tibsPinfS.poInfo3 = orgInd;
012200070222         tibsPinfS.poInfo4 = %EDITC(orgCpf:'X') + ' ' + %TRIMR(orgLoc) + ' '
012300070222         + orgPro;
012400070222         tibsPinfS.poInfo5 = 'Tel.   ' + orgTel;
012500070222         tibsPinfS.poInfo6 = 'Fax    ' + orgFax;
012600070222
012700070222         // E-mail: per Postel devo mettere un doppio '@', altrimenti si incazza di brutto.
012701161025
012800161025         tibsPinfS.poInfo7 = 'E-mail ' + orgDc1;
012900070222
013000070222         IF tibsPinfS.postel = *ON;
013100070222           tibsPinfS.poInfo7 = %TRIMR(tibsPinfS.poInfo7) + '@@';
013200070222         ELSE;
013300070222           tibsPinfS.poInfo7 = %TRIMR(tibsPinfS.poInfo7) + '@';
013400070222         ENDIF;
013500070222
013600120302         tibsPinfS.poInfo7 = %TRIMR(tibsPinfS.poInfo7) + 'brt.it';
013700070222
013800070222
013900070222       ENDSR;
014000070222
014100070222       //*******************************************************************************************
014200070222       //
014300070222       // Uscita dal programma.
014400070222       //
014500070222       //*******************************************************************************************
014600070222       BEGSR uscita;
014700070222         IF tibsPinfS.rpyOpCode = *BLANK;
014800070222           tibsPinfS.rpyOpCode = CODICE_OPERATIVO_ELABORAZIONE_ESEGUITA;
014900070222         ENDIF;
015000070222         *INLR = (tibsPinfS.setonlr = *ON);
015100070222         RETURN;
015200070222       ENDSR;
015300070222      /END-FREE
