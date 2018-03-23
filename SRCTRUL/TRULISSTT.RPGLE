000100080627     H DFTACTGRP(*NO) ACTGRP('TRULISST') BNDDIR('TRUL')
000200080627      //********************************************************************************************
000300080627      //
000400080627      //  X TESTARE il TRULISST
000500080627      //
000600080627      //********************************************************************************************
000700080627
000800080627      //********************************************************************************************
000900080627      //
001000080627      // Definizione procedure esterne.
001100080627      //
001200080627      //********************************************************************************************
001300080627
001400080627     D/COPY GAITRASRC/SRCPROTOPR,TRULISST
001500080627
001600080627      //********************************************************************************************
001700080627      //
001800080627      // Definizione variabili globali.
001900080627      //
002000080627      //********************************************************************************************
002100080711     D stringa         S           1250A   INZ('Fiat Croma targata BK865YB ricc-
002200080711     D                                     amente accessoriata con climatizzato-
002300080711     D                                     re, cerchi in lega, navigatore satel-
002400080711     d                                     litare, airbag, ABS, ESP, vernice me-
002500080711     d                                     tallizzata, sedili in pelle e tante -
002600080711     d                                     altre pugnette assolutamente inutili-
002700080711     d                                     .')
002800081209     D stringa200      S            200A   INZ('1aaaaaaaaaaaaaaaaaaaaaaaaaaaaa-
002900081209     D                                     aaaaaaaaaaaaaaaaaaa01bbbbbbbbb-
003000081209     D                                     bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb-
003100081209     D                                     bbbbbbbbb01ccccccccccccccccccc-
003200081209     D                                     ccccccccccccccccccccccccccccc0-
003300081209     D                                     1ddddddddddddddddddddddddddddd-
003400081209     D                                     ddddddddddddddddddd0')
003500081209     D sottostringa    S             50A
003600080627     D PosizioneProgressiva...
003700080627     D                 S             10I 0
003800080627     D esito           S             10I 0
003900080627
004000080627      //********************************************************************************************
004100080627      //
004200080627      // Main.
004300080627      //
004400080627      //********************************************************************************************
004500080627      /FREE
004600081209
004700080627       *INLR = *ON;
004800081209
004900080627       IF ISST_init() < 0;
005000080627        RETURN;
005100080627       ENDIF;
005200080627
005300080627       IF ISST_newStringa( Stringa : %Size(Stringa) ) < 0;
005400080627        RETURN;
005500080627       ENDIF;
005600080627
005700080708       DOU Sottostringa = *blank;
005800080708
005900080708          Sottostringa = ISST_getSottoStringa( %Size(Sottostringa)
006000080714                                             : PosizioneProgressiva
006100080708                                             );
006200080709          If Sottostringa <> *blank;
006300080709             DSPLY sottostringa;
006400080714             DSPLY PosizioneProgressiva;
006500080709          EndIf;
006600080708
006700080708       ENDdo;
006800081209
006900081209       IF ISST_newStringa( stringa200 : %Size(stringa200) ) < 0;
007000081209        RETURN;
007100081209       ENDIF;
007200081209
007300081209       DOU posizioneProgressiva = %LEN(stringa200);
007400081209
007500081209          Sottostringa = ISST_getSottoStringa( %Size(Sottostringa)
007600081209                                             : PosizioneProgressiva
007700081209                                             );
007800081209          If Sottostringa <> *blank;
007900081209             DSPLY sottostringa;
008000081209             DSPLY PosizioneProgressiva;
008100081209          EndIf;
008200081209
008300081209       ENDdo;
008400080708
008500080627       ISST_finalize();
008600080627
008700080627      /END-FREE
