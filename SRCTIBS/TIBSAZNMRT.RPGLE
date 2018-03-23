000100060906     H DFTACTGRP(*NO) BNDDIR('TIBS')
000200060906
000300060906      //********************************************************************************************
000400060906      //
000500060906      // Definizione costanti.
000600060906      //
000700060906      //********************************************************************************************
000800060906     D Giorno          C                   'G'
000900060906     D Settimana       C                   'S'
001000060906     D Mese            C                   'M'
001100060906     D AnnoSolare      C                   'A'
001200060906     D EsercizioCG     C                   'E'
001300060906     D Progressivo     C                   'P'
001400060906     D Assoluto        C                   'T'
001500060906     D DiGruppo        C                   'G'
001600060906     D DiSocieta       C                   'S'
001700060906     D POTutti         C                   '0'
001800060906     D POLivello1      C                   '1'
001900060906     D POLivello2      C                   '2'
002000060906     D Numerico        C                   'N'
002100060906     D Carattere       C                   'C'
002200060906     D SoloNumeri      C                   'N'
002300060906     D SoloLettere     C                   'L'
002400060906     D NumeriLettere   C                   'T'
002500060906
002600060906      //********************************************************************************************
002700060906      //
002800060906      // Definizione strutture dati.
002900060906      //
003000060906      //********************************************************************************************
003100060906     D aznmrr00f     E DS                  INZ PREFIX(r)
003200060907     D aznmrn00f     E DS                  INZ PREFIX(n)
003300060906
003400060906      //********************************************************************************************
003500060906      //
003600060906      // Definizione variabili.
003700060906      //
003800060906      //********************************************************************************************
003900060907     D rpyEsito        S             10I 0
004000060907     D rpyLibreria     S             10A
004100060912     D rqsLibreria     S             10A   INZ('     *libl')
004200060913     D rqsQuantita     S                   LIKE(nNumero) INZ(1000)
004300060913     D rpyCapienza     S                   LIKE(nNumero)
004400060907
004500060912     D*--------------------------------------------------
004600060912     D* Procedure name: Aznmr_inz
004700060912     D* Purpose:        Inizializza il modulo.
004800060912     D* Returns:        Esito.
004900060912     D* Parameter:      rqsCommit => Aprire i file sotto controllo sincronia.
005000060912     D* Parameter:      rqsLibreria => Libreria per EXTFILE.
005100060912     D* Parameter:      rpyEsito => Esito
005200060912     D*--------------------------------------------------
005300060912     D Aznmr_inz       PR            10I 0
005400060912     D  rqsCommit                     1A   CONST
005500060912     D                                     OPTIONS(*NOPASS:*OMIT)
005600060912     D  rqsLibreria                  10A   CONST
005700060912     D                                     OPTIONS(*NOPASS:*OMIT)
005800060912     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
005900060912
006000060907     D*--------------------------------------------------
006100060907     D* Procedure name: Aznmr_getNumeroN
006200060907     D* Purpose:        Preleva un numero numerico.
006300060907     D* Returns:        Numero.
006400060907     D* Parameter:      rqsCodice => Codice numeratore
006500060907     D* Parameter:      rqsSocieta => Società
006600060907     D* Parameter:      rqsUnita => Unità
006700060907     D* Parameter:      rqsKeyGest => Chiave gestionale
006800060907     D* Parameter:      rqsData => Data.
006900060907     D* Parameter:      rpyEsito => Esito
007000060907     D* Parameter:      rpyNumero => Numero.
007100060907     D*--------------------------------------------------
007200060907     D Aznmr_getNumeroN...
007300060907     D                 PR                  LIKE(nNumero)
007400060907     D  rqsCodice                          LIKE(nCodice)
007500060907     D                                     CONST
007600060907     D  rqsSocieta                         LIKE(nSocieta)
007700060907     D                                     CONST
007800060907     D                                     OPTIONS(*OMIT)
007900060907     D  rqsUnita                           LIKE(nUnita)
008000060907     D                                     CONST
008100060907     D                                     OPTIONS(*OMIT)
008200060907     D  rqsKeyGest                         LIKE(nKeyGest)
008300060907     D                                     CONST
008400060907     D                                     OPTIONS(*OMIT)
008500060907     D  rqsData                            LIKE(nDataPeriod)
008600060908     D                                     CONST
008700060907     D                                     OPTIONS(*OMIT)
008800060907     D  rpyEsito                     10I 0
008900060907     D  rpyNumero                          LIKE(nNumero)
009000060908     D                                     OPTIONS(*NOPASS:*OMIT)
009100060911
009200060913     D*--------------------------------------------------
009300060913     D* Procedure name: Aznmr_chkCapienzaN
009400060913     D* Purpose:        Controlla la capienza di un numeratore numerico.
009500060913     D* Returns:        Esito
009600060913     D* Parameter:      rqsCodice => Codice numeratore.
009700060913     D* Parameter:      rqsSocieta => Società
009800060913     D* Parameter:      rqsUnita => Unità
009900060913     D* Parameter:      rqsKeyGest => Chiave gestionale
010000060913     D* Parameter:      rqsData => Data.
010100060913     D* Parameter:      rqsQuantita => Quantità di numeri da prelevare.
010200060913     D* Parameter:      rpyCapienza => Quantità di numeri disponibili.
010300060913     D* Parameter:      rpyEsito => Esito.
010400060913     D*--------------------------------------------------
010500060913     D Aznmr_chkCapienzaN...
010600060913     D                 PR            10I 0
010700060913     D  rqsCodice                          LIKE(nCodice)
010800060913     D                                     CONST
010900060913     D  rqsSocieta                         LIKE(nSocieta)
011000060913     D                                     CONST
011100060913     D                                     OPTIONS(*OMIT)
011200060913     D  rqsUnita                           LIKE(nUnita)
011300060913     D                                     CONST
011400060913     D                                     OPTIONS(*OMIT)
011500060913     D  rqsKeyGest                         LIKE(nKeyGest)
011600060913     D                                     CONST
011700060913     D                                     OPTIONS(*OMIT)
011800060913     D  rqsData                            LIKE(nDataPeriod)
011900060913     D                                     CONST
012000060913     D                                     OPTIONS(*OMIT)
012100060913     D  rqsQuantita                        LIKE(nNumero)
012200060913     D  rpyCapienza                        LIKE(nNumero)
012300060913     D                                     OPTIONS(*NOPASS:*OMIT)
012400060913     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
012500060913
012600060906      /FREE
012700060906
012800060912       Aznmr_inz();
012900060912
013000060908       DSPLY ('ANNOSOLARE ' + %CHAR(
013100060908       Aznmr_getNumeroN('ANNOSOLARE':*OMIT:*OMIT:*OMIT:%DATE():rpyEsito)));
013200060908
013300060908       DSPLY ('ASSOLUTO ' + %CHAR(
013400060908       Aznmr_getNumeroN('ASSOLUTO':*OMIT:*OMIT:*OMIT:*OMIT:rpyEsito)));
013500060911
013600060911       nSocieta = '201';
013700060911       nkeyGest = '0027TRA';
013800060913       Aznmr_chkCapienzaN('ASSEGNITRA':nSocieta:*OMIT:nKeyGest:*OMIT:rqsQuantita
013900060913       :rpyCapienza:rpyEsito);
014000060913       DSPLY ('ASSEGNITRA Capienza ' + %CHAR(rpyCapienza));
014100060911       DSPLY ('ASSEGNITRA ' + %CHAR(
014200060911       Aznmr_getNumeroN('ASSEGNITRA':nSocieta:*OMIT:nKeyGest:*OMIT:rpyEsito)));
014300060911
014400060906       *INLR = *ON;
014500060906
014600060906      /END-FREE
