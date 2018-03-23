000100080320     D*--------------------------------------------------
000200080321     D* Procedure name: Aznmr_init
000300080320     D* Purpose:        Inizializza il modulo.
000400080320     D* Returns:        Esito.
000500080320     D* Parameter:      rqsCommit => Aprire i file sotto controllo sincronia.
000600080320     D* Parameter:      rqsLibreria => Libreria per EXTFILE.
000700080320     D* Parameter:      rpyEsito => Esito
000800080320     D*--------------------------------------------------
000900080321     D Aznmr_init      PR            10I 0
001000080320     D  rqsCommit                     1A   CONST
001100080320     D                                     OPTIONS(*NOPASS:*OMIT)
001200080320     D  rqsLibreria                  10A   CONST
001300080320     D                                     OPTIONS(*NOPASS:*OMIT)
001400080320     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
001500080320
001600080320     D*--------------------------------------------------
001700080320     D* Procedure name: Aznmr_getNumeroN
001800080320     D* Purpose:        Preleva un numero numerico.
001900080320     D* Returns:        Numero.
002000080320     D* Parameter:      rqsCodice => Codice numeratore
002100080320     D* Parameter:      rqsSocieta => Società
002200080320     D* Parameter:      rqsUnita => Unità
002300080320     D* Parameter:      rqsKeyGest => Chiave gestionale
002400080320     D* Parameter:      rqsData => Data.
002500080320     D* Parameter:      rpyEsito => Esito
002600080320     D* Parameter:      rpyNumero => Numero.
002700080320     D*--------------------------------------------------
002800080320     D Aznmr_getNumeroN...
002900080320     D                 PR            15P 0
003000080320     D  rqsCodice                    10A
003100080320     D                                     CONST
003200080320     D  rqsSocieta                    3A
003300080320     D                                     CONST
003400080320     D                                     OPTIONS(*OMIT)
003500080320     D  rqsUnita                      8A
003600080320     D                                     CONST
003700080320     D                                     OPTIONS(*OMIT)
003800080320     D  rqsKeyGest                   10A
003900080320     D                                     CONST
004000080320     D                                     OPTIONS(*OMIT)
004100080320     D  rqsData                        D
004200080320     D                                     CONST
004300080320     D                                     OPTIONS(*OMIT)
004400080320     D  rpyEsito                     10I 0
004500080320     D  rpyNumero                    15P 0
004600080320     D                                     OPTIONS(*NOPASS:*OMIT)
004700080320
004800080320     D*--------------------------------------------------
004900080320     D* Procedure name: Aznmr_chkCapienzaN
005000080320     D* Purpose:        Controlla la capienza di un numeratore numerico.
005100080320     D* Returns:        Esito
005200080320     D* Parameter:      rqsCodice => Codice numeratore.
005300080320     D* Parameter:      rqsSocieta => Società
005400080320     D* Parameter:      rqsUnita => Unità
005500080320     D* Parameter:      rqsKeyGest => Chiave gestionale
005600080320     D* Parameter:      rqsData => Data.
005700080320     D* Parameter:      rqsQuantita => Quantità di numeri da prelevare.
005800080320     D* Parameter:      rpyCapienza => Quantità di numeri disponibili.
005900080320     D* Parameter:      rpyEsito => Esito.
006000080320     D*--------------------------------------------------
006100080320     D Aznmr_chkCapienzaN...
006200080320     D                 PR            10I 0
006300080320     D  rqsCodice                    10A
006400080320     D                                     CONST
006500080320     D  rqsSocieta                    3A
006600080320     D                                     CONST
006700080320     D                                     OPTIONS(*OMIT)
006800080320     D  rqsUnita                      8A
006900080320     D                                     CONST
007000080320     D                                     OPTIONS(*OMIT)
007100080320     D  rqsKeyGest                   10A
007200080320     D                                     CONST
007300080320     D                                     OPTIONS(*OMIT)
007400080320     D  rqsData                        D
007500080320     D                                     CONST
007600080320     D                                     OPTIONS(*OMIT)
007700080320     D  rqsQuantita                  15P 0
007800080320     D  rpyCapienza                  15P 0
007900080320     D                                     OPTIONS(*NOPASS:*OMIT)
008000080320     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
008100080321
008200080321     D*--------------------------------------------------
008300080321     D* Procedure name: Aznmr_finalize
008400080321     D* Purpose:        Chiusura oggetto numeratore.
008500080321     D* Returns:        Esito.
008600080321     D*--------------------------------------------------
008700080321     D Aznmr_finalize  PR            10I 0
008800080321
