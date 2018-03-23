000100121109     D*--------------------------------------------------
000200121109     D* Procedure name: YPTCMSEQ_Init
000300121109     D* Purpose:        Inizializzazione modulo.
000400121109     D* Returns:        Esito.
000500121109     D* Parameter:      priKpjba
000600121109     D*--------------------------------------------------
000700121109     D YPTCMSEQ_Init   PR            10I 0
000800121109     D  priKpjba                           LIKE(kpjba)
000900121109
001000121112     D*--------------------------------------------------
001100121112     D* Procedure name: YPTCMSEQ_Finalize
001200121112     D* Purpose:        Chiusura modulo.
001300121112     D* Returns:        Esito.
001400121112     D*--------------------------------------------------
001500121112     D YPTCMSEQ_Finalize...
001600121112     D                 PR            10I 0
001700121112
001800121109     D*--------------------------------------------------
001900121109     D* Procedure name: YPTCMSEQ_NewFile
002000121109     D* Purpose:        Nuovo file.
002100121109     D* Returns:        Esito.
002200121109     D* Parameter:      priNomeFileFlusso => Nome file flusso. Se non passa...
002300121109     D*                          to sarà assegnato un nome automatico.
002400121109     D*--------------------------------------------------
002500121109     D YPTCMSEQ_NewFile...
002600121109     D                 PR            10I 0
002700121109     D  priNomeFileFlusso...
002800121109     D                              255A   VARYING
002900121109     D                                     CONST
003000121109     D                                     OPTIONS(*NOPASS)
003100121109
003200121109     D*--------------------------------------------------
003300121109     D* Procedure name: YPTCMSEQ_EndFile
003400121109     D* Purpose:        Fine file.
003500121109     D* Returns:        Esito.
003600121109     D* Parameter:      priCommit => *ON = commit; *OFF = rollback.
003700121109     D*--------------------------------------------------
003800121109     D YPTCMSEQ_EndFile...
003900121109     D                 PR            10I 0
004000121109     D  priCommit                      N   CONST
004100121109
004200121109     D*--------------------------------------------------
004300121109     D* Procedure name: YPTCMSEQ_NewRegistrazione
004400121109     D* Purpose:        Inizia una nuova registrazione.
004500121109     D* Returns:        Esito.
004600121109     D* Parameter:      priIdSocieta => ID società.
004700121109     D* Parameter:      priDataRegistrazione => Data registrazione.
004800121109     D* Parameter:      priNumeroRegistrazione => Data registrazione.
004900121109     D* Parameter:      priProfiloUtente => Profilo utente che ha inserito ...
005000121109     D*                          la registrazione in Proj.
005100121109     D* Parameter:      priKccBanca => Capo conto banca.
005200121109     D* Parameter:      priKscBanca => Sotto conto banca.
005300121109     D* Parameter:      priSegno => Dare/Avere
005400121109     D* Parameter:      priDivisa => Divisa.
005500121109     D* Parameter:      priImportoDivisa => Importo in divisa.
005600121109     D* Parameter:      priImporto => Importo in moneta corrente.
005700121109     D* Parameter:      priUnita => Unita.
005800121116     D* Parameter:      priDescrizione => Descrizione.
005900121109     D*--------------------------------------------------
006000121109     D YPTCMSEQ_NewRegistrazione...
006100121109     D                 PR            10I 0
006200121113     D  priSys                        3P 0
006300121113     D  priNrAsReg                    9P 0
006400121113     D  priNrRigaM                    5P 0
006500121113     D  priIdSocieta                  3A
006600121109     D  priDataRegistrazione...
006700121109     D                                 D   DATFMT(*ISO)
006800121109     D  priNumeroRegistrazione...
006900121109     D                                9P 0
007000121109     D  priProfiloUtente...
007100121109     D                               10A
007200121109     D  priKccBanca                   6A
007300121109     D  priKscBanca                   8A
007400121109     D  priSegno                      1A
007500121109     D  priDivisa                     4A
007600121109     D  priImportoDivisa...
007700121112     D                               13P 2 CONST
007800121112     D  priImporto                   13P 2 CONST
007900121112     D  priCausale                    4A
008000121112     D  priDataOperazione...
008100121112     D                                 D
008200121112     D  priDataValuta...
008300121112     D                                 D
008400121112     D  priUnita                      8A
008500121116     D  priDescrizione...
008600121116     D                              250A
008700130206     D  priBanca                      4A
008800130206     D  priLinea                      4A
008900130206
009000121112     D*--------------------------------------------------
009100121112     D* Procedure name: YPTCMSEQ_EndRegistrazione
009200121112     D* Purpose:        Fine registrazione.
009300121112     D* Returns:        Esito.
009400121112     D* Parameter:      priCommit => *ON = commit; *OFF = rollback.
009500121112     D*--------------------------------------------------
009600121112     D YPTCMSEQ_EndRegistrazione...
009700121112     D                 PR            10I 0
009800121112     D  priCommit                      N   CONST
009900121112
