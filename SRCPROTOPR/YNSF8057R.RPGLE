000100170726     D GetNrAsFattura  PR             1
000102170726     D  Societa                       3A   VALUE
000105170726     D  Ctb                           2A   VALUE
000108170726     D  KCC                           6A   VALUE
000111170726     D  KSC                           8A   VALUE
000114170726     D  DtPar                          D   VALUE
000117170726     D  NrPar                         9P 0 VALUE
000120170726     D  SerPar                        4A   VALUE
000122170726     D  Sys                           3P 0
000124170726     D  NrAsReg                       9P 0
000126170726     D  Esito                         1A
000127170726     D  prmNdreg00f                        LIKEDS(ndreg00f)
000128170726     D                                     OPTIONS(*NOPASS:*OMIT)
000129170726
000130170726     D*--------------------------------------------------
000131170726     D* Procedure name: isFatturaManuale
000132170726     D* Purpose:        Restituisce *ON se la fattura è manuale.
000133170726     D* Returns:        *ON = è una fattura manuale
000134170726     D* Parameter:      rqsSocieta => Codice società
000135170726     D* Parameter:      rqsCtb => Contabilità
000136170726     D* Parameter:      rqsKcc => Capo conto cliente
000137170726     D* Parameter:      rqsKsc => Sotto conto cliente
000138170726     D* Parameter:      rqsDtPar => Data partita
000139170726     D* Parameter:      rqsNrPar => Numero partita
000140170726     D* Parameter:      rqsSerPar => Serie partita
000141170726     D* Parameter:      rpyEsito => Esito
000142170726     D* Parameter:      rpyOpCode => Codice operativo risposta
000143170726     D* Parameter:      rpyStatus => Stato
000144170726     D*--------------------------------------------------
000145170726     D isFatturaManuale...
000146170726     D                 PR              N
000147170726     D  rqsSocieta                    3A
000148170726     D  rqsCtb                        2A
000149170726     D  rqsKcc                        6A
000150170726     D  rqsKsc                        8A
000151170726     D  rqsDtPar                       D
000152170726     D  rqsNrPar                      9P 0
000153170726     D  rqsSerPar                     4A
000154170726     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
000155170726     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
000156170726     D  rpyStatus                    10I 0 OPTIONS(*NOPASS:*OMIT)
000157170726
000158170726       // --------------------------------------------------
000159170726       // Prototype for procedure: IsFatturaPubblicaAmministrazione
000160170726       // --------------------------------------------------
000161170726
000162170726       DCL-PR IsFatturaPubblicaAmministrazione IND;
000163170726         prmSys PACKED(3:0) CONST;
000164170726         prmNrAsReg PACKED(9:0) CONST;
000165170726       END-PR ;
