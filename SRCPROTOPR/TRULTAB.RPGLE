000100090803       //--------------------------------------------------
000200090803       // Procedure name: getTabella
000300090803       // Purpose:        Reperimento dati di una tabella (TABEL o TNTBE)
000400090803       // Returns:        Integer (Esito)
000500090803       // Parameter:      pInFileTab   => Tipo file in cui accedere
000600090803       //                                 ("1"=TABEL00F, "2"=TNTBE01L)
000700090803       // Parameter:      pInCodiceTab => Codice tabella
000800090803       // Parameter:      pInKey1      => 1ª chiave tabella
000900090803       //                                 (unica per TABEL00F)
001000090803       // Parameter:      pInKey2      => 2ª chiave tabella
001100090803       //                                 (solo per tipo file "2")
001200090803       // Parameter:      pInLingua    => Codice lingua
001300090803       //                                 (solo per tipo file "2")
001400090803       // Parameter:      pInSistInfo  => Nome sistema informativo
001500090803       //                                 (solo per tipo file "2")
001600090803       // Parameter:      pOutLibreria => Nome libreria del file
001700090803       // Parameter:      pOutRrn      => Numero relativo record corrispondente
001800090803       // Parameter:      pOutKey2     => Campo TBLKE2 corrispondente ai dati reperiti
001900090803       //                                 (solo per tipo file "2")
002000090803       // Parameter:      pOutLingua   => Campo TBELIN corrispondente ai dati reperiti
002100090803       //                                 (solo per tipo file "2")
002200090803       // Parameter:      pOutSistInfo => Campo TBESIF corrispondente ai dati reperiti
002300090803       //                                 (solo per tipo file "2")
002400090803       // Parameter:      pOutApplicaz => Campo TBEAPL corrispondente ai dati reperiti
002500090803       //                                 (solo per tipo file "2")
002600090803       // Parameter:      pOutFtt      => Tipo trasmissione
002700090803       // Parameter:      pOutFlt      => Cod. filiale trasmissione dati
002800090803       // Parameter:      pOutFtr      => Flag trasmissione:
002900090803       //                                 " " = record da trasmettere
003000090803       //                                 "T" = record trasmesso
003100090803       //                                 "R" = record ricevuto
003200090803       // Parameter:      pOutDtr      => Data trasmissione
003300090803       // Parameter:      pOutAnnull   => Flag annullamento
003400090803       // Parameter:      pOutUnico    => Campo unico tabella
003500090803       //--------------------------------------------------
003600090810     d ds_TNTBE      e ds                  extname(TNTBE00F) qualified
003700090810     d                                     inz
003800090803       // - Reperimento dati tabella (TABEL00F o TNTBE01L)
003900090803     d getTabella...
004000090803     d                 pr            10i 0
004100090810     d  pInFileTab                    1a                          const
004200090810     d  pInCodiceTab                       like(ds_TNTBE.TBEcod)  const
004300090810     d  pInKey1                            like(ds_TNTBE.TBEke1)  const
004400090810     d  pInKey2                            like(ds_TNTBE.TBEke2)  const
004500090803     d                                      options(*nopass : *omit)
004600090810     d  pInLingua                          like(ds_TNTBE.TBElin)  const
004700090803     d                                      options(*nopass : *omit)
004800090810     d  pInSistInfo                        like(ds_TNTBE.TBEsif)  const
004900090803     d                                      options(*nopass : *omit)
005000090803     d  pOutLibreria                 10a    options(*nopass : *omit)
005100090803     d  pOutRrn                      10i 0  options(*nopass : *omit)
005200090810     d  pOutKey2                           like(ds_TNTBE.TBEke2)
005300090803     d                                      options(*nopass : *omit)
005400090810     d  pOutLingua                         like(ds_TNTBE.TBElin)
005500090803     d                                      options(*nopass : *omit)
005600090810     d  pOutSistInfo                       like(ds_TNTBE.TBEsif)
005700090803     d                                      options(*nopass : *omit)
005800090810     d  pOutApplicaz                       like(ds_TNTBE.TBEapl)
005900090803     d                                      options(*nopass : *omit)
006000090810     d  pOutFtt                            like(ds_TNTBE.TBEftt)
006100090803     d                                      options(*nopass : *omit)
006200090810     d  pOutFlt                            like(ds_TNTBE.TBEflt)
006300090803     d                                      options(*nopass : *omit)
006400090810     d  pOutFtr                            like(ds_TNTBE.TBEftr)
006500090803     d                                      options(*nopass : *omit)
006600090810     d  pOutDtr                            like(ds_TNTBE.TBEdtr)
006700090803     d                                      options(*nopass : *omit)
006800090810     d  pOutAtb                            like(ds_TNTBE.TBEatb)
006900090803     d                                      options(*nopass : *omit)
007000090810     d  pOutUnico                          like(ds_TNTBE.TBEuni)
007100090803     d                                      options(*nopass : *omit)
007200090810
007300090810       // - Chiusura archivi tabella (TABEL00F e TNTBE01L)
007400090810     d cloTabella...
007500090810     d                 pr            10i 0
007600090810     d  pInFileTab                    1a   const
007700090810     d                                     options(*nopass : *omit)
