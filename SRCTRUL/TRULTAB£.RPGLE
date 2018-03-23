000100090729       //--------------------------------------------------------------
000200090805       //?TRULTAB - Reperimento dati tabella da TABEL00F o TNTBE01L    ?
000300090729       //--------------------------------------------------------------
000400090729
000500090729     h NoMain
000600090729     h datedit(*ymd) decedit(*jobrun) option(*nodebugio)
000700090729
000800090729       //--------------------------------------------------------------
000900090729       //?Dichiarazione files.                                         ?
001000090729       //--------------------------------------------------------------
001100090729
001200090729     fTABEL00F  if   e           k disk
001300090729     f                                     usropn
001400090729     f                                     infds(TABELfbk)
001500090729
001600090729     fTNTBE01L  if   e           k disk
001700090729     f                                     usropn
001800090729     f                                     infds(TNTBEfbk)
001900090729
002000090729       //--------------------------------------------------------------
002100090729       //?Definizione costanti.                                        ?
002200090729       //--------------------------------------------------------------
002300090729
002400090729     d c_ErrParametriNonValidi...
002500090729     d                 c                   const(-1)
002600090729     d c_ErrTabellaInesistente...
002700090729     d                 c                   const(-2)
002800090810     d c_ErrCloseTABEL...
002900090810     d                 c                   const(-3)
003000090810     d c_ErrCloseTNTBE...
003100090810     d                 c                   const(-4)
003200090810     d c_ErrCloseTABELeTNTBE...
003300090810     d                 c                   const(-5)
003400090729
003500090729       //--------------------------------------------------------------
003600090729       //?Definizione schiere.                                         ?
003700090729       //--------------------------------------------------------------
003800090729
003900090729
004000090729       //--------------------------------------------------------------
004100090729       //?Definizione aree dati.                                       ?
004200090729       //--------------------------------------------------------------
004300090729
004400090729
004500090729       //--------------------------------------------------------------
004600090729       //?Definizione strutture dati.                                  ?
004700090729       //--------------------------------------------------------------
004800090729
004900090729     d TABELfbk        ds                  qualified
005000090729     d  File_Name             83     92                                         * File name
005100090729     d  Library               93    102                                         * Library name
005200090729     d  Db_Rrn               397    400i 0                                      * Relative-rcd-num
005300090729
005400090729     d TNTBEfbk        ds                  qualified
005500090729     d  File_Name             83     92                                         * File name
005600090729     d  Library               93    102                                         * Library name
005700090729     d  Db_Rrn               397    400i 0                                      * Relative-rcd-num
005800090729
005900090729       //--------------------------------------------------------------
006000090729       //?Definizione variabili globali.                               ?
006100090729       //--------------------------------------------------------------
006200090729
006300090729     d wDataIso        s               d   datfmt(*iso)  inz(*loval)
006400090803     d wOutEsito       s             10i 0 inz
006500090803
006600090729       //--------------------------------------------------------------
006700090731       //?Definizione prototipi procedure usate.                       ?
006800090729       //--------------------------------------------------------------
006900090729
007000090803       // - Reperimento dati di una tabella (TABEL o TNTBE)
007100090803      /copy gaitrasrc/srcProtoPr,TRULTAB
007200090731
007300090731       //--------------------------------------------------------------
007400090731       //?Definizione key-list.                                        ?
007500090731       //--------------------------------------------------------------
007600090731
007700090731       // - TABEL00F
007800090731     d k03tabel00    e ds                  extname(TABEL00F:*key)  inz
007900090731
008000090731       // - TNTBE01L
008100090731     d k05tntbe01    e ds                  extname(TNTBE01L:*key)  inz
008200090731
008300090729       //--------------------------------------------------------------
008400090803       //?Procedura getTabella                                        ?
008500090729       //--------------------------------------------------------------
008600090729
008700090729       // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
008800090803       // Procedure name: getTabella
008900090803       // Purpose:        Reperimento dati tabella (TABEL o TNTBE)
009000090803       // Returns:        Integer (Esito)
009100090803       // Parameter:      pInFileTab   => Tipo file in cui accedere
009200090803       //                                 ("1"=TABEL00F, "2"=TNTBE01L)
009300090803       // Parameter:      pInCodiceTab => Codice tabella
009400090803       // Parameter:      pInKey1      => 1ª chiave tabella
009500090803       //                                 (unica per TABEL00F)
009600090803       // Parameter:      pInKey2      => 2ª chiave tabella
009700090803       //                                 (solo per tipo file "2")
009800090803       // Parameter:      pInLingua    => Codice lingua
009900090803       //                                 (solo per tipo file "2")
010000090803       // Parameter:      pInSistInfo  => Nome sistema informativo
010100090803       //                                 (solo per tipo file "2")
010200090803       // Parameter:      pOutLibreria => Nome libreria del file
010300090803       // Parameter:      pOutRrn      => Numero relativo record corrispondente
010400090803       // Parameter:      pOutKey2     => Campo TBLKE2 corrispondente ai dati reperiti
010500090803       //                                 (solo per tipo file "2")
010600090803       // Parameter:      pOutLingua   => Campo TBELIN corrispondente ai dati reperiti
010700090803       //                                 (solo per tipo file "2")
010800090803       // Parameter:      pOutSistInfo => Campo TBESIF corrispondente ai dati reperiti
010900090803       //                                 (solo per tipo file "2")
011000090803       // Parameter:      pOutApplicaz => Campo TBEAPL corrispondente ai dati reperiti
011100090803       //                                 (solo per tipo file "2")
011200090803       // Parameter:      pOutFtt      => Tipo trasmissione
011300090803       // Parameter:      pOutFlt      => Cod. filiale trasmissione dati
011400090803       // Parameter:      pOutFtr      => Flag trasmissione:
011500090803       //                                 " " = record da trasmettere
011600090803       //                                 "T" = record trasmesso
011700090803       //                                 "R" = record ricevuto
011800090803       // Parameter:      pOutDtr      => Data trasmissione
011900090803       // Parameter:      pOutAnnull   => Flag annullamento
012000090803       // Parameter:      pOutUnico    => Campo unico tabella
012100090729       // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
012200090731
012300090803     p getTabella      B                   export
012400090731
012500090803     d getTabella...
012600090803     d                 pi            10i 0
012700090803     d  pInFileTab                    1a                 const
012800090803     d  pInCodiceTab                       like(TBEcod)  const
012900090803     d  pInKey1                            like(TBEke1)  const
013000090803     d  pInKey2                            like(TBEke2)  const
013100090803     d                                      options(*nopass : *omit)
013200090803     d  pInLingua                          like(TBElin)  const
013300090803     d                                      options(*nopass : *omit)
013400090803     d  pInSistInfo                        like(TBEsif)  const
013500090803     d                                      options(*nopass : *omit)
013600090803     d  pOutLibreria                 10a    options(*nopass : *omit)
013700090803     d  pOutRrn                      10i 0  options(*nopass : *omit)
013800090803     d  pOutKey2                           like(TBEke2)
013900090803     d                                      options(*nopass : *omit)
014000090803     d  pOutLingua                         like(TBElin)
014100090803     d                                      options(*nopass : *omit)
014200090803     d  pOutSistInfo                       like(TBEsif)
014300090803     d                                      options(*nopass : *omit)
014400090803     d  pOutApplicaz                       like(TBEapl)
014500090803     d                                      options(*nopass : *omit)
014600090803     d  pOutFtt                            like(TBEftt)
014700090803     d                                      options(*nopass : *omit)
014800090803     d  pOutFlt                            like(TBEflt)
014900090803     d                                      options(*nopass : *omit)
015000090803     d  pOutFtr                            like(TBEftr)
015100090803     d                                      options(*nopass : *omit)
015200090803     d  pOutDtr                            like(TBEdtr)
015300090803     d                                      options(*nopass : *omit)
015400090803     d  pOutAnnull                         like(TBEatb)
015500090803     d                                      options(*nopass : *omit)
015600090803     d  pOutUnico                          like(TBEuni)
015700090803     d                                      options(*nopass : *omit)
015800090729
015900090729      /free
016000090729
016100090729       // -?Pulizia parametri di output?
016200090803       clear  wOutEsito;
016300090804       if  %parms() > 6   and   %addr(pOutLibreria) <> *null;
016400090803         clear  pOutLibreria;
016500090729       endif;
016600090804       if  %parms() > 7   and   %addr(pOutRrn) <> *null;
016700090803         clear  pOutRrn;
016800090729       endif;
016900090804       if  %parms() > 8   and   %addr(pOutKey2) <> *null;
017000090803         clear  pOutKey2;
017100090731       endif;
017200090804       if  %parms() > 9   and   %addr(pOutLingua) <> *null;
017300090803         clear  pOutLingua;
017400090731       endif;
017500090804       if  %parms() > 10  and   %addr(pOutSistInfo) <> *null;
017600090803         clear  pOutSistInfo;
017700090731       endif;
017800090804       if  %parms() > 11  and   %addr(pOutApplicaz) <> *null;
017900090803         clear  pOutApplicaz;
018000090729       endif;
018100090804       if  %parms() > 12  and   %addr(pOutFtt) <> *null;
018200090803         clear  pOutFtt;
018300090729       endif;
018400090804       if  %parms() > 13  and   %addr(pOutFlt) <> *null;
018500090803         clear  pOutFlt;
018600090729       endif;
018700090804       if  %parms() > 14  and   %addr(pOutFtr) <> *null;
018800090803         clear  pOutFtr;
018900090729       endif;
019000090804       if  %parms() > 15  and   %addr(pOutDtr) <> *null;
019100090803         clear  pOutDtr;
019200090729       endif;
019300090804       if  %parms() > 16  and   %addr(pOutAnnull) <> *null;
019400090803         clear  pOutAnnull;
019500090730       endif;
019600090804       if  %parms() > 17  and   %addr(pOutUnico) <> *null;
019700090803         clear  pOutUnico;
019800090729       endif;
019900090729
020000090729       // -?Controllo parametri ricevuti?
020100090803       if     (pInFileTab <> '1'   and   pInFileTab <> '2')
020200091106           or (pInCodiceTab = *blank     and
020300091109               (pInKey1 = *blank   and   %addr(pInKey2)  = *null)
020400091109            or (pInKey1 = *blank   and   %addr(pInKey2) <> *null   and
020500091109                pInKey2 = *blank));
020600090803         wOutEsito = c_ErrParametriNonValidi;
020700090803         return  wOutEsito;
020800090729       endif;
020900090729
021000090729       // -?Aggancio tabella richiesta?
021100090804       SELECT;
021200090731
021300090731         // ·?In file TABEL00F?
021400090804         WHEN  pInFileTab = '1';
021500090731
021600090804
021700090804           if  NOT %open(TABEL00F);
021800090804             open  TABEL00F;
021900090804           endif;
022000090804
022100090805           TBLkut = 1;
022200090805           TBLcod = pInCodiceTab;
022300090805           TBLkey = pInKey1;
022400090805
022500090805           chain  %kds(k03tabel00)  TABEL;
022600090804
022700090804           if  Not  %found(TABEL00F);
022800090804             wOutEsito = c_ErrTabellaInesistente;
022900090804             return  wOutEsito;
023000090804           endif;
023100090804
023200090804           if  %parms() > 6   and   %addr(pOutLibreria) <> *null;
023300090804             pOutLibreria = TABELfbk.Library;
023400090804           endif;
023500090804           if  %parms() > 7   and   %addr(pOutRrn) <> *null;
023600090804             pOutRrn      = TABELfbk.Db_Rrn;
023700090804           endif;
023800090804           if  %parms() > 12  and   %addr(pOutFtt) <> *null;
023900090804             pOutFtt      = TBLftt;
024000090804           endif;
024100090804           if  %parms() > 13  and   %addr(pOutFlt) <> *null;
024200090804             pOutFlt      = TBLflt;
024300090804           endif;
024400090804           if  %parms() > 14  and   %addr(pOutFtr) <> *null;
024500090804             pOutFtr      = TBLftr;
024600090804           endif;
024700090804           if  %parms() > 15  and   %addr(pOutDtr) <> *null
024800090804                              and   TBLdtr > *zero;
024900090804             wDataIso     = %date(TBLdtr : *YMD);
025000090804             pOutDtr      = %dec(wDataIso);
025100090804           endif;
025200090804           if  %parms() > 16  and   %addr(pOutAnnull) <> *null;
025300090804             pOutAnnull   = TBLflg;
025400090804           endif;
025500090804           if  %parms() > 17  and   %addr(pOutUnico) <> *null;
025600090804             pOutUnico    = TBLuni;
025700090804           endif;
025800090731
025900090731         // ·?In file TNTBE01L?
026000090804         WHEN  pInFileTab = '2';
026100090731
026200090804           if  NOT %open(TNTBE01L);
026300090804             open  TNTBE01L;
026400090804           endif;
026500090804
026600090805           TBEcod = pInCodiceTab;
026700090805           TBEke1 = pInKey1;
026800090805           if  %parms() > 3   and   %addr(pInKey2) <> *null;
026900090805             TBEke2 = pInKey2;
027000090805           else;
027100090805             clear  TBEke2;
027200090805           endif;
027300090805           if  %parms() > 4   and   %addr(pInLingua) <> *null;
027400090805             TBElin = pInLingua;
027500090805           else;
027600090805             clear  TBElin;
027700090805           endif;
027800090805           if  %parms() > 5   and   %addr(pInSistInfo) <> *null;
027900090805             TBEsif = pInSistInfo;
028000090805           else;
028100090805             clear  TBEsif;
028200090805           endif;
028300090804
028400090805           select;
028500090805             when  %parms() > 5   and   %addr(pInKey2)     <> *null
028600090805                                  and   %addr(pInLingua)   <> *null
028700090805                                  and   %addr(pInSistInfo) <> *null;
028800090805               chain  %kds(k05tntbe01)  TNTBE000;
028900090805             when  %parms() > 4   and   %addr(pInKey2)   <> *null
029000090805                                  and   %addr(pInLingua) <> *null;
029100090805               chain  %kds(k05tntbe01 : 4)  TNTBE000;
029200090805             when  %parms() > 3   and   %addr(pInKey2) <> *null;
029300090805               chain  %kds(k05tntbe01 : 3)  TNTBE000;
029400090805             other;
029500090805               chain  %kds(k05tntbe01 : 2)  TNTBE000;
029600090805           endsl;
029700090804
029800090804           if  Not  %found(TNTBE01L);
029900090804             wOutEsito = c_ErrTabellaInesistente;
030000090804             return  wOutEsito;
030100090804           endif;
030200090804
030300090804           if  %parms() > 6   and   %addr(pOutLibreria) <> *null;
030400090804             pOutLibreria = TNTBEfbk.Library;
030500090804           endif;
030600090804           if  %parms() > 7   and   %addr(pOutRrn) <> *null;
030700090804             pOutRrn      = TNTBEfbk.Db_Rrn;
030800090804           endif;
030900090804           if  %parms() > 8   and   %addr(pOutKey2) <> *null;
031000090804             pOutKey2     = TBEke2;
031100090804           endif;
031200090804           if  %parms() > 9   and   %addr(pOutLingua) <> *null;
031300090804             pOutLingua   = TBElin;
031400090804           endif;
031500090804           if  %parms() > 10  and   %addr(pOutSistInfo) <> *null;
031600090804             pOutSistInfo = TBEsif;
031700090804           endif;
031800090804           if  %parms() > 11  and   %addr(pOutApplicaz) <> *null;
031900090804             pOutApplicaz = TBEapl;
032000090804           endif;
032100090804           if  %parms() > 12  and   %addr(pOutFtt) <> *null;
032200090804             pOutFtt      = TBEftt;
032300090804           endif;
032400090804           if  %parms() > 13  and   %addr(pOutFlt) <> *null;
032500090804             pOutFlt      = TBEflt;
032600090804           endif;
032700090804           if  %parms() > 14  and   %addr(pOutFtr) <> *null;
032800090804             pOutFtr      = TBEftr;
032900090804           endif;
033000090804           if  %parms() > 15  and   %addr(pOutDtr) <> *null;
033100090804             pOutDtr      = TBEdtr;
033200090804           endif;
033300090804           if  %parms() > 16  and   %addr(pOutAnnull) <> *null;
033400090804             pOutAnnull   = TBEatb;
033500090804           endif;
033600090804           if  %parms() > 17  and   %addr(pOutUnico) <> *null;
033700090804             pOutUnico    = TBEuni;
033800090804           endif;
033900090731
034000090804       ENDSL;
034100090729
034200090804       return  *zero;
034300090731
034400090729      /end-free
034500090731
034600090810     p getTabella      E
034700090810
034800090810
034900090810       // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
035000090810       // Procedure name: cloTabella
035100090810       // Purpose:        chiusura archvi tabelle (TABEL e/o TNTBE)
035200090810       // Returns:        Integer (Esito)
035300090810       // Parameter:      pInFileTab   => File da chiudere
035400090810       //                                 ("1"=TABEL00F, "2"=TNTBE01L)
035500090810       // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
035600090810
035700090810     p cloTabella      B                   export
035800090810
035900090810     d cloTabella...
036000090810     d                 pi            10i 0
036100090810     d  pInFileTab                    1a   const
036200090810     d                                     options(*nopass : *omit)
036300090810
036400090810      /free
036500090810
036600090810       clear  wOutEsito;
036700090810
036800090810       if  (%parms() = *zero   or   %addr(pInFileTab) = *null   or
036900090810                                    pInFileTab = '1')          and
037000090810            %open(TABEL00F);
037100090810         close(E)  TABEL00F;
037200090810         select;
037300090810           when  %error  and  wOutEsito =  *zero;
037400090810             wOutEsito = c_ErrCloseTABEL;
037500090810           when  %error  and  wOutEsito <> *zero;
037600090810             wOutEsito = c_ErrCloseTABELeTNTBE;
037700090810         endsl;
037800090810       endif;
037900090810
038000090810       if  (%parms() = *zero   or   %addr(pInFileTab) = *null   or
038100090810                                    pInFileTab = '2')          and
038200090810            %open(TNTBE01L);
038300090810         close(E)  TNTBE01L;
038400090810         select;
038500090810           when  %error  and  wOutEsito =  *zero;
038600090810             wOutEsito = c_ErrCloseTNTBE;
038700090810           when  %error  and  wOutEsito <> *zero;
038800090810             wOutEsito = c_ErrCloseTABELeTNTBE;
038900090810         endsl;
039000090810       endif;
039100090810
039200090810       return  wOutEsito;
039300090810
039400090810      /end-free
039500090810
039600090810     p cloTabella      E
