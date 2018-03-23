000100130517     /*MODULE
000200130517     /*PRM dbgview(*source)
000300130517     /*END
000400130517       //==============================================================
000500090805       //?TRULTAB - Reperimento dati tabella da TABEL00F o TNTBE01L    ?
000600130517       //==============================================================
000700130517       //
000800130517       // Questo modulo reperisce i dati della tabella ricevuta come
000900130517       // parametro.
001000130517       //
001100130517       // Può essere usato solo come sottoprocedura.
001200130517       // Ogni metodo restituisce un valore 10I:
001300130517       //    · = *zero => tutto ok
001400130517       //    · < *zero => errore
001500130517       //
001600130517       // _________________________________________________
001700130517       // PARAMETRI DELLA SOTTOPROCEDURA: UBLEG3C_Rtv...
001800130517       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
001900130517       // Posizione Uso Tipo       Lunghezza Descrizione
002000130517       // --------- --- ---------- --------- --------------------------
002100130517       // 1         In  *Dec          7 / 0  Codice cliente
002200130517       // 2         Out *Flag         1      P=Padre, F=Figlio
002300130517       // 3         Out *Dec          7 / 0  Codice padre
002400130517       // 4         Out *Char      3200      800 cod.clienti (packed) in famiglia
002500130517       // 4         Out *Char       800      800 flags "EasyWEB" (*on/*off)
002600130517       // --------- --- ----------  --------- -------------------------
002700130517       //
002800130517       // __________________
002900130517       // ESEMPI DI CHIAMATA
003000130517       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
003100130517       // if  UBLEG3C_Rtv ( pInKSC : pOutFLGPF : pOutPADRE
003200130517       //                   pOutSKC : pOutSKEW ) = *zero;
003300130517       //
003400130517       // ______________________________
003500130517       // ISTRUZIONI PER LA COMPILAZIONE
003600130517       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
003700130517       // 1. Creare il modulo UBLEG3C (Opz 15 PDM)
003800130517       // 2. Creare/Aggiornare il programma di servizio UBLEG3C (CRTSRVPGM / UPDSRVPGM)
003900130517       // 4. Cancellare il modulo.
004000130517       //
004100130517       //==============================================================
004200090729
004300090729     h NoMain
004400090729     h datedit(*ymd) decedit(*jobrun) option(*nodebugio)
004500090729
004600090729       //--------------------------------------------------------------
004700090729       //?Dichiarazione files.                                         ?
004800090729       //--------------------------------------------------------------
004900090729
005000090729     fTABEL00F  if   e           k disk
005100090729     f                                     usropn
005200090729     f                                     infds(TABELfbk)
005300090729
005400090729     fTNTBE01L  if   e           k disk
005500090729     f                                     usropn
005600090729     f                                     infds(TNTBEfbk)
005700090729
005800090729       //--------------------------------------------------------------
005900090729       //?Definizione costanti.                                        ?
006000090729       //--------------------------------------------------------------
006100090729
006200090729     d c_ErrParametriNonValidi...
006300090729     d                 c                   const(-1)
006400090729     d c_ErrTabellaInesistente...
006500090729     d                 c                   const(-2)
006600090810     d c_ErrCloseTABEL...
006700090810     d                 c                   const(-3)
006800090810     d c_ErrCloseTNTBE...
006900090810     d                 c                   const(-4)
007000090810     d c_ErrCloseTABELeTNTBE...
007100090810     d                 c                   const(-5)
007200090729
007300090729       //--------------------------------------------------------------
007400090729       //?Definizione schiere.                                         ?
007500090729       //--------------------------------------------------------------
007600090729
007700090729
007800090729       //--------------------------------------------------------------
007900090729       //?Definizione aree dati.                                       ?
008000090729       //--------------------------------------------------------------
008100090729
008200090729
008300090729       //--------------------------------------------------------------
008400090729       //?Definizione strutture dati.                                  ?
008500090729       //--------------------------------------------------------------
008600090729
008700090729     d TABELfbk        ds                  qualified
008800090729     d  File_Name             83     92                                         * File name
008900090729     d  Library               93    102                                         * Library name
009000090729     d  Db_Rrn               397    400i 0                                      * Relative-rcd-num
009100090729
009200090729     d TNTBEfbk        ds                  qualified
009300090729     d  File_Name             83     92                                         * File name
009400090729     d  Library               93    102                                         * Library name
009500090729     d  Db_Rrn               397    400i 0                                      * Relative-rcd-num
009600090729
009700090729       //--------------------------------------------------------------
009800090729       //?Definizione variabili globali.                               ?
009900090729       //--------------------------------------------------------------
010000090729
010100090729     d wDataIso        s               d   datfmt(*iso)  inz(*loval)
010200090803     d wOutEsito       s             10i 0 inz
010300090803
010400090729       //--------------------------------------------------------------
010500090731       //?Definizione prototipi procedure usate.                       ?
010600090729       //--------------------------------------------------------------
010700090729
010800090803       // - Reperimento dati di una tabella (TABEL o TNTBE)
010900090803      /copy gaitrasrc/srcProtoPr,TRULTAB
011000090731
011100090731       //--------------------------------------------------------------
011200090731       //?Definizione key-list.                                        ?
011300090731       //--------------------------------------------------------------
011400090731
011500090731       // - TABEL00F
011600090731     d k03tabel00    e ds                  extname(TABEL00F:*key)  inz
011700090731
011800090731       // - TNTBE01L
011900090731     d k05tntbe01    e ds                  extname(TNTBE01L:*key)  inz
012000090731
012100090729       //--------------------------------------------------------------
012200090803       //?Procedura getTabella                                        ?
012300090729       //--------------------------------------------------------------
012400090729
012500090729       // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
012600090803       // Procedure name: getTabella
012700090803       // Purpose:        Reperimento dati tabella (TABEL o TNTBE)
012800090803       // Returns:        Integer (Esito)
012900090803       // Parameter:      pInFileTab   => Tipo file in cui accedere
013000090803       //                                 ("1"=TABEL00F, "2"=TNTBE01L)
013100090803       // Parameter:      pInCodiceTab => Codice tabella
013200090803       // Parameter:      pInKey1      => 1ª chiave tabella
013300090803       //                                 (unica per TABEL00F)
013400090803       // Parameter:      pInKey2      => 2ª chiave tabella
013500090803       //                                 (solo per tipo file "2")
013600090803       // Parameter:      pInLingua    => Codice lingua
013700090803       //                                 (solo per tipo file "2")
013800090803       // Parameter:      pInSistInfo  => Nome sistema informativo
013900090803       //                                 (solo per tipo file "2")
014000090803       // Parameter:      pOutLibreria => Nome libreria del file
014100090803       // Parameter:      pOutRrn      => Numero relativo record corrispondente
014200090803       // Parameter:      pOutKey2     => Campo TBLKE2 corrispondente ai dati reperiti
014300090803       //                                 (solo per tipo file "2")
014400090803       // Parameter:      pOutLingua   => Campo TBELIN corrispondente ai dati reperiti
014500090803       //                                 (solo per tipo file "2")
014600090803       // Parameter:      pOutSistInfo => Campo TBESIF corrispondente ai dati reperiti
014700090803       //                                 (solo per tipo file "2")
014800090803       // Parameter:      pOutApplicaz => Campo TBEAPL corrispondente ai dati reperiti
014900090803       //                                 (solo per tipo file "2")
015000090803       // Parameter:      pOutFtt      => Tipo trasmissione
015100090803       // Parameter:      pOutFlt      => Cod. filiale trasmissione dati
015200090803       // Parameter:      pOutFtr      => Flag trasmissione:
015300090803       //                                 " " = record da trasmettere
015400090803       //                                 "T" = record trasmesso
015500090803       //                                 "R" = record ricevuto
015600090803       // Parameter:      pOutDtr      => Data trasmissione
015700090803       // Parameter:      pOutAnnull   => Flag annullamento
015800090803       // Parameter:      pOutUnico    => Campo unico tabella
015900090729       // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
016000090731
016100090803     p getTabella      B                   export
016200090731
016300090803     d getTabella...
016400090803     d                 pi            10i 0
016500090803     d  pInFileTab                    1a                 const
016600090803     d  pInCodiceTab                       like(TBEcod)  const
016700090803     d  pInKey1                            like(TBEke1)  const
016800090803     d  pInKey2                            like(TBEke2)  const
016900090803     d                                      options(*nopass : *omit)
017000090803     d  pInLingua                          like(TBElin)  const
017100090803     d                                      options(*nopass : *omit)
017200090803     d  pInSistInfo                        like(TBEsif)  const
017300090803     d                                      options(*nopass : *omit)
017400090803     d  pOutLibreria                 10a    options(*nopass : *omit)
017500090803     d  pOutRrn                      10i 0  options(*nopass : *omit)
017600090803     d  pOutKey2                           like(TBEke2)
017700090803     d                                      options(*nopass : *omit)
017800090803     d  pOutLingua                         like(TBElin)
017900090803     d                                      options(*nopass : *omit)
018000090803     d  pOutSistInfo                       like(TBEsif)
018100090803     d                                      options(*nopass : *omit)
018200090803     d  pOutApplicaz                       like(TBEapl)
018300090803     d                                      options(*nopass : *omit)
018400090803     d  pOutFtt                            like(TBEftt)
018500090803     d                                      options(*nopass : *omit)
018600090803     d  pOutFlt                            like(TBEflt)
018700090803     d                                      options(*nopass : *omit)
018800090803     d  pOutFtr                            like(TBEftr)
018900090803     d                                      options(*nopass : *omit)
019000090803     d  pOutDtr                            like(TBEdtr)
019100090803     d                                      options(*nopass : *omit)
019200090803     d  pOutAnnull                         like(TBEatb)
019300090803     d                                      options(*nopass : *omit)
019400090803     d  pOutUnico                          like(TBEuni)
019500090803     d                                      options(*nopass : *omit)
019600090729
019700090729      /free
019800090729
019900090729       // -?Pulizia parametri di output?
020000090803       clear  wOutEsito;
020100090804       if  %parms() > 6   and   %addr(pOutLibreria) <> *null;
020200090803         clear  pOutLibreria;
020300090729       endif;
020400090804       if  %parms() > 7   and   %addr(pOutRrn) <> *null;
020500090803         clear  pOutRrn;
020600090729       endif;
020700090804       if  %parms() > 8   and   %addr(pOutKey2) <> *null;
020800090803         clear  pOutKey2;
020900090731       endif;
021000090804       if  %parms() > 9   and   %addr(pOutLingua) <> *null;
021100090803         clear  pOutLingua;
021200090731       endif;
021300090804       if  %parms() > 10  and   %addr(pOutSistInfo) <> *null;
021400090803         clear  pOutSistInfo;
021500090731       endif;
021600090804       if  %parms() > 11  and   %addr(pOutApplicaz) <> *null;
021700090803         clear  pOutApplicaz;
021800090729       endif;
021900090804       if  %parms() > 12  and   %addr(pOutFtt) <> *null;
022000090803         clear  pOutFtt;
022100090729       endif;
022200090804       if  %parms() > 13  and   %addr(pOutFlt) <> *null;
022300090803         clear  pOutFlt;
022400090729       endif;
022500090804       if  %parms() > 14  and   %addr(pOutFtr) <> *null;
022600090803         clear  pOutFtr;
022700090729       endif;
022800090804       if  %parms() > 15  and   %addr(pOutDtr) <> *null;
022900090803         clear  pOutDtr;
023000090729       endif;
023100090804       if  %parms() > 16  and   %addr(pOutAnnull) <> *null;
023200090803         clear  pOutAnnull;
023300090730       endif;
023400090804       if  %parms() > 17  and   %addr(pOutUnico) <> *null;
023500090803         clear  pOutUnico;
023600090729       endif;
023700090729
023800090729       // -?Controllo parametri ricevuti?
023900090803       if     (pInFileTab <> '1'   and   pInFileTab <> '2')
024000091106           or (pInCodiceTab = *blank     and
024100091109               (pInKey1 = *blank   and   %addr(pInKey2)  = *null)
024200091109            or (pInKey1 = *blank   and   %addr(pInKey2) <> *null   and
024300091109                pInKey2 = *blank));
024400090803         wOutEsito = c_ErrParametriNonValidi;
024500090803         return  wOutEsito;
024600090729       endif;
024700090729
024800090729       // -?Aggancio tabella richiesta?
024900090804       SELECT;
025000090731
025100090731         // ·?In file TABEL00F?
025200090804         WHEN  pInFileTab = '1';
025300090804
025400090804           if  NOT %open(TABEL00F);
025500090804             open  TABEL00F;
025600090804           endif;
025700090804
025800130517           if  %parms()  >  4    and  %addr(pInLingua) <> *null  and
025900130517               pInLingua >= '1'  and  pInLingua <= '9';
026000130517             TBLkut = %int(pInLingua);
026100130517           else;
026200130517             TBLkut = 1;
026300130517           endif;
026400090805           TBLcod = pInCodiceTab;
026500090805           TBLkey = pInKey1;
026600090805
026700090805           chain  %kds(k03tabel00)  TABEL;
026800090804
026900090804           if  Not  %found(TABEL00F);
027000090804             wOutEsito = c_ErrTabellaInesistente;
027100090804             return  wOutEsito;
027200090804           endif;
027300090804
027400090804           if  %parms() > 6   and   %addr(pOutLibreria) <> *null;
027500090804             pOutLibreria = TABELfbk.Library;
027600090804           endif;
027700090804           if  %parms() > 7   and   %addr(pOutRrn) <> *null;
027800090804             pOutRrn      = TABELfbk.Db_Rrn;
027900090804           endif;
028000130517           if  %parms() > 9   and   %addr(pOutLingua) <> *null;
028100130517             pOutLingua   = %editc(TBLkut : 'X');
028200130517           endif;
028300090804           if  %parms() > 12  and   %addr(pOutFtt) <> *null;
028400090804             pOutFtt      = TBLftt;
028500090804           endif;
028600090804           if  %parms() > 13  and   %addr(pOutFlt) <> *null;
028700090804             pOutFlt      = TBLflt;
028800090804           endif;
028900090804           if  %parms() > 14  and   %addr(pOutFtr) <> *null;
029000090804             pOutFtr      = TBLftr;
029100090804           endif;
029200090804           if  %parms() > 15  and   %addr(pOutDtr) <> *null
029300090804                              and   TBLdtr > *zero;
029400090804             wDataIso     = %date(TBLdtr : *YMD);
029500090804             pOutDtr      = %dec(wDataIso);
029600090804           endif;
029700090804           if  %parms() > 16  and   %addr(pOutAnnull) <> *null;
029800090804             pOutAnnull   = TBLflg;
029900090804           endif;
030000090804           if  %parms() > 17  and   %addr(pOutUnico) <> *null;
030100090804             pOutUnico    = TBLuni;
030200090804           endif;
030300090731
030400090731         // ·?In file TNTBE01L?
030500090804         WHEN  pInFileTab = '2';
030600090731
030700090804           if  NOT %open(TNTBE01L);
030800090804             open  TNTBE01L;
030900090804           endif;
031000090804
031100090805           TBEcod = pInCodiceTab;
031200090805           TBEke1 = pInKey1;
031300090805           if  %parms() > 3   and   %addr(pInKey2) <> *null;
031400090805             TBEke2 = pInKey2;
031500090805           else;
031600090805             clear  TBEke2;
031700090805           endif;
031800090805           if  %parms() > 4   and   %addr(pInLingua) <> *null;
031900090805             TBElin = pInLingua;
032000090805           else;
032100090805             clear  TBElin;
032200090805           endif;
032300090805           if  %parms() > 5   and   %addr(pInSistInfo) <> *null;
032400090805             TBEsif = pInSistInfo;
032500090805           else;
032600090805             clear  TBEsif;
032700090805           endif;
032800090804
032900090805           select;
033000090805             when  %parms() > 5   and   %addr(pInKey2)     <> *null
033100090805                                  and   %addr(pInLingua)   <> *null
033200090805                                  and   %addr(pInSistInfo) <> *null;
033300090805               chain  %kds(k05tntbe01)  TNTBE000;
033400090805             when  %parms() > 4   and   %addr(pInKey2)   <> *null
033500090805                                  and   %addr(pInLingua) <> *null;
033600090805               chain  %kds(k05tntbe01 : 4)  TNTBE000;
033700090805             when  %parms() > 3   and   %addr(pInKey2) <> *null;
033800090805               chain  %kds(k05tntbe01 : 3)  TNTBE000;
033900090805             other;
034000090805               chain  %kds(k05tntbe01 : 2)  TNTBE000;
034100090805           endsl;
034200090804
034300090804           if  Not  %found(TNTBE01L);
034400090804             wOutEsito = c_ErrTabellaInesistente;
034500090804             return  wOutEsito;
034600090804           endif;
034700090804
034800090804           if  %parms() > 6   and   %addr(pOutLibreria) <> *null;
034900090804             pOutLibreria = TNTBEfbk.Library;
035000090804           endif;
035100090804           if  %parms() > 7   and   %addr(pOutRrn) <> *null;
035200090804             pOutRrn      = TNTBEfbk.Db_Rrn;
035300090804           endif;
035400090804           if  %parms() > 8   and   %addr(pOutKey2) <> *null;
035500090804             pOutKey2     = TBEke2;
035600090804           endif;
035700090804           if  %parms() > 9   and   %addr(pOutLingua) <> *null;
035800090804             pOutLingua   = TBElin;
035900090804           endif;
036000090804           if  %parms() > 10  and   %addr(pOutSistInfo) <> *null;
036100090804             pOutSistInfo = TBEsif;
036200090804           endif;
036300090804           if  %parms() > 11  and   %addr(pOutApplicaz) <> *null;
036400090804             pOutApplicaz = TBEapl;
036500090804           endif;
036600090804           if  %parms() > 12  and   %addr(pOutFtt) <> *null;
036700090804             pOutFtt      = TBEftt;
036800090804           endif;
036900090804           if  %parms() > 13  and   %addr(pOutFlt) <> *null;
037000090804             pOutFlt      = TBEflt;
037100090804           endif;
037200090804           if  %parms() > 14  and   %addr(pOutFtr) <> *null;
037300090804             pOutFtr      = TBEftr;
037400090804           endif;
037500090804           if  %parms() > 15  and   %addr(pOutDtr) <> *null;
037600090804             pOutDtr      = TBEdtr;
037700090804           endif;
037800090804           if  %parms() > 16  and   %addr(pOutAnnull) <> *null;
037900090804             pOutAnnull   = TBEatb;
038000090804           endif;
038100090804           if  %parms() > 17  and   %addr(pOutUnico) <> *null;
038200090804             pOutUnico    = TBEuni;
038300090804           endif;
038400090731
038500090804       ENDSL;
038600090729
038700090804       return  *zero;
038800090731
038900090729      /end-free
039000090731
039100090810     p getTabella      E
039200090810
039300090810
039400090810       // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
039500090810       // Procedure name: cloTabella
039600090810       // Purpose:        chiusura archvi tabelle (TABEL e/o TNTBE)
039700090810       // Returns:        Integer (Esito)
039800090810       // Parameter:      pInFileTab   => File da chiudere
039900090810       //                                 ("1"=TABEL00F, "2"=TNTBE01L)
040000090810       // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
040100090810
040200090810     p cloTabella      B                   export
040300090810
040400090810     d cloTabella...
040500090810     d                 pi            10i 0
040600090810     d  pInFileTab                    1a   const
040700090810     d                                     options(*nopass : *omit)
040800090810
040900090810      /free
041000090810
041100090810       clear  wOutEsito;
041200090810
041300090810       if  (%parms() = *zero   or   %addr(pInFileTab) = *null   or
041400090810                                    pInFileTab = '1')          and
041500090810            %open(TABEL00F);
041600090810         close(E)  TABEL00F;
041700090810         select;
041800090810           when  %error  and  wOutEsito =  *zero;
041900090810             wOutEsito = c_ErrCloseTABEL;
042000090810           when  %error  and  wOutEsito <> *zero;
042100090810             wOutEsito = c_ErrCloseTABELeTNTBE;
042200090810         endsl;
042300090810       endif;
042400090810
042500090810       if  (%parms() = *zero   or   %addr(pInFileTab) = *null   or
042600090810                                    pInFileTab = '2')          and
042700090810            %open(TNTBE01L);
042800090810         close(E)  TNTBE01L;
042900090810         select;
043000090810           when  %error  and  wOutEsito =  *zero;
043100090810             wOutEsito = c_ErrCloseTNTBE;
043200090810           when  %error  and  wOutEsito <> *zero;
043300090810             wOutEsito = c_ErrCloseTABELeTNTBE;
043400090810         endsl;
043500090810       endif;
043600090810
043700090810       return  wOutEsito;
043800090810
043900090810      /end-free
044000090810
044100090810     p cloTabella      E
