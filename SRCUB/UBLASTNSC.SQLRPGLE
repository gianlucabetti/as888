000100111011     /*MODULE
000200111011     /*PRM  dbgview(*source)
000300111024     /*CMD  ovrdbf file(FNBLP00F) tofile(FILTRAPRD/FNBLP00F) +
000400111024     /*CMD         ovrscope(*calllvl)
000500111024     /*END  dltovr file(FNBLP00F) lvl(*)
000600111011     /*END
000700111011       //==============================================================
000800111011       //
000900111011       // Questo modulo estrae l'ultimo segnacollo utilizzato per la
001000111011       //   serie ricevuta come parametro (dalle bolle in partenza).
001100111011       //
001200111011       // Può essere usato solo come sottoprocedura.
001300111011       // Ogni metodo restituisce un valore 10I:
001400111011       //    · = *zero => tutto ok
001500111011       //    · < *zero => errore
001600111011       //
001700111011       // _____________________________________________
001800111011       // PARAMETRI DELLA SOTTOPROCEDURA ubLastNSC_Init
001900111011       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
002000111011       // Posizione Uso Tipo      Descrizione
002100111011       // --------- --- --------- -------------------------------------
002200111011       //     /      /      /           /
002300111011       // --------- --- --------- -------------------------------------
002400111011       //
002500111011       // ____________________________________________
002600111011       // PARAMETRI DELLA SOTTOPROCEDURA ubLastNSC_Rtv
002700111011       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
002800111011       // Posizione Uso Tipo      Descrizione
002900111011       // --------- --- --------- -------------------------------------
003000111011       //     1     IN  SIGNED    Numero Serie
003100111024       //     2     IN  SIGNED    Linea di Partenza
003200111024       //     3     IN  SIGNED    Famiglia Clienti  (facoltativo)
003300111024       //     4     IN  SIGNED    Anno Spedizione   (facoltativo)
003400140225       //     5     OUT SIGNED    Numero Ultimo Segnacollo (n° maggiore)
003500111024       //     6     OUT SIGNED    Linea di Partenza
003600111024       //     7     OUT SIGNED    Anno Spedizione
003700111024       //     8     OUT SIGNED    Mese/Giorno Spedizione
003800111024       //     9     OUT SIGNED    Numero Spedizione
003900140225       //    10     OUT SIGNED    Numero Ultimo Segnacollo (più recente)
004000140225       //    11     OUT SIGNED    Linea di Partenza
004100140225       //    12     OUT SIGNED    Anno Spedizione
004200140225       //    13     OUT SIGNED    Mese/Giorno Spedizione
004300140225       //    14     OUT SIGNED    Numero Spedizione
004400111011       // --------- --- --------- -------------------------------------
004500111011       //
004600111011       // ______________________________________________
004700111011       // PARAMETRI DELLA SOTTOPROCEDURA ubLastNSC_Close
004800111011       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
004900111011       // Posizione Uso Tipo      Descrizione
005000111011       // --------- --- --------- -------------------------------------
005100111011       //     /      /      /           /
005200111011       // --------- --- --------- -------------------------------------
005300111011       //
005400111011       // __________________
005500111011       // ESEMPI DI CHIAMATA
005600111011       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
005700121121       // if ubLastNSC_Rtv ( pInSerie : pInFilSgncl :
005800111024       //                    pInLeg3C_skC : pInAnno :
005900111024       //                    pOutLastNSC : pOutLineaPar : pOutAnno :
006000111024       //                    pOutMmGg : pOutSpediz ) = *off;
006100111011       //
006200111011       // ______________________________
006300111011       // ISTRUZIONI PER LA COMPILAZIONE
006400111011       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
006500111011       // 1. Creare il modulo UBLASTNSC (Opz 15 PDM).
006600111011       // 2. Creare/Aggiornare il programma di servizio UBLASTNSC
006700111011       //    (*cmd  CRTSRVPGM / UPDSRVPGM).
006800111011       // 4. Cancellare il modulo.
006900111011       //
007000111011       //==============================================================
007100111011
007200111011     h BndDir('UBBNDDIR':'UBRTVNETA')
007300111011     h NoMain
007400111011
007500111011       //--------------------------------------------------------------
007600111011       //?Definizione archivi.                                         ?
007700111011       //--------------------------------------------------------------
007800111011
007900111011       //--------------------------------------------------------------
008000111011       //?Definizione costanti.                                        ?
008100111011       //--------------------------------------------------------------
008200111011
008300111011       // -?Errori gestiti?
008400111011     d c_ErrRepCurrSysNetA...
008500111011     d                 c                   const(-1)
008600111011     d c_ErrSQL...
008700111011     d                 c                   const(-2)
008800111011     d*// c_ErrOpenFNBLP...
008900111011     d*//                 c                   const(-3)
009000111011     d*// c_ErrCloseFNBLP...
009100111011     d*//                 c                   const(-4)
009200111011     d c_ErrNotFoundSped...
009300111011     d                 c                   const(1)
009400111011
009500111011       //--------------------------------------------------------------
009600111011       //?Definizione prototipi procedure.                             ?
009700111011       //--------------------------------------------------------------
009800111011
009900111011       // -?Reperimento Ultimo Segnacollo utilizzato?
010000111011     d/copy gaitrasrc/srcProtoPr,ubLastNSC
010100111011     d/copy gaitrasrc/srcProtoPi,ubLastNSC
010200111011
010300111011       // -?Reperimento NETA sistema AS/400 corrente?
010400111011     d/copy gaitrasrc/srcProtoPr,ubRtvNetA
010500111011
010600111011       // -?Parametri API QCAPCMD (Process Commands)?
010700111011     d Qcmd            s           2048    inz  varying
010800111011      /copy qSysInc/qRpgleSrc,QCAPCMD
010900111011       // -?API QCAPCMD (Process Commands)?
011000111011      /copy gaitrasrc/srcProtoPR,QCAPCMD
011100111011
011200111011       // -?Parametri gestione errori API.?
011300111011      /copy qsysinc/qrpglesrc,QUSEC
011400111011
011500111011       //--------------------------------------------------------------
011600111011       //?Definizione strutture dati globali.                          ?
011700111011       //--------------------------------------------------------------
011800111011
011900111011       //--------------------------------------------------------------
012000111011       //?Definizione variabili globali.                               ?
012100111011       //--------------------------------------------------------------
012200111011
012300111011       // -?Valori di ritorno?
012400111011     d retField        s             10i 0 inz
012500140225     d WretField1      s                   like(retField)         inz
012600140225     d WretField2      s                   like(retField)         inz
012700111011
012800111011       // -?Flags booleani?
012900111011     d $InitDone       s               n   inz
013000111011
013100111011       // -?Nome della libreria in cui è il file FNBLP?
013200111011     d wLibFile        s             21a   inz
013300111011
013400111011       // -?Stringa SQL da eseguire?
013500111011     d wSql            s           4096    inz  varying
013600140225     d wSaveSql        s                   inz  like(wSql)
013700111011
013800111011       // -?Dati estratti via SQL?
013900111011     d FNBLPds         ds                  inz
014000111011     d   sql_AAS                           like(ds_FNBLP.BLPaas)  inz
014100111024     d   sql_MGS                           like(ds_FNBLP.BLPaas)  inz
014200111011     d   sql_LNP                           like(ds_FNBLP.BLPlnp)  inz
014300111011     d*//sql_NRS                           like(ds_FNBLP.BLPnrs)  inz
014400111011     d   sql_NSP                           like(ds_FNBLP.BLPnsp)  inz
014500111011     d   sql_NCD                           like(ds_FNBLP.BLPncd)  inz
014600111011
014700111011       //--------------------------------------------------------------
014800111011       //?Definizione key-list globali.                                ?
014900111011       //--------------------------------------------------------------
015000111011
015100111011       //==============================================================
015200111011       //?Procedure name: ubLastNSC_Init                               ?
015300111011       // Purpose:        Inizializzazione variabili di lavoro
015400111011       // Returns:        Integer (Status_Code)
015500111011       //==============================================================
015600111011
015700111011     P ubLastNSC_Init...
015800111011     P                 B                   export
015900111011
016000111011     d ubLastNSC_Init...
016100111011     d                 PI            10i 0
016200111011
016300111011       // -?Definizione variabili locali.                                ?
016400111011       // -?Nome del sistema?
016500111011     d currSysNeta     s              8a   inz
016600111011
016700111011      /free
016800111011
016900111011       // -?Inizializzazione parametri di output?
017000111011       reset  retField;
017100111011       reset  $InitDone;
017200111011
017300111011       // -?Verifica del sistema AS/400 corrente?
017400111011       if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
017500111011         retField = c_ErrRepCurrSysNetA;
017600111011         return  retField;
017700111011       endif;
017800111011
017900111011       // -?Impostazione libreria per archivio FNBLP00F?
018000111011       if %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
018100111011         wLibFile = 'FILTRA201';
018200111011       else;
018300111011         wLibFile = 'FILTRAPRD';
018400111011       endif;
018500111011       wLibFile = %trimr( wLibFile ) + '/FNBLP00F';
018600111011
018700111011       // -?Valorizzazione opcode di metodo?
018800111011       $InitDone = *on;
018900111011
019000111011       // -?Uscita?
019100111011       return  retField;
019200111011
019300111011      /end-free
019400111011
019500111011     P ubLastNSC_Init...
019600111011     P                 E
019700111011
019800111011
019900111011       //==============================================================
020000111011       //?Procedure name: ubLastNSC_Rtv                                 ?
020100111024       // Purpose:        Reperimento ultimo segnacollo per LnP/NrS da
020200111011       //                 FNBLP
020300111011       // Returns:        Integer (Status_Code)
020400111011       // Parameter:      pInSerie      => Numero Serie
020500121121       // Parameter:      pInFilSgncl   => Filiale Segnacollo
020600111024       // Parameter:      pInLeg3C_skC  => Famiglia di clienti
020700111011       // Parameter:      pInAnno       => Anno Spedizione
020800140225       // Parameter:      pOutMaxNSC    => Numero Segnacollo maggiore
020900140225       // Parameter:      pOutMaxLNP    => Linea di Partenza
021000140225       // Parameter:      pOutMaxAAS    => Anno Spedizione
021100140225       // Parameter:      pOutMaxMGS    => Mese/Giorno Spedizione
021200140225       // Parameter:      pOutMaxNSP    => Numero Spedizione
021300140225       // Parameter:      pOutLastNSC   => Ultimo Numero Segnacollo
021400140225       // Parameter:      pOutLastLNP   => Linea di Partenza
021500140225       // Parameter:      pOutLastAAS   => Anno Spedizione
021600140225       // Parameter:      pOutLastMGS   => Mese/Giorno Spedizione
021700140225       // Parameter:      pOutLastNSP   => Numero Spedizione
021800111011       //==============================================================
021900111011
022000111011     P ubLastNSC_Rtv...
022100111011     P                 B                   export
022200111011
022300111011     d ubLastNSC_Rtv...
022400111011     d                 PI            10i 0
022500111011     d  pInSerie                           like(ds_FNBLP.BLPnrs)  const
022600121121     d*/pInLineaPar/*                      like(ds_FNBLP.BLPlnp)  const
022700121121     d  pInFilSgncl                        like(ds_FNBLP.BLPfls)  const
022800111024     d  pInLeg3C_SKC               3200a                          const
022900111024     d                                     options(*varsize :
023000111024     d                                             *omit)
023100111011     d  pInAnno                            like(ds_FNBLP.BLPaas)  const
023200111024     d                                     options(*omit)
023300140225     d  pOutMaxNSC                         like(ds_FNBLP.BLPncd)
023400140225     d  pOutMaxLNP                         like(ds_FNBLP.BLPlnp)
023500140225     d  pOutMaxAAS                         like(ds_FNBLP.BLPaas)
023600140225     d  pOutMaxMGS                         like(ds_FNBLP.BLPmgs)
023700140225     d  pOutMaxNSP                         like(ds_FNBLP.BLPnsp)
023800140225     d  pOutLastNSC                        like(ds_FNBLP.BLPncd)
023900140225     d  pOutLastLNP                        like(ds_FNBLP.BLPlnp)
024000140225     d  pOutLastAAS                        like(ds_FNBLP.BLPaas)
024100140225     d  pOutLastMGS                        like(ds_FNBLP.BLPmgs)
024200140225     d  pOutLastNSP                        like(ds_FNBLP.BLPnsp)
024300111024
024400111024       //______________________________________________________________
024500111024       //?Definizione variabili.                                       ?
024600111024       //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
024700111024
024800111024       // -?Indici di schiera?
024900111024     d xx              s              3  0 inz
025000111024
025100111024       //______________________________________________________________
025200111024       //?Definizione prototipi procedure e relativi parametri.        ?
025300111024       //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
025400111024
025500111024       // -?Reperimento legami in tab. "3C"?
025600111024      /copy gaitrasrc/srcProtoPI,ubLeg3C
025700111024
025800111024       //______________________________________________________________
025900111024       //?Specifiche di calcolo.                                       ?
026000111024       //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
026100111011
026200111011      /free
026300111011
026400111011       // -?Inizializzazione dei parametri di output?
026500111011       reset  retField;
026600140225       clear  pOutMaxNSC;
026700140225       clear  pOutMaxLNP;
026800140225       clear  pOutMaxAAS;
026900140225       clear  pOutMaxMGS;
027000140225       clear  pOutMaxNSP;
027100140225       clear  pOutLastNSC;
027200140225       clear  pOutLastLNP;
027300140225       clear  pOutLastAAS;
027400140225       clear  pOutLastMGS;
027500140225       clear  pOutLastNSP;
027600111011
027700111011       // -?Se non già inizializzata la procedure:?
027800111011       //  ?=> si forza l'inizializzazione?
027900111011       if  not $InitDone;
028000111011         retField = ubLastNSC_Init;
028100111011       endif;
028200111011       // -?Se ricevuto errore => uscita?
028300111011       if  retField < *zero;
028400111011         return  retField;
028500111011       endif;
028600111024
028700111024       if  %parms() > 2  and  %addr(pInLeg3C_skC) <> *null;
028800111024         ubLeg3C_skC = pInLeg3C_skC;
028900111024       endif;
029000111011
029100140225       // -?Preparazione stringa SQL?
029200140225       //  ?per reperimento segnacollo della serie?
029300111024       //  ?da spedizioni in partenza?
029400140225       wSaveSql = 'select BLPaas, BLPmgs, BLPlnp, BLPnsp, BLPncd +
029500140225                     from ' + %trimr(wLibFile) +
029600140225                  ' where BLPnrs = ' + %char(pInSerie) +
029700140225                    ' and BLPncd > 0';
029800111011       if  %addr(pInAnno) <> *null  and  pInAnno > *zero;
029900140225         wSaveSql += ' and BLPaas = ' + %char(pInAnno);
030000111011       endif;
030100121121       //if  %addr(pInLineaPar) <> *null  and  pInLineaPar > *zero;
030200140225       //  wSaveSql += ' and BLPlnp = ' + %char(pInLineaPar);
030300121121       if  %addr(pInFilSgncl) <> *null  and  pInFilSgncl > *zero;
030400140225         wSaveSql += ' and BLPfls = ' + %char(pInFilSgncl);
030500111011       endif;
030600111024       if  %addr(pInLeg3C_skC) <> *null  and  sch_SKC(1) > *zero;
030700140225         wSaveSql += ' and BLPksc in (';
030800111024         For  xx = 1  To  %elem(sch_SKC);
030900111024           Select;
031000111024             When  sch_SKC(xx) = *zero;
031100111024               leave;
031200111024             When  xx = 1;
031300140225               wSaveSql += %triml( %editc( sch_SKC(xx) : '3' ) );
031400111024             Other;
031500140225               wSaveSql += ', ' + %triml( %editc( sch_SKC(xx) : '3' ) );
031600111024           EndSl;
031700111024         EndFor;
031800140225         wSaveSql += ')';
031900111024       endif;
032000140225
032100140225       // -?Ordinamento?
032200140225       //  ?per reperimento segnacollo maggiore (per serie)?
032300140225       wSql = wSaveSql + ' order by BLPncd desc';
032400111011
032500111011       exec sql  prepare S1  from :wSql;
032600111011
032700111011       // -?Dichiarazione ed apertura cursore SQL?
032800111011       exec sql  declare C1  cursor for S1;
032900111011       exec sql  open C1;
033000111011
033100111011       // -?Reperimento ultimo segnacollo (per serie)?
033200111011       //  ?da spedizioni in partenza?
033300111011       clear  FNBLPds;
033400111011       exec sql  fetch next  from C1  into :FNBLPds;
033500111011
033600111011       select;
033700111011         // -?Rilevato Errore Sql?
033800111011         when  SQLcode < *zero;
033900111011           Dump(A);
034000111011           Qcmd = 'DSPJOBLOG job(*) output(*print)';
034100111011           clear Qcap0100;
034200111011           Qcabcsdh = *off;
034300111011           Qcapa    = *off;
034400111011           Qcacmdss = *off;
034500111011           Qcaerved = *allX'00';
034600111011           clear Qusec;
034700111011           Qusbprv  = %size(Qusec);
034800111011           ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
034900111011                             %size(Qcap0100) : 'CPOP0100' : *OMIT :
035000111011                             0 : 0 : Qusec);
035100111011           retField = c_ErrSQL;
035200111011         // -?Non reperita nessuna spedizione?
035300111011         when  SQLcode = 100;
035400111011           retField = c_ErrNotFoundSped;
035500111011         // -?Impostazione parametri di output?
035600111011         other;
035700140225           pOutMaxNSC = Sql_NCD;
035800140225           pOutMaxLNP = Sql_LNP;
035900140225           pOutMaxAAS = Sql_AAS;
036000140225           pOutMaxMGS = Sql_MGS;
036100140225           pOutMaxNSP = Sql_NSP;
036200111011       endsl;
036300140225
036400140225       // -?Salvataggio ritorno (1)?
036500140225       WretField1 = retField;
036600140225
036700140225       // -?Chiusura cursori SQL & Uscita?
036800140225       //  ?(SE rilevato errore SQL o NON reperiti dati)?
036900140225       if  WretField1 <> *zero;
037000140225         retField  = ubLastNSC_Close;
037100140225         return  WretField1;
037200140225       endif;
037300140225
037400140225       // -?Ordinamento?
037500140225       //  ?per reperimento ultimo segnacollo (per serie)?
037600140225       wSql = wSaveSql + ' order by BLPaas desc, BLPmgs desc, BLPncd desc';
037700140225
037800140225       exec sql  prepare S2  from :wSql;
037900140225
038000140225       // -?Dichiarazione ed apertura cursore SQL?
038100140225       exec sql  declare C2  cursor for S2;
038200140225       exec sql  open C2;
038300140225
038400140225       // -?Reperimento ultimo segnacollo (per serie)?
038500140225       //  ?da spedizioni in partenza?
038600140225       clear  FNBLPds;
038700140225       exec sql  fetch next  from C2  into :FNBLPds;
038800140225
038900140225       select;
039000140225         // -?Rilevato Errore Sql?
039100140225         when  SQLcode < *zero;
039200140225           Dump(A);
039300140225           Qcmd = 'DSPJOBLOG job(*) output(*print)';
039400140225           clear Qcap0100;
039500140225           Qcabcsdh = *off;
039600140225           Qcapa    = *off;
039700140225           Qcacmdss = *off;
039800140225           Qcaerved = *allX'00';
039900140225           clear Qusec;
040000140225           Qusbprv  = %size(Qusec);
040100140225           ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
040200140225                             %size(Qcap0100) : 'CPOP0100' : *OMIT :
040300140225                             0 : 0 : Qusec);
040400140225           retField = c_ErrSQL;
040500140225         // -?Non reperita nessuna spedizione?
040600140225         when  SQLcode = 100;
040700140225           retField = c_ErrNotFoundSped;
040800140225         // -?Impostazione parametri di output?
040900140225         other;
041000140225           pOutLastNSC = Sql_NCD;
041100140225           pOutLastLNP = Sql_LNP;
041200140225           pOutLastAAS = Sql_AAS;
041300140225           pOutLastMGS = Sql_MGS;
041400140225           pOutLastNSP = Sql_NSP;
041500140225       endsl;
041600140225
041700140225       // -?Salvataggio ritorno (2)?
041800140225       WretField2 = retField;
041900140225
042000140225       // -?Chiusura cursori SQL?
042100140225       retField  = ubLastNSC_Close;
042200111011
042300111011       // -?Uscita?
042400140225       select;
042500140225         when  WretField1 <> *zero;
042600140225           return  WretField1;
042700140225         when  WretField2 <> *zero;
042800140225           return  WretField2;
042900140225         other;
043000140225           return  retField;
043100140225       endsl;
043200111011
043300111011      /end-free
043400111011
043500111011     P ubLastNSC_Rtv...
043600111011     P                 E
043700111011
043800111011       //==============================================================
043900111011       //?Procedure name: ubLastNSC_Close                              ?
044000111011       // Purpose:        Chiusura archivi
044100111011       // Returns:        Integer (Status_Code)
044200111011       //==============================================================
044300111011
044400111011     P ubLastNSC_Close...
044500111011     P                 B                   export
044600111011
044700111011     d ubLastNSC_Close...
044800111011     d                 PI            10i 0
044900111011
045000111011      /free
045100111011
045200111011       // -?Inizializzazione dei parametri di output?
045300111011       reset  retField;
045400111011
045500140225       // -?Chiusura cursori SQL?
045600111011       exec sql  close C1;
045700140225       exec sql  close C2;
045800111011
045900111011       // -?Valorizzazione opcode di metodo?
046000111011       $InitDone = *off;
046100111011
046200111011       // -?Uscita?
046300111011       return  retField;
046400111011
046500111011      /end-free
046600111011
046700111011     P ubLastNSC_Close...
046800111011     P                 E
