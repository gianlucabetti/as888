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
003400111024       //     5     OUT SIGNED    Numero Ultimo Segnacollo
003500111024       //     6     OUT SIGNED    Linea di Partenza
003600111024       //     7     OUT SIGNED    Anno Spedizione
003700111024       //     8     OUT SIGNED    Mese/Giorno Spedizione
003800111024       //     9     OUT SIGNED    Numero Spedizione
003900111011       // --------- --- --------- -------------------------------------
004000111011       //
004100111011       // ______________________________________________
004200111011       // PARAMETRI DELLA SOTTOPROCEDURA ubLastNSC_Close
004300111011       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
004400111011       // Posizione Uso Tipo      Descrizione
004500111011       // --------- --- --------- -------------------------------------
004600111011       //     /      /      /           /
004700111011       // --------- --- --------- -------------------------------------
004800111011       //
004900111011       // __________________
005000111011       // ESEMPI DI CHIAMATA
005100111011       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
005200121121       // if ubLastNSC_Rtv ( pInSerie : pInFilSgncl :
005300111024       //                    pInLeg3C_skC : pInAnno :
005400111024       //                    pOutLastNSC : pOutLineaPar : pOutAnno :
005500111024       //                    pOutMmGg : pOutSpediz ) = *off;
005600111011       //
005700111011       // ______________________________
005800111011       // ISTRUZIONI PER LA COMPILAZIONE
005900111011       // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
006000111011       // 1. Creare il modulo UBLASTNSC (Opz 15 PDM).
006100111011       // 2. Creare/Aggiornare il programma di servizio UBLASTNSC
006200111011       //    (*cmd  CRTSRVPGM / UPDSRVPGM).
006300111011       // 4. Cancellare il modulo.
006400111011       //
006500111011       //==============================================================
006600111011
006700111011     h BndDir('UBBNDDIR':'UBRTVNETA')
006800111011     h NoMain
006900111011
007000111011       //--------------------------------------------------------------
007100111011       //?Definizione archivi.                                         ?
007200111011       //--------------------------------------------------------------
007300111011
007400111011       //--------------------------------------------------------------
007500111011       //?Definizione costanti.                                        ?
007600111011       //--------------------------------------------------------------
007700111011
007800111011       // -?Errori gestiti?
007900111011     d c_ErrRepCurrSysNetA...
008000111011     d                 c                   const(-1)
008100111011     d c_ErrSQL...
008200111011     d                 c                   const(-2)
008300111011     d*// c_ErrOpenFNBLP...
008400111011     d*//                 c                   const(-3)
008500111011     d*// c_ErrCloseFNBLP...
008600111011     d*//                 c                   const(-4)
008700111011     d c_ErrNotFoundSped...
008800111011     d                 c                   const(1)
008900111011
009000111011       //--------------------------------------------------------------
009100111011       //?Definizione prototipi procedure.                             ?
009200111011       //--------------------------------------------------------------
009300111011
009400111011       // -?Reperimento Ultimo Segnacollo utilizzato?
009500111011     d/copy gaitrasrc/srcProtoPr,ubLastNSC
009600111011     d/copy gaitrasrc/srcProtoPi,ubLastNSC
009700111011
009800111011       // -?Reperimento NETA sistema AS/400 corrente?
009900111011     d/copy gaitrasrc/srcProtoPr,ubRtvNetA
010000111011
010100111011       // -?Parametri API QCAPCMD (Process Commands)?
010200111011     d Qcmd            s           2048    inz  varying
010300111011      /copy qSysInc/qRpgleSrc,QCAPCMD
010400111011       // -?API QCAPCMD (Process Commands)?
010500111011      /copy gaitrasrc/srcProtoPR,QCAPCMD
010600111011
010700111011       // -?Parametri gestione errori API.?
010800111011      /copy qsysinc/qrpglesrc,QUSEC
010900111011
011000111011       //--------------------------------------------------------------
011100111011       //?Definizione strutture dati globali.                          ?
011200111011       //--------------------------------------------------------------
011300111011
011400111011       //--------------------------------------------------------------
011500111011       //?Definizione variabili globali.                               ?
011600111011       //--------------------------------------------------------------
011700111011
011800111011       // -?Valori di ritorno?
011900111011     d retField        s             10i 0 inz
012000111024     d WretField       s                   like(retField)         inz
012100111011
012200111011       // -?Flags booleani?
012300111011     d $InitDone       s               n   inz
012400111011
012500111011       // -?Nome della libreria in cui è il file FNBLP?
012600111011     d wLibFile        s             21a   inz
012700111011
012800111011       // -?Stringa SQL da eseguire?
012900111011     d wSql            s           4096    inz  varying
013000111011
013100111011       // -?Dati estratti via SQL?
013200111011     d FNBLPds         ds                  inz
013300111011     d   sql_AAS                           like(ds_FNBLP.BLPaas)  inz
013400111024     d   sql_MGS                           like(ds_FNBLP.BLPaas)  inz
013500111011     d   sql_LNP                           like(ds_FNBLP.BLPlnp)  inz
013600111011     d*//sql_NRS                           like(ds_FNBLP.BLPnrs)  inz
013700111011     d   sql_NSP                           like(ds_FNBLP.BLPnsp)  inz
013800111011     d   sql_NCD                           like(ds_FNBLP.BLPncd)  inz
013900111011
014000111011       //--------------------------------------------------------------
014100111011       //?Definizione key-list globali.                                ?
014200111011       //--------------------------------------------------------------
014300111011
014400111011       //==============================================================
014500111011       //?Procedure name: ubLastNSC_Init                               ?
014600111011       // Purpose:        Inizializzazione variabili di lavoro
014700111011       // Returns:        Integer (Status_Code)
014800111011       //==============================================================
014900111011
015000111011     P ubLastNSC_Init...
015100111011     P                 B                   export
015200111011
015300111011     d ubLastNSC_Init...
015400111011     d                 PI            10i 0
015500111011
015600111011       // -?Definizione variabili locali.                                ?
015700111011       // -?Nome del sistema?
015800111011     d currSysNeta     s              8a   inz
015900111011
016000111011      /free
016100111011
016200111011       // -?Inizializzazione parametri di output?
016300111011       reset  retField;
016400111011       reset  $InitDone;
016500111011
016600111011       // -?Verifica del sistema AS/400 corrente?
016700111011       if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
016800111011         retField = c_ErrRepCurrSysNetA;
016900111011         return  retField;
017000111011       endif;
017100111011
017200111011       // -?Impostazione libreria per archivio FNBLP00F?
017300111011       if %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
017400111011         wLibFile = 'FILTRA201';
017500111011       else;
017600111011         wLibFile = 'FILTRAPRD';
017700111011       endif;
017800111011       wLibFile = %trimr( wLibFile ) + '/FNBLP00F';
017900111011
018000111011       // -?Valorizzazione opcode di metodo?
018100111011       $InitDone = *on;
018200111011
018300111011       // -?Uscita?
018400111011       return  retField;
018500111011
018600111011      /end-free
018700111011
018800111011     P ubLastNSC_Init...
018900111011     P                 E
019000111011
019100111011
019200111011       //==============================================================
019300111011       //?Procedure name: ubLastNSC_Rtv                                 ?
019400111024       // Purpose:        Reperimento ultimo segnacollo per LnP/NrS da
019500111011       //                 FNBLP
019600111011       // Returns:        Integer (Status_Code)
019700111011       // Parameter:      pInSerie      => Numero Serie
019800121121       // Parameter:      pInFilSgncl   => Filiale Segnacollo
019900111024       // Parameter:      pInLeg3C_skC  => Famiglia di clienti
020000111011       // Parameter:      pInAnno       => Anno Spedizione
020100111011       // Parameter:      pOutLastNSC   => Ultimo Numero Segnacollo
020200111011       // Parameter:      pOutLineaPar  => Linea di Partenza
020300111011       // Parameter:      pOutAnno      => Anno Spedizione
020400111024       // Parameter:      pOutMmGg      => Mese/Giorno Spedizione
020500111011       // Parameter:      pOutSpediz    => Numero Spedizione
020600111011       //==============================================================
020700111011
020800111011     P ubLastNSC_Rtv...
020900111011     P                 B                   export
021000111011
021100111011     d ubLastNSC_Rtv...
021200111011     d                 PI            10i 0
021300111011     d  pInSerie                           like(ds_FNBLP.BLPnrs)  const
021400121121     d*/pInLineaPar/*                      like(ds_FNBLP.BLPlnp)  const
021500121121     d  pInFilSgncl                        like(ds_FNBLP.BLPfls)  const
021600111024     d  pInLeg3C_SKC               3200a                          const
021700111024     d                                     options(*varsize :
021800111024     d                                             *omit)
021900111011     d  pInAnno                            like(ds_FNBLP.BLPaas)  const
022000111024     d                                     options(*omit)
022100111011     d  pOutLastNSC                        like(ds_FNBLP.BLPncd)
022200111011     d  pOutLineaPar                       like(ds_FNBLP.BLPlnp)
022300111011     d  pOutAnno                           like(ds_FNBLP.BLPaas)
022400111024     d  pOutMmGg                           like(ds_FNBLP.BLPmgs)
022500111011     d  pOutSpediz                         like(ds_FNBLP.BLPnsp)
022600111024
022700111024       //______________________________________________________________
022800111024       //?Definizione variabili.                                       ?
022900111024       //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
023000111024
023100111024       // -?Indici di schiera?
023200111024     d xx              s              3  0 inz
023300111024
023400111024       //______________________________________________________________
023500111024       //?Definizione prototipi procedure e relativi parametri.        ?
023600111024       //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
023700111024
023800111024       // -?Reperimento legami in tab. "3C"?
023900111024      /copy gaitrasrc/srcProtoPI,ubLeg3C
024000111024
024100111024       //______________________________________________________________
024200111024       //?Specifiche di calcolo.                                       ?
024300111024       //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
024400111011
024500111011      /free
024600111011
024700111011       // -?Inizializzazione dei parametri di output?
024800111011       reset  retField;
024900111011       clear  pOutLastNSC;
025000111011       clear  pOutLineaPar;
025100111011       clear  pOutAnno;
025200111024       clear  pOutMmGg;
025300111011       clear  pOutSpediz;
025400111011
025500111011       // -?Se non già inizializzata la procedure:?
025600111011       //  ?=> si forza l'inizializzazione?
025700111011       if  not $InitDone;
025800111011         retField = ubLastNSC_Init;
025900111011       endif;
026000111011       // -?Se ricevuto errore => uscita?
026100111011       if  retField < *zero;
026200111011         return  retField;
026300111011       endif;
026400111024
026500111024       if  %parms() > 2  and  %addr(pInLeg3C_skC) <> *null;
026600111024         ubLeg3C_skC = pInLeg3C_skC;
026700111024       endif;
026800111011
026900111024       // -?Preparazione stringa SQL per?
027000111024       //  ?reperimento ultimo segnacollo (per serie)?
027100111024       //  ?da spedizioni in partenza?
027200111024       wSql = 'select BLPaas, BLPmgs, BLPlnp, BLPnsp, BLPncd +
027300111011                 from ' + %trimr(wLibFile) +
027400111024              ' where BLPnrs = ' + %char(pInSerie) +
027500111024                ' and BLPncd > 0';
027600111011       if  %addr(pInAnno) <> *null  and  pInAnno > *zero;
027700111011         wSql += ' and BLPaas = ' + %char(pInAnno);
027800111011       endif;
027900121121       //if  %addr(pInLineaPar) <> *null  and  pInLineaPar > *zero;
028000121121       //  wSql += ' and BLPlnp = ' + %char(pInLineaPar);
028100121121       if  %addr(pInFilSgncl) <> *null  and  pInFilSgncl > *zero;
028200121121         wSql += ' and BLPfls = ' + %char(pInFilSgncl);
028300111011       endif;
028400111024       if  %addr(pInLeg3C_skC) <> *null  and  sch_SKC(1) > *zero;
028500111024         wSql += ' and BLPksc in (';
028600111024         For  xx = 1  To  %elem(sch_SKC);
028700111024           Select;
028800111024             When  sch_SKC(xx) = *zero;
028900111024               leave;
029000111024             When  xx = 1;
029100111024               wSql += %triml( %editc( sch_SKC(xx) : '3' ) );
029200111024             Other;
029300111024               wSql += ', ' + %triml( %editc( sch_SKC(xx) : '3' ) );
029400111024           EndSl;
029500111024         EndFor;
029600111024         wSql += ')';
029700111024       endif;
029800111011       wSql += ' order by BLPncd desc';
029900111011
030000111011       exec sql  prepare S1  from :wSql;
030100111011
030200111011       // -?Dichiarazione ed apertura cursore SQL?
030300111011       exec sql  declare C1  cursor for S1;
030400111011       exec sql  open C1;
030500111011
030600111011       // -?Reperimento ultimo segnacollo (per serie)?
030700111011       //  ?da spedizioni in partenza?
030800111011       clear  FNBLPds;
030900111011       exec sql  fetch next  from C1  into :FNBLPds;
031000111011
031100111011       select;
031200111011         // -?Rilevato Errore Sql?
031300111011         when  SQLcode < *zero;
031400111011           Dump(A);
031500111011           Qcmd = 'DSPJOBLOG job(*) output(*print)';
031600111011           clear Qcap0100;
031700111011           Qcabcsdh = *off;
031800111011           Qcapa    = *off;
031900111011           Qcacmdss = *off;
032000111011           Qcaerved = *allX'00';
032100111011           clear Qusec;
032200111011           Qusbprv  = %size(Qusec);
032300111011           ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
032400111011                             %size(Qcap0100) : 'CPOP0100' : *OMIT :
032500111011                             0 : 0 : Qusec);
032600111011           retField = c_ErrSQL;
032700111011         // -?Non reperita nessuna spedizione?
032800111011         when  SQLcode = 100;
032900111011           retField = c_ErrNotFoundSped;
033000111011         // -?Impostazione parametri di output?
033100111011         other;
033200111024           pOutLastNSC  = Sql_NCD;
033300111024           pOutLineaPar = Sql_LNP;
033400111011           pOutAnno     = Sql_AAS;
033500111024           pOutMmGg     = Sql_MGS;
033600111011           pOutSpediz   = Sql_NSP;
033700111011       endsl;
033800111011
033900111011       // -?Chiusura cursore SQL?
034000111024       WretField = retField;
034100111024       retField  = ubLastNSC_Close;
034200111011
034300111011       // -?Uscita?
034400111024       if  WretField <> *zero;
034500111024         return  WretField;
034600111024       else;
034700111024         return  retField;
034800111024       endif;
034900111011
035000111011      /end-free
035100111011
035200111011     P ubLastNSC_Rtv...
035300111011     P                 E
035400111011
035500111011       //==============================================================
035600111011       //?Procedure name: ubLastNSC_Close                              ?
035700111011       // Purpose:        Chiusura archivi
035800111011       // Returns:        Integer (Status_Code)
035900111011       //==============================================================
036000111011
036100111011     P ubLastNSC_Close...
036200111011     P                 B                   export
036300111011
036400111011     d ubLastNSC_Close...
036500111011     d                 PI            10i 0
036600111011
036700111011      /free
036800111011
036900111011       // -?Inizializzazione dei parametri di output?
037000111011       reset  retField;
037100111011
037200111011       // -?Chiusura cursore SQL?
037300111011       exec sql  close C1;
037400111011
037500111011       // -?Valorizzazione opcode di metodo?
037600111011       $InitDone = *off;
037700111011
037800111011       // -?Uscita?
037900111011       return  retField;
038000111011
038100111011      /end-free
038200111011
038300111011     P ubLastNSC_Close...
038400111011     P                 E
