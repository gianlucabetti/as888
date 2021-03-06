000100090717      *PARMS dbgview(*source)
000200090603       //--------------------------------------------------------------
000300090603       //?Statistica consumi LdV e Segnacolli       - filtro           ?
000400090603       //--------------------------------------------------------------
000500090603
000600090603     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700090603
000800090603       //--------------------------------------------------------------
000900090603       //?Dichiarazione file.                                          ?
001000090603       //--------------------------------------------------------------
001100090603
001200090603       // - Video
001300090603     fTNSB20D   cf   e             workstn
001400090603     f                                     indds(IndDspF)
001500090603     f                                     infds(InfDspF)
001600090603
001700090603       //--------------------------------------------------------------
001800090603       //?Definizione costanti.                                        ?
001900090603       //--------------------------------------------------------------
002000090603
002100090717       // - Comandi di sistema
002200090717     d c_CmdDspDbf     c                   const('DSPDBF +
002300090717     d                                            file(*libl/WFCLS00F) +
002400090717     d                                            mbr(*first) +
002500090717     d***                                         rcdslt(*yes) +
002600090717     d                                            output(*)')
002700090717
002800090603       // - Tasti funzionali a video
002900090603     d c_F01           c                   const(x'31')
003000090603     d c_F02           c                   const(x'32')
003100090603     d c_F03           c                   const(x'33')
003200090603     d c_F05           c                   const(x'35')
003300090603     d c_F06           c                   const(x'36')
003400090603     d c_F07           c                   const(x'37')
003500090603     d c_F08           c                   const(x'38')
003600090603     d c_F09           c                   const(x'39')
003700090603     d c_F10           c                   const(x'3A')
003800090603     d c_F12           c                   const(x'3C')
003900090603     d c_F13           c                   const(x'B1')
004000090603     d c_F14           c                   const(x'B2')
004100090603     d c_F15           c                   const(x'B3')
004200090603     d c_F16           c                   const(x'B4')
004300090603     d c_F17           c                   const(x'B5')
004400090603     d c_F18           c                   const(x'B6')
004500090603     d c_F19           c                   const(x'B7')
004600090603     d c_F20           c                   const(x'B8')
004700090603     d c_F21           c                   const(x'B9')
004800090603     d c_F22           c                   const(x'BA')
004900090603     d c_F23           c                   const(x'BB')
005000090603     d c_F24           c                   const(x'BC')
005100090603     d c_Enter         c                   const(x'F1')
005200090603     d c_RollDown      c                   const(x'F4')
005300090603     d c_RollUp        c                   const(x'F5')
005400090603
005500090603       //--------------------------------------------------------------
005600090603       //?Definizione schiere.                                         ?
005700090603       //--------------------------------------------------------------
005800090603
005900090603       // - Messaggi di errore
006000090717     d $Msg            s             78    dim( 4)  ctdata  perrcd(01)
006100090603
006200090603       //--------------------------------------------------------------
006300090603       //?Definizione aree dati.                                       ?
006400090603       //--------------------------------------------------------------
006500090603
006600090603       // - Dati utente
006700090603     d �AzUte        e ds                  extname(AZUTE00F)
006800090603     d                                     dtaara
006900090603     d �DatiUte      e ds                  extname(dDatiUte)
007000090603     d                                     dtaara
007100090603
007200090603       //--------------------------------------------------------------
007300090603       //?Definizione strutture dati.                                  ?
007400090603       //--------------------------------------------------------------
007500090603
007600090603       // - Status ds
007700090603     d Status         sds
007800090603     d  SDSpgm           *proc
007900090603
008000090603       // - InfDS
008100090603     d InfDspF         ds
008200090603     d   dsp_aid             369    369a
008300090603
008400090603       // - Indicatori su DspF
008500090603     d IndDspF         ds                  inz
008600090603         // - Emissione messaggio di errore
008700090603     d   ErrMessage                    n   overlay(IndDspF : 28)
008800090603         // - Posizionamento cursore & segnalazione errore
008900090717     d   PosCurDTI                     n   overlay(IndDspF : 51)
009000090717     d   PosCurDTF                     n   overlay(IndDspF : 52)
009100090717     d   PosCurCWF                     n   overlay(IndDspF : 53)
009200090603         // - Riemissione videata
009300090603     d   ErrGenerico                   n   overlay(IndDspF : 99)
009400090603
009500090603       // - Parametri
009600090603     d KPJBA         e ds
009700090603     d TNSB21ds      e ds                  inz
009800090603
009900090603       // - Parametri x controllo profilo utenti
010000090603     d TIBS34ds      e ds
010100090603
010200090603       // - Parametri per controllo data e trasform. in giorni -XSRDA8-
010300090603     d WLBdat          ds                  inz
010400090603     d  G02dat                 1      8  0 inz
010500090603     d  G02inv                 9     16  0 inz
010600090603     d  G02err                17     17    inz
010700090603     d  G02tgi                18     22  0 inz
010800090603
010900090603       //--------------------------------------------------------------
011000090603       //?Definizione variabili globali.                               ?
011100090603       //--------------------------------------------------------------
011200090603
011300090603       // - Flags booleani
011400090603     d $Fine           s               n   inz
011500090603     d $InzD01         s               n   inz(*on)
011600090603
011700090603       // - Variabili per la gestione del video
011800090603     d $Video          s              2    inz('D1')
011900090603
012000090603       // - Campi di comodo
012100090603     d wDate           s              8  0 inz
012200090603     d W1Cdti          s                   like(V1Cdti)  inz
012300090603     d W1Cdtf          s                   like(V1Cdtf)  inz
012400090603
012500090603       //--------------------------------------------------------------
012600090603       //?Definizione prototipi procedure.                             ?
012700090603       //--------------------------------------------------------------
012800090603
012900090603       // - Reperimento dati utente
013000090603      /copy gaitrasrc/srcprotopr,TIBS34R
013100090603
013200090603       // - Controllo ed inversione date
013300090603      /copy gaitrasrc/srcprotopr,XSRDA8
013400090603
013500090603       // - Sottomissione lavoro batch
013600090603     d bch10           pr                  extpgm('BCH10')
013700090603     d   kpjba                             likeds(KPJBA)
013800090715
013900090715     d tnsb21r         pr                  extpgm('TNSB21R')
014000090715     d   kpjba                             likeds(KPJBA)
014100090717
014200090717       // - Execuzione comando di sistema
014300090717      /copy gaitrasrc/srcProtoPR,QCMDEXC
014400090603
014500090603       //--------------------------------------------------------------
014600090603       //?Definizione key-list.                                        ?
014700090603       //--------------------------------------------------------------
014800090603
014900090603
015000090603       //--------------------------------------------------------------
015100090603       //?Riepilogo indicatori utilizzati.                             ?
015200090603       //--------------------------------------------------------------
015300090603       //--------------------------------------------------------------
015400090603
015500090603       //--------------------------------------------------------------
015600090603       //?M A I N - L I N E                                            ?
015700090603       //--------------------------------------------------------------
015800090603
015900090603     c     *Entry        plist
016000090603     c                   parm                    KPJBA
016100090603
016200090603      /free
016300090603
016400090603       //?Operazioni iniziali?
016500090603       exsr  sr_RoutInz;
016600090603
016700090603       //?Gestione video?
016800090603       DOW  $Fine = *off;
016900090603         select;
017000090603           // - Richiesta dei dati in stampa
017100090603           when  $Video = 'D1';
017200090603             exsr  sr_GesD01;
017300090603           other;
017400090603             $Fine = *on;
017500090603         endsl;
017600090603       ENDDO;
017700090603
017800090603       //?Operazioni finali?
017900090603       exsr  sr_RoutEnd;
018000090603
018100090603       //--------------------------------------------------------------
018200090603       //?Operazioni iniziali.                                         ?
018300090603       //--------------------------------------------------------------
018400090603       BEGSR  sr_RoutInz;
018500090603
018600090603         *inLR = *on;
018700090603
018800090603         //?Impostazione campi "fissi"?
018900090603         V1Cpgm = SDSpgm;
019000090603
019100090603         //?Reperimento dati job?
019200090603         exsr  sr_DatiJob;
019300090603
019400090603         //?Reperimento della data corrente (in formato aaaa/mm/gg)?
019500090603         wDate = %int( %subst( %char( %dec( %timestamp() ) ) :1 :8 ) );
019600090603
019700090603       ENDSR;
019800090603
019900090603       //--------------------------------------------------------------
020000090603       //?Reperimento Dati del job (Utente/Operativi).                 ?
020100090603       //--------------------------------------------------------------
020200090603       BEGSR  sr_DatiJob;
020300090603
020400090603         in(E) �AzUte;
020500090603         if NOT %error;
020600090603           in(E) �DatiUte;
020700090603         endif;
020800090603         if %error or RSut = *blanks;
020900090603           clear TIBS34ds;
021000090603           tibs34r ( tibs34ds );
021100090603           in �AzUte;
021200090603           in �DatiUte;
021300090603         endif;
021400090603
021500090603       ENDSR;
021600090603
021700090603       //--------------------------------------------------------------
021800090603       //?Gestione videata D01                                         ?
021900090603       //--------------------------------------------------------------
022000090603       BEGSR  sr_GesD01;
022100090603
022200090603         //?Inizializzazione videata?
022300090603         if  $InzD01 = *on;
022400090603           exsr  sr_InzD01;
022500090603           $InzD01 = *off;
022600090603         endif;
022700090603
022800090603         //?Emissione videata?
022900090603         write SB20T01;
023000090603         exfmt SB20D01;
023100090603         reset ErrMessage;
023200090603         reset ErrGenerico;
023300090603         clear V1Dmsg;
023400090603
023500090603         SELECT;
023600090603
023700090603           //?F3=Fine?
023800090603           WHEN  dsp_aid = c_F03;
023900090603             $Fine = *on;
024000090717
024100090717           //?F9=Visualizzazione file WFCLS00F?
024200090717           WHEN  dsp_aid = c_F09;
024300090717             callp(e) ExecuteCommand ( c_CmdDspDbf : %len(c_CmdDspDbf) );
024400090603
024500090603           //?Invio / F6=Conferma?
024600090603           OTHER;
024700090603             exsr sr_CtrD01;
024800090603             if  ErrGenerico = *off   and   dsp_aid = c_F06;
024900090603               exsr  sr_SbmJob;
025000090603               $Fine = *on;
025100090603             endif;
025200090603
025300090603         ENDSL;
025400090603
025500090603       ENDSR;
025600090603
025700090603       //--------------------------------------------------------------
025800090603       //?Inizializzazione videata D01                                 ?
025900090603       //--------------------------------------------------------------
026000090603       BEGSR  sr_InzD01;
026100090603
026200090603         clear  SB20D01;
026300090717
026400090717         V1Cclrwf = 'S';
026500090603
026600090603       ENDSR;
026700090603
026800090603       //--------------------------------------------------------------
026900090603       //?Controllo dati immessi nella videata D01                     ?
027000090603       //--------------------------------------------------------------
027100090603       BEGSR  sr_CtrD01;
027200090603
027300090603         %subst(IndDspF : 50) = *off;
027400090603
027500090603         clear  W1Cdti;
027600090603         clear  W1Cdtf;
027700090603
027800090717         // -?DATA iniziale?
027900090603         if  V1Cdti =  *zero;
028000090603           ErrMessage  = *on;
028100090603           ErrGenerico = *on;
028200090603           PosCurDTI   = *on;
028300090603           V1Dmsg      = $Msg(01);
028400090603           leavesr;
028500090603         else;
028600090603           clear  WLBdat;
028700090603           G02dat = V1Cdti;
028800090603           xsrda8 ( WLBdat );
028900090603           if  G02err =  *on;
029000090603             ErrMessage  = *on;
029100090603             ErrGenerico = *on;
029200090603             PosCurDTI   = *on;
029300090603             V1Dmsg      = $Msg(02);
029400090603             leavesr;
029500090603           endif;
029600090603           V1Cdti = G02dat;
029700090603           W1Cdti = G02inv;
029800090603           W1Cdtf = G02inv;
029900090603         endif;
030000090603
030100090717         // -?DATA finale?
030200090603         if  V1Cdtf =  *zero;
030300090603           //W2Cdsp = wDate;
030400090603           ErrMessage  = *on;
030500090603           ErrGenerico = *on;
030600090603           PosCurDTF   = *on;
030700090603           V1Dmsg      = $Msg(01);
030800090603           leavesr;
030900090603         else;
031000090603           clear  WLBdat;
031100090603           G02dat = V1Cdtf;
031200090603           xsrda8 ( WLBdat );
031300090603           if  G02err =  *on;
031400090603             ErrMessage  = *on;
031500090603             ErrGenerico = *on;
031600090603             PosCurDTF   = *on;
031700090603             V1Dmsg      = $Msg(02);
031800090603             leavesr;
031900090603           endif;
032000090603           V1Cdtf = G02dat;
032100090603           W1Cdtf = G02inv;
032200090603         endif;
032300090603
032400090717         // -?RANGE di date?
032500090715         if  W1Cdti >  W1Cdtf;
032600090603           ErrMessage  = *on;
032700090603           ErrGenerico = *on;
032800090603           PosCurDTI   = *on;
032900090603           V1Dmsg      = $Msg(03);
033000090603           leavesr;
033100090603         endif;
033200090717
033300090717         // -?Pulizia anticipata del work-file: "S" o "N"?
033400090717         if  V1Cclrwf <> 'S'   and   V1Cclrwf <> 'N';
033500090717           ErrMessage  = *on;
033600090717           ErrGenerico = *on;
033700090717           PosCurCWF   = *on;
033800090717           V1Dmsg      = $Msg(04);
033900090717           leavesr;
034000090717         endif;
034100090603
034200090603       ENDSR;
034300090603
034400090603       //--------------------------------------------------------------
034500090603       //?Sottomissione lavoro batch                                   ?
034600090603       //--------------------------------------------------------------
034700090603       BEGSR  sr_SbmJob;
034800090603
034900090603         clear TNSB21ds;
035000090717         SB21cwf = V1Cclrwf;
035100090603         SB21dti = W1Cdti;
035200090603         SB21dtf = W1Cdtf;
035300090603
035400090603         kpjbu = TNSB21ds;
035500090603         kcoaz = 'SB21';
035600090603
035700090715         if  knmus = *all'1';
035800090715           TNSB21R ( kpjba );
035900090715         else;
036000090715           BCH10 ( kpjba );
036100090715         endif;
036200090603
036300090603       ENDSR;
036400090603
036500090603       //--------------------------------------------------------------
036600090603       //?Operazioni finali.                                           ?
036700090603       //--------------------------------------------------------------
036800090603       BEGSR  sr_RoutEnd;
036900090603
037000090603         //?"Chiusura" pgm?
037100090603         return;
037200090603
037300090603       ENDSR;
037400090603
037500090603      /end-free
037600090603
037700090603       //--------------------------------------------------------------
037800090603       //?Definizione schiere a tempo di compilazione                  ?
037900090603       //--------------------------------------------------------------
038000090603
038100090603** > $Msg -------------------------------------------------------------------*
038200090603Campo obbligatorio                                                             1
038300090603Data formalmente errata                                                        2
038400090603Range di date errato                                                           3
038500090717Immettere "S" (s�) o "N" (no)                                                  4
