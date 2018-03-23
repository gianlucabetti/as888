000100090424       //--------------------------------------------------------------
000200090424       //?Filtro di lancio per "anteprima bolle fatturate al cliente"  ?
000300090424       //--------------------------------------------------------------
000400090424
000500090424     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600090424
000700090424       //--------------------------------------------------------------
000800090424       //?Dichiarazione file.                                          ?
000900090424       //--------------------------------------------------------------
001000090424
001100090424       // - Upload via Internet: regole per cliente
001200090424     fTIVIR02L  if   e           k disk
001300090424
001400090424       // - Video
001500090424     fTIS7VFD1  cf   e             workstn
001600090424     f                                     indds(IndDspF)
001700090424     f                                     infds(InfDspF)
001800090424
001900090424       //--------------------------------------------------------------
002000090424       //?Definizione costanti.                                        ?
002100090424       //--------------------------------------------------------------
002200090424
002300090424       // - Tasti funzionali a video
002400090424     d c_F01           c                   const(x'31')
002500090424     d c_F02           c                   const(x'32')
002600090424     d c_F03           c                   const(x'33')
002700090424     d c_F04           c                   const(x'34')
002800090424     d c_F05           c                   const(x'35')
002900090424     d c_F06           c                   const(x'36')
003000090424     d c_F07           c                   const(x'37')
003100090424     d c_F08           c                   const(x'38')
003200090424     d c_F09           c                   const(x'39')
003300090424     d c_F10           c                   const(x'3A')
003400090424     d c_F11           c                   const(x'3B')
003500090424     d c_F12           c                   const(x'3C')
003600090424     d c_F13           c                   const(x'B1')
003700090424     d c_F14           c                   const(x'B2')
003800090424     d c_F15           c                   const(x'B3')
003900090424     d c_F16           c                   const(x'B4')
004000090424     d c_F17           c                   const(x'B5')
004100090424     d c_F18           c                   const(x'B6')
004200090424     d c_F19           c                   const(x'B7')
004300090424     d c_F20           c                   const(x'B8')
004400090424     d c_F21           c                   const(x'B9')
004500090424     d c_F22           c                   const(x'BA')
004600090424     d c_F23           c                   const(x'BB')
004700090424     d c_F24           c                   const(x'BC')
004800090424     d c_Enter         c                   const(x'F1')
004900090424     d c_RollDown      c                   const(x'F4')
005000090424     d c_RollUp        c                   const(x'F5')
005100090424
005200090424       // - Costante per controllo "caratteri solo numerici"
005300090424     d Digits          c                   const('0123456789')
005400090424
005500090424       //--------------------------------------------------------------
005600090424       //?Definizione schiere.                                         ?
005700090424       //--------------------------------------------------------------
005800090424
005900090424       // - Messaggi di errore
006000090424     d $Msg            s             78    dim(05) ctdata perrcd(1)
006100090424
006200090424     d $Ksc            s              7  0 dim(30) inz
006300090424
006400090424       //--------------------------------------------------------------
006500090424       //?Definizione aree dati.                                       ?
006600090424       //--------------------------------------------------------------
006700090424
006800090424       // - Dati utente
006900090424     d §AzUte        e ds                  extname(AZUTE00F)
007000090424     d                                     dtaara
007100090424     d §DatiUte      e ds                  extname(dDatiUte)
007200090424     d                                     dtaara
007300090424
007400090424       //--------------------------------------------------------------
007500090424       //?Definizione strutture dati.                                  ?
007600090424       //--------------------------------------------------------------
007700090424
007800090424       // - Status ds
007900090424     d Status         sds
008000090424     d  SDSpgm           *proc
008100090424     d  SDSprm           *parms
008200090424     d  SDSusr               254    263
008300090424
008400090424       // - InfDS
008500090424     d InfDspF         ds
008600090424     d  dsp_aid              369    369a
008700090424
008800090424       // - Indicatori su DspF
008900090424     d IndDspF         ds
009000090424         // Visualizzazione messaggio di errore
009100090424     d  ErrMessage                    1n   overlay(IndDspF : 28)
009200090424         // Indicatori di errore
009300090424     d  PosCurDAI                     1n   overlay(IndDspF : 51)
009400090424     d  PosCurDAF                     1n   overlay(IndDspF : 52)
009500090424     d  PosCurCLI                     1n   overlay(IndDspF : 53)
009600090424     d  PosCurCliVAS                  1n   overlay(IndDspF : 54)
009700090424     d  PosCurTPF                     1n   overlay(IndDspF : 55)
009800090424     d  ErrGenerico                   1n   overlay(IndDspF : 99)
009900090424
010000090424       // - Struttura per ridefinizione sottoconto cliente
010100090424       //   (7/0 packed in alfanum.)
010200090424     d dsKSA           ds             4    inz
010300090424     d   dsKSN                 1      4p 0 inz
010400090424
010500090424       // - Parametri per batch
010600090424     d Param           ds                  inz
010700090424     d  p_DataDa                      8s 0 inz
010800090424     d  p_DataA                       8s 0 inz
010900090424     d  p_Cli                         7s 0 inz
011000090424     d  p_TipCli                      1a   inz
011100090424     d  p_CliVGD                      8a   inz
011200090424     d  p_TipFil                      2a   inz
011300090424     d  p_GgBack                      3s 0 inz
011400090424     d  p_GgDaElab                    3s 0 inz
011500090424
011600090424       // - Parametri ricevuti
011700090424     d KPJBA         e ds
011800090424
011900090424       // - Parametri x Controllo profilo utenti
012000090424     d TIBS34ds      e ds
012100090424
012200090424       // - Parametri per Interrogazione clenti
012300090424     d XCLIRds       e ds                  inz
012400090424
012500090424       // - Parametri per pgm. TIBS69R (decodifica cliente)
012600090424     d TIBS69ds      e ds                  inz
012700090424     d ds_CNACO      e ds                  extname(CNACO00F) inz
012800090424     d ds_CNIND      e ds                  extname(CNIND00F) inz
012900090424     d ds_CNCLP      e ds                  extname(CNCLP00F) inz
013000090424     d ds_FNCLS      e ds                  extname(FNCLS00F) inz
013100090424
013200090424       //--------------------------------------------------------------
013300090424       //?Definizione variabili globali.                               ?
013400090424       //--------------------------------------------------------------
013500090424
013600090424       // - Flags booleani
013700090424     d $Fine           s               n   inz(*off)
013800090424     d $InzD01         s               n   inz(*on)
013900090424
014000090424       // - Campi per la gestione del video
014100090424     d $Video          s              2    inz('D1')
014200090424
014300090424       //--------------------------------------------------------------
014400090424       //?Definizione procedure usate.                                 ?
014500090424       //--------------------------------------------------------------
014600090424
014700090424       // - Reperimento dati utente
014800090424      /copy gaitrasrc/srcProtoPr,TIBS34R
014900090424
015000090424       // - Controllo/Decodifica cliente
015100090424      /copy gaitrasrc/srcprotopr,TIBS69R
015200090424
015300090424       // - Ricerca cliente
015400090424     d xclir           pr                  extpgm('XCLIR')
015500090424     d   xclirds                           likeds(XCLIRds)
015600090424
015700090424       // - Anteprima bolle fatturate al cliente
015800090424     d trtc28r2        pr                  extpgm('TRTC28R2')
015900090424     d   Param                             likeds(Param)
016000090424
016100090424       // - Lancio batch
016200090424     d bch10           pr                  extpgm('BCH10')
016300090424     d   kpjba                             likeds(KPJBA)
016400090424
016500090424       //--------------------------------------------------------------
016600090424       //?Definizione key-list.                                        ?
016700090424       //--------------------------------------------------------------
016800090424
016900090424       // - File TNTBE01L
017000090424     d k03tivir02    e ds                  extname(TIVIR02L : *key)
017100090424     d                                     inz
017200090424
017300090424       //--------------------------------------------------------------
017400090424       //?Riepilogo indicatori.                                        ?
017500090424       //--------------------------------------------------------------
017600090424
017700090424     c     *Entry        plist
017800090424     c                   parm                    KPJBA
017900090424
018000090424      /free
018100090424
018200090424       // - Start
018300090424       *inLR = *on;
018400090424
018500090424       // - Elab
018600090424       dow  $Fine = *off;
018700090424         select;
018800090424           when  $Video = 'D1';
018900090424             exsr  sr_GesD01;
019000090424           other;
019100090424             $Fine = *on;
019200090424         endsl;
019300090424       enddo;
019400090424
019500090424       // - End
019600090424       exsr sr_EndPgm;
019700090424
019800090424       //--------------------------------------------------------------
019900090424       //?Operazioni iniziali                                          ?
020000090424       //--------------------------------------------------------------
020100090424       BEGSR  *InzSR;
020200090424
020300090424         // Reperimento dati utente
020400090424         exsr sr_DatiJob;
020500090424
020600090424         // Impostazione nome programma in testata a video
020700090424         V1Tpgm = SDSpgm;
020800090424
020900090424       ENDSR;
021000090424
021100090424       //--------------------------------------------------------------
021200090424       //?Reperimento Dati del job (Utente/Operativi).                 ?
021300090424       //--------------------------------------------------------------
021400090424       BEGSR  sr_DatiJob;
021500090424
021600090424         in(E) §AzUte;
021700090424         if NOT %error;
021800090424           in(E) §DatiUte;
021900090424         endif;
022000090424         if %error or RSut = *blanks;
022100090424           clear TIBS34ds;
022200090424           tibs34r(tibs34ds);
022300090424           in §AzUte;
022400090424           in §DatiUte;
022500090424         endif;
022600090424
022700090424       ENDSR;
022800090424
022900090424       //--------------------------------------------------------------
023000090424       //?Gestione videata D01                                         ?
023100090424       //--------------------------------------------------------------
023200090424       BEGSR  sr_GesD01;
023300090424
023400090424         // Inizializzazione videata
023500090424         if  $InzD01 = *on;
023600090424           exsr  sr_InzD01;
023700090424           $InzD01  = *off;
023800090424         endif;
023900090424
024000090424         // Emissione videata
024100090424         if  ErrMessage = *off;
024200090424           write S7VFT01;
024300090424         endif;
024400090424
024500090424         exfmt S7VFD01;
024600090424
024700090424         reset ErrMessage;
024800090424         reset ErrGenerico;
024900090424         clear V1Dmsg;
025000090424
025100090424         SELECT;
025200090424
025300090424           // - F3=Fine
025400090424           WHEN  dsp_aid = c_F03;
025500090424             $Fine = *on;
025600090424
025700090424           // - F12=Ritorno
025800090424           WHEN  dsp_aid = c_F12;
025900090424             $Fine = *on;
026000090424
026100090424           // - Invio o F6=Conferma
026200090424           OTHER;
026300090424             exsr sr_CtrD01;
026400090424             select;
026500090424               // - rilevato errore
026600090424               when  ErrGenerico = *on;
026700090424               // - non premuto F6
026800090424               when  dsp_aid <> c_F06;
026900090424               // - Lancio
027000090424               other;
027100090424                 exsr sr_CallPgm;
027200090424             endsl;
027300090424
027400090424         ENDSL;
027500090424
027600090424       ENDSR;
027700090424
027800090424       //--------------------------------------------------------------
027900090424       //?Inzializzazione videata D01                                  ?
028000090424       //--------------------------------------------------------------
028100090424       BEGSR  sr_InzD01;
028200090424
028300090424         clear  S7VFD01;
028400090424
028500090424         VIDleg    = 'U';
028600090424         VIDCliVAS = *zero;
028700090701         VIDtpf    = 'AF';
028800090424
028900090424       ENDSR;
029000090424
029100090424       //--------------------------------------------------------------
029200090424       //?Controllo dati in videata D01                                ?
029300090424       //--------------------------------------------------------------
029400090424       BEGSR sr_CtrD01;
029500090424
029600090424         IndDspF = *off;
029700090424
029800090424         select;
029900090424           // - Data DA obbligatoria
030000090424           when  VIDdai <= *zero;
030100090424             ErrMessage  = *on;
030200090424             ErrGenerico = *on;
030300090424             PosCurDAI   = *on;
030400090424             V1Dmsg = $Msg(01);
030500090424             leavesr;
030600090424           // - Data A obbligatoria
030700090424           when  VIDdaf <= *zero;
030800090424             ErrMessage  = *on;
030900090424             ErrGenerico = *on;
031000090424             PosCurDAF   = *on;
031100090424             V1Dmsg = $Msg(01);
031200090424             leavesr;
031300090424
031400090424         endsl;
031500090424
031600090424         // - Controllo codice cliente
031700090424         select;
031800090424           // - Ricerca
031900090424           when  %scan('?' : VIDcli) > *zero;
032000090424             clear  XCLIRds;
032100090424             dXCcap = DUTkci;
032200090424             XCLIR ( XCLIRds );
032300090424             // - scompattamento del 1° codice ricevuto
032400090424             dsKSA = %subst( dXCksc : 1 : %len(dsKSA) );
032500090424             VIDcli = %editc( dsKSN : 'X' );
032600090424             ErrGenerico = *on;
032700090424             PosCurCLI   = *on;
032800090424             leavesr;
032900090424           // - Obbligatorietà
033000090424           when  VIDcli = *zero   or   VIDcli = *blank;
033100090424             ErrMessage  = *on;
033200090424             ErrGenerico = *on;
033300090424             PosCurCLI   = *on;
033400090424             V1Dmsg = $Msg(02);
033500090424             leavesr;
033600090424           // - Effettiva numericità
033700090424           when  %check(Digits : VIDcli) > *zeros;
033800090424             ErrMessage  = *on;
033900090424             ErrGenerico = *on;
034000090424             PosCurCLI   = *on;
034100090424             V1Dmsg = $Msg(03);
034200090424             leavesr;
034300090424           // - Controllo
034400090424           other;
034500090424             clear  TIBS69ds;
034600090424             I69kcc = DUTkci;
034700090424             I69kac = %int(VIDcli);
034800090424             I69sif = knsif;
034900090424             tibs69r(tibs69ds : ds_cnaco : ds_cnind : ds_cnclp : ds_fncls);
035000090424             if  O69err = *on;
035100090424               VIDclid = *all'*';
035200090424             else;
035300090424               VIDclid = ACOrag;
035400090424             endif;
035500090424
035600090424         endsl;
035700090424
035800090424         // - Controllo se cliente abilitato allo scambio dati
035900090424         //   (tipo file inserito a video)
036000090424         if  VIDcliVAS > *zero;
036100090424           VIRksc = VIDcliVAS;
036200090424         else;
036300090424           VIRksc = '0' + VIDcli;
036400090424         endif;
036500090424         VIRtip = VIDtpf;
036600090424         chain  %kds(K03tivir02 : 2)  TIVIR000;
036700090424         if  Not %found(TIVIR02L);
036800090424           ErrMessage  = *on;
036900090424           ErrGenerico = *on;
037000090424           if  VIDcliVAS > *zero;
037100090424             PosCurCliVAS = *on;
037200090424           else;
037300090424             PosCurCLI    = *on;
037400090424           endif;
037500090424           V1Dmsg = %trimr($Msg(04)) + ' ' + VIDtpf;
037600090424           leavesr;
037700090424         endif;
037800090424
037900090424         // - Tipo file
038000090424         if  VIDtpf = *blank;
038100090424           ErrMessage  = *on;
038200090424           ErrGenerico = *on;
038300090424           PosCurTPF   = *on;
038400090424           V1Dmsg = $Msg(05);
038500090424           leavesr;
038600090424         endif;
038700090424
038800090424       ENDSR;
038900090424
039000090424       //--------------------------------------------------------------
039100090424       //?Richiamo *pgm TRTC28R2                                       ?
039200090424       //--------------------------------------------------------------
039300090424       BEGSR  sr_CallPgm;
039400090424
039500090424         clear  Param;
039600090424         p_DataDa = VIDdai;
039700090424         p_DataA  = VIDdaf;
039800090424         p_Cli    = %int(VIDcli);
039900090424         p_TipCli = VIDleg;
040000090424         if  VIDcliVAS > *zero;
040100090424           p_CliVGD = VIDcliVAS;
040200090424         else;
040300090424           p_CliVGD = '0' + VIDcli;
040400090424         endif;
040500090424         p_TipFil = VIDtpf;
040600090424         kpjbu    = Param;
040700090424
040800090424         kcoaz    = 'S7V6';
040900090424
041000090424         if  knmus = *all'1' or videxe = 'I';
041100090424           trtc28r2 (kpjba);
041200090424         else;
041300090424           bch10 (kpjba);
041400090424         endif;
041500090424
041600090424         $InzD01 = *on;
041700090424
041800090424       ENDSR;
041900090424
042000090424       //--------------------------------------------------------------
042100090424       //?Fine: chiusura dei files di stampa                           ?
042200090424       //--------------------------------------------------------------
042300090424       BEGSR sr_EndPgm;
042400090424
042500090424         return;
042600090424
042700090424       ENDSR;
042800090424
042900090424      /end-free
043000090424
043100090424** - $Msg -------------------------------------------------------------------*
043200090424Data obbligatoria                                                              1
043300090424Codice cliente obbligatorio.                                                   2
043400090424Codice cliente alfanumerico NON ammesso                                        3
043500090424Il cliente indicato NON è abilitato allo scambio dati VAS del tipo file        4
043600090424Tipo File obbligatorio                                                         5
