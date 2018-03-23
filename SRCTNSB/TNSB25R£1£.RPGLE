000100120619       //==============================================================
000200120619       //?TNSB25R - Stampa copia LdV per Scannerizzazione Doc. Clienti ?
000300120619       //==============================================================
000400120619
000500120619       //--------------------------------------------------------------
000600120619       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700120619       //--------------------------------------------------------------
000800120619
000900120622     /*PRM  dbgview(*source)
001000120622     /*CMD  ovrdbf file(TITAS30C) tofile(GAITRAGRPS/TITAS30C) +
001100120622     /*CMD         ovrscope(*calllvl)
001200120622     /*CMD  ovrdbf file(TITAA30C) tofile(GAITRAGRPS/TITAA30C) +
001300120622     /*CMD         ovrscope(*calllvl)
001400120622     /*CMD  ovrdbf file(TITA430C) tofile(GAITRAGRPS/TITA430C) +
001500120622     /*CMD         ovrscope(*calllvl)
001600120622     /*CMD  ovrdbf file(FIAR531C) tofile(GAITRAGRPS/FIAR531C) +
001700120622     /*CMD         ovrscope(*calllvl)
001800120622     /*CMD  ovrdbf file(TITAA00F) tofile(GAITRAGRPS/TITAA00F) +
001900120622     /*CMD         ovrscope(*calllvl)
002000120622     /*END  dltovr file(TITAS30C TITAA30C TITA430C FIAR531C +
002100120622     /*END              TITAA00F) lvl(*)
002200120622     /*END
002300120619
002400120619       //--------------------------------------------------------------
002500120619       //?Specifiche di controllo.                                     ?
002600120619       //--------------------------------------------------------------
002700120619
002800120619     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002900120619     h dftactgrp(*no)
003000120621     h bnddir('TRUL')
003100120619
003200120619       //--------------------------------------------------------------
003300120619       //?Dichiarazione file.                                          ?
003400120619       //--------------------------------------------------------------
003500120619
003600120619       // -?Organigramma filiale/aziendale?
003700120619     fAZORG01L  if   e           k disk
003800120620
003900120620       // -?Tabelle?
004000120620     fTABEL00F  if   e           k disk
004100120620     fTNTBE01L  if   e           k disk
004200120620
004300120621       // -?Dati di Bolle SEDE?
004400120620     fTITAS30C  if   e           k disk    extfile(wLibTITAS)
004500120620     f                                     usropn
004600120621     fTITAA30C  if   e           k disk    extfile(wLibTITAA)
004700120621     f                                     usropn
004800120621     fTITA430C  if   e           k disk    extfile(wLibTITA4)
004900120621     f                                     usropn
005000120621     fFIAR531C  if   e           k disk    extfile(wLibFIAR5)
005100120621     f                                     usropn
005200120619
005300120619       // -?File di stampa?
005400120619     fTNSB25P   o    e             printer usropn
005500120619     f                                     rename(SB25POS   :A4prtPOS  )
005600120619     f                                     rename(SB25BOX   :A4prtBOX  )
005700120619     f                                     rename(SB25LOGO  :A4prtLogo )
005800120619     f                                     rename(SB25LOGODA:A4prtLogoD)
005900120619     f                                     rename(SB25LDV   :A4prtLDV  )
006000120621     f                                     rename(SB25MSG   :A4prtMSG  )
006100120619
006200120619       //--------------------------------------------------------------
006300120619       //?Definizione costanti.                                        ?
006400120619       //--------------------------------------------------------------
006500120619
006600120620       // -?Intestazioni in stampa?
006700120620     d $BRT            c                   const('BRT S.p.A.')
006800120620     d $Intest1        c                   const('Sede Operativa ed Am-
006900120620     d                                     ministrativa:')
007000120620     d $Intest2        c                   const('via E. Mattei, 42 -
007100120620     d                                     - 40138 Bologna')
007200120620     d $Intest3        c                   const('Sede Legale: Piazza -
007300120620     d                                     Diaz, 7 - 20123 Milano')
007400120620     d $Intest4        c                   const('C.F. e P.IVA 04507990150')
007500120620     d $Intest5        c                   const(' Capitale Sociale Eu-
007600120620     d                                     ro 24.000.000 int. vers.')
007700120620     d $Intest6        c                   const('info on-line')
007800120620     d $Intest7        c                   const('www.brt.it')
007900120620
008000120620       // -?Lunghezza (in inches) di una singola LdV?
008100120620     d c_Len1LdV       c                   const(5.775)
008200120620
008300120620       // -?Spessore delle linee in inches/pollici (*memo)?
008400120620     d c_LineNarrow    c                   const(0.008)
008500120620     d c_LineMedium    c                   const(0.017)
008600120620     d c_LineWide      c                   const(0.025)
008700120620
008800120620       // -?Posizioni dell'intestazione "BRT S.p.A."?
008900120620     d c_PosTBar       c                   const(0.270)
009000120620     d c_PosTBa1       c                   const(0.350)
009100120620     d c_PosTBa2x      c                   const(0.430)
009200120620     d c_PosTBa3x      c                   const(0.500)
009300120620     d c_PosTBa4x      c                   const(0.590)
009400120620     d c_PosTBa5x      c                   const(0.660)
009500120620     d c_PosTBa6x      c                   const(0.750)
009600120620
009700120620       // -?Posizioni dei bordi orizzontali superiori box?
009800120620       //  ?(distanza dal lato superiore dell'A4)?
009900120620       //  ?N.B.?- c_PosBoxNXs => posizione iniziale della linea?
010000120620       //                        ?orizzontale Superiore?
010100120620       //       ?- c_PosBoxNXi => posizione iniziale della linea?
010200120620       //                        ?orizzontale Inferiore?
010300120620     d c_PosBox1As     c                   const(0.200)
010400120620     d c_PosBox1Bs     c                   const(0.790)
010500120620     d c_PosBox1Bi     c                   const(2.850)
010600120620     d c_PosBox1Cs     c                   const(1.950)
010700120620     d c_PosBox1Ds     c                   const(2.453)
010800120620       // -?Posizioni di altre linee orizzontali?
010900120620     d c_PosLin1OS1    c                   const(1.350)
011000120620     d c_PosLin1OS2    c                   const(1.790)
011100120620     d c_PosLin1OS3    c                   const(2.010)
011200120620     d c_PosLin1OS4    c                   const(2.230)
011300120620     d c_PosLin1OS5    c                   const(2.470)
011400120620     d c_PosLin1OC1    c                   const(1.680)
011500120620     d c_PosLin1OC2    c                   const(1.240)
011600120620     d c_PosLin1OD1    c                   const(1.010)
011700120620
011800120620       // -?Dimensioni base per i simil-loghi Bartolini e BRT?
011900120620      /copy gaitrasrc/srcTNSY,LogoBRT_R1
012000120620
012100120620       // -?Limite righe stampabili su 1/4 di A4 in 1/9"?
012200120620     d c_MaxLine1      c                   const(025)
012300120621
012400120621       // -?COSTANTI IN STAMPA?
012500120621     d c_CoOr          c                   const('COLLI ORIGINALI     -
012600120621     d                                     N. ')
012700120622
012800120622       // -?Codice utente x TABEL00F?
012900120622     d c_Kut           c                   const(1)
013000120620
013100120620       //--------------------------------------------------------------
013200120620       //?Definizione schiere.                                         ?
013300120620       //--------------------------------------------------------------
013400120620
013500120620       // -?Messaggi di errore?
013600120621     d $Msg            s             78    dim( 2)  ctdata  perrcd( 1)
013700120620
013800120620       // -?Note e Particolarità?
013900120620     d $NP             s                   dim(6)  inz  like(Pnp1)
014000120620
014100120620       //--------------------------------------------------------------
014200120620       //?Definizione aree dati.                                       ?
014300120620       //--------------------------------------------------------------
014400120620
014500120620       // -?Dati utente?
014600120620     d §AzUte        e ds                  extname(AZUTE00F)
014700120620     d                                     dtaara
014800120620     d §DatiUte      e ds                  extname(dDatiUte)
014900120620     d                                     dtaara
015000120620
015100120620       //--------------------------------------------------------------
015200120620       //?Definizione strutture dati.                                  ?
015300120620       //--------------------------------------------------------------
015400120621
015500120621       // -?Parametri ricevuti?
015600120621     d TNSB25ds      e ds
015700120621
015800120621       // -?Tabella "3A" = Codici Bolla?
015900120621     d ds3A          e ds                  inz
016000120621       // -?Tabella "VPO" = Variabili Programmi Operativi?
016100120621     d dVPO          e ds                  inz
016200120621
016300120621       // -?Tipo rec. "A" del file TITA430C (Riferimento Alfanumerico)?
016400120621     d dTA4a         e ds                  inz
016500120621
016600130315       // -?Tipo rec. "BNB" del file FIAR531C (Bancali)?
016700120621     d dAR5bnb       e ds                  inz
016800130315
016900130315       // -?Tipo rec. "GEN" del file FIAR531C (Consegna/Allegati/...)?
017000130315     d dAR5gen       e ds                  inz
017100120622
017200120622       // -?Strutture dati di "comodo" per salvare i dati dei rec.?
017300120622       //  ?diversi del file TITAA (trk "M" e "O")?
017400120622     d TITAAds       e ds                  extname(TITAA00F)
017500120622     d TITAA_M_ds    e ds                  extname(TITAA00F) prefix(M)
017600120622     d TITAA_O_ds    e ds                  extname(TITAA00F) prefix(O)
017700120620
017800120620       // -?Codice a Barre?
017900120620     d BARcodeDS       ds            17    inz
018000120621     d   BARcdeAAS                    2s 0 inz
018100120621     d   BARcdeLNP                    3s 0 inz
018200120621     d   BARcdeNRS                    2s 0 inz
018300120621     d   BARcdeNSP                    7s 0 inz
018400120621     d*//BARcdeXXX                    3    inz
018500120621
018600120621       // -?Chiave di accesso ad eventuali Note?
018700120621     d KEYpar          ds             8    inz
018800120621     d   wTPpar                       1    inz
018900120621     d   wCDpar                       2    inz
019000120621     d   wProgr                       5    inz('S0001')
019100120620
019200120620       //--------------------------------------------------------------
019300120620       //?Definizione variabili globali.                               ?
019400120620       //--------------------------------------------------------------
019500120621
019600120621       // -?Flags booleani?
019700120621     d*// $Mamma          s               n   inz
019800120621     d $NoDesPartic    s               n   inz
019900120621
020000120621       // -?Indici di schiera?
020100120621     d xx              s              3  0 inz
020200120621
020300120621       // -?Nome del sistema?
020400120621     d currSysNeta     s              8a   inz
020500120620
020600120621       // -?Nomi estesi Libreria/File dei files?
020700120620     d wLibTITAS       s             21a   inz
020800120621     d wLibTITAA       s             21a   inz
020900120621     d wLibTITA4       s             21a   inz
021000120621     d wLibFIAR5       s             21a   inz
021100120620
021200120620       // -?Variabili per il calcolo delle posizioni per box in A4?
021300120620     d wSeqLdV         s              1  0 inz
021400120620     d wPosIni         s              5  3 inz
021500120621
021600120621       // -?Campi di comodo?
021700120621     d wData_EUR       s               d   datfmt(*eur)   inz(*loval)
021800120621     d w053a           s             53    inz
021900120620
022000120620       //--------------------------------------------------------------
022100120620       //?Definizione prototipi procedure.                             ?
022200120620       //--------------------------------------------------------------
022300120621
022400120621       // -?Reperimento NETA sistema AS/400 corrente?
022500120621      /copy gaitrasrc/srcProtoPR,UBRTVNETA
022600120620
022700120620       // -?Reperimento dati utente?
022800120620     d TIBS34ds      e ds
022900120620      /copy gaitrasrc/srcProtoPR,TIBS34R
023000120620
023100120620       // -?Parametri per TRUL28R (calcolo check digit del barcode)?
023200120620     d TRUL28ds      e ds                  inz
023300120702     d   I28tla      e                     inz('L')
023400120702     d   I28mod      e                     inz('BAR')
023500120620       // -?Calcolo check-digit per bar-code?
023600120620     d trul28r1        pr                  extpgm('TRUL28R1')
023700120620     d   trul28ds...
023800120620     d                                     likeds(TRUL28ds)
023900120622
024000120622       // -?Controllo/Decodifica cliente?
024100120622      /copy gaitrasrc/srcProtoPI,TIBS69R
024200120622      /copy gaitrasrc/srcProtoPR,TIBS69R
024300120620
024400120620       //--------------------------------------------------------------
024500120620       //?Definizione key-list.                                        ?
024600120620       //--------------------------------------------------------------
024700120620
024800120620       // -?File TABEL00F?
024900120620     d k03tabel00    e ds                  extname(TABEL00F : *key)
025000120620     d                                     prefix(k_)   inz
025100120620
025200120620       // -?File TNTBE01L?
025300120620     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
025400120620     d                                     prefix(k_)   inz
025500120620
025600120620       // -?File TITAS30C?
025700120620     d k05titas30    e ds                  extname(TITAS30C : *key)
025800120620     d                                     prefix(k_)   inz
025900120621
026000120621       // -?File TITAA30C?
026100120621     d k05titaa30    e ds                  extname(TITAA30C : *key)
026200120621     d                                     prefix(k_)   inz
026300120621
026400120621       // -?File TITA430C?
026500120621     d k05tita430    e ds                  extname(TITA430C : *key)
026600120621     d                                     prefix(k_)   inz
026700120621
026800120621       // -?File FIAR531C?
026900120621     d k08fiar531    e ds                  extname(FIAR531C : *key)
027000120621     d                                     prefix(k_)   inz
027100120620
027200120620       //--------------------------------------------------------------
027300120620       //?M A I N - L I N E                                            ?
027400120620       //--------------------------------------------------------------
027500120620
027600120620     c     *Entry        plist
027700120620     c                   parm                    TNSB25ds
027800120620
027900120620      /free
028000120620
028100120620       // -?Operazioni iniziali?
028200120620       exsr  sr_RoutInz;
028300120620
028400120620       // -?Stampa?
028500120620       exsr  sr_Stampa;
028600120620
028700120620       // -?Operazioni finali?
028800120620       exsr  sr_RoutEnd;
028900120620
029000120620
029100120620       //--------------------------------------------------------------
029200120620       //?Operazioni iniziali.                                         ?
029300120620       //--------------------------------------------------------------
029400120620       BEGSR  *InzSR;
029500120620
029600120620         // -?Verifica del sistema AS/400 corrente?
029700120620         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
029800120620           exsr  sr_RoutEnd;
029900120620         endif;
030000120620
030100120620         // -?Impostazione libreria per file TITAS30C?
030200120620         //  ?(che verrà aperto SOLO se richiesto dal chiamante)?
030300120620         if  %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
030400120620           wLibTITAS = 'GAITRAGRU/TITAS30C';
030500120621           wLibTITAA = 'GAITRAGRU/TITAA30C';
030600120621           wLibTITA4 = 'GAITRAGRU/TITA430C';
030700120621           wLibFIAR5 = 'GAITRAGRU/FIAR531C';
030800120620         else;
030900120620           wLibTITAS = 'GAITRAGRPS/TITAS30C';
031000120621           wLibTITAA = 'GAITRAGRPS/TITAA30C';
031100120621           wLibTITA4 = 'GAITRAGRPS/TITA430C';
031200120621           wLibFIAR5 = 'GAITRAGRPS/FIAR531C';
031300120620         endif;
031400120620
031500120620         // -?Reperimento dati job?
031600120620         exsr  sr_DatiJob;
031700120622
031800120622         // -?Impostazione campi fissi?
031900120622         k_TBLkut = c_Kut;
032000120620
032100120620         // -?Reperimento n° max di LdV stampabili da tab. "VPO"?
032200120620         exsr  sr_RepTabVPO;
032300120620
032400120620         // -?Apertura del PRTF?
032500120620         open  TNSB25P;
032600120620
032700120620       ENDSR;
032800120620
032900120620       //--------------------------------------------------------------
033000120620
033100120620       BEGSR  sr_RoutInz;
033200120620
033300120620         // -?SE richiesta la sola chiusura del *pgm => uscita?
033400120620         if  D25tla = 'C';
033500120620           exsr  sr_RoutEnd;
033600120620         endif;
033700120620
033800120620         // -?Richiesta la RI-apertura del file di stampa?
033900120620         if  D25fl1 = *on;
034000120621           close  TNSB25P;
034100120621           open   TNSB25P;
034200120620         endif;
034300120620
034400120620       ENDSR;
034500120620
034600120620       //--------------------------------------------------------------
034700120620       //?Reperimento Dati del job (Utente/Operativi).                 ?
034800120620       //--------------------------------------------------------------
034900120620       BEGSR  sr_DatiJob;
035000120620
035100120620         in(E) §AzUte;
035200120620         if NOT %error;
035300120620           in(E) §DatiUte;
035400120620         endif;
035500120620         if %error or RSut = *blanks;
035600120620           clear TIBS34ds;
035700120620           tibs34r ( tibs34ds );
035800120620           in §AzUte;
035900120620           in §DatiUte;
036000120620         endif;
036100120620
036200120620       ENDSR;
036300120620
036400120620       //--------------------------------------------------------------
036500120620       //?Reperimento n° max di LsV stampabili da tab. "VPO"           ?
036600120620       //--------------------------------------------------------------
036700120620       BEGSR  sr_RepTabVPO;
036800120620
036900120620         // -?Aggancio tabella "VPO" per reperimento del n° LdV stampabili?
037000120620         //  ?limite (per congelamento e blocco stampa)?
037100120620         clear  dVPO;
037200120620         k_TBEcod = 'VPO';
037300120620         k_TBEke1 = 'VPO';
037400120620         k_TBEke2 = *blank;
037500120620         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
037600120620         if  %found(TNTBE01L)  and  TBEatb = *blank;
037700120621           dVPO = TBEuni;
037800120620         endif;
037900120620         if  §VPOMaxLdv = *zero;
038000120620           §VPOMaxLdv = 10000;
038100120620         endif;
038200120620
038300120620       ENDSR;
038400120620
038500120620       //--------------------------------------------------------------
038600120620       //?Stampa copia LdV x scannerizzazione documenti clienti.       ?
038700120620       //--------------------------------------------------------------
038800120620       BEGSR  sr_Stampa;
038900120620
039000120620         // -?Pulizia del PRTF?
039100120620         exsr  sr_ClrPRTF;
039200120620
039300120620         Select;
039400120620
039500120620           // -?Passati i dati => è già (quasi) tutto pronto?
039600120620           When  D25ges = *blank;
039700120620             exsr  sr_CarTAB;
039800120621             if  D25dla = *blank;
039900120621               exsr  sr_DecodLNA;
040000120621             endif;
040100120620
040200120621           // -?Richiesto il reperimento dei dati da TITAS/TITAA/TITA4?
040300120621           //                                  ?e da FIAR5?
040400120620           When  D25ges = 'T';
040500120621             exsr  sr_CarTITAx;
040600120620
040700120621           // -?D25GES = "R" => Stampa una LDV vuota con un messaggio?
040800120620           //  ?per avvisare che è impossibile continuare la ristampa?
040900120620           //  ?perchè stampate più di 10.000 LdV !?
041000120620           When  D25ges = 'R';
041100120620             Pmsg1  = $Msg(1);
041200120620             Pmsg2  = %replace( %editc( §VPOmaxldv : 'J' ) :
041300120620                                        $Msg(2) : %scan(
041400120620                                        '&NrLdV' : $Msg(2) ) );
041500120620             exsr  sr_Posiz_Box1;
041600120620             write  A4prtMSG;
041700120620             D25tla = 'L';
041800120620             exsr  sr_RoutEnd;
041900120620
042000120620         EndSl;
042100120620
042200120620         // -?Preparazione dati per stampa bolla?
042300120620         exsr  sr_RiempiLDV;
042400120620
042500120620         // -?Stampa copia LdV x scannerizzazione documenti clienti?
042600120620         exsr  sr_StampaLDV;
042700120620
042800120620       ENDSR;
042900120621
043000120621       //--------------------------------------------------------------
043100120621       //?Pulizia del PrtF.                                            ?
043200120621       //--------------------------------------------------------------
043300120621       BEGSR  sr_ClrPRTF;
043400120621
043500120621         //clear  A4prtPos;        ?(NON ha campi)?
043600120621         clear  A4prtBox;
043700120621         clear  A4prtLogo;
043800120621         clear  A4prtLogoD;
043900120621         clear  A4prtLdv;
044000120621         clear  A4prtMsg;
044100120621
044200120621         clear  $NP;
044300120621         clear  xx;
044400120621
044500120621         clear  *in;
044600120621
044700120621       ENDSR;
044800120620
044900120620       //--------------------------------------------------------------
045000120620       //?Caricamento Tabelle.                                         ?
045100120620       //--------------------------------------------------------------
045200120620       BEGSR  sr_CarTAB;
045300120620
045400120620         // -?Tab. "3A" = Codici Bolla?
045500120620         clear  ds3A;
045600120620         k_TBLcod = '3A';
045700120620         k_TBLkey = D25cbo;
045800120620         chain  %kds( k03tabel00 )  TABEL;
045900120620         if  %found(TABEL00F);
046000120620           ds3A = TBLuni;
046100120620         endif;
046200120621         //$Mamma = (§3Abcm = 'S');
046300120620
046400120620       ENDSR;
046500120620
046600120620       //--------------------------------------------------------------
046700120621       //?Caricamento dati da TITAS30C/TITAA30C/TITA430C               ?
046800120620       //--------------------------------------------------------------
046900120621       BEGSR  sr_CarTITAx;
047000120620
047100120621         // -?Apertura files?
047200120620         if  Not  %open(TITAS30C);
047300120620           open  TITAS30C;
047400120620         endif;
047500120621         if  Not  %open(TITAA30C);
047600120621           open  TITAA30C;
047700120621         endif;
047800120621         if  Not  %open(TITA430C);
047900120621           open  TITA430C;
048000120621         endif;
048100120621         if  Not  %open(FIAR531C);
048200120621           open  FIAR531C;
048300120621         endif;
048400120620
048500120621         // -?Aggancio record TITAS30C?
048600120621         clear  k05titas30;
048700120621         k_TASaas = D25aas;
048800120621         k_TASlnp = D25lnp;
048900120621         k_TASnrs = D25nrs;
049000120621         k_TASnsp = D25nsp;
049100120621         if  D25tbl <> *blank;
049200120621           k_TAStbl = D25tbl;
049300120621           chain  %kds( k05titas30 )  TITAS30C;
049400120621         else;
049500120621           chain  %kds( k05titas30 : 4 )  TITAS30C;
049600120621         endif;
049700120621
049800120620         if  NOT  %found(TITAS30C);
049900120620           leavesr;
050000120620         endif;
050100120621
050200120621         // -?Aggancio record TITAA30C?
050300120622         clear  TITAA_M_ds;
050400120622         clear  TITAA_O_ds;
050500120621         clear  k05titaa30;
050600120621         k_TAAaas = D25aas;
050700120621         k_TAAlnp = D25lnp;
050800120621         k_TAAnrs = D25nrs;
050900120621         k_TAAnsp = D25nsp;
051000120622
051100120622         k_TAAtrc = 'O';
051200120622         chain  %kds( k05titaa30 )  TITAA30C;
051300120622         if  %found(TITAA30C);
051400120622           TITAA_O_ds = TITAAds;
051500120622         endif;
051600120621
051700120621         // -?Aggancio records TITA430C (non qui!)?
051800120625         clear  dTA4A;
051900120621         clear  k05tita430;
052000120621         k_TA4aas = D25aas;
052100120621         k_TA4lnp = D25lnp;
052200120621         k_TA4nrs = D25nrs;
052300120621         k_TA4nsp = D25nsp;
052400120621         //k_TA4trc = ...;
052500120621
052600130315         // -?Aggancio record FIAR531C ("GEN" qui, "BNB" non qui!)?
052700130315         clear  dAR5gen;
052800120625         clear  dAR5bnb;
052900120621         clear  k08fiar531;
053000120621         k_AR5aas = D25aas;
053100120621         k_AR5lnp = D25lnp;
053200120621         k_AR5nrs = D25nrs;
053300120621         k_AR5nsp = D25nsp;
053400120621         //k_AR5trd = ...;
053500130315         exsr  sr_RepFIAR5_gen;
053600120620
053700120621         // -?Impostaz. campi della DS in base a quelli in TITAS/TAA/TA4?
053800120625         //                                 ?e a quelli in FIAR5?
053900120621         exsr  sr_ds_TAx;
054000120620
054100120620       ENDSR;
054200120621
054300120621       //--------------------------------------------------------------
054400120621       //?Impostaz. campi della DS in base a quelli in TITAS/TAA/TA4.  ?
054500120621       //--------------------------------------------------------------
054600120621       BEGSR  sr_ds_TAx;
054700120621
054800120621         // -?Tipo bolla?
054900120621         if  D25tbl = *blank;
055000120621           D25tbl = TAStbl;
055100120621         endif;
055200120621
055300120621         // -?Codice bolla?
055400120621         D25cbo = TAScbo;
055500120621
055600120621         // -?Codice trattamento merce?
055700120621         D25ctm = TASctm;
055800120621
055900120625         // -?Caricamento tabelle "3A" = Codici Bolla,            ?
056000120621         exsr  sr_CarTAB;
056100120621
056200120621         // -?Mese/Giorno bolla?
056300120621         D25mgs = TASmgs;
056400120621
056500120621         // -?Riferimento mittente numerico?
056600120621         D25rmn = TASrmn;
056700120621
056800120621         // -?Riferimento mittente alfanumerico?
056900120625         exsr  sr_RepTITA4_A;
057000120621         D25rma = §TA4Arma;
057100160122         D25nas = §TA4Anas;
057200120621
057300120621         // -?Linea di arrivo?
057400120621         D25lna = TASlna;
057500120621         exsr  sr_DecodLNA;
057600120621
057700120621         // -?Cliente MITTENTE?
057800120621         clear  D25ccm;
057900120621         select;
058000120621           when  TASccm <> *zero;
058100120621             D25ccm = TASccm;
058200120621           when  ( §3Atb2 <> *blank  and
058300120621                   %subst( §3Atb2 : 1 : 1 ) = 'F' )  OR
058400120621                 %subst( §3Atb1 : 1 : 1 ) = 'F';
058500120621             D25ccm = TASksc;
058600120621         endsl;
058700120622         If  %subst( %editc( TASccm : 'X' ) : 4 : 4 ) = *zero  or
058800120622             %subst( %editc( TASccm : 'X' ) : 4 : 4 ) = *all'8';
058900120622           exsr  sr_RepTITAA_M;
059000120622           D25rsm = mTAArsc;
059100120622           D25inm = mTAAind;
059200120622           D25cam = mTAAcap;
059300120622           D25lom = mTAAloc;
059400120622           D25prm = mTAAprv;
059500120622           D25nzm = mTAAnaz;
059600120622         Else;
059700120622           clear  TIBS69ds;
059800120622           //I69sif = knsif;
059900120622           I69kcc = DUTkci;
060000120622           I69kac = D25ccm;
060100120622           I69kin = D25ccm;
060200120622           tibs69r( tibs69ds :
060300120622                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
060400120622           if  O69err <> *on;
060500120622             D25rsm = %subst( ACOrag : 1 : %len(D25rsm) );
060600120622             //D25rm2 = %subst( ACOrag : %len(D25rsm) + 1 );
060700120622             D25inm = INDvia;
060800120622             D25cam = INDcae;
060900120622             D25lom = INDcit;
061000120622             D25prm = INDprv;
061100120622             D25nzm = INDsta;
061200120622           endif;
061300120622         EndIf;
061400120621
061500120621         // -?Mittente originale?
061600120622         D25rmo = oTAArsc;
061700120621
061800120621         // -?Flag anteporto?
061900120621         D25fap = TASfap;
062000120621
062100120621         // -?Flag "allega documento di trasporto"?
062200120625         if  TASll1 = 'C'  or  TASll1 = 'S';
062300120625           D25bam = 'S';
062400120625         endif;
062500120621
062600120621         // -?Sigla operatore?
062700120621         D25sop = TASsop;
062800120621
062900120621         // -?Cliente DESTINATARIO?
063000120621         if  %subst( §3Atb1 : 1 : 1 ) = 'A';
063100120621           D25ccd = TASksc;
063200120621         endif;
063300120621         // ·?Se c'è 2ª bolla si imposta LNA9999?
063400120621         if  §3Atb2 <> *blank;
063500120621           D25ccd = (TASlna * 10000) + 9999;
063600120621         endif;
063700120621         D25rsd = TASrsd;
063800120621         clear  D25rd2;
063900120621         D25ind = TASind;
064000120621         D25cad = TAScad;
064100120621         D25lod = TASlod;
064200120621         D25prd = TASprd;
064300120621         D25nzd = TASnzd;
064400120621         // ·?Reperimento eventuale 2ª ragione sociale?
064500120625         exsr  sr_RepTITA4_D;
064600120621
064700120621         // -?Flag inoltro?
064800120621         D25fin = TASfin;
064900120621
065000120621         // -?Numero colli?
065100120621         D25ncl = TASncl;
065200120621         // -?Peso?
065300120621         D25pkf = TASpkf;
065400120621         // -?Volume?
065500120621         D25fvf = TASfvf;
065600120621         D25vlf = TASvlf;
065700120621
065800160122         // -?Natura merce ora in tita4 record A?
065900160122         //  D25nas = TASnas;
066000120621
066100120621         // -?Note?
066200120625         //  ?NO: in sede sono riportate solo per i clienti che l'hanno?
066300120625         //      ?richiesto.?
066400120622         clear  D25nt1;
066500120622         clear  D25nt2;
066600120625         //exsr  sr_RepTITA4_89;   ?(NO: in sede sono per pochi clienti)?
066700120621
066800120621         // -?Particolarità?
066900120621         D25gga = TASgga;
067000120621         D25gma = TASgma;
067100120621         D25gva = %subst( TASgva : 1 : 1 );
067200120621         exsr  sr_SchPARTIC;
067300120621
067400120621       ENDSR;
067500120621
067600120621       //--------------------------------------------------------------
067700120621       //?Decodifica Linea di Arrivo.                                  ?
067800120621       //--------------------------------------------------------------
067900120621       BEGSR  sr_DecodLNA;
068000120621
068100120621         chain  (D25lna)  AZORG;
068200120621
068300120621         if  %found(AZORG01L)  and  ORGfva = *blank;
068400120621           D25fag = ORGfag;
068500120621           D25dla = ORGdes;
068600120621           D25inl = ORGind;
068700120621           D25cal = ORGcpf;
068800120621           D25lol = ORGloc;
068900120621           D25prl = ORGpro;
069000120621           D25tel = ORGtel;
069100120621           D25fax = ORGfax;
069200120621         endif;
069300120621
069400120621       ENDSR;
069500120622
069600120622       //--------------------------------------------------------------
069700120622       //?Reperimento dati da TITAA30C x trk "M"                       ?
069800120622       //?(Indirizzo Mittente)                                         ?
069900120622       //--------------------------------------------------------------
070000120622       BEGSR  sr_RepTITAA_M;
070100120622
070200120622         //clear  TITAA_M_ds;      ?(già fatto)?
070300120622
070400120622         //clear  k05titaa30;      ?(già fatto)?
070500120622         //k_TAAaas = D25aas;      ?(già così)?
070600120622         //k_TAAlnp = D25lnp;      ?(già così)?
070700120622         //k_TAAnrs = D25nrs;      ?(già così)?
070800120622         //k_TAAnsp = D25nsp;      ?(già così)?
070900120622
071000120622         k_TAAtrc = 'M';
071100120622
071200120622         chain  %kds( k05titaa30 )  TITAA30C;
071300120622
071400120622         if  %found(TITAA30C);
071500120622           TITAA_M_ds = TITAAds;
071600120622         endif;
071700120622
071800120622       ENDSR;
071900120621
072000120621       //--------------------------------------------------------------
072100120622       //?Reperimento dati da TITA430C x trk "A"                       ?
072200120622       //?(Riferimento Mittente Alfanumerico)                          ?
072300120621       //--------------------------------------------------------------
072400120625       BEGSR  sr_RepTITA4_A;
072500120621
072600120625         //clear  dTA4A;           ?(già fatto)?
072700120621
072800120621         k_TA4trc = 'A';
072900120621         chain  %kds( k05tita430 )  TITA430C;
073000120621
073100120621         if  %found(TITA430C);
073200120621           dTA4A = TA4not;
073300120621         endif;
073400120621
073500120621       ENDSR;
073600120621
073700120621       //--------------------------------------------------------------
073800120622       //?Reperimento dati da TITA430C x trk "D"                       ?
073900120622       //?(2ª ragione sociale del destinatario)                        ?
074000120621       //--------------------------------------------------------------
074100120625       BEGSR  sr_RepTITA4_D;
074200120621
074300120621         //clear  dTA4D;           ?(NON esiste)?
074400120621         //clear  D25rd2;          ?(già fatto)?
074500120621
074600120621         k_TA4trc = 'D';
074700120621         chain  %kds( k05tita430 )  TITA430C;
074800120621
074900120621         if  %found(TITA430C);
075000120621           D25rd2 = TA4not;
075100120621         endif;
075200120621
075300120621       ENDSR;
075400120621
075500120621       //--------------------------------------------------------------
075600120622       //?Reperimento dati da TITA430C x trk "8" / "9"                 ?
075700120622       //?(Note - ma SOLO per alcuni clienti)                          ?
075800120621       //--------------------------------------------------------------
075900120625       //BEGSR  sr_RepTITA4_89;
076000120622       //
076100120622       //  //clear  D25nt1;          ?(già fatto)?
076200120622       //  //clear  D25nt2;          ?(già fatto)?
076300120622       //  //clear  dTA48;           ?(NON esiste)?
076400120622       //  //clear  dTA49;           ?(NON esiste)?
076500120622       //
076600120622       //  k_TA4trc = '8';
076700120622       //  chain  %kds( k05tita430 )  TITA430C;
076800120622       //  if  %found(TITA430C);
076900120622       //    D25nt1 = TA4not;
077000120622       //  endif;
077100120622       //
077200120622       //  k_TA4trc = '9';
077300120622       //  chain  %kds( k05tita430 )  TITA430C;
077400120622       //  if  %found(TITA430C);
077500120622       //    D25nt2 = TA4not;
077600120622       //  endif;
077700120622       //
077800120622       //ENDSR;
077900120625
078000120625       //--------------------------------------------------------------
078100120625       //?Reperimento Numero Colli Originale da FIAR5/BNB              ?
078200120625       //--------------------------------------------------------------
078300120625       BEGSR  sr_RepFIAR5_bnb;
078400120625
078500120625         //clear  dAR5bnb;         ?(già fatto)?
078600120625
078700120625         k_AR5trd = 'BNB';
078800120625         chain  %kds( k08fiar531 : 5 )  FIAR531C;
078900120625
079000120625         if  %found(FIAR531C);
079100120625           dAR5bnb = AR5uni;
079200120625         endif;
079300120625
079400120625       ENDSR;
079500130315
079600130315       //--------------------------------------------------------------
079700130315       //?Reperimento presenza di Allegati da FIAR5/GEN                ?
079800130315       //--------------------------------------------------------------
079900130315       BEGSR  sr_RepFIAR5_gen;
080000130315
080100130315         //clear  dAR5gen;         ?(già fatto)?
080200130315
080300130315         k_AR5trd = 'GEN';
080400130315         chain  %kds( k08fiar531 : 5 )  FIAR531C;
080500130315
080600130315         if  %found(FIAR531C);
080700130315           dAR5gen = AR5uni;
080800130315         endif;
080900130315
081000130315       ENDSR;
081100120621
081200120621       //--------------------------------------------------------------
081300120621       //?Inschieramento delle Note e delle Particolarità              ?
081400120621       //--------------------------------------------------------------
081500120621       BEGSR  sr_SchPARTIC;
081600120621
081700120621         //clear  $NP;             ?(già fatto)?
081800120621         //clear  XX;              ?(già fatto)?
081900120621         reset  $NoDesPartic;
082000120621
082100120621         // -?Impostazione delle eventuali note per prime?
082200120621         if  D25nt1 <> *blank;
082300120621           xx += 1;
082400120621           $NP(xx) = D25nt1;
082500120621         endif;
082600120621         if  D25nt2 <> *blank;
082700120621           xx += 1;
082800120621           $NP(xx) = D25nt2;
082900120621         endif;
083000120621
083100120621         // -?Le particolarità possono essere lunghe fino a 45 char.?
083200120621         //  ?Se i primi 30 (della 1ª riga a disposizione) non dovessero?
083300120621         //  ?bastare, il resto lo si mette nella riga successiva.?
083400120621         //  ?Se, invece, non risultano decodificate, si mettono solo i?
083500120621         //  ?codici sulla stessa riga.?
083600120621         //  ?Se uno solo risulta decodificato, esso va su una riga a?
083700120621         //  ?parte (vedi flag "$NoDesPartic": *off = serve riga nuova?
083800120621         //                                   ?*on  = ok riga procedente)?
083900120621
084000120621         // . -?Particolarità Giacenza?
084100120621         IF  D25gga <> *blank;
084200120621           wTpPar = 'Q';
084300120621           wCdPar = D25gga;
084400120621           exsr  sr_TABxPAR;
084500120621           if  TBLuni = *blank  and  $NoDesPartic = *on;
084600120621             $NP(xx) = %trim($NP(xx)) + '   ' + wCdPar;
084700120621           else;
084800120621             $NoDesPartic = (TBLuni = *blank);
084900120621             xx += 1;
085000120621             $NP(xx) = w053a;
085100120621             if  %subst( w053a : %len( $NP(xx) ) +1 ) <> *blank;
085200120621               xx +=1;
085300120621               $NP(xx) = '   ' +
085400120621                         %trim( %subst( w053a : %len( $NP(xx) ) + 1 ) );
085500120621             endif;
085600120621           endif;
085700120621         ENDIF;
085800120621
085900120621         // . -?Particolarità Consegna?
086000120621         IF  D25gma <> *blank;
086100120621           wTpPar = 'R';
086200120621           wCdPar = D25gma;
086300120621           exsr  sr_TABxPAR;
086400120621           if  TBLuni = *blank  and  $NoDesPartic = *on;
086500120621             $NP(xx) = %trim( $NP(xx) ) + '   ' + wCdPar;
086600120621           else;
086700120621             $NoDesPartic = (TBLuni = *blank);
086800120621             xx += 1;
086900120621             $NP(xx) = w053a;
087000120621             if  %subst( w053a : %len( $NP(xx) ) + 1) <> *blank;
087100120621               xx += 1;
087200120621               $NP(xx) = '   ' +
087300120621                         %trim( %subst( w053a : %len( $NP(xx) ) + 1 ) );
087400120621             endif;
087500120621           endif;
087600120621         ENDIF;
087700120621
087800120621         // . -?Particolarità Varie?
087900120621         //    ?- questa descrizione va messa nell'apposito campo PPAD?
088000120621         //      ?se è "PRIVATO"; idem se è "CONSEGNA AD UFFICI";?
088100120625         //    ?- se 'B' (bancali da rendere al mittente) è comparso un?
088200120625         //      ?apposito box (nella vera LdV);?
088300120621         //    ?- se 'O' (oltre 20 colli => bancali da riportare in mag.)?
088400120625         //      ?è comparso un apposito box, un'apposita nota e l'indica-?
088500120621         //      ?zione nella copia per il destinatario.?
088600120621         //    ?...Potrebbero anche non esserci più elementi di schiera?
088700120621         //    ?disponibili [xx >= %elem($NP)] !?
088800120621         IF  D25gva <> *blank   and
088900120621             D25gva <> 'B '     and  D25gva <> 'O '  and
089000120621             D25gva <> 'P '     and  D25gva <> 'U ';
089100120621           wTpPar = 'S';
089200120621           wCdPar = D25gva;
089300120621           exsr  sr_TABxPAR;
089400120621           if  TBLuni = *blank  and  $NoDesPartic = *on;
089500120621             $NP(xx) = %trim( $NP(xx) ) + '   ' + wCdPar;
089600120621           else;
089700120621             $NoDesPartic = (TBLuni = *blank);
089800120621             if  xx < %elem($NP);
089900120621               xx += 1;
090000120621               $NP(xx) = w053a;
090100120621               if  %subst( w053a : %len( $NP(xx) ) + 1) <> *blank  and
090200120621                   xx < %elem($NP);
090300120621                 xx += 1;
090400120621                 $NP(xx) = '   ' +
090500120621                           %trim( %subst( w053a : %len( $NP(xx) ) + 1 ) );
090600120621               endif;
090700120621             endif;
090800120621           endif;
090900120621         ENDIF;
091000120621
091100120621         // -?Imposto il numero originale di colli componenti la spediz.?
091200120621         //  ?e se ho ancora spazio lo memorizzo in "Note e Particolarità"?
091300120621         if  %subst( D25gva : 1 : 1 ) = 'O'  and  xx < %elem($NP);
091400120625           exsr  sr_RepFIAR5_bnb;
091500120621           if  §AR5bcor <> *zero;
091600120621             xx += 1;
091700120621             $NP(xx) = c_CoOr + %editc( §AR5bcor : 'K' );
091800120621           endif;
091900120621         endif;
092000120621
092100120621       ENDSR;
092200120621
092300120621       //--------------------------------------------------------------
092400120621       //?Reperimento PARTICOLARITÀ su TABEL00F                        ?
092500120621       //--------------------------------------------------------------
092600120621       BEGSR  sr_TABxPAR;
092700120621
092800120621         w053a = wCdPar;
092900120621         k_TBLcod = '7V';
093000120621         k_TBLkey = KeyPAR;
093100120621         chain  %kds (k03tabel00 )  TABEL;
093200120621
093300120621         if  %found(TABEL00F)  and  TBLflg = *blank;
093400120621           w053a = wCdPar + '-' + %trim( %subst( TBLuni : 1 : 49 ) );
093500120621         else;
093600120621           clear  TBLuni;
093700120621         endif;
093800120621
093900120621       ENDSR;
094000120620
094100120620       //--------------------------------------------------------------
094200120620       //?Preparazione dati per
094300120620       //?stampa copia LdV x scannerizzazione documenti clienti.       ?
094400120620       //--------------------------------------------------------------
094500120620       BEGSR  sr_RiempiLdV;
094600120620
094700120620         // -?Impotazione barcode: Anno+Lnp+Nrs+Nsp+Ckd?
094800120620         //  ?(richiamo del TRUL28R per ricavarne il check-digit)?
094900120620         reset  TRUL28ds;
095000120620         BARcdeAAS = %int( %subst( %editc( D25aas : 'X' ) : 3 : 2 ) );
095100120620         BARcdeLNP = D25lnp;
095200120620         BARcdeNRS = D25nrs;
095300120620         BARcdeNSP = D25nsp;
095400120620         I28cod    = BarCodeDS;
095500120620         trul28R1 ( trul28ds );
095600120620         if O28err = *blank;
095700120620           Tbcd = O28cod;
095800120620           *in50 = *on;
095900120620         endif;
096000120620
096100120620         // -?Costanti: dati BRT S.p.A.?
096200120620         TBAR = $BRT;
096300120620         TBA1 = $Intest1;
096400120620         TBA2 = $Intest2;
096500120621         //TBA3 = $Intest3;        ?NO!!!?
096600120620         TBA4 = $Intest4;
096700120620         TBA5 = $Intest5;
096800120621         //TBA6 = $Intest6;        ?NO!!!?
096900120620         TBA7 = $Intest7;
097000120620
097100120620         // -?Mittente?
097200120621         if  D25ccm <> *zero;
097300120621           Pccm = %editc( D25ccm : 'X' );
097400120620         endif;
097500120620         Prsm = D25rsm;
097600120620         Pinm = D25inm;
097700120621         Pcam = D25cam;
097800120621         Plom = D25lom;
097900120621         Pprm = D25prm;
098000120621         Pnzm = D25nzm;
098100120625
098200120621         // -?Mittente originale?
098300120621         if  D25rmo <> *blank;
098400120621           Pkmo = 'MITT. ORIGINALE';
098500120621           Prmo = D25rmo;
098600120621         endif;
098700120621
098800120621         // -?Flag "allega documento di trasporto"?
098900120621         select;
099000120621           // -?LdV con DDT allegato oltre il 1° tentativo di consegna?
099100120621           //when  D25bam = 'S'   and   w25tmc <> *blank;
099200120621           //  evalr Pddt = 'DDT sul collo';
099300120621           // -?LdV con DDT allegato (al 1° tentativo di consegna)?
099400120621           when  D25bam = 'S';
099500120621             evalr Pddt = 'DDT';
099600130315           // -?Esiste PDF abbinato?
099700130321           when  §AR5als = 'S';
099800130315             evalr Pddt = 'PACKING-LIST PDF';
099900120621           // -?Esiste packing-list abbinata?
100000130321           when  §AR5alx = 'S';
100100120621             evalr Pddt = 'PACKING-LIST';
100200120621         endsl;
100300120621
100400120621         // -?Sigla Operatore?
100500120625         //Psop = D25sop;
100600120621
100700120621         // -?Destinatario?
100800120621         if  D25ccd <> *zero;
100900120621           Pccd = %editc( D25ccd : 'X' );
101000120621         endif;
101100120621         Prsd = D25rsd;
101200120621         Prse = D25rd2;
101300120621         Pind = D25ind;
101400120621         Pcad = D25cad;
101500120621         Plod = D25lod;
101600120621         Pprd = D25prd;
101700120621         Pnzd = D25nzd;
101800120621
101900120621         // -?Colli: numero, peso, volume, flag tipo-volume?
102000120621         Pncl = D25ncl;
102100120621         Ppkf = D25pkf;
102200120621         Pvlf = D25vlf;
102300120621         if  D25fvf <> *blank;
102400120621           evalr Pfvf = D25fvf;
102500120621         endif;
102600120621
102700120621         // -?Natura merce?
102800120621         Pnat = D25nas;
102900120621
103000120621         // -?Filiale di Arrivo?
103100120621         Pkla  = 'FILIALE';
103200120621         Plna1 = %editc(D25lna : 'X')
103300120621               + ' '
103400120621               + D25dla
103500120621               + ' - '
103600120621               + D25inl
103700120621               + ' Tel.'
103800120621               + %trimr(D25tel);
103900120621         Plna2 = %editc(D25cal : 'X')
104000120621               + ' '
104100120621               + D25lol
104200120621               + ' '
104300120621               + D25prl
104400120621               + '                  '
104500120621               + ' Fax '
104600120621               + %trimr(D25fax);
104700120621
104800120621         // -?Riferimenti Mittente?
104900120621         Prmn = D25rmn;
105000120621         Prma = D25rma;
105100120621
105200120621         // -?Note (1 e 2) e Particolarità (3, 4, 5 e 6)?
105300120621         if  $NP(1) <> *blank  or  $NP(2) <> *blank  or
105400120621             $NP(3) <> *blank  or  $NP(4) <> *blank  or
105500120621             $NP(5) <> *blank  or  $NP(6) <> *blank;
105600120621           Pknp = 'NOTE  E  PARTICOLARITÀ';
105700120621           Pnp1 = $NP(1);
105800120621           Pnp2 = $NP(2);
105900120621           Pnp3 = $NP(3);
106000120621           Pnp4 = $NP(4);
106100120621           Pnp5 = $NP(5);
106200120621           Pnp6 = $NP(6);
106300120621         endif;
106400120621
106500120625         // -?Privato / Uffici?
106600120621         select;
106700170413           //*·when  D25gva = 'P ';
106800170413           //*·  evalr Ppad = 'PRIVATO';
106900120625           when  D25gva = 'U ';
107000120621             evalr Ppad = 'CONSEGNA AD UFFICI';
107100120625           //when  TASftc = 'A'  or  TAStc2 = 'A';
107200120625           //  evalr Ppad = 'APPUNTAMENTO';
107300120625           //when  D25gva = 'B ';
107400120625           //  evalr Ppad = 'BANCALI DA RENDERE AL MITTENTE';
107500120625           //when  D25gva = 'O ';
107600120625           //  evalr Ppad = 'BANCALI DA RIPORTARE A MAGAZZINO';
107700120621         endsl;
107800120621
107900120621         // -?Numero Spedizione?
108000120621         Plnp = D25lnp;
108100120621         Pnrs = D25nrs;
108200120621         Pnsp = D25nsp;
108300120621
108400120621         // -?Data Spedizione?
108500120622         //wData_EUR = %date( (D25aas * 10000) + D25mgs : *iso );
108600120622         //Pdsp = %dec(wData_EUR);
108700120622         reset  wData_EUR;
108800120622         monitor;
108900120622           wData_EUR = %date( (D25aas * 10000) + D25mgs : *iso );
109000120622           Pdsp = %dec(wData_EUR);
109100120622         on-error;
109200120622           Pdsp = D25aas;
109300120622         endmon;
109400120621
109500120621       ENDSR;
109600120620
109700120620       //--------------------------------------------------------------
109800120620       //?Stampa singola copia LdV x scannerizzaz. documenti clienti.  ?
109900120620       //--------------------------------------------------------------
110000120620       BEGSR  sr_StampaLdV;
110100120620
110200120620         // -?Posizionamento in testa al foglio A4?
110300120620         exsr  sr_Posiz_Box1;
110400120620
110500120620         // -?Copia LdV x scanerizzazione documenti clienti?
110600120620         write  A4prtLDV;
110700120620         write  A4prtBOX;
110800120620
110900120620         // -?Simil-Logo BRT?
111000120620         write  A4prtLOGO;
111100120620         write  A4prtLOGOd;
111200120620
111300120620       ENDSR;
111400120620
111500120620       //--------------------------------------------------------------
111600120620       //?Posizionamento del box all'interno della pag. A4.            ?
111700120620       //--------------------------------------------------------------
111800120620       BEGSR  sr_Posiz_Box1;
111900120620
112000120620         // -?Si salta alla pag. A4 successiva: la LdV in esame?
112100120620         //  ?dev'essere stampata in posizione iniziale dell'A4.?
112200120620         wSeqLdV = 1;
112300120620
112400120620         // -?Impostaz. delle posizioni nella pagina in corso di stampa?
112500120620         wPosIni = (c_Len1LdV * (wSeqLdV - 1));
112600120620
112700120620         // -?Posizionamento nel foglio A4 in base al n° LdV in stampa?
112800120620         WRITE  A4prtPOS;
112900120620
113000120620         // -?Posizionamenti nella copia per il vettore?
113100120620
113200120620         // -?Box (& Logo & BarCode)?
113300120620         §Box1As = wPosIni + c_PosBox1As;
113400120620         §Box1Ai = wPosIni + c_PosBox1Bs + c_LineNarrow;
113500120620         §Box1Bs = wPosIni + c_PosBox1Bs;
113600120620         §Box1Bi = wPosIni + c_PosBox1Bi;
113700120620
113800120620         // -?Simil-Logo BRT?
113900120620         §PlblS  = §Box1As;
114000120620         §PlblVS = 7.581;
114100120620         exsr  sr_SimilLogoBRT_;
114200120620
114300120620         // -?Costanti dati anagrafici BRT?
114400120620         §PC0    = wPosIni + c_PosTBar;
114500120620         §PC1    = wPosIni + c_PosTBa1;
114600120620         §PC2    = wPosIni + c_PosTBa2x;
114700120620         §PC3    = wPosIni + c_PosTBa3x;
114800120620         §PC4    = wPosIni + c_PosTBa4x;
114900120620         §PC5    = wPosIni + c_PosTBa5x;
115000120620         §PC6    = wPosIni + c_PosTBa6x;
115100120620
115200120620         // -?Linee orizzontali nel box?
115300120620         §Lo1S1  = wPosIni + c_PosLin1oS1;
115400120620         §Lo1S2  = wPosIni + c_PosLin1oS2;
115500120620       //§Lo1S3  = wPosIni + c_PosLin1oS3;
115600120620         §Lo1S4  = wPosIni + c_PosLin1oS4;
115700120620         §Lo1S5  = wPosIni + c_PosLin1OS5;
115800120620         //§Lo1C1  = wPosIni + c_PosLin1oC1;
115900120620         §Lo1C2  = wPosIni + c_PosLin1oC2;
116000120620         §Lo1D1  = wPosIni + c_PosLin1oD1;
116100120620
116200120620         // -?Lunghezza linee verticali nel box in copia per il vett.?
116300120620         §LLv1A  = §Lo1S5  - §Lo1S1;
116400120620         §LLv1B  = §Lo1S4  - §Box1Bs;
116500120620         §LLv1C  = §Box1Bi - §Box1Bs;
116600120620
116700120620       ENDSR;
116800120620
116900120620      /end-free
117000120620
117100120620       //-------------------------------------------------------------
117200120620       //?Posizionamento del simil-logo BRT (unico)                   ?
117300120620       //-------------------------------------------------------------
117400120620      /copy gaitrasrc/srcTNSY,LogoBRT_R2
117500120620
117600120620      /free
117700120620
117800120620       //--------------------------------------------------------------
117900120620       //?Operazioni finali.                                           ?
118000120620       //--------------------------------------------------------------
118100120620       BEGSR  sr_RoutEnd;
118200120620
118300120620         // -?Chiusura archivi / applicazioni?
118400120620         If  D25tla <> *blank;
118500120620
118600120620           *inLR = *on;
118700120622
118800120622           // -?Chiusura funzioni precedentemente aperte?
118900120622           reset  TIBS69ds;
119000120622           //I69tla = 'C';         ?(già così)?
119100120622           tibs69r( tibs69ds :
119200120622                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
119300120620
119400120620           // -?Chiusura file di stampa?
119500120620           close  TNSB25P;
119600120621
119700120621           // -?Chiusura files?
119800120621           if  %open(TITAS30C);
119900120621             close  TITAS30C;
120000120621           endif;
120100120621           if  %open(TITAA30C);
120200120621             close  TITAA30C;
120300120621           endif;
120400120621           if  %open(TITA430C);
120500120621             close  TITA430C;
120600120621           endif;
120700120621           if  %open(FIAR531C);
120800120621             close  FIAR531C;
120900120621           endif;
121000120620
121100120620         EndIf;
121200120620
121300120620         // -?Ritorno al chiamante?
121400120620         return;
121500120620
121600120620       ENDSR;
121700120620
121800120620      /end-free
121900120620
122000120620       //--------------------------------------------------------------
122100120620       //?Definizione schiere a tempo di compilazione                  ?
122200120620       //--------------------------------------------------------------
122300120620
122400120620** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
122500120620IMPOSSIBILE CONTINUARE LA RISTAMPA DELLE SIMIL-LDV
122600120620PERCHE' NE SONO GIA' STATE STAMPATE PIU' DI &NrLdV !!!
