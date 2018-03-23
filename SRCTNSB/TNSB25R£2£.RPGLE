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
005200170517     fTFNTC01L  if   e           k disk    usropn
005300120619
005400120619       // -?File di stampa?
005500120619     fTNSB25P   o    e             printer usropn
005600120619     f                                     rename(SB25POS   :A4prtPOS  )
005700120619     f                                     rename(SB25BOX   :A4prtBOX  )
005800120619     f                                     rename(SB25LOGO  :A4prtLogo )
005900120619     f                                     rename(SB25LOGODA:A4prtLogoD)
006000120619     f                                     rename(SB25LDV   :A4prtLDV  )
006100120621     f                                     rename(SB25MSG   :A4prtMSG  )
006200120619
006300120619       //--------------------------------------------------------------
006400120619       //?Definizione costanti.                                        ?
006500120619       //--------------------------------------------------------------
006600120619
006700120620       // -?Intestazioni in stampa?
006800120620     d $BRT            c                   const('BRT S.p.A.')
006900120620     d $Intest1        c                   const('Sede Operativa ed Am-
007000120620     d                                     ministrativa:')
007100120620     d $Intest2        c                   const('via E. Mattei, 42 -
007200120620     d                                     - 40138 Bologna')
007300120620     d $Intest3        c                   const('Sede Legale: Piazza -
007400120620     d                                     Diaz, 7 - 20123 Milano')
007500120620     d $Intest4        c                   const('C.F. e P.IVA 04507990150')
007600120620     d $Intest5        c                   const(' Capitale Sociale Eu-
007700120620     d                                     ro 24.000.000 int. vers.')
007800120620     d $Intest6        c                   const('info on-line')
007900120620     d $Intest7        c                   const('www.brt.it')
008000120620
008100120620       // -?Lunghezza (in inches) di una singola LdV?
008200120620     d c_Len1LdV       c                   const(5.775)
008300120620
008400120620       // -?Spessore delle linee in inches/pollici (*memo)?
008500120620     d c_LineNarrow    c                   const(0.008)
008600120620     d c_LineMedium    c                   const(0.017)
008700120620     d c_LineWide      c                   const(0.025)
008800120620
008900120620       // -?Posizioni dell'intestazione "BRT S.p.A."?
009000120620     d c_PosTBar       c                   const(0.270)
009100120620     d c_PosTBa1       c                   const(0.350)
009200120620     d c_PosTBa2x      c                   const(0.430)
009300120620     d c_PosTBa3x      c                   const(0.500)
009400120620     d c_PosTBa4x      c                   const(0.590)
009500120620     d c_PosTBa5x      c                   const(0.660)
009600120620     d c_PosTBa6x      c                   const(0.750)
009700120620
009800120620       // -?Posizioni dei bordi orizzontali superiori box?
009900120620       //  ?(distanza dal lato superiore dell'A4)?
010000120620       //  ?N.B.?- c_PosBoxNXs => posizione iniziale della linea?
010100120620       //                        ?orizzontale Superiore?
010200120620       //       ?- c_PosBoxNXi => posizione iniziale della linea?
010300120620       //                        ?orizzontale Inferiore?
010400120620     d c_PosBox1As     c                   const(0.200)
010500120620     d c_PosBox1Bs     c                   const(0.790)
010600120620     d c_PosBox1Bi     c                   const(2.850)
010700120620     d c_PosBox1Cs     c                   const(1.950)
010800120620     d c_PosBox1Ds     c                   const(2.453)
010900120620       // -?Posizioni di altre linee orizzontali?
011000120620     d c_PosLin1OS1    c                   const(1.350)
011100120620     d c_PosLin1OS2    c                   const(1.790)
011200120620     d c_PosLin1OS3    c                   const(2.010)
011300120620     d c_PosLin1OS4    c                   const(2.230)
011400120620     d c_PosLin1OS5    c                   const(2.470)
011500120620     d c_PosLin1OC1    c                   const(1.680)
011600120620     d c_PosLin1OC2    c                   const(1.240)
011700120620     d c_PosLin1OD1    c                   const(1.010)
011800120620
011900120620       // -?Dimensioni base per i simil-loghi Bartolini e BRT?
012000120620      /copy gaitrasrc/srcTNSY,LogoBRT_R1
012100120620
012200120620       // -?Limite righe stampabili su 1/4 di A4 in 1/9"?
012300120620     d c_MaxLine1      c                   const(025)
012400120621
012500120621       // -?COSTANTI IN STAMPA?
012600120621     d c_CoOr          c                   const('COLLI ORIGINALI     -
012700120621     d                                     N. ')
012800120622
012900120622       // -?Codice utente x TABEL00F?
013000120622     d c_Kut           c                   const(1)
013100120620
013200120620       //--------------------------------------------------------------
013300120620       //?Definizione schiere.                                         ?
013400120620       //--------------------------------------------------------------
013500120620
013600120620       // -?Messaggi di errore?
013700120621     d $Msg            s             78    dim( 2)  ctdata  perrcd( 1)
013800120620
013900120620       // -?Note e Particolarità?
014000120620     d $NP             s                   dim(6)  inz  like(Pnp1)
014100120620
014200120620       //--------------------------------------------------------------
014300120620       //?Definizione aree dati.                                       ?
014400120620       //--------------------------------------------------------------
014500120620
014600120620       // -?Dati utente?
014700120620     d §AzUte        e ds                  extname(AZUTE00F)
014800120620     d                                     dtaara
014900120620     d §DatiUte      e ds                  extname(dDatiUte)
015000120620     d                                     dtaara
015100120620
015200120620       //--------------------------------------------------------------
015300120620       //?Definizione strutture dati.                                  ?
015400120620       //--------------------------------------------------------------
015500120621
015600120621       // -?Parametri ricevuti?
015700120621     d TNSB25ds      e ds
015800120621
015900120621       // -?Tabella "3A" = Codici Bolla?
016000120621     d ds3A          e ds                  inz
016100120621       // -?Tabella "VPO" = Variabili Programmi Operativi?
016200120621     d dVPO          e ds                  inz
016300170517       // -?Tabella "CLI" = Abilitaz. particolari clienti?
016400170517     d dCLI          e ds                  inz
016500120621
016600120621       // -?Tipo rec. "A" del file TITA430C (Riferimento Alfanumerico)?
016700120621     d dTA4a         e ds                  inz
016800120621
016900130315       // -?Tipo rec. "BNB" del file FIAR531C (Bancali)?
017000120621     d dAR5bnb       e ds                  inz
017100130315
017200130315       // -?Tipo rec. "GEN" del file FIAR531C (Consegna/Allegati/...)?
017300130315     d dAR5gen       e ds                  inz
017400170517
017500170517       // -?Dati da AZORG00F?
017600170517     d Og143ccm      e ds                  inz               qualified
017700170517     d                                     extname(Og143)    prefix(§CM:3)
017800120622
017900120622       // -?Strutture dati di "comodo" per salvare i dati dei rec.?
018000120622       //  ?diversi del file TITAA (trk "M" e "O")?
018100120622     d TITAAds       e ds                  extname(TITAA00F)
018200120622     d TITAA_M_ds    e ds                  extname(TITAA00F) prefix(M)
018300120622     d TITAA_O_ds    e ds                  extname(TITAA00F) prefix(O)
018400120620
018500120620       // -?Codice a Barre?
018600120620     d BARcodeDS       ds            17    inz
018700120621     d   BARcdeAAS                    2s 0 inz
018800120621     d   BARcdeLNP                    3s 0 inz
018900120621     d   BARcdeNRS                    2s 0 inz
019000120621     d   BARcdeNSP                    7s 0 inz
019100120621     d*//BARcdeXXX                    3    inz
019200120621
019300120621       // -?Chiave di accesso ad eventuali Note?
019400120621     d KEYpar          ds             8    inz
019500120621     d   wTPpar                       1    inz
019600120621     d   wCDpar                       2    inz
019700120621     d   wProgr                       5    inz('S0001')
019800120620
019900120620       //--------------------------------------------------------------
020000120620       //?Definizione variabili globali.                               ?
020100120620       //--------------------------------------------------------------
020200120621
020300120621       // -?Flags booleani?
020400120621     d*// $Mamma          s               n   inz
020500120621     d $NoDesPartic    s               n   inz
020600120621
020700120621       // -?Indici di schiera?
020800120621     d xx              s              3  0 inz
020900120621
021000120621       // -?Nome del sistema?
021100120621     d currSysNeta     s              8a   inz
021200120620
021300120621       // -?Nomi estesi Libreria/File dei files?
021400120620     d wLibTITAS       s             21a   inz
021500120621     d wLibTITAA       s             21a   inz
021600120621     d wLibTITA4       s             21a   inz
021700120621     d wLibFIAR5       s             21a   inz
021800120620
021900120620       // -?Variabili per il calcolo delle posizioni per box in A4?
022000120620     d wSeqLdV         s              1  0 inz
022100120620     d wPosIni         s              5  3 inz
022200120621
022300120621       // -?Campi di comodo?
022400120621     d wData_EUR       s               d   datfmt(*eur)   inz(*loval)
022500120621     d w053a           s             53    inz
022600120620
022700120620       //--------------------------------------------------------------
022800120620       //?Definizione prototipi procedure.                             ?
022900120620       //--------------------------------------------------------------
023000120621
023100120621       // -?Reperimento NETA sistema AS/400 corrente?
023200120621      /copy gaitrasrc/srcProtoPR,UBRTVNETA
023300120620
023400120620       // -?Reperimento dati utente?
023500120620     d TIBS34ds      e ds
023600120620      /copy gaitrasrc/srcProtoPR,TIBS34R
023700120620
023800120620       // -?Parametri per TRUL28R (calcolo check digit del barcode)?
023900120620     d TRUL28ds      e ds                  inz
024000120702     d   I28tla      e                     inz('L')
024100120702     d   I28mod      e                     inz('BAR')
024200120620       // -?Calcolo check-digit per bar-code?
024300120620     d trul28r1        pr                  extpgm('TRUL28R1')
024400120620     d   trul28ds...
024500120620     d                                     likeds(TRUL28ds)
024600120622
024700120622       // -?Controllo/Decodifica cliente?
024800120622      /copy gaitrasrc/srcProtoPI,TIBS69R
024900120622      /copy gaitrasrc/srcProtoPR,TIBS69R
025000120620
025100120620       //--------------------------------------------------------------
025200120620       //?Definizione key-list.                                        ?
025300120620       //--------------------------------------------------------------
025400120620
025500120620       // -?File TABEL00F?
025600120620     d k03tabel00    e ds                  extname(TABEL00F : *key)
025700120620     d                                     prefix(k_)   inz
025800120620
025900120620       // -?File TNTBE01L?
026000120620     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
026100120620     d                                     prefix(k_)   inz
026200120620
026300120620       // -?File TITAS30C?
026400120620     d k05titas30    e ds                  extname(TITAS30C : *key)
026500120620     d                                     prefix(k_)   inz
026600120621
026700120621       // -?File TITAA30C?
026800120621     d k05titaa30    e ds                  extname(TITAA30C : *key)
026900120621     d                                     prefix(k_)   inz
027000120621
027100120621       // -?File TITA430C?
027200120621     d k05tita430    e ds                  extname(TITA430C : *key)
027300120621     d                                     prefix(k_)   inz
027400120621
027500120621       // -?File FIAR531C?
027600120621     d k08fiar531    e ds                  extname(FIAR531C : *key)
027700120621     d                                     prefix(k_)   inz
027800170517
027900170517       // -?File AZORG01L?
028000170517     d keyAZORG01    e ds                  extname(AZORG01L : *key)
028100170517     d                                     prefix(k_)   inz
028200170517
028300170517       // -?File TFNTC01L?
028400170517     d keyTFNTC01    e ds                  extname(TFNTC01L : *key)
028500170517     d                                     prefix(k_)   inz
028600120620
028700120620       //--------------------------------------------------------------
028800120620       //?M A I N - L I N E                                            ?
028900120620       //--------------------------------------------------------------
029000120620
029100120620     c     *Entry        plist
029200120620     c                   parm                    TNSB25ds
029300120620
029400120620      /free
029500120620
029600120620       // -?Operazioni iniziali?
029700120620       exsr  sr_RoutInz;
029800120620
029900120620       // -?Stampa?
030000120620       exsr  sr_Stampa;
030100120620
030200120620       // -?Operazioni finali?
030300120620       exsr  sr_RoutEnd;
030400120620
030500120620
030600120620       //--------------------------------------------------------------
030700120620       //?Operazioni iniziali.                                         ?
030800120620       //--------------------------------------------------------------
030900120620       BEGSR  *InzSR;
031000120620
031100120620         // -?Verifica del sistema AS/400 corrente?
031200120620         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
031300120620           exsr  sr_RoutEnd;
031400120620         endif;
031500120620
031600120620         // -?Impostazione libreria per file TITAS30C?
031700120620         //  ?(che verrà aperto SOLO se richiesto dal chiamante)?
031800120620         if  %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
031900120620           wLibTITAS = 'GAITRAGRU/TITAS30C';
032000120621           wLibTITAA = 'GAITRAGRU/TITAA30C';
032100120621           wLibTITA4 = 'GAITRAGRU/TITA430C';
032200120621           wLibFIAR5 = 'GAITRAGRU/FIAR531C';
032300120620         else;
032400120620           wLibTITAS = 'GAITRAGRPS/TITAS30C';
032500120621           wLibTITAA = 'GAITRAGRPS/TITAA30C';
032600120621           wLibTITA4 = 'GAITRAGRPS/TITA430C';
032700120621           wLibFIAR5 = 'GAITRAGRPS/FIAR531C';
032800120620         endif;
032900120620
033000120620         // -?Reperimento dati job?
033100120620         exsr  sr_DatiJob;
033200120622
033300120622         // -?Impostazione campi fissi?
033400120622         k_TBLkut = c_Kut;
033500120620
033600120620         // -?Reperimento n° max di LdV stampabili da tab. "VPO"?
033700120620         exsr  sr_RepTabVPO;
033800120620
033900120620         // -?Apertura del PRTF?
034000120620         open  TNSB25P;
034100120620
034200120620       ENDSR;
034300120620
034400120620       //--------------------------------------------------------------
034500120620
034600120620       BEGSR  sr_RoutInz;
034700120620
034800120620         // -?SE richiesta la sola chiusura del *pgm => uscita?
034900120620         if  D25tla = 'C';
035000120620           exsr  sr_RoutEnd;
035100120620         endif;
035200120620
035300120620         // -?Richiesta la RI-apertura del file di stampa?
035400120620         if  D25fl1 = *on;
035500120621           close  TNSB25P;
035600120621           open   TNSB25P;
035700120620         endif;
035800120620
035900120620       ENDSR;
036000120620
036100120620       //--------------------------------------------------------------
036200120620       //?Reperimento Dati del job (Utente/Operativi).                 ?
036300120620       //--------------------------------------------------------------
036400120620       BEGSR  sr_DatiJob;
036500120620
036600120620         in(E) §AzUte;
036700120620         if NOT %error;
036800120620           in(E) §DatiUte;
036900120620         endif;
037000120620         if %error or RSut = *blanks;
037100120620           clear TIBS34ds;
037200120620           tibs34r ( tibs34ds );
037300120620           in §AzUte;
037400120620           in §DatiUte;
037500120620         endif;
037600120620
037700120620       ENDSR;
037800120620
037900120620       //--------------------------------------------------------------
038000120620       //?Reperimento n° max di LsV stampabili da tab. "VPO"           ?
038100120620       //--------------------------------------------------------------
038200120620       BEGSR  sr_RepTabVPO;
038300120620
038400120620         // -?Aggancio tabella "VPO" per reperimento del n° LdV stampabili?
038500120620         //  ?limite (per congelamento e blocco stampa)?
038600120620         clear  dVPO;
038700170517         clear  k05tntbe01;
038800120620         k_TBEcod = 'VPO';
038900120620         k_TBEke1 = 'VPO';
039000120620         k_TBEke2 = *blank;
039100120620         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
039200120620         if  %found(TNTBE01L)  and  TBEatb = *blank;
039300120621           dVPO = TBEuni;
039400120620         endif;
039500120620         if  §VPOMaxLdv = *zero;
039600120620           §VPOMaxLdv = 10000;
039700120620         endif;
039800120620
039900120620       ENDSR;
040000120620
040100120620       //--------------------------------------------------------------
040200120620       //?Stampa copia LdV x scannerizzazione documenti clienti.       ?
040300120620       //--------------------------------------------------------------
040400120620       BEGSR  sr_Stampa;
040500120620
040600120620         // -?Pulizia del PRTF?
040700120620         exsr  sr_ClrPRTF;
040800120620
040900120620         Select;
041000120620
041100120620           // -?Passati i dati => è già (quasi) tutto pronto?
041200120620           When  D25ges = *blank;
041300120620             exsr  sr_CarTAB;
041400120621             if  D25dla = *blank;
041500120621               exsr  sr_DecodLNA;
041600120621             endif;
041700120620
041800120621           // -?Richiesto il reperimento dei dati da TITAS/TITAA/TITA4?
041900120621           //                                  ?e da FIAR5?
042000120620           When  D25ges = 'T';
042100120621             exsr  sr_CarTITAx;
042200120620
042300120621           // -?D25GES = "R" => Stampa una LDV vuota con un messaggio?
042400120620           //  ?per avvisare che è impossibile continuare la ristampa?
042500120620           //  ?perchè stampate più di 10.000 LdV !?
042600120620           When  D25ges = 'R';
042700120620             Pmsg1  = $Msg(1);
042800120620             Pmsg2  = %replace( %editc( §VPOmaxldv : 'J' ) :
042900120620                                        $Msg(2) : %scan(
043000120620                                        '&NrLdV' : $Msg(2) ) );
043100120620             exsr  sr_Posiz_Box1;
043200120620             write  A4prtMSG;
043300120620             D25tla = 'L';
043400120620             exsr  sr_RoutEnd;
043500120620
043600120620         EndSl;
043700120620
043800120620         // -?Preparazione dati per stampa bolla?
043900120620         exsr  sr_RiempiLDV;
044000120620
044100120620         // -?Stampa copia LdV x scannerizzazione documenti clienti?
044200120620         exsr  sr_StampaLDV;
044300120620
044400120620       ENDSR;
044500120621
044600120621       //--------------------------------------------------------------
044700120621       //?Pulizia del PrtF.                                            ?
044800120621       //--------------------------------------------------------------
044900120621       BEGSR  sr_ClrPRTF;
045000120621
045100120621         //clear  A4prtPos;        ?(NON ha campi)?
045200120621         clear  A4prtBox;
045300120621         clear  A4prtLogo;
045400120621         clear  A4prtLogoD;
045500120621         clear  A4prtLdv;
045600120621         clear  A4prtMsg;
045700120621
045800120621         clear  $NP;
045900120621         clear  xx;
046000120621
046100120621         clear  *in;
046200120621
046300120621       ENDSR;
046400120620
046500120620       //--------------------------------------------------------------
046600120620       //?Caricamento Tabelle.                                         ?
046700120620       //--------------------------------------------------------------
046800120620       BEGSR  sr_CarTAB;
046900120620
047000120620         // -?Tab. "3A" = Codici Bolla?
047100120620         clear  ds3A;
047200120620         k_TBLcod = '3A';
047300120620         k_TBLkey = D25cbo;
047400120620         chain  %kds( k03tabel00 )  TABEL;
047500120620         if  %found(TABEL00F);
047600120620           ds3A = TBLuni;
047700120620         endif;
047800120621         //$Mamma = (§3Abcm = 'S');
047900120620
048000120620       ENDSR;
048100120620
048200120620       //--------------------------------------------------------------
048300120621       //?Caricamento dati da TITAS30C/TITAA30C/TITA430C               ?
048400120620       //--------------------------------------------------------------
048500120621       BEGSR  sr_CarTITAx;
048600120620
048700120621         // -?Apertura files?
048800120620         if  Not  %open(TITAS30C);
048900120620           open  TITAS30C;
049000120620         endif;
049100120621         if  Not  %open(TITAA30C);
049200120621           open  TITAA30C;
049300120621         endif;
049400120621         if  Not  %open(TITA430C);
049500120621           open  TITA430C;
049600120621         endif;
049700120621         if  Not  %open(FIAR531C);
049800120621           open  FIAR531C;
049900120621         endif;
050000120620
050100120621         // -?Aggancio record TITAS30C?
050200120621         clear  k05titas30;
050300120621         k_TASaas = D25aas;
050400120621         k_TASlnp = D25lnp;
050500120621         k_TASnrs = D25nrs;
050600120621         k_TASnsp = D25nsp;
050700120621         if  D25tbl <> *blank;
050800120621           k_TAStbl = D25tbl;
050900120621           chain  %kds( k05titas30 )  TITAS30C;
051000120621         else;
051100120621           chain  %kds( k05titas30 : 4 )  TITAS30C;
051200120621         endif;
051300120621
051400120620         if  NOT  %found(TITAS30C);
051500120620           leavesr;
051600120620         endif;
051700120621
051800120621         // -?Aggancio record TITAA30C?
051900120622         clear  TITAA_M_ds;
052000120622         clear  TITAA_O_ds;
052100120621         clear  k05titaa30;
052200120621         k_TAAaas = D25aas;
052300120621         k_TAAlnp = D25lnp;
052400120621         k_TAAnrs = D25nrs;
052500120621         k_TAAnsp = D25nsp;
052600120622
052700120622         k_TAAtrc = 'O';
052800120622         chain  %kds( k05titaa30 )  TITAA30C;
052900120622         if  %found(TITAA30C);
053000120622           TITAA_O_ds = TITAAds;
053100120622         endif;
053200120621
053300120621         // -?Aggancio records TITA430C (non qui!)?
053400120625         clear  dTA4A;
053500120621         clear  k05tita430;
053600120621         k_TA4aas = D25aas;
053700120621         k_TA4lnp = D25lnp;
053800120621         k_TA4nrs = D25nrs;
053900120621         k_TA4nsp = D25nsp;
054000120621         //k_TA4trc = ...;
054100120621
054200130315         // -?Aggancio record FIAR531C ("GEN" qui, "BNB" non qui!)?
054300130315         clear  dAR5gen;
054400120625         clear  dAR5bnb;
054500120621         clear  k08fiar531;
054600120621         k_AR5aas = D25aas;
054700120621         k_AR5lnp = D25lnp;
054800120621         k_AR5nrs = D25nrs;
054900120621         k_AR5nsp = D25nsp;
055000120621         //k_AR5trd = ...;
055100130315         exsr  sr_RepFIAR5_gen;
055200120620
055300120621         // -?Impostaz. campi della DS in base a quelli in TITAS/TAA/TA4?
055400120625         //                                 ?e a quelli in FIAR5?
055500120621         exsr  sr_ds_TAx;
055600120620
055700120620       ENDSR;
055800120621
055900120621       //--------------------------------------------------------------
056000120621       //?Impostaz. campi della DS in base a quelli in TITAS/TAA/TA4.  ?
056100120621       //--------------------------------------------------------------
056200120621       BEGSR  sr_ds_TAx;
056300120621
056400120621         // -?Tipo bolla?
056500120621         if  D25tbl = *blank;
056600120621           D25tbl = TAStbl;
056700120621         endif;
056800120621
056900120621         // -?Codice bolla?
057000120621         D25cbo = TAScbo;
057100120621
057200120621         // -?Codice trattamento merce?
057300120621         D25ctm = TASctm;
057400120621
057500120625         // -?Caricamento tabelle "3A" = Codici Bolla,            ?
057600120621         exsr  sr_CarTAB;
057700120621
057800120621         // -?Mese/Giorno bolla?
057900120621         D25mgs = TASmgs;
058000120621
058100120621         // -?Riferimento mittente numerico?
058200120621         D25rmn = TASrmn;
058300120621
058400120621         // -?Riferimento mittente alfanumerico?
058500120625         exsr  sr_RepTITA4_A;
058600120621         D25rma = §TA4Arma;
058700160122         D25nas = §TA4Anas;
058800120621
058900120621         // -?Linea di arrivo?
059000120621         D25lna = TASlna;
059100120621         exsr  sr_DecodLNA;
059200120621
059300120621         // -?Cliente MITTENTE?
059400120621         clear  D25ccm;
059500120621         select;
059600120621           when  TASccm <> *zero;
059700120621             D25ccm = TASccm;
059800120621           when  ( §3Atb2 <> *blank  and
059900120621                   %subst( §3Atb2 : 1 : 1 ) = 'F' )  OR
060000120621                 %subst( §3Atb1 : 1 : 1 ) = 'F';
060100120621             D25ccm = TASksc;
060200120621         endsl;
060300120622         If  %subst( %editc( TASccm : 'X' ) : 4 : 4 ) = *zero  or
060400120622             %subst( %editc( TASccm : 'X' ) : 4 : 4 ) = *all'8';
060500120622           exsr  sr_RepTITAA_M;
060600120622           D25rsm = mTAArsc;
060700120622           D25inm = mTAAind;
060800120622           D25cam = mTAAcap;
060900120622           D25lom = mTAAloc;
061000120622           D25prm = mTAAprv;
061100120622           D25nzm = mTAAnaz;
061200120622         Else;
061300120622           clear  TIBS69ds;
061400120622           //I69sif = knsif;
061500120622           I69kcc = DUTkci;
061600120622           I69kac = D25ccm;
061700120622           I69kin = D25ccm;
061800120622           tibs69r( tibs69ds :
061900120622                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
062000120622           if  O69err <> *on;
062100120622             D25rsm = %subst( ACOrag : 1 : %len(D25rsm) );
062200120622             //D25rm2 = %subst( ACOrag : %len(D25rsm) + 1 );
062300120622             D25inm = INDvia;
062400120622             D25cam = INDcae;
062500120622             D25lom = INDcit;
062600120622             D25prm = INDprv;
062700120622             D25nzm = INDsta;
062800120622           endif;
062900120622         EndIf;
063000170517
063100170517         // -?Dati della filiale del Mittente?
063200170517         clear  Og143ccm;
063300170517         clear  keyAZORG01;
063400170517         k_ORGfil = D25ccm / 10000;
063500170517         chain  %kds( keyAZORG01 )  AZORG;
063600170517         if  %found(AZORG01L)  and  ORGfva = *blank;
063700170517           Og143ccm = ORGde3;
063800170517         endif;
063900170517
064000170517         // -?Cliente Mittente DPD?
064100170517         if  Og143ccm.§CMntw = 'DPD';
064200170517           exsr  sr_Mitt_DPD;
064300170517         endif;
064400120621
064500120621         // -?Mittente originale?
064600120622         D25rmo = oTAArsc;
064700120621
064800120621         // -?Flag anteporto?
064900120621         D25fap = TASfap;
065000120621
065100120621         // -?Flag "allega documento di trasporto"?
065200120625         if  TASll1 = 'C'  or  TASll1 = 'S';
065300120625           D25bam = 'S';
065400120625         endif;
065500120621
065600120621         // -?Sigla operatore?
065700120621         D25sop = TASsop;
065800120621
065900120621         // -?Cliente DESTINATARIO?
066000120621         if  %subst( §3Atb1 : 1 : 1 ) = 'A';
066100120621           D25ccd = TASksc;
066200120621         endif;
066300120621         // ·?Se c'è 2ª bolla si imposta LNA9999?
066400120621         if  §3Atb2 <> *blank;
066500120621           D25ccd = (TASlna * 10000) + 9999;
066600120621         endif;
066700120621         D25rsd = TASrsd;
066800120621         clear  D25rd2;
066900120621         D25ind = TASind;
067000120621         D25cad = TAScad;
067100120621         D25lod = TASlod;
067200120621         D25prd = TASprd;
067300120621         D25nzd = TASnzd;
067400120621         // ·?Reperimento eventuale 2ª ragione sociale?
067500120625         exsr  sr_RepTITA4_D;
067600120621
067700120621         // -?Flag inoltro?
067800120621         D25fin = TASfin;
067900120621
068000120621         // -?Numero colli?
068100120621         D25ncl = TASncl;
068200120621         // -?Peso?
068300120621         D25pkf = TASpkf;
068400120621         // -?Volume?
068500120621         D25fvf = TASfvf;
068600120621         D25vlf = TASvlf;
068700120621
068800160122         // -?Natura merce ora in tita4 record A?
068900160122         //  D25nas = TASnas;
069000120621
069100120621         // -?Note?
069200120625         //  ?NO: in sede sono riportate solo per i clienti che l'hanno?
069300120625         //      ?richiesto.?
069400120622         clear  D25nt1;
069500120622         clear  D25nt2;
069600120625         //exsr  sr_RepTITA4_89;   ?(NO: in sede sono per pochi clienti)?
069700120621
069800120621         // -?Particolarità?
069900120621         D25gga = TASgga;
070000120621         D25gma = TASgma;
070100120621         D25gva = %subst( TASgva : 1 : 1 );
070200120621         exsr  sr_SchPARTIC;
070300120621
070400120621       ENDSR;
070500170517
070600170517       //--------------------------------------------------------------
070700170517       //?Controllo SE ragione sociale Mittente da FORZARE (TFNTC) per ?
070800170517       //?spedizione DPD.                                              ?
070900170517       //--------------------------------------------------------------
071000170517       BEGSR  sr_Mitt_DPD;
071100170517
071200170517         // -?Reperim. abilitazioni del Mittente dalla tab. "CLI"?
071300170517         clear  k05tntbe01;
071400170517         k_TBEcod = 'CLI';
071500170517         k_TBEke1 = %editc( D25ccm : 'X' );
071600170517         chain  %kds( k05tntbe01 )  TNTBE000;
071700170517         if  Not %found( TNTBE01L )  or  TBEatb <> *blank;
071800170517           leavesr;
071900170517         endif;
072000170517         dCLI = TBEuni;
072100170517
072200170517         // -?Forzatura Mittente da note "AM"?
072300170517         If  §CLItfntc = 'S';
072400170517
072500170517           if  NOT %open( TFNTC01L );
072600170517             open  TFNTC01L;
072700170517           endif;
072800170517           clear  keyTFNTC01;
072900170517           k_NTCapl = 'C';
073000170517           k_NTCnk1 = %editc( DUTkci : 'X' ) + %editc( D25ccm : 'X' );
073100170517           k_NTCtnt = 'AM';
073200170517           chain  %kds( keyTFNTC01 )  TFNTC;
073300170517           if  %found( TFNTC01L )  and  NTCsns = 'S';
073400170517             D25rsm = NTCrnt;
073500170517           endif;
073600170517
073700170517         EndIf;
073800170517
073900170517         // -?Pulizia Indirizzo Mittente?
074000170517         If  §CLInoIMtt = 'S';
074100170517
074200170517           clear  D25inm;
074300170517           clear  D25cam;
074400170517           clear  D25lom;
074500170517           clear  D25prm;
074600170517           clear  D25nzm;
074700170517
074800170517         EndIf;
074900170517
075000170517       ENDSR;
075100120621
075200120621       //--------------------------------------------------------------
075300120621       //?Decodifica Linea di Arrivo.                                  ?
075400120621       //--------------------------------------------------------------
075500120621       BEGSR  sr_DecodLNA;
075600120621
075700120621         chain  (D25lna)  AZORG;
075800120621
075900120621         if  %found(AZORG01L)  and  ORGfva = *blank;
076000120621           D25fag = ORGfag;
076100120621           D25dla = ORGdes;
076200120621           D25inl = ORGind;
076300120621           D25cal = ORGcpf;
076400120621           D25lol = ORGloc;
076500120621           D25prl = ORGpro;
076600120621           D25tel = ORGtel;
076700120621           D25fax = ORGfax;
076800120621         endif;
076900120621
077000120621       ENDSR;
077100120622
077200120622       //--------------------------------------------------------------
077300120622       //?Reperimento dati da TITAA30C x trk "M"                       ?
077400120622       //?(Indirizzo Mittente)                                         ?
077500120622       //--------------------------------------------------------------
077600120622       BEGSR  sr_RepTITAA_M;
077700120622
077800120622         //clear  TITAA_M_ds;      ?(già fatto)?
077900120622
078000120622         //clear  k05titaa30;      ?(già fatto)?
078100120622         //k_TAAaas = D25aas;      ?(già così)?
078200120622         //k_TAAlnp = D25lnp;      ?(già così)?
078300120622         //k_TAAnrs = D25nrs;      ?(già così)?
078400120622         //k_TAAnsp = D25nsp;      ?(già così)?
078500120622
078600120622         k_TAAtrc = 'M';
078700120622
078800120622         chain  %kds( k05titaa30 )  TITAA30C;
078900120622
079000120622         if  %found(TITAA30C);
079100120622           TITAA_M_ds = TITAAds;
079200120622         endif;
079300120622
079400120622       ENDSR;
079500120621
079600120621       //--------------------------------------------------------------
079700120622       //?Reperimento dati da TITA430C x trk "A"                       ?
079800120622       //?(Riferimento Mittente Alfanumerico)                          ?
079900120621       //--------------------------------------------------------------
080000120625       BEGSR  sr_RepTITA4_A;
080100120621
080200120625         //clear  dTA4A;           ?(già fatto)?
080300120621
080400120621         k_TA4trc = 'A';
080500120621         chain  %kds( k05tita430 )  TITA430C;
080600120621
080700120621         if  %found(TITA430C);
080800120621           dTA4A = TA4not;
080900120621         endif;
081000120621
081100120621       ENDSR;
081200120621
081300120621       //--------------------------------------------------------------
081400120622       //?Reperimento dati da TITA430C x trk "D"                       ?
081500120622       //?(2ª ragione sociale del destinatario)                        ?
081600120621       //--------------------------------------------------------------
081700120625       BEGSR  sr_RepTITA4_D;
081800120621
081900120621         //clear  dTA4D;           ?(NON esiste)?
082000120621         //clear  D25rd2;          ?(già fatto)?
082100120621
082200120621         k_TA4trc = 'D';
082300120621         chain  %kds( k05tita430 )  TITA430C;
082400120621
082500120621         if  %found(TITA430C);
082600120621           D25rd2 = TA4not;
082700120621         endif;
082800120621
082900120621       ENDSR;
083000120621
083100120621       //--------------------------------------------------------------
083200120622       //?Reperimento dati da TITA430C x trk "8" / "9"                 ?
083300120622       //?(Note - ma SOLO per alcuni clienti)                          ?
083400120621       //--------------------------------------------------------------
083500120625       //BEGSR  sr_RepTITA4_89;
083600120622       //
083700120622       //  //clear  D25nt1;          ?(già fatto)?
083800120622       //  //clear  D25nt2;          ?(già fatto)?
083900120622       //  //clear  dTA48;           ?(NON esiste)?
084000120622       //  //clear  dTA49;           ?(NON esiste)?
084100120622       //
084200120622       //  k_TA4trc = '8';
084300120622       //  chain  %kds( k05tita430 )  TITA430C;
084400120622       //  if  %found(TITA430C);
084500120622       //    D25nt1 = TA4not;
084600120622       //  endif;
084700120622       //
084800120622       //  k_TA4trc = '9';
084900120622       //  chain  %kds( k05tita430 )  TITA430C;
085000120622       //  if  %found(TITA430C);
085100120622       //    D25nt2 = TA4not;
085200120622       //  endif;
085300120622       //
085400120622       //ENDSR;
085500120625
085600120625       //--------------------------------------------------------------
085700120625       //?Reperimento Numero Colli Originale da FIAR5/BNB              ?
085800120625       //--------------------------------------------------------------
085900120625       BEGSR  sr_RepFIAR5_bnb;
086000120625
086100120625         //clear  dAR5bnb;         ?(già fatto)?
086200120625
086300120625         k_AR5trd = 'BNB';
086400120625         chain  %kds( k08fiar531 : 5 )  FIAR531C;
086500120625
086600120625         if  %found(FIAR531C);
086700120625           dAR5bnb = AR5uni;
086800120625         endif;
086900120625
087000120625       ENDSR;
087100130315
087200130315       //--------------------------------------------------------------
087300130315       //?Reperimento presenza di Allegati da FIAR5/GEN                ?
087400130315       //--------------------------------------------------------------
087500130315       BEGSR  sr_RepFIAR5_gen;
087600130315
087700130315         //clear  dAR5gen;         ?(già fatto)?
087800130315
087900130315         k_AR5trd = 'GEN';
088000130315         chain  %kds( k08fiar531 : 5 )  FIAR531C;
088100130315
088200130315         if  %found(FIAR531C);
088300130315           dAR5gen = AR5uni;
088400130315         endif;
088500130315
088600130315       ENDSR;
088700120621
088800120621       //--------------------------------------------------------------
088900120621       //?Inschieramento delle Note e delle Particolarità              ?
089000120621       //--------------------------------------------------------------
089100120621       BEGSR  sr_SchPARTIC;
089200120621
089300120621         //clear  $NP;             ?(già fatto)?
089400120621         //clear  XX;              ?(già fatto)?
089500120621         reset  $NoDesPartic;
089600120621
089700120621         // -?Impostazione delle eventuali note per prime?
089800120621         if  D25nt1 <> *blank;
089900120621           xx += 1;
090000120621           $NP(xx) = D25nt1;
090100120621         endif;
090200120621         if  D25nt2 <> *blank;
090300120621           xx += 1;
090400120621           $NP(xx) = D25nt2;
090500120621         endif;
090600120621
090700120621         // -?Le particolarità possono essere lunghe fino a 45 char.?
090800120621         //  ?Se i primi 30 (della 1ª riga a disposizione) non dovessero?
090900120621         //  ?bastare, il resto lo si mette nella riga successiva.?
091000120621         //  ?Se, invece, non risultano decodificate, si mettono solo i?
091100120621         //  ?codici sulla stessa riga.?
091200120621         //  ?Se uno solo risulta decodificato, esso va su una riga a?
091300120621         //  ?parte (vedi flag "$NoDesPartic": *off = serve riga nuova?
091400120621         //                                   ?*on  = ok riga procedente)?
091500120621
091600120621         // . -?Particolarità Giacenza?
091700120621         IF  D25gga <> *blank;
091800120621           wTpPar = 'Q';
091900120621           wCdPar = D25gga;
092000120621           exsr  sr_TABxPAR;
092100120621           if  TBLuni = *blank  and  $NoDesPartic = *on;
092200120621             $NP(xx) = %trim($NP(xx)) + '   ' + wCdPar;
092300120621           else;
092400120621             $NoDesPartic = (TBLuni = *blank);
092500120621             xx += 1;
092600120621             $NP(xx) = w053a;
092700120621             if  %subst( w053a : %len( $NP(xx) ) +1 ) <> *blank;
092800120621               xx +=1;
092900120621               $NP(xx) = '   ' +
093000120621                         %trim( %subst( w053a : %len( $NP(xx) ) + 1 ) );
093100120621             endif;
093200120621           endif;
093300120621         ENDIF;
093400120621
093500120621         // . -?Particolarità Consegna?
093600120621         IF  D25gma <> *blank;
093700120621           wTpPar = 'R';
093800120621           wCdPar = D25gma;
093900120621           exsr  sr_TABxPAR;
094000120621           if  TBLuni = *blank  and  $NoDesPartic = *on;
094100120621             $NP(xx) = %trim( $NP(xx) ) + '   ' + wCdPar;
094200120621           else;
094300120621             $NoDesPartic = (TBLuni = *blank);
094400120621             xx += 1;
094500120621             $NP(xx) = w053a;
094600120621             if  %subst( w053a : %len( $NP(xx) ) + 1) <> *blank;
094700120621               xx += 1;
094800120621               $NP(xx) = '   ' +
094900120621                         %trim( %subst( w053a : %len( $NP(xx) ) + 1 ) );
095000120621             endif;
095100120621           endif;
095200120621         ENDIF;
095300120621
095400120621         // . -?Particolarità Varie?
095500120621         //    ?- questa descrizione va messa nell'apposito campo PPAD?
095600120621         //      ?se è "PRIVATO"; idem se è "CONSEGNA AD UFFICI";?
095700120625         //    ?- se 'B' (bancali da rendere al mittente) è comparso un?
095800120625         //      ?apposito box (nella vera LdV);?
095900120621         //    ?- se 'O' (oltre 20 colli => bancali da riportare in mag.)?
096000120625         //      ?è comparso un apposito box, un'apposita nota e l'indica-?
096100120621         //      ?zione nella copia per il destinatario.?
096200120621         //    ?...Potrebbero anche non esserci più elementi di schiera?
096300120621         //    ?disponibili [xx >= %elem($NP)] !?
096400120621         IF  D25gva <> *blank   and
096500120621             D25gva <> 'B '     and  D25gva <> 'O '  and
096600120621             D25gva <> 'P '     and  D25gva <> 'U ';
096700120621           wTpPar = 'S';
096800120621           wCdPar = D25gva;
096900120621           exsr  sr_TABxPAR;
097000120621           if  TBLuni = *blank  and  $NoDesPartic = *on;
097100120621             $NP(xx) = %trim( $NP(xx) ) + '   ' + wCdPar;
097200120621           else;
097300120621             $NoDesPartic = (TBLuni = *blank);
097400120621             if  xx < %elem($NP);
097500120621               xx += 1;
097600120621               $NP(xx) = w053a;
097700120621               if  %subst( w053a : %len( $NP(xx) ) + 1) <> *blank  and
097800120621                   xx < %elem($NP);
097900120621                 xx += 1;
098000120621                 $NP(xx) = '   ' +
098100120621                           %trim( %subst( w053a : %len( $NP(xx) ) + 1 ) );
098200120621               endif;
098300120621             endif;
098400120621           endif;
098500120621         ENDIF;
098600120621
098700120621         // -?Imposto il numero originale di colli componenti la spediz.?
098800120621         //  ?e se ho ancora spazio lo memorizzo in "Note e Particolarità"?
098900120621         if  %subst( D25gva : 1 : 1 ) = 'O'  and  xx < %elem($NP);
099000120625           exsr  sr_RepFIAR5_bnb;
099100120621           if  §AR5bcor <> *zero;
099200120621             xx += 1;
099300120621             $NP(xx) = c_CoOr + %editc( §AR5bcor : 'K' );
099400120621           endif;
099500120621         endif;
099600120621
099700120621       ENDSR;
099800120621
099900120621       //--------------------------------------------------------------
100000120621       //?Reperimento PARTICOLARITÀ su TABEL00F                        ?
100100120621       //--------------------------------------------------------------
100200120621       BEGSR  sr_TABxPAR;
100300120621
100400120621         w053a = wCdPar;
100500120621         k_TBLcod = '7V';
100600120621         k_TBLkey = KeyPAR;
100700120621         chain  %kds (k03tabel00 )  TABEL;
100800120621
100900120621         if  %found(TABEL00F)  and  TBLflg = *blank;
101000120621           w053a = wCdPar + '-' + %trim( %subst( TBLuni : 1 : 49 ) );
101100120621         else;
101200120621           clear  TBLuni;
101300120621         endif;
101400120621
101500120621       ENDSR;
101600120620
101700120620       //--------------------------------------------------------------
101800120620       //?Preparazione dati per
101900120620       //?stampa copia LdV x scannerizzazione documenti clienti.       ?
102000120620       //--------------------------------------------------------------
102100120620       BEGSR  sr_RiempiLdV;
102200120620
102300120620         // -?Impotazione barcode: Anno+Lnp+Nrs+Nsp+Ckd?
102400120620         //  ?(richiamo del TRUL28R per ricavarne il check-digit)?
102500120620         reset  TRUL28ds;
102600120620         BARcdeAAS = %int( %subst( %editc( D25aas : 'X' ) : 3 : 2 ) );
102700120620         BARcdeLNP = D25lnp;
102800120620         BARcdeNRS = D25nrs;
102900120620         BARcdeNSP = D25nsp;
103000120620         I28cod    = BarCodeDS;
103100120620         trul28R1 ( trul28ds );
103200120620         if O28err = *blank;
103300120620           Tbcd = O28cod;
103400120620           *in50 = *on;
103500120620         endif;
103600120620
103700120620         // -?Costanti: dati BRT S.p.A.?
103800120620         TBAR = $BRT;
103900120620         TBA1 = $Intest1;
104000120620         TBA2 = $Intest2;
104100120621         //TBA3 = $Intest3;        ?NO!!!?
104200120620         TBA4 = $Intest4;
104300120620         TBA5 = $Intest5;
104400120621         //TBA6 = $Intest6;        ?NO!!!?
104500120620         TBA7 = $Intest7;
104600120620
104700120620         // -?Mittente?
104800120621         if  D25ccm <> *zero;
104900120621           Pccm = %editc( D25ccm : 'X' );
105000120620         endif;
105100120620         Prsm = D25rsm;
105200120620         Pinm = D25inm;
105300120621         Pcam = D25cam;
105400120621         Plom = D25lom;
105500120621         Pprm = D25prm;
105600120621         Pnzm = D25nzm;
105700120625
105800120621         // -?Mittente originale?
105900120621         if  D25rmo <> *blank;
106000120621           Pkmo = 'MITT. ORIGINALE';
106100120621           Prmo = D25rmo;
106200120621         endif;
106300120621
106400120621         // -?Flag "allega documento di trasporto"?
106500120621         select;
106600120621           // -?LdV con DDT allegato oltre il 1° tentativo di consegna?
106700120621           //when  D25bam = 'S'   and   w25tmc <> *blank;
106800120621           //  evalr Pddt = 'DDT sul collo';
106900120621           // -?LdV con DDT allegato (al 1° tentativo di consegna)?
107000120621           when  D25bam = 'S';
107100120621             evalr Pddt = 'DDT';
107200130315           // -?Esiste PDF abbinato?
107300130321           when  §AR5als = 'S';
107400130315             evalr Pddt = 'PACKING-LIST PDF';
107500120621           // -?Esiste packing-list abbinata?
107600130321           when  §AR5alx = 'S';
107700120621             evalr Pddt = 'PACKING-LIST';
107800120621         endsl;
107900120621
108000120621         // -?Sigla Operatore?
108100120625         //Psop = D25sop;
108200120621
108300120621         // -?Destinatario?
108400120621         if  D25ccd <> *zero;
108500120621           Pccd = %editc( D25ccd : 'X' );
108600120621         endif;
108700120621         Prsd = D25rsd;
108800120621         Prse = D25rd2;
108900120621         Pind = D25ind;
109000120621         Pcad = D25cad;
109100120621         Plod = D25lod;
109200120621         Pprd = D25prd;
109300120621         Pnzd = D25nzd;
109400120621
109500120621         // -?Colli: numero, peso, volume, flag tipo-volume?
109600120621         Pncl = D25ncl;
109700120621         Ppkf = D25pkf;
109800120621         Pvlf = D25vlf;
109900120621         if  D25fvf <> *blank;
110000120621           evalr Pfvf = D25fvf;
110100120621         endif;
110200120621
110300120621         // -?Natura merce?
110400120621         Pnat = D25nas;
110500120621
110600120621         // -?Filiale di Arrivo?
110700120621         Pkla  = 'FILIALE';
110800120621         Plna1 = %editc(D25lna : 'X')
110900120621               + ' '
111000120621               + D25dla
111100120621               + ' - '
111200120621               + D25inl
111300120621               + ' Tel.'
111400120621               + %trimr(D25tel);
111500120621         Plna2 = %editc(D25cal : 'X')
111600120621               + ' '
111700120621               + D25lol
111800120621               + ' '
111900120621               + D25prl
112000120621               + '                  '
112100120621               + ' Fax '
112200120621               + %trimr(D25fax);
112300120621
112400120621         // -?Riferimenti Mittente?
112500120621         Prmn = D25rmn;
112600120621         Prma = D25rma;
112700120621
112800120621         // -?Note (1 e 2) e Particolarità (3, 4, 5 e 6)?
112900120621         if  $NP(1) <> *blank  or  $NP(2) <> *blank  or
113000120621             $NP(3) <> *blank  or  $NP(4) <> *blank  or
113100120621             $NP(5) <> *blank  or  $NP(6) <> *blank;
113200120621           Pknp = 'NOTE  E  PARTICOLARITÀ';
113300120621           Pnp1 = $NP(1);
113400120621           Pnp2 = $NP(2);
113500120621           Pnp3 = $NP(3);
113600120621           Pnp4 = $NP(4);
113700120621           Pnp5 = $NP(5);
113800120621           Pnp6 = $NP(6);
113900120621         endif;
114000120621
114100120625         // -?Privato / Uffici?
114200120621         select;
114300170413           //*·when  D25gva = 'P ';
114400170413           //*·  evalr Ppad = 'PRIVATO';
114500120625           when  D25gva = 'U ';
114600120621             evalr Ppad = 'CONSEGNA AD UFFICI';
114700120625           //when  TASftc = 'A'  or  TAStc2 = 'A';
114800120625           //  evalr Ppad = 'APPUNTAMENTO';
114900120625           //when  D25gva = 'B ';
115000120625           //  evalr Ppad = 'BANCALI DA RENDERE AL MITTENTE';
115100120625           //when  D25gva = 'O ';
115200120625           //  evalr Ppad = 'BANCALI DA RIPORTARE A MAGAZZINO';
115300120621         endsl;
115400120621
115500120621         // -?Numero Spedizione?
115600120621         Plnp = D25lnp;
115700120621         Pnrs = D25nrs;
115800120621         Pnsp = D25nsp;
115900120621
116000120621         // -?Data Spedizione?
116100120622         //wData_EUR = %date( (D25aas * 10000) + D25mgs : *iso );
116200120622         //Pdsp = %dec(wData_EUR);
116300120622         reset  wData_EUR;
116400120622         monitor;
116500120622           wData_EUR = %date( (D25aas * 10000) + D25mgs : *iso );
116600120622           Pdsp = %dec(wData_EUR);
116700120622         on-error;
116800120622           Pdsp = D25aas;
116900120622         endmon;
117000120621
117100120621       ENDSR;
117200120620
117300120620       //--------------------------------------------------------------
117400120620       //?Stampa singola copia LdV x scannerizzaz. documenti clienti.  ?
117500120620       //--------------------------------------------------------------
117600120620       BEGSR  sr_StampaLdV;
117700120620
117800120620         // -?Posizionamento in testa al foglio A4?
117900120620         exsr  sr_Posiz_Box1;
118000120620
118100120620         // -?Copia LdV x scanerizzazione documenti clienti?
118200120620         write  A4prtLDV;
118300120620         write  A4prtBOX;
118400120620
118500120620         // -?Simil-Logo BRT?
118600120620         write  A4prtLOGO;
118700120620         write  A4prtLOGOd;
118800120620
118900120620       ENDSR;
119000120620
119100120620       //--------------------------------------------------------------
119200120620       //?Posizionamento del box all'interno della pag. A4.            ?
119300120620       //--------------------------------------------------------------
119400120620       BEGSR  sr_Posiz_Box1;
119500120620
119600120620         // -?Si salta alla pag. A4 successiva: la LdV in esame?
119700120620         //  ?dev'essere stampata in posizione iniziale dell'A4.?
119800120620         wSeqLdV = 1;
119900120620
120000120620         // -?Impostaz. delle posizioni nella pagina in corso di stampa?
120100120620         wPosIni = (c_Len1LdV * (wSeqLdV - 1));
120200120620
120300120620         // -?Posizionamento nel foglio A4 in base al n° LdV in stampa?
120400120620         WRITE  A4prtPOS;
120500120620
120600120620         // -?Posizionamenti nella copia per il vettore?
120700120620
120800120620         // -?Box (& Logo & BarCode)?
120900120620         §Box1As = wPosIni + c_PosBox1As;
121000120620         §Box1Ai = wPosIni + c_PosBox1Bs + c_LineNarrow;
121100120620         §Box1Bs = wPosIni + c_PosBox1Bs;
121200120620         §Box1Bi = wPosIni + c_PosBox1Bi;
121300120620
121400120620         // -?Simil-Logo BRT?
121500120620         §PlblS  = §Box1As;
121600120620         §PlblVS = 7.581;
121700120620         exsr  sr_SimilLogoBRT_;
121800120620
121900120620         // -?Costanti dati anagrafici BRT?
122000120620         §PC0    = wPosIni + c_PosTBar;
122100120620         §PC1    = wPosIni + c_PosTBa1;
122200120620         §PC2    = wPosIni + c_PosTBa2x;
122300120620         §PC3    = wPosIni + c_PosTBa3x;
122400120620         §PC4    = wPosIni + c_PosTBa4x;
122500120620         §PC5    = wPosIni + c_PosTBa5x;
122600120620         §PC6    = wPosIni + c_PosTBa6x;
122700120620
122800120620         // -?Linee orizzontali nel box?
122900120620         §Lo1S1  = wPosIni + c_PosLin1oS1;
123000120620         §Lo1S2  = wPosIni + c_PosLin1oS2;
123100120620       //§Lo1S3  = wPosIni + c_PosLin1oS3;
123200120620         §Lo1S4  = wPosIni + c_PosLin1oS4;
123300120620         §Lo1S5  = wPosIni + c_PosLin1OS5;
123400120620         //§Lo1C1  = wPosIni + c_PosLin1oC1;
123500120620         §Lo1C2  = wPosIni + c_PosLin1oC2;
123600120620         §Lo1D1  = wPosIni + c_PosLin1oD1;
123700120620
123800120620         // -?Lunghezza linee verticali nel box in copia per il vett.?
123900120620         §LLv1A  = §Lo1S5  - §Lo1S1;
124000120620         §LLv1B  = §Lo1S4  - §Box1Bs;
124100120620         §LLv1C  = §Box1Bi - §Box1Bs;
124200120620
124300120620       ENDSR;
124400120620
124500120620      /end-free
124600120620
124700120620       //-------------------------------------------------------------
124800120620       //?Posizionamento del simil-logo BRT (unico)                   ?
124900120620       //-------------------------------------------------------------
125000120620      /copy gaitrasrc/srcTNSY,LogoBRT_R2
125100120620
125200120620      /free
125300120620
125400120620       //--------------------------------------------------------------
125500120620       //?Operazioni finali.                                           ?
125600120620       //--------------------------------------------------------------
125700120620       BEGSR  sr_RoutEnd;
125800120620
125900120620         // -?Chiusura archivi / applicazioni?
126000120620         If  D25tla <> *blank;
126100120620
126200120620           *inLR = *on;
126300120622
126400120622           // -?Chiusura funzioni precedentemente aperte?
126500120622           reset  TIBS69ds;
126600120622           //I69tla = 'C';         ?(già così)?
126700120622           tibs69r( tibs69ds :
126800120622                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
126900120620
127000120620           // -?Chiusura file di stampa?
127100120620           close  TNSB25P;
127200120621
127300120621           // -?Chiusura files?
127400120621           if  %open(TITAS30C);
127500120621             close  TITAS30C;
127600120621           endif;
127700120621           if  %open(TITAA30C);
127800120621             close  TITAA30C;
127900120621           endif;
128000120621           if  %open(TITA430C);
128100120621             close  TITA430C;
128200120621           endif;
128300120621           if  %open(FIAR531C);
128400120621             close  FIAR531C;
128500120621           endif;
128600170517           if  %open(TFNTC01L);
128700170517             close  TFNTC01L;
128800170517           endif;
128900120620
129000120620         EndIf;
129100120620
129200120620         // -?Ritorno al chiamante?
129300120620         return;
129400120620
129500120620       ENDSR;
129600120620
129700120620      /end-free
129800120620
129900120620       //--------------------------------------------------------------
130000120620       //?Definizione schiere a tempo di compilazione                  ?
130100120620       //--------------------------------------------------------------
130200120620
130300120620** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
130400120620IMPOSSIBILE CONTINUARE LA RISTAMPA DELLE SIMIL-LDV
130500120620PERCHE' NE SONO GIA' STATE STAMPATE PIU' DI &NrLdV !!!
