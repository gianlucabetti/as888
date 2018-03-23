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
023700170717
023800170717       // -?Parametri per FNLV55R (reperimento terminal arr/par/£6)?
023900170717     d FNLV55ds      e ds                  inz
024000170717     d   D55tpt      e                     inz('6')
024100170717      /copy gaitrasrc/srcProtoPR,FNLV55R
024200120620
024300120620       // -?Parametri per TRUL28R (calcolo check digit del barcode)?
024400120620     d TRUL28ds      e ds                  inz
024500120702     d   I28tla      e                     inz('L')
024600120702     d   I28mod      e                     inz('BAR')
024700120620       // -?Calcolo check-digit per bar-code?
024800120620     d trul28r1        pr                  extpgm('TRUL28R1')
024900120620     d   trul28ds...
025000120620     d                                     likeds(TRUL28ds)
025100120622
025200120622       // -?Controllo/Decodifica cliente?
025300120622      /copy gaitrasrc/srcProtoPI,TIBS69R
025400120622      /copy gaitrasrc/srcProtoPR,TIBS69R
025500120620
025600120620       //--------------------------------------------------------------
025700120620       //?Definizione key-list.                                        ?
025800120620       //--------------------------------------------------------------
025900120620
026000120620       // -?File TABEL00F?
026100120620     d k03tabel00    e ds                  extname(TABEL00F : *key)
026200120620     d                                     prefix(k_)   inz
026300120620
026400120620       // -?File TNTBE01L?
026500120620     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
026600120620     d                                     prefix(k_)   inz
026700120620
026800120620       // -?File TITAS30C?
026900120620     d k05titas30    e ds                  extname(TITAS30C : *key)
027000120620     d                                     prefix(k_)   inz
027100120621
027200120621       // -?File TITAA30C?
027300120621     d k05titaa30    e ds                  extname(TITAA30C : *key)
027400120621     d                                     prefix(k_)   inz
027500120621
027600120621       // -?File TITA430C?
027700120621     d k05tita430    e ds                  extname(TITA430C : *key)
027800120621     d                                     prefix(k_)   inz
027900120621
028000120621       // -?File FIAR531C?
028100120621     d k08fiar531    e ds                  extname(FIAR531C : *key)
028200120621     d                                     prefix(k_)   inz
028300170517
028400170517       // -?File AZORG01L?
028500170517     d keyAZORG01    e ds                  extname(AZORG01L : *key)
028600170517     d                                     prefix(k_)   inz
028700170517
028800170517       // -?File TFNTC01L?
028900170517     d keyTFNTC01    e ds                  extname(TFNTC01L : *key)
029000170517     d                                     prefix(k_)   inz
029100120620
029200120620       //--------------------------------------------------------------
029300120620       //?M A I N - L I N E                                            ?
029400120620       //--------------------------------------------------------------
029500120620
029600120620     c     *Entry        plist
029700120620     c                   parm                    TNSB25ds
029800120620
029900120620      /free
030000120620
030100120620       // -?Operazioni iniziali?
030200120620       exsr  sr_RoutInz;
030300120620
030400120620       // -?Stampa?
030500120620       exsr  sr_Stampa;
030600120620
030700120620       // -?Operazioni finali?
030800120620       exsr  sr_RoutEnd;
030900120620
031000120620
031100120620       //--------------------------------------------------------------
031200120620       //?Operazioni iniziali.                                         ?
031300120620       //--------------------------------------------------------------
031400120620       BEGSR  *InzSR;
031500120620
031600120620         // -?Verifica del sistema AS/400 corrente?
031700120620         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
031800120620           exsr  sr_RoutEnd;
031900120620         endif;
032000120620
032100120620         // -?Impostazione libreria per file TITAS30C?
032200120620         //  ?(che verrà aperto SOLO se richiesto dal chiamante)?
032300120620         if  %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
032400120620           wLibTITAS = 'GAITRAGRU/TITAS30C';
032500120621           wLibTITAA = 'GAITRAGRU/TITAA30C';
032600120621           wLibTITA4 = 'GAITRAGRU/TITA430C';
032700120621           wLibFIAR5 = 'GAITRAGRU/FIAR531C';
032800120620         else;
032900120620           wLibTITAS = 'GAITRAGRPS/TITAS30C';
033000120621           wLibTITAA = 'GAITRAGRPS/TITAA30C';
033100120621           wLibTITA4 = 'GAITRAGRPS/TITA430C';
033200120621           wLibFIAR5 = 'GAITRAGRPS/FIAR531C';
033300120620         endif;
033400120620
033500120620         // -?Reperimento dati job?
033600120620         exsr  sr_DatiJob;
033700120622
033800120622         // -?Impostazione campi fissi?
033900120622         k_TBLkut = c_Kut;
034000120620
034100120620         // -?Reperimento n° max di LdV stampabili da tab. "VPO"?
034200120620         exsr  sr_RepTabVPO;
034300120620
034400120620         // -?Apertura del PRTF?
034500120620         open  TNSB25P;
034600120620
034700120620       ENDSR;
034800120620
034900120620       //--------------------------------------------------------------
035000120620
035100120620       BEGSR  sr_RoutInz;
035200120620
035300120620         // -?SE richiesta la sola chiusura del *pgm => uscita?
035400120620         if  D25tla = 'C';
035500120620           exsr  sr_RoutEnd;
035600120620         endif;
035700120620
035800120620         // -?Richiesta la RI-apertura del file di stampa?
035900120620         if  D25fl1 = *on;
036000120621           close  TNSB25P;
036100120621           open   TNSB25P;
036200120620         endif;
036300120620
036400120620       ENDSR;
036500120620
036600120620       //--------------------------------------------------------------
036700120620       //?Reperimento Dati del job (Utente/Operativi).                 ?
036800120620       //--------------------------------------------------------------
036900120620       BEGSR  sr_DatiJob;
037000120620
037100120620         in(E) §AzUte;
037200120620         if NOT %error;
037300120620           in(E) §DatiUte;
037400120620         endif;
037500120620         if %error or RSut = *blanks;
037600120620           clear TIBS34ds;
037700120620           tibs34r ( tibs34ds );
037800120620           in §AzUte;
037900120620           in §DatiUte;
038000120620         endif;
038100120620
038200120620       ENDSR;
038300120620
038400120620       //--------------------------------------------------------------
038500120620       //?Reperimento n° max di LsV stampabili da tab. "VPO"           ?
038600120620       //--------------------------------------------------------------
038700120620       BEGSR  sr_RepTabVPO;
038800120620
038900120620         // -?Aggancio tabella "VPO" per reperimento del n° LdV stampabili?
039000120620         //  ?limite (per congelamento e blocco stampa)?
039100120620         clear  dVPO;
039200170517         clear  k05tntbe01;
039300120620         k_TBEcod = 'VPO';
039400120620         k_TBEke1 = 'VPO';
039500120620         k_TBEke2 = *blank;
039600120620         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
039700120620         if  %found(TNTBE01L)  and  TBEatb = *blank;
039800120621           dVPO = TBEuni;
039900120620         endif;
040000120620         if  §VPOMaxLdv = *zero;
040100120620           §VPOMaxLdv = 10000;
040200120620         endif;
040300120620
040400120620       ENDSR;
040500120620
040600120620       //--------------------------------------------------------------
040700120620       //?Stampa copia LdV x scannerizzazione documenti clienti.       ?
040800120620       //--------------------------------------------------------------
040900120620       BEGSR  sr_Stampa;
041000120620
041100120620         // -?Pulizia del PRTF?
041200120620         exsr  sr_ClrPRTF;
041300120620
041400120620         Select;
041500120620
041600120620           // -?Passati i dati => è già (quasi) tutto pronto?
041700120620           When  D25ges = *blank;
041800120620             exsr  sr_CarTAB;
041900120621             if  D25dla = *blank;
042000120621               exsr  sr_DecodLNA;
042100120621             endif;
042200120620
042300120621           // -?Richiesto il reperimento dei dati da TITAS/TITAA/TITA4?
042400120621           //                                  ?e da FIAR5?
042500120620           When  D25ges = 'T';
042600120621             exsr  sr_CarTITAx;
042700120620
042800120621           // -?D25GES = "R" => Stampa una LDV vuota con un messaggio?
042900120620           //  ?per avvisare che è impossibile continuare la ristampa?
043000120620           //  ?perchè stampate più di 10.000 LdV !?
043100120620           When  D25ges = 'R';
043200120620             Pmsg1  = $Msg(1);
043300120620             Pmsg2  = %replace( %editc( §VPOmaxldv : 'J' ) :
043400120620                                        $Msg(2) : %scan(
043500120620                                        '&NrLdV' : $Msg(2) ) );
043600120620             exsr  sr_Posiz_Box1;
043700120620             write  A4prtMSG;
043800120620             D25tla = 'L';
043900120620             exsr  sr_RoutEnd;
044000120620
044100120620         EndSl;
044200120620
044300120620         // -?Preparazione dati per stampa bolla?
044400120620         exsr  sr_RiempiLDV;
044500120620
044600120620         // -?Stampa copia LdV x scannerizzazione documenti clienti?
044700120620         exsr  sr_StampaLDV;
044800120620
044900120620       ENDSR;
045000120621
045100120621       //--------------------------------------------------------------
045200120621       //?Pulizia del PrtF.                                            ?
045300120621       //--------------------------------------------------------------
045400120621       BEGSR  sr_ClrPRTF;
045500120621
045600120621         //clear  A4prtPos;        ?(NON ha campi)?
045700120621         clear  A4prtBox;
045800120621         clear  A4prtLogo;
045900120621         clear  A4prtLogoD;
046000120621         clear  A4prtLdv;
046100120621         clear  A4prtMsg;
046200120621
046300120621         clear  $NP;
046400120621         clear  xx;
046500120621
046600120621         clear  *in;
046700120621
046800120621       ENDSR;
046900120620
047000120620       //--------------------------------------------------------------
047100120620       //?Caricamento Tabelle.                                         ?
047200120620       //--------------------------------------------------------------
047300120620       BEGSR  sr_CarTAB;
047400120620
047500120620         // -?Tab. "3A" = Codici Bolla?
047600120620         clear  ds3A;
047700120620         k_TBLcod = '3A';
047800120620         k_TBLkey = D25cbo;
047900120620         chain  %kds( k03tabel00 )  TABEL;
048000120620         if  %found(TABEL00F);
048100120620           ds3A = TBLuni;
048200120620         endif;
048300120621         //$Mamma = (§3Abcm = 'S');
048400120620
048500120620       ENDSR;
048600120620
048700120620       //--------------------------------------------------------------
048800120621       //?Caricamento dati da TITAS30C/TITAA30C/TITA430C               ?
048900120620       //--------------------------------------------------------------
049000120621       BEGSR  sr_CarTITAx;
049100120620
049200120621         // -?Apertura files?
049300120620         if  Not  %open(TITAS30C);
049400120620           open  TITAS30C;
049500120620         endif;
049600120621         if  Not  %open(TITAA30C);
049700120621           open  TITAA30C;
049800120621         endif;
049900120621         if  Not  %open(TITA430C);
050000120621           open  TITA430C;
050100120621         endif;
050200120621         if  Not  %open(FIAR531C);
050300120621           open  FIAR531C;
050400120621         endif;
050500120620
050600120621         // -?Aggancio record TITAS30C?
050700120621         clear  k05titas30;
050800120621         k_TASaas = D25aas;
050900120621         k_TASlnp = D25lnp;
051000120621         k_TASnrs = D25nrs;
051100120621         k_TASnsp = D25nsp;
051200120621         if  D25tbl <> *blank;
051300120621           k_TAStbl = D25tbl;
051400120621           chain  %kds( k05titas30 )  TITAS30C;
051500120621         else;
051600120621           chain  %kds( k05titas30 : 4 )  TITAS30C;
051700120621         endif;
051800120621
051900120620         if  NOT  %found(TITAS30C);
052000120620           leavesr;
052100120620         endif;
052200120621
052300120621         // -?Aggancio record TITAA30C?
052400120622         clear  TITAA_M_ds;
052500120622         clear  TITAA_O_ds;
052600120621         clear  k05titaa30;
052700120621         k_TAAaas = D25aas;
052800120621         k_TAAlnp = D25lnp;
052900120621         k_TAAnrs = D25nrs;
053000120621         k_TAAnsp = D25nsp;
053100120622
053200120622         k_TAAtrc = 'O';
053300120622         chain  %kds( k05titaa30 )  TITAA30C;
053400120622         if  %found(TITAA30C);
053500120622           TITAA_O_ds = TITAAds;
053600120622         endif;
053700120621
053800120621         // -?Aggancio records TITA430C (non qui!)?
053900120625         clear  dTA4A;
054000120621         clear  k05tita430;
054100120621         k_TA4aas = D25aas;
054200120621         k_TA4lnp = D25lnp;
054300120621         k_TA4nrs = D25nrs;
054400120621         k_TA4nsp = D25nsp;
054500120621         //k_TA4trc = ...;
054600120621
054700130315         // -?Aggancio record FIAR531C ("GEN" qui, "BNB" non qui!)?
054800130315         clear  dAR5gen;
054900120625         clear  dAR5bnb;
055000120621         clear  k08fiar531;
055100120621         k_AR5aas = D25aas;
055200120621         k_AR5lnp = D25lnp;
055300120621         k_AR5nrs = D25nrs;
055400120621         k_AR5nsp = D25nsp;
055500120621         //k_AR5trd = ...;
055600130315         exsr  sr_RepFIAR5_gen;
055700120620
055800120621         // -?Impostaz. campi della DS in base a quelli in TITAS/TAA/TA4?
055900120625         //                                 ?e a quelli in FIAR5?
056000120621         exsr  sr_ds_TAx;
056100120620
056200120620       ENDSR;
056300120621
056400120621       //--------------------------------------------------------------
056500120621       //?Impostaz. campi della DS in base a quelli in TITAS/TAA/TA4.  ?
056600120621       //--------------------------------------------------------------
056700120621       BEGSR  sr_ds_TAx;
056800120621
056900120621         // -?Tipo bolla?
057000120621         if  D25tbl = *blank;
057100120621           D25tbl = TAStbl;
057200120621         endif;
057300120621
057400120621         // -?Codice bolla?
057500120621         D25cbo = TAScbo;
057600120621
057700120621         // -?Codice trattamento merce?
057800120621         D25ctm = TASctm;
057900120621
058000120625         // -?Caricamento tabelle "3A" = Codici Bolla,            ?
058100120621         exsr  sr_CarTAB;
058200120621
058300120621         // -?Mese/Giorno bolla?
058400120621         D25mgs = TASmgs;
058500120621
058600120621         // -?Riferimento mittente numerico?
058700120621         D25rmn = TASrmn;
058800120621
058900120621         // -?Riferimento mittente alfanumerico?
059000120625         exsr  sr_RepTITA4_A;
059100120621         D25rma = §TA4Arma;
059200160122         D25nas = §TA4Anas;
059300120621
059400120621         // -?Linea di arrivo?
059500120621         D25lna = TASlna;
059600120621         exsr  sr_DecodLNA;
059700120621
059800120621         // -?Cliente MITTENTE?
059900120621         clear  D25ccm;
060000120621         select;
060100120621           when  TASccm <> *zero;
060200120621             D25ccm = TASccm;
060300120621           when  ( §3Atb2 <> *blank  and
060400120621                   %subst( §3Atb2 : 1 : 1 ) = 'F' )  OR
060500120621                 %subst( §3Atb1 : 1 : 1 ) = 'F';
060600120621             D25ccm = TASksc;
060700120621         endsl;
060800120622         If  %subst( %editc( TASccm : 'X' ) : 4 : 4 ) = *zero  or
060900120622             %subst( %editc( TASccm : 'X' ) : 4 : 4 ) = *all'8';
061000120622           exsr  sr_RepTITAA_M;
061100120622           D25rsm = mTAArsc;
061200120622           D25inm = mTAAind;
061300120622           D25cam = mTAAcap;
061400120622           D25lom = mTAAloc;
061500120622           D25prm = mTAAprv;
061600120622           D25nzm = mTAAnaz;
061700120622         Else;
061800120622           clear  TIBS69ds;
061900120622           //I69sif = knsif;
062000120622           I69kcc = DUTkci;
062100120622           I69kac = D25ccm;
062200120622           I69kin = D25ccm;
062300120622           tibs69r( tibs69ds :
062400120622                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
062500120622           if  O69err <> *on;
062600120622             D25rsm = %subst( ACOrag : 1 : %len(D25rsm) );
062700120622             //D25rm2 = %subst( ACOrag : %len(D25rsm) + 1 );
062800120622             D25inm = INDvia;
062900120622             D25cam = INDcae;
063000120622             D25lom = INDcit;
063100120622             D25prm = INDprv;
063200120622             D25nzm = INDsta;
063300120622           endif;
063400120622         EndIf;
063500170517
063600170517         // -?Dati della filiale del Mittente?
063700170517         clear  Og143ccm;
063800170517         clear  keyAZORG01;
063900170517         k_ORGfil = D25ccm / 10000;
064000170517         chain  %kds( keyAZORG01 )  AZORG;
064100170517         if  %found(AZORG01L)  and  ORGfva = *blank;
064200170517           Og143ccm = ORGde3;
064300170517         endif;
064400170517
064500170517         // -?Cliente Mittente DPD?
064600170517         if  Og143ccm.§CMntw = 'DPD';
064700170517           exsr  sr_Mitt_DPD;
064800170517         endif;
064900120621
065000120621         // -?Mittente originale?
065100120622         D25rmo = oTAArsc;
065200120621
065300120621         // -?Flag anteporto?
065400120621         D25fap = TASfap;
065500120621
065600120621         // -?Flag "allega documento di trasporto"?
065700120625         if  TASll1 = 'C'  or  TASll1 = 'S';
065800120625           D25bam = 'S';
065900120625         endif;
066000120621
066100120621         // -?Sigla operatore?
066200120621         D25sop = TASsop;
066300120621
066400120621         // -?Cliente DESTINATARIO?
066500120621         if  %subst( §3Atb1 : 1 : 1 ) = 'A';
066600120621           D25ccd = TASksc;
066700120621         endif;
066800120621         // ·?Se c'è 2ª bolla si imposta LNA9999?
066900120621         if  §3Atb2 <> *blank;
067000120621           D25ccd = (TASlna * 10000) + 9999;
067100120621         endif;
067200120621         D25rsd = TASrsd;
067300120621         clear  D25rd2;
067400120621         D25ind = TASind;
067500120621         D25cad = TAScad;
067600120621         D25lod = TASlod;
067700120621         D25prd = TASprd;
067800120621         D25nzd = TASnzd;
067900120621         // ·?Reperimento eventuale 2ª ragione sociale?
068000120625         exsr  sr_RepTITA4_D;
068100120621
068200120621         // -?Flag inoltro?
068300120621         D25fin = TASfin;
068400120621
068500120621         // -?Numero colli?
068600120621         D25ncl = TASncl;
068700120621         // -?Peso?
068800120621         D25pkf = TASpkf;
068900120621         // -?Volume?
069000120621         D25fvf = TASfvf;
069100120621         D25vlf = TASvlf;
069200120621
069300160122         // -?Natura merce ora in tita4 record A?
069400160122         //  D25nas = TASnas;
069500120621
069600120621         // -?Note?
069700120625         //  ?NO: in sede sono riportate solo per i clienti che l'hanno?
069800120625         //      ?richiesto.?
069900120622         clear  D25nt1;
070000120622         clear  D25nt2;
070100120625         //exsr  sr_RepTITA4_89;   ?(NO: in sede sono per pochi clienti)?
070200120621
070300120621         // -?Particolarità?
070400120621         D25gga = TASgga;
070500120621         D25gma = TASgma;
070600120621         D25gva = %subst( TASgva : 1 : 1 );
070700120621         exsr  sr_SchPARTIC;
070800120621
070900120621       ENDSR;
071000170517
071100170517       //--------------------------------------------------------------
071200170517       //?Controllo SE ragione sociale Mittente da FORZARE (TFNTC) per ?
071300170517       //?spedizione DPD.                                              ?
071400170517       //--------------------------------------------------------------
071500170517       BEGSR  sr_Mitt_DPD;
071600170517
071700170517         // -?Reperim. abilitazioni del Mittente dalla tab. "CLI"?
071800170517         clear  k05tntbe01;
071900170517         k_TBEcod = 'CLI';
072000170517         k_TBEke1 = %editc( D25ccm : 'X' );
072100170517         chain  %kds( k05tntbe01 )  TNTBE000;
072200170517         if  Not %found( TNTBE01L )  or  TBEatb <> *blank;
072300170517           leavesr;
072400170517         endif;
072500170517         dCLI = TBEuni;
072600170517
072700170517         // -?Forzatura Mittente da note "AM"?
072800170517         If  §CLItfntc = 'S';
072900170517
073000170517           if  NOT %open( TFNTC01L );
073100170517             open  TFNTC01L;
073200170517           endif;
073300170517           clear  keyTFNTC01;
073400170517           k_NTCapl = 'C';
073500170517           k_NTCnk1 = %editc( DUTkci : 'X' ) + %editc( D25ccm : 'X' );
073600170517           k_NTCtnt = 'AM';
073700170517           chain  %kds( keyTFNTC01 )  TFNTC;
073800170517           if  %found( TFNTC01L )  and  NTCsns = 'S';
073900170517             D25rsm = NTCrnt;
074000170517           endif;
074100170517
074200170517         EndIf;
074300170517
074400170517         // -?Pulizia Indirizzo Mittente?
074500170517         If  §CLInoIMtt = 'S';
074600170517
074700170517           clear  D25inm;
074800170517           clear  D25cam;
074900170517           clear  D25lom;
075000170517           clear  D25prm;
075100170517           clear  D25nzm;
075200170517
075300170517         EndIf;
075400170517
075500170517       ENDSR;
075600120621
075700120621       //--------------------------------------------------------------
075800120621       //?Decodifica Linea di Arrivo.                                  ?
075900120621       //--------------------------------------------------------------
076000120621       BEGSR  sr_DecodLNA;
076100170717
076200170717         // -?Reperimento capofila (£6)?
076300170717         if  D25lna <> D55lin;
076400170717           reset  FNLV55ds;
076500170717           //*·D55tpt =  '6'        //?(già così)?
076600170717           D55lin =  D25lna;
076700170905           //*·D55drf =  ( D25aas * 10000 ) + D25mgs;
076800170905           D55drf = %int( %subst( %char( %dec( %timestamp() ) )
076900170905                                  : 1 : 8 ) );
077000170717           FNLV55R ( FNLV55ds );
077100170717           if  D55err <> *blank;
077200170717             D55tfa =  D25lna;
077300170717           endif;
077400170717         endif;
077500170717         D25lna = D55tfa;
077600170717
077700170717         // -?Decodifica capofila (£6) della LNA?
077800120621         chain  (D25lna)  AZORG;
077900120621
078000120621         if  %found(AZORG01L)  and  ORGfva = *blank;
078100120621           D25fag = ORGfag;
078200120621           D25dla = ORGdes;
078300120621           D25inl = ORGind;
078400120621           D25cal = ORGcpf;
078500120621           D25lol = ORGloc;
078600120621           D25prl = ORGpro;
078700120621           D25tel = ORGtel;
078800120621           D25fax = ORGfax;
078900120621         endif;
079000120621
079100120621       ENDSR;
079200120622
079300120622       //--------------------------------------------------------------
079400120622       //?Reperimento dati da TITAA30C x trk "M"                       ?
079500120622       //?(Indirizzo Mittente)                                         ?
079600120622       //--------------------------------------------------------------
079700120622       BEGSR  sr_RepTITAA_M;
079800120622
079900120622         //clear  TITAA_M_ds;      ?(già fatto)?
080000120622
080100120622         //clear  k05titaa30;      ?(già fatto)?
080200120622         //k_TAAaas = D25aas;      ?(già così)?
080300120622         //k_TAAlnp = D25lnp;      ?(già così)?
080400120622         //k_TAAnrs = D25nrs;      ?(già così)?
080500120622         //k_TAAnsp = D25nsp;      ?(già così)?
080600120622
080700120622         k_TAAtrc = 'M';
080800120622
080900120622         chain  %kds( k05titaa30 )  TITAA30C;
081000120622
081100120622         if  %found(TITAA30C);
081200120622           TITAA_M_ds = TITAAds;
081300120622         endif;
081400120622
081500120622       ENDSR;
081600120621
081700120621       //--------------------------------------------------------------
081800120622       //?Reperimento dati da TITA430C x trk "A"                       ?
081900120622       //?(Riferimento Mittente Alfanumerico)                          ?
082000120621       //--------------------------------------------------------------
082100120625       BEGSR  sr_RepTITA4_A;
082200120621
082300120625         //clear  dTA4A;           ?(già fatto)?
082400120621
082500120621         k_TA4trc = 'A';
082600120621         chain  %kds( k05tita430 )  TITA430C;
082700120621
082800120621         if  %found(TITA430C);
082900120621           dTA4A = TA4not;
083000120621         endif;
083100120621
083200120621       ENDSR;
083300120621
083400120621       //--------------------------------------------------------------
083500120622       //?Reperimento dati da TITA430C x trk "D"                       ?
083600120622       //?(2ª ragione sociale del destinatario)                        ?
083700120621       //--------------------------------------------------------------
083800120625       BEGSR  sr_RepTITA4_D;
083900120621
084000120621         //clear  dTA4D;           ?(NON esiste)?
084100120621         //clear  D25rd2;          ?(già fatto)?
084200120621
084300120621         k_TA4trc = 'D';
084400120621         chain  %kds( k05tita430 )  TITA430C;
084500120621
084600120621         if  %found(TITA430C);
084700120621           D25rd2 = TA4not;
084800120621         endif;
084900120621
085000120621       ENDSR;
085100120621
085200120621       //--------------------------------------------------------------
085300120622       //?Reperimento dati da TITA430C x trk "8" / "9"                 ?
085400120622       //?(Note - ma SOLO per alcuni clienti)                          ?
085500120621       //--------------------------------------------------------------
085600120625       //BEGSR  sr_RepTITA4_89;
085700120622       //
085800120622       //  //clear  D25nt1;          ?(già fatto)?
085900120622       //  //clear  D25nt2;          ?(già fatto)?
086000120622       //  //clear  dTA48;           ?(NON esiste)?
086100120622       //  //clear  dTA49;           ?(NON esiste)?
086200120622       //
086300120622       //  k_TA4trc = '8';
086400120622       //  chain  %kds( k05tita430 )  TITA430C;
086500120622       //  if  %found(TITA430C);
086600120622       //    D25nt1 = TA4not;
086700120622       //  endif;
086800120622       //
086900120622       //  k_TA4trc = '9';
087000120622       //  chain  %kds( k05tita430 )  TITA430C;
087100120622       //  if  %found(TITA430C);
087200120622       //    D25nt2 = TA4not;
087300120622       //  endif;
087400120622       //
087500120622       //ENDSR;
087600120625
087700120625       //--------------------------------------------------------------
087800120625       //?Reperimento Numero Colli Originale da FIAR5/BNB              ?
087900120625       //--------------------------------------------------------------
088000120625       BEGSR  sr_RepFIAR5_bnb;
088100120625
088200120625         //clear  dAR5bnb;         ?(già fatto)?
088300120625
088400120625         k_AR5trd = 'BNB';
088500120625         chain  %kds( k08fiar531 : 5 )  FIAR531C;
088600120625
088700120625         if  %found(FIAR531C);
088800120625           dAR5bnb = AR5uni;
088900120625         endif;
089000120625
089100120625       ENDSR;
089200130315
089300130315       //--------------------------------------------------------------
089400130315       //?Reperimento presenza di Allegati da FIAR5/GEN                ?
089500130315       //--------------------------------------------------------------
089600130315       BEGSR  sr_RepFIAR5_gen;
089700130315
089800130315         //clear  dAR5gen;         ?(già fatto)?
089900130315
090000130315         k_AR5trd = 'GEN';
090100130315         chain  %kds( k08fiar531 : 5 )  FIAR531C;
090200130315
090300130315         if  %found(FIAR531C);
090400130315           dAR5gen = AR5uni;
090500130315         endif;
090600130315
090700130315       ENDSR;
090800120621
090900120621       //--------------------------------------------------------------
091000120621       //?Inschieramento delle Note e delle Particolarità              ?
091100120621       //--------------------------------------------------------------
091200120621       BEGSR  sr_SchPARTIC;
091300120621
091400120621         //clear  $NP;             ?(già fatto)?
091500120621         //clear  XX;              ?(già fatto)?
091600120621         reset  $NoDesPartic;
091700120621
091800120621         // -?Impostazione delle eventuali note per prime?
091900120621         if  D25nt1 <> *blank;
092000120621           xx += 1;
092100120621           $NP(xx) = D25nt1;
092200120621         endif;
092300120621         if  D25nt2 <> *blank;
092400120621           xx += 1;
092500120621           $NP(xx) = D25nt2;
092600120621         endif;
092700120621
092800120621         // -?Le particolarità possono essere lunghe fino a 45 char.?
092900120621         //  ?Se i primi 30 (della 1ª riga a disposizione) non dovessero?
093000120621         //  ?bastare, il resto lo si mette nella riga successiva.?
093100120621         //  ?Se, invece, non risultano decodificate, si mettono solo i?
093200120621         //  ?codici sulla stessa riga.?
093300120621         //  ?Se uno solo risulta decodificato, esso va su una riga a?
093400120621         //  ?parte (vedi flag "$NoDesPartic": *off = serve riga nuova?
093500120621         //                                   ?*on  = ok riga procedente)?
093600120621
093700120621         // . -?Particolarità Giacenza?
093800120621         IF  D25gga <> *blank;
093900120621           wTpPar = 'Q';
094000120621           wCdPar = D25gga;
094100120621           exsr  sr_TABxPAR;
094200120621           if  TBLuni = *blank  and  $NoDesPartic = *on;
094300120621             $NP(xx) = %trim($NP(xx)) + '   ' + wCdPar;
094400120621           else;
094500120621             $NoDesPartic = (TBLuni = *blank);
094600120621             xx += 1;
094700120621             $NP(xx) = w053a;
094800120621             if  %subst( w053a : %len( $NP(xx) ) +1 ) <> *blank;
094900120621               xx +=1;
095000120621               $NP(xx) = '   ' +
095100120621                         %trim( %subst( w053a : %len( $NP(xx) ) + 1 ) );
095200120621             endif;
095300120621           endif;
095400120621         ENDIF;
095500120621
095600120621         // . -?Particolarità Consegna?
095700120621         IF  D25gma <> *blank;
095800120621           wTpPar = 'R';
095900120621           wCdPar = D25gma;
096000120621           exsr  sr_TABxPAR;
096100120621           if  TBLuni = *blank  and  $NoDesPartic = *on;
096200120621             $NP(xx) = %trim( $NP(xx) ) + '   ' + wCdPar;
096300120621           else;
096400120621             $NoDesPartic = (TBLuni = *blank);
096500120621             xx += 1;
096600120621             $NP(xx) = w053a;
096700120621             if  %subst( w053a : %len( $NP(xx) ) + 1) <> *blank;
096800120621               xx += 1;
096900120621               $NP(xx) = '   ' +
097000120621                         %trim( %subst( w053a : %len( $NP(xx) ) + 1 ) );
097100120621             endif;
097200120621           endif;
097300120621         ENDIF;
097400120621
097500120621         // . -?Particolarità Varie?
097600120621         //    ?- questa descrizione va messa nell'apposito campo PPAD?
097700120621         //      ?se è "PRIVATO"; idem se è "CONSEGNA AD UFFICI";?
097800120625         //    ?- se 'B' (bancali da rendere al mittente) è comparso un?
097900120625         //      ?apposito box (nella vera LdV);?
098000120621         //    ?- se 'O' (oltre 20 colli => bancali da riportare in mag.)?
098100120625         //      ?è comparso un apposito box, un'apposita nota e l'indica-?
098200120621         //      ?zione nella copia per il destinatario.?
098300120621         //    ?...Potrebbero anche non esserci più elementi di schiera?
098400120621         //    ?disponibili [xx >= %elem($NP)] !?
098500120621         IF  D25gva <> *blank   and
098600120621             D25gva <> 'B '     and  D25gva <> 'O '  and
098700120621             D25gva <> 'P '     and  D25gva <> 'U ';
098800120621           wTpPar = 'S';
098900120621           wCdPar = D25gva;
099000120621           exsr  sr_TABxPAR;
099100120621           if  TBLuni = *blank  and  $NoDesPartic = *on;
099200120621             $NP(xx) = %trim( $NP(xx) ) + '   ' + wCdPar;
099300120621           else;
099400120621             $NoDesPartic = (TBLuni = *blank);
099500120621             if  xx < %elem($NP);
099600120621               xx += 1;
099700120621               $NP(xx) = w053a;
099800120621               if  %subst( w053a : %len( $NP(xx) ) + 1) <> *blank  and
099900120621                   xx < %elem($NP);
100000120621                 xx += 1;
100100120621                 $NP(xx) = '   ' +
100200120621                           %trim( %subst( w053a : %len( $NP(xx) ) + 1 ) );
100300120621               endif;
100400120621             endif;
100500120621           endif;
100600120621         ENDIF;
100700120621
100800120621         // -?Imposto il numero originale di colli componenti la spediz.?
100900120621         //  ?e se ho ancora spazio lo memorizzo in "Note e Particolarità"?
101000120621         if  %subst( D25gva : 1 : 1 ) = 'O'  and  xx < %elem($NP);
101100120625           exsr  sr_RepFIAR5_bnb;
101200120621           if  §AR5bcor <> *zero;
101300120621             xx += 1;
101400120621             $NP(xx) = c_CoOr + %editc( §AR5bcor : 'K' );
101500120621           endif;
101600120621         endif;
101700120621
101800120621       ENDSR;
101900120621
102000120621       //--------------------------------------------------------------
102100120621       //?Reperimento PARTICOLARITÀ su TABEL00F                        ?
102200120621       //--------------------------------------------------------------
102300120621       BEGSR  sr_TABxPAR;
102400120621
102500120621         w053a = wCdPar;
102600120621         k_TBLcod = '7V';
102700120621         k_TBLkey = KeyPAR;
102800120621         chain  %kds (k03tabel00 )  TABEL;
102900120621
103000120621         if  %found(TABEL00F)  and  TBLflg = *blank;
103100120621           w053a = wCdPar + '-' + %trim( %subst( TBLuni : 1 : 49 ) );
103200120621         else;
103300120621           clear  TBLuni;
103400120621         endif;
103500120621
103600120621       ENDSR;
103700120620
103800120620       //--------------------------------------------------------------
103900120620       //?Preparazione dati per
104000120620       //?stampa copia LdV x scannerizzazione documenti clienti.       ?
104100120620       //--------------------------------------------------------------
104200120620       BEGSR  sr_RiempiLdV;
104300120620
104400120620         // -?Impotazione barcode: Anno+Lnp+Nrs+Nsp+Ckd?
104500120620         //  ?(richiamo del TRUL28R per ricavarne il check-digit)?
104600120620         reset  TRUL28ds;
104700120620         BARcdeAAS = %int( %subst( %editc( D25aas : 'X' ) : 3 : 2 ) );
104800120620         BARcdeLNP = D25lnp;
104900120620         BARcdeNRS = D25nrs;
105000120620         BARcdeNSP = D25nsp;
105100120620         I28cod    = BarCodeDS;
105200120620         trul28R1 ( trul28ds );
105300120620         if O28err = *blank;
105400120620           Tbcd = O28cod;
105500120620           *in50 = *on;
105600120620         endif;
105700120620
105800120620         // -?Costanti: dati BRT S.p.A.?
105900120620         TBAR = $BRT;
106000120620         TBA1 = $Intest1;
106100120620         TBA2 = $Intest2;
106200120621         //TBA3 = $Intest3;        ?NO!!!?
106300120620         TBA4 = $Intest4;
106400120620         TBA5 = $Intest5;
106500120621         //TBA6 = $Intest6;        ?NO!!!?
106600120620         TBA7 = $Intest7;
106700120620
106800120620         // -?Mittente?
106900120621         if  D25ccm <> *zero;
107000120621           Pccm = %editc( D25ccm : 'X' );
107100120620         endif;
107200120620         Prsm = D25rsm;
107300120620         Pinm = D25inm;
107400120621         Pcam = D25cam;
107500120621         Plom = D25lom;
107600120621         Pprm = D25prm;
107700120621         Pnzm = D25nzm;
107800120625
107900120621         // -?Mittente originale?
108000120621         if  D25rmo <> *blank;
108100120621           Pkmo = 'MITT. ORIGINALE';
108200120621           Prmo = D25rmo;
108300120621         endif;
108400120621
108500120621         // -?Flag "allega documento di trasporto"?
108600120621         select;
108700120621           // -?LdV con DDT allegato oltre il 1° tentativo di consegna?
108800120621           //when  D25bam = 'S'   and   w25tmc <> *blank;
108900120621           //  evalr Pddt = 'DDT sul collo';
109000120621           // -?LdV con DDT allegato (al 1° tentativo di consegna)?
109100120621           when  D25bam = 'S';
109200120621             evalr Pddt = 'DDT';
109300130315           // -?Esiste PDF abbinato?
109400130321           when  §AR5als = 'S';
109500130315             evalr Pddt = 'PACKING-LIST PDF';
109600120621           // -?Esiste packing-list abbinata?
109700130321           when  §AR5alx = 'S';
109800120621             evalr Pddt = 'PACKING-LIST';
109900120621         endsl;
110000120621
110100120621         // -?Sigla Operatore?
110200120625         //Psop = D25sop;
110300120621
110400120621         // -?Destinatario?
110500120621         if  D25ccd <> *zero;
110600120621           Pccd = %editc( D25ccd : 'X' );
110700120621         endif;
110800120621         Prsd = D25rsd;
110900120621         Prse = D25rd2;
111000120621         Pind = D25ind;
111100120621         Pcad = D25cad;
111200120621         Plod = D25lod;
111300120621         Pprd = D25prd;
111400120621         Pnzd = D25nzd;
111500120621
111600120621         // -?Colli: numero, peso, volume, flag tipo-volume?
111700120621         Pncl = D25ncl;
111800120621         Ppkf = D25pkf;
111900120621         Pvlf = D25vlf;
112000120621         if  D25fvf <> *blank;
112100120621           evalr Pfvf = D25fvf;
112200120621         endif;
112300120621
112400120621         // -?Natura merce?
112500120621         Pnat = D25nas;
112600120621
112700120621         // -?Filiale di Arrivo?
112800120621         Pkla  = 'FILIALE';
112900120621         Plna1 = %editc(D25lna : 'X')
113000120621               + ' '
113100120621               + D25dla
113200120621               + ' - '
113300120621               + D25inl
113400120621               + ' Tel.'
113500120621               + %trimr(D25tel);
113600120621         Plna2 = %editc(D25cal : 'X')
113700120621               + ' '
113800120621               + D25lol
113900120621               + ' '
114000120621               + D25prl
114100120621               + '                  '
114200120621               + ' Fax '
114300120621               + %trimr(D25fax);
114400120621
114500120621         // -?Riferimenti Mittente?
114600120621         Prmn = D25rmn;
114700120621         Prma = D25rma;
114800120621
114900120621         // -?Note (1 e 2) e Particolarità (3, 4, 5 e 6)?
115000120621         if  $NP(1) <> *blank  or  $NP(2) <> *blank  or
115100120621             $NP(3) <> *blank  or  $NP(4) <> *blank  or
115200120621             $NP(5) <> *blank  or  $NP(6) <> *blank;
115300120621           Pknp = 'NOTE  E  PARTICOLARITÀ';
115400120621           Pnp1 = $NP(1);
115500120621           Pnp2 = $NP(2);
115600120621           Pnp3 = $NP(3);
115700120621           Pnp4 = $NP(4);
115800120621           Pnp5 = $NP(5);
115900120621           Pnp6 = $NP(6);
116000120621         endif;
116100120621
116200120625         // -?Privato / Uffici?
116300120621         select;
116400170413           //*·when  D25gva = 'P ';
116500170413           //*·  evalr Ppad = 'PRIVATO';
116600120625           when  D25gva = 'U ';
116700120621             evalr Ppad = 'CONSEGNA AD UFFICI';
116800120625           //when  TASftc = 'A'  or  TAStc2 = 'A';
116900120625           //  evalr Ppad = 'APPUNTAMENTO';
117000120625           //when  D25gva = 'B ';
117100120625           //  evalr Ppad = 'BANCALI DA RENDERE AL MITTENTE';
117200120625           //when  D25gva = 'O ';
117300120625           //  evalr Ppad = 'BANCALI DA RIPORTARE A MAGAZZINO';
117400120621         endsl;
117500120621
117600120621         // -?Numero Spedizione?
117700120621         Plnp = D25lnp;
117800120621         Pnrs = D25nrs;
117900120621         Pnsp = D25nsp;
118000120621
118100120621         // -?Data Spedizione?
118200120622         //wData_EUR = %date( (D25aas * 10000) + D25mgs : *iso );
118300120622         //Pdsp = %dec(wData_EUR);
118400120622         reset  wData_EUR;
118500120622         monitor;
118600120622           wData_EUR = %date( (D25aas * 10000) + D25mgs : *iso );
118700120622           Pdsp = %dec(wData_EUR);
118800120622         on-error;
118900120622           Pdsp = D25aas;
119000120622         endmon;
119100120621
119200120621       ENDSR;
119300120620
119400120620       //--------------------------------------------------------------
119500120620       //?Stampa singola copia LdV x scannerizzaz. documenti clienti.  ?
119600120620       //--------------------------------------------------------------
119700120620       BEGSR  sr_StampaLdV;
119800120620
119900120620         // -?Posizionamento in testa al foglio A4?
120000120620         exsr  sr_Posiz_Box1;
120100120620
120200120620         // -?Copia LdV x scanerizzazione documenti clienti?
120300120620         write  A4prtLDV;
120400120620         write  A4prtBOX;
120500120620
120600120620         // -?Simil-Logo BRT?
120700120620         write  A4prtLOGO;
120800120620         write  A4prtLOGOd;
120900120620
121000120620       ENDSR;
121100120620
121200120620       //--------------------------------------------------------------
121300120620       //?Posizionamento del box all'interno della pag. A4.            ?
121400120620       //--------------------------------------------------------------
121500120620       BEGSR  sr_Posiz_Box1;
121600120620
121700120620         // -?Si salta alla pag. A4 successiva: la LdV in esame?
121800120620         //  ?dev'essere stampata in posizione iniziale dell'A4.?
121900120620         wSeqLdV = 1;
122000120620
122100120620         // -?Impostaz. delle posizioni nella pagina in corso di stampa?
122200120620         wPosIni = (c_Len1LdV * (wSeqLdV - 1));
122300120620
122400120620         // -?Posizionamento nel foglio A4 in base al n° LdV in stampa?
122500120620         WRITE  A4prtPOS;
122600120620
122700120620         // -?Posizionamenti nella copia per il vettore?
122800120620
122900120620         // -?Box (& Logo & BarCode)?
123000120620         §Box1As = wPosIni + c_PosBox1As;
123100120620         §Box1Ai = wPosIni + c_PosBox1Bs + c_LineNarrow;
123200120620         §Box1Bs = wPosIni + c_PosBox1Bs;
123300120620         §Box1Bi = wPosIni + c_PosBox1Bi;
123400120620
123500120620         // -?Simil-Logo BRT?
123600120620         §PlblS  = §Box1As;
123700120620         §PlblVS = 7.581;
123800120620         exsr  sr_SimilLogoBRT_;
123900120620
124000120620         // -?Costanti dati anagrafici BRT?
124100120620         §PC0    = wPosIni + c_PosTBar;
124200120620         §PC1    = wPosIni + c_PosTBa1;
124300120620         §PC2    = wPosIni + c_PosTBa2x;
124400120620         §PC3    = wPosIni + c_PosTBa3x;
124500120620         §PC4    = wPosIni + c_PosTBa4x;
124600120620         §PC5    = wPosIni + c_PosTBa5x;
124700120620         §PC6    = wPosIni + c_PosTBa6x;
124800120620
124900120620         // -?Linee orizzontali nel box?
125000120620         §Lo1S1  = wPosIni + c_PosLin1oS1;
125100120620         §Lo1S2  = wPosIni + c_PosLin1oS2;
125200120620       //§Lo1S3  = wPosIni + c_PosLin1oS3;
125300120620         §Lo1S4  = wPosIni + c_PosLin1oS4;
125400120620         §Lo1S5  = wPosIni + c_PosLin1OS5;
125500120620         //§Lo1C1  = wPosIni + c_PosLin1oC1;
125600120620         §Lo1C2  = wPosIni + c_PosLin1oC2;
125700120620         §Lo1D1  = wPosIni + c_PosLin1oD1;
125800120620
125900120620         // -?Lunghezza linee verticali nel box in copia per il vett.?
126000120620         §LLv1A  = §Lo1S5  - §Lo1S1;
126100120620         §LLv1B  = §Lo1S4  - §Box1Bs;
126200120620         §LLv1C  = §Box1Bi - §Box1Bs;
126300120620
126400120620       ENDSR;
126500120620
126600120620      /end-free
126700120620
126800120620       //-------------------------------------------------------------
126900120620       //?Posizionamento del simil-logo BRT (unico)                   ?
127000120620       //-------------------------------------------------------------
127100120620      /copy gaitrasrc/srcTNSY,LogoBRT_R2
127200120620
127300120620      /free
127400120620
127500120620       //--------------------------------------------------------------
127600120620       //?Operazioni finali.                                           ?
127700120620       //--------------------------------------------------------------
127800120620       BEGSR  sr_RoutEnd;
127900120620
128000120620         // -?Chiusura archivi / applicazioni?
128100120620         If  D25tla <> *blank;
128200120620
128300120620           *inLR = *on;
128400120622
128500120622           // -?Chiusura funzioni precedentemente aperte?
128600120622           reset  TIBS69ds;
128700120622           //I69tla = 'C';         ?(già così)?
128800120622           tibs69r( tibs69ds :
128900120622                    ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
129000120620
129100120620           // -?Chiusura file di stampa?
129200120620           close  TNSB25P;
129300120621
129400120621           // -?Chiusura files?
129500120621           if  %open(TITAS30C);
129600120621             close  TITAS30C;
129700120621           endif;
129800120621           if  %open(TITAA30C);
129900120621             close  TITAA30C;
130000120621           endif;
130100120621           if  %open(TITA430C);
130200120621             close  TITA430C;
130300120621           endif;
130400120621           if  %open(FIAR531C);
130500120621             close  FIAR531C;
130600120621           endif;
130700170517           if  %open(TFNTC01L);
130800170517             close  TFNTC01L;
130900170517           endif;
131000120620
131100120620         EndIf;
131200120620
131300120620         // -?Ritorno al chiamante?
131400120620         return;
131500120620
131600120620       ENDSR;
131700120620
131800120620      /end-free
131900120620
132000120620       //--------------------------------------------------------------
132100120620       //?Definizione schiere a tempo di compilazione                  ?
132200120620       //--------------------------------------------------------------
132300120620
132400120620** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
132500120620IMPOSSIBILE CONTINUARE LA RISTAMPA DELLE SIMIL-LDV
132600120620PERCHE' NE SONO GIA' STATE STAMPATE PIU' DI &NrLdV !!!
