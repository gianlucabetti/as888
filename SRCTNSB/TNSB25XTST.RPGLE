000100120618       //==============================================================
000200120618       //?TNSB25XTST // TEST stampa copia LdV x Scannerizz.Doc.Clienti ?
000300120618       //==============================================================
000400120618
000500120618       //--------------------------------------------------------------
000600120618       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700120618       //--------------------------------------------------------------
000800120618
000900120618     /*END
001000120618
001100120618       //--------------------------------------------------------------
001200120618       //?Specifiche di controllo.                                     ?
001300120618       //--------------------------------------------------------------
001400120618
001500120618     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001600120618     h dftactgrp(*no)
001700120618
001800120618       //--------------------------------------------------------------
001900120618       //?Dichiarazione file.                                          ?
002000120618       //--------------------------------------------------------------
002100120618
002200120618       // -?Organigramma filiale/aziendale?
002300120618     fAZORG01L  if   e           k disk
002400120618
002500120618       // -?File di stampa?
002600120619     fTNSB25P   o    e             printer usropn
002700120619     f                                     infds(PrtInfoa4)
002800120618     f                                     rename(SB25POS   :A4prtPOS  )
002900120618     f                                     rename(SB25BOX   :A4prtBOX  )
003000120618     f                                     rename(SB25LOGO  :A4prtLogo )
003100120618     f                                     rename(SB25LOGODA:A4prtLogoD)
003200120618     f                                     rename(SB25LDV   :A4prtLDV  )
003300120618
003400120618       //--------------------------------------------------------------
003500120618       //?Definizione costanti.                                        ?
003600120618       //--------------------------------------------------------------
003700120618
003800120619       // -?Costanti di comodo?
003900120619     d c_Bar128        c                   const('123219812345674')
004000120619     d c_Lungh         c                   const('*...+....1....+....2....+....-
004100120618     d                                     3....+....4....+....5....+....6....+-
004200120618     d                                     ....7....+....8....+....A')
004300120619     d c_LunghN        c                   const(123456789012345678901234567890)
004400120619     d c_LunghA        c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZABC-
004500120618     d                                     DEFGHIJKLMNOPQRSTUVWXYZ')
004600120619     d c_LunghW        c                   const('WWWWWWWWWWWWWWWWWWWWWWWWWWWWW-
004700120618     d                                     WWWWWWWWWWWWWWWWWWWWWWW')
004800120619       // -?Intestazioni in stampa?
004900120618     d $BRT            c                   const('BRT S.p.A.')
005000120618     d $Intest1        c                   const('Sede Operativa ed Am-
005100120618     d                                     ministrativa:')
005200120618     d $Intest2        c                   const('via E. Mattei, 42 - -
005300120618     d                                     40138 Bologna')
005400120618     d $Intest3        c                   const('Sede Legale: Piazza -
005500120618     d                                     Diaz, 7 - 20123 Milano')
005600120618     d $Intest4x       c                   const('P.IVA 03987100371')
005700120618     d $Intest4        c                   const('C.F. e P.IVA 04507990150')
005800120618     d $Intest5x       c                   const('Capitale Sociale Eur-
005900120618     d                                     o 100.000.000 int. vers.')
006000120618     d $Intest5        c                   const(' Capitale Sociale Eu-
006100120618     d                                     ro 24.000.000 int. vers.')
006200120618     d $Intest6        c                   const('info on-line')
006300120618     d $Intest7_brt    c                   const('www.brt.it')
006400120619
006500120619       // -?Lunghezza (in inches) di una singola LdV?
006600120619     d c_Len1LdV       c                   const(5.775)
006700120619
006800120619       // -?Spessore delle linee in inches/pollici (*memo)?
006900120618     d c_LineNarrow    c                   const(0.008)
007000120618     d c_LineMedium    c                   const(0.017)
007100120618     d c_LineWide      c                   const(0.025)
007200120619
007300120619       // -?Posizioni dell'intestazione "BRT S.p.A."?
007400120618     d c_PosTBar       c                   const(0.270)
007500120618     d c_PosTBa1       c                   const(0.350)
007600120618     d c_PosTBa2x      c                   const(0.430)
007700120618     d c_PosTBa3x      c                   const(0.500)
007800120618     d c_PosTBa4x      c                   const(0.590)
007900120618     d c_PosTBa5x      c                   const(0.660)
008000120618     d c_PosTBa6x      c                   const(0.750)
008100120619
008200120619       // -?Posizioni dei bordi orizzontali superiori box?
008300120619       //  ?(distanza dal lato superiore dell'A4)?
008400120619       //  ?N.B.?- c_PosBoxNXs => posizione iniziale della linea?
008500120619       //                        ?orizzontale Superiore?
008600120619       //       ?- c_PosBoxNXi => posizione iniziale della linea?
008700120619       //                        ?orizzontale Inferiore?
008800120618     d c_PosBox1As     c                   const(0.200)
008900120618     d c_PosBox1Bs     c                   const(0.790)
009000120618     d c_PosBox1Bi     c                   const(2.850)
009100120618     d c_PosBox1Cs     c                   const(1.950)
009200120618     d c_PosBox1Ds     c                   const(2.453)
009300120619       // -?Posizioni di altre linee orizzontali?
009400120618     d c_PosLin1OS1    c                   const(1.350)
009500120618     d c_PosLin1OS2    c                   const(1.790)
009600120618     d c_PosLin1OS3    c                   const(2.010)
009700120618     d c_PosLin1OS4    c                   const(2.230)
009800120618     d c_PosLin1OS5    c                   const(2.470)
009900120618     d c_PosLin1OC1    c                   const(1.680)
010000120618     d c_PosLin1OC2    c                   const(1.240)
010100120618     d c_PosLin1OD1    c                   const(1.010)
010200120619
010300120618       // -?Dimensioni base per i simil-loghi Bartolini e BRT?
010400120618      /copy gaitrasrc/srcTNSY,LogoBRT_R1
010500120619
010600120618       // -?Limite righe stampabili su 1/4 di A4 in 1/9"?
010700120619     d c_MaxLine1      c                   const(025)
010800120619
010900120619       //--------------------------------------------------------------
011000120619       //?Definizione schiere.                                         ?
011100120619       //--------------------------------------------------------------
011200120619
011300120618     d $Cli            s             50    dim(26) ctdata perrcd(1)
011400120619     d $Note           s             90    dim( 7) ctdata perrcd(1)
011500120619
011600120619       // -?Note e Particolarità?
011700120618     d $NP             s                   dim(6)  inz  like(Pnp1)
011800120619
011900120619       //--------------------------------------------------------------
012000120619       //?Definizione aree dati.                                       ?
012100120619       //--------------------------------------------------------------
012200120619
012300120619       // -?Dati utente?
012400120619     d §AzUte        e ds                  extname(AZUTE00F)
012500120619     d                                     dtaara
012600120619     d §DatiUte      e ds                  extname(dDatiUte)
012700120619     d                                     dtaara
012800120619
012900120619       //--------------------------------------------------------------
013000120619       //?Definizione strutture dati.                                  ?
013100120619       //--------------------------------------------------------------
013200120619
013300120619       // -?Status ds?
013400120618     d Status         sds           333
013500120618     d  SDSpgm           *proc
013600120619     d*//  SDSprm           *parms
013700120618     d  SDSdta               191    198
013800120618     d  SDSjob               244    253
013900120618     d  SDSusr               254    263
014000120619
014100120619       // -?InfDS?
014200120619     d prtINFOa4       ds
014300120619     d   CurLineA4           367    368i 0
014400120619
014500120619       // -?Codice a Barre?
014600120619     d BARcodeDS       ds            17    inz
014700120619     d  BARcdeAAS                     2s 0 inz
014800120619     d  BARcdeLNP                     3s 0 inz
014900120619     d  BARcdeNRS                     2s 0 inz
015000120619     d  BARcdeNSP                     7s 0 inz
015100120619     d  BARcdeXXX                     3    inz
015200120619
015300120619       //--------------------------------------------------------------
015400120619       //?Definizione variabili globali.                               ?
015500120619       //--------------------------------------------------------------
015600120619
015700120619       // -?Numero di copie da stampare?
015800120619     d wNrCpy          s              3  0 inz(001)
015900120619
016000120619       // -?Variabili per il calcolo delle posizioni per box in A4?
016100120618     d wNumLdV         s              5  0 inz
016200120618     d wSeqLdV         s              1  0 inz
016300120618     d wPosIni         s              5  3 inz
016400120618
016500120618       //--------------------------------------------------------------
016600120619       //?Definizione prototipi procedure.                             ?
016700120618       //--------------------------------------------------------------
016800120619
016900120619       // -?Reperimento dati utente?
017000120619     d TIBS34ds      e ds
017100120619      /copy gaitrasrc/srcProtoPR,TIBS34R
017200120619
017300120619       // -?Parametri per TRUL28R (calcolo check digit del barcode)?
017400120619     d TRUL28ds      e ds                  inz
017500120619     d  I28tla       e                     inz('L')
017600120619     d  I28mod       e                     inz('BAR')
017700120619       // -?Calcolo check-digit per bar-code?
017800120618     d trul28r1        pr                  extpgm('TRUL28R1')
017900120618     d   trul28ds...
018000120618     d                                     likeds(TRUL28ds)
018100120618
018200120619       //--------------------------------------------------------------
018300120619       //?Definizione key-list.                                        ?
018400120619       //--------------------------------------------------------------
018500120618
018600120618
018700120619       //--------------------------------------------------------------
018800120619       //?M A I N - L I N E                                            ?
018900120619       //--------------------------------------------------------------
019000120619
019100120618     c     *Entry        plist
019200120618     c                   parm                    £Mod              1
019300120618     c                   parm                    £NrCpy           15 5
019400120619       // -?£Mod   - tipo modulo: *off = modulo completo ("WWWWW")?
019500120619       //  ?                      *on  = modulo fittizio ("ROSSI")?
019600120619       // -?£NrCpy - num. copie da stampare?
019700120619
019800120619      /free
019900120619
020000120619       // -?Operazioni iniziali?
020100120619       exsr  sr_RoutInz;
020200120619
020300120619       // -?Stampa?
020400120619       exsr  sr_Stampa;
020500120619
020600120619       // -?Operazioni finali?
020700120619       close  TNSB25P;
020800120619       return;
020900120618
021000120619       //--------------------------------------------------------------
021100120619       //?Operazioni iniziali.                                         ?
021200120619       //--------------------------------------------------------------
021300120619       BEGSR  sr_RoutInz;
021400120619
021500120619         *inLR  = *on;
021600120619
021700120619         // -?Reperimento dati job?
021800120619         exsr  sr_DatiJob;
021900120619
022000120619         // -?Verifica n° copie richieste?
022100120619         if  %parms() > 1;
022200120619           wNrCpy = £NrCpy;
022300120619         endif;
022400120619
022500120619         // -?Apertura del PRTF?
022600120619         open  TNSB25P;
022700120619
022800120619         // -?Pulizia del PRTF?
022900120619         exsr  sr_ClrPRTF;
023000120619
023100120619       ENDSR;
023200120619
023300120619       //--------------------------------------------------------------
023400120619       //?Reperimento Dati del job (Utente/Operativi).                 ?
023500120619       //--------------------------------------------------------------
023600120619       BEGSR  sr_DatiJob;
023700120619
023800120619         in(E) §AzUte;
023900120619         if NOT %error;
024000120619           in(E) §DatiUte;
024100120619         endif;
024200120619         if %error or RSut = *blanks;
024300120619           clear TIBS34ds;
024400120619           tibs34r ( tibs34ds );
024500120619           in §AzUte;
024600120619           in §DatiUte;
024700120619         endif;
024800120619
024900120619       ENDSR;
025000120618
025100120619       //--------------------------------------------------------------
025200120619       //?Pulizia del PrtF.                                            ?
025300120619       //--------------------------------------------------------------
025400120619       BEGSR  sr_ClrPRTF;
025500120619
025600120619         //clear  A4prtPos;        ?(NON ha campi)?
025700120619         clear  A4prtBox;
025800120619         clear  A4prtLogo;
025900120619         clear  A4prtLogoD;
026000120619         clear  A4prtLdv;
026100120619
026200120619         clear  *in;
026300120619
026400120619         clear  $NP;
026500120619
026600120619         // -?Tipo bolla non DPD?
026700120619         //*in71 = *off;           ?(già così)?
026800120619
026900120619       ENDSR;
027000120619
027100120619       //--------------------------------------------------------------
027200120619       //?Stampa copia LdV x scannerizzazione documenti clienti.       ?
027300120619       //--------------------------------------------------------------
027400120619       BEGSR  sr_Stampa;
027500120618
027600120619         // -?Decodifica filiale?
027700120619         exsr  sr_DecodPO;
027800120619
027900120619         For  wNumLdV = 1  To  wNrCpy;
028000120619
028100120619           // -?Preparazione dati per stampa bolla?
028200120619           exsr  sr_RiempiLDV;
028300120619
028400120619           // -?Stampa copia LdV x scannerizzazione documenti clienti?
028500120619           exsr  sr_StampaLDV;
028600120619
028700120619         EndFor;
028800120619
028900120619       ENDSR;
029000120619
029100120619       //--------------------------------------------------------------
029200120619       //?Decofica LNA (filiale utente).
029300120619       //--------------------------------------------------------------
029400120619       BEGSR  sr_DecodPO;
029500120619
029600120619         chain  (DUTpou)  AZORG;
029700120619
029800120619         if  Not %found(AZORG01L)  or  ORGfva <> *blank;
029900120619           clear  ORGdes;
030000120619           clear  ORGind;
030100120619           clear  ORGcpf;
030200120619           clear  ORGloc;
030300120619           clear  ORGpro;
030400120619           clear  ORGtel;
030500120619           clear  ORGfax;
030600120619         endif;
030700120619
030800120619       ENDSR;
030900120619
031000120619       //--------------------------------------------------------------
031100120619       //?Preparazione dati per
031200120619       //?stampa copia LdV x scannerizzazione documenti clienti.       ?
031300120619       //--------------------------------------------------------------
031400120619       BEGSR  sr_RiempiLdV;
031500120619
031600120619         // -?Indicatori utilizzati:?
031700120619         *in50 = *off;
031800120619         //*in71 = *off;
031900120619
032000120619         // -?Impotazione barcode?
032100120619         *in50 = *on;
032200120619         if  %parms() = *zero  or  £Mod = *off;
032300120619           Tbcd = c_Bar128;
032400120619         else;
032500120619           reset TRUL28ds;
032600120619           BarCodeDS  = c_Bar128;
032700120619           BARcdeAAS  = uYear;
032800120619           BARcdeNSP += (wNumLdV - 1);
032900120619           clear BARcdeXXX;
033000120619           I28cod     = BarCodeDS;
033100120619           trul28R1 ( trul28ds );
033200120619           if O28err = *blank;
033300120619             Tbcd = O28cod;
033400120619           endif;
033500120619         endif;
033600120619
033700120619         // -?Costanti: dati BRT S.p.A.?
033800120619         TBAR = $BRT;
033900120619         TBA1 = $Intest1;
034000120619         TBA2 = $Intest2;
034100120619         //TBA3 = $Intest3;       ?NO!!!?
034200120619         TBA4 = $Intest4;
034300120619         TBA5 = $Intest5;
034400120619         //TBA6 = $Intest6;       ?NO!!!?
034500120619         TBA7 = $Intest7_brt;
034600120619
034700120619         // -?Costanti: "RISTAMPA"?
034800120619         if  %parms() = *zero  or  £Mod = *off;
034900120619           Tkri = 'W';
035000120619         endif;
035100120619
035200120619         // -?Costanti: "PACKING-LIST" / "DDT"?
035300120619         if  %parms() = *zero  or  £Mod = *off;
035400120619           evalr  Pddt = 'PACKING-LIST / DDT sul collo';
035500120619         else;
035600120619           evalr  Pddt = 'DDT sul collo';
035700120619         endif;
035800120619
035900120619         // -?Costanti: "PRIVATO" / "APPUNTAMENTO"?
036000120619         if  %parms() = *zero  or  £Mod = *off;
036100120619           evalr  Ppad = 'CONSEGNA AD UFFICI';
036200120619         else;
036300120619           //evalr  Ppad = 'PRIVATO';
036400120619           evalr  Ppad = 'CONSEGNA AD UFFICI';
036500120619         endif;
036600120619
036700120619         // -?Spedizione?
036800120619         if  %parms() = *zero  or  £Mod = *off;
036900120619           Plnp = %int( %subst( %editc( c_LunghN : 'X' ) : 1 : %len(Plnp) ) );
037000120619           Pnrs = %int( %subst( %editc( c_LunghN : 'X' ) : 1 : %len(Pnrs) ) );
037100120619           Pnsp = %int( %subst( %editc( c_LunghN : 'X' ) : 1 : %len(Pnsp) ) );
037200120619           Pdsp = %int( %subst( %editc( c_LunghN : 'X' ) : 1 : %len(Pdsp) ) );
037300120619         else;
037400120619           //Plnp = DUTpou;
037500120619           Plnp = BARcdeLNP;
037600120619           Pnrs = BARcdeNRS;
037700120619           Pnsp = BARcdeNSP;
037800120619           Pdsp = *date;
037900120619         endif;
038000120619
038100120619         // -?Sigla operatore video?
038200120619         Psop = 'X';
038300120619
038400120619         // -?Mittente?
038500120619         if  %parms() = *zero  or  £Mod = *off;
038600120619           Pccm = %subst( %editc( c_LunghN : 'X' ) : 1 : %len(Pccm) );
038700120619           Prsm = c_Lungh;
038800120619           Pinm = c_LunghA;
038900120619           Pcam = %subst( %editc( c_LunghN : 'X' ) : 1 : %len(Pcam) );
039000120619           Plom = c_Lungh;
039100120619           Pprm = c_LunghA;
039200120619           Pnzm = c_LunghA;
039300120619           Pkmo = 'MITT. ORIGINALE';
039400120619           Prmo = c_Lungh;
039500120619         else;
039600120619           Pccm = %editc( DUTpou : 'X' ) + '1234';
039700120619           Prsm = $Cli(1);
039800120619           Pinm = $Cli(2);
039900120619           Pcam = $Cli(3);
040000120619           Plom = $Cli(4);
040100120619           Pprm = $Cli(5);
040200120619           Pnzm = $Cli(6);
040300120619           Pkmo = 'MITT. ORIGINALE';
040400120619           Prmo = $Cli(7);
040500120619         endif;
040600120619
040700120619         // -?Destinatario?
040800120619         if  %parms() = *zero  or  £Mod = *off;
040900120619           Pccd = %subst( %editc( c_LunghN : 'X' ) : 1 : %len(Pccd) );
041000120619           Prsd = c_LunghW;
041100120619           Prse = c_Lungh;
041200120619           Pind = c_LunghA;
041300120619           Pcad = %subst( %editc( c_LunghN : 'X' ) : 1 : %len(Pcad) );
041400120619           Plod = c_Lungh;
041500120619           Pprd = c_LunghA;
041600120619           Pnzd = c_LunghA;
041700120619         else;
041800120619           Pccd = %editc( DUTpou : 'X' ) + '8888';
041900120619           Prsd = $Cli(8);
042000120619           Prse = $Cli(9);
042100120619           Pind = $Cli(10);
042200120619           Pcad = $Cli(11);
042300120619           Plod = $Cli(12);
042400120619           Pprd = $Cli(13);
042500120619           Pnzd = $Cli(14);
042600120619         endif;
042700120619
042800120619         // -?Colli: numero, peso, volume, flag-tipo, descrizione?
042900120619         if  %parms() = *zero  or  £Mod = *off;
043000120619           Pncl = %int( %subst( %editc( c_LunghN : 'X' )
043100120619                                : 1 : %len(Pncl) ) );
043200120619           Ppkf = %int( %subst( %editc( c_LunghN : 'X' )
043300120619                                : 1 : %len(Ppkf) ) ) / 10;
043400120619           Pvlf = %int( %subst( %editc( c_LunghN : 'X' )
043500120619                                : 1 : %len(Pvlf) ) ) / 1000;
043600120619           Pfvf = 'W';
043700120619           Pnat = c_Lungh;
043800120619         else;
043900120619           Pncl = 5;
044000120619           Ppkf = 56,7;
044100120619           Pvlf = 2,345;
044200120619           Pfvf = 'R';
044300120619           Pnat = 'CARTONI';
044400120619          endif;
044500120619
044600120619         // -?Riferimento Mittente?
044700120619         if  %parms() = *zero  or  £Mod = *off;
044800120619           Prmn = %int( %subst( %editc( c_LunghN : 'X' )
044900120619                                : 1 : %len(Prmn) ) );
045000120619           Prma = c_LunghA;
045100120619         else;
045200120619           Prmn = 210987654321;
045300120619           Prma = 'ABC-EFGHIJKLMNO';
045400120619         endif;
045500120619
045600120619         // -?Note?
045700120619         if  %parms() = *zero  or  £Mod = *off;
045800120619           Pknp = 'NOTE  E  PARTICOLARITÀ';
045900120619           Pnp1 = c_Lungh;
046000120619           Pnp2 = c_LunghA;
046100120619         else;
046200120619           Pknp = 'NOTE  E  PARTICOLARITÀ';
046300120619           Pnp1 = %subst( $Note(1) : 1 : 50 );
046400120619           Pnp2 = %subst( $Note(2) : 1 : 50 );
046500120619         endif;
046600120619
046700120619         // -?Particolarità?
046800120619         if  %parms() = *zero  or  £Mod = *off;
046900120619           Pknp = 'NOTE  E  PARTICOLARITÀ';
047000120619           Pnp3 = c_Lungh;
047100120619           Pnp4 = c_LunghA;
047200120619           Pnp5 = c_LunghA;
047300120619           Pnp6 = c_LunghA;
047400120619           Pnp7 = c_Lungh;
047500120619           Pnp8 = c_Lungh;
047600120619         else;
047700120619           Pknp = 'NOTE  E  PARTICOLARITÀ';
047800120619           Pnp3 = %subst( $Note(3) : 1 : 50 );
047900120619           Pnp4 = %subst( $Note(4) : 1 : 50 );
048000120619           Pnp3 = %subst( $Note(5) : 1 : 50 );
048100120619           Pnp4 = %subst( $Note(6) : 1 : 50 );
048200120619         endif;
048300120619
048400120619         // -?Fil. Arrivo?
048500120619         Pkla = 'FILIALE';
048600120619         if  %parms() = *zero  or  £Mod = *off;
048700120619           Plna1 = '123'
048800120619                 + ' '
048900120619                 + 'ABCDEFGHIJKLMNOPQRST'
049000120619                 + ' - '
049100120619                 + 'INDIRIZZO-KLMNOPQRST'
049200120619                 + ' Tel.'
049300120619                 + '1234/67890123';
049400120619           Plna2 = '01234 '
049500120619                 + 'LOCALITA-JKLMNOPQRST '
049600120619                 + 'PR'
049700120619                 + '                  '
049800120619                 + ' Fax '
049900120619                 + '1234/67890123';
050000120619         else;
050100120619           Plna1 = %editc(ORGfil: 'X')
050200120619                 + ' '
050300120619                 + ORGdes
050400120619                 + ' - '
050500120619                 + ORGind
050600120619                 + ' Tel.'
050700120619                 + ORGtel;
050800120619           Plna2 = %editc(ORGcpf : 'X')
050900120619                 + ' '
051000120619                 + ORGloc
051100120619                 + ' '
051200120619                 + ORGpro
051300120619                 + '                  '
051400120619                 + ' Fax '
051500120619                 + ORGfax;
051600120619         endif;
051700120619
051800120619         // -?Altre costanti...?
051900120619         if  %parms() = *zero  or  £Mod = *off;
052000120619           Pnux = 'NON UTILIZZARE PER LA CONSEGNA';
052100120619         endif;
052200120619
052300120619       ENDSR;
052400120619
052500120619       //--------------------------------------------------------------
052600120619       //?Stampa singola copia LdV x scannerizzaz. documenti clienti.  ?
052700120619       //--------------------------------------------------------------
052800120619       BEGSR  sr_StampaLdV;
052900120619
053000120619         // -?Posizionamento in testa al foglio A4?
053100120619         exsr  sr_Posiz_Box1;
053200120619
053300120619         // -?Copia LdV x scanerizzazione documenti clienti?
053400120619         write  A4prtLDV;
053500120619         write  A4prtBOX;
053600120619
053700120619         // -?Simil-Logo BRT?
053800120619         write  A4prtLOGO;
053900120619         write  A4prtLOGOd;
054000120619
054100120619         // -?Dati DPD?
054200120619         //if  *in71;
054300120619         //  exsr  sr_StampaDPD;
054400120619         //endif;
054500120619
054600120619       ENDSR;
054700120618
054800120618       //--------------------------------------------------------------
054900120619       //?Posizionamento del box all'interno della pag. A4.            ?
055000120618       //--------------------------------------------------------------
055100120619       BEGSR  sr_Posiz_Box1;
055200120618
055300120619         // -?Si salta alla pag. A4 successiva: la LdV in esame?
055400120619         //  ?dev'essere stampata in posizione iniziale dell'A4.?
055500120619         wSeqLdV = 1;
055600120619
055700120618         // -?Impostaz. delle posizioni nella pagina in corso di stampa?
055800120619         wPosIni = (c_Len1LdV * (wSeqLdV - 1));
055900120618
056000120618         // -?Posizionamento nel foglio A4 in base al n° LdV in stampa?
056100120618         WRITE  A4prtPOS;
056200120618
056300120618         // -?Posizionamenti nella copia per il vettore?
056400120618
056500120619         // -?Box (& Logo & BarCode)?
056600120618         §Box1As = wPosIni + c_PosBox1As;
056700120618         §Box1Ai = wPosIni + c_PosBox1Bs + c_LineNarrow;
056800120618         §Box1Bs = wPosIni + c_PosBox1Bs;
056900120618         §Box1Bi = wPosIni + c_PosBox1Bi;
057000120618
057100120618         // -?Simil-Logo BRT?
057200120618         §PlblS  = §Box1As;
057300120618         §PlblVS = 7.581;
057400120618         exsr  sr_SimilLogoBRT_;
057500120618
057600120618         // -?Costanti dati anagrafici BRT?
057700120618         §PC0    = wPosIni + c_PosTBar;
057800120618         §PC1    = wPosIni + c_PosTBa1;
057900120618         §PC2    = wPosIni + c_PosTBa2x;
058000120618         §PC3    = wPosIni + c_PosTBa3x;
058100120618         §PC4    = wPosIni + c_PosTBa4x;
058200120618         §PC5    = wPosIni + c_PosTBa5x;
058300120618         §PC6    = wPosIni + c_PosTBa6x;
058400120618
058500120619         // -?Linee orizzontali nel box?
058600120618         §Lo1S1  = wPosIni + c_PosLin1oS1;
058700120618         §Lo1S2  = wPosIni + c_PosLin1oS2;
058800120618       //§Lo1S3  = wPosIni + c_PosLin1oS3;
058900120618         §Lo1S4  = wPosIni + c_PosLin1oS4;
059000120618         §Lo1S5  = wPosIni + c_PosLin1OS5;
059100120619         //§Lo1C1  = wPosIni + c_PosLin1oC1;
059200120618         §Lo1C2  = wPosIni + c_PosLin1oC2;
059300120618         §Lo1D1  = wPosIni + c_PosLin1oD1;
059400120618
059500120618         // -?Lunghezza linee verticali nel box in copia per il vett.?
059600120618         §LLv1A  = §Lo1S5  - §Lo1S1;
059700120618         §LLv1B  = §Lo1S4  - §Box1Bs;
059800120619         §LLv1C  = §Box1Bi - §Box1Bs;
059900120618
060000120618       ENDSR;
060100120619
060200120619      /end-free
060300120618
060400120618       //-------------------------------------------------------------
060500120618       //?Posizionamento del simil-logo BRT (unico)                   ?
060600120618       //-------------------------------------------------------------
060700120618      /copy gaitrasrc/srcTNSY,LogoBRT_R2
060800120618
060900120618      /free
061000120619
061100120619       //--------------------------------------------------------------
061200120619       //?Stampa dati DPD.                                             ?
061300120619       //--------------------------------------------------------------
061400120619       //BEGSR  sr_StampaDPD;
061500120619       //
061600120619       //  // -?Bar-Code?
061700120619       //  //  ?(1' 12 char di Riferimento Mittente Alfanumerico - se numerici)?
061800120619       //  //  ?nuovi controlli per nuovo barcode DPD?
061900120619       //  if  Prmn <> *zero;
062000120619       //    w15a = %editc( Prmn : 'X' );
062100120619       //    // -?controllo se vecchio o nuovo barcode?
062200120619       //    if  %subst( w15a : 14 : 1 ) = *blank;
062300120619       //      // - -?vecchio?
062400120619       //      Pbcd2 = '01 ' + %subst(w15a:1:3) + ' '   + %subst(w15a:4:8);
062500120619       //    else;
062600120619       //      // - -?nuovo?
062700120619       //      Pbcd2 = '01 ' + %subst(w15a:1:4) + ' '   + %subst(w15a:5:10);
062800120619       //    endif;
062900120619       //  endif;
063000120619       //
063100120619       //  // -?Impostazione data?
063200120619       //  PdpA = 'Date ' + %editc(Pdsp:'Y');
063300120619       //
063400120619       //  // -?Impostazione numero colli?
063500120619       //  PdpB = 'Parcel N. ' + %editc(Pncl:'Z');
063600120619       //
063700120619       //  // -?Impostazione numero parcel?
063800120619       //  PdpC = %char(Prmn);
063900120619       //
064000120619       //  // -?Impostazione "costante" da stampare tra le Note?
064100120619       //  PdpD = 'Controllare la corrispondenza';
064200120619       //  PdpE = 'del destinatario con l''indirizzo';
064300120619       //  PdpF = 'dell''etichetta DPD.';
064400120619       //
064500120619       //  // -?Impostaz. posizioni sulla copia per il vettore già stampata?
064600120619       //  §DP1 = wPosIni + 1.100;
064700120619       //  §DP2 = §DP1 + 0.550;
064800120619       //  §DP3 = §DP2 + 0.120;
064900120619       //  §DP4 = §DP3 + 0.120;
065000120619       //  §DP5 = §DP4 + 0.120;
065100120619       //  §DP6 = §DP5 + 0.160;
065200120619       //  §DP7 = §DP6 + 0.130;
065300120619       //  §DP8 = §DP7 + 0.130;
065400120619       //
065500120619       //  // -?(inutile testare l'overflow perchè siamo in ex-A5)?
065600120619       //  write  A4prtDPD;
065700120619       //
065800120619       //ENDSR;
065900120619
066000120619      /end-free
066100120619
066200120619       //--------------------------------------------------------------
066300120619       //?Definizione schiere a tempo di compilazione                  ?
066400120619       //--------------------------------------------------------------
066500120618
066600120619** -?$CLI?-?Dati anagrafici dei clienti (mit/des)?
066700120618ROSSI ERNESTO ....................*                 1
066800120618VIA DELLA FANTASIA, 99 ...........*                 2
06690012061840138                                               3
067000120618BOLOGNA ..........................*                 4
067100120618BO                                                  5
067200120618                                                    6
067300120618ARANCIONI ERMENEGILDO ............*                 7
067400120618VERDI GIUSEPPE ...................*                 8
067500120618(ESTENSIONE) .....................*                 9
067600120618VIA DELLA MUSICA, 10 .............*                10
06770012061837128                                              11
067800120618VERONA ...........................*                12
067900120618VR                                                 13
068000120618                                                   14
068100120618INZAGHI PLUTO ....................*                15
068200120618VIA DEI PAGLIACCI, 123 ...........*                16
06830012061840999                                              17
068400120618BOLOGNA ..........................*                18
068500120618BO                                                 19
068600120618                                                   20
068700120618KARNAK S.A (SM 00251)                              21
068800120618STRADA FONTESCARA 15                               22
06890012061847894                                              23
069000120618CHIESANUOVA                                        24
069100120618RN                                                 25
069200120618                                                   26
069300120619** -?$NOTE?                                   50>*                70>*?
069400120618TEST * NOTA 01 ....2....+....3....+....4....+....5
069500120618TEST * NOTA 02 ....2....+....3....+....4....+....5
069600120618TEST * PARTICOLARITA' 01 ....3....+....4....+....5
069700120618TEST * PARTICOLARITA' 02 ....3....+....4....+....5
069800120618TEST * PARTICOLARITA' 03 ....3....+....4....+....5
069900120618TEST * PARTICOLARITA' 04 ....3....+....4....+....5
070000120618TEST ....1....+....2....+....3....+....4....+....5
