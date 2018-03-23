000100101201     /*END
000200030623      *---------------------------------------------------------------*
000300110426      *?- - - - - - - - - - - -   P R O V A   - - - - - - - - - - - -?*
000400110426      *?                S T A M P A   S U   L A S E R                ?*
000500110426      *?             L E T T E R A   D I   V E T T U R A             ?*
000600030623      *---------------------------------------------------------------*
000700030623
000800080704     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000900110426     h dftactgrp(*no)
001000130628     h bnddir('TRUL')
001100030623
001200030623      *---------------------------------------------------------------*
001300030623
001400030623     fAZORG01L  if   e           k disk
001500030912      *
001600130710     fFNLSB6P   o    e             printer usropn infds(prtINFOa4)
001700080605     f                                     rename(LSB6POS :A4prtPOS )
001800130418     f                                     rename(LSB6BOX :A4prtBOX )
001900120530     f*//                                  rename(LSB6LOGBAR:A4LogoBAR)
002000120530     f*//                                  rename(LSB6LOGDA1:A4LogoDA1)
002100120530     f*//                                  rename(LSB6LOGBR2:A4LogoBR2)
002200120530     f*//                                  rename(LSB6LOGDA2:A4LogoDA2)
002300120530     f                                     rename(LSB6LOGBRT:A4LogoBRT)
002400120530     f                                     rename(LSB6LOGDA3:A4LogoDA3)
002500130418     f                                     rename(LSB6LDV :A4prtLDV )
002600080605     f                                     rename(LSB6LDVB:A4prtLDVB)
002700080922     f                                     rename(LSB6DPD :A4prtDPD )
002800080605     f                                     rename(LSB6FED1:A4prtFED1)
002900080605     f                                     rename(LSB6FED2:A4prtFED2)
003000080605     f                                     rename(LSB6FED3:A4prtFED3)
003100080605     f                                     rename(LSB6AR51:A4prtAR51)
003200080605     f                                     rename(LSB6AR52:A4prtAR52)
003300080605     f                                     rename(LSB6AR53:A4prtAR53)
003400080605     f                                     rename(LSB6AR54:A4prtAR54)
003500080605     f                                     rename(LSB6AR5A:A4prtAR5A)
003600151118     f*//                                  rename(LSB6AR5B:A4prtAR5B)
003700151118     f*//                                  rename(LSB6AR5C:A4prtAR5C)
003800080605     f                                     rename(LSB6AR5G:A4prtAR5G)
003900080605     f                                     rename(LSB6DB0 :A4prtDB0)
004000080605     f                                     rename(LSB6DT0 :A4prtDT0)
004100080605     f                                     rename(LSB6DT1 :A4prtDT1)
004200080605     f                                     rename(LSB6DT2 :A4prtDT2)
004300080605     f                                     rename(LSB6DC1 :A4prtDC1)
004400080605     f                                     rename(LSB6DK0 :A4prtDK0)
004500080605     f                                     rename(LSB6DK1 :A4prtDK1)
004600080605     f                                     rename(LSB6DK2 :A4prtDK2)
004700080605     f                                     rename(LSB6DK3 :A4prtDK3)
004800080605     f                                     rename(LSB6DK4 :A4prtDK4)
004900080605     f                                     rename(LSB6DK5 :A4prtDK5)
005000090406     f                                     rename(LSB6DLL :A4prtDLL)
005100080605     f                                     rename(LSB6DL0 :A4prtDL0)
005200080605     f                                     rename(LSB6DL0A:A4prtDL0A)
005300080605     f                                     rename(LSB6DL0B:A4prtDL0B)
005400080605     f                                     rename(LSB6DL0C:A4prtDL0C)
005500080605     f                                     rename(LSB6DL0D:A4prtDL0D)
005600080605     f                                     rename(LSB6DL0E:A4prtDL0E)
005700080605     f                                     rename(LSB6DL0F:A4prtDL0F)
005800080605     f                                     rename(LSB6DL1A:A4prtDL1A)
005900080605     f                                     rename(LSB6DL1B:A4prtDL1B)
006000080605     f                                     rename(LSB6DL1C:A4prtDL1C)
006100080605     f                                     rename(LSB6DL1D:A4prtDL1D)
006200080605     f                                     rename(LSB6DL1E:A4prtDL1E)
006300080605     f                                     rename(LSB6DL2 :A4prtDL2)
006400080605     f                                     rename(LSB6DL21:A4prtDL21)
006500080605     f                                     rename(LSB6DL3 :A4prtDL3)
006600080605     f                                     rename(LSB6DL4 :A4prtDL4)
006700080605     f                                     rename(LSB6DL5 :A4prtDL5)
006800080605     f                                     rename(LSB6DL6 :A4prtDL6)
006900080605     f                                     rename(LSB6DL7 :A4prtDL7)
007000080605     f                                     rename(LSB6DL8 :A4prtDL8)
007100080605     f                                     rename(LSB6DL9 :A4prtDL9)
007200080605     f                                     rename(LSB6AC0 :A4prtAC0)
007300080605     f                                     rename(LSB6AC0A:A4prtAC0A)
007400080605     f                                     rename(LSB6AC0B:A4prtAC0B)
007500080605     f                                     rename(LSB6AC1A:A4prtAC1A)
007600080605     f                                     rename(LSB6AC1B:A4prtAC1B)
007700080605     f                                     rename(LSB6AC2 :A4prtAC2)
007800080605     f                                     rename(LSB6AC3 :A4prtAC3)
007900080605     f                                     rename(LSB6AC4 :A4prtAC4)
008000080605     f                                     rename(LSB6ES0 :A4prtES0)
008100080605     f                                     rename(LSB6ES0A:A4prtES0A)
008200080605     f                                     rename(LSB6ES0B:A4prtES0B)
008300080605     f                                     rename(LSB6ES1A:A4prtES1A)
008400080605     f                                     rename(LSB6ES1B:A4prtES1B)
008500080605     f                                     rename(LSB6ES2 :A4prtES2)
008600080605     f                                     rename(LSB6ES3 :A4prtES3)
008700080605     f                                     rename(LSB6ES4 :A4prtES4)
008800080605     f                                     rename(LSB6SP0 :A4prtSP0)
008900080605     f                                     rename(LSB6SP1 :A4prtSP1)
009000080605     f                                     rename(LSB6NUL :A4prtNUL)
009100080605     f                                     rename(LSB6GEN :A4prtGEN)
009200080605     f                                     rename(LSB6LO  :A4prtLO)
009300080605     f                                     rename(LSB6SEG :A4prtSEG)
009400080605     f                                     rename(LSB6MSG :A4prtMSG)
009500090406     f*****                                rename(LSB6ROWNR:A4prtROWNR)
009600030623
009700030623      *---------------------------------------------------------------*
009800030623
009900030623      *
010000110426      *?  C O S T A N T I   - - - - - - - - - - - - - - - - - - - - - *?
010100031104      *                                           AaLnpSr»»»»NspK
010200091119     d C_bar128        c                   const('101029812345673')
010300030623     d $Lungh          c                   const('*...+....1....+....2....+....-
010400031104     d                                     3....+....4....+....5....+....6....+-
010500031104     d                                     ....7....+....8....+....A')
010600030623     d $LunghN         c                   const(123456789012345678901234567890)
010700030623     d $LunghA         c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZABC-
010800030623     d                                     DEFGHIJKLMNOPQRSTUVWXYZ')
010900030623     d $LunghW         c                   const('WWWWWWWWWWWWWWWWWWWWWWWWWWWWW-
011000030623     d                                     WWWWWWWWWWWWWWWWWWWWWWW')
011100120530     d*// $Bartolini      c                   const('BARTOLINI S.p.A.')
011200110426     d $BRT            c                   const('BRT S.p.A.')
011300060912     d $Intest1        c                   const('Sede Operativa ed Am-
011400060912     d                                     ministrativa:')
011500080606     d $Intest2        c                   const('via E. Mattei, 42 - -
011600110426     d                                     40138 Bologna')
011700060912     d $Intest3        c                   const('Sede Legale: Piazza -
011800060912     d                                     Diaz, 7 - 20123 Milano')
011900060912     d $Intest4x       c                   const('P.IVA 03987100371')
012000070215     d $Intest4        c                   const('C.F. e P.IVA 04507990150')
012100070215     d $Intest5x       c                   const('Capitale Sociale Eur-
012200060912     d                                     o 100.000.000 int. vers.')
012300070215     d $Intest5        c                   const(' Capitale Sociale Eu-
012400070215     d                                     ro 24.000.000 int. vers.')
012500060912     d $Intest6        c                   const('info on-line')
012600120530     d*// $Intest7        c                   const('www.bartolini.it')
012700120530     d $Intest7_brt    c                   const('www.brt.it')
012800080703      *
012900080703      * ----------------------------------
013000080605      *
013100110426      * -?Posizionamento all'inizio della pag. successiva?
013200080711     d c_§PS1          c                   const(0.950)
013300110426      * -?Lunghezza (in inches) di una singola LdV?
013400080703     d $LenLdV2        c                   const(5.775)
013500080704     d $LenLdV4        c                   const(2.890)
013600080924     d $LenLdVp        c                   const(5.850)
013700110426       // -?Lunghezza di 1/4 di A4?
013800080924     d c_Len2_inch     c                   const(5.850)
013900080924     d c_Len2_row8     c                   const(047)
014000080605      *
014100110426      * -?Spessore delle linee in inches/pollici (*memo)?
014200080702     d c_LineNarrow    c                   const(0.008)
014300080702     d c_LineMedium    c                   const(0.017)
014400080702     d c_LineWide      c                   const(0.025)
014500080702      *
014600120530      * -?Posizioni dell'intestazione "BRT S.p.A." nella 1ª copia?
014700080702     d c_PosTBar       c                   const(0.270)
014800080702     d c_PosTBa1       c                   const(0.350)
014900110426     d c_PosTBa2       c                   const(0.420)
015000110426     d c_PosTBa3       c                   const(0.490)
015100110426     d c_PosTBa4       c                   const(0.560)
015200110426     d c_PosTBa5       c                   const(0.630)
015300110426     d c_PosTBa6       c                   const(0.710)
015400110426     d c_PosTBa2x      c                   const(0.430)
015500110426     d c_PosTBa3x      c                   const(0.500)
015600110426     d c_PosTBa4x      c                   const(0.590)
015700110426     d c_PosTBa5x      c                   const(0.660)
015800110426     d c_PosTBa6x      c                   const(0.750)
015900080702      *
016000110426      * -?Posizioni dei bordi orizzontali superiori box nella 1ª copia?
016100110426      *  ?(distanza dal lato superiore dell'A4)?
016200110426      *  ?N.B.?- c_PosBoxNXs => posizione iniziale della linea?
016300110426      *                        ?orizzontale Superiore?
016400110426      *       ?- c_PosBoxNXi => posizione iniziale della linea?
016500110426      *                        ?orizzontale Inferiore?
016600080702     d c_PosBox1As     c                   const(0.200)
016700080702     d c_PosBox1Bs     c                   const(0.790)
016800080704     d c_PosBox1Bi     c                   const(2.850)
016900080702     d c_PosBox1Cs     c                   const(1.950)
017000100208     d*// c_PosBox1Ds     c                   const(2.555)
017100100208     d c_PosBox1Ds     c                   const(2.453)
017200100208     d*// c_PosBox1Di     c                   const(2.480)
017300080702      *
017400110426      * -?Posizioni di altre linee orizzontali nella 1ª copia?
017500080702     d c_PosLin1OS1    c                   const(1.350)
017600080702     d c_PosLin1OS2    c                   const(1.790)
017700080702     d c_PosLin1OS3    c                   const(2.010)
017800080702     d c_PosLin1OS4    c                   const(2.230)
017900091119     d c_PosLin1OS5    c                   const(2.470)
018000080702     d c_PosLin1OC1    c                   const(1.680)
018100080702     d c_PosLin1OC2    c                   const(1.240)
018200140915     d c_PosLin1OC3    c                   const(1.470)
018300080702     d c_PosLin1OD1    c                   const(1.010)
018400080702      *
018500110426      * -?Posizioni di altre costanti nella 1ª copia?
018600090217     ***d c_PosKSerEspWu  c                   const(0.900)
018700140917     d*// c_PosKBRTc      c                   const(0.970)     x FONT(00404)
018800140922     d c_PosKBRTc      c                   const(0.965)
018900140922     d*// c_PosKBRTc      c                   const(0.955)     x FONT(02305)
019000080702     d c_PosKTass      c                   const(2.130)
019100080702     d c_PosKRCV       c                   const(1.100)
019200080702     d c_PosKBAV       c                   const(1.850)
019300090515       //
019400110426       // -?Dimensioni base per i simil-loghi Bartolini e BRT?
019500120530      *///copy gaitrasrc/srcTNSY,LogoBAR_R0
019600120530      *///copy gaitrasrc/srcTNSY,LogoBRT3R0
019700120530      *///copy gaitrasrc/srcTNSY,LogoBRT_R0
019800120530      /copy gaitrasrc/srcTNSY,LogoBRT_R1
019900080605      *
020000080605      * ----------------------------------
020100080605      *
020200110426       // -?Posizione minima superiore per FedEx?
020300030912     d $MinLine1       c                   const(012)
020400030912     d $MinLine2       c                   const(032)
020500110426       // -?Limite righe stampabili su 1/4 di A4 in 1/9"?
020600110426     d $MaxLine1       c                   const(025)
020700110426       // -?Limite righe stampabili su 1/2 A4 in 1/8" per?
020800110426       //  ?Karnak/Titanedi/EDk + Arcte + Esprinet +?
020900110426       //  ?Logistica/Sait + Generico (con/senza logo)?
021000110426     d $MaxLine2       c                   const(068)
021100110426     d $LimOF          c                   const(08.250)
021200031203     d k_ROW           c                   const(06.015)
021300031203     d w_ROW           c                   const(00.125)
021400031203     d $LimOF_K        c                   const(02.525)
021500110426      * -?Costanti in stampa?
021600030915     d C_ScNS          c                   const('ELENCO SEGNACOLLI -
021700030915     d                                     NON SEQUENZIALI')
021800030915     d C_CoOr          c                   const('COLLI ORIGINALI     -
021900030915     d                                     N. ')
022000151118
022100151118       // -?Comandi da eseguire?
022200151118     d c_CmdOvrPrtF    c                   const('OVRPRTF +
022300151118     d                                           file(FNLSB6P) +
022400151118     d                                           devtype(*AFPDS) +
022500151118     d                                           usrdta(''TEST_PDF'') +
022600151118     d                                           tostmf(''+
022700151118     d                                             /temp/pdf_aa/+
022800151118     d                                             FNLSB6P.pdf'') +
022900151118     d                                           wscst(*PDF) +
023000151118     d                                           ovrscope(*calllvl) +
023100151118     d                                           ')
023200151118     d c_CmdDltOvr     c                   const('DLTOVR +
023300151118     d                                            file(FNLSB6P) +
023400151118     d                                            lvl(*)')
023500030623      *
023600110426      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *?
023700030623      *
023800031105     d $Cli            s             50    dim(26) ctdata perrcd(1)
023900030915     d $Note           s             90    dim(32) ctdata perrcd(1)
024000060912     ***d $Kcpr           s              6  0 dim(50) ctdata perrcd(1)
024100060912     ***d $Kdpr           s             50    dim(50) alt($Kcpr)
024200060912     ***d $Kqta           s              6  0 dim(50) ctdata perrcd(1)
024300060912     ***d $Knot           s              6    dim(50) alt($Kqta)
024400060912     ***d $Ktxt           s            100    dim(12) ctdata perrcd(1)
024500110426      * -?Note e Particolarità?
024600030915     d $NP             s                   dim(6)  inz  like(Pnp1)
024700110426      * -?Segnacolli NON sequenziali (memo-1- e stampa-2-)?
024800030912     d $SC1            s              7  0 dim(14) inz
024900030912     d $SC2            s              8    dim(14) inz
025000110426      * -?Quantità (editate) - a sx - e Unità di misura - a dx -?
025100030915     d $QUM            s             20    dim(3)  inz
025200030623      *
025300110426      *?  S T R U T T U R E - D A T I   - - - - - - - - - - - - - - - *?
025400030623      *
025500030623     d KPJBA         e ds
025600030623      *
025700110426      * -?Parametri x Controllo profilo utenti?
025800040223     d TIBS34ds      e ds
025900110426      * -?Ds di riferimento al file esterno AZUTE00F?
026000040223     d AZUTEds       e ds                  extname(AZUTE00F)
026100110426      * -?Ds per dati organigramma?
026200040223     d dDATIUTE      e ds
026300130628       //
026400130628       // -?Reperimento dati tabelle?
026500130628      /copy gaitrasrc/srcProtoPI,TRULTAB
026600130628      /copy gaitrasrc/srcProtoPR,TRULTAB
026700130628       // -?Tab. "GED" = Gestionale: divise?
026800130628     d dGED          e ds                  inz
026900130628       // -?Tab. "GEI" = Gestionale: importi vari?
027000130628     d dGEI          e ds                  inz
027100080715       //
027200110426       // -?Parametri per TRUL28R (calcolo check digit del barcode)?
027300080715     d TRUL28ds      e ds                  inz
027400080715     d  I28tla       e                     inz('L')
027500080715     d  I28mod       e                     inz('BAR')
027600080715       //
027700080715     d BARcodeDS       ds            17    inz
027800080715     d  BARcdeAAS                     2s 0 inz
027900080715     d  BARcdeLNP                     3s 0 inz
028000080715     d  BARcdeNRS                     2s 0 inz
028100080715     d  BARcdeNSP                     7s 0 inz
028200080715     d  BARcdeXXX                     3    inz
028300030623      *
028400030912     d prtINFOa4       ds
028500080627     d  CurLineA4            367    368i 0
028600030623      *
028700030623     d Status         sds           333
028800030623     d  SDSpgm           *proc
028900030623     d  SDSprm           *parms
029000030623     d  SDSdta               191    198
029100030623     d  SDSjob               244    253
029200030623     d  SDSusr               254    263
029300030623      *
029400110426      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *?
029500030623      *
029600030915     d $First          s              1    inz(*on)
029700101201     d $Fiar5          s               n   inz(*off)
029800080624     d W11d2           s             11  2 inz
029900030623     d W15a            s             15    inz
030000030623     d Wsc             s              7  0 inz(1234566)
030100030623     d wElSNS          s              2  0 inz(09)
030200031203     d wR_AR5          s              3  0 inz
030300070308     d wCOUNT          s              3  0 inz
030400030623     d wMinLine        s              3  0 inz
030500030623     d wMaxLine        s              3  0 inz
030600031103     d WX              s              3  0 inz
030700030623     d XX              s              3  0 inz
030800030623     d YY              s              3  0 inz(100)
030900030623     d wPage           s              4  0 inz(0001)
031000080715     d wwwPag          s              4  0 inz(0001)
031100080702     d wNrCpy          s              3  0 inz(002)
031200080715     d wBar128         s             15    inz
031300101201     d wDateDMY        s               d   datfmt(*dmy) inz
031400130418
031500130418       // -?Campi utilizzati per impostazione dell'output in stampa?
031600140922     d Pdsp            s              8  0 inz
031700130418     d Tla             s             50    inz
031800130418      *
031900080703      *
032000080703      * ----------------------------------
032100080703      *
032200110426      * -?Lunghezza (in inches) di una singola LdV?
032300080703     ***d*$Len1LdV****    c                   const(2.925)
032400080703     ***d $Len1LdV        c                   const(5.850)
032500080703     d $Len1LdV        s              5  3 inz(5.775)
032600110426      * -?Numero delle LdV (box) per pagina "A4"?
032700110426      *  ?(impostando - PER ORA - la copia per il destinatario tanto?
032800110426      *   ?lunga quanto quella per il corriere, ci staranno 2 LdV in?
032900110426      *   ?1 A4; quando si stamperanno solo le copie per il destina-?
033000110426      *   ?tario, ce ne staranno 4)?
033100080703     d $LdVxP          s              1  0 inz(2)
033200080605      * - Variabili per il calcolo delle posizioni per box in A4
033300080703     d wNumLdV         s              5  0 inz
033400080605     d wSeqLdV         s              1  0 inz
033500080605     d wPosIni         s              5  3 inz
033600080715
033700080715       //--------------------------------------------------------------
033800080715       //?Definizione procedure usate.
033900080715       //--------------------------------------------------------------
034000080715
034100110426       // -?Calcolo check-digit per bar-code?
034200080715     d trul28r1        pr                  extpgm('TRUL28R1')
034300080715     d   trul28ds...
034400080715     d                                     likeds(TRUL28ds)
034500140915
034600140915       // -?Calcolo/Controllo BRTcode?
034700140915      /copy gaitrasrc/srcProtoPI,TRUL28R0
034800140915      /copy gaitrasrc/srcProtoPR,TRUL28R0
034900151118
035000151118       // -?Parametri API QCAPCMD (Process Commands)?
035100151118     d Qcmd            s           2048    inz  varying
035200151118      /copy qSysInc/qRpgleSrc,QCAPCMD
035300151118       // -?API QCAPCMD (Process Commands)?
035400151118      /copy gaitrasrc/srcProtoPR,QCAPCMD
035500151118
035600151118       // -?Parametri gestione errori API.?
035700151118      /copy qsysinc/qrpglesrc,QUSEC
035800030623
035900030623      *---------------------------------------------------------------*
036000030623
036100030623      *
036200110426      *  ?D E F I N I Z I O N E   K E Y - L I S T   - - - - - - - - - *?
036300030623      *
036400030623
036500030623      *---------------------------------------------------------------*
036600110426      *?RIEPILOGO INDICATORI                                         ?*
036700030623      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
036800110426      *?18   ?- Stampa DATA e ORA CONSEGNA RICHIESTA                  *
036900110426      *?19   ?- Stampa IMPONIBILE, IVA, TOTALE ed ESTREMI FATTURA     *
037000110426      *?20   ?- Stampa CONTRASSEGNO                                   *
037100110426      *?21   ?- Stampa TOTALE DA ESIGERE ALLA CONSEGNA / AL RITIRO    *
037200110426      *?22   ?- Stampa NOTE (se NO imponibile e fattura)              *
037300110426      *?31   ?- di comodo                                             *
037400130418      *?33off?- Stampa della COPIA PER IL CORRIERE                    *
037500130418      *?33on ?- Stampa della COPIA PER IL DESTINATARIO                *
037600130418      *         (o per il Mittente, se P.F. Prepagato)                *
037700110426      *?35   ?- Richiesta FIRMA, DATA e ORA   in 1ª copia             *
037800030912      *         per ricevuta della merce senza riserve                *
037900030912      *         e indica "TOTALE DA ESIGERE ALLA CONSEGNA"            *
038000110426      *?36   ?- Richiesta FIRMA per QUIETANZA in 2ª copia             *
038100030912      *         e indica "TOTALE DA ESIGERE AL RITIRO"                *
038200110426      *?37   ?- Condiziona HIGHLIGHT nel campo PDS2  in rec. PRTLDV1  *
038300110426      *                                  per servizio "ESPRESSO"      *
038400110426      *                                o per servizio "POSTE IT."     *
038500110426      *?38   ?- Condiziona FONT      nel campo PAR5  in rec. PRTAR52  *
038600110426      *?39   ?- Condiziona UNDERLINE nel campo PRIG  in rec. PRTGEN   *
038700110426      *                            e nel campo PCTXX in rec. PRTDK5   *
038800110426      *                            e nel campo PCTXT in rec. PRTDTx   *
038900110426      *                            e nel campo PCTXT in rec. PRTDDx   *
039000110426      *?41   ?- Stampa copia per il corriere della 1ª LdV             *
039100110426      *?42   ?- Stampa copia per il destinatario della 1ª LdV         *
039200110426      *?43   ?- Stampa copia per il corriere della 2ª LdV             *
039300110426      *?44   ?- Stampa copia per il destinatario della 2ª LdV         *
039400110426      *?45off?- Posiziona l'inizio della packing-list al 2/4 dell'A4  *
039500110426      *         (packing-list di 2 LdV per A4)                        *
039600110426      *?45on ?- Posiziona l'inizio della packing-list in cima all'A4  *
039700110426      *         (1ª packing-list in A4 dopo la stampa di 4 LdV x A4)  *
039800110426      *?46on ?- Posiziona l'inizio della packing-list a metà dell'A4  *
039900110426      *         (2ª packing-list in A4 dopo la stampa di 4 LdV x A4)  *
040000120530      *?50   ?- Stampa CODICE A BARRE - BRT                           *
040100110426      *?51   ?- Stampa CODICE A BARRE - DPD                           *
040200110426      *?52   ?- Stampa CODICE A BARRE - O.R.M.                        *
040300120530      *?55   ?- NON stampa logo BRT in DDT della Logistica =>         *
040400120530      *         => Stampa il simil-logo in LSB6LOGBRT                 *
040500110426      *?69off?- Stampa su modulo in formato "A4"                      *
040600110426      *?69on ?- Stampa su modulo in formato "A5"                      *
040700120530      *?70   ?- Trattasi di BOLLA BRT                                 *
040800110426      *?71   ?- Trattasi di BOLLA DPD                                 *
040900110426      *?72   ?- Trattasi di BOLLA FedEx                               *
041000110426      *?73   ?- Trattasi di BOLLA EuroExpress con particolarità "CD"  *
041100030912      *===============================================================*
041200030623
041300030623      *
041400101201      *   M A I N - L I N E   - - - - - - - - - - - - - - - - - - - - *
041500030623      *
041600030623     c     *Entry        plist
041700030623     c                   parm                    £Mod              1
041800080624     c                   parm                    £Part             1
041900030623     c                   parm                    £NrRow           15 5
042000030623     c                   parm                    £NrCpy           15 5
042100110426      *?£Mod   - tipo modulo: '1' = modulo A4
042200110426      *?                      '2' = modulo ex A5  [A4/2] (*default)
042300110426      *?                      '4' = "tagliatella" [A4/4]
042400110426      *?                      'D' = imposta modulo DPD
042500110426      *?***                   'E' = imposta modulo EUROEXPRESS
042600110426      *?                      'F' = imposta modulo FEDEX
042700110426      *?                      'I' = imposta modulo +bancali +supermercato
042800110426      *?                                           +promemoria
042900110426      *?                      'P' = imposta modulo +promemoria (*all fonts)
043000110426      *?                      'R' = imposta modulo +ritiro_contestuale_alla_cons
043100110426      *?                      'r' = imposta modulo +bancali +supermercato
043200110426      *?                                           +promemoria
043300110426      *?                                           +ritiro_contestuale_alla_cons
043400110426      *?                      's' = imposta lista segnacolli non sequenziali su
043500110426      *?                            mod.A5
043600110426      *?                      'S' = imposta lista segnacolli non sequenziali su
043700110426      *?                            mod.A4
043800110426      *?                      'Q' = imposta lista segnacolli non sequenziali tra
043900110426      *?                            le note del mod.A5 o su mod.A4 a seconda di
044000110426      *?                            quanti sono (vedi parm £NrRow).
044100151118      *?                      'X' = converte spool in PDF
044200110426      *?£Part  - particolarità: '1' = imposta richiesta firma per ritiro
044300110426      *?                        '2' = imposta richiesta firma per consegna
044400110426      *?                        '9' = imposta entrambe le richieste di firma
044500110426      *?                        'A' = non stampa note/particolarità
044600110426      *?                        'B' = non stampa fattura/contrassegno/importo da
044700110426      *?                              esigere
044800120530      *?                        'C' = non stampa bar-code BRT
044900110426      *?                        'E' = non stampa i.v.a. (stampa esenzione)
045000110426      *?                        'F' = non stampa bancali da rendere al mittente
045100110426      *?                        'G' = non stampa note x destinatario supermercat
045200110426      *?                        'H' = non stampa promemoria x autotrasportatore
045300110426      *?                        'Z' = non stampa nè note nè fattura nè importi
045400110426      *?                              nè box bancali/supermercato/promemoria
045500120530      *?                              nè bar-code BRT
045600110426      *?                        '*' = stampa messaggio '***'
045700110426      *?£NrRow - num. righe da stampare per Segnacolli-FedEx o Supermercato-Fiar
045800110426      *?         oppure num. colli (non sequenziali) da stampare
045900110426      *?£NrCpy - num. copie da stampare
046000030912      *
046100040223     c     *dtaara       define    §azute        AZUTEds
046200040223     c     *dtaara       define    §datiute      dDATIUTE
046300040223     c                   in(E)     *dtaara
046400040223     c                   if        %ERROR or RSUT = *blanks
046500040223     c                   clear                   Tibs34Ds
046600040223     c                   call      'TIBS34R'
046700040223     c                   parm                    Tibs34Ds
046800040223     c                   in        *dtaara
046900040223     c                   endif
047000130628      *
047100130628      /free
047200130628
047300130628       If  getTabella ( c_Tntbe : 'GED' : '1         ' :
047400130628                          *omit : *blank : *blank :
047500130628                          *omit : *omit :
047600130628                          *omit : *omit : *omit : *omit :
047700130628                          *omit : *omit : *omit : *omit :
047800130628                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
047900130628                      = *zero      AND
048000130628           ds_TNTBE.TBEatb = *blank;
048100130628
048200130628         dGED = ds_TNTBE.TBEuni;
048300130628
048400130628         if  getTabella ( c_Tntbe : 'GEI' : §GEDcn :
048500130628                            *omit : *blank : *blank :
048600130628                            *omit : *omit :
048700130628                            *omit : *omit : *omit : *omit :
048800130628                            *omit : *omit : *omit : *omit :
048900130628                            ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
049000130628                        = *zero      AND
049100130628             ds_TNTBE.TBEatb = *blank;
049200130628           dGEI = ds_TNTBE.TBEuni;
049300130628         endif;
049400130628
049500130628       EndIf;
049600130628
049700130628      /end-free
049800030623      *
049900080703     c                   reset                   $LdVxP
050000080703sel 1c                   select
050100080703w   1c                   when      SDSprm >  3
050200030623     c                   eval      wNrCpy =  £NrCpy
050300080703w   1c                   when      SDSprm >  *zero   and
050400080703     c                             £Mod   =  '4'
050500080703     c                   eval      $LdVxP =  4
050600080703     c                   eval      wNrCpy =  4
050700080703w   1c                   when      SDSprm >  *zero   and
050800151118     c                             ( £Mod =  '1'     or
050900151118     c                               £Mod =  'X' )
051000080703     c                   eval      $LdVxP =  1
051100080703     c                   eval      wNrCpy =  1
051200080703x   1c                   other
051300030623     c                   reset                   wNrCpy
051400080703e   1c                   endsl
051500030623      *
051600110426      * -?Imposto il numero di elementi della schiera per stampa dei?
051700110426      *  ?segnacolli non sequenziali?
051800030623     c                   eval      wElSNS = %elem($SC1)
051900030915      *
052000110426      * -?Pulisco il PRTF?
052100030915     c                   exsr      ClrPRTF
052200030623      *
052300110426      * -?Controllo il tipo bolla da stampare?
052400080606     c                   exsr      TipoLDV
052500030623      *
052600110426      * -?Stampa?
052700030623     c                   exsr      STAMPA
052800030623      *
052900110426      * -?Fine?
053000030623     c     Fine          tag
053100130710     c                   close     FNLSB6P
053200151118     c                   if             SDSprm >  *zero
053300151118     c                             and  £Mod   = 'X'
053400151118     c                   exsr      sr_DltOvrPDF
053500151118     c                   endif
053600030623     c                   eval      *inLR  = *on
053700030623
053800030623      *---------------------------------------------------------------*
053900030623
054000030623      *
054100101201      *   S U B R O U T I N E S   - - - - - - - - - - - - - - - - - - *
054200030623      *
054300030912
054400030912      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
054500030912
054600030912     c     ClrPRTF       BEGSR
054700030912      *
054800130418     c                   clear                   A4prtBOX
054900120530     c*//                clear                   A4LogoBAR
055000120530     c*//                clear                   A4LogoDA1
055100120530     c*//                clear                   A4LogoBR2
055200120530     c*//                clear                   A4LogoDA2
055300120530     c                   clear                   A4LogoBRT
055400120530     c                   clear                   A4LogoDA3
055500130418     c                   clear                   A4prtLDV
055600080922     c                   clear                   A4prtDPD
055700030912     c                   clear                   A4prtFED1
055800030912     c                   clear                   A4prtFED2
055900030912     c                   clear                   A4prtFED3
056000030912     c                   clear                   A4prtAR51
056100030912     c                   clear                   A4prtAR52
056200030912     c                   clear                   A4prtAR53
056300030912     c                   clear                   A4prtAR5A
056400040223     c*** = AR5A         clear                   A4prtAR5B
056500040223     c*** = AR5A         clear                   A4prtAR5C
056600040223     c*** = AR5A         clear                   A4prtAR5D
056700040223     c*** = AR5A         clear                   A4prtAR5E
056800030912     c*** no campi...    clear                   A4prtSP0
056900030912     c*** no campi...    clear                   A4prtNUL
057000030912     c                   clear                   A4prtGEN
057100030912     c                   clear                   A4prtSEG
057200030912     c*** no campi...    clear                   A4prtSEP
057300030912     c                   clear                   A4prtMSG
057400090406     c*****              clear                   A4prtRowNr
057500030912      *
057600030915     c                   reset                   $Fiar5
057700030912     c                   reset                   $First
057800080605     c                   reset                   wPage
057900080703     c*** no !!!         clear                   wNumLdV
058000080605     c*** no !!!         clear                   wSeqLdV
058100030912      *
058200030912     c                   clear                   $NP
058300030912     c                   clear                   $SC1
058400030912     c                   clear                   $SC2
058500030912     c                   clear                   $QUM
058600030912      *
058700030912     c                   movea     *zeros        *in(01)
058800030912      *
058900030912     c                   ENDSR
059000030915
059100030915      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
059200030915
059300080606     c     TipoLDV       BEGSR
059400030915      *
059500110426      * -?Tipo modulo (per la separazione dei box: vettore/destinatario)?
059600110426      *  ?(*in69 = formato A5 / not *in69 = formato A4)?
059700030915     c                   eval      *in69  = *on
059800030915     c                   eval      wMaxLine = $MaxLine1
059900030915      *
060000110426      * -?Tipo bolla (per richiesta firma per ricevuta della merce?
060100110426      *  ?senza riserve)?
060200120530      *  ?(70 = BRT, 71 = DPD, 72 = FedEx, 73 = EuroExpress)?
060300040223     c                   movea     '1000'        *in(70)
060400030915      *
060500110426      * -?Definizione del formato pagina da stampare?
060600110426      *  ?('2' = formato A5 / '1' = formato A4 / 'Q' = ...da vedere)?
060700030915sel 1c                   SELECT
060800030915      *
060900080715w   1c                   WHEN          SDSprm >  *zero
061000080704     c                             and (£Mod  = '2'
061100151118     c                              or  £Mod  = 's')
061200151118      *
061300151118w   1c                   WHEN          SDSprm >  *zero
061400151118     c                             and  £Mod  = 'X'
061500151118     c                   exsr      sr_OvrToPDF
061600030915      *
061700080715w   1c                   WHEN          SDSprm >  *zero
061800080704     c                             and (£Mod  = '1'
061900030915     c                              or  £Mod  = 'D'
062000030915     c                              or  £Mod  = 'F'
062100040223     c                              or  £Mod  = 'E'
062200030915     c                              or  £Mod  = 'I'
062300070308     c                              or  £Mod  = 'P'
062400030915     c                              or  £Mod  = 'R'
062500030915     c                              or  £Mod  = 'r'
062600030915     c                              or  £Mod  = 'S')
062700030915     c                   eval      wMaxLine = $MaxLine2
062800031104sel 2c                   select
062900031104w   2c                   when      £Mod = 'D'
063000081127     c                   eval      *in69  = *on
063100040223     c                   movea     '0100'        *in(70)
063200031104w   2c                   when      £Mod = 'F'
063300081127     c                   eval      *in69  = *off
063400040223     c                   movea     '0010'        *in(70)
063500040223w   2c                   when      £Mod = 'E'
063600081127     c                   eval      *in69  = *off
063700040223     c                   movea     '0001'        *in(70)
063800031104e   2c                   endsl
063900081127      /free
064000081127         select;
064100081127
064200110426           // >?Promemoria per l'autotrasportatore?
064300110426           when  SDSprm > *zero   and   (£Mod  = 'P');
064400081127             eval *in69 = *off;
064500081127
064600110426           // >?dati FIAR5?
064700110426           when  SDSprm > *zero   and   (£Mod  = 'I'  or
064800081127                                         £Mod  = 'r');
064900081127             *in69 = *off;
065000081127
065100110426           // >?Riferimento O.R.M. per ritiro contestuale alla consegna?
065200110426           when  SDSprm > *zero   and   (£Mod  = 'R'  or
065300081127                                         £Mod  = 'r');
065400081127             *in69 = *off;
065500081127
065600110426           // >?elenco SEGNACOLLI NON SEQUENZIALI?
065700110426           //  ?(no x DPD   perchè monocollo)?
065800110426           //  ?(no x FedEx perchè segnacolliamo noi)?
065900110426           //  ?(no x "A5"  perchè sono già stati elencati tra le "Note e?
066000110426           //              ?Particolarità")?
066100110426           when  SDSprm  > *zero   and   (£Mod   = 'S'  or
066200081127                                          £Mod   = 's'  or
066300081127                                          £Mod   = 'Q'   and
066400081127                                          wElSNS > 4);
066500081127             *in69 = *off;
066600081127
066700081127         endsl;
066800081127      /end-free
066900081127      *
067000030915w   1c                   WHEN          SDSprm >  0
067100030915     c                             and £Mod   = 'Q'
067200031203if  2c                   if            SDSprm >  2
067300030915     c                             and £NrRow >  0
067400030915     c                   eval      wElSNS =  £NrRow
067500030915x   2c                   else
067600030915     c                   eval      wElSNS =  %elem($SC1)
067700030915e   2c                   endif
067800030915if  2c                   if        wElSNS <= 4
067900030915     c                   eval      wMaxLine = $MaxLine1
068000030915x   2c                   else
068100030915     c                   eval      wMaxLine = $MaxLine2
068200030915e   2c                   endif
068300030915      *
068400030915e   1c                   ENDSL
068500081127      *
068600081127     c                   if        NOT *in69
068700081127     c                   eval      wNrCpy = 1
068800081127     c                   endif
068900030915      *
069000130710     c                   open      FNLSB6P
069100031104     c                   clear                   Page4
069200030915      *
069300030915     c                   ENDSR
069400030912
069500030912      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
069600030623
069700030623     c     STAMPA        BEGSR
069800030623      *
069900110426      * -?Decodifica filiale?
070000030623     c                   exsr      DecodPO
070100030623      *
070200080715do  1c                   DO        wNrCpy        wwwPag
070300030623      *
070400110426      * -?Azzeramento numero di pagina?
070500030623     c                   reset                   wPage
070600030623      *
070700110426      * -?Preparazione dati per stampa bolla?
070800080715     c                   exsr      RiempiLDV
070900110426      * -?Stampa bolla?
071000080606     c                   exsr      StampaLDV
071100030623      *
071200030623e   1c                   EndDO
071300030623      *
071400030623     c                   ENDSR
071500080605      /free
071600151118
071700151118       //--------------------------------------------------------------
071800151118       //?Esecuzione Override al *PrtF per la generazione di un PDF.
071900151118       //--------------------------------------------------------------
072000151118       BEGSR  sr_OvrToPDF;
072100151118
072200151118         Qcmd = c_CmdOvrPrtF;
072300151118         exsr  sr_ExecCmd;
072400151118
072500151118       ENDSR;
072600151118
072700151118       //--------------------------------------------------------------
072800151118       //?Rimozione Override al *PrtF per la generazione di un PDF.
072900151118       //--------------------------------------------------------------
073000151118       BEGSR  sr_DltOvrPDF;
073100151118
073200151118         Qcmd = c_CmdDltOvr;
073300151118         exsr  sr_ExecCmd;
073400151118
073500151118       ENDSR;
073600151118
073700151118       //--------------------------------------------------------------
073800151118       //?Esecuzione del comando (già impostato).                      ?
073900151118       //--------------------------------------------------------------
074000151118       BEGSR  sr_ExecCmd;
074100151118
074200151118         clear Qcap0100;
074300151118         Qcabcsdh = *off;
074400151118         Qcapa    = *off;
074500151118         Qcacmdss = *off;
074600151118         Qcaerved = *allX'00';
074700151118
074800151118         clear Qusec;
074900151118         Qusbprv  = %size(Qusec);
075000151118
075100151118         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
075200151118                           %size(Qcap0100) : 'CPOP0100' : *omit :
075300151118                           0 : 0 : Qusec);
075400151118
075500151118         //if  Qusei <> *blank;
075600151118         //  ...;
075700151118         //endif;
075800151118
075900151118       ENDSR;
076000080605
076100080605       //--------------------------------------------------------------
076200130418       //?Posizionamento del box all'interno della pag. A4
076300080605       //--------------------------------------------------------------
076400130418       BEGSR  Posiz_Box;
076500080703
076600080703         select;
076700080703
076800110426           // -?Richiesta stampa di 0 LdV x pagina?
076900110426           //  ?=> default (2 LdV/pag.)?
077000080711           when  SDSprm = *zero;
077100080715             $LdVxP   = 2;
077200080715             $Len1LdV = $LenLdV2;
077300151118
077400151118           // -?Richiesta stampa in formato PDF?
077500151118           when  £Mod = 'X';
077600151118             $LdVxP   = 1;
077700080711
077800110426           // -?Richiesta stampa di 1 LdV x pagina?
077900110426           //  ?(LdV in partenza con DDT del Mitente)?
078000080704           when  £Mod = '1';
078100080704             $LdVxP   = 1;
078200080703
078300110426           // -?Richiesta stampa di 2 LdV x pagina (ex A5)?
078400110426           //  ?(copia per il vettore & copia per il destinatario)?
078500080704           when  £Mod = '2';
078600080703             $LdVxP   = 2;
078700080703             $Len1LdV = $LenLdV2;
078800080703
078900110426           // -?Richiesta stampa di 4 LdV x pagina?
079000110426           //  ?(solo copia per il vettore)?
079100080704           when  £Mod = '4';
079200080703             $LdVxP   = 4;
079300080703             $Len1LdV = $LenLdV4;
079400080703
079500080703           other;
079600080703             reset $LdVxP;
079700080703             reset $Len1LdV;
079800080703
079900080703         endsl;
080000080704
080100110426         // -?Richiesta "ripartenza" da foglio A4 successivo?
080200130418         //if DB0fl6 = *on  and  Not *in33;
080300080704         //  clear  wNumLdV;
080400080704         //endif;
080500080605
080600110426         // -?Se 2ª LdV nella pagina, ma LdV per A4 (intero):?
080700110426         //  ?si salta alla pag. A4 successiva; la LdV in esame?
080800110426         //  ?dev'essere stampata in posizione iniziale dell'A4,?
080900110426         //  ?seguita dai suoi dati ulteriori.?
081000130418         if  Not  *in33;
081100130418           wNumLdV += 1;
081200130418           wSeqLdV  = %rem( wNumLdV : $LdVxP );
081300130418         endif;
081400080703
081500080702         select;
081600080703
081700130418           // -?Modulo ex-A5 (2°)?
081800080715           when  SDSprm = *zero;
081900080715             if  wSeqLdV = *zero;
082000080715               wSeqLdV = 2;
082100080715             endif;
082200080715
082300130418           // -?Modulo A4 intero?
082400080702           when  NOT  *in69;
082500080702             wSeqLdV = 1;
082600151118           when  £Mod = '1'  or
082700151118                 £Mod = 'X';
082800080704             wSeqLdV  = 1;
082900080703
083000110426           // -?Calcolo della posizione x box all'interno della pagina "A4"?
083100080703           when  wSeqLdV = *zero   and
083200080715                 SDSprm  > *zero   and
083300080704                 £Mod    = '2';
083400080704             wSeqLdV = 2;
083500080703           when  wSeqLdV = *zero   and
083600080715                 SDSprm  > *zero   and
083700080704                 £Mod    = '4';
083800080703             wSeqLdV = 4;
083900080711           when  wSeqLdV = *zero   and
084000080711                 *in69;
084100080711             wSeqLdV  = 2;
084200080703
084300080702         endsl;
084400090515
084500110426         // -?Impostaz. delle posizioni nella pagina in corso di stampa?
084600090515         wPosIni = ($Len1LdV * (wSeqLdV - 1));
084700130422         if  *in33;
084800130422           wPosIni += $LenLdV4;
084900130422         endif;
085000090515
085100110426         // -?Se richiesta la stampa della sola packing-list?
085200110426         //  ?NON si dovranno stampare i dati della LdV?
085300110426         //  ?(quindi NON servono i posizionamenti che seguono)?
085400090515         //if  DB0fa4 = 'P';
085500090515         //  leavesr;
085600090515         //endif;
085700080702
085800110426         // -?Posizionamento nel foglio A4 in base al n° LdV in stampa?
085900130418         select;
086000130418           when  SDSprm > *zero   and
086100130418                 £Mod   = '4';
086200130418             *in41 = (wSeqLdV = 1);
086300130418             *in42 = (wSeqLdV = 2);
086400130418             *in43 = (wSeqLdV = 3);
086500130418             *in44 = (wSeqLdV = 4);
086600130418           when  Not *in33;
086700130418             *in41 = (wSeqLdV = 1);
086800130418             *in42 = *off;
086900130418             *in43 = (wSeqLdV = 2);
087000130418             *in44 = *off;
087100130418           other;
087200130418             *in41 = *off;
087300130418             *in42 = (wSeqLdV = 1);
087400130418             *in43 = *off;
087500130418             *in44 = (wSeqLdV = 2);
087600130418         endsl;
087700080702         WRITE  A4prtPOS;
087800080702
087900110426         // -?Posizionamenti nella copia per il vettore?
088000110426         //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
088100080702
088200110426         // -?Logo & BarCode?
088300080702         //§PS0    = wPosIni + c_PosBox1AS;    (v. §BOX1AS)
088400080702
088500110426         // -?Box in copia per il vettore?
088600090515         §Box1As = wPosIni + c_PosBox1As;
088700140922         §Box1Ai = wPosIni + c_PosBox1Bs  + c_LineNarrow;
088800090515         §Box1Bs = wPosIni + c_PosBox1Bs;
088900090515         §Box1Bi = wPosIni + c_PosBox1Bi;
089000140922         §Box1Bj = wPosIni + c_PosLin1oD1 + c_LineNarrow;
089100090515         §Box1Cs = wPosIni + c_PosBox1Cs;
089200140922         §Box1Ci = wPosIni + c_PosBox1Ds  + c_LineWide;
089300090515         §Box1Ds = wPosIni + c_PosBox1Ds;
089400091119         §Box1Di = §Box1Bi;
089500090515
089600120530         ////// -?Simil-Logo (solo) Bartolini?
089700120530         ////§PlblS  = §Box1As;
089800120530         ////§PlblVS = 7.128;
089900120530         ////exsr  sr_SimilLogoBART;
090000120530         //// -?Simil-Logo Bartolini (/Brt)?
090100120530         //§PlblS  = §Box1As + 0.125;
090200120530         //§PlblVS = 7.092;
090300120530         //exsr  sr_SimilLogoBAR;
090400120530         //// -?Simil-Logo (Bartolini/) Brt?
090500120530         //§P2blS  = §PlblS;
090600120530         //§P2blVS = §PlblVD + 0.030;
090700120530         //exsr  sr_SimilLogoBRT;
090800120530         // -?Simil-Logo BRT?
090900120530         §PlblS  = §Box1As;
091000120530         §PlblVS = 7.581;
091100120530         exsr  sr_SimilLogoBRT_;
091200110426
091300120530         // -?Costanti dati anagrafici BRT?
091400110426         §PC0    = wPosIni + c_PosTBar;
091500110426         §PC1    = wPosIni + c_PosTBa1;
091600120530         ////§PC2    = wPosIni + c_PosTBa2x;
091700120530         ////§PC3    = wPosIni + c_PosTBa3x;
091800120530         ////§PC4    = wPosIni + c_PosTBa4x;
091900120530         ////§PC5    = wPosIni + c_PosTBa5x;
092000120530         ////§PC6    = wPosIni + c_PosTBa6x;
092100120530         //§PC2    = wPosIni + c_PosTBa2;
092200120530         //§PC3    = wPosIni + c_PosTBa3;
092300120530         //§PC4    = wPosIni + c_PosTBa4;
092400120530         //§PC5    = wPosIni + c_PosTBa5;
092500120530         //§PC6    = wPosIni + c_PosTBa6;
092600120530         §PC2    = wPosIni + c_PosTBa2x;
092700120530         §PC3    = wPosIni + c_PosTBa3x;
092800120530         §PC4    = wPosIni + c_PosTBa4x;
092900120530         §PC5    = wPosIni + c_PosTBa5x;
093000120530         §PC6    = wPosIni + c_PosTBa6x;
093100090515
093200110426         // -?Linee orizzontali nel box in copia per il vettore?
093300090515         §Lo1S1  = wPosIni + c_PosLin1oS1;
093400090515         §Lo1S2  = wPosIni + c_PosLin1oS2;
093500090515       //§Lo1S3  = wPosIni + c_PosLin1oS3;
093600090515         §Lo1S4  = wPosIni + c_PosLin1oS4;
093700091119         §Lo1S5  = wPosIni + c_PosLin1OS5;
093800090515         §Lo1C1  = wPosIni + c_PosLin1oC1;
093900090515         §Lo1C2  = wPosIni + c_PosLin1oC2;
094000140915         §Lo1C3  = wPosIni + c_PosLin1oC3;
094100090515         §Lo1D1  = wPosIni + c_PosLin1oD1;
094200090515
094300110426         // -?Lunghezza linee verticali nel box in copia per il vett.?
094400090515         §LLv1A  = §Lo1S5  - §Lo1S1;
094500140922         //§LLv1B  = §Lo1S4  - §Box1Bs;
094600140922         //§LLv1C  = §Box1Di - §Box1Bs;
094700140922         //***§LLv1B  = §Lo1S4  - §Box1Bj + c_LineNarrow;
094800140922         §LLv1B  = §Lo1S4  - §Box1Bj;
094900140922         §LLv1C  = §Box1Di - §Box1Bj;
095000090515
095100110426         // -?Costante "R" = Ristampa  &  Numero Pagina?
095200090515         /////§Pk1    = wPosIni + c_PosKRi;
095300090515
095400110426         ///// -?Costante "Servizio Espresso Wuerth"?
095500090515         ///§Pk1    = wPosIni + c_PosKSerEspWu;
095600140917
095700140917         // -?Costante "BRTcode" & codice BRTcode?
095800140917         §Pk1    = wPosIni + c_PosKBRTc;
095900090515
096000110426         // -?Costante "Consegna tassativa"?
096100090515         §Pk2    = wPosIni + c_PosKTass;
096200090515
096300110426         // -?Costanti "(nome e cognome in stampatello)", "(firma)" e?
096400110426         //           ?"(data e ora)"?
096500090515         §Pk3    = §Box1Bi - 0.015;
096600090515
096700110426         // -?Costante con rif. alla legge in vigore per RCV?
096800090515         §Pk4    = wPosIni + c_PosKRCV;
096900090515
097000110426         // -?Costante "Bollo assolto in modo virtuale..."?
097100090515         §Pk5    = wPosIni + c_PosKBAV;
097200090515         §Pk6    = §Pk5 + 0.060;
097300121010
097400121010         // -?Partita IVA o Cod. Fiscale del cliente?
097500130418         §Pk7    = §Pk4 + 0.080;
097600090515
097700110426         // -?Costante "COPIA PER IL DESTINATARIO" in DDT "generico"?
097800090515         //§Pos    = c_PosKCXD2;       [NON utilizzato da questo *pgm]
097900080702
098000080702       ENDSR;
098100120530
098200120530      /end-free
098300090515
098400120322       //-------------------------------------------------------------
098500120322       //?Posizionamento del simil-logo Bartolini (unico)             ?
098600120322       //-------------------------------------------------------------
098700120530      *///copy gaitrasrc/srcTNSY,LogoBAR_R2
098800120322
098900120322       //-------------------------------------------------------------
099000120322       //?Posizionamento del simil-logo Bartolini (con BRT)           ?
099100120322       //-------------------------------------------------------------
099200120530      *///copy gaitrasrc/srcTNSY,LogoBAR3R2
099300110426
099400120322       //-------------------------------------------------------------
099500120322       //?Posizionamento del simil-logo BRT (con BARTOLINI)           ?
099600120322       //-------------------------------------------------------------
099700120530      *///copy gaitrasrc/srcTNSY,LogoBRT3R2
099800120521
099900120521       //-------------------------------------------------------------
100000120521       //?Posizionamento del simil-logo BRT (unico)                   ?
100100120521       //-------------------------------------------------------------
100200120530      /copy gaitrasrc/srcTNSY,LogoBRT_R2
100300030915
100400030915      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
100500030915
100600080606     c     StampaLDV     BEGSR
100700030915      *
100800110426      *?Azzeramento numero di pagina per bolla?
100900110426      *?(meglio: "reimpostazione a 1")?
101000030915     c                   reset                   wPage
101100030915      *
101200110426      *?(serve prima di barcode e cornice per impostare la pagina:?
101300110426      *? barcode & cornice vengono stampati nella pagina "attuale").?
101400080605      *
101500110426      *?Posizionamento sul foglio A4 in base al n° pagina in stampa?
101600130418     c                   eval      *in33 = *off
101700130418     c                   exsr      Posiz_Box
101800030915      *
101900120530      *?1ª parte - copia per il vettore (da ritornare in "BRT")?
102000130418     c                   write     A4prtLDV
102100130418     c                   write     A4prtBOX
102200110426      /free
102300110426         // >?Simil-Logo Bartolini?
102400110426         //write  A4LogoBAR;
102500110426         //write  A4LogoDA1;
102600110426         // >?Simil-Logo Bartolini/BRT?
102700120530         //write  A4LogoBR2;
102800120530         //write  A4LogoDA2;
102900120322         // >?Simil-Logo BRT?
103000120530         write  A4LogoBRT;
103100120530         write  A4LogoDA3;
103200110426
103300110426         // >?dati DPD?
103400080922         if  *in71;
103500080922           exsr  StampaDPD;
103600080922         endif;
103700080922      /end-free
103800080702      *
103900090406if  1c*****              if             SDSprm  < 2
104000090406     c*****                        and  wSeqLdV = 1
104100090406     c*****              eval      §PC1    = 0.20
104200090406do  2c*****              do        100           PRowNr
104300090406     c*****              eval      §PC0    = PRowNr / 9
104400090406     c*****              write     A4prtRowNr
104500090406e   2c*****              enddo
104600090406     c*****              eval      §PC1    = 7.90
104700090406do  2c*****              do        100           PRowNr
104800090406     c*****              eval      §PC0    = PRowNr / 9
104900090406     c*****              write     A4prtRowNr
105000090406e   2c*****              enddo
105100090406e   1c*****              endif
105200030915      *
105300030915sel 1c                   select
105400110426      * >?dati FedEx?
105500030915w   1c                   when      *in72
105600030915     c                   exsr      ListaFED
105700110426      * >?dati EuroExpress?
105800040223w   1c                   when      *in73
105900030915      *
106000030915e   1c                   endsl
106100070308      *
106200110426      * >?Promemoria per l'autotrasportatore?
106300070308if  1c                   if        SDSprm > *zero and
106400070308     c                             (£Mod  = 'P')
106500070308     c                   exsr      StampaPMxA
106600070308e   1c                   endif
106700030915      *
106800110426      * >?dati FIAR5?
106900060912if  1c                   if        SDSprm > *zero and
107000060912     c                             (£Mod  = 'I'  or
107100060912     c                              £Mod  = 'r')
107200030915     c                   exsr      ListaAR5
107300030915e   1c                   endif
107400030915      *
107500110426      * >?Riferimento O.R.M. per ritiro contestuale alla consegna?
107600060912if  1c                   if        SDSprm > *zero and
107700060912     c                             (£Mod  = 'R'  or
107800060912     c                              £Mod  = 'r')
107900030915     c                   exsr      StampaRCC
108000030915e   1c                   endif
108100030915      *
108200110426      * >?elenco SEGNACOLLI NON SEQUENZIALI?
108300110426      *  ?(no x DPD   perchè monocollo)?
108400110426      *  ?(no x FedEx perchè segnacolliamo noi)?
108500110426      *  ?(no x "A5"  perchè sono già stati elencati tra le "Note e?
108600110426      *              ?Particolarità")?
108700030915if  1c                   if        SDSprm  > *zero and
108800030915     c                             (£Mod   = 'S'  or
108900030915     c                              £Mod   = 's'  or
109000030915     c                              £Mod   = 'Q'   and
109100030915     c                              wElSNS > 4)
109200030915     c                   exsr      ListaScNS
109300030915e   1c                   endif
109400030915      *
109500080703     c                   select
109600110426      * >?Richiesta la sola copia per il vettore?
109700080703w   1c                   when      SDSprm >  *zero   and
109800080703     c                             £Mod   =  '4'
109900110426      * >?Solo se NON già stampata 2ª parte (cioè ancora 1ª pagina)?
110000080703w   1c                   when      wPage  < 2
110100110426      * -?separazione?
110200110426      * -?2ª parte - copia per il destinatario (da lasciare al cliente)?
110300110426      * -?"cornice" / barcode / righine varie...?
110400080715if  2c                   if        SDSprm  = *zero    or
110500080715     c                             £Mod   <> '4'
110600130418     c                   eval      *in33 = *on
110700060912     c***                write     A4prtSEP
110800130418     c                   exsr      Posiz_Box
110900130418     c                   write     A4prtLDV
111000130418     c                   write     A4prtBOX
111100110426      /free
111200110426         // >?Simil-Logo Bartolini?
111300110426         //write  A4LogoBAR;
111400110426         //write  A4LogoDA1;
111500110426         // >?Simil-Logo Bartolini/BRT?
111600120530         //write  A4LogoBR2;
111700120530         //write  A4LogoDA2;
111800120322         // >?Simil-Logo BRT?
111900120530         write  A4LogoBRT;
112000120530         write  A4LogoDA3;
112100110426      /end-free
112200080703e   2c                   endif
112300080703e   1c                   endsl
112400030915      *
112500110426      *?Stampa dei 3 asterischi finali (se richiesta)?
112600060912if  1c                   if        SDSprm > *zero and
112700060912     c                             £Mod   = '*'
112800030915     c                   move      '***'         Pmsg3
112900080704if  2c                   if        £Mod   = '4'
113000130418     c                   eval      *in33 = *off
113100130418     c                   exsr      Posiz_Box
113200080703x   2c                   else
113300130418     c                   eval      *in33 = *on
113400130418     c                   exsr      Posiz_Box
113500080703e   2c                   endif
113600030915     c                   write     A4prtMSG
113700030915e   1c                   endif
113800030915      *
113900030915     c                   ENDSR
114000070308
114100070308      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
114200070308
114300070308     c     StampaPMxA    BEGSR
114400070308      *
114500110426      *?Imposto le posizioni inziali per il primo dei box?
114600120530      * -?in bolla BRT: dopo eventuale elenco segnacolli non seq.?
114700070308     c   70              exsr      Rep_§LO2
114800110426      * -?in bolla FEDEX: posizione variabile?
114900110426      *  ?(dipende dal numero dei segnacolli elencati: intanto?
115000110426      *  ?posiziona la "base di partenza" sull'ultima posizione?
115100110426      *  ?eventualmente usata per l'elenco dei segnacolli FedEx;?
115200110426      *  ?poi si ri-calcoleranno in base al numero di righe stampate)?
115300070308     c   72              eval      §LO2   = §LO1 + 0.250
115400110426      * -?in bolla EUROEXPRESS: (non più gestito)?
115500080924      *
115600070308     c                   eval      §LO1   = §LO2
115700070308      *
115800110426      * -?Stampa testata?
115900070308     c                   eval      $First = 'B'
116000070308     c                   eval      Par5   = 'PROMEMORIA PER '
116100070308     c                                    + 'L''AUTOTRASPORTATORE'
116200070308     c                   exsr      StampaAR5
116300070308      *
116400110426      * -?Lasciare una riga vuota ANCHE dopo l'intestazione del box?
116500070308     c***                clear                   wCount
116600070308      *
116700110426      * -?Stampa testi (tutti)?
116800070308      *
116900110426      * >?Tipo "0"?
117000070308     c                   exsr      Salta2righe
117100070308     c                   eval      $First = *off
117200081127     c                   eval      Par5   = '00000WWWWWWWWWWWWWWWWWWWW+
117300070308     c                                       WWWWWWWWWWWWWWWWWWWWWWWWW+
117400070308     c                                       WWWWWWWWWWWWWWWWWWWWWWWW*'
117500070308     c                   exsr      StampaAR5
117600110426      * >?Tipo "1"?
117700070308     c                   exsr      Salta2righe
117800070308     c                   eval      $First = *on
117900081127     c                   eval      Par5   = '11111WWWWWWWWWWWWWWWWWWWW+
118000070308     c                                       WWWWWWWWWWWWWWWWWWWWWWWWW+
118100070308     c                                       WWWWWWWWWWWWWWWWWWWWWWWW*'
118200070308     c                   eval      *in38  = *off
118300070308     c                   exsr      StampaAR5
118400070308     c                   exsr      Salta2righe
118500081127     c                   eval      Par5   = '11111WWWWWWWWWWWWWWWWWWWW+
118600070308     c                                       WWWWWWWWWWWWWWWWWWWWWWWWW+
118700070308     c                                       WWWWWWWWWWWWWWWWWWWWWWWW*'
118800070308     c                   eval      *in38  = *on
118900070308     c                   exsr      StampaAR5
119000070308     c                   eval      *in38  = *off
119100110426      * >?Tipo "A"?
119200081127     c                   exsr      Salta2righe
119300070308     c                   eval      $First = 'A'
119400081127     c                   eval      Par5   = 'AAAAAWWWWWWWWWWWWWWWWWWWW+
119500081127     c                                       WWWWWWWWWWWWWWWWWWWWWWWWW+
119600081127     c                                       WWWWWWWWWWWWWWWWWWWWWWWW*'
119700081127     c                   exsr      StampaAR5
119800110426      * >?Tipo "B"?
119900070308     c                   exsr      Salta2righe
120000070308     c                   eval      $First = 'B'
120100081127     c                   eval      Par5   = 'BBBBBWWWWWWWWWWWWWWWWWWWW+
120200070308     c                                       WWWWWWWWWWWWWWWWWWWWWWWWW+
120300070308     c                                       WWWWWWWWWWWWWWWWWWWWWWWW*'
120400070308     c                   exsr      StampaAR5
120500110426      * >?Tipo "C"?
120600070308     c                   exsr      Salta2righe
120700070308     c                   eval      $First = 'C'
120800081127     c                   eval      Par5   = 'CCCCCWWWWWWWWWWWWWWWWWWWW+
120900070308     c                                       WWWWWWWWWWWWWWWWWWWWWWWWW+
121000070308     c                                       WWWWWWWWWWWWWWWWWWWWWWWW*'
121100070308     c                   exsr      StampaAR5
121200110426      * >?Tipo "D"?
121300070507     c***                exsr      Salta2righe
121400070507     c***                eval      $First = 'D'
121500081127     c***                eval      Par5   = 'DDDDDWWWWWWWWWWWWWWWWWWWW+
121600070507     c***                                    WWWWWWWWWWWWWWWWWWWWWWWWW+
121700070507     c***                                    WWWWWWWWWWWWWWWWWWWWWWWW*'
121800070507     c***                exsr      StampaAR5
121900110426      * >?Tipo "E"?
122000070507     c***                exsr      Salta2righe
122100070507     c***                eval      $First = 'E'
122200081127     c***                eval      Par5   = 'EEEEEWWWWWWWWWWWWWWWWWWWW+
122300070507     c***                                    WWWWWWWWWWWWWWWWWWWWWWWWW+
122400070507     c***                                    WWWWWWWWWWWWWWWWWWWWWWWW*'
122500070507     c***                exsr      StampaAR5
122600110426      * >?Tipo "F"?
122700070507     c***                exsr      Salta2righe
122800070507     c***                eval      $First = 'F'
122900081127     c***                eval      Par5   = 'FFFFFWWWWWWWWWWWWWWWWWWWW+
123000070507     c***                                    WWWWWWWWWWWWWWWWWWWWWWWWW+
123100070507     c***                                    WWWWWWWWWWWWWWWWWWWWWWWW*'
123200070507     c***                exsr      StampaAR5
123300110426      * >?Tipo "G"?
123400070507     c                   exsr      Salta2righe
123500070507     c                   eval      $First = 'G'
123600081127     c                   eval      Par5   = 'GGGGGWWWWWWWWWWWWWWWWWWWW+
123700070507     c                                       WWWWWWWWWWWWWWWWWWWWWWWWW+
123800070507     c                                       WWWWWWWWWWWWWWWWWWWWWWWW*'
123900070507     c                   exsr      StampaAR5
124000110426      *
124100110426      * >?Se destinatario assente inserire il plico in buca lettere?
124200070308     c                   exsr      Salta2righe
124300070308     c                   eval      $First = 'C'
124400070308     c                   eval      Par5   = 'Se il destinatario è asse-
124500070308     c                             nte inserire il plico nella buca de-
124600070308     c                             lle lettere'
124700070308     c                   exsr      StampaAR5
124800070308      *
124900110426      * >?Non effettuare ritiro di merce?
125000070308     c                   exsr      Salta2righe
125100070308     c                   eval      $First = 'D'
125200070308     c                   eval      Par5   = 'NON EFFETTUARE RITIRO DI -
125300070308     c                             MERCE'
125400070308     c                   exsr      StampaAR5
125500070308     c                   eval      $First = 'E'
125600070308     c                   eval      Par5   = 'Per questa consegna non è-
125700070308     c                              previsto nessun ritiro contestuale-
125800070308     c                             .'
125900070308     c                   exsr      StampaAR5
126000070308      *
126100110426      * >?Raccogliere Codice Fiscale di ...?
126200081127     c***                eval      $First = *off
126300081127if  2c***                if        wCount > *zeros
126400081127     c***                clear                   Par5
126500081127     c***                exsr      StampaAR5
126600081127e   2c***                endif
126700081127     c***                eval      Par5   = 'Spedizione n°'
126800081127     c***                                 + '123 12 1234567'
126900081127     c***                                 + ' del '
127000081127     c***                                 + %editc(Pdsp:'Y')
127100081127     c***                exsr      StampaAR5
127200081127     c***** (già fatto:) eval      $First = *off
127300081127     c***                eval      Par5   = 'raccogliere +
127400081127     c***                                   Codice Fiscale di "'
127500081127     c***                                 + 'ROSSI ERNESTO'
127600081127     c***                                 + '"'
127700081127     c***                exsr      StampaAR5
127800070308      *
127900110426      * -?stampo box?
128000070308     c                   eval      §LO2   = §PS1 + 0.050
128100070308     c                   write     A4prtAR53
128200070308      *
128300070308     c                   ENDSR
128400070308
128500070308      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
128600070308
128700070308     c     Salta2righe   BEGSR
128800070308      *
128900070308     c                   eval      $First = *off
129000070308     c                   clear                   Par5
129100070308     c                   exsr      StampaAR5
129200070308     c                   exsr      StampaAR5
129300070308      *
129400070308     c                   ENDSR
129500030915
129600030915      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
129700030915
129800030915     c     ListaAR5      BEGSR
129900030915      *
130000030915     c                   eval      $Fiar5 = *on
130100030915      *
130200110426      *?Imposto le posizioni inziali per il primo dei box?
130300120530      * -?in bolla BRT: dopo eventuale elenco segnacolli non seq.?
130400030915     c   70              exsr      Rep_§LO2
130500110426      * -?in bolla FEDEX: posizione variabile?
130600110426      *  ?(dipende dal numero dei segnacolli elencati: intanto?
130700110426      *  ?posiziona la "base di partenza" sull'ultima posizione?
130800110426      *  ?eventualmente usata per l'elenco dei segnacolli FedEx;?
130900110426      *  ?poi si ri-calcoleranno in base al numero di righe stampate)?
131000030915     c   72              eval      §LO2   = §LO1 + 0.250
131100110426      * -?in bolla EUROEXPRESS: non più gestito?
131200080924      *
131300030915     c                   eval      §LO1   = §LO2
131400030915      *
131500030915do  0c                   do        3             XX
131600030915      *
131700110426      *?Impostazione righe da stampare ("titolo" compreso)?
131800030915     c                   clear                   wR_AR5
131900030915sel 1c                   select
132000030915w   1c                   when      XX     = 1
132100030915if  2c                   if             SDSprm > 1
132200080624     c                             and (£Part <> 'D'
132300080624     c                             and  £Part <> 'Z')
132400030915     c                   eval      wR_AR5 = 2 + 1
132500030915e   2c                   endif
132600030915w   1c                   when      XX     = 2
132700030915if  2c                   if             SDSprm > 1
132800080624     c                             and (£Part <> 'E'
132900080624     c                             and  £Part <> 'Z')
133000030915if  3c                   if        SDSprm > 2
133100030915     c                   eval      wR_AR5 = £NrRow + 1
133200030915x   3c                   else
133300030915     c                   eval      wR_AR5 = 3 + 1
133400030915e   3c                   endif
133500030915e   2c                   endif
133600030915w   1c                   when      XX     = 3
133700030915if  2c                   if             SDSprm > 1
133800080624     c                             and (£Part <> 'F'
133900080624     c                             and  £Part <> 'Z')
134000030915     c                   eval      wR_AR5 = 1 + 1
134100030915e   2c                   endif
134200030915e   1c                   endsl
134300030915      *
134400110426      *?Ripulisco i campi utilizzati in seguito?
134500030915     c                   reset                   $First
134600030915      *
134700030915if  1c                   if        wR_AR5 > *zeros
134800030915      *
134900110426      *?Ciclo di stampa?
135000030915do  2c                   do        wR_AR5        Yy
135100030915      *
135200110426      *?Impostazione campi di stampa?
135300030915if  3c                   if        SDSprm < 2
135400030915     c                   eval      Par5   = $Lungh + $Lungh
135500030915x   3c                   else
135600030915sel 4c                   select
135700110426      * -?Bancali da rendere al mittente?
135800030915w   4c                   when           SDSprm > 1
135900080624     c                             and (£Part <> 'F'
136000080624     c                             and  £Part <> 'Z')
136100030915     c                             and  XX     = 1
136200030915     c                   clear                   Par5
136300030915if  5c                   if        Yy     = 1
136400030915     c                   eval      Par5   = 'BANCALI DA RENDERE AL MITTENTE    '
136500030915     c                   eval      $First = *on
136600030915x   5c                   else
136700030915     c                   eval      Par5   = $Note(7-1 + Yy)
136800030915e   5c                   endif
136900110426      * -?Destinatario supermercato?
137000030915w   4c                   when           SDSprm > 1
137100080624     c                             and (£Part <> 'G'
137200080624     c                             and  £Part <> 'Z')
137300030915     c                             and  XX     = 2
137400030915     c                   clear                   Par5
137500030915if  5c                   if        Yy     = 1
137600030915     c                   eval      Par5   = 'DESTINATARIO SUPERMERCATO         '
137700030915     c                   eval      $First = *on
137800030915x   5c                   else
137900030915if  6c                   if            SDSprm > 2
138000030915     c                             and £NrRow > *zeros
138100030915     c                   eval      Par5   = $Note(10)
138200030915x   6c                   else
138300030915     c                   eval      Par5   = $Note(9-1 + Yy)
138400030915e   6c                   endif
138500030915e   5c                   endif
138600110426     *** * -?Promemoria per autotrasportatore?
138700030915w   4***c                   when           SDSprm > 1
138800080624     ***c                             and (£Part <> 'H'
138900080624     ***c                             and  £Part <> 'Z')
139000030915     ***c                             and  XX     = 3
139100030915     ***c                   clear                   Par5
139200030915if  5***c                   if        Yy     = 1
139300030915     ***c                   eval      Par5   = 'PROMEMORIA PER AUTOTRASPORTATORE
139400030915     ***c                   eval      $First = *on
139500030915x   5***c                   else
139600030915     ***c                   eval      Par5   = $Note(29-1 + Yy)
139700030915e   5***c                   endif
139800110426      * -?Bancali da riportare a magazzino?
139900110426      *  ?(oltre 20 colli)?
140000030915w   4c                   when           SDSprm > 1
140100080624     c                             and (£Part <> 'D'
140200080624     c                             and  £Part <> 'F'
140300080624     c                             and  £Part <> 'Z')
140400030915     c                             and  XX     = 3
140500030915     c                   clear                   Par5
140600030915if  5c                   if        Yy     = 1
140700030915     c                   eval      Par5   = 'BANCALI DA RIPORTARE A MAGAZZINO  '
140800030915     c                   eval      $First = *on
140900030915x   5c                   else
141000030915     c                   eval      Par5   = $Note(32)
141100030915e   5c                   endif
141200030915e   4c                   endsl
141300030915e   3c                   endif
141400030915      *
141500110426      *?Stampa del singolo record?
141600030915     c                   exsr      StampaAR5
141700030915      *
141800030915e   2c                   enddo
141900030915      *
142000110426      *?Stampa del box?
142100030915     c                   eval      §LO2   = §PS1 + 0.050
142200030915     c                   write     A4prtAR53
142300030915      *
142400030915e   1c                   endif
142500030915      *
142600030915e   0c                   enddo
142700030915      *
142800030915     c                   ENDSR
142900030915
143000030915      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
143100030915
143200030915     c     StampaAR5     BEGSR
143300030915      *
143400110426      *?Controllo se superata l'ultima riga?
143500030915     c                   exsr      Test_OF
143600070308      *
143700110426      *?Conteggio dei records stampati?
143800110426      *?(utilizzato solo per lasciare una riga vuota in fase di stampa?
143900110426      * ?delle "note per l'autotrasportatore")?
144000070308     c                   add       1             wCount
144100030915      *
144200110426      *?Stampa del singolo record?
144300030915sel 1c                   select
144400030915      *
144500030915w   1c                   when      $First = *off
144600110426      * -?Incremento parametri per posizionamento nuova riga?
144700030915     c                   eval      §PS1   = §PS1 + 0.150
144800030915     c                   write     A4prtAR52
144900030915      *
145000030915w   1c                   when      $First = *on
145100110426      * -?Incremento parametri per posizionamento nuovo box?
145200110426      *  ?e relativa intestazione?
145300030915     c                   eval      §LO1   = §LO2 + 0.150
145400030915     c                   eval      §PS1   = §LO1 + 0.150
145500030915     c                   write     A4prtAR51
145600030915     c                   eval      $First = *off
145700070308      *
145800070308w   1c                   when      $First = '2'
145900110426      * -?Incremento parametri per posizionamento nuova riga?
146000110426      *  ?nel box "Promemoria per autotrasportatore"?
146100070308     c                   eval      §PS1   = §PS1 + 0.150
146200070308     c                   write     A4prtAR51
146300070308     c                   eval      $First = *off
146400030915      *
146500030915w   1c                   when      $First = 'A'
146600110426      * -?Incremento parametri per posizionamento nuovo box?
146700110426      *  ?e relativa intestazione?
146800030915     c                   eval      §LO1   = §LO2 + 0.150
146900030915     c                   eval      §PS1   = §LO1 + 0.200
147000030915     c                   write     A4prtAR5a
147100030915     c                   eval      $First = *off
147200070308      *
147300151118w   1***c                   when      $First = 'B'
147400151118     *** * -?Incremento parametri per posizionamento nuovo box?
147500151118     *** *  ?ed intestazione "Promemoria per autotrasportatore"?
147600151118     ***c                   eval      §LO1   = §LO2 + 0.150
147700151118     ***c                   eval      §PS1   = §LO1 + 0.125
147800151118     ***c                   write     A4prtAR5b
147900151118     ***c                   eval      $First = 'C'
148000151118     *** *
148100151118w   1***c                   when      $First = 'C'
148200151118     *** * -?Incremento parametri per posizionamento nuova riga?
148300151118     *** *  ?nel box "Promemoria per autotrasportatore"?
148400151118     ***c                   eval      §PS1   = §PS1 + 0.250
148500151118     ***c                   write     A4prtAR5c
148600070507     *** *
148700070507w   1***c                   when      $First = 'D'
148800110426     *** * -?Incremento parametri per posizionamento nuova riga?
148900110426     *** *  ?nel box "Promemoria per autotrasportatore"?
149000070507     ***c                   eval      §PS1   = §PS1 + 0.300
149100070507     ***c                   write     A4prtAR5d
149200070507     *** *
149300070507w   1***c                   when      $First = 'E'
149400110426     *** * -?Incremento parametri per posizionamento nuova riga?
149500110426     *** *  ?nel box "Promemoria per autotrasportatore"?
149600070507     ***c                   eval      §PS1   = §PS1 + 0.250
149700070507     ***c                   write     A4prtAR5e
149800070507     *** *
149900070507w   1***c                   when      $First = 'F'
150000110426     *** * -?Incremento parametri per posizionamento nuova riga?
150100110426     *** *  ?nel box "Promemoria per autotrasportatore"?
150200070507     ***c                   eval      §PS1   = §PS1 + 0.200
150300070507     ***c                   write     A4prtAR5f
150400070507      *
150500070507w   1c                   when      $First = 'G'
150600110426      * -?Incremento parametri per posizionamento nuova riga?
150700110426      *  ?nel box "Promemoria per autotrasportatore"?
150800070507     c                   eval      §PS1   = §PS1 + 0.170
150900070507     c                   write     A4prtAR5g
151000030915      *
151100030915e   1c                   endsl
151200081127      *
151300110426      *?Salvataggio per il successivo incremento?
151400110426      *?(cioè per il successivo record in stampa).?
151500081127     c                   eval      §LO2   = §PS1
151600030915      *
151700030915     c                   ENDSR
151800030915
151900030915      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
152000030915
152100030915     c     StampaRCC     BEGSR
152200030915      *
152300110426      * >?O.R.M. per ritiro contestuale alla consegna?
152400110426      *  ?(unico record)?
152500110426      *  ?Verrà stampato DOPO eventuali "Segnacolli Non Sequenziali"?
152600110426      *  ?(non solo come ultimo box)?
152700030915      *
152800110426      * -?Reperisco la posizione (in pollici) in cui posizionarmi?
152900110426      *  ?per la stampa in base all'ultima riga stampata?
153000110426      *  ?se non ho ancora stampato box?
153100030915cas 1c     §LO2          caseq     *zeros        Rep_§LO2
153200030915e   1c                   endcs
153300030915      *
153400110426      * -?stampo riga separatrice con indicazione "tagliare qui"?
153500080221     c***                exsr      StampaTAGLIA
153600110426      * -?stampo testata?
153700110426      *  ?(con i dati dell'O.R.M.)?
153800030915     c                   eval      $First = 'A'
153900030915     c                   eval      Pbt2a  = 'RITIRO CONTESTUALE ALLA '
154000030915     c                                    + 'CONSEGNA'
154100030915     c                                    + '     -     '
154200030915     c                   eval      Pbt2b  = 'O.R.M. numero '
154300040223     c                                    + %editw(DUTfel : '0   ')
154400030915     c                                    + ' '
154500030915     c                                    + %editw(12 : '0  ')
154600030915     c                                    + '  '
154700030915     c                                    + %editc(1234567 : 'Z')
154800030915     c                                    + ' '
154900030915     c                                    + %editw(12 : '0  ')
155000030915     c                   exsr      StampaAR5
155100110426      * -?stampo i dati dell'O.R.M.?
155200030915     c                   exsr      QUM_FnOrm
155300030915     c                   eval      Par5   = 'Quantità    : '
155400030915     c                                    + %trim($Qum(1)) + '    '
155500030915     c                                    + %trim($Qum(2)) + '    '
155600030915     c                                    + %trim($Qum(3))
155700030915     c                   exsr      StampaAR5
155800030915     c                   eval      Par5   = 'Destinatario: '
155900030915     c                                    + $Cli(15)
156000030915     c                   exsr      StampaAR5
156100030915     c                   eval      Par5   = '              '
156200030915     c                                    + $Cli(16)
156300030915     c                   exsr      StampaAR5
156400030915     c                   eval      Par5   = '              '
156500030915     c                                    + $Cli(17) + ' '
156600030915     c                                    + $Cli(18) + ' '
156700030915     c                                    + $Cli(19) + ' '
156800030915     c                                    + $Cli(20)
156900030915     c                   exsr      StampaAR5
157000110426      * -?stampo box?
157100030915     c                   eval      §LO2   = §PS1 + 0.050
157200030915     c                   write     A4prtAR53
157300030915      *
157400030915     c                   ENDSR
157500030915
157600030915      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
157700030915
157800080221     c***  StampaTAGLIA  BEGSR
157900030915      *
158000110426      * -?Incremento parametri per posizionamento della nuova riga con?
158100110426      *  ?l'indicazione "tagliare qui"?
158200080221     ***c***                eval      §LO1   = §LO2 + 0.200
158300080221     c***                eval      §PS1   = §LO2 + 0.300
158400030915      *
158500110426      * -?Controllo se tale riga ci sta nella pagina attuale?
158600080221     c***                exsr      Test_OF
158700030915      *
158800110426      * -?Stampo riga con indicazione "tagliare qui"?
158900030915      *   26 x 2 + 14 + 26 x 2 =  52 + 14 +  52 = 118
159000030915      *   26 x 3 + 14 + 26 x 3 =  78 + 14 +  78 = 170
159100030915      *   26 x 4 + 14 + 26 x 4 = 104 + 14 + 104 = 222
159200080221     c***                eval      Ptag = '- - - - - - - - - - - - - '
159300080221     c***                               + '- - - - - - - - - - - - - '
159400080221     c***                               + '- - - - - - - - - - - - - '
159500080221     c***                               + '- - - - - - - - - - - - - '
159600080221     c***                               + ' TAGLIARE  QUI  '
159700080221     c***                               + '- - - - - - - - - - - - - '
159800080221     c***                               + '- - - - - - - - - - - - - '
159900080221     c***                               + '- - - - - - - - - - - - - '
160000080221     c***                               + '- - - - - - - - - - - - - '
160100080221     c***                write     A4prtTAG
160200030915      *
160300110426      * -?Incremento parametri per posizionamento della riga successiva?
160400110426      *  ?("spaziata" maggiormente...)?
160500080221     c***                eval      §LO2   = §PS1 + 0.100
160600080221     ***c*** dopo...:       eval      §LO1   = §LO2 + 0.150
160700080221     ***c*** dopo...:       eval      §PS1   = §LO1 + 0.200
160800030915      *
160900080221     c***                ENDSR
161000030915
161100030915      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
161200030915
161300030915     c     StampaDPD     BEGSR
161400030915      *
161500110426      *?List Nr.?
161600060912     c*** non in stp:    eval      Plst  = %subst(Prma:1:3)  +  ' '
161700060912     c*** non in stp:                    + %subst(Prma:4:4)  +  ' '
161800060912     c*** non in stp:                    + %subst(Prma:8:4)  +  ' '
161900060912     c*** non in stp:                    + %subst(Prma:12:1)
162000110426      *?Depot?
162100060912     c*** non in stp:    eval      Pdpt  = %subst(Prma:1:3)
162200110426      *?Bar-Code?
162300110426      *?(1' 12 char di Riferimento Mittente Alfanumerico - se numerici)?
162400110426      *?nuovi controlli per nuovo barcode DPD?
162500080922if  1c                   if        Prmn   <> *zeros
162600080922     c                   move      Prmn          W15a
162700110426      * -?controllo se vecchio o nuovo barcode?
162800080922if  2c                   if        %subst(w15a:14:1) = *blanks
162900110426      * - -?vecchio?
163000080922     c                   eval      Pbcd2 = '01 ' + %subst(w15a:1:3) +
163100080922     c                                     ' '   + %subst(w15a:4:8)
163200110426      * - -?nuovo?
163300080922x   2c                   else
163400080922     c                   eval      Pbcd2 = '01 ' + %subst(w15a:1:4) +
163500080922     c                                     ' '   + %subst(w15a:5:10)
163600080922e   2c                   endif
163700080922e   1c                   endif
163800080922      /free
163900110426         // -?Impostazione data?
164000080922         PdpA = 'Date ' + %editc(Pdsp:'Y');
164100080922
164200110426         // -?Impostazione numero colli?
164300080922         PdpB = 'Parcel N. ' + %editc(Pncl:'Z');
164400080922
164500110426         // -?Impostazione numero parcel?
164600080922         PdpC = %char(Prmn);
164700080922
164800110426         // -?Impostazione "costante" da stampare tra le Note?
164900080922         PdpD = 'Controllare la corrispondenza';
165000080922         PdpE = 'del destinatario con l''indirizzo';
165100080922         PdpF = 'dell''etichetta DPD.';
165200080922
165300110426         // -?Impostaz. posizioni sulla copia per il vettore già stampata?
165400080922         §DP1 = wPosIni + 1.100;
165500080922         §DP2 = §DP1 + 0.550;
165600080922         §DP3 = §DP2 + 0.120;
165700080922         §DP4 = §DP3 + 0.120;
165800080922         §DP5 = §DP4 + 0.120;
165900080922         §DP6 = §DP5 + 0.160;
166000080922         §DP7 = §DP6 + 0.130;
166100080922         §DP8 = §DP7 + 0.130;
166200080922      /end-free
166300030915      *
166400110426      *?(inutile testare l'overflow perchè siamo in ex-A5)?
166500080922     c                   write     A4prtDPD
166600030915      *
166700030915     c                   ENDSR
166800030915
166900030915      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
167000030915
167100030915     c     ListaFED      BEGSR
167200030915      *
167300110426      * -?Stampo il simbolo "FedEx" (sulla 1ª pagina):?
167400110426      *  ?imposto le posizioni iniziali di stampa per le linee?
167500110426      *  ?(orizzontali-§LOn e verticali-§LVn)?
167600030915     c                   eval      §LO1   = 4.100
167700030915     c                   eval      §LO2   = §LO1 + 0.425
167800030915     c                   eval      §LV1   = §LO1 + 0.300
167900030915     c                   eval      §LV2   = §LV1 + 0.070
168000030915     c                   write     A4prtFED1
168100030915      *
168200110426      * -?Salto il numero necessario di righe per posizionarmi?
168300110426      *  ?correttamente e stampo le "costanti" di inizio lista?
168400110426      *  ?(non posso già essere in posizione da salto pagina !)?
168500030915do  1c                   dow       CurLineA4 < $MinLine2
168600030915     c                   write     A4prtNUL
168700030915e   1c                   enddo
168800030915     c                   write     A4prtFED2
168900030915      *
169000110426      * -?Stampo elenco segnacolli FedEx?
169100030915     c                   eval      §LO1   = §LO2 - 0.250
169200030915      *
169300030915if  1c                   if        SDSprm > 2
169400030915     c                   eval      xx = £NrRow
169500030915x   1c                   else
169600030915     c                   eval      xx = 3
169700030915e   1c                   endif
169800030915      *
169900030915do  1c                   DO        xx
170000110426      * -?Impostazione campi di stampa?
170100030915if  2c                   if        PPTN = 0
170200030915     c                   eval      PPTN = 123456789012
170300030915x   2c                   else
170400030915     c                   add       1             Pptn
170500030915e   2c                   endif
170600030915     c                   add       1             Psts
170700110426      * -?Stampa?
170800030915     c                   exsr      Test_OF
170900110426      * -?Imposto le posizioni iniziali di stampa per le linee?
171000110426      *  ?(orizzontali-§LOn e verticali-§LVn) e per i campi (§PSn)?
171100030915     c                   eval      §LO1 = §LO1 + 0.250 - 0.008
171200030915     c                   eval      §LO2 = §LO1 + 0.250
171300030915     c                   eval      §LV1 = §LO1
171400030915     c                   eval      §LV2 = §LO2 - 0.100
171500030915     c                   eval      §PS1 = §LO1 + 0.200
171600110426      * -?Stampa del singolo record?
171700030915     c                   write     A4prtFED3
171800030915e   1c                   ENDDO
171900030915      *
172000030915     c                   ENDSR
172100080918
172200080918      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
172300080918
172400080918     c     ListaScNS     BEGSR
172500080918      *
172600080918if  1c                   if        SDSprm > *zero and
172700080918     c                             £Mod   = 'S'
172800110426      * -?scarto il numero righe necessario?
172900080918     c                   eval      wMinLine = CurLineA4
173000080918if  2c                   if        §LO2     > §LO1
173100080918     c                   eval(h)   wMinLine = (§LO2 * 8) + 2
173200080918x   2c                   else
173300080918     c                   eval      wMinLine = CurLineA4 + 2
173400080918e   2c                   endif
173500080918do  2c                   dow       CurLineA4 < wMinLine
173600080918     c                   write     A4prtNUL
173700080918e   2c                   enddo
173800080918x   1c                   else
173900110426      * -?controllo l'overflow?
174000080918     c                   exsr      Test_OF
174100080918e   1c                   endif
174200080918      *
174300110426      * -?Stampa segnacolli dopo?
174400080918     c                   clear                   XX
174500080918     c                   if        Pncl <= 999
174600080918     c                   eval      YY    = Pncl
174700080918     c                   else
174800080918     c                   eval      YY    = 123
174900080918     c                   endif
175000080918     c                   eval      *in39 = *on
175100080918do  1c                   dow       YY > 0
175200080918     c                   exsr      StampaScNS
175300080918e   1c                   enddo
175400080918      *
175500080918     c                   ENDSR
175600030915
175700030915      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
175800030915
175900030915     c     StampaScNS    BEGSR
176000030915      *
176100110426      * -?In prima riga stampo la "intestazione"?
176200030915if  1c                   if        *in39
176300030915if  2c                   if        *in69
176400110426      * ·?foglio "A5"?
176500030915     c                   eval      YY      = YY  + 1
176600030915     c                   eval      $NP(yy) = C_ScNS + ':'
176700030915x   2c                   else
176800110426      * ·?foglio "A4"?
176900080922     c                   eval      Wsc   = 1234567
177000080922     c                   eval      Pnsx  = %char(Wsc) + '/'
177100080922     c                   eval      Wsc   = Wsc + Pncl
177200080922     c                   eval      Pnsx  = %trimr(Pnsx) + %char(Wsc)
177300030915     c                   eval      Prig  = C_ScNS
177400030915     c                                   + '         (dal '
177500080922     c                                   + %trim( %subst(Pnsx: 1: 7) )
177600030915     c                                   + ' al '
177700080922     c                                   + %trim( %subst(Pnsx: 9: 7) )
177800030915     c                                   + ')'
177900030915     c                   exsr      StampaGEN
178000030915e   2c                   endif
178100080922     c                   eval      Pnsx  =  'NON SEQUENZIALI'
178200030915     c                   eval      *in39 = *off
178300030915     c                   clear                   XX
178400030915     c                   clear                   $SC1
178500030915     c                   clear                   $SC2
178600030915if  2c                   if        SDSprm < 2
178700080922     c                   movel     $LunghN       Pnsx
178800030915     c                   reset                   Wsc
178900030915x   2c                   else
179000080922     c                   eval      Pnsx  =  'NON SEQUENZIALI'
179100030915     c                   eval      Wsc   =   34566
179200030915x   2c                   endif
179300030915x   1c                   else
179400030915      *
179500110426      * -?Prepara elenco?
179600030915     c                   clear                   $SC1
179700030915     c                   clear                   $SC2
179800030915do  2c                   do        wElSNS        xx
179900030915if  3c                   if        yy    = 0
180000030915     c                   leave
180100030915e   3c                   endif
180200030915     c                   add       xx            Wsc
180300030915     c                   eval      $SC1(xx) = Wsc
180400030915     c                   eval      yy    = yy - 1
180500030915e   2c                   enddo
180600030915     c                   eval      $SC2  = %editc($SC1 : 'Z')
180700030915     c                   movea     $SC2          Prig
180800030915      *
180900030915e   1c                   endif
181000030915      *
181100110426      * -?Al riempimento della riga: stampo la parte di elenco segnacolli?
181200030915if  1c                   if        *in69
181300110426      * ·?foglio "A5"?
181400030915     c                   eval      YY      = YY  + 1
181500030915     c                   movea(p)  $SC2          $NP(yy)
181600030915x   1c                   else
181700110426      * ·?foglio "A4"?
181800030915     c                   movea(p)  $SC2          Prig
181900030915     c                   exsr      StampaGEN
182000030915e   1c                   endif
182100030915if  1c                   if        XX  <  *hival
182200030915     c                   clear                   XX
182300030915e   1c                   endif
182400030915     c                   clear                   $SC1
182500030915     c                   clear                   $SC2
182600030915      *
182700030915     c                   ENDSR
182800030915
182900030915      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
183000030915
183100030915     c     StampaGEN     BEGSR
183200030915      *
183300110426      * -?Controllo se è stato raggiunto l'overflow nel formato in uso?
183400030915     c                   exsr      Test_OF
183500030915      *
183600110426      * -?Stampo la riga PRTGEN?
183700110426      *  ?Se sto stampando una "testata" (*in39) salto una riga prima?
183800110426      *  ?ed una riga dopo (per spaziare)?
183900030915if  1c                   if        *in39 = *on
184000030915     c                   write     A4prtNUL
184100030915e   1c                   endif
184200030915     c                   write     A4prtGEN
184300030915if  1c                   if        *in39 = *on
184400030915     c                   write     A4prtNUL
184500030915e   1c                   endif
184600030915      *
184700030915     c                   ENDSR
184800030623
184900030623      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
185000030623
185100030623     c     DecodPO       BEGSR
185200030623      *
185300040223     c     DUTpou        chain     AZORG
185400030623if  1c                   if        not %found(AZORG01L)
185500030623     c                             or  ORGfva <> *blanks
185600030623     c                   clear                   ORGdes
185700030623     c                   clear                   ORGind
185800030623     c                   clear                   ORGcpf
185900030623     c                   clear                   ORGloc
186000030623     c                   clear                   ORGpro
186100030623     c                   clear                   ORGtel
186200030623     c                   clear                   ORGfax
186300030623e   1c                   endif
186400030623      *
186500030623     c                   ENDSR
186600030623
186700030623      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
186800030623
186900080606     c     RiempiLDV     BEGSR
187000030623      *
187100110426      * -?Indicatori utilizzati:?
187200030623     c                   exsr      ImpIND
187300030623      *
187400110426      * -?Impotazione barcode?
187500140917      /free
187600140917        clear  BarCodeDS;
187700140917        If  SDSprm < 2;
187800140917          BARcdeAAS  = 78;
187900140917          BARcdeLNP  = 123;
188000140917          BARcdeNRS  = 12;
188100140917          BARcdeNSP  = 1234567;
188200140917        Else;
188300140917          BarCodeDS  = C_bar128;
188400140917          BARcdeAAS  = uYear;
188500140917          if  SDSprm >= 2;
188600140917            BARcdeNSP += (wwwPag - 1);
188700140917          endif;
188800140917          clear BARcdeXXX;
188900140917        EndIf;
189000140917      /end-free
189100140917      *
189200030623if  1c                   if            SDSprm <= 1
189300030623     c                             or  SDSprm >  1
189400080624     c                             and £Part <> 'C'
189500080715      /free
189600140917        reset  TRUL28ds;
189700140915        I28cod     = BarCodeDS;
189800140915        trul28R1 ( trul28ds );
189900140917        if  O28err = *blank;
190000140915          Wbar128 = O28cod;
190100140915        endif;
190200080715      /end-free
190300030623     c                   eval      *in50 = *on
190400030623     c                   movel     Wbar128       Tbcd
190500030623e   1c                   endif
190600140915      * -?Impotazione BRTcode?
190700140915      /free
190800140915        reset  TRUL28ds0;
190900140915        trul28ds0.i280cod = BarCodeDS;
191000140915        trul28r0 ( TRUL28ds0 );
191100140915        if  trul28ds0.o280err <> *on;
191200140917          //TbrtC = trul28ds0.o280cod;
191300140917          TbrtC = %subst( trul28ds0.o280cod :  1 : 5 ) + ' ' +
191400140917                  %subst( trul28ds0.o280cod :  6 : 5 ) + ' ' +
191500140917                  %subst( trul28ds0.o280cod : 11 : 5 ) + ' ' +
191600140917                  %subst( trul28ds0.o280cod : 16 : 4 );
191700140915        endif;
191800140915      /end-free
191900030623      *
192000120530      * -?Costanti: dati BRT S.p.A.?
192100110426     c                   eval      TBAR  = $BRT
192200060912     c                   eval      TBA1  = $Intest1
192300060912     c                   eval      TBA2  = $Intest2
192400080627     c                   eval      TBA3  = $Intest3
192500060912     c                   eval      TBA4  = $Intest4
192600060912     c                   eval      TBA5  = $Intest5
192700060912     c*** NON sopra:     eval      TBA6  = $Intest6
192800120530     c*//                eval      TBA7  = $Intest7
192900120530     c                   eval      TBA7  = $Intest7_brt
193000110426      * -?Costanti: "RISTAMPA"?
193100030623if  1c                   if        SDSprm < 2
193200030623     c***                eval      Tkri2 = 'RISTAMPA  '
193300030623e   1c                   endif
193400110426      * -?Costanti: "PER IL VETTORE" / "PER IL DESTINATARIO"?
193500030623if  1c                   if        SDSprm < 2
193600030623     c***                eval      Tcxv  = 'COPIA PER IL VETTORE ...+'
193700030623     c                   eval      Tcxd  = 'COPIA PER IL DESTINATARIO'
193800130729     c                   eval      Tcxd2 = Tcxd
193900030623x   1c                   else
194000030623     c***                eval      Tcxv  = 'COPIA PER IL VETTORE'
194100030623     c                   eval      Tcxd  = 'COPIA PER IL DESTINATARIO'
194200130729     c                   eval      Tcxd2 = Tcxd
194300030623e   1c                   endif
194400120530      * -?Costanti: "BRT S.P.A." per Quietanza?
194500030623if  1c                   if        SDSprm < 2
194600130418     c                   movel     $LunghA       Pbrt2
194700030623x   1c                   else
194800030623if  2c                   if        *in36
194900130418     c                   eval      Pbrt2 = $BRT
195000030623e   2c                   endif
195100030623e   1c                   endif
195200110426      * -?Costanti: "PRIVATO" / "APPUNTAMENTO"?
195300030623if  1c                   if        SDSprm < 2
195400090217     c***                evalr     Ppad = 'CONSEGNA GARANTITA'
195500090217     c                   evalr     Ppad = 'SERVIZIO ESPRESSO WUERTH'
195600090217     c*****              evalr     Psew = 'SERVIZIO ESPRESSO WUERTH'
195700030623x   1c                   else
195800080723     c***                evalr     Ppad = 'PRIVATO'
195900080723     c                   evalr     Ppad = 'CONSEGNA AD UFFICI'
196000030623e   1c                   endif
196100110426      * -?Costanti: "DDT"?
196200080721if  1c                   if        SDSprm < 2
196300081117     c                   evalr     Pddt   = 'PACKING-LIST g/DDT +
196400081117     c                                       sul collo'
196500080721x   1c                   else
196600081117     c                   evalr     Pddt   = 'PACKING-LIST GIA'' +
196700081117     c                                       CONSEGNATA'
196800081117     c***                evalr     Pddt   = 'PACKING-LIST già +
196900081117     c***                                    consegnata'
197000081117     c***                evalr     Pddt   = 'DDT sul collo'
197100080721e   1c                   endif
197200031103      *
197300110426      * -?Sigla operatore video?
197400030623     c                   movel     'X'           Psop
197500110426      * -?Mittente?
197600030623if  1c                   if        SDSprm < 2
197700030623     c                   movel     $LunghN       Pccm
197800030623     c                   movel     $Lungh        Prsm
197900030623     c                   movel     $LunghA       Pinm
198000030623     c                   movel     $LunghN       Pcam
198100030623     c                   movel     $Lungh        Plom
198200030623     c                   movel     $LunghA       Pprm
198300030623     c                   movel     $LunghA       Pnzm
198400030623     c                   eval      Pkmo  = 'MITT. ORIGINALE'
198500030623     c                   movel     $Lungh        Prmo
198600030623x   1c                   else
198700130418if  2c                   if        SDSprm > *zero and
198800060912     c                             £Mod   = 'k'
198900080715     c                   eval      Pccm  = '0011234'
199000031105     c                   eval      Prsm  = $Cli(21)
199100031105     c                   eval      Pinm  = $Cli(22)
199200031105     c                   eval      Pcam  = $Cli(23)
199300031105     c                   eval      Plom  = $Cli(24)
199400031105     c                   eval      Pprm  = $Cli(25)
199500031105     c                   eval      Pnzm  = $Cli(26)
199600031105x   2c                   else
199700040223     c                   movel     DUTpou        Pccm
199800080624     c                   move      '1234'        Pccm
199900030623     c                   eval      Prsm  = $Cli(1)
200000030623     c                   eval      Pinm  = $Cli(2)
200100030623     c                   eval      Pcam  = $Cli(3)
200200030623     c                   eval      Plom  = $Cli(4)
200300030623     c                   eval      Pprm  = $Cli(5)
200400030623     c                   eval      Pnzm  = $Cli(6)
200500030623     c                   eval      Pkmo  = 'MITT. ORIGINALE'
200600030623     c                   eval      Prmo  = $Cli(7)
200700031105e   2c                   endif
200800030623e   1c                   endif
200900110426      * -?Destinatario?
201000030623if  1c                   if        SDSprm < 2
201100030623     c                   movel     $LunghN       Pccd
201200090217     c                   movel     $LunghW       Prsd
201300090217     c                   movel     $Lungh        Prse
201400030623     c                   movel     $LunghA       Pind
201500030623     c                   movel     $LunghN       Pcad
201600030623     c                   movel     $Lungh        Plod
201700030623     c                   movel     $LunghA       Pprd
201800030623     c                   movel     $LunghA       Pnzd
201900030623x   1c                   else
202000080624     c                   eval      Pccd  = '0438888'
202100030623     c                   eval      Prsd  = $Cli(8)
202200030623     c                   eval      Prse  = $Cli(9)
202300030623     c                   eval      Pind  = $Cli(10)
202400030623     c                   eval      Pcad  = $Cli(11)
202500030623     c                   eval      Plod  = $Cli(12)
202600030623     c                   eval      Pprd  = $Cli(13)
202700030623     c                   eval      Pnzd  = $Cli(14)
202800030623e   1c                   endif
202900110426      * -?Fil. Arrivo?
203000080721     c                   eval      Pkla   =  'FILIALE'
203100030623if  1c                   if        SDSprm < 2
203200080702     c                   eval      Plna1  =  '123'
203300080702     c                                    +  ' '
203400080702     c                                    +  'ABCDEFGHIJKLMNOPQRST'
203500080702     c                                    +  ' - '
203600080702     c                                    +  'INDIRIZZO-KLMNOPQRST'
203700080702     c                                    +  ' Tel.'
203800080702     c                                    +  '1234/67890123'
203900080702     c                   eval      Plna2  =  '01234 '
204000080702     c                                    +  'LOCALITA-JKLMNOPQRST '
204100080702     c                                    +  'PR'
204200080702     c                                    +  '                  '
204300080702     c                                    +  ' Fax '
204400080702     c                                    +  '1234/67890123'
204500030623x   1c                   else
204600090406     c                   eval      Plna1  =  %editc(ORGfil: 'X')
204700080702     c                                    +  ' '
204800080702     c                                    +  ORGdes
204900080702     c                                    +  ' - '
205000080702     c                                    +  ORGind
205100080702     c                                    +  ' Tel.'
205200130418     c                                    +  %trimr(ORGtel)
205300080702     c                   eval      Plna2  =  %editc(ORGcpf : 'X')
205400080702     c                                    +  ' '
205500080702     c                                    +  ORGloc
205600080702     c                                    +  ' '
205700080702     c                                    +  ORGpro
205800080703     c                                    +  '                  '
205900080702     c                                    +  ' Fax '
206000130418     c                                    +  %trimr(ORGfax)
206100030623e   1c                   endif
206200110426      * -?Spedizione?
206300030623if  1c                   if        SDSprm < 2
206400030623     c                   movel     $LunghN       Plnp
206500030623     c                   movel     $LunghN       Pnrs
206600030623     c                   movel     $LunghN       Pnsp
206700030623     c                   movel     $LunghN       Pdsp
206800140922     c                   movel     $LunghN       Pdsp6
206900030623x   1c                   else
207000080715     c***                eval      Plnp  = DUTpou
207100080715     c                   eval      Plnp  = BARcdeLNP
207200080715     c                   eval      Pnrs  = BARcdeNRS
207300080715     c                   eval      Pnsp  = BARcdeNSP
207400130402     c                   eval      Pdsp  = *date
207500140922     c                   eval      Pdsp6 = Udate
207600030623e   1c                   endif
207700140915      * -?BRTcode?
207800140917     c*// già fatto:     eval      TbrtC = trul28ds0.o280cod
207900140917      *///free
208000140917      *//    TbrtC = %subst( trul28ds0.o280cod :  1 : 5 ) + ' ' +
208100140917      *//            %subst( trul28ds0.o280cod :  6 : 5 ) + ' ' +
208200140917      *//            %subst( trul28ds0.o280cod : 11 : 5 ) + ' ' +
208300140917      *//            %subst( trul28ds0.o280cod : 16 : 4 );
208400140917      *///end-free
208500110426      * -?Riferimento Mittente?
208600030623if  1c                   if        SDSprm < 2
208700030623     c                   movel     $LunghN       Prmn
208800030623     c                   movel     $LunghA       Prma
208900030623x   1c                   else
209000030623     c                   eval      Prmn  = 210987654321
209100030623     c                   eval      Prma  = 'ABC-EF.......PQ'
209200030623e   1c                   endif
209300110426      * -?Nominativo e telefono?
209400030623if  1c                   if        SDSprm < 2
209500030623     c                   movel     $Lungh        Pnom
209600030623     c                   movel     $Lungh        Ptel
209700030623x   1c                   else
209800030623     c                   eval      Pnom  = 'CELESTI LUIGI +----2----+----3----+'
209900030623     c                   eval      Ptel  = '051/123456789012'
210000030623e   1c                   endif
210100110426      * -?Servizio?
210200030623if  1c                   if        SDSprm < 2
210300030623     c                   movel     $LunghA       Pds0
210400030623     c                   movel     $Lungh        Pds1
210500030623     c                   movel     $LunghA       Pds2
210600070226     c                   eval      *in37 = *on
210700030623x   1c                   else
210800080624     c***                eval      Pds0  = 'RESO        '
210900030623     c                   eval      Pds1  = 'FRANCO    +C/ASSEGNO   '
211000030623     c                   eval      Pds2  = 'ESPRESSO    '
211100070226     c                   eval      *in37 = *on
211200030623e   1c                   endif
211300110426      * -?Zona / Costante "*" / Costante "Fermo Deposito"?
211400030623if  1c                   if        SDSprm < 2
211500030623     c                   movel     $LunghA       Pznc
211600070215     c                   movel     'D'           Pdis
211700030623     c                   movel     'A'           Pass
211800030623     c                   movel     '*'           Past
211900101124     c                   movel     $LunghA       Pndc
212000101124     c                   movel     'd:'          Pndc
212100040223     c                   movel     'V'           Pbva
212200030623     c                   movel     $LunghA       Pffd
212300030623x   1c                   else
212400030623     c                   eval      Pznc  = 54
212500070215     c***                eval      Pdis  = 'D'
212600080624     c                   eval      Pass  = 'A'
212700030623     c                   eval      Past  = '*'
212800101124     c***                eval      Pndc  = 'g:1234567890m'
212900101124     c                   eval      Pndc  = 'd:12345 (1234)'
213000040223     c                   eval      Pbva  = 'V'
213100080701     c*****              eval      Pffd  = 'FERMO DEPOSITO '
213200030623e   1c                   endif
213300110426      * -?Colli: numero, peso, volume, flag-tipo, descrizione?
213400030623if  1c                   if        SDSprm < 2
213500030623     c                   movel     $LunghN       Pncl
213600030623     c                   movel     $LunghN       Ppkf
213700030623     c                   movel     $LunghN       Pvlf
213800101201     c                   movel     'W'           Pfvf
213900101201     c                   movel     $Lungh        Pcpp
214000030623     c                   movel     $Lungh        Pnat
214100030623x   1c                   else
214200031105sel 2c                   select
214300031105w   2c                   when          SDSprm > 0
214400030623     c                             and £Mod   = 'Q'
214500030623     c                   eval      Pncl  = wElSNS
214600031105w   2c                   when          SDSprm > 0
214700031105     c                             and £Mod   = 'k'
214800031105     c                   eval      Pncl  = 1
214900031105x   2c                   other
215000030623     c                   eval      Pncl  = 100
215100031105e   2c                   endsl
215200031105if  2c                   if            SDSprm > 0
215300031105     c                             and £Mod   = 'k'
215400031105     c                   eval      Ppkf  = 2,3
215500031105     c                   eval      Pvlf  = 1,234
215600031105x   2c                   else
215700030623     c                   eval      Ppkf  = 56,7
215800030623     c                   eval      Pvlf  = 2,345
215900031105e   2c                   endif
216000101201     c                   eval      Pfvf  = 'R'
216100101201     c                   eval      wDateDMY = %date() - %days(1)
216200101201     c                   eval      Pcpp  = 'Consegnati 12345 +
216300101201     c                                      colli al ' +
216400101201     c                                   %editc( %dec(wDateDMY) : 'Y' )
216500030623     c                   eval      Pnat  = 'CARTONI'
216600030623e   1c                   endif
216700110426      * -?Segnacolli?
216800030623if  1c                   if        SDSprm < 2
216900030623     c                   movel     $LunghN       Ppos
217000030623     c                   movel     $LunghN       Psrs
217100080922     c                   movel     $LunghN       Wsc
217200080922     c***                eval      Pnsx = %editc(Wsc : 'Z') + '/'
217300080922     c***                               + %editc(Wsc : 'Z')
217400080922     c                   movel     $LunghN       Pnsx
217500030623x   1c                   else
217600040223if  3c                   if        DUTpou <> *zeros
217700040223     c                   move      DUTpou        Ppos
217800031103e   3c                   endif
217900030623     c                   eval      Psrs  = 88
218000030623sel 2c                   select
218100060912w   2c                   when          SDSprm > *zero and
218200060912     c                                (£Mod   = 's'
218300030623     c                             or  £Mod   = 'S'
218400030623     c                             or  £Mod   = 'Q'
218500060912     c                             and wElSNS >  2)
218600080922     c                   eval      Pnsx  =  'NON SEQUENZIALI'
218700060912w   2c                   when          SDSprm > *zero
218800060912     c                             and £Mod   = 'Q'
218900030623     c                             and wElSNS <= 2
219000080922     c                   eval      Wsc   = 1234567
219100080922     c                   eval      Pnsx = %editc(Wsc : 'Z')
219200030623if  3c                   if        wElSNS = 2
219300080922     c                   eval      Wsc   = Wsc + wElSNS
219400080922     c                   eval      Pnsx = %trimr(Pnsx) + '+'
219500080922     c                                  + %editc(Wsc : 'Z')
219600030623e   3c                   endif
219700060912w   2c                   when      SDSprm > *zero and
219800060912     c                             £Mod   = 'k'
219900080922     c                   eval      Wsc   = 1234567
220000080922     c                   eval      Pnsx  = %editc(Wsc : 'Z')
220100030623x   2c                   other
220200080922     c                   eval      Wsc   = 1234567
220300080922     c                   eval      Pnsx  = %editc(Wsc : 'Z') + '/'
220400080922     c                   eval      Wsc   = Wsc + Pncl
220500080922     c                   eval      Pnsx  = %trimr(Pnsx)
220600080922     c                                   + %editc(Wsc : 'Z')
220700030623e   2c                   endsl
220800030623e   1c                   endif
220900110426      * -?Consegne particolari?
221000030623if  1c                   if        SDSprm < 2
221100030623     c                   movel     $Lungh        Pcp1
221200030623     c                   movel     $Lungh        Pcp2
221300030623x   1c                   else
221400030623     c                   eval      Pcp1 = 'PER APPUNTAMENTO           '
221500030623     c                   eval      Pcp2 = 'AI PIANI                   '
221600030623e   1c                   endif
221700110426      * -?Turni di chiusura?
221800061107     c                   eval      Pktc   =  'TURNO DI CHIUSURA'
221900030623if  1c                   if        SDSprm < 2
222000030623     c                   movel     $Lungh        Pgc1
222100030623     c                   movel     $Lungh        Pgc2
222200030623x   1c                   else
222300030623     c                   eval      Pgc1 = 'MERCOLEDÌ MATT.'
222400030623     c                   eval      Pgc2 = 'MERCOLEDÌ POM. '
222500030623e   1c                   endif
222600110426      * -?Data e ora consegna richiesta?
222700030623if  1c                   if        SDSprm < 2
222800061123     c                   movel     'TASSATIVA'   Pcot
222900030623     c                   movel     'PRIMA DELLE' Ptdr
223000030623     c                   movel     $LunghN       Pdcr
223100030623     c                   movel     $LunghN       Phcr
223200030623x   1c                   else
223300061123     c                   eval      Pcot = 'TASSATIVA'
223400030623     c                   eval      Ptdr = 'PRIMA DEL  '
223500030623     c                   eval      Pdcr = *date
223600030623     c                   eval      Phcr = 1830
223700030623e   1c                   endif
223800030623     c     Phcr          comp      0                                  1818
223900110426      * -?Note?
224000030623if  1c                   if        SDSprm < 2
224100030623     c                   eval      *in16 = *on
224200031105     c                   eval      Pknp  = 'NOTE  E  PARTICOLARITÀ'
224300030623     c                   movel     $Lungh        Pnp1
224400030623     c                   movel     $LunghA       Pnp2
224500030623x   1c                   else
224600030623if  2c                   if             SDSprm > 1
224700080624     c                             and (£Part <> 'A'
224800080624     c                             and  £Part <> 'Z')
224900030623     c                   eval      *in16 = *on
225000031105     c                   eval      Pknp  = 'NOTE  E  PARTICOLARITÀ'
225100030623     c                   eval      Pnp1  = %subst($Note(1):1:50)
225200030623     c                   eval      Pnp2  = %subst($Note(2):1:50)
225300030623e   2c                   endif
225400030623e   1c                   endif
225500110426      * -?Particolarità?
225600030623if  1c                   if        SDSprm < 2
225700030623     c                   eval      *in16 = *on
225800031105     c                   eval      Pknp  = 'NOTE  E  PARTICOLARITÀ'
225900030623     c                   movel     $Lungh        Pnp3
226000030623     c                   movel     $LunghA       Pnp4
226100030623     c                   movel     $LunghA       Pnp5
226200030623     c                   movel     $LunghA       Pnp6
226300140915     c*//                movel     $Lungh        Pnp7
226400140915     c*//                movel     $Lungh        Pnp8
226500030623x   1c                   else
226600030623if  2c                   if            (SDSprm > 0
226700030623     c                             and  £Mod   = 'Q')
226800030623     c                             or  (SDSprm > 1
226900080624     c                             and (£Part <> 'A'
227000080624     c                             and  £Part <> 'Z'))
227100030623     c                   eval      *in16 = *on
227200031105     c                   eval      Pknp  = 'NOTE  E  PARTICOLARITÀ'
227300030623     c                   eval      Pnp3  = %subst($Note(3):1:50)
227400030623     c                   eval      Pnp4  = %subst($Note(4):1:50)
227500030623if  3c                   if            SDSprm >  0
227600030623     c                             and £Mod   = 'Q'
227700030623     c                             and wElSNS >  2
227800030623     c                             and wElSNS <= 4
227900110426      * -?elenco segnacolli non sequenziali tra le particolarità?
228000030623     c                   eval      Pnp5  = 'ELENCO SEGNACOLLI NON SEQUENZIALI:'
228100030623     c                   clear                   $SC1
228200030623     c                   clear                   $SC2
228300030623     c                   eval      Wsc   =   34566
228400030623do  4c                   do        wElSNS        xx
228500030623     c                   add       xx            Wsc
228600030623     c                   eval      $SC1(xx) = Wsc
228700030623e   4c                   enddo
228800030623     c                   eval      $SC2  = %editc($SC1:'Z')
228900030623     c                   movea     $SC2          Pnp6
229000030623x   3c                   else
229100030623     c                   eval      Pnp3  = %subst($Note(3):1:50)
229200030623     c                   eval      Pnp4  = %subst($Note(4):1:50)
229300030623     c                   eval      Pnp5  = %subst($Note(5):1:50)
229400030623     c                   eval      Pnp6  = %subst($Note(6):1:50)
229500101124     c*//                eval      Pnp7  = 'Consegnare al destinatario +
229600101124     c*//                                   D.D.T. e'
229700101124     c*//                eval      Pnp8  = 'D.A.S.                     +
229800101124     c*//                                           '
229900081118     c***                eval      PnpX  = '*...+....1....+....2....+..+
230000081118     c***                                   ..3....+'
230100030623e   3c                   endif
230200030623e   2c                   endif
230300030623e   1c                   endif
230400110426      * -?Importi da fatturare: imponibile / iva-esenzione / totale?
230500030623if  1c                   if        SDSprm < 2
230600080630     c                   z-add     1             w11d2
230700080701     c                   eval      Prcv   =  'Art. 1696 c.c. mod. +
230800080630     c                                       dlgs 286/2005 '
230900080630     c                                    +  'EUR' + ' '
231000080630     c                                    +  %trim(%editc(w11d2 : '4'))
231100080630     c                                    +  ' al kg.'
231200130628     c                   z-add     §GEIbl        w11d2
231300080702     c                   eval      Pbav1  =  'Bollo '
231400080702     c                                    +  %trim(%editc(w11d2:'4'))
231500080702     c                                    +  ' ' + 'EUR'
231600080702     c                                    +  ' assolto in modo virtuale '
231700080702     c                   eval      Pbav2  =  'Aut. Ag. Ent. MI 2 10/07/06 '
231800080702     c                                    +  'Prot. 45114'
231900121116     c*//                eval      Ppiv   =  'Codice Fiscale: WWWWWW12W88W944W'
232000121116     c*//                eval      PpivM  =  Ppiv
232100121116     c*//                eval      PpivD  =  Ppiv
232200030623     c                   eval      *in19 = *on
232300031103     c                   eval      Pkim  = 'IMPONIBILE'
232400130218     c*//                movel     $LunghN       Pimp
232500130218     c                   movel     $LunghA       Pimp
232600031103     c                   eval      Pkie  = 'I.V.A.ONE'
232700030623     c                   movel     $LunghA       Piva
232800031103     c                   eval      Pktf  = 'TOTALE FATTURA'
232900130218     c*//                movel     $LunghN       Pift
233000130218     c                   movel     $LunghA       Pift
233100031104     c***                movel     'EUR'         Pdiv
233200030623x   1c                   else
233300030623if  2c                   if             SDSprm > 1
233400080624     c                             and (£Part <> 'B'
233500080624     c                             and  £Part <> 'Z')
233600080630     c                   z-add     1             w11d2
233700080701     c                   eval      Prcv   =  'Art. 1696 c.c. mod. +
233800080630     c                                       dlgs 286/2005 '
233900080630     c                                    +  'EUR' + ' '
234000080630     c                                    +  %trim(%editc(w11d2 : '4'))
234100080630     c                                    +  ' al kg.'
234200130628     c                   z-add     §GEIbl        w11d2
234300080702     c                   eval      Pbav1  =  'Bollo '
234400080702     c                                    +  %trim(%editc(w11d2:'4'))
234500080702     c                                    +  ' ' + 'EUR'
234600080702     c                                    +  ' assolto in modo virtuale '
234700080702     c                   eval      Pbav2  =  'Aut. Ag. Ent. MI 2 10/07/06 '
234800080702     c                                    +  'Prot. 45114'
234900121116     c*//                eval      Ppiv   =  'Partita IVA: 12345678901'
235000121116     c*//                eval      PpivD  =  Ppiv
235100030623     c                   eval      *in19 = *on
235200031103     c                   eval      Pkim  = 'IMPONIBILE'
235300130418     c                   eval      w11d2 = 234.00
235400130418     c                   evalr     Pimp  = %editc( w11d2 : 'K' )
235500080624if  3c                   if        £Part = 'E'
235600031103     c                   eval      Pkie  = 'ESENZIONE'
235700080624     c                   clear                   w11d2
235800030623     c                   eval      Piva  = 'ES.ART.8/1 AB     '
235900030623x   3c                   else
236000031103     c                   eval      Pkie  = 'I.V.A.'
236100130418     c                   eval(h)   w11d2 = w11d2 * 20 / 100
236200130218     c                   evalr     Piva  = %editc(w11d2 : 'K')
236300030623e   3c                   endif
236400060912     c                   eval      Pktf  = 'TOTALE FATTURA'
236500130418     c                   eval      w11d2 = 234.00 + w11d2
236600130418     c                   evalr     Pift  = %editc(w11d2 : 'K')
236700030623     c***                eval      Pdiv  = 'EUR'
236800030623e   2c                   endif
236900030623e   1c                   endif
237000110426      * -?Fattura?
237100030623if  1c                   if        SDSprm < 2
237200030623     c                   eval      *in19 = *on
237300031104     c                   eval      Pkft  = 'FATTURA'
237400030623     c                   movel     $LunghN       Pnft
237500130215     c                   eval      Pfiv  = '(987)'
237600031104     c                   eval      Pkdf  = 'DEL'
237700030623     c                   movel     $LunghN       Pdft
237800030623x   1c                   else
237900030623if  2c                   if             SDSprm > 1
238000080624     c                             and (£Part <> 'B'
238100080624     c                             and  £Part <> 'Z')
238200030623     c                   eval      *in19 = *on
238300031104     c                   eval      Pkft  = 'FATTURA'
238400030623     c                   eval      Pnft  = 8765
238500130215     c                   eval      Pfiv  = '(432)'
238600031104     c                   eval      Pkdf  = 'DEL'
238700130215     c                   eval      Pdft  = Udate
238800030623e   2c                   endif
238900030623e   1c                   endif
239000110426      * -?Contrassegno?
239100030623if  1c                   if        SDSprm < 2
239200030623     c                   eval      *in20 = *on
239300030623     c                   movel     $LunghN       Pcas
239400030623     c                   movel     'EUR'         Pdca
239500030623     c                   movel     $Lungh        Ptic
239600091119     c                   movel     $Lungh        Pkic
239700091119     c                   movel     'INTESTARE A:'Pkic
239800100208     c                   eval      Pica = $Lungh
239900030623x   1c                   else
240000030623if  2c                   if             SDSprm > 1
240100080624     c                             and (£Part <> 'B'
240200080624     c                             and  £Part <> 'Z')
240300030623     c                   eval      *in20 = *on
240400030623     c                   eval      Pcas  = 123,45
240500030623     c***                eval      Pdca  = 'EUR'
240600080624     c                   eval      Ptic  = 'BC ASS.C/C CORRIERE/MITTENTE'
240700091119     c                   eval      Pkic  = 'INTESTARE A:'
240800091119     c                   eval      Pica  = $Cli(1)
240900030623e   2c                   endif
241000030623e   1c                   endif
241100110426      * -?Importi da esigere al ritiro o alla consegna?
241200030623if  1c                   if        SDSprm < 2
241300030623     c                   movel     $LunghN       Pies
241400030623     c                   movel     'EUR'         Pdie
241500031104     c                   movel     $LunghN       Pie1
241600031104     c                   movel     'DIV'         Pdi1
241700030623x   1c                   else
241800030623if  2c                   if             SDSprm > 1
241900080624     c                             and (£Part <> 'B'
242000080624     c                             and  £Part <> 'Z')
242100080624     c***                eval      Pdi1  = 'DIV'
242200080702     c**                 eval      Pie1  = %subst(%editc(Pcas : 'K')
242300080702     c**                                         : 3 : %len(Pie1))
242400080702     c**                 eval      Pies  = %subst(%editc(Pies : 'K')
242500080702     c**                                         : 3 : %len(Pies))
242600080702     c****               eval      w11d2 = Pcas + Pift
242700080702     c****               eval      Pies  = %editc(w11d2 : 'K')
242800130418     c                   eval      Pies  = Pcas + w11d2
242900031104     c***                eval      Pdie  = 'EUR'
243000030623e   2c                   endif
243100030623e   1c                   endif
243200080701     c                   eval      *in21  =  (Pies <> *zeros)
243300110426      * -?Altre costanti...?
243400080624if  1c***                if           SDSprm < 2
243500080624     c***                          or   SDSprm > 1
243600080624     c***                          and  £Parm <> '2'
243700080624if  1c                   if        *in35
243800091117      *Select;
243900110426      *  // -?costante per cliente?
244000110426      *  //  ?"Consorzio Lotterie Nazionali - Gratta & Vinci"?
244100091117      *  when  DB0ccm = c_CliCLN;
244200091117      *    Pkrm =  'LA FIRMA PER RICEVUTA DELLA MERCE VALE ANCHE PER +
244300091117      *             IL DDT N° '
244400091117      *         + %editc (WddtCLN : 'Z')
244500091117      *         + ' DEL '
244600091117      *         + %editc (WdspCLN : 'Y')
244700091117      *         + ' CON RISERVA DI CONTROLLO COME DA CONTRATTO C.L.N.';
244800110426      *  // -?costante per cliente?
244900110426      *  //  ?"S G Soluzioni Logistiche"?
245000091117      *  when  DB0ccm = c_CliSG;
245100091117      *    Pkrm = %trimr(Pkrm)
245200091117      *         + '.   INCLUDE CONTRATTO';
245300091117      *    if  DB0nas <> *blank;
245400091117      *      Pkrm = %trimr(Pkrm) + ' - '
245500091117      *           + %trim(DB0nas);
245600091117      *    endif;
245700091117      *    if  DB0rmo <> *blank;
245800091117      *      Pkrm = %trimr(Pkrm)
245900091117      *           + ' - MAT TV:'
246000091117      *           + %trim(DB0rmo);
246100091117      *    endif;
246200091117      *EndSl;
246300080317     c                   eval      Pkrm   =  'PER RICEVUTA DELLA MERCE-
246400080317     c                              E DEL DOCUMENTO DI TRASPORTO SENZA-
246500091117     c                              RISERVE' + $LunghA + $LunghA +
246600091117     c                              $LunghA
246700080605     c                   eval      Pkrm1 = '(nome e cognome in stampa+
246800080605     c                                       tello)'
246900080605     c                   eval      Pkrm2 = '(firma)'
247000080605     c                   eval      Pkrm3 = '(data e ora)'
247100080605e   1c                   endif
247200080605if  1c                   if        SDSprm < 2
247300080317     c                   eval      Pnux = 'NON UTILIZZARE PER LA CONSE-
247400080317     c                             GNA'
247500080317e   1c                   endif
247600030623      *
247700030623     c                   ENDSR
247800030623
247900030623      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
248000030623
248100030623     c     ImpIND        BEGSR
248200030623      *
248300110426      *?Indicatori utilizzati:?
248400110426      * -?Richiesta firma, data e ora consegna nella 1ª copia?
248500030623if  1c                   if             SDSprm < 2
248600030623     c                             or   SDSprm > 1
248700080624     c                             and  £Part <> '2'
248800030623     c                   eval      *in35 = *on
248900030623sel 2c                   select
249000110426      * - -?x DPD?
249100030623w   2c                   when          SDSprm > 0
249200030623     c                             and £Mod   = 'D'
249300030623     c                   eval      *in71 = *on
249400110426      * - -?x FedEx?
249500030623w   2c                   when          SDSprm > 0
249600030623     c                             and £Mod   = 'F'
249700030623     c                   eval      *in72 = *on
249800110426      * - -?x EuroExpress?
249900040223w   2c                   when          SDSprm > 0
250000040223     c                             and £Mod   = 'E'
250100040223     c                   eval      *in73 = *on
250200120530      * - -?x BRT?
250300030623x   2c                   other
250400030623     c                   eval      *in70 = *on
250500030623e   2c                   endsl
250600030623e   1c                   endif
250700110426      * -?Richiesta firma per quietanza nella 2ª copia?
250800030623     c                   if             SDSprm < 2
250900030623     c                             or   SDSprm > 1
251000080624     c                             and (£Part  = '2'
251100080624     c                              or  £Part  = '9')
251200030623     c                   eval      *in36 = *on
251300030623     c                   endif
251400030623      *
251500030623     c                   ENDSR
251600030623
251700030623      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
251800030623
251900030623     c     Test_OF       BEGSR
252000030623      *
252100110426      * -?Test raggiungimento della posizione limite?
252200030915if  1c                   if        §PS1      < $LimOF    and
252300030915     c                             CurLineA4 < wMaxLine
252400030915     c                   goto      No_OF
252500030915e   1c                   endif
252600030915      *
252700110426      * -?Stampa del box per dati da FIAR5?
252800110426      *  ?(se è già stata stampata almeno una riga di tale box)?
252900030915if  1c                   if        $Fiar5 = *on  and  $First = *off
253000030915     c                   eval      §LO2   = §PS1 + 0.050
253100030915     c                   write     A4prtAR53
253200030915e   1c                   endif
253300030915      *
253400110426      * -?Stampa dell'indicazione "Segue..." su pagina corrente?
253500030915     c                   clear                   Pseg0
253600030915     c                   clear                   Pseg1
253700030915     c                   eval      Pseg9  = '  Segue...'
253800030915     c                   write     A4prtSEG
253900030915      *
254000110426      * -?Se sono ancora nella prima pagina =>?
254100110426      *  ?stampo anche i dati per il destinatario e box/barcode/ecc.?
254200030915if  1c                   if        wPage < 2
254300080704if  2c                   if        £Mod   <> '4'
254400080605      /free
254500110426         // ·?Posizionamento nel foglio A4 per copia per il destinatario?
254600080605         *in41 = *off;
254700080605         *in42 = *off;
254800080605         *in43 = *off;
254900080605         *in44 = *off;
255000080605         *in42 = (wSeqLdV = 1);
255100080605         *in44 = (wSeqLdV = 2);
255200080605         WRITE  A4prtPOS;
255300080605      /end-free
255400110426      * ·?Stampa 2ª parte della bolla (da lasciare al cliente)?
255500130418     c                   eval      *in33 = *on
255600130418     c                   write     A4prtLDV
255700110426      * ·?Stampa "cornice" / barcode / righine varie...?
255800130418     c                   write     A4prtBOX
255900080703e   2c                   endif
256000030915e   1c                   endif
256100030915      *
256200110426      * -?Salta a pagina successiva e indica "(Seguito)"?
256300030915     c                   write     A4prtNUL
256400030915     c                   write     A4prtSP0
256500030915     c                   eval      wPage  = wPage + 1
256600030915     c                   eval      Pseg0  = 'Spedizione '
256700110321     c                                    + %editc(Plnp : 'X') + ' '
256800110321     c                                    + %editc(Pnrs : 'X') + ' '
256900110321     c                                    + %editc(Pnsp : 'X')
257000030915     c                                    + ' del ' + %editc(Pdsp : 'Y')
257100030915     c                   eval      Pseg1  = '(Seguito)             Pag.'
257200030915     c                                    + %editc(wPage:'Z')
257300030915     c                   clear                   Pseg9
257400030915     c                   write     A4prtSEG
257500030915      *
257600110426      * -?Stampa il numero pagina sulla nuova?
257700080901     c***                write     A4prtPAG2
257800110426      *                  c
257900110426      * -?Reimposta posizioni iniziali di stampa per linee?
258000110426      *  ?(orizzontali-§LOn e verticali-§LVn) e campi (§PSn).?
258100110426      *  ?Nelle pagine dalla 2ª in poi si parte da in alto:?
258200110426      *  ?non vengono ri-stampati i 2 box "vettore" e "destinatario".?
258300030915sel 1c                   select
258400120530      * ·?BRT?
258500030915w   1c                   when      *in70
258600030915     c                   eval      §LO1   = 0.950
258700030915     c                   eval      §LO2   = §LO1
258800030915     c                   eval      §PS1   = §LO1
258900110426      * ·?FedEx?
259000030915w   1c                   when      *in72
259100030915     c                   eval      §LO1   = 1.600
259200030915     c                   eval      §LO2   = §LO1 + 0.425
259300030915     c                   eval      §LV1   = §LO1 + 0.300
259400030915     c                   eval      §LV2   = §LV1 + 0.070
259500030915     c                   write     A4prtFED1
259600030915do  2c                   dow       CurLineA4 < $MinLine1
259700030915     c                   write     A4prtNUL
259800030915e   2c                   enddo
259900030915     c                   write     A4prtFED2
260000030915     c                   eval      §LO1   = §LO2 - 0.250
260100030915     c                   eval      §PS1   = §LO1 + 0.150
260200110426      * ·?EuroExpress?
260300110426      *  ?(non più gestito)?
260400040223w   1c                   when      *in73
260500030915      *
260600030915e   1c                   endsl
260700030915      *
260800110426      *?N.B.:?Per i SegnaColli non sequenziali non servono queste?
260900110426      *      ?impostazioni: trattasi di stampa con salto riga -skipb-?
261000110426      *      ?quindi sono già posizionato.?
261100030623      *
261200030623     c     No_OF         ENDSR
261300030623
261400030623      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
261500030623
261600030915     c     Rep_§LO2      BEGSR
261700030915      *
261800030915sel 1c                   select
261900030915      *
262000110426      * -?Se ho stampato l'elenco dei segnacolli non sequenziali, devo?
262100110426      *  ?"allineare" il posizionamento in pollici all'ultima riga?
262200110426      *  ?stampata (in ottavi di pollice).?
262300110426      *  ?=>   Num_ultima_riga_stampata / 8?
262400031103w   1c                   when      SDSprm  > *zero and
262500031103     c                             (£Mod   = 'S'  or
262600031103     c                              £Mod   = 's'  or
262700031104     c                              £Mod   = 'K'  or
262800031103     c                              £Mod   = 'Q'   and
262900031103     c                              wElSNS > 4)
263000030915     c                             and NOT *in69
263100030915     c                   eval(h)   §LO2   = CurLineA4 / 8
263200030915      *
263300110426      * -?Se non ho stampato né elenco segnacolli non sequenziali né?
263400110426      *  ?altri box, devo impostare comunque i dati per il posiziona-?
263500110426      *  ?mento di questo primo box?
263600030915w   1c                   when      §LO2   = *zeros
263700081127     c                   eval      §LO1   = 2.850
263800030915     c                   eval      §LO2   = §LO1
263900030915      *
264000030915e   1c                   endsl
264100030915      *
264200030915     c                   ENDSR
264300030915
264400030915      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
264500030915
264600030915     c     QUM_FNORM     BEGSR
264700030915      *
264800030915     c                   clear                   Wx
264900110426      * -?Colli?
265000030915if  1c                   if        Wx   < %elem($Qum)
265100030915     c                   add       1             wx
265200030915     c                   eval      $Qum(wx) = %editc(123 : '4')
265300030915     c                                      + ' colli'
265400030915e   1c                   endif
265500110426      * -?Peso?
265600030915if  1c                   if        Wx   < %elem($Qum)
265700030915     c                   add       1             wx
265800030915     c                   eval      $Qum(wx) = %editc(456.78 : '4')
265900030915     c                                      + ' kg'
266000030915e   1c                   endif
266100030915      *
266200030915     c                   ENDSR
266300030915
266400030915      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
266500030915
266600110426**   ?$CLI?                                      *
266700030623ROSSI ERNESTO ....................*                 1
266800030623VIA DELLA FANTASIA, 99 ...........*                 2
26690003062340138                                               3
267000030623BOLOGNA ..........................*                 4
267100030623BO                                                  5
267200030623                                                    6
267300030623ARANCIONI ERMENEGILDO ............*                 7
267400030623VERDI GIUSEPPE ...................*                 8
267500030623(ESTENSIONE) .....................*                 9
267600030623VIA DELLA MUSICA, 10 .............*                10
26770003062337128                                              11
267800030623VERONA ...........................*                12
267900030623VR                                                 13
268000030623                                                   14
268100030915INZAGHI PLUTO ....................*                15
268200030915VIA DEI PAGLIACCI, 123 ...........*                16
26830003091540999                                              17
268400030915BOLOGNA ..........................*                18
268500030915BO                                                 19
268600030915                                                   20
268700031105KARNAK S.A (SM 00251)                              21
268800031105STRADA FONTESCARA 15                               22
26890003110547894                                              23
269000031105CHIESANUOVA                                        24
269100031105RN                                                 25
269200031105                                                   26
269300110426**   ?$NOTE?                                  50>*                70>*
269400030623TEST * NOTA 01 ....2....+....3....+....4....+....5
269500030623TEST * NOTA 02 ....2....+....3....+....4....+....5
269600030623TEST * PARTICOLARITA' 01 ....3....+....4....+....5
269700030623TEST * PARTICOLARITA' 02 ....3....+....4....+....5
269800030623TEST * PARTICOLARITA' 03 ....3....+....4....+....5
269900030623TEST * PARTICOLARITA' 04 ....3....+....4....+....5
270000030623TEST ....1....+....2....+....3....+....4....+....5
270100030623EPAL                   N. 2
270200030623EUR                    N. 3
270300030623MOTIVO DELLA VARIAZIONE +....3....+....4....+....5....+....6....+....7....+....8
270400030623MOTIVO DELLA VARIAZIONE 02 ..3....+....4....+....5....+....6....+....7....+....8
270500030623MOTIVO DELLA VARIAZIONE 03 ..3....+....4....+....5....+....6....+....7....+....8
270600030623MOTIVO DELLA VARIAZIONE 04 ..3....+....4....+....5....+....6....+....7....+....8
270700030623MOTIVO DELLA VARIAZIONE 05 ..3....+....4....+....5....+....6....+....7....+....8
270800030623MOTIVO DELLA VARIAZIONE 06 ..3....+....4....+....5....+....6....+....7....+....8
270900030623MOTIVO DELLA VARIAZIONE 07 ..3....+....4....+....5....+....6....+....7....+....8
271000030623MOTIVO DELLA VARIAZIONE 08 ..3....+....4....+....5....+....6....+....7....+....8
271100030623MOTIVO DELLA VARIAZIONE 09 ..3....+....4....+....5....+....6....+....7....+....8
271200030623MOTIVO DELLA VARIAZIONE 10 ..3....+....4....+....5....+....6....+....7....+....8
271300030623MOTIVO DELLA VARIAZIONE 11 ..3....+....4....+....5....+....6....+....7....+....8
271400030623MOTIVO DELLA VARIAZIONE 12 ..3....+....4....+....5....+....6....+....7....+....8
271500030623MOTIVO DELLA VARIAZIONE 13 ..3....+....4....+....5....+....6....+....7....+....8
271600030623MOTIVO DELLA VARIAZIONE 14 ..3....+....4....+....5....+....6....+....7....+....8
271700030623MOTIVO DELLA VARIAZIONE 15 ..3....+....4....+....5....+....6....+....7....+....8
271800030623MOTIVO DELLA VARIAZIONE 16 ..3....+....4....+....5....+....6....+....7....+....8
271900030623MOTIVO DELLA VARIAZIONE 17 ..3....+....4....+....5....+....6....+....7....+....8
272000030623MOTIVO DELLA VARIAZIONE 18 ..3....+....4....+....5....+....6....+....7....+....8
272100030623MOTIVO DELLA VARIAZIONE 19 ..3....+....4....+....5....+....6....+....7....+....8
272200030623MOTIVO DELLA VARIAZIONE 20...3....+....4....+....5....+....6....+....7....+....8
272300030623PROMEMORIA PER AUTOTRASPORTATORE 01 ...4....+....5....+....6....+....7....+....8
272400030623PROMEMORIA PER AUTOTRASPORTATORE 02 ...4....+....5....+....6....+....7....+....8
272500030915BANCALI DA RIPORTARE A MAGAZZINO 01 ...4....+....5....+....6....+....7....+....8
