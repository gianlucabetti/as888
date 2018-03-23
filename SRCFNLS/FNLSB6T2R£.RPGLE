000100030515      *---------------------------------------------------------------*
000110060524      *?  STAMPA   LETTERE   DI   VETTURA   SU   STAMPANTE   LASER   ?*
000120040607      *---------------------------------------------------------------*
000130090304      *                                                               *
000140110310      *? A QUESTO PROGRAMMA È "LEGATA"? LA STORED-PROCEDURE FISP30R:?*
000150110310      *? VERIFICARE SE LA MODIFICA E' DA APPORTARE ANCHE AD ESSA !!! ?*
000160030515      *                                                               *
000170060524      *NOTA BENE:?> Codice cliente 0501331 => FedEx                  *
000180081127      *              codice fissato nella costante "c_CliFedEx"       *
000190150504      *            > Codice cliente 0506514 => FedEx (2)              *
000200150504      *              codice fissato nella costante "c_CliFedEx2"      *
000210070123      *            > Codice cliente 0201515 => PraOil oleodotti       *
000220081127      *              codice fissato nella costante "c_CliPraOil"      *
000230091116      *            > Codice cliente 0504450 => TTL                    *
000240091116      *              codice fissato nella costante "c_CliTTL"         *
000250090527      *            > Codice cliente 0632910 => Consorzio Lotterie Naz *
000260090527      *              codice fissato nella costante "c_CliCLN"         *
000270140513      *            > Cod. cliente 0017093 => Lotterie Nazionali       *
000280140513      *                   (gemello di backup su Bologna)              *
000290140513      *              cod. fissato nella costante "c_CliLN_2"          *
000300090611      *            > Codice cliente 0634923 => S G Soluzioni Logist.  *
000310090611      *              codice fissato nella costante "c_CliSG "         *
000320091117      *            > Codice cliente 0635042 => S G Soluzioni Logist.  *
000330091117      *              codice fissato nella costante "c_CliSG_2"        *
000340030515      *                                                               *
000350030515      *---------------------------------------------------------------*
000360110427
000370110427       //--------------------------------------------------------------
000380110427       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000390110427       //--------------------------------------------------------------
000400110427
000410110427     /*CMD addlible lib(GAITRAGRPS) position(*last)
000420110427     /*PRM dbgview(*source)
000430110427     /*END rmvlible lib(GAITRAGRPS)
000440110427     /*END
000450110427
000460110427       //--------------------------------------------------------------
000470110427       //?Specifiche di controllo.                                     ?
000480110427       //--------------------------------------------------------------
000490080703
000500080703     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000510080703     h dftactgrp(*no) actgrp(*caller)
000520080703
000530110427       //--------------------------------------------------------------
000540110427       //?Dichiarazione file.                                          ?
000550110427       //--------------------------------------------------------------
000560110427
000570110411       // -?Dati comuni?
000580030515     fTABEL00F  if   e           k disk
000590090302     fTNTBE01L  if   e           k disk
000600070305     fAZORG01L  if   e           k disk
000610041213     fFLNUF01L  Uf   e           k disk    usropn
000620110411       // -?Dati per LdV partenza?
000630030515     fFNBLP01L  if   e           k disk    usropn
000640030515     fFNBLT01L  if   e           k disk    usropn
000650110411       // -?Dati per LdV arrivo?
000660070305     fFNARB01L  if   e           k disk    usropn
000670070306     fFNART01L  if   e           k disk    usropn
000680070319     fFNEVB05L  if   e           k disk    usropn
000690070419     fFNEVB22L  if   e           k disk    usropn
000700070319     fFIARG01L  if   e           k disk    usropn
000710070319     fFIARN11L  if   e           k disk    usropn
000720110411       // -?Altri dati comuni?
000730090528     fFNLBL01L  if   e           k disk    usropn
000740070319     fFIARS01L  if   e           k disk    usropn
000750070305     fFIAR401L  if   e           k disk    usropn
000760030515     fFIAR501L  if   e           k disk    usropn
000770040531     fFIAR531C  if   e           k disk    usropn
000780040531     f                                     rename(FIAR5000:FIAR5031)
000790070305     fFIAR601L  if   e           k disk    usropn
000800070305     fFIAR701L  if   e           k disk    usropn
000810040607     fFIAR801L  if   e           k disk    usropn
000820070305     fFIAR901L  if   e           k disk    usropn
000830170605     fTFNTC01L  if   e           k disk    usropn
000840110411       // -?O.R.M.?
000850030903     fFNORM01L  if   e           k disk    usropn
000860140411     fFNORE01L  if   e           k disk    usropn
000870130306
000880130306       // -?Elenco PDF da stampare?
000890130306     fPDFLIST   o    e             disk    usropn
000900130306
000910110411       // -?Printer File?
000920130710     fFNLSB6P   o    e             printer usropn infds(prtINFOa4)
000930080604     f                                     rename(LSB6POS :A4prtPOS )
000940130418     f                                     rename(LSB6BOX :A4prtBOX )
000950120530     f*//                                  rename(LSB6LOGBAR:A4LogoBAR)
000960120530     f*//                                  rename(LSB6LOGDA1:A4LogoDA1)
000970120530     f*//                                  rename(LSB6LOGBR2:A4LogoBR2)
000980120530     f*//                                  rename(LSB6LOGDA2:A4LogoDA2)
000990120530     f                                     rename(LSB6LOGBRT:A4LogoBRT)
001000120530     f                                     rename(LSB6LOGDA3:A4LogoDA3)
001010130418     f                                     rename(LSB6LDV :A4prtLDV )
001020080604     f                                     rename(LSB6LDVB:A4prtLDVB)
001030080922     f                                     rename(LSB6DPD :A4prtDPD )
001040080604     f                                     rename(LSB6FED1:A4prtFED1)
001050080604     f                                     rename(LSB6FED2:A4prtFED2)
001060080604     f                                     rename(LSB6FED3:A4prtFED3)
001070080604     f                                     rename(LSB6AR51:A4prtAR51)
001080080604     f                                     rename(LSB6AR52:A4prtAR52)
001090080604     f                                     rename(LSB6AR53:A4prtAR53)
001100080604     f                                     rename(LSB6AR54:A4prtAR54)
001110080604     f                                     rename(LSB6AR5A:A4prtAR5A)
001120150304     f*//                                  rename(LSB6AR5B:A4prtAR5B)
001130150304     f*//                                  rename(LSB6AR5C:A4prtAR5C)
001140080604     f                                     rename(LSB6AR5G:A4prtAR5G)
001150080604     f                                     rename(LSB6DB0 :A4prtDB0)
001160080604     f                                     rename(LSB6DT0 :A4prtDT0)
001170080604     f                                     rename(LSB6DT1 :A4prtDT1)
001180080604     f                                     rename(LSB6DT2 :A4prtDT2)
001190080604     f                                     rename(LSB6DC1 :A4prtDC1)
001200080604     f                                     rename(LSB6DK0 :A4prtDK0)
001210080604     f                                     rename(LSB6DK1 :A4prtDK1)
001220080604     f                                     rename(LSB6DK2 :A4prtDK2)
001230080604     f                                     rename(LSB6DK3 :A4prtDK3)
001240080604     f                                     rename(LSB6DK4 :A4prtDK4)
001250080604     f                                     rename(LSB6DK5 :A4prtDK5)
001260081222     f                                     rename(LSB6DLL :A4prtDLL)
001270080604     f                                     rename(LSB6DL0 :A4prtDL0)
001280080604     f                                     rename(LSB6DL0A:A4prtDL0A)
001290080604     f                                     rename(LSB6DL0B:A4prtDL0B)
001300080604     f                                     rename(LSB6DL0C:A4prtDL0C)
001310080604     f                                     rename(LSB6DL0D:A4prtDL0D)
001320080604     f                                     rename(LSB6DL0E:A4prtDL0E)
001330080604     f                                     rename(LSB6DL0F:A4prtDL0F)
001340080604     f                                     rename(LSB6DL1A:A4prtDL1A)
001350080604     f                                     rename(LSB6DL1B:A4prtDL1B)
001360080604     f                                     rename(LSB6DL1C:A4prtDL1C)
001370080604     f                                     rename(LSB6DL1D:A4prtDL1D)
001380080604     f                                     rename(LSB6DL1E:A4prtDL1E)
001390080604     f                                     rename(LSB6DL2 :A4prtDL2)
001400080604     f                                     rename(LSB6DL21:A4prtDL21)
001410080604     f                                     rename(LSB6DL3 :A4prtDL3)
001420090323     f                                     rename(LSB6DL31:A4prtDL31)
001430080604     f                                     rename(LSB6DL4 :A4prtDL4)
001440080604     f                                     rename(LSB6DL5 :A4prtDL5)
001450080604     f                                     rename(LSB6DL6 :A4prtDL6)
001460080604     f                                     rename(LSB6DL7 :A4prtDL7)
001470080604     f                                     rename(LSB6DL8 :A4prtDL8)
001480080604     f                                     rename(LSB6DL9 :A4prtDL9)
001490080604     f                                     rename(LSB6AC0 :A4prtAC0)
001500080604     f                                     rename(LSB6AC0A:A4prtAC0A)
001510080604     f                                     rename(LSB6AC0B:A4prtAC0B)
001520080604     f                                     rename(LSB6AC1A:A4prtAC1A)
001530080604     f                                     rename(LSB6AC1B:A4prtAC1B)
001540080604     f                                     rename(LSB6AC2 :A4prtAC2)
001550080604     f                                     rename(LSB6AC3 :A4prtAC3)
001560080604     f                                     rename(LSB6AC4 :A4prtAC4)
001570080604     f                                     rename(LSB6ES0 :A4prtES0)
001580080604     f                                     rename(LSB6ES0A:A4prtES0A)
001590080604     f                                     rename(LSB6ES0B:A4prtES0B)
001600080604     f                                     rename(LSB6ES1A:A4prtES1A)
001610080604     f                                     rename(LSB6ES1B:A4prtES1B)
001620080604     f                                     rename(LSB6ES2 :A4prtES2)
001630080604     f                                     rename(LSB6ES3 :A4prtES3)
001640080604     f                                     rename(LSB6ES4 :A4prtES4)
001650080604     f                                     rename(LSB6SP0 :A4prtSP0)
001660080604     f                                     rename(LSB6SP1 :A4prtSP1)
001670080604     f                                     rename(LSB6NUL :A4prtNUL)
001680080604     f                                     rename(LSB6GEN :A4prtGEN)
001690080604     f                                     rename(LSB6LO  :A4prtLO)
001700080604     f                                     rename(LSB6SEG :A4prtSEG)
001710080604     f                                     rename(LSB6MSG :A4prtMSG)
001720030515
001730110427       //--------------------------------------------------------------
001740110427       //?Definizione costanti.                                        ?
001750110427       //--------------------------------------------------------------
001760110427
001770110427       // -?Intestazioni in stampa?
001780120530     d*// $Bartolini      c                   const('BARTOLINI S.p.A.')
001790110426     d $BRT            c                   const('BRT S.p.A.')
001800090414     d*// $CorrEspr       c                   const('CORRIERE  ESPRESSO')
001810060503     d $Intest1        c                   const('Sede Operativa ed Am-
001820060503     d                                     ministrativa:')
001830060503     d $Intest2        c                   const('via E. Mattei, 42 -
001840110426     d                                     - 40138 Bologna')
001850110426     d*//                                  - 40138 BOLOGNA')
001860060503     d $Intest3        c                   const('Sede Legale: Piazza -
001870060503     d                                     Diaz, 7 - 20123 Milano')
001880070215     d $Intest4        c                   const('C.F. e P.IVA 04507990150')
001890061206     d $Intest5        c                   const(' Capitale Sociale Eu-
001900061221     d                                     ro 24.000.000 int. vers.')
001910060503     d $Intest6        c                   const('info on-line')
001920120530     d*// $Intest7        c                   const('www.bartolini.it')
001930120530     d $Intest7_brt    c                   const('www.brt.it')
001940090107     d $Intest2_b      c                   const('via Enrico Mattei, +
001950090107     d                                            42 - 40138 BOLOGNA')
001960120530     d*// $Intest3_b      c                   const('Tel. 051 6015411 - +
001970120530     d*//                                            Fax 051 530365 - +
001980120530     d*//                                            www.bartolini.it')
001990120530     d $Intest3_b_brt  c                   const('Tel. 051 6015411 - +
002000120530     d                                            Fax 051 530365 - +
002010120530     d                                            www.brt.it')
002020081222     d $Intest4_b      c                   const('C.F. - P.IVA - +
002030081222     d                                            Reg. Imprese Milano +
002040081222     d                                            n. 0457990150')
002050081222     d $Intest5_b      c                   const('Capitale Sociale € +
002060081222     d                                            24.000.000,00 i.v. +
002070081222     d                                            - REA Milano n. +
002080081222     d                                            1734257')
002090110427
002100110427       // -?DIMENSIONI & POSIZIONI:?
002110110427
002120110427       // -?Lunghezza (in inches) di una singola LdV?
002130080924     d $LenLdV2        c                   const(5.775)
002140080924     d $LenLdV4        c                   const(2.890)
002150110427       // -?Lunghezza di 1/4 di A4?
002160080924     d c_Len1_inch     c                   const(2.925)
002170080923     d c_Len2_inch     c                   const(5.850)
002180080923     d c_Len2_row8     c                   const(047)
002190110427       // -?Spessore delle linee in inches/pollici (*memo)?
002200080701     d c_LineNarrow    c                   const(0.008)
002210080701     d c_LineMedium    c                   const(0.017)
002220080701     d c_LineWide      c                   const(0.025)
002230110427       // -?Dimensioni base per i simil-loghi Bartolini e BRT?
002240120530      *///copy gaitrasrc/srcTNSY,LogoBAR_R0
002250120530      *///copy gaitrasrc/srcTNSY,LogoBRT3R0
002260120530      *///copy gaitrasrc/srcTNSY,LogoBRT_R0
002270120530      /copy gaitrasrc/srcTNSY,LogoBRT_R1
002280110427
002290110427       // -?Posizionamento all'inizio della pag. A4 successiva?
002300110427     d c_§PS1          c                   const(0.950)
002310110427
002320120530       // -?Posizioni dell'intestazione "BRT S.p.A." nella 1ª copia?
002330080704     d c_PosTBar       c                   const(0.270)
002340080704     d c_PosTBa1       c                   const(0.350)
002350110426     d c_PosTBa2       c                   const(0.420)
002360110426     d c_PosTBa3       c                   const(0.490)
002370110426     d c_PosTBa4       c                   const(0.560)
002380110426     d c_PosTBa5       c                   const(0.630)
002390110426     d c_PosTBa6       c                   const(0.710)
002400110426     d c_PosTBa2x      c                   const(0.430)
002410110426     d c_PosTBa3x      c                   const(0.500)
002420110426     d c_PosTBa4x      c                   const(0.590)
002430110426     d c_PosTBa5x      c                   const(0.660)
002440110426     d c_PosTBa6x      c                   const(0.750)
002450110427       // -?Posizioni dei bordi orizzontali superiori box nella 1ª copia?
002460110427       //  ?(distanza dal lato superiore dell'A4)?
002470110427       //  ?N.B.?- c_PosBoxNXs => posizione iniziale della linea?
002480110427       //                        ?orizzontale Superiore?
002490110427       //       ?- c_PosBoxNXi => posizione iniziale della linea?
002500110427       //                        ?orizzontale Inferiore?
002510080704     d c_PosBox1As     c                   const(0.200)
002520080704     d c_PosBox1Bs     c                   const(0.790)
002530080704     d c_PosBox1Bi     c                   const(2.850)
002540080704     d c_PosBox1Cs     c                   const(1.950)
002550100208     d c_PosBox1Ds     c                   const(2.453)
002560110427       // -?Posizioni di altre linee orizzontali nella 1ª copia?
002570080704     d c_PosLin1OS1    c                   const(1.350)
002580080704     d c_PosLin1OS2    c                   const(1.790)
002590080704     d c_PosLin1OS3    c                   const(2.010)
002600080704     d c_PosLin1OS4    c                   const(2.230)
002610091119     d c_PosLin1OS5    c                   const(2.470)
002620080704     d c_PosLin1OC1    c                   const(1.680)
002630080704     d c_PosLin1OC2    c                   const(1.240)
002640140917     d c_PosLin1OC3    c                   const(1.470)
002650080704     d c_PosLin1OD1    c                   const(1.010)
002660140917
002670140917       // -?Posizioni di altre costanti?
002680140917     d*// c_PosKBRTc      c                   const(0.970)     x FONT(00404)
002690140923     d c_PosKBRTc      c                   const(0.965)
002700140923     d*// c_PosKBRTc      c                   const(0.955)     x FONT(02305)
002710080704     d c_PosKTass      c                   const(2.130)
002720080704     d c_PosKRCV       c                   const(1.100)
002730080704     d c_PosKBAV       c                   const(1.850)
002740080709     d c_PosKCXD2      c                   const(3.100)
002750110427
002760110427       // -?Posizione minima superiore per FedEx?
002770030709     d $MinLine1       c                   const(012)
002780110427       // -?Limite righe stampabili su 1/4 di A4 in 1/9"?
002790081002     d $MaxLine1       c                   const(025)
002800110427       // -?Limite righe stampabili su 1/2 A4 in 1/8" per?
002810110427       //  ?Karnak/Titanedi/EDk + Arcte + Esprinet +?
002820110427       //  ?Logistica/Sait + Generico (con/senza logo)?
002830080822     d $MaxLine2       c                   const(068)
002840090323     d $MaxLine2_pl1   c                   const(044)
002850090323     d $MaxLine2_pl2   c                   const(090)
002860110406     d $MaxLine2_pl1k  c                   const(045)
002870110406     d $MaxLine2_pl2k  c                   const(091)
002880110427       // -?Limite righe stampabili su 1/2 A4 in 1/8" per?
002890110427       //  ?Karnak/Titanedi/EDk?
002900080822     d $MaxLine3       c                   const(059)
002910090324     d $MaxLine3_pl1   c                   const(036)
002920090324     d $MaxLine3_pl2   c                   const(082)
002930110427       // -?Limite righe stampabili su 1/2 A4 in 1/8" per?
002940110427       //  ?Logistica/Sait + Arcte?
002950090323     d $MaxLine4       c                   const(065)
002960090324     d $MaxLine4_pl1   c                   const(041)
002970090324     d $MaxLine4_pl2   c                   const(087)
002980110427       // -?Limite righe stampabili su 1/2 A4 in 1/8" per?
002990110427       //  ?Arcte + Esprinet?
003000090324     d $MaxLine4_pl3   c                   const(042)
003010090324     d $MaxLine4_pl4   c                   const(088)
003020090324     d $MaxLine5_pl1   c                   const(045)
003030090324     d $MaxLine5_pl2   c                   const(091)
003040110427       // -?Limite righe stampabili su 1/2 A4 in 1/8" per?
003050110427       //  ?Esprinet?
003060081002     d $MaxLine6       c                   const(057)
003070081002     d $MaxLine6_pl1   c                   const(034)
003080081002     d $MaxLine6_pl2   c                   const(080)
003090110427
003100110427       // -?Limiti di OverFlow?
003110040608     d $BoxRow1        c                   const(010)
003120080707     d $LimOF          c                   const(08.250)
003130080709     d $LimOF1         c                   const(02.900)
003140080709     d $LimOF2         c                   const(05.800)
003150080709     d $LimOF3         c                   const(08.700)
003160030515     d $elSNSxA5       c                   const(04)
003170100208     d c_DateMax_A     c                   const('20391231')
003180110427
003190110427       // -?COSTANTI IN STAMPA?
003200110426     d c_CopiaXdest    c                   const('COPIA PER IL DESTINA-
003210080919     d                                     TARIO')
003220110426     d c_ScNS          c                   const('ELENCO SEGNACOLLI -
003230030515     d                                     NON SEQUENZIALI')
003240110426     d c_CoOr          c                   const('COLLI ORIGINALI     -
003250030717     d                                     N. ')
003260110426     d c_DeSM          c                   const('DESTINATARIO SUPERME-
003270031124     d                                     RCATO')
003280110426     d c_PMxA          c                   const('PROMEMORIA PER L''-
003290080604     d                                     AUTOTRASPORTATORE')
003300110427       // -?ALTRE COSTANTI:?
003310110427       // ·?Codice tab.7V-R x "Destinatario assente: plico in buca"?
003320110502     d*// Ktab7V_CB       c                   const('CB')
003330110427       // ·?Codice tab.7V-R x "Restituzione DDT firmato al mittente"?
003340110502     d*// Ktab7V_C1       c                   const('C1')
003350110427       // ·?Codice tab.7V-R x "Restituzione copie del contratto firmato"?
003360110502     d*// Ktab7V_C2       c                   const('C2')
003370110427       // ·?Codice tab.7V-R x "Restituzione copie del contratto firmato"?
003380110427       //  ?                e "Restituzione copia della C.I. del dest."?
003390110502     d*// Ktab7V_C3       c                   const('C3')
003400110427       // -?Codice tab.7V-R x "Restituzione copia C.I. del destinatario"?
003410110411     d*// Ktab7V_C4       c                   const('C4')
003420110427       // -?Codice tab.7V-R x "Restituzione modulo Vodafone firmato"?
003430110411     d*// Ktab7V_C5       c                   const('C5')
003440110427       // -?Codice tab.7V-R x "Non effettuare ritiro di merce"?
003450110502     d*// Ktab7V_NR       c                   const('NR')
003460110427       // -?Codice tab.7V-R x "Ritiro contestuale alla consegna"?
003470110502     d*// Ktab7V_RC       c                   const('RC')
003480110427       // -?Codice tab.7V-R x C1+RC?
003490110502     d*// Ktab7V_R1       c                   const('R1')
003500110427
003510130418
003520110427       // -?Codice cliente "FedEx"?
003530081127     d c_CliFedEx      c                   const(0501331)
003540150504     d c_CliFedEx2     c                   const(0506514)
003550110427       // -?Codice cliente "PraOil oleodotti italiani"?
003560081127     d c_CliPraOil     c                   const(0201515)
003570110427       // -?Codice cliente "TTL"?
003580091116     d c_CliTTL        c                   const(0504450)
003590110427       // -?Codice cliente "Consorzio Lotterie Nazionali - Gratta & Vinci"?
003600090527     d c_CliCLN        c                   const(0632910)
003610140513     d c_CliLN_2       c                   const(0017093)
003620110427       // -?Codice cliente "S G Soluzioni Logistiche"?
003630090611     d c_CliSG         c                   const(0634923)
003640091117     d c_CliSG_2       c                   const(0635042)
003650130418
003660091117
003670110427       // -?Costante per evitare la stampa del contratto x "S.G."?
003680091117     d c_NoContr       c                   const('NO CONTRATTO')
003690130306
003700130306       // -?Costante per identificare la presenza di PDF del cliente?
003710130308       //  ?in FIAR800F.AR8TMD (nel suo 1° byte)?
003720130308     d c_§PDF          c                   const('§')
003730110427
003740110427       // -?Costante per controllo "caratteri solo numerici"?
003750110426     d c_Digits        c                   const('1234567890')
003760140307
003770140307       // -?Costanti per conversione caratteri maiuscoli/minuscoli?
003780140307     d c_Mai           c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
003790140307     d c_Min           c                   const('abcdefghijklmnopqrstuvwxyz')
003800110427
003810110427       // -?Comando di override al PrtF?
003820100208     d c_CmdDltOvr     c                   const('DLTOVR +
003830130710     d                                            file(FNLSB6P) +
003840080708     d                                            lvl(*job)')
003850110427
003860110427       //--------------------------------------------------------------
003870110427       //?Definizione schiere.                                         ?
003880110427       //--------------------------------------------------------------
003890110427
003900110427       // -?Messaggi da stampare?
003910130312     d $Msg            s            100    dim(7)   ctdata  perrcd(1)
003920110427       // -?Giorni della settimana e relativa decodifica?
003930070306     d $GgS            s              1    dim(9)   ctdata  perrcd(1)
003940070306     d $Gio            s             15    dim(9)   alt($GgS)
003950110427       // -?Varie esenti IVA?
003960070319     d $Var            s              1    dim(10)
003970110427       // -?Note e Particolarità?
003980070306     d $NP             s                   dim(6)   inz  like(Pnp1)
003990110427       // -?Segnacolli NON sequenziali (memo-1- e stampa-2-)?
004000070306     d $SC1            s              7  0 dim(14)  inz
004010070306     d $SC2            s              8    dim(14)  inz
004020110427       // -?Tab. "2A" (Causali Eventi): causali con tipo consegna = "G"?
004030070308     d $2Acev_G        s                   dim(999) inz like(EVBcev)
004040110427       // -?Quantità (editate) - a sx - e Unità di misura - a dx -?
004050070306     d $QUM            s             20    dim(3)   inz
004060110427       // -?Quantità (editate) - a sx - e Unità di misura - a dx -?
004070070306     d $AR8o1          s                   dim(10)  inz  like(§AR8o1a)
004080110427       // -?Consegne Parziali Precedenti?
004090070319     d $CP             s                   dim(20)  inz(*all'9')
004100070319     d                                                   like(Elem_CP)
004110070319     d $CPdcm          s                   dim(20)  inz  like(ARTdcm)
004120110427
004130110427       //--------------------------------------------------------------
004140110427       //?Definizione strutture dati.                                  ?
004150110427       //--------------------------------------------------------------
004160110427
004170110427       // -?Parametri ricevuti?
004180030515     d KPJBA         e ds
004190110427       // ·?Dati LDV da stampare?
004200080703     d FNLSB5ds      e ds
004210130306     d FNLSB6ds1     e ds
004220110427       // -?Flag operativi DB0FLO di FNLSB5DS?
004230080606     d dDB0flo       e ds                  inz
004240110427
004250110427       // -?Parametri x Controllo profilo utenti?
004260040223     d TIBS34ds      e ds
004270110427       // -?Ds di riferimento al file esterno AZUTE00F?
004280040223     d AZUTEds       e ds                  extname(AZUTE00F)
004290110427       // -?Ds per dati organigramma?
004300040223     d dDATIUTE      e ds
004310110427
004320110427       // -?Conversioni in Euro?
004330061107     d YEURCOds      e ds                  inz
004340110427
004350110427       // -?Dati da AZORG00F?
004360070424     d OG143ccm      e ds                  inz     extname(OG143)
004370070424     d                                             prefix(§CM:3)
004380030515     d OG143         e ds                  inz
004390070424     d OG146arr      e ds                  inz     extname(OG146)
004400070424     d                                             prefix(A)
004410070424     d OG146cf£6     e ds                  inz     extname(OG146)
004420070424     d                                             prefix(£6:1)
004430030515     d OG148         e ds                  inz
004440070424     d OG148arr      e ds                  inz     extname(OG148)
004450070424     d                                             prefix(A)
004460070424     d OG148cf£6     e ds                  inz     extname(OG148)
004470070424     d                                             prefix(£6:1)
004480070424     d OG150arr      e ds                  inz     extname(OG150)
004490070424     d                                             prefix(A)
004500110427
004510110427       // -?Dati da FIAR400F?
004520061108     d dsBL4A        e ds                  inz
004530061108     d dsBL4I        e ds                  inz
004540071015     d dsBL4M        e ds                  inz
004550061108     d dTA4Q         e ds                  inz
004560110427
004570110427       // -?Flag operativi di FNORM - campo ORMFLO?
004580110427     d dORM01        e ds                  inz
004590140411
004600140411       // -?Dati per trc "O " del file FNORE (orari apertura)?
004610140411     d dOREorari     e ds
004620110427
004630110427       // -?Tabelle?
004640061107     d ds$2          e ds
004650061107     d ds1A          e ds                  inz
004660061107     d ds1P          e ds                  inz
004670070306     d ds2A          e ds                  inz
004680061107     d ds3A          e ds                  inz
004690061107     d ds5E          e ds                  inz
004700061107     d ds7R          e ds                  inz
004710061107     d dsCC          e ds
004720061107     d dsCV          e ds                  inz
004730061107     d dsEI          e ds                  inz
004740061107     d dsTB          e ds                  inz
004750030805     d dGEI          e ds
004760030805     d dGED          e ds
004770150304     d*// dTBA          e ds                  inz
004780071112     d dVPO          e ds                  inz
004790120530     d*// dVPObrt       e ds                  inz
004800061031     d dVPOdeco      e ds                  inz
004810031124     d dAR5gen       e ds                  inz
004820150304     d*// dAR5ban       e ds                  inz
004830061030     d dAR5bnb       e ds                  inz
004840030903     d dAR5rcc       e ds                  inz
004850040223     d dAR5scr       e ds                  inz     prefix(§AR5S:4)
004860041207     d dAR8          e ds                  inz
004870041207     d  §AR8tsd      e                     inz('S1')
004880040607     d dAR8T1        e ds                  inz
004890040607     d dAR8T2        e ds                  inz
004900040607     d dAR8C1        e ds                  inz
004910040607     d dAR8C2        e ds                  inz
004920040607     d dAR8C3        e ds                  inz
004930040607     d dAR8V1        e ds                  inz
004940040607     d dAR8V2        e ds                  inz
004950040607     d dAR8L1        e ds                  inz
004960040607     d dAR8O1        e ds                  inz
004970040607     d dAR8O2        e ds                  inz
004980041026     d dAR8LOG1T1    e ds                  inz
004990041026     d dAR8LOG1T2    e ds                  inz
005000041026     d dAR8LOG1T3    e ds                  inz
005010041026     d dAR8LOG1C1    e ds                  inz
005020041026     d dAR8LOG1C2    e ds                  inz
005030041026     d dAR8LOG1C3    e ds                  inz
005040060614     d dAR8LOG1C4    e ds                  inz
005050041026     d dAR8LOG1P1    e ds                  inz
005060050318     d dAR8LOG1Z1    e ds                  inz
005070050606     d dAR8LOG1S1    e ds                  inz
005080050606     d dAR8LOG1S2    e ds                  inz
005090050606     d dAR8LOG1S3    e ds                  inz
005100050606     d dAR8LOG1S4    e ds                  inz
005110050606     d dAR8LOG1S5    e ds                  inz
005120050113     d dAR8ARCTT1    e ds                  inz
005130050114     d dAR8ARCTT2    e ds                  inz
005140050113     d dAR8ARCTC1    e ds                  inz
005150050113     d dAR8ARCTC2    e ds                  inz
005160050113     d dAR8ARCTP1    e ds                  inz
005170050214     d dAR8ESP1T1    e ds                  inz
005180050214     d dAR8ESP1T2    e ds                  inz
005190050214     d dAR8ESP1C1    e ds                  inz
005200050214     d dAR8ESP1C2    e ds                  inz
005210050214     d dAR8ESP1P1    e ds                  inz
005220050214     d dAR8ESP1P2    e ds                  inz
005230130306     d dAR8§pdf      e ds                  inz
005240170605     d dCLI          e ds                  inz
005250110427     d TIBS02ds      e ds                  inz
005260110427     d   T02mod      e                     inz('C')
005270110427
005280110427       // -?Parametri per TRULINTR (determinaz. intestatario c/assegno)?
005290100208     d TRULINTds     e ds                  inz
005300100208     d   iINTtip     e                     inz('I')
005310110427       // -?Parametri per TRUL21R (calcolo limiti importo c/assegno)?
005320030515     d TRUL21ds      e ds                  inz
005330030515     d  I21tla       e                     inz('L')
005340110427       // -?Parametri per TRUL28R (calcolo check digit del barcode)?
005350030515     d TRUL28ds      e ds                  inz
005360030515     d  I28tla       e                     inz('L')
005370041019     d  I28mod       e                     inz('BAR')
005380110427       // -?Parametri per FNLV55R (reperimento terminal arr/par/£6)?
005390070212     d FNLV55ds      e ds                  inz
005400070212     d  D55tpt       e                     inz('6')
005410110427       // -?Parametri per FNLV80R (decodifica consegna richiesta)?
005420051129     d FNLV80ds      e ds                  inz
005430051129     d  ILV80tla     e                     inz('C')
005440110427       // -?Parametri per FIDN12R (ricerca C.A.')?
005450070319     d FIDN12ds      e ds                  inz
005460070319     d  I12fel       e                     inz('E')
005470070319     d  I12fan       e                     inz('E')
005480070319     d  I12fch       e                     inz('E')
005490110427       // -?Parametri per TNSD99R (calcolo tempi di delivery)?
005500090605     d TNSD99ds      e ds                  inz
005510090605     d  D98tla       e                     inz('C')
005520110427
005530110427       // -?Status ds?
005540030515     d Status         sds
005550130306     d   SDSpgm          *proc
005560130306     d*//SDSprm          *parms
005570130306     d   SDSusr              254    263
005580110427
005590110427       // -?InfDS?
005600030515     d PrtINFOa4       ds
005610080627     d  CurLineA4            367    368i 0
005620110427
005630110427       // -?Codice a Barre della LDV?
005640030515     d BARcodeDS       ds            17    inz
005650030515     d  BARcdeAAS                     2s 0 inz
005660030515     d  BARcdeLNP                     3s 0 inz
005670030515     d  BARcdeNRS                     2s 0 inz
005680030515     d  BARcdeNSP                     7s 0 inz
005690110427       // -?Codice a Barre dell'ORM?
005700040629     d BARcodeORM      ds            14    inz
005710040629     d  ORMpoe                             inz
005720040629     d  ORMnsr                             inz
005730040629     d  ORMnor                             inz
005740040629     d  ORMnrv                             inz
005750110427
005760110427       // -?Chiave di accesso ad eventuali Note?
005770030515     d KEYpar          ds             8    inz
005780030515     d  WTPpar                 1      1    inz
005790030515     d  WCDpar                 2      3    inz
005800030515     d  Wprogr                 4      8    inz('S0001')
005810110427
005820110427       // -?Dati delle eventuali consegne parziali precedenti?
005830140307     d Elem_CP         ds                  inz
005840140307     d   WCPdcm                       8s 0 inz
005850140307     d   WCPocm                       4s 0 inz
005860140307     d   WCPncl                       5s 0 inz
005870140307     d   WCPfir                            inz  like(EVB2dati)
005880130306
005890130306       // -?%TimeStamp?
005900130306     d wTime_ds        ds                  qualified     inz
005910130306     d   wDate_Iso                     d   datfmt(*iso)  inz
005920130306     d   wFiller                      1a                 inz
005930130307     d   wTime                         t   timfmt(*iso)  inz
005940110427
005950110427       //--------------------------------------------------------------
005960110427       //?Definizione variabili globali.                               ?
005970110427       //--------------------------------------------------------------
005980110427
005990110427       // -?Flags booleani?
006000110427     d $Mamma          s               n   inz
006010130215     d $Mamma_FedEx    s               n   inz
006020110427     d $FirstSCR       s               n   inz(*on)
006030110427     d $FIAR5          s               n   inz
006040110427     d $FIAR8          s               n   inz
006050110427     d $FIARN          s               n   inz
006060110427     d $E_AR8          s               n   inz(*on)
006070130306     d $§PDF           s               n   inz
006080110427     d $DDT_o          s               n   inz
006090110427     d $DDT_p          s               n   inz
006100110427     d $DDTlastPg      s               n   inz
006110110427     d $Partic         s               n   inz
006120110427     d $NoDesPartic    s               n   inz
006130110427     d $BolloVirt      s               n   inz
006140110427     d $No_CF          s               n   inz
006150110427     d $No_PI          s               n   inz
006160120113     d $No_x_Cons      s               n   inz
006170110427     d $Rit_a_Inc      s               n   inz
006180110427     d $A4_x_ScNS      s               n   inz
006190110427     d $LascAvv        s               n   inz
006200110427     d $ConsParz       s               n   inz
006210110427     d $C_A_           s               n   inz
006220110427     d $CAeur          s               n   inz
006230110427     d $GeoAtt         s               n   inz
006240090302     d $TabPXA         s               n   inz
006250110411     d $TabPXA2        s               n   inz
006260090924     d $BoxNxD         s               n   inz
006270100513     d $PreavvORM      s               n   inz
006280130215       // -?Altri flags?
006290130215     d $First          s              1    inz(*on)
006300110427
006310110427       // -?Indici di schiera?
006320110427     d XX              s              3  0 inz
006330110427     d YY              s              3  0 inz
006340110427     d WX              s              3  0 inz
006350110427
006360110427       // -?Campi di comodo alfanumerici?
006370040223     d SAVin7x         s              4    inz(*zeros)
006380030717     d w_CoOr          s             35    inz
006390030515     d wAWB            s             12    inz
006400030515     d w001a           s              1    inz
006410030515     d w008a           s              8    inz
006420030515     d w012a           s             12    inz
006430030515     d w015a           s             15    inz
006440100513     d w053a           s             53    inz
006450070424     d wDateA          s              8    inz
006460070717     d wGeoTel         s              1    inz
006470070717     d wGeoCon         s              1    inz
006480110427       // -?Campi di comodo numerici?
006490030515     d w0040           s              4  0 inz
006500030515     d w0080           s              8  0 inz
006510030624     d w0112           s             11  2 inz
006520030515     d w0140           s             14  0 inz
006530071105     d wDate           s              8  0 inz
006540030515     d wCOPIE          s              3  0 inz
006550140307     d wEVBdev         s                   like(EVBdev)   inz
006560140307     d wEVBhev         s                   like(EVBhev)   inz
006570110427
006580110427       // -?Campi utilizzati nella gestione dell'impaginazione A4?
006590030515     d wMinLine        s              3  0 inz
006600030515     d wMaxLine        s              3  0 inz
006610080604     d wPage           s              4  0 inz(0001)
006620030515     d WelSNS          s              2  0 inz
006630040608     d wOmaRowT        s              3  0 inz
006640040608     d wOmaRowD        s              3  0 inz
006650110427       // -?Variabili per il calcolo delle posizioni per box in A4?
006660080923     d wNumLdV         s              5  0 inz
006670080923     d wSeqLdV         s              1  0 inz
006680080923     d wPosIni         s              5  3 inz
006690110427
006700110427       // -?Lunghezza (in inches) di una singola LdV?
006710080703     d $Len1LdV        s              5  3 inz(5.775)
006720110427       // -?Numero delle LdV (box) per pagina "A4"?
006730110427       //  ?(dovendo stampare sia la copia per il destinatario quanto?
006740110427       //   ?quella per il vettore, ci stanno 2 LdV in 1 A4;?
006750110427       //   ?stampando solo le copie per il vettore ce ne stanno 4!)?
006760080703     d $LdVxP          s              1  0 inz(2)
006770110427
006780110427       // -?Campi utilizzati per impostazione dell'output in stampa?
006790140923     d Pdsp            s              8  0 inz
006800130418     d Tla             s             50    inz
006810130418      *
006820050624     d wBolloVirt      s              4  2 inz
006830030515     d Wcas            s             13  3 inz
006840030515     d Wiva            s             11  2 inz
006850071015     d Wpdc4A          s              4    inz
006860030515     d Data_EUR        s               d   datfmt(*eur)   inz(*loval)
006870101201     d Data_DMY        s               d   datfmt(*dmy)   inz(*loval)
006880170717       // -?Campi per il salvataggio dei dati anagrafici della filiale?
006890170717       //  ?capofile £6?
006900170717     d wDB0lnaCf£6     s                   like(DB0lna)
006910170717     d wDB0dlaCf£6     s                   like(DB0dla)
006920170717     d wDB0inlCf£6     s                   like(DB0inl)
006930170717     d wDB0calCf£6     s                   like(DB0cal)
006940170717     d wDB0lolCf£6     s                   like(DB0lol)
006950170717     d wDB0prlCf£6     s                   like(DB0prl)
006960170717     d wDB0telCf£6     s                   like(DB0tel)
006970170717     d wDB0faxCf£6     s                   like(DB0fax)
006980110427       // -?Campi utilizzati per il salvataggio momentaneo di dati?
006990040608     d SavLineA4       s                   like(CurLineA4)
007000040311     d SAVlnp          s                   like(DB0lnp)   inz(*loval)
007010040311     d SAVlna          s                   like(DB0lna)   inz(*loval)
007020030515     d SAVtrd          s                   like(AR5trd)   inz(*hival)
007030030515     d SAVdac          s                   like(AR5dac)   inz(*hival)
007040030515     d SAVorc          s                   like(AR5orc)   inz(*hival)
007050040607     d SAVprr          s                   like(AR8prr)   inz
007060041207     d SAVtrk          s                   like(AR8trk)   inz
007070070314     d wRSxCF          s                   like(DB0rsm)   inz
007080071114     d wB0cfi          s                   like(DB0cpi)   inz
007090071114     d wB0piv          s                   like(DB0cpi)   inz
007100031015     d wB0nrs          s                   like(ARBnrs)   inz
007110070514     d wB0ndc          s                   like(ARBndc)   inz
007120070514     d wB0ntc          s                   like(ARBntc)   inz
007130070514     d wB0dcp          s                   like(ARBdcp)   inz
007140081119     d wB0tmc          s                   like(ARBtmc)   inz
007150101125     d wB0dcm          s                   like(ARBdcm)   inz
007160101125     d wB0icc          s                   like(ARBicc)   inz
007170101125     d wB0ica          s                   like(ARBica)   inz
007180130124     d wB0fiv          s                   like(AR6fiv)   inz
007190060505     d Wnsc            s                   like(ARSnsc)   inz
007200060505     d Wtrc            s                   like(ARStrc)   inz
007210030515     d Wctr            s                   like(DB0ctr)   inz
007220031124     d Wdsp            s                   like(DB0dft)   inz
007230090527     d WddtCLN         s                   like(DB0drt)   inz
007240090527     d WdspCLN         s                   like(DB0dft)   inz
007250030515     d Wsvx            s                   like(AR6sv1)   inz
007260030515     d Wvax            s                   like(AR6va1)   inz
007270150304     d*// Wtba            s                   like(§AR5tba)  inz
007280080708
007290110427       // -?Comando da eseguire per QCMDEXC?
007300080708     d Qcmd            s            512    inz
007310080708
007320090302       //--------------------------------------------------------------
007330130306       //?Definizione prototipi procedure usate.                       ?
007340090302       //--------------------------------------------------------------
007350140917
007360140917       // -?Calcolo/Controllo BRTcode?
007370140917      /copy gaitrasrc/srcProtoPI,TRUL28R0
007380140917      /copy gaitrasrc/srcProtoPR,TRUL28R0
007390080708
007400100208       // -?Calcolo tempi di delivery?
007410090605      /copy gaitrasrc/srcProtoPR,TNSD99R
007420100208
007430100208       // -?Determinazione intestatario contrassegno?
007440100208      /copy gaitrasrc/srcProtoPR,TRULINTR
007450130306
007460130306       // -?Reperimento/Aggiornamento numeratori in GRU?
007470130306     d TRUL33ds      e ds                  inz
007480130306      /copy gaitrasrc/srcProtoPR,TRUL33R
007490090605
007500100208       // -?Override al PrtF?
007510080709     d fnlsB8r         pr                  extpgm('FNLSB8R')
007520080709     d  fnlsB5ds                           likeds(FNLSB5ds)
007530130306
007540130306       // -?Reperimento override apportate al PrtF?
007550130306     d wPrtF           s             10
007560130307     d trulPOVRds    e ds                  qualified
007570130306     d trulPOVRc       pr                  extpgm('TRULPOVRC')
007580130306     d   wPrtF                       10
007590130306     d   trulPOVRds                        likeds(trulPOVRds)
007600130306
007610130306       // -?Stampa tutte le immagini PDF abbinate alle LdV già stampate?
007620130312      /copy gaitrasrc/srcProtoPI,PDF00STAR
007630130312      /copy gaitrasrc/srcProtoPR,PDF00STAR
007640080708
007650100208       // -?Esecuzione comando di sistema?
007660080708     d qCmdExc         pr                  extpgm('QCMDEXC')
007670080708     d  Qcmd                        512    const  options(*varsize)
007680080708     d  Qlen                         15  5 const
007690090302
007700090302       //--------------------------------------------------------------
007710090302       //?Definizione key-list.                                        ?
007720090302       //--------------------------------------------------------------
007730090302
007740110427       // -?File TNTBE01L?
007750090302     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
007760090302     d                                     prefix(k_)   inz
007770130306
007780130306       // -?File FIAR801L?
007790130306     d k06fiar801    e ds                  extname(FIAR801L : *key)
007800130306     d                                     prefix(k_)   inz
007810140307
007820140307       // -?File FNEVB22L?
007830140307     d keyFNEVB22    e ds                  extname(FNEVB22L : *key)
007840140307     d                                     prefix(k_)   inz
007850140411
007860140411       // -?File FNORE01L?
007870140411     d keyFNORE01    e ds                  extname(FNORE01L : *key)
007880140411     d                                     prefix(k_)
007890140411     d                                     inz
007900170605
007910170605       // -?File TFNTC01L?
007920170605     d keyTFNTC01    e ds                  extname(TFNTC01L : *key)
007930170605     d                                     prefix(k_)
007940170605     d                                     inz
007950030515
007960030619      *===============================================================*
007970050217      *?RIEPILOGO INDICATORI                                         ?*
007980030515      *---------------------------------------------------------------*
007990050216      *?18   ?- Stampa DATA e ORA CONSEGNA RICHIESTA                  *
008000050216      *?19   ?- Stampa IMPONIBILE, IVA, TOTALE ed ESTREMI FATTURA     *
008010050216      *?20   ?- Stampa CONTRASSEGNO                                   *
008020050216      *?21   ?- Stampa TOTALE DA ESIGERE ALLA CONSEGNA / AL RITIRO    *
008030051129      *?22   ?- Stampa NOTE (se NO imponibile e fattura)              *
008040050216      *?31   ?- di comodo                                             *
008050130418      *?33off?- Stampa della COPIA PER IL CORRIERE                    *
008060130418      *?33on ?- Stampa della COPIA PER IL DESTINATARIO                *
008070130418      *         (o per il Mittente, se P.F. Prepagato)                *
008080050216      *?35   ?- Richiesta FIRMA, DATA e ORA   in 1ª copia             *
008090050217      *         per ricevuta della merce senza riserve                *
008100050217      *         e indica "TOTALE DA ESIGERE ALLA CONSEGNA"            *
008110050216      *?36   ?- Richiesta FIRMA per QUIETANZA in 2ª copia             *
008120050217      *         e indica "TOTALE DA ESIGERE AL RITIRO"                *
008130050216      *?37   ?- Condiziona HIGHLIGHT nel campo PDS2  in rec. PRTLDV1  *
008140050217      *                                  per servizio "ESPRESSO"      *
008150050217      *                                o per servizio "POSTE IT."     *
008160050216      *?38   ?- Condiziona FONT      nel campo PAR5  in rec. PRTAR52  *
008170050216      *?39   ?- Condiziona UNDERLINE nel campo PRIG  in rec. PRTGEN   *
008180050217      *                            e nel campo PCTXX in rec. PRTDK5   *
008190050217      *                            e nel campo PCTXT in rec. PRTDTx   *
008200050217      *                            e nel campo PCTXT in rec. PRTDDx   *
008210080604      *?41   ?- Stampa copia per il corriere della 1ª LdV             *
008220080604      *?42   ?- Stampa copia per il destinatario della 1ª LdV         *
008230080604      *?43   ?- Stampa copia per il corriere della 2ª LdV             *
008240080604      *?44   ?- Stampa copia per il destinatario della 2ª LdV         *
008250080918      *?45off?- Posiziona l'inizio della packing-list al 2/4 dell'A4  *
008260080923      *         (packing-list di 2 LdV per A4)                        *
008270080918      *?45on ?- Posiziona l'inizio della packing-list in cima all'A4  *
008280080923      *         (1ª packing-list in A4 dopo la stampa di 4 LdV x A4)  *
008290080923      *?46on ?- Posiziona l'inizio della packing-list a metà dell'A4  *
008300080923      *         (2ª packing-list in A4 dopo la stampa di 4 LdV x A4)  *
008310120530      *?50   ?- Stampa CODICE A BARRE - BRT                           *
008320050217      *?51   ?- Stampa CODICE A BARRE - DPD                           *
008330050217      *?52   ?- Stampa CODICE A BARRE - O.R.M.                        *
008340120530      *?55   ?- NON stampa logo BRT in DDT della Logistica =>         *
008350120530      *         => Stampa il simil-logo in LSB6LOGBRT                 *
008360050217      *?69off?- Stampa su modulo in formato "A4"                      *
008370050217      *?69on ?- Stampa su modulo in formato "A5"                      *
008380120530      *?70   ?- Trattasi di BOLLA BRT                                 *
008390050216      *?71   ?- Trattasi di BOLLA DPD                                 *
008400050216      *?72   ?- Trattasi di BOLLA FedEx                               *
008410080912      *?73   ?- Trattasi di BOLLA EuroExpress                         *
008420030619      *===============================================================*
008430030619      * Descrizione dei campi utilizzati per il posizionamento di     *
008440030619      *   linee orizzontali, linee verticali o campi di stampa        *
008450030619      * (utilizzati solo nel file di stampa per il formato "A4")      *
008460030619      * servono ad impostare la distanza di tali oggetti dal lato     *
008470030619      * superiore del foglio.                                         *
008480030619      *---------------------------------------------------------------*
008490080604      *? BOX nella copia per il vettore:                             ?*
008500080604      *§BOX1AS = posiz. linea orizzontale inizio box mittente (1)   ?*
008510080604      *§BOX1AI = posiz. linea orizzontale fine   box mittente (1)   ?*
008520080604      *§BOX1BS = posiz. linea orizzontale inizio box generale (2)   ?*
008530080604      *§BOX1BI = posiz. linea orizzontale fine   box generale (2)   ?*
008540140922      *§BOX1BJ = posiz. linea orizzontale fine   box BRTcode        ?*
008550080604      *§BOX1CS = posiz. linea orizzontale inizio box contrassegno   ?*
008560080604      *§BOX1CI = posiz. linea orizzontale fine   box contrassegno   ?*
008570080604      *§BOX1DS = posiz. linea orizzontale inizio box tot.da esigere ?*
008580080604      *§BOX1DI = posiz. linea orizzontale fine   box tot.da esigere ?*
008590080604      *§LO1S1  = posiz. linea orizzontale separatrice               ?*
008600080604      *           Destinatario / Colli, Peso, Volume...               *
008610080604      *§LO1S2  = posiz. linea orizzontale separatrice               ?*
008620080922      *           Colli, Peso, Volume / Zona                          *
008630080922      *§LO1S3  = posiz. linea orizzontale separatrice               ?*
008640080922      *           Zona / Segnacolli                                   *
008650080922      *§LO1S4  = posiz. linea orizzontale separatrice               ?*
008660080922      *           Segnacolli / Turno di chiusura                      *
008670080922      *§LO1S5  = posiz. linea orizzontale separatrice               ?*
008680080922      *           Turno di chiusura / "PER RICEVUTA DELLA MERCE..."   *
008690080604      *§LO1C1  = posiz. linea orizzontale separatrice               ?*
008700080624      *           Servizio / Consegne Particolari                     *
008710080604      *§LO1D1  = posiz. linea orizzontale separatrice               ?*
008720080604      *           Riferimenti Mittente / Note e Particolarità         *
008730080605      *§LLV1S1 = lunghezza linea verticale separatrice              ?*
008740080605      *           Destinatario / Riferimento mittente                 *
008750080605      *§LLV1S2 = lunghezza linea verticale separatrice              ?*
008760080605      *           Riferimento mittente / Spedizione                   *
008770120530      *§PC0 <=> §PC6 =  posizioni costanti intestazione "BRT"       ?*
008780120530      *           "BRT SpA", "Sede Operativa ed Amministrativa"       *
008790140917      *§PK1    = posiz. costante "BRTcode" + codice                 ?*
008800080604      *§PK2    = posiz. costante "TASSATIVA"                        ?*
008810080605      *§PK3    = posiz. costanti "(nome e cognome in stampatello)"  ?*
008820080605      *           (in "PER RICEVUTA DELLA MERCE ...")                 *
008830121017      *§PK4    = posiz. costante "COPIA PER IL DESTINATARIO"        ?*
008840121017      *§PK5    = posiz. riferimento alla legge in vigore x RCV      ?*
008850121017      *           (tra n° Spedizione e Imponibile...)                 *
008860121017      *§PK6    = posiz. costante "Bollo assolto in modo virtuale..."?*
008870121017      *§PK7    = posiz. Partita IVA o Cod. Fiscale del cliente      ?*
008880080604      *                                                               *
008890080604      *? Altre posizioni:                                            ?*
008900050325      *§PB1 = posizione linea orizzontale inizio box                ?*
008910050325      *§PB2 = posizione linea orizzontale fine   box                ?*
008920050216      *§LO1 = posizione x 1ª linea orizzontale                      ?*
008930080604      *        LSB6FED1 - richiesta nome e firma per FedEx            *
008940080604      *        LSB6FED3 - limite superiore del box per FedEx          *
008950080604      *        LSB6AR53 - limite superiore dei box da  FIAR5          *
008960030619      *                   (bancali da rendere, supermercato, promem.) *
008970080604      *        LSB6DK0  - linea separatrice nel 1° box DDT Karnak     *
008980080604      *        LSB6DL0A - linea separatrice nel box DDT Logistica     *
008990080604      *        LSB6DL0C - linea separatrice nel box DDT Logistica     *
009000050325      *                   (area Packing List)                         *
009010080604      *        LSB6DL0E - linea separatrice nel box DDT Logistica     *
009020050606      *                   (area Fattura)                              *
009030080604      *        LSB6AC0A - linea separatrice nel box DDT Arcte         *
009040080604      *        LSB6ES0A - linea separatrice nel box DDT Esprinet      *
009050050216      *§LO2 = posizione x 2ª linea orizzontale                      ?*
009060080604      *        LSB6FED1 - linea orizzontale separatrice per FedEx     *
009070080604      *        LSB6FED3 - limite inferiore del box per FedEx          *
009080080604      *        LSB6AR53 - limite inferiore dei box da  FIAR5          *
009090030619      *                   (bancali da rendere, supermercato, promem.) *
009100080604      *        LSB6DK0  - linea separatrice nel 2° box DDT Karnak     *
009110080604      *        LSB6DL0A - linea separatrice nel box DDT Logistica     *
009120080604      *        LSB6DL0C - linea separatrice nel box DDT Logistica     *
009130050325      *                   (area Packing List)                         *
009140080604      *        LSB6DL0E - linea separatrice nel box DDT Logistica     *
009150050610      *                   (area Fattura)                              *
009160080604      *        LSB6AC0A - linea separatrice nel box DDT Arcte         *
009170080604      *        LSB6ES0A - linea separatrice nel box DDT Esprinet      *
009180050216      *§LO3 <=> §LO9/A = posizioni x linee orizzontali              ?*
009190080604      *        LSB6DK0  - linee separatrici nel 2° box DDT Karnak     *
009200080604      *        LSB6DL0B - linee separatrici (orizzontali e verticali) *
009210050218      *                   nella parte finale del box DDT Logistica    *
009220080604      *        LSB6DL0F - linea separatrice nel box DDT Logistica     *
009230050610      *                   (area Fattura nella parte finale del box)   *
009240080604      *        LSB6AC0B - linee separatrici (orizzontali e verticali) *
009250050218      *                   nella parte finale del box DDT Arcte        *
009260080604      *        LSB6ES0B - linee separatrici (orizzontali e verticali) *
009270050216      *                   nella parte finale del box DDT Esprinet     *
009280050216      *§LV1 = posizione x 1ª linea verticale                        ?*
009290080604      *        LSB6FED1 - lineee verticali "lunghe" (0.15 inch) su    *
009300030619      *                   quella orizzontale per FedEx                *
009310080604      *        LSB6FED3 - lineee verticali "lunghe" (0.25 inch) nel   *
009320030619      *                   box per FedEx (separatori)                  *
009330050216      * "   = lunghezza delle linee verticali                       ?*
009340080604      *        LSB6DK0  - linee separatrici nella 1ª parte alta del   *
009350040607      *                   2° box per DDT Karnak                       *
009360080604      *        LSB6DL0A - linee separatrici nella 1ª parte alta del   *
009370050218      *                   box per DDT Logistica                       *
009380080604      *        LSB6DL0C - linee separatrici nella parte alta del box  *
009390050325      *                   DDT Logistica, area Packing List            *
009400080604      *        LSB6DL0E - linea separatrice nel box DDT Logistica     *
009410050610      *                   (area Fattura)                              *
009420080604      *        LSB6AC0A - linee separatrici nella 1ª parte alta del   *
009430050218      *                   box per DDT Arcte                           *
009440080604      *        LSB6AR54 - linea sx "simil" box AR54                   *
009450071025      *                   (v. "Ritiro contestuale alla consegna" o    *
009460071025      *                       "Ritiro porto franco prepagato")        *
009470050216      *§LV2 = posizione x 2ª linea verticale                        ?*
009480080604      *        LSB6FED1 - lineee verticali "corte" (0.07 inch) su     *
009490030619      *                   quella orizzontale per FedEx                *
009500080604      *        LSB6FED3 - lineee verticali "corte" (0.10 inch) nel    *
009510030619      *                   box per FedEx (separatori)                  *
009520050216      * "   = lunghezza delle linee verticali                       ?*
009530080604      *        LSB6DK0  - linee separatrici nella 2ª parte alta del   *
009540040607      *                   2° box per DDT Karnak                       *
009550080604      *        LSB6DL0A - linee separatrici nella 2ª parte alta del   *
009560050218      *                   box per DDT Logistica                       *
009570080604      *        LSB6DL0C - linee separatrici nella parte alta del box  *
009580050325      *                   DDT Logistica, area Packing List            *
009590080604      *        LSB6DL0F - linea separatrice nel box DDT Logistica     *
009600050610      *                   (area Fattura - corpo fattura con totali)   *
009610080604      *        LSB6AC0A - linee separatrici nella 2ª parte alta del   *
009620050218      *                   box per DDT Arcte                           *
009630080604      *        LSB6ES0A - linea separatrice nel 1° box DDT Esprinet   *
009640050216      *§LV3 <=> §LV7   = posizioni x linee verticali                ?*
009650080604      *        LSB6DK0  - linee separatrici nel 2° box DDT Karnak     *
009660080604      *        LSB6DL0A - linee separatrici nel box DDT Logistica     *
009670080604      *        LSB6DL0C - linee separatrici nel box DDT Logistica     *
009680050325      *                   (area Packing List)                         *
009690080604      *        LSB6AC0A - linee separatrici nel box DDT Arcte         *
009700050216      *§PS1 = posizione per campi                                   ?*
009710080604      *        LSB6FED3 - posizione dei campi nel box per FedEx:      *
009720030619      *                   · Package Tracking Number                   *
009730030619      *                   · Status                                    *
009740080604      *        LSB6AR51 - posizione dell'unico campo nel box          *
009750080604      *        LSB6AR52 - posizione dell'unico campo nel box          *
009760080604      *        LSB6AR5A - posizione dell'unico campo nel box          *
009770080604      *        LSB6AR5B - posizione dell'unico campo nel box          *
009780080604      *        LSB6AR5C - posizione dell'unico campo nel box          *
009790080604      *        LSB6AR5G - posizione dell'unico campo nel box          *
009800080604      *        LSB6TAG  - posizione dell'unico campo nel box          *
009810080604      *        LSB6DK0  - posizione della 1ª riga di testo            *
009820081003      *§PS2 <=> §PS8  -> posizioni dei campi                        ?*
009830080604      *        LSB6DK0  - posizioni delle righe di testo              *
009840080604      *        LSB6DL0B - posizioni delle righe di testo              *
009850080604      *        LSB6AC0B - posizioni delle righe di testo              *
009860080604      *        LSB6ES0B - posizioni delle righe di testo              *
009870081003      *        LSB6DL9  - posizioni delle righe di testo              *
009880050216      *§PR1 <=> §PR2  -> posizioni dei campi (rovesciati di 270°)   ?*
009890080604      *        LSB6DK0  - posizioni delle righe di testo              *
009900080604      *        LSB6DL0A - posizioni delle righe di testo              *
009910080604      *        LSB6DL0C - posizioni delle righe di testo              *
009920080604      *        LSB6AC0A - posizioni delle righe di testo              *
009930080604      *        LSB6ES0A - posizioni delle righe di testo              *
009940030619      *===============================================================*
009950030515
009960030515     c     *Entry        plist
009970080703     c                   parm                    FNLSB5ds
009980130710     c                   parm                    Prm_LNP           3 0
009990130710     c                   parm                    Prm_NRS           2 0
010000130306     c                   parm                    FNLSB6ds1
010010110427
010020110427       // -?Tipo lancio (DB0tla):?"C" -           Chiudo con LR?
010030110427       //                        ?"L" - Elaboro e chiudo con LR?
010040110427       //                        ?" " - Elaboro e chiudo in RETRN?
010050080703if  1c                   if        DB0tla = 'C'
010060030515     c                   goto      Fine
010070060921e   1c                   endif
010080110427
010090110427       // -?Operazioni iniziali?
010100070123     c                   exsr      sr_Start
010110110427
010120030515sel 1c                   SELECT
010130130306
010140130306       // -?Richiesta la stampa (finale) delle sole immagini PDF?
010150130710     c                   WHEN      %parms() > 3  and  DB6pdf = 'S'
010160130307     c*//                                        and  DB0fa4 = 'I'
010170130306     c                   if        DB6num > *zero
010180130306     c                   exsr      sr_PrtPdf
010190130306     c                   endif
010200130306     c                   exsr      sr_End
010210110427
010220110427       // -?NON passati i dati LDV => caricamento?
010230030515w   1c                   WHEN      DB0ges = *blanks
010240071008if  2c                   if             DB0tbo  = 'P'
010250071008     c                             or   DB0tbo  = '8'
010260030515     c                   exsr      CarBLP
010270030515x   2c                   else
010280030515     c                   exsr      CarARB
010290030515e   2c                   endif
010300110427
010310110427       // -?Passati i dati LDV => è già (quasi) tutto pronto?
010320110427       //  ?(solo dalla sede - vedi TNSD50R)?
010330081114w   1c                   WHEN      DB0ges = 'B'
010340090513     c                   exsr      CarTAB
010350081114     c*** (già fatto:)   clear                   dsBL4A
010360081114     c                   if        DB0fbr = 'S'
010370081114     c                   exsr      RepAR4_A
010380081114     c***                if        §B4Abm = 'S' or §B4Abm = 'C' or
010390081114     c***                          §B4Abm = 'P' or §B4Abm = 'Q'
010400081114     c***                movel     'S'           DB0bam
010410081114     c***                endif
010420081114     c                   endif
010430081114     c                   eval      wB0cfi = DB0cpi
010440081114     c                   eval      wB0piv = DB0cpi
010450110427
010460110427       // -?DB0GES = "R" => Stampa una LDV vuota con un messaggio?
010470110427       //  ?per avvisare che è impossibile continuare la ristampa?
010480110427       //  ?perchè stampate più di 6.000 bolle !?
010490030515w   1c                   WHEN      DB0ges = 'R'
010500071112     c                   exsr      Rep_TabVPO
010510071112     c                   eval      Pmsg1  = $Msg(1)
010520071112     c                   eval      Pmsg2  = %replace( %editc(
010530071112     c                                         §VPOmaxldv : 'J' ) :
010540071112     c                                         $Msg(2) : %scan(
010550071112     c                                         '&NrLdV' : $Msg(2) ))
010560130418     c                   eval      *in33 = *off
010570130418     c                   exsr      Posiz_Box
010580030515     c                   write     A4prtMSG
010590030515     c                   goto      Fine
010600110427
010610030515e   1c                   ENDSL
010620110427
010630110427       // -?Controllo del tipo LDV da stampare?
010640040607     c                   exsr      TipoLDV
010650080919      /free
010660080919
010670110427       //?»»»?Verifiche per chiusura pgm?«««?
010680080919       SELECT;
010690080919
010700110427         // -?Chiude SE richiamato per la sola verifica del modulo?
010710110427         //  ?(dal pgm FNLSE6R1 - cl FNLSE6C1)?
010720080919         WHEN  DB0ris  = 'R'   and
010730080919               DB0fl9  = *on   and
010740080919               DUTlpo  = *blanks;
010750080919           exsr  sr_End;
010760080919
010770110427         // -?Chiude SE richiesta la stampa in formato NON corrispondente?
010780080926
010790110427         // ·?Richiesta stampa di A4, mentre risulta di ex-A5?
010800110427         //  ?NON GESTITO:?rimane il richiamo "standard" dei pgm. di?
010810110427         //  ?manutenzione => 1 A4 x ogni LdV (anche se ex-A5)?
010820080919         //WHEN  DB0fa4  = '1'   and
010830080919         //      *in69   = *on;
010840080919         //  DB0tms = 'E';
010850080919         //  exsr  sr_End;
010860080919
010870110427         // -?Richiesta stampa di ex-A5, mentre risulta di A4?
010880080919         WHEN  DB0fa4  = '2'   and
010890080919               *in69   = *off;
010900080919           DB0tms = 'D';
010910080919           exsr  sr_End;
010920080919
010930080919       ENDSL;
010940080919
010950080919      /end-free
010960110427
010970110427       // -?Imposta i campi di stampa con quelli della DS?
010980030515     c                   exsr      PrepPRT
010990110427
011000110427       // -?Operazioni "inutili"?SE?richiesta la stampa della sola?
011010110427       //  ?packing-list:?
011020080919     c                   IF        DB0fa4 <> 'P'
011030110427
011040110427       // -?Decodifica i dati della LDV?
011050040607     c                   exsr      DecodLDV
011060110427
011070080919     c                   ENDIF
011080110427
011090110427       // -?Stampa della Lettera di Vettura?
011100030515sel 1c                   SELECT
011110110427       // -?Se RIMESSO MITTENTE stampo il numero di copie richiesto?
011120110427       //  ?(non per DPD e FedEx)?
011130110427       //  ?(le stampo qui perché devo stampare il riferimento numerico?
011140110427       //  ?aggiornato di volta in volta su FLNUF)?
011150030515w   1c                   WHEN      DB0ris = 'M'  and  *in70
011160030515     c                   clear                   wCOPIE
011170030515     c                   clear                   NUFaaa
011180030515     c                   eval      NUFcnu = 20
011190030515     c                   eval      NUFfil = DB0lnp
011200030515     c                   clear                   NUFfl2
011210030515do  2c                   DO        Wctr
011220030515     c     K04NUF        chain     FLNUF01L
011230030515if  3c                   if        %found(FLNUF01L)
011240030515     c                   add       1             NUFnum
011250030515     c                   UPDATE    FLNUF
011260030515     c                   eval      Prmn = NUFnum
011270040607     c                   exsr      StampaLDV
011280030515e   3c                   endif
011290030515e   2c                   ENDDO
011300110427       // -?Se FATTURA PREPAGATA stampo due copie?
011310110427       //  ?(solo se non attivata procedura O.R.M.)?
011320030515w   1c                   WHEN      DB0tbo = '8'  and  §OGorm <> 'S'
011330030515     c                   eval      wCOPIE = 2
011340110427       // -?Altrimenti stampo solo una copia?
011350030515x   1c                   OTHER
011360030515     c                   eval      wCOPIE = 1
011370030515e   1c                   ENDSL
011380110427
011390030515do  1c                   do        wCOPIE
011400080918      /free
011410080918         if  DB0fa4 = 'P';
011420130315           exsr  StampaPkgLst;
011430080918         else;
011440110427           exsr  StampaLDV;
011450080918         endif;
011460080918      /end-free
011470030515e   1c                   enddo
011480130306
011490130306       // -?Eventuale stampa immagini PDF?
011500130306       //  ?(si spera della sola spedizione di cui si è appena?
011510130306       //   ?stampata la LdV)?
011520130710     c                   if        %parms() > 3  and  DB6pdf = 'X'
011530130306     c                                           and  DB6num > *zero
011540130306     c                   exsr      sr_PrtPDF
011550130306     c                   endif
011560110427
011570110427       // -?Operazioni finali?
011580110427     c     Fine          tag
011590080624     c                   exsr      sr_End
011600110427
011610030515      *---------------------------------------------------------------*
011620050217      *?OPERAZIONI INIZIALI                                          ?*
011630030515      *---------------------------------------------------------------*
011640030515     c     *InzSR        BEGSR
011650131023      *
011660131023      * Reperimento dati utente
011670131023      * Reperisco nome azienda per stampa PREPAGATI (e non solo)
011680131023     c     *dtaara       define    §azute        AZUTEds
011690131023     c     *dtaara       define    §datiute      dDATIUTE
011700131023     c                   in(E)     *dtaara
011710131023     c                   if        %ERROR or RSUT = *blanks
011720131023     c                   clear                   Tibs34Ds
011730131023     c                   call      'TIBS34R'
011740131023     c                   parm                    Tibs34Ds
011750131023     c                   in        *dtaara
011760131023     c                   endif
011770131023      * Impostazione dati in KPJBA
011780131023     c                   eval      KNMUS  = SDSusr
011790110429      *
011800131023      *USCITA? SE  richiamato per la sola chiusura
011810130314     c*//                if        %subst(KPJBU:1:1) = 'C'
011820130314     c                   if        DB0tla = 'C'
011830110429     c                   leavesr
011840110429     c                   endif
011850070424      *
011860070424      * Reperisco la data del giorno
011870070424     c                   time                    w0140
011880070424     c                   move      w0140         w0080
011890070424     c     *eur          movel     w0080         Data_EUR
011900070424     c     *iso          movel     Data_EUR      w0080
011910070424     c                   move      w0080         wDateA
011920071105     c                   eval      wDate  = w0080
011930030515      *
011940030515      * Imposto i campi chiave "fissi"
011950040223     c                   eval      TBLkut = 1
011960060505     c                   eval      ARStrc = 'C'
011970070319     c                   eval      ARNcdn = 'LDV'
011980070319     c                   eval      ARNgst = 'S'
011990030515      *
012000030515      * Carico le varie esenti IVA
012010030515     c                   movel     'CC'          TBLcod
012020030515     c                   movel(p)  'VARIE'       TBLkey
012030030515     c                   clear                   XX
012040030515     c     K03TAB        setll     TABEL
012050030515     c     K02TAB        reade     TABEL
012060030515      *
012070070306do  1c                   DOW       NOT %eof(TABEL00F)
012080030515      * Deve essere NON annullata e con codice che inizia con "VARIE"
012090030515if  2c                   If        %subst(TBLkey:1:5) = 'VARIE'
012100030515     c                             and    TBLflg      = *blanks
012110030515     c                   movel     TBLuni        dsCC
012120030515if  3c                   if        §CCcei <> *blanks
012130030515     c                   add       1             xx
012140030515     c                   move      TBLkey        $Var(xx)
012150030515e   3c                   endif
012160030515e   2c                   EndIf
012170030515      *
012180030515     c     K02TAB        reade     TABEL
012190030515e   1c                   ENDDO
012200070306      *
012210070308      * Carico le causali eventi con tipo consegna = "G"
012220070306     c                   movel     '2A'          TBLcod
012230070306     c                   clear                   TBLkey
012240070306     c                   clear                   XX
012250070306     c     K03TAB        setll     TABEL
012260070306     c     K02TAB        reade     TABEL
012270070306      *
012280070306do  1c                   DOW       NOT %eof(TABEL00F)
012290070308      * Deve essere NON annullata e con flag §2AFTC = "G"
012300070308if  2c                   If        TBLflg = *blanks
012310070306     c                   movel     TBLuni        ds2A
012320070308if  3c                   if        §2Aftc = 'G'
012330070306     c                   add       1             xx
012340070308     c                   movel     TBLkey        $2Acev_G(xx)
012350070306e   3c                   endif
012360070306e   2c                   EndIf
012370070306      *
012380070306     c     K02TAB        reade     TABEL
012390070306e   1c                   ENDDO
012400030515      *
012410030515      * Carico tabella VARIABILI ARRIVI
012420030515     c                   clear                   ds$2
012430030515     c                   eval      TBLcod = '$2'
012440030515     c                   eval      TBLkey = '1'
012450030515     c     K03TAB        chain     TABEL
012460030515if  1c                   if        %found(TABEL00F)
012470030515     c                   movel     TBLuni        ds$2
012480030515e   1c                   endif
012490030515      *
012500030515      * Reperisco la moneta di conto
012510030515     c                   clear                   dGED
012520040607     c                   clear                   TIBS02ds
012530040607     c                   eval      T02mod = 'C'
012540040607     c                   eval      T02cod = 'GED'
012550040607     c                   eval      T02ke1 = '1         '
012560030515     c                   call      'TIBS02R'
012570030515     c                   parm                    KPJBA
012580040607     c                   parm                    TIBS02ds
012590030515if  1c                   if        T02err = *blanks
012600030515     c                   movel     T02uni        dGED
012610030515e   1c                   endif
012620030515      *
012630030515      * Con moneta di conto:
012640060301      *   Carico tabella GESTIONALE - IMPORTI VARI   e
012650030515      *   Reperisco l'importo limite per abilitazione c/a
012660030515     c                   clear                   dGEI
012670040607     c                   clear                   TIBS02ds
012680040607     c                   eval      T02mod = 'C'
012690040607     c                   eval      T02cod = 'GEI'
012700040607     c                   eval      T02ke1 = §GEDcn
012710030515     c                   call      'TIBS02R'
012720030515     c                   parm                    KPJBA
012730040607     c                   parm                    TIBS02ds
012740030515if  1c                   if        T02err = *blanks
012750030515     c                   movel     T02uni        dGEI
012760030515e   1c                   endif
012770061031      *
012780110426      * Reperisco data di decorrenza stampa "Per ricevuta di ...
012790071005      *                                      senza riserve"
012800090615      *              e di decorrenza stampa per C.L.N.
012810061031     c                   clear                   dVPOdeco
012820061031     c                   clear                   TIBS02ds
012830061031     c                   eval      T02mod = 'C'
012840071112     c***                eval      T02sif  = KNSIF
012850071112     c                   eval      T02cod = 'VPO'
012860061031     c                   eval      T02ke1 = 'DECO      '
012870061031     c                   call      'TIBS02R'
012880061031     c                   parm                    KPJBA
012890061031     c                   parm                    TIBS02ds
012900061031if  1c                   if        T02err = *blanks
012910061031     c                   movel     T02uni        dVPOdeco
012920061031e   1c                   endif
012930120530      *
012940120530      *//*?Reperisco data di decorrenza stampa nuovo logo BRT?
012950120530     c*//                clear                   dVPObrt
012960120530     c*//                clear                   TIBS02ds
012970120530     c*//                eval      T02mod = 'C'
012980120530     c***                eval      T02sif  = KNSIF
012990120530     c*//                eval      T02cod = 'VPO'
013000120530     c*//                eval      T02ke1 = 'BRT       '
013010120530     c*//                call      'TIBS02R'
013020120530     c*//                parm                    KPJBA
013030120530     c*//                parm                    TIBS02ds
013040120530if  1c*//                if        T02err = *blanks
013050120530     c*//                movel     T02uni        dVPObrt
013060120530e   1c*//                endif
013070080708      /free
013080110426         // -?Override al file di stampa?
013090081017         if  DB0tla <> 'C';
013100080709           fnlsB8r ( fnlsB5ds );
013110080708         endif;
013120080708      /end-free
013130030515      *
013140030515      * Apro i file di stampa
013150130710     c                   open      FNLSB6P
013160080918     c                   clear                   CurLineA4
013170130306      /free
013180130306         // -?Reperimento override apportate al file di stampa?
013190130306         //  ?per comunicarle per la stampa immagini PDF?
013200130306         if  DB0tla <> 'C';
013210130306           wPrtF = 'FNLSB6P';
013220130306           clear  trulPOVRds;
013230130307           trulPOVRc ( wPrtF : trulPOVRds);
013240130307           if trulPOVRds.§ULPoerr <> *blank;  //?Nessuna override rilevata?
013250130321             //trulPOVRds.§ULPoOutQ  = '*JOB      ';
013260130321             trulPOVRds.§ULPoOutQ  = 'PRT01     ';
013270130307             trulPOVRds.§ULPoFType = '*LDVUNI   ';
013280130307             trulPOVRds.§ULPoUDta  = 'LDV_UNI   ';
013290130306           endif;
013300130306         endif;
013310130306      /end-free
013320030515      *
013330030515     c                   ENDSR
013340071112      *
013350071112      *---------------------------------------------------------------*
013360071112      *?REPERIMENTO N° MAX DI LDV STAMPABILI DA TAB. "VPO"           ?*
013370071112      *---------------------------------------------------------------*
013380071112     c     Rep_TabVPO    BEGSR
013390071112      *
013400071112      * Aggancio tabella "VPO" per reperimento del n° LdV stampabili
013410071112      *   limite (per congelamento e blocco stampa)
013420071112     c                   clear                   dVPO
013430071112     c                   clear                   TIBS02ds
013440071112     c                   eval      T02mod  = 'C'
013450071112     c***                eval      T02sif  = KNSIF
013460071112     c                   eval      T02cod  = 'VPO'
013470071112     c                   movel(P)  'VPO'         T02ke1
013480071112     c                   call      'TIBS02R'
013490071112     c                   parm                    KPJBA
013500071112     c                   parm                    TIBS02ds
013510071112     c                   if        T02err  = *blanks
013520071112     c                   movel     T02uni        dVPO
013530071203     c                   if        §VPOMaxLdv = *zeros
013540071203     c                   eval      §VPOMaxLdv = 10000
013550071203     c                   endif
013560071112     c                   else
013570071112     c                   eval      §VPOMaxLdv = 10000
013580071112     c                   endif
013590071112      *
013600071112     c                   ENDSR
013610070123      *
013620070123      *---------------------------------------------------------------*
013630070306      *?ALTRE OPERAZIONI INIZIALI (NON PIÙ IN *INZSR)                ?*
013640070123      *---------------------------------------------------------------*
013650070123     c     sr_Start      BEGSR
013660070123      *
013670080709      /free
013680081017         select;
013690081017           when  DB0fa4 = *blank;
013700081017             DB0fa4 = '1';
013710081020           when  DB0fa4 = 'P';
013720081017             DB0fl6 = *on;
013730081017         endsl;
013740080709      /end-free
013750080709      *
013760070123if  1c                   if        DB0fl7 = '1'
013770130710     c                   close     FNLSB6P
013780130710     c                   open      FNLSB6P
013790081015     c                   clear                   CurLineA4
013800070123     c                   clear                   Page4
013810070123e   1c                   endif
013820070123      *
013830070123      * »»» CHIODO: SE richiamato per la sola verifica del modulo «««
013840070308      *     RIPULISCO il "livello della filiale" (S/1/2) perchè:
013850070123      *     1) sarò partito dal S.I. di sede, ma
013860070123      *     2) sarà stata cambiata la lista di librerie come x S.I. di
013870070308      *        filiale (dal CL FNLSE6C1)
013880070123if  1c                   if             DB0ris  = 'R'
013890070123     c                             and  DB0fl9  = *on
013900070123     c                             and  DUTlpo <> *blanks
013910070123     c                   clear                   DUTlpo
013920070123e   1c                   endif
013930070123      *
013940070308      * Apertura archivi di sede o filiale - comunque utilizzati
013950070123if  1c                   if        DUTlpo = 'S'
013960070123if  2c                   if        NOT  %open(FIAR531C)
013970070123     c                   open      FIAR531C
013980070123e   2c                   endif
013990070123x   1c                   else
014000070123if  2c                   if        NOT  %open(FIAR401L)
014010070123     c                   open      FIAR401L
014020070123e   2c                   endif
014030070123if  2c                   if        NOT  %open(FIAR501L)
014040070123     c                   open      FIAR501L
014050070123e   2c                   endif
014060070123if  2c                   if        NOT  %open(FLNUF01L)
014070070123     c                   open      FLNUF01L
014080070123e   2c                   endif
014090070123e   1c                   endif
014100110427      *
014110110427      * Pulizia del PRTF
014120110427     c                   exsr      ClrPRTF
014130070123      *
014140070123     c                   clear                   wCOPIE
014150070123      *
014160070123     c                   ENDSR
014170080624      *
014180080624      *---------------------------------------------------------------*
014190080624      *?OPERAZIONI FINALI                                            ?*
014200080624      *---------------------------------------------------------------*
014210080624     c     sr_End        BEGSR
014220080624      *
014230080624      * Chiusura pgm
014240080919if  1c                   IF        DB0tla <> *blanks
014250130710     c                   close     FNLSB6P
014260080708      /free
014270080708       Qcmd = C_CmdDltOvr;
014280080708       callp(e) qCmdExc(Qcmd : %size(Qcmd));
014290080708      /end-free
014300080624     c                   clear                   TIBS02ds
014310080624     c                   eval      T02tla = 'C'
014320080624     c                   call      'TIBS02R'
014330080624     c                   parm                    KPJBA
014340080624     c                   parm                    TIBS02ds
014350080624     c                   reset                   FNLV80ds
014360080624     c                   call      'FNLV80R'
014370080624     c                   parm                    KPJBA
014380080624     c                   parm                    FNLV80ds
014390080624     c                   clear                   FIDN12ds
014400080624     c                   eval      I12tla = 'C'
014410080624     c                   call      'FIDN12R'
014420080624     c                   parm                    FIDN12ds
014430090605      /free
014440090622           if  §VPOrcm <= wDate   and   DUTlpo  <> 'S';
014450090615             reset  TNSD99ds;
014460090615             tnsd99r ( TNSD99ds );
014470090615           endif;
014480100322           if  §VPOcas <= wDate   and   DUTlpo  <> 'S';
014490100208             clear  trulINTds;
014500100208             iINTtla = 'C';
014510100208             trulINTr ( TRULINTds );
014520100322           endif;
014530090605      /end-free
014540080624     c                   eval      *inLR  = *on
014550080624e   1c                   ENDIF
014560080919      *
014570080919      * Ritorno al chiamante
014580080919     c                   return
014590080624      *
014600080624     c                   ENDSR
014610030515      *
014620030515      *---------------------------------------------------------------*
014630050217      *?Pulisco i campi di stampa                                    ?*
014640030515      *---------------------------------------------------------------*
014650030515     c     ClrPRTF       BEGSR
014660030515      *
014670130418     c                   clear                   A4prtBOX
014680120530     c*//                clear                   A4LogoBAR
014690120530     c*//                clear                   A4LogoDA1
014700120530     c*//                clear                   A4LogoBR2
014710120530     c*//                clear                   A4LogoDA2
014720120530     c                   clear                   A4LogoBRT
014730120530     c                   clear                   A4LogoDA3
014740130418     c                   clear                   A4prtLDV
014750080604     c                   clear                   A4prtLDVB
014760080922     c                   clear                   A4prtDPD
014770030515     c                   clear                   A4prtFED1
014780030515     c                   clear                   A4prtFED2
014790030515     c                   clear                   A4prtFED3
014800030515     c                   clear                   A4prtAR51
014810030515     c                   clear                   A4prtAR52
014820030515     c                   clear                   A4prtAR53
014830030909     c                   clear                   A4prtAR5A
014840040607     c*** stessi campi:  clear                   A4prtAR5B
014850040607     c*** stessi campi:  clear                   A4prtAR5C
014860070321     c*** stessi campi:  clear                   A4prtAR5G
014870040311     c*** no campi:      clear                   A4prtSP0
014880040607     c*** no campi:      clear                   A4prtSP1
014890080709     c*** no campi:      clear                   A4prtSP2
014900040311     c*** no campi:      clear                   A4prtNUL
014910030911     c                   clear                   A4prtGEN
014920040607     c                   clear                   A4prtDK0
014930040607     c                   clear                   A4prtDK1
014940040607     c                   clear                   A4prtDK2
014950040607     c                   clear                   A4prtDK3
014960040607     c                   clear                   A4prtDK4
014970040607     c                   clear                   A4prtDK5
014980081222     c                   clear                   A4prtDLL
014990041019     c                   clear                   A4prtDL0
015000050318     c                   clear                   A4prtDL0A
015010050318     c                   clear                   A4prtDL0B
015020050318     c                   clear                   A4prtDL0C
015030050606     c                   clear                   A4prtDL0D
015040050606     c                   clear                   A4prtDL0E
015050050606     c                   clear                   A4prtDL0F
015060041028     c                   clear                   A4prtDL1A
015070041028     c                   clear                   A4prtDL1B
015080050318     c                   clear                   A4prtDL1C
015090050606     c                   clear                   A4prtDL1D
015100050606     c                   clear                   A4prtDL1E
015110041019     c                   clear                   A4prtDL2
015120060614     c                   clear                   A4prtDL21
015130041019     c                   clear                   A4prtDL3
015140041019     c                   clear                   A4prtDL4
015150050318     c                   clear                   A4prtDL5
015160050606     c                   clear                   A4prtDL6
015170050606     c                   clear                   A4prtDL7
015180050606     c                   clear                   A4prtDL8
015190050606     c                   clear                   A4prtDL9
015200050114     c                   clear                   A4prtAC0
015210050114     c                   clear                   A4prtAC1A
015220050114     c                   clear                   A4prtAC1B
015230050114     c                   clear                   A4prtAC2
015240050114     c                   clear                   A4prtAC3
015250050114     c                   clear                   A4prtAC4
015260050214     c                   clear                   A4prtES0
015270050214     c                   clear                   A4prtES0A
015280050214     c                   clear                   A4prtES0B
015290050215     c                   clear                   A4prtES1A
015300050215     c                   clear                   A4prtES1B
015310050214     c                   clear                   A4prtES2
015320050214     c                   clear                   A4prtES3
015330050215     c                   clear                   A4prtES4
015340050214     c                   clear                   A4prtDB0
015350041207     c                   clear                   A4prtDT0
015360040607     c                   clear                   A4prtDT1
015370040607     c*** stessi campi:  clear                   A4prtDT2
015380040607     c*** stessi campi:  clear                   A4prtDC1
015390030515     c                   clear                   A4prtSEG
015400030515     c                   clear                   A4prtMSG
015410030515      *
015420041207     c                   reset                   dAR8
015430060524     c                   clear                   dsBL4A
015440061030     c                   clear                   dsBL4I
015450061108     c                   clear                   dTA4Q
015460100514     c                   clear                   dOrm01
015470041207      *
015480070514     c                   reset                   $Mamma
015490130215     c                   reset                   $Mamma_FedEx
015500070306     c                   reset                   $FIARN
015510060921     c                   reset                   $FIAR5
015520030515     c                   reset                   $First
015530030515     c                   reset                   $FirstSCR
015540060921     c                   reset                   $FIAR8
015550130306     c                   reset                   $§PDF
015560040608     c                   reset                   $DDT_o
015570040608     c                   reset                   $DDT_p
015580040608     c                   reset                   $DDTlastPg
015590030515     c                   reset                   $Partic
015600030710     c                   reset                   $NoDesPartic
015610030910     c                   reset                   $BolloVirt
015620061106     c                   reset                   $No_CF
015630071114     c                   reset                   $No_PI
015640040707     c                   reset                   $No_x_Cons
015650101125     c                   reset                   $Rit_A_Inc
015660050513     c                   reset                   $A4_x_ScNS
015670070306     c                   reset                   $LascAvv
015680070319     c                   reset                   $ConsParz
015690070315     c                   reset                   $C_A_
015700030515     c                   reset                   $CAeur
015710090302     c                   reset                   $TabPXA
015720110411     c                   reset                   $TabPXA2
015730090924     c                   reset                   $BoxNxD
015740100513     c                   reset                   $PreavvORM
015750110427     c                   reset                   $GeoAtt
015760070717     c                   clear                   wGeoTel
015770070717     c                   clear                   wGeoCon
015780070717     c                   clear                   Wdsp
015790090527     c                   clear                   WddtCLN
015800090527     c                   clear                   WdspCLN
015810030717     c                   clear                   W_CoOr
015820040608     c                   clear                   wOmaRowT
015830040608     c                   clear                   wOmaRowD
015840050624     c                   clear                   wBolloVirt
015850030515     c                   clear                   Wcas
015860030515     c                   clear                   Wiva
015870071015     c                   clear                   Wpdc4a
015880080926     c                   clear                   wPage
015890080704     c                   reset                   $LdVxP
015900090121     c*** (da non fare:) clear                   wNumLdV
015910090121     c*** (da non fare:) clear                   wSeqLdV
015920071114     c                   clear                   wB0cfi
015930071114     c                   clear                   wB0piv
015940031015     c                   clear                   wB0nrs
015950070514     c                   clear                   wB0ndc
015960070514     c                   clear                   wB0ntc
015970070514     c                   clear                   wB0dcp
015980081119     c                   clear                   wB0tmc
015990101125     c                   clear                   wB0dcm
016000101125     c                   clear                   wB0icc
016010101125     c                   clear                   wB0ica
016020130124     c                   clear                   wB0fiv
016030140307     c                   clear                   wEVBdev
016040140307     c                   clear                   wEVBhev
016050030515      *
016060030515     c                   clear                   $NP
016070030515     c                   clear                   $SC1
016080030515     c                   clear                   $SC2
016090030905     c                   clear                   $QUM
016100040607     c                   clear                   $AR8o1
016110070319     c                   reset                   $CP
016120070319     c                   reset                   $CPdcm
016130030515      *
016140030515     c                   movea     *zeros        *in(01)
016150030515      *
016160030515     c                   ENDSR
016170030515      *
016180030515      *---------------------------------------------------------------*
016190050217      *?Determino il modulo LDV da usare per la stampa               ?*
016200120530      *?(se A5 o A4 e se DPD, FedEx o BRT)                           ?*
016210030515      *---------------------------------------------------------------*
016220040607     c     TipoLDV       BEGSR
016230070212      *
016240080703      * Impostazione ds "Flag operativi DB0FLO di FNLSB5DS"
016250070215     c                   movel     DB0flo        dDB0flo
016260070215      *
016270070212      * Impostazione della data spedizione
016280070212      * (se non già impostata in subr. "CarBLP" o "CarARB")
016290070212if  1c                   if        Wdsp   =  *zeros
016300070212if  2c                   if        DB0mgs <> *zeros
016310070212     c                   eval      Wdsp   =  (DB0aas * 10000) + DB0mgs
016320070212x   2c                   else
016330070212      *   prendo la data del giorno se non c'è la data spedizione
016340070212     c                   time                    w0140
016350070212     c                   move      w0140         w0080
016360070212     c     *eur          movel     w0080         Data_EUR
016370070212     c     *iso          movel     Data_EUR      Wdsp
016380070212e   2c                   endif
016390070212e   1c                   endif
016400040311      *
016410040311      * Dati della linea di partenza
016420040311if  1c                   if        DB0lnp <> SAVlnp
016430040311     c                   eval      SAVlnp =  DB0lnp
016440040311     c     DB0lnp        chain     AZORG
016450040311if  2c                   if        %found(AZORG01L)
016460040311     c                   movel     ORGde3        OG143
016470040311     c                   movel     ORGde8        OG148
016480040311x   2c                   else
016490040311     c                   clear                   OG143
016500040311     c                   clear                   OG148
016510040311e   2c                   endif
016520040311e   1c                   endif
016530040607      *
016540070308      * Dati della filiale del mittente
016550040607     c                   eval      WX     =  DB0ccm / 10000
016560040607if  1c                   if        WX     <> ORGfil
016570040607     c     WX            chain     AZORG
016580040607e   1c                   endif
016590040607if  1c                   if        %found(AZORG01L)
016600040607     c                   movel     ORGde3        OG143ccm
016610040607x   1c                   else
016620040607     c                   clear                   OG143ccm
016630040607e   1c                   endif
016640040311      *
016650040311      * Dati della linea di arrivo
016660040311if  1c                   if        DB0lna <> SAVlna
016670040311     c     DB0lna        chain     AZORG
016680040311if  2c                   if        %found(AZORG01L)
016690070312     c                   movel     ORGde6        OG146arr
016700061013     c                   movel     ORGde8        OG148arr
016710070312     c                   movel     ORGdf0        OG150arr
016720040311x   2c                   else
016730070312     c                   clear                   OG146arr
016740061013     c                   clear                   OG148arr
016750070312     c                   clear                   OG150arr
016760040311e   2c                   endif
016770050510e   1c                   endif
016780070212      *
016790070212      * Dati della capofila della linea di arrivo
016800070306      * - per verificare TELLUS   e
016810070306      * - per non stampare la "copia per il destinatario" nel caso
016820070306      *   di precedente "Lasciato Avviso"
016830170717      * - per i dati della LINEA DI ARRIVO in stampa
016840170717      *
016850170717      * - Reperimento capofila (£6)
016860170905      *   (ad oggi, NON alla data della spedizione)
016870170717if  1c                   if        DB0lna <> D55lin
016880170717     c                   reset                   FNLV55ds
016890170717     c*** già così:      eval      D55tpt =  '6'
016900170717     c                   eval      D55lin =  DB0lna
016910170905     c*//                eval      D55drf =  Wdsp
016920170905     c                   eval      D55drf =  wDate
016930170717     c                   call      'FNLV55R'
016940170717     c                   parm                    FNLV55ds
016950170717if  2c                   if        D55err <> *blanks
016960170717     c                   eval      D55tfa =  DB0lna
016970170717e   2c                   endif
016980170717e   1c                   endif
016990170717      * - Reperimento 146ª e 148ª descrizioni dell'organigramma
017000170717      *   (flag Tellus del capofila e flag gestione telefonate GEO)
017010170717if  1c                   if        D55tfa <> ORGfil
017020170717     c     D55tfa        chain     AZORG
017030170718e   1c                   endif
017040170718if  1c                   if        %found(AZORG01L)
017050170717     c                   eval      wDB0lnaCf£6 = ORGfil
017060170717     c                   eval      wDB0dlaCf£6 = ORGdes
017070170717     c                   eval      wDB0inlCf£6 = ORGind
017080170717     c                   eval      wDB0calCf£6 = ORGcpf
017090170717     c                   eval      wDB0lolCf£6 = ORGloc
017100170717     c                   eval      wDB0prlCf£6 = ORGpro
017110170717     c                   eval      wDB0telCf£6 = ORGtel
017120170717     c*//                eval      wDB0faxCf£6 = ORGfax
017130170717     c                   eval      wDB0faxCf£6 = ORGdd2
017140170717     c                   movel     ORGde6        OG146cf£6
017150170717     c                   movel     ORGde8        OG148cf£6
017160170718x   1c                   else
017170170717     c                   clear                   wDB0lnaCf£6
017180170717     c                   clear                   wDB0dlaCf£6
017190170717     c                   clear                   wDB0inlCf£6
017200170717     c                   clear                   wDB0calCf£6
017210170717     c                   clear                   wDB0lolCf£6
017220170717     c                   clear                   wDB0prlCf£6
017230170717     c                   clear                   wDB0telCf£6
017240170717     c                   clear                   wDB0faxCf£6
017250170717     c                   clear                   OG146cf£6
017260170717     c                   clear                   OG148cf£6
017270170718e   1c                   endif
017280070717      *
017290070717      *** Imposta flag attivazione GEO x LNA
017300070717     c                   eval      wGeoCon = A§OGtlls
017310070717     c                   eval      wGeoTel = A§OGgeot
017320070717if  1c                   if             A§OGdda  > *zeros
017330070717     c                             and  A§OGdda  < C_DateMax_A
017340070717     c                             and  A§OGdda <= wDateA
017350110427     c                   eval      $GeoAtt = *on
017360070717e   1c                   endif
017370070717      *
017380070717if  1c                   IF        DB0tbo = 'A'  AND
017390070717     c                                 (wGeoCon <> 'S'
017400070717     c                              or  wGeoTel <> 'S'
017410110427     c                              or  $GeoAtt <> *on)
017420070717      *** Imposta flag attivazione GEO x capofila £6
017430070717     c                   eval      wGeoCon = £6OGtlls
017440070717     c                   eval      wGeoTel = £6OGgeot
017450071109if  2c                   if             £6OGdda  > *zeros
017460070717     c                             and  £6OGdda  < C_DateMax_A
017470070717     c                             and  £6OGdda <= wDateA
017480110427     c                   eval      $GeoAtt = *on
017490071109e   2c                   endif
017500070717      *
017510070212e   1c                   ENDIF
017520050510      *
017530050510      * Verifica se richiesta stampa in A4 in tab. "7R"
017540050510      *   (gestione particolarità consegna)
017550050510     c                   clear                   ds7R
017560170717if  1c                   if        DB0gma <> *blanks
017570070312     c                   eval      TBLcod =  '7R'
017580050510     c                   movel(p)  DB0gma        TBLkey
017590050510     c     K03TAB        chain     TABEL
017600050510if  2c                   if        %found(TABEL00F)
017610050510     c                   movel     TBLuni        ds7R
017620050510e   2c                   endif
017630050510e   1c                   endif
017640071015      *
017650071015      * Verifica se LDV con "Ritiro Porto Franco Prepagato"
017660071015     c                   clear                   dsBL4M
017670071015     c                   if        DB0tbo = '8'
017680071015     c                   exsr      RepAR4_M
017690071015     c                   endif
017700130315      *
017710130315      * Reperimento dati da FIAR5 / "GEN"
017720130315      *   (per verificare se esistono Packing-List e/o PDF allegati
017730130315      *    - oltre che per reperire Nominativo e Telefono)
017740130315     c                   exsr      RepAR5gen
017750040607      *
017760130315      * Verifica se LDV con DDT e/o PDF
017770130315     c                   exsr      CercaDDToPDF
017780061031      *
017790071114      * Verifica se richiesta stampa in A4 per richiesta Codice Fiscale
017800071114      *   e/o Partita IVA al cliente.
017810120503      *-=»>?01/08/2008:?                                          <«=-*
017820080801      *-=»> Abolite richieste di Codice Fiscale e/o Partita Iva.  <«=-*
017830080801      *-=»> Tolta ricerca di 'sti 2 dati, lasciando il defualt di <«=-*
017840080801      *-=»> NON richiesta: $No_CF = *off e $No_PI = *off.         <«=-*
017850120503      *-=»>?03/05/2012:?                                          <«=-*
017860120503      *-=»>?Ripristinate richieste di Cod. Fiscale e/o Part. Iva.?<«=-*
017870120503if  1c                   if        DUTlpo <> 'S'
017880120503     c                   exsr      CheckCFPI
017890120503e   1c                   endif
017900070306      *
017910070319      * Verifica se richiesta stampa in A4 per note in FIARN00F
017920070306      *   (da stampare nel box "Note per l'autotrasportatore")
017930070319      * » se stampa in arrivo
017940070319if  1c                   if        DB0tbo = 'A'
017950070306     c                   exsr      CheckFIARN
017960070319e   1c                   endif
017970090302      /free
017980090302
017990090302         // Verifica se richiesta stampa in A4 per note specifiche
018000090302         //   per il cliente mittente (v. tab. "PXA")
018010090302         //   (da stampare nel box "Note per l'autotrasportatore")
018020090302         exsr  CheckTabPXA;
018030090924
018040090924         // Verifica se richiesta stampa in A4 per note specifiche
018050090924         //   per il destinatario (da parte del mittente)
018060090924         //   (da stampare in un box a parte)
018070090924         if  DB0ccm = c_CliSG;
018080090924           $BoxNxD = *on;
018090090924         endif;
018100090302      /end-free
018110070319      *
018120070319      * Verifica se richiesta spedizione già parzialmente consegnata
018130070319      *   (da stampare nel box "Note per l'autotrasportatore")
018140070717      * » se stampa in arrivo e LNA - o capofila £6 - con GEO attivato
018150070717      * completamente
018160070319if  1c                   if             DB0tbo   =  'A'
018170070514     c                             and  wB0dcp   >  *zeros
018180110427     c                             and  $GeoAtt  =  *on
018190140514      * -?Reperimento consegne parziali precedenti da FNART?
018200140514     c                   exsr      SchConsParz
018210140514     c                   if        xx > *zero
018220070413     c                   eval      $ConsParz     =  *on
018230140514     c                   endif
018240070319e   1c                   endif
018250070315      *
018260070315      * Verifica se richiesta stampa in A4 per C.A. aperta
018270070717      * » se stampa in arrivo e LNA - o capofila £6 - con GEO attivato
018280070717      * completamente
018290070319if  1c                   if             DB0tbo   =  'A'
018300110427     c                             and  $GeoAtt  =  *on
018310070316     c                   exsr      CercaCA
018320070316e   1c                   endif
018330030515      *
018340030515      * Tipo modulo (per la separazione dei box: vettore/destinatario)
018350030515      *   (*in69 = formato A5 / not *in69 = formato A4)
018360030515     c                   eval      *in69  = *on
018370030515      *
018380040607      * Tipo LDV (per richiesta firma per ricevuta della merce senza
018390040607      *   riserve)
018400120530      *   (70 = BRT, 71 = DPD, 72 = FedEx, 73 = EuroExpress)
018410080912     c                   movea     '1000'        *in(70)
018420081210      /free
018430081210         // - LDV DPD
018440170605         //if  §OGntw = 'DPD'   -  ?NON della LnP, MA della fil. mitt.:?
018450081210         if  §CMntw = 'DPD';
018460081210           *in71 = *on;
018470081210         endif;
018480081210      /end-free
018490040311      *
018500030515sel 1c                   SELECT
018510030515      *
018520030515      * Rimesso Mittente: va bene il formato A5
018530030515      *   (se non richiesta la forzatura del formato "A4")
018540040707w   1c                   WHEN      DB0ris = 'M'   and  DB0fa4 <> 'S'
018550040707      *
018560040707      * LDV POSTE non valida per la consegna: va bene il formato A5
018570040707      *   (se non richiesta la forzatura del formato "A4")
018580040707w   1c                   WHEN      §OGntw = 'PPT' and  DB0fa4 <> 'S'
018590071213      *
018600081210     *** * LDV DPD
018610081210     *** > (Spostato prima per NON rendere incompatibile DPD con il box
018620081210     *** >  di note aggiuntive "promemoria per l'autotrasportatore")
018630081210w   1***c*** non della LNP  WHEN      §OGntw = 'DPD' - ma della fil. mitt.:
018640081210w   1***c                   WHEN      §CMntw = 'DPD'
018650081210     ***c                   movea     '0100'        *in(70)
018660071213      *
018670071213      * LDV con mittente FedEx
018680150504w   1c                   WHEN      DB0ccm = c_CliFedEx   or
018690150504     c                             DB0ccm = c_CliFedEx2
018700080922     c                   movea     '0010'        *in(70)
018710071213     c                   eval      *in69  = *off
018720040607      *
018730040607      * LDV con DDT del mittente
018740081013      * - IN FUTURO: se al 1° tentativo di consegna
018750081013      *              (DB0FL1 = "D" - v. pgm FNLRS9R)
018760080917w   1c                   WHEN           $FIAR8 =  *on
018770081013     c***-=»>                      and  DB0fl1 <> 'D'
018780040607     c                   eval      *in69  = *off
018790061102      *
018800071114      * Codice Fiscale o Partita IVA da richiedere al cliente
018810071114w   1c                   WHEN           $No_CF = *on
018820071114     c                             or   $No_PI = *on
018830061102     c                   eval      *in69  = *off
018840070306      *
018850070306      * LDV con Note (dal file FIARN00F)
018860070306w   1c                   WHEN      $FIARN = *on
018870070306     c                   eval      *in69  = *off
018880080801      *
018890080801      * LDV già parzialmente consegnata
018900080801w   1c                   WHEN      $ConsParz   =  *on
018910080801     c                   eval      *in69  = *off
018920070315      *
018930070315      * LDV con C.A. aperta per la spedizione (dal file FNDCT00F)
018940070315w   1c                   WHEN      $C_A_  = *on
018950070315     c                   eval      *in69  = *off
018960060524      *
018970070123      * LDV con mittente "PraOil oleodotti italiani"
018980081127w   1c                   WHEN           DB0ccm = c_CliPraOil
018990060524     c                             or  (DB0fbr = 'S'
019000081127     c                             and  §B4Aks = c_CliPraOil)
019010060524     c                   eval      *in69  = *off
019020090302      /free
019030090302
019040090302           // LDV con mittente con promemoria per AUT in tab. "PXA"
019050110411           WHEN  $TabPXA = *on  or  $TabPXA2 = *on;
019060090302             *in69  = *off;
019070090924
019080090924           // LDV con mittente con promemoria per destinatario
019090090924           WHEN  $BoxNxD = *on;
019100090924             *in69  = *off;
019110090302      /end-free
019120030515      *
019130120530      * LDV BRT
019140150304     *** * - con Bancali da rendere al mittente
019150150304     *** *   o   Bancali da riportare a magazzino
019160150304      * - con Destinatario supermercato
019170030515      *   e/o Promemoria per autotrasportatore
019180040223      *   [Se non trovo niente su FIAR5 né x supermercati né x bancali
019190030515      *    ma ho queste condizioni: devo stampare la bolla in "A4" per
019200030515      *    poter stampare comunque il box vuoto con la memo "Destina-
019210050510      *    tario Supermercato" per poter scrivere a mano i promemoria
019220050510      *    per l'autotrasportatore - ci dovrebbero comunque sempre
019230050510      *    essere note per supermercato.]
019240050510      *   e/o con formato A4 previsto in tab. "7R" (partic. consegna)
019250150304w   1c*//                WHEN         %subst(DB0gva:1:1) = 'B'
019260150304     c*//                          or %subst(DB0gva:1:1) = 'O'
019270150304w   1c                   WHEN                DB0tc1      = 'S'
019280030717     c                             or        DB0tc2      = 'S'
019290071015     c                             or        §7Rldv      = 'S'
019300030515     c                   eval      *in69  = *off
019310071015      *
019320071015      * LDV con ritiro porto franco prepagato
019330071015w   1c                   WHEN          (DB0tbo   = '8'
019340071015     c                             and  §B4poe   > *zeros
019350071015     c                             and  §B4nor   > *zeros)
019360071015     c                   eval      *in69  = *off
019370071015      *
019380071015      * LDV con ritiro contestuale alla consegna
019390100608w   1c**!!!              WHEN          (DB0gma   = Ktab7V_RC
019400100608w   1c                   WHEN          (§7Rrc    = 'S'
019410071015     c                             and  §7Rldv   = 'S')
019420071015     c                   eval      *in69  = *off
019430030515      *
019440030515      * - con Forzatura richiesta al formato "A4"
019450030515w   1c                   WHEN      DB0fa4 = 'S'
019460030515     c                   eval      *in69  = *off
019470030515      *
019480030515e   1c                   ENDSL
019490050513      *
019500050513      * Verifica se con Segnacolli NON sequenziali
019510050513      * (di numero superiore a quelli che stanno al posto del range)
019520050513      * DA CONTROLLARE COMUNQUE (A4 o A5 che sia), VISTO CHE:
019530050513      * - se è previsto un A5, potrebbe scattare l'A4 (per numero
019540050513      *   di segnacolli da elencare troppo alto per lo spazio
019550050513      *   disponibile tra le "NOTE E PARTICOLARITA'")
019560050513      * - se è già previsto un A4, i segnacolli non sequenziali
019570050513      *   potrebbero comunque essere elencabili tra le "NOTE E
019580050513      *   PARTICOLARITA'" (e risparmiare una pagina in caso di
019590050513      *   LdV con DDT del mittente)
019600050513     c*** (già fatto:)   reset                   $A4_x_ScNS
019610050513if  1c                   IF             DB0fns = 'S'
019620050513     c                             and  DB0ncl >  2
019630050513      * - - imposto le righe di "Note e Particolarità" da stampare
019640050513     c                   exsr      SchPARTIC
019650050513      * - - controllo quante righe di "Note e Particolarità" servireb-
019660050513      *     bero per segnalare i segnacolli non sequenziali, in base
019670050513      *     al numero di colli (considerando che riuscirei ad elencare
019680050513      *     max 4 segnacolli per ogni riga di "Note e Particolarità")
019690050513     c                   eval      WelSNS = $elSNSxA5
019700050513     c     DB0ncl        div       WelSNS        YY
019710050513     c                   mvr                     XX
019720050513if  2c                   if        XX     >  0
019730050513     c                   eval      XX     =  YY + 1
019740050513x   2c                   else
019750050513     c                   eval      XX     =  YY
019760050513e   2c                   endif
019770050513      * - - conto anche la riga per l'intestazione
019780050513     c                   add       1             XX
019790050513      * - - conteggio le righe di "Note e Particolarità" utilizzate
019800050513      *     per reperire la 1ª riga di "Note e Particolarità" disponi-
019810050513      *     bile
019820050513     c                   eval      *in31  =  *off
019830050513     c                   eval      YY     =  1
019840050513     c     *blanks       lookup    $NP(yy)                                31
019850050513if  2c                   if        *in31  =  *on
019860050513     c                   eval      YY     =  YY - 1
019870050513x   2c                   else
019880050513     c                   eval      YY     =  %elem($NP)
019890050513e   2c                   endif
019900050513      * - - controllo se ho il numero di campi "Note e Particolarità"
019910050513      *     liberi a sufficienza per i segnacolli da elencare.
019920050513      *     Se non ci stanno => serve un modulo A4
019930050513if  2c                   if        XX > %elem($NP) - YY
019940050513     c                   eval      $A4_x_ScNS  = *on
019950050513     c                   eval      *in69  = *off
019960050513e   2c                   endif
019970050513e   1c                   ENDIF
019980030515      *
019990030515      * Imposto il numero di riga massima raggiungibile per l'eventuale
020000030515      *  stampa dei segnacolli non sequenziali
020010040607if  1c                   if        *in69
020020030515     c                   eval      wMaxLine = $MaxLine1
020030040607x   1c                   else
020040030515     c                   eval      wMaxLine = $MaxLine2
020050040607e   1c                   endif
020060030515      * Memo: ho stampato (sto stampando...) almeno un modulo A4 o A5.
020070030515      *   (utile solo per la segnalazione in stampa delle oltre 6.000
020080071008      *   pagine già stampate - in un formato e/o nell'altro).
020090080926      /free
020100080926         select;
020110100924           //?»»» Richiesta la stampa per PDA (4 LdV x A4)?
020120100924           //    ?di LdV per consegna AL 1° tentativo, ma?
020130100924           //    ?reperita packing-list (da stampare "a parte")?
020140100924           //?(la stampa della relativa packing-list verrà richiesta?
020150100924           // ?successivamente, dopo la stampa di tutte le LdV)?
020160100924           when  DB0fa4  = '4'    and
020170100924               //DB0fl1 <> 'D'    and
020180100924                 *in69   = *off   and
020190130306                 $FIAR8  = *on    and
020200130306                 $§PDF   = *off;
020210100924             DB0tms = 'D';
020220130306           //?»»» Richiesta la stampa per PDA (4 LdV x A4)?
020230130306           //    ?di LdV per consegna AL 1° tentativo, ma?
020240130306           //    ?reperita immagine PDF (da stampare "a parte")?
020250130306           //?(la stampa delle relative immagini verranno richieste?
020260130306           // ?successivamente, dopo la stampa di tutte le LdV e?
020270130306           // ?delle relative packing-list)?
020280130306           when  DB0fa4  = '4'    and
020290130306                 $FIAR8  = *off   and
020300130306                 $§PDF   = *on;
020310130307             DB0tms = 'i';
020320130306           //?»»» Richiesta la stampa per PDA (4 LdV x A4)?
020330130306           //    ?di LdV per consegna AL 1° tentativo, ma?
020340130306           //    ?reperita packing-list (da stampare "a parte") e?
020350130306           //    ?immagine PDF (anch'essa da stampare "a parte")?
020360130306           //?(la stampa della relativa packing-list e delle relative?
020370130306           // ?immagini verranno richieste successivamente, dopo la?
020380130306           // ?stampa di tutte le LdV)?
020390130306           when  DB0fa4  = '4'    and
020400130306               //DB0fl1 <> 'D'    and
020410130306                 *in69   = *off   and
020420130306                 $FIAR8  = *on    and
020430130306                 $§PDF   = *on;
020440130306             DB0tms = 'Y';
020450100924           //?»»» Richiesta la stampa per PDA (4 LdV x A4)?
020460080926           when  DB0fa4 = '4';
020470100927             DB0tms = 'Q';
020480100924           //?»»» Richiesta la stampa della packing-list per PDA?
020490081007           when  DB0fa4 = 'P'   and   $FIAR8 = *on;
020500080926             DB0tms = 'P';
020510081007           when  DB0fa4 = 'P'   and   $FIAR8 = *off;
020520081007             //DB0tms = *blank;
020530130307           //?»»» Richiesta la stampa normale e reperita immagine PDF?
020540130307           when  *in69  and  $§PDF;
020550130307             DB0tms = 'I';
020560130307           when  Not *in69  and  $§PDF;
020570130307             DB0tms = 'J';
020580100924           //?»»» Richiesta la stampa normale?
020590080926           when  *in69;
020600080926             DB0tms = '5';
020610080926           other;
020620080926             DB0tms = '4';
020630080926         endsl;
020640080926      /end-free
020650040707      *
020660040707      * Controllo se devo stampare "Non utilizzare per la consegna"
020670040707      * nella copia per il vettore,?senza?copia per il destinatario:
020680040707     c*** (già fatto:)   reset                   $No_x_Cons
020690071203      * - LdV recapito c/assegni:
020700090511      *   stampata in arrivo e con
020710120113if  1c                   if             DB0tbo <> 'P'
020720070306     c                             and  DB0tbo <> '8'
020730090511     c                             and  DB0lnp =  102
020740070306     c                             and  DB0cbo =  'FW'
020750071203     c                             and  DB0nft =  *zeros
020760071203      *   - fil. arrivo con GEO attivato
020770071203     c                             and (DB0fl1    =  'A'
020780071203     c                              or  wGeoCon   =  'S'
020790071203      *   - fil. arrivo abilitata al picking
020800120113     c                              or  A§OGpck   =  'S')
020810070306     c                   eval      $No_x_Cons  =  *on
020820120113e   1c                   endif
020830120113      *//* - LdV Poste
020840120113w   1c*//                   when      §OGntw   = 'PPT'
020850120113     c*//                   eval      $No_x_Cons  =  '2'
020860101125      /free
020870101125         // -?Controlla se "tagliatella" per Ritorno all'Incasso?
020880101129         //$Rit_a_Inc = ( DB0fa4  = '4'    and
020890101129         //               DB0tbo <> 'P'    and  DB0tbo <> '8'   and
020900101129         //               wB0dcm  > *zero  and
020910101129         //           ( ( DB0cas >  *zero  and  wB0icc =  'R' )  or
020920101129         //             ( DB0ift >  *zero  and  wB0ica =  'R' ) ) );
020930101129         // -?Controlla se LdV per Ritorno all'Incasso?
020940101129         $Rit_a_Inc = ( DB0fl4 <> *blank and
020950101129                        DB0tbo <> 'P'    and  DB0tbo <> '8'   and
020960101129                        wB0dcm  > *zero  and
020970101129                    ( ( DB0cas >  *zero  and  wB0icc =  'R' )  or
020980101129                      ( DB0ift >  *zero  and  wB0ica =  'R' ) ) );
020990101125      /end-free
021000030515      *
021010030515     c                   ENDSR
021020040607      *
021030040607      *---------------------------------------------------------------*
021040130315      *?Verifica esistenza dati del DDT allegato da stampare  o      ?*
021050130315      *?               di uno o più PDF allegati da stampare         ?*
021060040607      *---------------------------------------------------------------*
021070130315     c     CercaDDToPDF  BEGSR
021080040607      *
021090060921     c*** (da non fare:) reset                   $E_AR8
021100060921     c*** (già fatto:)   reset                   $FIAR8
021110130315     c*** (già fatto:)   reset                   $§PDF
021120040607     c*** (già fatto:)   reset                   $DDT_o
021130040607     c*** (già fatto:)   reset                   $DDT_p
021140040608     c*** (già fatto:)   reset                   $DDTlastPg
021150040607     c*** (già fatto:)   reset                   $AR8o1
021160040608     c*** (già fatto:)   reset                   wOmaRowT
021170040608     c*** (già fatto:)   reset                   wOmaRowD
021180041207     c*** (già fatto:)   reset                   dAR8
021190130315      *
021200130315      * -?Verifica esistenza PDF allegato?
021210130315     c                   eval      $§PDF  = (§AR5als = 'S')
021220130315      *
021230130315      * -?Verifica esistenza packing-list?
021240130315     c                   eval      $FIAR8 = (§AR5alx = 'S')
021250130315      *
021260130315if  1c                   if        NOT %open(FIAR801L)
021270130315     c                             and ($FIAR8  or  $§PDF)
021280130315     c                   open(e)   FIAR801L
021290130315      * Se errore significa che non c'è il file per cui non faccio
021300130315      *  più questa routine
021310130315if  2c                   if        %error
021320130315     c                   eval      $E_AR8 = *off
021330130315e   2c                   endif
021340130315e   1c                   endif
021350040607      *
021360130306     c                   ENDSR
021370061102      *
021380061102      *---------------------------------------------------------------*
021390071114      *?Verifica se Codice Fiscale e/o Partita Iva da richiedere al  ?*
021400071114      *?  cliente                                                    ?*
021410061102      *---------------------------------------------------------------*
021420061102      *  Il d.l. 223/06, poi convertito con la legge 248/06, ha      *
021430061102      *   reintrodotto l'obbligo di raccogliere il codice fiscale per *
021440061102      *   tutti i clienti e fornitori, siano essi aziende o privati.  *
021450061102      * SE?Porto Franco PrePagato?:                                   *
021460061102      *   il Cod.Fiscale dovrà essere del?Mittente?.                  *
021470061102      * SE?Porto Assegnato con Fattura Immediata?:                    *
021480061102      *   il Cod.Fiscale dovrà essere del?Destinatario?.              *
021490061102      * In sua assenza: dovrà essere richiesto al cliente stesso!     *
021500071114      *  Lo stesso decreto (poi convertito con la legge 248/06)      *
021510071114      *   prevede che a partire dal 01/01/2008 debba essere raccolta  *
021520071114      *   anche l'eventuale partita iva, del cliente o del fornitore. *
021530071114      * Stessi criteri già utilizzati per il codice fiscale.          *
021540061102      *---------------------------------------------------------------*
021550071114     c     CheckCFPI     BEGSR
021560061102      *
021570061106     c*** (già fatto:)   eval      $No_CF  = *off
021580071114     c*** (già fatto:)   eval      $No_PI  = *off
021590061102      *
021600071114      * Verifica se C.F./P.I. da richiedere (NON per clienti esteri)
021610071114      * - con fattura
021620071114if  1c                   IF            (DB0nft  <> *zeros
021630061106     c                              or  DB0ift  <> *zeros)
021640061103     c                             AND (
021650061102      * - - P.F. prepagato
021660061107      *     (con mittente italiano)
021670061107     c                                 (DB0tbo  =  '8'
021680061107     c                              and DB0nzm  =  *blanks)
021690061106      * - - P.A. (con fattura immediata)
021700061107      *     (con destinatario italiano)
021710061107     c                             or  (%subst(DB0tb1:1:1) = 'A'
021720061107     c                              and DB0nzd  =  *blanks)
021730061106      * - - con 2ª bolla
021740061107      *     (con destinatario italiano)
021750061103     c                             or  (DB0tb2  <> *blanks
021760121029     c                              and DB0tbo  <> 'P'
021770061107     c                              and DB0tbo  <> '8'
021780061107     c                              and DB0nzd  =  *blanks)
021790071114      * - => verirficare se da richiedere C.F. e/o P.I.
021800061103     c                                 )
021810120503      *
021820120503sel 2c                   select
021830120503      * · Esiste Partita Iva NON FITTIZIA: non chiedere nulla
021840120503w   2c                   when      wB0piv <> *blanks   and
021850120503     c                             %scan('$$' : wB0piv) = *zero
021860120503     c                   leavesr
021870120503      * · Esiste C.F. lungo 16 (privato) : non chiedere P.I.
021880120503w   2c                   when      %len( %trim( wB0cfi ) ) = 16
021890120503     c                   leavesr
021900120503      * · Esiste C.F. lungo 11 ..........: chiedere P.I. (se mancante)
021910120503w   2c*//                when      %len( %trim( wB0cfi ) ) = 11
021920120503e   2c                   endsl
021930071114      *
021940071114      * - senza codice fiscale
021950120503     c                   eval      $No_CF  = (wB0cfi = *blanks)
021960071114      *
021970071114      * - senza partita iva (se NON privato)
021980120503     c                   eval      $No_PI  = (wB0piv = *blanks)
021990071114      *
022000071114e   1c                   ENDIF
022010061102      *
022020061102     c                   ENDSR
022030070306      *
022040070306      *---------------------------------------------------------------*
022050070306      *?Verifica esistenza note da stampare (dal file FIARN00F)      ?*
022060070306      *---------------------------------------------------------------*
022070070306     c     CheckFIARN    BEGSR
022080070306      *
022090070306     c*** (già fatto:)   reset                   $FIARN
022100070306      *
022110070306      * Controllo esistenza note solo se c'è il file
022120070319     c     K06ARN11      chain     FIARN000
022130070319if  1c                   if             %found(FIARN11L)
022140070306     c                             and  ARNnob <> *blanks
022150070306     c                   eval      $FIARN = *on
022160070306e   1c                   endif
022170070306      *
022180070306     c                   ENDSR
022190070315      *
022200070315      *---------------------------------------------------------------*
022210070315      *?Verifica esistenza C.A. aperta legata alla spedizione        ?*
022220070315      *---------------------------------------------------------------*
022230070316     c     CercaCA       BEGSR
022240070315      *
022250070319     c*** (già fatto:)   reset                   $C_A_
022260070315      *
022270070319      * Richiamo del programma di comodo per recuperare le CA legate
022280070319      *   alla spedizione in esame
022290070319     c                   reset                   FIDN12ds
022300070514if  1c                   if        $Mamma = *off
022310070319     c                   eval      I12aas = DB0aas
022320070319     c                   eval      I12lnp = DB0lnp
022330070319     c                   eval      I12nrs = DB0nrs
022340070319     c                   eval      I12nsp = DB0nsp
022350070514x   1c                   else
022360070514     c                   eval      I12aas = LBLaap
022370070514     c                   eval      I12lnp = LBLlpp
022380070514     c                   eval      I12nrs = LBLnrp
022390070514     c                   eval      I12nsp = LBLnsp
022400070514e   1c                   endif
022410070319     c*** già così:      eval      I12fel = 'E'
022420070319     c***  "    "        eval      I12fan = 'E'
022430070319     c***  "    "        eval      I12fch = 'E'
022440070319      *
022450070319     c                   call      'FIDN12R'
022460070319     c                   parm                    FIDN12ds
022470070319      *
022480070319      * Se trovata almeno una CA
022490070319     c                   eval      $C_A_  = (O12nca > *zeros)
022500070315      *
022510070316     c                   ENDSR
022520070319      *
022530070319      *---------------------------------------------------------------*
022540070319      *?Verifica se stampa spedizione DOPO "Lasciato Avviso"         ?*
022550070319      *---------------------------------------------------------------*
022560070319     c     CheckLascAvv  BEGSR
022570070319      *
022580070319     c*** (già fatto:)   reset                   $LascAvv
022590070319      *
022600070319if  1c                   if        NOT  %open(FNEVB05L)
022610070319     c                   open      FNEVB05L
022620070319e   1c                   endif
022630070319      *
022640070319     c     KLDV          setgt     FNEVB000
022650070319     c     KLDV          readpe    FNEVB000
022660070319      *
022670070319      * Proseguo solo se ritrovo un evento
022680070319do  1c                   DOW       NOT  %eof(FNEVB05L)
022690070319      *
022700070319      * Verifico se evento di tipo Giacenza
022710070319     c                   eval      *in31    = *off
022720070319     c     EVBcev        lookup    $2Acev_G                               31
022730070319      **
022740070319sel 2c                   Select
022750070319      * - Evento GIACENZA --> NO LASCIATO AVVISO
022760070319w   2c                   When      *in31
022770070319     c                   leave
022780070319      * - Evento RIC --> 1° LASCIATO AVVISO
022790070319w   2c                   When      EVBcev   = 'RIC'
022800070319     c                   eval      $LascAvv = *on
022810140307      * - - salvataggio data/ora
022820140307     c                   eval      wEVBdev  = EVBdev
022830140307     c                   eval      wEVBhev  = EVBhev
022840070319     c                   leave
022850070319      * - Evento DA NON ELABORARE
022860070319x   2c                   Other
022870070319     c     KLDV          readpe    FNEVB000
022880070319e   2c                   Endsl
022890070319      **
022900070319e   1c                   ENDDO
022910070319      *
022920070319     c                   ENDSR
022930090302      /free
022940090302
022950090302       //--------------------------------------------------------------
022960090302       //?Verifica esistenza note da stampare (dal file TNTBE00F)      ?
022970090302       //--------------------------------------------------------------
022980090302       BEGSR  CheckTabPXA;
022990090302
023000090302         //clear  $TabPXA;          (già fatto)
023010110411         //clear  $TabPXA2;         (già fatto)
023020110411
023030110411         // -?Verifica esistenza "PXA" per particolarità consegna?
023040110411         If  DB0gma <> *blank;
023050170605           clear  k05tntbe01;
023060110411           k_TBEcod = 'PXA';
023070110411           k_TBEke1 = DB0gma;
023080110411           k_TBEke2 = '001            ';
023090110411           setll  %kds(k05tntbe01 : 3)  TNTBE000;
023100110411           // -?Bisogna testare se nota ANNULLATA?
023110110411           //$TabPXA  = %equal(TNTBE01L);
023120110411           reade  %kds(k05tntbe01 : 2)  TNTBE000;
023130110411           $TabPXA2 = (Not %eof(TNTBE01L)   and   TBEatb = *blank
023140110411                       and TBEke2 <= '006            ');
023150110411         EndIf;
023160090302
023170110411         // -?Reperimento eventuali "PXA" per mittente?
023180170605         clear  k05tntbe01;
023190090302         k_TBEcod = 'PXA';
023200090302         k_TBEke1 = %editc(DB0ccm : 'X');
023210100316         k_TBEke2 = '001            ';
023220090302         clear  k_TBElin;
023230090302         clear  k_TBEsif;
023240100316         setll  %kds(k05tntbe01 : 3)  TNTBE000;
023250110411         // -?Bisogna testare se nota ANNULLATA?
023260090302         //$TabPXA  = %equal(TNTBE01L);
023270090302         reade  %kds(k05tntbe01 : 2)  TNTBE000;
023280100316         $TabPXA  = (Not %eof(TNTBE01L)   and   TBEatb = *blank
023290100316                     and TBEke2 <= '006            ');
023300090302
023310090302       ENDSR;
023320090302      /end-free
023330030515      *
023340030515      *---------------------------------------------------------------*
023350050217      *?Imposta i campi di stampa con quelli della DS                ?*
023360030515      *---------------------------------------------------------------*
023370030515     c     PrepPRT       BEGSR
023380040223      *
023390040223      * Reperimento dati da FIAR5 / "GEN"
023400130315     c*** (già fatto:)   exsr      RepAR5gen
023410070212     *** *
023420070212     *** * Impostazione Data spedizione
023430070212     *** * (già fatto nella subr. "TipoLdV")
023440070319      *
023450100218      * CONTROLLO SE STAMPA SPEDIZIONE DOPO "LASCIATO AVVISO"
023460100218      * (?*-=»>   SE NON MESSA IN "FERMO DEPOSITO"   <«=-?):
023470081118      * (NON se richiesta stampa di "ricevuta" - 4 in A4)
023480110427      * per LNA (o capofila se LNA è in £6) con $GeoAtt = *ON =>
023490070717      * GEO attivato completamente
023500070319      * controllo se la spedizione ha già più tentativi di consegna e
023510070319      *   determino se in stato di "lasciato avviso"
023520070319      *
023530070319     c*** (già fatto:)   reset                   $LascAvv
023540070406if  1c                   if             DB0tbo    =  'A'
023550081117     c                             and  DB0fa4    <> '4'
023560070514     c                             and  wB0ntc    >  *zeros
023570110427     c                             and  $GeoAtt   =  *on
023580100218     c                             and  DB0ffd    =  *blank
023590070319     c                   exsr      CheckLascAvv
023600070319e   1c                   endif
023610030515      *
023620120530      * Impotazione dati BRT S.p.A.
023630120530     c*//                if        (DB0dft > *zero       and
023640120530     c*//                           DB0dft > §vpoBRTdta)  OR
023650120530     c*//                          (DB0dft = *zero       and
023660120530     c*//                           Wdsp   > §vpoBRTdta)
023670120530     c                   eval      TBAR   =  $BRT
023680120530     c*//                else
023690120530     c*//                eval      TBAR   =  $Bartolini
023700120530     c*//                endif
023710070307     c                   eval      TBA1   =  $Intest1
023720070307     c                   eval      TBA2   =  $Intest2
023730080707if  1c                   if            DB0nft <> *zeros
023740080707     c                             or  DB0ift <> *zeros
023750080627     c                   eval      TBA3   =  $Intest3
023760080707e   1c                   endif
023770070307     c                   eval      TBA4   =  $Intest4
023780070307     c                   eval      TBA5   =  $Intest5
023790070307     c*** NON sopra:     eval      TBA6   =  $Intest6
023800120530     c*//                eval      TBA7   =  $Intest7
023810120530     c                   eval      TBA7   =  $Intest7_BRT
023820030515      *
023830030515      * - Intestazione moduli
023840080919     c                   eval      Tcxd   =  c_CopiaXdest
023850130729     c*//                if        Not *in36     (NON ancora acceso)
023860130729if  1c                   if        DB0tbo <> '8'  or  DB0ris = 'M'
023870130729     c                   eval      Tcxd2  =  Tcxd
023880130729     c                   endif
023890091008      *
023900091008      /free
023910091008         select;
023920091008           // - Indicazione Copia LDV x cliente
023930091008           when  DB0ris = 'C';
023940091008             Pkco = 'C O P I A';
023950091008           // - Altre indicazioni: R=Ristampa, V=Variazione, ...
023960091008           other;
023970091008             Tkri = DB0ris;
023980091008         endsl;
023990091008      /end-free
024000030515      *
024010030515      * - Codice a barre: ANNO+LNP+NRS+NSP+CKD
024020030515      *   (chiamo il pgm TRUL28R per ricavarne il check digit)
024030030515     c                   reset                   TRUL28ds
024040030515     c                   z-add     DB0aas        BARcdeAAS
024050130710     c*//                eval      BARcdeLNP = DB0lnp
024060130710     c                   eval      BARcdeLNP = Prm_LNP
024070130710     c*//                eval      BARcdeNRS = DB0nrs
024080130710     c                   eval      BARcdeNRS = Prm_NRS
024090030515     c                   eval      BARcdeNSP = DB0nsp
024100030515     c                   movel     BarCodeDS     I28cod
024110030515     c                   call      'TRUL28R1'
024120030515     c                   parm                    TRUL28DS
024130030515if  2c                   if        O28err =  *blanks
024140030515     c                   movel     O28cod        Tbcd
024150030515e   2c                   endif
024160140917      *
024170140917      /free
024180140917
024190140917         // -?BRTcode?
024200140917         reset  TRUL28ds0;
024210140917         trul28ds0.i280cod = BarCodeDS;
024220140917         trul28r0 ( TRUL28ds0 );
024230140917         if  trul28ds0.o280err <> *on;
024240140917           //TbrtC = trul28ds0.o280cod;
024250140917           TbrtC = %subst( trul28ds0.o280cod :  1 : 5 ) + ' ' +
024260140917                   %subst( trul28ds0.o280cod :  6 : 5 ) + ' ' +
024270140917                   %subst( trul28ds0.o280cod : 11 : 5 ) + ' ' +
024280140917                   %subst( trul28ds0.o280cod : 16 : 4 );
024290140917         endif;
024300140917
024310140917      /end-free
024320030515      *
024330030515      * - Sigla operatore video
024340030515     c                   eval      Psop   =  DB0sop
024350080721      /free
024360080721
024370130306         // -?Flag "allega documento di trasporto"?
024380130418         //   IN FUTURO: se al 1° tentativo di consegna
024390130418         //              (DB0FL1 = "D" - v. pgm FNLRS9R)
024400080721         select;
024410081119
024420130306           // -?LdV con DDT allegato oltre il 1° tentativo di consegna?
024430081127           when  DB0bam =  'S'   and   wB0tmc <> *blank;
024440081127             evalr Pddt =  'DDT sul collo';
024450081119
024460130306           // -?LdV con DDT allegato (al 1° tentativo di consegna)?
024470080723           when  DB0bam =  'S';
024480080723             evalr Pddt =  'DDT';
024490081119
024500130306           // -?Esiste packing-list abbinata?
024510130306           when  $FIAR8 =  *on   and   $§PDF =  *off;
024520081013           //if  DB0fl1 <> 'D';
024530081013               evalr Pddt =  'PACKING-LIST';
024540081013           //else;
024550081013           //  evalr Pddt =  'PACKING-LIST GIA'' CONSEGNATA';
024560081013           //endif;
024570130306
024580130306           // -?LdV con PDF allegato?
024590130306           when  $FIAR8 =  *off  and   $§PDF =  *on;
024600130306             evalr Pddt =  'PACKING-LIST PDF';
024610130306
024620130306           // -?LdV con packing-list abbinata & PDF allegato?
024630130306           when  $FIAR8 =  *on   and   $§PDF =  *on;
024640130308             evalr Pddt =  'PACKING-LIST PDF';
024650081119
024660130306           // -?LdV senza PDF allegato & senza packing-list abbinata?
024670080721           other;
024680080721             clear Pddt;
024690081119
024700080721         endsl;
024710080721
024720080721      /end-free
024730030515      *
024740030515      * - Mittente
024750170605     c                   if        *in71
024760170605     c                   exsr      sr_Mitt_DPD
024770170605     c                   endif
024780170605      *
024790030515     c                   if        DB0ccm <> *zeros
024800030515     c                   move      DB0ccm        Pccm
024810030515     c                   endif
024820030515     c                   eval      Prsm   =  DB0rsm
024830030515     c                   eval      Pinm   =  DB0inm
024840030515     c                   eval      Pcam   =  DB0cam
024850030515     c                   eval      Plom   =  DB0lom
024860030515     c                   eval      Pprm   =  DB0prm
024870030515     c                   eval      Pnzm   =  DB0nzm
024880030515      *   Mittente originale
024890030515if  1c                   if        DB0rmo <> *blanks
024900030515     c                   eval      Pkmo   =  'MITT. ORIGINALE'
024910030515     c                   eval      Prmo   =  DB0rmo
024920030515e   1c                   endif
024930030515      *
024940070308      * - Filiale di Arrivo
024950030515if  1c                   if        DB0ris <> 'M'
024960080721     c                   eval      Pkla   =  'FILIALE'
024970170717     c                   eval      Plna1  =  %editc(wDB0lnaCf£6 : 'X')
024980080722     c                                    +  ' '
024990170717     c                                    +  wDB0dlaCf£6
025000080722     c                                    +  ' - '
025010170717     c                                    +  wDB0inlCf£6
025020140307     c*//                                 +  ' Tel.'
025030140307     c                                    +  '     '
025040170717     c                                    +  %trimr(wDB0telCf£6)
025050170717     c                   eval      Plna2  =  %editc(wDB0calCf£6 : 'X')
025060080722     c                                    +  ' '
025070170717     c                                    +  wDB0lolCf£6
025080080722     c                                    +  ' '
025090170717     c                                    +  wDB0prlCf£6
025100140307     c*//                                 +  '                  '
025110140307     c*//                                 +  ' Fax '
025120170717     c*//                                 +  %trimr(wDB0faxCf£6)
025130170717     c                   if        wDB0faxCf£6 <> *blanks
025140170717     c     c_Mai:c_Min   xlate     wDB0faxCf£6   DB0fax
025150140307     c                   eval      Plna2  =  %trimr(Plna2)
025160140307     c                                    +  '          '
025170140307     c*//*                                +  ' E-mail: '
025180140307     c                                    +  '         '
025190170717     c                                    +  %trimr(wDB0faxCf£6) + '@brt.it'
025200140307     c                   endif
025210030515e   1c                   endif
025220030515      *
025230030515      * - Destinatario
025240030515if  1c                   if        DB0ccd <> *zeros
025250030515     c                   move      DB0ccd        Pccd
025260030515e   1c                   endif
025270030515     c                   eval      Prsd   =  DB0rsd
025280030515     c                   eval      Prse   =  DB0rd2
025290030515     c                   eval      Pind   =  DB0ind
025300030515     c                   eval      Pcad   =  DB0cad
025310030515     c                   eval      Plod   =  DB0lod
025320030515     c                   eval      Pprd   =  DB0prd
025330030515     c                   eval      Pnzd   =  DB0nzd
025340030515      *
025350030515      * - Riferimenti mittente
025360030515     c                   eval      Prmn   =  DB0rmn
025370030515     c                   eval      Prma   =  DB0rma
025380061107      * se bolla DPD recupero il riferimento da FIAR4 rcd "I"
025390061107     c*** (già fatto:)   clear                   dsBL4I
025400060626     c                   if        *in71
025410061103     c                   exsr      RepAR4_I
025420060626     c                   endif
025430030515      *
025440030515      * - Nominativo & telefono
025450031124      *   (da FIAR5 / "GEN")
025460040607     c*** (già fatto:)   exsr      RepAR5gen
025470031124     c                   eval      Pnom   =  §AR5ref
025480031124     c                   eval      Ptel   =  §AR5teld
025490030515      *
025500030515      * - Spedizione
025510030515      *   (numero: non se Rimesso al Mittente)
025520030515if  1c                   if        DB0ris <> 'M'
025530030515     c                   eval      Plnp   =  DB0lnp
025540030515     c                   eval      Pnrs   =  DB0nrs
025550030515     c                   eval      Pnsp   =  DB0nsp
025560030515e   1c                   endif
025570030515      * . . Data spedizione
025580031124     c     *iso          movel     Wdsp          Data_EUR
025590130418     c     *eur          movel     Data_EUR      Pdsp
025600140923     c     *dmy          movel     Data_EUR      Pdsp6
025610030515      *
025620030515      * - Colli: numero, peso, volume, flag tipo-volume
025630030515     c                   eval      Pncl   =  DB0ncl
025640030515     c                   eval      Ppkf   =  DB0pkf
025650030515     c                   eval      Pvlf   =  DB0vlf
025660030515if  1c                   if            DB0fvf <> *blanks
025670030515     c                             and DB0ris <> 'M'
025680030717     ***c***                eval      Pfvf   =  '-' + DB0fvf + '-'
025690030717     c                   move      DB0fvf        Pfvf
025700030515e   1c                   endif
025710030515      *
025720030515      * - Natura merce
025730030515     c                   eval      Pnat   =  DB0nas
025740030515      *
025750030515      * - Servizio
025760100513      /free
025770100513         if  %subst(DB0tb1:1:1) = 'A'   or   DB0tb2 <> *blanks;
025780100513           exsr  sr_CheckORM_PrevvMail;
025790100513         endif;
025800100513      /end-free
025810040607      * . . Descrizione LDV
025820100513     c                   if        Not  $PreavvORM
025830030515     c                   eval      Pds1   =  DB0dcb
025840100513     c                   endif
025850030515      * . . Descrizione tipo servizio
025860030515if  1c                   if        DB0fst =  'S'
025870030515     c                   eval      Pds2   =  DB0dts
025880030515e   1c                   endif
025890030515      * . . Reso / Franco frontiera
025900030515sel 1c                   select
025910030515w   1c                   when      DB0fbr =  'S'
025920030515     c                   clear                   w008a
025930030515     c                   eval      w008a  =  '   RESO '
025940030515     c                   move      w008a         Pds2
025950030515w   1c                   when      DB0fbr =  'F'
025960030515     c                   clear                   w015a
025970030515     c                   eval      w015a  =  '   FR.FRONTIERA'
025980030515     c                   move      w015a         Pds2
025990030515e   1c                   endsl
026000030515      *
026010030515      * - Zona consegna
026020030515     c                   eval      Pznc   =  DB0znc
026030030515      *
026040030515      * - Flag "*" (gestito a parte)
026050040223      *
026060040223      * - Flag "V" = colli di valore
026070040223if  1c                   if        §AR5bva   <> *blank
026080040223     c                   eval      Pbva   =  'V'
026090040223e   1c                   endif
026100060921      *
026110070212      * - Codice Giro o Numero Distinta (se tipo bolla "ARRIVO")
026120071015if  1c                   IF              DB0tbo = 'A'
026130071015     c                             and  wGeoCon = 'S'
026140070212sel 2c                   Select
026150070212      * · - Stampa LdV in arrivo con codice giro (parametro passato)
026160071015w   2c                   When      DB0fl1     =  'A'
026170070215if  3c                   if        §DB0tgi    =  'M'
026180101028     c                   eval      Pndc   =  'g:' + §DB0cgi + 'm'
026190070215x   3c                   else
026200101028     c                   eval      Pndc   =  'g:' + §DB0cgi
026210070215e   3c                   endif
026220070212      * · - Stampa LdV con numero distinta consegna
026230071015w   2c                   When           wB0ndc  > *zeros
026240110427     c                             and  $GeoAtt =  *on
026250101028     c                   eval      Pndc   =  'd:'
026260070514     c                                    +  %trim(%editc(wB0ndc:'Z'))
026270101028if  3c                   if        %len(Pndc)  >= %len(%trimr(Pndc))
026280071016     c                                          + 3 + %len(Wpdc4a)
026290101028     c                   eval      Pndc   =  %trimr(Pndc)
026300071022     c                                    +  ' a:'
026310071022     c                                    +  Wpdc4a
026320071016e   3c                   endif
026330070212      * · - Stampa LdV con codice giro (da reperire in FIARG00F)
026340070213w   2c                   Other
026350070212if  3c                   if        NOT %open(FIARG01L)
026360070212     c                   open      FIARG01L
026370070212e   3c                   endif
026380070215     c                   clear                   ARGcgi
026390070215     c                   clear                   ARGtgi
026400070212     c     KLDV          chain     FIARG000
026410070215if  3c                   if        ARGtgi =  'M'
026420101028     c                   eval      Pndc   =  'g:' + ARGcgi + 'm'
026430070215x   3c                   else
026440101028     c                   eval      Pndc   =  'g:' + ARGcgi
026450070215e   3c                   endif
026460070215e   2c                   EndSl
026470070212e   1c                   ENDIF
026480030515      *
026490030515      * - Fermo deposito
026500030515if  1c                   if        DB0ffd <> *blanks
026510060921     c                   eval      Pffd   =  'FERMO DEPOSITO '
026520030515e   1c                   endif
026530030515      *
026540070308      * - Segnacolli: filiale, serie, dal / al
026550030515if  1c                   if        DB0fls <> *zeros
026560030515     c                   move      DB0fls        Ppos
026570030515e   1c                   endif
026580040607      * - - se bolla con mamma considero la serie della mamma
026590070515if  1c                   if        $Mamma =  *on
026600031015     c                   eval      Psrs   =  wB0nrs
026610031015x   1c                   else
026620030515     c                   eval      Psrs   =  DB0nrs
026630031015e   1c                   endif
026640030515      * - Se colli non sequenziali: stampo "NON SEQUENZIALI" + Elenco
026650030515      *   Se collo cumulativo ....:   "     SCD "X" Num.Colli
026660030515      *   Se colli sequenziali ...:   "     SCD "/" SCA
026670030515      *   Se collo singolo .......:   "     SCD
026680031015sel 1c                   select
026690030515      * - - non sequenziali
026700030515w   1c                   when      DB0fns =  'S'
026710050513if  2c                   if           *in69      = *on
026720050513     c                             or $A4_x_ScNS = *off
026730030715      *     caricamento delle "Note e Particolarità" per consentirne
026740030715      *     l'aggiunta come "Elenco segnacolli non sequenziali"
026750031015if  3c                   if        $Partic = *off
026760030515     c                   exsr      SchPARTIC
026770031015e   3c                   endif
026780030515     c                   exsr      ListaScNS
026790050511x   2c                   else
026800050511if  3c                   if        DB0ncl <= 2
026810050511     c                   exsr      ListaScNSd
026820081007x   3c                   else
026830081007     c                   eval      Pnsx   =  'NON SEQUENZIALI'
026840050511e   3c                   endif
026850030515e   2c                   endif
026860030515      * - - cumulativo
026870050114w   1c**!!!              when      DB0xco =  'X'
026880081010     c**!!!              eval      Pnsx   =  %editc(DB0ncd : 'Z')
026890081010     c**!!!                               +  'X'
026900081010     c**!!!                               +  %editc(DB0ncl : 'Z')
026910030515      * - - singolo o sequenziali
026920030515      *     (non se rimesso al mittente)
026930030515w   1c                   when      DB0ris <> 'M'
026940080922     c                   eval      Pnsx   =  %editc(DB0ncd : 'Z')
026950030515if  2c                   if            DB0nca <> *zeros
026960030515     c                             and DB0nca <> DB0ncd
026970080922     c                   eval      Pnsx   =  %trimr(Pnsx) + '-'
026980080922     c                                    +  %editc(DB0nca : 'Z')
026990030515e   2c                   endif
027000030515e   1c                   endsl
027010030515      *
027020030716      * - Note (1 e 2) e Particolarità (0, 3, 4, 5 e 6)
027030030710      *   con eventuali segnacolli non sequenziali (max 4 x riga)
027040030515if  1c                   if        $Partic = *off
027050030515     c                   exsr      SchPARTIC
027060030515e   1c                   endif
027070030716if  1c                   if           $NP(1) <> *blanks
027080030716     c                             or $NP(2) <> *blanks
027090030716     c                             or $NP(3) <> *blanks
027100030716     c                             or $NP(4) <> *blanks
027110030716     c                             or $NP(5) <> *blanks
027120030716     c                             or $NP(6) <> *blanks
027130030716     c                   eval      Pknp   =  'NOTE  E  PARTICOLARITÀ'
027140030710     c                   eval      Pnp1   =  $NP(1)
027150030710     c                   eval      Pnp2   =  $NP(2)
027160030710     c                   eval      Pnp3   =  $NP(3)
027170030710     c                   eval      Pnp4   =  $NP(4)
027180030710     c                   eval      Pnp5   =  $NP(5)
027190030710     c                   eval      Pnp6   =  $NP(6)
027200030716e   1c                   endif
027210030515      *
027220080401      * - Privato / Appuntamento / Uffici / Consegna Garantita
027230080401      *   (servizio consegna garantita x cliente Wuerth)
027240030515if  1c                   if        DB0gva =  'P '
027250080723     c                   evalr     Ppad = 'PRIVATO'
027260030515e   1c                   endif
027270041012if  1c                   if        DB0gva =  'U '
027280080723     c                   evalr     Ppad = 'CONSEGNA AD UFFICI'
027290041012e   1c                   endif
027300081010if  1c*** NO !!!         if           DB0tc1 =  'A'
027310081010     c*** NO !!!                   or DB0tc2 =  'A'
027320081010     c*** NO !!!         move      'APPUNTAMENTO'Ppad
027330081010e   1c*** NO !!!         endif
027340080401if  1c                   if             DB0nrs = 40
027350090518     c                             and  DB0tsp = 'E'
027360080401     c                             and (DB0lnp = 043
027370110429     c                              or  DB0lnp = 277
027380080401     c                              or  DB0lnp = 063)
027390090217     c*****              evalr     Ppad = 'CONSEGNA GARANTITA'
027400090227     c                   eval      Pds2 = 'CONSEGNA ESPRESSA +
027410090227     c                                     WÜRTH'
027420080401e   1c                   endif
027430030515      *
027440030515      * - Data e ora consegna richiesta
027450030515      *   e Flag "*" > x Data e/o Ora consegna richiesta
027460091116      *   (NO "Prima del" per il cliente TTL)
027470091116if  0c                   if        DB0ccm <> c_CliTTL  or
027480091116     c                             DB0dcr =  *zeros    or  DB0tcr <> 'P'
027490030515if  1c                   if        DB0dcr <> *zeros
027500030515     c                   eval      Past   =  '*'
027510030515     c     *iso          movel     DB0dcr        Data_EUR
027520030515     c     *eur          movel     Data_EUR      Pdcr
027530030515e   1c                   endif
027540030515if  1c                   if        DB0hcr <> *zeros
027550030515     c                   eval      Past   =  '*'
027560030515     c                   eval      *in18  =  *on
027570030515     c                   eval      Phcr   =  DB0hcr
027580030515e   1c                   endif
027590091116e   0c                   endif
027600030515      *
027610030515      * - Consegne particolari
027620030515     c                   eval      Pcp1   =  DB0dc1
027630030515     c                   eval      Pcp2   =  DB0dc2
027640030515     *** *
027650030515     *** * - Valore merce dichiarato e relativa divisa => NO
027660030515     ***c***                eval      Pvmd   =  DB0vmd
027670030515     ***c***                eval      Pvad   =  DB0vad
027680030515     *** *
027690030515     *** * - Importo da assicurare e relativa divisa   => NO
027700030515     ***c***                eval      Pvas   =  DB0vas
027710030515     ***c***                eval      Pias   =  DB0ias
027720030515     *** *
027730030515     *** * - Quantità da fatturare                     => NO
027740030515     ***c***                eval      Pqft   =  DB0qft
027750030515      *
027760030515      * - Imponibile / IVA / Esenzione / Totale fattura e divisa
027770030515      *   (importi "arrotondati" a 2 decimali in stampa)
027780030515if  1c                   if        DB0ift <> *zeros
027790030515     c                   eval      Pkim   = 'IMPONIBILE'
027800130218     c*//                   eval(h)   Pimp   =  DB0imv
027810130218     c                   eval(h)   w0112  =  DB0imv
027820130218     c                   evalr     Pimp   =  %editc(w0112 : 'K')
027830030515sel 2c                   select
027840030515w   2c                   when      DB0iva <> *zeros
027850030515     c                   eval      Pkie   = 'I.V.A.'
027860030624     c                   eval(h)   w0112  =  DB0iva
027870130218     c*//                   eval      Piva   =  '     ' +
027880130218     c*//                                       %editc(w0112 : 'K')
027890130218     c                   evalr     Piva   =  %editc(w0112 : 'K')
027900030515w   2c                   when         DB0cei <> *blanks
027910030515     c                             or DB0dce <> *blanks
027920030912     c                   eval      Pkie   = 'ESENZIONE'
027930030912     c                   eval      Piva   =  DB0dce
027940030515e   2c                   endsl
027950030515     c                   eval      Pktf   = 'TOTALE FATTURA'
027960130218     c*//                   eval(h)   Pift   =  DB0ift
027970130218     c                   eval(h)   w0112  =  DB0ift
027980130218     c                   evalr     Pift   =  %editc(w0112 : 'K')
027990030515if  2c                   if        DB0div <> §GEDcn
028000030515     c                   eval      Pdiv   =  DB0div
028010030515e   2c                   endif
028020030515e   1c                   endif
028030030515      *
028040030515      * - Contrassegno e relativa divisa
028050030515if  1c                   IF        DB0cas <> *zeros
028060030625      *
028070030515     c                   eval(h)   Pcas   =  DB0cas
028080030515if  2c                   if        DB0vca <> §GEDcn
028090030515     c                   eval      Pdca   =  DB0vca
028100030515e   2c                   endif
028110030515     c                   exsr      CtrCAs
028120030624      * - - Nuovo controllo abilitazione c/assegno
028130030624     c                   reset                   TRUL21DS
028140030624     c                   eval      I21cbo =  DB0cbo
028150030624     c                   eval      I21tsp =  DB0tsp
028160030624     c                   eval      I21lnp =  DB0lnp
028170030624     c                   eval      I21nzm =  DB0nzm
028180030624     c                   eval      I21lna =  DB0lna
028190030624     c                   eval      I21nzd =  DB0nzd
028200130826     c                   if        DB0ccm >  *zero
028210130826     c                   eval      I21ksc =  DB0ccm
028220130826     c                   else
028230130826     c                   eval      I21ksc =  (DB0lnp * 10000 ) + 8888
028240130826     c                   endif
028250030624     c                   eval      I21tic =  DB0tic
028260030624if  2c                   if        DB0vca <> §GEDcn
028270030624     c                   eval      I21imp =  Wcas
028280030624     c                   eval      I21div =  §GEDcn
028290030624x   2c                   else
028300030624     c                   eval      I21imp =  DB0cas
028310030624     c                   eval      I21div =  DB0vca
028320030624e   2c                   endif
028330030624     c                   eval      I21ute =  KNMUS
028340040607     c                   eval      I21pgm =  SDSpgm
028350030624     c                   call      'TRUL21R'
028360030624     c                   parm                    TRUL21ds
028370030624      * - - Carico la tabella "Tipo incasso C/Assegno" (1A)
028380030624     c                   exsr      Tab_1A
028390030624     c                   movel     §1Ades        DB0dti
028400030624if  2c                   if            §1Adeo <> *blanks
028410030624     c                             and O21fca <> *blanks
028420030624     c                   movel     §1Adeo        DB0dti
028430030624e   2c                   endif
028440100208      /free
028450100208           // - -?Reperimento intestatario c/assegno?
028460100322           if  §VPOcas <= wDate   and   DUTlpo  <> 'S';
028470100208             reset  trulINTds;
028480100208             iINTtic  = DB0tic;
028490100208             iINTrscM = DB0rsm;
028500100208             iINTrscO = DB0rmo;
028510100208             iINTcdi  = §AR5cdi;
028520100208             //if  §AR5cdi = *blank;
028530100208             //  iINTaas = DB0aas;
028540100208             //  iINTlnp = DB0lnp;
028550100208             //  iINTnrs = DB0nrs;
028560100208             //  iINTnsp = DB0nsp;
028570100208             //endif;
028580100208             trulINTr ( TRULINTds );
028590100322           endif;
028600100208      /end-free
028610030624      * - - Imposto i campi di stampa
028620030624     c                   eval      Ptic   =  DB0tic + ' ' + DB0dti
028630091119      /free
028640100208           // - - -?Intestatario assegno?
028650100322           if  DB0cas  <> *zero   and
028660100322              (§VPOcas <= wDate   and   DUTlpo  <> 'S')   and
028670100208               oINTerr = *blank;
028680100208             Pkic = 'INTESTARE A:';
028690100208             Pica = oINTrsci;
028700100208           endif;
028710091119      /end-free
028720030625      *
028730030515e   1c                   ENDIF
028740030515      *
028750030515      * - Totale da esigere al ritiro o alla consegna e relativa divisa
028760030515if  1c                   IF        DB0ift >  *zeros
028770030625      *
028780030515if  2c                   if            DB0vca <> §GEDcn
028790030515     c                             and DB0vca <> *blanks
028800030515     c                   eval      Pdie   =  DB0div
028810030515e   2c                   endif
028820030515      *   Controllo se C/Assegno da sommare al totale da esigere o no:
028830030515      *   se Franco-Prepagato il C/Assegno non si somma mai;
028840030515      *   se c'è il totale fattura: metto anche il il totale da esigere.
028850030515      *   (importi "arrotondati" a 2 decimali in stampa)
028860030515sel 2c                   Select
028870030515      *   Non ho C/Assegno o è Franco Prepagato
028880071008w   2c                   When          DB0cas  = *zeros
028890071008     c                             or  DB0tbo  = '8'
028900080701     c                   eval(h)   Pies   =  DB0ift
028910030625     c                   clear                   Pie1
028920030625     c                   clear                   Pdi1
028930030515      *   C/Assegno sommabile
028940030515w   2c                   When      DB0ste =  'S'    and $CAeur = *on
028950030515sel 3c                   select
028960030515      *   - c/assegno in EURO
028970030515w   3c                   when      DB0div =  §GEDcn and DB0vca =  §GEDcn
028980080701     c                   eval(h)   Pies   =  DB0ift + DB0cas
028990030624     c                   clear                   Pie1
029000030624     c                   clear                   Pdi1
029010030515      *   - c/assegno in DIVISA
029020030515w   3c                   when          DB0div =  §GEDcn
029030030515     c                             and DB0vca <> §GEDcn
029040080701     c                   eval(h)   Pies   =  DB0ift + Wcas
029050030624     c                   clear                   Pie1
029060030624     c                   clear                   Pdi1
029070030515e   3c                   endsl
029080030515      *   C/Assegno non sommabile
029090030515x   2c                   Other
029100080701     c                   eval(h)   Pie1   =  DB0cas
029110080701     c                   eval(h)   Pies   =  DB0ift
029120030515e   2c                   Endsl
029130030709      *
029140030709e   1c                   ENDIF
029150030515      *
029160030515      * - Numero e data fattura
029170030515if  1c                   if        DB0nft <> *zeros
029180030515     c                   eval      Pkft   =  'FATTURA   '
029190030515     c                   eval      Pnft   =  DB0nft
029200130124     c                   eval      Pfiv   = '(' + %editc(wB0fiv:'X') +
029210130124     c                                      ')'
029220030515if  2c                   if        DB0dft >  *zeros
029230030515     c                   eval      Pkdf   =  'DEL'
029240030515     c     *iso          movel     DB0dft        Data_EUR
029250130418     c     *dmy          movel     Data_EUR      Pdft
029260130418     c*//  *eur          movel     Data_EUR      Pdft
029270030515e   2c                   endif
029280030515e   1c                   endif
029290030515      *
029300030515      * Memorizzo il codice tariffa come numero di copie da stampare
029310030515      *   (per Rimesso Mittente - non per DPD o FEDEX)
029320030515if  1c                   if        DB0ris = 'M'  and  *in70
029330030515     c                   eval      Wctr   =  DB0ctr
029340030515e   1c                   endif
029350030515      *
029360030515     c                   ENDSR
029370170605      *
029380170605      /free
029390170605       //--------------------------------------------------------------
029400170605       //?Controllo SE ragione sociale Mittente da FORZARE (TFNTC) per ?
029410170605       //?spedizione DPD.                                              ?
029420170605       //--------------------------------------------------------------
029430170605       BEGSR  sr_Mitt_DPD;
029440170605
029450170605         // -?Reperim. abilitazioni del Mittente dalla tab. "CLI"?
029460170605         clear  k05tntbe01;
029470170605         k_TBEcod = 'CLI';
029480170605         k_TBEke1 = %editc( DB0ccm : 'X' );
029490170605         chain  %kds( k05tntbe01 )  TNTBE000;
029500170605         if  Not %found( TNTBE01L )  or  TBEatb <> *blank;
029510170605           leavesr;
029520170605         endif;
029530170605         dCLI = TBEuni;
029540170605         // -?Cliente SENZA forzatura Mittente da note "AM" e "AS"?
029550170605         if  §CLItfntc <> 'S'  and  §CLInoIMtt <> 'S';
029560170605           leavesr;
029570170605         endif;
029580170605
029590170605         // -?Forzatura Mittente da note "AM"?
029600170605         If  §CLItfntc = 'S';
029610170605
029620170605           if  NOT %open( TFNTC01L );
029630170605             open  TFNTC01L;
029640170605           endif;
029650170605           clear  keyTFNTC01;
029660170605           k_NTCapl = 'C';
029670170605           k_NTCnk1 = %editc( DUTkci : 'X' ) + %editc( DB0ccm : 'X' );
029680170605           k_NTCtnt = 'AM';
029690170605           chain  %kds( keyTFNTC01 )  TFNTC;
029700170605           if  %found( TFNTC01L )  and  NTCsns = 'S';
029710170605             DB0rsm = NTCrnt;
029720170605           endif;
029730170605
029740170605         EndIf;
029750170605
029760170605         // -?Pulizia Indirizzo Mittente?
029770170605         If  §CLInoIMtt = 'S';
029780170605
029790170605           clear  DB0inm;
029800170605           clear  DB0cam;
029810170605           clear  DB0lom;
029820170605           clear  DB0prm;
029830170605           clear  DB0nzm;
029840170605
029850170605         EndIf;
029860170605
029870170605       ENDSR;
029880170605
029890170605      /end-free
029900030515      *
029910030515      *---------------------------------------------------------------*
029920050217      *?Inschieramento delle Note e delle Particolarità              ?*
029930030515      *---------------------------------------------------------------*
029940030515     c     SchPARTIC     BEGSR
029950030515      *
029960030515     c*** (già fatto:)   reset                   $Partic
029970030710     c*** (già fatto:)   reset                   $NoDesPartic
029980030515     c*** (già fatto:)   clear                   $NP
029990030717     c*** (già fatto:)   clear                   W_CoOr
030000030515     c                   clear                   XX
030010030710      *
030020030710      * Imposto eventuali note per prime
030030030710if  1c                   if        DB0nt1 <> *blanks
030040030710     c                   add       1             xx
030050030710     c                   movel     DB0nt1        $NP(xx)
030060030710e   1c                   endif
030070030710if  1c                   if        DB0nt2 <> *blanks
030080030710     c                   add       1             xx
030090030710     c                   movel     DB0nt2        $NP(xx)
030100030710e   1c                   endif
030110030515      *
030120030515      * Le particolarità possono essere lunghe fino a 45 char.
030130030515      *   Se i primi 30 (della 1ª riga a disposizione) non dovessero
030140030515      *   bastare, il resto lo si mette nella riga successiva.
030150030710      *   Se, invece, non risultano decodificate, si mettono solo i
030160030710      *   codici sulla stessa riga.
030170030710      *   Se uno solo risulta decodificato, esso va su una riga a
030180030710      *   parte (vedi flag "$NoDesPartic": *off = serve riga nuova
030190030710      *                                    *on  = ok riga procedente)
030200030515      *
030210030515      * . . Particolarità Giacenza
030220030515if  1c                   IF        DB0gga <> *blanks
030230030515     c                   movel     'Q'           WTpPar
030240030515     c                   movel     DB0gga        WCdPar
030250030515     c                   exsr      TABxPAR
030260030710if  2c                   if            TBLuni       = *blanks
030270030710     c                             and $NoDesPartic = *on
030280030710     c                   eval      $NP(xx) = %trim($NP(xx))
030290030710     c                                     + '   ' + WCdPar
030300030710x   2c                   else
030310030710if  3c                   if        TBLuni       = *blanks
030320030710     c                   eval      $NoDesPartic = *on
030330030710x   3c                   else
030340030710     c                   eval      $NoDesPartic = *off
030350030710e   3c                   endif
030360030515     c                   add       1             xx
030370100513     c                   movel     w053a         $NP(xx)
030380100513if  3c                   if        %subst(w053a : %len($NP(xx))+1)
030390030515     c                              <> *blanks
030400030515     c                   add       1             xx
030410030515     c                   eval      $NP(xx) = '   ' +
030420100513     c                             %trim(%subst(w053a:%len($NP(xx))+1))
030430030710e   3c                   endif
030440030515e   2c                   endif
030450030515e   1c                   ENDIF
030460030515      *
030470030515      * . . Particolarità Consegna
030480030515if  1c                   IF        DB0gma <> *blanks
030490030515     c                   movel     'R'           WTpPar
030500030515     c                   movel     DB0gma        WCdPar
030510030515     c                   exsr      TABxPAR
030520030710if  2c                   if            TBLuni       = *blanks
030530030710     c                             and $NoDesPartic = *on
030540030710     c                   eval      $NP(xx) = %trim($NP(xx))
030550030710     c                                     + '   ' + WCdPar
030560030710x   2c                   else
030570030710if  3c                   if        TBLuni       = *blanks
030580030710     c                   eval      $NoDesPartic = *on
030590030710x   3c                   else
030600030710     c                   eval      $NoDesPartic = *off
030610030710e   3c                   endif
030620030515     c                   add       1             xx
030630100513     c                   movel     w053a         $NP(xx)
030640100513if  3c                   if        %subst(w053a : %len($NP(xx)) + 1)
030650030515     c                              <> *blanks
030660030515     c                   add       1             xx
030670030515     c                   eval      $NP(xx) = '   ' +
030680100513     c                             %trim(%subst(w053a:%len($NP(xx))+1))
030690030710e   3c                   endif
030700030515e   2c                   endif
030710030515e   1c                   ENDIF
030720030515      *
030730030515      * . . Particolarità Varie
030740030717      *     - questa descrizione va messa nell'apposito campo PPAD
030750041015      *       se è "PRIVATO" idem se è "CONSEGNA AD UFFICI"
030760150304     *** *     - se 'B' (bancali da rendere al mittente) comparirà un
030770150304     *** *       apposito box
030780150304     *** *     - se 'O' (oltre 20 colli => bancali da riportare in mag.)
030790150304     *** *       comparirà un apposito box, un'apposita nota e l'indica-
030800150304     *** *       zione nella copia per il destinatario.
030810030717      *     ...Potrebbero anche non esserci più elementi di schiera
030820030717      *     disponibili [xx >= %elem($NP)] !
030830030709if  1c                   IF            DB0gva <> *blanks
030840150304     c*//                          and DB0gva <> 'B '
030850150304     c*//                          and DB0gva <> 'O '
030860030709     c                             and DB0gva <> 'P '
030870041015     c                             and DB0gva <> 'U '
030880030515     c                   movel     'S'           WTpPar
030890030515     c                   movel     DB0gva        WCdPar
030900030515     c                   exsr      TABxPAR
030910030710if  2c                   if            TBLuni       = *blanks
030920030710     c                             and $NoDesPartic = *on
030930030710     c                   eval      $NP(xx) = %trim($NP(xx))
030940030710     c                                     + '   ' + WCdPar
030950030710x   2c                   else
030960030710if  3c                   if        TBLuni       = *blanks
030970030710     c                   eval      $NoDesPartic = *on
030980030710x   3c                   else
030990030710     c                   eval      $NoDesPartic = *off
031000030710e   3c                   endif
031010030717if  3c                   if            xx < %elem($NP)
031020030709     c                   add       1             xx
031030100513     c                   movel     w053a         $NP(xx)
031040100513if  4c                   if        %subst(w053a : %len($NP(xx)) + 1)
031050030709     c                              <> *blanks
031060030717     c                             and xx < %elem($NP)
031070030709     c                   add       1             xx
031080030709     c                   eval      $NP(xx) = '   ' +
031090100513     c                             %trim(%subst(w053a:%len($NP(xx))+1))
031100030717e   4c                   endif
031110030717e   3c                   endif
031120030709e   2c                   endif
031130030515e   1c                   ENDIF
031140030717      *
031150030717      * Imposto il numero originale di colli componenti la spedizione
031160030717      * e se ho ancora spazio lo memorizzo in "Note e Particolarità"
031170030717if  1c                   if        %subst(DB0gva:1:1) = 'O'
031180030717     c                   exsr      RepAR5bnb
031190030717if  2c                   if        W_CoOr  <> *blanks
031200030717     c                             and xx  <  %elem($NP)
031210030717     c                   add       1             xx
031220030717     c                   eval      $NP(xx) =  W_CoOr
031230030717e   2c                   endif
031240030717e   1c                   endif
031250030515      *
031260030515      * Imposto il flag per memorizzare la già avvenuta memorizzazione
031270030715      *   delle "Note e Particolarità"
031280030515     c                   eval      $Partic = *on
031290030515      *
031300030515     c                   ENDSR
031310070319      *
031320070319      *---------------------------------------------------------------*
031330070319      *?Inschieramento delle Consegne Parziali precedenti            ?*
031340070319      *---------------------------------------------------------------*
031350070319     c     SchConsParz   BEGSR
031360070319      *
031370070319     c*** (già fatto:)   reset                   $CP
031380070319     c*** (già fatto:)   reset                   $CPdcm
031390070319     c                   clear                   XX
031400070319      *
031410070319if  2c                   if        NOT %open(FNART01L)
031420070319     c                   open      FNART01L
031430070319e   2c                   endif
031440070319      *
031450070514if  2c                   if        $Mamma = *off
031460070319     c     KLDV          setll     FNART000
031470070319     c     KLDV          reade     FNART000
031480070514x   2c                   else
031490070514     c     KLBL          setll     FNART000
031500070514     c     KLBL          reade     FNART000
031510070514e   2c                   endif
031520070319      *
031530070319do  1c                   DOW       NOT  %eof(FNART01L)
031540070319      *
031550070319if  2c                   If             ARTatb = *blanks
031560070319     c                             and  ARTdcm > *zeros
031570070319      *
031580070319     c                   eval      *in31  = *off
031590070319     c                   eval      xx     = 1
031600070319     c     ARTdcm        lookup    $CPdcm(xx)                             31
031610070319if  3c                   if        NOT  *in31
031620070319     c                   eval      xx     = 1
031630070319     c     *zeros        lookup    $CPdcm(xx)                             31
031640070319if  4c                   if        *in31
031650140307     c*//                eval      $CP(xx)     = *zeros
031660140307     c                   clear                   Elem_CP
031670140307     c                   movel     Elem_CP       $CP(xx)
031680070319e   4c                   endif
031690070319e   3c                   endif
031700070319if  3c                   if        *in31
031710070319     c                   eval      $CPdcm(xx)  = ARTdcm
031720070319     c                   movel     $CP(xx)       Elem_CP
031730070319     c                   eval      WCPdcm      = ARTdcm
031740070319     c                   eval      WCPncl      = WCPncl + 1
031750070319     c                   movel     Elem_CP       $CP(xx)
031760070319e   3c                   endif
031770070319      *
031780070319e   2c                   EndIf
031790070319      *
031800070514if  2c                   if        $Mamma = *off
031810070319     c     KLDV          reade     FNART000
031820070514x   2c                   else
031830070514     c     KLBL          reade     FNART000
031840070514e   2c                   endif
031850070319      *
031860070319e   1c                   ENDDO
031870070319      *
031880140513if  0c                   if        xx     > *zero
031890140513      * Reperimento nominativi firmatari
031900140513     c                   exsr      sr_Firmatario
031910070319      * Ordinamento schiera per data
031920070319if  1c                   if        xx     > 1
031930070319     c                   sorta                   $CP
031940070319e   1c                   endif
031950140513e   0c                   endif
031960070319      *
031970070319     c                   ENDSR
031980140307      *
031990140307      /free
032000140307
032010140307       //--------------------------------------------------------------
032020140307       //?Reperimento nome firmatario                                  ?
032030140307       //--------------------------------------------------------------
032040140307       BEGSR  sr_Firmatario;
032050140307
032060140307if  1    if  NOT  %open(FNEVB22L);
032070140307           open  FNEVB22L;
032080140307e   1    endif;
032090140307
032100140307         clear  keyFNEVB22;
032110140307         k_EVB2aas = DB0aas;
032120140307         k_EVB2lnp = DB0lnp;
032130140307         k_EVB2nrs = DB0nrs;
032140140307         k_EVB2nsp = DB0nsp;
032150140307         k_EVB2cev = 'P  ';
032160140307         setll  %kds( keyFNEVB22 : 4 )  FNEVB200;
032170140307         reade  %kds( keyFNEVB22 : 4 )  FNEVB200;
032180140307         DoW  Not  %eof(FNEVB22L);
032190140307           if  EVB2cev = k_EVB2cev  and  EVB2trd = 'FIR';
032200140307             yy = %lookup( EVB2dev : $CPdcm );
032210140307             if  yy > *zero;
032220140307               Elem_CP = $CP(yy);
032230140307               if  WCPocm  <= EVB2hev;
032240140307                 WCPocm  = EVB2hev;
032250140307                 WCPfir  = EVB2dati;
032260140307                 $CP(yy) = Elem_CP;
032270140307               endif;
032280140307             endif;
032290140307           endif;
032300140307           reade  %kds( keyFNEVB22 : 4 )  FNEVB200;
032310140307         EndDo;
032320140307
032330140307       ENDSR;
032340140307
032350140307      /end-free
032360031124      *
032370031124      *---------------------------------------------------------------*
032380050217      *?Reperimento Nominativo e Telefono destinatario da FIAR5/GEN  ?*
032390031124      *---------------------------------------------------------------*
032400031124     c     RepAR5gen     BEGSR
032410031124      *
032420031124     c                   clear                   dAR5gen
032430031124      *
032440031124     c                   eval      AR5trd = 'GEN'
032450040301if  1c                   if        DUTlpo = 'S'
032460040531     c     K05AR501      chain     FIAR531C
032470031127x   1c                   else
032480031127     c     K05AR501      chain     FIAR5000
032490031127e   1c                   endif
032500031124      *
032510040301if  1c                   if           (DUTlpo =  'S'
032520040531     c                             and %found(FIAR531C)
032530040301     c                             OR  DUTlpo <> 'S'
032540040301     c                             and %found(FIAR501L))
032550040301     c                             and AR5atb =  *blanks
032560031124     c                   movel     AR5uni        dAR5gen
032570031124e   1c                   endif
032580031124      *
032590031124     c                   ENDSR
032600030717      *
032610030717      *---------------------------------------------------------------*
032620050217      *?Reperimento Numero Colli Originale da FIAR5/BNB              ?*
032630030717      *---------------------------------------------------------------*
032640030717     c     RepAR5bnb     BEGSR
032650030717      *
032660030717     c                   clear                   dAR5bnb
032670030717     c*** (già fatto:)   clear                   W_CoOr
032680030717      *
032690030717     c                   eval      AR5trd = 'BNB'
032700040301if  1c                   if        DUTlpo = 'S'
032710040531     c     K05AR501      chain     FIAR531C
032720040301x   1c                   else
032730030717     c     K05AR501      chain     FIAR5000
032740040301e   1c                   endif
032750030717      *
032760040301if  1c                   if           (DUTlpo =  'S'
032770040531     c                             and %found(FIAR531C)
032780040301     c                             OR  DUTlpo <> 'S'
032790040301     c                             and %found(FIAR501L))
032800040301     c                             and AR5atb =  *blanks
032810030717     c                   movel     AR5uni        dAR5bnb
032820030717if  2c                   if        §AR5bcor <> *zeros
032830030717     c                   eval      W_CoOr   =  C_CoOr
032840030717     c                                      +  %editc(§AR5bcor : 'K')
032850030717e   2c                   endif
032860030717e   1c                   endif
032870030717      *
032880030717     c                   ENDSR
032890030515      *
032900030515      *---------------------------------------------------------------*
032910051129      *?Decodifiche condizionate in Lettera di Vettura               ?*
032920030515      *---------------------------------------------------------------*
032930040607     c     DecodLDV      BEGSR
032940030515      *
032950030515      * Richiesta firma per ricevuta della merce senza riserve
032960030515      * (e NON  quella  per quietanza) => *in35 + not *in36
032970030515     c                   movea     '10'          *in(35)
032980101130      *
032990101130      /free
033000101130
033010101130         // -?Memorizzazione consegne parziali precedenti dal file?
033020101130         //  ?FNART00F - serve anche nella stampa delle "tagliatelle"?
033030140513         IF  $ConsParz;
033040140513
033050140513           // -?SE si stanno stampando "tagliatelle":?
033060140514           //  ?le consegne parziali vengono indicate nel Box1,?
033070140514           //  ?SE?ne sono state reperite?
033080140513           IF  DB0fa4 = '4'  and  $CP(1) < *all'9';
033090140513
033100101130             // -?Prima si totalizzano le quantità consegnate?
033110101130             clear  w0080;
033120101130             clear  w0140;
033130101130             For  xx = 1  To  %elem($CP);
033140101130               if  $CP(xx) < *all'9';
033150101130                 Elem_CP = $CP(xx);
033160101130                 monitor;
033170101130                   Data_Eur = %date( WCPdcm : *iso );
033180101130                 on-error;
033190101130                   Data_Eur = *loval;
033200101130                 endmon;
033210101130                 if       Data_Eur  > *loval  and
033220101130                     %dec(Data_Eur) > w0080;
033230101130                   w0080 = %dec(Data_Eur);
033240101130                 endif;
033250101130                 w0140 += WCPncl;
033260101130               endif;
033270101130             EndFor;
033280101130             // -?Poi si stampa un'unica indicazione?
033290101201             Data_DMY = %date( w0080 : *eur );
033300101201             Pcpp = 'Consegnati '
033310101201                  + %triml( %editc( w0140 : 'Z' ) )
033320101201                  + ' colli al '
033330101201                  + %triml( %editc( %dec( Data_DMY ) : 'Y' ) );
033340140513
033350140513           ENDIF;
033360140513
033370140513         ENDIF;
033380101130
033390101130      /end-free
033400070307      *
033410070307      * Intestazione moduli
033420070307      * - Se spedizione in stato "Lasciato Avviso"
033430070307      *   si stampano anche data/ora del 1° lasciato avviso
033440070307if  1c                   if        $LascAvv  = *on
033450140307     c                   z-add     wEVBdev       w0080
033460070315     c     *iso          test(d)                 w0080                  31
033470070315if  2c                   if        NOT *in31
033480071025     c     *iso          movel     w0080         Data_EUR
033490071025     c     *eur          movel     Data_EUR      w0080
033500070315x   2c                   else
033510070315     c                   clear                   w0080
033520070315e   2c                   endif
033530070307     c                   eval      Tla    =  'Lasciato avviso il '
033540070307     c                                    +  %editc(w0080 : 'Y')
033550070307     c                                    +  ' alle '
033560140307     c                                    +  %editw(wEVBhev : '  :  ')
033570070307e   1c                   endif
033580030709      *
033590100107     *** * Evidenziazione servizio "espresso"
033600100107     *** * o del servizio "Poste italiane"
033610100107     ***c                   eval      *in37 = (DB0tsp = 'E')
033620100107     ***c  n37              eval      *in37 = (§OGntw = 'PPT')
033630090611      *
033640090605      /free
033650100107
033660101130         // -?Evidenziazione servizio "espresso"?
033670101130         //           ?o del servizio "Poste Italiane"?
033680100107         *in37 = (DB0tsp = 'E'   or   DB0tsp = 'H'   or
033690100107                  §OGntw = 'PPT');
033700100107
033710101130         // -?Consegna al mattino (tempi consegna = 12 ore)?
033720101130         //  ?(NON per Würth)?
033730090615         if  §VPOrcm <= wDate and
033740090622             DUTlpo  <> 'S'   and
033750090615             DB0tsp   = 'E'   and
033760090615            (DB0nrs  <> 40    or  (DB0lnp <> 043   and
033770110429                                   DB0lnp <> 277   and
033780090605                                   DB0lnp <> 063));
033790090605           clear  TNSD99ds;
033800090605           if  DB0tbo = 'P'   or   DB0tbo = '8';
033810090605             D98tbo = 'P';
033820090605           else;
033830090605             D98tbo = 'A';
033840090605           endif;
033850090605           D98aas = DB0aas;
033860090605           D98lnp = DB0lnp;
033870090605           D98nrs = DB0nrs;
033880090605           D98nsp = DB0nsp;
033890090605           tnsd99r ( TNSD99ds );
033900120910           //*if  D98tci = 12;
033910120910           if  D98tcc = 12;
033920090605             Pdsm = 'M';
033930090605           endif;
033940090605         endif;
033950090605      /end-free
033960030515      *
033970030515      * Franco prepagato
033980030515      * (richiede firma per quietanza nella copia x destinatario)
033990030515if  1c                   if        DB0tbo =  '8'  and  DB0ris <> 'M'
034000030515     c                   movea     '01'          *in(35)
034010030515if  2c                   if        Pds1   =  *blanks
034020030515     c                   eval      Pds1   =  'FRANCO PREPAGATO        '
034030030515e   2c                   endif
034040120530     c*//                if        (DB0dft > *zero       and
034050120530     c*//                           DB0dft > §vpoBRTdta)  OR
034060120530     c*//                          (DB0dft = *zero       and
034070120530     c*//                           Wdsp   > §vpoBRTdta)
034080130418     c                   eval      Pbrt2  =  $BRT
034090120530     c*//                else
034100130418     c*//                eval      Pbrt2  =  $Bartolini
034110120530     c*//                endif
034120030515      * - Legge 450 (solo per prepagati)
034130030515if  2c                   if            DB0ias =  *zeros
034140030515     c                             and DB0vas =  *blanks
034150030515     c                             and DB0div =  §GEDcn
034160060301     c                             and §GElrp >  *zeros
034170030515     c*** v. IF sovrastante...     and DB0tbo =  '8'
034180060301if  3c                   if        Wdsp   >  20060228
034190110315     c                   z-add     §GElrp        w0112
034200080701     c                   eval      Prcv   =  'Art. 1696 c.c. mod. +
034210061102     c                                       dlgs 286/2005 '
034220060301     c                                    +  §GEDcn + ' '
034230060301     c                                    +  %trim(%editc(w0112 : '4'))
034240060301     c                                    +  ' al kg.'
034250060301x   3c                   else
034260110315     c                   z-add     6.20          w0112
034270080701     c                   eval      Prcv   =  'Legge 450/85 '
034280030515     c                                    +  §GEDcn + ' '
034290030624     c                                    +  %trim(%editc(w0112 : '4'))
034300030515     c                                    +  ' al kg.'
034310060301e   3c                   endif
034320030515e   2c                   endif
034330030515e   1c                   endif
034340031031      *
034350101125      /free
034360121017
034370121017         // -?Indicazione della Partiva IVA o del Cod. Fiscale?
034380121017         //  ?(se Porto Franco Prepagato?
034390121017         //    ?o Porto Assegnato con Fattura immediata)?
034400121017         //  ?- NON se mittente estero -?
034410121017             // -?con fattura?
034420121017         IF  (DB0nft <> *zero  or   DB0ift <> *zero)   AND     (
034430121017             // - -?P.F. prepagato?
034440121017             //    ?(con mittente italiano)?
034450121017             (DB0tbo =  '8'             and  DB0nzm  = *blank)  or
034460121017             // - -?P.A. (con fattura immediata)?
034470121017             //    ?(con destinatario italiano)?
034480121017             (%subst(DB0tb1:1:1) = 'A'  and  DB0nzd  = *blank)  or
034490121017             // - -?con 2ª bolla?
034500121017             //    ?(con destinatario italiano)?
034510121017             (DB0tb2 <> *blank          and  DB0tbo <> '8'
034520121029                                        and  DB0tbo <> 'P'
034530121017                                        and  DB0nzd  = *blank) );
034540121017           select;
034550121017             when  Not $No_PI  and  wB0piv <> *blank  and
034560121017                   // ·?Esiste Partita Iva NON FITTIZIA?
034570121026                   %scan('$$' : wB0piv) = *zero;
034580121017                   // ·?Non è un C.F. - lungo 16 (privato)?
034590121026                   //and  %len( %trim( wB0cfi ) ) < 16;
034600121017               Ppiv = 'Partita Iva: ' + wB0piv;
034610121017             when  Not $No_CF  and  wB0cfi <> *blank;
034620121017               Ppiv = 'Codice Fiscale: ' + wB0cfi;
034630121017           endsl;
034640130418           //select;
034650130418           //  // ·?Se P.F. Prepagato il C.F./P.I. è del Mittente?
034660130418           //  when  DB0tbo = '8';
034670130418           //    PpivM = Ppiv;
034680130418           //  // ·?Se P.A. con fattura immed. il C.F./P.I. è del Dest.?
034690130418           //  when  %subst(DB0tb1:1:1) = 'A';
034700130418           //    PpivD = Ppiv;
034710130418           //  // ·?Se 2ª bolla (DDT sì) è del Dest.?
034720130418           //  when  DB0tb2  <> *blanks  and  DB0tbo  <> 'P';
034730130418           //    PpivD = Ppiv;
034740130418           //endsl;
034750121017         ENDIF;
034760101125
034770101125         Select;
034780101125
034790101125           // -?Non utilizzare per la consegna?
034800101125           //  ?(stampa della sola copia per il vettore)?
034810101125           When  $No_x_Cons = *on;
034820101125             *in35 = *off;
034830101125             *in36 = *off;
034840101125             // -?costante da stampare se LDV recapito c/assegni?
034850101125             //  ?stampata dal pgm "Stampa DDT NO in arrivo"?
034860101129             Pnux  = 'NON UTILIZZARE PER LA CONSEGNA';
034870101125
034880101125           // -?Ritorno all'Incasso?
034890101125           When  $Rit_a_Inc = *on;
034900101125             *in40 = *on;
034910101125             Pnux  = 'RITORNO ALL''INCASSO';
034920101125
034930101125         EndSl;
034940101125
034950101125      /end-free
034960030910      *
034970030910      * Bollo assolto in modo virtuale
034980030910if  1c                   if        $BolloVirt =  *on
034990060719if  2c                   if        DB0dft <  20060801
035000080701     c                   eval      Pbav1  =  'Bollo '
035010080701     c                                    +  %trim(%editc(wBolloVirt:'4'))
035020080701     c                                    +  ' ' + DB0div
035030080701     c                                    +  ' assolto in modo virtuale '
035040080701     c                   eval      Pbav2  =  'Aut. Ag. Ent. BO 3 27/09/02 '
035050080701     c                                    +  'Prot. 31599'
035060060719x   2c                   else
035070080701     c                   eval      Pbav1  =  'Bollo '
035080080701     c                                    +  %trim(%editc(wBolloVirt:'4'))
035090080701     c                                    +  ' ' + DB0div
035100080701     c                                    +  ' assolto in modo virtuale '
035110080701     c                   eval      Pbav2  =  'Aut. Ag. Ent. MI 2 10/07/06 '
035120080701     c                                    +  'Prot. 45114'
035130060719e   2c                   endif
035140030910e   1c                   endif
035150050503      *
035160080605      * Costanti "PER RICEVUTA DELLA MERCE..."
035170080605     c                   eval      Pkrm1 = '(nome e cognome in stampa+
035180080605     c                                       tello)'
035190080605     c                   eval      Pkrm2 = '(firma)'
035200080605     c                   eval      Pkrm3 = '(data e ora)'
035210050504sel 1c                   select
035220050504      * - danon?stampare se
035230050504      *   · franco prepagato                (NOT *in35)
035240050504      *   · non utilizzare per la consegna  (NOT *in35)
035250101125      *   · ritorno all'incasso             (*in40)
035260050504      *   · bolla FedEx                     (*in72)
035270101125w   1c                   when      NOT *in35  or  *in40  or  *in72
035280050504     c*** (già fatto:)   clear                   Pkrm
035290080605     c                   clear                   Pkrm1
035300080605     c                   clear                   Pkrm2
035310080605     c                   clear                   Pkrm3
035320080922      * - costante "DPD"
035330120301w   1c                   when      *in71
035340080922     c                   eval      Pkrm   =  'PER RICEVUTA DELLA MERCE-
035350080922     c                              SENZA RISERVE'
035360120530      * - costante "standard" per "DDT sì"
035370090615w   1c                   when          §VPOrsr <= wDate
035380081119     c                             and  DB0bam =  'S'
035390071005     c                   eval      Pkrm   =  'PER RICEVUTA DELLA MERCE-
035400071005     c                              E DEL DOCUMENTO DI TRASPORTO SENZA-
035410071005     c                              RISERVE'
035420120530      * - costante "standard" per "DDT no" + Packing-List
035430120301w   1c                   when           DB0bam =  *blank
035440170605     c                             and ($FIAR8 =  *on
035450170605     c                              or  $§PDF  =  *on)
035460120301     c                   eval      Pkrm   =  'PER RICEVUTA DELLA MERCE-
035470120301     c                              E DEL DOCUMENTO DI TRASPORTO SENZA-
035480120301     c                              RISERVE'
035490120301      * - costante "standard" altrimenti
035500050504x   1c                   other
035510050504     c                   eval      Pkrm   =  'PER RICEVUTA DELLA MERCE-
035520050504     c                              SENZA RISERVE'
035530050504e   1c                   endsl
035540090513      /free
035550090611       Select;
035560101130         // -?costante per cliente?
035570101130         //  ?"Consorzio Lotterie Nazionali - Gratta & Vinci"?
035580140513         when  DB0ccm = c_CliCLN  or  DB0ccm = c_CliLN_2;
035590090528           exsr  sr_RepDdtCLN;
035600090706           Pkrm =  'LA FIRMA PER RICEVUTA DELLA MERCE VALE ANCHE PER +
035610090706                    IL DDT N° '
035620090527                + %editc (WddtCLN : 'Z')
035630090615                + ' DEL '
035640090527                + %editc (WdspCLN : 'Y')
035650101001                + ' CON RISERVA DI CONTROLLO COME DA CONTRATTO';
035660101001           if  Wdsp < 20101001;
035670101001             Pkrm = %trimr(Pkrm) + ' C.L.N.';
035680101001           else;
035690101001             Pkrm = %trimr(Pkrm) + ' L.N. S.R.L.';
035700101001           endif;
035710101130         // -?costante per cliente?
035720101130         //  ?"S G Soluzioni Logistiche"?
035730091117         when  DB0ccm = c_CliSG   or   DB0ccm = c_CliSG_2;
035740091117           if  DB0nas <> *blank   and   DB0nas <> c_NoContr;
035750091117             Pkrm = %trimr(Pkrm)
035760091117                  + '.   INCLUDE CONTRATTO - '
035770091117                  + %trim(DB0nas);
035780091117           endif;
035790091117           if  DB0rmo <> *blank;
035800091117             if  DB0nas <> *blank;
035810091117               Pkrm = %trimr(Pkrm)
035820091117                    + ' - MATR TV: '
035830091117                    + %trim(DB0rmo);
035840091117             else;
035850091117               Pkrm = %trimr(Pkrm)
035860091117                    + '.   MATR TV: '
035870091117                    + %trim(DB0rmo);
035880091117             endif;
035890091117           endif;
035900090611       EndSl;
035910090513      /end-free
035920030717      *
035930130418      *//* Indicazione degli "oltre 20 colli"
035940130418      *//* (MAI se Franco prepagato)
035950130418if  1c*//                   if            DB0tbo <> '8'
035960130418     c*//                             and *in36  =  *off
035970130418     c*//                             and W_CoOr <> *blanks
035980130418     c*//                   eval      Pbrt2  = W_CoOr
035990130418e   1c*//                   endif
036000030515      *
036010030515      * Verifico se imponibile/iva/totale/estremi fattura
036020030515     c                   eval      *in19  = ((DB0imv <> *zeros)  or
036030030515     c                                       (DB0iva <> *zeros)  or
036040030515     c                                       (DB0cei <> *blanks) or
036050030515     c                                       (DB0dce <> *blanks) or
036060030515     c                                       (DB0ift <> *zeros)  or
036070030515     c                                       (DB0nft <> *zeros)  or
036080031125     c                                       (DB0dft <> *zeros))
036090030515      *
036100030515      * Verifico se contrassegno
036110030515     c                   eval      *in20  =  (Pcas <> *zeros)
036120030515      *
036130030709      * Verifico se totale da esigere al ritiro o alla consegna
036140080701     c                   eval      *in21  =  (Pies <> *zeros)
036150030709      *
036160030710      * Intesto il 2° modulo diversamente per prepagato
036170030710      * (solo in partenza)
036180030710if  1c                   if        *in21  and  *in36
036190030710     c                             and   (DB0tbo = 'P'
036200030710     c                                or  DB0tbo = '8')
036210030710     c                   eval      Tcxd   =  'COPIA PER IL MITTENTE    '
036220130729     c                   clear                   Tcxd2
036230030710e   1c                   endif
036240030515      *
036250120530      * Verifico se codice a barre BRT da stampare
036260030515     c                   eval      *in50  =  (Tbcd <> *blanks)
036270040629      * Verifico se codice a barre O.R.M.    da stampare
036280040629      * (non è ancora impostato - vedi subr. "StampaRCC")
036290040629     c***                eval      *in52  =  (Pbco <> *blanks)
036300030515      *
036310030515      * Flag "*" > x Consegne particolari
036320030515     c                   if        DB0tc1 =  'A'  or  DB0tc2 = 'A'
036330030515     c                   eval      Past   =  '*'
036340030515     c                   endif
036350030515      *
036360030515      * C/ASSEGNO - - - - - - - - - - - - - - - - - - - - - - - - - - *
036370030515if  1c                   IF        DB0cas <> *zeros
036380030515      * - Flag "*" > x C/Assegno
036390030904if  2c                   if        O21fca <> *blanks
036400030904     c                   movel     '*'           Past
036410030515e   2c                   endif
036420030624      * - Imposto i campi di stampa
036430030624     c                   eval      Ptic   =  DB0tic + ' ' + DB0dti
036440030515e   1c                   ENDIF
036450030515      *
036460040607      * TIPO LDV  - - - - - - - - - - - - - - - - - - - - - - - - - - *
036470030515      * - Se Bolla in Assegnato: stampo una "A" vicino all'asterisco
036480030515      * e se Bolla in Assegnato Senza Fattura: stampo "SEGUE FATTURA"
036490030515if  1c                   if           %subst(DB0tb1:1:1) = 'A'
036500030515     c                             or        DB0tb2     <> *blanks
036510030515     c                   eval      Pass   =  'A'
036520030515if  2c                   if        DB0nft =  *zeros
036530031125     c                   eval      *in22  =  *on
036540130215     c                   eval      Piva   =  'SEGUE FATTURA'
036550030515e   2c                   endif
036560030515e   1c                   endif
036570030515      *
036580030515      * DECODIFICA TIPO CONSEGNA RICHIESTA  - - - - - - - - - - - - - *
036590051129sel 1***c                   Select
036600051129w   1***c                   WHEN      DB0tcr = *blanks
036610051129     *** * - giorno si
036620051129w   1***c                   WHEN      DB0dcr <> *zeros  and  DB0tcr = 'P'
036630051129     ***c                   eval      Ptdr   =  'PRIMA DEL  '
036640051129w   1***c                   WHEN      DB0dcr <> *zeros  and  DB0tcr = 'D'
036650051129     ***c                   eval      Ptdr   =  'DOPO IL    '
036660051129     *** * - giorno no  ora si
036670051129w   1***c                   WHEN      DB0dcr =  *zeros  and  DB0tcr = 'P'
036680051129     ***c                   eval      Ptdr   =  'PRIMA DELLE'
036690051129w   1***c                   WHEN      DB0dcr =  *zeros  and  DB0tcr = 'D'
036700051129     ***c                   eval      Ptdr   =  'DOPO LE    '
036710051129e   1***c                   ENDSL
036720051129if  1c                   IF            DB0dcr >  *zeros
036730051129     c                             or  DB0hcr >  *zeros
036740051129     c                   clear                   FNLV80ds
036750051129     c                   eval      ILV80aas   =  DB0aas
036760051129     c                   eval      ILV80lnp   =  DB0lnp
036770051129     c                   eval      ILV80nrs   =  DB0nrs
036780051129     c                   eval      ILV80nsp   =  DB0nsp
036790051129     c                   eval      ILV80mgs   =  DB0mgs
036800051129     c                   eval      ILV80tc1   =  DB0tc1
036810051129     c                   eval      ILV80tc2   =  DB0tc2
036820051129     c                   eval      ILV80dcr   =  DB0dcr
036830051129     c                   eval      ILV80hcr   =  DB0hcr
036840051129     c                   eval      ILV80tcr   =  DB0tcr
036850051129     c                   call      'FNLV80R'
036860051129     c                   parm                    KPJBA
036870051129     c                   parm                    FNLV80ds
036880051129sel 2c                   Select
036890051129      * errore
036900051129w   2c                   When      OLV80err  <>  *blanks
036910051129      * consegna tassativa (per ora solo ai supermercati)
036920051129w   2c                   When      OLV80tat   =  'S'
036930051129     c                   movel     OLV80tcrd     Pcot
036940091116      * NO "Prima del" per il cliente TTL
036950091116w   2c                   When      DB0ccm     = c_CliTTL   and
036960091116     c                             DB0dcr    <> *zeros     and  DB0tcr = 'P'
036970051129      * prima/dopo il/delle
036980051129      * (il giorno e/o l'ora di consegna richiesta sono già impostati
036990051129      *  nella subr. "TipoLDV")
037000051129w   2c                   Other
037010051129     c                   movel     OLV80tcrd     Ptdr
037020051129e   2c                   Endsl
037030051129e   1c                   ENDIF
037040030515      *
037050030515      * DECODIFICA DEI TURNI DI CHIUSURA  - - - - - - - - - - - - - - *
037060060921      * (SE?non?è "Fermo Deposito")
037070060921if  1c                   if        DB0ffd =  *blanks
037080060921     c                   eval      Pktc   =  'TURNO DI CHIUSURA'
037090060921e   1c                   endif
037100030515      * 1° turno di chiusura
037110060921if  1c                   IF             DB0gc1 <> *blanks
037120060921     c                             and  DB0ffd  = *blanks
037130030515      * - Flag "*" > Turno di chiusura
037140030515     c                   eval      Past   =  '*'
037150030515if  2c                   If        %subst(DB0gc1:1:1) = *blank
037160030515      * - Nessun giorno specificato - Solo 'M' o 'P'
037170060921     c*** già così:      clear                   Pgc1
037180030515if  3c                   if        DB0gc1 =  ' M'
037190030515     c                   eval      Pgc1   =  %trim($Gio(8))
037200030515x   3c                   else
037210030515     c                   eval      Pgc1   =  %trim($Gio(9))
037220030515e   3c                   endif
037230030515x   2c                   Else
037240030515      * - Giorno specificato
037250030515     c                   movel     DB0gc1        w001a
037260030515     c                   eval      XX     =  1
037270030515     c                   eval      *in31  =  *off
037280030515     c     w001a         lookup    $GgS(xx)                               31
037290030515     c   31              movel     $Gio(xx)      Pgc1
037300030515      *   + 'M' o 'P'
037310030515sel 3c                   select
037320030515w   3c                   when      %subst(DB0gc1:2:1) = 'M'
037330030515     c                   eval      Pgc1   =  %trim(Pgc1) + ' '
037340030515     c                                    +  %trim($Gio(8))
037350030515w   3c                   when      %subst(DB0gc1:2:1) = 'P'
037360030515     c                   eval      Pgc1   =  %trim(Pgc1) + ' '
037370030515     c                                    +  %trim($Gio(9))
037380030515e   3c                   endsl
037390030515e   2c                   Endif
037400030515e   1c                   ENDIF
037410030515      * 2° turno di chiusura
037420060921if  1c                   IF             DB0gc2 <> *blanks
037430060921     c                             and  DB0ffd  = *blanks
037440030515      * - Flag "*" > Turno di chiusura
037450030515     c                   eval      Past   =  '*'
037460030515if  2c                   If        %subst(DB0gc2:1:1) = *blank
037470030515      * - Nessun giorno specificato - Solo 'M' o 'P'
037480060921     c*** già così:      clear                   Pgc2
037490030515if  3c                   if        DB0gc2 =  ' M'
037500030515     c                   eval      Pgc2   =  %trim($Gio(8))
037510030515x   3c                   else
037520030515     c                   eval      Pgc2   =  %trim($Gio(9))
037530030515e   3c                   endif
037540030515x   2c                   Else
037550030515      * - Giorno specificato
037560030515     c                   movel     DB0gc2        w001a
037570030515     c                   eval      XX     =  1
037580030515     c                   eval      *in31  =  *off
037590030515     c     w001a         lookup    $GgS(xx)                               31
037600030515     c   31              movel     $Gio(xx)      Pgc2
037610030515      *   + 'M' o 'P'
037620030515sel 3c                   select
037630030515w   3c                   when      %subst(DB0gc2:2:1) = 'M'
037640030515     c                   eval      Pgc2   =  %trim(Pgc2) + ' '
037650030515     c                                    +  %trim($Gio(8))
037660030515w   3c                   when      %subst(DB0gc2:2:1) = 'P'
037670030515     c                   eval      Pgc2   =  %trim(Pgc2) + ' '
037680030515     c                                    +  %trim($Gio(9))
037690030515e   3c                   endsl
037700030515e   2c                   Endif
037710030515e   1c                   ENDIF
037720071114      *
037730071114      * ALTRI DATI/FLAG/... - - - - - - - - - - - - - - - - - - - - - *
037740030515      *
037750071114      * Se pgm richiamato x stampa LdV in arrivo:
037760030515if  1c                   IF        DB0fl1 =  'A'
037770071114      * - imposto l'"*" in base al parametro ricevuto.
037780030515if  2c                   if        DB0fl2 =  '*'
037790030515     c                   eval      Past   =  '*'
037800030515x   2c                   else
037810030515     c                   clear                   Past
037820030515e   2c                   endif
037830071114      * - idem per flag di bolla non abilitata
037840030515     c                   eval      Pdis   =  DB0fl5
037850030515e   1c                   endif
037860061102      *
037870071114      * Decodifica cliente a cui richiedere il codice fiscale e/o la
037880071114      *   partita iva
037890071114if  1c                   IF             $No_CF =  *on
037900071114     c                             or   $No_PI =  *on
037910061106     c                   clear                   wRSxCF
037920061102sel 2c                   select
037930061102      * - P.F. prepagato
037940061106w   2c                   when      DB0tbo =  '8'
037950061106     c                   movel     DB0rsm        wRSxCF
037960061102      * - P.A. con fattura immediata
037970061106w   2c                   when      %subst(DB0tb1:1:1) = 'A'
037980061106     c                   movel     DB0rsd        wRSxCF
037990061106      * - con 2ª bolla
038000061103w   2c                   when           DB0tb2 <> *blanks
038010121029     c                             and  DB0tbo <> 'P'
038020061103     c                             and  DB0tbo <> '8'
038030061106     c                   movel     DB0rsd        wRSxCF
038040061102e   2c                   endsl
038050061102      *
038060061102e   1c                   ENDIF
038070030515      *
038080030515     c                   ENDSR
038090080918      /free
038100090528
038110090528       //--------------------------------------------------------------
038120140307       //?Reperimento Numero e Data del DDT originale del mittente     ?
038130140307       //?"Consorzio Lotterie Nazionali - Gratta & Vinci"              ?
038140090528       //--------------------------------------------------------------
038150090528       BEGSR  sr_RepDdtCLN;
038160090528
038170110426         // -?Verifica se la spedizione ha mamma?
038180090528         if  not %open(FNLBL01L);
038190090528           open  FNLBL01L;
038200090528         endif;
038210090528         chain  (DB0aas : DB0lnp : DB0nrs : DB0nsp)  FNLBL000;
038220090528
038230090528         Select;
038240090528
038250110426           // -?Con Mamma: reperimento dati DDT dalla mamma?
038260090528           When  %found(FNLBL01L);
038270090528             if  not %open(FNBLP01L);
038280090528               open  FNBLP01L;
038290090528             endif;
038300090603             chain  (LBLaao : LBLlpo : LBLnro : LBLnso)  FNBLP000;
038310090528             if  %found(FNBLP01L);
038320090528               exsr  sr_DatiCLN_blp;
038330090528             endif;
038340090528
038350110426           // -?Dati da DS?
038360090901           When  DB0ges = 'B';
038370090901             exsr  sr_DatiCLN_DS;
038380090901
038390110426           // -?In Partenza: reperimento dati DDT da FNBLP?
038400090528           When  (DB0tbo = 'P'   or   DB0tbo = '8');
038410090528             exsr  sr_DatiCLN_blp;
038420090528
038430110426           // -?In Arrivo: reperimento dati DDT da FNARB?
038440090528           Other;
038450090528             exsr  sr_DatiCLN_arb;
038460090528
038470090528         EndSl;
038480090528
038490090528       ENDSR;
038500090528
038510090901       //--------------------------------------------------------------
038520140307       //?Da DS                                                        ?
038530090901       //--------------------------------------------------------------
038540090901       BEGSR  sr_DatiCLN_DS;
038550090901
038560110426         // -?Numero DDT?
038570090901         select;
038580090902           when  DB0rmn <> *zero;
038590100906             if  DB0rmn  > -99999999   and
038600100906                 DB0rmn <   99999999;
038610100906               WddtCLN = DB0rmn;
038620100906             endif;
038630090902           when  DB0ncd <> *zero;
038640090902             WddtCLN = DB0ncd;
038650090901         endsl;
038660090901
038670110426         // -?Data DDT in formato gg/mm/aaaa?
038680090902         Data_Eur = %date( ((DB0aas * 10000) + DB0mgs) : *iso );
038690090901         WdspCLN  = ( ( %subdt(Data_Eur : *days)  * 1000000 ) +
038700090901                      ( %subdt(Data_Eur : *months) * 10000 ) +
038710090901                      ( %subdt(Data_Eur : *years) ) );
038720090901
038730090901       ENDSR;
038740090901
038750090528       //--------------------------------------------------------------
038760090528       //?In Partenza: reperimento dati DDT da FNBLP                   ?
038770090528       //--------------------------------------------------------------
038780090528       BEGSR  sr_DatiCLN_blp;
038790090528
038800110426         // -?Numero DDT?
038810090528         select;
038820090528           when  BLPrmn <> *zero;
038830100906             if  BLPrmn  > -99999999   and
038840100906                 BLPrmn <   99999999;
038850100906               WddtCLN = BLPrmn;
038860100906             endif;
038870090528           when  BLPncd <> *zero;
038880090528             WddtCLN = BLPncd;
038890090528         endsl;
038900090528
038910110426         // -?Data DDT in formato gg/mm/aaaa?
038920090528         Data_Eur = %date( ((BLPaas * 10000) + BLPmgs) : *iso );
038930090528         WdspCLN  = ( ( %subdt(Data_Eur : *days)  * 1000000 ) +
038940090528                      ( %subdt(Data_Eur : *months) * 10000 ) +
038950090528                      ( %subdt(Data_Eur : *years) ) );
038960090528
038970090528       ENDSR;
038980090528
038990090528       //--------------------------------------------------------------
039000090528       //?In Arrivo: reperimento dati DDT da FNARB                     ?
039010090528       //--------------------------------------------------------------
039020090528       BEGSR  sr_DatiCLN_arb;
039030090528
039040110426         // -?Numero DDT?
039050090528         select;
039060090528           when  ARBrmn <> *zero;
039070100906             if  ARBrmn  > -99999999   and
039080100906                 ARBrmn <   99999999;
039090100906               WddtCLN = ARBrmn;
039100100906             endif;
039110090528           when  ARBncd <> *zero;
039120090528             WddtCLN = ARBncd;
039130090528         endsl;
039140090528
039150110426         // -?Data DDT in formato gg/mm/aaaa?
039160090528         Data_Eur = %date( ((ARBaas * 10000) + ARBmgs) : *iso );
039170090528         WdspCLN  = ( ( %subdt(Data_Eur : *days)  * 1000000 ) +
039180090528                      ( %subdt(Data_Eur : *months) * 10000 ) +
039190090528                      ( %subdt(Data_Eur : *years) ) );
039200090528
039210090528       ENDSR;
039220080918
039230080918       //--------------------------------------------------------------
039240140307       //?Stampa della sola Packing-List (2 per A4)                    ?
039250080918       //--------------------------------------------------------------
039260080918       BEGSR  StampaPkgLst;
039270130315
039280130315         // -?SE NON esiste il *file FIAR8 => uscita?
039290130315         if  $E_AR8 = *off;
039300130315           leavesr;
039310130315         endif;
039320080918
039330110426         // -?MODIFICA SOSPESA:?
039340110426         //  ?NON STAMPA LA PACKING-LIST DAL 2° TENTATIVO DI CONSEGNA?
039350081013         //if   $FIAR8 = *off   or   DB0fl1 = 'D';
039360081013         //  leavesr;
039370081013         //endif;
039380080918
039390110426         // -?Azzeramento numero di pagina per Lettera di Vettura?
039400110426         //  ?(meglio: "reimpostazione a 1")?
039410080926         reset  wPage;
039420080918
039430110426         // -?Posizionamento sul foglio A4 in base al n° pagina in stampa?
039440130418         *in33 = *off;
039450130418         exsr  Posiz_Box;
039460080919
039470110426         // -?Impostazione dei dati da stampare nel rec. con la costante?
039480110426         //  ?"Copia per il destinatario"  + estremi spedizione e pag.?
039490110426         //
039500110426         // -?"Copia per il destinatario" già impostato in sr. "PrepPrt"?
039510081017         //if  §AR8cxd = *blank;
039520130729         //  Tcxd  = c_CopiaXdest;
039530130729         //  Tcxd2 = Tcxd;
039540081017         //endif;
039550081017
039560110426         // -?Estremi spedizione da impostare SOLO se si stampando?
039570110426         //  ?la sola packing-list (DB0FA4 = "P")?
039580081017         if  DB0fa4 = 'P';
039590101028           Trsd = Prsd;
039600080922           Trif = 'Spedizione '
039610080924                + %editc(DB0lnp : 'X') + ' '
039620110310                + %editc(DB0nrs : 'X') + ' '
039630110310                + %editc(DB0nsp : 'X')
039640080922                + ' del ' + %editc(Pdsp : 'Y')
039650081017                + '        Pag.'
039660080922                + %editc(wPage:'Z');
039670101028           Tndc = Pndc;
039680081002           if  wSeqLdV = 1;
039690081002             §Pos   = wPosIni + c_PosBox1As + 0.040;
039700090422             §Pos   = wPosIni + c_PosBox1As + 0.035;
039710081002           else;
039720081002             §Pos   = wPosIni + c_PosBox1As - 0.070;
039730081002           endif;
039740080919         endif;
039750081017
039760110426         // -?La stampa di tale record verrà fatta dopo?
039770081017         //if  §AR8cxd = *blank   or   DB0fa4 = 'P';
039780081017         //  write A4prtDT0;
039790081017         //endif;
039800080918
039810110426         // -?Stampa Packing-List?
039820130315         exsr  StampaDDT;
039830080918
039840080918       ENDSR;
039850090528
039860080918      /end-free
039870030515      *
039880030515      *---------------------------------------------------------------*
039890050217      *?Stampa della Lettera di Vettura                              ?*
039900030515      *---------------------------------------------------------------*
039910040607     c     StampaLDV     BEGSR
039920080604      *
039930080604      * - Posizionamento sul foglio A4 in base al n° pagina in stampa
039940130418     c                   eval      *in33 = *off
039950130418     c                   exsr      Posiz_Box
039960030515      *
039970040607      * Azzeramento numero di pagina per Lettera di Vettura
039980030716      * (meglio: "reimpostazione a 1")
039990080926     c                   reset                   wPage
040000030515      *
040010120530      * 1ª parte - copia per il vettore (da ritornare in "BRT")
040020130418     c                   write     A4prtLDV
040030130418     c                   write     A4prtBOX
040040110426     c*//                write     A4LogoBAR
040050081020      /free
040060120530         //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
040070120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
040080130306         //  // >?Simil-Logo Bartolini/BRT?
040090120530         //  write  A4LogoBR2;
040100120530         //  write  A4LogoDA2;
040110120530         //else;
040120130306         //  // >?Simil-Logo Bartolini?
040130120530         //  write  A4LogoBAR;
040140120530         //  write  A4LogoDA1;
040150120530         //endif;
040160130306         // >?Simil-Logo BRT?
040170120530         write  A4LogoBRT;
040180120530         write  A4LogoDA3;
040190110426
040200110426         // >?dati DPD?
040210081020         if  *in71;
040220081020           exsr  StampaDPD;
040230081020         endif;
040240130306
040250130306         // -?Se LdV con PDF => registrazione rec. nel *file PDFLIST?
040260130710         if  %parms() > 3  and  (DB6pdf = 'F'  or  DB6pdf = 'X')
040270130306                           and  $§PDF   = *on;
040280130306           exsr  sr_WrtRec_PdfList;
040290130306         endif;
040300081020
040310130306         // -?Se richiesta solo tagliatella (copia per il vettore, da?
040320130306         //  ?lasciare però al destinatario) => fine?
040330081020         if  DB0fa4  = '4';
040340081020           leavesr;
040350081020         endif;
040360081020      /end-free
040370050510      *
040380081028      * Se LdV in formato "ex-A5" rimane la copia per il destinatario
040390050510     c     *in69         cabeq     *on           End_LdV
040400030515      *
040410030515sel 1c                   select
040420081028      * > dati DPD  (ora in ex-A5 o in 1/4 di A4)
040430081028w   1c***                when      *in71
040440030515      * > dati FedEx
040450030515w   1c                   when      *in72
040460030515     c                   exsr      ListaFED
040470130710      /free
040480130710             if  DB0fa4 = 'S';
040490130710               DB0fa4 = '1';
040500130710             endif;
040510130710      /end-free
040520081028      * > dati EuroExpress  (non più gestiti)
040530040223w   1c                   when      *in73
040540030515      *
040550030515e   1c                   endsl
040560030910      *
040570030910      * > elenco SEGNACOLLI NON SEQUENZIALI
040580050511      *  (no x max 2 perchè sono già stati elencati nell'apposita area)
040590030910      *  (no x DPD   perchè monocollo)
040600030910      *  (no x FedEx perchè segnacolliamo noi)
040610030910      *  (no x "A5"  perchè sono già stati elencati tra le "Note e
040620030910      *              Particolarità")
040630030910if  1c                   if        DB0fns =  'S'
040640050511     c                             and  DB0ncl > 2
040650050513     c                             and  $A4_x_ScNS  =   *on
040660030910     c***                          and  not *in71  and  not *in72
040670030910     c                             and  not *in69
040680030910     c                   exsr      ListaScNS
040690030910e   1c                   endif
040700130710      /free
040710130710             if  DB0fa4 = 'S';
040720130710               DB0fa4 = '1';
040730130710             endif;
040740130710      /end-free
040750031124      *
040760031124      * Reperimento della posizione utilizzabile per la stampa dei box
040770080825      *   (in pollici) in base all'ultima riga stampata in 1/8" o 1/9"
040780031124     c                   exsr      CalcRowPos
040790060524      *
040800070123      * > Spedizione con D.A.S. x cliente "PRAOIL OLEODOTTI ITALIANI"
040810081127if  1c                   if             DB0ccm = c_CliPraOil
040820060529     c                             or  (DB0fbr = 'S'
040830081127     c                             and  §B4Aks = c_CliPraOil)
040840060524     c                   exsr      StampaDAS
040850130710      /free
040860130710             if  DB0fa4 = 'S';
040870130710               DB0fa4 = '1';
040880130710             endif;
040890130710      /end-free
040900060524e   1c                   endif
040910031124      *
040920031124      * > Promemoria per autotrasportatore
040930110502if  1c*//*               if            (DB0gma = Ktab7V_CB
040940110502     c*//*                           or DB0gma = Ktab7V_C1
040950080625     c*** scontato x GMA='C1'       and DB0bam = 'S'
040960110502     c*//*                           or DB0gma = Ktab7V_C2
040970080729     c*** scontato x GMA='C2'       and DB0bam = 'S'
040980110502     c*//*                           or DB0gma = Ktab7V_C3
040990100513     c*** scontato x GMA='C3'       and DB0bam = 'S'
041000110411     c*//*                           or DB0gma = Ktab7V_C4
041010110411     c*//*                           or DB0gma = Ktab7V_C5
041020110502     c*//*                           or DB0gma = Ktab7V_R1
041030110502     c*//*                           or DB0gma = Ktab7V_NR)
041040110502     c*//*                         and  §7Rldv = 'S'
041050110502     c                   if            (§7Rldv = 'S'
041060110502     c                             and  $TabPXA2  = *on)
041070061106     c                             OR   $No_CF = *on
041080071114     c                             OR   $No_PI = *on
041090070306     c                             OR   $FIARN = *on
041100070315     c                             OR   $C_A_  = *on
041110080801     c                             OR   $ConsParz = *on
041120090302     c                             OR   $TabPXA   = *on
041130110411     c                             OR   $TabPXA2  = *on
041140031124     c                   exsr      ListaPMxA
041150130710      /free
041160130710             if  DB0fa4 = 'S';
041170130710               DB0fa4 = '1';
041180130710             endif;
041190130710      /end-free
041200031124e   1c                   endif
041210030515      *
041220030515      * > dati FIAR5
041230150304if  1c*//                if           %subst(DB0gva:1:1) = 'B'
041240150304     c*//                          or %subst(DB0gva:1:1) = 'O'
041250150304if  1c                   if                  DB0tc1      = 'S'
041260030717     c                             or        DB0tc2      = 'S'
041270030515     c                   exsr      ListaAR5
041280130710      /free
041290130710             if  DB0fa4 = 'S';
041300130710               DB0fa4 = '1';
041310130710             endif;
041320130710      /end-free
041330030515e   1c                   endif
041340030910      *
041350071015      * > Riferimento O.R.M.
041360071015sel 1c                   select
041370071015      * - > per ritiro contestuale alla consegna
041380100608w   1c                   when          (§7Rrc  = 'S'
041390071015     c                             and  §7Rldv = 'S')
041400030910     c                   exsr      StampaRCC
041410130710      /free
041420130710             if  DB0fa4 = 'S';
041430130710               DB0fa4 = '1';
041440130710             endif;
041450130710      /end-free
041460071015      * - > per ritiro porto franco prepagato
041470071015w   1c                   when           DB0tbo = '8'
041480071015     c                   exsr      StampaRPFPP
041490130710      /free
041500130710             if  DB0fa4 = 'S';
041510130710               DB0fa4 = '1';
041520130710             endif;
041530130710      /end-free
041540071015e   1c                   endsl
041550090924      /free
041560090924
041570110426         // >?Note per destinatario?
041580090924if  1    if  $BoxNxD;
041590090924           exsr  StampaNxD;
041600130710           if  DB0fa4 = 'S';
041610130710             DB0fa4 = '1';
041620130710           endif;
041630090924e   1    endif;
041640090924
041650090924      /end-free
041660040607      *
041670081013      * > DDT in LDV
041680081013      * - IN FUTURO: se al 1° tentativo di consegna
041690081013      *              (DB0FL1 = "D" - v. pgm FNLRS9R)
041700080917if  1c                   if             $FIAR8 =  *on
041710130315     c                             and  $E_AR8 =  *on
041720081013     c***-=»>                      and  DB0fl1 <> 'D'
041730040607     c                   exsr      StampaDDT
041740130710      /free
041750130710             if  DB0fa4 = 'S';
041760130710               DB0fa4 = '1';
041770130710             endif;
041780130710      /end-free
041790040607e   1c                   endif
041800130710      /free
041810130710
041820130710         if  DB0fa4 = 'S';
041830130710           exsr  ListaPMxAforz;
041840130710           //*in41 = *off;
041850130710           //*in42 = *off;
041860130710           //*in43 = *on;
041870130710           //*in44 = *off;
041880130710           //write  A4prtPOS;
041890130710         endif;
041900130710      /end-free
041910050510      *
041920050510     c     End_LdV       tag
041930030515      *
041940080703      * 2ª parte - copia per il destinatario (da lasciare)
041950130418     c                   eval      *in33 = *on
041960040607     c                   exsr      ChiusuraLDV
041970030515      *
041980030515      * Stampa dei 3 asterischi finali (se richiesta)
041990030515if  1c                   if        DB0ges = '*'
042000030515     c                   move      '***'         Pmsg3
042010130418     c                   eval      *in33 = *off
042020130418     c                   exsr      Posiz_Box
042030030515     c                   write     A4prtMSG
042040030515e   1c                   endif
042050030515      *
042060030515     c                   ENDSR
042070080604      /free
042080080604
042090080604       //--------------------------------------------------------------
042100140307       //?Posizionamento del box all'interno della pag. A4             ?
042110080604       //--------------------------------------------------------------
042120130418       BEGSR  Posiz_Box;
042130080704
042140080704         select;
042150080704
042160110426           // -?Richiesta stampa di 1 LdV x pagina?
042170110426           //  ?(LdV in modifica o con packing-list del mitente)?
042180080704           when  DB0fa4 = '1'   or   DB0fa4 = 'S';
042190080704             $LdVxP   = 1;
042200080918             $Len1LdV = $LenLdV2;
042210080704
042220110426           // -?Richiesta stampa di 2 LdV x pagina (ex A5)?
042230110426           //  ?(copia per il vettore & copia per il destinatario)?
042240080704           when  DB0fa4 = '2';
042250080704             $LdVxP   = 2;
042260080704             $Len1LdV = $LenLdV2;
042270080704
042280110426           // -?Richiesta stampa di 4 LdV x pagina?
042290110426           //  ?(solo copia per il vettore)?
042300080704           when  DB0fa4 = '4';
042310080704             $LdVxP   = 4;
042320080704             $Len1LdV = $LenLdV4;
042330080918
042340110426           // -?Richiesta stampa della sola packing-list?
042350110426           //  ?(solo copia per il vettore)?
042360080918           when  DB0fa4 = 'P';
042370080918             $LdVxP   = 2;
042380080924             $Len1LdV = c_Len2_inch;
042390080704
042400080704           other;
042410080704             reset $LdVxP;
042420080704             reset $Len1LdV;
042430080704
042440080704         endsl;
042450080704
042460110426         // -?Richiesta "ripartenza" da foglio A4 successivo?
042470130418         if DB0fl6 = *on  and  Not  *in33;
042480080704           clear  wNumLdV;
042490080704         endif;
042500080704
042510110426         // -?Calcolo posizione di partenza all'interno del foglio A4.?
042520110426         //  ?[Se 2ª LdV nella pagina, ma LdV per A4 (intero):?
042530110426         //   ?si salta alla pag. A4 successiva; la LdV in esame?
042540110426         //   ?dev'essere stampata in posizione iniziale dell'A4,?
042550110426         //   ?seguita dai suoi dati ulteriori.]?
042560130418         if  Not  *in33;
042570130418           wNumLdV  += 1;
042580130418           wSeqLdV   = %rem( wNumLdV : $LdVxP );
042590130418         endif;
042600080704
042610130422         // -?Stampa della copia per il vettore?
042620130422         IF  NOT  *in33;
042630130422
042640130422           select;
042650130422
042660130422             // -?Modulo A4 intero?
042670130422             when  NOT  *in69    and   DB0fa4 <> '4'
042680130422                                 and   DB0fa4 <> 'P';
042690130422               wSeqLdV = 1;
042700130422             when  DB0fa4 = '1'   or   DB0fa4 = 'S';
042710130422               wSeqLdV = 1;
042720130422
042730130422             // -?Calcolo della posizione x box all'interno della pagina "A4"?
042740130422             when  wSeqLdV = *zero   and
042750130422                   $LdVxP  = 2;
042760130422               wSeqLdV = 2;
042770130422             when  wSeqLdV = *zero   and
042780130422                   DB0fa4  = '4';
042790130422               wSeqLdV = 4;
042800130422             when  wSeqLdV = *zero   and
042810130422                   *in69;
042820130422               wSeqLdV  = 2;
042830130422
042840130422           endsl;
042850130422
042860130422         ELSE;
042870130422
042880130422           select;
042890130422
042900130422             // -?Modulo A5 o ricevuta (¼ di A4)?
042910130422             when  *in69;
042920130422
042930130422             // -?Modulo A4 intero - 1ª parte (alta)?
042940130422             when  CurLineA4 <  26   and   §PS1 < c_§PS1;
042950130422               wSeqLdV = 1;
042960130422
042970130422             // -?Modulo A4 intero - 2ª parte (passa)?
042980130422           //when  CurLineA4 <  78;   (in 1/9", ma sarà in 1/8"...)
042990130422             when  CurLineA4 <  70;
043000130422               wSeqLdV  = 2;
043010130422
043020130422             // -?Modulo A4 intero - superata 2ª parte?
043030130422             //  ?PER ORA?IMPOSSIBILE !!!?
043040130422             other;
043050130422               clear wMaxLine;
043060130422               exsr  Test_OF;
043070130422               wMaxLine = $MaxLine2;
043080130422               wSeqLdV  = 1;
043090130422
043100130422           endsl;
043110130422
043120130422         ENDIF;
043130080919
043140110426         // -?Impostaz. delle posizioni nella pagina in corso di stampa?
043150080919         wPosIni = ($Len1LdV * (wSeqLdV - 1));
043160130422         if  *in33;       // -?Stampa della copia per il destinatario?
043170130422           wPosIni += $LenLdV4;
043180130422         endif;
043190110426
043200120530         //// -?Impostaz. dimensioni per il logo Bartolini-BRT?
043210120530         //If  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
043220120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
043230120530         //  reset  c_Spessore;
043240120530         //  reset  c_DeltaSpig;
043250120530         //  reset  c_LunghMaxV;
043260120530         //  reset  c_LunghV;
043270120530         //  reset  c_LunghO;
043280120530         //  reset  c_XspaceX;
043290120530         //  reset  c_BOXspace;
043300120530         //Else;
043310120530         //  c_Spessore  = c_1_Spessore;
043320120530         //  c_DeltaSpig = c_1_DeltaSpig;
043330120530         //  c_LunghMaxV = c_1_LunghMaxV;
043340120530         //  c_LunghV    = c_1_LunghV;
043350120530         //  c_LunghO    = c_1_LunghO;
043360120530         //  c_XspaceX   = c_1_XspaceX;
043370120530         //  c_BOXspace  = c_1_BOXspace;
043380120530         //EndIf;
043390120530         // -?Dimensioni per il logo BRT già definite?
043400120530         //c_Spessore  = c_2_Spessore;
043410120530         //c_DeltaSpig = c_2_DeltaSpig;
043420120530         //c_LunghMaxV = c_2_LunghMaxV;
043430120530         //c_LunghV    = c_2_LunghV;
043440120530         //c_LunghO    = c_2_LunghO;
043450120530         //c_XspaceX   = c_2_XspaceX;
043460120530         //c_BOXspace  = c_2_BOXspace;
043470080704
043480110426         // -?Se richiesta la stampa della sola packing-list?
043490110426         //  ?NON si dovranno stampare i dati della LdV?
043500110426         //  ?(quindi NON servono i posizionamenti che seguono)?
043510080918         if  DB0fa4 = 'P';
043520080918           leavesr;
043530080918         endif;
043540080918
043550110426         // -?Posizionamento nel foglio A4 in base al n° LdV in stampa?
043560110426         //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
043570110426
043580130418         select;
043590130418           when  DB0fa4 = '4';
043600130418             *in41 = (wSeqLdV = 1);
043610130418             *in42 = (wSeqLdV = 2);
043620130418             *in43 = (wSeqLdV = 3);
043630130418             *in44 = (wSeqLdV = 4);
043640130418           when Not *in33;
043650130418             *in41 = (wSeqLdV = 1);
043660130418             *in42 = *off;
043670130418             *in43 = (wSeqLdV = 2);
043680130418             *in44 = *off;
043690130418           other;
043700130418             *in41 = *off;
043710130418             *in42 = (wSeqLdV = 1);
043720130418             *in43 = *off;
043730130418             *in44 = (wSeqLdV = 2);
043740130418         endsl;
043750080704         WRITE  A4prtPOS;
043760080704
043770110426         // -?Posizionamenti nella copia per il vettore?
043780110426         //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
043790080704
043800110426         // -?Box in copia per il vettore?
043810080704         §Box1As = wPosIni + c_PosBox1As;
043820140922         §Box1Ai = wPosIni + c_PosBox1Bs  + c_LineNarrow;
043830080704         §Box1Bs = wPosIni + c_PosBox1Bs;
043840080704         §Box1Bi = wPosIni + c_PosBox1Bi;
043850140922         §Box1Bj = wPosIni + c_PosLin1oD1 + c_LineNarrow;
043860080704         §Box1Cs = wPosIni + c_PosBox1Cs;
043870140922         §Box1Ci = wPosIni + c_PosBox1Ds  + c_LineWide;
043880080704         §Box1Ds = wPosIni + c_PosBox1Ds;
043890091119         §Box1Di = §Box1Bi;
043900081218
043910120530         //If  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
043920120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
043930120530         //  // -?Simil-Logo Bartolini?
043940120530         //  §PlblS  = §Box1As + 0.125;
043950120530         //  §PlblVS = 7.092;
043960120530         //  exsr  sr_SimilLogoBAR;
043970120530         //  // -?Simil-Logo BRT?
043980120530         //  §P2blS  = §PlblS;
043990120530         //  §P2blVS = §PlblVD + 0.030;
044000120530         //  exsr  sr_SimilLogoBRT;
044010120530         //Else;
044020120530         //  // -?Simil-Logo Bartolini?
044030120530         //  §PlblS  = §Box1As;
044040120530         //  §PlblVS = 7.128;
044050120530         //  exsr  sr_SimilLogoBART;
044060120530         //EndIf;
044070120530         // -?Simil-Logo BRT?
044080120530         §PlblS  = §Box1As;
044090120530         §PlblVS = 7.581;
044100120530         exsr  sr_SimilLogoBRT_;
044110080704
044120120530         // -?Costanti dati anagrafici BRT?
044130110426         §PC0    = wPosIni + c_PosTBar;
044140110426         §PC1    = wPosIni + c_PosTBa1;
044150120530         //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
044160120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
044170120530         //  §PC2    = wPosIni + c_PosTBa2;
044180120530         //  §PC3    = wPosIni + c_PosTBa3;
044190120530         //  §PC4    = wPosIni + c_PosTBa4;
044200120530         //  §PC5    = wPosIni + c_PosTBa5;
044210120530         //  §PC6    = wPosIni + c_PosTBa6;
044220120530         //else;
044230120530         //  §PC2    = wPosIni + c_PosTBa2x;
044240120530         //  §PC3    = wPosIni + c_PosTBa3x;
044250120530         //  §PC4    = wPosIni + c_PosTBa4x;
044260120530         //  §PC5    = wPosIni + c_PosTBa5x;
044270120530         //  §PC6    = wPosIni + c_PosTBa6x;
044280120530         //endif;
044290120530         §PC2    = wPosIni + c_PosTBa2x;
044300120530         §PC3    = wPosIni + c_PosTBa3x;
044310120530         §PC4    = wPosIni + c_PosTBa4x;
044320120530         §PC5    = wPosIni + c_PosTBa5x;
044330120530         §PC6    = wPosIni + c_PosTBa6x;
044340130729         §PC7    = §Box1Bi - 0.150;
044350110426
044360110426         // -?Linee orizzontali nel box in copia per il vettore?
044370080704         §Lo1S1  = wPosIni + c_PosLin1oS1;
044380080704         §Lo1S2  = wPosIni + c_PosLin1oS2;
044390080922       //§Lo1S3  = wPosIni + c_PosLin1oS3;
044400080704         §Lo1S4  = wPosIni + c_PosLin1oS4;
044410091119         §Lo1S5  = wPosIni + c_PosLin1oS5;
044420080704         §Lo1C1  = wPosIni + c_PosLin1oC1;
044430080704         §Lo1C2  = wPosIni + c_PosLin1oC2;
044440140917         §Lo1C3  = wPosIni + c_PosLin1oC3;
044450080704         §Lo1D1  = wPosIni + c_PosLin1oD1;
044460080704
044470110426         // -?Lunghezza linee verticali nel box in copia per il vett.?
044480080704         §LLv1A  = §Lo1S5  - §Lo1S1;
044490140922         //§LLv1B  = §Lo1S4  - §Box1Bs;
044500140922         //§LLv1C  = §Box1Di - §Box1Bs;
044510140923         //***§LLv1B  = §Lo1S4  - §Box1Bj + c_LineNarrow;
044520140923         §LLv1B  = §Lo1S4  - §Box1Bj;
044530140922         §LLv1C  = §Box1Di - §Box1Bj;
044540140917
044550140917         // -?Costante "BRTcode" & codice BRTcode?
044560140917         §Pk1    = wPosIni + c_PosKBRTc;
044570080704
044580110426         // -?Costante "Consegna tassativa"?
044590080704         §Pk2    = wPosIni + c_PosKTass;
044600080704
044610110426         // -?Costanti "(nome e cognome in stampatello)", "(firma)" e?
044620110426         //           ?"(data e ora)"?
044630080704         §Pk3    = §Box1Bi - 0.015;
044640080704
044650110426         // -?Costante con rif. alla legge in vigore per RCV?
044660080704         §Pk4    = wPosIni + c_PosKRCV;
044670080704
044680110426         // -?Costante "Bollo assolto in modo virtuale..."?
044690080704         §Pk5    = wPosIni + c_PosKBAV;
044700080704         §Pk6    = §Pk5 + 0.060;
044710121017
044720121017         // -?Partita IVA o Cod. Fiscale del cliente?
044730121017         §Pk7    = §Pk4 + 0.080;
044740080704
044750110426         // -?Costante "COPIA PER IL DESTINATARIO" in DDT "generico"?
044760080709         §Pos    = c_PosKCXD2;
044770080704
044780080704       ENDSR;
044790080605
044800080604      /end-free
044810110426
044820140307       //--------------------------------------------------------------
044830140307       //?Posizionamento del simil-logo Bartolini (unico)              ?
044840140307       //--------------------------------------------------------------
044850120530      *///copy gaitrasrc/srcTNSY,LogoBAR_R2
044860110426
044870140307       //--------------------------------------------------------------
044880140307       //?Posizionamento del simil-logo Bartolini (con BRT)            ?
044890140307       //--------------------------------------------------------------
044900120530      *///copy gaitrasrc/srcTNSY,LogoBAR3R2
044910110426
044920140307       //--------------------------------------------------------------
044930140307       //?Posizionamento del simil-logo BRT (con BARTOLINI)            ?
044940140307       //--------------------------------------------------------------
044950120530      *///copy gaitrasrc/srcTNSY,LogoBRT3R2
044960120530
044970140307       //--------------------------------------------------------------
044980140307       //?Posizionamento del simil-logo BRT (unico)                    ?
044990140307       //--------------------------------------------------------------
045000120530      /copy gaitrasrc/srcTNSY,LogoBRT_R2
045010110426
045020030515      *---------------------------------------------------------------*
045030050217      *?Gestione overflow                                            ?*
045040030515      *---------------------------------------------------------------*
045050030515     c     Test_OF       BEGSR
045060030515      *
045070030515      * Test raggiungimento della posizione limite
045080081003     c                   if          ((§PS1 < $LimOF   and   NOT *in45)
045090081003     c                             or (§PS1 < ($LimOF - c_Len1_inch)
045100081003     c                                     and *in45   and   NOT *in46)
045110081003     c                             or (§PS1 < ($LimOF + c_Len1_inch)
045120081003     c                                     and *in45   and   *in46))
045130080924     c                             and CurLineA4 < wMaxLine
045140080919     c                   leavesr
045150030515e   1c                   endif
045160030515      *
045170030515      * Stampa del box per dati da FIAR5
045180030515      * (se è già stata stampata almeno una riga di tale box)
045190060921if  1c                   if        $FIAR5 = *on  and  $First = *off
045200030911     c                   eval      §LO2   = §PS1 + 0.050
045210071008if  2c                   if        NOT *in52
045220030515     c                   write     A4prtAR53
045230071008x   2c                   else
045240071008     c                   eval      §PS2   = §LO2 - 0.500
045250071008     c                   write     A4prtAR54
045260071008e   2c                   endif
045270030515e   1c                   endif
045280041209      *
045290041209sel 1c                   select
045300040607      *
045310041209      * DDT KARNAK/TITANEDI/EDK
045320041209      * Stampa di logo, box, linee e testi fissi del DDT del mittente
045330040607      * (se si stà stampando tale DDT)
045340040607      * (ma NON per la parte "merce omaggio")
045350040607      * calcolando - prima - le posizioni iniziale e finale del box
045360041209w   1c                   when           $DDT_p    = *on
045370071008     c                             and  §AR8tsd   = 'K1'
045380040607     c                             and  $DDT_o    = *off
045390040608      * - Stampa parte finale DDT
045400040608if  2c                   if        $DDTlastPg = *off
045410041209      * - - Calcolo della posizione finale del box
045420040607     c                   eval      PCnv2  = '          '
045430040607     c                                    + '*** SEGUE ***'
045440040608     c                   exsr      Posiz_DK
045450040607     c                   write     A4prtDK4
045460040608x   2c                   else
045470041209      * - - Stampa dei dati in chiusura DDT (se ultima pagina)
045480040608      *     (la posizione finale del box verrà lì calcolata)
045490050217     c                   exsr      PrtDdtKARN_V2
045500040608e   2c                   endif
045510040607      * - Stampa logo, box, linee e testi fissi
045520081017     c                   if            §AR8cxd = *blanks
045530081017     c                             or  DB0fa4  = 'P'
045540050218     c                   write     A4prtDT0
045550080923     c                   endif
045560040607     c                   write     A4prtDK0
045570041019      *
045580050414      * DDT LOGISTICA/SAIT Abrasivi
045590050215      * Stampa di logo, box, linee e testi fissi del DDT del mittente
045600041019      * (se si stà stampando tale DDT)
045610041209w   1c                   when           $DDT_p    = *on
045620060627     c                             and  %subst(§AR8tsd:1:1) = 'L'
045630041021     c                   exsr      Posiz_DL
045640050217      * - - Chiusura box in fase di stampa dettaglio DDT
045650050217      *     (la stampa dei dati in chiusura DDT sarà solo nell'ultima
045660050318      *      pagina - prima dell'eventuale packing-list)
045670050318      *     La gestione del salto alla 1ª pagina della packing-list
045680050321      *     (AR8trk = 'Z1' & Page = *zeros) avviene nella subr.
045690050318      *     "PrtDdtLOG1_Z1" (vedi subr. "PrtPckLst_1").
045700081017     c                   if            §AR8cxd = *blanks
045710081017     c                             or  DB0fa4  = 'P'
045720050308     c                   write     A4prtDT0
045730050331     c                   endif
045740050318sel 2c                   select
045750050505      *     - si sta stampando il DDT Logistica o Sait
045760050318w   2c                   when      %subst(AR8trk:1:1) <> 'Z'
045770071008     c                             and %subst(AR8trk:1:1) <> 'S'
045780050218     c                   write     A4prtDL0A
045790050615      *     - si sta stampando la Packing-List
045800050318w   2c                   when      %subst(AR8trk:1:1) =  'Z'
045810050615     c                             and    Page        >  *zeros
045820050318     c                   write     A4prtDL0C
045830050606      *     - si sta stampando la Fattura (oltre 1ª pag.)
045840071008w   2c                   when      %subst(AR8trk:1:1) =  'S'
045850081003     c                             and    Page        >  *zeros
045860050609     c                   write     A4prtDL0E
045870050318e   2c                   endsl
045880050113      *
045890050113      * DDT ARCTE
045900050215      * Stampa di logo, box, linee e testi fissi del DDT del mittente
045910050113      * (se si stà stampando tale DDT)
045920050113w   1c                   when           $DDT_p    = *on
045930050113     c                             and  AR8tmd    = 'ARCT'
045940050114     c                   exsr      Posiz_AC
045950050217      * - - Chiusura box in fase di stampa dettaglio DDT
045960050217      *     (la stampa dei dati in chiusura DDT sarà solo nell'ultima
045970050217      *      pagina)
045980081017     c                   if            §AR8cxd = *blanks
045990081017     c                             or  DB0fa4  = 'P'
046000050331     c                   write     A4prtDT0
046010050331     c                   endif
046020050217     c                   write     A4prtAC0A
046030050215      *
046040050215      * DDT ESPRINET
046050050215      * Stampa di box, linee e testi fissi del DDT del mittente
046060050215      * (se si stà stampando tale DDT)
046070050215      * · Solo nella 1ª pagina vengono stampati il logo e la parte
046080050215      *        superiore del DDT.
046090050215      * · Solo nell'ultima pagina vene stampato il "fondo".
046100050215w   1c                   when           $DDT_p    = *on
046110060627     c                             and  §AR8tsd   = 'E1'
046120110405if 1bc                   if        %subst(SAVtrk:1:1 ) <> 'P'
046130110405     c                             or   %eof(FIAR801L)
046140050215      * - Stampa parte finale DDT
046150050215      * - - Calcolo delle posizioni di box & linee
046160050215     c                   exsr      Posiz_ES
046170050215      * - - Chiusura box in fase di stampa dettaglio DDT
046180050215      *     (la stampa dei dati in chiusura DDT sarà solo nell'ultima
046190050215      *      pagina)
046200050215if  2c***                if        %subst(AR8trk:1:1) <> 'P'
046210081017     c                   if            §AR8cxd = *blanks
046220081017     c                             or  DB0fa4  = 'P'
046230050331     c                   write     A4prtDT0
046240050331     c                   endif
046250050215     c                   write     A4prtES0A
046260050215e   2c***                endif
046270110405e  1bc                   endif
046280041209      *
046290041209      * DDT GENERICO con LOGO
046300050215      * Stampa di logo e box del DDT del mittente
046310041209      * (se si stà stampando tale DDT)
046320041209w   1c                   when           $DDT_p    = *on
046330041209     c                             and  §AR8tsd   = 'S2'
046340041209     *** * - Chiusura del box con i dati di mittente e destinatario
046350041209     *** *   (Se si stanno ancora stampando 'sti dati: bisogna calcolare
046360041209     *** *    la posizione di chiusura temporanea del box)
046370041209if  2***c                   if             AR8trk =  'B1'
046380041209     ***c                             or   AR8trk =  'B2'
046390041209     *** *   Impostazione della posizione finale per il box
046400041209     ***c                   exsr      Rep_§LO2
046410041209     ***c                   eval      §PB2   = §LO2 + 0.065
046420041209e   2***c                   endif
046430041209     *** *   Stampa del box
046440041209     ***c                   write     A4prtDB0
046450041209      * - Il logo è già stato SICURAMENTE stampato
046460041209     c***                exsr      Rep_§LO2
046470041209     c***                eval      §PB1   = §LO2
046480041209      *** Stampa del logo
046490041209     c***                write     A4prtDB0
046500041209      *
046510041209e   1c                   endsl
046520030515      *
046530030515      * Stampa dell'indicazione "Segue..." su pagina corrente
046540030515     c                   clear                   Pseg0
046550030515     c                   clear                   Pseg1
046560030515     c                   eval      Pseg9  = '  Segue...'
046570080919      /free
046580080923         select;
046590080923           when  *in45   and   NOT *in46;
046600080923             §PSA = 0.200;   // Per campo vuoto !
046610080923             §PSB = 0.200;   // Per campo vuoto !
046620080923             §PSC = c_Len2_inch - 0.050;
046630080923           when  *in45   and   *in46;
046640080923             §PSA = c_Len2_inch + 0.200;   // Per campo vuoto !
046650080923             §PSB = c_Len2_inch + 0.200;   // Per campo vuoto !
046660080924             §PSC = c_Len2_inch * 2 - 0.200;
046670080923           when  NOT *in45;
046680080923             §PSA = 0.400;
046690080923             §PSB = 0.700;
046700080923             §PSC = 8.750;
046710080923         endsl;
046720080919      /end-free
046730030515     c                   write     A4prtSEG
046740030515      *
046750031031      * Se sono ancora nella prima pagina =>
046760030515      * stampo anche i dati per il destinatario e box/barcode/ecc.
046770080604sel 1c                   select
046780080923      * > Se NON sto stampando un DDT del mittente
046790080922      *   (nel qual caso non stampo null'altro)
046800080923      *   => nulla
046810080919w   1c                   when      wPage  > 1
046820080923      * > Se sto stampando le sole copie per il vettore (4 in A4)
046830080923      *    o la relativa packing-list
046840080923      *   (nel qual caso non stampo null'altro)
046850080923      *   => nulla
046860081003w   1c                   when      DB0fa4 = '4'   or   DB0fa4 = 'P'
046870040607      * > Se NON è ristampa in arrivo per recapito c/assegni  e
046880040707      *   se NON sto stampando una LdV Poste
046890080703      *   se NON sto stampando (anche) un DDT del mittente
046900080703      *   se NON sto stampando le sole copie per il vettore (4 in A4)
046910080923      *   => posizionamento e stampa
046920120113w   1c                   when          $No_x_Cons =  *off
046930081013     c                             and ($FIAR8    =  *off)
046940081013     c***-=»>                       or  DB0fl1    =  'D')
046950080605      * - Posizionamento sul foglio A4 in base al n° pagina in stampa
046960130418     c                   eval      *in33 = *on
046970130418     c                   exsr      Posiz_Box
046980130418      * - Stampa 2ª parte della LdV (copia per il destinatario)
046990130418     c                   write     A4prtLDV
047000031031      * - Stampa "cornice" / barcode / righine varie in parte inferiore
047010130418     c                   write     A4prtBOX
047020110426     c*//                write     A4LogoBAR
047030110426      /free
047040120530         //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
047050120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
047060120530         //  // >?Simil-Logo Bartolini/Brt?
047070120530         //  write  A4LogoBR2;
047080120530         //  write  A4LogoDA2;
047090120530         //else;
047100120530         //  // >?Simil-Logo Bartolini?
047110120530         //  write  A4LogoBAR;
047120120530         //  write  A4LogoDA1;
047130120530         //endif;
047140120530         // >?Simil-Logo BRT?
047150120530         write  A4LogoBRT;
047160120530         write  A4LogoDA3;
047170110426      /end-free
047180120113      *//* > Se sto stampando una LdV Poste per recapito c/assegni
047190120113w   1c*//                   when          $No_x_Cons =  '2'
047200120113      *//* - Stampa 2ª parte "nulla" della LdV
047210130418     c*//                   eval      *in33 = *on
047220130418     c*//                   exsr      Posiz_Box
047230120113     c*//                   write     A4prtLDVB
047240080604e   1c                   endsl
047250080923      /free
047260110426         // -?Se richiesta la stampa della sola packing-list:?
047270110426         //  ?incremento del n° spedizione per calcolare correttam.?
047280110426         //  ?il n° di pagina iniziale per la spediz. successiva?
047290080925         if  DB0fa4 = 'P';
047300080925             wNumLdV += 1;
047310080925         endif;
047320080925
047330110426         // -?Reimpostazione degli indicatori per il posizionamento?
047340110426         //  ?all'interno del foglio A4?
047350080923         *in45 = (DB0fa4 = 'P');
047360080923         *in46 = (DB0fa4 = 'P'   and   %rem(wNumLdv : 2) = *zero);
047370080923      /end-free
047380030515      *
047390030515      * Salta a pagina successiva e indica "(Seguito)"
047400030515     c                   write     A4prtSP0
047410030515     c                   eval      wPage  = wPage + 1
047420080923      /free
047430081017         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
047440081017           write  A4prtNUL;
047450081017         endif;
047460081017
047470080923         IF  DB0fa4 <> 'P';
047480080922      /end-free
047490030515     c                   eval      Pseg0  = 'Spedizione '
047500110310     c                                    + %editc(DB0lnp : 'X') + ' '
047510110310     c                                    + %editc(DB0nrs : 'X') + ' '
047520110310     c                                    + %editc(DB0nsp : 'X')
047530030515     c                                    + ' del ' + %editc(Pdsp : 'Y')
047540030515     c                   eval      Pseg1  = '(Seguito)             Pag.'
047550030515     c                                    + %editc(wPage:'Z')
047560030515     c                   clear                   Pseg9
047570030515     c                   write     A4prtSEG
047580080922      /free
047590080923         ELSE;
047600081017
047610110426           // -?"Copia per il destinatario" già impostato in sr. "PrepPrt"?
047620081017           //if  §AR8cxd = *blank;
047630130729           //  Tcxd  = c_CopiaXdest;
047640130729           //  Tcxd2 = Tcxd;
047650081017           //endif;
047660081017
047670110426           // -?Estremi spedizione da impostare SOLO se si stampando?
047680110426           //  ?la sola packing-list (DB0FA4 = "P")?
047690081017           if  DB0fa4 = 'P';
047700101028             //Trsd = Prsd;   (già impostato nella subr. StampaPkgLst)?
047710081017             Trif = 'Spedizione '
047720081017                  + %editc(DB0lnp : 'X') + ' '
047730110310                  + %editc(DB0nrs : 'X') + ' '
047740110310                  + %editc(DB0nsp : 'X')
047750081017                  + ' del ' + %editc(Pdsp : 'Y')
047760081017                  + '        Pag.'
047770101028                  + %editc(wPage:'Z');
047780101028                //+ '        (Seguito)';
047790101028             //Tndc = Pndc;   (già impostato nella subr. StampaPkgLst)?
047800081017           endif;
047810080923
047820110426           // -?Verifica se la 2ª parte della packing-list sta nella?
047830110426           //  ?pag. attuale o se deve andare in quella successiva?
047840080923           wSeqLdV  = %rem( wNumLdV : $LdVxP );
047850080923           if  wSeqLdV = *zero   and   $LdVxP  = 2;
047860080923             wSeqLdV = 2;
047870080923           endif;
047880080923
047890110426           // -?Posizionamenti nella pagina in corso di stampa?
047900110426           //  ?(§POS = posizione verticale della costante "COPIA PER?
047910110426           //   ?IL DESTINATARIO" (con estremi spedizione e pag.)?
047920080923           wPosIni  = ($Len1LdV * (wSeqLdV - 1));
047930080924           if  wSeqLdV = 1;
047940080924             §Pos   = wPosIni + c_PosBox1As + 0.040;
047950090422             §Pos   = wPosIni + c_PosBox1As + 0.035;
047960080924           else;
047970080924             §Pos   = wPosIni + c_PosBox1As - 0.070;
047980080924           endif;
047990081017
048000081017           if  §AR8cxd = *blank   or   DB0fa4  = 'P';
048010081017             write A4prtDT0;
048020081017           endif;
048030080923
048040080923         ENDIF;
048050080922      /end-free
048060030515      *
048070030515      * Reimposta posizioni iniziali di stampa per linee
048080030515      *   (orizzontali-§LOn e verticali-§LVn) e campi (§PSn).
048090030515      *   Nelle pagine dalla 2ª in poi si parte da in alto:
048100030515      *   non vengono ri-stampati i 2 box "vettore" e "destinatario".
048110030515sel 1c                   select
048120120530      * - BRT
048130030515w   1c                   when      *in70
048140080711     c                   eval      §LO1   = c_§PS1
048150030515     c                   eval      §LO2   = §LO1
048160030515     c                   eval      §PS1   = §LO1
048170040607sel 2c                   select
048180050217      * - - con DDT del mittente KARNAK / TITANEDI / EDK
048190040607w   2c                   when           $DDT_p    = *on
048200071008     c                             and  §AR8tsd   = 'K1'
048210040608     c                   exsr      NewPageDK
048220050414      * - - con DDT del mittente LOGISTICA / SAIT ABRASIVI
048230041207w   2c                   when           $DDT_p    = *on
048240060627     c                             and  %subst(§AR8tsd:1:1) = 'L'
048250050321     c                   exsr      NewPageDL
048260050113      * - - con DDT del mittente ARCTE
048270050113w   2c                   when           $DDT_p    = *on
048280050113     c                             and  AR8tmd    = 'ARCT'
048290081002     c                   exsr      NewPageAC
048300050215      * - - con DDT del mittente ESPRINET
048310050215w   1c                   when           $DDT_p    = *on
048320060627     c                             and  §AR8tsd   = 'E1'
048330110405if  2c                   if        %subst(SAVtrk:1:1 ) <> 'P'
048340110405     c                             or   %eof(FIAR801L)
048350081002     c                   exsr      NewPageES
048360110405e   2c                   endif
048370041207      * - - con DDT generico (con marchio)
048380041207w   2c                   when           $DDT_p    = *on
048390041207     c                             and  §AR8tsd   = 'S2'
048400081017     c                   exsr      NewPageSTD
048410041207      * - - con DDT generico (senza marchio)
048420041207w   2c                   when           $DDT_p    = *on
048430041207     c*** ("scontato":)            and  §AR8tsd   = 'S1'
048440081017     c                   exsr      NewPageSTD
048450040607e   2c                   endsl
048460080922      * - DPD (ora in ex-A5)
048470080922w   1c***                when      *in71
048480030515      * - FedEx
048490030515w   1c                   when      *in72
048500030709     c                   eval      §LO1   = 1.600
048510030515     c                   eval      §LO2   = §LO1 + 0.425
048520030515     c                   eval      §LV1   = §LO1 + 0.300
048530030515     c                   eval      §LV2   = §LV1 + 0.070
048540030515     c                   write     A4prtFED1
048550030515do  2c                   dow       CurLineA4 < $MinLine1
048560030515     c                   write     A4prtNUL
048570030515e   2c                   enddo
048580030515     c                   write     A4prtFED2
048590030515     c                   eval      §LO1   = §LO2 - 0.250
048600030911     c                   eval      §PS1   = §LO1 + 0.150
048610040223      * - EuroExpress
048620040223w   1c                   when      *in73
048630030515      *
048640030515e   1c                   endsl
048650030515      *
048660030515      * N.B.: Per i SegnaColli non sequenziali non servono queste
048670030515      *       impostazioni: trattasi di stampa con salto riga -skipb-
048680030515      *       quindi sono già posizionato.
048690030515      *
048700080919     c                   ENDSR
048710030910      *
048720030910      *---------------------------------------------------------------*
048730050217      *?Reperimentoo della posizione (in pollici) in cui posizionarsi?*
048740050217      *?* per la stampa in base all'ultima riga stampata             ?*
048750030910      *---------------------------------------------------------------*
048760030910     c     Rep_§LO2      BEGSR
048770030910      *
048780030910sel 1c                   select
048790080825      *
048800080825      * Se ho stampato l'elenco dei segnacolli non sequenziali, devo
048810080825      *   "allineare" il posizionamento in pollici all'ultima riga
048820080825      *   stampata (in ottavi di pollice).
048830080825      *   =>   Num_ultima_riga_stampata / 8
048840080825w   1c                   when           DB0fns     = 'S'
048850080825     c                             and  DB0ncl     > 2
048860080825     c                             and  $A4_x_ScNS = *on
048870080825     c                             and  NOT  *in69
048880080825     c                   eval(h)   §LO2   = (CurLineA4 / 8) + 0,1
048890040607      *
048900040607      * Se ho stampato il DDT del mittente
048910080714      *   =>   Num_ultima_riga_stampata / 9
048920081013w   1c                   when           $FIAR8 =  *on
048930081013     c***-=»>                      and  DB0fl1 <> 'D'
048940080708     c                   eval(h)   §LO2   = (CurLineA4 / 9) + 0,1
048950030910      *
048960030910      * Se non ho stampato né elenco segnacolli non sequenziali né
048970040223      *   altri box, devo impostare comunque i dati per il posiziona-
048980030910      *   mento di questo primo box
048990030910w   1c                   when      §LO2   = *zeros
049000080624     c                   eval      §LO1   = 2.850
049010030910     c                   eval      §LO2   = §LO1
049020030910      *
049030030910e   1c                   endsl
049040030910      *
049050030910     c                   ENDSR
049060031124      *
049070031124      *---------------------------------------------------------------*
049080050217      *?Reperim. della posizione (in pollici) utlizzaile per la      ?*
049090080714      *?  stampa dei box in base all'ultima riga stampata in 1/9"    ?*
049100031124      *---------------------------------------------------------------*
049110031124     c     CalcRowPos    BEGSR
049120031124      *
049130031124      * Imposto le posizioni inziali per il primo dei box
049140080624     c                   select
049150080624      *
049160040607      * - in DDT del mittente: dopo eventuale elenco merce omaggio
049170120530      *   (come in LDV BRT)
049180120530      * - in LDV BRT: dopo eventuale elenco segnacolli non seq.
049190080624     c                   when      *in70
049200080624     c                   exsr      Rep_§LO2
049210080624      *
049220080922      * - in LDV DPD (ora in ex-A5)
049230080922     c***                when      *in71
049240080624      *
049250040607      * - in LDV FEDEX: posizione variabile
049260031124      *   (dipende dal numero dei segnacolli elencati: intanto
049270031124      *   posiziona la "base di partenza" sull'ultima posizione
049280031124      *   eventualmente usata per l'elenco dei segnacolli FedEx;
049290031124      *   poi si ri-calcoleranno in base al numero di righe stampate)
049300080624     c                   when      *in72
049310080624     c                   eval      §LO2   = §LO1 + 0.250
049320080624      *
049330080912      * - in LDV EUROEXPRESS: non più gestita
049340080624     c                   when      *in73
049350080912      *
049360080624     c                   endsl
049370080624      *
049380031124     c                   eval      §LO1   = §LO2
049390031124      *
049400031124     c                   ENDSR
049410130710      /free
049420130710       //--------------------------------------------------------------
049430130710       //?Forza un box "Promemoria per autotrasportatore"              ?
049440130710       //--------------------------------------------------------------
049450130710       BEGSR  ListaPMxAforz;
049460130710
049470130710         // -?Stampa testata?
049480130710         $First = *on;
049490130710         Par5   = '                    '
049500130710                + '                    '
049510130710                + '          '
049520130710                + C_PMxA;
049530130710         exsr  StampaAR5;
049540130710
049550130710         // -?Memorizza posizione iniziale per il box?
049560130710         //  ?(§PB1 sarà da reimpostare in §LO1 PRIMA della stampa del box)?
049570130710         §PB1   = §LO1;
049580130710
049590130710         // -?Stampa testo?
049600130710         §LO1   = §PS1 + 0.050;
049610130710         write  A4prtLO;
049620130710         §PS1   = §LO1;
049630130710         $First = 'G';
049640130710         Par5   = 'Testo FITTIZIO per FORZARE il modulo A4.';
049650130710         exsr  StampaAR5;
049660130710
049670130710         // -?Stampa (finale) box?
049680130710         //  ?(§PB1 è da reimpostare in §LO1: contiene il suo valore PRIMA?
049690130710         //  ?della stampa delle varie linee separatrici)?
049700130710         §LO1   = §PB1;
049710130710         §LO2   = §PS1 + 0.070;
049720130710         write  A4prtAR53;
049730130710
049740130710       ENDSR;
049750130710      /end-free
049760060524      *
049770060524      *---------------------------------------------------------------*
049780060524      *?Stampa box per riferimento al                                ?*
049790060524      *?DOCUMENTO di ACCOMPAGNAMENTO SEMPLIFICATO                    ?*
049800060524      *---------------------------------------------------------------*
049810060524     c     StampaDAS     BEGSR
049820060524      *
049830060524     c***                eval      Par5   = '*...+....1....+....2....+-
049840060524     c***                          ....3....+....4....+....5....+....6-
049850060524     c***                          ....+....7....+....8....+....9....+-
049860060524     c***                          ....A'
049870060524      *
049880060524      * - stampo testata
049890090302     ***c                   eval      $First = 'B'
049900090302     ***c                   eval      Par5   = C_PMxA
049910090302      /free
049920090302         $First = *on;
049930090302         Par5   = '                    '
049940090302                + '                    '
049950090302                + '          '
049960090302                + C_PMxA;
049970090302      /end-free
049980060524     c                   exsr      StampaAR5
049990090302      /free
050000090302
050010090302         // - memorizza posizione iniziale per il box
050020090302         //   (§PB1 sarà da reimpostare in §LO1 PRIMA della stampa del box)
050030090302         §PB1   = §LO1;
050040090302
050050090302         // - stampa linea separatrice
050060090302         §LO1   = §PS1 + 0.050;
050070090302         write  A4prtLO;
050080090302         §PS1   = §LO1;
050090090302      /end-free
050100060524      *
050110060524      * - stampo 1° testo
050120060524     c                   eval      $First = '2'
050130060524     c                   eval      Par5   = 'CONSEGNARE AL DESTINATARI-
050140060524     c                             O D.D.T. E D.A.S. (Documento di Acc-
050150060524     c                             ompagnamento Semplificato).'
050160060524     c                   exsr      StampaAR5
050170060524      *
050180060524      * - stampo 2° testo
050190061030     c*** (già fatto:)   eval      $First = *off
050200060524     c                   eval      Par5   = 'In caso di RIFIUTO compil-
050210060524     c                             are l''ultima pagina del D.A.S. ind-
050220060524     c                             icando:'
050230060524     c                   exsr      StampaAR5
050240060524     c                   eval      Par5   = 'a) nome Autotrasportatore-
050250060524     c                              nel campo "IL SOTTOSCRITTO ... INC-
050260060524     c                             ARICATO DEL TRASPORTO"'
050270060524     c                   exsr      StampaAR5
050280060524     c                   eval      Par5   = 'b) firma Autotrasportator-
050290060524     c                             e nel campo "FIRMA INCARICATO DEL T-
050300060524     c                             RASPORTO"'
050310060524     c                   exsr      StampaAR5
050320060524     c                   eval      Par5   = 'c) causale del rifiuto (b-
050330060524     c                             arrare "la merce non è conforme all-
050340060524     c                             ''ordine" o indicare altra motivazi-
050350060524     c                             one)'
050360060524     c                   exsr      StampaAR5
050370060524     c                   eval      Par5   = 'd) firma del destinatario-
050380060524     c                              nel campo "FIRMA DEL DESTINATARIO"'
050390060524     c                   exsr      StampaAR5
050400090302     *** *
050410090302     *** * - stampo box
050420090302     ***c                   eval      §LO2   = §PS1 + 0.050
050430090302     ***c                   write     A4prtAR53
050440090302      /free
050450090302         // - stampa box
050460090302         //   (§PB1 è da reimpostare in §LO1: contiene il suo valore PRIMA
050470090302         //   della stampa delle varie linee separatrici)
050480090302         §LO1   = §PB1;
050490090302         §LO2   = §PS1 + 0.070;
050500090302         write  A4prtAR53;
050510090302      /end-free
050520060524      *
050530060524     c                   ENDSR
050540031124      *
050550031124      *---------------------------------------------------------------*
050560050217      *?Prepara elenco per box "Promemoria per autotrasportatore"    ?*
050570031124      *---------------------------------------------------------------*
050580031124     c     ListaPMxA     BEGSR
050590031124      *
050600070308      * - stampo testata
050610070316     c                   eval      $First = *on
050620070319     c                   eval      Par5   = '                    '
050630070319     c                                    + '                    '
050640070319     c                                    + '          '
050650070319     c                                    + C_PMxA
050660031124     c                   exsr      StampaAR5
050670070315      *
050680070315      * - memorizzo posizione iniziale per il box
050690070320      *   (§PB1 sarà da reimpostare in §LO1 PRIMA della stampa del box)
050700070315     c                   eval      §PB1   = §LO1
050710031124      *
050720070308      * - stampo testo
050730081127      *
050740081212      /free
050750110411         // >?Mittente cliente in tab. "PXA"?
050760090302         IF  $TabPXA = *on;
050770090302
050780170605           clear  k05tntbe01;
050790090302           k_TBEcod = 'PXA';
050800090302           k_TBEke1 = %editc(DB0ccm : 'X');
050810100316           k_TBEke2 = '001            ';
050820090302           clear  k_TBElin;
050830090302           clear  k_TBEsif;
050840100316           //setll  %kds(k05tntbe01 : 3)  TNTBE000;  (già fatto)
050850090302           //reade  %kds(k05tntbe01 : 2)  TNTBE000;  (già fatto)
050860090302           clear  wx;
050870090302
050880100316           DoW  not  %eof(TNTBE01L)  and  TBEke2 <= '006            ';
050890090302
050900090302             if  TBEatb = *blank;
050910090302
050920110411               // -?Stampa della riga separatrice iniziale?
050930090302               if  wx = *zero;
050940090302                 §LO1   = §PS1 + 0.050;
050950090302                 write  A4prtLO;
050960090302                 §PS1   = §LO1;
050970090302                 $First = 'G';
050980090302               endif;
050990090302
051000110411               // -?Salta eventuali righe vuote?
051010090302               dow  %int( %trimr( TBEke2 ) ) > wx + 1;
051020090302                 clear  Par5;
051030090302                 exsr  StampaAR5;
051040090302                 wx += 1;
051050090302               enddo;
051060090302
051070110411               // -?Stampa la singola riga di testo?
051080090302               Par5   = TBEuni;
051090090302               exsr  StampaAR5;
051100090302               wx += 1;
051110090302
051120090302             endif;
051130090302
051140090302             reade  %kds(k05tntbe01 : 2)  TNTBE000;
051150090302
051160090302           EndDo;
051170090302
051180090302         ENDIF;
051190110411
051200110411         // >?Particolarità consegna in tab. "PXA"?
051210110411         IF  $TabPXA2 = *on;
051220110411
051230170605           clear  k05tntbe01;
051240110411           k_TBEcod = 'PXA';
051250110411           k_TBEke1 = DB0gma;
051260110411           k_TBEke2 = '001            ';
051270110411           clear  k_TBElin;
051280110411           clear  k_TBEsif;
051290110411           setll  %kds(k05tntbe01 : 3)  TNTBE000;
051300110411           reade  %kds(k05tntbe01 : 2)  TNTBE000;
051310110411           clear  wx;
051320110411
051330110411           DoW  not  %eof(TNTBE01L)  and  TBEke2 <= '006            ';
051340110411
051350110411             if  TBEatb = *blank;
051360110411
051370110411               // -?Stampa della riga separatrice iniziale?
051380110411               if  wx = *zero;
051390110411                 §LO1   = §PS1 + 0.050;
051400110411                 write  A4prtLO;
051410110411                 §PS1   = §LO1;
051420110411                 $First = 'G';
051430110411               endif;
051440110411
051450110411               // -?Salta eventuali righe vuote?
051460110411               dow  %int( %trimr( TBEke2 ) ) > wx + 1;
051470110411                 clear  Par5;
051480110411                 exsr  StampaAR5;
051490110411                 wx += 1;
051500110411               enddo;
051510110411
051520110411               // -?Stampa la singola riga di testo?
051530110411               Par5   = TBEuni;
051540110411               exsr  StampaAR5;
051550110411               wx += 1;
051560110411
051570110411             endif;
051580110411
051590110411             reade  %kds(k05tntbe01 : 2)  TNTBE000;
051600110411
051610110411           EndDo;
051620110411
051630110411         ENDIF;
051640090302
051650081212      /end-free
051660081127      *
051670031216sel 1c                   select
051680070306      * >
051690070306x   1c***                other
051700070306     c***                eval      $First = *off
051710070306     c***                clear                   Par5
051720070306     c***                exsr      StampaAR5
051730070306      *
051740070306e   1c                   endsl
051750061102      *
051760071114      * > Raccogliere Codice Fiscale e/o Partita Iva di ...
051770071114if  1c                   if             $No_CF = *on
051780071114     c                             or   $No_PI = *on
051790070320     c                   eval      §LO1   = §PS1 + 0.050
051800070320     c                   write     A4prtLO
051810070320     c                   eval      §PS1   = §LO1
051820070321     c                   eval      $First = 'G'
051830061102     c                   eval      Par5   = 'Spedizione n°'
051840061102     c                                    + %editw(DB0lnp:'0   ')
051850061102     c                                    + %editw(DB0nrs:'0  ')
051860061102     c                                    + ' '
051870061102     c                                    + %editc(DB0nsp:'Z')
051880061102     c                                    + ' del '
051890061102     c                                    + %editc(Pdsp:'Y')
051900061102     c                   exsr      StampaAR5
051910071114if  2c                   if        $No_PI = *on
051920071114     c*** (già fatto:)   eval      $First = 'G'
051930071114     c                   eval      Par5   = 'raccogliere +
051940071114     c                                      Partita I.v.a. di "'
051950071114     c                                    + %trim(wRSxCF)
051960071114     c                                    + '"'
051970071114     c                   exsr      StampaAR5
051980120504if  3c                   if        $No_CF = *off
051990120504     c*** (già fatto:)   eval      $First = 'G'
052000120504     c                   eval      Par5   = 'Codice Fiscale di "'
052010120504     c                                    + %trim(wRSxCF)
052020120504     c                                    + '": '
052030120504     c                                    + wB0cfi
052040120504     c                   exsr      StampaAR5
052050120504e   3c                   endif
052060071114e   2c                   endif
052070120503if  2c                   if        $No_CF = *on
052080120503     c*** (già fatto:)   eval      $First = 'G'
052090120503     c                   eval      Par5   = 'raccogliere +
052100120503     c                                      Codice Fiscale di "'
052110120503     c                                    + %trim(wRSxCF)
052120120503     c                                    + '"'
052130120503     c                   exsr      StampaAR5
052140120503e   2c                   endif
052150070306e   1c                   endif
052160070306      *
052170070306      * > Note bolla (dal file FIARN00F)
052180070306      *   (posizionamento e 1ª lettura già effettuate nella subr.
052190070306      *    "CheckFIARN")
052200070306if  1c                   if        $FIARN = *on
052210070320     c                   eval      §LO1   = §PS1 + 0.050
052220070320     c                   write     A4prtLO
052230070320     c                   eval      §PS1   = §LO1
052240070321     c                   eval      $First = 'G'
052250070319do  2c                   dou       %eof(FIARN11L)
052260070306     c                   eval      Par5   = ARNnob
052270070306     c                   exsr      StampaAR5
052280070319     c     K06ARN11      reade     FIARN000
052290070306e   2c                   enddo
052300070306e   1c                   endif
052310070319      *
052320070319      * > Consegne parziali precedenti (dal file FNART00F)
052330070406if  1c                   IF        $ConsParz     =  *on
052340101130     c*//(già fatto)?   exsr      SchConsParz
052350070320     c                   eval      §LO1   = §PS1 + 0.050
052360070320     c                   write     A4prtLO
052370070320     c                   eval      §PS1   = §LO1
052380070321     c                   eval      $First = 'G'
052390070319     c                   eval      xx     = 1
052400070319do  2c                   DoW            (xx      <= %elem($CP)
052410070319     c                             and  $CP(xx)  <  *all'9')
052420070319     c                   movel     $CP(xx)       Elem_CP
052430070319     c                   z-add     WCPdcm        w0080
052440070319     c     *iso          test(d)                 w0080                  31
052450070319if  3c                   if        NOT *in31
052460071025     c     *iso          movel     w0080         Data_EUR
052470071025     c     *eur          movel     Data_EUR      w0080
052480070319x   3c                   else
052490070319     c                   clear                   w0080
052500070319e   3c                   endif
052510140307 *...+*...1....+....2....+....3....+....4....+....5....+....6....+....7....+..
052520140307 Conse*nati colli 12345 in data 12/34/5678
052530140307 Conse*nati colli 12345 in data 12/34/5678 alle ore 12:34 - Firmatario: 123456
052540140307 Conse*nati colli 12345 il 12/34/5678 alle 12:34 a 1234567890123456789012345
052550140307 Conse*nati colli 12345 in data 12/34/5678 alle ore 12:34
052560140307      *                 Firmatario: 123456789012345678901234567890123456789012
052570070321     c                   eval      Par5   = 'Consegnati colli '
052580070319     c                                    + %editc(WCPncl:'Z')
052590070321     c                                    + ' in data '
052600070319     c                                    + %editc(w0080:'Y')
052610140307     c                   if        WCPocm > *zero
052620140307     c                   eval      Par5   = %trimr( Par5 )
052630140307     c                                    + ' alle ore '
052640140307     c                                    + %editw(WCPocm : '0 :  ')
052650140307     c                   endif
052660070319     c                   exsr      StampaAR5
052670140307     c                   if        WCPfir <> *blank
052680140307     c                   clear                   Par5
052690140307     c                   eval      %subst( Par5 : 24 ) = 'Firmatario: '
052700140307     c                                    + WCPfir
052710140307     c                   exsr      StampaAR5
052720140307     c                   endif
052730070319     c                   eval      xx     = xx + 1
052740070319e   2c                   EndDo
052750070319e   1c                   ENDIF
052760070321      *
052770070321      * > Reperita Comunicazione Anomalia
052780070321if  1c                   if        $C_A_  = *on
052790070321     c                   eval      §LO1   = §PS1 + 0.050
052800070321     c                   write     A4prtLO
052810070321     c                   eval      §PS1   = §LO1
052820070321     c                   eval      $First = 'G'
052830070321     c                   eval      Par5   = 'Presente Comunicazione +
052840070321     c                                                Anomalia'
052850070321     c                   exsr      StampaAR5
052860070321e   1c                   endif
052870031124      *
052880031124      * - stampo box
052890070320      *   (§PB1 è da reimpostare in §LO1: contiene il suo valore PRIMA
052900070320      *   della stampa delle varie linee separatrici)
052910070315     c                   eval      §LO1   = §PB1
052920070315     c                   eval      §LO2   = §PS1 + 0.070
052930031124     c                   write     A4prtAR53
052940031124      *
052950031124     c                   ENDSR
052960090924      /free
052970090924
052980090924       //--------------------------------------------------------------
052990090924       //?Prepara elenco per box "Note per il destinatario"            ?
053000090924       //--------------------------------------------------------------
053010090924       BEGSR  StampaNxD;
053020090924
053030090924         // - Stampa testata
053040090924         //$First = *on;
053050090924         //Par5   = '                    '
053060090924         //       + '                    '
053070090924         //       + '          '
053080090924         //       + c_NxD;
053090090924         //exsr  StampaAR5;
053100090924
053110090924         // - Memorizzazione posizione iniziale per il box
053120090924         //   (§PB1 sarà da reimpostare in §LO1 PRIMA della stampa del box)
053130090924         //§PB1 = §LO1;
053140090924
053150090924         // - Stampa della riga separatrice iniziale
053160090924         //§LO1   = §PS1 + 0.050;
053170090924         //write  A4prtLO;
053180090924         //§PS1   = §LO1;
053190090924
053200090924         // - Stampa la singola riga di testo
053210090925         $First = '1';
053220090924         Par5 = 'DECODER DIGITALE TERRESTRE OMAGGIO DA SKY PER AVER +
053230090924                 ADERITO ALL''OFFERTA';
053240090924         exsr  StampaAR5;
053250090924
053260090924         // - Memorizzazione posizione iniziale per il box
053270090924         //   (§PB1 sarà da reimpostare in §LO1 PRIMA della stampa del box)
053280090924         §PB1 = §LO1;
053290090924
053300090924         // - Stampa box
053310090924         //   (§PB1 è da reimpostare in §LO1: contiene il suo valore PRIMA
053320090924         //   della stampa delle varie linee separatrici)
053330090924         §LO1 = §PB1;
053340090925         §LO2 = §PS1 + 0.050;
053350090924         write  A4prtAR53;
053360090924
053370090924       ENDSR;
053380090924
053390090924      /end-free
053400030515      *
053410030515      *---------------------------------------------------------------*
053420050217      *?Prepara elenco per box - "Bancali da rendere al mittente"    ?*
053430150304      *?                         » NON PIÙ «                         ?*
053440050217      *?                       - "Dati per destinatario supermercato"?*
053450150304      *?                         » NON PIÙ «                         ?*
053460050217      *?                       - "Promemoria per autotrasportatore"  ?*
053470030515      *---------------------------------------------------------------*
053480030515     c     ListaAR5      BEGSR
053490030515      *
053500060921     c                   eval      $FIAR5 = *on
053510030515      *
053520030515      * Ripulisco i campi utilizzati in seguito
053530030515     c                   clear                   AR5trd
053540030515     c                   clear                   AR5dac
053550150304     c*//                clear                   dAR5ban
053560030717     c                   clear                   dAR5bnb
053570030903     c                   clear                   dAR5rcc
053580030515     c                   clear                   dAR5scr
053590030515     c                   reset                   $First
053600030515     c                   reset                   $FirstSCR
053610030515     c                   reset                   SAVtrd
053620030515      *
053630030515      * Posizionamento iniziale
053640040301if  1c                   if        DUTlpo = 'S'
053650040531     c     KLDV          setgt     FIAR531C
053660040301x   1c                   else
053670040607     c     KLDV          setgt     FIAR5000
053680040301e   1c                   endif
053690030515      *
053700030515      * Ciclo di lettura del file FIAR5
053710030515      *
053720030515do  1c                   DO        *Hival
053730030515      *
053740040607if  2c                   if        DUTlpo = 'S'
053750040531     c     KLDV          readpe    FIAR531C
053760040607x   2c                   else
053770040607     c     KLDV          readpe    FIAR5000
053780040607e   2c                   endif
053790040301      *
053800040301if  2c                   if           (DUTlpo =  'S'
053810040531     c                             and %eof(FIAR531C)
053820040301     c                             OR  DUTlpo <> 'S'
053830040301     c                             and %eof(FIAR501L))
053840030515     c                   leave
053850030515e   2c                   endif
053860030515if  2c                   if        AR5atb <> *blanks
053870030515     c                   iter
053880030515e   2c                   endif
053890030515      *
053900030515      * Controllo se devo stampare l'intestazione al nuovo box
053910030515if  2c                   if        AR5trd <> SAVtrd
053920030515      *   Stampa del precedente box (se stampata almeno una riga)
053930030910if  3c                   if             SAVtrd <> *hival
053940030910     c                             and  $First =  *off
053950030911     c                   eval      §LO2   = §PS1 + 0.050
053960030515     c                   write     A4prtAR53
053970030515e   3c                   endif
053980030515     c                   eval      $First =  *on
053990030515     c                   eval      SAVtrd =  AR5trd
054000030515e   2c                   endif
054010030515      *
054020030515sel 2c                   SELECT
054030150304      *//*
054040150304      *//* > Bancali da rendere al mittente
054050150304      *//*   (unico record, sequenzialmente prima dei rec. "SCR")
054060150304w   2c*//                WHEN      %subst(DB0gva:1:1) = 'B'
054070150304     c*//                          and    AR5trd      = 'BAN'
054080150304     c*//                movel     AR5uni        dAR5ban
054090150304      *//* - stampo testata
054100150304if  3c*//                if           (§AR5tba <> *zeros
054110150304     c*//                          or  §AR5tb2 <> *zeros)
054120150304     c*//                eval      Par5   = 'BANCALI DA RIPORTARE A MAGAZZINO E'
054130150304     c*//                                 + ' DA RENDERE AL MITTENTE           '
054140150304     c*//                exsr      StampaAR5
054150150304e   3c*//                endif
054160150304      *//* - stampo 1ª riga
054170150304if  3c*//                if        §AR5tba <> *zeros
054180150304     c*//                clear                   Par5
054190150304     c*//                clear                   Wtba
054200150304     c*//                eval      Wtba   = §AR5tba
054210150304     c*//                exsr      Tab_TBA
054220150304     c*//                eval      Par5   = §TBAdesc + 'N. '
054230150304     c*//                                 + %editc(§AR5nba : 'Z')
054240150304     c*//                exsr      StampaAR5
054250150304e   3c*//                endif
054260150304      *//* - stampo 2ª riga
054270150304if  3c*//                if        §AR5tb2 <> *zeros
054280150304     c*//                clear                   Par5
054290150304     c*//                clear                   Wtba
054300150304     c*//                eval      Wtba   = §AR5tb2
054310150304     c*//                exsr      Tab_TBA
054320150304     c*//                eval      Par5   = §TBAdesc + 'N. '
054330150304     c*//                                 + %editc(§AR5nb2 : 'Z')
054340150304     c*//                exsr      StampaAR5
054350150304e   3c*//                endif
054360150304      *//*
054370150304      *//* > Bancali da riportare a magazzino ("Oltre 20 colli")
054380150304      *//*   (unico record, sequenzialmente prima dei rec. "SCR",
054390150304      *//*    alternativo a "BAN" = bancali da rendere al mittente).
054400150304w   2c*//                WHEN      %subst(DB0gva:1:1) = 'O'
054410150304     c*//                          and    AR5trd      = 'BNB'
054420150304     c*//                movel     AR5uni        dAR5bnb
054430150304      *//* - stampo testata
054440150304if  3c*//                if           (§AR5btba <> *zeros
054450150304     c*//                          or  §AR5btb2 <> *zeros)
054460150304     c*//                eval      Par5   = 'BANCALI DA RIPORTARE A MAGAZZINO  '
054470150304     c*//                exsr      StampaAR5
054480150304e   3c*//                endif
054490150304      *//* - stampo 1ª riga
054500150304if  3c*//                if        §AR5btba <> *zeros
054510150304     c*//                clear                   Par5
054520150304     c*//                clear                   Wtba
054530150304     c*//                eval      Wtba   = §AR5btba
054540150304     c*//                exsr      Tab_TBA
054550150304     c*//                eval      Par5   = §TBAdesc + 'N. '
054560150304     c*//                                 + %editc(§AR5bnba : 'Z')
054570150304     c*//                exsr      StampaAR5
054580150304e   3c*//                endif
054590150304      *//* - stampo 2ª riga
054600150304if  3c*//                if        §AR5btb2 <> *zeros
054610150304     c*//                clear                   Par5
054620150304     c*//                clear                   Wtba
054630150304     c*//                eval      Wtba   = §AR5btb2
054640150304     c*//                exsr      Tab_TBA
054650150304     c*//                eval      Par5   = §TBAdesc + 'N. '
054660150304     c*//                                 + %editc(§AR5bnb2 : 'Z')
054670150304     c*//                exsr      StampaAR5
054680150304e   3c*//                endif
054690150304      *//* - stampo 3ª riga
054700150304if  3c*//                if        §AR5bcor <> *zeros
054710150304     c*//                clear                   Par5
054720150304     c*//                eval      Par5   = C_CoOr
054730150304     c*//                                 + %editc(§AR5bcor : 'K')
054740150304     c*//                exsr      StampaAR5
054750150304e   3c*//                endif
054760030515      *
054770030515      * > Destinatario supermercato
054780030515w   2c                   WHEN      (DB0tc1 = 'S' or DB0tc2 = 'S')
054790030515     c                             and AR5trd  = 'SCR'
054800030515     c                   movel     AR5uni        dAR5scr
054810030515      * - stampo testata
054820030515      *   (se subr. richiamata solo per verifica esistenza dati: esco)
054830030515if  3c                   if        $First =  *on
054840031124     c                   eval      Par5   = C_DeSM
054850030515     c                   exsr      StampaAR5
054860030515     c                   eval      $FirstSCR  = *off
054870030515e   3c                   endif
054880030515      * - stampo righe
054890030515     c                   clear                   Par5
054900030624     c                   eval      *in38 = *on
054910030515if  3c                   if           AR5dac <> SAVdac
054920030515     c                             or AR5orc <> SAVorc
054930030515     c                   eval      SAVdac = AR5dac
054940030515     c                   eval      SAVorc = AR5orc
054950030515      * - - giro data modifica
054960030515     c                   z-add     AR5dac        w0080
054970070315     c     *iso          test(d)                 w0080                  31
054980070315if  4c                   if        NOT *in31
054990071025     c     *iso          movel     w0080         Data_EUR
055000071025     c     *eur          movel     Data_EUR      w0080
055010070315x   4c                   else
055020070315     c                   clear                   w0080
055030070315e   4c                   endif
055040030515     c                   eval      Par5 = 'Modifica del '
055050030515     c                                  + %editc(w0080 : 'Y')
055060030515     c                                  + ' '
055070030515     c                                  + %editw(AR5orc : '  :  :  ')
055080030515     c                                  + ' '
055090040223     c                                  + §AR5Spru
055100030515     c                   exsr      StampaAR5
055110030515e   3c                   endif
055120040223     c                   eval      Par5 = §AR5Snom
055130030911     c                                  + '   Tel.: '
055140040223     c                                  + §AR5Stel
055150030911     c                   exsr      StampaAR5
055160040223     c                   eval      Par5 = §AR5Smot
055170030515     c                   exsr      StampaAR5
055180030624     c                   eval      *in38 = *off
055190031124      *
055200031124      * > Se destinatario assente inserire il plico in buca lettere
055210031124      *   (già stampato in box "Promemoria per autotrasportatore")
055220031124w   2c***                WHEN          DB0gma  = Ktab7V_CB
055230031124     c***                ...       ...
055240030903      *
055250030903      * > O.R.M. per ritiro contestuale alla consegna
055260030903      *   (unico record)
055270050510      *   Verrà stampato come ultimo box
055280050510      *   - prima del (eventuale) DDT del mittente
055290050510      *   - prima della copia per il destinatario
055300030910w   2c***                WHEN          DB0gma  = Ktab7V_RC
055310030910     c***                          and AR5trd  = 'RCC'
055320030903     c***                ...       ...
055330030515      *
055340030515e   2c                   ENDSL
055350030515      *
055360030515e   1c                   ENDDO
055370030515      *
055380030515      * Stampa del (ultimo) box (se stampata almeno una riga)
055390030515if  1c                   if        $First = *off
055400030911     c                   eval      §LO2   = §PS1 + 0.050
055410030515     c                   write     A4prtAR53
055420030515e   1c                   endif
055430030515      *
055440030515      * > Destinatario supermercato
055450030515      *   (stampa del box vuoto per memo se non già stampata almeno
055460030515      *   una riga)
055470030515if  1c                   IF        (DB0tc1 = 'S' or DB0tc2 = 'S')
055480030515     c                             and              $FirstSCR = *on
055490030515      * - stampo testata
055500030515     c                   eval      $First = *on
055510031124     c                   eval      Par5   = C_DeSM
055520030515     c                   exsr      StampaAR5
055530030515     c                   eval      $FirstSCR  = *off
055540030515      * - stampo 2 righe vuote
055550030515     c                   clear                   Par5
055560030515do  2c                   do        2
055570030515     c                   exsr      StampaAR5
055580030515e   2c                   enddo
055590030515      * - stampo box
055600030911     c                   eval      §LO2   = §PS1 + 0.050
055610030515     c                   write     A4prtAR53
055620030515e   1c                   ENDIF
055630070123      *
055640070123     c                   eval      $FIAR5 = *off
055650030515      *
055660030515     c                   ENDSR
055670030515      *
055680030515      *---------------------------------------------------------------*
055690050217      *?Stampa singola riga relativa a FIAR5                         ?*
055700030515      *---------------------------------------------------------------*
055710030515     c     StampaAR5     BEGSR
055720030515      *
055730030515      * Controllo se superata l'ultima riga
055740030515     c                   exsr      Test_OF
055750030515      *
055760030515      * Stampa del singolo record
055770030909sel 1c                   select
055780030909      *
055790030909w   1c                   when      $First = *off
055800030909      * - Incremento parametri per posizionamento nuova riga
055810030911     c                   eval      §PS1   = §PS1 + 0.150
055820030909     c                   write     A4prtAR52
055830030909      *
055840030909w   1c                   when      $First = *on
055850030515      * - Incremento parametri per posizionamento nuovo box
055860030515      *   e relativa intestazione
055870030911     c                   eval      §LO1   = §LO2 + 0.150
055880030911     c                   eval      §PS1   = §LO1 + 0.150
055890030515     c                   write     A4prtAR51
055900030515     c                   eval      $First = *off
055910060524      *
055920060524w   1c                   when      $First = '2'
055930060524      * - Incremento parametri per posizionamento nuova riga
055940060524      *   nel box "Promemoria per autotrasportatore"
055950060524     c                   eval      §PS1   = §PS1 + 0.150
055960060524     c                   write     A4prtAR51
055970060524     c                   eval      $First = *off
055980030909      *
055990030909w   1c                   when      $First = 'A'
056000030909      * - Incremento parametri per posizionamento nuovo box
056010030909      *   e relativa intestazione
056020030911     c                   eval      §LO1   = §LO2 + 0.150
056030030909     c                   eval      §PS1   = §LO1 + 0.200
056040030909     c                   write     A4prtAR5a
056050030909     c                   eval      $First = *off
056060150304      *//*
056070150304w   1c*//                when      $First = 'B'
056080150304      *//* - Incremento parametri per posizionamento nuovo box
056090150304      *//*   ed intestazione "Promemoria per autotrasportatore"
056100150304     c*//                eval      §LO1   = §LO2 + 0.150
056110150304     c*//                eval      §PS1   = §LO1 + 0.125
056120150304     c*//                write     A4prtAR5b
056130150304     c*//                eval      $First = 'C'
056140150304      *//*
056150150304w   1c*//                when      $First = 'C'
056160150304      *//* - Incremento parametri per posizionamento nuova riga
056170150304      *//*   nel box "Promemoria per autotrasportatore"
056180150304     c*//                eval      §PS1   = §PS1 + 0.250
056190150304     c*//                write     A4prtAR5c
056200070321      *
056210070321w   1c                   when      $First = 'G'
056220070321      * - Incremento parametri per posizionamento nuova riga
056230070321      *   nel box "Promemoria per autotrasportatore"
056240070321     c                   eval      §PS1   = §PS1 + 0.170
056250070321     c                   write     A4prtAR5g
056260030909      *
056270030909e   1c                   endsl
056280030515      *
056290030515     c                   ENDSR
056300030910      *
056310030910      *---------------------------------------------------------------*
056320050217      *?Stampa box "Ritiro Contestuale alla Consegna" da FIAR5/RCC   ?*
056330030910      *---------------------------------------------------------------*
056340030910     c     StampaRCC     BEGSR
056350030910      *
056360030910      * > O.R.M. per ritiro contestuale alla consegna
056370030910      *   (unico record)
056380031124      *   Verrà stampato come ultimo box (prima della copia per il
056390031124      *     destinatario)
056400030910      *
056410030910     c                   clear                   dAR5rcc
056420030910      *
056430030910     c                   eval      AR5trd = 'RCC'
056440040301if  1c                   if        DUTlpo = 'S'
056450040531     c     K05AR501      chain     FIAR531C
056460040301x   1c                   else
056470030910     c     K05AR501      chain     FIAR5000
056480040301e   1c                   endif
056490040301      *
056500071008if  1c                   if           (DUTlpo =  'S'
056510040531     c                             and %found(FIAR531C)
056520040301     c                             OR  DUTlpo <> 'S'
056530040301     c                             and %found(FIAR501L))
056540040301     c                             and AR5atb =  *blanks
056550030910     c                   movel     AR5uni        dAR5rcc
056560071015     c                   eval      ORMpoe = §AR5Rpoe
056570071015     c                   eval      ORMnsr = §AR5Rnsr
056580071015     c                   eval      ORMnor = §AR5Rnor
056590071015     c                   eval      ORMnrv = §AR5Rnrv
056600071015     c                   eval      Pbt2a  = 'RITIRO CONTESTUALE ALLA +
056610071015     c                                       CONSEGNA'
056620071015     c                   exsr      StampaORM
056630071015e   1c                   endif
056640071015      *
056650071015     c                   ENDSR
056660071015      *
056670071015      *---------------------------------------------------------------*
056680071015      *?Stampa box "Ritiro Porto Franco PrePagato"                   ?*
056690071015      *---------------------------------------------------------------*
056700071015     c     StampaRPFPP   BEGSR
056710071015      *
056720071015      * > O.R.M. per ritiro porto franco prepagato
056730071015      *   Verrà stampato come ultimo box (prima della copia per il
056740071015      *     destinatario)
056750071015      *
056760071015     c*** già fatto:     clear                   dsBL4M
056770071015     c*** già fatto:     exsr      RepAR4_M
056780071015      *
056790071015if  1c                   if             §B4poe  > *zeros
056800071015     c                             and  §B4nor  > *zeros
056810071015     c                   eval      ORMpoe = §B4poe
056820071015     c                   eval      ORMnsr = §B4nsr
056830071015     c                   eval      ORMnor = §B4nor
056840071015     c                   eval      ORMnrv = §B4nrv
056850071015     c                   eval      Pbt2a  = 'RITIRO PORTO FRANCO +
056860071015     c                                       PREPAGATO'
056870071015     c                   exsr      StampaORM
056880071015e   1c                   endif
056890071015      *
056900071015     c                   ENDSR
056910071015      *
056920071015      *---------------------------------------------------------------*
056930071015      *?Stampa dati O.R.M. in box "Ritiro Contestuale alla Consegna" ?*
056940071015      *?              o in quello "Ritiro Porto Franco Prepagato"    ?*
056950071015      *---------------------------------------------------------------*
056960071015     c     StampaORM     BEGSR
056970071015      *
056980071015     c                   eval      $FIAR5 = *on
056990071015      *
057000071015if  2c                   if        not %open(FNORM01L)
057010071015     c                   open      FNORM01L
057020071015e   2c                   endif
057030071015     c     K04ORM01      chain     FNORM000
057040071015      *
057050071015if  2c                   IF        %found(FNORM01L)
057060071015      *
057070071015      * Reperisco la posizione (in pollici) in cui posizionarmi
057080071015      *   per la stampa in base all'ultima riga stampata
057090071015      *   se non ho ancora stampato box
057100071015cas 3c     §LO2          caseq     *zeros        Rep_§LO2
057110071015e   3c                   endcs
057120071025      *
057130071015      * - stampo testata
057140071025      *   (con il n° O.R.M.)
057150071015     c                   eval      $First = 'A'
057160071015     c*** già impostato: eval      Pbt2a  = 'RITIRO CONTESTUALE ALLA +
057170071015     c***  "      "                          CONSEGNA'
057180071015     c                   eval      Pbt2b  = 'O.R.M. numero '
057190071015     c                                    + %editw(ORMpoe : '0   ')
057200071015     c                                    + ' '
057210071015     c                                    + %editw(ORMnsr : '0  ')
057220071015     c                                    + '  '
057230071015     c                                    + %editc(ORMnor : 'Z')
057240071015     c                                    + ' '
057250071015     c                                    + %editw(ORMnrv : '0  ')
057260071015     c                   exsr      StampaAR5
057270071025      *
057280071015      * - stampo i dati dell'O.R.M.
057290071015     c                   exsr      QUM_FnOrm
057300140411     c                   eval      Par5   = 'Quantità      : '
057310071015     c                                    + %trim($Qum(1)) + '    '
057320071015     c                                    + %trim($Qum(2)) + '    '
057330071015     c                                    + %trim($Qum(3))
057340071015     c                   exsr      StampaAR5
057350080228if  3c                   if        DB0tbo = '8'
057360140411     c                   eval      Par5   = 'Mittente      : '
057370080228     c                                    + ORMrsr
057380080228x   3c                   else
057390140411     c                   eval      Par5   = 'Destinatario  : '
057400071015     c                                    + ORMrsc
057410080228e   3c                   endif
057420071015     c                   exsr      StampaAR5
057430080228if  3c                   if        DB0tbo = '8'
057440140411     c                   eval      Par5   = '                '
057450080228     c                                    + ORMinr
057460080228x   3c                   else
057470140411     c                   eval      Par5   = '                '
057480071015     c                                    + ORMinc
057490080228e   3c                   endif
057500071015     c                   exsr      StampaAR5
057510080228if  3c                   if        DB0tbo = '8'
057520140411     c                   eval      Par5   = '                '
057530080228     c                                    + ORMcar + ' '
057540080228     c                                    + ORMlor + ' '
057550080228     c                                    + ORMprr + ' '
057560080228     c                                    + ORMnar
057570080228x   3c                   else
057580140411     c                   eval      Par5   = '                '
057590071015     c                                    + ORMcac + ' '
057600071015     c                                    + ORMloc + ' '
057610071015     c                                    + ORMprc + ' '
057620071015     c                                    + ORMnac
057630080228e   3c                   endif
057640071015     c                   exsr      StampaAR5
057650101022             // -?SE Ritiro Porto Franco Prepagato:?
057660080228if  3c                   if        DB0tbo = '8'
057670101022      /free
057680101022               // -?stampo il referente ed il relativo telefono?
057690101022               // -?(Comunque! Ci sono sempre!)?
057700140411               Par5   = 'Referente     : ' + ORMrer + '   '
057710101022                      + 'Telefono: '     + ORMter;
057720101022               exsr  StampaAR5;
057730101022      /end-free
057740140411     c                   eval      Par5   = 'Ordinante     : '
057750080228     c                                    + ORMrso
057760080228     c                   exsr      StampaAR5
057770140411     c                   eval      Par5   = 'Destinatario  : '
057780080303     c                                    + ORMrsc
057790080303     c                   exsr      StampaAR5
057800080228e   3c                   endif
057810071015      * - stampo le note
057820071015      *   (Comunque! Non solo "se ci sono". Al limite: scarto la 1ª.)
057830140411     c                   eval      Par5   = 'Note          : '
057840071015if  3c                   if        ORMno1 <> *blanks
057850071015     c                   eval      Par5   = %trim(Par5)  + ' '
057860071015     c                                    + ORMno1 + ' ' + ORMno2
057870071015x   3c                   else
057880071015     c                   eval      Par5   = %trim(Par5)  + ' '
057890071015     c                                    + ORMno2
057900071015e   3c                   endif
057910071015     c                   exsr      StampaAR5
057920140411      *
057930140411      * -?SE Ritiro Porto Franco Prepagato:?
057940140411if  3c                   IF        DB0tbo = '8'
057950140411      *
057960140411      * - stampo orari apertura (SE ci sono)
057970140411     c                   exsr      sr_OrariApert
057980071025      *
057990071025      * - stampo data & ora ritiro
058000071025     c                   z-add     ORMdar        w0080
058010071025     c     *iso          test(d)                 w0080                  31
058020071025if  4c                   if        NOT *in31
058030071025     c     *iso          movel     w0080         Data_EUR
058040071025     c     *eur          movel     Data_EUR      w0080
058050071025x   4c                   else
058060071025     c                   clear                   w0080
058070071025e   4c                   endif
058080140411     c                   eval      Par5   = 'Data ritiro   : '
058090071025     c                                    + %editc(w0080:'Y')
058100071025if  4c                   if        ORMorr > 1230
058110071025     c                   eval      Par5   = %trimr(Par5)
058120071025     c                                    + '          '
058130071025     c                                    + 'Ora ritiro: '
058140071025     c                                    + %editw(ORMorr:'0 :  ')
058150071025     c                   exsr      StampaAR5
058160071025x   4c                   else
058170071025     c                   exsr      StampaAR5
058180071025     c                   clear                   Par5
058190071025     c                   eval      %subst(Par5:50)
058200071025     c                                    = 'Ora ritiro: '
058210071025     c                                    + %editw(ORMorr:'0 :  ')
058220071025     c                   write     A4prtAR51
058230071025e   4c                   endif
058240140411      *
058250071025e   3c                   ENDIF
058260071025      *
058270071015      * - imposto codice a barre O.R.M.
058280071015     c                   movel     BarCodeORM    Pbco
058290071015      * - verifico se codice a barre da stampare
058300071015     c                   eval      *in52  =  (Pbco <> *zeros)
058310071025      *
058320071015      * - stampo box
058330071015     c                   eval      §LO2   = §PS1 + 0.050
058340071015if  3c                   if        NOT *in52
058350071015     c                   write     A4prtAR53
058360071015x   3c                   else
058370071025     c                   eval      §LV1   = §LO2 - §LO1
058380071015     c                   eval      §PS2   = §LO2 - 0.500
058390071015     c                   write     A4prtAR54
058400071015e   3c                   endif
058410071015      *
058420071015e   2c                   ENDIF
058430071015      *
058440071015     c                   eval      $FIAR5 = *off
058450071015      *
058460071015     c                   ENDSR
058470100513      /free
058480140411
058490140411       //--------------------------------------------------------------
058500140411       //?Reperimento, impostazione e stampa degli Orari di Apertura   ?
058510140411       //--------------------------------------------------------------
058520140411       BEGSR  sr_OrariApert;
058530140411
058540140411         clear  dOREorari;
058550140411
058560140411         // -?Recupero dati da estensione FNORE (trc "O ")?
058570140411         if  NOT  %open(FNORE01L);
058580140411           open  FNORE01L;
058590140411         endif;
058600140411         k_OREpoe = ORMpoe;
058610140411         k_OREnsr = ORMnsr;
058620140411         k_OREnor = ORMnor;
058630140411         k_OREnrv = ORMnrv;
058640140411         k_OREtrc = 'O ';
058650140411         chain  %kds(keyFNORE01)  FNORE000;
058660140411
058670140411         // -?...NON trovati => uscita dalla subroutine?
058680140411         if  Not  %found(FNORE01L);
058690140411           leavesr;
058700140411         endif;
058710140411
058720140411         // -?...Trovati => verifica degli orari?
058730140411         dOREorari = OREdati;
058740140411
058750140411         Select;
058760140411
058770140411           // -?C'è un solo orario "ALLE"?
058780140411           When  §OREoraMda = *zero  and  §OREoraMa > *zero  and
058790140411                 §OREoraPda = *zero  and  §OREoraPa = *zero;
058800140411             Par5 = 'Orari apertura: ' +
058810140411                    'Fino alle ' + %editw( §OREoraMa : ' 0:  ' );
058820140411           When  §OREoraMda = *zero  and  §OREoraMa = *zero  and
058830140411                 §OREoraPda = *zero  and  §OREoraPa > *zero;
058840140411             Par5 = 'Orari apertura: ' +
058850140411                    'Fino alle ' + %editw( §OREoraPa : ' 0:  ' );
058860140411
058870140411           // -?C'è un solo orario completo (per 1°)?
058880140411           When  §OREoraMda > *zero  and  §OREoraMa > *zero  and
058890140411                 §OREoraPda = *zero  and  §OREoraPa = *zero;
058900140411             Par5 = 'Orari apertura: ' +
058910140411                    %editw( §OREoraMda : ' 0:  ' ) + '  ' +
058920140411                    %editw( §OREoraMa  : ' 0:  ' );
058930140411
058940140411           // -?Ci sono due orari completi?
058950140411           When  §OREoraMda > *zero  and  §OREoraMa > *zero  and
058960140411                 §OREoraPda > *zero  and  §OREoraPa > *zero;
058970140411             Par5 = 'Orari apertura: ' +
058980140411                    %editw( §OREoraMda : ' 0:  ' ) + '  ' +
058990140411                    %editw( §OREoraMa  : ' 0:  ' ) + ' - ' +
059000140411                    %editw( §OREoraPda : ' 0:  ' ) + '  ' +
059010140411                    %editw( §OREoraPa  : ' 0:  ' );
059020140411
059030140411         EndSl;
059040140411
059050140411         exsr  StampaAR5;
059060140411
059070140411       ENDSR;
059080100513
059090100513       //--------------------------------------------------------------
059100100513       //?Verifica se presente ORM con preavviso mail                  ?
059110100513       //--------------------------------------------------------------
059120100513       BEGSR  sr_CheckORM_PrevvMail;
059130100514
059140100514         //clear  dOrm01               (già fatto)
059150100513
059160100513         // -?Reperimento num. ORM (se NON già reperito)?
059170100513         if  DB0tbo <> '8';
059180100513           exsr  RepAR4_M;
059190100513         endif;
059200100513
059210100513         // -?ORM inesistente: inutile proseguire?
059220100513         if  §B4poe = *zero    or   §B4nor = *zero;
059230100513           leavesr;
059240100513         endif;
059250100513
059260100513         // -?Verifica presenza O.R.M.?
059270100513         if  Not  %open(FNORM01L);
059280100513           open  FNORM01L;
059290100513         endif;
059300100513
059310100513         ORMpoe = §B4poe;
059320100513         ORMnsr = §B4nsr;
059330100513         ORMnor = §B4nor;
059340100513         ORMnrv = §B4nrv;
059350100513
059360100513         chain  K04ORM01  FNORM000;
059370100513
059380100513         // -?ORM non reperito: inutile proseguire?
059390100513         if  Not  %found(FNORM01L);
059400100513           leavesr;
059410100513         endif;
059420100513
059430100513         // -?Impostazione flag per stampa porto "ASSEGNATO"?
059440100513         dOrm01 = ORMflo;
059450100513         $PreavvORM = (§ORMpre = 'M');
059460100513
059470100513       ENDSR;
059480100513
059490100513      /end-free
059500030515      *
059510030515      *---------------------------------------------------------------*
059520050217      *?Stampa dati DPD                                              ?*
059530030515      *---------------------------------------------------------------*
059540030515     c     StampaDPD     BEGSR
059550030515      *
059560080922      * (EX) Bar-Code - non più stampato come codice a barre
059570030515      * (1' 12 char di Riferimento Mittente Alfanumerico - se numerici)
059580060626      *---
059590061030      * Il barcode DPD è stato variato ora può essere anche
059600061030      *   alfanumerico ed è lungo 14
059610061030      * Si trova sul file FIAR4 rcd 'I'
059620061030      * I controlli che faceva prima sono da cambiare
059630061030      *---
059640061030      *
059650060626      * nuovi controlli per nuovo barcode DPD
059660061030if  1c                   if        §B4Ipn <> *blanks
059670061030      * - controllo se vecchio o nuovo barcode
059680061030if  2c                   if        %subst(§B4Ipn:14:1) = *blanks
059690061030      * - - vecchio
059700061030     c                   eval      Pbcd2 = '01 ' + %subst(§B4Ipn:1:3) +
059710061030     c                                     ' '   + %subst(§B4Ipn:4:8)
059720061030      * - - nuovo
059730061030x   2c                   else
059740061030     c                   eval      Pbcd2 = '01 ' + %subst(§B4Ipn:1:4) +
059750061030     c                                     ' '   + %subst(§B4Ipn:5:10)
059760061030e   2c                   endif
059770061030e   1c                   endif
059780080922      /free
059790080922         // Impostazione data
059800080922         PdpA = 'Date ' + %editc(Pdsp:'Y');
059810080922
059820080922         // Impostazione numero colli
059830080922         PdpB = 'Parcel N. ' + %editc(Pncl:'Z');
059840080922
059850080922         // Impostazione numero parcel
059860080922         PdpC = §B4Ipn;
059870080922
059880080922         // Impostazione "costante" da stampare tra le Note
059890080922         PdpD = 'Controllare la corrispondenza';
059900080922         PdpE = 'del destinatario con l''indirizzo';
059910080922         PdpF = 'dell''etichetta DPD.';
059920080922
059930080922         // Impostaz. posizioni sulla copia per il vettore già stampata
059940080922         §DP1 = wPosIni + 1.100;
059950080922         §DP2 = §DP1 + 0.550;
059960080922         §DP3 = §DP2 + 0.120;
059970080922         §DP4 = §DP3 + 0.120;
059980080922         §DP5 = §DP4 + 0.120;
059990080922         §DP6 = §DP5 + 0.160;
060000080922         §DP7 = §DP6 + 0.130;
060010080922         §DP8 = §DP7 + 0.130;
060020080922      /end-free
060030080922      *
060040080922      * (inutile testare l'overflow perchè siamo in ex-A5)
060050080922     c                   write     A4prtDPD
060060030515      *
060070030515     c                   ENDSR
060080030515      *
060090030515      *---------------------------------------------------------------*
060100050217      *?Prepara elenco per box - Elenco "Package Tracking Number"    ?*
060110030515      *---------------------------------------------------------------*
060120030515     c     ListaFED      BEGSR
060130030515      *
060140030515      * Stampo il simbolo "FedEx" (sulla 1ª pagina):
060150030515      *   imposto le posizioni iniziali di stampa per le linee
060160030515      *   (orizzontali-§LOn e verticali-§LVn)
060170080912     c                   eval      §LO1   = 3.330
060180080912     c                   eval      §LO2   = §LO1 + 0.370
060190081023     c                   eval      §LV1   = §LO2 - 0.150
060200081023     c                   eval      §LV2   = §LO2 - 0.070
060210030515     c                   write     A4prtFED1
060220080818      *
060230080818      * Stampo le "costanti" di inizio lista
060240030515     c                   write     A4prtFED2
060250030515      *
060260030515      * Stampo elenco segnacolli FedEx
060270030709     c                   eval      §LO1   = §LO2 - 0.250
060280030515      * - per le bolle in partenza
060290030515if  1c                   IF           DB0tbo = 'P'
060300030515     c                             or DB0tbo = '8'
060310030515if  2c                   if        NOT %open(FNBLT01L)
060320030515     c                   open      FNBLT01L
060330030515e   2c                   endif
060340030515      * - per le bolle in arrivo
060350030515x   1c                   ELSE
060360070319if  2c                   if        NOT %open(FNART01L)
060370070319     c                   open      FNART01L
060380070319e   2c                   endif
060390030515e   1c                   ENDIF
060400070319if  1c                   if        NOT %open(FIARS01L)
060410070319     c                   open      FIARS01L
060420070319e   1c                   endif
060430030515     c                   eval      Wtrc = 'C'
060440030515     c                   clear                   Wnsc
060450030515     c                   clear                   Wawb
060460030515do  1c                   dou       Wawb = *blanks
060470030515     c                   exsr      StampaFED
060480030515e   1c                   enddo
060490051020      *
060500030515     c                   ENDSR
060510030515      *
060520030515      *---------------------------------------------------------------*
060530050217      *?Stampa singola riga relativa a FEDEX                         ?*
060540030515      *---------------------------------------------------------------*
060550030515     c     StampaFED     BEGSR
060560030515      *
060570030515      * Impostazione campi di stampa
060580030515     c                   clear                   Wawb
060590030515      * - Carico AWB FedEx
060600030515      * - - leggo segnacolli per bolle in partenza
060610030515if  1c                   IF           DB0tbo = 'P'
060620030515     c                             or DB0tbo = '8'
060630030515     c                   exsr      CarAWB_Blx
060640030515      * - - leggo segnacolli per bolle in arrivo
060650030515x   1c                   ELSE
060660030515     c                   exsr      CarAWB_Arx
060670030515e   1c                   ENDIF
060680030515     c     wAWB          cabeq     *blanks       End_FED
060690030515     c                   move      wAWB          Pptn
060700030515     c                   add       1             Psts
060710030515      *
060720030515      * Controllo se superata l'ultima riga
060730030515     c                   exsr      Test_OF
060740030709      *
060750030709      * Imposto le posizioni iniziali di stampa per le linee
060760030709      *   (orizzontali-§LOn e verticali-§LVn) e per i campi (§PSn)
060770030709     c                   eval      §LO1 = §LO1 + 0.250 - 0.008
060780030709     c                   eval      §LO2 = §LO1 + 0.250
060790030709     c                   eval      §LV1 = §LO1
060800030709     c                   eval      §LV2 = §LO2 - 0.100
060810030709     c                   eval      §PS1 = §LO1 + 0.200
060820030515      *
060830030515      * Stampa del singolo record
060840030515     c                   write     A4prtFED3
060850030515      *
060860030515     c     End_FED       ENDSR
060870030515      *
060880030515      *---------------------------------------------------------------*
060890051020      *?Reperimento AWB FedEx da file FNBLT e FIARS (bolle partenza) ?*
060900030515      *---------------------------------------------------------------*
060910030515     c     CarAWB_Blx    BEGSR
060920130215      *
060930130215      * Verifica se, al di là del codice bolla (vedi "$ "), esiste
060940130215      *   comunque una bolla mamma
060950130215     c                   exsr      sr_Mamma_FedEx
060960030515      *
060970030515      * Posizionamento
060980130215if  2c                   if        $Mamma = *off   and
060990130215     c                             $Mamma_FedEx = *off
061000040607     c     KLDVnsc       setgt     FNBLT000
061010070514x   2c                   else
061020070514     c     KLBLnsc       setgt     FNBLT000
061030070514e   2c                   endif
061040030515      *
061050030515      * Ciclo di lettura
061060030515do  1c                   DO        *hival
061070030515      *
061080130215if  2c                   if        $Mamma = *off   and
061090130215     c                             $Mamma_FedEx = *off
061100040607     c     KLDV          reade     FNBLT000
061110070514x   2c                   else
061120070514     c     KLBL          reade     FNBLT000
061130070514e   2c                   endif
061140030515if  2c                   if        %eof(FNBLT01L)
061150030515     c                   leave
061160030515e   2c                   endif
061170030515      * - per ogni segnacollo cerco l'AWB
061180060505     c     K05ARS01P     chain     FIARS000
061190060505if  2c                   if            %found(FIARS01L)
061200060505     c                             and %subst(ARSnot:1:1) >= *zeros
061210060505     c                   eval      wAWB = %subst(ARSnot:1:12)
061220060505     c                   eval      wNSC = ARSnsc
061230030515     c                   leave
061240030515e   2c                   endif
061250030515      *
061260030515e   1c                   ENDDO
061270030515      *
061280030515     c                   ENDSR
061290030515      *
061300030515      *---------------------------------------------------------------*
061310070319      *?Reperimento AWB FedEx da file FNART e FIARS (bolle arrivo)   ?*
061320030515      *---------------------------------------------------------------*
061330030515     c     CarAWB_Arx    BEGSR
061340130215      *
061350130215      * Verifica se, al di là del codice bolla (vedi "$ "), esiste
061360130215      *   comunque una bolla mamma
061370130215     c                   exsr      sr_Mamma_FedEx
061380030515      *
061390030515      * Posizionamento
061400130215if  2c                   if        $Mamma = *off   and
061410130215     c                             $Mamma_FedEx = *off
061420040607     c     KLDVnsc       setgt     FNART000
061430070514x   2c                   else
061440130215     c     KLBLnsc       setgt     FNART000
061450070514e   2c                   endif
061460030515      *
061470030515      * Ciclo di lettura
061480030515do  1c                   DO        *hival
061490030515      *
061500130215if  2c                   if        $Mamma = *off   and
061510130215     c                             $Mamma_FedEx = *off
061520040607     c     KLDV          reade     FNART000
061530070514x   2c                   else
061540070514     c     KLBL          reade     FNART000
061550070514e   2c                   endif
061560030515if  2c                   if        %eof(FNART01L)
061570030515     c                   leave
061580030515e   2c                   endif
061590030515      * - per ogni segnacollo cerco l'AWB
061600060505     c     K05ARS01A     chain     FIARS000
061610060505if  2c                   if            %found(FIARS01L)
061620030515     c                             and %subst(ARSnot:1:1) >= *zeros
061630030515     c                   eval      wAWB = %subst(ARSnot:1:12)
061640030515     c                   eval      wNSC = ARSnsc
061650030515     c                   leave
061660030515e   2c                   endif
061670030515      *
061680030515e   1c                   ENDDO
061690030515      *
061700030515     c                   ENDSR
061710130215      *
061720130215      /free
061730130215
061740130215       //--------------------------------------------------------------
061750130215       //?Verifica se la spedizione FedEx ha bolla mamma.              ?
061760130215       //--------------------------------------------------------------
061770130215       BEGSR  sr_Mamma_FedEx;
061780130215
061790130215         //$Mamma_FedEx = *off;          (già così)
061800130215
061810130215         If  Not $Mamma;
061820130215
061830130215           // -?Verifica se la spedizione ha comunque mamma?
061840130215           //  ?(al di là del codice bolla - vedi cod. "$ ")?
061850130215           if  not %open(FNLBL01L);
061860130215             open  FNLBL01L;
061870130215           endif;
061880130215           chain  (DB0aas : DB0lnp : DB0nrs : DB0nsp)  FNLBL000;
061890130215
061900130215           $Mamma_FedEx = %found(FNLBL01L);
061910130215
061920130215         EndIf;
061930130215
061940130215       ENDSR;
061950130215
061960130215      /end-free
061970040607      *
061980040607      *---------------------------------------------------------------*
061990080918      *?Stampa DDT / PACKING-LIST del mittente                       ?*
062000040607      *---------------------------------------------------------------*
062010040607     c     StampaDDT     BEGSR
062020130315      *
062030130315      * - Uscita in assenza del file FIAR801L
062040130315     c                   if        Not  $E_AR8
062050130315     c                   leavesr
062060130315     c                   endif
062070080825      *
062080080825      * Meglio togliere l'impostazione relativa ai segnacolli non seq.
062090080825      *   per far funzionare il 1° posizionamento nella subr. Rep_§LO2
062100080825     c                   reset                   $A4_x_ScNS
062110080918      /free
062120080923         *in45 = (DB0fa4 = 'P');
062130080923         *in46 = (DB0fa4 = 'P'   and   %rem(wNumLdv : 2) = *zero);
062140080918      /end-free
062150040607      *
062160040607      * Se già stampato qualcos'altro:
062170040607      *   forzo il salto alla pagina successiva
062180080926       // (SE NON si sta stampando SOLO una packing-list, nel qual caso
062190080926       //  il posizionamento nella pagina A4 verrà eseguito dagli
062200080926       //  indicatori 45 e 46)
062210080926if  1c                   if           (CurLineA4 > $MaxLine1
062220080926     c                             or  §LO2      > $LimOF1)
062230080926     c                             and NOT *in45
062240040607     c                   eval      CurLineA4 = wMaxLine
062250040607     c                   exsr      Test_OF
062260040607e   1c                   endif
062270040607      *
062280040607     c                   eval      $DDT_p = *on
062290040607      *
062300130315      /free
062310130315         // -?Posizionamento?
062320130315         clear  k06fiar801;
062330130315         k_AR8aas = DB0aas;
062340130315         k_AR8lnp = DB0lnp;
062350130315         k_AR8nrs = DB0nrs;
062360130315         k_AR8nsp = DB0nsp;
062370130315         k_AR8tmd = c_§PDF + '999';
062380130315         setgt  %kds( k06fiar801 : 5 )  FIAR8000;
062390130315      /end-free
062400130315      *
062410130315      * - Letura
062420040607do  1c                   dou          %eof(FIAR801L)
062430040607     c                             or AR8atb = *blanks
062440130308     c                             or AR8tmd <> c_§PDF
062450040607     c     KLDV          reade     FIAR8000
062460040607e   1c                   enddo
062470040607if  1c                   if        %eof(FIAR801L)
062480040607     c                   goto      NOprtDDT
062490040607e   1c                   endif
062500041207if  1c                   if        §AR8des   = *blanks
062510041207     c*** (già fatto:)   reset                   dAR8
062520041207     c                   reset                   TIBS02ds
062530041207     c                   eval      T02cod    = 'AR8'
062540041207     c                   movel(p)  AR8tmd        T02ke1
062550041207     c                   call      'TIBS02R'
062560041207     c                   parm                    KPJBA
062570041207     c                   parm                    TIBS02ds
062580041207if  2c                   if        T02err    = *blanks
062590041207     c                   movel     T02uni        dAR8
062600041207e   2c                   endif
062610041207e   1c                   endif
062620040607      *
062630040607sel 1c                   select
062640050217      * - Karnak o Titanedi o EDk (identiche)
062650080730w   1c                   when      §AR8tsd   = 'K1'
062660040607     c                   exsr      DDT_Karnak
062670050414      * - Logistica o Sait-Abrasivi
062680060627w   1c                   when      %subst(§AR8tsd:1:1) = 'L'
062690050218     c                   exsr      DDT_Logistica
062700050217      * - Arcte
062710050215w   1c                   when      AR8tmd    = 'ARCT'
062720050217     c                   exsr      DDT_Arcte
062730050214      * - Esprinet
062740060627w   1c                   when      §AR8tsd   = 'E1'
062750050214     c                   exsr      DDT_Esprinet
062760041207      * - dati generici (con logo)
062770041207w   1c                   when      §AR8tsd   = 'S2'
062780041207     c                   exsr      DDT_genericoL
062790041207      * - dati generici (senza logo)
062800041207w   1c***                when      §AR8tsd   = 'S1'
062810040607     c                   other
062820040607     c                   exsr      DDT_generico
062830040607e   1c                   endsl
062840040607      *
062850060921     c                   reset                   $FIAR8
062860040607     c                   reset                   $DDT_o
062870040607     c                   reset                   $DDT_p
062880040608     c                   reset                   $DDTlastPg
062890040608     c                   clear                   wOmaRowT
062900040608     c                   clear                   wOmaRowD
062910040607     c                   clear                   $AR8o1
062920040607      *
062930040607     c     NOprtDDT      ENDSR
062940040607      *
062950040607      *---------------------------------------------------------------*
062960050217      *?Stampa DDT del mittente KARNAK / TITANEDI                    ?*
062970040607      *---------------------------------------------------------------*
062980040607     c     DDT_Karnak    BEGSR
062990040607      *
063000080923      /free
063010080923         // Posizionamento iniziale all'interno del foglio A4
063020080923         write  A4prtSP1;
063030080923
063040080923         // Impostazione del nuovo limite x overflow
063050080923         select;
063060080923           when  NOT *in45;
063070080923             wMaxLine = $MaxLine3;
063080080923           when  *in45   and   NOT *in46;
063090080923             wMaxLine = $MaxLine3_pl1;
063100080923           when  *in45   and   *in46;
063110080923             wMaxLine = $MaxLine3_pl2;
063120080923         endsl;
063130080923      /end-free
063140040607      *
063150040607      * Impostazione testi fissi - - - - - - - - - - - - - - - - - - -*
063160040607      *
063170040607     c                   eval      PCtx1 = 'ATTENZIONE: VERIFICARE LA -
063180040607     c                             QUANTITA'' E L''INTEGRITA'' DEI CAR-
063190040607     c                             TONI. IN CASO DI DISCORDANZA RITIRA-
063200040607     c                             RE CON RISERVA.'
063210130118     c*//                eval      PCtx2 = 'La merce viagga a rischio -
063220130118     c*//                          e pericolo del committente anche se-
063230130118     c*//                           venduta franco destino. Trascorsi -
063240130118     c*//                          otto giorni dal ricevimento della m-
063250130118     c*//                          erce non si accettano reclami.   - -
063260130118     c*//                            IN CASO DI RITARDATO PAGAMENTO DE-
063270130118     c*//                          CORRERANNO GLI INTERESSI BANCARI IN-
063280130118     c*//                           VIGORE.'
063290130118     c*//                eval      PCtx3 = 'Per ogni vertenza l''acqui-
063300130118     c*//                          rente accetta la competenza della R-
063310130118     c*//                          epubblica di San Marino o in altern-
063320130118     c*//                          ativa del foro di Rimini.'
063330130118     c*//                eval      PCtx4 = 'NON SI ACCETTANO RESI E NO-
063340130118     c*//                          N SI EFFETTUANO SOSTITUZIONI SE NON-
063350130118     c*//                           AUTORIZZATI PER ISCRITTO DALLA NOS-
063360130118     c*//                          TRA DIREZIONE COMMERCIALE.'
063370130118     c*//                eval      PCtx5 = 'Quand''anche autorizzati, -
063380130118     c*//                          i resi verranno accettati e le sost-
063390130118     c*//                          ituzioni verranno effettuate solo d-
063400130118     c*//                          opo aver accertato il perfetto stat-
063410130118     c*//                          o nel nostro stabilimento della mer-
063420130118     c*//                          ce restituita.'
063430130118     c                   eval      PCtx2 = 'Eventuali reclami per mate-
063440130118     c                             riale mancante o danneggiato all''i-
063450130118     c                             nterno dei colli dovranno pervenire-
063460130118     c                              entro 8 (otto) giorni di calendari-
063470130118     c                             o dall''avvenuta consegna a MyO S.r-
063480130118     c                             .l. - Ufficio Resi - Via Santarcang-
063490130118     c                             iolese, 6 - 47825 Torriana (RN)'
063500130118     c                   eval      PCtx4 = 'Eventuali mancanze di coll-
063510130118     c                             i e/o avarie sono da contestare all-
063520130118     c                             ''atto della consegna con annotazio-
063530130118     c                             ne su lettera di vettura e DDT'
063540040709if  1c                   if        AR8tmd = 'TITA'
063550040709     c                   eval      PCtx6 = 'Depositario: K & K Logisti-
063560040709     c                             cs S.p.a. - via Santarcangiolese, 6-
063570040709     c                              - 47825 Torriana (RN) - P.I. 00953-
063580040709     c                             860400'
063590040709e   1c                   endif
063600080822      *
063610040607      * Conteggio delle righe "omaggio" - - - - - - - - - - - - - - -*
063620040607      *
063630040614     c                   clear                   xx
063640040607     c                   eval      SAVprr = AR8prr
063650040607     c     KLDV          setgt     FIAR8000
063660040607     c     KLDV          readpe    FIAR8000
063670040607do  1c                   dow       NOT %eof(FIAR801L)
063680040607sel 2c                   select
063690040607w   2c                   when      AR8atb <> *blanks
063700040611w   2c                   when      AR8trk =  'O2'
063710040608     c                   add       1             wOmaRowD
063720040611w   2c                   when      AR8trk =  'O1'
063730040608     c                   add       1             wOmaRowT
063740051110     c                   movel(p)  AR8uni        dAR8O1
063750040607     c                   add       1             xx
063760040607     c                   eval      $AR8o1(xx) =  §AR8O1a
063770040607x   2c                   other
063780040607     c                   leave
063790040607e   2c                   endsl
063800040607     c     KLDV          readpe    FIAR8000
063810040607e   1c                   enddo
063820040607      * - mi riposiziono
063830040607     c                   eval      AR8prr = SAVprr
063840040607     c     K06AR801      setll     FIAR8000
063850040607     c     K06AR801      reade     FIAR8000
063860040607      *
063870040607      * Reperimento dati, impostazione e stampa DDT mittente  - - - -*
063880040607      *
063890040607if  1c                   dow       NOT %eof(FIAR801L)
063900040607      *
063910040607sel 2c                   select
063920040607      * - Record annullato (non si stampa nulla)
063930040607w   2c                   when      AR8atb <> *blanks
063940040607      * - Dati di dettaglio: articolo, descrizione, quantità
063950040607w   2c                   when      AR8trk = 'C1'
063960050217     c                   exsr      PrtDdtKARN_C1
063970040607      * - Dati di dettaglio: testo per articoli non disponibili
063980040607w   2c                   when      AR8trk = 'C2'
063990050217     c                   exsr      PrtDdtKARN_C2
064000040607      * - Dati di dettaglio: descrizioni generiche (orario apertura)
064010040607w   2c                   when      AR8trk = 'C3'
064020050217     c                   exsr      PrtDdtKARN_C3
064030040607      * - Dati di testata: destinatario, cliente di fatturazione
064040040607w   2c                   when      AR8trk = 'T1'
064050050217     c                   exsr      PrtDdtKARN_T1
064060040607      * - Dati di testata: agente, numero ordine, estremi documento
064070040607w   2c                   when      AR8trk = 'T2'
064080050217     c                   exsr      PrtDdtKARN_T2
064090040607      * - Dati di fondo: mezzo e causale trasporto, colli, peso
064100040607w   2c                   when      AR8trk = 'V1'
064110050217     c                   exsr      PrtDdtKARN_V1
064120040607      * - Dati di fondo: vettore, annotazioni e note
064130040607w   2c                   when      AR8trk = 'V2'
064140050217     c                   exsr      PrtDdtKARN_V2
064150040607      * - Dati esterni: riga descrittiva con referente
064160040607w   2c                   when      AR8trk = 'L1'
064170050217     c                   exsr      PrtDdtKARN_L1
064180040607      * - Dati di omaggio: testo per articoli omaggiati
064190040607w   2c                   when      AR8trk = 'O1'
064200050217     c                   exsr      PrtDdtKARN_O1
064210040607      * - Dati di omaggio: articolo, descrizione, quantità
064220040607w   2c                   when      AR8trk = 'O2'
064230050217     c                   exsr      PrtDdtKARN_O2
064240040607e   2c                   endsl
064250040607      *
064260040607     c     KLDV          reade     FIAR8000
064270040607      *
064280040607e   1c                   enddo
064290040607      *
064300040607      * Reimpostazione del vecchio limite x overflow
064310040607     c                   eval      wMaxLine = $MaxLine2
064320040607      *
064330040607     c                   ENDSR
064340050214      *
064350040607      *---------------------------------------------------------------*
064360050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
064370050217      *?Dati di testata: destinatario, cliente di fatturazione       ?*
064380040607      *---------------------------------------------------------------*
064390050217     c     PrtDdtKARN_T1 BEGSR
064400040607      *
064410040607      * > logo
064420040607     c                   eval      §Logo  = 'LOGO' + AR8tmd
064430040607      *
064440051110     c                   movel(p)  AR8uni        dAR8T1
064450040607      *
064460040607      * > destinatario
064470040607     c                   eval      PCrsd  = §AR8t1a
064480040607     c                   eval      PCind  = §AR8t1b
064490040616if  1c                   if        §AR8t1c  > *zeros
064500040607     c                   eval      PCcad  = §AR8t1c
064510040616e   1c                   endif
064520040607     c                   eval      PClod  = §AR8t1d
064530040607     c                   eval      PCprd  = §AR8t1e
064540040607     c***                eval      PCnzd  = *blanks
064550040607      * > cliente di fatturazione
064560040607     c                   eval      PCrsf  = §AR8t1f
064570040607     c                   eval      PCinf  = §AR8t1g
064580040616if  1c                   if        §AR8t1h  > *zeros
064590040607     c                   eval      PCcaf  = §AR8t1h
064600040616e   1c                   endif
064610040607     c                   eval      PClof  = §AR8t1i
064620040607     c                   eval      PCprf  = §AR8t1j
064630040607     c***                eval      PCnzf  = *blanks
064640040607      *
064650040607     c                   ENDSR
064660040607      *
064670040607      *---------------------------------------------------------------*
064680050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
064690050217      *?Dati di testata: agente, ..., porto, ...                     ?*
064700040607      *---------------------------------------------------------------*
064710050217     c     PrtDdtKARN_T2 BEGSR
064720040607      *
064730051110     c                   movel(p)  AR8uni        dAR8T2
064740040607      *
064750040607      * > dati in testata DDT
064760040607      * - - codice agente
064770130122     c*//                eval      PCage  = §AR8t2a
064780040607      * - - numero e data ordine
064790040607     c                   eval      PCnro  = §AR8t2b
064800040607     c                   eval      PCdto  = §AR8t2c
064810040607      * - - riferimenti
064820040607     c                   eval      PCrif  = §AR8t2d
064830040607      * - - codice destinatario (dal mittente)
064840040611     c                   eval      PCcli  = §AR8t2e
064850040607      * - - numero e data documento (DDT)
064860040607     c                   eval      PCnrd  = §AR8t2f
064870040607     c                   eval      PCdtd  = §AR8t2g
064880040607      * - - numero pagina documento originale (qui ricalcolato)
064890040607     c***                eval      Page   = §AR8t2h
064900040607      * - - porto
064910040607     c                   eval      PCpto  = §AR8t2i
064920040607      * - - condizioni di pagamento
064930040611     c                   eval      PCpag  = §AR8t2j
064940040607     c                                    + ' '
064950040607     c                                    + %trim(§AR8t2k)
064960040607      * - - tipologia spedizione
064970040607     c                   eval      PCtsp  = §AR8t2l
064980040607      *
064990040607      * > Stampa testata DDT - - - - - - - - - - - - - - - - - - - - -*
065000040607      *
065010040607      *   - OverFlow impossibile qui! -
065020040607     c                   write     A4prtDK1
065030040607      *
065040040607     c                   ENDSR
065050040607      *
065060040607      *---------------------------------------------------------------*
065070050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
065080050217      *?Dati di dettaglio: articolo, descrizione, quantità           ?*
065090040607      *---------------------------------------------------------------*
065100050217     c     PrtDdtKARN_C1 BEGSR
065110040607      *
065120040607     c                   clear                   A4prtDK2
065130051110     c                   movel(p)  AR8uni        dAR8C1
065140040607      *
065150040607      * > dati in dettaglio DDT
065160040607      * - - codice prodotto
065170040607     c                   eval      PCcpr  = §AR8c1a
065180040607      * - - descrizione prodotto
065190040607     c                   eval      PCdpr  = §AR8c1b
065200040607      * - - quantità
065210040607     c                   eval      PCqta  = '     ' + §AR8c1c
065220040607      * - - note
065230040607     c***                eval      PCnot  = *blanks
065240040607      *
065250040607      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
065260040607      *
065270040607     c                   exsr      Test_OF
065280040607     c                   write     A4prtDK2
065290040607      *
065300040607     c                   ENDSR
065310040607      *
065320040607      *---------------------------------------------------------------*
065330050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
065340050217      *?Dati di dettaglio: testo per articoli non disponibili        ?*
065350040607      *---------------------------------------------------------------*
065360050217     c     PrtDdtKARN_C2 BEGSR
065370040607      *
065380040607     c                   clear                   A4prtDK3
065390051110     c                   movel(p)  AR8uni        dAR8C2
065400040607      *
065410040607      * > dati in note DDT
065420040607      * - - riga descrittiva
065430040607     c                   eval      PCtxt  = §AR8c2a
065440040607      *
065450040607      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
065460040607      *
065470040607     c                   exsr      Test_OF
065480040607     c                   write     A4prtDK3
065490040607      *
065500040607     c                   ENDSR
065510040607      *
065520040607      *---------------------------------------------------------------*
065530050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
065540050217      *?Dati di dettaglio: descrizioni generiche (orario apertura)   ?*
065550040607      *---------------------------------------------------------------*
065560050217     c     PrtDdtKARN_C3 BEGSR
065570040607      *
065580040607     c                   clear                   A4prtDK2
065590051110     c                   movel(p)  AR8uni        dAR8C3
065600040607      *
065610040607      * > dati in note DDT
065620040607      * - - riga descrittiva
065630040607     c                   eval      PCdpr  = §AR8c3a
065640040607      *
065650040607      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
065660040607      *
065670040607     c                   exsr      Test_OF
065680040607     c                   write     A4prtDK2
065690040607      *
065700040607     c                   ENDSR
065710040607      *
065720040607      *---------------------------------------------------------------*
065730050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
065740050217      *?Dati di fondo: mezzo e causale trasporto, colli, peso        ?*
065750040607      *---------------------------------------------------------------*
065760050217     c     PrtDdtKARN_V1 BEGSR
065770040607      *
065780051110     c                   movel(p)  AR8uni        dAR8V1
065790040607      *
065800040607      * Controllo se ci saranno da stampare tante righe omaggio da
065810040607      *   chiudere in pagina successiva:
065820040608      *   i campi del box finale del DDT (in totale 9 righe) verranno
065830040607      *   riempiti solo nella ultima pagina.
065840040607      *
065850080924      /free
065860080924         select;
065870080924           when  NOT *in45;
065880080924      /end-free
065890040608     c                   eval      $DDTlastPg = ($MaxLine2
065900040713     c                                        +  1
065910040608     c                                        -  CurLineA4
065920040608     c                                        -  $BoxRow1
065930040608     c                                        -  wOmaRowT
065940040608     c                                        >= wOmaRowD)
065950080924      /free
065960080924           when  *in45   and   NOT *in46;
065970110406             $DDTlastPg = ($MaxLine2_pl1k
065980080924                           + 1 - CurLineA4 - $BoxRow1 - wOmaRowT
065990080924                           >= wOmaRowD);
066000080924           when  *in45   and   *in46;
066010110406             $DDTlastPg = ($MaxLine2_pl2k
066020080924                           + 1 - CurLineA4 - $BoxRow1 - wOmaRowT
066030080924                           >= wOmaRowD);
066040080924         endsl;
066050080924      /end-free
066060040607      *
066070040607      * > dati in chiusura DDT
066080040607      *   Per ora imposto (ho già impostato) solo la DS "dAR8V1".
066090040607      *   I corrispondenti campi di stampa li impostero nella subr.
066100050217      *     "PrtDdtKARN_V2".
066110040607      *
066120040607     c                   ENDSR
066130040607      *
066140040607      *---------------------------------------------------------------*
066150050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
066160050217      *?Dati di fondo: vettore, annotazioni e note                   ?*
066170040607      *---------------------------------------------------------------*
066180050217     c     PrtDdtKARN_V2 BEGSR
066190040607      *
066200051110     c                   movel(p)  AR8uni        dAR8V2
066210040618      *
066220040618      * > dati in chiusura DDT
066230040618      *
066240040607      * Ho già controllato se ci saranno da stampare tante righe
066250040607      *   omaggio da chiudere in pagina successiva
066260040608if  1c                   if        $DDTlastPg = *off
066270040607     c                   eval      PCnv2  = '          '
066280040607     c                                    + '*** SEGUE ***'
066290040618x   1c                   else
066300050217     c                   exsr      PrtDdtKARNaV2
066310040607e   1c                   endif
066320040607      *
066330040607      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
066340040608      *
066350050217     c                   exsr      PrtDdtKARNbV2
066360040607      *
066370040607      * > Impostazione nuovo limite di OverFlow  - - - - - - - - - - -*
066380080923      /free
066390080923         select;
066400080923           when  NOT *in45;
066410080923             wMaxLine = $MaxLine2;
066420080923           when  *in45   and   NOT *in46;
066430080923             wMaxLine = $MaxLine2_pl1;
066440080923           when  *in45   and   *in46;
066450080923             wMaxLine = $MaxLine2_pl2;
066460080923         endsl;
066470080923      /end-free
066480040607      *
066490040608      * N.B.: la copia per il destinatario della LDV deve comparire
066500040608      *       nell'ultima pagina del DDT per il cliente (vedi subr.
066510040608      *       "ChiusuraLDV")
066520040607      *
066530040607     c                   ENDSR
066540040618      *
066550040618      *---------------------------------------------------------------*
066560040618      *
066570050217     c     PrtDdtKARNaV2 BEGSR
066580040618      *
066590040618      * > dati in chiusura DDT
066600040618      *
066610040618      * > > da dAR8V1
066620040618      * - - mezzo di trasporto
066630040618     c                   movel     §AR8v1a       PCmtr
066640040618      * - - causale trasporto
066650040618     c                   eval      PCctr  = §AR8v1b
066660040618      * - - numero colli
066670040618     c                   eval      PCncl  = §AR8v1c
066680040618      * - - peso in kg
066690040618     c                   eval      PCpkg  = §AR8v1d
066700040618      * - - aspetto esteriore dei beni
066710040618     c                   eval      PCaeb  = §AR8v1e
066720040618      *
066730040618      * > > da dAR8V2
066740120530      * - - dati vettore: · ragione sociale (BRT S.p.A.)
066750040618      *                   · indirizzo
066760040618      *                   · c.a.p./città/provincia
066770040618      *     => NON con i dati che manda il mittente !!!
066780040618      *        ...Vuoi mai che si sbagli ad inviarci della roba !!!
066790040618     c***                eval      PCvet  = %trim(§AR8v2a)
066800040618     c***                                 + ' - '
066810040618     c***                                 + %trim(§AR8v2b)
066820040618     c***                                 + ' - '
066830040618     c***                                 + %trim(§AR8v2c)
066840120530     c*//                if        (DB0dft > *zero       and
066850120530     c*//                           DB0dft > §vpoBRTdta)  OR
066860120530     c*//                          (DB0dft = *zero       and
066870120530     c*//                           Wdsp   > §vpoBRTdta)
066880110426     c                   eval      PCvet  = $BRT
066890110426     c                                    + ' - '
066900110426     c                                    + $Intest2
066910110426     c                                    + ' (BO)'
066920120530     c*//                else
066930120530     c*//                eval      PCvet  = $Bartolini
066940120530     c*//                                 + ' - '
066950120530     c*//                                 + $Intest2
066960120530     c*//                                 + ' (BO)'
066970120530     c*//                endif
066980040618      * - - data ritiro
066990040618     c*** non stampo:    eval      PCdtv  = §AR8v2d
067000040618      * - - orario ritiro
067010040618     c*** non stampo:    eval      PChmv  = §AR8v2e
067020040618      * - - annotazioni / variazioni
067030040618     c                   eval      PCvea  = §AR8v2f
067040040618      * - - note
067050040618     c                   eval      PCnv1  = §AR8v2g
067060040618     c                   eval      PCnv2  = §AR8v2h
067070040618      *
067080040618     c                   ENDSR
067090040618      *
067100040618      *---------------------------------------------------------------*
067110040618      *
067120050217     c     PrtDdtKARNbV2 BEGSR
067130040618      *
067140040618      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
067150040618      *
067160040618      * - - Calcolo la posizione finale del box
067170040618     c                   exsr      Posiz_DK
067180040618      *
067190040618      * - - Stampa parte finale DDT
067200040618     c                   write     A4prtDK4
067210040618      *
067220040618      * > Stampa logo, box, linee e testi fissi  - - - - - - - - - - -*
067230081017     c                   if            §AR8cxd = *blanks
067240081017     c                             or  DB0fa4  = 'P'
067250050331     c                   write     A4prtDT0
067260050331     c                   endif
067270040618     c                   write     A4prtDK0
067280040618      *
067290040618     c                   ENDSR
067300040607      *
067310040607      *---------------------------------------------------------------*
067320050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
067330050217      *?Dati esterni: riga descrittiva con referente                 ?*
067340040607      *---------------------------------------------------------------*
067350050217     c     PrtDdtKARN_L1 BEGSR
067360040607      *
067370051110     c                   movel(p)  AR8uni        dAR8L1
067380040607      *
067390040607      * > righe descrittive esterne al DDT
067400040607      * - - riga descrittiva
067410040607     c                   eval      PCtxx  = §AR8l1a
067420040607      *
067430040607      * > Stampa riga testo finale DDT - - - - - - - - - - - - - - - -*
067440040618      *   - OverFlow già testato per questa riga -
067450040607     c                   write     A4prtDK5
067460040607      *
067470040607     c                   ENDSR
067480040607      *
067490040607      *---------------------------------------------------------------*
067500050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
067510050217      *?Dati di omaggio: testo per articoli omaggiati                ?*
067520040607      *---------------------------------------------------------------*
067530050217     c     PrtDdtKARN_O1 BEGSR
067540040608      *
067550040608     c                   eval      $DDT_o = *on
067560040607      *
067570040607     c                   clear                   A4prtDK5
067580051110     c                   movel(p)  AR8uni        dAR8O1
067590040607      *
067600040607      * > righe descrittive esterne al DDT
067610040607      * - - riga descrittiva
067620040607     c                   eval      PCtxx  = §AR8o1a
067630040607      * - - nessuna sottolineatura
067640040608     c***                eval      *in39  = *off
067650040607      *
067660040607      * > Stampa testata omaggi in DDT - - - - - - - - - - - - - - - -*
067670040608      *   (se NON già stampata nella nuova pagina x OverFlow)
067680040608     c                   eval      SavLineA4 = CurLineA4
067690040607     c                   exsr      Test_OF
067700040608if  1c                   if        CurLineA4 = SavLineA4
067710040607     c                   write     A4prtDK5
067720040608e   1c                   endif
067730040608      * ...Dovendo ristampare le righe di intestazione degli omaggi
067740040608      *   in ogni pagina (da quella del 1° omaggio in poi), non calcolo
067750040608      *   quelle già stampate...
067760040608     c*** NO:            sub       1             wOmaRowT
067770040607      *
067780040607     c                   ENDSR
067790040607      *
067800040607      *---------------------------------------------------------------*
067810050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
067820050217      *?Dati di omaggio: articolo, descrizione, quantità             ?*
067830040607      *---------------------------------------------------------------*
067840050217     c     PrtDdtKARN_O2 BEGSR
067850040607      *
067860040607     c                   clear                   A4prtDK2
067870051110     c                   movel(p)  AR8uni        dAR8O2
067880040607      *
067890040607      * > dettaglio omaggi DDT
067900040607      * - - codice prodotto
067910040607     c                   eval      PCcpr  = §AR8o2a
067920040607      * - - descrizione prodotto
067930040607     c                   eval      PCdpr  = §AR8o2b
067940040607      * - - quantità
067950040618     c                   eval      PCqta  = '     ' + §AR8o2c
067960040607      * - - note
067970040607     c***                eval      PCnot  = *blanks
067980040607      *
067990040607      * > Stampa dettaglio omaggi in DDT - - - - - - - - - - - - - - -*
068000040607      *
068010040607     c                   exsr      Test_OF
068020040607     c                   write     A4prtDK2
068030040608     c                   sub       1             wOmaRowD
068040040607      *
068050040607     c                   ENDSR
068060040608      *
068070040608      *---------------------------------------------------------------*
068080081002      *?Impostazione nuova pagina di DDT Karnak / Titanedi / EDk     ?*
068090040608      *---------------------------------------------------------------*
068100081002     c     NewPageDK     BEGSR
068110040608      *
068120080923      /free
068130080923         // Impostazione del nuovo limite x overflow
068140080923         select;
068150080923           when  NOT *in45;
068160080923             wMaxLine = $MaxLine3;
068170080923           when  *in45   and   NOT *in46;
068180080923             wMaxLine = $MaxLine3_pl1;
068190080923           when  *in45   and   *in46;
068200080923             wMaxLine = $MaxLine3_pl2;
068210080923         endsl;
068220080923      /end-free
068230080923     c                   write     A4prtSP1
068240040608     c                   write     A4prtDK1
068250040608      *
068260040608if  1c                   if        $DDT_o = *on
068270040608      *
068280040608      * - Stampa 1 riga di dettaglio vuota
068290040608     c                   clear                   A4prtDK3
068300040608     c                   write     A4prtDK3
068310040618      * - Reimposta i dati nel box finale del DDT
068320050217     c                   exsr      PrtDdtKARNaV2
068330040618      * - Stampa fondo DDT
068340050217     c                   exsr      PrtDdtKARNbV2
068350040608      * - Stampa ultima riga
068360040608      *   ("Per qualsiasi problema o chiarimento contattare...)
068370040608     c                   clear                   A4prtDK5
068380040608     c                   eval      PCtxx  = §AR8l1a
068390040608     c                   write     A4prtDK5
068400040608      * - Stampa intestazione per merce omaggio
068410040608      *   xx = numero di righe per "intestazione omaggi"
068420040608     c                   eval      wx     = xx
068430040608do  2c                   dow           xx         >  *zeros
068440040608     c                             and xx         <= %elem($AR8o1)
068450040608     c                             and $AR8o1(xx) <> *blanks
068460040608     c                   eval      PCtxx  = $AR8o1(xx)
068470040608     c                   write     A4prtDK5
068480040608     c                   eval      xx     = xx - 1
068490040608e   2c                   enddo
068500040608     c                   eval      xx     = wx
068510040608      * -Se sta leggendo il tipo record "O1":?
068520040608      *   sposta il ciclo di lettura del file FIAR8 sul record
068530040608      *   successivo all'ultimo (appena stampato) dell'intestazione
068540040608      *   della merce omaggiata
068550040608if  2c                   if        AR8trk =  'O1'
068560040608if  3c                   dou       AR8trk <> 'O1'
068570040608     c     KLDV          reade     FIAR8000
068580040608e   3c                   enddo
068590040608if  3c*** è così:        if        AR8trk = 'O2'
068600040608     c     KLDV          readpe    FIAR8000
068610040707e   3c***                endif
068620040608e   2c                   endif
068630040608      * - Aggiorna controllo se "ultima pagina"
068640080924      /free
068650080924           select;
068660080924             when  NOT *in45;
068670080924      /end-free
068680040608     c                   eval      $DDTlastPg = ($MaxLine2
068690040713     c                                        +  1
068700040608     c                                        -  CurLineA4
068710040608     c                                        -  $BoxRow1
068720040608     c                                        -  wOmaRowT
068730040608     c                                        >= wOmaRowD)
068740080924      /free
068750080924             when  *in45   and   NOT *in46;
068760110406               $DDTlastPg = ($MaxLine2_pl1k
068770080924                             + 1 - CurLineA4 - $BoxRow1 - wOmaRowT
068780080924                             >= wOmaRowD);
068790080924             when  *in45   and   *in46;
068800110406               $DDTlastPg = ($MaxLine2_pl2k
068810080924                             + 1 - CurLineA4 - $BoxRow1 - wOmaRowT
068820080924                             >= wOmaRowD);
068830080924           endsl;
068840080924      /end-free
068850040608      *
068860040608e   1c                   endif
068870040608      *
068880040608     c                   ENDSR
068890040607      *
068900040607      *---------------------------------------------------------------*
068910050414      *?Posizionamento di linee e box nel DDT KARNAK / TITANEDI / EDk?*
068920040607      *---------------------------------------------------------------*
068930040607     c     Posiz_DK      BEGSR
068940040607      *
068950080918      /free
068960080923         // Impostazione posizioni di logo/box
068970080923         // (posizioni di "partenza" diverse a seconda del posizionam.
068980080923         //  iniziale nel foglio A4 attualmente in stampa)
068990080923         select;
069000080923           when  NOT *in45;
069010081002             §PB1 =  3.120;
069020080923           when  *in45   and   NOT *in46;
069030081002             §PB1 = 0.250;
069040080923           when  *in45   and   *in46;
069050081002             §PB1 = 6.000;
069060080923         endsl;
069070080918      /end-free
069080040607     c                   eval      §PB2 =  §PB1 + 1.23
069090080822     c                   eval      §PB3 =  §PB2 + 0.18
069100040607     c                   eval(h)   §PB4 =  CurLineA4 / 8 + 0.20
069110040607     c                   eval      §PB5 =  §PB4 + 0.18
069120040607     c                   eval      §PB6 =  §PB5 + 0.77
069130040607     c                   eval      §PB7 =  §PB5 + 0.165
069140040607     c                   eval      §PB8 =  §PB7 + 0.08
069150040607      * Impostazione posizioni di linee orizzontali nel 1° box
069160040607     c                   eval(h)   §LO1 =  §PB1 + (§PB2 - §PB1) / 2
069170040607      * Impostazione posizioni di linee orizzontali nel 2° box
069180080822     c                   eval      §LO2 =  §PB3 + 0.23
069190080822     c                   eval      §LO3 =  §LO2 + 0.145
069200080822     c                   eval      §LO4 =  §LO3 + 0.10
069210040607     c                   eval      §LO5 =  §PB4 - 0.15
069220040607      * Impostazione posizioni di linee orizzontali nel 3° box
069230040607     c                   eval      §LO6 =  §PB5 + 0.14
069240040607     c                   eval      §LO7 =  §LO6 + 0.12
069250040607     c                   eval      §LO8 =  §LO7 + 0.12
069260040607     c                   eval      §LO9 =  §LO8 + 0.13
069270040607      * Impostazione posizioni di linee verticali nel 2° box
069280040607     c                   eval      §LV1 =  §LO2 - §PB3
069290040607     c                   eval      §LV2 =  §LO3 - §LO2
069300040607     c                   eval      §LV3 =  §LO5 - §LO3
069310040607      * Impostazione posizioni di linee verticali nel 3° box
069320040607     c                   eval      §LV4 =  §LO6 - §PB5
069330040607     c                   eval      §LV5 =  §LO7 - §LO6
069340040607     c                   eval      §LV6 =  §LO8 - §LO7
069350040607     c                   eval      §LV7 =  §PB6 - §LO9
069360040607      * Impostazione posizioni di campi
069370040607     c                   eval      §PS1 =  §LO5 + 0.11
069380040607     c                   eval      §PS2 =  §PB4 + 0.08
069390040607     c                   eval      §PS3 =  §PS2 + 0.07
069400130118if  1c*//                if        §PB6 <  §PB1 + 4.58
069410130118     c*//                eval      §PR1 =  §PB1 + 4.58
069420130118if  1c                   if        §PB6 <  §PB1 + 3.30
069430130118     c                   eval      §PR1 =  §PB1 + 3.30
069440040607x   1c                   else
069450040607     c                   eval      §PR1 =  §PB6
069460040607e   1c                   endif
069470040607      *
069480040607     c                   ENDSR
069490041019      *
069500041019      *---------------------------------------------------------------*
069510050414      *?Stampa DDT del mittente LOGISTICA / SAIT ABRASIVI            ?*
069520041019      *---------------------------------------------------------------*
069530050218     c     DDT_Logistica BEGSR
069540041019      *
069550080924      /free
069560080924         // Impostazione del nuovo limite x overflow
069570080924         // (Codesto limite è valido solo per la stampa del dettaglio.
069580080924         //   Per la stampa del "fondo" bolla occorre REimpostarlo ad
069590080924         //   un valore più basso.
069600080924         //  Ovvero: stampo più righe di dettaglio che posso; se alla
069610080924         //  fine delle righe di dettaglio non rimane spazio a suffi-
069620080924         //  cienza per il fondo: prima della stampa del fondo si salta
069630080924         //  alla pagina successiva, dove non si stampa alcuna riga di
069640080924         //  dettaglio.)
069650080924         select;
069660080924           when  NOT *in45;
069670090323             wMaxLine = $MaxLine2;
069680080924           when  *in45   and   NOT *in46;
069690090323             wMaxLine = $MaxLine2_pl1;
069700080924           when  *in45   and   *in46;
069710090323             wMaxLine = $MaxLine2_pl2;
069720080924         endsl;
069730080924
069740080924         // Posizionamento iniziale all'interno del foglio A4
069750080924         write  A4prtSP1;
069760081006
069770081006         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
069780081006           write  A4prtNUL;
069790081006         endif;
069800080924      /end-free
069810041019      *
069820041019      * Impostazione testi fissi - - - - - - - - - - - - - - - - - - -*
069830041019      *
069840071008      * - prima solo per "Logistica", ora neanche più per lei...
069850071008      * - Sait Abrasivi SpA & Brumar Srl hanno Tipo Stampa DDT = "LC"
069860071008if  1c***                if        §AR8tsd <> 'LC'
069870071008     c***                eval      PCLtx1 = 'La merce viagga a rischio -
069880071008     c***                          e pericolo del Committente, pertanto -
069890071008     c***                          la Venditrice non risponde di -
069900071008     c***                          eventuali perdite o avarie derivanti -
069910071008     c***                          dal trasporto.'
069920071008     c***                eval      PCLtx2 = 'Il Destinatario, per -
069930071008     c***                          ottenere il risarcimento del danno, -
069940071008     c***                          deve far constatare al Corriere la -
069950071008     c***                          perdita o l''avaria all''atto del -
069960071008     c***                          ricevimento della merce.'
069970071008e   1c***                endif
069980050218      *
069990130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
070000130315      *  "StampaDDT")
070010050218     c***  KLDV          setll     FIAR8000       (già fatto)
070020050218     c***  KLDV          reade     FIAR8000       (già fatto)
070030041019      *
070040041019      * Reperimento dati, impostazione e stampa DDT mittente  - - - -*
070050041019      *
070060041019if  1c                   dow       NOT %eof(FIAR801L)
070070050318      *
070080050318if  2c                   if        AR8atb   = *blanks
070090050318     c                   eval      SAVtrk   = AR8trk
070100050318e   2c                   endif
070110041019      *
070120041019sel 2c                   select
070130041019      * - Record annullato (non si stampa nulla)
070140050218w   2c                   when      AR8atb  <> *blanks
070150041019      * - Dati di dettaglio: articolo, descrizione, quantità
070160050218w   2c                   when      AR8trk   = 'C1'
070170050217     c                   exsr      PrtDdtLOG1_C1
070180041103      * - Dati di dettaglio: descrizioni generiche
070190050218w   2c                   when      AR8trk   = 'C2'
070200050217     c                   exsr      PrtDdtLOG1_C2
070210041103      * - Dati di dettaglio: descrizioni generiche
070220050218w   2c                   when      AR8trk   = 'C3'
070230050217     c                   exsr      PrtDdtLOG1_C3
070240060614      * - Dati di dettaglio: corpo bolla non fincato
070250060614w   2c                   when      AR8trk   = 'C4'
070260060614     c                   exsr      PrtDdtLOG1_C4
070270090323      * - Dati di dettaglio: descrizioni generiche
070280090323w   2c                   when      AR8trk   = 'C5'
070290090323     c                   exsr      PrtDdtLOG1_C5
070300050318      * - Dati di packing-list: dettaglio
070310050318w   2c                   when      AR8trk   = 'Z1'
070320050318     c                   exsr      PrtDdtLOG1_Z1
070330041019      * - Dati di testata: sito e depositante
070340050218w   2c                   when      AR8trk   = 'T1'
070350050217     c                   exsr      PrtDdtLOG1_T1
070360041019      * - Dati di testata: destinatario, cliente di fatturazione
070370050218w   2c                   when      AR8trk   = 'T2'
070380050217     c                   exsr      PrtDdtLOG1_T2
070390041019      * - Dati di testata: agente, numero ordine, estremi documento
070400050218w   2c                   when      AR8trk   = 'T3'
070410050217     c                   exsr      PrtDdtLOG1_T3
070420041019      * - Dati di fondo: mezzo e causale trasporto, colli, peso
070430050218w   2c                   when      AR8trk   = 'P1'
070440050217     c                   exsr      PrtDdtLOG1_P1
070450050606      * - Dati di testata: FATTURA
070460050606w   2c                   when      AR8trk   = 'S1'
070470050606     c                   exsr      PrtDdtLOG1_S1
070480050606      * - Dati di dettaglio cpi 15: FATTURA
070490050606w   2c                   when      AR8trk   = 'S2'
070500050606     c                   exsr      PrtDdtLOG1_S2
070510050606      * - Dati di dettaglio cpi 20: FATTURA
070520050606w   2c                   when      AR8trk   = 'S3'
070530050606     c                   exsr      PrtDdtLOG1_S3
070540050606      * - Dati di fondo: FATTURA
070550050606w   2c                   when      AR8trk   = 'S4'
070560050606     c                   exsr      PrtDdtLOG1_S4
070570050606      * - Dati di fondo note: FATTURA
070580050606w   2c                   when      AR8trk   = 'S5'
070590050606     c                   exsr      PrtDdtLOG1_S5
070600041019e   2c                   endsl
070610041019      *
070620041019     c     KLDV          reade     FIAR8000
070630041019      *
070640041019e   1c                   enddo
070650050318      *
070660050318      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
070670050318      *
070680050505      * - se DDT con Packing-List:
070690050505      *   (altrimenti il box è già stato stampato in sr PrtDdtLOG1_P1)
070700050505if  1c                   if        %subst(SAVtrk:1:1) =  'Z'
070710050318      *
070720050318      * - - Calcolo la posizione finale del box e di linee separatrici
070730050318     c                   exsr      Posiz_DL
070740050318      *
070750050318      * > Stampa box, linee e testi fissi (no logo)  - - - - - - - - -*
070760081017     c                   if            §AR8cxd = *blanks
070770081017     c                             or  DB0fa4  = 'P'
070780050331     c                   write     A4prtDT0
070790050331     c                   endif
070800050318     c                   write     A4prtDL0C
070810050505      *
070820050505e   1c                   endif
070830050218      *
070840050218      * Reimpostazione del vecchio limite x overflow
070850080925      /free
070860080925         select;
070870080925           when  NOT *in45;
070880090323             wMaxLine = $MaxLine2;
070890080925           when  *in45   and   NOT *in46;
070900090323             wMaxLine = $MaxLine2_pl1;
070910080925           when  *in45   and   *in46;
070920090323             wMaxLine = $MaxLine2_pl2;
070930080925         endsl;
070940080925      /end-free
070950041019      *
070960041019     c                   ENDSR
070970081222
070980081222      /free
070990081222       //--------------------------------------------------------------
071000140307       //?Preparazione simil-logo Bartolini                            ?
071010081222       //--------------------------------------------------------------
071020090406       BEGSR  PrtDdtLOG1_SimilLogo;
071030081222
071040120530         //If  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
071050120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
071060120530         //
071070120530         //  // -?Preparazione simil-Logo BARTOLINI?
071080120530         //  §PLBLS  = §PBA + 0.150;
071090120530         //  §PLBLVS = 0.400;
071100120530         //  exsr  sr_SimilLogoBAR;
071110120530         //
071120120530         //  // -?Preparazione simil-Logo BRT?
071130120530         //  §P2blS  = §PlblS;
071140120530         //  //§P2blVS = §PlblVD + 0.050;
071150120530         //  §P2blVS = §PlblVD + 0.030;
071160120530         //  exsr  sr_SimilLogoBRT;
071170120530         //
071180120530         //Else;
071190120530         //
071200120530         //  // -?Preparazione simil-Logo BARTOLINI?
071210120530         //  §PLBLS  = §PBA + 0.050;
071220120530         //  §PLBLVS = 0.400;
071230120530         //  exsr  sr_SimilLogoBART;
071240120530         //
071250120530         //EndIf;
071260120530
071270120530         // -?Preparazione simil-Logo BRT?
071280120530         §PLBLS  = §PBA + 0.050;
071290120530         §PLBLVS = 0.400;
071300120530         exsr  sr_SimilLogoBRT_;
071310090406
071320120530         // -?Impostazione dati anagrafici BRT alla sua destra?
071330090422         §PC0    = §PBA    + 0.070 + 0.050;
071340090406         §PC1    = §PC0    + 0.100;
071350090406         §PC2    = §PC1    + 0.100;
071360090406         §PC3    = §PC2    + 0.090;
071370090406         §PC4    = §PC3    + 0.090;
071380090406         §PC5    = §PC4    + 0.090;
071390120530
071400120530         //§PLC    = 1.400;
071410120530         §PLC    = 1.000;
071420090406
071430120530         // -?"TBAR" è già correttamente impostato.?
071440120530         //TBAR    = $Bartolini;        (già così)
071450120530         //TBAR    = $BRT;              (già così)
071460090406         LBA1    = $Intest1;
071470090406         LBA2    = $Intest2_b;
071480120530         //LBA3    = $Intest3_b;
071490120530         LBA3    = $Intest3_b_brt;
071500090406         LBA4    = $Intest4_b;
071510090406         LBA5    = $Intest5_b;
071520081222
071530081222       ENDSR;
071540081222      /end-free
071550050214      *
071560041019      *---------------------------------------------------------------*
071570050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
071580050217      *?Dati di testata: sito e depositante                          ?*
071590041019      *---------------------------------------------------------------*
071600050217     c     PrtDdtLOG1_T1 BEGSR
071610041019      *
071620041019      * > logo
071630050218     c                   eval      §Logo    = 'LOGO' + AR8tmd
071640090107      /free
071650090107         // - Impostazione indicatore che condiziona la stampa del logo
071660120530         //   (o quella del "simil-logo" BRT)
071670090107         *in55 = (%subst(AR8tmd : 1 : 3) = 'LOG'          and
071680090107                  (§AR8tsd  = 'L1'  or  §AR8tsd  = 'L3'));
071690090107      /end-free
071700080908      *
071710060609      *? L O G O !! ?
071720060609      * per pulizia non vogliamo più avere tanti LOGx per ogni cliente che
071730060609      * vuole la stampa della fattura con il proprio logo e la bolla con
071740120530      * il logo BRT
071750060609      * anche Cappellini è d'accordo ad inviare i dati con il logo del
071760060609      * cliente da stampare nella LDV
071770060609      * quelli già esistenti avranno un nuovo logo un po' più piccolo per
071780071008      * problemi di spazio e i dati saranno inviati con AR8tmd = al nome del
071790071008      * nuovo logo e la tabella sarà fatta con §AR8tsd = 'L2' o 'L4'
071800060609      * i nuovi faremo in modo di avere o lo stesso logo oppure la stessa
071810060609      * logica descritta prima
071820060609      * ma può essere che un cliente voglia la fattura con il suo logo
071830120530      * mentre la bolla con il logo BRT quindi devo recuperare
071840060609      * il logo da stampare nella bolla non più da AR8TMD, che conterrà
071850060609      * il nome del logo da stampare nella fattura, ma dal campo del
071860120530      * del logo fattura, che conterrà il logo BRT, mentre il campo
071870071008      * §AR8tsd nella tabella sarà 'L1' o 'L3'
071880071008     c                   if        (§AR8tsd = 'L1' or §AR8tsd = 'L3') and
071890071008     c                             §AR8logof <> *blanks and
071900071008     c                             %subst(AR8tmd:1:3) <> 'LOG'
071910071008     c                   eval      §Logo    = 'LOGO' + §AR8logof
071920090107      /free
071930090107           // - Impostaz. indicatore che condiziona la stampa del logo
071940120530           //   (o quella del "simil-logo" BRT)
071950090107           *in55 = (%subst(§AR8logof : 1 : 3) = 'LOG');
071960090107      /end-free
071970060609     c                   endif
071980041019      *
071990080822     c                   movel(p)  AR8uni        dAR8LOG1T1
072000050414      *
072010060627if  1c                   if            §AR8tsd <> 'LC'
072020041019      *
072030041019      * > sito
072040060407     c                   select
072050071008     c                   when      §AR8tsd = 'L1' or
072060071008     c                             §AR8tsd = 'L3'
072070050707     c                   eval      PLOG1st0 = 'Deposito di:'
072080071008     c                   when      §AR8tsd = 'L2' or
072090071008     c                             §AR8tsd = 'L4'
072100060407     c                   eval      PLOG1st0 = 'Depositario:'
072110060407     c                   endsl
072120050707     c                   eval      PLOG1st1 = §A8LOG1t1a
072130050707     c                   eval      PLOG1st2 = §A8LOG1t1b
072140050707     c                   eval      PLOG1st3 = §A8LOG1t1c
072150050707     c                   eval      PLOG1st4 = §A8LOG1t1d
072160050707     c                   eval      PLOG1st5 = §A8LOG1t1e
072170041019      *
072180041019      * > depositante
072190060407     c                   select
072200071008     c                   when      §AR8tsd = 'L1' or
072210071008     c                             §AR8tsd = 'L3'
072220050707     c                   eval      PLOG1de0 = 'Depositario della:'
072230071008     c                   when      §AR8tsd = 'L2' or
072240071008     c                             §AR8tsd = 'L4'
072250060407     c                   eval      PLOG1de0 = 'Deposito di:'
072260060407     c                   endsl
072270050707     c                   eval      PLOG1de1 = §A8LOG1t1f
072280050707     c                   eval      PLOG1de2 = §A8LOG1t1g
072290050707     c                   eval      PLOG1de3 = §A8LOG1t1h
072300050707     c                   eval      PLOG1de4 = §A8LOG1t1i
072310050707     c                   eval      PLOG1de5 = §A8LOG1t1j
072320050414      *
072330050414x   1c                   else
072340050414      *
072350050707     c***                eval      PLOG1st0 = *blanks
072360050707     c                   eval      PLOG1st1 = §A8LOG1t1a + §A8LOG1t1b
072370050707     c                   eval      PLOG1st2 = §A8LOG1t1c + §A8LOG1t1d
072380050707     c                   eval      PLOG1st3 = §A8LOG1t1e + §A8LOG1t1f
072390050707     c                   eval      PLOG1st4 = §A8LOG1t1g + §A8LOG1t1h
072400050707     c                   eval      PLOG1st5 = §A8LOG1t1i + §A8LOG1t1j
072410050414      *
072420050414e   1c                   endif
072430050214      *
072440041022     c                   ENDSR
072450050214      *
072460041019      *---------------------------------------------------------------*
072470050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
072480050217      *?Dati di testata: destinatario, cliente di fatturazione       ?*
072490041019      *---------------------------------------------------------------*
072500050217     c     PrtDdtLOG1_T2 BEGSR
072510041019      *
072520051110     c                   clear                   dAR8LOG1T2
072530050218     c                   movel     AR8uni        dAR8LOG1T2
072540041019      *
072550041019      * > destinatario
072560050707if  1c                   if        §A8LOG1T2a <> *blanks
072570050707     c                   eval      PLOG1ccd   =  §A8LOG1T2a
072580050218e   1c                   endif
072590050707if  1c                   if        §A8LOG1T2c =  *blanks
072600050707     c                   clear                   PLOG1rsd
072610050707     c                   eval      PLOG1rsd2  =  §A8LOG1T2b
072620050218x   1c                   else
072630050707     c                   eval      PLOG1rsd   =  §A8LOG1T2b
072640050707     c                   eval      PLOG1rsd2  =  §A8LOG1T2c
072650050218e   1c                   endif
072660050707     c                   eval      PLOG1ind   =  §A8LOG1T2d
072670050707     c                   eval      PLOG1ind2  =  §A8LOG1T2e
072680050707if  1c                   if        §A8LOG1T2f >  *zeros
072690050707     c                   eval      PLOG1cad   =  §A8LOG1T2f
072700041019e   1c                   endif
072710050707     c                   eval      PLOG1lod   =  §A8LOG1T2g
072720041019      * > cliente di fatturazione
072730050707if  1c                   if        §A8LOG1T2h <> *blanks
072740050707     c                   eval      PLOG1ccf   =  §A8LOG1T2h
072750041019e   1c                   endif
072760050707if  1c                   if        §A8LOG1T2j =  *blanks
072770050707     c                   clear                   PLOG1rsf
072780050707     c                   eval      PLOG1rsf2  =  §A8LOG1T2i
072790050218x   1c                   else
072800050707     c                   eval      PLOG1rsf   =  §A8LOG1T2i
072810050707     c                   eval      PLOG1rsf2  =  §A8LOG1T2j
072820050218e   1c                   endif
072830050707     c                   eval      PLOG1inf   =  §A8LOG1T2k
072840050707     c                   eval      PLOG1inf2  =  §A8LOG1T2l
072850050707if  1c                   if        §A8LOG1T2m >  *zeros
072860050707     c                   eval      PLOG1caf   =  §A8LOG1T2m
072870041019e   1c                   endif
072880050707     c                   eval      PLOG1lof   =  §A8LOG1T2n
072890041019      *
072900041019     c                   ENDSR
072910041019      *
072920041019      *---------------------------------------------------------------*
072930050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
072940050217      *?Dati di testata: documento, rif.ordine, note                 ?*
072950041019      *---------------------------------------------------------------*
072960050217     c     PrtDdtLOG1_T3 BEGSR
072970041019      *
072980051110     c                   clear                   dAR8LOG1T3
072990050218     c                   movel     AR8uni        dAR8LOG1T3
073000050706      *
073010050706      * TEST effettiva numericità dei campi previsti numerici:
073020100513     c                   clear                   w053a
073030111013     ***c                   eval      WX    =  %len(§A8LOG1T3b)
073040111013     ***c                                   +  %len(§A8LOG1T3c)
073050111013     ***c                                   +  %len(§A8LOG1T3d)
073060111013     ***c                   eval      w053a =  %subst(dAR8LOG1T3: 4:WX)
073070111013     ***c                   eval      *in31 =  *off
073080111013     ***c     c_Digits      checkr    w053a :  WX                            31
073090111013if  1***c                   if        *in31
073100111013     ***c                   clear                   §A8LOG1T3b
073110111013     ***c                   clear                   §A8LOG1T3c
073120111013     ***c                   clear                   §A8LOG1T3d
073130111013e   1***c                   endif
073140121116     ***c                   eval      WX    =  %len(§A8LOG1T3b)
073150121116     ***c                   eval      w053a =  %subst(dAR8LOG1T3: 4:WX)
073160121116     ***c                   eval      *in31 =  *off
073170121116     ***c     c_Digits      checkr    w053a :  WX                            31
073180121116if  1***c                   if        *in31
073190121116     ***c                   clear                   §A8LOG1T3b
073200121116e   1***c                   endif
073210111013     c                   clear                   w053a
073220111013     c                   eval      WX    =  %len(§A8LOG1T3d)
073230111013     c                   eval      w053a =  %subst(dAR8LOG1T3:15:WX)
073240111013     c                   eval      *in31 =  *off
073250111013     c     c_Digits      checkr    w053a :  WX                            31
073260111013if  1c                   if        *in31
073270111013     c                   clear                   §A8LOG1T3d
073280111013e   1c                   endif
073290041019      *
073300041019      * > dati in testata DDT
073310041019      * - - numero e data documento (DDT)
073320050707     c                   eval      PLOG1nsr = §A8LOG1T3a
073330121116     ***c                   eval      PLOG1aas = §A8LOG1T3b
073340121116     c                   if        §A8Log1T3b <> *blanks   and
073350121116     c                             §A8Log1T3b <> *zeros
073360121116     c                   eval      PLOG1aas = §A8LOG1T3b
073370121116     c                   endif
073380111013     ***c                   if        §A8Log1T3c <> *zeros
073390111013     ***c                   eval      PLOG1nsp = %editc(§A8LOG1T3c:'X')
073400111013     ***c                   endif
073410111013     c                   if        §A8Log1T3c <> *blanks   and
073420111013     c                             §A8Log1T3c <> *zeros
073430111013     c                   eval      PLOG1nsp = §A8LOG1T3c
073440111013     c                   endif
073450100319     c                   if        §A8Log1T3a <> *blanks   and
073460121116     c                             §A8Log1T3b <> *blanks   and
073470100319     c                             §A8Log1T3b <> *zeros
073480100319     c                   eval      PLog1sepA = '/'
073490100319     c                   endif
073500121115     c                   if        §A8Log1T3b <> *zeros    and
073510121115     c                             §A8Log1T3c <> *blanks   and
073520100319     c                             §A8Log1T3c <> *zeros
073530100319     c                   eval      PLog1sepB = '/'
073540100319     c                   endif
073550110707      *     giro la data del documento
073560050707if  1c                   if        §A8LOG1T3d > *zeros
073570060922     c                   eval      *in31 =  *off
073580060922     c     *iso          test(d)                 §A8LOG1T3d             31
073590060922if  2c                   if        NOT *in31
073600050707     c     *iso          movel     §A8LOG1T3d    Data_EUR
073610050707     c     *eur          movel     Data_EUR      PLOG1dsp
073620060922x   2c                   else
073630060922     c                   clear                   PLOG1dsp
073640060922e   2c                   endif
073650050707e   1c                   endif
073660041019      * - - riferimento ordine
073670050707     c                   eval      PLOG1ref = §A8LOG1T3e
073680041019      * - - note consegna
073690050707     c                   eval      PLOG1not = §A8LOG1T3f
073700050707     c                   eval      PLOG1nt2 = §A8LOG1T3g
073710060612      * - - intestazione corpo bolla per Logistica tipo stampa L3 o L4
073720060922     c                   if        §AR8tsd = 'L3' or §AR8tsd = 'L4'
073730060612     c                   eval      *in23 = *On
073740060612     c                   eval      PLOG1int = §A8LOG1T3h
073750060612     c                   else
073760060612     c                   eval      *in23 = *off
073770060612     c                   endif
073780041019      *
073790041019      * > Stampa testata DDT - - - - - - - - - - - - - - - - - - - - -*
073800041019      *
073810041019      *   - OverFlow impossibile qui! -
073820050218     c***                exsr      Test_OF
073830050318     c                   exsr      Posiz_DL
073840081017     c                   if            §AR8cxd = *blanks
073850081017     c                             or  DB0fa4  = 'P'
073860050331     c                   write     A4prtDT0
073870050331     c                   endif
073880050218     c                   write     A4prtDL0
073890081222      /free
073900081222         if  *in55;
073910090406           exsr   PrtDdtLOG1_SimilLogo;
073920120530           //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
073930120530           //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
073940120530           //  write  A4LogoBR2;
073950120530           //else;
073960120530           //  write  A4LogoBAR;
073970120530           //endif;
073980120530           write  A4LogoBRT;
073990081222           write  A4prtDLL;
074000081222         endif;
074010081222      /end-free
074020050218     c*** quante righe?  write     A4prtDL0A
074030041028     c                   write     A4prtDL1A
074040050218     c                   write     A4prtDL1B
074050041019      *
074060041019     c                   ENDSR
074070050214      *
074080041019      *---------------------------------------------------------------*
074090050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
074100050217      *?Dati di dettaglio: articolo, quantità, descrizione           ?*
074110041019      *---------------------------------------------------------------*
074120050217     c     PrtDdtLOG1_C1 BEGSR
074130041019      *
074140041019     c                   clear                   A4prtDL2
074150051110     c                   clear                   dAR8LOG1C1
074160050218     c                   movel     AR8uni        dAR8LOG1C1
074170050706      *
074180050706      * TEST effettiva numericità dei campi previsti numerici:
074190100513     c                   clear                   w053a
074200050706     c                   eval      WX    =  %len(§A8LOG1C1b)
074210100513     c                   eval      w053a =  %subst(dAR8LOG1C1:21:WX)
074220050706     c                   eval      *in31 =  *off
074230110426     c     c_Digits      checkr    w053a :  WX                            31
074240050706if  1c                   if        *in31
074250050706     c                   clear                   §A8LOG1C1b
074260050706e   1c                   endif
074270041019      *
074280041019      * > dati in dettaglio DDT
074290041019      * - - codice prodotto
074300050707     c                   eval      PLOG1cpr  = §A8LOG1C1a
074310041019      * - - quantità
074320050707     c                   eval      PLOG1qta  = §A8LOG1C1b
074330041019      * - - descrizione prodotto
074340050707     c                   eval      PLOG1dpr  = §A8LOG1C1c
074350041019      *
074360041019      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
074370041019      *
074380041019     c                   exsr      Test_OF
074390041019     c                   write     A4prtDL2
074400041019      *
074410041019     c                   ENDSR
074420041019      *
074430041019      *---------------------------------------------------------------*
074440050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
074450050217      *?Dati di dettaglio: descrizione                               ?*
074460041019      *---------------------------------------------------------------*
074470050217     c     PrtDdtLOG1_C2 BEGSR
074480041019      *
074490041019     c                   clear                   A4prtDL2
074500051110     c                   clear                   dAR8LOG1C2
074510050218     c                   movel     AR8uni        dAR8LOG1C2
074520041019      *
074530041019      * > dati in note DDT
074540041019      * - - riga descrittiva
074550050707     c                   eval      PLOG1dpr  = §A8LOG1C2a
074560041019      *
074570041019      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
074580041019      *
074590041019     c                   exsr      Test_OF
074600041019     c                   write     A4prtDL2
074610041019      *
074620041019     c                   ENDSR
074630041019      *
074640041019      *---------------------------------------------------------------*
074650050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
074660050217      *?Dati di dettaglio: descrizioni                               ?*
074670041019      *---------------------------------------------------------------*
074680050217     c     PrtDdtLOG1_C3 BEGSR
074690041019      *
074700041019     c                   clear                   A4prtDL3
074710051110     c                   clear                   dAR8LOG1C3
074720050707     c                   movel     AR8uni        dAR8LOG1C3
074730041019      *
074740041019      * > dati in note DDT
074750041019      * - - riga descrittiva
074760050707     c                   eval      PLOG1txt  = §A8LOG1C3a
074770041019      *
074780041019      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
074790041019      *
074800041019     c                   exsr      Test_OF
074810041019     c                   write     A4prtDL3
074820041019      *
074830041019     c                   ENDSR
074840060614      *
074850060614      *---------------------------------------------------------------*
074860060614      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
074870060614      *?Dati di dettaglio: corpo bolla non fincato                   ?*
074880060614      *---------------------------------------------------------------*
074890060614     c     PrtDdtLOG1_C4 BEGSR
074900060614      *
074910060614     c                   clear                   A4prtDL21
074920060614     c                   clear                   dAR8LOG1C4
074930060614     c                   movel     AR8uni        dAR8LOG1C4
074940060614      *
074950060614      * > dati DDT
074960060614      * - - riga descrittiva
074970060614     c                   eval      PLOG1dpr1 = §A8LOG1C4a
074980060614      *
074990060614      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
075000060614      *
075010060614     c                   exsr      Test_OF
075020060614     c                   write     A4prtDL21
075030060614      *
075040060614     c                   ENDSR
075050090323      *
075060090323      *---------------------------------------------------------------*
075070090323      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
075080090323      *?Dati di dettaglio: descrizioni                               ?*
075090090323      *---------------------------------------------------------------*
075100090323     c     PrtDdtLOG1_C5 BEGSR
075110090323      *
075120090323     c                   clear                   A4prtDL31
075130090323     c                   clear                   dAR8LOG1S3
075140090323     c                   movel     AR8uni        dAR8LOG1S3
075150090323      *
075160090323      * > dati in note DDT
075170090323      * - - riga descrittiva
075180090323     c                   eval      PLOG1txt2 = §A8LOG1S3a
075190090323      *
075200090323      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
075210090323      *
075220090323     c                   exsr      Test_OF
075230090323     c                   write     A4prtDL31
075240090323      *
075250090323     c                   ENDSR
075260041019      *
075270041019      *---------------------------------------------------------------*
075280050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
075290050217      *?Dati di fondo: beni, colli, peso, volume, c/a, porto, trasp. ?*
075300041019      *---------------------------------------------------------------*
075310050217     c     PrtDdtLOG1_P1 BEGSR
075320041019      *
075330051110     c                   clear                   dAR8LOG1P1
075340050707     c                   movel     AR8uni        dAR8LOG1P1
075350050706      *
075360050706      * TEST effettiva numericità dei campi previsti numerici:
075370100513     c                   clear                   w053a
075380050706     c                   eval      WX    =  %len(§A8LOG1P1b)
075390050706     c                                   +  %len(§A8LOG1P1c)
075400050706     c                                   +  %len(§A8LOG1P1d)
075410100513     c                   eval      w053a =  %subst(dAR8LOG1P1:42:WX)
075420050706     c                   eval      *in31 =  *off
075430110426     c     c_Digits      checkr    w053a :  WX                            31
075440050706if  1c                   if        *in31
075450050706     c                   clear                   §A8LOG1P1b
075460050706     c                   clear                   §A8LOG1P1c
075470050706     c                   clear                   §A8LOG1P1d
075480050706e   1c                   endif
075490100513     c                   clear                   w053a
075500050706     c                   eval      WX    =  %len(§A8LOG1P1f)
075510100513     c                   eval      w053a =  %subst(dAR8LOG1P1:67:WX)
075520050706     c                   eval      *in31 =  *off
075530110426     c     c_Digits      checkr    w053a :  WX                            31
075540050706if  1c                   if        *in31
075550050706     c                   clear                   §A8LOG1P1f
075560050706e   1c                   endif
075570041019      *
075580041019      * > dati in chiusura DDT
075590041019      * - - aspetto esteriore dei beni
075600050707     c                   eval      PLOG1aeb  = §A8LOG1P1a
075610041019      * - - numero colli
075620050707     c                   eval      PLOG1ncl  = §A8LOG1P1b
075630041019      * - - peso in kg
075640050707     c                   eval      PLOG1pkg  = §A8LOG1P1c
075650041019      * - - volume in mc
075660050707     c                   eval      PLOG1vlm  = §A8LOG1P1d
075670041019      * - - c/assegno
075680050707     c                   eval      PLOG1cva  = §A8LOG1P1e
075690050707     c                   eval      PLOG1cas  = §A8LOG1P1f
075700041020      * - - causale trasporto
075710050707     c                   eval      PLOG1ctr  = §A8LOG1P1g
075720041020      * - - porto
075730050707     c                   eval      PLOG1pto  = §A8LOG1P1h
075740041020      * - - trasporto a cura
075750050707     c                   eval      PLOG1tra  = §A8LOG1P1i
075760041020      * - - trasportatore
075770050707     c                   eval      PLOG1vet  = §A8LOG1P1j
075780050505      *
075790050505      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
075800050505      *
075810050505      * - - Verifica OverFlow
075820080925      /free
075830080925           select;
075840080925             when  NOT *in45;
075850080925               wMaxLine = $MaxLine4;
075860080925             when  *in45   and   NOT *in46;
075870080925               wMaxLine = $MaxLine4_pl1;
075880080925             when  *in45   and   *in46;
075890080925               wMaxLine = $MaxLine4_pl2;
075900080925           endsl;
075910080925      /end-free
075920050505     c                   exsr      Test_OF
075930050505      * - - Calcolo della posizione finale di box e linee separatrici
075940050505     c                   exsr      Posiz_DL
075950050505      * - - Stampa parte finale DDT
075960050505     c                   write     A4prtDL4
075970050505      *
075980050505      * > Stampa box, linee e testi fissi (no logo)  - - - - - - - - -*
075990090331     c                   if            §AR8cxd = *blanks
076000090331     c                             or  DB0fa4  = 'P'
076010090331     c                   write     A4prtDT0
076020090331     c                   endif
076030050505     c                   write     A4prtDL0A
076040050505     c                   write     A4prtDL0B
076050050505      *
076060050505      * Azzero il numero di pagina per l'eventuale Packing-List
076070050505      *   (successiva)
076080050505     c                   clear                   Page
076090041019      *
076100041019      * N.B.: la copia per il destinatario della LDV deve comparire
076110041019      *       nell'ultima pagina del DDT per il cliente (vedi subr.
076120041019      *       "ChiusuraLDV")
076130041019      *
076140041019     c                   ENDSR
076150050318      *
076160050318      *---------------------------------------------------------------*
076170050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
076180050318      *?Dati di packing-list: dettaglio                              ?*
076190050318      *---------------------------------------------------------------*
076200050318     c     PrtDdtLOG1_Z1 BEGSR
076210050318      *
076220050318      * Prima della 1ª riga di packing-list: testata packing-list
076230050505if  1c                   if        Page     = *zeros
076240050318     c                   exsr      PrtPckLst_1
076250050318e   1c                   endif
076260050318      *
076270050318     c                   clear                   A4prtDL5
076280051110     c                   clear                   dAR8LOG1Z1
076290050707     c                   movel     AR8uni        dAR8LOG1Z1
076300050706      *
076310050706      * TEST effettiva numericità dei campi previsti numerici:
076320100513     c                   clear                   w053a
076330050706     c                   eval      WX    =  %len(§A8LOG1Z1d)
076340100513     c                   eval      w053a =  %subst(dAR8LOG1Z1:53:WX)
076350050706     c                   eval      *in31 =  *off
076360110426     c     c_Digits      checkr    w053a :  WX                            31
076370050706if  1c                   if        *in31
076380050706     c                   clear                   §A8LOG1Z1d
076390050706e   1c                   endif
076400050318      *
076410050318      * > dati in dettaglio Packing-List
076420050318      * - - sscc
076430050318     c                   eval      PCsscc   = §A8LOG1Z1a
076440050318      * - - gtin
076450050318     c                   eval      PCgtin   = §A8LOG1Z1b
076460050318      * - - codice prodotto
076470050318     c                   eval      PCcpr2   = §A8LOG1Z1c
076480050318      * - - quantità ul
076490050318     c                   eval      PLOG1QTA = §A8LOG1Z1d
076500050318      * - - tipo contenitore
076510050318     c                   eval      PCtcn    = §A8LOG1Z1e
076520050318      * - - codice contenitore
076530050318     c                   eval      PCccn    = §A8LOG1Z1f
076540050318      * - - numero lotto
076550050318     c                   eval      PClot    = §A8LOG1Z1g
076560050318      * - - data scadenza
076570050318     c                   eval      PCdts    = §A8LOG1Z1h
076580050318      *
076590050318      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
076600050318      *
076610050318     c                   exsr      Test_OF
076620050318     c                   write     A4prtDL5
076630050318      *
076640050318     c                   ENDSR
076650080908      *
076660050606      *---------------------------------------------------------------*
076670050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
076680080925      *?Dati di Fattura: Testata                                     ?*
076690050606      *---------------------------------------------------------------*
076700050606     c     PrtDdtLOG1_S1 BEGSR
076710050606      *
076720050606      * > logo Fattura
076730050606     c                   eval      §LogoF   = 'LOGO' + §AR8logof
076740090107      /free
076750090107         // - Impostazione indicatore che condiziona la stampa del logo
076760120530         //   (o quella del "simil-logo" BRT)
076770090107         *in55 = (%subst(§AR8logof : 1 : 3) = 'LOG'       and
076780090107                  (§AR8tsd  = 'L1'  or  §AR8tsd  = 'L3'));
076790090107      /end-free
076800080908      *
076810060609      *? L O G O !! ?
076820060609      *?vedi stampa testata bolla ?
076830071008     c                   if        (§AR8tsd = 'L1' or §AR8tsd = 'L3') and
076840071008     c                             §AR8logof <> *blanks and
076850071008     c                             %subst(AR8tmd:1:3) <> 'LOG'
076860071008     c                   eval      §LogoF   = 'LOGO' + AR8tmd
076870090107      /free
076880090107           // - Impostaz. indicatore che condiziona la stampa del logo
076890120530           //   (o quella del "simil-logo" BRT)
076900090107           *in55 = (%subst(AR8tmd : 1 : 3) = 'LOG');
076910090107      /end-free
076920060609     c                   endif
076930050606      *
076940051110     c                   clear                   dAR8LOG1S1
076950050606     c                   movel     AR8uni        dAR8LOG1S1
076960050706      *
076970050706      * TEST effettiva numericità dei campi previsti numerici:
076980100513     c                   clear                   w053a
076990050706     c                   eval      WX    =  %len(§A8LOG1S1i)
077000100513     c                   eval      w053a =  %subst(dAR8LOG1S1:216:WX)
077010050706     c                   eval      *in31 =  *off
077020110426     c     c_Digits      checkr    w053a :  WX                            31
077030050706if  1c                   if        *in31
077040050706     c                   clear                   §A8LOG1S1i
077050050706e   1c                   endif
077060050707      *
077070050606      * > intestazione fattura
077080050707if  1c                   if        §A8LOG1S1b =  *blanks
077090050707     c                   clear                   PLOG1rsf
077100050707     c                   eval      PLOG1rsf2  =  §A8LOG1S1a
077110050606x   1c                   else
077120050707     c                   eval      PLOG1rsf   =  §A8LOG1S1a
077130050707     c                   eval      PLOG1rsf2  =  §A8LOG1S1b
077140050606e   1c                   endif
077150050707     c                   eval      PLOG1inf   =  §A8LOG1S1c
077160050707     c                   eval      PLOG1inf2  =  §A8LOG1S1d
077170050707if  1c                   if        §A8LOG1S1e >  *zeros
077180050707     c                   eval      PLOG1caf   =  §A8LOG1S1e
077190050606e   1c                   endif
077200050707     c                   eval      PLOG1lof   =  §A8LOG1S1f
077210050606      * > partita IVA - codice fiscale
077220050707     c                   eval      PLOG1IVA   =  §A8LOG1S1g
077230050606      * > n./data fattura
077240050707     c                   eval      PLOG1nft   =  §A8LOG1S1h
077250050606      *     giro la data fattura
077260050706if  1c                   if        §A8LOG1S1i >  *zeros
077270060922     c                   eval      *in31 =  *off
077280060922     c     *iso          test(d)                 §A8LOG1S1i             31
077290060922if  2c                   if        NOT *in31
077300050707     c     *iso          movel     §A8LOG1S1i    Data_EUR
077310050707     c     *eur          movel     Data_EUR      PLOG1dft
077320060922x   2c                   else
077330060922     c                   clear                   PLOG1dft
077340060922e   2c                   endif
077350050706e   1c                   endif
077360050606      * > rif.ns.spedizione
077370060922     c                   eval      PLOG1nspa =   §A8LOG1S1j
077380050606      * > intestazione corpo fattura
077390050707     c                   eval      PLOG1tesf  =  §A8LOG1S1k
077400050707      *
077410050606      * > Stampa testata FATTURA - - - - - - - - - - - - - - - - - - -*
077420081015      *
077430080925      /free
077440081015         // - Forzatura del salto pagina
077450081015         CurLineA4 = wMaxLine + 1;
077460080925      /end-free
077470080925     c                   exsr      Test_OF
077480081015      /free
077490090323         // - Impostazione dei nuovi limiti di pagina
077500090323         select;
077510090323           when  NOT *in45;
077520090323             wMaxLine = $MaxLine2;
077530090323           when  *in45   and   NOT *in46;
077540090323             wMaxLine = $MaxLine2_pl1;
077550090323           when  *in45   and   *in46;
077560090323             wMaxLine = $MaxLine2_pl2;
077570090323         endsl;
077580081015      /end-free
077590080925     c                   exsr      Posiz_DL
077600081017     c                   if            §AR8cxd = *blanks
077610081017     c                             or  DB0fa4  = 'P'
077620080925     c                   write     A4prtDT0
077630080925     c                   endif
077640080925     c                   write     A4prtDL0D
077650050606     c                   write     A4prtDL1D
077660050606     c                   write     A4prtDL1E
077670050707      *
077680050606     c                   ENDSR
077690050606      *
077700050606      *---------------------------------------------------------------*
077710050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
077720050606      *?Dati di dettaglio corpo: FATTURA                             ?*
077730050606      *---------------------------------------------------------------*
077740050606     c     PrtDdtLOG1_S2 BEGSR
077750050606      *
077760050606     c                   clear                   A4prtDL6
077770051110     c                   clear                   dAR8LOG1S2
077780050606     c                   movel     AR8uni        dAR8LOG1S2
077790050606      *
077800050606      * > dati fattura
077810050606      * - - riga descrittiva 1
077820050707     c                   eval      PLOG1dft1 = §A8LOG1S2a
077830050606      *
077840050606      * > Stampa dettaglio FATTURA - - - - - - - - - - - - - - - - - -*
077850050606      *
077860050606     c                   exsr      Test_OF
077870050606     c                   write     A4prtDL6
077880050606      *
077890050606     c                   ENDSR
077900050606      *
077910050606      *---------------------------------------------------------------*
077920050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
077930050606      *?Dati di dettaglio corpo: FATTURA                             ?*
077940050606      *---------------------------------------------------------------*
077950050606     c     PrtDdtLOG1_S3 BEGSR
077960050606      *
077970050606     c                   clear                   A4prtDL7
077980051110     c                   clear                   dAR8LOG1S3
077990050707     c                   movel     AR8uni        dAR8LOG1S3
078000050606      *
078010050606      * > dati fattura
078020050606      * - - riga descrittiva 2
078030050707     c                   eval      PLOG1dft2 = §A8LOG1S3a
078040050606      *
078050050606      * > Stampa dettaglio FATTURA - - - - - - - - - - - - - - - - - -*
078060050606      *
078070050606     c                   exsr      Test_OF
078080050606     c                   write     A4prtDL7
078090050606      *
078100050606     c                   ENDSR
078110050606      *
078120050606      *---------------------------------------------------------------*
078130050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
078140050606      *?Dati di fondo: FATTURA                                       ?*
078150050606      *---------------------------------------------------------------*
078160050606     c     PrtDdtLOG1_S4 BEGSR
078170050606      *
078180051110     c                   clear                   dAR8LOG1S4
078190050707     c                   movel     AR8uni        dAR8LOG1S4
078200050706      *
078210050706      * TEST effettiva numericità dei campi previsti numerici:
078220100513     c                   clear                   w053a
078230050706     c                   eval      WX    =  %len(§A8LOG1S4a)
078240050706     c                                   +  %len(§A8LOG1S4b)
078250050706     c                                   +  %len(§A8LOG1S4c)
078260100513     c                   eval      w053a =  %subst(dAR8LOG1S4: 1:WX)
078270050706     c                   eval      *in31 =  *off
078280110426     c     c_Digits      checkr    w053a :  WX                            31
078290050706if  1c                   if        *in31
078300050706     c                   clear                   §A8LOG1S4a
078310050706     c                   clear                   §A8LOG1S4b
078320050706     c                   clear                   §A8LOG1S4c
078330050706e   1c                   endif
078340050606      *
078350050606      * > dati in chiusura FATTURA
078360050606      * - - dati di piede fattura riga 1
078370050707     c                   eval      PLOG1pft1 = §A8LOG1S4d
078380050606      * - - dati di piede fattura riga 2
078390050707     c                   eval      PLOG1pft2 = §A8LOG1S4e
078400050606      * - - dati di piede fattura riga 3
078410050707     c                   eval      PLOG1pft3 = §A8LOG1S4f
078420050606      * - - totale imponibile
078430050707     c                   eval      PLOG1timp = §A8LOG1S4a
078440050606      * - - totale iva
078450050707     c                   eval      PLOG1tiva = §A8LOG1S4b
078460050606      * - - totale fattura
078470050707     c                   eval      PLOG1tft  = §A8LOG1S4c
078480050606      *
078490050606      * > Stampa fondo FATTURA - - - - - - - - - - - - - - - - - - - -*
078500050606      *
078510050606      * - - Verifica OverFlow
078520080925      /free
078530080925           select;
078540080925             when  NOT *in45;
078550080925               wMaxLine = $MaxLine4;
078560080925             when  *in45   and   NOT *in46;
078570080925               wMaxLine = $MaxLine4_pl1;
078580080925             when  *in45   and   *in46;
078590080925               wMaxLine = $MaxLine4_pl2;
078600080925           endsl;
078610080925      /end-free
078620050606     c                   exsr      Test_OF
078630050606      * - - Calcolo della posizione finale di box e linee separatrici
078640050606     c                   exsr      Posiz_DL
078650050606      * - - Stampa parte finale FATTURA
078660050606     c                   write     A4prtDL8
078670050606      *
078680050606      * > Stampa box, linee (no logo)   - - - - - - - - - - - - - - -*
078690050606     c                   write     A4prtDL0E
078700050606     c                   write     A4prtDL0F
078710080908      *
078720050606     c                   ENDSR
078730050606      *
078740050606      *---------------------------------------------------------------*
078750050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
078760050606      *?Dati di fondo note: FATTURA                                  ?*
078770050606      *---------------------------------------------------------------*
078780050606     c     PrtDdtLOG1_S5 BEGSR
078790050606      *
078800051110     c                   clear                   dAR8LOG1S5
078810050707     c                   movel     AR8uni        dAR8LOG1S5
078820050606      *
078830050606      * > dati in chiusura FATTURA
078840050606      * - - nota 1
078850050707     c                   eval      PLOG1not1 = §A8LOG1S5a
078860050606      * - - nota 2
078870050707     c                   eval      PLOG1not2 = §A8LOG1S5b
078880050606      * - - nota 3
078890050707     c                   eval      PLOG1not3 = §A8LOG1S5c
078900050606      *
078910050606      * > Stampa fondo FATTURA - - - - - - - - - - - - - - - - - - - -*
078920050606      * - - Stampa parte finale FATTURA
078930050606     c                   write     A4prtDL9
078940080908      *
078950050606     c                   ENDSR
078960050321      *
078970050321      *---------------------------------------------------------------*
078980050414      *?Impostazione nuova pagina di DDT Logistica / Sait            ?*
078990050321      *---------------------------------------------------------------*
079000050321     c     NewPageDL     BEGSR
079010050321      *
079020080925      /free
079030080925         // Impostazione nuovo limite di OF
079040080925         SELECT;
079050080925           // Piede DDT   oppure   Piede Fattura
079060080925           WHEN  AR8trk = 'P1'   or   AR8trk = 'S4';
079070080925             select;
079080080925               when  NOT *in45;
079090080925                 wMaxLine = $MaxLine4;
079100080925               when  *in45   and   NOT *in46;
079110080925                 wMaxLine = $MaxLine4_pl1;
079120080925               when  *in45   and   *in46;
079130080925                 wMaxLine = $MaxLine4_pl2;
079140080925             endsl;
079150080925           // Dettaglio DDT  oppure   Testata & Dettaglio Fattura
079160080925           OTHER;
079170080925             select;
079180080925               when  NOT *in45;
079190090323                 wMaxLine = $MaxLine2;
079200080925               when  *in45   and   NOT *in46;
079210090323                 wMaxLine = $MaxLine2_pl1;
079220080925               when  *in45   and   *in46;
079230090323                 wMaxLine = $MaxLine2_pl2;
079240080925             endsl;
079250080925         ENDSL;
079260080925      /end-free
079270080925      *
079280080825      * RE-impostazione posizioni
079290080825     c                   exsr      Posiz_DL
079300080825      *
079310050321     c                   write     A4prtSP1
079320081006      /free
079330081006         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
079340081006           write  A4prtNUL;
079350081006         endif;
079360081006      /end-free
079370050321      *
079380050505sel 1c                   select
079390050505      * - altra pagina DDT Logistica / Sait
079400050707w   1c                   when          %subst(AR8trk:1:1) <> 'Z'
079410050606     c                             and %subst(AR8trk:1:1) <> 'S'
079420050321     c                   write     A4prtDL1B
079430050505      * - altra pagina Packing-List
079440050505w   1c                   when      %subst(AR8trk:1:1) =  'Z'
079450050502     c                              and   Page        >  *zeros
079460050321     c                   write     A4prtDL1C
079470050606      * - altra pagina Fattura
079480050606w   1c                   when      %subst(AR8trk:1:1) =  'S'
079490050606     c                              and   Page        >  *zeros
079500050606     c                   write     A4prtDL1E
079510050505e   1c                   endsl
079520050321      *
079530050321     c                   ENDSR
079540050318      *
079550050318      *---------------------------------------------------------------*
079560080709      *?Stampa testata della Packing-List per LOGISTICA              ?*
079570050318      *---------------------------------------------------------------*
079580050318     c     PrtPckLst_1   BEGSR
079590050318      *
079600050318      * > Salto a pagina successiva  - - - - - - - - - - - - - - - - -*
079610050318     c                   exsr      Test_OF
079620050318      *
079630050505      * > Stampa box, linee e testi fissi (CON logo)  - - - - - - - - -*
079640050318      *
079650050318      * - - Stampa "Copia per il destinatario" nella pagina successiva
079660081017     c*** poi in Test_OF:   if            §AR8cxd = *blanks
079670081017     c*** poi in Test_OF:             or  DB0fa4  = 'P'
079680050505     c*** poi in Test_OF:   write     A4prtDT0
079690050505     c*** poi in Test_OF:   endif
079700050318      * - - Stampa dati destinatario nel box superiore
079710050318     c                   write     A4prtDL1A
079720050318      * - - Stampa logo e box superiore x destinatario
079730050318     c                   write     A4prtDL0
079740081222      /free
079750081222         if  *in55;
079760090406           exsr   PrtDdtLOG1_SimilLogo;
079770120530           //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
079780120530           //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
079790120530           //  write  A4LogoBR2;
079800120530           //else;
079810120530           //  write  A4LogoBAR;
079820120530           //endif;
079830120530           write  A4LogoBRT;
079840081222           write  A4prtDLL;
079850081222         endif;
079860081222      /end-free
079870050318      * - - Stampa dati di testata nel box generale
079880050318     c                   write     A4prtDL1C
079890050318      * - - Stampa box generale
079900050318     c*** poi in Test_OF:   write     A4prtDL0C
079910050318      *
079920050318      * Impostazione del nuovo limite x overflow in packing-list
079930080925      /free
079940080925         select;
079950080925           when  NOT *in45;
079960090323             wMaxLine = $MaxLine2;
079970080925           when  *in45   and   NOT *in46;
079980090323             wMaxLine = $MaxLine2_pl1;
079990080925           when  *in45   and   *in46;
080000090323             wMaxLine = $MaxLine2_pl2;
080010080925         endsl;
080020080925      /end-free
080030050318      *
080040050318     c                   ENDSR
080050041019      *
080060041020      *---------------------------------------------------------------*
080070050414      *?Posizionamento di linee e box nel DDT LOGISTICA / SAIT       ?*
080080041020      *---------------------------------------------------------------*
080090041021     c     Posiz_DL      BEGSR
080100050218      *
080110080924      /free
080120080924         // Impostazione posizioni di logo/box
080130080924         // (posizioni di "partenza" diverse a seconda del posizionam.
080140080924         //  iniziale nel foglio A4 attualmente in stampa)
080150080924         select;
080160080924           when  NOT *in45;
080170081006             §PBA =  3.255;
080180080924           when  *in45   and   NOT *in46;
080190090422             §PBA = 0.255;
080200080924           when  *in45   and   *in46;
080210090422             §PBA = 6.005;
080220080924         endsl;
080230080924      /end-free
080240080708     c                   eval      §PBB  =  §PBA + 1.27
080250080708     c                   eval      §LOA  =  §PBA + 0.63
080260080708     c                   eval      §PS0  =  §PBA + 0.72
080270080708     c                   eval      §PS1  =  §PS0 + 0.10
080280080708     c                   eval      §PS2  =  §PS1 + 0.10
080290080708     c                   eval      §PS3  =  §PS2 + 0.10
080300080708     c                   eval      §PS4  =  §PS3 + 0.10
080310080708     c                   eval      §PS5  =  §PS4 + 0.10
080320080708      * Impostazione posizioni di logo/box per fattura
080330080708     c                   eval      §PBC  =  §PBA + 0.92
080340080708     c                   eval      §LOB  =  §PBA + 0.72
080350080708      * Impostazione posizioni di note fine fattura
080360080925     c                   eval      §PS6  =  §PBA + 5.35
080370081003     c                   eval      §PS7  =  §PS6 + 0.08
080380081003     c                   eval      §PS8  =  §PS7 + 0.08
080390080708      *
080400050321if  1c                   if        Page  <= 1
080410050218      *   (posizione di "partenza" su 1ª pagina)
080420080924     c                   eval      §PB1  =  §PBA + 1.375
080430050218x   1c                   else
080440050318      *   (posizione di "partenza" su pagine successive)
080450080822     c                   eval      §PB1  =  §PBA
080460050218e   1c                   endif
080470080924     c                   eval      §PB2  =  §PBA + 5.275
080480050218      * Impostazione posizioni di linee (orizzontali e verticali)
080490050218      *   nella parte superiore del box (testata)
080500050218     c                   eval      §LO1  =  §PB1 + 0.13
080510050318if  1c                   if        %subst(AR8trk:1:1) <> 'Z'
080520050218     c                   eval      §LO2  =  §LO1 + 0.13
080530050318     c                   eval      §LO3  =  §LO2 + 0.11
080540050318x   1c                   else
080550080825     c                   eval      §LO2  =  §LO1 + 0.115
080560050318     c                   eval      §LO3  =  §LO2
080570050318e   1c                   endif
080580050607if  1c                   if        %subst(AR8trk:1:1) = 'S'
080590050610     c                   eval      §LO3  =  §PB2 - 0.40
080600050607     c                   EndIf
080610050218      * Impostazione posizioni di linee (orizzontali e verticali)
080620050218      *   nella parte inferiore del box (piede)
080630050318if  1c                   if        %subst(AR8trk:1:1) <> 'Z'
080640080822     c                   eval      §LO4  =  §PB2 - 0.52
080650090422     c                   eval      §LO5  =  §LO4 + 0.13
080660090422     c                   eval      §LO6  =  §LO5 + 0.13
080670090422     c                   eval      §LO7  =  §LO6 + 0.13
080680050318e   1c                   endif
080690050218      * Impostazione lunghezza delle linee verticali
080700050218      *   nella parte superiore del box (testata)
080710050218     c                   eval      §LV1  =  §LO1 - §PB1
080720050218     c                   eval      §LV2  =  §LO2 - §LO1
080730050318if  1c                   if        %subst(AR8trk:1:1) <> 'Z'
080740050218     c                   eval      §LV3  =  §LO3 - §LO2
080750050318e   1c                   endif
080760050607if  1c                   if        %subst(AR8trk:1:1) = 'S'
080770050607     c                   eval      §LV2  =  0.40
080780050607     c                   EndIf
080790050218      * La lunghezza delle linee verticali separatrici nel dettaglio
080800050218      *   dipende dalla fase di stampa: se si sta stampando ancora il
080810050218      *   dettaglio deve essere più lunga, visto che non viene stampa-
080820050218      *   ta la parte finale del DDT...
080830080924if  1c                   if        %subst(AR8trk:1:1) = 'P'
080840080924     c                              and  (§PS1        < $LimOF
080850080924     c                                and NOT *in45
080860080924     c                                OR  §PS1        < ($LimOF - c_Len1_inch)
080870080924     c                                and *in45   and   NOT *in46
080880081003     c                                OR  §PS1        < ($LimOF + c_Len2_inch)
080890080924     c                                and *in45   and   *in46)
080900050318     c                              and   CurLineA4   <  wMaxLine
080910050218     c                   eval      §LV4  =  §LO4 - §LO3
080920050218x   1c                   else
080930050218     c                   eval      §LV4  =  §PB2 - §LO3
080940050218e   1c                   endif
080950080908      *
080960071008     c                   if        §AR8tsd = 'L3' or §AR8tsd = 'L4' and
080970060609     c                             %subst(AR8trk:1:1) <> 'Z' and
080980060609     c                             %subst(AR8trk:1:1) <> 'S'
080990071008     c                   eval      §LV3 = 0
081000071008     c                   eval      §LV4 = 0
081010060609     c                   endif
081020080908      *
081030050502      * - qui imposto le posizioni per il piede del box
081040050318if  1c                   if        %subst(AR8trk:1:1) <> 'Z'
081050050218     c                   eval      §LV5  =  §LO5 - §LO4
081060050218     c                   eval      §LV6  =  §LO6 - §LO5
081070050218     c                   eval      §LV7  =  §PB2 - §LO7
081080050318e   1c                   endif
081090041028      * Impostazione posizioni dei campi di testo verticali
081100080822     c                   eval      §PR1  =  §PB2 - 0.05
081110041020      *
081120041020     c                   ENDSR
081130050113      *
081140050113      *---------------------------------------------------------------*
081150050217      *?Stampa DDT del mittente ARCTE                                ?*
081160050113      *---------------------------------------------------------------*
081170050217     c     DDT_Arcte     BEGSR
081180050113      *
081190081002      /free
081200081002         // Impostazione del nuovo limite x overflow
081210081002         // (Codesto limite è valido solo per la stampa del dettaglio.
081220081002         //   Per la stampa del "fondo" bolla occorre REimpostarlo ad
081230081002         //   un valore più basso.
081240081002         //  Ovvero: stampo più righe di dettaglio che posso; se alla
081250081002         //  fine delle righe di dettaglio non rimane spazio a suffi-
081260081002         //  cienza per il fondo: prima della stampa del fondo si salta
081270081002         //  alla pagina successiva, dove non si stampa alcuna riga di
081280081002         //  dettaglio.)
081290081002         select;
081300081002           when  NOT *in45;
081310081002             wMaxLine = $MaxLine2;
081320081002           when  *in45   and   NOT *in46;
081330090324             wMaxLine = $MaxLine5_pl1;
081340081002           when  *in45   and   *in46;
081350090324             wMaxLine = $MaxLine5_pl2;
081360081002         endsl;
081370081002
081380081002         // Posizionamento iniziale all'interno del foglio A4
081390081002         write  A4prtSP1;
081400081002      /end-free
081410050113      *
081420050113      * Impostazione testi fissi - - - - - - - - - - - - - - - - - - -*
081430050113      *
081440050113     c                   eval      PCLtx1 =
081450050113     c                             'La merce viaggia a rischio e pericolo -
081460050113     c                              del committente. I reclami vanno fatti -
081470050113     c                              per iscritto entro 8 gg. dal ricevimento -
081480050113     c                              delle merce.'
081490050113     c                   eval      PCLtx2 =
081500050113     c                              'Il ritorno di merce deve -
081510050113     c                              essere preventivamente autorizzato -
081520050113     c                              dal Gruppo Arcte. Foro competente -
081530050113     c                              come da condizioni di vendita.'
081540050113      *
081550050113     c                   eval      PCLpx1 =
081560050113     c                             'CONTRIBUTO CONAI ASSOLTO'
081570050217      *
081580130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
081590130315      *  "StampaDDT")
081600050217     c***  KLDV          setll     FIAR8000       (già fatto)
081610050217     c***  KLDV          reade     FIAR8000       (già fatto)
081620050113      *
081630050113      * Reperimento dati, impostazione e stampa DDT mittente  - - - -*
081640050113      *
081650050113if  1c                   dow       NOT %eof(FIAR801L)
081660050113      *
081670050113sel 2c                   select
081680050113      * - Record annullato (non si stampa nulla)
081690050217w   2c                   when      AR8atb  <> *blanks
081700050707      * - Dati di dettaglio: descrizioni generiche
081710050217w   2c                   when      AR8trk   = 'C1'
081720050217     c                   exsr      PrtDdtARCT_C1
081730050217      * - Dati di dettaglio: articolo, descrizioni, taglie, q.tà
081740050217w   2c                   when      AR8trk   = 'C2'
081750050217     c                   exsr      PrtDdtARCT_C2
081760050113      * - Dati di testata: destinatario, cliente di fatturazione
081770050217w   2c                   when      AR8trk   = 'T1'
081780050217     c                   exsr      PrtDdtARCT_T1
081790050113      * - Dati di testata: agente, numero ordine, estremi documento
081800050217w   2c                   when      AR8trk   = 'T2'
081810050217     c                   exsr      PrtDdtARCT_T2
081820050113      * - Dati di fondo: mezzo e causale trasporto, colli, peso
081830050217w   2c                   when      AR8trk   = 'P1'
081840050217     c                   exsr      PrtDdtARCT_P1
081850050113e   2c                   endsl
081860050113      *
081870050113     c     KLDV          reade     FIAR8000
081880050113      *
081890050113e   1c                   enddo
081900050217      *
081910050217      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
081920050217      *
081930050217      * - - Verifica OverFlow
081940050217     c                   exsr      Test_OF
081950050218      *
081960050218      * - - Calcolo la posizione finale del box e di linee separatrici
081970050218     c                   exsr      Posiz_AC
081980050217      *
081990050217      * - - Stampa parte finale DDT
082000050217     c                   write     A4prtAC4
082010050217      *
082020050217      * > Stampa box, linee e testi fissi (no logo)  - - - - - - - - -*
082030081017     c                   if            §AR8cxd = *blanks
082040081017     c                             or  DB0fa4  = 'P'
082050050331     c                   write     A4prtDT0
082060050331     c                   endif
082070050217     c                   write     A4prtAC0A
082080050217     c                   write     A4prtAC0B
082090050217      *
082100050217      * Reimpostazione del vecchio limite x overflow
082110081002      /free
082120081002         select;
082130081002           when  NOT *in45;
082140081002             wMaxLine = $MaxLine2;
082150081002           when  *in45   and   NOT *in46;
082160081002             wMaxLine = $MaxLine2_pl1;
082170081002           when  *in45   and   *in46;
082180081002             wMaxLine = $MaxLine2_pl2;
082190081002         endsl;
082200081002      /end-free
082210050113      *
082220050113     c                   ENDSR
082230050214      *
082240050113      *---------------------------------------------------------------*
082250050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
082260050217      *?Dati di testata: destinatario, cliente di fatturazione       ?*
082270050113      *---------------------------------------------------------------*
082280050217     c     PrtDdtARCT_T1 BEGSR
082290050114      *
082300050114      * > logo
082310050114     c                   eval      §Logo  = 'LOGO' + AR8tmd
082320050113      *
082330051110     c                   clear                   dAR8ARCTT1
082340050707     c                   movel     AR8uni        dAR8ARCTT1
082350050706      *
082360050706      * TEST effettiva numericità dei campi previsti numerici:
082370100513     c                   clear                   w053a
082380050706     c                   eval      WX    =  %len(§ARCTT1a02)
082390100513     c                   eval      w053a =  %subst(dAR8ARCTT1:  7:WX)
082400050706     c                   eval      *in31 =  *off
082410110426     c     c_Digits      checkr    w053a :  WX                            31
082420050706if  1c                   if        *in31
082430050706     c                   clear                   §ARCTT1a02
082440050706e   1c                   endif
082450050706     c                   eval      WX    =  %len(§ARCTT1b02)
082460100513     c                   eval      w053a =  %subst(dAR8ARCTT1:163:WX)
082470050706     c                   eval      *in31 =  *off
082480110426     c     c_Digits      checkr    w053a :  WX                            31
082490050706if  1c                   if        *in31
082500050706     c                   clear                   §ARCTT1b02
082510050706e   1c                   endif
082520050113      *
082530050113      * > destinatario
082540050707     c                   eval      PARCTccd  = §ARCTT1a01
082550050707     c                   eval      PARCTccd1 = §ARCTT1a02
082560050707     c                   eval      PARCTrsd  = §ARCTT1a04
082570050707     c                   eval      PARCTrsd2 = §ARCTT1a05
082580050707     c                   eval      PARCTind  = §ARCTT1a06
082590050707     c                   eval      PARCTcad  = §ARCTT1a07
082600050707     c                   eval      PARCTlod  = §ARCTT1a08
082610050113      * > cliente di fatturazione
082620050707     c                   eval      PARCTccf  = §ARCTT1b01
082630050707     c                   eval      PARCTccf1 = §ARCTT1b02
082640050707     c                   eval      PARCTccf2 = §ARCTT1b03
082650050707     c                   eval      PARCTrsf  = §ARCTT1b04
082660050707     c                   eval      PARCTrsf2 = §ARCTT1b05
082670050707     c                   eval      PARCTinf  = §ARCTT1b06
082680050707     c                   eval      PARCTcaf  = §ARCTT1b07
082690050707     c                   eval      PARCTlof  = §ARCTT1b08
082700050113      * > magazzino spedizione
082710050707     c                   eval      PARCTmag  = §ARCTT1c01
082720050113      *
082730050113     c                   ENDSR
082740050113      *
082750050113      *---------------------------------------------------------------*
082760050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
082770050217      *?Dati di testata: documento                                   ?*
082780050113      *---------------------------------------------------------------*
082790050217     c     PrtDdtARCT_T2 BEGSR
082800050113      *
082810051110     c                   clear                   dAR8ARCTT2
082820050707     c                   movel     AR8uni        dAR8ARCTT2
082830050706      *
082840050706      * TEST effettiva numericità dei campi previsti numerici:
082850100513     c                   clear                   w053a
082860050706     c                   eval      WX    =  %len(§ARCTT2a02)
082870050706     c                                   +  %len(§ARCTT2a03)
082880100513     c                   eval      w053a =  %subst(dAR8ARCTT2: 9:WX)
082890050706     c                   eval      *in31 =  *off
082900110426     c     c_Digits      checkr    w053a :  WX                            31
082910050706if  1c                   if        *in31
082920050706     c                   clear                   §ARCTT2a02
082930050706     c                   clear                   §ARCTT2a03
082940050706e   1c                   endif
082950050113      *
082960050113      * > dati in testata DDT
082970050113      * - - numero e data documento (DDT)
082980050707     c                   eval      PARCTnsr = §ARCTT2a01
082990050707     c                   eval      PARCTnsp = §ARCTT2a02
083000050707     c                   eval      PARCTdsp = §ARCTT2a03
083010050113      *
083020050113      * > Stampa testata DDT - - - - - - - - - - - - - - - - - - - - -*
083030050113      *
083040050113      *   - OverFlow impossibile qui! -
083050050217     c***                exsr      Test_OF
083060050217     c                   exsr      Posiz_AC
083070081017     c                   if            §AR8cxd = *blanks
083080081017     c                             or  DB0fa4  = 'P'
083090050331     c                   write     A4prtDT0
083100050331     c                   endif
083110050217     c                   write     A4prtAC0
083120050217     c*** quante righe?  write     A4prtAC0A
083130050217     c                   write     A4prtAC1A
083140050217     c                   write     A4prtAC1B
083150050113      *
083160050113     c                   ENDSR
083170050214      *
083180050113      *---------------------------------------------------------------*
083190050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
083200050217      *?Dati di dettaglio: descrizioni generiche                     ?*
083210050113      *---------------------------------------------------------------*
083220050217     c     PrtDdtARCT_C1 BEGSR
083230050113      *
083240050114     c                   clear                   A4prtAC2
083250051110     c                   clear                   dAR8ARCTC1
083260050707     c                   movel     AR8uni        dAR8ARCTC1
083270050217      * > dati in note DDT
083280050217      * - - riga descrittiva
083290050707     c                   eval      PARCTdpr  = §ARCTC1a01
083300050113      *
083310050113      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
083320050113      *
083330050113     c                   exsr      Test_OF
083340050114     c                   write     A4prtAC2
083350050113      *
083360050113     c                   ENDSR
083370050113      *
083380050113      *---------------------------------------------------------------*
083390050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
083400050217      *?Dati di dettaglio: descrizione, taglie, qtà                  ?*
083410050113      *---------------------------------------------------------------*
083420050217     c     PrtDdtARCT_C2 BEGSR
083430050113      *
083440050114     c                   clear                   A4prtAC3
083450051110     c                   clear                   dAR8ARCTC2
083460050707     c                   movel     AR8uni        dAR8ARCTC2
083470050706      *
083480050706      * TEST effettiva numericità dei campi previsti numerici:
083490100513     c                   clear                   w053a
083500050706     c                   eval      WX    =  %len(§ARCTC2b00)
083510050706     c                                   +  %len(§ARCTC2b01)
083520050706     c                                   +  %len(§ARCTC2b02)
083530050706     c                                   +  %len(§ARCTC2b03)
083540050706     c                                   +  %len(§ARCTC2b04)
083550050706     c                                   +  %len(§ARCTC2b05)
083560050706     c                                   +  %len(§ARCTC2b06)
083570050706     c                                   +  %len(§ARCTC2b07)
083580050706     c                                   +  %len(§ARCTC2b08)
083590050706     c                                   +  %len(§ARCTC2b09)
083600100513     c                   eval      w053a =  %subst(dAR8ARCTC2:59:WX)
083610050706     c                   eval      *in31 =  *off
083620110426     c     c_Digits      checkr    w053a :  WX                            31
083630050706if  1c                   if        *in31
083640050706     c                   clear                   §ARCTC2b00
083650050706     c                   clear                   §ARCTC2b01
083660050706     c                   clear                   §ARCTC2b02
083670050706     c                   clear                   §ARCTC2b03
083680050706     c                   clear                   §ARCTC2b04
083690050706     c                   clear                   §ARCTC2b05
083700050706     c                   clear                   §ARCTC2b06
083710050706     c                   clear                   §ARCTC2b07
083720050706     c                   clear                   §ARCTC2b08
083730050706     c                   clear                   §ARCTC2b09
083740050706e   1c                   endif
083750050113      *
083760050113      * > dati in dettaglio DDT
083770050113      * - - descrizione articolo
083780050707     c                   eval      PARCTcpr  = §ARCTC2baa
083790050113      * - - taglia 0
083800050707     c                   eval      PARCTt0   = §ARCTC2b00
083810050113      * - - taglia 1
083820050707     c                   eval      PARCTt1   = §ARCTC2b01
083830050113      * - - taglia 2
083840050707     c                   eval      PARCTt2   = §ARCTC2b02
083850050113      * - - taglia 3
083860050707     c                   eval      PARCTt3   = §ARCTC2b03
083870050113      * - - taglia 4
083880050707     c                   eval      PARCTt4   = §ARCTC2b04
083890050113      * - - taglia 5
083900050707     c                   eval      PARCTt5   = §ARCTC2b05
083910050113      * - - taglia 6
083920050707     c                   eval      PARCTt6   = §ARCTC2b06
083930050113      * - - taglia 7
083940050707     c                   eval      PARCTt7   = §ARCTC2b07
083950050113      * - - taglia 8
083960050707     c                   eval      PARCTt8   = §ARCTC2b08
083970050113      * - - quantità
083980050707     c                   eval      PARCTqta  = §ARCTC2b09
083990050113      *
084000050113      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
084010050113      *
084020050113     c                   exsr      Test_OF
084030050114     c                   write     A4prtAC3
084040050113      *
084050050113     c                   ENDSR
084060050113      *
084070050113      *---------------------------------------------------------------*
084080050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
084090050217      *?Dati di fondo: beni, colli, peso, tot.qtà, porto, trasp.     ?*
084100050113      *---------------------------------------------------------------*
084110050217     c     PrtDdtARCT_P1 BEGSR
084120090324      *
084130090324      /free
084140090324           select;
084150090324             when  NOT *in45;
084160090324               wMaxLine = $MaxLine4;
084170090324             when  *in45   and   NOT *in46;
084180090324               wMaxLine = $MaxLine4_pl3;
084190090324             when  *in45   and   *in46;
084200090324               wMaxLine = $MaxLine4_pl4;
084210090324           endsl;
084220090324      /end-free
084230050113      *
084240051110     c                   clear                   dAR8ARCTP1
084250050707     c                   movel     AR8uni        dAR8ARCTP1
084260050706      *
084270050706      * TEST effettiva numericità dei campi previsti numerici:
084280100513     c                   clear                   w053a
084290050706     c                   eval      WX    =  %len(§ARCTP1a03)
084300100513     c                   eval      w053a =  %subst(dAR8ARCTP1:28:WX)
084310050706     c                   eval      *in31 =  *off
084320110426     c     c_Digits      checkr    w053a :  WX                            31
084330050706if  1c                   if        *in31
084340050706     c                   clear                   §ARCTP1a03
084350050706e   1c                   endif
084360050706     c                   eval      WX    =  %len(§ARCTP1b02)
084370050706     c                                   +  %len(§ARCTP1b03)
084380050706     c                                   +  %len(§ARCTP1b04)
084390100513     c                   eval      w053a =  %subst(dAR8ARCTP1:68:WX)
084400050706     c                   eval      *in31 =  *off
084410110426     c     c_Digits      checkr    w053a :  WX                            31
084420050706if  1c                   if        *in31
084430050706     c                   clear                   §ARCTP1b02
084440050706     c                   clear                   §ARCTP1b03
084450050706     c                   clear                   §ARCTP1b04
084460050706e   1c                   endif
084470050113      *
084480050113      * > dati in chiusura DDT
084490050114      * - - costanti di fine doc. e n. borderò
084500050707     c                   eval      PARCTc1   = §ARCTP1a01
084510050707     c                   eval      PARCTc2   = §ARCTP1a02
084520050707     c                   eval      PARCTc3   = §ARCTP1a03
084530050114      * - - costante segue fattura
084540050707     c                   eval      PARCTc4   = §ARCTp1a04
084550050113      * - - aspetto esteriore dei beni
084560050707     c                   eval      PARCTaeb  = §ARCTP1b01
084570050113      * - - numero colli
084580050707     c                   eval      PARCTncl  = §ARCTP1b02
084590050113      * - - peso in kg
084600050707     c                   eval      PARCTpkg  = §ARCTP1b03
084610050113      * - - totale qtà
084620050707     c                   eval      PARCTtqta = §ARCTP1b04
084630050113      * - - causale trasporto
084640050707     c                   eval      PARCTctr  = §ARCTP1c01
084650050113      * - - porto
084660050707     c                   eval      PARCTpto  = §ARCTP1c02
084670050113      * - - trasporto a cura
084680050707     c                   eval      PARCTtra  = §ARCTP1c03
084690050113      * - - trasportatore
084700050707     c                   eval      PARCTvet  = §ARCTP1d01
084710050217     *** *
084720050217     *** * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
084730050217     *** * ?(impostata a fine ciclo di lettura - sr. DDT_Arcte)?
084740050113      *
084750050113      * N.B.: la copia per il destinatario della LDV deve comparire
084760050113      *       nell'ultima pagina del DDT per il cliente (vedi subr.
084770050113      *       "ChiusuraLDV")
084780050113      *
084790050113     c                   ENDSR
084800081002      /free
084810081002       //--------------------------------------------------------------
084820140307       //?Impostazione nuova pagina di DDT Arcte                       ?
084830081002       //--------------------------------------------------------------
084840081002       BEGSR  NewPageAC;
084850081002
084860081002         // Impostazione del nuovo limite x overflow
084870081002         select;
084880081002           when  NOT *in45;
084890081002             wMaxLine = $MaxLine2;
084900081002           when  *in45   and   NOT *in46;
084910090324             wMaxLine = $MaxLine5_pl1;
084920081002           when  *in45   and   *in46;
084930090324             wMaxLine = $MaxLine5_pl2;
084940081002         endsl;
084950081002
084960081002         write  A4prtSP1;
084970081002         write  A4prtAC1B;
084980081002
084990081002       ENDSR;
085000081002      /end-free
085010050113      *
085020050113      *---------------------------------------------------------------*
085030050217      *?Posizionamento di linee e box nel DDT ARCTE                  ?*
085040050113      *---------------------------------------------------------------*
085050050114     c     Posiz_AC      BEGSR
085060080708      *
085070081002      /free
085080081002         // Impostazione posizioni di logo/box
085090081002         // (posizioni di "partenza" diverse a seconda del posizionam.
085100081002         //  iniziale nel foglio A4 attualmente in stampa)
085110081002         select;
085120081002           when  NOT *in45;
085130081002             §PBA =  3.130;
085140081002           when  *in45   and   NOT *in46;
085150081002             §PBA = 0.260;
085160081002           when  *in45   and   *in46;
085170081002             §PBA = 6.010;
085180081002         endsl;
085190081002      /end-free
085200080708     c                   eval      §PBB  =  §PBA + 1.27
085210080708     c                   eval      §LOA  =  §PBA + 0.63
085220050218      *
085230050217      * Impostazione posizioni di box
085240050217if  1c                   if        Page  <= 1
085250050217      *   (posizione di "partenza" su 1ª pagina)
085260081002     c                   eval      §PB1  =  §PBA + 1.38
085270050217x   1c                   else
085280050217      *   (posizione di "partenza" su pagine successive)
085290081002     c                   eval      §PB1  =  §PBA
085300050217e   1c                   endif
085310081002     c                   eval      §PB2  =  §PBA + 5.395
085320050217      * Impostazione posizioni di linee (orizzontali e verticali)
085330050217      *   nella parte superiore del box (testata)
085340080708     c                   eval      §LO1  =  §PB1 + 0.13
085350080708     c                   eval      §LO2  =  §LO1 + 0.13
085360050217      * Impostazione posizioni di linee (orizzontali e verticali)
085370050217      *   nella parte inferiore del box (piede)
085380081002     c                   eval      §LO3  =  §PBA + 5.005
085390081002     c                   eval      §LO4  =  §PBA + 5.135
085400081002     c                   eval      §LO5  =  §PBA + 5.265
085410050217      * Impostazione lunghezza delle linee verticali
085420050217      *   nella parte superiore del box (testata)
085430050217     c                   eval      §LV1  =  §LO1 - §PB1
085440050217     c                   eval      §LV2  =  §LO2 - §LO1
085450050217      * La lunghezza delle linee verticali separatrici nel dettaglio
085460050217      *   dipende dalla fase di stampa: se si sta stampando ancora il
085470050217      *   dettaglio deve essere più lunga, visto che non viene stampa-
085480050217      *   ta la parte finale del DDT...
085490050217if  1c                   if        %subst(AR8trk:1:1) = 'P'
085500050218     c                             and    §PS1        < $LimOF
085510050218     c                             and    CurLineA4   < wMaxLine
085520050217     c                   eval      §LV3  =  §LO3 - §LO2
085530050217x   1c                   else
085540050217     c                   eval      §LV3  =  §PB2 - §LO2
085550050217e   1c                   endif
085560050217     c                   eval      §LV4  =  §LO4 - §LO3
085570050217     c                   eval      §LV5  =  §LO5 - §LO4
085580050113      * Impostazione posizioni dei campi di testo verticali
085590081002     c                   eval      §PR1  =  §PBA + 5.35
085600081002     c                   eval      §PR2  =  §PBA + 5.47
085610050113      *
085620050113     c                   ENDSR
085630050214      *
085640050214      *---------------------------------------------------------------*
085650050217      *?Stampa DDT del mittente Esprinet                             ?*
085660050214      *---------------------------------------------------------------*
085670050214     c     DDT_Esprinet  BEGSR
085680050214      *
085690081002      /free
085700110405         // -?Impostazione del nuovo limite x overflow?
085710110405         //  ?Codesto limite è valido solo per la stampa del dettaglio.?
085720110405         //  ?Per la stampa del "fondo" bolla occorre REimpostarlo ad?
085730110405         //  ?un valore più basso.?
085740110405         //  ?Ovvero: stampo più righe di dettaglio che posso; se alla?
085750110405         //  ?fine delle righe di dettaglio non rimane spazio a suffi-?
085760110405         //  ?cienza per il fondo: prima della stampa del fondo si salta?
085770110405         //  ?alla pagina successiva, dove non si stampa alcuna riga di?
085780110405         //  ?dettaglio.?
085790081002         select;
085800081002           when  NOT *in45;
085810081002             wMaxLine = $MaxLine2;
085820081002           when  *in45   and   NOT *in46;
085830090324             wMaxLine = $MaxLine5_pl1;
085840081002           when  *in45   and   *in46;
085850090324             wMaxLine = $MaxLine5_pl2;
085860081002         endsl;
085870081002
085880110405         // -?Posizionamento iniziale all'interno del foglio A4?
085890081002         write  A4prtSP1;
085900110405
085910110405         // -?Pulizia tipo record?
085920110405         clear  SAVtrk;
085930081002      /end-free
085940050214      *
085950050214      * Impostazione testi fissi - - - - - - - - - - - - - - - - - - -*
085960050214      *
085970050325     ***c                   eval      PCLtx1 =
085980050325     ***c                             'Settimo Torinese - Tel. 011.895.39.29 *-
085990050325     ***c                              Padova - Tel. 049.870.50.40 *-
086000050325     ***c                              Bologna - Tel. 051.324.249 *-
086010050325     ***c                              Osimo - Tel. 071.781.96.75 *-
086020050325     ***c                              Roma - Tel. 06.723.15.43'
086030050325     ***c                   eval      PCLtx2 =
086040050325     ***c                             'Casalnuovo - Tel. 081.522.30.31 *-
086050050325     ***c                              Cinisello B. - Tel. 02.66.04.11.13 *-
086060050325     ***c                              Verona - Tel. 045.823.01.65 *-
086070050325     ***c                              Cagliari - Tel. 070.253.178'
086080050325     c                   eval      PCLtx3 =
086090131017     c                             'CONTRIBUTO AMBIENTALE CONAI ASSOLTO'
086100131017     c*//                           OVE DOVUTO'
086110050214      *
086120050214     c                   eval      PCLpx2 =
086130050214     c                             'IL PRESENTE DOCUMENTO E'' REGOLATO-
086140050214     c                              DALLE CONDIZIONI GENERALI DI VENDI-
086150050214     c                             TA PUBBLICATE SUL SITO INTERNET ALL-
086160050214     c                             ''INDIRIZZO www.esprinet.com CHE SI-
086170050325     c                              INTENDONO ESPRESSAMENTE CONOSCIUTE-
086180050325     c                              ED ACCETTATE'
086190050214      *
086200050214     c                   eval      PCLpx3a =
086210050214     c                             'NB. IN CASO DI RITARDATO PAGAMENTO-
086220050214     c                             , VERRANNO ADDEBITATI'
086230050214     c                   eval      PCLpx3b =
086240050214     c                             'INTERESSI AL TASSO DEL PRIME RATE -
086250050214     c                             MAGGIORATO DEL 2% A'
086260050214     c                   eval      PCLpx3c =
086270050214     c                             'DECORRERE DAL GIORNO SUCCESSIVO AL-
086280050214     c                             LA SCADENZA DELLA FATTURA'
086290050214      *
086300050214     c                   eval      PCLpx4a =
086310050214     c                             'Si prega di verificare la Vs. ragi-
086320050214     c                             one sociale, indirizzo e'
086330050214     c                   eval      PCLpx4b =
086340050214     c                             'partita IVA, dandoci eventuale sol-
086350050214     c                             lecita rettifica scritta.'
086360050214     c***                eval      PCLpx4c = *blanks
086370050214      *
086380050214     c                   eval      PCLpx5a =
086390050214     c                             '* I PRODOTTI IMPORTATI DAGLI U.S.A.-
086400050214     c                              SONO CON SPECIALE'
086410050214     c                   eval      PCLpx5b =
086420050214     c                             'LICENZA CHE PRECLUDE RIESPORTAZIONI-
086430050214     c                              NON AUTORIZZATE.'
086440050214     c***                eval      PCLpx5c = *blanks
086450050214      *
086460050325     ***c                   eval      PCLpx6  =
086470050325     ***c                             'CONTRIBUTO AMBIENTALE CONAI ASSOLTO-
086480050325     ***c                              OVE DOVUTO'
086490050325     c                   eval      PCLpx6  =
086500050325     c                             'LA SOCIETÀ SI IMPEGNA A VERSARE L''-
086510050325     c                             IVA NEI MODI E TERMINI PREVISTI (S-
086520050325     c                             OLIDARIETÀ ART. 60 BIS DPR 633/72-F-
086530050325     c                             INANZIARIA 2005)'
086540050214      *
086550130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
086560130315      *  "StampaDDT")
086570050214     c***  KLDV          setll     FIAR8000       (già fatto)
086580050214     c***  KLDV          reade     FIAR8000       (già fatto)
086590050214      *
086600050214      * Reperimento dati, impostazione e stampa DDT mittente  - - - -*
086610050214      *
086620050214if  1c                   dow       NOT %eof(FIAR801L)
086630050214      *
086640050214sel 2c                   select
086650050214      * - Record annullato (non si stampa nulla)
086660050217w   2c                   when      AR8atb  <> *blanks
086670050214      * - Dati di dettaglio: articolo, descrizione, quantità, ...
086680050217w   2c                   when      AR8trk   = 'C1'
086690050217     c                   exsr      PrtDdtESP1_C1
086700050214      * - Dati di dettaglio: testo (riga libera)
086710050217w   2c                   when      AR8trk   = 'C2'
086720050217     c                   exsr      PrtDdtESP1_C2
086730050214      * - Dati di testata: destinatario, cliente di fatturazione
086740050217w   2c                   when      AR8trk   = 'T1'
086750050217     c                   exsr      PrtDdtESP1_T1
086760050214      * - Dati di testata: estremi documento, riferimenti, ...
086770050217w   2c                   when      AR8trk   = 'T2'
086780050217     c                   exsr      PrtDdtESP1_T2
086790050214      * - Dati di fondo: totali del documento (netto merce, iva, ...)
086800050217w   2c                   when      AR8trk   = 'P1'
086810050217     c                   exsr      PrtDdtESP1_P1
086820050214      * - Dati di fondo: informazioni vettore
086830050217w   2c                   when      AR8trk   = 'P2'
086840050217     c                   exsr      PrtDdtESP1_P2
086850050214e   2c                   endsl
086860110405      *
086870110405     c                   eval      SAVtrk = AR8trk
086880050214      *
086890050214     c     KLDV          reade     FIAR8000
086900050214      *
086910050214e   1c                   enddo
086920050215      *
086930050215      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
086940050215      *
086950050215      * - - Verifica OverFlow
086960050215     c                   exsr      Test_OF
086970050218      *
086980050218      * - - Calcolo la posizione finale del box e di linee separatrici
086990050218     c                   exsr      Posiz_ES
087000050215      *
087010050215      * - - Stampa parte finale DDT
087020050215     c                   write     A4prtES4
087030050215      *
087040050215      * > Stampa box, linee e testi fissi (no logo)  - - - - - - - - -*
087050081017     c                   if            §AR8cxd = *blanks
087060081017     c                             or  DB0fa4  = 'P'
087070050331     c                   write     A4prtDT0
087080050331     c                   endif
087090050215     c                   write     A4prtES0A
087100050215     c                   write     A4prtES0B
087110050214      *
087120050214      * Reimpostazione del vecchio limite x overflow
087130081002      /free
087140081002         select;
087150081002           when  NOT *in45;
087160081002             wMaxLine = $MaxLine2;
087170081002           when  *in45   and   NOT *in46;
087180081002             wMaxLine = $MaxLine2_pl1;
087190081002           when  *in45   and   *in46;
087200081002             wMaxLine = $MaxLine2_pl2;
087210081002         endsl;
087220081002      /end-free
087230050214      *
087240050214     c                   ENDSR
087250050214      *
087260050214      *---------------------------------------------------------------*
087270050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
087280050217      *?Dati di testata: fatturato a, destinazione, cliente          ?*
087290050214      *---------------------------------------------------------------*
087300050217     c     PrtDdtESP1_T1 BEGSR
087310110405      *
087320110405      * - pulizia record a nuova packing-list
087330110405     c                   if            %subst( SAVtrk : 1 : 1 ) = 'P'
087340110405     c                             or  %subst( SAVtrk : 1 : 1 ) = 'C'
087350110405     c                   exsr      sr_ClearESP1
087360110405     c                   endif
087370050214      *
087380050214      * > logo
087390050214     c                   eval      §Logo  = 'LOGO' + AR8tmd
087400050214      *
087410051110     c                   movel(p)  AR8uni        dAR8ESP1T1
087420050214      *
087430050214      * > fatturato a
087440050707     c                   eval      PCrsf  = §A8ESP1T1a
087450050707     c                   eval      PCinf  = §A8ESP1T1b
087460050707if  1c                   if        §A8ESP1T1c > *zeros
087470050707     c                   eval      PCclpf = §A8ESP1T1c
087480050216     c                                    + ' '
087490050707     c                                    + §A8ESP1T1d
087500050216     c                                    + ' '
087510050707     c                                    + §A8ESP1T1e
087520050216x   1c                   else
087530050216     c                   eval      PCclpf = '     '
087540050216     c                                    + ' '
087550050707     c                                    + §A8ESP1T1d
087560050216     c                                    + ' '
087570050707     c                                    + §A8ESP1T1e
087580050216e   1c                   endif
087590050214      * > destinatazione
087600050707     c                   eval      PCrsd  = §A8ESP1T1f
087610050707     c                   eval      PCind  = §A8ESP1T1g
087620050707if  1c                   if        §A8ESP1T1h > *zeros
087630050707     c                   eval      PCclpd = §A8ESP1T1h
087640050216     c                                    + ' '
087650050707     c                                    + §A8ESP1T1i
087660050216     c                                    + ' '
087670050707     c                                    + §A8ESP1T1j
087680050216x   1c                   else
087690050216     c                   eval      PCclpd = '     '
087700050216     c                                    + ' '
087710050707     c                                    + §A8ESP1T1i
087720050216     c                                    + ' '
087730050707     c                                    + §A8ESP1T1j
087740050216e   1c                   endif
087750050214      * > dati in testata DDT
087760050214      * - - codice cliente
087770050707     c                   eval      PCcli  = §A8ESP1T1k
087780050214      * - - codice fiscale
087790050707     c                   eval      PCcf   = §A8ESP1T1l
087800050214      * - - partita iva
087810050707     c                   eval      PCpi   = §A8ESP1T1m
087820050214      *
087830050214     c                   ENDSR
087840050214      *
087850050214      *---------------------------------------------------------------*
087860050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
087870050217      *?Dati di testata: documento, riferimenti.                     ?*
087880050214      *---------------------------------------------------------------*
087890050217     c     PrtDdtESP1_T2 BEGSR
087900110405      *
087910110405      * - pulizia records a nuova packing-list
087920110405     c                   if            %subst( SAVtrk : 1 : 1 ) = 'P'
087930110405     c                             or  %subst( SAVtrk : 1 : 1 ) = 'C'
087940110405     c                   exsr      sr_ClearESP1
087950110405     c                   endif
087960050214      *
087970051110     c                   movel(p)  AR8uni        dAR8ESP1T2
087980050214      *
087990050214      * > dati in testata DDT
088000050214      * - - tipo documento
088010050707     c                   eval      PCtdo  = §A8ESP1T2a
088020050214      * - - data e numero documento (DDT)
088030050707     c                   eval      PCdtd  = §A8ESP1T2b
088040050707     c                   eval      PCnrd  = §A8ESP1T2c
088050050214      * - - riferimenti mittente
088060050707     c                   eval      PCrif  = §A8ESP1T2d
088070050214      * - - condizioni di pagamento
088080050707     c                   eval      PCpag  = §A8ESP1T2e
088090050214      * - - annotazioni
088100050707     c                   eval      PCann  = §A8ESP1T2f
088110050214      * - - agente
088120050707     c                   eval      PCag2  = §A8ESP1T2g
088130050214      *
088140050214      * > Stampa testata DDT - - - - - - - - - - - - - - - - - - - - -*
088150050214      *
088160050214      *   - OverFlow impossibile qui! -
088170050215     c***                exsr      Test_OF
088180050215     c                   exsr      Posiz_ES
088190081017     c                   if            §AR8cxd = *blanks
088200081017     c                             or  DB0fa4  = 'P'
088210050331     c                   write     A4prtDT0
088220050331     c                   endif
088230050214     c                   write     A4prtES0
088240081002     c*** (NON qui:)     write     A4prtES0A
088250081002     c                   write     A4prtSP1
088260050215     c                   write     A4prtES1A
088270050215     c                   write     A4prtES1B
088280050214      *
088290050214     c                   ENDSR
088300050214      *
088310050214      *---------------------------------------------------------------*
088320050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
088330050217      *?Dati di dettaglio: articolo, descrizione, quantità, ...      ?*
088340050214      *---------------------------------------------------------------*
088350050217     c     PrtDdtESP1_C1 BEGSR
088360110405      *
088370110405      * - pulizia records a nuova packing-list
088380110405     c                   if        %subst( SAVtrk : 1 : 1 ) = 'P'
088390110405     c                   exsr      sr_ClearESP1
088400110405     c                   endif
088410050214      *
088420050214     c                   clear                   A4prtES2
088430051110     c                   movel(p)  AR8uni        dAR8ESP1C1
088440050214      *
088450050214      * > dati in dettaglio DDT
088460050214      * - - codice prodotto
088470050707     c                   eval      PCcpr  = §A8ESP1C1a
088480050214      * - - descrizione prodotto
088490050707     c                   eval      PCdpr  = §A8ESP1C1b
088500050214      * - - quantità
088510050707     c                   eval      PCqta  = '      ' + §A8ESP1C1c
088520050214      * - - prezzo unitario
088530050707     c                   eval      PCpru  = §A8ESP1C1d
088540050214      * - - cambio: divisa + valore
088550050707     c                   eval      PCdiv  = §A8ESP1C1e
088560050707     c                   eval      PCcmb  = §A8ESP1C1f
088570050214      * - - % sconto
088580050707     c                   eval      PCprc  = §A8ESP1C1g
088590050214      * - - importo
088600050707     c                   eval      PCimp  = §A8ESP1C1h
088610050214      * - - codice iva
088620050707     c                   eval      PCciv  = §A8ESP1C1i
088630050214      *
088640050214      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
088650050214      *
088660050214     c                   exsr      Test_OF
088670050214     c                   write     A4prtES2
088680050214      *
088690050214     c                   ENDSR
088700050214      *
088710050214      *---------------------------------------------------------------*
088720050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
088730050217      *?Dati di dettaglio: note (riga libera)                        ?*
088740050214      *---------------------------------------------------------------*
088750050217     c     PrtDdtESP1_C2 BEGSR
088760110405      *
088770110405      * - pulizia records a nuova packing-list
088780110405     c                   if        %subst( SAVtrk : 1 : 1 ) = 'P'
088790110405     c                   exsr      sr_ClearESP1
088800110405     c                   endif
088810050214      *
088820050215     c                   clear                   A4prtES3
088830051110     c                   movel(p)  AR8uni        dAR8ESP1C2
088840050214      *
088850050214      * > dati in note DDT
088860050214      * - - riga descrittiva
088870050707     c                   eval      PCrig  = §A8ESP1C2a
088880050214      *
088890050214      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
088900050214      *
088910050214     c                   exsr      Test_OF
088920050214     c                   write     A4prtES3
088930050214      *
088940050214     c                   ENDSR
088950050214      *
088960050214      *---------------------------------------------------------------*
088970050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
088980050217      *?Dati di fondo: totali documento                              ?*
088990050214      *---------------------------------------------------------------*
089000050217     c     PrtDdtESP1_P1 BEGSR
089010050214      *
089020081002      /free
089030081002           select;
089040081002             when  NOT *in45;
089050081002               wMaxLine = $MaxLine6;
089060081002             when  *in45   and   NOT *in46;
089070081002               wMaxLine = $MaxLine6_pl1;
089080081002             when  *in45   and   *in46;
089090081002               wMaxLine = $MaxLine6_pl2;
089100081002           endsl;
089110081002      /end-free
089120051110     c                   movel(p)  AR8uni        dAR8ESP1P1
089130050215      *
089140050215      * > dati in fondo DDT
089150050215      * - - netto merce
089160050707     c                   eval      PCinm  = §A8ESP1P1a
089170050215      * - - spese di trasporto
089180050707     c                   eval      PCist  = §A8ESP1P1b
089190050215      * - - spese imballo
089200050707     c                   eval      PCisi  = §A8ESP1P1c
089210050215      * - - spese bolli
089220050707     c                   eval      PCisb  = §A8ESP1P1d
089230050215      * - - spese amministrative
089240050707     c                   eval      PCisa  = §A8ESP1P1e
089250050215      * - - spese varie
089260050707     c                   eval      PCisv  = §A8ESP1P1f
089270050215      * - - codici iva
089280050707     c                   eval      PCic1  = §A8ESP1P1g
089290050707     c                   eval      PCic2  = §A8ESP1P1m
089300050707     c                   eval      PCic3  = §A8ESP1P1s
089310050215      * - - descrizioni iva
089320050707     c                   eval      PCid1  = §A8ESP1P1h
089330050707     c                   eval      PCid2  = §A8ESP1P1n
089340050707     c                   eval      PCid3  = §A8ESP1P1t
089350050215      * - - imponibili iva: divise
089360050707     c                   eval      PCie1  = §A8ESP1P1i
089370050707     c                   eval      PCie2  = §A8ESP1P1o
089380050707     c                   eval      PCie3  = §A8ESP1P1u
089390050215      * - - imponibili iva: importi
089400050707     c                   eval      PCii1  = §A8ESP1P1j
089410050707     c                   eval      PCii2  = §A8ESP1P1p
089420050707     c                   eval      PCii3  = §A8ESP1P1v
089430050215      * - - aliquote iva
089440050707     c                   eval      PCia1  = §A8ESP1P1k
089450050707     c                   eval      PCia2  = §A8ESP1P1q
089460050707     c                   eval      PCia3  = §A8ESP1P1w
089470050215      * - - imposte iva
089480050707     c                   eval      PCiv1  = §A8ESP1P1l
089490050707     c                   eval      PCiv2  = §A8ESP1P1r
089500050707     c                   eval      PCiv3  = §A8ESP1P1x
089510050215      * - - Totali documento: divise + importi
089520050707     c                   eval      PCtdd  = §A8ESP1P1y
089530050707     c                   eval      PCtdi  = §A8ESP1P1z
089540050707     c                   eval      PCtdd2 = §A8ESP1P11
089550050707     c                   eval      PCtdi2 = §A8ESP1P12
089560050214      *
089570050214     c                   ENDSR
089580050214      *
089590050214      *---------------------------------------------------------------*
089600050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
089610050217      *?Dati di fondo: informazioni vettore                          ?*
089620050214      *---------------------------------------------------------------*
089630050217     c     PrtDdtESP1_P2 BEGSR
089640050214      *
089650081002      /free
089660081002           select;
089670081002             when  NOT *in45;
089680081002               wMaxLine = $MaxLine6;
089690081002             when  *in45   and   NOT *in46;
089700081002               wMaxLine = $MaxLine6_pl1;
089710081002             when  *in45   and   *in46;
089720081002               wMaxLine = $MaxLine6_pl2;
089730081002           endsl;
089740081002      /end-free
089750051110     c                   movel(p)  AR8uni        dAR8ESP1P2
089760050215      *
089770050215      * > dati in fondo DDT
089780050215      * - - resa
089790050707     c                   eval      PCres  = §A8ESP1P2a
089800050215      * - - trasporto a cura del
089810050707     c                   eval      PCtra  = §A8ESP1P2b
089820050215      * - - vettore
089830110426     c*//                eval      PCvet  = $Bartolini
089840110426     c*//                                 + ' - '
089850110426     c*//                                 + $Intest2
089860110426     c*//                                 + ' (BO)'
089870050707     c                   eval      PCvet2 = §A8ESP1P2c
089880050215      * - - causale trasporto
089890050707     c                   eval      PCctr2 = §A8ESP1P2d
089900050215      * - - aspetto esteriore dei beni
089910050707     c                   eval      PCaeb  = §A8ESP1P2e
089920050215      * - - numero colli
089930050707     c                   eval      PCncl2 = §A8ESP1P2f
089940050215      * - - peso complessivo
089950050707     c                   eval      PCpes  = §A8ESP1P2g
089960050215      * - - volume
089970050707     c                   eval      PCvol  = §A8ESP1P2h
089980050215      * - - inizio trasporto: data + ora
089990050707     c                   eval      PCdit  = §A8ESP1P2i
090000050707     c                   eval      PCoit  = §A8ESP1P2j
090010050215     *** *
090020050215     *** * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
090030050215     *** * ?(impostata a fine ciclo di lettura - sr. DDT_Esprinet)?
090040050217      *
090050050217      * N.B.: la copia per il destinatario della LDV deve comparire
090060050217      *       nell'ultima pagina del DDT per il cliente (vedi subr.
090070050217      *       "ChiusuraLDV")
090080050214      *
090090050214     c                   ENDSR
090100081002      /free
090110081002       //--------------------------------------------------------------
090120110405       //?Impostazione nuova pagina di DDT Esprinet                    ?
090130081002       //--------------------------------------------------------------
090140081002       BEGSR  NewPageES;
090150081002
090160110405         // -?Impostazione del nuovo limite x overflow?
090170081002         select;
090180081002           when  NOT *in45;
090190081002             wMaxLine = $MaxLine2;
090200081002           when  *in45   and   NOT *in46;
090210090324             wMaxLine = $MaxLine5_pl1;
090220081002           when  *in45   and   *in46;
090230090324             wMaxLine = $MaxLine5_pl2;
090240081002         endsl;
090250081002
090260081002         write  A4prtSP1;
090270081002         write  A4prtES1B;
090280081002
090290081002       ENDSR;
090300110405
090310110405       //--------------------------------------------------------------
090320110405       //?Pulizia campi a nuova packing-list relativa alla stessa LdV  ?
090330110405       //--------------------------------------------------------------
090340110405       BEGSR  sr_ClearESP1;
090350110405
090360110405         // -?Stampa fondo DDT?
090370110405
090380110405         // - -?Calcolo la posizione finale del box e di linee?
090390110405         //    ?separatrici?
090400110405         exsr  Posiz_ES;
090410110405
090420110405         // - -?Stampa parte finale DDT?
090430110405         write  A4prtES4;
090440110405
090450110405         // - -?Stampa box, linee e testi fissi (no logo)?
090460110405         if  §AR8cxd = *blank  or  DB0fa4 = 'P';
090470110405           write  A4prtDT0;
090480110405         endif;
090490110405         write  A4prtES0A;
090500110405         write  A4prtES0B;
090510110405
090520110405         // -?Pulizia record?
090530110405         select;
090540110405           when  %subst( AR8trk : 1 : 1 ) = 'T';
090550110405             clear  A4prtES0;
090560110405             clear  A4prtES0A;
090570110405             clear  A4prtES0B;
090580110405             clear  A4prtES1A;
090590110405             clear  A4prtES1B;
090600110405             clear  A4prtES2;
090610110405             clear  A4prtES3;
090620110405             clear  A4prtES4;
090630110405           when  %subst( AR8trk : 1 : 1 ) = 'C';
090640110405             clear  A4prtES2;
090650110405             clear  A4prtES3;
090660110405             clear  A4prtES4;
090670110405         endsl;
090680110405
090690110405         // -?Impostazione testi fissi?
090700131017         //PCLtx3 = 'CONTRIBUTO AMBIENTALE CONAI ASSOLTO OVE DOVUTO';
090710131017         PCLtx3 = 'CONTRIBUTO AMBIENTALE CONAI ASSOLTO';
090720110405
090730110405         PCLpx2 = 'IL PRESENTE DOCUMENTO E'' REGOLATO DALLE +
090740110405                   CONDIZIONI GENERALI DI VENDITA PUBBLICATE SUL SITO +
090750110405                   INTERNET ALL''INDIRIZZO www.esprinet.com CHE SI +
090760110405                   INTENDONO ESPRESSAMENTE CONOSCIUTE ED ACCETTATE';
090770110405
090780110405         PCLpx3a = 'NB. IN CASO DI RITARDATO PAGAMENTO, VERRANNO +
090790110405                    ADDEBITATI';
090800110405         PCLpx3b = 'INTERESSI AL TASSO DEL PRIME RATE MAGGIORATO +
090810110405                    DEL 2% A';
090820110405         PCLpx3c =
090830110405                   'DECORRERE DAL GIORNO SUCCESSIVO ALLA SCADENZA +
090840110405                    DELLA FATTURA';
090850110405
090860110405         PCLpx4a = 'Si prega di verificare la Vs. ragione sociale +
090870110405                    indirizzo e';
090880110405         PCLpx4b = 'partita IVA, dandoci eventuale sollecita +
090890110405                    rettifica scritta.';
090900110405
090910110405         PCLpx5a = '* I PRODOTTI IMPORTATI DAGLI U.S.A. SONO CON +
090920110405                    SPECIALE';
090930110405         PCLpx5b = 'LICENZA CHE PRECLUDE RIESPORTAZIONI NON +
090940110405                    AUTORIZZATE.';
090950110405
090960110405         PCLpx6  = 'LA SOCIETÀ SI IMPEGNA A VERSARE L''IVA NEI MODI +
090970110405                    E TERMINI PREVISTI (SOLIDARIETÀ ART. 60 BIS DPR +
090980110405                    633/72-FINANZIARIA 2005)';
090990110405
091000110405         // -?Salto Pagina?
091010110405         exsr  Test_OF;
091020110405
091030110405         // -?Re-Impostazione limiti di overflow?
091040110405         select;
091050110405           when  NOT *in45;
091060110405             wMaxLine = $MaxLine2;
091070110405           when  *in45   and   NOT *in46;
091080110405             wMaxLine = $MaxLine5_pl1;
091090110405           when  *in45   and   *in46;
091100110405             wMaxLine = $MaxLine5_pl2;
091110110405         endsl;
091120110405
091130110405         // -?Posizionamento iniziale all'interno del foglio A4?
091140110405         write  A4prtSP1;
091150110405
091160110405       ENDSR;
091170081002      /end-free
091180050214      *
091190050214      *---------------------------------------------------------------*
091200050217      *?Posizionamento di linee e box nel DDT ESPRINET               ?*
091210050214      *---------------------------------------------------------------*
091220050215     c     Posiz_ES      BEGSR
091230080708      *
091240081002      /free
091250081002         // Impostazione posizioni di logo/box
091260081002         // (posizioni di "partenza" diverse a seconda del posizionam.
091270081002         //  iniziale nel foglio A4 attualmente in stampa)
091280081002         select;
091290081002           when  NOT *in45;
091300081006             §PBA =  3.130;
091310081006             //§PBA =  3.255;        (più in basso di una riga)
091320081002           when  *in45   and   NOT *in46;
091330081002             §PBA = 0.255;
091340081002           when  *in45   and   *in46;
091350081002             §PBA = 6.005;
091360081002         endsl;
091370081002      /end-free
091380080708      * Impostazione posizioni di linee orizzontali
091390080708     c                   eval      §LOA  =  §PBA + 0.76
091400080708     c                   eval      §LOB  =  §LOA + 0.25
091410080708     c                   eval      §LOC  =  §LOB + 0.12
091420080708      * Impostazione posizioni di linee verticali
091430080708     c                   eval      §LVA  =  §LOA - 0.15
091440050214      *
091450050214      * Impostazione posizioni di box
091460050215if  1c                   if        Page  <= 1
091470050214      *   (posizione di "partenza" su 1ª pagina)
091480081002     c                   eval      §PB1  =  §PBA + 1.270
091490050214x   1c                   else
091500050214      *   (posizione di "partenza" su pagine successive)
091510081002     c                   eval      §PB1  =  §PBA + 0.020
091520050214e   1c                   endif
091530110405if  1c*//                if        %subst(AR8trk:1:1) = 'P'
091540110405if  1c                   if        %subst(SAVtrk:1:1) = 'P'
091550050331     c                             and    CurLineA4   < wMaxLine
091560081002     c                   eval      §PB2  =  §PBA + 5.360
091570050215x   1c                   else
091580050215     c                   eval(h)   §PB2  =  CurLineA4 / 8 + 0.05
091590081002     c                   if        §PB2  <  (§PBA + 5.320)
091600081002     c                   eval      §PB2  =  (§PBA + 5.320)
091610050331     c                   endif
091620050215e   1c                   endif
091630050215      * Impostazione posizioni di linee (orizzontali e verticali)
091640050214      *   nella parte superiore del box (testata)
091650050215     c                   eval      §LO1  =  §PB1 + 0.25
091660050214     c                   eval      §LO2  =  §LO1 + 0.11
091670050214      *   nella parte inferiore del box (piede)
091680110405if  1c*//                if        %subst(AR8trk:1:1) = 'P'
091690110405if  1c                   if        %subst(SAVtrk:1:1) = 'P'
091700050331     c                             and    CurLineA4   < wMaxLine
0917100810027.78 c                   eval      §LO3  =  §PBA + 3.900
0917200502158.00 c                   eval      §LO4  =  §LO3 + 0.24
0917300502158.39 c                   eval      §LO5  =  §LO4 + 0.36
0917400502158.52 c                   eval      §LO6  =  §LO5 + 0.14
0917500502148.64 c                   eval      §LO7  =  §LO6 + 0.12
0917600502148.77 c                   eval      §LO8  =  §LO7 + 0.13
0917700502148.89 c                   eval      §LO9  =  §LO8 + 0.12
0917800807088.97 c                   eval      §LO0  =  §LO9 + 0.10
091790050215e   1c                   endif
091800050214      * La lunghezza delle linee verticali separatrici nel dettaglio
091810050214      *   dipende dalla fase di stampa: se si sta stampando ancora il
091820050214      *   dettaglio deve essere più lunga, visto che non viene stampa-
091830050214      *   ta la parte finale del DDT...
091840110405if  1c*//                if        %subst(AR8trk:1:1) = 'P'
091850110405if  1c                   if        %subst(SAVtrk:1:1) = 'P'
091860050331     c                             and    CurLineA4   < wMaxLine
0918700502162.25 c                   eval      §LV1  =  §LO3 - §LO2
091880050215x   1c                   else
091890050216     c                   eval      §LV1  =  §PB2 - §LO2
091900050215e   1c                   endif
091910050214      *
091920050214      * Impostazione posizioni di campi di testo
091930050214      * - PCLtx1/tx2 (verticali)
0919400502179.10 c                   eval      §PR1  =  §PB2 - 0.07
091950050214      * - PCLpx2 ("Il presente documento...")
0919600502178.95 c                   eval      §PS2  =  §LO9 + 0.07
091970050214      * - PCLpx3a/px4a/px5a
0919800807089.04 c                   eval      §PS3  =  §LO0 + 0.07
091990050214      * - PCLpx3b/px4b/px5b
0920000502179.09 c                   eval      §PS4  =  §PS3 + 0.07
092010050214      * - PCLpx3c/px4c/px5c
0920200502179.14 c                   eval      §PS5  =  §PS4 + 0.07
092030050214      * - PCLpx6 ("Contributo ambientale CONAI...")
0920400502179.25 c                   eval      §PS6  =  §PB2 + 0.08
092050050214      *
092060050214     c                   ENDSR
092070041207      *
092080041207      *---------------------------------------------------------------*
092090050217      *?Stampa DDT del mittente (generico con logo)                  ?*
092100041207      *---------------------------------------------------------------*
092110041207     c     DDT_GenericoL BEGSR
092120041207      *
092130081003      /free
092140081003         // Impostazione del nuovo limite x overflow
092150081003         select;
092160081003           when  NOT *in45;
092170081003             wMaxLine = $MaxLine2;
092180081003           when  *in45   and   NOT *in46;
092190081003             wMaxLine = $MaxLine2_pl1;
092200081003           when  *in45   and   *in46;
092210081003             wMaxLine = $MaxLine2_pl2;
092220081003         endsl;
092230081003
092240081003         // Posizionamento iniziale all'interno del foglio A4
092250081003         write  A4prtSP1;
092260081006
092270081006         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
092280081006           write  A4prtNUL;
092290081006         endif;
092300081003      /end-free
092310041207      *
092320041207      * > logo
092330041207     c                   eval      §Logo  = 'LOGO' + AR8tmd
092340041207      *
092350081017     c                   if            §AR8cxd = *blanks
092360081017     c                             or  DB0fa4  = 'P'
092370080709     c                   write     A4prtDT0
092380080709     c                   endif
092390080709      *
092400041209      * Imposto posizione per logo
092410081003      /free
092420081003         select;
092430081003           when  NOT *in45;
092440081003             exsr  Rep_§LO2;
092450081003             §PB1 = §LO2 + (2 / 8);
092460081003           when  *in45   and   NOT *in46;
092470081003             §PB1 = 0.250;
092480081003           when  *in45   and   *in46;
092490081003             §PB1 = 6.000;
092500081003         endsl;
092510081003      /end-free
092520041209      *   Stampa del logo
092530041209     c                   write     A4prtDB0
092540041207      *
092550041207      * Reperimento dati e stampa DDT mittente - - - - - - - - - - - -*
092560041207      *
092570130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
092580130315      *  "StampaDDT")
092590041207     c***  KLDV          setll     FIAR8000       (già fatto)
092600041207     c***  KLDV          reade     FIAR8000       (già fatto)
092610041207      *
092620041207if  1c                   dow       NOT %eof(FIAR801L)
092630041207      *
092640041207if  2c                   if        AR8atb = *blanks
092650041207      *
092660041207      * > Stampa dei dati
092670041207     c                   exsr      Test_OF
092680041207sel 3c                   select
092690041207      * - Dati di dettaglio: testo in font a dimensione standard
092700041207w   3c                   when      AR8trk = 'C1'
092710051110     c                   movel(p)  AR8uni        PCddt
092720041207     c                   write     A4prtDC1
092730041207      * - Dati di testata:   testo in font a dimensioni massime
092740041207w   3c                   when      AR8trk = 'T1'
092750051110     c                   movel(p)  AR8uni        PCddt
092760041207     c                   write     A4prtDT1
092770041207      * - Dati di testata:   testo in font a dimensioni grandi
092780041207w   3c                   when      AR8trk = 'T2'
092790051110     c                   movel(p)  AR8uni        PCddt
092800041207     c                   write     A4prtDT2
092810041209      * - Linea separatrice
092820041209w   3c                   when      AR8trk = '--'
092830041209      *   - Stampa di una riga vuota per la riga separatrice
092840041209     c                   write     A4prtNUL
092850041209      *   - Stampa della linea orizzontale separatrice
092860090213     c                   eval(h)   §LO1   = (CurLineA4 / 8) - 0.050
092870041209     c                   write     A4prtLO
092880041207e   3c                   endsl
092890041207e   2c                   endif
092900041207      *
092910041207     c                   eval      SAVtrk = AR8trk
092920041207     c     KLDV          reade     FIAR8000
092930041207      *
092940041207e   1c                   enddo
092950080708      *
092960081003      /free
092970081003         select;
092980081003           when  NOT *in45;
092990081003             wMaxLine = $MaxLine2;
093000081003           when  *in45   and   NOT *in46;
093010081003             wMaxLine = $MaxLine2_pl1;
093020081003           when  *in45   and   *in46;
093030081003             wMaxLine = $MaxLine2_pl2;
093040081003         endsl;
093050081003      /end-free
093060041207      *
093070041207     c                   ENDSR
093080040607      *
093090040607      *---------------------------------------------------------------*
093100050217      *?Stampa DDT del mittente (generico senza logo)                ?*
093110040607      *---------------------------------------------------------------*
093120040607     c     DDT_Generico  BEGSR
093130040607      *
093140081003      /free
093150081003         // Impostazione del nuovo limite x overflow
093160081003         select;
093170081003           when  NOT *in45;
093180081003             wMaxLine = $MaxLine2;
093190081003           when  *in45   and   NOT *in46;
093200081003             wMaxLine = $MaxLine2_pl1;
093210081003           when  *in45   and   *in46;
093220081003             wMaxLine = $MaxLine2_pl2;
093230081003         endsl;
093240081003
093250081003         // Posizionamento iniziale all'interno del foglio A4
093260081003         write  A4prtSP1;
093270081006
093280081006         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
093290081006           write  A4prtNUL;
093300081006         endif;
093310081003      /end-free
093320040607      *
093330081017     c                   if            §AR8cxd = *blanks
093340081017     c                             or  DB0fa4  = 'P'
093350050331     c                   write     A4prtDT0
093360050331     c                   endif
093370040607      *
093380040607      * Reperimento dati e stampa DDT mittente - - - - - - - - - - - -*
093390040607      *
093400130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
093410130315      *  "StampaDDT")
093420040607     c***  KLDV          setll     FIAR8000       (già fatto)
093430040607     c***  KLDV          reade     FIAR8000       (già fatto)
093440040607      *
093450040607if  1c                   dow       NOT %eof(FIAR801L)
093460040607      *
093470040607if  2c                   if        AR8atb = *blanks
093480051110     c                   movel(p)  AR8uni        PCddt
093490040607     c                   exsr      Test_OF
093500040607sel 3c                   select
093510040607      * - Dati di dettaglio: testo in font a dimensione standard
093520040607w   3c                   when      AR8trk = 'C1'
093530040607     c                   write     A4prtDC1
093540040607      * - Dati di testata:   testo in font a dimensioni massime
093550040607w   3c                   when      AR8trk = 'T1'
093560040607     c                   write     A4prtDT1
093570040607      * - Dati di testata:   testo in font a dimensioni grandi
093580040607w   3c                   when      AR8trk = 'T2'
093590040607     c                   write     A4prtDT2
093600040607e   3c                   endsl
093610040607e   2c                   endif
093620040607      *
093630040607     c     KLDV          reade     FIAR8000
093640040607      *
093650040607e   1c                   enddo
093660080708      *
093670081003      /free
093680081003         select;
093690081003           when  NOT *in45;
093700081003             wMaxLine = $MaxLine2;
093710081003           when  *in45   and   NOT *in46;
093720081003             wMaxLine = $MaxLine2_pl1;
093730081003           when  *in45   and   *in46;
093740081003             wMaxLine = $MaxLine2_pl2;
093750081003         endsl;
093760081003      /end-free
093770040607      *
093780040607     c                   ENDSR
093790081017
093800081017      /free
093810081017       //--------------------------------------------------------------
093820140307       //?Impostazione nuova pagina di DDT Generico (con/senza logo)   ?
093830081017       //--------------------------------------------------------------
093840081017       BEGSR  NewPageSTD;
093850081017
093860081017         // Posizionamento iniziale nella nuova pagina
093870081017         //   (già fatto con A4prtSP0 se DB0FA4 = "P")
093880081017         if  DB0fa4 <> 'P';
093890081017           write  A4prtSP1;
093900081017           if  DB0fa4 = '1'   or   DB0fa4 = 'S';
093910081017             write  A4prtNUL;
093920081017           endif;
093930081017         endif;
093940081017
093950081017         // Stampa "COPIA PER IL DESTINATARIO"
093960081017         if  §AR8cxd = *blank   or   DB0fa4 = 'P';
093970081017           write  A4prtDT0;
093980081017         endif;
093990081017
094000081017         // Impostazione del nuovo limite x overflow
094010081017         select;
094020081017           when  NOT *in45;
094030081017             wMaxLine = $MaxLine2;
094040081017           when  *in45   and   NOT *in46;
094050081017             wMaxLine = $MaxLine2_pl1;
094060081017           when  *in45   and   *in46;
094070081017             wMaxLine = $MaxLine2_pl2;
094080081017         endsl;
094090081017
094100081017       ENDSR;
094110081017      /end-free
094120030515      *
094130030515      *---------------------------------------------------------------*
094140050217      *?Stampa del record "generico" (PRTGEN)                        ?*
094150030515      *---------------------------------------------------------------*
094160030515     c     StampaGEN     BEGSR
094170030515      *
094180030515      * Controllo se è stato raggiunto l'overflow nel formato in uso
094190030515     c                   exsr      Test_OF
094200030515      *
094210030911      * Stampo la riga PRTGEN
094220030911      * Se sto stampando una "testata" (*in39) salto una riga prima
094230030911      *   ed una riga dopo (per spaziare)
094240030911if  1c                   if        *in39 = *on
094250030515     c                   write     A4prtNUL
094260030911e   1c                   endif
094270030911     c                   write     A4prtGEN
094280030911if  1c                   if        *in39 = *on
094290030911     c                   write     A4prtNUL
094300030911e   1c                   endif
094310030515      *
094320030515     c                   ENDSR
094330030515      *
094340030515      *---------------------------------------------------------------*
094350050511      *?Prepara elenco per lista - SegnaColli Non Sequenziali (test.)?*
094360030515      *---------------------------------------------------------------*
094370030515     c     ListaScNS     BEGSR
094380030515      *
094390030515     c                   clear                   XX
094400030515      *
094410030515      * Nel foglio in formato "A5" devo solo riempire gli appositi
094420030715      *   campi "Note e Particolarità" (nessun salto riga/pagina è
094430030715      *   richiesto)
094440050513     c                   if           *in69       = *on
094450050513     c                             or $A4_x_ScNS  = *off
094460030515     c                   eval      WelSNS = $elSNSxA5
094470030515     c                   goto      ScNS_A5
094480030515     c                   endif
094490030515      *
094500030515      * Determino il numero di segnacolli da elencare per riga nel
094510030515      *   formato pagina "A4" (dove ci sono righe a parte).
094520030515      *   Dipende dal carattere specificato nel prtf: più grande è
094530030515      *   il carattere => meno segnacolli ci stanno sulla riga !
094540030911      *   A cpi 15 -come ora- ci sta tutta la schiera; a cpi 12 no !
094550030515     c                   eval      WelSNS = %elem($SC1)
094560030515      *
094570030515      * Spengo gli indicatori che condizionano in stampa le indicazioni
094580030515      *   per DPD / FedEx / Bancali-Supermercati-Autotrasportatori
094590030515      *   (se sono già state stampate nella 1ª pagina non devono più
094600030515      *   esserlo)
094610030910      *   (se non sono ancora state stampate, dovranno esserlo poi)
094620030515if  1c                   if        wPage > 1
094630050513     c                   movea     *in(70)       SAVin7x
094640040223     c                   movea     '0000'        *in(70)
094650030515e   1c                   endif
094660030515      *
094670030515      * Calcolo il numero di righe da saltare
094680030515     c                   eval      wMinLine = CurLineA4
094690030515if  1c                   if        §LO2     > §LO1
094700030515      * - se stampato almeno un box
094710030515     c                   eval(h)   wMinLine = (§LO2 * 8) + 2
094720030515x   1c                   else
094730030515      * - se non stampato alcun box
094740080730      /free
094750080729       // - Calcola la posizione della riga in 1/8" di pollice
094760080729       //   da quella attuale (in 1/9" di pollice)
094770080730         eval(h) wMinLine = (CurLineA4 * 8 / 9) + 1;
094780080730      /end-free
094790030515e   1c                   endif
094800030515      * ...e scarto il numero righe necessario
094810030515      *   (se non sono già da pagina nuova;
094820030515      *    se sono da pagina nuova: ci penserà poi la subr. "Test_OF"
094830030515      *       alla prima riga in stampa)
094840030515if  1c                   if        wMinLine  < wMaxLine
094850080729do  2c                   dou       CurLineA4 >= wMinLine
094860030515     c                   write     A4prtNUL
094870030515e   2c                   enddo
094880030515e   1c                   endif
094890030515      *
094900030515     c     ScNS_A5       tag
094910030515      *
094920030515      * Imposto l'indicatore per la stampa del titolo relativo all'
094930030515      *   elenco che segue (condiziona attributi di stampa diversi)
094940030515     c                   eval      *in39  = *on
094950050511      *
094960050511     c                   exsr      ListaScNSd
094970050511      *
094980050511     c                   ENDSR
094990050511      *
095000050511      *---------------------------------------------------------------*
095010050511      *?Prepara elenco per lista - SegnaColli Non Sequenziali (dett.)?*
095020050511      *---------------------------------------------------------------*
095030050511     c     ListaScNSd    BEGSR
095040030515      *
095050030515      * Stampo elenco dei segnacolli non sequenziali:
095060071008if  1c                   IF             DB0tbo  = 'P'
095070071008     c                             or   DB0tbo  = '8'
095080030515      * - per bolle in Partenza
095090030515     c                   exsr      ScNS_BLT
095100030515x   1c                   ELSE
095110081007      * - per bolle in Arrivo
095120030515     c                   exsr      ScNS_ART
095130030515e   1c                   ENDIF
095140030515      *
095150030515      * Alla fine: stampo eventuali segnacolli in elenco (non stampati)
095160030515      *              in foglio di formato "A4"
095170030515      *            imposto eventuali segnacolli in elenco (non messi in
095180030515      *              stampa tra le particolarità) in foglio "A5"
095190030515if  1c                   if        XX     > *zeros
095200030515     c                   eval      XX     = *hival
095210030515     c                   exsr      StampaScNS
095220030515e   1c                   endif
095230030910      *
095240030910      * Se sto stampando su formato pagina "A4": reimposto *in70, *in71
095250030910      *   e *in72 (spenti prima della stampa dell'elenco dei segnacolli
095260030910      *   non sequenziali) perchè saranno utilizzati per il posiziona-
095270030910      *   mento in nuova pagina (nella subr. "Test_OF") e per la stampa
095280040223      *   dei box per DPD, FedEx e quelli da FIAR5.
095290050513if  1c                   if             not *in69
095300030910     c                             and  not *in70
095310080922     c*** DPD non più in A4:       and  not *in71
095320030910     c                             and  not *in72
095330080912     c*** EEX non più gestita:     and  not *in73
095340030910     c                   movea     SAVin7x       *in(70)
095350030910e   1c                   endif
095360030515      *
095370030515     c                   ENDSR
095380030515      *
095390030515      *---------------------------------------------------------------*
095400050217      *?Prepara elenco SegnaColli Non Sequenziali da FNBLT (partenza)?*
095410030515      *---------------------------------------------------------------*
095420030515     c     ScNS_BLT      BEGSR
095430030515      *
095440030515if  1c                   if        not %open(FNBLT01L)
095450030515     c                   open      FNBLT01L
095460030515e   1c                   endif
095470030515      *
095480030515      * memorizzo l'ultimo numero di segnacollo
095490070514if  1c                   if        $Mamma = *off
095500040607     c     KLDV          setgt     FNBLT000
095510040607     c     KLDV          readpe    FNBLT000
095520070514x   1c                   else
095530070514     c     KLBL          setgt     FNBLT000
095540070514     c     KLBL          readpe    FNBLT000
095550070514e   1c                   endif
095560030515if  1c                   if        NOT %eof(FNBLT01L)
095570080922     c                   eval      %subst(Pnsx : 8) = ','
095580080922     c                                              + %editc(BLTnsc : 'Z')
095590030515e   1c                   endif
095600030515      *
095610030515      * memorizzo il primo numero di segnacollo
095620070514if  1c                   if        $Mamma = *off
095630040607     c     KLDV          setll     FNBLT000
095640040607     c     KLDV          reade     FNBLT000
095650070514x   1c                   else
095660070514     c     KLBL          setll     FNBLT000
095670070514     c     KLBL          reade     FNBLT000
095680070514e   1c                   endif
095690030515if  1c                   if        NOT %eof(FNBLT01L)
095700080922     c                   eval      %subst(Pnsx : 1 : 7) =
095710080922     c                                 %editc(BLTnsc : 'Z')
095720030515e   1c                   endif
095730030515      *
095740030515      * se ci sono solo 2 segnacolli non sequenziali:
095750030515      *   stanno già al posto del range - basta non stampare il msg
095760030515      *   ("NON SEQUENZIALI")
095770030515if  1c                   if        DB0ncl <= 2
095780050516     c                   eval      *in39  =  *off
095790030515     c                   goto      EndScNS_BLT
095800030515e   1c                   endif
095810030515      *
095820030515      * ciclo di lettura per il recupero di tutti i numeri di
095830030515      *   segnacollo
095840030515do  1c                   DOW       NOT %eof(FNBLT01L)
095850030515     c                   eval      Wnsc   =  BLTnsc
095860030515     c                   exsr      StampaScNS
095870070514if  2c                   if        $Mamma = *off
095880040607     c     KLDV          reade     FNBLT000
095890070514x   2c                   else
095900070514     c     KLBL          reade     FNBLT000
095910070514e   2c                   endif
095920030515e   1c                   ENDDO
095930030515      *
095940030515     c     EndScNS_BLT   ENDSR
095950030515      *
095960030515      *---------------------------------------------------------------*
095970050217      *?Prepara elenco SegnaColli Non Sequenziali da FNART (arrivo)  ?*
095980030515      *---------------------------------------------------------------*
095990030515     c     ScNS_ART      BEGSR
096000030515      *
096010070319if  1c                   if        not %open(FNART01L)
096020070319     c                   open      FNART01L
096030070319e   1c                   endif
096040030515      *
096050030515      * memorizzo l'ultimo numero di segnacollo
096060070514if  1c                   if        $Mamma = *off
096070040607     c     KLDV          setgt     FNART000
096080040607     c     KLDV          readpe    FNART000
096090070514x   1c                   else
096100070514     c     KLBL          setgt     FNART000
096110070514     c     KLBL          readpe    FNART000
096120070514e   1c                   endif
096130030515if  1c                   if        NOT %eof(FNART01L)
096140080922     c                   eval      %subst(Pnsx : 8) = ','
096150080922     c                                              + %editc(ARTnsc : 'Z')
096160030515e   1c                   endif
096170030515      *
096180030515      * memorizzo il primo numero di segnacollo
096190070514if  1c                   if        $Mamma = *off
096200040607     c     KLDV          setll     FNART000
096210040607     c     KLDV          reade     FNART000
096220070514x   1c                   else
096230070514     c     KLBL          setll     FNART000
096240070514     c     KLBL          reade     FNART000
096250070514e   1c                   endif
096260030515if  1c                   if        NOT %eof(FNART01L)
096270080922     c                   eval      %subst(Pnsx : 1 : 7) =
096280080922     c                                 %editc(ARTnsc : 'Z')
096290030515e   1c                   endif
096300030515      *
096310030515      * se ci sono solo 2 segnacolli non sequenziali:
096320030515      *   stanno già al posto del range - basta non stampare il msg
096330030515      *   ("NON SEQUENZIALI")
096340030515if  1c                   if        DB0ncl <= 2
096350050516     c                   eval      *in39  =  *off
096360030515     c                   goto      EndScNS_ART
096370030515e   1c                   endif
096380030515      *
096390030515      * ciclo di lettura per il recupero di tutti i numeri di
096400030515      *   segnacollo
096410030515do  1c                   DOW       NOT %eof(FNART01L)
096420030515     c                   eval      Wnsc   =  ARTnsc
096430030515     c                   exsr      StampaScNS
096440070514if  2c                   if        $Mamma = *off
096450040607     c     KLDV          reade     FNART000
096460070514x   2c                   else
096470070514     c     KLBL          reade     FNART000
096480070514e   2c                   endif
096490030515e   1c                   ENDDO
096500030515      *
096510030515     c     EndScNS_ART   ENDSR
096520030515      *
096530030515      *---------------------------------------------------------------*
096540050217      *?Stampa elenco SegnaColli Non Sequenziali in fogli "A4" o "A5"?*
096550030515      *---------------------------------------------------------------*
096560030515     c     StampaScNS    BEGSR
096570030515      *
096580030515      * In prima riga stampo la "intestazione"
096590030515if  1c                   if        *in39
096600050513if  2c                   if        *in69   or
096610050513     c                             $A4_x_ScNS = *off
096620050513      * - foglio "A5" oppure
096630050513      *   foglio "A4" in "NOTE E PARTICOLARITA'"
096640030515     c                   eval      YY      = YY  + 1
096650030515     c                   eval      $NP(yy) = C_ScNS + ':'
096660030515x   2c                   else
096670030515      * - foglio "A4"
096680030515     c                   eval      Prig  = C_ScNS
096690030515     c                                   + '         (dal '
096700080922     c                                   + %trim( %subst(Pnsx: 1: 7) )
096710030515     c                                   + ' al '
096720080922     c                                   + %trim( %subst(Pnsx: 9: 7) )
096730030515     c                                   + ')'
096740030515     c                   exsr      StampaGEN
096750030515e   2c                   endif
096760080922     c                   eval      Pnsx  =  'NON SEQUENZIALI'
096770030515     c                   eval      *in39 = *off
096780030515     c                   clear                   XX
096790030515     c                   clear                   $SC1
096800030515     c                   clear                   $SC2
096810030515e   1c                   endif
096820030515      *
096830030515      * Al riempimento della riga: stampo la parte di elenco segnacolli
096840030515if  1c                   if        XX  >= WelSNS
096850030515     c                   eval      $SC2 = %editc($SC1 : 'Z')
096860050513if  2c                   if        *in69   or
096870050513     c                             $A4_x_ScNS = *off
096880050513      * - foglio "A5"  oppure
096890050513      *   foglio "A4" in "NOTE E PARTICOLARITA'"
096900030515     c                   eval      YY      = YY  + 1
096910030515     c                   movea(p)  $SC2          $NP(yy)
096920030515x   2c                   else
096930030515      * - foglio "A4"
096940030911     c                   movea(p)  $SC2          Prig
096950030515     c                   exsr      StampaGEN
096960030515e   2c                   endif
096970030515if  2c                   if        XX  <  *hival
096980030515     c                   clear                   XX
096990030515e   2c                   endif
097000030515     c                   clear                   $SC1
097010030515     c                   clear                   $SC2
097020030515e   1c                   endif
097030030515      *
097040030515      * Aggiorno la nuova parte di elenco segnacolli
097050030515      *  (se non richiesta la stampa degli ultimi segnacolli non
097060030515      *   sequenziali residui - memorizzati ma non ancora stampati)
097070030515if  1c                   if        XX  <  *hival
097080030515     c                   eval      XX = xx  + 1
097090030515     c                   eval      $SC1(xx) = Wnsc
097100030515e   1c                   endif
097110030515      *
097120030515     c                   ENDSR
097130040607      *
097140040607      *---------------------------------------------------------------*
097150050217      *?Stampa della parte finale della Lettera di Vettura           ?*
097160040607      *---------------------------------------------------------------*
097170040607     c     ChiusuraLDV   BEGSR
097180080605      *
097190080605      * Posizionamento sul foglio A4 in base al n° pagina in stampa
097200130418     c                   eval      *in33 = *on
097210130418     c                   exsr      Posiz_Box
097220070307      *
097230070307      * Il tagliandino "Copia per il Destinatario" va ora stampato SE:
097240070307      * · spedizione NON in stato "lasciato avviso"   oppure
097250070307      * · spedizione in stato "lasciato avviso", ma P.A. con fattura
097260070307      *   immediata
097270040607      *
097280080703      * 2ª parte - copia per il destinatario (da lasciare al cliente)
097290040607      *   (Se NON è ristampa in arrivo per recapito c/assegni - vedi
097300040607      *   costante "Non utilizzare per la consegna")
097310130418      *                o per il mittente - se Porto Franco Prepagato
097320040607      * > "cornice" / barcode / righine varie in parte inferiore
097330040607      *
097340080604sel 1c                   select
097350080604w   1c                   when           $No_x_Cons         =  *off
097360070307     c                             and ($LascAvv           =  *off
097370070406     c                              or (%subst(DB0tb1:1:1) =  'A'
097380070406     c                              and DB0nft             >  *zeros))
097390130418     c                   write     A4prtLDV
097400130418     c                   write     A4prtBOX
097410110426     c*//                write     A4LogoBAR
097420110426      /free
097430120530         //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
097440120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
097450120530         //  // >?Simil-Logo Bartolini/Brt?
097460120530         //  write  A4LogoBR2;
097470120530         //  write  A4LogoDA2;
097480120530         //else;
097490120530         //  // >?Simil-Logo Bartolini?
097500120530         //  write  A4LogoBAR;
097510120530         //  write  A4LogoDA1;
097520120530         //endif;
097530120530         // >?Simil-Logo BRT?
097540120530         write  A4LogoBRT;
097550120530         write  A4LogoDA3;
097560110426      /end-free
097570080604w   1c                   when           $LascAvv           =  *on
097580070406     c                             and (%subst(DB0tb1:1:1) <> 'A'
097590070406     c                              or  DB0nft             =  *zeros)
097600070307     c                   eval      Pmsg3  =  Tla
097610070307     c                   write     A4prtLDVB
097620120113w   1c                   when      $No_x_Cons =  *on
097630120113w   1c*//                   when      $No_x_Cons =  '2'
097640120113     c*//                   eval      Pmsg3  =  ' NON UTILIZZARE PER'
097650120113     c*//                                    +  ' LASCIATO AVVISO -'
097660120113     c*//                                    +  ' SERVIZIO POSTE'
097670120113     c*//                   write     A4prtLDVB
097680080604e   1c                   endsl
097690040607      *
097700040607     c                   ENDSR
097710030515      *
097720030515      *---------------------------------------------------------------*
097730050217      *?Carico LDV da FNBLP00F                                       ?*
097740030515      *---------------------------------------------------------------*
097750030515     c     CarBLP        BEGSR
097760030515      *
097770030515      * Apertura files
097780030515     c                   if        not %open(FNBLP01L)
097790030515     c                   open      FNBLP01L
097800030515     c                   endif
097810030515     c                   if        not %open(FNARB01L)
097820030515     c                   open      FNARB01L
097830030515     c                   endif
097840070319     ***c v.sr sr_Start     if        not %open(FIAR401L)
097850070319     ***c " "     "         open      FIAR401L
097860070319     ***c " "     "         endif
097870030515     c                   if        not %open(FIAR601L)
097880030515     c                   open      FIAR601L
097890030515     c                   endif
097900030515     c                   if        not %open(FIAR701L)
097910030515     c                   open      FIAR701L
097920030515     c                   endif
097930060505     c                   if        not %open(FIAR901L)
097940060505     c                   open      FIAR901L
097950030515     c                   endif
097960071114      *
097970071114     c                   clear                   DB0cpi
097980030515      *
097990040607     c     KLDV          chain     FNBLP000
098000030515      *
098010030515if  1c                   if        NOT %found(FNBLP01L)
098020081117     c                   leavesr
098030030515e   1c                   endif
098040120509      *
098050120509      * Memorizzazione dei dati di FNBLP che serviranno anche dopo
098060120509      *   il reperimento dell'eventuale bolla mamma...
098070120509     c                   eval      wB0piv =  BLPcpi
098080030515      *
098090030515      * 1ª impostaz. campi della DS in base a quelli del file FNBLP
098100030515     c                   exsr      sr_DS_BLP
098110030515      *
098120070308      * Decodifico filiale di Arrivo
098130030515     c                   exsr      DecLNA
098140030515      *
098150030515      * 2ª impostaz. campi della DS in base a quelli delle tabelle
098160030515      *    "generiche" (3A, TB, 5E)
098170030515     c                   exsr      sr_DS_TAB
098180030515      *
098190030515      * Ultimi dati mancanti:
098200061030      * - Cod. cliente destinatario: imposto LNA9999
098210030515      *    SE   in Assegnato   O   c'è 2ª bolla
098220030515if  1c                   if           %subst(DB0tb1:1:1) = 'A'
098230030515     c                             or DB0tb2            <> *blanks
098240030515     c                   eval      DB0ccd = *all'9'
098250030515     c                   movel     BLPlna        DB0ccd
098260030515e   1c                   endif
098270120509      *
098280120509      * - Reperimento Data spedizione
098290120509if  1c                   if        DB0mgs <> *zeros
098300120509     c                   z-add     DB0mgs        Wdsp
098310120509     c                   movel     DB0aas        Wdsp
098320120509x   1c                   else
098330120509      *   prendo la data del giorno se non c'è la data spedizione
098340120509     c                   time                    w0140
098350120509     c                   move      w0140         w0080
098360120509     c     *eur          movel     w0080         Data_EUR
098370120509     c     *iso          movel     Data_EUR      Wdsp
098380120509e   1c                   endif
098390030515      *
098400061030      * 3ª impostaz. campi della DS: importi da calcolare
098410061031     c                   exsr      sr_DS_IMPO
098420030515      *
098430081117     c                   ENDSR
098440030515      *
098450030515      *---------------------------------------------------------------*
098460050217      *?Carico LDV da FNARB00F                                       ?*
098470030515      *---------------------------------------------------------------*
098480030515     c     CarARB        BEGSR
098490030515      *
098500030515      * Apertura files
098510030515     c                   if        not %open(FNARB01L)
098520030515     c                   open      FNARB01L
098530030515     c                   endif
098540070319     c                   if        NOT %open(FIARN11L)
098550070319     c                   open      FIARN11L
098560070319     c                   endif
098570030515     c                   if        not %open(FIAR601L)
098580030515     c                   open      FIAR601L
098590030515     c                   endif
098600030515     c                   if        not %open(FIAR701L)
098610030515     c                   open      FIAR701L
098620030515     c                   endif
098630060505     c                   if        not %open(FIAR901L)
098640060505     c                   open      FIAR901L
098650030515     c                   endif
098660071114      *
098670071114     c                   clear                   DB0cpi
098680030515      *
098690040607     c     KLDV          chain     FNARB01L
098700030515      *
098710030515if  1c                   if        NOT %found(FNARB01L)
098720070312     c                   clear                   ARBndc
098730070322     c                   clear                   ARBntc
098740070322     c                   clear                   ARBdcp
098750071015     c                   clear                   ARBpdc
098760101129     c                   clear                   ARBdcm
098770101129     c                   clear                   ARBicc
098780101129     c                   clear                   ARBica
098790120509     c                   clear                   ARBcpi
098800081117     c                   leavesr
098810030515e   1c                   endif
098820070514      *
098830070514      * Memorizzazione dei dati di FNARB che serviranno anche dopo
098840070514      *   il reperimento dell'eventuale bolla mamma...
098850070514     c                   eval      wB0ndc =  ARBndc
098860070514     c                   eval      wB0ntc =  ARBntc
098870070514     c                   eval      wB0dcp =  ARBdcp
098880081119     c                   eval      wB0tmc =  ARBtmc
098890071015     c                   move      ARBpdc        Wpdc4a
098900101125     c                   eval      wB0dcm =  ARBdcm
098910101125     c                   eval      wB0icc =  ARBicc
098920101125     c                   eval      wB0ica =  ARBica
098930120509     c                   eval      wB0piv =  ARBcpi
098940030515      *
098950030515      * 1ª impostaz. campi della DS in base a quelli del file FNARB
098960030515     c                   exsr      sr_DS_ARB
098970030515      *
098980070308      * Decodifico filiale di Arrivo
098990030515     c                   exsr      DecLNA
099000030515      *
099010030515      * 2ª impostaz. campi della DS in base a quelli delle tabelle
099020030515      *    "generiche" (3A, TB, 5E)
099030030515     c                   exsr      sr_DS_TAB
099040030515      *
099050030515      * Ultimi dati mancanti:
099060030515      * - Cod. cliente mittente
099070030515if  1c                   If        ARBccm <> *zeros
099080030515     c                   movel     ARBccm        DB0ccm
099090030515x   1c                   Else
099100030515if  2c                   if        %subst(DB0tb1:1:1) = 'F'
099110030515     c                   movel     ARBksc        DB0ccm
099120030515e   2c                   endif
099130030515e   1c                   EndIf
099140030515      * - Cod. cliente destinatario
099150030515if  2c                   if        %subst(DB0tb1:1:1) = 'A'
099160030515     c                   movel     ARBksc        DB0ccd
099170030515e   1c                   endif
099180030515      * - Se c'è 2ª bolla imposto LNA9999
099190030515if  1c                   if        DB0tb2 <> *blanks
099200030515     c                   eval      DB0ccd = *all'9'
099210030515     c                   movel     ARBlna        DB0ccd
099220030515e   1c                   endif
099230120509      * - Reperimento Data spedizione
099240120509if  1c                   if        DB0mgs <> *zeros
099250120509     c                   z-add     DB0mgs        Wdsp
099260120509     c                   movel     DB0aas        Wdsp
099270120509x   1c                   else
099280120509      *   prendo la data del giorno se non c'è la data spedizione
099290120509     c                   time                    w0140
099300120509     c                   move      w0140         w0080
099310120509     c     *eur          movel     w0080         Data_EUR
099320120509     c     *iso          movel     Data_EUR      Wdsp
099330120509e   1c                   endif
099340030515      *
099350061030      * 3ª impostaz. campi della DS: importi da calcolare
099360061031     c                   exsr      sr_DS_IMPO
099370030515      *
099380081117     c                   ENDSR
099390030515      *
099400030515      *---------------------------------------------------------------*
099410050217      *?1ª impostaz. campi della DS in base a quelli del file FNBLP  ?*
099420030515      *---------------------------------------------------------------*
099430030515     c     sr_DS_BLP     BEGSR
099440030515      *
099450030515     c                   eval      DB0cbo = BLPcbo
099460030515     c                   eval      DB0tsp = BLPtsp
099470030515     c                   eval      DB0mgs = BLPmgs
099480030515     c                   eval      DB0tc1 = BLPtc1
099490030515     c                   eval      DB0tc2 = BLPtc2
099500030515     c                   eval      DB0fns = BLPfns
099510030515     c                   eval      DB0rmn = BLPrmn
099520030515     c                   eval      DB0rma = BLPrma
099530030515     c                   eval      DB0ias = BLPias
099540030515     c                   eval      DB0vas = BLPvas
099550030515     c                   eval      DB0vmd = BLPvmd
099560030515     c                   eval      DB0vad = BLPvad
099570030515     c                   eval      DB0qft = BLPqft
099580030515     c                   eval      DB0lna = BLPlna
099590030515     c                   eval      DB0znc = BLPznc
099600030515     c                   eval      DB0rsm = BLPrsm
099610030515     c                   eval      DB0inm = BLPinm
099620030515     c                   eval      DB0cam = BLPcam
099630030515     c                   eval      DB0lom = BLPlom
099640030515     c                   eval      DB0prm = BLPprm
099650030515     c                   eval      DB0nzm = BLPnzm
099660030515     c                   eval      DB0fap = BLPfap
099670030515     c                   eval      DB0rmo = BLPrmo
099680030515     c                   eval      DB0rsd = BLPrsd
099690030515     c                   eval      DB0ind = BLPind
099700030515     c                   eval      DB0cad = BLPcad
099710030515     c                   eval      DB0lod = BLPlod
099720030515     c                   eval      DB0prd = BLPprd
099730030515     c                   eval      DB0nzd = BLPnzd
099740030515     c                   eval      DB0fin = BLPfin
099750030515     c                   eval      DB0gc1 = BLPgc1
099760030515     c                   eval      DB0gc2 = BLPgc2
099770030515     c                   eval      DB0nas = BLPnas
099780030515     c                   eval      DB0ncl = BLPncl
099790030515     c                   eval      DB0pkf = BLPpkf
099800030515     c                   eval      DB0fvf = BLPfvf
099810030515     c                   eval      DB0vlf = BLPvlf
099820030515     c                   eval      DB0fls = BLPfls
099830030515     c                   eval      DB0ncd = BLPncd
099840030515     c                   eval      DB0nca = BLPnca
099850030515     c                   eval      DB0xco = BLPxco
099860030515     c                   eval      DB0ffd = BLPffd
099870030515     c                   eval      DB0dcr = BLPdcr
099880030515     c                   eval      DB0hcr = BLPhcr
099890030515     c                   eval      DB0tcr = BLPtcr
099900030515     c                   eval      DB0fbr = BLPfbr
099910030515     c                   eval      DB0gga = BLPgga
099920030515     c                   eval      DB0gma = BLPgma
099930030515     c                   eval      DB0gva = BLPgva
099940030515     c                   move      ' '           DB0gva
099950030515     c                   eval      DB0sop = BLPsop
099960030515     c                   eval      DB0ctr = BLPctr
099970030515     c                   eval      DB0drt = BLPdrt
099980030515     c                   eval      DB0fpp = BLPfpp
099990030515     c                   eval      DB0nrt = BLPnrt
100000030515     c                   eval      DB0pdr = BLPpdr
100010030515      *
100020030515      * Cod. cliente mittente
100030040311     c                   move      BLPksc        w0040
100040030515if  1c                   IF        BLPccm <> *zeros
100050030515     c                   movel     BLPccm        DB0ccm
100060030515x   1c                   ELSE
100070030515      *   Lo imposto solo se non è codice generico destinatario
100080040311if  2c                   if        w0040  <> 9999
100090030515     c                   movel     BLPksc        DB0ccm
100100030515x   2c                   else
100110030515     c                   movel     BLPksc        DB0ccd
100120030515     c                   movel     BLPlna        DB0ccd
100130030515e   2c                   endif
100140030515e   1c                   ENDIF
100150030515      *
100160030515      * Allega documento di trasporto
100170081119     c                   clear                   DB0bam
100180060505if  1c                   if           BLPfst = 'S'
100190030515     c                             or BLPfst = 'C'
100200030515     c                   movel     'S'           DB0bam
100210060505e   1c                   endif
100220060524      *
100230060524      * Se bolla di RESO: Codice cliente da Assegnato
100240061107     c*** (già fatto:)   clear                   dsBL4A
100250060524if  1c                   if        DB0fbr = 'S'
100260060524     c                   exsr      RepAR4_A
100270060524e   1c                   endif
100280030515      *
100290030515      * Destinatario: vedo se esiste 2ª ragione sociale
100300061107     c                   clear                   DB0rd2
100310061107     c                   exsr      RepAR4_D
100320030515      *
100330030515      * Note
100340061107     c                   clear                   DB0nt1
100350061107     c                   clear                   DB0nt2
100360061107     c                   exsr      RepAR4_89
100370030515      *
100380030515     c                   ENDSR
100390030515      *
100400030515      *---------------------------------------------------------------*
100410050217      *?1ª impostaz. campi della DS in base a quelli del file FNARB  ?*
100420030515      *---------------------------------------------------------------*
100430030515     c     sr_DS_ARB     BEGSR
100440030515      *
100450030515     c                   eval      DB0cbo = ARBcbo
100460030515     c                   eval      DB0tsp = ARBtsp
100470030515     c                   eval      DB0mgs = ARBmgs
100480030515     c                   eval      DB0tc1 = ARBtc1
100490030515     c                   eval      DB0tc2 = ARBtc2
100500030515     c                   eval      DB0fns = ARBfns
100510030515     c                   eval      DB0rmn = ARBrmn
100520030515     c                   eval      DB0rma = ARBrma
100530030515     c                   eval      DB0ias = ARBias
100540030515     c                   eval      DB0vas = ARBvas
100550030515     c                   eval      DB0vmd = ARBvmd
100560030515     c                   eval      DB0vad = ARBvad
100570030515     c                   eval      DB0qft = ARBqft
100580030515     c                   eval      DB0lna = ARBlna
100590030515     c                   eval      DB0znc = ARBznc
100600030515     c                   eval      DB0rsm = ARBrsm
100610030515     c                   eval      DB0inm = ARBinm
100620030515     c                   eval      DB0cam = ARBcam
100630030515     c                   eval      DB0lom = ARBlom
100640030515     c                   eval      DB0prm = ARBprm
100650030515     c                   eval      DB0nzm = ARBnzm
100660030515     c                   eval      DB0fap = ARBfap
100670030515     c                   eval      DB0rmo = ARBrmo
100680030515     c                   eval      DB0rsd = ARBrsd
100690030515     c                   eval      DB0ind = ARBind
100700030515     c                   eval      DB0cad = ARBcad
100710030515     c                   eval      DB0lod = ARBlod
100720030515     c                   eval      DB0prd = ARBprd
100730030515     c                   eval      DB0nzd = ARBnzd
100740030515     c                   eval      DB0fin = ARBfin
100750030515     c                   eval      DB0gc1 = ARBgc1
100760030515     c                   eval      DB0gc2 = ARBgc2
100770030515     c                   eval      DB0nas = ARBnas
100780030515     c                   eval      DB0ncl = ARBncl
100790030515     c                   eval      DB0pkf = ARBpkf
100800030515     c                   eval      DB0fvf = ARBfvf
100810030515     c                   eval      DB0vlf = ARBvlf
100820030515     c                   eval      DB0fls = ARBfls
100830030515     c                   eval      DB0ncd = ARBncd
100840030515     c                   eval      DB0nca = ARBnca
100850030515     c                   eval      DB0xco = ARBxco
100860030515     c                   eval      DB0ffd = ARBffd
100870030515     c                   eval      DB0dcr = ARBdcr
100880030515     c                   eval      DB0hcr = ARBhcr
100890030515     c                   eval      DB0tcr = ARBtcr
100900030515     c                   eval      DB0fbr = ARBfbr
100910030515     c                   eval      DB0gga = ARBgga
100920030515     c                   eval      DB0gma = ARBgma
100930030515     c                   eval      DB0gva = ARBgva
100940030515     c                   move      ' '           DB0gva
100950030515     c                   clear                   DB0sop
100960030515     c                   eval      DB0ctr = ARBctr
100970030515     c                   clear                   DB0drt
100980030515     c                   clear                   DB0fpp
100990030515     c                   clear                   DB0nrt
101000030515     c                   clear                   DB0pdr
101010030515      *
101020030515      * Allega documento di trasporto
101030060524      * & Codice cliente da Assegnato
101040061107     c*** (già fatto:)   clear                   dsBL4A
101050031029     c                   clear                   DB0bam
101060061107     c                   exsr      RepAR4_A
101070060505     c                   if        §B4Abm = 'S' or §B4Abm = 'C' or
101080031029     c                             §B4Abm = 'P' or §B4Abm = 'Q'
101090030515     c                   movel     'S'           DB0bam
101100060505     c                   endif
101110030515      *
101120030515      * Destinatario: vedo se esiste 2ª ragione sociale
101130061107     c                   clear                   DB0rd2
101140061107     c                   exsr      RepAR4_D
101150030515      *
101160030515      * Note
101170061107     c                   clear                   DB0nt1
101180061107     c                   clear                   DB0nt2
101190061107     c                   exsr      RepAR4_89
101200030515      *
101210030515     c                   ENDSR
101220030515      *
101230030515      *---------------------------------------------------------------*
101240050217      *?2ª impostaz. campi della DS in base a quelli delle tabelle   ?*
101250050217      *?   "generiche" (non da "1A")                                 ?*
101260030515      *---------------------------------------------------------------*
101270030515     c     sr_DS_TAB     BEGSR
101280030515      *
101290030515      * Carico le tabelle "generiche" (3A, TB, 5E)
101300030515     c                   exsr      CarTAB
101310030515      *
101320030515      * Codici bolla
101330030515     c                   eval      DB0tb1 = §3Atb1
101340030515     c                   eval      DB0tb2 = §3Atb2
101350030515     c                   eval      DB0dcb = §3Ades
101360030515      * Tipo servizio
101370030515if  1c                   if        DB0dts = *blanks
101380030515     c                   eval      DB0dts = §5Edes
101390030515     c                   eval      DB0fst = §5Efst
101400030515e   1c                   endif
101410030515      * Consegne particolari
101420030515if  1c                   if            DB0tc1 <> *blanks
101430030515     c                             and DB0dc1 =  *blanks
101440030515     c                   movel(p)  DB0tc1        TBLkey
101450030515     c                   exsr      Tab_1P
101460030515     c                   eval      DB0dc1 = §1Pdes
101470030515e   1c                   endif
101480030515if  1c                   if            DB0tc2 <> *blanks
101490030515     c                             and DB0dc2 =  *blanks
101500030515     c                   movel(p)  DB0tc2        TBLkey
101510030515     c                   exsr      Tab_1P
101520030515     c                   eval      DB0dc2 = §1Pdes
101530030515e   1c                   endif
101540030515      * Esenzione IVA
101550030515if  1c                   if            DB0cei <> *blanks
101560030515     c                             and DB0dce =  *blanks
101570030515     c                   eval      DB0dce =  §EIdes
101580030515e   1c                   endif
101590030515      *
101600030515     c                   ENDSR
101610060524      *
101620060524      *---------------------------------------------------------------*
101630060524      *?Reperimento dati da FIAR400F x trk "A"                       ?*
101640061107      *?(dati aggiuntivi per P.Assegnato)                            ?*
101650060524      *---------------------------------------------------------------*
101660060524     c     RepAR4_A      BEGSR
101670060524      *
101680061107     c*** (già fatto:)   clear                   dsBL4A
101690061107      *
101700060524     c                   eval      Wtrc = 'A'
101710060524     c     KLDVtrc       chain     FIAR4000
101720060524      *
101730060524if  1c                   if        %found(FIAR401L)
101740060524     c                   movel     AR4Not        dsBL4A
101750060524e   1c                   endif
101760060524      *
101770060524     c                   ENDSR
101780061107      *
101790061107      *---------------------------------------------------------------*
101800061107      *?Reperimento dati da FIAR400F x trk "D"                       ?*
101810061107      *?(2ª ragione sociale del destinatario)                        ?*
101820061107      *---------------------------------------------------------------*
101830061107     c     RepAR4_D      BEGSR
101840061107      *
101850061107     c*** NON esiste:    clear                   dsBL4D
101860061107     c*** (già fatto:)   clear                   DB0rd2
101870061107      *
101880061107     c                   eval      Wtrc = 'D'
101890061107     c     KLDVtrc       chain     FIAR4000
101900061107      *
101910061107if  1c                   if        %found(FIAR401L)
101920061107     c                   movel     AR4not        DB0rd2
101930061107e   1c                   endif
101940061107      *
101950061107     c                   ENDSR
101960061107      *
101970060626      *---------------------------------------------------------------*
101980060626      *?Reperimento dati da FIAR400F x trk "I"                       ?*
101990061107      *?(dati DPD)                                                   ?*
102000060626      *---------------------------------------------------------------*
102010061030     c     RepAR4_I      BEGSR
102020061030      *
102030061107     c*** (già fatto:)   clear                   dsBL4I
102040061107      *
102050061030     c                   eval      Wtrc = 'I'
102060061030     c     KLDVtrc       chain     FIAR4000
102070061030      *
102080061030if  1c                   if        %found(FIAR401L)
102090061030     c                   movel     AR4not        dsBL4I
102100060626e   1c                   endif
102110061030      *
102120061030     c                   ENDSR
102130071008      *
102140071015      *---------------------------------------------------------------*
102150071015      *?Reperimento dati da FIAR400F x trk "M"                       ?*
102160071015      *?(dati aggiuntivi per O.R.M. abbinato al P.Franco PrePagato)  ?*
102170071015      *---------------------------------------------------------------*
102180071015     c     RepAR4_M      BEGSR
102190071015      *
102200071015     c*** (già fatto:)   clear                   dsBL4M
102210071015      *
102220071015     c                   eval      Wtrc = 'M'
102230071015     c     KLDVtrc       chain     FIAR4000
102240071015      *
102250071015if  1c                   if        %found(FIAR401L)
102260071015     c                   movel     AR4Not        dsBL4M
102270071015e   1c                   endif
102280071015      *
102290071015     c                   ENDSR
102300061107      *
102310061107      *---------------------------------------------------------------*
102320061107      *?Reperimento dati da FIAR400F x trk "Q"                       ?*
102330061107      *?(Codice Fiscale del mittente a sx & del destinatario a dx)   ?*
102340061107      *---------------------------------------------------------------*
102350061107     c     RepAR4_Q      BEGSR
102360061107      *
102370061108     c*** (già fatto:)   clear                   dTA4Q
102380061107     c*** (già fatto:)   clear                   DB0cpi
102390061107      *
102400061107     c                   eval      Wtrc = 'Q'
102410061107     c     KLDVtrc       chain     FIAR4000
102420061107      *
102430061107if  1c                   if        %found(FIAR401L)
102440061108     c                   movel     AR4not        dTA4Q
102450061108e   1c                   endif
102460061107      *
102470061107     c                   ENDSR
102480061107      *
102490061107      *---------------------------------------------------------------*
102500061107      *?Reperimento dati da FIAR400F x trk "8" / "9"                 ?*
102510061107      *?(note)                                                       ?*
102520061107      *---------------------------------------------------------------*
102530061107     c     RepAR4_89     BEGSR
102540061107      *
102550061107     c*** NON esiste:    clear                   dsBL48
102560061107     c*** NON usata:     clear                   dsBL49
102570061107     c*** (già fatto:)   clear                   DB0nt1
102580061107     c*** (già fatto:)   clear                   DB0nt2
102590061107      *
102600061107     c                   eval      Wtrc = '8'
102610061107     c     KLDVtrc       chain     FIAR4000
102620061107if  1c                   if        %found(FIAR401L)
102630061107     c                   movel     AR4not        DB0nt1
102640061107e   1c                   endif
102650061107      *
102660061107     c                   eval      Wtrc = '9'
102670061107     c     KLDVtrc       chain     FIAR4000
102680061107if  1c                   if        %found(FIAR401L)
102690061107     c                   movel     AR4not        DB0nt2
102700061107e   1c                   endif
102710061107      *
102720061107     c                   ENDSR
102730030515      *
102740030515      *---------------------------------------------------------------*
102750050217      *?Carico le tabelle che servono: 3A/TB/5E.                     ?*
102760030515      *---------------------------------------------------------------*
102770030515     c     CarTAB        BEGSR
102780030515      *
102790030515      * Tab. "3A" - Codici Bolla
102800030515      * (NO se rimesso mittente)
102810030515     c                   clear                   ds3A
102820030515if  1c                   if        DB0ris <> 'M'
102830030515     c                   movel     '3A'          TBLcod
102840030515     c                   movel(p)  DB0cbo        TBLkey
102850030515     c     K03TAB        chain     TABEL
102860030515if  2c                   if        %found(TABEL00F)
102870030515     c                   movel     TBLuni        ds3A
102880030515e   2c                   endif
102890030515e   1c                   endif
102900070515     c                   eval      $Mamma =  (§3Abcm = 'S')
102910030515      *
102920030515      * Tab. "TB" - Tipi Bolla
102930030515      * (NO se rimesso mittente)
102940030515      * se Bolla in assegnato, con tipo bolla vedo se da contabilizzare
102950030515      * se no non stampo i dati fattura
102960030515     c                   clear                   dsTB
102970030515if  1c                   if        DB0ris <> 'M'
102980030515     c                   movel     'TB'          TBLcod
102990061030if  2c                   if            §3Atb2 <> *blanks
103000121029     c                             and DB0tbo <> 'P'
103010030515     c                             and DB0tbo <> '8'
103020030515     c                   movel(p)  §3Atb2        TBLkey
103030030515x   2c                   else
103040030515     c                   movel(p)  §3Atb1        TBLkey
103050030515e   2c                   endif
103060030515     c     K03TAB        chain     TABEL
103070030515if  2c                   if        %found(TABEL00F)
103080030515     c                   movel     TBLuni        dsTB
103090030515e   2c                   endif
103100030515e   1c                   endif
103110030515      *
103120030515      * Tab. "5E" - Tipi Servizio
103130030515     c                   clear                   ds5E
103140090513if  1c                   if        DB0tsp <> *blanks
103150030515     c                   movel     '5E'          TBLcod
103160030515     c                   movel(p)  DB0tsp        TBLkey
103170030515     c     K03TAB        chain     TABEL
103180030515if  2c                   if        %found(TABEL00F)
103190030515     c                   movel     TBLuni        ds5E
103200030515e   2c                   endif
103210030515e   1c                   endif
103220030515      *
103230030911      * Tab. "EI" - Esenzioni I.V.A.
103240030515     c                   clear                   dsEI
103250030911      * Tab. "1A" - Tipi incasso contrassegno
103260030911     c                   clear                   ds1A
103270030911      * Tab. "1P" - Consegne particolari
103280030911     c                   clear                   ds1P
103290150304      *//* Tab. "TBA" - Tipi bancale
103300150304     c*//                clear                   dTBA
103310030515      *
103320030515     c                   ENDSR
103330030911      *
103340030911      *---------------------------------------------------------------*
103350050217      *?Carico tabella "EI" (Esenzione I.V.A.)                       ?*
103360030911      *---------------------------------------------------------------*
103370030911     c     Tab_EI        BEGSR
103380030911      *
103390030911     c                   clear                   dsEI
103400030911if  1c                   if        DB0cei <> *blanks  and  DB0dce = *blanks
103410030911     c                   movel     'EI'          TBLcod
103420030911     c                   movel(p)  DB0cei        TBLkey
103430030911     c     K03TAB        chain     TABEL
103440030911if  2c                   if        %found(TABEL00F)
103450030911     c                   movel     TBLuni        dsEI
103460030911e   2c                   endif
103470030911e   1c                   endif
103480030911     c                   eval      DB0dce =  §EIdes
103490030911      *
103500030911     c                   ENDSR
103510030515      *
103520030515      *---------------------------------------------------------------*
103530050217      *?Carico tabella "1A" (Tipo incasso contrassegno)              ?*
103540030515      *---------------------------------------------------------------*
103550030515     c     Tab_1A        BEGSR
103560030515      *
103570030515     c                   clear                   ds1A
103580030515if  1c                   if        DB0cas <> *zeros
103590030515     c                   movel     '1A'          TBLcod
103600030515     c                   movel(p)  DB0tic        TBLkey
103610030515     c     K03TAB        chain     TABEL
103620030515if  2c                   if        %found(TABEL00F)
103630030515     c                   movel     TBLuni        ds1A
103640030515e   2c                   endif
103650030515e   1c                   endif
103660030515      *
103670030515      * Imposto i dati nella DS
103680030515     c                   eval      DB0dti = §1Ades
103690030515     c                   eval      DB0ucm = §1Aucm
103700030515     c                   eval      DB0ste = §1Aste
103710030515      *
103720030515     c                   ENDSR
103730030515      *
103740030515      *---------------------------------------------------------------*
103750050217      *?Carico tabella "1P" (Consegne particolari)                   ?*
103760030515      *---------------------------------------------------------------*
103770030515     c     Tab_1P        BEGSR
103780030515      *
103790030515     c                   clear                   ds1P
103800030515     c                   movel     '1P'          TBLcod
103810030515     c*** già impostato: movel(p)  DB0tc1/2      TBLkey
103820030515     c     K03TAB        chain     TABEL
103830030515if  1c                   if        %found(TABEL00F)
103840030515     c                   movel     TBLuni        ds1P
103850030515e   1c                   endif
103860030515      *
103870030515     c                   ENDSR
103880030515      *
103890150304      *//*---------------------------------------------------------------*
103900150304      *//*?Reperisco tabella "TBA" (Tipi Bancale)                       ?*
103910150304      *//*---------------------------------------------------------------*
103920150304     c*//  Tab_TBA       BEGSR
103930150304      *//*
103940150304     c*//                clear                   dTBA
103950150304     c*//                reset                   TIBS02ds
103960150304     c*//                eval      T02cod = 'TBA'
103970150304     c*//                movel(p)  Wtba          T02ke1
103980150304     c*//                call      'TIBS02R'
103990150304     c*//                parm                    KPJBA
104000150304     c*//                parm                    TIBS02ds
104010150304if  1c*//                if        T02err = *blanks
104020150304     c*//                movel     T02uni        dTBA
104030150304e   1c*//                endif
104040150304      *//*
104050150304     c*//                ENDSR
104060030515      *
104070030515      *---------------------------------------------------------------*
104080050217      *?Calcolo importi LDV                                          ?*
104090030515      *---------------------------------------------------------------*
104100061031     c     sr_DS_IMPO    BEGSR
104110030515      *
104120030515      * Vedo se esiste tassazione
104130030515      *  solo se tipo bolla contabilizzabile
104140061030if  1c                   IF        §TBfcb =  '1'
104150030515     c
104160061030     c                   eval      Wtrc   =  '1'
104170040607     c     KLDVtrc       chain     FIAR6000
104180030515     c
104190030515if  2c                   if        %found(FIAR601L)
104200030515     c                   movel     AR6div        DB0div
104210030515     c                   exsr      CtrDIV
104220030515      *
104230030515      * NON c'è 2ª bolla
104240061031if  3c                   if            DB0tb2 = *blanks
104250121029     c                             or  DB0tbo = 'P'
104260061031     c                             or  DB0tbo = '8'
104270030515      *  Estremi fattura
104280030515     c                   z-add     AR6nft        DB0nft
104290030515     c                   z-add     AR6dft        DB0dft
104300130124     c                   z-add     AR6fiv        wB0fiv
104310061031      *  Se è assegnato o franco prepagato => stampo tutta la tassaz.
104320061031if  4c                   if            %subst(DB0tb1:1:1) = 'A'
104330061031     c                             or  DB0tbo = '8'
104340061030     c                   eval      Wtrc   =  '1'
104350030515     c                   exsr      RepTASSAZ
104360030515e   4c                   endif
104370030515e   3c                   endif
104380030515e   2c                   endif
104390030515      *
104400061030      * C'è 2ª bolla
104410061031if  2c                   IF            DB0tb2 <> *blanks
104420121029     c                             and DB0tbo <> 'P'
104430061031     c                             and DB0tbo <> '8'
104440030515      *
104450061030     c                   eval      Wtrc   =  '2'
104460040607     c     KLDVtrc       chain     FIAR6000
104470030515      *
104480061030if  3c                   If        %found(FIAR601L)
104490030515     c                   movel     AR6div        DB0div
104500030515     c                   exsr      CtrDIV
104510030515     c                   movel     AR6cei        DB0cei
104520030515      *   Estremi fattura
104530030515     c                   z-add     AR6nft        DB0nft
104540030515     c                   z-add     AR6dft        DB0dft
104550130124     c                   z-add     AR6fiv        wB0fiv
104560030515if  4c                   if        AR6ksc <> *zeros
104570030515     c                   eval      DB0ccd =  AR6ksc
104580030515e   4c                   endif
104590061030     c                   eval      Wtrc   =  '2'
104600030515     c                   exsr      RepTASSAZ
104610061030e   3c                   EndIf
104620030515      *
104630030515e   2c                   ENDIF
104640030515      *
104650030515e   1c                   ENDIF
104660030515      *
104670030515      * Vedo se ha contrassegno
104680060505     c     KLDV          chain     FIAR9000
104690060505if  1c                   IF        %found(FIAR901L)
104700061030     c                   eval      DB0cas =  AR9cas
104710061030     c                   eval      DB0vca =  AR9vca
104720061030     c                   eval      DB0gca =  AR9gca
104730061030     c                   eval      DB0tic =  AR9tic
104740030515x   1c                   ELSE
104750030515     c                   clear                   DB0cas
104760030515     c                   clear                   DB0vca
104770030515     c                   clear                   DB0gca
104780030515     c                   clear                   DB0tic
104790030515     c                   clear                   DB0dti
104800030515     c                   clear                   DB0ucm
104810030515     c                   clear                   DB0ste
104820030515e   1c                   ENDIF
104830120509      *
104840120509      * Se P.F. Prepagato verificherò il Codice Fiscale del Mittente
104850120509      *   (NON se mittente estero)
104860120509if  1c                   if           ( DB0tbo  = '8'
104870120509     c                             and (DB0nft  <> *zeros
104880120509     c                              or  DB0ift  <> *zeros)
104890120509     c                             and  DB0nzm  =  *blanks )
104900120509     c                   exsr      RepAR4_Q
104910120509     c                   movel     §TA4Qcfm      DB0cpi
104920120509e   1c                   endif
104930121029      * Se P.A. con fattura immediata o con 2ª bolla verificherò il
104940121029      *   Codice Fiscale (NON se destinatario estero)
104950121026if  1c*//                if           ( DB0tbo  = 'A'
104960121029if  1c                   if          ( (%subst(DB0tb1:1:1) = 'A'
104970121029     c                             or  (DB0tb2  <> *blanks
104980121029     c                             and  DB0tbo  <> 'P'
104990121029     c                             and  DB0tbo  <> '8') )
105000120509     c                             and (DB0nft  <> *zeros
105010120509     c                              or  DB0ift  <> *zeros)
105020120509     c                             and  DB0nzd  =  *blanks )
105030120509     c                   exsr      RepAR4_Q
105040120509     c                   movel     §TA4Qcfd      DB0cpi
105050120509e   1c                   endif
105060120509     c                   eval      wB0cfi = DB0cpi
105070061030      *
105080030515      * Vedo se si tratta di bolla figlia:
105090040607      * Se bolla con MAMMA prendo c/a e segnacolli da bolla MAMMA
105100070515if  1c                   if        $Mamma =  *on
105110081117     c                   exsr      BollaFiglia
105120030515e   1c                   endif
105130030515      *
105140030515     c                   ENDSR
105150030515      *
105160030515      *---------------------------------------------------------------*
105170050217      *?Decodifica LNA                                               ?*
105180030515      *---------------------------------------------------------------*
105190030515     c     DecLNA        BEGSR
105200030515      *
105210140307if  1c*//                if            DB0dla =  *blanks
105220140307     c*//                          and DB0ris <> 'M'
105230140307if  1c                   if        DB0ris <> 'M'
105240030515      *
105250040311     c                   eval      SAVlna = DB0lna
105260070312     c                   clear                   OG146arr
105270061016     c                   clear                   OG148arr
105280070312     c                   clear                   OG150arr
105290030515     c     DB0lna        chain     AZORG
105300030515      *
105310030515if  2c                   if        %found(AZORG01L)
105320030515     c                   eval      DB0fag = ORGfag
105330030515     c                   eval      DB0dla = ORGdes
105340030515     c                   eval      DB0inl = ORGind
105350030515     c                   eval      DB0cal = ORGcpf
105360030515     c                   eval      DB0lol = ORGloc
105370030515     c                   eval      DB0prl = ORGpro
105380030515     c                   eval      DB0tel = ORGtel
105390140307     c*//                eval      DB0fax = ORGfax
105400140307     c                   eval      DB0fax = ORGdd2
105410070312     c                   movel     ORGde6        OG146arr
105420061016     c                   movel     ORGde8        OG148arr
105430070312     c                   movel     ORGdf0        OG150arr
105440030515e   2c                   endif
105450030515      *
105460030515e   1c                   endif
105470030515      *
105480030515     c                   ENDSR
105490030515      *
105500030515      *---------------------------------------------------------------*
105510050217      *?Reperimento dati di tutta la tassazione per la stampa        ?*
105520030515      *---------------------------------------------------------------*
105530030515     c     RepTASSAZ     BEGSR
105540030515      *
105550030515     c                   eval      DB0imv = AR6imv
105560030515     c                   eval      DB0cei = AR6cei
105570030515     c                   eval      DB0ift = AR6ift
105580030911      * Esenzione IVA
105590030911if  1c                   if            DB0cei <> *blanks
105600030911     c                             and DB0dce =  *blanks
105610030911     c                   exsr      Tab_EI
105620030911e   1c                   endif
105630030515      *
105640030515      * Controllo se ci sono varie esenti
105650030515      *
105660030515      * - varia 1
105670030515     c                   eval      Wsvx = AR6sv1
105680030515     c                   eval      Wvax = AR6va1
105690030515     c                   exsr      RepTASSAZ_b
105700030515      * - varia 2
105710030515     c                   eval      Wsvx = AR6sv2
105720030515     c                   eval      Wvax = AR6va2
105730030515     c                   exsr      RepTASSAZ_b
105740030515      * - varia 3
105750030515     c                   eval      Wsvx = AR6sv3
105760030515     c                   eval      Wvax = AR6va3
105770030515     c                   exsr      RepTASSAZ_b
105780030515      * - varie ulteriori solo per tipo record 1 (prima bolla)
105790040607     c     KLDVtrc       setll     FIAR7000
105800040607     c     KLDVtrc       reade     FIAR7000
105810030515      *
105820030515do  2c                   Dow       Not %eof(FIAR701L)
105830030515      *
105840030515     c                   eval      Wsvx = AR7svn
105850030515     c                   eval      Wvax = AR7van
105860030515     c                   exsr      RepTASSAZ_b
105870030515      *
105880040607     c     KLDVtrc       reade     FIAR7000
105890030515e   2c                   Enddo
105900030515      *
105910030515     c                   ENDSR
105920030515      *
105930030515      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
105940050217      *?Reperimento dati di tutta la tassazione per la stampa        ?*
105950050217      *?- 2ª parte -                                                 ?*
105960030515      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
105970030515     c     RepTASSAZ_b   BEGSR
105980030515      *
105990030515sel 1c                   Select
106000030515      *
106010030515w   1c                   When      Wsvx       = *blanks
106020030515      *
106030030515w   1c                   When      Wsvx       = §$2iva
106040030515     c                   eval      DB0iva     = Wvax
106050030515      *
106060030515w   1c                   When      Wsvx       = §$2bol
106070030515     c                   add       Wvax          DB0ese
106080050624     c                   z-add     Wvax          wBolloVirt
106090050624     c                   eval      $BolloVirt = *on
106100030515      *
106110030515x   1c                   Other
106120050624     c                   eval      *in31      = *off
106130030515     c     Wsvx          lookup    $Var                                   31
106140030515     c   31              add       Wvax          DB0ese
106150030515     c   31              sub       Wvax          DB0imv
106160030515      *
106170030515w   1c                   Endsl
106180030515      *
106190030515     c                   ENDSR
106200030515      *
106210030515      *---------------------------------------------------------------*
106220050217      *?Chain su TABEL per le PARTICOLARITA'                         ?*
106230030515      *---------------------------------------------------------------*
106240030515     c     TABxPAR       BEGSR
106250030515      *
106260100513     c                   movel(p)  WCdPar        w053a
106270030515     c                   movel     '7V'          TBLcod
106280030515     c                   movel(p)  KeyPAR        TBLkey
106290030515     c     K03TAB        chain     TABEL
106300030515      *
106310030515if  1c                   if        %found(TABEL00F)
106320030515     c                             and TBLflg = *blanks
106330100513     c                   eval      w053a = WCdPar + '-'
106340100513     c                                   + %trim(%subst(TBLuni:1:49))
106350030515x   1c                   else
106360030515     c                   clear                   TBLuni
106370030515e   1c                   endif
106380030515      *
106390030515     c                   ENDSR
106400030515      *
106410030515      *---------------------------------------------------------------*
106420050217      *?CONTROLLO C/A SE E' DA CONVERTIRE IN EURO                    ?*
106430030515      *---------------------------------------------------------------*
106440030515     c     CtrCAs        BEGSR
106450030515      *
106460030515      * E' da convertire
106470030515if  1c                   if        DB0ucm = 'S'
106480030515     c                   eval      $CAeur = *on
106490030515e   1c                   endif
106500030515      *
106510030515      * Se in EURO è da convertire e da stampare in EURO
106520030515if  1c                   if        DB0vca = §GEDcn
106530030515     c                   eval      $CAeur = *on
106540030515     c                   endif
106550030515      *
106560030515      * Devo convertire il c/a da valuta a moneta di conto
106570030624if  1c                   if        DB0vca <> §GEDcn
106580030515     c                   clear                   YEURCODS
106590030515     c                   movel     DB0vca        YECdvi
106600030515     c                   z-add     DB0cas        YECimi
106610030515     c                   movel     §GEDcn        YECdvo
106620030515     c                   call      'YEURCO'
106630030515     c                   parm                    YEURCODS
106640030515if  2c                   if        YECesi = *blanks
106650030515     c                   z-add     YECimo        Wcas
106660030515e   2c                   endif
106670030515x   1c                   else
106680030515     c                   z-add     DB0cas        Wcas
106690030515e   1c                   endif
106700030515      *
106710030515     c                   ENDSR
106720030515      *
106730030515      *---------------------------------------------------------------*
106740050217      *?CONTROLLO DIVISA DELLA FATTURA                               ?*
106750030515      *---------------------------------------------------------------*
106760030515     c     CtrDIV        BEGSR
106770030515      *
106780030515     c                   clear                   dsCV
106790030515      *
106800030515     c                   movel     'CV'          TBLcod
106810030515     c                   movel(p)  DB0div        TBLkey
106820030515     c     K03TAB        chain     TABEL
106830030515     c                   if        %found(TABEL00F)
106840030515     c                   movel     TBLuni        dsCV
106850030515     c                   movel     §CVfdc        DB0fdc
106860030515     c                   endif
106870030515      *
106880030515     c                   ENDSR
106890030515      *
106900030515      *---------------------------------------------------------------*
106910050217      *?SE BOLLA CON MAMMA PRENDO C/A E SEGNACOLLI DA BOLLA MAMMA    ?*
106920030515      *---------------------------------------------------------------*
106930081117     c     BollaFiglia   BEGSR
106940030515      *
106950090528     c                   if        not %open(FNLBL01L)
106960090528     c                   open      FNLBL01L
106970090528     c                   endif
106980090528      *
106990040607     c     KLDV          chain     FNLBL000
107000030515if  1c                   if        %found(FNLBL01L)
107010030515      *
107020030515     c     KLBL          chain     FNARB000
107030030515if  2c                   if        %found(FNARB01L)
107040030515      *
107050031015     c                   eval      wB0nrs = ARBnrs
107060030515     c                   eval      DB0fns = ARBfns
107070030515     c                   eval      DB0fls = ARBfls
107080030515     c                   eval      DB0ncd = ARBncd
107090030515     c                   eval      DB0nca = ARBnca
107100050114     c**!!!              eval      DB0xco = ARBxco
107110030515     c                   eval      DB0znc = ARBznc
107120030515      * Aggancio AR9
107130060505     c     KLBL          chain     FIAR9000
107140060505if  3c                   IF        %found(FIAR901L)
107150030515     c                   eval      DB0cas = AR9cas
107160030515     c                   eval      DB0vca = AR9vca
107170030515     c                   eval      DB0gca = AR9gca
107180030515     c                   eval      DB0tic = AR9tic
107190030515x   3c                   ELSE
107200030515     c                   clear                   DB0cas
107210030515     c                   clear                   DB0vca
107220030515     c                   clear                   DB0gca
107230030515     c                   clear                   DB0tic
107240030515     c                   clear                   DB0dti
107250030515     c                   clear                   DB0ucm
107260030515     c                   clear                   DB0ste
107270030515e   3c                   ENDIF
107280030515      *
107290030515e   2c                   endif
107300030515      *
107310030515e   1c                   endif
107320030515      *
107330030515     c                   ENDSR
107340030903      *
107350030903      *---------------------------------------------------------------*
107360050217      *?Impostazione Quantità e Unità di Misura da FNORM             ?*
107370030903      *---------------------------------------------------------------*
107380030903     c     QUM_FNORM     BEGSR
107390030903      *
107400030905     c*** (già fatto:)   clear                   $Qum
107410030905     c                   clear                   Wx
107420030903      * Colli
107430030905if  1c                   if        ORMncl   > *zeros
107440030905     c                             and Wx   < %elem($Qum)
107450030905     c                   add       1             wx
107460030905     c                   eval      $Qum(wx) = %editc(ORMncl : '4')
107470030905     c                                      + ' colli'
107480030903e   1c                   endif
107490030903      * Peso
107500030905if  1c                   if        ORMpkg   > *zeros
107510030905     c                             and Wx   < %elem($Qum)
107520030905     c                   add       1             wx
107530030905     c                   eval      $Qum(wx) = %editc(ORMpkg : '4')
107540030905     c                                      + ' kg'
107550030905e   1c                   endif
107560071025      * Volume bancali
107570071025if  1c                   if        ORMvlm   > *zeros
107580071025     c                             and Wx   < %elem($Qum)
107590071025     c                   add       1             wx
107600071025     c                   eval      $Qum(wx) = %editc(ORMvlm : '4')
107610071025     c                                      + ' mc'
107620071025e   1c                   endif
107630030905     *** * Volume
107640030905     *** * (in ultimo posto SOLO SE non ci sono bilici/motrici/autotreni)
107650030905if  1***c                   if        ORMvlm   > *zeros
107660030905     ***c                             and (Wx  < %elem($Qum) - 1
107670030905     ***c                              or  Wx  = %elem($Qum) - 1
107680030905     ***c                              and w0112 = *zeros)
107690030905     ***c                   add       1             wx
107700030905     ***c                   eval      $Qum(wx) = %editc(ORMvlm : '4')
107710030905     ***c                                      + ' metri cubi'
107720030905e   1***c                   endif
107730030905     *** * Bancali
107740030905     *** * (in ultimo posto SOLO SE non ci sono bilici/motrici/autotreni)
107750030905if  1***c                   if        ORMbnc   > *zeros
107760030905     ***c                             and (Wx  < %elem($Qum) - 1
107770030905     ***c                              or  Wx  = %elem($Qum) - 1
107780030905     ***c                              and w0112 = *zeros)
107790030905     ***c                   add       1             wx
107800030905     ***c                   eval      $Qum(wx) = %editc(ORMbnc : '4')
107810030905     ***c                                      + ' bancali'
107820030905e   1***c                   endif
107830030905     *** * Bilici
107840030905if  1***c                   if        ORMblc  > *zeros
107850030905     ***c                             and Wx  < %elem($Qum)
107860030905     ***c                   add       1             wx
107870030905     ***c                   eval      $Qum(wx) = %editc(ORMblc : '4')
107880030905     ***c                                      + ' bilici'
107890030905e   1***c                   endif
107900030905     *** * Motrici
107910030905if  1***c                   if        ORMmtc  > *zeros
107920030905     ***c                             and Wx  < %elem($Qum)
107930030905     ***c                   add       1             wx
107940030905     ***c                   eval      $Qum(wx) = %editc(ORMmtc : '4')
107950030905     ***c                                      + ' motrici'
107960030905e   1***c                   endif
107970030905     *** * Autotreni
107980030905if  1***c                   if        ORMatt  > *zeros
107990030905     ***c                             and Wx  < %elem($Qum)
108000030905     ***c                   add       1             wx
108010030905     ***c                   eval      $Qum(wx) = %editc(ORMatt : '4')
108020030905     ***c                                      + ' autotreni'
108030030905e   1***c                   endif
108040030903      *
108050030903     c                   ENDSR
108060130306      *
108070130306      /free
108080130306
108090130306       //--------------------------------------------------------------
108100130306       //?Scrittura record (relativo alla spedizione) nel *file PDFLIST?
108110130306       //--------------------------------------------------------------
108120130306       BEGSR  sr_WrtRec_PdfList;
108130130306
108140130307         // -?NON richiesta stampa del PDF?
108150130710         if  %parms() = 3;
108160130306           leavesr;
108170130306         endif;
108180130306
108190130306         // -?Reperimento numero "chiave elenco"?(SE non ricevuto)?
108200130306         If  DB6num <= *zero;
108210130306
108220130306           clear  TRUL33ds;
108230130306           I33tla = 'L';
108240130306           I33ope = *zero;
108250130306           I33cnu = 55;
108260130306           I33num = 1;
108270130306           kpjbu  = TRUL33ds;
108280130306
108290130306           TRUL33R ( kpjba );
108300130306
108310130306           TRUL33ds = kpjbu;
108320130307           if  O33err > *zero;
108330130307             Pmsg1  = $Msg(3);
108340130307             Pmsg2  = $Msg(O33err + 3);
108350130418             *in33  = *off;
108360130418             exsr  Posiz_Box;
108370130307             write  A4prtMSG;
108380130307             DB0tms = 'E';
108390130306             exsr  sr_End;
108400130306           endif;
108410130306
108420130306           DB6num = %editc( O33nri : 'X' );
108430130306
108440130306         EndIf;
108450130307
108460130307         // -?Apertura file?
108470130307         if  Not %open(PDFLIST);
108480130307           open  PDFLIST;
108490130307         endif;
108500130306
108510130308         // -?Ciclo di lettura record "§..." (ex "§PDF") in FIAR801L?
108520130306         clear  k06fiar801;
108530130306         k_AR8aas = DB0aas;
108540130306         k_AR8lnp = DB0lnp;
108550130306         k_AR8nrs = DB0nrs;
108560130306         k_AR8nsp = DB0nsp;
108570130306         k_AR8tmd = c_§PDF;
108580130306         setll  %kds( k06fiar801 : 5 )  FIAR8000;
108590130308         reade  %kds( k06fiar801 : 4 )  FIAR8000;
108600130306
108610130308         DoW  Not %eof(FIAR801L)  and  %subst(AR8tmd : 1 : 1) = '§';
108620130306
108630130306           dAR8§PDF = AR8uni;
108640130308
108650130315           If  AR8atb = *blank  and  §AR8Ptrt = 'S';
108660130306
108670130308             // -?Scrittura del rec. relativo alla sped. nel *file PDFLIST?
108680130308             clear  RPDF;
108690130308             PDFklist  = DB6num;
108700130308             PDFksped  = %editc( DB0aas : 'X' ) + %editc( DB0lnp : 'X' ) +
108710130308                         %editc( DB0nrs : 'X' ) + %editc( DB0nsp : 'X' );
108720130311             PDFkey    = %trimr(§AR8PpdfP) + '_' + §AR8PpdfN;
108730130311             //PDFpath   = *blank;
108740130308             PDFinfo   = c_CopiaXdest + ' - ' + Prsd +
108750130308                         ' Spedizione ' + %editc(DB0lnp : 'X') +
108760130308                                    ' ' + %editc(DB0nrs : 'X') +
108770130308                                    ' ' + %editc(DB0nsp : 'X') +
108780130308                         ' del ' + %editc(Pdsp : 'Y') + ' ' +
108790130308                         Pndc;
108800130308             PDFresol  = 'S';
108810130308             PDFcopieM = '1';
108820130308             PDFcopieD = '0';
108830130308             PDFcopieS = '0';
108840130308             //PDFnoInfo = *blank;
108850130308             //PDFinfoP  = *blank;
108860130308             //PDFprtFst = *blank;
108870130308             wTime_ds  = %char( %timestamp() );
108880131008             PDFinsDt  = wTime_ds.wDate_Iso;
108890131008             PDFinsTm  = wTime_ds.wTime;
108900131008             //PDFprtDt  = wTime_ds.wDate_Iso;
108910131008             //PDFprtTm  = wTime_ds.wTime;
108920130308             PDFusrins = SDSusr;
108930130308             //__________
108940130308             write  RPDF;
108950130308             //¯¯¯¯¯¯¯¯¯¯
108960130308
108970130308           EndIf;
108980130306
108990130418           reade  %kds( k06fiar801 : 4 )  FIAR8000;
109000130306
109010130306         EndDo;
109020130306
109030130306       ENDSR;
109040130306
109050130306       //--------------------------------------------------------------
109060140307       //?Stampa immagini associate alle spediz. (dati già in PDFLIST) ?
109070130306       //--------------------------------------------------------------
109080130306       BEGSR  sr_PrtPdf;
109090130306
109100130306         // -?Impostazione parametri?
109110130312         PDF_KList    = DB6num;
109120130312         PDF_Printer  = trulPOVRds.§ULPoOutQ;
109130130312         PDF_FormType = trulPOVRds.§ULPoFType;
109140130312         PDF_UsrDta   = trulPOVRds.§ULPoUDta;
109150130306
109160130306         // -?Richiamo funzione di stampa di tutte le immagini PDF?
109170130312         //  ?(segnalando - in stampa - l'eventuale errore)?
109180130312         callp(E)  Stampa_PDF ( PDF_KList :
109190130312                                PDF_Printer :
109200130312                                PDF_FormType :
109210130312                                PDF_UsrDta );
109220130312
109230130312         if  %error;
109240130312           Pmsg2  = $Msg(7);
109250130418           *in33  = *off;
109260130418           exsr  Posiz_Box;
109270130312           write  A4prtMSG;
109280130312           DB0tms = 'E';
109290130312           exsr  sr_End;
109300130312         endif;
109310130307
109320130307         // -?Restituzione tipo modulo stampato?
109330130307         DB0tms = 'F';
109340130306
109350130306       ENDSR;
109360130306
109370130306      /end-free
109380030903      *
109390030515      *---------------------------------------------------------------*
109400050217      *?DEFINIZIONE KEY-LIST                                         ?*
109410030515      *---------------------------------------------------------------*
109420030515     c     DefKLIST      BEGSR
109430030515      *
109440051115      * - FNBLP01L / FNBLT01L /
109450060505      *   FNARB01L / FNART01L /
109460030515      *   FNLBL01L /
109470060505      *   FIAR501L/31C / FIAR901L
109480040607     c     KLDV          klist
109490030515     c                   kfld                    DB0aas
109500030515     c                   kfld                    DB0lnp
109510030515     c                   kfld                    DB0nrs
109520030515     c                   kfld                    DB0nsp
109530060505      * - FIAR401L / FIAR601L / FIAR701L
109540060505     c     KLDVtrc       klist
109550060505     c                   kfld                    DB0aas
109560060505     c                   kfld                    DB0lnp
109570060505     c                   kfld                    DB0nrs
109580060505     c                   kfld                    DB0nsp
109590060505     c                   kfld                    Wtrc
109600040607      * - FIAR501L/31C
109610030717     c     K05AR501      klist
109620030717     c                   kfld                    DB0aas
109630030717     c                   kfld                    DB0lnp
109640030717     c                   kfld                    DB0nrs
109650030717     c                   kfld                    DB0nsp
109660030717     c                   kfld                    AR5trd
109670040607      * - FIAR801L
109680040607     c     K06AR801      klist
109690040607     c                   kfld                    DB0aas
109700040607     c                   kfld                    DB0lnp
109710040607     c                   kfld                    DB0nrs
109720040607     c                   kfld                    DB0nsp
109730040607     c                   kfld                    AR8tmd
109740040607     c                   kfld                    AR8prr
109750070514      * - FNBLT01L / FNART01L
109760070514     c     KLDVnsc       Klist
109770070514     c                   Kfld                    DB0aas
109780070514     c                   Kfld                    DB0lnp
109790070514     c                   Kfld                    DB0nrs
109800070514     c                   Kfld                    DB0nsp
109810070514     c                   Kfld                    Wnsc
109820070319      * - FIARN11L
109830070319     c     K06ARN11      klist
109840070319     c                   kfld                    DB0aas
109850070319     c                   kfld                    DB0lnp
109860070319     c                   kfld                    DB0nrs
109870070319     c                   kfld                    DB0nsp
109880070319     c                   kfld                    ARNcdn
109890070319     c                   kfld                    ARNgst
109900090528      * - FNBLP01L/ / FNARB01L / FIAR901L di bolla MAMMA
109910070514     c     KLBL          klist
109920070514     c                   kfld                    LBLaap
109930070514     c                   kfld                    LBLlpp
109940070514     c                   kfld                    LBLnrp
109950070514     c                   kfld                    LBLnsp
109960070514      * - FNBLT01L / FNART01L di bolla MAMMA
109970070514     c     KLBLnsc       Klist
109980070514     c                   kfld                    LBLaap
109990070514     c                   kfld                    LBLlpp
110000070514     c                   kfld                    LBLnrp
110010070514     c                   kfld                    LBLnsp
110020070514     c                   Kfld                    Wnsc
110030070514      * - FIARS01L
110040070514     c     K05ARS01P     klist
110050070514     c                   kfld                    BLTfls
110060070514     c                   kfld                    BLTlna
110070070514     c                   kfld                    BLTnrs
110080070514     c                   kfld                    BLTnsc
110090070514     c                   kfld                    ARStrc
110100070514     c     K05ARS01A     klist
110110070514     c                   kfld                    ARTfls
110120070514     c                   kfld                    ARTlna
110130070514     c                   kfld                    ARTnrs
110140070514     c                   kfld                    ARTnsc
110150070514     c                   kfld                    ARStrc
110160060505      * - FNORM01L
110170060505     c     K04ORM01      klist
110180071015     c                   kfld                    ORMpoe
110190071015     c                   kfld                    ORMnsr
110200071015     c                   kfld                    ORMnor
110210071015     c                   kfld                    ORMnrv
110220030515      * - TABEL00F
110230030515     c     K03TAB        klist
110240040223     c                   kfld                    TBLkut
110250030515     c                   kfld                    TBLcod
110260030515     c                   kfld                    TBLkey
110270030515     c     K02TAB        klist
110280040223     c                   kfld                    TBLkut
110290030515     c                   kfld                    TBLcod
110300030515      * - FLNUF01L
110310030515     c     K04NUF        klist
110320030515     c                   kfld                    NUFaaa
110330030515     c                   kfld                    NUFcnu
110340030515     c                   kfld                    NUFfil
110350030515     c                   kfld                    NUFfl2
110360030515      *
110370030515     c                   ENDSR
110380030515
110390030515** > $Msg ---------------------------------------------------------------------*
110400030515IMPOSSIBILE CONTINUARE LA RISTAMPA BOLLE
110410071112PERCHE' STAMPATE PIU' DI &NrLdV BOLLE !!!
110420130307ERRORE IN REPERIMENTO NUMERATORE PER STAMPA PDF ALLEGATO ALLA LDV:
110430130307   NON REPERITO IL NUMERATORE
110440130307   NUMERATORE ALLOCATO
110450130307   SUPERATO IL LIMITE MASSIMO PER IL NUMERATORE
110460130312RILEVATO ERRORE IN FASE DI STAMPA DEI DOCUMENTI ALLEGATI
110470140307** > $GgS/$Gio *
1104800306241LUNEDI
1104900306242MARTEDI
1105000410263MERCOLEDI
1105100306244GIOVEDI
1105200306245VENERDI
1105300305156SABATO
1105400305157DOMENICA
110550030515M          MATT.
110560030515P          POM.
