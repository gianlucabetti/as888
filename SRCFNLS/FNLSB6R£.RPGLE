000100030515      *---------------------------------------------------------------*
000110060524      *?  STAMPA   LETTERE   DI   VETTURA   SU   STAMPANTE   LASER   ?*
000120040607      *---------------------------------------------------------------*
000130090304      *                                                               *
000140110310      *? A QUESTO PROGRAMMA È "LEGATA"? LA STORED-PROCEDURE FISP30R:?*
000150110310      *? VERIFICARE SE LA MODIFICA E' DA APPORTARE ANCHE AD ESSA !!! ?*
000160030515      *                                                               *
000170060524      *NOTA BENE:?> Codice cliente 0501331 => FedEx                  *
000180081127      *              codice fissato nella costante "c_CliFedEx"       *
000190150429      *            > Codice cliente 0506514 => FedEx (2)              *
000200150429      *              codice fissato nella costante "c_CliFedEx2"      *
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
000690140303     fFNEVB22L  if   e           k disk    usropn
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
000830170517     fTFNTC01L  if   e           k disk    usropn
000840110411       // -?O.R.M.?
000850030903     fFNORM01L  if   e           k disk    usropn
000860140410     fFNORE01L  if   e           k disk    usropn
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
001120150226     f*//                                  rename(LSB6AR5B:A4prtAR5B)
001130150226     f*//                                  rename(LSB6AR5C:A4prtAR5C)
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
002640140915     d c_PosLin1OC3    c                   const(1.470)
002650080704     d c_PosLin1OD1    c                   const(1.010)
002660140917
002670140917       // -?Posizioni di altre costanti?
002680140917     d*// c_PosKBRTc      c                   const(0.970)     x FONT(00404)
002690140922     d c_PosKBRTc      c                   const(0.965)
002700140922     d*// c_PosKBRTc      c                   const(0.955)     x FONT(02305)
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
003540150429     d c_CliFedEx2     c                   const(0506514)
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
003760140129
003770140129       // -?Costanti per conversione caratteri maiuscoli/minuscoli?
003780140129     d c_Mai           c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
003790140129     d c_Min           c                   const('abcdefghijklmnopqrstuvwxyz')
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
004590140410
004600140410       // -?Dati per trc "O " del file FNORE (orari apertura)?
004610140410     d dOREorari     e ds
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
004770150226     d*// dTBA          e ds                  inz
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
005240170517     d dCLI          e ds                  inz
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
005830140303     d Elem_CP         ds                  inz
005840140303     d   WCPdcm                       8s 0 inz
005850140303     d   WCPocm                       4s 0 inz
005860140303     d   WCPncl                       5s 0 inz
005870140303     d   WCPfir                            inz  like(EVB2dati)
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
006550131205     d wEVBdev         s                   like(EVBdev)   inz
006560131205     d wEVBhev         s                   like(EVBhev)   inz
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
006790140922     d Pdsp            s              8  0 inz
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
007270150226     d*// Wtba            s                   like(§AR5tba)  inz
007280080708
007290110427       // -?Comando da eseguire per QCMDEXC?
007300080708     d Qcmd            s            512    inz
007310080708
007320090302       //--------------------------------------------------------------
007330130306       //?Definizione prototipi procedure usate.                       ?
007340090302       //--------------------------------------------------------------
007350140915
007360140915       // -?Calcolo/Controllo BRTcode?
007370140915      /copy gaitrasrc/srcProtoPI,TRUL28R0
007380140915      /copy gaitrasrc/srcProtoPR,TRUL28R0
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
007810140303
007820140303       // -?File FNEVB22L?
007830140303     d keyFNEVB22    e ds                  extname(FNEVB22L : *key)
007840140303     d                                     prefix(k_)   inz
007850140410
007860140410       // -?File FNORE01L?
007870140410     d keyFNORE01    e ds                  extname(FNORE01L : *key)
007880140410     d                                     prefix(k_)
007890140410     d                                     inz
007900170517
007910170517       // -?File TFNTC01L?
007920170517     d keyTFNTC01    e ds                  extname(TFNTC01L : *key)
007930170517     d                                     prefix(k_)
007940170517     d                                     inz
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
008540140919      *§BOX1BJ = posiz. linea orizzontale fine   box BRTcode        ?*
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
009980130306     c                   parm                    FNLSB6ds1
009990110427
010000110427       // -?Tipo lancio (DB0tla):?"C" -           Chiudo con LR?
010010110427       //                        ?"L" - Elaboro e chiudo con LR?
010020110427       //                        ?" " - Elaboro e chiudo in RETRN?
010030080703if  1c                   if        DB0tla = 'C'
010040030515     c                   goto      Fine
010050060921e   1c                   endif
010060110427
010070110427       // -?Operazioni iniziali?
010080070123     c                   exsr      sr_Start
010090110427
010100030515sel 1c                   SELECT
010110130306
010120130306       // -?Richiesta la stampa (finale) delle sole immagini PDF?
010130130306     c                   WHEN      %parms() > 1  and  DB6pdf = 'S'
010140130307     c*//                                        and  DB0fa4 = 'I'
010150130306     c                   if        DB6num > *zero
010160130306     c                   exsr      sr_PrtPdf
010170130306     c                   endif
010180130306     c                   exsr      sr_End
010190110427
010200110427       // -?NON passati i dati LDV => caricamento?
010210030515w   1c                   WHEN      DB0ges = *blanks
010220071008if  2c                   if             DB0tbo  = 'P'
010230071008     c                             or   DB0tbo  = '8'
010240030515     c                   exsr      CarBLP
010250030515x   2c                   else
010260030515     c                   exsr      CarARB
010270030515e   2c                   endif
010280110427
010290110427       // -?Passati i dati LDV => è già (quasi) tutto pronto?
010300110427       //  ?(solo dalla sede - vedi TNSD50R)?
010310081114w   1c                   WHEN      DB0ges = 'B'
010320090513     c                   exsr      CarTAB
010330081114     c*** (già fatto:)   clear                   dsBL4A
010340081114     c                   if        DB0fbr = 'S'
010350081114     c                   exsr      RepAR4_A
010360081114     c***                if        §B4Abm = 'S' or §B4Abm = 'C' or
010370081114     c***                          §B4Abm = 'P' or §B4Abm = 'Q'
010380081114     c***                movel     'S'           DB0bam
010390081114     c***                endif
010400081114     c                   endif
010410081114     c                   eval      wB0cfi = DB0cpi
010420081114     c                   eval      wB0piv = DB0cpi
010430110427
010440110427       // -?DB0GES = "R" => Stampa una LDV vuota con un messaggio?
010450110427       //  ?per avvisare che è impossibile continuare la ristampa?
010460110427       //  ?perchè stampate più di 6.000 bolle !?
010470030515w   1c                   WHEN      DB0ges = 'R'
010480071112     c                   exsr      Rep_TabVPO
010490071112     c                   eval      Pmsg1  = $Msg(1)
010500071112     c                   eval      Pmsg2  = %replace( %editc(
010510071112     c                                         §VPOmaxldv : 'J' ) :
010520071112     c                                         $Msg(2) : %scan(
010530071112     c                                         '&NrLdV' : $Msg(2) ))
010540130418     c                   eval      *in33 = *off
010550130418     c                   exsr      Posiz_Box
010560030515     c                   write     A4prtMSG
010570030515     c                   goto      Fine
010580110427
010590030515e   1c                   ENDSL
010600110427
010610110427       // -?Controllo del tipo LDV da stampare?
010620040607     c                   exsr      TipoLDV
010630080919      /free
010640080919
010650110427       //?»»»?Verifiche per chiusura pgm?«««?
010660080919       SELECT;
010670080919
010680110427         // -?Chiude SE richiamato per la sola verifica del modulo?
010690110427         //  ?(dal pgm FNLSE6R1 - cl FNLSE6C1)?
010700080919         WHEN  DB0ris  = 'R'   and
010710080919               DB0fl9  = *on   and
010720080919               DUTlpo  = *blanks;
010730080919           exsr  sr_End;
010740080919
010750110427         // -?Chiude SE richiesta la stampa in formato NON corrispondente?
010760080926
010770110427         // ·?Richiesta stampa di A4, mentre risulta di ex-A5?
010780110427         //  ?NON GESTITO:?rimane il richiamo "standard" dei pgm. di?
010790110427         //  ?manutenzione => 1 A4 x ogni LdV (anche se ex-A5)?
010800080919         //WHEN  DB0fa4  = '1'   and
010810080919         //      *in69   = *on;
010820080919         //  DB0tms = 'E';
010830080919         //  exsr  sr_End;
010840080919
010850110427         // -?Richiesta stampa di ex-A5, mentre risulta di A4?
010860080919         WHEN  DB0fa4  = '2'   and
010870080919               *in69   = *off;
010880080919           DB0tms = 'D';
010890080919           exsr  sr_End;
010900080919
010910080919       ENDSL;
010920080919
010930080919      /end-free
010940110427
010950110427       // -?Imposta i campi di stampa con quelli della DS?
010960030515     c                   exsr      PrepPRT
010970110427
010980110427       // -?Operazioni "inutili"?SE?richiesta la stampa della sola?
010990110427       //  ?packing-list:?
011000080919     c                   IF        DB0fa4 <> 'P'
011010110427
011020110427       // -?Decodifica i dati della LDV?
011030040607     c                   exsr      DecodLDV
011040110427
011050080919     c                   ENDIF
011060110427
011070110427       // -?Stampa della Lettera di Vettura?
011080030515sel 1c                   SELECT
011090110427       // -?Se RIMESSO MITTENTE stampo il numero di copie richiesto?
011100110427       //  ?(non per DPD e FedEx)?
011110110427       //  ?(le stampo qui perché devo stampare il riferimento numerico?
011120110427       //  ?aggiornato di volta in volta su FLNUF)?
011130030515w   1c                   WHEN      DB0ris = 'M'  and  *in70
011140030515     c                   clear                   wCOPIE
011150030515     c                   clear                   NUFaaa
011160030515     c                   eval      NUFcnu = 20
011170030515     c                   eval      NUFfil = DB0lnp
011180030515     c                   clear                   NUFfl2
011190030515do  2c                   DO        Wctr
011200030515     c     K04NUF        chain     FLNUF01L
011210030515if  3c                   if        %found(FLNUF01L)
011220030515     c                   add       1             NUFnum
011230030515     c                   UPDATE    FLNUF
011240030515     c                   eval      Prmn = NUFnum
011250040607     c                   exsr      StampaLDV
011260030515e   3c                   endif
011270030515e   2c                   ENDDO
011280110427       // -?Se FATTURA PREPAGATA stampo due copie?
011290110427       //  ?(solo se non attivata procedura O.R.M.)?
011300030515w   1c                   WHEN      DB0tbo = '8'  and  §OGorm <> 'S'
011310030515     c                   eval      wCOPIE = 2
011320110427       // -?Altrimenti stampo solo una copia?
011330030515x   1c                   OTHER
011340030515     c                   eval      wCOPIE = 1
011350030515e   1c                   ENDSL
011360110427
011370030515do  1c                   do        wCOPIE
011380080918      /free
011390080918         if  DB0fa4 = 'P';
011400130315           exsr  StampaPkgLst;
011410080918         else;
011420110427           exsr  StampaLDV;
011430080918         endif;
011440080918      /end-free
011450030515e   1c                   enddo
011460130306
011470130306       // -?Eventuale stampa immagini PDF?
011480130306       //  ?(si spera della sola spedizione di cui si è appena?
011490130306       //   ?stampata la LdV)?
011500130306     c                   if        %parms() > 1  and  DB6pdf = 'X'
011510130306     c                                           and  DB6num > *zero
011520130306     c                   exsr      sr_PrtPDF
011530130306     c                   endif
011540110427
011550110427       // -?Operazioni finali?
011560110427     c     Fine          tag
011570080624     c                   exsr      sr_End
011580110427
011590030515      *---------------------------------------------------------------*
011600050217      *?OPERAZIONI INIZIALI                                          ?*
011610030515      *---------------------------------------------------------------*
011620030515     c     *InzSR        BEGSR
011630131023      *
011640131023      * Reperimento dati utente
011650131023      * Reperisco nome azienda per stampa PREPAGATI (e non solo)
011660131023     c     *dtaara       define    §azute        AZUTEds
011670131023     c     *dtaara       define    §datiute      dDATIUTE
011680131023     c                   in(E)     *dtaara
011690131023     c                   if        %ERROR or RSUT = *blanks
011700131023     c                   clear                   Tibs34Ds
011710131023     c                   call      'TIBS34R'
011720131023     c                   parm                    Tibs34Ds
011730131023     c                   in        *dtaara
011740131023     c                   endif
011750131023      * Impostazione dati in KPJBA
011760131023     c                   eval      KNMUS  = SDSusr
011770110429      *
011780131023      *USCITA? SE  richiamato per la sola chiusura
011790130314     c*//                if        %subst(KPJBU:1:1) = 'C'
011800130314     c                   if        DB0tla = 'C'
011810110429     c                   leavesr
011820110429     c                   endif
011830070424      *
011840070424      * Reperisco la data del giorno
011850070424     c                   time                    w0140
011860070424     c                   move      w0140         w0080
011870070424     c     *eur          movel     w0080         Data_EUR
011880070424     c     *iso          movel     Data_EUR      w0080
011890070424     c                   move      w0080         wDateA
011900071105     c                   eval      wDate  = w0080
011910030515      *
011920030515      * Imposto i campi chiave "fissi"
011930040223     c                   eval      TBLkut = 1
011940060505     c                   eval      ARStrc = 'C'
011950070319     c                   eval      ARNcdn = 'LDV'
011960070319     c                   eval      ARNgst = 'S'
011970030515      *
011980030515      * Carico le varie esenti IVA
011990030515     c                   movel     'CC'          TBLcod
012000030515     c                   movel(p)  'VARIE'       TBLkey
012010030515     c                   clear                   XX
012020030515     c     K03TAB        setll     TABEL
012030030515     c     K02TAB        reade     TABEL
012040030515      *
012050070306do  1c                   DOW       NOT %eof(TABEL00F)
012060030515      * Deve essere NON annullata e con codice che inizia con "VARIE"
012070030515if  2c                   If        %subst(TBLkey:1:5) = 'VARIE'
012080030515     c                             and    TBLflg      = *blanks
012090030515     c                   movel     TBLuni        dsCC
012100030515if  3c                   if        §CCcei <> *blanks
012110030515     c                   add       1             xx
012120030515     c                   move      TBLkey        $Var(xx)
012130030515e   3c                   endif
012140030515e   2c                   EndIf
012150030515      *
012160030515     c     K02TAB        reade     TABEL
012170030515e   1c                   ENDDO
012180070306      *
012190070308      * Carico le causali eventi con tipo consegna = "G"
012200070306     c                   movel     '2A'          TBLcod
012210070306     c                   clear                   TBLkey
012220070306     c                   clear                   XX
012230070306     c     K03TAB        setll     TABEL
012240070306     c     K02TAB        reade     TABEL
012250070306      *
012260070306do  1c                   DOW       NOT %eof(TABEL00F)
012270070308      * Deve essere NON annullata e con flag §2AFTC = "G"
012280070308if  2c                   If        TBLflg = *blanks
012290070306     c                   movel     TBLuni        ds2A
012300070308if  3c                   if        §2Aftc = 'G'
012310070306     c                   add       1             xx
012320070308     c                   movel     TBLkey        $2Acev_G(xx)
012330070306e   3c                   endif
012340070306e   2c                   EndIf
012350070306      *
012360070306     c     K02TAB        reade     TABEL
012370070306e   1c                   ENDDO
012380030515      *
012390030515      * Carico tabella VARIABILI ARRIVI
012400030515     c                   clear                   ds$2
012410030515     c                   eval      TBLcod = '$2'
012420030515     c                   eval      TBLkey = '1'
012430030515     c     K03TAB        chain     TABEL
012440030515if  1c                   if        %found(TABEL00F)
012450030515     c                   movel     TBLuni        ds$2
012460030515e   1c                   endif
012470030515      *
012480030515      * Reperisco la moneta di conto
012490030515     c                   clear                   dGED
012500040607     c                   clear                   TIBS02ds
012510040607     c                   eval      T02mod = 'C'
012520040607     c                   eval      T02cod = 'GED'
012530040607     c                   eval      T02ke1 = '1         '
012540030515     c                   call      'TIBS02R'
012550030515     c                   parm                    KPJBA
012560040607     c                   parm                    TIBS02ds
012570030515if  1c                   if        T02err = *blanks
012580030515     c                   movel     T02uni        dGED
012590030515e   1c                   endif
012600030515      *
012610030515      * Con moneta di conto:
012620060301      *   Carico tabella GESTIONALE - IMPORTI VARI   e
012630030515      *   Reperisco l'importo limite per abilitazione c/a
012640030515     c                   clear                   dGEI
012650040607     c                   clear                   TIBS02ds
012660040607     c                   eval      T02mod = 'C'
012670040607     c                   eval      T02cod = 'GEI'
012680040607     c                   eval      T02ke1 = §GEDcn
012690030515     c                   call      'TIBS02R'
012700030515     c                   parm                    KPJBA
012710040607     c                   parm                    TIBS02ds
012720030515if  1c                   if        T02err = *blanks
012730030515     c                   movel     T02uni        dGEI
012740030515e   1c                   endif
012750061031      *
012760110426      * Reperisco data di decorrenza stampa "Per ricevuta di ...
012770071005      *                                      senza riserve"
012780090615      *              e di decorrenza stampa per C.L.N.
012790061031     c                   clear                   dVPOdeco
012800061031     c                   clear                   TIBS02ds
012810061031     c                   eval      T02mod = 'C'
012820071112     c***                eval      T02sif  = KNSIF
012830071112     c                   eval      T02cod = 'VPO'
012840061031     c                   eval      T02ke1 = 'DECO      '
012850061031     c                   call      'TIBS02R'
012860061031     c                   parm                    KPJBA
012870061031     c                   parm                    TIBS02ds
012880061031if  1c                   if        T02err = *blanks
012890061031     c                   movel     T02uni        dVPOdeco
012900061031e   1c                   endif
012910120530      *
012920120530      *//*?Reperisco data di decorrenza stampa nuovo logo BRT?
012930120530     c*//                clear                   dVPObrt
012940120530     c*//                clear                   TIBS02ds
012950120530     c*//                eval      T02mod = 'C'
012960120530     c***                eval      T02sif  = KNSIF
012970120530     c*//                eval      T02cod = 'VPO'
012980120530     c*//                eval      T02ke1 = 'BRT       '
012990120530     c*//                call      'TIBS02R'
013000120530     c*//                parm                    KPJBA
013010120530     c*//                parm                    TIBS02ds
013020120530if  1c*//                if        T02err = *blanks
013030120530     c*//                movel     T02uni        dVPObrt
013040120530e   1c*//                endif
013050080708      /free
013060110426         // -?Override al file di stampa?
013070081017         if  DB0tla <> 'C';
013080080709           fnlsB8r ( fnlsB5ds );
013090080708         endif;
013100080708      /end-free
013110030515      *
013120030515      * Apro i file di stampa
013130130710     c                   open      FNLSB6P
013140080918     c                   clear                   CurLineA4
013150130306      /free
013160130306         // -?Reperimento override apportate al file di stampa?
013170130306         //  ?per comunicarle per la stampa immagini PDF?
013180130306         if  DB0tla <> 'C';
013190130306           wPrtF = 'FNLSB6P';
013200130306           clear  trulPOVRds;
013210130307           trulPOVRc ( wPrtF : trulPOVRds);
013220130307           if trulPOVRds.§ULPoerr <> *blank;  //?Nessuna override rilevata?
013230130321             //trulPOVRds.§ULPoOutQ  = '*JOB      ';
013240130321             trulPOVRds.§ULPoOutQ  = 'PRT01     ';
013250130307             trulPOVRds.§ULPoFType = '*LDVUNI   ';
013260130307             trulPOVRds.§ULPoUDta  = 'LDV_UNI   ';
013270130306           endif;
013280130306         endif;
013290130306      /end-free
013300030515      *
013310030515     c                   ENDSR
013320071112      *
013330071112      *---------------------------------------------------------------*
013340071112      *?REPERIMENTO N° MAX DI LDV STAMPABILI DA TAB. "VPO"           ?*
013350071112      *---------------------------------------------------------------*
013360071112     c     Rep_TabVPO    BEGSR
013370071112      *
013380071112      * Aggancio tabella "VPO" per reperimento del n° LdV stampabili
013390071112      *   limite (per congelamento e blocco stampa)
013400071112     c                   clear                   dVPO
013410071112     c                   clear                   TIBS02ds
013420071112     c                   eval      T02mod  = 'C'
013430071112     c***                eval      T02sif  = KNSIF
013440071112     c                   eval      T02cod  = 'VPO'
013450071112     c                   movel(P)  'VPO'         T02ke1
013460071112     c                   call      'TIBS02R'
013470071112     c                   parm                    KPJBA
013480071112     c                   parm                    TIBS02ds
013490071112     c                   if        T02err  = *blanks
013500071112     c                   movel     T02uni        dVPO
013510071203     c                   if        §VPOMaxLdv = *zeros
013520071203     c                   eval      §VPOMaxLdv = 10000
013530071203     c                   endif
013540071112     c                   else
013550071112     c                   eval      §VPOMaxLdv = 10000
013560071112     c                   endif
013570071112      *
013580071112     c                   ENDSR
013590070123      *
013600070123      *---------------------------------------------------------------*
013610070306      *?ALTRE OPERAZIONI INIZIALI (NON PIÙ IN *INZSR)                ?*
013620070123      *---------------------------------------------------------------*
013630070123     c     sr_Start      BEGSR
013640070123      *
013650080709      /free
013660081017         select;
013670081017           when  DB0fa4 = *blank;
013680081017             DB0fa4 = '1';
013690081020           when  DB0fa4 = 'P';
013700081017             DB0fl6 = *on;
013710081017         endsl;
013720080709      /end-free
013730080709      *
013740070123if  1c                   if        DB0fl7 = '1'
013750130710     c                   close     FNLSB6P
013760130710     c                   open      FNLSB6P
013770081015     c                   clear                   CurLineA4
013780070123     c                   clear                   Page4
013790070123e   1c                   endif
013800070123      *
013810070123      * »»» CHIODO: SE richiamato per la sola verifica del modulo «««
013820070308      *     RIPULISCO il "livello della filiale" (S/1/2) perchè:
013830070123      *     1) sarò partito dal S.I. di sede, ma
013840070123      *     2) sarà stata cambiata la lista di librerie come x S.I. di
013850070308      *        filiale (dal CL FNLSE6C1)
013860070123if  1c                   if             DB0ris  = 'R'
013870070123     c                             and  DB0fl9  = *on
013880070123     c                             and  DUTlpo <> *blanks
013890070123     c                   clear                   DUTlpo
013900070123e   1c                   endif
013910070123      *
013920070308      * Apertura archivi di sede o filiale - comunque utilizzati
013930070123if  1c                   if        DUTlpo = 'S'
013940070123if  2c                   if        NOT  %open(FIAR531C)
013950070123     c                   open      FIAR531C
013960070123e   2c                   endif
013970070123x   1c                   else
013980070123if  2c                   if        NOT  %open(FIAR401L)
013990070123     c                   open      FIAR401L
014000070123e   2c                   endif
014010070123if  2c                   if        NOT  %open(FIAR501L)
014020070123     c                   open      FIAR501L
014030070123e   2c                   endif
014040070123if  2c                   if        NOT  %open(FLNUF01L)
014050070123     c                   open      FLNUF01L
014060070123e   2c                   endif
014070070123e   1c                   endif
014080110427      *
014090110427      * Pulizia del PRTF
014100110427     c                   exsr      ClrPRTF
014110070123      *
014120070123     c                   clear                   wCOPIE
014130070123      *
014140070123     c                   ENDSR
014150080624      *
014160080624      *---------------------------------------------------------------*
014170080624      *?OPERAZIONI FINALI                                            ?*
014180080624      *---------------------------------------------------------------*
014190080624     c     sr_End        BEGSR
014200080624      *
014210080624      * Chiusura pgm
014220080919if  1c                   IF        DB0tla <> *blanks
014230130710     c                   close     FNLSB6P
014240080708      /free
014250080708       Qcmd = C_CmdDltOvr;
014260080708       callp(e) qCmdExc(Qcmd : %size(Qcmd));
014270080708      /end-free
014280080624     c                   clear                   TIBS02ds
014290080624     c                   eval      T02tla = 'C'
014300080624     c                   call      'TIBS02R'
014310080624     c                   parm                    KPJBA
014320080624     c                   parm                    TIBS02ds
014330080624     c                   reset                   FNLV80ds
014340080624     c                   call      'FNLV80R'
014350080624     c                   parm                    KPJBA
014360080624     c                   parm                    FNLV80ds
014370080624     c                   clear                   FIDN12ds
014380080624     c                   eval      I12tla = 'C'
014390080624     c                   call      'FIDN12R'
014400080624     c                   parm                    FIDN12ds
014410090605      /free
014420090622           if  §VPOrcm <= wDate   and   DUTlpo  <> 'S';
014430090615             reset  TNSD99ds;
014440090615             tnsd99r ( TNSD99ds );
014450090615           endif;
014460100322           if  §VPOcas <= wDate   and   DUTlpo  <> 'S';
014470100208             clear  trulINTds;
014480100208             iINTtla = 'C';
014490100208             trulINTr ( TRULINTds );
014500100322           endif;
014510090605      /end-free
014520080624     c                   eval      *inLR  = *on
014530080624e   1c                   ENDIF
014540080919      *
014550080919      * Ritorno al chiamante
014560080919     c                   return
014570080624      *
014580080624     c                   ENDSR
014590030515      *
014600030515      *---------------------------------------------------------------*
014610050217      *?Pulisco i campi di stampa                                    ?*
014620030515      *---------------------------------------------------------------*
014630030515     c     ClrPRTF       BEGSR
014640030515      *
014650130418     c                   clear                   A4prtBOX
014660120530     c*//                clear                   A4LogoBAR
014670120530     c*//                clear                   A4LogoDA1
014680120530     c*//                clear                   A4LogoBR2
014690120530     c*//                clear                   A4LogoDA2
014700120530     c                   clear                   A4LogoBRT
014710120530     c                   clear                   A4LogoDA3
014720130418     c                   clear                   A4prtLDV
014730080604     c                   clear                   A4prtLDVB
014740080922     c                   clear                   A4prtDPD
014750030515     c                   clear                   A4prtFED1
014760030515     c                   clear                   A4prtFED2
014770030515     c                   clear                   A4prtFED3
014780030515     c                   clear                   A4prtAR51
014790030515     c                   clear                   A4prtAR52
014800030515     c                   clear                   A4prtAR53
014810030909     c                   clear                   A4prtAR5A
014820040607     c*** stessi campi:  clear                   A4prtAR5B
014830040607     c*** stessi campi:  clear                   A4prtAR5C
014840070321     c*** stessi campi:  clear                   A4prtAR5G
014850040311     c*** no campi:      clear                   A4prtSP0
014860040607     c*** no campi:      clear                   A4prtSP1
014870080709     c*** no campi:      clear                   A4prtSP2
014880040311     c*** no campi:      clear                   A4prtNUL
014890030911     c                   clear                   A4prtGEN
014900040607     c                   clear                   A4prtDK0
014910040607     c                   clear                   A4prtDK1
014920040607     c                   clear                   A4prtDK2
014930040607     c                   clear                   A4prtDK3
014940040607     c                   clear                   A4prtDK4
014950040607     c                   clear                   A4prtDK5
014960081222     c                   clear                   A4prtDLL
014970041019     c                   clear                   A4prtDL0
014980050318     c                   clear                   A4prtDL0A
014990050318     c                   clear                   A4prtDL0B
015000050318     c                   clear                   A4prtDL0C
015010050606     c                   clear                   A4prtDL0D
015020050606     c                   clear                   A4prtDL0E
015030050606     c                   clear                   A4prtDL0F
015040041028     c                   clear                   A4prtDL1A
015050041028     c                   clear                   A4prtDL1B
015060050318     c                   clear                   A4prtDL1C
015070050606     c                   clear                   A4prtDL1D
015080050606     c                   clear                   A4prtDL1E
015090041019     c                   clear                   A4prtDL2
015100060614     c                   clear                   A4prtDL21
015110041019     c                   clear                   A4prtDL3
015120041019     c                   clear                   A4prtDL4
015130050318     c                   clear                   A4prtDL5
015140050606     c                   clear                   A4prtDL6
015150050606     c                   clear                   A4prtDL7
015160050606     c                   clear                   A4prtDL8
015170050606     c                   clear                   A4prtDL9
015180050114     c                   clear                   A4prtAC0
015190050114     c                   clear                   A4prtAC1A
015200050114     c                   clear                   A4prtAC1B
015210050114     c                   clear                   A4prtAC2
015220050114     c                   clear                   A4prtAC3
015230050114     c                   clear                   A4prtAC4
015240050214     c                   clear                   A4prtES0
015250050214     c                   clear                   A4prtES0A
015260050214     c                   clear                   A4prtES0B
015270050215     c                   clear                   A4prtES1A
015280050215     c                   clear                   A4prtES1B
015290050214     c                   clear                   A4prtES2
015300050214     c                   clear                   A4prtES3
015310050215     c                   clear                   A4prtES4
015320050214     c                   clear                   A4prtDB0
015330041207     c                   clear                   A4prtDT0
015340040607     c                   clear                   A4prtDT1
015350040607     c*** stessi campi:  clear                   A4prtDT2
015360040607     c*** stessi campi:  clear                   A4prtDC1
015370030515     c                   clear                   A4prtSEG
015380030515     c                   clear                   A4prtMSG
015390030515      *
015400041207     c                   reset                   dAR8
015410060524     c                   clear                   dsBL4A
015420061030     c                   clear                   dsBL4I
015430061108     c                   clear                   dTA4Q
015440100514     c                   clear                   dOrm01
015450041207      *
015460070514     c                   reset                   $Mamma
015470130215     c                   reset                   $Mamma_FedEx
015480070306     c                   reset                   $FIARN
015490060921     c                   reset                   $FIAR5
015500030515     c                   reset                   $First
015510030515     c                   reset                   $FirstSCR
015520060921     c                   reset                   $FIAR8
015530130306     c                   reset                   $§PDF
015540040608     c                   reset                   $DDT_o
015550040608     c                   reset                   $DDT_p
015560040608     c                   reset                   $DDTlastPg
015570030515     c                   reset                   $Partic
015580030710     c                   reset                   $NoDesPartic
015590030910     c                   reset                   $BolloVirt
015600061106     c                   reset                   $No_CF
015610071114     c                   reset                   $No_PI
015620040707     c                   reset                   $No_x_Cons
015630101125     c                   reset                   $Rit_A_Inc
015640050513     c                   reset                   $A4_x_ScNS
015650070306     c                   reset                   $LascAvv
015660070319     c                   reset                   $ConsParz
015670070315     c                   reset                   $C_A_
015680030515     c                   reset                   $CAeur
015690090302     c                   reset                   $TabPXA
015700110411     c                   reset                   $TabPXA2
015710090924     c                   reset                   $BoxNxD
015720100513     c                   reset                   $PreavvORM
015730110427     c                   reset                   $GeoAtt
015740070717     c                   clear                   wGeoTel
015750070717     c                   clear                   wGeoCon
015760070717     c                   clear                   Wdsp
015770090527     c                   clear                   WddtCLN
015780090527     c                   clear                   WdspCLN
015790030717     c                   clear                   W_CoOr
015800040608     c                   clear                   wOmaRowT
015810040608     c                   clear                   wOmaRowD
015820050624     c                   clear                   wBolloVirt
015830030515     c                   clear                   Wcas
015840030515     c                   clear                   Wiva
015850071015     c                   clear                   Wpdc4a
015860080926     c                   clear                   wPage
015870080704     c                   reset                   $LdVxP
015880090121     c*** (da non fare:) clear                   wNumLdV
015890090121     c*** (da non fare:) clear                   wSeqLdV
015900071114     c                   clear                   wB0cfi
015910071114     c                   clear                   wB0piv
015920031015     c                   clear                   wB0nrs
015930070514     c                   clear                   wB0ndc
015940070514     c                   clear                   wB0ntc
015950070514     c                   clear                   wB0dcp
015960081119     c                   clear                   wB0tmc
015970101125     c                   clear                   wB0dcm
015980101125     c                   clear                   wB0icc
015990101125     c                   clear                   wB0ica
016000130124     c                   clear                   wB0fiv
016010131205     c                   clear                   wEVBdev
016020131205     c                   clear                   wEVBhev
016030030515      *
016040030515     c                   clear                   $NP
016050030515     c                   clear                   $SC1
016060030515     c                   clear                   $SC2
016070030905     c                   clear                   $QUM
016080040607     c                   clear                   $AR8o1
016090070319     c                   reset                   $CP
016100070319     c                   reset                   $CPdcm
016110030515      *
016120030515     c                   movea     *zeros        *in(01)
016130030515      *
016140030515     c                   ENDSR
016150030515      *
016160030515      *---------------------------------------------------------------*
016170050217      *?Determino il modulo LDV da usare per la stampa               ?*
016180120530      *?(se A5 o A4 e se DPD, FedEx o BRT)                           ?*
016190030515      *---------------------------------------------------------------*
016200040607     c     TipoLDV       BEGSR
016210070212      *
016220080703      * Impostazione ds "Flag operativi DB0FLO di FNLSB5DS"
016230070215     c                   movel     DB0flo        dDB0flo
016240070215      *
016250070212      * Impostazione della data spedizione
016260070212      * (se non già impostata in subr. "CarBLP" o "CarARB")
016270070212if  1c                   if        Wdsp   =  *zeros
016280070212if  2c                   if        DB0mgs <> *zeros
016290070212     c                   eval      Wdsp   =  (DB0aas * 10000) + DB0mgs
016300070212x   2c                   else
016310070212      *   prendo la data del giorno se non c'è la data spedizione
016320070212     c                   time                    w0140
016330070212     c                   move      w0140         w0080
016340070212     c     *eur          movel     w0080         Data_EUR
016350070212     c     *iso          movel     Data_EUR      Wdsp
016360070212e   2c                   endif
016370070212e   1c                   endif
016380040311      *
016390040311      * Dati della linea di partenza
016400040311if  1c                   if        DB0lnp <> SAVlnp
016410040311     c                   eval      SAVlnp =  DB0lnp
016420040311     c     DB0lnp        chain     AZORG
016430040311if  2c                   if        %found(AZORG01L)
016440040311     c                   movel     ORGde3        OG143
016450040311     c                   movel     ORGde8        OG148
016460040311x   2c                   else
016470040311     c                   clear                   OG143
016480040311     c                   clear                   OG148
016490040311e   2c                   endif
016500040311e   1c                   endif
016510040607      *
016520070308      * Dati della filiale del mittente
016530040607     c                   eval      WX     =  DB0ccm / 10000
016540040607if  1c                   if        WX     <> ORGfil
016550040607     c     WX            chain     AZORG
016560040607e   1c                   endif
016570040607if  1c                   if        %found(AZORG01L)
016580040607     c                   movel     ORGde3        OG143ccm
016590040607x   1c                   else
016600040607     c                   clear                   OG143ccm
016610040607e   1c                   endif
016620040311      *
016630040311      * Dati della linea di arrivo
016640040311if  1c                   if        DB0lna <> SAVlna
016650040311     c     DB0lna        chain     AZORG
016660040311if  2c                   if        %found(AZORG01L)
016670070312     c                   movel     ORGde6        OG146arr
016680061013     c                   movel     ORGde8        OG148arr
016690070312     c                   movel     ORGdf0        OG150arr
016700040311x   2c                   else
016710070312     c                   clear                   OG146arr
016720061013     c                   clear                   OG148arr
016730070312     c                   clear                   OG150arr
016740040311e   2c                   endif
016750050510e   1c                   endif
016760070212      *
016770070212      * Dati della capofila della linea di arrivo
016780070306      * - per verificare TELLUS   e
016790070306      * - per non stampare la "copia per il destinatario" nel caso
016800070306      *   di precedente "Lasciato Avviso"
016810170717      * - per i dati della LINEA DI ARRIVO in stampa
016820170717      *
016830170717      * - Reperimento capofila (£6)
016840170905      *   (ad oggi, NON alla data della spedizione)
016850170717if  1c                   if        DB0lna <> D55lin
016860170717     c                   reset                   FNLV55ds
016870170717     c*** già così:      eval      D55tpt =  '6'
016880170717     c                   eval      D55lin =  DB0lna
016890170905     c*//                eval      D55drf =  Wdsp
016900170905     c                   eval      D55drf =  wDate
016910170717     c                   call      'FNLV55R'
016920170717     c                   parm                    FNLV55ds
016930170717if  2c                   if        D55err <> *blanks
016940170717     c                   eval      D55tfa =  DB0lna
016950170717e   2c                   endif
016960170717e   1c                   endif
016970170717      * - Reperimento 146ª e 148ª descrizioni dell'organigramma
016980170717      *   (flag Tellus del capofila e flag gestione telefonate GEO)
016990170718if  1c                   if        D55tfa <> ORGfil
017000170717     c     D55tfa        chain     AZORG
017010170718e   1c                   endif
017020170718if  1c                   if        %found(AZORG01L)
017030170717     c                   eval      wDB0lnaCf£6 = ORGfil
017040170717     c                   eval      wDB0dlaCf£6 = ORGdes
017050170717     c                   eval      wDB0inlCf£6 = ORGind
017060170717     c                   eval      wDB0calCf£6 = ORGcpf
017070170717     c                   eval      wDB0lolCf£6 = ORGloc
017080170717     c                   eval      wDB0prlCf£6 = ORGpro
017090170717     c                   eval      wDB0telCf£6 = ORGtel
017100170717     c*//                eval      wDB0faxCf£6 = ORGfax
017110170717     c                   eval      wDB0faxCf£6 = ORGdd2
017120170717     c                   movel     ORGde6        OG146cf£6
017130170717     c                   movel     ORGde8        OG148cf£6
017140170718x   1c                   else
017150170717     c                   clear                   wDB0lnaCf£6
017160170717     c                   clear                   wDB0dlaCf£6
017170170717     c                   clear                   wDB0inlCf£6
017180170717     c                   clear                   wDB0calCf£6
017190170717     c                   clear                   wDB0lolCf£6
017200170717     c                   clear                   wDB0prlCf£6
017210170717     c                   clear                   wDB0telCf£6
017220170717     c                   clear                   wDB0faxCf£6
017230170717     c                   clear                   OG146cf£6
017240170717     c                   clear                   OG148cf£6
017250170718e   1c                   endif
017260070717      *
017270070717      *** Imposta flag attivazione GEO x LNA
017280070717     c                   eval      wGeoCon = A§OGtlls
017290070717     c                   eval      wGeoTel = A§OGgeot
017300070717if  1c                   if             A§OGdda  > *zeros
017310070717     c                             and  A§OGdda  < C_DateMax_A
017320070717     c                             and  A§OGdda <= wDateA
017330110427     c                   eval      $GeoAtt = *on
017340070717e   1c                   endif
017350070717      *
017360070717if  1c                   IF        DB0tbo = 'A'  AND
017370070717     c                                 (wGeoCon <> 'S'
017380070717     c                              or  wGeoTel <> 'S'
017390110427     c                              or  $GeoAtt <> *on)
017400070717      *** Imposta flag attivazione GEO x capofila £6
017410070717     c                   eval      wGeoCon = £6OGtlls
017420070717     c                   eval      wGeoTel = £6OGgeot
017430071109if  2c                   if             £6OGdda  > *zeros
017440070717     c                             and  £6OGdda  < C_DateMax_A
017450070717     c                             and  £6OGdda <= wDateA
017460110427     c                   eval      $GeoAtt = *on
017470071109e   2c                   endif
017480070717      *
017490070212e   1c                   ENDIF
017500050510      *
017510050510      * Verifica se richiesta stampa in A4 in tab. "7R"
017520050510      *   (gestione particolarità consegna)
017530050510     c                   clear                   ds7R
017540170717if  1c                   if        DB0gma <> *blanks
017550070312     c                   eval      TBLcod =  '7R'
017560050510     c                   movel(p)  DB0gma        TBLkey
017570050510     c     K03TAB        chain     TABEL
017580050510if  2c                   if        %found(TABEL00F)
017590050510     c                   movel     TBLuni        ds7R
017600050510e   2c                   endif
017610050510e   1c                   endif
017620071015      *
017630071015      * Verifica se LDV con "Ritiro Porto Franco Prepagato"
017640071015     c                   clear                   dsBL4M
017650071015     c                   if        DB0tbo = '8'
017660071015     c                   exsr      RepAR4_M
017670071015     c                   endif
017680130315      *
017690130315      * Reperimento dati da FIAR5 / "GEN"
017700130315      *   (per verificare se esistono Packing-List e/o PDF allegati
017710130315      *    - oltre che per reperire Nominativo e Telefono)
017720130315     c                   exsr      RepAR5gen
017730040607      *
017740130315      * Verifica se LDV con DDT e/o PDF
017750130315     c                   exsr      CercaDDToPDF
017760061031      *
017770071114      * Verifica se richiesta stampa in A4 per richiesta Codice Fiscale
017780071114      *   e/o Partita IVA al cliente.
017790120503      *-=»>?01/08/2008:?                                          <«=-*
017800080801      *-=»> Abolite richieste di Codice Fiscale e/o Partita Iva.  <«=-*
017810080801      *-=»> Tolta ricerca di 'sti 2 dati, lasciando il defualt di <«=-*
017820080801      *-=»> NON richiesta: $No_CF = *off e $No_PI = *off.         <«=-*
017830120503      *-=»>?03/05/2012:?                                          <«=-*
017840120503      *-=»>?Ripristinate richieste di Cod. Fiscale e/o Part. Iva.?<«=-*
017850120503if  1c                   if        DUTlpo <> 'S'
017860120503     c                   exsr      CheckCFPI
017870120503e   1c                   endif
017880070306      *
017890070319      * Verifica se richiesta stampa in A4 per note in FIARN00F
017900070306      *   (da stampare nel box "Note per l'autotrasportatore")
017910070319      * » se stampa in arrivo
017920070319if  1c                   if        DB0tbo = 'A'
017930070306     c                   exsr      CheckFIARN
017940070319e   1c                   endif
017950090302      /free
017960090302
017970090302         // Verifica se richiesta stampa in A4 per note specifiche
017980090302         //   per il cliente mittente (v. tab. "PXA")
017990090302         //   (da stampare nel box "Note per l'autotrasportatore")
018000090302         exsr  CheckTabPXA;
018010090924
018020090924         // Verifica se richiesta stampa in A4 per note specifiche
018030090924         //   per il destinatario (da parte del mittente)
018040090924         //   (da stampare in un box a parte)
018050090924         if  DB0ccm = c_CliSG;
018060090924           $BoxNxD = *on;
018070090924         endif;
018080090302      /end-free
018090070319      *
018100070319      * Verifica se richiesta spedizione già parzialmente consegnata
018110070319      *   (da stampare nel box "Note per l'autotrasportatore")
018120070717      * » se stampa in arrivo e LNA - o capofila £6 - con GEO attivato
018130070717      * completamente
018140070319if  1c                   if             DB0tbo   =  'A'
018150070514     c                             and  wB0dcp   >  *zeros
018160110427     c                             and  $GeoAtt  =  *on
018170140514      * -?Reperimento consegne parziali precedenti da FNART?
018180140514     c                   exsr      SchConsParz
018190140514     c                   if        xx > *zero
018200070413     c                   eval      $ConsParz     =  *on
018210140514     c                   endif
018220070319e   1c                   endif
018230070315      *
018240070315      * Verifica se richiesta stampa in A4 per C.A. aperta
018250070717      * » se stampa in arrivo e LNA - o capofila £6 - con GEO attivato
018260070717      * completamente
018270070319if  1c                   if             DB0tbo   =  'A'
018280110427     c                             and  $GeoAtt  =  *on
018290070316     c                   exsr      CercaCA
018300070316e   1c                   endif
018310030515      *
018320030515      * Tipo modulo (per la separazione dei box: vettore/destinatario)
018330030515      *   (*in69 = formato A5 / not *in69 = formato A4)
018340030515     c                   eval      *in69  = *on
018350030515      *
018360040607      * Tipo LDV (per richiesta firma per ricevuta della merce senza
018370040607      *   riserve)
018380120530      *   (70 = BRT, 71 = DPD, 72 = FedEx, 73 = EuroExpress)
018390080912     c                   movea     '1000'        *in(70)
018400081210      /free
018410081210         // - LDV DPD
018420170517         //if  §OGntw = 'DPD'   -  ?NON della LnP, MA della fil. mitt.:?
018430081210         if  §CMntw = 'DPD';
018440081210           *in71 = *on;
018450081210         endif;
018460081210      /end-free
018470040311      *
018480030515sel 1c                   SELECT
018490030515      *
018500030515      * Rimesso Mittente: va bene il formato A5
018510030515      *   (se non richiesta la forzatura del formato "A4")
018520040707w   1c                   WHEN      DB0ris = 'M'   and  DB0fa4 <> 'S'
018530040707      *
018540040707      * LDV POSTE non valida per la consegna: va bene il formato A5
018550040707      *   (se non richiesta la forzatura del formato "A4")
018560040707w   1c                   WHEN      §OGntw = 'PPT' and  DB0fa4 <> 'S'
018570071213      *
018580081210     *** * LDV DPD
018590081210     *** > (Spostato prima per NON rendere incompatibile DPD con il box
018600081210     *** >  di note aggiuntive "promemoria per l'autotrasportatore")
018610081210w   1***c*** non della LNP  WHEN      §OGntw = 'DPD' - ma della fil. mitt.:
018620081210w   1***c                   WHEN      §CMntw = 'DPD'
018630081210     ***c                   movea     '0100'        *in(70)
018640071213      *
018650071213      * LDV con mittente FedEx
018660150429w   1c                   WHEN      DB0ccm = c_CliFedEx   or
018670150429     c                             DB0ccm = c_CliFedEx2
018680080922     c                   movea     '0010'        *in(70)
018690071213     c                   eval      *in69  = *off
018700040607      *
018710040607      * LDV con DDT del mittente
018720081013      * - IN FUTURO: se al 1° tentativo di consegna
018730081013      *              (DB0FL1 = "D" - v. pgm FNLRS9R)
018740080917w   1c                   WHEN           $FIAR8 =  *on
018750081013     c***-=»>                      and  DB0fl1 <> 'D'
018760040607     c                   eval      *in69  = *off
018770061102      *
018780071114      * Codice Fiscale o Partita IVA da richiedere al cliente
018790071114w   1c                   WHEN           $No_CF = *on
018800071114     c                             or   $No_PI = *on
018810061102     c                   eval      *in69  = *off
018820070306      *
018830070306      * LDV con Note (dal file FIARN00F)
018840070306w   1c                   WHEN      $FIARN = *on
018850070306     c                   eval      *in69  = *off
018860080801      *
018870080801      * LDV già parzialmente consegnata
018880080801w   1c                   WHEN      $ConsParz   =  *on
018890080801     c                   eval      *in69  = *off
018900070315      *
018910070315      * LDV con C.A. aperta per la spedizione (dal file FNDCT00F)
018920070315w   1c                   WHEN      $C_A_  = *on
018930070315     c                   eval      *in69  = *off
018940060524      *
018950070123      * LDV con mittente "PraOil oleodotti italiani"
018960081127w   1c                   WHEN           DB0ccm = c_CliPraOil
018970060524     c                             or  (DB0fbr = 'S'
018980081127     c                             and  §B4Aks = c_CliPraOil)
018990060524     c                   eval      *in69  = *off
019000090302      /free
019010090302
019020090302           // LDV con mittente con promemoria per AUT in tab. "PXA"
019030110411           WHEN  $TabPXA = *on  or  $TabPXA2 = *on;
019040090302             *in69  = *off;
019050090924
019060090924           // LDV con mittente con promemoria per destinatario
019070090924           WHEN  $BoxNxD = *on;
019080090924             *in69  = *off;
019090090302      /end-free
019100030515      *
019110120530      * LDV BRT
019120150224     *** * - con Bancali da rendere al mittente
019130150224     *** *   o   Bancali da riportare a magazzino
019140150224      * - con Destinatario supermercato
019150030515      *   e/o Promemoria per autotrasportatore
019160040223      *   [Se non trovo niente su FIAR5 né x supermercati né x bancali
019170030515      *    ma ho queste condizioni: devo stampare la bolla in "A4" per
019180030515      *    poter stampare comunque il box vuoto con la memo "Destina-
019190050510      *    tario Supermercato" per poter scrivere a mano i promemoria
019200050510      *    per l'autotrasportatore - ci dovrebbero comunque sempre
019210050510      *    essere note per supermercato.]
019220050510      *   e/o con formato A4 previsto in tab. "7R" (partic. consegna)
019230150224w   1c*//                WHEN         %subst(DB0gva:1:1) = 'B'
019240150224     c*//                          or %subst(DB0gva:1:1) = 'O'
019250150224w   1c                   WHEN                DB0tc1      = 'S'
019260030717     c                             or        DB0tc2      = 'S'
019270071015     c                             or        §7Rldv      = 'S'
019280030515     c                   eval      *in69  = *off
019290071015      *
019300071015      * LDV con ritiro porto franco prepagato
019310071015w   1c                   WHEN          (DB0tbo   = '8'
019320071015     c                             and  §B4poe   > *zeros
019330071015     c                             and  §B4nor   > *zeros)
019340071015     c                   eval      *in69  = *off
019350071015      *
019360071015      * LDV con ritiro contestuale alla consegna
019370100608w   1c**!!!              WHEN          (DB0gma   = Ktab7V_RC
019380100608w   1c                   WHEN          (§7Rrc    = 'S'
019390071015     c                             and  §7Rldv   = 'S')
019400071015     c                   eval      *in69  = *off
019410030515      *
019420030515      * - con Forzatura richiesta al formato "A4"
019430030515w   1c                   WHEN      DB0fa4 = 'S'
019440030515     c                   eval      *in69  = *off
019450030515      *
019460030515e   1c                   ENDSL
019470050513      *
019480050513      * Verifica se con Segnacolli NON sequenziali
019490050513      * (di numero superiore a quelli che stanno al posto del range)
019500050513      * DA CONTROLLARE COMUNQUE (A4 o A5 che sia), VISTO CHE:
019510050513      * - se è previsto un A5, potrebbe scattare l'A4 (per numero
019520050513      *   di segnacolli da elencare troppo alto per lo spazio
019530050513      *   disponibile tra le "NOTE E PARTICOLARITA'")
019540050513      * - se è già previsto un A4, i segnacolli non sequenziali
019550050513      *   potrebbero comunque essere elencabili tra le "NOTE E
019560050513      *   PARTICOLARITA'" (e risparmiare una pagina in caso di
019570050513      *   LdV con DDT del mittente)
019580050513     c*** (già fatto:)   reset                   $A4_x_ScNS
019590050513if  1c                   IF             DB0fns = 'S'
019600050513     c                             and  DB0ncl >  2
019610050513      * - - imposto le righe di "Note e Particolarità" da stampare
019620050513     c                   exsr      SchPARTIC
019630050513      * - - controllo quante righe di "Note e Particolarità" servireb-
019640050513      *     bero per segnalare i segnacolli non sequenziali, in base
019650050513      *     al numero di colli (considerando che riuscirei ad elencare
019660050513      *     max 4 segnacolli per ogni riga di "Note e Particolarità")
019670050513     c                   eval      WelSNS = $elSNSxA5
019680050513     c     DB0ncl        div       WelSNS        YY
019690050513     c                   mvr                     XX
019700050513if  2c                   if        XX     >  0
019710050513     c                   eval      XX     =  YY + 1
019720050513x   2c                   else
019730050513     c                   eval      XX     =  YY
019740050513e   2c                   endif
019750050513      * - - conto anche la riga per l'intestazione
019760050513     c                   add       1             XX
019770050513      * - - conteggio le righe di "Note e Particolarità" utilizzate
019780050513      *     per reperire la 1ª riga di "Note e Particolarità" disponi-
019790050513      *     bile
019800050513     c                   eval      *in31  =  *off
019810050513     c                   eval      YY     =  1
019820050513     c     *blanks       lookup    $NP(yy)                                31
019830050513if  2c                   if        *in31  =  *on
019840050513     c                   eval      YY     =  YY - 1
019850050513x   2c                   else
019860050513     c                   eval      YY     =  %elem($NP)
019870050513e   2c                   endif
019880050513      * - - controllo se ho il numero di campi "Note e Particolarità"
019890050513      *     liberi a sufficienza per i segnacolli da elencare.
019900050513      *     Se non ci stanno => serve un modulo A4
019910050513if  2c                   if        XX > %elem($NP) - YY
019920050513     c                   eval      $A4_x_ScNS  = *on
019930050513     c                   eval      *in69  = *off
019940050513e   2c                   endif
019950050513e   1c                   ENDIF
019960030515      *
019970030515      * Imposto il numero di riga massima raggiungibile per l'eventuale
019980030515      *  stampa dei segnacolli non sequenziali
019990040607if  1c                   if        *in69
020000030515     c                   eval      wMaxLine = $MaxLine1
020010040607x   1c                   else
020020030515     c                   eval      wMaxLine = $MaxLine2
020030040607e   1c                   endif
020040030515      * Memo: ho stampato (sto stampando...) almeno un modulo A4 o A5.
020050030515      *   (utile solo per la segnalazione in stampa delle oltre 6.000
020060071008      *   pagine già stampate - in un formato e/o nell'altro).
020070080926      /free
020080080926         select;
020090100924           //?»»» Richiesta la stampa per PDA (4 LdV x A4)?
020100100924           //    ?di LdV per consegna AL 1° tentativo, ma?
020110100924           //    ?reperita packing-list (da stampare "a parte")?
020120100924           //?(la stampa della relativa packing-list verrà richiesta?
020130100924           // ?successivamente, dopo la stampa di tutte le LdV)?
020140100924           when  DB0fa4  = '4'    and
020150100924               //DB0fl1 <> 'D'    and
020160100924                 *in69   = *off   and
020170130306                 $FIAR8  = *on    and
020180130306                 $§PDF   = *off;
020190100924             DB0tms = 'D';
020200130306           //?»»» Richiesta la stampa per PDA (4 LdV x A4)?
020210130306           //    ?di LdV per consegna AL 1° tentativo, ma?
020220130306           //    ?reperita immagine PDF (da stampare "a parte")?
020230130306           //?(la stampa delle relative immagini verranno richieste?
020240130306           // ?successivamente, dopo la stampa di tutte le LdV e?
020250130306           // ?delle relative packing-list)?
020260130306           when  DB0fa4  = '4'    and
020270130306                 $FIAR8  = *off   and
020280130306                 $§PDF   = *on;
020290130307             DB0tms = 'i';
020300130306           //?»»» Richiesta la stampa per PDA (4 LdV x A4)?
020310130306           //    ?di LdV per consegna AL 1° tentativo, ma?
020320130306           //    ?reperita packing-list (da stampare "a parte") e?
020330130306           //    ?immagine PDF (anch'essa da stampare "a parte")?
020340130306           //?(la stampa della relativa packing-list e delle relative?
020350130306           // ?immagini verranno richieste successivamente, dopo la?
020360130306           // ?stampa di tutte le LdV)?
020370130306           when  DB0fa4  = '4'    and
020380130306               //DB0fl1 <> 'D'    and
020390130306                 *in69   = *off   and
020400130306                 $FIAR8  = *on    and
020410130306                 $§PDF   = *on;
020420130306             DB0tms = 'Y';
020430100924           //?»»» Richiesta la stampa per PDA (4 LdV x A4)?
020440080926           when  DB0fa4 = '4';
020450100927             DB0tms = 'Q';
020460100924           //?»»» Richiesta la stampa della packing-list per PDA?
020470081007           when  DB0fa4 = 'P'   and   $FIAR8 = *on;
020480080926             DB0tms = 'P';
020490081007           when  DB0fa4 = 'P'   and   $FIAR8 = *off;
020500081007             //DB0tms = *blank;
020510130307           //?»»» Richiesta la stampa normale e reperita immagine PDF?
020520130307           when  *in69  and  $§PDF;
020530130307             DB0tms = 'I';
020540130307           when  Not *in69  and  $§PDF;
020550130307             DB0tms = 'J';
020560100924           //?»»» Richiesta la stampa normale?
020570080926           when  *in69;
020580080926             DB0tms = '5';
020590080926           other;
020600080926             DB0tms = '4';
020610080926         endsl;
020620080926      /end-free
020630040707      *
020640040707      * Controllo se devo stampare "Non utilizzare per la consegna"
020650040707      * nella copia per il vettore,?senza?copia per il destinatario:
020660040707     c*** (già fatto:)   reset                   $No_x_Cons
020670071203      * - LdV recapito c/assegni:
020680090511      *   stampata in arrivo e con
020690120113if  1c                   if             DB0tbo <> 'P'
020700070306     c                             and  DB0tbo <> '8'
020710090511     c                             and  DB0lnp =  102
020720070306     c                             and  DB0cbo =  'FW'
020730071203     c                             and  DB0nft =  *zeros
020740071203      *   - fil. arrivo con GEO attivato
020750071203     c                             and (DB0fl1    =  'A'
020760071203     c                              or  wGeoCon   =  'S'
020770071203      *   - fil. arrivo abilitata al picking
020780120113     c                              or  A§OGpck   =  'S')
020790070306     c                   eval      $No_x_Cons  =  *on
020800120113e   1c                   endif
020810120113      *//* - LdV Poste
020820120113w   1c*//                   when      §OGntw   = 'PPT'
020830120113     c*//                   eval      $No_x_Cons  =  '2'
020840101125      /free
020850101125         // -?Controlla se "tagliatella" per Ritorno all'Incasso?
020860101129         //$Rit_a_Inc = ( DB0fa4  = '4'    and
020870101129         //               DB0tbo <> 'P'    and  DB0tbo <> '8'   and
020880101129         //               wB0dcm  > *zero  and
020890101129         //           ( ( DB0cas >  *zero  and  wB0icc =  'R' )  or
020900101129         //             ( DB0ift >  *zero  and  wB0ica =  'R' ) ) );
020910101129         // -?Controlla se LdV per Ritorno all'Incasso?
020920101129         $Rit_a_Inc = ( DB0fl4 <> *blank and
020930101129                        DB0tbo <> 'P'    and  DB0tbo <> '8'   and
020940101129                        wB0dcm  > *zero  and
020950101129                    ( ( DB0cas >  *zero  and  wB0icc =  'R' )  or
020960101129                      ( DB0ift >  *zero  and  wB0ica =  'R' ) ) );
020970101125      /end-free
020980030515      *
020990030515     c                   ENDSR
021000040607      *
021010040607      *---------------------------------------------------------------*
021020130315      *?Verifica esistenza dati del DDT allegato da stampare  o      ?*
021030130315      *?               di uno o più PDF allegati da stampare         ?*
021040040607      *---------------------------------------------------------------*
021050130315     c     CercaDDToPDF  BEGSR
021060040607      *
021070060921     c*** (da non fare:) reset                   $E_AR8
021080060921     c*** (già fatto:)   reset                   $FIAR8
021090130315     c*** (già fatto:)   reset                   $§PDF
021100040607     c*** (già fatto:)   reset                   $DDT_o
021110040607     c*** (già fatto:)   reset                   $DDT_p
021120040608     c*** (già fatto:)   reset                   $DDTlastPg
021130040607     c*** (già fatto:)   reset                   $AR8o1
021140040608     c*** (già fatto:)   reset                   wOmaRowT
021150040608     c*** (già fatto:)   reset                   wOmaRowD
021160041207     c*** (già fatto:)   reset                   dAR8
021170130315      *
021180130315      * -?Verifica esistenza PDF allegato?
021190130315     c                   eval      $§PDF  = (§AR5als = 'S')
021200130315      *
021210130315      * -?Verifica esistenza packing-list?
021220130315     c                   eval      $FIAR8 = (§AR5alx = 'S')
021230130315      *
021240130315if  1c                   if        NOT %open(FIAR801L)
021250130315     c                             and ($FIAR8  or  $§PDF)
021260130315     c                   open(e)   FIAR801L
021270130315      * Se errore significa che non c'è il file per cui non faccio
021280130315      *  più questa routine
021290130315if  2c                   if        %error
021300130315     c                   eval      $E_AR8 = *off
021310130315e   2c                   endif
021320130315e   1c                   endif
021330040607      *
021340130306     c                   ENDSR
021350061102      *
021360061102      *---------------------------------------------------------------*
021370071114      *?Verifica se Codice Fiscale e/o Partita Iva da richiedere al  ?*
021380071114      *?  cliente                                                    ?*
021390061102      *---------------------------------------------------------------*
021400061102      *  Il d.l. 223/06, poi convertito con la legge 248/06, ha      *
021410061102      *   reintrodotto l'obbligo di raccogliere il codice fiscale per *
021420061102      *   tutti i clienti e fornitori, siano essi aziende o privati.  *
021430061102      * SE?Porto Franco PrePagato?:                                   *
021440061102      *   il Cod.Fiscale dovrà essere del?Mittente?.                  *
021450061102      * SE?Porto Assegnato con Fattura Immediata?:                    *
021460061102      *   il Cod.Fiscale dovrà essere del?Destinatario?.              *
021470061102      * In sua assenza: dovrà essere richiesto al cliente stesso!     *
021480071114      *  Lo stesso decreto (poi convertito con la legge 248/06)      *
021490071114      *   prevede che a partire dal 01/01/2008 debba essere raccolta  *
021500071114      *   anche l'eventuale partita iva, del cliente o del fornitore. *
021510071114      * Stessi criteri già utilizzati per il codice fiscale.          *
021520061102      *---------------------------------------------------------------*
021530071114     c     CheckCFPI     BEGSR
021540061102      *
021550061106     c*** (già fatto:)   eval      $No_CF  = *off
021560071114     c*** (già fatto:)   eval      $No_PI  = *off
021570061102      *
021580071114      * Verifica se C.F./P.I. da richiedere (NON per clienti esteri)
021590071114      * - con fattura
021600071114if  1c                   IF            (DB0nft  <> *zeros
021610061106     c                              or  DB0ift  <> *zeros)
021620061103     c                             AND (
021630061102      * - - P.F. prepagato
021640061107      *     (con mittente italiano)
021650061107     c                                 (DB0tbo  =  '8'
021660061107     c                              and DB0nzm  =  *blanks)
021670061106      * - - P.A. (con fattura immediata)
021680061107      *     (con destinatario italiano)
021690061107     c                             or  (%subst(DB0tb1:1:1) = 'A'
021700061107     c                              and DB0nzd  =  *blanks)
021710061106      * - - con 2ª bolla
021720061107      *     (con destinatario italiano)
021730061103     c                             or  (DB0tb2  <> *blanks
021740121029     c                              and DB0tbo  <> 'P'
021750061107     c                              and DB0tbo  <> '8'
021760061107     c                              and DB0nzd  =  *blanks)
021770071114      * - => verirficare se da richiedere C.F. e/o P.I.
021780061103     c                                 )
021790120503      *
021800120503sel 2c                   select
021810120503      * · Esiste Partita Iva NON FITTIZIA: non chiedere nulla
021820120503w   2c                   when      wB0piv <> *blanks   and
021830120503     c                             %scan('$$' : wB0piv) = *zero
021840120503     c                   leavesr
021850120503      * · Esiste C.F. lungo 16 (privato) : non chiedere P.I.
021860120503w   2c                   when      %len( %trim( wB0cfi ) ) = 16
021870120503     c                   leavesr
021880120503      * · Esiste C.F. lungo 11 ..........: chiedere P.I. (se mancante)
021890120503w   2c*//                when      %len( %trim( wB0cfi ) ) = 11
021900120503e   2c                   endsl
021910071114      *
021920071114      * - senza codice fiscale
021930120503     c                   eval      $No_CF  = (wB0cfi = *blanks)
021940071114      *
021950071114      * - senza partita iva (se NON privato)
021960120503     c                   eval      $No_PI  = (wB0piv = *blanks)
021970071114      *
021980071114e   1c                   ENDIF
021990061102      *
022000061102     c                   ENDSR
022010070306      *
022020070306      *---------------------------------------------------------------*
022030070306      *?Verifica esistenza note da stampare (dal file FIARN00F)      ?*
022040070306      *---------------------------------------------------------------*
022050070306     c     CheckFIARN    BEGSR
022060070306      *
022070070306     c*** (già fatto:)   reset                   $FIARN
022080070306      *
022090070306      * Controllo esistenza note solo se c'è il file
022100070319     c     K06ARN11      chain     FIARN000
022110070319if  1c                   if             %found(FIARN11L)
022120070306     c                             and  ARNnob <> *blanks
022130070306     c                   eval      $FIARN = *on
022140070306e   1c                   endif
022150070306      *
022160070306     c                   ENDSR
022170070315      *
022180070315      *---------------------------------------------------------------*
022190070315      *?Verifica esistenza C.A. aperta legata alla spedizione        ?*
022200070315      *---------------------------------------------------------------*
022210070316     c     CercaCA       BEGSR
022220070315      *
022230070319     c*** (già fatto:)   reset                   $C_A_
022240070315      *
022250070319      * Richiamo del programma di comodo per recuperare le CA legate
022260070319      *   alla spedizione in esame
022270070319     c                   reset                   FIDN12ds
022280070514if  1c                   if        $Mamma = *off
022290070319     c                   eval      I12aas = DB0aas
022300070319     c                   eval      I12lnp = DB0lnp
022310070319     c                   eval      I12nrs = DB0nrs
022320070319     c                   eval      I12nsp = DB0nsp
022330070514x   1c                   else
022340070514     c                   eval      I12aas = LBLaap
022350070514     c                   eval      I12lnp = LBLlpp
022360070514     c                   eval      I12nrs = LBLnrp
022370070514     c                   eval      I12nsp = LBLnsp
022380070514e   1c                   endif
022390070319     c*** già così:      eval      I12fel = 'E'
022400070319     c***  "    "        eval      I12fan = 'E'
022410070319     c***  "    "        eval      I12fch = 'E'
022420070319      *
022430070319     c                   call      'FIDN12R'
022440070319     c                   parm                    FIDN12ds
022450070319      *
022460070319      * Se trovata almeno una CA
022470070319     c                   eval      $C_A_  = (O12nca > *zeros)
022480070315      *
022490070316     c                   ENDSR
022500070319      *
022510070319      *---------------------------------------------------------------*
022520070319      *?Verifica se stampa spedizione DOPO "Lasciato Avviso"         ?*
022530070319      *---------------------------------------------------------------*
022540070319     c     CheckLascAvv  BEGSR
022550070319      *
022560070319     c*** (già fatto:)   reset                   $LascAvv
022570070319      *
022580070319if  1c                   if        NOT  %open(FNEVB05L)
022590070319     c                   open      FNEVB05L
022600070319e   1c                   endif
022610070319      *
022620070319     c     KLDV          setgt     FNEVB000
022630070319     c     KLDV          readpe    FNEVB000
022640070319      *
022650070319      * Proseguo solo se ritrovo un evento
022660070319do  1c                   DOW       NOT  %eof(FNEVB05L)
022670070319      *
022680070319      * Verifico se evento di tipo Giacenza
022690070319     c                   eval      *in31    = *off
022700070319     c     EVBcev        lookup    $2Acev_G                               31
022710070319      **
022720070319sel 2c                   Select
022730070319      * - Evento GIACENZA --> NO LASCIATO AVVISO
022740070319w   2c                   When      *in31
022750070319     c                   leave
022760070319      * - Evento RIC --> 1° LASCIATO AVVISO
022770070319w   2c                   When      EVBcev   = 'RIC'
022780070319     c                   eval      $LascAvv = *on
022790131205      * - - salvataggio data/ora
022800131205     c                   eval      wEVBdev  = EVBdev
022810131205     c                   eval      wEVBhev  = EVBhev
022820070319     c                   leave
022830070319      * - Evento DA NON ELABORARE
022840070319x   2c                   Other
022850070319     c     KLDV          readpe    FNEVB000
022860070319e   2c                   Endsl
022870070319      **
022880070319e   1c                   ENDDO
022890070319      *
022900070319     c                   ENDSR
022910090302      /free
022920090302
022930090302       //--------------------------------------------------------------
022940090302       //?Verifica esistenza note da stampare (dal file TNTBE00F)      ?
022950090302       //--------------------------------------------------------------
022960090302       BEGSR  CheckTabPXA;
022970090302
022980090302         //clear  $TabPXA;          (già fatto)
022990110411         //clear  $TabPXA2;         (già fatto)
023000110411
023010110411         // -?Verifica esistenza "PXA" per particolarità consegna?
023020110411         If  DB0gma <> *blank;
023030170517           clear  k05tntbe01;
023040110411           k_TBEcod = 'PXA';
023050110411           k_TBEke1 = DB0gma;
023060110411           k_TBEke2 = '001            ';
023070110411           setll  %kds(k05tntbe01 : 3)  TNTBE000;
023080110411           // -?Bisogna testare se nota ANNULLATA?
023090110411           //$TabPXA  = %equal(TNTBE01L);
023100110411           reade  %kds(k05tntbe01 : 2)  TNTBE000;
023110110411           $TabPXA2 = (Not %eof(TNTBE01L)   and   TBEatb = *blank
023120110411                       and TBEke2 <= '006            ');
023130110411         EndIf;
023140090302
023150110411         // -?Reperimento eventuali "PXA" per mittente?
023160170517         clear  k05tntbe01;
023170090302         k_TBEcod = 'PXA';
023180090302         k_TBEke1 = %editc(DB0ccm : 'X');
023190100316         k_TBEke2 = '001            ';
023200090302         clear  k_TBElin;
023210090302         clear  k_TBEsif;
023220100316         setll  %kds(k05tntbe01 : 3)  TNTBE000;
023230110411         // -?Bisogna testare se nota ANNULLATA?
023240090302         //$TabPXA  = %equal(TNTBE01L);
023250090302         reade  %kds(k05tntbe01 : 2)  TNTBE000;
023260100316         $TabPXA  = (Not %eof(TNTBE01L)   and   TBEatb = *blank
023270100316                     and TBEke2 <= '006            ');
023280090302
023290090302       ENDSR;
023300090302      /end-free
023310030515      *
023320030515      *---------------------------------------------------------------*
023330050217      *?Imposta i campi di stampa con quelli della DS                ?*
023340030515      *---------------------------------------------------------------*
023350030515     c     PrepPRT       BEGSR
023360040223      *
023370040223      * Reperimento dati da FIAR5 / "GEN"
023380130315     c*** (già fatto:)   exsr      RepAR5gen
023390070212     *** *
023400070212     *** * Impostazione Data spedizione
023410070212     *** * (già fatto nella subr. "TipoLdV")
023420070319      *
023430100218      * CONTROLLO SE STAMPA SPEDIZIONE DOPO "LASCIATO AVVISO"
023440100218      * (?*-=»>   SE NON MESSA IN "FERMO DEPOSITO"   <«=-?):
023450081118      * (NON se richiesta stampa di "ricevuta" - 4 in A4)
023460110427      * per LNA (o capofila se LNA è in £6) con $GeoAtt = *ON =>
023470070717      * GEO attivato completamente
023480070319      * controllo se la spedizione ha già più tentativi di consegna e
023490070319      *   determino se in stato di "lasciato avviso"
023500070319      *
023510070319     c*** (già fatto:)   reset                   $LascAvv
023520070406if  1c                   if             DB0tbo    =  'A'
023530081117     c                             and  DB0fa4    <> '4'
023540070514     c                             and  wB0ntc    >  *zeros
023550110427     c                             and  $GeoAtt   =  *on
023560100218     c                             and  DB0ffd    =  *blank
023570070319     c                   exsr      CheckLascAvv
023580070319e   1c                   endif
023590030515      *
023600120530      * Impotazione dati BRT S.p.A.
023610120530     c*//                if        (DB0dft > *zero       and
023620120530     c*//                           DB0dft > §vpoBRTdta)  OR
023630120530     c*//                          (DB0dft = *zero       and
023640120530     c*//                           Wdsp   > §vpoBRTdta)
023650120530     c                   eval      TBAR   =  $BRT
023660120530     c*//                else
023670120530     c*//                eval      TBAR   =  $Bartolini
023680120530     c*//                endif
023690070307     c                   eval      TBA1   =  $Intest1
023700070307     c                   eval      TBA2   =  $Intest2
023710080707if  1c                   if            DB0nft <> *zeros
023720080707     c                             or  DB0ift <> *zeros
023730080627     c                   eval      TBA3   =  $Intest3
023740080707e   1c                   endif
023750070307     c                   eval      TBA4   =  $Intest4
023760070307     c                   eval      TBA5   =  $Intest5
023770070307     c*** NON sopra:     eval      TBA6   =  $Intest6
023780120530     c*//                eval      TBA7   =  $Intest7
023790120530     c                   eval      TBA7   =  $Intest7_BRT
023800030515      *
023810030515      * - Intestazione moduli
023820080919     c                   eval      Tcxd   =  c_CopiaXdest
023830130726     c*//                if        Not *in36     (NON ancora acceso)
023840130726if  1c                   if        DB0tbo <> '8'  or  DB0ris = 'M'
023850130726     c                   eval      Tcxd2  =  Tcxd
023860130726     c                   endif
023870091008      *
023880091008      /free
023890091008         select;
023900091008           // - Indicazione Copia LDV x cliente
023910091008           when  DB0ris = 'C';
023920091008             Pkco = 'C O P I A';
023930091008           // - Altre indicazioni: R=Ristampa, V=Variazione, ...
023940091008           other;
023950091008             Tkri = DB0ris;
023960091008         endsl;
023970091008      /end-free
023980030515      *
023990030515      * - Codice a barre: ANNO+LNP+NRS+NSP+CKD
024000030515      *   (chiamo il pgm TRUL28R per ricavarne il check digit)
024010030515     c                   reset                   TRUL28ds
024020030515     c                   z-add     DB0aas        BARcdeAAS
024030030515     c                   eval      BARcdeLNP = DB0lnp
024040030515     c                   eval      BARcdeNRS = DB0nrs
024050030515     c                   eval      BARcdeNSP = DB0nsp
024060030515     c                   movel     BarCodeDS     I28cod
024070030515     c                   call      'TRUL28R1'
024080030515     c                   parm                    TRUL28DS
024090030515if  2c                   if        O28err =  *blanks
024100030515     c                   movel     O28cod        Tbcd
024110030515e   2c                   endif
024120140915      *
024130140915      /free
024140140915
024150140915         // -?BRTcode?
024160140915         reset  TRUL28ds0;
024170140915         trul28ds0.i280cod = BarCodeDS;
024180140915         trul28r0 ( TRUL28ds0 );
024190140915         if  trul28ds0.o280err <> *on;
024200140917           //TbrtC = trul28ds0.o280cod;
024210140917           TbrtC = %subst( trul28ds0.o280cod :  1 : 5 ) + ' ' +
024220140917                   %subst( trul28ds0.o280cod :  6 : 5 ) + ' ' +
024230140917                   %subst( trul28ds0.o280cod : 11 : 5 ) + ' ' +
024240140917                   %subst( trul28ds0.o280cod : 16 : 4 );
024250140915         endif;
024260140915
024270140915      /end-free
024280030515      *
024290030515      * - Sigla operatore video
024300030515     c                   eval      Psop   =  DB0sop
024310080721      /free
024320080721
024330130306         // -?Flag "allega documento di trasporto"?
024340130418         //   IN FUTURO: se al 1° tentativo di consegna
024350130418         //              (DB0FL1 = "D" - v. pgm FNLRS9R)
024360080721         select;
024370081119
024380130306           // -?LdV con DDT allegato oltre il 1° tentativo di consegna?
024390081127           when  DB0bam =  'S'   and   wB0tmc <> *blank;
024400081127             evalr Pddt =  'DDT sul collo';
024410081119
024420130306           // -?LdV con DDT allegato (al 1° tentativo di consegna)?
024430080723           when  DB0bam =  'S';
024440080723             evalr Pddt =  'DDT';
024450081119
024460130306           // -?Esiste packing-list abbinata?
024470130306           when  $FIAR8 =  *on   and   $§PDF =  *off;
024480081013           //if  DB0fl1 <> 'D';
024490081013               evalr Pddt =  'PACKING-LIST';
024500081013           //else;
024510081013           //  evalr Pddt =  'PACKING-LIST GIA'' CONSEGNATA';
024520081013           //endif;
024530130306
024540130306           // -?LdV con PDF allegato?
024550130306           when  $FIAR8 =  *off  and   $§PDF =  *on;
024560130306             evalr Pddt =  'PACKING-LIST PDF';
024570130306
024580130306           // -?LdV con packing-list abbinata & PDF allegato?
024590130306           when  $FIAR8 =  *on   and   $§PDF =  *on;
024600130308             evalr Pddt =  'PACKING-LIST PDF';
024610081119
024620130306           // -?LdV senza PDF allegato & senza packing-list abbinata?
024630080721           other;
024640080721             clear Pddt;
024650081119
024660080721         endsl;
024670080721
024680080721      /end-free
024690030515      *
024700030515      * - Mittente
024710170517     c                   if        *in71
024720170517     c                   exsr      sr_Mitt_DPD
024730170517     c                   endif
024740170517      *
024750030515     c                   if        DB0ccm <> *zeros
024760030515     c                   move      DB0ccm        Pccm
024770030515     c                   endif
024780030515     c                   eval      Prsm   =  DB0rsm
024790030515     c                   eval      Pinm   =  DB0inm
024800030515     c                   eval      Pcam   =  DB0cam
024810030515     c                   eval      Plom   =  DB0lom
024820030515     c                   eval      Pprm   =  DB0prm
024830030515     c                   eval      Pnzm   =  DB0nzm
024840030515      *   Mittente originale
024850030515if  1c                   if        DB0rmo <> *blanks
024860030515     c                   eval      Pkmo   =  'MITT. ORIGINALE'
024870030515     c                   eval      Prmo   =  DB0rmo
024880030515e   1c                   endif
024890030515      *
024900070308      * - Filiale di Arrivo
024910030515if  1c                   if        DB0ris <> 'M'
024920080721     c                   eval      Pkla   =  'FILIALE'
024930170717     c                   eval      Plna1  =  %editc(wDB0lnaCf£6 : 'X')
024940080722     c                                    +  ' '
024950170717     c                                    +  wDB0dlaCf£6
024960080722     c                                    +  ' - '
024970170717     c                                    +  wDB0inlCf£6
024980140129     c*//                                 +  ' Tel.'
024990140129     c                                    +  '     '
025000170717     c                                    +  %trimr(wDB0telCf£6)
025010170717     c                   eval      Plna2  =  %editc(wDB0calCf£6 : 'X')
025020080722     c                                    +  ' '
025030170717     c                                    +  wDB0lolCf£6
025040080722     c                                    +  ' '
025050170717     c                                    +  wDB0prlCf£6
025060140129     c*//                                 +  '                  '
025070140129     c*//                                 +  ' Fax '
025080170717     c*//                                 +  %trimr(wDB0faxCf£6)
025090170717     c                   if        wDB0faxCf£6 <> *blanks
025100170717     c     c_Mai:c_Min   xlate     wDB0faxCf£6   DB0fax
025110140129     c                   eval      Plna2  =  %trimr(Plna2)
025120140129     c                                    +  '          '
025130140129     c*//*                                +  ' E-mail: '
025140140129     c                                    +  '         '
025150170717     c                                    +  %trimr(wDB0faxCf£6) + '@brt.it'
025160140129     c                   endif
025170030515e   1c                   endif
025180030515      *
025190030515      * - Destinatario
025200030515if  1c                   if        DB0ccd <> *zeros
025210030515     c                   move      DB0ccd        Pccd
025220030515e   1c                   endif
025230030515     c                   eval      Prsd   =  DB0rsd
025240030515     c                   eval      Prse   =  DB0rd2
025250030515     c                   eval      Pind   =  DB0ind
025260030515     c                   eval      Pcad   =  DB0cad
025270030515     c                   eval      Plod   =  DB0lod
025280030515     c                   eval      Pprd   =  DB0prd
025290030515     c                   eval      Pnzd   =  DB0nzd
025300030515      *
025310030515      * - Riferimenti mittente
025320030515     c                   eval      Prmn   =  DB0rmn
025330030515     c                   eval      Prma   =  DB0rma
025340061107      * se bolla DPD recupero il riferimento da FIAR4 rcd "I"
025350061107     c*** (già fatto:)   clear                   dsBL4I
025360060626     c                   if        *in71
025370061103     c                   exsr      RepAR4_I
025380060626     c                   endif
025390030515      *
025400030515      * - Nominativo & telefono
025410031124      *   (da FIAR5 / "GEN")
025420040607     c*** (già fatto:)   exsr      RepAR5gen
025430031124     c                   eval      Pnom   =  §AR5ref
025440031124     c                   eval      Ptel   =  §AR5teld
025450030515      *
025460030515      * - Spedizione
025470030515      *   (numero: non se Rimesso al Mittente)
025480030515if  1c                   if        DB0ris <> 'M'
025490030515     c                   eval      Plnp   =  DB0lnp
025500030515     c                   eval      Pnrs   =  DB0nrs
025510030515     c                   eval      Pnsp   =  DB0nsp
025520030515e   1c                   endif
025530030515      * . . Data spedizione
025540031124     c     *iso          movel     Wdsp          Data_EUR
025550130418     c     *eur          movel     Data_EUR      Pdsp
025560140922     c     *dmy          movel     Data_EUR      Pdsp6
025570030515      *
025580030515      * - Colli: numero, peso, volume, flag tipo-volume
025590030515     c                   eval      Pncl   =  DB0ncl
025600030515     c                   eval      Ppkf   =  DB0pkf
025610030515     c                   eval      Pvlf   =  DB0vlf
025620030515if  1c                   if            DB0fvf <> *blanks
025630030515     c                             and DB0ris <> 'M'
025640030717     ***c***                eval      Pfvf   =  '-' + DB0fvf + '-'
025650030717     c                   move      DB0fvf        Pfvf
025660030515e   1c                   endif
025670030515      *
025680030515      * - Natura merce
025690030515     c                   eval      Pnat   =  DB0nas
025700030515      *
025710030515      * - Servizio
025720100513      /free
025730100513         if  %subst(DB0tb1:1:1) = 'A'   or   DB0tb2 <> *blanks;
025740100513           exsr  sr_CheckORM_PrevvMail;
025750100513         endif;
025760100513      /end-free
025770040607      * . . Descrizione LDV
025780100513     c                   if        Not  $PreavvORM
025790030515     c                   eval      Pds1   =  DB0dcb
025800100513     c                   endif
025810030515      * . . Descrizione tipo servizio
025820030515if  1c                   if        DB0fst =  'S'
025830030515     c                   eval      Pds2   =  DB0dts
025840030515e   1c                   endif
025850030515      * . . Reso / Franco frontiera
025860030515sel 1c                   select
025870030515w   1c                   when      DB0fbr =  'S'
025880030515     c                   clear                   w008a
025890030515     c                   eval      w008a  =  '   RESO '
025900030515     c                   move      w008a         Pds2
025910030515w   1c                   when      DB0fbr =  'F'
025920030515     c                   clear                   w015a
025930030515     c                   eval      w015a  =  '   FR.FRONTIERA'
025940030515     c                   move      w015a         Pds2
025950030515e   1c                   endsl
025960030515      *
025970030515      * - Zona consegna
025980030515     c                   eval      Pznc   =  DB0znc
025990030515      *
026000030515      * - Flag "*" (gestito a parte)
026010040223      *
026020040223      * - Flag "V" = colli di valore
026030040223if  1c                   if        §AR5bva   <> *blank
026040040223     c                   eval      Pbva   =  'V'
026050040223e   1c                   endif
026060060921      *
026070070212      * - Codice Giro o Numero Distinta (se tipo bolla "ARRIVO")
026080071015if  1c                   IF              DB0tbo = 'A'
026090071015     c                             and  wGeoCon = 'S'
026100070212sel 2c                   Select
026110070212      * · - Stampa LdV in arrivo con codice giro (parametro passato)
026120071015w   2c                   When      DB0fl1     =  'A'
026130070215if  3c                   if        §DB0tgi    =  'M'
026140101028     c                   eval      Pndc   =  'g:' + §DB0cgi + 'm'
026150070215x   3c                   else
026160101028     c                   eval      Pndc   =  'g:' + §DB0cgi
026170070215e   3c                   endif
026180070212      * · - Stampa LdV con numero distinta consegna
026190071015w   2c                   When           wB0ndc  > *zeros
026200110427     c                             and  $GeoAtt =  *on
026210101028     c                   eval      Pndc   =  'd:'
026220070514     c                                    +  %trim(%editc(wB0ndc:'Z'))
026230101028if  3c                   if        %len(Pndc)  >= %len(%trimr(Pndc))
026240071016     c                                          + 3 + %len(Wpdc4a)
026250101028     c                   eval      Pndc   =  %trimr(Pndc)
026260071022     c                                    +  ' a:'
026270071022     c                                    +  Wpdc4a
026280071016e   3c                   endif
026290070212      * · - Stampa LdV con codice giro (da reperire in FIARG00F)
026300070213w   2c                   Other
026310070212if  3c                   if        NOT %open(FIARG01L)
026320070212     c                   open      FIARG01L
026330070212e   3c                   endif
026340070215     c                   clear                   ARGcgi
026350070215     c                   clear                   ARGtgi
026360070212     c     KLDV          chain     FIARG000
026370070215if  3c                   if        ARGtgi =  'M'
026380101028     c                   eval      Pndc   =  'g:' + ARGcgi + 'm'
026390070215x   3c                   else
026400101028     c                   eval      Pndc   =  'g:' + ARGcgi
026410070215e   3c                   endif
026420070215e   2c                   EndSl
026430070212e   1c                   ENDIF
026440030515      *
026450030515      * - Fermo deposito
026460030515if  1c                   if        DB0ffd <> *blanks
026470060921     c                   eval      Pffd   =  'FERMO DEPOSITO '
026480030515e   1c                   endif
026490030515      *
026500070308      * - Segnacolli: filiale, serie, dal / al
026510030515if  1c                   if        DB0fls <> *zeros
026520030515     c                   move      DB0fls        Ppos
026530030515e   1c                   endif
026540040607      * - - se bolla con mamma considero la serie della mamma
026550070515if  1c                   if        $Mamma =  *on
026560031015     c                   eval      Psrs   =  wB0nrs
026570031015x   1c                   else
026580030515     c                   eval      Psrs   =  DB0nrs
026590031015e   1c                   endif
026600030515      * - Se colli non sequenziali: stampo "NON SEQUENZIALI" + Elenco
026610030515      *   Se collo cumulativo ....:   "     SCD "X" Num.Colli
026620030515      *   Se colli sequenziali ...:   "     SCD "/" SCA
026630030515      *   Se collo singolo .......:   "     SCD
026640031015sel 1c                   select
026650030515      * - - non sequenziali
026660030515w   1c                   when      DB0fns =  'S'
026670050513if  2c                   if           *in69      = *on
026680050513     c                             or $A4_x_ScNS = *off
026690030715      *     caricamento delle "Note e Particolarità" per consentirne
026700030715      *     l'aggiunta come "Elenco segnacolli non sequenziali"
026710031015if  3c                   if        $Partic = *off
026720030515     c                   exsr      SchPARTIC
026730031015e   3c                   endif
026740030515     c                   exsr      ListaScNS
026750050511x   2c                   else
026760050511if  3c                   if        DB0ncl <= 2
026770050511     c                   exsr      ListaScNSd
026780081007x   3c                   else
026790081007     c                   eval      Pnsx   =  'NON SEQUENZIALI'
026800050511e   3c                   endif
026810030515e   2c                   endif
026820030515      * - - cumulativo
026830050114w   1c**!!!              when      DB0xco =  'X'
026840081010     c**!!!              eval      Pnsx   =  %editc(DB0ncd : 'Z')
026850081010     c**!!!                               +  'X'
026860081010     c**!!!                               +  %editc(DB0ncl : 'Z')
026870030515      * - - singolo o sequenziali
026880030515      *     (non se rimesso al mittente)
026890030515w   1c                   when      DB0ris <> 'M'
026900080922     c                   eval      Pnsx   =  %editc(DB0ncd : 'Z')
026910030515if  2c                   if            DB0nca <> *zeros
026920030515     c                             and DB0nca <> DB0ncd
026930080922     c                   eval      Pnsx   =  %trimr(Pnsx) + '-'
026940080922     c                                    +  %editc(DB0nca : 'Z')
026950030515e   2c                   endif
026960030515e   1c                   endsl
026970030515      *
026980030716      * - Note (1 e 2) e Particolarità (0, 3, 4, 5 e 6)
026990030710      *   con eventuali segnacolli non sequenziali (max 4 x riga)
027000030515if  1c                   if        $Partic = *off
027010030515     c                   exsr      SchPARTIC
027020030515e   1c                   endif
027030030716if  1c                   if           $NP(1) <> *blanks
027040030716     c                             or $NP(2) <> *blanks
027050030716     c                             or $NP(3) <> *blanks
027060030716     c                             or $NP(4) <> *blanks
027070030716     c                             or $NP(5) <> *blanks
027080030716     c                             or $NP(6) <> *blanks
027090030716     c                   eval      Pknp   =  'NOTE  E  PARTICOLARITÀ'
027100030710     c                   eval      Pnp1   =  $NP(1)
027110030710     c                   eval      Pnp2   =  $NP(2)
027120030710     c                   eval      Pnp3   =  $NP(3)
027130030710     c                   eval      Pnp4   =  $NP(4)
027140030710     c                   eval      Pnp5   =  $NP(5)
027150030710     c                   eval      Pnp6   =  $NP(6)
027160030716e   1c                   endif
027170030515      *
027180080401      * - Privato / Appuntamento / Uffici / Consegna Garantita
027190080401      *   (servizio consegna garantita x cliente Wuerth)
027200170413if  1c*//                if        DB0gva =  'P '
027210170413     c*//                evalr     Ppad = 'PRIVATO'
027220170413e   1c*//                endif
027230041012if  1c                   if        DB0gva =  'U '
027240080723     c                   evalr     Ppad = 'CONSEGNA AD UFFICI'
027250041012e   1c                   endif
027260081010if  1c*** NO !!!         if           DB0tc1 =  'A'
027270081010     c*** NO !!!                   or DB0tc2 =  'A'
027280081010     c*** NO !!!         move      'APPUNTAMENTO'Ppad
027290081010e   1c*** NO !!!         endif
027300080401if  1c                   if             DB0nrs = 40
027310090518     c                             and  DB0tsp = 'E'
027320080401     c                             and (DB0lnp = 043
027330110429     c                              or  DB0lnp = 277
027340080401     c                              or  DB0lnp = 063)
027350090217     c*****              evalr     Ppad = 'CONSEGNA GARANTITA'
027360090227     c                   eval      Pds2 = 'CONSEGNA ESPRESSA +
027370090227     c                                     WÜRTH'
027380080401e   1c                   endif
027390030515      *
027400030515      * - Data e ora consegna richiesta
027410030515      *   e Flag "*" > x Data e/o Ora consegna richiesta
027420091116      *   (NO "Prima del" per il cliente TTL)
027430091116if  0c                   if        DB0ccm <> c_CliTTL  or
027440091116     c                             DB0dcr =  *zeros    or  DB0tcr <> 'P'
027450030515if  1c                   if        DB0dcr <> *zeros
027460030515     c                   eval      Past   =  '*'
027470030515     c     *iso          movel     DB0dcr        Data_EUR
027480030515     c     *eur          movel     Data_EUR      Pdcr
027490030515e   1c                   endif
027500030515if  1c                   if        DB0hcr <> *zeros
027510030515     c                   eval      Past   =  '*'
027520030515     c                   eval      *in18  =  *on
027530030515     c                   eval      Phcr   =  DB0hcr
027540030515e   1c                   endif
027550091116e   0c                   endif
027560030515      *
027570030515      * - Consegne particolari
027580030515     c                   eval      Pcp1   =  DB0dc1
027590030515     c                   eval      Pcp2   =  DB0dc2
027600030515     *** *
027610030515     *** * - Valore merce dichiarato e relativa divisa => NO
027620030515     ***c***                eval      Pvmd   =  DB0vmd
027630030515     ***c***                eval      Pvad   =  DB0vad
027640030515     *** *
027650030515     *** * - Importo da assicurare e relativa divisa   => NO
027660030515     ***c***                eval      Pvas   =  DB0vas
027670030515     ***c***                eval      Pias   =  DB0ias
027680030515     *** *
027690030515     *** * - Quantità da fatturare                     => NO
027700030515     ***c***                eval      Pqft   =  DB0qft
027710030515      *
027720030515      * - Imponibile / IVA / Esenzione / Totale fattura e divisa
027730030515      *   (importi "arrotondati" a 2 decimali in stampa)
027740030515if  1c                   if        DB0ift <> *zeros
027750030515     c                   eval      Pkim   = 'IMPONIBILE'
027760130218     c*//                   eval(h)   Pimp   =  DB0imv
027770130218     c                   eval(h)   w0112  =  DB0imv
027780130218     c                   evalr     Pimp   =  %editc(w0112 : 'K')
027790030515sel 2c                   select
027800030515w   2c                   when      DB0iva <> *zeros
027810030515     c                   eval      Pkie   = 'I.V.A.'
027820030624     c                   eval(h)   w0112  =  DB0iva
027830130218     c*//                   eval      Piva   =  '     ' +
027840130218     c*//                                       %editc(w0112 : 'K')
027850130218     c                   evalr     Piva   =  %editc(w0112 : 'K')
027860030515w   2c                   when         DB0cei <> *blanks
027870030515     c                             or DB0dce <> *blanks
027880030912     c                   eval      Pkie   = 'ESENZIONE'
027890030912     c                   eval      Piva   =  DB0dce
027900030515e   2c                   endsl
027910030515     c                   eval      Pktf   = 'TOTALE FATTURA'
027920130218     c*//                   eval(h)   Pift   =  DB0ift
027930130218     c                   eval(h)   w0112  =  DB0ift
027940130218     c                   evalr     Pift   =  %editc(w0112 : 'K')
027950030515if  2c                   if        DB0div <> §GEDcn
027960030515     c                   eval      Pdiv   =  DB0div
027970030515e   2c                   endif
027980030515e   1c                   endif
027990030515      *
028000030515      * - Contrassegno e relativa divisa
028010030515if  1c                   IF        DB0cas <> *zeros
028020030625      *
028030030515     c                   eval(h)   Pcas   =  DB0cas
028040030515if  2c                   if        DB0vca <> §GEDcn
028050030515     c                   eval      Pdca   =  DB0vca
028060030515e   2c                   endif
028070030515     c                   exsr      CtrCAs
028080030624      * - - Nuovo controllo abilitazione c/assegno
028090030624     c                   reset                   TRUL21DS
028100030624     c                   eval      I21cbo =  DB0cbo
028110030624     c                   eval      I21tsp =  DB0tsp
028120030624     c                   eval      I21lnp =  DB0lnp
028130030624     c                   eval      I21nzm =  DB0nzm
028140030624     c                   eval      I21lna =  DB0lna
028150030624     c                   eval      I21nzd =  DB0nzd
028160130826     c                   if        DB0ccm >  *zero
028170130826     c                   eval      I21ksc =  DB0ccm
028180130826     c                   else
028190130826     c                   eval      I21ksc =  (DB0lnp * 10000 ) + 8888
028200130826     c                   endif
028210030624     c                   eval      I21tic =  DB0tic
028220030624if  2c                   if        DB0vca <> §GEDcn
028230030624     c                   eval      I21imp =  Wcas
028240030624     c                   eval      I21div =  §GEDcn
028250030624x   2c                   else
028260030624     c                   eval      I21imp =  DB0cas
028270030624     c                   eval      I21div =  DB0vca
028280030624e   2c                   endif
028290030624     c                   eval      I21ute =  KNMUS
028300040607     c                   eval      I21pgm =  SDSpgm
028310030624     c                   call      'TRUL21R'
028320030624     c                   parm                    TRUL21ds
028330030624      * - - Carico la tabella "Tipo incasso C/Assegno" (1A)
028340030624     c                   exsr      Tab_1A
028350030624     c                   movel     §1Ades        DB0dti
028360030624if  2c                   if            §1Adeo <> *blanks
028370030624     c                             and O21fca <> *blanks
028380030624     c                   movel     §1Adeo        DB0dti
028390030624e   2c                   endif
028400100208      /free
028410100208           // - -?Reperimento intestatario c/assegno?
028420100322           if  §VPOcas <= wDate   and   DUTlpo  <> 'S';
028430100208             reset  trulINTds;
028440100208             iINTtic  = DB0tic;
028450100208             iINTrscM = DB0rsm;
028460100208             iINTrscO = DB0rmo;
028470100208             iINTcdi  = §AR5cdi;
028480100208             //if  §AR5cdi = *blank;
028490100208             //  iINTaas = DB0aas;
028500100208             //  iINTlnp = DB0lnp;
028510100208             //  iINTnrs = DB0nrs;
028520100208             //  iINTnsp = DB0nsp;
028530100208             //endif;
028540100208             trulINTr ( TRULINTds );
028550100322           endif;
028560100208      /end-free
028570030624      * - - Imposto i campi di stampa
028580030624     c                   eval      Ptic   =  DB0tic + ' ' + DB0dti
028590091119      /free
028600100208           // - - -?Intestatario assegno?
028610100322           if  DB0cas  <> *zero   and
028620100322              (§VPOcas <= wDate   and   DUTlpo  <> 'S')   and
028630100208               oINTerr = *blank;
028640100208             Pkic = 'INTESTARE A:';
028650100208             Pica = oINTrsci;
028660100208           endif;
028670091119      /end-free
028680030625      *
028690030515e   1c                   ENDIF
028700030515      *
028710030515      * - Totale da esigere al ritiro o alla consegna e relativa divisa
028720030515if  1c                   IF        DB0ift >  *zeros
028730030625      *
028740030515if  2c                   if            DB0vca <> §GEDcn
028750030515     c                             and DB0vca <> *blanks
028760030515     c                   eval      Pdie   =  DB0div
028770030515e   2c                   endif
028780030515      *   Controllo se C/Assegno da sommare al totale da esigere o no:
028790030515      *   se Franco-Prepagato il C/Assegno non si somma mai;
028800030515      *   se c'è il totale fattura: metto anche il il totale da esigere.
028810030515      *   (importi "arrotondati" a 2 decimali in stampa)
028820030515sel 2c                   Select
028830030515      *   Non ho C/Assegno o è Franco Prepagato
028840071008w   2c                   When          DB0cas  = *zeros
028850071008     c                             or  DB0tbo  = '8'
028860080701     c                   eval(h)   Pies   =  DB0ift
028870030625     c                   clear                   Pie1
028880030625     c                   clear                   Pdi1
028890030515      *   C/Assegno sommabile
028900030515w   2c                   When      DB0ste =  'S'    and $CAeur = *on
028910030515sel 3c                   select
028920030515      *   - c/assegno in EURO
028930030515w   3c                   when      DB0div =  §GEDcn and DB0vca =  §GEDcn
028940080701     c                   eval(h)   Pies   =  DB0ift + DB0cas
028950030624     c                   clear                   Pie1
028960030624     c                   clear                   Pdi1
028970030515      *   - c/assegno in DIVISA
028980030515w   3c                   when          DB0div =  §GEDcn
028990030515     c                             and DB0vca <> §GEDcn
029000080701     c                   eval(h)   Pies   =  DB0ift + Wcas
029010030624     c                   clear                   Pie1
029020030624     c                   clear                   Pdi1
029030030515e   3c                   endsl
029040030515      *   C/Assegno non sommabile
029050030515x   2c                   Other
029060080701     c                   eval(h)   Pie1   =  DB0cas
029070080701     c                   eval(h)   Pies   =  DB0ift
029080030515e   2c                   Endsl
029090030709      *
029100030709e   1c                   ENDIF
029110030515      *
029120030515      * - Numero e data fattura
029130030515if  1c                   if        DB0nft <> *zeros
029140030515     c                   eval      Pkft   =  'FATTURA   '
029150030515     c                   eval      Pnft   =  DB0nft
029160130124     c                   eval      Pfiv   = '(' + %editc(wB0fiv:'X') +
029170130124     c                                      ')'
029180030515if  2c                   if        DB0dft >  *zeros
029190030515     c                   eval      Pkdf   =  'DEL'
029200030515     c     *iso          movel     DB0dft        Data_EUR
029210130418     c     *dmy          movel     Data_EUR      Pdft
029220130418     c*//  *eur          movel     Data_EUR      Pdft
029230030515e   2c                   endif
029240030515e   1c                   endif
029250030515      *
029260030515      * Memorizzo il codice tariffa come numero di copie da stampare
029270030515      *   (per Rimesso Mittente - non per DPD o FEDEX)
029280030515if  1c                   if        DB0ris = 'M'  and  *in70
029290030515     c                   eval      Wctr   =  DB0ctr
029300030515e   1c                   endif
029310030515      *
029320030515     c                   ENDSR
029330170517      *
029340170517      /free
029350170517       //--------------------------------------------------------------
029360170517       //?Controllo SE ragione sociale Mittente da FORZARE (TFNTC) per ?
029370170517       //?spedizione DPD.                                              ?
029380170517       //--------------------------------------------------------------
029390170517       BEGSR  sr_Mitt_DPD;
029400170517
029410170517         // -?Reperim. abilitazioni del Mittente dalla tab. "CLI"?
029420170517         clear  k05tntbe01;
029430170517         k_TBEcod = 'CLI';
029440170517         k_TBEke1 = %editc( DB0ccm : 'X' );
029450170517         chain  %kds( k05tntbe01 )  TNTBE000;
029460170517         if  Not %found( TNTBE01L )  or  TBEatb <> *blank;
029470170517           leavesr;
029480170517         endif;
029490170517         dCLI = TBEuni;
029500170517         // -?Cliente SENZA forzatura Mittente da note "AM" e "AS"?
029510170517         if  §CLItfntc <> 'S'  and  §CLInoIMtt <> 'S';
029520170517           leavesr;
029530170517         endif;
029540170517
029550170517         // -?Forzatura Mittente da note "AM"?
029560170517         If  §CLItfntc = 'S';
029570170517
029580170517           if  NOT %open( TFNTC01L );
029590170517             open  TFNTC01L;
029600170517           endif;
029610170517           clear  keyTFNTC01;
029620170517           k_NTCapl = 'C';
029630170517           k_NTCnk1 = %editc( DUTkci : 'X' ) + %editc( DB0ccm : 'X' );
029640170517           k_NTCtnt = 'AM';
029650170517           chain  %kds( keyTFNTC01 )  TFNTC;
029660170517           if  %found( TFNTC01L )  and  NTCsns = 'S';
029670170517             DB0rsm = NTCrnt;
029680170517           endif;
029690170517
029700170517         EndIf;
029710170517
029720170517         // -?Pulizia Indirizzo Mittente?
029730170517         If  §CLInoIMtt = 'S';
029740170517
029750170517           clear  DB0inm;
029760170517           clear  DB0cam;
029770170517           clear  DB0lom;
029780170517           clear  DB0prm;
029790170517           clear  DB0nzm;
029800170517
029810170517         EndIf;
029820170517
029830170517       ENDSR;
029840170517
029850170517      /end-free
029860030515      *
029870030515      *---------------------------------------------------------------*
029880050217      *?Inschieramento delle Note e delle Particolarità              ?*
029890030515      *---------------------------------------------------------------*
029900030515     c     SchPARTIC     BEGSR
029910030515      *
029920030515     c*** (già fatto:)   reset                   $Partic
029930030710     c*** (già fatto:)   reset                   $NoDesPartic
029940030515     c*** (già fatto:)   clear                   $NP
029950030717     c*** (già fatto:)   clear                   W_CoOr
029960030515     c                   clear                   XX
029970030710      *
029980030710      * Imposto eventuali note per prime
029990030710if  1c                   if        DB0nt1 <> *blanks
030000030710     c                   add       1             xx
030010030710     c                   movel     DB0nt1        $NP(xx)
030020030710e   1c                   endif
030030030710if  1c                   if        DB0nt2 <> *blanks
030040030710     c                   add       1             xx
030050030710     c                   movel     DB0nt2        $NP(xx)
030060030710e   1c                   endif
030070030515      *
030080030515      * Le particolarità possono essere lunghe fino a 45 char.
030090030515      *   Se i primi 30 (della 1ª riga a disposizione) non dovessero
030100030515      *   bastare, il resto lo si mette nella riga successiva.
030110030710      *   Se, invece, non risultano decodificate, si mettono solo i
030120030710      *   codici sulla stessa riga.
030130030710      *   Se uno solo risulta decodificato, esso va su una riga a
030140030710      *   parte (vedi flag "$NoDesPartic": *off = serve riga nuova
030150030710      *                                    *on  = ok riga procedente)
030160030515      *
030170030515      * . . Particolarità Giacenza
030180030515if  1c                   IF        DB0gga <> *blanks
030190030515     c                   movel     'Q'           WTpPar
030200030515     c                   movel     DB0gga        WCdPar
030210030515     c                   exsr      TABxPAR
030220030710if  2c                   if            TBLuni       = *blanks
030230030710     c                             and $NoDesPartic = *on
030240030710     c                   eval      $NP(xx) = %trim($NP(xx))
030250030710     c                                     + '   ' + WCdPar
030260030710x   2c                   else
030270030710if  3c                   if        TBLuni       = *blanks
030280030710     c                   eval      $NoDesPartic = *on
030290030710x   3c                   else
030300030710     c                   eval      $NoDesPartic = *off
030310030710e   3c                   endif
030320030515     c                   add       1             xx
030330100513     c                   movel     w053a         $NP(xx)
030340100513if  3c                   if        %subst(w053a : %len($NP(xx))+1)
030350030515     c                              <> *blanks
030360030515     c                   add       1             xx
030370030515     c                   eval      $NP(xx) = '   ' +
030380100513     c                             %trim(%subst(w053a:%len($NP(xx))+1))
030390030710e   3c                   endif
030400030515e   2c                   endif
030410030515e   1c                   ENDIF
030420030515      *
030430030515      * . . Particolarità Consegna
030440030515if  1c                   IF        DB0gma <> *blanks
030450030515     c                   movel     'R'           WTpPar
030460030515     c                   movel     DB0gma        WCdPar
030470030515     c                   exsr      TABxPAR
030480030710if  2c                   if            TBLuni       = *blanks
030490030710     c                             and $NoDesPartic = *on
030500030710     c                   eval      $NP(xx) = %trim($NP(xx))
030510030710     c                                     + '   ' + WCdPar
030520030710x   2c                   else
030530030710if  3c                   if        TBLuni       = *blanks
030540030710     c                   eval      $NoDesPartic = *on
030550030710x   3c                   else
030560030710     c                   eval      $NoDesPartic = *off
030570030710e   3c                   endif
030580030515     c                   add       1             xx
030590100513     c                   movel     w053a         $NP(xx)
030600100513if  3c                   if        %subst(w053a : %len($NP(xx)) + 1)
030610030515     c                              <> *blanks
030620030515     c                   add       1             xx
030630030515     c                   eval      $NP(xx) = '   ' +
030640100513     c                             %trim(%subst(w053a:%len($NP(xx))+1))
030650030710e   3c                   endif
030660030515e   2c                   endif
030670030515e   1c                   ENDIF
030680030515      *
030690030515      * . . Particolarità Varie
030700030717      *     - questa descrizione va messa nell'apposito campo PPAD
030710041015      *       se è "PRIVATO" idem se è "CONSEGNA AD UFFICI"
030720150224     *** *     - se 'B' (bancali da rendere al mittente) comparirà un
030730150224     *** *       apposito box
030740150224     *** *     - se 'O' (oltre 20 colli => bancali da riportare in mag.)
030750150224     *** *       comparirà un apposito box, un'apposita nota e l'indica-
030760150224     *** *       zione nella copia per il destinatario.
030770030717      *     ...Potrebbero anche non esserci più elementi di schiera
030780030717      *     disponibili [xx >= %elem($NP)] !
030790030709if  1c                   IF            DB0gva <> *blanks
030800150224     c*//                          and DB0gva <> 'B '
030810150224     c*//                          and DB0gva <> 'O '
030820030709     c                             and DB0gva <> 'P '
030830041015     c                             and DB0gva <> 'U '
030840030515     c                   movel     'S'           WTpPar
030850030515     c                   movel     DB0gva        WCdPar
030860030515     c                   exsr      TABxPAR
030870030710if  2c                   if            TBLuni       = *blanks
030880030710     c                             and $NoDesPartic = *on
030890030710     c                   eval      $NP(xx) = %trim($NP(xx))
030900030710     c                                     + '   ' + WCdPar
030910030710x   2c                   else
030920030710if  3c                   if        TBLuni       = *blanks
030930030710     c                   eval      $NoDesPartic = *on
030940030710x   3c                   else
030950030710     c                   eval      $NoDesPartic = *off
030960030710e   3c                   endif
030970030717if  3c                   if            xx < %elem($NP)
030980030709     c                   add       1             xx
030990100513     c                   movel     w053a         $NP(xx)
031000100513if  4c                   if        %subst(w053a : %len($NP(xx)) + 1)
031010030709     c                              <> *blanks
031020030717     c                             and xx < %elem($NP)
031030030709     c                   add       1             xx
031040030709     c                   eval      $NP(xx) = '   ' +
031050100513     c                             %trim(%subst(w053a:%len($NP(xx))+1))
031060030717e   4c                   endif
031070030717e   3c                   endif
031080030709e   2c                   endif
031090030515e   1c                   ENDIF
031100030717      *
031110030717      * Imposto il numero originale di colli componenti la spedizione
031120030717      * e se ho ancora spazio lo memorizzo in "Note e Particolarità"
031130030717if  1c                   if        %subst(DB0gva:1:1) = 'O'
031140030717     c                   exsr      RepAR5bnb
031150030717if  2c                   if        W_CoOr  <> *blanks
031160030717     c                             and xx  <  %elem($NP)
031170030717     c                   add       1             xx
031180030717     c                   eval      $NP(xx) =  W_CoOr
031190030717e   2c                   endif
031200030717e   1c                   endif
031210030515      *
031220030515      * Imposto il flag per memorizzare la già avvenuta memorizzazione
031230030715      *   delle "Note e Particolarità"
031240030515     c                   eval      $Partic = *on
031250030515      *
031260030515     c                   ENDSR
031270070319      *
031280070319      *---------------------------------------------------------------*
031290070319      *?Inschieramento delle Consegne Parziali precedenti            ?*
031300070319      *---------------------------------------------------------------*
031310070319     c     SchConsParz   BEGSR
031320070319      *
031330070319     c*** (già fatto:)   reset                   $CP
031340070319     c*** (già fatto:)   reset                   $CPdcm
031350070319     c                   clear                   XX
031360070319      *
031370070319if  2c                   if        NOT %open(FNART01L)
031380070319     c                   open      FNART01L
031390070319e   2c                   endif
031400070319      *
031410070514if  2c                   if        $Mamma = *off
031420070319     c     KLDV          setll     FNART000
031430070319     c     KLDV          reade     FNART000
031440070514x   2c                   else
031450070514     c     KLBL          setll     FNART000
031460070514     c     KLBL          reade     FNART000
031470070514e   2c                   endif
031480070319      *
031490070319do  1c                   DOW       NOT  %eof(FNART01L)
031500070319      *
031510070319if  2c                   If             ARTatb = *blanks
031520070319     c                             and  ARTdcm > *zeros
031530070319      *
031540070319     c                   eval      *in31  = *off
031550070319     c                   eval      xx     = 1
031560070319     c     ARTdcm        lookup    $CPdcm(xx)                             31
031570070319if  3c                   if        NOT  *in31
031580070319     c                   eval      xx     = 1
031590070319     c     *zeros        lookup    $CPdcm(xx)                             31
031600070319if  4c                   if        *in31
031610140304     c*//                eval      $CP(xx)     = *zeros
031620140304     c                   clear                   Elem_CP
031630140304     c                   movel     Elem_CP       $CP(xx)
031640070319e   4c                   endif
031650070319e   3c                   endif
031660070319if  3c                   if        *in31
031670070319     c                   eval      $CPdcm(xx)  = ARTdcm
031680070319     c                   movel     $CP(xx)       Elem_CP
031690070319     c                   eval      WCPdcm      = ARTdcm
031700070319     c                   eval      WCPncl      = WCPncl + 1
031710070319     c                   movel     Elem_CP       $CP(xx)
031720070319e   3c                   endif
031730070319      *
031740070319e   2c                   EndIf
031750070319      *
031760070514if  2c                   if        $Mamma = *off
031770070319     c     KLDV          reade     FNART000
031780070514x   2c                   else
031790070514     c     KLBL          reade     FNART000
031800070514e   2c                   endif
031810070319      *
031820070319e   1c                   ENDDO
031830070319      *
031840140513if  0c                   if        xx     > *zero
031850140513      * Reperimento nominativi firmatari
031860140513     c                   exsr      sr_Firmatario
031870070319      * Ordinamento schiera per data
031880070319if  1c                   if        xx     > 1
031890070319     c                   sorta                   $CP
031900070319e   1c                   endif
031910140513e   0c                   endif
031920070319      *
031930070319     c                   ENDSR
031940140303      *
031950140303      /free
031960140303
031970140303       //--------------------------------------------------------------
031980140303       //?Reperimento nome firmatario                                  ?
031990140303       //--------------------------------------------------------------
032000140303       BEGSR  sr_Firmatario;
032010140303
032020140303if  1    if  NOT  %open(FNEVB22L);
032030140303           open  FNEVB22L;
032040140303e   1    endif;
032050140303
032060140303         clear  keyFNEVB22;
032070140303         k_EVB2aas = DB0aas;
032080140303         k_EVB2lnp = DB0lnp;
032090140303         k_EVB2nrs = DB0nrs;
032100140303         k_EVB2nsp = DB0nsp;
032110140303         k_EVB2cev = 'P  ';
032120140303         setll  %kds( keyFNEVB22 : 4 )  FNEVB200;
032130140303         reade  %kds( keyFNEVB22 : 4 )  FNEVB200;
032140140303         DoW  Not  %eof(FNEVB22L);
032150140303           if  EVB2cev = k_EVB2cev  and  EVB2trd = 'FIR';
032160140305             yy = %lookup( EVB2dev : $CPdcm );
032170140305             if  yy > *zero;
032180140305               Elem_CP = $CP(yy);
032190140303               if  WCPocm  <= EVB2hev;
032200140303                 WCPocm  = EVB2hev;
032210140303                 WCPfir  = EVB2dati;
032220140305                 $CP(yy) = Elem_CP;
032230140303               endif;
032240140303             endif;
032250140303           endif;
032260140304           reade  %kds( keyFNEVB22 : 4 )  FNEVB200;
032270140303         EndDo;
032280140303
032290140303       ENDSR;
032300140303
032310140303      /end-free
032320031124      *
032330031124      *---------------------------------------------------------------*
032340050217      *?Reperimento Nominativo e Telefono destinatario da FIAR5/GEN  ?*
032350031124      *---------------------------------------------------------------*
032360031124     c     RepAR5gen     BEGSR
032370031124      *
032380031124     c                   clear                   dAR5gen
032390031124      *
032400031124     c                   eval      AR5trd = 'GEN'
032410040301if  1c                   if        DUTlpo = 'S'
032420040531     c     K05AR501      chain     FIAR531C
032430031127x   1c                   else
032440031127     c     K05AR501      chain     FIAR5000
032450031127e   1c                   endif
032460031124      *
032470040301if  1c                   if           (DUTlpo =  'S'
032480040531     c                             and %found(FIAR531C)
032490040301     c                             OR  DUTlpo <> 'S'
032500040301     c                             and %found(FIAR501L))
032510040301     c                             and AR5atb =  *blanks
032520031124     c                   movel     AR5uni        dAR5gen
032530031124e   1c                   endif
032540031124      *
032550031124     c                   ENDSR
032560030717      *
032570030717      *---------------------------------------------------------------*
032580050217      *?Reperimento Numero Colli Originale da FIAR5/BNB              ?*
032590030717      *---------------------------------------------------------------*
032600030717     c     RepAR5bnb     BEGSR
032610030717      *
032620030717     c                   clear                   dAR5bnb
032630030717     c*** (già fatto:)   clear                   W_CoOr
032640030717      *
032650030717     c                   eval      AR5trd = 'BNB'
032660040301if  1c                   if        DUTlpo = 'S'
032670040531     c     K05AR501      chain     FIAR531C
032680040301x   1c                   else
032690030717     c     K05AR501      chain     FIAR5000
032700040301e   1c                   endif
032710030717      *
032720040301if  1c                   if           (DUTlpo =  'S'
032730040531     c                             and %found(FIAR531C)
032740040301     c                             OR  DUTlpo <> 'S'
032750040301     c                             and %found(FIAR501L))
032760040301     c                             and AR5atb =  *blanks
032770030717     c                   movel     AR5uni        dAR5bnb
032780030717if  2c                   if        §AR5bcor <> *zeros
032790030717     c                   eval      W_CoOr   =  C_CoOr
032800030717     c                                      +  %editc(§AR5bcor : 'K')
032810030717e   2c                   endif
032820030717e   1c                   endif
032830030717      *
032840030717     c                   ENDSR
032850030515      *
032860030515      *---------------------------------------------------------------*
032870051129      *?Decodifiche condizionate in Lettera di Vettura               ?*
032880030515      *---------------------------------------------------------------*
032890040607     c     DecodLDV      BEGSR
032900030515      *
032910030515      * Richiesta firma per ricevuta della merce senza riserve
032920030515      * (e NON  quella  per quietanza) => *in35 + not *in36
032930030515     c                   movea     '10'          *in(35)
032940101130      *
032950101130      /free
032960101130
032970101130         // -?Memorizzazione consegne parziali precedenti dal file?
032980101130         //  ?FNART00F - serve anche nella stampa delle "tagliatelle"?
032990140513         IF  $ConsParz;
033000140513
033010140513           // -?SE si stanno stampando "tagliatelle":?
033020140514           //  ?le consegne parziali vengono indicate nel Box1,?
033030140514           //  ?SE?ne sono state reperite?
033040140513           IF  DB0fa4 = '4'  and  $CP(1) < *all'9';
033050140513
033060101130             // -?Prima si totalizzano le quantità consegnate?
033070140513             clear  w0080;
033080140513             clear  w0140;
033090101130             For  xx = 1  To  %elem($CP);
033100101130               if  $CP(xx) < *all'9';
033110101130                 Elem_CP = $CP(xx);
033120101130                 monitor;
033130101130                   Data_Eur = %date( WCPdcm : *iso );
033140101130                 on-error;
033150101130                   Data_Eur = *loval;
033160101130                 endmon;
033170101130                 if       Data_Eur  > *loval  and
033180101130                     %dec(Data_Eur) > w0080;
033190101130                   w0080 = %dec(Data_Eur);
033200101130                 endif;
033210101130                 w0140 += WCPncl;
033220101130               endif;
033230101130             EndFor;
033240101130             // -?Poi si stampa un'unica indicazione?
033250101201             Data_DMY = %date( w0080 : *eur );
033260101201             Pcpp = 'Consegnati '
033270101201                  + %triml( %editc( w0140 : 'Z' ) )
033280101201                  + ' colli al '
033290101201                  + %triml( %editc( %dec( Data_DMY ) : 'Y' ) );
033300140513
033310140513           ENDIF;
033320140513
033330140513         ENDIF;
033340101130
033350101130      /end-free
033360070307      *
033370070307      * Intestazione moduli
033380070307      * - Se spedizione in stato "Lasciato Avviso"
033390070307      *   si stampano anche data/ora del 1° lasciato avviso
033400070307if  1c                   if        $LascAvv  = *on
033410131205     c                   z-add     wEVBdev       w0080
033420070315     c     *iso          test(d)                 w0080                  31
033430070315if  2c                   if        NOT *in31
033440071025     c     *iso          movel     w0080         Data_EUR
033450071025     c     *eur          movel     Data_EUR      w0080
033460070315x   2c                   else
033470070315     c                   clear                   w0080
033480070315e   2c                   endif
033490070307     c                   eval      Tla    =  'Lasciato avviso il '
033500070307     c                                    +  %editc(w0080 : 'Y')
033510070307     c                                    +  ' alle '
033520131205     c                                    +  %editw(wEVBhev : '  :  ')
033530070307e   1c                   endif
033540030709      *
033550100107     *** * Evidenziazione servizio "espresso"
033560100107     *** * o del servizio "Poste italiane"
033570100107     ***c                   eval      *in37 = (DB0tsp = 'E')
033580100107     ***c  n37              eval      *in37 = (§OGntw = 'PPT')
033590090611      *
033600090605      /free
033610100107
033620101130         // -?Evidenziazione servizio "espresso"?
033630101130         //           ?o del servizio "Poste Italiane"?
033640100107         *in37 = (DB0tsp = 'E'   or   DB0tsp = 'H'   or
033650100107                  §OGntw = 'PPT');
033660100107
033670101130         // -?Consegna al mattino (tempi consegna = 12 ore)?
033680101130         //  ?(NON per Würth)?
033690090615         if  §VPOrcm <= wDate and
033700090622             DUTlpo  <> 'S'   and
033710090615             DB0tsp   = 'E'   and
033720090615            (DB0nrs  <> 40    or  (DB0lnp <> 043   and
033730110429                                   DB0lnp <> 277   and
033740090605                                   DB0lnp <> 063));
033750090605           clear  TNSD99ds;
033760090605           if  DB0tbo = 'P'   or   DB0tbo = '8';
033770090605             D98tbo = 'P';
033780090605           else;
033790090605             D98tbo = 'A';
033800090605           endif;
033810090605           D98aas = DB0aas;
033820090605           D98lnp = DB0lnp;
033830090605           D98nrs = DB0nrs;
033840090605           D98nsp = DB0nsp;
033850090605           tnsd99r ( TNSD99ds );
033860120910           //*if  D98tci = 12;
033870120910           if  D98tcc = 12;
033880090605             Pdsm = 'M';
033890090605           endif;
033900090605         endif;
033910090605      /end-free
033920030515      *
033930030515      * Franco prepagato
033940030515      * (richiede firma per quietanza nella copia x destinatario)
033950030515if  1c                   if        DB0tbo =  '8'  and  DB0ris <> 'M'
033960030515     c                   movea     '01'          *in(35)
033970030515if  2c                   if        Pds1   =  *blanks
033980030515     c                   eval      Pds1   =  'FRANCO PREPAGATO        '
033990030515e   2c                   endif
034000120530     c*//                if        (DB0dft > *zero       and
034010120530     c*//                           DB0dft > §vpoBRTdta)  OR
034020120530     c*//                          (DB0dft = *zero       and
034030120530     c*//                           Wdsp   > §vpoBRTdta)
034040130418     c                   eval      Pbrt2  =  $BRT
034050120530     c*//                else
034060130418     c*//                eval      Pbrt2  =  $Bartolini
034070120530     c*//                endif
034080030515      * - Legge 450 (solo per prepagati)
034090030515if  2c                   if            DB0ias =  *zeros
034100030515     c                             and DB0vas =  *blanks
034110030515     c                             and DB0div =  §GEDcn
034120060301     c                             and §GElrp >  *zeros
034130030515     c*** v. IF sovrastante...     and DB0tbo =  '8'
034140060301if  3c                   if        Wdsp   >  20060228
034150110315     c                   z-add     §GElrp        w0112
034160080701     c                   eval      Prcv   =  'Art. 1696 c.c. mod. +
034170061102     c                                       dlgs 286/2005 '
034180060301     c                                    +  §GEDcn + ' '
034190060301     c                                    +  %trim(%editc(w0112 : '4'))
034200060301     c                                    +  ' al kg.'
034210060301x   3c                   else
034220110315     c                   z-add     6.20          w0112
034230080701     c                   eval      Prcv   =  'Legge 450/85 '
034240030515     c                                    +  §GEDcn + ' '
034250030624     c                                    +  %trim(%editc(w0112 : '4'))
034260030515     c                                    +  ' al kg.'
034270060301e   3c                   endif
034280030515e   2c                   endif
034290030515e   1c                   endif
034300031031      *
034310101125      /free
034320121017
034330121017         // -?Indicazione della Partiva IVA o del Cod. Fiscale?
034340121017         //  ?(se Porto Franco Prepagato?
034350121017         //    ?o Porto Assegnato con Fattura immediata)?
034360121017         //  ?- NON se mittente estero -?
034370121017             // -?con fattura?
034380121017         IF  (DB0nft <> *zero  or   DB0ift <> *zero)   AND     (
034390121017             // - -?P.F. prepagato?
034400121017             //    ?(con mittente italiano)?
034410121017             (DB0tbo =  '8'             and  DB0nzm  = *blank)  or
034420121017             // - -?P.A. (con fattura immediata)?
034430121017             //    ?(con destinatario italiano)?
034440121017             (%subst(DB0tb1:1:1) = 'A'  and  DB0nzd  = *blank)  or
034450121017             // - -?con 2ª bolla?
034460121017             //    ?(con destinatario italiano)?
034470121017             (DB0tb2 <> *blank          and  DB0tbo <> '8'
034480121029                                        and  DB0tbo <> 'P'
034490121017                                        and  DB0nzd  = *blank) );
034500121017           select;
034510121017             when  Not $No_PI  and  wB0piv <> *blank  and
034520121017                   // ·?Esiste Partita Iva NON FITTIZIA?
034530121026                   %scan('$$' : wB0piv) = *zero;
034540121017                   // ·?Non è un C.F. - lungo 16 (privato)?
034550121026                   //and  %len( %trim( wB0cfi ) ) < 16;
034560121017               Ppiv = 'Partita Iva: ' + wB0piv;
034570121017             when  Not $No_CF  and  wB0cfi <> *blank;
034580121017               Ppiv = 'Codice Fiscale: ' + wB0cfi;
034590121017           endsl;
034600130418           //select;
034610130418           //  // ·?Se P.F. Prepagato il C.F./P.I. è del Mittente?
034620130418           //  when  DB0tbo = '8';
034630130418           //    PpivM = Ppiv;
034640130418           //  // ·?Se P.A. con fattura immed. il C.F./P.I. è del Dest.?
034650130418           //  when  %subst(DB0tb1:1:1) = 'A';
034660130418           //    PpivD = Ppiv;
034670130418           //  // ·?Se 2ª bolla (DDT sì) è del Dest.?
034680130418           //  when  DB0tb2  <> *blanks  and  DB0tbo  <> 'P';
034690130418           //    PpivD = Ppiv;
034700130418           //endsl;
034710121017         ENDIF;
034720101125
034730101125         Select;
034740101125
034750101125           // -?Non utilizzare per la consegna?
034760101125           //  ?(stampa della sola copia per il vettore)?
034770101125           When  $No_x_Cons = *on;
034780101125             *in35 = *off;
034790101125             *in36 = *off;
034800101125             // -?costante da stampare se LDV recapito c/assegni?
034810101125             //  ?stampata dal pgm "Stampa DDT NO in arrivo"?
034820101129             Pnux  = 'NON UTILIZZARE PER LA CONSEGNA';
034830101125
034840101125           // -?Ritorno all'Incasso?
034850101125           When  $Rit_a_Inc = *on;
034860101125             *in40 = *on;
034870101125             Pnux  = 'RITORNO ALL''INCASSO';
034880101125
034890101125         EndSl;
034900101125
034910101125      /end-free
034920030910      *
034930030910      * Bollo assolto in modo virtuale
034940030910if  1c                   if        $BolloVirt =  *on
034950060719if  2c                   if        DB0dft <  20060801
034960080701     c                   eval      Pbav1  =  'Bollo '
034970080701     c                                    +  %trim(%editc(wBolloVirt:'4'))
034980080701     c                                    +  ' ' + DB0div
034990080701     c                                    +  ' assolto in modo virtuale '
035000080701     c                   eval      Pbav2  =  'Aut. Ag. Ent. BO 3 27/09/02 '
035010080701     c                                    +  'Prot. 31599'
035020060719x   2c                   else
035030080701     c                   eval      Pbav1  =  'Bollo '
035040080701     c                                    +  %trim(%editc(wBolloVirt:'4'))
035050080701     c                                    +  ' ' + DB0div
035060080701     c                                    +  ' assolto in modo virtuale '
035070080701     c                   eval      Pbav2  =  'Aut. Ag. Ent. MI 2 10/07/06 '
035080080701     c                                    +  'Prot. 45114'
035090060719e   2c                   endif
035100030910e   1c                   endif
035110050503      *
035120080605      * Costanti "PER RICEVUTA DELLA MERCE..."
035130080605     c                   eval      Pkrm1 = '(nome e cognome in stampa+
035140080605     c                                       tello)'
035150080605     c                   eval      Pkrm2 = '(firma)'
035160080605     c                   eval      Pkrm3 = '(data e ora)'
035170050504sel 1c                   select
035180050504      * - danon?stampare se
035190050504      *   · franco prepagato                (NOT *in35)
035200050504      *   · non utilizzare per la consegna  (NOT *in35)
035210101125      *   · ritorno all'incasso             (*in40)
035220050504      *   · bolla FedEx                     (*in72)
035230101125w   1c                   when      NOT *in35  or  *in40  or  *in72
035240050504     c*** (già fatto:)   clear                   Pkrm
035250080605     c                   clear                   Pkrm1
035260080605     c                   clear                   Pkrm2
035270080605     c                   clear                   Pkrm3
035280080922      * - costante "DPD"
035290120301w   1c                   when      *in71
035300080922     c                   eval      Pkrm   =  'PER RICEVUTA DELLA MERCE-
035310080922     c                              SENZA RISERVE'
035320120530      * - costante "standard" per "DDT sì"
035330090615w   1c                   when          §VPOrsr <= wDate
035340081119     c                             and  DB0bam =  'S'
035350071005     c                   eval      Pkrm   =  'PER RICEVUTA DELLA MERCE-
035360071005     c                              E DEL DOCUMENTO DI TRASPORTO SENZA-
035370071005     c                              RISERVE'
035380120530      * - costante "standard" per "DDT no" + Packing-List
035390120301w   1c                   when           DB0bam =  *blank
035400161012     c                             and ($FIAR8 =  *on
035410161012     c                              or  $§PDF  =  *on)
035420120301     c                   eval      Pkrm   =  'PER RICEVUTA DELLA MERCE-
035430120301     c                              E DEL DOCUMENTO DI TRASPORTO SENZA-
035440120301     c                              RISERVE'
035450120301      * - costante "standard" altrimenti
035460050504x   1c                   other
035470050504     c                   eval      Pkrm   =  'PER RICEVUTA DELLA MERCE-
035480050504     c                              SENZA RISERVE'
035490050504e   1c                   endsl
035500090513      /free
035510090611       Select;
035520101130         // -?costante per cliente?
035530101130         //  ?"Consorzio Lotterie Nazionali - Gratta & Vinci"?
035540140513         when  DB0ccm = c_CliCLN  or  DB0ccm = c_CliLN_2;
035550090528           exsr  sr_RepDdtCLN;
035560090706           Pkrm =  'LA FIRMA PER RICEVUTA DELLA MERCE VALE ANCHE PER +
035570090706                    IL DDT N° '
035580090527                + %editc (WddtCLN : 'Z')
035590090615                + ' DEL '
035600090527                + %editc (WdspCLN : 'Y')
035610101001                + ' CON RISERVA DI CONTROLLO COME DA CONTRATTO';
035620101001           if  Wdsp < 20101001;
035630101001             Pkrm = %trimr(Pkrm) + ' C.L.N.';
035640101001           else;
035650101001             Pkrm = %trimr(Pkrm) + ' L.N. S.R.L.';
035660101001           endif;
035670101130         // -?costante per cliente?
035680101130         //  ?"S G Soluzioni Logistiche"?
035690091117         when  DB0ccm = c_CliSG   or   DB0ccm = c_CliSG_2;
035700091117           if  DB0nas <> *blank   and   DB0nas <> c_NoContr;
035710091117             Pkrm = %trimr(Pkrm)
035720091117                  + '.   INCLUDE CONTRATTO - '
035730091117                  + %trim(DB0nas);
035740091117           endif;
035750091117           if  DB0rmo <> *blank;
035760091117             if  DB0nas <> *blank;
035770091117               Pkrm = %trimr(Pkrm)
035780091117                    + ' - MATR TV: '
035790091117                    + %trim(DB0rmo);
035800091117             else;
035810091117               Pkrm = %trimr(Pkrm)
035820091117                    + '.   MATR TV: '
035830091117                    + %trim(DB0rmo);
035840091117             endif;
035850091117           endif;
035860090611       EndSl;
035870090513      /end-free
035880030717      *
035890130418      *//* Indicazione degli "oltre 20 colli"
035900130418      *//* (MAI se Franco prepagato)
035910130418if  1c*//                   if            DB0tbo <> '8'
035920130418     c*//                             and *in36  =  *off
035930130418     c*//                             and W_CoOr <> *blanks
035940130418     c*//                   eval      Pbrt2  = W_CoOr
035950130418e   1c*//                   endif
035960030515      *
035970030515      * Verifico se imponibile/iva/totale/estremi fattura
035980030515     c                   eval      *in19  = ((DB0imv <> *zeros)  or
035990030515     c                                       (DB0iva <> *zeros)  or
036000030515     c                                       (DB0cei <> *blanks) or
036010030515     c                                       (DB0dce <> *blanks) or
036020030515     c                                       (DB0ift <> *zeros)  or
036030030515     c                                       (DB0nft <> *zeros)  or
036040031125     c                                       (DB0dft <> *zeros))
036050030515      *
036060030515      * Verifico se contrassegno
036070030515     c                   eval      *in20  =  (Pcas <> *zeros)
036080030515      *
036090030709      * Verifico se totale da esigere al ritiro o alla consegna
036100080701     c                   eval      *in21  =  (Pies <> *zeros)
036110030709      *
036120030710      * Intesto il 2° modulo diversamente per prepagato
036130030710      * (solo in partenza)
036140030710if  1c                   if        *in21  and  *in36
036150030710     c                             and   (DB0tbo = 'P'
036160030710     c                                or  DB0tbo = '8')
036170030710     c                   eval      Tcxd   =  'COPIA PER IL MITTENTE    '
036180130726     c                   clear                   Tcxd2
036190030710e   1c                   endif
036200030515      *
036210120530      * Verifico se codice a barre BRT da stampare
036220030515     c                   eval      *in50  =  (Tbcd <> *blanks)
036230040629      * Verifico se codice a barre O.R.M.    da stampare
036240040629      * (non è ancora impostato - vedi subr. "StampaRCC")
036250040629     c***                eval      *in52  =  (Pbco <> *blanks)
036260030515      *
036270030515      * Flag "*" > x Consegne particolari
036280030515     c                   if        DB0tc1 =  'A'  or  DB0tc2 = 'A'
036290030515     c                   eval      Past   =  '*'
036300030515     c                   endif
036310030515      *
036320030515      * C/ASSEGNO - - - - - - - - - - - - - - - - - - - - - - - - - - *
036330030515if  1c                   IF        DB0cas <> *zeros
036340030515      * - Flag "*" > x C/Assegno
036350030904if  2c                   if        O21fca <> *blanks
036360030904     c                   movel     '*'           Past
036370030515e   2c                   endif
036380030624      * - Imposto i campi di stampa
036390030624     c                   eval      Ptic   =  DB0tic + ' ' + DB0dti
036400030515e   1c                   ENDIF
036410030515      *
036420040607      * TIPO LDV  - - - - - - - - - - - - - - - - - - - - - - - - - - *
036430030515      * - Se Bolla in Assegnato: stampo una "A" vicino all'asterisco
036440030515      * e se Bolla in Assegnato Senza Fattura: stampo "SEGUE FATTURA"
036450030515if  1c                   if           %subst(DB0tb1:1:1) = 'A'
036460030515     c                             or        DB0tb2     <> *blanks
036470030515     c                   eval      Pass   =  'A'
036480030515if  2c                   if        DB0nft =  *zeros
036490031125     c                   eval      *in22  =  *on
036500130215     c                   eval      Piva   =  'SEGUE FATTURA'
036510030515e   2c                   endif
036520030515e   1c                   endif
036530030515      *
036540030515      * DECODIFICA TIPO CONSEGNA RICHIESTA  - - - - - - - - - - - - - *
036550051129sel 1***c                   Select
036560051129w   1***c                   WHEN      DB0tcr = *blanks
036570051129     *** * - giorno si
036580051129w   1***c                   WHEN      DB0dcr <> *zeros  and  DB0tcr = 'P'
036590051129     ***c                   eval      Ptdr   =  'PRIMA DEL  '
036600051129w   1***c                   WHEN      DB0dcr <> *zeros  and  DB0tcr = 'D'
036610051129     ***c                   eval      Ptdr   =  'DOPO IL    '
036620051129     *** * - giorno no  ora si
036630051129w   1***c                   WHEN      DB0dcr =  *zeros  and  DB0tcr = 'P'
036640051129     ***c                   eval      Ptdr   =  'PRIMA DELLE'
036650051129w   1***c                   WHEN      DB0dcr =  *zeros  and  DB0tcr = 'D'
036660051129     ***c                   eval      Ptdr   =  'DOPO LE    '
036670051129e   1***c                   ENDSL
036680051129if  1c                   IF            DB0dcr >  *zeros
036690051129     c                             or  DB0hcr >  *zeros
036700051129     c                   clear                   FNLV80ds
036710051129     c                   eval      ILV80aas   =  DB0aas
036720051129     c                   eval      ILV80lnp   =  DB0lnp
036730051129     c                   eval      ILV80nrs   =  DB0nrs
036740051129     c                   eval      ILV80nsp   =  DB0nsp
036750051129     c                   eval      ILV80mgs   =  DB0mgs
036760051129     c                   eval      ILV80tc1   =  DB0tc1
036770051129     c                   eval      ILV80tc2   =  DB0tc2
036780051129     c                   eval      ILV80dcr   =  DB0dcr
036790051129     c                   eval      ILV80hcr   =  DB0hcr
036800051129     c                   eval      ILV80tcr   =  DB0tcr
036810051129     c                   call      'FNLV80R'
036820051129     c                   parm                    KPJBA
036830051129     c                   parm                    FNLV80ds
036840051129sel 2c                   Select
036850051129      * errore
036860051129w   2c                   When      OLV80err  <>  *blanks
036870051129      * consegna tassativa (per ora solo ai supermercati)
036880051129w   2c                   When      OLV80tat   =  'S'
036890051129     c                   movel     OLV80tcrd     Pcot
036900091116      * NO "Prima del" per il cliente TTL
036910091116w   2c                   When      DB0ccm     = c_CliTTL   and
036920091116     c                             DB0dcr    <> *zeros     and  DB0tcr = 'P'
036930051129      * prima/dopo il/delle
036940051129      * (il giorno e/o l'ora di consegna richiesta sono già impostati
036950051129      *  nella subr. "TipoLDV")
036960051129w   2c                   Other
036970051129     c                   movel     OLV80tcrd     Ptdr
036980051129e   2c                   Endsl
036990051129e   1c                   ENDIF
037000030515      *
037010030515      * DECODIFICA DEI TURNI DI CHIUSURA  - - - - - - - - - - - - - - *
037020060921      * (SE?non?è "Fermo Deposito")
037030060921if  1c                   if        DB0ffd =  *blanks
037040060921     c                   eval      Pktc   =  'TURNO DI CHIUSURA'
037050060921e   1c                   endif
037060030515      * 1° turno di chiusura
037070060921if  1c                   IF             DB0gc1 <> *blanks
037080060921     c                             and  DB0ffd  = *blanks
037090030515      * - Flag "*" > Turno di chiusura
037100030515     c                   eval      Past   =  '*'
037110030515if  2c                   If        %subst(DB0gc1:1:1) = *blank
037120030515      * - Nessun giorno specificato - Solo 'M' o 'P'
037130060921     c*** già così:      clear                   Pgc1
037140030515if  3c                   if        DB0gc1 =  ' M'
037150030515     c                   eval      Pgc1   =  %trim($Gio(8))
037160030515x   3c                   else
037170030515     c                   eval      Pgc1   =  %trim($Gio(9))
037180030515e   3c                   endif
037190030515x   2c                   Else
037200030515      * - Giorno specificato
037210030515     c                   movel     DB0gc1        w001a
037220030515     c                   eval      XX     =  1
037230030515     c                   eval      *in31  =  *off
037240030515     c     w001a         lookup    $GgS(xx)                               31
037250030515     c   31              movel     $Gio(xx)      Pgc1
037260030515      *   + 'M' o 'P'
037270030515sel 3c                   select
037280030515w   3c                   when      %subst(DB0gc1:2:1) = 'M'
037290030515     c                   eval      Pgc1   =  %trim(Pgc1) + ' '
037300030515     c                                    +  %trim($Gio(8))
037310030515w   3c                   when      %subst(DB0gc1:2:1) = 'P'
037320030515     c                   eval      Pgc1   =  %trim(Pgc1) + ' '
037330030515     c                                    +  %trim($Gio(9))
037340030515e   3c                   endsl
037350030515e   2c                   Endif
037360030515e   1c                   ENDIF
037370030515      * 2° turno di chiusura
037380060921if  1c                   IF             DB0gc2 <> *blanks
037390060921     c                             and  DB0ffd  = *blanks
037400030515      * - Flag "*" > Turno di chiusura
037410030515     c                   eval      Past   =  '*'
037420030515if  2c                   If        %subst(DB0gc2:1:1) = *blank
037430030515      * - Nessun giorno specificato - Solo 'M' o 'P'
037440060921     c*** già così:      clear                   Pgc2
037450030515if  3c                   if        DB0gc2 =  ' M'
037460030515     c                   eval      Pgc2   =  %trim($Gio(8))
037470030515x   3c                   else
037480030515     c                   eval      Pgc2   =  %trim($Gio(9))
037490030515e   3c                   endif
037500030515x   2c                   Else
037510030515      * - Giorno specificato
037520030515     c                   movel     DB0gc2        w001a
037530030515     c                   eval      XX     =  1
037540030515     c                   eval      *in31  =  *off
037550030515     c     w001a         lookup    $GgS(xx)                               31
037560030515     c   31              movel     $Gio(xx)      Pgc2
037570030515      *   + 'M' o 'P'
037580030515sel 3c                   select
037590030515w   3c                   when      %subst(DB0gc2:2:1) = 'M'
037600030515     c                   eval      Pgc2   =  %trim(Pgc2) + ' '
037610030515     c                                    +  %trim($Gio(8))
037620030515w   3c                   when      %subst(DB0gc2:2:1) = 'P'
037630030515     c                   eval      Pgc2   =  %trim(Pgc2) + ' '
037640030515     c                                    +  %trim($Gio(9))
037650030515e   3c                   endsl
037660030515e   2c                   Endif
037670030515e   1c                   ENDIF
037680071114      *
037690071114      * ALTRI DATI/FLAG/... - - - - - - - - - - - - - - - - - - - - - *
037700030515      *
037710071114      * Se pgm richiamato x stampa LdV in arrivo:
037720030515if  1c                   IF        DB0fl1 =  'A'
037730071114      * - imposto l'"*" in base al parametro ricevuto.
037740030515if  2c                   if        DB0fl2 =  '*'
037750030515     c                   eval      Past   =  '*'
037760030515x   2c                   else
037770030515     c                   clear                   Past
037780030515e   2c                   endif
037790071114      * - idem per flag di bolla non abilitata
037800030515     c                   eval      Pdis   =  DB0fl5
037810030515e   1c                   endif
037820061102      *
037830071114      * Decodifica cliente a cui richiedere il codice fiscale e/o la
037840071114      *   partita iva
037850071114if  1c                   IF             $No_CF =  *on
037860071114     c                             or   $No_PI =  *on
037870061106     c                   clear                   wRSxCF
037880061102sel 2c                   select
037890061102      * - P.F. prepagato
037900061106w   2c                   when      DB0tbo =  '8'
037910061106     c                   movel     DB0rsm        wRSxCF
037920061102      * - P.A. con fattura immediata
037930061106w   2c                   when      %subst(DB0tb1:1:1) = 'A'
037940061106     c                   movel     DB0rsd        wRSxCF
037950061106      * - con 2ª bolla
037960061103w   2c                   when           DB0tb2 <> *blanks
037970121029     c                             and  DB0tbo <> 'P'
037980061103     c                             and  DB0tbo <> '8'
037990061106     c                   movel     DB0rsd        wRSxCF
038000061102e   2c                   endsl
038010061102      *
038020061102e   1c                   ENDIF
038030030515      *
038040030515     c                   ENDSR
038050080918      /free
038060090528
038070090528       //--------------------------------------------------------------
038080140303       //?Reperimento Numero e Data del DDT originale del mittente     ?
038090140303       //?"Consorzio Lotterie Nazionali - Gratta & Vinci"              ?
038100090528       //--------------------------------------------------------------
038110090528       BEGSR  sr_RepDdtCLN;
038120090528
038130110426         // -?Verifica se la spedizione ha mamma?
038140090528         if  not %open(FNLBL01L);
038150090528           open  FNLBL01L;
038160090528         endif;
038170090528         chain  (DB0aas : DB0lnp : DB0nrs : DB0nsp)  FNLBL000;
038180090528
038190090528         Select;
038200090528
038210110426           // -?Con Mamma: reperimento dati DDT dalla mamma?
038220090528           When  %found(FNLBL01L);
038230090528             if  not %open(FNBLP01L);
038240090528               open  FNBLP01L;
038250090528             endif;
038260090603             chain  (LBLaao : LBLlpo : LBLnro : LBLnso)  FNBLP000;
038270090528             if  %found(FNBLP01L);
038280090528               exsr  sr_DatiCLN_blp;
038290090528             endif;
038300090528
038310110426           // -?Dati da DS?
038320090901           When  DB0ges = 'B';
038330090901             exsr  sr_DatiCLN_DS;
038340090901
038350110426           // -?In Partenza: reperimento dati DDT da FNBLP?
038360090528           When  (DB0tbo = 'P'   or   DB0tbo = '8');
038370090528             exsr  sr_DatiCLN_blp;
038380090528
038390110426           // -?In Arrivo: reperimento dati DDT da FNARB?
038400090528           Other;
038410090528             exsr  sr_DatiCLN_arb;
038420090528
038430090528         EndSl;
038440090528
038450090528       ENDSR;
038460090528
038470090901       //--------------------------------------------------------------
038480140303       //?Da DS                                                        ?
038490090901       //--------------------------------------------------------------
038500090901       BEGSR  sr_DatiCLN_DS;
038510090901
038520110426         // -?Numero DDT?
038530090901         select;
038540090902           when  DB0rmn <> *zero;
038550100906             if  DB0rmn  > -99999999   and
038560100906                 DB0rmn <   99999999;
038570100906               WddtCLN = DB0rmn;
038580100906             endif;
038590090902           when  DB0ncd <> *zero;
038600090902             WddtCLN = DB0ncd;
038610090901         endsl;
038620090901
038630110426         // -?Data DDT in formato gg/mm/aaaa?
038640090902         Data_Eur = %date( ((DB0aas * 10000) + DB0mgs) : *iso );
038650090901         WdspCLN  = ( ( %subdt(Data_Eur : *days)  * 1000000 ) +
038660090901                      ( %subdt(Data_Eur : *months) * 10000 ) +
038670090901                      ( %subdt(Data_Eur : *years) ) );
038680090901
038690090901       ENDSR;
038700090901
038710090528       //--------------------------------------------------------------
038720090528       //?In Partenza: reperimento dati DDT da FNBLP                   ?
038730090528       //--------------------------------------------------------------
038740090528       BEGSR  sr_DatiCLN_blp;
038750090528
038760110426         // -?Numero DDT?
038770090528         select;
038780090528           when  BLPrmn <> *zero;
038790100906             if  BLPrmn  > -99999999   and
038800100906                 BLPrmn <   99999999;
038810100906               WddtCLN = BLPrmn;
038820100906             endif;
038830090528           when  BLPncd <> *zero;
038840090528             WddtCLN = BLPncd;
038850090528         endsl;
038860090528
038870110426         // -?Data DDT in formato gg/mm/aaaa?
038880090528         Data_Eur = %date( ((BLPaas * 10000) + BLPmgs) : *iso );
038890090528         WdspCLN  = ( ( %subdt(Data_Eur : *days)  * 1000000 ) +
038900090528                      ( %subdt(Data_Eur : *months) * 10000 ) +
038910090528                      ( %subdt(Data_Eur : *years) ) );
038920090528
038930090528       ENDSR;
038940090528
038950090528       //--------------------------------------------------------------
038960090528       //?In Arrivo: reperimento dati DDT da FNARB                     ?
038970090528       //--------------------------------------------------------------
038980090528       BEGSR  sr_DatiCLN_arb;
038990090528
039000110426         // -?Numero DDT?
039010090528         select;
039020090528           when  ARBrmn <> *zero;
039030100906             if  ARBrmn  > -99999999   and
039040100906                 ARBrmn <   99999999;
039050100906               WddtCLN = ARBrmn;
039060100906             endif;
039070090528           when  ARBncd <> *zero;
039080090528             WddtCLN = ARBncd;
039090090528         endsl;
039100090528
039110110426         // -?Data DDT in formato gg/mm/aaaa?
039120090528         Data_Eur = %date( ((ARBaas * 10000) + ARBmgs) : *iso );
039130090528         WdspCLN  = ( ( %subdt(Data_Eur : *days)  * 1000000 ) +
039140090528                      ( %subdt(Data_Eur : *months) * 10000 ) +
039150090528                      ( %subdt(Data_Eur : *years) ) );
039160090528
039170090528       ENDSR;
039180080918
039190080918       //--------------------------------------------------------------
039200140303       //?Stampa della sola Packing-List (2 per A4)                    ?
039210080918       //--------------------------------------------------------------
039220080918       BEGSR  StampaPkgLst;
039230130315
039240130315         // -?SE NON esiste il *file FIAR8 => uscita?
039250130315         if  $E_AR8 = *off;
039260130315           leavesr;
039270130315         endif;
039280080918
039290110426         // -?MODIFICA SOSPESA:?
039300110426         //  ?NON STAMPA LA PACKING-LIST DAL 2° TENTATIVO DI CONSEGNA?
039310081013         //if   $FIAR8 = *off   or   DB0fl1 = 'D';
039320081013         //  leavesr;
039330081013         //endif;
039340080918
039350110426         // -?Azzeramento numero di pagina per Lettera di Vettura?
039360110426         //  ?(meglio: "reimpostazione a 1")?
039370080926         reset  wPage;
039380080918
039390110426         // -?Posizionamento sul foglio A4 in base al n° pagina in stampa?
039400130418         *in33 = *off;
039410130418         exsr  Posiz_Box;
039420080919
039430110426         // -?Impostazione dei dati da stampare nel rec. con la costante?
039440110426         //  ?"Copia per il destinatario"  + estremi spedizione e pag.?
039450110426         //
039460110426         // -?"Copia per il destinatario" già impostato in sr. "PrepPrt"?
039470081017         //if  §AR8cxd = *blank;
039480130726         //  Tcxd  = c_CopiaXdest;
039490130726         //  Tcxd2 = Tcxd;
039500081017         //endif;
039510081017
039520110426         // -?Estremi spedizione da impostare SOLO se si stampando?
039530110426         //  ?la sola packing-list (DB0FA4 = "P")?
039540081017         if  DB0fa4 = 'P';
039550101028           Trsd = Prsd;
039560080922           Trif = 'Spedizione '
039570080924                + %editc(DB0lnp : 'X') + ' '
039580110310                + %editc(DB0nrs : 'X') + ' '
039590110310                + %editc(DB0nsp : 'X')
039600080922                + ' del ' + %editc(Pdsp : 'Y')
039610081017                + '        Pag.'
039620080922                + %editc(wPage:'Z');
039630101028           Tndc = Pndc;
039640081002           if  wSeqLdV = 1;
039650081002             §Pos   = wPosIni + c_PosBox1As + 0.040;
039660090422             §Pos   = wPosIni + c_PosBox1As + 0.035;
039670081002           else;
039680081002             §Pos   = wPosIni + c_PosBox1As - 0.070;
039690081002           endif;
039700080919         endif;
039710081017
039720110426         // -?La stampa di tale record verrà fatta dopo?
039730081017         //if  §AR8cxd = *blank   or   DB0fa4 = 'P';
039740081017         //  write A4prtDT0;
039750081017         //endif;
039760080918
039770110426         // -?Stampa Packing-List?
039780130315         exsr  StampaDDT;
039790080918
039800080918       ENDSR;
039810090528
039820080918      /end-free
039830030515      *
039840030515      *---------------------------------------------------------------*
039850050217      *?Stampa della Lettera di Vettura                              ?*
039860030515      *---------------------------------------------------------------*
039870040607     c     StampaLDV     BEGSR
039880080604      *
039890080604      * - Posizionamento sul foglio A4 in base al n° pagina in stampa
039900130418     c                   eval      *in33 = *off
039910130418     c                   exsr      Posiz_Box
039920030515      *
039930040607      * Azzeramento numero di pagina per Lettera di Vettura
039940030716      * (meglio: "reimpostazione a 1")
039950080926     c                   reset                   wPage
039960030515      *
039970120530      * 1ª parte - copia per il vettore (da ritornare in "BRT")
039980130418     c                   write     A4prtLDV
039990130418     c                   write     A4prtBOX
040000110426     c*//                write     A4LogoBAR
040010081020      /free
040020120530         //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
040030120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
040040130306         //  // >?Simil-Logo Bartolini/BRT?
040050120530         //  write  A4LogoBR2;
040060120530         //  write  A4LogoDA2;
040070120530         //else;
040080130306         //  // >?Simil-Logo Bartolini?
040090120530         //  write  A4LogoBAR;
040100120530         //  write  A4LogoDA1;
040110120530         //endif;
040120130306         // >?Simil-Logo BRT?
040130120530         write  A4LogoBRT;
040140120530         write  A4LogoDA3;
040150110426
040160110426         // >?dati DPD?
040170081020         if  *in71;
040180081020           exsr  StampaDPD;
040190081020         endif;
040200130306
040210130306         // -?Se LdV con PDF => registrazione rec. nel *file PDFLIST?
040220130306         if  %parms() > 1  and  (DB6pdf = 'F'  or  DB6pdf = 'X')
040230130306                           and  $§PDF   = *on;
040240130306           exsr  sr_WrtRec_PdfList;
040250130306         endif;
040260081020
040270130306         // -?Se richiesta solo tagliatella (copia per il vettore, da?
040280130306         //  ?lasciare però al destinatario) => fine?
040290081020         if  DB0fa4  = '4';
040300081020           leavesr;
040310081020         endif;
040320081020      /end-free
040330050510      *
040340081028      * Se LdV in formato "ex-A5" rimane la copia per il destinatario
040350050510     c     *in69         cabeq     *on           End_LdV
040360030515      *
040370030515sel 1c                   select
040380081028      * > dati DPD  (ora in ex-A5 o in 1/4 di A4)
040390081028w   1c***                when      *in71
040400030515      * > dati FedEx
040410030515w   1c                   when      *in72
040420030515     c                   exsr      ListaFED
040430081028      * > dati EuroExpress  (non più gestiti)
040440040223w   1c                   when      *in73
040450030515      *
040460030515e   1c                   endsl
040470030910      *
040480030910      * > elenco SEGNACOLLI NON SEQUENZIALI
040490050511      *  (no x max 2 perchè sono già stati elencati nell'apposita area)
040500030910      *  (no x DPD   perchè monocollo)
040510030910      *  (no x FedEx perchè segnacolliamo noi)
040520030910      *  (no x "A5"  perchè sono già stati elencati tra le "Note e
040530030910      *              Particolarità")
040540030910if  1c                   if        DB0fns =  'S'
040550050511     c                             and  DB0ncl > 2
040560050513     c                             and  $A4_x_ScNS  =   *on
040570030910     c***                          and  not *in71  and  not *in72
040580030910     c                             and  not *in69
040590030910     c                   exsr      ListaScNS
040600030910e   1c                   endif
040610031124      *
040620031124      * Reperimento della posizione utilizzabile per la stampa dei box
040630080825      *   (in pollici) in base all'ultima riga stampata in 1/8" o 1/9"
040640031124     c                   exsr      CalcRowPos
040650060524      *
040660070123      * > Spedizione con D.A.S. x cliente "PRAOIL OLEODOTTI ITALIANI"
040670081127if  1c                   if             DB0ccm = c_CliPraOil
040680060529     c                             or  (DB0fbr = 'S'
040690081127     c                             and  §B4Aks = c_CliPraOil)
040700060524     c                   exsr      StampaDAS
040710060524e   1c                   endif
040720031124      *
040730031124      * > Promemoria per autotrasportatore
040740110502if  1c*//*               if            (DB0gma = Ktab7V_CB
040750110502     c*//*                           or DB0gma = Ktab7V_C1
040760080625     c*** scontato x GMA='C1'       and DB0bam = 'S'
040770110502     c*//*                           or DB0gma = Ktab7V_C2
040780080729     c*** scontato x GMA='C2'       and DB0bam = 'S'
040790110502     c*//*                           or DB0gma = Ktab7V_C3
040800100513     c*** scontato x GMA='C3'       and DB0bam = 'S'
040810110411     c*//*                           or DB0gma = Ktab7V_C4
040820110411     c*//*                           or DB0gma = Ktab7V_C5
040830110502     c*//*                           or DB0gma = Ktab7V_R1
040840110502     c*//*                           or DB0gma = Ktab7V_NR)
040850110502     c*//*                         and  §7Rldv = 'S'
040860110502     c                   if            (§7Rldv = 'S'
040870110502     c                             and  $TabPXA2  = *on)
040880061106     c                             OR   $No_CF = *on
040890071114     c                             OR   $No_PI = *on
040900070306     c                             OR   $FIARN = *on
040910070315     c                             OR   $C_A_  = *on
040920080801     c                             OR   $ConsParz = *on
040930090302     c                             OR   $TabPXA   = *on
040940110411     c                             OR   $TabPXA2  = *on
040950031124     c                   exsr      ListaPMxA
040960031124e   1c                   endif
040970030515      *
040980030515      * > dati FIAR5
040990150224if  1c*//                if           %subst(DB0gva:1:1) = 'B'
041000150224     c*//                          or %subst(DB0gva:1:1) = 'O'
041010150224if  1c                   if                  DB0tc1      = 'S'
041020030717     c                             or        DB0tc2      = 'S'
041030030515     c                   exsr      ListaAR5
041040030515e   1c                   endif
041050030910      *
041060071015      * > Riferimento O.R.M.
041070071015sel 1c                   select
041080071015      * - > per ritiro contestuale alla consegna
041090100608w   1c                   when          (§7Rrc  = 'S'
041100071015     c                             and  §7Rldv = 'S')
041110030910     c                   exsr      StampaRCC
041120071015      * - > per ritiro porto franco prepagato
041130071015w   1c                   when           DB0tbo = '8'
041140071015     c                   exsr      StampaRPFPP
041150071015e   1c                   endsl
041160090924      /free
041170090924
041180110426         // >?Note per destinatario?
041190090924if  1    if  $BoxNxD;
041200090924           exsr  StampaNxD;
041210090924e   1    endif;
041220090924
041230090924      /end-free
041240040607      *
041250081013      * > DDT in LDV
041260081013      * - IN FUTURO: se al 1° tentativo di consegna
041270081013      *              (DB0FL1 = "D" - v. pgm FNLRS9R)
041280080917if  1c                   if             $FIAR8 =  *on
041290130315     c                             and  $E_AR8 =  *on
041300081013     c***-=»>                      and  DB0fl1 <> 'D'
041310040607     c                   exsr      StampaDDT
041320040607e   1c                   endif
041330050510      *
041340050510     c     End_LdV       tag
041350030515      *
041360080703      * 2ª parte - copia per il destinatario (da lasciare)
041370130418     c                   eval      *in33 = *on
041380040607     c                   exsr      ChiusuraLDV
041390030515      *
041400030515      * Stampa dei 3 asterischi finali (se richiesta)
041410030515if  1c                   if        DB0ges = '*'
041420030515     c                   move      '***'         Pmsg3
041430130418     c                   eval      *in33 = *off
041440130418     c                   exsr      Posiz_Box
041450030515     c                   write     A4prtMSG
041460030515e   1c                   endif
041470030515      *
041480030515     c                   ENDSR
041490080604      /free
041500080604
041510080604       //--------------------------------------------------------------
041520140303       //?Posizionamento del box all'interno della pag. A4             ?
041530080604       //--------------------------------------------------------------
041540130418       BEGSR  Posiz_Box;
041550080704
041560080704         select;
041570080704
041580110426           // -?Richiesta stampa di 1 LdV x pagina?
041590110426           //  ?(LdV in modifica o con packing-list del mitente)?
041600080704           when  DB0fa4 = '1'   or   DB0fa4 = 'S';
041610080704             $LdVxP   = 1;
041620080918             $Len1LdV = $LenLdV2;
041630080704
041640110426           // -?Richiesta stampa di 2 LdV x pagina (ex A5)?
041650110426           //  ?(copia per il vettore & copia per il destinatario)?
041660080704           when  DB0fa4 = '2';
041670080704             $LdVxP   = 2;
041680080704             $Len1LdV = $LenLdV2;
041690080704
041700110426           // -?Richiesta stampa di 4 LdV x pagina?
041710110426           //  ?(solo copia per il vettore)?
041720080704           when  DB0fa4 = '4';
041730080704             $LdVxP   = 4;
041740080704             $Len1LdV = $LenLdV4;
041750080918
041760110426           // -?Richiesta stampa della sola packing-list?
041770110426           //  ?(solo copia per il vettore)?
041780080918           when  DB0fa4 = 'P';
041790080918             $LdVxP   = 2;
041800080924             $Len1LdV = c_Len2_inch;
041810080704
041820080704           other;
041830080704             reset $LdVxP;
041840080704             reset $Len1LdV;
041850080704
041860080704         endsl;
041870080704
041880110426         // -?Richiesta "ripartenza" da foglio A4 successivo?
041890130418         if DB0fl6 = *on  and  Not  *in33;
041900080704           clear  wNumLdV;
041910080704         endif;
041920080704
041930110426         // -?Calcolo posizione di partenza all'interno del foglio A4.?
041940110426         //  ?[Se 2ª LdV nella pagina, ma LdV per A4 (intero):?
041950110426         //   ?si salta alla pag. A4 successiva; la LdV in esame?
041960110426         //   ?dev'essere stampata in posizione iniziale dell'A4,?
041970110426         //   ?seguita dai suoi dati ulteriori.]?
041980130418         if  Not  *in33;
041990130418           wNumLdV  += 1;
042000130418           wSeqLdV   = %rem( wNumLdV : $LdVxP );
042010130418         endif;
042020080704
042030130422         // -?Stampa della copia per il vettore?
042040130422         IF  NOT  *in33;
042050130422
042060130422           select;
042070130422
042080130422             // -?Modulo A4 intero?
042090130422             when  NOT  *in69    and   DB0fa4 <> '4'
042100130422                                 and   DB0fa4 <> 'P';
042110130422               wSeqLdV = 1;
042120130422             when  DB0fa4 = '1'   or   DB0fa4 = 'S';
042130130422               wSeqLdV = 1;
042140130422
042150130422             // -?Calcolo della posizione x box all'interno della pagina "A4"?
042160130422             when  wSeqLdV = *zero   and
042170130422                   $LdVxP  = 2;
042180130422               wSeqLdV = 2;
042190130422             when  wSeqLdV = *zero   and
042200130422                   DB0fa4  = '4';
042210130422               wSeqLdV = 4;
042220130422             when  wSeqLdV = *zero   and
042230130422                   *in69;
042240130422               wSeqLdV  = 2;
042250130422
042260130422           endsl;
042270130422
042280130422         ELSE;
042290130422
042300130422           select;
042310130422
042320130422             // -?Modulo A5 o ricevuta (¼ di A4)?
042330130422             when  *in69;
042340130422
042350130422             // -?Modulo A4 intero - 1ª parte (alta)?
042360130422             when  CurLineA4 <  26   and   §PS1 < c_§PS1;
042370130422               wSeqLdV = 1;
042380130422
042390130422             // -?Modulo A4 intero - 2ª parte (passa)?
042400130422           //when  CurLineA4 <  78;   (in 1/9", ma sarà in 1/8"...)
042410130422             when  CurLineA4 <  70;
042420130422               wSeqLdV  = 2;
042430130422
042440130422             // -?Modulo A4 intero - superata 2ª parte?
042450130422             //  ?PER ORA?IMPOSSIBILE !!!?
042460130422             other;
042470130422               clear wMaxLine;
042480130422               exsr  Test_OF;
042490130422               wMaxLine = $MaxLine2;
042500130422               wSeqLdV  = 1;
042510130422
042520130422           endsl;
042530130422
042540130422         ENDIF;
042550080919
042560110426         // -?Impostaz. delle posizioni nella pagina in corso di stampa?
042570080919         wPosIni = ($Len1LdV * (wSeqLdV - 1));
042580130422         if  *in33;       // -?Stampa della copia per il destinatario?
042590130422           wPosIni += $LenLdV4;
042600130422         endif;
042610110426
042620120530         //// -?Impostaz. dimensioni per il logo Bartolini-BRT?
042630120530         //If  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
042640120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
042650120530         //  reset  c_Spessore;
042660120530         //  reset  c_DeltaSpig;
042670120530         //  reset  c_LunghMaxV;
042680120530         //  reset  c_LunghV;
042690120530         //  reset  c_LunghO;
042700120530         //  reset  c_XspaceX;
042710120530         //  reset  c_BOXspace;
042720120530         //Else;
042730120530         //  c_Spessore  = c_1_Spessore;
042740120530         //  c_DeltaSpig = c_1_DeltaSpig;
042750120530         //  c_LunghMaxV = c_1_LunghMaxV;
042760120530         //  c_LunghV    = c_1_LunghV;
042770120530         //  c_LunghO    = c_1_LunghO;
042780120530         //  c_XspaceX   = c_1_XspaceX;
042790120530         //  c_BOXspace  = c_1_BOXspace;
042800120530         //EndIf;
042810120530         // -?Dimensioni per il logo BRT già definite?
042820120530         //c_Spessore  = c_2_Spessore;
042830120530         //c_DeltaSpig = c_2_DeltaSpig;
042840120530         //c_LunghMaxV = c_2_LunghMaxV;
042850120530         //c_LunghV    = c_2_LunghV;
042860120530         //c_LunghO    = c_2_LunghO;
042870120530         //c_XspaceX   = c_2_XspaceX;
042880120530         //c_BOXspace  = c_2_BOXspace;
042890080704
042900110426         // -?Se richiesta la stampa della sola packing-list?
042910110426         //  ?NON si dovranno stampare i dati della LdV?
042920110426         //  ?(quindi NON servono i posizionamenti che seguono)?
042930080918         if  DB0fa4 = 'P';
042940080918           leavesr;
042950080918         endif;
042960080918
042970110426         // -?Posizionamento nel foglio A4 in base al n° LdV in stampa?
042980110426         //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
042990110426
043000130418         select;
043010130418           when  DB0fa4 = '4';
043020130418             *in41 = (wSeqLdV = 1);
043030130418             *in42 = (wSeqLdV = 2);
043040130418             *in43 = (wSeqLdV = 3);
043050130418             *in44 = (wSeqLdV = 4);
043060130418           when Not *in33;
043070130418             *in41 = (wSeqLdV = 1);
043080130418             *in42 = *off;
043090130418             *in43 = (wSeqLdV = 2);
043100130418             *in44 = *off;
043110130418           other;
043120130418             *in41 = *off;
043130130418             *in42 = (wSeqLdV = 1);
043140130418             *in43 = *off;
043150130418             *in44 = (wSeqLdV = 2);
043160130418         endsl;
043170080704         WRITE  A4prtPOS;
043180080704
043190110426         // -?Posizionamenti nella copia per il vettore?
043200110426         //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
043210080704
043220110426         // -?Box in copia per il vettore?
043230080704         §Box1As = wPosIni + c_PosBox1As;
043240140919         §Box1Ai = wPosIni + c_PosBox1Bs  + c_LineNarrow;
043250080704         §Box1Bs = wPosIni + c_PosBox1Bs;
043260080704         §Box1Bi = wPosIni + c_PosBox1Bi;
043270140919         §Box1Bj = wPosIni + c_PosLin1oD1 + c_LineNarrow;
043280080704         §Box1Cs = wPosIni + c_PosBox1Cs;
043290140919         §Box1Ci = wPosIni + c_PosBox1Ds  + c_LineWide;
043300080704         §Box1Ds = wPosIni + c_PosBox1Ds;
043310091119         §Box1Di = §Box1Bi;
043320081218
043330120530         //If  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
043340120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
043350120530         //  // -?Simil-Logo Bartolini?
043360120530         //  §PlblS  = §Box1As + 0.125;
043370120530         //  §PlblVS = 7.092;
043380120530         //  exsr  sr_SimilLogoBAR;
043390120530         //  // -?Simil-Logo BRT?
043400120530         //  §P2blS  = §PlblS;
043410120530         //  §P2blVS = §PlblVD + 0.030;
043420120530         //  exsr  sr_SimilLogoBRT;
043430120530         //Else;
043440120530         //  // -?Simil-Logo Bartolini?
043450120530         //  §PlblS  = §Box1As;
043460120530         //  §PlblVS = 7.128;
043470120530         //  exsr  sr_SimilLogoBART;
043480120530         //EndIf;
043490120530         // -?Simil-Logo BRT?
043500120530         §PlblS  = §Box1As;
043510120530         §PlblVS = 7.581;
043520120530         exsr  sr_SimilLogoBRT_;
043530080704
043540120530         // -?Costanti dati anagrafici BRT?
043550110426         §PC0    = wPosIni + c_PosTBar;
043560110426         §PC1    = wPosIni + c_PosTBa1;
043570120530         //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
043580120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
043590120530         //  §PC2    = wPosIni + c_PosTBa2;
043600120530         //  §PC3    = wPosIni + c_PosTBa3;
043610120530         //  §PC4    = wPosIni + c_PosTBa4;
043620120530         //  §PC5    = wPosIni + c_PosTBa5;
043630120530         //  §PC6    = wPosIni + c_PosTBa6;
043640120530         //else;
043650120530         //  §PC2    = wPosIni + c_PosTBa2x;
043660120530         //  §PC3    = wPosIni + c_PosTBa3x;
043670120530         //  §PC4    = wPosIni + c_PosTBa4x;
043680120530         //  §PC5    = wPosIni + c_PosTBa5x;
043690120530         //  §PC6    = wPosIni + c_PosTBa6x;
043700120530         //endif;
043710120530         §PC2    = wPosIni + c_PosTBa2x;
043720120530         §PC3    = wPosIni + c_PosTBa3x;
043730120530         §PC4    = wPosIni + c_PosTBa4x;
043740120530         §PC5    = wPosIni + c_PosTBa5x;
043750120530         §PC6    = wPosIni + c_PosTBa6x;
043760130726         §PC7    = §Box1Bi - 0.150;
043770110426
043780110426         // -?Linee orizzontali nel box in copia per il vettore?
043790080704         §Lo1S1  = wPosIni + c_PosLin1oS1;
043800080704         §Lo1S2  = wPosIni + c_PosLin1oS2;
043810080922       //§Lo1S3  = wPosIni + c_PosLin1oS3;
043820080704         §Lo1S4  = wPosIni + c_PosLin1oS4;
043830091119         §Lo1S5  = wPosIni + c_PosLin1oS5;
043840080704         §Lo1C1  = wPosIni + c_PosLin1oC1;
043850080704         §Lo1C2  = wPosIni + c_PosLin1oC2;
043860140915         §Lo1C3  = wPosIni + c_PosLin1oC3;
043870080704         §Lo1D1  = wPosIni + c_PosLin1oD1;
043880080704
043890110426         // -?Lunghezza linee verticali nel box in copia per il vett.?
043900140922         §LLv1A  = §Lo1S5  - §Lo1S1;
043910140922         //§LLv1B  = §Lo1S4  - §Box1Bs;
043920140922         //§LLv1C  = §Box1Di - §Box1Bs;
043930140922         //***§LLv1B  = §Lo1S4  - §Box1Bj + c_LineNarrow;
043940140922         §LLv1B  = §Lo1S4  - §Box1Bj;
043950140922         §LLv1C  = §Box1Di - §Box1Bj;
043960140917
043970140917         // -?Costante "BRTcode" & codice BRTcode?
043980140917         §Pk1    = wPosIni + c_PosKBRTc;
043990080704
044000110426         // -?Costante "Consegna tassativa"?
044010080704         §Pk2    = wPosIni + c_PosKTass;
044020080704
044030110426         // -?Costanti "(nome e cognome in stampatello)", "(firma)" e?
044040110426         //           ?"(data e ora)"?
044050080704         §Pk3    = §Box1Bi - 0.015;
044060080704
044070110426         // -?Costante con rif. alla legge in vigore per RCV?
044080080704         §Pk4    = wPosIni + c_PosKRCV;
044090080704
044100110426         // -?Costante "Bollo assolto in modo virtuale..."?
044110080704         §Pk5    = wPosIni + c_PosKBAV;
044120080704         §Pk6    = §Pk5 + 0.060;
044130121017
044140121017         // -?Partita IVA o Cod. Fiscale del cliente?
044150121017         §Pk7    = §Pk4 + 0.080;
044160080704
044170110426         // -?Costante "COPIA PER IL DESTINATARIO" in DDT "generico"?
044180080709         §Pos    = c_PosKCXD2;
044190080704
044200080704       ENDSR;
044210080605
044220080604      /end-free
044230110426
044240140303       //--------------------------------------------------------------
044250140303       //?Posizionamento del simil-logo Bartolini (unico)              ?
044260140303       //--------------------------------------------------------------
044270120530      *///copy gaitrasrc/srcTNSY,LogoBAR_R2
044280110426
044290140303       //--------------------------------------------------------------
044300140303       //?Posizionamento del simil-logo Bartolini (con BRT)            ?
044310140303       //--------------------------------------------------------------
044320120530      *///copy gaitrasrc/srcTNSY,LogoBAR3R2
044330110426
044340140303       //--------------------------------------------------------------
044350140303       //?Posizionamento del simil-logo BRT (con BARTOLINI)            ?
044360140303       //--------------------------------------------------------------
044370120530      *///copy gaitrasrc/srcTNSY,LogoBRT3R2
044380120530
044390140303       //--------------------------------------------------------------
044400140303       //?Posizionamento del simil-logo BRT (unico)                    ?
044410140303       //--------------------------------------------------------------
044420120530      /copy gaitrasrc/srcTNSY,LogoBRT_R2
044430110426
044440030515      *---------------------------------------------------------------*
044450050217      *?Gestione overflow                                            ?*
044460030515      *---------------------------------------------------------------*
044470030515     c     Test_OF       BEGSR
044480030515      *
044490030515      * Test raggiungimento della posizione limite
044500081003     c                   if          ((§PS1 < $LimOF   and   NOT *in45)
044510081003     c                             or (§PS1 < ($LimOF - c_Len1_inch)
044520081003     c                                     and *in45   and   NOT *in46)
044530081003     c                             or (§PS1 < ($LimOF + c_Len1_inch)
044540081003     c                                     and *in45   and   *in46))
044550080924     c                             and CurLineA4 < wMaxLine
044560080919     c                   leavesr
044570030515e   1c                   endif
044580030515      *
044590030515      * Stampa del box per dati da FIAR5
044600030515      * (se è già stata stampata almeno una riga di tale box)
044610060921if  1c                   if        $FIAR5 = *on  and  $First = *off
044620030911     c                   eval      §LO2   = §PS1 + 0.050
044630071008if  2c                   if        NOT *in52
044640030515     c                   write     A4prtAR53
044650071008x   2c                   else
044660071008     c                   eval      §PS2   = §LO2 - 0.500
044670071008     c                   write     A4prtAR54
044680071008e   2c                   endif
044690030515e   1c                   endif
044700041209      *
044710041209sel 1c                   select
044720040607      *
044730041209      * DDT KARNAK/TITANEDI/EDK
044740041209      * Stampa di logo, box, linee e testi fissi del DDT del mittente
044750040607      * (se si stà stampando tale DDT)
044760040607      * (ma NON per la parte "merce omaggio")
044770040607      * calcolando - prima - le posizioni iniziale e finale del box
044780041209w   1c                   when           $DDT_p    = *on
044790071008     c                             and  §AR8tsd   = 'K1'
044800040607     c                             and  $DDT_o    = *off
044810040608      * - Stampa parte finale DDT
044820040608if  2c                   if        $DDTlastPg = *off
044830041209      * - - Calcolo della posizione finale del box
044840040607     c                   eval      PCnv2  = '          '
044850040607     c                                    + '*** SEGUE ***'
044860040608     c                   exsr      Posiz_DK
044870040607     c                   write     A4prtDK4
044880040608x   2c                   else
044890041209      * - - Stampa dei dati in chiusura DDT (se ultima pagina)
044900040608      *     (la posizione finale del box verrà lì calcolata)
044910050217     c                   exsr      PrtDdtKARN_V2
044920040608e   2c                   endif
044930040607      * - Stampa logo, box, linee e testi fissi
044940081017     c                   if            §AR8cxd = *blanks
044950081017     c                             or  DB0fa4  = 'P'
044960050218     c                   write     A4prtDT0
044970080923     c                   endif
044980040607     c                   write     A4prtDK0
044990041019      *
045000050414      * DDT LOGISTICA/SAIT Abrasivi
045010050215      * Stampa di logo, box, linee e testi fissi del DDT del mittente
045020041019      * (se si stà stampando tale DDT)
045030041209w   1c                   when           $DDT_p    = *on
045040060627     c                             and  %subst(§AR8tsd:1:1) = 'L'
045050041021     c                   exsr      Posiz_DL
045060050217      * - - Chiusura box in fase di stampa dettaglio DDT
045070050217      *     (la stampa dei dati in chiusura DDT sarà solo nell'ultima
045080050318      *      pagina - prima dell'eventuale packing-list)
045090050318      *     La gestione del salto alla 1ª pagina della packing-list
045100050321      *     (AR8trk = 'Z1' & Page = *zeros) avviene nella subr.
045110050318      *     "PrtDdtLOG1_Z1" (vedi subr. "PrtPckLst_1").
045120081017     c                   if            §AR8cxd = *blanks
045130081017     c                             or  DB0fa4  = 'P'
045140050308     c                   write     A4prtDT0
045150050331     c                   endif
045160050318sel 2c                   select
045170050505      *     - si sta stampando il DDT Logistica o Sait
045180050318w   2c                   when      %subst(AR8trk:1:1) <> 'Z'
045190071008     c                             and %subst(AR8trk:1:1) <> 'S'
045200050218     c                   write     A4prtDL0A
045210050615      *     - si sta stampando la Packing-List
045220050318w   2c                   when      %subst(AR8trk:1:1) =  'Z'
045230050615     c                             and    Page        >  *zeros
045240050318     c                   write     A4prtDL0C
045250050606      *     - si sta stampando la Fattura (oltre 1ª pag.)
045260071008w   2c                   when      %subst(AR8trk:1:1) =  'S'
045270081003     c                             and    Page        >  *zeros
045280050609     c                   write     A4prtDL0E
045290050318e   2c                   endsl
045300050113      *
045310050113      * DDT ARCTE
045320050215      * Stampa di logo, box, linee e testi fissi del DDT del mittente
045330050113      * (se si stà stampando tale DDT)
045340050113w   1c                   when           $DDT_p    = *on
045350050113     c                             and  AR8tmd    = 'ARCT'
045360050114     c                   exsr      Posiz_AC
045370050217      * - - Chiusura box in fase di stampa dettaglio DDT
045380050217      *     (la stampa dei dati in chiusura DDT sarà solo nell'ultima
045390050217      *      pagina)
045400081017     c                   if            §AR8cxd = *blanks
045410081017     c                             or  DB0fa4  = 'P'
045420050331     c                   write     A4prtDT0
045430050331     c                   endif
045440050217     c                   write     A4prtAC0A
045450050215      *
045460050215      * DDT ESPRINET
045470050215      * Stampa di box, linee e testi fissi del DDT del mittente
045480050215      * (se si stà stampando tale DDT)
045490050215      * · Solo nella 1ª pagina vengono stampati il logo e la parte
045500050215      *        superiore del DDT.
045510050215      * · Solo nell'ultima pagina vene stampato il "fondo".
045520050215w   1c                   when           $DDT_p    = *on
045530060627     c                             and  §AR8tsd   = 'E1'
045540110405if 1bc                   if        %subst(SAVtrk:1:1 ) <> 'P'
045550110405     c                             or   %eof(FIAR801L)
045560050215      * - Stampa parte finale DDT
045570050215      * - - Calcolo delle posizioni di box & linee
045580050215     c                   exsr      Posiz_ES
045590050215      * - - Chiusura box in fase di stampa dettaglio DDT
045600050215      *     (la stampa dei dati in chiusura DDT sarà solo nell'ultima
045610050215      *      pagina)
045620050215if  2c***                if        %subst(AR8trk:1:1) <> 'P'
045630081017     c                   if            §AR8cxd = *blanks
045640081017     c                             or  DB0fa4  = 'P'
045650050331     c                   write     A4prtDT0
045660050331     c                   endif
045670050215     c                   write     A4prtES0A
045680050215e   2c***                endif
045690110405e  1bc                   endif
045700041209      *
045710041209      * DDT GENERICO con LOGO
045720050215      * Stampa di logo e box del DDT del mittente
045730041209      * (se si stà stampando tale DDT)
045740041209w   1c                   when           $DDT_p    = *on
045750041209     c                             and  §AR8tsd   = 'S2'
045760041209     *** * - Chiusura del box con i dati di mittente e destinatario
045770041209     *** *   (Se si stanno ancora stampando 'sti dati: bisogna calcolare
045780041209     *** *    la posizione di chiusura temporanea del box)
045790041209if  2***c                   if             AR8trk =  'B1'
045800041209     ***c                             or   AR8trk =  'B2'
045810041209     *** *   Impostazione della posizione finale per il box
045820041209     ***c                   exsr      Rep_§LO2
045830041209     ***c                   eval      §PB2   = §LO2 + 0.065
045840041209e   2***c                   endif
045850041209     *** *   Stampa del box
045860041209     ***c                   write     A4prtDB0
045870041209      * - Il logo è già stato SICURAMENTE stampato
045880041209     c***                exsr      Rep_§LO2
045890041209     c***                eval      §PB1   = §LO2
045900041209      *** Stampa del logo
045910041209     c***                write     A4prtDB0
045920041209      *
045930041209e   1c                   endsl
045940030515      *
045950030515      * Stampa dell'indicazione "Segue..." su pagina corrente
045960030515     c                   clear                   Pseg0
045970030515     c                   clear                   Pseg1
045980030515     c                   eval      Pseg9  = '  Segue...'
045990080919      /free
046000080923         select;
046010080923           when  *in45   and   NOT *in46;
046020080923             §PSA = 0.200;   // Per campo vuoto !
046030080923             §PSB = 0.200;   // Per campo vuoto !
046040080923             §PSC = c_Len2_inch - 0.050;
046050080923           when  *in45   and   *in46;
046060080923             §PSA = c_Len2_inch + 0.200;   // Per campo vuoto !
046070080923             §PSB = c_Len2_inch + 0.200;   // Per campo vuoto !
046080080924             §PSC = c_Len2_inch * 2 - 0.200;
046090080923           when  NOT *in45;
046100080923             §PSA = 0.400;
046110080923             §PSB = 0.700;
046120080923             §PSC = 8.750;
046130080923         endsl;
046140080919      /end-free
046150030515     c                   write     A4prtSEG
046160030515      *
046170031031      * Se sono ancora nella prima pagina =>
046180030515      * stampo anche i dati per il destinatario e box/barcode/ecc.
046190080604sel 1c                   select
046200080923      * > Se NON sto stampando un DDT del mittente
046210080922      *   (nel qual caso non stampo null'altro)
046220080923      *   => nulla
046230080919w   1c                   when      wPage  > 1
046240080923      * > Se sto stampando le sole copie per il vettore (4 in A4)
046250080923      *    o la relativa packing-list
046260080923      *   (nel qual caso non stampo null'altro)
046270080923      *   => nulla
046280081003w   1c                   when      DB0fa4 = '4'   or   DB0fa4 = 'P'
046290040607      * > Se NON è ristampa in arrivo per recapito c/assegni  e
046300040707      *   se NON sto stampando una LdV Poste
046310080703      *   se NON sto stampando (anche) un DDT del mittente
046320080703      *   se NON sto stampando le sole copie per il vettore (4 in A4)
046330080923      *   => posizionamento e stampa
046340120113w   1c                   when          $No_x_Cons =  *off
046350081013     c                             and ($FIAR8    =  *off)
046360081013     c***-=»>                       or  DB0fl1    =  'D')
046370080605      * - Posizionamento sul foglio A4 in base al n° pagina in stampa
046380130418     c                   eval      *in33 = *on
046390130418     c                   exsr      Posiz_Box
046400130418      * - Stampa 2ª parte della LdV (copia per il destinatario)
046410130418     c                   write     A4prtLDV
046420031031      * - Stampa "cornice" / barcode / righine varie in parte inferiore
046430130418     c                   write     A4prtBOX
046440110426     c*//                write     A4LogoBAR
046450110426      /free
046460120530         //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
046470120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
046480120530         //  // >?Simil-Logo Bartolini/Brt?
046490120530         //  write  A4LogoBR2;
046500120530         //  write  A4LogoDA2;
046510120530         //else;
046520120530         //  // >?Simil-Logo Bartolini?
046530120530         //  write  A4LogoBAR;
046540120530         //  write  A4LogoDA1;
046550120530         //endif;
046560120530         // >?Simil-Logo BRT?
046570120530         write  A4LogoBRT;
046580120530         write  A4LogoDA3;
046590110426      /end-free
046600120113      *//* > Se sto stampando una LdV Poste per recapito c/assegni
046610120113w   1c*//                   when          $No_x_Cons =  '2'
046620120113      *//* - Stampa 2ª parte "nulla" della LdV
046630130418     c*//                   eval      *in33 = *on
046640130418     c*//                   exsr      Posiz_Box
046650120113     c*//                   write     A4prtLDVB
046660080604e   1c                   endsl
046670080923      /free
046680110426         // -?Se richiesta la stampa della sola packing-list:?
046690110426         //  ?incremento del n° spedizione per calcolare correttam.?
046700110426         //  ?il n° di pagina iniziale per la spediz. successiva?
046710080925         if  DB0fa4 = 'P';
046720080925             wNumLdV += 1;
046730080925         endif;
046740080925
046750110426         // -?Reimpostazione degli indicatori per il posizionamento?
046760110426         //  ?all'interno del foglio A4?
046770080923         *in45 = (DB0fa4 = 'P');
046780080923         *in46 = (DB0fa4 = 'P'   and   %rem(wNumLdv : 2) = *zero);
046790080923      /end-free
046800030515      *
046810030515      * Salta a pagina successiva e indica "(Seguito)"
046820030515     c                   write     A4prtSP0
046830030515     c                   eval      wPage  = wPage + 1
046840080923      /free
046850081017         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
046860081017           write  A4prtNUL;
046870081017         endif;
046880081017
046890080923         IF  DB0fa4 <> 'P';
046900080922      /end-free
046910030515     c                   eval      Pseg0  = 'Spedizione '
046920110310     c                                    + %editc(DB0lnp : 'X') + ' '
046930110310     c                                    + %editc(DB0nrs : 'X') + ' '
046940110310     c                                    + %editc(DB0nsp : 'X')
046950030515     c                                    + ' del ' + %editc(Pdsp : 'Y')
046960030515     c                   eval      Pseg1  = '(Seguito)             Pag.'
046970030515     c                                    + %editc(wPage:'Z')
046980030515     c                   clear                   Pseg9
046990030515     c                   write     A4prtSEG
047000080922      /free
047010080923         ELSE;
047020081017
047030110426           // -?"Copia per il destinatario" già impostato in sr. "PrepPrt"?
047040081017           //if  §AR8cxd = *blank;
047050130726           //  Tcxd  = c_CopiaXdest;
047060130726           //  Tcxd2 = Tcxd;
047070081017           //endif;
047080081017
047090110426           // -?Estremi spedizione da impostare SOLO se si stampando?
047100110426           //  ?la sola packing-list (DB0FA4 = "P")?
047110081017           if  DB0fa4 = 'P';
047120101028             //Trsd = Prsd;   (già impostato nella subr. StampaPkgLst)?
047130081017             Trif = 'Spedizione '
047140081017                  + %editc(DB0lnp : 'X') + ' '
047150110310                  + %editc(DB0nrs : 'X') + ' '
047160110310                  + %editc(DB0nsp : 'X')
047170081017                  + ' del ' + %editc(Pdsp : 'Y')
047180081017                  + '        Pag.'
047190101028                  + %editc(wPage:'Z');
047200101028                //+ '        (Seguito)';
047210101028             //Tndc = Pndc;   (già impostato nella subr. StampaPkgLst)?
047220081017           endif;
047230080923
047240110426           // -?Verifica se la 2ª parte della packing-list sta nella?
047250110426           //  ?pag. attuale o se deve andare in quella successiva?
047260080923           wSeqLdV  = %rem( wNumLdV : $LdVxP );
047270080923           if  wSeqLdV = *zero   and   $LdVxP  = 2;
047280080923             wSeqLdV = 2;
047290080923           endif;
047300080923
047310110426           // -?Posizionamenti nella pagina in corso di stampa?
047320110426           //  ?(§POS = posizione verticale della costante "COPIA PER?
047330110426           //   ?IL DESTINATARIO" (con estremi spedizione e pag.)?
047340080923           wPosIni  = ($Len1LdV * (wSeqLdV - 1));
047350080924           if  wSeqLdV = 1;
047360080924             §Pos   = wPosIni + c_PosBox1As + 0.040;
047370090422             §Pos   = wPosIni + c_PosBox1As + 0.035;
047380080924           else;
047390080924             §Pos   = wPosIni + c_PosBox1As - 0.070;
047400080924           endif;
047410081017
047420081017           if  §AR8cxd = *blank   or   DB0fa4  = 'P';
047430081017             write A4prtDT0;
047440081017           endif;
047450080923
047460080923         ENDIF;
047470080922      /end-free
047480030515      *
047490030515      * Reimposta posizioni iniziali di stampa per linee
047500030515      *   (orizzontali-§LOn e verticali-§LVn) e campi (§PSn).
047510030515      *   Nelle pagine dalla 2ª in poi si parte da in alto:
047520030515      *   non vengono ri-stampati i 2 box "vettore" e "destinatario".
047530030515sel 1c                   select
047540120530      * - BRT
047550030515w   1c                   when      *in70
047560080711     c                   eval      §LO1   = c_§PS1
047570030515     c                   eval      §LO2   = §LO1
047580030515     c                   eval      §PS1   = §LO1
047590040607sel 2c                   select
047600050217      * - - con DDT del mittente KARNAK / TITANEDI / EDK
047610040607w   2c                   when           $DDT_p    = *on
047620071008     c                             and  §AR8tsd   = 'K1'
047630040608     c                   exsr      NewPageDK
047640050414      * - - con DDT del mittente LOGISTICA / SAIT ABRASIVI
047650041207w   2c                   when           $DDT_p    = *on
047660060627     c                             and  %subst(§AR8tsd:1:1) = 'L'
047670050321     c                   exsr      NewPageDL
047680050113      * - - con DDT del mittente ARCTE
047690050113w   2c                   when           $DDT_p    = *on
047700050113     c                             and  AR8tmd    = 'ARCT'
047710081002     c                   exsr      NewPageAC
047720050215      * - - con DDT del mittente ESPRINET
047730050215w   1c                   when           $DDT_p    = *on
047740060627     c                             and  §AR8tsd   = 'E1'
047750110405if  2c                   if        %subst(SAVtrk:1:1 ) <> 'P'
047760110405     c                             or   %eof(FIAR801L)
047770081002     c                   exsr      NewPageES
047780110405e   2c                   endif
047790041207      * - - con DDT generico (con marchio)
047800041207w   2c                   when           $DDT_p    = *on
047810041207     c                             and  §AR8tsd   = 'S2'
047820081017     c                   exsr      NewPageSTD
047830041207      * - - con DDT generico (senza marchio)
047840041207w   2c                   when           $DDT_p    = *on
047850041207     c*** ("scontato":)            and  §AR8tsd   = 'S1'
047860081017     c                   exsr      NewPageSTD
047870040607e   2c                   endsl
047880080922      * - DPD (ora in ex-A5)
047890080922w   1c***                when      *in71
047900030515      * - FedEx
047910030515w   1c                   when      *in72
047920030709     c                   eval      §LO1   = 1.600
047930030515     c                   eval      §LO2   = §LO1 + 0.425
047940030515     c                   eval      §LV1   = §LO1 + 0.300
047950030515     c                   eval      §LV2   = §LV1 + 0.070
047960030515     c                   write     A4prtFED1
047970030515do  2c                   dow       CurLineA4 < $MinLine1
047980030515     c                   write     A4prtNUL
047990030515e   2c                   enddo
048000030515     c                   write     A4prtFED2
048010030515     c                   eval      §LO1   = §LO2 - 0.250
048020030911     c                   eval      §PS1   = §LO1 + 0.150
048030040223      * - EuroExpress
048040040223w   1c                   when      *in73
048050030515      *
048060030515e   1c                   endsl
048070030515      *
048080030515      * N.B.: Per i SegnaColli non sequenziali non servono queste
048090030515      *       impostazioni: trattasi di stampa con salto riga -skipb-
048100030515      *       quindi sono già posizionato.
048110030515      *
048120080919     c                   ENDSR
048130030910      *
048140030910      *---------------------------------------------------------------*
048150050217      *?Reperimentoo della posizione (in pollici) in cui posizionarsi?*
048160050217      *?* per la stampa in base all'ultima riga stampata             ?*
048170030910      *---------------------------------------------------------------*
048180030910     c     Rep_§LO2      BEGSR
048190030910      *
048200030910sel 1c                   select
048210080825      *
048220080825      * Se ho stampato l'elenco dei segnacolli non sequenziali, devo
048230080825      *   "allineare" il posizionamento in pollici all'ultima riga
048240080825      *   stampata (in ottavi di pollice).
048250080825      *   =>   Num_ultima_riga_stampata / 8
048260080825w   1c                   when           DB0fns     = 'S'
048270080825     c                             and  DB0ncl     > 2
048280080825     c                             and  $A4_x_ScNS = *on
048290080825     c                             and  NOT  *in69
048300080825     c                   eval(h)   §LO2   = (CurLineA4 / 8) + 0,1
048310040607      *
048320040607      * Se ho stampato il DDT del mittente
048330080714      *   =>   Num_ultima_riga_stampata / 9
048340081013w   1c                   when           $FIAR8 =  *on
048350081013     c***-=»>                      and  DB0fl1 <> 'D'
048360080708     c                   eval(h)   §LO2   = (CurLineA4 / 9) + 0,1
048370030910      *
048380030910      * Se non ho stampato né elenco segnacolli non sequenziali né
048390040223      *   altri box, devo impostare comunque i dati per il posiziona-
048400030910      *   mento di questo primo box
048410030910w   1c                   when      §LO2   = *zeros
048420080624     c                   eval      §LO1   = 2.850
048430030910     c                   eval      §LO2   = §LO1
048440030910      *
048450030910e   1c                   endsl
048460030910      *
048470030910     c                   ENDSR
048480031124      *
048490031124      *---------------------------------------------------------------*
048500050217      *?Reperim. della posizione (in pollici) utlizzaile per la      ?*
048510080714      *?  stampa dei box in base all'ultima riga stampata in 1/9"    ?*
048520031124      *---------------------------------------------------------------*
048530031124     c     CalcRowPos    BEGSR
048540031124      *
048550031124      * Imposto le posizioni inziali per il primo dei box
048560080624     c                   select
048570080624      *
048580040607      * - in DDT del mittente: dopo eventuale elenco merce omaggio
048590120530      *   (come in LDV BRT)
048600120530      * - in LDV BRT: dopo eventuale elenco segnacolli non seq.
048610080624     c                   when      *in70
048620080624     c                   exsr      Rep_§LO2
048630080624      *
048640080922      * - in LDV DPD (ora in ex-A5)
048650080922     c***                when      *in71
048660080624      *
048670040607      * - in LDV FEDEX: posizione variabile
048680031124      *   (dipende dal numero dei segnacolli elencati: intanto
048690031124      *   posiziona la "base di partenza" sull'ultima posizione
048700031124      *   eventualmente usata per l'elenco dei segnacolli FedEx;
048710031124      *   poi si ri-calcoleranno in base al numero di righe stampate)
048720080624     c                   when      *in72
048730080624     c                   eval      §LO2   = §LO1 + 0.250
048740080624      *
048750080912      * - in LDV EUROEXPRESS: non più gestita
048760080624     c                   when      *in73
048770080912      *
048780080624     c                   endsl
048790080624      *
048800031124     c                   eval      §LO1   = §LO2
048810031124      *
048820031124     c                   ENDSR
048830060524      *
048840060524      *---------------------------------------------------------------*
048850060524      *?Stampa box per riferimento al                                ?*
048860060524      *?DOCUMENTO di ACCOMPAGNAMENTO SEMPLIFICATO                    ?*
048870060524      *---------------------------------------------------------------*
048880060524     c     StampaDAS     BEGSR
048890060524      *
048900060524     c***                eval      Par5   = '*...+....1....+....2....+-
048910060524     c***                          ....3....+....4....+....5....+....6-
048920060524     c***                          ....+....7....+....8....+....9....+-
048930060524     c***                          ....A'
048940060524      *
048950060524      * - stampo testata
048960090302     ***c                   eval      $First = 'B'
048970090302     ***c                   eval      Par5   = C_PMxA
048980090302      /free
048990090302         $First = *on;
049000090302         Par5   = '                    '
049010090302                + '                    '
049020090302                + '          '
049030090302                + C_PMxA;
049040090302      /end-free
049050060524     c                   exsr      StampaAR5
049060090302      /free
049070090302
049080090302         // - memorizza posizione iniziale per il box
049090090302         //   (§PB1 sarà da reimpostare in §LO1 PRIMA della stampa del box)
049100090302         §PB1   = §LO1;
049110090302
049120090302         // - stampa linea separatrice
049130090302         §LO1   = §PS1 + 0.050;
049140090302         write  A4prtLO;
049150090302         §PS1   = §LO1;
049160090302      /end-free
049170060524      *
049180060524      * - stampo 1° testo
049190060524     c                   eval      $First = '2'
049200060524     c                   eval      Par5   = 'CONSEGNARE AL DESTINATARI-
049210060524     c                             O D.D.T. E D.A.S. (Documento di Acc-
049220060524     c                             ompagnamento Semplificato).'
049230060524     c                   exsr      StampaAR5
049240060524      *
049250060524      * - stampo 2° testo
049260061030     c*** (già fatto:)   eval      $First = *off
049270060524     c                   eval      Par5   = 'In caso di RIFIUTO compil-
049280060524     c                             are l''ultima pagina del D.A.S. ind-
049290060524     c                             icando:'
049300060524     c                   exsr      StampaAR5
049310060524     c                   eval      Par5   = 'a) nome Autotrasportatore-
049320060524     c                              nel campo "IL SOTTOSCRITTO ... INC-
049330060524     c                             ARICATO DEL TRASPORTO"'
049340060524     c                   exsr      StampaAR5
049350060524     c                   eval      Par5   = 'b) firma Autotrasportator-
049360060524     c                             e nel campo "FIRMA INCARICATO DEL T-
049370060524     c                             RASPORTO"'
049380060524     c                   exsr      StampaAR5
049390060524     c                   eval      Par5   = 'c) causale del rifiuto (b-
049400060524     c                             arrare "la merce non è conforme all-
049410060524     c                             ''ordine" o indicare altra motivazi-
049420060524     c                             one)'
049430060524     c                   exsr      StampaAR5
049440060524     c                   eval      Par5   = 'd) firma del destinatario-
049450060524     c                              nel campo "FIRMA DEL DESTINATARIO"'
049460060524     c                   exsr      StampaAR5
049470090302     *** *
049480090302     *** * - stampo box
049490090302     ***c                   eval      §LO2   = §PS1 + 0.050
049500090302     ***c                   write     A4prtAR53
049510090302      /free
049520090302         // - stampa box
049530090302         //   (§PB1 è da reimpostare in §LO1: contiene il suo valore PRIMA
049540090302         //   della stampa delle varie linee separatrici)
049550090302         §LO1   = §PB1;
049560090302         §LO2   = §PS1 + 0.070;
049570090302         write  A4prtAR53;
049580090302      /end-free
049590060524      *
049600060524     c                   ENDSR
049610031124      *
049620031124      *---------------------------------------------------------------*
049630050217      *?Prepara elenco per box "Promemoria per autotrasportatore"    ?*
049640031124      *---------------------------------------------------------------*
049650031124     c     ListaPMxA     BEGSR
049660031124      *
049670070308      * - stampo testata
049680070316     c                   eval      $First = *on
049690070319     c                   eval      Par5   = '                    '
049700070319     c                                    + '                    '
049710070319     c                                    + '          '
049720070319     c                                    + C_PMxA
049730031124     c                   exsr      StampaAR5
049740070315      *
049750070315      * - memorizzo posizione iniziale per il box
049760070320      *   (§PB1 sarà da reimpostare in §LO1 PRIMA della stampa del box)
049770070315     c                   eval      §PB1   = §LO1
049780031124      *
049790070308      * - stampo testo
049800081127      *
049810081212      /free
049820110411         // >?Mittente cliente in tab. "PXA"?
049830090302         IF  $TabPXA = *on;
049840090302
049850170517           clear  k05tntbe01;
049860090302           k_TBEcod = 'PXA';
049870090302           k_TBEke1 = %editc(DB0ccm : 'X');
049880100316           k_TBEke2 = '001            ';
049890090302           clear  k_TBElin;
049900090302           clear  k_TBEsif;
049910100316           //setll  %kds(k05tntbe01 : 3)  TNTBE000;  (già fatto)
049920090302           //reade  %kds(k05tntbe01 : 2)  TNTBE000;  (già fatto)
049930090302           clear  wx;
049940090302
049950100316           DoW  not  %eof(TNTBE01L)  and  TBEke2 <= '006            ';
049960090302
049970090302             if  TBEatb = *blank;
049980090302
049990110411               // -?Stampa della riga separatrice iniziale?
050000090302               if  wx = *zero;
050010090302                 §LO1   = §PS1 + 0.050;
050020090302                 write  A4prtLO;
050030090302                 §PS1   = §LO1;
050040090302                 $First = 'G';
050050090302               endif;
050060090302
050070110411               // -?Salta eventuali righe vuote?
050080090302               dow  %int( %trimr( TBEke2 ) ) > wx + 1;
050090090302                 clear  Par5;
050100090302                 exsr  StampaAR5;
050110090302                 wx += 1;
050120090302               enddo;
050130090302
050140110411               // -?Stampa la singola riga di testo?
050150090302               Par5   = TBEuni;
050160090302               exsr  StampaAR5;
050170090302               wx += 1;
050180090302
050190090302             endif;
050200090302
050210090302             reade  %kds(k05tntbe01 : 2)  TNTBE000;
050220090302
050230090302           EndDo;
050240090302
050250090302         ENDIF;
050260110411
050270110411         // >?Particolarità consegna in tab. "PXA"?
050280110411         IF  $TabPXA2 = *on;
050290110411
050300170517           clear  k05tntbe01;
050310110411           k_TBEcod = 'PXA';
050320110411           k_TBEke1 = DB0gma;
050330110411           k_TBEke2 = '001            ';
050340110411           clear  k_TBElin;
050350110411           clear  k_TBEsif;
050360110411           setll  %kds(k05tntbe01 : 3)  TNTBE000;
050370110411           reade  %kds(k05tntbe01 : 2)  TNTBE000;
050380110411           clear  wx;
050390110411
050400110411           DoW  not  %eof(TNTBE01L)  and  TBEke2 <= '006            ';
050410110411
050420110411             if  TBEatb = *blank;
050430110411
050440110411               // -?Stampa della riga separatrice iniziale?
050450110411               if  wx = *zero;
050460110411                 §LO1   = §PS1 + 0.050;
050470110411                 write  A4prtLO;
050480110411                 §PS1   = §LO1;
050490110411                 $First = 'G';
050500110411               endif;
050510110411
050520110411               // -?Salta eventuali righe vuote?
050530110411               dow  %int( %trimr( TBEke2 ) ) > wx + 1;
050540110411                 clear  Par5;
050550110411                 exsr  StampaAR5;
050560110411                 wx += 1;
050570110411               enddo;
050580110411
050590110411               // -?Stampa la singola riga di testo?
050600110411               Par5   = TBEuni;
050610110411               exsr  StampaAR5;
050620110411               wx += 1;
050630110411
050640110411             endif;
050650110411
050660110411             reade  %kds(k05tntbe01 : 2)  TNTBE000;
050670110411
050680110411           EndDo;
050690110411
050700110411         ENDIF;
050710090302
050720081212      /end-free
050730081127      *
050740031216sel 1c                   select
050750070306      * >
050760070306x   1c***                other
050770070306     c***                eval      $First = *off
050780070306     c***                clear                   Par5
050790070306     c***                exsr      StampaAR5
050800070306      *
050810070306e   1c                   endsl
050820061102      *
050830071114      * > Raccogliere Codice Fiscale e/o Partita Iva di ...
050840071114if  1c                   if             $No_CF = *on
050850071114     c                             or   $No_PI = *on
050860070320     c                   eval      §LO1   = §PS1 + 0.050
050870070320     c                   write     A4prtLO
050880070320     c                   eval      §PS1   = §LO1
050890070321     c                   eval      $First = 'G'
050900061102     c                   eval      Par5   = 'Spedizione n°'
050910061102     c                                    + %editw(DB0lnp:'0   ')
050920061102     c                                    + %editw(DB0nrs:'0  ')
050930061102     c                                    + ' '
050940061102     c                                    + %editc(DB0nsp:'Z')
050950061102     c                                    + ' del '
050960061102     c                                    + %editc(Pdsp:'Y')
050970061102     c                   exsr      StampaAR5
050980071114if  2c                   if        $No_PI = *on
050990071114     c*** (già fatto:)   eval      $First = 'G'
051000071114     c                   eval      Par5   = 'raccogliere +
051010071114     c                                      Partita I.v.a. di "'
051020071114     c                                    + %trim(wRSxCF)
051030071114     c                                    + '"'
051040071114     c                   exsr      StampaAR5
051050120504if  3c                   if        $No_CF = *off
051060120504     c*** (già fatto:)   eval      $First = 'G'
051070120504     c                   eval      Par5   = 'Codice Fiscale di "'
051080120504     c                                    + %trim(wRSxCF)
051090120504     c                                    + '": '
051100120504     c                                    + wB0cfi
051110120504     c                   exsr      StampaAR5
051120120504e   3c                   endif
051130071114e   2c                   endif
051140120503if  2c                   if        $No_CF = *on
051150120503     c*** (già fatto:)   eval      $First = 'G'
051160120503     c                   eval      Par5   = 'raccogliere +
051170120503     c                                      Codice Fiscale di "'
051180120503     c                                    + %trim(wRSxCF)
051190120503     c                                    + '"'
051200120503     c                   exsr      StampaAR5
051210120503e   2c                   endif
051220070306e   1c                   endif
051230070306      *
051240070306      * > Note bolla (dal file FIARN00F)
051250070306      *   (posizionamento e 1ª lettura già effettuate nella subr.
051260070306      *    "CheckFIARN")
051270070306if  1c                   if        $FIARN = *on
051280070320     c                   eval      §LO1   = §PS1 + 0.050
051290070320     c                   write     A4prtLO
051300070320     c                   eval      §PS1   = §LO1
051310070321     c                   eval      $First = 'G'
051320070319do  2c                   dou       %eof(FIARN11L)
051330070306     c                   eval      Par5   = ARNnob
051340070306     c                   exsr      StampaAR5
051350070319     c     K06ARN11      reade     FIARN000
051360070306e   2c                   enddo
051370070306e   1c                   endif
051380070319      *
051390070319      * > Consegne parziali precedenti (dal file FNART00F)
051400070406if  1c                   IF        $ConsParz     =  *on
051410101130     c*//(già fatto)?   exsr      SchConsParz
051420070320     c                   eval      §LO1   = §PS1 + 0.050
051430070320     c                   write     A4prtLO
051440070320     c                   eval      §PS1   = §LO1
051450070321     c                   eval      $First = 'G'
051460070319     c                   eval      xx     = 1
051470070319do  2c                   DoW            (xx      <= %elem($CP)
051480070319     c                             and  $CP(xx)  <  *all'9')
051490070319     c                   movel     $CP(xx)       Elem_CP
051500070319     c                   z-add     WCPdcm        w0080
051510070319     c     *iso          test(d)                 w0080                  31
051520070319if  3c                   if        NOT *in31
051530071025     c     *iso          movel     w0080         Data_EUR
051540071025     c     *eur          movel     Data_EUR      w0080
051550070319x   3c                   else
051560070319     c                   clear                   w0080
051570070319e   3c                   endif
051580140304 *...+*...1....+....2....+....3....+....4....+....5....+....6....+....7....+..
051590140305 Conse*nati colli 12345 in data 12/34/5678
051600140304 Conse*nati colli 12345 in data 12/34/5678 alle ore 12:34 - Firmatario: 123456
051610140304 Conse*nati colli 12345 il 12/34/5678 alle 12:34 a 1234567890123456789012345
051620140305 Conse*nati colli 12345 in data 12/34/5678 alle ore 12:34
051630140305      *                 Firmatario: 123456789012345678901234567890123456789012
051640070321     c                   eval      Par5   = 'Consegnati colli '
051650140305     c                                    + %editc(WCPncl:'Z')
051660140305     c                                    + ' in data '
051670070319     c                                    + %editc(w0080:'Y')
051680140304     c                   if        WCPocm > *zero
051690140304     c                   eval      Par5   = %trimr( Par5 )
051700140305     c                                    + ' alle ore '
051710140303     c                                    + %editw(WCPocm : '0 :  ')
051720140304     c                   endif
051730070319     c                   exsr      StampaAR5
051740140305     c                   if        WCPfir <> *blank
051750140305     c                   clear                   Par5
051760140305     c                   eval      %subst( Par5 : 24 ) = 'Firmatario: '
051770140305     c                                    + WCPfir
051780140305     c                   exsr      StampaAR5
051790140305     c                   endif
051800070319     c                   eval      xx     = xx + 1
051810070319e   2c                   EndDo
051820070319e   1c                   ENDIF
051830070321      *
051840070321      * > Reperita Comunicazione Anomalia
051850070321if  1c                   if        $C_A_  = *on
051860070321     c                   eval      §LO1   = §PS1 + 0.050
051870070321     c                   write     A4prtLO
051880070321     c                   eval      §PS1   = §LO1
051890070321     c                   eval      $First = 'G'
051900070321     c                   eval      Par5   = 'Presente Comunicazione +
051910070321     c                                                Anomalia'
051920070321     c                   exsr      StampaAR5
051930070321e   1c                   endif
051940031124      *
051950031124      * - stampo box
051960070320      *   (§PB1 è da reimpostare in §LO1: contiene il suo valore PRIMA
051970070320      *   della stampa delle varie linee separatrici)
051980070315     c                   eval      §LO1   = §PB1
051990070315     c                   eval      §LO2   = §PS1 + 0.070
052000031124     c                   write     A4prtAR53
052010031124      *
052020031124     c                   ENDSR
052030090924      /free
052040090924
052050090924       //--------------------------------------------------------------
052060090924       //?Prepara elenco per box "Note per il destinatario"            ?
052070090924       //--------------------------------------------------------------
052080090924       BEGSR  StampaNxD;
052090090924
052100090924         // - Stampa testata
052110090924         //$First = *on;
052120090924         //Par5   = '                    '
052130090924         //       + '                    '
052140090924         //       + '          '
052150090924         //       + c_NxD;
052160090924         //exsr  StampaAR5;
052170090924
052180090924         // - Memorizzazione posizione iniziale per il box
052190090924         //   (§PB1 sarà da reimpostare in §LO1 PRIMA della stampa del box)
052200090924         //§PB1 = §LO1;
052210090924
052220090924         // - Stampa della riga separatrice iniziale
052230090924         //§LO1   = §PS1 + 0.050;
052240090924         //write  A4prtLO;
052250090924         //§PS1   = §LO1;
052260090924
052270090924         // - Stampa la singola riga di testo
052280090925         $First = '1';
052290090924         Par5 = 'DECODER DIGITALE TERRESTRE OMAGGIO DA SKY PER AVER +
052300090924                 ADERITO ALL''OFFERTA';
052310090924         exsr  StampaAR5;
052320090924
052330090924         // - Memorizzazione posizione iniziale per il box
052340090924         //   (§PB1 sarà da reimpostare in §LO1 PRIMA della stampa del box)
052350090924         §PB1 = §LO1;
052360090924
052370090924         // - Stampa box
052380090924         //   (§PB1 è da reimpostare in §LO1: contiene il suo valore PRIMA
052390090924         //   della stampa delle varie linee separatrici)
052400090924         §LO1 = §PB1;
052410090925         §LO2 = §PS1 + 0.050;
052420090924         write  A4prtAR53;
052430090924
052440090924       ENDSR;
052450090924
052460090924      /end-free
052470030515      *
052480030515      *---------------------------------------------------------------*
052490050217      *?Prepara elenco per box - "Bancali da rendere al mittente"    ?*
052500150224      *?                         » NON PIÙ «                         ?*
052510050217      *?                       - "Dati per destinatario supermercato"?*
052520150224      *?                         » NON PIÙ «                         ?*
052530050217      *?                       - "Promemoria per autotrasportatore"  ?*
052540030515      *---------------------------------------------------------------*
052550030515     c     ListaAR5      BEGSR
052560030515      *
052570060921     c                   eval      $FIAR5 = *on
052580030515      *
052590030515      * Ripulisco i campi utilizzati in seguito
052600030515     c                   clear                   AR5trd
052610030515     c                   clear                   AR5dac
052620150304     c*//                clear                   dAR5ban
052630030717     c                   clear                   dAR5bnb
052640030903     c                   clear                   dAR5rcc
052650030515     c                   clear                   dAR5scr
052660030515     c                   reset                   $First
052670030515     c                   reset                   $FirstSCR
052680030515     c                   reset                   SAVtrd
052690030515      *
052700030515      * Posizionamento iniziale
052710040301if  1c                   if        DUTlpo = 'S'
052720040531     c     KLDV          setgt     FIAR531C
052730040301x   1c                   else
052740040607     c     KLDV          setgt     FIAR5000
052750040301e   1c                   endif
052760030515      *
052770030515      * Ciclo di lettura del file FIAR5
052780030515      *
052790030515do  1c                   DO        *Hival
052800030515      *
052810040607if  2c                   if        DUTlpo = 'S'
052820040531     c     KLDV          readpe    FIAR531C
052830040607x   2c                   else
052840040607     c     KLDV          readpe    FIAR5000
052850040607e   2c                   endif
052860040301      *
052870040301if  2c                   if           (DUTlpo =  'S'
052880040531     c                             and %eof(FIAR531C)
052890040301     c                             OR  DUTlpo <> 'S'
052900040301     c                             and %eof(FIAR501L))
052910030515     c                   leave
052920030515e   2c                   endif
052930030515if  2c                   if        AR5atb <> *blanks
052940030515     c                   iter
052950030515e   2c                   endif
052960030515      *
052970030515      * Controllo se devo stampare l'intestazione al nuovo box
052980030515if  2c                   if        AR5trd <> SAVtrd
052990030515      *   Stampa del precedente box (se stampata almeno una riga)
053000030910if  3c                   if             SAVtrd <> *hival
053010030910     c                             and  $First =  *off
053020030911     c                   eval      §LO2   = §PS1 + 0.050
053030030515     c                   write     A4prtAR53
053040030515e   3c                   endif
053050030515     c                   eval      $First =  *on
053060030515     c                   eval      SAVtrd =  AR5trd
053070030515e   2c                   endif
053080030515      *
053090150224sel 2c                   SELECT
053100150224      *//*
053110150224      *//* > Bancali da rendere al mittente
053120150224      *//*   (unico record, sequenzialmente prima dei rec. "SCR")
053130150224w   2c*//                WHEN      %subst(DB0gva:1:1) = 'B'
053140150224     c*//                          and    AR5trd      = 'BAN'
053150150224     c*//                movel     AR5uni        dAR5ban
053160150224      *//* - stampo testata
053170150224if  3c*//                if           (§AR5tba <> *zeros
053180150224     c*//                          or  §AR5tb2 <> *zeros)
053190150224     c*//                eval      Par5   = 'BANCALI DA RIPORTARE A MAGAZZINO E'
053200150224     c*//                                 + ' DA RENDERE AL MITTENTE           '
053210150224     c*//                exsr      StampaAR5
053220150224e   3c*//                endif
053230150224      *//* - stampo 1ª riga
053240150224if  3c*//                if        §AR5tba <> *zeros
053250150224     c*//                clear                   Par5
053260150224     c*//                clear                   Wtba
053270150224     c*//                eval      Wtba   = §AR5tba
053280150224     c*//                exsr      Tab_TBA
053290150224     c*//                eval      Par5   = §TBAdesc + 'N. '
053300150224     c*//                                 + %editc(§AR5nba : 'Z')
053310150224     c*//                exsr      StampaAR5
053320150224e   3c*//                endif
053330150224      *//* - stampo 2ª riga
053340150224if  3c*//                if        §AR5tb2 <> *zeros
053350150224     c*//                clear                   Par5
053360150224     c*//                clear                   Wtba
053370150224     c*//                eval      Wtba   = §AR5tb2
053380150224     c*//                exsr      Tab_TBA
053390150224     c*//                eval      Par5   = §TBAdesc + 'N. '
053400150224     c*//                                 + %editc(§AR5nb2 : 'Z')
053410150224     c*//                exsr      StampaAR5
053420150224e   3c*//                endif
053430150224      *//*
053440150224      *//* > Bancali da riportare a magazzino ("Oltre 20 colli")
053450150224      *//*   (unico record, sequenzialmente prima dei rec. "SCR",
053460150224      *//*    alternativo a "BAN" = bancali da rendere al mittente).
053470150224w   2c*//                WHEN      %subst(DB0gva:1:1) = 'O'
053480150224     c*//                          and    AR5trd      = 'BNB'
053490150224     c*//                movel     AR5uni        dAR5bnb
053500150224      *//* - stampo testata
053510150224if  3c*//                if           (§AR5btba <> *zeros
053520150224     c*//                          or  §AR5btb2 <> *zeros)
053530150224     c*//                eval      Par5   = 'BANCALI DA RIPORTARE A MAGAZZINO  '
053540150224     c*//                exsr      StampaAR5
053550150224e   3c*//                endif
053560150224      *//* - stampo 1ª riga
053570150224if  3c*//                if        §AR5btba <> *zeros
053580150224     c*//                clear                   Par5
053590150224     c*//                clear                   Wtba
053600150224     c*//                eval      Wtba   = §AR5btba
053610150224     c*//                exsr      Tab_TBA
053620150224     c*//                eval      Par5   = §TBAdesc + 'N. '
053630150224     c*//                                 + %editc(§AR5bnba : 'Z')
053640150224     c*//                exsr      StampaAR5
053650150224e   3c*//                endif
053660150224      *//* - stampo 2ª riga
053670150224if  3c*//                if        §AR5btb2 <> *zeros
053680150224     c*//                clear                   Par5
053690150224     c*//                clear                   Wtba
053700150224     c*//                eval      Wtba   = §AR5btb2
053710150224     c*//                exsr      Tab_TBA
053720150224     c*//                eval      Par5   = §TBAdesc + 'N. '
053730150224     c*//                                 + %editc(§AR5bnb2 : 'Z')
053740150224     c*//                exsr      StampaAR5
053750150224e   3c*//                endif
053760150224      *//* - stampo 3ª riga
053770150224if  3c*//                if        §AR5bcor <> *zeros
053780150224     c*//                clear                   Par5
053790150224     c*//                eval      Par5   = C_CoOr
053800150224     c*//                                 + %editc(§AR5bcor : 'K')
053810150224     c*//                exsr      StampaAR5
053820150224e   3c*//                endif
053830030515      *
053840030515      * > Destinatario supermercato
053850030515w   2c                   WHEN      (DB0tc1 = 'S' or DB0tc2 = 'S')
053860030515     c                             and AR5trd  = 'SCR'
053870030515     c                   movel     AR5uni        dAR5scr
053880030515      * - stampo testata
053890030515      *   (se subr. richiamata solo per verifica esistenza dati: esco)
053900030515if  3c                   if        $First =  *on
053910031124     c                   eval      Par5   = C_DeSM
053920030515     c                   exsr      StampaAR5
053930030515     c                   eval      $FirstSCR  = *off
053940030515e   3c                   endif
053950030515      * - stampo righe
053960030515     c                   clear                   Par5
053970030624     c                   eval      *in38 = *on
053980030515if  3c                   if           AR5dac <> SAVdac
053990030515     c                             or AR5orc <> SAVorc
054000030515     c                   eval      SAVdac = AR5dac
054010030515     c                   eval      SAVorc = AR5orc
054020030515      * - - giro data modifica
054030030515     c                   z-add     AR5dac        w0080
054040070315     c     *iso          test(d)                 w0080                  31
054050070315if  4c                   if        NOT *in31
054060071025     c     *iso          movel     w0080         Data_EUR
054070071025     c     *eur          movel     Data_EUR      w0080
054080070315x   4c                   else
054090070315     c                   clear                   w0080
054100070315e   4c                   endif
054110030515     c                   eval      Par5 = 'Modifica del '
054120030515     c                                  + %editc(w0080 : 'Y')
054130030515     c                                  + ' '
054140030515     c                                  + %editw(AR5orc : '  :  :  ')
054150030515     c                                  + ' '
054160040223     c                                  + §AR5Spru
054170030515     c                   exsr      StampaAR5
054180030515e   3c                   endif
054190040223     c                   eval      Par5 = §AR5Snom
054200030911     c                                  + '   Tel.: '
054210040223     c                                  + §AR5Stel
054220030911     c                   exsr      StampaAR5
054230040223     c                   eval      Par5 = §AR5Smot
054240030515     c                   exsr      StampaAR5
054250030624     c                   eval      *in38 = *off
054260031124      *
054270031124      * > Se destinatario assente inserire il plico in buca lettere
054280031124      *   (già stampato in box "Promemoria per autotrasportatore")
054290031124w   2c***                WHEN          DB0gma  = Ktab7V_CB
054300031124     c***                ...       ...
054310030903      *
054320030903      * > O.R.M. per ritiro contestuale alla consegna
054330030903      *   (unico record)
054340050510      *   Verrà stampato come ultimo box
054350050510      *   - prima del (eventuale) DDT del mittente
054360050510      *   - prima della copia per il destinatario
054370030910w   2c***                WHEN          DB0gma  = Ktab7V_RC
054380030910     c***                          and AR5trd  = 'RCC'
054390030903     c***                ...       ...
054400030515      *
054410030515e   2c                   ENDSL
054420030515      *
054430030515e   1c                   ENDDO
054440030515      *
054450030515      * Stampa del (ultimo) box (se stampata almeno una riga)
054460030515if  1c                   if        $First = *off
054470030911     c                   eval      §LO2   = §PS1 + 0.050
054480030515     c                   write     A4prtAR53
054490030515e   1c                   endif
054500030515      *
054510030515      * > Destinatario supermercato
054520030515      *   (stampa del box vuoto per memo se non già stampata almeno
054530030515      *   una riga)
054540030515if  1c                   IF        (DB0tc1 = 'S' or DB0tc2 = 'S')
054550030515     c                             and              $FirstSCR = *on
054560030515      * - stampo testata
054570030515     c                   eval      $First = *on
054580031124     c                   eval      Par5   = C_DeSM
054590030515     c                   exsr      StampaAR5
054600030515     c                   eval      $FirstSCR  = *off
054610030515      * - stampo 2 righe vuote
054620030515     c                   clear                   Par5
054630030515do  2c                   do        2
054640030515     c                   exsr      StampaAR5
054650030515e   2c                   enddo
054660030515      * - stampo box
054670030911     c                   eval      §LO2   = §PS1 + 0.050
054680030515     c                   write     A4prtAR53
054690030515e   1c                   ENDIF
054700070123      *
054710070123     c                   eval      $FIAR5 = *off
054720030515      *
054730030515     c                   ENDSR
054740030515      *
054750030515      *---------------------------------------------------------------*
054760050217      *?Stampa singola riga relativa a FIAR5                         ?*
054770030515      *---------------------------------------------------------------*
054780030515     c     StampaAR5     BEGSR
054790030515      *
054800030515      * Controllo se superata l'ultima riga
054810030515     c                   exsr      Test_OF
054820030515      *
054830030515      * Stampa del singolo record
054840030909sel 1c                   select
054850030909      *
054860030909w   1c                   when      $First = *off
054870030909      * - Incremento parametri per posizionamento nuova riga
054880030911     c                   eval      §PS1   = §PS1 + 0.150
054890030909     c                   write     A4prtAR52
054900030909      *
054910030909w   1c                   when      $First = *on
054920030515      * - Incremento parametri per posizionamento nuovo box
054930030515      *   e relativa intestazione
054940030911     c                   eval      §LO1   = §LO2 + 0.150
054950030911     c                   eval      §PS1   = §LO1 + 0.150
054960030515     c                   write     A4prtAR51
054970030515     c                   eval      $First = *off
054980060524      *
054990060524w   1c                   when      $First = '2'
055000060524      * - Incremento parametri per posizionamento nuova riga
055010060524      *   nel box "Promemoria per autotrasportatore"
055020060524     c                   eval      §PS1   = §PS1 + 0.150
055030060524     c                   write     A4prtAR51
055040060524     c                   eval      $First = *off
055050030909      *
055060030909w   1c                   when      $First = 'A'
055070030909      * - Incremento parametri per posizionamento nuovo box
055080030909      *   e relativa intestazione
055090030911     c                   eval      §LO1   = §LO2 + 0.150
055100030909     c                   eval      §PS1   = §LO1 + 0.200
055110030909     c                   write     A4prtAR5a
055120030909     c                   eval      $First = *off
055130150226      *//*
055140150226w   1c*//                when      $First = 'B'
055150150226      *//* - Incremento parametri per posizionamento nuovo box
055160150226      *//*   ed intestazione "Promemoria per autotrasportatore"
055170150226     c*//                eval      §LO1   = §LO2 + 0.150
055180150226     c*//                eval      §PS1   = §LO1 + 0.125
055190150226     c*//                write     A4prtAR5b
055200150226     c*//                eval      $First = 'C'
055210150226      *//*
055220150226w   1c*//                when      $First = 'C'
055230150226      *//* - Incremento parametri per posizionamento nuova riga
055240150226      *//*   nel box "Promemoria per autotrasportatore"
055250150226     c*//                eval      §PS1   = §PS1 + 0.250
055260150226     c*//                write     A4prtAR5c
055270070321      *
055280070321w   1c                   when      $First = 'G'
055290070321      * - Incremento parametri per posizionamento nuova riga
055300070321      *   nel box "Promemoria per autotrasportatore"
055310070321     c                   eval      §PS1   = §PS1 + 0.170
055320070321     c                   write     A4prtAR5g
055330030909      *
055340030909e   1c                   endsl
055350030515      *
055360030515     c                   ENDSR
055370030910      *
055380030910      *---------------------------------------------------------------*
055390050217      *?Stampa box "Ritiro Contestuale alla Consegna" da FIAR5/RCC   ?*
055400030910      *---------------------------------------------------------------*
055410030910     c     StampaRCC     BEGSR
055420030910      *
055430030910      * > O.R.M. per ritiro contestuale alla consegna
055440030910      *   (unico record)
055450031124      *   Verrà stampato come ultimo box (prima della copia per il
055460031124      *     destinatario)
055470030910      *
055480030910     c                   clear                   dAR5rcc
055490030910      *
055500030910     c                   eval      AR5trd = 'RCC'
055510040301if  1c                   if        DUTlpo = 'S'
055520040531     c     K05AR501      chain     FIAR531C
055530040301x   1c                   else
055540030910     c     K05AR501      chain     FIAR5000
055550040301e   1c                   endif
055560040301      *
055570071008if  1c                   if           (DUTlpo =  'S'
055580040531     c                             and %found(FIAR531C)
055590040301     c                             OR  DUTlpo <> 'S'
055600040301     c                             and %found(FIAR501L))
055610040301     c                             and AR5atb =  *blanks
055620030910     c                   movel     AR5uni        dAR5rcc
055630071015     c                   eval      ORMpoe = §AR5Rpoe
055640071015     c                   eval      ORMnsr = §AR5Rnsr
055650071015     c                   eval      ORMnor = §AR5Rnor
055660071015     c                   eval      ORMnrv = §AR5Rnrv
055670071015     c                   eval      Pbt2a  = 'RITIRO CONTESTUALE ALLA +
055680071015     c                                       CONSEGNA'
055690071015     c                   exsr      StampaORM
055700071015e   1c                   endif
055710071015      *
055720071015     c                   ENDSR
055730071015      *
055740071015      *---------------------------------------------------------------*
055750071015      *?Stampa box "Ritiro Porto Franco PrePagato"                   ?*
055760071015      *---------------------------------------------------------------*
055770071015     c     StampaRPFPP   BEGSR
055780071015      *
055790071015      * > O.R.M. per ritiro porto franco prepagato
055800071015      *   Verrà stampato come ultimo box (prima della copia per il
055810071015      *     destinatario)
055820071015      *
055830071015     c*** già fatto:     clear                   dsBL4M
055840071015     c*** già fatto:     exsr      RepAR4_M
055850071015      *
055860071015if  1c                   if             §B4poe  > *zeros
055870071015     c                             and  §B4nor  > *zeros
055880071015     c                   eval      ORMpoe = §B4poe
055890071015     c                   eval      ORMnsr = §B4nsr
055900071015     c                   eval      ORMnor = §B4nor
055910071015     c                   eval      ORMnrv = §B4nrv
055920071015     c                   eval      Pbt2a  = 'RITIRO PORTO FRANCO +
055930071015     c                                       PREPAGATO'
055940071015     c                   exsr      StampaORM
055950071015e   1c                   endif
055960071015      *
055970071015     c                   ENDSR
055980071015      *
055990071015      *---------------------------------------------------------------*
056000071015      *?Stampa dati O.R.M. in box "Ritiro Contestuale alla Consegna" ?*
056010071015      *?              o in quello "Ritiro Porto Franco Prepagato"    ?*
056020071015      *---------------------------------------------------------------*
056030071015     c     StampaORM     BEGSR
056040071015      *
056050071015     c                   eval      $FIAR5 = *on
056060071015      *
056070071015if  2c                   if        not %open(FNORM01L)
056080071015     c                   open      FNORM01L
056090071015e   2c                   endif
056100071015     c     K04ORM01      chain     FNORM000
056110071015      *
056120071015if  2c                   IF        %found(FNORM01L)
056130071015      *
056140071015      * Reperisco la posizione (in pollici) in cui posizionarmi
056150071015      *   per la stampa in base all'ultima riga stampata
056160071015      *   se non ho ancora stampato box
056170071015cas 3c     §LO2          caseq     *zeros        Rep_§LO2
056180071015e   3c                   endcs
056190071025      *
056200071015      * - stampo testata
056210071025      *   (con il n° O.R.M.)
056220071015     c                   eval      $First = 'A'
056230071015     c*** già impostato: eval      Pbt2a  = 'RITIRO CONTESTUALE ALLA +
056240071015     c***  "      "                          CONSEGNA'
056250071015     c                   eval      Pbt2b  = 'O.R.M. numero '
056260071015     c                                    + %editw(ORMpoe : '0   ')
056270071015     c                                    + ' '
056280071015     c                                    + %editw(ORMnsr : '0  ')
056290071015     c                                    + '  '
056300071015     c                                    + %editc(ORMnor : 'Z')
056310071015     c                                    + ' '
056320071015     c                                    + %editw(ORMnrv : '0  ')
056330071015     c                   exsr      StampaAR5
056340071025      *
056350071015      * - stampo i dati dell'O.R.M.
056360071015     c                   exsr      QUM_FnOrm
056370140410     c                   eval      Par5   = 'Quantità      : '
056380071015     c                                    + %trim($Qum(1)) + '    '
056390071015     c                                    + %trim($Qum(2)) + '    '
056400071015     c                                    + %trim($Qum(3))
056410071015     c                   exsr      StampaAR5
056420080228if  3c                   if        DB0tbo = '8'
056430140410     c                   eval      Par5   = 'Mittente      : '
056440080228     c                                    + ORMrsr
056450080228x   3c                   else
056460140410     c                   eval      Par5   = 'Destinatario  : '
056470071015     c                                    + ORMrsc
056480080228e   3c                   endif
056490071015     c                   exsr      StampaAR5
056500080228if  3c                   if        DB0tbo = '8'
056510140410     c                   eval      Par5   = '                '
056520080228     c                                    + ORMinr
056530080228x   3c                   else
056540140410     c                   eval      Par5   = '                '
056550071015     c                                    + ORMinc
056560080228e   3c                   endif
056570071015     c                   exsr      StampaAR5
056580080228if  3c                   if        DB0tbo = '8'
056590140410     c                   eval      Par5   = '                '
056600080228     c                                    + ORMcar + ' '
056610080228     c                                    + ORMlor + ' '
056620080228     c                                    + ORMprr + ' '
056630080228     c                                    + ORMnar
056640080228x   3c                   else
056650140410     c                   eval      Par5   = '                '
056660071015     c                                    + ORMcac + ' '
056670071015     c                                    + ORMloc + ' '
056680071015     c                                    + ORMprc + ' '
056690071015     c                                    + ORMnac
056700080228e   3c                   endif
056710071015     c                   exsr      StampaAR5
056720101022             // -?SE Ritiro Porto Franco Prepagato:?
056730080228if  3c                   if        DB0tbo = '8'
056740101022      /free
056750101022               // -?stampo il referente ed il relativo telefono?
056760101022               // -?(Comunque! Ci sono sempre!)?
056770140410               Par5   = 'Referente     : ' + ORMrer + '   '
056780101022                      + 'Telefono: '     + ORMter;
056790101022               exsr  StampaAR5;
056800101022      /end-free
056810140410     c                   eval      Par5   = 'Ordinante     : '
056820080228     c                                    + ORMrso
056830080228     c                   exsr      StampaAR5
056840140410     c                   eval      Par5   = 'Destinatario  : '
056850080303     c                                    + ORMrsc
056860080303     c                   exsr      StampaAR5
056870080228e   3c                   endif
056880071015      * - stampo le note
056890071015      *   (Comunque! Non solo "se ci sono". Al limite: scarto la 1ª.)
056900140410     c                   eval      Par5   = 'Note          : '
056910071015if  3c                   if        ORMno1 <> *blanks
056920071015     c                   eval      Par5   = %trim(Par5)  + ' '
056930071015     c                                    + ORMno1 + ' ' + ORMno2
056940071015x   3c                   else
056950071015     c                   eval      Par5   = %trim(Par5)  + ' '
056960071015     c                                    + ORMno2
056970071015e   3c                   endif
056980071015     c                   exsr      StampaAR5
056990071025      *
057000140410      * -?SE Ritiro Porto Franco Prepagato:?
057010071025if  3c                   IF        DB0tbo = '8'
057020140410      *
057030140410      * - stampo orari apertura (SE ci sono)
057040140410     c                   exsr      sr_OrariApert
057050140410      *
057060140410      * - stampo data & ora ritiro
057070071025     c                   z-add     ORMdar        w0080
057080071025     c     *iso          test(d)                 w0080                  31
057090071025if  4c                   if        NOT *in31
057100071025     c     *iso          movel     w0080         Data_EUR
057110071025     c     *eur          movel     Data_EUR      w0080
057120071025x   4c                   else
057130071025     c                   clear                   w0080
057140071025e   4c                   endif
057150140410     c                   eval      Par5   = 'Data ritiro   : '
057160071025     c                                    + %editc(w0080:'Y')
057170071025if  4c                   if        ORMorr > 1230
057180071025     c                   eval      Par5   = %trimr(Par5)
057190071025     c                                    + '          '
057200140410     c                                    + 'Ora ritiro: '
057210071025     c                                    + %editw(ORMorr:'0 :  ')
057220071025     c                   exsr      StampaAR5
057230071025x   4c                   else
057240071025     c                   exsr      StampaAR5
057250071025     c                   clear                   Par5
057260071025     c                   eval      %subst(Par5:50)
057270071025     c                                    = 'Ora ritiro: '
057280071025     c                                    + %editw(ORMorr:'0 :  ')
057290071025     c                   write     A4prtAR51
057300071025e   4c                   endif
057310140410      *
057320071025e   3c                   ENDIF
057330071025      *
057340071015      * - imposto codice a barre O.R.M.
057350071015     c                   movel     BarCodeORM    Pbco
057360071015      * - verifico se codice a barre da stampare
057370071015     c                   eval      *in52  =  (Pbco <> *zeros)
057380071025      *
057390071015      * - stampo box
057400071015     c                   eval      §LO2   = §PS1 + 0.050
057410071015if  3c                   if        NOT *in52
057420071015     c                   write     A4prtAR53
057430071015x   3c                   else
057440071025     c                   eval      §LV1   = §LO2 - §LO1
057450071015     c                   eval      §PS2   = §LO2 - 0.500
057460071015     c                   write     A4prtAR54
057470071015e   3c                   endif
057480071015      *
057490071015e   2c                   ENDIF
057500071015      *
057510071015     c                   eval      $FIAR5 = *off
057520071015      *
057530071015     c                   ENDSR
057540100513      /free
057550140410
057560140410       //--------------------------------------------------------------
057570140410       //?Reperimento, impostazione e stampa degli Orari di Apertura   ?
057580140410       //--------------------------------------------------------------
057590140410       BEGSR  sr_OrariApert;
057600140410
057610140410         clear  dOREorari;
057620140410
057630140410         // -?Recupero dati da estensione FNORE (trc "O ")?
057640140410         if  NOT  %open(FNORE01L);
057650140410           open  FNORE01L;
057660140410         endif;
057670140410         k_OREpoe = ORMpoe;
057680140410         k_OREnsr = ORMnsr;
057690140410         k_OREnor = ORMnor;
057700140410         k_OREnrv = ORMnrv;
057710140410         k_OREtrc = 'O ';
057720140410         chain  %kds(keyFNORE01)  FNORE000;
057730140410
057740140410         // -?...NON trovati => uscita dalla subroutine?
057750140410         if  Not  %found(FNORE01L);
057760140410           leavesr;
057770140410         endif;
057780140410
057790140410         // -?...Trovati => verifica degli orari?
057800140410         dOREorari = OREdati;
057810140410
057820140410         Select;
057830140410
057840140410           // -?C'è un solo orario "ALLE"?
057850140410           When  §OREoraMda = *zero  and  §OREoraMa > *zero  and
057860140410                 §OREoraPda = *zero  and  §OREoraPa = *zero;
057870140410             Par5 = 'Orari apertura: ' +
057880140410                    'Fino alle ' + %editw( §OREoraMa : ' 0:  ' );
057890140410           When  §OREoraMda = *zero  and  §OREoraMa = *zero  and
057900140410                 §OREoraPda = *zero  and  §OREoraPa > *zero;
057910140410             Par5 = 'Orari apertura: ' +
057920140410                    'Fino alle ' + %editw( §OREoraPa : ' 0:  ' );
057930140410
057940140410           // -?C'è un solo orario completo (per 1°)?
057950140410           When  §OREoraMda > *zero  and  §OREoraMa > *zero  and
057960140410                 §OREoraPda = *zero  and  §OREoraPa = *zero;
057970140410             Par5 = 'Orari apertura: ' +
057980140410                    %editw( §OREoraMda : ' 0:  ' ) + '  ' +
057990140410                    %editw( §OREoraMa  : ' 0:  ' );
058000140410
058010140410           // -?Ci sono due orari completi?
058020140410           When  §OREoraMda > *zero  and  §OREoraMa > *zero  and
058030140410                 §OREoraPda > *zero  and  §OREoraPa > *zero;
058040140410             Par5 = 'Orari apertura: ' +
058050140410                    %editw( §OREoraMda : ' 0:  ' ) + '  ' +
058060140410                    %editw( §OREoraMa  : ' 0:  ' ) + ' - ' +
058070140410                    %editw( §OREoraPda : ' 0:  ' ) + '  ' +
058080140410                    %editw( §OREoraPa  : ' 0:  ' );
058090140410
058100140410         EndSl;
058110140410
058120140410         exsr  StampaAR5;
058130140410
058140140410       ENDSR;
058150100513
058160100513       //--------------------------------------------------------------
058170100513       //?Verifica se presente ORM con preavviso mail                  ?
058180100513       //--------------------------------------------------------------
058190100513       BEGSR  sr_CheckORM_PrevvMail;
058200100514
058210100514         //clear  dOrm01               (già fatto)
058220100513
058230100513         // -?Reperimento num. ORM (se NON già reperito)?
058240100513         if  DB0tbo <> '8';
058250100513           exsr  RepAR4_M;
058260100513         endif;
058270100513
058280100513         // -?ORM inesistente: inutile proseguire?
058290100513         if  §B4poe = *zero    or   §B4nor = *zero;
058300100513           leavesr;
058310100513         endif;
058320100513
058330100513         // -?Verifica presenza O.R.M.?
058340100513         if  Not  %open(FNORM01L);
058350100513           open  FNORM01L;
058360100513         endif;
058370100513
058380100513         ORMpoe = §B4poe;
058390100513         ORMnsr = §B4nsr;
058400100513         ORMnor = §B4nor;
058410100513         ORMnrv = §B4nrv;
058420100513
058430100513         chain  K04ORM01  FNORM000;
058440100513
058450100513         // -?ORM non reperito: inutile proseguire?
058460100513         if  Not  %found(FNORM01L);
058470100513           leavesr;
058480100513         endif;
058490100513
058500100513         // -?Impostazione flag per stampa porto "ASSEGNATO"?
058510100513         dOrm01 = ORMflo;
058520100513         $PreavvORM = (§ORMpre = 'M');
058530100513
058540100513       ENDSR;
058550100513
058560100513      /end-free
058570030515      *
058580030515      *---------------------------------------------------------------*
058590050217      *?Stampa dati DPD                                              ?*
058600030515      *---------------------------------------------------------------*
058610030515     c     StampaDPD     BEGSR
058620030515      *
058630080922      * (EX) Bar-Code - non più stampato come codice a barre
058640030515      * (1' 12 char di Riferimento Mittente Alfanumerico - se numerici)
058650060626      *---
058660061030      * Il barcode DPD è stato variato ora può essere anche
058670061030      *   alfanumerico ed è lungo 14
058680061030      * Si trova sul file FIAR4 rcd 'I'
058690061030      * I controlli che faceva prima sono da cambiare
058700061030      *---
058710061030      *
058720060626      * nuovi controlli per nuovo barcode DPD
058730061030if  1c                   if        §B4Ipn <> *blanks
058740061030      * - controllo se vecchio o nuovo barcode
058750061030if  2c                   if        %subst(§B4Ipn:14:1) = *blanks
058760061030      * - - vecchio
058770061030     c                   eval      Pbcd2 = '01 ' + %subst(§B4Ipn:1:3) +
058780061030     c                                     ' '   + %subst(§B4Ipn:4:8)
058790061030      * - - nuovo
058800061030x   2c                   else
058810061030     c                   eval      Pbcd2 = '01 ' + %subst(§B4Ipn:1:4) +
058820061030     c                                     ' '   + %subst(§B4Ipn:5:10)
058830061030e   2c                   endif
058840061030e   1c                   endif
058850080922      /free
058860080922         // Impostazione data
058870080922         PdpA = 'Date ' + %editc(Pdsp:'Y');
058880080922
058890080922         // Impostazione numero colli
058900080922         PdpB = 'Parcel N. ' + %editc(Pncl:'Z');
058910080922
058920080922         // Impostazione numero parcel
058930080922         PdpC = §B4Ipn;
058940080922
058950080922         // Impostazione "costante" da stampare tra le Note
058960080922         PdpD = 'Controllare la corrispondenza';
058970080922         PdpE = 'del destinatario con l''indirizzo';
058980080922         PdpF = 'dell''etichetta DPD.';
058990080922
059000080922         // Impostaz. posizioni sulla copia per il vettore già stampata
059010080922         §DP1 = wPosIni + 1.100;
059020080922         §DP2 = §DP1 + 0.550;
059030080922         §DP3 = §DP2 + 0.120;
059040080922         §DP4 = §DP3 + 0.120;
059050080922         §DP5 = §DP4 + 0.120;
059060080922         §DP6 = §DP5 + 0.160;
059070080922         §DP7 = §DP6 + 0.130;
059080080922         §DP8 = §DP7 + 0.130;
059090080922      /end-free
059100080922      *
059110080922      * (inutile testare l'overflow perchè siamo in ex-A5)
059120080922     c                   write     A4prtDPD
059130030515      *
059140030515     c                   ENDSR
059150030515      *
059160030515      *---------------------------------------------------------------*
059170050217      *?Prepara elenco per box - Elenco "Package Tracking Number"    ?*
059180030515      *---------------------------------------------------------------*
059190030515     c     ListaFED      BEGSR
059200030515      *
059210030515      * Stampo il simbolo "FedEx" (sulla 1ª pagina):
059220030515      *   imposto le posizioni iniziali di stampa per le linee
059230030515      *   (orizzontali-§LOn e verticali-§LVn)
059240080912     c                   eval      §LO1   = 3.330
059250080912     c                   eval      §LO2   = §LO1 + 0.370
059260081023     c                   eval      §LV1   = §LO2 - 0.150
059270081023     c                   eval      §LV2   = §LO2 - 0.070
059280030515     c                   write     A4prtFED1
059290080818      *
059300080818      * Stampo le "costanti" di inizio lista
059310030515     c                   write     A4prtFED2
059320030515      *
059330030515      * Stampo elenco segnacolli FedEx
059340030709     c                   eval      §LO1   = §LO2 - 0.250
059350030515      * - per le bolle in partenza
059360030515if  1c                   IF           DB0tbo = 'P'
059370030515     c                             or DB0tbo = '8'
059380030515if  2c                   if        NOT %open(FNBLT01L)
059390030515     c                   open      FNBLT01L
059400030515e   2c                   endif
059410030515      * - per le bolle in arrivo
059420030515x   1c                   ELSE
059430070319if  2c                   if        NOT %open(FNART01L)
059440070319     c                   open      FNART01L
059450070319e   2c                   endif
059460030515e   1c                   ENDIF
059470070319if  1c                   if        NOT %open(FIARS01L)
059480070319     c                   open      FIARS01L
059490070319e   1c                   endif
059500030515     c                   eval      Wtrc = 'C'
059510030515     c                   clear                   Wnsc
059520030515     c                   clear                   Wawb
059530030515do  1c                   dou       Wawb = *blanks
059540030515     c                   exsr      StampaFED
059550030515e   1c                   enddo
059560051020      *
059570030515     c                   ENDSR
059580030515      *
059590030515      *---------------------------------------------------------------*
059600050217      *?Stampa singola riga relativa a FEDEX                         ?*
059610030515      *---------------------------------------------------------------*
059620030515     c     StampaFED     BEGSR
059630030515      *
059640030515      * Impostazione campi di stampa
059650030515     c                   clear                   Wawb
059660030515      * - Carico AWB FedEx
059670030515      * - - leggo segnacolli per bolle in partenza
059680030515if  1c                   IF           DB0tbo = 'P'
059690030515     c                             or DB0tbo = '8'
059700030515     c                   exsr      CarAWB_Blx
059710030515      * - - leggo segnacolli per bolle in arrivo
059720030515x   1c                   ELSE
059730030515     c                   exsr      CarAWB_Arx
059740030515e   1c                   ENDIF
059750030515     c     wAWB          cabeq     *blanks       End_FED
059760030515     c                   move      wAWB          Pptn
059770030515     c                   add       1             Psts
059780030515      *
059790030515      * Controllo se superata l'ultima riga
059800030515     c                   exsr      Test_OF
059810030709      *
059820030709      * Imposto le posizioni iniziali di stampa per le linee
059830030709      *   (orizzontali-§LOn e verticali-§LVn) e per i campi (§PSn)
059840030709     c                   eval      §LO1 = §LO1 + 0.250 - 0.008
059850030709     c                   eval      §LO2 = §LO1 + 0.250
059860030709     c                   eval      §LV1 = §LO1
059870030709     c                   eval      §LV2 = §LO2 - 0.100
059880030709     c                   eval      §PS1 = §LO1 + 0.200
059890030515      *
059900030515      * Stampa del singolo record
059910030515     c                   write     A4prtFED3
059920030515      *
059930030515     c     End_FED       ENDSR
059940030515      *
059950030515      *---------------------------------------------------------------*
059960051020      *?Reperimento AWB FedEx da file FNBLT e FIARS (bolle partenza) ?*
059970030515      *---------------------------------------------------------------*
059980030515     c     CarAWB_Blx    BEGSR
059990130215      *
060000130215      * Verifica se, al di là del codice bolla (vedi "$ "), esiste
060010130215      *   comunque una bolla mamma
060020130215     c                   exsr      sr_Mamma_FedEx
060030030515      *
060040030515      * Posizionamento
060050130215if  2c                   if        $Mamma = *off   and
060060130215     c                             $Mamma_FedEx = *off
060070040607     c     KLDVnsc       setgt     FNBLT000
060080070514x   2c                   else
060090070514     c     KLBLnsc       setgt     FNBLT000
060100070514e   2c                   endif
060110030515      *
060120030515      * Ciclo di lettura
060130030515do  1c                   DO        *hival
060140030515      *
060150130215if  2c                   if        $Mamma = *off   and
060160130215     c                             $Mamma_FedEx = *off
060170040607     c     KLDV          reade     FNBLT000
060180070514x   2c                   else
060190070514     c     KLBL          reade     FNBLT000
060200070514e   2c                   endif
060210030515if  2c                   if        %eof(FNBLT01L)
060220030515     c                   leave
060230030515e   2c                   endif
060240030515      * - per ogni segnacollo cerco l'AWB
060250060505     c     K05ARS01P     chain     FIARS000
060260060505if  2c                   if            %found(FIARS01L)
060270060505     c                             and %subst(ARSnot:1:1) >= *zeros
060280060505     c                   eval      wAWB = %subst(ARSnot:1:12)
060290060505     c                   eval      wNSC = ARSnsc
060300030515     c                   leave
060310030515e   2c                   endif
060320030515      *
060330030515e   1c                   ENDDO
060340030515      *
060350030515     c                   ENDSR
060360030515      *
060370030515      *---------------------------------------------------------------*
060380070319      *?Reperimento AWB FedEx da file FNART e FIARS (bolle arrivo)   ?*
060390030515      *---------------------------------------------------------------*
060400030515     c     CarAWB_Arx    BEGSR
060410130215      *
060420130215      * Verifica se, al di là del codice bolla (vedi "$ "), esiste
060430130215      *   comunque una bolla mamma
060440130215     c                   exsr      sr_Mamma_FedEx
060450030515      *
060460030515      * Posizionamento
060470130215if  2c                   if        $Mamma = *off   and
060480130215     c                             $Mamma_FedEx = *off
060490040607     c     KLDVnsc       setgt     FNART000
060500070514x   2c                   else
060510130215     c     KLBLnsc       setgt     FNART000
060520070514e   2c                   endif
060530030515      *
060540030515      * Ciclo di lettura
060550030515do  1c                   DO        *hival
060560030515      *
060570130215if  2c                   if        $Mamma = *off   and
060580130215     c                             $Mamma_FedEx = *off
060590040607     c     KLDV          reade     FNART000
060600070514x   2c                   else
060610070514     c     KLBL          reade     FNART000
060620070514e   2c                   endif
060630030515if  2c                   if        %eof(FNART01L)
060640030515     c                   leave
060650030515e   2c                   endif
060660030515      * - per ogni segnacollo cerco l'AWB
060670060505     c     K05ARS01A     chain     FIARS000
060680060505if  2c                   if            %found(FIARS01L)
060690030515     c                             and %subst(ARSnot:1:1) >= *zeros
060700030515     c                   eval      wAWB = %subst(ARSnot:1:12)
060710030515     c                   eval      wNSC = ARSnsc
060720030515     c                   leave
060730030515e   2c                   endif
060740030515      *
060750030515e   1c                   ENDDO
060760030515      *
060770030515     c                   ENDSR
060780130215      *
060790130215      /free
060800130215
060810130215       //--------------------------------------------------------------
060820130215       //?Verifica se la spedizione FedEx ha bolla mamma.              ?
060830130215       //--------------------------------------------------------------
060840130215       BEGSR  sr_Mamma_FedEx;
060850130215
060860130215         //$Mamma_FedEx = *off;          (già così)
060870130215
060880130215         If  Not $Mamma;
060890130215
060900130215           // -?Verifica se la spedizione ha comunque mamma?
060910130215           //  ?(al di là del codice bolla - vedi cod. "$ ")?
060920130215           if  not %open(FNLBL01L);
060930130215             open  FNLBL01L;
060940130215           endif;
060950130215           chain  (DB0aas : DB0lnp : DB0nrs : DB0nsp)  FNLBL000;
060960130215
060970130215           $Mamma_FedEx = %found(FNLBL01L);
060980130215
060990130215         EndIf;
061000130215
061010130215       ENDSR;
061020130215
061030130215      /end-free
061040040607      *
061050040607      *---------------------------------------------------------------*
061060080918      *?Stampa DDT / PACKING-LIST del mittente                       ?*
061070040607      *---------------------------------------------------------------*
061080040607     c     StampaDDT     BEGSR
061090130315      *
061100130315      * - Uscita in assenza del file FIAR801L
061110130315     c                   if        Not  $E_AR8
061120130315     c                   leavesr
061130130315     c                   endif
061140080825      *
061150080825      * Meglio togliere l'impostazione relativa ai segnacolli non seq.
061160080825      *   per far funzionare il 1° posizionamento nella subr. Rep_§LO2
061170080825     c                   reset                   $A4_x_ScNS
061180080918      /free
061190080923         *in45 = (DB0fa4 = 'P');
061200080923         *in46 = (DB0fa4 = 'P'   and   %rem(wNumLdv : 2) = *zero);
061210080918      /end-free
061220040607      *
061230040607      * Se già stampato qualcos'altro:
061240040607      *   forzo il salto alla pagina successiva
061250080926       // (SE NON si sta stampando SOLO una packing-list, nel qual caso
061260080926       //  il posizionamento nella pagina A4 verrà eseguito dagli
061270080926       //  indicatori 45 e 46)
061280080926if  1c                   if           (CurLineA4 > $MaxLine1
061290080926     c                             or  §LO2      > $LimOF1)
061300080926     c                             and NOT *in45
061310040607     c                   eval      CurLineA4 = wMaxLine
061320040607     c                   exsr      Test_OF
061330040607e   1c                   endif
061340040607      *
061350040607     c                   eval      $DDT_p = *on
061360040607      *
061370130315      /free
061380130315         // -?Posizionamento?
061390130315         clear  k06fiar801;
061400130315         k_AR8aas = DB0aas;
061410130315         k_AR8lnp = DB0lnp;
061420130315         k_AR8nrs = DB0nrs;
061430130315         k_AR8nsp = DB0nsp;
061440130315         k_AR8tmd = c_§PDF + '999';
061450130315         setgt  %kds( k06fiar801 : 5 )  FIAR8000;
061460130315      /end-free
061470130315      *
061480130315      * - Letura
061490040607do  1c                   dou          %eof(FIAR801L)
061500040607     c                             or AR8atb = *blanks
061510130308     c                             or AR8tmd <> c_§PDF
061520040607     c     KLDV          reade     FIAR8000
061530040607e   1c                   enddo
061540040607if  1c                   if        %eof(FIAR801L)
061550040607     c                   goto      NOprtDDT
061560040607e   1c                   endif
061570041207if  1c                   if        §AR8des   = *blanks
061580041207     c*** (già fatto:)   reset                   dAR8
061590041207     c                   reset                   TIBS02ds
061600041207     c                   eval      T02cod    = 'AR8'
061610041207     c                   movel(p)  AR8tmd        T02ke1
061620041207     c                   call      'TIBS02R'
061630041207     c                   parm                    KPJBA
061640041207     c                   parm                    TIBS02ds
061650041207if  2c                   if        T02err    = *blanks
061660041207     c                   movel     T02uni        dAR8
061670041207e   2c                   endif
061680041207e   1c                   endif
061690040607      *
061700040607sel 1c                   select
061710050217      * - Karnak o Titanedi o EDk (identiche)
061720080730w   1c                   when      §AR8tsd   = 'K1'
061730040607     c                   exsr      DDT_Karnak
061740050414      * - Logistica o Sait-Abrasivi
061750060627w   1c                   when      %subst(§AR8tsd:1:1) = 'L'
061760050218     c                   exsr      DDT_Logistica
061770050217      * - Arcte
061780050215w   1c                   when      AR8tmd    = 'ARCT'
061790050217     c                   exsr      DDT_Arcte
061800050214      * - Esprinet
061810060627w   1c                   when      §AR8tsd   = 'E1'
061820050214     c                   exsr      DDT_Esprinet
061830041207      * - dati generici (con logo)
061840041207w   1c                   when      §AR8tsd   = 'S2'
061850041207     c                   exsr      DDT_genericoL
061860041207      * - dati generici (senza logo)
061870041207w   1c***                when      §AR8tsd   = 'S1'
061880040607     c                   other
061890040607     c                   exsr      DDT_generico
061900040607e   1c                   endsl
061910040607      *
061920060921     c                   reset                   $FIAR8
061930040607     c                   reset                   $DDT_o
061940040607     c                   reset                   $DDT_p
061950040608     c                   reset                   $DDTlastPg
061960040608     c                   clear                   wOmaRowT
061970040608     c                   clear                   wOmaRowD
061980040607     c                   clear                   $AR8o1
061990040607      *
062000040607     c     NOprtDDT      ENDSR
062010040607      *
062020040607      *---------------------------------------------------------------*
062030050217      *?Stampa DDT del mittente KARNAK / TITANEDI                    ?*
062040040607      *---------------------------------------------------------------*
062050040607     c     DDT_Karnak    BEGSR
062060040607      *
062070080923      /free
062080080923         // Posizionamento iniziale all'interno del foglio A4
062090080923         write  A4prtSP1;
062100080923
062110080923         // Impostazione del nuovo limite x overflow
062120080923         select;
062130080923           when  NOT *in45;
062140080923             wMaxLine = $MaxLine3;
062150080923           when  *in45   and   NOT *in46;
062160080923             wMaxLine = $MaxLine3_pl1;
062170080923           when  *in45   and   *in46;
062180080923             wMaxLine = $MaxLine3_pl2;
062190080923         endsl;
062200080923      /end-free
062210040607      *
062220040607      * Impostazione testi fissi - - - - - - - - - - - - - - - - - - -*
062230040607      *
062240040607     c                   eval      PCtx1 = 'ATTENZIONE: VERIFICARE LA -
062250040607     c                             QUANTITA'' E L''INTEGRITA'' DEI CAR-
062260040607     c                             TONI. IN CASO DI DISCORDANZA RITIRA-
062270040607     c                             RE CON RISERVA.'
062280130118     c*//                eval      PCtx2 = 'La merce viagga a rischio -
062290130118     c*//                          e pericolo del committente anche se-
062300130118     c*//                           venduta franco destino. Trascorsi -
062310130118     c*//                          otto giorni dal ricevimento della m-
062320130118     c*//                          erce non si accettano reclami.   - -
062330130118     c*//                            IN CASO DI RITARDATO PAGAMENTO DE-
062340130118     c*//                          CORRERANNO GLI INTERESSI BANCARI IN-
062350130118     c*//                           VIGORE.'
062360130118     c*//                eval      PCtx3 = 'Per ogni vertenza l''acqui-
062370130118     c*//                          rente accetta la competenza della R-
062380130118     c*//                          epubblica di San Marino o in altern-
062390130118     c*//                          ativa del foro di Rimini.'
062400130118     c*//                eval      PCtx4 = 'NON SI ACCETTANO RESI E NO-
062410130118     c*//                          N SI EFFETTUANO SOSTITUZIONI SE NON-
062420130118     c*//                           AUTORIZZATI PER ISCRITTO DALLA NOS-
062430130118     c*//                          TRA DIREZIONE COMMERCIALE.'
062440130118     c*//                eval      PCtx5 = 'Quand''anche autorizzati, -
062450130118     c*//                          i resi verranno accettati e le sost-
062460130118     c*//                          ituzioni verranno effettuate solo d-
062470130118     c*//                          opo aver accertato il perfetto stat-
062480130118     c*//                          o nel nostro stabilimento della mer-
062490130118     c*//                          ce restituita.'
062500130118     c                   eval      PCtx2 = 'Eventuali reclami per mate-
062510130118     c                             riale mancante o danneggiato all''i-
062520130118     c                             nterno dei colli dovranno pervenire-
062530130118     c                              entro 8 (otto) giorni di calendari-
062540130118     c                             o dall''avvenuta consegna a MyO S.r-
062550130118     c                             .l. - Ufficio Resi - Via Santarcang-
062560130118     c                             iolese, 6 - 47825 Torriana (RN)'
062570130118     c                   eval      PCtx4 = 'Eventuali mancanze di coll-
062580130118     c                             i e/o avarie sono da contestare all-
062590130118     c                             ''atto della consegna con annotazio-
062600130118     c                             ne su lettera di vettura e DDT'
062610040709if  1c                   if        AR8tmd = 'TITA'
062620040709     c                   eval      PCtx6 = 'Depositario: K & K Logisti-
062630040709     c                             cs S.p.a. - via Santarcangiolese, 6-
062640040709     c                              - 47825 Torriana (RN) - P.I. 00953-
062650040709     c                             860400'
062660040709e   1c                   endif
062670080822      *
062680040607      * Conteggio delle righe "omaggio" - - - - - - - - - - - - - - -*
062690040607      *
062700040614     c                   clear                   xx
062710040607     c                   eval      SAVprr = AR8prr
062720040607     c     KLDV          setgt     FIAR8000
062730040607     c     KLDV          readpe    FIAR8000
062740040607do  1c                   dow       NOT %eof(FIAR801L)
062750040607sel 2c                   select
062760040607w   2c                   when      AR8atb <> *blanks
062770040611w   2c                   when      AR8trk =  'O2'
062780040608     c                   add       1             wOmaRowD
062790040611w   2c                   when      AR8trk =  'O1'
062800040608     c                   add       1             wOmaRowT
062810051110     c                   movel(p)  AR8uni        dAR8O1
062820040607     c                   add       1             xx
062830040607     c                   eval      $AR8o1(xx) =  §AR8O1a
062840040607x   2c                   other
062850040607     c                   leave
062860040607e   2c                   endsl
062870040607     c     KLDV          readpe    FIAR8000
062880040607e   1c                   enddo
062890040607      * - mi riposiziono
062900040607     c                   eval      AR8prr = SAVprr
062910040607     c     K06AR801      setll     FIAR8000
062920040607     c     K06AR801      reade     FIAR8000
062930040607      *
062940040607      * Reperimento dati, impostazione e stampa DDT mittente  - - - -*
062950040607      *
062960040607if  1c                   dow       NOT %eof(FIAR801L)
062970040607      *
062980040607sel 2c                   select
062990040607      * - Record annullato (non si stampa nulla)
063000040607w   2c                   when      AR8atb <> *blanks
063010040607      * - Dati di dettaglio: articolo, descrizione, quantità
063020040607w   2c                   when      AR8trk = 'C1'
063030050217     c                   exsr      PrtDdtKARN_C1
063040040607      * - Dati di dettaglio: testo per articoli non disponibili
063050040607w   2c                   when      AR8trk = 'C2'
063060050217     c                   exsr      PrtDdtKARN_C2
063070040607      * - Dati di dettaglio: descrizioni generiche (orario apertura)
063080040607w   2c                   when      AR8trk = 'C3'
063090050217     c                   exsr      PrtDdtKARN_C3
063100040607      * - Dati di testata: destinatario, cliente di fatturazione
063110040607w   2c                   when      AR8trk = 'T1'
063120050217     c                   exsr      PrtDdtKARN_T1
063130040607      * - Dati di testata: agente, numero ordine, estremi documento
063140040607w   2c                   when      AR8trk = 'T2'
063150050217     c                   exsr      PrtDdtKARN_T2
063160040607      * - Dati di fondo: mezzo e causale trasporto, colli, peso
063170040607w   2c                   when      AR8trk = 'V1'
063180050217     c                   exsr      PrtDdtKARN_V1
063190040607      * - Dati di fondo: vettore, annotazioni e note
063200040607w   2c                   when      AR8trk = 'V2'
063210050217     c                   exsr      PrtDdtKARN_V2
063220040607      * - Dati esterni: riga descrittiva con referente
063230040607w   2c                   when      AR8trk = 'L1'
063240050217     c                   exsr      PrtDdtKARN_L1
063250040607      * - Dati di omaggio: testo per articoli omaggiati
063260040607w   2c                   when      AR8trk = 'O1'
063270050217     c                   exsr      PrtDdtKARN_O1
063280040607      * - Dati di omaggio: articolo, descrizione, quantità
063290040607w   2c                   when      AR8trk = 'O2'
063300050217     c                   exsr      PrtDdtKARN_O2
063310040607e   2c                   endsl
063320040607      *
063330040607     c     KLDV          reade     FIAR8000
063340040607      *
063350040607e   1c                   enddo
063360040607      *
063370040607      * Reimpostazione del vecchio limite x overflow
063380040607     c                   eval      wMaxLine = $MaxLine2
063390040607      *
063400040607     c                   ENDSR
063410050214      *
063420040607      *---------------------------------------------------------------*
063430050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
063440050217      *?Dati di testata: destinatario, cliente di fatturazione       ?*
063450040607      *---------------------------------------------------------------*
063460050217     c     PrtDdtKARN_T1 BEGSR
063470040607      *
063480040607      * > logo
063490040607     c                   eval      §Logo  = 'LOGO' + AR8tmd
063500040607      *
063510051110     c                   movel(p)  AR8uni        dAR8T1
063520040607      *
063530040607      * > destinatario
063540040607     c                   eval      PCrsd  = §AR8t1a
063550040607     c                   eval      PCind  = §AR8t1b
063560040616if  1c                   if        §AR8t1c  > *zeros
063570040607     c                   eval      PCcad  = §AR8t1c
063580040616e   1c                   endif
063590040607     c                   eval      PClod  = §AR8t1d
063600040607     c                   eval      PCprd  = §AR8t1e
063610040607     c***                eval      PCnzd  = *blanks
063620040607      * > cliente di fatturazione
063630040607     c                   eval      PCrsf  = §AR8t1f
063640040607     c                   eval      PCinf  = §AR8t1g
063650040616if  1c                   if        §AR8t1h  > *zeros
063660040607     c                   eval      PCcaf  = §AR8t1h
063670040616e   1c                   endif
063680040607     c                   eval      PClof  = §AR8t1i
063690040607     c                   eval      PCprf  = §AR8t1j
063700040607     c***                eval      PCnzf  = *blanks
063710040607      *
063720040607     c                   ENDSR
063730040607      *
063740040607      *---------------------------------------------------------------*
063750050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
063760050217      *?Dati di testata: agente, ..., porto, ...                     ?*
063770040607      *---------------------------------------------------------------*
063780050217     c     PrtDdtKARN_T2 BEGSR
063790040607      *
063800051110     c                   movel(p)  AR8uni        dAR8T2
063810040607      *
063820040607      * > dati in testata DDT
063830040607      * - - codice agente
063840130122     c*//                eval      PCage  = §AR8t2a
063850040607      * - - numero e data ordine
063860040607     c                   eval      PCnro  = §AR8t2b
063870040607     c                   eval      PCdto  = §AR8t2c
063880040607      * - - riferimenti
063890040607     c                   eval      PCrif  = §AR8t2d
063900040607      * - - codice destinatario (dal mittente)
063910040611     c                   eval      PCcli  = §AR8t2e
063920040607      * - - numero e data documento (DDT)
063930040607     c                   eval      PCnrd  = §AR8t2f
063940040607     c                   eval      PCdtd  = §AR8t2g
063950040607      * - - numero pagina documento originale (qui ricalcolato)
063960040607     c***                eval      Page   = §AR8t2h
063970040607      * - - porto
063980040607     c                   eval      PCpto  = §AR8t2i
063990040607      * - - condizioni di pagamento
064000040611     c                   eval      PCpag  = §AR8t2j
064010040607     c                                    + ' '
064020040607     c                                    + %trim(§AR8t2k)
064030040607      * - - tipologia spedizione
064040040607     c                   eval      PCtsp  = §AR8t2l
064050040607      *
064060040607      * > Stampa testata DDT - - - - - - - - - - - - - - - - - - - - -*
064070040607      *
064080040607      *   - OverFlow impossibile qui! -
064090040607     c                   write     A4prtDK1
064100040607      *
064110040607     c                   ENDSR
064120040607      *
064130040607      *---------------------------------------------------------------*
064140050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
064150050217      *?Dati di dettaglio: articolo, descrizione, quantità           ?*
064160040607      *---------------------------------------------------------------*
064170050217     c     PrtDdtKARN_C1 BEGSR
064180040607      *
064190040607     c                   clear                   A4prtDK2
064200051110     c                   movel(p)  AR8uni        dAR8C1
064210040607      *
064220040607      * > dati in dettaglio DDT
064230040607      * - - codice prodotto
064240040607     c                   eval      PCcpr  = §AR8c1a
064250040607      * - - descrizione prodotto
064260040607     c                   eval      PCdpr  = §AR8c1b
064270040607      * - - quantità
064280040607     c                   eval      PCqta  = '     ' + §AR8c1c
064290040607      * - - note
064300040607     c***                eval      PCnot  = *blanks
064310040607      *
064320040607      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
064330040607      *
064340040607     c                   exsr      Test_OF
064350040607     c                   write     A4prtDK2
064360040607      *
064370040607     c                   ENDSR
064380040607      *
064390040607      *---------------------------------------------------------------*
064400050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
064410050217      *?Dati di dettaglio: testo per articoli non disponibili        ?*
064420040607      *---------------------------------------------------------------*
064430050217     c     PrtDdtKARN_C2 BEGSR
064440040607      *
064450040607     c                   clear                   A4prtDK3
064460051110     c                   movel(p)  AR8uni        dAR8C2
064470040607      *
064480040607      * > dati in note DDT
064490040607      * - - riga descrittiva
064500040607     c                   eval      PCtxt  = §AR8c2a
064510040607      *
064520040607      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
064530040607      *
064540040607     c                   exsr      Test_OF
064550040607     c                   write     A4prtDK3
064560040607      *
064570040607     c                   ENDSR
064580040607      *
064590040607      *---------------------------------------------------------------*
064600050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
064610050217      *?Dati di dettaglio: descrizioni generiche (orario apertura)   ?*
064620040607      *---------------------------------------------------------------*
064630050217     c     PrtDdtKARN_C3 BEGSR
064640040607      *
064650040607     c                   clear                   A4prtDK2
064660051110     c                   movel(p)  AR8uni        dAR8C3
064670040607      *
064680040607      * > dati in note DDT
064690040607      * - - riga descrittiva
064700040607     c                   eval      PCdpr  = §AR8c3a
064710040607      *
064720040607      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
064730040607      *
064740040607     c                   exsr      Test_OF
064750040607     c                   write     A4prtDK2
064760040607      *
064770040607     c                   ENDSR
064780040607      *
064790040607      *---------------------------------------------------------------*
064800050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
064810050217      *?Dati di fondo: mezzo e causale trasporto, colli, peso        ?*
064820040607      *---------------------------------------------------------------*
064830050217     c     PrtDdtKARN_V1 BEGSR
064840040607      *
064850051110     c                   movel(p)  AR8uni        dAR8V1
064860040607      *
064870040607      * Controllo se ci saranno da stampare tante righe omaggio da
064880040607      *   chiudere in pagina successiva:
064890040608      *   i campi del box finale del DDT (in totale 9 righe) verranno
064900040607      *   riempiti solo nella ultima pagina.
064910040607      *
064920080924      /free
064930080924         select;
064940080924           when  NOT *in45;
064950080924      /end-free
064960040608     c                   eval      $DDTlastPg = ($MaxLine2
064970040713     c                                        +  1
064980040608     c                                        -  CurLineA4
064990040608     c                                        -  $BoxRow1
065000040608     c                                        -  wOmaRowT
065010040608     c                                        >= wOmaRowD)
065020080924      /free
065030080924           when  *in45   and   NOT *in46;
065040110406             $DDTlastPg = ($MaxLine2_pl1k
065050080924                           + 1 - CurLineA4 - $BoxRow1 - wOmaRowT
065060080924                           >= wOmaRowD);
065070080924           when  *in45   and   *in46;
065080110406             $DDTlastPg = ($MaxLine2_pl2k
065090080924                           + 1 - CurLineA4 - $BoxRow1 - wOmaRowT
065100080924                           >= wOmaRowD);
065110080924         endsl;
065120080924      /end-free
065130040607      *
065140040607      * > dati in chiusura DDT
065150040607      *   Per ora imposto (ho già impostato) solo la DS "dAR8V1".
065160040607      *   I corrispondenti campi di stampa li impostero nella subr.
065170050217      *     "PrtDdtKARN_V2".
065180040607      *
065190040607     c                   ENDSR
065200040607      *
065210040607      *---------------------------------------------------------------*
065220050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
065230050217      *?Dati di fondo: vettore, annotazioni e note                   ?*
065240040607      *---------------------------------------------------------------*
065250050217     c     PrtDdtKARN_V2 BEGSR
065260040607      *
065270051110     c                   movel(p)  AR8uni        dAR8V2
065280040618      *
065290040618      * > dati in chiusura DDT
065300040618      *
065310040607      * Ho già controllato se ci saranno da stampare tante righe
065320040607      *   omaggio da chiudere in pagina successiva
065330040608if  1c                   if        $DDTlastPg = *off
065340040607     c                   eval      PCnv2  = '          '
065350040607     c                                    + '*** SEGUE ***'
065360040618x   1c                   else
065370050217     c                   exsr      PrtDdtKARNaV2
065380040607e   1c                   endif
065390040607      *
065400040607      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
065410040608      *
065420050217     c                   exsr      PrtDdtKARNbV2
065430040607      *
065440040607      * > Impostazione nuovo limite di OverFlow  - - - - - - - - - - -*
065450080923      /free
065460080923         select;
065470080923           when  NOT *in45;
065480080923             wMaxLine = $MaxLine2;
065490080923           when  *in45   and   NOT *in46;
065500080923             wMaxLine = $MaxLine2_pl1;
065510080923           when  *in45   and   *in46;
065520080923             wMaxLine = $MaxLine2_pl2;
065530080923         endsl;
065540080923      /end-free
065550040607      *
065560040608      * N.B.: la copia per il destinatario della LDV deve comparire
065570040608      *       nell'ultima pagina del DDT per il cliente (vedi subr.
065580040608      *       "ChiusuraLDV")
065590040607      *
065600040607     c                   ENDSR
065610040618      *
065620040618      *---------------------------------------------------------------*
065630040618      *
065640050217     c     PrtDdtKARNaV2 BEGSR
065650040618      *
065660040618      * > dati in chiusura DDT
065670040618      *
065680040618      * > > da dAR8V1
065690040618      * - - mezzo di trasporto
065700040618     c                   movel     §AR8v1a       PCmtr
065710040618      * - - causale trasporto
065720040618     c                   eval      PCctr  = §AR8v1b
065730040618      * - - numero colli
065740040618     c                   eval      PCncl  = §AR8v1c
065750040618      * - - peso in kg
065760040618     c                   eval      PCpkg  = §AR8v1d
065770040618      * - - aspetto esteriore dei beni
065780040618     c                   eval      PCaeb  = §AR8v1e
065790040618      *
065800040618      * > > da dAR8V2
065810120530      * - - dati vettore: · ragione sociale (BRT S.p.A.)
065820040618      *                   · indirizzo
065830040618      *                   · c.a.p./città/provincia
065840040618      *     => NON con i dati che manda il mittente !!!
065850040618      *        ...Vuoi mai che si sbagli ad inviarci della roba !!!
065860040618     c***                eval      PCvet  = %trim(§AR8v2a)
065870040618     c***                                 + ' - '
065880040618     c***                                 + %trim(§AR8v2b)
065890040618     c***                                 + ' - '
065900040618     c***                                 + %trim(§AR8v2c)
065910120530     c*//                if        (DB0dft > *zero       and
065920120530     c*//                           DB0dft > §vpoBRTdta)  OR
065930120530     c*//                          (DB0dft = *zero       and
065940120530     c*//                           Wdsp   > §vpoBRTdta)
065950110426     c                   eval      PCvet  = $BRT
065960110426     c                                    + ' - '
065970110426     c                                    + $Intest2
065980110426     c                                    + ' (BO)'
065990120530     c*//                else
066000120530     c*//                eval      PCvet  = $Bartolini
066010120530     c*//                                 + ' - '
066020120530     c*//                                 + $Intest2
066030120530     c*//                                 + ' (BO)'
066040120530     c*//                endif
066050040618      * - - data ritiro
066060040618     c*** non stampo:    eval      PCdtv  = §AR8v2d
066070040618      * - - orario ritiro
066080040618     c*** non stampo:    eval      PChmv  = §AR8v2e
066090040618      * - - annotazioni / variazioni
066100040618     c                   eval      PCvea  = §AR8v2f
066110040618      * - - note
066120040618     c                   eval      PCnv1  = §AR8v2g
066130040618     c                   eval      PCnv2  = §AR8v2h
066140040618      *
066150040618     c                   ENDSR
066160040618      *
066170040618      *---------------------------------------------------------------*
066180040618      *
066190050217     c     PrtDdtKARNbV2 BEGSR
066200040618      *
066210040618      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
066220040618      *
066230040618      * - - Calcolo la posizione finale del box
066240040618     c                   exsr      Posiz_DK
066250040618      *
066260040618      * - - Stampa parte finale DDT
066270040618     c                   write     A4prtDK4
066280040618      *
066290040618      * > Stampa logo, box, linee e testi fissi  - - - - - - - - - - -*
066300081017     c                   if            §AR8cxd = *blanks
066310081017     c                             or  DB0fa4  = 'P'
066320050331     c                   write     A4prtDT0
066330050331     c                   endif
066340040618     c                   write     A4prtDK0
066350040618      *
066360040618     c                   ENDSR
066370040607      *
066380040607      *---------------------------------------------------------------*
066390050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
066400050217      *?Dati esterni: riga descrittiva con referente                 ?*
066410040607      *---------------------------------------------------------------*
066420050217     c     PrtDdtKARN_L1 BEGSR
066430040607      *
066440051110     c                   movel(p)  AR8uni        dAR8L1
066450040607      *
066460040607      * > righe descrittive esterne al DDT
066470040607      * - - riga descrittiva
066480040607     c                   eval      PCtxx  = §AR8l1a
066490040607      *
066500040607      * > Stampa riga testo finale DDT - - - - - - - - - - - - - - - -*
066510040618      *   - OverFlow già testato per questa riga -
066520040607     c                   write     A4prtDK5
066530040607      *
066540040607     c                   ENDSR
066550040607      *
066560040607      *---------------------------------------------------------------*
066570050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
066580050217      *?Dati di omaggio: testo per articoli omaggiati                ?*
066590040607      *---------------------------------------------------------------*
066600050217     c     PrtDdtKARN_O1 BEGSR
066610040608      *
066620040608     c                   eval      $DDT_o = *on
066630040607      *
066640040607     c                   clear                   A4prtDK5
066650051110     c                   movel(p)  AR8uni        dAR8O1
066660040607      *
066670040607      * > righe descrittive esterne al DDT
066680040607      * - - riga descrittiva
066690040607     c                   eval      PCtxx  = §AR8o1a
066700040607      * - - nessuna sottolineatura
066710040608     c***                eval      *in39  = *off
066720040607      *
066730040607      * > Stampa testata omaggi in DDT - - - - - - - - - - - - - - - -*
066740040608      *   (se NON già stampata nella nuova pagina x OverFlow)
066750040608     c                   eval      SavLineA4 = CurLineA4
066760040607     c                   exsr      Test_OF
066770040608if  1c                   if        CurLineA4 = SavLineA4
066780040607     c                   write     A4prtDK5
066790040608e   1c                   endif
066800040608      * ...Dovendo ristampare le righe di intestazione degli omaggi
066810040608      *   in ogni pagina (da quella del 1° omaggio in poi), non calcolo
066820040608      *   quelle già stampate...
066830040608     c*** NO:            sub       1             wOmaRowT
066840040607      *
066850040607     c                   ENDSR
066860040607      *
066870040607      *---------------------------------------------------------------*
066880050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
066890050217      *?Dati di omaggio: articolo, descrizione, quantità             ?*
066900040607      *---------------------------------------------------------------*
066910050217     c     PrtDdtKARN_O2 BEGSR
066920040607      *
066930040607     c                   clear                   A4prtDK2
066940051110     c                   movel(p)  AR8uni        dAR8O2
066950040607      *
066960040607      * > dettaglio omaggi DDT
066970040607      * - - codice prodotto
066980040607     c                   eval      PCcpr  = §AR8o2a
066990040607      * - - descrizione prodotto
067000040607     c                   eval      PCdpr  = §AR8o2b
067010040607      * - - quantità
067020040618     c                   eval      PCqta  = '     ' + §AR8o2c
067030040607      * - - note
067040040607     c***                eval      PCnot  = *blanks
067050040607      *
067060040607      * > Stampa dettaglio omaggi in DDT - - - - - - - - - - - - - - -*
067070040607      *
067080040607     c                   exsr      Test_OF
067090040607     c                   write     A4prtDK2
067100040608     c                   sub       1             wOmaRowD
067110040607      *
067120040607     c                   ENDSR
067130040608      *
067140040608      *---------------------------------------------------------------*
067150081002      *?Impostazione nuova pagina di DDT Karnak / Titanedi / EDk     ?*
067160040608      *---------------------------------------------------------------*
067170081002     c     NewPageDK     BEGSR
067180040608      *
067190080923      /free
067200080923         // Impostazione del nuovo limite x overflow
067210080923         select;
067220080923           when  NOT *in45;
067230080923             wMaxLine = $MaxLine3;
067240080923           when  *in45   and   NOT *in46;
067250080923             wMaxLine = $MaxLine3_pl1;
067260080923           when  *in45   and   *in46;
067270080923             wMaxLine = $MaxLine3_pl2;
067280080923         endsl;
067290080923      /end-free
067300080923     c                   write     A4prtSP1
067310040608     c                   write     A4prtDK1
067320040608      *
067330040608if  1c                   if        $DDT_o = *on
067340040608      *
067350040608      * - Stampa 1 riga di dettaglio vuota
067360040608     c                   clear                   A4prtDK3
067370040608     c                   write     A4prtDK3
067380040618      * - Reimposta i dati nel box finale del DDT
067390050217     c                   exsr      PrtDdtKARNaV2
067400040618      * - Stampa fondo DDT
067410050217     c                   exsr      PrtDdtKARNbV2
067420040608      * - Stampa ultima riga
067430040608      *   ("Per qualsiasi problema o chiarimento contattare...)
067440040608     c                   clear                   A4prtDK5
067450040608     c                   eval      PCtxx  = §AR8l1a
067460040608     c                   write     A4prtDK5
067470040608      * - Stampa intestazione per merce omaggio
067480040608      *   xx = numero di righe per "intestazione omaggi"
067490040608     c                   eval      wx     = xx
067500040608do  2c                   dow           xx         >  *zeros
067510040608     c                             and xx         <= %elem($AR8o1)
067520040608     c                             and $AR8o1(xx) <> *blanks
067530040608     c                   eval      PCtxx  = $AR8o1(xx)
067540040608     c                   write     A4prtDK5
067550040608     c                   eval      xx     = xx - 1
067560040608e   2c                   enddo
067570040608     c                   eval      xx     = wx
067580040608      * -Se sta leggendo il tipo record "O1":?
067590040608      *   sposta il ciclo di lettura del file FIAR8 sul record
067600040608      *   successivo all'ultimo (appena stampato) dell'intestazione
067610040608      *   della merce omaggiata
067620040608if  2c                   if        AR8trk =  'O1'
067630040608if  3c                   dou       AR8trk <> 'O1'
067640040608     c     KLDV          reade     FIAR8000
067650040608e   3c                   enddo
067660040608if  3c*** è così:        if        AR8trk = 'O2'
067670040608     c     KLDV          readpe    FIAR8000
067680040707e   3c***                endif
067690040608e   2c                   endif
067700040608      * - Aggiorna controllo se "ultima pagina"
067710080924      /free
067720080924           select;
067730080924             when  NOT *in45;
067740080924      /end-free
067750040608     c                   eval      $DDTlastPg = ($MaxLine2
067760040713     c                                        +  1
067770040608     c                                        -  CurLineA4
067780040608     c                                        -  $BoxRow1
067790040608     c                                        -  wOmaRowT
067800040608     c                                        >= wOmaRowD)
067810080924      /free
067820080924             when  *in45   and   NOT *in46;
067830110406               $DDTlastPg = ($MaxLine2_pl1k
067840080924                             + 1 - CurLineA4 - $BoxRow1 - wOmaRowT
067850080924                             >= wOmaRowD);
067860080924             when  *in45   and   *in46;
067870110406               $DDTlastPg = ($MaxLine2_pl2k
067880080924                             + 1 - CurLineA4 - $BoxRow1 - wOmaRowT
067890080924                             >= wOmaRowD);
067900080924           endsl;
067910080924      /end-free
067920040608      *
067930040608e   1c                   endif
067940040608      *
067950040608     c                   ENDSR
067960040607      *
067970040607      *---------------------------------------------------------------*
067980050414      *?Posizionamento di linee e box nel DDT KARNAK / TITANEDI / EDk?*
067990040607      *---------------------------------------------------------------*
068000040607     c     Posiz_DK      BEGSR
068010040607      *
068020080918      /free
068030080923         // Impostazione posizioni di logo/box
068040080923         // (posizioni di "partenza" diverse a seconda del posizionam.
068050080923         //  iniziale nel foglio A4 attualmente in stampa)
068060080923         select;
068070080923           when  NOT *in45;
068080081002             §PB1 =  3.120;
068090080923           when  *in45   and   NOT *in46;
068100081002             §PB1 = 0.250;
068110080923           when  *in45   and   *in46;
068120081002             §PB1 = 6.000;
068130080923         endsl;
068140080918      /end-free
068150040607     c                   eval      §PB2 =  §PB1 + 1.23
068160080822     c                   eval      §PB3 =  §PB2 + 0.18
068170040607     c                   eval(h)   §PB4 =  CurLineA4 / 8 + 0.20
068180040607     c                   eval      §PB5 =  §PB4 + 0.18
068190040607     c                   eval      §PB6 =  §PB5 + 0.77
068200040607     c                   eval      §PB7 =  §PB5 + 0.165
068210040607     c                   eval      §PB8 =  §PB7 + 0.08
068220040607      * Impostazione posizioni di linee orizzontali nel 1° box
068230040607     c                   eval(h)   §LO1 =  §PB1 + (§PB2 - §PB1) / 2
068240040607      * Impostazione posizioni di linee orizzontali nel 2° box
068250080822     c                   eval      §LO2 =  §PB3 + 0.23
068260080822     c                   eval      §LO3 =  §LO2 + 0.145
068270080822     c                   eval      §LO4 =  §LO3 + 0.10
068280040607     c                   eval      §LO5 =  §PB4 - 0.15
068290040607      * Impostazione posizioni di linee orizzontali nel 3° box
068300040607     c                   eval      §LO6 =  §PB5 + 0.14
068310040607     c                   eval      §LO7 =  §LO6 + 0.12
068320040607     c                   eval      §LO8 =  §LO7 + 0.12
068330040607     c                   eval      §LO9 =  §LO8 + 0.13
068340040607      * Impostazione posizioni di linee verticali nel 2° box
068350040607     c                   eval      §LV1 =  §LO2 - §PB3
068360040607     c                   eval      §LV2 =  §LO3 - §LO2
068370040607     c                   eval      §LV3 =  §LO5 - §LO3
068380040607      * Impostazione posizioni di linee verticali nel 3° box
068390040607     c                   eval      §LV4 =  §LO6 - §PB5
068400040607     c                   eval      §LV5 =  §LO7 - §LO6
068410040607     c                   eval      §LV6 =  §LO8 - §LO7
068420040607     c                   eval      §LV7 =  §PB6 - §LO9
068430040607      * Impostazione posizioni di campi
068440040607     c                   eval      §PS1 =  §LO5 + 0.11
068450040607     c                   eval      §PS2 =  §PB4 + 0.08
068460040607     c                   eval      §PS3 =  §PS2 + 0.07
068470130118if  1c*//                if        §PB6 <  §PB1 + 4.58
068480130118     c*//                eval      §PR1 =  §PB1 + 4.58
068490130118if  1c                   if        §PB6 <  §PB1 + 3.30
068500130118     c                   eval      §PR1 =  §PB1 + 3.30
068510040607x   1c                   else
068520040607     c                   eval      §PR1 =  §PB6
068530040607e   1c                   endif
068540040607      *
068550040607     c                   ENDSR
068560041019      *
068570041019      *---------------------------------------------------------------*
068580050414      *?Stampa DDT del mittente LOGISTICA / SAIT ABRASIVI            ?*
068590041019      *---------------------------------------------------------------*
068600050218     c     DDT_Logistica BEGSR
068610041019      *
068620080924      /free
068630080924         // Impostazione del nuovo limite x overflow
068640080924         // (Codesto limite è valido solo per la stampa del dettaglio.
068650080924         //   Per la stampa del "fondo" bolla occorre REimpostarlo ad
068660080924         //   un valore più basso.
068670080924         //  Ovvero: stampo più righe di dettaglio che posso; se alla
068680080924         //  fine delle righe di dettaglio non rimane spazio a suffi-
068690080924         //  cienza per il fondo: prima della stampa del fondo si salta
068700080924         //  alla pagina successiva, dove non si stampa alcuna riga di
068710080924         //  dettaglio.)
068720080924         select;
068730080924           when  NOT *in45;
068740090323             wMaxLine = $MaxLine2;
068750080924           when  *in45   and   NOT *in46;
068760090323             wMaxLine = $MaxLine2_pl1;
068770080924           when  *in45   and   *in46;
068780090323             wMaxLine = $MaxLine2_pl2;
068790080924         endsl;
068800080924
068810080924         // Posizionamento iniziale all'interno del foglio A4
068820080924         write  A4prtSP1;
068830081006
068840081006         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
068850081006           write  A4prtNUL;
068860081006         endif;
068870080924      /end-free
068880041019      *
068890041019      * Impostazione testi fissi - - - - - - - - - - - - - - - - - - -*
068900041019      *
068910071008      * - prima solo per "Logistica", ora neanche più per lei...
068920071008      * - Sait Abrasivi SpA & Brumar Srl hanno Tipo Stampa DDT = "LC"
068930071008if  1c***                if        §AR8tsd <> 'LC'
068940071008     c***                eval      PCLtx1 = 'La merce viagga a rischio -
068950071008     c***                          e pericolo del Committente, pertanto -
068960071008     c***                          la Venditrice non risponde di -
068970071008     c***                          eventuali perdite o avarie derivanti -
068980071008     c***                          dal trasporto.'
068990071008     c***                eval      PCLtx2 = 'Il Destinatario, per -
069000071008     c***                          ottenere il risarcimento del danno, -
069010071008     c***                          deve far constatare al Corriere la -
069020071008     c***                          perdita o l''avaria all''atto del -
069030071008     c***                          ricevimento della merce.'
069040071008e   1c***                endif
069050050218      *
069060130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
069070130315      *  "StampaDDT")
069080050218     c***  KLDV          setll     FIAR8000       (già fatto)
069090050218     c***  KLDV          reade     FIAR8000       (già fatto)
069100041019      *
069110041019      * Reperimento dati, impostazione e stampa DDT mittente  - - - -*
069120041019      *
069130041019if  1c                   dow       NOT %eof(FIAR801L)
069140050318      *
069150050318if  2c                   if        AR8atb   = *blanks
069160050318     c                   eval      SAVtrk   = AR8trk
069170050318e   2c                   endif
069180041019      *
069190041019sel 2c                   select
069200041019      * - Record annullato (non si stampa nulla)
069210050218w   2c                   when      AR8atb  <> *blanks
069220041019      * - Dati di dettaglio: articolo, descrizione, quantità
069230050218w   2c                   when      AR8trk   = 'C1'
069240050217     c                   exsr      PrtDdtLOG1_C1
069250041103      * - Dati di dettaglio: descrizioni generiche
069260050218w   2c                   when      AR8trk   = 'C2'
069270050217     c                   exsr      PrtDdtLOG1_C2
069280041103      * - Dati di dettaglio: descrizioni generiche
069290050218w   2c                   when      AR8trk   = 'C3'
069300050217     c                   exsr      PrtDdtLOG1_C3
069310060614      * - Dati di dettaglio: corpo bolla non fincato
069320060614w   2c                   when      AR8trk   = 'C4'
069330060614     c                   exsr      PrtDdtLOG1_C4
069340090323      * - Dati di dettaglio: descrizioni generiche
069350090323w   2c                   when      AR8trk   = 'C5'
069360090323     c                   exsr      PrtDdtLOG1_C5
069370050318      * - Dati di packing-list: dettaglio
069380050318w   2c                   when      AR8trk   = 'Z1'
069390050318     c                   exsr      PrtDdtLOG1_Z1
069400041019      * - Dati di testata: sito e depositante
069410050218w   2c                   when      AR8trk   = 'T1'
069420050217     c                   exsr      PrtDdtLOG1_T1
069430041019      * - Dati di testata: destinatario, cliente di fatturazione
069440050218w   2c                   when      AR8trk   = 'T2'
069450050217     c                   exsr      PrtDdtLOG1_T2
069460041019      * - Dati di testata: agente, numero ordine, estremi documento
069470050218w   2c                   when      AR8trk   = 'T3'
069480050217     c                   exsr      PrtDdtLOG1_T3
069490041019      * - Dati di fondo: mezzo e causale trasporto, colli, peso
069500050218w   2c                   when      AR8trk   = 'P1'
069510050217     c                   exsr      PrtDdtLOG1_P1
069520050606      * - Dati di testata: FATTURA
069530050606w   2c                   when      AR8trk   = 'S1'
069540050606     c                   exsr      PrtDdtLOG1_S1
069550050606      * - Dati di dettaglio cpi 15: FATTURA
069560050606w   2c                   when      AR8trk   = 'S2'
069570050606     c                   exsr      PrtDdtLOG1_S2
069580050606      * - Dati di dettaglio cpi 20: FATTURA
069590050606w   2c                   when      AR8trk   = 'S3'
069600050606     c                   exsr      PrtDdtLOG1_S3
069610050606      * - Dati di fondo: FATTURA
069620050606w   2c                   when      AR8trk   = 'S4'
069630050606     c                   exsr      PrtDdtLOG1_S4
069640050606      * - Dati di fondo note: FATTURA
069650050606w   2c                   when      AR8trk   = 'S5'
069660050606     c                   exsr      PrtDdtLOG1_S5
069670041019e   2c                   endsl
069680041019      *
069690041019     c     KLDV          reade     FIAR8000
069700041019      *
069710041019e   1c                   enddo
069720050318      *
069730050318      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
069740050318      *
069750050505      * - se DDT con Packing-List:
069760050505      *   (altrimenti il box è già stato stampato in sr PrtDdtLOG1_P1)
069770050505if  1c                   if        %subst(SAVtrk:1:1) =  'Z'
069780050318      *
069790050318      * - - Calcolo la posizione finale del box e di linee separatrici
069800050318     c                   exsr      Posiz_DL
069810050318      *
069820050318      * > Stampa box, linee e testi fissi (no logo)  - - - - - - - - -*
069830081017     c                   if            §AR8cxd = *blanks
069840081017     c                             or  DB0fa4  = 'P'
069850050331     c                   write     A4prtDT0
069860050331     c                   endif
069870050318     c                   write     A4prtDL0C
069880050505      *
069890050505e   1c                   endif
069900050218      *
069910050218      * Reimpostazione del vecchio limite x overflow
069920080925      /free
069930080925         select;
069940080925           when  NOT *in45;
069950090323             wMaxLine = $MaxLine2;
069960080925           when  *in45   and   NOT *in46;
069970090323             wMaxLine = $MaxLine2_pl1;
069980080925           when  *in45   and   *in46;
069990090323             wMaxLine = $MaxLine2_pl2;
070000080925         endsl;
070010080925      /end-free
070020041019      *
070030041019     c                   ENDSR
070040081222
070050081222      /free
070060081222       //--------------------------------------------------------------
070070140303       //?Preparazione simil-logo Bartolini                            ?
070080081222       //--------------------------------------------------------------
070090090406       BEGSR  PrtDdtLOG1_SimilLogo;
070100081222
070110120530         //If  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
070120120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
070130120530         //
070140120530         //  // -?Preparazione simil-Logo BARTOLINI?
070150120530         //  §PLBLS  = §PBA + 0.150;
070160120530         //  §PLBLVS = 0.400;
070170120530         //  exsr  sr_SimilLogoBAR;
070180120530         //
070190120530         //  // -?Preparazione simil-Logo BRT?
070200120530         //  §P2blS  = §PlblS;
070210120530         //  //§P2blVS = §PlblVD + 0.050;
070220120530         //  §P2blVS = §PlblVD + 0.030;
070230120530         //  exsr  sr_SimilLogoBRT;
070240120530         //
070250120530         //Else;
070260120530         //
070270120530         //  // -?Preparazione simil-Logo BARTOLINI?
070280120530         //  §PLBLS  = §PBA + 0.050;
070290120530         //  §PLBLVS = 0.400;
070300120530         //  exsr  sr_SimilLogoBART;
070310120530         //
070320120530         //EndIf;
070330120530
070340120530         // -?Preparazione simil-Logo BRT?
070350120530         §PLBLS  = §PBA + 0.050;
070360120530         §PLBLVS = 0.400;
070370120530         exsr  sr_SimilLogoBRT_;
070380090406
070390120530         // -?Impostazione dati anagrafici BRT alla sua destra?
070400090422         §PC0    = §PBA    + 0.070 + 0.050;
070410090406         §PC1    = §PC0    + 0.100;
070420090406         §PC2    = §PC1    + 0.100;
070430090406         §PC3    = §PC2    + 0.090;
070440090406         §PC4    = §PC3    + 0.090;
070450090406         §PC5    = §PC4    + 0.090;
070460120530
070470120530         //§PLC    = 1.400;
070480120530         §PLC    = 1.000;
070490090406
070500120530         // -?"TBAR" è già correttamente impostato.?
070510120530         //TBAR    = $Bartolini;        (già così)
070520120530         //TBAR    = $BRT;              (già così)
070530090406         LBA1    = $Intest1;
070540090406         LBA2    = $Intest2_b;
070550120530         //LBA3    = $Intest3_b;
070560120530         LBA3    = $Intest3_b_brt;
070570090406         LBA4    = $Intest4_b;
070580090406         LBA5    = $Intest5_b;
070590081222
070600081222       ENDSR;
070610081222      /end-free
070620050214      *
070630041019      *---------------------------------------------------------------*
070640050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
070650050217      *?Dati di testata: sito e depositante                          ?*
070660041019      *---------------------------------------------------------------*
070670050217     c     PrtDdtLOG1_T1 BEGSR
070680041019      *
070690041019      * > logo
070700050218     c                   eval      §Logo    = 'LOGO' + AR8tmd
070710090107      /free
070720090107         // - Impostazione indicatore che condiziona la stampa del logo
070730120530         //   (o quella del "simil-logo" BRT)
070740090107         *in55 = (%subst(AR8tmd : 1 : 3) = 'LOG'          and
070750090107                  (§AR8tsd  = 'L1'  or  §AR8tsd  = 'L3'));
070760090107      /end-free
070770080908      *
070780060609      *? L O G O !! ?
070790060609      * per pulizia non vogliamo più avere tanti LOGx per ogni cliente che
070800060609      * vuole la stampa della fattura con il proprio logo e la bolla con
070810120530      * il logo BRT
070820060609      * anche Cappellini è d'accordo ad inviare i dati con il logo del
070830060609      * cliente da stampare nella LDV
070840060609      * quelli già esistenti avranno un nuovo logo un po' più piccolo per
070850071008      * problemi di spazio e i dati saranno inviati con AR8tmd = al nome del
070860071008      * nuovo logo e la tabella sarà fatta con §AR8tsd = 'L2' o 'L4'
070870060609      * i nuovi faremo in modo di avere o lo stesso logo oppure la stessa
070880060609      * logica descritta prima
070890060609      * ma può essere che un cliente voglia la fattura con il suo logo
070900120530      * mentre la bolla con il logo BRT quindi devo recuperare
070910060609      * il logo da stampare nella bolla non più da AR8TMD, che conterrà
070920060609      * il nome del logo da stampare nella fattura, ma dal campo del
070930120530      * del logo fattura, che conterrà il logo BRT, mentre il campo
070940071008      * §AR8tsd nella tabella sarà 'L1' o 'L3'
070950071008     c                   if        (§AR8tsd = 'L1' or §AR8tsd = 'L3') and
070960071008     c                             §AR8logof <> *blanks and
070970071008     c                             %subst(AR8tmd:1:3) <> 'LOG'
070980071008     c                   eval      §Logo    = 'LOGO' + §AR8logof
070990090107      /free
071000090107           // - Impostaz. indicatore che condiziona la stampa del logo
071010120530           //   (o quella del "simil-logo" BRT)
071020090107           *in55 = (%subst(§AR8logof : 1 : 3) = 'LOG');
071030090107      /end-free
071040060609     c                   endif
071050041019      *
071060080822     c                   movel(p)  AR8uni        dAR8LOG1T1
071070050414      *
071080060627if  1c                   if            §AR8tsd <> 'LC'
071090041019      *
071100041019      * > sito
071110060407     c                   select
071120071008     c                   when      §AR8tsd = 'L1' or
071130071008     c                             §AR8tsd = 'L3'
071140050707     c                   eval      PLOG1st0 = 'Deposito di:'
071150071008     c                   when      §AR8tsd = 'L2' or
071160071008     c                             §AR8tsd = 'L4'
071170060407     c                   eval      PLOG1st0 = 'Depositario:'
071180060407     c                   endsl
071190050707     c                   eval      PLOG1st1 = §A8LOG1t1a
071200050707     c                   eval      PLOG1st2 = §A8LOG1t1b
071210050707     c                   eval      PLOG1st3 = §A8LOG1t1c
071220050707     c                   eval      PLOG1st4 = §A8LOG1t1d
071230050707     c                   eval      PLOG1st5 = §A8LOG1t1e
071240041019      *
071250041019      * > depositante
071260060407     c                   select
071270071008     c                   when      §AR8tsd = 'L1' or
071280071008     c                             §AR8tsd = 'L3'
071290050707     c                   eval      PLOG1de0 = 'Depositario della:'
071300071008     c                   when      §AR8tsd = 'L2' or
071310071008     c                             §AR8tsd = 'L4'
071320060407     c                   eval      PLOG1de0 = 'Deposito di:'
071330060407     c                   endsl
071340050707     c                   eval      PLOG1de1 = §A8LOG1t1f
071350050707     c                   eval      PLOG1de2 = §A8LOG1t1g
071360050707     c                   eval      PLOG1de3 = §A8LOG1t1h
071370050707     c                   eval      PLOG1de4 = §A8LOG1t1i
071380050707     c                   eval      PLOG1de5 = §A8LOG1t1j
071390050414      *
071400050414x   1c                   else
071410050414      *
071420050707     c***                eval      PLOG1st0 = *blanks
071430050707     c                   eval      PLOG1st1 = §A8LOG1t1a + §A8LOG1t1b
071440050707     c                   eval      PLOG1st2 = §A8LOG1t1c + §A8LOG1t1d
071450050707     c                   eval      PLOG1st3 = §A8LOG1t1e + §A8LOG1t1f
071460050707     c                   eval      PLOG1st4 = §A8LOG1t1g + §A8LOG1t1h
071470050707     c                   eval      PLOG1st5 = §A8LOG1t1i + §A8LOG1t1j
071480050414      *
071490050414e   1c                   endif
071500050214      *
071510041022     c                   ENDSR
071520050214      *
071530041019      *---------------------------------------------------------------*
071540050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
071550050217      *?Dati di testata: destinatario, cliente di fatturazione       ?*
071560041019      *---------------------------------------------------------------*
071570050217     c     PrtDdtLOG1_T2 BEGSR
071580041019      *
071590051110     c                   clear                   dAR8LOG1T2
071600050218     c                   movel     AR8uni        dAR8LOG1T2
071610041019      *
071620041019      * > destinatario
071630050707if  1c                   if        §A8LOG1T2a <> *blanks
071640050707     c                   eval      PLOG1ccd   =  §A8LOG1T2a
071650050218e   1c                   endif
071660050707if  1c                   if        §A8LOG1T2c =  *blanks
071670050707     c                   clear                   PLOG1rsd
071680050707     c                   eval      PLOG1rsd2  =  §A8LOG1T2b
071690050218x   1c                   else
071700050707     c                   eval      PLOG1rsd   =  §A8LOG1T2b
071710050707     c                   eval      PLOG1rsd2  =  §A8LOG1T2c
071720050218e   1c                   endif
071730050707     c                   eval      PLOG1ind   =  §A8LOG1T2d
071740050707     c                   eval      PLOG1ind2  =  §A8LOG1T2e
071750050707if  1c                   if        §A8LOG1T2f >  *zeros
071760050707     c                   eval      PLOG1cad   =  §A8LOG1T2f
071770041019e   1c                   endif
071780050707     c                   eval      PLOG1lod   =  §A8LOG1T2g
071790041019      * > cliente di fatturazione
071800050707if  1c                   if        §A8LOG1T2h <> *blanks
071810050707     c                   eval      PLOG1ccf   =  §A8LOG1T2h
071820041019e   1c                   endif
071830050707if  1c                   if        §A8LOG1T2j =  *blanks
071840050707     c                   clear                   PLOG1rsf
071850050707     c                   eval      PLOG1rsf2  =  §A8LOG1T2i
071860050218x   1c                   else
071870050707     c                   eval      PLOG1rsf   =  §A8LOG1T2i
071880050707     c                   eval      PLOG1rsf2  =  §A8LOG1T2j
071890050218e   1c                   endif
071900050707     c                   eval      PLOG1inf   =  §A8LOG1T2k
071910050707     c                   eval      PLOG1inf2  =  §A8LOG1T2l
071920050707if  1c                   if        §A8LOG1T2m >  *zeros
071930050707     c                   eval      PLOG1caf   =  §A8LOG1T2m
071940041019e   1c                   endif
071950050707     c                   eval      PLOG1lof   =  §A8LOG1T2n
071960041019      *
071970041019     c                   ENDSR
071980041019      *
071990041019      *---------------------------------------------------------------*
072000050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
072010050217      *?Dati di testata: documento, rif.ordine, note                 ?*
072020041019      *---------------------------------------------------------------*
072030050217     c     PrtDdtLOG1_T3 BEGSR
072040041019      *
072050051110     c                   clear                   dAR8LOG1T3
072060050218     c                   movel     AR8uni        dAR8LOG1T3
072070050706      *
072080050706      * TEST effettiva numericità dei campi previsti numerici:
072090100513     c                   clear                   w053a
072100111013     ***c                   eval      WX    =  %len(§A8LOG1T3b)
072110111013     ***c                                   +  %len(§A8LOG1T3c)
072120111013     ***c                                   +  %len(§A8LOG1T3d)
072130111013     ***c                   eval      w053a =  %subst(dAR8LOG1T3: 4:WX)
072140111013     ***c                   eval      *in31 =  *off
072150111013     ***c     c_Digits      checkr    w053a :  WX                            31
072160111013if  1***c                   if        *in31
072170111013     ***c                   clear                   §A8LOG1T3b
072180111013     ***c                   clear                   §A8LOG1T3c
072190111013     ***c                   clear                   §A8LOG1T3d
072200111013e   1***c                   endif
072210121116     ***c                   eval      WX    =  %len(§A8LOG1T3b)
072220121116     ***c                   eval      w053a =  %subst(dAR8LOG1T3: 4:WX)
072230121116     ***c                   eval      *in31 =  *off
072240121116     ***c     c_Digits      checkr    w053a :  WX                            31
072250121116if  1***c                   if        *in31
072260121116     ***c                   clear                   §A8LOG1T3b
072270121116e   1***c                   endif
072280111013     c                   clear                   w053a
072290111013     c                   eval      WX    =  %len(§A8LOG1T3d)
072300111013     c                   eval      w053a =  %subst(dAR8LOG1T3:15:WX)
072310111013     c                   eval      *in31 =  *off
072320111013     c     c_Digits      checkr    w053a :  WX                            31
072330111013if  1c                   if        *in31
072340111013     c                   clear                   §A8LOG1T3d
072350111013e   1c                   endif
072360041019      *
072370041019      * > dati in testata DDT
072380041019      * - - numero e data documento (DDT)
072390050707     c                   eval      PLOG1nsr = §A8LOG1T3a
072400121116     ***c                   eval      PLOG1aas = §A8LOG1T3b
072410121116     c                   if        §A8Log1T3b <> *blanks   and
072420121116     c                             §A8Log1T3b <> *zeros
072430121116     c                   eval      PLOG1aas = §A8LOG1T3b
072440121116     c                   endif
072450111013     ***c                   if        §A8Log1T3c <> *zeros
072460111013     ***c                   eval      PLOG1nsp = %editc(§A8LOG1T3c:'X')
072470111013     ***c                   endif
072480111013     c                   if        §A8Log1T3c <> *blanks   and
072490111013     c                             §A8Log1T3c <> *zeros
072500111013     c                   eval      PLOG1nsp = §A8LOG1T3c
072510111013     c                   endif
072520100319     c                   if        §A8Log1T3a <> *blanks   and
072530121116     c                             §A8Log1T3b <> *blanks   and
072540100319     c                             §A8Log1T3b <> *zeros
072550100319     c                   eval      PLog1sepA = '/'
072560100319     c                   endif
072570121115     c                   if        §A8Log1T3b <> *zeros    and
072580121115     c                             §A8Log1T3c <> *blanks   and
072590100319     c                             §A8Log1T3c <> *zeros
072600100319     c                   eval      PLog1sepB = '/'
072610100319     c                   endif
072620110707      *     giro la data del documento
072630050707if  1c                   if        §A8LOG1T3d > *zeros
072640060922     c                   eval      *in31 =  *off
072650060922     c     *iso          test(d)                 §A8LOG1T3d             31
072660060922if  2c                   if        NOT *in31
072670050707     c     *iso          movel     §A8LOG1T3d    Data_EUR
072680050707     c     *eur          movel     Data_EUR      PLOG1dsp
072690060922x   2c                   else
072700060922     c                   clear                   PLOG1dsp
072710060922e   2c                   endif
072720050707e   1c                   endif
072730041019      * - - riferimento ordine
072740050707     c                   eval      PLOG1ref = §A8LOG1T3e
072750041019      * - - note consegna
072760050707     c                   eval      PLOG1not = §A8LOG1T3f
072770050707     c                   eval      PLOG1nt2 = §A8LOG1T3g
072780060612      * - - intestazione corpo bolla per Logistica tipo stampa L3 o L4
072790060922     c                   if        §AR8tsd = 'L3' or §AR8tsd = 'L4'
072800060612     c                   eval      *in23 = *On
072810060612     c                   eval      PLOG1int = §A8LOG1T3h
072820060612     c                   else
072830060612     c                   eval      *in23 = *off
072840060612     c                   endif
072850041019      *
072860041019      * > Stampa testata DDT - - - - - - - - - - - - - - - - - - - - -*
072870041019      *
072880041019      *   - OverFlow impossibile qui! -
072890050218     c***                exsr      Test_OF
072900050318     c                   exsr      Posiz_DL
072910081017     c                   if            §AR8cxd = *blanks
072920081017     c                             or  DB0fa4  = 'P'
072930050331     c                   write     A4prtDT0
072940050331     c                   endif
072950050218     c                   write     A4prtDL0
072960081222      /free
072970081222         if  *in55;
072980090406           exsr   PrtDdtLOG1_SimilLogo;
072990120530           //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
073000120530           //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
073010120530           //  write  A4LogoBR2;
073020120530           //else;
073030120530           //  write  A4LogoBAR;
073040120530           //endif;
073050120530           write  A4LogoBRT;
073060081222           write  A4prtDLL;
073070081222         endif;
073080081222      /end-free
073090050218     c*** quante righe?  write     A4prtDL0A
073100041028     c                   write     A4prtDL1A
073110050218     c                   write     A4prtDL1B
073120041019      *
073130041019     c                   ENDSR
073140050214      *
073150041019      *---------------------------------------------------------------*
073160050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
073170050217      *?Dati di dettaglio: articolo, quantità, descrizione           ?*
073180041019      *---------------------------------------------------------------*
073190050217     c     PrtDdtLOG1_C1 BEGSR
073200041019      *
073210041019     c                   clear                   A4prtDL2
073220051110     c                   clear                   dAR8LOG1C1
073230050218     c                   movel     AR8uni        dAR8LOG1C1
073240050706      *
073250050706      * TEST effettiva numericità dei campi previsti numerici:
073260100513     c                   clear                   w053a
073270050706     c                   eval      WX    =  %len(§A8LOG1C1b)
073280100513     c                   eval      w053a =  %subst(dAR8LOG1C1:21:WX)
073290050706     c                   eval      *in31 =  *off
073300110426     c     c_Digits      checkr    w053a :  WX                            31
073310050706if  1c                   if        *in31
073320050706     c                   clear                   §A8LOG1C1b
073330050706e   1c                   endif
073340041019      *
073350041019      * > dati in dettaglio DDT
073360041019      * - - codice prodotto
073370050707     c                   eval      PLOG1cpr  = §A8LOG1C1a
073380041019      * - - quantità
073390050707     c                   eval      PLOG1qta  = §A8LOG1C1b
073400041019      * - - descrizione prodotto
073410050707     c                   eval      PLOG1dpr  = §A8LOG1C1c
073420041019      *
073430041019      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
073440041019      *
073450041019     c                   exsr      Test_OF
073460041019     c                   write     A4prtDL2
073470041019      *
073480041019     c                   ENDSR
073490041019      *
073500041019      *---------------------------------------------------------------*
073510050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
073520050217      *?Dati di dettaglio: descrizione                               ?*
073530041019      *---------------------------------------------------------------*
073540050217     c     PrtDdtLOG1_C2 BEGSR
073550041019      *
073560041019     c                   clear                   A4prtDL2
073570051110     c                   clear                   dAR8LOG1C2
073580050218     c                   movel     AR8uni        dAR8LOG1C2
073590041019      *
073600041019      * > dati in note DDT
073610041019      * - - riga descrittiva
073620050707     c                   eval      PLOG1dpr  = §A8LOG1C2a
073630041019      *
073640041019      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
073650041019      *
073660041019     c                   exsr      Test_OF
073670041019     c                   write     A4prtDL2
073680041019      *
073690041019     c                   ENDSR
073700041019      *
073710041019      *---------------------------------------------------------------*
073720050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
073730050217      *?Dati di dettaglio: descrizioni                               ?*
073740041019      *---------------------------------------------------------------*
073750050217     c     PrtDdtLOG1_C3 BEGSR
073760041019      *
073770041019     c                   clear                   A4prtDL3
073780051110     c                   clear                   dAR8LOG1C3
073790050707     c                   movel     AR8uni        dAR8LOG1C3
073800041019      *
073810041019      * > dati in note DDT
073820041019      * - - riga descrittiva
073830050707     c                   eval      PLOG1txt  = §A8LOG1C3a
073840041019      *
073850041019      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
073860041019      *
073870041019     c                   exsr      Test_OF
073880041019     c                   write     A4prtDL3
073890041019      *
073900041019     c                   ENDSR
073910060614      *
073920060614      *---------------------------------------------------------------*
073930060614      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
073940060614      *?Dati di dettaglio: corpo bolla non fincato                   ?*
073950060614      *---------------------------------------------------------------*
073960060614     c     PrtDdtLOG1_C4 BEGSR
073970060614      *
073980060614     c                   clear                   A4prtDL21
073990060614     c                   clear                   dAR8LOG1C4
074000060614     c                   movel     AR8uni        dAR8LOG1C4
074010060614      *
074020060614      * > dati DDT
074030060614      * - - riga descrittiva
074040060614     c                   eval      PLOG1dpr1 = §A8LOG1C4a
074050060614      *
074060060614      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
074070060614      *
074080060614     c                   exsr      Test_OF
074090060614     c                   write     A4prtDL21
074100060614      *
074110060614     c                   ENDSR
074120090323      *
074130090323      *---------------------------------------------------------------*
074140090323      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
074150090323      *?Dati di dettaglio: descrizioni                               ?*
074160090323      *---------------------------------------------------------------*
074170090323     c     PrtDdtLOG1_C5 BEGSR
074180090323      *
074190090323     c                   clear                   A4prtDL31
074200090323     c                   clear                   dAR8LOG1S3
074210090323     c                   movel     AR8uni        dAR8LOG1S3
074220090323      *
074230090323      * > dati in note DDT
074240090323      * - - riga descrittiva
074250090323     c                   eval      PLOG1txt2 = §A8LOG1S3a
074260090323      *
074270090323      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
074280090323      *
074290090323     c                   exsr      Test_OF
074300090323     c                   write     A4prtDL31
074310090323      *
074320090323     c                   ENDSR
074330041019      *
074340041019      *---------------------------------------------------------------*
074350050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
074360050217      *?Dati di fondo: beni, colli, peso, volume, c/a, porto, trasp. ?*
074370041019      *---------------------------------------------------------------*
074380050217     c     PrtDdtLOG1_P1 BEGSR
074390041019      *
074400051110     c                   clear                   dAR8LOG1P1
074410050707     c                   movel     AR8uni        dAR8LOG1P1
074420050706      *
074430050706      * TEST effettiva numericità dei campi previsti numerici:
074440100513     c                   clear                   w053a
074450050706     c                   eval      WX    =  %len(§A8LOG1P1b)
074460050706     c                                   +  %len(§A8LOG1P1c)
074470050706     c                                   +  %len(§A8LOG1P1d)
074480100513     c                   eval      w053a =  %subst(dAR8LOG1P1:42:WX)
074490050706     c                   eval      *in31 =  *off
074500110426     c     c_Digits      checkr    w053a :  WX                            31
074510050706if  1c                   if        *in31
074520050706     c                   clear                   §A8LOG1P1b
074530050706     c                   clear                   §A8LOG1P1c
074540050706     c                   clear                   §A8LOG1P1d
074550050706e   1c                   endif
074560100513     c                   clear                   w053a
074570050706     c                   eval      WX    =  %len(§A8LOG1P1f)
074580100513     c                   eval      w053a =  %subst(dAR8LOG1P1:67:WX)
074590050706     c                   eval      *in31 =  *off
074600110426     c     c_Digits      checkr    w053a :  WX                            31
074610050706if  1c                   if        *in31
074620050706     c                   clear                   §A8LOG1P1f
074630050706e   1c                   endif
074640041019      *
074650041019      * > dati in chiusura DDT
074660041019      * - - aspetto esteriore dei beni
074670050707     c                   eval      PLOG1aeb  = §A8LOG1P1a
074680041019      * - - numero colli
074690050707     c                   eval      PLOG1ncl  = §A8LOG1P1b
074700041019      * - - peso in kg
074710050707     c                   eval      PLOG1pkg  = §A8LOG1P1c
074720041019      * - - volume in mc
074730050707     c                   eval      PLOG1vlm  = §A8LOG1P1d
074740041019      * - - c/assegno
074750050707     c                   eval      PLOG1cva  = §A8LOG1P1e
074760050707     c                   eval      PLOG1cas  = §A8LOG1P1f
074770041020      * - - causale trasporto
074780050707     c                   eval      PLOG1ctr  = §A8LOG1P1g
074790041020      * - - porto
074800050707     c                   eval      PLOG1pto  = §A8LOG1P1h
074810041020      * - - trasporto a cura
074820050707     c                   eval      PLOG1tra  = §A8LOG1P1i
074830041020      * - - trasportatore
074840050707     c                   eval      PLOG1vet  = §A8LOG1P1j
074850050505      *
074860050505      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
074870050505      *
074880050505      * - - Verifica OverFlow
074890080925      /free
074900080925           select;
074910080925             when  NOT *in45;
074920080925               wMaxLine = $MaxLine4;
074930080925             when  *in45   and   NOT *in46;
074940080925               wMaxLine = $MaxLine4_pl1;
074950080925             when  *in45   and   *in46;
074960080925               wMaxLine = $MaxLine4_pl2;
074970080925           endsl;
074980080925      /end-free
074990050505     c                   exsr      Test_OF
075000050505      * - - Calcolo della posizione finale di box e linee separatrici
075010050505     c                   exsr      Posiz_DL
075020050505      * - - Stampa parte finale DDT
075030050505     c                   write     A4prtDL4
075040050505      *
075050050505      * > Stampa box, linee e testi fissi (no logo)  - - - - - - - - -*
075060090331     c                   if            §AR8cxd = *blanks
075070090331     c                             or  DB0fa4  = 'P'
075080090331     c                   write     A4prtDT0
075090090331     c                   endif
075100050505     c                   write     A4prtDL0A
075110050505     c                   write     A4prtDL0B
075120050505      *
075130050505      * Azzero il numero di pagina per l'eventuale Packing-List
075140050505      *   (successiva)
075150050505     c                   clear                   Page
075160041019      *
075170041019      * N.B.: la copia per il destinatario della LDV deve comparire
075180041019      *       nell'ultima pagina del DDT per il cliente (vedi subr.
075190041019      *       "ChiusuraLDV")
075200041019      *
075210041019     c                   ENDSR
075220050318      *
075230050318      *---------------------------------------------------------------*
075240050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
075250050318      *?Dati di packing-list: dettaglio                              ?*
075260050318      *---------------------------------------------------------------*
075270050318     c     PrtDdtLOG1_Z1 BEGSR
075280050318      *
075290050318      * Prima della 1ª riga di packing-list: testata packing-list
075300050505if  1c                   if        Page     = *zeros
075310050318     c                   exsr      PrtPckLst_1
075320050318e   1c                   endif
075330050318      *
075340050318     c                   clear                   A4prtDL5
075350051110     c                   clear                   dAR8LOG1Z1
075360050707     c                   movel     AR8uni        dAR8LOG1Z1
075370050706      *
075380050706      * TEST effettiva numericità dei campi previsti numerici:
075390100513     c                   clear                   w053a
075400050706     c                   eval      WX    =  %len(§A8LOG1Z1d)
075410100513     c                   eval      w053a =  %subst(dAR8LOG1Z1:53:WX)
075420050706     c                   eval      *in31 =  *off
075430110426     c     c_Digits      checkr    w053a :  WX                            31
075440050706if  1c                   if        *in31
075450050706     c                   clear                   §A8LOG1Z1d
075460050706e   1c                   endif
075470050318      *
075480050318      * > dati in dettaglio Packing-List
075490050318      * - - sscc
075500050318     c                   eval      PCsscc   = §A8LOG1Z1a
075510050318      * - - gtin
075520050318     c                   eval      PCgtin   = §A8LOG1Z1b
075530050318      * - - codice prodotto
075540050318     c                   eval      PCcpr2   = §A8LOG1Z1c
075550050318      * - - quantità ul
075560050318     c                   eval      PLOG1QTA = §A8LOG1Z1d
075570050318      * - - tipo contenitore
075580050318     c                   eval      PCtcn    = §A8LOG1Z1e
075590050318      * - - codice contenitore
075600050318     c                   eval      PCccn    = §A8LOG1Z1f
075610050318      * - - numero lotto
075620050318     c                   eval      PClot    = §A8LOG1Z1g
075630050318      * - - data scadenza
075640050318     c                   eval      PCdts    = §A8LOG1Z1h
075650050318      *
075660050318      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
075670050318      *
075680050318     c                   exsr      Test_OF
075690050318     c                   write     A4prtDL5
075700050318      *
075710050318     c                   ENDSR
075720080908      *
075730050606      *---------------------------------------------------------------*
075740050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
075750080925      *?Dati di Fattura: Testata                                     ?*
075760050606      *---------------------------------------------------------------*
075770050606     c     PrtDdtLOG1_S1 BEGSR
075780050606      *
075790050606      * > logo Fattura
075800050606     c                   eval      §LogoF   = 'LOGO' + §AR8logof
075810090107      /free
075820090107         // - Impostazione indicatore che condiziona la stampa del logo
075830120530         //   (o quella del "simil-logo" BRT)
075840090107         *in55 = (%subst(§AR8logof : 1 : 3) = 'LOG'       and
075850090107                  (§AR8tsd  = 'L1'  or  §AR8tsd  = 'L3'));
075860090107      /end-free
075870080908      *
075880060609      *? L O G O !! ?
075890060609      *?vedi stampa testata bolla ?
075900071008     c                   if        (§AR8tsd = 'L1' or §AR8tsd = 'L3') and
075910071008     c                             §AR8logof <> *blanks and
075920071008     c                             %subst(AR8tmd:1:3) <> 'LOG'
075930071008     c                   eval      §LogoF   = 'LOGO' + AR8tmd
075940090107      /free
075950090107           // - Impostaz. indicatore che condiziona la stampa del logo
075960120530           //   (o quella del "simil-logo" BRT)
075970090107           *in55 = (%subst(AR8tmd : 1 : 3) = 'LOG');
075980090107      /end-free
075990060609     c                   endif
076000050606      *
076010051110     c                   clear                   dAR8LOG1S1
076020050606     c                   movel     AR8uni        dAR8LOG1S1
076030050706      *
076040050706      * TEST effettiva numericità dei campi previsti numerici:
076050100513     c                   clear                   w053a
076060050706     c                   eval      WX    =  %len(§A8LOG1S1i)
076070100513     c                   eval      w053a =  %subst(dAR8LOG1S1:216:WX)
076080050706     c                   eval      *in31 =  *off
076090110426     c     c_Digits      checkr    w053a :  WX                            31
076100050706if  1c                   if        *in31
076110050706     c                   clear                   §A8LOG1S1i
076120050706e   1c                   endif
076130050707      *
076140050606      * > intestazione fattura
076150050707if  1c                   if        §A8LOG1S1b =  *blanks
076160050707     c                   clear                   PLOG1rsf
076170050707     c                   eval      PLOG1rsf2  =  §A8LOG1S1a
076180050606x   1c                   else
076190050707     c                   eval      PLOG1rsf   =  §A8LOG1S1a
076200050707     c                   eval      PLOG1rsf2  =  §A8LOG1S1b
076210050606e   1c                   endif
076220050707     c                   eval      PLOG1inf   =  §A8LOG1S1c
076230050707     c                   eval      PLOG1inf2  =  §A8LOG1S1d
076240050707if  1c                   if        §A8LOG1S1e >  *zeros
076250050707     c                   eval      PLOG1caf   =  §A8LOG1S1e
076260050606e   1c                   endif
076270050707     c                   eval      PLOG1lof   =  §A8LOG1S1f
076280050606      * > partita IVA - codice fiscale
076290050707     c                   eval      PLOG1IVA   =  §A8LOG1S1g
076300050606      * > n./data fattura
076310050707     c                   eval      PLOG1nft   =  §A8LOG1S1h
076320050606      *     giro la data fattura
076330050706if  1c                   if        §A8LOG1S1i >  *zeros
076340060922     c                   eval      *in31 =  *off
076350060922     c     *iso          test(d)                 §A8LOG1S1i             31
076360060922if  2c                   if        NOT *in31
076370050707     c     *iso          movel     §A8LOG1S1i    Data_EUR
076380050707     c     *eur          movel     Data_EUR      PLOG1dft
076390060922x   2c                   else
076400060922     c                   clear                   PLOG1dft
076410060922e   2c                   endif
076420050706e   1c                   endif
076430050606      * > rif.ns.spedizione
076440060922     c                   eval      PLOG1nspa =   §A8LOG1S1j
076450050606      * > intestazione corpo fattura
076460050707     c                   eval      PLOG1tesf  =  §A8LOG1S1k
076470050707      *
076480050606      * > Stampa testata FATTURA - - - - - - - - - - - - - - - - - - -*
076490081015      *
076500080925      /free
076510081015         // - Forzatura del salto pagina
076520081015         CurLineA4 = wMaxLine + 1;
076530080925      /end-free
076540080925     c                   exsr      Test_OF
076550081015      /free
076560090323         // - Impostazione dei nuovi limiti di pagina
076570090323         select;
076580090323           when  NOT *in45;
076590090323             wMaxLine = $MaxLine2;
076600090323           when  *in45   and   NOT *in46;
076610090323             wMaxLine = $MaxLine2_pl1;
076620090323           when  *in45   and   *in46;
076630090323             wMaxLine = $MaxLine2_pl2;
076640090323         endsl;
076650081015      /end-free
076660080925     c                   exsr      Posiz_DL
076670081017     c                   if            §AR8cxd = *blanks
076680081017     c                             or  DB0fa4  = 'P'
076690080925     c                   write     A4prtDT0
076700080925     c                   endif
076710080925     c                   write     A4prtDL0D
076720050606     c                   write     A4prtDL1D
076730050606     c                   write     A4prtDL1E
076740050707      *
076750050606     c                   ENDSR
076760050606      *
076770050606      *---------------------------------------------------------------*
076780050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
076790050606      *?Dati di dettaglio corpo: FATTURA                             ?*
076800050606      *---------------------------------------------------------------*
076810050606     c     PrtDdtLOG1_S2 BEGSR
076820050606      *
076830050606     c                   clear                   A4prtDL6
076840051110     c                   clear                   dAR8LOG1S2
076850050606     c                   movel     AR8uni        dAR8LOG1S2
076860050606      *
076870050606      * > dati fattura
076880050606      * - - riga descrittiva 1
076890050707     c                   eval      PLOG1dft1 = §A8LOG1S2a
076900050606      *
076910050606      * > Stampa dettaglio FATTURA - - - - - - - - - - - - - - - - - -*
076920050606      *
076930050606     c                   exsr      Test_OF
076940050606     c                   write     A4prtDL6
076950050606      *
076960050606     c                   ENDSR
076970050606      *
076980050606      *---------------------------------------------------------------*
076990050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
077000050606      *?Dati di dettaglio corpo: FATTURA                             ?*
077010050606      *---------------------------------------------------------------*
077020050606     c     PrtDdtLOG1_S3 BEGSR
077030050606      *
077040050606     c                   clear                   A4prtDL7
077050051110     c                   clear                   dAR8LOG1S3
077060050707     c                   movel     AR8uni        dAR8LOG1S3
077070050606      *
077080050606      * > dati fattura
077090050606      * - - riga descrittiva 2
077100050707     c                   eval      PLOG1dft2 = §A8LOG1S3a
077110050606      *
077120050606      * > Stampa dettaglio FATTURA - - - - - - - - - - - - - - - - - -*
077130050606      *
077140050606     c                   exsr      Test_OF
077150050606     c                   write     A4prtDL7
077160050606      *
077170050606     c                   ENDSR
077180050606      *
077190050606      *---------------------------------------------------------------*
077200050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
077210050606      *?Dati di fondo: FATTURA                                       ?*
077220050606      *---------------------------------------------------------------*
077230050606     c     PrtDdtLOG1_S4 BEGSR
077240050606      *
077250051110     c                   clear                   dAR8LOG1S4
077260050707     c                   movel     AR8uni        dAR8LOG1S4
077270050706      *
077280050706      * TEST effettiva numericità dei campi previsti numerici:
077290100513     c                   clear                   w053a
077300050706     c                   eval      WX    =  %len(§A8LOG1S4a)
077310050706     c                                   +  %len(§A8LOG1S4b)
077320050706     c                                   +  %len(§A8LOG1S4c)
077330100513     c                   eval      w053a =  %subst(dAR8LOG1S4: 1:WX)
077340050706     c                   eval      *in31 =  *off
077350110426     c     c_Digits      checkr    w053a :  WX                            31
077360050706if  1c                   if        *in31
077370050706     c                   clear                   §A8LOG1S4a
077380050706     c                   clear                   §A8LOG1S4b
077390050706     c                   clear                   §A8LOG1S4c
077400050706e   1c                   endif
077410050606      *
077420050606      * > dati in chiusura FATTURA
077430050606      * - - dati di piede fattura riga 1
077440050707     c                   eval      PLOG1pft1 = §A8LOG1S4d
077450050606      * - - dati di piede fattura riga 2
077460050707     c                   eval      PLOG1pft2 = §A8LOG1S4e
077470050606      * - - dati di piede fattura riga 3
077480050707     c                   eval      PLOG1pft3 = §A8LOG1S4f
077490050606      * - - totale imponibile
077500050707     c                   eval      PLOG1timp = §A8LOG1S4a
077510050606      * - - totale iva
077520050707     c                   eval      PLOG1tiva = §A8LOG1S4b
077530050606      * - - totale fattura
077540050707     c                   eval      PLOG1tft  = §A8LOG1S4c
077550050606      *
077560050606      * > Stampa fondo FATTURA - - - - - - - - - - - - - - - - - - - -*
077570050606      *
077580050606      * - - Verifica OverFlow
077590080925      /free
077600080925           select;
077610080925             when  NOT *in45;
077620080925               wMaxLine = $MaxLine4;
077630080925             when  *in45   and   NOT *in46;
077640080925               wMaxLine = $MaxLine4_pl1;
077650080925             when  *in45   and   *in46;
077660080925               wMaxLine = $MaxLine4_pl2;
077670080925           endsl;
077680080925      /end-free
077690050606     c                   exsr      Test_OF
077700050606      * - - Calcolo della posizione finale di box e linee separatrici
077710050606     c                   exsr      Posiz_DL
077720050606      * - - Stampa parte finale FATTURA
077730050606     c                   write     A4prtDL8
077740050606      *
077750050606      * > Stampa box, linee (no logo)   - - - - - - - - - - - - - - -*
077760050606     c                   write     A4prtDL0E
077770050606     c                   write     A4prtDL0F
077780080908      *
077790050606     c                   ENDSR
077800050606      *
077810050606      *---------------------------------------------------------------*
077820050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
077830050606      *?Dati di fondo note: FATTURA                                  ?*
077840050606      *---------------------------------------------------------------*
077850050606     c     PrtDdtLOG1_S5 BEGSR
077860050606      *
077870051110     c                   clear                   dAR8LOG1S5
077880050707     c                   movel     AR8uni        dAR8LOG1S5
077890050606      *
077900050606      * > dati in chiusura FATTURA
077910050606      * - - nota 1
077920050707     c                   eval      PLOG1not1 = §A8LOG1S5a
077930050606      * - - nota 2
077940050707     c                   eval      PLOG1not2 = §A8LOG1S5b
077950050606      * - - nota 3
077960050707     c                   eval      PLOG1not3 = §A8LOG1S5c
077970050606      *
077980050606      * > Stampa fondo FATTURA - - - - - - - - - - - - - - - - - - - -*
077990050606      * - - Stampa parte finale FATTURA
078000050606     c                   write     A4prtDL9
078010080908      *
078020050606     c                   ENDSR
078030050321      *
078040050321      *---------------------------------------------------------------*
078050050414      *?Impostazione nuova pagina di DDT Logistica / Sait            ?*
078060050321      *---------------------------------------------------------------*
078070050321     c     NewPageDL     BEGSR
078080050321      *
078090080925      /free
078100080925         // Impostazione nuovo limite di OF
078110080925         SELECT;
078120080925           // Piede DDT   oppure   Piede Fattura
078130080925           WHEN  AR8trk = 'P1'   or   AR8trk = 'S4';
078140080925             select;
078150080925               when  NOT *in45;
078160080925                 wMaxLine = $MaxLine4;
078170080925               when  *in45   and   NOT *in46;
078180080925                 wMaxLine = $MaxLine4_pl1;
078190080925               when  *in45   and   *in46;
078200080925                 wMaxLine = $MaxLine4_pl2;
078210080925             endsl;
078220080925           // Dettaglio DDT  oppure   Testata & Dettaglio Fattura
078230080925           OTHER;
078240080925             select;
078250080925               when  NOT *in45;
078260090323                 wMaxLine = $MaxLine2;
078270080925               when  *in45   and   NOT *in46;
078280090323                 wMaxLine = $MaxLine2_pl1;
078290080925               when  *in45   and   *in46;
078300090323                 wMaxLine = $MaxLine2_pl2;
078310080925             endsl;
078320080925         ENDSL;
078330080925      /end-free
078340080925      *
078350080825      * RE-impostazione posizioni
078360080825     c                   exsr      Posiz_DL
078370080825      *
078380050321     c                   write     A4prtSP1
078390081006      /free
078400081006         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
078410081006           write  A4prtNUL;
078420081006         endif;
078430081006      /end-free
078440050321      *
078450050505sel 1c                   select
078460050505      * - altra pagina DDT Logistica / Sait
078470050707w   1c                   when          %subst(AR8trk:1:1) <> 'Z'
078480050606     c                             and %subst(AR8trk:1:1) <> 'S'
078490050321     c                   write     A4prtDL1B
078500050505      * - altra pagina Packing-List
078510050505w   1c                   when      %subst(AR8trk:1:1) =  'Z'
078520050502     c                              and   Page        >  *zeros
078530050321     c                   write     A4prtDL1C
078540050606      * - altra pagina Fattura
078550050606w   1c                   when      %subst(AR8trk:1:1) =  'S'
078560050606     c                              and   Page        >  *zeros
078570050606     c                   write     A4prtDL1E
078580050505e   1c                   endsl
078590050321      *
078600050321     c                   ENDSR
078610050318      *
078620050318      *---------------------------------------------------------------*
078630080709      *?Stampa testata della Packing-List per LOGISTICA              ?*
078640050318      *---------------------------------------------------------------*
078650050318     c     PrtPckLst_1   BEGSR
078660050318      *
078670050318      * > Salto a pagina successiva  - - - - - - - - - - - - - - - - -*
078680050318     c                   exsr      Test_OF
078690050318      *
078700050505      * > Stampa box, linee e testi fissi (CON logo)  - - - - - - - - -*
078710050318      *
078720050318      * - - Stampa "Copia per il destinatario" nella pagina successiva
078730081017     c*** poi in Test_OF:   if            §AR8cxd = *blanks
078740081017     c*** poi in Test_OF:             or  DB0fa4  = 'P'
078750050505     c*** poi in Test_OF:   write     A4prtDT0
078760050505     c*** poi in Test_OF:   endif
078770050318      * - - Stampa dati destinatario nel box superiore
078780050318     c                   write     A4prtDL1A
078790050318      * - - Stampa logo e box superiore x destinatario
078800050318     c                   write     A4prtDL0
078810081222      /free
078820081222         if  *in55;
078830090406           exsr   PrtDdtLOG1_SimilLogo;
078840120530           //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
078850120530           //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
078860120530           //  write  A4LogoBR2;
078870120530           //else;
078880120530           //  write  A4LogoBAR;
078890120530           //endif;
078900120530           write  A4LogoBRT;
078910081222           write  A4prtDLL;
078920081222         endif;
078930081222      /end-free
078940050318      * - - Stampa dati di testata nel box generale
078950050318     c                   write     A4prtDL1C
078960050318      * - - Stampa box generale
078970050318     c*** poi in Test_OF:   write     A4prtDL0C
078980050318      *
078990050318      * Impostazione del nuovo limite x overflow in packing-list
079000080925      /free
079010080925         select;
079020080925           when  NOT *in45;
079030090323             wMaxLine = $MaxLine2;
079040080925           when  *in45   and   NOT *in46;
079050090323             wMaxLine = $MaxLine2_pl1;
079060080925           when  *in45   and   *in46;
079070090323             wMaxLine = $MaxLine2_pl2;
079080080925         endsl;
079090080925      /end-free
079100050318      *
079110050318     c                   ENDSR
079120041019      *
079130041020      *---------------------------------------------------------------*
079140050414      *?Posizionamento di linee e box nel DDT LOGISTICA / SAIT       ?*
079150041020      *---------------------------------------------------------------*
079160041021     c     Posiz_DL      BEGSR
079170050218      *
079180080924      /free
079190080924         // Impostazione posizioni di logo/box
079200080924         // (posizioni di "partenza" diverse a seconda del posizionam.
079210080924         //  iniziale nel foglio A4 attualmente in stampa)
079220080924         select;
079230080924           when  NOT *in45;
079240081006             §PBA =  3.255;
079250080924           when  *in45   and   NOT *in46;
079260090422             §PBA = 0.255;
079270080924           when  *in45   and   *in46;
079280090422             §PBA = 6.005;
079290080924         endsl;
079300080924      /end-free
079310080708     c                   eval      §PBB  =  §PBA + 1.27
079320080708     c                   eval      §LOA  =  §PBA + 0.63
079330080708     c                   eval      §PS0  =  §PBA + 0.72
079340080708     c                   eval      §PS1  =  §PS0 + 0.10
079350080708     c                   eval      §PS2  =  §PS1 + 0.10
079360080708     c                   eval      §PS3  =  §PS2 + 0.10
079370080708     c                   eval      §PS4  =  §PS3 + 0.10
079380080708     c                   eval      §PS5  =  §PS4 + 0.10
079390080708      * Impostazione posizioni di logo/box per fattura
079400080708     c                   eval      §PBC  =  §PBA + 0.92
079410080708     c                   eval      §LOB  =  §PBA + 0.72
079420080708      * Impostazione posizioni di note fine fattura
079430080925     c                   eval      §PS6  =  §PBA + 5.35
079440081003     c                   eval      §PS7  =  §PS6 + 0.08
079450081003     c                   eval      §PS8  =  §PS7 + 0.08
079460080708      *
079470050321if  1c                   if        Page  <= 1
079480050218      *   (posizione di "partenza" su 1ª pagina)
079490080924     c                   eval      §PB1  =  §PBA + 1.375
079500050218x   1c                   else
079510050318      *   (posizione di "partenza" su pagine successive)
079520080822     c                   eval      §PB1  =  §PBA
079530050218e   1c                   endif
079540080924     c                   eval      §PB2  =  §PBA + 5.275
079550050218      * Impostazione posizioni di linee (orizzontali e verticali)
079560050218      *   nella parte superiore del box (testata)
079570050218     c                   eval      §LO1  =  §PB1 + 0.13
079580050318if  1c                   if        %subst(AR8trk:1:1) <> 'Z'
079590050218     c                   eval      §LO2  =  §LO1 + 0.13
079600050318     c                   eval      §LO3  =  §LO2 + 0.11
079610050318x   1c                   else
079620080825     c                   eval      §LO2  =  §LO1 + 0.115
079630050318     c                   eval      §LO3  =  §LO2
079640050318e   1c                   endif
079650050607if  1c                   if        %subst(AR8trk:1:1) = 'S'
079660050610     c                   eval      §LO3  =  §PB2 - 0.40
079670050607     c                   EndIf
079680050218      * Impostazione posizioni di linee (orizzontali e verticali)
079690050218      *   nella parte inferiore del box (piede)
079700050318if  1c                   if        %subst(AR8trk:1:1) <> 'Z'
079710080822     c                   eval      §LO4  =  §PB2 - 0.52
079720090422     c                   eval      §LO5  =  §LO4 + 0.13
079730090422     c                   eval      §LO6  =  §LO5 + 0.13
079740090422     c                   eval      §LO7  =  §LO6 + 0.13
079750050318e   1c                   endif
079760050218      * Impostazione lunghezza delle linee verticali
079770050218      *   nella parte superiore del box (testata)
079780050218     c                   eval      §LV1  =  §LO1 - §PB1
079790050218     c                   eval      §LV2  =  §LO2 - §LO1
079800050318if  1c                   if        %subst(AR8trk:1:1) <> 'Z'
079810050218     c                   eval      §LV3  =  §LO3 - §LO2
079820050318e   1c                   endif
079830050607if  1c                   if        %subst(AR8trk:1:1) = 'S'
079840050607     c                   eval      §LV2  =  0.40
079850050607     c                   EndIf
079860050218      * La lunghezza delle linee verticali separatrici nel dettaglio
079870050218      *   dipende dalla fase di stampa: se si sta stampando ancora il
079880050218      *   dettaglio deve essere più lunga, visto che non viene stampa-
079890050218      *   ta la parte finale del DDT...
079900080924if  1c                   if        %subst(AR8trk:1:1) = 'P'
079910080924     c                              and  (§PS1        < $LimOF
079920080924     c                                and NOT *in45
079930080924     c                                OR  §PS1        < ($LimOF - c_Len1_inch)
079940080924     c                                and *in45   and   NOT *in46
079950081003     c                                OR  §PS1        < ($LimOF + c_Len2_inch)
079960080924     c                                and *in45   and   *in46)
079970050318     c                              and   CurLineA4   <  wMaxLine
079980050218     c                   eval      §LV4  =  §LO4 - §LO3
079990050218x   1c                   else
080000050218     c                   eval      §LV4  =  §PB2 - §LO3
080010050218e   1c                   endif
080020080908      *
080030071008     c                   if        §AR8tsd = 'L3' or §AR8tsd = 'L4' and
080040060609     c                             %subst(AR8trk:1:1) <> 'Z' and
080050060609     c                             %subst(AR8trk:1:1) <> 'S'
080060071008     c                   eval      §LV3 = 0
080070071008     c                   eval      §LV4 = 0
080080060609     c                   endif
080090080908      *
080100050502      * - qui imposto le posizioni per il piede del box
080110050318if  1c                   if        %subst(AR8trk:1:1) <> 'Z'
080120050218     c                   eval      §LV5  =  §LO5 - §LO4
080130050218     c                   eval      §LV6  =  §LO6 - §LO5
080140050218     c                   eval      §LV7  =  §PB2 - §LO7
080150050318e   1c                   endif
080160041028      * Impostazione posizioni dei campi di testo verticali
080170080822     c                   eval      §PR1  =  §PB2 - 0.05
080180041020      *
080190041020     c                   ENDSR
080200050113      *
080210050113      *---------------------------------------------------------------*
080220050217      *?Stampa DDT del mittente ARCTE                                ?*
080230050113      *---------------------------------------------------------------*
080240050217     c     DDT_Arcte     BEGSR
080250050113      *
080260081002      /free
080270081002         // Impostazione del nuovo limite x overflow
080280081002         // (Codesto limite è valido solo per la stampa del dettaglio.
080290081002         //   Per la stampa del "fondo" bolla occorre REimpostarlo ad
080300081002         //   un valore più basso.
080310081002         //  Ovvero: stampo più righe di dettaglio che posso; se alla
080320081002         //  fine delle righe di dettaglio non rimane spazio a suffi-
080330081002         //  cienza per il fondo: prima della stampa del fondo si salta
080340081002         //  alla pagina successiva, dove non si stampa alcuna riga di
080350081002         //  dettaglio.)
080360081002         select;
080370081002           when  NOT *in45;
080380081002             wMaxLine = $MaxLine2;
080390081002           when  *in45   and   NOT *in46;
080400090324             wMaxLine = $MaxLine5_pl1;
080410081002           when  *in45   and   *in46;
080420090324             wMaxLine = $MaxLine5_pl2;
080430081002         endsl;
080440081002
080450081002         // Posizionamento iniziale all'interno del foglio A4
080460081002         write  A4prtSP1;
080470081002      /end-free
080480050113      *
080490050113      * Impostazione testi fissi - - - - - - - - - - - - - - - - - - -*
080500050113      *
080510050113     c                   eval      PCLtx1 =
080520050113     c                             'La merce viaggia a rischio e pericolo -
080530050113     c                              del committente. I reclami vanno fatti -
080540050113     c                              per iscritto entro 8 gg. dal ricevimento -
080550050113     c                              delle merce.'
080560050113     c                   eval      PCLtx2 =
080570050113     c                              'Il ritorno di merce deve -
080580050113     c                              essere preventivamente autorizzato -
080590050113     c                              dal Gruppo Arcte. Foro competente -
080600050113     c                              come da condizioni di vendita.'
080610050113      *
080620050113     c                   eval      PCLpx1 =
080630050113     c                             'CONTRIBUTO CONAI ASSOLTO'
080640050217      *
080650130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
080660130315      *  "StampaDDT")
080670050217     c***  KLDV          setll     FIAR8000       (già fatto)
080680050217     c***  KLDV          reade     FIAR8000       (già fatto)
080690050113      *
080700050113      * Reperimento dati, impostazione e stampa DDT mittente  - - - -*
080710050113      *
080720050113if  1c                   dow       NOT %eof(FIAR801L)
080730050113      *
080740050113sel 2c                   select
080750050113      * - Record annullato (non si stampa nulla)
080760050217w   2c                   when      AR8atb  <> *blanks
080770050707      * - Dati di dettaglio: descrizioni generiche
080780050217w   2c                   when      AR8trk   = 'C1'
080790050217     c                   exsr      PrtDdtARCT_C1
080800050217      * - Dati di dettaglio: articolo, descrizioni, taglie, q.tà
080810050217w   2c                   when      AR8trk   = 'C2'
080820050217     c                   exsr      PrtDdtARCT_C2
080830050113      * - Dati di testata: destinatario, cliente di fatturazione
080840050217w   2c                   when      AR8trk   = 'T1'
080850050217     c                   exsr      PrtDdtARCT_T1
080860050113      * - Dati di testata: agente, numero ordine, estremi documento
080870050217w   2c                   when      AR8trk   = 'T2'
080880050217     c                   exsr      PrtDdtARCT_T2
080890050113      * - Dati di fondo: mezzo e causale trasporto, colli, peso
080900050217w   2c                   when      AR8trk   = 'P1'
080910050217     c                   exsr      PrtDdtARCT_P1
080920050113e   2c                   endsl
080930050113      *
080940050113     c     KLDV          reade     FIAR8000
080950050113      *
080960050113e   1c                   enddo
080970050217      *
080980050217      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
080990050217      *
081000050217      * - - Verifica OverFlow
081010050217     c                   exsr      Test_OF
081020050218      *
081030050218      * - - Calcolo la posizione finale del box e di linee separatrici
081040050218     c                   exsr      Posiz_AC
081050050217      *
081060050217      * - - Stampa parte finale DDT
081070050217     c                   write     A4prtAC4
081080050217      *
081090050217      * > Stampa box, linee e testi fissi (no logo)  - - - - - - - - -*
081100081017     c                   if            §AR8cxd = *blanks
081110081017     c                             or  DB0fa4  = 'P'
081120050331     c                   write     A4prtDT0
081130050331     c                   endif
081140050217     c                   write     A4prtAC0A
081150050217     c                   write     A4prtAC0B
081160050217      *
081170050217      * Reimpostazione del vecchio limite x overflow
081180081002      /free
081190081002         select;
081200081002           when  NOT *in45;
081210081002             wMaxLine = $MaxLine2;
081220081002           when  *in45   and   NOT *in46;
081230081002             wMaxLine = $MaxLine2_pl1;
081240081002           when  *in45   and   *in46;
081250081002             wMaxLine = $MaxLine2_pl2;
081260081002         endsl;
081270081002      /end-free
081280050113      *
081290050113     c                   ENDSR
081300050214      *
081310050113      *---------------------------------------------------------------*
081320050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
081330050217      *?Dati di testata: destinatario, cliente di fatturazione       ?*
081340050113      *---------------------------------------------------------------*
081350050217     c     PrtDdtARCT_T1 BEGSR
081360050114      *
081370050114      * > logo
081380050114     c                   eval      §Logo  = 'LOGO' + AR8tmd
081390050113      *
081400051110     c                   clear                   dAR8ARCTT1
081410050707     c                   movel     AR8uni        dAR8ARCTT1
081420050706      *
081430050706      * TEST effettiva numericità dei campi previsti numerici:
081440100513     c                   clear                   w053a
081450050706     c                   eval      WX    =  %len(§ARCTT1a02)
081460100513     c                   eval      w053a =  %subst(dAR8ARCTT1:  7:WX)
081470050706     c                   eval      *in31 =  *off
081480110426     c     c_Digits      checkr    w053a :  WX                            31
081490050706if  1c                   if        *in31
081500050706     c                   clear                   §ARCTT1a02
081510050706e   1c                   endif
081520050706     c                   eval      WX    =  %len(§ARCTT1b02)
081530100513     c                   eval      w053a =  %subst(dAR8ARCTT1:163:WX)
081540050706     c                   eval      *in31 =  *off
081550110426     c     c_Digits      checkr    w053a :  WX                            31
081560050706if  1c                   if        *in31
081570050706     c                   clear                   §ARCTT1b02
081580050706e   1c                   endif
081590050113      *
081600050113      * > destinatario
081610050707     c                   eval      PARCTccd  = §ARCTT1a01
081620050707     c                   eval      PARCTccd1 = §ARCTT1a02
081630050707     c                   eval      PARCTrsd  = §ARCTT1a04
081640050707     c                   eval      PARCTrsd2 = §ARCTT1a05
081650050707     c                   eval      PARCTind  = §ARCTT1a06
081660050707     c                   eval      PARCTcad  = §ARCTT1a07
081670050707     c                   eval      PARCTlod  = §ARCTT1a08
081680050113      * > cliente di fatturazione
081690050707     c                   eval      PARCTccf  = §ARCTT1b01
081700050707     c                   eval      PARCTccf1 = §ARCTT1b02
081710050707     c                   eval      PARCTccf2 = §ARCTT1b03
081720050707     c                   eval      PARCTrsf  = §ARCTT1b04
081730050707     c                   eval      PARCTrsf2 = §ARCTT1b05
081740050707     c                   eval      PARCTinf  = §ARCTT1b06
081750050707     c                   eval      PARCTcaf  = §ARCTT1b07
081760050707     c                   eval      PARCTlof  = §ARCTT1b08
081770050113      * > magazzino spedizione
081780050707     c                   eval      PARCTmag  = §ARCTT1c01
081790050113      *
081800050113     c                   ENDSR
081810050113      *
081820050113      *---------------------------------------------------------------*
081830050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
081840050217      *?Dati di testata: documento                                   ?*
081850050113      *---------------------------------------------------------------*
081860050217     c     PrtDdtARCT_T2 BEGSR
081870050113      *
081880051110     c                   clear                   dAR8ARCTT2
081890050707     c                   movel     AR8uni        dAR8ARCTT2
081900050706      *
081910050706      * TEST effettiva numericità dei campi previsti numerici:
081920100513     c                   clear                   w053a
081930050706     c                   eval      WX    =  %len(§ARCTT2a02)
081940050706     c                                   +  %len(§ARCTT2a03)
081950100513     c                   eval      w053a =  %subst(dAR8ARCTT2: 9:WX)
081960050706     c                   eval      *in31 =  *off
081970110426     c     c_Digits      checkr    w053a :  WX                            31
081980050706if  1c                   if        *in31
081990050706     c                   clear                   §ARCTT2a02
082000050706     c                   clear                   §ARCTT2a03
082010050706e   1c                   endif
082020050113      *
082030050113      * > dati in testata DDT
082040050113      * - - numero e data documento (DDT)
082050050707     c                   eval      PARCTnsr = §ARCTT2a01
082060050707     c                   eval      PARCTnsp = §ARCTT2a02
082070050707     c                   eval      PARCTdsp = §ARCTT2a03
082080050113      *
082090050113      * > Stampa testata DDT - - - - - - - - - - - - - - - - - - - - -*
082100050113      *
082110050113      *   - OverFlow impossibile qui! -
082120050217     c***                exsr      Test_OF
082130050217     c                   exsr      Posiz_AC
082140081017     c                   if            §AR8cxd = *blanks
082150081017     c                             or  DB0fa4  = 'P'
082160050331     c                   write     A4prtDT0
082170050331     c                   endif
082180050217     c                   write     A4prtAC0
082190050217     c*** quante righe?  write     A4prtAC0A
082200050217     c                   write     A4prtAC1A
082210050217     c                   write     A4prtAC1B
082220050113      *
082230050113     c                   ENDSR
082240050214      *
082250050113      *---------------------------------------------------------------*
082260050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
082270050217      *?Dati di dettaglio: descrizioni generiche                     ?*
082280050113      *---------------------------------------------------------------*
082290050217     c     PrtDdtARCT_C1 BEGSR
082300050113      *
082310050114     c                   clear                   A4prtAC2
082320051110     c                   clear                   dAR8ARCTC1
082330050707     c                   movel     AR8uni        dAR8ARCTC1
082340050217      * > dati in note DDT
082350050217      * - - riga descrittiva
082360050707     c                   eval      PARCTdpr  = §ARCTC1a01
082370050113      *
082380050113      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
082390050113      *
082400050113     c                   exsr      Test_OF
082410050114     c                   write     A4prtAC2
082420050113      *
082430050113     c                   ENDSR
082440050113      *
082450050113      *---------------------------------------------------------------*
082460050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
082470050217      *?Dati di dettaglio: descrizione, taglie, qtà                  ?*
082480050113      *---------------------------------------------------------------*
082490050217     c     PrtDdtARCT_C2 BEGSR
082500050113      *
082510050114     c                   clear                   A4prtAC3
082520051110     c                   clear                   dAR8ARCTC2
082530050707     c                   movel     AR8uni        dAR8ARCTC2
082540050706      *
082550050706      * TEST effettiva numericità dei campi previsti numerici:
082560100513     c                   clear                   w053a
082570050706     c                   eval      WX    =  %len(§ARCTC2b00)
082580050706     c                                   +  %len(§ARCTC2b01)
082590050706     c                                   +  %len(§ARCTC2b02)
082600050706     c                                   +  %len(§ARCTC2b03)
082610050706     c                                   +  %len(§ARCTC2b04)
082620050706     c                                   +  %len(§ARCTC2b05)
082630050706     c                                   +  %len(§ARCTC2b06)
082640050706     c                                   +  %len(§ARCTC2b07)
082650050706     c                                   +  %len(§ARCTC2b08)
082660050706     c                                   +  %len(§ARCTC2b09)
082670100513     c                   eval      w053a =  %subst(dAR8ARCTC2:59:WX)
082680050706     c                   eval      *in31 =  *off
082690110426     c     c_Digits      checkr    w053a :  WX                            31
082700050706if  1c                   if        *in31
082710050706     c                   clear                   §ARCTC2b00
082720050706     c                   clear                   §ARCTC2b01
082730050706     c                   clear                   §ARCTC2b02
082740050706     c                   clear                   §ARCTC2b03
082750050706     c                   clear                   §ARCTC2b04
082760050706     c                   clear                   §ARCTC2b05
082770050706     c                   clear                   §ARCTC2b06
082780050706     c                   clear                   §ARCTC2b07
082790050706     c                   clear                   §ARCTC2b08
082800050706     c                   clear                   §ARCTC2b09
082810050706e   1c                   endif
082820050113      *
082830050113      * > dati in dettaglio DDT
082840050113      * - - descrizione articolo
082850050707     c                   eval      PARCTcpr  = §ARCTC2baa
082860050113      * - - taglia 0
082870050707     c                   eval      PARCTt0   = §ARCTC2b00
082880050113      * - - taglia 1
082890050707     c                   eval      PARCTt1   = §ARCTC2b01
082900050113      * - - taglia 2
082910050707     c                   eval      PARCTt2   = §ARCTC2b02
082920050113      * - - taglia 3
082930050707     c                   eval      PARCTt3   = §ARCTC2b03
082940050113      * - - taglia 4
082950050707     c                   eval      PARCTt4   = §ARCTC2b04
082960050113      * - - taglia 5
082970050707     c                   eval      PARCTt5   = §ARCTC2b05
082980050113      * - - taglia 6
082990050707     c                   eval      PARCTt6   = §ARCTC2b06
083000050113      * - - taglia 7
083010050707     c                   eval      PARCTt7   = §ARCTC2b07
083020050113      * - - taglia 8
083030050707     c                   eval      PARCTt8   = §ARCTC2b08
083040050113      * - - quantità
083050050707     c                   eval      PARCTqta  = §ARCTC2b09
083060050113      *
083070050113      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
083080050113      *
083090050113     c                   exsr      Test_OF
083100050114     c                   write     A4prtAC3
083110050113      *
083120050113     c                   ENDSR
083130050113      *
083140050113      *---------------------------------------------------------------*
083150050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
083160050217      *?Dati di fondo: beni, colli, peso, tot.qtà, porto, trasp.     ?*
083170050113      *---------------------------------------------------------------*
083180050217     c     PrtDdtARCT_P1 BEGSR
083190090324      *
083200090324      /free
083210090324           select;
083220090324             when  NOT *in45;
083230090324               wMaxLine = $MaxLine4;
083240090324             when  *in45   and   NOT *in46;
083250090324               wMaxLine = $MaxLine4_pl3;
083260090324             when  *in45   and   *in46;
083270090324               wMaxLine = $MaxLine4_pl4;
083280090324           endsl;
083290090324      /end-free
083300050113      *
083310051110     c                   clear                   dAR8ARCTP1
083320050707     c                   movel     AR8uni        dAR8ARCTP1
083330050706      *
083340050706      * TEST effettiva numericità dei campi previsti numerici:
083350100513     c                   clear                   w053a
083360050706     c                   eval      WX    =  %len(§ARCTP1a03)
083370100513     c                   eval      w053a =  %subst(dAR8ARCTP1:28:WX)
083380050706     c                   eval      *in31 =  *off
083390110426     c     c_Digits      checkr    w053a :  WX                            31
083400050706if  1c                   if        *in31
083410050706     c                   clear                   §ARCTP1a03
083420050706e   1c                   endif
083430050706     c                   eval      WX    =  %len(§ARCTP1b02)
083440050706     c                                   +  %len(§ARCTP1b03)
083450050706     c                                   +  %len(§ARCTP1b04)
083460100513     c                   eval      w053a =  %subst(dAR8ARCTP1:68:WX)
083470050706     c                   eval      *in31 =  *off
083480110426     c     c_Digits      checkr    w053a :  WX                            31
083490050706if  1c                   if        *in31
083500050706     c                   clear                   §ARCTP1b02
083510050706     c                   clear                   §ARCTP1b03
083520050706     c                   clear                   §ARCTP1b04
083530050706e   1c                   endif
083540050113      *
083550050113      * > dati in chiusura DDT
083560050114      * - - costanti di fine doc. e n. borderò
083570050707     c                   eval      PARCTc1   = §ARCTP1a01
083580050707     c                   eval      PARCTc2   = §ARCTP1a02
083590050707     c                   eval      PARCTc3   = §ARCTP1a03
083600050114      * - - costante segue fattura
083610050707     c                   eval      PARCTc4   = §ARCTp1a04
083620050113      * - - aspetto esteriore dei beni
083630050707     c                   eval      PARCTaeb  = §ARCTP1b01
083640050113      * - - numero colli
083650050707     c                   eval      PARCTncl  = §ARCTP1b02
083660050113      * - - peso in kg
083670050707     c                   eval      PARCTpkg  = §ARCTP1b03
083680050113      * - - totale qtà
083690050707     c                   eval      PARCTtqta = §ARCTP1b04
083700050113      * - - causale trasporto
083710050707     c                   eval      PARCTctr  = §ARCTP1c01
083720050113      * - - porto
083730050707     c                   eval      PARCTpto  = §ARCTP1c02
083740050113      * - - trasporto a cura
083750050707     c                   eval      PARCTtra  = §ARCTP1c03
083760050113      * - - trasportatore
083770050707     c                   eval      PARCTvet  = §ARCTP1d01
083780050217     *** *
083790050217     *** * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
083800050217     *** * ?(impostata a fine ciclo di lettura - sr. DDT_Arcte)?
083810050113      *
083820050113      * N.B.: la copia per il destinatario della LDV deve comparire
083830050113      *       nell'ultima pagina del DDT per il cliente (vedi subr.
083840050113      *       "ChiusuraLDV")
083850050113      *
083860050113     c                   ENDSR
083870081002      /free
083880081002       //--------------------------------------------------------------
083890140303       //?Impostazione nuova pagina di DDT Arcte                       ?
083900081002       //--------------------------------------------------------------
083910081002       BEGSR  NewPageAC;
083920081002
083930081002         // Impostazione del nuovo limite x overflow
083940081002         select;
083950081002           when  NOT *in45;
083960081002             wMaxLine = $MaxLine2;
083970081002           when  *in45   and   NOT *in46;
083980090324             wMaxLine = $MaxLine5_pl1;
083990081002           when  *in45   and   *in46;
084000090324             wMaxLine = $MaxLine5_pl2;
084010081002         endsl;
084020081002
084030081002         write  A4prtSP1;
084040081002         write  A4prtAC1B;
084050081002
084060081002       ENDSR;
084070081002      /end-free
084080050113      *
084090050113      *---------------------------------------------------------------*
084100050217      *?Posizionamento di linee e box nel DDT ARCTE                  ?*
084110050113      *---------------------------------------------------------------*
084120050114     c     Posiz_AC      BEGSR
084130080708      *
084140081002      /free
084150081002         // Impostazione posizioni di logo/box
084160081002         // (posizioni di "partenza" diverse a seconda del posizionam.
084170081002         //  iniziale nel foglio A4 attualmente in stampa)
084180081002         select;
084190081002           when  NOT *in45;
084200081002             §PBA =  3.130;
084210081002           when  *in45   and   NOT *in46;
084220081002             §PBA = 0.260;
084230081002           when  *in45   and   *in46;
084240081002             §PBA = 6.010;
084250081002         endsl;
084260081002      /end-free
084270080708     c                   eval      §PBB  =  §PBA + 1.27
084280080708     c                   eval      §LOA  =  §PBA + 0.63
084290050218      *
084300050217      * Impostazione posizioni di box
084310050217if  1c                   if        Page  <= 1
084320050217      *   (posizione di "partenza" su 1ª pagina)
084330081002     c                   eval      §PB1  =  §PBA + 1.38
084340050217x   1c                   else
084350050217      *   (posizione di "partenza" su pagine successive)
084360081002     c                   eval      §PB1  =  §PBA
084370050217e   1c                   endif
084380081002     c                   eval      §PB2  =  §PBA + 5.395
084390050217      * Impostazione posizioni di linee (orizzontali e verticali)
084400050217      *   nella parte superiore del box (testata)
084410080708     c                   eval      §LO1  =  §PB1 + 0.13
084420080708     c                   eval      §LO2  =  §LO1 + 0.13
084430050217      * Impostazione posizioni di linee (orizzontali e verticali)
084440050217      *   nella parte inferiore del box (piede)
084450081002     c                   eval      §LO3  =  §PBA + 5.005
084460081002     c                   eval      §LO4  =  §PBA + 5.135
084470081002     c                   eval      §LO5  =  §PBA + 5.265
084480050217      * Impostazione lunghezza delle linee verticali
084490050217      *   nella parte superiore del box (testata)
084500050217     c                   eval      §LV1  =  §LO1 - §PB1
084510050217     c                   eval      §LV2  =  §LO2 - §LO1
084520050217      * La lunghezza delle linee verticali separatrici nel dettaglio
084530050217      *   dipende dalla fase di stampa: se si sta stampando ancora il
084540050217      *   dettaglio deve essere più lunga, visto che non viene stampa-
084550050217      *   ta la parte finale del DDT...
084560050217if  1c                   if        %subst(AR8trk:1:1) = 'P'
084570050218     c                             and    §PS1        < $LimOF
084580050218     c                             and    CurLineA4   < wMaxLine
084590050217     c                   eval      §LV3  =  §LO3 - §LO2
084600050217x   1c                   else
084610050217     c                   eval      §LV3  =  §PB2 - §LO2
084620050217e   1c                   endif
084630050217     c                   eval      §LV4  =  §LO4 - §LO3
084640050217     c                   eval      §LV5  =  §LO5 - §LO4
084650050113      * Impostazione posizioni dei campi di testo verticali
084660081002     c                   eval      §PR1  =  §PBA + 5.35
084670081002     c                   eval      §PR2  =  §PBA + 5.47
084680050113      *
084690050113     c                   ENDSR
084700050214      *
084710050214      *---------------------------------------------------------------*
084720050217      *?Stampa DDT del mittente Esprinet                             ?*
084730050214      *---------------------------------------------------------------*
084740050214     c     DDT_Esprinet  BEGSR
084750050214      *
084760081002      /free
084770110405         // -?Impostazione del nuovo limite x overflow?
084780110405         //  ?Codesto limite è valido solo per la stampa del dettaglio.?
084790110405         //  ?Per la stampa del "fondo" bolla occorre REimpostarlo ad?
084800110405         //  ?un valore più basso.?
084810110405         //  ?Ovvero: stampo più righe di dettaglio che posso; se alla?
084820110405         //  ?fine delle righe di dettaglio non rimane spazio a suffi-?
084830110405         //  ?cienza per il fondo: prima della stampa del fondo si salta?
084840110405         //  ?alla pagina successiva, dove non si stampa alcuna riga di?
084850110405         //  ?dettaglio.?
084860081002         select;
084870081002           when  NOT *in45;
084880081002             wMaxLine = $MaxLine2;
084890081002           when  *in45   and   NOT *in46;
084900090324             wMaxLine = $MaxLine5_pl1;
084910081002           when  *in45   and   *in46;
084920090324             wMaxLine = $MaxLine5_pl2;
084930081002         endsl;
084940081002
084950110405         // -?Posizionamento iniziale all'interno del foglio A4?
084960081002         write  A4prtSP1;
084970110405
084980110405         // -?Pulizia tipo record?
084990110405         clear  SAVtrk;
085000081002      /end-free
085010050214      *
085020050214      * Impostazione testi fissi - - - - - - - - - - - - - - - - - - -*
085030050214      *
085040050325     ***c                   eval      PCLtx1 =
085050050325     ***c                             'Settimo Torinese - Tel. 011.895.39.29 *-
085060050325     ***c                              Padova - Tel. 049.870.50.40 *-
085070050325     ***c                              Bologna - Tel. 051.324.249 *-
085080050325     ***c                              Osimo - Tel. 071.781.96.75 *-
085090050325     ***c                              Roma - Tel. 06.723.15.43'
085100050325     ***c                   eval      PCLtx2 =
085110050325     ***c                             'Casalnuovo - Tel. 081.522.30.31 *-
085120050325     ***c                              Cinisello B. - Tel. 02.66.04.11.13 *-
085130050325     ***c                              Verona - Tel. 045.823.01.65 *-
085140050325     ***c                              Cagliari - Tel. 070.253.178'
085150050325     c                   eval      PCLtx3 =
085160131017     c                             'CONTRIBUTO AMBIENTALE CONAI ASSOLTO'
085170131017     c*//                           OVE DOVUTO'
085180050214      *
085190050214     c                   eval      PCLpx2 =
085200050214     c                             'IL PRESENTE DOCUMENTO E'' REGOLATO-
085210050214     c                              DALLE CONDIZIONI GENERALI DI VENDI-
085220050214     c                             TA PUBBLICATE SUL SITO INTERNET ALL-
085230050214     c                             ''INDIRIZZO www.esprinet.com CHE SI-
085240050325     c                              INTENDONO ESPRESSAMENTE CONOSCIUTE-
085250050325     c                              ED ACCETTATE'
085260050214      *
085270050214     c                   eval      PCLpx3a =
085280050214     c                             'NB. IN CASO DI RITARDATO PAGAMENTO-
085290050214     c                             , VERRANNO ADDEBITATI'
085300050214     c                   eval      PCLpx3b =
085310050214     c                             'INTERESSI AL TASSO DEL PRIME RATE -
085320050214     c                             MAGGIORATO DEL 2% A'
085330050214     c                   eval      PCLpx3c =
085340050214     c                             'DECORRERE DAL GIORNO SUCCESSIVO AL-
085350050214     c                             LA SCADENZA DELLA FATTURA'
085360050214      *
085370050214     c                   eval      PCLpx4a =
085380050214     c                             'Si prega di verificare la Vs. ragi-
085390050214     c                             one sociale, indirizzo e'
085400050214     c                   eval      PCLpx4b =
085410050214     c                             'partita IVA, dandoci eventuale sol-
085420050214     c                             lecita rettifica scritta.'
085430050214     c***                eval      PCLpx4c = *blanks
085440050214      *
085450050214     c                   eval      PCLpx5a =
085460050214     c                             '* I PRODOTTI IMPORTATI DAGLI U.S.A.-
085470050214     c                              SONO CON SPECIALE'
085480050214     c                   eval      PCLpx5b =
085490050214     c                             'LICENZA CHE PRECLUDE RIESPORTAZIONI-
085500050214     c                              NON AUTORIZZATE.'
085510050214     c***                eval      PCLpx5c = *blanks
085520050214      *
085530050325     ***c                   eval      PCLpx6  =
085540050325     ***c                             'CONTRIBUTO AMBIENTALE CONAI ASSOLTO-
085550050325     ***c                              OVE DOVUTO'
085560050325     c                   eval      PCLpx6  =
085570050325     c                             'LA SOCIETÀ SI IMPEGNA A VERSARE L''-
085580050325     c                             IVA NEI MODI E TERMINI PREVISTI (S-
085590050325     c                             OLIDARIETÀ ART. 60 BIS DPR 633/72-F-
085600050325     c                             INANZIARIA 2005)'
085610050214      *
085620130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
085630130315      *  "StampaDDT")
085640050214     c***  KLDV          setll     FIAR8000       (già fatto)
085650050214     c***  KLDV          reade     FIAR8000       (già fatto)
085660050214      *
085670050214      * Reperimento dati, impostazione e stampa DDT mittente  - - - -*
085680050214      *
085690050214if  1c                   dow       NOT %eof(FIAR801L)
085700050214      *
085710050214sel 2c                   select
085720050214      * - Record annullato (non si stampa nulla)
085730050217w   2c                   when      AR8atb  <> *blanks
085740050214      * - Dati di dettaglio: articolo, descrizione, quantità, ...
085750050217w   2c                   when      AR8trk   = 'C1'
085760050217     c                   exsr      PrtDdtESP1_C1
085770050214      * - Dati di dettaglio: testo (riga libera)
085780050217w   2c                   when      AR8trk   = 'C2'
085790050217     c                   exsr      PrtDdtESP1_C2
085800050214      * - Dati di testata: destinatario, cliente di fatturazione
085810050217w   2c                   when      AR8trk   = 'T1'
085820050217     c                   exsr      PrtDdtESP1_T1
085830050214      * - Dati di testata: estremi documento, riferimenti, ...
085840050217w   2c                   when      AR8trk   = 'T2'
085850050217     c                   exsr      PrtDdtESP1_T2
085860050214      * - Dati di fondo: totali del documento (netto merce, iva, ...)
085870050217w   2c                   when      AR8trk   = 'P1'
085880050217     c                   exsr      PrtDdtESP1_P1
085890050214      * - Dati di fondo: informazioni vettore
085900050217w   2c                   when      AR8trk   = 'P2'
085910050217     c                   exsr      PrtDdtESP1_P2
085920050214e   2c                   endsl
085930110405      *
085940110405     c                   eval      SAVtrk = AR8trk
085950050214      *
085960050214     c     KLDV          reade     FIAR8000
085970050214      *
085980050214e   1c                   enddo
085990050215      *
086000050215      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
086010050215      *
086020050215      * - - Verifica OverFlow
086030050215     c                   exsr      Test_OF
086040050218      *
086050050218      * - - Calcolo la posizione finale del box e di linee separatrici
086060050218     c                   exsr      Posiz_ES
086070050215      *
086080050215      * - - Stampa parte finale DDT
086090050215     c                   write     A4prtES4
086100050215      *
086110050215      * > Stampa box, linee e testi fissi (no logo)  - - - - - - - - -*
086120081017     c                   if            §AR8cxd = *blanks
086130081017     c                             or  DB0fa4  = 'P'
086140050331     c                   write     A4prtDT0
086150050331     c                   endif
086160050215     c                   write     A4prtES0A
086170050215     c                   write     A4prtES0B
086180050214      *
086190050214      * Reimpostazione del vecchio limite x overflow
086200081002      /free
086210081002         select;
086220081002           when  NOT *in45;
086230081002             wMaxLine = $MaxLine2;
086240081002           when  *in45   and   NOT *in46;
086250081002             wMaxLine = $MaxLine2_pl1;
086260081002           when  *in45   and   *in46;
086270081002             wMaxLine = $MaxLine2_pl2;
086280081002         endsl;
086290081002      /end-free
086300050214      *
086310050214     c                   ENDSR
086320050214      *
086330050214      *---------------------------------------------------------------*
086340050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
086350050217      *?Dati di testata: fatturato a, destinazione, cliente          ?*
086360050214      *---------------------------------------------------------------*
086370050217     c     PrtDdtESP1_T1 BEGSR
086380110405      *
086390110405      * - pulizia record a nuova packing-list
086400110405     c                   if            %subst( SAVtrk : 1 : 1 ) = 'P'
086410110405     c                             or  %subst( SAVtrk : 1 : 1 ) = 'C'
086420110405     c                   exsr      sr_ClearESP1
086430110405     c                   endif
086440050214      *
086450050214      * > logo
086460050214     c                   eval      §Logo  = 'LOGO' + AR8tmd
086470050214      *
086480051110     c                   movel(p)  AR8uni        dAR8ESP1T1
086490050214      *
086500050214      * > fatturato a
086510050707     c                   eval      PCrsf  = §A8ESP1T1a
086520050707     c                   eval      PCinf  = §A8ESP1T1b
086530050707if  1c                   if        §A8ESP1T1c > *zeros
086540050707     c                   eval      PCclpf = §A8ESP1T1c
086550050216     c                                    + ' '
086560050707     c                                    + §A8ESP1T1d
086570050216     c                                    + ' '
086580050707     c                                    + §A8ESP1T1e
086590050216x   1c                   else
086600050216     c                   eval      PCclpf = '     '
086610050216     c                                    + ' '
086620050707     c                                    + §A8ESP1T1d
086630050216     c                                    + ' '
086640050707     c                                    + §A8ESP1T1e
086650050216e   1c                   endif
086660050214      * > destinatazione
086670050707     c                   eval      PCrsd  = §A8ESP1T1f
086680050707     c                   eval      PCind  = §A8ESP1T1g
086690050707if  1c                   if        §A8ESP1T1h > *zeros
086700050707     c                   eval      PCclpd = §A8ESP1T1h
086710050216     c                                    + ' '
086720050707     c                                    + §A8ESP1T1i
086730050216     c                                    + ' '
086740050707     c                                    + §A8ESP1T1j
086750050216x   1c                   else
086760050216     c                   eval      PCclpd = '     '
086770050216     c                                    + ' '
086780050707     c                                    + §A8ESP1T1i
086790050216     c                                    + ' '
086800050707     c                                    + §A8ESP1T1j
086810050216e   1c                   endif
086820050214      * > dati in testata DDT
086830050214      * - - codice cliente
086840050707     c                   eval      PCcli  = §A8ESP1T1k
086850050214      * - - codice fiscale
086860050707     c                   eval      PCcf   = §A8ESP1T1l
086870050214      * - - partita iva
086880050707     c                   eval      PCpi   = §A8ESP1T1m
086890050214      *
086900050214     c                   ENDSR
086910050214      *
086920050214      *---------------------------------------------------------------*
086930050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
086940050217      *?Dati di testata: documento, riferimenti.                     ?*
086950050214      *---------------------------------------------------------------*
086960050217     c     PrtDdtESP1_T2 BEGSR
086970110405      *
086980110405      * - pulizia records a nuova packing-list
086990110405     c                   if            %subst( SAVtrk : 1 : 1 ) = 'P'
087000110405     c                             or  %subst( SAVtrk : 1 : 1 ) = 'C'
087010110405     c                   exsr      sr_ClearESP1
087020110405     c                   endif
087030050214      *
087040051110     c                   movel(p)  AR8uni        dAR8ESP1T2
087050050214      *
087060050214      * > dati in testata DDT
087070050214      * - - tipo documento
087080050707     c                   eval      PCtdo  = §A8ESP1T2a
087090050214      * - - data e numero documento (DDT)
087100050707     c                   eval      PCdtd  = §A8ESP1T2b
087110050707     c                   eval      PCnrd  = §A8ESP1T2c
087120050214      * - - riferimenti mittente
087130050707     c                   eval      PCrif  = §A8ESP1T2d
087140050214      * - - condizioni di pagamento
087150050707     c                   eval      PCpag  = §A8ESP1T2e
087160050214      * - - annotazioni
087170050707     c                   eval      PCann  = §A8ESP1T2f
087180050214      * - - agente
087190050707     c                   eval      PCag2  = §A8ESP1T2g
087200050214      *
087210050214      * > Stampa testata DDT - - - - - - - - - - - - - - - - - - - - -*
087220050214      *
087230050214      *   - OverFlow impossibile qui! -
087240050215     c***                exsr      Test_OF
087250050215     c                   exsr      Posiz_ES
087260081017     c                   if            §AR8cxd = *blanks
087270081017     c                             or  DB0fa4  = 'P'
087280050331     c                   write     A4prtDT0
087290050331     c                   endif
087300050214     c                   write     A4prtES0
087310081002     c*** (NON qui:)     write     A4prtES0A
087320081002     c                   write     A4prtSP1
087330050215     c                   write     A4prtES1A
087340050215     c                   write     A4prtES1B
087350050214      *
087360050214     c                   ENDSR
087370050214      *
087380050214      *---------------------------------------------------------------*
087390050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
087400050217      *?Dati di dettaglio: articolo, descrizione, quantità, ...      ?*
087410050214      *---------------------------------------------------------------*
087420050217     c     PrtDdtESP1_C1 BEGSR
087430110405      *
087440110405      * - pulizia records a nuova packing-list
087450110405     c                   if        %subst( SAVtrk : 1 : 1 ) = 'P'
087460110405     c                   exsr      sr_ClearESP1
087470110405     c                   endif
087480050214      *
087490050214     c                   clear                   A4prtES2
087500051110     c                   movel(p)  AR8uni        dAR8ESP1C1
087510050214      *
087520050214      * > dati in dettaglio DDT
087530050214      * - - codice prodotto
087540050707     c                   eval      PCcpr  = §A8ESP1C1a
087550050214      * - - descrizione prodotto
087560050707     c                   eval      PCdpr  = §A8ESP1C1b
087570050214      * - - quantità
087580050707     c                   eval      PCqta  = '      ' + §A8ESP1C1c
087590050214      * - - prezzo unitario
087600050707     c                   eval      PCpru  = §A8ESP1C1d
087610050214      * - - cambio: divisa + valore
087620050707     c                   eval      PCdiv  = §A8ESP1C1e
087630050707     c                   eval      PCcmb  = §A8ESP1C1f
087640050214      * - - % sconto
087650050707     c                   eval      PCprc  = §A8ESP1C1g
087660050214      * - - importo
087670050707     c                   eval      PCimp  = §A8ESP1C1h
087680050214      * - - codice iva
087690050707     c                   eval      PCciv  = §A8ESP1C1i
087700050214      *
087710050214      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
087720050214      *
087730050214     c                   exsr      Test_OF
087740050214     c                   write     A4prtES2
087750050214      *
087760050214     c                   ENDSR
087770050214      *
087780050214      *---------------------------------------------------------------*
087790050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
087800050217      *?Dati di dettaglio: note (riga libera)                        ?*
087810050214      *---------------------------------------------------------------*
087820050217     c     PrtDdtESP1_C2 BEGSR
087830110405      *
087840110405      * - pulizia records a nuova packing-list
087850110405     c                   if        %subst( SAVtrk : 1 : 1 ) = 'P'
087860110405     c                   exsr      sr_ClearESP1
087870110405     c                   endif
087880050214      *
087890050215     c                   clear                   A4prtES3
087900051110     c                   movel(p)  AR8uni        dAR8ESP1C2
087910050214      *
087920050214      * > dati in note DDT
087930050214      * - - riga descrittiva
087940050707     c                   eval      PCrig  = §A8ESP1C2a
087950050214      *
087960050214      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
087970050214      *
087980050214     c                   exsr      Test_OF
087990050214     c                   write     A4prtES3
088000050214      *
088010050214     c                   ENDSR
088020050214      *
088030050214      *---------------------------------------------------------------*
088040050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
088050050217      *?Dati di fondo: totali documento                              ?*
088060050214      *---------------------------------------------------------------*
088070050217     c     PrtDdtESP1_P1 BEGSR
088080050214      *
088090081002      /free
088100081002           select;
088110081002             when  NOT *in45;
088120081002               wMaxLine = $MaxLine6;
088130081002             when  *in45   and   NOT *in46;
088140081002               wMaxLine = $MaxLine6_pl1;
088150081002             when  *in45   and   *in46;
088160081002               wMaxLine = $MaxLine6_pl2;
088170081002           endsl;
088180081002      /end-free
088190051110     c                   movel(p)  AR8uni        dAR8ESP1P1
088200050215      *
088210050215      * > dati in fondo DDT
088220050215      * - - netto merce
088230050707     c                   eval      PCinm  = §A8ESP1P1a
088240050215      * - - spese di trasporto
088250050707     c                   eval      PCist  = §A8ESP1P1b
088260050215      * - - spese imballo
088270050707     c                   eval      PCisi  = §A8ESP1P1c
088280050215      * - - spese bolli
088290050707     c                   eval      PCisb  = §A8ESP1P1d
088300050215      * - - spese amministrative
088310050707     c                   eval      PCisa  = §A8ESP1P1e
088320050215      * - - spese varie
088330050707     c                   eval      PCisv  = §A8ESP1P1f
088340050215      * - - codici iva
088350050707     c                   eval      PCic1  = §A8ESP1P1g
088360050707     c                   eval      PCic2  = §A8ESP1P1m
088370050707     c                   eval      PCic3  = §A8ESP1P1s
088380050215      * - - descrizioni iva
088390050707     c                   eval      PCid1  = §A8ESP1P1h
088400050707     c                   eval      PCid2  = §A8ESP1P1n
088410050707     c                   eval      PCid3  = §A8ESP1P1t
088420050215      * - - imponibili iva: divise
088430050707     c                   eval      PCie1  = §A8ESP1P1i
088440050707     c                   eval      PCie2  = §A8ESP1P1o
088450050707     c                   eval      PCie3  = §A8ESP1P1u
088460050215      * - - imponibili iva: importi
088470050707     c                   eval      PCii1  = §A8ESP1P1j
088480050707     c                   eval      PCii2  = §A8ESP1P1p
088490050707     c                   eval      PCii3  = §A8ESP1P1v
088500050215      * - - aliquote iva
088510050707     c                   eval      PCia1  = §A8ESP1P1k
088520050707     c                   eval      PCia2  = §A8ESP1P1q
088530050707     c                   eval      PCia3  = §A8ESP1P1w
088540050215      * - - imposte iva
088550050707     c                   eval      PCiv1  = §A8ESP1P1l
088560050707     c                   eval      PCiv2  = §A8ESP1P1r
088570050707     c                   eval      PCiv3  = §A8ESP1P1x
088580050215      * - - Totali documento: divise + importi
088590050707     c                   eval      PCtdd  = §A8ESP1P1y
088600050707     c                   eval      PCtdi  = §A8ESP1P1z
088610050707     c                   eval      PCtdd2 = §A8ESP1P11
088620050707     c                   eval      PCtdi2 = §A8ESP1P12
088630050214      *
088640050214     c                   ENDSR
088650050214      *
088660050214      *---------------------------------------------------------------*
088670050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
088680050217      *?Dati di fondo: informazioni vettore                          ?*
088690050214      *---------------------------------------------------------------*
088700050217     c     PrtDdtESP1_P2 BEGSR
088710050214      *
088720081002      /free
088730081002           select;
088740081002             when  NOT *in45;
088750081002               wMaxLine = $MaxLine6;
088760081002             when  *in45   and   NOT *in46;
088770081002               wMaxLine = $MaxLine6_pl1;
088780081002             when  *in45   and   *in46;
088790081002               wMaxLine = $MaxLine6_pl2;
088800081002           endsl;
088810081002      /end-free
088820051110     c                   movel(p)  AR8uni        dAR8ESP1P2
088830050215      *
088840050215      * > dati in fondo DDT
088850050215      * - - resa
088860050707     c                   eval      PCres  = §A8ESP1P2a
088870050215      * - - trasporto a cura del
088880050707     c                   eval      PCtra  = §A8ESP1P2b
088890050215      * - - vettore
088900110426     c*//                eval      PCvet  = $Bartolini
088910110426     c*//                                 + ' - '
088920110426     c*//                                 + $Intest2
088930110426     c*//                                 + ' (BO)'
088940050707     c                   eval      PCvet2 = §A8ESP1P2c
088950050215      * - - causale trasporto
088960050707     c                   eval      PCctr2 = §A8ESP1P2d
088970050215      * - - aspetto esteriore dei beni
088980050707     c                   eval      PCaeb  = §A8ESP1P2e
088990050215      * - - numero colli
089000050707     c                   eval      PCncl2 = §A8ESP1P2f
089010050215      * - - peso complessivo
089020050707     c                   eval      PCpes  = §A8ESP1P2g
089030050215      * - - volume
089040050707     c                   eval      PCvol  = §A8ESP1P2h
089050050215      * - - inizio trasporto: data + ora
089060050707     c                   eval      PCdit  = §A8ESP1P2i
089070050707     c                   eval      PCoit  = §A8ESP1P2j
089080050215     *** *
089090050215     *** * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
089100050215     *** * ?(impostata a fine ciclo di lettura - sr. DDT_Esprinet)?
089110050217      *
089120050217      * N.B.: la copia per il destinatario della LDV deve comparire
089130050217      *       nell'ultima pagina del DDT per il cliente (vedi subr.
089140050217      *       "ChiusuraLDV")
089150050214      *
089160050214     c                   ENDSR
089170081002      /free
089180081002       //--------------------------------------------------------------
089190110405       //?Impostazione nuova pagina di DDT Esprinet                    ?
089200081002       //--------------------------------------------------------------
089210081002       BEGSR  NewPageES;
089220081002
089230110405         // -?Impostazione del nuovo limite x overflow?
089240081002         select;
089250081002           when  NOT *in45;
089260081002             wMaxLine = $MaxLine2;
089270081002           when  *in45   and   NOT *in46;
089280090324             wMaxLine = $MaxLine5_pl1;
089290081002           when  *in45   and   *in46;
089300090324             wMaxLine = $MaxLine5_pl2;
089310081002         endsl;
089320081002
089330081002         write  A4prtSP1;
089340081002         write  A4prtES1B;
089350081002
089360081002       ENDSR;
089370110405
089380110405       //--------------------------------------------------------------
089390110405       //?Pulizia campi a nuova packing-list relativa alla stessa LdV  ?
089400110405       //--------------------------------------------------------------
089410110405       BEGSR  sr_ClearESP1;
089420110405
089430110405         // -?Stampa fondo DDT?
089440110405
089450110405         // - -?Calcolo la posizione finale del box e di linee?
089460110405         //    ?separatrici?
089470110405         exsr  Posiz_ES;
089480110405
089490110405         // - -?Stampa parte finale DDT?
089500110405         write  A4prtES4;
089510110405
089520110405         // - -?Stampa box, linee e testi fissi (no logo)?
089530110405         if  §AR8cxd = *blank  or  DB0fa4 = 'P';
089540110405           write  A4prtDT0;
089550110405         endif;
089560110405         write  A4prtES0A;
089570110405         write  A4prtES0B;
089580110405
089590110405         // -?Pulizia record?
089600110405         select;
089610110405           when  %subst( AR8trk : 1 : 1 ) = 'T';
089620110405             clear  A4prtES0;
089630110405             clear  A4prtES0A;
089640110405             clear  A4prtES0B;
089650110405             clear  A4prtES1A;
089660110405             clear  A4prtES1B;
089670110405             clear  A4prtES2;
089680110405             clear  A4prtES3;
089690110405             clear  A4prtES4;
089700110405           when  %subst( AR8trk : 1 : 1 ) = 'C';
089710110405             clear  A4prtES2;
089720110405             clear  A4prtES3;
089730110405             clear  A4prtES4;
089740110405         endsl;
089750110405
089760110405         // -?Impostazione testi fissi?
089770131017         //PCLtx3 = 'CONTRIBUTO AMBIENTALE CONAI ASSOLTO OVE DOVUTO';
089780131017         PCLtx3 = 'CONTRIBUTO AMBIENTALE CONAI ASSOLTO';
089790110405
089800110405         PCLpx2 = 'IL PRESENTE DOCUMENTO E'' REGOLATO DALLE +
089810110405                   CONDIZIONI GENERALI DI VENDITA PUBBLICATE SUL SITO +
089820110405                   INTERNET ALL''INDIRIZZO www.esprinet.com CHE SI +
089830110405                   INTENDONO ESPRESSAMENTE CONOSCIUTE ED ACCETTATE';
089840110405
089850110405         PCLpx3a = 'NB. IN CASO DI RITARDATO PAGAMENTO, VERRANNO +
089860110405                    ADDEBITATI';
089870110405         PCLpx3b = 'INTERESSI AL TASSO DEL PRIME RATE MAGGIORATO +
089880110405                    DEL 2% A';
089890110405         PCLpx3c =
089900110405                   'DECORRERE DAL GIORNO SUCCESSIVO ALLA SCADENZA +
089910110405                    DELLA FATTURA';
089920110405
089930110405         PCLpx4a = 'Si prega di verificare la Vs. ragione sociale +
089940110405                    indirizzo e';
089950110405         PCLpx4b = 'partita IVA, dandoci eventuale sollecita +
089960110405                    rettifica scritta.';
089970110405
089980110405         PCLpx5a = '* I PRODOTTI IMPORTATI DAGLI U.S.A. SONO CON +
089990110405                    SPECIALE';
090000110405         PCLpx5b = 'LICENZA CHE PRECLUDE RIESPORTAZIONI NON +
090010110405                    AUTORIZZATE.';
090020110405
090030110405         PCLpx6  = 'LA SOCIETÀ SI IMPEGNA A VERSARE L''IVA NEI MODI +
090040110405                    E TERMINI PREVISTI (SOLIDARIETÀ ART. 60 BIS DPR +
090050110405                    633/72-FINANZIARIA 2005)';
090060110405
090070110405         // -?Salto Pagina?
090080110405         exsr  Test_OF;
090090110405
090100110405         // -?Re-Impostazione limiti di overflow?
090110110405         select;
090120110405           when  NOT *in45;
090130110405             wMaxLine = $MaxLine2;
090140110405           when  *in45   and   NOT *in46;
090150110405             wMaxLine = $MaxLine5_pl1;
090160110405           when  *in45   and   *in46;
090170110405             wMaxLine = $MaxLine5_pl2;
090180110405         endsl;
090190110405
090200110405         // -?Posizionamento iniziale all'interno del foglio A4?
090210110405         write  A4prtSP1;
090220110405
090230110405       ENDSR;
090240081002      /end-free
090250050214      *
090260050214      *---------------------------------------------------------------*
090270050217      *?Posizionamento di linee e box nel DDT ESPRINET               ?*
090280050214      *---------------------------------------------------------------*
090290050215     c     Posiz_ES      BEGSR
090300080708      *
090310081002      /free
090320081002         // Impostazione posizioni di logo/box
090330081002         // (posizioni di "partenza" diverse a seconda del posizionam.
090340081002         //  iniziale nel foglio A4 attualmente in stampa)
090350081002         select;
090360081002           when  NOT *in45;
090370081006             §PBA =  3.130;
090380081006             //§PBA =  3.255;        (più in basso di una riga)
090390081002           when  *in45   and   NOT *in46;
090400081002             §PBA = 0.255;
090410081002           when  *in45   and   *in46;
090420081002             §PBA = 6.005;
090430081002         endsl;
090440081002      /end-free
090450080708      * Impostazione posizioni di linee orizzontali
090460080708     c                   eval      §LOA  =  §PBA + 0.76
090470080708     c                   eval      §LOB  =  §LOA + 0.25
090480080708     c                   eval      §LOC  =  §LOB + 0.12
090490080708      * Impostazione posizioni di linee verticali
090500080708     c                   eval      §LVA  =  §LOA - 0.15
090510050214      *
090520050214      * Impostazione posizioni di box
090530050215if  1c                   if        Page  <= 1
090540050214      *   (posizione di "partenza" su 1ª pagina)
090550081002     c                   eval      §PB1  =  §PBA + 1.270
090560050214x   1c                   else
090570050214      *   (posizione di "partenza" su pagine successive)
090580081002     c                   eval      §PB1  =  §PBA + 0.020
090590050214e   1c                   endif
090600110405if  1c*//                if        %subst(AR8trk:1:1) = 'P'
090610110405if  1c                   if        %subst(SAVtrk:1:1) = 'P'
090620050331     c                             and    CurLineA4   < wMaxLine
090630081002     c                   eval      §PB2  =  §PBA + 5.360
090640050215x   1c                   else
090650050215     c                   eval(h)   §PB2  =  CurLineA4 / 8 + 0.05
090660081002     c                   if        §PB2  <  (§PBA + 5.320)
090670081002     c                   eval      §PB2  =  (§PBA + 5.320)
090680050331     c                   endif
090690050215e   1c                   endif
090700050215      * Impostazione posizioni di linee (orizzontali e verticali)
090710050214      *   nella parte superiore del box (testata)
090720050215     c                   eval      §LO1  =  §PB1 + 0.25
090730050214     c                   eval      §LO2  =  §LO1 + 0.11
090740050214      *   nella parte inferiore del box (piede)
090750110405if  1c*//                if        %subst(AR8trk:1:1) = 'P'
090760110405if  1c                   if        %subst(SAVtrk:1:1) = 'P'
090770050331     c                             and    CurLineA4   < wMaxLine
0907800810027.78 c                   eval      §LO3  =  §PBA + 3.900
0907900502158.00 c                   eval      §LO4  =  §LO3 + 0.24
0908000502158.39 c                   eval      §LO5  =  §LO4 + 0.36
0908100502158.52 c                   eval      §LO6  =  §LO5 + 0.14
0908200502148.64 c                   eval      §LO7  =  §LO6 + 0.12
0908300502148.77 c                   eval      §LO8  =  §LO7 + 0.13
0908400502148.89 c                   eval      §LO9  =  §LO8 + 0.12
0908500807088.97 c                   eval      §LO0  =  §LO9 + 0.10
090860050215e   1c                   endif
090870050214      * La lunghezza delle linee verticali separatrici nel dettaglio
090880050214      *   dipende dalla fase di stampa: se si sta stampando ancora il
090890050214      *   dettaglio deve essere più lunga, visto che non viene stampa-
090900050214      *   ta la parte finale del DDT...
090910110405if  1c*//                if        %subst(AR8trk:1:1) = 'P'
090920110405if  1c                   if        %subst(SAVtrk:1:1) = 'P'
090930050331     c                             and    CurLineA4   < wMaxLine
0909400502162.25 c                   eval      §LV1  =  §LO3 - §LO2
090950050215x   1c                   else
090960050216     c                   eval      §LV1  =  §PB2 - §LO2
090970050215e   1c                   endif
090980050214      *
090990050214      * Impostazione posizioni di campi di testo
091000050214      * - PCLtx1/tx2 (verticali)
0910100502179.10 c                   eval      §PR1  =  §PB2 - 0.07
091020050214      * - PCLpx2 ("Il presente documento...")
0910300502178.95 c                   eval      §PS2  =  §LO9 + 0.07
091040050214      * - PCLpx3a/px4a/px5a
0910500807089.04 c                   eval      §PS3  =  §LO0 + 0.07
091060050214      * - PCLpx3b/px4b/px5b
0910700502179.09 c                   eval      §PS4  =  §PS3 + 0.07
091080050214      * - PCLpx3c/px4c/px5c
0910900502179.14 c                   eval      §PS5  =  §PS4 + 0.07
091100050214      * - PCLpx6 ("Contributo ambientale CONAI...")
0911100502179.25 c                   eval      §PS6  =  §PB2 + 0.08
091120050214      *
091130050214     c                   ENDSR
091140041207      *
091150041207      *---------------------------------------------------------------*
091160050217      *?Stampa DDT del mittente (generico con logo)                  ?*
091170041207      *---------------------------------------------------------------*
091180041207     c     DDT_GenericoL BEGSR
091190041207      *
091200081003      /free
091210081003         // Impostazione del nuovo limite x overflow
091220081003         select;
091230081003           when  NOT *in45;
091240081003             wMaxLine = $MaxLine2;
091250081003           when  *in45   and   NOT *in46;
091260081003             wMaxLine = $MaxLine2_pl1;
091270081003           when  *in45   and   *in46;
091280081003             wMaxLine = $MaxLine2_pl2;
091290081003         endsl;
091300081003
091310081003         // Posizionamento iniziale all'interno del foglio A4
091320081003         write  A4prtSP1;
091330081006
091340081006         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
091350081006           write  A4prtNUL;
091360081006         endif;
091370081003      /end-free
091380041207      *
091390041207      * > logo
091400041207     c                   eval      §Logo  = 'LOGO' + AR8tmd
091410041207      *
091420081017     c                   if            §AR8cxd = *blanks
091430081017     c                             or  DB0fa4  = 'P'
091440080709     c                   write     A4prtDT0
091450080709     c                   endif
091460080709      *
091470041209      * Imposto posizione per logo
091480081003      /free
091490081003         select;
091500081003           when  NOT *in45;
091510081003             exsr  Rep_§LO2;
091520081003             §PB1 = §LO2 + (2 / 8);
091530081003           when  *in45   and   NOT *in46;
091540081003             §PB1 = 0.250;
091550081003           when  *in45   and   *in46;
091560081003             §PB1 = 6.000;
091570081003         endsl;
091580081003      /end-free
091590041209      *   Stampa del logo
091600041209     c                   write     A4prtDB0
091610041207      *
091620041207      * Reperimento dati e stampa DDT mittente - - - - - - - - - - - -*
091630041207      *
091640130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
091650130315      *  "StampaDDT")
091660041207     c***  KLDV          setll     FIAR8000       (già fatto)
091670041207     c***  KLDV          reade     FIAR8000       (già fatto)
091680041207      *
091690041207if  1c                   dow       NOT %eof(FIAR801L)
091700041207      *
091710041207if  2c                   if        AR8atb = *blanks
091720041207      *
091730041207      * > Stampa dei dati
091740041207     c                   exsr      Test_OF
091750041207sel 3c                   select
091760041207      * - Dati di dettaglio: testo in font a dimensione standard
091770041207w   3c                   when      AR8trk = 'C1'
091780051110     c                   movel(p)  AR8uni        PCddt
091790041207     c                   write     A4prtDC1
091800041207      * - Dati di testata:   testo in font a dimensioni massime
091810041207w   3c                   when      AR8trk = 'T1'
091820051110     c                   movel(p)  AR8uni        PCddt
091830041207     c                   write     A4prtDT1
091840041207      * - Dati di testata:   testo in font a dimensioni grandi
091850041207w   3c                   when      AR8trk = 'T2'
091860051110     c                   movel(p)  AR8uni        PCddt
091870041207     c                   write     A4prtDT2
091880041209      * - Linea separatrice
091890041209w   3c                   when      AR8trk = '--'
091900041209      *   - Stampa di una riga vuota per la riga separatrice
091910041209     c                   write     A4prtNUL
091920041209      *   - Stampa della linea orizzontale separatrice
091930090213     c                   eval(h)   §LO1   = (CurLineA4 / 8) - 0.050
091940041209     c                   write     A4prtLO
091950041207e   3c                   endsl
091960041207e   2c                   endif
091970041207      *
091980041207     c                   eval      SAVtrk = AR8trk
091990041207     c     KLDV          reade     FIAR8000
092000041207      *
092010041207e   1c                   enddo
092020080708      *
092030081003      /free
092040081003         select;
092050081003           when  NOT *in45;
092060081003             wMaxLine = $MaxLine2;
092070081003           when  *in45   and   NOT *in46;
092080081003             wMaxLine = $MaxLine2_pl1;
092090081003           when  *in45   and   *in46;
092100081003             wMaxLine = $MaxLine2_pl2;
092110081003         endsl;
092120081003      /end-free
092130041207      *
092140041207     c                   ENDSR
092150040607      *
092160040607      *---------------------------------------------------------------*
092170050217      *?Stampa DDT del mittente (generico senza logo)                ?*
092180040607      *---------------------------------------------------------------*
092190040607     c     DDT_Generico  BEGSR
092200040607      *
092210081003      /free
092220081003         // Impostazione del nuovo limite x overflow
092230081003         select;
092240081003           when  NOT *in45;
092250081003             wMaxLine = $MaxLine2;
092260081003           when  *in45   and   NOT *in46;
092270081003             wMaxLine = $MaxLine2_pl1;
092280081003           when  *in45   and   *in46;
092290081003             wMaxLine = $MaxLine2_pl2;
092300081003         endsl;
092310081003
092320081003         // Posizionamento iniziale all'interno del foglio A4
092330081003         write  A4prtSP1;
092340081006
092350081006         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
092360081006           write  A4prtNUL;
092370081006         endif;
092380081003      /end-free
092390040607      *
092400081017     c                   if            §AR8cxd = *blanks
092410081017     c                             or  DB0fa4  = 'P'
092420050331     c                   write     A4prtDT0
092430050331     c                   endif
092440040607      *
092450040607      * Reperimento dati e stampa DDT mittente - - - - - - - - - - - -*
092460040607      *
092470130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
092480130315      *  "StampaDDT")
092490040607     c***  KLDV          setll     FIAR8000       (già fatto)
092500040607     c***  KLDV          reade     FIAR8000       (già fatto)
092510040607      *
092520040607if  1c                   dow       NOT %eof(FIAR801L)
092530040607      *
092540040607if  2c                   if        AR8atb = *blanks
092550051110     c                   movel(p)  AR8uni        PCddt
092560040607     c                   exsr      Test_OF
092570040607sel 3c                   select
092580040607      * - Dati di dettaglio: testo in font a dimensione standard
092590040607w   3c                   when      AR8trk = 'C1'
092600040607     c                   write     A4prtDC1
092610040607      * - Dati di testata:   testo in font a dimensioni massime
092620040607w   3c                   when      AR8trk = 'T1'
092630040607     c                   write     A4prtDT1
092640040607      * - Dati di testata:   testo in font a dimensioni grandi
092650040607w   3c                   when      AR8trk = 'T2'
092660040607     c                   write     A4prtDT2
092670040607e   3c                   endsl
092680040607e   2c                   endif
092690040607      *
092700040607     c     KLDV          reade     FIAR8000
092710040607      *
092720040607e   1c                   enddo
092730080708      *
092740081003      /free
092750081003         select;
092760081003           when  NOT *in45;
092770081003             wMaxLine = $MaxLine2;
092780081003           when  *in45   and   NOT *in46;
092790081003             wMaxLine = $MaxLine2_pl1;
092800081003           when  *in45   and   *in46;
092810081003             wMaxLine = $MaxLine2_pl2;
092820081003         endsl;
092830081003      /end-free
092840040607      *
092850040607     c                   ENDSR
092860081017
092870081017      /free
092880081017       //--------------------------------------------------------------
092890140303       //?Impostazione nuova pagina di DDT Generico (con/senza logo)   ?
092900081017       //--------------------------------------------------------------
092910081017       BEGSR  NewPageSTD;
092920081017
092930081017         // Posizionamento iniziale nella nuova pagina
092940081017         //   (già fatto con A4prtSP0 se DB0FA4 = "P")
092950081017         if  DB0fa4 <> 'P';
092960081017           write  A4prtSP1;
092970081017           if  DB0fa4 = '1'   or   DB0fa4 = 'S';
092980081017             write  A4prtNUL;
092990081017           endif;
093000081017         endif;
093010081017
093020081017         // Stampa "COPIA PER IL DESTINATARIO"
093030081017         if  §AR8cxd = *blank   or   DB0fa4 = 'P';
093040081017           write  A4prtDT0;
093050081017         endif;
093060081017
093070081017         // Impostazione del nuovo limite x overflow
093080081017         select;
093090081017           when  NOT *in45;
093100081017             wMaxLine = $MaxLine2;
093110081017           when  *in45   and   NOT *in46;
093120081017             wMaxLine = $MaxLine2_pl1;
093130081017           when  *in45   and   *in46;
093140081017             wMaxLine = $MaxLine2_pl2;
093150081017         endsl;
093160081017
093170081017       ENDSR;
093180081017      /end-free
093190030515      *
093200030515      *---------------------------------------------------------------*
093210050217      *?Stampa del record "generico" (PRTGEN)                        ?*
093220030515      *---------------------------------------------------------------*
093230030515     c     StampaGEN     BEGSR
093240030515      *
093250030515      * Controllo se è stato raggiunto l'overflow nel formato in uso
093260030515     c                   exsr      Test_OF
093270030515      *
093280030911      * Stampo la riga PRTGEN
093290030911      * Se sto stampando una "testata" (*in39) salto una riga prima
093300030911      *   ed una riga dopo (per spaziare)
093310030911if  1c                   if        *in39 = *on
093320030515     c                   write     A4prtNUL
093330030911e   1c                   endif
093340030911     c                   write     A4prtGEN
093350030911if  1c                   if        *in39 = *on
093360030911     c                   write     A4prtNUL
093370030911e   1c                   endif
093380030515      *
093390030515     c                   ENDSR
093400030515      *
093410030515      *---------------------------------------------------------------*
093420050511      *?Prepara elenco per lista - SegnaColli Non Sequenziali (test.)?*
093430030515      *---------------------------------------------------------------*
093440030515     c     ListaScNS     BEGSR
093450030515      *
093460030515     c                   clear                   XX
093470030515      *
093480030515      * Nel foglio in formato "A5" devo solo riempire gli appositi
093490030715      *   campi "Note e Particolarità" (nessun salto riga/pagina è
093500030715      *   richiesto)
093510050513     c                   if           *in69       = *on
093520050513     c                             or $A4_x_ScNS  = *off
093530030515     c                   eval      WelSNS = $elSNSxA5
093540030515     c                   goto      ScNS_A5
093550030515     c                   endif
093560030515      *
093570030515      * Determino il numero di segnacolli da elencare per riga nel
093580030515      *   formato pagina "A4" (dove ci sono righe a parte).
093590030515      *   Dipende dal carattere specificato nel prtf: più grande è
093600030515      *   il carattere => meno segnacolli ci stanno sulla riga !
093610030911      *   A cpi 15 -come ora- ci sta tutta la schiera; a cpi 12 no !
093620030515     c                   eval      WelSNS = %elem($SC1)
093630030515      *
093640030515      * Spengo gli indicatori che condizionano in stampa le indicazioni
093650030515      *   per DPD / FedEx / Bancali-Supermercati-Autotrasportatori
093660030515      *   (se sono già state stampate nella 1ª pagina non devono più
093670030515      *   esserlo)
093680030910      *   (se non sono ancora state stampate, dovranno esserlo poi)
093690030515if  1c                   if        wPage > 1
093700050513     c                   movea     *in(70)       SAVin7x
093710040223     c                   movea     '0000'        *in(70)
093720030515e   1c                   endif
093730030515      *
093740030515      * Calcolo il numero di righe da saltare
093750030515     c                   eval      wMinLine = CurLineA4
093760030515if  1c                   if        §LO2     > §LO1
093770030515      * - se stampato almeno un box
093780030515     c                   eval(h)   wMinLine = (§LO2 * 8) + 2
093790030515x   1c                   else
093800030515      * - se non stampato alcun box
093810080730      /free
093820080729       // - Calcola la posizione della riga in 1/8" di pollice
093830080729       //   da quella attuale (in 1/9" di pollice)
093840080730         eval(h) wMinLine = (CurLineA4 * 8 / 9) + 1;
093850080730      /end-free
093860030515e   1c                   endif
093870030515      * ...e scarto il numero righe necessario
093880030515      *   (se non sono già da pagina nuova;
093890030515      *    se sono da pagina nuova: ci penserà poi la subr. "Test_OF"
093900030515      *       alla prima riga in stampa)
093910030515if  1c                   if        wMinLine  < wMaxLine
093920080729do  2c                   dou       CurLineA4 >= wMinLine
093930030515     c                   write     A4prtNUL
093940030515e   2c                   enddo
093950030515e   1c                   endif
093960030515      *
093970030515     c     ScNS_A5       tag
093980030515      *
093990030515      * Imposto l'indicatore per la stampa del titolo relativo all'
094000030515      *   elenco che segue (condiziona attributi di stampa diversi)
094010030515     c                   eval      *in39  = *on
094020050511      *
094030050511     c                   exsr      ListaScNSd
094040050511      *
094050050511     c                   ENDSR
094060050511      *
094070050511      *---------------------------------------------------------------*
094080050511      *?Prepara elenco per lista - SegnaColli Non Sequenziali (dett.)?*
094090050511      *---------------------------------------------------------------*
094100050511     c     ListaScNSd    BEGSR
094110030515      *
094120030515      * Stampo elenco dei segnacolli non sequenziali:
094130071008if  1c                   IF             DB0tbo  = 'P'
094140071008     c                             or   DB0tbo  = '8'
094150030515      * - per bolle in Partenza
094160030515     c                   exsr      ScNS_BLT
094170030515x   1c                   ELSE
094180081007      * - per bolle in Arrivo
094190030515     c                   exsr      ScNS_ART
094200030515e   1c                   ENDIF
094210030515      *
094220030515      * Alla fine: stampo eventuali segnacolli in elenco (non stampati)
094230030515      *              in foglio di formato "A4"
094240030515      *            imposto eventuali segnacolli in elenco (non messi in
094250030515      *              stampa tra le particolarità) in foglio "A5"
094260030515if  1c                   if        XX     > *zeros
094270030515     c                   eval      XX     = *hival
094280030515     c                   exsr      StampaScNS
094290030515e   1c                   endif
094300030910      *
094310030910      * Se sto stampando su formato pagina "A4": reimposto *in70, *in71
094320030910      *   e *in72 (spenti prima della stampa dell'elenco dei segnacolli
094330030910      *   non sequenziali) perchè saranno utilizzati per il posiziona-
094340030910      *   mento in nuova pagina (nella subr. "Test_OF") e per la stampa
094350040223      *   dei box per DPD, FedEx e quelli da FIAR5.
094360050513if  1c                   if             not *in69
094370030910     c                             and  not *in70
094380080922     c*** DPD non più in A4:       and  not *in71
094390030910     c                             and  not *in72
094400080912     c*** EEX non più gestita:     and  not *in73
094410030910     c                   movea     SAVin7x       *in(70)
094420030910e   1c                   endif
094430030515      *
094440030515     c                   ENDSR
094450030515      *
094460030515      *---------------------------------------------------------------*
094470050217      *?Prepara elenco SegnaColli Non Sequenziali da FNBLT (partenza)?*
094480030515      *---------------------------------------------------------------*
094490030515     c     ScNS_BLT      BEGSR
094500030515      *
094510030515if  1c                   if        not %open(FNBLT01L)
094520030515     c                   open      FNBLT01L
094530030515e   1c                   endif
094540030515      *
094550030515      * memorizzo l'ultimo numero di segnacollo
094560070514if  1c                   if        $Mamma = *off
094570040607     c     KLDV          setgt     FNBLT000
094580040607     c     KLDV          readpe    FNBLT000
094590070514x   1c                   else
094600070514     c     KLBL          setgt     FNBLT000
094610070514     c     KLBL          readpe    FNBLT000
094620070514e   1c                   endif
094630030515if  1c                   if        NOT %eof(FNBLT01L)
094640080922     c                   eval      %subst(Pnsx : 8) = ','
094650080922     c                                              + %editc(BLTnsc : 'Z')
094660030515e   1c                   endif
094670030515      *
094680030515      * memorizzo il primo numero di segnacollo
094690070514if  1c                   if        $Mamma = *off
094700040607     c     KLDV          setll     FNBLT000
094710040607     c     KLDV          reade     FNBLT000
094720070514x   1c                   else
094730070514     c     KLBL          setll     FNBLT000
094740070514     c     KLBL          reade     FNBLT000
094750070514e   1c                   endif
094760030515if  1c                   if        NOT %eof(FNBLT01L)
094770080922     c                   eval      %subst(Pnsx : 1 : 7) =
094780080922     c                                 %editc(BLTnsc : 'Z')
094790030515e   1c                   endif
094800030515      *
094810030515      * se ci sono solo 2 segnacolli non sequenziali:
094820030515      *   stanno già al posto del range - basta non stampare il msg
094830030515      *   ("NON SEQUENZIALI")
094840030515if  1c                   if        DB0ncl <= 2
094850050516     c                   eval      *in39  =  *off
094860030515     c                   goto      EndScNS_BLT
094870030515e   1c                   endif
094880030515      *
094890030515      * ciclo di lettura per il recupero di tutti i numeri di
094900030515      *   segnacollo
094910030515do  1c                   DOW       NOT %eof(FNBLT01L)
094920030515     c                   eval      Wnsc   =  BLTnsc
094930030515     c                   exsr      StampaScNS
094940070514if  2c                   if        $Mamma = *off
094950040607     c     KLDV          reade     FNBLT000
094960070514x   2c                   else
094970070514     c     KLBL          reade     FNBLT000
094980070514e   2c                   endif
094990030515e   1c                   ENDDO
095000030515      *
095010030515     c     EndScNS_BLT   ENDSR
095020030515      *
095030030515      *---------------------------------------------------------------*
095040050217      *?Prepara elenco SegnaColli Non Sequenziali da FNART (arrivo)  ?*
095050030515      *---------------------------------------------------------------*
095060030515     c     ScNS_ART      BEGSR
095070030515      *
095080070319if  1c                   if        not %open(FNART01L)
095090070319     c                   open      FNART01L
095100070319e   1c                   endif
095110030515      *
095120030515      * memorizzo l'ultimo numero di segnacollo
095130070514if  1c                   if        $Mamma = *off
095140040607     c     KLDV          setgt     FNART000
095150040607     c     KLDV          readpe    FNART000
095160070514x   1c                   else
095170070514     c     KLBL          setgt     FNART000
095180070514     c     KLBL          readpe    FNART000
095190070514e   1c                   endif
095200030515if  1c                   if        NOT %eof(FNART01L)
095210080922     c                   eval      %subst(Pnsx : 8) = ','
095220080922     c                                              + %editc(ARTnsc : 'Z')
095230030515e   1c                   endif
095240030515      *
095250030515      * memorizzo il primo numero di segnacollo
095260070514if  1c                   if        $Mamma = *off
095270040607     c     KLDV          setll     FNART000
095280040607     c     KLDV          reade     FNART000
095290070514x   1c                   else
095300070514     c     KLBL          setll     FNART000
095310070514     c     KLBL          reade     FNART000
095320070514e   1c                   endif
095330030515if  1c                   if        NOT %eof(FNART01L)
095340080922     c                   eval      %subst(Pnsx : 1 : 7) =
095350080922     c                                 %editc(ARTnsc : 'Z')
095360030515e   1c                   endif
095370030515      *
095380030515      * se ci sono solo 2 segnacolli non sequenziali:
095390030515      *   stanno già al posto del range - basta non stampare il msg
095400030515      *   ("NON SEQUENZIALI")
095410030515if  1c                   if        DB0ncl <= 2
095420050516     c                   eval      *in39  =  *off
095430030515     c                   goto      EndScNS_ART
095440030515e   1c                   endif
095450030515      *
095460030515      * ciclo di lettura per il recupero di tutti i numeri di
095470030515      *   segnacollo
095480030515do  1c                   DOW       NOT %eof(FNART01L)
095490030515     c                   eval      Wnsc   =  ARTnsc
095500030515     c                   exsr      StampaScNS
095510070514if  2c                   if        $Mamma = *off
095520040607     c     KLDV          reade     FNART000
095530070514x   2c                   else
095540070514     c     KLBL          reade     FNART000
095550070514e   2c                   endif
095560030515e   1c                   ENDDO
095570030515      *
095580030515     c     EndScNS_ART   ENDSR
095590030515      *
095600030515      *---------------------------------------------------------------*
095610050217      *?Stampa elenco SegnaColli Non Sequenziali in fogli "A4" o "A5"?*
095620030515      *---------------------------------------------------------------*
095630030515     c     StampaScNS    BEGSR
095640030515      *
095650030515      * In prima riga stampo la "intestazione"
095660030515if  1c                   if        *in39
095670050513if  2c                   if        *in69   or
095680050513     c                             $A4_x_ScNS = *off
095690050513      * - foglio "A5" oppure
095700050513      *   foglio "A4" in "NOTE E PARTICOLARITA'"
095710030515     c                   eval      YY      = YY  + 1
095720030515     c                   eval      $NP(yy) = C_ScNS + ':'
095730030515x   2c                   else
095740030515      * - foglio "A4"
095750030515     c                   eval      Prig  = C_ScNS
095760030515     c                                   + '         (dal '
095770080922     c                                   + %trim( %subst(Pnsx: 1: 7) )
095780030515     c                                   + ' al '
095790080922     c                                   + %trim( %subst(Pnsx: 9: 7) )
095800030515     c                                   + ')'
095810030515     c                   exsr      StampaGEN
095820030515e   2c                   endif
095830080922     c                   eval      Pnsx  =  'NON SEQUENZIALI'
095840030515     c                   eval      *in39 = *off
095850030515     c                   clear                   XX
095860030515     c                   clear                   $SC1
095870030515     c                   clear                   $SC2
095880030515e   1c                   endif
095890030515      *
095900030515      * Al riempimento della riga: stampo la parte di elenco segnacolli
095910030515if  1c                   if        XX  >= WelSNS
095920030515     c                   eval      $SC2 = %editc($SC1 : 'Z')
095930050513if  2c                   if        *in69   or
095940050513     c                             $A4_x_ScNS = *off
095950050513      * - foglio "A5"  oppure
095960050513      *   foglio "A4" in "NOTE E PARTICOLARITA'"
095970030515     c                   eval      YY      = YY  + 1
095980030515     c                   movea(p)  $SC2          $NP(yy)
095990030515x   2c                   else
096000030515      * - foglio "A4"
096010030911     c                   movea(p)  $SC2          Prig
096020030515     c                   exsr      StampaGEN
096030030515e   2c                   endif
096040030515if  2c                   if        XX  <  *hival
096050030515     c                   clear                   XX
096060030515e   2c                   endif
096070030515     c                   clear                   $SC1
096080030515     c                   clear                   $SC2
096090030515e   1c                   endif
096100030515      *
096110030515      * Aggiorno la nuova parte di elenco segnacolli
096120030515      *  (se non richiesta la stampa degli ultimi segnacolli non
096130030515      *   sequenziali residui - memorizzati ma non ancora stampati)
096140030515if  1c                   if        XX  <  *hival
096150030515     c                   eval      XX = xx  + 1
096160030515     c                   eval      $SC1(xx) = Wnsc
096170030515e   1c                   endif
096180030515      *
096190030515     c                   ENDSR
096200040607      *
096210040607      *---------------------------------------------------------------*
096220050217      *?Stampa della parte finale della Lettera di Vettura           ?*
096230040607      *---------------------------------------------------------------*
096240040607     c     ChiusuraLDV   BEGSR
096250080605      *
096260080605      * Posizionamento sul foglio A4 in base al n° pagina in stampa
096270130418     c                   eval      *in33 = *on
096280130418     c                   exsr      Posiz_Box
096290070307      *
096300070307      * Il tagliandino "Copia per il Destinatario" va ora stampato SE:
096310070307      * · spedizione NON in stato "lasciato avviso"   oppure
096320070307      * · spedizione in stato "lasciato avviso", ma P.A. con fattura
096330070307      *   immediata
096340040607      *
096350080703      * 2ª parte - copia per il destinatario (da lasciare al cliente)
096360040607      *   (Se NON è ristampa in arrivo per recapito c/assegni - vedi
096370040607      *   costante "Non utilizzare per la consegna")
096380130418      *                o per il mittente - se Porto Franco Prepagato
096390040607      * > "cornice" / barcode / righine varie in parte inferiore
096400040607      *
096410080604sel 1c                   select
096420080604w   1c                   when           $No_x_Cons         =  *off
096430070307     c                             and ($LascAvv           =  *off
096440070406     c                              or (%subst(DB0tb1:1:1) =  'A'
096450070406     c                              and DB0nft             >  *zeros))
096460130418     c                   write     A4prtLDV
096470130418     c                   write     A4prtBOX
096480110426     c*//                write     A4LogoBAR
096490110426      /free
096500120530         //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
096510120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
096520120530         //  // >?Simil-Logo Bartolini/Brt?
096530120530         //  write  A4LogoBR2;
096540120530         //  write  A4LogoDA2;
096550120530         //else;
096560120530         //  // >?Simil-Logo Bartolini?
096570120530         //  write  A4LogoBAR;
096580120530         //  write  A4LogoDA1;
096590120530         //endif;
096600120530         // >?Simil-Logo BRT?
096610120530         write  A4LogoBRT;
096620120530         write  A4LogoDA3;
096630110426      /end-free
096640080604w   1c                   when           $LascAvv           =  *on
096650070406     c                             and (%subst(DB0tb1:1:1) <> 'A'
096660070406     c                              or  DB0nft             =  *zeros)
096670070307     c                   eval      Pmsg3  =  Tla
096680070307     c                   write     A4prtLDVB
096690120113w   1c                   when      $No_x_Cons =  *on
096700120113w   1c*//                   when      $No_x_Cons =  '2'
096710120113     c*//                   eval      Pmsg3  =  ' NON UTILIZZARE PER'
096720120113     c*//                                    +  ' LASCIATO AVVISO -'
096730120113     c*//                                    +  ' SERVIZIO POSTE'
096740120113     c*//                   write     A4prtLDVB
096750080604e   1c                   endsl
096760040607      *
096770040607     c                   ENDSR
096780030515      *
096790030515      *---------------------------------------------------------------*
096800050217      *?Carico LDV da FNBLP00F                                       ?*
096810030515      *---------------------------------------------------------------*
096820030515     c     CarBLP        BEGSR
096830030515      *
096840030515      * Apertura files
096850030515     c                   if        not %open(FNBLP01L)
096860030515     c                   open      FNBLP01L
096870030515     c                   endif
096880030515     c                   if        not %open(FNARB01L)
096890030515     c                   open      FNARB01L
096900030515     c                   endif
096910070319     ***c v.sr sr_Start     if        not %open(FIAR401L)
096920070319     ***c " "     "         open      FIAR401L
096930070319     ***c " "     "         endif
096940030515     c                   if        not %open(FIAR601L)
096950030515     c                   open      FIAR601L
096960030515     c                   endif
096970030515     c                   if        not %open(FIAR701L)
096980030515     c                   open      FIAR701L
096990030515     c                   endif
097000060505     c                   if        not %open(FIAR901L)
097010060505     c                   open      FIAR901L
097020030515     c                   endif
097030071114      *
097040071114     c                   clear                   DB0cpi
097050030515      *
097060040607     c     KLDV          chain     FNBLP000
097070030515      *
097080030515if  1c                   if        NOT %found(FNBLP01L)
097090081117     c                   leavesr
097100030515e   1c                   endif
097110120509      *
097120120509      * Memorizzazione dei dati di FNBLP che serviranno anche dopo
097130120509      *   il reperimento dell'eventuale bolla mamma...
097140120509     c                   eval      wB0piv =  BLPcpi
097150030515      *
097160030515      * 1ª impostaz. campi della DS in base a quelli del file FNBLP
097170030515     c                   exsr      sr_DS_BLP
097180030515      *
097190070308      * Decodifico filiale di Arrivo
097200030515     c                   exsr      DecLNA
097210030515      *
097220030515      * 2ª impostaz. campi della DS in base a quelli delle tabelle
097230030515      *    "generiche" (3A, TB, 5E)
097240030515     c                   exsr      sr_DS_TAB
097250030515      *
097260030515      * Ultimi dati mancanti:
097270061030      * - Cod. cliente destinatario: imposto LNA9999
097280030515      *    SE   in Assegnato   O   c'è 2ª bolla
097290030515if  1c                   if           %subst(DB0tb1:1:1) = 'A'
097300030515     c                             or DB0tb2            <> *blanks
097310030515     c                   eval      DB0ccd = *all'9'
097320030515     c                   movel     BLPlna        DB0ccd
097330030515e   1c                   endif
097340120509      *
097350120509      * - Reperimento Data spedizione
097360120509if  1c                   if        DB0mgs <> *zeros
097370120509     c                   z-add     DB0mgs        Wdsp
097380120509     c                   movel     DB0aas        Wdsp
097390120509x   1c                   else
097400120509      *   prendo la data del giorno se non c'è la data spedizione
097410120509     c                   time                    w0140
097420120509     c                   move      w0140         w0080
097430120509     c     *eur          movel     w0080         Data_EUR
097440120509     c     *iso          movel     Data_EUR      Wdsp
097450120509e   1c                   endif
097460030515      *
097470061030      * 3ª impostaz. campi della DS: importi da calcolare
097480061031     c                   exsr      sr_DS_IMPO
097490030515      *
097500081117     c                   ENDSR
097510030515      *
097520030515      *---------------------------------------------------------------*
097530050217      *?Carico LDV da FNARB00F                                       ?*
097540030515      *---------------------------------------------------------------*
097550030515     c     CarARB        BEGSR
097560030515      *
097570030515      * Apertura files
097580030515     c                   if        not %open(FNARB01L)
097590030515     c                   open      FNARB01L
097600030515     c                   endif
097610070319     c                   if        NOT %open(FIARN11L)
097620070319     c                   open      FIARN11L
097630070319     c                   endif
097640030515     c                   if        not %open(FIAR601L)
097650030515     c                   open      FIAR601L
097660030515     c                   endif
097670030515     c                   if        not %open(FIAR701L)
097680030515     c                   open      FIAR701L
097690030515     c                   endif
097700060505     c                   if        not %open(FIAR901L)
097710060505     c                   open      FIAR901L
097720030515     c                   endif
097730071114      *
097740071114     c                   clear                   DB0cpi
097750030515      *
097760040607     c     KLDV          chain     FNARB01L
097770030515      *
097780030515if  1c                   if        NOT %found(FNARB01L)
097790070312     c                   clear                   ARBndc
097800070322     c                   clear                   ARBntc
097810070322     c                   clear                   ARBdcp
097820071015     c                   clear                   ARBpdc
097830101129     c                   clear                   ARBdcm
097840101129     c                   clear                   ARBicc
097850101129     c                   clear                   ARBica
097860120509     c                   clear                   ARBcpi
097870081117     c                   leavesr
097880030515e   1c                   endif
097890070514      *
097900070514      * Memorizzazione dei dati di FNARB che serviranno anche dopo
097910070514      *   il reperimento dell'eventuale bolla mamma...
097920070514     c                   eval      wB0ndc =  ARBndc
097930070514     c                   eval      wB0ntc =  ARBntc
097940070514     c                   eval      wB0dcp =  ARBdcp
097950081119     c                   eval      wB0tmc =  ARBtmc
097960071015     c                   move      ARBpdc        Wpdc4a
097970101125     c                   eval      wB0dcm =  ARBdcm
097980101125     c                   eval      wB0icc =  ARBicc
097990101125     c                   eval      wB0ica =  ARBica
098000120509     c                   eval      wB0piv =  ARBcpi
098010030515      *
098020030515      * 1ª impostaz. campi della DS in base a quelli del file FNARB
098030030515     c                   exsr      sr_DS_ARB
098040030515      *
098050070308      * Decodifico filiale di Arrivo
098060030515     c                   exsr      DecLNA
098070030515      *
098080030515      * 2ª impostaz. campi della DS in base a quelli delle tabelle
098090030515      *    "generiche" (3A, TB, 5E)
098100030515     c                   exsr      sr_DS_TAB
098110030515      *
098120030515      * Ultimi dati mancanti:
098130030515      * - Cod. cliente mittente
098140030515if  1c                   If        ARBccm <> *zeros
098150030515     c                   movel     ARBccm        DB0ccm
098160030515x   1c                   Else
098170030515if  2c                   if        %subst(DB0tb1:1:1) = 'F'
098180030515     c                   movel     ARBksc        DB0ccm
098190030515e   2c                   endif
098200030515e   1c                   EndIf
098210030515      * - Cod. cliente destinatario
098220030515if  2c                   if        %subst(DB0tb1:1:1) = 'A'
098230030515     c                   movel     ARBksc        DB0ccd
098240030515e   1c                   endif
098250030515      * - Se c'è 2ª bolla imposto LNA9999
098260030515if  1c                   if        DB0tb2 <> *blanks
098270030515     c                   eval      DB0ccd = *all'9'
098280030515     c                   movel     ARBlna        DB0ccd
098290030515e   1c                   endif
098300120509      * - Reperimento Data spedizione
098310120509if  1c                   if        DB0mgs <> *zeros
098320120509     c                   z-add     DB0mgs        Wdsp
098330120509     c                   movel     DB0aas        Wdsp
098340120509x   1c                   else
098350120509      *   prendo la data del giorno se non c'è la data spedizione
098360120509     c                   time                    w0140
098370120509     c                   move      w0140         w0080
098380120509     c     *eur          movel     w0080         Data_EUR
098390120509     c     *iso          movel     Data_EUR      Wdsp
098400120509e   1c                   endif
098410030515      *
098420061030      * 3ª impostaz. campi della DS: importi da calcolare
098430061031     c                   exsr      sr_DS_IMPO
098440030515      *
098450081117     c                   ENDSR
098460030515      *
098470030515      *---------------------------------------------------------------*
098480050217      *?1ª impostaz. campi della DS in base a quelli del file FNBLP  ?*
098490030515      *---------------------------------------------------------------*
098500030515     c     sr_DS_BLP     BEGSR
098510030515      *
098520030515     c                   eval      DB0cbo = BLPcbo
098530030515     c                   eval      DB0tsp = BLPtsp
098540030515     c                   eval      DB0mgs = BLPmgs
098550030515     c                   eval      DB0tc1 = BLPtc1
098560030515     c                   eval      DB0tc2 = BLPtc2
098570030515     c                   eval      DB0fns = BLPfns
098580030515     c                   eval      DB0rmn = BLPrmn
098590030515     c                   eval      DB0rma = BLPrma
098600030515     c                   eval      DB0ias = BLPias
098610030515     c                   eval      DB0vas = BLPvas
098620030515     c                   eval      DB0vmd = BLPvmd
098630030515     c                   eval      DB0vad = BLPvad
098640030515     c                   eval      DB0qft = BLPqft
098650030515     c                   eval      DB0lna = BLPlna
098660030515     c                   eval      DB0znc = BLPznc
098670030515     c                   eval      DB0rsm = BLPrsm
098680030515     c                   eval      DB0inm = BLPinm
098690030515     c                   eval      DB0cam = BLPcam
098700030515     c                   eval      DB0lom = BLPlom
098710030515     c                   eval      DB0prm = BLPprm
098720030515     c                   eval      DB0nzm = BLPnzm
098730030515     c                   eval      DB0fap = BLPfap
098740030515     c                   eval      DB0rmo = BLPrmo
098750030515     c                   eval      DB0rsd = BLPrsd
098760030515     c                   eval      DB0ind = BLPind
098770030515     c                   eval      DB0cad = BLPcad
098780030515     c                   eval      DB0lod = BLPlod
098790030515     c                   eval      DB0prd = BLPprd
098800030515     c                   eval      DB0nzd = BLPnzd
098810030515     c                   eval      DB0fin = BLPfin
098820030515     c                   eval      DB0gc1 = BLPgc1
098830030515     c                   eval      DB0gc2 = BLPgc2
098840030515     c                   eval      DB0nas = BLPnas
098850030515     c                   eval      DB0ncl = BLPncl
098860030515     c                   eval      DB0pkf = BLPpkf
098870030515     c                   eval      DB0fvf = BLPfvf
098880030515     c                   eval      DB0vlf = BLPvlf
098890030515     c                   eval      DB0fls = BLPfls
098900030515     c                   eval      DB0ncd = BLPncd
098910030515     c                   eval      DB0nca = BLPnca
098920030515     c                   eval      DB0xco = BLPxco
098930030515     c                   eval      DB0ffd = BLPffd
098940030515     c                   eval      DB0dcr = BLPdcr
098950030515     c                   eval      DB0hcr = BLPhcr
098960030515     c                   eval      DB0tcr = BLPtcr
098970030515     c                   eval      DB0fbr = BLPfbr
098980030515     c                   eval      DB0gga = BLPgga
098990030515     c                   eval      DB0gma = BLPgma
099000030515     c                   eval      DB0gva = BLPgva
099010030515     c                   move      ' '           DB0gva
099020030515     c                   eval      DB0sop = BLPsop
099030030515     c                   eval      DB0ctr = BLPctr
099040030515     c                   eval      DB0drt = BLPdrt
099050030515     c                   eval      DB0fpp = BLPfpp
099060030515     c                   eval      DB0nrt = BLPnrt
099070030515     c                   eval      DB0pdr = BLPpdr
099080030515      *
099090030515      * Cod. cliente mittente
099100040311     c                   move      BLPksc        w0040
099110030515if  1c                   IF        BLPccm <> *zeros
099120030515     c                   movel     BLPccm        DB0ccm
099130030515x   1c                   ELSE
099140030515      *   Lo imposto solo se non è codice generico destinatario
099150040311if  2c                   if        w0040  <> 9999
099160030515     c                   movel     BLPksc        DB0ccm
099170030515x   2c                   else
099180030515     c                   movel     BLPksc        DB0ccd
099190030515     c                   movel     BLPlna        DB0ccd
099200030515e   2c                   endif
099210030515e   1c                   ENDIF
099220030515      *
099230030515      * Allega documento di trasporto
099240081119     c                   clear                   DB0bam
099250060505if  1c                   if           BLPfst = 'S'
099260030515     c                             or BLPfst = 'C'
099270030515     c                   movel     'S'           DB0bam
099280060505e   1c                   endif
099290060524      *
099300060524      * Se bolla di RESO: Codice cliente da Assegnato
099310061107     c*** (già fatto:)   clear                   dsBL4A
099320060524if  1c                   if        DB0fbr = 'S'
099330060524     c                   exsr      RepAR4_A
099340060524e   1c                   endif
099350030515      *
099360030515      * Destinatario: vedo se esiste 2ª ragione sociale
099370061107     c                   clear                   DB0rd2
099380061107     c                   exsr      RepAR4_D
099390030515      *
099400030515      * Note
099410061107     c                   clear                   DB0nt1
099420061107     c                   clear                   DB0nt2
099430061107     c                   exsr      RepAR4_89
099440030515      *
099450030515     c                   ENDSR
099460030515      *
099470030515      *---------------------------------------------------------------*
099480050217      *?1ª impostaz. campi della DS in base a quelli del file FNARB  ?*
099490030515      *---------------------------------------------------------------*
099500030515     c     sr_DS_ARB     BEGSR
099510030515      *
099520030515     c                   eval      DB0cbo = ARBcbo
099530030515     c                   eval      DB0tsp = ARBtsp
099540030515     c                   eval      DB0mgs = ARBmgs
099550030515     c                   eval      DB0tc1 = ARBtc1
099560030515     c                   eval      DB0tc2 = ARBtc2
099570030515     c                   eval      DB0fns = ARBfns
099580030515     c                   eval      DB0rmn = ARBrmn
099590030515     c                   eval      DB0rma = ARBrma
099600030515     c                   eval      DB0ias = ARBias
099610030515     c                   eval      DB0vas = ARBvas
099620030515     c                   eval      DB0vmd = ARBvmd
099630030515     c                   eval      DB0vad = ARBvad
099640030515     c                   eval      DB0qft = ARBqft
099650030515     c                   eval      DB0lna = ARBlna
099660030515     c                   eval      DB0znc = ARBznc
099670030515     c                   eval      DB0rsm = ARBrsm
099680030515     c                   eval      DB0inm = ARBinm
099690030515     c                   eval      DB0cam = ARBcam
099700030515     c                   eval      DB0lom = ARBlom
099710030515     c                   eval      DB0prm = ARBprm
099720030515     c                   eval      DB0nzm = ARBnzm
099730030515     c                   eval      DB0fap = ARBfap
099740030515     c                   eval      DB0rmo = ARBrmo
099750030515     c                   eval      DB0rsd = ARBrsd
099760030515     c                   eval      DB0ind = ARBind
099770030515     c                   eval      DB0cad = ARBcad
099780030515     c                   eval      DB0lod = ARBlod
099790030515     c                   eval      DB0prd = ARBprd
099800030515     c                   eval      DB0nzd = ARBnzd
099810030515     c                   eval      DB0fin = ARBfin
099820030515     c                   eval      DB0gc1 = ARBgc1
099830030515     c                   eval      DB0gc2 = ARBgc2
099840030515     c                   eval      DB0nas = ARBnas
099850030515     c                   eval      DB0ncl = ARBncl
099860030515     c                   eval      DB0pkf = ARBpkf
099870030515     c                   eval      DB0fvf = ARBfvf
099880030515     c                   eval      DB0vlf = ARBvlf
099890030515     c                   eval      DB0fls = ARBfls
099900030515     c                   eval      DB0ncd = ARBncd
099910030515     c                   eval      DB0nca = ARBnca
099920030515     c                   eval      DB0xco = ARBxco
099930030515     c                   eval      DB0ffd = ARBffd
099940030515     c                   eval      DB0dcr = ARBdcr
099950030515     c                   eval      DB0hcr = ARBhcr
099960030515     c                   eval      DB0tcr = ARBtcr
099970030515     c                   eval      DB0fbr = ARBfbr
099980030515     c                   eval      DB0gga = ARBgga
099990030515     c                   eval      DB0gma = ARBgma
100000030515     c                   eval      DB0gva = ARBgva
100010030515     c                   move      ' '           DB0gva
100020030515     c                   clear                   DB0sop
100030030515     c                   eval      DB0ctr = ARBctr
100040030515     c                   clear                   DB0drt
100050030515     c                   clear                   DB0fpp
100060030515     c                   clear                   DB0nrt
100070030515     c                   clear                   DB0pdr
100080030515      *
100090030515      * Allega documento di trasporto
100100060524      * & Codice cliente da Assegnato
100110061107     c*** (già fatto:)   clear                   dsBL4A
100120031029     c                   clear                   DB0bam
100130061107     c                   exsr      RepAR4_A
100140060505     c                   if        §B4Abm = 'S' or §B4Abm = 'C' or
100150031029     c                             §B4Abm = 'P' or §B4Abm = 'Q'
100160030515     c                   movel     'S'           DB0bam
100170060505     c                   endif
100180030515      *
100190030515      * Destinatario: vedo se esiste 2ª ragione sociale
100200061107     c                   clear                   DB0rd2
100210061107     c                   exsr      RepAR4_D
100220030515      *
100230030515      * Note
100240061107     c                   clear                   DB0nt1
100250061107     c                   clear                   DB0nt2
100260061107     c                   exsr      RepAR4_89
100270030515      *
100280030515     c                   ENDSR
100290030515      *
100300030515      *---------------------------------------------------------------*
100310050217      *?2ª impostaz. campi della DS in base a quelli delle tabelle   ?*
100320050217      *?   "generiche" (non da "1A")                                 ?*
100330030515      *---------------------------------------------------------------*
100340030515     c     sr_DS_TAB     BEGSR
100350030515      *
100360030515      * Carico le tabelle "generiche" (3A, TB, 5E)
100370030515     c                   exsr      CarTAB
100380030515      *
100390030515      * Codici bolla
100400030515     c                   eval      DB0tb1 = §3Atb1
100410030515     c                   eval      DB0tb2 = §3Atb2
100420030515     c                   eval      DB0dcb = §3Ades
100430030515      * Tipo servizio
100440030515if  1c                   if        DB0dts = *blanks
100450030515     c                   eval      DB0dts = §5Edes
100460030515     c                   eval      DB0fst = §5Efst
100470030515e   1c                   endif
100480030515      * Consegne particolari
100490030515if  1c                   if            DB0tc1 <> *blanks
100500030515     c                             and DB0dc1 =  *blanks
100510030515     c                   movel(p)  DB0tc1        TBLkey
100520030515     c                   exsr      Tab_1P
100530030515     c                   eval      DB0dc1 = §1Pdes
100540030515e   1c                   endif
100550030515if  1c                   if            DB0tc2 <> *blanks
100560030515     c                             and DB0dc2 =  *blanks
100570030515     c                   movel(p)  DB0tc2        TBLkey
100580030515     c                   exsr      Tab_1P
100590030515     c                   eval      DB0dc2 = §1Pdes
100600030515e   1c                   endif
100610030515      * Esenzione IVA
100620030515if  1c                   if            DB0cei <> *blanks
100630030515     c                             and DB0dce =  *blanks
100640030515     c                   eval      DB0dce =  §EIdes
100650030515e   1c                   endif
100660030515      *
100670030515     c                   ENDSR
100680060524      *
100690060524      *---------------------------------------------------------------*
100700060524      *?Reperimento dati da FIAR400F x trk "A"                       ?*
100710061107      *?(dati aggiuntivi per P.Assegnato)                            ?*
100720060524      *---------------------------------------------------------------*
100730060524     c     RepAR4_A      BEGSR
100740060524      *
100750061107     c*** (già fatto:)   clear                   dsBL4A
100760061107      *
100770060524     c                   eval      Wtrc = 'A'
100780060524     c     KLDVtrc       chain     FIAR4000
100790060524      *
100800060524if  1c                   if        %found(FIAR401L)
100810060524     c                   movel     AR4Not        dsBL4A
100820060524e   1c                   endif
100830060524      *
100840060524     c                   ENDSR
100850061107      *
100860061107      *---------------------------------------------------------------*
100870061107      *?Reperimento dati da FIAR400F x trk "D"                       ?*
100880061107      *?(2ª ragione sociale del destinatario)                        ?*
100890061107      *---------------------------------------------------------------*
100900061107     c     RepAR4_D      BEGSR
100910061107      *
100920061107     c*** NON esiste:    clear                   dsBL4D
100930061107     c*** (già fatto:)   clear                   DB0rd2
100940061107      *
100950061107     c                   eval      Wtrc = 'D'
100960061107     c     KLDVtrc       chain     FIAR4000
100970061107      *
100980061107if  1c                   if        %found(FIAR401L)
100990061107     c                   movel     AR4not        DB0rd2
101000061107e   1c                   endif
101010061107      *
101020061107     c                   ENDSR
101030061107      *
101040060626      *---------------------------------------------------------------*
101050060626      *?Reperimento dati da FIAR400F x trk "I"                       ?*
101060061107      *?(dati DPD)                                                   ?*
101070060626      *---------------------------------------------------------------*
101080061030     c     RepAR4_I      BEGSR
101090061030      *
101100061107     c*** (già fatto:)   clear                   dsBL4I
101110061107      *
101120061030     c                   eval      Wtrc = 'I'
101130061030     c     KLDVtrc       chain     FIAR4000
101140061030      *
101150061030if  1c                   if        %found(FIAR401L)
101160061030     c                   movel     AR4not        dsBL4I
101170060626e   1c                   endif
101180061030      *
101190061030     c                   ENDSR
101200071008      *
101210071015      *---------------------------------------------------------------*
101220071015      *?Reperimento dati da FIAR400F x trk "M"                       ?*
101230071015      *?(dati aggiuntivi per O.R.M. abbinato al P.Franco PrePagato)  ?*
101240071015      *---------------------------------------------------------------*
101250071015     c     RepAR4_M      BEGSR
101260071015      *
101270071015     c*** (già fatto:)   clear                   dsBL4M
101280071015      *
101290071015     c                   eval      Wtrc = 'M'
101300071015     c     KLDVtrc       chain     FIAR4000
101310071015      *
101320071015if  1c                   if        %found(FIAR401L)
101330071015     c                   movel     AR4Not        dsBL4M
101340071015e   1c                   endif
101350071015      *
101360071015     c                   ENDSR
101370061107      *
101380061107      *---------------------------------------------------------------*
101390061107      *?Reperimento dati da FIAR400F x trk "Q"                       ?*
101400061107      *?(Codice Fiscale del mittente a sx & del destinatario a dx)   ?*
101410061107      *---------------------------------------------------------------*
101420061107     c     RepAR4_Q      BEGSR
101430061107      *
101440061108     c*** (già fatto:)   clear                   dTA4Q
101450061107     c*** (già fatto:)   clear                   DB0cpi
101460061107      *
101470061107     c                   eval      Wtrc = 'Q'
101480061107     c     KLDVtrc       chain     FIAR4000
101490061107      *
101500061107if  1c                   if        %found(FIAR401L)
101510061108     c                   movel     AR4not        dTA4Q
101520061108e   1c                   endif
101530061107      *
101540061107     c                   ENDSR
101550061107      *
101560061107      *---------------------------------------------------------------*
101570061107      *?Reperimento dati da FIAR400F x trk "8" / "9"                 ?*
101580061107      *?(note)                                                       ?*
101590061107      *---------------------------------------------------------------*
101600061107     c     RepAR4_89     BEGSR
101610061107      *
101620061107     c*** NON esiste:    clear                   dsBL48
101630061107     c*** NON usata:     clear                   dsBL49
101640061107     c*** (già fatto:)   clear                   DB0nt1
101650061107     c*** (già fatto:)   clear                   DB0nt2
101660061107      *
101670061107     c                   eval      Wtrc = '8'
101680061107     c     KLDVtrc       chain     FIAR4000
101690061107if  1c                   if        %found(FIAR401L)
101700061107     c                   movel     AR4not        DB0nt1
101710061107e   1c                   endif
101720061107      *
101730061107     c                   eval      Wtrc = '9'
101740061107     c     KLDVtrc       chain     FIAR4000
101750061107if  1c                   if        %found(FIAR401L)
101760061107     c                   movel     AR4not        DB0nt2
101770061107e   1c                   endif
101780061107      *
101790061107     c                   ENDSR
101800030515      *
101810030515      *---------------------------------------------------------------*
101820050217      *?Carico le tabelle che servono: 3A/TB/5E.                     ?*
101830030515      *---------------------------------------------------------------*
101840030515     c     CarTAB        BEGSR
101850030515      *
101860030515      * Tab. "3A" - Codici Bolla
101870030515      * (NO se rimesso mittente)
101880030515     c                   clear                   ds3A
101890030515if  1c                   if        DB0ris <> 'M'
101900030515     c                   movel     '3A'          TBLcod
101910030515     c                   movel(p)  DB0cbo        TBLkey
101920030515     c     K03TAB        chain     TABEL
101930030515if  2c                   if        %found(TABEL00F)
101940030515     c                   movel     TBLuni        ds3A
101950030515e   2c                   endif
101960030515e   1c                   endif
101970070515     c                   eval      $Mamma =  (§3Abcm = 'S')
101980030515      *
101990030515      * Tab. "TB" - Tipi Bolla
102000030515      * (NO se rimesso mittente)
102010030515      * se Bolla in assegnato, con tipo bolla vedo se da contabilizzare
102020030515      * se no non stampo i dati fattura
102030030515     c                   clear                   dsTB
102040030515if  1c                   if        DB0ris <> 'M'
102050030515     c                   movel     'TB'          TBLcod
102060061030if  2c                   if            §3Atb2 <> *blanks
102070121029     c                             and DB0tbo <> 'P'
102080030515     c                             and DB0tbo <> '8'
102090030515     c                   movel(p)  §3Atb2        TBLkey
102100030515x   2c                   else
102110030515     c                   movel(p)  §3Atb1        TBLkey
102120030515e   2c                   endif
102130030515     c     K03TAB        chain     TABEL
102140030515if  2c                   if        %found(TABEL00F)
102150030515     c                   movel     TBLuni        dsTB
102160030515e   2c                   endif
102170030515e   1c                   endif
102180030515      *
102190030515      * Tab. "5E" - Tipi Servizio
102200030515     c                   clear                   ds5E
102210090513if  1c                   if        DB0tsp <> *blanks
102220030515     c                   movel     '5E'          TBLcod
102230030515     c                   movel(p)  DB0tsp        TBLkey
102240030515     c     K03TAB        chain     TABEL
102250030515if  2c                   if        %found(TABEL00F)
102260030515     c                   movel     TBLuni        ds5E
102270030515e   2c                   endif
102280030515e   1c                   endif
102290030515      *
102300030911      * Tab. "EI" - Esenzioni I.V.A.
102310030515     c                   clear                   dsEI
102320030911      * Tab. "1A" - Tipi incasso contrassegno
102330030911     c                   clear                   ds1A
102340030911      * Tab. "1P" - Consegne particolari
102350030911     c                   clear                   ds1P
102360150226      *//* Tab. "TBA" - Tipi bancale
102370150226     c*//                clear                   dTBA
102380030515      *
102390030515     c                   ENDSR
102400030911      *
102410030911      *---------------------------------------------------------------*
102420050217      *?Carico tabella "EI" (Esenzione I.V.A.)                       ?*
102430030911      *---------------------------------------------------------------*
102440030911     c     Tab_EI        BEGSR
102450030911      *
102460030911     c                   clear                   dsEI
102470030911if  1c                   if        DB0cei <> *blanks  and  DB0dce = *blanks
102480030911     c                   movel     'EI'          TBLcod
102490030911     c                   movel(p)  DB0cei        TBLkey
102500030911     c     K03TAB        chain     TABEL
102510030911if  2c                   if        %found(TABEL00F)
102520030911     c                   movel     TBLuni        dsEI
102530030911e   2c                   endif
102540030911e   1c                   endif
102550030911     c                   eval      DB0dce =  §EIdes
102560030911      *
102570030911     c                   ENDSR
102580030515      *
102590030515      *---------------------------------------------------------------*
102600050217      *?Carico tabella "1A" (Tipo incasso contrassegno)              ?*
102610030515      *---------------------------------------------------------------*
102620030515     c     Tab_1A        BEGSR
102630030515      *
102640030515     c                   clear                   ds1A
102650030515if  1c                   if        DB0cas <> *zeros
102660030515     c                   movel     '1A'          TBLcod
102670030515     c                   movel(p)  DB0tic        TBLkey
102680030515     c     K03TAB        chain     TABEL
102690030515if  2c                   if        %found(TABEL00F)
102700030515     c                   movel     TBLuni        ds1A
102710030515e   2c                   endif
102720030515e   1c                   endif
102730030515      *
102740030515      * Imposto i dati nella DS
102750030515     c                   eval      DB0dti = §1Ades
102760030515     c                   eval      DB0ucm = §1Aucm
102770030515     c                   eval      DB0ste = §1Aste
102780030515      *
102790030515     c                   ENDSR
102800030515      *
102810030515      *---------------------------------------------------------------*
102820050217      *?Carico tabella "1P" (Consegne particolari)                   ?*
102830030515      *---------------------------------------------------------------*
102840030515     c     Tab_1P        BEGSR
102850030515      *
102860030515     c                   clear                   ds1P
102870030515     c                   movel     '1P'          TBLcod
102880030515     c*** già impostato: movel(p)  DB0tc1/2      TBLkey
102890030515     c     K03TAB        chain     TABEL
102900030515if  1c                   if        %found(TABEL00F)
102910030515     c                   movel     TBLuni        ds1P
102920030515e   1c                   endif
102930030515      *
102940030515     c                   ENDSR
102950150226      *
102960150226      *//*---------------------------------------------------------------*
102970150226      *//*?Reperisco tabella "TBA" (Tipi Bancale)                       ?*
102980150226      *//*---------------------------------------------------------------*
102990150226     c*//  Tab_TBA       BEGSR
103000150226      *//*
103010150226     c*//                clear                   dTBA
103020150226     c*//                reset                   TIBS02ds
103030150226     c*//                eval      T02cod = 'TBA'
103040150226     c*//                movel(p)  Wtba          T02ke1
103050150226     c*//                call      'TIBS02R'
103060150226     c*//                parm                    KPJBA
103070150226     c*//                parm                    TIBS02ds
103080150226if  1c*//                if        T02err = *blanks
103090150226     c*//                movel     T02uni        dTBA
103100150226e   1c*//                endif
103110150226      *//*
103120150226     c*//                ENDSR
103130030515      *
103140030515      *---------------------------------------------------------------*
103150050217      *?Calcolo importi LDV                                          ?*
103160030515      *---------------------------------------------------------------*
103170061031     c     sr_DS_IMPO    BEGSR
103180030515      *
103190030515      * Vedo se esiste tassazione
103200030515      *  solo se tipo bolla contabilizzabile
103210061030if  1c                   IF        §TBfcb =  '1'
103220030515     c
103230061030     c                   eval      Wtrc   =  '1'
103240040607     c     KLDVtrc       chain     FIAR6000
103250030515     c
103260030515if  2c                   if        %found(FIAR601L)
103270030515     c                   movel     AR6div        DB0div
103280030515     c                   exsr      CtrDIV
103290030515      *
103300030515      * NON c'è 2ª bolla
103310061031if  3c                   if            DB0tb2 = *blanks
103320121029     c                             or  DB0tbo = 'P'
103330061031     c                             or  DB0tbo = '8'
103340030515      *  Estremi fattura
103350030515     c                   z-add     AR6nft        DB0nft
103360030515     c                   z-add     AR6dft        DB0dft
103370130124     c                   z-add     AR6fiv        wB0fiv
103380061031      *  Se è assegnato o franco prepagato => stampo tutta la tassaz.
103390061031if  4c                   if            %subst(DB0tb1:1:1) = 'A'
103400061031     c                             or  DB0tbo = '8'
103410061030     c                   eval      Wtrc   =  '1'
103420030515     c                   exsr      RepTASSAZ
103430030515e   4c                   endif
103440030515e   3c                   endif
103450030515e   2c                   endif
103460030515      *
103470061030      * C'è 2ª bolla
103480061031if  2c                   IF            DB0tb2 <> *blanks
103490121029     c                             and DB0tbo <> 'P'
103500061031     c                             and DB0tbo <> '8'
103510030515      *
103520061030     c                   eval      Wtrc   =  '2'
103530040607     c     KLDVtrc       chain     FIAR6000
103540030515      *
103550061030if  3c                   If        %found(FIAR601L)
103560030515     c                   movel     AR6div        DB0div
103570030515     c                   exsr      CtrDIV
103580030515     c                   movel     AR6cei        DB0cei
103590030515      *   Estremi fattura
103600030515     c                   z-add     AR6nft        DB0nft
103610030515     c                   z-add     AR6dft        DB0dft
103620130124     c                   z-add     AR6fiv        wB0fiv
103630030515if  4c                   if        AR6ksc <> *zeros
103640030515     c                   eval      DB0ccd =  AR6ksc
103650030515e   4c                   endif
103660061030     c                   eval      Wtrc   =  '2'
103670030515     c                   exsr      RepTASSAZ
103680061030e   3c                   EndIf
103690030515      *
103700030515e   2c                   ENDIF
103710030515      *
103720030515e   1c                   ENDIF
103730030515      *
103740030515      * Vedo se ha contrassegno
103750060505     c     KLDV          chain     FIAR9000
103760060505if  1c                   IF        %found(FIAR901L)
103770061030     c                   eval      DB0cas =  AR9cas
103780061030     c                   eval      DB0vca =  AR9vca
103790061030     c                   eval      DB0gca =  AR9gca
103800061030     c                   eval      DB0tic =  AR9tic
103810030515x   1c                   ELSE
103820030515     c                   clear                   DB0cas
103830030515     c                   clear                   DB0vca
103840030515     c                   clear                   DB0gca
103850030515     c                   clear                   DB0tic
103860030515     c                   clear                   DB0dti
103870030515     c                   clear                   DB0ucm
103880030515     c                   clear                   DB0ste
103890030515e   1c                   ENDIF
103900120509      *
103910120509      * Se P.F. Prepagato verificherò il Codice Fiscale del Mittente
103920120509      *   (NON se mittente estero)
103930120509if  1c                   if           ( DB0tbo  = '8'
103940120509     c                             and (DB0nft  <> *zeros
103950120509     c                              or  DB0ift  <> *zeros)
103960120509     c                             and  DB0nzm  =  *blanks )
103970120509     c                   exsr      RepAR4_Q
103980120509     c                   movel     §TA4Qcfm      DB0cpi
103990120509e   1c                   endif
104000121029      * Se P.A. con fattura immediata o con 2ª bolla verificherò il
104010121029      *   Codice Fiscale (NON se destinatario estero)
104020121026if  1c*//                if           ( DB0tbo  = 'A'
104030121029if  1c                   if          ( (%subst(DB0tb1:1:1) = 'A'
104040121029     c                             or  (DB0tb2  <> *blanks
104050121029     c                             and  DB0tbo  <> 'P'
104060121029     c                             and  DB0tbo  <> '8') )
104070120509     c                             and (DB0nft  <> *zeros
104080120509     c                              or  DB0ift  <> *zeros)
104090120509     c                             and  DB0nzd  =  *blanks )
104100120509     c                   exsr      RepAR4_Q
104110120509     c                   movel     §TA4Qcfd      DB0cpi
104120120509e   1c                   endif
104130120509     c                   eval      wB0cfi = DB0cpi
104140061030      *
104150030515      * Vedo se si tratta di bolla figlia:
104160040607      * Se bolla con MAMMA prendo c/a e segnacolli da bolla MAMMA
104170070515if  1c                   if        $Mamma =  *on
104180081117     c                   exsr      BollaFiglia
104190030515e   1c                   endif
104200030515      *
104210030515     c                   ENDSR
104220030515      *
104230030515      *---------------------------------------------------------------*
104240050217      *?Decodifica LNA                                               ?*
104250030515      *---------------------------------------------------------------*
104260030515     c     DecLNA        BEGSR
104270030515      *
104280140206if  1c*//                if            DB0dla =  *blanks
104290140206     c*//                          and DB0ris <> 'M'
104300140206if  1c                   if        DB0ris <> 'M'
104310030515      *
104320040311     c                   eval      SAVlna = DB0lna
104330070312     c                   clear                   OG146arr
104340061016     c                   clear                   OG148arr
104350070312     c                   clear                   OG150arr
104360030515     c     DB0lna        chain     AZORG
104370030515      *
104380030515if  2c                   if        %found(AZORG01L)
104390030515     c                   eval      DB0fag = ORGfag
104400030515     c                   eval      DB0dla = ORGdes
104410030515     c                   eval      DB0inl = ORGind
104420030515     c                   eval      DB0cal = ORGcpf
104430030515     c                   eval      DB0lol = ORGloc
104440030515     c                   eval      DB0prl = ORGpro
104450030515     c                   eval      DB0tel = ORGtel
104460140129     c*//                eval      DB0fax = ORGfax
104470140129     c                   eval      DB0fax = ORGdd2
104480070312     c                   movel     ORGde6        OG146arr
104490061016     c                   movel     ORGde8        OG148arr
104500070312     c                   movel     ORGdf0        OG150arr
104510030515e   2c                   endif
104520030515      *
104530030515e   1c                   endif
104540030515      *
104550030515     c                   ENDSR
104560030515      *
104570030515      *---------------------------------------------------------------*
104580050217      *?Reperimento dati di tutta la tassazione per la stampa        ?*
104590030515      *---------------------------------------------------------------*
104600030515     c     RepTASSAZ     BEGSR
104610030515      *
104620030515     c                   eval      DB0imv = AR6imv
104630030515     c                   eval      DB0cei = AR6cei
104640030515     c                   eval      DB0ift = AR6ift
104650030911      * Esenzione IVA
104660030911if  1c                   if            DB0cei <> *blanks
104670030911     c                             and DB0dce =  *blanks
104680030911     c                   exsr      Tab_EI
104690030911e   1c                   endif
104700030515      *
104710030515      * Controllo se ci sono varie esenti
104720030515      *
104730030515      * - varia 1
104740030515     c                   eval      Wsvx = AR6sv1
104750030515     c                   eval      Wvax = AR6va1
104760030515     c                   exsr      RepTASSAZ_b
104770030515      * - varia 2
104780030515     c                   eval      Wsvx = AR6sv2
104790030515     c                   eval      Wvax = AR6va2
104800030515     c                   exsr      RepTASSAZ_b
104810030515      * - varia 3
104820030515     c                   eval      Wsvx = AR6sv3
104830030515     c                   eval      Wvax = AR6va3
104840030515     c                   exsr      RepTASSAZ_b
104850030515      * - varie ulteriori solo per tipo record 1 (prima bolla)
104860040607     c     KLDVtrc       setll     FIAR7000
104870040607     c     KLDVtrc       reade     FIAR7000
104880030515      *
104890030515do  2c                   Dow       Not %eof(FIAR701L)
104900030515      *
104910030515     c                   eval      Wsvx = AR7svn
104920030515     c                   eval      Wvax = AR7van
104930030515     c                   exsr      RepTASSAZ_b
104940030515      *
104950040607     c     KLDVtrc       reade     FIAR7000
104960030515e   2c                   Enddo
104970030515      *
104980030515     c                   ENDSR
104990030515      *
105000030515      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
105010050217      *?Reperimento dati di tutta la tassazione per la stampa        ?*
105020050217      *?- 2ª parte -                                                 ?*
105030030515      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
105040030515     c     RepTASSAZ_b   BEGSR
105050030515      *
105060030515sel 1c                   Select
105070030515      *
105080030515w   1c                   When      Wsvx       = *blanks
105090030515      *
105100030515w   1c                   When      Wsvx       = §$2iva
105110030515     c                   eval      DB0iva     = Wvax
105120030515      *
105130030515w   1c                   When      Wsvx       = §$2bol
105140030515     c                   add       Wvax          DB0ese
105150050624     c                   z-add     Wvax          wBolloVirt
105160050624     c                   eval      $BolloVirt = *on
105170030515      *
105180030515x   1c                   Other
105190050624     c                   eval      *in31      = *off
105200030515     c     Wsvx          lookup    $Var                                   31
105210030515     c   31              add       Wvax          DB0ese
105220030515     c   31              sub       Wvax          DB0imv
105230030515      *
105240030515w   1c                   Endsl
105250030515      *
105260030515     c                   ENDSR
105270030515      *
105280030515      *---------------------------------------------------------------*
105290050217      *?Chain su TABEL per le PARTICOLARITA'                         ?*
105300030515      *---------------------------------------------------------------*
105310030515     c     TABxPAR       BEGSR
105320030515      *
105330100513     c                   movel(p)  WCdPar        w053a
105340030515     c                   movel     '7V'          TBLcod
105350030515     c                   movel(p)  KeyPAR        TBLkey
105360030515     c     K03TAB        chain     TABEL
105370030515      *
105380030515if  1c                   if        %found(TABEL00F)
105390030515     c                             and TBLflg = *blanks
105400100513     c                   eval      w053a = WCdPar + '-'
105410100513     c                                   + %trim(%subst(TBLuni:1:49))
105420030515x   1c                   else
105430030515     c                   clear                   TBLuni
105440030515e   1c                   endif
105450030515      *
105460030515     c                   ENDSR
105470030515      *
105480030515      *---------------------------------------------------------------*
105490050217      *?CONTROLLO C/A SE E' DA CONVERTIRE IN EURO                    ?*
105500030515      *---------------------------------------------------------------*
105510030515     c     CtrCAs        BEGSR
105520030515      *
105530030515      * E' da convertire
105540030515if  1c                   if        DB0ucm = 'S'
105550030515     c                   eval      $CAeur = *on
105560030515e   1c                   endif
105570030515      *
105580030515      * Se in EURO è da convertire e da stampare in EURO
105590030515if  1c                   if        DB0vca = §GEDcn
105600030515     c                   eval      $CAeur = *on
105610030515     c                   endif
105620030515      *
105630030515      * Devo convertire il c/a da valuta a moneta di conto
105640030624if  1c                   if        DB0vca <> §GEDcn
105650030515     c                   clear                   YEURCODS
105660030515     c                   movel     DB0vca        YECdvi
105670030515     c                   z-add     DB0cas        YECimi
105680030515     c                   movel     §GEDcn        YECdvo
105690030515     c                   call      'YEURCO'
105700030515     c                   parm                    YEURCODS
105710030515if  2c                   if        YECesi = *blanks
105720030515     c                   z-add     YECimo        Wcas
105730030515e   2c                   endif
105740030515x   1c                   else
105750030515     c                   z-add     DB0cas        Wcas
105760030515e   1c                   endif
105770030515      *
105780030515     c                   ENDSR
105790030515      *
105800030515      *---------------------------------------------------------------*
105810050217      *?CONTROLLO DIVISA DELLA FATTURA                               ?*
105820030515      *---------------------------------------------------------------*
105830030515     c     CtrDIV        BEGSR
105840030515      *
105850030515     c                   clear                   dsCV
105860030515      *
105870030515     c                   movel     'CV'          TBLcod
105880030515     c                   movel(p)  DB0div        TBLkey
105890030515     c     K03TAB        chain     TABEL
105900030515     c                   if        %found(TABEL00F)
105910030515     c                   movel     TBLuni        dsCV
105920030515     c                   movel     §CVfdc        DB0fdc
105930030515     c                   endif
105940030515      *
105950030515     c                   ENDSR
105960030515      *
105970030515      *---------------------------------------------------------------*
105980050217      *?SE BOLLA CON MAMMA PRENDO C/A E SEGNACOLLI DA BOLLA MAMMA    ?*
105990030515      *---------------------------------------------------------------*
106000081117     c     BollaFiglia   BEGSR
106010030515      *
106020090528     c                   if        not %open(FNLBL01L)
106030090528     c                   open      FNLBL01L
106040090528     c                   endif
106050090528      *
106060040607     c     KLDV          chain     FNLBL000
106070030515if  1c                   if        %found(FNLBL01L)
106080030515      *
106090030515     c     KLBL          chain     FNARB000
106100030515if  2c                   if        %found(FNARB01L)
106110030515      *
106120031015     c                   eval      wB0nrs = ARBnrs
106130030515     c                   eval      DB0fns = ARBfns
106140030515     c                   eval      DB0fls = ARBfls
106150030515     c                   eval      DB0ncd = ARBncd
106160030515     c                   eval      DB0nca = ARBnca
106170050114     c**!!!              eval      DB0xco = ARBxco
106180030515     c                   eval      DB0znc = ARBznc
106190030515      * Aggancio AR9
106200060505     c     KLBL          chain     FIAR9000
106210060505if  3c                   IF        %found(FIAR901L)
106220030515     c                   eval      DB0cas = AR9cas
106230030515     c                   eval      DB0vca = AR9vca
106240030515     c                   eval      DB0gca = AR9gca
106250030515     c                   eval      DB0tic = AR9tic
106260030515x   3c                   ELSE
106270030515     c                   clear                   DB0cas
106280030515     c                   clear                   DB0vca
106290030515     c                   clear                   DB0gca
106300030515     c                   clear                   DB0tic
106310030515     c                   clear                   DB0dti
106320030515     c                   clear                   DB0ucm
106330030515     c                   clear                   DB0ste
106340030515e   3c                   ENDIF
106350030515      *
106360030515e   2c                   endif
106370030515      *
106380030515e   1c                   endif
106390030515      *
106400030515     c                   ENDSR
106410030903      *
106420030903      *---------------------------------------------------------------*
106430050217      *?Impostazione Quantità e Unità di Misura da FNORM             ?*
106440030903      *---------------------------------------------------------------*
106450030903     c     QUM_FNORM     BEGSR
106460030903      *
106470030905     c*** (già fatto:)   clear                   $Qum
106480030905     c                   clear                   Wx
106490030903      * Colli
106500030905if  1c                   if        ORMncl   > *zeros
106510030905     c                             and Wx   < %elem($Qum)
106520030905     c                   add       1             wx
106530030905     c                   eval      $Qum(wx) = %editc(ORMncl : '4')
106540030905     c                                      + ' colli'
106550030903e   1c                   endif
106560030903      * Peso
106570030905if  1c                   if        ORMpkg   > *zeros
106580030905     c                             and Wx   < %elem($Qum)
106590030905     c                   add       1             wx
106600030905     c                   eval      $Qum(wx) = %editc(ORMpkg : '4')
106610030905     c                                      + ' kg'
106620030905e   1c                   endif
106630071025      * Volume bancali
106640071025if  1c                   if        ORMvlm   > *zeros
106650071025     c                             and Wx   < %elem($Qum)
106660071025     c                   add       1             wx
106670071025     c                   eval      $Qum(wx) = %editc(ORMvlm : '4')
106680071025     c                                      + ' mc'
106690071025e   1c                   endif
106700030905     *** * Volume
106710030905     *** * (in ultimo posto SOLO SE non ci sono bilici/motrici/autotreni)
106720030905if  1***c                   if        ORMvlm   > *zeros
106730030905     ***c                             and (Wx  < %elem($Qum) - 1
106740030905     ***c                              or  Wx  = %elem($Qum) - 1
106750030905     ***c                              and w0112 = *zeros)
106760030905     ***c                   add       1             wx
106770030905     ***c                   eval      $Qum(wx) = %editc(ORMvlm : '4')
106780030905     ***c                                      + ' metri cubi'
106790030905e   1***c                   endif
106800030905     *** * Bancali
106810030905     *** * (in ultimo posto SOLO SE non ci sono bilici/motrici/autotreni)
106820030905if  1***c                   if        ORMbnc   > *zeros
106830030905     ***c                             and (Wx  < %elem($Qum) - 1
106840030905     ***c                              or  Wx  = %elem($Qum) - 1
106850030905     ***c                              and w0112 = *zeros)
106860030905     ***c                   add       1             wx
106870030905     ***c                   eval      $Qum(wx) = %editc(ORMbnc : '4')
106880030905     ***c                                      + ' bancali'
106890030905e   1***c                   endif
106900030905     *** * Bilici
106910030905if  1***c                   if        ORMblc  > *zeros
106920030905     ***c                             and Wx  < %elem($Qum)
106930030905     ***c                   add       1             wx
106940030905     ***c                   eval      $Qum(wx) = %editc(ORMblc : '4')
106950030905     ***c                                      + ' bilici'
106960030905e   1***c                   endif
106970030905     *** * Motrici
106980030905if  1***c                   if        ORMmtc  > *zeros
106990030905     ***c                             and Wx  < %elem($Qum)
107000030905     ***c                   add       1             wx
107010030905     ***c                   eval      $Qum(wx) = %editc(ORMmtc : '4')
107020030905     ***c                                      + ' motrici'
107030030905e   1***c                   endif
107040030905     *** * Autotreni
107050030905if  1***c                   if        ORMatt  > *zeros
107060030905     ***c                             and Wx  < %elem($Qum)
107070030905     ***c                   add       1             wx
107080030905     ***c                   eval      $Qum(wx) = %editc(ORMatt : '4')
107090030905     ***c                                      + ' autotreni'
107100030905e   1***c                   endif
107110030903      *
107120030903     c                   ENDSR
107130130306      *
107140130306      /free
107150130306
107160130306       //--------------------------------------------------------------
107170130306       //?Scrittura record (relativo alla spedizione) nel *file PDFLIST?
107180130306       //--------------------------------------------------------------
107190130306       BEGSR  sr_WrtRec_PdfList;
107200130306
107210130307         // -?NON richiesta stampa del PDF?
107220140404         if  %parms() = 1;
107230130306           leavesr;
107240130306         endif;
107250130306
107260130306         // -?Reperimento numero "chiave elenco"?(SE non ricevuto)?
107270130306         If  DB6num <= *zero;
107280130306
107290130306           clear  TRUL33ds;
107300130306           I33tla = 'L';
107310130306           I33ope = *zero;
107320130306           I33cnu = 55;
107330130306           I33num = 1;
107340130306           kpjbu  = TRUL33ds;
107350130306
107360130306           TRUL33R ( kpjba );
107370130306
107380130306           TRUL33ds = kpjbu;
107390130307           if  O33err > *zero;
107400130307             Pmsg1  = $Msg(3);
107410130307             Pmsg2  = $Msg(O33err + 3);
107420130418             *in33  = *off;
107430130418             exsr  Posiz_Box;
107440130307             write  A4prtMSG;
107450130307             DB0tms = 'E';
107460130306             exsr  sr_End;
107470130306           endif;
107480130306
107490130306           DB6num = %editc( O33nri : 'X' );
107500130306
107510130306         EndIf;
107520130307
107530130307         // -?Apertura file?
107540130307         if  Not %open(PDFLIST);
107550130307           open  PDFLIST;
107560130307         endif;
107570130306
107580130308         // -?Ciclo di lettura record "§..." (ex "§PDF") in FIAR801L?
107590130306         clear  k06fiar801;
107600130306         k_AR8aas = DB0aas;
107610130306         k_AR8lnp = DB0lnp;
107620130306         k_AR8nrs = DB0nrs;
107630130306         k_AR8nsp = DB0nsp;
107640130306         k_AR8tmd = c_§PDF;
107650130306         setll  %kds( k06fiar801 : 5 )  FIAR8000;
107660130308         reade  %kds( k06fiar801 : 4 )  FIAR8000;
107670130306
107680130308         DoW  Not %eof(FIAR801L)  and  %subst(AR8tmd : 1 : 1) = '§';
107690130306
107700130306           dAR8§PDF = AR8uni;
107710130308
107720130315           If  AR8atb = *blank  and  §AR8Ptrt = 'S';
107730130306
107740130308             // -?Scrittura del rec. relativo alla sped. nel *file PDFLIST?
107750130308             clear  RPDF;
107760130308             PDFklist  = DB6num;
107770130308             PDFksped  = %editc( DB0aas : 'X' ) + %editc( DB0lnp : 'X' ) +
107780130308                         %editc( DB0nrs : 'X' ) + %editc( DB0nsp : 'X' );
107790130311             PDFkey    = %trimr(§AR8PpdfP) + '_' + §AR8PpdfN;
107800130311             //PDFpath   = *blank;
107810130308             PDFinfo   = c_CopiaXdest + ' - ' + Prsd +
107820130308                         ' Spedizione ' + %editc(DB0lnp : 'X') +
107830130308                                    ' ' + %editc(DB0nrs : 'X') +
107840130308                                    ' ' + %editc(DB0nsp : 'X') +
107850130308                         ' del ' + %editc(Pdsp : 'Y') + ' ' +
107860130308                         Pndc;
107870130308             PDFresol  = 'S';
107880130308             PDFcopieM = '1';
107890130308             PDFcopieD = '0';
107900130308             PDFcopieS = '0';
107910130308             //PDFnoInfo = *blank;
107920130308             //PDFinfoP  = *blank;
107930130308             //PDFprtFst = *blank;
107940130308             wTime_ds  = %char( %timestamp() );
107950131008             PDFinsDt  = wTime_ds.wDate_Iso;
107960131008             PDFinsTm  = wTime_ds.wTime;
107970131008             //PDFprtDt  = wTime_ds.wDate_Iso;
107980131008             //PDFprtTm  = wTime_ds.wTime;
107990130308             PDFusrins = SDSusr;
108000130308             //__________
108010130308             write  RPDF;
108020130308             //¯¯¯¯¯¯¯¯¯¯
108030130308
108040130308           EndIf;
108050130306
108060130418           reade  %kds( k06fiar801 : 4 )  FIAR8000;
108070130306
108080130306         EndDo;
108090130306
108100130306       ENDSR;
108110130306
108120130306       //--------------------------------------------------------------
108130140303       //?Stampa immagini associate alle spediz. (dati già in PDFLIST) ?
108140130306       //--------------------------------------------------------------
108150130306       BEGSR  sr_PrtPdf;
108160130306
108170130306         // -?Impostazione parametri?
108180130312         PDF_KList    = DB6num;
108190130312         PDF_Printer  = trulPOVRds.§ULPoOutQ;
108200130312         PDF_FormType = trulPOVRds.§ULPoFType;
108210130312         PDF_UsrDta   = trulPOVRds.§ULPoUDta;
108220130306
108230130306         // -?Richiamo funzione di stampa di tutte le immagini PDF?
108240130312         //  ?(segnalando - in stampa - l'eventuale errore)?
108250130312         callp(E)  Stampa_PDF ( PDF_KList :
108260130312                                PDF_Printer :
108270130312                                PDF_FormType :
108280130312                                PDF_UsrDta );
108290130312
108300130312         if  %error;
108310130312           Pmsg2  = $Msg(7);
108320130418           *in33  = *off;
108330130418           exsr  Posiz_Box;
108340130312           write  A4prtMSG;
108350130312           DB0tms = 'E';
108360130312           exsr  sr_End;
108370130312         endif;
108380130307
108390130307         // -?Restituzione tipo modulo stampato?
108400130307         DB0tms = 'F';
108410130306
108420130306       ENDSR;
108430130306
108440130306      /end-free
108450030903      *
108460030515      *---------------------------------------------------------------*
108470050217      *?DEFINIZIONE KEY-LIST                                         ?*
108480030515      *---------------------------------------------------------------*
108490030515     c     DefKLIST      BEGSR
108500030515      *
108510051115      * - FNBLP01L / FNBLT01L /
108520060505      *   FNARB01L / FNART01L /
108530030515      *   FNLBL01L /
108540060505      *   FIAR501L/31C / FIAR901L
108550040607     c     KLDV          klist
108560030515     c                   kfld                    DB0aas
108570030515     c                   kfld                    DB0lnp
108580030515     c                   kfld                    DB0nrs
108590030515     c                   kfld                    DB0nsp
108600060505      * - FIAR401L / FIAR601L / FIAR701L
108610060505     c     KLDVtrc       klist
108620060505     c                   kfld                    DB0aas
108630060505     c                   kfld                    DB0lnp
108640060505     c                   kfld                    DB0nrs
108650060505     c                   kfld                    DB0nsp
108660060505     c                   kfld                    Wtrc
108670040607      * - FIAR501L/31C
108680030717     c     K05AR501      klist
108690030717     c                   kfld                    DB0aas
108700030717     c                   kfld                    DB0lnp
108710030717     c                   kfld                    DB0nrs
108720030717     c                   kfld                    DB0nsp
108730030717     c                   kfld                    AR5trd
108740040607      * - FIAR801L
108750040607     c     K06AR801      klist
108760040607     c                   kfld                    DB0aas
108770040607     c                   kfld                    DB0lnp
108780040607     c                   kfld                    DB0nrs
108790040607     c                   kfld                    DB0nsp
108800040607     c                   kfld                    AR8tmd
108810040607     c                   kfld                    AR8prr
108820070514      * - FNBLT01L / FNART01L
108830070514     c     KLDVnsc       Klist
108840070514     c                   Kfld                    DB0aas
108850070514     c                   Kfld                    DB0lnp
108860070514     c                   Kfld                    DB0nrs
108870070514     c                   Kfld                    DB0nsp
108880070514     c                   Kfld                    Wnsc
108890070319      * - FIARN11L
108900070319     c     K06ARN11      klist
108910070319     c                   kfld                    DB0aas
108920070319     c                   kfld                    DB0lnp
108930070319     c                   kfld                    DB0nrs
108940070319     c                   kfld                    DB0nsp
108950070319     c                   kfld                    ARNcdn
108960070319     c                   kfld                    ARNgst
108970090528      * - FNBLP01L/ / FNARB01L / FIAR901L di bolla MAMMA
108980070514     c     KLBL          klist
108990070514     c                   kfld                    LBLaap
109000070514     c                   kfld                    LBLlpp
109010070514     c                   kfld                    LBLnrp
109020070514     c                   kfld                    LBLnsp
109030070514      * - FNBLT01L / FNART01L di bolla MAMMA
109040070514     c     KLBLnsc       Klist
109050070514     c                   kfld                    LBLaap
109060070514     c                   kfld                    LBLlpp
109070070514     c                   kfld                    LBLnrp
109080070514     c                   kfld                    LBLnsp
109090070514     c                   Kfld                    Wnsc
109100070514      * - FIARS01L
109110070514     c     K05ARS01P     klist
109120070514     c                   kfld                    BLTfls
109130070514     c                   kfld                    BLTlna
109140070514     c                   kfld                    BLTnrs
109150070514     c                   kfld                    BLTnsc
109160070514     c                   kfld                    ARStrc
109170070514     c     K05ARS01A     klist
109180070514     c                   kfld                    ARTfls
109190070514     c                   kfld                    ARTlna
109200070514     c                   kfld                    ARTnrs
109210070514     c                   kfld                    ARTnsc
109220070514     c                   kfld                    ARStrc
109230060505      * - FNORM01L
109240060505     c     K04ORM01      klist
109250071015     c                   kfld                    ORMpoe
109260071015     c                   kfld                    ORMnsr
109270071015     c                   kfld                    ORMnor
109280071015     c                   kfld                    ORMnrv
109290030515      * - TABEL00F
109300030515     c     K03TAB        klist
109310040223     c                   kfld                    TBLkut
109320030515     c                   kfld                    TBLcod
109330030515     c                   kfld                    TBLkey
109340030515     c     K02TAB        klist
109350040223     c                   kfld                    TBLkut
109360030515     c                   kfld                    TBLcod
109370030515      * - FLNUF01L
109380030515     c     K04NUF        klist
109390030515     c                   kfld                    NUFaaa
109400030515     c                   kfld                    NUFcnu
109410030515     c                   kfld                    NUFfil
109420030515     c                   kfld                    NUFfl2
109430030515      *
109440030515     c                   ENDSR
109450030515
109460030515** > $Msg ---------------------------------------------------------------------*
109470030515IMPOSSIBILE CONTINUARE LA RISTAMPA BOLLE
109480071112PERCHE' STAMPATE PIU' DI &NrLdV BOLLE !!!
109490130307ERRORE IN REPERIMENTO NUMERATORE PER STAMPA PDF ALLEGATO ALLA LDV:
109500130307   NON REPERITO IL NUMERATORE
109510130307   NUMERATORE ALLOCATO
109520130307   SUPERATO IL LIMITE MASSIMO PER IL NUMERATORE
109530130312RILEVATO ERRORE IN FASE DI STAMPA DEI DOCUMENTI ALLEGATI
109540140303** > $GgS/$Gio *
1095500306241LUNEDI
1095600306242MARTEDI
1095700410263MERCOLEDI
1095800306244GIOVEDI
1095900306245VENERDI
1096000305156SABATO
1096100305157DOMENICA
109620030515M          MATT.
109630030515P          POM.
