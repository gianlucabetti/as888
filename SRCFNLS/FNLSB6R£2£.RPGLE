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
006880110427       // -?Campi utilizzati per il salvataggio momentaneo di dati?
006890040608     d SavLineA4       s                   like(CurLineA4)
006900040311     d SAVlnp          s                   like(DB0lnp)   inz(*loval)
006910040311     d SAVlna          s                   like(DB0lna)   inz(*loval)
006920030515     d SAVtrd          s                   like(AR5trd)   inz(*hival)
006930030515     d SAVdac          s                   like(AR5dac)   inz(*hival)
006940030515     d SAVorc          s                   like(AR5orc)   inz(*hival)
006950040607     d SAVprr          s                   like(AR8prr)   inz
006960041207     d SAVtrk          s                   like(AR8trk)   inz
006970070314     d wRSxCF          s                   like(DB0rsm)   inz
006980071114     d wB0cfi          s                   like(DB0cpi)   inz
006990071114     d wB0piv          s                   like(DB0cpi)   inz
007000031015     d wB0nrs          s                   like(ARBnrs)   inz
007010070514     d wB0ndc          s                   like(ARBndc)   inz
007020070514     d wB0ntc          s                   like(ARBntc)   inz
007030070514     d wB0dcp          s                   like(ARBdcp)   inz
007040081119     d wB0tmc          s                   like(ARBtmc)   inz
007050101125     d wB0dcm          s                   like(ARBdcm)   inz
007060101125     d wB0icc          s                   like(ARBicc)   inz
007070101125     d wB0ica          s                   like(ARBica)   inz
007080130124     d wB0fiv          s                   like(AR6fiv)   inz
007090060505     d Wnsc            s                   like(ARSnsc)   inz
007100060505     d Wtrc            s                   like(ARStrc)   inz
007110030515     d Wctr            s                   like(DB0ctr)   inz
007120031124     d Wdsp            s                   like(DB0dft)   inz
007130090527     d WddtCLN         s                   like(DB0drt)   inz
007140090527     d WdspCLN         s                   like(DB0dft)   inz
007150030515     d Wsvx            s                   like(AR6sv1)   inz
007160030515     d Wvax            s                   like(AR6va1)   inz
007170150226     d*// Wtba            s                   like(§AR5tba)  inz
007180080708
007190110427       // -?Comando da eseguire per QCMDEXC?
007200080708     d Qcmd            s            512    inz
007210080708
007220090302       //--------------------------------------------------------------
007230130306       //?Definizione prototipi procedure usate.                       ?
007240090302       //--------------------------------------------------------------
007250140915
007260140915       // -?Calcolo/Controllo BRTcode?
007270140915      /copy gaitrasrc/srcProtoPI,TRUL28R0
007280140915      /copy gaitrasrc/srcProtoPR,TRUL28R0
007290080708
007300100208       // -?Calcolo tempi di delivery?
007310090605      /copy gaitrasrc/srcProtoPR,TNSD99R
007320100208
007330100208       // -?Determinazione intestatario contrassegno?
007340100208      /copy gaitrasrc/srcProtoPR,TRULINTR
007350130306
007360130306       // -?Reperimento/Aggiornamento numeratori in GRU?
007370130306     d TRUL33ds      e ds                  inz
007380130306      /copy gaitrasrc/srcProtoPR,TRUL33R
007390090605
007400100208       // -?Override al PrtF?
007410080709     d fnlsB8r         pr                  extpgm('FNLSB8R')
007420080709     d  fnlsB5ds                           likeds(FNLSB5ds)
007430130306
007440130306       // -?Reperimento override apportate al PrtF?
007450130306     d wPrtF           s             10
007460130307     d trulPOVRds    e ds                  qualified
007470130306     d trulPOVRc       pr                  extpgm('TRULPOVRC')
007480130306     d   wPrtF                       10
007490130306     d   trulPOVRds                        likeds(trulPOVRds)
007500130306
007510130306       // -?Stampa tutte le immagini PDF abbinate alle LdV già stampate?
007520130312      /copy gaitrasrc/srcProtoPI,PDF00STAR
007530130312      /copy gaitrasrc/srcProtoPR,PDF00STAR
007540080708
007550100208       // -?Esecuzione comando di sistema?
007560080708     d qCmdExc         pr                  extpgm('QCMDEXC')
007570080708     d  Qcmd                        512    const  options(*varsize)
007580080708     d  Qlen                         15  5 const
007590090302
007600090302       //--------------------------------------------------------------
007610090302       //?Definizione key-list.                                        ?
007620090302       //--------------------------------------------------------------
007630090302
007640110427       // -?File TNTBE01L?
007650090302     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
007660090302     d                                     prefix(k_)   inz
007670130306
007680130306       // -?File FIAR801L?
007690130306     d k06fiar801    e ds                  extname(FIAR801L : *key)
007700130306     d                                     prefix(k_)   inz
007710140303
007720140303       // -?File FNEVB22L?
007730140303     d keyFNEVB22    e ds                  extname(FNEVB22L : *key)
007740140303     d                                     prefix(k_)   inz
007750140410
007760140410       // -?File FNORE01L?
007770140410     d keyFNORE01    e ds                  extname(FNORE01L : *key)
007780140410     d                                     prefix(k_)
007790140410     d                                     inz
007800170517
007810170517       // -?File TFNTC01L?
007820170517     d keyTFNTC01    e ds                  extname(TFNTC01L : *key)
007830170517     d                                     prefix(k_)
007840170517     d                                     inz
007850030515
007860030619      *===============================================================*
007870050217      *?RIEPILOGO INDICATORI                                         ?*
007880030515      *---------------------------------------------------------------*
007890050216      *?18   ?- Stampa DATA e ORA CONSEGNA RICHIESTA                  *
007900050216      *?19   ?- Stampa IMPONIBILE, IVA, TOTALE ed ESTREMI FATTURA     *
007910050216      *?20   ?- Stampa CONTRASSEGNO                                   *
007920050216      *?21   ?- Stampa TOTALE DA ESIGERE ALLA CONSEGNA / AL RITIRO    *
007930051129      *?22   ?- Stampa NOTE (se NO imponibile e fattura)              *
007940050216      *?31   ?- di comodo                                             *
007950130418      *?33off?- Stampa della COPIA PER IL CORRIERE                    *
007960130418      *?33on ?- Stampa della COPIA PER IL DESTINATARIO                *
007970130418      *         (o per il Mittente, se P.F. Prepagato)                *
007980050216      *?35   ?- Richiesta FIRMA, DATA e ORA   in 1ª copia             *
007990050217      *         per ricevuta della merce senza riserve                *
008000050217      *         e indica "TOTALE DA ESIGERE ALLA CONSEGNA"            *
008010050216      *?36   ?- Richiesta FIRMA per QUIETANZA in 2ª copia             *
008020050217      *         e indica "TOTALE DA ESIGERE AL RITIRO"                *
008030050216      *?37   ?- Condiziona HIGHLIGHT nel campo PDS2  in rec. PRTLDV1  *
008040050217      *                                  per servizio "ESPRESSO"      *
008050050217      *                                o per servizio "POSTE IT."     *
008060050216      *?38   ?- Condiziona FONT      nel campo PAR5  in rec. PRTAR52  *
008070050216      *?39   ?- Condiziona UNDERLINE nel campo PRIG  in rec. PRTGEN   *
008080050217      *                            e nel campo PCTXX in rec. PRTDK5   *
008090050217      *                            e nel campo PCTXT in rec. PRTDTx   *
008100050217      *                            e nel campo PCTXT in rec. PRTDDx   *
008110080604      *?41   ?- Stampa copia per il corriere della 1ª LdV             *
008120080604      *?42   ?- Stampa copia per il destinatario della 1ª LdV         *
008130080604      *?43   ?- Stampa copia per il corriere della 2ª LdV             *
008140080604      *?44   ?- Stampa copia per il destinatario della 2ª LdV         *
008150080918      *?45off?- Posiziona l'inizio della packing-list al 2/4 dell'A4  *
008160080923      *         (packing-list di 2 LdV per A4)                        *
008170080918      *?45on ?- Posiziona l'inizio della packing-list in cima all'A4  *
008180080923      *         (1ª packing-list in A4 dopo la stampa di 4 LdV x A4)  *
008190080923      *?46on ?- Posiziona l'inizio della packing-list a metà dell'A4  *
008200080923      *         (2ª packing-list in A4 dopo la stampa di 4 LdV x A4)  *
008210120530      *?50   ?- Stampa CODICE A BARRE - BRT                           *
008220050217      *?51   ?- Stampa CODICE A BARRE - DPD                           *
008230050217      *?52   ?- Stampa CODICE A BARRE - O.R.M.                        *
008240120530      *?55   ?- NON stampa logo BRT in DDT della Logistica =>         *
008250120530      *         => Stampa il simil-logo in LSB6LOGBRT                 *
008260050217      *?69off?- Stampa su modulo in formato "A4"                      *
008270050217      *?69on ?- Stampa su modulo in formato "A5"                      *
008280120530      *?70   ?- Trattasi di BOLLA BRT                                 *
008290050216      *?71   ?- Trattasi di BOLLA DPD                                 *
008300050216      *?72   ?- Trattasi di BOLLA FedEx                               *
008310080912      *?73   ?- Trattasi di BOLLA EuroExpress                         *
008320030619      *===============================================================*
008330030619      * Descrizione dei campi utilizzati per il posizionamento di     *
008340030619      *   linee orizzontali, linee verticali o campi di stampa        *
008350030619      * (utilizzati solo nel file di stampa per il formato "A4")      *
008360030619      * servono ad impostare la distanza di tali oggetti dal lato     *
008370030619      * superiore del foglio.                                         *
008380030619      *---------------------------------------------------------------*
008390080604      *? BOX nella copia per il vettore:                             ?*
008400080604      *§BOX1AS = posiz. linea orizzontale inizio box mittente (1)   ?*
008410080604      *§BOX1AI = posiz. linea orizzontale fine   box mittente (1)   ?*
008420080604      *§BOX1BS = posiz. linea orizzontale inizio box generale (2)   ?*
008430080604      *§BOX1BI = posiz. linea orizzontale fine   box generale (2)   ?*
008440140919      *§BOX1BJ = posiz. linea orizzontale fine   box BRTcode        ?*
008450080604      *§BOX1CS = posiz. linea orizzontale inizio box contrassegno   ?*
008460080604      *§BOX1CI = posiz. linea orizzontale fine   box contrassegno   ?*
008470080604      *§BOX1DS = posiz. linea orizzontale inizio box tot.da esigere ?*
008480080604      *§BOX1DI = posiz. linea orizzontale fine   box tot.da esigere ?*
008490080604      *§LO1S1  = posiz. linea orizzontale separatrice               ?*
008500080604      *           Destinatario / Colli, Peso, Volume...               *
008510080604      *§LO1S2  = posiz. linea orizzontale separatrice               ?*
008520080922      *           Colli, Peso, Volume / Zona                          *
008530080922      *§LO1S3  = posiz. linea orizzontale separatrice               ?*
008540080922      *           Zona / Segnacolli                                   *
008550080922      *§LO1S4  = posiz. linea orizzontale separatrice               ?*
008560080922      *           Segnacolli / Turno di chiusura                      *
008570080922      *§LO1S5  = posiz. linea orizzontale separatrice               ?*
008580080922      *           Turno di chiusura / "PER RICEVUTA DELLA MERCE..."   *
008590080604      *§LO1C1  = posiz. linea orizzontale separatrice               ?*
008600080624      *           Servizio / Consegne Particolari                     *
008610080604      *§LO1D1  = posiz. linea orizzontale separatrice               ?*
008620080604      *           Riferimenti Mittente / Note e Particolarità         *
008630080605      *§LLV1S1 = lunghezza linea verticale separatrice              ?*
008640080605      *           Destinatario / Riferimento mittente                 *
008650080605      *§LLV1S2 = lunghezza linea verticale separatrice              ?*
008660080605      *           Riferimento mittente / Spedizione                   *
008670120530      *§PC0 <=> §PC6 =  posizioni costanti intestazione "BRT"       ?*
008680120530      *           "BRT SpA", "Sede Operativa ed Amministrativa"       *
008690140917      *§PK1    = posiz. costante "BRTcode" + codice                 ?*
008700080604      *§PK2    = posiz. costante "TASSATIVA"                        ?*
008710080605      *§PK3    = posiz. costanti "(nome e cognome in stampatello)"  ?*
008720080605      *           (in "PER RICEVUTA DELLA MERCE ...")                 *
008730121017      *§PK4    = posiz. costante "COPIA PER IL DESTINATARIO"        ?*
008740121017      *§PK5    = posiz. riferimento alla legge in vigore x RCV      ?*
008750121017      *           (tra n° Spedizione e Imponibile...)                 *
008760121017      *§PK6    = posiz. costante "Bollo assolto in modo virtuale..."?*
008770121017      *§PK7    = posiz. Partita IVA o Cod. Fiscale del cliente      ?*
008780080604      *                                                               *
008790080604      *? Altre posizioni:                                            ?*
008800050325      *§PB1 = posizione linea orizzontale inizio box                ?*
008810050325      *§PB2 = posizione linea orizzontale fine   box                ?*
008820050216      *§LO1 = posizione x 1ª linea orizzontale                      ?*
008830080604      *        LSB6FED1 - richiesta nome e firma per FedEx            *
008840080604      *        LSB6FED3 - limite superiore del box per FedEx          *
008850080604      *        LSB6AR53 - limite superiore dei box da  FIAR5          *
008860030619      *                   (bancali da rendere, supermercato, promem.) *
008870080604      *        LSB6DK0  - linea separatrice nel 1° box DDT Karnak     *
008880080604      *        LSB6DL0A - linea separatrice nel box DDT Logistica     *
008890080604      *        LSB6DL0C - linea separatrice nel box DDT Logistica     *
008900050325      *                   (area Packing List)                         *
008910080604      *        LSB6DL0E - linea separatrice nel box DDT Logistica     *
008920050606      *                   (area Fattura)                              *
008930080604      *        LSB6AC0A - linea separatrice nel box DDT Arcte         *
008940080604      *        LSB6ES0A - linea separatrice nel box DDT Esprinet      *
008950050216      *§LO2 = posizione x 2ª linea orizzontale                      ?*
008960080604      *        LSB6FED1 - linea orizzontale separatrice per FedEx     *
008970080604      *        LSB6FED3 - limite inferiore del box per FedEx          *
008980080604      *        LSB6AR53 - limite inferiore dei box da  FIAR5          *
008990030619      *                   (bancali da rendere, supermercato, promem.) *
009000080604      *        LSB6DK0  - linea separatrice nel 2° box DDT Karnak     *
009010080604      *        LSB6DL0A - linea separatrice nel box DDT Logistica     *
009020080604      *        LSB6DL0C - linea separatrice nel box DDT Logistica     *
009030050325      *                   (area Packing List)                         *
009040080604      *        LSB6DL0E - linea separatrice nel box DDT Logistica     *
009050050610      *                   (area Fattura)                              *
009060080604      *        LSB6AC0A - linea separatrice nel box DDT Arcte         *
009070080604      *        LSB6ES0A - linea separatrice nel box DDT Esprinet      *
009080050216      *§LO3 <=> §LO9/A = posizioni x linee orizzontali              ?*
009090080604      *        LSB6DK0  - linee separatrici nel 2° box DDT Karnak     *
009100080604      *        LSB6DL0B - linee separatrici (orizzontali e verticali) *
009110050218      *                   nella parte finale del box DDT Logistica    *
009120080604      *        LSB6DL0F - linea separatrice nel box DDT Logistica     *
009130050610      *                   (area Fattura nella parte finale del box)   *
009140080604      *        LSB6AC0B - linee separatrici (orizzontali e verticali) *
009150050218      *                   nella parte finale del box DDT Arcte        *
009160080604      *        LSB6ES0B - linee separatrici (orizzontali e verticali) *
009170050216      *                   nella parte finale del box DDT Esprinet     *
009180050216      *§LV1 = posizione x 1ª linea verticale                        ?*
009190080604      *        LSB6FED1 - lineee verticali "lunghe" (0.15 inch) su    *
009200030619      *                   quella orizzontale per FedEx                *
009210080604      *        LSB6FED3 - lineee verticali "lunghe" (0.25 inch) nel   *
009220030619      *                   box per FedEx (separatori)                  *
009230050216      * "   = lunghezza delle linee verticali                       ?*
009240080604      *        LSB6DK0  - linee separatrici nella 1ª parte alta del   *
009250040607      *                   2° box per DDT Karnak                       *
009260080604      *        LSB6DL0A - linee separatrici nella 1ª parte alta del   *
009270050218      *                   box per DDT Logistica                       *
009280080604      *        LSB6DL0C - linee separatrici nella parte alta del box  *
009290050325      *                   DDT Logistica, area Packing List            *
009300080604      *        LSB6DL0E - linea separatrice nel box DDT Logistica     *
009310050610      *                   (area Fattura)                              *
009320080604      *        LSB6AC0A - linee separatrici nella 1ª parte alta del   *
009330050218      *                   box per DDT Arcte                           *
009340080604      *        LSB6AR54 - linea sx "simil" box AR54                   *
009350071025      *                   (v. "Ritiro contestuale alla consegna" o    *
009360071025      *                       "Ritiro porto franco prepagato")        *
009370050216      *§LV2 = posizione x 2ª linea verticale                        ?*
009380080604      *        LSB6FED1 - lineee verticali "corte" (0.07 inch) su     *
009390030619      *                   quella orizzontale per FedEx                *
009400080604      *        LSB6FED3 - lineee verticali "corte" (0.10 inch) nel    *
009410030619      *                   box per FedEx (separatori)                  *
009420050216      * "   = lunghezza delle linee verticali                       ?*
009430080604      *        LSB6DK0  - linee separatrici nella 2ª parte alta del   *
009440040607      *                   2° box per DDT Karnak                       *
009450080604      *        LSB6DL0A - linee separatrici nella 2ª parte alta del   *
009460050218      *                   box per DDT Logistica                       *
009470080604      *        LSB6DL0C - linee separatrici nella parte alta del box  *
009480050325      *                   DDT Logistica, area Packing List            *
009490080604      *        LSB6DL0F - linea separatrice nel box DDT Logistica     *
009500050610      *                   (area Fattura - corpo fattura con totali)   *
009510080604      *        LSB6AC0A - linee separatrici nella 2ª parte alta del   *
009520050218      *                   box per DDT Arcte                           *
009530080604      *        LSB6ES0A - linea separatrice nel 1° box DDT Esprinet   *
009540050216      *§LV3 <=> §LV7   = posizioni x linee verticali                ?*
009550080604      *        LSB6DK0  - linee separatrici nel 2° box DDT Karnak     *
009560080604      *        LSB6DL0A - linee separatrici nel box DDT Logistica     *
009570080604      *        LSB6DL0C - linee separatrici nel box DDT Logistica     *
009580050325      *                   (area Packing List)                         *
009590080604      *        LSB6AC0A - linee separatrici nel box DDT Arcte         *
009600050216      *§PS1 = posizione per campi                                   ?*
009610080604      *        LSB6FED3 - posizione dei campi nel box per FedEx:      *
009620030619      *                   · Package Tracking Number                   *
009630030619      *                   · Status                                    *
009640080604      *        LSB6AR51 - posizione dell'unico campo nel box          *
009650080604      *        LSB6AR52 - posizione dell'unico campo nel box          *
009660080604      *        LSB6AR5A - posizione dell'unico campo nel box          *
009670080604      *        LSB6AR5B - posizione dell'unico campo nel box          *
009680080604      *        LSB6AR5C - posizione dell'unico campo nel box          *
009690080604      *        LSB6AR5G - posizione dell'unico campo nel box          *
009700080604      *        LSB6TAG  - posizione dell'unico campo nel box          *
009710080604      *        LSB6DK0  - posizione della 1ª riga di testo            *
009720081003      *§PS2 <=> §PS8  -> posizioni dei campi                        ?*
009730080604      *        LSB6DK0  - posizioni delle righe di testo              *
009740080604      *        LSB6DL0B - posizioni delle righe di testo              *
009750080604      *        LSB6AC0B - posizioni delle righe di testo              *
009760080604      *        LSB6ES0B - posizioni delle righe di testo              *
009770081003      *        LSB6DL9  - posizioni delle righe di testo              *
009780050216      *§PR1 <=> §PR2  -> posizioni dei campi (rovesciati di 270°)   ?*
009790080604      *        LSB6DK0  - posizioni delle righe di testo              *
009800080604      *        LSB6DL0A - posizioni delle righe di testo              *
009810080604      *        LSB6DL0C - posizioni delle righe di testo              *
009820080604      *        LSB6AC0A - posizioni delle righe di testo              *
009830080604      *        LSB6ES0A - posizioni delle righe di testo              *
009840030619      *===============================================================*
009850030515
009860030515     c     *Entry        plist
009870080703     c                   parm                    FNLSB5ds
009880130306     c                   parm                    FNLSB6ds1
009890110427
009900110427       // -?Tipo lancio (DB0tla):?"C" -           Chiudo con LR?
009910110427       //                        ?"L" - Elaboro e chiudo con LR?
009920110427       //                        ?" " - Elaboro e chiudo in RETRN?
009930080703if  1c                   if        DB0tla = 'C'
009940030515     c                   goto      Fine
009950060921e   1c                   endif
009960110427
009970110427       // -?Operazioni iniziali?
009980070123     c                   exsr      sr_Start
009990110427
010000030515sel 1c                   SELECT
010010130306
010020130306       // -?Richiesta la stampa (finale) delle sole immagini PDF?
010030130306     c                   WHEN      %parms() > 1  and  DB6pdf = 'S'
010040130307     c*//                                        and  DB0fa4 = 'I'
010050130306     c                   if        DB6num > *zero
010060130306     c                   exsr      sr_PrtPdf
010070130306     c                   endif
010080130306     c                   exsr      sr_End
010090110427
010100110427       // -?NON passati i dati LDV => caricamento?
010110030515w   1c                   WHEN      DB0ges = *blanks
010120071008if  2c                   if             DB0tbo  = 'P'
010130071008     c                             or   DB0tbo  = '8'
010140030515     c                   exsr      CarBLP
010150030515x   2c                   else
010160030515     c                   exsr      CarARB
010170030515e   2c                   endif
010180110427
010190110427       // -?Passati i dati LDV => è già (quasi) tutto pronto?
010200110427       //  ?(solo dalla sede - vedi TNSD50R)?
010210081114w   1c                   WHEN      DB0ges = 'B'
010220090513     c                   exsr      CarTAB
010230081114     c*** (già fatto:)   clear                   dsBL4A
010240081114     c                   if        DB0fbr = 'S'
010250081114     c                   exsr      RepAR4_A
010260081114     c***                if        §B4Abm = 'S' or §B4Abm = 'C' or
010270081114     c***                          §B4Abm = 'P' or §B4Abm = 'Q'
010280081114     c***                movel     'S'           DB0bam
010290081114     c***                endif
010300081114     c                   endif
010310081114     c                   eval      wB0cfi = DB0cpi
010320081114     c                   eval      wB0piv = DB0cpi
010330110427
010340110427       // -?DB0GES = "R" => Stampa una LDV vuota con un messaggio?
010350110427       //  ?per avvisare che è impossibile continuare la ristampa?
010360110427       //  ?perchè stampate più di 6.000 bolle !?
010370030515w   1c                   WHEN      DB0ges = 'R'
010380071112     c                   exsr      Rep_TabVPO
010390071112     c                   eval      Pmsg1  = $Msg(1)
010400071112     c                   eval      Pmsg2  = %replace( %editc(
010410071112     c                                         §VPOmaxldv : 'J' ) :
010420071112     c                                         $Msg(2) : %scan(
010430071112     c                                         '&NrLdV' : $Msg(2) ))
010440130418     c                   eval      *in33 = *off
010450130418     c                   exsr      Posiz_Box
010460030515     c                   write     A4prtMSG
010470030515     c                   goto      Fine
010480110427
010490030515e   1c                   ENDSL
010500110427
010510110427       // -?Controllo del tipo LDV da stampare?
010520040607     c                   exsr      TipoLDV
010530080919      /free
010540080919
010550110427       //?»»»?Verifiche per chiusura pgm?«««?
010560080919       SELECT;
010570080919
010580110427         // -?Chiude SE richiamato per la sola verifica del modulo?
010590110427         //  ?(dal pgm FNLSE6R1 - cl FNLSE6C1)?
010600080919         WHEN  DB0ris  = 'R'   and
010610080919               DB0fl9  = *on   and
010620080919               DUTlpo  = *blanks;
010630080919           exsr  sr_End;
010640080919
010650110427         // -?Chiude SE richiesta la stampa in formato NON corrispondente?
010660080926
010670110427         // ·?Richiesta stampa di A4, mentre risulta di ex-A5?
010680110427         //  ?NON GESTITO:?rimane il richiamo "standard" dei pgm. di?
010690110427         //  ?manutenzione => 1 A4 x ogni LdV (anche se ex-A5)?
010700080919         //WHEN  DB0fa4  = '1'   and
010710080919         //      *in69   = *on;
010720080919         //  DB0tms = 'E';
010730080919         //  exsr  sr_End;
010740080919
010750110427         // -?Richiesta stampa di ex-A5, mentre risulta di A4?
010760080919         WHEN  DB0fa4  = '2'   and
010770080919               *in69   = *off;
010780080919           DB0tms = 'D';
010790080919           exsr  sr_End;
010800080919
010810080919       ENDSL;
010820080919
010830080919      /end-free
010840110427
010850110427       // -?Imposta i campi di stampa con quelli della DS?
010860030515     c                   exsr      PrepPRT
010870110427
010880110427       // -?Operazioni "inutili"?SE?richiesta la stampa della sola?
010890110427       //  ?packing-list:?
010900080919     c                   IF        DB0fa4 <> 'P'
010910110427
010920110427       // -?Decodifica i dati della LDV?
010930040607     c                   exsr      DecodLDV
010940110427
010950080919     c                   ENDIF
010960110427
010970110427       // -?Stampa della Lettera di Vettura?
010980030515sel 1c                   SELECT
010990110427       // -?Se RIMESSO MITTENTE stampo il numero di copie richiesto?
011000110427       //  ?(non per DPD e FedEx)?
011010110427       //  ?(le stampo qui perché devo stampare il riferimento numerico?
011020110427       //  ?aggiornato di volta in volta su FLNUF)?
011030030515w   1c                   WHEN      DB0ris = 'M'  and  *in70
011040030515     c                   clear                   wCOPIE
011050030515     c                   clear                   NUFaaa
011060030515     c                   eval      NUFcnu = 20
011070030515     c                   eval      NUFfil = DB0lnp
011080030515     c                   clear                   NUFfl2
011090030515do  2c                   DO        Wctr
011100030515     c     K04NUF        chain     FLNUF01L
011110030515if  3c                   if        %found(FLNUF01L)
011120030515     c                   add       1             NUFnum
011130030515     c                   UPDATE    FLNUF
011140030515     c                   eval      Prmn = NUFnum
011150040607     c                   exsr      StampaLDV
011160030515e   3c                   endif
011170030515e   2c                   ENDDO
011180110427       // -?Se FATTURA PREPAGATA stampo due copie?
011190110427       //  ?(solo se non attivata procedura O.R.M.)?
011200030515w   1c                   WHEN      DB0tbo = '8'  and  §OGorm <> 'S'
011210030515     c                   eval      wCOPIE = 2
011220110427       // -?Altrimenti stampo solo una copia?
011230030515x   1c                   OTHER
011240030515     c                   eval      wCOPIE = 1
011250030515e   1c                   ENDSL
011260110427
011270030515do  1c                   do        wCOPIE
011280080918      /free
011290080918         if  DB0fa4 = 'P';
011300130315           exsr  StampaPkgLst;
011310080918         else;
011320110427           exsr  StampaLDV;
011330080918         endif;
011340080918      /end-free
011350030515e   1c                   enddo
011360130306
011370130306       // -?Eventuale stampa immagini PDF?
011380130306       //  ?(si spera della sola spedizione di cui si è appena?
011390130306       //   ?stampata la LdV)?
011400130306     c                   if        %parms() > 1  and  DB6pdf = 'X'
011410130306     c                                           and  DB6num > *zero
011420130306     c                   exsr      sr_PrtPDF
011430130306     c                   endif
011440110427
011450110427       // -?Operazioni finali?
011460110427     c     Fine          tag
011470080624     c                   exsr      sr_End
011480110427
011490030515      *---------------------------------------------------------------*
011500050217      *?OPERAZIONI INIZIALI                                          ?*
011510030515      *---------------------------------------------------------------*
011520030515     c     *InzSR        BEGSR
011530131023      *
011540131023      * Reperimento dati utente
011550131023      * Reperisco nome azienda per stampa PREPAGATI (e non solo)
011560131023     c     *dtaara       define    §azute        AZUTEds
011570131023     c     *dtaara       define    §datiute      dDATIUTE
011580131023     c                   in(E)     *dtaara
011590131023     c                   if        %ERROR or RSUT = *blanks
011600131023     c                   clear                   Tibs34Ds
011610131023     c                   call      'TIBS34R'
011620131023     c                   parm                    Tibs34Ds
011630131023     c                   in        *dtaara
011640131023     c                   endif
011650131023      * Impostazione dati in KPJBA
011660131023     c                   eval      KNMUS  = SDSusr
011670110429      *
011680131023      *USCITA? SE  richiamato per la sola chiusura
011690130314     c*//                if        %subst(KPJBU:1:1) = 'C'
011700130314     c                   if        DB0tla = 'C'
011710110429     c                   leavesr
011720110429     c                   endif
011730070424      *
011740070424      * Reperisco la data del giorno
011750070424     c                   time                    w0140
011760070424     c                   move      w0140         w0080
011770070424     c     *eur          movel     w0080         Data_EUR
011780070424     c     *iso          movel     Data_EUR      w0080
011790070424     c                   move      w0080         wDateA
011800071105     c                   eval      wDate  = w0080
011810030515      *
011820030515      * Imposto i campi chiave "fissi"
011830040223     c                   eval      TBLkut = 1
011840060505     c                   eval      ARStrc = 'C'
011850070319     c                   eval      ARNcdn = 'LDV'
011860070319     c                   eval      ARNgst = 'S'
011870030515      *
011880030515      * Carico le varie esenti IVA
011890030515     c                   movel     'CC'          TBLcod
011900030515     c                   movel(p)  'VARIE'       TBLkey
011910030515     c                   clear                   XX
011920030515     c     K03TAB        setll     TABEL
011930030515     c     K02TAB        reade     TABEL
011940030515      *
011950070306do  1c                   DOW       NOT %eof(TABEL00F)
011960030515      * Deve essere NON annullata e con codice che inizia con "VARIE"
011970030515if  2c                   If        %subst(TBLkey:1:5) = 'VARIE'
011980030515     c                             and    TBLflg      = *blanks
011990030515     c                   movel     TBLuni        dsCC
012000030515if  3c                   if        §CCcei <> *blanks
012010030515     c                   add       1             xx
012020030515     c                   move      TBLkey        $Var(xx)
012030030515e   3c                   endif
012040030515e   2c                   EndIf
012050030515      *
012060030515     c     K02TAB        reade     TABEL
012070030515e   1c                   ENDDO
012080070306      *
012090070308      * Carico le causali eventi con tipo consegna = "G"
012100070306     c                   movel     '2A'          TBLcod
012110070306     c                   clear                   TBLkey
012120070306     c                   clear                   XX
012130070306     c     K03TAB        setll     TABEL
012140070306     c     K02TAB        reade     TABEL
012150070306      *
012160070306do  1c                   DOW       NOT %eof(TABEL00F)
012170070308      * Deve essere NON annullata e con flag §2AFTC = "G"
012180070308if  2c                   If        TBLflg = *blanks
012190070306     c                   movel     TBLuni        ds2A
012200070308if  3c                   if        §2Aftc = 'G'
012210070306     c                   add       1             xx
012220070308     c                   movel     TBLkey        $2Acev_G(xx)
012230070306e   3c                   endif
012240070306e   2c                   EndIf
012250070306      *
012260070306     c     K02TAB        reade     TABEL
012270070306e   1c                   ENDDO
012280030515      *
012290030515      * Carico tabella VARIABILI ARRIVI
012300030515     c                   clear                   ds$2
012310030515     c                   eval      TBLcod = '$2'
012320030515     c                   eval      TBLkey = '1'
012330030515     c     K03TAB        chain     TABEL
012340030515if  1c                   if        %found(TABEL00F)
012350030515     c                   movel     TBLuni        ds$2
012360030515e   1c                   endif
012370030515      *
012380030515      * Reperisco la moneta di conto
012390030515     c                   clear                   dGED
012400040607     c                   clear                   TIBS02ds
012410040607     c                   eval      T02mod = 'C'
012420040607     c                   eval      T02cod = 'GED'
012430040607     c                   eval      T02ke1 = '1         '
012440030515     c                   call      'TIBS02R'
012450030515     c                   parm                    KPJBA
012460040607     c                   parm                    TIBS02ds
012470030515if  1c                   if        T02err = *blanks
012480030515     c                   movel     T02uni        dGED
012490030515e   1c                   endif
012500030515      *
012510030515      * Con moneta di conto:
012520060301      *   Carico tabella GESTIONALE - IMPORTI VARI   e
012530030515      *   Reperisco l'importo limite per abilitazione c/a
012540030515     c                   clear                   dGEI
012550040607     c                   clear                   TIBS02ds
012560040607     c                   eval      T02mod = 'C'
012570040607     c                   eval      T02cod = 'GEI'
012580040607     c                   eval      T02ke1 = §GEDcn
012590030515     c                   call      'TIBS02R'
012600030515     c                   parm                    KPJBA
012610040607     c                   parm                    TIBS02ds
012620030515if  1c                   if        T02err = *blanks
012630030515     c                   movel     T02uni        dGEI
012640030515e   1c                   endif
012650061031      *
012660110426      * Reperisco data di decorrenza stampa "Per ricevuta di ...
012670071005      *                                      senza riserve"
012680090615      *              e di decorrenza stampa per C.L.N.
012690061031     c                   clear                   dVPOdeco
012700061031     c                   clear                   TIBS02ds
012710061031     c                   eval      T02mod = 'C'
012720071112     c***                eval      T02sif  = KNSIF
012730071112     c                   eval      T02cod = 'VPO'
012740061031     c                   eval      T02ke1 = 'DECO      '
012750061031     c                   call      'TIBS02R'
012760061031     c                   parm                    KPJBA
012770061031     c                   parm                    TIBS02ds
012780061031if  1c                   if        T02err = *blanks
012790061031     c                   movel     T02uni        dVPOdeco
012800061031e   1c                   endif
012810120530      *
012820120530      *//*?Reperisco data di decorrenza stampa nuovo logo BRT?
012830120530     c*//                clear                   dVPObrt
012840120530     c*//                clear                   TIBS02ds
012850120530     c*//                eval      T02mod = 'C'
012860120530     c***                eval      T02sif  = KNSIF
012870120530     c*//                eval      T02cod = 'VPO'
012880120530     c*//                eval      T02ke1 = 'BRT       '
012890120530     c*//                call      'TIBS02R'
012900120530     c*//                parm                    KPJBA
012910120530     c*//                parm                    TIBS02ds
012920120530if  1c*//                if        T02err = *blanks
012930120530     c*//                movel     T02uni        dVPObrt
012940120530e   1c*//                endif
012950080708      /free
012960110426         // -?Override al file di stampa?
012970081017         if  DB0tla <> 'C';
012980080709           fnlsB8r ( fnlsB5ds );
012990080708         endif;
013000080708      /end-free
013010030515      *
013020030515      * Apro i file di stampa
013030130710     c                   open      FNLSB6P
013040080918     c                   clear                   CurLineA4
013050130306      /free
013060130306         // -?Reperimento override apportate al file di stampa?
013070130306         //  ?per comunicarle per la stampa immagini PDF?
013080130306         if  DB0tla <> 'C';
013090130306           wPrtF = 'FNLSB6P';
013100130306           clear  trulPOVRds;
013110130307           trulPOVRc ( wPrtF : trulPOVRds);
013120130307           if trulPOVRds.§ULPoerr <> *blank;  //?Nessuna override rilevata?
013130130321             //trulPOVRds.§ULPoOutQ  = '*JOB      ';
013140130321             trulPOVRds.§ULPoOutQ  = 'PRT01     ';
013150130307             trulPOVRds.§ULPoFType = '*LDVUNI   ';
013160130307             trulPOVRds.§ULPoUDta  = 'LDV_UNI   ';
013170130306           endif;
013180130306         endif;
013190130306      /end-free
013200030515      *
013210030515     c                   ENDSR
013220071112      *
013230071112      *---------------------------------------------------------------*
013240071112      *?REPERIMENTO N° MAX DI LDV STAMPABILI DA TAB. "VPO"           ?*
013250071112      *---------------------------------------------------------------*
013260071112     c     Rep_TabVPO    BEGSR
013270071112      *
013280071112      * Aggancio tabella "VPO" per reperimento del n° LdV stampabili
013290071112      *   limite (per congelamento e blocco stampa)
013300071112     c                   clear                   dVPO
013310071112     c                   clear                   TIBS02ds
013320071112     c                   eval      T02mod  = 'C'
013330071112     c***                eval      T02sif  = KNSIF
013340071112     c                   eval      T02cod  = 'VPO'
013350071112     c                   movel(P)  'VPO'         T02ke1
013360071112     c                   call      'TIBS02R'
013370071112     c                   parm                    KPJBA
013380071112     c                   parm                    TIBS02ds
013390071112     c                   if        T02err  = *blanks
013400071112     c                   movel     T02uni        dVPO
013410071203     c                   if        §VPOMaxLdv = *zeros
013420071203     c                   eval      §VPOMaxLdv = 10000
013430071203     c                   endif
013440071112     c                   else
013450071112     c                   eval      §VPOMaxLdv = 10000
013460071112     c                   endif
013470071112      *
013480071112     c                   ENDSR
013490070123      *
013500070123      *---------------------------------------------------------------*
013510070306      *?ALTRE OPERAZIONI INIZIALI (NON PIÙ IN *INZSR)                ?*
013520070123      *---------------------------------------------------------------*
013530070123     c     sr_Start      BEGSR
013540070123      *
013550080709      /free
013560081017         select;
013570081017           when  DB0fa4 = *blank;
013580081017             DB0fa4 = '1';
013590081020           when  DB0fa4 = 'P';
013600081017             DB0fl6 = *on;
013610081017         endsl;
013620080709      /end-free
013630080709      *
013640070123if  1c                   if        DB0fl7 = '1'
013650130710     c                   close     FNLSB6P
013660130710     c                   open      FNLSB6P
013670081015     c                   clear                   CurLineA4
013680070123     c                   clear                   Page4
013690070123e   1c                   endif
013700070123      *
013710070123      * »»» CHIODO: SE richiamato per la sola verifica del modulo «««
013720070308      *     RIPULISCO il "livello della filiale" (S/1/2) perchè:
013730070123      *     1) sarò partito dal S.I. di sede, ma
013740070123      *     2) sarà stata cambiata la lista di librerie come x S.I. di
013750070308      *        filiale (dal CL FNLSE6C1)
013760070123if  1c                   if             DB0ris  = 'R'
013770070123     c                             and  DB0fl9  = *on
013780070123     c                             and  DUTlpo <> *blanks
013790070123     c                   clear                   DUTlpo
013800070123e   1c                   endif
013810070123      *
013820070308      * Apertura archivi di sede o filiale - comunque utilizzati
013830070123if  1c                   if        DUTlpo = 'S'
013840070123if  2c                   if        NOT  %open(FIAR531C)
013850070123     c                   open      FIAR531C
013860070123e   2c                   endif
013870070123x   1c                   else
013880070123if  2c                   if        NOT  %open(FIAR401L)
013890070123     c                   open      FIAR401L
013900070123e   2c                   endif
013910070123if  2c                   if        NOT  %open(FIAR501L)
013920070123     c                   open      FIAR501L
013930070123e   2c                   endif
013940070123if  2c                   if        NOT  %open(FLNUF01L)
013950070123     c                   open      FLNUF01L
013960070123e   2c                   endif
013970070123e   1c                   endif
013980110427      *
013990110427      * Pulizia del PRTF
014000110427     c                   exsr      ClrPRTF
014010070123      *
014020070123     c                   clear                   wCOPIE
014030070123      *
014040070123     c                   ENDSR
014050080624      *
014060080624      *---------------------------------------------------------------*
014070080624      *?OPERAZIONI FINALI                                            ?*
014080080624      *---------------------------------------------------------------*
014090080624     c     sr_End        BEGSR
014100080624      *
014110080624      * Chiusura pgm
014120080919if  1c                   IF        DB0tla <> *blanks
014130130710     c                   close     FNLSB6P
014140080708      /free
014150080708       Qcmd = C_CmdDltOvr;
014160080708       callp(e) qCmdExc(Qcmd : %size(Qcmd));
014170080708      /end-free
014180080624     c                   clear                   TIBS02ds
014190080624     c                   eval      T02tla = 'C'
014200080624     c                   call      'TIBS02R'
014210080624     c                   parm                    KPJBA
014220080624     c                   parm                    TIBS02ds
014230080624     c                   reset                   FNLV80ds
014240080624     c                   call      'FNLV80R'
014250080624     c                   parm                    KPJBA
014260080624     c                   parm                    FNLV80ds
014270080624     c                   clear                   FIDN12ds
014280080624     c                   eval      I12tla = 'C'
014290080624     c                   call      'FIDN12R'
014300080624     c                   parm                    FIDN12ds
014310090605      /free
014320090622           if  §VPOrcm <= wDate   and   DUTlpo  <> 'S';
014330090615             reset  TNSD99ds;
014340090615             tnsd99r ( TNSD99ds );
014350090615           endif;
014360100322           if  §VPOcas <= wDate   and   DUTlpo  <> 'S';
014370100208             clear  trulINTds;
014380100208             iINTtla = 'C';
014390100208             trulINTr ( TRULINTds );
014400100322           endif;
014410090605      /end-free
014420080624     c                   eval      *inLR  = *on
014430080624e   1c                   ENDIF
014440080919      *
014450080919      * Ritorno al chiamante
014460080919     c                   return
014470080624      *
014480080624     c                   ENDSR
014490030515      *
014500030515      *---------------------------------------------------------------*
014510050217      *?Pulisco i campi di stampa                                    ?*
014520030515      *---------------------------------------------------------------*
014530030515     c     ClrPRTF       BEGSR
014540030515      *
014550130418     c                   clear                   A4prtBOX
014560120530     c*//                clear                   A4LogoBAR
014570120530     c*//                clear                   A4LogoDA1
014580120530     c*//                clear                   A4LogoBR2
014590120530     c*//                clear                   A4LogoDA2
014600120530     c                   clear                   A4LogoBRT
014610120530     c                   clear                   A4LogoDA3
014620130418     c                   clear                   A4prtLDV
014630080604     c                   clear                   A4prtLDVB
014640080922     c                   clear                   A4prtDPD
014650030515     c                   clear                   A4prtFED1
014660030515     c                   clear                   A4prtFED2
014670030515     c                   clear                   A4prtFED3
014680030515     c                   clear                   A4prtAR51
014690030515     c                   clear                   A4prtAR52
014700030515     c                   clear                   A4prtAR53
014710030909     c                   clear                   A4prtAR5A
014720040607     c*** stessi campi:  clear                   A4prtAR5B
014730040607     c*** stessi campi:  clear                   A4prtAR5C
014740070321     c*** stessi campi:  clear                   A4prtAR5G
014750040311     c*** no campi:      clear                   A4prtSP0
014760040607     c*** no campi:      clear                   A4prtSP1
014770080709     c*** no campi:      clear                   A4prtSP2
014780040311     c*** no campi:      clear                   A4prtNUL
014790030911     c                   clear                   A4prtGEN
014800040607     c                   clear                   A4prtDK0
014810040607     c                   clear                   A4prtDK1
014820040607     c                   clear                   A4prtDK2
014830040607     c                   clear                   A4prtDK3
014840040607     c                   clear                   A4prtDK4
014850040607     c                   clear                   A4prtDK5
014860081222     c                   clear                   A4prtDLL
014870041019     c                   clear                   A4prtDL0
014880050318     c                   clear                   A4prtDL0A
014890050318     c                   clear                   A4prtDL0B
014900050318     c                   clear                   A4prtDL0C
014910050606     c                   clear                   A4prtDL0D
014920050606     c                   clear                   A4prtDL0E
014930050606     c                   clear                   A4prtDL0F
014940041028     c                   clear                   A4prtDL1A
014950041028     c                   clear                   A4prtDL1B
014960050318     c                   clear                   A4prtDL1C
014970050606     c                   clear                   A4prtDL1D
014980050606     c                   clear                   A4prtDL1E
014990041019     c                   clear                   A4prtDL2
015000060614     c                   clear                   A4prtDL21
015010041019     c                   clear                   A4prtDL3
015020041019     c                   clear                   A4prtDL4
015030050318     c                   clear                   A4prtDL5
015040050606     c                   clear                   A4prtDL6
015050050606     c                   clear                   A4prtDL7
015060050606     c                   clear                   A4prtDL8
015070050606     c                   clear                   A4prtDL9
015080050114     c                   clear                   A4prtAC0
015090050114     c                   clear                   A4prtAC1A
015100050114     c                   clear                   A4prtAC1B
015110050114     c                   clear                   A4prtAC2
015120050114     c                   clear                   A4prtAC3
015130050114     c                   clear                   A4prtAC4
015140050214     c                   clear                   A4prtES0
015150050214     c                   clear                   A4prtES0A
015160050214     c                   clear                   A4prtES0B
015170050215     c                   clear                   A4prtES1A
015180050215     c                   clear                   A4prtES1B
015190050214     c                   clear                   A4prtES2
015200050214     c                   clear                   A4prtES3
015210050215     c                   clear                   A4prtES4
015220050214     c                   clear                   A4prtDB0
015230041207     c                   clear                   A4prtDT0
015240040607     c                   clear                   A4prtDT1
015250040607     c*** stessi campi:  clear                   A4prtDT2
015260040607     c*** stessi campi:  clear                   A4prtDC1
015270030515     c                   clear                   A4prtSEG
015280030515     c                   clear                   A4prtMSG
015290030515      *
015300041207     c                   reset                   dAR8
015310060524     c                   clear                   dsBL4A
015320061030     c                   clear                   dsBL4I
015330061108     c                   clear                   dTA4Q
015340100514     c                   clear                   dOrm01
015350041207      *
015360070514     c                   reset                   $Mamma
015370130215     c                   reset                   $Mamma_FedEx
015380070306     c                   reset                   $FIARN
015390060921     c                   reset                   $FIAR5
015400030515     c                   reset                   $First
015410030515     c                   reset                   $FirstSCR
015420060921     c                   reset                   $FIAR8
015430130306     c                   reset                   $§PDF
015440040608     c                   reset                   $DDT_o
015450040608     c                   reset                   $DDT_p
015460040608     c                   reset                   $DDTlastPg
015470030515     c                   reset                   $Partic
015480030710     c                   reset                   $NoDesPartic
015490030910     c                   reset                   $BolloVirt
015500061106     c                   reset                   $No_CF
015510071114     c                   reset                   $No_PI
015520040707     c                   reset                   $No_x_Cons
015530101125     c                   reset                   $Rit_A_Inc
015540050513     c                   reset                   $A4_x_ScNS
015550070306     c                   reset                   $LascAvv
015560070319     c                   reset                   $ConsParz
015570070315     c                   reset                   $C_A_
015580030515     c                   reset                   $CAeur
015590090302     c                   reset                   $TabPXA
015600110411     c                   reset                   $TabPXA2
015610090924     c                   reset                   $BoxNxD
015620100513     c                   reset                   $PreavvORM
015630110427     c                   reset                   $GeoAtt
015640070717     c                   clear                   wGeoTel
015650070717     c                   clear                   wGeoCon
015660070717     c                   clear                   Wdsp
015670090527     c                   clear                   WddtCLN
015680090527     c                   clear                   WdspCLN
015690030717     c                   clear                   W_CoOr
015700040608     c                   clear                   wOmaRowT
015710040608     c                   clear                   wOmaRowD
015720050624     c                   clear                   wBolloVirt
015730030515     c                   clear                   Wcas
015740030515     c                   clear                   Wiva
015750071015     c                   clear                   Wpdc4a
015760080926     c                   clear                   wPage
015770080704     c                   reset                   $LdVxP
015780090121     c*** (da non fare:) clear                   wNumLdV
015790090121     c*** (da non fare:) clear                   wSeqLdV
015800071114     c                   clear                   wB0cfi
015810071114     c                   clear                   wB0piv
015820031015     c                   clear                   wB0nrs
015830070514     c                   clear                   wB0ndc
015840070514     c                   clear                   wB0ntc
015850070514     c                   clear                   wB0dcp
015860081119     c                   clear                   wB0tmc
015870101125     c                   clear                   wB0dcm
015880101125     c                   clear                   wB0icc
015890101125     c                   clear                   wB0ica
015900130124     c                   clear                   wB0fiv
015910131205     c                   clear                   wEVBdev
015920131205     c                   clear                   wEVBhev
015930030515      *
015940030515     c                   clear                   $NP
015950030515     c                   clear                   $SC1
015960030515     c                   clear                   $SC2
015970030905     c                   clear                   $QUM
015980040607     c                   clear                   $AR8o1
015990070319     c                   reset                   $CP
016000070319     c                   reset                   $CPdcm
016010030515      *
016020030515     c                   movea     *zeros        *in(01)
016030030515      *
016040030515     c                   ENDSR
016050030515      *
016060030515      *---------------------------------------------------------------*
016070050217      *?Determino il modulo LDV da usare per la stampa               ?*
016080120530      *?(se A5 o A4 e se DPD, FedEx o BRT)                           ?*
016090030515      *---------------------------------------------------------------*
016100040607     c     TipoLDV       BEGSR
016110070212      *
016120080703      * Impostazione ds "Flag operativi DB0FLO di FNLSB5DS"
016130070215     c                   movel     DB0flo        dDB0flo
016140070215      *
016150070212      * Impostazione della data spedizione
016160070212      * (se non già impostata in subr. "CarBLP" o "CarARB")
016170070212if  1c                   if        Wdsp   =  *zeros
016180070212if  2c                   if        DB0mgs <> *zeros
016190070212     c                   eval      Wdsp   =  (DB0aas * 10000) + DB0mgs
016200070212x   2c                   else
016210070212      *   prendo la data del giorno se non c'è la data spedizione
016220070212     c                   time                    w0140
016230070212     c                   move      w0140         w0080
016240070212     c     *eur          movel     w0080         Data_EUR
016250070212     c     *iso          movel     Data_EUR      Wdsp
016260070212e   2c                   endif
016270070212e   1c                   endif
016280040311      *
016290040311      * Dati della linea di partenza
016300040311if  1c                   if        DB0lnp <> SAVlnp
016310040311     c                   eval      SAVlnp =  DB0lnp
016320040311     c     DB0lnp        chain     AZORG
016330040311if  2c                   if        %found(AZORG01L)
016340040311     c                   movel     ORGde3        OG143
016350040311     c                   movel     ORGde8        OG148
016360040311x   2c                   else
016370040311     c                   clear                   OG143
016380040311     c                   clear                   OG148
016390040311e   2c                   endif
016400040311e   1c                   endif
016410040607      *
016420070308      * Dati della filiale del mittente
016430040607     c                   eval      WX     =  DB0ccm / 10000
016440040607if  1c                   if        WX     <> ORGfil
016450040607     c     WX            chain     AZORG
016460040607e   1c                   endif
016470040607if  1c                   if        %found(AZORG01L)
016480040607     c                   movel     ORGde3        OG143ccm
016490040607x   1c                   else
016500040607     c                   clear                   OG143ccm
016510040607e   1c                   endif
016520040311      *
016530040311      * Dati della linea di arrivo
016540040311if  1c                   if        DB0lna <> SAVlna
016550040311     c     DB0lna        chain     AZORG
016560040311if  2c                   if        %found(AZORG01L)
016570070312     c                   movel     ORGde6        OG146arr
016580061013     c                   movel     ORGde8        OG148arr
016590070312     c                   movel     ORGdf0        OG150arr
016600040311x   2c                   else
016610070312     c                   clear                   OG146arr
016620061013     c                   clear                   OG148arr
016630070312     c                   clear                   OG150arr
016640040311e   2c                   endif
016650050510e   1c                   endif
016660070212      *
016670070212      * Dati della capofila della linea di arrivo
016680070306      * - per verificare TELLUS   e
016690070306      * - per non stampare la "copia per il destinatario" nel caso
016700070306      *   di precedente "Lasciato Avviso"
016710070717      *
016720070717      *** Imposta flag attivazione GEO x LNA
016730070717     c                   eval      wGeoCon = A§OGtlls
016740070717     c                   eval      wGeoTel = A§OGgeot
016750070717if  1c                   if             A§OGdda  > *zeros
016760070717     c                             and  A§OGdda  < C_DateMax_A
016770070717     c                             and  A§OGdda <= wDateA
016780110427     c                   eval      $GeoAtt = *on
016790070717e   1c                   endif
016800070717      *
016810070717if  1c                   IF        DB0tbo = 'A'  AND
016820070717     c                                 (wGeoCon <> 'S'
016830070717     c                              or  wGeoTel <> 'S'
016840110427     c                              or  $GeoAtt <> *on)
016850070212      * - Reperimento capofila (£6)
016860070212if  2c                   if        DB0lna <> D55lin
016870070212     c                   reset                   FNLV55ds
016880070212     c*** già così:      eval      D55tpt =  '6'
016890070212     c                   eval      D55lin =  DB0lna
016900070212     c                   eval      D55drf =  Wdsp
016910070212     c                   call      'FNLV55R'
016920070212     c                   parm                    FNLV55ds
016930070212if  3c                   if        D55err <> *blanks
016940070212     c                   eval      D55tfa =  DB0lna
016950070212e   3c                   endif
016960070212e   2c                   endif
016970070321      * - Reperimento 146ª e 148ª descrizioni dell'organigramma
016980070306      *   (flag Tellus del capofila e flag gestione telefonate GEO)
016990070212if  2c                   if        D55tfa <> ORGfil
017000070212     c     D55tfa        chain     AZORG
017010071109if  3c                   if        %found(AZORG01L)
017020070717     c                   movel     ORGde6        OG146cf£6
017030070717     c                   movel     ORGde8        OG148cf£6
017040071109x   3c                   else
017050071109     c                   clear                   OG146cf£6
017060071109     c                   clear                   OG148cf£6
017070071109e   3c                   endif
017080071109e   2c                   endif
017090070717      *** Imposta flag attivazione GEO x capofila £6
017100070717     c                   eval      wGeoCon = £6OGtlls
017110070717     c                   eval      wGeoTel = £6OGgeot
017120071109if  2c                   if             £6OGdda  > *zeros
017130070717     c                             and  £6OGdda  < C_DateMax_A
017140070717     c                             and  £6OGdda <= wDateA
017150110427     c                   eval      $GeoAtt = *on
017160071109e   2c                   endif
017170070717      *
017180070212e   1c                   ENDIF
017190050510      *
017200050510      * Verifica se richiesta stampa in A4 in tab. "7R"
017210050510      *   (gestione particolarità consegna)
017220050510     c                   clear                   ds7R
017230050510if  2c                   if        DB0gma <> *blanks
017240070312     c                   eval      TBLcod =  '7R'
017250050510     c                   movel(p)  DB0gma        TBLkey
017260050510     c     K03TAB        chain     TABEL
017270050510if  2c                   if        %found(TABEL00F)
017280050510     c                   movel     TBLuni        ds7R
017290050510e   2c                   endif
017300050510e   1c                   endif
017310071015      *
017320071015      * Verifica se LDV con "Ritiro Porto Franco Prepagato"
017330071015     c                   clear                   dsBL4M
017340071015     c                   if        DB0tbo = '8'
017350071015     c                   exsr      RepAR4_M
017360071015     c                   endif
017370130315      *
017380130315      * Reperimento dati da FIAR5 / "GEN"
017390130315      *   (per verificare se esistono Packing-List e/o PDF allegati
017400130315      *    - oltre che per reperire Nominativo e Telefono)
017410130315     c                   exsr      RepAR5gen
017420040607      *
017430130315      * Verifica se LDV con DDT e/o PDF
017440130315     c                   exsr      CercaDDToPDF
017450061031      *
017460071114      * Verifica se richiesta stampa in A4 per richiesta Codice Fiscale
017470071114      *   e/o Partita IVA al cliente.
017480120503      *-=»>?01/08/2008:?                                          <«=-*
017490080801      *-=»> Abolite richieste di Codice Fiscale e/o Partita Iva.  <«=-*
017500080801      *-=»> Tolta ricerca di 'sti 2 dati, lasciando il defualt di <«=-*
017510080801      *-=»> NON richiesta: $No_CF = *off e $No_PI = *off.         <«=-*
017520120503      *-=»>?03/05/2012:?                                          <«=-*
017530120503      *-=»>?Ripristinate richieste di Cod. Fiscale e/o Part. Iva.?<«=-*
017540120503if  1c                   if        DUTlpo <> 'S'
017550120503     c                   exsr      CheckCFPI
017560120503e   1c                   endif
017570070306      *
017580070319      * Verifica se richiesta stampa in A4 per note in FIARN00F
017590070306      *   (da stampare nel box "Note per l'autotrasportatore")
017600070319      * » se stampa in arrivo
017610070319if  1c                   if        DB0tbo = 'A'
017620070306     c                   exsr      CheckFIARN
017630070319e   1c                   endif
017640090302      /free
017650090302
017660090302         // Verifica se richiesta stampa in A4 per note specifiche
017670090302         //   per il cliente mittente (v. tab. "PXA")
017680090302         //   (da stampare nel box "Note per l'autotrasportatore")
017690090302         exsr  CheckTabPXA;
017700090924
017710090924         // Verifica se richiesta stampa in A4 per note specifiche
017720090924         //   per il destinatario (da parte del mittente)
017730090924         //   (da stampare in un box a parte)
017740090924         if  DB0ccm = c_CliSG;
017750090924           $BoxNxD = *on;
017760090924         endif;
017770090302      /end-free
017780070319      *
017790070319      * Verifica se richiesta spedizione già parzialmente consegnata
017800070319      *   (da stampare nel box "Note per l'autotrasportatore")
017810070717      * » se stampa in arrivo e LNA - o capofila £6 - con GEO attivato
017820070717      * completamente
017830070319if  1c                   if             DB0tbo   =  'A'
017840070514     c                             and  wB0dcp   >  *zeros
017850110427     c                             and  $GeoAtt  =  *on
017860140514      * -?Reperimento consegne parziali precedenti da FNART?
017870140514     c                   exsr      SchConsParz
017880140514     c                   if        xx > *zero
017890070413     c                   eval      $ConsParz     =  *on
017900140514     c                   endif
017910070319e   1c                   endif
017920070315      *
017930070315      * Verifica se richiesta stampa in A4 per C.A. aperta
017940070717      * » se stampa in arrivo e LNA - o capofila £6 - con GEO attivato
017950070717      * completamente
017960070319if  1c                   if             DB0tbo   =  'A'
017970110427     c                             and  $GeoAtt  =  *on
017980070316     c                   exsr      CercaCA
017990070316e   1c                   endif
018000030515      *
018010030515      * Tipo modulo (per la separazione dei box: vettore/destinatario)
018020030515      *   (*in69 = formato A5 / not *in69 = formato A4)
018030030515     c                   eval      *in69  = *on
018040030515      *
018050040607      * Tipo LDV (per richiesta firma per ricevuta della merce senza
018060040607      *   riserve)
018070120530      *   (70 = BRT, 71 = DPD, 72 = FedEx, 73 = EuroExpress)
018080080912     c                   movea     '1000'        *in(70)
018090081210      /free
018100081210         // - LDV DPD
018110170517         //if  §OGntw = 'DPD'   -  ?NON della LnP, MA della fil. mitt.:?
018120081210         if  §CMntw = 'DPD';
018130081210           *in71 = *on;
018140081210         endif;
018150081210      /end-free
018160040311      *
018170030515sel 1c                   SELECT
018180030515      *
018190030515      * Rimesso Mittente: va bene il formato A5
018200030515      *   (se non richiesta la forzatura del formato "A4")
018210040707w   1c                   WHEN      DB0ris = 'M'   and  DB0fa4 <> 'S'
018220040707      *
018230040707      * LDV POSTE non valida per la consegna: va bene il formato A5
018240040707      *   (se non richiesta la forzatura del formato "A4")
018250040707w   1c                   WHEN      §OGntw = 'PPT' and  DB0fa4 <> 'S'
018260071213      *
018270081210     *** * LDV DPD
018280081210     *** > (Spostato prima per NON rendere incompatibile DPD con il box
018290081210     *** >  di note aggiuntive "promemoria per l'autotrasportatore")
018300081210w   1***c*** non della LNP  WHEN      §OGntw = 'DPD' - ma della fil. mitt.:
018310081210w   1***c                   WHEN      §CMntw = 'DPD'
018320081210     ***c                   movea     '0100'        *in(70)
018330071213      *
018340071213      * LDV con mittente FedEx
018350150429w   1c                   WHEN      DB0ccm = c_CliFedEx   or
018360150429     c                             DB0ccm = c_CliFedEx2
018370080922     c                   movea     '0010'        *in(70)
018380071213     c                   eval      *in69  = *off
018390040607      *
018400040607      * LDV con DDT del mittente
018410081013      * - IN FUTURO: se al 1° tentativo di consegna
018420081013      *              (DB0FL1 = "D" - v. pgm FNLRS9R)
018430080917w   1c                   WHEN           $FIAR8 =  *on
018440081013     c***-=»>                      and  DB0fl1 <> 'D'
018450040607     c                   eval      *in69  = *off
018460061102      *
018470071114      * Codice Fiscale o Partita IVA da richiedere al cliente
018480071114w   1c                   WHEN           $No_CF = *on
018490071114     c                             or   $No_PI = *on
018500061102     c                   eval      *in69  = *off
018510070306      *
018520070306      * LDV con Note (dal file FIARN00F)
018530070306w   1c                   WHEN      $FIARN = *on
018540070306     c                   eval      *in69  = *off
018550080801      *
018560080801      * LDV già parzialmente consegnata
018570080801w   1c                   WHEN      $ConsParz   =  *on
018580080801     c                   eval      *in69  = *off
018590070315      *
018600070315      * LDV con C.A. aperta per la spedizione (dal file FNDCT00F)
018610070315w   1c                   WHEN      $C_A_  = *on
018620070315     c                   eval      *in69  = *off
018630060524      *
018640070123      * LDV con mittente "PraOil oleodotti italiani"
018650081127w   1c                   WHEN           DB0ccm = c_CliPraOil
018660060524     c                             or  (DB0fbr = 'S'
018670081127     c                             and  §B4Aks = c_CliPraOil)
018680060524     c                   eval      *in69  = *off
018690090302      /free
018700090302
018710090302           // LDV con mittente con promemoria per AUT in tab. "PXA"
018720110411           WHEN  $TabPXA = *on  or  $TabPXA2 = *on;
018730090302             *in69  = *off;
018740090924
018750090924           // LDV con mittente con promemoria per destinatario
018760090924           WHEN  $BoxNxD = *on;
018770090924             *in69  = *off;
018780090302      /end-free
018790030515      *
018800120530      * LDV BRT
018810150224     *** * - con Bancali da rendere al mittente
018820150224     *** *   o   Bancali da riportare a magazzino
018830150224      * - con Destinatario supermercato
018840030515      *   e/o Promemoria per autotrasportatore
018850040223      *   [Se non trovo niente su FIAR5 né x supermercati né x bancali
018860030515      *    ma ho queste condizioni: devo stampare la bolla in "A4" per
018870030515      *    poter stampare comunque il box vuoto con la memo "Destina-
018880050510      *    tario Supermercato" per poter scrivere a mano i promemoria
018890050510      *    per l'autotrasportatore - ci dovrebbero comunque sempre
018900050510      *    essere note per supermercato.]
018910050510      *   e/o con formato A4 previsto in tab. "7R" (partic. consegna)
018920150224w   1c*//                WHEN         %subst(DB0gva:1:1) = 'B'
018930150224     c*//                          or %subst(DB0gva:1:1) = 'O'
018940150224w   1c                   WHEN                DB0tc1      = 'S'
018950030717     c                             or        DB0tc2      = 'S'
018960071015     c                             or        §7Rldv      = 'S'
018970030515     c                   eval      *in69  = *off
018980071015      *
018990071015      * LDV con ritiro porto franco prepagato
019000071015w   1c                   WHEN          (DB0tbo   = '8'
019010071015     c                             and  §B4poe   > *zeros
019020071015     c                             and  §B4nor   > *zeros)
019030071015     c                   eval      *in69  = *off
019040071015      *
019050071015      * LDV con ritiro contestuale alla consegna
019060100608w   1c**!!!              WHEN          (DB0gma   = Ktab7V_RC
019070100608w   1c                   WHEN          (§7Rrc    = 'S'
019080071015     c                             and  §7Rldv   = 'S')
019090071015     c                   eval      *in69  = *off
019100030515      *
019110030515      * - con Forzatura richiesta al formato "A4"
019120030515w   1c                   WHEN      DB0fa4 = 'S'
019130030515     c                   eval      *in69  = *off
019140030515      *
019150030515e   1c                   ENDSL
019160050513      *
019170050513      * Verifica se con Segnacolli NON sequenziali
019180050513      * (di numero superiore a quelli che stanno al posto del range)
019190050513      * DA CONTROLLARE COMUNQUE (A4 o A5 che sia), VISTO CHE:
019200050513      * - se è previsto un A5, potrebbe scattare l'A4 (per numero
019210050513      *   di segnacolli da elencare troppo alto per lo spazio
019220050513      *   disponibile tra le "NOTE E PARTICOLARITA'")
019230050513      * - se è già previsto un A4, i segnacolli non sequenziali
019240050513      *   potrebbero comunque essere elencabili tra le "NOTE E
019250050513      *   PARTICOLARITA'" (e risparmiare una pagina in caso di
019260050513      *   LdV con DDT del mittente)
019270050513     c*** (già fatto:)   reset                   $A4_x_ScNS
019280050513if  1c                   IF             DB0fns = 'S'
019290050513     c                             and  DB0ncl >  2
019300050513      * - - imposto le righe di "Note e Particolarità" da stampare
019310050513     c                   exsr      SchPARTIC
019320050513      * - - controllo quante righe di "Note e Particolarità" servireb-
019330050513      *     bero per segnalare i segnacolli non sequenziali, in base
019340050513      *     al numero di colli (considerando che riuscirei ad elencare
019350050513      *     max 4 segnacolli per ogni riga di "Note e Particolarità")
019360050513     c                   eval      WelSNS = $elSNSxA5
019370050513     c     DB0ncl        div       WelSNS        YY
019380050513     c                   mvr                     XX
019390050513if  2c                   if        XX     >  0
019400050513     c                   eval      XX     =  YY + 1
019410050513x   2c                   else
019420050513     c                   eval      XX     =  YY
019430050513e   2c                   endif
019440050513      * - - conto anche la riga per l'intestazione
019450050513     c                   add       1             XX
019460050513      * - - conteggio le righe di "Note e Particolarità" utilizzate
019470050513      *     per reperire la 1ª riga di "Note e Particolarità" disponi-
019480050513      *     bile
019490050513     c                   eval      *in31  =  *off
019500050513     c                   eval      YY     =  1
019510050513     c     *blanks       lookup    $NP(yy)                                31
019520050513if  2c                   if        *in31  =  *on
019530050513     c                   eval      YY     =  YY - 1
019540050513x   2c                   else
019550050513     c                   eval      YY     =  %elem($NP)
019560050513e   2c                   endif
019570050513      * - - controllo se ho il numero di campi "Note e Particolarità"
019580050513      *     liberi a sufficienza per i segnacolli da elencare.
019590050513      *     Se non ci stanno => serve un modulo A4
019600050513if  2c                   if        XX > %elem($NP) - YY
019610050513     c                   eval      $A4_x_ScNS  = *on
019620050513     c                   eval      *in69  = *off
019630050513e   2c                   endif
019640050513e   1c                   ENDIF
019650030515      *
019660030515      * Imposto il numero di riga massima raggiungibile per l'eventuale
019670030515      *  stampa dei segnacolli non sequenziali
019680040607if  1c                   if        *in69
019690030515     c                   eval      wMaxLine = $MaxLine1
019700040607x   1c                   else
019710030515     c                   eval      wMaxLine = $MaxLine2
019720040607e   1c                   endif
019730030515      * Memo: ho stampato (sto stampando...) almeno un modulo A4 o A5.
019740030515      *   (utile solo per la segnalazione in stampa delle oltre 6.000
019750071008      *   pagine già stampate - in un formato e/o nell'altro).
019760080926      /free
019770080926         select;
019780100924           //?»»» Richiesta la stampa per PDA (4 LdV x A4)?
019790100924           //    ?di LdV per consegna AL 1° tentativo, ma?
019800100924           //    ?reperita packing-list (da stampare "a parte")?
019810100924           //?(la stampa della relativa packing-list verrà richiesta?
019820100924           // ?successivamente, dopo la stampa di tutte le LdV)?
019830100924           when  DB0fa4  = '4'    and
019840100924               //DB0fl1 <> 'D'    and
019850100924                 *in69   = *off   and
019860130306                 $FIAR8  = *on    and
019870130306                 $§PDF   = *off;
019880100924             DB0tms = 'D';
019890130306           //?»»» Richiesta la stampa per PDA (4 LdV x A4)?
019900130306           //    ?di LdV per consegna AL 1° tentativo, ma?
019910130306           //    ?reperita immagine PDF (da stampare "a parte")?
019920130306           //?(la stampa delle relative immagini verranno richieste?
019930130306           // ?successivamente, dopo la stampa di tutte le LdV e?
019940130306           // ?delle relative packing-list)?
019950130306           when  DB0fa4  = '4'    and
019960130306                 $FIAR8  = *off   and
019970130306                 $§PDF   = *on;
019980130307             DB0tms = 'i';
019990130306           //?»»» Richiesta la stampa per PDA (4 LdV x A4)?
020000130306           //    ?di LdV per consegna AL 1° tentativo, ma?
020010130306           //    ?reperita packing-list (da stampare "a parte") e?
020020130306           //    ?immagine PDF (anch'essa da stampare "a parte")?
020030130306           //?(la stampa della relativa packing-list e delle relative?
020040130306           // ?immagini verranno richieste successivamente, dopo la?
020050130306           // ?stampa di tutte le LdV)?
020060130306           when  DB0fa4  = '4'    and
020070130306               //DB0fl1 <> 'D'    and
020080130306                 *in69   = *off   and
020090130306                 $FIAR8  = *on    and
020100130306                 $§PDF   = *on;
020110130306             DB0tms = 'Y';
020120100924           //?»»» Richiesta la stampa per PDA (4 LdV x A4)?
020130080926           when  DB0fa4 = '4';
020140100927             DB0tms = 'Q';
020150100924           //?»»» Richiesta la stampa della packing-list per PDA?
020160081007           when  DB0fa4 = 'P'   and   $FIAR8 = *on;
020170080926             DB0tms = 'P';
020180081007           when  DB0fa4 = 'P'   and   $FIAR8 = *off;
020190081007             //DB0tms = *blank;
020200130307           //?»»» Richiesta la stampa normale e reperita immagine PDF?
020210130307           when  *in69  and  $§PDF;
020220130307             DB0tms = 'I';
020230130307           when  Not *in69  and  $§PDF;
020240130307             DB0tms = 'J';
020250100924           //?»»» Richiesta la stampa normale?
020260080926           when  *in69;
020270080926             DB0tms = '5';
020280080926           other;
020290080926             DB0tms = '4';
020300080926         endsl;
020310080926      /end-free
020320040707      *
020330040707      * Controllo se devo stampare "Non utilizzare per la consegna"
020340040707      * nella copia per il vettore,?senza?copia per il destinatario:
020350040707     c*** (già fatto:)   reset                   $No_x_Cons
020360071203      * - LdV recapito c/assegni:
020370090511      *   stampata in arrivo e con
020380120113if  1c                   if             DB0tbo <> 'P'
020390070306     c                             and  DB0tbo <> '8'
020400090511     c                             and  DB0lnp =  102
020410070306     c                             and  DB0cbo =  'FW'
020420071203     c                             and  DB0nft =  *zeros
020430071203      *   - fil. arrivo con GEO attivato
020440071203     c                             and (DB0fl1    =  'A'
020450071203     c                              or  wGeoCon   =  'S'
020460071203      *   - fil. arrivo abilitata al picking
020470120113     c                              or  A§OGpck   =  'S')
020480070306     c                   eval      $No_x_Cons  =  *on
020490120113e   1c                   endif
020500120113      *//* - LdV Poste
020510120113w   1c*//                   when      §OGntw   = 'PPT'
020520120113     c*//                   eval      $No_x_Cons  =  '2'
020530101125      /free
020540101125         // -?Controlla se "tagliatella" per Ritorno all'Incasso?
020550101129         //$Rit_a_Inc = ( DB0fa4  = '4'    and
020560101129         //               DB0tbo <> 'P'    and  DB0tbo <> '8'   and
020570101129         //               wB0dcm  > *zero  and
020580101129         //           ( ( DB0cas >  *zero  and  wB0icc =  'R' )  or
020590101129         //             ( DB0ift >  *zero  and  wB0ica =  'R' ) ) );
020600101129         // -?Controlla se LdV per Ritorno all'Incasso?
020610101129         $Rit_a_Inc = ( DB0fl4 <> *blank and
020620101129                        DB0tbo <> 'P'    and  DB0tbo <> '8'   and
020630101129                        wB0dcm  > *zero  and
020640101129                    ( ( DB0cas >  *zero  and  wB0icc =  'R' )  or
020650101129                      ( DB0ift >  *zero  and  wB0ica =  'R' ) ) );
020660101125      /end-free
020670030515      *
020680030515     c                   ENDSR
020690040607      *
020700040607      *---------------------------------------------------------------*
020710130315      *?Verifica esistenza dati del DDT allegato da stampare  o      ?*
020720130315      *?               di uno o più PDF allegati da stampare         ?*
020730040607      *---------------------------------------------------------------*
020740130315     c     CercaDDToPDF  BEGSR
020750040607      *
020760060921     c*** (da non fare:) reset                   $E_AR8
020770060921     c*** (già fatto:)   reset                   $FIAR8
020780130315     c*** (già fatto:)   reset                   $§PDF
020790040607     c*** (già fatto:)   reset                   $DDT_o
020800040607     c*** (già fatto:)   reset                   $DDT_p
020810040608     c*** (già fatto:)   reset                   $DDTlastPg
020820040607     c*** (già fatto:)   reset                   $AR8o1
020830040608     c*** (già fatto:)   reset                   wOmaRowT
020840040608     c*** (già fatto:)   reset                   wOmaRowD
020850041207     c*** (già fatto:)   reset                   dAR8
020860130315      *
020870130315      * -?Verifica esistenza PDF allegato?
020880130315     c                   eval      $§PDF  = (§AR5als = 'S')
020890130315      *
020900130315      * -?Verifica esistenza packing-list?
020910130315     c                   eval      $FIAR8 = (§AR5alx = 'S')
020920130315      *
020930130315if  1c                   if        NOT %open(FIAR801L)
020940130315     c                             and ($FIAR8  or  $§PDF)
020950130315     c                   open(e)   FIAR801L
020960130315      * Se errore significa che non c'è il file per cui non faccio
020970130315      *  più questa routine
020980130315if  2c                   if        %error
020990130315     c                   eval      $E_AR8 = *off
021000130315e   2c                   endif
021010130315e   1c                   endif
021020040607      *
021030130306     c                   ENDSR
021040061102      *
021050061102      *---------------------------------------------------------------*
021060071114      *?Verifica se Codice Fiscale e/o Partita Iva da richiedere al  ?*
021070071114      *?  cliente                                                    ?*
021080061102      *---------------------------------------------------------------*
021090061102      *  Il d.l. 223/06, poi convertito con la legge 248/06, ha      *
021100061102      *   reintrodotto l'obbligo di raccogliere il codice fiscale per *
021110061102      *   tutti i clienti e fornitori, siano essi aziende o privati.  *
021120061102      * SE?Porto Franco PrePagato?:                                   *
021130061102      *   il Cod.Fiscale dovrà essere del?Mittente?.                  *
021140061102      * SE?Porto Assegnato con Fattura Immediata?:                    *
021150061102      *   il Cod.Fiscale dovrà essere del?Destinatario?.              *
021160061102      * In sua assenza: dovrà essere richiesto al cliente stesso!     *
021170071114      *  Lo stesso decreto (poi convertito con la legge 248/06)      *
021180071114      *   prevede che a partire dal 01/01/2008 debba essere raccolta  *
021190071114      *   anche l'eventuale partita iva, del cliente o del fornitore. *
021200071114      * Stessi criteri già utilizzati per il codice fiscale.          *
021210061102      *---------------------------------------------------------------*
021220071114     c     CheckCFPI     BEGSR
021230061102      *
021240061106     c*** (già fatto:)   eval      $No_CF  = *off
021250071114     c*** (già fatto:)   eval      $No_PI  = *off
021260061102      *
021270071114      * Verifica se C.F./P.I. da richiedere (NON per clienti esteri)
021280071114      * - con fattura
021290071114if  1c                   IF            (DB0nft  <> *zeros
021300061106     c                              or  DB0ift  <> *zeros)
021310061103     c                             AND (
021320061102      * - - P.F. prepagato
021330061107      *     (con mittente italiano)
021340061107     c                                 (DB0tbo  =  '8'
021350061107     c                              and DB0nzm  =  *blanks)
021360061106      * - - P.A. (con fattura immediata)
021370061107      *     (con destinatario italiano)
021380061107     c                             or  (%subst(DB0tb1:1:1) = 'A'
021390061107     c                              and DB0nzd  =  *blanks)
021400061106      * - - con 2ª bolla
021410061107      *     (con destinatario italiano)
021420061103     c                             or  (DB0tb2  <> *blanks
021430121029     c                              and DB0tbo  <> 'P'
021440061107     c                              and DB0tbo  <> '8'
021450061107     c                              and DB0nzd  =  *blanks)
021460071114      * - => verirficare se da richiedere C.F. e/o P.I.
021470061103     c                                 )
021480120503      *
021490120503sel 2c                   select
021500120503      * · Esiste Partita Iva NON FITTIZIA: non chiedere nulla
021510120503w   2c                   when      wB0piv <> *blanks   and
021520120503     c                             %scan('$$' : wB0piv) = *zero
021530120503     c                   leavesr
021540120503      * · Esiste C.F. lungo 16 (privato) : non chiedere P.I.
021550120503w   2c                   when      %len( %trim( wB0cfi ) ) = 16
021560120503     c                   leavesr
021570120503      * · Esiste C.F. lungo 11 ..........: chiedere P.I. (se mancante)
021580120503w   2c*//                when      %len( %trim( wB0cfi ) ) = 11
021590120503e   2c                   endsl
021600071114      *
021610071114      * - senza codice fiscale
021620120503     c                   eval      $No_CF  = (wB0cfi = *blanks)
021630071114      *
021640071114      * - senza partita iva (se NON privato)
021650120503     c                   eval      $No_PI  = (wB0piv = *blanks)
021660071114      *
021670071114e   1c                   ENDIF
021680061102      *
021690061102     c                   ENDSR
021700070306      *
021710070306      *---------------------------------------------------------------*
021720070306      *?Verifica esistenza note da stampare (dal file FIARN00F)      ?*
021730070306      *---------------------------------------------------------------*
021740070306     c     CheckFIARN    BEGSR
021750070306      *
021760070306     c*** (già fatto:)   reset                   $FIARN
021770070306      *
021780070306      * Controllo esistenza note solo se c'è il file
021790070319     c     K06ARN11      chain     FIARN000
021800070319if  1c                   if             %found(FIARN11L)
021810070306     c                             and  ARNnob <> *blanks
021820070306     c                   eval      $FIARN = *on
021830070306e   1c                   endif
021840070306      *
021850070306     c                   ENDSR
021860070315      *
021870070315      *---------------------------------------------------------------*
021880070315      *?Verifica esistenza C.A. aperta legata alla spedizione        ?*
021890070315      *---------------------------------------------------------------*
021900070316     c     CercaCA       BEGSR
021910070315      *
021920070319     c*** (già fatto:)   reset                   $C_A_
021930070315      *
021940070319      * Richiamo del programma di comodo per recuperare le CA legate
021950070319      *   alla spedizione in esame
021960070319     c                   reset                   FIDN12ds
021970070514if  1c                   if        $Mamma = *off
021980070319     c                   eval      I12aas = DB0aas
021990070319     c                   eval      I12lnp = DB0lnp
022000070319     c                   eval      I12nrs = DB0nrs
022010070319     c                   eval      I12nsp = DB0nsp
022020070514x   1c                   else
022030070514     c                   eval      I12aas = LBLaap
022040070514     c                   eval      I12lnp = LBLlpp
022050070514     c                   eval      I12nrs = LBLnrp
022060070514     c                   eval      I12nsp = LBLnsp
022070070514e   1c                   endif
022080070319     c*** già così:      eval      I12fel = 'E'
022090070319     c***  "    "        eval      I12fan = 'E'
022100070319     c***  "    "        eval      I12fch = 'E'
022110070319      *
022120070319     c                   call      'FIDN12R'
022130070319     c                   parm                    FIDN12ds
022140070319      *
022150070319      * Se trovata almeno una CA
022160070319     c                   eval      $C_A_  = (O12nca > *zeros)
022170070315      *
022180070316     c                   ENDSR
022190070319      *
022200070319      *---------------------------------------------------------------*
022210070319      *?Verifica se stampa spedizione DOPO "Lasciato Avviso"         ?*
022220070319      *---------------------------------------------------------------*
022230070319     c     CheckLascAvv  BEGSR
022240070319      *
022250070319     c*** (già fatto:)   reset                   $LascAvv
022260070319      *
022270070319if  1c                   if        NOT  %open(FNEVB05L)
022280070319     c                   open      FNEVB05L
022290070319e   1c                   endif
022300070319      *
022310070319     c     KLDV          setgt     FNEVB000
022320070319     c     KLDV          readpe    FNEVB000
022330070319      *
022340070319      * Proseguo solo se ritrovo un evento
022350070319do  1c                   DOW       NOT  %eof(FNEVB05L)
022360070319      *
022370070319      * Verifico se evento di tipo Giacenza
022380070319     c                   eval      *in31    = *off
022390070319     c     EVBcev        lookup    $2Acev_G                               31
022400070319      **
022410070319sel 2c                   Select
022420070319      * - Evento GIACENZA --> NO LASCIATO AVVISO
022430070319w   2c                   When      *in31
022440070319     c                   leave
022450070319      * - Evento RIC --> 1° LASCIATO AVVISO
022460070319w   2c                   When      EVBcev   = 'RIC'
022470070319     c                   eval      $LascAvv = *on
022480131205      * - - salvataggio data/ora
022490131205     c                   eval      wEVBdev  = EVBdev
022500131205     c                   eval      wEVBhev  = EVBhev
022510070319     c                   leave
022520070319      * - Evento DA NON ELABORARE
022530070319x   2c                   Other
022540070319     c     KLDV          readpe    FNEVB000
022550070319e   2c                   Endsl
022560070319      **
022570070319e   1c                   ENDDO
022580070319      *
022590070319     c                   ENDSR
022600090302      /free
022610090302
022620090302       //--------------------------------------------------------------
022630090302       //?Verifica esistenza note da stampare (dal file TNTBE00F)      ?
022640090302       //--------------------------------------------------------------
022650090302       BEGSR  CheckTabPXA;
022660090302
022670090302         //clear  $TabPXA;          (già fatto)
022680110411         //clear  $TabPXA2;         (già fatto)
022690110411
022700110411         // -?Verifica esistenza "PXA" per particolarità consegna?
022710110411         If  DB0gma <> *blank;
022720170517           clear  k05tntbe01;
022730110411           k_TBEcod = 'PXA';
022740110411           k_TBEke1 = DB0gma;
022750110411           k_TBEke2 = '001            ';
022760110411           setll  %kds(k05tntbe01 : 3)  TNTBE000;
022770110411           // -?Bisogna testare se nota ANNULLATA?
022780110411           //$TabPXA  = %equal(TNTBE01L);
022790110411           reade  %kds(k05tntbe01 : 2)  TNTBE000;
022800110411           $TabPXA2 = (Not %eof(TNTBE01L)   and   TBEatb = *blank
022810110411                       and TBEke2 <= '006            ');
022820110411         EndIf;
022830090302
022840110411         // -?Reperimento eventuali "PXA" per mittente?
022850170517         clear  k05tntbe01;
022860090302         k_TBEcod = 'PXA';
022870090302         k_TBEke1 = %editc(DB0ccm : 'X');
022880100316         k_TBEke2 = '001            ';
022890090302         clear  k_TBElin;
022900090302         clear  k_TBEsif;
022910100316         setll  %kds(k05tntbe01 : 3)  TNTBE000;
022920110411         // -?Bisogna testare se nota ANNULLATA?
022930090302         //$TabPXA  = %equal(TNTBE01L);
022940090302         reade  %kds(k05tntbe01 : 2)  TNTBE000;
022950100316         $TabPXA  = (Not %eof(TNTBE01L)   and   TBEatb = *blank
022960100316                     and TBEke2 <= '006            ');
022970090302
022980090302       ENDSR;
022990090302      /end-free
023000030515      *
023010030515      *---------------------------------------------------------------*
023020050217      *?Imposta i campi di stampa con quelli della DS                ?*
023030030515      *---------------------------------------------------------------*
023040030515     c     PrepPRT       BEGSR
023050040223      *
023060040223      * Reperimento dati da FIAR5 / "GEN"
023070130315     c*** (già fatto:)   exsr      RepAR5gen
023080070212     *** *
023090070212     *** * Impostazione Data spedizione
023100070212     *** * (già fatto nella subr. "TipoLdV")
023110070319      *
023120100218      * CONTROLLO SE STAMPA SPEDIZIONE DOPO "LASCIATO AVVISO"
023130100218      * (?*-=»>   SE NON MESSA IN "FERMO DEPOSITO"   <«=-?):
023140081118      * (NON se richiesta stampa di "ricevuta" - 4 in A4)
023150110427      * per LNA (o capofila se LNA è in £6) con $GeoAtt = *ON =>
023160070717      * GEO attivato completamente
023170070319      * controllo se la spedizione ha già più tentativi di consegna e
023180070319      *   determino se in stato di "lasciato avviso"
023190070319      *
023200070319     c*** (già fatto:)   reset                   $LascAvv
023210070406if  1c                   if             DB0tbo    =  'A'
023220081117     c                             and  DB0fa4    <> '4'
023230070514     c                             and  wB0ntc    >  *zeros
023240110427     c                             and  $GeoAtt   =  *on
023250100218     c                             and  DB0ffd    =  *blank
023260070319     c                   exsr      CheckLascAvv
023270070319e   1c                   endif
023280030515      *
023290120530      * Impotazione dati BRT S.p.A.
023300120530     c*//                if        (DB0dft > *zero       and
023310120530     c*//                           DB0dft > §vpoBRTdta)  OR
023320120530     c*//                          (DB0dft = *zero       and
023330120530     c*//                           Wdsp   > §vpoBRTdta)
023340120530     c                   eval      TBAR   =  $BRT
023350120530     c*//                else
023360120530     c*//                eval      TBAR   =  $Bartolini
023370120530     c*//                endif
023380070307     c                   eval      TBA1   =  $Intest1
023390070307     c                   eval      TBA2   =  $Intest2
023400080707if  1c                   if            DB0nft <> *zeros
023410080707     c                             or  DB0ift <> *zeros
023420080627     c                   eval      TBA3   =  $Intest3
023430080707e   1c                   endif
023440070307     c                   eval      TBA4   =  $Intest4
023450070307     c                   eval      TBA5   =  $Intest5
023460070307     c*** NON sopra:     eval      TBA6   =  $Intest6
023470120530     c*//                eval      TBA7   =  $Intest7
023480120530     c                   eval      TBA7   =  $Intest7_BRT
023490030515      *
023500030515      * - Intestazione moduli
023510080919     c                   eval      Tcxd   =  c_CopiaXdest
023520130726     c*//                if        Not *in36     (NON ancora acceso)
023530130726if  1c                   if        DB0tbo <> '8'  or  DB0ris = 'M'
023540130726     c                   eval      Tcxd2  =  Tcxd
023550130726     c                   endif
023560091008      *
023570091008      /free
023580091008         select;
023590091008           // - Indicazione Copia LDV x cliente
023600091008           when  DB0ris = 'C';
023610091008             Pkco = 'C O P I A';
023620091008           // - Altre indicazioni: R=Ristampa, V=Variazione, ...
023630091008           other;
023640091008             Tkri = DB0ris;
023650091008         endsl;
023660091008      /end-free
023670030515      *
023680030515      * - Codice a barre: ANNO+LNP+NRS+NSP+CKD
023690030515      *   (chiamo il pgm TRUL28R per ricavarne il check digit)
023700030515     c                   reset                   TRUL28ds
023710030515     c                   z-add     DB0aas        BARcdeAAS
023720030515     c                   eval      BARcdeLNP = DB0lnp
023730030515     c                   eval      BARcdeNRS = DB0nrs
023740030515     c                   eval      BARcdeNSP = DB0nsp
023750030515     c                   movel     BarCodeDS     I28cod
023760030515     c                   call      'TRUL28R1'
023770030515     c                   parm                    TRUL28DS
023780030515if  2c                   if        O28err =  *blanks
023790030515     c                   movel     O28cod        Tbcd
023800030515e   2c                   endif
023810140915      *
023820140915      /free
023830140915
023840140915         // -?BRTcode?
023850140915         reset  TRUL28ds0;
023860140915         trul28ds0.i280cod = BarCodeDS;
023870140915         trul28r0 ( TRUL28ds0 );
023880140915         if  trul28ds0.o280err <> *on;
023890140917           //TbrtC = trul28ds0.o280cod;
023900140917           TbrtC = %subst( trul28ds0.o280cod :  1 : 5 ) + ' ' +
023910140917                   %subst( trul28ds0.o280cod :  6 : 5 ) + ' ' +
023920140917                   %subst( trul28ds0.o280cod : 11 : 5 ) + ' ' +
023930140917                   %subst( trul28ds0.o280cod : 16 : 4 );
023940140915         endif;
023950140915
023960140915      /end-free
023970030515      *
023980030515      * - Sigla operatore video
023990030515     c                   eval      Psop   =  DB0sop
024000080721      /free
024010080721
024020130306         // -?Flag "allega documento di trasporto"?
024030130418         //   IN FUTURO: se al 1° tentativo di consegna
024040130418         //              (DB0FL1 = "D" - v. pgm FNLRS9R)
024050080721         select;
024060081119
024070130306           // -?LdV con DDT allegato oltre il 1° tentativo di consegna?
024080081127           when  DB0bam =  'S'   and   wB0tmc <> *blank;
024090081127             evalr Pddt =  'DDT sul collo';
024100081119
024110130306           // -?LdV con DDT allegato (al 1° tentativo di consegna)?
024120080723           when  DB0bam =  'S';
024130080723             evalr Pddt =  'DDT';
024140081119
024150130306           // -?Esiste packing-list abbinata?
024160130306           when  $FIAR8 =  *on   and   $§PDF =  *off;
024170081013           //if  DB0fl1 <> 'D';
024180081013               evalr Pddt =  'PACKING-LIST';
024190081013           //else;
024200081013           //  evalr Pddt =  'PACKING-LIST GIA'' CONSEGNATA';
024210081013           //endif;
024220130306
024230130306           // -?LdV con PDF allegato?
024240130306           when  $FIAR8 =  *off  and   $§PDF =  *on;
024250130306             evalr Pddt =  'PACKING-LIST PDF';
024260130306
024270130306           // -?LdV con packing-list abbinata & PDF allegato?
024280130306           when  $FIAR8 =  *on   and   $§PDF =  *on;
024290130308             evalr Pddt =  'PACKING-LIST PDF';
024300081119
024310130306           // -?LdV senza PDF allegato & senza packing-list abbinata?
024320080721           other;
024330080721             clear Pddt;
024340081119
024350080721         endsl;
024360080721
024370080721      /end-free
024380030515      *
024390030515      * - Mittente
024400170517     c                   if        *in71
024410170517     c                   exsr      sr_Mitt_DPD
024420170517     c                   endif
024430170517      *
024440030515     c                   if        DB0ccm <> *zeros
024450030515     c                   move      DB0ccm        Pccm
024460030515     c                   endif
024470030515     c                   eval      Prsm   =  DB0rsm
024480030515     c                   eval      Pinm   =  DB0inm
024490030515     c                   eval      Pcam   =  DB0cam
024500030515     c                   eval      Plom   =  DB0lom
024510030515     c                   eval      Pprm   =  DB0prm
024520030515     c                   eval      Pnzm   =  DB0nzm
024530030515      *   Mittente originale
024540030515if  1c                   if        DB0rmo <> *blanks
024550030515     c                   eval      Pkmo   =  'MITT. ORIGINALE'
024560030515     c                   eval      Prmo   =  DB0rmo
024570030515e   1c                   endif
024580030515      *
024590070308      * - Filiale di Arrivo
024600030515if  1c                   if        DB0ris <> 'M'
024610080721     c                   eval      Pkla   =  'FILIALE'
024620080701     c                   eval      Plna1  =  %editc(DB0lna : 'X')
024630080722     c                                    +  ' '
024640080702     c                                    +  DB0dla
024650080722     c                                    +  ' - '
024660080722     c                                    +  DB0inl
024670140129     c*//                                 +  ' Tel.'
024680140129     c                                    +  '     '
024690080722     c                                    +  %trimr(DB0tel)
024700080722     c                   eval      Plna2  =  %editc(DB0cal : 'X')
024710080722     c                                    +  ' '
024720080722     c                                    +  DB0lol
024730080722     c                                    +  ' '
024740080722     c                                    +  DB0prl
024750140129     c*//                                 +  '                  '
024760140129     c*//                                 +  ' Fax '
024770140129     c*//                                 +  %trimr(DB0fax)
024780140129     c                   if        DB0fax <> *blanks
024790140129     c     c_Mai:c_Min   xlate     DB0fax        DB0fax
024800140129     c                   eval      Plna2  =  %trimr(Plna2)
024810140129     c                                    +  '          '
024820140129     c*//*                                +  ' E-mail: '
024830140129     c                                    +  '         '
024840140129     c                                    +  %trimr(DB0fax) + '@brt.it'
024850140129     c                   endif
024860030515e   1c                   endif
024870030515      *
024880030515      * - Destinatario
024890030515if  1c                   if        DB0ccd <> *zeros
024900030515     c                   move      DB0ccd        Pccd
024910030515e   1c                   endif
024920030515     c                   eval      Prsd   =  DB0rsd
024930030515     c                   eval      Prse   =  DB0rd2
024940030515     c                   eval      Pind   =  DB0ind
024950030515     c                   eval      Pcad   =  DB0cad
024960030515     c                   eval      Plod   =  DB0lod
024970030515     c                   eval      Pprd   =  DB0prd
024980030515     c                   eval      Pnzd   =  DB0nzd
024990030515      *
025000030515      * - Riferimenti mittente
025010030515     c                   eval      Prmn   =  DB0rmn
025020030515     c                   eval      Prma   =  DB0rma
025030061107      * se bolla DPD recupero il riferimento da FIAR4 rcd "I"
025040061107     c*** (già fatto:)   clear                   dsBL4I
025050060626     c                   if        *in71
025060061103     c                   exsr      RepAR4_I
025070060626     c                   endif
025080030515      *
025090030515      * - Nominativo & telefono
025100031124      *   (da FIAR5 / "GEN")
025110040607     c*** (già fatto:)   exsr      RepAR5gen
025120031124     c                   eval      Pnom   =  §AR5ref
025130031124     c                   eval      Ptel   =  §AR5teld
025140030515      *
025150030515      * - Spedizione
025160030515      *   (numero: non se Rimesso al Mittente)
025170030515if  1c                   if        DB0ris <> 'M'
025180030515     c                   eval      Plnp   =  DB0lnp
025190030515     c                   eval      Pnrs   =  DB0nrs
025200030515     c                   eval      Pnsp   =  DB0nsp
025210030515e   1c                   endif
025220030515      * . . Data spedizione
025230031124     c     *iso          movel     Wdsp          Data_EUR
025240130418     c     *eur          movel     Data_EUR      Pdsp
025250140922     c     *dmy          movel     Data_EUR      Pdsp6
025260030515      *
025270030515      * - Colli: numero, peso, volume, flag tipo-volume
025280030515     c                   eval      Pncl   =  DB0ncl
025290030515     c                   eval      Ppkf   =  DB0pkf
025300030515     c                   eval      Pvlf   =  DB0vlf
025310030515if  1c                   if            DB0fvf <> *blanks
025320030515     c                             and DB0ris <> 'M'
025330030717     ***c***                eval      Pfvf   =  '-' + DB0fvf + '-'
025340030717     c                   move      DB0fvf        Pfvf
025350030515e   1c                   endif
025360030515      *
025370030515      * - Natura merce
025380030515     c                   eval      Pnat   =  DB0nas
025390030515      *
025400030515      * - Servizio
025410100513      /free
025420100513         if  %subst(DB0tb1:1:1) = 'A'   or   DB0tb2 <> *blanks;
025430100513           exsr  sr_CheckORM_PrevvMail;
025440100513         endif;
025450100513      /end-free
025460040607      * . . Descrizione LDV
025470100513     c                   if        Not  $PreavvORM
025480030515     c                   eval      Pds1   =  DB0dcb
025490100513     c                   endif
025500030515      * . . Descrizione tipo servizio
025510030515if  1c                   if        DB0fst =  'S'
025520030515     c                   eval      Pds2   =  DB0dts
025530030515e   1c                   endif
025540030515      * . . Reso / Franco frontiera
025550030515sel 1c                   select
025560030515w   1c                   when      DB0fbr =  'S'
025570030515     c                   clear                   w008a
025580030515     c                   eval      w008a  =  '   RESO '
025590030515     c                   move      w008a         Pds2
025600030515w   1c                   when      DB0fbr =  'F'
025610030515     c                   clear                   w015a
025620030515     c                   eval      w015a  =  '   FR.FRONTIERA'
025630030515     c                   move      w015a         Pds2
025640030515e   1c                   endsl
025650030515      *
025660030515      * - Zona consegna
025670030515     c                   eval      Pznc   =  DB0znc
025680030515      *
025690030515      * - Flag "*" (gestito a parte)
025700040223      *
025710040223      * - Flag "V" = colli di valore
025720040223if  1c                   if        §AR5bva   <> *blank
025730040223     c                   eval      Pbva   =  'V'
025740040223e   1c                   endif
025750060921      *
025760070212      * - Codice Giro o Numero Distinta (se tipo bolla "ARRIVO")
025770071015if  1c                   IF              DB0tbo = 'A'
025780071015     c                             and  wGeoCon = 'S'
025790070212sel 2c                   Select
025800070212      * · - Stampa LdV in arrivo con codice giro (parametro passato)
025810071015w   2c                   When      DB0fl1     =  'A'
025820070215if  3c                   if        §DB0tgi    =  'M'
025830101028     c                   eval      Pndc   =  'g:' + §DB0cgi + 'm'
025840070215x   3c                   else
025850101028     c                   eval      Pndc   =  'g:' + §DB0cgi
025860070215e   3c                   endif
025870070212      * · - Stampa LdV con numero distinta consegna
025880071015w   2c                   When           wB0ndc  > *zeros
025890110427     c                             and  $GeoAtt =  *on
025900101028     c                   eval      Pndc   =  'd:'
025910070514     c                                    +  %trim(%editc(wB0ndc:'Z'))
025920101028if  3c                   if        %len(Pndc)  >= %len(%trimr(Pndc))
025930071016     c                                          + 3 + %len(Wpdc4a)
025940101028     c                   eval      Pndc   =  %trimr(Pndc)
025950071022     c                                    +  ' a:'
025960071022     c                                    +  Wpdc4a
025970071016e   3c                   endif
025980070212      * · - Stampa LdV con codice giro (da reperire in FIARG00F)
025990070213w   2c                   Other
026000070212if  3c                   if        NOT %open(FIARG01L)
026010070212     c                   open      FIARG01L
026020070212e   3c                   endif
026030070215     c                   clear                   ARGcgi
026040070215     c                   clear                   ARGtgi
026050070212     c     KLDV          chain     FIARG000
026060070215if  3c                   if        ARGtgi =  'M'
026070101028     c                   eval      Pndc   =  'g:' + ARGcgi + 'm'
026080070215x   3c                   else
026090101028     c                   eval      Pndc   =  'g:' + ARGcgi
026100070215e   3c                   endif
026110070215e   2c                   EndSl
026120070212e   1c                   ENDIF
026130030515      *
026140030515      * - Fermo deposito
026150030515if  1c                   if        DB0ffd <> *blanks
026160060921     c                   eval      Pffd   =  'FERMO DEPOSITO '
026170030515e   1c                   endif
026180030515      *
026190070308      * - Segnacolli: filiale, serie, dal / al
026200030515if  1c                   if        DB0fls <> *zeros
026210030515     c                   move      DB0fls        Ppos
026220030515e   1c                   endif
026230040607      * - - se bolla con mamma considero la serie della mamma
026240070515if  1c                   if        $Mamma =  *on
026250031015     c                   eval      Psrs   =  wB0nrs
026260031015x   1c                   else
026270030515     c                   eval      Psrs   =  DB0nrs
026280031015e   1c                   endif
026290030515      * - Se colli non sequenziali: stampo "NON SEQUENZIALI" + Elenco
026300030515      *   Se collo cumulativo ....:   "     SCD "X" Num.Colli
026310030515      *   Se colli sequenziali ...:   "     SCD "/" SCA
026320030515      *   Se collo singolo .......:   "     SCD
026330031015sel 1c                   select
026340030515      * - - non sequenziali
026350030515w   1c                   when      DB0fns =  'S'
026360050513if  2c                   if           *in69      = *on
026370050513     c                             or $A4_x_ScNS = *off
026380030715      *     caricamento delle "Note e Particolarità" per consentirne
026390030715      *     l'aggiunta come "Elenco segnacolli non sequenziali"
026400031015if  3c                   if        $Partic = *off
026410030515     c                   exsr      SchPARTIC
026420031015e   3c                   endif
026430030515     c                   exsr      ListaScNS
026440050511x   2c                   else
026450050511if  3c                   if        DB0ncl <= 2
026460050511     c                   exsr      ListaScNSd
026470081007x   3c                   else
026480081007     c                   eval      Pnsx   =  'NON SEQUENZIALI'
026490050511e   3c                   endif
026500030515e   2c                   endif
026510030515      * - - cumulativo
026520050114w   1c**!!!              when      DB0xco =  'X'
026530081010     c**!!!              eval      Pnsx   =  %editc(DB0ncd : 'Z')
026540081010     c**!!!                               +  'X'
026550081010     c**!!!                               +  %editc(DB0ncl : 'Z')
026560030515      * - - singolo o sequenziali
026570030515      *     (non se rimesso al mittente)
026580030515w   1c                   when      DB0ris <> 'M'
026590080922     c                   eval      Pnsx   =  %editc(DB0ncd : 'Z')
026600030515if  2c                   if            DB0nca <> *zeros
026610030515     c                             and DB0nca <> DB0ncd
026620080922     c                   eval      Pnsx   =  %trimr(Pnsx) + '-'
026630080922     c                                    +  %editc(DB0nca : 'Z')
026640030515e   2c                   endif
026650030515e   1c                   endsl
026660030515      *
026670030716      * - Note (1 e 2) e Particolarità (0, 3, 4, 5 e 6)
026680030710      *   con eventuali segnacolli non sequenziali (max 4 x riga)
026690030515if  1c                   if        $Partic = *off
026700030515     c                   exsr      SchPARTIC
026710030515e   1c                   endif
026720030716if  1c                   if           $NP(1) <> *blanks
026730030716     c                             or $NP(2) <> *blanks
026740030716     c                             or $NP(3) <> *blanks
026750030716     c                             or $NP(4) <> *blanks
026760030716     c                             or $NP(5) <> *blanks
026770030716     c                             or $NP(6) <> *blanks
026780030716     c                   eval      Pknp   =  'NOTE  E  PARTICOLARITÀ'
026790030710     c                   eval      Pnp1   =  $NP(1)
026800030710     c                   eval      Pnp2   =  $NP(2)
026810030710     c                   eval      Pnp3   =  $NP(3)
026820030710     c                   eval      Pnp4   =  $NP(4)
026830030710     c                   eval      Pnp5   =  $NP(5)
026840030710     c                   eval      Pnp6   =  $NP(6)
026850030716e   1c                   endif
026860030515      *
026870080401      * - Privato / Appuntamento / Uffici / Consegna Garantita
026880080401      *   (servizio consegna garantita x cliente Wuerth)
026890170413if  1c*//                if        DB0gva =  'P '
026900170413     c*//                evalr     Ppad = 'PRIVATO'
026910170413e   1c*//                endif
026920041012if  1c                   if        DB0gva =  'U '
026930080723     c                   evalr     Ppad = 'CONSEGNA AD UFFICI'
026940041012e   1c                   endif
026950081010if  1c*** NO !!!         if           DB0tc1 =  'A'
026960081010     c*** NO !!!                   or DB0tc2 =  'A'
026970081010     c*** NO !!!         move      'APPUNTAMENTO'Ppad
026980081010e   1c*** NO !!!         endif
026990080401if  1c                   if             DB0nrs = 40
027000090518     c                             and  DB0tsp = 'E'
027010080401     c                             and (DB0lnp = 043
027020110429     c                              or  DB0lnp = 277
027030080401     c                              or  DB0lnp = 063)
027040090217     c*****              evalr     Ppad = 'CONSEGNA GARANTITA'
027050090227     c                   eval      Pds2 = 'CONSEGNA ESPRESSA +
027060090227     c                                     WÜRTH'
027070080401e   1c                   endif
027080030515      *
027090030515      * - Data e ora consegna richiesta
027100030515      *   e Flag "*" > x Data e/o Ora consegna richiesta
027110091116      *   (NO "Prima del" per il cliente TTL)
027120091116if  0c                   if        DB0ccm <> c_CliTTL  or
027130091116     c                             DB0dcr =  *zeros    or  DB0tcr <> 'P'
027140030515if  1c                   if        DB0dcr <> *zeros
027150030515     c                   eval      Past   =  '*'
027160030515     c     *iso          movel     DB0dcr        Data_EUR
027170030515     c     *eur          movel     Data_EUR      Pdcr
027180030515e   1c                   endif
027190030515if  1c                   if        DB0hcr <> *zeros
027200030515     c                   eval      Past   =  '*'
027210030515     c                   eval      *in18  =  *on
027220030515     c                   eval      Phcr   =  DB0hcr
027230030515e   1c                   endif
027240091116e   0c                   endif
027250030515      *
027260030515      * - Consegne particolari
027270030515     c                   eval      Pcp1   =  DB0dc1
027280030515     c                   eval      Pcp2   =  DB0dc2
027290030515     *** *
027300030515     *** * - Valore merce dichiarato e relativa divisa => NO
027310030515     ***c***                eval      Pvmd   =  DB0vmd
027320030515     ***c***                eval      Pvad   =  DB0vad
027330030515     *** *
027340030515     *** * - Importo da assicurare e relativa divisa   => NO
027350030515     ***c***                eval      Pvas   =  DB0vas
027360030515     ***c***                eval      Pias   =  DB0ias
027370030515     *** *
027380030515     *** * - Quantità da fatturare                     => NO
027390030515     ***c***                eval      Pqft   =  DB0qft
027400030515      *
027410030515      * - Imponibile / IVA / Esenzione / Totale fattura e divisa
027420030515      *   (importi "arrotondati" a 2 decimali in stampa)
027430030515if  1c                   if        DB0ift <> *zeros
027440030515     c                   eval      Pkim   = 'IMPONIBILE'
027450130218     c*//                   eval(h)   Pimp   =  DB0imv
027460130218     c                   eval(h)   w0112  =  DB0imv
027470130218     c                   evalr     Pimp   =  %editc(w0112 : 'K')
027480030515sel 2c                   select
027490030515w   2c                   when      DB0iva <> *zeros
027500030515     c                   eval      Pkie   = 'I.V.A.'
027510030624     c                   eval(h)   w0112  =  DB0iva
027520130218     c*//                   eval      Piva   =  '     ' +
027530130218     c*//                                       %editc(w0112 : 'K')
027540130218     c                   evalr     Piva   =  %editc(w0112 : 'K')
027550030515w   2c                   when         DB0cei <> *blanks
027560030515     c                             or DB0dce <> *blanks
027570030912     c                   eval      Pkie   = 'ESENZIONE'
027580030912     c                   eval      Piva   =  DB0dce
027590030515e   2c                   endsl
027600030515     c                   eval      Pktf   = 'TOTALE FATTURA'
027610130218     c*//                   eval(h)   Pift   =  DB0ift
027620130218     c                   eval(h)   w0112  =  DB0ift
027630130218     c                   evalr     Pift   =  %editc(w0112 : 'K')
027640030515if  2c                   if        DB0div <> §GEDcn
027650030515     c                   eval      Pdiv   =  DB0div
027660030515e   2c                   endif
027670030515e   1c                   endif
027680030515      *
027690030515      * - Contrassegno e relativa divisa
027700030515if  1c                   IF        DB0cas <> *zeros
027710030625      *
027720030515     c                   eval(h)   Pcas   =  DB0cas
027730030515if  2c                   if        DB0vca <> §GEDcn
027740030515     c                   eval      Pdca   =  DB0vca
027750030515e   2c                   endif
027760030515     c                   exsr      CtrCAs
027770030624      * - - Nuovo controllo abilitazione c/assegno
027780030624     c                   reset                   TRUL21DS
027790030624     c                   eval      I21cbo =  DB0cbo
027800030624     c                   eval      I21tsp =  DB0tsp
027810030624     c                   eval      I21lnp =  DB0lnp
027820030624     c                   eval      I21nzm =  DB0nzm
027830030624     c                   eval      I21lna =  DB0lna
027840030624     c                   eval      I21nzd =  DB0nzd
027850130826     c                   if        DB0ccm >  *zero
027860130826     c                   eval      I21ksc =  DB0ccm
027870130826     c                   else
027880130826     c                   eval      I21ksc =  (DB0lnp * 10000 ) + 8888
027890130826     c                   endif
027900030624     c                   eval      I21tic =  DB0tic
027910030624if  2c                   if        DB0vca <> §GEDcn
027920030624     c                   eval      I21imp =  Wcas
027930030624     c                   eval      I21div =  §GEDcn
027940030624x   2c                   else
027950030624     c                   eval      I21imp =  DB0cas
027960030624     c                   eval      I21div =  DB0vca
027970030624e   2c                   endif
027980030624     c                   eval      I21ute =  KNMUS
027990040607     c                   eval      I21pgm =  SDSpgm
028000030624     c                   call      'TRUL21R'
028010030624     c                   parm                    TRUL21ds
028020030624      * - - Carico la tabella "Tipo incasso C/Assegno" (1A)
028030030624     c                   exsr      Tab_1A
028040030624     c                   movel     §1Ades        DB0dti
028050030624if  2c                   if            §1Adeo <> *blanks
028060030624     c                             and O21fca <> *blanks
028070030624     c                   movel     §1Adeo        DB0dti
028080030624e   2c                   endif
028090100208      /free
028100100208           // - -?Reperimento intestatario c/assegno?
028110100322           if  §VPOcas <= wDate   and   DUTlpo  <> 'S';
028120100208             reset  trulINTds;
028130100208             iINTtic  = DB0tic;
028140100208             iINTrscM = DB0rsm;
028150100208             iINTrscO = DB0rmo;
028160100208             iINTcdi  = §AR5cdi;
028170100208             //if  §AR5cdi = *blank;
028180100208             //  iINTaas = DB0aas;
028190100208             //  iINTlnp = DB0lnp;
028200100208             //  iINTnrs = DB0nrs;
028210100208             //  iINTnsp = DB0nsp;
028220100208             //endif;
028230100208             trulINTr ( TRULINTds );
028240100322           endif;
028250100208      /end-free
028260030624      * - - Imposto i campi di stampa
028270030624     c                   eval      Ptic   =  DB0tic + ' ' + DB0dti
028280091119      /free
028290100208           // - - -?Intestatario assegno?
028300100322           if  DB0cas  <> *zero   and
028310100322              (§VPOcas <= wDate   and   DUTlpo  <> 'S')   and
028320100208               oINTerr = *blank;
028330100208             Pkic = 'INTESTARE A:';
028340100208             Pica = oINTrsci;
028350100208           endif;
028360091119      /end-free
028370030625      *
028380030515e   1c                   ENDIF
028390030515      *
028400030515      * - Totale da esigere al ritiro o alla consegna e relativa divisa
028410030515if  1c                   IF        DB0ift >  *zeros
028420030625      *
028430030515if  2c                   if            DB0vca <> §GEDcn
028440030515     c                             and DB0vca <> *blanks
028450030515     c                   eval      Pdie   =  DB0div
028460030515e   2c                   endif
028470030515      *   Controllo se C/Assegno da sommare al totale da esigere o no:
028480030515      *   se Franco-Prepagato il C/Assegno non si somma mai;
028490030515      *   se c'è il totale fattura: metto anche il il totale da esigere.
028500030515      *   (importi "arrotondati" a 2 decimali in stampa)
028510030515sel 2c                   Select
028520030515      *   Non ho C/Assegno o è Franco Prepagato
028530071008w   2c                   When          DB0cas  = *zeros
028540071008     c                             or  DB0tbo  = '8'
028550080701     c                   eval(h)   Pies   =  DB0ift
028560030625     c                   clear                   Pie1
028570030625     c                   clear                   Pdi1
028580030515      *   C/Assegno sommabile
028590030515w   2c                   When      DB0ste =  'S'    and $CAeur = *on
028600030515sel 3c                   select
028610030515      *   - c/assegno in EURO
028620030515w   3c                   when      DB0div =  §GEDcn and DB0vca =  §GEDcn
028630080701     c                   eval(h)   Pies   =  DB0ift + DB0cas
028640030624     c                   clear                   Pie1
028650030624     c                   clear                   Pdi1
028660030515      *   - c/assegno in DIVISA
028670030515w   3c                   when          DB0div =  §GEDcn
028680030515     c                             and DB0vca <> §GEDcn
028690080701     c                   eval(h)   Pies   =  DB0ift + Wcas
028700030624     c                   clear                   Pie1
028710030624     c                   clear                   Pdi1
028720030515e   3c                   endsl
028730030515      *   C/Assegno non sommabile
028740030515x   2c                   Other
028750080701     c                   eval(h)   Pie1   =  DB0cas
028760080701     c                   eval(h)   Pies   =  DB0ift
028770030515e   2c                   Endsl
028780030709      *
028790030709e   1c                   ENDIF
028800030515      *
028810030515      * - Numero e data fattura
028820030515if  1c                   if        DB0nft <> *zeros
028830030515     c                   eval      Pkft   =  'FATTURA   '
028840030515     c                   eval      Pnft   =  DB0nft
028850130124     c                   eval      Pfiv   = '(' + %editc(wB0fiv:'X') +
028860130124     c                                      ')'
028870030515if  2c                   if        DB0dft >  *zeros
028880030515     c                   eval      Pkdf   =  'DEL'
028890030515     c     *iso          movel     DB0dft        Data_EUR
028900130418     c     *dmy          movel     Data_EUR      Pdft
028910130418     c*//  *eur          movel     Data_EUR      Pdft
028920030515e   2c                   endif
028930030515e   1c                   endif
028940030515      *
028950030515      * Memorizzo il codice tariffa come numero di copie da stampare
028960030515      *   (per Rimesso Mittente - non per DPD o FEDEX)
028970030515if  1c                   if        DB0ris = 'M'  and  *in70
028980030515     c                   eval      Wctr   =  DB0ctr
028990030515e   1c                   endif
029000030515      *
029010030515     c                   ENDSR
029020170517      *
029030170517      /free
029040170517       //--------------------------------------------------------------
029050170517       //?Controllo SE ragione sociale Mittente da FORZARE (TFNTC) per ?
029060170517       //?spedizione DPD.                                              ?
029070170517       //--------------------------------------------------------------
029080170517       BEGSR  sr_Mitt_DPD;
029090170517
029100170517         // -?Reperim. abilitazioni del Mittente dalla tab. "CLI"?
029110170517         clear  k05tntbe01;
029120170517         k_TBEcod = 'CLI';
029130170517         k_TBEke1 = %editc( DB0ccm : 'X' );
029140170517         chain  %kds( k05tntbe01 )  TNTBE000;
029150170517         if  Not %found( TNTBE01L )  or  TBEatb <> *blank;
029160170517           leavesr;
029170170517         endif;
029180170517         dCLI = TBEuni;
029190170517         // -?Cliente SENZA forzatura Mittente da note "AM" e "AS"?
029200170517         if  §CLItfntc <> 'S'  and  §CLInoIMtt <> 'S';
029210170517           leavesr;
029220170517         endif;
029230170517
029240170517         // -?Forzatura Mittente da note "AM"?
029250170517         If  §CLItfntc = 'S';
029260170517
029270170517           if  NOT %open( TFNTC01L );
029280170517             open  TFNTC01L;
029290170517           endif;
029300170517           clear  keyTFNTC01;
029310170517           k_NTCapl = 'C';
029320170517           k_NTCnk1 = %editc( DUTkci : 'X' ) + %editc( DB0ccm : 'X' );
029330170517           k_NTCtnt = 'AM';
029340170517           chain  %kds( keyTFNTC01 )  TFNTC;
029350170517           if  %found( TFNTC01L )  and  NTCsns = 'S';
029360170517             DB0rsm = NTCrnt;
029370170517           endif;
029380170517
029390170517         EndIf;
029400170517
029410170517         // -?Pulizia Indirizzo Mittente?
029420170517         If  §CLInoIMtt = 'S';
029430170517
029440170517           clear  DB0inm;
029450170517           clear  DB0cam;
029460170517           clear  DB0lom;
029470170517           clear  DB0prm;
029480170517           clear  DB0nzm;
029490170517
029500170517         EndIf;
029510170517
029520170517       ENDSR;
029530170517
029540170517      /end-free
029550030515      *
029560030515      *---------------------------------------------------------------*
029570050217      *?Inschieramento delle Note e delle Particolarità              ?*
029580030515      *---------------------------------------------------------------*
029590030515     c     SchPARTIC     BEGSR
029600030515      *
029610030515     c*** (già fatto:)   reset                   $Partic
029620030710     c*** (già fatto:)   reset                   $NoDesPartic
029630030515     c*** (già fatto:)   clear                   $NP
029640030717     c*** (già fatto:)   clear                   W_CoOr
029650030515     c                   clear                   XX
029660030710      *
029670030710      * Imposto eventuali note per prime
029680030710if  1c                   if        DB0nt1 <> *blanks
029690030710     c                   add       1             xx
029700030710     c                   movel     DB0nt1        $NP(xx)
029710030710e   1c                   endif
029720030710if  1c                   if        DB0nt2 <> *blanks
029730030710     c                   add       1             xx
029740030710     c                   movel     DB0nt2        $NP(xx)
029750030710e   1c                   endif
029760030515      *
029770030515      * Le particolarità possono essere lunghe fino a 45 char.
029780030515      *   Se i primi 30 (della 1ª riga a disposizione) non dovessero
029790030515      *   bastare, il resto lo si mette nella riga successiva.
029800030710      *   Se, invece, non risultano decodificate, si mettono solo i
029810030710      *   codici sulla stessa riga.
029820030710      *   Se uno solo risulta decodificato, esso va su una riga a
029830030710      *   parte (vedi flag "$NoDesPartic": *off = serve riga nuova
029840030710      *                                    *on  = ok riga procedente)
029850030515      *
029860030515      * . . Particolarità Giacenza
029870030515if  1c                   IF        DB0gga <> *blanks
029880030515     c                   movel     'Q'           WTpPar
029890030515     c                   movel     DB0gga        WCdPar
029900030515     c                   exsr      TABxPAR
029910030710if  2c                   if            TBLuni       = *blanks
029920030710     c                             and $NoDesPartic = *on
029930030710     c                   eval      $NP(xx) = %trim($NP(xx))
029940030710     c                                     + '   ' + WCdPar
029950030710x   2c                   else
029960030710if  3c                   if        TBLuni       = *blanks
029970030710     c                   eval      $NoDesPartic = *on
029980030710x   3c                   else
029990030710     c                   eval      $NoDesPartic = *off
030000030710e   3c                   endif
030010030515     c                   add       1             xx
030020100513     c                   movel     w053a         $NP(xx)
030030100513if  3c                   if        %subst(w053a : %len($NP(xx))+1)
030040030515     c                              <> *blanks
030050030515     c                   add       1             xx
030060030515     c                   eval      $NP(xx) = '   ' +
030070100513     c                             %trim(%subst(w053a:%len($NP(xx))+1))
030080030710e   3c                   endif
030090030515e   2c                   endif
030100030515e   1c                   ENDIF
030110030515      *
030120030515      * . . Particolarità Consegna
030130030515if  1c                   IF        DB0gma <> *blanks
030140030515     c                   movel     'R'           WTpPar
030150030515     c                   movel     DB0gma        WCdPar
030160030515     c                   exsr      TABxPAR
030170030710if  2c                   if            TBLuni       = *blanks
030180030710     c                             and $NoDesPartic = *on
030190030710     c                   eval      $NP(xx) = %trim($NP(xx))
030200030710     c                                     + '   ' + WCdPar
030210030710x   2c                   else
030220030710if  3c                   if        TBLuni       = *blanks
030230030710     c                   eval      $NoDesPartic = *on
030240030710x   3c                   else
030250030710     c                   eval      $NoDesPartic = *off
030260030710e   3c                   endif
030270030515     c                   add       1             xx
030280100513     c                   movel     w053a         $NP(xx)
030290100513if  3c                   if        %subst(w053a : %len($NP(xx)) + 1)
030300030515     c                              <> *blanks
030310030515     c                   add       1             xx
030320030515     c                   eval      $NP(xx) = '   ' +
030330100513     c                             %trim(%subst(w053a:%len($NP(xx))+1))
030340030710e   3c                   endif
030350030515e   2c                   endif
030360030515e   1c                   ENDIF
030370030515      *
030380030515      * . . Particolarità Varie
030390030717      *     - questa descrizione va messa nell'apposito campo PPAD
030400041015      *       se è "PRIVATO" idem se è "CONSEGNA AD UFFICI"
030410150224     *** *     - se 'B' (bancali da rendere al mittente) comparirà un
030420150224     *** *       apposito box
030430150224     *** *     - se 'O' (oltre 20 colli => bancali da riportare in mag.)
030440150224     *** *       comparirà un apposito box, un'apposita nota e l'indica-
030450150224     *** *       zione nella copia per il destinatario.
030460030717      *     ...Potrebbero anche non esserci più elementi di schiera
030470030717      *     disponibili [xx >= %elem($NP)] !
030480030709if  1c                   IF            DB0gva <> *blanks
030490150224     c*//                          and DB0gva <> 'B '
030500150224     c*//                          and DB0gva <> 'O '
030510030709     c                             and DB0gva <> 'P '
030520041015     c                             and DB0gva <> 'U '
030530030515     c                   movel     'S'           WTpPar
030540030515     c                   movel     DB0gva        WCdPar
030550030515     c                   exsr      TABxPAR
030560030710if  2c                   if            TBLuni       = *blanks
030570030710     c                             and $NoDesPartic = *on
030580030710     c                   eval      $NP(xx) = %trim($NP(xx))
030590030710     c                                     + '   ' + WCdPar
030600030710x   2c                   else
030610030710if  3c                   if        TBLuni       = *blanks
030620030710     c                   eval      $NoDesPartic = *on
030630030710x   3c                   else
030640030710     c                   eval      $NoDesPartic = *off
030650030710e   3c                   endif
030660030717if  3c                   if            xx < %elem($NP)
030670030709     c                   add       1             xx
030680100513     c                   movel     w053a         $NP(xx)
030690100513if  4c                   if        %subst(w053a : %len($NP(xx)) + 1)
030700030709     c                              <> *blanks
030710030717     c                             and xx < %elem($NP)
030720030709     c                   add       1             xx
030730030709     c                   eval      $NP(xx) = '   ' +
030740100513     c                             %trim(%subst(w053a:%len($NP(xx))+1))
030750030717e   4c                   endif
030760030717e   3c                   endif
030770030709e   2c                   endif
030780030515e   1c                   ENDIF
030790030717      *
030800030717      * Imposto il numero originale di colli componenti la spedizione
030810030717      * e se ho ancora spazio lo memorizzo in "Note e Particolarità"
030820030717if  1c                   if        %subst(DB0gva:1:1) = 'O'
030830030717     c                   exsr      RepAR5bnb
030840030717if  2c                   if        W_CoOr  <> *blanks
030850030717     c                             and xx  <  %elem($NP)
030860030717     c                   add       1             xx
030870030717     c                   eval      $NP(xx) =  W_CoOr
030880030717e   2c                   endif
030890030717e   1c                   endif
030900030515      *
030910030515      * Imposto il flag per memorizzare la già avvenuta memorizzazione
030920030715      *   delle "Note e Particolarità"
030930030515     c                   eval      $Partic = *on
030940030515      *
030950030515     c                   ENDSR
030960070319      *
030970070319      *---------------------------------------------------------------*
030980070319      *?Inschieramento delle Consegne Parziali precedenti            ?*
030990070319      *---------------------------------------------------------------*
031000070319     c     SchConsParz   BEGSR
031010070319      *
031020070319     c*** (già fatto:)   reset                   $CP
031030070319     c*** (già fatto:)   reset                   $CPdcm
031040070319     c                   clear                   XX
031050070319      *
031060070319if  2c                   if        NOT %open(FNART01L)
031070070319     c                   open      FNART01L
031080070319e   2c                   endif
031090070319      *
031100070514if  2c                   if        $Mamma = *off
031110070319     c     KLDV          setll     FNART000
031120070319     c     KLDV          reade     FNART000
031130070514x   2c                   else
031140070514     c     KLBL          setll     FNART000
031150070514     c     KLBL          reade     FNART000
031160070514e   2c                   endif
031170070319      *
031180070319do  1c                   DOW       NOT  %eof(FNART01L)
031190070319      *
031200070319if  2c                   If             ARTatb = *blanks
031210070319     c                             and  ARTdcm > *zeros
031220070319      *
031230070319     c                   eval      *in31  = *off
031240070319     c                   eval      xx     = 1
031250070319     c     ARTdcm        lookup    $CPdcm(xx)                             31
031260070319if  3c                   if        NOT  *in31
031270070319     c                   eval      xx     = 1
031280070319     c     *zeros        lookup    $CPdcm(xx)                             31
031290070319if  4c                   if        *in31
031300140304     c*//                eval      $CP(xx)     = *zeros
031310140304     c                   clear                   Elem_CP
031320140304     c                   movel     Elem_CP       $CP(xx)
031330070319e   4c                   endif
031340070319e   3c                   endif
031350070319if  3c                   if        *in31
031360070319     c                   eval      $CPdcm(xx)  = ARTdcm
031370070319     c                   movel     $CP(xx)       Elem_CP
031380070319     c                   eval      WCPdcm      = ARTdcm
031390070319     c                   eval      WCPncl      = WCPncl + 1
031400070319     c                   movel     Elem_CP       $CP(xx)
031410070319e   3c                   endif
031420070319      *
031430070319e   2c                   EndIf
031440070319      *
031450070514if  2c                   if        $Mamma = *off
031460070319     c     KLDV          reade     FNART000
031470070514x   2c                   else
031480070514     c     KLBL          reade     FNART000
031490070514e   2c                   endif
031500070319      *
031510070319e   1c                   ENDDO
031520070319      *
031530140513if  0c                   if        xx     > *zero
031540140513      * Reperimento nominativi firmatari
031550140513     c                   exsr      sr_Firmatario
031560070319      * Ordinamento schiera per data
031570070319if  1c                   if        xx     > 1
031580070319     c                   sorta                   $CP
031590070319e   1c                   endif
031600140513e   0c                   endif
031610070319      *
031620070319     c                   ENDSR
031630140303      *
031640140303      /free
031650140303
031660140303       //--------------------------------------------------------------
031670140303       //?Reperimento nome firmatario                                  ?
031680140303       //--------------------------------------------------------------
031690140303       BEGSR  sr_Firmatario;
031700140303
031710140303if  1    if  NOT  %open(FNEVB22L);
031720140303           open  FNEVB22L;
031730140303e   1    endif;
031740140303
031750140303         clear  keyFNEVB22;
031760140303         k_EVB2aas = DB0aas;
031770140303         k_EVB2lnp = DB0lnp;
031780140303         k_EVB2nrs = DB0nrs;
031790140303         k_EVB2nsp = DB0nsp;
031800140303         k_EVB2cev = 'P  ';
031810140303         setll  %kds( keyFNEVB22 : 4 )  FNEVB200;
031820140303         reade  %kds( keyFNEVB22 : 4 )  FNEVB200;
031830140303         DoW  Not  %eof(FNEVB22L);
031840140303           if  EVB2cev = k_EVB2cev  and  EVB2trd = 'FIR';
031850140305             yy = %lookup( EVB2dev : $CPdcm );
031860140305             if  yy > *zero;
031870140305               Elem_CP = $CP(yy);
031880140303               if  WCPocm  <= EVB2hev;
031890140303                 WCPocm  = EVB2hev;
031900140303                 WCPfir  = EVB2dati;
031910140305                 $CP(yy) = Elem_CP;
031920140303               endif;
031930140303             endif;
031940140303           endif;
031950140304           reade  %kds( keyFNEVB22 : 4 )  FNEVB200;
031960140303         EndDo;
031970140303
031980140303       ENDSR;
031990140303
032000140303      /end-free
032010031124      *
032020031124      *---------------------------------------------------------------*
032030050217      *?Reperimento Nominativo e Telefono destinatario da FIAR5/GEN  ?*
032040031124      *---------------------------------------------------------------*
032050031124     c     RepAR5gen     BEGSR
032060031124      *
032070031124     c                   clear                   dAR5gen
032080031124      *
032090031124     c                   eval      AR5trd = 'GEN'
032100040301if  1c                   if        DUTlpo = 'S'
032110040531     c     K05AR501      chain     FIAR531C
032120031127x   1c                   else
032130031127     c     K05AR501      chain     FIAR5000
032140031127e   1c                   endif
032150031124      *
032160040301if  1c                   if           (DUTlpo =  'S'
032170040531     c                             and %found(FIAR531C)
032180040301     c                             OR  DUTlpo <> 'S'
032190040301     c                             and %found(FIAR501L))
032200040301     c                             and AR5atb =  *blanks
032210031124     c                   movel     AR5uni        dAR5gen
032220031124e   1c                   endif
032230031124      *
032240031124     c                   ENDSR
032250030717      *
032260030717      *---------------------------------------------------------------*
032270050217      *?Reperimento Numero Colli Originale da FIAR5/BNB              ?*
032280030717      *---------------------------------------------------------------*
032290030717     c     RepAR5bnb     BEGSR
032300030717      *
032310030717     c                   clear                   dAR5bnb
032320030717     c*** (già fatto:)   clear                   W_CoOr
032330030717      *
032340030717     c                   eval      AR5trd = 'BNB'
032350040301if  1c                   if        DUTlpo = 'S'
032360040531     c     K05AR501      chain     FIAR531C
032370040301x   1c                   else
032380030717     c     K05AR501      chain     FIAR5000
032390040301e   1c                   endif
032400030717      *
032410040301if  1c                   if           (DUTlpo =  'S'
032420040531     c                             and %found(FIAR531C)
032430040301     c                             OR  DUTlpo <> 'S'
032440040301     c                             and %found(FIAR501L))
032450040301     c                             and AR5atb =  *blanks
032460030717     c                   movel     AR5uni        dAR5bnb
032470030717if  2c                   if        §AR5bcor <> *zeros
032480030717     c                   eval      W_CoOr   =  C_CoOr
032490030717     c                                      +  %editc(§AR5bcor : 'K')
032500030717e   2c                   endif
032510030717e   1c                   endif
032520030717      *
032530030717     c                   ENDSR
032540030515      *
032550030515      *---------------------------------------------------------------*
032560051129      *?Decodifiche condizionate in Lettera di Vettura               ?*
032570030515      *---------------------------------------------------------------*
032580040607     c     DecodLDV      BEGSR
032590030515      *
032600030515      * Richiesta firma per ricevuta della merce senza riserve
032610030515      * (e NON  quella  per quietanza) => *in35 + not *in36
032620030515     c                   movea     '10'          *in(35)
032630101130      *
032640101130      /free
032650101130
032660101130         // -?Memorizzazione consegne parziali precedenti dal file?
032670101130         //  ?FNART00F - serve anche nella stampa delle "tagliatelle"?
032680140513         IF  $ConsParz;
032690140513
032700140513           // -?SE si stanno stampando "tagliatelle":?
032710140514           //  ?le consegne parziali vengono indicate nel Box1,?
032720140514           //  ?SE?ne sono state reperite?
032730140513           IF  DB0fa4 = '4'  and  $CP(1) < *all'9';
032740140513
032750101130             // -?Prima si totalizzano le quantità consegnate?
032760140513             clear  w0080;
032770140513             clear  w0140;
032780101130             For  xx = 1  To  %elem($CP);
032790101130               if  $CP(xx) < *all'9';
032800101130                 Elem_CP = $CP(xx);
032810101130                 monitor;
032820101130                   Data_Eur = %date( WCPdcm : *iso );
032830101130                 on-error;
032840101130                   Data_Eur = *loval;
032850101130                 endmon;
032860101130                 if       Data_Eur  > *loval  and
032870101130                     %dec(Data_Eur) > w0080;
032880101130                   w0080 = %dec(Data_Eur);
032890101130                 endif;
032900101130                 w0140 += WCPncl;
032910101130               endif;
032920101130             EndFor;
032930101130             // -?Poi si stampa un'unica indicazione?
032940101201             Data_DMY = %date( w0080 : *eur );
032950101201             Pcpp = 'Consegnati '
032960101201                  + %triml( %editc( w0140 : 'Z' ) )
032970101201                  + ' colli al '
032980101201                  + %triml( %editc( %dec( Data_DMY ) : 'Y' ) );
032990140513
033000140513           ENDIF;
033010140513
033020140513         ENDIF;
033030101130
033040101130      /end-free
033050070307      *
033060070307      * Intestazione moduli
033070070307      * - Se spedizione in stato "Lasciato Avviso"
033080070307      *   si stampano anche data/ora del 1° lasciato avviso
033090070307if  1c                   if        $LascAvv  = *on
033100131205     c                   z-add     wEVBdev       w0080
033110070315     c     *iso          test(d)                 w0080                  31
033120070315if  2c                   if        NOT *in31
033130071025     c     *iso          movel     w0080         Data_EUR
033140071025     c     *eur          movel     Data_EUR      w0080
033150070315x   2c                   else
033160070315     c                   clear                   w0080
033170070315e   2c                   endif
033180070307     c                   eval      Tla    =  'Lasciato avviso il '
033190070307     c                                    +  %editc(w0080 : 'Y')
033200070307     c                                    +  ' alle '
033210131205     c                                    +  %editw(wEVBhev : '  :  ')
033220070307e   1c                   endif
033230030709      *
033240100107     *** * Evidenziazione servizio "espresso"
033250100107     *** * o del servizio "Poste italiane"
033260100107     ***c                   eval      *in37 = (DB0tsp = 'E')
033270100107     ***c  n37              eval      *in37 = (§OGntw = 'PPT')
033280090611      *
033290090605      /free
033300100107
033310101130         // -?Evidenziazione servizio "espresso"?
033320101130         //           ?o del servizio "Poste Italiane"?
033330100107         *in37 = (DB0tsp = 'E'   or   DB0tsp = 'H'   or
033340100107                  §OGntw = 'PPT');
033350100107
033360101130         // -?Consegna al mattino (tempi consegna = 12 ore)?
033370101130         //  ?(NON per Würth)?
033380090615         if  §VPOrcm <= wDate and
033390090622             DUTlpo  <> 'S'   and
033400090615             DB0tsp   = 'E'   and
033410090615            (DB0nrs  <> 40    or  (DB0lnp <> 043   and
033420110429                                   DB0lnp <> 277   and
033430090605                                   DB0lnp <> 063));
033440090605           clear  TNSD99ds;
033450090605           if  DB0tbo = 'P'   or   DB0tbo = '8';
033460090605             D98tbo = 'P';
033470090605           else;
033480090605             D98tbo = 'A';
033490090605           endif;
033500090605           D98aas = DB0aas;
033510090605           D98lnp = DB0lnp;
033520090605           D98nrs = DB0nrs;
033530090605           D98nsp = DB0nsp;
033540090605           tnsd99r ( TNSD99ds );
033550120910           //*if  D98tci = 12;
033560120910           if  D98tcc = 12;
033570090605             Pdsm = 'M';
033580090605           endif;
033590090605         endif;
033600090605      /end-free
033610030515      *
033620030515      * Franco prepagato
033630030515      * (richiede firma per quietanza nella copia x destinatario)
033640030515if  1c                   if        DB0tbo =  '8'  and  DB0ris <> 'M'
033650030515     c                   movea     '01'          *in(35)
033660030515if  2c                   if        Pds1   =  *blanks
033670030515     c                   eval      Pds1   =  'FRANCO PREPAGATO        '
033680030515e   2c                   endif
033690120530     c*//                if        (DB0dft > *zero       and
033700120530     c*//                           DB0dft > §vpoBRTdta)  OR
033710120530     c*//                          (DB0dft = *zero       and
033720120530     c*//                           Wdsp   > §vpoBRTdta)
033730130418     c                   eval      Pbrt2  =  $BRT
033740120530     c*//                else
033750130418     c*//                eval      Pbrt2  =  $Bartolini
033760120530     c*//                endif
033770030515      * - Legge 450 (solo per prepagati)
033780030515if  2c                   if            DB0ias =  *zeros
033790030515     c                             and DB0vas =  *blanks
033800030515     c                             and DB0div =  §GEDcn
033810060301     c                             and §GElrp >  *zeros
033820030515     c*** v. IF sovrastante...     and DB0tbo =  '8'
033830060301if  3c                   if        Wdsp   >  20060228
033840110315     c                   z-add     §GElrp        w0112
033850080701     c                   eval      Prcv   =  'Art. 1696 c.c. mod. +
033860061102     c                                       dlgs 286/2005 '
033870060301     c                                    +  §GEDcn + ' '
033880060301     c                                    +  %trim(%editc(w0112 : '4'))
033890060301     c                                    +  ' al kg.'
033900060301x   3c                   else
033910110315     c                   z-add     6.20          w0112
033920080701     c                   eval      Prcv   =  'Legge 450/85 '
033930030515     c                                    +  §GEDcn + ' '
033940030624     c                                    +  %trim(%editc(w0112 : '4'))
033950030515     c                                    +  ' al kg.'
033960060301e   3c                   endif
033970030515e   2c                   endif
033980030515e   1c                   endif
033990031031      *
034000101125      /free
034010121017
034020121017         // -?Indicazione della Partiva IVA o del Cod. Fiscale?
034030121017         //  ?(se Porto Franco Prepagato?
034040121017         //    ?o Porto Assegnato con Fattura immediata)?
034050121017         //  ?- NON se mittente estero -?
034060121017             // -?con fattura?
034070121017         IF  (DB0nft <> *zero  or   DB0ift <> *zero)   AND     (
034080121017             // - -?P.F. prepagato?
034090121017             //    ?(con mittente italiano)?
034100121017             (DB0tbo =  '8'             and  DB0nzm  = *blank)  or
034110121017             // - -?P.A. (con fattura immediata)?
034120121017             //    ?(con destinatario italiano)?
034130121017             (%subst(DB0tb1:1:1) = 'A'  and  DB0nzd  = *blank)  or
034140121017             // - -?con 2ª bolla?
034150121017             //    ?(con destinatario italiano)?
034160121017             (DB0tb2 <> *blank          and  DB0tbo <> '8'
034170121029                                        and  DB0tbo <> 'P'
034180121017                                        and  DB0nzd  = *blank) );
034190121017           select;
034200121017             when  Not $No_PI  and  wB0piv <> *blank  and
034210121017                   // ·?Esiste Partita Iva NON FITTIZIA?
034220121026                   %scan('$$' : wB0piv) = *zero;
034230121017                   // ·?Non è un C.F. - lungo 16 (privato)?
034240121026                   //and  %len( %trim( wB0cfi ) ) < 16;
034250121017               Ppiv = 'Partita Iva: ' + wB0piv;
034260121017             when  Not $No_CF  and  wB0cfi <> *blank;
034270121017               Ppiv = 'Codice Fiscale: ' + wB0cfi;
034280121017           endsl;
034290130418           //select;
034300130418           //  // ·?Se P.F. Prepagato il C.F./P.I. è del Mittente?
034310130418           //  when  DB0tbo = '8';
034320130418           //    PpivM = Ppiv;
034330130418           //  // ·?Se P.A. con fattura immed. il C.F./P.I. è del Dest.?
034340130418           //  when  %subst(DB0tb1:1:1) = 'A';
034350130418           //    PpivD = Ppiv;
034360130418           //  // ·?Se 2ª bolla (DDT sì) è del Dest.?
034370130418           //  when  DB0tb2  <> *blanks  and  DB0tbo  <> 'P';
034380130418           //    PpivD = Ppiv;
034390130418           //endsl;
034400121017         ENDIF;
034410101125
034420101125         Select;
034430101125
034440101125           // -?Non utilizzare per la consegna?
034450101125           //  ?(stampa della sola copia per il vettore)?
034460101125           When  $No_x_Cons = *on;
034470101125             *in35 = *off;
034480101125             *in36 = *off;
034490101125             // -?costante da stampare se LDV recapito c/assegni?
034500101125             //  ?stampata dal pgm "Stampa DDT NO in arrivo"?
034510101129             Pnux  = 'NON UTILIZZARE PER LA CONSEGNA';
034520101125
034530101125           // -?Ritorno all'Incasso?
034540101125           When  $Rit_a_Inc = *on;
034550101125             *in40 = *on;
034560101125             Pnux  = 'RITORNO ALL''INCASSO';
034570101125
034580101125         EndSl;
034590101125
034600101125      /end-free
034610030910      *
034620030910      * Bollo assolto in modo virtuale
034630030910if  1c                   if        $BolloVirt =  *on
034640060719if  2c                   if        DB0dft <  20060801
034650080701     c                   eval      Pbav1  =  'Bollo '
034660080701     c                                    +  %trim(%editc(wBolloVirt:'4'))
034670080701     c                                    +  ' ' + DB0div
034680080701     c                                    +  ' assolto in modo virtuale '
034690080701     c                   eval      Pbav2  =  'Aut. Ag. Ent. BO 3 27/09/02 '
034700080701     c                                    +  'Prot. 31599'
034710060719x   2c                   else
034720080701     c                   eval      Pbav1  =  'Bollo '
034730080701     c                                    +  %trim(%editc(wBolloVirt:'4'))
034740080701     c                                    +  ' ' + DB0div
034750080701     c                                    +  ' assolto in modo virtuale '
034760080701     c                   eval      Pbav2  =  'Aut. Ag. Ent. MI 2 10/07/06 '
034770080701     c                                    +  'Prot. 45114'
034780060719e   2c                   endif
034790030910e   1c                   endif
034800050503      *
034810080605      * Costanti "PER RICEVUTA DELLA MERCE..."
034820080605     c                   eval      Pkrm1 = '(nome e cognome in stampa+
034830080605     c                                       tello)'
034840080605     c                   eval      Pkrm2 = '(firma)'
034850080605     c                   eval      Pkrm3 = '(data e ora)'
034860050504sel 1c                   select
034870050504      * - danon?stampare se
034880050504      *   · franco prepagato                (NOT *in35)
034890050504      *   · non utilizzare per la consegna  (NOT *in35)
034900101125      *   · ritorno all'incasso             (*in40)
034910050504      *   · bolla FedEx                     (*in72)
034920101125w   1c                   when      NOT *in35  or  *in40  or  *in72
034930050504     c*** (già fatto:)   clear                   Pkrm
034940080605     c                   clear                   Pkrm1
034950080605     c                   clear                   Pkrm2
034960080605     c                   clear                   Pkrm3
034970080922      * - costante "DPD"
034980120301w   1c                   when      *in71
034990080922     c                   eval      Pkrm   =  'PER RICEVUTA DELLA MERCE-
035000080922     c                              SENZA RISERVE'
035010120530      * - costante "standard" per "DDT sì"
035020090615w   1c                   when          §VPOrsr <= wDate
035030081119     c                             and  DB0bam =  'S'
035040071005     c                   eval      Pkrm   =  'PER RICEVUTA DELLA MERCE-
035050071005     c                              E DEL DOCUMENTO DI TRASPORTO SENZA-
035060071005     c                              RISERVE'
035070120530      * - costante "standard" per "DDT no" + Packing-List
035080120301w   1c                   when           DB0bam =  *blank
035090161012     c                             and ($FIAR8 =  *on
035100161012     c                              or  $§PDF  =  *on)
035110120301     c                   eval      Pkrm   =  'PER RICEVUTA DELLA MERCE-
035120120301     c                              E DEL DOCUMENTO DI TRASPORTO SENZA-
035130120301     c                              RISERVE'
035140120301      * - costante "standard" altrimenti
035150050504x   1c                   other
035160050504     c                   eval      Pkrm   =  'PER RICEVUTA DELLA MERCE-
035170050504     c                              SENZA RISERVE'
035180050504e   1c                   endsl
035190090513      /free
035200090611       Select;
035210101130         // -?costante per cliente?
035220101130         //  ?"Consorzio Lotterie Nazionali - Gratta & Vinci"?
035230140513         when  DB0ccm = c_CliCLN  or  DB0ccm = c_CliLN_2;
035240090528           exsr  sr_RepDdtCLN;
035250090706           Pkrm =  'LA FIRMA PER RICEVUTA DELLA MERCE VALE ANCHE PER +
035260090706                    IL DDT N° '
035270090527                + %editc (WddtCLN : 'Z')
035280090615                + ' DEL '
035290090527                + %editc (WdspCLN : 'Y')
035300101001                + ' CON RISERVA DI CONTROLLO COME DA CONTRATTO';
035310101001           if  Wdsp < 20101001;
035320101001             Pkrm = %trimr(Pkrm) + ' C.L.N.';
035330101001           else;
035340101001             Pkrm = %trimr(Pkrm) + ' L.N. S.R.L.';
035350101001           endif;
035360101130         // -?costante per cliente?
035370101130         //  ?"S G Soluzioni Logistiche"?
035380091117         when  DB0ccm = c_CliSG   or   DB0ccm = c_CliSG_2;
035390091117           if  DB0nas <> *blank   and   DB0nas <> c_NoContr;
035400091117             Pkrm = %trimr(Pkrm)
035410091117                  + '.   INCLUDE CONTRATTO - '
035420091117                  + %trim(DB0nas);
035430091117           endif;
035440091117           if  DB0rmo <> *blank;
035450091117             if  DB0nas <> *blank;
035460091117               Pkrm = %trimr(Pkrm)
035470091117                    + ' - MATR TV: '
035480091117                    + %trim(DB0rmo);
035490091117             else;
035500091117               Pkrm = %trimr(Pkrm)
035510091117                    + '.   MATR TV: '
035520091117                    + %trim(DB0rmo);
035530091117             endif;
035540091117           endif;
035550090611       EndSl;
035560090513      /end-free
035570030717      *
035580130418      *//* Indicazione degli "oltre 20 colli"
035590130418      *//* (MAI se Franco prepagato)
035600130418if  1c*//                   if            DB0tbo <> '8'
035610130418     c*//                             and *in36  =  *off
035620130418     c*//                             and W_CoOr <> *blanks
035630130418     c*//                   eval      Pbrt2  = W_CoOr
035640130418e   1c*//                   endif
035650030515      *
035660030515      * Verifico se imponibile/iva/totale/estremi fattura
035670030515     c                   eval      *in19  = ((DB0imv <> *zeros)  or
035680030515     c                                       (DB0iva <> *zeros)  or
035690030515     c                                       (DB0cei <> *blanks) or
035700030515     c                                       (DB0dce <> *blanks) or
035710030515     c                                       (DB0ift <> *zeros)  or
035720030515     c                                       (DB0nft <> *zeros)  or
035730031125     c                                       (DB0dft <> *zeros))
035740030515      *
035750030515      * Verifico se contrassegno
035760030515     c                   eval      *in20  =  (Pcas <> *zeros)
035770030515      *
035780030709      * Verifico se totale da esigere al ritiro o alla consegna
035790080701     c                   eval      *in21  =  (Pies <> *zeros)
035800030709      *
035810030710      * Intesto il 2° modulo diversamente per prepagato
035820030710      * (solo in partenza)
035830030710if  1c                   if        *in21  and  *in36
035840030710     c                             and   (DB0tbo = 'P'
035850030710     c                                or  DB0tbo = '8')
035860030710     c                   eval      Tcxd   =  'COPIA PER IL MITTENTE    '
035870130726     c                   clear                   Tcxd2
035880030710e   1c                   endif
035890030515      *
035900120530      * Verifico se codice a barre BRT da stampare
035910030515     c                   eval      *in50  =  (Tbcd <> *blanks)
035920040629      * Verifico se codice a barre O.R.M.    da stampare
035930040629      * (non è ancora impostato - vedi subr. "StampaRCC")
035940040629     c***                eval      *in52  =  (Pbco <> *blanks)
035950030515      *
035960030515      * Flag "*" > x Consegne particolari
035970030515     c                   if        DB0tc1 =  'A'  or  DB0tc2 = 'A'
035980030515     c                   eval      Past   =  '*'
035990030515     c                   endif
036000030515      *
036010030515      * C/ASSEGNO - - - - - - - - - - - - - - - - - - - - - - - - - - *
036020030515if  1c                   IF        DB0cas <> *zeros
036030030515      * - Flag "*" > x C/Assegno
036040030904if  2c                   if        O21fca <> *blanks
036050030904     c                   movel     '*'           Past
036060030515e   2c                   endif
036070030624      * - Imposto i campi di stampa
036080030624     c                   eval      Ptic   =  DB0tic + ' ' + DB0dti
036090030515e   1c                   ENDIF
036100030515      *
036110040607      * TIPO LDV  - - - - - - - - - - - - - - - - - - - - - - - - - - *
036120030515      * - Se Bolla in Assegnato: stampo una "A" vicino all'asterisco
036130030515      * e se Bolla in Assegnato Senza Fattura: stampo "SEGUE FATTURA"
036140030515if  1c                   if           %subst(DB0tb1:1:1) = 'A'
036150030515     c                             or        DB0tb2     <> *blanks
036160030515     c                   eval      Pass   =  'A'
036170030515if  2c                   if        DB0nft =  *zeros
036180031125     c                   eval      *in22  =  *on
036190130215     c                   eval      Piva   =  'SEGUE FATTURA'
036200030515e   2c                   endif
036210030515e   1c                   endif
036220030515      *
036230030515      * DECODIFICA TIPO CONSEGNA RICHIESTA  - - - - - - - - - - - - - *
036240051129sel 1***c                   Select
036250051129w   1***c                   WHEN      DB0tcr = *blanks
036260051129     *** * - giorno si
036270051129w   1***c                   WHEN      DB0dcr <> *zeros  and  DB0tcr = 'P'
036280051129     ***c                   eval      Ptdr   =  'PRIMA DEL  '
036290051129w   1***c                   WHEN      DB0dcr <> *zeros  and  DB0tcr = 'D'
036300051129     ***c                   eval      Ptdr   =  'DOPO IL    '
036310051129     *** * - giorno no  ora si
036320051129w   1***c                   WHEN      DB0dcr =  *zeros  and  DB0tcr = 'P'
036330051129     ***c                   eval      Ptdr   =  'PRIMA DELLE'
036340051129w   1***c                   WHEN      DB0dcr =  *zeros  and  DB0tcr = 'D'
036350051129     ***c                   eval      Ptdr   =  'DOPO LE    '
036360051129e   1***c                   ENDSL
036370051129if  1c                   IF            DB0dcr >  *zeros
036380051129     c                             or  DB0hcr >  *zeros
036390051129     c                   clear                   FNLV80ds
036400051129     c                   eval      ILV80aas   =  DB0aas
036410051129     c                   eval      ILV80lnp   =  DB0lnp
036420051129     c                   eval      ILV80nrs   =  DB0nrs
036430051129     c                   eval      ILV80nsp   =  DB0nsp
036440051129     c                   eval      ILV80mgs   =  DB0mgs
036450051129     c                   eval      ILV80tc1   =  DB0tc1
036460051129     c                   eval      ILV80tc2   =  DB0tc2
036470051129     c                   eval      ILV80dcr   =  DB0dcr
036480051129     c                   eval      ILV80hcr   =  DB0hcr
036490051129     c                   eval      ILV80tcr   =  DB0tcr
036500051129     c                   call      'FNLV80R'
036510051129     c                   parm                    KPJBA
036520051129     c                   parm                    FNLV80ds
036530051129sel 2c                   Select
036540051129      * errore
036550051129w   2c                   When      OLV80err  <>  *blanks
036560051129      * consegna tassativa (per ora solo ai supermercati)
036570051129w   2c                   When      OLV80tat   =  'S'
036580051129     c                   movel     OLV80tcrd     Pcot
036590091116      * NO "Prima del" per il cliente TTL
036600091116w   2c                   When      DB0ccm     = c_CliTTL   and
036610091116     c                             DB0dcr    <> *zeros     and  DB0tcr = 'P'
036620051129      * prima/dopo il/delle
036630051129      * (il giorno e/o l'ora di consegna richiesta sono già impostati
036640051129      *  nella subr. "TipoLDV")
036650051129w   2c                   Other
036660051129     c                   movel     OLV80tcrd     Ptdr
036670051129e   2c                   Endsl
036680051129e   1c                   ENDIF
036690030515      *
036700030515      * DECODIFICA DEI TURNI DI CHIUSURA  - - - - - - - - - - - - - - *
036710060921      * (SE?non?è "Fermo Deposito")
036720060921if  1c                   if        DB0ffd =  *blanks
036730060921     c                   eval      Pktc   =  'TURNO DI CHIUSURA'
036740060921e   1c                   endif
036750030515      * 1° turno di chiusura
036760060921if  1c                   IF             DB0gc1 <> *blanks
036770060921     c                             and  DB0ffd  = *blanks
036780030515      * - Flag "*" > Turno di chiusura
036790030515     c                   eval      Past   =  '*'
036800030515if  2c                   If        %subst(DB0gc1:1:1) = *blank
036810030515      * - Nessun giorno specificato - Solo 'M' o 'P'
036820060921     c*** già così:      clear                   Pgc1
036830030515if  3c                   if        DB0gc1 =  ' M'
036840030515     c                   eval      Pgc1   =  %trim($Gio(8))
036850030515x   3c                   else
036860030515     c                   eval      Pgc1   =  %trim($Gio(9))
036870030515e   3c                   endif
036880030515x   2c                   Else
036890030515      * - Giorno specificato
036900030515     c                   movel     DB0gc1        w001a
036910030515     c                   eval      XX     =  1
036920030515     c                   eval      *in31  =  *off
036930030515     c     w001a         lookup    $GgS(xx)                               31
036940030515     c   31              movel     $Gio(xx)      Pgc1
036950030515      *   + 'M' o 'P'
036960030515sel 3c                   select
036970030515w   3c                   when      %subst(DB0gc1:2:1) = 'M'
036980030515     c                   eval      Pgc1   =  %trim(Pgc1) + ' '
036990030515     c                                    +  %trim($Gio(8))
037000030515w   3c                   when      %subst(DB0gc1:2:1) = 'P'
037010030515     c                   eval      Pgc1   =  %trim(Pgc1) + ' '
037020030515     c                                    +  %trim($Gio(9))
037030030515e   3c                   endsl
037040030515e   2c                   Endif
037050030515e   1c                   ENDIF
037060030515      * 2° turno di chiusura
037070060921if  1c                   IF             DB0gc2 <> *blanks
037080060921     c                             and  DB0ffd  = *blanks
037090030515      * - Flag "*" > Turno di chiusura
037100030515     c                   eval      Past   =  '*'
037110030515if  2c                   If        %subst(DB0gc2:1:1) = *blank
037120030515      * - Nessun giorno specificato - Solo 'M' o 'P'
037130060921     c*** già così:      clear                   Pgc2
037140030515if  3c                   if        DB0gc2 =  ' M'
037150030515     c                   eval      Pgc2   =  %trim($Gio(8))
037160030515x   3c                   else
037170030515     c                   eval      Pgc2   =  %trim($Gio(9))
037180030515e   3c                   endif
037190030515x   2c                   Else
037200030515      * - Giorno specificato
037210030515     c                   movel     DB0gc2        w001a
037220030515     c                   eval      XX     =  1
037230030515     c                   eval      *in31  =  *off
037240030515     c     w001a         lookup    $GgS(xx)                               31
037250030515     c   31              movel     $Gio(xx)      Pgc2
037260030515      *   + 'M' o 'P'
037270030515sel 3c                   select
037280030515w   3c                   when      %subst(DB0gc2:2:1) = 'M'
037290030515     c                   eval      Pgc2   =  %trim(Pgc2) + ' '
037300030515     c                                    +  %trim($Gio(8))
037310030515w   3c                   when      %subst(DB0gc2:2:1) = 'P'
037320030515     c                   eval      Pgc2   =  %trim(Pgc2) + ' '
037330030515     c                                    +  %trim($Gio(9))
037340030515e   3c                   endsl
037350030515e   2c                   Endif
037360030515e   1c                   ENDIF
037370071114      *
037380071114      * ALTRI DATI/FLAG/... - - - - - - - - - - - - - - - - - - - - - *
037390030515      *
037400071114      * Se pgm richiamato x stampa LdV in arrivo:
037410030515if  1c                   IF        DB0fl1 =  'A'
037420071114      * - imposto l'"*" in base al parametro ricevuto.
037430030515if  2c                   if        DB0fl2 =  '*'
037440030515     c                   eval      Past   =  '*'
037450030515x   2c                   else
037460030515     c                   clear                   Past
037470030515e   2c                   endif
037480071114      * - idem per flag di bolla non abilitata
037490030515     c                   eval      Pdis   =  DB0fl5
037500030515e   1c                   endif
037510061102      *
037520071114      * Decodifica cliente a cui richiedere il codice fiscale e/o la
037530071114      *   partita iva
037540071114if  1c                   IF             $No_CF =  *on
037550071114     c                             or   $No_PI =  *on
037560061106     c                   clear                   wRSxCF
037570061102sel 2c                   select
037580061102      * - P.F. prepagato
037590061106w   2c                   when      DB0tbo =  '8'
037600061106     c                   movel     DB0rsm        wRSxCF
037610061102      * - P.A. con fattura immediata
037620061106w   2c                   when      %subst(DB0tb1:1:1) = 'A'
037630061106     c                   movel     DB0rsd        wRSxCF
037640061106      * - con 2ª bolla
037650061103w   2c                   when           DB0tb2 <> *blanks
037660121029     c                             and  DB0tbo <> 'P'
037670061103     c                             and  DB0tbo <> '8'
037680061106     c                   movel     DB0rsd        wRSxCF
037690061102e   2c                   endsl
037700061102      *
037710061102e   1c                   ENDIF
037720030515      *
037730030515     c                   ENDSR
037740080918      /free
037750090528
037760090528       //--------------------------------------------------------------
037770140303       //?Reperimento Numero e Data del DDT originale del mittente     ?
037780140303       //?"Consorzio Lotterie Nazionali - Gratta & Vinci"              ?
037790090528       //--------------------------------------------------------------
037800090528       BEGSR  sr_RepDdtCLN;
037810090528
037820110426         // -?Verifica se la spedizione ha mamma?
037830090528         if  not %open(FNLBL01L);
037840090528           open  FNLBL01L;
037850090528         endif;
037860090528         chain  (DB0aas : DB0lnp : DB0nrs : DB0nsp)  FNLBL000;
037870090528
037880090528         Select;
037890090528
037900110426           // -?Con Mamma: reperimento dati DDT dalla mamma?
037910090528           When  %found(FNLBL01L);
037920090528             if  not %open(FNBLP01L);
037930090528               open  FNBLP01L;
037940090528             endif;
037950090603             chain  (LBLaao : LBLlpo : LBLnro : LBLnso)  FNBLP000;
037960090528             if  %found(FNBLP01L);
037970090528               exsr  sr_DatiCLN_blp;
037980090528             endif;
037990090528
038000110426           // -?Dati da DS?
038010090901           When  DB0ges = 'B';
038020090901             exsr  sr_DatiCLN_DS;
038030090901
038040110426           // -?In Partenza: reperimento dati DDT da FNBLP?
038050090528           When  (DB0tbo = 'P'   or   DB0tbo = '8');
038060090528             exsr  sr_DatiCLN_blp;
038070090528
038080110426           // -?In Arrivo: reperimento dati DDT da FNARB?
038090090528           Other;
038100090528             exsr  sr_DatiCLN_arb;
038110090528
038120090528         EndSl;
038130090528
038140090528       ENDSR;
038150090528
038160090901       //--------------------------------------------------------------
038170140303       //?Da DS                                                        ?
038180090901       //--------------------------------------------------------------
038190090901       BEGSR  sr_DatiCLN_DS;
038200090901
038210110426         // -?Numero DDT?
038220090901         select;
038230090902           when  DB0rmn <> *zero;
038240100906             if  DB0rmn  > -99999999   and
038250100906                 DB0rmn <   99999999;
038260100906               WddtCLN = DB0rmn;
038270100906             endif;
038280090902           when  DB0ncd <> *zero;
038290090902             WddtCLN = DB0ncd;
038300090901         endsl;
038310090901
038320110426         // -?Data DDT in formato gg/mm/aaaa?
038330090902         Data_Eur = %date( ((DB0aas * 10000) + DB0mgs) : *iso );
038340090901         WdspCLN  = ( ( %subdt(Data_Eur : *days)  * 1000000 ) +
038350090901                      ( %subdt(Data_Eur : *months) * 10000 ) +
038360090901                      ( %subdt(Data_Eur : *years) ) );
038370090901
038380090901       ENDSR;
038390090901
038400090528       //--------------------------------------------------------------
038410090528       //?In Partenza: reperimento dati DDT da FNBLP                   ?
038420090528       //--------------------------------------------------------------
038430090528       BEGSR  sr_DatiCLN_blp;
038440090528
038450110426         // -?Numero DDT?
038460090528         select;
038470090528           when  BLPrmn <> *zero;
038480100906             if  BLPrmn  > -99999999   and
038490100906                 BLPrmn <   99999999;
038500100906               WddtCLN = BLPrmn;
038510100906             endif;
038520090528           when  BLPncd <> *zero;
038530090528             WddtCLN = BLPncd;
038540090528         endsl;
038550090528
038560110426         // -?Data DDT in formato gg/mm/aaaa?
038570090528         Data_Eur = %date( ((BLPaas * 10000) + BLPmgs) : *iso );
038580090528         WdspCLN  = ( ( %subdt(Data_Eur : *days)  * 1000000 ) +
038590090528                      ( %subdt(Data_Eur : *months) * 10000 ) +
038600090528                      ( %subdt(Data_Eur : *years) ) );
038610090528
038620090528       ENDSR;
038630090528
038640090528       //--------------------------------------------------------------
038650090528       //?In Arrivo: reperimento dati DDT da FNARB                     ?
038660090528       //--------------------------------------------------------------
038670090528       BEGSR  sr_DatiCLN_arb;
038680090528
038690110426         // -?Numero DDT?
038700090528         select;
038710090528           when  ARBrmn <> *zero;
038720100906             if  ARBrmn  > -99999999   and
038730100906                 ARBrmn <   99999999;
038740100906               WddtCLN = ARBrmn;
038750100906             endif;
038760090528           when  ARBncd <> *zero;
038770090528             WddtCLN = ARBncd;
038780090528         endsl;
038790090528
038800110426         // -?Data DDT in formato gg/mm/aaaa?
038810090528         Data_Eur = %date( ((ARBaas * 10000) + ARBmgs) : *iso );
038820090528         WdspCLN  = ( ( %subdt(Data_Eur : *days)  * 1000000 ) +
038830090528                      ( %subdt(Data_Eur : *months) * 10000 ) +
038840090528                      ( %subdt(Data_Eur : *years) ) );
038850090528
038860090528       ENDSR;
038870080918
038880080918       //--------------------------------------------------------------
038890140303       //?Stampa della sola Packing-List (2 per A4)                    ?
038900080918       //--------------------------------------------------------------
038910080918       BEGSR  StampaPkgLst;
038920130315
038930130315         // -?SE NON esiste il *file FIAR8 => uscita?
038940130315         if  $E_AR8 = *off;
038950130315           leavesr;
038960130315         endif;
038970080918
038980110426         // -?MODIFICA SOSPESA:?
038990110426         //  ?NON STAMPA LA PACKING-LIST DAL 2° TENTATIVO DI CONSEGNA?
039000081013         //if   $FIAR8 = *off   or   DB0fl1 = 'D';
039010081013         //  leavesr;
039020081013         //endif;
039030080918
039040110426         // -?Azzeramento numero di pagina per Lettera di Vettura?
039050110426         //  ?(meglio: "reimpostazione a 1")?
039060080926         reset  wPage;
039070080918
039080110426         // -?Posizionamento sul foglio A4 in base al n° pagina in stampa?
039090130418         *in33 = *off;
039100130418         exsr  Posiz_Box;
039110080919
039120110426         // -?Impostazione dei dati da stampare nel rec. con la costante?
039130110426         //  ?"Copia per il destinatario"  + estremi spedizione e pag.?
039140110426         //
039150110426         // -?"Copia per il destinatario" già impostato in sr. "PrepPrt"?
039160081017         //if  §AR8cxd = *blank;
039170130726         //  Tcxd  = c_CopiaXdest;
039180130726         //  Tcxd2 = Tcxd;
039190081017         //endif;
039200081017
039210110426         // -?Estremi spedizione da impostare SOLO se si stampando?
039220110426         //  ?la sola packing-list (DB0FA4 = "P")?
039230081017         if  DB0fa4 = 'P';
039240101028           Trsd = Prsd;
039250080922           Trif = 'Spedizione '
039260080924                + %editc(DB0lnp : 'X') + ' '
039270110310                + %editc(DB0nrs : 'X') + ' '
039280110310                + %editc(DB0nsp : 'X')
039290080922                + ' del ' + %editc(Pdsp : 'Y')
039300081017                + '        Pag.'
039310080922                + %editc(wPage:'Z');
039320101028           Tndc = Pndc;
039330081002           if  wSeqLdV = 1;
039340081002             §Pos   = wPosIni + c_PosBox1As + 0.040;
039350090422             §Pos   = wPosIni + c_PosBox1As + 0.035;
039360081002           else;
039370081002             §Pos   = wPosIni + c_PosBox1As - 0.070;
039380081002           endif;
039390080919         endif;
039400081017
039410110426         // -?La stampa di tale record verrà fatta dopo?
039420081017         //if  §AR8cxd = *blank   or   DB0fa4 = 'P';
039430081017         //  write A4prtDT0;
039440081017         //endif;
039450080918
039460110426         // -?Stampa Packing-List?
039470130315         exsr  StampaDDT;
039480080918
039490080918       ENDSR;
039500090528
039510080918      /end-free
039520030515      *
039530030515      *---------------------------------------------------------------*
039540050217      *?Stampa della Lettera di Vettura                              ?*
039550030515      *---------------------------------------------------------------*
039560040607     c     StampaLDV     BEGSR
039570080604      *
039580080604      * - Posizionamento sul foglio A4 in base al n° pagina in stampa
039590130418     c                   eval      *in33 = *off
039600130418     c                   exsr      Posiz_Box
039610030515      *
039620040607      * Azzeramento numero di pagina per Lettera di Vettura
039630030716      * (meglio: "reimpostazione a 1")
039640080926     c                   reset                   wPage
039650030515      *
039660120530      * 1ª parte - copia per il vettore (da ritornare in "BRT")
039670130418     c                   write     A4prtLDV
039680130418     c                   write     A4prtBOX
039690110426     c*//                write     A4LogoBAR
039700081020      /free
039710120530         //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
039720120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
039730130306         //  // >?Simil-Logo Bartolini/BRT?
039740120530         //  write  A4LogoBR2;
039750120530         //  write  A4LogoDA2;
039760120530         //else;
039770130306         //  // >?Simil-Logo Bartolini?
039780120530         //  write  A4LogoBAR;
039790120530         //  write  A4LogoDA1;
039800120530         //endif;
039810130306         // >?Simil-Logo BRT?
039820120530         write  A4LogoBRT;
039830120530         write  A4LogoDA3;
039840110426
039850110426         // >?dati DPD?
039860081020         if  *in71;
039870081020           exsr  StampaDPD;
039880081020         endif;
039890130306
039900130306         // -?Se LdV con PDF => registrazione rec. nel *file PDFLIST?
039910130306         if  %parms() > 1  and  (DB6pdf = 'F'  or  DB6pdf = 'X')
039920130306                           and  $§PDF   = *on;
039930130306           exsr  sr_WrtRec_PdfList;
039940130306         endif;
039950081020
039960130306         // -?Se richiesta solo tagliatella (copia per il vettore, da?
039970130306         //  ?lasciare però al destinatario) => fine?
039980081020         if  DB0fa4  = '4';
039990081020           leavesr;
040000081020         endif;
040010081020      /end-free
040020050510      *
040030081028      * Se LdV in formato "ex-A5" rimane la copia per il destinatario
040040050510     c     *in69         cabeq     *on           End_LdV
040050030515      *
040060030515sel 1c                   select
040070081028      * > dati DPD  (ora in ex-A5 o in 1/4 di A4)
040080081028w   1c***                when      *in71
040090030515      * > dati FedEx
040100030515w   1c                   when      *in72
040110030515     c                   exsr      ListaFED
040120081028      * > dati EuroExpress  (non più gestiti)
040130040223w   1c                   when      *in73
040140030515      *
040150030515e   1c                   endsl
040160030910      *
040170030910      * > elenco SEGNACOLLI NON SEQUENZIALI
040180050511      *  (no x max 2 perchè sono già stati elencati nell'apposita area)
040190030910      *  (no x DPD   perchè monocollo)
040200030910      *  (no x FedEx perchè segnacolliamo noi)
040210030910      *  (no x "A5"  perchè sono già stati elencati tra le "Note e
040220030910      *              Particolarità")
040230030910if  1c                   if        DB0fns =  'S'
040240050511     c                             and  DB0ncl > 2
040250050513     c                             and  $A4_x_ScNS  =   *on
040260030910     c***                          and  not *in71  and  not *in72
040270030910     c                             and  not *in69
040280030910     c                   exsr      ListaScNS
040290030910e   1c                   endif
040300031124      *
040310031124      * Reperimento della posizione utilizzabile per la stampa dei box
040320080825      *   (in pollici) in base all'ultima riga stampata in 1/8" o 1/9"
040330031124     c                   exsr      CalcRowPos
040340060524      *
040350070123      * > Spedizione con D.A.S. x cliente "PRAOIL OLEODOTTI ITALIANI"
040360081127if  1c                   if             DB0ccm = c_CliPraOil
040370060529     c                             or  (DB0fbr = 'S'
040380081127     c                             and  §B4Aks = c_CliPraOil)
040390060524     c                   exsr      StampaDAS
040400060524e   1c                   endif
040410031124      *
040420031124      * > Promemoria per autotrasportatore
040430110502if  1c*//*               if            (DB0gma = Ktab7V_CB
040440110502     c*//*                           or DB0gma = Ktab7V_C1
040450080625     c*** scontato x GMA='C1'       and DB0bam = 'S'
040460110502     c*//*                           or DB0gma = Ktab7V_C2
040470080729     c*** scontato x GMA='C2'       and DB0bam = 'S'
040480110502     c*//*                           or DB0gma = Ktab7V_C3
040490100513     c*** scontato x GMA='C3'       and DB0bam = 'S'
040500110411     c*//*                           or DB0gma = Ktab7V_C4
040510110411     c*//*                           or DB0gma = Ktab7V_C5
040520110502     c*//*                           or DB0gma = Ktab7V_R1
040530110502     c*//*                           or DB0gma = Ktab7V_NR)
040540110502     c*//*                         and  §7Rldv = 'S'
040550110502     c                   if            (§7Rldv = 'S'
040560110502     c                             and  $TabPXA2  = *on)
040570061106     c                             OR   $No_CF = *on
040580071114     c                             OR   $No_PI = *on
040590070306     c                             OR   $FIARN = *on
040600070315     c                             OR   $C_A_  = *on
040610080801     c                             OR   $ConsParz = *on
040620090302     c                             OR   $TabPXA   = *on
040630110411     c                             OR   $TabPXA2  = *on
040640031124     c                   exsr      ListaPMxA
040650031124e   1c                   endif
040660030515      *
040670030515      * > dati FIAR5
040680150224if  1c*//                if           %subst(DB0gva:1:1) = 'B'
040690150224     c*//                          or %subst(DB0gva:1:1) = 'O'
040700150224if  1c                   if                  DB0tc1      = 'S'
040710030717     c                             or        DB0tc2      = 'S'
040720030515     c                   exsr      ListaAR5
040730030515e   1c                   endif
040740030910      *
040750071015      * > Riferimento O.R.M.
040760071015sel 1c                   select
040770071015      * - > per ritiro contestuale alla consegna
040780100608w   1c                   when          (§7Rrc  = 'S'
040790071015     c                             and  §7Rldv = 'S')
040800030910     c                   exsr      StampaRCC
040810071015      * - > per ritiro porto franco prepagato
040820071015w   1c                   when           DB0tbo = '8'
040830071015     c                   exsr      StampaRPFPP
040840071015e   1c                   endsl
040850090924      /free
040860090924
040870110426         // >?Note per destinatario?
040880090924if  1    if  $BoxNxD;
040890090924           exsr  StampaNxD;
040900090924e   1    endif;
040910090924
040920090924      /end-free
040930040607      *
040940081013      * > DDT in LDV
040950081013      * - IN FUTURO: se al 1° tentativo di consegna
040960081013      *              (DB0FL1 = "D" - v. pgm FNLRS9R)
040970080917if  1c                   if             $FIAR8 =  *on
040980130315     c                             and  $E_AR8 =  *on
040990081013     c***-=»>                      and  DB0fl1 <> 'D'
041000040607     c                   exsr      StampaDDT
041010040607e   1c                   endif
041020050510      *
041030050510     c     End_LdV       tag
041040030515      *
041050080703      * 2ª parte - copia per il destinatario (da lasciare)
041060130418     c                   eval      *in33 = *on
041070040607     c                   exsr      ChiusuraLDV
041080030515      *
041090030515      * Stampa dei 3 asterischi finali (se richiesta)
041100030515if  1c                   if        DB0ges = '*'
041110030515     c                   move      '***'         Pmsg3
041120130418     c                   eval      *in33 = *off
041130130418     c                   exsr      Posiz_Box
041140030515     c                   write     A4prtMSG
041150030515e   1c                   endif
041160030515      *
041170030515     c                   ENDSR
041180080604      /free
041190080604
041200080604       //--------------------------------------------------------------
041210140303       //?Posizionamento del box all'interno della pag. A4             ?
041220080604       //--------------------------------------------------------------
041230130418       BEGSR  Posiz_Box;
041240080704
041250080704         select;
041260080704
041270110426           // -?Richiesta stampa di 1 LdV x pagina?
041280110426           //  ?(LdV in modifica o con packing-list del mitente)?
041290080704           when  DB0fa4 = '1'   or   DB0fa4 = 'S';
041300080704             $LdVxP   = 1;
041310080918             $Len1LdV = $LenLdV2;
041320080704
041330110426           // -?Richiesta stampa di 2 LdV x pagina (ex A5)?
041340110426           //  ?(copia per il vettore & copia per il destinatario)?
041350080704           when  DB0fa4 = '2';
041360080704             $LdVxP   = 2;
041370080704             $Len1LdV = $LenLdV2;
041380080704
041390110426           // -?Richiesta stampa di 4 LdV x pagina?
041400110426           //  ?(solo copia per il vettore)?
041410080704           when  DB0fa4 = '4';
041420080704             $LdVxP   = 4;
041430080704             $Len1LdV = $LenLdV4;
041440080918
041450110426           // -?Richiesta stampa della sola packing-list?
041460110426           //  ?(solo copia per il vettore)?
041470080918           when  DB0fa4 = 'P';
041480080918             $LdVxP   = 2;
041490080924             $Len1LdV = c_Len2_inch;
041500080704
041510080704           other;
041520080704             reset $LdVxP;
041530080704             reset $Len1LdV;
041540080704
041550080704         endsl;
041560080704
041570110426         // -?Richiesta "ripartenza" da foglio A4 successivo?
041580130418         if DB0fl6 = *on  and  Not  *in33;
041590080704           clear  wNumLdV;
041600080704         endif;
041610080704
041620110426         // -?Calcolo posizione di partenza all'interno del foglio A4.?
041630110426         //  ?[Se 2ª LdV nella pagina, ma LdV per A4 (intero):?
041640110426         //   ?si salta alla pag. A4 successiva; la LdV in esame?
041650110426         //   ?dev'essere stampata in posizione iniziale dell'A4,?
041660110426         //   ?seguita dai suoi dati ulteriori.]?
041670130418         if  Not  *in33;
041680130418           wNumLdV  += 1;
041690130418           wSeqLdV   = %rem( wNumLdV : $LdVxP );
041700130418         endif;
041710080704
041720130422         // -?Stampa della copia per il vettore?
041730130422         IF  NOT  *in33;
041740130422
041750130422           select;
041760130422
041770130422             // -?Modulo A4 intero?
041780130422             when  NOT  *in69    and   DB0fa4 <> '4'
041790130422                                 and   DB0fa4 <> 'P';
041800130422               wSeqLdV = 1;
041810130422             when  DB0fa4 = '1'   or   DB0fa4 = 'S';
041820130422               wSeqLdV = 1;
041830130422
041840130422             // -?Calcolo della posizione x box all'interno della pagina "A4"?
041850130422             when  wSeqLdV = *zero   and
041860130422                   $LdVxP  = 2;
041870130422               wSeqLdV = 2;
041880130422             when  wSeqLdV = *zero   and
041890130422                   DB0fa4  = '4';
041900130422               wSeqLdV = 4;
041910130422             when  wSeqLdV = *zero   and
041920130422                   *in69;
041930130422               wSeqLdV  = 2;
041940130422
041950130422           endsl;
041960130422
041970130422         ELSE;
041980130422
041990130422           select;
042000130422
042010130422             // -?Modulo A5 o ricevuta (¼ di A4)?
042020130422             when  *in69;
042030130422
042040130422             // -?Modulo A4 intero - 1ª parte (alta)?
042050130422             when  CurLineA4 <  26   and   §PS1 < c_§PS1;
042060130422               wSeqLdV = 1;
042070130422
042080130422             // -?Modulo A4 intero - 2ª parte (passa)?
042090130422           //when  CurLineA4 <  78;   (in 1/9", ma sarà in 1/8"...)
042100130422             when  CurLineA4 <  70;
042110130422               wSeqLdV  = 2;
042120130422
042130130422             // -?Modulo A4 intero - superata 2ª parte?
042140130422             //  ?PER ORA?IMPOSSIBILE !!!?
042150130422             other;
042160130422               clear wMaxLine;
042170130422               exsr  Test_OF;
042180130422               wMaxLine = $MaxLine2;
042190130422               wSeqLdV  = 1;
042200130422
042210130422           endsl;
042220130422
042230130422         ENDIF;
042240080919
042250110426         // -?Impostaz. delle posizioni nella pagina in corso di stampa?
042260080919         wPosIni = ($Len1LdV * (wSeqLdV - 1));
042270130422         if  *in33;       // -?Stampa della copia per il destinatario?
042280130422           wPosIni += $LenLdV4;
042290130422         endif;
042300110426
042310120530         //// -?Impostaz. dimensioni per il logo Bartolini-BRT?
042320120530         //If  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
042330120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
042340120530         //  reset  c_Spessore;
042350120530         //  reset  c_DeltaSpig;
042360120530         //  reset  c_LunghMaxV;
042370120530         //  reset  c_LunghV;
042380120530         //  reset  c_LunghO;
042390120530         //  reset  c_XspaceX;
042400120530         //  reset  c_BOXspace;
042410120530         //Else;
042420120530         //  c_Spessore  = c_1_Spessore;
042430120530         //  c_DeltaSpig = c_1_DeltaSpig;
042440120530         //  c_LunghMaxV = c_1_LunghMaxV;
042450120530         //  c_LunghV    = c_1_LunghV;
042460120530         //  c_LunghO    = c_1_LunghO;
042470120530         //  c_XspaceX   = c_1_XspaceX;
042480120530         //  c_BOXspace  = c_1_BOXspace;
042490120530         //EndIf;
042500120530         // -?Dimensioni per il logo BRT già definite?
042510120530         //c_Spessore  = c_2_Spessore;
042520120530         //c_DeltaSpig = c_2_DeltaSpig;
042530120530         //c_LunghMaxV = c_2_LunghMaxV;
042540120530         //c_LunghV    = c_2_LunghV;
042550120530         //c_LunghO    = c_2_LunghO;
042560120530         //c_XspaceX   = c_2_XspaceX;
042570120530         //c_BOXspace  = c_2_BOXspace;
042580080704
042590110426         // -?Se richiesta la stampa della sola packing-list?
042600110426         //  ?NON si dovranno stampare i dati della LdV?
042610110426         //  ?(quindi NON servono i posizionamenti che seguono)?
042620080918         if  DB0fa4 = 'P';
042630080918           leavesr;
042640080918         endif;
042650080918
042660110426         // -?Posizionamento nel foglio A4 in base al n° LdV in stampa?
042670110426         //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
042680110426
042690130418         select;
042700130418           when  DB0fa4 = '4';
042710130418             *in41 = (wSeqLdV = 1);
042720130418             *in42 = (wSeqLdV = 2);
042730130418             *in43 = (wSeqLdV = 3);
042740130418             *in44 = (wSeqLdV = 4);
042750130418           when Not *in33;
042760130418             *in41 = (wSeqLdV = 1);
042770130418             *in42 = *off;
042780130418             *in43 = (wSeqLdV = 2);
042790130418             *in44 = *off;
042800130418           other;
042810130418             *in41 = *off;
042820130418             *in42 = (wSeqLdV = 1);
042830130418             *in43 = *off;
042840130418             *in44 = (wSeqLdV = 2);
042850130418         endsl;
042860080704         WRITE  A4prtPOS;
042870080704
042880110426         // -?Posizionamenti nella copia per il vettore?
042890110426         //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
042900080704
042910110426         // -?Box in copia per il vettore?
042920080704         §Box1As = wPosIni + c_PosBox1As;
042930140919         §Box1Ai = wPosIni + c_PosBox1Bs  + c_LineNarrow;
042940080704         §Box1Bs = wPosIni + c_PosBox1Bs;
042950080704         §Box1Bi = wPosIni + c_PosBox1Bi;
042960140919         §Box1Bj = wPosIni + c_PosLin1oD1 + c_LineNarrow;
042970080704         §Box1Cs = wPosIni + c_PosBox1Cs;
042980140919         §Box1Ci = wPosIni + c_PosBox1Ds  + c_LineWide;
042990080704         §Box1Ds = wPosIni + c_PosBox1Ds;
043000091119         §Box1Di = §Box1Bi;
043010081218
043020120530         //If  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
043030120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
043040120530         //  // -?Simil-Logo Bartolini?
043050120530         //  §PlblS  = §Box1As + 0.125;
043060120530         //  §PlblVS = 7.092;
043070120530         //  exsr  sr_SimilLogoBAR;
043080120530         //  // -?Simil-Logo BRT?
043090120530         //  §P2blS  = §PlblS;
043100120530         //  §P2blVS = §PlblVD + 0.030;
043110120530         //  exsr  sr_SimilLogoBRT;
043120120530         //Else;
043130120530         //  // -?Simil-Logo Bartolini?
043140120530         //  §PlblS  = §Box1As;
043150120530         //  §PlblVS = 7.128;
043160120530         //  exsr  sr_SimilLogoBART;
043170120530         //EndIf;
043180120530         // -?Simil-Logo BRT?
043190120530         §PlblS  = §Box1As;
043200120530         §PlblVS = 7.581;
043210120530         exsr  sr_SimilLogoBRT_;
043220080704
043230120530         // -?Costanti dati anagrafici BRT?
043240110426         §PC0    = wPosIni + c_PosTBar;
043250110426         §PC1    = wPosIni + c_PosTBa1;
043260120530         //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
043270120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
043280120530         //  §PC2    = wPosIni + c_PosTBa2;
043290120530         //  §PC3    = wPosIni + c_PosTBa3;
043300120530         //  §PC4    = wPosIni + c_PosTBa4;
043310120530         //  §PC5    = wPosIni + c_PosTBa5;
043320120530         //  §PC6    = wPosIni + c_PosTBa6;
043330120530         //else;
043340120530         //  §PC2    = wPosIni + c_PosTBa2x;
043350120530         //  §PC3    = wPosIni + c_PosTBa3x;
043360120530         //  §PC4    = wPosIni + c_PosTBa4x;
043370120530         //  §PC5    = wPosIni + c_PosTBa5x;
043380120530         //  §PC6    = wPosIni + c_PosTBa6x;
043390120530         //endif;
043400120530         §PC2    = wPosIni + c_PosTBa2x;
043410120530         §PC3    = wPosIni + c_PosTBa3x;
043420120530         §PC4    = wPosIni + c_PosTBa4x;
043430120530         §PC5    = wPosIni + c_PosTBa5x;
043440120530         §PC6    = wPosIni + c_PosTBa6x;
043450130726         §PC7    = §Box1Bi - 0.150;
043460110426
043470110426         // -?Linee orizzontali nel box in copia per il vettore?
043480080704         §Lo1S1  = wPosIni + c_PosLin1oS1;
043490080704         §Lo1S2  = wPosIni + c_PosLin1oS2;
043500080922       //§Lo1S3  = wPosIni + c_PosLin1oS3;
043510080704         §Lo1S4  = wPosIni + c_PosLin1oS4;
043520091119         §Lo1S5  = wPosIni + c_PosLin1oS5;
043530080704         §Lo1C1  = wPosIni + c_PosLin1oC1;
043540080704         §Lo1C2  = wPosIni + c_PosLin1oC2;
043550140915         §Lo1C3  = wPosIni + c_PosLin1oC3;
043560080704         §Lo1D1  = wPosIni + c_PosLin1oD1;
043570080704
043580110426         // -?Lunghezza linee verticali nel box in copia per il vett.?
043590140922         §LLv1A  = §Lo1S5  - §Lo1S1;
043600140922         //§LLv1B  = §Lo1S4  - §Box1Bs;
043610140922         //§LLv1C  = §Box1Di - §Box1Bs;
043620140922         //***§LLv1B  = §Lo1S4  - §Box1Bj + c_LineNarrow;
043630140922         §LLv1B  = §Lo1S4  - §Box1Bj;
043640140922         §LLv1C  = §Box1Di - §Box1Bj;
043650140917
043660140917         // -?Costante "BRTcode" & codice BRTcode?
043670140917         §Pk1    = wPosIni + c_PosKBRTc;
043680080704
043690110426         // -?Costante "Consegna tassativa"?
043700080704         §Pk2    = wPosIni + c_PosKTass;
043710080704
043720110426         // -?Costanti "(nome e cognome in stampatello)", "(firma)" e?
043730110426         //           ?"(data e ora)"?
043740080704         §Pk3    = §Box1Bi - 0.015;
043750080704
043760110426         // -?Costante con rif. alla legge in vigore per RCV?
043770080704         §Pk4    = wPosIni + c_PosKRCV;
043780080704
043790110426         // -?Costante "Bollo assolto in modo virtuale..."?
043800080704         §Pk5    = wPosIni + c_PosKBAV;
043810080704         §Pk6    = §Pk5 + 0.060;
043820121017
043830121017         // -?Partita IVA o Cod. Fiscale del cliente?
043840121017         §Pk7    = §Pk4 + 0.080;
043850080704
043860110426         // -?Costante "COPIA PER IL DESTINATARIO" in DDT "generico"?
043870080709         §Pos    = c_PosKCXD2;
043880080704
043890080704       ENDSR;
043900080605
043910080604      /end-free
043920110426
043930140303       //--------------------------------------------------------------
043940140303       //?Posizionamento del simil-logo Bartolini (unico)              ?
043950140303       //--------------------------------------------------------------
043960120530      *///copy gaitrasrc/srcTNSY,LogoBAR_R2
043970110426
043980140303       //--------------------------------------------------------------
043990140303       //?Posizionamento del simil-logo Bartolini (con BRT)            ?
044000140303       //--------------------------------------------------------------
044010120530      *///copy gaitrasrc/srcTNSY,LogoBAR3R2
044020110426
044030140303       //--------------------------------------------------------------
044040140303       //?Posizionamento del simil-logo BRT (con BARTOLINI)            ?
044050140303       //--------------------------------------------------------------
044060120530      *///copy gaitrasrc/srcTNSY,LogoBRT3R2
044070120530
044080140303       //--------------------------------------------------------------
044090140303       //?Posizionamento del simil-logo BRT (unico)                    ?
044100140303       //--------------------------------------------------------------
044110120530      /copy gaitrasrc/srcTNSY,LogoBRT_R2
044120110426
044130030515      *---------------------------------------------------------------*
044140050217      *?Gestione overflow                                            ?*
044150030515      *---------------------------------------------------------------*
044160030515     c     Test_OF       BEGSR
044170030515      *
044180030515      * Test raggiungimento della posizione limite
044190081003     c                   if          ((§PS1 < $LimOF   and   NOT *in45)
044200081003     c                             or (§PS1 < ($LimOF - c_Len1_inch)
044210081003     c                                     and *in45   and   NOT *in46)
044220081003     c                             or (§PS1 < ($LimOF + c_Len1_inch)
044230081003     c                                     and *in45   and   *in46))
044240080924     c                             and CurLineA4 < wMaxLine
044250080919     c                   leavesr
044260030515e   1c                   endif
044270030515      *
044280030515      * Stampa del box per dati da FIAR5
044290030515      * (se è già stata stampata almeno una riga di tale box)
044300060921if  1c                   if        $FIAR5 = *on  and  $First = *off
044310030911     c                   eval      §LO2   = §PS1 + 0.050
044320071008if  2c                   if        NOT *in52
044330030515     c                   write     A4prtAR53
044340071008x   2c                   else
044350071008     c                   eval      §PS2   = §LO2 - 0.500
044360071008     c                   write     A4prtAR54
044370071008e   2c                   endif
044380030515e   1c                   endif
044390041209      *
044400041209sel 1c                   select
044410040607      *
044420041209      * DDT KARNAK/TITANEDI/EDK
044430041209      * Stampa di logo, box, linee e testi fissi del DDT del mittente
044440040607      * (se si stà stampando tale DDT)
044450040607      * (ma NON per la parte "merce omaggio")
044460040607      * calcolando - prima - le posizioni iniziale e finale del box
044470041209w   1c                   when           $DDT_p    = *on
044480071008     c                             and  §AR8tsd   = 'K1'
044490040607     c                             and  $DDT_o    = *off
044500040608      * - Stampa parte finale DDT
044510040608if  2c                   if        $DDTlastPg = *off
044520041209      * - - Calcolo della posizione finale del box
044530040607     c                   eval      PCnv2  = '          '
044540040607     c                                    + '*** SEGUE ***'
044550040608     c                   exsr      Posiz_DK
044560040607     c                   write     A4prtDK4
044570040608x   2c                   else
044580041209      * - - Stampa dei dati in chiusura DDT (se ultima pagina)
044590040608      *     (la posizione finale del box verrà lì calcolata)
044600050217     c                   exsr      PrtDdtKARN_V2
044610040608e   2c                   endif
044620040607      * - Stampa logo, box, linee e testi fissi
044630081017     c                   if            §AR8cxd = *blanks
044640081017     c                             or  DB0fa4  = 'P'
044650050218     c                   write     A4prtDT0
044660080923     c                   endif
044670040607     c                   write     A4prtDK0
044680041019      *
044690050414      * DDT LOGISTICA/SAIT Abrasivi
044700050215      * Stampa di logo, box, linee e testi fissi del DDT del mittente
044710041019      * (se si stà stampando tale DDT)
044720041209w   1c                   when           $DDT_p    = *on
044730060627     c                             and  %subst(§AR8tsd:1:1) = 'L'
044740041021     c                   exsr      Posiz_DL
044750050217      * - - Chiusura box in fase di stampa dettaglio DDT
044760050217      *     (la stampa dei dati in chiusura DDT sarà solo nell'ultima
044770050318      *      pagina - prima dell'eventuale packing-list)
044780050318      *     La gestione del salto alla 1ª pagina della packing-list
044790050321      *     (AR8trk = 'Z1' & Page = *zeros) avviene nella subr.
044800050318      *     "PrtDdtLOG1_Z1" (vedi subr. "PrtPckLst_1").
044810081017     c                   if            §AR8cxd = *blanks
044820081017     c                             or  DB0fa4  = 'P'
044830050308     c                   write     A4prtDT0
044840050331     c                   endif
044850050318sel 2c                   select
044860050505      *     - si sta stampando il DDT Logistica o Sait
044870050318w   2c                   when      %subst(AR8trk:1:1) <> 'Z'
044880071008     c                             and %subst(AR8trk:1:1) <> 'S'
044890050218     c                   write     A4prtDL0A
044900050615      *     - si sta stampando la Packing-List
044910050318w   2c                   when      %subst(AR8trk:1:1) =  'Z'
044920050615     c                             and    Page        >  *zeros
044930050318     c                   write     A4prtDL0C
044940050606      *     - si sta stampando la Fattura (oltre 1ª pag.)
044950071008w   2c                   when      %subst(AR8trk:1:1) =  'S'
044960081003     c                             and    Page        >  *zeros
044970050609     c                   write     A4prtDL0E
044980050318e   2c                   endsl
044990050113      *
045000050113      * DDT ARCTE
045010050215      * Stampa di logo, box, linee e testi fissi del DDT del mittente
045020050113      * (se si stà stampando tale DDT)
045030050113w   1c                   when           $DDT_p    = *on
045040050113     c                             and  AR8tmd    = 'ARCT'
045050050114     c                   exsr      Posiz_AC
045060050217      * - - Chiusura box in fase di stampa dettaglio DDT
045070050217      *     (la stampa dei dati in chiusura DDT sarà solo nell'ultima
045080050217      *      pagina)
045090081017     c                   if            §AR8cxd = *blanks
045100081017     c                             or  DB0fa4  = 'P'
045110050331     c                   write     A4prtDT0
045120050331     c                   endif
045130050217     c                   write     A4prtAC0A
045140050215      *
045150050215      * DDT ESPRINET
045160050215      * Stampa di box, linee e testi fissi del DDT del mittente
045170050215      * (se si stà stampando tale DDT)
045180050215      * · Solo nella 1ª pagina vengono stampati il logo e la parte
045190050215      *        superiore del DDT.
045200050215      * · Solo nell'ultima pagina vene stampato il "fondo".
045210050215w   1c                   when           $DDT_p    = *on
045220060627     c                             and  §AR8tsd   = 'E1'
045230110405if 1bc                   if        %subst(SAVtrk:1:1 ) <> 'P'
045240110405     c                             or   %eof(FIAR801L)
045250050215      * - Stampa parte finale DDT
045260050215      * - - Calcolo delle posizioni di box & linee
045270050215     c                   exsr      Posiz_ES
045280050215      * - - Chiusura box in fase di stampa dettaglio DDT
045290050215      *     (la stampa dei dati in chiusura DDT sarà solo nell'ultima
045300050215      *      pagina)
045310050215if  2c***                if        %subst(AR8trk:1:1) <> 'P'
045320081017     c                   if            §AR8cxd = *blanks
045330081017     c                             or  DB0fa4  = 'P'
045340050331     c                   write     A4prtDT0
045350050331     c                   endif
045360050215     c                   write     A4prtES0A
045370050215e   2c***                endif
045380110405e  1bc                   endif
045390041209      *
045400041209      * DDT GENERICO con LOGO
045410050215      * Stampa di logo e box del DDT del mittente
045420041209      * (se si stà stampando tale DDT)
045430041209w   1c                   when           $DDT_p    = *on
045440041209     c                             and  §AR8tsd   = 'S2'
045450041209     *** * - Chiusura del box con i dati di mittente e destinatario
045460041209     *** *   (Se si stanno ancora stampando 'sti dati: bisogna calcolare
045470041209     *** *    la posizione di chiusura temporanea del box)
045480041209if  2***c                   if             AR8trk =  'B1'
045490041209     ***c                             or   AR8trk =  'B2'
045500041209     *** *   Impostazione della posizione finale per il box
045510041209     ***c                   exsr      Rep_§LO2
045520041209     ***c                   eval      §PB2   = §LO2 + 0.065
045530041209e   2***c                   endif
045540041209     *** *   Stampa del box
045550041209     ***c                   write     A4prtDB0
045560041209      * - Il logo è già stato SICURAMENTE stampato
045570041209     c***                exsr      Rep_§LO2
045580041209     c***                eval      §PB1   = §LO2
045590041209      *** Stampa del logo
045600041209     c***                write     A4prtDB0
045610041209      *
045620041209e   1c                   endsl
045630030515      *
045640030515      * Stampa dell'indicazione "Segue..." su pagina corrente
045650030515     c                   clear                   Pseg0
045660030515     c                   clear                   Pseg1
045670030515     c                   eval      Pseg9  = '  Segue...'
045680080919      /free
045690080923         select;
045700080923           when  *in45   and   NOT *in46;
045710080923             §PSA = 0.200;   // Per campo vuoto !
045720080923             §PSB = 0.200;   // Per campo vuoto !
045730080923             §PSC = c_Len2_inch - 0.050;
045740080923           when  *in45   and   *in46;
045750080923             §PSA = c_Len2_inch + 0.200;   // Per campo vuoto !
045760080923             §PSB = c_Len2_inch + 0.200;   // Per campo vuoto !
045770080924             §PSC = c_Len2_inch * 2 - 0.200;
045780080923           when  NOT *in45;
045790080923             §PSA = 0.400;
045800080923             §PSB = 0.700;
045810080923             §PSC = 8.750;
045820080923         endsl;
045830080919      /end-free
045840030515     c                   write     A4prtSEG
045850030515      *
045860031031      * Se sono ancora nella prima pagina =>
045870030515      * stampo anche i dati per il destinatario e box/barcode/ecc.
045880080604sel 1c                   select
045890080923      * > Se NON sto stampando un DDT del mittente
045900080922      *   (nel qual caso non stampo null'altro)
045910080923      *   => nulla
045920080919w   1c                   when      wPage  > 1
045930080923      * > Se sto stampando le sole copie per il vettore (4 in A4)
045940080923      *    o la relativa packing-list
045950080923      *   (nel qual caso non stampo null'altro)
045960080923      *   => nulla
045970081003w   1c                   when      DB0fa4 = '4'   or   DB0fa4 = 'P'
045980040607      * > Se NON è ristampa in arrivo per recapito c/assegni  e
045990040707      *   se NON sto stampando una LdV Poste
046000080703      *   se NON sto stampando (anche) un DDT del mittente
046010080703      *   se NON sto stampando le sole copie per il vettore (4 in A4)
046020080923      *   => posizionamento e stampa
046030120113w   1c                   when          $No_x_Cons =  *off
046040081013     c                             and ($FIAR8    =  *off)
046050081013     c***-=»>                       or  DB0fl1    =  'D')
046060080605      * - Posizionamento sul foglio A4 in base al n° pagina in stampa
046070130418     c                   eval      *in33 = *on
046080130418     c                   exsr      Posiz_Box
046090130418      * - Stampa 2ª parte della LdV (copia per il destinatario)
046100130418     c                   write     A4prtLDV
046110031031      * - Stampa "cornice" / barcode / righine varie in parte inferiore
046120130418     c                   write     A4prtBOX
046130110426     c*//                write     A4LogoBAR
046140110426      /free
046150120530         //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
046160120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
046170120530         //  // >?Simil-Logo Bartolini/Brt?
046180120530         //  write  A4LogoBR2;
046190120530         //  write  A4LogoDA2;
046200120530         //else;
046210120530         //  // >?Simil-Logo Bartolini?
046220120530         //  write  A4LogoBAR;
046230120530         //  write  A4LogoDA1;
046240120530         //endif;
046250120530         // >?Simil-Logo BRT?
046260120530         write  A4LogoBRT;
046270120530         write  A4LogoDA3;
046280110426      /end-free
046290120113      *//* > Se sto stampando una LdV Poste per recapito c/assegni
046300120113w   1c*//                   when          $No_x_Cons =  '2'
046310120113      *//* - Stampa 2ª parte "nulla" della LdV
046320130418     c*//                   eval      *in33 = *on
046330130418     c*//                   exsr      Posiz_Box
046340120113     c*//                   write     A4prtLDVB
046350080604e   1c                   endsl
046360080923      /free
046370110426         // -?Se richiesta la stampa della sola packing-list:?
046380110426         //  ?incremento del n° spedizione per calcolare correttam.?
046390110426         //  ?il n° di pagina iniziale per la spediz. successiva?
046400080925         if  DB0fa4 = 'P';
046410080925             wNumLdV += 1;
046420080925         endif;
046430080925
046440110426         // -?Reimpostazione degli indicatori per il posizionamento?
046450110426         //  ?all'interno del foglio A4?
046460080923         *in45 = (DB0fa4 = 'P');
046470080923         *in46 = (DB0fa4 = 'P'   and   %rem(wNumLdv : 2) = *zero);
046480080923      /end-free
046490030515      *
046500030515      * Salta a pagina successiva e indica "(Seguito)"
046510030515     c                   write     A4prtSP0
046520030515     c                   eval      wPage  = wPage + 1
046530080923      /free
046540081017         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
046550081017           write  A4prtNUL;
046560081017         endif;
046570081017
046580080923         IF  DB0fa4 <> 'P';
046590080922      /end-free
046600030515     c                   eval      Pseg0  = 'Spedizione '
046610110310     c                                    + %editc(DB0lnp : 'X') + ' '
046620110310     c                                    + %editc(DB0nrs : 'X') + ' '
046630110310     c                                    + %editc(DB0nsp : 'X')
046640030515     c                                    + ' del ' + %editc(Pdsp : 'Y')
046650030515     c                   eval      Pseg1  = '(Seguito)             Pag.'
046660030515     c                                    + %editc(wPage:'Z')
046670030515     c                   clear                   Pseg9
046680030515     c                   write     A4prtSEG
046690080922      /free
046700080923         ELSE;
046710081017
046720110426           // -?"Copia per il destinatario" già impostato in sr. "PrepPrt"?
046730081017           //if  §AR8cxd = *blank;
046740130726           //  Tcxd  = c_CopiaXdest;
046750130726           //  Tcxd2 = Tcxd;
046760081017           //endif;
046770081017
046780110426           // -?Estremi spedizione da impostare SOLO se si stampando?
046790110426           //  ?la sola packing-list (DB0FA4 = "P")?
046800081017           if  DB0fa4 = 'P';
046810101028             //Trsd = Prsd;   (già impostato nella subr. StampaPkgLst)?
046820081017             Trif = 'Spedizione '
046830081017                  + %editc(DB0lnp : 'X') + ' '
046840110310                  + %editc(DB0nrs : 'X') + ' '
046850110310                  + %editc(DB0nsp : 'X')
046860081017                  + ' del ' + %editc(Pdsp : 'Y')
046870081017                  + '        Pag.'
046880101028                  + %editc(wPage:'Z');
046890101028                //+ '        (Seguito)';
046900101028             //Tndc = Pndc;   (già impostato nella subr. StampaPkgLst)?
046910081017           endif;
046920080923
046930110426           // -?Verifica se la 2ª parte della packing-list sta nella?
046940110426           //  ?pag. attuale o se deve andare in quella successiva?
046950080923           wSeqLdV  = %rem( wNumLdV : $LdVxP );
046960080923           if  wSeqLdV = *zero   and   $LdVxP  = 2;
046970080923             wSeqLdV = 2;
046980080923           endif;
046990080923
047000110426           // -?Posizionamenti nella pagina in corso di stampa?
047010110426           //  ?(§POS = posizione verticale della costante "COPIA PER?
047020110426           //   ?IL DESTINATARIO" (con estremi spedizione e pag.)?
047030080923           wPosIni  = ($Len1LdV * (wSeqLdV - 1));
047040080924           if  wSeqLdV = 1;
047050080924             §Pos   = wPosIni + c_PosBox1As + 0.040;
047060090422             §Pos   = wPosIni + c_PosBox1As + 0.035;
047070080924           else;
047080080924             §Pos   = wPosIni + c_PosBox1As - 0.070;
047090080924           endif;
047100081017
047110081017           if  §AR8cxd = *blank   or   DB0fa4  = 'P';
047120081017             write A4prtDT0;
047130081017           endif;
047140080923
047150080923         ENDIF;
047160080922      /end-free
047170030515      *
047180030515      * Reimposta posizioni iniziali di stampa per linee
047190030515      *   (orizzontali-§LOn e verticali-§LVn) e campi (§PSn).
047200030515      *   Nelle pagine dalla 2ª in poi si parte da in alto:
047210030515      *   non vengono ri-stampati i 2 box "vettore" e "destinatario".
047220030515sel 1c                   select
047230120530      * - BRT
047240030515w   1c                   when      *in70
047250080711     c                   eval      §LO1   = c_§PS1
047260030515     c                   eval      §LO2   = §LO1
047270030515     c                   eval      §PS1   = §LO1
047280040607sel 2c                   select
047290050217      * - - con DDT del mittente KARNAK / TITANEDI / EDK
047300040607w   2c                   when           $DDT_p    = *on
047310071008     c                             and  §AR8tsd   = 'K1'
047320040608     c                   exsr      NewPageDK
047330050414      * - - con DDT del mittente LOGISTICA / SAIT ABRASIVI
047340041207w   2c                   when           $DDT_p    = *on
047350060627     c                             and  %subst(§AR8tsd:1:1) = 'L'
047360050321     c                   exsr      NewPageDL
047370050113      * - - con DDT del mittente ARCTE
047380050113w   2c                   when           $DDT_p    = *on
047390050113     c                             and  AR8tmd    = 'ARCT'
047400081002     c                   exsr      NewPageAC
047410050215      * - - con DDT del mittente ESPRINET
047420050215w   1c                   when           $DDT_p    = *on
047430060627     c                             and  §AR8tsd   = 'E1'
047440110405if  2c                   if        %subst(SAVtrk:1:1 ) <> 'P'
047450110405     c                             or   %eof(FIAR801L)
047460081002     c                   exsr      NewPageES
047470110405e   2c                   endif
047480041207      * - - con DDT generico (con marchio)
047490041207w   2c                   when           $DDT_p    = *on
047500041207     c                             and  §AR8tsd   = 'S2'
047510081017     c                   exsr      NewPageSTD
047520041207      * - - con DDT generico (senza marchio)
047530041207w   2c                   when           $DDT_p    = *on
047540041207     c*** ("scontato":)            and  §AR8tsd   = 'S1'
047550081017     c                   exsr      NewPageSTD
047560040607e   2c                   endsl
047570080922      * - DPD (ora in ex-A5)
047580080922w   1c***                when      *in71
047590030515      * - FedEx
047600030515w   1c                   when      *in72
047610030709     c                   eval      §LO1   = 1.600
047620030515     c                   eval      §LO2   = §LO1 + 0.425
047630030515     c                   eval      §LV1   = §LO1 + 0.300
047640030515     c                   eval      §LV2   = §LV1 + 0.070
047650030515     c                   write     A4prtFED1
047660030515do  2c                   dow       CurLineA4 < $MinLine1
047670030515     c                   write     A4prtNUL
047680030515e   2c                   enddo
047690030515     c                   write     A4prtFED2
047700030515     c                   eval      §LO1   = §LO2 - 0.250
047710030911     c                   eval      §PS1   = §LO1 + 0.150
047720040223      * - EuroExpress
047730040223w   1c                   when      *in73
047740030515      *
047750030515e   1c                   endsl
047760030515      *
047770030515      * N.B.: Per i SegnaColli non sequenziali non servono queste
047780030515      *       impostazioni: trattasi di stampa con salto riga -skipb-
047790030515      *       quindi sono già posizionato.
047800030515      *
047810080919     c                   ENDSR
047820030910      *
047830030910      *---------------------------------------------------------------*
047840050217      *?Reperimentoo della posizione (in pollici) in cui posizionarsi?*
047850050217      *?* per la stampa in base all'ultima riga stampata             ?*
047860030910      *---------------------------------------------------------------*
047870030910     c     Rep_§LO2      BEGSR
047880030910      *
047890030910sel 1c                   select
047900080825      *
047910080825      * Se ho stampato l'elenco dei segnacolli non sequenziali, devo
047920080825      *   "allineare" il posizionamento in pollici all'ultima riga
047930080825      *   stampata (in ottavi di pollice).
047940080825      *   =>   Num_ultima_riga_stampata / 8
047950080825w   1c                   when           DB0fns     = 'S'
047960080825     c                             and  DB0ncl     > 2
047970080825     c                             and  $A4_x_ScNS = *on
047980080825     c                             and  NOT  *in69
047990080825     c                   eval(h)   §LO2   = (CurLineA4 / 8) + 0,1
048000040607      *
048010040607      * Se ho stampato il DDT del mittente
048020080714      *   =>   Num_ultima_riga_stampata / 9
048030081013w   1c                   when           $FIAR8 =  *on
048040081013     c***-=»>                      and  DB0fl1 <> 'D'
048050080708     c                   eval(h)   §LO2   = (CurLineA4 / 9) + 0,1
048060030910      *
048070030910      * Se non ho stampato né elenco segnacolli non sequenziali né
048080040223      *   altri box, devo impostare comunque i dati per il posiziona-
048090030910      *   mento di questo primo box
048100030910w   1c                   when      §LO2   = *zeros
048110080624     c                   eval      §LO1   = 2.850
048120030910     c                   eval      §LO2   = §LO1
048130030910      *
048140030910e   1c                   endsl
048150030910      *
048160030910     c                   ENDSR
048170031124      *
048180031124      *---------------------------------------------------------------*
048190050217      *?Reperim. della posizione (in pollici) utlizzaile per la      ?*
048200080714      *?  stampa dei box in base all'ultima riga stampata in 1/9"    ?*
048210031124      *---------------------------------------------------------------*
048220031124     c     CalcRowPos    BEGSR
048230031124      *
048240031124      * Imposto le posizioni inziali per il primo dei box
048250080624     c                   select
048260080624      *
048270040607      * - in DDT del mittente: dopo eventuale elenco merce omaggio
048280120530      *   (come in LDV BRT)
048290120530      * - in LDV BRT: dopo eventuale elenco segnacolli non seq.
048300080624     c                   when      *in70
048310080624     c                   exsr      Rep_§LO2
048320080624      *
048330080922      * - in LDV DPD (ora in ex-A5)
048340080922     c***                when      *in71
048350080624      *
048360040607      * - in LDV FEDEX: posizione variabile
048370031124      *   (dipende dal numero dei segnacolli elencati: intanto
048380031124      *   posiziona la "base di partenza" sull'ultima posizione
048390031124      *   eventualmente usata per l'elenco dei segnacolli FedEx;
048400031124      *   poi si ri-calcoleranno in base al numero di righe stampate)
048410080624     c                   when      *in72
048420080624     c                   eval      §LO2   = §LO1 + 0.250
048430080624      *
048440080912      * - in LDV EUROEXPRESS: non più gestita
048450080624     c                   when      *in73
048460080912      *
048470080624     c                   endsl
048480080624      *
048490031124     c                   eval      §LO1   = §LO2
048500031124      *
048510031124     c                   ENDSR
048520060524      *
048530060524      *---------------------------------------------------------------*
048540060524      *?Stampa box per riferimento al                                ?*
048550060524      *?DOCUMENTO di ACCOMPAGNAMENTO SEMPLIFICATO                    ?*
048560060524      *---------------------------------------------------------------*
048570060524     c     StampaDAS     BEGSR
048580060524      *
048590060524     c***                eval      Par5   = '*...+....1....+....2....+-
048600060524     c***                          ....3....+....4....+....5....+....6-
048610060524     c***                          ....+....7....+....8....+....9....+-
048620060524     c***                          ....A'
048630060524      *
048640060524      * - stampo testata
048650090302     ***c                   eval      $First = 'B'
048660090302     ***c                   eval      Par5   = C_PMxA
048670090302      /free
048680090302         $First = *on;
048690090302         Par5   = '                    '
048700090302                + '                    '
048710090302                + '          '
048720090302                + C_PMxA;
048730090302      /end-free
048740060524     c                   exsr      StampaAR5
048750090302      /free
048760090302
048770090302         // - memorizza posizione iniziale per il box
048780090302         //   (§PB1 sarà da reimpostare in §LO1 PRIMA della stampa del box)
048790090302         §PB1   = §LO1;
048800090302
048810090302         // - stampa linea separatrice
048820090302         §LO1   = §PS1 + 0.050;
048830090302         write  A4prtLO;
048840090302         §PS1   = §LO1;
048850090302      /end-free
048860060524      *
048870060524      * - stampo 1° testo
048880060524     c                   eval      $First = '2'
048890060524     c                   eval      Par5   = 'CONSEGNARE AL DESTINATARI-
048900060524     c                             O D.D.T. E D.A.S. (Documento di Acc-
048910060524     c                             ompagnamento Semplificato).'
048920060524     c                   exsr      StampaAR5
048930060524      *
048940060524      * - stampo 2° testo
048950061030     c*** (già fatto:)   eval      $First = *off
048960060524     c                   eval      Par5   = 'In caso di RIFIUTO compil-
048970060524     c                             are l''ultima pagina del D.A.S. ind-
048980060524     c                             icando:'
048990060524     c                   exsr      StampaAR5
049000060524     c                   eval      Par5   = 'a) nome Autotrasportatore-
049010060524     c                              nel campo "IL SOTTOSCRITTO ... INC-
049020060524     c                             ARICATO DEL TRASPORTO"'
049030060524     c                   exsr      StampaAR5
049040060524     c                   eval      Par5   = 'b) firma Autotrasportator-
049050060524     c                             e nel campo "FIRMA INCARICATO DEL T-
049060060524     c                             RASPORTO"'
049070060524     c                   exsr      StampaAR5
049080060524     c                   eval      Par5   = 'c) causale del rifiuto (b-
049090060524     c                             arrare "la merce non è conforme all-
049100060524     c                             ''ordine" o indicare altra motivazi-
049110060524     c                             one)'
049120060524     c                   exsr      StampaAR5
049130060524     c                   eval      Par5   = 'd) firma del destinatario-
049140060524     c                              nel campo "FIRMA DEL DESTINATARIO"'
049150060524     c                   exsr      StampaAR5
049160090302     *** *
049170090302     *** * - stampo box
049180090302     ***c                   eval      §LO2   = §PS1 + 0.050
049190090302     ***c                   write     A4prtAR53
049200090302      /free
049210090302         // - stampa box
049220090302         //   (§PB1 è da reimpostare in §LO1: contiene il suo valore PRIMA
049230090302         //   della stampa delle varie linee separatrici)
049240090302         §LO1   = §PB1;
049250090302         §LO2   = §PS1 + 0.070;
049260090302         write  A4prtAR53;
049270090302      /end-free
049280060524      *
049290060524     c                   ENDSR
049300031124      *
049310031124      *---------------------------------------------------------------*
049320050217      *?Prepara elenco per box "Promemoria per autotrasportatore"    ?*
049330031124      *---------------------------------------------------------------*
049340031124     c     ListaPMxA     BEGSR
049350031124      *
049360070308      * - stampo testata
049370070316     c                   eval      $First = *on
049380070319     c                   eval      Par5   = '                    '
049390070319     c                                    + '                    '
049400070319     c                                    + '          '
049410070319     c                                    + C_PMxA
049420031124     c                   exsr      StampaAR5
049430070315      *
049440070315      * - memorizzo posizione iniziale per il box
049450070320      *   (§PB1 sarà da reimpostare in §LO1 PRIMA della stampa del box)
049460070315     c                   eval      §PB1   = §LO1
049470031124      *
049480070308      * - stampo testo
049490081127      *
049500081212      /free
049510110411         // >?Mittente cliente in tab. "PXA"?
049520090302         IF  $TabPXA = *on;
049530090302
049540170517           clear  k05tntbe01;
049550090302           k_TBEcod = 'PXA';
049560090302           k_TBEke1 = %editc(DB0ccm : 'X');
049570100316           k_TBEke2 = '001            ';
049580090302           clear  k_TBElin;
049590090302           clear  k_TBEsif;
049600100316           //setll  %kds(k05tntbe01 : 3)  TNTBE000;  (già fatto)
049610090302           //reade  %kds(k05tntbe01 : 2)  TNTBE000;  (già fatto)
049620090302           clear  wx;
049630090302
049640100316           DoW  not  %eof(TNTBE01L)  and  TBEke2 <= '006            ';
049650090302
049660090302             if  TBEatb = *blank;
049670090302
049680110411               // -?Stampa della riga separatrice iniziale?
049690090302               if  wx = *zero;
049700090302                 §LO1   = §PS1 + 0.050;
049710090302                 write  A4prtLO;
049720090302                 §PS1   = §LO1;
049730090302                 $First = 'G';
049740090302               endif;
049750090302
049760110411               // -?Salta eventuali righe vuote?
049770090302               dow  %int( %trimr( TBEke2 ) ) > wx + 1;
049780090302                 clear  Par5;
049790090302                 exsr  StampaAR5;
049800090302                 wx += 1;
049810090302               enddo;
049820090302
049830110411               // -?Stampa la singola riga di testo?
049840090302               Par5   = TBEuni;
049850090302               exsr  StampaAR5;
049860090302               wx += 1;
049870090302
049880090302             endif;
049890090302
049900090302             reade  %kds(k05tntbe01 : 2)  TNTBE000;
049910090302
049920090302           EndDo;
049930090302
049940090302         ENDIF;
049950110411
049960110411         // >?Particolarità consegna in tab. "PXA"?
049970110411         IF  $TabPXA2 = *on;
049980110411
049990170517           clear  k05tntbe01;
050000110411           k_TBEcod = 'PXA';
050010110411           k_TBEke1 = DB0gma;
050020110411           k_TBEke2 = '001            ';
050030110411           clear  k_TBElin;
050040110411           clear  k_TBEsif;
050050110411           setll  %kds(k05tntbe01 : 3)  TNTBE000;
050060110411           reade  %kds(k05tntbe01 : 2)  TNTBE000;
050070110411           clear  wx;
050080110411
050090110411           DoW  not  %eof(TNTBE01L)  and  TBEke2 <= '006            ';
050100110411
050110110411             if  TBEatb = *blank;
050120110411
050130110411               // -?Stampa della riga separatrice iniziale?
050140110411               if  wx = *zero;
050150110411                 §LO1   = §PS1 + 0.050;
050160110411                 write  A4prtLO;
050170110411                 §PS1   = §LO1;
050180110411                 $First = 'G';
050190110411               endif;
050200110411
050210110411               // -?Salta eventuali righe vuote?
050220110411               dow  %int( %trimr( TBEke2 ) ) > wx + 1;
050230110411                 clear  Par5;
050240110411                 exsr  StampaAR5;
050250110411                 wx += 1;
050260110411               enddo;
050270110411
050280110411               // -?Stampa la singola riga di testo?
050290110411               Par5   = TBEuni;
050300110411               exsr  StampaAR5;
050310110411               wx += 1;
050320110411
050330110411             endif;
050340110411
050350110411             reade  %kds(k05tntbe01 : 2)  TNTBE000;
050360110411
050370110411           EndDo;
050380110411
050390110411         ENDIF;
050400090302
050410081212      /end-free
050420081127      *
050430031216sel 1c                   select
050440070306      * >
050450070306x   1c***                other
050460070306     c***                eval      $First = *off
050470070306     c***                clear                   Par5
050480070306     c***                exsr      StampaAR5
050490070306      *
050500070306e   1c                   endsl
050510061102      *
050520071114      * > Raccogliere Codice Fiscale e/o Partita Iva di ...
050530071114if  1c                   if             $No_CF = *on
050540071114     c                             or   $No_PI = *on
050550070320     c                   eval      §LO1   = §PS1 + 0.050
050560070320     c                   write     A4prtLO
050570070320     c                   eval      §PS1   = §LO1
050580070321     c                   eval      $First = 'G'
050590061102     c                   eval      Par5   = 'Spedizione n°'
050600061102     c                                    + %editw(DB0lnp:'0   ')
050610061102     c                                    + %editw(DB0nrs:'0  ')
050620061102     c                                    + ' '
050630061102     c                                    + %editc(DB0nsp:'Z')
050640061102     c                                    + ' del '
050650061102     c                                    + %editc(Pdsp:'Y')
050660061102     c                   exsr      StampaAR5
050670071114if  2c                   if        $No_PI = *on
050680071114     c*** (già fatto:)   eval      $First = 'G'
050690071114     c                   eval      Par5   = 'raccogliere +
050700071114     c                                      Partita I.v.a. di "'
050710071114     c                                    + %trim(wRSxCF)
050720071114     c                                    + '"'
050730071114     c                   exsr      StampaAR5
050740120504if  3c                   if        $No_CF = *off
050750120504     c*** (già fatto:)   eval      $First = 'G'
050760120504     c                   eval      Par5   = 'Codice Fiscale di "'
050770120504     c                                    + %trim(wRSxCF)
050780120504     c                                    + '": '
050790120504     c                                    + wB0cfi
050800120504     c                   exsr      StampaAR5
050810120504e   3c                   endif
050820071114e   2c                   endif
050830120503if  2c                   if        $No_CF = *on
050840120503     c*** (già fatto:)   eval      $First = 'G'
050850120503     c                   eval      Par5   = 'raccogliere +
050860120503     c                                      Codice Fiscale di "'
050870120503     c                                    + %trim(wRSxCF)
050880120503     c                                    + '"'
050890120503     c                   exsr      StampaAR5
050900120503e   2c                   endif
050910070306e   1c                   endif
050920070306      *
050930070306      * > Note bolla (dal file FIARN00F)
050940070306      *   (posizionamento e 1ª lettura già effettuate nella subr.
050950070306      *    "CheckFIARN")
050960070306if  1c                   if        $FIARN = *on
050970070320     c                   eval      §LO1   = §PS1 + 0.050
050980070320     c                   write     A4prtLO
050990070320     c                   eval      §PS1   = §LO1
051000070321     c                   eval      $First = 'G'
051010070319do  2c                   dou       %eof(FIARN11L)
051020070306     c                   eval      Par5   = ARNnob
051030070306     c                   exsr      StampaAR5
051040070319     c     K06ARN11      reade     FIARN000
051050070306e   2c                   enddo
051060070306e   1c                   endif
051070070319      *
051080070319      * > Consegne parziali precedenti (dal file FNART00F)
051090070406if  1c                   IF        $ConsParz     =  *on
051100101130     c*//(già fatto)?   exsr      SchConsParz
051110070320     c                   eval      §LO1   = §PS1 + 0.050
051120070320     c                   write     A4prtLO
051130070320     c                   eval      §PS1   = §LO1
051140070321     c                   eval      $First = 'G'
051150070319     c                   eval      xx     = 1
051160070319do  2c                   DoW            (xx      <= %elem($CP)
051170070319     c                             and  $CP(xx)  <  *all'9')
051180070319     c                   movel     $CP(xx)       Elem_CP
051190070319     c                   z-add     WCPdcm        w0080
051200070319     c     *iso          test(d)                 w0080                  31
051210070319if  3c                   if        NOT *in31
051220071025     c     *iso          movel     w0080         Data_EUR
051230071025     c     *eur          movel     Data_EUR      w0080
051240070319x   3c                   else
051250070319     c                   clear                   w0080
051260070319e   3c                   endif
051270140304 *...+*...1....+....2....+....3....+....4....+....5....+....6....+....7....+..
051280140305 Conse*nati colli 12345 in data 12/34/5678
051290140304 Conse*nati colli 12345 in data 12/34/5678 alle ore 12:34 - Firmatario: 123456
051300140304 Conse*nati colli 12345 il 12/34/5678 alle 12:34 a 1234567890123456789012345
051310140305 Conse*nati colli 12345 in data 12/34/5678 alle ore 12:34
051320140305      *                 Firmatario: 123456789012345678901234567890123456789012
051330070321     c                   eval      Par5   = 'Consegnati colli '
051340140305     c                                    + %editc(WCPncl:'Z')
051350140305     c                                    + ' in data '
051360070319     c                                    + %editc(w0080:'Y')
051370140304     c                   if        WCPocm > *zero
051380140304     c                   eval      Par5   = %trimr( Par5 )
051390140305     c                                    + ' alle ore '
051400140303     c                                    + %editw(WCPocm : '0 :  ')
051410140304     c                   endif
051420070319     c                   exsr      StampaAR5
051430140305     c                   if        WCPfir <> *blank
051440140305     c                   clear                   Par5
051450140305     c                   eval      %subst( Par5 : 24 ) = 'Firmatario: '
051460140305     c                                    + WCPfir
051470140305     c                   exsr      StampaAR5
051480140305     c                   endif
051490070319     c                   eval      xx     = xx + 1
051500070319e   2c                   EndDo
051510070319e   1c                   ENDIF
051520070321      *
051530070321      * > Reperita Comunicazione Anomalia
051540070321if  1c                   if        $C_A_  = *on
051550070321     c                   eval      §LO1   = §PS1 + 0.050
051560070321     c                   write     A4prtLO
051570070321     c                   eval      §PS1   = §LO1
051580070321     c                   eval      $First = 'G'
051590070321     c                   eval      Par5   = 'Presente Comunicazione +
051600070321     c                                                Anomalia'
051610070321     c                   exsr      StampaAR5
051620070321e   1c                   endif
051630031124      *
051640031124      * - stampo box
051650070320      *   (§PB1 è da reimpostare in §LO1: contiene il suo valore PRIMA
051660070320      *   della stampa delle varie linee separatrici)
051670070315     c                   eval      §LO1   = §PB1
051680070315     c                   eval      §LO2   = §PS1 + 0.070
051690031124     c                   write     A4prtAR53
051700031124      *
051710031124     c                   ENDSR
051720090924      /free
051730090924
051740090924       //--------------------------------------------------------------
051750090924       //?Prepara elenco per box "Note per il destinatario"            ?
051760090924       //--------------------------------------------------------------
051770090924       BEGSR  StampaNxD;
051780090924
051790090924         // - Stampa testata
051800090924         //$First = *on;
051810090924         //Par5   = '                    '
051820090924         //       + '                    '
051830090924         //       + '          '
051840090924         //       + c_NxD;
051850090924         //exsr  StampaAR5;
051860090924
051870090924         // - Memorizzazione posizione iniziale per il box
051880090924         //   (§PB1 sarà da reimpostare in §LO1 PRIMA della stampa del box)
051890090924         //§PB1 = §LO1;
051900090924
051910090924         // - Stampa della riga separatrice iniziale
051920090924         //§LO1   = §PS1 + 0.050;
051930090924         //write  A4prtLO;
051940090924         //§PS1   = §LO1;
051950090924
051960090924         // - Stampa la singola riga di testo
051970090925         $First = '1';
051980090924         Par5 = 'DECODER DIGITALE TERRESTRE OMAGGIO DA SKY PER AVER +
051990090924                 ADERITO ALL''OFFERTA';
052000090924         exsr  StampaAR5;
052010090924
052020090924         // - Memorizzazione posizione iniziale per il box
052030090924         //   (§PB1 sarà da reimpostare in §LO1 PRIMA della stampa del box)
052040090924         §PB1 = §LO1;
052050090924
052060090924         // - Stampa box
052070090924         //   (§PB1 è da reimpostare in §LO1: contiene il suo valore PRIMA
052080090924         //   della stampa delle varie linee separatrici)
052090090924         §LO1 = §PB1;
052100090925         §LO2 = §PS1 + 0.050;
052110090924         write  A4prtAR53;
052120090924
052130090924       ENDSR;
052140090924
052150090924      /end-free
052160030515      *
052170030515      *---------------------------------------------------------------*
052180050217      *?Prepara elenco per box - "Bancali da rendere al mittente"    ?*
052190150224      *?                         » NON PIÙ «                         ?*
052200050217      *?                       - "Dati per destinatario supermercato"?*
052210150224      *?                         » NON PIÙ «                         ?*
052220050217      *?                       - "Promemoria per autotrasportatore"  ?*
052230030515      *---------------------------------------------------------------*
052240030515     c     ListaAR5      BEGSR
052250030515      *
052260060921     c                   eval      $FIAR5 = *on
052270030515      *
052280030515      * Ripulisco i campi utilizzati in seguito
052290030515     c                   clear                   AR5trd
052300030515     c                   clear                   AR5dac
052310150304     c*//                clear                   dAR5ban
052320030717     c                   clear                   dAR5bnb
052330030903     c                   clear                   dAR5rcc
052340030515     c                   clear                   dAR5scr
052350030515     c                   reset                   $First
052360030515     c                   reset                   $FirstSCR
052370030515     c                   reset                   SAVtrd
052380030515      *
052390030515      * Posizionamento iniziale
052400040301if  1c                   if        DUTlpo = 'S'
052410040531     c     KLDV          setgt     FIAR531C
052420040301x   1c                   else
052430040607     c     KLDV          setgt     FIAR5000
052440040301e   1c                   endif
052450030515      *
052460030515      * Ciclo di lettura del file FIAR5
052470030515      *
052480030515do  1c                   DO        *Hival
052490030515      *
052500040607if  2c                   if        DUTlpo = 'S'
052510040531     c     KLDV          readpe    FIAR531C
052520040607x   2c                   else
052530040607     c     KLDV          readpe    FIAR5000
052540040607e   2c                   endif
052550040301      *
052560040301if  2c                   if           (DUTlpo =  'S'
052570040531     c                             and %eof(FIAR531C)
052580040301     c                             OR  DUTlpo <> 'S'
052590040301     c                             and %eof(FIAR501L))
052600030515     c                   leave
052610030515e   2c                   endif
052620030515if  2c                   if        AR5atb <> *blanks
052630030515     c                   iter
052640030515e   2c                   endif
052650030515      *
052660030515      * Controllo se devo stampare l'intestazione al nuovo box
052670030515if  2c                   if        AR5trd <> SAVtrd
052680030515      *   Stampa del precedente box (se stampata almeno una riga)
052690030910if  3c                   if             SAVtrd <> *hival
052700030910     c                             and  $First =  *off
052710030911     c                   eval      §LO2   = §PS1 + 0.050
052720030515     c                   write     A4prtAR53
052730030515e   3c                   endif
052740030515     c                   eval      $First =  *on
052750030515     c                   eval      SAVtrd =  AR5trd
052760030515e   2c                   endif
052770030515      *
052780150224sel 2c                   SELECT
052790150224      *//*
052800150224      *//* > Bancali da rendere al mittente
052810150224      *//*   (unico record, sequenzialmente prima dei rec. "SCR")
052820150224w   2c*//                WHEN      %subst(DB0gva:1:1) = 'B'
052830150224     c*//                          and    AR5trd      = 'BAN'
052840150224     c*//                movel     AR5uni        dAR5ban
052850150224      *//* - stampo testata
052860150224if  3c*//                if           (§AR5tba <> *zeros
052870150224     c*//                          or  §AR5tb2 <> *zeros)
052880150224     c*//                eval      Par5   = 'BANCALI DA RIPORTARE A MAGAZZINO E'
052890150224     c*//                                 + ' DA RENDERE AL MITTENTE           '
052900150224     c*//                exsr      StampaAR5
052910150224e   3c*//                endif
052920150224      *//* - stampo 1ª riga
052930150224if  3c*//                if        §AR5tba <> *zeros
052940150224     c*//                clear                   Par5
052950150224     c*//                clear                   Wtba
052960150224     c*//                eval      Wtba   = §AR5tba
052970150224     c*//                exsr      Tab_TBA
052980150224     c*//                eval      Par5   = §TBAdesc + 'N. '
052990150224     c*//                                 + %editc(§AR5nba : 'Z')
053000150224     c*//                exsr      StampaAR5
053010150224e   3c*//                endif
053020150224      *//* - stampo 2ª riga
053030150224if  3c*//                if        §AR5tb2 <> *zeros
053040150224     c*//                clear                   Par5
053050150224     c*//                clear                   Wtba
053060150224     c*//                eval      Wtba   = §AR5tb2
053070150224     c*//                exsr      Tab_TBA
053080150224     c*//                eval      Par5   = §TBAdesc + 'N. '
053090150224     c*//                                 + %editc(§AR5nb2 : 'Z')
053100150224     c*//                exsr      StampaAR5
053110150224e   3c*//                endif
053120150224      *//*
053130150224      *//* > Bancali da riportare a magazzino ("Oltre 20 colli")
053140150224      *//*   (unico record, sequenzialmente prima dei rec. "SCR",
053150150224      *//*    alternativo a "BAN" = bancali da rendere al mittente).
053160150224w   2c*//                WHEN      %subst(DB0gva:1:1) = 'O'
053170150224     c*//                          and    AR5trd      = 'BNB'
053180150224     c*//                movel     AR5uni        dAR5bnb
053190150224      *//* - stampo testata
053200150224if  3c*//                if           (§AR5btba <> *zeros
053210150224     c*//                          or  §AR5btb2 <> *zeros)
053220150224     c*//                eval      Par5   = 'BANCALI DA RIPORTARE A MAGAZZINO  '
053230150224     c*//                exsr      StampaAR5
053240150224e   3c*//                endif
053250150224      *//* - stampo 1ª riga
053260150224if  3c*//                if        §AR5btba <> *zeros
053270150224     c*//                clear                   Par5
053280150224     c*//                clear                   Wtba
053290150224     c*//                eval      Wtba   = §AR5btba
053300150224     c*//                exsr      Tab_TBA
053310150224     c*//                eval      Par5   = §TBAdesc + 'N. '
053320150224     c*//                                 + %editc(§AR5bnba : 'Z')
053330150224     c*//                exsr      StampaAR5
053340150224e   3c*//                endif
053350150224      *//* - stampo 2ª riga
053360150224if  3c*//                if        §AR5btb2 <> *zeros
053370150224     c*//                clear                   Par5
053380150224     c*//                clear                   Wtba
053390150224     c*//                eval      Wtba   = §AR5btb2
053400150224     c*//                exsr      Tab_TBA
053410150224     c*//                eval      Par5   = §TBAdesc + 'N. '
053420150224     c*//                                 + %editc(§AR5bnb2 : 'Z')
053430150224     c*//                exsr      StampaAR5
053440150224e   3c*//                endif
053450150224      *//* - stampo 3ª riga
053460150224if  3c*//                if        §AR5bcor <> *zeros
053470150224     c*//                clear                   Par5
053480150224     c*//                eval      Par5   = C_CoOr
053490150224     c*//                                 + %editc(§AR5bcor : 'K')
053500150224     c*//                exsr      StampaAR5
053510150224e   3c*//                endif
053520030515      *
053530030515      * > Destinatario supermercato
053540030515w   2c                   WHEN      (DB0tc1 = 'S' or DB0tc2 = 'S')
053550030515     c                             and AR5trd  = 'SCR'
053560030515     c                   movel     AR5uni        dAR5scr
053570030515      * - stampo testata
053580030515      *   (se subr. richiamata solo per verifica esistenza dati: esco)
053590030515if  3c                   if        $First =  *on
053600031124     c                   eval      Par5   = C_DeSM
053610030515     c                   exsr      StampaAR5
053620030515     c                   eval      $FirstSCR  = *off
053630030515e   3c                   endif
053640030515      * - stampo righe
053650030515     c                   clear                   Par5
053660030624     c                   eval      *in38 = *on
053670030515if  3c                   if           AR5dac <> SAVdac
053680030515     c                             or AR5orc <> SAVorc
053690030515     c                   eval      SAVdac = AR5dac
053700030515     c                   eval      SAVorc = AR5orc
053710030515      * - - giro data modifica
053720030515     c                   z-add     AR5dac        w0080
053730070315     c     *iso          test(d)                 w0080                  31
053740070315if  4c                   if        NOT *in31
053750071025     c     *iso          movel     w0080         Data_EUR
053760071025     c     *eur          movel     Data_EUR      w0080
053770070315x   4c                   else
053780070315     c                   clear                   w0080
053790070315e   4c                   endif
053800030515     c                   eval      Par5 = 'Modifica del '
053810030515     c                                  + %editc(w0080 : 'Y')
053820030515     c                                  + ' '
053830030515     c                                  + %editw(AR5orc : '  :  :  ')
053840030515     c                                  + ' '
053850040223     c                                  + §AR5Spru
053860030515     c                   exsr      StampaAR5
053870030515e   3c                   endif
053880040223     c                   eval      Par5 = §AR5Snom
053890030911     c                                  + '   Tel.: '
053900040223     c                                  + §AR5Stel
053910030911     c                   exsr      StampaAR5
053920040223     c                   eval      Par5 = §AR5Smot
053930030515     c                   exsr      StampaAR5
053940030624     c                   eval      *in38 = *off
053950031124      *
053960031124      * > Se destinatario assente inserire il plico in buca lettere
053970031124      *   (già stampato in box "Promemoria per autotrasportatore")
053980031124w   2c***                WHEN          DB0gma  = Ktab7V_CB
053990031124     c***                ...       ...
054000030903      *
054010030903      * > O.R.M. per ritiro contestuale alla consegna
054020030903      *   (unico record)
054030050510      *   Verrà stampato come ultimo box
054040050510      *   - prima del (eventuale) DDT del mittente
054050050510      *   - prima della copia per il destinatario
054060030910w   2c***                WHEN          DB0gma  = Ktab7V_RC
054070030910     c***                          and AR5trd  = 'RCC'
054080030903     c***                ...       ...
054090030515      *
054100030515e   2c                   ENDSL
054110030515      *
054120030515e   1c                   ENDDO
054130030515      *
054140030515      * Stampa del (ultimo) box (se stampata almeno una riga)
054150030515if  1c                   if        $First = *off
054160030911     c                   eval      §LO2   = §PS1 + 0.050
054170030515     c                   write     A4prtAR53
054180030515e   1c                   endif
054190030515      *
054200030515      * > Destinatario supermercato
054210030515      *   (stampa del box vuoto per memo se non già stampata almeno
054220030515      *   una riga)
054230030515if  1c                   IF        (DB0tc1 = 'S' or DB0tc2 = 'S')
054240030515     c                             and              $FirstSCR = *on
054250030515      * - stampo testata
054260030515     c                   eval      $First = *on
054270031124     c                   eval      Par5   = C_DeSM
054280030515     c                   exsr      StampaAR5
054290030515     c                   eval      $FirstSCR  = *off
054300030515      * - stampo 2 righe vuote
054310030515     c                   clear                   Par5
054320030515do  2c                   do        2
054330030515     c                   exsr      StampaAR5
054340030515e   2c                   enddo
054350030515      * - stampo box
054360030911     c                   eval      §LO2   = §PS1 + 0.050
054370030515     c                   write     A4prtAR53
054380030515e   1c                   ENDIF
054390070123      *
054400070123     c                   eval      $FIAR5 = *off
054410030515      *
054420030515     c                   ENDSR
054430030515      *
054440030515      *---------------------------------------------------------------*
054450050217      *?Stampa singola riga relativa a FIAR5                         ?*
054460030515      *---------------------------------------------------------------*
054470030515     c     StampaAR5     BEGSR
054480030515      *
054490030515      * Controllo se superata l'ultima riga
054500030515     c                   exsr      Test_OF
054510030515      *
054520030515      * Stampa del singolo record
054530030909sel 1c                   select
054540030909      *
054550030909w   1c                   when      $First = *off
054560030909      * - Incremento parametri per posizionamento nuova riga
054570030911     c                   eval      §PS1   = §PS1 + 0.150
054580030909     c                   write     A4prtAR52
054590030909      *
054600030909w   1c                   when      $First = *on
054610030515      * - Incremento parametri per posizionamento nuovo box
054620030515      *   e relativa intestazione
054630030911     c                   eval      §LO1   = §LO2 + 0.150
054640030911     c                   eval      §PS1   = §LO1 + 0.150
054650030515     c                   write     A4prtAR51
054660030515     c                   eval      $First = *off
054670060524      *
054680060524w   1c                   when      $First = '2'
054690060524      * - Incremento parametri per posizionamento nuova riga
054700060524      *   nel box "Promemoria per autotrasportatore"
054710060524     c                   eval      §PS1   = §PS1 + 0.150
054720060524     c                   write     A4prtAR51
054730060524     c                   eval      $First = *off
054740030909      *
054750030909w   1c                   when      $First = 'A'
054760030909      * - Incremento parametri per posizionamento nuovo box
054770030909      *   e relativa intestazione
054780030911     c                   eval      §LO1   = §LO2 + 0.150
054790030909     c                   eval      §PS1   = §LO1 + 0.200
054800030909     c                   write     A4prtAR5a
054810030909     c                   eval      $First = *off
054820150226      *//*
054830150226w   1c*//                when      $First = 'B'
054840150226      *//* - Incremento parametri per posizionamento nuovo box
054850150226      *//*   ed intestazione "Promemoria per autotrasportatore"
054860150226     c*//                eval      §LO1   = §LO2 + 0.150
054870150226     c*//                eval      §PS1   = §LO1 + 0.125
054880150226     c*//                write     A4prtAR5b
054890150226     c*//                eval      $First = 'C'
054900150226      *//*
054910150226w   1c*//                when      $First = 'C'
054920150226      *//* - Incremento parametri per posizionamento nuova riga
054930150226      *//*   nel box "Promemoria per autotrasportatore"
054940150226     c*//                eval      §PS1   = §PS1 + 0.250
054950150226     c*//                write     A4prtAR5c
054960070321      *
054970070321w   1c                   when      $First = 'G'
054980070321      * - Incremento parametri per posizionamento nuova riga
054990070321      *   nel box "Promemoria per autotrasportatore"
055000070321     c                   eval      §PS1   = §PS1 + 0.170
055010070321     c                   write     A4prtAR5g
055020030909      *
055030030909e   1c                   endsl
055040030515      *
055050030515     c                   ENDSR
055060030910      *
055070030910      *---------------------------------------------------------------*
055080050217      *?Stampa box "Ritiro Contestuale alla Consegna" da FIAR5/RCC   ?*
055090030910      *---------------------------------------------------------------*
055100030910     c     StampaRCC     BEGSR
055110030910      *
055120030910      * > O.R.M. per ritiro contestuale alla consegna
055130030910      *   (unico record)
055140031124      *   Verrà stampato come ultimo box (prima della copia per il
055150031124      *     destinatario)
055160030910      *
055170030910     c                   clear                   dAR5rcc
055180030910      *
055190030910     c                   eval      AR5trd = 'RCC'
055200040301if  1c                   if        DUTlpo = 'S'
055210040531     c     K05AR501      chain     FIAR531C
055220040301x   1c                   else
055230030910     c     K05AR501      chain     FIAR5000
055240040301e   1c                   endif
055250040301      *
055260071008if  1c                   if           (DUTlpo =  'S'
055270040531     c                             and %found(FIAR531C)
055280040301     c                             OR  DUTlpo <> 'S'
055290040301     c                             and %found(FIAR501L))
055300040301     c                             and AR5atb =  *blanks
055310030910     c                   movel     AR5uni        dAR5rcc
055320071015     c                   eval      ORMpoe = §AR5Rpoe
055330071015     c                   eval      ORMnsr = §AR5Rnsr
055340071015     c                   eval      ORMnor = §AR5Rnor
055350071015     c                   eval      ORMnrv = §AR5Rnrv
055360071015     c                   eval      Pbt2a  = 'RITIRO CONTESTUALE ALLA +
055370071015     c                                       CONSEGNA'
055380071015     c                   exsr      StampaORM
055390071015e   1c                   endif
055400071015      *
055410071015     c                   ENDSR
055420071015      *
055430071015      *---------------------------------------------------------------*
055440071015      *?Stampa box "Ritiro Porto Franco PrePagato"                   ?*
055450071015      *---------------------------------------------------------------*
055460071015     c     StampaRPFPP   BEGSR
055470071015      *
055480071015      * > O.R.M. per ritiro porto franco prepagato
055490071015      *   Verrà stampato come ultimo box (prima della copia per il
055500071015      *     destinatario)
055510071015      *
055520071015     c*** già fatto:     clear                   dsBL4M
055530071015     c*** già fatto:     exsr      RepAR4_M
055540071015      *
055550071015if  1c                   if             §B4poe  > *zeros
055560071015     c                             and  §B4nor  > *zeros
055570071015     c                   eval      ORMpoe = §B4poe
055580071015     c                   eval      ORMnsr = §B4nsr
055590071015     c                   eval      ORMnor = §B4nor
055600071015     c                   eval      ORMnrv = §B4nrv
055610071015     c                   eval      Pbt2a  = 'RITIRO PORTO FRANCO +
055620071015     c                                       PREPAGATO'
055630071015     c                   exsr      StampaORM
055640071015e   1c                   endif
055650071015      *
055660071015     c                   ENDSR
055670071015      *
055680071015      *---------------------------------------------------------------*
055690071015      *?Stampa dati O.R.M. in box "Ritiro Contestuale alla Consegna" ?*
055700071015      *?              o in quello "Ritiro Porto Franco Prepagato"    ?*
055710071015      *---------------------------------------------------------------*
055720071015     c     StampaORM     BEGSR
055730071015      *
055740071015     c                   eval      $FIAR5 = *on
055750071015      *
055760071015if  2c                   if        not %open(FNORM01L)
055770071015     c                   open      FNORM01L
055780071015e   2c                   endif
055790071015     c     K04ORM01      chain     FNORM000
055800071015      *
055810071015if  2c                   IF        %found(FNORM01L)
055820071015      *
055830071015      * Reperisco la posizione (in pollici) in cui posizionarmi
055840071015      *   per la stampa in base all'ultima riga stampata
055850071015      *   se non ho ancora stampato box
055860071015cas 3c     §LO2          caseq     *zeros        Rep_§LO2
055870071015e   3c                   endcs
055880071025      *
055890071015      * - stampo testata
055900071025      *   (con il n° O.R.M.)
055910071015     c                   eval      $First = 'A'
055920071015     c*** già impostato: eval      Pbt2a  = 'RITIRO CONTESTUALE ALLA +
055930071015     c***  "      "                          CONSEGNA'
055940071015     c                   eval      Pbt2b  = 'O.R.M. numero '
055950071015     c                                    + %editw(ORMpoe : '0   ')
055960071015     c                                    + ' '
055970071015     c                                    + %editw(ORMnsr : '0  ')
055980071015     c                                    + '  '
055990071015     c                                    + %editc(ORMnor : 'Z')
056000071015     c                                    + ' '
056010071015     c                                    + %editw(ORMnrv : '0  ')
056020071015     c                   exsr      StampaAR5
056030071025      *
056040071015      * - stampo i dati dell'O.R.M.
056050071015     c                   exsr      QUM_FnOrm
056060140410     c                   eval      Par5   = 'Quantità      : '
056070071015     c                                    + %trim($Qum(1)) + '    '
056080071015     c                                    + %trim($Qum(2)) + '    '
056090071015     c                                    + %trim($Qum(3))
056100071015     c                   exsr      StampaAR5
056110080228if  3c                   if        DB0tbo = '8'
056120140410     c                   eval      Par5   = 'Mittente      : '
056130080228     c                                    + ORMrsr
056140080228x   3c                   else
056150140410     c                   eval      Par5   = 'Destinatario  : '
056160071015     c                                    + ORMrsc
056170080228e   3c                   endif
056180071015     c                   exsr      StampaAR5
056190080228if  3c                   if        DB0tbo = '8'
056200140410     c                   eval      Par5   = '                '
056210080228     c                                    + ORMinr
056220080228x   3c                   else
056230140410     c                   eval      Par5   = '                '
056240071015     c                                    + ORMinc
056250080228e   3c                   endif
056260071015     c                   exsr      StampaAR5
056270080228if  3c                   if        DB0tbo = '8'
056280140410     c                   eval      Par5   = '                '
056290080228     c                                    + ORMcar + ' '
056300080228     c                                    + ORMlor + ' '
056310080228     c                                    + ORMprr + ' '
056320080228     c                                    + ORMnar
056330080228x   3c                   else
056340140410     c                   eval      Par5   = '                '
056350071015     c                                    + ORMcac + ' '
056360071015     c                                    + ORMloc + ' '
056370071015     c                                    + ORMprc + ' '
056380071015     c                                    + ORMnac
056390080228e   3c                   endif
056400071015     c                   exsr      StampaAR5
056410101022             // -?SE Ritiro Porto Franco Prepagato:?
056420080228if  3c                   if        DB0tbo = '8'
056430101022      /free
056440101022               // -?stampo il referente ed il relativo telefono?
056450101022               // -?(Comunque! Ci sono sempre!)?
056460140410               Par5   = 'Referente     : ' + ORMrer + '   '
056470101022                      + 'Telefono: '     + ORMter;
056480101022               exsr  StampaAR5;
056490101022      /end-free
056500140410     c                   eval      Par5   = 'Ordinante     : '
056510080228     c                                    + ORMrso
056520080228     c                   exsr      StampaAR5
056530140410     c                   eval      Par5   = 'Destinatario  : '
056540080303     c                                    + ORMrsc
056550080303     c                   exsr      StampaAR5
056560080228e   3c                   endif
056570071015      * - stampo le note
056580071015      *   (Comunque! Non solo "se ci sono". Al limite: scarto la 1ª.)
056590140410     c                   eval      Par5   = 'Note          : '
056600071015if  3c                   if        ORMno1 <> *blanks
056610071015     c                   eval      Par5   = %trim(Par5)  + ' '
056620071015     c                                    + ORMno1 + ' ' + ORMno2
056630071015x   3c                   else
056640071015     c                   eval      Par5   = %trim(Par5)  + ' '
056650071015     c                                    + ORMno2
056660071015e   3c                   endif
056670071015     c                   exsr      StampaAR5
056680071025      *
056690140410      * -?SE Ritiro Porto Franco Prepagato:?
056700071025if  3c                   IF        DB0tbo = '8'
056710140410      *
056720140410      * - stampo orari apertura (SE ci sono)
056730140410     c                   exsr      sr_OrariApert
056740140410      *
056750140410      * - stampo data & ora ritiro
056760071025     c                   z-add     ORMdar        w0080
056770071025     c     *iso          test(d)                 w0080                  31
056780071025if  4c                   if        NOT *in31
056790071025     c     *iso          movel     w0080         Data_EUR
056800071025     c     *eur          movel     Data_EUR      w0080
056810071025x   4c                   else
056820071025     c                   clear                   w0080
056830071025e   4c                   endif
056840140410     c                   eval      Par5   = 'Data ritiro   : '
056850071025     c                                    + %editc(w0080:'Y')
056860071025if  4c                   if        ORMorr > 1230
056870071025     c                   eval      Par5   = %trimr(Par5)
056880071025     c                                    + '          '
056890140410     c                                    + 'Ora ritiro: '
056900071025     c                                    + %editw(ORMorr:'0 :  ')
056910071025     c                   exsr      StampaAR5
056920071025x   4c                   else
056930071025     c                   exsr      StampaAR5
056940071025     c                   clear                   Par5
056950071025     c                   eval      %subst(Par5:50)
056960071025     c                                    = 'Ora ritiro: '
056970071025     c                                    + %editw(ORMorr:'0 :  ')
056980071025     c                   write     A4prtAR51
056990071025e   4c                   endif
057000140410      *
057010071025e   3c                   ENDIF
057020071025      *
057030071015      * - imposto codice a barre O.R.M.
057040071015     c                   movel     BarCodeORM    Pbco
057050071015      * - verifico se codice a barre da stampare
057060071015     c                   eval      *in52  =  (Pbco <> *zeros)
057070071025      *
057080071015      * - stampo box
057090071015     c                   eval      §LO2   = §PS1 + 0.050
057100071015if  3c                   if        NOT *in52
057110071015     c                   write     A4prtAR53
057120071015x   3c                   else
057130071025     c                   eval      §LV1   = §LO2 - §LO1
057140071015     c                   eval      §PS2   = §LO2 - 0.500
057150071015     c                   write     A4prtAR54
057160071015e   3c                   endif
057170071015      *
057180071015e   2c                   ENDIF
057190071015      *
057200071015     c                   eval      $FIAR5 = *off
057210071015      *
057220071015     c                   ENDSR
057230100513      /free
057240140410
057250140410       //--------------------------------------------------------------
057260140410       //?Reperimento, impostazione e stampa degli Orari di Apertura   ?
057270140410       //--------------------------------------------------------------
057280140410       BEGSR  sr_OrariApert;
057290140410
057300140410         clear  dOREorari;
057310140410
057320140410         // -?Recupero dati da estensione FNORE (trc "O ")?
057330140410         if  NOT  %open(FNORE01L);
057340140410           open  FNORE01L;
057350140410         endif;
057360140410         k_OREpoe = ORMpoe;
057370140410         k_OREnsr = ORMnsr;
057380140410         k_OREnor = ORMnor;
057390140410         k_OREnrv = ORMnrv;
057400140410         k_OREtrc = 'O ';
057410140410         chain  %kds(keyFNORE01)  FNORE000;
057420140410
057430140410         // -?...NON trovati => uscita dalla subroutine?
057440140410         if  Not  %found(FNORE01L);
057450140410           leavesr;
057460140410         endif;
057470140410
057480140410         // -?...Trovati => verifica degli orari?
057490140410         dOREorari = OREdati;
057500140410
057510140410         Select;
057520140410
057530140410           // -?C'è un solo orario "ALLE"?
057540140410           When  §OREoraMda = *zero  and  §OREoraMa > *zero  and
057550140410                 §OREoraPda = *zero  and  §OREoraPa = *zero;
057560140410             Par5 = 'Orari apertura: ' +
057570140410                    'Fino alle ' + %editw( §OREoraMa : ' 0:  ' );
057580140410           When  §OREoraMda = *zero  and  §OREoraMa = *zero  and
057590140410                 §OREoraPda = *zero  and  §OREoraPa > *zero;
057600140410             Par5 = 'Orari apertura: ' +
057610140410                    'Fino alle ' + %editw( §OREoraPa : ' 0:  ' );
057620140410
057630140410           // -?C'è un solo orario completo (per 1°)?
057640140410           When  §OREoraMda > *zero  and  §OREoraMa > *zero  and
057650140410                 §OREoraPda = *zero  and  §OREoraPa = *zero;
057660140410             Par5 = 'Orari apertura: ' +
057670140410                    %editw( §OREoraMda : ' 0:  ' ) + '  ' +
057680140410                    %editw( §OREoraMa  : ' 0:  ' );
057690140410
057700140410           // -?Ci sono due orari completi?
057710140410           When  §OREoraMda > *zero  and  §OREoraMa > *zero  and
057720140410                 §OREoraPda > *zero  and  §OREoraPa > *zero;
057730140410             Par5 = 'Orari apertura: ' +
057740140410                    %editw( §OREoraMda : ' 0:  ' ) + '  ' +
057750140410                    %editw( §OREoraMa  : ' 0:  ' ) + ' - ' +
057760140410                    %editw( §OREoraPda : ' 0:  ' ) + '  ' +
057770140410                    %editw( §OREoraPa  : ' 0:  ' );
057780140410
057790140410         EndSl;
057800140410
057810140410         exsr  StampaAR5;
057820140410
057830140410       ENDSR;
057840100513
057850100513       //--------------------------------------------------------------
057860100513       //?Verifica se presente ORM con preavviso mail                  ?
057870100513       //--------------------------------------------------------------
057880100513       BEGSR  sr_CheckORM_PrevvMail;
057890100514
057900100514         //clear  dOrm01               (già fatto)
057910100513
057920100513         // -?Reperimento num. ORM (se NON già reperito)?
057930100513         if  DB0tbo <> '8';
057940100513           exsr  RepAR4_M;
057950100513         endif;
057960100513
057970100513         // -?ORM inesistente: inutile proseguire?
057980100513         if  §B4poe = *zero    or   §B4nor = *zero;
057990100513           leavesr;
058000100513         endif;
058010100513
058020100513         // -?Verifica presenza O.R.M.?
058030100513         if  Not  %open(FNORM01L);
058040100513           open  FNORM01L;
058050100513         endif;
058060100513
058070100513         ORMpoe = §B4poe;
058080100513         ORMnsr = §B4nsr;
058090100513         ORMnor = §B4nor;
058100100513         ORMnrv = §B4nrv;
058110100513
058120100513         chain  K04ORM01  FNORM000;
058130100513
058140100513         // -?ORM non reperito: inutile proseguire?
058150100513         if  Not  %found(FNORM01L);
058160100513           leavesr;
058170100513         endif;
058180100513
058190100513         // -?Impostazione flag per stampa porto "ASSEGNATO"?
058200100513         dOrm01 = ORMflo;
058210100513         $PreavvORM = (§ORMpre = 'M');
058220100513
058230100513       ENDSR;
058240100513
058250100513      /end-free
058260030515      *
058270030515      *---------------------------------------------------------------*
058280050217      *?Stampa dati DPD                                              ?*
058290030515      *---------------------------------------------------------------*
058300030515     c     StampaDPD     BEGSR
058310030515      *
058320080922      * (EX) Bar-Code - non più stampato come codice a barre
058330030515      * (1' 12 char di Riferimento Mittente Alfanumerico - se numerici)
058340060626      *---
058350061030      * Il barcode DPD è stato variato ora può essere anche
058360061030      *   alfanumerico ed è lungo 14
058370061030      * Si trova sul file FIAR4 rcd 'I'
058380061030      * I controlli che faceva prima sono da cambiare
058390061030      *---
058400061030      *
058410060626      * nuovi controlli per nuovo barcode DPD
058420061030if  1c                   if        §B4Ipn <> *blanks
058430061030      * - controllo se vecchio o nuovo barcode
058440061030if  2c                   if        %subst(§B4Ipn:14:1) = *blanks
058450061030      * - - vecchio
058460061030     c                   eval      Pbcd2 = '01 ' + %subst(§B4Ipn:1:3) +
058470061030     c                                     ' '   + %subst(§B4Ipn:4:8)
058480061030      * - - nuovo
058490061030x   2c                   else
058500061030     c                   eval      Pbcd2 = '01 ' + %subst(§B4Ipn:1:4) +
058510061030     c                                     ' '   + %subst(§B4Ipn:5:10)
058520061030e   2c                   endif
058530061030e   1c                   endif
058540080922      /free
058550080922         // Impostazione data
058560080922         PdpA = 'Date ' + %editc(Pdsp:'Y');
058570080922
058580080922         // Impostazione numero colli
058590080922         PdpB = 'Parcel N. ' + %editc(Pncl:'Z');
058600080922
058610080922         // Impostazione numero parcel
058620080922         PdpC = §B4Ipn;
058630080922
058640080922         // Impostazione "costante" da stampare tra le Note
058650080922         PdpD = 'Controllare la corrispondenza';
058660080922         PdpE = 'del destinatario con l''indirizzo';
058670080922         PdpF = 'dell''etichetta DPD.';
058680080922
058690080922         // Impostaz. posizioni sulla copia per il vettore già stampata
058700080922         §DP1 = wPosIni + 1.100;
058710080922         §DP2 = §DP1 + 0.550;
058720080922         §DP3 = §DP2 + 0.120;
058730080922         §DP4 = §DP3 + 0.120;
058740080922         §DP5 = §DP4 + 0.120;
058750080922         §DP6 = §DP5 + 0.160;
058760080922         §DP7 = §DP6 + 0.130;
058770080922         §DP8 = §DP7 + 0.130;
058780080922      /end-free
058790080922      *
058800080922      * (inutile testare l'overflow perchè siamo in ex-A5)
058810080922     c                   write     A4prtDPD
058820030515      *
058830030515     c                   ENDSR
058840030515      *
058850030515      *---------------------------------------------------------------*
058860050217      *?Prepara elenco per box - Elenco "Package Tracking Number"    ?*
058870030515      *---------------------------------------------------------------*
058880030515     c     ListaFED      BEGSR
058890030515      *
058900030515      * Stampo il simbolo "FedEx" (sulla 1ª pagina):
058910030515      *   imposto le posizioni iniziali di stampa per le linee
058920030515      *   (orizzontali-§LOn e verticali-§LVn)
058930080912     c                   eval      §LO1   = 3.330
058940080912     c                   eval      §LO2   = §LO1 + 0.370
058950081023     c                   eval      §LV1   = §LO2 - 0.150
058960081023     c                   eval      §LV2   = §LO2 - 0.070
058970030515     c                   write     A4prtFED1
058980080818      *
058990080818      * Stampo le "costanti" di inizio lista
059000030515     c                   write     A4prtFED2
059010030515      *
059020030515      * Stampo elenco segnacolli FedEx
059030030709     c                   eval      §LO1   = §LO2 - 0.250
059040030515      * - per le bolle in partenza
059050030515if  1c                   IF           DB0tbo = 'P'
059060030515     c                             or DB0tbo = '8'
059070030515if  2c                   if        NOT %open(FNBLT01L)
059080030515     c                   open      FNBLT01L
059090030515e   2c                   endif
059100030515      * - per le bolle in arrivo
059110030515x   1c                   ELSE
059120070319if  2c                   if        NOT %open(FNART01L)
059130070319     c                   open      FNART01L
059140070319e   2c                   endif
059150030515e   1c                   ENDIF
059160070319if  1c                   if        NOT %open(FIARS01L)
059170070319     c                   open      FIARS01L
059180070319e   1c                   endif
059190030515     c                   eval      Wtrc = 'C'
059200030515     c                   clear                   Wnsc
059210030515     c                   clear                   Wawb
059220030515do  1c                   dou       Wawb = *blanks
059230030515     c                   exsr      StampaFED
059240030515e   1c                   enddo
059250051020      *
059260030515     c                   ENDSR
059270030515      *
059280030515      *---------------------------------------------------------------*
059290050217      *?Stampa singola riga relativa a FEDEX                         ?*
059300030515      *---------------------------------------------------------------*
059310030515     c     StampaFED     BEGSR
059320030515      *
059330030515      * Impostazione campi di stampa
059340030515     c                   clear                   Wawb
059350030515      * - Carico AWB FedEx
059360030515      * - - leggo segnacolli per bolle in partenza
059370030515if  1c                   IF           DB0tbo = 'P'
059380030515     c                             or DB0tbo = '8'
059390030515     c                   exsr      CarAWB_Blx
059400030515      * - - leggo segnacolli per bolle in arrivo
059410030515x   1c                   ELSE
059420030515     c                   exsr      CarAWB_Arx
059430030515e   1c                   ENDIF
059440030515     c     wAWB          cabeq     *blanks       End_FED
059450030515     c                   move      wAWB          Pptn
059460030515     c                   add       1             Psts
059470030515      *
059480030515      * Controllo se superata l'ultima riga
059490030515     c                   exsr      Test_OF
059500030709      *
059510030709      * Imposto le posizioni iniziali di stampa per le linee
059520030709      *   (orizzontali-§LOn e verticali-§LVn) e per i campi (§PSn)
059530030709     c                   eval      §LO1 = §LO1 + 0.250 - 0.008
059540030709     c                   eval      §LO2 = §LO1 + 0.250
059550030709     c                   eval      §LV1 = §LO1
059560030709     c                   eval      §LV2 = §LO2 - 0.100
059570030709     c                   eval      §PS1 = §LO1 + 0.200
059580030515      *
059590030515      * Stampa del singolo record
059600030515     c                   write     A4prtFED3
059610030515      *
059620030515     c     End_FED       ENDSR
059630030515      *
059640030515      *---------------------------------------------------------------*
059650051020      *?Reperimento AWB FedEx da file FNBLT e FIARS (bolle partenza) ?*
059660030515      *---------------------------------------------------------------*
059670030515     c     CarAWB_Blx    BEGSR
059680130215      *
059690130215      * Verifica se, al di là del codice bolla (vedi "$ "), esiste
059700130215      *   comunque una bolla mamma
059710130215     c                   exsr      sr_Mamma_FedEx
059720030515      *
059730030515      * Posizionamento
059740130215if  2c                   if        $Mamma = *off   and
059750130215     c                             $Mamma_FedEx = *off
059760040607     c     KLDVnsc       setgt     FNBLT000
059770070514x   2c                   else
059780070514     c     KLBLnsc       setgt     FNBLT000
059790070514e   2c                   endif
059800030515      *
059810030515      * Ciclo di lettura
059820030515do  1c                   DO        *hival
059830030515      *
059840130215if  2c                   if        $Mamma = *off   and
059850130215     c                             $Mamma_FedEx = *off
059860040607     c     KLDV          reade     FNBLT000
059870070514x   2c                   else
059880070514     c     KLBL          reade     FNBLT000
059890070514e   2c                   endif
059900030515if  2c                   if        %eof(FNBLT01L)
059910030515     c                   leave
059920030515e   2c                   endif
059930030515      * - per ogni segnacollo cerco l'AWB
059940060505     c     K05ARS01P     chain     FIARS000
059950060505if  2c                   if            %found(FIARS01L)
059960060505     c                             and %subst(ARSnot:1:1) >= *zeros
059970060505     c                   eval      wAWB = %subst(ARSnot:1:12)
059980060505     c                   eval      wNSC = ARSnsc
059990030515     c                   leave
060000030515e   2c                   endif
060010030515      *
060020030515e   1c                   ENDDO
060030030515      *
060040030515     c                   ENDSR
060050030515      *
060060030515      *---------------------------------------------------------------*
060070070319      *?Reperimento AWB FedEx da file FNART e FIARS (bolle arrivo)   ?*
060080030515      *---------------------------------------------------------------*
060090030515     c     CarAWB_Arx    BEGSR
060100130215      *
060110130215      * Verifica se, al di là del codice bolla (vedi "$ "), esiste
060120130215      *   comunque una bolla mamma
060130130215     c                   exsr      sr_Mamma_FedEx
060140030515      *
060150030515      * Posizionamento
060160130215if  2c                   if        $Mamma = *off   and
060170130215     c                             $Mamma_FedEx = *off
060180040607     c     KLDVnsc       setgt     FNART000
060190070514x   2c                   else
060200130215     c     KLBLnsc       setgt     FNART000
060210070514e   2c                   endif
060220030515      *
060230030515      * Ciclo di lettura
060240030515do  1c                   DO        *hival
060250030515      *
060260130215if  2c                   if        $Mamma = *off   and
060270130215     c                             $Mamma_FedEx = *off
060280040607     c     KLDV          reade     FNART000
060290070514x   2c                   else
060300070514     c     KLBL          reade     FNART000
060310070514e   2c                   endif
060320030515if  2c                   if        %eof(FNART01L)
060330030515     c                   leave
060340030515e   2c                   endif
060350030515      * - per ogni segnacollo cerco l'AWB
060360060505     c     K05ARS01A     chain     FIARS000
060370060505if  2c                   if            %found(FIARS01L)
060380030515     c                             and %subst(ARSnot:1:1) >= *zeros
060390030515     c                   eval      wAWB = %subst(ARSnot:1:12)
060400030515     c                   eval      wNSC = ARSnsc
060410030515     c                   leave
060420030515e   2c                   endif
060430030515      *
060440030515e   1c                   ENDDO
060450030515      *
060460030515     c                   ENDSR
060470130215      *
060480130215      /free
060490130215
060500130215       //--------------------------------------------------------------
060510130215       //?Verifica se la spedizione FedEx ha bolla mamma.              ?
060520130215       //--------------------------------------------------------------
060530130215       BEGSR  sr_Mamma_FedEx;
060540130215
060550130215         //$Mamma_FedEx = *off;          (già così)
060560130215
060570130215         If  Not $Mamma;
060580130215
060590130215           // -?Verifica se la spedizione ha comunque mamma?
060600130215           //  ?(al di là del codice bolla - vedi cod. "$ ")?
060610130215           if  not %open(FNLBL01L);
060620130215             open  FNLBL01L;
060630130215           endif;
060640130215           chain  (DB0aas : DB0lnp : DB0nrs : DB0nsp)  FNLBL000;
060650130215
060660130215           $Mamma_FedEx = %found(FNLBL01L);
060670130215
060680130215         EndIf;
060690130215
060700130215       ENDSR;
060710130215
060720130215      /end-free
060730040607      *
060740040607      *---------------------------------------------------------------*
060750080918      *?Stampa DDT / PACKING-LIST del mittente                       ?*
060760040607      *---------------------------------------------------------------*
060770040607     c     StampaDDT     BEGSR
060780130315      *
060790130315      * - Uscita in assenza del file FIAR801L
060800130315     c                   if        Not  $E_AR8
060810130315     c                   leavesr
060820130315     c                   endif
060830080825      *
060840080825      * Meglio togliere l'impostazione relativa ai segnacolli non seq.
060850080825      *   per far funzionare il 1° posizionamento nella subr. Rep_§LO2
060860080825     c                   reset                   $A4_x_ScNS
060870080918      /free
060880080923         *in45 = (DB0fa4 = 'P');
060890080923         *in46 = (DB0fa4 = 'P'   and   %rem(wNumLdv : 2) = *zero);
060900080918      /end-free
060910040607      *
060920040607      * Se già stampato qualcos'altro:
060930040607      *   forzo il salto alla pagina successiva
060940080926       // (SE NON si sta stampando SOLO una packing-list, nel qual caso
060950080926       //  il posizionamento nella pagina A4 verrà eseguito dagli
060960080926       //  indicatori 45 e 46)
060970080926if  1c                   if           (CurLineA4 > $MaxLine1
060980080926     c                             or  §LO2      > $LimOF1)
060990080926     c                             and NOT *in45
061000040607     c                   eval      CurLineA4 = wMaxLine
061010040607     c                   exsr      Test_OF
061020040607e   1c                   endif
061030040607      *
061040040607     c                   eval      $DDT_p = *on
061050040607      *
061060130315      /free
061070130315         // -?Posizionamento?
061080130315         clear  k06fiar801;
061090130315         k_AR8aas = DB0aas;
061100130315         k_AR8lnp = DB0lnp;
061110130315         k_AR8nrs = DB0nrs;
061120130315         k_AR8nsp = DB0nsp;
061130130315         k_AR8tmd = c_§PDF + '999';
061140130315         setgt  %kds( k06fiar801 : 5 )  FIAR8000;
061150130315      /end-free
061160130315      *
061170130315      * - Letura
061180040607do  1c                   dou          %eof(FIAR801L)
061190040607     c                             or AR8atb = *blanks
061200130308     c                             or AR8tmd <> c_§PDF
061210040607     c     KLDV          reade     FIAR8000
061220040607e   1c                   enddo
061230040607if  1c                   if        %eof(FIAR801L)
061240040607     c                   goto      NOprtDDT
061250040607e   1c                   endif
061260041207if  1c                   if        §AR8des   = *blanks
061270041207     c*** (già fatto:)   reset                   dAR8
061280041207     c                   reset                   TIBS02ds
061290041207     c                   eval      T02cod    = 'AR8'
061300041207     c                   movel(p)  AR8tmd        T02ke1
061310041207     c                   call      'TIBS02R'
061320041207     c                   parm                    KPJBA
061330041207     c                   parm                    TIBS02ds
061340041207if  2c                   if        T02err    = *blanks
061350041207     c                   movel     T02uni        dAR8
061360041207e   2c                   endif
061370041207e   1c                   endif
061380040607      *
061390040607sel 1c                   select
061400050217      * - Karnak o Titanedi o EDk (identiche)
061410080730w   1c                   when      §AR8tsd   = 'K1'
061420040607     c                   exsr      DDT_Karnak
061430050414      * - Logistica o Sait-Abrasivi
061440060627w   1c                   when      %subst(§AR8tsd:1:1) = 'L'
061450050218     c                   exsr      DDT_Logistica
061460050217      * - Arcte
061470050215w   1c                   when      AR8tmd    = 'ARCT'
061480050217     c                   exsr      DDT_Arcte
061490050214      * - Esprinet
061500060627w   1c                   when      §AR8tsd   = 'E1'
061510050214     c                   exsr      DDT_Esprinet
061520041207      * - dati generici (con logo)
061530041207w   1c                   when      §AR8tsd   = 'S2'
061540041207     c                   exsr      DDT_genericoL
061550041207      * - dati generici (senza logo)
061560041207w   1c***                when      §AR8tsd   = 'S1'
061570040607     c                   other
061580040607     c                   exsr      DDT_generico
061590040607e   1c                   endsl
061600040607      *
061610060921     c                   reset                   $FIAR8
061620040607     c                   reset                   $DDT_o
061630040607     c                   reset                   $DDT_p
061640040608     c                   reset                   $DDTlastPg
061650040608     c                   clear                   wOmaRowT
061660040608     c                   clear                   wOmaRowD
061670040607     c                   clear                   $AR8o1
061680040607      *
061690040607     c     NOprtDDT      ENDSR
061700040607      *
061710040607      *---------------------------------------------------------------*
061720050217      *?Stampa DDT del mittente KARNAK / TITANEDI                    ?*
061730040607      *---------------------------------------------------------------*
061740040607     c     DDT_Karnak    BEGSR
061750040607      *
061760080923      /free
061770080923         // Posizionamento iniziale all'interno del foglio A4
061780080923         write  A4prtSP1;
061790080923
061800080923         // Impostazione del nuovo limite x overflow
061810080923         select;
061820080923           when  NOT *in45;
061830080923             wMaxLine = $MaxLine3;
061840080923           when  *in45   and   NOT *in46;
061850080923             wMaxLine = $MaxLine3_pl1;
061860080923           when  *in45   and   *in46;
061870080923             wMaxLine = $MaxLine3_pl2;
061880080923         endsl;
061890080923      /end-free
061900040607      *
061910040607      * Impostazione testi fissi - - - - - - - - - - - - - - - - - - -*
061920040607      *
061930040607     c                   eval      PCtx1 = 'ATTENZIONE: VERIFICARE LA -
061940040607     c                             QUANTITA'' E L''INTEGRITA'' DEI CAR-
061950040607     c                             TONI. IN CASO DI DISCORDANZA RITIRA-
061960040607     c                             RE CON RISERVA.'
061970130118     c*//                eval      PCtx2 = 'La merce viagga a rischio -
061980130118     c*//                          e pericolo del committente anche se-
061990130118     c*//                           venduta franco destino. Trascorsi -
062000130118     c*//                          otto giorni dal ricevimento della m-
062010130118     c*//                          erce non si accettano reclami.   - -
062020130118     c*//                            IN CASO DI RITARDATO PAGAMENTO DE-
062030130118     c*//                          CORRERANNO GLI INTERESSI BANCARI IN-
062040130118     c*//                           VIGORE.'
062050130118     c*//                eval      PCtx3 = 'Per ogni vertenza l''acqui-
062060130118     c*//                          rente accetta la competenza della R-
062070130118     c*//                          epubblica di San Marino o in altern-
062080130118     c*//                          ativa del foro di Rimini.'
062090130118     c*//                eval      PCtx4 = 'NON SI ACCETTANO RESI E NO-
062100130118     c*//                          N SI EFFETTUANO SOSTITUZIONI SE NON-
062110130118     c*//                           AUTORIZZATI PER ISCRITTO DALLA NOS-
062120130118     c*//                          TRA DIREZIONE COMMERCIALE.'
062130130118     c*//                eval      PCtx5 = 'Quand''anche autorizzati, -
062140130118     c*//                          i resi verranno accettati e le sost-
062150130118     c*//                          ituzioni verranno effettuate solo d-
062160130118     c*//                          opo aver accertato il perfetto stat-
062170130118     c*//                          o nel nostro stabilimento della mer-
062180130118     c*//                          ce restituita.'
062190130118     c                   eval      PCtx2 = 'Eventuali reclami per mate-
062200130118     c                             riale mancante o danneggiato all''i-
062210130118     c                             nterno dei colli dovranno pervenire-
062220130118     c                              entro 8 (otto) giorni di calendari-
062230130118     c                             o dall''avvenuta consegna a MyO S.r-
062240130118     c                             .l. - Ufficio Resi - Via Santarcang-
062250130118     c                             iolese, 6 - 47825 Torriana (RN)'
062260130118     c                   eval      PCtx4 = 'Eventuali mancanze di coll-
062270130118     c                             i e/o avarie sono da contestare all-
062280130118     c                             ''atto della consegna con annotazio-
062290130118     c                             ne su lettera di vettura e DDT'
062300040709if  1c                   if        AR8tmd = 'TITA'
062310040709     c                   eval      PCtx6 = 'Depositario: K & K Logisti-
062320040709     c                             cs S.p.a. - via Santarcangiolese, 6-
062330040709     c                              - 47825 Torriana (RN) - P.I. 00953-
062340040709     c                             860400'
062350040709e   1c                   endif
062360080822      *
062370040607      * Conteggio delle righe "omaggio" - - - - - - - - - - - - - - -*
062380040607      *
062390040614     c                   clear                   xx
062400040607     c                   eval      SAVprr = AR8prr
062410040607     c     KLDV          setgt     FIAR8000
062420040607     c     KLDV          readpe    FIAR8000
062430040607do  1c                   dow       NOT %eof(FIAR801L)
062440040607sel 2c                   select
062450040607w   2c                   when      AR8atb <> *blanks
062460040611w   2c                   when      AR8trk =  'O2'
062470040608     c                   add       1             wOmaRowD
062480040611w   2c                   when      AR8trk =  'O1'
062490040608     c                   add       1             wOmaRowT
062500051110     c                   movel(p)  AR8uni        dAR8O1
062510040607     c                   add       1             xx
062520040607     c                   eval      $AR8o1(xx) =  §AR8O1a
062530040607x   2c                   other
062540040607     c                   leave
062550040607e   2c                   endsl
062560040607     c     KLDV          readpe    FIAR8000
062570040607e   1c                   enddo
062580040607      * - mi riposiziono
062590040607     c                   eval      AR8prr = SAVprr
062600040607     c     K06AR801      setll     FIAR8000
062610040607     c     K06AR801      reade     FIAR8000
062620040607      *
062630040607      * Reperimento dati, impostazione e stampa DDT mittente  - - - -*
062640040607      *
062650040607if  1c                   dow       NOT %eof(FIAR801L)
062660040607      *
062670040607sel 2c                   select
062680040607      * - Record annullato (non si stampa nulla)
062690040607w   2c                   when      AR8atb <> *blanks
062700040607      * - Dati di dettaglio: articolo, descrizione, quantità
062710040607w   2c                   when      AR8trk = 'C1'
062720050217     c                   exsr      PrtDdtKARN_C1
062730040607      * - Dati di dettaglio: testo per articoli non disponibili
062740040607w   2c                   when      AR8trk = 'C2'
062750050217     c                   exsr      PrtDdtKARN_C2
062760040607      * - Dati di dettaglio: descrizioni generiche (orario apertura)
062770040607w   2c                   when      AR8trk = 'C3'
062780050217     c                   exsr      PrtDdtKARN_C3
062790040607      * - Dati di testata: destinatario, cliente di fatturazione
062800040607w   2c                   when      AR8trk = 'T1'
062810050217     c                   exsr      PrtDdtKARN_T1
062820040607      * - Dati di testata: agente, numero ordine, estremi documento
062830040607w   2c                   when      AR8trk = 'T2'
062840050217     c                   exsr      PrtDdtKARN_T2
062850040607      * - Dati di fondo: mezzo e causale trasporto, colli, peso
062860040607w   2c                   when      AR8trk = 'V1'
062870050217     c                   exsr      PrtDdtKARN_V1
062880040607      * - Dati di fondo: vettore, annotazioni e note
062890040607w   2c                   when      AR8trk = 'V2'
062900050217     c                   exsr      PrtDdtKARN_V2
062910040607      * - Dati esterni: riga descrittiva con referente
062920040607w   2c                   when      AR8trk = 'L1'
062930050217     c                   exsr      PrtDdtKARN_L1
062940040607      * - Dati di omaggio: testo per articoli omaggiati
062950040607w   2c                   when      AR8trk = 'O1'
062960050217     c                   exsr      PrtDdtKARN_O1
062970040607      * - Dati di omaggio: articolo, descrizione, quantità
062980040607w   2c                   when      AR8trk = 'O2'
062990050217     c                   exsr      PrtDdtKARN_O2
063000040607e   2c                   endsl
063010040607      *
063020040607     c     KLDV          reade     FIAR8000
063030040607      *
063040040607e   1c                   enddo
063050040607      *
063060040607      * Reimpostazione del vecchio limite x overflow
063070040607     c                   eval      wMaxLine = $MaxLine2
063080040607      *
063090040607     c                   ENDSR
063100050214      *
063110040607      *---------------------------------------------------------------*
063120050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
063130050217      *?Dati di testata: destinatario, cliente di fatturazione       ?*
063140040607      *---------------------------------------------------------------*
063150050217     c     PrtDdtKARN_T1 BEGSR
063160040607      *
063170040607      * > logo
063180040607     c                   eval      §Logo  = 'LOGO' + AR8tmd
063190040607      *
063200051110     c                   movel(p)  AR8uni        dAR8T1
063210040607      *
063220040607      * > destinatario
063230040607     c                   eval      PCrsd  = §AR8t1a
063240040607     c                   eval      PCind  = §AR8t1b
063250040616if  1c                   if        §AR8t1c  > *zeros
063260040607     c                   eval      PCcad  = §AR8t1c
063270040616e   1c                   endif
063280040607     c                   eval      PClod  = §AR8t1d
063290040607     c                   eval      PCprd  = §AR8t1e
063300040607     c***                eval      PCnzd  = *blanks
063310040607      * > cliente di fatturazione
063320040607     c                   eval      PCrsf  = §AR8t1f
063330040607     c                   eval      PCinf  = §AR8t1g
063340040616if  1c                   if        §AR8t1h  > *zeros
063350040607     c                   eval      PCcaf  = §AR8t1h
063360040616e   1c                   endif
063370040607     c                   eval      PClof  = §AR8t1i
063380040607     c                   eval      PCprf  = §AR8t1j
063390040607     c***                eval      PCnzf  = *blanks
063400040607      *
063410040607     c                   ENDSR
063420040607      *
063430040607      *---------------------------------------------------------------*
063440050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
063450050217      *?Dati di testata: agente, ..., porto, ...                     ?*
063460040607      *---------------------------------------------------------------*
063470050217     c     PrtDdtKARN_T2 BEGSR
063480040607      *
063490051110     c                   movel(p)  AR8uni        dAR8T2
063500040607      *
063510040607      * > dati in testata DDT
063520040607      * - - codice agente
063530130122     c*//                eval      PCage  = §AR8t2a
063540040607      * - - numero e data ordine
063550040607     c                   eval      PCnro  = §AR8t2b
063560040607     c                   eval      PCdto  = §AR8t2c
063570040607      * - - riferimenti
063580040607     c                   eval      PCrif  = §AR8t2d
063590040607      * - - codice destinatario (dal mittente)
063600040611     c                   eval      PCcli  = §AR8t2e
063610040607      * - - numero e data documento (DDT)
063620040607     c                   eval      PCnrd  = §AR8t2f
063630040607     c                   eval      PCdtd  = §AR8t2g
063640040607      * - - numero pagina documento originale (qui ricalcolato)
063650040607     c***                eval      Page   = §AR8t2h
063660040607      * - - porto
063670040607     c                   eval      PCpto  = §AR8t2i
063680040607      * - - condizioni di pagamento
063690040611     c                   eval      PCpag  = §AR8t2j
063700040607     c                                    + ' '
063710040607     c                                    + %trim(§AR8t2k)
063720040607      * - - tipologia spedizione
063730040607     c                   eval      PCtsp  = §AR8t2l
063740040607      *
063750040607      * > Stampa testata DDT - - - - - - - - - - - - - - - - - - - - -*
063760040607      *
063770040607      *   - OverFlow impossibile qui! -
063780040607     c                   write     A4prtDK1
063790040607      *
063800040607     c                   ENDSR
063810040607      *
063820040607      *---------------------------------------------------------------*
063830050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
063840050217      *?Dati di dettaglio: articolo, descrizione, quantità           ?*
063850040607      *---------------------------------------------------------------*
063860050217     c     PrtDdtKARN_C1 BEGSR
063870040607      *
063880040607     c                   clear                   A4prtDK2
063890051110     c                   movel(p)  AR8uni        dAR8C1
063900040607      *
063910040607      * > dati in dettaglio DDT
063920040607      * - - codice prodotto
063930040607     c                   eval      PCcpr  = §AR8c1a
063940040607      * - - descrizione prodotto
063950040607     c                   eval      PCdpr  = §AR8c1b
063960040607      * - - quantità
063970040607     c                   eval      PCqta  = '     ' + §AR8c1c
063980040607      * - - note
063990040607     c***                eval      PCnot  = *blanks
064000040607      *
064010040607      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
064020040607      *
064030040607     c                   exsr      Test_OF
064040040607     c                   write     A4prtDK2
064050040607      *
064060040607     c                   ENDSR
064070040607      *
064080040607      *---------------------------------------------------------------*
064090050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
064100050217      *?Dati di dettaglio: testo per articoli non disponibili        ?*
064110040607      *---------------------------------------------------------------*
064120050217     c     PrtDdtKARN_C2 BEGSR
064130040607      *
064140040607     c                   clear                   A4prtDK3
064150051110     c                   movel(p)  AR8uni        dAR8C2
064160040607      *
064170040607      * > dati in note DDT
064180040607      * - - riga descrittiva
064190040607     c                   eval      PCtxt  = §AR8c2a
064200040607      *
064210040607      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
064220040607      *
064230040607     c                   exsr      Test_OF
064240040607     c                   write     A4prtDK3
064250040607      *
064260040607     c                   ENDSR
064270040607      *
064280040607      *---------------------------------------------------------------*
064290050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
064300050217      *?Dati di dettaglio: descrizioni generiche (orario apertura)   ?*
064310040607      *---------------------------------------------------------------*
064320050217     c     PrtDdtKARN_C3 BEGSR
064330040607      *
064340040607     c                   clear                   A4prtDK2
064350051110     c                   movel(p)  AR8uni        dAR8C3
064360040607      *
064370040607      * > dati in note DDT
064380040607      * - - riga descrittiva
064390040607     c                   eval      PCdpr  = §AR8c3a
064400040607      *
064410040607      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
064420040607      *
064430040607     c                   exsr      Test_OF
064440040607     c                   write     A4prtDK2
064450040607      *
064460040607     c                   ENDSR
064470040607      *
064480040607      *---------------------------------------------------------------*
064490050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
064500050217      *?Dati di fondo: mezzo e causale trasporto, colli, peso        ?*
064510040607      *---------------------------------------------------------------*
064520050217     c     PrtDdtKARN_V1 BEGSR
064530040607      *
064540051110     c                   movel(p)  AR8uni        dAR8V1
064550040607      *
064560040607      * Controllo se ci saranno da stampare tante righe omaggio da
064570040607      *   chiudere in pagina successiva:
064580040608      *   i campi del box finale del DDT (in totale 9 righe) verranno
064590040607      *   riempiti solo nella ultima pagina.
064600040607      *
064610080924      /free
064620080924         select;
064630080924           when  NOT *in45;
064640080924      /end-free
064650040608     c                   eval      $DDTlastPg = ($MaxLine2
064660040713     c                                        +  1
064670040608     c                                        -  CurLineA4
064680040608     c                                        -  $BoxRow1
064690040608     c                                        -  wOmaRowT
064700040608     c                                        >= wOmaRowD)
064710080924      /free
064720080924           when  *in45   and   NOT *in46;
064730110406             $DDTlastPg = ($MaxLine2_pl1k
064740080924                           + 1 - CurLineA4 - $BoxRow1 - wOmaRowT
064750080924                           >= wOmaRowD);
064760080924           when  *in45   and   *in46;
064770110406             $DDTlastPg = ($MaxLine2_pl2k
064780080924                           + 1 - CurLineA4 - $BoxRow1 - wOmaRowT
064790080924                           >= wOmaRowD);
064800080924         endsl;
064810080924      /end-free
064820040607      *
064830040607      * > dati in chiusura DDT
064840040607      *   Per ora imposto (ho già impostato) solo la DS "dAR8V1".
064850040607      *   I corrispondenti campi di stampa li impostero nella subr.
064860050217      *     "PrtDdtKARN_V2".
064870040607      *
064880040607     c                   ENDSR
064890040607      *
064900040607      *---------------------------------------------------------------*
064910050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
064920050217      *?Dati di fondo: vettore, annotazioni e note                   ?*
064930040607      *---------------------------------------------------------------*
064940050217     c     PrtDdtKARN_V2 BEGSR
064950040607      *
064960051110     c                   movel(p)  AR8uni        dAR8V2
064970040618      *
064980040618      * > dati in chiusura DDT
064990040618      *
065000040607      * Ho già controllato se ci saranno da stampare tante righe
065010040607      *   omaggio da chiudere in pagina successiva
065020040608if  1c                   if        $DDTlastPg = *off
065030040607     c                   eval      PCnv2  = '          '
065040040607     c                                    + '*** SEGUE ***'
065050040618x   1c                   else
065060050217     c                   exsr      PrtDdtKARNaV2
065070040607e   1c                   endif
065080040607      *
065090040607      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
065100040608      *
065110050217     c                   exsr      PrtDdtKARNbV2
065120040607      *
065130040607      * > Impostazione nuovo limite di OverFlow  - - - - - - - - - - -*
065140080923      /free
065150080923         select;
065160080923           when  NOT *in45;
065170080923             wMaxLine = $MaxLine2;
065180080923           when  *in45   and   NOT *in46;
065190080923             wMaxLine = $MaxLine2_pl1;
065200080923           when  *in45   and   *in46;
065210080923             wMaxLine = $MaxLine2_pl2;
065220080923         endsl;
065230080923      /end-free
065240040607      *
065250040608      * N.B.: la copia per il destinatario della LDV deve comparire
065260040608      *       nell'ultima pagina del DDT per il cliente (vedi subr.
065270040608      *       "ChiusuraLDV")
065280040607      *
065290040607     c                   ENDSR
065300040618      *
065310040618      *---------------------------------------------------------------*
065320040618      *
065330050217     c     PrtDdtKARNaV2 BEGSR
065340040618      *
065350040618      * > dati in chiusura DDT
065360040618      *
065370040618      * > > da dAR8V1
065380040618      * - - mezzo di trasporto
065390040618     c                   movel     §AR8v1a       PCmtr
065400040618      * - - causale trasporto
065410040618     c                   eval      PCctr  = §AR8v1b
065420040618      * - - numero colli
065430040618     c                   eval      PCncl  = §AR8v1c
065440040618      * - - peso in kg
065450040618     c                   eval      PCpkg  = §AR8v1d
065460040618      * - - aspetto esteriore dei beni
065470040618     c                   eval      PCaeb  = §AR8v1e
065480040618      *
065490040618      * > > da dAR8V2
065500120530      * - - dati vettore: · ragione sociale (BRT S.p.A.)
065510040618      *                   · indirizzo
065520040618      *                   · c.a.p./città/provincia
065530040618      *     => NON con i dati che manda il mittente !!!
065540040618      *        ...Vuoi mai che si sbagli ad inviarci della roba !!!
065550040618     c***                eval      PCvet  = %trim(§AR8v2a)
065560040618     c***                                 + ' - '
065570040618     c***                                 + %trim(§AR8v2b)
065580040618     c***                                 + ' - '
065590040618     c***                                 + %trim(§AR8v2c)
065600120530     c*//                if        (DB0dft > *zero       and
065610120530     c*//                           DB0dft > §vpoBRTdta)  OR
065620120530     c*//                          (DB0dft = *zero       and
065630120530     c*//                           Wdsp   > §vpoBRTdta)
065640110426     c                   eval      PCvet  = $BRT
065650110426     c                                    + ' - '
065660110426     c                                    + $Intest2
065670110426     c                                    + ' (BO)'
065680120530     c*//                else
065690120530     c*//                eval      PCvet  = $Bartolini
065700120530     c*//                                 + ' - '
065710120530     c*//                                 + $Intest2
065720120530     c*//                                 + ' (BO)'
065730120530     c*//                endif
065740040618      * - - data ritiro
065750040618     c*** non stampo:    eval      PCdtv  = §AR8v2d
065760040618      * - - orario ritiro
065770040618     c*** non stampo:    eval      PChmv  = §AR8v2e
065780040618      * - - annotazioni / variazioni
065790040618     c                   eval      PCvea  = §AR8v2f
065800040618      * - - note
065810040618     c                   eval      PCnv1  = §AR8v2g
065820040618     c                   eval      PCnv2  = §AR8v2h
065830040618      *
065840040618     c                   ENDSR
065850040618      *
065860040618      *---------------------------------------------------------------*
065870040618      *
065880050217     c     PrtDdtKARNbV2 BEGSR
065890040618      *
065900040618      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
065910040618      *
065920040618      * - - Calcolo la posizione finale del box
065930040618     c                   exsr      Posiz_DK
065940040618      *
065950040618      * - - Stampa parte finale DDT
065960040618     c                   write     A4prtDK4
065970040618      *
065980040618      * > Stampa logo, box, linee e testi fissi  - - - - - - - - - - -*
065990081017     c                   if            §AR8cxd = *blanks
066000081017     c                             or  DB0fa4  = 'P'
066010050331     c                   write     A4prtDT0
066020050331     c                   endif
066030040618     c                   write     A4prtDK0
066040040618      *
066050040618     c                   ENDSR
066060040607      *
066070040607      *---------------------------------------------------------------*
066080050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
066090050217      *?Dati esterni: riga descrittiva con referente                 ?*
066100040607      *---------------------------------------------------------------*
066110050217     c     PrtDdtKARN_L1 BEGSR
066120040607      *
066130051110     c                   movel(p)  AR8uni        dAR8L1
066140040607      *
066150040607      * > righe descrittive esterne al DDT
066160040607      * - - riga descrittiva
066170040607     c                   eval      PCtxx  = §AR8l1a
066180040607      *
066190040607      * > Stampa riga testo finale DDT - - - - - - - - - - - - - - - -*
066200040618      *   - OverFlow già testato per questa riga -
066210040607     c                   write     A4prtDK5
066220040607      *
066230040607     c                   ENDSR
066240040607      *
066250040607      *---------------------------------------------------------------*
066260050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
066270050217      *?Dati di omaggio: testo per articoli omaggiati                ?*
066280040607      *---------------------------------------------------------------*
066290050217     c     PrtDdtKARN_O1 BEGSR
066300040608      *
066310040608     c                   eval      $DDT_o = *on
066320040607      *
066330040607     c                   clear                   A4prtDK5
066340051110     c                   movel(p)  AR8uni        dAR8O1
066350040607      *
066360040607      * > righe descrittive esterne al DDT
066370040607      * - - riga descrittiva
066380040607     c                   eval      PCtxx  = §AR8o1a
066390040607      * - - nessuna sottolineatura
066400040608     c***                eval      *in39  = *off
066410040607      *
066420040607      * > Stampa testata omaggi in DDT - - - - - - - - - - - - - - - -*
066430040608      *   (se NON già stampata nella nuova pagina x OverFlow)
066440040608     c                   eval      SavLineA4 = CurLineA4
066450040607     c                   exsr      Test_OF
066460040608if  1c                   if        CurLineA4 = SavLineA4
066470040607     c                   write     A4prtDK5
066480040608e   1c                   endif
066490040608      * ...Dovendo ristampare le righe di intestazione degli omaggi
066500040608      *   in ogni pagina (da quella del 1° omaggio in poi), non calcolo
066510040608      *   quelle già stampate...
066520040608     c*** NO:            sub       1             wOmaRowT
066530040607      *
066540040607     c                   ENDSR
066550040607      *
066560040607      *---------------------------------------------------------------*
066570050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
066580050217      *?Dati di omaggio: articolo, descrizione, quantità             ?*
066590040607      *---------------------------------------------------------------*
066600050217     c     PrtDdtKARN_O2 BEGSR
066610040607      *
066620040607     c                   clear                   A4prtDK2
066630051110     c                   movel(p)  AR8uni        dAR8O2
066640040607      *
066650040607      * > dettaglio omaggi DDT
066660040607      * - - codice prodotto
066670040607     c                   eval      PCcpr  = §AR8o2a
066680040607      * - - descrizione prodotto
066690040607     c                   eval      PCdpr  = §AR8o2b
066700040607      * - - quantità
066710040618     c                   eval      PCqta  = '     ' + §AR8o2c
066720040607      * - - note
066730040607     c***                eval      PCnot  = *blanks
066740040607      *
066750040607      * > Stampa dettaglio omaggi in DDT - - - - - - - - - - - - - - -*
066760040607      *
066770040607     c                   exsr      Test_OF
066780040607     c                   write     A4prtDK2
066790040608     c                   sub       1             wOmaRowD
066800040607      *
066810040607     c                   ENDSR
066820040608      *
066830040608      *---------------------------------------------------------------*
066840081002      *?Impostazione nuova pagina di DDT Karnak / Titanedi / EDk     ?*
066850040608      *---------------------------------------------------------------*
066860081002     c     NewPageDK     BEGSR
066870040608      *
066880080923      /free
066890080923         // Impostazione del nuovo limite x overflow
066900080923         select;
066910080923           when  NOT *in45;
066920080923             wMaxLine = $MaxLine3;
066930080923           when  *in45   and   NOT *in46;
066940080923             wMaxLine = $MaxLine3_pl1;
066950080923           when  *in45   and   *in46;
066960080923             wMaxLine = $MaxLine3_pl2;
066970080923         endsl;
066980080923      /end-free
066990080923     c                   write     A4prtSP1
067000040608     c                   write     A4prtDK1
067010040608      *
067020040608if  1c                   if        $DDT_o = *on
067030040608      *
067040040608      * - Stampa 1 riga di dettaglio vuota
067050040608     c                   clear                   A4prtDK3
067060040608     c                   write     A4prtDK3
067070040618      * - Reimposta i dati nel box finale del DDT
067080050217     c                   exsr      PrtDdtKARNaV2
067090040618      * - Stampa fondo DDT
067100050217     c                   exsr      PrtDdtKARNbV2
067110040608      * - Stampa ultima riga
067120040608      *   ("Per qualsiasi problema o chiarimento contattare...)
067130040608     c                   clear                   A4prtDK5
067140040608     c                   eval      PCtxx  = §AR8l1a
067150040608     c                   write     A4prtDK5
067160040608      * - Stampa intestazione per merce omaggio
067170040608      *   xx = numero di righe per "intestazione omaggi"
067180040608     c                   eval      wx     = xx
067190040608do  2c                   dow           xx         >  *zeros
067200040608     c                             and xx         <= %elem($AR8o1)
067210040608     c                             and $AR8o1(xx) <> *blanks
067220040608     c                   eval      PCtxx  = $AR8o1(xx)
067230040608     c                   write     A4prtDK5
067240040608     c                   eval      xx     = xx - 1
067250040608e   2c                   enddo
067260040608     c                   eval      xx     = wx
067270040608      * -Se sta leggendo il tipo record "O1":?
067280040608      *   sposta il ciclo di lettura del file FIAR8 sul record
067290040608      *   successivo all'ultimo (appena stampato) dell'intestazione
067300040608      *   della merce omaggiata
067310040608if  2c                   if        AR8trk =  'O1'
067320040608if  3c                   dou       AR8trk <> 'O1'
067330040608     c     KLDV          reade     FIAR8000
067340040608e   3c                   enddo
067350040608if  3c*** è così:        if        AR8trk = 'O2'
067360040608     c     KLDV          readpe    FIAR8000
067370040707e   3c***                endif
067380040608e   2c                   endif
067390040608      * - Aggiorna controllo se "ultima pagina"
067400080924      /free
067410080924           select;
067420080924             when  NOT *in45;
067430080924      /end-free
067440040608     c                   eval      $DDTlastPg = ($MaxLine2
067450040713     c                                        +  1
067460040608     c                                        -  CurLineA4
067470040608     c                                        -  $BoxRow1
067480040608     c                                        -  wOmaRowT
067490040608     c                                        >= wOmaRowD)
067500080924      /free
067510080924             when  *in45   and   NOT *in46;
067520110406               $DDTlastPg = ($MaxLine2_pl1k
067530080924                             + 1 - CurLineA4 - $BoxRow1 - wOmaRowT
067540080924                             >= wOmaRowD);
067550080924             when  *in45   and   *in46;
067560110406               $DDTlastPg = ($MaxLine2_pl2k
067570080924                             + 1 - CurLineA4 - $BoxRow1 - wOmaRowT
067580080924                             >= wOmaRowD);
067590080924           endsl;
067600080924      /end-free
067610040608      *
067620040608e   1c                   endif
067630040608      *
067640040608     c                   ENDSR
067650040607      *
067660040607      *---------------------------------------------------------------*
067670050414      *?Posizionamento di linee e box nel DDT KARNAK / TITANEDI / EDk?*
067680040607      *---------------------------------------------------------------*
067690040607     c     Posiz_DK      BEGSR
067700040607      *
067710080918      /free
067720080923         // Impostazione posizioni di logo/box
067730080923         // (posizioni di "partenza" diverse a seconda del posizionam.
067740080923         //  iniziale nel foglio A4 attualmente in stampa)
067750080923         select;
067760080923           when  NOT *in45;
067770081002             §PB1 =  3.120;
067780080923           when  *in45   and   NOT *in46;
067790081002             §PB1 = 0.250;
067800080923           when  *in45   and   *in46;
067810081002             §PB1 = 6.000;
067820080923         endsl;
067830080918      /end-free
067840040607     c                   eval      §PB2 =  §PB1 + 1.23
067850080822     c                   eval      §PB3 =  §PB2 + 0.18
067860040607     c                   eval(h)   §PB4 =  CurLineA4 / 8 + 0.20
067870040607     c                   eval      §PB5 =  §PB4 + 0.18
067880040607     c                   eval      §PB6 =  §PB5 + 0.77
067890040607     c                   eval      §PB7 =  §PB5 + 0.165
067900040607     c                   eval      §PB8 =  §PB7 + 0.08
067910040607      * Impostazione posizioni di linee orizzontali nel 1° box
067920040607     c                   eval(h)   §LO1 =  §PB1 + (§PB2 - §PB1) / 2
067930040607      * Impostazione posizioni di linee orizzontali nel 2° box
067940080822     c                   eval      §LO2 =  §PB3 + 0.23
067950080822     c                   eval      §LO3 =  §LO2 + 0.145
067960080822     c                   eval      §LO4 =  §LO3 + 0.10
067970040607     c                   eval      §LO5 =  §PB4 - 0.15
067980040607      * Impostazione posizioni di linee orizzontali nel 3° box
067990040607     c                   eval      §LO6 =  §PB5 + 0.14
068000040607     c                   eval      §LO7 =  §LO6 + 0.12
068010040607     c                   eval      §LO8 =  §LO7 + 0.12
068020040607     c                   eval      §LO9 =  §LO8 + 0.13
068030040607      * Impostazione posizioni di linee verticali nel 2° box
068040040607     c                   eval      §LV1 =  §LO2 - §PB3
068050040607     c                   eval      §LV2 =  §LO3 - §LO2
068060040607     c                   eval      §LV3 =  §LO5 - §LO3
068070040607      * Impostazione posizioni di linee verticali nel 3° box
068080040607     c                   eval      §LV4 =  §LO6 - §PB5
068090040607     c                   eval      §LV5 =  §LO7 - §LO6
068100040607     c                   eval      §LV6 =  §LO8 - §LO7
068110040607     c                   eval      §LV7 =  §PB6 - §LO9
068120040607      * Impostazione posizioni di campi
068130040607     c                   eval      §PS1 =  §LO5 + 0.11
068140040607     c                   eval      §PS2 =  §PB4 + 0.08
068150040607     c                   eval      §PS3 =  §PS2 + 0.07
068160130118if  1c*//                if        §PB6 <  §PB1 + 4.58
068170130118     c*//                eval      §PR1 =  §PB1 + 4.58
068180130118if  1c                   if        §PB6 <  §PB1 + 3.30
068190130118     c                   eval      §PR1 =  §PB1 + 3.30
068200040607x   1c                   else
068210040607     c                   eval      §PR1 =  §PB6
068220040607e   1c                   endif
068230040607      *
068240040607     c                   ENDSR
068250041019      *
068260041019      *---------------------------------------------------------------*
068270050414      *?Stampa DDT del mittente LOGISTICA / SAIT ABRASIVI            ?*
068280041019      *---------------------------------------------------------------*
068290050218     c     DDT_Logistica BEGSR
068300041019      *
068310080924      /free
068320080924         // Impostazione del nuovo limite x overflow
068330080924         // (Codesto limite è valido solo per la stampa del dettaglio.
068340080924         //   Per la stampa del "fondo" bolla occorre REimpostarlo ad
068350080924         //   un valore più basso.
068360080924         //  Ovvero: stampo più righe di dettaglio che posso; se alla
068370080924         //  fine delle righe di dettaglio non rimane spazio a suffi-
068380080924         //  cienza per il fondo: prima della stampa del fondo si salta
068390080924         //  alla pagina successiva, dove non si stampa alcuna riga di
068400080924         //  dettaglio.)
068410080924         select;
068420080924           when  NOT *in45;
068430090323             wMaxLine = $MaxLine2;
068440080924           when  *in45   and   NOT *in46;
068450090323             wMaxLine = $MaxLine2_pl1;
068460080924           when  *in45   and   *in46;
068470090323             wMaxLine = $MaxLine2_pl2;
068480080924         endsl;
068490080924
068500080924         // Posizionamento iniziale all'interno del foglio A4
068510080924         write  A4prtSP1;
068520081006
068530081006         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
068540081006           write  A4prtNUL;
068550081006         endif;
068560080924      /end-free
068570041019      *
068580041019      * Impostazione testi fissi - - - - - - - - - - - - - - - - - - -*
068590041019      *
068600071008      * - prima solo per "Logistica", ora neanche più per lei...
068610071008      * - Sait Abrasivi SpA & Brumar Srl hanno Tipo Stampa DDT = "LC"
068620071008if  1c***                if        §AR8tsd <> 'LC'
068630071008     c***                eval      PCLtx1 = 'La merce viagga a rischio -
068640071008     c***                          e pericolo del Committente, pertanto -
068650071008     c***                          la Venditrice non risponde di -
068660071008     c***                          eventuali perdite o avarie derivanti -
068670071008     c***                          dal trasporto.'
068680071008     c***                eval      PCLtx2 = 'Il Destinatario, per -
068690071008     c***                          ottenere il risarcimento del danno, -
068700071008     c***                          deve far constatare al Corriere la -
068710071008     c***                          perdita o l''avaria all''atto del -
068720071008     c***                          ricevimento della merce.'
068730071008e   1c***                endif
068740050218      *
068750130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
068760130315      *  "StampaDDT")
068770050218     c***  KLDV          setll     FIAR8000       (già fatto)
068780050218     c***  KLDV          reade     FIAR8000       (già fatto)
068790041019      *
068800041019      * Reperimento dati, impostazione e stampa DDT mittente  - - - -*
068810041019      *
068820041019if  1c                   dow       NOT %eof(FIAR801L)
068830050318      *
068840050318if  2c                   if        AR8atb   = *blanks
068850050318     c                   eval      SAVtrk   = AR8trk
068860050318e   2c                   endif
068870041019      *
068880041019sel 2c                   select
068890041019      * - Record annullato (non si stampa nulla)
068900050218w   2c                   when      AR8atb  <> *blanks
068910041019      * - Dati di dettaglio: articolo, descrizione, quantità
068920050218w   2c                   when      AR8trk   = 'C1'
068930050217     c                   exsr      PrtDdtLOG1_C1
068940041103      * - Dati di dettaglio: descrizioni generiche
068950050218w   2c                   when      AR8trk   = 'C2'
068960050217     c                   exsr      PrtDdtLOG1_C2
068970041103      * - Dati di dettaglio: descrizioni generiche
068980050218w   2c                   when      AR8trk   = 'C3'
068990050217     c                   exsr      PrtDdtLOG1_C3
069000060614      * - Dati di dettaglio: corpo bolla non fincato
069010060614w   2c                   when      AR8trk   = 'C4'
069020060614     c                   exsr      PrtDdtLOG1_C4
069030090323      * - Dati di dettaglio: descrizioni generiche
069040090323w   2c                   when      AR8trk   = 'C5'
069050090323     c                   exsr      PrtDdtLOG1_C5
069060050318      * - Dati di packing-list: dettaglio
069070050318w   2c                   when      AR8trk   = 'Z1'
069080050318     c                   exsr      PrtDdtLOG1_Z1
069090041019      * - Dati di testata: sito e depositante
069100050218w   2c                   when      AR8trk   = 'T1'
069110050217     c                   exsr      PrtDdtLOG1_T1
069120041019      * - Dati di testata: destinatario, cliente di fatturazione
069130050218w   2c                   when      AR8trk   = 'T2'
069140050217     c                   exsr      PrtDdtLOG1_T2
069150041019      * - Dati di testata: agente, numero ordine, estremi documento
069160050218w   2c                   when      AR8trk   = 'T3'
069170050217     c                   exsr      PrtDdtLOG1_T3
069180041019      * - Dati di fondo: mezzo e causale trasporto, colli, peso
069190050218w   2c                   when      AR8trk   = 'P1'
069200050217     c                   exsr      PrtDdtLOG1_P1
069210050606      * - Dati di testata: FATTURA
069220050606w   2c                   when      AR8trk   = 'S1'
069230050606     c                   exsr      PrtDdtLOG1_S1
069240050606      * - Dati di dettaglio cpi 15: FATTURA
069250050606w   2c                   when      AR8trk   = 'S2'
069260050606     c                   exsr      PrtDdtLOG1_S2
069270050606      * - Dati di dettaglio cpi 20: FATTURA
069280050606w   2c                   when      AR8trk   = 'S3'
069290050606     c                   exsr      PrtDdtLOG1_S3
069300050606      * - Dati di fondo: FATTURA
069310050606w   2c                   when      AR8trk   = 'S4'
069320050606     c                   exsr      PrtDdtLOG1_S4
069330050606      * - Dati di fondo note: FATTURA
069340050606w   2c                   when      AR8trk   = 'S5'
069350050606     c                   exsr      PrtDdtLOG1_S5
069360041019e   2c                   endsl
069370041019      *
069380041019     c     KLDV          reade     FIAR8000
069390041019      *
069400041019e   1c                   enddo
069410050318      *
069420050318      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
069430050318      *
069440050505      * - se DDT con Packing-List:
069450050505      *   (altrimenti il box è già stato stampato in sr PrtDdtLOG1_P1)
069460050505if  1c                   if        %subst(SAVtrk:1:1) =  'Z'
069470050318      *
069480050318      * - - Calcolo la posizione finale del box e di linee separatrici
069490050318     c                   exsr      Posiz_DL
069500050318      *
069510050318      * > Stampa box, linee e testi fissi (no logo)  - - - - - - - - -*
069520081017     c                   if            §AR8cxd = *blanks
069530081017     c                             or  DB0fa4  = 'P'
069540050331     c                   write     A4prtDT0
069550050331     c                   endif
069560050318     c                   write     A4prtDL0C
069570050505      *
069580050505e   1c                   endif
069590050218      *
069600050218      * Reimpostazione del vecchio limite x overflow
069610080925      /free
069620080925         select;
069630080925           when  NOT *in45;
069640090323             wMaxLine = $MaxLine2;
069650080925           when  *in45   and   NOT *in46;
069660090323             wMaxLine = $MaxLine2_pl1;
069670080925           when  *in45   and   *in46;
069680090323             wMaxLine = $MaxLine2_pl2;
069690080925         endsl;
069700080925      /end-free
069710041019      *
069720041019     c                   ENDSR
069730081222
069740081222      /free
069750081222       //--------------------------------------------------------------
069760140303       //?Preparazione simil-logo Bartolini                            ?
069770081222       //--------------------------------------------------------------
069780090406       BEGSR  PrtDdtLOG1_SimilLogo;
069790081222
069800120530         //If  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
069810120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
069820120530         //
069830120530         //  // -?Preparazione simil-Logo BARTOLINI?
069840120530         //  §PLBLS  = §PBA + 0.150;
069850120530         //  §PLBLVS = 0.400;
069860120530         //  exsr  sr_SimilLogoBAR;
069870120530         //
069880120530         //  // -?Preparazione simil-Logo BRT?
069890120530         //  §P2blS  = §PlblS;
069900120530         //  //§P2blVS = §PlblVD + 0.050;
069910120530         //  §P2blVS = §PlblVD + 0.030;
069920120530         //  exsr  sr_SimilLogoBRT;
069930120530         //
069940120530         //Else;
069950120530         //
069960120530         //  // -?Preparazione simil-Logo BARTOLINI?
069970120530         //  §PLBLS  = §PBA + 0.050;
069980120530         //  §PLBLVS = 0.400;
069990120530         //  exsr  sr_SimilLogoBART;
070000120530         //
070010120530         //EndIf;
070020120530
070030120530         // -?Preparazione simil-Logo BRT?
070040120530         §PLBLS  = §PBA + 0.050;
070050120530         §PLBLVS = 0.400;
070060120530         exsr  sr_SimilLogoBRT_;
070070090406
070080120530         // -?Impostazione dati anagrafici BRT alla sua destra?
070090090422         §PC0    = §PBA    + 0.070 + 0.050;
070100090406         §PC1    = §PC0    + 0.100;
070110090406         §PC2    = §PC1    + 0.100;
070120090406         §PC3    = §PC2    + 0.090;
070130090406         §PC4    = §PC3    + 0.090;
070140090406         §PC5    = §PC4    + 0.090;
070150120530
070160120530         //§PLC    = 1.400;
070170120530         §PLC    = 1.000;
070180090406
070190120530         // -?"TBAR" è già correttamente impostato.?
070200120530         //TBAR    = $Bartolini;        (già così)
070210120530         //TBAR    = $BRT;              (già così)
070220090406         LBA1    = $Intest1;
070230090406         LBA2    = $Intest2_b;
070240120530         //LBA3    = $Intest3_b;
070250120530         LBA3    = $Intest3_b_brt;
070260090406         LBA4    = $Intest4_b;
070270090406         LBA5    = $Intest5_b;
070280081222
070290081222       ENDSR;
070300081222      /end-free
070310050214      *
070320041019      *---------------------------------------------------------------*
070330050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
070340050217      *?Dati di testata: sito e depositante                          ?*
070350041019      *---------------------------------------------------------------*
070360050217     c     PrtDdtLOG1_T1 BEGSR
070370041019      *
070380041019      * > logo
070390050218     c                   eval      §Logo    = 'LOGO' + AR8tmd
070400090107      /free
070410090107         // - Impostazione indicatore che condiziona la stampa del logo
070420120530         //   (o quella del "simil-logo" BRT)
070430090107         *in55 = (%subst(AR8tmd : 1 : 3) = 'LOG'          and
070440090107                  (§AR8tsd  = 'L1'  or  §AR8tsd  = 'L3'));
070450090107      /end-free
070460080908      *
070470060609      *? L O G O !! ?
070480060609      * per pulizia non vogliamo più avere tanti LOGx per ogni cliente che
070490060609      * vuole la stampa della fattura con il proprio logo e la bolla con
070500120530      * il logo BRT
070510060609      * anche Cappellini è d'accordo ad inviare i dati con il logo del
070520060609      * cliente da stampare nella LDV
070530060609      * quelli già esistenti avranno un nuovo logo un po' più piccolo per
070540071008      * problemi di spazio e i dati saranno inviati con AR8tmd = al nome del
070550071008      * nuovo logo e la tabella sarà fatta con §AR8tsd = 'L2' o 'L4'
070560060609      * i nuovi faremo in modo di avere o lo stesso logo oppure la stessa
070570060609      * logica descritta prima
070580060609      * ma può essere che un cliente voglia la fattura con il suo logo
070590120530      * mentre la bolla con il logo BRT quindi devo recuperare
070600060609      * il logo da stampare nella bolla non più da AR8TMD, che conterrà
070610060609      * il nome del logo da stampare nella fattura, ma dal campo del
070620120530      * del logo fattura, che conterrà il logo BRT, mentre il campo
070630071008      * §AR8tsd nella tabella sarà 'L1' o 'L3'
070640071008     c                   if        (§AR8tsd = 'L1' or §AR8tsd = 'L3') and
070650071008     c                             §AR8logof <> *blanks and
070660071008     c                             %subst(AR8tmd:1:3) <> 'LOG'
070670071008     c                   eval      §Logo    = 'LOGO' + §AR8logof
070680090107      /free
070690090107           // - Impostaz. indicatore che condiziona la stampa del logo
070700120530           //   (o quella del "simil-logo" BRT)
070710090107           *in55 = (%subst(§AR8logof : 1 : 3) = 'LOG');
070720090107      /end-free
070730060609     c                   endif
070740041019      *
070750080822     c                   movel(p)  AR8uni        dAR8LOG1T1
070760050414      *
070770060627if  1c                   if            §AR8tsd <> 'LC'
070780041019      *
070790041019      * > sito
070800060407     c                   select
070810071008     c                   when      §AR8tsd = 'L1' or
070820071008     c                             §AR8tsd = 'L3'
070830050707     c                   eval      PLOG1st0 = 'Deposito di:'
070840071008     c                   when      §AR8tsd = 'L2' or
070850071008     c                             §AR8tsd = 'L4'
070860060407     c                   eval      PLOG1st0 = 'Depositario:'
070870060407     c                   endsl
070880050707     c                   eval      PLOG1st1 = §A8LOG1t1a
070890050707     c                   eval      PLOG1st2 = §A8LOG1t1b
070900050707     c                   eval      PLOG1st3 = §A8LOG1t1c
070910050707     c                   eval      PLOG1st4 = §A8LOG1t1d
070920050707     c                   eval      PLOG1st5 = §A8LOG1t1e
070930041019      *
070940041019      * > depositante
070950060407     c                   select
070960071008     c                   when      §AR8tsd = 'L1' or
070970071008     c                             §AR8tsd = 'L3'
070980050707     c                   eval      PLOG1de0 = 'Depositario della:'
070990071008     c                   when      §AR8tsd = 'L2' or
071000071008     c                             §AR8tsd = 'L4'
071010060407     c                   eval      PLOG1de0 = 'Deposito di:'
071020060407     c                   endsl
071030050707     c                   eval      PLOG1de1 = §A8LOG1t1f
071040050707     c                   eval      PLOG1de2 = §A8LOG1t1g
071050050707     c                   eval      PLOG1de3 = §A8LOG1t1h
071060050707     c                   eval      PLOG1de4 = §A8LOG1t1i
071070050707     c                   eval      PLOG1de5 = §A8LOG1t1j
071080050414      *
071090050414x   1c                   else
071100050414      *
071110050707     c***                eval      PLOG1st0 = *blanks
071120050707     c                   eval      PLOG1st1 = §A8LOG1t1a + §A8LOG1t1b
071130050707     c                   eval      PLOG1st2 = §A8LOG1t1c + §A8LOG1t1d
071140050707     c                   eval      PLOG1st3 = §A8LOG1t1e + §A8LOG1t1f
071150050707     c                   eval      PLOG1st4 = §A8LOG1t1g + §A8LOG1t1h
071160050707     c                   eval      PLOG1st5 = §A8LOG1t1i + §A8LOG1t1j
071170050414      *
071180050414e   1c                   endif
071190050214      *
071200041022     c                   ENDSR
071210050214      *
071220041019      *---------------------------------------------------------------*
071230050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
071240050217      *?Dati di testata: destinatario, cliente di fatturazione       ?*
071250041019      *---------------------------------------------------------------*
071260050217     c     PrtDdtLOG1_T2 BEGSR
071270041019      *
071280051110     c                   clear                   dAR8LOG1T2
071290050218     c                   movel     AR8uni        dAR8LOG1T2
071300041019      *
071310041019      * > destinatario
071320050707if  1c                   if        §A8LOG1T2a <> *blanks
071330050707     c                   eval      PLOG1ccd   =  §A8LOG1T2a
071340050218e   1c                   endif
071350050707if  1c                   if        §A8LOG1T2c =  *blanks
071360050707     c                   clear                   PLOG1rsd
071370050707     c                   eval      PLOG1rsd2  =  §A8LOG1T2b
071380050218x   1c                   else
071390050707     c                   eval      PLOG1rsd   =  §A8LOG1T2b
071400050707     c                   eval      PLOG1rsd2  =  §A8LOG1T2c
071410050218e   1c                   endif
071420050707     c                   eval      PLOG1ind   =  §A8LOG1T2d
071430050707     c                   eval      PLOG1ind2  =  §A8LOG1T2e
071440050707if  1c                   if        §A8LOG1T2f >  *zeros
071450050707     c                   eval      PLOG1cad   =  §A8LOG1T2f
071460041019e   1c                   endif
071470050707     c                   eval      PLOG1lod   =  §A8LOG1T2g
071480041019      * > cliente di fatturazione
071490050707if  1c                   if        §A8LOG1T2h <> *blanks
071500050707     c                   eval      PLOG1ccf   =  §A8LOG1T2h
071510041019e   1c                   endif
071520050707if  1c                   if        §A8LOG1T2j =  *blanks
071530050707     c                   clear                   PLOG1rsf
071540050707     c                   eval      PLOG1rsf2  =  §A8LOG1T2i
071550050218x   1c                   else
071560050707     c                   eval      PLOG1rsf   =  §A8LOG1T2i
071570050707     c                   eval      PLOG1rsf2  =  §A8LOG1T2j
071580050218e   1c                   endif
071590050707     c                   eval      PLOG1inf   =  §A8LOG1T2k
071600050707     c                   eval      PLOG1inf2  =  §A8LOG1T2l
071610050707if  1c                   if        §A8LOG1T2m >  *zeros
071620050707     c                   eval      PLOG1caf   =  §A8LOG1T2m
071630041019e   1c                   endif
071640050707     c                   eval      PLOG1lof   =  §A8LOG1T2n
071650041019      *
071660041019     c                   ENDSR
071670041019      *
071680041019      *---------------------------------------------------------------*
071690050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
071700050217      *?Dati di testata: documento, rif.ordine, note                 ?*
071710041019      *---------------------------------------------------------------*
071720050217     c     PrtDdtLOG1_T3 BEGSR
071730041019      *
071740051110     c                   clear                   dAR8LOG1T3
071750050218     c                   movel     AR8uni        dAR8LOG1T3
071760050706      *
071770050706      * TEST effettiva numericità dei campi previsti numerici:
071780100513     c                   clear                   w053a
071790111013     ***c                   eval      WX    =  %len(§A8LOG1T3b)
071800111013     ***c                                   +  %len(§A8LOG1T3c)
071810111013     ***c                                   +  %len(§A8LOG1T3d)
071820111013     ***c                   eval      w053a =  %subst(dAR8LOG1T3: 4:WX)
071830111013     ***c                   eval      *in31 =  *off
071840111013     ***c     c_Digits      checkr    w053a :  WX                            31
071850111013if  1***c                   if        *in31
071860111013     ***c                   clear                   §A8LOG1T3b
071870111013     ***c                   clear                   §A8LOG1T3c
071880111013     ***c                   clear                   §A8LOG1T3d
071890111013e   1***c                   endif
071900121116     ***c                   eval      WX    =  %len(§A8LOG1T3b)
071910121116     ***c                   eval      w053a =  %subst(dAR8LOG1T3: 4:WX)
071920121116     ***c                   eval      *in31 =  *off
071930121116     ***c     c_Digits      checkr    w053a :  WX                            31
071940121116if  1***c                   if        *in31
071950121116     ***c                   clear                   §A8LOG1T3b
071960121116e   1***c                   endif
071970111013     c                   clear                   w053a
071980111013     c                   eval      WX    =  %len(§A8LOG1T3d)
071990111013     c                   eval      w053a =  %subst(dAR8LOG1T3:15:WX)
072000111013     c                   eval      *in31 =  *off
072010111013     c     c_Digits      checkr    w053a :  WX                            31
072020111013if  1c                   if        *in31
072030111013     c                   clear                   §A8LOG1T3d
072040111013e   1c                   endif
072050041019      *
072060041019      * > dati in testata DDT
072070041019      * - - numero e data documento (DDT)
072080050707     c                   eval      PLOG1nsr = §A8LOG1T3a
072090121116     ***c                   eval      PLOG1aas = §A8LOG1T3b
072100121116     c                   if        §A8Log1T3b <> *blanks   and
072110121116     c                             §A8Log1T3b <> *zeros
072120121116     c                   eval      PLOG1aas = §A8LOG1T3b
072130121116     c                   endif
072140111013     ***c                   if        §A8Log1T3c <> *zeros
072150111013     ***c                   eval      PLOG1nsp = %editc(§A8LOG1T3c:'X')
072160111013     ***c                   endif
072170111013     c                   if        §A8Log1T3c <> *blanks   and
072180111013     c                             §A8Log1T3c <> *zeros
072190111013     c                   eval      PLOG1nsp = §A8LOG1T3c
072200111013     c                   endif
072210100319     c                   if        §A8Log1T3a <> *blanks   and
072220121116     c                             §A8Log1T3b <> *blanks   and
072230100319     c                             §A8Log1T3b <> *zeros
072240100319     c                   eval      PLog1sepA = '/'
072250100319     c                   endif
072260121115     c                   if        §A8Log1T3b <> *zeros    and
072270121115     c                             §A8Log1T3c <> *blanks   and
072280100319     c                             §A8Log1T3c <> *zeros
072290100319     c                   eval      PLog1sepB = '/'
072300100319     c                   endif
072310110707      *     giro la data del documento
072320050707if  1c                   if        §A8LOG1T3d > *zeros
072330060922     c                   eval      *in31 =  *off
072340060922     c     *iso          test(d)                 §A8LOG1T3d             31
072350060922if  2c                   if        NOT *in31
072360050707     c     *iso          movel     §A8LOG1T3d    Data_EUR
072370050707     c     *eur          movel     Data_EUR      PLOG1dsp
072380060922x   2c                   else
072390060922     c                   clear                   PLOG1dsp
072400060922e   2c                   endif
072410050707e   1c                   endif
072420041019      * - - riferimento ordine
072430050707     c                   eval      PLOG1ref = §A8LOG1T3e
072440041019      * - - note consegna
072450050707     c                   eval      PLOG1not = §A8LOG1T3f
072460050707     c                   eval      PLOG1nt2 = §A8LOG1T3g
072470060612      * - - intestazione corpo bolla per Logistica tipo stampa L3 o L4
072480060922     c                   if        §AR8tsd = 'L3' or §AR8tsd = 'L4'
072490060612     c                   eval      *in23 = *On
072500060612     c                   eval      PLOG1int = §A8LOG1T3h
072510060612     c                   else
072520060612     c                   eval      *in23 = *off
072530060612     c                   endif
072540041019      *
072550041019      * > Stampa testata DDT - - - - - - - - - - - - - - - - - - - - -*
072560041019      *
072570041019      *   - OverFlow impossibile qui! -
072580050218     c***                exsr      Test_OF
072590050318     c                   exsr      Posiz_DL
072600081017     c                   if            §AR8cxd = *blanks
072610081017     c                             or  DB0fa4  = 'P'
072620050331     c                   write     A4prtDT0
072630050331     c                   endif
072640050218     c                   write     A4prtDL0
072650081222      /free
072660081222         if  *in55;
072670090406           exsr   PrtDdtLOG1_SimilLogo;
072680120530           //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
072690120530           //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
072700120530           //  write  A4LogoBR2;
072710120530           //else;
072720120530           //  write  A4LogoBAR;
072730120530           //endif;
072740120530           write  A4LogoBRT;
072750081222           write  A4prtDLL;
072760081222         endif;
072770081222      /end-free
072780050218     c*** quante righe?  write     A4prtDL0A
072790041028     c                   write     A4prtDL1A
072800050218     c                   write     A4prtDL1B
072810041019      *
072820041019     c                   ENDSR
072830050214      *
072840041019      *---------------------------------------------------------------*
072850050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
072860050217      *?Dati di dettaglio: articolo, quantità, descrizione           ?*
072870041019      *---------------------------------------------------------------*
072880050217     c     PrtDdtLOG1_C1 BEGSR
072890041019      *
072900041019     c                   clear                   A4prtDL2
072910051110     c                   clear                   dAR8LOG1C1
072920050218     c                   movel     AR8uni        dAR8LOG1C1
072930050706      *
072940050706      * TEST effettiva numericità dei campi previsti numerici:
072950100513     c                   clear                   w053a
072960050706     c                   eval      WX    =  %len(§A8LOG1C1b)
072970100513     c                   eval      w053a =  %subst(dAR8LOG1C1:21:WX)
072980050706     c                   eval      *in31 =  *off
072990110426     c     c_Digits      checkr    w053a :  WX                            31
073000050706if  1c                   if        *in31
073010050706     c                   clear                   §A8LOG1C1b
073020050706e   1c                   endif
073030041019      *
073040041019      * > dati in dettaglio DDT
073050041019      * - - codice prodotto
073060050707     c                   eval      PLOG1cpr  = §A8LOG1C1a
073070041019      * - - quantità
073080050707     c                   eval      PLOG1qta  = §A8LOG1C1b
073090041019      * - - descrizione prodotto
073100050707     c                   eval      PLOG1dpr  = §A8LOG1C1c
073110041019      *
073120041019      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
073130041019      *
073140041019     c                   exsr      Test_OF
073150041019     c                   write     A4prtDL2
073160041019      *
073170041019     c                   ENDSR
073180041019      *
073190041019      *---------------------------------------------------------------*
073200050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
073210050217      *?Dati di dettaglio: descrizione                               ?*
073220041019      *---------------------------------------------------------------*
073230050217     c     PrtDdtLOG1_C2 BEGSR
073240041019      *
073250041019     c                   clear                   A4prtDL2
073260051110     c                   clear                   dAR8LOG1C2
073270050218     c                   movel     AR8uni        dAR8LOG1C2
073280041019      *
073290041019      * > dati in note DDT
073300041019      * - - riga descrittiva
073310050707     c                   eval      PLOG1dpr  = §A8LOG1C2a
073320041019      *
073330041019      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
073340041019      *
073350041019     c                   exsr      Test_OF
073360041019     c                   write     A4prtDL2
073370041019      *
073380041019     c                   ENDSR
073390041019      *
073400041019      *---------------------------------------------------------------*
073410050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
073420050217      *?Dati di dettaglio: descrizioni                               ?*
073430041019      *---------------------------------------------------------------*
073440050217     c     PrtDdtLOG1_C3 BEGSR
073450041019      *
073460041019     c                   clear                   A4prtDL3
073470051110     c                   clear                   dAR8LOG1C3
073480050707     c                   movel     AR8uni        dAR8LOG1C3
073490041019      *
073500041019      * > dati in note DDT
073510041019      * - - riga descrittiva
073520050707     c                   eval      PLOG1txt  = §A8LOG1C3a
073530041019      *
073540041019      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
073550041019      *
073560041019     c                   exsr      Test_OF
073570041019     c                   write     A4prtDL3
073580041019      *
073590041019     c                   ENDSR
073600060614      *
073610060614      *---------------------------------------------------------------*
073620060614      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
073630060614      *?Dati di dettaglio: corpo bolla non fincato                   ?*
073640060614      *---------------------------------------------------------------*
073650060614     c     PrtDdtLOG1_C4 BEGSR
073660060614      *
073670060614     c                   clear                   A4prtDL21
073680060614     c                   clear                   dAR8LOG1C4
073690060614     c                   movel     AR8uni        dAR8LOG1C4
073700060614      *
073710060614      * > dati DDT
073720060614      * - - riga descrittiva
073730060614     c                   eval      PLOG1dpr1 = §A8LOG1C4a
073740060614      *
073750060614      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
073760060614      *
073770060614     c                   exsr      Test_OF
073780060614     c                   write     A4prtDL21
073790060614      *
073800060614     c                   ENDSR
073810090323      *
073820090323      *---------------------------------------------------------------*
073830090323      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
073840090323      *?Dati di dettaglio: descrizioni                               ?*
073850090323      *---------------------------------------------------------------*
073860090323     c     PrtDdtLOG1_C5 BEGSR
073870090323      *
073880090323     c                   clear                   A4prtDL31
073890090323     c                   clear                   dAR8LOG1S3
073900090323     c                   movel     AR8uni        dAR8LOG1S3
073910090323      *
073920090323      * > dati in note DDT
073930090323      * - - riga descrittiva
073940090323     c                   eval      PLOG1txt2 = §A8LOG1S3a
073950090323      *
073960090323      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
073970090323      *
073980090323     c                   exsr      Test_OF
073990090323     c                   write     A4prtDL31
074000090323      *
074010090323     c                   ENDSR
074020041019      *
074030041019      *---------------------------------------------------------------*
074040050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
074050050217      *?Dati di fondo: beni, colli, peso, volume, c/a, porto, trasp. ?*
074060041019      *---------------------------------------------------------------*
074070050217     c     PrtDdtLOG1_P1 BEGSR
074080041019      *
074090051110     c                   clear                   dAR8LOG1P1
074100050707     c                   movel     AR8uni        dAR8LOG1P1
074110050706      *
074120050706      * TEST effettiva numericità dei campi previsti numerici:
074130100513     c                   clear                   w053a
074140050706     c                   eval      WX    =  %len(§A8LOG1P1b)
074150050706     c                                   +  %len(§A8LOG1P1c)
074160050706     c                                   +  %len(§A8LOG1P1d)
074170100513     c                   eval      w053a =  %subst(dAR8LOG1P1:42:WX)
074180050706     c                   eval      *in31 =  *off
074190110426     c     c_Digits      checkr    w053a :  WX                            31
074200050706if  1c                   if        *in31
074210050706     c                   clear                   §A8LOG1P1b
074220050706     c                   clear                   §A8LOG1P1c
074230050706     c                   clear                   §A8LOG1P1d
074240050706e   1c                   endif
074250100513     c                   clear                   w053a
074260050706     c                   eval      WX    =  %len(§A8LOG1P1f)
074270100513     c                   eval      w053a =  %subst(dAR8LOG1P1:67:WX)
074280050706     c                   eval      *in31 =  *off
074290110426     c     c_Digits      checkr    w053a :  WX                            31
074300050706if  1c                   if        *in31
074310050706     c                   clear                   §A8LOG1P1f
074320050706e   1c                   endif
074330041019      *
074340041019      * > dati in chiusura DDT
074350041019      * - - aspetto esteriore dei beni
074360050707     c                   eval      PLOG1aeb  = §A8LOG1P1a
074370041019      * - - numero colli
074380050707     c                   eval      PLOG1ncl  = §A8LOG1P1b
074390041019      * - - peso in kg
074400050707     c                   eval      PLOG1pkg  = §A8LOG1P1c
074410041019      * - - volume in mc
074420050707     c                   eval      PLOG1vlm  = §A8LOG1P1d
074430041019      * - - c/assegno
074440050707     c                   eval      PLOG1cva  = §A8LOG1P1e
074450050707     c                   eval      PLOG1cas  = §A8LOG1P1f
074460041020      * - - causale trasporto
074470050707     c                   eval      PLOG1ctr  = §A8LOG1P1g
074480041020      * - - porto
074490050707     c                   eval      PLOG1pto  = §A8LOG1P1h
074500041020      * - - trasporto a cura
074510050707     c                   eval      PLOG1tra  = §A8LOG1P1i
074520041020      * - - trasportatore
074530050707     c                   eval      PLOG1vet  = §A8LOG1P1j
074540050505      *
074550050505      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
074560050505      *
074570050505      * - - Verifica OverFlow
074580080925      /free
074590080925           select;
074600080925             when  NOT *in45;
074610080925               wMaxLine = $MaxLine4;
074620080925             when  *in45   and   NOT *in46;
074630080925               wMaxLine = $MaxLine4_pl1;
074640080925             when  *in45   and   *in46;
074650080925               wMaxLine = $MaxLine4_pl2;
074660080925           endsl;
074670080925      /end-free
074680050505     c                   exsr      Test_OF
074690050505      * - - Calcolo della posizione finale di box e linee separatrici
074700050505     c                   exsr      Posiz_DL
074710050505      * - - Stampa parte finale DDT
074720050505     c                   write     A4prtDL4
074730050505      *
074740050505      * > Stampa box, linee e testi fissi (no logo)  - - - - - - - - -*
074750090331     c                   if            §AR8cxd = *blanks
074760090331     c                             or  DB0fa4  = 'P'
074770090331     c                   write     A4prtDT0
074780090331     c                   endif
074790050505     c                   write     A4prtDL0A
074800050505     c                   write     A4prtDL0B
074810050505      *
074820050505      * Azzero il numero di pagina per l'eventuale Packing-List
074830050505      *   (successiva)
074840050505     c                   clear                   Page
074850041019      *
074860041019      * N.B.: la copia per il destinatario della LDV deve comparire
074870041019      *       nell'ultima pagina del DDT per il cliente (vedi subr.
074880041019      *       "ChiusuraLDV")
074890041019      *
074900041019     c                   ENDSR
074910050318      *
074920050318      *---------------------------------------------------------------*
074930050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
074940050318      *?Dati di packing-list: dettaglio                              ?*
074950050318      *---------------------------------------------------------------*
074960050318     c     PrtDdtLOG1_Z1 BEGSR
074970050318      *
074980050318      * Prima della 1ª riga di packing-list: testata packing-list
074990050505if  1c                   if        Page     = *zeros
075000050318     c                   exsr      PrtPckLst_1
075010050318e   1c                   endif
075020050318      *
075030050318     c                   clear                   A4prtDL5
075040051110     c                   clear                   dAR8LOG1Z1
075050050707     c                   movel     AR8uni        dAR8LOG1Z1
075060050706      *
075070050706      * TEST effettiva numericità dei campi previsti numerici:
075080100513     c                   clear                   w053a
075090050706     c                   eval      WX    =  %len(§A8LOG1Z1d)
075100100513     c                   eval      w053a =  %subst(dAR8LOG1Z1:53:WX)
075110050706     c                   eval      *in31 =  *off
075120110426     c     c_Digits      checkr    w053a :  WX                            31
075130050706if  1c                   if        *in31
075140050706     c                   clear                   §A8LOG1Z1d
075150050706e   1c                   endif
075160050318      *
075170050318      * > dati in dettaglio Packing-List
075180050318      * - - sscc
075190050318     c                   eval      PCsscc   = §A8LOG1Z1a
075200050318      * - - gtin
075210050318     c                   eval      PCgtin   = §A8LOG1Z1b
075220050318      * - - codice prodotto
075230050318     c                   eval      PCcpr2   = §A8LOG1Z1c
075240050318      * - - quantità ul
075250050318     c                   eval      PLOG1QTA = §A8LOG1Z1d
075260050318      * - - tipo contenitore
075270050318     c                   eval      PCtcn    = §A8LOG1Z1e
075280050318      * - - codice contenitore
075290050318     c                   eval      PCccn    = §A8LOG1Z1f
075300050318      * - - numero lotto
075310050318     c                   eval      PClot    = §A8LOG1Z1g
075320050318      * - - data scadenza
075330050318     c                   eval      PCdts    = §A8LOG1Z1h
075340050318      *
075350050318      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
075360050318      *
075370050318     c                   exsr      Test_OF
075380050318     c                   write     A4prtDL5
075390050318      *
075400050318     c                   ENDSR
075410080908      *
075420050606      *---------------------------------------------------------------*
075430050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
075440080925      *?Dati di Fattura: Testata                                     ?*
075450050606      *---------------------------------------------------------------*
075460050606     c     PrtDdtLOG1_S1 BEGSR
075470050606      *
075480050606      * > logo Fattura
075490050606     c                   eval      §LogoF   = 'LOGO' + §AR8logof
075500090107      /free
075510090107         // - Impostazione indicatore che condiziona la stampa del logo
075520120530         //   (o quella del "simil-logo" BRT)
075530090107         *in55 = (%subst(§AR8logof : 1 : 3) = 'LOG'       and
075540090107                  (§AR8tsd  = 'L1'  or  §AR8tsd  = 'L3'));
075550090107      /end-free
075560080908      *
075570060609      *? L O G O !! ?
075580060609      *?vedi stampa testata bolla ?
075590071008     c                   if        (§AR8tsd = 'L1' or §AR8tsd = 'L3') and
075600071008     c                             §AR8logof <> *blanks and
075610071008     c                             %subst(AR8tmd:1:3) <> 'LOG'
075620071008     c                   eval      §LogoF   = 'LOGO' + AR8tmd
075630090107      /free
075640090107           // - Impostaz. indicatore che condiziona la stampa del logo
075650120530           //   (o quella del "simil-logo" BRT)
075660090107           *in55 = (%subst(AR8tmd : 1 : 3) = 'LOG');
075670090107      /end-free
075680060609     c                   endif
075690050606      *
075700051110     c                   clear                   dAR8LOG1S1
075710050606     c                   movel     AR8uni        dAR8LOG1S1
075720050706      *
075730050706      * TEST effettiva numericità dei campi previsti numerici:
075740100513     c                   clear                   w053a
075750050706     c                   eval      WX    =  %len(§A8LOG1S1i)
075760100513     c                   eval      w053a =  %subst(dAR8LOG1S1:216:WX)
075770050706     c                   eval      *in31 =  *off
075780110426     c     c_Digits      checkr    w053a :  WX                            31
075790050706if  1c                   if        *in31
075800050706     c                   clear                   §A8LOG1S1i
075810050706e   1c                   endif
075820050707      *
075830050606      * > intestazione fattura
075840050707if  1c                   if        §A8LOG1S1b =  *blanks
075850050707     c                   clear                   PLOG1rsf
075860050707     c                   eval      PLOG1rsf2  =  §A8LOG1S1a
075870050606x   1c                   else
075880050707     c                   eval      PLOG1rsf   =  §A8LOG1S1a
075890050707     c                   eval      PLOG1rsf2  =  §A8LOG1S1b
075900050606e   1c                   endif
075910050707     c                   eval      PLOG1inf   =  §A8LOG1S1c
075920050707     c                   eval      PLOG1inf2  =  §A8LOG1S1d
075930050707if  1c                   if        §A8LOG1S1e >  *zeros
075940050707     c                   eval      PLOG1caf   =  §A8LOG1S1e
075950050606e   1c                   endif
075960050707     c                   eval      PLOG1lof   =  §A8LOG1S1f
075970050606      * > partita IVA - codice fiscale
075980050707     c                   eval      PLOG1IVA   =  §A8LOG1S1g
075990050606      * > n./data fattura
076000050707     c                   eval      PLOG1nft   =  §A8LOG1S1h
076010050606      *     giro la data fattura
076020050706if  1c                   if        §A8LOG1S1i >  *zeros
076030060922     c                   eval      *in31 =  *off
076040060922     c     *iso          test(d)                 §A8LOG1S1i             31
076050060922if  2c                   if        NOT *in31
076060050707     c     *iso          movel     §A8LOG1S1i    Data_EUR
076070050707     c     *eur          movel     Data_EUR      PLOG1dft
076080060922x   2c                   else
076090060922     c                   clear                   PLOG1dft
076100060922e   2c                   endif
076110050706e   1c                   endif
076120050606      * > rif.ns.spedizione
076130060922     c                   eval      PLOG1nspa =   §A8LOG1S1j
076140050606      * > intestazione corpo fattura
076150050707     c                   eval      PLOG1tesf  =  §A8LOG1S1k
076160050707      *
076170050606      * > Stampa testata FATTURA - - - - - - - - - - - - - - - - - - -*
076180081015      *
076190080925      /free
076200081015         // - Forzatura del salto pagina
076210081015         CurLineA4 = wMaxLine + 1;
076220080925      /end-free
076230080925     c                   exsr      Test_OF
076240081015      /free
076250090323         // - Impostazione dei nuovi limiti di pagina
076260090323         select;
076270090323           when  NOT *in45;
076280090323             wMaxLine = $MaxLine2;
076290090323           when  *in45   and   NOT *in46;
076300090323             wMaxLine = $MaxLine2_pl1;
076310090323           when  *in45   and   *in46;
076320090323             wMaxLine = $MaxLine2_pl2;
076330090323         endsl;
076340081015      /end-free
076350080925     c                   exsr      Posiz_DL
076360081017     c                   if            §AR8cxd = *blanks
076370081017     c                             or  DB0fa4  = 'P'
076380080925     c                   write     A4prtDT0
076390080925     c                   endif
076400080925     c                   write     A4prtDL0D
076410050606     c                   write     A4prtDL1D
076420050606     c                   write     A4prtDL1E
076430050707      *
076440050606     c                   ENDSR
076450050606      *
076460050606      *---------------------------------------------------------------*
076470050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
076480050606      *?Dati di dettaglio corpo: FATTURA                             ?*
076490050606      *---------------------------------------------------------------*
076500050606     c     PrtDdtLOG1_S2 BEGSR
076510050606      *
076520050606     c                   clear                   A4prtDL6
076530051110     c                   clear                   dAR8LOG1S2
076540050606     c                   movel     AR8uni        dAR8LOG1S2
076550050606      *
076560050606      * > dati fattura
076570050606      * - - riga descrittiva 1
076580050707     c                   eval      PLOG1dft1 = §A8LOG1S2a
076590050606      *
076600050606      * > Stampa dettaglio FATTURA - - - - - - - - - - - - - - - - - -*
076610050606      *
076620050606     c                   exsr      Test_OF
076630050606     c                   write     A4prtDL6
076640050606      *
076650050606     c                   ENDSR
076660050606      *
076670050606      *---------------------------------------------------------------*
076680050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
076690050606      *?Dati di dettaglio corpo: FATTURA                             ?*
076700050606      *---------------------------------------------------------------*
076710050606     c     PrtDdtLOG1_S3 BEGSR
076720050606      *
076730050606     c                   clear                   A4prtDL7
076740051110     c                   clear                   dAR8LOG1S3
076750050707     c                   movel     AR8uni        dAR8LOG1S3
076760050606      *
076770050606      * > dati fattura
076780050606      * - - riga descrittiva 2
076790050707     c                   eval      PLOG1dft2 = §A8LOG1S3a
076800050606      *
076810050606      * > Stampa dettaglio FATTURA - - - - - - - - - - - - - - - - - -*
076820050606      *
076830050606     c                   exsr      Test_OF
076840050606     c                   write     A4prtDL7
076850050606      *
076860050606     c                   ENDSR
076870050606      *
076880050606      *---------------------------------------------------------------*
076890050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
076900050606      *?Dati di fondo: FATTURA                                       ?*
076910050606      *---------------------------------------------------------------*
076920050606     c     PrtDdtLOG1_S4 BEGSR
076930050606      *
076940051110     c                   clear                   dAR8LOG1S4
076950050707     c                   movel     AR8uni        dAR8LOG1S4
076960050706      *
076970050706      * TEST effettiva numericità dei campi previsti numerici:
076980100513     c                   clear                   w053a
076990050706     c                   eval      WX    =  %len(§A8LOG1S4a)
077000050706     c                                   +  %len(§A8LOG1S4b)
077010050706     c                                   +  %len(§A8LOG1S4c)
077020100513     c                   eval      w053a =  %subst(dAR8LOG1S4: 1:WX)
077030050706     c                   eval      *in31 =  *off
077040110426     c     c_Digits      checkr    w053a :  WX                            31
077050050706if  1c                   if        *in31
077060050706     c                   clear                   §A8LOG1S4a
077070050706     c                   clear                   §A8LOG1S4b
077080050706     c                   clear                   §A8LOG1S4c
077090050706e   1c                   endif
077100050606      *
077110050606      * > dati in chiusura FATTURA
077120050606      * - - dati di piede fattura riga 1
077130050707     c                   eval      PLOG1pft1 = §A8LOG1S4d
077140050606      * - - dati di piede fattura riga 2
077150050707     c                   eval      PLOG1pft2 = §A8LOG1S4e
077160050606      * - - dati di piede fattura riga 3
077170050707     c                   eval      PLOG1pft3 = §A8LOG1S4f
077180050606      * - - totale imponibile
077190050707     c                   eval      PLOG1timp = §A8LOG1S4a
077200050606      * - - totale iva
077210050707     c                   eval      PLOG1tiva = §A8LOG1S4b
077220050606      * - - totale fattura
077230050707     c                   eval      PLOG1tft  = §A8LOG1S4c
077240050606      *
077250050606      * > Stampa fondo FATTURA - - - - - - - - - - - - - - - - - - - -*
077260050606      *
077270050606      * - - Verifica OverFlow
077280080925      /free
077290080925           select;
077300080925             when  NOT *in45;
077310080925               wMaxLine = $MaxLine4;
077320080925             when  *in45   and   NOT *in46;
077330080925               wMaxLine = $MaxLine4_pl1;
077340080925             when  *in45   and   *in46;
077350080925               wMaxLine = $MaxLine4_pl2;
077360080925           endsl;
077370080925      /end-free
077380050606     c                   exsr      Test_OF
077390050606      * - - Calcolo della posizione finale di box e linee separatrici
077400050606     c                   exsr      Posiz_DL
077410050606      * - - Stampa parte finale FATTURA
077420050606     c                   write     A4prtDL8
077430050606      *
077440050606      * > Stampa box, linee (no logo)   - - - - - - - - - - - - - - -*
077450050606     c                   write     A4prtDL0E
077460050606     c                   write     A4prtDL0F
077470080908      *
077480050606     c                   ENDSR
077490050606      *
077500050606      *---------------------------------------------------------------*
077510050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
077520050606      *?Dati di fondo note: FATTURA                                  ?*
077530050606      *---------------------------------------------------------------*
077540050606     c     PrtDdtLOG1_S5 BEGSR
077550050606      *
077560051110     c                   clear                   dAR8LOG1S5
077570050707     c                   movel     AR8uni        dAR8LOG1S5
077580050606      *
077590050606      * > dati in chiusura FATTURA
077600050606      * - - nota 1
077610050707     c                   eval      PLOG1not1 = §A8LOG1S5a
077620050606      * - - nota 2
077630050707     c                   eval      PLOG1not2 = §A8LOG1S5b
077640050606      * - - nota 3
077650050707     c                   eval      PLOG1not3 = §A8LOG1S5c
077660050606      *
077670050606      * > Stampa fondo FATTURA - - - - - - - - - - - - - - - - - - - -*
077680050606      * - - Stampa parte finale FATTURA
077690050606     c                   write     A4prtDL9
077700080908      *
077710050606     c                   ENDSR
077720050321      *
077730050321      *---------------------------------------------------------------*
077740050414      *?Impostazione nuova pagina di DDT Logistica / Sait            ?*
077750050321      *---------------------------------------------------------------*
077760050321     c     NewPageDL     BEGSR
077770050321      *
077780080925      /free
077790080925         // Impostazione nuovo limite di OF
077800080925         SELECT;
077810080925           // Piede DDT   oppure   Piede Fattura
077820080925           WHEN  AR8trk = 'P1'   or   AR8trk = 'S4';
077830080925             select;
077840080925               when  NOT *in45;
077850080925                 wMaxLine = $MaxLine4;
077860080925               when  *in45   and   NOT *in46;
077870080925                 wMaxLine = $MaxLine4_pl1;
077880080925               when  *in45   and   *in46;
077890080925                 wMaxLine = $MaxLine4_pl2;
077900080925             endsl;
077910080925           // Dettaglio DDT  oppure   Testata & Dettaglio Fattura
077920080925           OTHER;
077930080925             select;
077940080925               when  NOT *in45;
077950090323                 wMaxLine = $MaxLine2;
077960080925               when  *in45   and   NOT *in46;
077970090323                 wMaxLine = $MaxLine2_pl1;
077980080925               when  *in45   and   *in46;
077990090323                 wMaxLine = $MaxLine2_pl2;
078000080925             endsl;
078010080925         ENDSL;
078020080925      /end-free
078030080925      *
078040080825      * RE-impostazione posizioni
078050080825     c                   exsr      Posiz_DL
078060080825      *
078070050321     c                   write     A4prtSP1
078080081006      /free
078090081006         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
078100081006           write  A4prtNUL;
078110081006         endif;
078120081006      /end-free
078130050321      *
078140050505sel 1c                   select
078150050505      * - altra pagina DDT Logistica / Sait
078160050707w   1c                   when          %subst(AR8trk:1:1) <> 'Z'
078170050606     c                             and %subst(AR8trk:1:1) <> 'S'
078180050321     c                   write     A4prtDL1B
078190050505      * - altra pagina Packing-List
078200050505w   1c                   when      %subst(AR8trk:1:1) =  'Z'
078210050502     c                              and   Page        >  *zeros
078220050321     c                   write     A4prtDL1C
078230050606      * - altra pagina Fattura
078240050606w   1c                   when      %subst(AR8trk:1:1) =  'S'
078250050606     c                              and   Page        >  *zeros
078260050606     c                   write     A4prtDL1E
078270050505e   1c                   endsl
078280050321      *
078290050321     c                   ENDSR
078300050318      *
078310050318      *---------------------------------------------------------------*
078320080709      *?Stampa testata della Packing-List per LOGISTICA              ?*
078330050318      *---------------------------------------------------------------*
078340050318     c     PrtPckLst_1   BEGSR
078350050318      *
078360050318      * > Salto a pagina successiva  - - - - - - - - - - - - - - - - -*
078370050318     c                   exsr      Test_OF
078380050318      *
078390050505      * > Stampa box, linee e testi fissi (CON logo)  - - - - - - - - -*
078400050318      *
078410050318      * - - Stampa "Copia per il destinatario" nella pagina successiva
078420081017     c*** poi in Test_OF:   if            §AR8cxd = *blanks
078430081017     c*** poi in Test_OF:             or  DB0fa4  = 'P'
078440050505     c*** poi in Test_OF:   write     A4prtDT0
078450050505     c*** poi in Test_OF:   endif
078460050318      * - - Stampa dati destinatario nel box superiore
078470050318     c                   write     A4prtDL1A
078480050318      * - - Stampa logo e box superiore x destinatario
078490050318     c                   write     A4prtDL0
078500081222      /free
078510081222         if  *in55;
078520090406           exsr   PrtDdtLOG1_SimilLogo;
078530120530           //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
078540120530           //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
078550120530           //  write  A4LogoBR2;
078560120530           //else;
078570120530           //  write  A4LogoBAR;
078580120530           //endif;
078590120530           write  A4LogoBRT;
078600081222           write  A4prtDLL;
078610081222         endif;
078620081222      /end-free
078630050318      * - - Stampa dati di testata nel box generale
078640050318     c                   write     A4prtDL1C
078650050318      * - - Stampa box generale
078660050318     c*** poi in Test_OF:   write     A4prtDL0C
078670050318      *
078680050318      * Impostazione del nuovo limite x overflow in packing-list
078690080925      /free
078700080925         select;
078710080925           when  NOT *in45;
078720090323             wMaxLine = $MaxLine2;
078730080925           when  *in45   and   NOT *in46;
078740090323             wMaxLine = $MaxLine2_pl1;
078750080925           when  *in45   and   *in46;
078760090323             wMaxLine = $MaxLine2_pl2;
078770080925         endsl;
078780080925      /end-free
078790050318      *
078800050318     c                   ENDSR
078810041019      *
078820041020      *---------------------------------------------------------------*
078830050414      *?Posizionamento di linee e box nel DDT LOGISTICA / SAIT       ?*
078840041020      *---------------------------------------------------------------*
078850041021     c     Posiz_DL      BEGSR
078860050218      *
078870080924      /free
078880080924         // Impostazione posizioni di logo/box
078890080924         // (posizioni di "partenza" diverse a seconda del posizionam.
078900080924         //  iniziale nel foglio A4 attualmente in stampa)
078910080924         select;
078920080924           when  NOT *in45;
078930081006             §PBA =  3.255;
078940080924           when  *in45   and   NOT *in46;
078950090422             §PBA = 0.255;
078960080924           when  *in45   and   *in46;
078970090422             §PBA = 6.005;
078980080924         endsl;
078990080924      /end-free
079000080708     c                   eval      §PBB  =  §PBA + 1.27
079010080708     c                   eval      §LOA  =  §PBA + 0.63
079020080708     c                   eval      §PS0  =  §PBA + 0.72
079030080708     c                   eval      §PS1  =  §PS0 + 0.10
079040080708     c                   eval      §PS2  =  §PS1 + 0.10
079050080708     c                   eval      §PS3  =  §PS2 + 0.10
079060080708     c                   eval      §PS4  =  §PS3 + 0.10
079070080708     c                   eval      §PS5  =  §PS4 + 0.10
079080080708      * Impostazione posizioni di logo/box per fattura
079090080708     c                   eval      §PBC  =  §PBA + 0.92
079100080708     c                   eval      §LOB  =  §PBA + 0.72
079110080708      * Impostazione posizioni di note fine fattura
079120080925     c                   eval      §PS6  =  §PBA + 5.35
079130081003     c                   eval      §PS7  =  §PS6 + 0.08
079140081003     c                   eval      §PS8  =  §PS7 + 0.08
079150080708      *
079160050321if  1c                   if        Page  <= 1
079170050218      *   (posizione di "partenza" su 1ª pagina)
079180080924     c                   eval      §PB1  =  §PBA + 1.375
079190050218x   1c                   else
079200050318      *   (posizione di "partenza" su pagine successive)
079210080822     c                   eval      §PB1  =  §PBA
079220050218e   1c                   endif
079230080924     c                   eval      §PB2  =  §PBA + 5.275
079240050218      * Impostazione posizioni di linee (orizzontali e verticali)
079250050218      *   nella parte superiore del box (testata)
079260050218     c                   eval      §LO1  =  §PB1 + 0.13
079270050318if  1c                   if        %subst(AR8trk:1:1) <> 'Z'
079280050218     c                   eval      §LO2  =  §LO1 + 0.13
079290050318     c                   eval      §LO3  =  §LO2 + 0.11
079300050318x   1c                   else
079310080825     c                   eval      §LO2  =  §LO1 + 0.115
079320050318     c                   eval      §LO3  =  §LO2
079330050318e   1c                   endif
079340050607if  1c                   if        %subst(AR8trk:1:1) = 'S'
079350050610     c                   eval      §LO3  =  §PB2 - 0.40
079360050607     c                   EndIf
079370050218      * Impostazione posizioni di linee (orizzontali e verticali)
079380050218      *   nella parte inferiore del box (piede)
079390050318if  1c                   if        %subst(AR8trk:1:1) <> 'Z'
079400080822     c                   eval      §LO4  =  §PB2 - 0.52
079410090422     c                   eval      §LO5  =  §LO4 + 0.13
079420090422     c                   eval      §LO6  =  §LO5 + 0.13
079430090422     c                   eval      §LO7  =  §LO6 + 0.13
079440050318e   1c                   endif
079450050218      * Impostazione lunghezza delle linee verticali
079460050218      *   nella parte superiore del box (testata)
079470050218     c                   eval      §LV1  =  §LO1 - §PB1
079480050218     c                   eval      §LV2  =  §LO2 - §LO1
079490050318if  1c                   if        %subst(AR8trk:1:1) <> 'Z'
079500050218     c                   eval      §LV3  =  §LO3 - §LO2
079510050318e   1c                   endif
079520050607if  1c                   if        %subst(AR8trk:1:1) = 'S'
079530050607     c                   eval      §LV2  =  0.40
079540050607     c                   EndIf
079550050218      * La lunghezza delle linee verticali separatrici nel dettaglio
079560050218      *   dipende dalla fase di stampa: se si sta stampando ancora il
079570050218      *   dettaglio deve essere più lunga, visto che non viene stampa-
079580050218      *   ta la parte finale del DDT...
079590080924if  1c                   if        %subst(AR8trk:1:1) = 'P'
079600080924     c                              and  (§PS1        < $LimOF
079610080924     c                                and NOT *in45
079620080924     c                                OR  §PS1        < ($LimOF - c_Len1_inch)
079630080924     c                                and *in45   and   NOT *in46
079640081003     c                                OR  §PS1        < ($LimOF + c_Len2_inch)
079650080924     c                                and *in45   and   *in46)
079660050318     c                              and   CurLineA4   <  wMaxLine
079670050218     c                   eval      §LV4  =  §LO4 - §LO3
079680050218x   1c                   else
079690050218     c                   eval      §LV4  =  §PB2 - §LO3
079700050218e   1c                   endif
079710080908      *
079720071008     c                   if        §AR8tsd = 'L3' or §AR8tsd = 'L4' and
079730060609     c                             %subst(AR8trk:1:1) <> 'Z' and
079740060609     c                             %subst(AR8trk:1:1) <> 'S'
079750071008     c                   eval      §LV3 = 0
079760071008     c                   eval      §LV4 = 0
079770060609     c                   endif
079780080908      *
079790050502      * - qui imposto le posizioni per il piede del box
079800050318if  1c                   if        %subst(AR8trk:1:1) <> 'Z'
079810050218     c                   eval      §LV5  =  §LO5 - §LO4
079820050218     c                   eval      §LV6  =  §LO6 - §LO5
079830050218     c                   eval      §LV7  =  §PB2 - §LO7
079840050318e   1c                   endif
079850041028      * Impostazione posizioni dei campi di testo verticali
079860080822     c                   eval      §PR1  =  §PB2 - 0.05
079870041020      *
079880041020     c                   ENDSR
079890050113      *
079900050113      *---------------------------------------------------------------*
079910050217      *?Stampa DDT del mittente ARCTE                                ?*
079920050113      *---------------------------------------------------------------*
079930050217     c     DDT_Arcte     BEGSR
079940050113      *
079950081002      /free
079960081002         // Impostazione del nuovo limite x overflow
079970081002         // (Codesto limite è valido solo per la stampa del dettaglio.
079980081002         //   Per la stampa del "fondo" bolla occorre REimpostarlo ad
079990081002         //   un valore più basso.
080000081002         //  Ovvero: stampo più righe di dettaglio che posso; se alla
080010081002         //  fine delle righe di dettaglio non rimane spazio a suffi-
080020081002         //  cienza per il fondo: prima della stampa del fondo si salta
080030081002         //  alla pagina successiva, dove non si stampa alcuna riga di
080040081002         //  dettaglio.)
080050081002         select;
080060081002           when  NOT *in45;
080070081002             wMaxLine = $MaxLine2;
080080081002           when  *in45   and   NOT *in46;
080090090324             wMaxLine = $MaxLine5_pl1;
080100081002           when  *in45   and   *in46;
080110090324             wMaxLine = $MaxLine5_pl2;
080120081002         endsl;
080130081002
080140081002         // Posizionamento iniziale all'interno del foglio A4
080150081002         write  A4prtSP1;
080160081002      /end-free
080170050113      *
080180050113      * Impostazione testi fissi - - - - - - - - - - - - - - - - - - -*
080190050113      *
080200050113     c                   eval      PCLtx1 =
080210050113     c                             'La merce viaggia a rischio e pericolo -
080220050113     c                              del committente. I reclami vanno fatti -
080230050113     c                              per iscritto entro 8 gg. dal ricevimento -
080240050113     c                              delle merce.'
080250050113     c                   eval      PCLtx2 =
080260050113     c                              'Il ritorno di merce deve -
080270050113     c                              essere preventivamente autorizzato -
080280050113     c                              dal Gruppo Arcte. Foro competente -
080290050113     c                              come da condizioni di vendita.'
080300050113      *
080310050113     c                   eval      PCLpx1 =
080320050113     c                             'CONTRIBUTO CONAI ASSOLTO'
080330050217      *
080340130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
080350130315      *  "StampaDDT")
080360050217     c***  KLDV          setll     FIAR8000       (già fatto)
080370050217     c***  KLDV          reade     FIAR8000       (già fatto)
080380050113      *
080390050113      * Reperimento dati, impostazione e stampa DDT mittente  - - - -*
080400050113      *
080410050113if  1c                   dow       NOT %eof(FIAR801L)
080420050113      *
080430050113sel 2c                   select
080440050113      * - Record annullato (non si stampa nulla)
080450050217w   2c                   when      AR8atb  <> *blanks
080460050707      * - Dati di dettaglio: descrizioni generiche
080470050217w   2c                   when      AR8trk   = 'C1'
080480050217     c                   exsr      PrtDdtARCT_C1
080490050217      * - Dati di dettaglio: articolo, descrizioni, taglie, q.tà
080500050217w   2c                   when      AR8trk   = 'C2'
080510050217     c                   exsr      PrtDdtARCT_C2
080520050113      * - Dati di testata: destinatario, cliente di fatturazione
080530050217w   2c                   when      AR8trk   = 'T1'
080540050217     c                   exsr      PrtDdtARCT_T1
080550050113      * - Dati di testata: agente, numero ordine, estremi documento
080560050217w   2c                   when      AR8trk   = 'T2'
080570050217     c                   exsr      PrtDdtARCT_T2
080580050113      * - Dati di fondo: mezzo e causale trasporto, colli, peso
080590050217w   2c                   when      AR8trk   = 'P1'
080600050217     c                   exsr      PrtDdtARCT_P1
080610050113e   2c                   endsl
080620050113      *
080630050113     c     KLDV          reade     FIAR8000
080640050113      *
080650050113e   1c                   enddo
080660050217      *
080670050217      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
080680050217      *
080690050217      * - - Verifica OverFlow
080700050217     c                   exsr      Test_OF
080710050218      *
080720050218      * - - Calcolo la posizione finale del box e di linee separatrici
080730050218     c                   exsr      Posiz_AC
080740050217      *
080750050217      * - - Stampa parte finale DDT
080760050217     c                   write     A4prtAC4
080770050217      *
080780050217      * > Stampa box, linee e testi fissi (no logo)  - - - - - - - - -*
080790081017     c                   if            §AR8cxd = *blanks
080800081017     c                             or  DB0fa4  = 'P'
080810050331     c                   write     A4prtDT0
080820050331     c                   endif
080830050217     c                   write     A4prtAC0A
080840050217     c                   write     A4prtAC0B
080850050217      *
080860050217      * Reimpostazione del vecchio limite x overflow
080870081002      /free
080880081002         select;
080890081002           when  NOT *in45;
080900081002             wMaxLine = $MaxLine2;
080910081002           when  *in45   and   NOT *in46;
080920081002             wMaxLine = $MaxLine2_pl1;
080930081002           when  *in45   and   *in46;
080940081002             wMaxLine = $MaxLine2_pl2;
080950081002         endsl;
080960081002      /end-free
080970050113      *
080980050113     c                   ENDSR
080990050214      *
081000050113      *---------------------------------------------------------------*
081010050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
081020050217      *?Dati di testata: destinatario, cliente di fatturazione       ?*
081030050113      *---------------------------------------------------------------*
081040050217     c     PrtDdtARCT_T1 BEGSR
081050050114      *
081060050114      * > logo
081070050114     c                   eval      §Logo  = 'LOGO' + AR8tmd
081080050113      *
081090051110     c                   clear                   dAR8ARCTT1
081100050707     c                   movel     AR8uni        dAR8ARCTT1
081110050706      *
081120050706      * TEST effettiva numericità dei campi previsti numerici:
081130100513     c                   clear                   w053a
081140050706     c                   eval      WX    =  %len(§ARCTT1a02)
081150100513     c                   eval      w053a =  %subst(dAR8ARCTT1:  7:WX)
081160050706     c                   eval      *in31 =  *off
081170110426     c     c_Digits      checkr    w053a :  WX                            31
081180050706if  1c                   if        *in31
081190050706     c                   clear                   §ARCTT1a02
081200050706e   1c                   endif
081210050706     c                   eval      WX    =  %len(§ARCTT1b02)
081220100513     c                   eval      w053a =  %subst(dAR8ARCTT1:163:WX)
081230050706     c                   eval      *in31 =  *off
081240110426     c     c_Digits      checkr    w053a :  WX                            31
081250050706if  1c                   if        *in31
081260050706     c                   clear                   §ARCTT1b02
081270050706e   1c                   endif
081280050113      *
081290050113      * > destinatario
081300050707     c                   eval      PARCTccd  = §ARCTT1a01
081310050707     c                   eval      PARCTccd1 = §ARCTT1a02
081320050707     c                   eval      PARCTrsd  = §ARCTT1a04
081330050707     c                   eval      PARCTrsd2 = §ARCTT1a05
081340050707     c                   eval      PARCTind  = §ARCTT1a06
081350050707     c                   eval      PARCTcad  = §ARCTT1a07
081360050707     c                   eval      PARCTlod  = §ARCTT1a08
081370050113      * > cliente di fatturazione
081380050707     c                   eval      PARCTccf  = §ARCTT1b01
081390050707     c                   eval      PARCTccf1 = §ARCTT1b02
081400050707     c                   eval      PARCTccf2 = §ARCTT1b03
081410050707     c                   eval      PARCTrsf  = §ARCTT1b04
081420050707     c                   eval      PARCTrsf2 = §ARCTT1b05
081430050707     c                   eval      PARCTinf  = §ARCTT1b06
081440050707     c                   eval      PARCTcaf  = §ARCTT1b07
081450050707     c                   eval      PARCTlof  = §ARCTT1b08
081460050113      * > magazzino spedizione
081470050707     c                   eval      PARCTmag  = §ARCTT1c01
081480050113      *
081490050113     c                   ENDSR
081500050113      *
081510050113      *---------------------------------------------------------------*
081520050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
081530050217      *?Dati di testata: documento                                   ?*
081540050113      *---------------------------------------------------------------*
081550050217     c     PrtDdtARCT_T2 BEGSR
081560050113      *
081570051110     c                   clear                   dAR8ARCTT2
081580050707     c                   movel     AR8uni        dAR8ARCTT2
081590050706      *
081600050706      * TEST effettiva numericità dei campi previsti numerici:
081610100513     c                   clear                   w053a
081620050706     c                   eval      WX    =  %len(§ARCTT2a02)
081630050706     c                                   +  %len(§ARCTT2a03)
081640100513     c                   eval      w053a =  %subst(dAR8ARCTT2: 9:WX)
081650050706     c                   eval      *in31 =  *off
081660110426     c     c_Digits      checkr    w053a :  WX                            31
081670050706if  1c                   if        *in31
081680050706     c                   clear                   §ARCTT2a02
081690050706     c                   clear                   §ARCTT2a03
081700050706e   1c                   endif
081710050113      *
081720050113      * > dati in testata DDT
081730050113      * - - numero e data documento (DDT)
081740050707     c                   eval      PARCTnsr = §ARCTT2a01
081750050707     c                   eval      PARCTnsp = §ARCTT2a02
081760050707     c                   eval      PARCTdsp = §ARCTT2a03
081770050113      *
081780050113      * > Stampa testata DDT - - - - - - - - - - - - - - - - - - - - -*
081790050113      *
081800050113      *   - OverFlow impossibile qui! -
081810050217     c***                exsr      Test_OF
081820050217     c                   exsr      Posiz_AC
081830081017     c                   if            §AR8cxd = *blanks
081840081017     c                             or  DB0fa4  = 'P'
081850050331     c                   write     A4prtDT0
081860050331     c                   endif
081870050217     c                   write     A4prtAC0
081880050217     c*** quante righe?  write     A4prtAC0A
081890050217     c                   write     A4prtAC1A
081900050217     c                   write     A4prtAC1B
081910050113      *
081920050113     c                   ENDSR
081930050214      *
081940050113      *---------------------------------------------------------------*
081950050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
081960050217      *?Dati di dettaglio: descrizioni generiche                     ?*
081970050113      *---------------------------------------------------------------*
081980050217     c     PrtDdtARCT_C1 BEGSR
081990050113      *
082000050114     c                   clear                   A4prtAC2
082010051110     c                   clear                   dAR8ARCTC1
082020050707     c                   movel     AR8uni        dAR8ARCTC1
082030050217      * > dati in note DDT
082040050217      * - - riga descrittiva
082050050707     c                   eval      PARCTdpr  = §ARCTC1a01
082060050113      *
082070050113      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
082080050113      *
082090050113     c                   exsr      Test_OF
082100050114     c                   write     A4prtAC2
082110050113      *
082120050113     c                   ENDSR
082130050113      *
082140050113      *---------------------------------------------------------------*
082150050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
082160050217      *?Dati di dettaglio: descrizione, taglie, qtà                  ?*
082170050113      *---------------------------------------------------------------*
082180050217     c     PrtDdtARCT_C2 BEGSR
082190050113      *
082200050114     c                   clear                   A4prtAC3
082210051110     c                   clear                   dAR8ARCTC2
082220050707     c                   movel     AR8uni        dAR8ARCTC2
082230050706      *
082240050706      * TEST effettiva numericità dei campi previsti numerici:
082250100513     c                   clear                   w053a
082260050706     c                   eval      WX    =  %len(§ARCTC2b00)
082270050706     c                                   +  %len(§ARCTC2b01)
082280050706     c                                   +  %len(§ARCTC2b02)
082290050706     c                                   +  %len(§ARCTC2b03)
082300050706     c                                   +  %len(§ARCTC2b04)
082310050706     c                                   +  %len(§ARCTC2b05)
082320050706     c                                   +  %len(§ARCTC2b06)
082330050706     c                                   +  %len(§ARCTC2b07)
082340050706     c                                   +  %len(§ARCTC2b08)
082350050706     c                                   +  %len(§ARCTC2b09)
082360100513     c                   eval      w053a =  %subst(dAR8ARCTC2:59:WX)
082370050706     c                   eval      *in31 =  *off
082380110426     c     c_Digits      checkr    w053a :  WX                            31
082390050706if  1c                   if        *in31
082400050706     c                   clear                   §ARCTC2b00
082410050706     c                   clear                   §ARCTC2b01
082420050706     c                   clear                   §ARCTC2b02
082430050706     c                   clear                   §ARCTC2b03
082440050706     c                   clear                   §ARCTC2b04
082450050706     c                   clear                   §ARCTC2b05
082460050706     c                   clear                   §ARCTC2b06
082470050706     c                   clear                   §ARCTC2b07
082480050706     c                   clear                   §ARCTC2b08
082490050706     c                   clear                   §ARCTC2b09
082500050706e   1c                   endif
082510050113      *
082520050113      * > dati in dettaglio DDT
082530050113      * - - descrizione articolo
082540050707     c                   eval      PARCTcpr  = §ARCTC2baa
082550050113      * - - taglia 0
082560050707     c                   eval      PARCTt0   = §ARCTC2b00
082570050113      * - - taglia 1
082580050707     c                   eval      PARCTt1   = §ARCTC2b01
082590050113      * - - taglia 2
082600050707     c                   eval      PARCTt2   = §ARCTC2b02
082610050113      * - - taglia 3
082620050707     c                   eval      PARCTt3   = §ARCTC2b03
082630050113      * - - taglia 4
082640050707     c                   eval      PARCTt4   = §ARCTC2b04
082650050113      * - - taglia 5
082660050707     c                   eval      PARCTt5   = §ARCTC2b05
082670050113      * - - taglia 6
082680050707     c                   eval      PARCTt6   = §ARCTC2b06
082690050113      * - - taglia 7
082700050707     c                   eval      PARCTt7   = §ARCTC2b07
082710050113      * - - taglia 8
082720050707     c                   eval      PARCTt8   = §ARCTC2b08
082730050113      * - - quantità
082740050707     c                   eval      PARCTqta  = §ARCTC2b09
082750050113      *
082760050113      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
082770050113      *
082780050113     c                   exsr      Test_OF
082790050114     c                   write     A4prtAC3
082800050113      *
082810050113     c                   ENDSR
082820050113      *
082830050113      *---------------------------------------------------------------*
082840050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
082850050217      *?Dati di fondo: beni, colli, peso, tot.qtà, porto, trasp.     ?*
082860050113      *---------------------------------------------------------------*
082870050217     c     PrtDdtARCT_P1 BEGSR
082880090324      *
082890090324      /free
082900090324           select;
082910090324             when  NOT *in45;
082920090324               wMaxLine = $MaxLine4;
082930090324             when  *in45   and   NOT *in46;
082940090324               wMaxLine = $MaxLine4_pl3;
082950090324             when  *in45   and   *in46;
082960090324               wMaxLine = $MaxLine4_pl4;
082970090324           endsl;
082980090324      /end-free
082990050113      *
083000051110     c                   clear                   dAR8ARCTP1
083010050707     c                   movel     AR8uni        dAR8ARCTP1
083020050706      *
083030050706      * TEST effettiva numericità dei campi previsti numerici:
083040100513     c                   clear                   w053a
083050050706     c                   eval      WX    =  %len(§ARCTP1a03)
083060100513     c                   eval      w053a =  %subst(dAR8ARCTP1:28:WX)
083070050706     c                   eval      *in31 =  *off
083080110426     c     c_Digits      checkr    w053a :  WX                            31
083090050706if  1c                   if        *in31
083100050706     c                   clear                   §ARCTP1a03
083110050706e   1c                   endif
083120050706     c                   eval      WX    =  %len(§ARCTP1b02)
083130050706     c                                   +  %len(§ARCTP1b03)
083140050706     c                                   +  %len(§ARCTP1b04)
083150100513     c                   eval      w053a =  %subst(dAR8ARCTP1:68:WX)
083160050706     c                   eval      *in31 =  *off
083170110426     c     c_Digits      checkr    w053a :  WX                            31
083180050706if  1c                   if        *in31
083190050706     c                   clear                   §ARCTP1b02
083200050706     c                   clear                   §ARCTP1b03
083210050706     c                   clear                   §ARCTP1b04
083220050706e   1c                   endif
083230050113      *
083240050113      * > dati in chiusura DDT
083250050114      * - - costanti di fine doc. e n. borderò
083260050707     c                   eval      PARCTc1   = §ARCTP1a01
083270050707     c                   eval      PARCTc2   = §ARCTP1a02
083280050707     c                   eval      PARCTc3   = §ARCTP1a03
083290050114      * - - costante segue fattura
083300050707     c                   eval      PARCTc4   = §ARCTp1a04
083310050113      * - - aspetto esteriore dei beni
083320050707     c                   eval      PARCTaeb  = §ARCTP1b01
083330050113      * - - numero colli
083340050707     c                   eval      PARCTncl  = §ARCTP1b02
083350050113      * - - peso in kg
083360050707     c                   eval      PARCTpkg  = §ARCTP1b03
083370050113      * - - totale qtà
083380050707     c                   eval      PARCTtqta = §ARCTP1b04
083390050113      * - - causale trasporto
083400050707     c                   eval      PARCTctr  = §ARCTP1c01
083410050113      * - - porto
083420050707     c                   eval      PARCTpto  = §ARCTP1c02
083430050113      * - - trasporto a cura
083440050707     c                   eval      PARCTtra  = §ARCTP1c03
083450050113      * - - trasportatore
083460050707     c                   eval      PARCTvet  = §ARCTP1d01
083470050217     *** *
083480050217     *** * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
083490050217     *** * ?(impostata a fine ciclo di lettura - sr. DDT_Arcte)?
083500050113      *
083510050113      * N.B.: la copia per il destinatario della LDV deve comparire
083520050113      *       nell'ultima pagina del DDT per il cliente (vedi subr.
083530050113      *       "ChiusuraLDV")
083540050113      *
083550050113     c                   ENDSR
083560081002      /free
083570081002       //--------------------------------------------------------------
083580140303       //?Impostazione nuova pagina di DDT Arcte                       ?
083590081002       //--------------------------------------------------------------
083600081002       BEGSR  NewPageAC;
083610081002
083620081002         // Impostazione del nuovo limite x overflow
083630081002         select;
083640081002           when  NOT *in45;
083650081002             wMaxLine = $MaxLine2;
083660081002           when  *in45   and   NOT *in46;
083670090324             wMaxLine = $MaxLine5_pl1;
083680081002           when  *in45   and   *in46;
083690090324             wMaxLine = $MaxLine5_pl2;
083700081002         endsl;
083710081002
083720081002         write  A4prtSP1;
083730081002         write  A4prtAC1B;
083740081002
083750081002       ENDSR;
083760081002      /end-free
083770050113      *
083780050113      *---------------------------------------------------------------*
083790050217      *?Posizionamento di linee e box nel DDT ARCTE                  ?*
083800050113      *---------------------------------------------------------------*
083810050114     c     Posiz_AC      BEGSR
083820080708      *
083830081002      /free
083840081002         // Impostazione posizioni di logo/box
083850081002         // (posizioni di "partenza" diverse a seconda del posizionam.
083860081002         //  iniziale nel foglio A4 attualmente in stampa)
083870081002         select;
083880081002           when  NOT *in45;
083890081002             §PBA =  3.130;
083900081002           when  *in45   and   NOT *in46;
083910081002             §PBA = 0.260;
083920081002           when  *in45   and   *in46;
083930081002             §PBA = 6.010;
083940081002         endsl;
083950081002      /end-free
083960080708     c                   eval      §PBB  =  §PBA + 1.27
083970080708     c                   eval      §LOA  =  §PBA + 0.63
083980050218      *
083990050217      * Impostazione posizioni di box
084000050217if  1c                   if        Page  <= 1
084010050217      *   (posizione di "partenza" su 1ª pagina)
084020081002     c                   eval      §PB1  =  §PBA + 1.38
084030050217x   1c                   else
084040050217      *   (posizione di "partenza" su pagine successive)
084050081002     c                   eval      §PB1  =  §PBA
084060050217e   1c                   endif
084070081002     c                   eval      §PB2  =  §PBA + 5.395
084080050217      * Impostazione posizioni di linee (orizzontali e verticali)
084090050217      *   nella parte superiore del box (testata)
084100080708     c                   eval      §LO1  =  §PB1 + 0.13
084110080708     c                   eval      §LO2  =  §LO1 + 0.13
084120050217      * Impostazione posizioni di linee (orizzontali e verticali)
084130050217      *   nella parte inferiore del box (piede)
084140081002     c                   eval      §LO3  =  §PBA + 5.005
084150081002     c                   eval      §LO4  =  §PBA + 5.135
084160081002     c                   eval      §LO5  =  §PBA + 5.265
084170050217      * Impostazione lunghezza delle linee verticali
084180050217      *   nella parte superiore del box (testata)
084190050217     c                   eval      §LV1  =  §LO1 - §PB1
084200050217     c                   eval      §LV2  =  §LO2 - §LO1
084210050217      * La lunghezza delle linee verticali separatrici nel dettaglio
084220050217      *   dipende dalla fase di stampa: se si sta stampando ancora il
084230050217      *   dettaglio deve essere più lunga, visto che non viene stampa-
084240050217      *   ta la parte finale del DDT...
084250050217if  1c                   if        %subst(AR8trk:1:1) = 'P'
084260050218     c                             and    §PS1        < $LimOF
084270050218     c                             and    CurLineA4   < wMaxLine
084280050217     c                   eval      §LV3  =  §LO3 - §LO2
084290050217x   1c                   else
084300050217     c                   eval      §LV3  =  §PB2 - §LO2
084310050217e   1c                   endif
084320050217     c                   eval      §LV4  =  §LO4 - §LO3
084330050217     c                   eval      §LV5  =  §LO5 - §LO4
084340050113      * Impostazione posizioni dei campi di testo verticali
084350081002     c                   eval      §PR1  =  §PBA + 5.35
084360081002     c                   eval      §PR2  =  §PBA + 5.47
084370050113      *
084380050113     c                   ENDSR
084390050214      *
084400050214      *---------------------------------------------------------------*
084410050217      *?Stampa DDT del mittente Esprinet                             ?*
084420050214      *---------------------------------------------------------------*
084430050214     c     DDT_Esprinet  BEGSR
084440050214      *
084450081002      /free
084460110405         // -?Impostazione del nuovo limite x overflow?
084470110405         //  ?Codesto limite è valido solo per la stampa del dettaglio.?
084480110405         //  ?Per la stampa del "fondo" bolla occorre REimpostarlo ad?
084490110405         //  ?un valore più basso.?
084500110405         //  ?Ovvero: stampo più righe di dettaglio che posso; se alla?
084510110405         //  ?fine delle righe di dettaglio non rimane spazio a suffi-?
084520110405         //  ?cienza per il fondo: prima della stampa del fondo si salta?
084530110405         //  ?alla pagina successiva, dove non si stampa alcuna riga di?
084540110405         //  ?dettaglio.?
084550081002         select;
084560081002           when  NOT *in45;
084570081002             wMaxLine = $MaxLine2;
084580081002           when  *in45   and   NOT *in46;
084590090324             wMaxLine = $MaxLine5_pl1;
084600081002           when  *in45   and   *in46;
084610090324             wMaxLine = $MaxLine5_pl2;
084620081002         endsl;
084630081002
084640110405         // -?Posizionamento iniziale all'interno del foglio A4?
084650081002         write  A4prtSP1;
084660110405
084670110405         // -?Pulizia tipo record?
084680110405         clear  SAVtrk;
084690081002      /end-free
084700050214      *
084710050214      * Impostazione testi fissi - - - - - - - - - - - - - - - - - - -*
084720050214      *
084730050325     ***c                   eval      PCLtx1 =
084740050325     ***c                             'Settimo Torinese - Tel. 011.895.39.29 *-
084750050325     ***c                              Padova - Tel. 049.870.50.40 *-
084760050325     ***c                              Bologna - Tel. 051.324.249 *-
084770050325     ***c                              Osimo - Tel. 071.781.96.75 *-
084780050325     ***c                              Roma - Tel. 06.723.15.43'
084790050325     ***c                   eval      PCLtx2 =
084800050325     ***c                             'Casalnuovo - Tel. 081.522.30.31 *-
084810050325     ***c                              Cinisello B. - Tel. 02.66.04.11.13 *-
084820050325     ***c                              Verona - Tel. 045.823.01.65 *-
084830050325     ***c                              Cagliari - Tel. 070.253.178'
084840050325     c                   eval      PCLtx3 =
084850131017     c                             'CONTRIBUTO AMBIENTALE CONAI ASSOLTO'
084860131017     c*//                           OVE DOVUTO'
084870050214      *
084880050214     c                   eval      PCLpx2 =
084890050214     c                             'IL PRESENTE DOCUMENTO E'' REGOLATO-
084900050214     c                              DALLE CONDIZIONI GENERALI DI VENDI-
084910050214     c                             TA PUBBLICATE SUL SITO INTERNET ALL-
084920050214     c                             ''INDIRIZZO www.esprinet.com CHE SI-
084930050325     c                              INTENDONO ESPRESSAMENTE CONOSCIUTE-
084940050325     c                              ED ACCETTATE'
084950050214      *
084960050214     c                   eval      PCLpx3a =
084970050214     c                             'NB. IN CASO DI RITARDATO PAGAMENTO-
084980050214     c                             , VERRANNO ADDEBITATI'
084990050214     c                   eval      PCLpx3b =
085000050214     c                             'INTERESSI AL TASSO DEL PRIME RATE -
085010050214     c                             MAGGIORATO DEL 2% A'
085020050214     c                   eval      PCLpx3c =
085030050214     c                             'DECORRERE DAL GIORNO SUCCESSIVO AL-
085040050214     c                             LA SCADENZA DELLA FATTURA'
085050050214      *
085060050214     c                   eval      PCLpx4a =
085070050214     c                             'Si prega di verificare la Vs. ragi-
085080050214     c                             one sociale, indirizzo e'
085090050214     c                   eval      PCLpx4b =
085100050214     c                             'partita IVA, dandoci eventuale sol-
085110050214     c                             lecita rettifica scritta.'
085120050214     c***                eval      PCLpx4c = *blanks
085130050214      *
085140050214     c                   eval      PCLpx5a =
085150050214     c                             '* I PRODOTTI IMPORTATI DAGLI U.S.A.-
085160050214     c                              SONO CON SPECIALE'
085170050214     c                   eval      PCLpx5b =
085180050214     c                             'LICENZA CHE PRECLUDE RIESPORTAZIONI-
085190050214     c                              NON AUTORIZZATE.'
085200050214     c***                eval      PCLpx5c = *blanks
085210050214      *
085220050325     ***c                   eval      PCLpx6  =
085230050325     ***c                             'CONTRIBUTO AMBIENTALE CONAI ASSOLTO-
085240050325     ***c                              OVE DOVUTO'
085250050325     c                   eval      PCLpx6  =
085260050325     c                             'LA SOCIETÀ SI IMPEGNA A VERSARE L''-
085270050325     c                             IVA NEI MODI E TERMINI PREVISTI (S-
085280050325     c                             OLIDARIETÀ ART. 60 BIS DPR 633/72-F-
085290050325     c                             INANZIARIA 2005)'
085300050214      *
085310130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
085320130315      *  "StampaDDT")
085330050214     c***  KLDV          setll     FIAR8000       (già fatto)
085340050214     c***  KLDV          reade     FIAR8000       (già fatto)
085350050214      *
085360050214      * Reperimento dati, impostazione e stampa DDT mittente  - - - -*
085370050214      *
085380050214if  1c                   dow       NOT %eof(FIAR801L)
085390050214      *
085400050214sel 2c                   select
085410050214      * - Record annullato (non si stampa nulla)
085420050217w   2c                   when      AR8atb  <> *blanks
085430050214      * - Dati di dettaglio: articolo, descrizione, quantità, ...
085440050217w   2c                   when      AR8trk   = 'C1'
085450050217     c                   exsr      PrtDdtESP1_C1
085460050214      * - Dati di dettaglio: testo (riga libera)
085470050217w   2c                   when      AR8trk   = 'C2'
085480050217     c                   exsr      PrtDdtESP1_C2
085490050214      * - Dati di testata: destinatario, cliente di fatturazione
085500050217w   2c                   when      AR8trk   = 'T1'
085510050217     c                   exsr      PrtDdtESP1_T1
085520050214      * - Dati di testata: estremi documento, riferimenti, ...
085530050217w   2c                   when      AR8trk   = 'T2'
085540050217     c                   exsr      PrtDdtESP1_T2
085550050214      * - Dati di fondo: totali del documento (netto merce, iva, ...)
085560050217w   2c                   when      AR8trk   = 'P1'
085570050217     c                   exsr      PrtDdtESP1_P1
085580050214      * - Dati di fondo: informazioni vettore
085590050217w   2c                   when      AR8trk   = 'P2'
085600050217     c                   exsr      PrtDdtESP1_P2
085610050214e   2c                   endsl
085620110405      *
085630110405     c                   eval      SAVtrk = AR8trk
085640050214      *
085650050214     c     KLDV          reade     FIAR8000
085660050214      *
085670050214e   1c                   enddo
085680050215      *
085690050215      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
085700050215      *
085710050215      * - - Verifica OverFlow
085720050215     c                   exsr      Test_OF
085730050218      *
085740050218      * - - Calcolo la posizione finale del box e di linee separatrici
085750050218     c                   exsr      Posiz_ES
085760050215      *
085770050215      * - - Stampa parte finale DDT
085780050215     c                   write     A4prtES4
085790050215      *
085800050215      * > Stampa box, linee e testi fissi (no logo)  - - - - - - - - -*
085810081017     c                   if            §AR8cxd = *blanks
085820081017     c                             or  DB0fa4  = 'P'
085830050331     c                   write     A4prtDT0
085840050331     c                   endif
085850050215     c                   write     A4prtES0A
085860050215     c                   write     A4prtES0B
085870050214      *
085880050214      * Reimpostazione del vecchio limite x overflow
085890081002      /free
085900081002         select;
085910081002           when  NOT *in45;
085920081002             wMaxLine = $MaxLine2;
085930081002           when  *in45   and   NOT *in46;
085940081002             wMaxLine = $MaxLine2_pl1;
085950081002           when  *in45   and   *in46;
085960081002             wMaxLine = $MaxLine2_pl2;
085970081002         endsl;
085980081002      /end-free
085990050214      *
086000050214     c                   ENDSR
086010050214      *
086020050214      *---------------------------------------------------------------*
086030050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
086040050217      *?Dati di testata: fatturato a, destinazione, cliente          ?*
086050050214      *---------------------------------------------------------------*
086060050217     c     PrtDdtESP1_T1 BEGSR
086070110405      *
086080110405      * - pulizia record a nuova packing-list
086090110405     c                   if            %subst( SAVtrk : 1 : 1 ) = 'P'
086100110405     c                             or  %subst( SAVtrk : 1 : 1 ) = 'C'
086110110405     c                   exsr      sr_ClearESP1
086120110405     c                   endif
086130050214      *
086140050214      * > logo
086150050214     c                   eval      §Logo  = 'LOGO' + AR8tmd
086160050214      *
086170051110     c                   movel(p)  AR8uni        dAR8ESP1T1
086180050214      *
086190050214      * > fatturato a
086200050707     c                   eval      PCrsf  = §A8ESP1T1a
086210050707     c                   eval      PCinf  = §A8ESP1T1b
086220050707if  1c                   if        §A8ESP1T1c > *zeros
086230050707     c                   eval      PCclpf = §A8ESP1T1c
086240050216     c                                    + ' '
086250050707     c                                    + §A8ESP1T1d
086260050216     c                                    + ' '
086270050707     c                                    + §A8ESP1T1e
086280050216x   1c                   else
086290050216     c                   eval      PCclpf = '     '
086300050216     c                                    + ' '
086310050707     c                                    + §A8ESP1T1d
086320050216     c                                    + ' '
086330050707     c                                    + §A8ESP1T1e
086340050216e   1c                   endif
086350050214      * > destinatazione
086360050707     c                   eval      PCrsd  = §A8ESP1T1f
086370050707     c                   eval      PCind  = §A8ESP1T1g
086380050707if  1c                   if        §A8ESP1T1h > *zeros
086390050707     c                   eval      PCclpd = §A8ESP1T1h
086400050216     c                                    + ' '
086410050707     c                                    + §A8ESP1T1i
086420050216     c                                    + ' '
086430050707     c                                    + §A8ESP1T1j
086440050216x   1c                   else
086450050216     c                   eval      PCclpd = '     '
086460050216     c                                    + ' '
086470050707     c                                    + §A8ESP1T1i
086480050216     c                                    + ' '
086490050707     c                                    + §A8ESP1T1j
086500050216e   1c                   endif
086510050214      * > dati in testata DDT
086520050214      * - - codice cliente
086530050707     c                   eval      PCcli  = §A8ESP1T1k
086540050214      * - - codice fiscale
086550050707     c                   eval      PCcf   = §A8ESP1T1l
086560050214      * - - partita iva
086570050707     c                   eval      PCpi   = §A8ESP1T1m
086580050214      *
086590050214     c                   ENDSR
086600050214      *
086610050214      *---------------------------------------------------------------*
086620050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
086630050217      *?Dati di testata: documento, riferimenti.                     ?*
086640050214      *---------------------------------------------------------------*
086650050217     c     PrtDdtESP1_T2 BEGSR
086660110405      *
086670110405      * - pulizia records a nuova packing-list
086680110405     c                   if            %subst( SAVtrk : 1 : 1 ) = 'P'
086690110405     c                             or  %subst( SAVtrk : 1 : 1 ) = 'C'
086700110405     c                   exsr      sr_ClearESP1
086710110405     c                   endif
086720050214      *
086730051110     c                   movel(p)  AR8uni        dAR8ESP1T2
086740050214      *
086750050214      * > dati in testata DDT
086760050214      * - - tipo documento
086770050707     c                   eval      PCtdo  = §A8ESP1T2a
086780050214      * - - data e numero documento (DDT)
086790050707     c                   eval      PCdtd  = §A8ESP1T2b
086800050707     c                   eval      PCnrd  = §A8ESP1T2c
086810050214      * - - riferimenti mittente
086820050707     c                   eval      PCrif  = §A8ESP1T2d
086830050214      * - - condizioni di pagamento
086840050707     c                   eval      PCpag  = §A8ESP1T2e
086850050214      * - - annotazioni
086860050707     c                   eval      PCann  = §A8ESP1T2f
086870050214      * - - agente
086880050707     c                   eval      PCag2  = §A8ESP1T2g
086890050214      *
086900050214      * > Stampa testata DDT - - - - - - - - - - - - - - - - - - - - -*
086910050214      *
086920050214      *   - OverFlow impossibile qui! -
086930050215     c***                exsr      Test_OF
086940050215     c                   exsr      Posiz_ES
086950081017     c                   if            §AR8cxd = *blanks
086960081017     c                             or  DB0fa4  = 'P'
086970050331     c                   write     A4prtDT0
086980050331     c                   endif
086990050214     c                   write     A4prtES0
087000081002     c*** (NON qui:)     write     A4prtES0A
087010081002     c                   write     A4prtSP1
087020050215     c                   write     A4prtES1A
087030050215     c                   write     A4prtES1B
087040050214      *
087050050214     c                   ENDSR
087060050214      *
087070050214      *---------------------------------------------------------------*
087080050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
087090050217      *?Dati di dettaglio: articolo, descrizione, quantità, ...      ?*
087100050214      *---------------------------------------------------------------*
087110050217     c     PrtDdtESP1_C1 BEGSR
087120110405      *
087130110405      * - pulizia records a nuova packing-list
087140110405     c                   if        %subst( SAVtrk : 1 : 1 ) = 'P'
087150110405     c                   exsr      sr_ClearESP1
087160110405     c                   endif
087170050214      *
087180050214     c                   clear                   A4prtES2
087190051110     c                   movel(p)  AR8uni        dAR8ESP1C1
087200050214      *
087210050214      * > dati in dettaglio DDT
087220050214      * - - codice prodotto
087230050707     c                   eval      PCcpr  = §A8ESP1C1a
087240050214      * - - descrizione prodotto
087250050707     c                   eval      PCdpr  = §A8ESP1C1b
087260050214      * - - quantità
087270050707     c                   eval      PCqta  = '      ' + §A8ESP1C1c
087280050214      * - - prezzo unitario
087290050707     c                   eval      PCpru  = §A8ESP1C1d
087300050214      * - - cambio: divisa + valore
087310050707     c                   eval      PCdiv  = §A8ESP1C1e
087320050707     c                   eval      PCcmb  = §A8ESP1C1f
087330050214      * - - % sconto
087340050707     c                   eval      PCprc  = §A8ESP1C1g
087350050214      * - - importo
087360050707     c                   eval      PCimp  = §A8ESP1C1h
087370050214      * - - codice iva
087380050707     c                   eval      PCciv  = §A8ESP1C1i
087390050214      *
087400050214      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
087410050214      *
087420050214     c                   exsr      Test_OF
087430050214     c                   write     A4prtES2
087440050214      *
087450050214     c                   ENDSR
087460050214      *
087470050214      *---------------------------------------------------------------*
087480050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
087490050217      *?Dati di dettaglio: note (riga libera)                        ?*
087500050214      *---------------------------------------------------------------*
087510050217     c     PrtDdtESP1_C2 BEGSR
087520110405      *
087530110405      * - pulizia records a nuova packing-list
087540110405     c                   if        %subst( SAVtrk : 1 : 1 ) = 'P'
087550110405     c                   exsr      sr_ClearESP1
087560110405     c                   endif
087570050214      *
087580050215     c                   clear                   A4prtES3
087590051110     c                   movel(p)  AR8uni        dAR8ESP1C2
087600050214      *
087610050214      * > dati in note DDT
087620050214      * - - riga descrittiva
087630050707     c                   eval      PCrig  = §A8ESP1C2a
087640050214      *
087650050214      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
087660050214      *
087670050214     c                   exsr      Test_OF
087680050214     c                   write     A4prtES3
087690050214      *
087700050214     c                   ENDSR
087710050214      *
087720050214      *---------------------------------------------------------------*
087730050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
087740050217      *?Dati di fondo: totali documento                              ?*
087750050214      *---------------------------------------------------------------*
087760050217     c     PrtDdtESP1_P1 BEGSR
087770050214      *
087780081002      /free
087790081002           select;
087800081002             when  NOT *in45;
087810081002               wMaxLine = $MaxLine6;
087820081002             when  *in45   and   NOT *in46;
087830081002               wMaxLine = $MaxLine6_pl1;
087840081002             when  *in45   and   *in46;
087850081002               wMaxLine = $MaxLine6_pl2;
087860081002           endsl;
087870081002      /end-free
087880051110     c                   movel(p)  AR8uni        dAR8ESP1P1
087890050215      *
087900050215      * > dati in fondo DDT
087910050215      * - - netto merce
087920050707     c                   eval      PCinm  = §A8ESP1P1a
087930050215      * - - spese di trasporto
087940050707     c                   eval      PCist  = §A8ESP1P1b
087950050215      * - - spese imballo
087960050707     c                   eval      PCisi  = §A8ESP1P1c
087970050215      * - - spese bolli
087980050707     c                   eval      PCisb  = §A8ESP1P1d
087990050215      * - - spese amministrative
088000050707     c                   eval      PCisa  = §A8ESP1P1e
088010050215      * - - spese varie
088020050707     c                   eval      PCisv  = §A8ESP1P1f
088030050215      * - - codici iva
088040050707     c                   eval      PCic1  = §A8ESP1P1g
088050050707     c                   eval      PCic2  = §A8ESP1P1m
088060050707     c                   eval      PCic3  = §A8ESP1P1s
088070050215      * - - descrizioni iva
088080050707     c                   eval      PCid1  = §A8ESP1P1h
088090050707     c                   eval      PCid2  = §A8ESP1P1n
088100050707     c                   eval      PCid3  = §A8ESP1P1t
088110050215      * - - imponibili iva: divise
088120050707     c                   eval      PCie1  = §A8ESP1P1i
088130050707     c                   eval      PCie2  = §A8ESP1P1o
088140050707     c                   eval      PCie3  = §A8ESP1P1u
088150050215      * - - imponibili iva: importi
088160050707     c                   eval      PCii1  = §A8ESP1P1j
088170050707     c                   eval      PCii2  = §A8ESP1P1p
088180050707     c                   eval      PCii3  = §A8ESP1P1v
088190050215      * - - aliquote iva
088200050707     c                   eval      PCia1  = §A8ESP1P1k
088210050707     c                   eval      PCia2  = §A8ESP1P1q
088220050707     c                   eval      PCia3  = §A8ESP1P1w
088230050215      * - - imposte iva
088240050707     c                   eval      PCiv1  = §A8ESP1P1l
088250050707     c                   eval      PCiv2  = §A8ESP1P1r
088260050707     c                   eval      PCiv3  = §A8ESP1P1x
088270050215      * - - Totali documento: divise + importi
088280050707     c                   eval      PCtdd  = §A8ESP1P1y
088290050707     c                   eval      PCtdi  = §A8ESP1P1z
088300050707     c                   eval      PCtdd2 = §A8ESP1P11
088310050707     c                   eval      PCtdi2 = §A8ESP1P12
088320050214      *
088330050214     c                   ENDSR
088340050214      *
088350050214      *---------------------------------------------------------------*
088360050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
088370050217      *?Dati di fondo: informazioni vettore                          ?*
088380050214      *---------------------------------------------------------------*
088390050217     c     PrtDdtESP1_P2 BEGSR
088400050214      *
088410081002      /free
088420081002           select;
088430081002             when  NOT *in45;
088440081002               wMaxLine = $MaxLine6;
088450081002             when  *in45   and   NOT *in46;
088460081002               wMaxLine = $MaxLine6_pl1;
088470081002             when  *in45   and   *in46;
088480081002               wMaxLine = $MaxLine6_pl2;
088490081002           endsl;
088500081002      /end-free
088510051110     c                   movel(p)  AR8uni        dAR8ESP1P2
088520050215      *
088530050215      * > dati in fondo DDT
088540050215      * - - resa
088550050707     c                   eval      PCres  = §A8ESP1P2a
088560050215      * - - trasporto a cura del
088570050707     c                   eval      PCtra  = §A8ESP1P2b
088580050215      * - - vettore
088590110426     c*//                eval      PCvet  = $Bartolini
088600110426     c*//                                 + ' - '
088610110426     c*//                                 + $Intest2
088620110426     c*//                                 + ' (BO)'
088630050707     c                   eval      PCvet2 = §A8ESP1P2c
088640050215      * - - causale trasporto
088650050707     c                   eval      PCctr2 = §A8ESP1P2d
088660050215      * - - aspetto esteriore dei beni
088670050707     c                   eval      PCaeb  = §A8ESP1P2e
088680050215      * - - numero colli
088690050707     c                   eval      PCncl2 = §A8ESP1P2f
088700050215      * - - peso complessivo
088710050707     c                   eval      PCpes  = §A8ESP1P2g
088720050215      * - - volume
088730050707     c                   eval      PCvol  = §A8ESP1P2h
088740050215      * - - inizio trasporto: data + ora
088750050707     c                   eval      PCdit  = §A8ESP1P2i
088760050707     c                   eval      PCoit  = §A8ESP1P2j
088770050215     *** *
088780050215     *** * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
088790050215     *** * ?(impostata a fine ciclo di lettura - sr. DDT_Esprinet)?
088800050217      *
088810050217      * N.B.: la copia per il destinatario della LDV deve comparire
088820050217      *       nell'ultima pagina del DDT per il cliente (vedi subr.
088830050217      *       "ChiusuraLDV")
088840050214      *
088850050214     c                   ENDSR
088860081002      /free
088870081002       //--------------------------------------------------------------
088880110405       //?Impostazione nuova pagina di DDT Esprinet                    ?
088890081002       //--------------------------------------------------------------
088900081002       BEGSR  NewPageES;
088910081002
088920110405         // -?Impostazione del nuovo limite x overflow?
088930081002         select;
088940081002           when  NOT *in45;
088950081002             wMaxLine = $MaxLine2;
088960081002           when  *in45   and   NOT *in46;
088970090324             wMaxLine = $MaxLine5_pl1;
088980081002           when  *in45   and   *in46;
088990090324             wMaxLine = $MaxLine5_pl2;
089000081002         endsl;
089010081002
089020081002         write  A4prtSP1;
089030081002         write  A4prtES1B;
089040081002
089050081002       ENDSR;
089060110405
089070110405       //--------------------------------------------------------------
089080110405       //?Pulizia campi a nuova packing-list relativa alla stessa LdV  ?
089090110405       //--------------------------------------------------------------
089100110405       BEGSR  sr_ClearESP1;
089110110405
089120110405         // -?Stampa fondo DDT?
089130110405
089140110405         // - -?Calcolo la posizione finale del box e di linee?
089150110405         //    ?separatrici?
089160110405         exsr  Posiz_ES;
089170110405
089180110405         // - -?Stampa parte finale DDT?
089190110405         write  A4prtES4;
089200110405
089210110405         // - -?Stampa box, linee e testi fissi (no logo)?
089220110405         if  §AR8cxd = *blank  or  DB0fa4 = 'P';
089230110405           write  A4prtDT0;
089240110405         endif;
089250110405         write  A4prtES0A;
089260110405         write  A4prtES0B;
089270110405
089280110405         // -?Pulizia record?
089290110405         select;
089300110405           when  %subst( AR8trk : 1 : 1 ) = 'T';
089310110405             clear  A4prtES0;
089320110405             clear  A4prtES0A;
089330110405             clear  A4prtES0B;
089340110405             clear  A4prtES1A;
089350110405             clear  A4prtES1B;
089360110405             clear  A4prtES2;
089370110405             clear  A4prtES3;
089380110405             clear  A4prtES4;
089390110405           when  %subst( AR8trk : 1 : 1 ) = 'C';
089400110405             clear  A4prtES2;
089410110405             clear  A4prtES3;
089420110405             clear  A4prtES4;
089430110405         endsl;
089440110405
089450110405         // -?Impostazione testi fissi?
089460131017         //PCLtx3 = 'CONTRIBUTO AMBIENTALE CONAI ASSOLTO OVE DOVUTO';
089470131017         PCLtx3 = 'CONTRIBUTO AMBIENTALE CONAI ASSOLTO';
089480110405
089490110405         PCLpx2 = 'IL PRESENTE DOCUMENTO E'' REGOLATO DALLE +
089500110405                   CONDIZIONI GENERALI DI VENDITA PUBBLICATE SUL SITO +
089510110405                   INTERNET ALL''INDIRIZZO www.esprinet.com CHE SI +
089520110405                   INTENDONO ESPRESSAMENTE CONOSCIUTE ED ACCETTATE';
089530110405
089540110405         PCLpx3a = 'NB. IN CASO DI RITARDATO PAGAMENTO, VERRANNO +
089550110405                    ADDEBITATI';
089560110405         PCLpx3b = 'INTERESSI AL TASSO DEL PRIME RATE MAGGIORATO +
089570110405                    DEL 2% A';
089580110405         PCLpx3c =
089590110405                   'DECORRERE DAL GIORNO SUCCESSIVO ALLA SCADENZA +
089600110405                    DELLA FATTURA';
089610110405
089620110405         PCLpx4a = 'Si prega di verificare la Vs. ragione sociale +
089630110405                    indirizzo e';
089640110405         PCLpx4b = 'partita IVA, dandoci eventuale sollecita +
089650110405                    rettifica scritta.';
089660110405
089670110405         PCLpx5a = '* I PRODOTTI IMPORTATI DAGLI U.S.A. SONO CON +
089680110405                    SPECIALE';
089690110405         PCLpx5b = 'LICENZA CHE PRECLUDE RIESPORTAZIONI NON +
089700110405                    AUTORIZZATE.';
089710110405
089720110405         PCLpx6  = 'LA SOCIETÀ SI IMPEGNA A VERSARE L''IVA NEI MODI +
089730110405                    E TERMINI PREVISTI (SOLIDARIETÀ ART. 60 BIS DPR +
089740110405                    633/72-FINANZIARIA 2005)';
089750110405
089760110405         // -?Salto Pagina?
089770110405         exsr  Test_OF;
089780110405
089790110405         // -?Re-Impostazione limiti di overflow?
089800110405         select;
089810110405           when  NOT *in45;
089820110405             wMaxLine = $MaxLine2;
089830110405           when  *in45   and   NOT *in46;
089840110405             wMaxLine = $MaxLine5_pl1;
089850110405           when  *in45   and   *in46;
089860110405             wMaxLine = $MaxLine5_pl2;
089870110405         endsl;
089880110405
089890110405         // -?Posizionamento iniziale all'interno del foglio A4?
089900110405         write  A4prtSP1;
089910110405
089920110405       ENDSR;
089930081002      /end-free
089940050214      *
089950050214      *---------------------------------------------------------------*
089960050217      *?Posizionamento di linee e box nel DDT ESPRINET               ?*
089970050214      *---------------------------------------------------------------*
089980050215     c     Posiz_ES      BEGSR
089990080708      *
090000081002      /free
090010081002         // Impostazione posizioni di logo/box
090020081002         // (posizioni di "partenza" diverse a seconda del posizionam.
090030081002         //  iniziale nel foglio A4 attualmente in stampa)
090040081002         select;
090050081002           when  NOT *in45;
090060081006             §PBA =  3.130;
090070081006             //§PBA =  3.255;        (più in basso di una riga)
090080081002           when  *in45   and   NOT *in46;
090090081002             §PBA = 0.255;
090100081002           when  *in45   and   *in46;
090110081002             §PBA = 6.005;
090120081002         endsl;
090130081002      /end-free
090140080708      * Impostazione posizioni di linee orizzontali
090150080708     c                   eval      §LOA  =  §PBA + 0.76
090160080708     c                   eval      §LOB  =  §LOA + 0.25
090170080708     c                   eval      §LOC  =  §LOB + 0.12
090180080708      * Impostazione posizioni di linee verticali
090190080708     c                   eval      §LVA  =  §LOA - 0.15
090200050214      *
090210050214      * Impostazione posizioni di box
090220050215if  1c                   if        Page  <= 1
090230050214      *   (posizione di "partenza" su 1ª pagina)
090240081002     c                   eval      §PB1  =  §PBA + 1.270
090250050214x   1c                   else
090260050214      *   (posizione di "partenza" su pagine successive)
090270081002     c                   eval      §PB1  =  §PBA + 0.020
090280050214e   1c                   endif
090290110405if  1c*//                if        %subst(AR8trk:1:1) = 'P'
090300110405if  1c                   if        %subst(SAVtrk:1:1) = 'P'
090310050331     c                             and    CurLineA4   < wMaxLine
090320081002     c                   eval      §PB2  =  §PBA + 5.360
090330050215x   1c                   else
090340050215     c                   eval(h)   §PB2  =  CurLineA4 / 8 + 0.05
090350081002     c                   if        §PB2  <  (§PBA + 5.320)
090360081002     c                   eval      §PB2  =  (§PBA + 5.320)
090370050331     c                   endif
090380050215e   1c                   endif
090390050215      * Impostazione posizioni di linee (orizzontali e verticali)
090400050214      *   nella parte superiore del box (testata)
090410050215     c                   eval      §LO1  =  §PB1 + 0.25
090420050214     c                   eval      §LO2  =  §LO1 + 0.11
090430050214      *   nella parte inferiore del box (piede)
090440110405if  1c*//                if        %subst(AR8trk:1:1) = 'P'
090450110405if  1c                   if        %subst(SAVtrk:1:1) = 'P'
090460050331     c                             and    CurLineA4   < wMaxLine
0904700810027.78 c                   eval      §LO3  =  §PBA + 3.900
0904800502158.00 c                   eval      §LO4  =  §LO3 + 0.24
0904900502158.39 c                   eval      §LO5  =  §LO4 + 0.36
0905000502158.52 c                   eval      §LO6  =  §LO5 + 0.14
0905100502148.64 c                   eval      §LO7  =  §LO6 + 0.12
0905200502148.77 c                   eval      §LO8  =  §LO7 + 0.13
0905300502148.89 c                   eval      §LO9  =  §LO8 + 0.12
0905400807088.97 c                   eval      §LO0  =  §LO9 + 0.10
090550050215e   1c                   endif
090560050214      * La lunghezza delle linee verticali separatrici nel dettaglio
090570050214      *   dipende dalla fase di stampa: se si sta stampando ancora il
090580050214      *   dettaglio deve essere più lunga, visto che non viene stampa-
090590050214      *   ta la parte finale del DDT...
090600110405if  1c*//                if        %subst(AR8trk:1:1) = 'P'
090610110405if  1c                   if        %subst(SAVtrk:1:1) = 'P'
090620050331     c                             and    CurLineA4   < wMaxLine
0906300502162.25 c                   eval      §LV1  =  §LO3 - §LO2
090640050215x   1c                   else
090650050216     c                   eval      §LV1  =  §PB2 - §LO2
090660050215e   1c                   endif
090670050214      *
090680050214      * Impostazione posizioni di campi di testo
090690050214      * - PCLtx1/tx2 (verticali)
0907000502179.10 c                   eval      §PR1  =  §PB2 - 0.07
090710050214      * - PCLpx2 ("Il presente documento...")
0907200502178.95 c                   eval      §PS2  =  §LO9 + 0.07
090730050214      * - PCLpx3a/px4a/px5a
0907400807089.04 c                   eval      §PS3  =  §LO0 + 0.07
090750050214      * - PCLpx3b/px4b/px5b
0907600502179.09 c                   eval      §PS4  =  §PS3 + 0.07
090770050214      * - PCLpx3c/px4c/px5c
0907800502179.14 c                   eval      §PS5  =  §PS4 + 0.07
090790050214      * - PCLpx6 ("Contributo ambientale CONAI...")
0908000502179.25 c                   eval      §PS6  =  §PB2 + 0.08
090810050214      *
090820050214     c                   ENDSR
090830041207      *
090840041207      *---------------------------------------------------------------*
090850050217      *?Stampa DDT del mittente (generico con logo)                  ?*
090860041207      *---------------------------------------------------------------*
090870041207     c     DDT_GenericoL BEGSR
090880041207      *
090890081003      /free
090900081003         // Impostazione del nuovo limite x overflow
090910081003         select;
090920081003           when  NOT *in45;
090930081003             wMaxLine = $MaxLine2;
090940081003           when  *in45   and   NOT *in46;
090950081003             wMaxLine = $MaxLine2_pl1;
090960081003           when  *in45   and   *in46;
090970081003             wMaxLine = $MaxLine2_pl2;
090980081003         endsl;
090990081003
091000081003         // Posizionamento iniziale all'interno del foglio A4
091010081003         write  A4prtSP1;
091020081006
091030081006         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
091040081006           write  A4prtNUL;
091050081006         endif;
091060081003      /end-free
091070041207      *
091080041207      * > logo
091090041207     c                   eval      §Logo  = 'LOGO' + AR8tmd
091100041207      *
091110081017     c                   if            §AR8cxd = *blanks
091120081017     c                             or  DB0fa4  = 'P'
091130080709     c                   write     A4prtDT0
091140080709     c                   endif
091150080709      *
091160041209      * Imposto posizione per logo
091170081003      /free
091180081003         select;
091190081003           when  NOT *in45;
091200081003             exsr  Rep_§LO2;
091210081003             §PB1 = §LO2 + (2 / 8);
091220081003           when  *in45   and   NOT *in46;
091230081003             §PB1 = 0.250;
091240081003           when  *in45   and   *in46;
091250081003             §PB1 = 6.000;
091260081003         endsl;
091270081003      /end-free
091280041209      *   Stampa del logo
091290041209     c                   write     A4prtDB0
091300041207      *
091310041207      * Reperimento dati e stampa DDT mittente - - - - - - - - - - - -*
091320041207      *
091330130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
091340130315      *  "StampaDDT")
091350041207     c***  KLDV          setll     FIAR8000       (già fatto)
091360041207     c***  KLDV          reade     FIAR8000       (già fatto)
091370041207      *
091380041207if  1c                   dow       NOT %eof(FIAR801L)
091390041207      *
091400041207if  2c                   if        AR8atb = *blanks
091410041207      *
091420041207      * > Stampa dei dati
091430041207     c                   exsr      Test_OF
091440041207sel 3c                   select
091450041207      * - Dati di dettaglio: testo in font a dimensione standard
091460041207w   3c                   when      AR8trk = 'C1'
091470051110     c                   movel(p)  AR8uni        PCddt
091480041207     c                   write     A4prtDC1
091490041207      * - Dati di testata:   testo in font a dimensioni massime
091500041207w   3c                   when      AR8trk = 'T1'
091510051110     c                   movel(p)  AR8uni        PCddt
091520041207     c                   write     A4prtDT1
091530041207      * - Dati di testata:   testo in font a dimensioni grandi
091540041207w   3c                   when      AR8trk = 'T2'
091550051110     c                   movel(p)  AR8uni        PCddt
091560041207     c                   write     A4prtDT2
091570041209      * - Linea separatrice
091580041209w   3c                   when      AR8trk = '--'
091590041209      *   - Stampa di una riga vuota per la riga separatrice
091600041209     c                   write     A4prtNUL
091610041209      *   - Stampa della linea orizzontale separatrice
091620090213     c                   eval(h)   §LO1   = (CurLineA4 / 8) - 0.050
091630041209     c                   write     A4prtLO
091640041207e   3c                   endsl
091650041207e   2c                   endif
091660041207      *
091670041207     c                   eval      SAVtrk = AR8trk
091680041207     c     KLDV          reade     FIAR8000
091690041207      *
091700041207e   1c                   enddo
091710080708      *
091720081003      /free
091730081003         select;
091740081003           when  NOT *in45;
091750081003             wMaxLine = $MaxLine2;
091760081003           when  *in45   and   NOT *in46;
091770081003             wMaxLine = $MaxLine2_pl1;
091780081003           when  *in45   and   *in46;
091790081003             wMaxLine = $MaxLine2_pl2;
091800081003         endsl;
091810081003      /end-free
091820041207      *
091830041207     c                   ENDSR
091840040607      *
091850040607      *---------------------------------------------------------------*
091860050217      *?Stampa DDT del mittente (generico senza logo)                ?*
091870040607      *---------------------------------------------------------------*
091880040607     c     DDT_Generico  BEGSR
091890040607      *
091900081003      /free
091910081003         // Impostazione del nuovo limite x overflow
091920081003         select;
091930081003           when  NOT *in45;
091940081003             wMaxLine = $MaxLine2;
091950081003           when  *in45   and   NOT *in46;
091960081003             wMaxLine = $MaxLine2_pl1;
091970081003           when  *in45   and   *in46;
091980081003             wMaxLine = $MaxLine2_pl2;
091990081003         endsl;
092000081003
092010081003         // Posizionamento iniziale all'interno del foglio A4
092020081003         write  A4prtSP1;
092030081006
092040081006         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
092050081006           write  A4prtNUL;
092060081006         endif;
092070081003      /end-free
092080040607      *
092090081017     c                   if            §AR8cxd = *blanks
092100081017     c                             or  DB0fa4  = 'P'
092110050331     c                   write     A4prtDT0
092120050331     c                   endif
092130040607      *
092140040607      * Reperimento dati e stampa DDT mittente - - - - - - - - - - - -*
092150040607      *
092160130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
092170130315      *  "StampaDDT")
092180040607     c***  KLDV          setll     FIAR8000       (già fatto)
092190040607     c***  KLDV          reade     FIAR8000       (già fatto)
092200040607      *
092210040607if  1c                   dow       NOT %eof(FIAR801L)
092220040607      *
092230040607if  2c                   if        AR8atb = *blanks
092240051110     c                   movel(p)  AR8uni        PCddt
092250040607     c                   exsr      Test_OF
092260040607sel 3c                   select
092270040607      * - Dati di dettaglio: testo in font a dimensione standard
092280040607w   3c                   when      AR8trk = 'C1'
092290040607     c                   write     A4prtDC1
092300040607      * - Dati di testata:   testo in font a dimensioni massime
092310040607w   3c                   when      AR8trk = 'T1'
092320040607     c                   write     A4prtDT1
092330040607      * - Dati di testata:   testo in font a dimensioni grandi
092340040607w   3c                   when      AR8trk = 'T2'
092350040607     c                   write     A4prtDT2
092360040607e   3c                   endsl
092370040607e   2c                   endif
092380040607      *
092390040607     c     KLDV          reade     FIAR8000
092400040607      *
092410040607e   1c                   enddo
092420080708      *
092430081003      /free
092440081003         select;
092450081003           when  NOT *in45;
092460081003             wMaxLine = $MaxLine2;
092470081003           when  *in45   and   NOT *in46;
092480081003             wMaxLine = $MaxLine2_pl1;
092490081003           when  *in45   and   *in46;
092500081003             wMaxLine = $MaxLine2_pl2;
092510081003         endsl;
092520081003      /end-free
092530040607      *
092540040607     c                   ENDSR
092550081017
092560081017      /free
092570081017       //--------------------------------------------------------------
092580140303       //?Impostazione nuova pagina di DDT Generico (con/senza logo)   ?
092590081017       //--------------------------------------------------------------
092600081017       BEGSR  NewPageSTD;
092610081017
092620081017         // Posizionamento iniziale nella nuova pagina
092630081017         //   (già fatto con A4prtSP0 se DB0FA4 = "P")
092640081017         if  DB0fa4 <> 'P';
092650081017           write  A4prtSP1;
092660081017           if  DB0fa4 = '1'   or   DB0fa4 = 'S';
092670081017             write  A4prtNUL;
092680081017           endif;
092690081017         endif;
092700081017
092710081017         // Stampa "COPIA PER IL DESTINATARIO"
092720081017         if  §AR8cxd = *blank   or   DB0fa4 = 'P';
092730081017           write  A4prtDT0;
092740081017         endif;
092750081017
092760081017         // Impostazione del nuovo limite x overflow
092770081017         select;
092780081017           when  NOT *in45;
092790081017             wMaxLine = $MaxLine2;
092800081017           when  *in45   and   NOT *in46;
092810081017             wMaxLine = $MaxLine2_pl1;
092820081017           when  *in45   and   *in46;
092830081017             wMaxLine = $MaxLine2_pl2;
092840081017         endsl;
092850081017
092860081017       ENDSR;
092870081017      /end-free
092880030515      *
092890030515      *---------------------------------------------------------------*
092900050217      *?Stampa del record "generico" (PRTGEN)                        ?*
092910030515      *---------------------------------------------------------------*
092920030515     c     StampaGEN     BEGSR
092930030515      *
092940030515      * Controllo se è stato raggiunto l'overflow nel formato in uso
092950030515     c                   exsr      Test_OF
092960030515      *
092970030911      * Stampo la riga PRTGEN
092980030911      * Se sto stampando una "testata" (*in39) salto una riga prima
092990030911      *   ed una riga dopo (per spaziare)
093000030911if  1c                   if        *in39 = *on
093010030515     c                   write     A4prtNUL
093020030911e   1c                   endif
093030030911     c                   write     A4prtGEN
093040030911if  1c                   if        *in39 = *on
093050030911     c                   write     A4prtNUL
093060030911e   1c                   endif
093070030515      *
093080030515     c                   ENDSR
093090030515      *
093100030515      *---------------------------------------------------------------*
093110050511      *?Prepara elenco per lista - SegnaColli Non Sequenziali (test.)?*
093120030515      *---------------------------------------------------------------*
093130030515     c     ListaScNS     BEGSR
093140030515      *
093150030515     c                   clear                   XX
093160030515      *
093170030515      * Nel foglio in formato "A5" devo solo riempire gli appositi
093180030715      *   campi "Note e Particolarità" (nessun salto riga/pagina è
093190030715      *   richiesto)
093200050513     c                   if           *in69       = *on
093210050513     c                             or $A4_x_ScNS  = *off
093220030515     c                   eval      WelSNS = $elSNSxA5
093230030515     c                   goto      ScNS_A5
093240030515     c                   endif
093250030515      *
093260030515      * Determino il numero di segnacolli da elencare per riga nel
093270030515      *   formato pagina "A4" (dove ci sono righe a parte).
093280030515      *   Dipende dal carattere specificato nel prtf: più grande è
093290030515      *   il carattere => meno segnacolli ci stanno sulla riga !
093300030911      *   A cpi 15 -come ora- ci sta tutta la schiera; a cpi 12 no !
093310030515     c                   eval      WelSNS = %elem($SC1)
093320030515      *
093330030515      * Spengo gli indicatori che condizionano in stampa le indicazioni
093340030515      *   per DPD / FedEx / Bancali-Supermercati-Autotrasportatori
093350030515      *   (se sono già state stampate nella 1ª pagina non devono più
093360030515      *   esserlo)
093370030910      *   (se non sono ancora state stampate, dovranno esserlo poi)
093380030515if  1c                   if        wPage > 1
093390050513     c                   movea     *in(70)       SAVin7x
093400040223     c                   movea     '0000'        *in(70)
093410030515e   1c                   endif
093420030515      *
093430030515      * Calcolo il numero di righe da saltare
093440030515     c                   eval      wMinLine = CurLineA4
093450030515if  1c                   if        §LO2     > §LO1
093460030515      * - se stampato almeno un box
093470030515     c                   eval(h)   wMinLine = (§LO2 * 8) + 2
093480030515x   1c                   else
093490030515      * - se non stampato alcun box
093500080730      /free
093510080729       // - Calcola la posizione della riga in 1/8" di pollice
093520080729       //   da quella attuale (in 1/9" di pollice)
093530080730         eval(h) wMinLine = (CurLineA4 * 8 / 9) + 1;
093540080730      /end-free
093550030515e   1c                   endif
093560030515      * ...e scarto il numero righe necessario
093570030515      *   (se non sono già da pagina nuova;
093580030515      *    se sono da pagina nuova: ci penserà poi la subr. "Test_OF"
093590030515      *       alla prima riga in stampa)
093600030515if  1c                   if        wMinLine  < wMaxLine
093610080729do  2c                   dou       CurLineA4 >= wMinLine
093620030515     c                   write     A4prtNUL
093630030515e   2c                   enddo
093640030515e   1c                   endif
093650030515      *
093660030515     c     ScNS_A5       tag
093670030515      *
093680030515      * Imposto l'indicatore per la stampa del titolo relativo all'
093690030515      *   elenco che segue (condiziona attributi di stampa diversi)
093700030515     c                   eval      *in39  = *on
093710050511      *
093720050511     c                   exsr      ListaScNSd
093730050511      *
093740050511     c                   ENDSR
093750050511      *
093760050511      *---------------------------------------------------------------*
093770050511      *?Prepara elenco per lista - SegnaColli Non Sequenziali (dett.)?*
093780050511      *---------------------------------------------------------------*
093790050511     c     ListaScNSd    BEGSR
093800030515      *
093810030515      * Stampo elenco dei segnacolli non sequenziali:
093820071008if  1c                   IF             DB0tbo  = 'P'
093830071008     c                             or   DB0tbo  = '8'
093840030515      * - per bolle in Partenza
093850030515     c                   exsr      ScNS_BLT
093860030515x   1c                   ELSE
093870081007      * - per bolle in Arrivo
093880030515     c                   exsr      ScNS_ART
093890030515e   1c                   ENDIF
093900030515      *
093910030515      * Alla fine: stampo eventuali segnacolli in elenco (non stampati)
093920030515      *              in foglio di formato "A4"
093930030515      *            imposto eventuali segnacolli in elenco (non messi in
093940030515      *              stampa tra le particolarità) in foglio "A5"
093950030515if  1c                   if        XX     > *zeros
093960030515     c                   eval      XX     = *hival
093970030515     c                   exsr      StampaScNS
093980030515e   1c                   endif
093990030910      *
094000030910      * Se sto stampando su formato pagina "A4": reimposto *in70, *in71
094010030910      *   e *in72 (spenti prima della stampa dell'elenco dei segnacolli
094020030910      *   non sequenziali) perchè saranno utilizzati per il posiziona-
094030030910      *   mento in nuova pagina (nella subr. "Test_OF") e per la stampa
094040040223      *   dei box per DPD, FedEx e quelli da FIAR5.
094050050513if  1c                   if             not *in69
094060030910     c                             and  not *in70
094070080922     c*** DPD non più in A4:       and  not *in71
094080030910     c                             and  not *in72
094090080912     c*** EEX non più gestita:     and  not *in73
094100030910     c                   movea     SAVin7x       *in(70)
094110030910e   1c                   endif
094120030515      *
094130030515     c                   ENDSR
094140030515      *
094150030515      *---------------------------------------------------------------*
094160050217      *?Prepara elenco SegnaColli Non Sequenziali da FNBLT (partenza)?*
094170030515      *---------------------------------------------------------------*
094180030515     c     ScNS_BLT      BEGSR
094190030515      *
094200030515if  1c                   if        not %open(FNBLT01L)
094210030515     c                   open      FNBLT01L
094220030515e   1c                   endif
094230030515      *
094240030515      * memorizzo l'ultimo numero di segnacollo
094250070514if  1c                   if        $Mamma = *off
094260040607     c     KLDV          setgt     FNBLT000
094270040607     c     KLDV          readpe    FNBLT000
094280070514x   1c                   else
094290070514     c     KLBL          setgt     FNBLT000
094300070514     c     KLBL          readpe    FNBLT000
094310070514e   1c                   endif
094320030515if  1c                   if        NOT %eof(FNBLT01L)
094330080922     c                   eval      %subst(Pnsx : 8) = ','
094340080922     c                                              + %editc(BLTnsc : 'Z')
094350030515e   1c                   endif
094360030515      *
094370030515      * memorizzo il primo numero di segnacollo
094380070514if  1c                   if        $Mamma = *off
094390040607     c     KLDV          setll     FNBLT000
094400040607     c     KLDV          reade     FNBLT000
094410070514x   1c                   else
094420070514     c     KLBL          setll     FNBLT000
094430070514     c     KLBL          reade     FNBLT000
094440070514e   1c                   endif
094450030515if  1c                   if        NOT %eof(FNBLT01L)
094460080922     c                   eval      %subst(Pnsx : 1 : 7) =
094470080922     c                                 %editc(BLTnsc : 'Z')
094480030515e   1c                   endif
094490030515      *
094500030515      * se ci sono solo 2 segnacolli non sequenziali:
094510030515      *   stanno già al posto del range - basta non stampare il msg
094520030515      *   ("NON SEQUENZIALI")
094530030515if  1c                   if        DB0ncl <= 2
094540050516     c                   eval      *in39  =  *off
094550030515     c                   goto      EndScNS_BLT
094560030515e   1c                   endif
094570030515      *
094580030515      * ciclo di lettura per il recupero di tutti i numeri di
094590030515      *   segnacollo
094600030515do  1c                   DOW       NOT %eof(FNBLT01L)
094610030515     c                   eval      Wnsc   =  BLTnsc
094620030515     c                   exsr      StampaScNS
094630070514if  2c                   if        $Mamma = *off
094640040607     c     KLDV          reade     FNBLT000
094650070514x   2c                   else
094660070514     c     KLBL          reade     FNBLT000
094670070514e   2c                   endif
094680030515e   1c                   ENDDO
094690030515      *
094700030515     c     EndScNS_BLT   ENDSR
094710030515      *
094720030515      *---------------------------------------------------------------*
094730050217      *?Prepara elenco SegnaColli Non Sequenziali da FNART (arrivo)  ?*
094740030515      *---------------------------------------------------------------*
094750030515     c     ScNS_ART      BEGSR
094760030515      *
094770070319if  1c                   if        not %open(FNART01L)
094780070319     c                   open      FNART01L
094790070319e   1c                   endif
094800030515      *
094810030515      * memorizzo l'ultimo numero di segnacollo
094820070514if  1c                   if        $Mamma = *off
094830040607     c     KLDV          setgt     FNART000
094840040607     c     KLDV          readpe    FNART000
094850070514x   1c                   else
094860070514     c     KLBL          setgt     FNART000
094870070514     c     KLBL          readpe    FNART000
094880070514e   1c                   endif
094890030515if  1c                   if        NOT %eof(FNART01L)
094900080922     c                   eval      %subst(Pnsx : 8) = ','
094910080922     c                                              + %editc(ARTnsc : 'Z')
094920030515e   1c                   endif
094930030515      *
094940030515      * memorizzo il primo numero di segnacollo
094950070514if  1c                   if        $Mamma = *off
094960040607     c     KLDV          setll     FNART000
094970040607     c     KLDV          reade     FNART000
094980070514x   1c                   else
094990070514     c     KLBL          setll     FNART000
095000070514     c     KLBL          reade     FNART000
095010070514e   1c                   endif
095020030515if  1c                   if        NOT %eof(FNART01L)
095030080922     c                   eval      %subst(Pnsx : 1 : 7) =
095040080922     c                                 %editc(ARTnsc : 'Z')
095050030515e   1c                   endif
095060030515      *
095070030515      * se ci sono solo 2 segnacolli non sequenziali:
095080030515      *   stanno già al posto del range - basta non stampare il msg
095090030515      *   ("NON SEQUENZIALI")
095100030515if  1c                   if        DB0ncl <= 2
095110050516     c                   eval      *in39  =  *off
095120030515     c                   goto      EndScNS_ART
095130030515e   1c                   endif
095140030515      *
095150030515      * ciclo di lettura per il recupero di tutti i numeri di
095160030515      *   segnacollo
095170030515do  1c                   DOW       NOT %eof(FNART01L)
095180030515     c                   eval      Wnsc   =  ARTnsc
095190030515     c                   exsr      StampaScNS
095200070514if  2c                   if        $Mamma = *off
095210040607     c     KLDV          reade     FNART000
095220070514x   2c                   else
095230070514     c     KLBL          reade     FNART000
095240070514e   2c                   endif
095250030515e   1c                   ENDDO
095260030515      *
095270030515     c     EndScNS_ART   ENDSR
095280030515      *
095290030515      *---------------------------------------------------------------*
095300050217      *?Stampa elenco SegnaColli Non Sequenziali in fogli "A4" o "A5"?*
095310030515      *---------------------------------------------------------------*
095320030515     c     StampaScNS    BEGSR
095330030515      *
095340030515      * In prima riga stampo la "intestazione"
095350030515if  1c                   if        *in39
095360050513if  2c                   if        *in69   or
095370050513     c                             $A4_x_ScNS = *off
095380050513      * - foglio "A5" oppure
095390050513      *   foglio "A4" in "NOTE E PARTICOLARITA'"
095400030515     c                   eval      YY      = YY  + 1
095410030515     c                   eval      $NP(yy) = C_ScNS + ':'
095420030515x   2c                   else
095430030515      * - foglio "A4"
095440030515     c                   eval      Prig  = C_ScNS
095450030515     c                                   + '         (dal '
095460080922     c                                   + %trim( %subst(Pnsx: 1: 7) )
095470030515     c                                   + ' al '
095480080922     c                                   + %trim( %subst(Pnsx: 9: 7) )
095490030515     c                                   + ')'
095500030515     c                   exsr      StampaGEN
095510030515e   2c                   endif
095520080922     c                   eval      Pnsx  =  'NON SEQUENZIALI'
095530030515     c                   eval      *in39 = *off
095540030515     c                   clear                   XX
095550030515     c                   clear                   $SC1
095560030515     c                   clear                   $SC2
095570030515e   1c                   endif
095580030515      *
095590030515      * Al riempimento della riga: stampo la parte di elenco segnacolli
095600030515if  1c                   if        XX  >= WelSNS
095610030515     c                   eval      $SC2 = %editc($SC1 : 'Z')
095620050513if  2c                   if        *in69   or
095630050513     c                             $A4_x_ScNS = *off
095640050513      * - foglio "A5"  oppure
095650050513      *   foglio "A4" in "NOTE E PARTICOLARITA'"
095660030515     c                   eval      YY      = YY  + 1
095670030515     c                   movea(p)  $SC2          $NP(yy)
095680030515x   2c                   else
095690030515      * - foglio "A4"
095700030911     c                   movea(p)  $SC2          Prig
095710030515     c                   exsr      StampaGEN
095720030515e   2c                   endif
095730030515if  2c                   if        XX  <  *hival
095740030515     c                   clear                   XX
095750030515e   2c                   endif
095760030515     c                   clear                   $SC1
095770030515     c                   clear                   $SC2
095780030515e   1c                   endif
095790030515      *
095800030515      * Aggiorno la nuova parte di elenco segnacolli
095810030515      *  (se non richiesta la stampa degli ultimi segnacolli non
095820030515      *   sequenziali residui - memorizzati ma non ancora stampati)
095830030515if  1c                   if        XX  <  *hival
095840030515     c                   eval      XX = xx  + 1
095850030515     c                   eval      $SC1(xx) = Wnsc
095860030515e   1c                   endif
095870030515      *
095880030515     c                   ENDSR
095890040607      *
095900040607      *---------------------------------------------------------------*
095910050217      *?Stampa della parte finale della Lettera di Vettura           ?*
095920040607      *---------------------------------------------------------------*
095930040607     c     ChiusuraLDV   BEGSR
095940080605      *
095950080605      * Posizionamento sul foglio A4 in base al n° pagina in stampa
095960130418     c                   eval      *in33 = *on
095970130418     c                   exsr      Posiz_Box
095980070307      *
095990070307      * Il tagliandino "Copia per il Destinatario" va ora stampato SE:
096000070307      * · spedizione NON in stato "lasciato avviso"   oppure
096010070307      * · spedizione in stato "lasciato avviso", ma P.A. con fattura
096020070307      *   immediata
096030040607      *
096040080703      * 2ª parte - copia per il destinatario (da lasciare al cliente)
096050040607      *   (Se NON è ristampa in arrivo per recapito c/assegni - vedi
096060040607      *   costante "Non utilizzare per la consegna")
096070130418      *                o per il mittente - se Porto Franco Prepagato
096080040607      * > "cornice" / barcode / righine varie in parte inferiore
096090040607      *
096100080604sel 1c                   select
096110080604w   1c                   when           $No_x_Cons         =  *off
096120070307     c                             and ($LascAvv           =  *off
096130070406     c                              or (%subst(DB0tb1:1:1) =  'A'
096140070406     c                              and DB0nft             >  *zeros))
096150130418     c                   write     A4prtLDV
096160130418     c                   write     A4prtBOX
096170110426     c*//                write     A4LogoBAR
096180110426      /free
096190120530         //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
096200120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
096210120530         //  // >?Simil-Logo Bartolini/Brt?
096220120530         //  write  A4LogoBR2;
096230120530         //  write  A4LogoDA2;
096240120530         //else;
096250120530         //  // >?Simil-Logo Bartolini?
096260120530         //  write  A4LogoBAR;
096270120530         //  write  A4LogoDA1;
096280120530         //endif;
096290120530         // >?Simil-Logo BRT?
096300120530         write  A4LogoBRT;
096310120530         write  A4LogoDA3;
096320110426      /end-free
096330080604w   1c                   when           $LascAvv           =  *on
096340070406     c                             and (%subst(DB0tb1:1:1) <> 'A'
096350070406     c                              or  DB0nft             =  *zeros)
096360070307     c                   eval      Pmsg3  =  Tla
096370070307     c                   write     A4prtLDVB
096380120113w   1c                   when      $No_x_Cons =  *on
096390120113w   1c*//                   when      $No_x_Cons =  '2'
096400120113     c*//                   eval      Pmsg3  =  ' NON UTILIZZARE PER'
096410120113     c*//                                    +  ' LASCIATO AVVISO -'
096420120113     c*//                                    +  ' SERVIZIO POSTE'
096430120113     c*//                   write     A4prtLDVB
096440080604e   1c                   endsl
096450040607      *
096460040607     c                   ENDSR
096470030515      *
096480030515      *---------------------------------------------------------------*
096490050217      *?Carico LDV da FNBLP00F                                       ?*
096500030515      *---------------------------------------------------------------*
096510030515     c     CarBLP        BEGSR
096520030515      *
096530030515      * Apertura files
096540030515     c                   if        not %open(FNBLP01L)
096550030515     c                   open      FNBLP01L
096560030515     c                   endif
096570030515     c                   if        not %open(FNARB01L)
096580030515     c                   open      FNARB01L
096590030515     c                   endif
096600070319     ***c v.sr sr_Start     if        not %open(FIAR401L)
096610070319     ***c " "     "         open      FIAR401L
096620070319     ***c " "     "         endif
096630030515     c                   if        not %open(FIAR601L)
096640030515     c                   open      FIAR601L
096650030515     c                   endif
096660030515     c                   if        not %open(FIAR701L)
096670030515     c                   open      FIAR701L
096680030515     c                   endif
096690060505     c                   if        not %open(FIAR901L)
096700060505     c                   open      FIAR901L
096710030515     c                   endif
096720071114      *
096730071114     c                   clear                   DB0cpi
096740030515      *
096750040607     c     KLDV          chain     FNBLP000
096760030515      *
096770030515if  1c                   if        NOT %found(FNBLP01L)
096780081117     c                   leavesr
096790030515e   1c                   endif
096800120509      *
096810120509      * Memorizzazione dei dati di FNBLP che serviranno anche dopo
096820120509      *   il reperimento dell'eventuale bolla mamma...
096830120509     c                   eval      wB0piv =  BLPcpi
096840030515      *
096850030515      * 1ª impostaz. campi della DS in base a quelli del file FNBLP
096860030515     c                   exsr      sr_DS_BLP
096870030515      *
096880070308      * Decodifico filiale di Arrivo
096890030515     c                   exsr      DecLNA
096900030515      *
096910030515      * 2ª impostaz. campi della DS in base a quelli delle tabelle
096920030515      *    "generiche" (3A, TB, 5E)
096930030515     c                   exsr      sr_DS_TAB
096940030515      *
096950030515      * Ultimi dati mancanti:
096960061030      * - Cod. cliente destinatario: imposto LNA9999
096970030515      *    SE   in Assegnato   O   c'è 2ª bolla
096980030515if  1c                   if           %subst(DB0tb1:1:1) = 'A'
096990030515     c                             or DB0tb2            <> *blanks
097000030515     c                   eval      DB0ccd = *all'9'
097010030515     c                   movel     BLPlna        DB0ccd
097020030515e   1c                   endif
097030120509      *
097040120509      * - Reperimento Data spedizione
097050120509if  1c                   if        DB0mgs <> *zeros
097060120509     c                   z-add     DB0mgs        Wdsp
097070120509     c                   movel     DB0aas        Wdsp
097080120509x   1c                   else
097090120509      *   prendo la data del giorno se non c'è la data spedizione
097100120509     c                   time                    w0140
097110120509     c                   move      w0140         w0080
097120120509     c     *eur          movel     w0080         Data_EUR
097130120509     c     *iso          movel     Data_EUR      Wdsp
097140120509e   1c                   endif
097150030515      *
097160061030      * 3ª impostaz. campi della DS: importi da calcolare
097170061031     c                   exsr      sr_DS_IMPO
097180030515      *
097190081117     c                   ENDSR
097200030515      *
097210030515      *---------------------------------------------------------------*
097220050217      *?Carico LDV da FNARB00F                                       ?*
097230030515      *---------------------------------------------------------------*
097240030515     c     CarARB        BEGSR
097250030515      *
097260030515      * Apertura files
097270030515     c                   if        not %open(FNARB01L)
097280030515     c                   open      FNARB01L
097290030515     c                   endif
097300070319     c                   if        NOT %open(FIARN11L)
097310070319     c                   open      FIARN11L
097320070319     c                   endif
097330030515     c                   if        not %open(FIAR601L)
097340030515     c                   open      FIAR601L
097350030515     c                   endif
097360030515     c                   if        not %open(FIAR701L)
097370030515     c                   open      FIAR701L
097380030515     c                   endif
097390060505     c                   if        not %open(FIAR901L)
097400060505     c                   open      FIAR901L
097410030515     c                   endif
097420071114      *
097430071114     c                   clear                   DB0cpi
097440030515      *
097450040607     c     KLDV          chain     FNARB01L
097460030515      *
097470030515if  1c                   if        NOT %found(FNARB01L)
097480070312     c                   clear                   ARBndc
097490070322     c                   clear                   ARBntc
097500070322     c                   clear                   ARBdcp
097510071015     c                   clear                   ARBpdc
097520101129     c                   clear                   ARBdcm
097530101129     c                   clear                   ARBicc
097540101129     c                   clear                   ARBica
097550120509     c                   clear                   ARBcpi
097560081117     c                   leavesr
097570030515e   1c                   endif
097580070514      *
097590070514      * Memorizzazione dei dati di FNARB che serviranno anche dopo
097600070514      *   il reperimento dell'eventuale bolla mamma...
097610070514     c                   eval      wB0ndc =  ARBndc
097620070514     c                   eval      wB0ntc =  ARBntc
097630070514     c                   eval      wB0dcp =  ARBdcp
097640081119     c                   eval      wB0tmc =  ARBtmc
097650071015     c                   move      ARBpdc        Wpdc4a
097660101125     c                   eval      wB0dcm =  ARBdcm
097670101125     c                   eval      wB0icc =  ARBicc
097680101125     c                   eval      wB0ica =  ARBica
097690120509     c                   eval      wB0piv =  ARBcpi
097700030515      *
097710030515      * 1ª impostaz. campi della DS in base a quelli del file FNARB
097720030515     c                   exsr      sr_DS_ARB
097730030515      *
097740070308      * Decodifico filiale di Arrivo
097750030515     c                   exsr      DecLNA
097760030515      *
097770030515      * 2ª impostaz. campi della DS in base a quelli delle tabelle
097780030515      *    "generiche" (3A, TB, 5E)
097790030515     c                   exsr      sr_DS_TAB
097800030515      *
097810030515      * Ultimi dati mancanti:
097820030515      * - Cod. cliente mittente
097830030515if  1c                   If        ARBccm <> *zeros
097840030515     c                   movel     ARBccm        DB0ccm
097850030515x   1c                   Else
097860030515if  2c                   if        %subst(DB0tb1:1:1) = 'F'
097870030515     c                   movel     ARBksc        DB0ccm
097880030515e   2c                   endif
097890030515e   1c                   EndIf
097900030515      * - Cod. cliente destinatario
097910030515if  2c                   if        %subst(DB0tb1:1:1) = 'A'
097920030515     c                   movel     ARBksc        DB0ccd
097930030515e   1c                   endif
097940030515      * - Se c'è 2ª bolla imposto LNA9999
097950030515if  1c                   if        DB0tb2 <> *blanks
097960030515     c                   eval      DB0ccd = *all'9'
097970030515     c                   movel     ARBlna        DB0ccd
097980030515e   1c                   endif
097990120509      * - Reperimento Data spedizione
098000120509if  1c                   if        DB0mgs <> *zeros
098010120509     c                   z-add     DB0mgs        Wdsp
098020120509     c                   movel     DB0aas        Wdsp
098030120509x   1c                   else
098040120509      *   prendo la data del giorno se non c'è la data spedizione
098050120509     c                   time                    w0140
098060120509     c                   move      w0140         w0080
098070120509     c     *eur          movel     w0080         Data_EUR
098080120509     c     *iso          movel     Data_EUR      Wdsp
098090120509e   1c                   endif
098100030515      *
098110061030      * 3ª impostaz. campi della DS: importi da calcolare
098120061031     c                   exsr      sr_DS_IMPO
098130030515      *
098140081117     c                   ENDSR
098150030515      *
098160030515      *---------------------------------------------------------------*
098170050217      *?1ª impostaz. campi della DS in base a quelli del file FNBLP  ?*
098180030515      *---------------------------------------------------------------*
098190030515     c     sr_DS_BLP     BEGSR
098200030515      *
098210030515     c                   eval      DB0cbo = BLPcbo
098220030515     c                   eval      DB0tsp = BLPtsp
098230030515     c                   eval      DB0mgs = BLPmgs
098240030515     c                   eval      DB0tc1 = BLPtc1
098250030515     c                   eval      DB0tc2 = BLPtc2
098260030515     c                   eval      DB0fns = BLPfns
098270030515     c                   eval      DB0rmn = BLPrmn
098280030515     c                   eval      DB0rma = BLPrma
098290030515     c                   eval      DB0ias = BLPias
098300030515     c                   eval      DB0vas = BLPvas
098310030515     c                   eval      DB0vmd = BLPvmd
098320030515     c                   eval      DB0vad = BLPvad
098330030515     c                   eval      DB0qft = BLPqft
098340030515     c                   eval      DB0lna = BLPlna
098350030515     c                   eval      DB0znc = BLPznc
098360030515     c                   eval      DB0rsm = BLPrsm
098370030515     c                   eval      DB0inm = BLPinm
098380030515     c                   eval      DB0cam = BLPcam
098390030515     c                   eval      DB0lom = BLPlom
098400030515     c                   eval      DB0prm = BLPprm
098410030515     c                   eval      DB0nzm = BLPnzm
098420030515     c                   eval      DB0fap = BLPfap
098430030515     c                   eval      DB0rmo = BLPrmo
098440030515     c                   eval      DB0rsd = BLPrsd
098450030515     c                   eval      DB0ind = BLPind
098460030515     c                   eval      DB0cad = BLPcad
098470030515     c                   eval      DB0lod = BLPlod
098480030515     c                   eval      DB0prd = BLPprd
098490030515     c                   eval      DB0nzd = BLPnzd
098500030515     c                   eval      DB0fin = BLPfin
098510030515     c                   eval      DB0gc1 = BLPgc1
098520030515     c                   eval      DB0gc2 = BLPgc2
098530030515     c                   eval      DB0nas = BLPnas
098540030515     c                   eval      DB0ncl = BLPncl
098550030515     c                   eval      DB0pkf = BLPpkf
098560030515     c                   eval      DB0fvf = BLPfvf
098570030515     c                   eval      DB0vlf = BLPvlf
098580030515     c                   eval      DB0fls = BLPfls
098590030515     c                   eval      DB0ncd = BLPncd
098600030515     c                   eval      DB0nca = BLPnca
098610030515     c                   eval      DB0xco = BLPxco
098620030515     c                   eval      DB0ffd = BLPffd
098630030515     c                   eval      DB0dcr = BLPdcr
098640030515     c                   eval      DB0hcr = BLPhcr
098650030515     c                   eval      DB0tcr = BLPtcr
098660030515     c                   eval      DB0fbr = BLPfbr
098670030515     c                   eval      DB0gga = BLPgga
098680030515     c                   eval      DB0gma = BLPgma
098690030515     c                   eval      DB0gva = BLPgva
098700030515     c                   move      ' '           DB0gva
098710030515     c                   eval      DB0sop = BLPsop
098720030515     c                   eval      DB0ctr = BLPctr
098730030515     c                   eval      DB0drt = BLPdrt
098740030515     c                   eval      DB0fpp = BLPfpp
098750030515     c                   eval      DB0nrt = BLPnrt
098760030515     c                   eval      DB0pdr = BLPpdr
098770030515      *
098780030515      * Cod. cliente mittente
098790040311     c                   move      BLPksc        w0040
098800030515if  1c                   IF        BLPccm <> *zeros
098810030515     c                   movel     BLPccm        DB0ccm
098820030515x   1c                   ELSE
098830030515      *   Lo imposto solo se non è codice generico destinatario
098840040311if  2c                   if        w0040  <> 9999
098850030515     c                   movel     BLPksc        DB0ccm
098860030515x   2c                   else
098870030515     c                   movel     BLPksc        DB0ccd
098880030515     c                   movel     BLPlna        DB0ccd
098890030515e   2c                   endif
098900030515e   1c                   ENDIF
098910030515      *
098920030515      * Allega documento di trasporto
098930081119     c                   clear                   DB0bam
098940060505if  1c                   if           BLPfst = 'S'
098950030515     c                             or BLPfst = 'C'
098960030515     c                   movel     'S'           DB0bam
098970060505e   1c                   endif
098980060524      *
098990060524      * Se bolla di RESO: Codice cliente da Assegnato
099000061107     c*** (già fatto:)   clear                   dsBL4A
099010060524if  1c                   if        DB0fbr = 'S'
099020060524     c                   exsr      RepAR4_A
099030060524e   1c                   endif
099040030515      *
099050030515      * Destinatario: vedo se esiste 2ª ragione sociale
099060061107     c                   clear                   DB0rd2
099070061107     c                   exsr      RepAR4_D
099080030515      *
099090030515      * Note
099100061107     c                   clear                   DB0nt1
099110061107     c                   clear                   DB0nt2
099120061107     c                   exsr      RepAR4_89
099130030515      *
099140030515     c                   ENDSR
099150030515      *
099160030515      *---------------------------------------------------------------*
099170050217      *?1ª impostaz. campi della DS in base a quelli del file FNARB  ?*
099180030515      *---------------------------------------------------------------*
099190030515     c     sr_DS_ARB     BEGSR
099200030515      *
099210030515     c                   eval      DB0cbo = ARBcbo
099220030515     c                   eval      DB0tsp = ARBtsp
099230030515     c                   eval      DB0mgs = ARBmgs
099240030515     c                   eval      DB0tc1 = ARBtc1
099250030515     c                   eval      DB0tc2 = ARBtc2
099260030515     c                   eval      DB0fns = ARBfns
099270030515     c                   eval      DB0rmn = ARBrmn
099280030515     c                   eval      DB0rma = ARBrma
099290030515     c                   eval      DB0ias = ARBias
099300030515     c                   eval      DB0vas = ARBvas
099310030515     c                   eval      DB0vmd = ARBvmd
099320030515     c                   eval      DB0vad = ARBvad
099330030515     c                   eval      DB0qft = ARBqft
099340030515     c                   eval      DB0lna = ARBlna
099350030515     c                   eval      DB0znc = ARBznc
099360030515     c                   eval      DB0rsm = ARBrsm
099370030515     c                   eval      DB0inm = ARBinm
099380030515     c                   eval      DB0cam = ARBcam
099390030515     c                   eval      DB0lom = ARBlom
099400030515     c                   eval      DB0prm = ARBprm
099410030515     c                   eval      DB0nzm = ARBnzm
099420030515     c                   eval      DB0fap = ARBfap
099430030515     c                   eval      DB0rmo = ARBrmo
099440030515     c                   eval      DB0rsd = ARBrsd
099450030515     c                   eval      DB0ind = ARBind
099460030515     c                   eval      DB0cad = ARBcad
099470030515     c                   eval      DB0lod = ARBlod
099480030515     c                   eval      DB0prd = ARBprd
099490030515     c                   eval      DB0nzd = ARBnzd
099500030515     c                   eval      DB0fin = ARBfin
099510030515     c                   eval      DB0gc1 = ARBgc1
099520030515     c                   eval      DB0gc2 = ARBgc2
099530030515     c                   eval      DB0nas = ARBnas
099540030515     c                   eval      DB0ncl = ARBncl
099550030515     c                   eval      DB0pkf = ARBpkf
099560030515     c                   eval      DB0fvf = ARBfvf
099570030515     c                   eval      DB0vlf = ARBvlf
099580030515     c                   eval      DB0fls = ARBfls
099590030515     c                   eval      DB0ncd = ARBncd
099600030515     c                   eval      DB0nca = ARBnca
099610030515     c                   eval      DB0xco = ARBxco
099620030515     c                   eval      DB0ffd = ARBffd
099630030515     c                   eval      DB0dcr = ARBdcr
099640030515     c                   eval      DB0hcr = ARBhcr
099650030515     c                   eval      DB0tcr = ARBtcr
099660030515     c                   eval      DB0fbr = ARBfbr
099670030515     c                   eval      DB0gga = ARBgga
099680030515     c                   eval      DB0gma = ARBgma
099690030515     c                   eval      DB0gva = ARBgva
099700030515     c                   move      ' '           DB0gva
099710030515     c                   clear                   DB0sop
099720030515     c                   eval      DB0ctr = ARBctr
099730030515     c                   clear                   DB0drt
099740030515     c                   clear                   DB0fpp
099750030515     c                   clear                   DB0nrt
099760030515     c                   clear                   DB0pdr
099770030515      *
099780030515      * Allega documento di trasporto
099790060524      * & Codice cliente da Assegnato
099800061107     c*** (già fatto:)   clear                   dsBL4A
099810031029     c                   clear                   DB0bam
099820061107     c                   exsr      RepAR4_A
099830060505     c                   if        §B4Abm = 'S' or §B4Abm = 'C' or
099840031029     c                             §B4Abm = 'P' or §B4Abm = 'Q'
099850030515     c                   movel     'S'           DB0bam
099860060505     c                   endif
099870030515      *
099880030515      * Destinatario: vedo se esiste 2ª ragione sociale
099890061107     c                   clear                   DB0rd2
099900061107     c                   exsr      RepAR4_D
099910030515      *
099920030515      * Note
099930061107     c                   clear                   DB0nt1
099940061107     c                   clear                   DB0nt2
099950061107     c                   exsr      RepAR4_89
099960030515      *
099970030515     c                   ENDSR
099980030515      *
099990030515      *---------------------------------------------------------------*
100000050217      *?2ª impostaz. campi della DS in base a quelli delle tabelle   ?*
100010050217      *?   "generiche" (non da "1A")                                 ?*
100020030515      *---------------------------------------------------------------*
100030030515     c     sr_DS_TAB     BEGSR
100040030515      *
100050030515      * Carico le tabelle "generiche" (3A, TB, 5E)
100060030515     c                   exsr      CarTAB
100070030515      *
100080030515      * Codici bolla
100090030515     c                   eval      DB0tb1 = §3Atb1
100100030515     c                   eval      DB0tb2 = §3Atb2
100110030515     c                   eval      DB0dcb = §3Ades
100120030515      * Tipo servizio
100130030515if  1c                   if        DB0dts = *blanks
100140030515     c                   eval      DB0dts = §5Edes
100150030515     c                   eval      DB0fst = §5Efst
100160030515e   1c                   endif
100170030515      * Consegne particolari
100180030515if  1c                   if            DB0tc1 <> *blanks
100190030515     c                             and DB0dc1 =  *blanks
100200030515     c                   movel(p)  DB0tc1        TBLkey
100210030515     c                   exsr      Tab_1P
100220030515     c                   eval      DB0dc1 = §1Pdes
100230030515e   1c                   endif
100240030515if  1c                   if            DB0tc2 <> *blanks
100250030515     c                             and DB0dc2 =  *blanks
100260030515     c                   movel(p)  DB0tc2        TBLkey
100270030515     c                   exsr      Tab_1P
100280030515     c                   eval      DB0dc2 = §1Pdes
100290030515e   1c                   endif
100300030515      * Esenzione IVA
100310030515if  1c                   if            DB0cei <> *blanks
100320030515     c                             and DB0dce =  *blanks
100330030515     c                   eval      DB0dce =  §EIdes
100340030515e   1c                   endif
100350030515      *
100360030515     c                   ENDSR
100370060524      *
100380060524      *---------------------------------------------------------------*
100390060524      *?Reperimento dati da FIAR400F x trk "A"                       ?*
100400061107      *?(dati aggiuntivi per P.Assegnato)                            ?*
100410060524      *---------------------------------------------------------------*
100420060524     c     RepAR4_A      BEGSR
100430060524      *
100440061107     c*** (già fatto:)   clear                   dsBL4A
100450061107      *
100460060524     c                   eval      Wtrc = 'A'
100470060524     c     KLDVtrc       chain     FIAR4000
100480060524      *
100490060524if  1c                   if        %found(FIAR401L)
100500060524     c                   movel     AR4Not        dsBL4A
100510060524e   1c                   endif
100520060524      *
100530060524     c                   ENDSR
100540061107      *
100550061107      *---------------------------------------------------------------*
100560061107      *?Reperimento dati da FIAR400F x trk "D"                       ?*
100570061107      *?(2ª ragione sociale del destinatario)                        ?*
100580061107      *---------------------------------------------------------------*
100590061107     c     RepAR4_D      BEGSR
100600061107      *
100610061107     c*** NON esiste:    clear                   dsBL4D
100620061107     c*** (già fatto:)   clear                   DB0rd2
100630061107      *
100640061107     c                   eval      Wtrc = 'D'
100650061107     c     KLDVtrc       chain     FIAR4000
100660061107      *
100670061107if  1c                   if        %found(FIAR401L)
100680061107     c                   movel     AR4not        DB0rd2
100690061107e   1c                   endif
100700061107      *
100710061107     c                   ENDSR
100720061107      *
100730060626      *---------------------------------------------------------------*
100740060626      *?Reperimento dati da FIAR400F x trk "I"                       ?*
100750061107      *?(dati DPD)                                                   ?*
100760060626      *---------------------------------------------------------------*
100770061030     c     RepAR4_I      BEGSR
100780061030      *
100790061107     c*** (già fatto:)   clear                   dsBL4I
100800061107      *
100810061030     c                   eval      Wtrc = 'I'
100820061030     c     KLDVtrc       chain     FIAR4000
100830061030      *
100840061030if  1c                   if        %found(FIAR401L)
100850061030     c                   movel     AR4not        dsBL4I
100860060626e   1c                   endif
100870061030      *
100880061030     c                   ENDSR
100890071008      *
100900071015      *---------------------------------------------------------------*
100910071015      *?Reperimento dati da FIAR400F x trk "M"                       ?*
100920071015      *?(dati aggiuntivi per O.R.M. abbinato al P.Franco PrePagato)  ?*
100930071015      *---------------------------------------------------------------*
100940071015     c     RepAR4_M      BEGSR
100950071015      *
100960071015     c*** (già fatto:)   clear                   dsBL4M
100970071015      *
100980071015     c                   eval      Wtrc = 'M'
100990071015     c     KLDVtrc       chain     FIAR4000
101000071015      *
101010071015if  1c                   if        %found(FIAR401L)
101020071015     c                   movel     AR4Not        dsBL4M
101030071015e   1c                   endif
101040071015      *
101050071015     c                   ENDSR
101060061107      *
101070061107      *---------------------------------------------------------------*
101080061107      *?Reperimento dati da FIAR400F x trk "Q"                       ?*
101090061107      *?(Codice Fiscale del mittente a sx & del destinatario a dx)   ?*
101100061107      *---------------------------------------------------------------*
101110061107     c     RepAR4_Q      BEGSR
101120061107      *
101130061108     c*** (già fatto:)   clear                   dTA4Q
101140061107     c*** (già fatto:)   clear                   DB0cpi
101150061107      *
101160061107     c                   eval      Wtrc = 'Q'
101170061107     c     KLDVtrc       chain     FIAR4000
101180061107      *
101190061107if  1c                   if        %found(FIAR401L)
101200061108     c                   movel     AR4not        dTA4Q
101210061108e   1c                   endif
101220061107      *
101230061107     c                   ENDSR
101240061107      *
101250061107      *---------------------------------------------------------------*
101260061107      *?Reperimento dati da FIAR400F x trk "8" / "9"                 ?*
101270061107      *?(note)                                                       ?*
101280061107      *---------------------------------------------------------------*
101290061107     c     RepAR4_89     BEGSR
101300061107      *
101310061107     c*** NON esiste:    clear                   dsBL48
101320061107     c*** NON usata:     clear                   dsBL49
101330061107     c*** (già fatto:)   clear                   DB0nt1
101340061107     c*** (già fatto:)   clear                   DB0nt2
101350061107      *
101360061107     c                   eval      Wtrc = '8'
101370061107     c     KLDVtrc       chain     FIAR4000
101380061107if  1c                   if        %found(FIAR401L)
101390061107     c                   movel     AR4not        DB0nt1
101400061107e   1c                   endif
101410061107      *
101420061107     c                   eval      Wtrc = '9'
101430061107     c     KLDVtrc       chain     FIAR4000
101440061107if  1c                   if        %found(FIAR401L)
101450061107     c                   movel     AR4not        DB0nt2
101460061107e   1c                   endif
101470061107      *
101480061107     c                   ENDSR
101490030515      *
101500030515      *---------------------------------------------------------------*
101510050217      *?Carico le tabelle che servono: 3A/TB/5E.                     ?*
101520030515      *---------------------------------------------------------------*
101530030515     c     CarTAB        BEGSR
101540030515      *
101550030515      * Tab. "3A" - Codici Bolla
101560030515      * (NO se rimesso mittente)
101570030515     c                   clear                   ds3A
101580030515if  1c                   if        DB0ris <> 'M'
101590030515     c                   movel     '3A'          TBLcod
101600030515     c                   movel(p)  DB0cbo        TBLkey
101610030515     c     K03TAB        chain     TABEL
101620030515if  2c                   if        %found(TABEL00F)
101630030515     c                   movel     TBLuni        ds3A
101640030515e   2c                   endif
101650030515e   1c                   endif
101660070515     c                   eval      $Mamma =  (§3Abcm = 'S')
101670030515      *
101680030515      * Tab. "TB" - Tipi Bolla
101690030515      * (NO se rimesso mittente)
101700030515      * se Bolla in assegnato, con tipo bolla vedo se da contabilizzare
101710030515      * se no non stampo i dati fattura
101720030515     c                   clear                   dsTB
101730030515if  1c                   if        DB0ris <> 'M'
101740030515     c                   movel     'TB'          TBLcod
101750061030if  2c                   if            §3Atb2 <> *blanks
101760121029     c                             and DB0tbo <> 'P'
101770030515     c                             and DB0tbo <> '8'
101780030515     c                   movel(p)  §3Atb2        TBLkey
101790030515x   2c                   else
101800030515     c                   movel(p)  §3Atb1        TBLkey
101810030515e   2c                   endif
101820030515     c     K03TAB        chain     TABEL
101830030515if  2c                   if        %found(TABEL00F)
101840030515     c                   movel     TBLuni        dsTB
101850030515e   2c                   endif
101860030515e   1c                   endif
101870030515      *
101880030515      * Tab. "5E" - Tipi Servizio
101890030515     c                   clear                   ds5E
101900090513if  1c                   if        DB0tsp <> *blanks
101910030515     c                   movel     '5E'          TBLcod
101920030515     c                   movel(p)  DB0tsp        TBLkey
101930030515     c     K03TAB        chain     TABEL
101940030515if  2c                   if        %found(TABEL00F)
101950030515     c                   movel     TBLuni        ds5E
101960030515e   2c                   endif
101970030515e   1c                   endif
101980030515      *
101990030911      * Tab. "EI" - Esenzioni I.V.A.
102000030515     c                   clear                   dsEI
102010030911      * Tab. "1A" - Tipi incasso contrassegno
102020030911     c                   clear                   ds1A
102030030911      * Tab. "1P" - Consegne particolari
102040030911     c                   clear                   ds1P
102050150226      *//* Tab. "TBA" - Tipi bancale
102060150226     c*//                clear                   dTBA
102070030515      *
102080030515     c                   ENDSR
102090030911      *
102100030911      *---------------------------------------------------------------*
102110050217      *?Carico tabella "EI" (Esenzione I.V.A.)                       ?*
102120030911      *---------------------------------------------------------------*
102130030911     c     Tab_EI        BEGSR
102140030911      *
102150030911     c                   clear                   dsEI
102160030911if  1c                   if        DB0cei <> *blanks  and  DB0dce = *blanks
102170030911     c                   movel     'EI'          TBLcod
102180030911     c                   movel(p)  DB0cei        TBLkey
102190030911     c     K03TAB        chain     TABEL
102200030911if  2c                   if        %found(TABEL00F)
102210030911     c                   movel     TBLuni        dsEI
102220030911e   2c                   endif
102230030911e   1c                   endif
102240030911     c                   eval      DB0dce =  §EIdes
102250030911      *
102260030911     c                   ENDSR
102270030515      *
102280030515      *---------------------------------------------------------------*
102290050217      *?Carico tabella "1A" (Tipo incasso contrassegno)              ?*
102300030515      *---------------------------------------------------------------*
102310030515     c     Tab_1A        BEGSR
102320030515      *
102330030515     c                   clear                   ds1A
102340030515if  1c                   if        DB0cas <> *zeros
102350030515     c                   movel     '1A'          TBLcod
102360030515     c                   movel(p)  DB0tic        TBLkey
102370030515     c     K03TAB        chain     TABEL
102380030515if  2c                   if        %found(TABEL00F)
102390030515     c                   movel     TBLuni        ds1A
102400030515e   2c                   endif
102410030515e   1c                   endif
102420030515      *
102430030515      * Imposto i dati nella DS
102440030515     c                   eval      DB0dti = §1Ades
102450030515     c                   eval      DB0ucm = §1Aucm
102460030515     c                   eval      DB0ste = §1Aste
102470030515      *
102480030515     c                   ENDSR
102490030515      *
102500030515      *---------------------------------------------------------------*
102510050217      *?Carico tabella "1P" (Consegne particolari)                   ?*
102520030515      *---------------------------------------------------------------*
102530030515     c     Tab_1P        BEGSR
102540030515      *
102550030515     c                   clear                   ds1P
102560030515     c                   movel     '1P'          TBLcod
102570030515     c*** già impostato: movel(p)  DB0tc1/2      TBLkey
102580030515     c     K03TAB        chain     TABEL
102590030515if  1c                   if        %found(TABEL00F)
102600030515     c                   movel     TBLuni        ds1P
102610030515e   1c                   endif
102620030515      *
102630030515     c                   ENDSR
102640150226      *
102650150226      *//*---------------------------------------------------------------*
102660150226      *//*?Reperisco tabella "TBA" (Tipi Bancale)                       ?*
102670150226      *//*---------------------------------------------------------------*
102680150226     c*//  Tab_TBA       BEGSR
102690150226      *//*
102700150226     c*//                clear                   dTBA
102710150226     c*//                reset                   TIBS02ds
102720150226     c*//                eval      T02cod = 'TBA'
102730150226     c*//                movel(p)  Wtba          T02ke1
102740150226     c*//                call      'TIBS02R'
102750150226     c*//                parm                    KPJBA
102760150226     c*//                parm                    TIBS02ds
102770150226if  1c*//                if        T02err = *blanks
102780150226     c*//                movel     T02uni        dTBA
102790150226e   1c*//                endif
102800150226      *//*
102810150226     c*//                ENDSR
102820030515      *
102830030515      *---------------------------------------------------------------*
102840050217      *?Calcolo importi LDV                                          ?*
102850030515      *---------------------------------------------------------------*
102860061031     c     sr_DS_IMPO    BEGSR
102870030515      *
102880030515      * Vedo se esiste tassazione
102890030515      *  solo se tipo bolla contabilizzabile
102900061030if  1c                   IF        §TBfcb =  '1'
102910030515     c
102920061030     c                   eval      Wtrc   =  '1'
102930040607     c     KLDVtrc       chain     FIAR6000
102940030515     c
102950030515if  2c                   if        %found(FIAR601L)
102960030515     c                   movel     AR6div        DB0div
102970030515     c                   exsr      CtrDIV
102980030515      *
102990030515      * NON c'è 2ª bolla
103000061031if  3c                   if            DB0tb2 = *blanks
103010121029     c                             or  DB0tbo = 'P'
103020061031     c                             or  DB0tbo = '8'
103030030515      *  Estremi fattura
103040030515     c                   z-add     AR6nft        DB0nft
103050030515     c                   z-add     AR6dft        DB0dft
103060130124     c                   z-add     AR6fiv        wB0fiv
103070061031      *  Se è assegnato o franco prepagato => stampo tutta la tassaz.
103080061031if  4c                   if            %subst(DB0tb1:1:1) = 'A'
103090061031     c                             or  DB0tbo = '8'
103100061030     c                   eval      Wtrc   =  '1'
103110030515     c                   exsr      RepTASSAZ
103120030515e   4c                   endif
103130030515e   3c                   endif
103140030515e   2c                   endif
103150030515      *
103160061030      * C'è 2ª bolla
103170061031if  2c                   IF            DB0tb2 <> *blanks
103180121029     c                             and DB0tbo <> 'P'
103190061031     c                             and DB0tbo <> '8'
103200030515      *
103210061030     c                   eval      Wtrc   =  '2'
103220040607     c     KLDVtrc       chain     FIAR6000
103230030515      *
103240061030if  3c                   If        %found(FIAR601L)
103250030515     c                   movel     AR6div        DB0div
103260030515     c                   exsr      CtrDIV
103270030515     c                   movel     AR6cei        DB0cei
103280030515      *   Estremi fattura
103290030515     c                   z-add     AR6nft        DB0nft
103300030515     c                   z-add     AR6dft        DB0dft
103310130124     c                   z-add     AR6fiv        wB0fiv
103320030515if  4c                   if        AR6ksc <> *zeros
103330030515     c                   eval      DB0ccd =  AR6ksc
103340030515e   4c                   endif
103350061030     c                   eval      Wtrc   =  '2'
103360030515     c                   exsr      RepTASSAZ
103370061030e   3c                   EndIf
103380030515      *
103390030515e   2c                   ENDIF
103400030515      *
103410030515e   1c                   ENDIF
103420030515      *
103430030515      * Vedo se ha contrassegno
103440060505     c     KLDV          chain     FIAR9000
103450060505if  1c                   IF        %found(FIAR901L)
103460061030     c                   eval      DB0cas =  AR9cas
103470061030     c                   eval      DB0vca =  AR9vca
103480061030     c                   eval      DB0gca =  AR9gca
103490061030     c                   eval      DB0tic =  AR9tic
103500030515x   1c                   ELSE
103510030515     c                   clear                   DB0cas
103520030515     c                   clear                   DB0vca
103530030515     c                   clear                   DB0gca
103540030515     c                   clear                   DB0tic
103550030515     c                   clear                   DB0dti
103560030515     c                   clear                   DB0ucm
103570030515     c                   clear                   DB0ste
103580030515e   1c                   ENDIF
103590120509      *
103600120509      * Se P.F. Prepagato verificherò il Codice Fiscale del Mittente
103610120509      *   (NON se mittente estero)
103620120509if  1c                   if           ( DB0tbo  = '8'
103630120509     c                             and (DB0nft  <> *zeros
103640120509     c                              or  DB0ift  <> *zeros)
103650120509     c                             and  DB0nzm  =  *blanks )
103660120509     c                   exsr      RepAR4_Q
103670120509     c                   movel     §TA4Qcfm      DB0cpi
103680120509e   1c                   endif
103690121029      * Se P.A. con fattura immediata o con 2ª bolla verificherò il
103700121029      *   Codice Fiscale (NON se destinatario estero)
103710121026if  1c*//                if           ( DB0tbo  = 'A'
103720121029if  1c                   if          ( (%subst(DB0tb1:1:1) = 'A'
103730121029     c                             or  (DB0tb2  <> *blanks
103740121029     c                             and  DB0tbo  <> 'P'
103750121029     c                             and  DB0tbo  <> '8') )
103760120509     c                             and (DB0nft  <> *zeros
103770120509     c                              or  DB0ift  <> *zeros)
103780120509     c                             and  DB0nzd  =  *blanks )
103790120509     c                   exsr      RepAR4_Q
103800120509     c                   movel     §TA4Qcfd      DB0cpi
103810120509e   1c                   endif
103820120509     c                   eval      wB0cfi = DB0cpi
103830061030      *
103840030515      * Vedo se si tratta di bolla figlia:
103850040607      * Se bolla con MAMMA prendo c/a e segnacolli da bolla MAMMA
103860070515if  1c                   if        $Mamma =  *on
103870081117     c                   exsr      BollaFiglia
103880030515e   1c                   endif
103890030515      *
103900030515     c                   ENDSR
103910030515      *
103920030515      *---------------------------------------------------------------*
103930050217      *?Decodifica LNA                                               ?*
103940030515      *---------------------------------------------------------------*
103950030515     c     DecLNA        BEGSR
103960030515      *
103970140206if  1c*//                if            DB0dla =  *blanks
103980140206     c*//                          and DB0ris <> 'M'
103990140206if  1c                   if        DB0ris <> 'M'
104000030515      *
104010040311     c                   eval      SAVlna = DB0lna
104020070312     c                   clear                   OG146arr
104030061016     c                   clear                   OG148arr
104040070312     c                   clear                   OG150arr
104050030515     c     DB0lna        chain     AZORG
104060030515      *
104070030515if  2c                   if        %found(AZORG01L)
104080030515     c                   eval      DB0fag = ORGfag
104090030515     c                   eval      DB0dla = ORGdes
104100030515     c                   eval      DB0inl = ORGind
104110030515     c                   eval      DB0cal = ORGcpf
104120030515     c                   eval      DB0lol = ORGloc
104130030515     c                   eval      DB0prl = ORGpro
104140030515     c                   eval      DB0tel = ORGtel
104150140129     c*//                eval      DB0fax = ORGfax
104160140129     c                   eval      DB0fax = ORGdd2
104170070312     c                   movel     ORGde6        OG146arr
104180061016     c                   movel     ORGde8        OG148arr
104190070312     c                   movel     ORGdf0        OG150arr
104200030515e   2c                   endif
104210030515      *
104220030515e   1c                   endif
104230030515      *
104240030515     c                   ENDSR
104250030515      *
104260030515      *---------------------------------------------------------------*
104270050217      *?Reperimento dati di tutta la tassazione per la stampa        ?*
104280030515      *---------------------------------------------------------------*
104290030515     c     RepTASSAZ     BEGSR
104300030515      *
104310030515     c                   eval      DB0imv = AR6imv
104320030515     c                   eval      DB0cei = AR6cei
104330030515     c                   eval      DB0ift = AR6ift
104340030911      * Esenzione IVA
104350030911if  1c                   if            DB0cei <> *blanks
104360030911     c                             and DB0dce =  *blanks
104370030911     c                   exsr      Tab_EI
104380030911e   1c                   endif
104390030515      *
104400030515      * Controllo se ci sono varie esenti
104410030515      *
104420030515      * - varia 1
104430030515     c                   eval      Wsvx = AR6sv1
104440030515     c                   eval      Wvax = AR6va1
104450030515     c                   exsr      RepTASSAZ_b
104460030515      * - varia 2
104470030515     c                   eval      Wsvx = AR6sv2
104480030515     c                   eval      Wvax = AR6va2
104490030515     c                   exsr      RepTASSAZ_b
104500030515      * - varia 3
104510030515     c                   eval      Wsvx = AR6sv3
104520030515     c                   eval      Wvax = AR6va3
104530030515     c                   exsr      RepTASSAZ_b
104540030515      * - varie ulteriori solo per tipo record 1 (prima bolla)
104550040607     c     KLDVtrc       setll     FIAR7000
104560040607     c     KLDVtrc       reade     FIAR7000
104570030515      *
104580030515do  2c                   Dow       Not %eof(FIAR701L)
104590030515      *
104600030515     c                   eval      Wsvx = AR7svn
104610030515     c                   eval      Wvax = AR7van
104620030515     c                   exsr      RepTASSAZ_b
104630030515      *
104640040607     c     KLDVtrc       reade     FIAR7000
104650030515e   2c                   Enddo
104660030515      *
104670030515     c                   ENDSR
104680030515      *
104690030515      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
104700050217      *?Reperimento dati di tutta la tassazione per la stampa        ?*
104710050217      *?- 2ª parte -                                                 ?*
104720030515      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
104730030515     c     RepTASSAZ_b   BEGSR
104740030515      *
104750030515sel 1c                   Select
104760030515      *
104770030515w   1c                   When      Wsvx       = *blanks
104780030515      *
104790030515w   1c                   When      Wsvx       = §$2iva
104800030515     c                   eval      DB0iva     = Wvax
104810030515      *
104820030515w   1c                   When      Wsvx       = §$2bol
104830030515     c                   add       Wvax          DB0ese
104840050624     c                   z-add     Wvax          wBolloVirt
104850050624     c                   eval      $BolloVirt = *on
104860030515      *
104870030515x   1c                   Other
104880050624     c                   eval      *in31      = *off
104890030515     c     Wsvx          lookup    $Var                                   31
104900030515     c   31              add       Wvax          DB0ese
104910030515     c   31              sub       Wvax          DB0imv
104920030515      *
104930030515w   1c                   Endsl
104940030515      *
104950030515     c                   ENDSR
104960030515      *
104970030515      *---------------------------------------------------------------*
104980050217      *?Chain su TABEL per le PARTICOLARITA'                         ?*
104990030515      *---------------------------------------------------------------*
105000030515     c     TABxPAR       BEGSR
105010030515      *
105020100513     c                   movel(p)  WCdPar        w053a
105030030515     c                   movel     '7V'          TBLcod
105040030515     c                   movel(p)  KeyPAR        TBLkey
105050030515     c     K03TAB        chain     TABEL
105060030515      *
105070030515if  1c                   if        %found(TABEL00F)
105080030515     c                             and TBLflg = *blanks
105090100513     c                   eval      w053a = WCdPar + '-'
105100100513     c                                   + %trim(%subst(TBLuni:1:49))
105110030515x   1c                   else
105120030515     c                   clear                   TBLuni
105130030515e   1c                   endif
105140030515      *
105150030515     c                   ENDSR
105160030515      *
105170030515      *---------------------------------------------------------------*
105180050217      *?CONTROLLO C/A SE E' DA CONVERTIRE IN EURO                    ?*
105190030515      *---------------------------------------------------------------*
105200030515     c     CtrCAs        BEGSR
105210030515      *
105220030515      * E' da convertire
105230030515if  1c                   if        DB0ucm = 'S'
105240030515     c                   eval      $CAeur = *on
105250030515e   1c                   endif
105260030515      *
105270030515      * Se in EURO è da convertire e da stampare in EURO
105280030515if  1c                   if        DB0vca = §GEDcn
105290030515     c                   eval      $CAeur = *on
105300030515     c                   endif
105310030515      *
105320030515      * Devo convertire il c/a da valuta a moneta di conto
105330030624if  1c                   if        DB0vca <> §GEDcn
105340030515     c                   clear                   YEURCODS
105350030515     c                   movel     DB0vca        YECdvi
105360030515     c                   z-add     DB0cas        YECimi
105370030515     c                   movel     §GEDcn        YECdvo
105380030515     c                   call      'YEURCO'
105390030515     c                   parm                    YEURCODS
105400030515if  2c                   if        YECesi = *blanks
105410030515     c                   z-add     YECimo        Wcas
105420030515e   2c                   endif
105430030515x   1c                   else
105440030515     c                   z-add     DB0cas        Wcas
105450030515e   1c                   endif
105460030515      *
105470030515     c                   ENDSR
105480030515      *
105490030515      *---------------------------------------------------------------*
105500050217      *?CONTROLLO DIVISA DELLA FATTURA                               ?*
105510030515      *---------------------------------------------------------------*
105520030515     c     CtrDIV        BEGSR
105530030515      *
105540030515     c                   clear                   dsCV
105550030515      *
105560030515     c                   movel     'CV'          TBLcod
105570030515     c                   movel(p)  DB0div        TBLkey
105580030515     c     K03TAB        chain     TABEL
105590030515     c                   if        %found(TABEL00F)
105600030515     c                   movel     TBLuni        dsCV
105610030515     c                   movel     §CVfdc        DB0fdc
105620030515     c                   endif
105630030515      *
105640030515     c                   ENDSR
105650030515      *
105660030515      *---------------------------------------------------------------*
105670050217      *?SE BOLLA CON MAMMA PRENDO C/A E SEGNACOLLI DA BOLLA MAMMA    ?*
105680030515      *---------------------------------------------------------------*
105690081117     c     BollaFiglia   BEGSR
105700030515      *
105710090528     c                   if        not %open(FNLBL01L)
105720090528     c                   open      FNLBL01L
105730090528     c                   endif
105740090528      *
105750040607     c     KLDV          chain     FNLBL000
105760030515if  1c                   if        %found(FNLBL01L)
105770030515      *
105780030515     c     KLBL          chain     FNARB000
105790030515if  2c                   if        %found(FNARB01L)
105800030515      *
105810031015     c                   eval      wB0nrs = ARBnrs
105820030515     c                   eval      DB0fns = ARBfns
105830030515     c                   eval      DB0fls = ARBfls
105840030515     c                   eval      DB0ncd = ARBncd
105850030515     c                   eval      DB0nca = ARBnca
105860050114     c**!!!              eval      DB0xco = ARBxco
105870030515     c                   eval      DB0znc = ARBznc
105880030515      * Aggancio AR9
105890060505     c     KLBL          chain     FIAR9000
105900060505if  3c                   IF        %found(FIAR901L)
105910030515     c                   eval      DB0cas = AR9cas
105920030515     c                   eval      DB0vca = AR9vca
105930030515     c                   eval      DB0gca = AR9gca
105940030515     c                   eval      DB0tic = AR9tic
105950030515x   3c                   ELSE
105960030515     c                   clear                   DB0cas
105970030515     c                   clear                   DB0vca
105980030515     c                   clear                   DB0gca
105990030515     c                   clear                   DB0tic
106000030515     c                   clear                   DB0dti
106010030515     c                   clear                   DB0ucm
106020030515     c                   clear                   DB0ste
106030030515e   3c                   ENDIF
106040030515      *
106050030515e   2c                   endif
106060030515      *
106070030515e   1c                   endif
106080030515      *
106090030515     c                   ENDSR
106100030903      *
106110030903      *---------------------------------------------------------------*
106120050217      *?Impostazione Quantità e Unità di Misura da FNORM             ?*
106130030903      *---------------------------------------------------------------*
106140030903     c     QUM_FNORM     BEGSR
106150030903      *
106160030905     c*** (già fatto:)   clear                   $Qum
106170030905     c                   clear                   Wx
106180030903      * Colli
106190030905if  1c                   if        ORMncl   > *zeros
106200030905     c                             and Wx   < %elem($Qum)
106210030905     c                   add       1             wx
106220030905     c                   eval      $Qum(wx) = %editc(ORMncl : '4')
106230030905     c                                      + ' colli'
106240030903e   1c                   endif
106250030903      * Peso
106260030905if  1c                   if        ORMpkg   > *zeros
106270030905     c                             and Wx   < %elem($Qum)
106280030905     c                   add       1             wx
106290030905     c                   eval      $Qum(wx) = %editc(ORMpkg : '4')
106300030905     c                                      + ' kg'
106310030905e   1c                   endif
106320071025      * Volume bancali
106330071025if  1c                   if        ORMvlm   > *zeros
106340071025     c                             and Wx   < %elem($Qum)
106350071025     c                   add       1             wx
106360071025     c                   eval      $Qum(wx) = %editc(ORMvlm : '4')
106370071025     c                                      + ' mc'
106380071025e   1c                   endif
106390030905     *** * Volume
106400030905     *** * (in ultimo posto SOLO SE non ci sono bilici/motrici/autotreni)
106410030905if  1***c                   if        ORMvlm   > *zeros
106420030905     ***c                             and (Wx  < %elem($Qum) - 1
106430030905     ***c                              or  Wx  = %elem($Qum) - 1
106440030905     ***c                              and w0112 = *zeros)
106450030905     ***c                   add       1             wx
106460030905     ***c                   eval      $Qum(wx) = %editc(ORMvlm : '4')
106470030905     ***c                                      + ' metri cubi'
106480030905e   1***c                   endif
106490030905     *** * Bancali
106500030905     *** * (in ultimo posto SOLO SE non ci sono bilici/motrici/autotreni)
106510030905if  1***c                   if        ORMbnc   > *zeros
106520030905     ***c                             and (Wx  < %elem($Qum) - 1
106530030905     ***c                              or  Wx  = %elem($Qum) - 1
106540030905     ***c                              and w0112 = *zeros)
106550030905     ***c                   add       1             wx
106560030905     ***c                   eval      $Qum(wx) = %editc(ORMbnc : '4')
106570030905     ***c                                      + ' bancali'
106580030905e   1***c                   endif
106590030905     *** * Bilici
106600030905if  1***c                   if        ORMblc  > *zeros
106610030905     ***c                             and Wx  < %elem($Qum)
106620030905     ***c                   add       1             wx
106630030905     ***c                   eval      $Qum(wx) = %editc(ORMblc : '4')
106640030905     ***c                                      + ' bilici'
106650030905e   1***c                   endif
106660030905     *** * Motrici
106670030905if  1***c                   if        ORMmtc  > *zeros
106680030905     ***c                             and Wx  < %elem($Qum)
106690030905     ***c                   add       1             wx
106700030905     ***c                   eval      $Qum(wx) = %editc(ORMmtc : '4')
106710030905     ***c                                      + ' motrici'
106720030905e   1***c                   endif
106730030905     *** * Autotreni
106740030905if  1***c                   if        ORMatt  > *zeros
106750030905     ***c                             and Wx  < %elem($Qum)
106760030905     ***c                   add       1             wx
106770030905     ***c                   eval      $Qum(wx) = %editc(ORMatt : '4')
106780030905     ***c                                      + ' autotreni'
106790030905e   1***c                   endif
106800030903      *
106810030903     c                   ENDSR
106820130306      *
106830130306      /free
106840130306
106850130306       //--------------------------------------------------------------
106860130306       //?Scrittura record (relativo alla spedizione) nel *file PDFLIST?
106870130306       //--------------------------------------------------------------
106880130306       BEGSR  sr_WrtRec_PdfList;
106890130306
106900130307         // -?NON richiesta stampa del PDF?
106910140404         if  %parms() = 1;
106920130306           leavesr;
106930130306         endif;
106940130306
106950130306         // -?Reperimento numero "chiave elenco"?(SE non ricevuto)?
106960130306         If  DB6num <= *zero;
106970130306
106980130306           clear  TRUL33ds;
106990130306           I33tla = 'L';
107000130306           I33ope = *zero;
107010130306           I33cnu = 55;
107020130306           I33num = 1;
107030130306           kpjbu  = TRUL33ds;
107040130306
107050130306           TRUL33R ( kpjba );
107060130306
107070130306           TRUL33ds = kpjbu;
107080130307           if  O33err > *zero;
107090130307             Pmsg1  = $Msg(3);
107100130307             Pmsg2  = $Msg(O33err + 3);
107110130418             *in33  = *off;
107120130418             exsr  Posiz_Box;
107130130307             write  A4prtMSG;
107140130307             DB0tms = 'E';
107150130306             exsr  sr_End;
107160130306           endif;
107170130306
107180130306           DB6num = %editc( O33nri : 'X' );
107190130306
107200130306         EndIf;
107210130307
107220130307         // -?Apertura file?
107230130307         if  Not %open(PDFLIST);
107240130307           open  PDFLIST;
107250130307         endif;
107260130306
107270130308         // -?Ciclo di lettura record "§..." (ex "§PDF") in FIAR801L?
107280130306         clear  k06fiar801;
107290130306         k_AR8aas = DB0aas;
107300130306         k_AR8lnp = DB0lnp;
107310130306         k_AR8nrs = DB0nrs;
107320130306         k_AR8nsp = DB0nsp;
107330130306         k_AR8tmd = c_§PDF;
107340130306         setll  %kds( k06fiar801 : 5 )  FIAR8000;
107350130308         reade  %kds( k06fiar801 : 4 )  FIAR8000;
107360130306
107370130308         DoW  Not %eof(FIAR801L)  and  %subst(AR8tmd : 1 : 1) = '§';
107380130306
107390130306           dAR8§PDF = AR8uni;
107400130308
107410130315           If  AR8atb = *blank  and  §AR8Ptrt = 'S';
107420130306
107430130308             // -?Scrittura del rec. relativo alla sped. nel *file PDFLIST?
107440130308             clear  RPDF;
107450130308             PDFklist  = DB6num;
107460130308             PDFksped  = %editc( DB0aas : 'X' ) + %editc( DB0lnp : 'X' ) +
107470130308                         %editc( DB0nrs : 'X' ) + %editc( DB0nsp : 'X' );
107480130311             PDFkey    = %trimr(§AR8PpdfP) + '_' + §AR8PpdfN;
107490130311             //PDFpath   = *blank;
107500130308             PDFinfo   = c_CopiaXdest + ' - ' + Prsd +
107510130308                         ' Spedizione ' + %editc(DB0lnp : 'X') +
107520130308                                    ' ' + %editc(DB0nrs : 'X') +
107530130308                                    ' ' + %editc(DB0nsp : 'X') +
107540130308                         ' del ' + %editc(Pdsp : 'Y') + ' ' +
107550130308                         Pndc;
107560130308             PDFresol  = 'S';
107570130308             PDFcopieM = '1';
107580130308             PDFcopieD = '0';
107590130308             PDFcopieS = '0';
107600130308             //PDFnoInfo = *blank;
107610130308             //PDFinfoP  = *blank;
107620130308             //PDFprtFst = *blank;
107630130308             wTime_ds  = %char( %timestamp() );
107640131008             PDFinsDt  = wTime_ds.wDate_Iso;
107650131008             PDFinsTm  = wTime_ds.wTime;
107660131008             //PDFprtDt  = wTime_ds.wDate_Iso;
107670131008             //PDFprtTm  = wTime_ds.wTime;
107680130308             PDFusrins = SDSusr;
107690130308             //__________
107700130308             write  RPDF;
107710130308             //¯¯¯¯¯¯¯¯¯¯
107720130308
107730130308           EndIf;
107740130306
107750130418           reade  %kds( k06fiar801 : 4 )  FIAR8000;
107760130306
107770130306         EndDo;
107780130306
107790130306       ENDSR;
107800130306
107810130306       //--------------------------------------------------------------
107820140303       //?Stampa immagini associate alle spediz. (dati già in PDFLIST) ?
107830130306       //--------------------------------------------------------------
107840130306       BEGSR  sr_PrtPdf;
107850130306
107860130306         // -?Impostazione parametri?
107870130312         PDF_KList    = DB6num;
107880130312         PDF_Printer  = trulPOVRds.§ULPoOutQ;
107890130312         PDF_FormType = trulPOVRds.§ULPoFType;
107900130312         PDF_UsrDta   = trulPOVRds.§ULPoUDta;
107910130306
107920130306         // -?Richiamo funzione di stampa di tutte le immagini PDF?
107930130312         //  ?(segnalando - in stampa - l'eventuale errore)?
107940130312         callp(E)  Stampa_PDF ( PDF_KList :
107950130312                                PDF_Printer :
107960130312                                PDF_FormType :
107970130312                                PDF_UsrDta );
107980130312
107990130312         if  %error;
108000130312           Pmsg2  = $Msg(7);
108010130418           *in33  = *off;
108020130418           exsr  Posiz_Box;
108030130312           write  A4prtMSG;
108040130312           DB0tms = 'E';
108050130312           exsr  sr_End;
108060130312         endif;
108070130307
108080130307         // -?Restituzione tipo modulo stampato?
108090130307         DB0tms = 'F';
108100130306
108110130306       ENDSR;
108120130306
108130130306      /end-free
108140030903      *
108150030515      *---------------------------------------------------------------*
108160050217      *?DEFINIZIONE KEY-LIST                                         ?*
108170030515      *---------------------------------------------------------------*
108180030515     c     DefKLIST      BEGSR
108190030515      *
108200051115      * - FNBLP01L / FNBLT01L /
108210060505      *   FNARB01L / FNART01L /
108220030515      *   FNLBL01L /
108230060505      *   FIAR501L/31C / FIAR901L
108240040607     c     KLDV          klist
108250030515     c                   kfld                    DB0aas
108260030515     c                   kfld                    DB0lnp
108270030515     c                   kfld                    DB0nrs
108280030515     c                   kfld                    DB0nsp
108290060505      * - FIAR401L / FIAR601L / FIAR701L
108300060505     c     KLDVtrc       klist
108310060505     c                   kfld                    DB0aas
108320060505     c                   kfld                    DB0lnp
108330060505     c                   kfld                    DB0nrs
108340060505     c                   kfld                    DB0nsp
108350060505     c                   kfld                    Wtrc
108360040607      * - FIAR501L/31C
108370030717     c     K05AR501      klist
108380030717     c                   kfld                    DB0aas
108390030717     c                   kfld                    DB0lnp
108400030717     c                   kfld                    DB0nrs
108410030717     c                   kfld                    DB0nsp
108420030717     c                   kfld                    AR5trd
108430040607      * - FIAR801L
108440040607     c     K06AR801      klist
108450040607     c                   kfld                    DB0aas
108460040607     c                   kfld                    DB0lnp
108470040607     c                   kfld                    DB0nrs
108480040607     c                   kfld                    DB0nsp
108490040607     c                   kfld                    AR8tmd
108500040607     c                   kfld                    AR8prr
108510070514      * - FNBLT01L / FNART01L
108520070514     c     KLDVnsc       Klist
108530070514     c                   Kfld                    DB0aas
108540070514     c                   Kfld                    DB0lnp
108550070514     c                   Kfld                    DB0nrs
108560070514     c                   Kfld                    DB0nsp
108570070514     c                   Kfld                    Wnsc
108580070319      * - FIARN11L
108590070319     c     K06ARN11      klist
108600070319     c                   kfld                    DB0aas
108610070319     c                   kfld                    DB0lnp
108620070319     c                   kfld                    DB0nrs
108630070319     c                   kfld                    DB0nsp
108640070319     c                   kfld                    ARNcdn
108650070319     c                   kfld                    ARNgst
108660090528      * - FNBLP01L/ / FNARB01L / FIAR901L di bolla MAMMA
108670070514     c     KLBL          klist
108680070514     c                   kfld                    LBLaap
108690070514     c                   kfld                    LBLlpp
108700070514     c                   kfld                    LBLnrp
108710070514     c                   kfld                    LBLnsp
108720070514      * - FNBLT01L / FNART01L di bolla MAMMA
108730070514     c     KLBLnsc       Klist
108740070514     c                   kfld                    LBLaap
108750070514     c                   kfld                    LBLlpp
108760070514     c                   kfld                    LBLnrp
108770070514     c                   kfld                    LBLnsp
108780070514     c                   Kfld                    Wnsc
108790070514      * - FIARS01L
108800070514     c     K05ARS01P     klist
108810070514     c                   kfld                    BLTfls
108820070514     c                   kfld                    BLTlna
108830070514     c                   kfld                    BLTnrs
108840070514     c                   kfld                    BLTnsc
108850070514     c                   kfld                    ARStrc
108860070514     c     K05ARS01A     klist
108870070514     c                   kfld                    ARTfls
108880070514     c                   kfld                    ARTlna
108890070514     c                   kfld                    ARTnrs
108900070514     c                   kfld                    ARTnsc
108910070514     c                   kfld                    ARStrc
108920060505      * - FNORM01L
108930060505     c     K04ORM01      klist
108940071015     c                   kfld                    ORMpoe
108950071015     c                   kfld                    ORMnsr
108960071015     c                   kfld                    ORMnor
108970071015     c                   kfld                    ORMnrv
108980030515      * - TABEL00F
108990030515     c     K03TAB        klist
109000040223     c                   kfld                    TBLkut
109010030515     c                   kfld                    TBLcod
109020030515     c                   kfld                    TBLkey
109030030515     c     K02TAB        klist
109040040223     c                   kfld                    TBLkut
109050030515     c                   kfld                    TBLcod
109060030515      * - FLNUF01L
109070030515     c     K04NUF        klist
109080030515     c                   kfld                    NUFaaa
109090030515     c                   kfld                    NUFcnu
109100030515     c                   kfld                    NUFfil
109110030515     c                   kfld                    NUFfl2
109120030515      *
109130030515     c                   ENDSR
109140030515
109150030515** > $Msg ---------------------------------------------------------------------*
109160030515IMPOSSIBILE CONTINUARE LA RISTAMPA BOLLE
109170071112PERCHE' STAMPATE PIU' DI &NrLdV BOLLE !!!
109180130307ERRORE IN REPERIMENTO NUMERATORE PER STAMPA PDF ALLEGATO ALLA LDV:
109190130307   NON REPERITO IL NUMERATORE
109200130307   NUMERATORE ALLOCATO
109210130307   SUPERATO IL LIMITE MASSIMO PER IL NUMERATORE
109220130312RILEVATO ERRORE IN FASE DI STAMPA DEI DOCUMENTI ALLEGATI
109230140303** > $GgS/$Gio *
1092400306241LUNEDI
1092500306242MARTEDI
1092600410263MERCOLEDI
1092700306244GIOVEDI
1092800306245VENERDI
1092900305156SABATO
1093000305157DOMENICA
109310030515M          MATT.
109320030515P          POM.
