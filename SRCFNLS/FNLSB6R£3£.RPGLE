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
016840170717if  1c                   if        DB0lna <> D55lin
016850170717     c                   reset                   FNLV55ds
016860170717     c*** già così:      eval      D55tpt =  '6'
016870170717     c                   eval      D55lin =  DB0lna
016880170717     c                   eval      D55drf =  Wdsp
016890170717     c                   call      'FNLV55R'
016900170717     c                   parm                    FNLV55ds
016910170717if  2c                   if        D55err <> *blanks
016920170717     c                   eval      D55tfa =  DB0lna
016930170717e   2c                   endif
016940170717e   1c                   endif
016950170717      * - Reperimento 146ª e 148ª descrizioni dell'organigramma
016960170717      *   (flag Tellus del capofila e flag gestione telefonate GEO)
016970170718if  1c                   if        D55tfa <> ORGfil
016980170717     c     D55tfa        chain     AZORG
016990170718e   1c                   endif
017000170718if  1c                   if        %found(AZORG01L)
017010170717     c                   eval      wDB0lnaCf£6 = ORGfil
017020170717     c                   eval      wDB0dlaCf£6 = ORGdes
017030170717     c                   eval      wDB0inlCf£6 = ORGind
017040170717     c                   eval      wDB0calCf£6 = ORGcpf
017050170717     c                   eval      wDB0lolCf£6 = ORGloc
017060170717     c                   eval      wDB0prlCf£6 = ORGpro
017070170717     c                   eval      wDB0telCf£6 = ORGtel
017080170717     c*//                eval      wDB0faxCf£6 = ORGfax
017090170717     c                   eval      wDB0faxCf£6 = ORGdd2
017100170717     c                   movel     ORGde6        OG146cf£6
017110170717     c                   movel     ORGde8        OG148cf£6
017120170718x   1c                   else
017130170717     c                   clear                   wDB0lnaCf£6
017140170717     c                   clear                   wDB0dlaCf£6
017150170717     c                   clear                   wDB0inlCf£6
017160170717     c                   clear                   wDB0calCf£6
017170170717     c                   clear                   wDB0lolCf£6
017180170717     c                   clear                   wDB0prlCf£6
017190170717     c                   clear                   wDB0telCf£6
017200170717     c                   clear                   wDB0faxCf£6
017210170717     c                   clear                   OG146cf£6
017220170717     c                   clear                   OG148cf£6
017230170718e   1c                   endif
017240070717      *
017250070717      *** Imposta flag attivazione GEO x LNA
017260070717     c                   eval      wGeoCon = A§OGtlls
017270070717     c                   eval      wGeoTel = A§OGgeot
017280070717if  1c                   if             A§OGdda  > *zeros
017290070717     c                             and  A§OGdda  < C_DateMax_A
017300070717     c                             and  A§OGdda <= wDateA
017310110427     c                   eval      $GeoAtt = *on
017320070717e   1c                   endif
017330070717      *
017340070717if  1c                   IF        DB0tbo = 'A'  AND
017350070717     c                                 (wGeoCon <> 'S'
017360070717     c                              or  wGeoTel <> 'S'
017370110427     c                              or  $GeoAtt <> *on)
017380070717      *** Imposta flag attivazione GEO x capofila £6
017390070717     c                   eval      wGeoCon = £6OGtlls
017400070717     c                   eval      wGeoTel = £6OGgeot
017410071109if  2c                   if             £6OGdda  > *zeros
017420070717     c                             and  £6OGdda  < C_DateMax_A
017430070717     c                             and  £6OGdda <= wDateA
017440110427     c                   eval      $GeoAtt = *on
017450071109e   2c                   endif
017460070717      *
017470070212e   1c                   ENDIF
017480050510      *
017490050510      * Verifica se richiesta stampa in A4 in tab. "7R"
017500050510      *   (gestione particolarità consegna)
017510050510     c                   clear                   ds7R
017520170717if  1c                   if        DB0gma <> *blanks
017530070312     c                   eval      TBLcod =  '7R'
017540050510     c                   movel(p)  DB0gma        TBLkey
017550050510     c     K03TAB        chain     TABEL
017560050510if  2c                   if        %found(TABEL00F)
017570050510     c                   movel     TBLuni        ds7R
017580050510e   2c                   endif
017590050510e   1c                   endif
017600071015      *
017610071015      * Verifica se LDV con "Ritiro Porto Franco Prepagato"
017620071015     c                   clear                   dsBL4M
017630071015     c                   if        DB0tbo = '8'
017640071015     c                   exsr      RepAR4_M
017650071015     c                   endif
017660130315      *
017670130315      * Reperimento dati da FIAR5 / "GEN"
017680130315      *   (per verificare se esistono Packing-List e/o PDF allegati
017690130315      *    - oltre che per reperire Nominativo e Telefono)
017700130315     c                   exsr      RepAR5gen
017710040607      *
017720130315      * Verifica se LDV con DDT e/o PDF
017730130315     c                   exsr      CercaDDToPDF
017740061031      *
017750071114      * Verifica se richiesta stampa in A4 per richiesta Codice Fiscale
017760071114      *   e/o Partita IVA al cliente.
017770120503      *-=»>?01/08/2008:?                                          <«=-*
017780080801      *-=»> Abolite richieste di Codice Fiscale e/o Partita Iva.  <«=-*
017790080801      *-=»> Tolta ricerca di 'sti 2 dati, lasciando il defualt di <«=-*
017800080801      *-=»> NON richiesta: $No_CF = *off e $No_PI = *off.         <«=-*
017810120503      *-=»>?03/05/2012:?                                          <«=-*
017820120503      *-=»>?Ripristinate richieste di Cod. Fiscale e/o Part. Iva.?<«=-*
017830120503if  1c                   if        DUTlpo <> 'S'
017840120503     c                   exsr      CheckCFPI
017850120503e   1c                   endif
017860070306      *
017870070319      * Verifica se richiesta stampa in A4 per note in FIARN00F
017880070306      *   (da stampare nel box "Note per l'autotrasportatore")
017890070319      * » se stampa in arrivo
017900070319if  1c                   if        DB0tbo = 'A'
017910070306     c                   exsr      CheckFIARN
017920070319e   1c                   endif
017930090302      /free
017940090302
017950090302         // Verifica se richiesta stampa in A4 per note specifiche
017960090302         //   per il cliente mittente (v. tab. "PXA")
017970090302         //   (da stampare nel box "Note per l'autotrasportatore")
017980090302         exsr  CheckTabPXA;
017990090924
018000090924         // Verifica se richiesta stampa in A4 per note specifiche
018010090924         //   per il destinatario (da parte del mittente)
018020090924         //   (da stampare in un box a parte)
018030090924         if  DB0ccm = c_CliSG;
018040090924           $BoxNxD = *on;
018050090924         endif;
018060090302      /end-free
018070070319      *
018080070319      * Verifica se richiesta spedizione già parzialmente consegnata
018090070319      *   (da stampare nel box "Note per l'autotrasportatore")
018100070717      * » se stampa in arrivo e LNA - o capofila £6 - con GEO attivato
018110070717      * completamente
018120070319if  1c                   if             DB0tbo   =  'A'
018130070514     c                             and  wB0dcp   >  *zeros
018140110427     c                             and  $GeoAtt  =  *on
018150140514      * -?Reperimento consegne parziali precedenti da FNART?
018160140514     c                   exsr      SchConsParz
018170140514     c                   if        xx > *zero
018180070413     c                   eval      $ConsParz     =  *on
018190140514     c                   endif
018200070319e   1c                   endif
018210070315      *
018220070315      * Verifica se richiesta stampa in A4 per C.A. aperta
018230070717      * » se stampa in arrivo e LNA - o capofila £6 - con GEO attivato
018240070717      * completamente
018250070319if  1c                   if             DB0tbo   =  'A'
018260110427     c                             and  $GeoAtt  =  *on
018270070316     c                   exsr      CercaCA
018280070316e   1c                   endif
018290030515      *
018300030515      * Tipo modulo (per la separazione dei box: vettore/destinatario)
018310030515      *   (*in69 = formato A5 / not *in69 = formato A4)
018320030515     c                   eval      *in69  = *on
018330030515      *
018340040607      * Tipo LDV (per richiesta firma per ricevuta della merce senza
018350040607      *   riserve)
018360120530      *   (70 = BRT, 71 = DPD, 72 = FedEx, 73 = EuroExpress)
018370080912     c                   movea     '1000'        *in(70)
018380081210      /free
018390081210         // - LDV DPD
018400170517         //if  §OGntw = 'DPD'   -  ?NON della LnP, MA della fil. mitt.:?
018410081210         if  §CMntw = 'DPD';
018420081210           *in71 = *on;
018430081210         endif;
018440081210      /end-free
018450040311      *
018460030515sel 1c                   SELECT
018470030515      *
018480030515      * Rimesso Mittente: va bene il formato A5
018490030515      *   (se non richiesta la forzatura del formato "A4")
018500040707w   1c                   WHEN      DB0ris = 'M'   and  DB0fa4 <> 'S'
018510040707      *
018520040707      * LDV POSTE non valida per la consegna: va bene il formato A5
018530040707      *   (se non richiesta la forzatura del formato "A4")
018540040707w   1c                   WHEN      §OGntw = 'PPT' and  DB0fa4 <> 'S'
018550071213      *
018560081210     *** * LDV DPD
018570081210     *** > (Spostato prima per NON rendere incompatibile DPD con il box
018580081210     *** >  di note aggiuntive "promemoria per l'autotrasportatore")
018590081210w   1***c*** non della LNP  WHEN      §OGntw = 'DPD' - ma della fil. mitt.:
018600081210w   1***c                   WHEN      §CMntw = 'DPD'
018610081210     ***c                   movea     '0100'        *in(70)
018620071213      *
018630071213      * LDV con mittente FedEx
018640150429w   1c                   WHEN      DB0ccm = c_CliFedEx   or
018650150429     c                             DB0ccm = c_CliFedEx2
018660080922     c                   movea     '0010'        *in(70)
018670071213     c                   eval      *in69  = *off
018680040607      *
018690040607      * LDV con DDT del mittente
018700081013      * - IN FUTURO: se al 1° tentativo di consegna
018710081013      *              (DB0FL1 = "D" - v. pgm FNLRS9R)
018720080917w   1c                   WHEN           $FIAR8 =  *on
018730081013     c***-=»>                      and  DB0fl1 <> 'D'
018740040607     c                   eval      *in69  = *off
018750061102      *
018760071114      * Codice Fiscale o Partita IVA da richiedere al cliente
018770071114w   1c                   WHEN           $No_CF = *on
018780071114     c                             or   $No_PI = *on
018790061102     c                   eval      *in69  = *off
018800070306      *
018810070306      * LDV con Note (dal file FIARN00F)
018820070306w   1c                   WHEN      $FIARN = *on
018830070306     c                   eval      *in69  = *off
018840080801      *
018850080801      * LDV già parzialmente consegnata
018860080801w   1c                   WHEN      $ConsParz   =  *on
018870080801     c                   eval      *in69  = *off
018880070315      *
018890070315      * LDV con C.A. aperta per la spedizione (dal file FNDCT00F)
018900070315w   1c                   WHEN      $C_A_  = *on
018910070315     c                   eval      *in69  = *off
018920060524      *
018930070123      * LDV con mittente "PraOil oleodotti italiani"
018940081127w   1c                   WHEN           DB0ccm = c_CliPraOil
018950060524     c                             or  (DB0fbr = 'S'
018960081127     c                             and  §B4Aks = c_CliPraOil)
018970060524     c                   eval      *in69  = *off
018980090302      /free
018990090302
019000090302           // LDV con mittente con promemoria per AUT in tab. "PXA"
019010110411           WHEN  $TabPXA = *on  or  $TabPXA2 = *on;
019020090302             *in69  = *off;
019030090924
019040090924           // LDV con mittente con promemoria per destinatario
019050090924           WHEN  $BoxNxD = *on;
019060090924             *in69  = *off;
019070090302      /end-free
019080030515      *
019090120530      * LDV BRT
019100150224     *** * - con Bancali da rendere al mittente
019110150224     *** *   o   Bancali da riportare a magazzino
019120150224      * - con Destinatario supermercato
019130030515      *   e/o Promemoria per autotrasportatore
019140040223      *   [Se non trovo niente su FIAR5 né x supermercati né x bancali
019150030515      *    ma ho queste condizioni: devo stampare la bolla in "A4" per
019160030515      *    poter stampare comunque il box vuoto con la memo "Destina-
019170050510      *    tario Supermercato" per poter scrivere a mano i promemoria
019180050510      *    per l'autotrasportatore - ci dovrebbero comunque sempre
019190050510      *    essere note per supermercato.]
019200050510      *   e/o con formato A4 previsto in tab. "7R" (partic. consegna)
019210150224w   1c*//                WHEN         %subst(DB0gva:1:1) = 'B'
019220150224     c*//                          or %subst(DB0gva:1:1) = 'O'
019230150224w   1c                   WHEN                DB0tc1      = 'S'
019240030717     c                             or        DB0tc2      = 'S'
019250071015     c                             or        §7Rldv      = 'S'
019260030515     c                   eval      *in69  = *off
019270071015      *
019280071015      * LDV con ritiro porto franco prepagato
019290071015w   1c                   WHEN          (DB0tbo   = '8'
019300071015     c                             and  §B4poe   > *zeros
019310071015     c                             and  §B4nor   > *zeros)
019320071015     c                   eval      *in69  = *off
019330071015      *
019340071015      * LDV con ritiro contestuale alla consegna
019350100608w   1c**!!!              WHEN          (DB0gma   = Ktab7V_RC
019360100608w   1c                   WHEN          (§7Rrc    = 'S'
019370071015     c                             and  §7Rldv   = 'S')
019380071015     c                   eval      *in69  = *off
019390030515      *
019400030515      * - con Forzatura richiesta al formato "A4"
019410030515w   1c                   WHEN      DB0fa4 = 'S'
019420030515     c                   eval      *in69  = *off
019430030515      *
019440030515e   1c                   ENDSL
019450050513      *
019460050513      * Verifica se con Segnacolli NON sequenziali
019470050513      * (di numero superiore a quelli che stanno al posto del range)
019480050513      * DA CONTROLLARE COMUNQUE (A4 o A5 che sia), VISTO CHE:
019490050513      * - se è previsto un A5, potrebbe scattare l'A4 (per numero
019500050513      *   di segnacolli da elencare troppo alto per lo spazio
019510050513      *   disponibile tra le "NOTE E PARTICOLARITA'")
019520050513      * - se è già previsto un A4, i segnacolli non sequenziali
019530050513      *   potrebbero comunque essere elencabili tra le "NOTE E
019540050513      *   PARTICOLARITA'" (e risparmiare una pagina in caso di
019550050513      *   LdV con DDT del mittente)
019560050513     c*** (già fatto:)   reset                   $A4_x_ScNS
019570050513if  1c                   IF             DB0fns = 'S'
019580050513     c                             and  DB0ncl >  2
019590050513      * - - imposto le righe di "Note e Particolarità" da stampare
019600050513     c                   exsr      SchPARTIC
019610050513      * - - controllo quante righe di "Note e Particolarità" servireb-
019620050513      *     bero per segnalare i segnacolli non sequenziali, in base
019630050513      *     al numero di colli (considerando che riuscirei ad elencare
019640050513      *     max 4 segnacolli per ogni riga di "Note e Particolarità")
019650050513     c                   eval      WelSNS = $elSNSxA5
019660050513     c     DB0ncl        div       WelSNS        YY
019670050513     c                   mvr                     XX
019680050513if  2c                   if        XX     >  0
019690050513     c                   eval      XX     =  YY + 1
019700050513x   2c                   else
019710050513     c                   eval      XX     =  YY
019720050513e   2c                   endif
019730050513      * - - conto anche la riga per l'intestazione
019740050513     c                   add       1             XX
019750050513      * - - conteggio le righe di "Note e Particolarità" utilizzate
019760050513      *     per reperire la 1ª riga di "Note e Particolarità" disponi-
019770050513      *     bile
019780050513     c                   eval      *in31  =  *off
019790050513     c                   eval      YY     =  1
019800050513     c     *blanks       lookup    $NP(yy)                                31
019810050513if  2c                   if        *in31  =  *on
019820050513     c                   eval      YY     =  YY - 1
019830050513x   2c                   else
019840050513     c                   eval      YY     =  %elem($NP)
019850050513e   2c                   endif
019860050513      * - - controllo se ho il numero di campi "Note e Particolarità"
019870050513      *     liberi a sufficienza per i segnacolli da elencare.
019880050513      *     Se non ci stanno => serve un modulo A4
019890050513if  2c                   if        XX > %elem($NP) - YY
019900050513     c                   eval      $A4_x_ScNS  = *on
019910050513     c                   eval      *in69  = *off
019920050513e   2c                   endif
019930050513e   1c                   ENDIF
019940030515      *
019950030515      * Imposto il numero di riga massima raggiungibile per l'eventuale
019960030515      *  stampa dei segnacolli non sequenziali
019970040607if  1c                   if        *in69
019980030515     c                   eval      wMaxLine = $MaxLine1
019990040607x   1c                   else
020000030515     c                   eval      wMaxLine = $MaxLine2
020010040607e   1c                   endif
020020030515      * Memo: ho stampato (sto stampando...) almeno un modulo A4 o A5.
020030030515      *   (utile solo per la segnalazione in stampa delle oltre 6.000
020040071008      *   pagine già stampate - in un formato e/o nell'altro).
020050080926      /free
020060080926         select;
020070100924           //?»»» Richiesta la stampa per PDA (4 LdV x A4)?
020080100924           //    ?di LdV per consegna AL 1° tentativo, ma?
020090100924           //    ?reperita packing-list (da stampare "a parte")?
020100100924           //?(la stampa della relativa packing-list verrà richiesta?
020110100924           // ?successivamente, dopo la stampa di tutte le LdV)?
020120100924           when  DB0fa4  = '4'    and
020130100924               //DB0fl1 <> 'D'    and
020140100924                 *in69   = *off   and
020150130306                 $FIAR8  = *on    and
020160130306                 $§PDF   = *off;
020170100924             DB0tms = 'D';
020180130306           //?»»» Richiesta la stampa per PDA (4 LdV x A4)?
020190130306           //    ?di LdV per consegna AL 1° tentativo, ma?
020200130306           //    ?reperita immagine PDF (da stampare "a parte")?
020210130306           //?(la stampa delle relative immagini verranno richieste?
020220130306           // ?successivamente, dopo la stampa di tutte le LdV e?
020230130306           // ?delle relative packing-list)?
020240130306           when  DB0fa4  = '4'    and
020250130306                 $FIAR8  = *off   and
020260130306                 $§PDF   = *on;
020270130307             DB0tms = 'i';
020280130306           //?»»» Richiesta la stampa per PDA (4 LdV x A4)?
020290130306           //    ?di LdV per consegna AL 1° tentativo, ma?
020300130306           //    ?reperita packing-list (da stampare "a parte") e?
020310130306           //    ?immagine PDF (anch'essa da stampare "a parte")?
020320130306           //?(la stampa della relativa packing-list e delle relative?
020330130306           // ?immagini verranno richieste successivamente, dopo la?
020340130306           // ?stampa di tutte le LdV)?
020350130306           when  DB0fa4  = '4'    and
020360130306               //DB0fl1 <> 'D'    and
020370130306                 *in69   = *off   and
020380130306                 $FIAR8  = *on    and
020390130306                 $§PDF   = *on;
020400130306             DB0tms = 'Y';
020410100924           //?»»» Richiesta la stampa per PDA (4 LdV x A4)?
020420080926           when  DB0fa4 = '4';
020430100927             DB0tms = 'Q';
020440100924           //?»»» Richiesta la stampa della packing-list per PDA?
020450081007           when  DB0fa4 = 'P'   and   $FIAR8 = *on;
020460080926             DB0tms = 'P';
020470081007           when  DB0fa4 = 'P'   and   $FIAR8 = *off;
020480081007             //DB0tms = *blank;
020490130307           //?»»» Richiesta la stampa normale e reperita immagine PDF?
020500130307           when  *in69  and  $§PDF;
020510130307             DB0tms = 'I';
020520130307           when  Not *in69  and  $§PDF;
020530130307             DB0tms = 'J';
020540100924           //?»»» Richiesta la stampa normale?
020550080926           when  *in69;
020560080926             DB0tms = '5';
020570080926           other;
020580080926             DB0tms = '4';
020590080926         endsl;
020600080926      /end-free
020610040707      *
020620040707      * Controllo se devo stampare "Non utilizzare per la consegna"
020630040707      * nella copia per il vettore,?senza?copia per il destinatario:
020640040707     c*** (già fatto:)   reset                   $No_x_Cons
020650071203      * - LdV recapito c/assegni:
020660090511      *   stampata in arrivo e con
020670120113if  1c                   if             DB0tbo <> 'P'
020680070306     c                             and  DB0tbo <> '8'
020690090511     c                             and  DB0lnp =  102
020700070306     c                             and  DB0cbo =  'FW'
020710071203     c                             and  DB0nft =  *zeros
020720071203      *   - fil. arrivo con GEO attivato
020730071203     c                             and (DB0fl1    =  'A'
020740071203     c                              or  wGeoCon   =  'S'
020750071203      *   - fil. arrivo abilitata al picking
020760120113     c                              or  A§OGpck   =  'S')
020770070306     c                   eval      $No_x_Cons  =  *on
020780120113e   1c                   endif
020790120113      *//* - LdV Poste
020800120113w   1c*//                   when      §OGntw   = 'PPT'
020810120113     c*//                   eval      $No_x_Cons  =  '2'
020820101125      /free
020830101125         // -?Controlla se "tagliatella" per Ritorno all'Incasso?
020840101129         //$Rit_a_Inc = ( DB0fa4  = '4'    and
020850101129         //               DB0tbo <> 'P'    and  DB0tbo <> '8'   and
020860101129         //               wB0dcm  > *zero  and
020870101129         //           ( ( DB0cas >  *zero  and  wB0icc =  'R' )  or
020880101129         //             ( DB0ift >  *zero  and  wB0ica =  'R' ) ) );
020890101129         // -?Controlla se LdV per Ritorno all'Incasso?
020900101129         $Rit_a_Inc = ( DB0fl4 <> *blank and
020910101129                        DB0tbo <> 'P'    and  DB0tbo <> '8'   and
020920101129                        wB0dcm  > *zero  and
020930101129                    ( ( DB0cas >  *zero  and  wB0icc =  'R' )  or
020940101129                      ( DB0ift >  *zero  and  wB0ica =  'R' ) ) );
020950101125      /end-free
020960030515      *
020970030515     c                   ENDSR
020980040607      *
020990040607      *---------------------------------------------------------------*
021000130315      *?Verifica esistenza dati del DDT allegato da stampare  o      ?*
021010130315      *?               di uno o più PDF allegati da stampare         ?*
021020040607      *---------------------------------------------------------------*
021030130315     c     CercaDDToPDF  BEGSR
021040040607      *
021050060921     c*** (da non fare:) reset                   $E_AR8
021060060921     c*** (già fatto:)   reset                   $FIAR8
021070130315     c*** (già fatto:)   reset                   $§PDF
021080040607     c*** (già fatto:)   reset                   $DDT_o
021090040607     c*** (già fatto:)   reset                   $DDT_p
021100040608     c*** (già fatto:)   reset                   $DDTlastPg
021110040607     c*** (già fatto:)   reset                   $AR8o1
021120040608     c*** (già fatto:)   reset                   wOmaRowT
021130040608     c*** (già fatto:)   reset                   wOmaRowD
021140041207     c*** (già fatto:)   reset                   dAR8
021150130315      *
021160130315      * -?Verifica esistenza PDF allegato?
021170130315     c                   eval      $§PDF  = (§AR5als = 'S')
021180130315      *
021190130315      * -?Verifica esistenza packing-list?
021200130315     c                   eval      $FIAR8 = (§AR5alx = 'S')
021210130315      *
021220130315if  1c                   if        NOT %open(FIAR801L)
021230130315     c                             and ($FIAR8  or  $§PDF)
021240130315     c                   open(e)   FIAR801L
021250130315      * Se errore significa che non c'è il file per cui non faccio
021260130315      *  più questa routine
021270130315if  2c                   if        %error
021280130315     c                   eval      $E_AR8 = *off
021290130315e   2c                   endif
021300130315e   1c                   endif
021310040607      *
021320130306     c                   ENDSR
021330061102      *
021340061102      *---------------------------------------------------------------*
021350071114      *?Verifica se Codice Fiscale e/o Partita Iva da richiedere al  ?*
021360071114      *?  cliente                                                    ?*
021370061102      *---------------------------------------------------------------*
021380061102      *  Il d.l. 223/06, poi convertito con la legge 248/06, ha      *
021390061102      *   reintrodotto l'obbligo di raccogliere il codice fiscale per *
021400061102      *   tutti i clienti e fornitori, siano essi aziende o privati.  *
021410061102      * SE?Porto Franco PrePagato?:                                   *
021420061102      *   il Cod.Fiscale dovrà essere del?Mittente?.                  *
021430061102      * SE?Porto Assegnato con Fattura Immediata?:                    *
021440061102      *   il Cod.Fiscale dovrà essere del?Destinatario?.              *
021450061102      * In sua assenza: dovrà essere richiesto al cliente stesso!     *
021460071114      *  Lo stesso decreto (poi convertito con la legge 248/06)      *
021470071114      *   prevede che a partire dal 01/01/2008 debba essere raccolta  *
021480071114      *   anche l'eventuale partita iva, del cliente o del fornitore. *
021490071114      * Stessi criteri già utilizzati per il codice fiscale.          *
021500061102      *---------------------------------------------------------------*
021510071114     c     CheckCFPI     BEGSR
021520061102      *
021530061106     c*** (già fatto:)   eval      $No_CF  = *off
021540071114     c*** (già fatto:)   eval      $No_PI  = *off
021550061102      *
021560071114      * Verifica se C.F./P.I. da richiedere (NON per clienti esteri)
021570071114      * - con fattura
021580071114if  1c                   IF            (DB0nft  <> *zeros
021590061106     c                              or  DB0ift  <> *zeros)
021600061103     c                             AND (
021610061102      * - - P.F. prepagato
021620061107      *     (con mittente italiano)
021630061107     c                                 (DB0tbo  =  '8'
021640061107     c                              and DB0nzm  =  *blanks)
021650061106      * - - P.A. (con fattura immediata)
021660061107      *     (con destinatario italiano)
021670061107     c                             or  (%subst(DB0tb1:1:1) = 'A'
021680061107     c                              and DB0nzd  =  *blanks)
021690061106      * - - con 2ª bolla
021700061107      *     (con destinatario italiano)
021710061103     c                             or  (DB0tb2  <> *blanks
021720121029     c                              and DB0tbo  <> 'P'
021730061107     c                              and DB0tbo  <> '8'
021740061107     c                              and DB0nzd  =  *blanks)
021750071114      * - => verirficare se da richiedere C.F. e/o P.I.
021760061103     c                                 )
021770120503      *
021780120503sel 2c                   select
021790120503      * · Esiste Partita Iva NON FITTIZIA: non chiedere nulla
021800120503w   2c                   when      wB0piv <> *blanks   and
021810120503     c                             %scan('$$' : wB0piv) = *zero
021820120503     c                   leavesr
021830120503      * · Esiste C.F. lungo 16 (privato) : non chiedere P.I.
021840120503w   2c                   when      %len( %trim( wB0cfi ) ) = 16
021850120503     c                   leavesr
021860120503      * · Esiste C.F. lungo 11 ..........: chiedere P.I. (se mancante)
021870120503w   2c*//                when      %len( %trim( wB0cfi ) ) = 11
021880120503e   2c                   endsl
021890071114      *
021900071114      * - senza codice fiscale
021910120503     c                   eval      $No_CF  = (wB0cfi = *blanks)
021920071114      *
021930071114      * - senza partita iva (se NON privato)
021940120503     c                   eval      $No_PI  = (wB0piv = *blanks)
021950071114      *
021960071114e   1c                   ENDIF
021970061102      *
021980061102     c                   ENDSR
021990070306      *
022000070306      *---------------------------------------------------------------*
022010070306      *?Verifica esistenza note da stampare (dal file FIARN00F)      ?*
022020070306      *---------------------------------------------------------------*
022030070306     c     CheckFIARN    BEGSR
022040070306      *
022050070306     c*** (già fatto:)   reset                   $FIARN
022060070306      *
022070070306      * Controllo esistenza note solo se c'è il file
022080070319     c     K06ARN11      chain     FIARN000
022090070319if  1c                   if             %found(FIARN11L)
022100070306     c                             and  ARNnob <> *blanks
022110070306     c                   eval      $FIARN = *on
022120070306e   1c                   endif
022130070306      *
022140070306     c                   ENDSR
022150070315      *
022160070315      *---------------------------------------------------------------*
022170070315      *?Verifica esistenza C.A. aperta legata alla spedizione        ?*
022180070315      *---------------------------------------------------------------*
022190070316     c     CercaCA       BEGSR
022200070315      *
022210070319     c*** (già fatto:)   reset                   $C_A_
022220070315      *
022230070319      * Richiamo del programma di comodo per recuperare le CA legate
022240070319      *   alla spedizione in esame
022250070319     c                   reset                   FIDN12ds
022260070514if  1c                   if        $Mamma = *off
022270070319     c                   eval      I12aas = DB0aas
022280070319     c                   eval      I12lnp = DB0lnp
022290070319     c                   eval      I12nrs = DB0nrs
022300070319     c                   eval      I12nsp = DB0nsp
022310070514x   1c                   else
022320070514     c                   eval      I12aas = LBLaap
022330070514     c                   eval      I12lnp = LBLlpp
022340070514     c                   eval      I12nrs = LBLnrp
022350070514     c                   eval      I12nsp = LBLnsp
022360070514e   1c                   endif
022370070319     c*** già così:      eval      I12fel = 'E'
022380070319     c***  "    "        eval      I12fan = 'E'
022390070319     c***  "    "        eval      I12fch = 'E'
022400070319      *
022410070319     c                   call      'FIDN12R'
022420070319     c                   parm                    FIDN12ds
022430070319      *
022440070319      * Se trovata almeno una CA
022450070319     c                   eval      $C_A_  = (O12nca > *zeros)
022460070315      *
022470070316     c                   ENDSR
022480070319      *
022490070319      *---------------------------------------------------------------*
022500070319      *?Verifica se stampa spedizione DOPO "Lasciato Avviso"         ?*
022510070319      *---------------------------------------------------------------*
022520070319     c     CheckLascAvv  BEGSR
022530070319      *
022540070319     c*** (già fatto:)   reset                   $LascAvv
022550070319      *
022560070319if  1c                   if        NOT  %open(FNEVB05L)
022570070319     c                   open      FNEVB05L
022580070319e   1c                   endif
022590070319      *
022600070319     c     KLDV          setgt     FNEVB000
022610070319     c     KLDV          readpe    FNEVB000
022620070319      *
022630070319      * Proseguo solo se ritrovo un evento
022640070319do  1c                   DOW       NOT  %eof(FNEVB05L)
022650070319      *
022660070319      * Verifico se evento di tipo Giacenza
022670070319     c                   eval      *in31    = *off
022680070319     c     EVBcev        lookup    $2Acev_G                               31
022690070319      **
022700070319sel 2c                   Select
022710070319      * - Evento GIACENZA --> NO LASCIATO AVVISO
022720070319w   2c                   When      *in31
022730070319     c                   leave
022740070319      * - Evento RIC --> 1° LASCIATO AVVISO
022750070319w   2c                   When      EVBcev   = 'RIC'
022760070319     c                   eval      $LascAvv = *on
022770131205      * - - salvataggio data/ora
022780131205     c                   eval      wEVBdev  = EVBdev
022790131205     c                   eval      wEVBhev  = EVBhev
022800070319     c                   leave
022810070319      * - Evento DA NON ELABORARE
022820070319x   2c                   Other
022830070319     c     KLDV          readpe    FNEVB000
022840070319e   2c                   Endsl
022850070319      **
022860070319e   1c                   ENDDO
022870070319      *
022880070319     c                   ENDSR
022890090302      /free
022900090302
022910090302       //--------------------------------------------------------------
022920090302       //?Verifica esistenza note da stampare (dal file TNTBE00F)      ?
022930090302       //--------------------------------------------------------------
022940090302       BEGSR  CheckTabPXA;
022950090302
022960090302         //clear  $TabPXA;          (già fatto)
022970110411         //clear  $TabPXA2;         (già fatto)
022980110411
022990110411         // -?Verifica esistenza "PXA" per particolarità consegna?
023000110411         If  DB0gma <> *blank;
023010170517           clear  k05tntbe01;
023020110411           k_TBEcod = 'PXA';
023030110411           k_TBEke1 = DB0gma;
023040110411           k_TBEke2 = '001            ';
023050110411           setll  %kds(k05tntbe01 : 3)  TNTBE000;
023060110411           // -?Bisogna testare se nota ANNULLATA?
023070110411           //$TabPXA  = %equal(TNTBE01L);
023080110411           reade  %kds(k05tntbe01 : 2)  TNTBE000;
023090110411           $TabPXA2 = (Not %eof(TNTBE01L)   and   TBEatb = *blank
023100110411                       and TBEke2 <= '006            ');
023110110411         EndIf;
023120090302
023130110411         // -?Reperimento eventuali "PXA" per mittente?
023140170517         clear  k05tntbe01;
023150090302         k_TBEcod = 'PXA';
023160090302         k_TBEke1 = %editc(DB0ccm : 'X');
023170100316         k_TBEke2 = '001            ';
023180090302         clear  k_TBElin;
023190090302         clear  k_TBEsif;
023200100316         setll  %kds(k05tntbe01 : 3)  TNTBE000;
023210110411         // -?Bisogna testare se nota ANNULLATA?
023220090302         //$TabPXA  = %equal(TNTBE01L);
023230090302         reade  %kds(k05tntbe01 : 2)  TNTBE000;
023240100316         $TabPXA  = (Not %eof(TNTBE01L)   and   TBEatb = *blank
023250100316                     and TBEke2 <= '006            ');
023260090302
023270090302       ENDSR;
023280090302      /end-free
023290030515      *
023300030515      *---------------------------------------------------------------*
023310050217      *?Imposta i campi di stampa con quelli della DS                ?*
023320030515      *---------------------------------------------------------------*
023330030515     c     PrepPRT       BEGSR
023340040223      *
023350040223      * Reperimento dati da FIAR5 / "GEN"
023360130315     c*** (già fatto:)   exsr      RepAR5gen
023370070212     *** *
023380070212     *** * Impostazione Data spedizione
023390070212     *** * (già fatto nella subr. "TipoLdV")
023400070319      *
023410100218      * CONTROLLO SE STAMPA SPEDIZIONE DOPO "LASCIATO AVVISO"
023420100218      * (?*-=»>   SE NON MESSA IN "FERMO DEPOSITO"   <«=-?):
023430081118      * (NON se richiesta stampa di "ricevuta" - 4 in A4)
023440110427      * per LNA (o capofila se LNA è in £6) con $GeoAtt = *ON =>
023450070717      * GEO attivato completamente
023460070319      * controllo se la spedizione ha già più tentativi di consegna e
023470070319      *   determino se in stato di "lasciato avviso"
023480070319      *
023490070319     c*** (già fatto:)   reset                   $LascAvv
023500070406if  1c                   if             DB0tbo    =  'A'
023510081117     c                             and  DB0fa4    <> '4'
023520070514     c                             and  wB0ntc    >  *zeros
023530110427     c                             and  $GeoAtt   =  *on
023540100218     c                             and  DB0ffd    =  *blank
023550070319     c                   exsr      CheckLascAvv
023560070319e   1c                   endif
023570030515      *
023580120530      * Impotazione dati BRT S.p.A.
023590120530     c*//                if        (DB0dft > *zero       and
023600120530     c*//                           DB0dft > §vpoBRTdta)  OR
023610120530     c*//                          (DB0dft = *zero       and
023620120530     c*//                           Wdsp   > §vpoBRTdta)
023630120530     c                   eval      TBAR   =  $BRT
023640120530     c*//                else
023650120530     c*//                eval      TBAR   =  $Bartolini
023660120530     c*//                endif
023670070307     c                   eval      TBA1   =  $Intest1
023680070307     c                   eval      TBA2   =  $Intest2
023690080707if  1c                   if            DB0nft <> *zeros
023700080707     c                             or  DB0ift <> *zeros
023710080627     c                   eval      TBA3   =  $Intest3
023720080707e   1c                   endif
023730070307     c                   eval      TBA4   =  $Intest4
023740070307     c                   eval      TBA5   =  $Intest5
023750070307     c*** NON sopra:     eval      TBA6   =  $Intest6
023760120530     c*//                eval      TBA7   =  $Intest7
023770120530     c                   eval      TBA7   =  $Intest7_BRT
023780030515      *
023790030515      * - Intestazione moduli
023800080919     c                   eval      Tcxd   =  c_CopiaXdest
023810130726     c*//                if        Not *in36     (NON ancora acceso)
023820130726if  1c                   if        DB0tbo <> '8'  or  DB0ris = 'M'
023830130726     c                   eval      Tcxd2  =  Tcxd
023840130726     c                   endif
023850091008      *
023860091008      /free
023870091008         select;
023880091008           // - Indicazione Copia LDV x cliente
023890091008           when  DB0ris = 'C';
023900091008             Pkco = 'C O P I A';
023910091008           // - Altre indicazioni: R=Ristampa, V=Variazione, ...
023920091008           other;
023930091008             Tkri = DB0ris;
023940091008         endsl;
023950091008      /end-free
023960030515      *
023970030515      * - Codice a barre: ANNO+LNP+NRS+NSP+CKD
023980030515      *   (chiamo il pgm TRUL28R per ricavarne il check digit)
023990030515     c                   reset                   TRUL28ds
024000030515     c                   z-add     DB0aas        BARcdeAAS
024010030515     c                   eval      BARcdeLNP = DB0lnp
024020030515     c                   eval      BARcdeNRS = DB0nrs
024030030515     c                   eval      BARcdeNSP = DB0nsp
024040030515     c                   movel     BarCodeDS     I28cod
024050030515     c                   call      'TRUL28R1'
024060030515     c                   parm                    TRUL28DS
024070030515if  2c                   if        O28err =  *blanks
024080030515     c                   movel     O28cod        Tbcd
024090030515e   2c                   endif
024100140915      *
024110140915      /free
024120140915
024130140915         // -?BRTcode?
024140140915         reset  TRUL28ds0;
024150140915         trul28ds0.i280cod = BarCodeDS;
024160140915         trul28r0 ( TRUL28ds0 );
024170140915         if  trul28ds0.o280err <> *on;
024180140917           //TbrtC = trul28ds0.o280cod;
024190140917           TbrtC = %subst( trul28ds0.o280cod :  1 : 5 ) + ' ' +
024200140917                   %subst( trul28ds0.o280cod :  6 : 5 ) + ' ' +
024210140917                   %subst( trul28ds0.o280cod : 11 : 5 ) + ' ' +
024220140917                   %subst( trul28ds0.o280cod : 16 : 4 );
024230140915         endif;
024240140915
024250140915      /end-free
024260030515      *
024270030515      * - Sigla operatore video
024280030515     c                   eval      Psop   =  DB0sop
024290080721      /free
024300080721
024310130306         // -?Flag "allega documento di trasporto"?
024320130418         //   IN FUTURO: se al 1° tentativo di consegna
024330130418         //              (DB0FL1 = "D" - v. pgm FNLRS9R)
024340080721         select;
024350081119
024360130306           // -?LdV con DDT allegato oltre il 1° tentativo di consegna?
024370081127           when  DB0bam =  'S'   and   wB0tmc <> *blank;
024380081127             evalr Pddt =  'DDT sul collo';
024390081119
024400130306           // -?LdV con DDT allegato (al 1° tentativo di consegna)?
024410080723           when  DB0bam =  'S';
024420080723             evalr Pddt =  'DDT';
024430081119
024440130306           // -?Esiste packing-list abbinata?
024450130306           when  $FIAR8 =  *on   and   $§PDF =  *off;
024460081013           //if  DB0fl1 <> 'D';
024470081013               evalr Pddt =  'PACKING-LIST';
024480081013           //else;
024490081013           //  evalr Pddt =  'PACKING-LIST GIA'' CONSEGNATA';
024500081013           //endif;
024510130306
024520130306           // -?LdV con PDF allegato?
024530130306           when  $FIAR8 =  *off  and   $§PDF =  *on;
024540130306             evalr Pddt =  'PACKING-LIST PDF';
024550130306
024560130306           // -?LdV con packing-list abbinata & PDF allegato?
024570130306           when  $FIAR8 =  *on   and   $§PDF =  *on;
024580130308             evalr Pddt =  'PACKING-LIST PDF';
024590081119
024600130306           // -?LdV senza PDF allegato & senza packing-list abbinata?
024610080721           other;
024620080721             clear Pddt;
024630081119
024640080721         endsl;
024650080721
024660080721      /end-free
024670030515      *
024680030515      * - Mittente
024690170517     c                   if        *in71
024700170517     c                   exsr      sr_Mitt_DPD
024710170517     c                   endif
024720170517      *
024730030515     c                   if        DB0ccm <> *zeros
024740030515     c                   move      DB0ccm        Pccm
024750030515     c                   endif
024760030515     c                   eval      Prsm   =  DB0rsm
024770030515     c                   eval      Pinm   =  DB0inm
024780030515     c                   eval      Pcam   =  DB0cam
024790030515     c                   eval      Plom   =  DB0lom
024800030515     c                   eval      Pprm   =  DB0prm
024810030515     c                   eval      Pnzm   =  DB0nzm
024820030515      *   Mittente originale
024830030515if  1c                   if        DB0rmo <> *blanks
024840030515     c                   eval      Pkmo   =  'MITT. ORIGINALE'
024850030515     c                   eval      Prmo   =  DB0rmo
024860030515e   1c                   endif
024870030515      *
024880070308      * - Filiale di Arrivo
024890030515if  1c                   if        DB0ris <> 'M'
024900080721     c                   eval      Pkla   =  'FILIALE'
024910170717     c                   eval      Plna1  =  %editc(wDB0lnaCf£6 : 'X')
024920080722     c                                    +  ' '
024930170717     c                                    +  wDB0dlaCf£6
024940080722     c                                    +  ' - '
024950170717     c                                    +  wDB0inlCf£6
024960140129     c*//                                 +  ' Tel.'
024970140129     c                                    +  '     '
024980170717     c                                    +  %trimr(wDB0telCf£6)
024990170717     c                   eval      Plna2  =  %editc(wDB0calCf£6 : 'X')
025000080722     c                                    +  ' '
025010170717     c                                    +  wDB0lolCf£6
025020080722     c                                    +  ' '
025030170717     c                                    +  wDB0prlCf£6
025040140129     c*//                                 +  '                  '
025050140129     c*//                                 +  ' Fax '
025060170717     c*//                                 +  %trimr(wDB0faxCf£6)
025070170717     c                   if        wDB0faxCf£6 <> *blanks
025080170717     c     c_Mai:c_Min   xlate     wDB0faxCf£6   DB0fax
025090140129     c                   eval      Plna2  =  %trimr(Plna2)
025100140129     c                                    +  '          '
025110140129     c*//*                                +  ' E-mail: '
025120140129     c                                    +  '         '
025130170717     c                                    +  %trimr(wDB0faxCf£6) + '@brt.it'
025140140129     c                   endif
025150030515e   1c                   endif
025160030515      *
025170030515      * - Destinatario
025180030515if  1c                   if        DB0ccd <> *zeros
025190030515     c                   move      DB0ccd        Pccd
025200030515e   1c                   endif
025210030515     c                   eval      Prsd   =  DB0rsd
025220030515     c                   eval      Prse   =  DB0rd2
025230030515     c                   eval      Pind   =  DB0ind
025240030515     c                   eval      Pcad   =  DB0cad
025250030515     c                   eval      Plod   =  DB0lod
025260030515     c                   eval      Pprd   =  DB0prd
025270030515     c                   eval      Pnzd   =  DB0nzd
025280030515      *
025290030515      * - Riferimenti mittente
025300030515     c                   eval      Prmn   =  DB0rmn
025310030515     c                   eval      Prma   =  DB0rma
025320061107      * se bolla DPD recupero il riferimento da FIAR4 rcd "I"
025330061107     c*** (già fatto:)   clear                   dsBL4I
025340060626     c                   if        *in71
025350061103     c                   exsr      RepAR4_I
025360060626     c                   endif
025370030515      *
025380030515      * - Nominativo & telefono
025390031124      *   (da FIAR5 / "GEN")
025400040607     c*** (già fatto:)   exsr      RepAR5gen
025410031124     c                   eval      Pnom   =  §AR5ref
025420031124     c                   eval      Ptel   =  §AR5teld
025430030515      *
025440030515      * - Spedizione
025450030515      *   (numero: non se Rimesso al Mittente)
025460030515if  1c                   if        DB0ris <> 'M'
025470030515     c                   eval      Plnp   =  DB0lnp
025480030515     c                   eval      Pnrs   =  DB0nrs
025490030515     c                   eval      Pnsp   =  DB0nsp
025500030515e   1c                   endif
025510030515      * . . Data spedizione
025520031124     c     *iso          movel     Wdsp          Data_EUR
025530130418     c     *eur          movel     Data_EUR      Pdsp
025540140922     c     *dmy          movel     Data_EUR      Pdsp6
025550030515      *
025560030515      * - Colli: numero, peso, volume, flag tipo-volume
025570030515     c                   eval      Pncl   =  DB0ncl
025580030515     c                   eval      Ppkf   =  DB0pkf
025590030515     c                   eval      Pvlf   =  DB0vlf
025600030515if  1c                   if            DB0fvf <> *blanks
025610030515     c                             and DB0ris <> 'M'
025620030717     ***c***                eval      Pfvf   =  '-' + DB0fvf + '-'
025630030717     c                   move      DB0fvf        Pfvf
025640030515e   1c                   endif
025650030515      *
025660030515      * - Natura merce
025670030515     c                   eval      Pnat   =  DB0nas
025680030515      *
025690030515      * - Servizio
025700100513      /free
025710100513         if  %subst(DB0tb1:1:1) = 'A'   or   DB0tb2 <> *blanks;
025720100513           exsr  sr_CheckORM_PrevvMail;
025730100513         endif;
025740100513      /end-free
025750040607      * . . Descrizione LDV
025760100513     c                   if        Not  $PreavvORM
025770030515     c                   eval      Pds1   =  DB0dcb
025780100513     c                   endif
025790030515      * . . Descrizione tipo servizio
025800030515if  1c                   if        DB0fst =  'S'
025810030515     c                   eval      Pds2   =  DB0dts
025820030515e   1c                   endif
025830030515      * . . Reso / Franco frontiera
025840030515sel 1c                   select
025850030515w   1c                   when      DB0fbr =  'S'
025860030515     c                   clear                   w008a
025870030515     c                   eval      w008a  =  '   RESO '
025880030515     c                   move      w008a         Pds2
025890030515w   1c                   when      DB0fbr =  'F'
025900030515     c                   clear                   w015a
025910030515     c                   eval      w015a  =  '   FR.FRONTIERA'
025920030515     c                   move      w015a         Pds2
025930030515e   1c                   endsl
025940030515      *
025950030515      * - Zona consegna
025960030515     c                   eval      Pznc   =  DB0znc
025970030515      *
025980030515      * - Flag "*" (gestito a parte)
025990040223      *
026000040223      * - Flag "V" = colli di valore
026010040223if  1c                   if        §AR5bva   <> *blank
026020040223     c                   eval      Pbva   =  'V'
026030040223e   1c                   endif
026040060921      *
026050070212      * - Codice Giro o Numero Distinta (se tipo bolla "ARRIVO")
026060071015if  1c                   IF              DB0tbo = 'A'
026070071015     c                             and  wGeoCon = 'S'
026080070212sel 2c                   Select
026090070212      * · - Stampa LdV in arrivo con codice giro (parametro passato)
026100071015w   2c                   When      DB0fl1     =  'A'
026110070215if  3c                   if        §DB0tgi    =  'M'
026120101028     c                   eval      Pndc   =  'g:' + §DB0cgi + 'm'
026130070215x   3c                   else
026140101028     c                   eval      Pndc   =  'g:' + §DB0cgi
026150070215e   3c                   endif
026160070212      * · - Stampa LdV con numero distinta consegna
026170071015w   2c                   When           wB0ndc  > *zeros
026180110427     c                             and  $GeoAtt =  *on
026190101028     c                   eval      Pndc   =  'd:'
026200070514     c                                    +  %trim(%editc(wB0ndc:'Z'))
026210101028if  3c                   if        %len(Pndc)  >= %len(%trimr(Pndc))
026220071016     c                                          + 3 + %len(Wpdc4a)
026230101028     c                   eval      Pndc   =  %trimr(Pndc)
026240071022     c                                    +  ' a:'
026250071022     c                                    +  Wpdc4a
026260071016e   3c                   endif
026270070212      * · - Stampa LdV con codice giro (da reperire in FIARG00F)
026280070213w   2c                   Other
026290070212if  3c                   if        NOT %open(FIARG01L)
026300070212     c                   open      FIARG01L
026310070212e   3c                   endif
026320070215     c                   clear                   ARGcgi
026330070215     c                   clear                   ARGtgi
026340070212     c     KLDV          chain     FIARG000
026350070215if  3c                   if        ARGtgi =  'M'
026360101028     c                   eval      Pndc   =  'g:' + ARGcgi + 'm'
026370070215x   3c                   else
026380101028     c                   eval      Pndc   =  'g:' + ARGcgi
026390070215e   3c                   endif
026400070215e   2c                   EndSl
026410070212e   1c                   ENDIF
026420030515      *
026430030515      * - Fermo deposito
026440030515if  1c                   if        DB0ffd <> *blanks
026450060921     c                   eval      Pffd   =  'FERMO DEPOSITO '
026460030515e   1c                   endif
026470030515      *
026480070308      * - Segnacolli: filiale, serie, dal / al
026490030515if  1c                   if        DB0fls <> *zeros
026500030515     c                   move      DB0fls        Ppos
026510030515e   1c                   endif
026520040607      * - - se bolla con mamma considero la serie della mamma
026530070515if  1c                   if        $Mamma =  *on
026540031015     c                   eval      Psrs   =  wB0nrs
026550031015x   1c                   else
026560030515     c                   eval      Psrs   =  DB0nrs
026570031015e   1c                   endif
026580030515      * - Se colli non sequenziali: stampo "NON SEQUENZIALI" + Elenco
026590030515      *   Se collo cumulativo ....:   "     SCD "X" Num.Colli
026600030515      *   Se colli sequenziali ...:   "     SCD "/" SCA
026610030515      *   Se collo singolo .......:   "     SCD
026620031015sel 1c                   select
026630030515      * - - non sequenziali
026640030515w   1c                   when      DB0fns =  'S'
026650050513if  2c                   if           *in69      = *on
026660050513     c                             or $A4_x_ScNS = *off
026670030715      *     caricamento delle "Note e Particolarità" per consentirne
026680030715      *     l'aggiunta come "Elenco segnacolli non sequenziali"
026690031015if  3c                   if        $Partic = *off
026700030515     c                   exsr      SchPARTIC
026710031015e   3c                   endif
026720030515     c                   exsr      ListaScNS
026730050511x   2c                   else
026740050511if  3c                   if        DB0ncl <= 2
026750050511     c                   exsr      ListaScNSd
026760081007x   3c                   else
026770081007     c                   eval      Pnsx   =  'NON SEQUENZIALI'
026780050511e   3c                   endif
026790030515e   2c                   endif
026800030515      * - - cumulativo
026810050114w   1c**!!!              when      DB0xco =  'X'
026820081010     c**!!!              eval      Pnsx   =  %editc(DB0ncd : 'Z')
026830081010     c**!!!                               +  'X'
026840081010     c**!!!                               +  %editc(DB0ncl : 'Z')
026850030515      * - - singolo o sequenziali
026860030515      *     (non se rimesso al mittente)
026870030515w   1c                   when      DB0ris <> 'M'
026880080922     c                   eval      Pnsx   =  %editc(DB0ncd : 'Z')
026890030515if  2c                   if            DB0nca <> *zeros
026900030515     c                             and DB0nca <> DB0ncd
026910080922     c                   eval      Pnsx   =  %trimr(Pnsx) + '-'
026920080922     c                                    +  %editc(DB0nca : 'Z')
026930030515e   2c                   endif
026940030515e   1c                   endsl
026950030515      *
026960030716      * - Note (1 e 2) e Particolarità (0, 3, 4, 5 e 6)
026970030710      *   con eventuali segnacolli non sequenziali (max 4 x riga)
026980030515if  1c                   if        $Partic = *off
026990030515     c                   exsr      SchPARTIC
027000030515e   1c                   endif
027010030716if  1c                   if           $NP(1) <> *blanks
027020030716     c                             or $NP(2) <> *blanks
027030030716     c                             or $NP(3) <> *blanks
027040030716     c                             or $NP(4) <> *blanks
027050030716     c                             or $NP(5) <> *blanks
027060030716     c                             or $NP(6) <> *blanks
027070030716     c                   eval      Pknp   =  'NOTE  E  PARTICOLARITÀ'
027080030710     c                   eval      Pnp1   =  $NP(1)
027090030710     c                   eval      Pnp2   =  $NP(2)
027100030710     c                   eval      Pnp3   =  $NP(3)
027110030710     c                   eval      Pnp4   =  $NP(4)
027120030710     c                   eval      Pnp5   =  $NP(5)
027130030710     c                   eval      Pnp6   =  $NP(6)
027140030716e   1c                   endif
027150030515      *
027160080401      * - Privato / Appuntamento / Uffici / Consegna Garantita
027170080401      *   (servizio consegna garantita x cliente Wuerth)
027180170413if  1c*//                if        DB0gva =  'P '
027190170413     c*//                evalr     Ppad = 'PRIVATO'
027200170413e   1c*//                endif
027210041012if  1c                   if        DB0gva =  'U '
027220080723     c                   evalr     Ppad = 'CONSEGNA AD UFFICI'
027230041012e   1c                   endif
027240081010if  1c*** NO !!!         if           DB0tc1 =  'A'
027250081010     c*** NO !!!                   or DB0tc2 =  'A'
027260081010     c*** NO !!!         move      'APPUNTAMENTO'Ppad
027270081010e   1c*** NO !!!         endif
027280080401if  1c                   if             DB0nrs = 40
027290090518     c                             and  DB0tsp = 'E'
027300080401     c                             and (DB0lnp = 043
027310110429     c                              or  DB0lnp = 277
027320080401     c                              or  DB0lnp = 063)
027330090217     c*****              evalr     Ppad = 'CONSEGNA GARANTITA'
027340090227     c                   eval      Pds2 = 'CONSEGNA ESPRESSA +
027350090227     c                                     WÜRTH'
027360080401e   1c                   endif
027370030515      *
027380030515      * - Data e ora consegna richiesta
027390030515      *   e Flag "*" > x Data e/o Ora consegna richiesta
027400091116      *   (NO "Prima del" per il cliente TTL)
027410091116if  0c                   if        DB0ccm <> c_CliTTL  or
027420091116     c                             DB0dcr =  *zeros    or  DB0tcr <> 'P'
027430030515if  1c                   if        DB0dcr <> *zeros
027440030515     c                   eval      Past   =  '*'
027450030515     c     *iso          movel     DB0dcr        Data_EUR
027460030515     c     *eur          movel     Data_EUR      Pdcr
027470030515e   1c                   endif
027480030515if  1c                   if        DB0hcr <> *zeros
027490030515     c                   eval      Past   =  '*'
027500030515     c                   eval      *in18  =  *on
027510030515     c                   eval      Phcr   =  DB0hcr
027520030515e   1c                   endif
027530091116e   0c                   endif
027540030515      *
027550030515      * - Consegne particolari
027560030515     c                   eval      Pcp1   =  DB0dc1
027570030515     c                   eval      Pcp2   =  DB0dc2
027580030515     *** *
027590030515     *** * - Valore merce dichiarato e relativa divisa => NO
027600030515     ***c***                eval      Pvmd   =  DB0vmd
027610030515     ***c***                eval      Pvad   =  DB0vad
027620030515     *** *
027630030515     *** * - Importo da assicurare e relativa divisa   => NO
027640030515     ***c***                eval      Pvas   =  DB0vas
027650030515     ***c***                eval      Pias   =  DB0ias
027660030515     *** *
027670030515     *** * - Quantità da fatturare                     => NO
027680030515     ***c***                eval      Pqft   =  DB0qft
027690030515      *
027700030515      * - Imponibile / IVA / Esenzione / Totale fattura e divisa
027710030515      *   (importi "arrotondati" a 2 decimali in stampa)
027720030515if  1c                   if        DB0ift <> *zeros
027730030515     c                   eval      Pkim   = 'IMPONIBILE'
027740130218     c*//                   eval(h)   Pimp   =  DB0imv
027750130218     c                   eval(h)   w0112  =  DB0imv
027760130218     c                   evalr     Pimp   =  %editc(w0112 : 'K')
027770030515sel 2c                   select
027780030515w   2c                   when      DB0iva <> *zeros
027790030515     c                   eval      Pkie   = 'I.V.A.'
027800030624     c                   eval(h)   w0112  =  DB0iva
027810130218     c*//                   eval      Piva   =  '     ' +
027820130218     c*//                                       %editc(w0112 : 'K')
027830130218     c                   evalr     Piva   =  %editc(w0112 : 'K')
027840030515w   2c                   when         DB0cei <> *blanks
027850030515     c                             or DB0dce <> *blanks
027860030912     c                   eval      Pkie   = 'ESENZIONE'
027870030912     c                   eval      Piva   =  DB0dce
027880030515e   2c                   endsl
027890030515     c                   eval      Pktf   = 'TOTALE FATTURA'
027900130218     c*//                   eval(h)   Pift   =  DB0ift
027910130218     c                   eval(h)   w0112  =  DB0ift
027920130218     c                   evalr     Pift   =  %editc(w0112 : 'K')
027930030515if  2c                   if        DB0div <> §GEDcn
027940030515     c                   eval      Pdiv   =  DB0div
027950030515e   2c                   endif
027960030515e   1c                   endif
027970030515      *
027980030515      * - Contrassegno e relativa divisa
027990030515if  1c                   IF        DB0cas <> *zeros
028000030625      *
028010030515     c                   eval(h)   Pcas   =  DB0cas
028020030515if  2c                   if        DB0vca <> §GEDcn
028030030515     c                   eval      Pdca   =  DB0vca
028040030515e   2c                   endif
028050030515     c                   exsr      CtrCAs
028060030624      * - - Nuovo controllo abilitazione c/assegno
028070030624     c                   reset                   TRUL21DS
028080030624     c                   eval      I21cbo =  DB0cbo
028090030624     c                   eval      I21tsp =  DB0tsp
028100030624     c                   eval      I21lnp =  DB0lnp
028110030624     c                   eval      I21nzm =  DB0nzm
028120030624     c                   eval      I21lna =  DB0lna
028130030624     c                   eval      I21nzd =  DB0nzd
028140130826     c                   if        DB0ccm >  *zero
028150130826     c                   eval      I21ksc =  DB0ccm
028160130826     c                   else
028170130826     c                   eval      I21ksc =  (DB0lnp * 10000 ) + 8888
028180130826     c                   endif
028190030624     c                   eval      I21tic =  DB0tic
028200030624if  2c                   if        DB0vca <> §GEDcn
028210030624     c                   eval      I21imp =  Wcas
028220030624     c                   eval      I21div =  §GEDcn
028230030624x   2c                   else
028240030624     c                   eval      I21imp =  DB0cas
028250030624     c                   eval      I21div =  DB0vca
028260030624e   2c                   endif
028270030624     c                   eval      I21ute =  KNMUS
028280040607     c                   eval      I21pgm =  SDSpgm
028290030624     c                   call      'TRUL21R'
028300030624     c                   parm                    TRUL21ds
028310030624      * - - Carico la tabella "Tipo incasso C/Assegno" (1A)
028320030624     c                   exsr      Tab_1A
028330030624     c                   movel     §1Ades        DB0dti
028340030624if  2c                   if            §1Adeo <> *blanks
028350030624     c                             and O21fca <> *blanks
028360030624     c                   movel     §1Adeo        DB0dti
028370030624e   2c                   endif
028380100208      /free
028390100208           // - -?Reperimento intestatario c/assegno?
028400100322           if  §VPOcas <= wDate   and   DUTlpo  <> 'S';
028410100208             reset  trulINTds;
028420100208             iINTtic  = DB0tic;
028430100208             iINTrscM = DB0rsm;
028440100208             iINTrscO = DB0rmo;
028450100208             iINTcdi  = §AR5cdi;
028460100208             //if  §AR5cdi = *blank;
028470100208             //  iINTaas = DB0aas;
028480100208             //  iINTlnp = DB0lnp;
028490100208             //  iINTnrs = DB0nrs;
028500100208             //  iINTnsp = DB0nsp;
028510100208             //endif;
028520100208             trulINTr ( TRULINTds );
028530100322           endif;
028540100208      /end-free
028550030624      * - - Imposto i campi di stampa
028560030624     c                   eval      Ptic   =  DB0tic + ' ' + DB0dti
028570091119      /free
028580100208           // - - -?Intestatario assegno?
028590100322           if  DB0cas  <> *zero   and
028600100322              (§VPOcas <= wDate   and   DUTlpo  <> 'S')   and
028610100208               oINTerr = *blank;
028620100208             Pkic = 'INTESTARE A:';
028630100208             Pica = oINTrsci;
028640100208           endif;
028650091119      /end-free
028660030625      *
028670030515e   1c                   ENDIF
028680030515      *
028690030515      * - Totale da esigere al ritiro o alla consegna e relativa divisa
028700030515if  1c                   IF        DB0ift >  *zeros
028710030625      *
028720030515if  2c                   if            DB0vca <> §GEDcn
028730030515     c                             and DB0vca <> *blanks
028740030515     c                   eval      Pdie   =  DB0div
028750030515e   2c                   endif
028760030515      *   Controllo se C/Assegno da sommare al totale da esigere o no:
028770030515      *   se Franco-Prepagato il C/Assegno non si somma mai;
028780030515      *   se c'è il totale fattura: metto anche il il totale da esigere.
028790030515      *   (importi "arrotondati" a 2 decimali in stampa)
028800030515sel 2c                   Select
028810030515      *   Non ho C/Assegno o è Franco Prepagato
028820071008w   2c                   When          DB0cas  = *zeros
028830071008     c                             or  DB0tbo  = '8'
028840080701     c                   eval(h)   Pies   =  DB0ift
028850030625     c                   clear                   Pie1
028860030625     c                   clear                   Pdi1
028870030515      *   C/Assegno sommabile
028880030515w   2c                   When      DB0ste =  'S'    and $CAeur = *on
028890030515sel 3c                   select
028900030515      *   - c/assegno in EURO
028910030515w   3c                   when      DB0div =  §GEDcn and DB0vca =  §GEDcn
028920080701     c                   eval(h)   Pies   =  DB0ift + DB0cas
028930030624     c                   clear                   Pie1
028940030624     c                   clear                   Pdi1
028950030515      *   - c/assegno in DIVISA
028960030515w   3c                   when          DB0div =  §GEDcn
028970030515     c                             and DB0vca <> §GEDcn
028980080701     c                   eval(h)   Pies   =  DB0ift + Wcas
028990030624     c                   clear                   Pie1
029000030624     c                   clear                   Pdi1
029010030515e   3c                   endsl
029020030515      *   C/Assegno non sommabile
029030030515x   2c                   Other
029040080701     c                   eval(h)   Pie1   =  DB0cas
029050080701     c                   eval(h)   Pies   =  DB0ift
029060030515e   2c                   Endsl
029070030709      *
029080030709e   1c                   ENDIF
029090030515      *
029100030515      * - Numero e data fattura
029110030515if  1c                   if        DB0nft <> *zeros
029120030515     c                   eval      Pkft   =  'FATTURA   '
029130030515     c                   eval      Pnft   =  DB0nft
029140130124     c                   eval      Pfiv   = '(' + %editc(wB0fiv:'X') +
029150130124     c                                      ')'
029160030515if  2c                   if        DB0dft >  *zeros
029170030515     c                   eval      Pkdf   =  'DEL'
029180030515     c     *iso          movel     DB0dft        Data_EUR
029190130418     c     *dmy          movel     Data_EUR      Pdft
029200130418     c*//  *eur          movel     Data_EUR      Pdft
029210030515e   2c                   endif
029220030515e   1c                   endif
029230030515      *
029240030515      * Memorizzo il codice tariffa come numero di copie da stampare
029250030515      *   (per Rimesso Mittente - non per DPD o FEDEX)
029260030515if  1c                   if        DB0ris = 'M'  and  *in70
029270030515     c                   eval      Wctr   =  DB0ctr
029280030515e   1c                   endif
029290030515      *
029300030515     c                   ENDSR
029310170517      *
029320170517      /free
029330170517       //--------------------------------------------------------------
029340170517       //?Controllo SE ragione sociale Mittente da FORZARE (TFNTC) per ?
029350170517       //?spedizione DPD.                                              ?
029360170517       //--------------------------------------------------------------
029370170517       BEGSR  sr_Mitt_DPD;
029380170517
029390170517         // -?Reperim. abilitazioni del Mittente dalla tab. "CLI"?
029400170517         clear  k05tntbe01;
029410170517         k_TBEcod = 'CLI';
029420170517         k_TBEke1 = %editc( DB0ccm : 'X' );
029430170517         chain  %kds( k05tntbe01 )  TNTBE000;
029440170517         if  Not %found( TNTBE01L )  or  TBEatb <> *blank;
029450170517           leavesr;
029460170517         endif;
029470170517         dCLI = TBEuni;
029480170517         // -?Cliente SENZA forzatura Mittente da note "AM" e "AS"?
029490170517         if  §CLItfntc <> 'S'  and  §CLInoIMtt <> 'S';
029500170517           leavesr;
029510170517         endif;
029520170517
029530170517         // -?Forzatura Mittente da note "AM"?
029540170517         If  §CLItfntc = 'S';
029550170517
029560170517           if  NOT %open( TFNTC01L );
029570170517             open  TFNTC01L;
029580170517           endif;
029590170517           clear  keyTFNTC01;
029600170517           k_NTCapl = 'C';
029610170517           k_NTCnk1 = %editc( DUTkci : 'X' ) + %editc( DB0ccm : 'X' );
029620170517           k_NTCtnt = 'AM';
029630170517           chain  %kds( keyTFNTC01 )  TFNTC;
029640170517           if  %found( TFNTC01L )  and  NTCsns = 'S';
029650170517             DB0rsm = NTCrnt;
029660170517           endif;
029670170517
029680170517         EndIf;
029690170517
029700170517         // -?Pulizia Indirizzo Mittente?
029710170517         If  §CLInoIMtt = 'S';
029720170517
029730170517           clear  DB0inm;
029740170517           clear  DB0cam;
029750170517           clear  DB0lom;
029760170517           clear  DB0prm;
029770170517           clear  DB0nzm;
029780170517
029790170517         EndIf;
029800170517
029810170517       ENDSR;
029820170517
029830170517      /end-free
029840030515      *
029850030515      *---------------------------------------------------------------*
029860050217      *?Inschieramento delle Note e delle Particolarità              ?*
029870030515      *---------------------------------------------------------------*
029880030515     c     SchPARTIC     BEGSR
029890030515      *
029900030515     c*** (già fatto:)   reset                   $Partic
029910030710     c*** (già fatto:)   reset                   $NoDesPartic
029920030515     c*** (già fatto:)   clear                   $NP
029930030717     c*** (già fatto:)   clear                   W_CoOr
029940030515     c                   clear                   XX
029950030710      *
029960030710      * Imposto eventuali note per prime
029970030710if  1c                   if        DB0nt1 <> *blanks
029980030710     c                   add       1             xx
029990030710     c                   movel     DB0nt1        $NP(xx)
030000030710e   1c                   endif
030010030710if  1c                   if        DB0nt2 <> *blanks
030020030710     c                   add       1             xx
030030030710     c                   movel     DB0nt2        $NP(xx)
030040030710e   1c                   endif
030050030515      *
030060030515      * Le particolarità possono essere lunghe fino a 45 char.
030070030515      *   Se i primi 30 (della 1ª riga a disposizione) non dovessero
030080030515      *   bastare, il resto lo si mette nella riga successiva.
030090030710      *   Se, invece, non risultano decodificate, si mettono solo i
030100030710      *   codici sulla stessa riga.
030110030710      *   Se uno solo risulta decodificato, esso va su una riga a
030120030710      *   parte (vedi flag "$NoDesPartic": *off = serve riga nuova
030130030710      *                                    *on  = ok riga procedente)
030140030515      *
030150030515      * . . Particolarità Giacenza
030160030515if  1c                   IF        DB0gga <> *blanks
030170030515     c                   movel     'Q'           WTpPar
030180030515     c                   movel     DB0gga        WCdPar
030190030515     c                   exsr      TABxPAR
030200030710if  2c                   if            TBLuni       = *blanks
030210030710     c                             and $NoDesPartic = *on
030220030710     c                   eval      $NP(xx) = %trim($NP(xx))
030230030710     c                                     + '   ' + WCdPar
030240030710x   2c                   else
030250030710if  3c                   if        TBLuni       = *blanks
030260030710     c                   eval      $NoDesPartic = *on
030270030710x   3c                   else
030280030710     c                   eval      $NoDesPartic = *off
030290030710e   3c                   endif
030300030515     c                   add       1             xx
030310100513     c                   movel     w053a         $NP(xx)
030320100513if  3c                   if        %subst(w053a : %len($NP(xx))+1)
030330030515     c                              <> *blanks
030340030515     c                   add       1             xx
030350030515     c                   eval      $NP(xx) = '   ' +
030360100513     c                             %trim(%subst(w053a:%len($NP(xx))+1))
030370030710e   3c                   endif
030380030515e   2c                   endif
030390030515e   1c                   ENDIF
030400030515      *
030410030515      * . . Particolarità Consegna
030420030515if  1c                   IF        DB0gma <> *blanks
030430030515     c                   movel     'R'           WTpPar
030440030515     c                   movel     DB0gma        WCdPar
030450030515     c                   exsr      TABxPAR
030460030710if  2c                   if            TBLuni       = *blanks
030470030710     c                             and $NoDesPartic = *on
030480030710     c                   eval      $NP(xx) = %trim($NP(xx))
030490030710     c                                     + '   ' + WCdPar
030500030710x   2c                   else
030510030710if  3c                   if        TBLuni       = *blanks
030520030710     c                   eval      $NoDesPartic = *on
030530030710x   3c                   else
030540030710     c                   eval      $NoDesPartic = *off
030550030710e   3c                   endif
030560030515     c                   add       1             xx
030570100513     c                   movel     w053a         $NP(xx)
030580100513if  3c                   if        %subst(w053a : %len($NP(xx)) + 1)
030590030515     c                              <> *blanks
030600030515     c                   add       1             xx
030610030515     c                   eval      $NP(xx) = '   ' +
030620100513     c                             %trim(%subst(w053a:%len($NP(xx))+1))
030630030710e   3c                   endif
030640030515e   2c                   endif
030650030515e   1c                   ENDIF
030660030515      *
030670030515      * . . Particolarità Varie
030680030717      *     - questa descrizione va messa nell'apposito campo PPAD
030690041015      *       se è "PRIVATO" idem se è "CONSEGNA AD UFFICI"
030700150224     *** *     - se 'B' (bancali da rendere al mittente) comparirà un
030710150224     *** *       apposito box
030720150224     *** *     - se 'O' (oltre 20 colli => bancali da riportare in mag.)
030730150224     *** *       comparirà un apposito box, un'apposita nota e l'indica-
030740150224     *** *       zione nella copia per il destinatario.
030750030717      *     ...Potrebbero anche non esserci più elementi di schiera
030760030717      *     disponibili [xx >= %elem($NP)] !
030770030709if  1c                   IF            DB0gva <> *blanks
030780150224     c*//                          and DB0gva <> 'B '
030790150224     c*//                          and DB0gva <> 'O '
030800030709     c                             and DB0gva <> 'P '
030810041015     c                             and DB0gva <> 'U '
030820030515     c                   movel     'S'           WTpPar
030830030515     c                   movel     DB0gva        WCdPar
030840030515     c                   exsr      TABxPAR
030850030710if  2c                   if            TBLuni       = *blanks
030860030710     c                             and $NoDesPartic = *on
030870030710     c                   eval      $NP(xx) = %trim($NP(xx))
030880030710     c                                     + '   ' + WCdPar
030890030710x   2c                   else
030900030710if  3c                   if        TBLuni       = *blanks
030910030710     c                   eval      $NoDesPartic = *on
030920030710x   3c                   else
030930030710     c                   eval      $NoDesPartic = *off
030940030710e   3c                   endif
030950030717if  3c                   if            xx < %elem($NP)
030960030709     c                   add       1             xx
030970100513     c                   movel     w053a         $NP(xx)
030980100513if  4c                   if        %subst(w053a : %len($NP(xx)) + 1)
030990030709     c                              <> *blanks
031000030717     c                             and xx < %elem($NP)
031010030709     c                   add       1             xx
031020030709     c                   eval      $NP(xx) = '   ' +
031030100513     c                             %trim(%subst(w053a:%len($NP(xx))+1))
031040030717e   4c                   endif
031050030717e   3c                   endif
031060030709e   2c                   endif
031070030515e   1c                   ENDIF
031080030717      *
031090030717      * Imposto il numero originale di colli componenti la spedizione
031100030717      * e se ho ancora spazio lo memorizzo in "Note e Particolarità"
031110030717if  1c                   if        %subst(DB0gva:1:1) = 'O'
031120030717     c                   exsr      RepAR5bnb
031130030717if  2c                   if        W_CoOr  <> *blanks
031140030717     c                             and xx  <  %elem($NP)
031150030717     c                   add       1             xx
031160030717     c                   eval      $NP(xx) =  W_CoOr
031170030717e   2c                   endif
031180030717e   1c                   endif
031190030515      *
031200030515      * Imposto il flag per memorizzare la già avvenuta memorizzazione
031210030715      *   delle "Note e Particolarità"
031220030515     c                   eval      $Partic = *on
031230030515      *
031240030515     c                   ENDSR
031250070319      *
031260070319      *---------------------------------------------------------------*
031270070319      *?Inschieramento delle Consegne Parziali precedenti            ?*
031280070319      *---------------------------------------------------------------*
031290070319     c     SchConsParz   BEGSR
031300070319      *
031310070319     c*** (già fatto:)   reset                   $CP
031320070319     c*** (già fatto:)   reset                   $CPdcm
031330070319     c                   clear                   XX
031340070319      *
031350070319if  2c                   if        NOT %open(FNART01L)
031360070319     c                   open      FNART01L
031370070319e   2c                   endif
031380070319      *
031390070514if  2c                   if        $Mamma = *off
031400070319     c     KLDV          setll     FNART000
031410070319     c     KLDV          reade     FNART000
031420070514x   2c                   else
031430070514     c     KLBL          setll     FNART000
031440070514     c     KLBL          reade     FNART000
031450070514e   2c                   endif
031460070319      *
031470070319do  1c                   DOW       NOT  %eof(FNART01L)
031480070319      *
031490070319if  2c                   If             ARTatb = *blanks
031500070319     c                             and  ARTdcm > *zeros
031510070319      *
031520070319     c                   eval      *in31  = *off
031530070319     c                   eval      xx     = 1
031540070319     c     ARTdcm        lookup    $CPdcm(xx)                             31
031550070319if  3c                   if        NOT  *in31
031560070319     c                   eval      xx     = 1
031570070319     c     *zeros        lookup    $CPdcm(xx)                             31
031580070319if  4c                   if        *in31
031590140304     c*//                eval      $CP(xx)     = *zeros
031600140304     c                   clear                   Elem_CP
031610140304     c                   movel     Elem_CP       $CP(xx)
031620070319e   4c                   endif
031630070319e   3c                   endif
031640070319if  3c                   if        *in31
031650070319     c                   eval      $CPdcm(xx)  = ARTdcm
031660070319     c                   movel     $CP(xx)       Elem_CP
031670070319     c                   eval      WCPdcm      = ARTdcm
031680070319     c                   eval      WCPncl      = WCPncl + 1
031690070319     c                   movel     Elem_CP       $CP(xx)
031700070319e   3c                   endif
031710070319      *
031720070319e   2c                   EndIf
031730070319      *
031740070514if  2c                   if        $Mamma = *off
031750070319     c     KLDV          reade     FNART000
031760070514x   2c                   else
031770070514     c     KLBL          reade     FNART000
031780070514e   2c                   endif
031790070319      *
031800070319e   1c                   ENDDO
031810070319      *
031820140513if  0c                   if        xx     > *zero
031830140513      * Reperimento nominativi firmatari
031840140513     c                   exsr      sr_Firmatario
031850070319      * Ordinamento schiera per data
031860070319if  1c                   if        xx     > 1
031870070319     c                   sorta                   $CP
031880070319e   1c                   endif
031890140513e   0c                   endif
031900070319      *
031910070319     c                   ENDSR
031920140303      *
031930140303      /free
031940140303
031950140303       //--------------------------------------------------------------
031960140303       //?Reperimento nome firmatario                                  ?
031970140303       //--------------------------------------------------------------
031980140303       BEGSR  sr_Firmatario;
031990140303
032000140303if  1    if  NOT  %open(FNEVB22L);
032010140303           open  FNEVB22L;
032020140303e   1    endif;
032030140303
032040140303         clear  keyFNEVB22;
032050140303         k_EVB2aas = DB0aas;
032060140303         k_EVB2lnp = DB0lnp;
032070140303         k_EVB2nrs = DB0nrs;
032080140303         k_EVB2nsp = DB0nsp;
032090140303         k_EVB2cev = 'P  ';
032100140303         setll  %kds( keyFNEVB22 : 4 )  FNEVB200;
032110140303         reade  %kds( keyFNEVB22 : 4 )  FNEVB200;
032120140303         DoW  Not  %eof(FNEVB22L);
032130140303           if  EVB2cev = k_EVB2cev  and  EVB2trd = 'FIR';
032140140305             yy = %lookup( EVB2dev : $CPdcm );
032150140305             if  yy > *zero;
032160140305               Elem_CP = $CP(yy);
032170140303               if  WCPocm  <= EVB2hev;
032180140303                 WCPocm  = EVB2hev;
032190140303                 WCPfir  = EVB2dati;
032200140305                 $CP(yy) = Elem_CP;
032210140303               endif;
032220140303             endif;
032230140303           endif;
032240140304           reade  %kds( keyFNEVB22 : 4 )  FNEVB200;
032250140303         EndDo;
032260140303
032270140303       ENDSR;
032280140303
032290140303      /end-free
032300031124      *
032310031124      *---------------------------------------------------------------*
032320050217      *?Reperimento Nominativo e Telefono destinatario da FIAR5/GEN  ?*
032330031124      *---------------------------------------------------------------*
032340031124     c     RepAR5gen     BEGSR
032350031124      *
032360031124     c                   clear                   dAR5gen
032370031124      *
032380031124     c                   eval      AR5trd = 'GEN'
032390040301if  1c                   if        DUTlpo = 'S'
032400040531     c     K05AR501      chain     FIAR531C
032410031127x   1c                   else
032420031127     c     K05AR501      chain     FIAR5000
032430031127e   1c                   endif
032440031124      *
032450040301if  1c                   if           (DUTlpo =  'S'
032460040531     c                             and %found(FIAR531C)
032470040301     c                             OR  DUTlpo <> 'S'
032480040301     c                             and %found(FIAR501L))
032490040301     c                             and AR5atb =  *blanks
032500031124     c                   movel     AR5uni        dAR5gen
032510031124e   1c                   endif
032520031124      *
032530031124     c                   ENDSR
032540030717      *
032550030717      *---------------------------------------------------------------*
032560050217      *?Reperimento Numero Colli Originale da FIAR5/BNB              ?*
032570030717      *---------------------------------------------------------------*
032580030717     c     RepAR5bnb     BEGSR
032590030717      *
032600030717     c                   clear                   dAR5bnb
032610030717     c*** (già fatto:)   clear                   W_CoOr
032620030717      *
032630030717     c                   eval      AR5trd = 'BNB'
032640040301if  1c                   if        DUTlpo = 'S'
032650040531     c     K05AR501      chain     FIAR531C
032660040301x   1c                   else
032670030717     c     K05AR501      chain     FIAR5000
032680040301e   1c                   endif
032690030717      *
032700040301if  1c                   if           (DUTlpo =  'S'
032710040531     c                             and %found(FIAR531C)
032720040301     c                             OR  DUTlpo <> 'S'
032730040301     c                             and %found(FIAR501L))
032740040301     c                             and AR5atb =  *blanks
032750030717     c                   movel     AR5uni        dAR5bnb
032760030717if  2c                   if        §AR5bcor <> *zeros
032770030717     c                   eval      W_CoOr   =  C_CoOr
032780030717     c                                      +  %editc(§AR5bcor : 'K')
032790030717e   2c                   endif
032800030717e   1c                   endif
032810030717      *
032820030717     c                   ENDSR
032830030515      *
032840030515      *---------------------------------------------------------------*
032850051129      *?Decodifiche condizionate in Lettera di Vettura               ?*
032860030515      *---------------------------------------------------------------*
032870040607     c     DecodLDV      BEGSR
032880030515      *
032890030515      * Richiesta firma per ricevuta della merce senza riserve
032900030515      * (e NON  quella  per quietanza) => *in35 + not *in36
032910030515     c                   movea     '10'          *in(35)
032920101130      *
032930101130      /free
032940101130
032950101130         // -?Memorizzazione consegne parziali precedenti dal file?
032960101130         //  ?FNART00F - serve anche nella stampa delle "tagliatelle"?
032970140513         IF  $ConsParz;
032980140513
032990140513           // -?SE si stanno stampando "tagliatelle":?
033000140514           //  ?le consegne parziali vengono indicate nel Box1,?
033010140514           //  ?SE?ne sono state reperite?
033020140513           IF  DB0fa4 = '4'  and  $CP(1) < *all'9';
033030140513
033040101130             // -?Prima si totalizzano le quantità consegnate?
033050140513             clear  w0080;
033060140513             clear  w0140;
033070101130             For  xx = 1  To  %elem($CP);
033080101130               if  $CP(xx) < *all'9';
033090101130                 Elem_CP = $CP(xx);
033100101130                 monitor;
033110101130                   Data_Eur = %date( WCPdcm : *iso );
033120101130                 on-error;
033130101130                   Data_Eur = *loval;
033140101130                 endmon;
033150101130                 if       Data_Eur  > *loval  and
033160101130                     %dec(Data_Eur) > w0080;
033170101130                   w0080 = %dec(Data_Eur);
033180101130                 endif;
033190101130                 w0140 += WCPncl;
033200101130               endif;
033210101130             EndFor;
033220101130             // -?Poi si stampa un'unica indicazione?
033230101201             Data_DMY = %date( w0080 : *eur );
033240101201             Pcpp = 'Consegnati '
033250101201                  + %triml( %editc( w0140 : 'Z' ) )
033260101201                  + ' colli al '
033270101201                  + %triml( %editc( %dec( Data_DMY ) : 'Y' ) );
033280140513
033290140513           ENDIF;
033300140513
033310140513         ENDIF;
033320101130
033330101130      /end-free
033340070307      *
033350070307      * Intestazione moduli
033360070307      * - Se spedizione in stato "Lasciato Avviso"
033370070307      *   si stampano anche data/ora del 1° lasciato avviso
033380070307if  1c                   if        $LascAvv  = *on
033390131205     c                   z-add     wEVBdev       w0080
033400070315     c     *iso          test(d)                 w0080                  31
033410070315if  2c                   if        NOT *in31
033420071025     c     *iso          movel     w0080         Data_EUR
033430071025     c     *eur          movel     Data_EUR      w0080
033440070315x   2c                   else
033450070315     c                   clear                   w0080
033460070315e   2c                   endif
033470070307     c                   eval      Tla    =  'Lasciato avviso il '
033480070307     c                                    +  %editc(w0080 : 'Y')
033490070307     c                                    +  ' alle '
033500131205     c                                    +  %editw(wEVBhev : '  :  ')
033510070307e   1c                   endif
033520030709      *
033530100107     *** * Evidenziazione servizio "espresso"
033540100107     *** * o del servizio "Poste italiane"
033550100107     ***c                   eval      *in37 = (DB0tsp = 'E')
033560100107     ***c  n37              eval      *in37 = (§OGntw = 'PPT')
033570090611      *
033580090605      /free
033590100107
033600101130         // -?Evidenziazione servizio "espresso"?
033610101130         //           ?o del servizio "Poste Italiane"?
033620100107         *in37 = (DB0tsp = 'E'   or   DB0tsp = 'H'   or
033630100107                  §OGntw = 'PPT');
033640100107
033650101130         // -?Consegna al mattino (tempi consegna = 12 ore)?
033660101130         //  ?(NON per Würth)?
033670090615         if  §VPOrcm <= wDate and
033680090622             DUTlpo  <> 'S'   and
033690090615             DB0tsp   = 'E'   and
033700090615            (DB0nrs  <> 40    or  (DB0lnp <> 043   and
033710110429                                   DB0lnp <> 277   and
033720090605                                   DB0lnp <> 063));
033730090605           clear  TNSD99ds;
033740090605           if  DB0tbo = 'P'   or   DB0tbo = '8';
033750090605             D98tbo = 'P';
033760090605           else;
033770090605             D98tbo = 'A';
033780090605           endif;
033790090605           D98aas = DB0aas;
033800090605           D98lnp = DB0lnp;
033810090605           D98nrs = DB0nrs;
033820090605           D98nsp = DB0nsp;
033830090605           tnsd99r ( TNSD99ds );
033840120910           //*if  D98tci = 12;
033850120910           if  D98tcc = 12;
033860090605             Pdsm = 'M';
033870090605           endif;
033880090605         endif;
033890090605      /end-free
033900030515      *
033910030515      * Franco prepagato
033920030515      * (richiede firma per quietanza nella copia x destinatario)
033930030515if  1c                   if        DB0tbo =  '8'  and  DB0ris <> 'M'
033940030515     c                   movea     '01'          *in(35)
033950030515if  2c                   if        Pds1   =  *blanks
033960030515     c                   eval      Pds1   =  'FRANCO PREPAGATO        '
033970030515e   2c                   endif
033980120530     c*//                if        (DB0dft > *zero       and
033990120530     c*//                           DB0dft > §vpoBRTdta)  OR
034000120530     c*//                          (DB0dft = *zero       and
034010120530     c*//                           Wdsp   > §vpoBRTdta)
034020130418     c                   eval      Pbrt2  =  $BRT
034030120530     c*//                else
034040130418     c*//                eval      Pbrt2  =  $Bartolini
034050120530     c*//                endif
034060030515      * - Legge 450 (solo per prepagati)
034070030515if  2c                   if            DB0ias =  *zeros
034080030515     c                             and DB0vas =  *blanks
034090030515     c                             and DB0div =  §GEDcn
034100060301     c                             and §GElrp >  *zeros
034110030515     c*** v. IF sovrastante...     and DB0tbo =  '8'
034120060301if  3c                   if        Wdsp   >  20060228
034130110315     c                   z-add     §GElrp        w0112
034140080701     c                   eval      Prcv   =  'Art. 1696 c.c. mod. +
034150061102     c                                       dlgs 286/2005 '
034160060301     c                                    +  §GEDcn + ' '
034170060301     c                                    +  %trim(%editc(w0112 : '4'))
034180060301     c                                    +  ' al kg.'
034190060301x   3c                   else
034200110315     c                   z-add     6.20          w0112
034210080701     c                   eval      Prcv   =  'Legge 450/85 '
034220030515     c                                    +  §GEDcn + ' '
034230030624     c                                    +  %trim(%editc(w0112 : '4'))
034240030515     c                                    +  ' al kg.'
034250060301e   3c                   endif
034260030515e   2c                   endif
034270030515e   1c                   endif
034280031031      *
034290101125      /free
034300121017
034310121017         // -?Indicazione della Partiva IVA o del Cod. Fiscale?
034320121017         //  ?(se Porto Franco Prepagato?
034330121017         //    ?o Porto Assegnato con Fattura immediata)?
034340121017         //  ?- NON se mittente estero -?
034350121017             // -?con fattura?
034360121017         IF  (DB0nft <> *zero  or   DB0ift <> *zero)   AND     (
034370121017             // - -?P.F. prepagato?
034380121017             //    ?(con mittente italiano)?
034390121017             (DB0tbo =  '8'             and  DB0nzm  = *blank)  or
034400121017             // - -?P.A. (con fattura immediata)?
034410121017             //    ?(con destinatario italiano)?
034420121017             (%subst(DB0tb1:1:1) = 'A'  and  DB0nzd  = *blank)  or
034430121017             // - -?con 2ª bolla?
034440121017             //    ?(con destinatario italiano)?
034450121017             (DB0tb2 <> *blank          and  DB0tbo <> '8'
034460121029                                        and  DB0tbo <> 'P'
034470121017                                        and  DB0nzd  = *blank) );
034480121017           select;
034490121017             when  Not $No_PI  and  wB0piv <> *blank  and
034500121017                   // ·?Esiste Partita Iva NON FITTIZIA?
034510121026                   %scan('$$' : wB0piv) = *zero;
034520121017                   // ·?Non è un C.F. - lungo 16 (privato)?
034530121026                   //and  %len( %trim( wB0cfi ) ) < 16;
034540121017               Ppiv = 'Partita Iva: ' + wB0piv;
034550121017             when  Not $No_CF  and  wB0cfi <> *blank;
034560121017               Ppiv = 'Codice Fiscale: ' + wB0cfi;
034570121017           endsl;
034580130418           //select;
034590130418           //  // ·?Se P.F. Prepagato il C.F./P.I. è del Mittente?
034600130418           //  when  DB0tbo = '8';
034610130418           //    PpivM = Ppiv;
034620130418           //  // ·?Se P.A. con fattura immed. il C.F./P.I. è del Dest.?
034630130418           //  when  %subst(DB0tb1:1:1) = 'A';
034640130418           //    PpivD = Ppiv;
034650130418           //  // ·?Se 2ª bolla (DDT sì) è del Dest.?
034660130418           //  when  DB0tb2  <> *blanks  and  DB0tbo  <> 'P';
034670130418           //    PpivD = Ppiv;
034680130418           //endsl;
034690121017         ENDIF;
034700101125
034710101125         Select;
034720101125
034730101125           // -?Non utilizzare per la consegna?
034740101125           //  ?(stampa della sola copia per il vettore)?
034750101125           When  $No_x_Cons = *on;
034760101125             *in35 = *off;
034770101125             *in36 = *off;
034780101125             // -?costante da stampare se LDV recapito c/assegni?
034790101125             //  ?stampata dal pgm "Stampa DDT NO in arrivo"?
034800101129             Pnux  = 'NON UTILIZZARE PER LA CONSEGNA';
034810101125
034820101125           // -?Ritorno all'Incasso?
034830101125           When  $Rit_a_Inc = *on;
034840101125             *in40 = *on;
034850101125             Pnux  = 'RITORNO ALL''INCASSO';
034860101125
034870101125         EndSl;
034880101125
034890101125      /end-free
034900030910      *
034910030910      * Bollo assolto in modo virtuale
034920030910if  1c                   if        $BolloVirt =  *on
034930060719if  2c                   if        DB0dft <  20060801
034940080701     c                   eval      Pbav1  =  'Bollo '
034950080701     c                                    +  %trim(%editc(wBolloVirt:'4'))
034960080701     c                                    +  ' ' + DB0div
034970080701     c                                    +  ' assolto in modo virtuale '
034980080701     c                   eval      Pbav2  =  'Aut. Ag. Ent. BO 3 27/09/02 '
034990080701     c                                    +  'Prot. 31599'
035000060719x   2c                   else
035010080701     c                   eval      Pbav1  =  'Bollo '
035020080701     c                                    +  %trim(%editc(wBolloVirt:'4'))
035030080701     c                                    +  ' ' + DB0div
035040080701     c                                    +  ' assolto in modo virtuale '
035050080701     c                   eval      Pbav2  =  'Aut. Ag. Ent. MI 2 10/07/06 '
035060080701     c                                    +  'Prot. 45114'
035070060719e   2c                   endif
035080030910e   1c                   endif
035090050503      *
035100080605      * Costanti "PER RICEVUTA DELLA MERCE..."
035110080605     c                   eval      Pkrm1 = '(nome e cognome in stampa+
035120080605     c                                       tello)'
035130080605     c                   eval      Pkrm2 = '(firma)'
035140080605     c                   eval      Pkrm3 = '(data e ora)'
035150050504sel 1c                   select
035160050504      * - danon?stampare se
035170050504      *   · franco prepagato                (NOT *in35)
035180050504      *   · non utilizzare per la consegna  (NOT *in35)
035190101125      *   · ritorno all'incasso             (*in40)
035200050504      *   · bolla FedEx                     (*in72)
035210101125w   1c                   when      NOT *in35  or  *in40  or  *in72
035220050504     c*** (già fatto:)   clear                   Pkrm
035230080605     c                   clear                   Pkrm1
035240080605     c                   clear                   Pkrm2
035250080605     c                   clear                   Pkrm3
035260080922      * - costante "DPD"
035270120301w   1c                   when      *in71
035280080922     c                   eval      Pkrm   =  'PER RICEVUTA DELLA MERCE-
035290080922     c                              SENZA RISERVE'
035300120530      * - costante "standard" per "DDT sì"
035310090615w   1c                   when          §VPOrsr <= wDate
035320081119     c                             and  DB0bam =  'S'
035330071005     c                   eval      Pkrm   =  'PER RICEVUTA DELLA MERCE-
035340071005     c                              E DEL DOCUMENTO DI TRASPORTO SENZA-
035350071005     c                              RISERVE'
035360120530      * - costante "standard" per "DDT no" + Packing-List
035370120301w   1c                   when           DB0bam =  *blank
035380161012     c                             and ($FIAR8 =  *on
035390161012     c                              or  $§PDF  =  *on)
035400120301     c                   eval      Pkrm   =  'PER RICEVUTA DELLA MERCE-
035410120301     c                              E DEL DOCUMENTO DI TRASPORTO SENZA-
035420120301     c                              RISERVE'
035430120301      * - costante "standard" altrimenti
035440050504x   1c                   other
035450050504     c                   eval      Pkrm   =  'PER RICEVUTA DELLA MERCE-
035460050504     c                              SENZA RISERVE'
035470050504e   1c                   endsl
035480090513      /free
035490090611       Select;
035500101130         // -?costante per cliente?
035510101130         //  ?"Consorzio Lotterie Nazionali - Gratta & Vinci"?
035520140513         when  DB0ccm = c_CliCLN  or  DB0ccm = c_CliLN_2;
035530090528           exsr  sr_RepDdtCLN;
035540090706           Pkrm =  'LA FIRMA PER RICEVUTA DELLA MERCE VALE ANCHE PER +
035550090706                    IL DDT N° '
035560090527                + %editc (WddtCLN : 'Z')
035570090615                + ' DEL '
035580090527                + %editc (WdspCLN : 'Y')
035590101001                + ' CON RISERVA DI CONTROLLO COME DA CONTRATTO';
035600101001           if  Wdsp < 20101001;
035610101001             Pkrm = %trimr(Pkrm) + ' C.L.N.';
035620101001           else;
035630101001             Pkrm = %trimr(Pkrm) + ' L.N. S.R.L.';
035640101001           endif;
035650101130         // -?costante per cliente?
035660101130         //  ?"S G Soluzioni Logistiche"?
035670091117         when  DB0ccm = c_CliSG   or   DB0ccm = c_CliSG_2;
035680091117           if  DB0nas <> *blank   and   DB0nas <> c_NoContr;
035690091117             Pkrm = %trimr(Pkrm)
035700091117                  + '.   INCLUDE CONTRATTO - '
035710091117                  + %trim(DB0nas);
035720091117           endif;
035730091117           if  DB0rmo <> *blank;
035740091117             if  DB0nas <> *blank;
035750091117               Pkrm = %trimr(Pkrm)
035760091117                    + ' - MATR TV: '
035770091117                    + %trim(DB0rmo);
035780091117             else;
035790091117               Pkrm = %trimr(Pkrm)
035800091117                    + '.   MATR TV: '
035810091117                    + %trim(DB0rmo);
035820091117             endif;
035830091117           endif;
035840090611       EndSl;
035850090513      /end-free
035860030717      *
035870130418      *//* Indicazione degli "oltre 20 colli"
035880130418      *//* (MAI se Franco prepagato)
035890130418if  1c*//                   if            DB0tbo <> '8'
035900130418     c*//                             and *in36  =  *off
035910130418     c*//                             and W_CoOr <> *blanks
035920130418     c*//                   eval      Pbrt2  = W_CoOr
035930130418e   1c*//                   endif
035940030515      *
035950030515      * Verifico se imponibile/iva/totale/estremi fattura
035960030515     c                   eval      *in19  = ((DB0imv <> *zeros)  or
035970030515     c                                       (DB0iva <> *zeros)  or
035980030515     c                                       (DB0cei <> *blanks) or
035990030515     c                                       (DB0dce <> *blanks) or
036000030515     c                                       (DB0ift <> *zeros)  or
036010030515     c                                       (DB0nft <> *zeros)  or
036020031125     c                                       (DB0dft <> *zeros))
036030030515      *
036040030515      * Verifico se contrassegno
036050030515     c                   eval      *in20  =  (Pcas <> *zeros)
036060030515      *
036070030709      * Verifico se totale da esigere al ritiro o alla consegna
036080080701     c                   eval      *in21  =  (Pies <> *zeros)
036090030709      *
036100030710      * Intesto il 2° modulo diversamente per prepagato
036110030710      * (solo in partenza)
036120030710if  1c                   if        *in21  and  *in36
036130030710     c                             and   (DB0tbo = 'P'
036140030710     c                                or  DB0tbo = '8')
036150030710     c                   eval      Tcxd   =  'COPIA PER IL MITTENTE    '
036160130726     c                   clear                   Tcxd2
036170030710e   1c                   endif
036180030515      *
036190120530      * Verifico se codice a barre BRT da stampare
036200030515     c                   eval      *in50  =  (Tbcd <> *blanks)
036210040629      * Verifico se codice a barre O.R.M.    da stampare
036220040629      * (non è ancora impostato - vedi subr. "StampaRCC")
036230040629     c***                eval      *in52  =  (Pbco <> *blanks)
036240030515      *
036250030515      * Flag "*" > x Consegne particolari
036260030515     c                   if        DB0tc1 =  'A'  or  DB0tc2 = 'A'
036270030515     c                   eval      Past   =  '*'
036280030515     c                   endif
036290030515      *
036300030515      * C/ASSEGNO - - - - - - - - - - - - - - - - - - - - - - - - - - *
036310030515if  1c                   IF        DB0cas <> *zeros
036320030515      * - Flag "*" > x C/Assegno
036330030904if  2c                   if        O21fca <> *blanks
036340030904     c                   movel     '*'           Past
036350030515e   2c                   endif
036360030624      * - Imposto i campi di stampa
036370030624     c                   eval      Ptic   =  DB0tic + ' ' + DB0dti
036380030515e   1c                   ENDIF
036390030515      *
036400040607      * TIPO LDV  - - - - - - - - - - - - - - - - - - - - - - - - - - *
036410030515      * - Se Bolla in Assegnato: stampo una "A" vicino all'asterisco
036420030515      * e se Bolla in Assegnato Senza Fattura: stampo "SEGUE FATTURA"
036430030515if  1c                   if           %subst(DB0tb1:1:1) = 'A'
036440030515     c                             or        DB0tb2     <> *blanks
036450030515     c                   eval      Pass   =  'A'
036460030515if  2c                   if        DB0nft =  *zeros
036470031125     c                   eval      *in22  =  *on
036480130215     c                   eval      Piva   =  'SEGUE FATTURA'
036490030515e   2c                   endif
036500030515e   1c                   endif
036510030515      *
036520030515      * DECODIFICA TIPO CONSEGNA RICHIESTA  - - - - - - - - - - - - - *
036530051129sel 1***c                   Select
036540051129w   1***c                   WHEN      DB0tcr = *blanks
036550051129     *** * - giorno si
036560051129w   1***c                   WHEN      DB0dcr <> *zeros  and  DB0tcr = 'P'
036570051129     ***c                   eval      Ptdr   =  'PRIMA DEL  '
036580051129w   1***c                   WHEN      DB0dcr <> *zeros  and  DB0tcr = 'D'
036590051129     ***c                   eval      Ptdr   =  'DOPO IL    '
036600051129     *** * - giorno no  ora si
036610051129w   1***c                   WHEN      DB0dcr =  *zeros  and  DB0tcr = 'P'
036620051129     ***c                   eval      Ptdr   =  'PRIMA DELLE'
036630051129w   1***c                   WHEN      DB0dcr =  *zeros  and  DB0tcr = 'D'
036640051129     ***c                   eval      Ptdr   =  'DOPO LE    '
036650051129e   1***c                   ENDSL
036660051129if  1c                   IF            DB0dcr >  *zeros
036670051129     c                             or  DB0hcr >  *zeros
036680051129     c                   clear                   FNLV80ds
036690051129     c                   eval      ILV80aas   =  DB0aas
036700051129     c                   eval      ILV80lnp   =  DB0lnp
036710051129     c                   eval      ILV80nrs   =  DB0nrs
036720051129     c                   eval      ILV80nsp   =  DB0nsp
036730051129     c                   eval      ILV80mgs   =  DB0mgs
036740051129     c                   eval      ILV80tc1   =  DB0tc1
036750051129     c                   eval      ILV80tc2   =  DB0tc2
036760051129     c                   eval      ILV80dcr   =  DB0dcr
036770051129     c                   eval      ILV80hcr   =  DB0hcr
036780051129     c                   eval      ILV80tcr   =  DB0tcr
036790051129     c                   call      'FNLV80R'
036800051129     c                   parm                    KPJBA
036810051129     c                   parm                    FNLV80ds
036820051129sel 2c                   Select
036830051129      * errore
036840051129w   2c                   When      OLV80err  <>  *blanks
036850051129      * consegna tassativa (per ora solo ai supermercati)
036860051129w   2c                   When      OLV80tat   =  'S'
036870051129     c                   movel     OLV80tcrd     Pcot
036880091116      * NO "Prima del" per il cliente TTL
036890091116w   2c                   When      DB0ccm     = c_CliTTL   and
036900091116     c                             DB0dcr    <> *zeros     and  DB0tcr = 'P'
036910051129      * prima/dopo il/delle
036920051129      * (il giorno e/o l'ora di consegna richiesta sono già impostati
036930051129      *  nella subr. "TipoLDV")
036940051129w   2c                   Other
036950051129     c                   movel     OLV80tcrd     Ptdr
036960051129e   2c                   Endsl
036970051129e   1c                   ENDIF
036980030515      *
036990030515      * DECODIFICA DEI TURNI DI CHIUSURA  - - - - - - - - - - - - - - *
037000060921      * (SE?non?è "Fermo Deposito")
037010060921if  1c                   if        DB0ffd =  *blanks
037020060921     c                   eval      Pktc   =  'TURNO DI CHIUSURA'
037030060921e   1c                   endif
037040030515      * 1° turno di chiusura
037050060921if  1c                   IF             DB0gc1 <> *blanks
037060060921     c                             and  DB0ffd  = *blanks
037070030515      * - Flag "*" > Turno di chiusura
037080030515     c                   eval      Past   =  '*'
037090030515if  2c                   If        %subst(DB0gc1:1:1) = *blank
037100030515      * - Nessun giorno specificato - Solo 'M' o 'P'
037110060921     c*** già così:      clear                   Pgc1
037120030515if  3c                   if        DB0gc1 =  ' M'
037130030515     c                   eval      Pgc1   =  %trim($Gio(8))
037140030515x   3c                   else
037150030515     c                   eval      Pgc1   =  %trim($Gio(9))
037160030515e   3c                   endif
037170030515x   2c                   Else
037180030515      * - Giorno specificato
037190030515     c                   movel     DB0gc1        w001a
037200030515     c                   eval      XX     =  1
037210030515     c                   eval      *in31  =  *off
037220030515     c     w001a         lookup    $GgS(xx)                               31
037230030515     c   31              movel     $Gio(xx)      Pgc1
037240030515      *   + 'M' o 'P'
037250030515sel 3c                   select
037260030515w   3c                   when      %subst(DB0gc1:2:1) = 'M'
037270030515     c                   eval      Pgc1   =  %trim(Pgc1) + ' '
037280030515     c                                    +  %trim($Gio(8))
037290030515w   3c                   when      %subst(DB0gc1:2:1) = 'P'
037300030515     c                   eval      Pgc1   =  %trim(Pgc1) + ' '
037310030515     c                                    +  %trim($Gio(9))
037320030515e   3c                   endsl
037330030515e   2c                   Endif
037340030515e   1c                   ENDIF
037350030515      * 2° turno di chiusura
037360060921if  1c                   IF             DB0gc2 <> *blanks
037370060921     c                             and  DB0ffd  = *blanks
037380030515      * - Flag "*" > Turno di chiusura
037390030515     c                   eval      Past   =  '*'
037400030515if  2c                   If        %subst(DB0gc2:1:1) = *blank
037410030515      * - Nessun giorno specificato - Solo 'M' o 'P'
037420060921     c*** già così:      clear                   Pgc2
037430030515if  3c                   if        DB0gc2 =  ' M'
037440030515     c                   eval      Pgc2   =  %trim($Gio(8))
037450030515x   3c                   else
037460030515     c                   eval      Pgc2   =  %trim($Gio(9))
037470030515e   3c                   endif
037480030515x   2c                   Else
037490030515      * - Giorno specificato
037500030515     c                   movel     DB0gc2        w001a
037510030515     c                   eval      XX     =  1
037520030515     c                   eval      *in31  =  *off
037530030515     c     w001a         lookup    $GgS(xx)                               31
037540030515     c   31              movel     $Gio(xx)      Pgc2
037550030515      *   + 'M' o 'P'
037560030515sel 3c                   select
037570030515w   3c                   when      %subst(DB0gc2:2:1) = 'M'
037580030515     c                   eval      Pgc2   =  %trim(Pgc2) + ' '
037590030515     c                                    +  %trim($Gio(8))
037600030515w   3c                   when      %subst(DB0gc2:2:1) = 'P'
037610030515     c                   eval      Pgc2   =  %trim(Pgc2) + ' '
037620030515     c                                    +  %trim($Gio(9))
037630030515e   3c                   endsl
037640030515e   2c                   Endif
037650030515e   1c                   ENDIF
037660071114      *
037670071114      * ALTRI DATI/FLAG/... - - - - - - - - - - - - - - - - - - - - - *
037680030515      *
037690071114      * Se pgm richiamato x stampa LdV in arrivo:
037700030515if  1c                   IF        DB0fl1 =  'A'
037710071114      * - imposto l'"*" in base al parametro ricevuto.
037720030515if  2c                   if        DB0fl2 =  '*'
037730030515     c                   eval      Past   =  '*'
037740030515x   2c                   else
037750030515     c                   clear                   Past
037760030515e   2c                   endif
037770071114      * - idem per flag di bolla non abilitata
037780030515     c                   eval      Pdis   =  DB0fl5
037790030515e   1c                   endif
037800061102      *
037810071114      * Decodifica cliente a cui richiedere il codice fiscale e/o la
037820071114      *   partita iva
037830071114if  1c                   IF             $No_CF =  *on
037840071114     c                             or   $No_PI =  *on
037850061106     c                   clear                   wRSxCF
037860061102sel 2c                   select
037870061102      * - P.F. prepagato
037880061106w   2c                   when      DB0tbo =  '8'
037890061106     c                   movel     DB0rsm        wRSxCF
037900061102      * - P.A. con fattura immediata
037910061106w   2c                   when      %subst(DB0tb1:1:1) = 'A'
037920061106     c                   movel     DB0rsd        wRSxCF
037930061106      * - con 2ª bolla
037940061103w   2c                   when           DB0tb2 <> *blanks
037950121029     c                             and  DB0tbo <> 'P'
037960061103     c                             and  DB0tbo <> '8'
037970061106     c                   movel     DB0rsd        wRSxCF
037980061102e   2c                   endsl
037990061102      *
038000061102e   1c                   ENDIF
038010030515      *
038020030515     c                   ENDSR
038030080918      /free
038040090528
038050090528       //--------------------------------------------------------------
038060140303       //?Reperimento Numero e Data del DDT originale del mittente     ?
038070140303       //?"Consorzio Lotterie Nazionali - Gratta & Vinci"              ?
038080090528       //--------------------------------------------------------------
038090090528       BEGSR  sr_RepDdtCLN;
038100090528
038110110426         // -?Verifica se la spedizione ha mamma?
038120090528         if  not %open(FNLBL01L);
038130090528           open  FNLBL01L;
038140090528         endif;
038150090528         chain  (DB0aas : DB0lnp : DB0nrs : DB0nsp)  FNLBL000;
038160090528
038170090528         Select;
038180090528
038190110426           // -?Con Mamma: reperimento dati DDT dalla mamma?
038200090528           When  %found(FNLBL01L);
038210090528             if  not %open(FNBLP01L);
038220090528               open  FNBLP01L;
038230090528             endif;
038240090603             chain  (LBLaao : LBLlpo : LBLnro : LBLnso)  FNBLP000;
038250090528             if  %found(FNBLP01L);
038260090528               exsr  sr_DatiCLN_blp;
038270090528             endif;
038280090528
038290110426           // -?Dati da DS?
038300090901           When  DB0ges = 'B';
038310090901             exsr  sr_DatiCLN_DS;
038320090901
038330110426           // -?In Partenza: reperimento dati DDT da FNBLP?
038340090528           When  (DB0tbo = 'P'   or   DB0tbo = '8');
038350090528             exsr  sr_DatiCLN_blp;
038360090528
038370110426           // -?In Arrivo: reperimento dati DDT da FNARB?
038380090528           Other;
038390090528             exsr  sr_DatiCLN_arb;
038400090528
038410090528         EndSl;
038420090528
038430090528       ENDSR;
038440090528
038450090901       //--------------------------------------------------------------
038460140303       //?Da DS                                                        ?
038470090901       //--------------------------------------------------------------
038480090901       BEGSR  sr_DatiCLN_DS;
038490090901
038500110426         // -?Numero DDT?
038510090901         select;
038520090902           when  DB0rmn <> *zero;
038530100906             if  DB0rmn  > -99999999   and
038540100906                 DB0rmn <   99999999;
038550100906               WddtCLN = DB0rmn;
038560100906             endif;
038570090902           when  DB0ncd <> *zero;
038580090902             WddtCLN = DB0ncd;
038590090901         endsl;
038600090901
038610110426         // -?Data DDT in formato gg/mm/aaaa?
038620090902         Data_Eur = %date( ((DB0aas * 10000) + DB0mgs) : *iso );
038630090901         WdspCLN  = ( ( %subdt(Data_Eur : *days)  * 1000000 ) +
038640090901                      ( %subdt(Data_Eur : *months) * 10000 ) +
038650090901                      ( %subdt(Data_Eur : *years) ) );
038660090901
038670090901       ENDSR;
038680090901
038690090528       //--------------------------------------------------------------
038700090528       //?In Partenza: reperimento dati DDT da FNBLP                   ?
038710090528       //--------------------------------------------------------------
038720090528       BEGSR  sr_DatiCLN_blp;
038730090528
038740110426         // -?Numero DDT?
038750090528         select;
038760090528           when  BLPrmn <> *zero;
038770100906             if  BLPrmn  > -99999999   and
038780100906                 BLPrmn <   99999999;
038790100906               WddtCLN = BLPrmn;
038800100906             endif;
038810090528           when  BLPncd <> *zero;
038820090528             WddtCLN = BLPncd;
038830090528         endsl;
038840090528
038850110426         // -?Data DDT in formato gg/mm/aaaa?
038860090528         Data_Eur = %date( ((BLPaas * 10000) + BLPmgs) : *iso );
038870090528         WdspCLN  = ( ( %subdt(Data_Eur : *days)  * 1000000 ) +
038880090528                      ( %subdt(Data_Eur : *months) * 10000 ) +
038890090528                      ( %subdt(Data_Eur : *years) ) );
038900090528
038910090528       ENDSR;
038920090528
038930090528       //--------------------------------------------------------------
038940090528       //?In Arrivo: reperimento dati DDT da FNARB                     ?
038950090528       //--------------------------------------------------------------
038960090528       BEGSR  sr_DatiCLN_arb;
038970090528
038980110426         // -?Numero DDT?
038990090528         select;
039000090528           when  ARBrmn <> *zero;
039010100906             if  ARBrmn  > -99999999   and
039020100906                 ARBrmn <   99999999;
039030100906               WddtCLN = ARBrmn;
039040100906             endif;
039050090528           when  ARBncd <> *zero;
039060090528             WddtCLN = ARBncd;
039070090528         endsl;
039080090528
039090110426         // -?Data DDT in formato gg/mm/aaaa?
039100090528         Data_Eur = %date( ((ARBaas * 10000) + ARBmgs) : *iso );
039110090528         WdspCLN  = ( ( %subdt(Data_Eur : *days)  * 1000000 ) +
039120090528                      ( %subdt(Data_Eur : *months) * 10000 ) +
039130090528                      ( %subdt(Data_Eur : *years) ) );
039140090528
039150090528       ENDSR;
039160080918
039170080918       //--------------------------------------------------------------
039180140303       //?Stampa della sola Packing-List (2 per A4)                    ?
039190080918       //--------------------------------------------------------------
039200080918       BEGSR  StampaPkgLst;
039210130315
039220130315         // -?SE NON esiste il *file FIAR8 => uscita?
039230130315         if  $E_AR8 = *off;
039240130315           leavesr;
039250130315         endif;
039260080918
039270110426         // -?MODIFICA SOSPESA:?
039280110426         //  ?NON STAMPA LA PACKING-LIST DAL 2° TENTATIVO DI CONSEGNA?
039290081013         //if   $FIAR8 = *off   or   DB0fl1 = 'D';
039300081013         //  leavesr;
039310081013         //endif;
039320080918
039330110426         // -?Azzeramento numero di pagina per Lettera di Vettura?
039340110426         //  ?(meglio: "reimpostazione a 1")?
039350080926         reset  wPage;
039360080918
039370110426         // -?Posizionamento sul foglio A4 in base al n° pagina in stampa?
039380130418         *in33 = *off;
039390130418         exsr  Posiz_Box;
039400080919
039410110426         // -?Impostazione dei dati da stampare nel rec. con la costante?
039420110426         //  ?"Copia per il destinatario"  + estremi spedizione e pag.?
039430110426         //
039440110426         // -?"Copia per il destinatario" già impostato in sr. "PrepPrt"?
039450081017         //if  §AR8cxd = *blank;
039460130726         //  Tcxd  = c_CopiaXdest;
039470130726         //  Tcxd2 = Tcxd;
039480081017         //endif;
039490081017
039500110426         // -?Estremi spedizione da impostare SOLO se si stampando?
039510110426         //  ?la sola packing-list (DB0FA4 = "P")?
039520081017         if  DB0fa4 = 'P';
039530101028           Trsd = Prsd;
039540080922           Trif = 'Spedizione '
039550080924                + %editc(DB0lnp : 'X') + ' '
039560110310                + %editc(DB0nrs : 'X') + ' '
039570110310                + %editc(DB0nsp : 'X')
039580080922                + ' del ' + %editc(Pdsp : 'Y')
039590081017                + '        Pag.'
039600080922                + %editc(wPage:'Z');
039610101028           Tndc = Pndc;
039620081002           if  wSeqLdV = 1;
039630081002             §Pos   = wPosIni + c_PosBox1As + 0.040;
039640090422             §Pos   = wPosIni + c_PosBox1As + 0.035;
039650081002           else;
039660081002             §Pos   = wPosIni + c_PosBox1As - 0.070;
039670081002           endif;
039680080919         endif;
039690081017
039700110426         // -?La stampa di tale record verrà fatta dopo?
039710081017         //if  §AR8cxd = *blank   or   DB0fa4 = 'P';
039720081017         //  write A4prtDT0;
039730081017         //endif;
039740080918
039750110426         // -?Stampa Packing-List?
039760130315         exsr  StampaDDT;
039770080918
039780080918       ENDSR;
039790090528
039800080918      /end-free
039810030515      *
039820030515      *---------------------------------------------------------------*
039830050217      *?Stampa della Lettera di Vettura                              ?*
039840030515      *---------------------------------------------------------------*
039850040607     c     StampaLDV     BEGSR
039860080604      *
039870080604      * - Posizionamento sul foglio A4 in base al n° pagina in stampa
039880130418     c                   eval      *in33 = *off
039890130418     c                   exsr      Posiz_Box
039900030515      *
039910040607      * Azzeramento numero di pagina per Lettera di Vettura
039920030716      * (meglio: "reimpostazione a 1")
039930080926     c                   reset                   wPage
039940030515      *
039950120530      * 1ª parte - copia per il vettore (da ritornare in "BRT")
039960130418     c                   write     A4prtLDV
039970130418     c                   write     A4prtBOX
039980110426     c*//                write     A4LogoBAR
039990081020      /free
040000120530         //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
040010120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
040020130306         //  // >?Simil-Logo Bartolini/BRT?
040030120530         //  write  A4LogoBR2;
040040120530         //  write  A4LogoDA2;
040050120530         //else;
040060130306         //  // >?Simil-Logo Bartolini?
040070120530         //  write  A4LogoBAR;
040080120530         //  write  A4LogoDA1;
040090120530         //endif;
040100130306         // >?Simil-Logo BRT?
040110120530         write  A4LogoBRT;
040120120530         write  A4LogoDA3;
040130110426
040140110426         // >?dati DPD?
040150081020         if  *in71;
040160081020           exsr  StampaDPD;
040170081020         endif;
040180130306
040190130306         // -?Se LdV con PDF => registrazione rec. nel *file PDFLIST?
040200130306         if  %parms() > 1  and  (DB6pdf = 'F'  or  DB6pdf = 'X')
040210130306                           and  $§PDF   = *on;
040220130306           exsr  sr_WrtRec_PdfList;
040230130306         endif;
040240081020
040250130306         // -?Se richiesta solo tagliatella (copia per il vettore, da?
040260130306         //  ?lasciare però al destinatario) => fine?
040270081020         if  DB0fa4  = '4';
040280081020           leavesr;
040290081020         endif;
040300081020      /end-free
040310050510      *
040320081028      * Se LdV in formato "ex-A5" rimane la copia per il destinatario
040330050510     c     *in69         cabeq     *on           End_LdV
040340030515      *
040350030515sel 1c                   select
040360081028      * > dati DPD  (ora in ex-A5 o in 1/4 di A4)
040370081028w   1c***                when      *in71
040380030515      * > dati FedEx
040390030515w   1c                   when      *in72
040400030515     c                   exsr      ListaFED
040410081028      * > dati EuroExpress  (non più gestiti)
040420040223w   1c                   when      *in73
040430030515      *
040440030515e   1c                   endsl
040450030910      *
040460030910      * > elenco SEGNACOLLI NON SEQUENZIALI
040470050511      *  (no x max 2 perchè sono già stati elencati nell'apposita area)
040480030910      *  (no x DPD   perchè monocollo)
040490030910      *  (no x FedEx perchè segnacolliamo noi)
040500030910      *  (no x "A5"  perchè sono già stati elencati tra le "Note e
040510030910      *              Particolarità")
040520030910if  1c                   if        DB0fns =  'S'
040530050511     c                             and  DB0ncl > 2
040540050513     c                             and  $A4_x_ScNS  =   *on
040550030910     c***                          and  not *in71  and  not *in72
040560030910     c                             and  not *in69
040570030910     c                   exsr      ListaScNS
040580030910e   1c                   endif
040590031124      *
040600031124      * Reperimento della posizione utilizzabile per la stampa dei box
040610080825      *   (in pollici) in base all'ultima riga stampata in 1/8" o 1/9"
040620031124     c                   exsr      CalcRowPos
040630060524      *
040640070123      * > Spedizione con D.A.S. x cliente "PRAOIL OLEODOTTI ITALIANI"
040650081127if  1c                   if             DB0ccm = c_CliPraOil
040660060529     c                             or  (DB0fbr = 'S'
040670081127     c                             and  §B4Aks = c_CliPraOil)
040680060524     c                   exsr      StampaDAS
040690060524e   1c                   endif
040700031124      *
040710031124      * > Promemoria per autotrasportatore
040720110502if  1c*//*               if            (DB0gma = Ktab7V_CB
040730110502     c*//*                           or DB0gma = Ktab7V_C1
040740080625     c*** scontato x GMA='C1'       and DB0bam = 'S'
040750110502     c*//*                           or DB0gma = Ktab7V_C2
040760080729     c*** scontato x GMA='C2'       and DB0bam = 'S'
040770110502     c*//*                           or DB0gma = Ktab7V_C3
040780100513     c*** scontato x GMA='C3'       and DB0bam = 'S'
040790110411     c*//*                           or DB0gma = Ktab7V_C4
040800110411     c*//*                           or DB0gma = Ktab7V_C5
040810110502     c*//*                           or DB0gma = Ktab7V_R1
040820110502     c*//*                           or DB0gma = Ktab7V_NR)
040830110502     c*//*                         and  §7Rldv = 'S'
040840110502     c                   if            (§7Rldv = 'S'
040850110502     c                             and  $TabPXA2  = *on)
040860061106     c                             OR   $No_CF = *on
040870071114     c                             OR   $No_PI = *on
040880070306     c                             OR   $FIARN = *on
040890070315     c                             OR   $C_A_  = *on
040900080801     c                             OR   $ConsParz = *on
040910090302     c                             OR   $TabPXA   = *on
040920110411     c                             OR   $TabPXA2  = *on
040930031124     c                   exsr      ListaPMxA
040940031124e   1c                   endif
040950030515      *
040960030515      * > dati FIAR5
040970150224if  1c*//                if           %subst(DB0gva:1:1) = 'B'
040980150224     c*//                          or %subst(DB0gva:1:1) = 'O'
040990150224if  1c                   if                  DB0tc1      = 'S'
041000030717     c                             or        DB0tc2      = 'S'
041010030515     c                   exsr      ListaAR5
041020030515e   1c                   endif
041030030910      *
041040071015      * > Riferimento O.R.M.
041050071015sel 1c                   select
041060071015      * - > per ritiro contestuale alla consegna
041070100608w   1c                   when          (§7Rrc  = 'S'
041080071015     c                             and  §7Rldv = 'S')
041090030910     c                   exsr      StampaRCC
041100071015      * - > per ritiro porto franco prepagato
041110071015w   1c                   when           DB0tbo = '8'
041120071015     c                   exsr      StampaRPFPP
041130071015e   1c                   endsl
041140090924      /free
041150090924
041160110426         // >?Note per destinatario?
041170090924if  1    if  $BoxNxD;
041180090924           exsr  StampaNxD;
041190090924e   1    endif;
041200090924
041210090924      /end-free
041220040607      *
041230081013      * > DDT in LDV
041240081013      * - IN FUTURO: se al 1° tentativo di consegna
041250081013      *              (DB0FL1 = "D" - v. pgm FNLRS9R)
041260080917if  1c                   if             $FIAR8 =  *on
041270130315     c                             and  $E_AR8 =  *on
041280081013     c***-=»>                      and  DB0fl1 <> 'D'
041290040607     c                   exsr      StampaDDT
041300040607e   1c                   endif
041310050510      *
041320050510     c     End_LdV       tag
041330030515      *
041340080703      * 2ª parte - copia per il destinatario (da lasciare)
041350130418     c                   eval      *in33 = *on
041360040607     c                   exsr      ChiusuraLDV
041370030515      *
041380030515      * Stampa dei 3 asterischi finali (se richiesta)
041390030515if  1c                   if        DB0ges = '*'
041400030515     c                   move      '***'         Pmsg3
041410130418     c                   eval      *in33 = *off
041420130418     c                   exsr      Posiz_Box
041430030515     c                   write     A4prtMSG
041440030515e   1c                   endif
041450030515      *
041460030515     c                   ENDSR
041470080604      /free
041480080604
041490080604       //--------------------------------------------------------------
041500140303       //?Posizionamento del box all'interno della pag. A4             ?
041510080604       //--------------------------------------------------------------
041520130418       BEGSR  Posiz_Box;
041530080704
041540080704         select;
041550080704
041560110426           // -?Richiesta stampa di 1 LdV x pagina?
041570110426           //  ?(LdV in modifica o con packing-list del mitente)?
041580080704           when  DB0fa4 = '1'   or   DB0fa4 = 'S';
041590080704             $LdVxP   = 1;
041600080918             $Len1LdV = $LenLdV2;
041610080704
041620110426           // -?Richiesta stampa di 2 LdV x pagina (ex A5)?
041630110426           //  ?(copia per il vettore & copia per il destinatario)?
041640080704           when  DB0fa4 = '2';
041650080704             $LdVxP   = 2;
041660080704             $Len1LdV = $LenLdV2;
041670080704
041680110426           // -?Richiesta stampa di 4 LdV x pagina?
041690110426           //  ?(solo copia per il vettore)?
041700080704           when  DB0fa4 = '4';
041710080704             $LdVxP   = 4;
041720080704             $Len1LdV = $LenLdV4;
041730080918
041740110426           // -?Richiesta stampa della sola packing-list?
041750110426           //  ?(solo copia per il vettore)?
041760080918           when  DB0fa4 = 'P';
041770080918             $LdVxP   = 2;
041780080924             $Len1LdV = c_Len2_inch;
041790080704
041800080704           other;
041810080704             reset $LdVxP;
041820080704             reset $Len1LdV;
041830080704
041840080704         endsl;
041850080704
041860110426         // -?Richiesta "ripartenza" da foglio A4 successivo?
041870130418         if DB0fl6 = *on  and  Not  *in33;
041880080704           clear  wNumLdV;
041890080704         endif;
041900080704
041910110426         // -?Calcolo posizione di partenza all'interno del foglio A4.?
041920110426         //  ?[Se 2ª LdV nella pagina, ma LdV per A4 (intero):?
041930110426         //   ?si salta alla pag. A4 successiva; la LdV in esame?
041940110426         //   ?dev'essere stampata in posizione iniziale dell'A4,?
041950110426         //   ?seguita dai suoi dati ulteriori.]?
041960130418         if  Not  *in33;
041970130418           wNumLdV  += 1;
041980130418           wSeqLdV   = %rem( wNumLdV : $LdVxP );
041990130418         endif;
042000080704
042010130422         // -?Stampa della copia per il vettore?
042020130422         IF  NOT  *in33;
042030130422
042040130422           select;
042050130422
042060130422             // -?Modulo A4 intero?
042070130422             when  NOT  *in69    and   DB0fa4 <> '4'
042080130422                                 and   DB0fa4 <> 'P';
042090130422               wSeqLdV = 1;
042100130422             when  DB0fa4 = '1'   or   DB0fa4 = 'S';
042110130422               wSeqLdV = 1;
042120130422
042130130422             // -?Calcolo della posizione x box all'interno della pagina "A4"?
042140130422             when  wSeqLdV = *zero   and
042150130422                   $LdVxP  = 2;
042160130422               wSeqLdV = 2;
042170130422             when  wSeqLdV = *zero   and
042180130422                   DB0fa4  = '4';
042190130422               wSeqLdV = 4;
042200130422             when  wSeqLdV = *zero   and
042210130422                   *in69;
042220130422               wSeqLdV  = 2;
042230130422
042240130422           endsl;
042250130422
042260130422         ELSE;
042270130422
042280130422           select;
042290130422
042300130422             // -?Modulo A5 o ricevuta (¼ di A4)?
042310130422             when  *in69;
042320130422
042330130422             // -?Modulo A4 intero - 1ª parte (alta)?
042340130422             when  CurLineA4 <  26   and   §PS1 < c_§PS1;
042350130422               wSeqLdV = 1;
042360130422
042370130422             // -?Modulo A4 intero - 2ª parte (passa)?
042380130422           //when  CurLineA4 <  78;   (in 1/9", ma sarà in 1/8"...)
042390130422             when  CurLineA4 <  70;
042400130422               wSeqLdV  = 2;
042410130422
042420130422             // -?Modulo A4 intero - superata 2ª parte?
042430130422             //  ?PER ORA?IMPOSSIBILE !!!?
042440130422             other;
042450130422               clear wMaxLine;
042460130422               exsr  Test_OF;
042470130422               wMaxLine = $MaxLine2;
042480130422               wSeqLdV  = 1;
042490130422
042500130422           endsl;
042510130422
042520130422         ENDIF;
042530080919
042540110426         // -?Impostaz. delle posizioni nella pagina in corso di stampa?
042550080919         wPosIni = ($Len1LdV * (wSeqLdV - 1));
042560130422         if  *in33;       // -?Stampa della copia per il destinatario?
042570130422           wPosIni += $LenLdV4;
042580130422         endif;
042590110426
042600120530         //// -?Impostaz. dimensioni per il logo Bartolini-BRT?
042610120530         //If  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
042620120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
042630120530         //  reset  c_Spessore;
042640120530         //  reset  c_DeltaSpig;
042650120530         //  reset  c_LunghMaxV;
042660120530         //  reset  c_LunghV;
042670120530         //  reset  c_LunghO;
042680120530         //  reset  c_XspaceX;
042690120530         //  reset  c_BOXspace;
042700120530         //Else;
042710120530         //  c_Spessore  = c_1_Spessore;
042720120530         //  c_DeltaSpig = c_1_DeltaSpig;
042730120530         //  c_LunghMaxV = c_1_LunghMaxV;
042740120530         //  c_LunghV    = c_1_LunghV;
042750120530         //  c_LunghO    = c_1_LunghO;
042760120530         //  c_XspaceX   = c_1_XspaceX;
042770120530         //  c_BOXspace  = c_1_BOXspace;
042780120530         //EndIf;
042790120530         // -?Dimensioni per il logo BRT già definite?
042800120530         //c_Spessore  = c_2_Spessore;
042810120530         //c_DeltaSpig = c_2_DeltaSpig;
042820120530         //c_LunghMaxV = c_2_LunghMaxV;
042830120530         //c_LunghV    = c_2_LunghV;
042840120530         //c_LunghO    = c_2_LunghO;
042850120530         //c_XspaceX   = c_2_XspaceX;
042860120530         //c_BOXspace  = c_2_BOXspace;
042870080704
042880110426         // -?Se richiesta la stampa della sola packing-list?
042890110426         //  ?NON si dovranno stampare i dati della LdV?
042900110426         //  ?(quindi NON servono i posizionamenti che seguono)?
042910080918         if  DB0fa4 = 'P';
042920080918           leavesr;
042930080918         endif;
042940080918
042950110426         // -?Posizionamento nel foglio A4 in base al n° LdV in stampa?
042960110426         //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
042970110426
042980130418         select;
042990130418           when  DB0fa4 = '4';
043000130418             *in41 = (wSeqLdV = 1);
043010130418             *in42 = (wSeqLdV = 2);
043020130418             *in43 = (wSeqLdV = 3);
043030130418             *in44 = (wSeqLdV = 4);
043040130418           when Not *in33;
043050130418             *in41 = (wSeqLdV = 1);
043060130418             *in42 = *off;
043070130418             *in43 = (wSeqLdV = 2);
043080130418             *in44 = *off;
043090130418           other;
043100130418             *in41 = *off;
043110130418             *in42 = (wSeqLdV = 1);
043120130418             *in43 = *off;
043130130418             *in44 = (wSeqLdV = 2);
043140130418         endsl;
043150080704         WRITE  A4prtPOS;
043160080704
043170110426         // -?Posizionamenti nella copia per il vettore?
043180110426         //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
043190080704
043200110426         // -?Box in copia per il vettore?
043210080704         §Box1As = wPosIni + c_PosBox1As;
043220140919         §Box1Ai = wPosIni + c_PosBox1Bs  + c_LineNarrow;
043230080704         §Box1Bs = wPosIni + c_PosBox1Bs;
043240080704         §Box1Bi = wPosIni + c_PosBox1Bi;
043250140919         §Box1Bj = wPosIni + c_PosLin1oD1 + c_LineNarrow;
043260080704         §Box1Cs = wPosIni + c_PosBox1Cs;
043270140919         §Box1Ci = wPosIni + c_PosBox1Ds  + c_LineWide;
043280080704         §Box1Ds = wPosIni + c_PosBox1Ds;
043290091119         §Box1Di = §Box1Bi;
043300081218
043310120530         //If  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
043320120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
043330120530         //  // -?Simil-Logo Bartolini?
043340120530         //  §PlblS  = §Box1As + 0.125;
043350120530         //  §PlblVS = 7.092;
043360120530         //  exsr  sr_SimilLogoBAR;
043370120530         //  // -?Simil-Logo BRT?
043380120530         //  §P2blS  = §PlblS;
043390120530         //  §P2blVS = §PlblVD + 0.030;
043400120530         //  exsr  sr_SimilLogoBRT;
043410120530         //Else;
043420120530         //  // -?Simil-Logo Bartolini?
043430120530         //  §PlblS  = §Box1As;
043440120530         //  §PlblVS = 7.128;
043450120530         //  exsr  sr_SimilLogoBART;
043460120530         //EndIf;
043470120530         // -?Simil-Logo BRT?
043480120530         §PlblS  = §Box1As;
043490120530         §PlblVS = 7.581;
043500120530         exsr  sr_SimilLogoBRT_;
043510080704
043520120530         // -?Costanti dati anagrafici BRT?
043530110426         §PC0    = wPosIni + c_PosTBar;
043540110426         §PC1    = wPosIni + c_PosTBa1;
043550120530         //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
043560120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
043570120530         //  §PC2    = wPosIni + c_PosTBa2;
043580120530         //  §PC3    = wPosIni + c_PosTBa3;
043590120530         //  §PC4    = wPosIni + c_PosTBa4;
043600120530         //  §PC5    = wPosIni + c_PosTBa5;
043610120530         //  §PC6    = wPosIni + c_PosTBa6;
043620120530         //else;
043630120530         //  §PC2    = wPosIni + c_PosTBa2x;
043640120530         //  §PC3    = wPosIni + c_PosTBa3x;
043650120530         //  §PC4    = wPosIni + c_PosTBa4x;
043660120530         //  §PC5    = wPosIni + c_PosTBa5x;
043670120530         //  §PC6    = wPosIni + c_PosTBa6x;
043680120530         //endif;
043690120530         §PC2    = wPosIni + c_PosTBa2x;
043700120530         §PC3    = wPosIni + c_PosTBa3x;
043710120530         §PC4    = wPosIni + c_PosTBa4x;
043720120530         §PC5    = wPosIni + c_PosTBa5x;
043730120530         §PC6    = wPosIni + c_PosTBa6x;
043740130726         §PC7    = §Box1Bi - 0.150;
043750110426
043760110426         // -?Linee orizzontali nel box in copia per il vettore?
043770080704         §Lo1S1  = wPosIni + c_PosLin1oS1;
043780080704         §Lo1S2  = wPosIni + c_PosLin1oS2;
043790080922       //§Lo1S3  = wPosIni + c_PosLin1oS3;
043800080704         §Lo1S4  = wPosIni + c_PosLin1oS4;
043810091119         §Lo1S5  = wPosIni + c_PosLin1oS5;
043820080704         §Lo1C1  = wPosIni + c_PosLin1oC1;
043830080704         §Lo1C2  = wPosIni + c_PosLin1oC2;
043840140915         §Lo1C3  = wPosIni + c_PosLin1oC3;
043850080704         §Lo1D1  = wPosIni + c_PosLin1oD1;
043860080704
043870110426         // -?Lunghezza linee verticali nel box in copia per il vett.?
043880140922         §LLv1A  = §Lo1S5  - §Lo1S1;
043890140922         //§LLv1B  = §Lo1S4  - §Box1Bs;
043900140922         //§LLv1C  = §Box1Di - §Box1Bs;
043910140922         //***§LLv1B  = §Lo1S4  - §Box1Bj + c_LineNarrow;
043920140922         §LLv1B  = §Lo1S4  - §Box1Bj;
043930140922         §LLv1C  = §Box1Di - §Box1Bj;
043940140917
043950140917         // -?Costante "BRTcode" & codice BRTcode?
043960140917         §Pk1    = wPosIni + c_PosKBRTc;
043970080704
043980110426         // -?Costante "Consegna tassativa"?
043990080704         §Pk2    = wPosIni + c_PosKTass;
044000080704
044010110426         // -?Costanti "(nome e cognome in stampatello)", "(firma)" e?
044020110426         //           ?"(data e ora)"?
044030080704         §Pk3    = §Box1Bi - 0.015;
044040080704
044050110426         // -?Costante con rif. alla legge in vigore per RCV?
044060080704         §Pk4    = wPosIni + c_PosKRCV;
044070080704
044080110426         // -?Costante "Bollo assolto in modo virtuale..."?
044090080704         §Pk5    = wPosIni + c_PosKBAV;
044100080704         §Pk6    = §Pk5 + 0.060;
044110121017
044120121017         // -?Partita IVA o Cod. Fiscale del cliente?
044130121017         §Pk7    = §Pk4 + 0.080;
044140080704
044150110426         // -?Costante "COPIA PER IL DESTINATARIO" in DDT "generico"?
044160080709         §Pos    = c_PosKCXD2;
044170080704
044180080704       ENDSR;
044190080605
044200080604      /end-free
044210110426
044220140303       //--------------------------------------------------------------
044230140303       //?Posizionamento del simil-logo Bartolini (unico)              ?
044240140303       //--------------------------------------------------------------
044250120530      *///copy gaitrasrc/srcTNSY,LogoBAR_R2
044260110426
044270140303       //--------------------------------------------------------------
044280140303       //?Posizionamento del simil-logo Bartolini (con BRT)            ?
044290140303       //--------------------------------------------------------------
044300120530      *///copy gaitrasrc/srcTNSY,LogoBAR3R2
044310110426
044320140303       //--------------------------------------------------------------
044330140303       //?Posizionamento del simil-logo BRT (con BARTOLINI)            ?
044340140303       //--------------------------------------------------------------
044350120530      *///copy gaitrasrc/srcTNSY,LogoBRT3R2
044360120530
044370140303       //--------------------------------------------------------------
044380140303       //?Posizionamento del simil-logo BRT (unico)                    ?
044390140303       //--------------------------------------------------------------
044400120530      /copy gaitrasrc/srcTNSY,LogoBRT_R2
044410110426
044420030515      *---------------------------------------------------------------*
044430050217      *?Gestione overflow                                            ?*
044440030515      *---------------------------------------------------------------*
044450030515     c     Test_OF       BEGSR
044460030515      *
044470030515      * Test raggiungimento della posizione limite
044480081003     c                   if          ((§PS1 < $LimOF   and   NOT *in45)
044490081003     c                             or (§PS1 < ($LimOF - c_Len1_inch)
044500081003     c                                     and *in45   and   NOT *in46)
044510081003     c                             or (§PS1 < ($LimOF + c_Len1_inch)
044520081003     c                                     and *in45   and   *in46))
044530080924     c                             and CurLineA4 < wMaxLine
044540080919     c                   leavesr
044550030515e   1c                   endif
044560030515      *
044570030515      * Stampa del box per dati da FIAR5
044580030515      * (se è già stata stampata almeno una riga di tale box)
044590060921if  1c                   if        $FIAR5 = *on  and  $First = *off
044600030911     c                   eval      §LO2   = §PS1 + 0.050
044610071008if  2c                   if        NOT *in52
044620030515     c                   write     A4prtAR53
044630071008x   2c                   else
044640071008     c                   eval      §PS2   = §LO2 - 0.500
044650071008     c                   write     A4prtAR54
044660071008e   2c                   endif
044670030515e   1c                   endif
044680041209      *
044690041209sel 1c                   select
044700040607      *
044710041209      * DDT KARNAK/TITANEDI/EDK
044720041209      * Stampa di logo, box, linee e testi fissi del DDT del mittente
044730040607      * (se si stà stampando tale DDT)
044740040607      * (ma NON per la parte "merce omaggio")
044750040607      * calcolando - prima - le posizioni iniziale e finale del box
044760041209w   1c                   when           $DDT_p    = *on
044770071008     c                             and  §AR8tsd   = 'K1'
044780040607     c                             and  $DDT_o    = *off
044790040608      * - Stampa parte finale DDT
044800040608if  2c                   if        $DDTlastPg = *off
044810041209      * - - Calcolo della posizione finale del box
044820040607     c                   eval      PCnv2  = '          '
044830040607     c                                    + '*** SEGUE ***'
044840040608     c                   exsr      Posiz_DK
044850040607     c                   write     A4prtDK4
044860040608x   2c                   else
044870041209      * - - Stampa dei dati in chiusura DDT (se ultima pagina)
044880040608      *     (la posizione finale del box verrà lì calcolata)
044890050217     c                   exsr      PrtDdtKARN_V2
044900040608e   2c                   endif
044910040607      * - Stampa logo, box, linee e testi fissi
044920081017     c                   if            §AR8cxd = *blanks
044930081017     c                             or  DB0fa4  = 'P'
044940050218     c                   write     A4prtDT0
044950080923     c                   endif
044960040607     c                   write     A4prtDK0
044970041019      *
044980050414      * DDT LOGISTICA/SAIT Abrasivi
044990050215      * Stampa di logo, box, linee e testi fissi del DDT del mittente
045000041019      * (se si stà stampando tale DDT)
045010041209w   1c                   when           $DDT_p    = *on
045020060627     c                             and  %subst(§AR8tsd:1:1) = 'L'
045030041021     c                   exsr      Posiz_DL
045040050217      * - - Chiusura box in fase di stampa dettaglio DDT
045050050217      *     (la stampa dei dati in chiusura DDT sarà solo nell'ultima
045060050318      *      pagina - prima dell'eventuale packing-list)
045070050318      *     La gestione del salto alla 1ª pagina della packing-list
045080050321      *     (AR8trk = 'Z1' & Page = *zeros) avviene nella subr.
045090050318      *     "PrtDdtLOG1_Z1" (vedi subr. "PrtPckLst_1").
045100081017     c                   if            §AR8cxd = *blanks
045110081017     c                             or  DB0fa4  = 'P'
045120050308     c                   write     A4prtDT0
045130050331     c                   endif
045140050318sel 2c                   select
045150050505      *     - si sta stampando il DDT Logistica o Sait
045160050318w   2c                   when      %subst(AR8trk:1:1) <> 'Z'
045170071008     c                             and %subst(AR8trk:1:1) <> 'S'
045180050218     c                   write     A4prtDL0A
045190050615      *     - si sta stampando la Packing-List
045200050318w   2c                   when      %subst(AR8trk:1:1) =  'Z'
045210050615     c                             and    Page        >  *zeros
045220050318     c                   write     A4prtDL0C
045230050606      *     - si sta stampando la Fattura (oltre 1ª pag.)
045240071008w   2c                   when      %subst(AR8trk:1:1) =  'S'
045250081003     c                             and    Page        >  *zeros
045260050609     c                   write     A4prtDL0E
045270050318e   2c                   endsl
045280050113      *
045290050113      * DDT ARCTE
045300050215      * Stampa di logo, box, linee e testi fissi del DDT del mittente
045310050113      * (se si stà stampando tale DDT)
045320050113w   1c                   when           $DDT_p    = *on
045330050113     c                             and  AR8tmd    = 'ARCT'
045340050114     c                   exsr      Posiz_AC
045350050217      * - - Chiusura box in fase di stampa dettaglio DDT
045360050217      *     (la stampa dei dati in chiusura DDT sarà solo nell'ultima
045370050217      *      pagina)
045380081017     c                   if            §AR8cxd = *blanks
045390081017     c                             or  DB0fa4  = 'P'
045400050331     c                   write     A4prtDT0
045410050331     c                   endif
045420050217     c                   write     A4prtAC0A
045430050215      *
045440050215      * DDT ESPRINET
045450050215      * Stampa di box, linee e testi fissi del DDT del mittente
045460050215      * (se si stà stampando tale DDT)
045470050215      * · Solo nella 1ª pagina vengono stampati il logo e la parte
045480050215      *        superiore del DDT.
045490050215      * · Solo nell'ultima pagina vene stampato il "fondo".
045500050215w   1c                   when           $DDT_p    = *on
045510060627     c                             and  §AR8tsd   = 'E1'
045520110405if 1bc                   if        %subst(SAVtrk:1:1 ) <> 'P'
045530110405     c                             or   %eof(FIAR801L)
045540050215      * - Stampa parte finale DDT
045550050215      * - - Calcolo delle posizioni di box & linee
045560050215     c                   exsr      Posiz_ES
045570050215      * - - Chiusura box in fase di stampa dettaglio DDT
045580050215      *     (la stampa dei dati in chiusura DDT sarà solo nell'ultima
045590050215      *      pagina)
045600050215if  2c***                if        %subst(AR8trk:1:1) <> 'P'
045610081017     c                   if            §AR8cxd = *blanks
045620081017     c                             or  DB0fa4  = 'P'
045630050331     c                   write     A4prtDT0
045640050331     c                   endif
045650050215     c                   write     A4prtES0A
045660050215e   2c***                endif
045670110405e  1bc                   endif
045680041209      *
045690041209      * DDT GENERICO con LOGO
045700050215      * Stampa di logo e box del DDT del mittente
045710041209      * (se si stà stampando tale DDT)
045720041209w   1c                   when           $DDT_p    = *on
045730041209     c                             and  §AR8tsd   = 'S2'
045740041209     *** * - Chiusura del box con i dati di mittente e destinatario
045750041209     *** *   (Se si stanno ancora stampando 'sti dati: bisogna calcolare
045760041209     *** *    la posizione di chiusura temporanea del box)
045770041209if  2***c                   if             AR8trk =  'B1'
045780041209     ***c                             or   AR8trk =  'B2'
045790041209     *** *   Impostazione della posizione finale per il box
045800041209     ***c                   exsr      Rep_§LO2
045810041209     ***c                   eval      §PB2   = §LO2 + 0.065
045820041209e   2***c                   endif
045830041209     *** *   Stampa del box
045840041209     ***c                   write     A4prtDB0
045850041209      * - Il logo è già stato SICURAMENTE stampato
045860041209     c***                exsr      Rep_§LO2
045870041209     c***                eval      §PB1   = §LO2
045880041209      *** Stampa del logo
045890041209     c***                write     A4prtDB0
045900041209      *
045910041209e   1c                   endsl
045920030515      *
045930030515      * Stampa dell'indicazione "Segue..." su pagina corrente
045940030515     c                   clear                   Pseg0
045950030515     c                   clear                   Pseg1
045960030515     c                   eval      Pseg9  = '  Segue...'
045970080919      /free
045980080923         select;
045990080923           when  *in45   and   NOT *in46;
046000080923             §PSA = 0.200;   // Per campo vuoto !
046010080923             §PSB = 0.200;   // Per campo vuoto !
046020080923             §PSC = c_Len2_inch - 0.050;
046030080923           when  *in45   and   *in46;
046040080923             §PSA = c_Len2_inch + 0.200;   // Per campo vuoto !
046050080923             §PSB = c_Len2_inch + 0.200;   // Per campo vuoto !
046060080924             §PSC = c_Len2_inch * 2 - 0.200;
046070080923           when  NOT *in45;
046080080923             §PSA = 0.400;
046090080923             §PSB = 0.700;
046100080923             §PSC = 8.750;
046110080923         endsl;
046120080919      /end-free
046130030515     c                   write     A4prtSEG
046140030515      *
046150031031      * Se sono ancora nella prima pagina =>
046160030515      * stampo anche i dati per il destinatario e box/barcode/ecc.
046170080604sel 1c                   select
046180080923      * > Se NON sto stampando un DDT del mittente
046190080922      *   (nel qual caso non stampo null'altro)
046200080923      *   => nulla
046210080919w   1c                   when      wPage  > 1
046220080923      * > Se sto stampando le sole copie per il vettore (4 in A4)
046230080923      *    o la relativa packing-list
046240080923      *   (nel qual caso non stampo null'altro)
046250080923      *   => nulla
046260081003w   1c                   when      DB0fa4 = '4'   or   DB0fa4 = 'P'
046270040607      * > Se NON è ristampa in arrivo per recapito c/assegni  e
046280040707      *   se NON sto stampando una LdV Poste
046290080703      *   se NON sto stampando (anche) un DDT del mittente
046300080703      *   se NON sto stampando le sole copie per il vettore (4 in A4)
046310080923      *   => posizionamento e stampa
046320120113w   1c                   when          $No_x_Cons =  *off
046330081013     c                             and ($FIAR8    =  *off)
046340081013     c***-=»>                       or  DB0fl1    =  'D')
046350080605      * - Posizionamento sul foglio A4 in base al n° pagina in stampa
046360130418     c                   eval      *in33 = *on
046370130418     c                   exsr      Posiz_Box
046380130418      * - Stampa 2ª parte della LdV (copia per il destinatario)
046390130418     c                   write     A4prtLDV
046400031031      * - Stampa "cornice" / barcode / righine varie in parte inferiore
046410130418     c                   write     A4prtBOX
046420110426     c*//                write     A4LogoBAR
046430110426      /free
046440120530         //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
046450120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
046460120530         //  // >?Simil-Logo Bartolini/Brt?
046470120530         //  write  A4LogoBR2;
046480120530         //  write  A4LogoDA2;
046490120530         //else;
046500120530         //  // >?Simil-Logo Bartolini?
046510120530         //  write  A4LogoBAR;
046520120530         //  write  A4LogoDA1;
046530120530         //endif;
046540120530         // >?Simil-Logo BRT?
046550120530         write  A4LogoBRT;
046560120530         write  A4LogoDA3;
046570110426      /end-free
046580120113      *//* > Se sto stampando una LdV Poste per recapito c/assegni
046590120113w   1c*//                   when          $No_x_Cons =  '2'
046600120113      *//* - Stampa 2ª parte "nulla" della LdV
046610130418     c*//                   eval      *in33 = *on
046620130418     c*//                   exsr      Posiz_Box
046630120113     c*//                   write     A4prtLDVB
046640080604e   1c                   endsl
046650080923      /free
046660110426         // -?Se richiesta la stampa della sola packing-list:?
046670110426         //  ?incremento del n° spedizione per calcolare correttam.?
046680110426         //  ?il n° di pagina iniziale per la spediz. successiva?
046690080925         if  DB0fa4 = 'P';
046700080925             wNumLdV += 1;
046710080925         endif;
046720080925
046730110426         // -?Reimpostazione degli indicatori per il posizionamento?
046740110426         //  ?all'interno del foglio A4?
046750080923         *in45 = (DB0fa4 = 'P');
046760080923         *in46 = (DB0fa4 = 'P'   and   %rem(wNumLdv : 2) = *zero);
046770080923      /end-free
046780030515      *
046790030515      * Salta a pagina successiva e indica "(Seguito)"
046800030515     c                   write     A4prtSP0
046810030515     c                   eval      wPage  = wPage + 1
046820080923      /free
046830081017         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
046840081017           write  A4prtNUL;
046850081017         endif;
046860081017
046870080923         IF  DB0fa4 <> 'P';
046880080922      /end-free
046890030515     c                   eval      Pseg0  = 'Spedizione '
046900110310     c                                    + %editc(DB0lnp : 'X') + ' '
046910110310     c                                    + %editc(DB0nrs : 'X') + ' '
046920110310     c                                    + %editc(DB0nsp : 'X')
046930030515     c                                    + ' del ' + %editc(Pdsp : 'Y')
046940030515     c                   eval      Pseg1  = '(Seguito)             Pag.'
046950030515     c                                    + %editc(wPage:'Z')
046960030515     c                   clear                   Pseg9
046970030515     c                   write     A4prtSEG
046980080922      /free
046990080923         ELSE;
047000081017
047010110426           // -?"Copia per il destinatario" già impostato in sr. "PrepPrt"?
047020081017           //if  §AR8cxd = *blank;
047030130726           //  Tcxd  = c_CopiaXdest;
047040130726           //  Tcxd2 = Tcxd;
047050081017           //endif;
047060081017
047070110426           // -?Estremi spedizione da impostare SOLO se si stampando?
047080110426           //  ?la sola packing-list (DB0FA4 = "P")?
047090081017           if  DB0fa4 = 'P';
047100101028             //Trsd = Prsd;   (già impostato nella subr. StampaPkgLst)?
047110081017             Trif = 'Spedizione '
047120081017                  + %editc(DB0lnp : 'X') + ' '
047130110310                  + %editc(DB0nrs : 'X') + ' '
047140110310                  + %editc(DB0nsp : 'X')
047150081017                  + ' del ' + %editc(Pdsp : 'Y')
047160081017                  + '        Pag.'
047170101028                  + %editc(wPage:'Z');
047180101028                //+ '        (Seguito)';
047190101028             //Tndc = Pndc;   (già impostato nella subr. StampaPkgLst)?
047200081017           endif;
047210080923
047220110426           // -?Verifica se la 2ª parte della packing-list sta nella?
047230110426           //  ?pag. attuale o se deve andare in quella successiva?
047240080923           wSeqLdV  = %rem( wNumLdV : $LdVxP );
047250080923           if  wSeqLdV = *zero   and   $LdVxP  = 2;
047260080923             wSeqLdV = 2;
047270080923           endif;
047280080923
047290110426           // -?Posizionamenti nella pagina in corso di stampa?
047300110426           //  ?(§POS = posizione verticale della costante "COPIA PER?
047310110426           //   ?IL DESTINATARIO" (con estremi spedizione e pag.)?
047320080923           wPosIni  = ($Len1LdV * (wSeqLdV - 1));
047330080924           if  wSeqLdV = 1;
047340080924             §Pos   = wPosIni + c_PosBox1As + 0.040;
047350090422             §Pos   = wPosIni + c_PosBox1As + 0.035;
047360080924           else;
047370080924             §Pos   = wPosIni + c_PosBox1As - 0.070;
047380080924           endif;
047390081017
047400081017           if  §AR8cxd = *blank   or   DB0fa4  = 'P';
047410081017             write A4prtDT0;
047420081017           endif;
047430080923
047440080923         ENDIF;
047450080922      /end-free
047460030515      *
047470030515      * Reimposta posizioni iniziali di stampa per linee
047480030515      *   (orizzontali-§LOn e verticali-§LVn) e campi (§PSn).
047490030515      *   Nelle pagine dalla 2ª in poi si parte da in alto:
047500030515      *   non vengono ri-stampati i 2 box "vettore" e "destinatario".
047510030515sel 1c                   select
047520120530      * - BRT
047530030515w   1c                   when      *in70
047540080711     c                   eval      §LO1   = c_§PS1
047550030515     c                   eval      §LO2   = §LO1
047560030515     c                   eval      §PS1   = §LO1
047570040607sel 2c                   select
047580050217      * - - con DDT del mittente KARNAK / TITANEDI / EDK
047590040607w   2c                   when           $DDT_p    = *on
047600071008     c                             and  §AR8tsd   = 'K1'
047610040608     c                   exsr      NewPageDK
047620050414      * - - con DDT del mittente LOGISTICA / SAIT ABRASIVI
047630041207w   2c                   when           $DDT_p    = *on
047640060627     c                             and  %subst(§AR8tsd:1:1) = 'L'
047650050321     c                   exsr      NewPageDL
047660050113      * - - con DDT del mittente ARCTE
047670050113w   2c                   when           $DDT_p    = *on
047680050113     c                             and  AR8tmd    = 'ARCT'
047690081002     c                   exsr      NewPageAC
047700050215      * - - con DDT del mittente ESPRINET
047710050215w   1c                   when           $DDT_p    = *on
047720060627     c                             and  §AR8tsd   = 'E1'
047730110405if  2c                   if        %subst(SAVtrk:1:1 ) <> 'P'
047740110405     c                             or   %eof(FIAR801L)
047750081002     c                   exsr      NewPageES
047760110405e   2c                   endif
047770041207      * - - con DDT generico (con marchio)
047780041207w   2c                   when           $DDT_p    = *on
047790041207     c                             and  §AR8tsd   = 'S2'
047800081017     c                   exsr      NewPageSTD
047810041207      * - - con DDT generico (senza marchio)
047820041207w   2c                   when           $DDT_p    = *on
047830041207     c*** ("scontato":)            and  §AR8tsd   = 'S1'
047840081017     c                   exsr      NewPageSTD
047850040607e   2c                   endsl
047860080922      * - DPD (ora in ex-A5)
047870080922w   1c***                when      *in71
047880030515      * - FedEx
047890030515w   1c                   when      *in72
047900030709     c                   eval      §LO1   = 1.600
047910030515     c                   eval      §LO2   = §LO1 + 0.425
047920030515     c                   eval      §LV1   = §LO1 + 0.300
047930030515     c                   eval      §LV2   = §LV1 + 0.070
047940030515     c                   write     A4prtFED1
047950030515do  2c                   dow       CurLineA4 < $MinLine1
047960030515     c                   write     A4prtNUL
047970030515e   2c                   enddo
047980030515     c                   write     A4prtFED2
047990030515     c                   eval      §LO1   = §LO2 - 0.250
048000030911     c                   eval      §PS1   = §LO1 + 0.150
048010040223      * - EuroExpress
048020040223w   1c                   when      *in73
048030030515      *
048040030515e   1c                   endsl
048050030515      *
048060030515      * N.B.: Per i SegnaColli non sequenziali non servono queste
048070030515      *       impostazioni: trattasi di stampa con salto riga -skipb-
048080030515      *       quindi sono già posizionato.
048090030515      *
048100080919     c                   ENDSR
048110030910      *
048120030910      *---------------------------------------------------------------*
048130050217      *?Reperimentoo della posizione (in pollici) in cui posizionarsi?*
048140050217      *?* per la stampa in base all'ultima riga stampata             ?*
048150030910      *---------------------------------------------------------------*
048160030910     c     Rep_§LO2      BEGSR
048170030910      *
048180030910sel 1c                   select
048190080825      *
048200080825      * Se ho stampato l'elenco dei segnacolli non sequenziali, devo
048210080825      *   "allineare" il posizionamento in pollici all'ultima riga
048220080825      *   stampata (in ottavi di pollice).
048230080825      *   =>   Num_ultima_riga_stampata / 8
048240080825w   1c                   when           DB0fns     = 'S'
048250080825     c                             and  DB0ncl     > 2
048260080825     c                             and  $A4_x_ScNS = *on
048270080825     c                             and  NOT  *in69
048280080825     c                   eval(h)   §LO2   = (CurLineA4 / 8) + 0,1
048290040607      *
048300040607      * Se ho stampato il DDT del mittente
048310080714      *   =>   Num_ultima_riga_stampata / 9
048320081013w   1c                   when           $FIAR8 =  *on
048330081013     c***-=»>                      and  DB0fl1 <> 'D'
048340080708     c                   eval(h)   §LO2   = (CurLineA4 / 9) + 0,1
048350030910      *
048360030910      * Se non ho stampato né elenco segnacolli non sequenziali né
048370040223      *   altri box, devo impostare comunque i dati per il posiziona-
048380030910      *   mento di questo primo box
048390030910w   1c                   when      §LO2   = *zeros
048400080624     c                   eval      §LO1   = 2.850
048410030910     c                   eval      §LO2   = §LO1
048420030910      *
048430030910e   1c                   endsl
048440030910      *
048450030910     c                   ENDSR
048460031124      *
048470031124      *---------------------------------------------------------------*
048480050217      *?Reperim. della posizione (in pollici) utlizzaile per la      ?*
048490080714      *?  stampa dei box in base all'ultima riga stampata in 1/9"    ?*
048500031124      *---------------------------------------------------------------*
048510031124     c     CalcRowPos    BEGSR
048520031124      *
048530031124      * Imposto le posizioni inziali per il primo dei box
048540080624     c                   select
048550080624      *
048560040607      * - in DDT del mittente: dopo eventuale elenco merce omaggio
048570120530      *   (come in LDV BRT)
048580120530      * - in LDV BRT: dopo eventuale elenco segnacolli non seq.
048590080624     c                   when      *in70
048600080624     c                   exsr      Rep_§LO2
048610080624      *
048620080922      * - in LDV DPD (ora in ex-A5)
048630080922     c***                when      *in71
048640080624      *
048650040607      * - in LDV FEDEX: posizione variabile
048660031124      *   (dipende dal numero dei segnacolli elencati: intanto
048670031124      *   posiziona la "base di partenza" sull'ultima posizione
048680031124      *   eventualmente usata per l'elenco dei segnacolli FedEx;
048690031124      *   poi si ri-calcoleranno in base al numero di righe stampate)
048700080624     c                   when      *in72
048710080624     c                   eval      §LO2   = §LO1 + 0.250
048720080624      *
048730080912      * - in LDV EUROEXPRESS: non più gestita
048740080624     c                   when      *in73
048750080912      *
048760080624     c                   endsl
048770080624      *
048780031124     c                   eval      §LO1   = §LO2
048790031124      *
048800031124     c                   ENDSR
048810060524      *
048820060524      *---------------------------------------------------------------*
048830060524      *?Stampa box per riferimento al                                ?*
048840060524      *?DOCUMENTO di ACCOMPAGNAMENTO SEMPLIFICATO                    ?*
048850060524      *---------------------------------------------------------------*
048860060524     c     StampaDAS     BEGSR
048870060524      *
048880060524     c***                eval      Par5   = '*...+....1....+....2....+-
048890060524     c***                          ....3....+....4....+....5....+....6-
048900060524     c***                          ....+....7....+....8....+....9....+-
048910060524     c***                          ....A'
048920060524      *
048930060524      * - stampo testata
048940090302     ***c                   eval      $First = 'B'
048950090302     ***c                   eval      Par5   = C_PMxA
048960090302      /free
048970090302         $First = *on;
048980090302         Par5   = '                    '
048990090302                + '                    '
049000090302                + '          '
049010090302                + C_PMxA;
049020090302      /end-free
049030060524     c                   exsr      StampaAR5
049040090302      /free
049050090302
049060090302         // - memorizza posizione iniziale per il box
049070090302         //   (§PB1 sarà da reimpostare in §LO1 PRIMA della stampa del box)
049080090302         §PB1   = §LO1;
049090090302
049100090302         // - stampa linea separatrice
049110090302         §LO1   = §PS1 + 0.050;
049120090302         write  A4prtLO;
049130090302         §PS1   = §LO1;
049140090302      /end-free
049150060524      *
049160060524      * - stampo 1° testo
049170060524     c                   eval      $First = '2'
049180060524     c                   eval      Par5   = 'CONSEGNARE AL DESTINATARI-
049190060524     c                             O D.D.T. E D.A.S. (Documento di Acc-
049200060524     c                             ompagnamento Semplificato).'
049210060524     c                   exsr      StampaAR5
049220060524      *
049230060524      * - stampo 2° testo
049240061030     c*** (già fatto:)   eval      $First = *off
049250060524     c                   eval      Par5   = 'In caso di RIFIUTO compil-
049260060524     c                             are l''ultima pagina del D.A.S. ind-
049270060524     c                             icando:'
049280060524     c                   exsr      StampaAR5
049290060524     c                   eval      Par5   = 'a) nome Autotrasportatore-
049300060524     c                              nel campo "IL SOTTOSCRITTO ... INC-
049310060524     c                             ARICATO DEL TRASPORTO"'
049320060524     c                   exsr      StampaAR5
049330060524     c                   eval      Par5   = 'b) firma Autotrasportator-
049340060524     c                             e nel campo "FIRMA INCARICATO DEL T-
049350060524     c                             RASPORTO"'
049360060524     c                   exsr      StampaAR5
049370060524     c                   eval      Par5   = 'c) causale del rifiuto (b-
049380060524     c                             arrare "la merce non è conforme all-
049390060524     c                             ''ordine" o indicare altra motivazi-
049400060524     c                             one)'
049410060524     c                   exsr      StampaAR5
049420060524     c                   eval      Par5   = 'd) firma del destinatario-
049430060524     c                              nel campo "FIRMA DEL DESTINATARIO"'
049440060524     c                   exsr      StampaAR5
049450090302     *** *
049460090302     *** * - stampo box
049470090302     ***c                   eval      §LO2   = §PS1 + 0.050
049480090302     ***c                   write     A4prtAR53
049490090302      /free
049500090302         // - stampa box
049510090302         //   (§PB1 è da reimpostare in §LO1: contiene il suo valore PRIMA
049520090302         //   della stampa delle varie linee separatrici)
049530090302         §LO1   = §PB1;
049540090302         §LO2   = §PS1 + 0.070;
049550090302         write  A4prtAR53;
049560090302      /end-free
049570060524      *
049580060524     c                   ENDSR
049590031124      *
049600031124      *---------------------------------------------------------------*
049610050217      *?Prepara elenco per box "Promemoria per autotrasportatore"    ?*
049620031124      *---------------------------------------------------------------*
049630031124     c     ListaPMxA     BEGSR
049640031124      *
049650070308      * - stampo testata
049660070316     c                   eval      $First = *on
049670070319     c                   eval      Par5   = '                    '
049680070319     c                                    + '                    '
049690070319     c                                    + '          '
049700070319     c                                    + C_PMxA
049710031124     c                   exsr      StampaAR5
049720070315      *
049730070315      * - memorizzo posizione iniziale per il box
049740070320      *   (§PB1 sarà da reimpostare in §LO1 PRIMA della stampa del box)
049750070315     c                   eval      §PB1   = §LO1
049760031124      *
049770070308      * - stampo testo
049780081127      *
049790081212      /free
049800110411         // >?Mittente cliente in tab. "PXA"?
049810090302         IF  $TabPXA = *on;
049820090302
049830170517           clear  k05tntbe01;
049840090302           k_TBEcod = 'PXA';
049850090302           k_TBEke1 = %editc(DB0ccm : 'X');
049860100316           k_TBEke2 = '001            ';
049870090302           clear  k_TBElin;
049880090302           clear  k_TBEsif;
049890100316           //setll  %kds(k05tntbe01 : 3)  TNTBE000;  (già fatto)
049900090302           //reade  %kds(k05tntbe01 : 2)  TNTBE000;  (già fatto)
049910090302           clear  wx;
049920090302
049930100316           DoW  not  %eof(TNTBE01L)  and  TBEke2 <= '006            ';
049940090302
049950090302             if  TBEatb = *blank;
049960090302
049970110411               // -?Stampa della riga separatrice iniziale?
049980090302               if  wx = *zero;
049990090302                 §LO1   = §PS1 + 0.050;
050000090302                 write  A4prtLO;
050010090302                 §PS1   = §LO1;
050020090302                 $First = 'G';
050030090302               endif;
050040090302
050050110411               // -?Salta eventuali righe vuote?
050060090302               dow  %int( %trimr( TBEke2 ) ) > wx + 1;
050070090302                 clear  Par5;
050080090302                 exsr  StampaAR5;
050090090302                 wx += 1;
050100090302               enddo;
050110090302
050120110411               // -?Stampa la singola riga di testo?
050130090302               Par5   = TBEuni;
050140090302               exsr  StampaAR5;
050150090302               wx += 1;
050160090302
050170090302             endif;
050180090302
050190090302             reade  %kds(k05tntbe01 : 2)  TNTBE000;
050200090302
050210090302           EndDo;
050220090302
050230090302         ENDIF;
050240110411
050250110411         // >?Particolarità consegna in tab. "PXA"?
050260110411         IF  $TabPXA2 = *on;
050270110411
050280170517           clear  k05tntbe01;
050290110411           k_TBEcod = 'PXA';
050300110411           k_TBEke1 = DB0gma;
050310110411           k_TBEke2 = '001            ';
050320110411           clear  k_TBElin;
050330110411           clear  k_TBEsif;
050340110411           setll  %kds(k05tntbe01 : 3)  TNTBE000;
050350110411           reade  %kds(k05tntbe01 : 2)  TNTBE000;
050360110411           clear  wx;
050370110411
050380110411           DoW  not  %eof(TNTBE01L)  and  TBEke2 <= '006            ';
050390110411
050400110411             if  TBEatb = *blank;
050410110411
050420110411               // -?Stampa della riga separatrice iniziale?
050430110411               if  wx = *zero;
050440110411                 §LO1   = §PS1 + 0.050;
050450110411                 write  A4prtLO;
050460110411                 §PS1   = §LO1;
050470110411                 $First = 'G';
050480110411               endif;
050490110411
050500110411               // -?Salta eventuali righe vuote?
050510110411               dow  %int( %trimr( TBEke2 ) ) > wx + 1;
050520110411                 clear  Par5;
050530110411                 exsr  StampaAR5;
050540110411                 wx += 1;
050550110411               enddo;
050560110411
050570110411               // -?Stampa la singola riga di testo?
050580110411               Par5   = TBEuni;
050590110411               exsr  StampaAR5;
050600110411               wx += 1;
050610110411
050620110411             endif;
050630110411
050640110411             reade  %kds(k05tntbe01 : 2)  TNTBE000;
050650110411
050660110411           EndDo;
050670110411
050680110411         ENDIF;
050690090302
050700081212      /end-free
050710081127      *
050720031216sel 1c                   select
050730070306      * >
050740070306x   1c***                other
050750070306     c***                eval      $First = *off
050760070306     c***                clear                   Par5
050770070306     c***                exsr      StampaAR5
050780070306      *
050790070306e   1c                   endsl
050800061102      *
050810071114      * > Raccogliere Codice Fiscale e/o Partita Iva di ...
050820071114if  1c                   if             $No_CF = *on
050830071114     c                             or   $No_PI = *on
050840070320     c                   eval      §LO1   = §PS1 + 0.050
050850070320     c                   write     A4prtLO
050860070320     c                   eval      §PS1   = §LO1
050870070321     c                   eval      $First = 'G'
050880061102     c                   eval      Par5   = 'Spedizione n°'
050890061102     c                                    + %editw(DB0lnp:'0   ')
050900061102     c                                    + %editw(DB0nrs:'0  ')
050910061102     c                                    + ' '
050920061102     c                                    + %editc(DB0nsp:'Z')
050930061102     c                                    + ' del '
050940061102     c                                    + %editc(Pdsp:'Y')
050950061102     c                   exsr      StampaAR5
050960071114if  2c                   if        $No_PI = *on
050970071114     c*** (già fatto:)   eval      $First = 'G'
050980071114     c                   eval      Par5   = 'raccogliere +
050990071114     c                                      Partita I.v.a. di "'
051000071114     c                                    + %trim(wRSxCF)
051010071114     c                                    + '"'
051020071114     c                   exsr      StampaAR5
051030120504if  3c                   if        $No_CF = *off
051040120504     c*** (già fatto:)   eval      $First = 'G'
051050120504     c                   eval      Par5   = 'Codice Fiscale di "'
051060120504     c                                    + %trim(wRSxCF)
051070120504     c                                    + '": '
051080120504     c                                    + wB0cfi
051090120504     c                   exsr      StampaAR5
051100120504e   3c                   endif
051110071114e   2c                   endif
051120120503if  2c                   if        $No_CF = *on
051130120503     c*** (già fatto:)   eval      $First = 'G'
051140120503     c                   eval      Par5   = 'raccogliere +
051150120503     c                                      Codice Fiscale di "'
051160120503     c                                    + %trim(wRSxCF)
051170120503     c                                    + '"'
051180120503     c                   exsr      StampaAR5
051190120503e   2c                   endif
051200070306e   1c                   endif
051210070306      *
051220070306      * > Note bolla (dal file FIARN00F)
051230070306      *   (posizionamento e 1ª lettura già effettuate nella subr.
051240070306      *    "CheckFIARN")
051250070306if  1c                   if        $FIARN = *on
051260070320     c                   eval      §LO1   = §PS1 + 0.050
051270070320     c                   write     A4prtLO
051280070320     c                   eval      §PS1   = §LO1
051290070321     c                   eval      $First = 'G'
051300070319do  2c                   dou       %eof(FIARN11L)
051310070306     c                   eval      Par5   = ARNnob
051320070306     c                   exsr      StampaAR5
051330070319     c     K06ARN11      reade     FIARN000
051340070306e   2c                   enddo
051350070306e   1c                   endif
051360070319      *
051370070319      * > Consegne parziali precedenti (dal file FNART00F)
051380070406if  1c                   IF        $ConsParz     =  *on
051390101130     c*//(già fatto)?   exsr      SchConsParz
051400070320     c                   eval      §LO1   = §PS1 + 0.050
051410070320     c                   write     A4prtLO
051420070320     c                   eval      §PS1   = §LO1
051430070321     c                   eval      $First = 'G'
051440070319     c                   eval      xx     = 1
051450070319do  2c                   DoW            (xx      <= %elem($CP)
051460070319     c                             and  $CP(xx)  <  *all'9')
051470070319     c                   movel     $CP(xx)       Elem_CP
051480070319     c                   z-add     WCPdcm        w0080
051490070319     c     *iso          test(d)                 w0080                  31
051500070319if  3c                   if        NOT *in31
051510071025     c     *iso          movel     w0080         Data_EUR
051520071025     c     *eur          movel     Data_EUR      w0080
051530070319x   3c                   else
051540070319     c                   clear                   w0080
051550070319e   3c                   endif
051560140304 *...+*...1....+....2....+....3....+....4....+....5....+....6....+....7....+..
051570140305 Conse*nati colli 12345 in data 12/34/5678
051580140304 Conse*nati colli 12345 in data 12/34/5678 alle ore 12:34 - Firmatario: 123456
051590140304 Conse*nati colli 12345 il 12/34/5678 alle 12:34 a 1234567890123456789012345
051600140305 Conse*nati colli 12345 in data 12/34/5678 alle ore 12:34
051610140305      *                 Firmatario: 123456789012345678901234567890123456789012
051620070321     c                   eval      Par5   = 'Consegnati colli '
051630140305     c                                    + %editc(WCPncl:'Z')
051640140305     c                                    + ' in data '
051650070319     c                                    + %editc(w0080:'Y')
051660140304     c                   if        WCPocm > *zero
051670140304     c                   eval      Par5   = %trimr( Par5 )
051680140305     c                                    + ' alle ore '
051690140303     c                                    + %editw(WCPocm : '0 :  ')
051700140304     c                   endif
051710070319     c                   exsr      StampaAR5
051720140305     c                   if        WCPfir <> *blank
051730140305     c                   clear                   Par5
051740140305     c                   eval      %subst( Par5 : 24 ) = 'Firmatario: '
051750140305     c                                    + WCPfir
051760140305     c                   exsr      StampaAR5
051770140305     c                   endif
051780070319     c                   eval      xx     = xx + 1
051790070319e   2c                   EndDo
051800070319e   1c                   ENDIF
051810070321      *
051820070321      * > Reperita Comunicazione Anomalia
051830070321if  1c                   if        $C_A_  = *on
051840070321     c                   eval      §LO1   = §PS1 + 0.050
051850070321     c                   write     A4prtLO
051860070321     c                   eval      §PS1   = §LO1
051870070321     c                   eval      $First = 'G'
051880070321     c                   eval      Par5   = 'Presente Comunicazione +
051890070321     c                                                Anomalia'
051900070321     c                   exsr      StampaAR5
051910070321e   1c                   endif
051920031124      *
051930031124      * - stampo box
051940070320      *   (§PB1 è da reimpostare in §LO1: contiene il suo valore PRIMA
051950070320      *   della stampa delle varie linee separatrici)
051960070315     c                   eval      §LO1   = §PB1
051970070315     c                   eval      §LO2   = §PS1 + 0.070
051980031124     c                   write     A4prtAR53
051990031124      *
052000031124     c                   ENDSR
052010090924      /free
052020090924
052030090924       //--------------------------------------------------------------
052040090924       //?Prepara elenco per box "Note per il destinatario"            ?
052050090924       //--------------------------------------------------------------
052060090924       BEGSR  StampaNxD;
052070090924
052080090924         // - Stampa testata
052090090924         //$First = *on;
052100090924         //Par5   = '                    '
052110090924         //       + '                    '
052120090924         //       + '          '
052130090924         //       + c_NxD;
052140090924         //exsr  StampaAR5;
052150090924
052160090924         // - Memorizzazione posizione iniziale per il box
052170090924         //   (§PB1 sarà da reimpostare in §LO1 PRIMA della stampa del box)
052180090924         //§PB1 = §LO1;
052190090924
052200090924         // - Stampa della riga separatrice iniziale
052210090924         //§LO1   = §PS1 + 0.050;
052220090924         //write  A4prtLO;
052230090924         //§PS1   = §LO1;
052240090924
052250090924         // - Stampa la singola riga di testo
052260090925         $First = '1';
052270090924         Par5 = 'DECODER DIGITALE TERRESTRE OMAGGIO DA SKY PER AVER +
052280090924                 ADERITO ALL''OFFERTA';
052290090924         exsr  StampaAR5;
052300090924
052310090924         // - Memorizzazione posizione iniziale per il box
052320090924         //   (§PB1 sarà da reimpostare in §LO1 PRIMA della stampa del box)
052330090924         §PB1 = §LO1;
052340090924
052350090924         // - Stampa box
052360090924         //   (§PB1 è da reimpostare in §LO1: contiene il suo valore PRIMA
052370090924         //   della stampa delle varie linee separatrici)
052380090924         §LO1 = §PB1;
052390090925         §LO2 = §PS1 + 0.050;
052400090924         write  A4prtAR53;
052410090924
052420090924       ENDSR;
052430090924
052440090924      /end-free
052450030515      *
052460030515      *---------------------------------------------------------------*
052470050217      *?Prepara elenco per box - "Bancali da rendere al mittente"    ?*
052480150224      *?                         » NON PIÙ «                         ?*
052490050217      *?                       - "Dati per destinatario supermercato"?*
052500150224      *?                         » NON PIÙ «                         ?*
052510050217      *?                       - "Promemoria per autotrasportatore"  ?*
052520030515      *---------------------------------------------------------------*
052530030515     c     ListaAR5      BEGSR
052540030515      *
052550060921     c                   eval      $FIAR5 = *on
052560030515      *
052570030515      * Ripulisco i campi utilizzati in seguito
052580030515     c                   clear                   AR5trd
052590030515     c                   clear                   AR5dac
052600150304     c*//                clear                   dAR5ban
052610030717     c                   clear                   dAR5bnb
052620030903     c                   clear                   dAR5rcc
052630030515     c                   clear                   dAR5scr
052640030515     c                   reset                   $First
052650030515     c                   reset                   $FirstSCR
052660030515     c                   reset                   SAVtrd
052670030515      *
052680030515      * Posizionamento iniziale
052690040301if  1c                   if        DUTlpo = 'S'
052700040531     c     KLDV          setgt     FIAR531C
052710040301x   1c                   else
052720040607     c     KLDV          setgt     FIAR5000
052730040301e   1c                   endif
052740030515      *
052750030515      * Ciclo di lettura del file FIAR5
052760030515      *
052770030515do  1c                   DO        *Hival
052780030515      *
052790040607if  2c                   if        DUTlpo = 'S'
052800040531     c     KLDV          readpe    FIAR531C
052810040607x   2c                   else
052820040607     c     KLDV          readpe    FIAR5000
052830040607e   2c                   endif
052840040301      *
052850040301if  2c                   if           (DUTlpo =  'S'
052860040531     c                             and %eof(FIAR531C)
052870040301     c                             OR  DUTlpo <> 'S'
052880040301     c                             and %eof(FIAR501L))
052890030515     c                   leave
052900030515e   2c                   endif
052910030515if  2c                   if        AR5atb <> *blanks
052920030515     c                   iter
052930030515e   2c                   endif
052940030515      *
052950030515      * Controllo se devo stampare l'intestazione al nuovo box
052960030515if  2c                   if        AR5trd <> SAVtrd
052970030515      *   Stampa del precedente box (se stampata almeno una riga)
052980030910if  3c                   if             SAVtrd <> *hival
052990030910     c                             and  $First =  *off
053000030911     c                   eval      §LO2   = §PS1 + 0.050
053010030515     c                   write     A4prtAR53
053020030515e   3c                   endif
053030030515     c                   eval      $First =  *on
053040030515     c                   eval      SAVtrd =  AR5trd
053050030515e   2c                   endif
053060030515      *
053070150224sel 2c                   SELECT
053080150224      *//*
053090150224      *//* > Bancali da rendere al mittente
053100150224      *//*   (unico record, sequenzialmente prima dei rec. "SCR")
053110150224w   2c*//                WHEN      %subst(DB0gva:1:1) = 'B'
053120150224     c*//                          and    AR5trd      = 'BAN'
053130150224     c*//                movel     AR5uni        dAR5ban
053140150224      *//* - stampo testata
053150150224if  3c*//                if           (§AR5tba <> *zeros
053160150224     c*//                          or  §AR5tb2 <> *zeros)
053170150224     c*//                eval      Par5   = 'BANCALI DA RIPORTARE A MAGAZZINO E'
053180150224     c*//                                 + ' DA RENDERE AL MITTENTE           '
053190150224     c*//                exsr      StampaAR5
053200150224e   3c*//                endif
053210150224      *//* - stampo 1ª riga
053220150224if  3c*//                if        §AR5tba <> *zeros
053230150224     c*//                clear                   Par5
053240150224     c*//                clear                   Wtba
053250150224     c*//                eval      Wtba   = §AR5tba
053260150224     c*//                exsr      Tab_TBA
053270150224     c*//                eval      Par5   = §TBAdesc + 'N. '
053280150224     c*//                                 + %editc(§AR5nba : 'Z')
053290150224     c*//                exsr      StampaAR5
053300150224e   3c*//                endif
053310150224      *//* - stampo 2ª riga
053320150224if  3c*//                if        §AR5tb2 <> *zeros
053330150224     c*//                clear                   Par5
053340150224     c*//                clear                   Wtba
053350150224     c*//                eval      Wtba   = §AR5tb2
053360150224     c*//                exsr      Tab_TBA
053370150224     c*//                eval      Par5   = §TBAdesc + 'N. '
053380150224     c*//                                 + %editc(§AR5nb2 : 'Z')
053390150224     c*//                exsr      StampaAR5
053400150224e   3c*//                endif
053410150224      *//*
053420150224      *//* > Bancali da riportare a magazzino ("Oltre 20 colli")
053430150224      *//*   (unico record, sequenzialmente prima dei rec. "SCR",
053440150224      *//*    alternativo a "BAN" = bancali da rendere al mittente).
053450150224w   2c*//                WHEN      %subst(DB0gva:1:1) = 'O'
053460150224     c*//                          and    AR5trd      = 'BNB'
053470150224     c*//                movel     AR5uni        dAR5bnb
053480150224      *//* - stampo testata
053490150224if  3c*//                if           (§AR5btba <> *zeros
053500150224     c*//                          or  §AR5btb2 <> *zeros)
053510150224     c*//                eval      Par5   = 'BANCALI DA RIPORTARE A MAGAZZINO  '
053520150224     c*//                exsr      StampaAR5
053530150224e   3c*//                endif
053540150224      *//* - stampo 1ª riga
053550150224if  3c*//                if        §AR5btba <> *zeros
053560150224     c*//                clear                   Par5
053570150224     c*//                clear                   Wtba
053580150224     c*//                eval      Wtba   = §AR5btba
053590150224     c*//                exsr      Tab_TBA
053600150224     c*//                eval      Par5   = §TBAdesc + 'N. '
053610150224     c*//                                 + %editc(§AR5bnba : 'Z')
053620150224     c*//                exsr      StampaAR5
053630150224e   3c*//                endif
053640150224      *//* - stampo 2ª riga
053650150224if  3c*//                if        §AR5btb2 <> *zeros
053660150224     c*//                clear                   Par5
053670150224     c*//                clear                   Wtba
053680150224     c*//                eval      Wtba   = §AR5btb2
053690150224     c*//                exsr      Tab_TBA
053700150224     c*//                eval      Par5   = §TBAdesc + 'N. '
053710150224     c*//                                 + %editc(§AR5bnb2 : 'Z')
053720150224     c*//                exsr      StampaAR5
053730150224e   3c*//                endif
053740150224      *//* - stampo 3ª riga
053750150224if  3c*//                if        §AR5bcor <> *zeros
053760150224     c*//                clear                   Par5
053770150224     c*//                eval      Par5   = C_CoOr
053780150224     c*//                                 + %editc(§AR5bcor : 'K')
053790150224     c*//                exsr      StampaAR5
053800150224e   3c*//                endif
053810030515      *
053820030515      * > Destinatario supermercato
053830030515w   2c                   WHEN      (DB0tc1 = 'S' or DB0tc2 = 'S')
053840030515     c                             and AR5trd  = 'SCR'
053850030515     c                   movel     AR5uni        dAR5scr
053860030515      * - stampo testata
053870030515      *   (se subr. richiamata solo per verifica esistenza dati: esco)
053880030515if  3c                   if        $First =  *on
053890031124     c                   eval      Par5   = C_DeSM
053900030515     c                   exsr      StampaAR5
053910030515     c                   eval      $FirstSCR  = *off
053920030515e   3c                   endif
053930030515      * - stampo righe
053940030515     c                   clear                   Par5
053950030624     c                   eval      *in38 = *on
053960030515if  3c                   if           AR5dac <> SAVdac
053970030515     c                             or AR5orc <> SAVorc
053980030515     c                   eval      SAVdac = AR5dac
053990030515     c                   eval      SAVorc = AR5orc
054000030515      * - - giro data modifica
054010030515     c                   z-add     AR5dac        w0080
054020070315     c     *iso          test(d)                 w0080                  31
054030070315if  4c                   if        NOT *in31
054040071025     c     *iso          movel     w0080         Data_EUR
054050071025     c     *eur          movel     Data_EUR      w0080
054060070315x   4c                   else
054070070315     c                   clear                   w0080
054080070315e   4c                   endif
054090030515     c                   eval      Par5 = 'Modifica del '
054100030515     c                                  + %editc(w0080 : 'Y')
054110030515     c                                  + ' '
054120030515     c                                  + %editw(AR5orc : '  :  :  ')
054130030515     c                                  + ' '
054140040223     c                                  + §AR5Spru
054150030515     c                   exsr      StampaAR5
054160030515e   3c                   endif
054170040223     c                   eval      Par5 = §AR5Snom
054180030911     c                                  + '   Tel.: '
054190040223     c                                  + §AR5Stel
054200030911     c                   exsr      StampaAR5
054210040223     c                   eval      Par5 = §AR5Smot
054220030515     c                   exsr      StampaAR5
054230030624     c                   eval      *in38 = *off
054240031124      *
054250031124      * > Se destinatario assente inserire il plico in buca lettere
054260031124      *   (già stampato in box "Promemoria per autotrasportatore")
054270031124w   2c***                WHEN          DB0gma  = Ktab7V_CB
054280031124     c***                ...       ...
054290030903      *
054300030903      * > O.R.M. per ritiro contestuale alla consegna
054310030903      *   (unico record)
054320050510      *   Verrà stampato come ultimo box
054330050510      *   - prima del (eventuale) DDT del mittente
054340050510      *   - prima della copia per il destinatario
054350030910w   2c***                WHEN          DB0gma  = Ktab7V_RC
054360030910     c***                          and AR5trd  = 'RCC'
054370030903     c***                ...       ...
054380030515      *
054390030515e   2c                   ENDSL
054400030515      *
054410030515e   1c                   ENDDO
054420030515      *
054430030515      * Stampa del (ultimo) box (se stampata almeno una riga)
054440030515if  1c                   if        $First = *off
054450030911     c                   eval      §LO2   = §PS1 + 0.050
054460030515     c                   write     A4prtAR53
054470030515e   1c                   endif
054480030515      *
054490030515      * > Destinatario supermercato
054500030515      *   (stampa del box vuoto per memo se non già stampata almeno
054510030515      *   una riga)
054520030515if  1c                   IF        (DB0tc1 = 'S' or DB0tc2 = 'S')
054530030515     c                             and              $FirstSCR = *on
054540030515      * - stampo testata
054550030515     c                   eval      $First = *on
054560031124     c                   eval      Par5   = C_DeSM
054570030515     c                   exsr      StampaAR5
054580030515     c                   eval      $FirstSCR  = *off
054590030515      * - stampo 2 righe vuote
054600030515     c                   clear                   Par5
054610030515do  2c                   do        2
054620030515     c                   exsr      StampaAR5
054630030515e   2c                   enddo
054640030515      * - stampo box
054650030911     c                   eval      §LO2   = §PS1 + 0.050
054660030515     c                   write     A4prtAR53
054670030515e   1c                   ENDIF
054680070123      *
054690070123     c                   eval      $FIAR5 = *off
054700030515      *
054710030515     c                   ENDSR
054720030515      *
054730030515      *---------------------------------------------------------------*
054740050217      *?Stampa singola riga relativa a FIAR5                         ?*
054750030515      *---------------------------------------------------------------*
054760030515     c     StampaAR5     BEGSR
054770030515      *
054780030515      * Controllo se superata l'ultima riga
054790030515     c                   exsr      Test_OF
054800030515      *
054810030515      * Stampa del singolo record
054820030909sel 1c                   select
054830030909      *
054840030909w   1c                   when      $First = *off
054850030909      * - Incremento parametri per posizionamento nuova riga
054860030911     c                   eval      §PS1   = §PS1 + 0.150
054870030909     c                   write     A4prtAR52
054880030909      *
054890030909w   1c                   when      $First = *on
054900030515      * - Incremento parametri per posizionamento nuovo box
054910030515      *   e relativa intestazione
054920030911     c                   eval      §LO1   = §LO2 + 0.150
054930030911     c                   eval      §PS1   = §LO1 + 0.150
054940030515     c                   write     A4prtAR51
054950030515     c                   eval      $First = *off
054960060524      *
054970060524w   1c                   when      $First = '2'
054980060524      * - Incremento parametri per posizionamento nuova riga
054990060524      *   nel box "Promemoria per autotrasportatore"
055000060524     c                   eval      §PS1   = §PS1 + 0.150
055010060524     c                   write     A4prtAR51
055020060524     c                   eval      $First = *off
055030030909      *
055040030909w   1c                   when      $First = 'A'
055050030909      * - Incremento parametri per posizionamento nuovo box
055060030909      *   e relativa intestazione
055070030911     c                   eval      §LO1   = §LO2 + 0.150
055080030909     c                   eval      §PS1   = §LO1 + 0.200
055090030909     c                   write     A4prtAR5a
055100030909     c                   eval      $First = *off
055110150226      *//*
055120150226w   1c*//                when      $First = 'B'
055130150226      *//* - Incremento parametri per posizionamento nuovo box
055140150226      *//*   ed intestazione "Promemoria per autotrasportatore"
055150150226     c*//                eval      §LO1   = §LO2 + 0.150
055160150226     c*//                eval      §PS1   = §LO1 + 0.125
055170150226     c*//                write     A4prtAR5b
055180150226     c*//                eval      $First = 'C'
055190150226      *//*
055200150226w   1c*//                when      $First = 'C'
055210150226      *//* - Incremento parametri per posizionamento nuova riga
055220150226      *//*   nel box "Promemoria per autotrasportatore"
055230150226     c*//                eval      §PS1   = §PS1 + 0.250
055240150226     c*//                write     A4prtAR5c
055250070321      *
055260070321w   1c                   when      $First = 'G'
055270070321      * - Incremento parametri per posizionamento nuova riga
055280070321      *   nel box "Promemoria per autotrasportatore"
055290070321     c                   eval      §PS1   = §PS1 + 0.170
055300070321     c                   write     A4prtAR5g
055310030909      *
055320030909e   1c                   endsl
055330030515      *
055340030515     c                   ENDSR
055350030910      *
055360030910      *---------------------------------------------------------------*
055370050217      *?Stampa box "Ritiro Contestuale alla Consegna" da FIAR5/RCC   ?*
055380030910      *---------------------------------------------------------------*
055390030910     c     StampaRCC     BEGSR
055400030910      *
055410030910      * > O.R.M. per ritiro contestuale alla consegna
055420030910      *   (unico record)
055430031124      *   Verrà stampato come ultimo box (prima della copia per il
055440031124      *     destinatario)
055450030910      *
055460030910     c                   clear                   dAR5rcc
055470030910      *
055480030910     c                   eval      AR5trd = 'RCC'
055490040301if  1c                   if        DUTlpo = 'S'
055500040531     c     K05AR501      chain     FIAR531C
055510040301x   1c                   else
055520030910     c     K05AR501      chain     FIAR5000
055530040301e   1c                   endif
055540040301      *
055550071008if  1c                   if           (DUTlpo =  'S'
055560040531     c                             and %found(FIAR531C)
055570040301     c                             OR  DUTlpo <> 'S'
055580040301     c                             and %found(FIAR501L))
055590040301     c                             and AR5atb =  *blanks
055600030910     c                   movel     AR5uni        dAR5rcc
055610071015     c                   eval      ORMpoe = §AR5Rpoe
055620071015     c                   eval      ORMnsr = §AR5Rnsr
055630071015     c                   eval      ORMnor = §AR5Rnor
055640071015     c                   eval      ORMnrv = §AR5Rnrv
055650071015     c                   eval      Pbt2a  = 'RITIRO CONTESTUALE ALLA +
055660071015     c                                       CONSEGNA'
055670071015     c                   exsr      StampaORM
055680071015e   1c                   endif
055690071015      *
055700071015     c                   ENDSR
055710071015      *
055720071015      *---------------------------------------------------------------*
055730071015      *?Stampa box "Ritiro Porto Franco PrePagato"                   ?*
055740071015      *---------------------------------------------------------------*
055750071015     c     StampaRPFPP   BEGSR
055760071015      *
055770071015      * > O.R.M. per ritiro porto franco prepagato
055780071015      *   Verrà stampato come ultimo box (prima della copia per il
055790071015      *     destinatario)
055800071015      *
055810071015     c*** già fatto:     clear                   dsBL4M
055820071015     c*** già fatto:     exsr      RepAR4_M
055830071015      *
055840071015if  1c                   if             §B4poe  > *zeros
055850071015     c                             and  §B4nor  > *zeros
055860071015     c                   eval      ORMpoe = §B4poe
055870071015     c                   eval      ORMnsr = §B4nsr
055880071015     c                   eval      ORMnor = §B4nor
055890071015     c                   eval      ORMnrv = §B4nrv
055900071015     c                   eval      Pbt2a  = 'RITIRO PORTO FRANCO +
055910071015     c                                       PREPAGATO'
055920071015     c                   exsr      StampaORM
055930071015e   1c                   endif
055940071015      *
055950071015     c                   ENDSR
055960071015      *
055970071015      *---------------------------------------------------------------*
055980071015      *?Stampa dati O.R.M. in box "Ritiro Contestuale alla Consegna" ?*
055990071015      *?              o in quello "Ritiro Porto Franco Prepagato"    ?*
056000071015      *---------------------------------------------------------------*
056010071015     c     StampaORM     BEGSR
056020071015      *
056030071015     c                   eval      $FIAR5 = *on
056040071015      *
056050071015if  2c                   if        not %open(FNORM01L)
056060071015     c                   open      FNORM01L
056070071015e   2c                   endif
056080071015     c     K04ORM01      chain     FNORM000
056090071015      *
056100071015if  2c                   IF        %found(FNORM01L)
056110071015      *
056120071015      * Reperisco la posizione (in pollici) in cui posizionarmi
056130071015      *   per la stampa in base all'ultima riga stampata
056140071015      *   se non ho ancora stampato box
056150071015cas 3c     §LO2          caseq     *zeros        Rep_§LO2
056160071015e   3c                   endcs
056170071025      *
056180071015      * - stampo testata
056190071025      *   (con il n° O.R.M.)
056200071015     c                   eval      $First = 'A'
056210071015     c*** già impostato: eval      Pbt2a  = 'RITIRO CONTESTUALE ALLA +
056220071015     c***  "      "                          CONSEGNA'
056230071015     c                   eval      Pbt2b  = 'O.R.M. numero '
056240071015     c                                    + %editw(ORMpoe : '0   ')
056250071015     c                                    + ' '
056260071015     c                                    + %editw(ORMnsr : '0  ')
056270071015     c                                    + '  '
056280071015     c                                    + %editc(ORMnor : 'Z')
056290071015     c                                    + ' '
056300071015     c                                    + %editw(ORMnrv : '0  ')
056310071015     c                   exsr      StampaAR5
056320071025      *
056330071015      * - stampo i dati dell'O.R.M.
056340071015     c                   exsr      QUM_FnOrm
056350140410     c                   eval      Par5   = 'Quantità      : '
056360071015     c                                    + %trim($Qum(1)) + '    '
056370071015     c                                    + %trim($Qum(2)) + '    '
056380071015     c                                    + %trim($Qum(3))
056390071015     c                   exsr      StampaAR5
056400080228if  3c                   if        DB0tbo = '8'
056410140410     c                   eval      Par5   = 'Mittente      : '
056420080228     c                                    + ORMrsr
056430080228x   3c                   else
056440140410     c                   eval      Par5   = 'Destinatario  : '
056450071015     c                                    + ORMrsc
056460080228e   3c                   endif
056470071015     c                   exsr      StampaAR5
056480080228if  3c                   if        DB0tbo = '8'
056490140410     c                   eval      Par5   = '                '
056500080228     c                                    + ORMinr
056510080228x   3c                   else
056520140410     c                   eval      Par5   = '                '
056530071015     c                                    + ORMinc
056540080228e   3c                   endif
056550071015     c                   exsr      StampaAR5
056560080228if  3c                   if        DB0tbo = '8'
056570140410     c                   eval      Par5   = '                '
056580080228     c                                    + ORMcar + ' '
056590080228     c                                    + ORMlor + ' '
056600080228     c                                    + ORMprr + ' '
056610080228     c                                    + ORMnar
056620080228x   3c                   else
056630140410     c                   eval      Par5   = '                '
056640071015     c                                    + ORMcac + ' '
056650071015     c                                    + ORMloc + ' '
056660071015     c                                    + ORMprc + ' '
056670071015     c                                    + ORMnac
056680080228e   3c                   endif
056690071015     c                   exsr      StampaAR5
056700101022             // -?SE Ritiro Porto Franco Prepagato:?
056710080228if  3c                   if        DB0tbo = '8'
056720101022      /free
056730101022               // -?stampo il referente ed il relativo telefono?
056740101022               // -?(Comunque! Ci sono sempre!)?
056750140410               Par5   = 'Referente     : ' + ORMrer + '   '
056760101022                      + 'Telefono: '     + ORMter;
056770101022               exsr  StampaAR5;
056780101022      /end-free
056790140410     c                   eval      Par5   = 'Ordinante     : '
056800080228     c                                    + ORMrso
056810080228     c                   exsr      StampaAR5
056820140410     c                   eval      Par5   = 'Destinatario  : '
056830080303     c                                    + ORMrsc
056840080303     c                   exsr      StampaAR5
056850080228e   3c                   endif
056860071015      * - stampo le note
056870071015      *   (Comunque! Non solo "se ci sono". Al limite: scarto la 1ª.)
056880140410     c                   eval      Par5   = 'Note          : '
056890071015if  3c                   if        ORMno1 <> *blanks
056900071015     c                   eval      Par5   = %trim(Par5)  + ' '
056910071015     c                                    + ORMno1 + ' ' + ORMno2
056920071015x   3c                   else
056930071015     c                   eval      Par5   = %trim(Par5)  + ' '
056940071015     c                                    + ORMno2
056950071015e   3c                   endif
056960071015     c                   exsr      StampaAR5
056970071025      *
056980140410      * -?SE Ritiro Porto Franco Prepagato:?
056990071025if  3c                   IF        DB0tbo = '8'
057000140410      *
057010140410      * - stampo orari apertura (SE ci sono)
057020140410     c                   exsr      sr_OrariApert
057030140410      *
057040140410      * - stampo data & ora ritiro
057050071025     c                   z-add     ORMdar        w0080
057060071025     c     *iso          test(d)                 w0080                  31
057070071025if  4c                   if        NOT *in31
057080071025     c     *iso          movel     w0080         Data_EUR
057090071025     c     *eur          movel     Data_EUR      w0080
057100071025x   4c                   else
057110071025     c                   clear                   w0080
057120071025e   4c                   endif
057130140410     c                   eval      Par5   = 'Data ritiro   : '
057140071025     c                                    + %editc(w0080:'Y')
057150071025if  4c                   if        ORMorr > 1230
057160071025     c                   eval      Par5   = %trimr(Par5)
057170071025     c                                    + '          '
057180140410     c                                    + 'Ora ritiro: '
057190071025     c                                    + %editw(ORMorr:'0 :  ')
057200071025     c                   exsr      StampaAR5
057210071025x   4c                   else
057220071025     c                   exsr      StampaAR5
057230071025     c                   clear                   Par5
057240071025     c                   eval      %subst(Par5:50)
057250071025     c                                    = 'Ora ritiro: '
057260071025     c                                    + %editw(ORMorr:'0 :  ')
057270071025     c                   write     A4prtAR51
057280071025e   4c                   endif
057290140410      *
057300071025e   3c                   ENDIF
057310071025      *
057320071015      * - imposto codice a barre O.R.M.
057330071015     c                   movel     BarCodeORM    Pbco
057340071015      * - verifico se codice a barre da stampare
057350071015     c                   eval      *in52  =  (Pbco <> *zeros)
057360071025      *
057370071015      * - stampo box
057380071015     c                   eval      §LO2   = §PS1 + 0.050
057390071015if  3c                   if        NOT *in52
057400071015     c                   write     A4prtAR53
057410071015x   3c                   else
057420071025     c                   eval      §LV1   = §LO2 - §LO1
057430071015     c                   eval      §PS2   = §LO2 - 0.500
057440071015     c                   write     A4prtAR54
057450071015e   3c                   endif
057460071015      *
057470071015e   2c                   ENDIF
057480071015      *
057490071015     c                   eval      $FIAR5 = *off
057500071015      *
057510071015     c                   ENDSR
057520100513      /free
057530140410
057540140410       //--------------------------------------------------------------
057550140410       //?Reperimento, impostazione e stampa degli Orari di Apertura   ?
057560140410       //--------------------------------------------------------------
057570140410       BEGSR  sr_OrariApert;
057580140410
057590140410         clear  dOREorari;
057600140410
057610140410         // -?Recupero dati da estensione FNORE (trc "O ")?
057620140410         if  NOT  %open(FNORE01L);
057630140410           open  FNORE01L;
057640140410         endif;
057650140410         k_OREpoe = ORMpoe;
057660140410         k_OREnsr = ORMnsr;
057670140410         k_OREnor = ORMnor;
057680140410         k_OREnrv = ORMnrv;
057690140410         k_OREtrc = 'O ';
057700140410         chain  %kds(keyFNORE01)  FNORE000;
057710140410
057720140410         // -?...NON trovati => uscita dalla subroutine?
057730140410         if  Not  %found(FNORE01L);
057740140410           leavesr;
057750140410         endif;
057760140410
057770140410         // -?...Trovati => verifica degli orari?
057780140410         dOREorari = OREdati;
057790140410
057800140410         Select;
057810140410
057820140410           // -?C'è un solo orario "ALLE"?
057830140410           When  §OREoraMda = *zero  and  §OREoraMa > *zero  and
057840140410                 §OREoraPda = *zero  and  §OREoraPa = *zero;
057850140410             Par5 = 'Orari apertura: ' +
057860140410                    'Fino alle ' + %editw( §OREoraMa : ' 0:  ' );
057870140410           When  §OREoraMda = *zero  and  §OREoraMa = *zero  and
057880140410                 §OREoraPda = *zero  and  §OREoraPa > *zero;
057890140410             Par5 = 'Orari apertura: ' +
057900140410                    'Fino alle ' + %editw( §OREoraPa : ' 0:  ' );
057910140410
057920140410           // -?C'è un solo orario completo (per 1°)?
057930140410           When  §OREoraMda > *zero  and  §OREoraMa > *zero  and
057940140410                 §OREoraPda = *zero  and  §OREoraPa = *zero;
057950140410             Par5 = 'Orari apertura: ' +
057960140410                    %editw( §OREoraMda : ' 0:  ' ) + '  ' +
057970140410                    %editw( §OREoraMa  : ' 0:  ' );
057980140410
057990140410           // -?Ci sono due orari completi?
058000140410           When  §OREoraMda > *zero  and  §OREoraMa > *zero  and
058010140410                 §OREoraPda > *zero  and  §OREoraPa > *zero;
058020140410             Par5 = 'Orari apertura: ' +
058030140410                    %editw( §OREoraMda : ' 0:  ' ) + '  ' +
058040140410                    %editw( §OREoraMa  : ' 0:  ' ) + ' - ' +
058050140410                    %editw( §OREoraPda : ' 0:  ' ) + '  ' +
058060140410                    %editw( §OREoraPa  : ' 0:  ' );
058070140410
058080140410         EndSl;
058090140410
058100140410         exsr  StampaAR5;
058110140410
058120140410       ENDSR;
058130100513
058140100513       //--------------------------------------------------------------
058150100513       //?Verifica se presente ORM con preavviso mail                  ?
058160100513       //--------------------------------------------------------------
058170100513       BEGSR  sr_CheckORM_PrevvMail;
058180100514
058190100514         //clear  dOrm01               (già fatto)
058200100513
058210100513         // -?Reperimento num. ORM (se NON già reperito)?
058220100513         if  DB0tbo <> '8';
058230100513           exsr  RepAR4_M;
058240100513         endif;
058250100513
058260100513         // -?ORM inesistente: inutile proseguire?
058270100513         if  §B4poe = *zero    or   §B4nor = *zero;
058280100513           leavesr;
058290100513         endif;
058300100513
058310100513         // -?Verifica presenza O.R.M.?
058320100513         if  Not  %open(FNORM01L);
058330100513           open  FNORM01L;
058340100513         endif;
058350100513
058360100513         ORMpoe = §B4poe;
058370100513         ORMnsr = §B4nsr;
058380100513         ORMnor = §B4nor;
058390100513         ORMnrv = §B4nrv;
058400100513
058410100513         chain  K04ORM01  FNORM000;
058420100513
058430100513         // -?ORM non reperito: inutile proseguire?
058440100513         if  Not  %found(FNORM01L);
058450100513           leavesr;
058460100513         endif;
058470100513
058480100513         // -?Impostazione flag per stampa porto "ASSEGNATO"?
058490100513         dOrm01 = ORMflo;
058500100513         $PreavvORM = (§ORMpre = 'M');
058510100513
058520100513       ENDSR;
058530100513
058540100513      /end-free
058550030515      *
058560030515      *---------------------------------------------------------------*
058570050217      *?Stampa dati DPD                                              ?*
058580030515      *---------------------------------------------------------------*
058590030515     c     StampaDPD     BEGSR
058600030515      *
058610080922      * (EX) Bar-Code - non più stampato come codice a barre
058620030515      * (1' 12 char di Riferimento Mittente Alfanumerico - se numerici)
058630060626      *---
058640061030      * Il barcode DPD è stato variato ora può essere anche
058650061030      *   alfanumerico ed è lungo 14
058660061030      * Si trova sul file FIAR4 rcd 'I'
058670061030      * I controlli che faceva prima sono da cambiare
058680061030      *---
058690061030      *
058700060626      * nuovi controlli per nuovo barcode DPD
058710061030if  1c                   if        §B4Ipn <> *blanks
058720061030      * - controllo se vecchio o nuovo barcode
058730061030if  2c                   if        %subst(§B4Ipn:14:1) = *blanks
058740061030      * - - vecchio
058750061030     c                   eval      Pbcd2 = '01 ' + %subst(§B4Ipn:1:3) +
058760061030     c                                     ' '   + %subst(§B4Ipn:4:8)
058770061030      * - - nuovo
058780061030x   2c                   else
058790061030     c                   eval      Pbcd2 = '01 ' + %subst(§B4Ipn:1:4) +
058800061030     c                                     ' '   + %subst(§B4Ipn:5:10)
058810061030e   2c                   endif
058820061030e   1c                   endif
058830080922      /free
058840080922         // Impostazione data
058850080922         PdpA = 'Date ' + %editc(Pdsp:'Y');
058860080922
058870080922         // Impostazione numero colli
058880080922         PdpB = 'Parcel N. ' + %editc(Pncl:'Z');
058890080922
058900080922         // Impostazione numero parcel
058910080922         PdpC = §B4Ipn;
058920080922
058930080922         // Impostazione "costante" da stampare tra le Note
058940080922         PdpD = 'Controllare la corrispondenza';
058950080922         PdpE = 'del destinatario con l''indirizzo';
058960080922         PdpF = 'dell''etichetta DPD.';
058970080922
058980080922         // Impostaz. posizioni sulla copia per il vettore già stampata
058990080922         §DP1 = wPosIni + 1.100;
059000080922         §DP2 = §DP1 + 0.550;
059010080922         §DP3 = §DP2 + 0.120;
059020080922         §DP4 = §DP3 + 0.120;
059030080922         §DP5 = §DP4 + 0.120;
059040080922         §DP6 = §DP5 + 0.160;
059050080922         §DP7 = §DP6 + 0.130;
059060080922         §DP8 = §DP7 + 0.130;
059070080922      /end-free
059080080922      *
059090080922      * (inutile testare l'overflow perchè siamo in ex-A5)
059100080922     c                   write     A4prtDPD
059110030515      *
059120030515     c                   ENDSR
059130030515      *
059140030515      *---------------------------------------------------------------*
059150050217      *?Prepara elenco per box - Elenco "Package Tracking Number"    ?*
059160030515      *---------------------------------------------------------------*
059170030515     c     ListaFED      BEGSR
059180030515      *
059190030515      * Stampo il simbolo "FedEx" (sulla 1ª pagina):
059200030515      *   imposto le posizioni iniziali di stampa per le linee
059210030515      *   (orizzontali-§LOn e verticali-§LVn)
059220080912     c                   eval      §LO1   = 3.330
059230080912     c                   eval      §LO2   = §LO1 + 0.370
059240081023     c                   eval      §LV1   = §LO2 - 0.150
059250081023     c                   eval      §LV2   = §LO2 - 0.070
059260030515     c                   write     A4prtFED1
059270080818      *
059280080818      * Stampo le "costanti" di inizio lista
059290030515     c                   write     A4prtFED2
059300030515      *
059310030515      * Stampo elenco segnacolli FedEx
059320030709     c                   eval      §LO1   = §LO2 - 0.250
059330030515      * - per le bolle in partenza
059340030515if  1c                   IF           DB0tbo = 'P'
059350030515     c                             or DB0tbo = '8'
059360030515if  2c                   if        NOT %open(FNBLT01L)
059370030515     c                   open      FNBLT01L
059380030515e   2c                   endif
059390030515      * - per le bolle in arrivo
059400030515x   1c                   ELSE
059410070319if  2c                   if        NOT %open(FNART01L)
059420070319     c                   open      FNART01L
059430070319e   2c                   endif
059440030515e   1c                   ENDIF
059450070319if  1c                   if        NOT %open(FIARS01L)
059460070319     c                   open      FIARS01L
059470070319e   1c                   endif
059480030515     c                   eval      Wtrc = 'C'
059490030515     c                   clear                   Wnsc
059500030515     c                   clear                   Wawb
059510030515do  1c                   dou       Wawb = *blanks
059520030515     c                   exsr      StampaFED
059530030515e   1c                   enddo
059540051020      *
059550030515     c                   ENDSR
059560030515      *
059570030515      *---------------------------------------------------------------*
059580050217      *?Stampa singola riga relativa a FEDEX                         ?*
059590030515      *---------------------------------------------------------------*
059600030515     c     StampaFED     BEGSR
059610030515      *
059620030515      * Impostazione campi di stampa
059630030515     c                   clear                   Wawb
059640030515      * - Carico AWB FedEx
059650030515      * - - leggo segnacolli per bolle in partenza
059660030515if  1c                   IF           DB0tbo = 'P'
059670030515     c                             or DB0tbo = '8'
059680030515     c                   exsr      CarAWB_Blx
059690030515      * - - leggo segnacolli per bolle in arrivo
059700030515x   1c                   ELSE
059710030515     c                   exsr      CarAWB_Arx
059720030515e   1c                   ENDIF
059730030515     c     wAWB          cabeq     *blanks       End_FED
059740030515     c                   move      wAWB          Pptn
059750030515     c                   add       1             Psts
059760030515      *
059770030515      * Controllo se superata l'ultima riga
059780030515     c                   exsr      Test_OF
059790030709      *
059800030709      * Imposto le posizioni iniziali di stampa per le linee
059810030709      *   (orizzontali-§LOn e verticali-§LVn) e per i campi (§PSn)
059820030709     c                   eval      §LO1 = §LO1 + 0.250 - 0.008
059830030709     c                   eval      §LO2 = §LO1 + 0.250
059840030709     c                   eval      §LV1 = §LO1
059850030709     c                   eval      §LV2 = §LO2 - 0.100
059860030709     c                   eval      §PS1 = §LO1 + 0.200
059870030515      *
059880030515      * Stampa del singolo record
059890030515     c                   write     A4prtFED3
059900030515      *
059910030515     c     End_FED       ENDSR
059920030515      *
059930030515      *---------------------------------------------------------------*
059940051020      *?Reperimento AWB FedEx da file FNBLT e FIARS (bolle partenza) ?*
059950030515      *---------------------------------------------------------------*
059960030515     c     CarAWB_Blx    BEGSR
059970130215      *
059980130215      * Verifica se, al di là del codice bolla (vedi "$ "), esiste
059990130215      *   comunque una bolla mamma
060000130215     c                   exsr      sr_Mamma_FedEx
060010030515      *
060020030515      * Posizionamento
060030130215if  2c                   if        $Mamma = *off   and
060040130215     c                             $Mamma_FedEx = *off
060050040607     c     KLDVnsc       setgt     FNBLT000
060060070514x   2c                   else
060070070514     c     KLBLnsc       setgt     FNBLT000
060080070514e   2c                   endif
060090030515      *
060100030515      * Ciclo di lettura
060110030515do  1c                   DO        *hival
060120030515      *
060130130215if  2c                   if        $Mamma = *off   and
060140130215     c                             $Mamma_FedEx = *off
060150040607     c     KLDV          reade     FNBLT000
060160070514x   2c                   else
060170070514     c     KLBL          reade     FNBLT000
060180070514e   2c                   endif
060190030515if  2c                   if        %eof(FNBLT01L)
060200030515     c                   leave
060210030515e   2c                   endif
060220030515      * - per ogni segnacollo cerco l'AWB
060230060505     c     K05ARS01P     chain     FIARS000
060240060505if  2c                   if            %found(FIARS01L)
060250060505     c                             and %subst(ARSnot:1:1) >= *zeros
060260060505     c                   eval      wAWB = %subst(ARSnot:1:12)
060270060505     c                   eval      wNSC = ARSnsc
060280030515     c                   leave
060290030515e   2c                   endif
060300030515      *
060310030515e   1c                   ENDDO
060320030515      *
060330030515     c                   ENDSR
060340030515      *
060350030515      *---------------------------------------------------------------*
060360070319      *?Reperimento AWB FedEx da file FNART e FIARS (bolle arrivo)   ?*
060370030515      *---------------------------------------------------------------*
060380030515     c     CarAWB_Arx    BEGSR
060390130215      *
060400130215      * Verifica se, al di là del codice bolla (vedi "$ "), esiste
060410130215      *   comunque una bolla mamma
060420130215     c                   exsr      sr_Mamma_FedEx
060430030515      *
060440030515      * Posizionamento
060450130215if  2c                   if        $Mamma = *off   and
060460130215     c                             $Mamma_FedEx = *off
060470040607     c     KLDVnsc       setgt     FNART000
060480070514x   2c                   else
060490130215     c     KLBLnsc       setgt     FNART000
060500070514e   2c                   endif
060510030515      *
060520030515      * Ciclo di lettura
060530030515do  1c                   DO        *hival
060540030515      *
060550130215if  2c                   if        $Mamma = *off   and
060560130215     c                             $Mamma_FedEx = *off
060570040607     c     KLDV          reade     FNART000
060580070514x   2c                   else
060590070514     c     KLBL          reade     FNART000
060600070514e   2c                   endif
060610030515if  2c                   if        %eof(FNART01L)
060620030515     c                   leave
060630030515e   2c                   endif
060640030515      * - per ogni segnacollo cerco l'AWB
060650060505     c     K05ARS01A     chain     FIARS000
060660060505if  2c                   if            %found(FIARS01L)
060670030515     c                             and %subst(ARSnot:1:1) >= *zeros
060680030515     c                   eval      wAWB = %subst(ARSnot:1:12)
060690030515     c                   eval      wNSC = ARSnsc
060700030515     c                   leave
060710030515e   2c                   endif
060720030515      *
060730030515e   1c                   ENDDO
060740030515      *
060750030515     c                   ENDSR
060760130215      *
060770130215      /free
060780130215
060790130215       //--------------------------------------------------------------
060800130215       //?Verifica se la spedizione FedEx ha bolla mamma.              ?
060810130215       //--------------------------------------------------------------
060820130215       BEGSR  sr_Mamma_FedEx;
060830130215
060840130215         //$Mamma_FedEx = *off;          (già così)
060850130215
060860130215         If  Not $Mamma;
060870130215
060880130215           // -?Verifica se la spedizione ha comunque mamma?
060890130215           //  ?(al di là del codice bolla - vedi cod. "$ ")?
060900130215           if  not %open(FNLBL01L);
060910130215             open  FNLBL01L;
060920130215           endif;
060930130215           chain  (DB0aas : DB0lnp : DB0nrs : DB0nsp)  FNLBL000;
060940130215
060950130215           $Mamma_FedEx = %found(FNLBL01L);
060960130215
060970130215         EndIf;
060980130215
060990130215       ENDSR;
061000130215
061010130215      /end-free
061020040607      *
061030040607      *---------------------------------------------------------------*
061040080918      *?Stampa DDT / PACKING-LIST del mittente                       ?*
061050040607      *---------------------------------------------------------------*
061060040607     c     StampaDDT     BEGSR
061070130315      *
061080130315      * - Uscita in assenza del file FIAR801L
061090130315     c                   if        Not  $E_AR8
061100130315     c                   leavesr
061110130315     c                   endif
061120080825      *
061130080825      * Meglio togliere l'impostazione relativa ai segnacolli non seq.
061140080825      *   per far funzionare il 1° posizionamento nella subr. Rep_§LO2
061150080825     c                   reset                   $A4_x_ScNS
061160080918      /free
061170080923         *in45 = (DB0fa4 = 'P');
061180080923         *in46 = (DB0fa4 = 'P'   and   %rem(wNumLdv : 2) = *zero);
061190080918      /end-free
061200040607      *
061210040607      * Se già stampato qualcos'altro:
061220040607      *   forzo il salto alla pagina successiva
061230080926       // (SE NON si sta stampando SOLO una packing-list, nel qual caso
061240080926       //  il posizionamento nella pagina A4 verrà eseguito dagli
061250080926       //  indicatori 45 e 46)
061260080926if  1c                   if           (CurLineA4 > $MaxLine1
061270080926     c                             or  §LO2      > $LimOF1)
061280080926     c                             and NOT *in45
061290040607     c                   eval      CurLineA4 = wMaxLine
061300040607     c                   exsr      Test_OF
061310040607e   1c                   endif
061320040607      *
061330040607     c                   eval      $DDT_p = *on
061340040607      *
061350130315      /free
061360130315         // -?Posizionamento?
061370130315         clear  k06fiar801;
061380130315         k_AR8aas = DB0aas;
061390130315         k_AR8lnp = DB0lnp;
061400130315         k_AR8nrs = DB0nrs;
061410130315         k_AR8nsp = DB0nsp;
061420130315         k_AR8tmd = c_§PDF + '999';
061430130315         setgt  %kds( k06fiar801 : 5 )  FIAR8000;
061440130315      /end-free
061450130315      *
061460130315      * - Letura
061470040607do  1c                   dou          %eof(FIAR801L)
061480040607     c                             or AR8atb = *blanks
061490130308     c                             or AR8tmd <> c_§PDF
061500040607     c     KLDV          reade     FIAR8000
061510040607e   1c                   enddo
061520040607if  1c                   if        %eof(FIAR801L)
061530040607     c                   goto      NOprtDDT
061540040607e   1c                   endif
061550041207if  1c                   if        §AR8des   = *blanks
061560041207     c*** (già fatto:)   reset                   dAR8
061570041207     c                   reset                   TIBS02ds
061580041207     c                   eval      T02cod    = 'AR8'
061590041207     c                   movel(p)  AR8tmd        T02ke1
061600041207     c                   call      'TIBS02R'
061610041207     c                   parm                    KPJBA
061620041207     c                   parm                    TIBS02ds
061630041207if  2c                   if        T02err    = *blanks
061640041207     c                   movel     T02uni        dAR8
061650041207e   2c                   endif
061660041207e   1c                   endif
061670040607      *
061680040607sel 1c                   select
061690050217      * - Karnak o Titanedi o EDk (identiche)
061700080730w   1c                   when      §AR8tsd   = 'K1'
061710040607     c                   exsr      DDT_Karnak
061720050414      * - Logistica o Sait-Abrasivi
061730060627w   1c                   when      %subst(§AR8tsd:1:1) = 'L'
061740050218     c                   exsr      DDT_Logistica
061750050217      * - Arcte
061760050215w   1c                   when      AR8tmd    = 'ARCT'
061770050217     c                   exsr      DDT_Arcte
061780050214      * - Esprinet
061790060627w   1c                   when      §AR8tsd   = 'E1'
061800050214     c                   exsr      DDT_Esprinet
061810041207      * - dati generici (con logo)
061820041207w   1c                   when      §AR8tsd   = 'S2'
061830041207     c                   exsr      DDT_genericoL
061840041207      * - dati generici (senza logo)
061850041207w   1c***                when      §AR8tsd   = 'S1'
061860040607     c                   other
061870040607     c                   exsr      DDT_generico
061880040607e   1c                   endsl
061890040607      *
061900060921     c                   reset                   $FIAR8
061910040607     c                   reset                   $DDT_o
061920040607     c                   reset                   $DDT_p
061930040608     c                   reset                   $DDTlastPg
061940040608     c                   clear                   wOmaRowT
061950040608     c                   clear                   wOmaRowD
061960040607     c                   clear                   $AR8o1
061970040607      *
061980040607     c     NOprtDDT      ENDSR
061990040607      *
062000040607      *---------------------------------------------------------------*
062010050217      *?Stampa DDT del mittente KARNAK / TITANEDI                    ?*
062020040607      *---------------------------------------------------------------*
062030040607     c     DDT_Karnak    BEGSR
062040040607      *
062050080923      /free
062060080923         // Posizionamento iniziale all'interno del foglio A4
062070080923         write  A4prtSP1;
062080080923
062090080923         // Impostazione del nuovo limite x overflow
062100080923         select;
062110080923           when  NOT *in45;
062120080923             wMaxLine = $MaxLine3;
062130080923           when  *in45   and   NOT *in46;
062140080923             wMaxLine = $MaxLine3_pl1;
062150080923           when  *in45   and   *in46;
062160080923             wMaxLine = $MaxLine3_pl2;
062170080923         endsl;
062180080923      /end-free
062190040607      *
062200040607      * Impostazione testi fissi - - - - - - - - - - - - - - - - - - -*
062210040607      *
062220040607     c                   eval      PCtx1 = 'ATTENZIONE: VERIFICARE LA -
062230040607     c                             QUANTITA'' E L''INTEGRITA'' DEI CAR-
062240040607     c                             TONI. IN CASO DI DISCORDANZA RITIRA-
062250040607     c                             RE CON RISERVA.'
062260130118     c*//                eval      PCtx2 = 'La merce viagga a rischio -
062270130118     c*//                          e pericolo del committente anche se-
062280130118     c*//                           venduta franco destino. Trascorsi -
062290130118     c*//                          otto giorni dal ricevimento della m-
062300130118     c*//                          erce non si accettano reclami.   - -
062310130118     c*//                            IN CASO DI RITARDATO PAGAMENTO DE-
062320130118     c*//                          CORRERANNO GLI INTERESSI BANCARI IN-
062330130118     c*//                           VIGORE.'
062340130118     c*//                eval      PCtx3 = 'Per ogni vertenza l''acqui-
062350130118     c*//                          rente accetta la competenza della R-
062360130118     c*//                          epubblica di San Marino o in altern-
062370130118     c*//                          ativa del foro di Rimini.'
062380130118     c*//                eval      PCtx4 = 'NON SI ACCETTANO RESI E NO-
062390130118     c*//                          N SI EFFETTUANO SOSTITUZIONI SE NON-
062400130118     c*//                           AUTORIZZATI PER ISCRITTO DALLA NOS-
062410130118     c*//                          TRA DIREZIONE COMMERCIALE.'
062420130118     c*//                eval      PCtx5 = 'Quand''anche autorizzati, -
062430130118     c*//                          i resi verranno accettati e le sost-
062440130118     c*//                          ituzioni verranno effettuate solo d-
062450130118     c*//                          opo aver accertato il perfetto stat-
062460130118     c*//                          o nel nostro stabilimento della mer-
062470130118     c*//                          ce restituita.'
062480130118     c                   eval      PCtx2 = 'Eventuali reclami per mate-
062490130118     c                             riale mancante o danneggiato all''i-
062500130118     c                             nterno dei colli dovranno pervenire-
062510130118     c                              entro 8 (otto) giorni di calendari-
062520130118     c                             o dall''avvenuta consegna a MyO S.r-
062530130118     c                             .l. - Ufficio Resi - Via Santarcang-
062540130118     c                             iolese, 6 - 47825 Torriana (RN)'
062550130118     c                   eval      PCtx4 = 'Eventuali mancanze di coll-
062560130118     c                             i e/o avarie sono da contestare all-
062570130118     c                             ''atto della consegna con annotazio-
062580130118     c                             ne su lettera di vettura e DDT'
062590040709if  1c                   if        AR8tmd = 'TITA'
062600040709     c                   eval      PCtx6 = 'Depositario: K & K Logisti-
062610040709     c                             cs S.p.a. - via Santarcangiolese, 6-
062620040709     c                              - 47825 Torriana (RN) - P.I. 00953-
062630040709     c                             860400'
062640040709e   1c                   endif
062650080822      *
062660040607      * Conteggio delle righe "omaggio" - - - - - - - - - - - - - - -*
062670040607      *
062680040614     c                   clear                   xx
062690040607     c                   eval      SAVprr = AR8prr
062700040607     c     KLDV          setgt     FIAR8000
062710040607     c     KLDV          readpe    FIAR8000
062720040607do  1c                   dow       NOT %eof(FIAR801L)
062730040607sel 2c                   select
062740040607w   2c                   when      AR8atb <> *blanks
062750040611w   2c                   when      AR8trk =  'O2'
062760040608     c                   add       1             wOmaRowD
062770040611w   2c                   when      AR8trk =  'O1'
062780040608     c                   add       1             wOmaRowT
062790051110     c                   movel(p)  AR8uni        dAR8O1
062800040607     c                   add       1             xx
062810040607     c                   eval      $AR8o1(xx) =  §AR8O1a
062820040607x   2c                   other
062830040607     c                   leave
062840040607e   2c                   endsl
062850040607     c     KLDV          readpe    FIAR8000
062860040607e   1c                   enddo
062870040607      * - mi riposiziono
062880040607     c                   eval      AR8prr = SAVprr
062890040607     c     K06AR801      setll     FIAR8000
062900040607     c     K06AR801      reade     FIAR8000
062910040607      *
062920040607      * Reperimento dati, impostazione e stampa DDT mittente  - - - -*
062930040607      *
062940040607if  1c                   dow       NOT %eof(FIAR801L)
062950040607      *
062960040607sel 2c                   select
062970040607      * - Record annullato (non si stampa nulla)
062980040607w   2c                   when      AR8atb <> *blanks
062990040607      * - Dati di dettaglio: articolo, descrizione, quantità
063000040607w   2c                   when      AR8trk = 'C1'
063010050217     c                   exsr      PrtDdtKARN_C1
063020040607      * - Dati di dettaglio: testo per articoli non disponibili
063030040607w   2c                   when      AR8trk = 'C2'
063040050217     c                   exsr      PrtDdtKARN_C2
063050040607      * - Dati di dettaglio: descrizioni generiche (orario apertura)
063060040607w   2c                   when      AR8trk = 'C3'
063070050217     c                   exsr      PrtDdtKARN_C3
063080040607      * - Dati di testata: destinatario, cliente di fatturazione
063090040607w   2c                   when      AR8trk = 'T1'
063100050217     c                   exsr      PrtDdtKARN_T1
063110040607      * - Dati di testata: agente, numero ordine, estremi documento
063120040607w   2c                   when      AR8trk = 'T2'
063130050217     c                   exsr      PrtDdtKARN_T2
063140040607      * - Dati di fondo: mezzo e causale trasporto, colli, peso
063150040607w   2c                   when      AR8trk = 'V1'
063160050217     c                   exsr      PrtDdtKARN_V1
063170040607      * - Dati di fondo: vettore, annotazioni e note
063180040607w   2c                   when      AR8trk = 'V2'
063190050217     c                   exsr      PrtDdtKARN_V2
063200040607      * - Dati esterni: riga descrittiva con referente
063210040607w   2c                   when      AR8trk = 'L1'
063220050217     c                   exsr      PrtDdtKARN_L1
063230040607      * - Dati di omaggio: testo per articoli omaggiati
063240040607w   2c                   when      AR8trk = 'O1'
063250050217     c                   exsr      PrtDdtKARN_O1
063260040607      * - Dati di omaggio: articolo, descrizione, quantità
063270040607w   2c                   when      AR8trk = 'O2'
063280050217     c                   exsr      PrtDdtKARN_O2
063290040607e   2c                   endsl
063300040607      *
063310040607     c     KLDV          reade     FIAR8000
063320040607      *
063330040607e   1c                   enddo
063340040607      *
063350040607      * Reimpostazione del vecchio limite x overflow
063360040607     c                   eval      wMaxLine = $MaxLine2
063370040607      *
063380040607     c                   ENDSR
063390050214      *
063400040607      *---------------------------------------------------------------*
063410050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
063420050217      *?Dati di testata: destinatario, cliente di fatturazione       ?*
063430040607      *---------------------------------------------------------------*
063440050217     c     PrtDdtKARN_T1 BEGSR
063450040607      *
063460040607      * > logo
063470040607     c                   eval      §Logo  = 'LOGO' + AR8tmd
063480040607      *
063490051110     c                   movel(p)  AR8uni        dAR8T1
063500040607      *
063510040607      * > destinatario
063520040607     c                   eval      PCrsd  = §AR8t1a
063530040607     c                   eval      PCind  = §AR8t1b
063540040616if  1c                   if        §AR8t1c  > *zeros
063550040607     c                   eval      PCcad  = §AR8t1c
063560040616e   1c                   endif
063570040607     c                   eval      PClod  = §AR8t1d
063580040607     c                   eval      PCprd  = §AR8t1e
063590040607     c***                eval      PCnzd  = *blanks
063600040607      * > cliente di fatturazione
063610040607     c                   eval      PCrsf  = §AR8t1f
063620040607     c                   eval      PCinf  = §AR8t1g
063630040616if  1c                   if        §AR8t1h  > *zeros
063640040607     c                   eval      PCcaf  = §AR8t1h
063650040616e   1c                   endif
063660040607     c                   eval      PClof  = §AR8t1i
063670040607     c                   eval      PCprf  = §AR8t1j
063680040607     c***                eval      PCnzf  = *blanks
063690040607      *
063700040607     c                   ENDSR
063710040607      *
063720040607      *---------------------------------------------------------------*
063730050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
063740050217      *?Dati di testata: agente, ..., porto, ...                     ?*
063750040607      *---------------------------------------------------------------*
063760050217     c     PrtDdtKARN_T2 BEGSR
063770040607      *
063780051110     c                   movel(p)  AR8uni        dAR8T2
063790040607      *
063800040607      * > dati in testata DDT
063810040607      * - - codice agente
063820130122     c*//                eval      PCage  = §AR8t2a
063830040607      * - - numero e data ordine
063840040607     c                   eval      PCnro  = §AR8t2b
063850040607     c                   eval      PCdto  = §AR8t2c
063860040607      * - - riferimenti
063870040607     c                   eval      PCrif  = §AR8t2d
063880040607      * - - codice destinatario (dal mittente)
063890040611     c                   eval      PCcli  = §AR8t2e
063900040607      * - - numero e data documento (DDT)
063910040607     c                   eval      PCnrd  = §AR8t2f
063920040607     c                   eval      PCdtd  = §AR8t2g
063930040607      * - - numero pagina documento originale (qui ricalcolato)
063940040607     c***                eval      Page   = §AR8t2h
063950040607      * - - porto
063960040607     c                   eval      PCpto  = §AR8t2i
063970040607      * - - condizioni di pagamento
063980040611     c                   eval      PCpag  = §AR8t2j
063990040607     c                                    + ' '
064000040607     c                                    + %trim(§AR8t2k)
064010040607      * - - tipologia spedizione
064020040607     c                   eval      PCtsp  = §AR8t2l
064030040607      *
064040040607      * > Stampa testata DDT - - - - - - - - - - - - - - - - - - - - -*
064050040607      *
064060040607      *   - OverFlow impossibile qui! -
064070040607     c                   write     A4prtDK1
064080040607      *
064090040607     c                   ENDSR
064100040607      *
064110040607      *---------------------------------------------------------------*
064120050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
064130050217      *?Dati di dettaglio: articolo, descrizione, quantità           ?*
064140040607      *---------------------------------------------------------------*
064150050217     c     PrtDdtKARN_C1 BEGSR
064160040607      *
064170040607     c                   clear                   A4prtDK2
064180051110     c                   movel(p)  AR8uni        dAR8C1
064190040607      *
064200040607      * > dati in dettaglio DDT
064210040607      * - - codice prodotto
064220040607     c                   eval      PCcpr  = §AR8c1a
064230040607      * - - descrizione prodotto
064240040607     c                   eval      PCdpr  = §AR8c1b
064250040607      * - - quantità
064260040607     c                   eval      PCqta  = '     ' + §AR8c1c
064270040607      * - - note
064280040607     c***                eval      PCnot  = *blanks
064290040607      *
064300040607      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
064310040607      *
064320040607     c                   exsr      Test_OF
064330040607     c                   write     A4prtDK2
064340040607      *
064350040607     c                   ENDSR
064360040607      *
064370040607      *---------------------------------------------------------------*
064380050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
064390050217      *?Dati di dettaglio: testo per articoli non disponibili        ?*
064400040607      *---------------------------------------------------------------*
064410050217     c     PrtDdtKARN_C2 BEGSR
064420040607      *
064430040607     c                   clear                   A4prtDK3
064440051110     c                   movel(p)  AR8uni        dAR8C2
064450040607      *
064460040607      * > dati in note DDT
064470040607      * - - riga descrittiva
064480040607     c                   eval      PCtxt  = §AR8c2a
064490040607      *
064500040607      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
064510040607      *
064520040607     c                   exsr      Test_OF
064530040607     c                   write     A4prtDK3
064540040607      *
064550040607     c                   ENDSR
064560040607      *
064570040607      *---------------------------------------------------------------*
064580050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
064590050217      *?Dati di dettaglio: descrizioni generiche (orario apertura)   ?*
064600040607      *---------------------------------------------------------------*
064610050217     c     PrtDdtKARN_C3 BEGSR
064620040607      *
064630040607     c                   clear                   A4prtDK2
064640051110     c                   movel(p)  AR8uni        dAR8C3
064650040607      *
064660040607      * > dati in note DDT
064670040607      * - - riga descrittiva
064680040607     c                   eval      PCdpr  = §AR8c3a
064690040607      *
064700040607      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
064710040607      *
064720040607     c                   exsr      Test_OF
064730040607     c                   write     A4prtDK2
064740040607      *
064750040607     c                   ENDSR
064760040607      *
064770040607      *---------------------------------------------------------------*
064780050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
064790050217      *?Dati di fondo: mezzo e causale trasporto, colli, peso        ?*
064800040607      *---------------------------------------------------------------*
064810050217     c     PrtDdtKARN_V1 BEGSR
064820040607      *
064830051110     c                   movel(p)  AR8uni        dAR8V1
064840040607      *
064850040607      * Controllo se ci saranno da stampare tante righe omaggio da
064860040607      *   chiudere in pagina successiva:
064870040608      *   i campi del box finale del DDT (in totale 9 righe) verranno
064880040607      *   riempiti solo nella ultima pagina.
064890040607      *
064900080924      /free
064910080924         select;
064920080924           when  NOT *in45;
064930080924      /end-free
064940040608     c                   eval      $DDTlastPg = ($MaxLine2
064950040713     c                                        +  1
064960040608     c                                        -  CurLineA4
064970040608     c                                        -  $BoxRow1
064980040608     c                                        -  wOmaRowT
064990040608     c                                        >= wOmaRowD)
065000080924      /free
065010080924           when  *in45   and   NOT *in46;
065020110406             $DDTlastPg = ($MaxLine2_pl1k
065030080924                           + 1 - CurLineA4 - $BoxRow1 - wOmaRowT
065040080924                           >= wOmaRowD);
065050080924           when  *in45   and   *in46;
065060110406             $DDTlastPg = ($MaxLine2_pl2k
065070080924                           + 1 - CurLineA4 - $BoxRow1 - wOmaRowT
065080080924                           >= wOmaRowD);
065090080924         endsl;
065100080924      /end-free
065110040607      *
065120040607      * > dati in chiusura DDT
065130040607      *   Per ora imposto (ho già impostato) solo la DS "dAR8V1".
065140040607      *   I corrispondenti campi di stampa li impostero nella subr.
065150050217      *     "PrtDdtKARN_V2".
065160040607      *
065170040607     c                   ENDSR
065180040607      *
065190040607      *---------------------------------------------------------------*
065200050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
065210050217      *?Dati di fondo: vettore, annotazioni e note                   ?*
065220040607      *---------------------------------------------------------------*
065230050217     c     PrtDdtKARN_V2 BEGSR
065240040607      *
065250051110     c                   movel(p)  AR8uni        dAR8V2
065260040618      *
065270040618      * > dati in chiusura DDT
065280040618      *
065290040607      * Ho già controllato se ci saranno da stampare tante righe
065300040607      *   omaggio da chiudere in pagina successiva
065310040608if  1c                   if        $DDTlastPg = *off
065320040607     c                   eval      PCnv2  = '          '
065330040607     c                                    + '*** SEGUE ***'
065340040618x   1c                   else
065350050217     c                   exsr      PrtDdtKARNaV2
065360040607e   1c                   endif
065370040607      *
065380040607      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
065390040608      *
065400050217     c                   exsr      PrtDdtKARNbV2
065410040607      *
065420040607      * > Impostazione nuovo limite di OverFlow  - - - - - - - - - - -*
065430080923      /free
065440080923         select;
065450080923           when  NOT *in45;
065460080923             wMaxLine = $MaxLine2;
065470080923           when  *in45   and   NOT *in46;
065480080923             wMaxLine = $MaxLine2_pl1;
065490080923           when  *in45   and   *in46;
065500080923             wMaxLine = $MaxLine2_pl2;
065510080923         endsl;
065520080923      /end-free
065530040607      *
065540040608      * N.B.: la copia per il destinatario della LDV deve comparire
065550040608      *       nell'ultima pagina del DDT per il cliente (vedi subr.
065560040608      *       "ChiusuraLDV")
065570040607      *
065580040607     c                   ENDSR
065590040618      *
065600040618      *---------------------------------------------------------------*
065610040618      *
065620050217     c     PrtDdtKARNaV2 BEGSR
065630040618      *
065640040618      * > dati in chiusura DDT
065650040618      *
065660040618      * > > da dAR8V1
065670040618      * - - mezzo di trasporto
065680040618     c                   movel     §AR8v1a       PCmtr
065690040618      * - - causale trasporto
065700040618     c                   eval      PCctr  = §AR8v1b
065710040618      * - - numero colli
065720040618     c                   eval      PCncl  = §AR8v1c
065730040618      * - - peso in kg
065740040618     c                   eval      PCpkg  = §AR8v1d
065750040618      * - - aspetto esteriore dei beni
065760040618     c                   eval      PCaeb  = §AR8v1e
065770040618      *
065780040618      * > > da dAR8V2
065790120530      * - - dati vettore: · ragione sociale (BRT S.p.A.)
065800040618      *                   · indirizzo
065810040618      *                   · c.a.p./città/provincia
065820040618      *     => NON con i dati che manda il mittente !!!
065830040618      *        ...Vuoi mai che si sbagli ad inviarci della roba !!!
065840040618     c***                eval      PCvet  = %trim(§AR8v2a)
065850040618     c***                                 + ' - '
065860040618     c***                                 + %trim(§AR8v2b)
065870040618     c***                                 + ' - '
065880040618     c***                                 + %trim(§AR8v2c)
065890120530     c*//                if        (DB0dft > *zero       and
065900120530     c*//                           DB0dft > §vpoBRTdta)  OR
065910120530     c*//                          (DB0dft = *zero       and
065920120530     c*//                           Wdsp   > §vpoBRTdta)
065930110426     c                   eval      PCvet  = $BRT
065940110426     c                                    + ' - '
065950110426     c                                    + $Intest2
065960110426     c                                    + ' (BO)'
065970120530     c*//                else
065980120530     c*//                eval      PCvet  = $Bartolini
065990120530     c*//                                 + ' - '
066000120530     c*//                                 + $Intest2
066010120530     c*//                                 + ' (BO)'
066020120530     c*//                endif
066030040618      * - - data ritiro
066040040618     c*** non stampo:    eval      PCdtv  = §AR8v2d
066050040618      * - - orario ritiro
066060040618     c*** non stampo:    eval      PChmv  = §AR8v2e
066070040618      * - - annotazioni / variazioni
066080040618     c                   eval      PCvea  = §AR8v2f
066090040618      * - - note
066100040618     c                   eval      PCnv1  = §AR8v2g
066110040618     c                   eval      PCnv2  = §AR8v2h
066120040618      *
066130040618     c                   ENDSR
066140040618      *
066150040618      *---------------------------------------------------------------*
066160040618      *
066170050217     c     PrtDdtKARNbV2 BEGSR
066180040618      *
066190040618      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
066200040618      *
066210040618      * - - Calcolo la posizione finale del box
066220040618     c                   exsr      Posiz_DK
066230040618      *
066240040618      * - - Stampa parte finale DDT
066250040618     c                   write     A4prtDK4
066260040618      *
066270040618      * > Stampa logo, box, linee e testi fissi  - - - - - - - - - - -*
066280081017     c                   if            §AR8cxd = *blanks
066290081017     c                             or  DB0fa4  = 'P'
066300050331     c                   write     A4prtDT0
066310050331     c                   endif
066320040618     c                   write     A4prtDK0
066330040618      *
066340040618     c                   ENDSR
066350040607      *
066360040607      *---------------------------------------------------------------*
066370050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
066380050217      *?Dati esterni: riga descrittiva con referente                 ?*
066390040607      *---------------------------------------------------------------*
066400050217     c     PrtDdtKARN_L1 BEGSR
066410040607      *
066420051110     c                   movel(p)  AR8uni        dAR8L1
066430040607      *
066440040607      * > righe descrittive esterne al DDT
066450040607      * - - riga descrittiva
066460040607     c                   eval      PCtxx  = §AR8l1a
066470040607      *
066480040607      * > Stampa riga testo finale DDT - - - - - - - - - - - - - - - -*
066490040618      *   - OverFlow già testato per questa riga -
066500040607     c                   write     A4prtDK5
066510040607      *
066520040607     c                   ENDSR
066530040607      *
066540040607      *---------------------------------------------------------------*
066550050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
066560050217      *?Dati di omaggio: testo per articoli omaggiati                ?*
066570040607      *---------------------------------------------------------------*
066580050217     c     PrtDdtKARN_O1 BEGSR
066590040608      *
066600040608     c                   eval      $DDT_o = *on
066610040607      *
066620040607     c                   clear                   A4prtDK5
066630051110     c                   movel(p)  AR8uni        dAR8O1
066640040607      *
066650040607      * > righe descrittive esterne al DDT
066660040607      * - - riga descrittiva
066670040607     c                   eval      PCtxx  = §AR8o1a
066680040607      * - - nessuna sottolineatura
066690040608     c***                eval      *in39  = *off
066700040607      *
066710040607      * > Stampa testata omaggi in DDT - - - - - - - - - - - - - - - -*
066720040608      *   (se NON già stampata nella nuova pagina x OverFlow)
066730040608     c                   eval      SavLineA4 = CurLineA4
066740040607     c                   exsr      Test_OF
066750040608if  1c                   if        CurLineA4 = SavLineA4
066760040607     c                   write     A4prtDK5
066770040608e   1c                   endif
066780040608      * ...Dovendo ristampare le righe di intestazione degli omaggi
066790040608      *   in ogni pagina (da quella del 1° omaggio in poi), non calcolo
066800040608      *   quelle già stampate...
066810040608     c*** NO:            sub       1             wOmaRowT
066820040607      *
066830040607     c                   ENDSR
066840040607      *
066850040607      *---------------------------------------------------------------*
066860050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
066870050217      *?Dati di omaggio: articolo, descrizione, quantità             ?*
066880040607      *---------------------------------------------------------------*
066890050217     c     PrtDdtKARN_O2 BEGSR
066900040607      *
066910040607     c                   clear                   A4prtDK2
066920051110     c                   movel(p)  AR8uni        dAR8O2
066930040607      *
066940040607      * > dettaglio omaggi DDT
066950040607      * - - codice prodotto
066960040607     c                   eval      PCcpr  = §AR8o2a
066970040607      * - - descrizione prodotto
066980040607     c                   eval      PCdpr  = §AR8o2b
066990040607      * - - quantità
067000040618     c                   eval      PCqta  = '     ' + §AR8o2c
067010040607      * - - note
067020040607     c***                eval      PCnot  = *blanks
067030040607      *
067040040607      * > Stampa dettaglio omaggi in DDT - - - - - - - - - - - - - - -*
067050040607      *
067060040607     c                   exsr      Test_OF
067070040607     c                   write     A4prtDK2
067080040608     c                   sub       1             wOmaRowD
067090040607      *
067100040607     c                   ENDSR
067110040608      *
067120040608      *---------------------------------------------------------------*
067130081002      *?Impostazione nuova pagina di DDT Karnak / Titanedi / EDk     ?*
067140040608      *---------------------------------------------------------------*
067150081002     c     NewPageDK     BEGSR
067160040608      *
067170080923      /free
067180080923         // Impostazione del nuovo limite x overflow
067190080923         select;
067200080923           when  NOT *in45;
067210080923             wMaxLine = $MaxLine3;
067220080923           when  *in45   and   NOT *in46;
067230080923             wMaxLine = $MaxLine3_pl1;
067240080923           when  *in45   and   *in46;
067250080923             wMaxLine = $MaxLine3_pl2;
067260080923         endsl;
067270080923      /end-free
067280080923     c                   write     A4prtSP1
067290040608     c                   write     A4prtDK1
067300040608      *
067310040608if  1c                   if        $DDT_o = *on
067320040608      *
067330040608      * - Stampa 1 riga di dettaglio vuota
067340040608     c                   clear                   A4prtDK3
067350040608     c                   write     A4prtDK3
067360040618      * - Reimposta i dati nel box finale del DDT
067370050217     c                   exsr      PrtDdtKARNaV2
067380040618      * - Stampa fondo DDT
067390050217     c                   exsr      PrtDdtKARNbV2
067400040608      * - Stampa ultima riga
067410040608      *   ("Per qualsiasi problema o chiarimento contattare...)
067420040608     c                   clear                   A4prtDK5
067430040608     c                   eval      PCtxx  = §AR8l1a
067440040608     c                   write     A4prtDK5
067450040608      * - Stampa intestazione per merce omaggio
067460040608      *   xx = numero di righe per "intestazione omaggi"
067470040608     c                   eval      wx     = xx
067480040608do  2c                   dow           xx         >  *zeros
067490040608     c                             and xx         <= %elem($AR8o1)
067500040608     c                             and $AR8o1(xx) <> *blanks
067510040608     c                   eval      PCtxx  = $AR8o1(xx)
067520040608     c                   write     A4prtDK5
067530040608     c                   eval      xx     = xx - 1
067540040608e   2c                   enddo
067550040608     c                   eval      xx     = wx
067560040608      * -Se sta leggendo il tipo record "O1":?
067570040608      *   sposta il ciclo di lettura del file FIAR8 sul record
067580040608      *   successivo all'ultimo (appena stampato) dell'intestazione
067590040608      *   della merce omaggiata
067600040608if  2c                   if        AR8trk =  'O1'
067610040608if  3c                   dou       AR8trk <> 'O1'
067620040608     c     KLDV          reade     FIAR8000
067630040608e   3c                   enddo
067640040608if  3c*** è così:        if        AR8trk = 'O2'
067650040608     c     KLDV          readpe    FIAR8000
067660040707e   3c***                endif
067670040608e   2c                   endif
067680040608      * - Aggiorna controllo se "ultima pagina"
067690080924      /free
067700080924           select;
067710080924             when  NOT *in45;
067720080924      /end-free
067730040608     c                   eval      $DDTlastPg = ($MaxLine2
067740040713     c                                        +  1
067750040608     c                                        -  CurLineA4
067760040608     c                                        -  $BoxRow1
067770040608     c                                        -  wOmaRowT
067780040608     c                                        >= wOmaRowD)
067790080924      /free
067800080924             when  *in45   and   NOT *in46;
067810110406               $DDTlastPg = ($MaxLine2_pl1k
067820080924                             + 1 - CurLineA4 - $BoxRow1 - wOmaRowT
067830080924                             >= wOmaRowD);
067840080924             when  *in45   and   *in46;
067850110406               $DDTlastPg = ($MaxLine2_pl2k
067860080924                             + 1 - CurLineA4 - $BoxRow1 - wOmaRowT
067870080924                             >= wOmaRowD);
067880080924           endsl;
067890080924      /end-free
067900040608      *
067910040608e   1c                   endif
067920040608      *
067930040608     c                   ENDSR
067940040607      *
067950040607      *---------------------------------------------------------------*
067960050414      *?Posizionamento di linee e box nel DDT KARNAK / TITANEDI / EDk?*
067970040607      *---------------------------------------------------------------*
067980040607     c     Posiz_DK      BEGSR
067990040607      *
068000080918      /free
068010080923         // Impostazione posizioni di logo/box
068020080923         // (posizioni di "partenza" diverse a seconda del posizionam.
068030080923         //  iniziale nel foglio A4 attualmente in stampa)
068040080923         select;
068050080923           when  NOT *in45;
068060081002             §PB1 =  3.120;
068070080923           when  *in45   and   NOT *in46;
068080081002             §PB1 = 0.250;
068090080923           when  *in45   and   *in46;
068100081002             §PB1 = 6.000;
068110080923         endsl;
068120080918      /end-free
068130040607     c                   eval      §PB2 =  §PB1 + 1.23
068140080822     c                   eval      §PB3 =  §PB2 + 0.18
068150040607     c                   eval(h)   §PB4 =  CurLineA4 / 8 + 0.20
068160040607     c                   eval      §PB5 =  §PB4 + 0.18
068170040607     c                   eval      §PB6 =  §PB5 + 0.77
068180040607     c                   eval      §PB7 =  §PB5 + 0.165
068190040607     c                   eval      §PB8 =  §PB7 + 0.08
068200040607      * Impostazione posizioni di linee orizzontali nel 1° box
068210040607     c                   eval(h)   §LO1 =  §PB1 + (§PB2 - §PB1) / 2
068220040607      * Impostazione posizioni di linee orizzontali nel 2° box
068230080822     c                   eval      §LO2 =  §PB3 + 0.23
068240080822     c                   eval      §LO3 =  §LO2 + 0.145
068250080822     c                   eval      §LO4 =  §LO3 + 0.10
068260040607     c                   eval      §LO5 =  §PB4 - 0.15
068270040607      * Impostazione posizioni di linee orizzontali nel 3° box
068280040607     c                   eval      §LO6 =  §PB5 + 0.14
068290040607     c                   eval      §LO7 =  §LO6 + 0.12
068300040607     c                   eval      §LO8 =  §LO7 + 0.12
068310040607     c                   eval      §LO9 =  §LO8 + 0.13
068320040607      * Impostazione posizioni di linee verticali nel 2° box
068330040607     c                   eval      §LV1 =  §LO2 - §PB3
068340040607     c                   eval      §LV2 =  §LO3 - §LO2
068350040607     c                   eval      §LV3 =  §LO5 - §LO3
068360040607      * Impostazione posizioni di linee verticali nel 3° box
068370040607     c                   eval      §LV4 =  §LO6 - §PB5
068380040607     c                   eval      §LV5 =  §LO7 - §LO6
068390040607     c                   eval      §LV6 =  §LO8 - §LO7
068400040607     c                   eval      §LV7 =  §PB6 - §LO9
068410040607      * Impostazione posizioni di campi
068420040607     c                   eval      §PS1 =  §LO5 + 0.11
068430040607     c                   eval      §PS2 =  §PB4 + 0.08
068440040607     c                   eval      §PS3 =  §PS2 + 0.07
068450130118if  1c*//                if        §PB6 <  §PB1 + 4.58
068460130118     c*//                eval      §PR1 =  §PB1 + 4.58
068470130118if  1c                   if        §PB6 <  §PB1 + 3.30
068480130118     c                   eval      §PR1 =  §PB1 + 3.30
068490040607x   1c                   else
068500040607     c                   eval      §PR1 =  §PB6
068510040607e   1c                   endif
068520040607      *
068530040607     c                   ENDSR
068540041019      *
068550041019      *---------------------------------------------------------------*
068560050414      *?Stampa DDT del mittente LOGISTICA / SAIT ABRASIVI            ?*
068570041019      *---------------------------------------------------------------*
068580050218     c     DDT_Logistica BEGSR
068590041019      *
068600080924      /free
068610080924         // Impostazione del nuovo limite x overflow
068620080924         // (Codesto limite è valido solo per la stampa del dettaglio.
068630080924         //   Per la stampa del "fondo" bolla occorre REimpostarlo ad
068640080924         //   un valore più basso.
068650080924         //  Ovvero: stampo più righe di dettaglio che posso; se alla
068660080924         //  fine delle righe di dettaglio non rimane spazio a suffi-
068670080924         //  cienza per il fondo: prima della stampa del fondo si salta
068680080924         //  alla pagina successiva, dove non si stampa alcuna riga di
068690080924         //  dettaglio.)
068700080924         select;
068710080924           when  NOT *in45;
068720090323             wMaxLine = $MaxLine2;
068730080924           when  *in45   and   NOT *in46;
068740090323             wMaxLine = $MaxLine2_pl1;
068750080924           when  *in45   and   *in46;
068760090323             wMaxLine = $MaxLine2_pl2;
068770080924         endsl;
068780080924
068790080924         // Posizionamento iniziale all'interno del foglio A4
068800080924         write  A4prtSP1;
068810081006
068820081006         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
068830081006           write  A4prtNUL;
068840081006         endif;
068850080924      /end-free
068860041019      *
068870041019      * Impostazione testi fissi - - - - - - - - - - - - - - - - - - -*
068880041019      *
068890071008      * - prima solo per "Logistica", ora neanche più per lei...
068900071008      * - Sait Abrasivi SpA & Brumar Srl hanno Tipo Stampa DDT = "LC"
068910071008if  1c***                if        §AR8tsd <> 'LC'
068920071008     c***                eval      PCLtx1 = 'La merce viagga a rischio -
068930071008     c***                          e pericolo del Committente, pertanto -
068940071008     c***                          la Venditrice non risponde di -
068950071008     c***                          eventuali perdite o avarie derivanti -
068960071008     c***                          dal trasporto.'
068970071008     c***                eval      PCLtx2 = 'Il Destinatario, per -
068980071008     c***                          ottenere il risarcimento del danno, -
068990071008     c***                          deve far constatare al Corriere la -
069000071008     c***                          perdita o l''avaria all''atto del -
069010071008     c***                          ricevimento della merce.'
069020071008e   1c***                endif
069030050218      *
069040130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
069050130315      *  "StampaDDT")
069060050218     c***  KLDV          setll     FIAR8000       (già fatto)
069070050218     c***  KLDV          reade     FIAR8000       (già fatto)
069080041019      *
069090041019      * Reperimento dati, impostazione e stampa DDT mittente  - - - -*
069100041019      *
069110041019if  1c                   dow       NOT %eof(FIAR801L)
069120050318      *
069130050318if  2c                   if        AR8atb   = *blanks
069140050318     c                   eval      SAVtrk   = AR8trk
069150050318e   2c                   endif
069160041019      *
069170041019sel 2c                   select
069180041019      * - Record annullato (non si stampa nulla)
069190050218w   2c                   when      AR8atb  <> *blanks
069200041019      * - Dati di dettaglio: articolo, descrizione, quantità
069210050218w   2c                   when      AR8trk   = 'C1'
069220050217     c                   exsr      PrtDdtLOG1_C1
069230041103      * - Dati di dettaglio: descrizioni generiche
069240050218w   2c                   when      AR8trk   = 'C2'
069250050217     c                   exsr      PrtDdtLOG1_C2
069260041103      * - Dati di dettaglio: descrizioni generiche
069270050218w   2c                   when      AR8trk   = 'C3'
069280050217     c                   exsr      PrtDdtLOG1_C3
069290060614      * - Dati di dettaglio: corpo bolla non fincato
069300060614w   2c                   when      AR8trk   = 'C4'
069310060614     c                   exsr      PrtDdtLOG1_C4
069320090323      * - Dati di dettaglio: descrizioni generiche
069330090323w   2c                   when      AR8trk   = 'C5'
069340090323     c                   exsr      PrtDdtLOG1_C5
069350050318      * - Dati di packing-list: dettaglio
069360050318w   2c                   when      AR8trk   = 'Z1'
069370050318     c                   exsr      PrtDdtLOG1_Z1
069380041019      * - Dati di testata: sito e depositante
069390050218w   2c                   when      AR8trk   = 'T1'
069400050217     c                   exsr      PrtDdtLOG1_T1
069410041019      * - Dati di testata: destinatario, cliente di fatturazione
069420050218w   2c                   when      AR8trk   = 'T2'
069430050217     c                   exsr      PrtDdtLOG1_T2
069440041019      * - Dati di testata: agente, numero ordine, estremi documento
069450050218w   2c                   when      AR8trk   = 'T3'
069460050217     c                   exsr      PrtDdtLOG1_T3
069470041019      * - Dati di fondo: mezzo e causale trasporto, colli, peso
069480050218w   2c                   when      AR8trk   = 'P1'
069490050217     c                   exsr      PrtDdtLOG1_P1
069500050606      * - Dati di testata: FATTURA
069510050606w   2c                   when      AR8trk   = 'S1'
069520050606     c                   exsr      PrtDdtLOG1_S1
069530050606      * - Dati di dettaglio cpi 15: FATTURA
069540050606w   2c                   when      AR8trk   = 'S2'
069550050606     c                   exsr      PrtDdtLOG1_S2
069560050606      * - Dati di dettaglio cpi 20: FATTURA
069570050606w   2c                   when      AR8trk   = 'S3'
069580050606     c                   exsr      PrtDdtLOG1_S3
069590050606      * - Dati di fondo: FATTURA
069600050606w   2c                   when      AR8trk   = 'S4'
069610050606     c                   exsr      PrtDdtLOG1_S4
069620050606      * - Dati di fondo note: FATTURA
069630050606w   2c                   when      AR8trk   = 'S5'
069640050606     c                   exsr      PrtDdtLOG1_S5
069650041019e   2c                   endsl
069660041019      *
069670041019     c     KLDV          reade     FIAR8000
069680041019      *
069690041019e   1c                   enddo
069700050318      *
069710050318      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
069720050318      *
069730050505      * - se DDT con Packing-List:
069740050505      *   (altrimenti il box è già stato stampato in sr PrtDdtLOG1_P1)
069750050505if  1c                   if        %subst(SAVtrk:1:1) =  'Z'
069760050318      *
069770050318      * - - Calcolo la posizione finale del box e di linee separatrici
069780050318     c                   exsr      Posiz_DL
069790050318      *
069800050318      * > Stampa box, linee e testi fissi (no logo)  - - - - - - - - -*
069810081017     c                   if            §AR8cxd = *blanks
069820081017     c                             or  DB0fa4  = 'P'
069830050331     c                   write     A4prtDT0
069840050331     c                   endif
069850050318     c                   write     A4prtDL0C
069860050505      *
069870050505e   1c                   endif
069880050218      *
069890050218      * Reimpostazione del vecchio limite x overflow
069900080925      /free
069910080925         select;
069920080925           when  NOT *in45;
069930090323             wMaxLine = $MaxLine2;
069940080925           when  *in45   and   NOT *in46;
069950090323             wMaxLine = $MaxLine2_pl1;
069960080925           when  *in45   and   *in46;
069970090323             wMaxLine = $MaxLine2_pl2;
069980080925         endsl;
069990080925      /end-free
070000041019      *
070010041019     c                   ENDSR
070020081222
070030081222      /free
070040081222       //--------------------------------------------------------------
070050140303       //?Preparazione simil-logo Bartolini                            ?
070060081222       //--------------------------------------------------------------
070070090406       BEGSR  PrtDdtLOG1_SimilLogo;
070080081222
070090120530         //If  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
070100120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
070110120530         //
070120120530         //  // -?Preparazione simil-Logo BARTOLINI?
070130120530         //  §PLBLS  = §PBA + 0.150;
070140120530         //  §PLBLVS = 0.400;
070150120530         //  exsr  sr_SimilLogoBAR;
070160120530         //
070170120530         //  // -?Preparazione simil-Logo BRT?
070180120530         //  §P2blS  = §PlblS;
070190120530         //  //§P2blVS = §PlblVD + 0.050;
070200120530         //  §P2blVS = §PlblVD + 0.030;
070210120530         //  exsr  sr_SimilLogoBRT;
070220120530         //
070230120530         //Else;
070240120530         //
070250120530         //  // -?Preparazione simil-Logo BARTOLINI?
070260120530         //  §PLBLS  = §PBA + 0.050;
070270120530         //  §PLBLVS = 0.400;
070280120530         //  exsr  sr_SimilLogoBART;
070290120530         //
070300120530         //EndIf;
070310120530
070320120530         // -?Preparazione simil-Logo BRT?
070330120530         §PLBLS  = §PBA + 0.050;
070340120530         §PLBLVS = 0.400;
070350120530         exsr  sr_SimilLogoBRT_;
070360090406
070370120530         // -?Impostazione dati anagrafici BRT alla sua destra?
070380090422         §PC0    = §PBA    + 0.070 + 0.050;
070390090406         §PC1    = §PC0    + 0.100;
070400090406         §PC2    = §PC1    + 0.100;
070410090406         §PC3    = §PC2    + 0.090;
070420090406         §PC4    = §PC3    + 0.090;
070430090406         §PC5    = §PC4    + 0.090;
070440120530
070450120530         //§PLC    = 1.400;
070460120530         §PLC    = 1.000;
070470090406
070480120530         // -?"TBAR" è già correttamente impostato.?
070490120530         //TBAR    = $Bartolini;        (già così)
070500120530         //TBAR    = $BRT;              (già così)
070510090406         LBA1    = $Intest1;
070520090406         LBA2    = $Intest2_b;
070530120530         //LBA3    = $Intest3_b;
070540120530         LBA3    = $Intest3_b_brt;
070550090406         LBA4    = $Intest4_b;
070560090406         LBA5    = $Intest5_b;
070570081222
070580081222       ENDSR;
070590081222      /end-free
070600050214      *
070610041019      *---------------------------------------------------------------*
070620050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
070630050217      *?Dati di testata: sito e depositante                          ?*
070640041019      *---------------------------------------------------------------*
070650050217     c     PrtDdtLOG1_T1 BEGSR
070660041019      *
070670041019      * > logo
070680050218     c                   eval      §Logo    = 'LOGO' + AR8tmd
070690090107      /free
070700090107         // - Impostazione indicatore che condiziona la stampa del logo
070710120530         //   (o quella del "simil-logo" BRT)
070720090107         *in55 = (%subst(AR8tmd : 1 : 3) = 'LOG'          and
070730090107                  (§AR8tsd  = 'L1'  or  §AR8tsd  = 'L3'));
070740090107      /end-free
070750080908      *
070760060609      *? L O G O !! ?
070770060609      * per pulizia non vogliamo più avere tanti LOGx per ogni cliente che
070780060609      * vuole la stampa della fattura con il proprio logo e la bolla con
070790120530      * il logo BRT
070800060609      * anche Cappellini è d'accordo ad inviare i dati con il logo del
070810060609      * cliente da stampare nella LDV
070820060609      * quelli già esistenti avranno un nuovo logo un po' più piccolo per
070830071008      * problemi di spazio e i dati saranno inviati con AR8tmd = al nome del
070840071008      * nuovo logo e la tabella sarà fatta con §AR8tsd = 'L2' o 'L4'
070850060609      * i nuovi faremo in modo di avere o lo stesso logo oppure la stessa
070860060609      * logica descritta prima
070870060609      * ma può essere che un cliente voglia la fattura con il suo logo
070880120530      * mentre la bolla con il logo BRT quindi devo recuperare
070890060609      * il logo da stampare nella bolla non più da AR8TMD, che conterrà
070900060609      * il nome del logo da stampare nella fattura, ma dal campo del
070910120530      * del logo fattura, che conterrà il logo BRT, mentre il campo
070920071008      * §AR8tsd nella tabella sarà 'L1' o 'L3'
070930071008     c                   if        (§AR8tsd = 'L1' or §AR8tsd = 'L3') and
070940071008     c                             §AR8logof <> *blanks and
070950071008     c                             %subst(AR8tmd:1:3) <> 'LOG'
070960071008     c                   eval      §Logo    = 'LOGO' + §AR8logof
070970090107      /free
070980090107           // - Impostaz. indicatore che condiziona la stampa del logo
070990120530           //   (o quella del "simil-logo" BRT)
071000090107           *in55 = (%subst(§AR8logof : 1 : 3) = 'LOG');
071010090107      /end-free
071020060609     c                   endif
071030041019      *
071040080822     c                   movel(p)  AR8uni        dAR8LOG1T1
071050050414      *
071060060627if  1c                   if            §AR8tsd <> 'LC'
071070041019      *
071080041019      * > sito
071090060407     c                   select
071100071008     c                   when      §AR8tsd = 'L1' or
071110071008     c                             §AR8tsd = 'L3'
071120050707     c                   eval      PLOG1st0 = 'Deposito di:'
071130071008     c                   when      §AR8tsd = 'L2' or
071140071008     c                             §AR8tsd = 'L4'
071150060407     c                   eval      PLOG1st0 = 'Depositario:'
071160060407     c                   endsl
071170050707     c                   eval      PLOG1st1 = §A8LOG1t1a
071180050707     c                   eval      PLOG1st2 = §A8LOG1t1b
071190050707     c                   eval      PLOG1st3 = §A8LOG1t1c
071200050707     c                   eval      PLOG1st4 = §A8LOG1t1d
071210050707     c                   eval      PLOG1st5 = §A8LOG1t1e
071220041019      *
071230041019      * > depositante
071240060407     c                   select
071250071008     c                   when      §AR8tsd = 'L1' or
071260071008     c                             §AR8tsd = 'L3'
071270050707     c                   eval      PLOG1de0 = 'Depositario della:'
071280071008     c                   when      §AR8tsd = 'L2' or
071290071008     c                             §AR8tsd = 'L4'
071300060407     c                   eval      PLOG1de0 = 'Deposito di:'
071310060407     c                   endsl
071320050707     c                   eval      PLOG1de1 = §A8LOG1t1f
071330050707     c                   eval      PLOG1de2 = §A8LOG1t1g
071340050707     c                   eval      PLOG1de3 = §A8LOG1t1h
071350050707     c                   eval      PLOG1de4 = §A8LOG1t1i
071360050707     c                   eval      PLOG1de5 = §A8LOG1t1j
071370050414      *
071380050414x   1c                   else
071390050414      *
071400050707     c***                eval      PLOG1st0 = *blanks
071410050707     c                   eval      PLOG1st1 = §A8LOG1t1a + §A8LOG1t1b
071420050707     c                   eval      PLOG1st2 = §A8LOG1t1c + §A8LOG1t1d
071430050707     c                   eval      PLOG1st3 = §A8LOG1t1e + §A8LOG1t1f
071440050707     c                   eval      PLOG1st4 = §A8LOG1t1g + §A8LOG1t1h
071450050707     c                   eval      PLOG1st5 = §A8LOG1t1i + §A8LOG1t1j
071460050414      *
071470050414e   1c                   endif
071480050214      *
071490041022     c                   ENDSR
071500050214      *
071510041019      *---------------------------------------------------------------*
071520050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
071530050217      *?Dati di testata: destinatario, cliente di fatturazione       ?*
071540041019      *---------------------------------------------------------------*
071550050217     c     PrtDdtLOG1_T2 BEGSR
071560041019      *
071570051110     c                   clear                   dAR8LOG1T2
071580050218     c                   movel     AR8uni        dAR8LOG1T2
071590041019      *
071600041019      * > destinatario
071610050707if  1c                   if        §A8LOG1T2a <> *blanks
071620050707     c                   eval      PLOG1ccd   =  §A8LOG1T2a
071630050218e   1c                   endif
071640050707if  1c                   if        §A8LOG1T2c =  *blanks
071650050707     c                   clear                   PLOG1rsd
071660050707     c                   eval      PLOG1rsd2  =  §A8LOG1T2b
071670050218x   1c                   else
071680050707     c                   eval      PLOG1rsd   =  §A8LOG1T2b
071690050707     c                   eval      PLOG1rsd2  =  §A8LOG1T2c
071700050218e   1c                   endif
071710050707     c                   eval      PLOG1ind   =  §A8LOG1T2d
071720050707     c                   eval      PLOG1ind2  =  §A8LOG1T2e
071730050707if  1c                   if        §A8LOG1T2f >  *zeros
071740050707     c                   eval      PLOG1cad   =  §A8LOG1T2f
071750041019e   1c                   endif
071760050707     c                   eval      PLOG1lod   =  §A8LOG1T2g
071770041019      * > cliente di fatturazione
071780050707if  1c                   if        §A8LOG1T2h <> *blanks
071790050707     c                   eval      PLOG1ccf   =  §A8LOG1T2h
071800041019e   1c                   endif
071810050707if  1c                   if        §A8LOG1T2j =  *blanks
071820050707     c                   clear                   PLOG1rsf
071830050707     c                   eval      PLOG1rsf2  =  §A8LOG1T2i
071840050218x   1c                   else
071850050707     c                   eval      PLOG1rsf   =  §A8LOG1T2i
071860050707     c                   eval      PLOG1rsf2  =  §A8LOG1T2j
071870050218e   1c                   endif
071880050707     c                   eval      PLOG1inf   =  §A8LOG1T2k
071890050707     c                   eval      PLOG1inf2  =  §A8LOG1T2l
071900050707if  1c                   if        §A8LOG1T2m >  *zeros
071910050707     c                   eval      PLOG1caf   =  §A8LOG1T2m
071920041019e   1c                   endif
071930050707     c                   eval      PLOG1lof   =  §A8LOG1T2n
071940041019      *
071950041019     c                   ENDSR
071960041019      *
071970041019      *---------------------------------------------------------------*
071980050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
071990050217      *?Dati di testata: documento, rif.ordine, note                 ?*
072000041019      *---------------------------------------------------------------*
072010050217     c     PrtDdtLOG1_T3 BEGSR
072020041019      *
072030051110     c                   clear                   dAR8LOG1T3
072040050218     c                   movel     AR8uni        dAR8LOG1T3
072050050706      *
072060050706      * TEST effettiva numericità dei campi previsti numerici:
072070100513     c                   clear                   w053a
072080111013     ***c                   eval      WX    =  %len(§A8LOG1T3b)
072090111013     ***c                                   +  %len(§A8LOG1T3c)
072100111013     ***c                                   +  %len(§A8LOG1T3d)
072110111013     ***c                   eval      w053a =  %subst(dAR8LOG1T3: 4:WX)
072120111013     ***c                   eval      *in31 =  *off
072130111013     ***c     c_Digits      checkr    w053a :  WX                            31
072140111013if  1***c                   if        *in31
072150111013     ***c                   clear                   §A8LOG1T3b
072160111013     ***c                   clear                   §A8LOG1T3c
072170111013     ***c                   clear                   §A8LOG1T3d
072180111013e   1***c                   endif
072190121116     ***c                   eval      WX    =  %len(§A8LOG1T3b)
072200121116     ***c                   eval      w053a =  %subst(dAR8LOG1T3: 4:WX)
072210121116     ***c                   eval      *in31 =  *off
072220121116     ***c     c_Digits      checkr    w053a :  WX                            31
072230121116if  1***c                   if        *in31
072240121116     ***c                   clear                   §A8LOG1T3b
072250121116e   1***c                   endif
072260111013     c                   clear                   w053a
072270111013     c                   eval      WX    =  %len(§A8LOG1T3d)
072280111013     c                   eval      w053a =  %subst(dAR8LOG1T3:15:WX)
072290111013     c                   eval      *in31 =  *off
072300111013     c     c_Digits      checkr    w053a :  WX                            31
072310111013if  1c                   if        *in31
072320111013     c                   clear                   §A8LOG1T3d
072330111013e   1c                   endif
072340041019      *
072350041019      * > dati in testata DDT
072360041019      * - - numero e data documento (DDT)
072370050707     c                   eval      PLOG1nsr = §A8LOG1T3a
072380121116     ***c                   eval      PLOG1aas = §A8LOG1T3b
072390121116     c                   if        §A8Log1T3b <> *blanks   and
072400121116     c                             §A8Log1T3b <> *zeros
072410121116     c                   eval      PLOG1aas = §A8LOG1T3b
072420121116     c                   endif
072430111013     ***c                   if        §A8Log1T3c <> *zeros
072440111013     ***c                   eval      PLOG1nsp = %editc(§A8LOG1T3c:'X')
072450111013     ***c                   endif
072460111013     c                   if        §A8Log1T3c <> *blanks   and
072470111013     c                             §A8Log1T3c <> *zeros
072480111013     c                   eval      PLOG1nsp = §A8LOG1T3c
072490111013     c                   endif
072500100319     c                   if        §A8Log1T3a <> *blanks   and
072510121116     c                             §A8Log1T3b <> *blanks   and
072520100319     c                             §A8Log1T3b <> *zeros
072530100319     c                   eval      PLog1sepA = '/'
072540100319     c                   endif
072550121115     c                   if        §A8Log1T3b <> *zeros    and
072560121115     c                             §A8Log1T3c <> *blanks   and
072570100319     c                             §A8Log1T3c <> *zeros
072580100319     c                   eval      PLog1sepB = '/'
072590100319     c                   endif
072600110707      *     giro la data del documento
072610050707if  1c                   if        §A8LOG1T3d > *zeros
072620060922     c                   eval      *in31 =  *off
072630060922     c     *iso          test(d)                 §A8LOG1T3d             31
072640060922if  2c                   if        NOT *in31
072650050707     c     *iso          movel     §A8LOG1T3d    Data_EUR
072660050707     c     *eur          movel     Data_EUR      PLOG1dsp
072670060922x   2c                   else
072680060922     c                   clear                   PLOG1dsp
072690060922e   2c                   endif
072700050707e   1c                   endif
072710041019      * - - riferimento ordine
072720050707     c                   eval      PLOG1ref = §A8LOG1T3e
072730041019      * - - note consegna
072740050707     c                   eval      PLOG1not = §A8LOG1T3f
072750050707     c                   eval      PLOG1nt2 = §A8LOG1T3g
072760060612      * - - intestazione corpo bolla per Logistica tipo stampa L3 o L4
072770060922     c                   if        §AR8tsd = 'L3' or §AR8tsd = 'L4'
072780060612     c                   eval      *in23 = *On
072790060612     c                   eval      PLOG1int = §A8LOG1T3h
072800060612     c                   else
072810060612     c                   eval      *in23 = *off
072820060612     c                   endif
072830041019      *
072840041019      * > Stampa testata DDT - - - - - - - - - - - - - - - - - - - - -*
072850041019      *
072860041019      *   - OverFlow impossibile qui! -
072870050218     c***                exsr      Test_OF
072880050318     c                   exsr      Posiz_DL
072890081017     c                   if            §AR8cxd = *blanks
072900081017     c                             or  DB0fa4  = 'P'
072910050331     c                   write     A4prtDT0
072920050331     c                   endif
072930050218     c                   write     A4prtDL0
072940081222      /free
072950081222         if  *in55;
072960090406           exsr   PrtDdtLOG1_SimilLogo;
072970120530           //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
072980120530           //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
072990120530           //  write  A4LogoBR2;
073000120530           //else;
073010120530           //  write  A4LogoBAR;
073020120530           //endif;
073030120530           write  A4LogoBRT;
073040081222           write  A4prtDLL;
073050081222         endif;
073060081222      /end-free
073070050218     c*** quante righe?  write     A4prtDL0A
073080041028     c                   write     A4prtDL1A
073090050218     c                   write     A4prtDL1B
073100041019      *
073110041019     c                   ENDSR
073120050214      *
073130041019      *---------------------------------------------------------------*
073140050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
073150050217      *?Dati di dettaglio: articolo, quantità, descrizione           ?*
073160041019      *---------------------------------------------------------------*
073170050217     c     PrtDdtLOG1_C1 BEGSR
073180041019      *
073190041019     c                   clear                   A4prtDL2
073200051110     c                   clear                   dAR8LOG1C1
073210050218     c                   movel     AR8uni        dAR8LOG1C1
073220050706      *
073230050706      * TEST effettiva numericità dei campi previsti numerici:
073240100513     c                   clear                   w053a
073250050706     c                   eval      WX    =  %len(§A8LOG1C1b)
073260100513     c                   eval      w053a =  %subst(dAR8LOG1C1:21:WX)
073270050706     c                   eval      *in31 =  *off
073280110426     c     c_Digits      checkr    w053a :  WX                            31
073290050706if  1c                   if        *in31
073300050706     c                   clear                   §A8LOG1C1b
073310050706e   1c                   endif
073320041019      *
073330041019      * > dati in dettaglio DDT
073340041019      * - - codice prodotto
073350050707     c                   eval      PLOG1cpr  = §A8LOG1C1a
073360041019      * - - quantità
073370050707     c                   eval      PLOG1qta  = §A8LOG1C1b
073380041019      * - - descrizione prodotto
073390050707     c                   eval      PLOG1dpr  = §A8LOG1C1c
073400041019      *
073410041019      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
073420041019      *
073430041019     c                   exsr      Test_OF
073440041019     c                   write     A4prtDL2
073450041019      *
073460041019     c                   ENDSR
073470041019      *
073480041019      *---------------------------------------------------------------*
073490050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
073500050217      *?Dati di dettaglio: descrizione                               ?*
073510041019      *---------------------------------------------------------------*
073520050217     c     PrtDdtLOG1_C2 BEGSR
073530041019      *
073540041019     c                   clear                   A4prtDL2
073550051110     c                   clear                   dAR8LOG1C2
073560050218     c                   movel     AR8uni        dAR8LOG1C2
073570041019      *
073580041019      * > dati in note DDT
073590041019      * - - riga descrittiva
073600050707     c                   eval      PLOG1dpr  = §A8LOG1C2a
073610041019      *
073620041019      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
073630041019      *
073640041019     c                   exsr      Test_OF
073650041019     c                   write     A4prtDL2
073660041019      *
073670041019     c                   ENDSR
073680041019      *
073690041019      *---------------------------------------------------------------*
073700050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
073710050217      *?Dati di dettaglio: descrizioni                               ?*
073720041019      *---------------------------------------------------------------*
073730050217     c     PrtDdtLOG1_C3 BEGSR
073740041019      *
073750041019     c                   clear                   A4prtDL3
073760051110     c                   clear                   dAR8LOG1C3
073770050707     c                   movel     AR8uni        dAR8LOG1C3
073780041019      *
073790041019      * > dati in note DDT
073800041019      * - - riga descrittiva
073810050707     c                   eval      PLOG1txt  = §A8LOG1C3a
073820041019      *
073830041019      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
073840041019      *
073850041019     c                   exsr      Test_OF
073860041019     c                   write     A4prtDL3
073870041019      *
073880041019     c                   ENDSR
073890060614      *
073900060614      *---------------------------------------------------------------*
073910060614      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
073920060614      *?Dati di dettaglio: corpo bolla non fincato                   ?*
073930060614      *---------------------------------------------------------------*
073940060614     c     PrtDdtLOG1_C4 BEGSR
073950060614      *
073960060614     c                   clear                   A4prtDL21
073970060614     c                   clear                   dAR8LOG1C4
073980060614     c                   movel     AR8uni        dAR8LOG1C4
073990060614      *
074000060614      * > dati DDT
074010060614      * - - riga descrittiva
074020060614     c                   eval      PLOG1dpr1 = §A8LOG1C4a
074030060614      *
074040060614      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
074050060614      *
074060060614     c                   exsr      Test_OF
074070060614     c                   write     A4prtDL21
074080060614      *
074090060614     c                   ENDSR
074100090323      *
074110090323      *---------------------------------------------------------------*
074120090323      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
074130090323      *?Dati di dettaglio: descrizioni                               ?*
074140090323      *---------------------------------------------------------------*
074150090323     c     PrtDdtLOG1_C5 BEGSR
074160090323      *
074170090323     c                   clear                   A4prtDL31
074180090323     c                   clear                   dAR8LOG1S3
074190090323     c                   movel     AR8uni        dAR8LOG1S3
074200090323      *
074210090323      * > dati in note DDT
074220090323      * - - riga descrittiva
074230090323     c                   eval      PLOG1txt2 = §A8LOG1S3a
074240090323      *
074250090323      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
074260090323      *
074270090323     c                   exsr      Test_OF
074280090323     c                   write     A4prtDL31
074290090323      *
074300090323     c                   ENDSR
074310041019      *
074320041019      *---------------------------------------------------------------*
074330050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
074340050217      *?Dati di fondo: beni, colli, peso, volume, c/a, porto, trasp. ?*
074350041019      *---------------------------------------------------------------*
074360050217     c     PrtDdtLOG1_P1 BEGSR
074370041019      *
074380051110     c                   clear                   dAR8LOG1P1
074390050707     c                   movel     AR8uni        dAR8LOG1P1
074400050706      *
074410050706      * TEST effettiva numericità dei campi previsti numerici:
074420100513     c                   clear                   w053a
074430050706     c                   eval      WX    =  %len(§A8LOG1P1b)
074440050706     c                                   +  %len(§A8LOG1P1c)
074450050706     c                                   +  %len(§A8LOG1P1d)
074460100513     c                   eval      w053a =  %subst(dAR8LOG1P1:42:WX)
074470050706     c                   eval      *in31 =  *off
074480110426     c     c_Digits      checkr    w053a :  WX                            31
074490050706if  1c                   if        *in31
074500050706     c                   clear                   §A8LOG1P1b
074510050706     c                   clear                   §A8LOG1P1c
074520050706     c                   clear                   §A8LOG1P1d
074530050706e   1c                   endif
074540100513     c                   clear                   w053a
074550050706     c                   eval      WX    =  %len(§A8LOG1P1f)
074560100513     c                   eval      w053a =  %subst(dAR8LOG1P1:67:WX)
074570050706     c                   eval      *in31 =  *off
074580110426     c     c_Digits      checkr    w053a :  WX                            31
074590050706if  1c                   if        *in31
074600050706     c                   clear                   §A8LOG1P1f
074610050706e   1c                   endif
074620041019      *
074630041019      * > dati in chiusura DDT
074640041019      * - - aspetto esteriore dei beni
074650050707     c                   eval      PLOG1aeb  = §A8LOG1P1a
074660041019      * - - numero colli
074670050707     c                   eval      PLOG1ncl  = §A8LOG1P1b
074680041019      * - - peso in kg
074690050707     c                   eval      PLOG1pkg  = §A8LOG1P1c
074700041019      * - - volume in mc
074710050707     c                   eval      PLOG1vlm  = §A8LOG1P1d
074720041019      * - - c/assegno
074730050707     c                   eval      PLOG1cva  = §A8LOG1P1e
074740050707     c                   eval      PLOG1cas  = §A8LOG1P1f
074750041020      * - - causale trasporto
074760050707     c                   eval      PLOG1ctr  = §A8LOG1P1g
074770041020      * - - porto
074780050707     c                   eval      PLOG1pto  = §A8LOG1P1h
074790041020      * - - trasporto a cura
074800050707     c                   eval      PLOG1tra  = §A8LOG1P1i
074810041020      * - - trasportatore
074820050707     c                   eval      PLOG1vet  = §A8LOG1P1j
074830050505      *
074840050505      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
074850050505      *
074860050505      * - - Verifica OverFlow
074870080925      /free
074880080925           select;
074890080925             when  NOT *in45;
074900080925               wMaxLine = $MaxLine4;
074910080925             when  *in45   and   NOT *in46;
074920080925               wMaxLine = $MaxLine4_pl1;
074930080925             when  *in45   and   *in46;
074940080925               wMaxLine = $MaxLine4_pl2;
074950080925           endsl;
074960080925      /end-free
074970050505     c                   exsr      Test_OF
074980050505      * - - Calcolo della posizione finale di box e linee separatrici
074990050505     c                   exsr      Posiz_DL
075000050505      * - - Stampa parte finale DDT
075010050505     c                   write     A4prtDL4
075020050505      *
075030050505      * > Stampa box, linee e testi fissi (no logo)  - - - - - - - - -*
075040090331     c                   if            §AR8cxd = *blanks
075050090331     c                             or  DB0fa4  = 'P'
075060090331     c                   write     A4prtDT0
075070090331     c                   endif
075080050505     c                   write     A4prtDL0A
075090050505     c                   write     A4prtDL0B
075100050505      *
075110050505      * Azzero il numero di pagina per l'eventuale Packing-List
075120050505      *   (successiva)
075130050505     c                   clear                   Page
075140041019      *
075150041019      * N.B.: la copia per il destinatario della LDV deve comparire
075160041019      *       nell'ultima pagina del DDT per il cliente (vedi subr.
075170041019      *       "ChiusuraLDV")
075180041019      *
075190041019     c                   ENDSR
075200050318      *
075210050318      *---------------------------------------------------------------*
075220050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
075230050318      *?Dati di packing-list: dettaglio                              ?*
075240050318      *---------------------------------------------------------------*
075250050318     c     PrtDdtLOG1_Z1 BEGSR
075260050318      *
075270050318      * Prima della 1ª riga di packing-list: testata packing-list
075280050505if  1c                   if        Page     = *zeros
075290050318     c                   exsr      PrtPckLst_1
075300050318e   1c                   endif
075310050318      *
075320050318     c                   clear                   A4prtDL5
075330051110     c                   clear                   dAR8LOG1Z1
075340050707     c                   movel     AR8uni        dAR8LOG1Z1
075350050706      *
075360050706      * TEST effettiva numericità dei campi previsti numerici:
075370100513     c                   clear                   w053a
075380050706     c                   eval      WX    =  %len(§A8LOG1Z1d)
075390100513     c                   eval      w053a =  %subst(dAR8LOG1Z1:53:WX)
075400050706     c                   eval      *in31 =  *off
075410110426     c     c_Digits      checkr    w053a :  WX                            31
075420050706if  1c                   if        *in31
075430050706     c                   clear                   §A8LOG1Z1d
075440050706e   1c                   endif
075450050318      *
075460050318      * > dati in dettaglio Packing-List
075470050318      * - - sscc
075480050318     c                   eval      PCsscc   = §A8LOG1Z1a
075490050318      * - - gtin
075500050318     c                   eval      PCgtin   = §A8LOG1Z1b
075510050318      * - - codice prodotto
075520050318     c                   eval      PCcpr2   = §A8LOG1Z1c
075530050318      * - - quantità ul
075540050318     c                   eval      PLOG1QTA = §A8LOG1Z1d
075550050318      * - - tipo contenitore
075560050318     c                   eval      PCtcn    = §A8LOG1Z1e
075570050318      * - - codice contenitore
075580050318     c                   eval      PCccn    = §A8LOG1Z1f
075590050318      * - - numero lotto
075600050318     c                   eval      PClot    = §A8LOG1Z1g
075610050318      * - - data scadenza
075620050318     c                   eval      PCdts    = §A8LOG1Z1h
075630050318      *
075640050318      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
075650050318      *
075660050318     c                   exsr      Test_OF
075670050318     c                   write     A4prtDL5
075680050318      *
075690050318     c                   ENDSR
075700080908      *
075710050606      *---------------------------------------------------------------*
075720050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
075730080925      *?Dati di Fattura: Testata                                     ?*
075740050606      *---------------------------------------------------------------*
075750050606     c     PrtDdtLOG1_S1 BEGSR
075760050606      *
075770050606      * > logo Fattura
075780050606     c                   eval      §LogoF   = 'LOGO' + §AR8logof
075790090107      /free
075800090107         // - Impostazione indicatore che condiziona la stampa del logo
075810120530         //   (o quella del "simil-logo" BRT)
075820090107         *in55 = (%subst(§AR8logof : 1 : 3) = 'LOG'       and
075830090107                  (§AR8tsd  = 'L1'  or  §AR8tsd  = 'L3'));
075840090107      /end-free
075850080908      *
075860060609      *? L O G O !! ?
075870060609      *?vedi stampa testata bolla ?
075880071008     c                   if        (§AR8tsd = 'L1' or §AR8tsd = 'L3') and
075890071008     c                             §AR8logof <> *blanks and
075900071008     c                             %subst(AR8tmd:1:3) <> 'LOG'
075910071008     c                   eval      §LogoF   = 'LOGO' + AR8tmd
075920090107      /free
075930090107           // - Impostaz. indicatore che condiziona la stampa del logo
075940120530           //   (o quella del "simil-logo" BRT)
075950090107           *in55 = (%subst(AR8tmd : 1 : 3) = 'LOG');
075960090107      /end-free
075970060609     c                   endif
075980050606      *
075990051110     c                   clear                   dAR8LOG1S1
076000050606     c                   movel     AR8uni        dAR8LOG1S1
076010050706      *
076020050706      * TEST effettiva numericità dei campi previsti numerici:
076030100513     c                   clear                   w053a
076040050706     c                   eval      WX    =  %len(§A8LOG1S1i)
076050100513     c                   eval      w053a =  %subst(dAR8LOG1S1:216:WX)
076060050706     c                   eval      *in31 =  *off
076070110426     c     c_Digits      checkr    w053a :  WX                            31
076080050706if  1c                   if        *in31
076090050706     c                   clear                   §A8LOG1S1i
076100050706e   1c                   endif
076110050707      *
076120050606      * > intestazione fattura
076130050707if  1c                   if        §A8LOG1S1b =  *blanks
076140050707     c                   clear                   PLOG1rsf
076150050707     c                   eval      PLOG1rsf2  =  §A8LOG1S1a
076160050606x   1c                   else
076170050707     c                   eval      PLOG1rsf   =  §A8LOG1S1a
076180050707     c                   eval      PLOG1rsf2  =  §A8LOG1S1b
076190050606e   1c                   endif
076200050707     c                   eval      PLOG1inf   =  §A8LOG1S1c
076210050707     c                   eval      PLOG1inf2  =  §A8LOG1S1d
076220050707if  1c                   if        §A8LOG1S1e >  *zeros
076230050707     c                   eval      PLOG1caf   =  §A8LOG1S1e
076240050606e   1c                   endif
076250050707     c                   eval      PLOG1lof   =  §A8LOG1S1f
076260050606      * > partita IVA - codice fiscale
076270050707     c                   eval      PLOG1IVA   =  §A8LOG1S1g
076280050606      * > n./data fattura
076290050707     c                   eval      PLOG1nft   =  §A8LOG1S1h
076300050606      *     giro la data fattura
076310050706if  1c                   if        §A8LOG1S1i >  *zeros
076320060922     c                   eval      *in31 =  *off
076330060922     c     *iso          test(d)                 §A8LOG1S1i             31
076340060922if  2c                   if        NOT *in31
076350050707     c     *iso          movel     §A8LOG1S1i    Data_EUR
076360050707     c     *eur          movel     Data_EUR      PLOG1dft
076370060922x   2c                   else
076380060922     c                   clear                   PLOG1dft
076390060922e   2c                   endif
076400050706e   1c                   endif
076410050606      * > rif.ns.spedizione
076420060922     c                   eval      PLOG1nspa =   §A8LOG1S1j
076430050606      * > intestazione corpo fattura
076440050707     c                   eval      PLOG1tesf  =  §A8LOG1S1k
076450050707      *
076460050606      * > Stampa testata FATTURA - - - - - - - - - - - - - - - - - - -*
076470081015      *
076480080925      /free
076490081015         // - Forzatura del salto pagina
076500081015         CurLineA4 = wMaxLine + 1;
076510080925      /end-free
076520080925     c                   exsr      Test_OF
076530081015      /free
076540090323         // - Impostazione dei nuovi limiti di pagina
076550090323         select;
076560090323           when  NOT *in45;
076570090323             wMaxLine = $MaxLine2;
076580090323           when  *in45   and   NOT *in46;
076590090323             wMaxLine = $MaxLine2_pl1;
076600090323           when  *in45   and   *in46;
076610090323             wMaxLine = $MaxLine2_pl2;
076620090323         endsl;
076630081015      /end-free
076640080925     c                   exsr      Posiz_DL
076650081017     c                   if            §AR8cxd = *blanks
076660081017     c                             or  DB0fa4  = 'P'
076670080925     c                   write     A4prtDT0
076680080925     c                   endif
076690080925     c                   write     A4prtDL0D
076700050606     c                   write     A4prtDL1D
076710050606     c                   write     A4prtDL1E
076720050707      *
076730050606     c                   ENDSR
076740050606      *
076750050606      *---------------------------------------------------------------*
076760050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
076770050606      *?Dati di dettaglio corpo: FATTURA                             ?*
076780050606      *---------------------------------------------------------------*
076790050606     c     PrtDdtLOG1_S2 BEGSR
076800050606      *
076810050606     c                   clear                   A4prtDL6
076820051110     c                   clear                   dAR8LOG1S2
076830050606     c                   movel     AR8uni        dAR8LOG1S2
076840050606      *
076850050606      * > dati fattura
076860050606      * - - riga descrittiva 1
076870050707     c                   eval      PLOG1dft1 = §A8LOG1S2a
076880050606      *
076890050606      * > Stampa dettaglio FATTURA - - - - - - - - - - - - - - - - - -*
076900050606      *
076910050606     c                   exsr      Test_OF
076920050606     c                   write     A4prtDL6
076930050606      *
076940050606     c                   ENDSR
076950050606      *
076960050606      *---------------------------------------------------------------*
076970050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
076980050606      *?Dati di dettaglio corpo: FATTURA                             ?*
076990050606      *---------------------------------------------------------------*
077000050606     c     PrtDdtLOG1_S3 BEGSR
077010050606      *
077020050606     c                   clear                   A4prtDL7
077030051110     c                   clear                   dAR8LOG1S3
077040050707     c                   movel     AR8uni        dAR8LOG1S3
077050050606      *
077060050606      * > dati fattura
077070050606      * - - riga descrittiva 2
077080050707     c                   eval      PLOG1dft2 = §A8LOG1S3a
077090050606      *
077100050606      * > Stampa dettaglio FATTURA - - - - - - - - - - - - - - - - - -*
077110050606      *
077120050606     c                   exsr      Test_OF
077130050606     c                   write     A4prtDL7
077140050606      *
077150050606     c                   ENDSR
077160050606      *
077170050606      *---------------------------------------------------------------*
077180050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
077190050606      *?Dati di fondo: FATTURA                                       ?*
077200050606      *---------------------------------------------------------------*
077210050606     c     PrtDdtLOG1_S4 BEGSR
077220050606      *
077230051110     c                   clear                   dAR8LOG1S4
077240050707     c                   movel     AR8uni        dAR8LOG1S4
077250050706      *
077260050706      * TEST effettiva numericità dei campi previsti numerici:
077270100513     c                   clear                   w053a
077280050706     c                   eval      WX    =  %len(§A8LOG1S4a)
077290050706     c                                   +  %len(§A8LOG1S4b)
077300050706     c                                   +  %len(§A8LOG1S4c)
077310100513     c                   eval      w053a =  %subst(dAR8LOG1S4: 1:WX)
077320050706     c                   eval      *in31 =  *off
077330110426     c     c_Digits      checkr    w053a :  WX                            31
077340050706if  1c                   if        *in31
077350050706     c                   clear                   §A8LOG1S4a
077360050706     c                   clear                   §A8LOG1S4b
077370050706     c                   clear                   §A8LOG1S4c
077380050706e   1c                   endif
077390050606      *
077400050606      * > dati in chiusura FATTURA
077410050606      * - - dati di piede fattura riga 1
077420050707     c                   eval      PLOG1pft1 = §A8LOG1S4d
077430050606      * - - dati di piede fattura riga 2
077440050707     c                   eval      PLOG1pft2 = §A8LOG1S4e
077450050606      * - - dati di piede fattura riga 3
077460050707     c                   eval      PLOG1pft3 = §A8LOG1S4f
077470050606      * - - totale imponibile
077480050707     c                   eval      PLOG1timp = §A8LOG1S4a
077490050606      * - - totale iva
077500050707     c                   eval      PLOG1tiva = §A8LOG1S4b
077510050606      * - - totale fattura
077520050707     c                   eval      PLOG1tft  = §A8LOG1S4c
077530050606      *
077540050606      * > Stampa fondo FATTURA - - - - - - - - - - - - - - - - - - - -*
077550050606      *
077560050606      * - - Verifica OverFlow
077570080925      /free
077580080925           select;
077590080925             when  NOT *in45;
077600080925               wMaxLine = $MaxLine4;
077610080925             when  *in45   and   NOT *in46;
077620080925               wMaxLine = $MaxLine4_pl1;
077630080925             when  *in45   and   *in46;
077640080925               wMaxLine = $MaxLine4_pl2;
077650080925           endsl;
077660080925      /end-free
077670050606     c                   exsr      Test_OF
077680050606      * - - Calcolo della posizione finale di box e linee separatrici
077690050606     c                   exsr      Posiz_DL
077700050606      * - - Stampa parte finale FATTURA
077710050606     c                   write     A4prtDL8
077720050606      *
077730050606      * > Stampa box, linee (no logo)   - - - - - - - - - - - - - - -*
077740050606     c                   write     A4prtDL0E
077750050606     c                   write     A4prtDL0F
077760080908      *
077770050606     c                   ENDSR
077780050606      *
077790050606      *---------------------------------------------------------------*
077800050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
077810050606      *?Dati di fondo note: FATTURA                                  ?*
077820050606      *---------------------------------------------------------------*
077830050606     c     PrtDdtLOG1_S5 BEGSR
077840050606      *
077850051110     c                   clear                   dAR8LOG1S5
077860050707     c                   movel     AR8uni        dAR8LOG1S5
077870050606      *
077880050606      * > dati in chiusura FATTURA
077890050606      * - - nota 1
077900050707     c                   eval      PLOG1not1 = §A8LOG1S5a
077910050606      * - - nota 2
077920050707     c                   eval      PLOG1not2 = §A8LOG1S5b
077930050606      * - - nota 3
077940050707     c                   eval      PLOG1not3 = §A8LOG1S5c
077950050606      *
077960050606      * > Stampa fondo FATTURA - - - - - - - - - - - - - - - - - - - -*
077970050606      * - - Stampa parte finale FATTURA
077980050606     c                   write     A4prtDL9
077990080908      *
078000050606     c                   ENDSR
078010050321      *
078020050321      *---------------------------------------------------------------*
078030050414      *?Impostazione nuova pagina di DDT Logistica / Sait            ?*
078040050321      *---------------------------------------------------------------*
078050050321     c     NewPageDL     BEGSR
078060050321      *
078070080925      /free
078080080925         // Impostazione nuovo limite di OF
078090080925         SELECT;
078100080925           // Piede DDT   oppure   Piede Fattura
078110080925           WHEN  AR8trk = 'P1'   or   AR8trk = 'S4';
078120080925             select;
078130080925               when  NOT *in45;
078140080925                 wMaxLine = $MaxLine4;
078150080925               when  *in45   and   NOT *in46;
078160080925                 wMaxLine = $MaxLine4_pl1;
078170080925               when  *in45   and   *in46;
078180080925                 wMaxLine = $MaxLine4_pl2;
078190080925             endsl;
078200080925           // Dettaglio DDT  oppure   Testata & Dettaglio Fattura
078210080925           OTHER;
078220080925             select;
078230080925               when  NOT *in45;
078240090323                 wMaxLine = $MaxLine2;
078250080925               when  *in45   and   NOT *in46;
078260090323                 wMaxLine = $MaxLine2_pl1;
078270080925               when  *in45   and   *in46;
078280090323                 wMaxLine = $MaxLine2_pl2;
078290080925             endsl;
078300080925         ENDSL;
078310080925      /end-free
078320080925      *
078330080825      * RE-impostazione posizioni
078340080825     c                   exsr      Posiz_DL
078350080825      *
078360050321     c                   write     A4prtSP1
078370081006      /free
078380081006         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
078390081006           write  A4prtNUL;
078400081006         endif;
078410081006      /end-free
078420050321      *
078430050505sel 1c                   select
078440050505      * - altra pagina DDT Logistica / Sait
078450050707w   1c                   when          %subst(AR8trk:1:1) <> 'Z'
078460050606     c                             and %subst(AR8trk:1:1) <> 'S'
078470050321     c                   write     A4prtDL1B
078480050505      * - altra pagina Packing-List
078490050505w   1c                   when      %subst(AR8trk:1:1) =  'Z'
078500050502     c                              and   Page        >  *zeros
078510050321     c                   write     A4prtDL1C
078520050606      * - altra pagina Fattura
078530050606w   1c                   when      %subst(AR8trk:1:1) =  'S'
078540050606     c                              and   Page        >  *zeros
078550050606     c                   write     A4prtDL1E
078560050505e   1c                   endsl
078570050321      *
078580050321     c                   ENDSR
078590050318      *
078600050318      *---------------------------------------------------------------*
078610080709      *?Stampa testata della Packing-List per LOGISTICA              ?*
078620050318      *---------------------------------------------------------------*
078630050318     c     PrtPckLst_1   BEGSR
078640050318      *
078650050318      * > Salto a pagina successiva  - - - - - - - - - - - - - - - - -*
078660050318     c                   exsr      Test_OF
078670050318      *
078680050505      * > Stampa box, linee e testi fissi (CON logo)  - - - - - - - - -*
078690050318      *
078700050318      * - - Stampa "Copia per il destinatario" nella pagina successiva
078710081017     c*** poi in Test_OF:   if            §AR8cxd = *blanks
078720081017     c*** poi in Test_OF:             or  DB0fa4  = 'P'
078730050505     c*** poi in Test_OF:   write     A4prtDT0
078740050505     c*** poi in Test_OF:   endif
078750050318      * - - Stampa dati destinatario nel box superiore
078760050318     c                   write     A4prtDL1A
078770050318      * - - Stampa logo e box superiore x destinatario
078780050318     c                   write     A4prtDL0
078790081222      /free
078800081222         if  *in55;
078810090406           exsr   PrtDdtLOG1_SimilLogo;
078820120530           //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
078830120530           //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
078840120530           //  write  A4LogoBR2;
078850120530           //else;
078860120530           //  write  A4LogoBAR;
078870120530           //endif;
078880120530           write  A4LogoBRT;
078890081222           write  A4prtDLL;
078900081222         endif;
078910081222      /end-free
078920050318      * - - Stampa dati di testata nel box generale
078930050318     c                   write     A4prtDL1C
078940050318      * - - Stampa box generale
078950050318     c*** poi in Test_OF:   write     A4prtDL0C
078960050318      *
078970050318      * Impostazione del nuovo limite x overflow in packing-list
078980080925      /free
078990080925         select;
079000080925           when  NOT *in45;
079010090323             wMaxLine = $MaxLine2;
079020080925           when  *in45   and   NOT *in46;
079030090323             wMaxLine = $MaxLine2_pl1;
079040080925           when  *in45   and   *in46;
079050090323             wMaxLine = $MaxLine2_pl2;
079060080925         endsl;
079070080925      /end-free
079080050318      *
079090050318     c                   ENDSR
079100041019      *
079110041020      *---------------------------------------------------------------*
079120050414      *?Posizionamento di linee e box nel DDT LOGISTICA / SAIT       ?*
079130041020      *---------------------------------------------------------------*
079140041021     c     Posiz_DL      BEGSR
079150050218      *
079160080924      /free
079170080924         // Impostazione posizioni di logo/box
079180080924         // (posizioni di "partenza" diverse a seconda del posizionam.
079190080924         //  iniziale nel foglio A4 attualmente in stampa)
079200080924         select;
079210080924           when  NOT *in45;
079220081006             §PBA =  3.255;
079230080924           when  *in45   and   NOT *in46;
079240090422             §PBA = 0.255;
079250080924           when  *in45   and   *in46;
079260090422             §PBA = 6.005;
079270080924         endsl;
079280080924      /end-free
079290080708     c                   eval      §PBB  =  §PBA + 1.27
079300080708     c                   eval      §LOA  =  §PBA + 0.63
079310080708     c                   eval      §PS0  =  §PBA + 0.72
079320080708     c                   eval      §PS1  =  §PS0 + 0.10
079330080708     c                   eval      §PS2  =  §PS1 + 0.10
079340080708     c                   eval      §PS3  =  §PS2 + 0.10
079350080708     c                   eval      §PS4  =  §PS3 + 0.10
079360080708     c                   eval      §PS5  =  §PS4 + 0.10
079370080708      * Impostazione posizioni di logo/box per fattura
079380080708     c                   eval      §PBC  =  §PBA + 0.92
079390080708     c                   eval      §LOB  =  §PBA + 0.72
079400080708      * Impostazione posizioni di note fine fattura
079410080925     c                   eval      §PS6  =  §PBA + 5.35
079420081003     c                   eval      §PS7  =  §PS6 + 0.08
079430081003     c                   eval      §PS8  =  §PS7 + 0.08
079440080708      *
079450050321if  1c                   if        Page  <= 1
079460050218      *   (posizione di "partenza" su 1ª pagina)
079470080924     c                   eval      §PB1  =  §PBA + 1.375
079480050218x   1c                   else
079490050318      *   (posizione di "partenza" su pagine successive)
079500080822     c                   eval      §PB1  =  §PBA
079510050218e   1c                   endif
079520080924     c                   eval      §PB2  =  §PBA + 5.275
079530050218      * Impostazione posizioni di linee (orizzontali e verticali)
079540050218      *   nella parte superiore del box (testata)
079550050218     c                   eval      §LO1  =  §PB1 + 0.13
079560050318if  1c                   if        %subst(AR8trk:1:1) <> 'Z'
079570050218     c                   eval      §LO2  =  §LO1 + 0.13
079580050318     c                   eval      §LO3  =  §LO2 + 0.11
079590050318x   1c                   else
079600080825     c                   eval      §LO2  =  §LO1 + 0.115
079610050318     c                   eval      §LO3  =  §LO2
079620050318e   1c                   endif
079630050607if  1c                   if        %subst(AR8trk:1:1) = 'S'
079640050610     c                   eval      §LO3  =  §PB2 - 0.40
079650050607     c                   EndIf
079660050218      * Impostazione posizioni di linee (orizzontali e verticali)
079670050218      *   nella parte inferiore del box (piede)
079680050318if  1c                   if        %subst(AR8trk:1:1) <> 'Z'
079690080822     c                   eval      §LO4  =  §PB2 - 0.52
079700090422     c                   eval      §LO5  =  §LO4 + 0.13
079710090422     c                   eval      §LO6  =  §LO5 + 0.13
079720090422     c                   eval      §LO7  =  §LO6 + 0.13
079730050318e   1c                   endif
079740050218      * Impostazione lunghezza delle linee verticali
079750050218      *   nella parte superiore del box (testata)
079760050218     c                   eval      §LV1  =  §LO1 - §PB1
079770050218     c                   eval      §LV2  =  §LO2 - §LO1
079780050318if  1c                   if        %subst(AR8trk:1:1) <> 'Z'
079790050218     c                   eval      §LV3  =  §LO3 - §LO2
079800050318e   1c                   endif
079810050607if  1c                   if        %subst(AR8trk:1:1) = 'S'
079820050607     c                   eval      §LV2  =  0.40
079830050607     c                   EndIf
079840050218      * La lunghezza delle linee verticali separatrici nel dettaglio
079850050218      *   dipende dalla fase di stampa: se si sta stampando ancora il
079860050218      *   dettaglio deve essere più lunga, visto che non viene stampa-
079870050218      *   ta la parte finale del DDT...
079880080924if  1c                   if        %subst(AR8trk:1:1) = 'P'
079890080924     c                              and  (§PS1        < $LimOF
079900080924     c                                and NOT *in45
079910080924     c                                OR  §PS1        < ($LimOF - c_Len1_inch)
079920080924     c                                and *in45   and   NOT *in46
079930081003     c                                OR  §PS1        < ($LimOF + c_Len2_inch)
079940080924     c                                and *in45   and   *in46)
079950050318     c                              and   CurLineA4   <  wMaxLine
079960050218     c                   eval      §LV4  =  §LO4 - §LO3
079970050218x   1c                   else
079980050218     c                   eval      §LV4  =  §PB2 - §LO3
079990050218e   1c                   endif
080000080908      *
080010071008     c                   if        §AR8tsd = 'L3' or §AR8tsd = 'L4' and
080020060609     c                             %subst(AR8trk:1:1) <> 'Z' and
080030060609     c                             %subst(AR8trk:1:1) <> 'S'
080040071008     c                   eval      §LV3 = 0
080050071008     c                   eval      §LV4 = 0
080060060609     c                   endif
080070080908      *
080080050502      * - qui imposto le posizioni per il piede del box
080090050318if  1c                   if        %subst(AR8trk:1:1) <> 'Z'
080100050218     c                   eval      §LV5  =  §LO5 - §LO4
080110050218     c                   eval      §LV6  =  §LO6 - §LO5
080120050218     c                   eval      §LV7  =  §PB2 - §LO7
080130050318e   1c                   endif
080140041028      * Impostazione posizioni dei campi di testo verticali
080150080822     c                   eval      §PR1  =  §PB2 - 0.05
080160041020      *
080170041020     c                   ENDSR
080180050113      *
080190050113      *---------------------------------------------------------------*
080200050217      *?Stampa DDT del mittente ARCTE                                ?*
080210050113      *---------------------------------------------------------------*
080220050217     c     DDT_Arcte     BEGSR
080230050113      *
080240081002      /free
080250081002         // Impostazione del nuovo limite x overflow
080260081002         // (Codesto limite è valido solo per la stampa del dettaglio.
080270081002         //   Per la stampa del "fondo" bolla occorre REimpostarlo ad
080280081002         //   un valore più basso.
080290081002         //  Ovvero: stampo più righe di dettaglio che posso; se alla
080300081002         //  fine delle righe di dettaglio non rimane spazio a suffi-
080310081002         //  cienza per il fondo: prima della stampa del fondo si salta
080320081002         //  alla pagina successiva, dove non si stampa alcuna riga di
080330081002         //  dettaglio.)
080340081002         select;
080350081002           when  NOT *in45;
080360081002             wMaxLine = $MaxLine2;
080370081002           when  *in45   and   NOT *in46;
080380090324             wMaxLine = $MaxLine5_pl1;
080390081002           when  *in45   and   *in46;
080400090324             wMaxLine = $MaxLine5_pl2;
080410081002         endsl;
080420081002
080430081002         // Posizionamento iniziale all'interno del foglio A4
080440081002         write  A4prtSP1;
080450081002      /end-free
080460050113      *
080470050113      * Impostazione testi fissi - - - - - - - - - - - - - - - - - - -*
080480050113      *
080490050113     c                   eval      PCLtx1 =
080500050113     c                             'La merce viaggia a rischio e pericolo -
080510050113     c                              del committente. I reclami vanno fatti -
080520050113     c                              per iscritto entro 8 gg. dal ricevimento -
080530050113     c                              delle merce.'
080540050113     c                   eval      PCLtx2 =
080550050113     c                              'Il ritorno di merce deve -
080560050113     c                              essere preventivamente autorizzato -
080570050113     c                              dal Gruppo Arcte. Foro competente -
080580050113     c                              come da condizioni di vendita.'
080590050113      *
080600050113     c                   eval      PCLpx1 =
080610050113     c                             'CONTRIBUTO CONAI ASSOLTO'
080620050217      *
080630130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
080640130315      *  "StampaDDT")
080650050217     c***  KLDV          setll     FIAR8000       (già fatto)
080660050217     c***  KLDV          reade     FIAR8000       (già fatto)
080670050113      *
080680050113      * Reperimento dati, impostazione e stampa DDT mittente  - - - -*
080690050113      *
080700050113if  1c                   dow       NOT %eof(FIAR801L)
080710050113      *
080720050113sel 2c                   select
080730050113      * - Record annullato (non si stampa nulla)
080740050217w   2c                   when      AR8atb  <> *blanks
080750050707      * - Dati di dettaglio: descrizioni generiche
080760050217w   2c                   when      AR8trk   = 'C1'
080770050217     c                   exsr      PrtDdtARCT_C1
080780050217      * - Dati di dettaglio: articolo, descrizioni, taglie, q.tà
080790050217w   2c                   when      AR8trk   = 'C2'
080800050217     c                   exsr      PrtDdtARCT_C2
080810050113      * - Dati di testata: destinatario, cliente di fatturazione
080820050217w   2c                   when      AR8trk   = 'T1'
080830050217     c                   exsr      PrtDdtARCT_T1
080840050113      * - Dati di testata: agente, numero ordine, estremi documento
080850050217w   2c                   when      AR8trk   = 'T2'
080860050217     c                   exsr      PrtDdtARCT_T2
080870050113      * - Dati di fondo: mezzo e causale trasporto, colli, peso
080880050217w   2c                   when      AR8trk   = 'P1'
080890050217     c                   exsr      PrtDdtARCT_P1
080900050113e   2c                   endsl
080910050113      *
080920050113     c     KLDV          reade     FIAR8000
080930050113      *
080940050113e   1c                   enddo
080950050217      *
080960050217      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
080970050217      *
080980050217      * - - Verifica OverFlow
080990050217     c                   exsr      Test_OF
081000050218      *
081010050218      * - - Calcolo la posizione finale del box e di linee separatrici
081020050218     c                   exsr      Posiz_AC
081030050217      *
081040050217      * - - Stampa parte finale DDT
081050050217     c                   write     A4prtAC4
081060050217      *
081070050217      * > Stampa box, linee e testi fissi (no logo)  - - - - - - - - -*
081080081017     c                   if            §AR8cxd = *blanks
081090081017     c                             or  DB0fa4  = 'P'
081100050331     c                   write     A4prtDT0
081110050331     c                   endif
081120050217     c                   write     A4prtAC0A
081130050217     c                   write     A4prtAC0B
081140050217      *
081150050217      * Reimpostazione del vecchio limite x overflow
081160081002      /free
081170081002         select;
081180081002           when  NOT *in45;
081190081002             wMaxLine = $MaxLine2;
081200081002           when  *in45   and   NOT *in46;
081210081002             wMaxLine = $MaxLine2_pl1;
081220081002           when  *in45   and   *in46;
081230081002             wMaxLine = $MaxLine2_pl2;
081240081002         endsl;
081250081002      /end-free
081260050113      *
081270050113     c                   ENDSR
081280050214      *
081290050113      *---------------------------------------------------------------*
081300050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
081310050217      *?Dati di testata: destinatario, cliente di fatturazione       ?*
081320050113      *---------------------------------------------------------------*
081330050217     c     PrtDdtARCT_T1 BEGSR
081340050114      *
081350050114      * > logo
081360050114     c                   eval      §Logo  = 'LOGO' + AR8tmd
081370050113      *
081380051110     c                   clear                   dAR8ARCTT1
081390050707     c                   movel     AR8uni        dAR8ARCTT1
081400050706      *
081410050706      * TEST effettiva numericità dei campi previsti numerici:
081420100513     c                   clear                   w053a
081430050706     c                   eval      WX    =  %len(§ARCTT1a02)
081440100513     c                   eval      w053a =  %subst(dAR8ARCTT1:  7:WX)
081450050706     c                   eval      *in31 =  *off
081460110426     c     c_Digits      checkr    w053a :  WX                            31
081470050706if  1c                   if        *in31
081480050706     c                   clear                   §ARCTT1a02
081490050706e   1c                   endif
081500050706     c                   eval      WX    =  %len(§ARCTT1b02)
081510100513     c                   eval      w053a =  %subst(dAR8ARCTT1:163:WX)
081520050706     c                   eval      *in31 =  *off
081530110426     c     c_Digits      checkr    w053a :  WX                            31
081540050706if  1c                   if        *in31
081550050706     c                   clear                   §ARCTT1b02
081560050706e   1c                   endif
081570050113      *
081580050113      * > destinatario
081590050707     c                   eval      PARCTccd  = §ARCTT1a01
081600050707     c                   eval      PARCTccd1 = §ARCTT1a02
081610050707     c                   eval      PARCTrsd  = §ARCTT1a04
081620050707     c                   eval      PARCTrsd2 = §ARCTT1a05
081630050707     c                   eval      PARCTind  = §ARCTT1a06
081640050707     c                   eval      PARCTcad  = §ARCTT1a07
081650050707     c                   eval      PARCTlod  = §ARCTT1a08
081660050113      * > cliente di fatturazione
081670050707     c                   eval      PARCTccf  = §ARCTT1b01
081680050707     c                   eval      PARCTccf1 = §ARCTT1b02
081690050707     c                   eval      PARCTccf2 = §ARCTT1b03
081700050707     c                   eval      PARCTrsf  = §ARCTT1b04
081710050707     c                   eval      PARCTrsf2 = §ARCTT1b05
081720050707     c                   eval      PARCTinf  = §ARCTT1b06
081730050707     c                   eval      PARCTcaf  = §ARCTT1b07
081740050707     c                   eval      PARCTlof  = §ARCTT1b08
081750050113      * > magazzino spedizione
081760050707     c                   eval      PARCTmag  = §ARCTT1c01
081770050113      *
081780050113     c                   ENDSR
081790050113      *
081800050113      *---------------------------------------------------------------*
081810050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
081820050217      *?Dati di testata: documento                                   ?*
081830050113      *---------------------------------------------------------------*
081840050217     c     PrtDdtARCT_T2 BEGSR
081850050113      *
081860051110     c                   clear                   dAR8ARCTT2
081870050707     c                   movel     AR8uni        dAR8ARCTT2
081880050706      *
081890050706      * TEST effettiva numericità dei campi previsti numerici:
081900100513     c                   clear                   w053a
081910050706     c                   eval      WX    =  %len(§ARCTT2a02)
081920050706     c                                   +  %len(§ARCTT2a03)
081930100513     c                   eval      w053a =  %subst(dAR8ARCTT2: 9:WX)
081940050706     c                   eval      *in31 =  *off
081950110426     c     c_Digits      checkr    w053a :  WX                            31
081960050706if  1c                   if        *in31
081970050706     c                   clear                   §ARCTT2a02
081980050706     c                   clear                   §ARCTT2a03
081990050706e   1c                   endif
082000050113      *
082010050113      * > dati in testata DDT
082020050113      * - - numero e data documento (DDT)
082030050707     c                   eval      PARCTnsr = §ARCTT2a01
082040050707     c                   eval      PARCTnsp = §ARCTT2a02
082050050707     c                   eval      PARCTdsp = §ARCTT2a03
082060050113      *
082070050113      * > Stampa testata DDT - - - - - - - - - - - - - - - - - - - - -*
082080050113      *
082090050113      *   - OverFlow impossibile qui! -
082100050217     c***                exsr      Test_OF
082110050217     c                   exsr      Posiz_AC
082120081017     c                   if            §AR8cxd = *blanks
082130081017     c                             or  DB0fa4  = 'P'
082140050331     c                   write     A4prtDT0
082150050331     c                   endif
082160050217     c                   write     A4prtAC0
082170050217     c*** quante righe?  write     A4prtAC0A
082180050217     c                   write     A4prtAC1A
082190050217     c                   write     A4prtAC1B
082200050113      *
082210050113     c                   ENDSR
082220050214      *
082230050113      *---------------------------------------------------------------*
082240050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
082250050217      *?Dati di dettaglio: descrizioni generiche                     ?*
082260050113      *---------------------------------------------------------------*
082270050217     c     PrtDdtARCT_C1 BEGSR
082280050113      *
082290050114     c                   clear                   A4prtAC2
082300051110     c                   clear                   dAR8ARCTC1
082310050707     c                   movel     AR8uni        dAR8ARCTC1
082320050217      * > dati in note DDT
082330050217      * - - riga descrittiva
082340050707     c                   eval      PARCTdpr  = §ARCTC1a01
082350050113      *
082360050113      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
082370050113      *
082380050113     c                   exsr      Test_OF
082390050114     c                   write     A4prtAC2
082400050113      *
082410050113     c                   ENDSR
082420050113      *
082430050113      *---------------------------------------------------------------*
082440050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
082450050217      *?Dati di dettaglio: descrizione, taglie, qtà                  ?*
082460050113      *---------------------------------------------------------------*
082470050217     c     PrtDdtARCT_C2 BEGSR
082480050113      *
082490050114     c                   clear                   A4prtAC3
082500051110     c                   clear                   dAR8ARCTC2
082510050707     c                   movel     AR8uni        dAR8ARCTC2
082520050706      *
082530050706      * TEST effettiva numericità dei campi previsti numerici:
082540100513     c                   clear                   w053a
082550050706     c                   eval      WX    =  %len(§ARCTC2b00)
082560050706     c                                   +  %len(§ARCTC2b01)
082570050706     c                                   +  %len(§ARCTC2b02)
082580050706     c                                   +  %len(§ARCTC2b03)
082590050706     c                                   +  %len(§ARCTC2b04)
082600050706     c                                   +  %len(§ARCTC2b05)
082610050706     c                                   +  %len(§ARCTC2b06)
082620050706     c                                   +  %len(§ARCTC2b07)
082630050706     c                                   +  %len(§ARCTC2b08)
082640050706     c                                   +  %len(§ARCTC2b09)
082650100513     c                   eval      w053a =  %subst(dAR8ARCTC2:59:WX)
082660050706     c                   eval      *in31 =  *off
082670110426     c     c_Digits      checkr    w053a :  WX                            31
082680050706if  1c                   if        *in31
082690050706     c                   clear                   §ARCTC2b00
082700050706     c                   clear                   §ARCTC2b01
082710050706     c                   clear                   §ARCTC2b02
082720050706     c                   clear                   §ARCTC2b03
082730050706     c                   clear                   §ARCTC2b04
082740050706     c                   clear                   §ARCTC2b05
082750050706     c                   clear                   §ARCTC2b06
082760050706     c                   clear                   §ARCTC2b07
082770050706     c                   clear                   §ARCTC2b08
082780050706     c                   clear                   §ARCTC2b09
082790050706e   1c                   endif
082800050113      *
082810050113      * > dati in dettaglio DDT
082820050113      * - - descrizione articolo
082830050707     c                   eval      PARCTcpr  = §ARCTC2baa
082840050113      * - - taglia 0
082850050707     c                   eval      PARCTt0   = §ARCTC2b00
082860050113      * - - taglia 1
082870050707     c                   eval      PARCTt1   = §ARCTC2b01
082880050113      * - - taglia 2
082890050707     c                   eval      PARCTt2   = §ARCTC2b02
082900050113      * - - taglia 3
082910050707     c                   eval      PARCTt3   = §ARCTC2b03
082920050113      * - - taglia 4
082930050707     c                   eval      PARCTt4   = §ARCTC2b04
082940050113      * - - taglia 5
082950050707     c                   eval      PARCTt5   = §ARCTC2b05
082960050113      * - - taglia 6
082970050707     c                   eval      PARCTt6   = §ARCTC2b06
082980050113      * - - taglia 7
082990050707     c                   eval      PARCTt7   = §ARCTC2b07
083000050113      * - - taglia 8
083010050707     c                   eval      PARCTt8   = §ARCTC2b08
083020050113      * - - quantità
083030050707     c                   eval      PARCTqta  = §ARCTC2b09
083040050113      *
083050050113      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
083060050113      *
083070050113     c                   exsr      Test_OF
083080050114     c                   write     A4prtAC3
083090050113      *
083100050113     c                   ENDSR
083110050113      *
083120050113      *---------------------------------------------------------------*
083130050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
083140050217      *?Dati di fondo: beni, colli, peso, tot.qtà, porto, trasp.     ?*
083150050113      *---------------------------------------------------------------*
083160050217     c     PrtDdtARCT_P1 BEGSR
083170090324      *
083180090324      /free
083190090324           select;
083200090324             when  NOT *in45;
083210090324               wMaxLine = $MaxLine4;
083220090324             when  *in45   and   NOT *in46;
083230090324               wMaxLine = $MaxLine4_pl3;
083240090324             when  *in45   and   *in46;
083250090324               wMaxLine = $MaxLine4_pl4;
083260090324           endsl;
083270090324      /end-free
083280050113      *
083290051110     c                   clear                   dAR8ARCTP1
083300050707     c                   movel     AR8uni        dAR8ARCTP1
083310050706      *
083320050706      * TEST effettiva numericità dei campi previsti numerici:
083330100513     c                   clear                   w053a
083340050706     c                   eval      WX    =  %len(§ARCTP1a03)
083350100513     c                   eval      w053a =  %subst(dAR8ARCTP1:28:WX)
083360050706     c                   eval      *in31 =  *off
083370110426     c     c_Digits      checkr    w053a :  WX                            31
083380050706if  1c                   if        *in31
083390050706     c                   clear                   §ARCTP1a03
083400050706e   1c                   endif
083410050706     c                   eval      WX    =  %len(§ARCTP1b02)
083420050706     c                                   +  %len(§ARCTP1b03)
083430050706     c                                   +  %len(§ARCTP1b04)
083440100513     c                   eval      w053a =  %subst(dAR8ARCTP1:68:WX)
083450050706     c                   eval      *in31 =  *off
083460110426     c     c_Digits      checkr    w053a :  WX                            31
083470050706if  1c                   if        *in31
083480050706     c                   clear                   §ARCTP1b02
083490050706     c                   clear                   §ARCTP1b03
083500050706     c                   clear                   §ARCTP1b04
083510050706e   1c                   endif
083520050113      *
083530050113      * > dati in chiusura DDT
083540050114      * - - costanti di fine doc. e n. borderò
083550050707     c                   eval      PARCTc1   = §ARCTP1a01
083560050707     c                   eval      PARCTc2   = §ARCTP1a02
083570050707     c                   eval      PARCTc3   = §ARCTP1a03
083580050114      * - - costante segue fattura
083590050707     c                   eval      PARCTc4   = §ARCTp1a04
083600050113      * - - aspetto esteriore dei beni
083610050707     c                   eval      PARCTaeb  = §ARCTP1b01
083620050113      * - - numero colli
083630050707     c                   eval      PARCTncl  = §ARCTP1b02
083640050113      * - - peso in kg
083650050707     c                   eval      PARCTpkg  = §ARCTP1b03
083660050113      * - - totale qtà
083670050707     c                   eval      PARCTtqta = §ARCTP1b04
083680050113      * - - causale trasporto
083690050707     c                   eval      PARCTctr  = §ARCTP1c01
083700050113      * - - porto
083710050707     c                   eval      PARCTpto  = §ARCTP1c02
083720050113      * - - trasporto a cura
083730050707     c                   eval      PARCTtra  = §ARCTP1c03
083740050113      * - - trasportatore
083750050707     c                   eval      PARCTvet  = §ARCTP1d01
083760050217     *** *
083770050217     *** * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
083780050217     *** * ?(impostata a fine ciclo di lettura - sr. DDT_Arcte)?
083790050113      *
083800050113      * N.B.: la copia per il destinatario della LDV deve comparire
083810050113      *       nell'ultima pagina del DDT per il cliente (vedi subr.
083820050113      *       "ChiusuraLDV")
083830050113      *
083840050113     c                   ENDSR
083850081002      /free
083860081002       //--------------------------------------------------------------
083870140303       //?Impostazione nuova pagina di DDT Arcte                       ?
083880081002       //--------------------------------------------------------------
083890081002       BEGSR  NewPageAC;
083900081002
083910081002         // Impostazione del nuovo limite x overflow
083920081002         select;
083930081002           when  NOT *in45;
083940081002             wMaxLine = $MaxLine2;
083950081002           when  *in45   and   NOT *in46;
083960090324             wMaxLine = $MaxLine5_pl1;
083970081002           when  *in45   and   *in46;
083980090324             wMaxLine = $MaxLine5_pl2;
083990081002         endsl;
084000081002
084010081002         write  A4prtSP1;
084020081002         write  A4prtAC1B;
084030081002
084040081002       ENDSR;
084050081002      /end-free
084060050113      *
084070050113      *---------------------------------------------------------------*
084080050217      *?Posizionamento di linee e box nel DDT ARCTE                  ?*
084090050113      *---------------------------------------------------------------*
084100050114     c     Posiz_AC      BEGSR
084110080708      *
084120081002      /free
084130081002         // Impostazione posizioni di logo/box
084140081002         // (posizioni di "partenza" diverse a seconda del posizionam.
084150081002         //  iniziale nel foglio A4 attualmente in stampa)
084160081002         select;
084170081002           when  NOT *in45;
084180081002             §PBA =  3.130;
084190081002           when  *in45   and   NOT *in46;
084200081002             §PBA = 0.260;
084210081002           when  *in45   and   *in46;
084220081002             §PBA = 6.010;
084230081002         endsl;
084240081002      /end-free
084250080708     c                   eval      §PBB  =  §PBA + 1.27
084260080708     c                   eval      §LOA  =  §PBA + 0.63
084270050218      *
084280050217      * Impostazione posizioni di box
084290050217if  1c                   if        Page  <= 1
084300050217      *   (posizione di "partenza" su 1ª pagina)
084310081002     c                   eval      §PB1  =  §PBA + 1.38
084320050217x   1c                   else
084330050217      *   (posizione di "partenza" su pagine successive)
084340081002     c                   eval      §PB1  =  §PBA
084350050217e   1c                   endif
084360081002     c                   eval      §PB2  =  §PBA + 5.395
084370050217      * Impostazione posizioni di linee (orizzontali e verticali)
084380050217      *   nella parte superiore del box (testata)
084390080708     c                   eval      §LO1  =  §PB1 + 0.13
084400080708     c                   eval      §LO2  =  §LO1 + 0.13
084410050217      * Impostazione posizioni di linee (orizzontali e verticali)
084420050217      *   nella parte inferiore del box (piede)
084430081002     c                   eval      §LO3  =  §PBA + 5.005
084440081002     c                   eval      §LO4  =  §PBA + 5.135
084450081002     c                   eval      §LO5  =  §PBA + 5.265
084460050217      * Impostazione lunghezza delle linee verticali
084470050217      *   nella parte superiore del box (testata)
084480050217     c                   eval      §LV1  =  §LO1 - §PB1
084490050217     c                   eval      §LV2  =  §LO2 - §LO1
084500050217      * La lunghezza delle linee verticali separatrici nel dettaglio
084510050217      *   dipende dalla fase di stampa: se si sta stampando ancora il
084520050217      *   dettaglio deve essere più lunga, visto che non viene stampa-
084530050217      *   ta la parte finale del DDT...
084540050217if  1c                   if        %subst(AR8trk:1:1) = 'P'
084550050218     c                             and    §PS1        < $LimOF
084560050218     c                             and    CurLineA4   < wMaxLine
084570050217     c                   eval      §LV3  =  §LO3 - §LO2
084580050217x   1c                   else
084590050217     c                   eval      §LV3  =  §PB2 - §LO2
084600050217e   1c                   endif
084610050217     c                   eval      §LV4  =  §LO4 - §LO3
084620050217     c                   eval      §LV5  =  §LO5 - §LO4
084630050113      * Impostazione posizioni dei campi di testo verticali
084640081002     c                   eval      §PR1  =  §PBA + 5.35
084650081002     c                   eval      §PR2  =  §PBA + 5.47
084660050113      *
084670050113     c                   ENDSR
084680050214      *
084690050214      *---------------------------------------------------------------*
084700050217      *?Stampa DDT del mittente Esprinet                             ?*
084710050214      *---------------------------------------------------------------*
084720050214     c     DDT_Esprinet  BEGSR
084730050214      *
084740081002      /free
084750110405         // -?Impostazione del nuovo limite x overflow?
084760110405         //  ?Codesto limite è valido solo per la stampa del dettaglio.?
084770110405         //  ?Per la stampa del "fondo" bolla occorre REimpostarlo ad?
084780110405         //  ?un valore più basso.?
084790110405         //  ?Ovvero: stampo più righe di dettaglio che posso; se alla?
084800110405         //  ?fine delle righe di dettaglio non rimane spazio a suffi-?
084810110405         //  ?cienza per il fondo: prima della stampa del fondo si salta?
084820110405         //  ?alla pagina successiva, dove non si stampa alcuna riga di?
084830110405         //  ?dettaglio.?
084840081002         select;
084850081002           when  NOT *in45;
084860081002             wMaxLine = $MaxLine2;
084870081002           when  *in45   and   NOT *in46;
084880090324             wMaxLine = $MaxLine5_pl1;
084890081002           when  *in45   and   *in46;
084900090324             wMaxLine = $MaxLine5_pl2;
084910081002         endsl;
084920081002
084930110405         // -?Posizionamento iniziale all'interno del foglio A4?
084940081002         write  A4prtSP1;
084950110405
084960110405         // -?Pulizia tipo record?
084970110405         clear  SAVtrk;
084980081002      /end-free
084990050214      *
085000050214      * Impostazione testi fissi - - - - - - - - - - - - - - - - - - -*
085010050214      *
085020050325     ***c                   eval      PCLtx1 =
085030050325     ***c                             'Settimo Torinese - Tel. 011.895.39.29 *-
085040050325     ***c                              Padova - Tel. 049.870.50.40 *-
085050050325     ***c                              Bologna - Tel. 051.324.249 *-
085060050325     ***c                              Osimo - Tel. 071.781.96.75 *-
085070050325     ***c                              Roma - Tel. 06.723.15.43'
085080050325     ***c                   eval      PCLtx2 =
085090050325     ***c                             'Casalnuovo - Tel. 081.522.30.31 *-
085100050325     ***c                              Cinisello B. - Tel. 02.66.04.11.13 *-
085110050325     ***c                              Verona - Tel. 045.823.01.65 *-
085120050325     ***c                              Cagliari - Tel. 070.253.178'
085130050325     c                   eval      PCLtx3 =
085140131017     c                             'CONTRIBUTO AMBIENTALE CONAI ASSOLTO'
085150131017     c*//                           OVE DOVUTO'
085160050214      *
085170050214     c                   eval      PCLpx2 =
085180050214     c                             'IL PRESENTE DOCUMENTO E'' REGOLATO-
085190050214     c                              DALLE CONDIZIONI GENERALI DI VENDI-
085200050214     c                             TA PUBBLICATE SUL SITO INTERNET ALL-
085210050214     c                             ''INDIRIZZO www.esprinet.com CHE SI-
085220050325     c                              INTENDONO ESPRESSAMENTE CONOSCIUTE-
085230050325     c                              ED ACCETTATE'
085240050214      *
085250050214     c                   eval      PCLpx3a =
085260050214     c                             'NB. IN CASO DI RITARDATO PAGAMENTO-
085270050214     c                             , VERRANNO ADDEBITATI'
085280050214     c                   eval      PCLpx3b =
085290050214     c                             'INTERESSI AL TASSO DEL PRIME RATE -
085300050214     c                             MAGGIORATO DEL 2% A'
085310050214     c                   eval      PCLpx3c =
085320050214     c                             'DECORRERE DAL GIORNO SUCCESSIVO AL-
085330050214     c                             LA SCADENZA DELLA FATTURA'
085340050214      *
085350050214     c                   eval      PCLpx4a =
085360050214     c                             'Si prega di verificare la Vs. ragi-
085370050214     c                             one sociale, indirizzo e'
085380050214     c                   eval      PCLpx4b =
085390050214     c                             'partita IVA, dandoci eventuale sol-
085400050214     c                             lecita rettifica scritta.'
085410050214     c***                eval      PCLpx4c = *blanks
085420050214      *
085430050214     c                   eval      PCLpx5a =
085440050214     c                             '* I PRODOTTI IMPORTATI DAGLI U.S.A.-
085450050214     c                              SONO CON SPECIALE'
085460050214     c                   eval      PCLpx5b =
085470050214     c                             'LICENZA CHE PRECLUDE RIESPORTAZIONI-
085480050214     c                              NON AUTORIZZATE.'
085490050214     c***                eval      PCLpx5c = *blanks
085500050214      *
085510050325     ***c                   eval      PCLpx6  =
085520050325     ***c                             'CONTRIBUTO AMBIENTALE CONAI ASSOLTO-
085530050325     ***c                              OVE DOVUTO'
085540050325     c                   eval      PCLpx6  =
085550050325     c                             'LA SOCIETÀ SI IMPEGNA A VERSARE L''-
085560050325     c                             IVA NEI MODI E TERMINI PREVISTI (S-
085570050325     c                             OLIDARIETÀ ART. 60 BIS DPR 633/72-F-
085580050325     c                             INANZIARIA 2005)'
085590050214      *
085600130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
085610130315      *  "StampaDDT")
085620050214     c***  KLDV          setll     FIAR8000       (già fatto)
085630050214     c***  KLDV          reade     FIAR8000       (già fatto)
085640050214      *
085650050214      * Reperimento dati, impostazione e stampa DDT mittente  - - - -*
085660050214      *
085670050214if  1c                   dow       NOT %eof(FIAR801L)
085680050214      *
085690050214sel 2c                   select
085700050214      * - Record annullato (non si stampa nulla)
085710050217w   2c                   when      AR8atb  <> *blanks
085720050214      * - Dati di dettaglio: articolo, descrizione, quantità, ...
085730050217w   2c                   when      AR8trk   = 'C1'
085740050217     c                   exsr      PrtDdtESP1_C1
085750050214      * - Dati di dettaglio: testo (riga libera)
085760050217w   2c                   when      AR8trk   = 'C2'
085770050217     c                   exsr      PrtDdtESP1_C2
085780050214      * - Dati di testata: destinatario, cliente di fatturazione
085790050217w   2c                   when      AR8trk   = 'T1'
085800050217     c                   exsr      PrtDdtESP1_T1
085810050214      * - Dati di testata: estremi documento, riferimenti, ...
085820050217w   2c                   when      AR8trk   = 'T2'
085830050217     c                   exsr      PrtDdtESP1_T2
085840050214      * - Dati di fondo: totali del documento (netto merce, iva, ...)
085850050217w   2c                   when      AR8trk   = 'P1'
085860050217     c                   exsr      PrtDdtESP1_P1
085870050214      * - Dati di fondo: informazioni vettore
085880050217w   2c                   when      AR8trk   = 'P2'
085890050217     c                   exsr      PrtDdtESP1_P2
085900050214e   2c                   endsl
085910110405      *
085920110405     c                   eval      SAVtrk = AR8trk
085930050214      *
085940050214     c     KLDV          reade     FIAR8000
085950050214      *
085960050214e   1c                   enddo
085970050215      *
085980050215      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
085990050215      *
086000050215      * - - Verifica OverFlow
086010050215     c                   exsr      Test_OF
086020050218      *
086030050218      * - - Calcolo la posizione finale del box e di linee separatrici
086040050218     c                   exsr      Posiz_ES
086050050215      *
086060050215      * - - Stampa parte finale DDT
086070050215     c                   write     A4prtES4
086080050215      *
086090050215      * > Stampa box, linee e testi fissi (no logo)  - - - - - - - - -*
086100081017     c                   if            §AR8cxd = *blanks
086110081017     c                             or  DB0fa4  = 'P'
086120050331     c                   write     A4prtDT0
086130050331     c                   endif
086140050215     c                   write     A4prtES0A
086150050215     c                   write     A4prtES0B
086160050214      *
086170050214      * Reimpostazione del vecchio limite x overflow
086180081002      /free
086190081002         select;
086200081002           when  NOT *in45;
086210081002             wMaxLine = $MaxLine2;
086220081002           when  *in45   and   NOT *in46;
086230081002             wMaxLine = $MaxLine2_pl1;
086240081002           when  *in45   and   *in46;
086250081002             wMaxLine = $MaxLine2_pl2;
086260081002         endsl;
086270081002      /end-free
086280050214      *
086290050214     c                   ENDSR
086300050214      *
086310050214      *---------------------------------------------------------------*
086320050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
086330050217      *?Dati di testata: fatturato a, destinazione, cliente          ?*
086340050214      *---------------------------------------------------------------*
086350050217     c     PrtDdtESP1_T1 BEGSR
086360110405      *
086370110405      * - pulizia record a nuova packing-list
086380110405     c                   if            %subst( SAVtrk : 1 : 1 ) = 'P'
086390110405     c                             or  %subst( SAVtrk : 1 : 1 ) = 'C'
086400110405     c                   exsr      sr_ClearESP1
086410110405     c                   endif
086420050214      *
086430050214      * > logo
086440050214     c                   eval      §Logo  = 'LOGO' + AR8tmd
086450050214      *
086460051110     c                   movel(p)  AR8uni        dAR8ESP1T1
086470050214      *
086480050214      * > fatturato a
086490050707     c                   eval      PCrsf  = §A8ESP1T1a
086500050707     c                   eval      PCinf  = §A8ESP1T1b
086510050707if  1c                   if        §A8ESP1T1c > *zeros
086520050707     c                   eval      PCclpf = §A8ESP1T1c
086530050216     c                                    + ' '
086540050707     c                                    + §A8ESP1T1d
086550050216     c                                    + ' '
086560050707     c                                    + §A8ESP1T1e
086570050216x   1c                   else
086580050216     c                   eval      PCclpf = '     '
086590050216     c                                    + ' '
086600050707     c                                    + §A8ESP1T1d
086610050216     c                                    + ' '
086620050707     c                                    + §A8ESP1T1e
086630050216e   1c                   endif
086640050214      * > destinatazione
086650050707     c                   eval      PCrsd  = §A8ESP1T1f
086660050707     c                   eval      PCind  = §A8ESP1T1g
086670050707if  1c                   if        §A8ESP1T1h > *zeros
086680050707     c                   eval      PCclpd = §A8ESP1T1h
086690050216     c                                    + ' '
086700050707     c                                    + §A8ESP1T1i
086710050216     c                                    + ' '
086720050707     c                                    + §A8ESP1T1j
086730050216x   1c                   else
086740050216     c                   eval      PCclpd = '     '
086750050216     c                                    + ' '
086760050707     c                                    + §A8ESP1T1i
086770050216     c                                    + ' '
086780050707     c                                    + §A8ESP1T1j
086790050216e   1c                   endif
086800050214      * > dati in testata DDT
086810050214      * - - codice cliente
086820050707     c                   eval      PCcli  = §A8ESP1T1k
086830050214      * - - codice fiscale
086840050707     c                   eval      PCcf   = §A8ESP1T1l
086850050214      * - - partita iva
086860050707     c                   eval      PCpi   = §A8ESP1T1m
086870050214      *
086880050214     c                   ENDSR
086890050214      *
086900050214      *---------------------------------------------------------------*
086910050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
086920050217      *?Dati di testata: documento, riferimenti.                     ?*
086930050214      *---------------------------------------------------------------*
086940050217     c     PrtDdtESP1_T2 BEGSR
086950110405      *
086960110405      * - pulizia records a nuova packing-list
086970110405     c                   if            %subst( SAVtrk : 1 : 1 ) = 'P'
086980110405     c                             or  %subst( SAVtrk : 1 : 1 ) = 'C'
086990110405     c                   exsr      sr_ClearESP1
087000110405     c                   endif
087010050214      *
087020051110     c                   movel(p)  AR8uni        dAR8ESP1T2
087030050214      *
087040050214      * > dati in testata DDT
087050050214      * - - tipo documento
087060050707     c                   eval      PCtdo  = §A8ESP1T2a
087070050214      * - - data e numero documento (DDT)
087080050707     c                   eval      PCdtd  = §A8ESP1T2b
087090050707     c                   eval      PCnrd  = §A8ESP1T2c
087100050214      * - - riferimenti mittente
087110050707     c                   eval      PCrif  = §A8ESP1T2d
087120050214      * - - condizioni di pagamento
087130050707     c                   eval      PCpag  = §A8ESP1T2e
087140050214      * - - annotazioni
087150050707     c                   eval      PCann  = §A8ESP1T2f
087160050214      * - - agente
087170050707     c                   eval      PCag2  = §A8ESP1T2g
087180050214      *
087190050214      * > Stampa testata DDT - - - - - - - - - - - - - - - - - - - - -*
087200050214      *
087210050214      *   - OverFlow impossibile qui! -
087220050215     c***                exsr      Test_OF
087230050215     c                   exsr      Posiz_ES
087240081017     c                   if            §AR8cxd = *blanks
087250081017     c                             or  DB0fa4  = 'P'
087260050331     c                   write     A4prtDT0
087270050331     c                   endif
087280050214     c                   write     A4prtES0
087290081002     c*** (NON qui:)     write     A4prtES0A
087300081002     c                   write     A4prtSP1
087310050215     c                   write     A4prtES1A
087320050215     c                   write     A4prtES1B
087330050214      *
087340050214     c                   ENDSR
087350050214      *
087360050214      *---------------------------------------------------------------*
087370050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
087380050217      *?Dati di dettaglio: articolo, descrizione, quantità, ...      ?*
087390050214      *---------------------------------------------------------------*
087400050217     c     PrtDdtESP1_C1 BEGSR
087410110405      *
087420110405      * - pulizia records a nuova packing-list
087430110405     c                   if        %subst( SAVtrk : 1 : 1 ) = 'P'
087440110405     c                   exsr      sr_ClearESP1
087450110405     c                   endif
087460050214      *
087470050214     c                   clear                   A4prtES2
087480051110     c                   movel(p)  AR8uni        dAR8ESP1C1
087490050214      *
087500050214      * > dati in dettaglio DDT
087510050214      * - - codice prodotto
087520050707     c                   eval      PCcpr  = §A8ESP1C1a
087530050214      * - - descrizione prodotto
087540050707     c                   eval      PCdpr  = §A8ESP1C1b
087550050214      * - - quantità
087560050707     c                   eval      PCqta  = '      ' + §A8ESP1C1c
087570050214      * - - prezzo unitario
087580050707     c                   eval      PCpru  = §A8ESP1C1d
087590050214      * - - cambio: divisa + valore
087600050707     c                   eval      PCdiv  = §A8ESP1C1e
087610050707     c                   eval      PCcmb  = §A8ESP1C1f
087620050214      * - - % sconto
087630050707     c                   eval      PCprc  = §A8ESP1C1g
087640050214      * - - importo
087650050707     c                   eval      PCimp  = §A8ESP1C1h
087660050214      * - - codice iva
087670050707     c                   eval      PCciv  = §A8ESP1C1i
087680050214      *
087690050214      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
087700050214      *
087710050214     c                   exsr      Test_OF
087720050214     c                   write     A4prtES2
087730050214      *
087740050214     c                   ENDSR
087750050214      *
087760050214      *---------------------------------------------------------------*
087770050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
087780050217      *?Dati di dettaglio: note (riga libera)                        ?*
087790050214      *---------------------------------------------------------------*
087800050217     c     PrtDdtESP1_C2 BEGSR
087810110405      *
087820110405      * - pulizia records a nuova packing-list
087830110405     c                   if        %subst( SAVtrk : 1 : 1 ) = 'P'
087840110405     c                   exsr      sr_ClearESP1
087850110405     c                   endif
087860050214      *
087870050215     c                   clear                   A4prtES3
087880051110     c                   movel(p)  AR8uni        dAR8ESP1C2
087890050214      *
087900050214      * > dati in note DDT
087910050214      * - - riga descrittiva
087920050707     c                   eval      PCrig  = §A8ESP1C2a
087930050214      *
087940050214      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
087950050214      *
087960050214     c                   exsr      Test_OF
087970050214     c                   write     A4prtES3
087980050214      *
087990050214     c                   ENDSR
088000050214      *
088010050214      *---------------------------------------------------------------*
088020050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
088030050217      *?Dati di fondo: totali documento                              ?*
088040050214      *---------------------------------------------------------------*
088050050217     c     PrtDdtESP1_P1 BEGSR
088060050214      *
088070081002      /free
088080081002           select;
088090081002             when  NOT *in45;
088100081002               wMaxLine = $MaxLine6;
088110081002             when  *in45   and   NOT *in46;
088120081002               wMaxLine = $MaxLine6_pl1;
088130081002             when  *in45   and   *in46;
088140081002               wMaxLine = $MaxLine6_pl2;
088150081002           endsl;
088160081002      /end-free
088170051110     c                   movel(p)  AR8uni        dAR8ESP1P1
088180050215      *
088190050215      * > dati in fondo DDT
088200050215      * - - netto merce
088210050707     c                   eval      PCinm  = §A8ESP1P1a
088220050215      * - - spese di trasporto
088230050707     c                   eval      PCist  = §A8ESP1P1b
088240050215      * - - spese imballo
088250050707     c                   eval      PCisi  = §A8ESP1P1c
088260050215      * - - spese bolli
088270050707     c                   eval      PCisb  = §A8ESP1P1d
088280050215      * - - spese amministrative
088290050707     c                   eval      PCisa  = §A8ESP1P1e
088300050215      * - - spese varie
088310050707     c                   eval      PCisv  = §A8ESP1P1f
088320050215      * - - codici iva
088330050707     c                   eval      PCic1  = §A8ESP1P1g
088340050707     c                   eval      PCic2  = §A8ESP1P1m
088350050707     c                   eval      PCic3  = §A8ESP1P1s
088360050215      * - - descrizioni iva
088370050707     c                   eval      PCid1  = §A8ESP1P1h
088380050707     c                   eval      PCid2  = §A8ESP1P1n
088390050707     c                   eval      PCid3  = §A8ESP1P1t
088400050215      * - - imponibili iva: divise
088410050707     c                   eval      PCie1  = §A8ESP1P1i
088420050707     c                   eval      PCie2  = §A8ESP1P1o
088430050707     c                   eval      PCie3  = §A8ESP1P1u
088440050215      * - - imponibili iva: importi
088450050707     c                   eval      PCii1  = §A8ESP1P1j
088460050707     c                   eval      PCii2  = §A8ESP1P1p
088470050707     c                   eval      PCii3  = §A8ESP1P1v
088480050215      * - - aliquote iva
088490050707     c                   eval      PCia1  = §A8ESP1P1k
088500050707     c                   eval      PCia2  = §A8ESP1P1q
088510050707     c                   eval      PCia3  = §A8ESP1P1w
088520050215      * - - imposte iva
088530050707     c                   eval      PCiv1  = §A8ESP1P1l
088540050707     c                   eval      PCiv2  = §A8ESP1P1r
088550050707     c                   eval      PCiv3  = §A8ESP1P1x
088560050215      * - - Totali documento: divise + importi
088570050707     c                   eval      PCtdd  = §A8ESP1P1y
088580050707     c                   eval      PCtdi  = §A8ESP1P1z
088590050707     c                   eval      PCtdd2 = §A8ESP1P11
088600050707     c                   eval      PCtdi2 = §A8ESP1P12
088610050214      *
088620050214     c                   ENDSR
088630050214      *
088640050214      *---------------------------------------------------------------*
088650050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
088660050217      *?Dati di fondo: informazioni vettore                          ?*
088670050214      *---------------------------------------------------------------*
088680050217     c     PrtDdtESP1_P2 BEGSR
088690050214      *
088700081002      /free
088710081002           select;
088720081002             when  NOT *in45;
088730081002               wMaxLine = $MaxLine6;
088740081002             when  *in45   and   NOT *in46;
088750081002               wMaxLine = $MaxLine6_pl1;
088760081002             when  *in45   and   *in46;
088770081002               wMaxLine = $MaxLine6_pl2;
088780081002           endsl;
088790081002      /end-free
088800051110     c                   movel(p)  AR8uni        dAR8ESP1P2
088810050215      *
088820050215      * > dati in fondo DDT
088830050215      * - - resa
088840050707     c                   eval      PCres  = §A8ESP1P2a
088850050215      * - - trasporto a cura del
088860050707     c                   eval      PCtra  = §A8ESP1P2b
088870050215      * - - vettore
088880110426     c*//                eval      PCvet  = $Bartolini
088890110426     c*//                                 + ' - '
088900110426     c*//                                 + $Intest2
088910110426     c*//                                 + ' (BO)'
088920050707     c                   eval      PCvet2 = §A8ESP1P2c
088930050215      * - - causale trasporto
088940050707     c                   eval      PCctr2 = §A8ESP1P2d
088950050215      * - - aspetto esteriore dei beni
088960050707     c                   eval      PCaeb  = §A8ESP1P2e
088970050215      * - - numero colli
088980050707     c                   eval      PCncl2 = §A8ESP1P2f
088990050215      * - - peso complessivo
089000050707     c                   eval      PCpes  = §A8ESP1P2g
089010050215      * - - volume
089020050707     c                   eval      PCvol  = §A8ESP1P2h
089030050215      * - - inizio trasporto: data + ora
089040050707     c                   eval      PCdit  = §A8ESP1P2i
089050050707     c                   eval      PCoit  = §A8ESP1P2j
089060050215     *** *
089070050215     *** * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
089080050215     *** * ?(impostata a fine ciclo di lettura - sr. DDT_Esprinet)?
089090050217      *
089100050217      * N.B.: la copia per il destinatario della LDV deve comparire
089110050217      *       nell'ultima pagina del DDT per il cliente (vedi subr.
089120050217      *       "ChiusuraLDV")
089130050214      *
089140050214     c                   ENDSR
089150081002      /free
089160081002       //--------------------------------------------------------------
089170110405       //?Impostazione nuova pagina di DDT Esprinet                    ?
089180081002       //--------------------------------------------------------------
089190081002       BEGSR  NewPageES;
089200081002
089210110405         // -?Impostazione del nuovo limite x overflow?
089220081002         select;
089230081002           when  NOT *in45;
089240081002             wMaxLine = $MaxLine2;
089250081002           when  *in45   and   NOT *in46;
089260090324             wMaxLine = $MaxLine5_pl1;
089270081002           when  *in45   and   *in46;
089280090324             wMaxLine = $MaxLine5_pl2;
089290081002         endsl;
089300081002
089310081002         write  A4prtSP1;
089320081002         write  A4prtES1B;
089330081002
089340081002       ENDSR;
089350110405
089360110405       //--------------------------------------------------------------
089370110405       //?Pulizia campi a nuova packing-list relativa alla stessa LdV  ?
089380110405       //--------------------------------------------------------------
089390110405       BEGSR  sr_ClearESP1;
089400110405
089410110405         // -?Stampa fondo DDT?
089420110405
089430110405         // - -?Calcolo la posizione finale del box e di linee?
089440110405         //    ?separatrici?
089450110405         exsr  Posiz_ES;
089460110405
089470110405         // - -?Stampa parte finale DDT?
089480110405         write  A4prtES4;
089490110405
089500110405         // - -?Stampa box, linee e testi fissi (no logo)?
089510110405         if  §AR8cxd = *blank  or  DB0fa4 = 'P';
089520110405           write  A4prtDT0;
089530110405         endif;
089540110405         write  A4prtES0A;
089550110405         write  A4prtES0B;
089560110405
089570110405         // -?Pulizia record?
089580110405         select;
089590110405           when  %subst( AR8trk : 1 : 1 ) = 'T';
089600110405             clear  A4prtES0;
089610110405             clear  A4prtES0A;
089620110405             clear  A4prtES0B;
089630110405             clear  A4prtES1A;
089640110405             clear  A4prtES1B;
089650110405             clear  A4prtES2;
089660110405             clear  A4prtES3;
089670110405             clear  A4prtES4;
089680110405           when  %subst( AR8trk : 1 : 1 ) = 'C';
089690110405             clear  A4prtES2;
089700110405             clear  A4prtES3;
089710110405             clear  A4prtES4;
089720110405         endsl;
089730110405
089740110405         // -?Impostazione testi fissi?
089750131017         //PCLtx3 = 'CONTRIBUTO AMBIENTALE CONAI ASSOLTO OVE DOVUTO';
089760131017         PCLtx3 = 'CONTRIBUTO AMBIENTALE CONAI ASSOLTO';
089770110405
089780110405         PCLpx2 = 'IL PRESENTE DOCUMENTO E'' REGOLATO DALLE +
089790110405                   CONDIZIONI GENERALI DI VENDITA PUBBLICATE SUL SITO +
089800110405                   INTERNET ALL''INDIRIZZO www.esprinet.com CHE SI +
089810110405                   INTENDONO ESPRESSAMENTE CONOSCIUTE ED ACCETTATE';
089820110405
089830110405         PCLpx3a = 'NB. IN CASO DI RITARDATO PAGAMENTO, VERRANNO +
089840110405                    ADDEBITATI';
089850110405         PCLpx3b = 'INTERESSI AL TASSO DEL PRIME RATE MAGGIORATO +
089860110405                    DEL 2% A';
089870110405         PCLpx3c =
089880110405                   'DECORRERE DAL GIORNO SUCCESSIVO ALLA SCADENZA +
089890110405                    DELLA FATTURA';
089900110405
089910110405         PCLpx4a = 'Si prega di verificare la Vs. ragione sociale +
089920110405                    indirizzo e';
089930110405         PCLpx4b = 'partita IVA, dandoci eventuale sollecita +
089940110405                    rettifica scritta.';
089950110405
089960110405         PCLpx5a = '* I PRODOTTI IMPORTATI DAGLI U.S.A. SONO CON +
089970110405                    SPECIALE';
089980110405         PCLpx5b = 'LICENZA CHE PRECLUDE RIESPORTAZIONI NON +
089990110405                    AUTORIZZATE.';
090000110405
090010110405         PCLpx6  = 'LA SOCIETÀ SI IMPEGNA A VERSARE L''IVA NEI MODI +
090020110405                    E TERMINI PREVISTI (SOLIDARIETÀ ART. 60 BIS DPR +
090030110405                    633/72-FINANZIARIA 2005)';
090040110405
090050110405         // -?Salto Pagina?
090060110405         exsr  Test_OF;
090070110405
090080110405         // -?Re-Impostazione limiti di overflow?
090090110405         select;
090100110405           when  NOT *in45;
090110110405             wMaxLine = $MaxLine2;
090120110405           when  *in45   and   NOT *in46;
090130110405             wMaxLine = $MaxLine5_pl1;
090140110405           when  *in45   and   *in46;
090150110405             wMaxLine = $MaxLine5_pl2;
090160110405         endsl;
090170110405
090180110405         // -?Posizionamento iniziale all'interno del foglio A4?
090190110405         write  A4prtSP1;
090200110405
090210110405       ENDSR;
090220081002      /end-free
090230050214      *
090240050214      *---------------------------------------------------------------*
090250050217      *?Posizionamento di linee e box nel DDT ESPRINET               ?*
090260050214      *---------------------------------------------------------------*
090270050215     c     Posiz_ES      BEGSR
090280080708      *
090290081002      /free
090300081002         // Impostazione posizioni di logo/box
090310081002         // (posizioni di "partenza" diverse a seconda del posizionam.
090320081002         //  iniziale nel foglio A4 attualmente in stampa)
090330081002         select;
090340081002           when  NOT *in45;
090350081006             §PBA =  3.130;
090360081006             //§PBA =  3.255;        (più in basso di una riga)
090370081002           when  *in45   and   NOT *in46;
090380081002             §PBA = 0.255;
090390081002           when  *in45   and   *in46;
090400081002             §PBA = 6.005;
090410081002         endsl;
090420081002      /end-free
090430080708      * Impostazione posizioni di linee orizzontali
090440080708     c                   eval      §LOA  =  §PBA + 0.76
090450080708     c                   eval      §LOB  =  §LOA + 0.25
090460080708     c                   eval      §LOC  =  §LOB + 0.12
090470080708      * Impostazione posizioni di linee verticali
090480080708     c                   eval      §LVA  =  §LOA - 0.15
090490050214      *
090500050214      * Impostazione posizioni di box
090510050215if  1c                   if        Page  <= 1
090520050214      *   (posizione di "partenza" su 1ª pagina)
090530081002     c                   eval      §PB1  =  §PBA + 1.270
090540050214x   1c                   else
090550050214      *   (posizione di "partenza" su pagine successive)
090560081002     c                   eval      §PB1  =  §PBA + 0.020
090570050214e   1c                   endif
090580110405if  1c*//                if        %subst(AR8trk:1:1) = 'P'
090590110405if  1c                   if        %subst(SAVtrk:1:1) = 'P'
090600050331     c                             and    CurLineA4   < wMaxLine
090610081002     c                   eval      §PB2  =  §PBA + 5.360
090620050215x   1c                   else
090630050215     c                   eval(h)   §PB2  =  CurLineA4 / 8 + 0.05
090640081002     c                   if        §PB2  <  (§PBA + 5.320)
090650081002     c                   eval      §PB2  =  (§PBA + 5.320)
090660050331     c                   endif
090670050215e   1c                   endif
090680050215      * Impostazione posizioni di linee (orizzontali e verticali)
090690050214      *   nella parte superiore del box (testata)
090700050215     c                   eval      §LO1  =  §PB1 + 0.25
090710050214     c                   eval      §LO2  =  §LO1 + 0.11
090720050214      *   nella parte inferiore del box (piede)
090730110405if  1c*//                if        %subst(AR8trk:1:1) = 'P'
090740110405if  1c                   if        %subst(SAVtrk:1:1) = 'P'
090750050331     c                             and    CurLineA4   < wMaxLine
0907600810027.78 c                   eval      §LO3  =  §PBA + 3.900
0907700502158.00 c                   eval      §LO4  =  §LO3 + 0.24
0907800502158.39 c                   eval      §LO5  =  §LO4 + 0.36
0907900502158.52 c                   eval      §LO6  =  §LO5 + 0.14
0908000502148.64 c                   eval      §LO7  =  §LO6 + 0.12
0908100502148.77 c                   eval      §LO8  =  §LO7 + 0.13
0908200502148.89 c                   eval      §LO9  =  §LO8 + 0.12
0908300807088.97 c                   eval      §LO0  =  §LO9 + 0.10
090840050215e   1c                   endif
090850050214      * La lunghezza delle linee verticali separatrici nel dettaglio
090860050214      *   dipende dalla fase di stampa: se si sta stampando ancora il
090870050214      *   dettaglio deve essere più lunga, visto che non viene stampa-
090880050214      *   ta la parte finale del DDT...
090890110405if  1c*//                if        %subst(AR8trk:1:1) = 'P'
090900110405if  1c                   if        %subst(SAVtrk:1:1) = 'P'
090910050331     c                             and    CurLineA4   < wMaxLine
0909200502162.25 c                   eval      §LV1  =  §LO3 - §LO2
090930050215x   1c                   else
090940050216     c                   eval      §LV1  =  §PB2 - §LO2
090950050215e   1c                   endif
090960050214      *
090970050214      * Impostazione posizioni di campi di testo
090980050214      * - PCLtx1/tx2 (verticali)
0909900502179.10 c                   eval      §PR1  =  §PB2 - 0.07
091000050214      * - PCLpx2 ("Il presente documento...")
0910100502178.95 c                   eval      §PS2  =  §LO9 + 0.07
091020050214      * - PCLpx3a/px4a/px5a
0910300807089.04 c                   eval      §PS3  =  §LO0 + 0.07
091040050214      * - PCLpx3b/px4b/px5b
0910500502179.09 c                   eval      §PS4  =  §PS3 + 0.07
091060050214      * - PCLpx3c/px4c/px5c
0910700502179.14 c                   eval      §PS5  =  §PS4 + 0.07
091080050214      * - PCLpx6 ("Contributo ambientale CONAI...")
0910900502179.25 c                   eval      §PS6  =  §PB2 + 0.08
091100050214      *
091110050214     c                   ENDSR
091120041207      *
091130041207      *---------------------------------------------------------------*
091140050217      *?Stampa DDT del mittente (generico con logo)                  ?*
091150041207      *---------------------------------------------------------------*
091160041207     c     DDT_GenericoL BEGSR
091170041207      *
091180081003      /free
091190081003         // Impostazione del nuovo limite x overflow
091200081003         select;
091210081003           when  NOT *in45;
091220081003             wMaxLine = $MaxLine2;
091230081003           when  *in45   and   NOT *in46;
091240081003             wMaxLine = $MaxLine2_pl1;
091250081003           when  *in45   and   *in46;
091260081003             wMaxLine = $MaxLine2_pl2;
091270081003         endsl;
091280081003
091290081003         // Posizionamento iniziale all'interno del foglio A4
091300081003         write  A4prtSP1;
091310081006
091320081006         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
091330081006           write  A4prtNUL;
091340081006         endif;
091350081003      /end-free
091360041207      *
091370041207      * > logo
091380041207     c                   eval      §Logo  = 'LOGO' + AR8tmd
091390041207      *
091400081017     c                   if            §AR8cxd = *blanks
091410081017     c                             or  DB0fa4  = 'P'
091420080709     c                   write     A4prtDT0
091430080709     c                   endif
091440080709      *
091450041209      * Imposto posizione per logo
091460081003      /free
091470081003         select;
091480081003           when  NOT *in45;
091490081003             exsr  Rep_§LO2;
091500081003             §PB1 = §LO2 + (2 / 8);
091510081003           when  *in45   and   NOT *in46;
091520081003             §PB1 = 0.250;
091530081003           when  *in45   and   *in46;
091540081003             §PB1 = 6.000;
091550081003         endsl;
091560081003      /end-free
091570041209      *   Stampa del logo
091580041209     c                   write     A4prtDB0
091590041207      *
091600041207      * Reperimento dati e stampa DDT mittente - - - - - - - - - - - -*
091610041207      *
091620130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
091630130315      *  "StampaDDT")
091640041207     c***  KLDV          setll     FIAR8000       (già fatto)
091650041207     c***  KLDV          reade     FIAR8000       (già fatto)
091660041207      *
091670041207if  1c                   dow       NOT %eof(FIAR801L)
091680041207      *
091690041207if  2c                   if        AR8atb = *blanks
091700041207      *
091710041207      * > Stampa dei dati
091720041207     c                   exsr      Test_OF
091730041207sel 3c                   select
091740041207      * - Dati di dettaglio: testo in font a dimensione standard
091750041207w   3c                   when      AR8trk = 'C1'
091760051110     c                   movel(p)  AR8uni        PCddt
091770041207     c                   write     A4prtDC1
091780041207      * - Dati di testata:   testo in font a dimensioni massime
091790041207w   3c                   when      AR8trk = 'T1'
091800051110     c                   movel(p)  AR8uni        PCddt
091810041207     c                   write     A4prtDT1
091820041207      * - Dati di testata:   testo in font a dimensioni grandi
091830041207w   3c                   when      AR8trk = 'T2'
091840051110     c                   movel(p)  AR8uni        PCddt
091850041207     c                   write     A4prtDT2
091860041209      * - Linea separatrice
091870041209w   3c                   when      AR8trk = '--'
091880041209      *   - Stampa di una riga vuota per la riga separatrice
091890041209     c                   write     A4prtNUL
091900041209      *   - Stampa della linea orizzontale separatrice
091910090213     c                   eval(h)   §LO1   = (CurLineA4 / 8) - 0.050
091920041209     c                   write     A4prtLO
091930041207e   3c                   endsl
091940041207e   2c                   endif
091950041207      *
091960041207     c                   eval      SAVtrk = AR8trk
091970041207     c     KLDV          reade     FIAR8000
091980041207      *
091990041207e   1c                   enddo
092000080708      *
092010081003      /free
092020081003         select;
092030081003           when  NOT *in45;
092040081003             wMaxLine = $MaxLine2;
092050081003           when  *in45   and   NOT *in46;
092060081003             wMaxLine = $MaxLine2_pl1;
092070081003           when  *in45   and   *in46;
092080081003             wMaxLine = $MaxLine2_pl2;
092090081003         endsl;
092100081003      /end-free
092110041207      *
092120041207     c                   ENDSR
092130040607      *
092140040607      *---------------------------------------------------------------*
092150050217      *?Stampa DDT del mittente (generico senza logo)                ?*
092160040607      *---------------------------------------------------------------*
092170040607     c     DDT_Generico  BEGSR
092180040607      *
092190081003      /free
092200081003         // Impostazione del nuovo limite x overflow
092210081003         select;
092220081003           when  NOT *in45;
092230081003             wMaxLine = $MaxLine2;
092240081003           when  *in45   and   NOT *in46;
092250081003             wMaxLine = $MaxLine2_pl1;
092260081003           when  *in45   and   *in46;
092270081003             wMaxLine = $MaxLine2_pl2;
092280081003         endsl;
092290081003
092300081003         // Posizionamento iniziale all'interno del foglio A4
092310081003         write  A4prtSP1;
092320081006
092330081006         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
092340081006           write  A4prtNUL;
092350081006         endif;
092360081003      /end-free
092370040607      *
092380081017     c                   if            §AR8cxd = *blanks
092390081017     c                             or  DB0fa4  = 'P'
092400050331     c                   write     A4prtDT0
092410050331     c                   endif
092420040607      *
092430040607      * Reperimento dati e stampa DDT mittente - - - - - - - - - - - -*
092440040607      *
092450130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
092460130315      *  "StampaDDT")
092470040607     c***  KLDV          setll     FIAR8000       (già fatto)
092480040607     c***  KLDV          reade     FIAR8000       (già fatto)
092490040607      *
092500040607if  1c                   dow       NOT %eof(FIAR801L)
092510040607      *
092520040607if  2c                   if        AR8atb = *blanks
092530051110     c                   movel(p)  AR8uni        PCddt
092540040607     c                   exsr      Test_OF
092550040607sel 3c                   select
092560040607      * - Dati di dettaglio: testo in font a dimensione standard
092570040607w   3c                   when      AR8trk = 'C1'
092580040607     c                   write     A4prtDC1
092590040607      * - Dati di testata:   testo in font a dimensioni massime
092600040607w   3c                   when      AR8trk = 'T1'
092610040607     c                   write     A4prtDT1
092620040607      * - Dati di testata:   testo in font a dimensioni grandi
092630040607w   3c                   when      AR8trk = 'T2'
092640040607     c                   write     A4prtDT2
092650040607e   3c                   endsl
092660040607e   2c                   endif
092670040607      *
092680040607     c     KLDV          reade     FIAR8000
092690040607      *
092700040607e   1c                   enddo
092710080708      *
092720081003      /free
092730081003         select;
092740081003           when  NOT *in45;
092750081003             wMaxLine = $MaxLine2;
092760081003           when  *in45   and   NOT *in46;
092770081003             wMaxLine = $MaxLine2_pl1;
092780081003           when  *in45   and   *in46;
092790081003             wMaxLine = $MaxLine2_pl2;
092800081003         endsl;
092810081003      /end-free
092820040607      *
092830040607     c                   ENDSR
092840081017
092850081017      /free
092860081017       //--------------------------------------------------------------
092870140303       //?Impostazione nuova pagina di DDT Generico (con/senza logo)   ?
092880081017       //--------------------------------------------------------------
092890081017       BEGSR  NewPageSTD;
092900081017
092910081017         // Posizionamento iniziale nella nuova pagina
092920081017         //   (già fatto con A4prtSP0 se DB0FA4 = "P")
092930081017         if  DB0fa4 <> 'P';
092940081017           write  A4prtSP1;
092950081017           if  DB0fa4 = '1'   or   DB0fa4 = 'S';
092960081017             write  A4prtNUL;
092970081017           endif;
092980081017         endif;
092990081017
093000081017         // Stampa "COPIA PER IL DESTINATARIO"
093010081017         if  §AR8cxd = *blank   or   DB0fa4 = 'P';
093020081017           write  A4prtDT0;
093030081017         endif;
093040081017
093050081017         // Impostazione del nuovo limite x overflow
093060081017         select;
093070081017           when  NOT *in45;
093080081017             wMaxLine = $MaxLine2;
093090081017           when  *in45   and   NOT *in46;
093100081017             wMaxLine = $MaxLine2_pl1;
093110081017           when  *in45   and   *in46;
093120081017             wMaxLine = $MaxLine2_pl2;
093130081017         endsl;
093140081017
093150081017       ENDSR;
093160081017      /end-free
093170030515      *
093180030515      *---------------------------------------------------------------*
093190050217      *?Stampa del record "generico" (PRTGEN)                        ?*
093200030515      *---------------------------------------------------------------*
093210030515     c     StampaGEN     BEGSR
093220030515      *
093230030515      * Controllo se è stato raggiunto l'overflow nel formato in uso
093240030515     c                   exsr      Test_OF
093250030515      *
093260030911      * Stampo la riga PRTGEN
093270030911      * Se sto stampando una "testata" (*in39) salto una riga prima
093280030911      *   ed una riga dopo (per spaziare)
093290030911if  1c                   if        *in39 = *on
093300030515     c                   write     A4prtNUL
093310030911e   1c                   endif
093320030911     c                   write     A4prtGEN
093330030911if  1c                   if        *in39 = *on
093340030911     c                   write     A4prtNUL
093350030911e   1c                   endif
093360030515      *
093370030515     c                   ENDSR
093380030515      *
093390030515      *---------------------------------------------------------------*
093400050511      *?Prepara elenco per lista - SegnaColli Non Sequenziali (test.)?*
093410030515      *---------------------------------------------------------------*
093420030515     c     ListaScNS     BEGSR
093430030515      *
093440030515     c                   clear                   XX
093450030515      *
093460030515      * Nel foglio in formato "A5" devo solo riempire gli appositi
093470030715      *   campi "Note e Particolarità" (nessun salto riga/pagina è
093480030715      *   richiesto)
093490050513     c                   if           *in69       = *on
093500050513     c                             or $A4_x_ScNS  = *off
093510030515     c                   eval      WelSNS = $elSNSxA5
093520030515     c                   goto      ScNS_A5
093530030515     c                   endif
093540030515      *
093550030515      * Determino il numero di segnacolli da elencare per riga nel
093560030515      *   formato pagina "A4" (dove ci sono righe a parte).
093570030515      *   Dipende dal carattere specificato nel prtf: più grande è
093580030515      *   il carattere => meno segnacolli ci stanno sulla riga !
093590030911      *   A cpi 15 -come ora- ci sta tutta la schiera; a cpi 12 no !
093600030515     c                   eval      WelSNS = %elem($SC1)
093610030515      *
093620030515      * Spengo gli indicatori che condizionano in stampa le indicazioni
093630030515      *   per DPD / FedEx / Bancali-Supermercati-Autotrasportatori
093640030515      *   (se sono già state stampate nella 1ª pagina non devono più
093650030515      *   esserlo)
093660030910      *   (se non sono ancora state stampate, dovranno esserlo poi)
093670030515if  1c                   if        wPage > 1
093680050513     c                   movea     *in(70)       SAVin7x
093690040223     c                   movea     '0000'        *in(70)
093700030515e   1c                   endif
093710030515      *
093720030515      * Calcolo il numero di righe da saltare
093730030515     c                   eval      wMinLine = CurLineA4
093740030515if  1c                   if        §LO2     > §LO1
093750030515      * - se stampato almeno un box
093760030515     c                   eval(h)   wMinLine = (§LO2 * 8) + 2
093770030515x   1c                   else
093780030515      * - se non stampato alcun box
093790080730      /free
093800080729       // - Calcola la posizione della riga in 1/8" di pollice
093810080729       //   da quella attuale (in 1/9" di pollice)
093820080730         eval(h) wMinLine = (CurLineA4 * 8 / 9) + 1;
093830080730      /end-free
093840030515e   1c                   endif
093850030515      * ...e scarto il numero righe necessario
093860030515      *   (se non sono già da pagina nuova;
093870030515      *    se sono da pagina nuova: ci penserà poi la subr. "Test_OF"
093880030515      *       alla prima riga in stampa)
093890030515if  1c                   if        wMinLine  < wMaxLine
093900080729do  2c                   dou       CurLineA4 >= wMinLine
093910030515     c                   write     A4prtNUL
093920030515e   2c                   enddo
093930030515e   1c                   endif
093940030515      *
093950030515     c     ScNS_A5       tag
093960030515      *
093970030515      * Imposto l'indicatore per la stampa del titolo relativo all'
093980030515      *   elenco che segue (condiziona attributi di stampa diversi)
093990030515     c                   eval      *in39  = *on
094000050511      *
094010050511     c                   exsr      ListaScNSd
094020050511      *
094030050511     c                   ENDSR
094040050511      *
094050050511      *---------------------------------------------------------------*
094060050511      *?Prepara elenco per lista - SegnaColli Non Sequenziali (dett.)?*
094070050511      *---------------------------------------------------------------*
094080050511     c     ListaScNSd    BEGSR
094090030515      *
094100030515      * Stampo elenco dei segnacolli non sequenziali:
094110071008if  1c                   IF             DB0tbo  = 'P'
094120071008     c                             or   DB0tbo  = '8'
094130030515      * - per bolle in Partenza
094140030515     c                   exsr      ScNS_BLT
094150030515x   1c                   ELSE
094160081007      * - per bolle in Arrivo
094170030515     c                   exsr      ScNS_ART
094180030515e   1c                   ENDIF
094190030515      *
094200030515      * Alla fine: stampo eventuali segnacolli in elenco (non stampati)
094210030515      *              in foglio di formato "A4"
094220030515      *            imposto eventuali segnacolli in elenco (non messi in
094230030515      *              stampa tra le particolarità) in foglio "A5"
094240030515if  1c                   if        XX     > *zeros
094250030515     c                   eval      XX     = *hival
094260030515     c                   exsr      StampaScNS
094270030515e   1c                   endif
094280030910      *
094290030910      * Se sto stampando su formato pagina "A4": reimposto *in70, *in71
094300030910      *   e *in72 (spenti prima della stampa dell'elenco dei segnacolli
094310030910      *   non sequenziali) perchè saranno utilizzati per il posiziona-
094320030910      *   mento in nuova pagina (nella subr. "Test_OF") e per la stampa
094330040223      *   dei box per DPD, FedEx e quelli da FIAR5.
094340050513if  1c                   if             not *in69
094350030910     c                             and  not *in70
094360080922     c*** DPD non più in A4:       and  not *in71
094370030910     c                             and  not *in72
094380080912     c*** EEX non più gestita:     and  not *in73
094390030910     c                   movea     SAVin7x       *in(70)
094400030910e   1c                   endif
094410030515      *
094420030515     c                   ENDSR
094430030515      *
094440030515      *---------------------------------------------------------------*
094450050217      *?Prepara elenco SegnaColli Non Sequenziali da FNBLT (partenza)?*
094460030515      *---------------------------------------------------------------*
094470030515     c     ScNS_BLT      BEGSR
094480030515      *
094490030515if  1c                   if        not %open(FNBLT01L)
094500030515     c                   open      FNBLT01L
094510030515e   1c                   endif
094520030515      *
094530030515      * memorizzo l'ultimo numero di segnacollo
094540070514if  1c                   if        $Mamma = *off
094550040607     c     KLDV          setgt     FNBLT000
094560040607     c     KLDV          readpe    FNBLT000
094570070514x   1c                   else
094580070514     c     KLBL          setgt     FNBLT000
094590070514     c     KLBL          readpe    FNBLT000
094600070514e   1c                   endif
094610030515if  1c                   if        NOT %eof(FNBLT01L)
094620080922     c                   eval      %subst(Pnsx : 8) = ','
094630080922     c                                              + %editc(BLTnsc : 'Z')
094640030515e   1c                   endif
094650030515      *
094660030515      * memorizzo il primo numero di segnacollo
094670070514if  1c                   if        $Mamma = *off
094680040607     c     KLDV          setll     FNBLT000
094690040607     c     KLDV          reade     FNBLT000
094700070514x   1c                   else
094710070514     c     KLBL          setll     FNBLT000
094720070514     c     KLBL          reade     FNBLT000
094730070514e   1c                   endif
094740030515if  1c                   if        NOT %eof(FNBLT01L)
094750080922     c                   eval      %subst(Pnsx : 1 : 7) =
094760080922     c                                 %editc(BLTnsc : 'Z')
094770030515e   1c                   endif
094780030515      *
094790030515      * se ci sono solo 2 segnacolli non sequenziali:
094800030515      *   stanno già al posto del range - basta non stampare il msg
094810030515      *   ("NON SEQUENZIALI")
094820030515if  1c                   if        DB0ncl <= 2
094830050516     c                   eval      *in39  =  *off
094840030515     c                   goto      EndScNS_BLT
094850030515e   1c                   endif
094860030515      *
094870030515      * ciclo di lettura per il recupero di tutti i numeri di
094880030515      *   segnacollo
094890030515do  1c                   DOW       NOT %eof(FNBLT01L)
094900030515     c                   eval      Wnsc   =  BLTnsc
094910030515     c                   exsr      StampaScNS
094920070514if  2c                   if        $Mamma = *off
094930040607     c     KLDV          reade     FNBLT000
094940070514x   2c                   else
094950070514     c     KLBL          reade     FNBLT000
094960070514e   2c                   endif
094970030515e   1c                   ENDDO
094980030515      *
094990030515     c     EndScNS_BLT   ENDSR
095000030515      *
095010030515      *---------------------------------------------------------------*
095020050217      *?Prepara elenco SegnaColli Non Sequenziali da FNART (arrivo)  ?*
095030030515      *---------------------------------------------------------------*
095040030515     c     ScNS_ART      BEGSR
095050030515      *
095060070319if  1c                   if        not %open(FNART01L)
095070070319     c                   open      FNART01L
095080070319e   1c                   endif
095090030515      *
095100030515      * memorizzo l'ultimo numero di segnacollo
095110070514if  1c                   if        $Mamma = *off
095120040607     c     KLDV          setgt     FNART000
095130040607     c     KLDV          readpe    FNART000
095140070514x   1c                   else
095150070514     c     KLBL          setgt     FNART000
095160070514     c     KLBL          readpe    FNART000
095170070514e   1c                   endif
095180030515if  1c                   if        NOT %eof(FNART01L)
095190080922     c                   eval      %subst(Pnsx : 8) = ','
095200080922     c                                              + %editc(ARTnsc : 'Z')
095210030515e   1c                   endif
095220030515      *
095230030515      * memorizzo il primo numero di segnacollo
095240070514if  1c                   if        $Mamma = *off
095250040607     c     KLDV          setll     FNART000
095260040607     c     KLDV          reade     FNART000
095270070514x   1c                   else
095280070514     c     KLBL          setll     FNART000
095290070514     c     KLBL          reade     FNART000
095300070514e   1c                   endif
095310030515if  1c                   if        NOT %eof(FNART01L)
095320080922     c                   eval      %subst(Pnsx : 1 : 7) =
095330080922     c                                 %editc(ARTnsc : 'Z')
095340030515e   1c                   endif
095350030515      *
095360030515      * se ci sono solo 2 segnacolli non sequenziali:
095370030515      *   stanno già al posto del range - basta non stampare il msg
095380030515      *   ("NON SEQUENZIALI")
095390030515if  1c                   if        DB0ncl <= 2
095400050516     c                   eval      *in39  =  *off
095410030515     c                   goto      EndScNS_ART
095420030515e   1c                   endif
095430030515      *
095440030515      * ciclo di lettura per il recupero di tutti i numeri di
095450030515      *   segnacollo
095460030515do  1c                   DOW       NOT %eof(FNART01L)
095470030515     c                   eval      Wnsc   =  ARTnsc
095480030515     c                   exsr      StampaScNS
095490070514if  2c                   if        $Mamma = *off
095500040607     c     KLDV          reade     FNART000
095510070514x   2c                   else
095520070514     c     KLBL          reade     FNART000
095530070514e   2c                   endif
095540030515e   1c                   ENDDO
095550030515      *
095560030515     c     EndScNS_ART   ENDSR
095570030515      *
095580030515      *---------------------------------------------------------------*
095590050217      *?Stampa elenco SegnaColli Non Sequenziali in fogli "A4" o "A5"?*
095600030515      *---------------------------------------------------------------*
095610030515     c     StampaScNS    BEGSR
095620030515      *
095630030515      * In prima riga stampo la "intestazione"
095640030515if  1c                   if        *in39
095650050513if  2c                   if        *in69   or
095660050513     c                             $A4_x_ScNS = *off
095670050513      * - foglio "A5" oppure
095680050513      *   foglio "A4" in "NOTE E PARTICOLARITA'"
095690030515     c                   eval      YY      = YY  + 1
095700030515     c                   eval      $NP(yy) = C_ScNS + ':'
095710030515x   2c                   else
095720030515      * - foglio "A4"
095730030515     c                   eval      Prig  = C_ScNS
095740030515     c                                   + '         (dal '
095750080922     c                                   + %trim( %subst(Pnsx: 1: 7) )
095760030515     c                                   + ' al '
095770080922     c                                   + %trim( %subst(Pnsx: 9: 7) )
095780030515     c                                   + ')'
095790030515     c                   exsr      StampaGEN
095800030515e   2c                   endif
095810080922     c                   eval      Pnsx  =  'NON SEQUENZIALI'
095820030515     c                   eval      *in39 = *off
095830030515     c                   clear                   XX
095840030515     c                   clear                   $SC1
095850030515     c                   clear                   $SC2
095860030515e   1c                   endif
095870030515      *
095880030515      * Al riempimento della riga: stampo la parte di elenco segnacolli
095890030515if  1c                   if        XX  >= WelSNS
095900030515     c                   eval      $SC2 = %editc($SC1 : 'Z')
095910050513if  2c                   if        *in69   or
095920050513     c                             $A4_x_ScNS = *off
095930050513      * - foglio "A5"  oppure
095940050513      *   foglio "A4" in "NOTE E PARTICOLARITA'"
095950030515     c                   eval      YY      = YY  + 1
095960030515     c                   movea(p)  $SC2          $NP(yy)
095970030515x   2c                   else
095980030515      * - foglio "A4"
095990030911     c                   movea(p)  $SC2          Prig
096000030515     c                   exsr      StampaGEN
096010030515e   2c                   endif
096020030515if  2c                   if        XX  <  *hival
096030030515     c                   clear                   XX
096040030515e   2c                   endif
096050030515     c                   clear                   $SC1
096060030515     c                   clear                   $SC2
096070030515e   1c                   endif
096080030515      *
096090030515      * Aggiorno la nuova parte di elenco segnacolli
096100030515      *  (se non richiesta la stampa degli ultimi segnacolli non
096110030515      *   sequenziali residui - memorizzati ma non ancora stampati)
096120030515if  1c                   if        XX  <  *hival
096130030515     c                   eval      XX = xx  + 1
096140030515     c                   eval      $SC1(xx) = Wnsc
096150030515e   1c                   endif
096160030515      *
096170030515     c                   ENDSR
096180040607      *
096190040607      *---------------------------------------------------------------*
096200050217      *?Stampa della parte finale della Lettera di Vettura           ?*
096210040607      *---------------------------------------------------------------*
096220040607     c     ChiusuraLDV   BEGSR
096230080605      *
096240080605      * Posizionamento sul foglio A4 in base al n° pagina in stampa
096250130418     c                   eval      *in33 = *on
096260130418     c                   exsr      Posiz_Box
096270070307      *
096280070307      * Il tagliandino "Copia per il Destinatario" va ora stampato SE:
096290070307      * · spedizione NON in stato "lasciato avviso"   oppure
096300070307      * · spedizione in stato "lasciato avviso", ma P.A. con fattura
096310070307      *   immediata
096320040607      *
096330080703      * 2ª parte - copia per il destinatario (da lasciare al cliente)
096340040607      *   (Se NON è ristampa in arrivo per recapito c/assegni - vedi
096350040607      *   costante "Non utilizzare per la consegna")
096360130418      *                o per il mittente - se Porto Franco Prepagato
096370040607      * > "cornice" / barcode / righine varie in parte inferiore
096380040607      *
096390080604sel 1c                   select
096400080604w   1c                   when           $No_x_Cons         =  *off
096410070307     c                             and ($LascAvv           =  *off
096420070406     c                              or (%subst(DB0tb1:1:1) =  'A'
096430070406     c                              and DB0nft             >  *zeros))
096440130418     c                   write     A4prtLDV
096450130418     c                   write     A4prtBOX
096460110426     c*//                write     A4LogoBAR
096470110426      /free
096480120530         //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
096490120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
096500120530         //  // >?Simil-Logo Bartolini/Brt?
096510120530         //  write  A4LogoBR2;
096520120530         //  write  A4LogoDA2;
096530120530         //else;
096540120530         //  // >?Simil-Logo Bartolini?
096550120530         //  write  A4LogoBAR;
096560120530         //  write  A4LogoDA1;
096570120530         //endif;
096580120530         // >?Simil-Logo BRT?
096590120530         write  A4LogoBRT;
096600120530         write  A4LogoDA3;
096610110426      /end-free
096620080604w   1c                   when           $LascAvv           =  *on
096630070406     c                             and (%subst(DB0tb1:1:1) <> 'A'
096640070406     c                              or  DB0nft             =  *zeros)
096650070307     c                   eval      Pmsg3  =  Tla
096660070307     c                   write     A4prtLDVB
096670120113w   1c                   when      $No_x_Cons =  *on
096680120113w   1c*//                   when      $No_x_Cons =  '2'
096690120113     c*//                   eval      Pmsg3  =  ' NON UTILIZZARE PER'
096700120113     c*//                                    +  ' LASCIATO AVVISO -'
096710120113     c*//                                    +  ' SERVIZIO POSTE'
096720120113     c*//                   write     A4prtLDVB
096730080604e   1c                   endsl
096740040607      *
096750040607     c                   ENDSR
096760030515      *
096770030515      *---------------------------------------------------------------*
096780050217      *?Carico LDV da FNBLP00F                                       ?*
096790030515      *---------------------------------------------------------------*
096800030515     c     CarBLP        BEGSR
096810030515      *
096820030515      * Apertura files
096830030515     c                   if        not %open(FNBLP01L)
096840030515     c                   open      FNBLP01L
096850030515     c                   endif
096860030515     c                   if        not %open(FNARB01L)
096870030515     c                   open      FNARB01L
096880030515     c                   endif
096890070319     ***c v.sr sr_Start     if        not %open(FIAR401L)
096900070319     ***c " "     "         open      FIAR401L
096910070319     ***c " "     "         endif
096920030515     c                   if        not %open(FIAR601L)
096930030515     c                   open      FIAR601L
096940030515     c                   endif
096950030515     c                   if        not %open(FIAR701L)
096960030515     c                   open      FIAR701L
096970030515     c                   endif
096980060505     c                   if        not %open(FIAR901L)
096990060505     c                   open      FIAR901L
097000030515     c                   endif
097010071114      *
097020071114     c                   clear                   DB0cpi
097030030515      *
097040040607     c     KLDV          chain     FNBLP000
097050030515      *
097060030515if  1c                   if        NOT %found(FNBLP01L)
097070081117     c                   leavesr
097080030515e   1c                   endif
097090120509      *
097100120509      * Memorizzazione dei dati di FNBLP che serviranno anche dopo
097110120509      *   il reperimento dell'eventuale bolla mamma...
097120120509     c                   eval      wB0piv =  BLPcpi
097130030515      *
097140030515      * 1ª impostaz. campi della DS in base a quelli del file FNBLP
097150030515     c                   exsr      sr_DS_BLP
097160030515      *
097170070308      * Decodifico filiale di Arrivo
097180030515     c                   exsr      DecLNA
097190030515      *
097200030515      * 2ª impostaz. campi della DS in base a quelli delle tabelle
097210030515      *    "generiche" (3A, TB, 5E)
097220030515     c                   exsr      sr_DS_TAB
097230030515      *
097240030515      * Ultimi dati mancanti:
097250061030      * - Cod. cliente destinatario: imposto LNA9999
097260030515      *    SE   in Assegnato   O   c'è 2ª bolla
097270030515if  1c                   if           %subst(DB0tb1:1:1) = 'A'
097280030515     c                             or DB0tb2            <> *blanks
097290030515     c                   eval      DB0ccd = *all'9'
097300030515     c                   movel     BLPlna        DB0ccd
097310030515e   1c                   endif
097320120509      *
097330120509      * - Reperimento Data spedizione
097340120509if  1c                   if        DB0mgs <> *zeros
097350120509     c                   z-add     DB0mgs        Wdsp
097360120509     c                   movel     DB0aas        Wdsp
097370120509x   1c                   else
097380120509      *   prendo la data del giorno se non c'è la data spedizione
097390120509     c                   time                    w0140
097400120509     c                   move      w0140         w0080
097410120509     c     *eur          movel     w0080         Data_EUR
097420120509     c     *iso          movel     Data_EUR      Wdsp
097430120509e   1c                   endif
097440030515      *
097450061030      * 3ª impostaz. campi della DS: importi da calcolare
097460061031     c                   exsr      sr_DS_IMPO
097470030515      *
097480081117     c                   ENDSR
097490030515      *
097500030515      *---------------------------------------------------------------*
097510050217      *?Carico LDV da FNARB00F                                       ?*
097520030515      *---------------------------------------------------------------*
097530030515     c     CarARB        BEGSR
097540030515      *
097550030515      * Apertura files
097560030515     c                   if        not %open(FNARB01L)
097570030515     c                   open      FNARB01L
097580030515     c                   endif
097590070319     c                   if        NOT %open(FIARN11L)
097600070319     c                   open      FIARN11L
097610070319     c                   endif
097620030515     c                   if        not %open(FIAR601L)
097630030515     c                   open      FIAR601L
097640030515     c                   endif
097650030515     c                   if        not %open(FIAR701L)
097660030515     c                   open      FIAR701L
097670030515     c                   endif
097680060505     c                   if        not %open(FIAR901L)
097690060505     c                   open      FIAR901L
097700030515     c                   endif
097710071114      *
097720071114     c                   clear                   DB0cpi
097730030515      *
097740040607     c     KLDV          chain     FNARB01L
097750030515      *
097760030515if  1c                   if        NOT %found(FNARB01L)
097770070312     c                   clear                   ARBndc
097780070322     c                   clear                   ARBntc
097790070322     c                   clear                   ARBdcp
097800071015     c                   clear                   ARBpdc
097810101129     c                   clear                   ARBdcm
097820101129     c                   clear                   ARBicc
097830101129     c                   clear                   ARBica
097840120509     c                   clear                   ARBcpi
097850081117     c                   leavesr
097860030515e   1c                   endif
097870070514      *
097880070514      * Memorizzazione dei dati di FNARB che serviranno anche dopo
097890070514      *   il reperimento dell'eventuale bolla mamma...
097900070514     c                   eval      wB0ndc =  ARBndc
097910070514     c                   eval      wB0ntc =  ARBntc
097920070514     c                   eval      wB0dcp =  ARBdcp
097930081119     c                   eval      wB0tmc =  ARBtmc
097940071015     c                   move      ARBpdc        Wpdc4a
097950101125     c                   eval      wB0dcm =  ARBdcm
097960101125     c                   eval      wB0icc =  ARBicc
097970101125     c                   eval      wB0ica =  ARBica
097980120509     c                   eval      wB0piv =  ARBcpi
097990030515      *
098000030515      * 1ª impostaz. campi della DS in base a quelli del file FNARB
098010030515     c                   exsr      sr_DS_ARB
098020030515      *
098030070308      * Decodifico filiale di Arrivo
098040030515     c                   exsr      DecLNA
098050030515      *
098060030515      * 2ª impostaz. campi della DS in base a quelli delle tabelle
098070030515      *    "generiche" (3A, TB, 5E)
098080030515     c                   exsr      sr_DS_TAB
098090030515      *
098100030515      * Ultimi dati mancanti:
098110030515      * - Cod. cliente mittente
098120030515if  1c                   If        ARBccm <> *zeros
098130030515     c                   movel     ARBccm        DB0ccm
098140030515x   1c                   Else
098150030515if  2c                   if        %subst(DB0tb1:1:1) = 'F'
098160030515     c                   movel     ARBksc        DB0ccm
098170030515e   2c                   endif
098180030515e   1c                   EndIf
098190030515      * - Cod. cliente destinatario
098200030515if  2c                   if        %subst(DB0tb1:1:1) = 'A'
098210030515     c                   movel     ARBksc        DB0ccd
098220030515e   1c                   endif
098230030515      * - Se c'è 2ª bolla imposto LNA9999
098240030515if  1c                   if        DB0tb2 <> *blanks
098250030515     c                   eval      DB0ccd = *all'9'
098260030515     c                   movel     ARBlna        DB0ccd
098270030515e   1c                   endif
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
098460050217      *?1ª impostaz. campi della DS in base a quelli del file FNBLP  ?*
098470030515      *---------------------------------------------------------------*
098480030515     c     sr_DS_BLP     BEGSR
098490030515      *
098500030515     c                   eval      DB0cbo = BLPcbo
098510030515     c                   eval      DB0tsp = BLPtsp
098520030515     c                   eval      DB0mgs = BLPmgs
098530030515     c                   eval      DB0tc1 = BLPtc1
098540030515     c                   eval      DB0tc2 = BLPtc2
098550030515     c                   eval      DB0fns = BLPfns
098560030515     c                   eval      DB0rmn = BLPrmn
098570030515     c                   eval      DB0rma = BLPrma
098580030515     c                   eval      DB0ias = BLPias
098590030515     c                   eval      DB0vas = BLPvas
098600030515     c                   eval      DB0vmd = BLPvmd
098610030515     c                   eval      DB0vad = BLPvad
098620030515     c                   eval      DB0qft = BLPqft
098630030515     c                   eval      DB0lna = BLPlna
098640030515     c                   eval      DB0znc = BLPznc
098650030515     c                   eval      DB0rsm = BLPrsm
098660030515     c                   eval      DB0inm = BLPinm
098670030515     c                   eval      DB0cam = BLPcam
098680030515     c                   eval      DB0lom = BLPlom
098690030515     c                   eval      DB0prm = BLPprm
098700030515     c                   eval      DB0nzm = BLPnzm
098710030515     c                   eval      DB0fap = BLPfap
098720030515     c                   eval      DB0rmo = BLPrmo
098730030515     c                   eval      DB0rsd = BLPrsd
098740030515     c                   eval      DB0ind = BLPind
098750030515     c                   eval      DB0cad = BLPcad
098760030515     c                   eval      DB0lod = BLPlod
098770030515     c                   eval      DB0prd = BLPprd
098780030515     c                   eval      DB0nzd = BLPnzd
098790030515     c                   eval      DB0fin = BLPfin
098800030515     c                   eval      DB0gc1 = BLPgc1
098810030515     c                   eval      DB0gc2 = BLPgc2
098820030515     c                   eval      DB0nas = BLPnas
098830030515     c                   eval      DB0ncl = BLPncl
098840030515     c                   eval      DB0pkf = BLPpkf
098850030515     c                   eval      DB0fvf = BLPfvf
098860030515     c                   eval      DB0vlf = BLPvlf
098870030515     c                   eval      DB0fls = BLPfls
098880030515     c                   eval      DB0ncd = BLPncd
098890030515     c                   eval      DB0nca = BLPnca
098900030515     c                   eval      DB0xco = BLPxco
098910030515     c                   eval      DB0ffd = BLPffd
098920030515     c                   eval      DB0dcr = BLPdcr
098930030515     c                   eval      DB0hcr = BLPhcr
098940030515     c                   eval      DB0tcr = BLPtcr
098950030515     c                   eval      DB0fbr = BLPfbr
098960030515     c                   eval      DB0gga = BLPgga
098970030515     c                   eval      DB0gma = BLPgma
098980030515     c                   eval      DB0gva = BLPgva
098990030515     c                   move      ' '           DB0gva
099000030515     c                   eval      DB0sop = BLPsop
099010030515     c                   eval      DB0ctr = BLPctr
099020030515     c                   eval      DB0drt = BLPdrt
099030030515     c                   eval      DB0fpp = BLPfpp
099040030515     c                   eval      DB0nrt = BLPnrt
099050030515     c                   eval      DB0pdr = BLPpdr
099060030515      *
099070030515      * Cod. cliente mittente
099080040311     c                   move      BLPksc        w0040
099090030515if  1c                   IF        BLPccm <> *zeros
099100030515     c                   movel     BLPccm        DB0ccm
099110030515x   1c                   ELSE
099120030515      *   Lo imposto solo se non è codice generico destinatario
099130040311if  2c                   if        w0040  <> 9999
099140030515     c                   movel     BLPksc        DB0ccm
099150030515x   2c                   else
099160030515     c                   movel     BLPksc        DB0ccd
099170030515     c                   movel     BLPlna        DB0ccd
099180030515e   2c                   endif
099190030515e   1c                   ENDIF
099200030515      *
099210030515      * Allega documento di trasporto
099220081119     c                   clear                   DB0bam
099230060505if  1c                   if           BLPfst = 'S'
099240030515     c                             or BLPfst = 'C'
099250030515     c                   movel     'S'           DB0bam
099260060505e   1c                   endif
099270060524      *
099280060524      * Se bolla di RESO: Codice cliente da Assegnato
099290061107     c*** (già fatto:)   clear                   dsBL4A
099300060524if  1c                   if        DB0fbr = 'S'
099310060524     c                   exsr      RepAR4_A
099320060524e   1c                   endif
099330030515      *
099340030515      * Destinatario: vedo se esiste 2ª ragione sociale
099350061107     c                   clear                   DB0rd2
099360061107     c                   exsr      RepAR4_D
099370030515      *
099380030515      * Note
099390061107     c                   clear                   DB0nt1
099400061107     c                   clear                   DB0nt2
099410061107     c                   exsr      RepAR4_89
099420030515      *
099430030515     c                   ENDSR
099440030515      *
099450030515      *---------------------------------------------------------------*
099460050217      *?1ª impostaz. campi della DS in base a quelli del file FNARB  ?*
099470030515      *---------------------------------------------------------------*
099480030515     c     sr_DS_ARB     BEGSR
099490030515      *
099500030515     c                   eval      DB0cbo = ARBcbo
099510030515     c                   eval      DB0tsp = ARBtsp
099520030515     c                   eval      DB0mgs = ARBmgs
099530030515     c                   eval      DB0tc1 = ARBtc1
099540030515     c                   eval      DB0tc2 = ARBtc2
099550030515     c                   eval      DB0fns = ARBfns
099560030515     c                   eval      DB0rmn = ARBrmn
099570030515     c                   eval      DB0rma = ARBrma
099580030515     c                   eval      DB0ias = ARBias
099590030515     c                   eval      DB0vas = ARBvas
099600030515     c                   eval      DB0vmd = ARBvmd
099610030515     c                   eval      DB0vad = ARBvad
099620030515     c                   eval      DB0qft = ARBqft
099630030515     c                   eval      DB0lna = ARBlna
099640030515     c                   eval      DB0znc = ARBznc
099650030515     c                   eval      DB0rsm = ARBrsm
099660030515     c                   eval      DB0inm = ARBinm
099670030515     c                   eval      DB0cam = ARBcam
099680030515     c                   eval      DB0lom = ARBlom
099690030515     c                   eval      DB0prm = ARBprm
099700030515     c                   eval      DB0nzm = ARBnzm
099710030515     c                   eval      DB0fap = ARBfap
099720030515     c                   eval      DB0rmo = ARBrmo
099730030515     c                   eval      DB0rsd = ARBrsd
099740030515     c                   eval      DB0ind = ARBind
099750030515     c                   eval      DB0cad = ARBcad
099760030515     c                   eval      DB0lod = ARBlod
099770030515     c                   eval      DB0prd = ARBprd
099780030515     c                   eval      DB0nzd = ARBnzd
099790030515     c                   eval      DB0fin = ARBfin
099800030515     c                   eval      DB0gc1 = ARBgc1
099810030515     c                   eval      DB0gc2 = ARBgc2
099820030515     c                   eval      DB0nas = ARBnas
099830030515     c                   eval      DB0ncl = ARBncl
099840030515     c                   eval      DB0pkf = ARBpkf
099850030515     c                   eval      DB0fvf = ARBfvf
099860030515     c                   eval      DB0vlf = ARBvlf
099870030515     c                   eval      DB0fls = ARBfls
099880030515     c                   eval      DB0ncd = ARBncd
099890030515     c                   eval      DB0nca = ARBnca
099900030515     c                   eval      DB0xco = ARBxco
099910030515     c                   eval      DB0ffd = ARBffd
099920030515     c                   eval      DB0dcr = ARBdcr
099930030515     c                   eval      DB0hcr = ARBhcr
099940030515     c                   eval      DB0tcr = ARBtcr
099950030515     c                   eval      DB0fbr = ARBfbr
099960030515     c                   eval      DB0gga = ARBgga
099970030515     c                   eval      DB0gma = ARBgma
099980030515     c                   eval      DB0gva = ARBgva
099990030515     c                   move      ' '           DB0gva
100000030515     c                   clear                   DB0sop
100010030515     c                   eval      DB0ctr = ARBctr
100020030515     c                   clear                   DB0drt
100030030515     c                   clear                   DB0fpp
100040030515     c                   clear                   DB0nrt
100050030515     c                   clear                   DB0pdr
100060030515      *
100070030515      * Allega documento di trasporto
100080060524      * & Codice cliente da Assegnato
100090061107     c*** (già fatto:)   clear                   dsBL4A
100100031029     c                   clear                   DB0bam
100110061107     c                   exsr      RepAR4_A
100120060505     c                   if        §B4Abm = 'S' or §B4Abm = 'C' or
100130031029     c                             §B4Abm = 'P' or §B4Abm = 'Q'
100140030515     c                   movel     'S'           DB0bam
100150060505     c                   endif
100160030515      *
100170030515      * Destinatario: vedo se esiste 2ª ragione sociale
100180061107     c                   clear                   DB0rd2
100190061107     c                   exsr      RepAR4_D
100200030515      *
100210030515      * Note
100220061107     c                   clear                   DB0nt1
100230061107     c                   clear                   DB0nt2
100240061107     c                   exsr      RepAR4_89
100250030515      *
100260030515     c                   ENDSR
100270030515      *
100280030515      *---------------------------------------------------------------*
100290050217      *?2ª impostaz. campi della DS in base a quelli delle tabelle   ?*
100300050217      *?   "generiche" (non da "1A")                                 ?*
100310030515      *---------------------------------------------------------------*
100320030515     c     sr_DS_TAB     BEGSR
100330030515      *
100340030515      * Carico le tabelle "generiche" (3A, TB, 5E)
100350030515     c                   exsr      CarTAB
100360030515      *
100370030515      * Codici bolla
100380030515     c                   eval      DB0tb1 = §3Atb1
100390030515     c                   eval      DB0tb2 = §3Atb2
100400030515     c                   eval      DB0dcb = §3Ades
100410030515      * Tipo servizio
100420030515if  1c                   if        DB0dts = *blanks
100430030515     c                   eval      DB0dts = §5Edes
100440030515     c                   eval      DB0fst = §5Efst
100450030515e   1c                   endif
100460030515      * Consegne particolari
100470030515if  1c                   if            DB0tc1 <> *blanks
100480030515     c                             and DB0dc1 =  *blanks
100490030515     c                   movel(p)  DB0tc1        TBLkey
100500030515     c                   exsr      Tab_1P
100510030515     c                   eval      DB0dc1 = §1Pdes
100520030515e   1c                   endif
100530030515if  1c                   if            DB0tc2 <> *blanks
100540030515     c                             and DB0dc2 =  *blanks
100550030515     c                   movel(p)  DB0tc2        TBLkey
100560030515     c                   exsr      Tab_1P
100570030515     c                   eval      DB0dc2 = §1Pdes
100580030515e   1c                   endif
100590030515      * Esenzione IVA
100600030515if  1c                   if            DB0cei <> *blanks
100610030515     c                             and DB0dce =  *blanks
100620030515     c                   eval      DB0dce =  §EIdes
100630030515e   1c                   endif
100640030515      *
100650030515     c                   ENDSR
100660060524      *
100670060524      *---------------------------------------------------------------*
100680060524      *?Reperimento dati da FIAR400F x trk "A"                       ?*
100690061107      *?(dati aggiuntivi per P.Assegnato)                            ?*
100700060524      *---------------------------------------------------------------*
100710060524     c     RepAR4_A      BEGSR
100720060524      *
100730061107     c*** (già fatto:)   clear                   dsBL4A
100740061107      *
100750060524     c                   eval      Wtrc = 'A'
100760060524     c     KLDVtrc       chain     FIAR4000
100770060524      *
100780060524if  1c                   if        %found(FIAR401L)
100790060524     c                   movel     AR4Not        dsBL4A
100800060524e   1c                   endif
100810060524      *
100820060524     c                   ENDSR
100830061107      *
100840061107      *---------------------------------------------------------------*
100850061107      *?Reperimento dati da FIAR400F x trk "D"                       ?*
100860061107      *?(2ª ragione sociale del destinatario)                        ?*
100870061107      *---------------------------------------------------------------*
100880061107     c     RepAR4_D      BEGSR
100890061107      *
100900061107     c*** NON esiste:    clear                   dsBL4D
100910061107     c*** (già fatto:)   clear                   DB0rd2
100920061107      *
100930061107     c                   eval      Wtrc = 'D'
100940061107     c     KLDVtrc       chain     FIAR4000
100950061107      *
100960061107if  1c                   if        %found(FIAR401L)
100970061107     c                   movel     AR4not        DB0rd2
100980061107e   1c                   endif
100990061107      *
101000061107     c                   ENDSR
101010061107      *
101020060626      *---------------------------------------------------------------*
101030060626      *?Reperimento dati da FIAR400F x trk "I"                       ?*
101040061107      *?(dati DPD)                                                   ?*
101050060626      *---------------------------------------------------------------*
101060061030     c     RepAR4_I      BEGSR
101070061030      *
101080061107     c*** (già fatto:)   clear                   dsBL4I
101090061107      *
101100061030     c                   eval      Wtrc = 'I'
101110061030     c     KLDVtrc       chain     FIAR4000
101120061030      *
101130061030if  1c                   if        %found(FIAR401L)
101140061030     c                   movel     AR4not        dsBL4I
101150060626e   1c                   endif
101160061030      *
101170061030     c                   ENDSR
101180071008      *
101190071015      *---------------------------------------------------------------*
101200071015      *?Reperimento dati da FIAR400F x trk "M"                       ?*
101210071015      *?(dati aggiuntivi per O.R.M. abbinato al P.Franco PrePagato)  ?*
101220071015      *---------------------------------------------------------------*
101230071015     c     RepAR4_M      BEGSR
101240071015      *
101250071015     c*** (già fatto:)   clear                   dsBL4M
101260071015      *
101270071015     c                   eval      Wtrc = 'M'
101280071015     c     KLDVtrc       chain     FIAR4000
101290071015      *
101300071015if  1c                   if        %found(FIAR401L)
101310071015     c                   movel     AR4Not        dsBL4M
101320071015e   1c                   endif
101330071015      *
101340071015     c                   ENDSR
101350061107      *
101360061107      *---------------------------------------------------------------*
101370061107      *?Reperimento dati da FIAR400F x trk "Q"                       ?*
101380061107      *?(Codice Fiscale del mittente a sx & del destinatario a dx)   ?*
101390061107      *---------------------------------------------------------------*
101400061107     c     RepAR4_Q      BEGSR
101410061107      *
101420061108     c*** (già fatto:)   clear                   dTA4Q
101430061107     c*** (già fatto:)   clear                   DB0cpi
101440061107      *
101450061107     c                   eval      Wtrc = 'Q'
101460061107     c     KLDVtrc       chain     FIAR4000
101470061107      *
101480061107if  1c                   if        %found(FIAR401L)
101490061108     c                   movel     AR4not        dTA4Q
101500061108e   1c                   endif
101510061107      *
101520061107     c                   ENDSR
101530061107      *
101540061107      *---------------------------------------------------------------*
101550061107      *?Reperimento dati da FIAR400F x trk "8" / "9"                 ?*
101560061107      *?(note)                                                       ?*
101570061107      *---------------------------------------------------------------*
101580061107     c     RepAR4_89     BEGSR
101590061107      *
101600061107     c*** NON esiste:    clear                   dsBL48
101610061107     c*** NON usata:     clear                   dsBL49
101620061107     c*** (già fatto:)   clear                   DB0nt1
101630061107     c*** (già fatto:)   clear                   DB0nt2
101640061107      *
101650061107     c                   eval      Wtrc = '8'
101660061107     c     KLDVtrc       chain     FIAR4000
101670061107if  1c                   if        %found(FIAR401L)
101680061107     c                   movel     AR4not        DB0nt1
101690061107e   1c                   endif
101700061107      *
101710061107     c                   eval      Wtrc = '9'
101720061107     c     KLDVtrc       chain     FIAR4000
101730061107if  1c                   if        %found(FIAR401L)
101740061107     c                   movel     AR4not        DB0nt2
101750061107e   1c                   endif
101760061107      *
101770061107     c                   ENDSR
101780030515      *
101790030515      *---------------------------------------------------------------*
101800050217      *?Carico le tabelle che servono: 3A/TB/5E.                     ?*
101810030515      *---------------------------------------------------------------*
101820030515     c     CarTAB        BEGSR
101830030515      *
101840030515      * Tab. "3A" - Codici Bolla
101850030515      * (NO se rimesso mittente)
101860030515     c                   clear                   ds3A
101870030515if  1c                   if        DB0ris <> 'M'
101880030515     c                   movel     '3A'          TBLcod
101890030515     c                   movel(p)  DB0cbo        TBLkey
101900030515     c     K03TAB        chain     TABEL
101910030515if  2c                   if        %found(TABEL00F)
101920030515     c                   movel     TBLuni        ds3A
101930030515e   2c                   endif
101940030515e   1c                   endif
101950070515     c                   eval      $Mamma =  (§3Abcm = 'S')
101960030515      *
101970030515      * Tab. "TB" - Tipi Bolla
101980030515      * (NO se rimesso mittente)
101990030515      * se Bolla in assegnato, con tipo bolla vedo se da contabilizzare
102000030515      * se no non stampo i dati fattura
102010030515     c                   clear                   dsTB
102020030515if  1c                   if        DB0ris <> 'M'
102030030515     c                   movel     'TB'          TBLcod
102040061030if  2c                   if            §3Atb2 <> *blanks
102050121029     c                             and DB0tbo <> 'P'
102060030515     c                             and DB0tbo <> '8'
102070030515     c                   movel(p)  §3Atb2        TBLkey
102080030515x   2c                   else
102090030515     c                   movel(p)  §3Atb1        TBLkey
102100030515e   2c                   endif
102110030515     c     K03TAB        chain     TABEL
102120030515if  2c                   if        %found(TABEL00F)
102130030515     c                   movel     TBLuni        dsTB
102140030515e   2c                   endif
102150030515e   1c                   endif
102160030515      *
102170030515      * Tab. "5E" - Tipi Servizio
102180030515     c                   clear                   ds5E
102190090513if  1c                   if        DB0tsp <> *blanks
102200030515     c                   movel     '5E'          TBLcod
102210030515     c                   movel(p)  DB0tsp        TBLkey
102220030515     c     K03TAB        chain     TABEL
102230030515if  2c                   if        %found(TABEL00F)
102240030515     c                   movel     TBLuni        ds5E
102250030515e   2c                   endif
102260030515e   1c                   endif
102270030515      *
102280030911      * Tab. "EI" - Esenzioni I.V.A.
102290030515     c                   clear                   dsEI
102300030911      * Tab. "1A" - Tipi incasso contrassegno
102310030911     c                   clear                   ds1A
102320030911      * Tab. "1P" - Consegne particolari
102330030911     c                   clear                   ds1P
102340150226      *//* Tab. "TBA" - Tipi bancale
102350150226     c*//                clear                   dTBA
102360030515      *
102370030515     c                   ENDSR
102380030911      *
102390030911      *---------------------------------------------------------------*
102400050217      *?Carico tabella "EI" (Esenzione I.V.A.)                       ?*
102410030911      *---------------------------------------------------------------*
102420030911     c     Tab_EI        BEGSR
102430030911      *
102440030911     c                   clear                   dsEI
102450030911if  1c                   if        DB0cei <> *blanks  and  DB0dce = *blanks
102460030911     c                   movel     'EI'          TBLcod
102470030911     c                   movel(p)  DB0cei        TBLkey
102480030911     c     K03TAB        chain     TABEL
102490030911if  2c                   if        %found(TABEL00F)
102500030911     c                   movel     TBLuni        dsEI
102510030911e   2c                   endif
102520030911e   1c                   endif
102530030911     c                   eval      DB0dce =  §EIdes
102540030911      *
102550030911     c                   ENDSR
102560030515      *
102570030515      *---------------------------------------------------------------*
102580050217      *?Carico tabella "1A" (Tipo incasso contrassegno)              ?*
102590030515      *---------------------------------------------------------------*
102600030515     c     Tab_1A        BEGSR
102610030515      *
102620030515     c                   clear                   ds1A
102630030515if  1c                   if        DB0cas <> *zeros
102640030515     c                   movel     '1A'          TBLcod
102650030515     c                   movel(p)  DB0tic        TBLkey
102660030515     c     K03TAB        chain     TABEL
102670030515if  2c                   if        %found(TABEL00F)
102680030515     c                   movel     TBLuni        ds1A
102690030515e   2c                   endif
102700030515e   1c                   endif
102710030515      *
102720030515      * Imposto i dati nella DS
102730030515     c                   eval      DB0dti = §1Ades
102740030515     c                   eval      DB0ucm = §1Aucm
102750030515     c                   eval      DB0ste = §1Aste
102760030515      *
102770030515     c                   ENDSR
102780030515      *
102790030515      *---------------------------------------------------------------*
102800050217      *?Carico tabella "1P" (Consegne particolari)                   ?*
102810030515      *---------------------------------------------------------------*
102820030515     c     Tab_1P        BEGSR
102830030515      *
102840030515     c                   clear                   ds1P
102850030515     c                   movel     '1P'          TBLcod
102860030515     c*** già impostato: movel(p)  DB0tc1/2      TBLkey
102870030515     c     K03TAB        chain     TABEL
102880030515if  1c                   if        %found(TABEL00F)
102890030515     c                   movel     TBLuni        ds1P
102900030515e   1c                   endif
102910030515      *
102920030515     c                   ENDSR
102930150226      *
102940150226      *//*---------------------------------------------------------------*
102950150226      *//*?Reperisco tabella "TBA" (Tipi Bancale)                       ?*
102960150226      *//*---------------------------------------------------------------*
102970150226     c*//  Tab_TBA       BEGSR
102980150226      *//*
102990150226     c*//                clear                   dTBA
103000150226     c*//                reset                   TIBS02ds
103010150226     c*//                eval      T02cod = 'TBA'
103020150226     c*//                movel(p)  Wtba          T02ke1
103030150226     c*//                call      'TIBS02R'
103040150226     c*//                parm                    KPJBA
103050150226     c*//                parm                    TIBS02ds
103060150226if  1c*//                if        T02err = *blanks
103070150226     c*//                movel     T02uni        dTBA
103080150226e   1c*//                endif
103090150226      *//*
103100150226     c*//                ENDSR
103110030515      *
103120030515      *---------------------------------------------------------------*
103130050217      *?Calcolo importi LDV                                          ?*
103140030515      *---------------------------------------------------------------*
103150061031     c     sr_DS_IMPO    BEGSR
103160030515      *
103170030515      * Vedo se esiste tassazione
103180030515      *  solo se tipo bolla contabilizzabile
103190061030if  1c                   IF        §TBfcb =  '1'
103200030515     c
103210061030     c                   eval      Wtrc   =  '1'
103220040607     c     KLDVtrc       chain     FIAR6000
103230030515     c
103240030515if  2c                   if        %found(FIAR601L)
103250030515     c                   movel     AR6div        DB0div
103260030515     c                   exsr      CtrDIV
103270030515      *
103280030515      * NON c'è 2ª bolla
103290061031if  3c                   if            DB0tb2 = *blanks
103300121029     c                             or  DB0tbo = 'P'
103310061031     c                             or  DB0tbo = '8'
103320030515      *  Estremi fattura
103330030515     c                   z-add     AR6nft        DB0nft
103340030515     c                   z-add     AR6dft        DB0dft
103350130124     c                   z-add     AR6fiv        wB0fiv
103360061031      *  Se è assegnato o franco prepagato => stampo tutta la tassaz.
103370061031if  4c                   if            %subst(DB0tb1:1:1) = 'A'
103380061031     c                             or  DB0tbo = '8'
103390061030     c                   eval      Wtrc   =  '1'
103400030515     c                   exsr      RepTASSAZ
103410030515e   4c                   endif
103420030515e   3c                   endif
103430030515e   2c                   endif
103440030515      *
103450061030      * C'è 2ª bolla
103460061031if  2c                   IF            DB0tb2 <> *blanks
103470121029     c                             and DB0tbo <> 'P'
103480061031     c                             and DB0tbo <> '8'
103490030515      *
103500061030     c                   eval      Wtrc   =  '2'
103510040607     c     KLDVtrc       chain     FIAR6000
103520030515      *
103530061030if  3c                   If        %found(FIAR601L)
103540030515     c                   movel     AR6div        DB0div
103550030515     c                   exsr      CtrDIV
103560030515     c                   movel     AR6cei        DB0cei
103570030515      *   Estremi fattura
103580030515     c                   z-add     AR6nft        DB0nft
103590030515     c                   z-add     AR6dft        DB0dft
103600130124     c                   z-add     AR6fiv        wB0fiv
103610030515if  4c                   if        AR6ksc <> *zeros
103620030515     c                   eval      DB0ccd =  AR6ksc
103630030515e   4c                   endif
103640061030     c                   eval      Wtrc   =  '2'
103650030515     c                   exsr      RepTASSAZ
103660061030e   3c                   EndIf
103670030515      *
103680030515e   2c                   ENDIF
103690030515      *
103700030515e   1c                   ENDIF
103710030515      *
103720030515      * Vedo se ha contrassegno
103730060505     c     KLDV          chain     FIAR9000
103740060505if  1c                   IF        %found(FIAR901L)
103750061030     c                   eval      DB0cas =  AR9cas
103760061030     c                   eval      DB0vca =  AR9vca
103770061030     c                   eval      DB0gca =  AR9gca
103780061030     c                   eval      DB0tic =  AR9tic
103790030515x   1c                   ELSE
103800030515     c                   clear                   DB0cas
103810030515     c                   clear                   DB0vca
103820030515     c                   clear                   DB0gca
103830030515     c                   clear                   DB0tic
103840030515     c                   clear                   DB0dti
103850030515     c                   clear                   DB0ucm
103860030515     c                   clear                   DB0ste
103870030515e   1c                   ENDIF
103880120509      *
103890120509      * Se P.F. Prepagato verificherò il Codice Fiscale del Mittente
103900120509      *   (NON se mittente estero)
103910120509if  1c                   if           ( DB0tbo  = '8'
103920120509     c                             and (DB0nft  <> *zeros
103930120509     c                              or  DB0ift  <> *zeros)
103940120509     c                             and  DB0nzm  =  *blanks )
103950120509     c                   exsr      RepAR4_Q
103960120509     c                   movel     §TA4Qcfm      DB0cpi
103970120509e   1c                   endif
103980121029      * Se P.A. con fattura immediata o con 2ª bolla verificherò il
103990121029      *   Codice Fiscale (NON se destinatario estero)
104000121026if  1c*//                if           ( DB0tbo  = 'A'
104010121029if  1c                   if          ( (%subst(DB0tb1:1:1) = 'A'
104020121029     c                             or  (DB0tb2  <> *blanks
104030121029     c                             and  DB0tbo  <> 'P'
104040121029     c                             and  DB0tbo  <> '8') )
104050120509     c                             and (DB0nft  <> *zeros
104060120509     c                              or  DB0ift  <> *zeros)
104070120509     c                             and  DB0nzd  =  *blanks )
104080120509     c                   exsr      RepAR4_Q
104090120509     c                   movel     §TA4Qcfd      DB0cpi
104100120509e   1c                   endif
104110120509     c                   eval      wB0cfi = DB0cpi
104120061030      *
104130030515      * Vedo se si tratta di bolla figlia:
104140040607      * Se bolla con MAMMA prendo c/a e segnacolli da bolla MAMMA
104150070515if  1c                   if        $Mamma =  *on
104160081117     c                   exsr      BollaFiglia
104170030515e   1c                   endif
104180030515      *
104190030515     c                   ENDSR
104200030515      *
104210030515      *---------------------------------------------------------------*
104220050217      *?Decodifica LNA                                               ?*
104230030515      *---------------------------------------------------------------*
104240030515     c     DecLNA        BEGSR
104250030515      *
104260140206if  1c*//                if            DB0dla =  *blanks
104270140206     c*//                          and DB0ris <> 'M'
104280140206if  1c                   if        DB0ris <> 'M'
104290030515      *
104300040311     c                   eval      SAVlna = DB0lna
104310070312     c                   clear                   OG146arr
104320061016     c                   clear                   OG148arr
104330070312     c                   clear                   OG150arr
104340030515     c     DB0lna        chain     AZORG
104350030515      *
104360030515if  2c                   if        %found(AZORG01L)
104370030515     c                   eval      DB0fag = ORGfag
104380030515     c                   eval      DB0dla = ORGdes
104390030515     c                   eval      DB0inl = ORGind
104400030515     c                   eval      DB0cal = ORGcpf
104410030515     c                   eval      DB0lol = ORGloc
104420030515     c                   eval      DB0prl = ORGpro
104430030515     c                   eval      DB0tel = ORGtel
104440140129     c*//                eval      DB0fax = ORGfax
104450140129     c                   eval      DB0fax = ORGdd2
104460070312     c                   movel     ORGde6        OG146arr
104470061016     c                   movel     ORGde8        OG148arr
104480070312     c                   movel     ORGdf0        OG150arr
104490030515e   2c                   endif
104500030515      *
104510030515e   1c                   endif
104520030515      *
104530030515     c                   ENDSR
104540030515      *
104550030515      *---------------------------------------------------------------*
104560050217      *?Reperimento dati di tutta la tassazione per la stampa        ?*
104570030515      *---------------------------------------------------------------*
104580030515     c     RepTASSAZ     BEGSR
104590030515      *
104600030515     c                   eval      DB0imv = AR6imv
104610030515     c                   eval      DB0cei = AR6cei
104620030515     c                   eval      DB0ift = AR6ift
104630030911      * Esenzione IVA
104640030911if  1c                   if            DB0cei <> *blanks
104650030911     c                             and DB0dce =  *blanks
104660030911     c                   exsr      Tab_EI
104670030911e   1c                   endif
104680030515      *
104690030515      * Controllo se ci sono varie esenti
104700030515      *
104710030515      * - varia 1
104720030515     c                   eval      Wsvx = AR6sv1
104730030515     c                   eval      Wvax = AR6va1
104740030515     c                   exsr      RepTASSAZ_b
104750030515      * - varia 2
104760030515     c                   eval      Wsvx = AR6sv2
104770030515     c                   eval      Wvax = AR6va2
104780030515     c                   exsr      RepTASSAZ_b
104790030515      * - varia 3
104800030515     c                   eval      Wsvx = AR6sv3
104810030515     c                   eval      Wvax = AR6va3
104820030515     c                   exsr      RepTASSAZ_b
104830030515      * - varie ulteriori solo per tipo record 1 (prima bolla)
104840040607     c     KLDVtrc       setll     FIAR7000
104850040607     c     KLDVtrc       reade     FIAR7000
104860030515      *
104870030515do  2c                   Dow       Not %eof(FIAR701L)
104880030515      *
104890030515     c                   eval      Wsvx = AR7svn
104900030515     c                   eval      Wvax = AR7van
104910030515     c                   exsr      RepTASSAZ_b
104920030515      *
104930040607     c     KLDVtrc       reade     FIAR7000
104940030515e   2c                   Enddo
104950030515      *
104960030515     c                   ENDSR
104970030515      *
104980030515      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
104990050217      *?Reperimento dati di tutta la tassazione per la stampa        ?*
105000050217      *?- 2ª parte -                                                 ?*
105010030515      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
105020030515     c     RepTASSAZ_b   BEGSR
105030030515      *
105040030515sel 1c                   Select
105050030515      *
105060030515w   1c                   When      Wsvx       = *blanks
105070030515      *
105080030515w   1c                   When      Wsvx       = §$2iva
105090030515     c                   eval      DB0iva     = Wvax
105100030515      *
105110030515w   1c                   When      Wsvx       = §$2bol
105120030515     c                   add       Wvax          DB0ese
105130050624     c                   z-add     Wvax          wBolloVirt
105140050624     c                   eval      $BolloVirt = *on
105150030515      *
105160030515x   1c                   Other
105170050624     c                   eval      *in31      = *off
105180030515     c     Wsvx          lookup    $Var                                   31
105190030515     c   31              add       Wvax          DB0ese
105200030515     c   31              sub       Wvax          DB0imv
105210030515      *
105220030515w   1c                   Endsl
105230030515      *
105240030515     c                   ENDSR
105250030515      *
105260030515      *---------------------------------------------------------------*
105270050217      *?Chain su TABEL per le PARTICOLARITA'                         ?*
105280030515      *---------------------------------------------------------------*
105290030515     c     TABxPAR       BEGSR
105300030515      *
105310100513     c                   movel(p)  WCdPar        w053a
105320030515     c                   movel     '7V'          TBLcod
105330030515     c                   movel(p)  KeyPAR        TBLkey
105340030515     c     K03TAB        chain     TABEL
105350030515      *
105360030515if  1c                   if        %found(TABEL00F)
105370030515     c                             and TBLflg = *blanks
105380100513     c                   eval      w053a = WCdPar + '-'
105390100513     c                                   + %trim(%subst(TBLuni:1:49))
105400030515x   1c                   else
105410030515     c                   clear                   TBLuni
105420030515e   1c                   endif
105430030515      *
105440030515     c                   ENDSR
105450030515      *
105460030515      *---------------------------------------------------------------*
105470050217      *?CONTROLLO C/A SE E' DA CONVERTIRE IN EURO                    ?*
105480030515      *---------------------------------------------------------------*
105490030515     c     CtrCAs        BEGSR
105500030515      *
105510030515      * E' da convertire
105520030515if  1c                   if        DB0ucm = 'S'
105530030515     c                   eval      $CAeur = *on
105540030515e   1c                   endif
105550030515      *
105560030515      * Se in EURO è da convertire e da stampare in EURO
105570030515if  1c                   if        DB0vca = §GEDcn
105580030515     c                   eval      $CAeur = *on
105590030515     c                   endif
105600030515      *
105610030515      * Devo convertire il c/a da valuta a moneta di conto
105620030624if  1c                   if        DB0vca <> §GEDcn
105630030515     c                   clear                   YEURCODS
105640030515     c                   movel     DB0vca        YECdvi
105650030515     c                   z-add     DB0cas        YECimi
105660030515     c                   movel     §GEDcn        YECdvo
105670030515     c                   call      'YEURCO'
105680030515     c                   parm                    YEURCODS
105690030515if  2c                   if        YECesi = *blanks
105700030515     c                   z-add     YECimo        Wcas
105710030515e   2c                   endif
105720030515x   1c                   else
105730030515     c                   z-add     DB0cas        Wcas
105740030515e   1c                   endif
105750030515      *
105760030515     c                   ENDSR
105770030515      *
105780030515      *---------------------------------------------------------------*
105790050217      *?CONTROLLO DIVISA DELLA FATTURA                               ?*
105800030515      *---------------------------------------------------------------*
105810030515     c     CtrDIV        BEGSR
105820030515      *
105830030515     c                   clear                   dsCV
105840030515      *
105850030515     c                   movel     'CV'          TBLcod
105860030515     c                   movel(p)  DB0div        TBLkey
105870030515     c     K03TAB        chain     TABEL
105880030515     c                   if        %found(TABEL00F)
105890030515     c                   movel     TBLuni        dsCV
105900030515     c                   movel     §CVfdc        DB0fdc
105910030515     c                   endif
105920030515      *
105930030515     c                   ENDSR
105940030515      *
105950030515      *---------------------------------------------------------------*
105960050217      *?SE BOLLA CON MAMMA PRENDO C/A E SEGNACOLLI DA BOLLA MAMMA    ?*
105970030515      *---------------------------------------------------------------*
105980081117     c     BollaFiglia   BEGSR
105990030515      *
106000090528     c                   if        not %open(FNLBL01L)
106010090528     c                   open      FNLBL01L
106020090528     c                   endif
106030090528      *
106040040607     c     KLDV          chain     FNLBL000
106050030515if  1c                   if        %found(FNLBL01L)
106060030515      *
106070030515     c     KLBL          chain     FNARB000
106080030515if  2c                   if        %found(FNARB01L)
106090030515      *
106100031015     c                   eval      wB0nrs = ARBnrs
106110030515     c                   eval      DB0fns = ARBfns
106120030515     c                   eval      DB0fls = ARBfls
106130030515     c                   eval      DB0ncd = ARBncd
106140030515     c                   eval      DB0nca = ARBnca
106150050114     c**!!!              eval      DB0xco = ARBxco
106160030515     c                   eval      DB0znc = ARBznc
106170030515      * Aggancio AR9
106180060505     c     KLBL          chain     FIAR9000
106190060505if  3c                   IF        %found(FIAR901L)
106200030515     c                   eval      DB0cas = AR9cas
106210030515     c                   eval      DB0vca = AR9vca
106220030515     c                   eval      DB0gca = AR9gca
106230030515     c                   eval      DB0tic = AR9tic
106240030515x   3c                   ELSE
106250030515     c                   clear                   DB0cas
106260030515     c                   clear                   DB0vca
106270030515     c                   clear                   DB0gca
106280030515     c                   clear                   DB0tic
106290030515     c                   clear                   DB0dti
106300030515     c                   clear                   DB0ucm
106310030515     c                   clear                   DB0ste
106320030515e   3c                   ENDIF
106330030515      *
106340030515e   2c                   endif
106350030515      *
106360030515e   1c                   endif
106370030515      *
106380030515     c                   ENDSR
106390030903      *
106400030903      *---------------------------------------------------------------*
106410050217      *?Impostazione Quantità e Unità di Misura da FNORM             ?*
106420030903      *---------------------------------------------------------------*
106430030903     c     QUM_FNORM     BEGSR
106440030903      *
106450030905     c*** (già fatto:)   clear                   $Qum
106460030905     c                   clear                   Wx
106470030903      * Colli
106480030905if  1c                   if        ORMncl   > *zeros
106490030905     c                             and Wx   < %elem($Qum)
106500030905     c                   add       1             wx
106510030905     c                   eval      $Qum(wx) = %editc(ORMncl : '4')
106520030905     c                                      + ' colli'
106530030903e   1c                   endif
106540030903      * Peso
106550030905if  1c                   if        ORMpkg   > *zeros
106560030905     c                             and Wx   < %elem($Qum)
106570030905     c                   add       1             wx
106580030905     c                   eval      $Qum(wx) = %editc(ORMpkg : '4')
106590030905     c                                      + ' kg'
106600030905e   1c                   endif
106610071025      * Volume bancali
106620071025if  1c                   if        ORMvlm   > *zeros
106630071025     c                             and Wx   < %elem($Qum)
106640071025     c                   add       1             wx
106650071025     c                   eval      $Qum(wx) = %editc(ORMvlm : '4')
106660071025     c                                      + ' mc'
106670071025e   1c                   endif
106680030905     *** * Volume
106690030905     *** * (in ultimo posto SOLO SE non ci sono bilici/motrici/autotreni)
106700030905if  1***c                   if        ORMvlm   > *zeros
106710030905     ***c                             and (Wx  < %elem($Qum) - 1
106720030905     ***c                              or  Wx  = %elem($Qum) - 1
106730030905     ***c                              and w0112 = *zeros)
106740030905     ***c                   add       1             wx
106750030905     ***c                   eval      $Qum(wx) = %editc(ORMvlm : '4')
106760030905     ***c                                      + ' metri cubi'
106770030905e   1***c                   endif
106780030905     *** * Bancali
106790030905     *** * (in ultimo posto SOLO SE non ci sono bilici/motrici/autotreni)
106800030905if  1***c                   if        ORMbnc   > *zeros
106810030905     ***c                             and (Wx  < %elem($Qum) - 1
106820030905     ***c                              or  Wx  = %elem($Qum) - 1
106830030905     ***c                              and w0112 = *zeros)
106840030905     ***c                   add       1             wx
106850030905     ***c                   eval      $Qum(wx) = %editc(ORMbnc : '4')
106860030905     ***c                                      + ' bancali'
106870030905e   1***c                   endif
106880030905     *** * Bilici
106890030905if  1***c                   if        ORMblc  > *zeros
106900030905     ***c                             and Wx  < %elem($Qum)
106910030905     ***c                   add       1             wx
106920030905     ***c                   eval      $Qum(wx) = %editc(ORMblc : '4')
106930030905     ***c                                      + ' bilici'
106940030905e   1***c                   endif
106950030905     *** * Motrici
106960030905if  1***c                   if        ORMmtc  > *zeros
106970030905     ***c                             and Wx  < %elem($Qum)
106980030905     ***c                   add       1             wx
106990030905     ***c                   eval      $Qum(wx) = %editc(ORMmtc : '4')
107000030905     ***c                                      + ' motrici'
107010030905e   1***c                   endif
107020030905     *** * Autotreni
107030030905if  1***c                   if        ORMatt  > *zeros
107040030905     ***c                             and Wx  < %elem($Qum)
107050030905     ***c                   add       1             wx
107060030905     ***c                   eval      $Qum(wx) = %editc(ORMatt : '4')
107070030905     ***c                                      + ' autotreni'
107080030905e   1***c                   endif
107090030903      *
107100030903     c                   ENDSR
107110130306      *
107120130306      /free
107130130306
107140130306       //--------------------------------------------------------------
107150130306       //?Scrittura record (relativo alla spedizione) nel *file PDFLIST?
107160130306       //--------------------------------------------------------------
107170130306       BEGSR  sr_WrtRec_PdfList;
107180130306
107190130307         // -?NON richiesta stampa del PDF?
107200140404         if  %parms() = 1;
107210130306           leavesr;
107220130306         endif;
107230130306
107240130306         // -?Reperimento numero "chiave elenco"?(SE non ricevuto)?
107250130306         If  DB6num <= *zero;
107260130306
107270130306           clear  TRUL33ds;
107280130306           I33tla = 'L';
107290130306           I33ope = *zero;
107300130306           I33cnu = 55;
107310130306           I33num = 1;
107320130306           kpjbu  = TRUL33ds;
107330130306
107340130306           TRUL33R ( kpjba );
107350130306
107360130306           TRUL33ds = kpjbu;
107370130307           if  O33err > *zero;
107380130307             Pmsg1  = $Msg(3);
107390130307             Pmsg2  = $Msg(O33err + 3);
107400130418             *in33  = *off;
107410130418             exsr  Posiz_Box;
107420130307             write  A4prtMSG;
107430130307             DB0tms = 'E';
107440130306             exsr  sr_End;
107450130306           endif;
107460130306
107470130306           DB6num = %editc( O33nri : 'X' );
107480130306
107490130306         EndIf;
107500130307
107510130307         // -?Apertura file?
107520130307         if  Not %open(PDFLIST);
107530130307           open  PDFLIST;
107540130307         endif;
107550130306
107560130308         // -?Ciclo di lettura record "§..." (ex "§PDF") in FIAR801L?
107570130306         clear  k06fiar801;
107580130306         k_AR8aas = DB0aas;
107590130306         k_AR8lnp = DB0lnp;
107600130306         k_AR8nrs = DB0nrs;
107610130306         k_AR8nsp = DB0nsp;
107620130306         k_AR8tmd = c_§PDF;
107630130306         setll  %kds( k06fiar801 : 5 )  FIAR8000;
107640130308         reade  %kds( k06fiar801 : 4 )  FIAR8000;
107650130306
107660130308         DoW  Not %eof(FIAR801L)  and  %subst(AR8tmd : 1 : 1) = '§';
107670130306
107680130306           dAR8§PDF = AR8uni;
107690130308
107700130315           If  AR8atb = *blank  and  §AR8Ptrt = 'S';
107710130306
107720130308             // -?Scrittura del rec. relativo alla sped. nel *file PDFLIST?
107730130308             clear  RPDF;
107740130308             PDFklist  = DB6num;
107750130308             PDFksped  = %editc( DB0aas : 'X' ) + %editc( DB0lnp : 'X' ) +
107760130308                         %editc( DB0nrs : 'X' ) + %editc( DB0nsp : 'X' );
107770130311             PDFkey    = %trimr(§AR8PpdfP) + '_' + §AR8PpdfN;
107780130311             //PDFpath   = *blank;
107790130308             PDFinfo   = c_CopiaXdest + ' - ' + Prsd +
107800130308                         ' Spedizione ' + %editc(DB0lnp : 'X') +
107810130308                                    ' ' + %editc(DB0nrs : 'X') +
107820130308                                    ' ' + %editc(DB0nsp : 'X') +
107830130308                         ' del ' + %editc(Pdsp : 'Y') + ' ' +
107840130308                         Pndc;
107850130308             PDFresol  = 'S';
107860130308             PDFcopieM = '1';
107870130308             PDFcopieD = '0';
107880130308             PDFcopieS = '0';
107890130308             //PDFnoInfo = *blank;
107900130308             //PDFinfoP  = *blank;
107910130308             //PDFprtFst = *blank;
107920130308             wTime_ds  = %char( %timestamp() );
107930131008             PDFinsDt  = wTime_ds.wDate_Iso;
107940131008             PDFinsTm  = wTime_ds.wTime;
107950131008             //PDFprtDt  = wTime_ds.wDate_Iso;
107960131008             //PDFprtTm  = wTime_ds.wTime;
107970130308             PDFusrins = SDSusr;
107980130308             //__________
107990130308             write  RPDF;
108000130308             //¯¯¯¯¯¯¯¯¯¯
108010130308
108020130308           EndIf;
108030130306
108040130418           reade  %kds( k06fiar801 : 4 )  FIAR8000;
108050130306
108060130306         EndDo;
108070130306
108080130306       ENDSR;
108090130306
108100130306       //--------------------------------------------------------------
108110140303       //?Stampa immagini associate alle spediz. (dati già in PDFLIST) ?
108120130306       //--------------------------------------------------------------
108130130306       BEGSR  sr_PrtPdf;
108140130306
108150130306         // -?Impostazione parametri?
108160130312         PDF_KList    = DB6num;
108170130312         PDF_Printer  = trulPOVRds.§ULPoOutQ;
108180130312         PDF_FormType = trulPOVRds.§ULPoFType;
108190130312         PDF_UsrDta   = trulPOVRds.§ULPoUDta;
108200130306
108210130306         // -?Richiamo funzione di stampa di tutte le immagini PDF?
108220130312         //  ?(segnalando - in stampa - l'eventuale errore)?
108230130312         callp(E)  Stampa_PDF ( PDF_KList :
108240130312                                PDF_Printer :
108250130312                                PDF_FormType :
108260130312                                PDF_UsrDta );
108270130312
108280130312         if  %error;
108290130312           Pmsg2  = $Msg(7);
108300130418           *in33  = *off;
108310130418           exsr  Posiz_Box;
108320130312           write  A4prtMSG;
108330130312           DB0tms = 'E';
108340130312           exsr  sr_End;
108350130312         endif;
108360130307
108370130307         // -?Restituzione tipo modulo stampato?
108380130307         DB0tms = 'F';
108390130306
108400130306       ENDSR;
108410130306
108420130306      /end-free
108430030903      *
108440030515      *---------------------------------------------------------------*
108450050217      *?DEFINIZIONE KEY-LIST                                         ?*
108460030515      *---------------------------------------------------------------*
108470030515     c     DefKLIST      BEGSR
108480030515      *
108490051115      * - FNBLP01L / FNBLT01L /
108500060505      *   FNARB01L / FNART01L /
108510030515      *   FNLBL01L /
108520060505      *   FIAR501L/31C / FIAR901L
108530040607     c     KLDV          klist
108540030515     c                   kfld                    DB0aas
108550030515     c                   kfld                    DB0lnp
108560030515     c                   kfld                    DB0nrs
108570030515     c                   kfld                    DB0nsp
108580060505      * - FIAR401L / FIAR601L / FIAR701L
108590060505     c     KLDVtrc       klist
108600060505     c                   kfld                    DB0aas
108610060505     c                   kfld                    DB0lnp
108620060505     c                   kfld                    DB0nrs
108630060505     c                   kfld                    DB0nsp
108640060505     c                   kfld                    Wtrc
108650040607      * - FIAR501L/31C
108660030717     c     K05AR501      klist
108670030717     c                   kfld                    DB0aas
108680030717     c                   kfld                    DB0lnp
108690030717     c                   kfld                    DB0nrs
108700030717     c                   kfld                    DB0nsp
108710030717     c                   kfld                    AR5trd
108720040607      * - FIAR801L
108730040607     c     K06AR801      klist
108740040607     c                   kfld                    DB0aas
108750040607     c                   kfld                    DB0lnp
108760040607     c                   kfld                    DB0nrs
108770040607     c                   kfld                    DB0nsp
108780040607     c                   kfld                    AR8tmd
108790040607     c                   kfld                    AR8prr
108800070514      * - FNBLT01L / FNART01L
108810070514     c     KLDVnsc       Klist
108820070514     c                   Kfld                    DB0aas
108830070514     c                   Kfld                    DB0lnp
108840070514     c                   Kfld                    DB0nrs
108850070514     c                   Kfld                    DB0nsp
108860070514     c                   Kfld                    Wnsc
108870070319      * - FIARN11L
108880070319     c     K06ARN11      klist
108890070319     c                   kfld                    DB0aas
108900070319     c                   kfld                    DB0lnp
108910070319     c                   kfld                    DB0nrs
108920070319     c                   kfld                    DB0nsp
108930070319     c                   kfld                    ARNcdn
108940070319     c                   kfld                    ARNgst
108950090528      * - FNBLP01L/ / FNARB01L / FIAR901L di bolla MAMMA
108960070514     c     KLBL          klist
108970070514     c                   kfld                    LBLaap
108980070514     c                   kfld                    LBLlpp
108990070514     c                   kfld                    LBLnrp
109000070514     c                   kfld                    LBLnsp
109010070514      * - FNBLT01L / FNART01L di bolla MAMMA
109020070514     c     KLBLnsc       Klist
109030070514     c                   kfld                    LBLaap
109040070514     c                   kfld                    LBLlpp
109050070514     c                   kfld                    LBLnrp
109060070514     c                   kfld                    LBLnsp
109070070514     c                   Kfld                    Wnsc
109080070514      * - FIARS01L
109090070514     c     K05ARS01P     klist
109100070514     c                   kfld                    BLTfls
109110070514     c                   kfld                    BLTlna
109120070514     c                   kfld                    BLTnrs
109130070514     c                   kfld                    BLTnsc
109140070514     c                   kfld                    ARStrc
109150070514     c     K05ARS01A     klist
109160070514     c                   kfld                    ARTfls
109170070514     c                   kfld                    ARTlna
109180070514     c                   kfld                    ARTnrs
109190070514     c                   kfld                    ARTnsc
109200070514     c                   kfld                    ARStrc
109210060505      * - FNORM01L
109220060505     c     K04ORM01      klist
109230071015     c                   kfld                    ORMpoe
109240071015     c                   kfld                    ORMnsr
109250071015     c                   kfld                    ORMnor
109260071015     c                   kfld                    ORMnrv
109270030515      * - TABEL00F
109280030515     c     K03TAB        klist
109290040223     c                   kfld                    TBLkut
109300030515     c                   kfld                    TBLcod
109310030515     c                   kfld                    TBLkey
109320030515     c     K02TAB        klist
109330040223     c                   kfld                    TBLkut
109340030515     c                   kfld                    TBLcod
109350030515      * - FLNUF01L
109360030515     c     K04NUF        klist
109370030515     c                   kfld                    NUFaaa
109380030515     c                   kfld                    NUFcnu
109390030515     c                   kfld                    NUFfil
109400030515     c                   kfld                    NUFfl2
109410030515      *
109420030515     c                   ENDSR
109430030515
109440030515** > $Msg ---------------------------------------------------------------------*
109450030515IMPOSSIBILE CONTINUARE LA RISTAMPA BOLLE
109460071112PERCHE' STAMPATE PIU' DI &NrLdV BOLLE !!!
109470130307ERRORE IN REPERIMENTO NUMERATORE PER STAMPA PDF ALLEGATO ALLA LDV:
109480130307   NON REPERITO IL NUMERATORE
109490130307   NUMERATORE ALLOCATO
109500130307   SUPERATO IL LIMITE MASSIMO PER IL NUMERATORE
109510130312RILEVATO ERRORE IN FASE DI STAMPA DEI DOCUMENTI ALLEGATI
109520140303** > $GgS/$Gio *
1095300306241LUNEDI
1095400306242MARTEDI
1095500410263MERCOLEDI
1095600306244GIOVEDI
1095700306245VENERDI
1095800305156SABATO
1095900305157DOMENICA
109600030515M          MATT.
109610030515P          POM.
