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
000830110411       // -?O.R.M.?
000840030903     fFNORM01L  if   e           k disk    usropn
000850140410     fFNORE01L  if   e           k disk    usropn
000860130306
000870130306       // -?Elenco PDF da stampare?
000880130306     fPDFLIST   o    e             disk    usropn
000890130306
000900110411       // -?Printer File?
000910130710     fFNLSB6P   o    e             printer usropn infds(prtINFOa4)
000920080604     f                                     rename(LSB6POS :A4prtPOS )
000930130418     f                                     rename(LSB6BOX :A4prtBOX )
000940120530     f*//                                  rename(LSB6LOGBAR:A4LogoBAR)
000950120530     f*//                                  rename(LSB6LOGDA1:A4LogoDA1)
000960120530     f*//                                  rename(LSB6LOGBR2:A4LogoBR2)
000970120530     f*//                                  rename(LSB6LOGDA2:A4LogoDA2)
000980120530     f                                     rename(LSB6LOGBRT:A4LogoBRT)
000990120530     f                                     rename(LSB6LOGDA3:A4LogoDA3)
001000130418     f                                     rename(LSB6LDV :A4prtLDV )
001010080604     f                                     rename(LSB6LDVB:A4prtLDVB)
001020080922     f                                     rename(LSB6DPD :A4prtDPD )
001030080604     f                                     rename(LSB6FED1:A4prtFED1)
001040080604     f                                     rename(LSB6FED2:A4prtFED2)
001050080604     f                                     rename(LSB6FED3:A4prtFED3)
001060080604     f                                     rename(LSB6AR51:A4prtAR51)
001070080604     f                                     rename(LSB6AR52:A4prtAR52)
001080080604     f                                     rename(LSB6AR53:A4prtAR53)
001090080604     f                                     rename(LSB6AR54:A4prtAR54)
001100080604     f                                     rename(LSB6AR5A:A4prtAR5A)
001110150226     f*//                                  rename(LSB6AR5B:A4prtAR5B)
001120150226     f*//                                  rename(LSB6AR5C:A4prtAR5C)
001130080604     f                                     rename(LSB6AR5G:A4prtAR5G)
001140080604     f                                     rename(LSB6DB0 :A4prtDB0)
001150080604     f                                     rename(LSB6DT0 :A4prtDT0)
001160080604     f                                     rename(LSB6DT1 :A4prtDT1)
001170080604     f                                     rename(LSB6DT2 :A4prtDT2)
001180080604     f                                     rename(LSB6DC1 :A4prtDC1)
001190080604     f                                     rename(LSB6DK0 :A4prtDK0)
001200080604     f                                     rename(LSB6DK1 :A4prtDK1)
001210080604     f                                     rename(LSB6DK2 :A4prtDK2)
001220080604     f                                     rename(LSB6DK3 :A4prtDK3)
001230080604     f                                     rename(LSB6DK4 :A4prtDK4)
001240080604     f                                     rename(LSB6DK5 :A4prtDK5)
001250081222     f                                     rename(LSB6DLL :A4prtDLL)
001260080604     f                                     rename(LSB6DL0 :A4prtDL0)
001270080604     f                                     rename(LSB6DL0A:A4prtDL0A)
001280080604     f                                     rename(LSB6DL0B:A4prtDL0B)
001290080604     f                                     rename(LSB6DL0C:A4prtDL0C)
001300080604     f                                     rename(LSB6DL0D:A4prtDL0D)
001310080604     f                                     rename(LSB6DL0E:A4prtDL0E)
001320080604     f                                     rename(LSB6DL0F:A4prtDL0F)
001330080604     f                                     rename(LSB6DL1A:A4prtDL1A)
001340080604     f                                     rename(LSB6DL1B:A4prtDL1B)
001350080604     f                                     rename(LSB6DL1C:A4prtDL1C)
001360080604     f                                     rename(LSB6DL1D:A4prtDL1D)
001370080604     f                                     rename(LSB6DL1E:A4prtDL1E)
001380080604     f                                     rename(LSB6DL2 :A4prtDL2)
001390080604     f                                     rename(LSB6DL21:A4prtDL21)
001400080604     f                                     rename(LSB6DL3 :A4prtDL3)
001410090323     f                                     rename(LSB6DL31:A4prtDL31)
001420080604     f                                     rename(LSB6DL4 :A4prtDL4)
001430080604     f                                     rename(LSB6DL5 :A4prtDL5)
001440080604     f                                     rename(LSB6DL6 :A4prtDL6)
001450080604     f                                     rename(LSB6DL7 :A4prtDL7)
001460080604     f                                     rename(LSB6DL8 :A4prtDL8)
001470080604     f                                     rename(LSB6DL9 :A4prtDL9)
001480080604     f                                     rename(LSB6AC0 :A4prtAC0)
001490080604     f                                     rename(LSB6AC0A:A4prtAC0A)
001500080604     f                                     rename(LSB6AC0B:A4prtAC0B)
001510080604     f                                     rename(LSB6AC1A:A4prtAC1A)
001520080604     f                                     rename(LSB6AC1B:A4prtAC1B)
001530080604     f                                     rename(LSB6AC2 :A4prtAC2)
001540080604     f                                     rename(LSB6AC3 :A4prtAC3)
001550080604     f                                     rename(LSB6AC4 :A4prtAC4)
001560080604     f                                     rename(LSB6ES0 :A4prtES0)
001570080604     f                                     rename(LSB6ES0A:A4prtES0A)
001580080604     f                                     rename(LSB6ES0B:A4prtES0B)
001590080604     f                                     rename(LSB6ES1A:A4prtES1A)
001600080604     f                                     rename(LSB6ES1B:A4prtES1B)
001610080604     f                                     rename(LSB6ES2 :A4prtES2)
001620080604     f                                     rename(LSB6ES3 :A4prtES3)
001630080604     f                                     rename(LSB6ES4 :A4prtES4)
001640080604     f                                     rename(LSB6SP0 :A4prtSP0)
001650080604     f                                     rename(LSB6SP1 :A4prtSP1)
001660080604     f                                     rename(LSB6NUL :A4prtNUL)
001670080604     f                                     rename(LSB6GEN :A4prtGEN)
001680080604     f                                     rename(LSB6LO  :A4prtLO)
001690080604     f                                     rename(LSB6SEG :A4prtSEG)
001700080604     f                                     rename(LSB6MSG :A4prtMSG)
001710030515
001720110427       //--------------------------------------------------------------
001730110427       //?Definizione costanti.                                        ?
001740110427       //--------------------------------------------------------------
001750110427
001760110427       // -?Intestazioni in stampa?
001770120530     d*// $Bartolini      c                   const('BARTOLINI S.p.A.')
001780110426     d $BRT            c                   const('BRT S.p.A.')
001790090414     d*// $CorrEspr       c                   const('CORRIERE  ESPRESSO')
001800060503     d $Intest1        c                   const('Sede Operativa ed Am-
001810060503     d                                     ministrativa:')
001820060503     d $Intest2        c                   const('via E. Mattei, 42 -
001830110426     d                                     - 40138 Bologna')
001840110426     d*//                                  - 40138 BOLOGNA')
001850060503     d $Intest3        c                   const('Sede Legale: Piazza -
001860060503     d                                     Diaz, 7 - 20123 Milano')
001870070215     d $Intest4        c                   const('C.F. e P.IVA 04507990150')
001880061206     d $Intest5        c                   const(' Capitale Sociale Eu-
001890061221     d                                     ro 24.000.000 int. vers.')
001900060503     d $Intest6        c                   const('info on-line')
001910120530     d*// $Intest7        c                   const('www.bartolini.it')
001920120530     d $Intest7_brt    c                   const('www.brt.it')
001930090107     d $Intest2_b      c                   const('via Enrico Mattei, +
001940090107     d                                            42 - 40138 BOLOGNA')
001950120530     d*// $Intest3_b      c                   const('Tel. 051 6015411 - +
001960120530     d*//                                            Fax 051 530365 - +
001970120530     d*//                                            www.bartolini.it')
001980120530     d $Intest3_b_brt  c                   const('Tel. 051 6015411 - +
001990120530     d                                            Fax 051 530365 - +
002000120530     d                                            www.brt.it')
002010081222     d $Intest4_b      c                   const('C.F. - P.IVA - +
002020081222     d                                            Reg. Imprese Milano +
002030081222     d                                            n. 0457990150')
002040081222     d $Intest5_b      c                   const('Capitale Sociale € +
002050081222     d                                            24.000.000,00 i.v. +
002060081222     d                                            - REA Milano n. +
002070081222     d                                            1734257')
002080110427
002090110427       // -?DIMENSIONI & POSIZIONI:?
002100110427
002110110427       // -?Lunghezza (in inches) di una singola LdV?
002120080924     d $LenLdV2        c                   const(5.775)
002130080924     d $LenLdV4        c                   const(2.890)
002140110427       // -?Lunghezza di 1/4 di A4?
002150080924     d c_Len1_inch     c                   const(2.925)
002160080923     d c_Len2_inch     c                   const(5.850)
002170080923     d c_Len2_row8     c                   const(047)
002180110427       // -?Spessore delle linee in inches/pollici (*memo)?
002190080701     d c_LineNarrow    c                   const(0.008)
002200080701     d c_LineMedium    c                   const(0.017)
002210080701     d c_LineWide      c                   const(0.025)
002220110427       // -?Dimensioni base per i simil-loghi Bartolini e BRT?
002230120530      *///copy gaitrasrc/srcTNSY,LogoBAR_R0
002240120530      *///copy gaitrasrc/srcTNSY,LogoBRT3R0
002250120530      *///copy gaitrasrc/srcTNSY,LogoBRT_R0
002260120530      /copy gaitrasrc/srcTNSY,LogoBRT_R1
002270110427
002280110427       // -?Posizionamento all'inizio della pag. A4 successiva?
002290110427     d c_§PS1          c                   const(0.950)
002300110427
002310120530       // -?Posizioni dell'intestazione "BRT S.p.A." nella 1ª copia?
002320080704     d c_PosTBar       c                   const(0.270)
002330080704     d c_PosTBa1       c                   const(0.350)
002340110426     d c_PosTBa2       c                   const(0.420)
002350110426     d c_PosTBa3       c                   const(0.490)
002360110426     d c_PosTBa4       c                   const(0.560)
002370110426     d c_PosTBa5       c                   const(0.630)
002380110426     d c_PosTBa6       c                   const(0.710)
002390110426     d c_PosTBa2x      c                   const(0.430)
002400110426     d c_PosTBa3x      c                   const(0.500)
002410110426     d c_PosTBa4x      c                   const(0.590)
002420110426     d c_PosTBa5x      c                   const(0.660)
002430110426     d c_PosTBa6x      c                   const(0.750)
002440110427       // -?Posizioni dei bordi orizzontali superiori box nella 1ª copia?
002450110427       //  ?(distanza dal lato superiore dell'A4)?
002460110427       //  ?N.B.?- c_PosBoxNXs => posizione iniziale della linea?
002470110427       //                        ?orizzontale Superiore?
002480110427       //       ?- c_PosBoxNXi => posizione iniziale della linea?
002490110427       //                        ?orizzontale Inferiore?
002500080704     d c_PosBox1As     c                   const(0.200)
002510080704     d c_PosBox1Bs     c                   const(0.790)
002520080704     d c_PosBox1Bi     c                   const(2.850)
002530080704     d c_PosBox1Cs     c                   const(1.950)
002540100208     d c_PosBox1Ds     c                   const(2.453)
002550110427       // -?Posizioni di altre linee orizzontali nella 1ª copia?
002560080704     d c_PosLin1OS1    c                   const(1.350)
002570080704     d c_PosLin1OS2    c                   const(1.790)
002580080704     d c_PosLin1OS3    c                   const(2.010)
002590080704     d c_PosLin1OS4    c                   const(2.230)
002600091119     d c_PosLin1OS5    c                   const(2.470)
002610080704     d c_PosLin1OC1    c                   const(1.680)
002620080704     d c_PosLin1OC2    c                   const(1.240)
002630140915     d c_PosLin1OC3    c                   const(1.470)
002640080704     d c_PosLin1OD1    c                   const(1.010)
002650140917
002660140917       // -?Posizioni di altre costanti?
002670140917     d*// c_PosKBRTc      c                   const(0.970)     x FONT(00404)
002680140922     d c_PosKBRTc      c                   const(0.965)
002690140922     d*// c_PosKBRTc      c                   const(0.955)     x FONT(02305)
002700080704     d c_PosKTass      c                   const(2.130)
002710080704     d c_PosKRCV       c                   const(1.100)
002720080704     d c_PosKBAV       c                   const(1.850)
002730080709     d c_PosKCXD2      c                   const(3.100)
002740110427
002750110427       // -?Posizione minima superiore per FedEx?
002760030709     d $MinLine1       c                   const(012)
002770110427       // -?Limite righe stampabili su 1/4 di A4 in 1/9"?
002780081002     d $MaxLine1       c                   const(025)
002790110427       // -?Limite righe stampabili su 1/2 A4 in 1/8" per?
002800110427       //  ?Karnak/Titanedi/EDk + Arcte + Esprinet +?
002810110427       //  ?Logistica/Sait + Generico (con/senza logo)?
002820080822     d $MaxLine2       c                   const(068)
002830090323     d $MaxLine2_pl1   c                   const(044)
002840090323     d $MaxLine2_pl2   c                   const(090)
002850110406     d $MaxLine2_pl1k  c                   const(045)
002860110406     d $MaxLine2_pl2k  c                   const(091)
002870110427       // -?Limite righe stampabili su 1/2 A4 in 1/8" per?
002880110427       //  ?Karnak/Titanedi/EDk?
002890080822     d $MaxLine3       c                   const(059)
002900090324     d $MaxLine3_pl1   c                   const(036)
002910090324     d $MaxLine3_pl2   c                   const(082)
002920110427       // -?Limite righe stampabili su 1/2 A4 in 1/8" per?
002930110427       //  ?Logistica/Sait + Arcte?
002940090323     d $MaxLine4       c                   const(065)
002950090324     d $MaxLine4_pl1   c                   const(041)
002960090324     d $MaxLine4_pl2   c                   const(087)
002970110427       // -?Limite righe stampabili su 1/2 A4 in 1/8" per?
002980110427       //  ?Arcte + Esprinet?
002990090324     d $MaxLine4_pl3   c                   const(042)
003000090324     d $MaxLine4_pl4   c                   const(088)
003010090324     d $MaxLine5_pl1   c                   const(045)
003020090324     d $MaxLine5_pl2   c                   const(091)
003030110427       // -?Limite righe stampabili su 1/2 A4 in 1/8" per?
003040110427       //  ?Esprinet?
003050081002     d $MaxLine6       c                   const(057)
003060081002     d $MaxLine6_pl1   c                   const(034)
003070081002     d $MaxLine6_pl2   c                   const(080)
003080110427
003090110427       // -?Limiti di OverFlow?
003100040608     d $BoxRow1        c                   const(010)
003110080707     d $LimOF          c                   const(08.250)
003120080709     d $LimOF1         c                   const(02.900)
003130080709     d $LimOF2         c                   const(05.800)
003140080709     d $LimOF3         c                   const(08.700)
003150030515     d $elSNSxA5       c                   const(04)
003160100208     d c_DateMax_A     c                   const('20391231')
003170110427
003180110427       // -?COSTANTI IN STAMPA?
003190110426     d c_CopiaXdest    c                   const('COPIA PER IL DESTINA-
003200080919     d                                     TARIO')
003210110426     d c_ScNS          c                   const('ELENCO SEGNACOLLI -
003220030515     d                                     NON SEQUENZIALI')
003230110426     d c_CoOr          c                   const('COLLI ORIGINALI     -
003240030717     d                                     N. ')
003250110426     d c_DeSM          c                   const('DESTINATARIO SUPERME-
003260031124     d                                     RCATO')
003270110426     d c_PMxA          c                   const('PROMEMORIA PER L''-
003280080604     d                                     AUTOTRASPORTATORE')
003290110427       // -?ALTRE COSTANTI:?
003300110427       // ·?Codice tab.7V-R x "Destinatario assente: plico in buca"?
003310110502     d*// Ktab7V_CB       c                   const('CB')
003320110427       // ·?Codice tab.7V-R x "Restituzione DDT firmato al mittente"?
003330110502     d*// Ktab7V_C1       c                   const('C1')
003340110427       // ·?Codice tab.7V-R x "Restituzione copie del contratto firmato"?
003350110502     d*// Ktab7V_C2       c                   const('C2')
003360110427       // ·?Codice tab.7V-R x "Restituzione copie del contratto firmato"?
003370110427       //  ?                e "Restituzione copia della C.I. del dest."?
003380110502     d*// Ktab7V_C3       c                   const('C3')
003390110427       // -?Codice tab.7V-R x "Restituzione copia C.I. del destinatario"?
003400110411     d*// Ktab7V_C4       c                   const('C4')
003410110427       // -?Codice tab.7V-R x "Restituzione modulo Vodafone firmato"?
003420110411     d*// Ktab7V_C5       c                   const('C5')
003430110427       // -?Codice tab.7V-R x "Non effettuare ritiro di merce"?
003440110502     d*// Ktab7V_NR       c                   const('NR')
003450110427       // -?Codice tab.7V-R x "Ritiro contestuale alla consegna"?
003460110502     d*// Ktab7V_RC       c                   const('RC')
003470110427       // -?Codice tab.7V-R x C1+RC?
003480110502     d*// Ktab7V_R1       c                   const('R1')
003490110427
003500130418
003510110427       // -?Codice cliente "FedEx"?
003520081127     d c_CliFedEx      c                   const(0501331)
003530150429     d c_CliFedEx2     c                   const(0506514)
003540110427       // -?Codice cliente "PraOil oleodotti italiani"?
003550081127     d c_CliPraOil     c                   const(0201515)
003560110427       // -?Codice cliente "TTL"?
003570091116     d c_CliTTL        c                   const(0504450)
003580110427       // -?Codice cliente "Consorzio Lotterie Nazionali - Gratta & Vinci"?
003590090527     d c_CliCLN        c                   const(0632910)
003600140513     d c_CliLN_2       c                   const(0017093)
003610110427       // -?Codice cliente "S G Soluzioni Logistiche"?
003620090611     d c_CliSG         c                   const(0634923)
003630091117     d c_CliSG_2       c                   const(0635042)
003640130418
003650091117
003660110427       // -?Costante per evitare la stampa del contratto x "S.G."?
003670091117     d c_NoContr       c                   const('NO CONTRATTO')
003680130306
003690130306       // -?Costante per identificare la presenza di PDF del cliente?
003700130308       //  ?in FIAR800F.AR8TMD (nel suo 1° byte)?
003710130308     d c_§PDF          c                   const('§')
003720110427
003730110427       // -?Costante per controllo "caratteri solo numerici"?
003740110426     d c_Digits        c                   const('1234567890')
003750140129
003760140129       // -?Costanti per conversione caratteri maiuscoli/minuscoli?
003770140129     d c_Mai           c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
003780140129     d c_Min           c                   const('abcdefghijklmnopqrstuvwxyz')
003790110427
003800110427       // -?Comando di override al PrtF?
003810100208     d c_CmdDltOvr     c                   const('DLTOVR +
003820130710     d                                            file(FNLSB6P) +
003830080708     d                                            lvl(*job)')
003840110427
003850110427       //--------------------------------------------------------------
003860110427       //?Definizione schiere.                                         ?
003870110427       //--------------------------------------------------------------
003880110427
003890110427       // -?Messaggi da stampare?
003900130312     d $Msg            s            100    dim(7)   ctdata  perrcd(1)
003910110427       // -?Giorni della settimana e relativa decodifica?
003920070306     d $GgS            s              1    dim(9)   ctdata  perrcd(1)
003930070306     d $Gio            s             15    dim(9)   alt($GgS)
003940110427       // -?Varie esenti IVA?
003950070319     d $Var            s              1    dim(10)
003960110427       // -?Note e Particolarità?
003970070306     d $NP             s                   dim(6)   inz  like(Pnp1)
003980110427       // -?Segnacolli NON sequenziali (memo-1- e stampa-2-)?
003990070306     d $SC1            s              7  0 dim(14)  inz
004000070306     d $SC2            s              8    dim(14)  inz
004010110427       // -?Tab. "2A" (Causali Eventi): causali con tipo consegna = "G"?
004020070308     d $2Acev_G        s                   dim(999) inz like(EVBcev)
004030110427       // -?Quantità (editate) - a sx - e Unità di misura - a dx -?
004040070306     d $QUM            s             20    dim(3)   inz
004050110427       // -?Quantità (editate) - a sx - e Unità di misura - a dx -?
004060070306     d $AR8o1          s                   dim(10)  inz  like(§AR8o1a)
004070110427       // -?Consegne Parziali Precedenti?
004080070319     d $CP             s                   dim(20)  inz(*all'9')
004090070319     d                                                   like(Elem_CP)
004100070319     d $CPdcm          s                   dim(20)  inz  like(ARTdcm)
004110110427
004120110427       //--------------------------------------------------------------
004130110427       //?Definizione strutture dati.                                  ?
004140110427       //--------------------------------------------------------------
004150110427
004160110427       // -?Parametri ricevuti?
004170030515     d KPJBA         e ds
004180110427       // ·?Dati LDV da stampare?
004190080703     d FNLSB5ds      e ds
004200130306     d FNLSB6ds1     e ds
004210110427       // -?Flag operativi DB0FLO di FNLSB5DS?
004220080606     d dDB0flo       e ds                  inz
004230110427
004240110427       // -?Parametri x Controllo profilo utenti?
004250040223     d TIBS34ds      e ds
004260110427       // -?Ds di riferimento al file esterno AZUTE00F?
004270040223     d AZUTEds       e ds                  extname(AZUTE00F)
004280110427       // -?Ds per dati organigramma?
004290040223     d dDATIUTE      e ds
004300110427
004310110427       // -?Conversioni in Euro?
004320061107     d YEURCOds      e ds                  inz
004330110427
004340110427       // -?Dati da AZORG00F?
004350070424     d OG143ccm      e ds                  inz     extname(OG143)
004360070424     d                                             prefix(§CM:3)
004370030515     d OG143         e ds                  inz
004380070424     d OG146arr      e ds                  inz     extname(OG146)
004390070424     d                                             prefix(A)
004400070424     d OG146cf£6     e ds                  inz     extname(OG146)
004410070424     d                                             prefix(£6:1)
004420030515     d OG148         e ds                  inz
004430070424     d OG148arr      e ds                  inz     extname(OG148)
004440070424     d                                             prefix(A)
004450070424     d OG148cf£6     e ds                  inz     extname(OG148)
004460070424     d                                             prefix(£6:1)
004470070424     d OG150arr      e ds                  inz     extname(OG150)
004480070424     d                                             prefix(A)
004490110427
004500110427       // -?Dati da FIAR400F?
004510061108     d dsBL4A        e ds                  inz
004520061108     d dsBL4I        e ds                  inz
004530071015     d dsBL4M        e ds                  inz
004540061108     d dTA4Q         e ds                  inz
004550110427
004560110427       // -?Flag operativi di FNORM - campo ORMFLO?
004570110427     d dORM01        e ds                  inz
004580140410
004590140410       // -?Dati per trc "O " del file FNORE (orari apertura)?
004600140410     d dOREorari     e ds
004610110427
004620110427       // -?Tabelle?
004630061107     d ds$2          e ds
004640061107     d ds1A          e ds                  inz
004650061107     d ds1P          e ds                  inz
004660070306     d ds2A          e ds                  inz
004670061107     d ds3A          e ds                  inz
004680061107     d ds5E          e ds                  inz
004690061107     d ds7R          e ds                  inz
004700061107     d dsCC          e ds
004710061107     d dsCV          e ds                  inz
004720061107     d dsEI          e ds                  inz
004730061107     d dsTB          e ds                  inz
004740030805     d dGEI          e ds
004750030805     d dGED          e ds
004760150226     d*// dTBA          e ds                  inz
004770071112     d dVPO          e ds                  inz
004780120530     d*// dVPObrt       e ds                  inz
004790061031     d dVPOdeco      e ds                  inz
004800031124     d dAR5gen       e ds                  inz
004810150304     d*// dAR5ban       e ds                  inz
004820061030     d dAR5bnb       e ds                  inz
004830030903     d dAR5rcc       e ds                  inz
004840040223     d dAR5scr       e ds                  inz     prefix(§AR5S:4)
004850041207     d dAR8          e ds                  inz
004860041207     d  §AR8tsd      e                     inz('S1')
004870040607     d dAR8T1        e ds                  inz
004880040607     d dAR8T2        e ds                  inz
004890040607     d dAR8C1        e ds                  inz
004900040607     d dAR8C2        e ds                  inz
004910040607     d dAR8C3        e ds                  inz
004920040607     d dAR8V1        e ds                  inz
004930040607     d dAR8V2        e ds                  inz
004940040607     d dAR8L1        e ds                  inz
004950040607     d dAR8O1        e ds                  inz
004960040607     d dAR8O2        e ds                  inz
004970041026     d dAR8LOG1T1    e ds                  inz
004980041026     d dAR8LOG1T2    e ds                  inz
004990041026     d dAR8LOG1T3    e ds                  inz
005000041026     d dAR8LOG1C1    e ds                  inz
005010041026     d dAR8LOG1C2    e ds                  inz
005020041026     d dAR8LOG1C3    e ds                  inz
005030060614     d dAR8LOG1C4    e ds                  inz
005040041026     d dAR8LOG1P1    e ds                  inz
005050050318     d dAR8LOG1Z1    e ds                  inz
005060050606     d dAR8LOG1S1    e ds                  inz
005070050606     d dAR8LOG1S2    e ds                  inz
005080050606     d dAR8LOG1S3    e ds                  inz
005090050606     d dAR8LOG1S4    e ds                  inz
005100050606     d dAR8LOG1S5    e ds                  inz
005110050113     d dAR8ARCTT1    e ds                  inz
005120050114     d dAR8ARCTT2    e ds                  inz
005130050113     d dAR8ARCTC1    e ds                  inz
005140050113     d dAR8ARCTC2    e ds                  inz
005150050113     d dAR8ARCTP1    e ds                  inz
005160050214     d dAR8ESP1T1    e ds                  inz
005170050214     d dAR8ESP1T2    e ds                  inz
005180050214     d dAR8ESP1C1    e ds                  inz
005190050214     d dAR8ESP1C2    e ds                  inz
005200050214     d dAR8ESP1P1    e ds                  inz
005210050214     d dAR8ESP1P2    e ds                  inz
005220130306     d dAR8§pdf      e ds                  inz
005230110427     d TIBS02ds      e ds                  inz
005240110427     d   T02mod      e                     inz('C')
005250110427
005260110427       // -?Parametri per TRULINTR (determinaz. intestatario c/assegno)?
005270100208     d TRULINTds     e ds                  inz
005280100208     d   iINTtip     e                     inz('I')
005290110427       // -?Parametri per TRUL21R (calcolo limiti importo c/assegno)?
005300030515     d TRUL21ds      e ds                  inz
005310030515     d  I21tla       e                     inz('L')
005320110427       // -?Parametri per TRUL28R (calcolo check digit del barcode)?
005330030515     d TRUL28ds      e ds                  inz
005340030515     d  I28tla       e                     inz('L')
005350041019     d  I28mod       e                     inz('BAR')
005360110427       // -?Parametri per FNLV55R (reperimento terminal arr/par/£6)?
005370070212     d FNLV55ds      e ds                  inz
005380070212     d  D55tpt       e                     inz('6')
005390110427       // -?Parametri per FNLV80R (decodifica consegna richiesta)?
005400051129     d FNLV80ds      e ds                  inz
005410051129     d  ILV80tla     e                     inz('C')
005420110427       // -?Parametri per FIDN12R (ricerca C.A.')?
005430070319     d FIDN12ds      e ds                  inz
005440070319     d  I12fel       e                     inz('E')
005450070319     d  I12fan       e                     inz('E')
005460070319     d  I12fch       e                     inz('E')
005470110427       // -?Parametri per TNSD99R (calcolo tempi di delivery)?
005480090605     d TNSD99ds      e ds                  inz
005490090605     d  D98tla       e                     inz('C')
005500110427
005510110427       // -?Status ds?
005520030515     d Status         sds
005530130306     d   SDSpgm          *proc
005540130306     d*//SDSprm          *parms
005550130306     d   SDSusr              254    263
005560110427
005570110427       // -?InfDS?
005580030515     d PrtINFOa4       ds
005590080627     d  CurLineA4            367    368i 0
005600110427
005610110427       // -?Codice a Barre della LDV?
005620030515     d BARcodeDS       ds            17    inz
005630030515     d  BARcdeAAS                     2s 0 inz
005640030515     d  BARcdeLNP                     3s 0 inz
005650030515     d  BARcdeNRS                     2s 0 inz
005660030515     d  BARcdeNSP                     7s 0 inz
005670110427       // -?Codice a Barre dell'ORM?
005680040629     d BARcodeORM      ds            14    inz
005690040629     d  ORMpoe                             inz
005700040629     d  ORMnsr                             inz
005710040629     d  ORMnor                             inz
005720040629     d  ORMnrv                             inz
005730110427
005740110427       // -?Chiave di accesso ad eventuali Note?
005750030515     d KEYpar          ds             8    inz
005760030515     d  WTPpar                 1      1    inz
005770030515     d  WCDpar                 2      3    inz
005780030515     d  Wprogr                 4      8    inz('S0001')
005790110427
005800110427       // -?Dati delle eventuali consegne parziali precedenti?
005810140303     d Elem_CP         ds                  inz
005820140303     d   WCPdcm                       8s 0 inz
005830140303     d   WCPocm                       4s 0 inz
005840140303     d   WCPncl                       5s 0 inz
005850140303     d   WCPfir                            inz  like(EVB2dati)
005860130306
005870130306       // -?%TimeStamp?
005880130306     d wTime_ds        ds                  qualified     inz
005890130306     d   wDate_Iso                     d   datfmt(*iso)  inz
005900130306     d   wFiller                      1a                 inz
005910130307     d   wTime                         t   timfmt(*iso)  inz
005920110427
005930110427       //--------------------------------------------------------------
005940110427       //?Definizione variabili globali.                               ?
005950110427       //--------------------------------------------------------------
005960110427
005970110427       // -?Flags booleani?
005980110427     d $Mamma          s               n   inz
005990130215     d $Mamma_FedEx    s               n   inz
006000110427     d $FirstSCR       s               n   inz(*on)
006010110427     d $FIAR5          s               n   inz
006020110427     d $FIAR8          s               n   inz
006030110427     d $FIARN          s               n   inz
006040110427     d $E_AR8          s               n   inz(*on)
006050130306     d $§PDF           s               n   inz
006060110427     d $DDT_o          s               n   inz
006070110427     d $DDT_p          s               n   inz
006080110427     d $DDTlastPg      s               n   inz
006090110427     d $Partic         s               n   inz
006100110427     d $NoDesPartic    s               n   inz
006110110427     d $BolloVirt      s               n   inz
006120110427     d $No_CF          s               n   inz
006130110427     d $No_PI          s               n   inz
006140120113     d $No_x_Cons      s               n   inz
006150110427     d $Rit_a_Inc      s               n   inz
006160110427     d $A4_x_ScNS      s               n   inz
006170110427     d $LascAvv        s               n   inz
006180110427     d $ConsParz       s               n   inz
006190110427     d $C_A_           s               n   inz
006200110427     d $CAeur          s               n   inz
006210110427     d $GeoAtt         s               n   inz
006220090302     d $TabPXA         s               n   inz
006230110411     d $TabPXA2        s               n   inz
006240090924     d $BoxNxD         s               n   inz
006250100513     d $PreavvORM      s               n   inz
006260130215       // -?Altri flags?
006270130215     d $First          s              1    inz(*on)
006280110427
006290110427       // -?Indici di schiera?
006300110427     d XX              s              3  0 inz
006310110427     d YY              s              3  0 inz
006320110427     d WX              s              3  0 inz
006330110427
006340110427       // -?Campi di comodo alfanumerici?
006350040223     d SAVin7x         s              4    inz(*zeros)
006360030717     d w_CoOr          s             35    inz
006370030515     d wAWB            s             12    inz
006380030515     d w001a           s              1    inz
006390030515     d w008a           s              8    inz
006400030515     d w012a           s             12    inz
006410030515     d w015a           s             15    inz
006420100513     d w053a           s             53    inz
006430070424     d wDateA          s              8    inz
006440070717     d wGeoTel         s              1    inz
006450070717     d wGeoCon         s              1    inz
006460110427       // -?Campi di comodo numerici?
006470030515     d w0040           s              4  0 inz
006480030515     d w0080           s              8  0 inz
006490030624     d w0112           s             11  2 inz
006500030515     d w0140           s             14  0 inz
006510071105     d wDate           s              8  0 inz
006520030515     d wCOPIE          s              3  0 inz
006530131205     d wEVBdev         s                   like(EVBdev)   inz
006540131205     d wEVBhev         s                   like(EVBhev)   inz
006550110427
006560110427       // -?Campi utilizzati nella gestione dell'impaginazione A4?
006570030515     d wMinLine        s              3  0 inz
006580030515     d wMaxLine        s              3  0 inz
006590080604     d wPage           s              4  0 inz(0001)
006600030515     d WelSNS          s              2  0 inz
006610040608     d wOmaRowT        s              3  0 inz
006620040608     d wOmaRowD        s              3  0 inz
006630110427       // -?Variabili per il calcolo delle posizioni per box in A4?
006640080923     d wNumLdV         s              5  0 inz
006650080923     d wSeqLdV         s              1  0 inz
006660080923     d wPosIni         s              5  3 inz
006670110427
006680110427       // -?Lunghezza (in inches) di una singola LdV?
006690080703     d $Len1LdV        s              5  3 inz(5.775)
006700110427       // -?Numero delle LdV (box) per pagina "A4"?
006710110427       //  ?(dovendo stampare sia la copia per il destinatario quanto?
006720110427       //   ?quella per il vettore, ci stanno 2 LdV in 1 A4;?
006730110427       //   ?stampando solo le copie per il vettore ce ne stanno 4!)?
006740080703     d $LdVxP          s              1  0 inz(2)
006750110427
006760110427       // -?Campi utilizzati per impostazione dell'output in stampa?
006770140922     d Pdsp            s              8  0 inz
006780130418     d Tla             s             50    inz
006790130418      *
006800050624     d wBolloVirt      s              4  2 inz
006810030515     d Wcas            s             13  3 inz
006820030515     d Wiva            s             11  2 inz
006830071015     d Wpdc4A          s              4    inz
006840030515     d Data_EUR        s               d   datfmt(*eur)   inz(*loval)
006850101201     d Data_DMY        s               d   datfmt(*dmy)   inz(*loval)
006860110427       // -?Campi utilizzati per il salvataggio momentaneo di dati?
006870040608     d SavLineA4       s                   like(CurLineA4)
006880040311     d SAVlnp          s                   like(DB0lnp)   inz(*loval)
006890040311     d SAVlna          s                   like(DB0lna)   inz(*loval)
006900030515     d SAVtrd          s                   like(AR5trd)   inz(*hival)
006910030515     d SAVdac          s                   like(AR5dac)   inz(*hival)
006920030515     d SAVorc          s                   like(AR5orc)   inz(*hival)
006930040607     d SAVprr          s                   like(AR8prr)   inz
006940041207     d SAVtrk          s                   like(AR8trk)   inz
006950070314     d wRSxCF          s                   like(DB0rsm)   inz
006960071114     d wB0cfi          s                   like(DB0cpi)   inz
006970071114     d wB0piv          s                   like(DB0cpi)   inz
006980031015     d wB0nrs          s                   like(ARBnrs)   inz
006990070514     d wB0ndc          s                   like(ARBndc)   inz
007000070514     d wB0ntc          s                   like(ARBntc)   inz
007010070514     d wB0dcp          s                   like(ARBdcp)   inz
007020081119     d wB0tmc          s                   like(ARBtmc)   inz
007030101125     d wB0dcm          s                   like(ARBdcm)   inz
007040101125     d wB0icc          s                   like(ARBicc)   inz
007050101125     d wB0ica          s                   like(ARBica)   inz
007060130124     d wB0fiv          s                   like(AR6fiv)   inz
007070060505     d Wnsc            s                   like(ARSnsc)   inz
007080060505     d Wtrc            s                   like(ARStrc)   inz
007090030515     d Wctr            s                   like(DB0ctr)   inz
007100031124     d Wdsp            s                   like(DB0dft)   inz
007110090527     d WddtCLN         s                   like(DB0drt)   inz
007120090527     d WdspCLN         s                   like(DB0dft)   inz
007130030515     d Wsvx            s                   like(AR6sv1)   inz
007140030515     d Wvax            s                   like(AR6va1)   inz
007150150226     d*// Wtba            s                   like(§AR5tba)  inz
007160080708
007170110427       // -?Comando da eseguire per QCMDEXC?
007180080708     d Qcmd            s            512    inz
007190080708
007200090302       //--------------------------------------------------------------
007210130306       //?Definizione prototipi procedure usate.                       ?
007220090302       //--------------------------------------------------------------
007230140915
007240140915       // -?Calcolo/Controllo BRTcode?
007250140915      /copy gaitrasrc/srcProtoPI,TRUL28R0
007260140915      /copy gaitrasrc/srcProtoPR,TRUL28R0
007270080708
007280100208       // -?Calcolo tempi di delivery?
007290090605      /copy gaitrasrc/srcProtoPR,TNSD99R
007300100208
007310100208       // -?Determinazione intestatario contrassegno?
007320100208      /copy gaitrasrc/srcProtoPR,TRULINTR
007330130306
007340130306       // -?Reperimento/Aggiornamento numeratori in GRU?
007350130306     d TRUL33ds      e ds                  inz
007360130306      /copy gaitrasrc/srcProtoPR,TRUL33R
007370090605
007380100208       // -?Override al PrtF?
007390080709     d fnlsB8r         pr                  extpgm('FNLSB8R')
007400080709     d  fnlsB5ds                           likeds(FNLSB5ds)
007410130306
007420130306       // -?Reperimento override apportate al PrtF?
007430130306     d wPrtF           s             10
007440130307     d trulPOVRds    e ds                  qualified
007450130306     d trulPOVRc       pr                  extpgm('TRULPOVRC')
007460130306     d   wPrtF                       10
007470130306     d   trulPOVRds                        likeds(trulPOVRds)
007480130306
007490130306       // -?Stampa tutte le immagini PDF abbinate alle LdV già stampate?
007500130312      /copy gaitrasrc/srcProtoPI,PDF00STAR
007510130312      /copy gaitrasrc/srcProtoPR,PDF00STAR
007520080708
007530100208       // -?Esecuzione comando di sistema?
007540080708     d qCmdExc         pr                  extpgm('QCMDEXC')
007550080708     d  Qcmd                        512    const  options(*varsize)
007560080708     d  Qlen                         15  5 const
007570090302
007580090302       //--------------------------------------------------------------
007590090302       //?Definizione key-list.                                        ?
007600090302       //--------------------------------------------------------------
007610090302
007620110427       // -?File TNTBE01L?
007630090302     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
007640090302     d                                     prefix(k_)   inz
007650130306
007660130306       // -?File FIAR801L?
007670130306     d k06fiar801    e ds                  extname(FIAR801L : *key)
007680130306     d                                     prefix(k_)   inz
007690140303
007700140303       // -?File FNEVB22L?
007710140303     d keyFNEVB22    e ds                  extname(FNEVB22L : *key)
007720140303     d                                     prefix(k_)   inz
007730140410
007740140410       // -?File FNORE01L?
007750140410     d keyFNORE01    e ds                  extname(FNORE01L : *key)
007760140410     d                                     prefix(k_)
007770140410     d                                     inz
007780030515
007790030619      *===============================================================*
007800050217      *?RIEPILOGO INDICATORI                                         ?*
007810030515      *---------------------------------------------------------------*
007820050216      *?18   ?- Stampa DATA e ORA CONSEGNA RICHIESTA                  *
007830050216      *?19   ?- Stampa IMPONIBILE, IVA, TOTALE ed ESTREMI FATTURA     *
007840050216      *?20   ?- Stampa CONTRASSEGNO                                   *
007850050216      *?21   ?- Stampa TOTALE DA ESIGERE ALLA CONSEGNA / AL RITIRO    *
007860051129      *?22   ?- Stampa NOTE (se NO imponibile e fattura)              *
007870050216      *?31   ?- di comodo                                             *
007880130418      *?33off?- Stampa della COPIA PER IL CORRIERE                    *
007890130418      *?33on ?- Stampa della COPIA PER IL DESTINATARIO                *
007900130418      *         (o per il Mittente, se P.F. Prepagato)                *
007910050216      *?35   ?- Richiesta FIRMA, DATA e ORA   in 1ª copia             *
007920050217      *         per ricevuta della merce senza riserve                *
007930050217      *         e indica "TOTALE DA ESIGERE ALLA CONSEGNA"            *
007940050216      *?36   ?- Richiesta FIRMA per QUIETANZA in 2ª copia             *
007950050217      *         e indica "TOTALE DA ESIGERE AL RITIRO"                *
007960050216      *?37   ?- Condiziona HIGHLIGHT nel campo PDS2  in rec. PRTLDV1  *
007970050217      *                                  per servizio "ESPRESSO"      *
007980050217      *                                o per servizio "POSTE IT."     *
007990050216      *?38   ?- Condiziona FONT      nel campo PAR5  in rec. PRTAR52  *
008000050216      *?39   ?- Condiziona UNDERLINE nel campo PRIG  in rec. PRTGEN   *
008010050217      *                            e nel campo PCTXX in rec. PRTDK5   *
008020050217      *                            e nel campo PCTXT in rec. PRTDTx   *
008030050217      *                            e nel campo PCTXT in rec. PRTDDx   *
008040080604      *?41   ?- Stampa copia per il corriere della 1ª LdV             *
008050080604      *?42   ?- Stampa copia per il destinatario della 1ª LdV         *
008060080604      *?43   ?- Stampa copia per il corriere della 2ª LdV             *
008070080604      *?44   ?- Stampa copia per il destinatario della 2ª LdV         *
008080080918      *?45off?- Posiziona l'inizio della packing-list al 2/4 dell'A4  *
008090080923      *         (packing-list di 2 LdV per A4)                        *
008100080918      *?45on ?- Posiziona l'inizio della packing-list in cima all'A4  *
008110080923      *         (1ª packing-list in A4 dopo la stampa di 4 LdV x A4)  *
008120080923      *?46on ?- Posiziona l'inizio della packing-list a metà dell'A4  *
008130080923      *         (2ª packing-list in A4 dopo la stampa di 4 LdV x A4)  *
008140120530      *?50   ?- Stampa CODICE A BARRE - BRT                           *
008150050217      *?51   ?- Stampa CODICE A BARRE - DPD                           *
008160050217      *?52   ?- Stampa CODICE A BARRE - O.R.M.                        *
008170120530      *?55   ?- NON stampa logo BRT in DDT della Logistica =>         *
008180120530      *         => Stampa il simil-logo in LSB6LOGBRT                 *
008190050217      *?69off?- Stampa su modulo in formato "A4"                      *
008200050217      *?69on ?- Stampa su modulo in formato "A5"                      *
008210120530      *?70   ?- Trattasi di BOLLA BRT                                 *
008220050216      *?71   ?- Trattasi di BOLLA DPD                                 *
008230050216      *?72   ?- Trattasi di BOLLA FedEx                               *
008240080912      *?73   ?- Trattasi di BOLLA EuroExpress                         *
008250030619      *===============================================================*
008260030619      * Descrizione dei campi utilizzati per il posizionamento di     *
008270030619      *   linee orizzontali, linee verticali o campi di stampa        *
008280030619      * (utilizzati solo nel file di stampa per il formato "A4")      *
008290030619      * servono ad impostare la distanza di tali oggetti dal lato     *
008300030619      * superiore del foglio.                                         *
008310030619      *---------------------------------------------------------------*
008320080604      *? BOX nella copia per il vettore:                             ?*
008330080604      *§BOX1AS = posiz. linea orizzontale inizio box mittente (1)   ?*
008340080604      *§BOX1AI = posiz. linea orizzontale fine   box mittente (1)   ?*
008350080604      *§BOX1BS = posiz. linea orizzontale inizio box generale (2)   ?*
008360080604      *§BOX1BI = posiz. linea orizzontale fine   box generale (2)   ?*
008370140919      *§BOX1BJ = posiz. linea orizzontale fine   box BRTcode        ?*
008380080604      *§BOX1CS = posiz. linea orizzontale inizio box contrassegno   ?*
008390080604      *§BOX1CI = posiz. linea orizzontale fine   box contrassegno   ?*
008400080604      *§BOX1DS = posiz. linea orizzontale inizio box tot.da esigere ?*
008410080604      *§BOX1DI = posiz. linea orizzontale fine   box tot.da esigere ?*
008420080604      *§LO1S1  = posiz. linea orizzontale separatrice               ?*
008430080604      *           Destinatario / Colli, Peso, Volume...               *
008440080604      *§LO1S2  = posiz. linea orizzontale separatrice               ?*
008450080922      *           Colli, Peso, Volume / Zona                          *
008460080922      *§LO1S3  = posiz. linea orizzontale separatrice               ?*
008470080922      *           Zona / Segnacolli                                   *
008480080922      *§LO1S4  = posiz. linea orizzontale separatrice               ?*
008490080922      *           Segnacolli / Turno di chiusura                      *
008500080922      *§LO1S5  = posiz. linea orizzontale separatrice               ?*
008510080922      *           Turno di chiusura / "PER RICEVUTA DELLA MERCE..."   *
008520080604      *§LO1C1  = posiz. linea orizzontale separatrice               ?*
008530080624      *           Servizio / Consegne Particolari                     *
008540080604      *§LO1D1  = posiz. linea orizzontale separatrice               ?*
008550080604      *           Riferimenti Mittente / Note e Particolarità         *
008560080605      *§LLV1S1 = lunghezza linea verticale separatrice              ?*
008570080605      *           Destinatario / Riferimento mittente                 *
008580080605      *§LLV1S2 = lunghezza linea verticale separatrice              ?*
008590080605      *           Riferimento mittente / Spedizione                   *
008600120530      *§PC0 <=> §PC6 =  posizioni costanti intestazione "BRT"       ?*
008610120530      *           "BRT SpA", "Sede Operativa ed Amministrativa"       *
008620140917      *§PK1    = posiz. costante "BRTcode" + codice                 ?*
008630080604      *§PK2    = posiz. costante "TASSATIVA"                        ?*
008640080605      *§PK3    = posiz. costanti "(nome e cognome in stampatello)"  ?*
008650080605      *           (in "PER RICEVUTA DELLA MERCE ...")                 *
008660121017      *§PK4    = posiz. costante "COPIA PER IL DESTINATARIO"        ?*
008670121017      *§PK5    = posiz. riferimento alla legge in vigore x RCV      ?*
008680121017      *           (tra n° Spedizione e Imponibile...)                 *
008690121017      *§PK6    = posiz. costante "Bollo assolto in modo virtuale..."?*
008700121017      *§PK7    = posiz. Partita IVA o Cod. Fiscale del cliente      ?*
008710080604      *                                                               *
008720080604      *? Altre posizioni:                                            ?*
008730050325      *§PB1 = posizione linea orizzontale inizio box                ?*
008740050325      *§PB2 = posizione linea orizzontale fine   box                ?*
008750050216      *§LO1 = posizione x 1ª linea orizzontale                      ?*
008760080604      *        LSB6FED1 - richiesta nome e firma per FedEx            *
008770080604      *        LSB6FED3 - limite superiore del box per FedEx          *
008780080604      *        LSB6AR53 - limite superiore dei box da  FIAR5          *
008790030619      *                   (bancali da rendere, supermercato, promem.) *
008800080604      *        LSB6DK0  - linea separatrice nel 1° box DDT Karnak     *
008810080604      *        LSB6DL0A - linea separatrice nel box DDT Logistica     *
008820080604      *        LSB6DL0C - linea separatrice nel box DDT Logistica     *
008830050325      *                   (area Packing List)                         *
008840080604      *        LSB6DL0E - linea separatrice nel box DDT Logistica     *
008850050606      *                   (area Fattura)                              *
008860080604      *        LSB6AC0A - linea separatrice nel box DDT Arcte         *
008870080604      *        LSB6ES0A - linea separatrice nel box DDT Esprinet      *
008880050216      *§LO2 = posizione x 2ª linea orizzontale                      ?*
008890080604      *        LSB6FED1 - linea orizzontale separatrice per FedEx     *
008900080604      *        LSB6FED3 - limite inferiore del box per FedEx          *
008910080604      *        LSB6AR53 - limite inferiore dei box da  FIAR5          *
008920030619      *                   (bancali da rendere, supermercato, promem.) *
008930080604      *        LSB6DK0  - linea separatrice nel 2° box DDT Karnak     *
008940080604      *        LSB6DL0A - linea separatrice nel box DDT Logistica     *
008950080604      *        LSB6DL0C - linea separatrice nel box DDT Logistica     *
008960050325      *                   (area Packing List)                         *
008970080604      *        LSB6DL0E - linea separatrice nel box DDT Logistica     *
008980050610      *                   (area Fattura)                              *
008990080604      *        LSB6AC0A - linea separatrice nel box DDT Arcte         *
009000080604      *        LSB6ES0A - linea separatrice nel box DDT Esprinet      *
009010050216      *§LO3 <=> §LO9/A = posizioni x linee orizzontali              ?*
009020080604      *        LSB6DK0  - linee separatrici nel 2° box DDT Karnak     *
009030080604      *        LSB6DL0B - linee separatrici (orizzontali e verticali) *
009040050218      *                   nella parte finale del box DDT Logistica    *
009050080604      *        LSB6DL0F - linea separatrice nel box DDT Logistica     *
009060050610      *                   (area Fattura nella parte finale del box)   *
009070080604      *        LSB6AC0B - linee separatrici (orizzontali e verticali) *
009080050218      *                   nella parte finale del box DDT Arcte        *
009090080604      *        LSB6ES0B - linee separatrici (orizzontali e verticali) *
009100050216      *                   nella parte finale del box DDT Esprinet     *
009110050216      *§LV1 = posizione x 1ª linea verticale                        ?*
009120080604      *        LSB6FED1 - lineee verticali "lunghe" (0.15 inch) su    *
009130030619      *                   quella orizzontale per FedEx                *
009140080604      *        LSB6FED3 - lineee verticali "lunghe" (0.25 inch) nel   *
009150030619      *                   box per FedEx (separatori)                  *
009160050216      * "   = lunghezza delle linee verticali                       ?*
009170080604      *        LSB6DK0  - linee separatrici nella 1ª parte alta del   *
009180040607      *                   2° box per DDT Karnak                       *
009190080604      *        LSB6DL0A - linee separatrici nella 1ª parte alta del   *
009200050218      *                   box per DDT Logistica                       *
009210080604      *        LSB6DL0C - linee separatrici nella parte alta del box  *
009220050325      *                   DDT Logistica, area Packing List            *
009230080604      *        LSB6DL0E - linea separatrice nel box DDT Logistica     *
009240050610      *                   (area Fattura)                              *
009250080604      *        LSB6AC0A - linee separatrici nella 1ª parte alta del   *
009260050218      *                   box per DDT Arcte                           *
009270080604      *        LSB6AR54 - linea sx "simil" box AR54                   *
009280071025      *                   (v. "Ritiro contestuale alla consegna" o    *
009290071025      *                       "Ritiro porto franco prepagato")        *
009300050216      *§LV2 = posizione x 2ª linea verticale                        ?*
009310080604      *        LSB6FED1 - lineee verticali "corte" (0.07 inch) su     *
009320030619      *                   quella orizzontale per FedEx                *
009330080604      *        LSB6FED3 - lineee verticali "corte" (0.10 inch) nel    *
009340030619      *                   box per FedEx (separatori)                  *
009350050216      * "   = lunghezza delle linee verticali                       ?*
009360080604      *        LSB6DK0  - linee separatrici nella 2ª parte alta del   *
009370040607      *                   2° box per DDT Karnak                       *
009380080604      *        LSB6DL0A - linee separatrici nella 2ª parte alta del   *
009390050218      *                   box per DDT Logistica                       *
009400080604      *        LSB6DL0C - linee separatrici nella parte alta del box  *
009410050325      *                   DDT Logistica, area Packing List            *
009420080604      *        LSB6DL0F - linea separatrice nel box DDT Logistica     *
009430050610      *                   (area Fattura - corpo fattura con totali)   *
009440080604      *        LSB6AC0A - linee separatrici nella 2ª parte alta del   *
009450050218      *                   box per DDT Arcte                           *
009460080604      *        LSB6ES0A - linea separatrice nel 1° box DDT Esprinet   *
009470050216      *§LV3 <=> §LV7   = posizioni x linee verticali                ?*
009480080604      *        LSB6DK0  - linee separatrici nel 2° box DDT Karnak     *
009490080604      *        LSB6DL0A - linee separatrici nel box DDT Logistica     *
009500080604      *        LSB6DL0C - linee separatrici nel box DDT Logistica     *
009510050325      *                   (area Packing List)                         *
009520080604      *        LSB6AC0A - linee separatrici nel box DDT Arcte         *
009530050216      *§PS1 = posizione per campi                                   ?*
009540080604      *        LSB6FED3 - posizione dei campi nel box per FedEx:      *
009550030619      *                   · Package Tracking Number                   *
009560030619      *                   · Status                                    *
009570080604      *        LSB6AR51 - posizione dell'unico campo nel box          *
009580080604      *        LSB6AR52 - posizione dell'unico campo nel box          *
009590080604      *        LSB6AR5A - posizione dell'unico campo nel box          *
009600080604      *        LSB6AR5B - posizione dell'unico campo nel box          *
009610080604      *        LSB6AR5C - posizione dell'unico campo nel box          *
009620080604      *        LSB6AR5G - posizione dell'unico campo nel box          *
009630080604      *        LSB6TAG  - posizione dell'unico campo nel box          *
009640080604      *        LSB6DK0  - posizione della 1ª riga di testo            *
009650081003      *§PS2 <=> §PS8  -> posizioni dei campi                        ?*
009660080604      *        LSB6DK0  - posizioni delle righe di testo              *
009670080604      *        LSB6DL0B - posizioni delle righe di testo              *
009680080604      *        LSB6AC0B - posizioni delle righe di testo              *
009690080604      *        LSB6ES0B - posizioni delle righe di testo              *
009700081003      *        LSB6DL9  - posizioni delle righe di testo              *
009710050216      *§PR1 <=> §PR2  -> posizioni dei campi (rovesciati di 270°)   ?*
009720080604      *        LSB6DK0  - posizioni delle righe di testo              *
009730080604      *        LSB6DL0A - posizioni delle righe di testo              *
009740080604      *        LSB6DL0C - posizioni delle righe di testo              *
009750080604      *        LSB6AC0A - posizioni delle righe di testo              *
009760080604      *        LSB6ES0A - posizioni delle righe di testo              *
009770030619      *===============================================================*
009780030515
009790030515     c     *Entry        plist
009800080703     c                   parm                    FNLSB5ds
009810130306     c                   parm                    FNLSB6ds1
009820110427
009830110427       // -?Tipo lancio (DB0tla):?"C" -           Chiudo con LR?
009840110427       //                        ?"L" - Elaboro e chiudo con LR?
009850110427       //                        ?" " - Elaboro e chiudo in RETRN?
009860080703if  1c                   if        DB0tla = 'C'
009870030515     c                   goto      Fine
009880060921e   1c                   endif
009890110427
009900110427       // -?Operazioni iniziali?
009910070123     c                   exsr      sr_Start
009920110427
009930030515sel 1c                   SELECT
009940130306
009950130306       // -?Richiesta la stampa (finale) delle sole immagini PDF?
009960130306     c                   WHEN      %parms() > 1  and  DB6pdf = 'S'
009970130307     c*//                                        and  DB0fa4 = 'I'
009980130306     c                   if        DB6num > *zero
009990130306     c                   exsr      sr_PrtPdf
010000130306     c                   endif
010010130306     c                   exsr      sr_End
010020110427
010030110427       // -?NON passati i dati LDV => caricamento?
010040030515w   1c                   WHEN      DB0ges = *blanks
010050071008if  2c                   if             DB0tbo  = 'P'
010060071008     c                             or   DB0tbo  = '8'
010070030515     c                   exsr      CarBLP
010080030515x   2c                   else
010090030515     c                   exsr      CarARB
010100030515e   2c                   endif
010110110427
010120110427       // -?Passati i dati LDV => è già (quasi) tutto pronto?
010130110427       //  ?(solo dalla sede - vedi TNSD50R)?
010140081114w   1c                   WHEN      DB0ges = 'B'
010150090513     c                   exsr      CarTAB
010160081114     c*** (già fatto:)   clear                   dsBL4A
010170081114     c                   if        DB0fbr = 'S'
010180081114     c                   exsr      RepAR4_A
010190081114     c***                if        §B4Abm = 'S' or §B4Abm = 'C' or
010200081114     c***                          §B4Abm = 'P' or §B4Abm = 'Q'
010210081114     c***                movel     'S'           DB0bam
010220081114     c***                endif
010230081114     c                   endif
010240081114     c                   eval      wB0cfi = DB0cpi
010250081114     c                   eval      wB0piv = DB0cpi
010260110427
010270110427       // -?DB0GES = "R" => Stampa una LDV vuota con un messaggio?
010280110427       //  ?per avvisare che è impossibile continuare la ristampa?
010290110427       //  ?perchè stampate più di 6.000 bolle !?
010300030515w   1c                   WHEN      DB0ges = 'R'
010310071112     c                   exsr      Rep_TabVPO
010320071112     c                   eval      Pmsg1  = $Msg(1)
010330071112     c                   eval      Pmsg2  = %replace( %editc(
010340071112     c                                         §VPOmaxldv : 'J' ) :
010350071112     c                                         $Msg(2) : %scan(
010360071112     c                                         '&NrLdV' : $Msg(2) ))
010370130418     c                   eval      *in33 = *off
010380130418     c                   exsr      Posiz_Box
010390030515     c                   write     A4prtMSG
010400030515     c                   goto      Fine
010410110427
010420030515e   1c                   ENDSL
010430110427
010440110427       // -?Controllo del tipo LDV da stampare?
010450040607     c                   exsr      TipoLDV
010460080919      /free
010470080919
010480110427       //?»»»?Verifiche per chiusura pgm?«««?
010490080919       SELECT;
010500080919
010510110427         // -?Chiude SE richiamato per la sola verifica del modulo?
010520110427         //  ?(dal pgm FNLSE6R1 - cl FNLSE6C1)?
010530080919         WHEN  DB0ris  = 'R'   and
010540080919               DB0fl9  = *on   and
010550080919               DUTlpo  = *blanks;
010560080919           exsr  sr_End;
010570080919
010580110427         // -?Chiude SE richiesta la stampa in formato NON corrispondente?
010590080926
010600110427         // ·?Richiesta stampa di A4, mentre risulta di ex-A5?
010610110427         //  ?NON GESTITO:?rimane il richiamo "standard" dei pgm. di?
010620110427         //  ?manutenzione => 1 A4 x ogni LdV (anche se ex-A5)?
010630080919         //WHEN  DB0fa4  = '1'   and
010640080919         //      *in69   = *on;
010650080919         //  DB0tms = 'E';
010660080919         //  exsr  sr_End;
010670080919
010680110427         // -?Richiesta stampa di ex-A5, mentre risulta di A4?
010690080919         WHEN  DB0fa4  = '2'   and
010700080919               *in69   = *off;
010710080919           DB0tms = 'D';
010720080919           exsr  sr_End;
010730080919
010740080919       ENDSL;
010750080919
010760080919      /end-free
010770110427
010780110427       // -?Imposta i campi di stampa con quelli della DS?
010790030515     c                   exsr      PrepPRT
010800110427
010810110427       // -?Operazioni "inutili"?SE?richiesta la stampa della sola?
010820110427       //  ?packing-list:?
010830080919     c                   IF        DB0fa4 <> 'P'
010840110427
010850110427       // -?Decodifica i dati della LDV?
010860040607     c                   exsr      DecodLDV
010870110427
010880080919     c                   ENDIF
010890110427
010900110427       // -?Stampa della Lettera di Vettura?
010910030515sel 1c                   SELECT
010920110427       // -?Se RIMESSO MITTENTE stampo il numero di copie richiesto?
010930110427       //  ?(non per DPD e FedEx)?
010940110427       //  ?(le stampo qui perché devo stampare il riferimento numerico?
010950110427       //  ?aggiornato di volta in volta su FLNUF)?
010960030515w   1c                   WHEN      DB0ris = 'M'  and  *in70
010970030515     c                   clear                   wCOPIE
010980030515     c                   clear                   NUFaaa
010990030515     c                   eval      NUFcnu = 20
011000030515     c                   eval      NUFfil = DB0lnp
011010030515     c                   clear                   NUFfl2
011020030515do  2c                   DO        Wctr
011030030515     c     K04NUF        chain     FLNUF01L
011040030515if  3c                   if        %found(FLNUF01L)
011050030515     c                   add       1             NUFnum
011060030515     c                   UPDATE    FLNUF
011070030515     c                   eval      Prmn = NUFnum
011080040607     c                   exsr      StampaLDV
011090030515e   3c                   endif
011100030515e   2c                   ENDDO
011110110427       // -?Se FATTURA PREPAGATA stampo due copie?
011120110427       //  ?(solo se non attivata procedura O.R.M.)?
011130030515w   1c                   WHEN      DB0tbo = '8'  and  §OGorm <> 'S'
011140030515     c                   eval      wCOPIE = 2
011150110427       // -?Altrimenti stampo solo una copia?
011160030515x   1c                   OTHER
011170030515     c                   eval      wCOPIE = 1
011180030515e   1c                   ENDSL
011190110427
011200030515do  1c                   do        wCOPIE
011210080918      /free
011220080918         if  DB0fa4 = 'P';
011230130315           exsr  StampaPkgLst;
011240080918         else;
011250110427           exsr  StampaLDV;
011260080918         endif;
011270080918      /end-free
011280030515e   1c                   enddo
011290130306
011300130306       // -?Eventuale stampa immagini PDF?
011310130306       //  ?(si spera della sola spedizione di cui si è appena?
011320130306       //   ?stampata la LdV)?
011330130306     c                   if        %parms() > 1  and  DB6pdf = 'X'
011340130306     c                                           and  DB6num > *zero
011350130306     c                   exsr      sr_PrtPDF
011360130306     c                   endif
011370110427
011380110427       // -?Operazioni finali?
011390110427     c     Fine          tag
011400080624     c                   exsr      sr_End
011410110427
011420030515      *---------------------------------------------------------------*
011430050217      *?OPERAZIONI INIZIALI                                          ?*
011440030515      *---------------------------------------------------------------*
011450030515     c     *InzSR        BEGSR
011460131023      *
011470131023      * Reperimento dati utente
011480131023      * Reperisco nome azienda per stampa PREPAGATI (e non solo)
011490131023     c     *dtaara       define    §azute        AZUTEds
011500131023     c     *dtaara       define    §datiute      dDATIUTE
011510131023     c                   in(E)     *dtaara
011520131023     c                   if        %ERROR or RSUT = *blanks
011530131023     c                   clear                   Tibs34Ds
011540131023     c                   call      'TIBS34R'
011550131023     c                   parm                    Tibs34Ds
011560131023     c                   in        *dtaara
011570131023     c                   endif
011580131023      * Impostazione dati in KPJBA
011590131023     c                   eval      KNMUS  = SDSusr
011600110429      *
011610131023      *USCITA? SE  richiamato per la sola chiusura
011620130314     c*//                if        %subst(KPJBU:1:1) = 'C'
011630130314     c                   if        DB0tla = 'C'
011640110429     c                   leavesr
011650110429     c                   endif
011660070424      *
011670070424      * Reperisco la data del giorno
011680070424     c                   time                    w0140
011690070424     c                   move      w0140         w0080
011700070424     c     *eur          movel     w0080         Data_EUR
011710070424     c     *iso          movel     Data_EUR      w0080
011720070424     c                   move      w0080         wDateA
011730071105     c                   eval      wDate  = w0080
011740030515      *
011750030515      * Imposto i campi chiave "fissi"
011760040223     c                   eval      TBLkut = 1
011770060505     c                   eval      ARStrc = 'C'
011780070319     c                   eval      ARNcdn = 'LDV'
011790070319     c                   eval      ARNgst = 'S'
011800030515      *
011810030515      * Carico le varie esenti IVA
011820030515     c                   movel     'CC'          TBLcod
011830030515     c                   movel(p)  'VARIE'       TBLkey
011840030515     c                   clear                   XX
011850030515     c     K03TAB        setll     TABEL
011860030515     c     K02TAB        reade     TABEL
011870030515      *
011880070306do  1c                   DOW       NOT %eof(TABEL00F)
011890030515      * Deve essere NON annullata e con codice che inizia con "VARIE"
011900030515if  2c                   If        %subst(TBLkey:1:5) = 'VARIE'
011910030515     c                             and    TBLflg      = *blanks
011920030515     c                   movel     TBLuni        dsCC
011930030515if  3c                   if        §CCcei <> *blanks
011940030515     c                   add       1             xx
011950030515     c                   move      TBLkey        $Var(xx)
011960030515e   3c                   endif
011970030515e   2c                   EndIf
011980030515      *
011990030515     c     K02TAB        reade     TABEL
012000030515e   1c                   ENDDO
012010070306      *
012020070308      * Carico le causali eventi con tipo consegna = "G"
012030070306     c                   movel     '2A'          TBLcod
012040070306     c                   clear                   TBLkey
012050070306     c                   clear                   XX
012060070306     c     K03TAB        setll     TABEL
012070070306     c     K02TAB        reade     TABEL
012080070306      *
012090070306do  1c                   DOW       NOT %eof(TABEL00F)
012100070308      * Deve essere NON annullata e con flag §2AFTC = "G"
012110070308if  2c                   If        TBLflg = *blanks
012120070306     c                   movel     TBLuni        ds2A
012130070308if  3c                   if        §2Aftc = 'G'
012140070306     c                   add       1             xx
012150070308     c                   movel     TBLkey        $2Acev_G(xx)
012160070306e   3c                   endif
012170070306e   2c                   EndIf
012180070306      *
012190070306     c     K02TAB        reade     TABEL
012200070306e   1c                   ENDDO
012210030515      *
012220030515      * Carico tabella VARIABILI ARRIVI
012230030515     c                   clear                   ds$2
012240030515     c                   eval      TBLcod = '$2'
012250030515     c                   eval      TBLkey = '1'
012260030515     c     K03TAB        chain     TABEL
012270030515if  1c                   if        %found(TABEL00F)
012280030515     c                   movel     TBLuni        ds$2
012290030515e   1c                   endif
012300030515      *
012310030515      * Reperisco la moneta di conto
012320030515     c                   clear                   dGED
012330040607     c                   clear                   TIBS02ds
012340040607     c                   eval      T02mod = 'C'
012350040607     c                   eval      T02cod = 'GED'
012360040607     c                   eval      T02ke1 = '1         '
012370030515     c                   call      'TIBS02R'
012380030515     c                   parm                    KPJBA
012390040607     c                   parm                    TIBS02ds
012400030515if  1c                   if        T02err = *blanks
012410030515     c                   movel     T02uni        dGED
012420030515e   1c                   endif
012430030515      *
012440030515      * Con moneta di conto:
012450060301      *   Carico tabella GESTIONALE - IMPORTI VARI   e
012460030515      *   Reperisco l'importo limite per abilitazione c/a
012470030515     c                   clear                   dGEI
012480040607     c                   clear                   TIBS02ds
012490040607     c                   eval      T02mod = 'C'
012500040607     c                   eval      T02cod = 'GEI'
012510040607     c                   eval      T02ke1 = §GEDcn
012520030515     c                   call      'TIBS02R'
012530030515     c                   parm                    KPJBA
012540040607     c                   parm                    TIBS02ds
012550030515if  1c                   if        T02err = *blanks
012560030515     c                   movel     T02uni        dGEI
012570030515e   1c                   endif
012580061031      *
012590110426      * Reperisco data di decorrenza stampa "Per ricevuta di ...
012600071005      *                                      senza riserve"
012610090615      *              e di decorrenza stampa per C.L.N.
012620061031     c                   clear                   dVPOdeco
012630061031     c                   clear                   TIBS02ds
012640061031     c                   eval      T02mod = 'C'
012650071112     c***                eval      T02sif  = KNSIF
012660071112     c                   eval      T02cod = 'VPO'
012670061031     c                   eval      T02ke1 = 'DECO      '
012680061031     c                   call      'TIBS02R'
012690061031     c                   parm                    KPJBA
012700061031     c                   parm                    TIBS02ds
012710061031if  1c                   if        T02err = *blanks
012720061031     c                   movel     T02uni        dVPOdeco
012730061031e   1c                   endif
012740120530      *
012750120530      *//*?Reperisco data di decorrenza stampa nuovo logo BRT?
012760120530     c*//                clear                   dVPObrt
012770120530     c*//                clear                   TIBS02ds
012780120530     c*//                eval      T02mod = 'C'
012790120530     c***                eval      T02sif  = KNSIF
012800120530     c*//                eval      T02cod = 'VPO'
012810120530     c*//                eval      T02ke1 = 'BRT       '
012820120530     c*//                call      'TIBS02R'
012830120530     c*//                parm                    KPJBA
012840120530     c*//                parm                    TIBS02ds
012850120530if  1c*//                if        T02err = *blanks
012860120530     c*//                movel     T02uni        dVPObrt
012870120530e   1c*//                endif
012880080708      /free
012890110426         // -?Override al file di stampa?
012900081017         if  DB0tla <> 'C';
012910080709           fnlsB8r ( fnlsB5ds );
012920080708         endif;
012930080708      /end-free
012940030515      *
012950030515      * Apro i file di stampa
012960130710     c                   open      FNLSB6P
012970080918     c                   clear                   CurLineA4
012980130306      /free
012990130306         // -?Reperimento override apportate al file di stampa?
013000130306         //  ?per comunicarle per la stampa immagini PDF?
013010130306         if  DB0tla <> 'C';
013020130306           wPrtF = 'FNLSB6P';
013030130306           clear  trulPOVRds;
013040130307           trulPOVRc ( wPrtF : trulPOVRds);
013050130307           if trulPOVRds.§ULPoerr <> *blank;  //?Nessuna override rilevata?
013060130321             //trulPOVRds.§ULPoOutQ  = '*JOB      ';
013070130321             trulPOVRds.§ULPoOutQ  = 'PRT01     ';
013080130307             trulPOVRds.§ULPoFType = '*LDVUNI   ';
013090130307             trulPOVRds.§ULPoUDta  = 'LDV_UNI   ';
013100130306           endif;
013110130306         endif;
013120130306      /end-free
013130030515      *
013140030515     c                   ENDSR
013150071112      *
013160071112      *---------------------------------------------------------------*
013170071112      *?REPERIMENTO N° MAX DI LDV STAMPABILI DA TAB. "VPO"           ?*
013180071112      *---------------------------------------------------------------*
013190071112     c     Rep_TabVPO    BEGSR
013200071112      *
013210071112      * Aggancio tabella "VPO" per reperimento del n° LdV stampabili
013220071112      *   limite (per congelamento e blocco stampa)
013230071112     c                   clear                   dVPO
013240071112     c                   clear                   TIBS02ds
013250071112     c                   eval      T02mod  = 'C'
013260071112     c***                eval      T02sif  = KNSIF
013270071112     c                   eval      T02cod  = 'VPO'
013280071112     c                   movel(P)  'VPO'         T02ke1
013290071112     c                   call      'TIBS02R'
013300071112     c                   parm                    KPJBA
013310071112     c                   parm                    TIBS02ds
013320071112     c                   if        T02err  = *blanks
013330071112     c                   movel     T02uni        dVPO
013340071203     c                   if        §VPOMaxLdv = *zeros
013350071203     c                   eval      §VPOMaxLdv = 10000
013360071203     c                   endif
013370071112     c                   else
013380071112     c                   eval      §VPOMaxLdv = 10000
013390071112     c                   endif
013400071112      *
013410071112     c                   ENDSR
013420070123      *
013430070123      *---------------------------------------------------------------*
013440070306      *?ALTRE OPERAZIONI INIZIALI (NON PIÙ IN *INZSR)                ?*
013450070123      *---------------------------------------------------------------*
013460070123     c     sr_Start      BEGSR
013470070123      *
013480080709      /free
013490081017         select;
013500081017           when  DB0fa4 = *blank;
013510081017             DB0fa4 = '1';
013520081020           when  DB0fa4 = 'P';
013530081017             DB0fl6 = *on;
013540081017         endsl;
013550080709      /end-free
013560080709      *
013570070123if  1c                   if        DB0fl7 = '1'
013580130710     c                   close     FNLSB6P
013590130710     c                   open      FNLSB6P
013600081015     c                   clear                   CurLineA4
013610070123     c                   clear                   Page4
013620070123e   1c                   endif
013630070123      *
013640070123      * »»» CHIODO: SE richiamato per la sola verifica del modulo «««
013650070308      *     RIPULISCO il "livello della filiale" (S/1/2) perchè:
013660070123      *     1) sarò partito dal S.I. di sede, ma
013670070123      *     2) sarà stata cambiata la lista di librerie come x S.I. di
013680070308      *        filiale (dal CL FNLSE6C1)
013690070123if  1c                   if             DB0ris  = 'R'
013700070123     c                             and  DB0fl9  = *on
013710070123     c                             and  DUTlpo <> *blanks
013720070123     c                   clear                   DUTlpo
013730070123e   1c                   endif
013740070123      *
013750070308      * Apertura archivi di sede o filiale - comunque utilizzati
013760070123if  1c                   if        DUTlpo = 'S'
013770070123if  2c                   if        NOT  %open(FIAR531C)
013780070123     c                   open      FIAR531C
013790070123e   2c                   endif
013800070123x   1c                   else
013810070123if  2c                   if        NOT  %open(FIAR401L)
013820070123     c                   open      FIAR401L
013830070123e   2c                   endif
013840070123if  2c                   if        NOT  %open(FIAR501L)
013850070123     c                   open      FIAR501L
013860070123e   2c                   endif
013870070123if  2c                   if        NOT  %open(FLNUF01L)
013880070123     c                   open      FLNUF01L
013890070123e   2c                   endif
013900070123e   1c                   endif
013910110427      *
013920110427      * Pulizia del PRTF
013930110427     c                   exsr      ClrPRTF
013940070123      *
013950070123     c                   clear                   wCOPIE
013960070123      *
013970070123     c                   ENDSR
013980080624      *
013990080624      *---------------------------------------------------------------*
014000080624      *?OPERAZIONI FINALI                                            ?*
014010080624      *---------------------------------------------------------------*
014020080624     c     sr_End        BEGSR
014030080624      *
014040080624      * Chiusura pgm
014050080919if  1c                   IF        DB0tla <> *blanks
014060130710     c                   close     FNLSB6P
014070080708      /free
014080080708       Qcmd = C_CmdDltOvr;
014090080708       callp(e) qCmdExc(Qcmd : %size(Qcmd));
014100080708      /end-free
014110080624     c                   clear                   TIBS02ds
014120080624     c                   eval      T02tla = 'C'
014130080624     c                   call      'TIBS02R'
014140080624     c                   parm                    KPJBA
014150080624     c                   parm                    TIBS02ds
014160080624     c                   reset                   FNLV80ds
014170080624     c                   call      'FNLV80R'
014180080624     c                   parm                    KPJBA
014190080624     c                   parm                    FNLV80ds
014200080624     c                   clear                   FIDN12ds
014210080624     c                   eval      I12tla = 'C'
014220080624     c                   call      'FIDN12R'
014230080624     c                   parm                    FIDN12ds
014240090605      /free
014250090622           if  §VPOrcm <= wDate   and   DUTlpo  <> 'S';
014260090615             reset  TNSD99ds;
014270090615             tnsd99r ( TNSD99ds );
014280090615           endif;
014290100322           if  §VPOcas <= wDate   and   DUTlpo  <> 'S';
014300100208             clear  trulINTds;
014310100208             iINTtla = 'C';
014320100208             trulINTr ( TRULINTds );
014330100322           endif;
014340090605      /end-free
014350080624     c                   eval      *inLR  = *on
014360080624e   1c                   ENDIF
014370080919      *
014380080919      * Ritorno al chiamante
014390080919     c                   return
014400080624      *
014410080624     c                   ENDSR
014420030515      *
014430030515      *---------------------------------------------------------------*
014440050217      *?Pulisco i campi di stampa                                    ?*
014450030515      *---------------------------------------------------------------*
014460030515     c     ClrPRTF       BEGSR
014470030515      *
014480130418     c                   clear                   A4prtBOX
014490120530     c*//                clear                   A4LogoBAR
014500120530     c*//                clear                   A4LogoDA1
014510120530     c*//                clear                   A4LogoBR2
014520120530     c*//                clear                   A4LogoDA2
014530120530     c                   clear                   A4LogoBRT
014540120530     c                   clear                   A4LogoDA3
014550130418     c                   clear                   A4prtLDV
014560080604     c                   clear                   A4prtLDVB
014570080922     c                   clear                   A4prtDPD
014580030515     c                   clear                   A4prtFED1
014590030515     c                   clear                   A4prtFED2
014600030515     c                   clear                   A4prtFED3
014610030515     c                   clear                   A4prtAR51
014620030515     c                   clear                   A4prtAR52
014630030515     c                   clear                   A4prtAR53
014640030909     c                   clear                   A4prtAR5A
014650040607     c*** stessi campi:  clear                   A4prtAR5B
014660040607     c*** stessi campi:  clear                   A4prtAR5C
014670070321     c*** stessi campi:  clear                   A4prtAR5G
014680040311     c*** no campi:      clear                   A4prtSP0
014690040607     c*** no campi:      clear                   A4prtSP1
014700080709     c*** no campi:      clear                   A4prtSP2
014710040311     c*** no campi:      clear                   A4prtNUL
014720030911     c                   clear                   A4prtGEN
014730040607     c                   clear                   A4prtDK0
014740040607     c                   clear                   A4prtDK1
014750040607     c                   clear                   A4prtDK2
014760040607     c                   clear                   A4prtDK3
014770040607     c                   clear                   A4prtDK4
014780040607     c                   clear                   A4prtDK5
014790081222     c                   clear                   A4prtDLL
014800041019     c                   clear                   A4prtDL0
014810050318     c                   clear                   A4prtDL0A
014820050318     c                   clear                   A4prtDL0B
014830050318     c                   clear                   A4prtDL0C
014840050606     c                   clear                   A4prtDL0D
014850050606     c                   clear                   A4prtDL0E
014860050606     c                   clear                   A4prtDL0F
014870041028     c                   clear                   A4prtDL1A
014880041028     c                   clear                   A4prtDL1B
014890050318     c                   clear                   A4prtDL1C
014900050606     c                   clear                   A4prtDL1D
014910050606     c                   clear                   A4prtDL1E
014920041019     c                   clear                   A4prtDL2
014930060614     c                   clear                   A4prtDL21
014940041019     c                   clear                   A4prtDL3
014950041019     c                   clear                   A4prtDL4
014960050318     c                   clear                   A4prtDL5
014970050606     c                   clear                   A4prtDL6
014980050606     c                   clear                   A4prtDL7
014990050606     c                   clear                   A4prtDL8
015000050606     c                   clear                   A4prtDL9
015010050114     c                   clear                   A4prtAC0
015020050114     c                   clear                   A4prtAC1A
015030050114     c                   clear                   A4prtAC1B
015040050114     c                   clear                   A4prtAC2
015050050114     c                   clear                   A4prtAC3
015060050114     c                   clear                   A4prtAC4
015070050214     c                   clear                   A4prtES0
015080050214     c                   clear                   A4prtES0A
015090050214     c                   clear                   A4prtES0B
015100050215     c                   clear                   A4prtES1A
015110050215     c                   clear                   A4prtES1B
015120050214     c                   clear                   A4prtES2
015130050214     c                   clear                   A4prtES3
015140050215     c                   clear                   A4prtES4
015150050214     c                   clear                   A4prtDB0
015160041207     c                   clear                   A4prtDT0
015170040607     c                   clear                   A4prtDT1
015180040607     c*** stessi campi:  clear                   A4prtDT2
015190040607     c*** stessi campi:  clear                   A4prtDC1
015200030515     c                   clear                   A4prtSEG
015210030515     c                   clear                   A4prtMSG
015220030515      *
015230041207     c                   reset                   dAR8
015240060524     c                   clear                   dsBL4A
015250061030     c                   clear                   dsBL4I
015260061108     c                   clear                   dTA4Q
015270100514     c                   clear                   dOrm01
015280041207      *
015290070514     c                   reset                   $Mamma
015300130215     c                   reset                   $Mamma_FedEx
015310070306     c                   reset                   $FIARN
015320060921     c                   reset                   $FIAR5
015330030515     c                   reset                   $First
015340030515     c                   reset                   $FirstSCR
015350060921     c                   reset                   $FIAR8
015360130306     c                   reset                   $§PDF
015370040608     c                   reset                   $DDT_o
015380040608     c                   reset                   $DDT_p
015390040608     c                   reset                   $DDTlastPg
015400030515     c                   reset                   $Partic
015410030710     c                   reset                   $NoDesPartic
015420030910     c                   reset                   $BolloVirt
015430061106     c                   reset                   $No_CF
015440071114     c                   reset                   $No_PI
015450040707     c                   reset                   $No_x_Cons
015460101125     c                   reset                   $Rit_A_Inc
015470050513     c                   reset                   $A4_x_ScNS
015480070306     c                   reset                   $LascAvv
015490070319     c                   reset                   $ConsParz
015500070315     c                   reset                   $C_A_
015510030515     c                   reset                   $CAeur
015520090302     c                   reset                   $TabPXA
015530110411     c                   reset                   $TabPXA2
015540090924     c                   reset                   $BoxNxD
015550100513     c                   reset                   $PreavvORM
015560110427     c                   reset                   $GeoAtt
015570070717     c                   clear                   wGeoTel
015580070717     c                   clear                   wGeoCon
015590070717     c                   clear                   Wdsp
015600090527     c                   clear                   WddtCLN
015610090527     c                   clear                   WdspCLN
015620030717     c                   clear                   W_CoOr
015630040608     c                   clear                   wOmaRowT
015640040608     c                   clear                   wOmaRowD
015650050624     c                   clear                   wBolloVirt
015660030515     c                   clear                   Wcas
015670030515     c                   clear                   Wiva
015680071015     c                   clear                   Wpdc4a
015690080926     c                   clear                   wPage
015700080704     c                   reset                   $LdVxP
015710090121     c*** (da non fare:) clear                   wNumLdV
015720090121     c*** (da non fare:) clear                   wSeqLdV
015730071114     c                   clear                   wB0cfi
015740071114     c                   clear                   wB0piv
015750031015     c                   clear                   wB0nrs
015760070514     c                   clear                   wB0ndc
015770070514     c                   clear                   wB0ntc
015780070514     c                   clear                   wB0dcp
015790081119     c                   clear                   wB0tmc
015800101125     c                   clear                   wB0dcm
015810101125     c                   clear                   wB0icc
015820101125     c                   clear                   wB0ica
015830130124     c                   clear                   wB0fiv
015840131205     c                   clear                   wEVBdev
015850131205     c                   clear                   wEVBhev
015860030515      *
015870030515     c                   clear                   $NP
015880030515     c                   clear                   $SC1
015890030515     c                   clear                   $SC2
015900030905     c                   clear                   $QUM
015910040607     c                   clear                   $AR8o1
015920070319     c                   reset                   $CP
015930070319     c                   reset                   $CPdcm
015940030515      *
015950030515     c                   movea     *zeros        *in(01)
015960030515      *
015970030515     c                   ENDSR
015980030515      *
015990030515      *---------------------------------------------------------------*
016000050217      *?Determino il modulo LDV da usare per la stampa               ?*
016010120530      *?(se A5 o A4 e se DPD, FedEx o BRT)                           ?*
016020030515      *---------------------------------------------------------------*
016030040607     c     TipoLDV       BEGSR
016040070212      *
016050080703      * Impostazione ds "Flag operativi DB0FLO di FNLSB5DS"
016060070215     c                   movel     DB0flo        dDB0flo
016070070215      *
016080070212      * Impostazione della data spedizione
016090070212      * (se non già impostata in subr. "CarBLP" o "CarARB")
016100070212if  1c                   if        Wdsp   =  *zeros
016110070212if  2c                   if        DB0mgs <> *zeros
016120070212     c                   eval      Wdsp   =  (DB0aas * 10000) + DB0mgs
016130070212x   2c                   else
016140070212      *   prendo la data del giorno se non c'è la data spedizione
016150070212     c                   time                    w0140
016160070212     c                   move      w0140         w0080
016170070212     c     *eur          movel     w0080         Data_EUR
016180070212     c     *iso          movel     Data_EUR      Wdsp
016190070212e   2c                   endif
016200070212e   1c                   endif
016210040311      *
016220040311      * Dati della linea di partenza
016230040311if  1c                   if        DB0lnp <> SAVlnp
016240040311     c                   eval      SAVlnp =  DB0lnp
016250040311     c     DB0lnp        chain     AZORG
016260040311if  2c                   if        %found(AZORG01L)
016270040311     c                   movel     ORGde3        OG143
016280040311     c                   movel     ORGde8        OG148
016290040311x   2c                   else
016300040311     c                   clear                   OG143
016310040311     c                   clear                   OG148
016320040311e   2c                   endif
016330040311e   1c                   endif
016340040607      *
016350070308      * Dati della filiale del mittente
016360040607     c                   eval      WX     =  DB0ccm / 10000
016370040607if  1c                   if        WX     <> ORGfil
016380040607     c     WX            chain     AZORG
016390040607e   1c                   endif
016400040607if  1c                   if        %found(AZORG01L)
016410040607     c                   movel     ORGde3        OG143ccm
016420040607x   1c                   else
016430040607     c                   clear                   OG143ccm
016440040607e   1c                   endif
016450040311      *
016460040311      * Dati della linea di arrivo
016470040311if  1c                   if        DB0lna <> SAVlna
016480040311     c     DB0lna        chain     AZORG
016490040311if  2c                   if        %found(AZORG01L)
016500070312     c                   movel     ORGde6        OG146arr
016510061013     c                   movel     ORGde8        OG148arr
016520070312     c                   movel     ORGdf0        OG150arr
016530040311x   2c                   else
016540070312     c                   clear                   OG146arr
016550061013     c                   clear                   OG148arr
016560070312     c                   clear                   OG150arr
016570040311e   2c                   endif
016580050510e   1c                   endif
016590070212      *
016600070212      * Dati della capofila della linea di arrivo
016610070306      * - per verificare TELLUS   e
016620070306      * - per non stampare la "copia per il destinatario" nel caso
016630070306      *   di precedente "Lasciato Avviso"
016640070717      *
016650070717      *** Imposta flag attivazione GEO x LNA
016660070717     c                   eval      wGeoCon = A§OGtlls
016670070717     c                   eval      wGeoTel = A§OGgeot
016680070717if  1c                   if             A§OGdda  > *zeros
016690070717     c                             and  A§OGdda  < C_DateMax_A
016700070717     c                             and  A§OGdda <= wDateA
016710110427     c                   eval      $GeoAtt = *on
016720070717e   1c                   endif
016730070717      *
016740070717if  1c                   IF        DB0tbo = 'A'  AND
016750070717     c                                 (wGeoCon <> 'S'
016760070717     c                              or  wGeoTel <> 'S'
016770110427     c                              or  $GeoAtt <> *on)
016780070212      * - Reperimento capofila (£6)
016790070212if  2c                   if        DB0lna <> D55lin
016800070212     c                   reset                   FNLV55ds
016810070212     c*** già così:      eval      D55tpt =  '6'
016820070212     c                   eval      D55lin =  DB0lna
016830070212     c                   eval      D55drf =  Wdsp
016840070212     c                   call      'FNLV55R'
016850070212     c                   parm                    FNLV55ds
016860070212if  3c                   if        D55err <> *blanks
016870070212     c                   eval      D55tfa =  DB0lna
016880070212e   3c                   endif
016890070212e   2c                   endif
016900070321      * - Reperimento 146ª e 148ª descrizioni dell'organigramma
016910070306      *   (flag Tellus del capofila e flag gestione telefonate GEO)
016920070212if  2c                   if        D55tfa <> ORGfil
016930070212     c     D55tfa        chain     AZORG
016940071109if  3c                   if        %found(AZORG01L)
016950070717     c                   movel     ORGde6        OG146cf£6
016960070717     c                   movel     ORGde8        OG148cf£6
016970071109x   3c                   else
016980071109     c                   clear                   OG146cf£6
016990071109     c                   clear                   OG148cf£6
017000071109e   3c                   endif
017010071109e   2c                   endif
017020070717      *** Imposta flag attivazione GEO x capofila £6
017030070717     c                   eval      wGeoCon = £6OGtlls
017040070717     c                   eval      wGeoTel = £6OGgeot
017050071109if  2c                   if             £6OGdda  > *zeros
017060070717     c                             and  £6OGdda  < C_DateMax_A
017070070717     c                             and  £6OGdda <= wDateA
017080110427     c                   eval      $GeoAtt = *on
017090071109e   2c                   endif
017100070717      *
017110070212e   1c                   ENDIF
017120050510      *
017130050510      * Verifica se richiesta stampa in A4 in tab. "7R"
017140050510      *   (gestione particolarità consegna)
017150050510     c                   clear                   ds7R
017160050510if  2c                   if        DB0gma <> *blanks
017170070312     c                   eval      TBLcod =  '7R'
017180050510     c                   movel(p)  DB0gma        TBLkey
017190050510     c     K03TAB        chain     TABEL
017200050510if  2c                   if        %found(TABEL00F)
017210050510     c                   movel     TBLuni        ds7R
017220050510e   2c                   endif
017230050510e   1c                   endif
017240071015      *
017250071015      * Verifica se LDV con "Ritiro Porto Franco Prepagato"
017260071015     c                   clear                   dsBL4M
017270071015     c                   if        DB0tbo = '8'
017280071015     c                   exsr      RepAR4_M
017290071015     c                   endif
017300130315      *
017310130315      * Reperimento dati da FIAR5 / "GEN"
017320130315      *   (per verificare se esistono Packing-List e/o PDF allegati
017330130315      *    - oltre che per reperire Nominativo e Telefono)
017340130315     c                   exsr      RepAR5gen
017350040607      *
017360130315      * Verifica se LDV con DDT e/o PDF
017370130315     c                   exsr      CercaDDToPDF
017380061031      *
017390071114      * Verifica se richiesta stampa in A4 per richiesta Codice Fiscale
017400071114      *   e/o Partita IVA al cliente.
017410120503      *-=»>?01/08/2008:?                                          <«=-*
017420080801      *-=»> Abolite richieste di Codice Fiscale e/o Partita Iva.  <«=-*
017430080801      *-=»> Tolta ricerca di 'sti 2 dati, lasciando il defualt di <«=-*
017440080801      *-=»> NON richiesta: $No_CF = *off e $No_PI = *off.         <«=-*
017450120503      *-=»>?03/05/2012:?                                          <«=-*
017460120503      *-=»>?Ripristinate richieste di Cod. Fiscale e/o Part. Iva.?<«=-*
017470120503if  1c                   if        DUTlpo <> 'S'
017480120503     c                   exsr      CheckCFPI
017490120503e   1c                   endif
017500070306      *
017510070319      * Verifica se richiesta stampa in A4 per note in FIARN00F
017520070306      *   (da stampare nel box "Note per l'autotrasportatore")
017530070319      * » se stampa in arrivo
017540070319if  1c                   if        DB0tbo = 'A'
017550070306     c                   exsr      CheckFIARN
017560070319e   1c                   endif
017570090302      /free
017580090302
017590090302         // Verifica se richiesta stampa in A4 per note specifiche
017600090302         //   per il cliente mittente (v. tab. "PXA")
017610090302         //   (da stampare nel box "Note per l'autotrasportatore")
017620090302         exsr  CheckTabPXA;
017630090924
017640090924         // Verifica se richiesta stampa in A4 per note specifiche
017650090924         //   per il destinatario (da parte del mittente)
017660090924         //   (da stampare in un box a parte)
017670090924         if  DB0ccm = c_CliSG;
017680090924           $BoxNxD = *on;
017690090924         endif;
017700090302      /end-free
017710070319      *
017720070319      * Verifica se richiesta spedizione già parzialmente consegnata
017730070319      *   (da stampare nel box "Note per l'autotrasportatore")
017740070717      * » se stampa in arrivo e LNA - o capofila £6 - con GEO attivato
017750070717      * completamente
017760070319if  1c                   if             DB0tbo   =  'A'
017770070514     c                             and  wB0dcp   >  *zeros
017780110427     c                             and  $GeoAtt  =  *on
017790140514      * -?Reperimento consegne parziali precedenti da FNART?
017800140514     c                   exsr      SchConsParz
017810140514     c                   if        xx > *zero
017820070413     c                   eval      $ConsParz     =  *on
017830140514     c                   endif
017840070319e   1c                   endif
017850070315      *
017860070315      * Verifica se richiesta stampa in A4 per C.A. aperta
017870070717      * » se stampa in arrivo e LNA - o capofila £6 - con GEO attivato
017880070717      * completamente
017890070319if  1c                   if             DB0tbo   =  'A'
017900110427     c                             and  $GeoAtt  =  *on
017910070316     c                   exsr      CercaCA
017920070316e   1c                   endif
017930030515      *
017940030515      * Tipo modulo (per la separazione dei box: vettore/destinatario)
017950030515      *   (*in69 = formato A5 / not *in69 = formato A4)
017960030515     c                   eval      *in69  = *on
017970030515      *
017980040607      * Tipo LDV (per richiesta firma per ricevuta della merce senza
017990040607      *   riserve)
018000120530      *   (70 = BRT, 71 = DPD, 72 = FedEx, 73 = EuroExpress)
018010080912     c                   movea     '1000'        *in(70)
018020081210      /free
018030081210         // - LDV DPD
018040081210         //if  §OGntw = 'DPD'   -   NON della LnP, MA della fil. mitt.:
018050081210         if  §CMntw = 'DPD';
018060081210           *in71 = *on;
018070081210         endif;
018080081210      /end-free
018090040311      *
018100030515sel 1c                   SELECT
018110030515      *
018120030515      * Rimesso Mittente: va bene il formato A5
018130030515      *   (se non richiesta la forzatura del formato "A4")
018140040707w   1c                   WHEN      DB0ris = 'M'   and  DB0fa4 <> 'S'
018150040707      *
018160040707      * LDV POSTE non valida per la consegna: va bene il formato A5
018170040707      *   (se non richiesta la forzatura del formato "A4")
018180040707w   1c                   WHEN      §OGntw = 'PPT' and  DB0fa4 <> 'S'
018190071213      *
018200081210     *** * LDV DPD
018210081210     *** > (Spostato prima per NON rendere incompatibile DPD con il box
018220081210     *** >  di note aggiuntive "promemoria per l'autotrasportatore")
018230081210w   1***c*** non della LNP  WHEN      §OGntw = 'DPD' - ma della fil. mitt.:
018240081210w   1***c                   WHEN      §CMntw = 'DPD'
018250081210     ***c                   movea     '0100'        *in(70)
018260071213      *
018270071213      * LDV con mittente FedEx
018280150429w   1c                   WHEN      DB0ccm = c_CliFedEx   or
018290150429     c                             DB0ccm = c_CliFedEx2
018300080922     c                   movea     '0010'        *in(70)
018310071213     c                   eval      *in69  = *off
018320040607      *
018330040607      * LDV con DDT del mittente
018340081013      * - IN FUTURO: se al 1° tentativo di consegna
018350081013      *              (DB0FL1 = "D" - v. pgm FNLRS9R)
018360080917w   1c                   WHEN           $FIAR8 =  *on
018370081013     c***-=»>                      and  DB0fl1 <> 'D'
018380040607     c                   eval      *in69  = *off
018390061102      *
018400071114      * Codice Fiscale o Partita IVA da richiedere al cliente
018410071114w   1c                   WHEN           $No_CF = *on
018420071114     c                             or   $No_PI = *on
018430061102     c                   eval      *in69  = *off
018440070306      *
018450070306      * LDV con Note (dal file FIARN00F)
018460070306w   1c                   WHEN      $FIARN = *on
018470070306     c                   eval      *in69  = *off
018480080801      *
018490080801      * LDV già parzialmente consegnata
018500080801w   1c                   WHEN      $ConsParz   =  *on
018510080801     c                   eval      *in69  = *off
018520070315      *
018530070315      * LDV con C.A. aperta per la spedizione (dal file FNDCT00F)
018540070315w   1c                   WHEN      $C_A_  = *on
018550070315     c                   eval      *in69  = *off
018560060524      *
018570070123      * LDV con mittente "PraOil oleodotti italiani"
018580081127w   1c                   WHEN           DB0ccm = c_CliPraOil
018590060524     c                             or  (DB0fbr = 'S'
018600081127     c                             and  §B4Aks = c_CliPraOil)
018610060524     c                   eval      *in69  = *off
018620090302      /free
018630090302
018640090302           // LDV con mittente con promemoria per AUT in tab. "PXA"
018650110411           WHEN  $TabPXA = *on  or  $TabPXA2 = *on;
018660090302             *in69  = *off;
018670090924
018680090924           // LDV con mittente con promemoria per destinatario
018690090924           WHEN  $BoxNxD = *on;
018700090924             *in69  = *off;
018710090302      /end-free
018720030515      *
018730120530      * LDV BRT
018740150224     *** * - con Bancali da rendere al mittente
018750150224     *** *   o   Bancali da riportare a magazzino
018760150224      * - con Destinatario supermercato
018770030515      *   e/o Promemoria per autotrasportatore
018780040223      *   [Se non trovo niente su FIAR5 né x supermercati né x bancali
018790030515      *    ma ho queste condizioni: devo stampare la bolla in "A4" per
018800030515      *    poter stampare comunque il box vuoto con la memo "Destina-
018810050510      *    tario Supermercato" per poter scrivere a mano i promemoria
018820050510      *    per l'autotrasportatore - ci dovrebbero comunque sempre
018830050510      *    essere note per supermercato.]
018840050510      *   e/o con formato A4 previsto in tab. "7R" (partic. consegna)
018850150224w   1c*//                WHEN         %subst(DB0gva:1:1) = 'B'
018860150224     c*//                          or %subst(DB0gva:1:1) = 'O'
018870150224w   1c                   WHEN                DB0tc1      = 'S'
018880030717     c                             or        DB0tc2      = 'S'
018890071015     c                             or        §7Rldv      = 'S'
018900030515     c                   eval      *in69  = *off
018910071015      *
018920071015      * LDV con ritiro porto franco prepagato
018930071015w   1c                   WHEN          (DB0tbo   = '8'
018940071015     c                             and  §B4poe   > *zeros
018950071015     c                             and  §B4nor   > *zeros)
018960071015     c                   eval      *in69  = *off
018970071015      *
018980071015      * LDV con ritiro contestuale alla consegna
018990100608w   1c**!!!              WHEN          (DB0gma   = Ktab7V_RC
019000100608w   1c                   WHEN          (§7Rrc    = 'S'
019010071015     c                             and  §7Rldv   = 'S')
019020071015     c                   eval      *in69  = *off
019030030515      *
019040030515      * - con Forzatura richiesta al formato "A4"
019050030515w   1c                   WHEN      DB0fa4 = 'S'
019060030515     c                   eval      *in69  = *off
019070030515      *
019080030515e   1c                   ENDSL
019090050513      *
019100050513      * Verifica se con Segnacolli NON sequenziali
019110050513      * (di numero superiore a quelli che stanno al posto del range)
019120050513      * DA CONTROLLARE COMUNQUE (A4 o A5 che sia), VISTO CHE:
019130050513      * - se è previsto un A5, potrebbe scattare l'A4 (per numero
019140050513      *   di segnacolli da elencare troppo alto per lo spazio
019150050513      *   disponibile tra le "NOTE E PARTICOLARITA'")
019160050513      * - se è già previsto un A4, i segnacolli non sequenziali
019170050513      *   potrebbero comunque essere elencabili tra le "NOTE E
019180050513      *   PARTICOLARITA'" (e risparmiare una pagina in caso di
019190050513      *   LdV con DDT del mittente)
019200050513     c*** (già fatto:)   reset                   $A4_x_ScNS
019210050513if  1c                   IF             DB0fns = 'S'
019220050513     c                             and  DB0ncl >  2
019230050513      * - - imposto le righe di "Note e Particolarità" da stampare
019240050513     c                   exsr      SchPARTIC
019250050513      * - - controllo quante righe di "Note e Particolarità" servireb-
019260050513      *     bero per segnalare i segnacolli non sequenziali, in base
019270050513      *     al numero di colli (considerando che riuscirei ad elencare
019280050513      *     max 4 segnacolli per ogni riga di "Note e Particolarità")
019290050513     c                   eval      WelSNS = $elSNSxA5
019300050513     c     DB0ncl        div       WelSNS        YY
019310050513     c                   mvr                     XX
019320050513if  2c                   if        XX     >  0
019330050513     c                   eval      XX     =  YY + 1
019340050513x   2c                   else
019350050513     c                   eval      XX     =  YY
019360050513e   2c                   endif
019370050513      * - - conto anche la riga per l'intestazione
019380050513     c                   add       1             XX
019390050513      * - - conteggio le righe di "Note e Particolarità" utilizzate
019400050513      *     per reperire la 1ª riga di "Note e Particolarità" disponi-
019410050513      *     bile
019420050513     c                   eval      *in31  =  *off
019430050513     c                   eval      YY     =  1
019440050513     c     *blanks       lookup    $NP(yy)                                31
019450050513if  2c                   if        *in31  =  *on
019460050513     c                   eval      YY     =  YY - 1
019470050513x   2c                   else
019480050513     c                   eval      YY     =  %elem($NP)
019490050513e   2c                   endif
019500050513      * - - controllo se ho il numero di campi "Note e Particolarità"
019510050513      *     liberi a sufficienza per i segnacolli da elencare.
019520050513      *     Se non ci stanno => serve un modulo A4
019530050513if  2c                   if        XX > %elem($NP) - YY
019540050513     c                   eval      $A4_x_ScNS  = *on
019550050513     c                   eval      *in69  = *off
019560050513e   2c                   endif
019570050513e   1c                   ENDIF
019580030515      *
019590030515      * Imposto il numero di riga massima raggiungibile per l'eventuale
019600030515      *  stampa dei segnacolli non sequenziali
019610040607if  1c                   if        *in69
019620030515     c                   eval      wMaxLine = $MaxLine1
019630040607x   1c                   else
019640030515     c                   eval      wMaxLine = $MaxLine2
019650040607e   1c                   endif
019660030515      * Memo: ho stampato (sto stampando...) almeno un modulo A4 o A5.
019670030515      *   (utile solo per la segnalazione in stampa delle oltre 6.000
019680071008      *   pagine già stampate - in un formato e/o nell'altro).
019690080926      /free
019700080926         select;
019710100924           //?»»» Richiesta la stampa per PDA (4 LdV x A4)?
019720100924           //    ?di LdV per consegna AL 1° tentativo, ma?
019730100924           //    ?reperita packing-list (da stampare "a parte")?
019740100924           //?(la stampa della relativa packing-list verrà richiesta?
019750100924           // ?successivamente, dopo la stampa di tutte le LdV)?
019760100924           when  DB0fa4  = '4'    and
019770100924               //DB0fl1 <> 'D'    and
019780100924                 *in69   = *off   and
019790130306                 $FIAR8  = *on    and
019800130306                 $§PDF   = *off;
019810100924             DB0tms = 'D';
019820130306           //?»»» Richiesta la stampa per PDA (4 LdV x A4)?
019830130306           //    ?di LdV per consegna AL 1° tentativo, ma?
019840130306           //    ?reperita immagine PDF (da stampare "a parte")?
019850130306           //?(la stampa delle relative immagini verranno richieste?
019860130306           // ?successivamente, dopo la stampa di tutte le LdV e?
019870130306           // ?delle relative packing-list)?
019880130306           when  DB0fa4  = '4'    and
019890130306                 $FIAR8  = *off   and
019900130306                 $§PDF   = *on;
019910130307             DB0tms = 'i';
019920130306           //?»»» Richiesta la stampa per PDA (4 LdV x A4)?
019930130306           //    ?di LdV per consegna AL 1° tentativo, ma?
019940130306           //    ?reperita packing-list (da stampare "a parte") e?
019950130306           //    ?immagine PDF (anch'essa da stampare "a parte")?
019960130306           //?(la stampa della relativa packing-list e delle relative?
019970130306           // ?immagini verranno richieste successivamente, dopo la?
019980130306           // ?stampa di tutte le LdV)?
019990130306           when  DB0fa4  = '4'    and
020000130306               //DB0fl1 <> 'D'    and
020010130306                 *in69   = *off   and
020020130306                 $FIAR8  = *on    and
020030130306                 $§PDF   = *on;
020040130306             DB0tms = 'Y';
020050100924           //?»»» Richiesta la stampa per PDA (4 LdV x A4)?
020060080926           when  DB0fa4 = '4';
020070100927             DB0tms = 'Q';
020080100924           //?»»» Richiesta la stampa della packing-list per PDA?
020090081007           when  DB0fa4 = 'P'   and   $FIAR8 = *on;
020100080926             DB0tms = 'P';
020110081007           when  DB0fa4 = 'P'   and   $FIAR8 = *off;
020120081007             //DB0tms = *blank;
020130130307           //?»»» Richiesta la stampa normale e reperita immagine PDF?
020140130307           when  *in69  and  $§PDF;
020150130307             DB0tms = 'I';
020160130307           when  Not *in69  and  $§PDF;
020170130307             DB0tms = 'J';
020180100924           //?»»» Richiesta la stampa normale?
020190080926           when  *in69;
020200080926             DB0tms = '5';
020210080926           other;
020220080926             DB0tms = '4';
020230080926         endsl;
020240080926      /end-free
020250040707      *
020260040707      * Controllo se devo stampare "Non utilizzare per la consegna"
020270040707      * nella copia per il vettore,?senza?copia per il destinatario:
020280040707     c*** (già fatto:)   reset                   $No_x_Cons
020290071203      * - LdV recapito c/assegni:
020300090511      *   stampata in arrivo e con
020310120113if  1c                   if             DB0tbo <> 'P'
020320070306     c                             and  DB0tbo <> '8'
020330090511     c                             and  DB0lnp =  102
020340070306     c                             and  DB0cbo =  'FW'
020350071203     c                             and  DB0nft =  *zeros
020360071203      *   - fil. arrivo con GEO attivato
020370071203     c                             and (DB0fl1    =  'A'
020380071203     c                              or  wGeoCon   =  'S'
020390071203      *   - fil. arrivo abilitata al picking
020400120113     c                              or  A§OGpck   =  'S')
020410070306     c                   eval      $No_x_Cons  =  *on
020420120113e   1c                   endif
020430120113      *//* - LdV Poste
020440120113w   1c*//                   when      §OGntw   = 'PPT'
020450120113     c*//                   eval      $No_x_Cons  =  '2'
020460101125      /free
020470101125         // -?Controlla se "tagliatella" per Ritorno all'Incasso?
020480101129         //$Rit_a_Inc = ( DB0fa4  = '4'    and
020490101129         //               DB0tbo <> 'P'    and  DB0tbo <> '8'   and
020500101129         //               wB0dcm  > *zero  and
020510101129         //           ( ( DB0cas >  *zero  and  wB0icc =  'R' )  or
020520101129         //             ( DB0ift >  *zero  and  wB0ica =  'R' ) ) );
020530101129         // -?Controlla se LdV per Ritorno all'Incasso?
020540101129         $Rit_a_Inc = ( DB0fl4 <> *blank and
020550101129                        DB0tbo <> 'P'    and  DB0tbo <> '8'   and
020560101129                        wB0dcm  > *zero  and
020570101129                    ( ( DB0cas >  *zero  and  wB0icc =  'R' )  or
020580101129                      ( DB0ift >  *zero  and  wB0ica =  'R' ) ) );
020590101125      /end-free
020600030515      *
020610030515     c                   ENDSR
020620040607      *
020630040607      *---------------------------------------------------------------*
020640130315      *?Verifica esistenza dati del DDT allegato da stampare  o      ?*
020650130315      *?               di uno o più PDF allegati da stampare         ?*
020660040607      *---------------------------------------------------------------*
020670130315     c     CercaDDToPDF  BEGSR
020680040607      *
020690060921     c*** (da non fare:) reset                   $E_AR8
020700060921     c*** (già fatto:)   reset                   $FIAR8
020710130315     c*** (già fatto:)   reset                   $§PDF
020720040607     c*** (già fatto:)   reset                   $DDT_o
020730040607     c*** (già fatto:)   reset                   $DDT_p
020740040608     c*** (già fatto:)   reset                   $DDTlastPg
020750040607     c*** (già fatto:)   reset                   $AR8o1
020760040608     c*** (già fatto:)   reset                   wOmaRowT
020770040608     c*** (già fatto:)   reset                   wOmaRowD
020780041207     c*** (già fatto:)   reset                   dAR8
020790130315      *
020800130315      * -?Verifica esistenza PDF allegato?
020810130315     c                   eval      $§PDF  = (§AR5als = 'S')
020820130315      *
020830130315      * -?Verifica esistenza packing-list?
020840130315     c                   eval      $FIAR8 = (§AR5alx = 'S')
020850130315      *
020860130315if  1c                   if        NOT %open(FIAR801L)
020870130315     c                             and ($FIAR8  or  $§PDF)
020880130315     c                   open(e)   FIAR801L
020890130315      * Se errore significa che non c'è il file per cui non faccio
020900130315      *  più questa routine
020910130315if  2c                   if        %error
020920130315     c                   eval      $E_AR8 = *off
020930130315e   2c                   endif
020940130315e   1c                   endif
020950040607      *
020960130306     c                   ENDSR
020970061102      *
020980061102      *---------------------------------------------------------------*
020990071114      *?Verifica se Codice Fiscale e/o Partita Iva da richiedere al  ?*
021000071114      *?  cliente                                                    ?*
021010061102      *---------------------------------------------------------------*
021020061102      *  Il d.l. 223/06, poi convertito con la legge 248/06, ha      *
021030061102      *   reintrodotto l'obbligo di raccogliere il codice fiscale per *
021040061102      *   tutti i clienti e fornitori, siano essi aziende o privati.  *
021050061102      * SE?Porto Franco PrePagato?:                                   *
021060061102      *   il Cod.Fiscale dovrà essere del?Mittente?.                  *
021070061102      * SE?Porto Assegnato con Fattura Immediata?:                    *
021080061102      *   il Cod.Fiscale dovrà essere del?Destinatario?.              *
021090061102      * In sua assenza: dovrà essere richiesto al cliente stesso!     *
021100071114      *  Lo stesso decreto (poi convertito con la legge 248/06)      *
021110071114      *   prevede che a partire dal 01/01/2008 debba essere raccolta  *
021120071114      *   anche l'eventuale partita iva, del cliente o del fornitore. *
021130071114      * Stessi criteri già utilizzati per il codice fiscale.          *
021140061102      *---------------------------------------------------------------*
021150071114     c     CheckCFPI     BEGSR
021160061102      *
021170061106     c*** (già fatto:)   eval      $No_CF  = *off
021180071114     c*** (già fatto:)   eval      $No_PI  = *off
021190061102      *
021200071114      * Verifica se C.F./P.I. da richiedere (NON per clienti esteri)
021210071114      * - con fattura
021220071114if  1c                   IF            (DB0nft  <> *zeros
021230061106     c                              or  DB0ift  <> *zeros)
021240061103     c                             AND (
021250061102      * - - P.F. prepagato
021260061107      *     (con mittente italiano)
021270061107     c                                 (DB0tbo  =  '8'
021280061107     c                              and DB0nzm  =  *blanks)
021290061106      * - - P.A. (con fattura immediata)
021300061107      *     (con destinatario italiano)
021310061107     c                             or  (%subst(DB0tb1:1:1) = 'A'
021320061107     c                              and DB0nzd  =  *blanks)
021330061106      * - - con 2ª bolla
021340061107      *     (con destinatario italiano)
021350061103     c                             or  (DB0tb2  <> *blanks
021360121029     c                              and DB0tbo  <> 'P'
021370061107     c                              and DB0tbo  <> '8'
021380061107     c                              and DB0nzd  =  *blanks)
021390071114      * - => verirficare se da richiedere C.F. e/o P.I.
021400061103     c                                 )
021410120503      *
021420120503sel 2c                   select
021430120503      * · Esiste Partita Iva NON FITTIZIA: non chiedere nulla
021440120503w   2c                   when      wB0piv <> *blanks   and
021450120503     c                             %scan('$$' : wB0piv) = *zero
021460120503     c                   leavesr
021470120503      * · Esiste C.F. lungo 16 (privato) : non chiedere P.I.
021480120503w   2c                   when      %len( %trim( wB0cfi ) ) = 16
021490120503     c                   leavesr
021500120503      * · Esiste C.F. lungo 11 ..........: chiedere P.I. (se mancante)
021510120503w   2c*//                when      %len( %trim( wB0cfi ) ) = 11
021520120503e   2c                   endsl
021530071114      *
021540071114      * - senza codice fiscale
021550120503     c                   eval      $No_CF  = (wB0cfi = *blanks)
021560071114      *
021570071114      * - senza partita iva (se NON privato)
021580120503     c                   eval      $No_PI  = (wB0piv = *blanks)
021590071114      *
021600071114e   1c                   ENDIF
021610061102      *
021620061102     c                   ENDSR
021630070306      *
021640070306      *---------------------------------------------------------------*
021650070306      *?Verifica esistenza note da stampare (dal file FIARN00F)      ?*
021660070306      *---------------------------------------------------------------*
021670070306     c     CheckFIARN    BEGSR
021680070306      *
021690070306     c*** (già fatto:)   reset                   $FIARN
021700070306      *
021710070306      * Controllo esistenza note solo se c'è il file
021720070319     c     K06ARN11      chain     FIARN000
021730070319if  1c                   if             %found(FIARN11L)
021740070306     c                             and  ARNnob <> *blanks
021750070306     c                   eval      $FIARN = *on
021760070306e   1c                   endif
021770070306      *
021780070306     c                   ENDSR
021790070315      *
021800070315      *---------------------------------------------------------------*
021810070315      *?Verifica esistenza C.A. aperta legata alla spedizione        ?*
021820070315      *---------------------------------------------------------------*
021830070316     c     CercaCA       BEGSR
021840070315      *
021850070319     c*** (già fatto:)   reset                   $C_A_
021860070315      *
021870070319      * Richiamo del programma di comodo per recuperare le CA legate
021880070319      *   alla spedizione in esame
021890070319     c                   reset                   FIDN12ds
021900070514if  1c                   if        $Mamma = *off
021910070319     c                   eval      I12aas = DB0aas
021920070319     c                   eval      I12lnp = DB0lnp
021930070319     c                   eval      I12nrs = DB0nrs
021940070319     c                   eval      I12nsp = DB0nsp
021950070514x   1c                   else
021960070514     c                   eval      I12aas = LBLaap
021970070514     c                   eval      I12lnp = LBLlpp
021980070514     c                   eval      I12nrs = LBLnrp
021990070514     c                   eval      I12nsp = LBLnsp
022000070514e   1c                   endif
022010070319     c*** già così:      eval      I12fel = 'E'
022020070319     c***  "    "        eval      I12fan = 'E'
022030070319     c***  "    "        eval      I12fch = 'E'
022040070319      *
022050070319     c                   call      'FIDN12R'
022060070319     c                   parm                    FIDN12ds
022070070319      *
022080070319      * Se trovata almeno una CA
022090070319     c                   eval      $C_A_  = (O12nca > *zeros)
022100070315      *
022110070316     c                   ENDSR
022120070319      *
022130070319      *---------------------------------------------------------------*
022140070319      *?Verifica se stampa spedizione DOPO "Lasciato Avviso"         ?*
022150070319      *---------------------------------------------------------------*
022160070319     c     CheckLascAvv  BEGSR
022170070319      *
022180070319     c*** (già fatto:)   reset                   $LascAvv
022190070319      *
022200070319if  1c                   if        NOT  %open(FNEVB05L)
022210070319     c                   open      FNEVB05L
022220070319e   1c                   endif
022230070319      *
022240070319     c     KLDV          setgt     FNEVB000
022250070319     c     KLDV          readpe    FNEVB000
022260070319      *
022270070319      * Proseguo solo se ritrovo un evento
022280070319do  1c                   DOW       NOT  %eof(FNEVB05L)
022290070319      *
022300070319      * Verifico se evento di tipo Giacenza
022310070319     c                   eval      *in31    = *off
022320070319     c     EVBcev        lookup    $2Acev_G                               31
022330070319      **
022340070319sel 2c                   Select
022350070319      * - Evento GIACENZA --> NO LASCIATO AVVISO
022360070319w   2c                   When      *in31
022370070319     c                   leave
022380070319      * - Evento RIC --> 1° LASCIATO AVVISO
022390070319w   2c                   When      EVBcev   = 'RIC'
022400070319     c                   eval      $LascAvv = *on
022410131205      * - - salvataggio data/ora
022420131205     c                   eval      wEVBdev  = EVBdev
022430131205     c                   eval      wEVBhev  = EVBhev
022440070319     c                   leave
022450070319      * - Evento DA NON ELABORARE
022460070319x   2c                   Other
022470070319     c     KLDV          readpe    FNEVB000
022480070319e   2c                   Endsl
022490070319      **
022500070319e   1c                   ENDDO
022510070319      *
022520070319     c                   ENDSR
022530090302      /free
022540090302
022550090302       //--------------------------------------------------------------
022560090302       //?Verifica esistenza note da stampare (dal file TNTBE00F)      ?
022570090302       //--------------------------------------------------------------
022580090302       BEGSR  CheckTabPXA;
022590090302
022600090302         //clear  $TabPXA;          (già fatto)
022610110411         //clear  $TabPXA2;         (già fatto)
022620110411
022630110411         // -?Verifica esistenza "PXA" per particolarità consegna?
022640110411         If  DB0gma <> *blank;
022650110411           k_TBEcod = 'PXA';
022660110411           k_TBEke1 = DB0gma;
022670110411           k_TBEke2 = '001            ';
022680110411           setll  %kds(k05tntbe01 : 3)  TNTBE000;
022690110411           // -?Bisogna testare se nota ANNULLATA?
022700110411           //$TabPXA  = %equal(TNTBE01L);
022710110411           reade  %kds(k05tntbe01 : 2)  TNTBE000;
022720110411           $TabPXA2 = (Not %eof(TNTBE01L)   and   TBEatb = *blank
022730110411                       and TBEke2 <= '006            ');
022740110411         EndIf;
022750090302
022760110411         // -?Reperimento eventuali "PXA" per mittente?
022770090302         k_TBEcod = 'PXA';
022780090302         k_TBEke1 = %editc(DB0ccm : 'X');
022790100316         k_TBEke2 = '001            ';
022800090302         clear  k_TBElin;
022810090302         clear  k_TBEsif;
022820100316         setll  %kds(k05tntbe01 : 3)  TNTBE000;
022830110411         // -?Bisogna testare se nota ANNULLATA?
022840090302         //$TabPXA  = %equal(TNTBE01L);
022850090302         reade  %kds(k05tntbe01 : 2)  TNTBE000;
022860100316         $TabPXA  = (Not %eof(TNTBE01L)   and   TBEatb = *blank
022870100316                     and TBEke2 <= '006            ');
022880090302
022890090302       ENDSR;
022900090302      /end-free
022910030515      *
022920030515      *---------------------------------------------------------------*
022930050217      *?Imposta i campi di stampa con quelli della DS                ?*
022940030515      *---------------------------------------------------------------*
022950030515     c     PrepPRT       BEGSR
022960040223      *
022970040223      * Reperimento dati da FIAR5 / "GEN"
022980130315     c*** (già fatto:)   exsr      RepAR5gen
022990070212     *** *
023000070212     *** * Impostazione Data spedizione
023010070212     *** * (già fatto nella subr. "TipoLdV")
023020070319      *
023030100218      * CONTROLLO SE STAMPA SPEDIZIONE DOPO "LASCIATO AVVISO"
023040100218      * (?*-=»>   SE NON MESSA IN "FERMO DEPOSITO"   <«=-?):
023050081118      * (NON se richiesta stampa di "ricevuta" - 4 in A4)
023060110427      * per LNA (o capofila se LNA è in £6) con $GeoAtt = *ON =>
023070070717      * GEO attivato completamente
023080070319      * controllo se la spedizione ha già più tentativi di consegna e
023090070319      *   determino se in stato di "lasciato avviso"
023100070319      *
023110070319     c*** (già fatto:)   reset                   $LascAvv
023120070406if  1c                   if             DB0tbo    =  'A'
023130081117     c                             and  DB0fa4    <> '4'
023140070514     c                             and  wB0ntc    >  *zeros
023150110427     c                             and  $GeoAtt   =  *on
023160100218     c                             and  DB0ffd    =  *blank
023170070319     c                   exsr      CheckLascAvv
023180070319e   1c                   endif
023190030515      *
023200120530      * Impotazione dati BRT S.p.A.
023210120530     c*//                if        (DB0dft > *zero       and
023220120530     c*//                           DB0dft > §vpoBRTdta)  OR
023230120530     c*//                          (DB0dft = *zero       and
023240120530     c*//                           Wdsp   > §vpoBRTdta)
023250120530     c                   eval      TBAR   =  $BRT
023260120530     c*//                else
023270120530     c*//                eval      TBAR   =  $Bartolini
023280120530     c*//                endif
023290070307     c                   eval      TBA1   =  $Intest1
023300070307     c                   eval      TBA2   =  $Intest2
023310080707if  1c                   if            DB0nft <> *zeros
023320080707     c                             or  DB0ift <> *zeros
023330080627     c                   eval      TBA3   =  $Intest3
023340080707e   1c                   endif
023350070307     c                   eval      TBA4   =  $Intest4
023360070307     c                   eval      TBA5   =  $Intest5
023370070307     c*** NON sopra:     eval      TBA6   =  $Intest6
023380120530     c*//                eval      TBA7   =  $Intest7
023390120530     c                   eval      TBA7   =  $Intest7_BRT
023400030515      *
023410030515      * - Intestazione moduli
023420080919     c                   eval      Tcxd   =  c_CopiaXdest
023430130726     c*//                if        Not *in36     (NON ancora acceso)
023440130726if  1c                   if        DB0tbo <> '8'  or  DB0ris = 'M'
023450130726     c                   eval      Tcxd2  =  Tcxd
023460130726     c                   endif
023470091008      *
023480091008      /free
023490091008         select;
023500091008           // - Indicazione Copia LDV x cliente
023510091008           when  DB0ris = 'C';
023520091008             Pkco = 'C O P I A';
023530091008           // - Altre indicazioni: R=Ristampa, V=Variazione, ...
023540091008           other;
023550091008             Tkri = DB0ris;
023560091008         endsl;
023570091008      /end-free
023580030515      *
023590030515      * - Codice a barre: ANNO+LNP+NRS+NSP+CKD
023600030515      *   (chiamo il pgm TRUL28R per ricavarne il check digit)
023610030515     c                   reset                   TRUL28ds
023620030515     c                   z-add     DB0aas        BARcdeAAS
023630030515     c                   eval      BARcdeLNP = DB0lnp
023640030515     c                   eval      BARcdeNRS = DB0nrs
023650030515     c                   eval      BARcdeNSP = DB0nsp
023660030515     c                   movel     BarCodeDS     I28cod
023670030515     c                   call      'TRUL28R1'
023680030515     c                   parm                    TRUL28DS
023690030515if  2c                   if        O28err =  *blanks
023700030515     c                   movel     O28cod        Tbcd
023710030515e   2c                   endif
023720140915      *
023730140915      /free
023740140915
023750140915         // -?BRTcode?
023760140915         reset  TRUL28ds0;
023770140915         trul28ds0.i280cod = BarCodeDS;
023780140915         trul28r0 ( TRUL28ds0 );
023790140915         if  trul28ds0.o280err <> *on;
023800140917           //TbrtC = trul28ds0.o280cod;
023810140917           TbrtC = %subst( trul28ds0.o280cod :  1 : 5 ) + ' ' +
023820140917                   %subst( trul28ds0.o280cod :  6 : 5 ) + ' ' +
023830140917                   %subst( trul28ds0.o280cod : 11 : 5 ) + ' ' +
023840140917                   %subst( trul28ds0.o280cod : 16 : 4 );
023850140915         endif;
023860140915
023870140915      /end-free
023880030515      *
023890030515      * - Sigla operatore video
023900030515     c                   eval      Psop   =  DB0sop
023910080721      /free
023920080721
023930130306         // -?Flag "allega documento di trasporto"?
023940130418         //   IN FUTURO: se al 1° tentativo di consegna
023950130418         //              (DB0FL1 = "D" - v. pgm FNLRS9R)
023960080721         select;
023970081119
023980130306           // -?LdV con DDT allegato oltre il 1° tentativo di consegna?
023990081127           when  DB0bam =  'S'   and   wB0tmc <> *blank;
024000081127             evalr Pddt =  'DDT sul collo';
024010081119
024020130306           // -?LdV con DDT allegato (al 1° tentativo di consegna)?
024030080723           when  DB0bam =  'S';
024040080723             evalr Pddt =  'DDT';
024050081119
024060130306           // -?Esiste packing-list abbinata?
024070130306           when  $FIAR8 =  *on   and   $§PDF =  *off;
024080081013           //if  DB0fl1 <> 'D';
024090081013               evalr Pddt =  'PACKING-LIST';
024100081013           //else;
024110081013           //  evalr Pddt =  'PACKING-LIST GIA'' CONSEGNATA';
024120081013           //endif;
024130130306
024140130306           // -?LdV con PDF allegato?
024150130306           when  $FIAR8 =  *off  and   $§PDF =  *on;
024160130306             evalr Pddt =  'PACKING-LIST PDF';
024170130306
024180130306           // -?LdV con packing-list abbinata & PDF allegato?
024190130306           when  $FIAR8 =  *on   and   $§PDF =  *on;
024200130308             evalr Pddt =  'PACKING-LIST PDF';
024210081119
024220130306           // -?LdV senza PDF allegato & senza packing-list abbinata?
024230080721           other;
024240080721             clear Pddt;
024250081119
024260080721         endsl;
024270080721
024280080721      /end-free
024290030515      *
024300030515      * - Mittente
024310030515     c                   if        DB0ccm <> *zeros
024320030515     c                   move      DB0ccm        Pccm
024330030515     c                   endif
024340030515     c                   eval      Prsm   =  DB0rsm
024350030515     c                   eval      Pinm   =  DB0inm
024360030515     c                   eval      Pcam   =  DB0cam
024370030515     c                   eval      Plom   =  DB0lom
024380030515     c                   eval      Pprm   =  DB0prm
024390030515     c                   eval      Pnzm   =  DB0nzm
024400030515      *   Mittente originale
024410030515if  1c                   if        DB0rmo <> *blanks
024420030515     c                   eval      Pkmo   =  'MITT. ORIGINALE'
024430030515     c                   eval      Prmo   =  DB0rmo
024440030515e   1c                   endif
024450030515      *
024460070308      * - Filiale di Arrivo
024470030515if  1c                   if        DB0ris <> 'M'
024480080721     c                   eval      Pkla   =  'FILIALE'
024490080701     c                   eval      Plna1  =  %editc(DB0lna : 'X')
024500080722     c                                    +  ' '
024510080702     c                                    +  DB0dla
024520080722     c                                    +  ' - '
024530080722     c                                    +  DB0inl
024540140129     c*//                                 +  ' Tel.'
024550140129     c                                    +  '     '
024560080722     c                                    +  %trimr(DB0tel)
024570080722     c                   eval      Plna2  =  %editc(DB0cal : 'X')
024580080722     c                                    +  ' '
024590080722     c                                    +  DB0lol
024600080722     c                                    +  ' '
024610080722     c                                    +  DB0prl
024620140129     c*//                                 +  '                  '
024630140129     c*//                                 +  ' Fax '
024640140129     c*//                                 +  %trimr(DB0fax)
024650140129     c                   if        DB0fax <> *blanks
024660140129     c     c_Mai:c_Min   xlate     DB0fax        DB0fax
024670140129     c                   eval      Plna2  =  %trimr(Plna2)
024680140129     c                                    +  '          '
024690140129     c*//*                                +  ' E-mail: '
024700140129     c                                    +  '         '
024710140129     c                                    +  %trimr(DB0fax) + '@brt.it'
024720140129     c                   endif
024730030515e   1c                   endif
024740030515      *
024750030515      * - Destinatario
024760030515if  1c                   if        DB0ccd <> *zeros
024770030515     c                   move      DB0ccd        Pccd
024780030515e   1c                   endif
024790030515     c                   eval      Prsd   =  DB0rsd
024800030515     c                   eval      Prse   =  DB0rd2
024810030515     c                   eval      Pind   =  DB0ind
024820030515     c                   eval      Pcad   =  DB0cad
024830030515     c                   eval      Plod   =  DB0lod
024840030515     c                   eval      Pprd   =  DB0prd
024850030515     c                   eval      Pnzd   =  DB0nzd
024860030515      *
024870030515      * - Riferimenti mittente
024880030515     c                   eval      Prmn   =  DB0rmn
024890030515     c                   eval      Prma   =  DB0rma
024900061107      * se bolla DPD recupero il riferimento da FIAR4 rcd "I"
024910061107     c*** (già fatto:)   clear                   dsBL4I
024920060626     c                   if        *in71
024930061103     c                   exsr      RepAR4_I
024940060626     c                   endif
024950030515      *
024960030515      * - Nominativo & telefono
024970031124      *   (da FIAR5 / "GEN")
024980040607     c*** (già fatto:)   exsr      RepAR5gen
024990031124     c                   eval      Pnom   =  §AR5ref
025000031124     c                   eval      Ptel   =  §AR5teld
025010030515      *
025020030515      * - Spedizione
025030030515      *   (numero: non se Rimesso al Mittente)
025040030515if  1c                   if        DB0ris <> 'M'
025050030515     c                   eval      Plnp   =  DB0lnp
025060030515     c                   eval      Pnrs   =  DB0nrs
025070030515     c                   eval      Pnsp   =  DB0nsp
025080030515e   1c                   endif
025090030515      * . . Data spedizione
025100031124     c     *iso          movel     Wdsp          Data_EUR
025110130418     c     *eur          movel     Data_EUR      Pdsp
025120140922     c     *dmy          movel     Data_EUR      Pdsp6
025130030515      *
025140030515      * - Colli: numero, peso, volume, flag tipo-volume
025150030515     c                   eval      Pncl   =  DB0ncl
025160030515     c                   eval      Ppkf   =  DB0pkf
025170030515     c                   eval      Pvlf   =  DB0vlf
025180030515if  1c                   if            DB0fvf <> *blanks
025190030515     c                             and DB0ris <> 'M'
025200030717     ***c***                eval      Pfvf   =  '-' + DB0fvf + '-'
025210030717     c                   move      DB0fvf        Pfvf
025220030515e   1c                   endif
025230030515      *
025240030515      * - Natura merce
025250030515     c                   eval      Pnat   =  DB0nas
025260030515      *
025270030515      * - Servizio
025280100513      /free
025290100513         if  %subst(DB0tb1:1:1) = 'A'   or   DB0tb2 <> *blanks;
025300100513           exsr  sr_CheckORM_PrevvMail;
025310100513         endif;
025320100513      /end-free
025330040607      * . . Descrizione LDV
025340100513     c                   if        Not  $PreavvORM
025350030515     c                   eval      Pds1   =  DB0dcb
025360100513     c                   endif
025370030515      * . . Descrizione tipo servizio
025380030515if  1c                   if        DB0fst =  'S'
025390030515     c                   eval      Pds2   =  DB0dts
025400030515e   1c                   endif
025410030515      * . . Reso / Franco frontiera
025420030515sel 1c                   select
025430030515w   1c                   when      DB0fbr =  'S'
025440030515     c                   clear                   w008a
025450030515     c                   eval      w008a  =  '   RESO '
025460030515     c                   move      w008a         Pds2
025470030515w   1c                   when      DB0fbr =  'F'
025480030515     c                   clear                   w015a
025490030515     c                   eval      w015a  =  '   FR.FRONTIERA'
025500030515     c                   move      w015a         Pds2
025510030515e   1c                   endsl
025520030515      *
025530030515      * - Zona consegna
025540030515     c                   eval      Pznc   =  DB0znc
025550030515      *
025560030515      * - Flag "*" (gestito a parte)
025570040223      *
025580040223      * - Flag "V" = colli di valore
025590040223if  1c                   if        §AR5bva   <> *blank
025600040223     c                   eval      Pbva   =  'V'
025610040223e   1c                   endif
025620060921      *
025630070212      * - Codice Giro o Numero Distinta (se tipo bolla "ARRIVO")
025640071015if  1c                   IF              DB0tbo = 'A'
025650071015     c                             and  wGeoCon = 'S'
025660070212sel 2c                   Select
025670070212      * · - Stampa LdV in arrivo con codice giro (parametro passato)
025680071015w   2c                   When      DB0fl1     =  'A'
025690070215if  3c                   if        §DB0tgi    =  'M'
025700101028     c                   eval      Pndc   =  'g:' + §DB0cgi + 'm'
025710070215x   3c                   else
025720101028     c                   eval      Pndc   =  'g:' + §DB0cgi
025730070215e   3c                   endif
025740070212      * · - Stampa LdV con numero distinta consegna
025750071015w   2c                   When           wB0ndc  > *zeros
025760110427     c                             and  $GeoAtt =  *on
025770101028     c                   eval      Pndc   =  'd:'
025780070514     c                                    +  %trim(%editc(wB0ndc:'Z'))
025790101028if  3c                   if        %len(Pndc)  >= %len(%trimr(Pndc))
025800071016     c                                          + 3 + %len(Wpdc4a)
025810101028     c                   eval      Pndc   =  %trimr(Pndc)
025820071022     c                                    +  ' a:'
025830071022     c                                    +  Wpdc4a
025840071016e   3c                   endif
025850070212      * · - Stampa LdV con codice giro (da reperire in FIARG00F)
025860070213w   2c                   Other
025870070212if  3c                   if        NOT %open(FIARG01L)
025880070212     c                   open      FIARG01L
025890070212e   3c                   endif
025900070215     c                   clear                   ARGcgi
025910070215     c                   clear                   ARGtgi
025920070212     c     KLDV          chain     FIARG000
025930070215if  3c                   if        ARGtgi =  'M'
025940101028     c                   eval      Pndc   =  'g:' + ARGcgi + 'm'
025950070215x   3c                   else
025960101028     c                   eval      Pndc   =  'g:' + ARGcgi
025970070215e   3c                   endif
025980070215e   2c                   EndSl
025990070212e   1c                   ENDIF
026000030515      *
026010030515      * - Fermo deposito
026020030515if  1c                   if        DB0ffd <> *blanks
026030060921     c                   eval      Pffd   =  'FERMO DEPOSITO '
026040030515e   1c                   endif
026050030515      *
026060070308      * - Segnacolli: filiale, serie, dal / al
026070030515if  1c                   if        DB0fls <> *zeros
026080030515     c                   move      DB0fls        Ppos
026090030515e   1c                   endif
026100040607      * - - se bolla con mamma considero la serie della mamma
026110070515if  1c                   if        $Mamma =  *on
026120031015     c                   eval      Psrs   =  wB0nrs
026130031015x   1c                   else
026140030515     c                   eval      Psrs   =  DB0nrs
026150031015e   1c                   endif
026160030515      * - Se colli non sequenziali: stampo "NON SEQUENZIALI" + Elenco
026170030515      *   Se collo cumulativo ....:   "     SCD "X" Num.Colli
026180030515      *   Se colli sequenziali ...:   "     SCD "/" SCA
026190030515      *   Se collo singolo .......:   "     SCD
026200031015sel 1c                   select
026210030515      * - - non sequenziali
026220030515w   1c                   when      DB0fns =  'S'
026230050513if  2c                   if           *in69      = *on
026240050513     c                             or $A4_x_ScNS = *off
026250030715      *     caricamento delle "Note e Particolarità" per consentirne
026260030715      *     l'aggiunta come "Elenco segnacolli non sequenziali"
026270031015if  3c                   if        $Partic = *off
026280030515     c                   exsr      SchPARTIC
026290031015e   3c                   endif
026300030515     c                   exsr      ListaScNS
026310050511x   2c                   else
026320050511if  3c                   if        DB0ncl <= 2
026330050511     c                   exsr      ListaScNSd
026340081007x   3c                   else
026350081007     c                   eval      Pnsx   =  'NON SEQUENZIALI'
026360050511e   3c                   endif
026370030515e   2c                   endif
026380030515      * - - cumulativo
026390050114w   1c**!!!              when      DB0xco =  'X'
026400081010     c**!!!              eval      Pnsx   =  %editc(DB0ncd : 'Z')
026410081010     c**!!!                               +  'X'
026420081010     c**!!!                               +  %editc(DB0ncl : 'Z')
026430030515      * - - singolo o sequenziali
026440030515      *     (non se rimesso al mittente)
026450030515w   1c                   when      DB0ris <> 'M'
026460080922     c                   eval      Pnsx   =  %editc(DB0ncd : 'Z')
026470030515if  2c                   if            DB0nca <> *zeros
026480030515     c                             and DB0nca <> DB0ncd
026490080922     c                   eval      Pnsx   =  %trimr(Pnsx) + '-'
026500080922     c                                    +  %editc(DB0nca : 'Z')
026510030515e   2c                   endif
026520030515e   1c                   endsl
026530030515      *
026540030716      * - Note (1 e 2) e Particolarità (0, 3, 4, 5 e 6)
026550030710      *   con eventuali segnacolli non sequenziali (max 4 x riga)
026560030515if  1c                   if        $Partic = *off
026570030515     c                   exsr      SchPARTIC
026580030515e   1c                   endif
026590030716if  1c                   if           $NP(1) <> *blanks
026600030716     c                             or $NP(2) <> *blanks
026610030716     c                             or $NP(3) <> *blanks
026620030716     c                             or $NP(4) <> *blanks
026630030716     c                             or $NP(5) <> *blanks
026640030716     c                             or $NP(6) <> *blanks
026650030716     c                   eval      Pknp   =  'NOTE  E  PARTICOLARITÀ'
026660030710     c                   eval      Pnp1   =  $NP(1)
026670030710     c                   eval      Pnp2   =  $NP(2)
026680030710     c                   eval      Pnp3   =  $NP(3)
026690030710     c                   eval      Pnp4   =  $NP(4)
026700030710     c                   eval      Pnp5   =  $NP(5)
026710030710     c                   eval      Pnp6   =  $NP(6)
026720030716e   1c                   endif
026730030515      *
026740080401      * - Privato / Appuntamento / Uffici / Consegna Garantita
026750080401      *   (servizio consegna garantita x cliente Wuerth)
026760170413if  1c*//                if        DB0gva =  'P '
026770170413     c*//                evalr     Ppad = 'PRIVATO'
026780170413e   1c*//                endif
026790041012if  1c                   if        DB0gva =  'U '
026800080723     c                   evalr     Ppad = 'CONSEGNA AD UFFICI'
026810041012e   1c                   endif
026820081010if  1c*** NO !!!         if           DB0tc1 =  'A'
026830081010     c*** NO !!!                   or DB0tc2 =  'A'
026840081010     c*** NO !!!         move      'APPUNTAMENTO'Ppad
026850081010e   1c*** NO !!!         endif
026860080401if  1c                   if             DB0nrs = 40
026870090518     c                             and  DB0tsp = 'E'
026880080401     c                             and (DB0lnp = 043
026890110429     c                              or  DB0lnp = 277
026900080401     c                              or  DB0lnp = 063)
026910090217     c*****              evalr     Ppad = 'CONSEGNA GARANTITA'
026920090227     c                   eval      Pds2 = 'CONSEGNA ESPRESSA +
026930090227     c                                     WÜRTH'
026940080401e   1c                   endif
026950030515      *
026960030515      * - Data e ora consegna richiesta
026970030515      *   e Flag "*" > x Data e/o Ora consegna richiesta
026980091116      *   (NO "Prima del" per il cliente TTL)
026990091116if  0c                   if        DB0ccm <> c_CliTTL  or
027000091116     c                             DB0dcr =  *zeros    or  DB0tcr <> 'P'
027010030515if  1c                   if        DB0dcr <> *zeros
027020030515     c                   eval      Past   =  '*'
027030030515     c     *iso          movel     DB0dcr        Data_EUR
027040030515     c     *eur          movel     Data_EUR      Pdcr
027050030515e   1c                   endif
027060030515if  1c                   if        DB0hcr <> *zeros
027070030515     c                   eval      Past   =  '*'
027080030515     c                   eval      *in18  =  *on
027090030515     c                   eval      Phcr   =  DB0hcr
027100030515e   1c                   endif
027110091116e   0c                   endif
027120030515      *
027130030515      * - Consegne particolari
027140030515     c                   eval      Pcp1   =  DB0dc1
027150030515     c                   eval      Pcp2   =  DB0dc2
027160030515     *** *
027170030515     *** * - Valore merce dichiarato e relativa divisa => NO
027180030515     ***c***                eval      Pvmd   =  DB0vmd
027190030515     ***c***                eval      Pvad   =  DB0vad
027200030515     *** *
027210030515     *** * - Importo da assicurare e relativa divisa   => NO
027220030515     ***c***                eval      Pvas   =  DB0vas
027230030515     ***c***                eval      Pias   =  DB0ias
027240030515     *** *
027250030515     *** * - Quantità da fatturare                     => NO
027260030515     ***c***                eval      Pqft   =  DB0qft
027270030515      *
027280030515      * - Imponibile / IVA / Esenzione / Totale fattura e divisa
027290030515      *   (importi "arrotondati" a 2 decimali in stampa)
027300030515if  1c                   if        DB0ift <> *zeros
027310030515     c                   eval      Pkim   = 'IMPONIBILE'
027320130218     c*//                   eval(h)   Pimp   =  DB0imv
027330130218     c                   eval(h)   w0112  =  DB0imv
027340130218     c                   evalr     Pimp   =  %editc(w0112 : 'K')
027350030515sel 2c                   select
027360030515w   2c                   when      DB0iva <> *zeros
027370030515     c                   eval      Pkie   = 'I.V.A.'
027380030624     c                   eval(h)   w0112  =  DB0iva
027390130218     c*//                   eval      Piva   =  '     ' +
027400130218     c*//                                       %editc(w0112 : 'K')
027410130218     c                   evalr     Piva   =  %editc(w0112 : 'K')
027420030515w   2c                   when         DB0cei <> *blanks
027430030515     c                             or DB0dce <> *blanks
027440030912     c                   eval      Pkie   = 'ESENZIONE'
027450030912     c                   eval      Piva   =  DB0dce
027460030515e   2c                   endsl
027470030515     c                   eval      Pktf   = 'TOTALE FATTURA'
027480130218     c*//                   eval(h)   Pift   =  DB0ift
027490130218     c                   eval(h)   w0112  =  DB0ift
027500130218     c                   evalr     Pift   =  %editc(w0112 : 'K')
027510030515if  2c                   if        DB0div <> §GEDcn
027520030515     c                   eval      Pdiv   =  DB0div
027530030515e   2c                   endif
027540030515e   1c                   endif
027550030515      *
027560030515      * - Contrassegno e relativa divisa
027570030515if  1c                   IF        DB0cas <> *zeros
027580030625      *
027590030515     c                   eval(h)   Pcas   =  DB0cas
027600030515if  2c                   if        DB0vca <> §GEDcn
027610030515     c                   eval      Pdca   =  DB0vca
027620030515e   2c                   endif
027630030515     c                   exsr      CtrCAs
027640030624      * - - Nuovo controllo abilitazione c/assegno
027650030624     c                   reset                   TRUL21DS
027660030624     c                   eval      I21cbo =  DB0cbo
027670030624     c                   eval      I21tsp =  DB0tsp
027680030624     c                   eval      I21lnp =  DB0lnp
027690030624     c                   eval      I21nzm =  DB0nzm
027700030624     c                   eval      I21lna =  DB0lna
027710030624     c                   eval      I21nzd =  DB0nzd
027720130826     c                   if        DB0ccm >  *zero
027730130826     c                   eval      I21ksc =  DB0ccm
027740130826     c                   else
027750130826     c                   eval      I21ksc =  (DB0lnp * 10000 ) + 8888
027760130826     c                   endif
027770030624     c                   eval      I21tic =  DB0tic
027780030624if  2c                   if        DB0vca <> §GEDcn
027790030624     c                   eval      I21imp =  Wcas
027800030624     c                   eval      I21div =  §GEDcn
027810030624x   2c                   else
027820030624     c                   eval      I21imp =  DB0cas
027830030624     c                   eval      I21div =  DB0vca
027840030624e   2c                   endif
027850030624     c                   eval      I21ute =  KNMUS
027860040607     c                   eval      I21pgm =  SDSpgm
027870030624     c                   call      'TRUL21R'
027880030624     c                   parm                    TRUL21ds
027890030624      * - - Carico la tabella "Tipo incasso C/Assegno" (1A)
027900030624     c                   exsr      Tab_1A
027910030624     c                   movel     §1Ades        DB0dti
027920030624if  2c                   if            §1Adeo <> *blanks
027930030624     c                             and O21fca <> *blanks
027940030624     c                   movel     §1Adeo        DB0dti
027950030624e   2c                   endif
027960100208      /free
027970100208           // - -?Reperimento intestatario c/assegno?
027980100322           if  §VPOcas <= wDate   and   DUTlpo  <> 'S';
027990100208             reset  trulINTds;
028000100208             iINTtic  = DB0tic;
028010100208             iINTrscM = DB0rsm;
028020100208             iINTrscO = DB0rmo;
028030100208             iINTcdi  = §AR5cdi;
028040100208             //if  §AR5cdi = *blank;
028050100208             //  iINTaas = DB0aas;
028060100208             //  iINTlnp = DB0lnp;
028070100208             //  iINTnrs = DB0nrs;
028080100208             //  iINTnsp = DB0nsp;
028090100208             //endif;
028100100208             trulINTr ( TRULINTds );
028110100322           endif;
028120100208      /end-free
028130030624      * - - Imposto i campi di stampa
028140030624     c                   eval      Ptic   =  DB0tic + ' ' + DB0dti
028150091119      /free
028160100208           // - - -?Intestatario assegno?
028170100322           if  DB0cas  <> *zero   and
028180100322              (§VPOcas <= wDate   and   DUTlpo  <> 'S')   and
028190100208               oINTerr = *blank;
028200100208             Pkic = 'INTESTARE A:';
028210100208             Pica = oINTrsci;
028220100208           endif;
028230091119      /end-free
028240030625      *
028250030515e   1c                   ENDIF
028260030515      *
028270030515      * - Totale da esigere al ritiro o alla consegna e relativa divisa
028280030515if  1c                   IF        DB0ift >  *zeros
028290030625      *
028300030515if  2c                   if            DB0vca <> §GEDcn
028310030515     c                             and DB0vca <> *blanks
028320030515     c                   eval      Pdie   =  DB0div
028330030515e   2c                   endif
028340030515      *   Controllo se C/Assegno da sommare al totale da esigere o no:
028350030515      *   se Franco-Prepagato il C/Assegno non si somma mai;
028360030515      *   se c'è il totale fattura: metto anche il il totale da esigere.
028370030515      *   (importi "arrotondati" a 2 decimali in stampa)
028380030515sel 2c                   Select
028390030515      *   Non ho C/Assegno o è Franco Prepagato
028400071008w   2c                   When          DB0cas  = *zeros
028410071008     c                             or  DB0tbo  = '8'
028420080701     c                   eval(h)   Pies   =  DB0ift
028430030625     c                   clear                   Pie1
028440030625     c                   clear                   Pdi1
028450030515      *   C/Assegno sommabile
028460030515w   2c                   When      DB0ste =  'S'    and $CAeur = *on
028470030515sel 3c                   select
028480030515      *   - c/assegno in EURO
028490030515w   3c                   when      DB0div =  §GEDcn and DB0vca =  §GEDcn
028500080701     c                   eval(h)   Pies   =  DB0ift + DB0cas
028510030624     c                   clear                   Pie1
028520030624     c                   clear                   Pdi1
028530030515      *   - c/assegno in DIVISA
028540030515w   3c                   when          DB0div =  §GEDcn
028550030515     c                             and DB0vca <> §GEDcn
028560080701     c                   eval(h)   Pies   =  DB0ift + Wcas
028570030624     c                   clear                   Pie1
028580030624     c                   clear                   Pdi1
028590030515e   3c                   endsl
028600030515      *   C/Assegno non sommabile
028610030515x   2c                   Other
028620080701     c                   eval(h)   Pie1   =  DB0cas
028630080701     c                   eval(h)   Pies   =  DB0ift
028640030515e   2c                   Endsl
028650030709      *
028660030709e   1c                   ENDIF
028670030515      *
028680030515      * - Numero e data fattura
028690030515if  1c                   if        DB0nft <> *zeros
028700030515     c                   eval      Pkft   =  'FATTURA   '
028710030515     c                   eval      Pnft   =  DB0nft
028720130124     c                   eval      Pfiv   = '(' + %editc(wB0fiv:'X') +
028730130124     c                                      ')'
028740030515if  2c                   if        DB0dft >  *zeros
028750030515     c                   eval      Pkdf   =  'DEL'
028760030515     c     *iso          movel     DB0dft        Data_EUR
028770130418     c     *dmy          movel     Data_EUR      Pdft
028780130418     c*//  *eur          movel     Data_EUR      Pdft
028790030515e   2c                   endif
028800030515e   1c                   endif
028810030515      *
028820030515      * Memorizzo il codice tariffa come numero di copie da stampare
028830030515      *   (per Rimesso Mittente - non per DPD o FEDEX)
028840030515if  1c                   if        DB0ris = 'M'  and  *in70
028850030515     c                   eval      Wctr   =  DB0ctr
028860030515e   1c                   endif
028870030515      *
028880030515     c                   ENDSR
028890030515      *
028900030515      *---------------------------------------------------------------*
028910050217      *?Inschieramento delle Note e delle Particolarità              ?*
028920030515      *---------------------------------------------------------------*
028930030515     c     SchPARTIC     BEGSR
028940030515      *
028950030515     c*** (già fatto:)   reset                   $Partic
028960030710     c*** (già fatto:)   reset                   $NoDesPartic
028970030515     c*** (già fatto:)   clear                   $NP
028980030717     c*** (già fatto:)   clear                   W_CoOr
028990030515     c                   clear                   XX
029000030710      *
029010030710      * Imposto eventuali note per prime
029020030710if  1c                   if        DB0nt1 <> *blanks
029030030710     c                   add       1             xx
029040030710     c                   movel     DB0nt1        $NP(xx)
029050030710e   1c                   endif
029060030710if  1c                   if        DB0nt2 <> *blanks
029070030710     c                   add       1             xx
029080030710     c                   movel     DB0nt2        $NP(xx)
029090030710e   1c                   endif
029100030515      *
029110030515      * Le particolarità possono essere lunghe fino a 45 char.
029120030515      *   Se i primi 30 (della 1ª riga a disposizione) non dovessero
029130030515      *   bastare, il resto lo si mette nella riga successiva.
029140030710      *   Se, invece, non risultano decodificate, si mettono solo i
029150030710      *   codici sulla stessa riga.
029160030710      *   Se uno solo risulta decodificato, esso va su una riga a
029170030710      *   parte (vedi flag "$NoDesPartic": *off = serve riga nuova
029180030710      *                                    *on  = ok riga procedente)
029190030515      *
029200030515      * . . Particolarità Giacenza
029210030515if  1c                   IF        DB0gga <> *blanks
029220030515     c                   movel     'Q'           WTpPar
029230030515     c                   movel     DB0gga        WCdPar
029240030515     c                   exsr      TABxPAR
029250030710if  2c                   if            TBLuni       = *blanks
029260030710     c                             and $NoDesPartic = *on
029270030710     c                   eval      $NP(xx) = %trim($NP(xx))
029280030710     c                                     + '   ' + WCdPar
029290030710x   2c                   else
029300030710if  3c                   if        TBLuni       = *blanks
029310030710     c                   eval      $NoDesPartic = *on
029320030710x   3c                   else
029330030710     c                   eval      $NoDesPartic = *off
029340030710e   3c                   endif
029350030515     c                   add       1             xx
029360100513     c                   movel     w053a         $NP(xx)
029370100513if  3c                   if        %subst(w053a : %len($NP(xx))+1)
029380030515     c                              <> *blanks
029390030515     c                   add       1             xx
029400030515     c                   eval      $NP(xx) = '   ' +
029410100513     c                             %trim(%subst(w053a:%len($NP(xx))+1))
029420030710e   3c                   endif
029430030515e   2c                   endif
029440030515e   1c                   ENDIF
029450030515      *
029460030515      * . . Particolarità Consegna
029470030515if  1c                   IF        DB0gma <> *blanks
029480030515     c                   movel     'R'           WTpPar
029490030515     c                   movel     DB0gma        WCdPar
029500030515     c                   exsr      TABxPAR
029510030710if  2c                   if            TBLuni       = *blanks
029520030710     c                             and $NoDesPartic = *on
029530030710     c                   eval      $NP(xx) = %trim($NP(xx))
029540030710     c                                     + '   ' + WCdPar
029550030710x   2c                   else
029560030710if  3c                   if        TBLuni       = *blanks
029570030710     c                   eval      $NoDesPartic = *on
029580030710x   3c                   else
029590030710     c                   eval      $NoDesPartic = *off
029600030710e   3c                   endif
029610030515     c                   add       1             xx
029620100513     c                   movel     w053a         $NP(xx)
029630100513if  3c                   if        %subst(w053a : %len($NP(xx)) + 1)
029640030515     c                              <> *blanks
029650030515     c                   add       1             xx
029660030515     c                   eval      $NP(xx) = '   ' +
029670100513     c                             %trim(%subst(w053a:%len($NP(xx))+1))
029680030710e   3c                   endif
029690030515e   2c                   endif
029700030515e   1c                   ENDIF
029710030515      *
029720030515      * . . Particolarità Varie
029730030717      *     - questa descrizione va messa nell'apposito campo PPAD
029740041015      *       se è "PRIVATO" idem se è "CONSEGNA AD UFFICI"
029750150224     *** *     - se 'B' (bancali da rendere al mittente) comparirà un
029760150224     *** *       apposito box
029770150224     *** *     - se 'O' (oltre 20 colli => bancali da riportare in mag.)
029780150224     *** *       comparirà un apposito box, un'apposita nota e l'indica-
029790150224     *** *       zione nella copia per il destinatario.
029800030717      *     ...Potrebbero anche non esserci più elementi di schiera
029810030717      *     disponibili [xx >= %elem($NP)] !
029820030709if  1c                   IF            DB0gva <> *blanks
029830150224     c*//                          and DB0gva <> 'B '
029840150224     c*//                          and DB0gva <> 'O '
029850030709     c                             and DB0gva <> 'P '
029860041015     c                             and DB0gva <> 'U '
029870030515     c                   movel     'S'           WTpPar
029880030515     c                   movel     DB0gva        WCdPar
029890030515     c                   exsr      TABxPAR
029900030710if  2c                   if            TBLuni       = *blanks
029910030710     c                             and $NoDesPartic = *on
029920030710     c                   eval      $NP(xx) = %trim($NP(xx))
029930030710     c                                     + '   ' + WCdPar
029940030710x   2c                   else
029950030710if  3c                   if        TBLuni       = *blanks
029960030710     c                   eval      $NoDesPartic = *on
029970030710x   3c                   else
029980030710     c                   eval      $NoDesPartic = *off
029990030710e   3c                   endif
030000030717if  3c                   if            xx < %elem($NP)
030010030709     c                   add       1             xx
030020100513     c                   movel     w053a         $NP(xx)
030030100513if  4c                   if        %subst(w053a : %len($NP(xx)) + 1)
030040030709     c                              <> *blanks
030050030717     c                             and xx < %elem($NP)
030060030709     c                   add       1             xx
030070030709     c                   eval      $NP(xx) = '   ' +
030080100513     c                             %trim(%subst(w053a:%len($NP(xx))+1))
030090030717e   4c                   endif
030100030717e   3c                   endif
030110030709e   2c                   endif
030120030515e   1c                   ENDIF
030130030717      *
030140030717      * Imposto il numero originale di colli componenti la spedizione
030150030717      * e se ho ancora spazio lo memorizzo in "Note e Particolarità"
030160030717if  1c                   if        %subst(DB0gva:1:1) = 'O'
030170030717     c                   exsr      RepAR5bnb
030180030717if  2c                   if        W_CoOr  <> *blanks
030190030717     c                             and xx  <  %elem($NP)
030200030717     c                   add       1             xx
030210030717     c                   eval      $NP(xx) =  W_CoOr
030220030717e   2c                   endif
030230030717e   1c                   endif
030240030515      *
030250030515      * Imposto il flag per memorizzare la già avvenuta memorizzazione
030260030715      *   delle "Note e Particolarità"
030270030515     c                   eval      $Partic = *on
030280030515      *
030290030515     c                   ENDSR
030300070319      *
030310070319      *---------------------------------------------------------------*
030320070319      *?Inschieramento delle Consegne Parziali precedenti            ?*
030330070319      *---------------------------------------------------------------*
030340070319     c     SchConsParz   BEGSR
030350070319      *
030360070319     c*** (già fatto:)   reset                   $CP
030370070319     c*** (già fatto:)   reset                   $CPdcm
030380070319     c                   clear                   XX
030390070319      *
030400070319if  2c                   if        NOT %open(FNART01L)
030410070319     c                   open      FNART01L
030420070319e   2c                   endif
030430070319      *
030440070514if  2c                   if        $Mamma = *off
030450070319     c     KLDV          setll     FNART000
030460070319     c     KLDV          reade     FNART000
030470070514x   2c                   else
030480070514     c     KLBL          setll     FNART000
030490070514     c     KLBL          reade     FNART000
030500070514e   2c                   endif
030510070319      *
030520070319do  1c                   DOW       NOT  %eof(FNART01L)
030530070319      *
030540070319if  2c                   If             ARTatb = *blanks
030550070319     c                             and  ARTdcm > *zeros
030560070319      *
030570070319     c                   eval      *in31  = *off
030580070319     c                   eval      xx     = 1
030590070319     c     ARTdcm        lookup    $CPdcm(xx)                             31
030600070319if  3c                   if        NOT  *in31
030610070319     c                   eval      xx     = 1
030620070319     c     *zeros        lookup    $CPdcm(xx)                             31
030630070319if  4c                   if        *in31
030640140304     c*//                eval      $CP(xx)     = *zeros
030650140304     c                   clear                   Elem_CP
030660140304     c                   movel     Elem_CP       $CP(xx)
030670070319e   4c                   endif
030680070319e   3c                   endif
030690070319if  3c                   if        *in31
030700070319     c                   eval      $CPdcm(xx)  = ARTdcm
030710070319     c                   movel     $CP(xx)       Elem_CP
030720070319     c                   eval      WCPdcm      = ARTdcm
030730070319     c                   eval      WCPncl      = WCPncl + 1
030740070319     c                   movel     Elem_CP       $CP(xx)
030750070319e   3c                   endif
030760070319      *
030770070319e   2c                   EndIf
030780070319      *
030790070514if  2c                   if        $Mamma = *off
030800070319     c     KLDV          reade     FNART000
030810070514x   2c                   else
030820070514     c     KLBL          reade     FNART000
030830070514e   2c                   endif
030840070319      *
030850070319e   1c                   ENDDO
030860070319      *
030870140513if  0c                   if        xx     > *zero
030880140513      * Reperimento nominativi firmatari
030890140513     c                   exsr      sr_Firmatario
030900070319      * Ordinamento schiera per data
030910070319if  1c                   if        xx     > 1
030920070319     c                   sorta                   $CP
030930070319e   1c                   endif
030940140513e   0c                   endif
030950070319      *
030960070319     c                   ENDSR
030970140303      *
030980140303      /free
030990140303
031000140303       //--------------------------------------------------------------
031010140303       //?Reperimento nome firmatario                                  ?
031020140303       //--------------------------------------------------------------
031030140303       BEGSR  sr_Firmatario;
031040140303
031050140303if  1    if  NOT  %open(FNEVB22L);
031060140303           open  FNEVB22L;
031070140303e   1    endif;
031080140303
031090140303         clear  keyFNEVB22;
031100140303         k_EVB2aas = DB0aas;
031110140303         k_EVB2lnp = DB0lnp;
031120140303         k_EVB2nrs = DB0nrs;
031130140303         k_EVB2nsp = DB0nsp;
031140140303         k_EVB2cev = 'P  ';
031150140303         setll  %kds( keyFNEVB22 : 4 )  FNEVB200;
031160140303         reade  %kds( keyFNEVB22 : 4 )  FNEVB200;
031170140303         DoW  Not  %eof(FNEVB22L);
031180140303           if  EVB2cev = k_EVB2cev  and  EVB2trd = 'FIR';
031190140305             yy = %lookup( EVB2dev : $CPdcm );
031200140305             if  yy > *zero;
031210140305               Elem_CP = $CP(yy);
031220140303               if  WCPocm  <= EVB2hev;
031230140303                 WCPocm  = EVB2hev;
031240140303                 WCPfir  = EVB2dati;
031250140305                 $CP(yy) = Elem_CP;
031260140303               endif;
031270140303             endif;
031280140303           endif;
031290140304           reade  %kds( keyFNEVB22 : 4 )  FNEVB200;
031300140303         EndDo;
031310140303
031320140303       ENDSR;
031330140303
031340140303      /end-free
031350031124      *
031360031124      *---------------------------------------------------------------*
031370050217      *?Reperimento Nominativo e Telefono destinatario da FIAR5/GEN  ?*
031380031124      *---------------------------------------------------------------*
031390031124     c     RepAR5gen     BEGSR
031400031124      *
031410031124     c                   clear                   dAR5gen
031420031124      *
031430031124     c                   eval      AR5trd = 'GEN'
031440040301if  1c                   if        DUTlpo = 'S'
031450040531     c     K05AR501      chain     FIAR531C
031460031127x   1c                   else
031470031127     c     K05AR501      chain     FIAR5000
031480031127e   1c                   endif
031490031124      *
031500040301if  1c                   if           (DUTlpo =  'S'
031510040531     c                             and %found(FIAR531C)
031520040301     c                             OR  DUTlpo <> 'S'
031530040301     c                             and %found(FIAR501L))
031540040301     c                             and AR5atb =  *blanks
031550031124     c                   movel     AR5uni        dAR5gen
031560031124e   1c                   endif
031570031124      *
031580031124     c                   ENDSR
031590030717      *
031600030717      *---------------------------------------------------------------*
031610050217      *?Reperimento Numero Colli Originale da FIAR5/BNB              ?*
031620030717      *---------------------------------------------------------------*
031630030717     c     RepAR5bnb     BEGSR
031640030717      *
031650030717     c                   clear                   dAR5bnb
031660030717     c*** (già fatto:)   clear                   W_CoOr
031670030717      *
031680030717     c                   eval      AR5trd = 'BNB'
031690040301if  1c                   if        DUTlpo = 'S'
031700040531     c     K05AR501      chain     FIAR531C
031710040301x   1c                   else
031720030717     c     K05AR501      chain     FIAR5000
031730040301e   1c                   endif
031740030717      *
031750040301if  1c                   if           (DUTlpo =  'S'
031760040531     c                             and %found(FIAR531C)
031770040301     c                             OR  DUTlpo <> 'S'
031780040301     c                             and %found(FIAR501L))
031790040301     c                             and AR5atb =  *blanks
031800030717     c                   movel     AR5uni        dAR5bnb
031810030717if  2c                   if        §AR5bcor <> *zeros
031820030717     c                   eval      W_CoOr   =  C_CoOr
031830030717     c                                      +  %editc(§AR5bcor : 'K')
031840030717e   2c                   endif
031850030717e   1c                   endif
031860030717      *
031870030717     c                   ENDSR
031880030515      *
031890030515      *---------------------------------------------------------------*
031900051129      *?Decodifiche condizionate in Lettera di Vettura               ?*
031910030515      *---------------------------------------------------------------*
031920040607     c     DecodLDV      BEGSR
031930030515      *
031940030515      * Richiesta firma per ricevuta della merce senza riserve
031950030515      * (e NON  quella  per quietanza) => *in35 + not *in36
031960030515     c                   movea     '10'          *in(35)
031970101130      *
031980101130      /free
031990101130
032000101130         // -?Memorizzazione consegne parziali precedenti dal file?
032010101130         //  ?FNART00F - serve anche nella stampa delle "tagliatelle"?
032020140513         IF  $ConsParz;
032030140513
032040140513           // -?SE si stanno stampando "tagliatelle":?
032050140514           //  ?le consegne parziali vengono indicate nel Box1,?
032060140514           //  ?SE?ne sono state reperite?
032070140513           IF  DB0fa4 = '4'  and  $CP(1) < *all'9';
032080140513
032090101130             // -?Prima si totalizzano le quantità consegnate?
032100140513             clear  w0080;
032110140513             clear  w0140;
032120101130             For  xx = 1  To  %elem($CP);
032130101130               if  $CP(xx) < *all'9';
032140101130                 Elem_CP = $CP(xx);
032150101130                 monitor;
032160101130                   Data_Eur = %date( WCPdcm : *iso );
032170101130                 on-error;
032180101130                   Data_Eur = *loval;
032190101130                 endmon;
032200101130                 if       Data_Eur  > *loval  and
032210101130                     %dec(Data_Eur) > w0080;
032220101130                   w0080 = %dec(Data_Eur);
032230101130                 endif;
032240101130                 w0140 += WCPncl;
032250101130               endif;
032260101130             EndFor;
032270101130             // -?Poi si stampa un'unica indicazione?
032280101201             Data_DMY = %date( w0080 : *eur );
032290101201             Pcpp = 'Consegnati '
032300101201                  + %triml( %editc( w0140 : 'Z' ) )
032310101201                  + ' colli al '
032320101201                  + %triml( %editc( %dec( Data_DMY ) : 'Y' ) );
032330140513
032340140513           ENDIF;
032350140513
032360140513         ENDIF;
032370101130
032380101130      /end-free
032390070307      *
032400070307      * Intestazione moduli
032410070307      * - Se spedizione in stato "Lasciato Avviso"
032420070307      *   si stampano anche data/ora del 1° lasciato avviso
032430070307if  1c                   if        $LascAvv  = *on
032440131205     c                   z-add     wEVBdev       w0080
032450070315     c     *iso          test(d)                 w0080                  31
032460070315if  2c                   if        NOT *in31
032470071025     c     *iso          movel     w0080         Data_EUR
032480071025     c     *eur          movel     Data_EUR      w0080
032490070315x   2c                   else
032500070315     c                   clear                   w0080
032510070315e   2c                   endif
032520070307     c                   eval      Tla    =  'Lasciato avviso il '
032530070307     c                                    +  %editc(w0080 : 'Y')
032540070307     c                                    +  ' alle '
032550131205     c                                    +  %editw(wEVBhev : '  :  ')
032560070307e   1c                   endif
032570030709      *
032580100107     *** * Evidenziazione servizio "espresso"
032590100107     *** * o del servizio "Poste italiane"
032600100107     ***c                   eval      *in37 = (DB0tsp = 'E')
032610100107     ***c  n37              eval      *in37 = (§OGntw = 'PPT')
032620090611      *
032630090605      /free
032640100107
032650101130         // -?Evidenziazione servizio "espresso"?
032660101130         //           ?o del servizio "Poste Italiane"?
032670100107         *in37 = (DB0tsp = 'E'   or   DB0tsp = 'H'   or
032680100107                  §OGntw = 'PPT');
032690100107
032700101130         // -?Consegna al mattino (tempi consegna = 12 ore)?
032710101130         //  ?(NON per Würth)?
032720090615         if  §VPOrcm <= wDate and
032730090622             DUTlpo  <> 'S'   and
032740090615             DB0tsp   = 'E'   and
032750090615            (DB0nrs  <> 40    or  (DB0lnp <> 043   and
032760110429                                   DB0lnp <> 277   and
032770090605                                   DB0lnp <> 063));
032780090605           clear  TNSD99ds;
032790090605           if  DB0tbo = 'P'   or   DB0tbo = '8';
032800090605             D98tbo = 'P';
032810090605           else;
032820090605             D98tbo = 'A';
032830090605           endif;
032840090605           D98aas = DB0aas;
032850090605           D98lnp = DB0lnp;
032860090605           D98nrs = DB0nrs;
032870090605           D98nsp = DB0nsp;
032880090605           tnsd99r ( TNSD99ds );
032890120910           //*if  D98tci = 12;
032900120910           if  D98tcc = 12;
032910090605             Pdsm = 'M';
032920090605           endif;
032930090605         endif;
032940090605      /end-free
032950030515      *
032960030515      * Franco prepagato
032970030515      * (richiede firma per quietanza nella copia x destinatario)
032980030515if  1c                   if        DB0tbo =  '8'  and  DB0ris <> 'M'
032990030515     c                   movea     '01'          *in(35)
033000030515if  2c                   if        Pds1   =  *blanks
033010030515     c                   eval      Pds1   =  'FRANCO PREPAGATO        '
033020030515e   2c                   endif
033030120530     c*//                if        (DB0dft > *zero       and
033040120530     c*//                           DB0dft > §vpoBRTdta)  OR
033050120530     c*//                          (DB0dft = *zero       and
033060120530     c*//                           Wdsp   > §vpoBRTdta)
033070130418     c                   eval      Pbrt2  =  $BRT
033080120530     c*//                else
033090130418     c*//                eval      Pbrt2  =  $Bartolini
033100120530     c*//                endif
033110030515      * - Legge 450 (solo per prepagati)
033120030515if  2c                   if            DB0ias =  *zeros
033130030515     c                             and DB0vas =  *blanks
033140030515     c                             and DB0div =  §GEDcn
033150060301     c                             and §GElrp >  *zeros
033160030515     c*** v. IF sovrastante...     and DB0tbo =  '8'
033170060301if  3c                   if        Wdsp   >  20060228
033180110315     c                   z-add     §GElrp        w0112
033190080701     c                   eval      Prcv   =  'Art. 1696 c.c. mod. +
033200061102     c                                       dlgs 286/2005 '
033210060301     c                                    +  §GEDcn + ' '
033220060301     c                                    +  %trim(%editc(w0112 : '4'))
033230060301     c                                    +  ' al kg.'
033240060301x   3c                   else
033250110315     c                   z-add     6.20          w0112
033260080701     c                   eval      Prcv   =  'Legge 450/85 '
033270030515     c                                    +  §GEDcn + ' '
033280030624     c                                    +  %trim(%editc(w0112 : '4'))
033290030515     c                                    +  ' al kg.'
033300060301e   3c                   endif
033310030515e   2c                   endif
033320030515e   1c                   endif
033330031031      *
033340101125      /free
033350121017
033360121017         // -?Indicazione della Partiva IVA o del Cod. Fiscale?
033370121017         //  ?(se Porto Franco Prepagato?
033380121017         //    ?o Porto Assegnato con Fattura immediata)?
033390121017         //  ?- NON se mittente estero -?
033400121017             // -?con fattura?
033410121017         IF  (DB0nft <> *zero  or   DB0ift <> *zero)   AND     (
033420121017             // - -?P.F. prepagato?
033430121017             //    ?(con mittente italiano)?
033440121017             (DB0tbo =  '8'             and  DB0nzm  = *blank)  or
033450121017             // - -?P.A. (con fattura immediata)?
033460121017             //    ?(con destinatario italiano)?
033470121017             (%subst(DB0tb1:1:1) = 'A'  and  DB0nzd  = *blank)  or
033480121017             // - -?con 2ª bolla?
033490121017             //    ?(con destinatario italiano)?
033500121017             (DB0tb2 <> *blank          and  DB0tbo <> '8'
033510121029                                        and  DB0tbo <> 'P'
033520121017                                        and  DB0nzd  = *blank) );
033530121017           select;
033540121017             when  Not $No_PI  and  wB0piv <> *blank  and
033550121017                   // ·?Esiste Partita Iva NON FITTIZIA?
033560121026                   %scan('$$' : wB0piv) = *zero;
033570121017                   // ·?Non è un C.F. - lungo 16 (privato)?
033580121026                   //and  %len( %trim( wB0cfi ) ) < 16;
033590121017               Ppiv = 'Partita Iva: ' + wB0piv;
033600121017             when  Not $No_CF  and  wB0cfi <> *blank;
033610121017               Ppiv = 'Codice Fiscale: ' + wB0cfi;
033620121017           endsl;
033630130418           //select;
033640130418           //  // ·?Se P.F. Prepagato il C.F./P.I. è del Mittente?
033650130418           //  when  DB0tbo = '8';
033660130418           //    PpivM = Ppiv;
033670130418           //  // ·?Se P.A. con fattura immed. il C.F./P.I. è del Dest.?
033680130418           //  when  %subst(DB0tb1:1:1) = 'A';
033690130418           //    PpivD = Ppiv;
033700130418           //  // ·?Se 2ª bolla (DDT sì) è del Dest.?
033710130418           //  when  DB0tb2  <> *blanks  and  DB0tbo  <> 'P';
033720130418           //    PpivD = Ppiv;
033730130418           //endsl;
033740121017         ENDIF;
033750101125
033760101125         Select;
033770101125
033780101125           // -?Non utilizzare per la consegna?
033790101125           //  ?(stampa della sola copia per il vettore)?
033800101125           When  $No_x_Cons = *on;
033810101125             *in35 = *off;
033820101125             *in36 = *off;
033830101125             // -?costante da stampare se LDV recapito c/assegni?
033840101125             //  ?stampata dal pgm "Stampa DDT NO in arrivo"?
033850101129             Pnux  = 'NON UTILIZZARE PER LA CONSEGNA';
033860101125
033870101125           // -?Ritorno all'Incasso?
033880101125           When  $Rit_a_Inc = *on;
033890101125             *in40 = *on;
033900101125             Pnux  = 'RITORNO ALL''INCASSO';
033910101125
033920101125         EndSl;
033930101125
033940101125      /end-free
033950030910      *
033960030910      * Bollo assolto in modo virtuale
033970030910if  1c                   if        $BolloVirt =  *on
033980060719if  2c                   if        DB0dft <  20060801
033990080701     c                   eval      Pbav1  =  'Bollo '
034000080701     c                                    +  %trim(%editc(wBolloVirt:'4'))
034010080701     c                                    +  ' ' + DB0div
034020080701     c                                    +  ' assolto in modo virtuale '
034030080701     c                   eval      Pbav2  =  'Aut. Ag. Ent. BO 3 27/09/02 '
034040080701     c                                    +  'Prot. 31599'
034050060719x   2c                   else
034060080701     c                   eval      Pbav1  =  'Bollo '
034070080701     c                                    +  %trim(%editc(wBolloVirt:'4'))
034080080701     c                                    +  ' ' + DB0div
034090080701     c                                    +  ' assolto in modo virtuale '
034100080701     c                   eval      Pbav2  =  'Aut. Ag. Ent. MI 2 10/07/06 '
034110080701     c                                    +  'Prot. 45114'
034120060719e   2c                   endif
034130030910e   1c                   endif
034140050503      *
034150080605      * Costanti "PER RICEVUTA DELLA MERCE..."
034160080605     c                   eval      Pkrm1 = '(nome e cognome in stampa+
034170080605     c                                       tello)'
034180080605     c                   eval      Pkrm2 = '(firma)'
034190080605     c                   eval      Pkrm3 = '(data e ora)'
034200050504sel 1c                   select
034210050504      * - danon?stampare se
034220050504      *   · franco prepagato                (NOT *in35)
034230050504      *   · non utilizzare per la consegna  (NOT *in35)
034240101125      *   · ritorno all'incasso             (*in40)
034250050504      *   · bolla FedEx                     (*in72)
034260101125w   1c                   when      NOT *in35  or  *in40  or  *in72
034270050504     c*** (già fatto:)   clear                   Pkrm
034280080605     c                   clear                   Pkrm1
034290080605     c                   clear                   Pkrm2
034300080605     c                   clear                   Pkrm3
034310080922      * - costante "DPD"
034320120301w   1c                   when      *in71
034330080922     c                   eval      Pkrm   =  'PER RICEVUTA DELLA MERCE-
034340080922     c                              SENZA RISERVE'
034350120530      * - costante "standard" per "DDT sì"
034360090615w   1c                   when          §VPOrsr <= wDate
034370081119     c                             and  DB0bam =  'S'
034380071005     c                   eval      Pkrm   =  'PER RICEVUTA DELLA MERCE-
034390071005     c                              E DEL DOCUMENTO DI TRASPORTO SENZA-
034400071005     c                              RISERVE'
034410120530      * - costante "standard" per "DDT no" + Packing-List
034420120301w   1c                   when           DB0bam =  *blank
034430161012     c                             and ($FIAR8 =  *on
034440161012     c                              or  $§PDF  =  *on)
034450120301     c                   eval      Pkrm   =  'PER RICEVUTA DELLA MERCE-
034460120301     c                              E DEL DOCUMENTO DI TRASPORTO SENZA-
034470120301     c                              RISERVE'
034480120301      * - costante "standard" altrimenti
034490050504x   1c                   other
034500050504     c                   eval      Pkrm   =  'PER RICEVUTA DELLA MERCE-
034510050504     c                              SENZA RISERVE'
034520050504e   1c                   endsl
034530090513      /free
034540090611       Select;
034550101130         // -?costante per cliente?
034560101130         //  ?"Consorzio Lotterie Nazionali - Gratta & Vinci"?
034570140513         when  DB0ccm = c_CliCLN  or  DB0ccm = c_CliLN_2;
034580090528           exsr  sr_RepDdtCLN;
034590090706           Pkrm =  'LA FIRMA PER RICEVUTA DELLA MERCE VALE ANCHE PER +
034600090706                    IL DDT N° '
034610090527                + %editc (WddtCLN : 'Z')
034620090615                + ' DEL '
034630090527                + %editc (WdspCLN : 'Y')
034640101001                + ' CON RISERVA DI CONTROLLO COME DA CONTRATTO';
034650101001           if  Wdsp < 20101001;
034660101001             Pkrm = %trimr(Pkrm) + ' C.L.N.';
034670101001           else;
034680101001             Pkrm = %trimr(Pkrm) + ' L.N. S.R.L.';
034690101001           endif;
034700101130         // -?costante per cliente?
034710101130         //  ?"S G Soluzioni Logistiche"?
034720091117         when  DB0ccm = c_CliSG   or   DB0ccm = c_CliSG_2;
034730091117           if  DB0nas <> *blank   and   DB0nas <> c_NoContr;
034740091117             Pkrm = %trimr(Pkrm)
034750091117                  + '.   INCLUDE CONTRATTO - '
034760091117                  + %trim(DB0nas);
034770091117           endif;
034780091117           if  DB0rmo <> *blank;
034790091117             if  DB0nas <> *blank;
034800091117               Pkrm = %trimr(Pkrm)
034810091117                    + ' - MATR TV: '
034820091117                    + %trim(DB0rmo);
034830091117             else;
034840091117               Pkrm = %trimr(Pkrm)
034850091117                    + '.   MATR TV: '
034860091117                    + %trim(DB0rmo);
034870091117             endif;
034880091117           endif;
034890090611       EndSl;
034900090513      /end-free
034910030717      *
034920130418      *//* Indicazione degli "oltre 20 colli"
034930130418      *//* (MAI se Franco prepagato)
034940130418if  1c*//                   if            DB0tbo <> '8'
034950130418     c*//                             and *in36  =  *off
034960130418     c*//                             and W_CoOr <> *blanks
034970130418     c*//                   eval      Pbrt2  = W_CoOr
034980130418e   1c*//                   endif
034990030515      *
035000030515      * Verifico se imponibile/iva/totale/estremi fattura
035010030515     c                   eval      *in19  = ((DB0imv <> *zeros)  or
035020030515     c                                       (DB0iva <> *zeros)  or
035030030515     c                                       (DB0cei <> *blanks) or
035040030515     c                                       (DB0dce <> *blanks) or
035050030515     c                                       (DB0ift <> *zeros)  or
035060030515     c                                       (DB0nft <> *zeros)  or
035070031125     c                                       (DB0dft <> *zeros))
035080030515      *
035090030515      * Verifico se contrassegno
035100030515     c                   eval      *in20  =  (Pcas <> *zeros)
035110030515      *
035120030709      * Verifico se totale da esigere al ritiro o alla consegna
035130080701     c                   eval      *in21  =  (Pies <> *zeros)
035140030709      *
035150030710      * Intesto il 2° modulo diversamente per prepagato
035160030710      * (solo in partenza)
035170030710if  1c                   if        *in21  and  *in36
035180030710     c                             and   (DB0tbo = 'P'
035190030710     c                                or  DB0tbo = '8')
035200030710     c                   eval      Tcxd   =  'COPIA PER IL MITTENTE    '
035210130726     c                   clear                   Tcxd2
035220030710e   1c                   endif
035230030515      *
035240120530      * Verifico se codice a barre BRT da stampare
035250030515     c                   eval      *in50  =  (Tbcd <> *blanks)
035260040629      * Verifico se codice a barre O.R.M.    da stampare
035270040629      * (non è ancora impostato - vedi subr. "StampaRCC")
035280040629     c***                eval      *in52  =  (Pbco <> *blanks)
035290030515      *
035300030515      * Flag "*" > x Consegne particolari
035310030515     c                   if        DB0tc1 =  'A'  or  DB0tc2 = 'A'
035320030515     c                   eval      Past   =  '*'
035330030515     c                   endif
035340030515      *
035350030515      * C/ASSEGNO - - - - - - - - - - - - - - - - - - - - - - - - - - *
035360030515if  1c                   IF        DB0cas <> *zeros
035370030515      * - Flag "*" > x C/Assegno
035380030904if  2c                   if        O21fca <> *blanks
035390030904     c                   movel     '*'           Past
035400030515e   2c                   endif
035410030624      * - Imposto i campi di stampa
035420030624     c                   eval      Ptic   =  DB0tic + ' ' + DB0dti
035430030515e   1c                   ENDIF
035440030515      *
035450040607      * TIPO LDV  - - - - - - - - - - - - - - - - - - - - - - - - - - *
035460030515      * - Se Bolla in Assegnato: stampo una "A" vicino all'asterisco
035470030515      * e se Bolla in Assegnato Senza Fattura: stampo "SEGUE FATTURA"
035480030515if  1c                   if           %subst(DB0tb1:1:1) = 'A'
035490030515     c                             or        DB0tb2     <> *blanks
035500030515     c                   eval      Pass   =  'A'
035510030515if  2c                   if        DB0nft =  *zeros
035520031125     c                   eval      *in22  =  *on
035530130215     c                   eval      Piva   =  'SEGUE FATTURA'
035540030515e   2c                   endif
035550030515e   1c                   endif
035560030515      *
035570030515      * DECODIFICA TIPO CONSEGNA RICHIESTA  - - - - - - - - - - - - - *
035580051129sel 1***c                   Select
035590051129w   1***c                   WHEN      DB0tcr = *blanks
035600051129     *** * - giorno si
035610051129w   1***c                   WHEN      DB0dcr <> *zeros  and  DB0tcr = 'P'
035620051129     ***c                   eval      Ptdr   =  'PRIMA DEL  '
035630051129w   1***c                   WHEN      DB0dcr <> *zeros  and  DB0tcr = 'D'
035640051129     ***c                   eval      Ptdr   =  'DOPO IL    '
035650051129     *** * - giorno no  ora si
035660051129w   1***c                   WHEN      DB0dcr =  *zeros  and  DB0tcr = 'P'
035670051129     ***c                   eval      Ptdr   =  'PRIMA DELLE'
035680051129w   1***c                   WHEN      DB0dcr =  *zeros  and  DB0tcr = 'D'
035690051129     ***c                   eval      Ptdr   =  'DOPO LE    '
035700051129e   1***c                   ENDSL
035710051129if  1c                   IF            DB0dcr >  *zeros
035720051129     c                             or  DB0hcr >  *zeros
035730051129     c                   clear                   FNLV80ds
035740051129     c                   eval      ILV80aas   =  DB0aas
035750051129     c                   eval      ILV80lnp   =  DB0lnp
035760051129     c                   eval      ILV80nrs   =  DB0nrs
035770051129     c                   eval      ILV80nsp   =  DB0nsp
035780051129     c                   eval      ILV80mgs   =  DB0mgs
035790051129     c                   eval      ILV80tc1   =  DB0tc1
035800051129     c                   eval      ILV80tc2   =  DB0tc2
035810051129     c                   eval      ILV80dcr   =  DB0dcr
035820051129     c                   eval      ILV80hcr   =  DB0hcr
035830051129     c                   eval      ILV80tcr   =  DB0tcr
035840051129     c                   call      'FNLV80R'
035850051129     c                   parm                    KPJBA
035860051129     c                   parm                    FNLV80ds
035870051129sel 2c                   Select
035880051129      * errore
035890051129w   2c                   When      OLV80err  <>  *blanks
035900051129      * consegna tassativa (per ora solo ai supermercati)
035910051129w   2c                   When      OLV80tat   =  'S'
035920051129     c                   movel     OLV80tcrd     Pcot
035930091116      * NO "Prima del" per il cliente TTL
035940091116w   2c                   When      DB0ccm     = c_CliTTL   and
035950091116     c                             DB0dcr    <> *zeros     and  DB0tcr = 'P'
035960051129      * prima/dopo il/delle
035970051129      * (il giorno e/o l'ora di consegna richiesta sono già impostati
035980051129      *  nella subr. "TipoLDV")
035990051129w   2c                   Other
036000051129     c                   movel     OLV80tcrd     Ptdr
036010051129e   2c                   Endsl
036020051129e   1c                   ENDIF
036030030515      *
036040030515      * DECODIFICA DEI TURNI DI CHIUSURA  - - - - - - - - - - - - - - *
036050060921      * (SE?non?è "Fermo Deposito")
036060060921if  1c                   if        DB0ffd =  *blanks
036070060921     c                   eval      Pktc   =  'TURNO DI CHIUSURA'
036080060921e   1c                   endif
036090030515      * 1° turno di chiusura
036100060921if  1c                   IF             DB0gc1 <> *blanks
036110060921     c                             and  DB0ffd  = *blanks
036120030515      * - Flag "*" > Turno di chiusura
036130030515     c                   eval      Past   =  '*'
036140030515if  2c                   If        %subst(DB0gc1:1:1) = *blank
036150030515      * - Nessun giorno specificato - Solo 'M' o 'P'
036160060921     c*** già così:      clear                   Pgc1
036170030515if  3c                   if        DB0gc1 =  ' M'
036180030515     c                   eval      Pgc1   =  %trim($Gio(8))
036190030515x   3c                   else
036200030515     c                   eval      Pgc1   =  %trim($Gio(9))
036210030515e   3c                   endif
036220030515x   2c                   Else
036230030515      * - Giorno specificato
036240030515     c                   movel     DB0gc1        w001a
036250030515     c                   eval      XX     =  1
036260030515     c                   eval      *in31  =  *off
036270030515     c     w001a         lookup    $GgS(xx)                               31
036280030515     c   31              movel     $Gio(xx)      Pgc1
036290030515      *   + 'M' o 'P'
036300030515sel 3c                   select
036310030515w   3c                   when      %subst(DB0gc1:2:1) = 'M'
036320030515     c                   eval      Pgc1   =  %trim(Pgc1) + ' '
036330030515     c                                    +  %trim($Gio(8))
036340030515w   3c                   when      %subst(DB0gc1:2:1) = 'P'
036350030515     c                   eval      Pgc1   =  %trim(Pgc1) + ' '
036360030515     c                                    +  %trim($Gio(9))
036370030515e   3c                   endsl
036380030515e   2c                   Endif
036390030515e   1c                   ENDIF
036400030515      * 2° turno di chiusura
036410060921if  1c                   IF             DB0gc2 <> *blanks
036420060921     c                             and  DB0ffd  = *blanks
036430030515      * - Flag "*" > Turno di chiusura
036440030515     c                   eval      Past   =  '*'
036450030515if  2c                   If        %subst(DB0gc2:1:1) = *blank
036460030515      * - Nessun giorno specificato - Solo 'M' o 'P'
036470060921     c*** già così:      clear                   Pgc2
036480030515if  3c                   if        DB0gc2 =  ' M'
036490030515     c                   eval      Pgc2   =  %trim($Gio(8))
036500030515x   3c                   else
036510030515     c                   eval      Pgc2   =  %trim($Gio(9))
036520030515e   3c                   endif
036530030515x   2c                   Else
036540030515      * - Giorno specificato
036550030515     c                   movel     DB0gc2        w001a
036560030515     c                   eval      XX     =  1
036570030515     c                   eval      *in31  =  *off
036580030515     c     w001a         lookup    $GgS(xx)                               31
036590030515     c   31              movel     $Gio(xx)      Pgc2
036600030515      *   + 'M' o 'P'
036610030515sel 3c                   select
036620030515w   3c                   when      %subst(DB0gc2:2:1) = 'M'
036630030515     c                   eval      Pgc2   =  %trim(Pgc2) + ' '
036640030515     c                                    +  %trim($Gio(8))
036650030515w   3c                   when      %subst(DB0gc2:2:1) = 'P'
036660030515     c                   eval      Pgc2   =  %trim(Pgc2) + ' '
036670030515     c                                    +  %trim($Gio(9))
036680030515e   3c                   endsl
036690030515e   2c                   Endif
036700030515e   1c                   ENDIF
036710071114      *
036720071114      * ALTRI DATI/FLAG/... - - - - - - - - - - - - - - - - - - - - - *
036730030515      *
036740071114      * Se pgm richiamato x stampa LdV in arrivo:
036750030515if  1c                   IF        DB0fl1 =  'A'
036760071114      * - imposto l'"*" in base al parametro ricevuto.
036770030515if  2c                   if        DB0fl2 =  '*'
036780030515     c                   eval      Past   =  '*'
036790030515x   2c                   else
036800030515     c                   clear                   Past
036810030515e   2c                   endif
036820071114      * - idem per flag di bolla non abilitata
036830030515     c                   eval      Pdis   =  DB0fl5
036840030515e   1c                   endif
036850061102      *
036860071114      * Decodifica cliente a cui richiedere il codice fiscale e/o la
036870071114      *   partita iva
036880071114if  1c                   IF             $No_CF =  *on
036890071114     c                             or   $No_PI =  *on
036900061106     c                   clear                   wRSxCF
036910061102sel 2c                   select
036920061102      * - P.F. prepagato
036930061106w   2c                   when      DB0tbo =  '8'
036940061106     c                   movel     DB0rsm        wRSxCF
036950061102      * - P.A. con fattura immediata
036960061106w   2c                   when      %subst(DB0tb1:1:1) = 'A'
036970061106     c                   movel     DB0rsd        wRSxCF
036980061106      * - con 2ª bolla
036990061103w   2c                   when           DB0tb2 <> *blanks
037000121029     c                             and  DB0tbo <> 'P'
037010061103     c                             and  DB0tbo <> '8'
037020061106     c                   movel     DB0rsd        wRSxCF
037030061102e   2c                   endsl
037040061102      *
037050061102e   1c                   ENDIF
037060030515      *
037070030515     c                   ENDSR
037080080918      /free
037090090528
037100090528       //--------------------------------------------------------------
037110140303       //?Reperimento Numero e Data del DDT originale del mittente     ?
037120140303       //?"Consorzio Lotterie Nazionali - Gratta & Vinci"              ?
037130090528       //--------------------------------------------------------------
037140090528       BEGSR  sr_RepDdtCLN;
037150090528
037160110426         // -?Verifica se la spedizione ha mamma?
037170090528         if  not %open(FNLBL01L);
037180090528           open  FNLBL01L;
037190090528         endif;
037200090528         chain  (DB0aas : DB0lnp : DB0nrs : DB0nsp)  FNLBL000;
037210090528
037220090528         Select;
037230090528
037240110426           // -?Con Mamma: reperimento dati DDT dalla mamma?
037250090528           When  %found(FNLBL01L);
037260090528             if  not %open(FNBLP01L);
037270090528               open  FNBLP01L;
037280090528             endif;
037290090603             chain  (LBLaao : LBLlpo : LBLnro : LBLnso)  FNBLP000;
037300090528             if  %found(FNBLP01L);
037310090528               exsr  sr_DatiCLN_blp;
037320090528             endif;
037330090528
037340110426           // -?Dati da DS?
037350090901           When  DB0ges = 'B';
037360090901             exsr  sr_DatiCLN_DS;
037370090901
037380110426           // -?In Partenza: reperimento dati DDT da FNBLP?
037390090528           When  (DB0tbo = 'P'   or   DB0tbo = '8');
037400090528             exsr  sr_DatiCLN_blp;
037410090528
037420110426           // -?In Arrivo: reperimento dati DDT da FNARB?
037430090528           Other;
037440090528             exsr  sr_DatiCLN_arb;
037450090528
037460090528         EndSl;
037470090528
037480090528       ENDSR;
037490090528
037500090901       //--------------------------------------------------------------
037510140303       //?Da DS                                                        ?
037520090901       //--------------------------------------------------------------
037530090901       BEGSR  sr_DatiCLN_DS;
037540090901
037550110426         // -?Numero DDT?
037560090901         select;
037570090902           when  DB0rmn <> *zero;
037580100906             if  DB0rmn  > -99999999   and
037590100906                 DB0rmn <   99999999;
037600100906               WddtCLN = DB0rmn;
037610100906             endif;
037620090902           when  DB0ncd <> *zero;
037630090902             WddtCLN = DB0ncd;
037640090901         endsl;
037650090901
037660110426         // -?Data DDT in formato gg/mm/aaaa?
037670090902         Data_Eur = %date( ((DB0aas * 10000) + DB0mgs) : *iso );
037680090901         WdspCLN  = ( ( %subdt(Data_Eur : *days)  * 1000000 ) +
037690090901                      ( %subdt(Data_Eur : *months) * 10000 ) +
037700090901                      ( %subdt(Data_Eur : *years) ) );
037710090901
037720090901       ENDSR;
037730090901
037740090528       //--------------------------------------------------------------
037750090528       //?In Partenza: reperimento dati DDT da FNBLP                   ?
037760090528       //--------------------------------------------------------------
037770090528       BEGSR  sr_DatiCLN_blp;
037780090528
037790110426         // -?Numero DDT?
037800090528         select;
037810090528           when  BLPrmn <> *zero;
037820100906             if  BLPrmn  > -99999999   and
037830100906                 BLPrmn <   99999999;
037840100906               WddtCLN = BLPrmn;
037850100906             endif;
037860090528           when  BLPncd <> *zero;
037870090528             WddtCLN = BLPncd;
037880090528         endsl;
037890090528
037900110426         // -?Data DDT in formato gg/mm/aaaa?
037910090528         Data_Eur = %date( ((BLPaas * 10000) + BLPmgs) : *iso );
037920090528         WdspCLN  = ( ( %subdt(Data_Eur : *days)  * 1000000 ) +
037930090528                      ( %subdt(Data_Eur : *months) * 10000 ) +
037940090528                      ( %subdt(Data_Eur : *years) ) );
037950090528
037960090528       ENDSR;
037970090528
037980090528       //--------------------------------------------------------------
037990090528       //?In Arrivo: reperimento dati DDT da FNARB                     ?
038000090528       //--------------------------------------------------------------
038010090528       BEGSR  sr_DatiCLN_arb;
038020090528
038030110426         // -?Numero DDT?
038040090528         select;
038050090528           when  ARBrmn <> *zero;
038060100906             if  ARBrmn  > -99999999   and
038070100906                 ARBrmn <   99999999;
038080100906               WddtCLN = ARBrmn;
038090100906             endif;
038100090528           when  ARBncd <> *zero;
038110090528             WddtCLN = ARBncd;
038120090528         endsl;
038130090528
038140110426         // -?Data DDT in formato gg/mm/aaaa?
038150090528         Data_Eur = %date( ((ARBaas * 10000) + ARBmgs) : *iso );
038160090528         WdspCLN  = ( ( %subdt(Data_Eur : *days)  * 1000000 ) +
038170090528                      ( %subdt(Data_Eur : *months) * 10000 ) +
038180090528                      ( %subdt(Data_Eur : *years) ) );
038190090528
038200090528       ENDSR;
038210080918
038220080918       //--------------------------------------------------------------
038230140303       //?Stampa della sola Packing-List (2 per A4)                    ?
038240080918       //--------------------------------------------------------------
038250080918       BEGSR  StampaPkgLst;
038260130315
038270130315         // -?SE NON esiste il *file FIAR8 => uscita?
038280130315         if  $E_AR8 = *off;
038290130315           leavesr;
038300130315         endif;
038310080918
038320110426         // -?MODIFICA SOSPESA:?
038330110426         //  ?NON STAMPA LA PACKING-LIST DAL 2° TENTATIVO DI CONSEGNA?
038340081013         //if   $FIAR8 = *off   or   DB0fl1 = 'D';
038350081013         //  leavesr;
038360081013         //endif;
038370080918
038380110426         // -?Azzeramento numero di pagina per Lettera di Vettura?
038390110426         //  ?(meglio: "reimpostazione a 1")?
038400080926         reset  wPage;
038410080918
038420110426         // -?Posizionamento sul foglio A4 in base al n° pagina in stampa?
038430130418         *in33 = *off;
038440130418         exsr  Posiz_Box;
038450080919
038460110426         // -?Impostazione dei dati da stampare nel rec. con la costante?
038470110426         //  ?"Copia per il destinatario"  + estremi spedizione e pag.?
038480110426         //
038490110426         // -?"Copia per il destinatario" già impostato in sr. "PrepPrt"?
038500081017         //if  §AR8cxd = *blank;
038510130726         //  Tcxd  = c_CopiaXdest;
038520130726         //  Tcxd2 = Tcxd;
038530081017         //endif;
038540081017
038550110426         // -?Estremi spedizione da impostare SOLO se si stampando?
038560110426         //  ?la sola packing-list (DB0FA4 = "P")?
038570081017         if  DB0fa4 = 'P';
038580101028           Trsd = Prsd;
038590080922           Trif = 'Spedizione '
038600080924                + %editc(DB0lnp : 'X') + ' '
038610110310                + %editc(DB0nrs : 'X') + ' '
038620110310                + %editc(DB0nsp : 'X')
038630080922                + ' del ' + %editc(Pdsp : 'Y')
038640081017                + '        Pag.'
038650080922                + %editc(wPage:'Z');
038660101028           Tndc = Pndc;
038670081002           if  wSeqLdV = 1;
038680081002             §Pos   = wPosIni + c_PosBox1As + 0.040;
038690090422             §Pos   = wPosIni + c_PosBox1As + 0.035;
038700081002           else;
038710081002             §Pos   = wPosIni + c_PosBox1As - 0.070;
038720081002           endif;
038730080919         endif;
038740081017
038750110426         // -?La stampa di tale record verrà fatta dopo?
038760081017         //if  §AR8cxd = *blank   or   DB0fa4 = 'P';
038770081017         //  write A4prtDT0;
038780081017         //endif;
038790080918
038800110426         // -?Stampa Packing-List?
038810130315         exsr  StampaDDT;
038820080918
038830080918       ENDSR;
038840090528
038850080918      /end-free
038860030515      *
038870030515      *---------------------------------------------------------------*
038880050217      *?Stampa della Lettera di Vettura                              ?*
038890030515      *---------------------------------------------------------------*
038900040607     c     StampaLDV     BEGSR
038910080604      *
038920080604      * - Posizionamento sul foglio A4 in base al n° pagina in stampa
038930130418     c                   eval      *in33 = *off
038940130418     c                   exsr      Posiz_Box
038950030515      *
038960040607      * Azzeramento numero di pagina per Lettera di Vettura
038970030716      * (meglio: "reimpostazione a 1")
038980080926     c                   reset                   wPage
038990030515      *
039000120530      * 1ª parte - copia per il vettore (da ritornare in "BRT")
039010130418     c                   write     A4prtLDV
039020130418     c                   write     A4prtBOX
039030110426     c*//                write     A4LogoBAR
039040081020      /free
039050120530         //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
039060120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
039070130306         //  // >?Simil-Logo Bartolini/BRT?
039080120530         //  write  A4LogoBR2;
039090120530         //  write  A4LogoDA2;
039100120530         //else;
039110130306         //  // >?Simil-Logo Bartolini?
039120120530         //  write  A4LogoBAR;
039130120530         //  write  A4LogoDA1;
039140120530         //endif;
039150130306         // >?Simil-Logo BRT?
039160120530         write  A4LogoBRT;
039170120530         write  A4LogoDA3;
039180110426
039190110426         // >?dati DPD?
039200081020         if  *in71;
039210081020           exsr  StampaDPD;
039220081020         endif;
039230130306
039240130306         // -?Se LdV con PDF => registrazione rec. nel *file PDFLIST?
039250130306         if  %parms() > 1  and  (DB6pdf = 'F'  or  DB6pdf = 'X')
039260130306                           and  $§PDF   = *on;
039270130306           exsr  sr_WrtRec_PdfList;
039280130306         endif;
039290081020
039300130306         // -?Se richiesta solo tagliatella (copia per il vettore, da?
039310130306         //  ?lasciare però al destinatario) => fine?
039320081020         if  DB0fa4  = '4';
039330081020           leavesr;
039340081020         endif;
039350081020      /end-free
039360050510      *
039370081028      * Se LdV in formato "ex-A5" rimane la copia per il destinatario
039380050510     c     *in69         cabeq     *on           End_LdV
039390030515      *
039400030515sel 1c                   select
039410081028      * > dati DPD  (ora in ex-A5 o in 1/4 di A4)
039420081028w   1c***                when      *in71
039430030515      * > dati FedEx
039440030515w   1c                   when      *in72
039450030515     c                   exsr      ListaFED
039460081028      * > dati EuroExpress  (non più gestiti)
039470040223w   1c                   when      *in73
039480030515      *
039490030515e   1c                   endsl
039500030910      *
039510030910      * > elenco SEGNACOLLI NON SEQUENZIALI
039520050511      *  (no x max 2 perchè sono già stati elencati nell'apposita area)
039530030910      *  (no x DPD   perchè monocollo)
039540030910      *  (no x FedEx perchè segnacolliamo noi)
039550030910      *  (no x "A5"  perchè sono già stati elencati tra le "Note e
039560030910      *              Particolarità")
039570030910if  1c                   if        DB0fns =  'S'
039580050511     c                             and  DB0ncl > 2
039590050513     c                             and  $A4_x_ScNS  =   *on
039600030910     c***                          and  not *in71  and  not *in72
039610030910     c                             and  not *in69
039620030910     c                   exsr      ListaScNS
039630030910e   1c                   endif
039640031124      *
039650031124      * Reperimento della posizione utilizzabile per la stampa dei box
039660080825      *   (in pollici) in base all'ultima riga stampata in 1/8" o 1/9"
039670031124     c                   exsr      CalcRowPos
039680060524      *
039690070123      * > Spedizione con D.A.S. x cliente "PRAOIL OLEODOTTI ITALIANI"
039700081127if  1c                   if             DB0ccm = c_CliPraOil
039710060529     c                             or  (DB0fbr = 'S'
039720081127     c                             and  §B4Aks = c_CliPraOil)
039730060524     c                   exsr      StampaDAS
039740060524e   1c                   endif
039750031124      *
039760031124      * > Promemoria per autotrasportatore
039770110502if  1c*//*               if            (DB0gma = Ktab7V_CB
039780110502     c*//*                           or DB0gma = Ktab7V_C1
039790080625     c*** scontato x GMA='C1'       and DB0bam = 'S'
039800110502     c*//*                           or DB0gma = Ktab7V_C2
039810080729     c*** scontato x GMA='C2'       and DB0bam = 'S'
039820110502     c*//*                           or DB0gma = Ktab7V_C3
039830100513     c*** scontato x GMA='C3'       and DB0bam = 'S'
039840110411     c*//*                           or DB0gma = Ktab7V_C4
039850110411     c*//*                           or DB0gma = Ktab7V_C5
039860110502     c*//*                           or DB0gma = Ktab7V_R1
039870110502     c*//*                           or DB0gma = Ktab7V_NR)
039880110502     c*//*                         and  §7Rldv = 'S'
039890110502     c                   if            (§7Rldv = 'S'
039900110502     c                             and  $TabPXA2  = *on)
039910061106     c                             OR   $No_CF = *on
039920071114     c                             OR   $No_PI = *on
039930070306     c                             OR   $FIARN = *on
039940070315     c                             OR   $C_A_  = *on
039950080801     c                             OR   $ConsParz = *on
039960090302     c                             OR   $TabPXA   = *on
039970110411     c                             OR   $TabPXA2  = *on
039980031124     c                   exsr      ListaPMxA
039990031124e   1c                   endif
040000030515      *
040010030515      * > dati FIAR5
040020150224if  1c*//                if           %subst(DB0gva:1:1) = 'B'
040030150224     c*//                          or %subst(DB0gva:1:1) = 'O'
040040150224if  1c                   if                  DB0tc1      = 'S'
040050030717     c                             or        DB0tc2      = 'S'
040060030515     c                   exsr      ListaAR5
040070030515e   1c                   endif
040080030910      *
040090071015      * > Riferimento O.R.M.
040100071015sel 1c                   select
040110071015      * - > per ritiro contestuale alla consegna
040120100608w   1c                   when          (§7Rrc  = 'S'
040130071015     c                             and  §7Rldv = 'S')
040140030910     c                   exsr      StampaRCC
040150071015      * - > per ritiro porto franco prepagato
040160071015w   1c                   when           DB0tbo = '8'
040170071015     c                   exsr      StampaRPFPP
040180071015e   1c                   endsl
040190090924      /free
040200090924
040210110426         // >?Note per destinatario?
040220090924if  1    if  $BoxNxD;
040230090924           exsr  StampaNxD;
040240090924e   1    endif;
040250090924
040260090924      /end-free
040270040607      *
040280081013      * > DDT in LDV
040290081013      * - IN FUTURO: se al 1° tentativo di consegna
040300081013      *              (DB0FL1 = "D" - v. pgm FNLRS9R)
040310080917if  1c                   if             $FIAR8 =  *on
040320130315     c                             and  $E_AR8 =  *on
040330081013     c***-=»>                      and  DB0fl1 <> 'D'
040340040607     c                   exsr      StampaDDT
040350040607e   1c                   endif
040360050510      *
040370050510     c     End_LdV       tag
040380030515      *
040390080703      * 2ª parte - copia per il destinatario (da lasciare)
040400130418     c                   eval      *in33 = *on
040410040607     c                   exsr      ChiusuraLDV
040420030515      *
040430030515      * Stampa dei 3 asterischi finali (se richiesta)
040440030515if  1c                   if        DB0ges = '*'
040450030515     c                   move      '***'         Pmsg3
040460130418     c                   eval      *in33 = *off
040470130418     c                   exsr      Posiz_Box
040480030515     c                   write     A4prtMSG
040490030515e   1c                   endif
040500030515      *
040510030515     c                   ENDSR
040520080604      /free
040530080604
040540080604       //--------------------------------------------------------------
040550140303       //?Posizionamento del box all'interno della pag. A4             ?
040560080604       //--------------------------------------------------------------
040570130418       BEGSR  Posiz_Box;
040580080704
040590080704         select;
040600080704
040610110426           // -?Richiesta stampa di 1 LdV x pagina?
040620110426           //  ?(LdV in modifica o con packing-list del mitente)?
040630080704           when  DB0fa4 = '1'   or   DB0fa4 = 'S';
040640080704             $LdVxP   = 1;
040650080918             $Len1LdV = $LenLdV2;
040660080704
040670110426           // -?Richiesta stampa di 2 LdV x pagina (ex A5)?
040680110426           //  ?(copia per il vettore & copia per il destinatario)?
040690080704           when  DB0fa4 = '2';
040700080704             $LdVxP   = 2;
040710080704             $Len1LdV = $LenLdV2;
040720080704
040730110426           // -?Richiesta stampa di 4 LdV x pagina?
040740110426           //  ?(solo copia per il vettore)?
040750080704           when  DB0fa4 = '4';
040760080704             $LdVxP   = 4;
040770080704             $Len1LdV = $LenLdV4;
040780080918
040790110426           // -?Richiesta stampa della sola packing-list?
040800110426           //  ?(solo copia per il vettore)?
040810080918           when  DB0fa4 = 'P';
040820080918             $LdVxP   = 2;
040830080924             $Len1LdV = c_Len2_inch;
040840080704
040850080704           other;
040860080704             reset $LdVxP;
040870080704             reset $Len1LdV;
040880080704
040890080704         endsl;
040900080704
040910110426         // -?Richiesta "ripartenza" da foglio A4 successivo?
040920130418         if DB0fl6 = *on  and  Not  *in33;
040930080704           clear  wNumLdV;
040940080704         endif;
040950080704
040960110426         // -?Calcolo posizione di partenza all'interno del foglio A4.?
040970110426         //  ?[Se 2ª LdV nella pagina, ma LdV per A4 (intero):?
040980110426         //   ?si salta alla pag. A4 successiva; la LdV in esame?
040990110426         //   ?dev'essere stampata in posizione iniziale dell'A4,?
041000110426         //   ?seguita dai suoi dati ulteriori.]?
041010130418         if  Not  *in33;
041020130418           wNumLdV  += 1;
041030130418           wSeqLdV   = %rem( wNumLdV : $LdVxP );
041040130418         endif;
041050080704
041060130422         // -?Stampa della copia per il vettore?
041070130422         IF  NOT  *in33;
041080130422
041090130422           select;
041100130422
041110130422             // -?Modulo A4 intero?
041120130422             when  NOT  *in69    and   DB0fa4 <> '4'
041130130422                                 and   DB0fa4 <> 'P';
041140130422               wSeqLdV = 1;
041150130422             when  DB0fa4 = '1'   or   DB0fa4 = 'S';
041160130422               wSeqLdV = 1;
041170130422
041180130422             // -?Calcolo della posizione x box all'interno della pagina "A4"?
041190130422             when  wSeqLdV = *zero   and
041200130422                   $LdVxP  = 2;
041210130422               wSeqLdV = 2;
041220130422             when  wSeqLdV = *zero   and
041230130422                   DB0fa4  = '4';
041240130422               wSeqLdV = 4;
041250130422             when  wSeqLdV = *zero   and
041260130422                   *in69;
041270130422               wSeqLdV  = 2;
041280130422
041290130422           endsl;
041300130422
041310130422         ELSE;
041320130422
041330130422           select;
041340130422
041350130422             // -?Modulo A5 o ricevuta (¼ di A4)?
041360130422             when  *in69;
041370130422
041380130422             // -?Modulo A4 intero - 1ª parte (alta)?
041390130422             when  CurLineA4 <  26   and   §PS1 < c_§PS1;
041400130422               wSeqLdV = 1;
041410130422
041420130422             // -?Modulo A4 intero - 2ª parte (passa)?
041430130422           //when  CurLineA4 <  78;   (in 1/9", ma sarà in 1/8"...)
041440130422             when  CurLineA4 <  70;
041450130422               wSeqLdV  = 2;
041460130422
041470130422             // -?Modulo A4 intero - superata 2ª parte?
041480130422             //  ?PER ORA?IMPOSSIBILE !!!?
041490130422             other;
041500130422               clear wMaxLine;
041510130422               exsr  Test_OF;
041520130422               wMaxLine = $MaxLine2;
041530130422               wSeqLdV  = 1;
041540130422
041550130422           endsl;
041560130422
041570130422         ENDIF;
041580080919
041590110426         // -?Impostaz. delle posizioni nella pagina in corso di stampa?
041600080919         wPosIni = ($Len1LdV * (wSeqLdV - 1));
041610130422         if  *in33;       // -?Stampa della copia per il destinatario?
041620130422           wPosIni += $LenLdV4;
041630130422         endif;
041640110426
041650120530         //// -?Impostaz. dimensioni per il logo Bartolini-BRT?
041660120530         //If  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
041670120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
041680120530         //  reset  c_Spessore;
041690120530         //  reset  c_DeltaSpig;
041700120530         //  reset  c_LunghMaxV;
041710120530         //  reset  c_LunghV;
041720120530         //  reset  c_LunghO;
041730120530         //  reset  c_XspaceX;
041740120530         //  reset  c_BOXspace;
041750120530         //Else;
041760120530         //  c_Spessore  = c_1_Spessore;
041770120530         //  c_DeltaSpig = c_1_DeltaSpig;
041780120530         //  c_LunghMaxV = c_1_LunghMaxV;
041790120530         //  c_LunghV    = c_1_LunghV;
041800120530         //  c_LunghO    = c_1_LunghO;
041810120530         //  c_XspaceX   = c_1_XspaceX;
041820120530         //  c_BOXspace  = c_1_BOXspace;
041830120530         //EndIf;
041840120530         // -?Dimensioni per il logo BRT già definite?
041850120530         //c_Spessore  = c_2_Spessore;
041860120530         //c_DeltaSpig = c_2_DeltaSpig;
041870120530         //c_LunghMaxV = c_2_LunghMaxV;
041880120530         //c_LunghV    = c_2_LunghV;
041890120530         //c_LunghO    = c_2_LunghO;
041900120530         //c_XspaceX   = c_2_XspaceX;
041910120530         //c_BOXspace  = c_2_BOXspace;
041920080704
041930110426         // -?Se richiesta la stampa della sola packing-list?
041940110426         //  ?NON si dovranno stampare i dati della LdV?
041950110426         //  ?(quindi NON servono i posizionamenti che seguono)?
041960080918         if  DB0fa4 = 'P';
041970080918           leavesr;
041980080918         endif;
041990080918
042000110426         // -?Posizionamento nel foglio A4 in base al n° LdV in stampa?
042010110426         //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
042020110426
042030130418         select;
042040130418           when  DB0fa4 = '4';
042050130418             *in41 = (wSeqLdV = 1);
042060130418             *in42 = (wSeqLdV = 2);
042070130418             *in43 = (wSeqLdV = 3);
042080130418             *in44 = (wSeqLdV = 4);
042090130418           when Not *in33;
042100130418             *in41 = (wSeqLdV = 1);
042110130418             *in42 = *off;
042120130418             *in43 = (wSeqLdV = 2);
042130130418             *in44 = *off;
042140130418           other;
042150130418             *in41 = *off;
042160130418             *in42 = (wSeqLdV = 1);
042170130418             *in43 = *off;
042180130418             *in44 = (wSeqLdV = 2);
042190130418         endsl;
042200080704         WRITE  A4prtPOS;
042210080704
042220110426         // -?Posizionamenti nella copia per il vettore?
042230110426         //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
042240080704
042250110426         // -?Box in copia per il vettore?
042260080704         §Box1As = wPosIni + c_PosBox1As;
042270140919         §Box1Ai = wPosIni + c_PosBox1Bs  + c_LineNarrow;
042280080704         §Box1Bs = wPosIni + c_PosBox1Bs;
042290080704         §Box1Bi = wPosIni + c_PosBox1Bi;
042300140919         §Box1Bj = wPosIni + c_PosLin1oD1 + c_LineNarrow;
042310080704         §Box1Cs = wPosIni + c_PosBox1Cs;
042320140919         §Box1Ci = wPosIni + c_PosBox1Ds  + c_LineWide;
042330080704         §Box1Ds = wPosIni + c_PosBox1Ds;
042340091119         §Box1Di = §Box1Bi;
042350081218
042360120530         //If  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
042370120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
042380120530         //  // -?Simil-Logo Bartolini?
042390120530         //  §PlblS  = §Box1As + 0.125;
042400120530         //  §PlblVS = 7.092;
042410120530         //  exsr  sr_SimilLogoBAR;
042420120530         //  // -?Simil-Logo BRT?
042430120530         //  §P2blS  = §PlblS;
042440120530         //  §P2blVS = §PlblVD + 0.030;
042450120530         //  exsr  sr_SimilLogoBRT;
042460120530         //Else;
042470120530         //  // -?Simil-Logo Bartolini?
042480120530         //  §PlblS  = §Box1As;
042490120530         //  §PlblVS = 7.128;
042500120530         //  exsr  sr_SimilLogoBART;
042510120530         //EndIf;
042520120530         // -?Simil-Logo BRT?
042530120530         §PlblS  = §Box1As;
042540120530         §PlblVS = 7.581;
042550120530         exsr  sr_SimilLogoBRT_;
042560080704
042570120530         // -?Costanti dati anagrafici BRT?
042580110426         §PC0    = wPosIni + c_PosTBar;
042590110426         §PC1    = wPosIni + c_PosTBa1;
042600120530         //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
042610120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
042620120530         //  §PC2    = wPosIni + c_PosTBa2;
042630120530         //  §PC3    = wPosIni + c_PosTBa3;
042640120530         //  §PC4    = wPosIni + c_PosTBa4;
042650120530         //  §PC5    = wPosIni + c_PosTBa5;
042660120530         //  §PC6    = wPosIni + c_PosTBa6;
042670120530         //else;
042680120530         //  §PC2    = wPosIni + c_PosTBa2x;
042690120530         //  §PC3    = wPosIni + c_PosTBa3x;
042700120530         //  §PC4    = wPosIni + c_PosTBa4x;
042710120530         //  §PC5    = wPosIni + c_PosTBa5x;
042720120530         //  §PC6    = wPosIni + c_PosTBa6x;
042730120530         //endif;
042740120530         §PC2    = wPosIni + c_PosTBa2x;
042750120530         §PC3    = wPosIni + c_PosTBa3x;
042760120530         §PC4    = wPosIni + c_PosTBa4x;
042770120530         §PC5    = wPosIni + c_PosTBa5x;
042780120530         §PC6    = wPosIni + c_PosTBa6x;
042790130726         §PC7    = §Box1Bi - 0.150;
042800110426
042810110426         // -?Linee orizzontali nel box in copia per il vettore?
042820080704         §Lo1S1  = wPosIni + c_PosLin1oS1;
042830080704         §Lo1S2  = wPosIni + c_PosLin1oS2;
042840080922       //§Lo1S3  = wPosIni + c_PosLin1oS3;
042850080704         §Lo1S4  = wPosIni + c_PosLin1oS4;
042860091119         §Lo1S5  = wPosIni + c_PosLin1oS5;
042870080704         §Lo1C1  = wPosIni + c_PosLin1oC1;
042880080704         §Lo1C2  = wPosIni + c_PosLin1oC2;
042890140915         §Lo1C3  = wPosIni + c_PosLin1oC3;
042900080704         §Lo1D1  = wPosIni + c_PosLin1oD1;
042910080704
042920110426         // -?Lunghezza linee verticali nel box in copia per il vett.?
042930140922         §LLv1A  = §Lo1S5  - §Lo1S1;
042940140922         //§LLv1B  = §Lo1S4  - §Box1Bs;
042950140922         //§LLv1C  = §Box1Di - §Box1Bs;
042960140922         //***§LLv1B  = §Lo1S4  - §Box1Bj + c_LineNarrow;
042970140922         §LLv1B  = §Lo1S4  - §Box1Bj;
042980140922         §LLv1C  = §Box1Di - §Box1Bj;
042990140917
043000140917         // -?Costante "BRTcode" & codice BRTcode?
043010140917         §Pk1    = wPosIni + c_PosKBRTc;
043020080704
043030110426         // -?Costante "Consegna tassativa"?
043040080704         §Pk2    = wPosIni + c_PosKTass;
043050080704
043060110426         // -?Costanti "(nome e cognome in stampatello)", "(firma)" e?
043070110426         //           ?"(data e ora)"?
043080080704         §Pk3    = §Box1Bi - 0.015;
043090080704
043100110426         // -?Costante con rif. alla legge in vigore per RCV?
043110080704         §Pk4    = wPosIni + c_PosKRCV;
043120080704
043130110426         // -?Costante "Bollo assolto in modo virtuale..."?
043140080704         §Pk5    = wPosIni + c_PosKBAV;
043150080704         §Pk6    = §Pk5 + 0.060;
043160121017
043170121017         // -?Partita IVA o Cod. Fiscale del cliente?
043180121017         §Pk7    = §Pk4 + 0.080;
043190080704
043200110426         // -?Costante "COPIA PER IL DESTINATARIO" in DDT "generico"?
043210080709         §Pos    = c_PosKCXD2;
043220080704
043230080704       ENDSR;
043240080605
043250080604      /end-free
043260110426
043270140303       //--------------------------------------------------------------
043280140303       //?Posizionamento del simil-logo Bartolini (unico)              ?
043290140303       //--------------------------------------------------------------
043300120530      *///copy gaitrasrc/srcTNSY,LogoBAR_R2
043310110426
043320140303       //--------------------------------------------------------------
043330140303       //?Posizionamento del simil-logo Bartolini (con BRT)            ?
043340140303       //--------------------------------------------------------------
043350120530      *///copy gaitrasrc/srcTNSY,LogoBAR3R2
043360110426
043370140303       //--------------------------------------------------------------
043380140303       //?Posizionamento del simil-logo BRT (con BARTOLINI)            ?
043390140303       //--------------------------------------------------------------
043400120530      *///copy gaitrasrc/srcTNSY,LogoBRT3R2
043410120530
043420140303       //--------------------------------------------------------------
043430140303       //?Posizionamento del simil-logo BRT (unico)                    ?
043440140303       //--------------------------------------------------------------
043450120530      /copy gaitrasrc/srcTNSY,LogoBRT_R2
043460110426
043470030515      *---------------------------------------------------------------*
043480050217      *?Gestione overflow                                            ?*
043490030515      *---------------------------------------------------------------*
043500030515     c     Test_OF       BEGSR
043510030515      *
043520030515      * Test raggiungimento della posizione limite
043530081003     c                   if          ((§PS1 < $LimOF   and   NOT *in45)
043540081003     c                             or (§PS1 < ($LimOF - c_Len1_inch)
043550081003     c                                     and *in45   and   NOT *in46)
043560081003     c                             or (§PS1 < ($LimOF + c_Len1_inch)
043570081003     c                                     and *in45   and   *in46))
043580080924     c                             and CurLineA4 < wMaxLine
043590080919     c                   leavesr
043600030515e   1c                   endif
043610030515      *
043620030515      * Stampa del box per dati da FIAR5
043630030515      * (se è già stata stampata almeno una riga di tale box)
043640060921if  1c                   if        $FIAR5 = *on  and  $First = *off
043650030911     c                   eval      §LO2   = §PS1 + 0.050
043660071008if  2c                   if        NOT *in52
043670030515     c                   write     A4prtAR53
043680071008x   2c                   else
043690071008     c                   eval      §PS2   = §LO2 - 0.500
043700071008     c                   write     A4prtAR54
043710071008e   2c                   endif
043720030515e   1c                   endif
043730041209      *
043740041209sel 1c                   select
043750040607      *
043760041209      * DDT KARNAK/TITANEDI/EDK
043770041209      * Stampa di logo, box, linee e testi fissi del DDT del mittente
043780040607      * (se si stà stampando tale DDT)
043790040607      * (ma NON per la parte "merce omaggio")
043800040607      * calcolando - prima - le posizioni iniziale e finale del box
043810041209w   1c                   when           $DDT_p    = *on
043820071008     c                             and  §AR8tsd   = 'K1'
043830040607     c                             and  $DDT_o    = *off
043840040608      * - Stampa parte finale DDT
043850040608if  2c                   if        $DDTlastPg = *off
043860041209      * - - Calcolo della posizione finale del box
043870040607     c                   eval      PCnv2  = '          '
043880040607     c                                    + '*** SEGUE ***'
043890040608     c                   exsr      Posiz_DK
043900040607     c                   write     A4prtDK4
043910040608x   2c                   else
043920041209      * - - Stampa dei dati in chiusura DDT (se ultima pagina)
043930040608      *     (la posizione finale del box verrà lì calcolata)
043940050217     c                   exsr      PrtDdtKARN_V2
043950040608e   2c                   endif
043960040607      * - Stampa logo, box, linee e testi fissi
043970081017     c                   if            §AR8cxd = *blanks
043980081017     c                             or  DB0fa4  = 'P'
043990050218     c                   write     A4prtDT0
044000080923     c                   endif
044010040607     c                   write     A4prtDK0
044020041019      *
044030050414      * DDT LOGISTICA/SAIT Abrasivi
044040050215      * Stampa di logo, box, linee e testi fissi del DDT del mittente
044050041019      * (se si stà stampando tale DDT)
044060041209w   1c                   when           $DDT_p    = *on
044070060627     c                             and  %subst(§AR8tsd:1:1) = 'L'
044080041021     c                   exsr      Posiz_DL
044090050217      * - - Chiusura box in fase di stampa dettaglio DDT
044100050217      *     (la stampa dei dati in chiusura DDT sarà solo nell'ultima
044110050318      *      pagina - prima dell'eventuale packing-list)
044120050318      *     La gestione del salto alla 1ª pagina della packing-list
044130050321      *     (AR8trk = 'Z1' & Page = *zeros) avviene nella subr.
044140050318      *     "PrtDdtLOG1_Z1" (vedi subr. "PrtPckLst_1").
044150081017     c                   if            §AR8cxd = *blanks
044160081017     c                             or  DB0fa4  = 'P'
044170050308     c                   write     A4prtDT0
044180050331     c                   endif
044190050318sel 2c                   select
044200050505      *     - si sta stampando il DDT Logistica o Sait
044210050318w   2c                   when      %subst(AR8trk:1:1) <> 'Z'
044220071008     c                             and %subst(AR8trk:1:1) <> 'S'
044230050218     c                   write     A4prtDL0A
044240050615      *     - si sta stampando la Packing-List
044250050318w   2c                   when      %subst(AR8trk:1:1) =  'Z'
044260050615     c                             and    Page        >  *zeros
044270050318     c                   write     A4prtDL0C
044280050606      *     - si sta stampando la Fattura (oltre 1ª pag.)
044290071008w   2c                   when      %subst(AR8trk:1:1) =  'S'
044300081003     c                             and    Page        >  *zeros
044310050609     c                   write     A4prtDL0E
044320050318e   2c                   endsl
044330050113      *
044340050113      * DDT ARCTE
044350050215      * Stampa di logo, box, linee e testi fissi del DDT del mittente
044360050113      * (se si stà stampando tale DDT)
044370050113w   1c                   when           $DDT_p    = *on
044380050113     c                             and  AR8tmd    = 'ARCT'
044390050114     c                   exsr      Posiz_AC
044400050217      * - - Chiusura box in fase di stampa dettaglio DDT
044410050217      *     (la stampa dei dati in chiusura DDT sarà solo nell'ultima
044420050217      *      pagina)
044430081017     c                   if            §AR8cxd = *blanks
044440081017     c                             or  DB0fa4  = 'P'
044450050331     c                   write     A4prtDT0
044460050331     c                   endif
044470050217     c                   write     A4prtAC0A
044480050215      *
044490050215      * DDT ESPRINET
044500050215      * Stampa di box, linee e testi fissi del DDT del mittente
044510050215      * (se si stà stampando tale DDT)
044520050215      * · Solo nella 1ª pagina vengono stampati il logo e la parte
044530050215      *        superiore del DDT.
044540050215      * · Solo nell'ultima pagina vene stampato il "fondo".
044550050215w   1c                   when           $DDT_p    = *on
044560060627     c                             and  §AR8tsd   = 'E1'
044570110405if 1bc                   if        %subst(SAVtrk:1:1 ) <> 'P'
044580110405     c                             or   %eof(FIAR801L)
044590050215      * - Stampa parte finale DDT
044600050215      * - - Calcolo delle posizioni di box & linee
044610050215     c                   exsr      Posiz_ES
044620050215      * - - Chiusura box in fase di stampa dettaglio DDT
044630050215      *     (la stampa dei dati in chiusura DDT sarà solo nell'ultima
044640050215      *      pagina)
044650050215if  2c***                if        %subst(AR8trk:1:1) <> 'P'
044660081017     c                   if            §AR8cxd = *blanks
044670081017     c                             or  DB0fa4  = 'P'
044680050331     c                   write     A4prtDT0
044690050331     c                   endif
044700050215     c                   write     A4prtES0A
044710050215e   2c***                endif
044720110405e  1bc                   endif
044730041209      *
044740041209      * DDT GENERICO con LOGO
044750050215      * Stampa di logo e box del DDT del mittente
044760041209      * (se si stà stampando tale DDT)
044770041209w   1c                   when           $DDT_p    = *on
044780041209     c                             and  §AR8tsd   = 'S2'
044790041209     *** * - Chiusura del box con i dati di mittente e destinatario
044800041209     *** *   (Se si stanno ancora stampando 'sti dati: bisogna calcolare
044810041209     *** *    la posizione di chiusura temporanea del box)
044820041209if  2***c                   if             AR8trk =  'B1'
044830041209     ***c                             or   AR8trk =  'B2'
044840041209     *** *   Impostazione della posizione finale per il box
044850041209     ***c                   exsr      Rep_§LO2
044860041209     ***c                   eval      §PB2   = §LO2 + 0.065
044870041209e   2***c                   endif
044880041209     *** *   Stampa del box
044890041209     ***c                   write     A4prtDB0
044900041209      * - Il logo è già stato SICURAMENTE stampato
044910041209     c***                exsr      Rep_§LO2
044920041209     c***                eval      §PB1   = §LO2
044930041209      *** Stampa del logo
044940041209     c***                write     A4prtDB0
044950041209      *
044960041209e   1c                   endsl
044970030515      *
044980030515      * Stampa dell'indicazione "Segue..." su pagina corrente
044990030515     c                   clear                   Pseg0
045000030515     c                   clear                   Pseg1
045010030515     c                   eval      Pseg9  = '  Segue...'
045020080919      /free
045030080923         select;
045040080923           when  *in45   and   NOT *in46;
045050080923             §PSA = 0.200;   // Per campo vuoto !
045060080923             §PSB = 0.200;   // Per campo vuoto !
045070080923             §PSC = c_Len2_inch - 0.050;
045080080923           when  *in45   and   *in46;
045090080923             §PSA = c_Len2_inch + 0.200;   // Per campo vuoto !
045100080923             §PSB = c_Len2_inch + 0.200;   // Per campo vuoto !
045110080924             §PSC = c_Len2_inch * 2 - 0.200;
045120080923           when  NOT *in45;
045130080923             §PSA = 0.400;
045140080923             §PSB = 0.700;
045150080923             §PSC = 8.750;
045160080923         endsl;
045170080919      /end-free
045180030515     c                   write     A4prtSEG
045190030515      *
045200031031      * Se sono ancora nella prima pagina =>
045210030515      * stampo anche i dati per il destinatario e box/barcode/ecc.
045220080604sel 1c                   select
045230080923      * > Se NON sto stampando un DDT del mittente
045240080922      *   (nel qual caso non stampo null'altro)
045250080923      *   => nulla
045260080919w   1c                   when      wPage  > 1
045270080923      * > Se sto stampando le sole copie per il vettore (4 in A4)
045280080923      *    o la relativa packing-list
045290080923      *   (nel qual caso non stampo null'altro)
045300080923      *   => nulla
045310081003w   1c                   when      DB0fa4 = '4'   or   DB0fa4 = 'P'
045320040607      * > Se NON è ristampa in arrivo per recapito c/assegni  e
045330040707      *   se NON sto stampando una LdV Poste
045340080703      *   se NON sto stampando (anche) un DDT del mittente
045350080703      *   se NON sto stampando le sole copie per il vettore (4 in A4)
045360080923      *   => posizionamento e stampa
045370120113w   1c                   when          $No_x_Cons =  *off
045380081013     c                             and ($FIAR8    =  *off)
045390081013     c***-=»>                       or  DB0fl1    =  'D')
045400080605      * - Posizionamento sul foglio A4 in base al n° pagina in stampa
045410130418     c                   eval      *in33 = *on
045420130418     c                   exsr      Posiz_Box
045430130418      * - Stampa 2ª parte della LdV (copia per il destinatario)
045440130418     c                   write     A4prtLDV
045450031031      * - Stampa "cornice" / barcode / righine varie in parte inferiore
045460130418     c                   write     A4prtBOX
045470110426     c*//                write     A4LogoBAR
045480110426      /free
045490120530         //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
045500120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
045510120530         //  // >?Simil-Logo Bartolini/Brt?
045520120530         //  write  A4LogoBR2;
045530120530         //  write  A4LogoDA2;
045540120530         //else;
045550120530         //  // >?Simil-Logo Bartolini?
045560120530         //  write  A4LogoBAR;
045570120530         //  write  A4LogoDA1;
045580120530         //endif;
045590120530         // >?Simil-Logo BRT?
045600120530         write  A4LogoBRT;
045610120530         write  A4LogoDA3;
045620110426      /end-free
045630120113      *//* > Se sto stampando una LdV Poste per recapito c/assegni
045640120113w   1c*//                   when          $No_x_Cons =  '2'
045650120113      *//* - Stampa 2ª parte "nulla" della LdV
045660130418     c*//                   eval      *in33 = *on
045670130418     c*//                   exsr      Posiz_Box
045680120113     c*//                   write     A4prtLDVB
045690080604e   1c                   endsl
045700080923      /free
045710110426         // -?Se richiesta la stampa della sola packing-list:?
045720110426         //  ?incremento del n° spedizione per calcolare correttam.?
045730110426         //  ?il n° di pagina iniziale per la spediz. successiva?
045740080925         if  DB0fa4 = 'P';
045750080925             wNumLdV += 1;
045760080925         endif;
045770080925
045780110426         // -?Reimpostazione degli indicatori per il posizionamento?
045790110426         //  ?all'interno del foglio A4?
045800080923         *in45 = (DB0fa4 = 'P');
045810080923         *in46 = (DB0fa4 = 'P'   and   %rem(wNumLdv : 2) = *zero);
045820080923      /end-free
045830030515      *
045840030515      * Salta a pagina successiva e indica "(Seguito)"
045850030515     c                   write     A4prtSP0
045860030515     c                   eval      wPage  = wPage + 1
045870080923      /free
045880081017         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
045890081017           write  A4prtNUL;
045900081017         endif;
045910081017
045920080923         IF  DB0fa4 <> 'P';
045930080922      /end-free
045940030515     c                   eval      Pseg0  = 'Spedizione '
045950110310     c                                    + %editc(DB0lnp : 'X') + ' '
045960110310     c                                    + %editc(DB0nrs : 'X') + ' '
045970110310     c                                    + %editc(DB0nsp : 'X')
045980030515     c                                    + ' del ' + %editc(Pdsp : 'Y')
045990030515     c                   eval      Pseg1  = '(Seguito)             Pag.'
046000030515     c                                    + %editc(wPage:'Z')
046010030515     c                   clear                   Pseg9
046020030515     c                   write     A4prtSEG
046030080922      /free
046040080923         ELSE;
046050081017
046060110426           // -?"Copia per il destinatario" già impostato in sr. "PrepPrt"?
046070081017           //if  §AR8cxd = *blank;
046080130726           //  Tcxd  = c_CopiaXdest;
046090130726           //  Tcxd2 = Tcxd;
046100081017           //endif;
046110081017
046120110426           // -?Estremi spedizione da impostare SOLO se si stampando?
046130110426           //  ?la sola packing-list (DB0FA4 = "P")?
046140081017           if  DB0fa4 = 'P';
046150101028             //Trsd = Prsd;   (già impostato nella subr. StampaPkgLst)?
046160081017             Trif = 'Spedizione '
046170081017                  + %editc(DB0lnp : 'X') + ' '
046180110310                  + %editc(DB0nrs : 'X') + ' '
046190110310                  + %editc(DB0nsp : 'X')
046200081017                  + ' del ' + %editc(Pdsp : 'Y')
046210081017                  + '        Pag.'
046220101028                  + %editc(wPage:'Z');
046230101028                //+ '        (Seguito)';
046240101028             //Tndc = Pndc;   (già impostato nella subr. StampaPkgLst)?
046250081017           endif;
046260080923
046270110426           // -?Verifica se la 2ª parte della packing-list sta nella?
046280110426           //  ?pag. attuale o se deve andare in quella successiva?
046290080923           wSeqLdV  = %rem( wNumLdV : $LdVxP );
046300080923           if  wSeqLdV = *zero   and   $LdVxP  = 2;
046310080923             wSeqLdV = 2;
046320080923           endif;
046330080923
046340110426           // -?Posizionamenti nella pagina in corso di stampa?
046350110426           //  ?(§POS = posizione verticale della costante "COPIA PER?
046360110426           //   ?IL DESTINATARIO" (con estremi spedizione e pag.)?
046370080923           wPosIni  = ($Len1LdV * (wSeqLdV - 1));
046380080924           if  wSeqLdV = 1;
046390080924             §Pos   = wPosIni + c_PosBox1As + 0.040;
046400090422             §Pos   = wPosIni + c_PosBox1As + 0.035;
046410080924           else;
046420080924             §Pos   = wPosIni + c_PosBox1As - 0.070;
046430080924           endif;
046440081017
046450081017           if  §AR8cxd = *blank   or   DB0fa4  = 'P';
046460081017             write A4prtDT0;
046470081017           endif;
046480080923
046490080923         ENDIF;
046500080922      /end-free
046510030515      *
046520030515      * Reimposta posizioni iniziali di stampa per linee
046530030515      *   (orizzontali-§LOn e verticali-§LVn) e campi (§PSn).
046540030515      *   Nelle pagine dalla 2ª in poi si parte da in alto:
046550030515      *   non vengono ri-stampati i 2 box "vettore" e "destinatario".
046560030515sel 1c                   select
046570120530      * - BRT
046580030515w   1c                   when      *in70
046590080711     c                   eval      §LO1   = c_§PS1
046600030515     c                   eval      §LO2   = §LO1
046610030515     c                   eval      §PS1   = §LO1
046620040607sel 2c                   select
046630050217      * - - con DDT del mittente KARNAK / TITANEDI / EDK
046640040607w   2c                   when           $DDT_p    = *on
046650071008     c                             and  §AR8tsd   = 'K1'
046660040608     c                   exsr      NewPageDK
046670050414      * - - con DDT del mittente LOGISTICA / SAIT ABRASIVI
046680041207w   2c                   when           $DDT_p    = *on
046690060627     c                             and  %subst(§AR8tsd:1:1) = 'L'
046700050321     c                   exsr      NewPageDL
046710050113      * - - con DDT del mittente ARCTE
046720050113w   2c                   when           $DDT_p    = *on
046730050113     c                             and  AR8tmd    = 'ARCT'
046740081002     c                   exsr      NewPageAC
046750050215      * - - con DDT del mittente ESPRINET
046760050215w   1c                   when           $DDT_p    = *on
046770060627     c                             and  §AR8tsd   = 'E1'
046780110405if  2c                   if        %subst(SAVtrk:1:1 ) <> 'P'
046790110405     c                             or   %eof(FIAR801L)
046800081002     c                   exsr      NewPageES
046810110405e   2c                   endif
046820041207      * - - con DDT generico (con marchio)
046830041207w   2c                   when           $DDT_p    = *on
046840041207     c                             and  §AR8tsd   = 'S2'
046850081017     c                   exsr      NewPageSTD
046860041207      * - - con DDT generico (senza marchio)
046870041207w   2c                   when           $DDT_p    = *on
046880041207     c*** ("scontato":)            and  §AR8tsd   = 'S1'
046890081017     c                   exsr      NewPageSTD
046900040607e   2c                   endsl
046910080922      * - DPD (ora in ex-A5)
046920080922w   1c***                when      *in71
046930030515      * - FedEx
046940030515w   1c                   when      *in72
046950030709     c                   eval      §LO1   = 1.600
046960030515     c                   eval      §LO2   = §LO1 + 0.425
046970030515     c                   eval      §LV1   = §LO1 + 0.300
046980030515     c                   eval      §LV2   = §LV1 + 0.070
046990030515     c                   write     A4prtFED1
047000030515do  2c                   dow       CurLineA4 < $MinLine1
047010030515     c                   write     A4prtNUL
047020030515e   2c                   enddo
047030030515     c                   write     A4prtFED2
047040030515     c                   eval      §LO1   = §LO2 - 0.250
047050030911     c                   eval      §PS1   = §LO1 + 0.150
047060040223      * - EuroExpress
047070040223w   1c                   when      *in73
047080030515      *
047090030515e   1c                   endsl
047100030515      *
047110030515      * N.B.: Per i SegnaColli non sequenziali non servono queste
047120030515      *       impostazioni: trattasi di stampa con salto riga -skipb-
047130030515      *       quindi sono già posizionato.
047140030515      *
047150080919     c                   ENDSR
047160030910      *
047170030910      *---------------------------------------------------------------*
047180050217      *?Reperimentoo della posizione (in pollici) in cui posizionarsi?*
047190050217      *?* per la stampa in base all'ultima riga stampata             ?*
047200030910      *---------------------------------------------------------------*
047210030910     c     Rep_§LO2      BEGSR
047220030910      *
047230030910sel 1c                   select
047240080825      *
047250080825      * Se ho stampato l'elenco dei segnacolli non sequenziali, devo
047260080825      *   "allineare" il posizionamento in pollici all'ultima riga
047270080825      *   stampata (in ottavi di pollice).
047280080825      *   =>   Num_ultima_riga_stampata / 8
047290080825w   1c                   when           DB0fns     = 'S'
047300080825     c                             and  DB0ncl     > 2
047310080825     c                             and  $A4_x_ScNS = *on
047320080825     c                             and  NOT  *in69
047330080825     c                   eval(h)   §LO2   = (CurLineA4 / 8) + 0,1
047340040607      *
047350040607      * Se ho stampato il DDT del mittente
047360080714      *   =>   Num_ultima_riga_stampata / 9
047370081013w   1c                   when           $FIAR8 =  *on
047380081013     c***-=»>                      and  DB0fl1 <> 'D'
047390080708     c                   eval(h)   §LO2   = (CurLineA4 / 9) + 0,1
047400030910      *
047410030910      * Se non ho stampato né elenco segnacolli non sequenziali né
047420040223      *   altri box, devo impostare comunque i dati per il posiziona-
047430030910      *   mento di questo primo box
047440030910w   1c                   when      §LO2   = *zeros
047450080624     c                   eval      §LO1   = 2.850
047460030910     c                   eval      §LO2   = §LO1
047470030910      *
047480030910e   1c                   endsl
047490030910      *
047500030910     c                   ENDSR
047510031124      *
047520031124      *---------------------------------------------------------------*
047530050217      *?Reperim. della posizione (in pollici) utlizzaile per la      ?*
047540080714      *?  stampa dei box in base all'ultima riga stampata in 1/9"    ?*
047550031124      *---------------------------------------------------------------*
047560031124     c     CalcRowPos    BEGSR
047570031124      *
047580031124      * Imposto le posizioni inziali per il primo dei box
047590080624     c                   select
047600080624      *
047610040607      * - in DDT del mittente: dopo eventuale elenco merce omaggio
047620120530      *   (come in LDV BRT)
047630120530      * - in LDV BRT: dopo eventuale elenco segnacolli non seq.
047640080624     c                   when      *in70
047650080624     c                   exsr      Rep_§LO2
047660080624      *
047670080922      * - in LDV DPD (ora in ex-A5)
047680080922     c***                when      *in71
047690080624      *
047700040607      * - in LDV FEDEX: posizione variabile
047710031124      *   (dipende dal numero dei segnacolli elencati: intanto
047720031124      *   posiziona la "base di partenza" sull'ultima posizione
047730031124      *   eventualmente usata per l'elenco dei segnacolli FedEx;
047740031124      *   poi si ri-calcoleranno in base al numero di righe stampate)
047750080624     c                   when      *in72
047760080624     c                   eval      §LO2   = §LO1 + 0.250
047770080624      *
047780080912      * - in LDV EUROEXPRESS: non più gestita
047790080624     c                   when      *in73
047800080912      *
047810080624     c                   endsl
047820080624      *
047830031124     c                   eval      §LO1   = §LO2
047840031124      *
047850031124     c                   ENDSR
047860060524      *
047870060524      *---------------------------------------------------------------*
047880060524      *?Stampa box per riferimento al                                ?*
047890060524      *?DOCUMENTO di ACCOMPAGNAMENTO SEMPLIFICATO                    ?*
047900060524      *---------------------------------------------------------------*
047910060524     c     StampaDAS     BEGSR
047920060524      *
047930060524     c***                eval      Par5   = '*...+....1....+....2....+-
047940060524     c***                          ....3....+....4....+....5....+....6-
047950060524     c***                          ....+....7....+....8....+....9....+-
047960060524     c***                          ....A'
047970060524      *
047980060524      * - stampo testata
047990090302     ***c                   eval      $First = 'B'
048000090302     ***c                   eval      Par5   = C_PMxA
048010090302      /free
048020090302         $First = *on;
048030090302         Par5   = '                    '
048040090302                + '                    '
048050090302                + '          '
048060090302                + C_PMxA;
048070090302      /end-free
048080060524     c                   exsr      StampaAR5
048090090302      /free
048100090302
048110090302         // - memorizza posizione iniziale per il box
048120090302         //   (§PB1 sarà da reimpostare in §LO1 PRIMA della stampa del box)
048130090302         §PB1   = §LO1;
048140090302
048150090302         // - stampa linea separatrice
048160090302         §LO1   = §PS1 + 0.050;
048170090302         write  A4prtLO;
048180090302         §PS1   = §LO1;
048190090302      /end-free
048200060524      *
048210060524      * - stampo 1° testo
048220060524     c                   eval      $First = '2'
048230060524     c                   eval      Par5   = 'CONSEGNARE AL DESTINATARI-
048240060524     c                             O D.D.T. E D.A.S. (Documento di Acc-
048250060524     c                             ompagnamento Semplificato).'
048260060524     c                   exsr      StampaAR5
048270060524      *
048280060524      * - stampo 2° testo
048290061030     c*** (già fatto:)   eval      $First = *off
048300060524     c                   eval      Par5   = 'In caso di RIFIUTO compil-
048310060524     c                             are l''ultima pagina del D.A.S. ind-
048320060524     c                             icando:'
048330060524     c                   exsr      StampaAR5
048340060524     c                   eval      Par5   = 'a) nome Autotrasportatore-
048350060524     c                              nel campo "IL SOTTOSCRITTO ... INC-
048360060524     c                             ARICATO DEL TRASPORTO"'
048370060524     c                   exsr      StampaAR5
048380060524     c                   eval      Par5   = 'b) firma Autotrasportator-
048390060524     c                             e nel campo "FIRMA INCARICATO DEL T-
048400060524     c                             RASPORTO"'
048410060524     c                   exsr      StampaAR5
048420060524     c                   eval      Par5   = 'c) causale del rifiuto (b-
048430060524     c                             arrare "la merce non è conforme all-
048440060524     c                             ''ordine" o indicare altra motivazi-
048450060524     c                             one)'
048460060524     c                   exsr      StampaAR5
048470060524     c                   eval      Par5   = 'd) firma del destinatario-
048480060524     c                              nel campo "FIRMA DEL DESTINATARIO"'
048490060524     c                   exsr      StampaAR5
048500090302     *** *
048510090302     *** * - stampo box
048520090302     ***c                   eval      §LO2   = §PS1 + 0.050
048530090302     ***c                   write     A4prtAR53
048540090302      /free
048550090302         // - stampa box
048560090302         //   (§PB1 è da reimpostare in §LO1: contiene il suo valore PRIMA
048570090302         //   della stampa delle varie linee separatrici)
048580090302         §LO1   = §PB1;
048590090302         §LO2   = §PS1 + 0.070;
048600090302         write  A4prtAR53;
048610090302      /end-free
048620060524      *
048630060524     c                   ENDSR
048640031124      *
048650031124      *---------------------------------------------------------------*
048660050217      *?Prepara elenco per box "Promemoria per autotrasportatore"    ?*
048670031124      *---------------------------------------------------------------*
048680031124     c     ListaPMxA     BEGSR
048690031124      *
048700070308      * - stampo testata
048710070316     c                   eval      $First = *on
048720070319     c                   eval      Par5   = '                    '
048730070319     c                                    + '                    '
048740070319     c                                    + '          '
048750070319     c                                    + C_PMxA
048760031124     c                   exsr      StampaAR5
048770070315      *
048780070315      * - memorizzo posizione iniziale per il box
048790070320      *   (§PB1 sarà da reimpostare in §LO1 PRIMA della stampa del box)
048800070315     c                   eval      §PB1   = §LO1
048810031124      *
048820070308      * - stampo testo
048830081127      *
048840081212      /free
048850110411         // >?Mittente cliente in tab. "PXA"?
048860090302         IF  $TabPXA = *on;
048870090302
048880090302           k_TBEcod = 'PXA';
048890090302           k_TBEke1 = %editc(DB0ccm : 'X');
048900100316           k_TBEke2 = '001            ';
048910090302           clear  k_TBElin;
048920090302           clear  k_TBEsif;
048930100316           //setll  %kds(k05tntbe01 : 3)  TNTBE000;  (già fatto)
048940090302           //reade  %kds(k05tntbe01 : 2)  TNTBE000;  (già fatto)
048950090302           clear  wx;
048960090302
048970100316           DoW  not  %eof(TNTBE01L)  and  TBEke2 <= '006            ';
048980090302
048990090302             if  TBEatb = *blank;
049000090302
049010110411               // -?Stampa della riga separatrice iniziale?
049020090302               if  wx = *zero;
049030090302                 §LO1   = §PS1 + 0.050;
049040090302                 write  A4prtLO;
049050090302                 §PS1   = §LO1;
049060090302                 $First = 'G';
049070090302               endif;
049080090302
049090110411               // -?Salta eventuali righe vuote?
049100090302               dow  %int( %trimr( TBEke2 ) ) > wx + 1;
049110090302                 clear  Par5;
049120090302                 exsr  StampaAR5;
049130090302                 wx += 1;
049140090302               enddo;
049150090302
049160110411               // -?Stampa la singola riga di testo?
049170090302               Par5   = TBEuni;
049180090302               exsr  StampaAR5;
049190090302               wx += 1;
049200090302
049210090302             endif;
049220090302
049230090302             reade  %kds(k05tntbe01 : 2)  TNTBE000;
049240090302
049250090302           EndDo;
049260090302
049270090302         ENDIF;
049280110411
049290110411         // >?Particolarità consegna in tab. "PXA"?
049300110411         IF  $TabPXA2 = *on;
049310110411
049320110411           k_TBEcod = 'PXA';
049330110411           k_TBEke1 = DB0gma;
049340110411           k_TBEke2 = '001            ';
049350110411           clear  k_TBElin;
049360110411           clear  k_TBEsif;
049370110411           setll  %kds(k05tntbe01 : 3)  TNTBE000;
049380110411           reade  %kds(k05tntbe01 : 2)  TNTBE000;
049390110411           clear  wx;
049400110411
049410110411           DoW  not  %eof(TNTBE01L)  and  TBEke2 <= '006            ';
049420110411
049430110411             if  TBEatb = *blank;
049440110411
049450110411               // -?Stampa della riga separatrice iniziale?
049460110411               if  wx = *zero;
049470110411                 §LO1   = §PS1 + 0.050;
049480110411                 write  A4prtLO;
049490110411                 §PS1   = §LO1;
049500110411                 $First = 'G';
049510110411               endif;
049520110411
049530110411               // -?Salta eventuali righe vuote?
049540110411               dow  %int( %trimr( TBEke2 ) ) > wx + 1;
049550110411                 clear  Par5;
049560110411                 exsr  StampaAR5;
049570110411                 wx += 1;
049580110411               enddo;
049590110411
049600110411               // -?Stampa la singola riga di testo?
049610110411               Par5   = TBEuni;
049620110411               exsr  StampaAR5;
049630110411               wx += 1;
049640110411
049650110411             endif;
049660110411
049670110411             reade  %kds(k05tntbe01 : 2)  TNTBE000;
049680110411
049690110411           EndDo;
049700110411
049710110411         ENDIF;
049720090302
049730081212      /end-free
049740081127      *
049750031216sel 1c                   select
049760070306      * >
049770070306x   1c***                other
049780070306     c***                eval      $First = *off
049790070306     c***                clear                   Par5
049800070306     c***                exsr      StampaAR5
049810070306      *
049820070306e   1c                   endsl
049830061102      *
049840071114      * > Raccogliere Codice Fiscale e/o Partita Iva di ...
049850071114if  1c                   if             $No_CF = *on
049860071114     c                             or   $No_PI = *on
049870070320     c                   eval      §LO1   = §PS1 + 0.050
049880070320     c                   write     A4prtLO
049890070320     c                   eval      §PS1   = §LO1
049900070321     c                   eval      $First = 'G'
049910061102     c                   eval      Par5   = 'Spedizione n°'
049920061102     c                                    + %editw(DB0lnp:'0   ')
049930061102     c                                    + %editw(DB0nrs:'0  ')
049940061102     c                                    + ' '
049950061102     c                                    + %editc(DB0nsp:'Z')
049960061102     c                                    + ' del '
049970061102     c                                    + %editc(Pdsp:'Y')
049980061102     c                   exsr      StampaAR5
049990071114if  2c                   if        $No_PI = *on
050000071114     c*** (già fatto:)   eval      $First = 'G'
050010071114     c                   eval      Par5   = 'raccogliere +
050020071114     c                                      Partita I.v.a. di "'
050030071114     c                                    + %trim(wRSxCF)
050040071114     c                                    + '"'
050050071114     c                   exsr      StampaAR5
050060120504if  3c                   if        $No_CF = *off
050070120504     c*** (già fatto:)   eval      $First = 'G'
050080120504     c                   eval      Par5   = 'Codice Fiscale di "'
050090120504     c                                    + %trim(wRSxCF)
050100120504     c                                    + '": '
050110120504     c                                    + wB0cfi
050120120504     c                   exsr      StampaAR5
050130120504e   3c                   endif
050140071114e   2c                   endif
050150120503if  2c                   if        $No_CF = *on
050160120503     c*** (già fatto:)   eval      $First = 'G'
050170120503     c                   eval      Par5   = 'raccogliere +
050180120503     c                                      Codice Fiscale di "'
050190120503     c                                    + %trim(wRSxCF)
050200120503     c                                    + '"'
050210120503     c                   exsr      StampaAR5
050220120503e   2c                   endif
050230070306e   1c                   endif
050240070306      *
050250070306      * > Note bolla (dal file FIARN00F)
050260070306      *   (posizionamento e 1ª lettura già effettuate nella subr.
050270070306      *    "CheckFIARN")
050280070306if  1c                   if        $FIARN = *on
050290070320     c                   eval      §LO1   = §PS1 + 0.050
050300070320     c                   write     A4prtLO
050310070320     c                   eval      §PS1   = §LO1
050320070321     c                   eval      $First = 'G'
050330070319do  2c                   dou       %eof(FIARN11L)
050340070306     c                   eval      Par5   = ARNnob
050350070306     c                   exsr      StampaAR5
050360070319     c     K06ARN11      reade     FIARN000
050370070306e   2c                   enddo
050380070306e   1c                   endif
050390070319      *
050400070319      * > Consegne parziali precedenti (dal file FNART00F)
050410070406if  1c                   IF        $ConsParz     =  *on
050420101130     c*//(già fatto)?   exsr      SchConsParz
050430070320     c                   eval      §LO1   = §PS1 + 0.050
050440070320     c                   write     A4prtLO
050450070320     c                   eval      §PS1   = §LO1
050460070321     c                   eval      $First = 'G'
050470070319     c                   eval      xx     = 1
050480070319do  2c                   DoW            (xx      <= %elem($CP)
050490070319     c                             and  $CP(xx)  <  *all'9')
050500070319     c                   movel     $CP(xx)       Elem_CP
050510070319     c                   z-add     WCPdcm        w0080
050520070319     c     *iso          test(d)                 w0080                  31
050530070319if  3c                   if        NOT *in31
050540071025     c     *iso          movel     w0080         Data_EUR
050550071025     c     *eur          movel     Data_EUR      w0080
050560070319x   3c                   else
050570070319     c                   clear                   w0080
050580070319e   3c                   endif
050590140304 *...+*...1....+....2....+....3....+....4....+....5....+....6....+....7....+..
050600140305 Conse*nati colli 12345 in data 12/34/5678
050610140304 Conse*nati colli 12345 in data 12/34/5678 alle ore 12:34 - Firmatario: 123456
050620140304 Conse*nati colli 12345 il 12/34/5678 alle 12:34 a 1234567890123456789012345
050630140305 Conse*nati colli 12345 in data 12/34/5678 alle ore 12:34
050640140305      *                 Firmatario: 123456789012345678901234567890123456789012
050650070321     c                   eval      Par5   = 'Consegnati colli '
050660140305     c                                    + %editc(WCPncl:'Z')
050670140305     c                                    + ' in data '
050680070319     c                                    + %editc(w0080:'Y')
050690140304     c                   if        WCPocm > *zero
050700140304     c                   eval      Par5   = %trimr( Par5 )
050710140305     c                                    + ' alle ore '
050720140303     c                                    + %editw(WCPocm : '0 :  ')
050730140304     c                   endif
050740070319     c                   exsr      StampaAR5
050750140305     c                   if        WCPfir <> *blank
050760140305     c                   clear                   Par5
050770140305     c                   eval      %subst( Par5 : 24 ) = 'Firmatario: '
050780140305     c                                    + WCPfir
050790140305     c                   exsr      StampaAR5
050800140305     c                   endif
050810070319     c                   eval      xx     = xx + 1
050820070319e   2c                   EndDo
050830070319e   1c                   ENDIF
050840070321      *
050850070321      * > Reperita Comunicazione Anomalia
050860070321if  1c                   if        $C_A_  = *on
050870070321     c                   eval      §LO1   = §PS1 + 0.050
050880070321     c                   write     A4prtLO
050890070321     c                   eval      §PS1   = §LO1
050900070321     c                   eval      $First = 'G'
050910070321     c                   eval      Par5   = 'Presente Comunicazione +
050920070321     c                                                Anomalia'
050930070321     c                   exsr      StampaAR5
050940070321e   1c                   endif
050950031124      *
050960031124      * - stampo box
050970070320      *   (§PB1 è da reimpostare in §LO1: contiene il suo valore PRIMA
050980070320      *   della stampa delle varie linee separatrici)
050990070315     c                   eval      §LO1   = §PB1
051000070315     c                   eval      §LO2   = §PS1 + 0.070
051010031124     c                   write     A4prtAR53
051020031124      *
051030031124     c                   ENDSR
051040090924      /free
051050090924
051060090924       //--------------------------------------------------------------
051070090924       //?Prepara elenco per box "Note per il destinatario"            ?
051080090924       //--------------------------------------------------------------
051090090924       BEGSR  StampaNxD;
051100090924
051110090924         // - Stampa testata
051120090924         //$First = *on;
051130090924         //Par5   = '                    '
051140090924         //       + '                    '
051150090924         //       + '          '
051160090924         //       + c_NxD;
051170090924         //exsr  StampaAR5;
051180090924
051190090924         // - Memorizzazione posizione iniziale per il box
051200090924         //   (§PB1 sarà da reimpostare in §LO1 PRIMA della stampa del box)
051210090924         //§PB1 = §LO1;
051220090924
051230090924         // - Stampa della riga separatrice iniziale
051240090924         //§LO1   = §PS1 + 0.050;
051250090924         //write  A4prtLO;
051260090924         //§PS1   = §LO1;
051270090924
051280090924         // - Stampa la singola riga di testo
051290090925         $First = '1';
051300090924         Par5 = 'DECODER DIGITALE TERRESTRE OMAGGIO DA SKY PER AVER +
051310090924                 ADERITO ALL''OFFERTA';
051320090924         exsr  StampaAR5;
051330090924
051340090924         // - Memorizzazione posizione iniziale per il box
051350090924         //   (§PB1 sarà da reimpostare in §LO1 PRIMA della stampa del box)
051360090924         §PB1 = §LO1;
051370090924
051380090924         // - Stampa box
051390090924         //   (§PB1 è da reimpostare in §LO1: contiene il suo valore PRIMA
051400090924         //   della stampa delle varie linee separatrici)
051410090924         §LO1 = §PB1;
051420090925         §LO2 = §PS1 + 0.050;
051430090924         write  A4prtAR53;
051440090924
051450090924       ENDSR;
051460090924
051470090924      /end-free
051480030515      *
051490030515      *---------------------------------------------------------------*
051500050217      *?Prepara elenco per box - "Bancali da rendere al mittente"    ?*
051510150224      *?                         » NON PIÙ «                         ?*
051520050217      *?                       - "Dati per destinatario supermercato"?*
051530150224      *?                         » NON PIÙ «                         ?*
051540050217      *?                       - "Promemoria per autotrasportatore"  ?*
051550030515      *---------------------------------------------------------------*
051560030515     c     ListaAR5      BEGSR
051570030515      *
051580060921     c                   eval      $FIAR5 = *on
051590030515      *
051600030515      * Ripulisco i campi utilizzati in seguito
051610030515     c                   clear                   AR5trd
051620030515     c                   clear                   AR5dac
051630150304     c*//                clear                   dAR5ban
051640030717     c                   clear                   dAR5bnb
051650030903     c                   clear                   dAR5rcc
051660030515     c                   clear                   dAR5scr
051670030515     c                   reset                   $First
051680030515     c                   reset                   $FirstSCR
051690030515     c                   reset                   SAVtrd
051700030515      *
051710030515      * Posizionamento iniziale
051720040301if  1c                   if        DUTlpo = 'S'
051730040531     c     KLDV          setgt     FIAR531C
051740040301x   1c                   else
051750040607     c     KLDV          setgt     FIAR5000
051760040301e   1c                   endif
051770030515      *
051780030515      * Ciclo di lettura del file FIAR5
051790030515      *
051800030515do  1c                   DO        *Hival
051810030515      *
051820040607if  2c                   if        DUTlpo = 'S'
051830040531     c     KLDV          readpe    FIAR531C
051840040607x   2c                   else
051850040607     c     KLDV          readpe    FIAR5000
051860040607e   2c                   endif
051870040301      *
051880040301if  2c                   if           (DUTlpo =  'S'
051890040531     c                             and %eof(FIAR531C)
051900040301     c                             OR  DUTlpo <> 'S'
051910040301     c                             and %eof(FIAR501L))
051920030515     c                   leave
051930030515e   2c                   endif
051940030515if  2c                   if        AR5atb <> *blanks
051950030515     c                   iter
051960030515e   2c                   endif
051970030515      *
051980030515      * Controllo se devo stampare l'intestazione al nuovo box
051990030515if  2c                   if        AR5trd <> SAVtrd
052000030515      *   Stampa del precedente box (se stampata almeno una riga)
052010030910if  3c                   if             SAVtrd <> *hival
052020030910     c                             and  $First =  *off
052030030911     c                   eval      §LO2   = §PS1 + 0.050
052040030515     c                   write     A4prtAR53
052050030515e   3c                   endif
052060030515     c                   eval      $First =  *on
052070030515     c                   eval      SAVtrd =  AR5trd
052080030515e   2c                   endif
052090030515      *
052100150224sel 2c                   SELECT
052110150224      *//*
052120150224      *//* > Bancali da rendere al mittente
052130150224      *//*   (unico record, sequenzialmente prima dei rec. "SCR")
052140150224w   2c*//                WHEN      %subst(DB0gva:1:1) = 'B'
052150150224     c*//                          and    AR5trd      = 'BAN'
052160150224     c*//                movel     AR5uni        dAR5ban
052170150224      *//* - stampo testata
052180150224if  3c*//                if           (§AR5tba <> *zeros
052190150224     c*//                          or  §AR5tb2 <> *zeros)
052200150224     c*//                eval      Par5   = 'BANCALI DA RIPORTARE A MAGAZZINO E'
052210150224     c*//                                 + ' DA RENDERE AL MITTENTE           '
052220150224     c*//                exsr      StampaAR5
052230150224e   3c*//                endif
052240150224      *//* - stampo 1ª riga
052250150224if  3c*//                if        §AR5tba <> *zeros
052260150224     c*//                clear                   Par5
052270150224     c*//                clear                   Wtba
052280150224     c*//                eval      Wtba   = §AR5tba
052290150224     c*//                exsr      Tab_TBA
052300150224     c*//                eval      Par5   = §TBAdesc + 'N. '
052310150224     c*//                                 + %editc(§AR5nba : 'Z')
052320150224     c*//                exsr      StampaAR5
052330150224e   3c*//                endif
052340150224      *//* - stampo 2ª riga
052350150224if  3c*//                if        §AR5tb2 <> *zeros
052360150224     c*//                clear                   Par5
052370150224     c*//                clear                   Wtba
052380150224     c*//                eval      Wtba   = §AR5tb2
052390150224     c*//                exsr      Tab_TBA
052400150224     c*//                eval      Par5   = §TBAdesc + 'N. '
052410150224     c*//                                 + %editc(§AR5nb2 : 'Z')
052420150224     c*//                exsr      StampaAR5
052430150224e   3c*//                endif
052440150224      *//*
052450150224      *//* > Bancali da riportare a magazzino ("Oltre 20 colli")
052460150224      *//*   (unico record, sequenzialmente prima dei rec. "SCR",
052470150224      *//*    alternativo a "BAN" = bancali da rendere al mittente).
052480150224w   2c*//                WHEN      %subst(DB0gva:1:1) = 'O'
052490150224     c*//                          and    AR5trd      = 'BNB'
052500150224     c*//                movel     AR5uni        dAR5bnb
052510150224      *//* - stampo testata
052520150224if  3c*//                if           (§AR5btba <> *zeros
052530150224     c*//                          or  §AR5btb2 <> *zeros)
052540150224     c*//                eval      Par5   = 'BANCALI DA RIPORTARE A MAGAZZINO  '
052550150224     c*//                exsr      StampaAR5
052560150224e   3c*//                endif
052570150224      *//* - stampo 1ª riga
052580150224if  3c*//                if        §AR5btba <> *zeros
052590150224     c*//                clear                   Par5
052600150224     c*//                clear                   Wtba
052610150224     c*//                eval      Wtba   = §AR5btba
052620150224     c*//                exsr      Tab_TBA
052630150224     c*//                eval      Par5   = §TBAdesc + 'N. '
052640150224     c*//                                 + %editc(§AR5bnba : 'Z')
052650150224     c*//                exsr      StampaAR5
052660150224e   3c*//                endif
052670150224      *//* - stampo 2ª riga
052680150224if  3c*//                if        §AR5btb2 <> *zeros
052690150224     c*//                clear                   Par5
052700150224     c*//                clear                   Wtba
052710150224     c*//                eval      Wtba   = §AR5btb2
052720150224     c*//                exsr      Tab_TBA
052730150224     c*//                eval      Par5   = §TBAdesc + 'N. '
052740150224     c*//                                 + %editc(§AR5bnb2 : 'Z')
052750150224     c*//                exsr      StampaAR5
052760150224e   3c*//                endif
052770150224      *//* - stampo 3ª riga
052780150224if  3c*//                if        §AR5bcor <> *zeros
052790150224     c*//                clear                   Par5
052800150224     c*//                eval      Par5   = C_CoOr
052810150224     c*//                                 + %editc(§AR5bcor : 'K')
052820150224     c*//                exsr      StampaAR5
052830150224e   3c*//                endif
052840030515      *
052850030515      * > Destinatario supermercato
052860030515w   2c                   WHEN      (DB0tc1 = 'S' or DB0tc2 = 'S')
052870030515     c                             and AR5trd  = 'SCR'
052880030515     c                   movel     AR5uni        dAR5scr
052890030515      * - stampo testata
052900030515      *   (se subr. richiamata solo per verifica esistenza dati: esco)
052910030515if  3c                   if        $First =  *on
052920031124     c                   eval      Par5   = C_DeSM
052930030515     c                   exsr      StampaAR5
052940030515     c                   eval      $FirstSCR  = *off
052950030515e   3c                   endif
052960030515      * - stampo righe
052970030515     c                   clear                   Par5
052980030624     c                   eval      *in38 = *on
052990030515if  3c                   if           AR5dac <> SAVdac
053000030515     c                             or AR5orc <> SAVorc
053010030515     c                   eval      SAVdac = AR5dac
053020030515     c                   eval      SAVorc = AR5orc
053030030515      * - - giro data modifica
053040030515     c                   z-add     AR5dac        w0080
053050070315     c     *iso          test(d)                 w0080                  31
053060070315if  4c                   if        NOT *in31
053070071025     c     *iso          movel     w0080         Data_EUR
053080071025     c     *eur          movel     Data_EUR      w0080
053090070315x   4c                   else
053100070315     c                   clear                   w0080
053110070315e   4c                   endif
053120030515     c                   eval      Par5 = 'Modifica del '
053130030515     c                                  + %editc(w0080 : 'Y')
053140030515     c                                  + ' '
053150030515     c                                  + %editw(AR5orc : '  :  :  ')
053160030515     c                                  + ' '
053170040223     c                                  + §AR5Spru
053180030515     c                   exsr      StampaAR5
053190030515e   3c                   endif
053200040223     c                   eval      Par5 = §AR5Snom
053210030911     c                                  + '   Tel.: '
053220040223     c                                  + §AR5Stel
053230030911     c                   exsr      StampaAR5
053240040223     c                   eval      Par5 = §AR5Smot
053250030515     c                   exsr      StampaAR5
053260030624     c                   eval      *in38 = *off
053270031124      *
053280031124      * > Se destinatario assente inserire il plico in buca lettere
053290031124      *   (già stampato in box "Promemoria per autotrasportatore")
053300031124w   2c***                WHEN          DB0gma  = Ktab7V_CB
053310031124     c***                ...       ...
053320030903      *
053330030903      * > O.R.M. per ritiro contestuale alla consegna
053340030903      *   (unico record)
053350050510      *   Verrà stampato come ultimo box
053360050510      *   - prima del (eventuale) DDT del mittente
053370050510      *   - prima della copia per il destinatario
053380030910w   2c***                WHEN          DB0gma  = Ktab7V_RC
053390030910     c***                          and AR5trd  = 'RCC'
053400030903     c***                ...       ...
053410030515      *
053420030515e   2c                   ENDSL
053430030515      *
053440030515e   1c                   ENDDO
053450030515      *
053460030515      * Stampa del (ultimo) box (se stampata almeno una riga)
053470030515if  1c                   if        $First = *off
053480030911     c                   eval      §LO2   = §PS1 + 0.050
053490030515     c                   write     A4prtAR53
053500030515e   1c                   endif
053510030515      *
053520030515      * > Destinatario supermercato
053530030515      *   (stampa del box vuoto per memo se non già stampata almeno
053540030515      *   una riga)
053550030515if  1c                   IF        (DB0tc1 = 'S' or DB0tc2 = 'S')
053560030515     c                             and              $FirstSCR = *on
053570030515      * - stampo testata
053580030515     c                   eval      $First = *on
053590031124     c                   eval      Par5   = C_DeSM
053600030515     c                   exsr      StampaAR5
053610030515     c                   eval      $FirstSCR  = *off
053620030515      * - stampo 2 righe vuote
053630030515     c                   clear                   Par5
053640030515do  2c                   do        2
053650030515     c                   exsr      StampaAR5
053660030515e   2c                   enddo
053670030515      * - stampo box
053680030911     c                   eval      §LO2   = §PS1 + 0.050
053690030515     c                   write     A4prtAR53
053700030515e   1c                   ENDIF
053710070123      *
053720070123     c                   eval      $FIAR5 = *off
053730030515      *
053740030515     c                   ENDSR
053750030515      *
053760030515      *---------------------------------------------------------------*
053770050217      *?Stampa singola riga relativa a FIAR5                         ?*
053780030515      *---------------------------------------------------------------*
053790030515     c     StampaAR5     BEGSR
053800030515      *
053810030515      * Controllo se superata l'ultima riga
053820030515     c                   exsr      Test_OF
053830030515      *
053840030515      * Stampa del singolo record
053850030909sel 1c                   select
053860030909      *
053870030909w   1c                   when      $First = *off
053880030909      * - Incremento parametri per posizionamento nuova riga
053890030911     c                   eval      §PS1   = §PS1 + 0.150
053900030909     c                   write     A4prtAR52
053910030909      *
053920030909w   1c                   when      $First = *on
053930030515      * - Incremento parametri per posizionamento nuovo box
053940030515      *   e relativa intestazione
053950030911     c                   eval      §LO1   = §LO2 + 0.150
053960030911     c                   eval      §PS1   = §LO1 + 0.150
053970030515     c                   write     A4prtAR51
053980030515     c                   eval      $First = *off
053990060524      *
054000060524w   1c                   when      $First = '2'
054010060524      * - Incremento parametri per posizionamento nuova riga
054020060524      *   nel box "Promemoria per autotrasportatore"
054030060524     c                   eval      §PS1   = §PS1 + 0.150
054040060524     c                   write     A4prtAR51
054050060524     c                   eval      $First = *off
054060030909      *
054070030909w   1c                   when      $First = 'A'
054080030909      * - Incremento parametri per posizionamento nuovo box
054090030909      *   e relativa intestazione
054100030911     c                   eval      §LO1   = §LO2 + 0.150
054110030909     c                   eval      §PS1   = §LO1 + 0.200
054120030909     c                   write     A4prtAR5a
054130030909     c                   eval      $First = *off
054140150226      *//*
054150150226w   1c*//                when      $First = 'B'
054160150226      *//* - Incremento parametri per posizionamento nuovo box
054170150226      *//*   ed intestazione "Promemoria per autotrasportatore"
054180150226     c*//                eval      §LO1   = §LO2 + 0.150
054190150226     c*//                eval      §PS1   = §LO1 + 0.125
054200150226     c*//                write     A4prtAR5b
054210150226     c*//                eval      $First = 'C'
054220150226      *//*
054230150226w   1c*//                when      $First = 'C'
054240150226      *//* - Incremento parametri per posizionamento nuova riga
054250150226      *//*   nel box "Promemoria per autotrasportatore"
054260150226     c*//                eval      §PS1   = §PS1 + 0.250
054270150226     c*//                write     A4prtAR5c
054280070321      *
054290070321w   1c                   when      $First = 'G'
054300070321      * - Incremento parametri per posizionamento nuova riga
054310070321      *   nel box "Promemoria per autotrasportatore"
054320070321     c                   eval      §PS1   = §PS1 + 0.170
054330070321     c                   write     A4prtAR5g
054340030909      *
054350030909e   1c                   endsl
054360030515      *
054370030515     c                   ENDSR
054380030910      *
054390030910      *---------------------------------------------------------------*
054400050217      *?Stampa box "Ritiro Contestuale alla Consegna" da FIAR5/RCC   ?*
054410030910      *---------------------------------------------------------------*
054420030910     c     StampaRCC     BEGSR
054430030910      *
054440030910      * > O.R.M. per ritiro contestuale alla consegna
054450030910      *   (unico record)
054460031124      *   Verrà stampato come ultimo box (prima della copia per il
054470031124      *     destinatario)
054480030910      *
054490030910     c                   clear                   dAR5rcc
054500030910      *
054510030910     c                   eval      AR5trd = 'RCC'
054520040301if  1c                   if        DUTlpo = 'S'
054530040531     c     K05AR501      chain     FIAR531C
054540040301x   1c                   else
054550030910     c     K05AR501      chain     FIAR5000
054560040301e   1c                   endif
054570040301      *
054580071008if  1c                   if           (DUTlpo =  'S'
054590040531     c                             and %found(FIAR531C)
054600040301     c                             OR  DUTlpo <> 'S'
054610040301     c                             and %found(FIAR501L))
054620040301     c                             and AR5atb =  *blanks
054630030910     c                   movel     AR5uni        dAR5rcc
054640071015     c                   eval      ORMpoe = §AR5Rpoe
054650071015     c                   eval      ORMnsr = §AR5Rnsr
054660071015     c                   eval      ORMnor = §AR5Rnor
054670071015     c                   eval      ORMnrv = §AR5Rnrv
054680071015     c                   eval      Pbt2a  = 'RITIRO CONTESTUALE ALLA +
054690071015     c                                       CONSEGNA'
054700071015     c                   exsr      StampaORM
054710071015e   1c                   endif
054720071015      *
054730071015     c                   ENDSR
054740071015      *
054750071015      *---------------------------------------------------------------*
054760071015      *?Stampa box "Ritiro Porto Franco PrePagato"                   ?*
054770071015      *---------------------------------------------------------------*
054780071015     c     StampaRPFPP   BEGSR
054790071015      *
054800071015      * > O.R.M. per ritiro porto franco prepagato
054810071015      *   Verrà stampato come ultimo box (prima della copia per il
054820071015      *     destinatario)
054830071015      *
054840071015     c*** già fatto:     clear                   dsBL4M
054850071015     c*** già fatto:     exsr      RepAR4_M
054860071015      *
054870071015if  1c                   if             §B4poe  > *zeros
054880071015     c                             and  §B4nor  > *zeros
054890071015     c                   eval      ORMpoe = §B4poe
054900071015     c                   eval      ORMnsr = §B4nsr
054910071015     c                   eval      ORMnor = §B4nor
054920071015     c                   eval      ORMnrv = §B4nrv
054930071015     c                   eval      Pbt2a  = 'RITIRO PORTO FRANCO +
054940071015     c                                       PREPAGATO'
054950071015     c                   exsr      StampaORM
054960071015e   1c                   endif
054970071015      *
054980071015     c                   ENDSR
054990071015      *
055000071015      *---------------------------------------------------------------*
055010071015      *?Stampa dati O.R.M. in box "Ritiro Contestuale alla Consegna" ?*
055020071015      *?              o in quello "Ritiro Porto Franco Prepagato"    ?*
055030071015      *---------------------------------------------------------------*
055040071015     c     StampaORM     BEGSR
055050071015      *
055060071015     c                   eval      $FIAR5 = *on
055070071015      *
055080071015if  2c                   if        not %open(FNORM01L)
055090071015     c                   open      FNORM01L
055100071015e   2c                   endif
055110071015     c     K04ORM01      chain     FNORM000
055120071015      *
055130071015if  2c                   IF        %found(FNORM01L)
055140071015      *
055150071015      * Reperisco la posizione (in pollici) in cui posizionarmi
055160071015      *   per la stampa in base all'ultima riga stampata
055170071015      *   se non ho ancora stampato box
055180071015cas 3c     §LO2          caseq     *zeros        Rep_§LO2
055190071015e   3c                   endcs
055200071025      *
055210071015      * - stampo testata
055220071025      *   (con il n° O.R.M.)
055230071015     c                   eval      $First = 'A'
055240071015     c*** già impostato: eval      Pbt2a  = 'RITIRO CONTESTUALE ALLA +
055250071015     c***  "      "                          CONSEGNA'
055260071015     c                   eval      Pbt2b  = 'O.R.M. numero '
055270071015     c                                    + %editw(ORMpoe : '0   ')
055280071015     c                                    + ' '
055290071015     c                                    + %editw(ORMnsr : '0  ')
055300071015     c                                    + '  '
055310071015     c                                    + %editc(ORMnor : 'Z')
055320071015     c                                    + ' '
055330071015     c                                    + %editw(ORMnrv : '0  ')
055340071015     c                   exsr      StampaAR5
055350071025      *
055360071015      * - stampo i dati dell'O.R.M.
055370071015     c                   exsr      QUM_FnOrm
055380140410     c                   eval      Par5   = 'Quantità      : '
055390071015     c                                    + %trim($Qum(1)) + '    '
055400071015     c                                    + %trim($Qum(2)) + '    '
055410071015     c                                    + %trim($Qum(3))
055420071015     c                   exsr      StampaAR5
055430080228if  3c                   if        DB0tbo = '8'
055440140410     c                   eval      Par5   = 'Mittente      : '
055450080228     c                                    + ORMrsr
055460080228x   3c                   else
055470140410     c                   eval      Par5   = 'Destinatario  : '
055480071015     c                                    + ORMrsc
055490080228e   3c                   endif
055500071015     c                   exsr      StampaAR5
055510080228if  3c                   if        DB0tbo = '8'
055520140410     c                   eval      Par5   = '                '
055530080228     c                                    + ORMinr
055540080228x   3c                   else
055550140410     c                   eval      Par5   = '                '
055560071015     c                                    + ORMinc
055570080228e   3c                   endif
055580071015     c                   exsr      StampaAR5
055590080228if  3c                   if        DB0tbo = '8'
055600140410     c                   eval      Par5   = '                '
055610080228     c                                    + ORMcar + ' '
055620080228     c                                    + ORMlor + ' '
055630080228     c                                    + ORMprr + ' '
055640080228     c                                    + ORMnar
055650080228x   3c                   else
055660140410     c                   eval      Par5   = '                '
055670071015     c                                    + ORMcac + ' '
055680071015     c                                    + ORMloc + ' '
055690071015     c                                    + ORMprc + ' '
055700071015     c                                    + ORMnac
055710080228e   3c                   endif
055720071015     c                   exsr      StampaAR5
055730101022             // -?SE Ritiro Porto Franco Prepagato:?
055740080228if  3c                   if        DB0tbo = '8'
055750101022      /free
055760101022               // -?stampo il referente ed il relativo telefono?
055770101022               // -?(Comunque! Ci sono sempre!)?
055780140410               Par5   = 'Referente     : ' + ORMrer + '   '
055790101022                      + 'Telefono: '     + ORMter;
055800101022               exsr  StampaAR5;
055810101022      /end-free
055820140410     c                   eval      Par5   = 'Ordinante     : '
055830080228     c                                    + ORMrso
055840080228     c                   exsr      StampaAR5
055850140410     c                   eval      Par5   = 'Destinatario  : '
055860080303     c                                    + ORMrsc
055870080303     c                   exsr      StampaAR5
055880080228e   3c                   endif
055890071015      * - stampo le note
055900071015      *   (Comunque! Non solo "se ci sono". Al limite: scarto la 1ª.)
055910140410     c                   eval      Par5   = 'Note          : '
055920071015if  3c                   if        ORMno1 <> *blanks
055930071015     c                   eval      Par5   = %trim(Par5)  + ' '
055940071015     c                                    + ORMno1 + ' ' + ORMno2
055950071015x   3c                   else
055960071015     c                   eval      Par5   = %trim(Par5)  + ' '
055970071015     c                                    + ORMno2
055980071015e   3c                   endif
055990071015     c                   exsr      StampaAR5
056000071025      *
056010140410      * -?SE Ritiro Porto Franco Prepagato:?
056020071025if  3c                   IF        DB0tbo = '8'
056030140410      *
056040140410      * - stampo orari apertura (SE ci sono)
056050140410     c                   exsr      sr_OrariApert
056060140410      *
056070140410      * - stampo data & ora ritiro
056080071025     c                   z-add     ORMdar        w0080
056090071025     c     *iso          test(d)                 w0080                  31
056100071025if  4c                   if        NOT *in31
056110071025     c     *iso          movel     w0080         Data_EUR
056120071025     c     *eur          movel     Data_EUR      w0080
056130071025x   4c                   else
056140071025     c                   clear                   w0080
056150071025e   4c                   endif
056160140410     c                   eval      Par5   = 'Data ritiro   : '
056170071025     c                                    + %editc(w0080:'Y')
056180071025if  4c                   if        ORMorr > 1230
056190071025     c                   eval      Par5   = %trimr(Par5)
056200071025     c                                    + '          '
056210140410     c                                    + 'Ora ritiro: '
056220071025     c                                    + %editw(ORMorr:'0 :  ')
056230071025     c                   exsr      StampaAR5
056240071025x   4c                   else
056250071025     c                   exsr      StampaAR5
056260071025     c                   clear                   Par5
056270071025     c                   eval      %subst(Par5:50)
056280071025     c                                    = 'Ora ritiro: '
056290071025     c                                    + %editw(ORMorr:'0 :  ')
056300071025     c                   write     A4prtAR51
056310071025e   4c                   endif
056320140410      *
056330071025e   3c                   ENDIF
056340071025      *
056350071015      * - imposto codice a barre O.R.M.
056360071015     c                   movel     BarCodeORM    Pbco
056370071015      * - verifico se codice a barre da stampare
056380071015     c                   eval      *in52  =  (Pbco <> *zeros)
056390071025      *
056400071015      * - stampo box
056410071015     c                   eval      §LO2   = §PS1 + 0.050
056420071015if  3c                   if        NOT *in52
056430071015     c                   write     A4prtAR53
056440071015x   3c                   else
056450071025     c                   eval      §LV1   = §LO2 - §LO1
056460071015     c                   eval      §PS2   = §LO2 - 0.500
056470071015     c                   write     A4prtAR54
056480071015e   3c                   endif
056490071015      *
056500071015e   2c                   ENDIF
056510071015      *
056520071015     c                   eval      $FIAR5 = *off
056530071015      *
056540071015     c                   ENDSR
056550100513      /free
056560140410
056570140410       //--------------------------------------------------------------
056580140410       //?Reperimento, impostazione e stampa degli Orari di Apertura   ?
056590140410       //--------------------------------------------------------------
056600140410       BEGSR  sr_OrariApert;
056610140410
056620140410         clear  dOREorari;
056630140410
056640140410         // -?Recupero dati da estensione FNORE (trc "O ")?
056650140410         if  NOT  %open(FNORE01L);
056660140410           open  FNORE01L;
056670140410         endif;
056680140410         k_OREpoe = ORMpoe;
056690140410         k_OREnsr = ORMnsr;
056700140410         k_OREnor = ORMnor;
056710140410         k_OREnrv = ORMnrv;
056720140410         k_OREtrc = 'O ';
056730140410         chain  %kds(keyFNORE01)  FNORE000;
056740140410
056750140410         // -?...NON trovati => uscita dalla subroutine?
056760140410         if  Not  %found(FNORE01L);
056770140410           leavesr;
056780140410         endif;
056790140410
056800140410         // -?...Trovati => verifica degli orari?
056810140410         dOREorari = OREdati;
056820140410
056830140410         Select;
056840140410
056850140410           // -?C'è un solo orario "ALLE"?
056860140410           When  §OREoraMda = *zero  and  §OREoraMa > *zero  and
056870140410                 §OREoraPda = *zero  and  §OREoraPa = *zero;
056880140410             Par5 = 'Orari apertura: ' +
056890140410                    'Fino alle ' + %editw( §OREoraMa : ' 0:  ' );
056900140410           When  §OREoraMda = *zero  and  §OREoraMa = *zero  and
056910140410                 §OREoraPda = *zero  and  §OREoraPa > *zero;
056920140410             Par5 = 'Orari apertura: ' +
056930140410                    'Fino alle ' + %editw( §OREoraPa : ' 0:  ' );
056940140410
056950140410           // -?C'è un solo orario completo (per 1°)?
056960140410           When  §OREoraMda > *zero  and  §OREoraMa > *zero  and
056970140410                 §OREoraPda = *zero  and  §OREoraPa = *zero;
056980140410             Par5 = 'Orari apertura: ' +
056990140410                    %editw( §OREoraMda : ' 0:  ' ) + '  ' +
057000140410                    %editw( §OREoraMa  : ' 0:  ' );
057010140410
057020140410           // -?Ci sono due orari completi?
057030140410           When  §OREoraMda > *zero  and  §OREoraMa > *zero  and
057040140410                 §OREoraPda > *zero  and  §OREoraPa > *zero;
057050140410             Par5 = 'Orari apertura: ' +
057060140410                    %editw( §OREoraMda : ' 0:  ' ) + '  ' +
057070140410                    %editw( §OREoraMa  : ' 0:  ' ) + ' - ' +
057080140410                    %editw( §OREoraPda : ' 0:  ' ) + '  ' +
057090140410                    %editw( §OREoraPa  : ' 0:  ' );
057100140410
057110140410         EndSl;
057120140410
057130140410         exsr  StampaAR5;
057140140410
057150140410       ENDSR;
057160100513
057170100513       //--------------------------------------------------------------
057180100513       //?Verifica se presente ORM con preavviso mail                  ?
057190100513       //--------------------------------------------------------------
057200100513       BEGSR  sr_CheckORM_PrevvMail;
057210100514
057220100514         //clear  dOrm01               (già fatto)
057230100513
057240100513         // -?Reperimento num. ORM (se NON già reperito)?
057250100513         if  DB0tbo <> '8';
057260100513           exsr  RepAR4_M;
057270100513         endif;
057280100513
057290100513         // -?ORM inesistente: inutile proseguire?
057300100513         if  §B4poe = *zero    or   §B4nor = *zero;
057310100513           leavesr;
057320100513         endif;
057330100513
057340100513         // -?Verifica presenza O.R.M.?
057350100513         if  Not  %open(FNORM01L);
057360100513           open  FNORM01L;
057370100513         endif;
057380100513
057390100513         ORMpoe = §B4poe;
057400100513         ORMnsr = §B4nsr;
057410100513         ORMnor = §B4nor;
057420100513         ORMnrv = §B4nrv;
057430100513
057440100513         chain  K04ORM01  FNORM000;
057450100513
057460100513         // -?ORM non reperito: inutile proseguire?
057470100513         if  Not  %found(FNORM01L);
057480100513           leavesr;
057490100513         endif;
057500100513
057510100513         // -?Impostazione flag per stampa porto "ASSEGNATO"?
057520100513         dOrm01 = ORMflo;
057530100513         $PreavvORM = (§ORMpre = 'M');
057540100513
057550100513       ENDSR;
057560100513
057570100513      /end-free
057580030515      *
057590030515      *---------------------------------------------------------------*
057600050217      *?Stampa dati DPD                                              ?*
057610030515      *---------------------------------------------------------------*
057620030515     c     StampaDPD     BEGSR
057630030515      *
057640080922      * (EX) Bar-Code - non più stampato come codice a barre
057650030515      * (1' 12 char di Riferimento Mittente Alfanumerico - se numerici)
057660060626      *---
057670061030      * Il barcode DPD è stato variato ora può essere anche
057680061030      *   alfanumerico ed è lungo 14
057690061030      * Si trova sul file FIAR4 rcd 'I'
057700061030      * I controlli che faceva prima sono da cambiare
057710061030      *---
057720061030      *
057730060626      * nuovi controlli per nuovo barcode DPD
057740061030if  1c                   if        §B4Ipn <> *blanks
057750061030      * - controllo se vecchio o nuovo barcode
057760061030if  2c                   if        %subst(§B4Ipn:14:1) = *blanks
057770061030      * - - vecchio
057780061030     c                   eval      Pbcd2 = '01 ' + %subst(§B4Ipn:1:3) +
057790061030     c                                     ' '   + %subst(§B4Ipn:4:8)
057800061030      * - - nuovo
057810061030x   2c                   else
057820061030     c                   eval      Pbcd2 = '01 ' + %subst(§B4Ipn:1:4) +
057830061030     c                                     ' '   + %subst(§B4Ipn:5:10)
057840061030e   2c                   endif
057850061030e   1c                   endif
057860080922      /free
057870080922         // Impostazione data
057880080922         PdpA = 'Date ' + %editc(Pdsp:'Y');
057890080922
057900080922         // Impostazione numero colli
057910080922         PdpB = 'Parcel N. ' + %editc(Pncl:'Z');
057920080922
057930080922         // Impostazione numero parcel
057940080922         PdpC = §B4Ipn;
057950080922
057960080922         // Impostazione "costante" da stampare tra le Note
057970080922         PdpD = 'Controllare la corrispondenza';
057980080922         PdpE = 'del destinatario con l''indirizzo';
057990080922         PdpF = 'dell''etichetta DPD.';
058000080922
058010080922         // Impostaz. posizioni sulla copia per il vettore già stampata
058020080922         §DP1 = wPosIni + 1.100;
058030080922         §DP2 = §DP1 + 0.550;
058040080922         §DP3 = §DP2 + 0.120;
058050080922         §DP4 = §DP3 + 0.120;
058060080922         §DP5 = §DP4 + 0.120;
058070080922         §DP6 = §DP5 + 0.160;
058080080922         §DP7 = §DP6 + 0.130;
058090080922         §DP8 = §DP7 + 0.130;
058100080922      /end-free
058110080922      *
058120080922      * (inutile testare l'overflow perchè siamo in ex-A5)
058130080922     c                   write     A4prtDPD
058140030515      *
058150030515     c                   ENDSR
058160030515      *
058170030515      *---------------------------------------------------------------*
058180050217      *?Prepara elenco per box - Elenco "Package Tracking Number"    ?*
058190030515      *---------------------------------------------------------------*
058200030515     c     ListaFED      BEGSR
058210030515      *
058220030515      * Stampo il simbolo "FedEx" (sulla 1ª pagina):
058230030515      *   imposto le posizioni iniziali di stampa per le linee
058240030515      *   (orizzontali-§LOn e verticali-§LVn)
058250080912     c                   eval      §LO1   = 3.330
058260080912     c                   eval      §LO2   = §LO1 + 0.370
058270081023     c                   eval      §LV1   = §LO2 - 0.150
058280081023     c                   eval      §LV2   = §LO2 - 0.070
058290030515     c                   write     A4prtFED1
058300080818      *
058310080818      * Stampo le "costanti" di inizio lista
058320030515     c                   write     A4prtFED2
058330030515      *
058340030515      * Stampo elenco segnacolli FedEx
058350030709     c                   eval      §LO1   = §LO2 - 0.250
058360030515      * - per le bolle in partenza
058370030515if  1c                   IF           DB0tbo = 'P'
058380030515     c                             or DB0tbo = '8'
058390030515if  2c                   if        NOT %open(FNBLT01L)
058400030515     c                   open      FNBLT01L
058410030515e   2c                   endif
058420030515      * - per le bolle in arrivo
058430030515x   1c                   ELSE
058440070319if  2c                   if        NOT %open(FNART01L)
058450070319     c                   open      FNART01L
058460070319e   2c                   endif
058470030515e   1c                   ENDIF
058480070319if  1c                   if        NOT %open(FIARS01L)
058490070319     c                   open      FIARS01L
058500070319e   1c                   endif
058510030515     c                   eval      Wtrc = 'C'
058520030515     c                   clear                   Wnsc
058530030515     c                   clear                   Wawb
058540030515do  1c                   dou       Wawb = *blanks
058550030515     c                   exsr      StampaFED
058560030515e   1c                   enddo
058570051020      *
058580030515     c                   ENDSR
058590030515      *
058600030515      *---------------------------------------------------------------*
058610050217      *?Stampa singola riga relativa a FEDEX                         ?*
058620030515      *---------------------------------------------------------------*
058630030515     c     StampaFED     BEGSR
058640030515      *
058650030515      * Impostazione campi di stampa
058660030515     c                   clear                   Wawb
058670030515      * - Carico AWB FedEx
058680030515      * - - leggo segnacolli per bolle in partenza
058690030515if  1c                   IF           DB0tbo = 'P'
058700030515     c                             or DB0tbo = '8'
058710030515     c                   exsr      CarAWB_Blx
058720030515      * - - leggo segnacolli per bolle in arrivo
058730030515x   1c                   ELSE
058740030515     c                   exsr      CarAWB_Arx
058750030515e   1c                   ENDIF
058760030515     c     wAWB          cabeq     *blanks       End_FED
058770030515     c                   move      wAWB          Pptn
058780030515     c                   add       1             Psts
058790030515      *
058800030515      * Controllo se superata l'ultima riga
058810030515     c                   exsr      Test_OF
058820030709      *
058830030709      * Imposto le posizioni iniziali di stampa per le linee
058840030709      *   (orizzontali-§LOn e verticali-§LVn) e per i campi (§PSn)
058850030709     c                   eval      §LO1 = §LO1 + 0.250 - 0.008
058860030709     c                   eval      §LO2 = §LO1 + 0.250
058870030709     c                   eval      §LV1 = §LO1
058880030709     c                   eval      §LV2 = §LO2 - 0.100
058890030709     c                   eval      §PS1 = §LO1 + 0.200
058900030515      *
058910030515      * Stampa del singolo record
058920030515     c                   write     A4prtFED3
058930030515      *
058940030515     c     End_FED       ENDSR
058950030515      *
058960030515      *---------------------------------------------------------------*
058970051020      *?Reperimento AWB FedEx da file FNBLT e FIARS (bolle partenza) ?*
058980030515      *---------------------------------------------------------------*
058990030515     c     CarAWB_Blx    BEGSR
059000130215      *
059010130215      * Verifica se, al di là del codice bolla (vedi "$ "), esiste
059020130215      *   comunque una bolla mamma
059030130215     c                   exsr      sr_Mamma_FedEx
059040030515      *
059050030515      * Posizionamento
059060130215if  2c                   if        $Mamma = *off   and
059070130215     c                             $Mamma_FedEx = *off
059080040607     c     KLDVnsc       setgt     FNBLT000
059090070514x   2c                   else
059100070514     c     KLBLnsc       setgt     FNBLT000
059110070514e   2c                   endif
059120030515      *
059130030515      * Ciclo di lettura
059140030515do  1c                   DO        *hival
059150030515      *
059160130215if  2c                   if        $Mamma = *off   and
059170130215     c                             $Mamma_FedEx = *off
059180040607     c     KLDV          reade     FNBLT000
059190070514x   2c                   else
059200070514     c     KLBL          reade     FNBLT000
059210070514e   2c                   endif
059220030515if  2c                   if        %eof(FNBLT01L)
059230030515     c                   leave
059240030515e   2c                   endif
059250030515      * - per ogni segnacollo cerco l'AWB
059260060505     c     K05ARS01P     chain     FIARS000
059270060505if  2c                   if            %found(FIARS01L)
059280060505     c                             and %subst(ARSnot:1:1) >= *zeros
059290060505     c                   eval      wAWB = %subst(ARSnot:1:12)
059300060505     c                   eval      wNSC = ARSnsc
059310030515     c                   leave
059320030515e   2c                   endif
059330030515      *
059340030515e   1c                   ENDDO
059350030515      *
059360030515     c                   ENDSR
059370030515      *
059380030515      *---------------------------------------------------------------*
059390070319      *?Reperimento AWB FedEx da file FNART e FIARS (bolle arrivo)   ?*
059400030515      *---------------------------------------------------------------*
059410030515     c     CarAWB_Arx    BEGSR
059420130215      *
059430130215      * Verifica se, al di là del codice bolla (vedi "$ "), esiste
059440130215      *   comunque una bolla mamma
059450130215     c                   exsr      sr_Mamma_FedEx
059460030515      *
059470030515      * Posizionamento
059480130215if  2c                   if        $Mamma = *off   and
059490130215     c                             $Mamma_FedEx = *off
059500040607     c     KLDVnsc       setgt     FNART000
059510070514x   2c                   else
059520130215     c     KLBLnsc       setgt     FNART000
059530070514e   2c                   endif
059540030515      *
059550030515      * Ciclo di lettura
059560030515do  1c                   DO        *hival
059570030515      *
059580130215if  2c                   if        $Mamma = *off   and
059590130215     c                             $Mamma_FedEx = *off
059600040607     c     KLDV          reade     FNART000
059610070514x   2c                   else
059620070514     c     KLBL          reade     FNART000
059630070514e   2c                   endif
059640030515if  2c                   if        %eof(FNART01L)
059650030515     c                   leave
059660030515e   2c                   endif
059670030515      * - per ogni segnacollo cerco l'AWB
059680060505     c     K05ARS01A     chain     FIARS000
059690060505if  2c                   if            %found(FIARS01L)
059700030515     c                             and %subst(ARSnot:1:1) >= *zeros
059710030515     c                   eval      wAWB = %subst(ARSnot:1:12)
059720030515     c                   eval      wNSC = ARSnsc
059730030515     c                   leave
059740030515e   2c                   endif
059750030515      *
059760030515e   1c                   ENDDO
059770030515      *
059780030515     c                   ENDSR
059790130215      *
059800130215      /free
059810130215
059820130215       //--------------------------------------------------------------
059830130215       //?Verifica se la spedizione FedEx ha bolla mamma.              ?
059840130215       //--------------------------------------------------------------
059850130215       BEGSR  sr_Mamma_FedEx;
059860130215
059870130215         //$Mamma_FedEx = *off;          (già così)
059880130215
059890130215         If  Not $Mamma;
059900130215
059910130215           // -?Verifica se la spedizione ha comunque mamma?
059920130215           //  ?(al di là del codice bolla - vedi cod. "$ ")?
059930130215           if  not %open(FNLBL01L);
059940130215             open  FNLBL01L;
059950130215           endif;
059960130215           chain  (DB0aas : DB0lnp : DB0nrs : DB0nsp)  FNLBL000;
059970130215
059980130215           $Mamma_FedEx = %found(FNLBL01L);
059990130215
060000130215         EndIf;
060010130215
060020130215       ENDSR;
060030130215
060040130215      /end-free
060050040607      *
060060040607      *---------------------------------------------------------------*
060070080918      *?Stampa DDT / PACKING-LIST del mittente                       ?*
060080040607      *---------------------------------------------------------------*
060090040607     c     StampaDDT     BEGSR
060100130315      *
060110130315      * - Uscita in assenza del file FIAR801L
060120130315     c                   if        Not  $E_AR8
060130130315     c                   leavesr
060140130315     c                   endif
060150080825      *
060160080825      * Meglio togliere l'impostazione relativa ai segnacolli non seq.
060170080825      *   per far funzionare il 1° posizionamento nella subr. Rep_§LO2
060180080825     c                   reset                   $A4_x_ScNS
060190080918      /free
060200080923         *in45 = (DB0fa4 = 'P');
060210080923         *in46 = (DB0fa4 = 'P'   and   %rem(wNumLdv : 2) = *zero);
060220080918      /end-free
060230040607      *
060240040607      * Se già stampato qualcos'altro:
060250040607      *   forzo il salto alla pagina successiva
060260080926       // (SE NON si sta stampando SOLO una packing-list, nel qual caso
060270080926       //  il posizionamento nella pagina A4 verrà eseguito dagli
060280080926       //  indicatori 45 e 46)
060290080926if  1c                   if           (CurLineA4 > $MaxLine1
060300080926     c                             or  §LO2      > $LimOF1)
060310080926     c                             and NOT *in45
060320040607     c                   eval      CurLineA4 = wMaxLine
060330040607     c                   exsr      Test_OF
060340040607e   1c                   endif
060350040607      *
060360040607     c                   eval      $DDT_p = *on
060370040607      *
060380130315      /free
060390130315         // -?Posizionamento?
060400130315         clear  k06fiar801;
060410130315         k_AR8aas = DB0aas;
060420130315         k_AR8lnp = DB0lnp;
060430130315         k_AR8nrs = DB0nrs;
060440130315         k_AR8nsp = DB0nsp;
060450130315         k_AR8tmd = c_§PDF + '999';
060460130315         setgt  %kds( k06fiar801 : 5 )  FIAR8000;
060470130315      /end-free
060480130315      *
060490130315      * - Letura
060500040607do  1c                   dou          %eof(FIAR801L)
060510040607     c                             or AR8atb = *blanks
060520130308     c                             or AR8tmd <> c_§PDF
060530040607     c     KLDV          reade     FIAR8000
060540040607e   1c                   enddo
060550040607if  1c                   if        %eof(FIAR801L)
060560040607     c                   goto      NOprtDDT
060570040607e   1c                   endif
060580041207if  1c                   if        §AR8des   = *blanks
060590041207     c*** (già fatto:)   reset                   dAR8
060600041207     c                   reset                   TIBS02ds
060610041207     c                   eval      T02cod    = 'AR8'
060620041207     c                   movel(p)  AR8tmd        T02ke1
060630041207     c                   call      'TIBS02R'
060640041207     c                   parm                    KPJBA
060650041207     c                   parm                    TIBS02ds
060660041207if  2c                   if        T02err    = *blanks
060670041207     c                   movel     T02uni        dAR8
060680041207e   2c                   endif
060690041207e   1c                   endif
060700040607      *
060710040607sel 1c                   select
060720050217      * - Karnak o Titanedi o EDk (identiche)
060730080730w   1c                   when      §AR8tsd   = 'K1'
060740040607     c                   exsr      DDT_Karnak
060750050414      * - Logistica o Sait-Abrasivi
060760060627w   1c                   when      %subst(§AR8tsd:1:1) = 'L'
060770050218     c                   exsr      DDT_Logistica
060780050217      * - Arcte
060790050215w   1c                   when      AR8tmd    = 'ARCT'
060800050217     c                   exsr      DDT_Arcte
060810050214      * - Esprinet
060820060627w   1c                   when      §AR8tsd   = 'E1'
060830050214     c                   exsr      DDT_Esprinet
060840041207      * - dati generici (con logo)
060850041207w   1c                   when      §AR8tsd   = 'S2'
060860041207     c                   exsr      DDT_genericoL
060870041207      * - dati generici (senza logo)
060880041207w   1c***                when      §AR8tsd   = 'S1'
060890040607     c                   other
060900040607     c                   exsr      DDT_generico
060910040607e   1c                   endsl
060920040607      *
060930060921     c                   reset                   $FIAR8
060940040607     c                   reset                   $DDT_o
060950040607     c                   reset                   $DDT_p
060960040608     c                   reset                   $DDTlastPg
060970040608     c                   clear                   wOmaRowT
060980040608     c                   clear                   wOmaRowD
060990040607     c                   clear                   $AR8o1
061000040607      *
061010040607     c     NOprtDDT      ENDSR
061020040607      *
061030040607      *---------------------------------------------------------------*
061040050217      *?Stampa DDT del mittente KARNAK / TITANEDI                    ?*
061050040607      *---------------------------------------------------------------*
061060040607     c     DDT_Karnak    BEGSR
061070040607      *
061080080923      /free
061090080923         // Posizionamento iniziale all'interno del foglio A4
061100080923         write  A4prtSP1;
061110080923
061120080923         // Impostazione del nuovo limite x overflow
061130080923         select;
061140080923           when  NOT *in45;
061150080923             wMaxLine = $MaxLine3;
061160080923           when  *in45   and   NOT *in46;
061170080923             wMaxLine = $MaxLine3_pl1;
061180080923           when  *in45   and   *in46;
061190080923             wMaxLine = $MaxLine3_pl2;
061200080923         endsl;
061210080923      /end-free
061220040607      *
061230040607      * Impostazione testi fissi - - - - - - - - - - - - - - - - - - -*
061240040607      *
061250040607     c                   eval      PCtx1 = 'ATTENZIONE: VERIFICARE LA -
061260040607     c                             QUANTITA'' E L''INTEGRITA'' DEI CAR-
061270040607     c                             TONI. IN CASO DI DISCORDANZA RITIRA-
061280040607     c                             RE CON RISERVA.'
061290130118     c*//                eval      PCtx2 = 'La merce viagga a rischio -
061300130118     c*//                          e pericolo del committente anche se-
061310130118     c*//                           venduta franco destino. Trascorsi -
061320130118     c*//                          otto giorni dal ricevimento della m-
061330130118     c*//                          erce non si accettano reclami.   - -
061340130118     c*//                            IN CASO DI RITARDATO PAGAMENTO DE-
061350130118     c*//                          CORRERANNO GLI INTERESSI BANCARI IN-
061360130118     c*//                           VIGORE.'
061370130118     c*//                eval      PCtx3 = 'Per ogni vertenza l''acqui-
061380130118     c*//                          rente accetta la competenza della R-
061390130118     c*//                          epubblica di San Marino o in altern-
061400130118     c*//                          ativa del foro di Rimini.'
061410130118     c*//                eval      PCtx4 = 'NON SI ACCETTANO RESI E NO-
061420130118     c*//                          N SI EFFETTUANO SOSTITUZIONI SE NON-
061430130118     c*//                           AUTORIZZATI PER ISCRITTO DALLA NOS-
061440130118     c*//                          TRA DIREZIONE COMMERCIALE.'
061450130118     c*//                eval      PCtx5 = 'Quand''anche autorizzati, -
061460130118     c*//                          i resi verranno accettati e le sost-
061470130118     c*//                          ituzioni verranno effettuate solo d-
061480130118     c*//                          opo aver accertato il perfetto stat-
061490130118     c*//                          o nel nostro stabilimento della mer-
061500130118     c*//                          ce restituita.'
061510130118     c                   eval      PCtx2 = 'Eventuali reclami per mate-
061520130118     c                             riale mancante o danneggiato all''i-
061530130118     c                             nterno dei colli dovranno pervenire-
061540130118     c                              entro 8 (otto) giorni di calendari-
061550130118     c                             o dall''avvenuta consegna a MyO S.r-
061560130118     c                             .l. - Ufficio Resi - Via Santarcang-
061570130118     c                             iolese, 6 - 47825 Torriana (RN)'
061580130118     c                   eval      PCtx4 = 'Eventuali mancanze di coll-
061590130118     c                             i e/o avarie sono da contestare all-
061600130118     c                             ''atto della consegna con annotazio-
061610130118     c                             ne su lettera di vettura e DDT'
061620040709if  1c                   if        AR8tmd = 'TITA'
061630040709     c                   eval      PCtx6 = 'Depositario: K & K Logisti-
061640040709     c                             cs S.p.a. - via Santarcangiolese, 6-
061650040709     c                              - 47825 Torriana (RN) - P.I. 00953-
061660040709     c                             860400'
061670040709e   1c                   endif
061680080822      *
061690040607      * Conteggio delle righe "omaggio" - - - - - - - - - - - - - - -*
061700040607      *
061710040614     c                   clear                   xx
061720040607     c                   eval      SAVprr = AR8prr
061730040607     c     KLDV          setgt     FIAR8000
061740040607     c     KLDV          readpe    FIAR8000
061750040607do  1c                   dow       NOT %eof(FIAR801L)
061760040607sel 2c                   select
061770040607w   2c                   when      AR8atb <> *blanks
061780040611w   2c                   when      AR8trk =  'O2'
061790040608     c                   add       1             wOmaRowD
061800040611w   2c                   when      AR8trk =  'O1'
061810040608     c                   add       1             wOmaRowT
061820051110     c                   movel(p)  AR8uni        dAR8O1
061830040607     c                   add       1             xx
061840040607     c                   eval      $AR8o1(xx) =  §AR8O1a
061850040607x   2c                   other
061860040607     c                   leave
061870040607e   2c                   endsl
061880040607     c     KLDV          readpe    FIAR8000
061890040607e   1c                   enddo
061900040607      * - mi riposiziono
061910040607     c                   eval      AR8prr = SAVprr
061920040607     c     K06AR801      setll     FIAR8000
061930040607     c     K06AR801      reade     FIAR8000
061940040607      *
061950040607      * Reperimento dati, impostazione e stampa DDT mittente  - - - -*
061960040607      *
061970040607if  1c                   dow       NOT %eof(FIAR801L)
061980040607      *
061990040607sel 2c                   select
062000040607      * - Record annullato (non si stampa nulla)
062010040607w   2c                   when      AR8atb <> *blanks
062020040607      * - Dati di dettaglio: articolo, descrizione, quantità
062030040607w   2c                   when      AR8trk = 'C1'
062040050217     c                   exsr      PrtDdtKARN_C1
062050040607      * - Dati di dettaglio: testo per articoli non disponibili
062060040607w   2c                   when      AR8trk = 'C2'
062070050217     c                   exsr      PrtDdtKARN_C2
062080040607      * - Dati di dettaglio: descrizioni generiche (orario apertura)
062090040607w   2c                   when      AR8trk = 'C3'
062100050217     c                   exsr      PrtDdtKARN_C3
062110040607      * - Dati di testata: destinatario, cliente di fatturazione
062120040607w   2c                   when      AR8trk = 'T1'
062130050217     c                   exsr      PrtDdtKARN_T1
062140040607      * - Dati di testata: agente, numero ordine, estremi documento
062150040607w   2c                   when      AR8trk = 'T2'
062160050217     c                   exsr      PrtDdtKARN_T2
062170040607      * - Dati di fondo: mezzo e causale trasporto, colli, peso
062180040607w   2c                   when      AR8trk = 'V1'
062190050217     c                   exsr      PrtDdtKARN_V1
062200040607      * - Dati di fondo: vettore, annotazioni e note
062210040607w   2c                   when      AR8trk = 'V2'
062220050217     c                   exsr      PrtDdtKARN_V2
062230040607      * - Dati esterni: riga descrittiva con referente
062240040607w   2c                   when      AR8trk = 'L1'
062250050217     c                   exsr      PrtDdtKARN_L1
062260040607      * - Dati di omaggio: testo per articoli omaggiati
062270040607w   2c                   when      AR8trk = 'O1'
062280050217     c                   exsr      PrtDdtKARN_O1
062290040607      * - Dati di omaggio: articolo, descrizione, quantità
062300040607w   2c                   when      AR8trk = 'O2'
062310050217     c                   exsr      PrtDdtKARN_O2
062320040607e   2c                   endsl
062330040607      *
062340040607     c     KLDV          reade     FIAR8000
062350040607      *
062360040607e   1c                   enddo
062370040607      *
062380040607      * Reimpostazione del vecchio limite x overflow
062390040607     c                   eval      wMaxLine = $MaxLine2
062400040607      *
062410040607     c                   ENDSR
062420050214      *
062430040607      *---------------------------------------------------------------*
062440050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
062450050217      *?Dati di testata: destinatario, cliente di fatturazione       ?*
062460040607      *---------------------------------------------------------------*
062470050217     c     PrtDdtKARN_T1 BEGSR
062480040607      *
062490040607      * > logo
062500040607     c                   eval      §Logo  = 'LOGO' + AR8tmd
062510040607      *
062520051110     c                   movel(p)  AR8uni        dAR8T1
062530040607      *
062540040607      * > destinatario
062550040607     c                   eval      PCrsd  = §AR8t1a
062560040607     c                   eval      PCind  = §AR8t1b
062570040616if  1c                   if        §AR8t1c  > *zeros
062580040607     c                   eval      PCcad  = §AR8t1c
062590040616e   1c                   endif
062600040607     c                   eval      PClod  = §AR8t1d
062610040607     c                   eval      PCprd  = §AR8t1e
062620040607     c***                eval      PCnzd  = *blanks
062630040607      * > cliente di fatturazione
062640040607     c                   eval      PCrsf  = §AR8t1f
062650040607     c                   eval      PCinf  = §AR8t1g
062660040616if  1c                   if        §AR8t1h  > *zeros
062670040607     c                   eval      PCcaf  = §AR8t1h
062680040616e   1c                   endif
062690040607     c                   eval      PClof  = §AR8t1i
062700040607     c                   eval      PCprf  = §AR8t1j
062710040607     c***                eval      PCnzf  = *blanks
062720040607      *
062730040607     c                   ENDSR
062740040607      *
062750040607      *---------------------------------------------------------------*
062760050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
062770050217      *?Dati di testata: agente, ..., porto, ...                     ?*
062780040607      *---------------------------------------------------------------*
062790050217     c     PrtDdtKARN_T2 BEGSR
062800040607      *
062810051110     c                   movel(p)  AR8uni        dAR8T2
062820040607      *
062830040607      * > dati in testata DDT
062840040607      * - - codice agente
062850130122     c*//                eval      PCage  = §AR8t2a
062860040607      * - - numero e data ordine
062870040607     c                   eval      PCnro  = §AR8t2b
062880040607     c                   eval      PCdto  = §AR8t2c
062890040607      * - - riferimenti
062900040607     c                   eval      PCrif  = §AR8t2d
062910040607      * - - codice destinatario (dal mittente)
062920040611     c                   eval      PCcli  = §AR8t2e
062930040607      * - - numero e data documento (DDT)
062940040607     c                   eval      PCnrd  = §AR8t2f
062950040607     c                   eval      PCdtd  = §AR8t2g
062960040607      * - - numero pagina documento originale (qui ricalcolato)
062970040607     c***                eval      Page   = §AR8t2h
062980040607      * - - porto
062990040607     c                   eval      PCpto  = §AR8t2i
063000040607      * - - condizioni di pagamento
063010040611     c                   eval      PCpag  = §AR8t2j
063020040607     c                                    + ' '
063030040607     c                                    + %trim(§AR8t2k)
063040040607      * - - tipologia spedizione
063050040607     c                   eval      PCtsp  = §AR8t2l
063060040607      *
063070040607      * > Stampa testata DDT - - - - - - - - - - - - - - - - - - - - -*
063080040607      *
063090040607      *   - OverFlow impossibile qui! -
063100040607     c                   write     A4prtDK1
063110040607      *
063120040607     c                   ENDSR
063130040607      *
063140040607      *---------------------------------------------------------------*
063150050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
063160050217      *?Dati di dettaglio: articolo, descrizione, quantità           ?*
063170040607      *---------------------------------------------------------------*
063180050217     c     PrtDdtKARN_C1 BEGSR
063190040607      *
063200040607     c                   clear                   A4prtDK2
063210051110     c                   movel(p)  AR8uni        dAR8C1
063220040607      *
063230040607      * > dati in dettaglio DDT
063240040607      * - - codice prodotto
063250040607     c                   eval      PCcpr  = §AR8c1a
063260040607      * - - descrizione prodotto
063270040607     c                   eval      PCdpr  = §AR8c1b
063280040607      * - - quantità
063290040607     c                   eval      PCqta  = '     ' + §AR8c1c
063300040607      * - - note
063310040607     c***                eval      PCnot  = *blanks
063320040607      *
063330040607      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
063340040607      *
063350040607     c                   exsr      Test_OF
063360040607     c                   write     A4prtDK2
063370040607      *
063380040607     c                   ENDSR
063390040607      *
063400040607      *---------------------------------------------------------------*
063410050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
063420050217      *?Dati di dettaglio: testo per articoli non disponibili        ?*
063430040607      *---------------------------------------------------------------*
063440050217     c     PrtDdtKARN_C2 BEGSR
063450040607      *
063460040607     c                   clear                   A4prtDK3
063470051110     c                   movel(p)  AR8uni        dAR8C2
063480040607      *
063490040607      * > dati in note DDT
063500040607      * - - riga descrittiva
063510040607     c                   eval      PCtxt  = §AR8c2a
063520040607      *
063530040607      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
063540040607      *
063550040607     c                   exsr      Test_OF
063560040607     c                   write     A4prtDK3
063570040607      *
063580040607     c                   ENDSR
063590040607      *
063600040607      *---------------------------------------------------------------*
063610050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
063620050217      *?Dati di dettaglio: descrizioni generiche (orario apertura)   ?*
063630040607      *---------------------------------------------------------------*
063640050217     c     PrtDdtKARN_C3 BEGSR
063650040607      *
063660040607     c                   clear                   A4prtDK2
063670051110     c                   movel(p)  AR8uni        dAR8C3
063680040607      *
063690040607      * > dati in note DDT
063700040607      * - - riga descrittiva
063710040607     c                   eval      PCdpr  = §AR8c3a
063720040607      *
063730040607      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
063740040607      *
063750040607     c                   exsr      Test_OF
063760040607     c                   write     A4prtDK2
063770040607      *
063780040607     c                   ENDSR
063790040607      *
063800040607      *---------------------------------------------------------------*
063810050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
063820050217      *?Dati di fondo: mezzo e causale trasporto, colli, peso        ?*
063830040607      *---------------------------------------------------------------*
063840050217     c     PrtDdtKARN_V1 BEGSR
063850040607      *
063860051110     c                   movel(p)  AR8uni        dAR8V1
063870040607      *
063880040607      * Controllo se ci saranno da stampare tante righe omaggio da
063890040607      *   chiudere in pagina successiva:
063900040608      *   i campi del box finale del DDT (in totale 9 righe) verranno
063910040607      *   riempiti solo nella ultima pagina.
063920040607      *
063930080924      /free
063940080924         select;
063950080924           when  NOT *in45;
063960080924      /end-free
063970040608     c                   eval      $DDTlastPg = ($MaxLine2
063980040713     c                                        +  1
063990040608     c                                        -  CurLineA4
064000040608     c                                        -  $BoxRow1
064010040608     c                                        -  wOmaRowT
064020040608     c                                        >= wOmaRowD)
064030080924      /free
064040080924           when  *in45   and   NOT *in46;
064050110406             $DDTlastPg = ($MaxLine2_pl1k
064060080924                           + 1 - CurLineA4 - $BoxRow1 - wOmaRowT
064070080924                           >= wOmaRowD);
064080080924           when  *in45   and   *in46;
064090110406             $DDTlastPg = ($MaxLine2_pl2k
064100080924                           + 1 - CurLineA4 - $BoxRow1 - wOmaRowT
064110080924                           >= wOmaRowD);
064120080924         endsl;
064130080924      /end-free
064140040607      *
064150040607      * > dati in chiusura DDT
064160040607      *   Per ora imposto (ho già impostato) solo la DS "dAR8V1".
064170040607      *   I corrispondenti campi di stampa li impostero nella subr.
064180050217      *     "PrtDdtKARN_V2".
064190040607      *
064200040607     c                   ENDSR
064210040607      *
064220040607      *---------------------------------------------------------------*
064230050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
064240050217      *?Dati di fondo: vettore, annotazioni e note                   ?*
064250040607      *---------------------------------------------------------------*
064260050217     c     PrtDdtKARN_V2 BEGSR
064270040607      *
064280051110     c                   movel(p)  AR8uni        dAR8V2
064290040618      *
064300040618      * > dati in chiusura DDT
064310040618      *
064320040607      * Ho già controllato se ci saranno da stampare tante righe
064330040607      *   omaggio da chiudere in pagina successiva
064340040608if  1c                   if        $DDTlastPg = *off
064350040607     c                   eval      PCnv2  = '          '
064360040607     c                                    + '*** SEGUE ***'
064370040618x   1c                   else
064380050217     c                   exsr      PrtDdtKARNaV2
064390040607e   1c                   endif
064400040607      *
064410040607      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
064420040608      *
064430050217     c                   exsr      PrtDdtKARNbV2
064440040607      *
064450040607      * > Impostazione nuovo limite di OverFlow  - - - - - - - - - - -*
064460080923      /free
064470080923         select;
064480080923           when  NOT *in45;
064490080923             wMaxLine = $MaxLine2;
064500080923           when  *in45   and   NOT *in46;
064510080923             wMaxLine = $MaxLine2_pl1;
064520080923           when  *in45   and   *in46;
064530080923             wMaxLine = $MaxLine2_pl2;
064540080923         endsl;
064550080923      /end-free
064560040607      *
064570040608      * N.B.: la copia per il destinatario della LDV deve comparire
064580040608      *       nell'ultima pagina del DDT per il cliente (vedi subr.
064590040608      *       "ChiusuraLDV")
064600040607      *
064610040607     c                   ENDSR
064620040618      *
064630040618      *---------------------------------------------------------------*
064640040618      *
064650050217     c     PrtDdtKARNaV2 BEGSR
064660040618      *
064670040618      * > dati in chiusura DDT
064680040618      *
064690040618      * > > da dAR8V1
064700040618      * - - mezzo di trasporto
064710040618     c                   movel     §AR8v1a       PCmtr
064720040618      * - - causale trasporto
064730040618     c                   eval      PCctr  = §AR8v1b
064740040618      * - - numero colli
064750040618     c                   eval      PCncl  = §AR8v1c
064760040618      * - - peso in kg
064770040618     c                   eval      PCpkg  = §AR8v1d
064780040618      * - - aspetto esteriore dei beni
064790040618     c                   eval      PCaeb  = §AR8v1e
064800040618      *
064810040618      * > > da dAR8V2
064820120530      * - - dati vettore: · ragione sociale (BRT S.p.A.)
064830040618      *                   · indirizzo
064840040618      *                   · c.a.p./città/provincia
064850040618      *     => NON con i dati che manda il mittente !!!
064860040618      *        ...Vuoi mai che si sbagli ad inviarci della roba !!!
064870040618     c***                eval      PCvet  = %trim(§AR8v2a)
064880040618     c***                                 + ' - '
064890040618     c***                                 + %trim(§AR8v2b)
064900040618     c***                                 + ' - '
064910040618     c***                                 + %trim(§AR8v2c)
064920120530     c*//                if        (DB0dft > *zero       and
064930120530     c*//                           DB0dft > §vpoBRTdta)  OR
064940120530     c*//                          (DB0dft = *zero       and
064950120530     c*//                           Wdsp   > §vpoBRTdta)
064960110426     c                   eval      PCvet  = $BRT
064970110426     c                                    + ' - '
064980110426     c                                    + $Intest2
064990110426     c                                    + ' (BO)'
065000120530     c*//                else
065010120530     c*//                eval      PCvet  = $Bartolini
065020120530     c*//                                 + ' - '
065030120530     c*//                                 + $Intest2
065040120530     c*//                                 + ' (BO)'
065050120530     c*//                endif
065060040618      * - - data ritiro
065070040618     c*** non stampo:    eval      PCdtv  = §AR8v2d
065080040618      * - - orario ritiro
065090040618     c*** non stampo:    eval      PChmv  = §AR8v2e
065100040618      * - - annotazioni / variazioni
065110040618     c                   eval      PCvea  = §AR8v2f
065120040618      * - - note
065130040618     c                   eval      PCnv1  = §AR8v2g
065140040618     c                   eval      PCnv2  = §AR8v2h
065150040618      *
065160040618     c                   ENDSR
065170040618      *
065180040618      *---------------------------------------------------------------*
065190040618      *
065200050217     c     PrtDdtKARNbV2 BEGSR
065210040618      *
065220040618      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
065230040618      *
065240040618      * - - Calcolo la posizione finale del box
065250040618     c                   exsr      Posiz_DK
065260040618      *
065270040618      * - - Stampa parte finale DDT
065280040618     c                   write     A4prtDK4
065290040618      *
065300040618      * > Stampa logo, box, linee e testi fissi  - - - - - - - - - - -*
065310081017     c                   if            §AR8cxd = *blanks
065320081017     c                             or  DB0fa4  = 'P'
065330050331     c                   write     A4prtDT0
065340050331     c                   endif
065350040618     c                   write     A4prtDK0
065360040618      *
065370040618     c                   ENDSR
065380040607      *
065390040607      *---------------------------------------------------------------*
065400050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
065410050217      *?Dati esterni: riga descrittiva con referente                 ?*
065420040607      *---------------------------------------------------------------*
065430050217     c     PrtDdtKARN_L1 BEGSR
065440040607      *
065450051110     c                   movel(p)  AR8uni        dAR8L1
065460040607      *
065470040607      * > righe descrittive esterne al DDT
065480040607      * - - riga descrittiva
065490040607     c                   eval      PCtxx  = §AR8l1a
065500040607      *
065510040607      * > Stampa riga testo finale DDT - - - - - - - - - - - - - - - -*
065520040618      *   - OverFlow già testato per questa riga -
065530040607     c                   write     A4prtDK5
065540040607      *
065550040607     c                   ENDSR
065560040607      *
065570040607      *---------------------------------------------------------------*
065580050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
065590050217      *?Dati di omaggio: testo per articoli omaggiati                ?*
065600040607      *---------------------------------------------------------------*
065610050217     c     PrtDdtKARN_O1 BEGSR
065620040608      *
065630040608     c                   eval      $DDT_o = *on
065640040607      *
065650040607     c                   clear                   A4prtDK5
065660051110     c                   movel(p)  AR8uni        dAR8O1
065670040607      *
065680040607      * > righe descrittive esterne al DDT
065690040607      * - - riga descrittiva
065700040607     c                   eval      PCtxx  = §AR8o1a
065710040607      * - - nessuna sottolineatura
065720040608     c***                eval      *in39  = *off
065730040607      *
065740040607      * > Stampa testata omaggi in DDT - - - - - - - - - - - - - - - -*
065750040608      *   (se NON già stampata nella nuova pagina x OverFlow)
065760040608     c                   eval      SavLineA4 = CurLineA4
065770040607     c                   exsr      Test_OF
065780040608if  1c                   if        CurLineA4 = SavLineA4
065790040607     c                   write     A4prtDK5
065800040608e   1c                   endif
065810040608      * ...Dovendo ristampare le righe di intestazione degli omaggi
065820040608      *   in ogni pagina (da quella del 1° omaggio in poi), non calcolo
065830040608      *   quelle già stampate...
065840040608     c*** NO:            sub       1             wOmaRowT
065850040607      *
065860040607     c                   ENDSR
065870040607      *
065880040607      *---------------------------------------------------------------*
065890050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
065900050217      *?Dati di omaggio: articolo, descrizione, quantità             ?*
065910040607      *---------------------------------------------------------------*
065920050217     c     PrtDdtKARN_O2 BEGSR
065930040607      *
065940040607     c                   clear                   A4prtDK2
065950051110     c                   movel(p)  AR8uni        dAR8O2
065960040607      *
065970040607      * > dettaglio omaggi DDT
065980040607      * - - codice prodotto
065990040607     c                   eval      PCcpr  = §AR8o2a
066000040607      * - - descrizione prodotto
066010040607     c                   eval      PCdpr  = §AR8o2b
066020040607      * - - quantità
066030040618     c                   eval      PCqta  = '     ' + §AR8o2c
066040040607      * - - note
066050040607     c***                eval      PCnot  = *blanks
066060040607      *
066070040607      * > Stampa dettaglio omaggi in DDT - - - - - - - - - - - - - - -*
066080040607      *
066090040607     c                   exsr      Test_OF
066100040607     c                   write     A4prtDK2
066110040608     c                   sub       1             wOmaRowD
066120040607      *
066130040607     c                   ENDSR
066140040608      *
066150040608      *---------------------------------------------------------------*
066160081002      *?Impostazione nuova pagina di DDT Karnak / Titanedi / EDk     ?*
066170040608      *---------------------------------------------------------------*
066180081002     c     NewPageDK     BEGSR
066190040608      *
066200080923      /free
066210080923         // Impostazione del nuovo limite x overflow
066220080923         select;
066230080923           when  NOT *in45;
066240080923             wMaxLine = $MaxLine3;
066250080923           when  *in45   and   NOT *in46;
066260080923             wMaxLine = $MaxLine3_pl1;
066270080923           when  *in45   and   *in46;
066280080923             wMaxLine = $MaxLine3_pl2;
066290080923         endsl;
066300080923      /end-free
066310080923     c                   write     A4prtSP1
066320040608     c                   write     A4prtDK1
066330040608      *
066340040608if  1c                   if        $DDT_o = *on
066350040608      *
066360040608      * - Stampa 1 riga di dettaglio vuota
066370040608     c                   clear                   A4prtDK3
066380040608     c                   write     A4prtDK3
066390040618      * - Reimposta i dati nel box finale del DDT
066400050217     c                   exsr      PrtDdtKARNaV2
066410040618      * - Stampa fondo DDT
066420050217     c                   exsr      PrtDdtKARNbV2
066430040608      * - Stampa ultima riga
066440040608      *   ("Per qualsiasi problema o chiarimento contattare...)
066450040608     c                   clear                   A4prtDK5
066460040608     c                   eval      PCtxx  = §AR8l1a
066470040608     c                   write     A4prtDK5
066480040608      * - Stampa intestazione per merce omaggio
066490040608      *   xx = numero di righe per "intestazione omaggi"
066500040608     c                   eval      wx     = xx
066510040608do  2c                   dow           xx         >  *zeros
066520040608     c                             and xx         <= %elem($AR8o1)
066530040608     c                             and $AR8o1(xx) <> *blanks
066540040608     c                   eval      PCtxx  = $AR8o1(xx)
066550040608     c                   write     A4prtDK5
066560040608     c                   eval      xx     = xx - 1
066570040608e   2c                   enddo
066580040608     c                   eval      xx     = wx
066590040608      * -Se sta leggendo il tipo record "O1":?
066600040608      *   sposta il ciclo di lettura del file FIAR8 sul record
066610040608      *   successivo all'ultimo (appena stampato) dell'intestazione
066620040608      *   della merce omaggiata
066630040608if  2c                   if        AR8trk =  'O1'
066640040608if  3c                   dou       AR8trk <> 'O1'
066650040608     c     KLDV          reade     FIAR8000
066660040608e   3c                   enddo
066670040608if  3c*** è così:        if        AR8trk = 'O2'
066680040608     c     KLDV          readpe    FIAR8000
066690040707e   3c***                endif
066700040608e   2c                   endif
066710040608      * - Aggiorna controllo se "ultima pagina"
066720080924      /free
066730080924           select;
066740080924             when  NOT *in45;
066750080924      /end-free
066760040608     c                   eval      $DDTlastPg = ($MaxLine2
066770040713     c                                        +  1
066780040608     c                                        -  CurLineA4
066790040608     c                                        -  $BoxRow1
066800040608     c                                        -  wOmaRowT
066810040608     c                                        >= wOmaRowD)
066820080924      /free
066830080924             when  *in45   and   NOT *in46;
066840110406               $DDTlastPg = ($MaxLine2_pl1k
066850080924                             + 1 - CurLineA4 - $BoxRow1 - wOmaRowT
066860080924                             >= wOmaRowD);
066870080924             when  *in45   and   *in46;
066880110406               $DDTlastPg = ($MaxLine2_pl2k
066890080924                             + 1 - CurLineA4 - $BoxRow1 - wOmaRowT
066900080924                             >= wOmaRowD);
066910080924           endsl;
066920080924      /end-free
066930040608      *
066940040608e   1c                   endif
066950040608      *
066960040608     c                   ENDSR
066970040607      *
066980040607      *---------------------------------------------------------------*
066990050414      *?Posizionamento di linee e box nel DDT KARNAK / TITANEDI / EDk?*
067000040607      *---------------------------------------------------------------*
067010040607     c     Posiz_DK      BEGSR
067020040607      *
067030080918      /free
067040080923         // Impostazione posizioni di logo/box
067050080923         // (posizioni di "partenza" diverse a seconda del posizionam.
067060080923         //  iniziale nel foglio A4 attualmente in stampa)
067070080923         select;
067080080923           when  NOT *in45;
067090081002             §PB1 =  3.120;
067100080923           when  *in45   and   NOT *in46;
067110081002             §PB1 = 0.250;
067120080923           when  *in45   and   *in46;
067130081002             §PB1 = 6.000;
067140080923         endsl;
067150080918      /end-free
067160040607     c                   eval      §PB2 =  §PB1 + 1.23
067170080822     c                   eval      §PB3 =  §PB2 + 0.18
067180040607     c                   eval(h)   §PB4 =  CurLineA4 / 8 + 0.20
067190040607     c                   eval      §PB5 =  §PB4 + 0.18
067200040607     c                   eval      §PB6 =  §PB5 + 0.77
067210040607     c                   eval      §PB7 =  §PB5 + 0.165
067220040607     c                   eval      §PB8 =  §PB7 + 0.08
067230040607      * Impostazione posizioni di linee orizzontali nel 1° box
067240040607     c                   eval(h)   §LO1 =  §PB1 + (§PB2 - §PB1) / 2
067250040607      * Impostazione posizioni di linee orizzontali nel 2° box
067260080822     c                   eval      §LO2 =  §PB3 + 0.23
067270080822     c                   eval      §LO3 =  §LO2 + 0.145
067280080822     c                   eval      §LO4 =  §LO3 + 0.10
067290040607     c                   eval      §LO5 =  §PB4 - 0.15
067300040607      * Impostazione posizioni di linee orizzontali nel 3° box
067310040607     c                   eval      §LO6 =  §PB5 + 0.14
067320040607     c                   eval      §LO7 =  §LO6 + 0.12
067330040607     c                   eval      §LO8 =  §LO7 + 0.12
067340040607     c                   eval      §LO9 =  §LO8 + 0.13
067350040607      * Impostazione posizioni di linee verticali nel 2° box
067360040607     c                   eval      §LV1 =  §LO2 - §PB3
067370040607     c                   eval      §LV2 =  §LO3 - §LO2
067380040607     c                   eval      §LV3 =  §LO5 - §LO3
067390040607      * Impostazione posizioni di linee verticali nel 3° box
067400040607     c                   eval      §LV4 =  §LO6 - §PB5
067410040607     c                   eval      §LV5 =  §LO7 - §LO6
067420040607     c                   eval      §LV6 =  §LO8 - §LO7
067430040607     c                   eval      §LV7 =  §PB6 - §LO9
067440040607      * Impostazione posizioni di campi
067450040607     c                   eval      §PS1 =  §LO5 + 0.11
067460040607     c                   eval      §PS2 =  §PB4 + 0.08
067470040607     c                   eval      §PS3 =  §PS2 + 0.07
067480130118if  1c*//                if        §PB6 <  §PB1 + 4.58
067490130118     c*//                eval      §PR1 =  §PB1 + 4.58
067500130118if  1c                   if        §PB6 <  §PB1 + 3.30
067510130118     c                   eval      §PR1 =  §PB1 + 3.30
067520040607x   1c                   else
067530040607     c                   eval      §PR1 =  §PB6
067540040607e   1c                   endif
067550040607      *
067560040607     c                   ENDSR
067570041019      *
067580041019      *---------------------------------------------------------------*
067590050414      *?Stampa DDT del mittente LOGISTICA / SAIT ABRASIVI            ?*
067600041019      *---------------------------------------------------------------*
067610050218     c     DDT_Logistica BEGSR
067620041019      *
067630080924      /free
067640080924         // Impostazione del nuovo limite x overflow
067650080924         // (Codesto limite è valido solo per la stampa del dettaglio.
067660080924         //   Per la stampa del "fondo" bolla occorre REimpostarlo ad
067670080924         //   un valore più basso.
067680080924         //  Ovvero: stampo più righe di dettaglio che posso; se alla
067690080924         //  fine delle righe di dettaglio non rimane spazio a suffi-
067700080924         //  cienza per il fondo: prima della stampa del fondo si salta
067710080924         //  alla pagina successiva, dove non si stampa alcuna riga di
067720080924         //  dettaglio.)
067730080924         select;
067740080924           when  NOT *in45;
067750090323             wMaxLine = $MaxLine2;
067760080924           when  *in45   and   NOT *in46;
067770090323             wMaxLine = $MaxLine2_pl1;
067780080924           when  *in45   and   *in46;
067790090323             wMaxLine = $MaxLine2_pl2;
067800080924         endsl;
067810080924
067820080924         // Posizionamento iniziale all'interno del foglio A4
067830080924         write  A4prtSP1;
067840081006
067850081006         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
067860081006           write  A4prtNUL;
067870081006         endif;
067880080924      /end-free
067890041019      *
067900041019      * Impostazione testi fissi - - - - - - - - - - - - - - - - - - -*
067910041019      *
067920071008      * - prima solo per "Logistica", ora neanche più per lei...
067930071008      * - Sait Abrasivi SpA & Brumar Srl hanno Tipo Stampa DDT = "LC"
067940071008if  1c***                if        §AR8tsd <> 'LC'
067950071008     c***                eval      PCLtx1 = 'La merce viagga a rischio -
067960071008     c***                          e pericolo del Committente, pertanto -
067970071008     c***                          la Venditrice non risponde di -
067980071008     c***                          eventuali perdite o avarie derivanti -
067990071008     c***                          dal trasporto.'
068000071008     c***                eval      PCLtx2 = 'Il Destinatario, per -
068010071008     c***                          ottenere il risarcimento del danno, -
068020071008     c***                          deve far constatare al Corriere la -
068030071008     c***                          perdita o l''avaria all''atto del -
068040071008     c***                          ricevimento della merce.'
068050071008e   1c***                endif
068060050218      *
068070130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
068080130315      *  "StampaDDT")
068090050218     c***  KLDV          setll     FIAR8000       (già fatto)
068100050218     c***  KLDV          reade     FIAR8000       (già fatto)
068110041019      *
068120041019      * Reperimento dati, impostazione e stampa DDT mittente  - - - -*
068130041019      *
068140041019if  1c                   dow       NOT %eof(FIAR801L)
068150050318      *
068160050318if  2c                   if        AR8atb   = *blanks
068170050318     c                   eval      SAVtrk   = AR8trk
068180050318e   2c                   endif
068190041019      *
068200041019sel 2c                   select
068210041019      * - Record annullato (non si stampa nulla)
068220050218w   2c                   when      AR8atb  <> *blanks
068230041019      * - Dati di dettaglio: articolo, descrizione, quantità
068240050218w   2c                   when      AR8trk   = 'C1'
068250050217     c                   exsr      PrtDdtLOG1_C1
068260041103      * - Dati di dettaglio: descrizioni generiche
068270050218w   2c                   when      AR8trk   = 'C2'
068280050217     c                   exsr      PrtDdtLOG1_C2
068290041103      * - Dati di dettaglio: descrizioni generiche
068300050218w   2c                   when      AR8trk   = 'C3'
068310050217     c                   exsr      PrtDdtLOG1_C3
068320060614      * - Dati di dettaglio: corpo bolla non fincato
068330060614w   2c                   when      AR8trk   = 'C4'
068340060614     c                   exsr      PrtDdtLOG1_C4
068350090323      * - Dati di dettaglio: descrizioni generiche
068360090323w   2c                   when      AR8trk   = 'C5'
068370090323     c                   exsr      PrtDdtLOG1_C5
068380050318      * - Dati di packing-list: dettaglio
068390050318w   2c                   when      AR8trk   = 'Z1'
068400050318     c                   exsr      PrtDdtLOG1_Z1
068410041019      * - Dati di testata: sito e depositante
068420050218w   2c                   when      AR8trk   = 'T1'
068430050217     c                   exsr      PrtDdtLOG1_T1
068440041019      * - Dati di testata: destinatario, cliente di fatturazione
068450050218w   2c                   when      AR8trk   = 'T2'
068460050217     c                   exsr      PrtDdtLOG1_T2
068470041019      * - Dati di testata: agente, numero ordine, estremi documento
068480050218w   2c                   when      AR8trk   = 'T3'
068490050217     c                   exsr      PrtDdtLOG1_T3
068500041019      * - Dati di fondo: mezzo e causale trasporto, colli, peso
068510050218w   2c                   when      AR8trk   = 'P1'
068520050217     c                   exsr      PrtDdtLOG1_P1
068530050606      * - Dati di testata: FATTURA
068540050606w   2c                   when      AR8trk   = 'S1'
068550050606     c                   exsr      PrtDdtLOG1_S1
068560050606      * - Dati di dettaglio cpi 15: FATTURA
068570050606w   2c                   when      AR8trk   = 'S2'
068580050606     c                   exsr      PrtDdtLOG1_S2
068590050606      * - Dati di dettaglio cpi 20: FATTURA
068600050606w   2c                   when      AR8trk   = 'S3'
068610050606     c                   exsr      PrtDdtLOG1_S3
068620050606      * - Dati di fondo: FATTURA
068630050606w   2c                   when      AR8trk   = 'S4'
068640050606     c                   exsr      PrtDdtLOG1_S4
068650050606      * - Dati di fondo note: FATTURA
068660050606w   2c                   when      AR8trk   = 'S5'
068670050606     c                   exsr      PrtDdtLOG1_S5
068680041019e   2c                   endsl
068690041019      *
068700041019     c     KLDV          reade     FIAR8000
068710041019      *
068720041019e   1c                   enddo
068730050318      *
068740050318      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
068750050318      *
068760050505      * - se DDT con Packing-List:
068770050505      *   (altrimenti il box è già stato stampato in sr PrtDdtLOG1_P1)
068780050505if  1c                   if        %subst(SAVtrk:1:1) =  'Z'
068790050318      *
068800050318      * - - Calcolo la posizione finale del box e di linee separatrici
068810050318     c                   exsr      Posiz_DL
068820050318      *
068830050318      * > Stampa box, linee e testi fissi (no logo)  - - - - - - - - -*
068840081017     c                   if            §AR8cxd = *blanks
068850081017     c                             or  DB0fa4  = 'P'
068860050331     c                   write     A4prtDT0
068870050331     c                   endif
068880050318     c                   write     A4prtDL0C
068890050505      *
068900050505e   1c                   endif
068910050218      *
068920050218      * Reimpostazione del vecchio limite x overflow
068930080925      /free
068940080925         select;
068950080925           when  NOT *in45;
068960090323             wMaxLine = $MaxLine2;
068970080925           when  *in45   and   NOT *in46;
068980090323             wMaxLine = $MaxLine2_pl1;
068990080925           when  *in45   and   *in46;
069000090323             wMaxLine = $MaxLine2_pl2;
069010080925         endsl;
069020080925      /end-free
069030041019      *
069040041019     c                   ENDSR
069050081222
069060081222      /free
069070081222       //--------------------------------------------------------------
069080140303       //?Preparazione simil-logo Bartolini                            ?
069090081222       //--------------------------------------------------------------
069100090406       BEGSR  PrtDdtLOG1_SimilLogo;
069110081222
069120120530         //If  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
069130120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
069140120530         //
069150120530         //  // -?Preparazione simil-Logo BARTOLINI?
069160120530         //  §PLBLS  = §PBA + 0.150;
069170120530         //  §PLBLVS = 0.400;
069180120530         //  exsr  sr_SimilLogoBAR;
069190120530         //
069200120530         //  // -?Preparazione simil-Logo BRT?
069210120530         //  §P2blS  = §PlblS;
069220120530         //  //§P2blVS = §PlblVD + 0.050;
069230120530         //  §P2blVS = §PlblVD + 0.030;
069240120530         //  exsr  sr_SimilLogoBRT;
069250120530         //
069260120530         //Else;
069270120530         //
069280120530         //  // -?Preparazione simil-Logo BARTOLINI?
069290120530         //  §PLBLS  = §PBA + 0.050;
069300120530         //  §PLBLVS = 0.400;
069310120530         //  exsr  sr_SimilLogoBART;
069320120530         //
069330120530         //EndIf;
069340120530
069350120530         // -?Preparazione simil-Logo BRT?
069360120530         §PLBLS  = §PBA + 0.050;
069370120530         §PLBLVS = 0.400;
069380120530         exsr  sr_SimilLogoBRT_;
069390090406
069400120530         // -?Impostazione dati anagrafici BRT alla sua destra?
069410090422         §PC0    = §PBA    + 0.070 + 0.050;
069420090406         §PC1    = §PC0    + 0.100;
069430090406         §PC2    = §PC1    + 0.100;
069440090406         §PC3    = §PC2    + 0.090;
069450090406         §PC4    = §PC3    + 0.090;
069460090406         §PC5    = §PC4    + 0.090;
069470120530
069480120530         //§PLC    = 1.400;
069490120530         §PLC    = 1.000;
069500090406
069510120530         // -?"TBAR" è già correttamente impostato.?
069520120530         //TBAR    = $Bartolini;        (già così)
069530120530         //TBAR    = $BRT;              (già così)
069540090406         LBA1    = $Intest1;
069550090406         LBA2    = $Intest2_b;
069560120530         //LBA3    = $Intest3_b;
069570120530         LBA3    = $Intest3_b_brt;
069580090406         LBA4    = $Intest4_b;
069590090406         LBA5    = $Intest5_b;
069600081222
069610081222       ENDSR;
069620081222      /end-free
069630050214      *
069640041019      *---------------------------------------------------------------*
069650050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
069660050217      *?Dati di testata: sito e depositante                          ?*
069670041019      *---------------------------------------------------------------*
069680050217     c     PrtDdtLOG1_T1 BEGSR
069690041019      *
069700041019      * > logo
069710050218     c                   eval      §Logo    = 'LOGO' + AR8tmd
069720090107      /free
069730090107         // - Impostazione indicatore che condiziona la stampa del logo
069740120530         //   (o quella del "simil-logo" BRT)
069750090107         *in55 = (%subst(AR8tmd : 1 : 3) = 'LOG'          and
069760090107                  (§AR8tsd  = 'L1'  or  §AR8tsd  = 'L3'));
069770090107      /end-free
069780080908      *
069790060609      *? L O G O !! ?
069800060609      * per pulizia non vogliamo più avere tanti LOGx per ogni cliente che
069810060609      * vuole la stampa della fattura con il proprio logo e la bolla con
069820120530      * il logo BRT
069830060609      * anche Cappellini è d'accordo ad inviare i dati con il logo del
069840060609      * cliente da stampare nella LDV
069850060609      * quelli già esistenti avranno un nuovo logo un po' più piccolo per
069860071008      * problemi di spazio e i dati saranno inviati con AR8tmd = al nome del
069870071008      * nuovo logo e la tabella sarà fatta con §AR8tsd = 'L2' o 'L4'
069880060609      * i nuovi faremo in modo di avere o lo stesso logo oppure la stessa
069890060609      * logica descritta prima
069900060609      * ma può essere che un cliente voglia la fattura con il suo logo
069910120530      * mentre la bolla con il logo BRT quindi devo recuperare
069920060609      * il logo da stampare nella bolla non più da AR8TMD, che conterrà
069930060609      * il nome del logo da stampare nella fattura, ma dal campo del
069940120530      * del logo fattura, che conterrà il logo BRT, mentre il campo
069950071008      * §AR8tsd nella tabella sarà 'L1' o 'L3'
069960071008     c                   if        (§AR8tsd = 'L1' or §AR8tsd = 'L3') and
069970071008     c                             §AR8logof <> *blanks and
069980071008     c                             %subst(AR8tmd:1:3) <> 'LOG'
069990071008     c                   eval      §Logo    = 'LOGO' + §AR8logof
070000090107      /free
070010090107           // - Impostaz. indicatore che condiziona la stampa del logo
070020120530           //   (o quella del "simil-logo" BRT)
070030090107           *in55 = (%subst(§AR8logof : 1 : 3) = 'LOG');
070040090107      /end-free
070050060609     c                   endif
070060041019      *
070070080822     c                   movel(p)  AR8uni        dAR8LOG1T1
070080050414      *
070090060627if  1c                   if            §AR8tsd <> 'LC'
070100041019      *
070110041019      * > sito
070120060407     c                   select
070130071008     c                   when      §AR8tsd = 'L1' or
070140071008     c                             §AR8tsd = 'L3'
070150050707     c                   eval      PLOG1st0 = 'Deposito di:'
070160071008     c                   when      §AR8tsd = 'L2' or
070170071008     c                             §AR8tsd = 'L4'
070180060407     c                   eval      PLOG1st0 = 'Depositario:'
070190060407     c                   endsl
070200050707     c                   eval      PLOG1st1 = §A8LOG1t1a
070210050707     c                   eval      PLOG1st2 = §A8LOG1t1b
070220050707     c                   eval      PLOG1st3 = §A8LOG1t1c
070230050707     c                   eval      PLOG1st4 = §A8LOG1t1d
070240050707     c                   eval      PLOG1st5 = §A8LOG1t1e
070250041019      *
070260041019      * > depositante
070270060407     c                   select
070280071008     c                   when      §AR8tsd = 'L1' or
070290071008     c                             §AR8tsd = 'L3'
070300050707     c                   eval      PLOG1de0 = 'Depositario della:'
070310071008     c                   when      §AR8tsd = 'L2' or
070320071008     c                             §AR8tsd = 'L4'
070330060407     c                   eval      PLOG1de0 = 'Deposito di:'
070340060407     c                   endsl
070350050707     c                   eval      PLOG1de1 = §A8LOG1t1f
070360050707     c                   eval      PLOG1de2 = §A8LOG1t1g
070370050707     c                   eval      PLOG1de3 = §A8LOG1t1h
070380050707     c                   eval      PLOG1de4 = §A8LOG1t1i
070390050707     c                   eval      PLOG1de5 = §A8LOG1t1j
070400050414      *
070410050414x   1c                   else
070420050414      *
070430050707     c***                eval      PLOG1st0 = *blanks
070440050707     c                   eval      PLOG1st1 = §A8LOG1t1a + §A8LOG1t1b
070450050707     c                   eval      PLOG1st2 = §A8LOG1t1c + §A8LOG1t1d
070460050707     c                   eval      PLOG1st3 = §A8LOG1t1e + §A8LOG1t1f
070470050707     c                   eval      PLOG1st4 = §A8LOG1t1g + §A8LOG1t1h
070480050707     c                   eval      PLOG1st5 = §A8LOG1t1i + §A8LOG1t1j
070490050414      *
070500050414e   1c                   endif
070510050214      *
070520041022     c                   ENDSR
070530050214      *
070540041019      *---------------------------------------------------------------*
070550050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
070560050217      *?Dati di testata: destinatario, cliente di fatturazione       ?*
070570041019      *---------------------------------------------------------------*
070580050217     c     PrtDdtLOG1_T2 BEGSR
070590041019      *
070600051110     c                   clear                   dAR8LOG1T2
070610050218     c                   movel     AR8uni        dAR8LOG1T2
070620041019      *
070630041019      * > destinatario
070640050707if  1c                   if        §A8LOG1T2a <> *blanks
070650050707     c                   eval      PLOG1ccd   =  §A8LOG1T2a
070660050218e   1c                   endif
070670050707if  1c                   if        §A8LOG1T2c =  *blanks
070680050707     c                   clear                   PLOG1rsd
070690050707     c                   eval      PLOG1rsd2  =  §A8LOG1T2b
070700050218x   1c                   else
070710050707     c                   eval      PLOG1rsd   =  §A8LOG1T2b
070720050707     c                   eval      PLOG1rsd2  =  §A8LOG1T2c
070730050218e   1c                   endif
070740050707     c                   eval      PLOG1ind   =  §A8LOG1T2d
070750050707     c                   eval      PLOG1ind2  =  §A8LOG1T2e
070760050707if  1c                   if        §A8LOG1T2f >  *zeros
070770050707     c                   eval      PLOG1cad   =  §A8LOG1T2f
070780041019e   1c                   endif
070790050707     c                   eval      PLOG1lod   =  §A8LOG1T2g
070800041019      * > cliente di fatturazione
070810050707if  1c                   if        §A8LOG1T2h <> *blanks
070820050707     c                   eval      PLOG1ccf   =  §A8LOG1T2h
070830041019e   1c                   endif
070840050707if  1c                   if        §A8LOG1T2j =  *blanks
070850050707     c                   clear                   PLOG1rsf
070860050707     c                   eval      PLOG1rsf2  =  §A8LOG1T2i
070870050218x   1c                   else
070880050707     c                   eval      PLOG1rsf   =  §A8LOG1T2i
070890050707     c                   eval      PLOG1rsf2  =  §A8LOG1T2j
070900050218e   1c                   endif
070910050707     c                   eval      PLOG1inf   =  §A8LOG1T2k
070920050707     c                   eval      PLOG1inf2  =  §A8LOG1T2l
070930050707if  1c                   if        §A8LOG1T2m >  *zeros
070940050707     c                   eval      PLOG1caf   =  §A8LOG1T2m
070950041019e   1c                   endif
070960050707     c                   eval      PLOG1lof   =  §A8LOG1T2n
070970041019      *
070980041019     c                   ENDSR
070990041019      *
071000041019      *---------------------------------------------------------------*
071010050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
071020050217      *?Dati di testata: documento, rif.ordine, note                 ?*
071030041019      *---------------------------------------------------------------*
071040050217     c     PrtDdtLOG1_T3 BEGSR
071050041019      *
071060051110     c                   clear                   dAR8LOG1T3
071070050218     c                   movel     AR8uni        dAR8LOG1T3
071080050706      *
071090050706      * TEST effettiva numericità dei campi previsti numerici:
071100100513     c                   clear                   w053a
071110111013     ***c                   eval      WX    =  %len(§A8LOG1T3b)
071120111013     ***c                                   +  %len(§A8LOG1T3c)
071130111013     ***c                                   +  %len(§A8LOG1T3d)
071140111013     ***c                   eval      w053a =  %subst(dAR8LOG1T3: 4:WX)
071150111013     ***c                   eval      *in31 =  *off
071160111013     ***c     c_Digits      checkr    w053a :  WX                            31
071170111013if  1***c                   if        *in31
071180111013     ***c                   clear                   §A8LOG1T3b
071190111013     ***c                   clear                   §A8LOG1T3c
071200111013     ***c                   clear                   §A8LOG1T3d
071210111013e   1***c                   endif
071220121116     ***c                   eval      WX    =  %len(§A8LOG1T3b)
071230121116     ***c                   eval      w053a =  %subst(dAR8LOG1T3: 4:WX)
071240121116     ***c                   eval      *in31 =  *off
071250121116     ***c     c_Digits      checkr    w053a :  WX                            31
071260121116if  1***c                   if        *in31
071270121116     ***c                   clear                   §A8LOG1T3b
071280121116e   1***c                   endif
071290111013     c                   clear                   w053a
071300111013     c                   eval      WX    =  %len(§A8LOG1T3d)
071310111013     c                   eval      w053a =  %subst(dAR8LOG1T3:15:WX)
071320111013     c                   eval      *in31 =  *off
071330111013     c     c_Digits      checkr    w053a :  WX                            31
071340111013if  1c                   if        *in31
071350111013     c                   clear                   §A8LOG1T3d
071360111013e   1c                   endif
071370041019      *
071380041019      * > dati in testata DDT
071390041019      * - - numero e data documento (DDT)
071400050707     c                   eval      PLOG1nsr = §A8LOG1T3a
071410121116     ***c                   eval      PLOG1aas = §A8LOG1T3b
071420121116     c                   if        §A8Log1T3b <> *blanks   and
071430121116     c                             §A8Log1T3b <> *zeros
071440121116     c                   eval      PLOG1aas = §A8LOG1T3b
071450121116     c                   endif
071460111013     ***c                   if        §A8Log1T3c <> *zeros
071470111013     ***c                   eval      PLOG1nsp = %editc(§A8LOG1T3c:'X')
071480111013     ***c                   endif
071490111013     c                   if        §A8Log1T3c <> *blanks   and
071500111013     c                             §A8Log1T3c <> *zeros
071510111013     c                   eval      PLOG1nsp = §A8LOG1T3c
071520111013     c                   endif
071530100319     c                   if        §A8Log1T3a <> *blanks   and
071540121116     c                             §A8Log1T3b <> *blanks   and
071550100319     c                             §A8Log1T3b <> *zeros
071560100319     c                   eval      PLog1sepA = '/'
071570100319     c                   endif
071580121115     c                   if        §A8Log1T3b <> *zeros    and
071590121115     c                             §A8Log1T3c <> *blanks   and
071600100319     c                             §A8Log1T3c <> *zeros
071610100319     c                   eval      PLog1sepB = '/'
071620100319     c                   endif
071630110707      *     giro la data del documento
071640050707if  1c                   if        §A8LOG1T3d > *zeros
071650060922     c                   eval      *in31 =  *off
071660060922     c     *iso          test(d)                 §A8LOG1T3d             31
071670060922if  2c                   if        NOT *in31
071680050707     c     *iso          movel     §A8LOG1T3d    Data_EUR
071690050707     c     *eur          movel     Data_EUR      PLOG1dsp
071700060922x   2c                   else
071710060922     c                   clear                   PLOG1dsp
071720060922e   2c                   endif
071730050707e   1c                   endif
071740041019      * - - riferimento ordine
071750050707     c                   eval      PLOG1ref = §A8LOG1T3e
071760041019      * - - note consegna
071770050707     c                   eval      PLOG1not = §A8LOG1T3f
071780050707     c                   eval      PLOG1nt2 = §A8LOG1T3g
071790060612      * - - intestazione corpo bolla per Logistica tipo stampa L3 o L4
071800060922     c                   if        §AR8tsd = 'L3' or §AR8tsd = 'L4'
071810060612     c                   eval      *in23 = *On
071820060612     c                   eval      PLOG1int = §A8LOG1T3h
071830060612     c                   else
071840060612     c                   eval      *in23 = *off
071850060612     c                   endif
071860041019      *
071870041019      * > Stampa testata DDT - - - - - - - - - - - - - - - - - - - - -*
071880041019      *
071890041019      *   - OverFlow impossibile qui! -
071900050218     c***                exsr      Test_OF
071910050318     c                   exsr      Posiz_DL
071920081017     c                   if            §AR8cxd = *blanks
071930081017     c                             or  DB0fa4  = 'P'
071940050331     c                   write     A4prtDT0
071950050331     c                   endif
071960050218     c                   write     A4prtDL0
071970081222      /free
071980081222         if  *in55;
071990090406           exsr   PrtDdtLOG1_SimilLogo;
072000120530           //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
072010120530           //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
072020120530           //  write  A4LogoBR2;
072030120530           //else;
072040120530           //  write  A4LogoBAR;
072050120530           //endif;
072060120530           write  A4LogoBRT;
072070081222           write  A4prtDLL;
072080081222         endif;
072090081222      /end-free
072100050218     c*** quante righe?  write     A4prtDL0A
072110041028     c                   write     A4prtDL1A
072120050218     c                   write     A4prtDL1B
072130041019      *
072140041019     c                   ENDSR
072150050214      *
072160041019      *---------------------------------------------------------------*
072170050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
072180050217      *?Dati di dettaglio: articolo, quantità, descrizione           ?*
072190041019      *---------------------------------------------------------------*
072200050217     c     PrtDdtLOG1_C1 BEGSR
072210041019      *
072220041019     c                   clear                   A4prtDL2
072230051110     c                   clear                   dAR8LOG1C1
072240050218     c                   movel     AR8uni        dAR8LOG1C1
072250050706      *
072260050706      * TEST effettiva numericità dei campi previsti numerici:
072270100513     c                   clear                   w053a
072280050706     c                   eval      WX    =  %len(§A8LOG1C1b)
072290100513     c                   eval      w053a =  %subst(dAR8LOG1C1:21:WX)
072300050706     c                   eval      *in31 =  *off
072310110426     c     c_Digits      checkr    w053a :  WX                            31
072320050706if  1c                   if        *in31
072330050706     c                   clear                   §A8LOG1C1b
072340050706e   1c                   endif
072350041019      *
072360041019      * > dati in dettaglio DDT
072370041019      * - - codice prodotto
072380050707     c                   eval      PLOG1cpr  = §A8LOG1C1a
072390041019      * - - quantità
072400050707     c                   eval      PLOG1qta  = §A8LOG1C1b
072410041019      * - - descrizione prodotto
072420050707     c                   eval      PLOG1dpr  = §A8LOG1C1c
072430041019      *
072440041019      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
072450041019      *
072460041019     c                   exsr      Test_OF
072470041019     c                   write     A4prtDL2
072480041019      *
072490041019     c                   ENDSR
072500041019      *
072510041019      *---------------------------------------------------------------*
072520050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
072530050217      *?Dati di dettaglio: descrizione                               ?*
072540041019      *---------------------------------------------------------------*
072550050217     c     PrtDdtLOG1_C2 BEGSR
072560041019      *
072570041019     c                   clear                   A4prtDL2
072580051110     c                   clear                   dAR8LOG1C2
072590050218     c                   movel     AR8uni        dAR8LOG1C2
072600041019      *
072610041019      * > dati in note DDT
072620041019      * - - riga descrittiva
072630050707     c                   eval      PLOG1dpr  = §A8LOG1C2a
072640041019      *
072650041019      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
072660041019      *
072670041019     c                   exsr      Test_OF
072680041019     c                   write     A4prtDL2
072690041019      *
072700041019     c                   ENDSR
072710041019      *
072720041019      *---------------------------------------------------------------*
072730050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
072740050217      *?Dati di dettaglio: descrizioni                               ?*
072750041019      *---------------------------------------------------------------*
072760050217     c     PrtDdtLOG1_C3 BEGSR
072770041019      *
072780041019     c                   clear                   A4prtDL3
072790051110     c                   clear                   dAR8LOG1C3
072800050707     c                   movel     AR8uni        dAR8LOG1C3
072810041019      *
072820041019      * > dati in note DDT
072830041019      * - - riga descrittiva
072840050707     c                   eval      PLOG1txt  = §A8LOG1C3a
072850041019      *
072860041019      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
072870041019      *
072880041019     c                   exsr      Test_OF
072890041019     c                   write     A4prtDL3
072900041019      *
072910041019     c                   ENDSR
072920060614      *
072930060614      *---------------------------------------------------------------*
072940060614      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
072950060614      *?Dati di dettaglio: corpo bolla non fincato                   ?*
072960060614      *---------------------------------------------------------------*
072970060614     c     PrtDdtLOG1_C4 BEGSR
072980060614      *
072990060614     c                   clear                   A4prtDL21
073000060614     c                   clear                   dAR8LOG1C4
073010060614     c                   movel     AR8uni        dAR8LOG1C4
073020060614      *
073030060614      * > dati DDT
073040060614      * - - riga descrittiva
073050060614     c                   eval      PLOG1dpr1 = §A8LOG1C4a
073060060614      *
073070060614      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
073080060614      *
073090060614     c                   exsr      Test_OF
073100060614     c                   write     A4prtDL21
073110060614      *
073120060614     c                   ENDSR
073130090323      *
073140090323      *---------------------------------------------------------------*
073150090323      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
073160090323      *?Dati di dettaglio: descrizioni                               ?*
073170090323      *---------------------------------------------------------------*
073180090323     c     PrtDdtLOG1_C5 BEGSR
073190090323      *
073200090323     c                   clear                   A4prtDL31
073210090323     c                   clear                   dAR8LOG1S3
073220090323     c                   movel     AR8uni        dAR8LOG1S3
073230090323      *
073240090323      * > dati in note DDT
073250090323      * - - riga descrittiva
073260090323     c                   eval      PLOG1txt2 = §A8LOG1S3a
073270090323      *
073280090323      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
073290090323      *
073300090323     c                   exsr      Test_OF
073310090323     c                   write     A4prtDL31
073320090323      *
073330090323     c                   ENDSR
073340041019      *
073350041019      *---------------------------------------------------------------*
073360050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
073370050217      *?Dati di fondo: beni, colli, peso, volume, c/a, porto, trasp. ?*
073380041019      *---------------------------------------------------------------*
073390050217     c     PrtDdtLOG1_P1 BEGSR
073400041019      *
073410051110     c                   clear                   dAR8LOG1P1
073420050707     c                   movel     AR8uni        dAR8LOG1P1
073430050706      *
073440050706      * TEST effettiva numericità dei campi previsti numerici:
073450100513     c                   clear                   w053a
073460050706     c                   eval      WX    =  %len(§A8LOG1P1b)
073470050706     c                                   +  %len(§A8LOG1P1c)
073480050706     c                                   +  %len(§A8LOG1P1d)
073490100513     c                   eval      w053a =  %subst(dAR8LOG1P1:42:WX)
073500050706     c                   eval      *in31 =  *off
073510110426     c     c_Digits      checkr    w053a :  WX                            31
073520050706if  1c                   if        *in31
073530050706     c                   clear                   §A8LOG1P1b
073540050706     c                   clear                   §A8LOG1P1c
073550050706     c                   clear                   §A8LOG1P1d
073560050706e   1c                   endif
073570100513     c                   clear                   w053a
073580050706     c                   eval      WX    =  %len(§A8LOG1P1f)
073590100513     c                   eval      w053a =  %subst(dAR8LOG1P1:67:WX)
073600050706     c                   eval      *in31 =  *off
073610110426     c     c_Digits      checkr    w053a :  WX                            31
073620050706if  1c                   if        *in31
073630050706     c                   clear                   §A8LOG1P1f
073640050706e   1c                   endif
073650041019      *
073660041019      * > dati in chiusura DDT
073670041019      * - - aspetto esteriore dei beni
073680050707     c                   eval      PLOG1aeb  = §A8LOG1P1a
073690041019      * - - numero colli
073700050707     c                   eval      PLOG1ncl  = §A8LOG1P1b
073710041019      * - - peso in kg
073720050707     c                   eval      PLOG1pkg  = §A8LOG1P1c
073730041019      * - - volume in mc
073740050707     c                   eval      PLOG1vlm  = §A8LOG1P1d
073750041019      * - - c/assegno
073760050707     c                   eval      PLOG1cva  = §A8LOG1P1e
073770050707     c                   eval      PLOG1cas  = §A8LOG1P1f
073780041020      * - - causale trasporto
073790050707     c                   eval      PLOG1ctr  = §A8LOG1P1g
073800041020      * - - porto
073810050707     c                   eval      PLOG1pto  = §A8LOG1P1h
073820041020      * - - trasporto a cura
073830050707     c                   eval      PLOG1tra  = §A8LOG1P1i
073840041020      * - - trasportatore
073850050707     c                   eval      PLOG1vet  = §A8LOG1P1j
073860050505      *
073870050505      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
073880050505      *
073890050505      * - - Verifica OverFlow
073900080925      /free
073910080925           select;
073920080925             when  NOT *in45;
073930080925               wMaxLine = $MaxLine4;
073940080925             when  *in45   and   NOT *in46;
073950080925               wMaxLine = $MaxLine4_pl1;
073960080925             when  *in45   and   *in46;
073970080925               wMaxLine = $MaxLine4_pl2;
073980080925           endsl;
073990080925      /end-free
074000050505     c                   exsr      Test_OF
074010050505      * - - Calcolo della posizione finale di box e linee separatrici
074020050505     c                   exsr      Posiz_DL
074030050505      * - - Stampa parte finale DDT
074040050505     c                   write     A4prtDL4
074050050505      *
074060050505      * > Stampa box, linee e testi fissi (no logo)  - - - - - - - - -*
074070090331     c                   if            §AR8cxd = *blanks
074080090331     c                             or  DB0fa4  = 'P'
074090090331     c                   write     A4prtDT0
074100090331     c                   endif
074110050505     c                   write     A4prtDL0A
074120050505     c                   write     A4prtDL0B
074130050505      *
074140050505      * Azzero il numero di pagina per l'eventuale Packing-List
074150050505      *   (successiva)
074160050505     c                   clear                   Page
074170041019      *
074180041019      * N.B.: la copia per il destinatario della LDV deve comparire
074190041019      *       nell'ultima pagina del DDT per il cliente (vedi subr.
074200041019      *       "ChiusuraLDV")
074210041019      *
074220041019     c                   ENDSR
074230050318      *
074240050318      *---------------------------------------------------------------*
074250050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
074260050318      *?Dati di packing-list: dettaglio                              ?*
074270050318      *---------------------------------------------------------------*
074280050318     c     PrtDdtLOG1_Z1 BEGSR
074290050318      *
074300050318      * Prima della 1ª riga di packing-list: testata packing-list
074310050505if  1c                   if        Page     = *zeros
074320050318     c                   exsr      PrtPckLst_1
074330050318e   1c                   endif
074340050318      *
074350050318     c                   clear                   A4prtDL5
074360051110     c                   clear                   dAR8LOG1Z1
074370050707     c                   movel     AR8uni        dAR8LOG1Z1
074380050706      *
074390050706      * TEST effettiva numericità dei campi previsti numerici:
074400100513     c                   clear                   w053a
074410050706     c                   eval      WX    =  %len(§A8LOG1Z1d)
074420100513     c                   eval      w053a =  %subst(dAR8LOG1Z1:53:WX)
074430050706     c                   eval      *in31 =  *off
074440110426     c     c_Digits      checkr    w053a :  WX                            31
074450050706if  1c                   if        *in31
074460050706     c                   clear                   §A8LOG1Z1d
074470050706e   1c                   endif
074480050318      *
074490050318      * > dati in dettaglio Packing-List
074500050318      * - - sscc
074510050318     c                   eval      PCsscc   = §A8LOG1Z1a
074520050318      * - - gtin
074530050318     c                   eval      PCgtin   = §A8LOG1Z1b
074540050318      * - - codice prodotto
074550050318     c                   eval      PCcpr2   = §A8LOG1Z1c
074560050318      * - - quantità ul
074570050318     c                   eval      PLOG1QTA = §A8LOG1Z1d
074580050318      * - - tipo contenitore
074590050318     c                   eval      PCtcn    = §A8LOG1Z1e
074600050318      * - - codice contenitore
074610050318     c                   eval      PCccn    = §A8LOG1Z1f
074620050318      * - - numero lotto
074630050318     c                   eval      PClot    = §A8LOG1Z1g
074640050318      * - - data scadenza
074650050318     c                   eval      PCdts    = §A8LOG1Z1h
074660050318      *
074670050318      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
074680050318      *
074690050318     c                   exsr      Test_OF
074700050318     c                   write     A4prtDL5
074710050318      *
074720050318     c                   ENDSR
074730080908      *
074740050606      *---------------------------------------------------------------*
074750050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
074760080925      *?Dati di Fattura: Testata                                     ?*
074770050606      *---------------------------------------------------------------*
074780050606     c     PrtDdtLOG1_S1 BEGSR
074790050606      *
074800050606      * > logo Fattura
074810050606     c                   eval      §LogoF   = 'LOGO' + §AR8logof
074820090107      /free
074830090107         // - Impostazione indicatore che condiziona la stampa del logo
074840120530         //   (o quella del "simil-logo" BRT)
074850090107         *in55 = (%subst(§AR8logof : 1 : 3) = 'LOG'       and
074860090107                  (§AR8tsd  = 'L1'  or  §AR8tsd  = 'L3'));
074870090107      /end-free
074880080908      *
074890060609      *? L O G O !! ?
074900060609      *?vedi stampa testata bolla ?
074910071008     c                   if        (§AR8tsd = 'L1' or §AR8tsd = 'L3') and
074920071008     c                             §AR8logof <> *blanks and
074930071008     c                             %subst(AR8tmd:1:3) <> 'LOG'
074940071008     c                   eval      §LogoF   = 'LOGO' + AR8tmd
074950090107      /free
074960090107           // - Impostaz. indicatore che condiziona la stampa del logo
074970120530           //   (o quella del "simil-logo" BRT)
074980090107           *in55 = (%subst(AR8tmd : 1 : 3) = 'LOG');
074990090107      /end-free
075000060609     c                   endif
075010050606      *
075020051110     c                   clear                   dAR8LOG1S1
075030050606     c                   movel     AR8uni        dAR8LOG1S1
075040050706      *
075050050706      * TEST effettiva numericità dei campi previsti numerici:
075060100513     c                   clear                   w053a
075070050706     c                   eval      WX    =  %len(§A8LOG1S1i)
075080100513     c                   eval      w053a =  %subst(dAR8LOG1S1:216:WX)
075090050706     c                   eval      *in31 =  *off
075100110426     c     c_Digits      checkr    w053a :  WX                            31
075110050706if  1c                   if        *in31
075120050706     c                   clear                   §A8LOG1S1i
075130050706e   1c                   endif
075140050707      *
075150050606      * > intestazione fattura
075160050707if  1c                   if        §A8LOG1S1b =  *blanks
075170050707     c                   clear                   PLOG1rsf
075180050707     c                   eval      PLOG1rsf2  =  §A8LOG1S1a
075190050606x   1c                   else
075200050707     c                   eval      PLOG1rsf   =  §A8LOG1S1a
075210050707     c                   eval      PLOG1rsf2  =  §A8LOG1S1b
075220050606e   1c                   endif
075230050707     c                   eval      PLOG1inf   =  §A8LOG1S1c
075240050707     c                   eval      PLOG1inf2  =  §A8LOG1S1d
075250050707if  1c                   if        §A8LOG1S1e >  *zeros
075260050707     c                   eval      PLOG1caf   =  §A8LOG1S1e
075270050606e   1c                   endif
075280050707     c                   eval      PLOG1lof   =  §A8LOG1S1f
075290050606      * > partita IVA - codice fiscale
075300050707     c                   eval      PLOG1IVA   =  §A8LOG1S1g
075310050606      * > n./data fattura
075320050707     c                   eval      PLOG1nft   =  §A8LOG1S1h
075330050606      *     giro la data fattura
075340050706if  1c                   if        §A8LOG1S1i >  *zeros
075350060922     c                   eval      *in31 =  *off
075360060922     c     *iso          test(d)                 §A8LOG1S1i             31
075370060922if  2c                   if        NOT *in31
075380050707     c     *iso          movel     §A8LOG1S1i    Data_EUR
075390050707     c     *eur          movel     Data_EUR      PLOG1dft
075400060922x   2c                   else
075410060922     c                   clear                   PLOG1dft
075420060922e   2c                   endif
075430050706e   1c                   endif
075440050606      * > rif.ns.spedizione
075450060922     c                   eval      PLOG1nspa =   §A8LOG1S1j
075460050606      * > intestazione corpo fattura
075470050707     c                   eval      PLOG1tesf  =  §A8LOG1S1k
075480050707      *
075490050606      * > Stampa testata FATTURA - - - - - - - - - - - - - - - - - - -*
075500081015      *
075510080925      /free
075520081015         // - Forzatura del salto pagina
075530081015         CurLineA4 = wMaxLine + 1;
075540080925      /end-free
075550080925     c                   exsr      Test_OF
075560081015      /free
075570090323         // - Impostazione dei nuovi limiti di pagina
075580090323         select;
075590090323           when  NOT *in45;
075600090323             wMaxLine = $MaxLine2;
075610090323           when  *in45   and   NOT *in46;
075620090323             wMaxLine = $MaxLine2_pl1;
075630090323           when  *in45   and   *in46;
075640090323             wMaxLine = $MaxLine2_pl2;
075650090323         endsl;
075660081015      /end-free
075670080925     c                   exsr      Posiz_DL
075680081017     c                   if            §AR8cxd = *blanks
075690081017     c                             or  DB0fa4  = 'P'
075700080925     c                   write     A4prtDT0
075710080925     c                   endif
075720080925     c                   write     A4prtDL0D
075730050606     c                   write     A4prtDL1D
075740050606     c                   write     A4prtDL1E
075750050707      *
075760050606     c                   ENDSR
075770050606      *
075780050606      *---------------------------------------------------------------*
075790050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
075800050606      *?Dati di dettaglio corpo: FATTURA                             ?*
075810050606      *---------------------------------------------------------------*
075820050606     c     PrtDdtLOG1_S2 BEGSR
075830050606      *
075840050606     c                   clear                   A4prtDL6
075850051110     c                   clear                   dAR8LOG1S2
075860050606     c                   movel     AR8uni        dAR8LOG1S2
075870050606      *
075880050606      * > dati fattura
075890050606      * - - riga descrittiva 1
075900050707     c                   eval      PLOG1dft1 = §A8LOG1S2a
075910050606      *
075920050606      * > Stampa dettaglio FATTURA - - - - - - - - - - - - - - - - - -*
075930050606      *
075940050606     c                   exsr      Test_OF
075950050606     c                   write     A4prtDL6
075960050606      *
075970050606     c                   ENDSR
075980050606      *
075990050606      *---------------------------------------------------------------*
076000050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
076010050606      *?Dati di dettaglio corpo: FATTURA                             ?*
076020050606      *---------------------------------------------------------------*
076030050606     c     PrtDdtLOG1_S3 BEGSR
076040050606      *
076050050606     c                   clear                   A4prtDL7
076060051110     c                   clear                   dAR8LOG1S3
076070050707     c                   movel     AR8uni        dAR8LOG1S3
076080050606      *
076090050606      * > dati fattura
076100050606      * - - riga descrittiva 2
076110050707     c                   eval      PLOG1dft2 = §A8LOG1S3a
076120050606      *
076130050606      * > Stampa dettaglio FATTURA - - - - - - - - - - - - - - - - - -*
076140050606      *
076150050606     c                   exsr      Test_OF
076160050606     c                   write     A4prtDL7
076170050606      *
076180050606     c                   ENDSR
076190050606      *
076200050606      *---------------------------------------------------------------*
076210050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
076220050606      *?Dati di fondo: FATTURA                                       ?*
076230050606      *---------------------------------------------------------------*
076240050606     c     PrtDdtLOG1_S4 BEGSR
076250050606      *
076260051110     c                   clear                   dAR8LOG1S4
076270050707     c                   movel     AR8uni        dAR8LOG1S4
076280050706      *
076290050706      * TEST effettiva numericità dei campi previsti numerici:
076300100513     c                   clear                   w053a
076310050706     c                   eval      WX    =  %len(§A8LOG1S4a)
076320050706     c                                   +  %len(§A8LOG1S4b)
076330050706     c                                   +  %len(§A8LOG1S4c)
076340100513     c                   eval      w053a =  %subst(dAR8LOG1S4: 1:WX)
076350050706     c                   eval      *in31 =  *off
076360110426     c     c_Digits      checkr    w053a :  WX                            31
076370050706if  1c                   if        *in31
076380050706     c                   clear                   §A8LOG1S4a
076390050706     c                   clear                   §A8LOG1S4b
076400050706     c                   clear                   §A8LOG1S4c
076410050706e   1c                   endif
076420050606      *
076430050606      * > dati in chiusura FATTURA
076440050606      * - - dati di piede fattura riga 1
076450050707     c                   eval      PLOG1pft1 = §A8LOG1S4d
076460050606      * - - dati di piede fattura riga 2
076470050707     c                   eval      PLOG1pft2 = §A8LOG1S4e
076480050606      * - - dati di piede fattura riga 3
076490050707     c                   eval      PLOG1pft3 = §A8LOG1S4f
076500050606      * - - totale imponibile
076510050707     c                   eval      PLOG1timp = §A8LOG1S4a
076520050606      * - - totale iva
076530050707     c                   eval      PLOG1tiva = §A8LOG1S4b
076540050606      * - - totale fattura
076550050707     c                   eval      PLOG1tft  = §A8LOG1S4c
076560050606      *
076570050606      * > Stampa fondo FATTURA - - - - - - - - - - - - - - - - - - - -*
076580050606      *
076590050606      * - - Verifica OverFlow
076600080925      /free
076610080925           select;
076620080925             when  NOT *in45;
076630080925               wMaxLine = $MaxLine4;
076640080925             when  *in45   and   NOT *in46;
076650080925               wMaxLine = $MaxLine4_pl1;
076660080925             when  *in45   and   *in46;
076670080925               wMaxLine = $MaxLine4_pl2;
076680080925           endsl;
076690080925      /end-free
076700050606     c                   exsr      Test_OF
076710050606      * - - Calcolo della posizione finale di box e linee separatrici
076720050606     c                   exsr      Posiz_DL
076730050606      * - - Stampa parte finale FATTURA
076740050606     c                   write     A4prtDL8
076750050606      *
076760050606      * > Stampa box, linee (no logo)   - - - - - - - - - - - - - - -*
076770050606     c                   write     A4prtDL0E
076780050606     c                   write     A4prtDL0F
076790080908      *
076800050606     c                   ENDSR
076810050606      *
076820050606      *---------------------------------------------------------------*
076830050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
076840050606      *?Dati di fondo note: FATTURA                                  ?*
076850050606      *---------------------------------------------------------------*
076860050606     c     PrtDdtLOG1_S5 BEGSR
076870050606      *
076880051110     c                   clear                   dAR8LOG1S5
076890050707     c                   movel     AR8uni        dAR8LOG1S5
076900050606      *
076910050606      * > dati in chiusura FATTURA
076920050606      * - - nota 1
076930050707     c                   eval      PLOG1not1 = §A8LOG1S5a
076940050606      * - - nota 2
076950050707     c                   eval      PLOG1not2 = §A8LOG1S5b
076960050606      * - - nota 3
076970050707     c                   eval      PLOG1not3 = §A8LOG1S5c
076980050606      *
076990050606      * > Stampa fondo FATTURA - - - - - - - - - - - - - - - - - - - -*
077000050606      * - - Stampa parte finale FATTURA
077010050606     c                   write     A4prtDL9
077020080908      *
077030050606     c                   ENDSR
077040050321      *
077050050321      *---------------------------------------------------------------*
077060050414      *?Impostazione nuova pagina di DDT Logistica / Sait            ?*
077070050321      *---------------------------------------------------------------*
077080050321     c     NewPageDL     BEGSR
077090050321      *
077100080925      /free
077110080925         // Impostazione nuovo limite di OF
077120080925         SELECT;
077130080925           // Piede DDT   oppure   Piede Fattura
077140080925           WHEN  AR8trk = 'P1'   or   AR8trk = 'S4';
077150080925             select;
077160080925               when  NOT *in45;
077170080925                 wMaxLine = $MaxLine4;
077180080925               when  *in45   and   NOT *in46;
077190080925                 wMaxLine = $MaxLine4_pl1;
077200080925               when  *in45   and   *in46;
077210080925                 wMaxLine = $MaxLine4_pl2;
077220080925             endsl;
077230080925           // Dettaglio DDT  oppure   Testata & Dettaglio Fattura
077240080925           OTHER;
077250080925             select;
077260080925               when  NOT *in45;
077270090323                 wMaxLine = $MaxLine2;
077280080925               when  *in45   and   NOT *in46;
077290090323                 wMaxLine = $MaxLine2_pl1;
077300080925               when  *in45   and   *in46;
077310090323                 wMaxLine = $MaxLine2_pl2;
077320080925             endsl;
077330080925         ENDSL;
077340080925      /end-free
077350080925      *
077360080825      * RE-impostazione posizioni
077370080825     c                   exsr      Posiz_DL
077380080825      *
077390050321     c                   write     A4prtSP1
077400081006      /free
077410081006         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
077420081006           write  A4prtNUL;
077430081006         endif;
077440081006      /end-free
077450050321      *
077460050505sel 1c                   select
077470050505      * - altra pagina DDT Logistica / Sait
077480050707w   1c                   when          %subst(AR8trk:1:1) <> 'Z'
077490050606     c                             and %subst(AR8trk:1:1) <> 'S'
077500050321     c                   write     A4prtDL1B
077510050505      * - altra pagina Packing-List
077520050505w   1c                   when      %subst(AR8trk:1:1) =  'Z'
077530050502     c                              and   Page        >  *zeros
077540050321     c                   write     A4prtDL1C
077550050606      * - altra pagina Fattura
077560050606w   1c                   when      %subst(AR8trk:1:1) =  'S'
077570050606     c                              and   Page        >  *zeros
077580050606     c                   write     A4prtDL1E
077590050505e   1c                   endsl
077600050321      *
077610050321     c                   ENDSR
077620050318      *
077630050318      *---------------------------------------------------------------*
077640080709      *?Stampa testata della Packing-List per LOGISTICA              ?*
077650050318      *---------------------------------------------------------------*
077660050318     c     PrtPckLst_1   BEGSR
077670050318      *
077680050318      * > Salto a pagina successiva  - - - - - - - - - - - - - - - - -*
077690050318     c                   exsr      Test_OF
077700050318      *
077710050505      * > Stampa box, linee e testi fissi (CON logo)  - - - - - - - - -*
077720050318      *
077730050318      * - - Stampa "Copia per il destinatario" nella pagina successiva
077740081017     c*** poi in Test_OF:   if            §AR8cxd = *blanks
077750081017     c*** poi in Test_OF:             or  DB0fa4  = 'P'
077760050505     c*** poi in Test_OF:   write     A4prtDT0
077770050505     c*** poi in Test_OF:   endif
077780050318      * - - Stampa dati destinatario nel box superiore
077790050318     c                   write     A4prtDL1A
077800050318      * - - Stampa logo e box superiore x destinatario
077810050318     c                   write     A4prtDL0
077820081222      /free
077830081222         if  *in55;
077840090406           exsr   PrtDdtLOG1_SimilLogo;
077850120530           //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
077860120530           //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
077870120530           //  write  A4LogoBR2;
077880120530           //else;
077890120530           //  write  A4LogoBAR;
077900120530           //endif;
077910120530           write  A4LogoBRT;
077920081222           write  A4prtDLL;
077930081222         endif;
077940081222      /end-free
077950050318      * - - Stampa dati di testata nel box generale
077960050318     c                   write     A4prtDL1C
077970050318      * - - Stampa box generale
077980050318     c*** poi in Test_OF:   write     A4prtDL0C
077990050318      *
078000050318      * Impostazione del nuovo limite x overflow in packing-list
078010080925      /free
078020080925         select;
078030080925           when  NOT *in45;
078040090323             wMaxLine = $MaxLine2;
078050080925           when  *in45   and   NOT *in46;
078060090323             wMaxLine = $MaxLine2_pl1;
078070080925           when  *in45   and   *in46;
078080090323             wMaxLine = $MaxLine2_pl2;
078090080925         endsl;
078100080925      /end-free
078110050318      *
078120050318     c                   ENDSR
078130041019      *
078140041020      *---------------------------------------------------------------*
078150050414      *?Posizionamento di linee e box nel DDT LOGISTICA / SAIT       ?*
078160041020      *---------------------------------------------------------------*
078170041021     c     Posiz_DL      BEGSR
078180050218      *
078190080924      /free
078200080924         // Impostazione posizioni di logo/box
078210080924         // (posizioni di "partenza" diverse a seconda del posizionam.
078220080924         //  iniziale nel foglio A4 attualmente in stampa)
078230080924         select;
078240080924           when  NOT *in45;
078250081006             §PBA =  3.255;
078260080924           when  *in45   and   NOT *in46;
078270090422             §PBA = 0.255;
078280080924           when  *in45   and   *in46;
078290090422             §PBA = 6.005;
078300080924         endsl;
078310080924      /end-free
078320080708     c                   eval      §PBB  =  §PBA + 1.27
078330080708     c                   eval      §LOA  =  §PBA + 0.63
078340080708     c                   eval      §PS0  =  §PBA + 0.72
078350080708     c                   eval      §PS1  =  §PS0 + 0.10
078360080708     c                   eval      §PS2  =  §PS1 + 0.10
078370080708     c                   eval      §PS3  =  §PS2 + 0.10
078380080708     c                   eval      §PS4  =  §PS3 + 0.10
078390080708     c                   eval      §PS5  =  §PS4 + 0.10
078400080708      * Impostazione posizioni di logo/box per fattura
078410080708     c                   eval      §PBC  =  §PBA + 0.92
078420080708     c                   eval      §LOB  =  §PBA + 0.72
078430080708      * Impostazione posizioni di note fine fattura
078440080925     c                   eval      §PS6  =  §PBA + 5.35
078450081003     c                   eval      §PS7  =  §PS6 + 0.08
078460081003     c                   eval      §PS8  =  §PS7 + 0.08
078470080708      *
078480050321if  1c                   if        Page  <= 1
078490050218      *   (posizione di "partenza" su 1ª pagina)
078500080924     c                   eval      §PB1  =  §PBA + 1.375
078510050218x   1c                   else
078520050318      *   (posizione di "partenza" su pagine successive)
078530080822     c                   eval      §PB1  =  §PBA
078540050218e   1c                   endif
078550080924     c                   eval      §PB2  =  §PBA + 5.275
078560050218      * Impostazione posizioni di linee (orizzontali e verticali)
078570050218      *   nella parte superiore del box (testata)
078580050218     c                   eval      §LO1  =  §PB1 + 0.13
078590050318if  1c                   if        %subst(AR8trk:1:1) <> 'Z'
078600050218     c                   eval      §LO2  =  §LO1 + 0.13
078610050318     c                   eval      §LO3  =  §LO2 + 0.11
078620050318x   1c                   else
078630080825     c                   eval      §LO2  =  §LO1 + 0.115
078640050318     c                   eval      §LO3  =  §LO2
078650050318e   1c                   endif
078660050607if  1c                   if        %subst(AR8trk:1:1) = 'S'
078670050610     c                   eval      §LO3  =  §PB2 - 0.40
078680050607     c                   EndIf
078690050218      * Impostazione posizioni di linee (orizzontali e verticali)
078700050218      *   nella parte inferiore del box (piede)
078710050318if  1c                   if        %subst(AR8trk:1:1) <> 'Z'
078720080822     c                   eval      §LO4  =  §PB2 - 0.52
078730090422     c                   eval      §LO5  =  §LO4 + 0.13
078740090422     c                   eval      §LO6  =  §LO5 + 0.13
078750090422     c                   eval      §LO7  =  §LO6 + 0.13
078760050318e   1c                   endif
078770050218      * Impostazione lunghezza delle linee verticali
078780050218      *   nella parte superiore del box (testata)
078790050218     c                   eval      §LV1  =  §LO1 - §PB1
078800050218     c                   eval      §LV2  =  §LO2 - §LO1
078810050318if  1c                   if        %subst(AR8trk:1:1) <> 'Z'
078820050218     c                   eval      §LV3  =  §LO3 - §LO2
078830050318e   1c                   endif
078840050607if  1c                   if        %subst(AR8trk:1:1) = 'S'
078850050607     c                   eval      §LV2  =  0.40
078860050607     c                   EndIf
078870050218      * La lunghezza delle linee verticali separatrici nel dettaglio
078880050218      *   dipende dalla fase di stampa: se si sta stampando ancora il
078890050218      *   dettaglio deve essere più lunga, visto che non viene stampa-
078900050218      *   ta la parte finale del DDT...
078910080924if  1c                   if        %subst(AR8trk:1:1) = 'P'
078920080924     c                              and  (§PS1        < $LimOF
078930080924     c                                and NOT *in45
078940080924     c                                OR  §PS1        < ($LimOF - c_Len1_inch)
078950080924     c                                and *in45   and   NOT *in46
078960081003     c                                OR  §PS1        < ($LimOF + c_Len2_inch)
078970080924     c                                and *in45   and   *in46)
078980050318     c                              and   CurLineA4   <  wMaxLine
078990050218     c                   eval      §LV4  =  §LO4 - §LO3
079000050218x   1c                   else
079010050218     c                   eval      §LV4  =  §PB2 - §LO3
079020050218e   1c                   endif
079030080908      *
079040071008     c                   if        §AR8tsd = 'L3' or §AR8tsd = 'L4' and
079050060609     c                             %subst(AR8trk:1:1) <> 'Z' and
079060060609     c                             %subst(AR8trk:1:1) <> 'S'
079070071008     c                   eval      §LV3 = 0
079080071008     c                   eval      §LV4 = 0
079090060609     c                   endif
079100080908      *
079110050502      * - qui imposto le posizioni per il piede del box
079120050318if  1c                   if        %subst(AR8trk:1:1) <> 'Z'
079130050218     c                   eval      §LV5  =  §LO5 - §LO4
079140050218     c                   eval      §LV6  =  §LO6 - §LO5
079150050218     c                   eval      §LV7  =  §PB2 - §LO7
079160050318e   1c                   endif
079170041028      * Impostazione posizioni dei campi di testo verticali
079180080822     c                   eval      §PR1  =  §PB2 - 0.05
079190041020      *
079200041020     c                   ENDSR
079210050113      *
079220050113      *---------------------------------------------------------------*
079230050217      *?Stampa DDT del mittente ARCTE                                ?*
079240050113      *---------------------------------------------------------------*
079250050217     c     DDT_Arcte     BEGSR
079260050113      *
079270081002      /free
079280081002         // Impostazione del nuovo limite x overflow
079290081002         // (Codesto limite è valido solo per la stampa del dettaglio.
079300081002         //   Per la stampa del "fondo" bolla occorre REimpostarlo ad
079310081002         //   un valore più basso.
079320081002         //  Ovvero: stampo più righe di dettaglio che posso; se alla
079330081002         //  fine delle righe di dettaglio non rimane spazio a suffi-
079340081002         //  cienza per il fondo: prima della stampa del fondo si salta
079350081002         //  alla pagina successiva, dove non si stampa alcuna riga di
079360081002         //  dettaglio.)
079370081002         select;
079380081002           when  NOT *in45;
079390081002             wMaxLine = $MaxLine2;
079400081002           when  *in45   and   NOT *in46;
079410090324             wMaxLine = $MaxLine5_pl1;
079420081002           when  *in45   and   *in46;
079430090324             wMaxLine = $MaxLine5_pl2;
079440081002         endsl;
079450081002
079460081002         // Posizionamento iniziale all'interno del foglio A4
079470081002         write  A4prtSP1;
079480081002      /end-free
079490050113      *
079500050113      * Impostazione testi fissi - - - - - - - - - - - - - - - - - - -*
079510050113      *
079520050113     c                   eval      PCLtx1 =
079530050113     c                             'La merce viaggia a rischio e pericolo -
079540050113     c                              del committente. I reclami vanno fatti -
079550050113     c                              per iscritto entro 8 gg. dal ricevimento -
079560050113     c                              delle merce.'
079570050113     c                   eval      PCLtx2 =
079580050113     c                              'Il ritorno di merce deve -
079590050113     c                              essere preventivamente autorizzato -
079600050113     c                              dal Gruppo Arcte. Foro competente -
079610050113     c                              come da condizioni di vendita.'
079620050113      *
079630050113     c                   eval      PCLpx1 =
079640050113     c                             'CONTRIBUTO CONAI ASSOLTO'
079650050217      *
079660130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
079670130315      *  "StampaDDT")
079680050217     c***  KLDV          setll     FIAR8000       (già fatto)
079690050217     c***  KLDV          reade     FIAR8000       (già fatto)
079700050113      *
079710050113      * Reperimento dati, impostazione e stampa DDT mittente  - - - -*
079720050113      *
079730050113if  1c                   dow       NOT %eof(FIAR801L)
079740050113      *
079750050113sel 2c                   select
079760050113      * - Record annullato (non si stampa nulla)
079770050217w   2c                   when      AR8atb  <> *blanks
079780050707      * - Dati di dettaglio: descrizioni generiche
079790050217w   2c                   when      AR8trk   = 'C1'
079800050217     c                   exsr      PrtDdtARCT_C1
079810050217      * - Dati di dettaglio: articolo, descrizioni, taglie, q.tà
079820050217w   2c                   when      AR8trk   = 'C2'
079830050217     c                   exsr      PrtDdtARCT_C2
079840050113      * - Dati di testata: destinatario, cliente di fatturazione
079850050217w   2c                   when      AR8trk   = 'T1'
079860050217     c                   exsr      PrtDdtARCT_T1
079870050113      * - Dati di testata: agente, numero ordine, estremi documento
079880050217w   2c                   when      AR8trk   = 'T2'
079890050217     c                   exsr      PrtDdtARCT_T2
079900050113      * - Dati di fondo: mezzo e causale trasporto, colli, peso
079910050217w   2c                   when      AR8trk   = 'P1'
079920050217     c                   exsr      PrtDdtARCT_P1
079930050113e   2c                   endsl
079940050113      *
079950050113     c     KLDV          reade     FIAR8000
079960050113      *
079970050113e   1c                   enddo
079980050217      *
079990050217      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
080000050217      *
080010050217      * - - Verifica OverFlow
080020050217     c                   exsr      Test_OF
080030050218      *
080040050218      * - - Calcolo la posizione finale del box e di linee separatrici
080050050218     c                   exsr      Posiz_AC
080060050217      *
080070050217      * - - Stampa parte finale DDT
080080050217     c                   write     A4prtAC4
080090050217      *
080100050217      * > Stampa box, linee e testi fissi (no logo)  - - - - - - - - -*
080110081017     c                   if            §AR8cxd = *blanks
080120081017     c                             or  DB0fa4  = 'P'
080130050331     c                   write     A4prtDT0
080140050331     c                   endif
080150050217     c                   write     A4prtAC0A
080160050217     c                   write     A4prtAC0B
080170050217      *
080180050217      * Reimpostazione del vecchio limite x overflow
080190081002      /free
080200081002         select;
080210081002           when  NOT *in45;
080220081002             wMaxLine = $MaxLine2;
080230081002           when  *in45   and   NOT *in46;
080240081002             wMaxLine = $MaxLine2_pl1;
080250081002           when  *in45   and   *in46;
080260081002             wMaxLine = $MaxLine2_pl2;
080270081002         endsl;
080280081002      /end-free
080290050113      *
080300050113     c                   ENDSR
080310050214      *
080320050113      *---------------------------------------------------------------*
080330050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
080340050217      *?Dati di testata: destinatario, cliente di fatturazione       ?*
080350050113      *---------------------------------------------------------------*
080360050217     c     PrtDdtARCT_T1 BEGSR
080370050114      *
080380050114      * > logo
080390050114     c                   eval      §Logo  = 'LOGO' + AR8tmd
080400050113      *
080410051110     c                   clear                   dAR8ARCTT1
080420050707     c                   movel     AR8uni        dAR8ARCTT1
080430050706      *
080440050706      * TEST effettiva numericità dei campi previsti numerici:
080450100513     c                   clear                   w053a
080460050706     c                   eval      WX    =  %len(§ARCTT1a02)
080470100513     c                   eval      w053a =  %subst(dAR8ARCTT1:  7:WX)
080480050706     c                   eval      *in31 =  *off
080490110426     c     c_Digits      checkr    w053a :  WX                            31
080500050706if  1c                   if        *in31
080510050706     c                   clear                   §ARCTT1a02
080520050706e   1c                   endif
080530050706     c                   eval      WX    =  %len(§ARCTT1b02)
080540100513     c                   eval      w053a =  %subst(dAR8ARCTT1:163:WX)
080550050706     c                   eval      *in31 =  *off
080560110426     c     c_Digits      checkr    w053a :  WX                            31
080570050706if  1c                   if        *in31
080580050706     c                   clear                   §ARCTT1b02
080590050706e   1c                   endif
080600050113      *
080610050113      * > destinatario
080620050707     c                   eval      PARCTccd  = §ARCTT1a01
080630050707     c                   eval      PARCTccd1 = §ARCTT1a02
080640050707     c                   eval      PARCTrsd  = §ARCTT1a04
080650050707     c                   eval      PARCTrsd2 = §ARCTT1a05
080660050707     c                   eval      PARCTind  = §ARCTT1a06
080670050707     c                   eval      PARCTcad  = §ARCTT1a07
080680050707     c                   eval      PARCTlod  = §ARCTT1a08
080690050113      * > cliente di fatturazione
080700050707     c                   eval      PARCTccf  = §ARCTT1b01
080710050707     c                   eval      PARCTccf1 = §ARCTT1b02
080720050707     c                   eval      PARCTccf2 = §ARCTT1b03
080730050707     c                   eval      PARCTrsf  = §ARCTT1b04
080740050707     c                   eval      PARCTrsf2 = §ARCTT1b05
080750050707     c                   eval      PARCTinf  = §ARCTT1b06
080760050707     c                   eval      PARCTcaf  = §ARCTT1b07
080770050707     c                   eval      PARCTlof  = §ARCTT1b08
080780050113      * > magazzino spedizione
080790050707     c                   eval      PARCTmag  = §ARCTT1c01
080800050113      *
080810050113     c                   ENDSR
080820050113      *
080830050113      *---------------------------------------------------------------*
080840050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
080850050217      *?Dati di testata: documento                                   ?*
080860050113      *---------------------------------------------------------------*
080870050217     c     PrtDdtARCT_T2 BEGSR
080880050113      *
080890051110     c                   clear                   dAR8ARCTT2
080900050707     c                   movel     AR8uni        dAR8ARCTT2
080910050706      *
080920050706      * TEST effettiva numericità dei campi previsti numerici:
080930100513     c                   clear                   w053a
080940050706     c                   eval      WX    =  %len(§ARCTT2a02)
080950050706     c                                   +  %len(§ARCTT2a03)
080960100513     c                   eval      w053a =  %subst(dAR8ARCTT2: 9:WX)
080970050706     c                   eval      *in31 =  *off
080980110426     c     c_Digits      checkr    w053a :  WX                            31
080990050706if  1c                   if        *in31
081000050706     c                   clear                   §ARCTT2a02
081010050706     c                   clear                   §ARCTT2a03
081020050706e   1c                   endif
081030050113      *
081040050113      * > dati in testata DDT
081050050113      * - - numero e data documento (DDT)
081060050707     c                   eval      PARCTnsr = §ARCTT2a01
081070050707     c                   eval      PARCTnsp = §ARCTT2a02
081080050707     c                   eval      PARCTdsp = §ARCTT2a03
081090050113      *
081100050113      * > Stampa testata DDT - - - - - - - - - - - - - - - - - - - - -*
081110050113      *
081120050113      *   - OverFlow impossibile qui! -
081130050217     c***                exsr      Test_OF
081140050217     c                   exsr      Posiz_AC
081150081017     c                   if            §AR8cxd = *blanks
081160081017     c                             or  DB0fa4  = 'P'
081170050331     c                   write     A4prtDT0
081180050331     c                   endif
081190050217     c                   write     A4prtAC0
081200050217     c*** quante righe?  write     A4prtAC0A
081210050217     c                   write     A4prtAC1A
081220050217     c                   write     A4prtAC1B
081230050113      *
081240050113     c                   ENDSR
081250050214      *
081260050113      *---------------------------------------------------------------*
081270050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
081280050217      *?Dati di dettaglio: descrizioni generiche                     ?*
081290050113      *---------------------------------------------------------------*
081300050217     c     PrtDdtARCT_C1 BEGSR
081310050113      *
081320050114     c                   clear                   A4prtAC2
081330051110     c                   clear                   dAR8ARCTC1
081340050707     c                   movel     AR8uni        dAR8ARCTC1
081350050217      * > dati in note DDT
081360050217      * - - riga descrittiva
081370050707     c                   eval      PARCTdpr  = §ARCTC1a01
081380050113      *
081390050113      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
081400050113      *
081410050113     c                   exsr      Test_OF
081420050114     c                   write     A4prtAC2
081430050113      *
081440050113     c                   ENDSR
081450050113      *
081460050113      *---------------------------------------------------------------*
081470050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
081480050217      *?Dati di dettaglio: descrizione, taglie, qtà                  ?*
081490050113      *---------------------------------------------------------------*
081500050217     c     PrtDdtARCT_C2 BEGSR
081510050113      *
081520050114     c                   clear                   A4prtAC3
081530051110     c                   clear                   dAR8ARCTC2
081540050707     c                   movel     AR8uni        dAR8ARCTC2
081550050706      *
081560050706      * TEST effettiva numericità dei campi previsti numerici:
081570100513     c                   clear                   w053a
081580050706     c                   eval      WX    =  %len(§ARCTC2b00)
081590050706     c                                   +  %len(§ARCTC2b01)
081600050706     c                                   +  %len(§ARCTC2b02)
081610050706     c                                   +  %len(§ARCTC2b03)
081620050706     c                                   +  %len(§ARCTC2b04)
081630050706     c                                   +  %len(§ARCTC2b05)
081640050706     c                                   +  %len(§ARCTC2b06)
081650050706     c                                   +  %len(§ARCTC2b07)
081660050706     c                                   +  %len(§ARCTC2b08)
081670050706     c                                   +  %len(§ARCTC2b09)
081680100513     c                   eval      w053a =  %subst(dAR8ARCTC2:59:WX)
081690050706     c                   eval      *in31 =  *off
081700110426     c     c_Digits      checkr    w053a :  WX                            31
081710050706if  1c                   if        *in31
081720050706     c                   clear                   §ARCTC2b00
081730050706     c                   clear                   §ARCTC2b01
081740050706     c                   clear                   §ARCTC2b02
081750050706     c                   clear                   §ARCTC2b03
081760050706     c                   clear                   §ARCTC2b04
081770050706     c                   clear                   §ARCTC2b05
081780050706     c                   clear                   §ARCTC2b06
081790050706     c                   clear                   §ARCTC2b07
081800050706     c                   clear                   §ARCTC2b08
081810050706     c                   clear                   §ARCTC2b09
081820050706e   1c                   endif
081830050113      *
081840050113      * > dati in dettaglio DDT
081850050113      * - - descrizione articolo
081860050707     c                   eval      PARCTcpr  = §ARCTC2baa
081870050113      * - - taglia 0
081880050707     c                   eval      PARCTt0   = §ARCTC2b00
081890050113      * - - taglia 1
081900050707     c                   eval      PARCTt1   = §ARCTC2b01
081910050113      * - - taglia 2
081920050707     c                   eval      PARCTt2   = §ARCTC2b02
081930050113      * - - taglia 3
081940050707     c                   eval      PARCTt3   = §ARCTC2b03
081950050113      * - - taglia 4
081960050707     c                   eval      PARCTt4   = §ARCTC2b04
081970050113      * - - taglia 5
081980050707     c                   eval      PARCTt5   = §ARCTC2b05
081990050113      * - - taglia 6
082000050707     c                   eval      PARCTt6   = §ARCTC2b06
082010050113      * - - taglia 7
082020050707     c                   eval      PARCTt7   = §ARCTC2b07
082030050113      * - - taglia 8
082040050707     c                   eval      PARCTt8   = §ARCTC2b08
082050050113      * - - quantità
082060050707     c                   eval      PARCTqta  = §ARCTC2b09
082070050113      *
082080050113      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
082090050113      *
082100050113     c                   exsr      Test_OF
082110050114     c                   write     A4prtAC3
082120050113      *
082130050113     c                   ENDSR
082140050113      *
082150050113      *---------------------------------------------------------------*
082160050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
082170050217      *?Dati di fondo: beni, colli, peso, tot.qtà, porto, trasp.     ?*
082180050113      *---------------------------------------------------------------*
082190050217     c     PrtDdtARCT_P1 BEGSR
082200090324      *
082210090324      /free
082220090324           select;
082230090324             when  NOT *in45;
082240090324               wMaxLine = $MaxLine4;
082250090324             when  *in45   and   NOT *in46;
082260090324               wMaxLine = $MaxLine4_pl3;
082270090324             when  *in45   and   *in46;
082280090324               wMaxLine = $MaxLine4_pl4;
082290090324           endsl;
082300090324      /end-free
082310050113      *
082320051110     c                   clear                   dAR8ARCTP1
082330050707     c                   movel     AR8uni        dAR8ARCTP1
082340050706      *
082350050706      * TEST effettiva numericità dei campi previsti numerici:
082360100513     c                   clear                   w053a
082370050706     c                   eval      WX    =  %len(§ARCTP1a03)
082380100513     c                   eval      w053a =  %subst(dAR8ARCTP1:28:WX)
082390050706     c                   eval      *in31 =  *off
082400110426     c     c_Digits      checkr    w053a :  WX                            31
082410050706if  1c                   if        *in31
082420050706     c                   clear                   §ARCTP1a03
082430050706e   1c                   endif
082440050706     c                   eval      WX    =  %len(§ARCTP1b02)
082450050706     c                                   +  %len(§ARCTP1b03)
082460050706     c                                   +  %len(§ARCTP1b04)
082470100513     c                   eval      w053a =  %subst(dAR8ARCTP1:68:WX)
082480050706     c                   eval      *in31 =  *off
082490110426     c     c_Digits      checkr    w053a :  WX                            31
082500050706if  1c                   if        *in31
082510050706     c                   clear                   §ARCTP1b02
082520050706     c                   clear                   §ARCTP1b03
082530050706     c                   clear                   §ARCTP1b04
082540050706e   1c                   endif
082550050113      *
082560050113      * > dati in chiusura DDT
082570050114      * - - costanti di fine doc. e n. borderò
082580050707     c                   eval      PARCTc1   = §ARCTP1a01
082590050707     c                   eval      PARCTc2   = §ARCTP1a02
082600050707     c                   eval      PARCTc3   = §ARCTP1a03
082610050114      * - - costante segue fattura
082620050707     c                   eval      PARCTc4   = §ARCTp1a04
082630050113      * - - aspetto esteriore dei beni
082640050707     c                   eval      PARCTaeb  = §ARCTP1b01
082650050113      * - - numero colli
082660050707     c                   eval      PARCTncl  = §ARCTP1b02
082670050113      * - - peso in kg
082680050707     c                   eval      PARCTpkg  = §ARCTP1b03
082690050113      * - - totale qtà
082700050707     c                   eval      PARCTtqta = §ARCTP1b04
082710050113      * - - causale trasporto
082720050707     c                   eval      PARCTctr  = §ARCTP1c01
082730050113      * - - porto
082740050707     c                   eval      PARCTpto  = §ARCTP1c02
082750050113      * - - trasporto a cura
082760050707     c                   eval      PARCTtra  = §ARCTP1c03
082770050113      * - - trasportatore
082780050707     c                   eval      PARCTvet  = §ARCTP1d01
082790050217     *** *
082800050217     *** * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
082810050217     *** * ?(impostata a fine ciclo di lettura - sr. DDT_Arcte)?
082820050113      *
082830050113      * N.B.: la copia per il destinatario della LDV deve comparire
082840050113      *       nell'ultima pagina del DDT per il cliente (vedi subr.
082850050113      *       "ChiusuraLDV")
082860050113      *
082870050113     c                   ENDSR
082880081002      /free
082890081002       //--------------------------------------------------------------
082900140303       //?Impostazione nuova pagina di DDT Arcte                       ?
082910081002       //--------------------------------------------------------------
082920081002       BEGSR  NewPageAC;
082930081002
082940081002         // Impostazione del nuovo limite x overflow
082950081002         select;
082960081002           when  NOT *in45;
082970081002             wMaxLine = $MaxLine2;
082980081002           when  *in45   and   NOT *in46;
082990090324             wMaxLine = $MaxLine5_pl1;
083000081002           when  *in45   and   *in46;
083010090324             wMaxLine = $MaxLine5_pl2;
083020081002         endsl;
083030081002
083040081002         write  A4prtSP1;
083050081002         write  A4prtAC1B;
083060081002
083070081002       ENDSR;
083080081002      /end-free
083090050113      *
083100050113      *---------------------------------------------------------------*
083110050217      *?Posizionamento di linee e box nel DDT ARCTE                  ?*
083120050113      *---------------------------------------------------------------*
083130050114     c     Posiz_AC      BEGSR
083140080708      *
083150081002      /free
083160081002         // Impostazione posizioni di logo/box
083170081002         // (posizioni di "partenza" diverse a seconda del posizionam.
083180081002         //  iniziale nel foglio A4 attualmente in stampa)
083190081002         select;
083200081002           when  NOT *in45;
083210081002             §PBA =  3.130;
083220081002           when  *in45   and   NOT *in46;
083230081002             §PBA = 0.260;
083240081002           when  *in45   and   *in46;
083250081002             §PBA = 6.010;
083260081002         endsl;
083270081002      /end-free
083280080708     c                   eval      §PBB  =  §PBA + 1.27
083290080708     c                   eval      §LOA  =  §PBA + 0.63
083300050218      *
083310050217      * Impostazione posizioni di box
083320050217if  1c                   if        Page  <= 1
083330050217      *   (posizione di "partenza" su 1ª pagina)
083340081002     c                   eval      §PB1  =  §PBA + 1.38
083350050217x   1c                   else
083360050217      *   (posizione di "partenza" su pagine successive)
083370081002     c                   eval      §PB1  =  §PBA
083380050217e   1c                   endif
083390081002     c                   eval      §PB2  =  §PBA + 5.395
083400050217      * Impostazione posizioni di linee (orizzontali e verticali)
083410050217      *   nella parte superiore del box (testata)
083420080708     c                   eval      §LO1  =  §PB1 + 0.13
083430080708     c                   eval      §LO2  =  §LO1 + 0.13
083440050217      * Impostazione posizioni di linee (orizzontali e verticali)
083450050217      *   nella parte inferiore del box (piede)
083460081002     c                   eval      §LO3  =  §PBA + 5.005
083470081002     c                   eval      §LO4  =  §PBA + 5.135
083480081002     c                   eval      §LO5  =  §PBA + 5.265
083490050217      * Impostazione lunghezza delle linee verticali
083500050217      *   nella parte superiore del box (testata)
083510050217     c                   eval      §LV1  =  §LO1 - §PB1
083520050217     c                   eval      §LV2  =  §LO2 - §LO1
083530050217      * La lunghezza delle linee verticali separatrici nel dettaglio
083540050217      *   dipende dalla fase di stampa: se si sta stampando ancora il
083550050217      *   dettaglio deve essere più lunga, visto che non viene stampa-
083560050217      *   ta la parte finale del DDT...
083570050217if  1c                   if        %subst(AR8trk:1:1) = 'P'
083580050218     c                             and    §PS1        < $LimOF
083590050218     c                             and    CurLineA4   < wMaxLine
083600050217     c                   eval      §LV3  =  §LO3 - §LO2
083610050217x   1c                   else
083620050217     c                   eval      §LV3  =  §PB2 - §LO2
083630050217e   1c                   endif
083640050217     c                   eval      §LV4  =  §LO4 - §LO3
083650050217     c                   eval      §LV5  =  §LO5 - §LO4
083660050113      * Impostazione posizioni dei campi di testo verticali
083670081002     c                   eval      §PR1  =  §PBA + 5.35
083680081002     c                   eval      §PR2  =  §PBA + 5.47
083690050113      *
083700050113     c                   ENDSR
083710050214      *
083720050214      *---------------------------------------------------------------*
083730050217      *?Stampa DDT del mittente Esprinet                             ?*
083740050214      *---------------------------------------------------------------*
083750050214     c     DDT_Esprinet  BEGSR
083760050214      *
083770081002      /free
083780110405         // -?Impostazione del nuovo limite x overflow?
083790110405         //  ?Codesto limite è valido solo per la stampa del dettaglio.?
083800110405         //  ?Per la stampa del "fondo" bolla occorre REimpostarlo ad?
083810110405         //  ?un valore più basso.?
083820110405         //  ?Ovvero: stampo più righe di dettaglio che posso; se alla?
083830110405         //  ?fine delle righe di dettaglio non rimane spazio a suffi-?
083840110405         //  ?cienza per il fondo: prima della stampa del fondo si salta?
083850110405         //  ?alla pagina successiva, dove non si stampa alcuna riga di?
083860110405         //  ?dettaglio.?
083870081002         select;
083880081002           when  NOT *in45;
083890081002             wMaxLine = $MaxLine2;
083900081002           when  *in45   and   NOT *in46;
083910090324             wMaxLine = $MaxLine5_pl1;
083920081002           when  *in45   and   *in46;
083930090324             wMaxLine = $MaxLine5_pl2;
083940081002         endsl;
083950081002
083960110405         // -?Posizionamento iniziale all'interno del foglio A4?
083970081002         write  A4prtSP1;
083980110405
083990110405         // -?Pulizia tipo record?
084000110405         clear  SAVtrk;
084010081002      /end-free
084020050214      *
084030050214      * Impostazione testi fissi - - - - - - - - - - - - - - - - - - -*
084040050214      *
084050050325     ***c                   eval      PCLtx1 =
084060050325     ***c                             'Settimo Torinese - Tel. 011.895.39.29 *-
084070050325     ***c                              Padova - Tel. 049.870.50.40 *-
084080050325     ***c                              Bologna - Tel. 051.324.249 *-
084090050325     ***c                              Osimo - Tel. 071.781.96.75 *-
084100050325     ***c                              Roma - Tel. 06.723.15.43'
084110050325     ***c                   eval      PCLtx2 =
084120050325     ***c                             'Casalnuovo - Tel. 081.522.30.31 *-
084130050325     ***c                              Cinisello B. - Tel. 02.66.04.11.13 *-
084140050325     ***c                              Verona - Tel. 045.823.01.65 *-
084150050325     ***c                              Cagliari - Tel. 070.253.178'
084160050325     c                   eval      PCLtx3 =
084170131017     c                             'CONTRIBUTO AMBIENTALE CONAI ASSOLTO'
084180131017     c*//                           OVE DOVUTO'
084190050214      *
084200050214     c                   eval      PCLpx2 =
084210050214     c                             'IL PRESENTE DOCUMENTO E'' REGOLATO-
084220050214     c                              DALLE CONDIZIONI GENERALI DI VENDI-
084230050214     c                             TA PUBBLICATE SUL SITO INTERNET ALL-
084240050214     c                             ''INDIRIZZO www.esprinet.com CHE SI-
084250050325     c                              INTENDONO ESPRESSAMENTE CONOSCIUTE-
084260050325     c                              ED ACCETTATE'
084270050214      *
084280050214     c                   eval      PCLpx3a =
084290050214     c                             'NB. IN CASO DI RITARDATO PAGAMENTO-
084300050214     c                             , VERRANNO ADDEBITATI'
084310050214     c                   eval      PCLpx3b =
084320050214     c                             'INTERESSI AL TASSO DEL PRIME RATE -
084330050214     c                             MAGGIORATO DEL 2% A'
084340050214     c                   eval      PCLpx3c =
084350050214     c                             'DECORRERE DAL GIORNO SUCCESSIVO AL-
084360050214     c                             LA SCADENZA DELLA FATTURA'
084370050214      *
084380050214     c                   eval      PCLpx4a =
084390050214     c                             'Si prega di verificare la Vs. ragi-
084400050214     c                             one sociale, indirizzo e'
084410050214     c                   eval      PCLpx4b =
084420050214     c                             'partita IVA, dandoci eventuale sol-
084430050214     c                             lecita rettifica scritta.'
084440050214     c***                eval      PCLpx4c = *blanks
084450050214      *
084460050214     c                   eval      PCLpx5a =
084470050214     c                             '* I PRODOTTI IMPORTATI DAGLI U.S.A.-
084480050214     c                              SONO CON SPECIALE'
084490050214     c                   eval      PCLpx5b =
084500050214     c                             'LICENZA CHE PRECLUDE RIESPORTAZIONI-
084510050214     c                              NON AUTORIZZATE.'
084520050214     c***                eval      PCLpx5c = *blanks
084530050214      *
084540050325     ***c                   eval      PCLpx6  =
084550050325     ***c                             'CONTRIBUTO AMBIENTALE CONAI ASSOLTO-
084560050325     ***c                              OVE DOVUTO'
084570050325     c                   eval      PCLpx6  =
084580050325     c                             'LA SOCIETÀ SI IMPEGNA A VERSARE L''-
084590050325     c                             IVA NEI MODI E TERMINI PREVISTI (S-
084600050325     c                             OLIDARIETÀ ART. 60 BIS DPR 633/72-F-
084610050325     c                             INANZIARIA 2005)'
084620050214      *
084630130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
084640130315      *  "StampaDDT")
084650050214     c***  KLDV          setll     FIAR8000       (già fatto)
084660050214     c***  KLDV          reade     FIAR8000       (già fatto)
084670050214      *
084680050214      * Reperimento dati, impostazione e stampa DDT mittente  - - - -*
084690050214      *
084700050214if  1c                   dow       NOT %eof(FIAR801L)
084710050214      *
084720050214sel 2c                   select
084730050214      * - Record annullato (non si stampa nulla)
084740050217w   2c                   when      AR8atb  <> *blanks
084750050214      * - Dati di dettaglio: articolo, descrizione, quantità, ...
084760050217w   2c                   when      AR8trk   = 'C1'
084770050217     c                   exsr      PrtDdtESP1_C1
084780050214      * - Dati di dettaglio: testo (riga libera)
084790050217w   2c                   when      AR8trk   = 'C2'
084800050217     c                   exsr      PrtDdtESP1_C2
084810050214      * - Dati di testata: destinatario, cliente di fatturazione
084820050217w   2c                   when      AR8trk   = 'T1'
084830050217     c                   exsr      PrtDdtESP1_T1
084840050214      * - Dati di testata: estremi documento, riferimenti, ...
084850050217w   2c                   when      AR8trk   = 'T2'
084860050217     c                   exsr      PrtDdtESP1_T2
084870050214      * - Dati di fondo: totali del documento (netto merce, iva, ...)
084880050217w   2c                   when      AR8trk   = 'P1'
084890050217     c                   exsr      PrtDdtESP1_P1
084900050214      * - Dati di fondo: informazioni vettore
084910050217w   2c                   when      AR8trk   = 'P2'
084920050217     c                   exsr      PrtDdtESP1_P2
084930050214e   2c                   endsl
084940110405      *
084950110405     c                   eval      SAVtrk = AR8trk
084960050214      *
084970050214     c     KLDV          reade     FIAR8000
084980050214      *
084990050214e   1c                   enddo
085000050215      *
085010050215      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
085020050215      *
085030050215      * - - Verifica OverFlow
085040050215     c                   exsr      Test_OF
085050050218      *
085060050218      * - - Calcolo la posizione finale del box e di linee separatrici
085070050218     c                   exsr      Posiz_ES
085080050215      *
085090050215      * - - Stampa parte finale DDT
085100050215     c                   write     A4prtES4
085110050215      *
085120050215      * > Stampa box, linee e testi fissi (no logo)  - - - - - - - - -*
085130081017     c                   if            §AR8cxd = *blanks
085140081017     c                             or  DB0fa4  = 'P'
085150050331     c                   write     A4prtDT0
085160050331     c                   endif
085170050215     c                   write     A4prtES0A
085180050215     c                   write     A4prtES0B
085190050214      *
085200050214      * Reimpostazione del vecchio limite x overflow
085210081002      /free
085220081002         select;
085230081002           when  NOT *in45;
085240081002             wMaxLine = $MaxLine2;
085250081002           when  *in45   and   NOT *in46;
085260081002             wMaxLine = $MaxLine2_pl1;
085270081002           when  *in45   and   *in46;
085280081002             wMaxLine = $MaxLine2_pl2;
085290081002         endsl;
085300081002      /end-free
085310050214      *
085320050214     c                   ENDSR
085330050214      *
085340050214      *---------------------------------------------------------------*
085350050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
085360050217      *?Dati di testata: fatturato a, destinazione, cliente          ?*
085370050214      *---------------------------------------------------------------*
085380050217     c     PrtDdtESP1_T1 BEGSR
085390110405      *
085400110405      * - pulizia record a nuova packing-list
085410110405     c                   if            %subst( SAVtrk : 1 : 1 ) = 'P'
085420110405     c                             or  %subst( SAVtrk : 1 : 1 ) = 'C'
085430110405     c                   exsr      sr_ClearESP1
085440110405     c                   endif
085450050214      *
085460050214      * > logo
085470050214     c                   eval      §Logo  = 'LOGO' + AR8tmd
085480050214      *
085490051110     c                   movel(p)  AR8uni        dAR8ESP1T1
085500050214      *
085510050214      * > fatturato a
085520050707     c                   eval      PCrsf  = §A8ESP1T1a
085530050707     c                   eval      PCinf  = §A8ESP1T1b
085540050707if  1c                   if        §A8ESP1T1c > *zeros
085550050707     c                   eval      PCclpf = §A8ESP1T1c
085560050216     c                                    + ' '
085570050707     c                                    + §A8ESP1T1d
085580050216     c                                    + ' '
085590050707     c                                    + §A8ESP1T1e
085600050216x   1c                   else
085610050216     c                   eval      PCclpf = '     '
085620050216     c                                    + ' '
085630050707     c                                    + §A8ESP1T1d
085640050216     c                                    + ' '
085650050707     c                                    + §A8ESP1T1e
085660050216e   1c                   endif
085670050214      * > destinatazione
085680050707     c                   eval      PCrsd  = §A8ESP1T1f
085690050707     c                   eval      PCind  = §A8ESP1T1g
085700050707if  1c                   if        §A8ESP1T1h > *zeros
085710050707     c                   eval      PCclpd = §A8ESP1T1h
085720050216     c                                    + ' '
085730050707     c                                    + §A8ESP1T1i
085740050216     c                                    + ' '
085750050707     c                                    + §A8ESP1T1j
085760050216x   1c                   else
085770050216     c                   eval      PCclpd = '     '
085780050216     c                                    + ' '
085790050707     c                                    + §A8ESP1T1i
085800050216     c                                    + ' '
085810050707     c                                    + §A8ESP1T1j
085820050216e   1c                   endif
085830050214      * > dati in testata DDT
085840050214      * - - codice cliente
085850050707     c                   eval      PCcli  = §A8ESP1T1k
085860050214      * - - codice fiscale
085870050707     c                   eval      PCcf   = §A8ESP1T1l
085880050214      * - - partita iva
085890050707     c                   eval      PCpi   = §A8ESP1T1m
085900050214      *
085910050214     c                   ENDSR
085920050214      *
085930050214      *---------------------------------------------------------------*
085940050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
085950050217      *?Dati di testata: documento, riferimenti.                     ?*
085960050214      *---------------------------------------------------------------*
085970050217     c     PrtDdtESP1_T2 BEGSR
085980110405      *
085990110405      * - pulizia records a nuova packing-list
086000110405     c                   if            %subst( SAVtrk : 1 : 1 ) = 'P'
086010110405     c                             or  %subst( SAVtrk : 1 : 1 ) = 'C'
086020110405     c                   exsr      sr_ClearESP1
086030110405     c                   endif
086040050214      *
086050051110     c                   movel(p)  AR8uni        dAR8ESP1T2
086060050214      *
086070050214      * > dati in testata DDT
086080050214      * - - tipo documento
086090050707     c                   eval      PCtdo  = §A8ESP1T2a
086100050214      * - - data e numero documento (DDT)
086110050707     c                   eval      PCdtd  = §A8ESP1T2b
086120050707     c                   eval      PCnrd  = §A8ESP1T2c
086130050214      * - - riferimenti mittente
086140050707     c                   eval      PCrif  = §A8ESP1T2d
086150050214      * - - condizioni di pagamento
086160050707     c                   eval      PCpag  = §A8ESP1T2e
086170050214      * - - annotazioni
086180050707     c                   eval      PCann  = §A8ESP1T2f
086190050214      * - - agente
086200050707     c                   eval      PCag2  = §A8ESP1T2g
086210050214      *
086220050214      * > Stampa testata DDT - - - - - - - - - - - - - - - - - - - - -*
086230050214      *
086240050214      *   - OverFlow impossibile qui! -
086250050215     c***                exsr      Test_OF
086260050215     c                   exsr      Posiz_ES
086270081017     c                   if            §AR8cxd = *blanks
086280081017     c                             or  DB0fa4  = 'P'
086290050331     c                   write     A4prtDT0
086300050331     c                   endif
086310050214     c                   write     A4prtES0
086320081002     c*** (NON qui:)     write     A4prtES0A
086330081002     c                   write     A4prtSP1
086340050215     c                   write     A4prtES1A
086350050215     c                   write     A4prtES1B
086360050214      *
086370050214     c                   ENDSR
086380050214      *
086390050214      *---------------------------------------------------------------*
086400050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
086410050217      *?Dati di dettaglio: articolo, descrizione, quantità, ...      ?*
086420050214      *---------------------------------------------------------------*
086430050217     c     PrtDdtESP1_C1 BEGSR
086440110405      *
086450110405      * - pulizia records a nuova packing-list
086460110405     c                   if        %subst( SAVtrk : 1 : 1 ) = 'P'
086470110405     c                   exsr      sr_ClearESP1
086480110405     c                   endif
086490050214      *
086500050214     c                   clear                   A4prtES2
086510051110     c                   movel(p)  AR8uni        dAR8ESP1C1
086520050214      *
086530050214      * > dati in dettaglio DDT
086540050214      * - - codice prodotto
086550050707     c                   eval      PCcpr  = §A8ESP1C1a
086560050214      * - - descrizione prodotto
086570050707     c                   eval      PCdpr  = §A8ESP1C1b
086580050214      * - - quantità
086590050707     c                   eval      PCqta  = '      ' + §A8ESP1C1c
086600050214      * - - prezzo unitario
086610050707     c                   eval      PCpru  = §A8ESP1C1d
086620050214      * - - cambio: divisa + valore
086630050707     c                   eval      PCdiv  = §A8ESP1C1e
086640050707     c                   eval      PCcmb  = §A8ESP1C1f
086650050214      * - - % sconto
086660050707     c                   eval      PCprc  = §A8ESP1C1g
086670050214      * - - importo
086680050707     c                   eval      PCimp  = §A8ESP1C1h
086690050214      * - - codice iva
086700050707     c                   eval      PCciv  = §A8ESP1C1i
086710050214      *
086720050214      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
086730050214      *
086740050214     c                   exsr      Test_OF
086750050214     c                   write     A4prtES2
086760050214      *
086770050214     c                   ENDSR
086780050214      *
086790050214      *---------------------------------------------------------------*
086800050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
086810050217      *?Dati di dettaglio: note (riga libera)                        ?*
086820050214      *---------------------------------------------------------------*
086830050217     c     PrtDdtESP1_C2 BEGSR
086840110405      *
086850110405      * - pulizia records a nuova packing-list
086860110405     c                   if        %subst( SAVtrk : 1 : 1 ) = 'P'
086870110405     c                   exsr      sr_ClearESP1
086880110405     c                   endif
086890050214      *
086900050215     c                   clear                   A4prtES3
086910051110     c                   movel(p)  AR8uni        dAR8ESP1C2
086920050214      *
086930050214      * > dati in note DDT
086940050214      * - - riga descrittiva
086950050707     c                   eval      PCrig  = §A8ESP1C2a
086960050214      *
086970050214      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
086980050214      *
086990050214     c                   exsr      Test_OF
087000050214     c                   write     A4prtES3
087010050214      *
087020050214     c                   ENDSR
087030050214      *
087040050214      *---------------------------------------------------------------*
087050050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
087060050217      *?Dati di fondo: totali documento                              ?*
087070050214      *---------------------------------------------------------------*
087080050217     c     PrtDdtESP1_P1 BEGSR
087090050214      *
087100081002      /free
087110081002           select;
087120081002             when  NOT *in45;
087130081002               wMaxLine = $MaxLine6;
087140081002             when  *in45   and   NOT *in46;
087150081002               wMaxLine = $MaxLine6_pl1;
087160081002             when  *in45   and   *in46;
087170081002               wMaxLine = $MaxLine6_pl2;
087180081002           endsl;
087190081002      /end-free
087200051110     c                   movel(p)  AR8uni        dAR8ESP1P1
087210050215      *
087220050215      * > dati in fondo DDT
087230050215      * - - netto merce
087240050707     c                   eval      PCinm  = §A8ESP1P1a
087250050215      * - - spese di trasporto
087260050707     c                   eval      PCist  = §A8ESP1P1b
087270050215      * - - spese imballo
087280050707     c                   eval      PCisi  = §A8ESP1P1c
087290050215      * - - spese bolli
087300050707     c                   eval      PCisb  = §A8ESP1P1d
087310050215      * - - spese amministrative
087320050707     c                   eval      PCisa  = §A8ESP1P1e
087330050215      * - - spese varie
087340050707     c                   eval      PCisv  = §A8ESP1P1f
087350050215      * - - codici iva
087360050707     c                   eval      PCic1  = §A8ESP1P1g
087370050707     c                   eval      PCic2  = §A8ESP1P1m
087380050707     c                   eval      PCic3  = §A8ESP1P1s
087390050215      * - - descrizioni iva
087400050707     c                   eval      PCid1  = §A8ESP1P1h
087410050707     c                   eval      PCid2  = §A8ESP1P1n
087420050707     c                   eval      PCid3  = §A8ESP1P1t
087430050215      * - - imponibili iva: divise
087440050707     c                   eval      PCie1  = §A8ESP1P1i
087450050707     c                   eval      PCie2  = §A8ESP1P1o
087460050707     c                   eval      PCie3  = §A8ESP1P1u
087470050215      * - - imponibili iva: importi
087480050707     c                   eval      PCii1  = §A8ESP1P1j
087490050707     c                   eval      PCii2  = §A8ESP1P1p
087500050707     c                   eval      PCii3  = §A8ESP1P1v
087510050215      * - - aliquote iva
087520050707     c                   eval      PCia1  = §A8ESP1P1k
087530050707     c                   eval      PCia2  = §A8ESP1P1q
087540050707     c                   eval      PCia3  = §A8ESP1P1w
087550050215      * - - imposte iva
087560050707     c                   eval      PCiv1  = §A8ESP1P1l
087570050707     c                   eval      PCiv2  = §A8ESP1P1r
087580050707     c                   eval      PCiv3  = §A8ESP1P1x
087590050215      * - - Totali documento: divise + importi
087600050707     c                   eval      PCtdd  = §A8ESP1P1y
087610050707     c                   eval      PCtdi  = §A8ESP1P1z
087620050707     c                   eval      PCtdd2 = §A8ESP1P11
087630050707     c                   eval      PCtdi2 = §A8ESP1P12
087640050214      *
087650050214     c                   ENDSR
087660050214      *
087670050214      *---------------------------------------------------------------*
087680050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
087690050217      *?Dati di fondo: informazioni vettore                          ?*
087700050214      *---------------------------------------------------------------*
087710050217     c     PrtDdtESP1_P2 BEGSR
087720050214      *
087730081002      /free
087740081002           select;
087750081002             when  NOT *in45;
087760081002               wMaxLine = $MaxLine6;
087770081002             when  *in45   and   NOT *in46;
087780081002               wMaxLine = $MaxLine6_pl1;
087790081002             when  *in45   and   *in46;
087800081002               wMaxLine = $MaxLine6_pl2;
087810081002           endsl;
087820081002      /end-free
087830051110     c                   movel(p)  AR8uni        dAR8ESP1P2
087840050215      *
087850050215      * > dati in fondo DDT
087860050215      * - - resa
087870050707     c                   eval      PCres  = §A8ESP1P2a
087880050215      * - - trasporto a cura del
087890050707     c                   eval      PCtra  = §A8ESP1P2b
087900050215      * - - vettore
087910110426     c*//                eval      PCvet  = $Bartolini
087920110426     c*//                                 + ' - '
087930110426     c*//                                 + $Intest2
087940110426     c*//                                 + ' (BO)'
087950050707     c                   eval      PCvet2 = §A8ESP1P2c
087960050215      * - - causale trasporto
087970050707     c                   eval      PCctr2 = §A8ESP1P2d
087980050215      * - - aspetto esteriore dei beni
087990050707     c                   eval      PCaeb  = §A8ESP1P2e
088000050215      * - - numero colli
088010050707     c                   eval      PCncl2 = §A8ESP1P2f
088020050215      * - - peso complessivo
088030050707     c                   eval      PCpes  = §A8ESP1P2g
088040050215      * - - volume
088050050707     c                   eval      PCvol  = §A8ESP1P2h
088060050215      * - - inizio trasporto: data + ora
088070050707     c                   eval      PCdit  = §A8ESP1P2i
088080050707     c                   eval      PCoit  = §A8ESP1P2j
088090050215     *** *
088100050215     *** * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
088110050215     *** * ?(impostata a fine ciclo di lettura - sr. DDT_Esprinet)?
088120050217      *
088130050217      * N.B.: la copia per il destinatario della LDV deve comparire
088140050217      *       nell'ultima pagina del DDT per il cliente (vedi subr.
088150050217      *       "ChiusuraLDV")
088160050214      *
088170050214     c                   ENDSR
088180081002      /free
088190081002       //--------------------------------------------------------------
088200110405       //?Impostazione nuova pagina di DDT Esprinet                    ?
088210081002       //--------------------------------------------------------------
088220081002       BEGSR  NewPageES;
088230081002
088240110405         // -?Impostazione del nuovo limite x overflow?
088250081002         select;
088260081002           when  NOT *in45;
088270081002             wMaxLine = $MaxLine2;
088280081002           when  *in45   and   NOT *in46;
088290090324             wMaxLine = $MaxLine5_pl1;
088300081002           when  *in45   and   *in46;
088310090324             wMaxLine = $MaxLine5_pl2;
088320081002         endsl;
088330081002
088340081002         write  A4prtSP1;
088350081002         write  A4prtES1B;
088360081002
088370081002       ENDSR;
088380110405
088390110405       //--------------------------------------------------------------
088400110405       //?Pulizia campi a nuova packing-list relativa alla stessa LdV  ?
088410110405       //--------------------------------------------------------------
088420110405       BEGSR  sr_ClearESP1;
088430110405
088440110405         // -?Stampa fondo DDT?
088450110405
088460110405         // - -?Calcolo la posizione finale del box e di linee?
088470110405         //    ?separatrici?
088480110405         exsr  Posiz_ES;
088490110405
088500110405         // - -?Stampa parte finale DDT?
088510110405         write  A4prtES4;
088520110405
088530110405         // - -?Stampa box, linee e testi fissi (no logo)?
088540110405         if  §AR8cxd = *blank  or  DB0fa4 = 'P';
088550110405           write  A4prtDT0;
088560110405         endif;
088570110405         write  A4prtES0A;
088580110405         write  A4prtES0B;
088590110405
088600110405         // -?Pulizia record?
088610110405         select;
088620110405           when  %subst( AR8trk : 1 : 1 ) = 'T';
088630110405             clear  A4prtES0;
088640110405             clear  A4prtES0A;
088650110405             clear  A4prtES0B;
088660110405             clear  A4prtES1A;
088670110405             clear  A4prtES1B;
088680110405             clear  A4prtES2;
088690110405             clear  A4prtES3;
088700110405             clear  A4prtES4;
088710110405           when  %subst( AR8trk : 1 : 1 ) = 'C';
088720110405             clear  A4prtES2;
088730110405             clear  A4prtES3;
088740110405             clear  A4prtES4;
088750110405         endsl;
088760110405
088770110405         // -?Impostazione testi fissi?
088780131017         //PCLtx3 = 'CONTRIBUTO AMBIENTALE CONAI ASSOLTO OVE DOVUTO';
088790131017         PCLtx3 = 'CONTRIBUTO AMBIENTALE CONAI ASSOLTO';
088800110405
088810110405         PCLpx2 = 'IL PRESENTE DOCUMENTO E'' REGOLATO DALLE +
088820110405                   CONDIZIONI GENERALI DI VENDITA PUBBLICATE SUL SITO +
088830110405                   INTERNET ALL''INDIRIZZO www.esprinet.com CHE SI +
088840110405                   INTENDONO ESPRESSAMENTE CONOSCIUTE ED ACCETTATE';
088850110405
088860110405         PCLpx3a = 'NB. IN CASO DI RITARDATO PAGAMENTO, VERRANNO +
088870110405                    ADDEBITATI';
088880110405         PCLpx3b = 'INTERESSI AL TASSO DEL PRIME RATE MAGGIORATO +
088890110405                    DEL 2% A';
088900110405         PCLpx3c =
088910110405                   'DECORRERE DAL GIORNO SUCCESSIVO ALLA SCADENZA +
088920110405                    DELLA FATTURA';
088930110405
088940110405         PCLpx4a = 'Si prega di verificare la Vs. ragione sociale +
088950110405                    indirizzo e';
088960110405         PCLpx4b = 'partita IVA, dandoci eventuale sollecita +
088970110405                    rettifica scritta.';
088980110405
088990110405         PCLpx5a = '* I PRODOTTI IMPORTATI DAGLI U.S.A. SONO CON +
089000110405                    SPECIALE';
089010110405         PCLpx5b = 'LICENZA CHE PRECLUDE RIESPORTAZIONI NON +
089020110405                    AUTORIZZATE.';
089030110405
089040110405         PCLpx6  = 'LA SOCIETÀ SI IMPEGNA A VERSARE L''IVA NEI MODI +
089050110405                    E TERMINI PREVISTI (SOLIDARIETÀ ART. 60 BIS DPR +
089060110405                    633/72-FINANZIARIA 2005)';
089070110405
089080110405         // -?Salto Pagina?
089090110405         exsr  Test_OF;
089100110405
089110110405         // -?Re-Impostazione limiti di overflow?
089120110405         select;
089130110405           when  NOT *in45;
089140110405             wMaxLine = $MaxLine2;
089150110405           when  *in45   and   NOT *in46;
089160110405             wMaxLine = $MaxLine5_pl1;
089170110405           when  *in45   and   *in46;
089180110405             wMaxLine = $MaxLine5_pl2;
089190110405         endsl;
089200110405
089210110405         // -?Posizionamento iniziale all'interno del foglio A4?
089220110405         write  A4prtSP1;
089230110405
089240110405       ENDSR;
089250081002      /end-free
089260050214      *
089270050214      *---------------------------------------------------------------*
089280050217      *?Posizionamento di linee e box nel DDT ESPRINET               ?*
089290050214      *---------------------------------------------------------------*
089300050215     c     Posiz_ES      BEGSR
089310080708      *
089320081002      /free
089330081002         // Impostazione posizioni di logo/box
089340081002         // (posizioni di "partenza" diverse a seconda del posizionam.
089350081002         //  iniziale nel foglio A4 attualmente in stampa)
089360081002         select;
089370081002           when  NOT *in45;
089380081006             §PBA =  3.130;
089390081006             //§PBA =  3.255;        (più in basso di una riga)
089400081002           when  *in45   and   NOT *in46;
089410081002             §PBA = 0.255;
089420081002           when  *in45   and   *in46;
089430081002             §PBA = 6.005;
089440081002         endsl;
089450081002      /end-free
089460080708      * Impostazione posizioni di linee orizzontali
089470080708     c                   eval      §LOA  =  §PBA + 0.76
089480080708     c                   eval      §LOB  =  §LOA + 0.25
089490080708     c                   eval      §LOC  =  §LOB + 0.12
089500080708      * Impostazione posizioni di linee verticali
089510080708     c                   eval      §LVA  =  §LOA - 0.15
089520050214      *
089530050214      * Impostazione posizioni di box
089540050215if  1c                   if        Page  <= 1
089550050214      *   (posizione di "partenza" su 1ª pagina)
089560081002     c                   eval      §PB1  =  §PBA + 1.270
089570050214x   1c                   else
089580050214      *   (posizione di "partenza" su pagine successive)
089590081002     c                   eval      §PB1  =  §PBA + 0.020
089600050214e   1c                   endif
089610110405if  1c*//                if        %subst(AR8trk:1:1) = 'P'
089620110405if  1c                   if        %subst(SAVtrk:1:1) = 'P'
089630050331     c                             and    CurLineA4   < wMaxLine
089640081002     c                   eval      §PB2  =  §PBA + 5.360
089650050215x   1c                   else
089660050215     c                   eval(h)   §PB2  =  CurLineA4 / 8 + 0.05
089670081002     c                   if        §PB2  <  (§PBA + 5.320)
089680081002     c                   eval      §PB2  =  (§PBA + 5.320)
089690050331     c                   endif
089700050215e   1c                   endif
089710050215      * Impostazione posizioni di linee (orizzontali e verticali)
089720050214      *   nella parte superiore del box (testata)
089730050215     c                   eval      §LO1  =  §PB1 + 0.25
089740050214     c                   eval      §LO2  =  §LO1 + 0.11
089750050214      *   nella parte inferiore del box (piede)
089760110405if  1c*//                if        %subst(AR8trk:1:1) = 'P'
089770110405if  1c                   if        %subst(SAVtrk:1:1) = 'P'
089780050331     c                             and    CurLineA4   < wMaxLine
0897900810027.78 c                   eval      §LO3  =  §PBA + 3.900
0898000502158.00 c                   eval      §LO4  =  §LO3 + 0.24
0898100502158.39 c                   eval      §LO5  =  §LO4 + 0.36
0898200502158.52 c                   eval      §LO6  =  §LO5 + 0.14
0898300502148.64 c                   eval      §LO7  =  §LO6 + 0.12
0898400502148.77 c                   eval      §LO8  =  §LO7 + 0.13
0898500502148.89 c                   eval      §LO9  =  §LO8 + 0.12
0898600807088.97 c                   eval      §LO0  =  §LO9 + 0.10
089870050215e   1c                   endif
089880050214      * La lunghezza delle linee verticali separatrici nel dettaglio
089890050214      *   dipende dalla fase di stampa: se si sta stampando ancora il
089900050214      *   dettaglio deve essere più lunga, visto che non viene stampa-
089910050214      *   ta la parte finale del DDT...
089920110405if  1c*//                if        %subst(AR8trk:1:1) = 'P'
089930110405if  1c                   if        %subst(SAVtrk:1:1) = 'P'
089940050331     c                             and    CurLineA4   < wMaxLine
0899500502162.25 c                   eval      §LV1  =  §LO3 - §LO2
089960050215x   1c                   else
089970050216     c                   eval      §LV1  =  §PB2 - §LO2
089980050215e   1c                   endif
089990050214      *
090000050214      * Impostazione posizioni di campi di testo
090010050214      * - PCLtx1/tx2 (verticali)
0900200502179.10 c                   eval      §PR1  =  §PB2 - 0.07
090030050214      * - PCLpx2 ("Il presente documento...")
0900400502178.95 c                   eval      §PS2  =  §LO9 + 0.07
090050050214      * - PCLpx3a/px4a/px5a
0900600807089.04 c                   eval      §PS3  =  §LO0 + 0.07
090070050214      * - PCLpx3b/px4b/px5b
0900800502179.09 c                   eval      §PS4  =  §PS3 + 0.07
090090050214      * - PCLpx3c/px4c/px5c
0901000502179.14 c                   eval      §PS5  =  §PS4 + 0.07
090110050214      * - PCLpx6 ("Contributo ambientale CONAI...")
0901200502179.25 c                   eval      §PS6  =  §PB2 + 0.08
090130050214      *
090140050214     c                   ENDSR
090150041207      *
090160041207      *---------------------------------------------------------------*
090170050217      *?Stampa DDT del mittente (generico con logo)                  ?*
090180041207      *---------------------------------------------------------------*
090190041207     c     DDT_GenericoL BEGSR
090200041207      *
090210081003      /free
090220081003         // Impostazione del nuovo limite x overflow
090230081003         select;
090240081003           when  NOT *in45;
090250081003             wMaxLine = $MaxLine2;
090260081003           when  *in45   and   NOT *in46;
090270081003             wMaxLine = $MaxLine2_pl1;
090280081003           when  *in45   and   *in46;
090290081003             wMaxLine = $MaxLine2_pl2;
090300081003         endsl;
090310081003
090320081003         // Posizionamento iniziale all'interno del foglio A4
090330081003         write  A4prtSP1;
090340081006
090350081006         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
090360081006           write  A4prtNUL;
090370081006         endif;
090380081003      /end-free
090390041207      *
090400041207      * > logo
090410041207     c                   eval      §Logo  = 'LOGO' + AR8tmd
090420041207      *
090430081017     c                   if            §AR8cxd = *blanks
090440081017     c                             or  DB0fa4  = 'P'
090450080709     c                   write     A4prtDT0
090460080709     c                   endif
090470080709      *
090480041209      * Imposto posizione per logo
090490081003      /free
090500081003         select;
090510081003           when  NOT *in45;
090520081003             exsr  Rep_§LO2;
090530081003             §PB1 = §LO2 + (2 / 8);
090540081003           when  *in45   and   NOT *in46;
090550081003             §PB1 = 0.250;
090560081003           when  *in45   and   *in46;
090570081003             §PB1 = 6.000;
090580081003         endsl;
090590081003      /end-free
090600041209      *   Stampa del logo
090610041209     c                   write     A4prtDB0
090620041207      *
090630041207      * Reperimento dati e stampa DDT mittente - - - - - - - - - - - -*
090640041207      *
090650130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
090660130315      *  "StampaDDT")
090670041207     c***  KLDV          setll     FIAR8000       (già fatto)
090680041207     c***  KLDV          reade     FIAR8000       (già fatto)
090690041207      *
090700041207if  1c                   dow       NOT %eof(FIAR801L)
090710041207      *
090720041207if  2c                   if        AR8atb = *blanks
090730041207      *
090740041207      * > Stampa dei dati
090750041207     c                   exsr      Test_OF
090760041207sel 3c                   select
090770041207      * - Dati di dettaglio: testo in font a dimensione standard
090780041207w   3c                   when      AR8trk = 'C1'
090790051110     c                   movel(p)  AR8uni        PCddt
090800041207     c                   write     A4prtDC1
090810041207      * - Dati di testata:   testo in font a dimensioni massime
090820041207w   3c                   when      AR8trk = 'T1'
090830051110     c                   movel(p)  AR8uni        PCddt
090840041207     c                   write     A4prtDT1
090850041207      * - Dati di testata:   testo in font a dimensioni grandi
090860041207w   3c                   when      AR8trk = 'T2'
090870051110     c                   movel(p)  AR8uni        PCddt
090880041207     c                   write     A4prtDT2
090890041209      * - Linea separatrice
090900041209w   3c                   when      AR8trk = '--'
090910041209      *   - Stampa di una riga vuota per la riga separatrice
090920041209     c                   write     A4prtNUL
090930041209      *   - Stampa della linea orizzontale separatrice
090940090213     c                   eval(h)   §LO1   = (CurLineA4 / 8) - 0.050
090950041209     c                   write     A4prtLO
090960041207e   3c                   endsl
090970041207e   2c                   endif
090980041207      *
090990041207     c                   eval      SAVtrk = AR8trk
091000041207     c     KLDV          reade     FIAR8000
091010041207      *
091020041207e   1c                   enddo
091030080708      *
091040081003      /free
091050081003         select;
091060081003           when  NOT *in45;
091070081003             wMaxLine = $MaxLine2;
091080081003           when  *in45   and   NOT *in46;
091090081003             wMaxLine = $MaxLine2_pl1;
091100081003           when  *in45   and   *in46;
091110081003             wMaxLine = $MaxLine2_pl2;
091120081003         endsl;
091130081003      /end-free
091140041207      *
091150041207     c                   ENDSR
091160040607      *
091170040607      *---------------------------------------------------------------*
091180050217      *?Stampa DDT del mittente (generico senza logo)                ?*
091190040607      *---------------------------------------------------------------*
091200040607     c     DDT_Generico  BEGSR
091210040607      *
091220081003      /free
091230081003         // Impostazione del nuovo limite x overflow
091240081003         select;
091250081003           when  NOT *in45;
091260081003             wMaxLine = $MaxLine2;
091270081003           when  *in45   and   NOT *in46;
091280081003             wMaxLine = $MaxLine2_pl1;
091290081003           when  *in45   and   *in46;
091300081003             wMaxLine = $MaxLine2_pl2;
091310081003         endsl;
091320081003
091330081003         // Posizionamento iniziale all'interno del foglio A4
091340081003         write  A4prtSP1;
091350081006
091360081006         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
091370081006           write  A4prtNUL;
091380081006         endif;
091390081003      /end-free
091400040607      *
091410081017     c                   if            §AR8cxd = *blanks
091420081017     c                             or  DB0fa4  = 'P'
091430050331     c                   write     A4prtDT0
091440050331     c                   endif
091450040607      *
091460040607      * Reperimento dati e stampa DDT mittente - - - - - - - - - - - -*
091470040607      *
091480130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
091490130315      *  "StampaDDT")
091500040607     c***  KLDV          setll     FIAR8000       (già fatto)
091510040607     c***  KLDV          reade     FIAR8000       (già fatto)
091520040607      *
091530040607if  1c                   dow       NOT %eof(FIAR801L)
091540040607      *
091550040607if  2c                   if        AR8atb = *blanks
091560051110     c                   movel(p)  AR8uni        PCddt
091570040607     c                   exsr      Test_OF
091580040607sel 3c                   select
091590040607      * - Dati di dettaglio: testo in font a dimensione standard
091600040607w   3c                   when      AR8trk = 'C1'
091610040607     c                   write     A4prtDC1
091620040607      * - Dati di testata:   testo in font a dimensioni massime
091630040607w   3c                   when      AR8trk = 'T1'
091640040607     c                   write     A4prtDT1
091650040607      * - Dati di testata:   testo in font a dimensioni grandi
091660040607w   3c                   when      AR8trk = 'T2'
091670040607     c                   write     A4prtDT2
091680040607e   3c                   endsl
091690040607e   2c                   endif
091700040607      *
091710040607     c     KLDV          reade     FIAR8000
091720040607      *
091730040607e   1c                   enddo
091740080708      *
091750081003      /free
091760081003         select;
091770081003           when  NOT *in45;
091780081003             wMaxLine = $MaxLine2;
091790081003           when  *in45   and   NOT *in46;
091800081003             wMaxLine = $MaxLine2_pl1;
091810081003           when  *in45   and   *in46;
091820081003             wMaxLine = $MaxLine2_pl2;
091830081003         endsl;
091840081003      /end-free
091850040607      *
091860040607     c                   ENDSR
091870081017
091880081017      /free
091890081017       //--------------------------------------------------------------
091900140303       //?Impostazione nuova pagina di DDT Generico (con/senza logo)   ?
091910081017       //--------------------------------------------------------------
091920081017       BEGSR  NewPageSTD;
091930081017
091940081017         // Posizionamento iniziale nella nuova pagina
091950081017         //   (già fatto con A4prtSP0 se DB0FA4 = "P")
091960081017         if  DB0fa4 <> 'P';
091970081017           write  A4prtSP1;
091980081017           if  DB0fa4 = '1'   or   DB0fa4 = 'S';
091990081017             write  A4prtNUL;
092000081017           endif;
092010081017         endif;
092020081017
092030081017         // Stampa "COPIA PER IL DESTINATARIO"
092040081017         if  §AR8cxd = *blank   or   DB0fa4 = 'P';
092050081017           write  A4prtDT0;
092060081017         endif;
092070081017
092080081017         // Impostazione del nuovo limite x overflow
092090081017         select;
092100081017           when  NOT *in45;
092110081017             wMaxLine = $MaxLine2;
092120081017           when  *in45   and   NOT *in46;
092130081017             wMaxLine = $MaxLine2_pl1;
092140081017           when  *in45   and   *in46;
092150081017             wMaxLine = $MaxLine2_pl2;
092160081017         endsl;
092170081017
092180081017       ENDSR;
092190081017      /end-free
092200030515      *
092210030515      *---------------------------------------------------------------*
092220050217      *?Stampa del record "generico" (PRTGEN)                        ?*
092230030515      *---------------------------------------------------------------*
092240030515     c     StampaGEN     BEGSR
092250030515      *
092260030515      * Controllo se è stato raggiunto l'overflow nel formato in uso
092270030515     c                   exsr      Test_OF
092280030515      *
092290030911      * Stampo la riga PRTGEN
092300030911      * Se sto stampando una "testata" (*in39) salto una riga prima
092310030911      *   ed una riga dopo (per spaziare)
092320030911if  1c                   if        *in39 = *on
092330030515     c                   write     A4prtNUL
092340030911e   1c                   endif
092350030911     c                   write     A4prtGEN
092360030911if  1c                   if        *in39 = *on
092370030911     c                   write     A4prtNUL
092380030911e   1c                   endif
092390030515      *
092400030515     c                   ENDSR
092410030515      *
092420030515      *---------------------------------------------------------------*
092430050511      *?Prepara elenco per lista - SegnaColli Non Sequenziali (test.)?*
092440030515      *---------------------------------------------------------------*
092450030515     c     ListaScNS     BEGSR
092460030515      *
092470030515     c                   clear                   XX
092480030515      *
092490030515      * Nel foglio in formato "A5" devo solo riempire gli appositi
092500030715      *   campi "Note e Particolarità" (nessun salto riga/pagina è
092510030715      *   richiesto)
092520050513     c                   if           *in69       = *on
092530050513     c                             or $A4_x_ScNS  = *off
092540030515     c                   eval      WelSNS = $elSNSxA5
092550030515     c                   goto      ScNS_A5
092560030515     c                   endif
092570030515      *
092580030515      * Determino il numero di segnacolli da elencare per riga nel
092590030515      *   formato pagina "A4" (dove ci sono righe a parte).
092600030515      *   Dipende dal carattere specificato nel prtf: più grande è
092610030515      *   il carattere => meno segnacolli ci stanno sulla riga !
092620030911      *   A cpi 15 -come ora- ci sta tutta la schiera; a cpi 12 no !
092630030515     c                   eval      WelSNS = %elem($SC1)
092640030515      *
092650030515      * Spengo gli indicatori che condizionano in stampa le indicazioni
092660030515      *   per DPD / FedEx / Bancali-Supermercati-Autotrasportatori
092670030515      *   (se sono già state stampate nella 1ª pagina non devono più
092680030515      *   esserlo)
092690030910      *   (se non sono ancora state stampate, dovranno esserlo poi)
092700030515if  1c                   if        wPage > 1
092710050513     c                   movea     *in(70)       SAVin7x
092720040223     c                   movea     '0000'        *in(70)
092730030515e   1c                   endif
092740030515      *
092750030515      * Calcolo il numero di righe da saltare
092760030515     c                   eval      wMinLine = CurLineA4
092770030515if  1c                   if        §LO2     > §LO1
092780030515      * - se stampato almeno un box
092790030515     c                   eval(h)   wMinLine = (§LO2 * 8) + 2
092800030515x   1c                   else
092810030515      * - se non stampato alcun box
092820080730      /free
092830080729       // - Calcola la posizione della riga in 1/8" di pollice
092840080729       //   da quella attuale (in 1/9" di pollice)
092850080730         eval(h) wMinLine = (CurLineA4 * 8 / 9) + 1;
092860080730      /end-free
092870030515e   1c                   endif
092880030515      * ...e scarto il numero righe necessario
092890030515      *   (se non sono già da pagina nuova;
092900030515      *    se sono da pagina nuova: ci penserà poi la subr. "Test_OF"
092910030515      *       alla prima riga in stampa)
092920030515if  1c                   if        wMinLine  < wMaxLine
092930080729do  2c                   dou       CurLineA4 >= wMinLine
092940030515     c                   write     A4prtNUL
092950030515e   2c                   enddo
092960030515e   1c                   endif
092970030515      *
092980030515     c     ScNS_A5       tag
092990030515      *
093000030515      * Imposto l'indicatore per la stampa del titolo relativo all'
093010030515      *   elenco che segue (condiziona attributi di stampa diversi)
093020030515     c                   eval      *in39  = *on
093030050511      *
093040050511     c                   exsr      ListaScNSd
093050050511      *
093060050511     c                   ENDSR
093070050511      *
093080050511      *---------------------------------------------------------------*
093090050511      *?Prepara elenco per lista - SegnaColli Non Sequenziali (dett.)?*
093100050511      *---------------------------------------------------------------*
093110050511     c     ListaScNSd    BEGSR
093120030515      *
093130030515      * Stampo elenco dei segnacolli non sequenziali:
093140071008if  1c                   IF             DB0tbo  = 'P'
093150071008     c                             or   DB0tbo  = '8'
093160030515      * - per bolle in Partenza
093170030515     c                   exsr      ScNS_BLT
093180030515x   1c                   ELSE
093190081007      * - per bolle in Arrivo
093200030515     c                   exsr      ScNS_ART
093210030515e   1c                   ENDIF
093220030515      *
093230030515      * Alla fine: stampo eventuali segnacolli in elenco (non stampati)
093240030515      *              in foglio di formato "A4"
093250030515      *            imposto eventuali segnacolli in elenco (non messi in
093260030515      *              stampa tra le particolarità) in foglio "A5"
093270030515if  1c                   if        XX     > *zeros
093280030515     c                   eval      XX     = *hival
093290030515     c                   exsr      StampaScNS
093300030515e   1c                   endif
093310030910      *
093320030910      * Se sto stampando su formato pagina "A4": reimposto *in70, *in71
093330030910      *   e *in72 (spenti prima della stampa dell'elenco dei segnacolli
093340030910      *   non sequenziali) perchè saranno utilizzati per il posiziona-
093350030910      *   mento in nuova pagina (nella subr. "Test_OF") e per la stampa
093360040223      *   dei box per DPD, FedEx e quelli da FIAR5.
093370050513if  1c                   if             not *in69
093380030910     c                             and  not *in70
093390080922     c*** DPD non più in A4:       and  not *in71
093400030910     c                             and  not *in72
093410080912     c*** EEX non più gestita:     and  not *in73
093420030910     c                   movea     SAVin7x       *in(70)
093430030910e   1c                   endif
093440030515      *
093450030515     c                   ENDSR
093460030515      *
093470030515      *---------------------------------------------------------------*
093480050217      *?Prepara elenco SegnaColli Non Sequenziali da FNBLT (partenza)?*
093490030515      *---------------------------------------------------------------*
093500030515     c     ScNS_BLT      BEGSR
093510030515      *
093520030515if  1c                   if        not %open(FNBLT01L)
093530030515     c                   open      FNBLT01L
093540030515e   1c                   endif
093550030515      *
093560030515      * memorizzo l'ultimo numero di segnacollo
093570070514if  1c                   if        $Mamma = *off
093580040607     c     KLDV          setgt     FNBLT000
093590040607     c     KLDV          readpe    FNBLT000
093600070514x   1c                   else
093610070514     c     KLBL          setgt     FNBLT000
093620070514     c     KLBL          readpe    FNBLT000
093630070514e   1c                   endif
093640030515if  1c                   if        NOT %eof(FNBLT01L)
093650080922     c                   eval      %subst(Pnsx : 8) = ','
093660080922     c                                              + %editc(BLTnsc : 'Z')
093670030515e   1c                   endif
093680030515      *
093690030515      * memorizzo il primo numero di segnacollo
093700070514if  1c                   if        $Mamma = *off
093710040607     c     KLDV          setll     FNBLT000
093720040607     c     KLDV          reade     FNBLT000
093730070514x   1c                   else
093740070514     c     KLBL          setll     FNBLT000
093750070514     c     KLBL          reade     FNBLT000
093760070514e   1c                   endif
093770030515if  1c                   if        NOT %eof(FNBLT01L)
093780080922     c                   eval      %subst(Pnsx : 1 : 7) =
093790080922     c                                 %editc(BLTnsc : 'Z')
093800030515e   1c                   endif
093810030515      *
093820030515      * se ci sono solo 2 segnacolli non sequenziali:
093830030515      *   stanno già al posto del range - basta non stampare il msg
093840030515      *   ("NON SEQUENZIALI")
093850030515if  1c                   if        DB0ncl <= 2
093860050516     c                   eval      *in39  =  *off
093870030515     c                   goto      EndScNS_BLT
093880030515e   1c                   endif
093890030515      *
093900030515      * ciclo di lettura per il recupero di tutti i numeri di
093910030515      *   segnacollo
093920030515do  1c                   DOW       NOT %eof(FNBLT01L)
093930030515     c                   eval      Wnsc   =  BLTnsc
093940030515     c                   exsr      StampaScNS
093950070514if  2c                   if        $Mamma = *off
093960040607     c     KLDV          reade     FNBLT000
093970070514x   2c                   else
093980070514     c     KLBL          reade     FNBLT000
093990070514e   2c                   endif
094000030515e   1c                   ENDDO
094010030515      *
094020030515     c     EndScNS_BLT   ENDSR
094030030515      *
094040030515      *---------------------------------------------------------------*
094050050217      *?Prepara elenco SegnaColli Non Sequenziali da FNART (arrivo)  ?*
094060030515      *---------------------------------------------------------------*
094070030515     c     ScNS_ART      BEGSR
094080030515      *
094090070319if  1c                   if        not %open(FNART01L)
094100070319     c                   open      FNART01L
094110070319e   1c                   endif
094120030515      *
094130030515      * memorizzo l'ultimo numero di segnacollo
094140070514if  1c                   if        $Mamma = *off
094150040607     c     KLDV          setgt     FNART000
094160040607     c     KLDV          readpe    FNART000
094170070514x   1c                   else
094180070514     c     KLBL          setgt     FNART000
094190070514     c     KLBL          readpe    FNART000
094200070514e   1c                   endif
094210030515if  1c                   if        NOT %eof(FNART01L)
094220080922     c                   eval      %subst(Pnsx : 8) = ','
094230080922     c                                              + %editc(ARTnsc : 'Z')
094240030515e   1c                   endif
094250030515      *
094260030515      * memorizzo il primo numero di segnacollo
094270070514if  1c                   if        $Mamma = *off
094280040607     c     KLDV          setll     FNART000
094290040607     c     KLDV          reade     FNART000
094300070514x   1c                   else
094310070514     c     KLBL          setll     FNART000
094320070514     c     KLBL          reade     FNART000
094330070514e   1c                   endif
094340030515if  1c                   if        NOT %eof(FNART01L)
094350080922     c                   eval      %subst(Pnsx : 1 : 7) =
094360080922     c                                 %editc(ARTnsc : 'Z')
094370030515e   1c                   endif
094380030515      *
094390030515      * se ci sono solo 2 segnacolli non sequenziali:
094400030515      *   stanno già al posto del range - basta non stampare il msg
094410030515      *   ("NON SEQUENZIALI")
094420030515if  1c                   if        DB0ncl <= 2
094430050516     c                   eval      *in39  =  *off
094440030515     c                   goto      EndScNS_ART
094450030515e   1c                   endif
094460030515      *
094470030515      * ciclo di lettura per il recupero di tutti i numeri di
094480030515      *   segnacollo
094490030515do  1c                   DOW       NOT %eof(FNART01L)
094500030515     c                   eval      Wnsc   =  ARTnsc
094510030515     c                   exsr      StampaScNS
094520070514if  2c                   if        $Mamma = *off
094530040607     c     KLDV          reade     FNART000
094540070514x   2c                   else
094550070514     c     KLBL          reade     FNART000
094560070514e   2c                   endif
094570030515e   1c                   ENDDO
094580030515      *
094590030515     c     EndScNS_ART   ENDSR
094600030515      *
094610030515      *---------------------------------------------------------------*
094620050217      *?Stampa elenco SegnaColli Non Sequenziali in fogli "A4" o "A5"?*
094630030515      *---------------------------------------------------------------*
094640030515     c     StampaScNS    BEGSR
094650030515      *
094660030515      * In prima riga stampo la "intestazione"
094670030515if  1c                   if        *in39
094680050513if  2c                   if        *in69   or
094690050513     c                             $A4_x_ScNS = *off
094700050513      * - foglio "A5" oppure
094710050513      *   foglio "A4" in "NOTE E PARTICOLARITA'"
094720030515     c                   eval      YY      = YY  + 1
094730030515     c                   eval      $NP(yy) = C_ScNS + ':'
094740030515x   2c                   else
094750030515      * - foglio "A4"
094760030515     c                   eval      Prig  = C_ScNS
094770030515     c                                   + '         (dal '
094780080922     c                                   + %trim( %subst(Pnsx: 1: 7) )
094790030515     c                                   + ' al '
094800080922     c                                   + %trim( %subst(Pnsx: 9: 7) )
094810030515     c                                   + ')'
094820030515     c                   exsr      StampaGEN
094830030515e   2c                   endif
094840080922     c                   eval      Pnsx  =  'NON SEQUENZIALI'
094850030515     c                   eval      *in39 = *off
094860030515     c                   clear                   XX
094870030515     c                   clear                   $SC1
094880030515     c                   clear                   $SC2
094890030515e   1c                   endif
094900030515      *
094910030515      * Al riempimento della riga: stampo la parte di elenco segnacolli
094920030515if  1c                   if        XX  >= WelSNS
094930030515     c                   eval      $SC2 = %editc($SC1 : 'Z')
094940050513if  2c                   if        *in69   or
094950050513     c                             $A4_x_ScNS = *off
094960050513      * - foglio "A5"  oppure
094970050513      *   foglio "A4" in "NOTE E PARTICOLARITA'"
094980030515     c                   eval      YY      = YY  + 1
094990030515     c                   movea(p)  $SC2          $NP(yy)
095000030515x   2c                   else
095010030515      * - foglio "A4"
095020030911     c                   movea(p)  $SC2          Prig
095030030515     c                   exsr      StampaGEN
095040030515e   2c                   endif
095050030515if  2c                   if        XX  <  *hival
095060030515     c                   clear                   XX
095070030515e   2c                   endif
095080030515     c                   clear                   $SC1
095090030515     c                   clear                   $SC2
095100030515e   1c                   endif
095110030515      *
095120030515      * Aggiorno la nuova parte di elenco segnacolli
095130030515      *  (se non richiesta la stampa degli ultimi segnacolli non
095140030515      *   sequenziali residui - memorizzati ma non ancora stampati)
095150030515if  1c                   if        XX  <  *hival
095160030515     c                   eval      XX = xx  + 1
095170030515     c                   eval      $SC1(xx) = Wnsc
095180030515e   1c                   endif
095190030515      *
095200030515     c                   ENDSR
095210040607      *
095220040607      *---------------------------------------------------------------*
095230050217      *?Stampa della parte finale della Lettera di Vettura           ?*
095240040607      *---------------------------------------------------------------*
095250040607     c     ChiusuraLDV   BEGSR
095260080605      *
095270080605      * Posizionamento sul foglio A4 in base al n° pagina in stampa
095280130418     c                   eval      *in33 = *on
095290130418     c                   exsr      Posiz_Box
095300070307      *
095310070307      * Il tagliandino "Copia per il Destinatario" va ora stampato SE:
095320070307      * · spedizione NON in stato "lasciato avviso"   oppure
095330070307      * · spedizione in stato "lasciato avviso", ma P.A. con fattura
095340070307      *   immediata
095350040607      *
095360080703      * 2ª parte - copia per il destinatario (da lasciare al cliente)
095370040607      *   (Se NON è ristampa in arrivo per recapito c/assegni - vedi
095380040607      *   costante "Non utilizzare per la consegna")
095390130418      *                o per il mittente - se Porto Franco Prepagato
095400040607      * > "cornice" / barcode / righine varie in parte inferiore
095410040607      *
095420080604sel 1c                   select
095430080604w   1c                   when           $No_x_Cons         =  *off
095440070307     c                             and ($LascAvv           =  *off
095450070406     c                              or (%subst(DB0tb1:1:1) =  'A'
095460070406     c                              and DB0nft             >  *zeros))
095470130418     c                   write     A4prtLDV
095480130418     c                   write     A4prtBOX
095490110426     c*//                write     A4LogoBAR
095500110426      /free
095510120530         //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
095520120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
095530120530         //  // >?Simil-Logo Bartolini/Brt?
095540120530         //  write  A4LogoBR2;
095550120530         //  write  A4LogoDA2;
095560120530         //else;
095570120530         //  // >?Simil-Logo Bartolini?
095580120530         //  write  A4LogoBAR;
095590120530         //  write  A4LogoDA1;
095600120530         //endif;
095610120530         // >?Simil-Logo BRT?
095620120530         write  A4LogoBRT;
095630120530         write  A4LogoDA3;
095640110426      /end-free
095650080604w   1c                   when           $LascAvv           =  *on
095660070406     c                             and (%subst(DB0tb1:1:1) <> 'A'
095670070406     c                              or  DB0nft             =  *zeros)
095680070307     c                   eval      Pmsg3  =  Tla
095690070307     c                   write     A4prtLDVB
095700120113w   1c                   when      $No_x_Cons =  *on
095710120113w   1c*//                   when      $No_x_Cons =  '2'
095720120113     c*//                   eval      Pmsg3  =  ' NON UTILIZZARE PER'
095730120113     c*//                                    +  ' LASCIATO AVVISO -'
095740120113     c*//                                    +  ' SERVIZIO POSTE'
095750120113     c*//                   write     A4prtLDVB
095760080604e   1c                   endsl
095770040607      *
095780040607     c                   ENDSR
095790030515      *
095800030515      *---------------------------------------------------------------*
095810050217      *?Carico LDV da FNBLP00F                                       ?*
095820030515      *---------------------------------------------------------------*
095830030515     c     CarBLP        BEGSR
095840030515      *
095850030515      * Apertura files
095860030515     c                   if        not %open(FNBLP01L)
095870030515     c                   open      FNBLP01L
095880030515     c                   endif
095890030515     c                   if        not %open(FNARB01L)
095900030515     c                   open      FNARB01L
095910030515     c                   endif
095920070319     ***c v.sr sr_Start     if        not %open(FIAR401L)
095930070319     ***c " "     "         open      FIAR401L
095940070319     ***c " "     "         endif
095950030515     c                   if        not %open(FIAR601L)
095960030515     c                   open      FIAR601L
095970030515     c                   endif
095980030515     c                   if        not %open(FIAR701L)
095990030515     c                   open      FIAR701L
096000030515     c                   endif
096010060505     c                   if        not %open(FIAR901L)
096020060505     c                   open      FIAR901L
096030030515     c                   endif
096040071114      *
096050071114     c                   clear                   DB0cpi
096060030515      *
096070040607     c     KLDV          chain     FNBLP000
096080030515      *
096090030515if  1c                   if        NOT %found(FNBLP01L)
096100081117     c                   leavesr
096110030515e   1c                   endif
096120120509      *
096130120509      * Memorizzazione dei dati di FNBLP che serviranno anche dopo
096140120509      *   il reperimento dell'eventuale bolla mamma...
096150120509     c                   eval      wB0piv =  BLPcpi
096160030515      *
096170030515      * 1ª impostaz. campi della DS in base a quelli del file FNBLP
096180030515     c                   exsr      sr_DS_BLP
096190030515      *
096200070308      * Decodifico filiale di Arrivo
096210030515     c                   exsr      DecLNA
096220030515      *
096230030515      * 2ª impostaz. campi della DS in base a quelli delle tabelle
096240030515      *    "generiche" (3A, TB, 5E)
096250030515     c                   exsr      sr_DS_TAB
096260030515      *
096270030515      * Ultimi dati mancanti:
096280061030      * - Cod. cliente destinatario: imposto LNA9999
096290030515      *    SE   in Assegnato   O   c'è 2ª bolla
096300030515if  1c                   if           %subst(DB0tb1:1:1) = 'A'
096310030515     c                             or DB0tb2            <> *blanks
096320030515     c                   eval      DB0ccd = *all'9'
096330030515     c                   movel     BLPlna        DB0ccd
096340030515e   1c                   endif
096350120509      *
096360120509      * - Reperimento Data spedizione
096370120509if  1c                   if        DB0mgs <> *zeros
096380120509     c                   z-add     DB0mgs        Wdsp
096390120509     c                   movel     DB0aas        Wdsp
096400120509x   1c                   else
096410120509      *   prendo la data del giorno se non c'è la data spedizione
096420120509     c                   time                    w0140
096430120509     c                   move      w0140         w0080
096440120509     c     *eur          movel     w0080         Data_EUR
096450120509     c     *iso          movel     Data_EUR      Wdsp
096460120509e   1c                   endif
096470030515      *
096480061030      * 3ª impostaz. campi della DS: importi da calcolare
096490061031     c                   exsr      sr_DS_IMPO
096500030515      *
096510081117     c                   ENDSR
096520030515      *
096530030515      *---------------------------------------------------------------*
096540050217      *?Carico LDV da FNARB00F                                       ?*
096550030515      *---------------------------------------------------------------*
096560030515     c     CarARB        BEGSR
096570030515      *
096580030515      * Apertura files
096590030515     c                   if        not %open(FNARB01L)
096600030515     c                   open      FNARB01L
096610030515     c                   endif
096620070319     c                   if        NOT %open(FIARN11L)
096630070319     c                   open      FIARN11L
096640070319     c                   endif
096650030515     c                   if        not %open(FIAR601L)
096660030515     c                   open      FIAR601L
096670030515     c                   endif
096680030515     c                   if        not %open(FIAR701L)
096690030515     c                   open      FIAR701L
096700030515     c                   endif
096710060505     c                   if        not %open(FIAR901L)
096720060505     c                   open      FIAR901L
096730030515     c                   endif
096740071114      *
096750071114     c                   clear                   DB0cpi
096760030515      *
096770040607     c     KLDV          chain     FNARB01L
096780030515      *
096790030515if  1c                   if        NOT %found(FNARB01L)
096800070312     c                   clear                   ARBndc
096810070322     c                   clear                   ARBntc
096820070322     c                   clear                   ARBdcp
096830071015     c                   clear                   ARBpdc
096840101129     c                   clear                   ARBdcm
096850101129     c                   clear                   ARBicc
096860101129     c                   clear                   ARBica
096870120509     c                   clear                   ARBcpi
096880081117     c                   leavesr
096890030515e   1c                   endif
096900070514      *
096910070514      * Memorizzazione dei dati di FNARB che serviranno anche dopo
096920070514      *   il reperimento dell'eventuale bolla mamma...
096930070514     c                   eval      wB0ndc =  ARBndc
096940070514     c                   eval      wB0ntc =  ARBntc
096950070514     c                   eval      wB0dcp =  ARBdcp
096960081119     c                   eval      wB0tmc =  ARBtmc
096970071015     c                   move      ARBpdc        Wpdc4a
096980101125     c                   eval      wB0dcm =  ARBdcm
096990101125     c                   eval      wB0icc =  ARBicc
097000101125     c                   eval      wB0ica =  ARBica
097010120509     c                   eval      wB0piv =  ARBcpi
097020030515      *
097030030515      * 1ª impostaz. campi della DS in base a quelli del file FNARB
097040030515     c                   exsr      sr_DS_ARB
097050030515      *
097060070308      * Decodifico filiale di Arrivo
097070030515     c                   exsr      DecLNA
097080030515      *
097090030515      * 2ª impostaz. campi della DS in base a quelli delle tabelle
097100030515      *    "generiche" (3A, TB, 5E)
097110030515     c                   exsr      sr_DS_TAB
097120030515      *
097130030515      * Ultimi dati mancanti:
097140030515      * - Cod. cliente mittente
097150030515if  1c                   If        ARBccm <> *zeros
097160030515     c                   movel     ARBccm        DB0ccm
097170030515x   1c                   Else
097180030515if  2c                   if        %subst(DB0tb1:1:1) = 'F'
097190030515     c                   movel     ARBksc        DB0ccm
097200030515e   2c                   endif
097210030515e   1c                   EndIf
097220030515      * - Cod. cliente destinatario
097230030515if  2c                   if        %subst(DB0tb1:1:1) = 'A'
097240030515     c                   movel     ARBksc        DB0ccd
097250030515e   1c                   endif
097260030515      * - Se c'è 2ª bolla imposto LNA9999
097270030515if  1c                   if        DB0tb2 <> *blanks
097280030515     c                   eval      DB0ccd = *all'9'
097290030515     c                   movel     ARBlna        DB0ccd
097300030515e   1c                   endif
097310120509      * - Reperimento Data spedizione
097320120509if  1c                   if        DB0mgs <> *zeros
097330120509     c                   z-add     DB0mgs        Wdsp
097340120509     c                   movel     DB0aas        Wdsp
097350120509x   1c                   else
097360120509      *   prendo la data del giorno se non c'è la data spedizione
097370120509     c                   time                    w0140
097380120509     c                   move      w0140         w0080
097390120509     c     *eur          movel     w0080         Data_EUR
097400120509     c     *iso          movel     Data_EUR      Wdsp
097410120509e   1c                   endif
097420030515      *
097430061030      * 3ª impostaz. campi della DS: importi da calcolare
097440061031     c                   exsr      sr_DS_IMPO
097450030515      *
097460081117     c                   ENDSR
097470030515      *
097480030515      *---------------------------------------------------------------*
097490050217      *?1ª impostaz. campi della DS in base a quelli del file FNBLP  ?*
097500030515      *---------------------------------------------------------------*
097510030515     c     sr_DS_BLP     BEGSR
097520030515      *
097530030515     c                   eval      DB0cbo = BLPcbo
097540030515     c                   eval      DB0tsp = BLPtsp
097550030515     c                   eval      DB0mgs = BLPmgs
097560030515     c                   eval      DB0tc1 = BLPtc1
097570030515     c                   eval      DB0tc2 = BLPtc2
097580030515     c                   eval      DB0fns = BLPfns
097590030515     c                   eval      DB0rmn = BLPrmn
097600030515     c                   eval      DB0rma = BLPrma
097610030515     c                   eval      DB0ias = BLPias
097620030515     c                   eval      DB0vas = BLPvas
097630030515     c                   eval      DB0vmd = BLPvmd
097640030515     c                   eval      DB0vad = BLPvad
097650030515     c                   eval      DB0qft = BLPqft
097660030515     c                   eval      DB0lna = BLPlna
097670030515     c                   eval      DB0znc = BLPznc
097680030515     c                   eval      DB0rsm = BLPrsm
097690030515     c                   eval      DB0inm = BLPinm
097700030515     c                   eval      DB0cam = BLPcam
097710030515     c                   eval      DB0lom = BLPlom
097720030515     c                   eval      DB0prm = BLPprm
097730030515     c                   eval      DB0nzm = BLPnzm
097740030515     c                   eval      DB0fap = BLPfap
097750030515     c                   eval      DB0rmo = BLPrmo
097760030515     c                   eval      DB0rsd = BLPrsd
097770030515     c                   eval      DB0ind = BLPind
097780030515     c                   eval      DB0cad = BLPcad
097790030515     c                   eval      DB0lod = BLPlod
097800030515     c                   eval      DB0prd = BLPprd
097810030515     c                   eval      DB0nzd = BLPnzd
097820030515     c                   eval      DB0fin = BLPfin
097830030515     c                   eval      DB0gc1 = BLPgc1
097840030515     c                   eval      DB0gc2 = BLPgc2
097850030515     c                   eval      DB0nas = BLPnas
097860030515     c                   eval      DB0ncl = BLPncl
097870030515     c                   eval      DB0pkf = BLPpkf
097880030515     c                   eval      DB0fvf = BLPfvf
097890030515     c                   eval      DB0vlf = BLPvlf
097900030515     c                   eval      DB0fls = BLPfls
097910030515     c                   eval      DB0ncd = BLPncd
097920030515     c                   eval      DB0nca = BLPnca
097930030515     c                   eval      DB0xco = BLPxco
097940030515     c                   eval      DB0ffd = BLPffd
097950030515     c                   eval      DB0dcr = BLPdcr
097960030515     c                   eval      DB0hcr = BLPhcr
097970030515     c                   eval      DB0tcr = BLPtcr
097980030515     c                   eval      DB0fbr = BLPfbr
097990030515     c                   eval      DB0gga = BLPgga
098000030515     c                   eval      DB0gma = BLPgma
098010030515     c                   eval      DB0gva = BLPgva
098020030515     c                   move      ' '           DB0gva
098030030515     c                   eval      DB0sop = BLPsop
098040030515     c                   eval      DB0ctr = BLPctr
098050030515     c                   eval      DB0drt = BLPdrt
098060030515     c                   eval      DB0fpp = BLPfpp
098070030515     c                   eval      DB0nrt = BLPnrt
098080030515     c                   eval      DB0pdr = BLPpdr
098090030515      *
098100030515      * Cod. cliente mittente
098110040311     c                   move      BLPksc        w0040
098120030515if  1c                   IF        BLPccm <> *zeros
098130030515     c                   movel     BLPccm        DB0ccm
098140030515x   1c                   ELSE
098150030515      *   Lo imposto solo se non è codice generico destinatario
098160040311if  2c                   if        w0040  <> 9999
098170030515     c                   movel     BLPksc        DB0ccm
098180030515x   2c                   else
098190030515     c                   movel     BLPksc        DB0ccd
098200030515     c                   movel     BLPlna        DB0ccd
098210030515e   2c                   endif
098220030515e   1c                   ENDIF
098230030515      *
098240030515      * Allega documento di trasporto
098250081119     c                   clear                   DB0bam
098260060505if  1c                   if           BLPfst = 'S'
098270030515     c                             or BLPfst = 'C'
098280030515     c                   movel     'S'           DB0bam
098290060505e   1c                   endif
098300060524      *
098310060524      * Se bolla di RESO: Codice cliente da Assegnato
098320061107     c*** (già fatto:)   clear                   dsBL4A
098330060524if  1c                   if        DB0fbr = 'S'
098340060524     c                   exsr      RepAR4_A
098350060524e   1c                   endif
098360030515      *
098370030515      * Destinatario: vedo se esiste 2ª ragione sociale
098380061107     c                   clear                   DB0rd2
098390061107     c                   exsr      RepAR4_D
098400030515      *
098410030515      * Note
098420061107     c                   clear                   DB0nt1
098430061107     c                   clear                   DB0nt2
098440061107     c                   exsr      RepAR4_89
098450030515      *
098460030515     c                   ENDSR
098470030515      *
098480030515      *---------------------------------------------------------------*
098490050217      *?1ª impostaz. campi della DS in base a quelli del file FNARB  ?*
098500030515      *---------------------------------------------------------------*
098510030515     c     sr_DS_ARB     BEGSR
098520030515      *
098530030515     c                   eval      DB0cbo = ARBcbo
098540030515     c                   eval      DB0tsp = ARBtsp
098550030515     c                   eval      DB0mgs = ARBmgs
098560030515     c                   eval      DB0tc1 = ARBtc1
098570030515     c                   eval      DB0tc2 = ARBtc2
098580030515     c                   eval      DB0fns = ARBfns
098590030515     c                   eval      DB0rmn = ARBrmn
098600030515     c                   eval      DB0rma = ARBrma
098610030515     c                   eval      DB0ias = ARBias
098620030515     c                   eval      DB0vas = ARBvas
098630030515     c                   eval      DB0vmd = ARBvmd
098640030515     c                   eval      DB0vad = ARBvad
098650030515     c                   eval      DB0qft = ARBqft
098660030515     c                   eval      DB0lna = ARBlna
098670030515     c                   eval      DB0znc = ARBznc
098680030515     c                   eval      DB0rsm = ARBrsm
098690030515     c                   eval      DB0inm = ARBinm
098700030515     c                   eval      DB0cam = ARBcam
098710030515     c                   eval      DB0lom = ARBlom
098720030515     c                   eval      DB0prm = ARBprm
098730030515     c                   eval      DB0nzm = ARBnzm
098740030515     c                   eval      DB0fap = ARBfap
098750030515     c                   eval      DB0rmo = ARBrmo
098760030515     c                   eval      DB0rsd = ARBrsd
098770030515     c                   eval      DB0ind = ARBind
098780030515     c                   eval      DB0cad = ARBcad
098790030515     c                   eval      DB0lod = ARBlod
098800030515     c                   eval      DB0prd = ARBprd
098810030515     c                   eval      DB0nzd = ARBnzd
098820030515     c                   eval      DB0fin = ARBfin
098830030515     c                   eval      DB0gc1 = ARBgc1
098840030515     c                   eval      DB0gc2 = ARBgc2
098850030515     c                   eval      DB0nas = ARBnas
098860030515     c                   eval      DB0ncl = ARBncl
098870030515     c                   eval      DB0pkf = ARBpkf
098880030515     c                   eval      DB0fvf = ARBfvf
098890030515     c                   eval      DB0vlf = ARBvlf
098900030515     c                   eval      DB0fls = ARBfls
098910030515     c                   eval      DB0ncd = ARBncd
098920030515     c                   eval      DB0nca = ARBnca
098930030515     c                   eval      DB0xco = ARBxco
098940030515     c                   eval      DB0ffd = ARBffd
098950030515     c                   eval      DB0dcr = ARBdcr
098960030515     c                   eval      DB0hcr = ARBhcr
098970030515     c                   eval      DB0tcr = ARBtcr
098980030515     c                   eval      DB0fbr = ARBfbr
098990030515     c                   eval      DB0gga = ARBgga
099000030515     c                   eval      DB0gma = ARBgma
099010030515     c                   eval      DB0gva = ARBgva
099020030515     c                   move      ' '           DB0gva
099030030515     c                   clear                   DB0sop
099040030515     c                   eval      DB0ctr = ARBctr
099050030515     c                   clear                   DB0drt
099060030515     c                   clear                   DB0fpp
099070030515     c                   clear                   DB0nrt
099080030515     c                   clear                   DB0pdr
099090030515      *
099100030515      * Allega documento di trasporto
099110060524      * & Codice cliente da Assegnato
099120061107     c*** (già fatto:)   clear                   dsBL4A
099130031029     c                   clear                   DB0bam
099140061107     c                   exsr      RepAR4_A
099150060505     c                   if        §B4Abm = 'S' or §B4Abm = 'C' or
099160031029     c                             §B4Abm = 'P' or §B4Abm = 'Q'
099170030515     c                   movel     'S'           DB0bam
099180060505     c                   endif
099190030515      *
099200030515      * Destinatario: vedo se esiste 2ª ragione sociale
099210061107     c                   clear                   DB0rd2
099220061107     c                   exsr      RepAR4_D
099230030515      *
099240030515      * Note
099250061107     c                   clear                   DB0nt1
099260061107     c                   clear                   DB0nt2
099270061107     c                   exsr      RepAR4_89
099280030515      *
099290030515     c                   ENDSR
099300030515      *
099310030515      *---------------------------------------------------------------*
099320050217      *?2ª impostaz. campi della DS in base a quelli delle tabelle   ?*
099330050217      *?   "generiche" (non da "1A")                                 ?*
099340030515      *---------------------------------------------------------------*
099350030515     c     sr_DS_TAB     BEGSR
099360030515      *
099370030515      * Carico le tabelle "generiche" (3A, TB, 5E)
099380030515     c                   exsr      CarTAB
099390030515      *
099400030515      * Codici bolla
099410030515     c                   eval      DB0tb1 = §3Atb1
099420030515     c                   eval      DB0tb2 = §3Atb2
099430030515     c                   eval      DB0dcb = §3Ades
099440030515      * Tipo servizio
099450030515if  1c                   if        DB0dts = *blanks
099460030515     c                   eval      DB0dts = §5Edes
099470030515     c                   eval      DB0fst = §5Efst
099480030515e   1c                   endif
099490030515      * Consegne particolari
099500030515if  1c                   if            DB0tc1 <> *blanks
099510030515     c                             and DB0dc1 =  *blanks
099520030515     c                   movel(p)  DB0tc1        TBLkey
099530030515     c                   exsr      Tab_1P
099540030515     c                   eval      DB0dc1 = §1Pdes
099550030515e   1c                   endif
099560030515if  1c                   if            DB0tc2 <> *blanks
099570030515     c                             and DB0dc2 =  *blanks
099580030515     c                   movel(p)  DB0tc2        TBLkey
099590030515     c                   exsr      Tab_1P
099600030515     c                   eval      DB0dc2 = §1Pdes
099610030515e   1c                   endif
099620030515      * Esenzione IVA
099630030515if  1c                   if            DB0cei <> *blanks
099640030515     c                             and DB0dce =  *blanks
099650030515     c                   eval      DB0dce =  §EIdes
099660030515e   1c                   endif
099670030515      *
099680030515     c                   ENDSR
099690060524      *
099700060524      *---------------------------------------------------------------*
099710060524      *?Reperimento dati da FIAR400F x trk "A"                       ?*
099720061107      *?(dati aggiuntivi per P.Assegnato)                            ?*
099730060524      *---------------------------------------------------------------*
099740060524     c     RepAR4_A      BEGSR
099750060524      *
099760061107     c*** (già fatto:)   clear                   dsBL4A
099770061107      *
099780060524     c                   eval      Wtrc = 'A'
099790060524     c     KLDVtrc       chain     FIAR4000
099800060524      *
099810060524if  1c                   if        %found(FIAR401L)
099820060524     c                   movel     AR4Not        dsBL4A
099830060524e   1c                   endif
099840060524      *
099850060524     c                   ENDSR
099860061107      *
099870061107      *---------------------------------------------------------------*
099880061107      *?Reperimento dati da FIAR400F x trk "D"                       ?*
099890061107      *?(2ª ragione sociale del destinatario)                        ?*
099900061107      *---------------------------------------------------------------*
099910061107     c     RepAR4_D      BEGSR
099920061107      *
099930061107     c*** NON esiste:    clear                   dsBL4D
099940061107     c*** (già fatto:)   clear                   DB0rd2
099950061107      *
099960061107     c                   eval      Wtrc = 'D'
099970061107     c     KLDVtrc       chain     FIAR4000
099980061107      *
099990061107if  1c                   if        %found(FIAR401L)
100000061107     c                   movel     AR4not        DB0rd2
100010061107e   1c                   endif
100020061107      *
100030061107     c                   ENDSR
100040061107      *
100050060626      *---------------------------------------------------------------*
100060060626      *?Reperimento dati da FIAR400F x trk "I"                       ?*
100070061107      *?(dati DPD)                                                   ?*
100080060626      *---------------------------------------------------------------*
100090061030     c     RepAR4_I      BEGSR
100100061030      *
100110061107     c*** (già fatto:)   clear                   dsBL4I
100120061107      *
100130061030     c                   eval      Wtrc = 'I'
100140061030     c     KLDVtrc       chain     FIAR4000
100150061030      *
100160061030if  1c                   if        %found(FIAR401L)
100170061030     c                   movel     AR4not        dsBL4I
100180060626e   1c                   endif
100190061030      *
100200061030     c                   ENDSR
100210071008      *
100220071015      *---------------------------------------------------------------*
100230071015      *?Reperimento dati da FIAR400F x trk "M"                       ?*
100240071015      *?(dati aggiuntivi per O.R.M. abbinato al P.Franco PrePagato)  ?*
100250071015      *---------------------------------------------------------------*
100260071015     c     RepAR4_M      BEGSR
100270071015      *
100280071015     c*** (già fatto:)   clear                   dsBL4M
100290071015      *
100300071015     c                   eval      Wtrc = 'M'
100310071015     c     KLDVtrc       chain     FIAR4000
100320071015      *
100330071015if  1c                   if        %found(FIAR401L)
100340071015     c                   movel     AR4Not        dsBL4M
100350071015e   1c                   endif
100360071015      *
100370071015     c                   ENDSR
100380061107      *
100390061107      *---------------------------------------------------------------*
100400061107      *?Reperimento dati da FIAR400F x trk "Q"                       ?*
100410061107      *?(Codice Fiscale del mittente a sx & del destinatario a dx)   ?*
100420061107      *---------------------------------------------------------------*
100430061107     c     RepAR4_Q      BEGSR
100440061107      *
100450061108     c*** (già fatto:)   clear                   dTA4Q
100460061107     c*** (già fatto:)   clear                   DB0cpi
100470061107      *
100480061107     c                   eval      Wtrc = 'Q'
100490061107     c     KLDVtrc       chain     FIAR4000
100500061107      *
100510061107if  1c                   if        %found(FIAR401L)
100520061108     c                   movel     AR4not        dTA4Q
100530061108e   1c                   endif
100540061107      *
100550061107     c                   ENDSR
100560061107      *
100570061107      *---------------------------------------------------------------*
100580061107      *?Reperimento dati da FIAR400F x trk "8" / "9"                 ?*
100590061107      *?(note)                                                       ?*
100600061107      *---------------------------------------------------------------*
100610061107     c     RepAR4_89     BEGSR
100620061107      *
100630061107     c*** NON esiste:    clear                   dsBL48
100640061107     c*** NON usata:     clear                   dsBL49
100650061107     c*** (già fatto:)   clear                   DB0nt1
100660061107     c*** (già fatto:)   clear                   DB0nt2
100670061107      *
100680061107     c                   eval      Wtrc = '8'
100690061107     c     KLDVtrc       chain     FIAR4000
100700061107if  1c                   if        %found(FIAR401L)
100710061107     c                   movel     AR4not        DB0nt1
100720061107e   1c                   endif
100730061107      *
100740061107     c                   eval      Wtrc = '9'
100750061107     c     KLDVtrc       chain     FIAR4000
100760061107if  1c                   if        %found(FIAR401L)
100770061107     c                   movel     AR4not        DB0nt2
100780061107e   1c                   endif
100790061107      *
100800061107     c                   ENDSR
100810030515      *
100820030515      *---------------------------------------------------------------*
100830050217      *?Carico le tabelle che servono: 3A/TB/5E.                     ?*
100840030515      *---------------------------------------------------------------*
100850030515     c     CarTAB        BEGSR
100860030515      *
100870030515      * Tab. "3A" - Codici Bolla
100880030515      * (NO se rimesso mittente)
100890030515     c                   clear                   ds3A
100900030515if  1c                   if        DB0ris <> 'M'
100910030515     c                   movel     '3A'          TBLcod
100920030515     c                   movel(p)  DB0cbo        TBLkey
100930030515     c     K03TAB        chain     TABEL
100940030515if  2c                   if        %found(TABEL00F)
100950030515     c                   movel     TBLuni        ds3A
100960030515e   2c                   endif
100970030515e   1c                   endif
100980070515     c                   eval      $Mamma =  (§3Abcm = 'S')
100990030515      *
101000030515      * Tab. "TB" - Tipi Bolla
101010030515      * (NO se rimesso mittente)
101020030515      * se Bolla in assegnato, con tipo bolla vedo se da contabilizzare
101030030515      * se no non stampo i dati fattura
101040030515     c                   clear                   dsTB
101050030515if  1c                   if        DB0ris <> 'M'
101060030515     c                   movel     'TB'          TBLcod
101070061030if  2c                   if            §3Atb2 <> *blanks
101080121029     c                             and DB0tbo <> 'P'
101090030515     c                             and DB0tbo <> '8'
101100030515     c                   movel(p)  §3Atb2        TBLkey
101110030515x   2c                   else
101120030515     c                   movel(p)  §3Atb1        TBLkey
101130030515e   2c                   endif
101140030515     c     K03TAB        chain     TABEL
101150030515if  2c                   if        %found(TABEL00F)
101160030515     c                   movel     TBLuni        dsTB
101170030515e   2c                   endif
101180030515e   1c                   endif
101190030515      *
101200030515      * Tab. "5E" - Tipi Servizio
101210030515     c                   clear                   ds5E
101220090513if  1c                   if        DB0tsp <> *blanks
101230030515     c                   movel     '5E'          TBLcod
101240030515     c                   movel(p)  DB0tsp        TBLkey
101250030515     c     K03TAB        chain     TABEL
101260030515if  2c                   if        %found(TABEL00F)
101270030515     c                   movel     TBLuni        ds5E
101280030515e   2c                   endif
101290030515e   1c                   endif
101300030515      *
101310030911      * Tab. "EI" - Esenzioni I.V.A.
101320030515     c                   clear                   dsEI
101330030911      * Tab. "1A" - Tipi incasso contrassegno
101340030911     c                   clear                   ds1A
101350030911      * Tab. "1P" - Consegne particolari
101360030911     c                   clear                   ds1P
101370150226      *//* Tab. "TBA" - Tipi bancale
101380150226     c*//                clear                   dTBA
101390030515      *
101400030515     c                   ENDSR
101410030911      *
101420030911      *---------------------------------------------------------------*
101430050217      *?Carico tabella "EI" (Esenzione I.V.A.)                       ?*
101440030911      *---------------------------------------------------------------*
101450030911     c     Tab_EI        BEGSR
101460030911      *
101470030911     c                   clear                   dsEI
101480030911if  1c                   if        DB0cei <> *blanks  and  DB0dce = *blanks
101490030911     c                   movel     'EI'          TBLcod
101500030911     c                   movel(p)  DB0cei        TBLkey
101510030911     c     K03TAB        chain     TABEL
101520030911if  2c                   if        %found(TABEL00F)
101530030911     c                   movel     TBLuni        dsEI
101540030911e   2c                   endif
101550030911e   1c                   endif
101560030911     c                   eval      DB0dce =  §EIdes
101570030911      *
101580030911     c                   ENDSR
101590030515      *
101600030515      *---------------------------------------------------------------*
101610050217      *?Carico tabella "1A" (Tipo incasso contrassegno)              ?*
101620030515      *---------------------------------------------------------------*
101630030515     c     Tab_1A        BEGSR
101640030515      *
101650030515     c                   clear                   ds1A
101660030515if  1c                   if        DB0cas <> *zeros
101670030515     c                   movel     '1A'          TBLcod
101680030515     c                   movel(p)  DB0tic        TBLkey
101690030515     c     K03TAB        chain     TABEL
101700030515if  2c                   if        %found(TABEL00F)
101710030515     c                   movel     TBLuni        ds1A
101720030515e   2c                   endif
101730030515e   1c                   endif
101740030515      *
101750030515      * Imposto i dati nella DS
101760030515     c                   eval      DB0dti = §1Ades
101770030515     c                   eval      DB0ucm = §1Aucm
101780030515     c                   eval      DB0ste = §1Aste
101790030515      *
101800030515     c                   ENDSR
101810030515      *
101820030515      *---------------------------------------------------------------*
101830050217      *?Carico tabella "1P" (Consegne particolari)                   ?*
101840030515      *---------------------------------------------------------------*
101850030515     c     Tab_1P        BEGSR
101860030515      *
101870030515     c                   clear                   ds1P
101880030515     c                   movel     '1P'          TBLcod
101890030515     c*** già impostato: movel(p)  DB0tc1/2      TBLkey
101900030515     c     K03TAB        chain     TABEL
101910030515if  1c                   if        %found(TABEL00F)
101920030515     c                   movel     TBLuni        ds1P
101930030515e   1c                   endif
101940030515      *
101950030515     c                   ENDSR
101960150226      *
101970150226      *//*---------------------------------------------------------------*
101980150226      *//*?Reperisco tabella "TBA" (Tipi Bancale)                       ?*
101990150226      *//*---------------------------------------------------------------*
102000150226     c*//  Tab_TBA       BEGSR
102010150226      *//*
102020150226     c*//                clear                   dTBA
102030150226     c*//                reset                   TIBS02ds
102040150226     c*//                eval      T02cod = 'TBA'
102050150226     c*//                movel(p)  Wtba          T02ke1
102060150226     c*//                call      'TIBS02R'
102070150226     c*//                parm                    KPJBA
102080150226     c*//                parm                    TIBS02ds
102090150226if  1c*//                if        T02err = *blanks
102100150226     c*//                movel     T02uni        dTBA
102110150226e   1c*//                endif
102120150226      *//*
102130150226     c*//                ENDSR
102140030515      *
102150030515      *---------------------------------------------------------------*
102160050217      *?Calcolo importi LDV                                          ?*
102170030515      *---------------------------------------------------------------*
102180061031     c     sr_DS_IMPO    BEGSR
102190030515      *
102200030515      * Vedo se esiste tassazione
102210030515      *  solo se tipo bolla contabilizzabile
102220061030if  1c                   IF        §TBfcb =  '1'
102230030515     c
102240061030     c                   eval      Wtrc   =  '1'
102250040607     c     KLDVtrc       chain     FIAR6000
102260030515     c
102270030515if  2c                   if        %found(FIAR601L)
102280030515     c                   movel     AR6div        DB0div
102290030515     c                   exsr      CtrDIV
102300030515      *
102310030515      * NON c'è 2ª bolla
102320061031if  3c                   if            DB0tb2 = *blanks
102330121029     c                             or  DB0tbo = 'P'
102340061031     c                             or  DB0tbo = '8'
102350030515      *  Estremi fattura
102360030515     c                   z-add     AR6nft        DB0nft
102370030515     c                   z-add     AR6dft        DB0dft
102380130124     c                   z-add     AR6fiv        wB0fiv
102390061031      *  Se è assegnato o franco prepagato => stampo tutta la tassaz.
102400061031if  4c                   if            %subst(DB0tb1:1:1) = 'A'
102410061031     c                             or  DB0tbo = '8'
102420061030     c                   eval      Wtrc   =  '1'
102430030515     c                   exsr      RepTASSAZ
102440030515e   4c                   endif
102450030515e   3c                   endif
102460030515e   2c                   endif
102470030515      *
102480061030      * C'è 2ª bolla
102490061031if  2c                   IF            DB0tb2 <> *blanks
102500121029     c                             and DB0tbo <> 'P'
102510061031     c                             and DB0tbo <> '8'
102520030515      *
102530061030     c                   eval      Wtrc   =  '2'
102540040607     c     KLDVtrc       chain     FIAR6000
102550030515      *
102560061030if  3c                   If        %found(FIAR601L)
102570030515     c                   movel     AR6div        DB0div
102580030515     c                   exsr      CtrDIV
102590030515     c                   movel     AR6cei        DB0cei
102600030515      *   Estremi fattura
102610030515     c                   z-add     AR6nft        DB0nft
102620030515     c                   z-add     AR6dft        DB0dft
102630130124     c                   z-add     AR6fiv        wB0fiv
102640030515if  4c                   if        AR6ksc <> *zeros
102650030515     c                   eval      DB0ccd =  AR6ksc
102660030515e   4c                   endif
102670061030     c                   eval      Wtrc   =  '2'
102680030515     c                   exsr      RepTASSAZ
102690061030e   3c                   EndIf
102700030515      *
102710030515e   2c                   ENDIF
102720030515      *
102730030515e   1c                   ENDIF
102740030515      *
102750030515      * Vedo se ha contrassegno
102760060505     c     KLDV          chain     FIAR9000
102770060505if  1c                   IF        %found(FIAR901L)
102780061030     c                   eval      DB0cas =  AR9cas
102790061030     c                   eval      DB0vca =  AR9vca
102800061030     c                   eval      DB0gca =  AR9gca
102810061030     c                   eval      DB0tic =  AR9tic
102820030515x   1c                   ELSE
102830030515     c                   clear                   DB0cas
102840030515     c                   clear                   DB0vca
102850030515     c                   clear                   DB0gca
102860030515     c                   clear                   DB0tic
102870030515     c                   clear                   DB0dti
102880030515     c                   clear                   DB0ucm
102890030515     c                   clear                   DB0ste
102900030515e   1c                   ENDIF
102910120509      *
102920120509      * Se P.F. Prepagato verificherò il Codice Fiscale del Mittente
102930120509      *   (NON se mittente estero)
102940120509if  1c                   if           ( DB0tbo  = '8'
102950120509     c                             and (DB0nft  <> *zeros
102960120509     c                              or  DB0ift  <> *zeros)
102970120509     c                             and  DB0nzm  =  *blanks )
102980120509     c                   exsr      RepAR4_Q
102990120509     c                   movel     §TA4Qcfm      DB0cpi
103000120509e   1c                   endif
103010121029      * Se P.A. con fattura immediata o con 2ª bolla verificherò il
103020121029      *   Codice Fiscale (NON se destinatario estero)
103030121026if  1c*//                if           ( DB0tbo  = 'A'
103040121029if  1c                   if          ( (%subst(DB0tb1:1:1) = 'A'
103050121029     c                             or  (DB0tb2  <> *blanks
103060121029     c                             and  DB0tbo  <> 'P'
103070121029     c                             and  DB0tbo  <> '8') )
103080120509     c                             and (DB0nft  <> *zeros
103090120509     c                              or  DB0ift  <> *zeros)
103100120509     c                             and  DB0nzd  =  *blanks )
103110120509     c                   exsr      RepAR4_Q
103120120509     c                   movel     §TA4Qcfd      DB0cpi
103130120509e   1c                   endif
103140120509     c                   eval      wB0cfi = DB0cpi
103150061030      *
103160030515      * Vedo se si tratta di bolla figlia:
103170040607      * Se bolla con MAMMA prendo c/a e segnacolli da bolla MAMMA
103180070515if  1c                   if        $Mamma =  *on
103190081117     c                   exsr      BollaFiglia
103200030515e   1c                   endif
103210030515      *
103220030515     c                   ENDSR
103230030515      *
103240030515      *---------------------------------------------------------------*
103250050217      *?Decodifica LNA                                               ?*
103260030515      *---------------------------------------------------------------*
103270030515     c     DecLNA        BEGSR
103280030515      *
103290140206if  1c*//                if            DB0dla =  *blanks
103300140206     c*//                          and DB0ris <> 'M'
103310140206if  1c                   if        DB0ris <> 'M'
103320030515      *
103330040311     c                   eval      SAVlna = DB0lna
103340070312     c                   clear                   OG146arr
103350061016     c                   clear                   OG148arr
103360070312     c                   clear                   OG150arr
103370030515     c     DB0lna        chain     AZORG
103380030515      *
103390030515if  2c                   if        %found(AZORG01L)
103400030515     c                   eval      DB0fag = ORGfag
103410030515     c                   eval      DB0dla = ORGdes
103420030515     c                   eval      DB0inl = ORGind
103430030515     c                   eval      DB0cal = ORGcpf
103440030515     c                   eval      DB0lol = ORGloc
103450030515     c                   eval      DB0prl = ORGpro
103460030515     c                   eval      DB0tel = ORGtel
103470140129     c*//                eval      DB0fax = ORGfax
103480140129     c                   eval      DB0fax = ORGdd2
103490070312     c                   movel     ORGde6        OG146arr
103500061016     c                   movel     ORGde8        OG148arr
103510070312     c                   movel     ORGdf0        OG150arr
103520030515e   2c                   endif
103530030515      *
103540030515e   1c                   endif
103550030515      *
103560030515     c                   ENDSR
103570030515      *
103580030515      *---------------------------------------------------------------*
103590050217      *?Reperimento dati di tutta la tassazione per la stampa        ?*
103600030515      *---------------------------------------------------------------*
103610030515     c     RepTASSAZ     BEGSR
103620030515      *
103630030515     c                   eval      DB0imv = AR6imv
103640030515     c                   eval      DB0cei = AR6cei
103650030515     c                   eval      DB0ift = AR6ift
103660030911      * Esenzione IVA
103670030911if  1c                   if            DB0cei <> *blanks
103680030911     c                             and DB0dce =  *blanks
103690030911     c                   exsr      Tab_EI
103700030911e   1c                   endif
103710030515      *
103720030515      * Controllo se ci sono varie esenti
103730030515      *
103740030515      * - varia 1
103750030515     c                   eval      Wsvx = AR6sv1
103760030515     c                   eval      Wvax = AR6va1
103770030515     c                   exsr      RepTASSAZ_b
103780030515      * - varia 2
103790030515     c                   eval      Wsvx = AR6sv2
103800030515     c                   eval      Wvax = AR6va2
103810030515     c                   exsr      RepTASSAZ_b
103820030515      * - varia 3
103830030515     c                   eval      Wsvx = AR6sv3
103840030515     c                   eval      Wvax = AR6va3
103850030515     c                   exsr      RepTASSAZ_b
103860030515      * - varie ulteriori solo per tipo record 1 (prima bolla)
103870040607     c     KLDVtrc       setll     FIAR7000
103880040607     c     KLDVtrc       reade     FIAR7000
103890030515      *
103900030515do  2c                   Dow       Not %eof(FIAR701L)
103910030515      *
103920030515     c                   eval      Wsvx = AR7svn
103930030515     c                   eval      Wvax = AR7van
103940030515     c                   exsr      RepTASSAZ_b
103950030515      *
103960040607     c     KLDVtrc       reade     FIAR7000
103970030515e   2c                   Enddo
103980030515      *
103990030515     c                   ENDSR
104000030515      *
104010030515      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
104020050217      *?Reperimento dati di tutta la tassazione per la stampa        ?*
104030050217      *?- 2ª parte -                                                 ?*
104040030515      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
104050030515     c     RepTASSAZ_b   BEGSR
104060030515      *
104070030515sel 1c                   Select
104080030515      *
104090030515w   1c                   When      Wsvx       = *blanks
104100030515      *
104110030515w   1c                   When      Wsvx       = §$2iva
104120030515     c                   eval      DB0iva     = Wvax
104130030515      *
104140030515w   1c                   When      Wsvx       = §$2bol
104150030515     c                   add       Wvax          DB0ese
104160050624     c                   z-add     Wvax          wBolloVirt
104170050624     c                   eval      $BolloVirt = *on
104180030515      *
104190030515x   1c                   Other
104200050624     c                   eval      *in31      = *off
104210030515     c     Wsvx          lookup    $Var                                   31
104220030515     c   31              add       Wvax          DB0ese
104230030515     c   31              sub       Wvax          DB0imv
104240030515      *
104250030515w   1c                   Endsl
104260030515      *
104270030515     c                   ENDSR
104280030515      *
104290030515      *---------------------------------------------------------------*
104300050217      *?Chain su TABEL per le PARTICOLARITA'                         ?*
104310030515      *---------------------------------------------------------------*
104320030515     c     TABxPAR       BEGSR
104330030515      *
104340100513     c                   movel(p)  WCdPar        w053a
104350030515     c                   movel     '7V'          TBLcod
104360030515     c                   movel(p)  KeyPAR        TBLkey
104370030515     c     K03TAB        chain     TABEL
104380030515      *
104390030515if  1c                   if        %found(TABEL00F)
104400030515     c                             and TBLflg = *blanks
104410100513     c                   eval      w053a = WCdPar + '-'
104420100513     c                                   + %trim(%subst(TBLuni:1:49))
104430030515x   1c                   else
104440030515     c                   clear                   TBLuni
104450030515e   1c                   endif
104460030515      *
104470030515     c                   ENDSR
104480030515      *
104490030515      *---------------------------------------------------------------*
104500050217      *?CONTROLLO C/A SE E' DA CONVERTIRE IN EURO                    ?*
104510030515      *---------------------------------------------------------------*
104520030515     c     CtrCAs        BEGSR
104530030515      *
104540030515      * E' da convertire
104550030515if  1c                   if        DB0ucm = 'S'
104560030515     c                   eval      $CAeur = *on
104570030515e   1c                   endif
104580030515      *
104590030515      * Se in EURO è da convertire e da stampare in EURO
104600030515if  1c                   if        DB0vca = §GEDcn
104610030515     c                   eval      $CAeur = *on
104620030515     c                   endif
104630030515      *
104640030515      * Devo convertire il c/a da valuta a moneta di conto
104650030624if  1c                   if        DB0vca <> §GEDcn
104660030515     c                   clear                   YEURCODS
104670030515     c                   movel     DB0vca        YECdvi
104680030515     c                   z-add     DB0cas        YECimi
104690030515     c                   movel     §GEDcn        YECdvo
104700030515     c                   call      'YEURCO'
104710030515     c                   parm                    YEURCODS
104720030515if  2c                   if        YECesi = *blanks
104730030515     c                   z-add     YECimo        Wcas
104740030515e   2c                   endif
104750030515x   1c                   else
104760030515     c                   z-add     DB0cas        Wcas
104770030515e   1c                   endif
104780030515      *
104790030515     c                   ENDSR
104800030515      *
104810030515      *---------------------------------------------------------------*
104820050217      *?CONTROLLO DIVISA DELLA FATTURA                               ?*
104830030515      *---------------------------------------------------------------*
104840030515     c     CtrDIV        BEGSR
104850030515      *
104860030515     c                   clear                   dsCV
104870030515      *
104880030515     c                   movel     'CV'          TBLcod
104890030515     c                   movel(p)  DB0div        TBLkey
104900030515     c     K03TAB        chain     TABEL
104910030515     c                   if        %found(TABEL00F)
104920030515     c                   movel     TBLuni        dsCV
104930030515     c                   movel     §CVfdc        DB0fdc
104940030515     c                   endif
104950030515      *
104960030515     c                   ENDSR
104970030515      *
104980030515      *---------------------------------------------------------------*
104990050217      *?SE BOLLA CON MAMMA PRENDO C/A E SEGNACOLLI DA BOLLA MAMMA    ?*
105000030515      *---------------------------------------------------------------*
105010081117     c     BollaFiglia   BEGSR
105020030515      *
105030090528     c                   if        not %open(FNLBL01L)
105040090528     c                   open      FNLBL01L
105050090528     c                   endif
105060090528      *
105070040607     c     KLDV          chain     FNLBL000
105080030515if  1c                   if        %found(FNLBL01L)
105090030515      *
105100030515     c     KLBL          chain     FNARB000
105110030515if  2c                   if        %found(FNARB01L)
105120030515      *
105130031015     c                   eval      wB0nrs = ARBnrs
105140030515     c                   eval      DB0fns = ARBfns
105150030515     c                   eval      DB0fls = ARBfls
105160030515     c                   eval      DB0ncd = ARBncd
105170030515     c                   eval      DB0nca = ARBnca
105180050114     c**!!!              eval      DB0xco = ARBxco
105190030515     c                   eval      DB0znc = ARBznc
105200030515      * Aggancio AR9
105210060505     c     KLBL          chain     FIAR9000
105220060505if  3c                   IF        %found(FIAR901L)
105230030515     c                   eval      DB0cas = AR9cas
105240030515     c                   eval      DB0vca = AR9vca
105250030515     c                   eval      DB0gca = AR9gca
105260030515     c                   eval      DB0tic = AR9tic
105270030515x   3c                   ELSE
105280030515     c                   clear                   DB0cas
105290030515     c                   clear                   DB0vca
105300030515     c                   clear                   DB0gca
105310030515     c                   clear                   DB0tic
105320030515     c                   clear                   DB0dti
105330030515     c                   clear                   DB0ucm
105340030515     c                   clear                   DB0ste
105350030515e   3c                   ENDIF
105360030515      *
105370030515e   2c                   endif
105380030515      *
105390030515e   1c                   endif
105400030515      *
105410030515     c                   ENDSR
105420030903      *
105430030903      *---------------------------------------------------------------*
105440050217      *?Impostazione Quantità e Unità di Misura da FNORM             ?*
105450030903      *---------------------------------------------------------------*
105460030903     c     QUM_FNORM     BEGSR
105470030903      *
105480030905     c*** (già fatto:)   clear                   $Qum
105490030905     c                   clear                   Wx
105500030903      * Colli
105510030905if  1c                   if        ORMncl   > *zeros
105520030905     c                             and Wx   < %elem($Qum)
105530030905     c                   add       1             wx
105540030905     c                   eval      $Qum(wx) = %editc(ORMncl : '4')
105550030905     c                                      + ' colli'
105560030903e   1c                   endif
105570030903      * Peso
105580030905if  1c                   if        ORMpkg   > *zeros
105590030905     c                             and Wx   < %elem($Qum)
105600030905     c                   add       1             wx
105610030905     c                   eval      $Qum(wx) = %editc(ORMpkg : '4')
105620030905     c                                      + ' kg'
105630030905e   1c                   endif
105640071025      * Volume bancali
105650071025if  1c                   if        ORMvlm   > *zeros
105660071025     c                             and Wx   < %elem($Qum)
105670071025     c                   add       1             wx
105680071025     c                   eval      $Qum(wx) = %editc(ORMvlm : '4')
105690071025     c                                      + ' mc'
105700071025e   1c                   endif
105710030905     *** * Volume
105720030905     *** * (in ultimo posto SOLO SE non ci sono bilici/motrici/autotreni)
105730030905if  1***c                   if        ORMvlm   > *zeros
105740030905     ***c                             and (Wx  < %elem($Qum) - 1
105750030905     ***c                              or  Wx  = %elem($Qum) - 1
105760030905     ***c                              and w0112 = *zeros)
105770030905     ***c                   add       1             wx
105780030905     ***c                   eval      $Qum(wx) = %editc(ORMvlm : '4')
105790030905     ***c                                      + ' metri cubi'
105800030905e   1***c                   endif
105810030905     *** * Bancali
105820030905     *** * (in ultimo posto SOLO SE non ci sono bilici/motrici/autotreni)
105830030905if  1***c                   if        ORMbnc   > *zeros
105840030905     ***c                             and (Wx  < %elem($Qum) - 1
105850030905     ***c                              or  Wx  = %elem($Qum) - 1
105860030905     ***c                              and w0112 = *zeros)
105870030905     ***c                   add       1             wx
105880030905     ***c                   eval      $Qum(wx) = %editc(ORMbnc : '4')
105890030905     ***c                                      + ' bancali'
105900030905e   1***c                   endif
105910030905     *** * Bilici
105920030905if  1***c                   if        ORMblc  > *zeros
105930030905     ***c                             and Wx  < %elem($Qum)
105940030905     ***c                   add       1             wx
105950030905     ***c                   eval      $Qum(wx) = %editc(ORMblc : '4')
105960030905     ***c                                      + ' bilici'
105970030905e   1***c                   endif
105980030905     *** * Motrici
105990030905if  1***c                   if        ORMmtc  > *zeros
106000030905     ***c                             and Wx  < %elem($Qum)
106010030905     ***c                   add       1             wx
106020030905     ***c                   eval      $Qum(wx) = %editc(ORMmtc : '4')
106030030905     ***c                                      + ' motrici'
106040030905e   1***c                   endif
106050030905     *** * Autotreni
106060030905if  1***c                   if        ORMatt  > *zeros
106070030905     ***c                             and Wx  < %elem($Qum)
106080030905     ***c                   add       1             wx
106090030905     ***c                   eval      $Qum(wx) = %editc(ORMatt : '4')
106100030905     ***c                                      + ' autotreni'
106110030905e   1***c                   endif
106120030903      *
106130030903     c                   ENDSR
106140130306      *
106150130306      /free
106160130306
106170130306       //--------------------------------------------------------------
106180130306       //?Scrittura record (relativo alla spedizione) nel *file PDFLIST?
106190130306       //--------------------------------------------------------------
106200130306       BEGSR  sr_WrtRec_PdfList;
106210130306
106220130307         // -?NON richiesta stampa del PDF?
106230140404         if  %parms() = 1;
106240130306           leavesr;
106250130306         endif;
106260130306
106270130306         // -?Reperimento numero "chiave elenco"?(SE non ricevuto)?
106280130306         If  DB6num <= *zero;
106290130306
106300130306           clear  TRUL33ds;
106310130306           I33tla = 'L';
106320130306           I33ope = *zero;
106330130306           I33cnu = 55;
106340130306           I33num = 1;
106350130306           kpjbu  = TRUL33ds;
106360130306
106370130306           TRUL33R ( kpjba );
106380130306
106390130306           TRUL33ds = kpjbu;
106400130307           if  O33err > *zero;
106410130307             Pmsg1  = $Msg(3);
106420130307             Pmsg2  = $Msg(O33err + 3);
106430130418             *in33  = *off;
106440130418             exsr  Posiz_Box;
106450130307             write  A4prtMSG;
106460130307             DB0tms = 'E';
106470130306             exsr  sr_End;
106480130306           endif;
106490130306
106500130306           DB6num = %editc( O33nri : 'X' );
106510130306
106520130306         EndIf;
106530130307
106540130307         // -?Apertura file?
106550130307         if  Not %open(PDFLIST);
106560130307           open  PDFLIST;
106570130307         endif;
106580130306
106590130308         // -?Ciclo di lettura record "§..." (ex "§PDF") in FIAR801L?
106600130306         clear  k06fiar801;
106610130306         k_AR8aas = DB0aas;
106620130306         k_AR8lnp = DB0lnp;
106630130306         k_AR8nrs = DB0nrs;
106640130306         k_AR8nsp = DB0nsp;
106650130306         k_AR8tmd = c_§PDF;
106660130306         setll  %kds( k06fiar801 : 5 )  FIAR8000;
106670130308         reade  %kds( k06fiar801 : 4 )  FIAR8000;
106680130306
106690130308         DoW  Not %eof(FIAR801L)  and  %subst(AR8tmd : 1 : 1) = '§';
106700130306
106710130306           dAR8§PDF = AR8uni;
106720130308
106730130315           If  AR8atb = *blank  and  §AR8Ptrt = 'S';
106740130306
106750130308             // -?Scrittura del rec. relativo alla sped. nel *file PDFLIST?
106760130308             clear  RPDF;
106770130308             PDFklist  = DB6num;
106780130308             PDFksped  = %editc( DB0aas : 'X' ) + %editc( DB0lnp : 'X' ) +
106790130308                         %editc( DB0nrs : 'X' ) + %editc( DB0nsp : 'X' );
106800130311             PDFkey    = %trimr(§AR8PpdfP) + '_' + §AR8PpdfN;
106810130311             //PDFpath   = *blank;
106820130308             PDFinfo   = c_CopiaXdest + ' - ' + Prsd +
106830130308                         ' Spedizione ' + %editc(DB0lnp : 'X') +
106840130308                                    ' ' + %editc(DB0nrs : 'X') +
106850130308                                    ' ' + %editc(DB0nsp : 'X') +
106860130308                         ' del ' + %editc(Pdsp : 'Y') + ' ' +
106870130308                         Pndc;
106880130308             PDFresol  = 'S';
106890130308             PDFcopieM = '1';
106900130308             PDFcopieD = '0';
106910130308             PDFcopieS = '0';
106920130308             //PDFnoInfo = *blank;
106930130308             //PDFinfoP  = *blank;
106940130308             //PDFprtFst = *blank;
106950130308             wTime_ds  = %char( %timestamp() );
106960131008             PDFinsDt  = wTime_ds.wDate_Iso;
106970131008             PDFinsTm  = wTime_ds.wTime;
106980131008             //PDFprtDt  = wTime_ds.wDate_Iso;
106990131008             //PDFprtTm  = wTime_ds.wTime;
107000130308             PDFusrins = SDSusr;
107010130308             //__________
107020130308             write  RPDF;
107030130308             //¯¯¯¯¯¯¯¯¯¯
107040130308
107050130308           EndIf;
107060130306
107070130418           reade  %kds( k06fiar801 : 4 )  FIAR8000;
107080130306
107090130306         EndDo;
107100130306
107110130306       ENDSR;
107120130306
107130130306       //--------------------------------------------------------------
107140140303       //?Stampa immagini associate alle spediz. (dati già in PDFLIST) ?
107150130306       //--------------------------------------------------------------
107160130306       BEGSR  sr_PrtPdf;
107170130306
107180130306         // -?Impostazione parametri?
107190130312         PDF_KList    = DB6num;
107200130312         PDF_Printer  = trulPOVRds.§ULPoOutQ;
107210130312         PDF_FormType = trulPOVRds.§ULPoFType;
107220130312         PDF_UsrDta   = trulPOVRds.§ULPoUDta;
107230130306
107240130306         // -?Richiamo funzione di stampa di tutte le immagini PDF?
107250130312         //  ?(segnalando - in stampa - l'eventuale errore)?
107260130312         callp(E)  Stampa_PDF ( PDF_KList :
107270130312                                PDF_Printer :
107280130312                                PDF_FormType :
107290130312                                PDF_UsrDta );
107300130312
107310130312         if  %error;
107320130312           Pmsg2  = $Msg(7);
107330130418           *in33  = *off;
107340130418           exsr  Posiz_Box;
107350130312           write  A4prtMSG;
107360130312           DB0tms = 'E';
107370130312           exsr  sr_End;
107380130312         endif;
107390130307
107400130307         // -?Restituzione tipo modulo stampato?
107410130307         DB0tms = 'F';
107420130306
107430130306       ENDSR;
107440130306
107450130306      /end-free
107460030903      *
107470030515      *---------------------------------------------------------------*
107480050217      *?DEFINIZIONE KEY-LIST                                         ?*
107490030515      *---------------------------------------------------------------*
107500030515     c     DefKLIST      BEGSR
107510030515      *
107520051115      * - FNBLP01L / FNBLT01L /
107530060505      *   FNARB01L / FNART01L /
107540030515      *   FNLBL01L /
107550060505      *   FIAR501L/31C / FIAR901L
107560040607     c     KLDV          klist
107570030515     c                   kfld                    DB0aas
107580030515     c                   kfld                    DB0lnp
107590030515     c                   kfld                    DB0nrs
107600030515     c                   kfld                    DB0nsp
107610060505      * - FIAR401L / FIAR601L / FIAR701L
107620060505     c     KLDVtrc       klist
107630060505     c                   kfld                    DB0aas
107640060505     c                   kfld                    DB0lnp
107650060505     c                   kfld                    DB0nrs
107660060505     c                   kfld                    DB0nsp
107670060505     c                   kfld                    Wtrc
107680040607      * - FIAR501L/31C
107690030717     c     K05AR501      klist
107700030717     c                   kfld                    DB0aas
107710030717     c                   kfld                    DB0lnp
107720030717     c                   kfld                    DB0nrs
107730030717     c                   kfld                    DB0nsp
107740030717     c                   kfld                    AR5trd
107750040607      * - FIAR801L
107760040607     c     K06AR801      klist
107770040607     c                   kfld                    DB0aas
107780040607     c                   kfld                    DB0lnp
107790040607     c                   kfld                    DB0nrs
107800040607     c                   kfld                    DB0nsp
107810040607     c                   kfld                    AR8tmd
107820040607     c                   kfld                    AR8prr
107830070514      * - FNBLT01L / FNART01L
107840070514     c     KLDVnsc       Klist
107850070514     c                   Kfld                    DB0aas
107860070514     c                   Kfld                    DB0lnp
107870070514     c                   Kfld                    DB0nrs
107880070514     c                   Kfld                    DB0nsp
107890070514     c                   Kfld                    Wnsc
107900070319      * - FIARN11L
107910070319     c     K06ARN11      klist
107920070319     c                   kfld                    DB0aas
107930070319     c                   kfld                    DB0lnp
107940070319     c                   kfld                    DB0nrs
107950070319     c                   kfld                    DB0nsp
107960070319     c                   kfld                    ARNcdn
107970070319     c                   kfld                    ARNgst
107980090528      * - FNBLP01L/ / FNARB01L / FIAR901L di bolla MAMMA
107990070514     c     KLBL          klist
108000070514     c                   kfld                    LBLaap
108010070514     c                   kfld                    LBLlpp
108020070514     c                   kfld                    LBLnrp
108030070514     c                   kfld                    LBLnsp
108040070514      * - FNBLT01L / FNART01L di bolla MAMMA
108050070514     c     KLBLnsc       Klist
108060070514     c                   kfld                    LBLaap
108070070514     c                   kfld                    LBLlpp
108080070514     c                   kfld                    LBLnrp
108090070514     c                   kfld                    LBLnsp
108100070514     c                   Kfld                    Wnsc
108110070514      * - FIARS01L
108120070514     c     K05ARS01P     klist
108130070514     c                   kfld                    BLTfls
108140070514     c                   kfld                    BLTlna
108150070514     c                   kfld                    BLTnrs
108160070514     c                   kfld                    BLTnsc
108170070514     c                   kfld                    ARStrc
108180070514     c     K05ARS01A     klist
108190070514     c                   kfld                    ARTfls
108200070514     c                   kfld                    ARTlna
108210070514     c                   kfld                    ARTnrs
108220070514     c                   kfld                    ARTnsc
108230070514     c                   kfld                    ARStrc
108240060505      * - FNORM01L
108250060505     c     K04ORM01      klist
108260071015     c                   kfld                    ORMpoe
108270071015     c                   kfld                    ORMnsr
108280071015     c                   kfld                    ORMnor
108290071015     c                   kfld                    ORMnrv
108300030515      * - TABEL00F
108310030515     c     K03TAB        klist
108320040223     c                   kfld                    TBLkut
108330030515     c                   kfld                    TBLcod
108340030515     c                   kfld                    TBLkey
108350030515     c     K02TAB        klist
108360040223     c                   kfld                    TBLkut
108370030515     c                   kfld                    TBLcod
108380030515      * - FLNUF01L
108390030515     c     K04NUF        klist
108400030515     c                   kfld                    NUFaaa
108410030515     c                   kfld                    NUFcnu
108420030515     c                   kfld                    NUFfil
108430030515     c                   kfld                    NUFfl2
108440030515      *
108450030515     c                   ENDSR
108460030515
108470030515** > $Msg ---------------------------------------------------------------------*
108480030515IMPOSSIBILE CONTINUARE LA RISTAMPA BOLLE
108490071112PERCHE' STAMPATE PIU' DI &NrLdV BOLLE !!!
108500130307ERRORE IN REPERIMENTO NUMERATORE PER STAMPA PDF ALLEGATO ALLA LDV:
108510130307   NON REPERITO IL NUMERATORE
108520130307   NUMERATORE ALLOCATO
108530130307   SUPERATO IL LIMITE MASSIMO PER IL NUMERATORE
108540130312RILEVATO ERRORE IN FASE DI STAMPA DEI DOCUMENTI ALLEGATI
108550140303** > $GgS/$Gio *
1085600306241LUNEDI
1085700306242MARTEDI
1085800410263MERCOLEDI
1085900306244GIOVEDI
1086000306245VENERDI
1086100305156SABATO
1086200305157DOMENICA
108630030515M          MATT.
108640030515P          POM.
