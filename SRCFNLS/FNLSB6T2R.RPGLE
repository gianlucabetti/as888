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
000840171117       // -?Bolle: estensione DPD?
000850171117     fFIPND01L  if   e           k disk    usropn
000860110411       // -?O.R.M.?
000870030903     fFNORM01L  if   e           k disk    usropn
000880140411     fFNORE01L  if   e           k disk    usropn
000890130306
000900130306       // -?Elenco PDF da stampare?
000910130306     fPDFLIST   o    e             disk    usropn
000920130306
000930110411       // -?Printer File?
000940130710     fFNLSB6P   o    e             printer usropn infds(prtINFOa4)
000950080604     f                                     rename(LSB6POS :A4prtPOS )
000960130418     f                                     rename(LSB6BOX :A4prtBOX )
000970120530     f*//                                  rename(LSB6LOGBAR:A4LogoBAR)
000980120530     f*//                                  rename(LSB6LOGDA1:A4LogoDA1)
000990120530     f*//                                  rename(LSB6LOGBR2:A4LogoBR2)
001000120530     f*//                                  rename(LSB6LOGDA2:A4LogoDA2)
001010120530     f                                     rename(LSB6LOGBRT:A4LogoBRT)
001020120530     f                                     rename(LSB6LOGDA3:A4LogoDA3)
001030130418     f                                     rename(LSB6LDV :A4prtLDV )
001040080604     f                                     rename(LSB6LDVB:A4prtLDVB)
001050080922     f                                     rename(LSB6DPD :A4prtDPD )
001060080604     f                                     rename(LSB6FED1:A4prtFED1)
001070080604     f                                     rename(LSB6FED2:A4prtFED2)
001080080604     f                                     rename(LSB6FED3:A4prtFED3)
001090080604     f                                     rename(LSB6AR51:A4prtAR51)
001100080604     f                                     rename(LSB6AR52:A4prtAR52)
001110080604     f                                     rename(LSB6AR53:A4prtAR53)
001120080604     f                                     rename(LSB6AR54:A4prtAR54)
001130080604     f                                     rename(LSB6AR5A:A4prtAR5A)
001140150304     f*//                                  rename(LSB6AR5B:A4prtAR5B)
001150150304     f*//                                  rename(LSB6AR5C:A4prtAR5C)
001160080604     f                                     rename(LSB6AR5G:A4prtAR5G)
001170080604     f                                     rename(LSB6DB0 :A4prtDB0)
001180080604     f                                     rename(LSB6DT0 :A4prtDT0)
001190080604     f                                     rename(LSB6DT1 :A4prtDT1)
001200080604     f                                     rename(LSB6DT2 :A4prtDT2)
001210080604     f                                     rename(LSB6DC1 :A4prtDC1)
001220080604     f                                     rename(LSB6DK0 :A4prtDK0)
001230080604     f                                     rename(LSB6DK1 :A4prtDK1)
001240080604     f                                     rename(LSB6DK2 :A4prtDK2)
001250080604     f                                     rename(LSB6DK3 :A4prtDK3)
001260080604     f                                     rename(LSB6DK4 :A4prtDK4)
001270080604     f                                     rename(LSB6DK5 :A4prtDK5)
001280081222     f                                     rename(LSB6DLL :A4prtDLL)
001290080604     f                                     rename(LSB6DL0 :A4prtDL0)
001300080604     f                                     rename(LSB6DL0A:A4prtDL0A)
001310080604     f                                     rename(LSB6DL0B:A4prtDL0B)
001320080604     f                                     rename(LSB6DL0C:A4prtDL0C)
001330080604     f                                     rename(LSB6DL0D:A4prtDL0D)
001340080604     f                                     rename(LSB6DL0E:A4prtDL0E)
001350080604     f                                     rename(LSB6DL0F:A4prtDL0F)
001360080604     f                                     rename(LSB6DL1A:A4prtDL1A)
001370080604     f                                     rename(LSB6DL1B:A4prtDL1B)
001380080604     f                                     rename(LSB6DL1C:A4prtDL1C)
001390080604     f                                     rename(LSB6DL1D:A4prtDL1D)
001400080604     f                                     rename(LSB6DL1E:A4prtDL1E)
001410080604     f                                     rename(LSB6DL2 :A4prtDL2)
001420080604     f                                     rename(LSB6DL21:A4prtDL21)
001430080604     f                                     rename(LSB6DL3 :A4prtDL3)
001440090323     f                                     rename(LSB6DL31:A4prtDL31)
001450080604     f                                     rename(LSB6DL4 :A4prtDL4)
001460080604     f                                     rename(LSB6DL5 :A4prtDL5)
001470080604     f                                     rename(LSB6DL6 :A4prtDL6)
001480080604     f                                     rename(LSB6DL7 :A4prtDL7)
001490080604     f                                     rename(LSB6DL8 :A4prtDL8)
001500080604     f                                     rename(LSB6DL9 :A4prtDL9)
001510080604     f                                     rename(LSB6AC0 :A4prtAC0)
001520080604     f                                     rename(LSB6AC0A:A4prtAC0A)
001530080604     f                                     rename(LSB6AC0B:A4prtAC0B)
001540080604     f                                     rename(LSB6AC1A:A4prtAC1A)
001550080604     f                                     rename(LSB6AC1B:A4prtAC1B)
001560080604     f                                     rename(LSB6AC2 :A4prtAC2)
001570080604     f                                     rename(LSB6AC3 :A4prtAC3)
001580080604     f                                     rename(LSB6AC4 :A4prtAC4)
001590080604     f                                     rename(LSB6ES0 :A4prtES0)
001600080604     f                                     rename(LSB6ES0A:A4prtES0A)
001610080604     f                                     rename(LSB6ES0B:A4prtES0B)
001620080604     f                                     rename(LSB6ES1A:A4prtES1A)
001630080604     f                                     rename(LSB6ES1B:A4prtES1B)
001640080604     f                                     rename(LSB6ES2 :A4prtES2)
001650080604     f                                     rename(LSB6ES3 :A4prtES3)
001660080604     f                                     rename(LSB6ES4 :A4prtES4)
001670080604     f                                     rename(LSB6SP0 :A4prtSP0)
001680080604     f                                     rename(LSB6SP1 :A4prtSP1)
001690080604     f                                     rename(LSB6NUL :A4prtNUL)
001700080604     f                                     rename(LSB6GEN :A4prtGEN)
001710080604     f                                     rename(LSB6LO  :A4prtLO)
001720080604     f                                     rename(LSB6SEG :A4prtSEG)
001730080604     f                                     rename(LSB6MSG :A4prtMSG)
001740030515
001750110427       //--------------------------------------------------------------
001760110427       //?Definizione costanti.                                        ?
001770110427       //--------------------------------------------------------------
001780110427
001790110427       // -?Intestazioni in stampa?
001800120530     d*// $Bartolini      c                   const('BARTOLINI S.p.A.')
001810110426     d $BRT            c                   const('BRT S.p.A.')
001820090414     d*// $CorrEspr       c                   const('CORRIERE  ESPRESSO')
001830060503     d $Intest1        c                   const('Sede Operativa ed Am-
001840060503     d                                     ministrativa:')
001850060503     d $Intest2        c                   const('via E. Mattei, 42 -
001860110426     d                                     - 40138 Bologna')
001870110426     d*//                                  - 40138 BOLOGNA')
001880060503     d $Intest3        c                   const('Sede Legale: Piazza -
001890060503     d                                     Diaz, 7 - 20123 Milano')
001900070215     d $Intest4        c                   const('C.F. e P.IVA 04507990150')
001910061206     d $Intest5        c                   const(' Capitale Sociale Eu-
001920061221     d                                     ro 24.000.000 int. vers.')
001930060503     d $Intest6        c                   const('info on-line')
001940120530     d*// $Intest7        c                   const('www.bartolini.it')
001950120530     d $Intest7_brt    c                   const('www.brt.it')
001960090107     d $Intest2_b      c                   const('via Enrico Mattei, +
001970090107     d                                            42 - 40138 BOLOGNA')
001980120530     d*// $Intest3_b      c                   const('Tel. 051 6015411 - +
001990120530     d*//                                            Fax 051 530365 - +
002000120530     d*//                                            www.bartolini.it')
002010120530     d $Intest3_b_brt  c                   const('Tel. 051 6015411 - +
002020120530     d                                            Fax 051 530365 - +
002030120530     d                                            www.brt.it')
002040081222     d $Intest4_b      c                   const('C.F. - P.IVA - +
002050081222     d                                            Reg. Imprese Milano +
002060081222     d                                            n. 0457990150')
002070081222     d $Intest5_b      c                   const('Capitale Sociale € +
002080081222     d                                            24.000.000,00 i.v. +
002090081222     d                                            - REA Milano n. +
002100081222     d                                            1734257')
002110110427
002120110427       // -?DIMENSIONI & POSIZIONI:?
002130110427
002140110427       // -?Lunghezza (in inches) di una singola LdV?
002150080924     d $LenLdV2        c                   const(5.775)
002160080924     d $LenLdV4        c                   const(2.890)
002170110427       // -?Lunghezza di 1/4 di A4?
002180080924     d c_Len1_inch     c                   const(2.925)
002190080923     d c_Len2_inch     c                   const(5.850)
002200080923     d c_Len2_row8     c                   const(047)
002210110427       // -?Spessore delle linee in inches/pollici (*memo)?
002220080701     d c_LineNarrow    c                   const(0.008)
002230080701     d c_LineMedium    c                   const(0.017)
002240080701     d c_LineWide      c                   const(0.025)
002250110427       // -?Dimensioni base per i simil-loghi Bartolini e BRT?
002260120530      *///copy gaitrasrc/srcTNSY,LogoBAR_R0
002270120530      *///copy gaitrasrc/srcTNSY,LogoBRT3R0
002280120530      *///copy gaitrasrc/srcTNSY,LogoBRT_R0
002290120530      /copy gaitrasrc/srcTNSY,LogoBRT_R1
002300110427
002310110427       // -?Posizionamento all'inizio della pag. A4 successiva?
002320110427     d c_§PS1          c                   const(0.950)
002330110427
002340120530       // -?Posizioni dell'intestazione "BRT S.p.A." nella 1ª copia?
002350080704     d c_PosTBar       c                   const(0.270)
002360080704     d c_PosTBa1       c                   const(0.350)
002370110426     d c_PosTBa2       c                   const(0.420)
002380110426     d c_PosTBa3       c                   const(0.490)
002390110426     d c_PosTBa4       c                   const(0.560)
002400110426     d c_PosTBa5       c                   const(0.630)
002410110426     d c_PosTBa6       c                   const(0.710)
002420110426     d c_PosTBa2x      c                   const(0.430)
002430110426     d c_PosTBa3x      c                   const(0.500)
002440110426     d c_PosTBa4x      c                   const(0.590)
002450110426     d c_PosTBa5x      c                   const(0.660)
002460110426     d c_PosTBa6x      c                   const(0.750)
002470110427       // -?Posizioni dei bordi orizzontali superiori box nella 1ª copia?
002480110427       //  ?(distanza dal lato superiore dell'A4)?
002490110427       //  ?N.B.?- c_PosBoxNXs => posizione iniziale della linea?
002500110427       //                        ?orizzontale Superiore?
002510110427       //       ?- c_PosBoxNXi => posizione iniziale della linea?
002520110427       //                        ?orizzontale Inferiore?
002530080704     d c_PosBox1As     c                   const(0.200)
002540080704     d c_PosBox1Bs     c                   const(0.790)
002550080704     d c_PosBox1Bi     c                   const(2.850)
002560080704     d c_PosBox1Cs     c                   const(1.950)
002570100208     d c_PosBox1Ds     c                   const(2.453)
002580110427       // -?Posizioni di altre linee orizzontali nella 1ª copia?
002590080704     d c_PosLin1OS1    c                   const(1.350)
002600080704     d c_PosLin1OS2    c                   const(1.790)
002610080704     d c_PosLin1OS3    c                   const(2.010)
002620080704     d c_PosLin1OS4    c                   const(2.230)
002630091119     d c_PosLin1OS5    c                   const(2.470)
002640080704     d c_PosLin1OC1    c                   const(1.680)
002650080704     d c_PosLin1OC2    c                   const(1.240)
002660140917     d c_PosLin1OC3    c                   const(1.470)
002670080704     d c_PosLin1OD1    c                   const(1.010)
002680140917
002690140917       // -?Posizioni di altre costanti?
002700140917     d*// c_PosKBRTc      c                   const(0.970)     x FONT(00404)
002710140923     d c_PosKBRTc      c                   const(0.965)
002720140923     d*// c_PosKBRTc      c                   const(0.955)     x FONT(02305)
002730080704     d c_PosKTass      c                   const(2.130)
002740080704     d c_PosKRCV       c                   const(1.100)
002750080704     d c_PosKBAV       c                   const(1.850)
002760080709     d c_PosKCXD2      c                   const(3.100)
002770110427
002780110427       // -?Posizione minima superiore per FedEx?
002790030709     d $MinLine1       c                   const(012)
002800110427       // -?Limite righe stampabili su 1/4 di A4 in 1/9"?
002810081002     d $MaxLine1       c                   const(025)
002820110427       // -?Limite righe stampabili su 1/2 A4 in 1/8" per?
002830110427       //  ?Karnak/Titanedi/EDk + Arcte + Esprinet +?
002840110427       //  ?Logistica/Sait + Generico (con/senza logo)?
002850080822     d $MaxLine2       c                   const(068)
002860090323     d $MaxLine2_pl1   c                   const(044)
002870090323     d $MaxLine2_pl2   c                   const(090)
002880110406     d $MaxLine2_pl1k  c                   const(045)
002890110406     d $MaxLine2_pl2k  c                   const(091)
002900110427       // -?Limite righe stampabili su 1/2 A4 in 1/8" per?
002910110427       //  ?Karnak/Titanedi/EDk?
002920080822     d $MaxLine3       c                   const(059)
002930090324     d $MaxLine3_pl1   c                   const(036)
002940090324     d $MaxLine3_pl2   c                   const(082)
002950110427       // -?Limite righe stampabili su 1/2 A4 in 1/8" per?
002960110427       //  ?Logistica/Sait + Arcte?
002970090323     d $MaxLine4       c                   const(065)
002980090324     d $MaxLine4_pl1   c                   const(041)
002990090324     d $MaxLine4_pl2   c                   const(087)
003000110427       // -?Limite righe stampabili su 1/2 A4 in 1/8" per?
003010110427       //  ?Arcte + Esprinet?
003020090324     d $MaxLine4_pl3   c                   const(042)
003030090324     d $MaxLine4_pl4   c                   const(088)
003040090324     d $MaxLine5_pl1   c                   const(045)
003050090324     d $MaxLine5_pl2   c                   const(091)
003060110427       // -?Limite righe stampabili su 1/2 A4 in 1/8" per?
003070110427       //  ?Esprinet?
003080081002     d $MaxLine6       c                   const(057)
003090081002     d $MaxLine6_pl1   c                   const(034)
003100081002     d $MaxLine6_pl2   c                   const(080)
003110110427
003120110427       // -?Limiti di OverFlow?
003130040608     d $BoxRow1        c                   const(010)
003140080707     d $LimOF          c                   const(08.250)
003150080709     d $LimOF1         c                   const(02.900)
003160080709     d $LimOF2         c                   const(05.800)
003170080709     d $LimOF3         c                   const(08.700)
003180030515     d $elSNSxA5       c                   const(04)
003190100208     d c_DateMax_A     c                   const('20391231')
003200110427
003210110427       // -?COSTANTI IN STAMPA?
003220110426     d c_CopiaXdest    c                   const('COPIA PER IL DESTINA-
003230080919     d                                     TARIO')
003240110426     d c_ScNS          c                   const('ELENCO SEGNACOLLI -
003250030515     d                                     NON SEQUENZIALI')
003260110426     d c_CoOr          c                   const('COLLI ORIGINALI     -
003270030717     d                                     N. ')
003280110426     d c_DeSM          c                   const('DESTINATARIO SUPERME-
003290031124     d                                     RCATO')
003300110426     d c_PMxA          c                   const('PROMEMORIA PER L''-
003310080604     d                                     AUTOTRASPORTATORE')
003320110427       // -?ALTRE COSTANTI:?
003330110427       // ·?Codice tab.7V-R x "Destinatario assente: plico in buca"?
003340110502     d*// Ktab7V_CB       c                   const('CB')
003350110427       // ·?Codice tab.7V-R x "Restituzione DDT firmato al mittente"?
003360110502     d*// Ktab7V_C1       c                   const('C1')
003370110427       // ·?Codice tab.7V-R x "Restituzione copie del contratto firmato"?
003380110502     d*// Ktab7V_C2       c                   const('C2')
003390110427       // ·?Codice tab.7V-R x "Restituzione copie del contratto firmato"?
003400110427       //  ?                e "Restituzione copia della C.I. del dest."?
003410110502     d*// Ktab7V_C3       c                   const('C3')
003420110427       // -?Codice tab.7V-R x "Restituzione copia C.I. del destinatario"?
003430110411     d*// Ktab7V_C4       c                   const('C4')
003440110427       // -?Codice tab.7V-R x "Restituzione modulo Vodafone firmato"?
003450110411     d*// Ktab7V_C5       c                   const('C5')
003460110427       // -?Codice tab.7V-R x "Non effettuare ritiro di merce"?
003470110502     d*// Ktab7V_NR       c                   const('NR')
003480110427       // -?Codice tab.7V-R x "Ritiro contestuale alla consegna"?
003490110502     d*// Ktab7V_RC       c                   const('RC')
003500110427       // -?Codice tab.7V-R x C1+RC?
003510110502     d*// Ktab7V_R1       c                   const('R1')
003520110427
003530130418
003540110427       // -?Codice cliente "FedEx"?
003550081127     d c_CliFedEx      c                   const(0501331)
003560150504     d c_CliFedEx2     c                   const(0506514)
003570110427       // -?Codice cliente "PraOil oleodotti italiani"?
003580081127     d c_CliPraOil     c                   const(0201515)
003590110427       // -?Codice cliente "TTL"?
003600091116     d c_CliTTL        c                   const(0504450)
003610110427       // -?Codice cliente "Consorzio Lotterie Nazionali - Gratta & Vinci"?
003620090527     d c_CliCLN        c                   const(0632910)
003630140513     d c_CliLN_2       c                   const(0017093)
003640110427       // -?Codice cliente "S G Soluzioni Logistiche"?
003650090611     d c_CliSG         c                   const(0634923)
003660091117     d c_CliSG_2       c                   const(0635042)
003670130418
003680091117
003690110427       // -?Costante per evitare la stampa del contratto x "S.G."?
003700091117     d c_NoContr       c                   const('NO CONTRATTO')
003710130306
003720130306       // -?Costante per identificare la presenza di PDF del cliente?
003730130308       //  ?in FIAR800F.AR8TMD (nel suo 1° byte)?
003740130308     d c_§PDF          c                   const('§')
003750110427
003760110427       // -?Costante per controllo "caratteri solo numerici"?
003770110426     d c_Digits        c                   const('1234567890')
003780140307
003790140307       // -?Costanti per conversione caratteri maiuscoli/minuscoli?
003800140307     d c_Mai           c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
003810140307     d c_Min           c                   const('abcdefghijklmnopqrstuvwxyz')
003820110427
003830110427       // -?Comando di override al PrtF?
003840100208     d c_CmdDltOvr     c                   const('DLTOVR +
003850130710     d                                            file(FNLSB6P) +
003860080708     d                                            lvl(*job)')
003870110427
003880110427       //--------------------------------------------------------------
003890110427       //?Definizione schiere.                                         ?
003900110427       //--------------------------------------------------------------
003910110427
003920110427       // -?Messaggi da stampare?
003930130312     d $Msg            s            100    dim(7)   ctdata  perrcd(1)
003940110427       // -?Giorni della settimana e relativa decodifica?
003950070306     d $GgS            s              1    dim(9)   ctdata  perrcd(1)
003960070306     d $Gio            s             15    dim(9)   alt($GgS)
003970110427       // -?Varie esenti IVA?
003980070319     d $Var            s              1    dim(10)
003990110427       // -?Note e Particolarità?
004000070306     d $NP             s                   dim(6)   inz  like(Pnp1)
004010110427       // -?Segnacolli NON sequenziali (memo-1- e stampa-2-)?
004020070306     d $SC1            s              7  0 dim(14)  inz
004030070306     d $SC2            s              8    dim(14)  inz
004040110427       // -?Tab. "2A" (Causali Eventi): causali con tipo consegna = "G"?
004050070308     d $2Acev_G        s                   dim(999) inz like(EVBcev)
004060110427       // -?Quantità (editate) - a sx - e Unità di misura - a dx -?
004070070306     d $QUM            s             20    dim(3)   inz
004080110427       // -?Quantità (editate) - a sx - e Unità di misura - a dx -?
004090070306     d $AR8o1          s                   dim(10)  inz  like(§AR8o1a)
004100110427       // -?Consegne Parziali Precedenti?
004110070319     d $CP             s                   dim(20)  inz(*all'9')
004120070319     d                                                   like(Elem_CP)
004130070319     d $CPdcm          s                   dim(20)  inz  like(ARTdcm)
004140110427
004150110427       //--------------------------------------------------------------
004160110427       //?Definizione strutture dati.                                  ?
004170110427       //--------------------------------------------------------------
004180110427
004190110427       // -?Parametri ricevuti?
004200030515     d KPJBA         e ds
004210110427       // ·?Dati LDV da stampare?
004220080703     d FNLSB5ds      e ds
004230130306     d FNLSB6ds1     e ds
004240110427       // -?Flag operativi DB0FLO di FNLSB5DS?
004250080606     d dDB0flo       e ds                  inz
004260110427
004270110427       // -?Parametri x Controllo profilo utenti?
004280040223     d TIBS34ds      e ds
004290110427       // -?Ds di riferimento al file esterno AZUTE00F?
004300040223     d AZUTEds       e ds                  extname(AZUTE00F)
004310110427       // -?Ds per dati organigramma?
004320040223     d dDATIUTE      e ds
004330110427
004340110427       // -?Conversioni in Euro?
004350061107     d YEURCOds      e ds                  inz
004360110427
004370110427       // -?Dati da AZORG00F?
004380070424     d OG143ccm      e ds                  inz     extname(OG143)
004390070424     d                                             prefix(§CM:3)
004400030515     d OG143         e ds                  inz
004410070424     d OG146arr      e ds                  inz     extname(OG146)
004420070424     d                                             prefix(A)
004430070424     d OG146cf£6     e ds                  inz     extname(OG146)
004440070424     d                                             prefix(£6:1)
004450030515     d OG148         e ds                  inz
004460070424     d OG148arr      e ds                  inz     extname(OG148)
004470070424     d                                             prefix(A)
004480070424     d OG148cf£6     e ds                  inz     extname(OG148)
004490070424     d                                             prefix(£6:1)
004500070424     d OG150arr      e ds                  inz     extname(OG150)
004510070424     d                                             prefix(A)
004520110427
004530110427       // -?Dati da FIAR400F?
004540061108     d dsBL4A        e ds                  inz
004550171117     d*// dsBL4I        e ds                  inz
004560071015     d dsBL4M        e ds                  inz
004570061108     d dTA4Q         e ds                  inz
004580110427
004590110427       // -?Flag operativi di FNORM - campo ORMFLO?
004600110427     d dORM01        e ds                  inz
004610140411
004620140411       // -?Dati per trc "O " del file FNORE (orari apertura)?
004630140411     d dOREorari     e ds
004640110427
004650110427       // -?Tabelle?
004660061107     d ds$2          e ds
004670061107     d ds1A          e ds                  inz
004680061107     d ds1P          e ds                  inz
004690070306     d ds2A          e ds                  inz
004700061107     d ds3A          e ds                  inz
004710061107     d ds5E          e ds                  inz
004720061107     d ds7R          e ds                  inz
004730061107     d dsCC          e ds
004740061107     d dsCV          e ds                  inz
004750061107     d dsEI          e ds                  inz
004760061107     d dsTB          e ds                  inz
004770030805     d dGEI          e ds
004780030805     d dGED          e ds
004790150304     d*// dTBA          e ds                  inz
004800071112     d dVPO          e ds                  inz
004810120530     d*// dVPObrt       e ds                  inz
004820061031     d dVPOdeco      e ds                  inz
004830031124     d dAR5gen       e ds                  inz
004840150304     d*// dAR5ban       e ds                  inz
004850061030     d dAR5bnb       e ds                  inz
004860030903     d dAR5rcc       e ds                  inz
004870040223     d dAR5scr       e ds                  inz     prefix(§AR5S:4)
004880041207     d dAR8          e ds                  inz
004890041207     d  §AR8tsd      e                     inz('S1')
004900040607     d dAR8T1        e ds                  inz
004910040607     d dAR8T2        e ds                  inz
004920040607     d dAR8C1        e ds                  inz
004930040607     d dAR8C2        e ds                  inz
004940040607     d dAR8C3        e ds                  inz
004950040607     d dAR8V1        e ds                  inz
004960040607     d dAR8V2        e ds                  inz
004970040607     d dAR8L1        e ds                  inz
004980040607     d dAR8O1        e ds                  inz
004990040607     d dAR8O2        e ds                  inz
005000041026     d dAR8LOG1T1    e ds                  inz
005010041026     d dAR8LOG1T2    e ds                  inz
005020041026     d dAR8LOG1T3    e ds                  inz
005030041026     d dAR8LOG1C1    e ds                  inz
005040041026     d dAR8LOG1C2    e ds                  inz
005050041026     d dAR8LOG1C3    e ds                  inz
005060060614     d dAR8LOG1C4    e ds                  inz
005070041026     d dAR8LOG1P1    e ds                  inz
005080050318     d dAR8LOG1Z1    e ds                  inz
005090050606     d dAR8LOG1S1    e ds                  inz
005100050606     d dAR8LOG1S2    e ds                  inz
005110050606     d dAR8LOG1S3    e ds                  inz
005120050606     d dAR8LOG1S4    e ds                  inz
005130050606     d dAR8LOG1S5    e ds                  inz
005140050113     d dAR8ARCTT1    e ds                  inz
005150050114     d dAR8ARCTT2    e ds                  inz
005160050113     d dAR8ARCTC1    e ds                  inz
005170050113     d dAR8ARCTC2    e ds                  inz
005180050113     d dAR8ARCTP1    e ds                  inz
005190050214     d dAR8ESP1T1    e ds                  inz
005200050214     d dAR8ESP1T2    e ds                  inz
005210050214     d dAR8ESP1C1    e ds                  inz
005220050214     d dAR8ESP1C2    e ds                  inz
005230050214     d dAR8ESP1P1    e ds                  inz
005240050214     d dAR8ESP1P2    e ds                  inz
005250130306     d dAR8§pdf      e ds                  inz
005260170605     d dCLI          e ds                  inz
005270110427     d TIBS02ds      e ds                  inz
005280110427     d   T02mod      e                     inz('C')
005290110427
005300110427       // -?Parametri per TRULINTR (determinaz. intestatario c/assegno)?
005310100208     d TRULINTds     e ds                  inz
005320100208     d   iINTtip     e                     inz('I')
005330110427       // -?Parametri per TRUL21R (calcolo limiti importo c/assegno)?
005340030515     d TRUL21ds      e ds                  inz
005350030515     d  I21tla       e                     inz('L')
005360110427       // -?Parametri per TRUL28R (calcolo check digit del barcode)?
005370030515     d TRUL28ds      e ds                  inz
005380030515     d  I28tla       e                     inz('L')
005390041019     d  I28mod       e                     inz('BAR')
005400110427       // -?Parametri per FNLV55R (reperimento terminal arr/par/£6)?
005410070212     d FNLV55ds      e ds                  inz
005420070212     d  D55tpt       e                     inz('6')
005430110427       // -?Parametri per FNLV80R (decodifica consegna richiesta)?
005440051129     d FNLV80ds      e ds                  inz
005450051129     d  ILV80tla     e                     inz('C')
005460110427       // -?Parametri per FIDN12R (ricerca C.A.')?
005470070319     d FIDN12ds      e ds                  inz
005480070319     d  I12fel       e                     inz('E')
005490070319     d  I12fan       e                     inz('E')
005500070319     d  I12fch       e                     inz('E')
005510110427       // -?Parametri per TNSD99R (calcolo tempi di delivery)?
005520090605     d TNSD99ds      e ds                  inz
005530090605     d  D98tla       e                     inz('C')
005540110427
005550110427       // -?Status ds?
005560030515     d Status         sds
005570130306     d   SDSpgm          *proc
005580130306     d*//SDSprm          *parms
005590130306     d   SDSusr              254    263
005600110427
005610110427       // -?InfDS?
005620030515     d PrtINFOa4       ds
005630080627     d  CurLineA4            367    368i 0
005640110427
005650110427       // -?Codice a Barre della LDV?
005660030515     d BARcodeDS       ds            17    inz
005670030515     d  BARcdeAAS                     2s 0 inz
005680030515     d  BARcdeLNP                     3s 0 inz
005690030515     d  BARcdeNRS                     2s 0 inz
005700030515     d  BARcdeNSP                     7s 0 inz
005710110427       // -?Codice a Barre dell'ORM?
005720040629     d BARcodeORM      ds            14    inz
005730040629     d  ORMpoe                             inz
005740040629     d  ORMnsr                             inz
005750040629     d  ORMnor                             inz
005760040629     d  ORMnrv                             inz
005770110427
005780110427       // -?Chiave di accesso ad eventuali Note?
005790030515     d KEYpar          ds             8    inz
005800030515     d  WTPpar                 1      1    inz
005810030515     d  WCDpar                 2      3    inz
005820030515     d  Wprogr                 4      8    inz('S0001')
005830110427
005840110427       // -?Dati delle eventuali consegne parziali precedenti?
005850140307     d Elem_CP         ds                  inz
005860140307     d   WCPdcm                       8s 0 inz
005870140307     d   WCPocm                       4s 0 inz
005880140307     d   WCPncl                       5s 0 inz
005890140307     d   WCPfir                            inz  like(EVB2dati)
005900130306
005910130306       // -?%TimeStamp?
005920130306     d wTime_ds        ds                  qualified     inz
005930130306     d   wDate_Iso                     d   datfmt(*iso)  inz
005940130306     d   wFiller                      1a                 inz
005950130307     d   wTime                         t   timfmt(*iso)  inz
005960110427
005970110427       //--------------------------------------------------------------
005980110427       //?Definizione variabili globali.                               ?
005990110427       //--------------------------------------------------------------
006000110427
006010110427       // -?Flags booleani?
006020110427     d $Mamma          s               n   inz
006030130215     d $Mamma_FedEx    s               n   inz
006040110427     d $FirstSCR       s               n   inz(*on)
006050110427     d $FIAR5          s               n   inz
006060110427     d $FIAR8          s               n   inz
006070110427     d $FIARN          s               n   inz
006080110427     d $E_AR8          s               n   inz(*on)
006090130306     d $§PDF           s               n   inz
006100110427     d $DDT_o          s               n   inz
006110110427     d $DDT_p          s               n   inz
006120110427     d $DDTlastPg      s               n   inz
006130110427     d $Partic         s               n   inz
006140110427     d $NoDesPartic    s               n   inz
006150110427     d $BolloVirt      s               n   inz
006160110427     d $No_CF          s               n   inz
006170110427     d $No_PI          s               n   inz
006180120113     d $No_x_Cons      s               n   inz
006190110427     d $Rit_a_Inc      s               n   inz
006200110427     d $A4_x_ScNS      s               n   inz
006210110427     d $LascAvv        s               n   inz
006220110427     d $ConsParz       s               n   inz
006230110427     d $C_A_           s               n   inz
006240110427     d $CAeur          s               n   inz
006250110427     d $GeoAtt         s               n   inz
006260090302     d $TabPXA         s               n   inz
006270110411     d $TabPXA2        s               n   inz
006280090924     d $BoxNxD         s               n   inz
006290100513     d $PreavvORM      s               n   inz
006300130215       // -?Altri flags?
006310130215     d $First          s              1    inz(*on)
006320110427
006330110427       // -?Indici di schiera?
006340110427     d XX              s              3  0 inz
006350110427     d YY              s              3  0 inz
006360110427     d WX              s              3  0 inz
006370110427
006380110427       // -?Campi di comodo alfanumerici?
006390040223     d SAVin7x         s              4    inz(*zeros)
006400030717     d w_CoOr          s             35    inz
006410030515     d wAWB            s             12    inz
006420030515     d w001a           s              1    inz
006430030515     d w008a           s              8    inz
006440030515     d w012a           s             12    inz
006450030515     d w015a           s             15    inz
006460100513     d w053a           s             53    inz
006470070424     d wDateA          s              8    inz
006480070717     d wGeoTel         s              1    inz
006490070717     d wGeoCon         s              1    inz
006500110427       // -?Campi di comodo numerici?
006510030515     d w0040           s              4  0 inz
006520030515     d w0080           s              8  0 inz
006530030624     d w0112           s             11  2 inz
006540030515     d w0140           s             14  0 inz
006550071105     d wDate           s              8  0 inz
006560030515     d wCOPIE          s              3  0 inz
006570140307     d wEVBdev         s                   like(EVBdev)   inz
006580140307     d wEVBhev         s                   like(EVBhev)   inz
006590110427
006600110427       // -?Campi utilizzati nella gestione dell'impaginazione A4?
006610030515     d wMinLine        s              3  0 inz
006620030515     d wMaxLine        s              3  0 inz
006630080604     d wPage           s              4  0 inz(0001)
006640030515     d WelSNS          s              2  0 inz
006650040608     d wOmaRowT        s              3  0 inz
006660040608     d wOmaRowD        s              3  0 inz
006670110427       // -?Variabili per il calcolo delle posizioni per box in A4?
006680080923     d wNumLdV         s              5  0 inz
006690080923     d wSeqLdV         s              1  0 inz
006700080923     d wPosIni         s              5  3 inz
006710110427
006720110427       // -?Lunghezza (in inches) di una singola LdV?
006730080703     d $Len1LdV        s              5  3 inz(5.775)
006740110427       // -?Numero delle LdV (box) per pagina "A4"?
006750110427       //  ?(dovendo stampare sia la copia per il destinatario quanto?
006760110427       //   ?quella per il vettore, ci stanno 2 LdV in 1 A4;?
006770110427       //   ?stampando solo le copie per il vettore ce ne stanno 4!)?
006780080703     d $LdVxP          s              1  0 inz(2)
006790110427
006800110427       // -?Campi utilizzati per impostazione dell'output in stampa?
006810140923     d Pdsp            s              8  0 inz
006820130418     d Tla             s             50    inz
006830130418      *
006840050624     d wBolloVirt      s              4  2 inz
006850030515     d Wcas            s             13  3 inz
006860030515     d Wiva            s             11  2 inz
006870071015     d Wpdc4A          s              4    inz
006880030515     d Data_EUR        s               d   datfmt(*eur)   inz(*loval)
006890101201     d Data_DMY        s               d   datfmt(*dmy)   inz(*loval)
006900170717       // -?Campi per il salvataggio dei dati anagrafici della filiale?
006910170717       //  ?capofile £6?
006920170717     d wDB0lnaCf£6     s                   like(DB0lna)
006930170717     d wDB0dlaCf£6     s                   like(DB0dla)
006940170717     d wDB0inlCf£6     s                   like(DB0inl)
006950170717     d wDB0calCf£6     s                   like(DB0cal)
006960170717     d wDB0lolCf£6     s                   like(DB0lol)
006970170717     d wDB0prlCf£6     s                   like(DB0prl)
006980170717     d wDB0telCf£6     s                   like(DB0tel)
006990170717     d wDB0faxCf£6     s                   like(DB0fax)
007000110427       // -?Campi utilizzati per il salvataggio momentaneo di dati?
007010040608     d SavLineA4       s                   like(CurLineA4)
007020040311     d SAVlnp          s                   like(DB0lnp)   inz(*loval)
007030040311     d SAVlna          s                   like(DB0lna)   inz(*loval)
007040030515     d SAVtrd          s                   like(AR5trd)   inz(*hival)
007050030515     d SAVdac          s                   like(AR5dac)   inz(*hival)
007060030515     d SAVorc          s                   like(AR5orc)   inz(*hival)
007070040607     d SAVprr          s                   like(AR8prr)   inz
007080041207     d SAVtrk          s                   like(AR8trk)   inz
007090070314     d wRSxCF          s                   like(DB0rsm)   inz
007100071114     d wB0cfi          s                   like(DB0cpi)   inz
007110071114     d wB0piv          s                   like(DB0cpi)   inz
007120031015     d wB0nrs          s                   like(ARBnrs)   inz
007130070514     d wB0ndc          s                   like(ARBndc)   inz
007140070514     d wB0ntc          s                   like(ARBntc)   inz
007150070514     d wB0dcp          s                   like(ARBdcp)   inz
007160081119     d wB0tmc          s                   like(ARBtmc)   inz
007170101125     d wB0dcm          s                   like(ARBdcm)   inz
007180101125     d wB0icc          s                   like(ARBicc)   inz
007190101125     d wB0ica          s                   like(ARBica)   inz
007200130124     d wB0fiv          s                   like(AR6fiv)   inz
007210060505     d Wnsc            s                   like(ARSnsc)   inz
007220060505     d Wtrc            s                   like(ARStrc)   inz
007230030515     d Wctr            s                   like(DB0ctr)   inz
007240031124     d Wdsp            s                   like(DB0dft)   inz
007250090527     d WddtCLN         s                   like(DB0drt)   inz
007260090527     d WdspCLN         s                   like(DB0dft)   inz
007270030515     d Wsvx            s                   like(AR6sv1)   inz
007280030515     d Wvax            s                   like(AR6va1)   inz
007290150304     d*// Wtba            s                   like(§AR5tba)  inz
007300080708
007310110427       // -?Comando da eseguire per QCMDEXC?
007320080708     d Qcmd            s            512    inz
007330080708
007340090302       //--------------------------------------------------------------
007350130306       //?Definizione prototipi procedure usate.                       ?
007360090302       //--------------------------------------------------------------
007370140917
007380140917       // -?Calcolo/Controllo BRTcode?
007390140917      /copy gaitrasrc/srcProtoPI,TRUL28R0
007400140917      /copy gaitrasrc/srcProtoPR,TRUL28R0
007410080708
007420100208       // -?Calcolo tempi di delivery?
007430090605      /copy gaitrasrc/srcProtoPR,TNSD99R
007440100208
007450100208       // -?Determinazione intestatario contrassegno?
007460100208      /copy gaitrasrc/srcProtoPR,TRULINTR
007470130306
007480130306       // -?Reperimento/Aggiornamento numeratori in GRU?
007490130306     d TRUL33ds      e ds                  inz
007500130306      /copy gaitrasrc/srcProtoPR,TRUL33R
007510090605
007520100208       // -?Override al PrtF?
007530080709     d fnlsB8r         pr                  extpgm('FNLSB8R')
007540080709     d  fnlsB5ds                           likeds(FNLSB5ds)
007550130306
007560130306       // -?Reperimento override apportate al PrtF?
007570130306     d wPrtF           s             10
007580130307     d trulPOVRds    e ds                  qualified
007590130306     d trulPOVRc       pr                  extpgm('TRULPOVRC')
007600130306     d   wPrtF                       10
007610130306     d   trulPOVRds                        likeds(trulPOVRds)
007620130306
007630130306       // -?Stampa tutte le immagini PDF abbinate alle LdV già stampate?
007640130312      /copy gaitrasrc/srcProtoPI,PDF00STAR
007650130312      /copy gaitrasrc/srcProtoPR,PDF00STAR
007660080708
007670100208       // -?Esecuzione comando di sistema?
007680080708     d qCmdExc         pr                  extpgm('QCMDEXC')
007690080708     d  Qcmd                        512    const  options(*varsize)
007700080708     d  Qlen                         15  5 const
007710090302
007720090302       //--------------------------------------------------------------
007730090302       //?Definizione key-list.                                        ?
007740090302       //--------------------------------------------------------------
007750090302
007760110427       // -?File TNTBE01L?
007770090302     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
007780090302     d                                     prefix(k_)   inz
007790130306
007800130306       // -?File FIAR801L?
007810130306     d k06fiar801    e ds                  extname(FIAR801L : *key)
007820130306     d                                     prefix(k_)   inz
007830140307
007840140307       // -?File FNEVB22L?
007850140307     d keyFNEVB22    e ds                  extname(FNEVB22L : *key)
007860140307     d                                     prefix(k_)   inz
007870140411
007880140411       // -?File FNORE01L?
007890140411     d keyFNORE01    e ds                  extname(FNORE01L : *key)
007900140411     d                                     prefix(k_)
007910140411     d                                     inz
007920170605
007930170605       // -?File TFNTC01L?
007940170605     d keyTFNTC01    e ds                  extname(TFNTC01L : *key)
007950170605     d                                     prefix(k_)
007960170605     d                                     inz
007970030515
007980030619      *===============================================================*
007990050217      *?RIEPILOGO INDICATORI                                         ?*
008000030515      *---------------------------------------------------------------*
008010050216      *?18   ?- Stampa DATA e ORA CONSEGNA RICHIESTA                  *
008020050216      *?19   ?- Stampa IMPONIBILE, IVA, TOTALE ed ESTREMI FATTURA     *
008030050216      *?20   ?- Stampa CONTRASSEGNO                                   *
008040050216      *?21   ?- Stampa TOTALE DA ESIGERE ALLA CONSEGNA / AL RITIRO    *
008050051129      *?22   ?- Stampa NOTE (se NO imponibile e fattura)              *
008060050216      *?31   ?- di comodo                                             *
008070130418      *?33off?- Stampa della COPIA PER IL CORRIERE                    *
008080130418      *?33on ?- Stampa della COPIA PER IL DESTINATARIO                *
008090130418      *         (o per il Mittente, se P.F. Prepagato)                *
008100050216      *?35   ?- Richiesta FIRMA, DATA e ORA   in 1ª copia             *
008110050217      *         per ricevuta della merce senza riserve                *
008120050217      *         e indica "TOTALE DA ESIGERE ALLA CONSEGNA"            *
008130050216      *?36   ?- Richiesta FIRMA per QUIETANZA in 2ª copia             *
008140050217      *         e indica "TOTALE DA ESIGERE AL RITIRO"                *
008150050216      *?37   ?- Condiziona HIGHLIGHT nel campo PDS2  in rec. PRTLDV1  *
008160050217      *                                  per servizio "ESPRESSO"      *
008170050217      *                                o per servizio "POSTE IT."     *
008180050216      *?38   ?- Condiziona FONT      nel campo PAR5  in rec. PRTAR52  *
008190050216      *?39   ?- Condiziona UNDERLINE nel campo PRIG  in rec. PRTGEN   *
008200050217      *                            e nel campo PCTXX in rec. PRTDK5   *
008210050217      *                            e nel campo PCTXT in rec. PRTDTx   *
008220050217      *                            e nel campo PCTXT in rec. PRTDDx   *
008230080604      *?41   ?- Stampa copia per il corriere della 1ª LdV             *
008240080604      *?42   ?- Stampa copia per il destinatario della 1ª LdV         *
008250080604      *?43   ?- Stampa copia per il corriere della 2ª LdV             *
008260080604      *?44   ?- Stampa copia per il destinatario della 2ª LdV         *
008270080918      *?45off?- Posiziona l'inizio della packing-list al 2/4 dell'A4  *
008280080923      *         (packing-list di 2 LdV per A4)                        *
008290080918      *?45on ?- Posiziona l'inizio della packing-list in cima all'A4  *
008300080923      *         (1ª packing-list in A4 dopo la stampa di 4 LdV x A4)  *
008310080923      *?46on ?- Posiziona l'inizio della packing-list a metà dell'A4  *
008320080923      *         (2ª packing-list in A4 dopo la stampa di 4 LdV x A4)  *
008330120530      *?50   ?- Stampa CODICE A BARRE - BRT                           *
008340050217      *?51   ?- Stampa CODICE A BARRE - DPD                           *
008350050217      *?52   ?- Stampa CODICE A BARRE - O.R.M.                        *
008360120530      *?55   ?- NON stampa logo BRT in DDT della Logistica =>         *
008370120530      *         => Stampa il simil-logo in LSB6LOGBRT                 *
008380050217      *?69off?- Stampa su modulo in formato "A4"                      *
008390050217      *?69on ?- Stampa su modulo in formato "A5"                      *
008400120530      *?70   ?- Trattasi di BOLLA BRT                                 *
008410050216      *?71   ?- Trattasi di BOLLA DPD                                 *
008420050216      *?72   ?- Trattasi di BOLLA FedEx                               *
008430080912      *?73   ?- Trattasi di BOLLA EuroExpress                         *
008440030619      *===============================================================*
008450030619      * Descrizione dei campi utilizzati per il posizionamento di     *
008460030619      *   linee orizzontali, linee verticali o campi di stampa        *
008470030619      * (utilizzati solo nel file di stampa per il formato "A4")      *
008480030619      * servono ad impostare la distanza di tali oggetti dal lato     *
008490030619      * superiore del foglio.                                         *
008500030619      *---------------------------------------------------------------*
008510080604      *? BOX nella copia per il vettore:                             ?*
008520080604      *§BOX1AS = posiz. linea orizzontale inizio box mittente (1)   ?*
008530080604      *§BOX1AI = posiz. linea orizzontale fine   box mittente (1)   ?*
008540080604      *§BOX1BS = posiz. linea orizzontale inizio box generale (2)   ?*
008550080604      *§BOX1BI = posiz. linea orizzontale fine   box generale (2)   ?*
008560140922      *§BOX1BJ = posiz. linea orizzontale fine   box BRTcode        ?*
008570080604      *§BOX1CS = posiz. linea orizzontale inizio box contrassegno   ?*
008580080604      *§BOX1CI = posiz. linea orizzontale fine   box contrassegno   ?*
008590080604      *§BOX1DS = posiz. linea orizzontale inizio box tot.da esigere ?*
008600080604      *§BOX1DI = posiz. linea orizzontale fine   box tot.da esigere ?*
008610080604      *§LO1S1  = posiz. linea orizzontale separatrice               ?*
008620080604      *           Destinatario / Colli, Peso, Volume...               *
008630080604      *§LO1S2  = posiz. linea orizzontale separatrice               ?*
008640080922      *           Colli, Peso, Volume / Zona                          *
008650080922      *§LO1S3  = posiz. linea orizzontale separatrice               ?*
008660080922      *           Zona / Segnacolli                                   *
008670080922      *§LO1S4  = posiz. linea orizzontale separatrice               ?*
008680080922      *           Segnacolli / Turno di chiusura                      *
008690080922      *§LO1S5  = posiz. linea orizzontale separatrice               ?*
008700080922      *           Turno di chiusura / "PER RICEVUTA DELLA MERCE..."   *
008710080604      *§LO1C1  = posiz. linea orizzontale separatrice               ?*
008720080624      *           Servizio / Consegne Particolari                     *
008730080604      *§LO1D1  = posiz. linea orizzontale separatrice               ?*
008740080604      *           Riferimenti Mittente / Note e Particolarità         *
008750080605      *§LLV1S1 = lunghezza linea verticale separatrice              ?*
008760080605      *           Destinatario / Riferimento mittente                 *
008770080605      *§LLV1S2 = lunghezza linea verticale separatrice              ?*
008780080605      *           Riferimento mittente / Spedizione                   *
008790120530      *§PC0 <=> §PC6 =  posizioni costanti intestazione "BRT"       ?*
008800120530      *           "BRT SpA", "Sede Operativa ed Amministrativa"       *
008810140917      *§PK1    = posiz. costante "BRTcode" + codice                 ?*
008820080604      *§PK2    = posiz. costante "TASSATIVA"                        ?*
008830080605      *§PK3    = posiz. costanti "(nome e cognome in stampatello)"  ?*
008840080605      *           (in "PER RICEVUTA DELLA MERCE ...")                 *
008850121017      *§PK4    = posiz. costante "COPIA PER IL DESTINATARIO"        ?*
008860121017      *§PK5    = posiz. riferimento alla legge in vigore x RCV      ?*
008870121017      *           (tra n° Spedizione e Imponibile...)                 *
008880121017      *§PK6    = posiz. costante "Bollo assolto in modo virtuale..."?*
008890121017      *§PK7    = posiz. Partita IVA o Cod. Fiscale del cliente      ?*
008900080604      *                                                               *
008910080604      *? Altre posizioni:                                            ?*
008920050325      *§PB1 = posizione linea orizzontale inizio box                ?*
008930050325      *§PB2 = posizione linea orizzontale fine   box                ?*
008940050216      *§LO1 = posizione x 1ª linea orizzontale                      ?*
008950080604      *        LSB6FED1 - richiesta nome e firma per FedEx            *
008960080604      *        LSB6FED3 - limite superiore del box per FedEx          *
008970080604      *        LSB6AR53 - limite superiore dei box da  FIAR5          *
008980030619      *                   (bancali da rendere, supermercato, promem.) *
008990080604      *        LSB6DK0  - linea separatrice nel 1° box DDT Karnak     *
009000080604      *        LSB6DL0A - linea separatrice nel box DDT Logistica     *
009010080604      *        LSB6DL0C - linea separatrice nel box DDT Logistica     *
009020050325      *                   (area Packing List)                         *
009030080604      *        LSB6DL0E - linea separatrice nel box DDT Logistica     *
009040050606      *                   (area Fattura)                              *
009050080604      *        LSB6AC0A - linea separatrice nel box DDT Arcte         *
009060080604      *        LSB6ES0A - linea separatrice nel box DDT Esprinet      *
009070050216      *§LO2 = posizione x 2ª linea orizzontale                      ?*
009080080604      *        LSB6FED1 - linea orizzontale separatrice per FedEx     *
009090080604      *        LSB6FED3 - limite inferiore del box per FedEx          *
009100080604      *        LSB6AR53 - limite inferiore dei box da  FIAR5          *
009110030619      *                   (bancali da rendere, supermercato, promem.) *
009120080604      *        LSB6DK0  - linea separatrice nel 2° box DDT Karnak     *
009130080604      *        LSB6DL0A - linea separatrice nel box DDT Logistica     *
009140080604      *        LSB6DL0C - linea separatrice nel box DDT Logistica     *
009150050325      *                   (area Packing List)                         *
009160080604      *        LSB6DL0E - linea separatrice nel box DDT Logistica     *
009170050610      *                   (area Fattura)                              *
009180080604      *        LSB6AC0A - linea separatrice nel box DDT Arcte         *
009190080604      *        LSB6ES0A - linea separatrice nel box DDT Esprinet      *
009200050216      *§LO3 <=> §LO9/A = posizioni x linee orizzontali              ?*
009210080604      *        LSB6DK0  - linee separatrici nel 2° box DDT Karnak     *
009220080604      *        LSB6DL0B - linee separatrici (orizzontali e verticali) *
009230050218      *                   nella parte finale del box DDT Logistica    *
009240080604      *        LSB6DL0F - linea separatrice nel box DDT Logistica     *
009250050610      *                   (area Fattura nella parte finale del box)   *
009260080604      *        LSB6AC0B - linee separatrici (orizzontali e verticali) *
009270050218      *                   nella parte finale del box DDT Arcte        *
009280080604      *        LSB6ES0B - linee separatrici (orizzontali e verticali) *
009290050216      *                   nella parte finale del box DDT Esprinet     *
009300050216      *§LV1 = posizione x 1ª linea verticale                        ?*
009310080604      *        LSB6FED1 - lineee verticali "lunghe" (0.15 inch) su    *
009320030619      *                   quella orizzontale per FedEx                *
009330080604      *        LSB6FED3 - lineee verticali "lunghe" (0.25 inch) nel   *
009340030619      *                   box per FedEx (separatori)                  *
009350050216      * "   = lunghezza delle linee verticali                       ?*
009360080604      *        LSB6DK0  - linee separatrici nella 1ª parte alta del   *
009370040607      *                   2° box per DDT Karnak                       *
009380080604      *        LSB6DL0A - linee separatrici nella 1ª parte alta del   *
009390050218      *                   box per DDT Logistica                       *
009400080604      *        LSB6DL0C - linee separatrici nella parte alta del box  *
009410050325      *                   DDT Logistica, area Packing List            *
009420080604      *        LSB6DL0E - linea separatrice nel box DDT Logistica     *
009430050610      *                   (area Fattura)                              *
009440080604      *        LSB6AC0A - linee separatrici nella 1ª parte alta del   *
009450050218      *                   box per DDT Arcte                           *
009460080604      *        LSB6AR54 - linea sx "simil" box AR54                   *
009470071025      *                   (v. "Ritiro contestuale alla consegna" o    *
009480071025      *                       "Ritiro porto franco prepagato")        *
009490050216      *§LV2 = posizione x 2ª linea verticale                        ?*
009500080604      *        LSB6FED1 - lineee verticali "corte" (0.07 inch) su     *
009510030619      *                   quella orizzontale per FedEx                *
009520080604      *        LSB6FED3 - lineee verticali "corte" (0.10 inch) nel    *
009530030619      *                   box per FedEx (separatori)                  *
009540050216      * "   = lunghezza delle linee verticali                       ?*
009550080604      *        LSB6DK0  - linee separatrici nella 2ª parte alta del   *
009560040607      *                   2° box per DDT Karnak                       *
009570080604      *        LSB6DL0A - linee separatrici nella 2ª parte alta del   *
009580050218      *                   box per DDT Logistica                       *
009590080604      *        LSB6DL0C - linee separatrici nella parte alta del box  *
009600050325      *                   DDT Logistica, area Packing List            *
009610080604      *        LSB6DL0F - linea separatrice nel box DDT Logistica     *
009620050610      *                   (area Fattura - corpo fattura con totali)   *
009630080604      *        LSB6AC0A - linee separatrici nella 2ª parte alta del   *
009640050218      *                   box per DDT Arcte                           *
009650080604      *        LSB6ES0A - linea separatrice nel 1° box DDT Esprinet   *
009660050216      *§LV3 <=> §LV7   = posizioni x linee verticali                ?*
009670080604      *        LSB6DK0  - linee separatrici nel 2° box DDT Karnak     *
009680080604      *        LSB6DL0A - linee separatrici nel box DDT Logistica     *
009690080604      *        LSB6DL0C - linee separatrici nel box DDT Logistica     *
009700050325      *                   (area Packing List)                         *
009710080604      *        LSB6AC0A - linee separatrici nel box DDT Arcte         *
009720050216      *§PS1 = posizione per campi                                   ?*
009730080604      *        LSB6FED3 - posizione dei campi nel box per FedEx:      *
009740030619      *                   · Package Tracking Number                   *
009750030619      *                   · Status                                    *
009760080604      *        LSB6AR51 - posizione dell'unico campo nel box          *
009770080604      *        LSB6AR52 - posizione dell'unico campo nel box          *
009780080604      *        LSB6AR5A - posizione dell'unico campo nel box          *
009790080604      *        LSB6AR5B - posizione dell'unico campo nel box          *
009800080604      *        LSB6AR5C - posizione dell'unico campo nel box          *
009810080604      *        LSB6AR5G - posizione dell'unico campo nel box          *
009820080604      *        LSB6TAG  - posizione dell'unico campo nel box          *
009830080604      *        LSB6DK0  - posizione della 1ª riga di testo            *
009840081003      *§PS2 <=> §PS8  -> posizioni dei campi                        ?*
009850080604      *        LSB6DK0  - posizioni delle righe di testo              *
009860080604      *        LSB6DL0B - posizioni delle righe di testo              *
009870080604      *        LSB6AC0B - posizioni delle righe di testo              *
009880080604      *        LSB6ES0B - posizioni delle righe di testo              *
009890081003      *        LSB6DL9  - posizioni delle righe di testo              *
009900050216      *§PR1 <=> §PR2  -> posizioni dei campi (rovesciati di 270°)   ?*
009910080604      *        LSB6DK0  - posizioni delle righe di testo              *
009920080604      *        LSB6DL0A - posizioni delle righe di testo              *
009930080604      *        LSB6DL0C - posizioni delle righe di testo              *
009940080604      *        LSB6AC0A - posizioni delle righe di testo              *
009950080604      *        LSB6ES0A - posizioni delle righe di testo              *
009960030619      *===============================================================*
009970030515
009980030515     c     *Entry        plist
009990080703     c                   parm                    FNLSB5ds
010000130710     c                   parm                    Prm_LNP           3 0
010010130710     c                   parm                    Prm_NRS           2 0
010020130306     c                   parm                    FNLSB6ds1
010030110427
010040110427       // -?Tipo lancio (DB0tla):?"C" -           Chiudo con LR?
010050110427       //                        ?"L" - Elaboro e chiudo con LR?
010060110427       //                        ?" " - Elaboro e chiudo in RETRN?
010070080703if  1c                   if        DB0tla = 'C'
010080030515     c                   goto      Fine
010090060921e   1c                   endif
010100110427
010110110427       // -?Operazioni iniziali?
010120070123     c                   exsr      sr_Start
010130110427
010140030515sel 1c                   SELECT
010150130306
010160130306       // -?Richiesta la stampa (finale) delle sole immagini PDF?
010170130710     c                   WHEN      %parms() > 3  and  DB6pdf = 'S'
010180130307     c*//                                        and  DB0fa4 = 'I'
010190130306     c                   if        DB6num > *zero
010200130306     c                   exsr      sr_PrtPdf
010210130306     c                   endif
010220130306     c                   exsr      sr_End
010230110427
010240110427       // -?NON passati i dati LDV => caricamento?
010250030515w   1c                   WHEN      DB0ges = *blanks
010260071008if  2c                   if             DB0tbo  = 'P'
010270071008     c                             or   DB0tbo  = '8'
010280030515     c                   exsr      CarBLP
010290030515x   2c                   else
010300030515     c                   exsr      CarARB
010310030515e   2c                   endif
010320110427
010330110427       // -?Passati i dati LDV => è già (quasi) tutto pronto?
010340110427       //  ?(solo dalla sede - vedi TNSD50R)?
010350081114w   1c                   WHEN      DB0ges = 'B'
010360090513     c                   exsr      CarTAB
010370081114     c*** (già fatto:)   clear                   dsBL4A
010380081114     c                   if        DB0fbr = 'S'
010390081114     c                   exsr      RepAR4_A
010400081114     c***                if        §B4Abm = 'S' or §B4Abm = 'C' or
010410081114     c***                          §B4Abm = 'P' or §B4Abm = 'Q'
010420081114     c***                movel     'S'           DB0bam
010430081114     c***                endif
010440081114     c                   endif
010450081114     c                   eval      wB0cfi = DB0cpi
010460081114     c                   eval      wB0piv = DB0cpi
010470110427
010480110427       // -?DB0GES = "R" => Stampa una LDV vuota con un messaggio?
010490110427       //  ?per avvisare che è impossibile continuare la ristampa?
010500110427       //  ?perchè stampate più di 6.000 bolle !?
010510030515w   1c                   WHEN      DB0ges = 'R'
010520071112     c                   exsr      Rep_TabVPO
010530071112     c                   eval      Pmsg1  = $Msg(1)
010540071112     c                   eval      Pmsg2  = %replace( %editc(
010550071112     c                                         §VPOmaxldv : 'J' ) :
010560071112     c                                         $Msg(2) : %scan(
010570071112     c                                         '&NrLdV' : $Msg(2) ))
010580130418     c                   eval      *in33 = *off
010590130418     c                   exsr      Posiz_Box
010600030515     c                   write     A4prtMSG
010610030515     c                   goto      Fine
010620110427
010630030515e   1c                   ENDSL
010640110427
010650110427       // -?Controllo del tipo LDV da stampare?
010660040607     c                   exsr      TipoLDV
010670080919      /free
010680080919
010690110427       //?»»»?Verifiche per chiusura pgm?«««?
010700080919       SELECT;
010710080919
010720110427         // -?Chiude SE richiamato per la sola verifica del modulo?
010730110427         //  ?(dal pgm FNLSE6R1 - cl FNLSE6C1)?
010740080919         WHEN  DB0ris  = 'R'   and
010750080919               DB0fl9  = *on   and
010760080919               DUTlpo  = *blanks;
010770080919           exsr  sr_End;
010780080919
010790110427         // -?Chiude SE richiesta la stampa in formato NON corrispondente?
010800080926
010810110427         // ·?Richiesta stampa di A4, mentre risulta di ex-A5?
010820110427         //  ?NON GESTITO:?rimane il richiamo "standard" dei pgm. di?
010830110427         //  ?manutenzione => 1 A4 x ogni LdV (anche se ex-A5)?
010840080919         //WHEN  DB0fa4  = '1'   and
010850080919         //      *in69   = *on;
010860080919         //  DB0tms = 'E';
010870080919         //  exsr  sr_End;
010880080919
010890110427         // -?Richiesta stampa di ex-A5, mentre risulta di A4?
010900080919         WHEN  DB0fa4  = '2'   and
010910080919               *in69   = *off;
010920080919           DB0tms = 'D';
010930080919           exsr  sr_End;
010940080919
010950080919       ENDSL;
010960080919
010970080919      /end-free
010980110427
010990110427       // -?Imposta i campi di stampa con quelli della DS?
011000030515     c                   exsr      PrepPRT
011010110427
011020110427       // -?Operazioni "inutili"?SE?richiesta la stampa della sola?
011030110427       //  ?packing-list:?
011040080919     c                   IF        DB0fa4 <> 'P'
011050110427
011060110427       // -?Decodifica i dati della LDV?
011070040607     c                   exsr      DecodLDV
011080110427
011090080919     c                   ENDIF
011100110427
011110110427       // -?Stampa della Lettera di Vettura?
011120030515sel 1c                   SELECT
011130110427       // -?Se RIMESSO MITTENTE stampo il numero di copie richiesto?
011140110427       //  ?(non per DPD e FedEx)?
011150110427       //  ?(le stampo qui perché devo stampare il riferimento numerico?
011160110427       //  ?aggiornato di volta in volta su FLNUF)?
011170030515w   1c                   WHEN      DB0ris = 'M'  and  *in70
011180030515     c                   clear                   wCOPIE
011190030515     c                   clear                   NUFaaa
011200030515     c                   eval      NUFcnu = 20
011210030515     c                   eval      NUFfil = DB0lnp
011220030515     c                   clear                   NUFfl2
011230030515do  2c                   DO        Wctr
011240030515     c     K04NUF        chain     FLNUF01L
011250030515if  3c                   if        %found(FLNUF01L)
011260030515     c                   add       1             NUFnum
011270030515     c                   UPDATE    FLNUF
011280030515     c                   eval      Prmn = NUFnum
011290040607     c                   exsr      StampaLDV
011300030515e   3c                   endif
011310030515e   2c                   ENDDO
011320110427       // -?Se FATTURA PREPAGATA stampo due copie?
011330110427       //  ?(solo se non attivata procedura O.R.M.)?
011340030515w   1c                   WHEN      DB0tbo = '8'  and  §OGorm <> 'S'
011350030515     c                   eval      wCOPIE = 2
011360110427       // -?Altrimenti stampo solo una copia?
011370030515x   1c                   OTHER
011380030515     c                   eval      wCOPIE = 1
011390030515e   1c                   ENDSL
011400110427
011410030515do  1c                   do        wCOPIE
011420080918      /free
011430080918         if  DB0fa4 = 'P';
011440130315           exsr  StampaPkgLst;
011450080918         else;
011460110427           exsr  StampaLDV;
011470080918         endif;
011480080918      /end-free
011490030515e   1c                   enddo
011500130306
011510130306       // -?Eventuale stampa immagini PDF?
011520130306       //  ?(si spera della sola spedizione di cui si è appena?
011530130306       //   ?stampata la LdV)?
011540130710     c                   if        %parms() > 3  and  DB6pdf = 'X'
011550130306     c                                           and  DB6num > *zero
011560130306     c                   exsr      sr_PrtPDF
011570130306     c                   endif
011580110427
011590110427       // -?Operazioni finali?
011600110427     c     Fine          tag
011610080624     c                   exsr      sr_End
011620110427
011630030515      *---------------------------------------------------------------*
011640050217      *?OPERAZIONI INIZIALI                                          ?*
011650030515      *---------------------------------------------------------------*
011660030515     c     *InzSR        BEGSR
011670131023      *
011680131023      * Reperimento dati utente
011690131023      * Reperisco nome azienda per stampa PREPAGATI (e non solo)
011700131023     c     *dtaara       define    §azute        AZUTEds
011710131023     c     *dtaara       define    §datiute      dDATIUTE
011720131023     c                   in(E)     *dtaara
011730131023     c                   if        %ERROR or RSUT = *blanks
011740131023     c                   clear                   Tibs34Ds
011750131023     c                   call      'TIBS34R'
011760131023     c                   parm                    Tibs34Ds
011770131023     c                   in        *dtaara
011780131023     c                   endif
011790131023      * Impostazione dati in KPJBA
011800131023     c                   eval      KNMUS  = SDSusr
011810110429      *
011820131023      *USCITA? SE  richiamato per la sola chiusura
011830130314     c*//                if        %subst(KPJBU:1:1) = 'C'
011840130314     c                   if        DB0tla = 'C'
011850110429     c                   leavesr
011860110429     c                   endif
011870070424      *
011880070424      * Reperisco la data del giorno
011890070424     c                   time                    w0140
011900070424     c                   move      w0140         w0080
011910070424     c     *eur          movel     w0080         Data_EUR
011920070424     c     *iso          movel     Data_EUR      w0080
011930070424     c                   move      w0080         wDateA
011940071105     c                   eval      wDate  = w0080
011950030515      *
011960030515      * Imposto i campi chiave "fissi"
011970040223     c                   eval      TBLkut = 1
011980060505     c                   eval      ARStrc = 'C'
011990070319     c                   eval      ARNcdn = 'LDV'
012000070319     c                   eval      ARNgst = 'S'
012010030515      *
012020030515      * Carico le varie esenti IVA
012030030515     c                   movel     'CC'          TBLcod
012040030515     c                   movel(p)  'VARIE'       TBLkey
012050030515     c                   clear                   XX
012060030515     c     K03TAB        setll     TABEL
012070030515     c     K02TAB        reade     TABEL
012080030515      *
012090070306do  1c                   DOW       NOT %eof(TABEL00F)
012100030515      * Deve essere NON annullata e con codice che inizia con "VARIE"
012110030515if  2c                   If        %subst(TBLkey:1:5) = 'VARIE'
012120030515     c                             and    TBLflg      = *blanks
012130030515     c                   movel     TBLuni        dsCC
012140030515if  3c                   if        §CCcei <> *blanks
012150030515     c                   add       1             xx
012160030515     c                   move      TBLkey        $Var(xx)
012170030515e   3c                   endif
012180030515e   2c                   EndIf
012190030515      *
012200030515     c     K02TAB        reade     TABEL
012210030515e   1c                   ENDDO
012220070306      *
012230070308      * Carico le causali eventi con tipo consegna = "G"
012240070306     c                   movel     '2A'          TBLcod
012250070306     c                   clear                   TBLkey
012260070306     c                   clear                   XX
012270070306     c     K03TAB        setll     TABEL
012280070306     c     K02TAB        reade     TABEL
012290070306      *
012300070306do  1c                   DOW       NOT %eof(TABEL00F)
012310070308      * Deve essere NON annullata e con flag §2AFTC = "G"
012320070308if  2c                   If        TBLflg = *blanks
012330070306     c                   movel     TBLuni        ds2A
012340070308if  3c                   if        §2Aftc = 'G'
012350070306     c                   add       1             xx
012360070308     c                   movel     TBLkey        $2Acev_G(xx)
012370070306e   3c                   endif
012380070306e   2c                   EndIf
012390070306      *
012400070306     c     K02TAB        reade     TABEL
012410070306e   1c                   ENDDO
012420030515      *
012430030515      * Carico tabella VARIABILI ARRIVI
012440030515     c                   clear                   ds$2
012450030515     c                   eval      TBLcod = '$2'
012460030515     c                   eval      TBLkey = '1'
012470030515     c     K03TAB        chain     TABEL
012480030515if  1c                   if        %found(TABEL00F)
012490030515     c                   movel     TBLuni        ds$2
012500030515e   1c                   endif
012510030515      *
012520030515      * Reperisco la moneta di conto
012530030515     c                   clear                   dGED
012540040607     c                   clear                   TIBS02ds
012550040607     c                   eval      T02mod = 'C'
012560040607     c                   eval      T02cod = 'GED'
012570040607     c                   eval      T02ke1 = '1         '
012580030515     c                   call      'TIBS02R'
012590030515     c                   parm                    KPJBA
012600040607     c                   parm                    TIBS02ds
012610030515if  1c                   if        T02err = *blanks
012620030515     c                   movel     T02uni        dGED
012630030515e   1c                   endif
012640030515      *
012650030515      * Con moneta di conto:
012660060301      *   Carico tabella GESTIONALE - IMPORTI VARI   e
012670030515      *   Reperisco l'importo limite per abilitazione c/a
012680030515     c                   clear                   dGEI
012690040607     c                   clear                   TIBS02ds
012700040607     c                   eval      T02mod = 'C'
012710040607     c                   eval      T02cod = 'GEI'
012720040607     c                   eval      T02ke1 = §GEDcn
012730030515     c                   call      'TIBS02R'
012740030515     c                   parm                    KPJBA
012750040607     c                   parm                    TIBS02ds
012760030515if  1c                   if        T02err = *blanks
012770030515     c                   movel     T02uni        dGEI
012780030515e   1c                   endif
012790061031      *
012800110426      * Reperisco data di decorrenza stampa "Per ricevuta di ...
012810071005      *                                      senza riserve"
012820090615      *              e di decorrenza stampa per C.L.N.
012830061031     c                   clear                   dVPOdeco
012840061031     c                   clear                   TIBS02ds
012850061031     c                   eval      T02mod = 'C'
012860071112     c***                eval      T02sif  = KNSIF
012870071112     c                   eval      T02cod = 'VPO'
012880061031     c                   eval      T02ke1 = 'DECO      '
012890061031     c                   call      'TIBS02R'
012900061031     c                   parm                    KPJBA
012910061031     c                   parm                    TIBS02ds
012920061031if  1c                   if        T02err = *blanks
012930061031     c                   movel     T02uni        dVPOdeco
012940061031e   1c                   endif
012950120530      *
012960120530      *//*?Reperisco data di decorrenza stampa nuovo logo BRT?
012970120530     c*//                clear                   dVPObrt
012980120530     c*//                clear                   TIBS02ds
012990120530     c*//                eval      T02mod = 'C'
013000120530     c***                eval      T02sif  = KNSIF
013010120530     c*//                eval      T02cod = 'VPO'
013020120530     c*//                eval      T02ke1 = 'BRT       '
013030120530     c*//                call      'TIBS02R'
013040120530     c*//                parm                    KPJBA
013050120530     c*//                parm                    TIBS02ds
013060120530if  1c*//                if        T02err = *blanks
013070120530     c*//                movel     T02uni        dVPObrt
013080120530e   1c*//                endif
013090080708      /free
013100110426         // -?Override al file di stampa?
013110081017         if  DB0tla <> 'C';
013120080709           fnlsB8r ( fnlsB5ds );
013130080708         endif;
013140080708      /end-free
013150030515      *
013160030515      * Apro i file di stampa
013170130710     c                   open      FNLSB6P
013180080918     c                   clear                   CurLineA4
013190130306      /free
013200130306         // -?Reperimento override apportate al file di stampa?
013210130306         //  ?per comunicarle per la stampa immagini PDF?
013220130306         if  DB0tla <> 'C';
013230130306           wPrtF = 'FNLSB6P';
013240130306           clear  trulPOVRds;
013250130307           trulPOVRc ( wPrtF : trulPOVRds);
013260130307           if trulPOVRds.§ULPoerr <> *blank;  //?Nessuna override rilevata?
013270130321             //trulPOVRds.§ULPoOutQ  = '*JOB      ';
013280130321             trulPOVRds.§ULPoOutQ  = 'PRT01     ';
013290130307             trulPOVRds.§ULPoFType = '*LDVUNI   ';
013300130307             trulPOVRds.§ULPoUDta  = 'LDV_UNI   ';
013310130306           endif;
013320130306         endif;
013330130306      /end-free
013340030515      *
013350030515     c                   ENDSR
013360071112      *
013370071112      *---------------------------------------------------------------*
013380071112      *?REPERIMENTO N° MAX DI LDV STAMPABILI DA TAB. "VPO"           ?*
013390071112      *---------------------------------------------------------------*
013400071112     c     Rep_TabVPO    BEGSR
013410071112      *
013420071112      * Aggancio tabella "VPO" per reperimento del n° LdV stampabili
013430071112      *   limite (per congelamento e blocco stampa)
013440071112     c                   clear                   dVPO
013450071112     c                   clear                   TIBS02ds
013460071112     c                   eval      T02mod  = 'C'
013470071112     c***                eval      T02sif  = KNSIF
013480071112     c                   eval      T02cod  = 'VPO'
013490071112     c                   movel(P)  'VPO'         T02ke1
013500071112     c                   call      'TIBS02R'
013510071112     c                   parm                    KPJBA
013520071112     c                   parm                    TIBS02ds
013530071112     c                   if        T02err  = *blanks
013540071112     c                   movel     T02uni        dVPO
013550071203     c                   if        §VPOMaxLdv = *zeros
013560071203     c                   eval      §VPOMaxLdv = 10000
013570071203     c                   endif
013580071112     c                   else
013590071112     c                   eval      §VPOMaxLdv = 10000
013600071112     c                   endif
013610071112      *
013620071112     c                   ENDSR
013630070123      *
013640070123      *---------------------------------------------------------------*
013650070306      *?ALTRE OPERAZIONI INIZIALI (NON PIÙ IN *INZSR)                ?*
013660070123      *---------------------------------------------------------------*
013670070123     c     sr_Start      BEGSR
013680070123      *
013690080709      /free
013700081017         select;
013710081017           when  DB0fa4 = *blank;
013720081017             DB0fa4 = '1';
013730081020           when  DB0fa4 = 'P';
013740081017             DB0fl6 = *on;
013750081017         endsl;
013760080709      /end-free
013770080709      *
013780070123if  1c                   if        DB0fl7 = '1'
013790130710     c                   close     FNLSB6P
013800130710     c                   open      FNLSB6P
013810081015     c                   clear                   CurLineA4
013820070123     c                   clear                   Page4
013830070123e   1c                   endif
013840070123      *
013850070123      * »»» CHIODO: SE richiamato per la sola verifica del modulo «««
013860070308      *     RIPULISCO il "livello della filiale" (S/1/2) perchè:
013870070123      *     1) sarò partito dal S.I. di sede, ma
013880070123      *     2) sarà stata cambiata la lista di librerie come x S.I. di
013890070308      *        filiale (dal CL FNLSE6C1)
013900070123if  1c                   if             DB0ris  = 'R'
013910070123     c                             and  DB0fl9  = *on
013920070123     c                             and  DUTlpo <> *blanks
013930070123     c                   clear                   DUTlpo
013940070123e   1c                   endif
013950070123      *
013960070308      * Apertura archivi di sede o filiale - comunque utilizzati
013970070123if  1c                   if        DUTlpo = 'S'
013980070123if  2c                   if        NOT  %open(FIAR531C)
013990070123     c                   open      FIAR531C
014000070123e   2c                   endif
014010070123x   1c                   else
014020070123if  2c                   if        NOT  %open(FIAR401L)
014030070123     c                   open      FIAR401L
014040070123e   2c                   endif
014050070123if  2c                   if        NOT  %open(FIAR501L)
014060070123     c                   open      FIAR501L
014070070123e   2c                   endif
014080070123if  2c                   if        NOT  %open(FLNUF01L)
014090070123     c                   open      FLNUF01L
014100070123e   2c                   endif
014110070123e   1c                   endif
014120110427      *
014130110427      * Pulizia del PRTF
014140110427     c                   exsr      ClrPRTF
014150070123      *
014160070123     c                   clear                   wCOPIE
014170070123      *
014180070123     c                   ENDSR
014190080624      *
014200080624      *---------------------------------------------------------------*
014210080624      *?OPERAZIONI FINALI                                            ?*
014220080624      *---------------------------------------------------------------*
014230080624     c     sr_End        BEGSR
014240080624      *
014250080624      * Chiusura pgm
014260080919if  1c                   IF        DB0tla <> *blanks
014270130710     c                   close     FNLSB6P
014280080708      /free
014290080708       Qcmd = C_CmdDltOvr;
014300080708       callp(e) qCmdExc(Qcmd : %size(Qcmd));
014310080708      /end-free
014320080624     c                   clear                   TIBS02ds
014330080624     c                   eval      T02tla = 'C'
014340080624     c                   call      'TIBS02R'
014350080624     c                   parm                    KPJBA
014360080624     c                   parm                    TIBS02ds
014370080624     c                   reset                   FNLV80ds
014380080624     c                   call      'FNLV80R'
014390080624     c                   parm                    KPJBA
014400080624     c                   parm                    FNLV80ds
014410080624     c                   clear                   FIDN12ds
014420080624     c                   eval      I12tla = 'C'
014430080624     c                   call      'FIDN12R'
014440080624     c                   parm                    FIDN12ds
014450090605      /free
014460090622           if  §VPOrcm <= wDate   and   DUTlpo  <> 'S';
014470090615             reset  TNSD99ds;
014480090615             tnsd99r ( TNSD99ds );
014490090615           endif;
014500100322           if  §VPOcas <= wDate   and   DUTlpo  <> 'S';
014510100208             clear  trulINTds;
014520100208             iINTtla = 'C';
014530100208             trulINTr ( TRULINTds );
014540100322           endif;
014550090605      /end-free
014560080624     c                   eval      *inLR  = *on
014570080624e   1c                   ENDIF
014580080919      *
014590080919      * Ritorno al chiamante
014600080919     c                   return
014610080624      *
014620080624     c                   ENDSR
014630030515      *
014640030515      *---------------------------------------------------------------*
014650050217      *?Pulisco i campi di stampa                                    ?*
014660030515      *---------------------------------------------------------------*
014670030515     c     ClrPRTF       BEGSR
014680030515      *
014690130418     c                   clear                   A4prtBOX
014700120530     c*//                clear                   A4LogoBAR
014710120530     c*//                clear                   A4LogoDA1
014720120530     c*//                clear                   A4LogoBR2
014730120530     c*//                clear                   A4LogoDA2
014740120530     c                   clear                   A4LogoBRT
014750120530     c                   clear                   A4LogoDA3
014760130418     c                   clear                   A4prtLDV
014770080604     c                   clear                   A4prtLDVB
014780080922     c                   clear                   A4prtDPD
014790030515     c                   clear                   A4prtFED1
014800030515     c                   clear                   A4prtFED2
014810030515     c                   clear                   A4prtFED3
014820030515     c                   clear                   A4prtAR51
014830030515     c                   clear                   A4prtAR52
014840030515     c                   clear                   A4prtAR53
014850030909     c                   clear                   A4prtAR5A
014860040607     c*** stessi campi:  clear                   A4prtAR5B
014870040607     c*** stessi campi:  clear                   A4prtAR5C
014880070321     c*** stessi campi:  clear                   A4prtAR5G
014890040311     c*** no campi:      clear                   A4prtSP0
014900040607     c*** no campi:      clear                   A4prtSP1
014910080709     c*** no campi:      clear                   A4prtSP2
014920040311     c*** no campi:      clear                   A4prtNUL
014930030911     c                   clear                   A4prtGEN
014940040607     c                   clear                   A4prtDK0
014950040607     c                   clear                   A4prtDK1
014960040607     c                   clear                   A4prtDK2
014970040607     c                   clear                   A4prtDK3
014980040607     c                   clear                   A4prtDK4
014990040607     c                   clear                   A4prtDK5
015000081222     c                   clear                   A4prtDLL
015010041019     c                   clear                   A4prtDL0
015020050318     c                   clear                   A4prtDL0A
015030050318     c                   clear                   A4prtDL0B
015040050318     c                   clear                   A4prtDL0C
015050050606     c                   clear                   A4prtDL0D
015060050606     c                   clear                   A4prtDL0E
015070050606     c                   clear                   A4prtDL0F
015080041028     c                   clear                   A4prtDL1A
015090041028     c                   clear                   A4prtDL1B
015100050318     c                   clear                   A4prtDL1C
015110050606     c                   clear                   A4prtDL1D
015120050606     c                   clear                   A4prtDL1E
015130041019     c                   clear                   A4prtDL2
015140060614     c                   clear                   A4prtDL21
015150041019     c                   clear                   A4prtDL3
015160041019     c                   clear                   A4prtDL4
015170050318     c                   clear                   A4prtDL5
015180050606     c                   clear                   A4prtDL6
015190050606     c                   clear                   A4prtDL7
015200050606     c                   clear                   A4prtDL8
015210050606     c                   clear                   A4prtDL9
015220050114     c                   clear                   A4prtAC0
015230050114     c                   clear                   A4prtAC1A
015240050114     c                   clear                   A4prtAC1B
015250050114     c                   clear                   A4prtAC2
015260050114     c                   clear                   A4prtAC3
015270050114     c                   clear                   A4prtAC4
015280050214     c                   clear                   A4prtES0
015290050214     c                   clear                   A4prtES0A
015300050214     c                   clear                   A4prtES0B
015310050215     c                   clear                   A4prtES1A
015320050215     c                   clear                   A4prtES1B
015330050214     c                   clear                   A4prtES2
015340050214     c                   clear                   A4prtES3
015350050215     c                   clear                   A4prtES4
015360050214     c                   clear                   A4prtDB0
015370041207     c                   clear                   A4prtDT0
015380040607     c                   clear                   A4prtDT1
015390040607     c*** stessi campi:  clear                   A4prtDT2
015400040607     c*** stessi campi:  clear                   A4prtDC1
015410030515     c                   clear                   A4prtSEG
015420030515     c                   clear                   A4prtMSG
015430030515      *
015440041207     c                   reset                   dAR8
015450060524     c                   clear                   dsBL4A
015460171117     c*//                clear                   dsBL4I
015470061108     c                   clear                   dTA4Q
015480100514     c                   clear                   dOrm01
015490041207      *
015500070514     c                   reset                   $Mamma
015510130215     c                   reset                   $Mamma_FedEx
015520070306     c                   reset                   $FIARN
015530060921     c                   reset                   $FIAR5
015540030515     c                   reset                   $First
015550030515     c                   reset                   $FirstSCR
015560060921     c                   reset                   $FIAR8
015570130306     c                   reset                   $§PDF
015580040608     c                   reset                   $DDT_o
015590040608     c                   reset                   $DDT_p
015600040608     c                   reset                   $DDTlastPg
015610030515     c                   reset                   $Partic
015620030710     c                   reset                   $NoDesPartic
015630030910     c                   reset                   $BolloVirt
015640061106     c                   reset                   $No_CF
015650071114     c                   reset                   $No_PI
015660040707     c                   reset                   $No_x_Cons
015670101125     c                   reset                   $Rit_A_Inc
015680050513     c                   reset                   $A4_x_ScNS
015690070306     c                   reset                   $LascAvv
015700070319     c                   reset                   $ConsParz
015710070315     c                   reset                   $C_A_
015720030515     c                   reset                   $CAeur
015730090302     c                   reset                   $TabPXA
015740110411     c                   reset                   $TabPXA2
015750090924     c                   reset                   $BoxNxD
015760100513     c                   reset                   $PreavvORM
015770110427     c                   reset                   $GeoAtt
015780070717     c                   clear                   wGeoTel
015790070717     c                   clear                   wGeoCon
015800070717     c                   clear                   Wdsp
015810090527     c                   clear                   WddtCLN
015820090527     c                   clear                   WdspCLN
015830030717     c                   clear                   W_CoOr
015840040608     c                   clear                   wOmaRowT
015850040608     c                   clear                   wOmaRowD
015860050624     c                   clear                   wBolloVirt
015870030515     c                   clear                   Wcas
015880030515     c                   clear                   Wiva
015890071015     c                   clear                   Wpdc4a
015900080926     c                   clear                   wPage
015910080704     c                   reset                   $LdVxP
015920090121     c*** (da non fare:) clear                   wNumLdV
015930090121     c*** (da non fare:) clear                   wSeqLdV
015940071114     c                   clear                   wB0cfi
015950071114     c                   clear                   wB0piv
015960031015     c                   clear                   wB0nrs
015970070514     c                   clear                   wB0ndc
015980070514     c                   clear                   wB0ntc
015990070514     c                   clear                   wB0dcp
016000081119     c                   clear                   wB0tmc
016010101125     c                   clear                   wB0dcm
016020101125     c                   clear                   wB0icc
016030101125     c                   clear                   wB0ica
016040130124     c                   clear                   wB0fiv
016050140307     c                   clear                   wEVBdev
016060140307     c                   clear                   wEVBhev
016070030515      *
016080030515     c                   clear                   $NP
016090030515     c                   clear                   $SC1
016100030515     c                   clear                   $SC2
016110030905     c                   clear                   $QUM
016120040607     c                   clear                   $AR8o1
016130070319     c                   reset                   $CP
016140070319     c                   reset                   $CPdcm
016150030515      *
016160030515     c                   movea     *zeros        *in(01)
016170030515      *
016180030515     c                   ENDSR
016190030515      *
016200030515      *---------------------------------------------------------------*
016210050217      *?Determino il modulo LDV da usare per la stampa               ?*
016220120530      *?(se A5 o A4 e se DPD, FedEx o BRT)                           ?*
016230030515      *---------------------------------------------------------------*
016240040607     c     TipoLDV       BEGSR
016250070212      *
016260080703      * Impostazione ds "Flag operativi DB0FLO di FNLSB5DS"
016270070215     c                   movel     DB0flo        dDB0flo
016280070215      *
016290070212      * Impostazione della data spedizione
016300070212      * (se non già impostata in subr. "CarBLP" o "CarARB")
016310070212if  1c                   if        Wdsp   =  *zeros
016320070212if  2c                   if        DB0mgs <> *zeros
016330070212     c                   eval      Wdsp   =  (DB0aas * 10000) + DB0mgs
016340070212x   2c                   else
016350070212      *   prendo la data del giorno se non c'è la data spedizione
016360070212     c                   time                    w0140
016370070212     c                   move      w0140         w0080
016380070212     c     *eur          movel     w0080         Data_EUR
016390070212     c     *iso          movel     Data_EUR      Wdsp
016400070212e   2c                   endif
016410070212e   1c                   endif
016420040311      *
016430040311      * Dati della linea di partenza
016440040311if  1c                   if        DB0lnp <> SAVlnp
016450040311     c                   eval      SAVlnp =  DB0lnp
016460040311     c     DB0lnp        chain     AZORG
016470040311if  2c                   if        %found(AZORG01L)
016480040311     c                   movel     ORGde3        OG143
016490040311     c                   movel     ORGde8        OG148
016500040311x   2c                   else
016510040311     c                   clear                   OG143
016520040311     c                   clear                   OG148
016530040311e   2c                   endif
016540040311e   1c                   endif
016550040607      *
016560070308      * Dati della filiale del mittente
016570040607     c                   eval      WX     =  DB0ccm / 10000
016580040607if  1c                   if        WX     <> ORGfil
016590040607     c     WX            chain     AZORG
016600040607e   1c                   endif
016610040607if  1c                   if        %found(AZORG01L)
016620040607     c                   movel     ORGde3        OG143ccm
016630040607x   1c                   else
016640040607     c                   clear                   OG143ccm
016650040607e   1c                   endif
016660040311      *
016670040311      * Dati della linea di arrivo
016680040311if  1c                   if        DB0lna <> SAVlna
016690040311     c     DB0lna        chain     AZORG
016700040311if  2c                   if        %found(AZORG01L)
016710070312     c                   movel     ORGde6        OG146arr
016720061013     c                   movel     ORGde8        OG148arr
016730070312     c                   movel     ORGdf0        OG150arr
016740040311x   2c                   else
016750070312     c                   clear                   OG146arr
016760061013     c                   clear                   OG148arr
016770070312     c                   clear                   OG150arr
016780040311e   2c                   endif
016790050510e   1c                   endif
016800070212      *
016810070212      * Dati della capofila della linea di arrivo
016820070306      * - per verificare TELLUS   e
016830070306      * - per non stampare la "copia per il destinatario" nel caso
016840070306      *   di precedente "Lasciato Avviso"
016850170717      * - per i dati della LINEA DI ARRIVO in stampa
016860170717      *
016870170717      * - Reperimento capofila (£6)
016880170905      *   (ad oggi, NON alla data della spedizione)
016890170717if  1c                   if        DB0lna <> D55lin
016900170717     c                   reset                   FNLV55ds
016910170717     c*** già così:      eval      D55tpt =  '6'
016920170717     c                   eval      D55lin =  DB0lna
016930170905     c*//                eval      D55drf =  Wdsp
016940170905     c                   eval      D55drf =  wDate
016950170717     c                   call      'FNLV55R'
016960170717     c                   parm                    FNLV55ds
016970170717if  2c                   if        D55err <> *blanks
016980170717     c                   eval      D55tfa =  DB0lna
016990170717e   2c                   endif
017000170717e   1c                   endif
017010170717      * - Reperimento 146ª e 148ª descrizioni dell'organigramma
017020170717      *   (flag Tellus del capofila e flag gestione telefonate GEO)
017030170717if  1c                   if        D55tfa <> ORGfil
017040170717     c     D55tfa        chain     AZORG
017050170718e   1c                   endif
017060170718if  1c                   if        %found(AZORG01L)
017070170717     c                   eval      wDB0lnaCf£6 = ORGfil
017080170717     c                   eval      wDB0dlaCf£6 = ORGdes
017090170717     c                   eval      wDB0inlCf£6 = ORGind
017100170717     c                   eval      wDB0calCf£6 = ORGcpf
017110170717     c                   eval      wDB0lolCf£6 = ORGloc
017120170717     c                   eval      wDB0prlCf£6 = ORGpro
017130170717     c                   eval      wDB0telCf£6 = ORGtel
017140170717     c*//                eval      wDB0faxCf£6 = ORGfax
017150170717     c                   eval      wDB0faxCf£6 = ORGdd2
017160170717     c                   movel     ORGde6        OG146cf£6
017170170717     c                   movel     ORGde8        OG148cf£6
017180170718x   1c                   else
017190170717     c                   clear                   wDB0lnaCf£6
017200170717     c                   clear                   wDB0dlaCf£6
017210170717     c                   clear                   wDB0inlCf£6
017220170717     c                   clear                   wDB0calCf£6
017230170717     c                   clear                   wDB0lolCf£6
017240170717     c                   clear                   wDB0prlCf£6
017250170717     c                   clear                   wDB0telCf£6
017260170717     c                   clear                   wDB0faxCf£6
017270170717     c                   clear                   OG146cf£6
017280170717     c                   clear                   OG148cf£6
017290170718e   1c                   endif
017300070717      *
017310070717      *** Imposta flag attivazione GEO x LNA
017320070717     c                   eval      wGeoCon = A§OGtlls
017330070717     c                   eval      wGeoTel = A§OGgeot
017340070717if  1c                   if             A§OGdda  > *zeros
017350070717     c                             and  A§OGdda  < C_DateMax_A
017360070717     c                             and  A§OGdda <= wDateA
017370110427     c                   eval      $GeoAtt = *on
017380070717e   1c                   endif
017390070717      *
017400070717if  1c                   IF        DB0tbo = 'A'  AND
017410070717     c                                 (wGeoCon <> 'S'
017420070717     c                              or  wGeoTel <> 'S'
017430110427     c                              or  $GeoAtt <> *on)
017440070717      *** Imposta flag attivazione GEO x capofila £6
017450070717     c                   eval      wGeoCon = £6OGtlls
017460070717     c                   eval      wGeoTel = £6OGgeot
017470071109if  2c                   if             £6OGdda  > *zeros
017480070717     c                             and  £6OGdda  < C_DateMax_A
017490070717     c                             and  £6OGdda <= wDateA
017500110427     c                   eval      $GeoAtt = *on
017510071109e   2c                   endif
017520070717      *
017530070212e   1c                   ENDIF
017540050510      *
017550050510      * Verifica se richiesta stampa in A4 in tab. "7R"
017560050510      *   (gestione particolarità consegna)
017570050510     c                   clear                   ds7R
017580170717if  1c                   if        DB0gma <> *blanks
017590070312     c                   eval      TBLcod =  '7R'
017600050510     c                   movel(p)  DB0gma        TBLkey
017610050510     c     K03TAB        chain     TABEL
017620050510if  2c                   if        %found(TABEL00F)
017630050510     c                   movel     TBLuni        ds7R
017640050510e   2c                   endif
017650050510e   1c                   endif
017660071015      *
017670071015      * Verifica se LDV con "Ritiro Porto Franco Prepagato"
017680071015     c                   clear                   dsBL4M
017690071015     c                   if        DB0tbo = '8'
017700071015     c                   exsr      RepAR4_M
017710071015     c                   endif
017720130315      *
017730130315      * Reperimento dati da FIAR5 / "GEN"
017740130315      *   (per verificare se esistono Packing-List e/o PDF allegati
017750130315      *    - oltre che per reperire Nominativo e Telefono)
017760130315     c                   exsr      RepAR5gen
017770040607      *
017780130315      * Verifica se LDV con DDT e/o PDF
017790130315     c                   exsr      CercaDDToPDF
017800061031      *
017810071114      * Verifica se richiesta stampa in A4 per richiesta Codice Fiscale
017820071114      *   e/o Partita IVA al cliente.
017830120503      *-=»>?01/08/2008:?                                          <«=-*
017840080801      *-=»> Abolite richieste di Codice Fiscale e/o Partita Iva.  <«=-*
017850080801      *-=»> Tolta ricerca di 'sti 2 dati, lasciando il defualt di <«=-*
017860080801      *-=»> NON richiesta: $No_CF = *off e $No_PI = *off.         <«=-*
017870120503      *-=»>?03/05/2012:?                                          <«=-*
017880120503      *-=»>?Ripristinate richieste di Cod. Fiscale e/o Part. Iva.?<«=-*
017890120503if  1c                   if        DUTlpo <> 'S'
017900120503     c                   exsr      CheckCFPI
017910120503e   1c                   endif
017920070306      *
017930070319      * Verifica se richiesta stampa in A4 per note in FIARN00F
017940070306      *   (da stampare nel box "Note per l'autotrasportatore")
017950070319      * » se stampa in arrivo
017960070319if  1c                   if        DB0tbo = 'A'
017970070306     c                   exsr      CheckFIARN
017980070319e   1c                   endif
017990090302      /free
018000090302
018010090302         // Verifica se richiesta stampa in A4 per note specifiche
018020090302         //   per il cliente mittente (v. tab. "PXA")
018030090302         //   (da stampare nel box "Note per l'autotrasportatore")
018040090302         exsr  CheckTabPXA;
018050090924
018060090924         // Verifica se richiesta stampa in A4 per note specifiche
018070090924         //   per il destinatario (da parte del mittente)
018080090924         //   (da stampare in un box a parte)
018090090924         if  DB0ccm = c_CliSG;
018100090924           $BoxNxD = *on;
018110090924         endif;
018120090302      /end-free
018130070319      *
018140070319      * Verifica se richiesta spedizione già parzialmente consegnata
018150070319      *   (da stampare nel box "Note per l'autotrasportatore")
018160070717      * » se stampa in arrivo e LNA - o capofila £6 - con GEO attivato
018170070717      * completamente
018180070319if  1c                   if             DB0tbo   =  'A'
018190070514     c                             and  wB0dcp   >  *zeros
018200110427     c                             and  $GeoAtt  =  *on
018210140514      * -?Reperimento consegne parziali precedenti da FNART?
018220140514     c                   exsr      SchConsParz
018230140514     c                   if        xx > *zero
018240070413     c                   eval      $ConsParz     =  *on
018250140514     c                   endif
018260070319e   1c                   endif
018270070315      *
018280070315      * Verifica se richiesta stampa in A4 per C.A. aperta
018290070717      * » se stampa in arrivo e LNA - o capofila £6 - con GEO attivato
018300070717      * completamente
018310070319if  1c                   if             DB0tbo   =  'A'
018320110427     c                             and  $GeoAtt  =  *on
018330070316     c                   exsr      CercaCA
018340070316e   1c                   endif
018350030515      *
018360030515      * Tipo modulo (per la separazione dei box: vettore/destinatario)
018370030515      *   (*in69 = formato A5 / not *in69 = formato A4)
018380030515     c                   eval      *in69  = *on
018390030515      *
018400040607      * Tipo LDV (per richiesta firma per ricevuta della merce senza
018410040607      *   riserve)
018420120530      *   (70 = BRT, 71 = DPD, 72 = FedEx, 73 = EuroExpress)
018430080912     c                   movea     '1000'        *in(70)
018440081210      /free
018450081210         // - LDV DPD
018460170605         //if  §OGntw = 'DPD'   -  ?NON della LnP, MA della fil. mitt.:?
018470081210         if  §CMntw = 'DPD';
018480081210           *in71 = *on;
018490081210         endif;
018500081210      /end-free
018510040311      *
018520030515sel 1c                   SELECT
018530030515      *
018540030515      * Rimesso Mittente: va bene il formato A5
018550030515      *   (se non richiesta la forzatura del formato "A4")
018560040707w   1c                   WHEN      DB0ris = 'M'   and  DB0fa4 <> 'S'
018570040707      *
018580040707      * LDV POSTE non valida per la consegna: va bene il formato A5
018590040707      *   (se non richiesta la forzatura del formato "A4")
018600040707w   1c                   WHEN      §OGntw = 'PPT' and  DB0fa4 <> 'S'
018610071213      *
018620081210     *** * LDV DPD
018630081210     *** > (Spostato prima per NON rendere incompatibile DPD con il box
018640081210     *** >  di note aggiuntive "promemoria per l'autotrasportatore")
018650081210w   1***c*** non della LNP  WHEN      §OGntw = 'DPD' - ma della fil. mitt.:
018660081210w   1***c                   WHEN      §CMntw = 'DPD'
018670081210     ***c                   movea     '0100'        *in(70)
018680071213      *
018690071213      * LDV con mittente FedEx
018700150504w   1c                   WHEN      DB0ccm = c_CliFedEx   or
018710150504     c                             DB0ccm = c_CliFedEx2
018720080922     c                   movea     '0010'        *in(70)
018730071213     c                   eval      *in69  = *off
018740040607      *
018750040607      * LDV con DDT del mittente
018760081013      * - IN FUTURO: se al 1° tentativo di consegna
018770081013      *              (DB0FL1 = "D" - v. pgm FNLRS9R)
018780080917w   1c                   WHEN           $FIAR8 =  *on
018790081013     c***-=»>                      and  DB0fl1 <> 'D'
018800040607     c                   eval      *in69  = *off
018810061102      *
018820071114      * Codice Fiscale o Partita IVA da richiedere al cliente
018830071114w   1c                   WHEN           $No_CF = *on
018840071114     c                             or   $No_PI = *on
018850061102     c                   eval      *in69  = *off
018860070306      *
018870070306      * LDV con Note (dal file FIARN00F)
018880070306w   1c                   WHEN      $FIARN = *on
018890070306     c                   eval      *in69  = *off
018900080801      *
018910080801      * LDV già parzialmente consegnata
018920080801w   1c                   WHEN      $ConsParz   =  *on
018930080801     c                   eval      *in69  = *off
018940070315      *
018950070315      * LDV con C.A. aperta per la spedizione (dal file FNDCT00F)
018960070315w   1c                   WHEN      $C_A_  = *on
018970070315     c                   eval      *in69  = *off
018980060524      *
018990070123      * LDV con mittente "PraOil oleodotti italiani"
019000081127w   1c                   WHEN           DB0ccm = c_CliPraOil
019010060524     c                             or  (DB0fbr = 'S'
019020081127     c                             and  §B4Aks = c_CliPraOil)
019030060524     c                   eval      *in69  = *off
019040090302      /free
019050090302
019060090302           // LDV con mittente con promemoria per AUT in tab. "PXA"
019070110411           WHEN  $TabPXA = *on  or  $TabPXA2 = *on;
019080090302             *in69  = *off;
019090090924
019100090924           // LDV con mittente con promemoria per destinatario
019110090924           WHEN  $BoxNxD = *on;
019120090924             *in69  = *off;
019130090302      /end-free
019140030515      *
019150120530      * LDV BRT
019160150304     *** * - con Bancali da rendere al mittente
019170150304     *** *   o   Bancali da riportare a magazzino
019180150304      * - con Destinatario supermercato
019190030515      *   e/o Promemoria per autotrasportatore
019200040223      *   [Se non trovo niente su FIAR5 né x supermercati né x bancali
019210030515      *    ma ho queste condizioni: devo stampare la bolla in "A4" per
019220030515      *    poter stampare comunque il box vuoto con la memo "Destina-
019230050510      *    tario Supermercato" per poter scrivere a mano i promemoria
019240050510      *    per l'autotrasportatore - ci dovrebbero comunque sempre
019250050510      *    essere note per supermercato.]
019260050510      *   e/o con formato A4 previsto in tab. "7R" (partic. consegna)
019270150304w   1c*//                WHEN         %subst(DB0gva:1:1) = 'B'
019280150304     c*//                          or %subst(DB0gva:1:1) = 'O'
019290150304w   1c                   WHEN                DB0tc1      = 'S'
019300030717     c                             or        DB0tc2      = 'S'
019310071015     c                             or        §7Rldv      = 'S'
019320030515     c                   eval      *in69  = *off
019330071015      *
019340071015      * LDV con ritiro porto franco prepagato
019350071015w   1c                   WHEN          (DB0tbo   = '8'
019360071015     c                             and  §B4poe   > *zeros
019370071015     c                             and  §B4nor   > *zeros)
019380071015     c                   eval      *in69  = *off
019390071015      *
019400071015      * LDV con ritiro contestuale alla consegna
019410100608w   1c**!!!              WHEN          (DB0gma   = Ktab7V_RC
019420100608w   1c                   WHEN          (§7Rrc    = 'S'
019430071015     c                             and  §7Rldv   = 'S')
019440071015     c                   eval      *in69  = *off
019450030515      *
019460030515      * - con Forzatura richiesta al formato "A4"
019470030515w   1c                   WHEN      DB0fa4 = 'S'
019480030515     c                   eval      *in69  = *off
019490030515      *
019500030515e   1c                   ENDSL
019510050513      *
019520050513      * Verifica se con Segnacolli NON sequenziali
019530050513      * (di numero superiore a quelli che stanno al posto del range)
019540050513      * DA CONTROLLARE COMUNQUE (A4 o A5 che sia), VISTO CHE:
019550050513      * - se è previsto un A5, potrebbe scattare l'A4 (per numero
019560050513      *   di segnacolli da elencare troppo alto per lo spazio
019570050513      *   disponibile tra le "NOTE E PARTICOLARITA'")
019580050513      * - se è già previsto un A4, i segnacolli non sequenziali
019590050513      *   potrebbero comunque essere elencabili tra le "NOTE E
019600050513      *   PARTICOLARITA'" (e risparmiare una pagina in caso di
019610050513      *   LdV con DDT del mittente)
019620050513     c*** (già fatto:)   reset                   $A4_x_ScNS
019630050513if  1c                   IF             DB0fns = 'S'
019640050513     c                             and  DB0ncl >  2
019650050513      * - - imposto le righe di "Note e Particolarità" da stampare
019660050513     c                   exsr      SchPARTIC
019670050513      * - - controllo quante righe di "Note e Particolarità" servireb-
019680050513      *     bero per segnalare i segnacolli non sequenziali, in base
019690050513      *     al numero di colli (considerando che riuscirei ad elencare
019700050513      *     max 4 segnacolli per ogni riga di "Note e Particolarità")
019710050513     c                   eval      WelSNS = $elSNSxA5
019720050513     c     DB0ncl        div       WelSNS        YY
019730050513     c                   mvr                     XX
019740050513if  2c                   if        XX     >  0
019750050513     c                   eval      XX     =  YY + 1
019760050513x   2c                   else
019770050513     c                   eval      XX     =  YY
019780050513e   2c                   endif
019790050513      * - - conto anche la riga per l'intestazione
019800050513     c                   add       1             XX
019810050513      * - - conteggio le righe di "Note e Particolarità" utilizzate
019820050513      *     per reperire la 1ª riga di "Note e Particolarità" disponi-
019830050513      *     bile
019840050513     c                   eval      *in31  =  *off
019850050513     c                   eval      YY     =  1
019860050513     c     *blanks       lookup    $NP(yy)                                31
019870050513if  2c                   if        *in31  =  *on
019880050513     c                   eval      YY     =  YY - 1
019890050513x   2c                   else
019900050513     c                   eval      YY     =  %elem($NP)
019910050513e   2c                   endif
019920050513      * - - controllo se ho il numero di campi "Note e Particolarità"
019930050513      *     liberi a sufficienza per i segnacolli da elencare.
019940050513      *     Se non ci stanno => serve un modulo A4
019950050513if  2c                   if        XX > %elem($NP) - YY
019960050513     c                   eval      $A4_x_ScNS  = *on
019970050513     c                   eval      *in69  = *off
019980050513e   2c                   endif
019990050513e   1c                   ENDIF
020000030515      *
020010030515      * Imposto il numero di riga massima raggiungibile per l'eventuale
020020030515      *  stampa dei segnacolli non sequenziali
020030040607if  1c                   if        *in69
020040030515     c                   eval      wMaxLine = $MaxLine1
020050040607x   1c                   else
020060030515     c                   eval      wMaxLine = $MaxLine2
020070040607e   1c                   endif
020080030515      * Memo: ho stampato (sto stampando...) almeno un modulo A4 o A5.
020090030515      *   (utile solo per la segnalazione in stampa delle oltre 6.000
020100071008      *   pagine già stampate - in un formato e/o nell'altro).
020110080926      /free
020120080926         select;
020130100924           //?»»» Richiesta la stampa per PDA (4 LdV x A4)?
020140100924           //    ?di LdV per consegna AL 1° tentativo, ma?
020150100924           //    ?reperita packing-list (da stampare "a parte")?
020160100924           //?(la stampa della relativa packing-list verrà richiesta?
020170100924           // ?successivamente, dopo la stampa di tutte le LdV)?
020180100924           when  DB0fa4  = '4'    and
020190100924               //DB0fl1 <> 'D'    and
020200100924                 *in69   = *off   and
020210130306                 $FIAR8  = *on    and
020220130306                 $§PDF   = *off;
020230100924             DB0tms = 'D';
020240130306           //?»»» Richiesta la stampa per PDA (4 LdV x A4)?
020250130306           //    ?di LdV per consegna AL 1° tentativo, ma?
020260130306           //    ?reperita immagine PDF (da stampare "a parte")?
020270130306           //?(la stampa delle relative immagini verranno richieste?
020280130306           // ?successivamente, dopo la stampa di tutte le LdV e?
020290130306           // ?delle relative packing-list)?
020300130306           when  DB0fa4  = '4'    and
020310130306                 $FIAR8  = *off   and
020320130306                 $§PDF   = *on;
020330130307             DB0tms = 'i';
020340130306           //?»»» Richiesta la stampa per PDA (4 LdV x A4)?
020350130306           //    ?di LdV per consegna AL 1° tentativo, ma?
020360130306           //    ?reperita packing-list (da stampare "a parte") e?
020370130306           //    ?immagine PDF (anch'essa da stampare "a parte")?
020380130306           //?(la stampa della relativa packing-list e delle relative?
020390130306           // ?immagini verranno richieste successivamente, dopo la?
020400130306           // ?stampa di tutte le LdV)?
020410130306           when  DB0fa4  = '4'    and
020420130306               //DB0fl1 <> 'D'    and
020430130306                 *in69   = *off   and
020440130306                 $FIAR8  = *on    and
020450130306                 $§PDF   = *on;
020460130306             DB0tms = 'Y';
020470100924           //?»»» Richiesta la stampa per PDA (4 LdV x A4)?
020480080926           when  DB0fa4 = '4';
020490100927             DB0tms = 'Q';
020500100924           //?»»» Richiesta la stampa della packing-list per PDA?
020510081007           when  DB0fa4 = 'P'   and   $FIAR8 = *on;
020520080926             DB0tms = 'P';
020530081007           when  DB0fa4 = 'P'   and   $FIAR8 = *off;
020540081007             //DB0tms = *blank;
020550130307           //?»»» Richiesta la stampa normale e reperita immagine PDF?
020560130307           when  *in69  and  $§PDF;
020570130307             DB0tms = 'I';
020580130307           when  Not *in69  and  $§PDF;
020590130307             DB0tms = 'J';
020600100924           //?»»» Richiesta la stampa normale?
020610080926           when  *in69;
020620080926             DB0tms = '5';
020630080926           other;
020640080926             DB0tms = '4';
020650080926         endsl;
020660080926      /end-free
020670040707      *
020680040707      * Controllo se devo stampare "Non utilizzare per la consegna"
020690040707      * nella copia per il vettore,?senza?copia per il destinatario:
020700040707     c*** (già fatto:)   reset                   $No_x_Cons
020710071203      * - LdV recapito c/assegni:
020720090511      *   stampata in arrivo e con
020730120113if  1c                   if             DB0tbo <> 'P'
020740070306     c                             and  DB0tbo <> '8'
020750090511     c                             and  DB0lnp =  102
020760070306     c                             and  DB0cbo =  'FW'
020770071203     c                             and  DB0nft =  *zeros
020780071203      *   - fil. arrivo con GEO attivato
020790071203     c                             and (DB0fl1    =  'A'
020800071203     c                              or  wGeoCon   =  'S'
020810071203      *   - fil. arrivo abilitata al picking
020820120113     c                              or  A§OGpck   =  'S')
020830070306     c                   eval      $No_x_Cons  =  *on
020840120113e   1c                   endif
020850120113      *//* - LdV Poste
020860120113w   1c*//                   when      §OGntw   = 'PPT'
020870120113     c*//                   eval      $No_x_Cons  =  '2'
020880101125      /free
020890101125         // -?Controlla se "tagliatella" per Ritorno all'Incasso?
020900101129         //$Rit_a_Inc = ( DB0fa4  = '4'    and
020910101129         //               DB0tbo <> 'P'    and  DB0tbo <> '8'   and
020920101129         //               wB0dcm  > *zero  and
020930101129         //           ( ( DB0cas >  *zero  and  wB0icc =  'R' )  or
020940101129         //             ( DB0ift >  *zero  and  wB0ica =  'R' ) ) );
020950101129         // -?Controlla se LdV per Ritorno all'Incasso?
020960101129         $Rit_a_Inc = ( DB0fl4 <> *blank and
020970101129                        DB0tbo <> 'P'    and  DB0tbo <> '8'   and
020980101129                        wB0dcm  > *zero  and
020990101129                    ( ( DB0cas >  *zero  and  wB0icc =  'R' )  or
021000101129                      ( DB0ift >  *zero  and  wB0ica =  'R' ) ) );
021010101125      /end-free
021020030515      *
021030030515     c                   ENDSR
021040040607      *
021050040607      *---------------------------------------------------------------*
021060130315      *?Verifica esistenza dati del DDT allegato da stampare  o      ?*
021070130315      *?               di uno o più PDF allegati da stampare         ?*
021080040607      *---------------------------------------------------------------*
021090130315     c     CercaDDToPDF  BEGSR
021100040607      *
021110060921     c*** (da non fare:) reset                   $E_AR8
021120060921     c*** (già fatto:)   reset                   $FIAR8
021130130315     c*** (già fatto:)   reset                   $§PDF
021140040607     c*** (già fatto:)   reset                   $DDT_o
021150040607     c*** (già fatto:)   reset                   $DDT_p
021160040608     c*** (già fatto:)   reset                   $DDTlastPg
021170040607     c*** (già fatto:)   reset                   $AR8o1
021180040608     c*** (già fatto:)   reset                   wOmaRowT
021190040608     c*** (già fatto:)   reset                   wOmaRowD
021200041207     c*** (già fatto:)   reset                   dAR8
021210130315      *
021220130315      * -?Verifica esistenza PDF allegato?
021230130315     c                   eval      $§PDF  = (§AR5als = 'S')
021240130315      *
021250130315      * -?Verifica esistenza packing-list?
021260130315     c                   eval      $FIAR8 = (§AR5alx = 'S')
021270130315      *
021280130315if  1c                   if        NOT %open(FIAR801L)
021290130315     c                             and ($FIAR8  or  $§PDF)
021300130315     c                   open(e)   FIAR801L
021310130315      * Se errore significa che non c'è il file per cui non faccio
021320130315      *  più questa routine
021330130315if  2c                   if        %error
021340130315     c                   eval      $E_AR8 = *off
021350130315e   2c                   endif
021360130315e   1c                   endif
021370040607      *
021380130306     c                   ENDSR
021390061102      *
021400061102      *---------------------------------------------------------------*
021410071114      *?Verifica se Codice Fiscale e/o Partita Iva da richiedere al  ?*
021420071114      *?  cliente                                                    ?*
021430061102      *---------------------------------------------------------------*
021440061102      *  Il d.l. 223/06, poi convertito con la legge 248/06, ha      *
021450061102      *   reintrodotto l'obbligo di raccogliere il codice fiscale per *
021460061102      *   tutti i clienti e fornitori, siano essi aziende o privati.  *
021470061102      * SE?Porto Franco PrePagato?:                                   *
021480061102      *   il Cod.Fiscale dovrà essere del?Mittente?.                  *
021490061102      * SE?Porto Assegnato con Fattura Immediata?:                    *
021500061102      *   il Cod.Fiscale dovrà essere del?Destinatario?.              *
021510061102      * In sua assenza: dovrà essere richiesto al cliente stesso!     *
021520071114      *  Lo stesso decreto (poi convertito con la legge 248/06)      *
021530071114      *   prevede che a partire dal 01/01/2008 debba essere raccolta  *
021540071114      *   anche l'eventuale partita iva, del cliente o del fornitore. *
021550071114      * Stessi criteri già utilizzati per il codice fiscale.          *
021560061102      *---------------------------------------------------------------*
021570071114     c     CheckCFPI     BEGSR
021580061102      *
021590061106     c*** (già fatto:)   eval      $No_CF  = *off
021600071114     c*** (già fatto:)   eval      $No_PI  = *off
021610061102      *
021620071114      * Verifica se C.F./P.I. da richiedere (NON per clienti esteri)
021630071114      * - con fattura
021640071114if  1c                   IF            (DB0nft  <> *zeros
021650061106     c                              or  DB0ift  <> *zeros)
021660061103     c                             AND (
021670061102      * - - P.F. prepagato
021680061107      *     (con mittente italiano)
021690061107     c                                 (DB0tbo  =  '8'
021700061107     c                              and DB0nzm  =  *blanks)
021710061106      * - - P.A. (con fattura immediata)
021720061107      *     (con destinatario italiano)
021730061107     c                             or  (%subst(DB0tb1:1:1) = 'A'
021740061107     c                              and DB0nzd  =  *blanks)
021750061106      * - - con 2ª bolla
021760061107      *     (con destinatario italiano)
021770061103     c                             or  (DB0tb2  <> *blanks
021780121029     c                              and DB0tbo  <> 'P'
021790061107     c                              and DB0tbo  <> '8'
021800061107     c                              and DB0nzd  =  *blanks)
021810071114      * - => verirficare se da richiedere C.F. e/o P.I.
021820061103     c                                 )
021830120503      *
021840120503sel 2c                   select
021850120503      * · Esiste Partita Iva NON FITTIZIA: non chiedere nulla
021860120503w   2c                   when      wB0piv <> *blanks   and
021870120503     c                             %scan('$$' : wB0piv) = *zero
021880120503     c                   leavesr
021890120503      * · Esiste C.F. lungo 16 (privato) : non chiedere P.I.
021900120503w   2c                   when      %len( %trim( wB0cfi ) ) = 16
021910120503     c                   leavesr
021920120503      * · Esiste C.F. lungo 11 ..........: chiedere P.I. (se mancante)
021930120503w   2c*//                when      %len( %trim( wB0cfi ) ) = 11
021940120503e   2c                   endsl
021950071114      *
021960071114      * - senza codice fiscale
021970120503     c                   eval      $No_CF  = (wB0cfi = *blanks)
021980071114      *
021990071114      * - senza partita iva (se NON privato)
022000120503     c                   eval      $No_PI  = (wB0piv = *blanks)
022010071114      *
022020071114e   1c                   ENDIF
022030061102      *
022040061102     c                   ENDSR
022050070306      *
022060070306      *---------------------------------------------------------------*
022070070306      *?Verifica esistenza note da stampare (dal file FIARN00F)      ?*
022080070306      *---------------------------------------------------------------*
022090070306     c     CheckFIARN    BEGSR
022100070306      *
022110070306     c*** (già fatto:)   reset                   $FIARN
022120070306      *
022130070306      * Controllo esistenza note solo se c'è il file
022140070319     c     K06ARN11      chain     FIARN000
022150070319if  1c                   if             %found(FIARN11L)
022160070306     c                             and  ARNnob <> *blanks
022170070306     c                   eval      $FIARN = *on
022180070306e   1c                   endif
022190070306      *
022200070306     c                   ENDSR
022210070315      *
022220070315      *---------------------------------------------------------------*
022230070315      *?Verifica esistenza C.A. aperta legata alla spedizione        ?*
022240070315      *---------------------------------------------------------------*
022250070316     c     CercaCA       BEGSR
022260070315      *
022270070319     c*** (già fatto:)   reset                   $C_A_
022280070315      *
022290070319      * Richiamo del programma di comodo per recuperare le CA legate
022300070319      *   alla spedizione in esame
022310070319     c                   reset                   FIDN12ds
022320070514if  1c                   if        $Mamma = *off
022330070319     c                   eval      I12aas = DB0aas
022340070319     c                   eval      I12lnp = DB0lnp
022350070319     c                   eval      I12nrs = DB0nrs
022360070319     c                   eval      I12nsp = DB0nsp
022370070514x   1c                   else
022380070514     c                   eval      I12aas = LBLaap
022390070514     c                   eval      I12lnp = LBLlpp
022400070514     c                   eval      I12nrs = LBLnrp
022410070514     c                   eval      I12nsp = LBLnsp
022420070514e   1c                   endif
022430070319     c*** già così:      eval      I12fel = 'E'
022440070319     c***  "    "        eval      I12fan = 'E'
022450070319     c***  "    "        eval      I12fch = 'E'
022460070319      *
022470070319     c                   call      'FIDN12R'
022480070319     c                   parm                    FIDN12ds
022490070319      *
022500070319      * Se trovata almeno una CA
022510070319     c                   eval      $C_A_  = (O12nca > *zeros)
022520070315      *
022530070316     c                   ENDSR
022540070319      *
022550070319      *---------------------------------------------------------------*
022560070319      *?Verifica se stampa spedizione DOPO "Lasciato Avviso"         ?*
022570070319      *---------------------------------------------------------------*
022580070319     c     CheckLascAvv  BEGSR
022590070319      *
022600070319     c*** (già fatto:)   reset                   $LascAvv
022610070319      *
022620070319if  1c                   if        NOT  %open(FNEVB05L)
022630070319     c                   open      FNEVB05L
022640070319e   1c                   endif
022650070319      *
022660070319     c     KLDV          setgt     FNEVB000
022670070319     c     KLDV          readpe    FNEVB000
022680070319      *
022690070319      * Proseguo solo se ritrovo un evento
022700070319do  1c                   DOW       NOT  %eof(FNEVB05L)
022710070319      *
022720070319      * Verifico se evento di tipo Giacenza
022730070319     c                   eval      *in31    = *off
022740070319     c     EVBcev        lookup    $2Acev_G                               31
022750070319      **
022760070319sel 2c                   Select
022770070319      * - Evento GIACENZA --> NO LASCIATO AVVISO
022780070319w   2c                   When      *in31
022790070319     c                   leave
022800070319      * - Evento RIC --> 1° LASCIATO AVVISO
022810070319w   2c                   When      EVBcev   = 'RIC'
022820070319     c                   eval      $LascAvv = *on
022830140307      * - - salvataggio data/ora
022840140307     c                   eval      wEVBdev  = EVBdev
022850140307     c                   eval      wEVBhev  = EVBhev
022860070319     c                   leave
022870070319      * - Evento DA NON ELABORARE
022880070319x   2c                   Other
022890070319     c     KLDV          readpe    FNEVB000
022900070319e   2c                   Endsl
022910070319      **
022920070319e   1c                   ENDDO
022930070319      *
022940070319     c                   ENDSR
022950090302      /free
022960090302
022970090302       //--------------------------------------------------------------
022980090302       //?Verifica esistenza note da stampare (dal file TNTBE00F)      ?
022990090302       //--------------------------------------------------------------
023000090302       BEGSR  CheckTabPXA;
023010090302
023020090302         //clear  $TabPXA;          (già fatto)
023030110411         //clear  $TabPXA2;         (già fatto)
023040110411
023050110411         // -?Verifica esistenza "PXA" per particolarità consegna?
023060110411         If  DB0gma <> *blank;
023070170605           clear  k05tntbe01;
023080110411           k_TBEcod = 'PXA';
023090110411           k_TBEke1 = DB0gma;
023100110411           k_TBEke2 = '001            ';
023110110411           setll  %kds(k05tntbe01 : 3)  TNTBE000;
023120110411           // -?Bisogna testare se nota ANNULLATA?
023130110411           //$TabPXA  = %equal(TNTBE01L);
023140110411           reade  %kds(k05tntbe01 : 2)  TNTBE000;
023150110411           $TabPXA2 = (Not %eof(TNTBE01L)   and   TBEatb = *blank
023160110411                       and TBEke2 <= '006            ');
023170110411         EndIf;
023180090302
023190110411         // -?Reperimento eventuali "PXA" per mittente?
023200170605         clear  k05tntbe01;
023210090302         k_TBEcod = 'PXA';
023220090302         k_TBEke1 = %editc(DB0ccm : 'X');
023230100316         k_TBEke2 = '001            ';
023240090302         clear  k_TBElin;
023250090302         clear  k_TBEsif;
023260100316         setll  %kds(k05tntbe01 : 3)  TNTBE000;
023270110411         // -?Bisogna testare se nota ANNULLATA?
023280090302         //$TabPXA  = %equal(TNTBE01L);
023290090302         reade  %kds(k05tntbe01 : 2)  TNTBE000;
023300100316         $TabPXA  = (Not %eof(TNTBE01L)   and   TBEatb = *blank
023310100316                     and TBEke2 <= '006            ');
023320090302
023330090302       ENDSR;
023340090302      /end-free
023350030515      *
023360030515      *---------------------------------------------------------------*
023370050217      *?Imposta i campi di stampa con quelli della DS                ?*
023380030515      *---------------------------------------------------------------*
023390030515     c     PrepPRT       BEGSR
023400040223      *
023410040223      * Reperimento dati da FIAR5 / "GEN"
023420130315     c*** (già fatto:)   exsr      RepAR5gen
023430070212     *** *
023440070212     *** * Impostazione Data spedizione
023450070212     *** * (già fatto nella subr. "TipoLdV")
023460070319      *
023470100218      * CONTROLLO SE STAMPA SPEDIZIONE DOPO "LASCIATO AVVISO"
023480100218      * (?*-=»>   SE NON MESSA IN "FERMO DEPOSITO"   <«=-?):
023490081118      * (NON se richiesta stampa di "ricevuta" - 4 in A4)
023500110427      * per LNA (o capofila se LNA è in £6) con $GeoAtt = *ON =>
023510070717      * GEO attivato completamente
023520070319      * controllo se la spedizione ha già più tentativi di consegna e
023530070319      *   determino se in stato di "lasciato avviso"
023540070319      *
023550070319     c*** (già fatto:)   reset                   $LascAvv
023560070406if  1c                   if             DB0tbo    =  'A'
023570081117     c                             and  DB0fa4    <> '4'
023580070514     c                             and  wB0ntc    >  *zeros
023590110427     c                             and  $GeoAtt   =  *on
023600100218     c                             and  DB0ffd    =  *blank
023610070319     c                   exsr      CheckLascAvv
023620070319e   1c                   endif
023630030515      *
023640120530      * Impotazione dati BRT S.p.A.
023650120530     c*//                if        (DB0dft > *zero       and
023660120530     c*//                           DB0dft > §vpoBRTdta)  OR
023670120530     c*//                          (DB0dft = *zero       and
023680120530     c*//                           Wdsp   > §vpoBRTdta)
023690120530     c                   eval      TBAR   =  $BRT
023700120530     c*//                else
023710120530     c*//                eval      TBAR   =  $Bartolini
023720120530     c*//                endif
023730070307     c                   eval      TBA1   =  $Intest1
023740070307     c                   eval      TBA2   =  $Intest2
023750080707if  1c                   if            DB0nft <> *zeros
023760080707     c                             or  DB0ift <> *zeros
023770080627     c                   eval      TBA3   =  $Intest3
023780080707e   1c                   endif
023790070307     c                   eval      TBA4   =  $Intest4
023800070307     c                   eval      TBA5   =  $Intest5
023810070307     c*** NON sopra:     eval      TBA6   =  $Intest6
023820120530     c*//                eval      TBA7   =  $Intest7
023830120530     c                   eval      TBA7   =  $Intest7_BRT
023840030515      *
023850030515      * - Intestazione moduli
023860080919     c                   eval      Tcxd   =  c_CopiaXdest
023870130729     c*//                if        Not *in36     (NON ancora acceso)
023880130729if  1c                   if        DB0tbo <> '8'  or  DB0ris = 'M'
023890130729     c                   eval      Tcxd2  =  Tcxd
023900130729     c                   endif
023910091008      *
023920091008      /free
023930091008         select;
023940091008           // - Indicazione Copia LDV x cliente
023950091008           when  DB0ris = 'C';
023960091008             Pkco = 'C O P I A';
023970091008           // - Altre indicazioni: R=Ristampa, V=Variazione, ...
023980091008           other;
023990091008             Tkri = DB0ris;
024000091008         endsl;
024010091008      /end-free
024020030515      *
024030030515      * - Codice a barre: ANNO+LNP+NRS+NSP+CKD
024040030515      *   (chiamo il pgm TRUL28R per ricavarne il check digit)
024050030515     c                   reset                   TRUL28ds
024060030515     c                   z-add     DB0aas        BARcdeAAS
024070130710     c*//                eval      BARcdeLNP = DB0lnp
024080130710     c                   eval      BARcdeLNP = Prm_LNP
024090130710     c*//                eval      BARcdeNRS = DB0nrs
024100130710     c                   eval      BARcdeNRS = Prm_NRS
024110030515     c                   eval      BARcdeNSP = DB0nsp
024120030515     c                   movel     BarCodeDS     I28cod
024130030515     c                   call      'TRUL28R1'
024140030515     c                   parm                    TRUL28DS
024150030515if  2c                   if        O28err =  *blanks
024160030515     c                   movel     O28cod        Tbcd
024170030515e   2c                   endif
024180140917      *
024190140917      /free
024200140917
024210140917         // -?BRTcode?
024220140917         reset  TRUL28ds0;
024230140917         trul28ds0.i280cod = BarCodeDS;
024240140917         trul28r0 ( TRUL28ds0 );
024250140917         if  trul28ds0.o280err <> *on;
024260140917           //TbrtC = trul28ds0.o280cod;
024270140917           TbrtC = %subst( trul28ds0.o280cod :  1 : 5 ) + ' ' +
024280140917                   %subst( trul28ds0.o280cod :  6 : 5 ) + ' ' +
024290140917                   %subst( trul28ds0.o280cod : 11 : 5 ) + ' ' +
024300140917                   %subst( trul28ds0.o280cod : 16 : 4 );
024310140917         endif;
024320140917
024330140917      /end-free
024340030515      *
024350030515      * - Sigla operatore video
024360030515     c                   eval      Psop   =  DB0sop
024370080721      /free
024380080721
024390130306         // -?Flag "allega documento di trasporto"?
024400130418         //   IN FUTURO: se al 1° tentativo di consegna
024410130418         //              (DB0FL1 = "D" - v. pgm FNLRS9R)
024420080721         select;
024430081119
024440130306           // -?LdV con DDT allegato oltre il 1° tentativo di consegna?
024450081127           when  DB0bam =  'S'   and   wB0tmc <> *blank;
024460081127             evalr Pddt =  'DDT sul collo';
024470081119
024480130306           // -?LdV con DDT allegato (al 1° tentativo di consegna)?
024490080723           when  DB0bam =  'S';
024500080723             evalr Pddt =  'DDT';
024510081119
024520130306           // -?Esiste packing-list abbinata?
024530130306           when  $FIAR8 =  *on   and   $§PDF =  *off;
024540081013           //if  DB0fl1 <> 'D';
024550081013               evalr Pddt =  'PACKING-LIST';
024560081013           //else;
024570081013           //  evalr Pddt =  'PACKING-LIST GIA'' CONSEGNATA';
024580081013           //endif;
024590130306
024600130306           // -?LdV con PDF allegato?
024610130306           when  $FIAR8 =  *off  and   $§PDF =  *on;
024620130306             evalr Pddt =  'PACKING-LIST PDF';
024630130306
024640130306           // -?LdV con packing-list abbinata & PDF allegato?
024650130306           when  $FIAR8 =  *on   and   $§PDF =  *on;
024660130308             evalr Pddt =  'PACKING-LIST PDF';
024670081119
024680130306           // -?LdV senza PDF allegato & senza packing-list abbinata?
024690080721           other;
024700080721             clear Pddt;
024710081119
024720080721         endsl;
024730080721
024740080721      /end-free
024750030515      *
024760030515      * - Mittente
024770170605     c                   if        *in71
024780170605     c                   exsr      sr_Mitt_DPD
024790170605     c                   endif
024800170605      *
024810030515     c                   if        DB0ccm <> *zeros
024820030515     c                   move      DB0ccm        Pccm
024830030515     c                   endif
024840030515     c                   eval      Prsm   =  DB0rsm
024850030515     c                   eval      Pinm   =  DB0inm
024860030515     c                   eval      Pcam   =  DB0cam
024870030515     c                   eval      Plom   =  DB0lom
024880030515     c                   eval      Pprm   =  DB0prm
024890030515     c                   eval      Pnzm   =  DB0nzm
024900030515      *   Mittente originale
024910030515if  1c                   if        DB0rmo <> *blanks
024920030515     c                   eval      Pkmo   =  'MITT. ORIGINALE'
024930030515     c                   eval      Prmo   =  DB0rmo
024940030515e   1c                   endif
024950030515      *
024960070308      * - Filiale di Arrivo
024970030515if  1c                   if        DB0ris <> 'M'
024980080721     c                   eval      Pkla   =  'FILIALE'
024990170717     c                   eval      Plna1  =  %editc(wDB0lnaCf£6 : 'X')
025000080722     c                                    +  ' '
025010170717     c                                    +  wDB0dlaCf£6
025020080722     c                                    +  ' - '
025030170717     c                                    +  wDB0inlCf£6
025040140307     c*//                                 +  ' Tel.'
025050140307     c                                    +  '     '
025060170717     c                                    +  %trimr(wDB0telCf£6)
025070170717     c                   eval      Plna2  =  %editc(wDB0calCf£6 : 'X')
025080080722     c                                    +  ' '
025090170717     c                                    +  wDB0lolCf£6
025100080722     c                                    +  ' '
025110170717     c                                    +  wDB0prlCf£6
025120140307     c*//                                 +  '                  '
025130140307     c*//                                 +  ' Fax '
025140170717     c*//                                 +  %trimr(wDB0faxCf£6)
025150170717     c                   if        wDB0faxCf£6 <> *blanks
025160170717     c     c_Mai:c_Min   xlate     wDB0faxCf£6   DB0fax
025170140307     c                   eval      Plna2  =  %trimr(Plna2)
025180140307     c                                    +  '          '
025190140307     c*//*                                +  ' E-mail: '
025200140307     c                                    +  '         '
025210170717     c                                    +  %trimr(wDB0faxCf£6) + '@brt.it'
025220140307     c                   endif
025230030515e   1c                   endif
025240030515      *
025250030515      * - Destinatario
025260030515if  1c                   if        DB0ccd <> *zeros
025270030515     c                   move      DB0ccd        Pccd
025280030515e   1c                   endif
025290030515     c                   eval      Prsd   =  DB0rsd
025300030515     c                   eval      Prse   =  DB0rd2
025310030515     c                   eval      Pind   =  DB0ind
025320030515     c                   eval      Pcad   =  DB0cad
025330030515     c                   eval      Plod   =  DB0lod
025340030515     c                   eval      Pprd   =  DB0prd
025350030515     c                   eval      Pnzd   =  DB0nzd
025360030515      *
025370030515      * - Riferimenti mittente
025380030515     c                   eval      Prmn   =  DB0rmn
025390030515     c                   eval      Prma   =  DB0rma
025400171117      *//* se bolla DPD recupero il riferimento da FIAR4 rcd "I"
025410171117      * se bolla DPD recupero il riferimento da FIPND00F
025420171117     c*//*** (già fatto:)   clear                   dsBL4I
025430060626     c                   if        *in71
025440171117     c*//                exsr      RepAR4_I
025450171117     c                   exsr      RepFIPND
025460060626     c                   endif
025470030515      *
025480030515      * - Nominativo & telefono
025490031124      *   (da FIAR5 / "GEN")
025500040607     c*** (già fatto:)   exsr      RepAR5gen
025510031124     c                   eval      Pnom   =  §AR5ref
025520031124     c                   eval      Ptel   =  §AR5teld
025530030515      *
025540030515      * - Spedizione
025550030515      *   (numero: non se Rimesso al Mittente)
025560030515if  1c                   if        DB0ris <> 'M'
025570030515     c                   eval      Plnp   =  DB0lnp
025580030515     c                   eval      Pnrs   =  DB0nrs
025590030515     c                   eval      Pnsp   =  DB0nsp
025600030515e   1c                   endif
025610030515      * . . Data spedizione
025620031124     c     *iso          movel     Wdsp          Data_EUR
025630130418     c     *eur          movel     Data_EUR      Pdsp
025640140923     c     *dmy          movel     Data_EUR      Pdsp6
025650030515      *
025660030515      * - Colli: numero, peso, volume, flag tipo-volume
025670030515     c                   eval      Pncl   =  DB0ncl
025680030515     c                   eval      Ppkf   =  DB0pkf
025690030515     c                   eval      Pvlf   =  DB0vlf
025700030515if  1c                   if            DB0fvf <> *blanks
025710030515     c                             and DB0ris <> 'M'
025720030717     ***c***                eval      Pfvf   =  '-' + DB0fvf + '-'
025730030717     c                   move      DB0fvf        Pfvf
025740030515e   1c                   endif
025750030515      *
025760030515      * - Natura merce
025770030515     c                   eval      Pnat   =  DB0nas
025780030515      *
025790030515      * - Servizio
025800100513      /free
025810100513         if  %subst(DB0tb1:1:1) = 'A'   or   DB0tb2 <> *blanks;
025820100513           exsr  sr_CheckORM_PrevvMail;
025830100513         endif;
025840100513      /end-free
025850040607      * . . Descrizione LDV
025860100513     c                   if        Not  $PreavvORM
025870030515     c                   eval      Pds1   =  DB0dcb
025880100513     c                   endif
025890030515      * . . Descrizione tipo servizio
025900030515if  1c                   if        DB0fst =  'S'
025910030515     c                   eval      Pds2   =  DB0dts
025920030515e   1c                   endif
025930030515      * . . Reso / Franco frontiera
025940030515sel 1c                   select
025950030515w   1c                   when      DB0fbr =  'S'
025960030515     c                   clear                   w008a
025970030515     c                   eval      w008a  =  '   RESO '
025980030515     c                   move      w008a         Pds2
025990030515w   1c                   when      DB0fbr =  'F'
026000030515     c                   clear                   w015a
026010030515     c                   eval      w015a  =  '   FR.FRONTIERA'
026020030515     c                   move      w015a         Pds2
026030030515e   1c                   endsl
026040030515      *
026050030515      * - Zona consegna
026060030515     c                   eval      Pznc   =  DB0znc
026070030515      *
026080030515      * - Flag "*" (gestito a parte)
026090040223      *
026100040223      * - Flag "V" = colli di valore
026110040223if  1c                   if        §AR5bva   <> *blank
026120040223     c                   eval      Pbva   =  'V'
026130040223e   1c                   endif
026140060921      *
026150070212      * - Codice Giro o Numero Distinta (se tipo bolla "ARRIVO")
026160071015if  1c                   IF              DB0tbo = 'A'
026170071015     c                             and  wGeoCon = 'S'
026180070212sel 2c                   Select
026190070212      * · - Stampa LdV in arrivo con codice giro (parametro passato)
026200071015w   2c                   When      DB0fl1     =  'A'
026210070215if  3c                   if        §DB0tgi    =  'M'
026220101028     c                   eval      Pndc   =  'g:' + §DB0cgi + 'm'
026230070215x   3c                   else
026240101028     c                   eval      Pndc   =  'g:' + §DB0cgi
026250070215e   3c                   endif
026260070212      * · - Stampa LdV con numero distinta consegna
026270071015w   2c                   When           wB0ndc  > *zeros
026280110427     c                             and  $GeoAtt =  *on
026290101028     c                   eval      Pndc   =  'd:'
026300070514     c                                    +  %trim(%editc(wB0ndc:'Z'))
026310101028if  3c                   if        %len(Pndc)  >= %len(%trimr(Pndc))
026320071016     c                                          + 3 + %len(Wpdc4a)
026330101028     c                   eval      Pndc   =  %trimr(Pndc)
026340071022     c                                    +  ' a:'
026350071022     c                                    +  Wpdc4a
026360071016e   3c                   endif
026370070212      * · - Stampa LdV con codice giro (da reperire in FIARG00F)
026380070213w   2c                   Other
026390070212if  3c                   if        NOT %open(FIARG01L)
026400070212     c                   open      FIARG01L
026410070212e   3c                   endif
026420070215     c                   clear                   ARGcgi
026430070215     c                   clear                   ARGtgi
026440070212     c     KLDV          chain     FIARG000
026450070215if  3c                   if        ARGtgi =  'M'
026460101028     c                   eval      Pndc   =  'g:' + ARGcgi + 'm'
026470070215x   3c                   else
026480101028     c                   eval      Pndc   =  'g:' + ARGcgi
026490070215e   3c                   endif
026500070215e   2c                   EndSl
026510070212e   1c                   ENDIF
026520030515      *
026530030515      * - Fermo deposito
026540030515if  1c                   if        DB0ffd <> *blanks
026550060921     c                   eval      Pffd   =  'FERMO DEPOSITO '
026560030515e   1c                   endif
026570030515      *
026580070308      * - Segnacolli: filiale, serie, dal / al
026590030515if  1c                   if        DB0fls <> *zeros
026600030515     c                   move      DB0fls        Ppos
026610030515e   1c                   endif
026620040607      * - - se bolla con mamma considero la serie della mamma
026630070515if  1c                   if        $Mamma =  *on
026640031015     c                   eval      Psrs   =  wB0nrs
026650031015x   1c                   else
026660030515     c                   eval      Psrs   =  DB0nrs
026670031015e   1c                   endif
026680030515      * - Se colli non sequenziali: stampo "NON SEQUENZIALI" + Elenco
026690030515      *   Se collo cumulativo ....:   "     SCD "X" Num.Colli
026700030515      *   Se colli sequenziali ...:   "     SCD "/" SCA
026710030515      *   Se collo singolo .......:   "     SCD
026720031015sel 1c                   select
026730030515      * - - non sequenziali
026740030515w   1c                   when      DB0fns =  'S'
026750050513if  2c                   if           *in69      = *on
026760050513     c                             or $A4_x_ScNS = *off
026770030715      *     caricamento delle "Note e Particolarità" per consentirne
026780030715      *     l'aggiunta come "Elenco segnacolli non sequenziali"
026790031015if  3c                   if        $Partic = *off
026800030515     c                   exsr      SchPARTIC
026810031015e   3c                   endif
026820030515     c                   exsr      ListaScNS
026830050511x   2c                   else
026840050511if  3c                   if        DB0ncl <= 2
026850050511     c                   exsr      ListaScNSd
026860081007x   3c                   else
026870081007     c                   eval      Pnsx   =  'NON SEQUENZIALI'
026880050511e   3c                   endif
026890030515e   2c                   endif
026900030515      * - - cumulativo
026910050114w   1c**!!!              when      DB0xco =  'X'
026920081010     c**!!!              eval      Pnsx   =  %editc(DB0ncd : 'Z')
026930081010     c**!!!                               +  'X'
026940081010     c**!!!                               +  %editc(DB0ncl : 'Z')
026950030515      * - - singolo o sequenziali
026960030515      *     (non se rimesso al mittente)
026970030515w   1c                   when      DB0ris <> 'M'
026980080922     c                   eval      Pnsx   =  %editc(DB0ncd : 'Z')
026990030515if  2c                   if            DB0nca <> *zeros
027000030515     c                             and DB0nca <> DB0ncd
027010080922     c                   eval      Pnsx   =  %trimr(Pnsx) + '-'
027020080922     c                                    +  %editc(DB0nca : 'Z')
027030030515e   2c                   endif
027040030515e   1c                   endsl
027050030515      *
027060030716      * - Note (1 e 2) e Particolarità (0, 3, 4, 5 e 6)
027070030710      *   con eventuali segnacolli non sequenziali (max 4 x riga)
027080030515if  1c                   if        $Partic = *off
027090030515     c                   exsr      SchPARTIC
027100030515e   1c                   endif
027110030716if  1c                   if           $NP(1) <> *blanks
027120030716     c                             or $NP(2) <> *blanks
027130030716     c                             or $NP(3) <> *blanks
027140030716     c                             or $NP(4) <> *blanks
027150030716     c                             or $NP(5) <> *blanks
027160030716     c                             or $NP(6) <> *blanks
027170030716     c                   eval      Pknp   =  'NOTE  E  PARTICOLARITÀ'
027180030710     c                   eval      Pnp1   =  $NP(1)
027190030710     c                   eval      Pnp2   =  $NP(2)
027200030710     c                   eval      Pnp3   =  $NP(3)
027210030710     c                   eval      Pnp4   =  $NP(4)
027220030710     c                   eval      Pnp5   =  $NP(5)
027230030710     c                   eval      Pnp6   =  $NP(6)
027240030716e   1c                   endif
027250030515      *
027260080401      * - Privato / Appuntamento / Uffici / Consegna Garantita
027270080401      *   (servizio consegna garantita x cliente Wuerth)
027280030515if  1c                   if        DB0gva =  'P '
027290080723     c                   evalr     Ppad = 'PRIVATO'
027300030515e   1c                   endif
027310041012if  1c                   if        DB0gva =  'U '
027320080723     c                   evalr     Ppad = 'CONSEGNA AD UFFICI'
027330041012e   1c                   endif
027340081010if  1c*** NO !!!         if           DB0tc1 =  'A'
027350081010     c*** NO !!!                   or DB0tc2 =  'A'
027360081010     c*** NO !!!         move      'APPUNTAMENTO'Ppad
027370081010e   1c*** NO !!!         endif
027380080401if  1c                   if             DB0nrs = 40
027390090518     c                             and  DB0tsp = 'E'
027400080401     c                             and (DB0lnp = 043
027410110429     c                              or  DB0lnp = 277
027420080401     c                              or  DB0lnp = 063)
027430090217     c*****              evalr     Ppad = 'CONSEGNA GARANTITA'
027440090227     c                   eval      Pds2 = 'CONSEGNA ESPRESSA +
027450090227     c                                     WÜRTH'
027460080401e   1c                   endif
027470030515      *
027480030515      * - Data e ora consegna richiesta
027490030515      *   e Flag "*" > x Data e/o Ora consegna richiesta
027500091116      *   (NO "Prima del" per il cliente TTL)
027510091116if  0c                   if        DB0ccm <> c_CliTTL  or
027520091116     c                             DB0dcr =  *zeros    or  DB0tcr <> 'P'
027530030515if  1c                   if        DB0dcr <> *zeros
027540030515     c                   eval      Past   =  '*'
027550030515     c     *iso          movel     DB0dcr        Data_EUR
027560030515     c     *eur          movel     Data_EUR      Pdcr
027570030515e   1c                   endif
027580030515if  1c                   if        DB0hcr <> *zeros
027590030515     c                   eval      Past   =  '*'
027600030515     c                   eval      *in18  =  *on
027610030515     c                   eval      Phcr   =  DB0hcr
027620030515e   1c                   endif
027630091116e   0c                   endif
027640030515      *
027650030515      * - Consegne particolari
027660030515     c                   eval      Pcp1   =  DB0dc1
027670030515     c                   eval      Pcp2   =  DB0dc2
027680030515     *** *
027690030515     *** * - Valore merce dichiarato e relativa divisa => NO
027700030515     ***c***                eval      Pvmd   =  DB0vmd
027710030515     ***c***                eval      Pvad   =  DB0vad
027720030515     *** *
027730030515     *** * - Importo da assicurare e relativa divisa   => NO
027740030515     ***c***                eval      Pvas   =  DB0vas
027750030515     ***c***                eval      Pias   =  DB0ias
027760030515     *** *
027770030515     *** * - Quantità da fatturare                     => NO
027780030515     ***c***                eval      Pqft   =  DB0qft
027790030515      *
027800030515      * - Imponibile / IVA / Esenzione / Totale fattura e divisa
027810030515      *   (importi "arrotondati" a 2 decimali in stampa)
027820030515if  1c                   if        DB0ift <> *zeros
027830030515     c                   eval      Pkim   = 'IMPONIBILE'
027840130218     c*//                   eval(h)   Pimp   =  DB0imv
027850130218     c                   eval(h)   w0112  =  DB0imv
027860130218     c                   evalr     Pimp   =  %editc(w0112 : 'K')
027870030515sel 2c                   select
027880030515w   2c                   when      DB0iva <> *zeros
027890030515     c                   eval      Pkie   = 'I.V.A.'
027900030624     c                   eval(h)   w0112  =  DB0iva
027910130218     c*//                   eval      Piva   =  '     ' +
027920130218     c*//                                       %editc(w0112 : 'K')
027930130218     c                   evalr     Piva   =  %editc(w0112 : 'K')
027940030515w   2c                   when         DB0cei <> *blanks
027950030515     c                             or DB0dce <> *blanks
027960030912     c                   eval      Pkie   = 'ESENZIONE'
027970030912     c                   eval      Piva   =  DB0dce
027980030515e   2c                   endsl
027990030515     c                   eval      Pktf   = 'TOTALE FATTURA'
028000130218     c*//                   eval(h)   Pift   =  DB0ift
028010130218     c                   eval(h)   w0112  =  DB0ift
028020130218     c                   evalr     Pift   =  %editc(w0112 : 'K')
028030030515if  2c                   if        DB0div <> §GEDcn
028040030515     c                   eval      Pdiv   =  DB0div
028050030515e   2c                   endif
028060030515e   1c                   endif
028070030515      *
028080030515      * - Contrassegno e relativa divisa
028090030515if  1c                   IF        DB0cas <> *zeros
028100030625      *
028110030515     c                   eval(h)   Pcas   =  DB0cas
028120030515if  2c                   if        DB0vca <> §GEDcn
028130030515     c                   eval      Pdca   =  DB0vca
028140030515e   2c                   endif
028150030515     c                   exsr      CtrCAs
028160030624      * - - Nuovo controllo abilitazione c/assegno
028170030624     c                   reset                   TRUL21DS
028180030624     c                   eval      I21cbo =  DB0cbo
028190030624     c                   eval      I21tsp =  DB0tsp
028200030624     c                   eval      I21lnp =  DB0lnp
028210030624     c                   eval      I21nzm =  DB0nzm
028220030624     c                   eval      I21lna =  DB0lna
028230030624     c                   eval      I21nzd =  DB0nzd
028240130826     c                   if        DB0ccm >  *zero
028250130826     c                   eval      I21ksc =  DB0ccm
028260130826     c                   else
028270130826     c                   eval      I21ksc =  (DB0lnp * 10000 ) + 8888
028280130826     c                   endif
028290030624     c                   eval      I21tic =  DB0tic
028300030624if  2c                   if        DB0vca <> §GEDcn
028310030624     c                   eval      I21imp =  Wcas
028320030624     c                   eval      I21div =  §GEDcn
028330030624x   2c                   else
028340030624     c                   eval      I21imp =  DB0cas
028350030624     c                   eval      I21div =  DB0vca
028360030624e   2c                   endif
028370030624     c                   eval      I21ute =  KNMUS
028380040607     c                   eval      I21pgm =  SDSpgm
028390030624     c                   call      'TRUL21R'
028400030624     c                   parm                    TRUL21ds
028410030624      * - - Carico la tabella "Tipo incasso C/Assegno" (1A)
028420030624     c                   exsr      Tab_1A
028430030624     c                   movel     §1Ades        DB0dti
028440030624if  2c                   if            §1Adeo <> *blanks
028450030624     c                             and O21fca <> *blanks
028460030624     c                   movel     §1Adeo        DB0dti
028470030624e   2c                   endif
028480100208      /free
028490100208           // - -?Reperimento intestatario c/assegno?
028500100322           if  §VPOcas <= wDate   and   DUTlpo  <> 'S';
028510100208             reset  trulINTds;
028520100208             iINTtic  = DB0tic;
028530100208             iINTrscM = DB0rsm;
028540100208             iINTrscO = DB0rmo;
028550100208             iINTcdi  = §AR5cdi;
028560100208             //if  §AR5cdi = *blank;
028570100208             //  iINTaas = DB0aas;
028580100208             //  iINTlnp = DB0lnp;
028590100208             //  iINTnrs = DB0nrs;
028600100208             //  iINTnsp = DB0nsp;
028610100208             //endif;
028620100208             trulINTr ( TRULINTds );
028630100322           endif;
028640100208      /end-free
028650030624      * - - Imposto i campi di stampa
028660030624     c                   eval      Ptic   =  DB0tic + ' ' + DB0dti
028670091119      /free
028680100208           // - - -?Intestatario assegno?
028690100322           if  DB0cas  <> *zero   and
028700100322              (§VPOcas <= wDate   and   DUTlpo  <> 'S')   and
028710100208               oINTerr = *blank;
028720100208             Pkic = 'INTESTARE A:';
028730100208             Pica = oINTrsci;
028740100208           endif;
028750091119      /end-free
028760030625      *
028770030515e   1c                   ENDIF
028780030515      *
028790030515      * - Totale da esigere al ritiro o alla consegna e relativa divisa
028800030515if  1c                   IF        DB0ift >  *zeros
028810030625      *
028820030515if  2c                   if            DB0vca <> §GEDcn
028830030515     c                             and DB0vca <> *blanks
028840030515     c                   eval      Pdie   =  DB0div
028850030515e   2c                   endif
028860030515      *   Controllo se C/Assegno da sommare al totale da esigere o no:
028870030515      *   se Franco-Prepagato il C/Assegno non si somma mai;
028880030515      *   se c'è il totale fattura: metto anche il il totale da esigere.
028890030515      *   (importi "arrotondati" a 2 decimali in stampa)
028900030515sel 2c                   Select
028910030515      *   Non ho C/Assegno o è Franco Prepagato
028920071008w   2c                   When          DB0cas  = *zeros
028930071008     c                             or  DB0tbo  = '8'
028940080701     c                   eval(h)   Pies   =  DB0ift
028950030625     c                   clear                   Pie1
028960030625     c                   clear                   Pdi1
028970030515      *   C/Assegno sommabile
028980030515w   2c                   When      DB0ste =  'S'    and $CAeur = *on
028990030515sel 3c                   select
029000030515      *   - c/assegno in EURO
029010030515w   3c                   when      DB0div =  §GEDcn and DB0vca =  §GEDcn
029020080701     c                   eval(h)   Pies   =  DB0ift + DB0cas
029030030624     c                   clear                   Pie1
029040030624     c                   clear                   Pdi1
029050030515      *   - c/assegno in DIVISA
029060030515w   3c                   when          DB0div =  §GEDcn
029070030515     c                             and DB0vca <> §GEDcn
029080080701     c                   eval(h)   Pies   =  DB0ift + Wcas
029090030624     c                   clear                   Pie1
029100030624     c                   clear                   Pdi1
029110030515e   3c                   endsl
029120030515      *   C/Assegno non sommabile
029130030515x   2c                   Other
029140080701     c                   eval(h)   Pie1   =  DB0cas
029150080701     c                   eval(h)   Pies   =  DB0ift
029160030515e   2c                   Endsl
029170030709      *
029180030709e   1c                   ENDIF
029190030515      *
029200030515      * - Numero e data fattura
029210030515if  1c                   if        DB0nft <> *zeros
029220030515     c                   eval      Pkft   =  'FATTURA   '
029230030515     c                   eval      Pnft   =  DB0nft
029240130124     c                   eval      Pfiv   = '(' + %editc(wB0fiv:'X') +
029250130124     c                                      ')'
029260030515if  2c                   if        DB0dft >  *zeros
029270030515     c                   eval      Pkdf   =  'DEL'
029280030515     c     *iso          movel     DB0dft        Data_EUR
029290130418     c     *dmy          movel     Data_EUR      Pdft
029300130418     c*//  *eur          movel     Data_EUR      Pdft
029310030515e   2c                   endif
029320030515e   1c                   endif
029330030515      *
029340030515      * Memorizzo il codice tariffa come numero di copie da stampare
029350030515      *   (per Rimesso Mittente - non per DPD o FEDEX)
029360030515if  1c                   if        DB0ris = 'M'  and  *in70
029370030515     c                   eval      Wctr   =  DB0ctr
029380030515e   1c                   endif
029390030515      *
029400030515     c                   ENDSR
029410170605      *
029420170605      /free
029430170605       //--------------------------------------------------------------
029440170605       //?Controllo SE ragione sociale Mittente da FORZARE (TFNTC) per ?
029450170605       //?spedizione DPD.                                              ?
029460170605       //--------------------------------------------------------------
029470170605       BEGSR  sr_Mitt_DPD;
029480170605
029490170605         // -?Reperim. abilitazioni del Mittente dalla tab. "CLI"?
029500170605         clear  k05tntbe01;
029510170605         k_TBEcod = 'CLI';
029520170605         k_TBEke1 = %editc( DB0ccm : 'X' );
029530170605         chain  %kds( k05tntbe01 )  TNTBE000;
029540170605         if  Not %found( TNTBE01L )  or  TBEatb <> *blank;
029550170605           leavesr;
029560170605         endif;
029570170605         dCLI = TBEuni;
029580170605         // -?Cliente SENZA forzatura Mittente da note "AM" e "AS"?
029590170605         if  §CLItfntc <> 'S'  and  §CLInoIMtt <> 'S';
029600170605           leavesr;
029610170605         endif;
029620170605
029630170605         // -?Forzatura Mittente da note "AM"?
029640170605         If  §CLItfntc = 'S';
029650170605
029660170605           if  NOT %open( TFNTC01L );
029670170605             open  TFNTC01L;
029680170605           endif;
029690170605           clear  keyTFNTC01;
029700170605           k_NTCapl = 'C';
029710170605           k_NTCnk1 = %editc( DUTkci : 'X' ) + %editc( DB0ccm : 'X' );
029720170605           k_NTCtnt = 'AM';
029730170605           chain  %kds( keyTFNTC01 )  TFNTC;
029740170605           if  %found( TFNTC01L )  and  NTCsns = 'S';
029750170605             DB0rsm = NTCrnt;
029760170605           endif;
029770170605
029780170605         EndIf;
029790170605
029800170605         // -?Pulizia Indirizzo Mittente?
029810170605         If  §CLInoIMtt = 'S';
029820170605
029830170605           clear  DB0inm;
029840170605           clear  DB0cam;
029850170605           clear  DB0lom;
029860170605           clear  DB0prm;
029870170605           clear  DB0nzm;
029880170605
029890170605         EndIf;
029900170605
029910170605       ENDSR;
029920170605
029930170605      /end-free
029940030515      *
029950030515      *---------------------------------------------------------------*
029960050217      *?Inschieramento delle Note e delle Particolarità              ?*
029970030515      *---------------------------------------------------------------*
029980030515     c     SchPARTIC     BEGSR
029990030515      *
030000030515     c*** (già fatto:)   reset                   $Partic
030010030710     c*** (già fatto:)   reset                   $NoDesPartic
030020030515     c*** (già fatto:)   clear                   $NP
030030030717     c*** (già fatto:)   clear                   W_CoOr
030040030515     c                   clear                   XX
030050030710      *
030060030710      * Imposto eventuali note per prime
030070030710if  1c                   if        DB0nt1 <> *blanks
030080030710     c                   add       1             xx
030090030710     c                   movel     DB0nt1        $NP(xx)
030100030710e   1c                   endif
030110030710if  1c                   if        DB0nt2 <> *blanks
030120030710     c                   add       1             xx
030130030710     c                   movel     DB0nt2        $NP(xx)
030140030710e   1c                   endif
030150030515      *
030160030515      * Le particolarità possono essere lunghe fino a 45 char.
030170030515      *   Se i primi 30 (della 1ª riga a disposizione) non dovessero
030180030515      *   bastare, il resto lo si mette nella riga successiva.
030190030710      *   Se, invece, non risultano decodificate, si mettono solo i
030200030710      *   codici sulla stessa riga.
030210030710      *   Se uno solo risulta decodificato, esso va su una riga a
030220030710      *   parte (vedi flag "$NoDesPartic": *off = serve riga nuova
030230030710      *                                    *on  = ok riga procedente)
030240030515      *
030250030515      * . . Particolarità Giacenza
030260030515if  1c                   IF        DB0gga <> *blanks
030270030515     c                   movel     'Q'           WTpPar
030280030515     c                   movel     DB0gga        WCdPar
030290030515     c                   exsr      TABxPAR
030300030710if  2c                   if            TBLuni       = *blanks
030310030710     c                             and $NoDesPartic = *on
030320030710     c                   eval      $NP(xx) = %trim($NP(xx))
030330030710     c                                     + '   ' + WCdPar
030340030710x   2c                   else
030350030710if  3c                   if        TBLuni       = *blanks
030360030710     c                   eval      $NoDesPartic = *on
030370030710x   3c                   else
030380030710     c                   eval      $NoDesPartic = *off
030390030710e   3c                   endif
030400030515     c                   add       1             xx
030410100513     c                   movel     w053a         $NP(xx)
030420100513if  3c                   if        %subst(w053a : %len($NP(xx))+1)
030430030515     c                              <> *blanks
030440030515     c                   add       1             xx
030450030515     c                   eval      $NP(xx) = '   ' +
030460100513     c                             %trim(%subst(w053a:%len($NP(xx))+1))
030470030710e   3c                   endif
030480030515e   2c                   endif
030490030515e   1c                   ENDIF
030500030515      *
030510030515      * . . Particolarità Consegna
030520030515if  1c                   IF        DB0gma <> *blanks
030530030515     c                   movel     'R'           WTpPar
030540030515     c                   movel     DB0gma        WCdPar
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
030660030515     c                   add       1             xx
030670100513     c                   movel     w053a         $NP(xx)
030680100513if  3c                   if        %subst(w053a : %len($NP(xx)) + 1)
030690030515     c                              <> *blanks
030700030515     c                   add       1             xx
030710030515     c                   eval      $NP(xx) = '   ' +
030720100513     c                             %trim(%subst(w053a:%len($NP(xx))+1))
030730030710e   3c                   endif
030740030515e   2c                   endif
030750030515e   1c                   ENDIF
030760030515      *
030770030515      * . . Particolarità Varie
030780030717      *     - questa descrizione va messa nell'apposito campo PPAD
030790041015      *       se è "PRIVATO" idem se è "CONSEGNA AD UFFICI"
030800150304     *** *     - se 'B' (bancali da rendere al mittente) comparirà un
030810150304     *** *       apposito box
030820150304     *** *     - se 'O' (oltre 20 colli => bancali da riportare in mag.)
030830150304     *** *       comparirà un apposito box, un'apposita nota e l'indica-
030840150304     *** *       zione nella copia per il destinatario.
030850030717      *     ...Potrebbero anche non esserci più elementi di schiera
030860030717      *     disponibili [xx >= %elem($NP)] !
030870030709if  1c                   IF            DB0gva <> *blanks
030880150304     c*//                          and DB0gva <> 'B '
030890150304     c*//                          and DB0gva <> 'O '
030900030709     c                             and DB0gva <> 'P '
030910041015     c                             and DB0gva <> 'U '
030920030515     c                   movel     'S'           WTpPar
030930030515     c                   movel     DB0gva        WCdPar
030940030515     c                   exsr      TABxPAR
030950030710if  2c                   if            TBLuni       = *blanks
030960030710     c                             and $NoDesPartic = *on
030970030710     c                   eval      $NP(xx) = %trim($NP(xx))
030980030710     c                                     + '   ' + WCdPar
030990030710x   2c                   else
031000030710if  3c                   if        TBLuni       = *blanks
031010030710     c                   eval      $NoDesPartic = *on
031020030710x   3c                   else
031030030710     c                   eval      $NoDesPartic = *off
031040030710e   3c                   endif
031050030717if  3c                   if            xx < %elem($NP)
031060030709     c                   add       1             xx
031070100513     c                   movel     w053a         $NP(xx)
031080100513if  4c                   if        %subst(w053a : %len($NP(xx)) + 1)
031090030709     c                              <> *blanks
031100030717     c                             and xx < %elem($NP)
031110030709     c                   add       1             xx
031120030709     c                   eval      $NP(xx) = '   ' +
031130100513     c                             %trim(%subst(w053a:%len($NP(xx))+1))
031140030717e   4c                   endif
031150030717e   3c                   endif
031160030709e   2c                   endif
031170030515e   1c                   ENDIF
031180030717      *
031190030717      * Imposto il numero originale di colli componenti la spedizione
031200030717      * e se ho ancora spazio lo memorizzo in "Note e Particolarità"
031210030717if  1c                   if        %subst(DB0gva:1:1) = 'O'
031220030717     c                   exsr      RepAR5bnb
031230030717if  2c                   if        W_CoOr  <> *blanks
031240030717     c                             and xx  <  %elem($NP)
031250030717     c                   add       1             xx
031260030717     c                   eval      $NP(xx) =  W_CoOr
031270030717e   2c                   endif
031280030717e   1c                   endif
031290030515      *
031300030515      * Imposto il flag per memorizzare la già avvenuta memorizzazione
031310030715      *   delle "Note e Particolarità"
031320030515     c                   eval      $Partic = *on
031330030515      *
031340030515     c                   ENDSR
031350070319      *
031360070319      *---------------------------------------------------------------*
031370070319      *?Inschieramento delle Consegne Parziali precedenti            ?*
031380070319      *---------------------------------------------------------------*
031390070319     c     SchConsParz   BEGSR
031400070319      *
031410070319     c*** (già fatto:)   reset                   $CP
031420070319     c*** (già fatto:)   reset                   $CPdcm
031430070319     c                   clear                   XX
031440070319      *
031450070319if  2c                   if        NOT %open(FNART01L)
031460070319     c                   open      FNART01L
031470070319e   2c                   endif
031480070319      *
031490070514if  2c                   if        $Mamma = *off
031500070319     c     KLDV          setll     FNART000
031510070319     c     KLDV          reade     FNART000
031520070514x   2c                   else
031530070514     c     KLBL          setll     FNART000
031540070514     c     KLBL          reade     FNART000
031550070514e   2c                   endif
031560070319      *
031570070319do  1c                   DOW       NOT  %eof(FNART01L)
031580070319      *
031590070319if  2c                   If             ARTatb = *blanks
031600070319     c                             and  ARTdcm > *zeros
031610070319      *
031620070319     c                   eval      *in31  = *off
031630070319     c                   eval      xx     = 1
031640070319     c     ARTdcm        lookup    $CPdcm(xx)                             31
031650070319if  3c                   if        NOT  *in31
031660070319     c                   eval      xx     = 1
031670070319     c     *zeros        lookup    $CPdcm(xx)                             31
031680070319if  4c                   if        *in31
031690140307     c*//                eval      $CP(xx)     = *zeros
031700140307     c                   clear                   Elem_CP
031710140307     c                   movel     Elem_CP       $CP(xx)
031720070319e   4c                   endif
031730070319e   3c                   endif
031740070319if  3c                   if        *in31
031750070319     c                   eval      $CPdcm(xx)  = ARTdcm
031760070319     c                   movel     $CP(xx)       Elem_CP
031770070319     c                   eval      WCPdcm      = ARTdcm
031780070319     c                   eval      WCPncl      = WCPncl + 1
031790070319     c                   movel     Elem_CP       $CP(xx)
031800070319e   3c                   endif
031810070319      *
031820070319e   2c                   EndIf
031830070319      *
031840070514if  2c                   if        $Mamma = *off
031850070319     c     KLDV          reade     FNART000
031860070514x   2c                   else
031870070514     c     KLBL          reade     FNART000
031880070514e   2c                   endif
031890070319      *
031900070319e   1c                   ENDDO
031910070319      *
031920140513if  0c                   if        xx     > *zero
031930140513      * Reperimento nominativi firmatari
031940140513     c                   exsr      sr_Firmatario
031950070319      * Ordinamento schiera per data
031960070319if  1c                   if        xx     > 1
031970070319     c                   sorta                   $CP
031980070319e   1c                   endif
031990140513e   0c                   endif
032000070319      *
032010070319     c                   ENDSR
032020140307      *
032030140307      /free
032040140307
032050140307       //--------------------------------------------------------------
032060140307       //?Reperimento nome firmatario                                  ?
032070140307       //--------------------------------------------------------------
032080140307       BEGSR  sr_Firmatario;
032090140307
032100140307if  1    if  NOT  %open(FNEVB22L);
032110140307           open  FNEVB22L;
032120140307e   1    endif;
032130140307
032140140307         clear  keyFNEVB22;
032150140307         k_EVB2aas = DB0aas;
032160140307         k_EVB2lnp = DB0lnp;
032170140307         k_EVB2nrs = DB0nrs;
032180140307         k_EVB2nsp = DB0nsp;
032190140307         k_EVB2cev = 'P  ';
032200140307         setll  %kds( keyFNEVB22 : 4 )  FNEVB200;
032210140307         reade  %kds( keyFNEVB22 : 4 )  FNEVB200;
032220140307         DoW  Not  %eof(FNEVB22L);
032230140307           if  EVB2cev = k_EVB2cev  and  EVB2trd = 'FIR';
032240140307             yy = %lookup( EVB2dev : $CPdcm );
032250140307             if  yy > *zero;
032260140307               Elem_CP = $CP(yy);
032270140307               if  WCPocm  <= EVB2hev;
032280140307                 WCPocm  = EVB2hev;
032290140307                 WCPfir  = EVB2dati;
032300140307                 $CP(yy) = Elem_CP;
032310140307               endif;
032320140307             endif;
032330140307           endif;
032340140307           reade  %kds( keyFNEVB22 : 4 )  FNEVB200;
032350140307         EndDo;
032360140307
032370140307       ENDSR;
032380140307
032390140307      /end-free
032400031124      *
032410031124      *---------------------------------------------------------------*
032420050217      *?Reperimento Nominativo e Telefono destinatario da FIAR5/GEN  ?*
032430031124      *---------------------------------------------------------------*
032440031124     c     RepAR5gen     BEGSR
032450031124      *
032460031124     c                   clear                   dAR5gen
032470031124      *
032480031124     c                   eval      AR5trd = 'GEN'
032490040301if  1c                   if        DUTlpo = 'S'
032500040531     c     K05AR501      chain     FIAR531C
032510031127x   1c                   else
032520031127     c     K05AR501      chain     FIAR5000
032530031127e   1c                   endif
032540031124      *
032550040301if  1c                   if           (DUTlpo =  'S'
032560040531     c                             and %found(FIAR531C)
032570040301     c                             OR  DUTlpo <> 'S'
032580040301     c                             and %found(FIAR501L))
032590040301     c                             and AR5atb =  *blanks
032600031124     c                   movel     AR5uni        dAR5gen
032610031124e   1c                   endif
032620031124      *
032630031124     c                   ENDSR
032640030717      *
032650030717      *---------------------------------------------------------------*
032660050217      *?Reperimento Numero Colli Originale da FIAR5/BNB              ?*
032670030717      *---------------------------------------------------------------*
032680030717     c     RepAR5bnb     BEGSR
032690030717      *
032700030717     c                   clear                   dAR5bnb
032710030717     c*** (già fatto:)   clear                   W_CoOr
032720030717      *
032730030717     c                   eval      AR5trd = 'BNB'
032740040301if  1c                   if        DUTlpo = 'S'
032750040531     c     K05AR501      chain     FIAR531C
032760040301x   1c                   else
032770030717     c     K05AR501      chain     FIAR5000
032780040301e   1c                   endif
032790030717      *
032800040301if  1c                   if           (DUTlpo =  'S'
032810040531     c                             and %found(FIAR531C)
032820040301     c                             OR  DUTlpo <> 'S'
032830040301     c                             and %found(FIAR501L))
032840040301     c                             and AR5atb =  *blanks
032850030717     c                   movel     AR5uni        dAR5bnb
032860030717if  2c                   if        §AR5bcor <> *zeros
032870030717     c                   eval      W_CoOr   =  C_CoOr
032880030717     c                                      +  %editc(§AR5bcor : 'K')
032890030717e   2c                   endif
032900030717e   1c                   endif
032910030717      *
032920030717     c                   ENDSR
032930030515      *
032940030515      *---------------------------------------------------------------*
032950051129      *?Decodifiche condizionate in Lettera di Vettura               ?*
032960030515      *---------------------------------------------------------------*
032970040607     c     DecodLDV      BEGSR
032980030515      *
032990030515      * Richiesta firma per ricevuta della merce senza riserve
033000030515      * (e NON  quella  per quietanza) => *in35 + not *in36
033010030515     c                   movea     '10'          *in(35)
033020101130      *
033030101130      /free
033040101130
033050101130         // -?Memorizzazione consegne parziali precedenti dal file?
033060101130         //  ?FNART00F - serve anche nella stampa delle "tagliatelle"?
033070140513         IF  $ConsParz;
033080140513
033090140513           // -?SE si stanno stampando "tagliatelle":?
033100140514           //  ?le consegne parziali vengono indicate nel Box1,?
033110140514           //  ?SE?ne sono state reperite?
033120140513           IF  DB0fa4 = '4'  and  $CP(1) < *all'9';
033130140513
033140101130             // -?Prima si totalizzano le quantità consegnate?
033150101130             clear  w0080;
033160101130             clear  w0140;
033170101130             For  xx = 1  To  %elem($CP);
033180101130               if  $CP(xx) < *all'9';
033190101130                 Elem_CP = $CP(xx);
033200101130                 monitor;
033210101130                   Data_Eur = %date( WCPdcm : *iso );
033220101130                 on-error;
033230101130                   Data_Eur = *loval;
033240101130                 endmon;
033250101130                 if       Data_Eur  > *loval  and
033260101130                     %dec(Data_Eur) > w0080;
033270101130                   w0080 = %dec(Data_Eur);
033280101130                 endif;
033290101130                 w0140 += WCPncl;
033300101130               endif;
033310101130             EndFor;
033320101130             // -?Poi si stampa un'unica indicazione?
033330101201             Data_DMY = %date( w0080 : *eur );
033340101201             Pcpp = 'Consegnati '
033350101201                  + %triml( %editc( w0140 : 'Z' ) )
033360101201                  + ' colli al '
033370101201                  + %triml( %editc( %dec( Data_DMY ) : 'Y' ) );
033380140513
033390140513           ENDIF;
033400140513
033410140513         ENDIF;
033420101130
033430101130      /end-free
033440070307      *
033450070307      * Intestazione moduli
033460070307      * - Se spedizione in stato "Lasciato Avviso"
033470070307      *   si stampano anche data/ora del 1° lasciato avviso
033480070307if  1c                   if        $LascAvv  = *on
033490140307     c                   z-add     wEVBdev       w0080
033500070315     c     *iso          test(d)                 w0080                  31
033510070315if  2c                   if        NOT *in31
033520071025     c     *iso          movel     w0080         Data_EUR
033530071025     c     *eur          movel     Data_EUR      w0080
033540070315x   2c                   else
033550070315     c                   clear                   w0080
033560070315e   2c                   endif
033570070307     c                   eval      Tla    =  'Lasciato avviso il '
033580070307     c                                    +  %editc(w0080 : 'Y')
033590070307     c                                    +  ' alle '
033600140307     c                                    +  %editw(wEVBhev : '  :  ')
033610070307e   1c                   endif
033620030709      *
033630100107     *** * Evidenziazione servizio "espresso"
033640100107     *** * o del servizio "Poste italiane"
033650100107     ***c                   eval      *in37 = (DB0tsp = 'E')
033660100107     ***c  n37              eval      *in37 = (§OGntw = 'PPT')
033670090611      *
033680090605      /free
033690100107
033700101130         // -?Evidenziazione servizio "espresso"?
033710101130         //           ?o del servizio "Poste Italiane"?
033720100107         *in37 = (DB0tsp = 'E'   or   DB0tsp = 'H'   or
033730100107                  §OGntw = 'PPT');
033740100107
033750101130         // -?Consegna al mattino (tempi consegna = 12 ore)?
033760101130         //  ?(NON per Würth)?
033770090615         if  §VPOrcm <= wDate and
033780090622             DUTlpo  <> 'S'   and
033790090615             DB0tsp   = 'E'   and
033800090615            (DB0nrs  <> 40    or  (DB0lnp <> 043   and
033810110429                                   DB0lnp <> 277   and
033820090605                                   DB0lnp <> 063));
033830090605           clear  TNSD99ds;
033840090605           if  DB0tbo = 'P'   or   DB0tbo = '8';
033850090605             D98tbo = 'P';
033860090605           else;
033870090605             D98tbo = 'A';
033880090605           endif;
033890090605           D98aas = DB0aas;
033900090605           D98lnp = DB0lnp;
033910090605           D98nrs = DB0nrs;
033920090605           D98nsp = DB0nsp;
033930090605           tnsd99r ( TNSD99ds );
033940120910           //*if  D98tci = 12;
033950120910           if  D98tcc = 12;
033960090605             Pdsm = 'M';
033970090605           endif;
033980090605         endif;
033990090605      /end-free
034000030515      *
034010030515      * Franco prepagato
034020030515      * (richiede firma per quietanza nella copia x destinatario)
034030030515if  1c                   if        DB0tbo =  '8'  and  DB0ris <> 'M'
034040030515     c                   movea     '01'          *in(35)
034050030515if  2c                   if        Pds1   =  *blanks
034060030515     c                   eval      Pds1   =  'FRANCO PREPAGATO        '
034070030515e   2c                   endif
034080120530     c*//                if        (DB0dft > *zero       and
034090120530     c*//                           DB0dft > §vpoBRTdta)  OR
034100120530     c*//                          (DB0dft = *zero       and
034110120530     c*//                           Wdsp   > §vpoBRTdta)
034120130418     c                   eval      Pbrt2  =  $BRT
034130120530     c*//                else
034140130418     c*//                eval      Pbrt2  =  $Bartolini
034150120530     c*//                endif
034160030515      * - Legge 450 (solo per prepagati)
034170030515if  2c                   if            DB0ias =  *zeros
034180030515     c                             and DB0vas =  *blanks
034190030515     c                             and DB0div =  §GEDcn
034200060301     c                             and §GElrp >  *zeros
034210030515     c*** v. IF sovrastante...     and DB0tbo =  '8'
034220060301if  3c                   if        Wdsp   >  20060228
034230110315     c                   z-add     §GElrp        w0112
034240080701     c                   eval      Prcv   =  'Art. 1696 c.c. mod. +
034250061102     c                                       dlgs 286/2005 '
034260060301     c                                    +  §GEDcn + ' '
034270060301     c                                    +  %trim(%editc(w0112 : '4'))
034280060301     c                                    +  ' al kg.'
034290060301x   3c                   else
034300110315     c                   z-add     6.20          w0112
034310080701     c                   eval      Prcv   =  'Legge 450/85 '
034320030515     c                                    +  §GEDcn + ' '
034330030624     c                                    +  %trim(%editc(w0112 : '4'))
034340030515     c                                    +  ' al kg.'
034350060301e   3c                   endif
034360030515e   2c                   endif
034370030515e   1c                   endif
034380031031      *
034390101125      /free
034400121017
034410121017         // -?Indicazione della Partiva IVA o del Cod. Fiscale?
034420121017         //  ?(se Porto Franco Prepagato?
034430121017         //    ?o Porto Assegnato con Fattura immediata)?
034440121017         //  ?- NON se mittente estero -?
034450121017             // -?con fattura?
034460121017         IF  (DB0nft <> *zero  or   DB0ift <> *zero)   AND     (
034470121017             // - -?P.F. prepagato?
034480121017             //    ?(con mittente italiano)?
034490121017             (DB0tbo =  '8'             and  DB0nzm  = *blank)  or
034500121017             // - -?P.A. (con fattura immediata)?
034510121017             //    ?(con destinatario italiano)?
034520121017             (%subst(DB0tb1:1:1) = 'A'  and  DB0nzd  = *blank)  or
034530121017             // - -?con 2ª bolla?
034540121017             //    ?(con destinatario italiano)?
034550121017             (DB0tb2 <> *blank          and  DB0tbo <> '8'
034560121029                                        and  DB0tbo <> 'P'
034570121017                                        and  DB0nzd  = *blank) );
034580121017           select;
034590121017             when  Not $No_PI  and  wB0piv <> *blank  and
034600121017                   // ·?Esiste Partita Iva NON FITTIZIA?
034610121026                   %scan('$$' : wB0piv) = *zero;
034620121017                   // ·?Non è un C.F. - lungo 16 (privato)?
034630121026                   //and  %len( %trim( wB0cfi ) ) < 16;
034640121017               Ppiv = 'Partita Iva: ' + wB0piv;
034650121017             when  Not $No_CF  and  wB0cfi <> *blank;
034660121017               Ppiv = 'Codice Fiscale: ' + wB0cfi;
034670121017           endsl;
034680130418           //select;
034690130418           //  // ·?Se P.F. Prepagato il C.F./P.I. è del Mittente?
034700130418           //  when  DB0tbo = '8';
034710130418           //    PpivM = Ppiv;
034720130418           //  // ·?Se P.A. con fattura immed. il C.F./P.I. è del Dest.?
034730130418           //  when  %subst(DB0tb1:1:1) = 'A';
034740130418           //    PpivD = Ppiv;
034750130418           //  // ·?Se 2ª bolla (DDT sì) è del Dest.?
034760130418           //  when  DB0tb2  <> *blanks  and  DB0tbo  <> 'P';
034770130418           //    PpivD = Ppiv;
034780130418           //endsl;
034790121017         ENDIF;
034800101125
034810101125         Select;
034820101125
034830101125           // -?Non utilizzare per la consegna?
034840101125           //  ?(stampa della sola copia per il vettore)?
034850101125           When  $No_x_Cons = *on;
034860101125             *in35 = *off;
034870101125             *in36 = *off;
034880101125             // -?costante da stampare se LDV recapito c/assegni?
034890101125             //  ?stampata dal pgm "Stampa DDT NO in arrivo"?
034900101129             Pnux  = 'NON UTILIZZARE PER LA CONSEGNA';
034910101125
034920101125           // -?Ritorno all'Incasso?
034930101125           When  $Rit_a_Inc = *on;
034940101125             *in40 = *on;
034950101125             Pnux  = 'RITORNO ALL''INCASSO';
034960101125
034970101125         EndSl;
034980101125
034990101125      /end-free
035000030910      *
035010030910      * Bollo assolto in modo virtuale
035020030910if  1c                   if        $BolloVirt =  *on
035030060719if  2c                   if        DB0dft <  20060801
035040080701     c                   eval      Pbav1  =  'Bollo '
035050080701     c                                    +  %trim(%editc(wBolloVirt:'4'))
035060080701     c                                    +  ' ' + DB0div
035070080701     c                                    +  ' assolto in modo virtuale '
035080080701     c                   eval      Pbav2  =  'Aut. Ag. Ent. BO 3 27/09/02 '
035090080701     c                                    +  'Prot. 31599'
035100060719x   2c                   else
035110080701     c                   eval      Pbav1  =  'Bollo '
035120080701     c                                    +  %trim(%editc(wBolloVirt:'4'))
035130080701     c                                    +  ' ' + DB0div
035140080701     c                                    +  ' assolto in modo virtuale '
035150080701     c                   eval      Pbav2  =  'Aut. Ag. Ent. MI 2 10/07/06 '
035160080701     c                                    +  'Prot. 45114'
035170060719e   2c                   endif
035180030910e   1c                   endif
035190050503      *
035200080605      * Costanti "PER RICEVUTA DELLA MERCE..."
035210080605     c                   eval      Pkrm1 = '(nome e cognome in stampa+
035220080605     c                                       tello)'
035230080605     c                   eval      Pkrm2 = '(firma)'
035240080605     c                   eval      Pkrm3 = '(data e ora)'
035250050504sel 1c                   select
035260050504      * - danon?stampare se
035270050504      *   · franco prepagato                (NOT *in35)
035280050504      *   · non utilizzare per la consegna  (NOT *in35)
035290101125      *   · ritorno all'incasso             (*in40)
035300050504      *   · bolla FedEx                     (*in72)
035310101125w   1c                   when      NOT *in35  or  *in40  or  *in72
035320050504     c*** (già fatto:)   clear                   Pkrm
035330080605     c                   clear                   Pkrm1
035340080605     c                   clear                   Pkrm2
035350080605     c                   clear                   Pkrm3
035360080922      * - costante "DPD"
035370120301w   1c                   when      *in71
035380080922     c                   eval      Pkrm   =  'PER RICEVUTA DELLA MERCE-
035390080922     c                              SENZA RISERVE'
035400120530      * - costante "standard" per "DDT sì"
035410090615w   1c                   when          §VPOrsr <= wDate
035420081119     c                             and  DB0bam =  'S'
035430071005     c                   eval      Pkrm   =  'PER RICEVUTA DELLA MERCE-
035440071005     c                              E DEL DOCUMENTO DI TRASPORTO SENZA-
035450071005     c                              RISERVE'
035460120530      * - costante "standard" per "DDT no" + Packing-List
035470120301w   1c                   when           DB0bam =  *blank
035480170605     c                             and ($FIAR8 =  *on
035490170605     c                              or  $§PDF  =  *on)
035500120301     c                   eval      Pkrm   =  'PER RICEVUTA DELLA MERCE-
035510120301     c                              E DEL DOCUMENTO DI TRASPORTO SENZA-
035520120301     c                              RISERVE'
035530120301      * - costante "standard" altrimenti
035540050504x   1c                   other
035550050504     c                   eval      Pkrm   =  'PER RICEVUTA DELLA MERCE-
035560050504     c                              SENZA RISERVE'
035570050504e   1c                   endsl
035580090513      /free
035590090611       Select;
035600101130         // -?costante per cliente?
035610101130         //  ?"Consorzio Lotterie Nazionali - Gratta & Vinci"?
035620140513         when  DB0ccm = c_CliCLN  or  DB0ccm = c_CliLN_2;
035630090528           exsr  sr_RepDdtCLN;
035640090706           Pkrm =  'LA FIRMA PER RICEVUTA DELLA MERCE VALE ANCHE PER +
035650090706                    IL DDT N° '
035660090527                + %editc (WddtCLN : 'Z')
035670090615                + ' DEL '
035680090527                + %editc (WdspCLN : 'Y')
035690101001                + ' CON RISERVA DI CONTROLLO COME DA CONTRATTO';
035700101001           if  Wdsp < 20101001;
035710101001             Pkrm = %trimr(Pkrm) + ' C.L.N.';
035720101001           else;
035730101001             Pkrm = %trimr(Pkrm) + ' L.N. S.R.L.';
035740101001           endif;
035750101130         // -?costante per cliente?
035760101130         //  ?"S G Soluzioni Logistiche"?
035770091117         when  DB0ccm = c_CliSG   or   DB0ccm = c_CliSG_2;
035780091117           if  DB0nas <> *blank   and   DB0nas <> c_NoContr;
035790091117             Pkrm = %trimr(Pkrm)
035800091117                  + '.   INCLUDE CONTRATTO - '
035810091117                  + %trim(DB0nas);
035820091117           endif;
035830091117           if  DB0rmo <> *blank;
035840091117             if  DB0nas <> *blank;
035850091117               Pkrm = %trimr(Pkrm)
035860091117                    + ' - MATR TV: '
035870091117                    + %trim(DB0rmo);
035880091117             else;
035890091117               Pkrm = %trimr(Pkrm)
035900091117                    + '.   MATR TV: '
035910091117                    + %trim(DB0rmo);
035920091117             endif;
035930091117           endif;
035940090611       EndSl;
035950090513      /end-free
035960030717      *
035970130418      *//* Indicazione degli "oltre 20 colli"
035980130418      *//* (MAI se Franco prepagato)
035990130418if  1c*//                   if            DB0tbo <> '8'
036000130418     c*//                             and *in36  =  *off
036010130418     c*//                             and W_CoOr <> *blanks
036020130418     c*//                   eval      Pbrt2  = W_CoOr
036030130418e   1c*//                   endif
036040030515      *
036050030515      * Verifico se imponibile/iva/totale/estremi fattura
036060030515     c                   eval      *in19  = ((DB0imv <> *zeros)  or
036070030515     c                                       (DB0iva <> *zeros)  or
036080030515     c                                       (DB0cei <> *blanks) or
036090030515     c                                       (DB0dce <> *blanks) or
036100030515     c                                       (DB0ift <> *zeros)  or
036110030515     c                                       (DB0nft <> *zeros)  or
036120031125     c                                       (DB0dft <> *zeros))
036130030515      *
036140030515      * Verifico se contrassegno
036150030515     c                   eval      *in20  =  (Pcas <> *zeros)
036160030515      *
036170030709      * Verifico se totale da esigere al ritiro o alla consegna
036180080701     c                   eval      *in21  =  (Pies <> *zeros)
036190030709      *
036200030710      * Intesto il 2° modulo diversamente per prepagato
036210030710      * (solo in partenza)
036220030710if  1c                   if        *in21  and  *in36
036230030710     c                             and   (DB0tbo = 'P'
036240030710     c                                or  DB0tbo = '8')
036250030710     c                   eval      Tcxd   =  'COPIA PER IL MITTENTE    '
036260130729     c                   clear                   Tcxd2
036270030710e   1c                   endif
036280030515      *
036290120530      * Verifico se codice a barre BRT da stampare
036300030515     c                   eval      *in50  =  (Tbcd <> *blanks)
036310040629      * Verifico se codice a barre O.R.M.    da stampare
036320040629      * (non è ancora impostato - vedi subr. "StampaRCC")
036330040629     c***                eval      *in52  =  (Pbco <> *blanks)
036340030515      *
036350030515      * Flag "*" > x Consegne particolari
036360030515     c                   if        DB0tc1 =  'A'  or  DB0tc2 = 'A'
036370030515     c                   eval      Past   =  '*'
036380030515     c                   endif
036390030515      *
036400030515      * C/ASSEGNO - - - - - - - - - - - - - - - - - - - - - - - - - - *
036410030515if  1c                   IF        DB0cas <> *zeros
036420030515      * - Flag "*" > x C/Assegno
036430030904if  2c                   if        O21fca <> *blanks
036440030904     c                   movel     '*'           Past
036450030515e   2c                   endif
036460030624      * - Imposto i campi di stampa
036470030624     c                   eval      Ptic   =  DB0tic + ' ' + DB0dti
036480030515e   1c                   ENDIF
036490030515      *
036500040607      * TIPO LDV  - - - - - - - - - - - - - - - - - - - - - - - - - - *
036510030515      * - Se Bolla in Assegnato: stampo una "A" vicino all'asterisco
036520030515      * e se Bolla in Assegnato Senza Fattura: stampo "SEGUE FATTURA"
036530030515if  1c                   if           %subst(DB0tb1:1:1) = 'A'
036540030515     c                             or        DB0tb2     <> *blanks
036550030515     c                   eval      Pass   =  'A'
036560030515if  2c                   if        DB0nft =  *zeros
036570031125     c                   eval      *in22  =  *on
036580130215     c                   eval      Piva   =  'SEGUE FATTURA'
036590030515e   2c                   endif
036600030515e   1c                   endif
036610030515      *
036620030515      * DECODIFICA TIPO CONSEGNA RICHIESTA  - - - - - - - - - - - - - *
036630051129sel 1***c                   Select
036640051129w   1***c                   WHEN      DB0tcr = *blanks
036650051129     *** * - giorno si
036660051129w   1***c                   WHEN      DB0dcr <> *zeros  and  DB0tcr = 'P'
036670051129     ***c                   eval      Ptdr   =  'PRIMA DEL  '
036680051129w   1***c                   WHEN      DB0dcr <> *zeros  and  DB0tcr = 'D'
036690051129     ***c                   eval      Ptdr   =  'DOPO IL    '
036700051129     *** * - giorno no  ora si
036710051129w   1***c                   WHEN      DB0dcr =  *zeros  and  DB0tcr = 'P'
036720051129     ***c                   eval      Ptdr   =  'PRIMA DELLE'
036730051129w   1***c                   WHEN      DB0dcr =  *zeros  and  DB0tcr = 'D'
036740051129     ***c                   eval      Ptdr   =  'DOPO LE    '
036750051129e   1***c                   ENDSL
036760051129if  1c                   IF            DB0dcr >  *zeros
036770051129     c                             or  DB0hcr >  *zeros
036780051129     c                   clear                   FNLV80ds
036790051129     c                   eval      ILV80aas   =  DB0aas
036800051129     c                   eval      ILV80lnp   =  DB0lnp
036810051129     c                   eval      ILV80nrs   =  DB0nrs
036820051129     c                   eval      ILV80nsp   =  DB0nsp
036830051129     c                   eval      ILV80mgs   =  DB0mgs
036840051129     c                   eval      ILV80tc1   =  DB0tc1
036850051129     c                   eval      ILV80tc2   =  DB0tc2
036860051129     c                   eval      ILV80dcr   =  DB0dcr
036870051129     c                   eval      ILV80hcr   =  DB0hcr
036880051129     c                   eval      ILV80tcr   =  DB0tcr
036890051129     c                   call      'FNLV80R'
036900051129     c                   parm                    KPJBA
036910051129     c                   parm                    FNLV80ds
036920051129sel 2c                   Select
036930051129      * errore
036940051129w   2c                   When      OLV80err  <>  *blanks
036950051129      * consegna tassativa (per ora solo ai supermercati)
036960051129w   2c                   When      OLV80tat   =  'S'
036970051129     c                   movel     OLV80tcrd     Pcot
036980091116      * NO "Prima del" per il cliente TTL
036990091116w   2c                   When      DB0ccm     = c_CliTTL   and
037000091116     c                             DB0dcr    <> *zeros     and  DB0tcr = 'P'
037010051129      * prima/dopo il/delle
037020051129      * (il giorno e/o l'ora di consegna richiesta sono già impostati
037030051129      *  nella subr. "TipoLDV")
037040051129w   2c                   Other
037050051129     c                   movel     OLV80tcrd     Ptdr
037060051129e   2c                   Endsl
037070051129e   1c                   ENDIF
037080030515      *
037090030515      * DECODIFICA DEI TURNI DI CHIUSURA  - - - - - - - - - - - - - - *
037100060921      * (SE?non?è "Fermo Deposito")
037110060921if  1c                   if        DB0ffd =  *blanks
037120060921     c                   eval      Pktc   =  'TURNO DI CHIUSURA'
037130060921e   1c                   endif
037140030515      * 1° turno di chiusura
037150060921if  1c                   IF             DB0gc1 <> *blanks
037160060921     c                             and  DB0ffd  = *blanks
037170030515      * - Flag "*" > Turno di chiusura
037180030515     c                   eval      Past   =  '*'
037190030515if  2c                   If        %subst(DB0gc1:1:1) = *blank
037200030515      * - Nessun giorno specificato - Solo 'M' o 'P'
037210060921     c*** già così:      clear                   Pgc1
037220030515if  3c                   if        DB0gc1 =  ' M'
037230030515     c                   eval      Pgc1   =  %trim($Gio(8))
037240030515x   3c                   else
037250030515     c                   eval      Pgc1   =  %trim($Gio(9))
037260030515e   3c                   endif
037270030515x   2c                   Else
037280030515      * - Giorno specificato
037290030515     c                   movel     DB0gc1        w001a
037300030515     c                   eval      XX     =  1
037310030515     c                   eval      *in31  =  *off
037320030515     c     w001a         lookup    $GgS(xx)                               31
037330030515     c   31              movel     $Gio(xx)      Pgc1
037340030515      *   + 'M' o 'P'
037350030515sel 3c                   select
037360030515w   3c                   when      %subst(DB0gc1:2:1) = 'M'
037370030515     c                   eval      Pgc1   =  %trim(Pgc1) + ' '
037380030515     c                                    +  %trim($Gio(8))
037390030515w   3c                   when      %subst(DB0gc1:2:1) = 'P'
037400030515     c                   eval      Pgc1   =  %trim(Pgc1) + ' '
037410030515     c                                    +  %trim($Gio(9))
037420030515e   3c                   endsl
037430030515e   2c                   Endif
037440030515e   1c                   ENDIF
037450030515      * 2° turno di chiusura
037460060921if  1c                   IF             DB0gc2 <> *blanks
037470060921     c                             and  DB0ffd  = *blanks
037480030515      * - Flag "*" > Turno di chiusura
037490030515     c                   eval      Past   =  '*'
037500030515if  2c                   If        %subst(DB0gc2:1:1) = *blank
037510030515      * - Nessun giorno specificato - Solo 'M' o 'P'
037520060921     c*** già così:      clear                   Pgc2
037530030515if  3c                   if        DB0gc2 =  ' M'
037540030515     c                   eval      Pgc2   =  %trim($Gio(8))
037550030515x   3c                   else
037560030515     c                   eval      Pgc2   =  %trim($Gio(9))
037570030515e   3c                   endif
037580030515x   2c                   Else
037590030515      * - Giorno specificato
037600030515     c                   movel     DB0gc2        w001a
037610030515     c                   eval      XX     =  1
037620030515     c                   eval      *in31  =  *off
037630030515     c     w001a         lookup    $GgS(xx)                               31
037640030515     c   31              movel     $Gio(xx)      Pgc2
037650030515      *   + 'M' o 'P'
037660030515sel 3c                   select
037670030515w   3c                   when      %subst(DB0gc2:2:1) = 'M'
037680030515     c                   eval      Pgc2   =  %trim(Pgc2) + ' '
037690030515     c                                    +  %trim($Gio(8))
037700030515w   3c                   when      %subst(DB0gc2:2:1) = 'P'
037710030515     c                   eval      Pgc2   =  %trim(Pgc2) + ' '
037720030515     c                                    +  %trim($Gio(9))
037730030515e   3c                   endsl
037740030515e   2c                   Endif
037750030515e   1c                   ENDIF
037760071114      *
037770071114      * ALTRI DATI/FLAG/... - - - - - - - - - - - - - - - - - - - - - *
037780030515      *
037790071114      * Se pgm richiamato x stampa LdV in arrivo:
037800030515if  1c                   IF        DB0fl1 =  'A'
037810071114      * - imposto l'"*" in base al parametro ricevuto.
037820030515if  2c                   if        DB0fl2 =  '*'
037830030515     c                   eval      Past   =  '*'
037840030515x   2c                   else
037850030515     c                   clear                   Past
037860030515e   2c                   endif
037870071114      * - idem per flag di bolla non abilitata
037880030515     c                   eval      Pdis   =  DB0fl5
037890030515e   1c                   endif
037900061102      *
037910071114      * Decodifica cliente a cui richiedere il codice fiscale e/o la
037920071114      *   partita iva
037930071114if  1c                   IF             $No_CF =  *on
037940071114     c                             or   $No_PI =  *on
037950061106     c                   clear                   wRSxCF
037960061102sel 2c                   select
037970061102      * - P.F. prepagato
037980061106w   2c                   when      DB0tbo =  '8'
037990061106     c                   movel     DB0rsm        wRSxCF
038000061102      * - P.A. con fattura immediata
038010061106w   2c                   when      %subst(DB0tb1:1:1) = 'A'
038020061106     c                   movel     DB0rsd        wRSxCF
038030061106      * - con 2ª bolla
038040061103w   2c                   when           DB0tb2 <> *blanks
038050121029     c                             and  DB0tbo <> 'P'
038060061103     c                             and  DB0tbo <> '8'
038070061106     c                   movel     DB0rsd        wRSxCF
038080061102e   2c                   endsl
038090061102      *
038100061102e   1c                   ENDIF
038110030515      *
038120030515     c                   ENDSR
038130080918      /free
038140090528
038150090528       //--------------------------------------------------------------
038160140307       //?Reperimento Numero e Data del DDT originale del mittente     ?
038170140307       //?"Consorzio Lotterie Nazionali - Gratta & Vinci"              ?
038180090528       //--------------------------------------------------------------
038190090528       BEGSR  sr_RepDdtCLN;
038200090528
038210110426         // -?Verifica se la spedizione ha mamma?
038220090528         if  not %open(FNLBL01L);
038230090528           open  FNLBL01L;
038240090528         endif;
038250090528         chain  (DB0aas : DB0lnp : DB0nrs : DB0nsp)  FNLBL000;
038260090528
038270090528         Select;
038280090528
038290110426           // -?Con Mamma: reperimento dati DDT dalla mamma?
038300090528           When  %found(FNLBL01L);
038310090528             if  not %open(FNBLP01L);
038320090528               open  FNBLP01L;
038330090528             endif;
038340090603             chain  (LBLaao : LBLlpo : LBLnro : LBLnso)  FNBLP000;
038350090528             if  %found(FNBLP01L);
038360090528               exsr  sr_DatiCLN_blp;
038370090528             endif;
038380090528
038390110426           // -?Dati da DS?
038400090901           When  DB0ges = 'B';
038410090901             exsr  sr_DatiCLN_DS;
038420090901
038430110426           // -?In Partenza: reperimento dati DDT da FNBLP?
038440090528           When  (DB0tbo = 'P'   or   DB0tbo = '8');
038450090528             exsr  sr_DatiCLN_blp;
038460090528
038470110426           // -?In Arrivo: reperimento dati DDT da FNARB?
038480090528           Other;
038490090528             exsr  sr_DatiCLN_arb;
038500090528
038510090528         EndSl;
038520090528
038530090528       ENDSR;
038540090528
038550090901       //--------------------------------------------------------------
038560140307       //?Da DS                                                        ?
038570090901       //--------------------------------------------------------------
038580090901       BEGSR  sr_DatiCLN_DS;
038590090901
038600110426         // -?Numero DDT?
038610090901         select;
038620090902           when  DB0rmn <> *zero;
038630100906             if  DB0rmn  > -99999999   and
038640100906                 DB0rmn <   99999999;
038650100906               WddtCLN = DB0rmn;
038660100906             endif;
038670090902           when  DB0ncd <> *zero;
038680090902             WddtCLN = DB0ncd;
038690090901         endsl;
038700090901
038710110426         // -?Data DDT in formato gg/mm/aaaa?
038720090902         Data_Eur = %date( ((DB0aas * 10000) + DB0mgs) : *iso );
038730090901         WdspCLN  = ( ( %subdt(Data_Eur : *days)  * 1000000 ) +
038740090901                      ( %subdt(Data_Eur : *months) * 10000 ) +
038750090901                      ( %subdt(Data_Eur : *years) ) );
038760090901
038770090901       ENDSR;
038780090901
038790090528       //--------------------------------------------------------------
038800090528       //?In Partenza: reperimento dati DDT da FNBLP                   ?
038810090528       //--------------------------------------------------------------
038820090528       BEGSR  sr_DatiCLN_blp;
038830090528
038840110426         // -?Numero DDT?
038850090528         select;
038860090528           when  BLPrmn <> *zero;
038870100906             if  BLPrmn  > -99999999   and
038880100906                 BLPrmn <   99999999;
038890100906               WddtCLN = BLPrmn;
038900100906             endif;
038910090528           when  BLPncd <> *zero;
038920090528             WddtCLN = BLPncd;
038930090528         endsl;
038940090528
038950110426         // -?Data DDT in formato gg/mm/aaaa?
038960090528         Data_Eur = %date( ((BLPaas * 10000) + BLPmgs) : *iso );
038970090528         WdspCLN  = ( ( %subdt(Data_Eur : *days)  * 1000000 ) +
038980090528                      ( %subdt(Data_Eur : *months) * 10000 ) +
038990090528                      ( %subdt(Data_Eur : *years) ) );
039000090528
039010090528       ENDSR;
039020090528
039030090528       //--------------------------------------------------------------
039040090528       //?In Arrivo: reperimento dati DDT da FNARB                     ?
039050090528       //--------------------------------------------------------------
039060090528       BEGSR  sr_DatiCLN_arb;
039070090528
039080110426         // -?Numero DDT?
039090090528         select;
039100090528           when  ARBrmn <> *zero;
039110100906             if  ARBrmn  > -99999999   and
039120100906                 ARBrmn <   99999999;
039130100906               WddtCLN = ARBrmn;
039140100906             endif;
039150090528           when  ARBncd <> *zero;
039160090528             WddtCLN = ARBncd;
039170090528         endsl;
039180090528
039190110426         // -?Data DDT in formato gg/mm/aaaa?
039200090528         Data_Eur = %date( ((ARBaas * 10000) + ARBmgs) : *iso );
039210090528         WdspCLN  = ( ( %subdt(Data_Eur : *days)  * 1000000 ) +
039220090528                      ( %subdt(Data_Eur : *months) * 10000 ) +
039230090528                      ( %subdt(Data_Eur : *years) ) );
039240090528
039250090528       ENDSR;
039260080918
039270080918       //--------------------------------------------------------------
039280140307       //?Stampa della sola Packing-List (2 per A4)                    ?
039290080918       //--------------------------------------------------------------
039300080918       BEGSR  StampaPkgLst;
039310130315
039320130315         // -?SE NON esiste il *file FIAR8 => uscita?
039330130315         if  $E_AR8 = *off;
039340130315           leavesr;
039350130315         endif;
039360080918
039370110426         // -?MODIFICA SOSPESA:?
039380110426         //  ?NON STAMPA LA PACKING-LIST DAL 2° TENTATIVO DI CONSEGNA?
039390081013         //if   $FIAR8 = *off   or   DB0fl1 = 'D';
039400081013         //  leavesr;
039410081013         //endif;
039420080918
039430110426         // -?Azzeramento numero di pagina per Lettera di Vettura?
039440110426         //  ?(meglio: "reimpostazione a 1")?
039450080926         reset  wPage;
039460080918
039470110426         // -?Posizionamento sul foglio A4 in base al n° pagina in stampa?
039480130418         *in33 = *off;
039490130418         exsr  Posiz_Box;
039500080919
039510110426         // -?Impostazione dei dati da stampare nel rec. con la costante?
039520110426         //  ?"Copia per il destinatario"  + estremi spedizione e pag.?
039530110426         //
039540110426         // -?"Copia per il destinatario" già impostato in sr. "PrepPrt"?
039550081017         //if  §AR8cxd = *blank;
039560130729         //  Tcxd  = c_CopiaXdest;
039570130729         //  Tcxd2 = Tcxd;
039580081017         //endif;
039590081017
039600110426         // -?Estremi spedizione da impostare SOLO se si stampando?
039610110426         //  ?la sola packing-list (DB0FA4 = "P")?
039620081017         if  DB0fa4 = 'P';
039630101028           Trsd = Prsd;
039640080922           Trif = 'Spedizione '
039650080924                + %editc(DB0lnp : 'X') + ' '
039660110310                + %editc(DB0nrs : 'X') + ' '
039670110310                + %editc(DB0nsp : 'X')
039680080922                + ' del ' + %editc(Pdsp : 'Y')
039690081017                + '        Pag.'
039700080922                + %editc(wPage:'Z');
039710101028           Tndc = Pndc;
039720081002           if  wSeqLdV = 1;
039730081002             §Pos   = wPosIni + c_PosBox1As + 0.040;
039740090422             §Pos   = wPosIni + c_PosBox1As + 0.035;
039750081002           else;
039760081002             §Pos   = wPosIni + c_PosBox1As - 0.070;
039770081002           endif;
039780080919         endif;
039790081017
039800110426         // -?La stampa di tale record verrà fatta dopo?
039810081017         //if  §AR8cxd = *blank   or   DB0fa4 = 'P';
039820081017         //  write A4prtDT0;
039830081017         //endif;
039840080918
039850110426         // -?Stampa Packing-List?
039860130315         exsr  StampaDDT;
039870080918
039880080918       ENDSR;
039890090528
039900080918      /end-free
039910030515      *
039920030515      *---------------------------------------------------------------*
039930050217      *?Stampa della Lettera di Vettura                              ?*
039940030515      *---------------------------------------------------------------*
039950040607     c     StampaLDV     BEGSR
039960080604      *
039970080604      * - Posizionamento sul foglio A4 in base al n° pagina in stampa
039980130418     c                   eval      *in33 = *off
039990130418     c                   exsr      Posiz_Box
040000030515      *
040010040607      * Azzeramento numero di pagina per Lettera di Vettura
040020030716      * (meglio: "reimpostazione a 1")
040030080926     c                   reset                   wPage
040040030515      *
040050120530      * 1ª parte - copia per il vettore (da ritornare in "BRT")
040060130418     c                   write     A4prtLDV
040070130418     c                   write     A4prtBOX
040080110426     c*//                write     A4LogoBAR
040090081020      /free
040100120530         //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
040110120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
040120130306         //  // >?Simil-Logo Bartolini/BRT?
040130120530         //  write  A4LogoBR2;
040140120530         //  write  A4LogoDA2;
040150120530         //else;
040160130306         //  // >?Simil-Logo Bartolini?
040170120530         //  write  A4LogoBAR;
040180120530         //  write  A4LogoDA1;
040190120530         //endif;
040200130306         // >?Simil-Logo BRT?
040210120530         write  A4LogoBRT;
040220120530         write  A4LogoDA3;
040230110426
040240110426         // >?dati DPD?
040250081020         if  *in71;
040260081020           exsr  StampaDPD;
040270081020         endif;
040280130306
040290130306         // -?Se LdV con PDF => registrazione rec. nel *file PDFLIST?
040300130710         if  %parms() > 3  and  (DB6pdf = 'F'  or  DB6pdf = 'X')
040310130306                           and  $§PDF   = *on;
040320130306           exsr  sr_WrtRec_PdfList;
040330130306         endif;
040340081020
040350130306         // -?Se richiesta solo tagliatella (copia per il vettore, da?
040360130306         //  ?lasciare però al destinatario) => fine?
040370081020         if  DB0fa4  = '4';
040380081020           leavesr;
040390081020         endif;
040400081020      /end-free
040410050510      *
040420081028      * Se LdV in formato "ex-A5" rimane la copia per il destinatario
040430050510     c     *in69         cabeq     *on           End_LdV
040440030515      *
040450030515sel 1c                   select
040460081028      * > dati DPD  (ora in ex-A5 o in 1/4 di A4)
040470081028w   1c***                when      *in71
040480030515      * > dati FedEx
040490030515w   1c                   when      *in72
040500030515     c                   exsr      ListaFED
040510130710      /free
040520130710             if  DB0fa4 = 'S';
040530130710               DB0fa4 = '1';
040540130710             endif;
040550130710      /end-free
040560081028      * > dati EuroExpress  (non più gestiti)
040570040223w   1c                   when      *in73
040580030515      *
040590030515e   1c                   endsl
040600030910      *
040610030910      * > elenco SEGNACOLLI NON SEQUENZIALI
040620050511      *  (no x max 2 perchè sono già stati elencati nell'apposita area)
040630030910      *  (no x DPD   perchè monocollo)
040640030910      *  (no x FedEx perchè segnacolliamo noi)
040650030910      *  (no x "A5"  perchè sono già stati elencati tra le "Note e
040660030910      *              Particolarità")
040670030910if  1c                   if        DB0fns =  'S'
040680050511     c                             and  DB0ncl > 2
040690050513     c                             and  $A4_x_ScNS  =   *on
040700030910     c***                          and  not *in71  and  not *in72
040710030910     c                             and  not *in69
040720030910     c                   exsr      ListaScNS
040730030910e   1c                   endif
040740130710      /free
040750130710             if  DB0fa4 = 'S';
040760130710               DB0fa4 = '1';
040770130710             endif;
040780130710      /end-free
040790031124      *
040800031124      * Reperimento della posizione utilizzabile per la stampa dei box
040810080825      *   (in pollici) in base all'ultima riga stampata in 1/8" o 1/9"
040820031124     c                   exsr      CalcRowPos
040830060524      *
040840070123      * > Spedizione con D.A.S. x cliente "PRAOIL OLEODOTTI ITALIANI"
040850081127if  1c                   if             DB0ccm = c_CliPraOil
040860060529     c                             or  (DB0fbr = 'S'
040870081127     c                             and  §B4Aks = c_CliPraOil)
040880060524     c                   exsr      StampaDAS
040890130710      /free
040900130710             if  DB0fa4 = 'S';
040910130710               DB0fa4 = '1';
040920130710             endif;
040930130710      /end-free
040940060524e   1c                   endif
040950031124      *
040960031124      * > Promemoria per autotrasportatore
040970110502if  1c*//*               if            (DB0gma = Ktab7V_CB
040980110502     c*//*                           or DB0gma = Ktab7V_C1
040990080625     c*** scontato x GMA='C1'       and DB0bam = 'S'
041000110502     c*//*                           or DB0gma = Ktab7V_C2
041010080729     c*** scontato x GMA='C2'       and DB0bam = 'S'
041020110502     c*//*                           or DB0gma = Ktab7V_C3
041030100513     c*** scontato x GMA='C3'       and DB0bam = 'S'
041040110411     c*//*                           or DB0gma = Ktab7V_C4
041050110411     c*//*                           or DB0gma = Ktab7V_C5
041060110502     c*//*                           or DB0gma = Ktab7V_R1
041070110502     c*//*                           or DB0gma = Ktab7V_NR)
041080110502     c*//*                         and  §7Rldv = 'S'
041090110502     c                   if            (§7Rldv = 'S'
041100110502     c                             and  $TabPXA2  = *on)
041110061106     c                             OR   $No_CF = *on
041120071114     c                             OR   $No_PI = *on
041130070306     c                             OR   $FIARN = *on
041140070315     c                             OR   $C_A_  = *on
041150080801     c                             OR   $ConsParz = *on
041160090302     c                             OR   $TabPXA   = *on
041170110411     c                             OR   $TabPXA2  = *on
041180031124     c                   exsr      ListaPMxA
041190130710      /free
041200130710             if  DB0fa4 = 'S';
041210130710               DB0fa4 = '1';
041220130710             endif;
041230130710      /end-free
041240031124e   1c                   endif
041250030515      *
041260030515      * > dati FIAR5
041270150304if  1c*//                if           %subst(DB0gva:1:1) = 'B'
041280150304     c*//                          or %subst(DB0gva:1:1) = 'O'
041290150304if  1c                   if                  DB0tc1      = 'S'
041300030717     c                             or        DB0tc2      = 'S'
041310030515     c                   exsr      ListaAR5
041320130710      /free
041330130710             if  DB0fa4 = 'S';
041340130710               DB0fa4 = '1';
041350130710             endif;
041360130710      /end-free
041370030515e   1c                   endif
041380030910      *
041390071015      * > Riferimento O.R.M.
041400071015sel 1c                   select
041410071015      * - > per ritiro contestuale alla consegna
041420100608w   1c                   when          (§7Rrc  = 'S'
041430071015     c                             and  §7Rldv = 'S')
041440030910     c                   exsr      StampaRCC
041450130710      /free
041460130710             if  DB0fa4 = 'S';
041470130710               DB0fa4 = '1';
041480130710             endif;
041490130710      /end-free
041500071015      * - > per ritiro porto franco prepagato
041510071015w   1c                   when           DB0tbo = '8'
041520071015     c                   exsr      StampaRPFPP
041530130710      /free
041540130710             if  DB0fa4 = 'S';
041550130710               DB0fa4 = '1';
041560130710             endif;
041570130710      /end-free
041580071015e   1c                   endsl
041590090924      /free
041600090924
041610110426         // >?Note per destinatario?
041620090924if  1    if  $BoxNxD;
041630090924           exsr  StampaNxD;
041640130710           if  DB0fa4 = 'S';
041650130710             DB0fa4 = '1';
041660130710           endif;
041670090924e   1    endif;
041680090924
041690090924      /end-free
041700040607      *
041710081013      * > DDT in LDV
041720081013      * - IN FUTURO: se al 1° tentativo di consegna
041730081013      *              (DB0FL1 = "D" - v. pgm FNLRS9R)
041740080917if  1c                   if             $FIAR8 =  *on
041750130315     c                             and  $E_AR8 =  *on
041760081013     c***-=»>                      and  DB0fl1 <> 'D'
041770040607     c                   exsr      StampaDDT
041780130710      /free
041790130710             if  DB0fa4 = 'S';
041800130710               DB0fa4 = '1';
041810130710             endif;
041820130710      /end-free
041830040607e   1c                   endif
041840130710      /free
041850130710
041860130710         if  DB0fa4 = 'S';
041870130710           exsr  ListaPMxAforz;
041880130710           //*in41 = *off;
041890130710           //*in42 = *off;
041900130710           //*in43 = *on;
041910130710           //*in44 = *off;
041920130710           //write  A4prtPOS;
041930130710         endif;
041940130710      /end-free
041950050510      *
041960050510     c     End_LdV       tag
041970030515      *
041980080703      * 2ª parte - copia per il destinatario (da lasciare)
041990130418     c                   eval      *in33 = *on
042000040607     c                   exsr      ChiusuraLDV
042010030515      *
042020030515      * Stampa dei 3 asterischi finali (se richiesta)
042030030515if  1c                   if        DB0ges = '*'
042040030515     c                   move      '***'         Pmsg3
042050130418     c                   eval      *in33 = *off
042060130418     c                   exsr      Posiz_Box
042070030515     c                   write     A4prtMSG
042080030515e   1c                   endif
042090030515      *
042100030515     c                   ENDSR
042110080604      /free
042120080604
042130080604       //--------------------------------------------------------------
042140140307       //?Posizionamento del box all'interno della pag. A4             ?
042150080604       //--------------------------------------------------------------
042160130418       BEGSR  Posiz_Box;
042170080704
042180080704         select;
042190080704
042200110426           // -?Richiesta stampa di 1 LdV x pagina?
042210110426           //  ?(LdV in modifica o con packing-list del mitente)?
042220080704           when  DB0fa4 = '1'   or   DB0fa4 = 'S';
042230080704             $LdVxP   = 1;
042240080918             $Len1LdV = $LenLdV2;
042250080704
042260110426           // -?Richiesta stampa di 2 LdV x pagina (ex A5)?
042270110426           //  ?(copia per il vettore & copia per il destinatario)?
042280080704           when  DB0fa4 = '2';
042290080704             $LdVxP   = 2;
042300080704             $Len1LdV = $LenLdV2;
042310080704
042320110426           // -?Richiesta stampa di 4 LdV x pagina?
042330110426           //  ?(solo copia per il vettore)?
042340080704           when  DB0fa4 = '4';
042350080704             $LdVxP   = 4;
042360080704             $Len1LdV = $LenLdV4;
042370080918
042380110426           // -?Richiesta stampa della sola packing-list?
042390110426           //  ?(solo copia per il vettore)?
042400080918           when  DB0fa4 = 'P';
042410080918             $LdVxP   = 2;
042420080924             $Len1LdV = c_Len2_inch;
042430080704
042440080704           other;
042450080704             reset $LdVxP;
042460080704             reset $Len1LdV;
042470080704
042480080704         endsl;
042490080704
042500110426         // -?Richiesta "ripartenza" da foglio A4 successivo?
042510130418         if DB0fl6 = *on  and  Not  *in33;
042520080704           clear  wNumLdV;
042530080704         endif;
042540080704
042550110426         // -?Calcolo posizione di partenza all'interno del foglio A4.?
042560110426         //  ?[Se 2ª LdV nella pagina, ma LdV per A4 (intero):?
042570110426         //   ?si salta alla pag. A4 successiva; la LdV in esame?
042580110426         //   ?dev'essere stampata in posizione iniziale dell'A4,?
042590110426         //   ?seguita dai suoi dati ulteriori.]?
042600130418         if  Not  *in33;
042610130418           wNumLdV  += 1;
042620130418           wSeqLdV   = %rem( wNumLdV : $LdVxP );
042630130418         endif;
042640080704
042650130422         // -?Stampa della copia per il vettore?
042660130422         IF  NOT  *in33;
042670130422
042680130422           select;
042690130422
042700130422             // -?Modulo A4 intero?
042710130422             when  NOT  *in69    and   DB0fa4 <> '4'
042720130422                                 and   DB0fa4 <> 'P';
042730130422               wSeqLdV = 1;
042740130422             when  DB0fa4 = '1'   or   DB0fa4 = 'S';
042750130422               wSeqLdV = 1;
042760130422
042770130422             // -?Calcolo della posizione x box all'interno della pagina "A4"?
042780130422             when  wSeqLdV = *zero   and
042790130422                   $LdVxP  = 2;
042800130422               wSeqLdV = 2;
042810130422             when  wSeqLdV = *zero   and
042820130422                   DB0fa4  = '4';
042830130422               wSeqLdV = 4;
042840130422             when  wSeqLdV = *zero   and
042850130422                   *in69;
042860130422               wSeqLdV  = 2;
042870130422
042880130422           endsl;
042890130422
042900130422         ELSE;
042910130422
042920130422           select;
042930130422
042940130422             // -?Modulo A5 o ricevuta (¼ di A4)?
042950130422             when  *in69;
042960130422
042970130422             // -?Modulo A4 intero - 1ª parte (alta)?
042980130422             when  CurLineA4 <  26   and   §PS1 < c_§PS1;
042990130422               wSeqLdV = 1;
043000130422
043010130422             // -?Modulo A4 intero - 2ª parte (passa)?
043020130422           //when  CurLineA4 <  78;   (in 1/9", ma sarà in 1/8"...)
043030130422             when  CurLineA4 <  70;
043040130422               wSeqLdV  = 2;
043050130422
043060130422             // -?Modulo A4 intero - superata 2ª parte?
043070130422             //  ?PER ORA?IMPOSSIBILE !!!?
043080130422             other;
043090130422               clear wMaxLine;
043100130422               exsr  Test_OF;
043110130422               wMaxLine = $MaxLine2;
043120130422               wSeqLdV  = 1;
043130130422
043140130422           endsl;
043150130422
043160130422         ENDIF;
043170080919
043180110426         // -?Impostaz. delle posizioni nella pagina in corso di stampa?
043190080919         wPosIni = ($Len1LdV * (wSeqLdV - 1));
043200130422         if  *in33;       // -?Stampa della copia per il destinatario?
043210130422           wPosIni += $LenLdV4;
043220130422         endif;
043230110426
043240120530         //// -?Impostaz. dimensioni per il logo Bartolini-BRT?
043250120530         //If  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
043260120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
043270120530         //  reset  c_Spessore;
043280120530         //  reset  c_DeltaSpig;
043290120530         //  reset  c_LunghMaxV;
043300120530         //  reset  c_LunghV;
043310120530         //  reset  c_LunghO;
043320120530         //  reset  c_XspaceX;
043330120530         //  reset  c_BOXspace;
043340120530         //Else;
043350120530         //  c_Spessore  = c_1_Spessore;
043360120530         //  c_DeltaSpig = c_1_DeltaSpig;
043370120530         //  c_LunghMaxV = c_1_LunghMaxV;
043380120530         //  c_LunghV    = c_1_LunghV;
043390120530         //  c_LunghO    = c_1_LunghO;
043400120530         //  c_XspaceX   = c_1_XspaceX;
043410120530         //  c_BOXspace  = c_1_BOXspace;
043420120530         //EndIf;
043430120530         // -?Dimensioni per il logo BRT già definite?
043440120530         //c_Spessore  = c_2_Spessore;
043450120530         //c_DeltaSpig = c_2_DeltaSpig;
043460120530         //c_LunghMaxV = c_2_LunghMaxV;
043470120530         //c_LunghV    = c_2_LunghV;
043480120530         //c_LunghO    = c_2_LunghO;
043490120530         //c_XspaceX   = c_2_XspaceX;
043500120530         //c_BOXspace  = c_2_BOXspace;
043510080704
043520110426         // -?Se richiesta la stampa della sola packing-list?
043530110426         //  ?NON si dovranno stampare i dati della LdV?
043540110426         //  ?(quindi NON servono i posizionamenti che seguono)?
043550080918         if  DB0fa4 = 'P';
043560080918           leavesr;
043570080918         endif;
043580080918
043590110426         // -?Posizionamento nel foglio A4 in base al n° LdV in stampa?
043600110426         //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
043610110426
043620130418         select;
043630130418           when  DB0fa4 = '4';
043640130418             *in41 = (wSeqLdV = 1);
043650130418             *in42 = (wSeqLdV = 2);
043660130418             *in43 = (wSeqLdV = 3);
043670130418             *in44 = (wSeqLdV = 4);
043680130418           when Not *in33;
043690130418             *in41 = (wSeqLdV = 1);
043700130418             *in42 = *off;
043710130418             *in43 = (wSeqLdV = 2);
043720130418             *in44 = *off;
043730130418           other;
043740130418             *in41 = *off;
043750130418             *in42 = (wSeqLdV = 1);
043760130418             *in43 = *off;
043770130418             *in44 = (wSeqLdV = 2);
043780130418         endsl;
043790080704         WRITE  A4prtPOS;
043800080704
043810110426         // -?Posizionamenti nella copia per il vettore?
043820110426         //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
043830080704
043840110426         // -?Box in copia per il vettore?
043850080704         §Box1As = wPosIni + c_PosBox1As;
043860140922         §Box1Ai = wPosIni + c_PosBox1Bs  + c_LineNarrow;
043870080704         §Box1Bs = wPosIni + c_PosBox1Bs;
043880080704         §Box1Bi = wPosIni + c_PosBox1Bi;
043890140922         §Box1Bj = wPosIni + c_PosLin1oD1 + c_LineNarrow;
043900080704         §Box1Cs = wPosIni + c_PosBox1Cs;
043910140922         §Box1Ci = wPosIni + c_PosBox1Ds  + c_LineWide;
043920080704         §Box1Ds = wPosIni + c_PosBox1Ds;
043930091119         §Box1Di = §Box1Bi;
043940081218
043950120530         //If  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
043960120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
043970120530         //  // -?Simil-Logo Bartolini?
043980120530         //  §PlblS  = §Box1As + 0.125;
043990120530         //  §PlblVS = 7.092;
044000120530         //  exsr  sr_SimilLogoBAR;
044010120530         //  // -?Simil-Logo BRT?
044020120530         //  §P2blS  = §PlblS;
044030120530         //  §P2blVS = §PlblVD + 0.030;
044040120530         //  exsr  sr_SimilLogoBRT;
044050120530         //Else;
044060120530         //  // -?Simil-Logo Bartolini?
044070120530         //  §PlblS  = §Box1As;
044080120530         //  §PlblVS = 7.128;
044090120530         //  exsr  sr_SimilLogoBART;
044100120530         //EndIf;
044110120530         // -?Simil-Logo BRT?
044120120530         §PlblS  = §Box1As;
044130120530         §PlblVS = 7.581;
044140120530         exsr  sr_SimilLogoBRT_;
044150080704
044160120530         // -?Costanti dati anagrafici BRT?
044170110426         §PC0    = wPosIni + c_PosTBar;
044180110426         §PC1    = wPosIni + c_PosTBa1;
044190120530         //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
044200120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
044210120530         //  §PC2    = wPosIni + c_PosTBa2;
044220120530         //  §PC3    = wPosIni + c_PosTBa3;
044230120530         //  §PC4    = wPosIni + c_PosTBa4;
044240120530         //  §PC5    = wPosIni + c_PosTBa5;
044250120530         //  §PC6    = wPosIni + c_PosTBa6;
044260120530         //else;
044270120530         //  §PC2    = wPosIni + c_PosTBa2x;
044280120530         //  §PC3    = wPosIni + c_PosTBa3x;
044290120530         //  §PC4    = wPosIni + c_PosTBa4x;
044300120530         //  §PC5    = wPosIni + c_PosTBa5x;
044310120530         //  §PC6    = wPosIni + c_PosTBa6x;
044320120530         //endif;
044330120530         §PC2    = wPosIni + c_PosTBa2x;
044340120530         §PC3    = wPosIni + c_PosTBa3x;
044350120530         §PC4    = wPosIni + c_PosTBa4x;
044360120530         §PC5    = wPosIni + c_PosTBa5x;
044370120530         §PC6    = wPosIni + c_PosTBa6x;
044380130729         §PC7    = §Box1Bi - 0.150;
044390110426
044400110426         // -?Linee orizzontali nel box in copia per il vettore?
044410080704         §Lo1S1  = wPosIni + c_PosLin1oS1;
044420080704         §Lo1S2  = wPosIni + c_PosLin1oS2;
044430080922       //§Lo1S3  = wPosIni + c_PosLin1oS3;
044440080704         §Lo1S4  = wPosIni + c_PosLin1oS4;
044450091119         §Lo1S5  = wPosIni + c_PosLin1oS5;
044460080704         §Lo1C1  = wPosIni + c_PosLin1oC1;
044470080704         §Lo1C2  = wPosIni + c_PosLin1oC2;
044480140917         §Lo1C3  = wPosIni + c_PosLin1oC3;
044490080704         §Lo1D1  = wPosIni + c_PosLin1oD1;
044500080704
044510110426         // -?Lunghezza linee verticali nel box in copia per il vett.?
044520080704         §LLv1A  = §Lo1S5  - §Lo1S1;
044530140922         //§LLv1B  = §Lo1S4  - §Box1Bs;
044540140922         //§LLv1C  = §Box1Di - §Box1Bs;
044550140923         //***§LLv1B  = §Lo1S4  - §Box1Bj + c_LineNarrow;
044560140923         §LLv1B  = §Lo1S4  - §Box1Bj;
044570140922         §LLv1C  = §Box1Di - §Box1Bj;
044580140917
044590140917         // -?Costante "BRTcode" & codice BRTcode?
044600140917         §Pk1    = wPosIni + c_PosKBRTc;
044610080704
044620110426         // -?Costante "Consegna tassativa"?
044630080704         §Pk2    = wPosIni + c_PosKTass;
044640080704
044650110426         // -?Costanti "(nome e cognome in stampatello)", "(firma)" e?
044660110426         //           ?"(data e ora)"?
044670080704         §Pk3    = §Box1Bi - 0.015;
044680080704
044690110426         // -?Costante con rif. alla legge in vigore per RCV?
044700080704         §Pk4    = wPosIni + c_PosKRCV;
044710080704
044720110426         // -?Costante "Bollo assolto in modo virtuale..."?
044730080704         §Pk5    = wPosIni + c_PosKBAV;
044740080704         §Pk6    = §Pk5 + 0.060;
044750121017
044760121017         // -?Partita IVA o Cod. Fiscale del cliente?
044770121017         §Pk7    = §Pk4 + 0.080;
044780080704
044790110426         // -?Costante "COPIA PER IL DESTINATARIO" in DDT "generico"?
044800080709         §Pos    = c_PosKCXD2;
044810080704
044820080704       ENDSR;
044830080605
044840080604      /end-free
044850110426
044860140307       //--------------------------------------------------------------
044870140307       //?Posizionamento del simil-logo Bartolini (unico)              ?
044880140307       //--------------------------------------------------------------
044890120530      *///copy gaitrasrc/srcTNSY,LogoBAR_R2
044900110426
044910140307       //--------------------------------------------------------------
044920140307       //?Posizionamento del simil-logo Bartolini (con BRT)            ?
044930140307       //--------------------------------------------------------------
044940120530      *///copy gaitrasrc/srcTNSY,LogoBAR3R2
044950110426
044960140307       //--------------------------------------------------------------
044970140307       //?Posizionamento del simil-logo BRT (con BARTOLINI)            ?
044980140307       //--------------------------------------------------------------
044990120530      *///copy gaitrasrc/srcTNSY,LogoBRT3R2
045000120530
045010140307       //--------------------------------------------------------------
045020140307       //?Posizionamento del simil-logo BRT (unico)                    ?
045030140307       //--------------------------------------------------------------
045040120530      /copy gaitrasrc/srcTNSY,LogoBRT_R2
045050110426
045060030515      *---------------------------------------------------------------*
045070050217      *?Gestione overflow                                            ?*
045080030515      *---------------------------------------------------------------*
045090030515     c     Test_OF       BEGSR
045100030515      *
045110030515      * Test raggiungimento della posizione limite
045120081003     c                   if          ((§PS1 < $LimOF   and   NOT *in45)
045130081003     c                             or (§PS1 < ($LimOF - c_Len1_inch)
045140081003     c                                     and *in45   and   NOT *in46)
045150081003     c                             or (§PS1 < ($LimOF + c_Len1_inch)
045160081003     c                                     and *in45   and   *in46))
045170080924     c                             and CurLineA4 < wMaxLine
045180080919     c                   leavesr
045190030515e   1c                   endif
045200030515      *
045210030515      * Stampa del box per dati da FIAR5
045220030515      * (se è già stata stampata almeno una riga di tale box)
045230060921if  1c                   if        $FIAR5 = *on  and  $First = *off
045240030911     c                   eval      §LO2   = §PS1 + 0.050
045250071008if  2c                   if        NOT *in52
045260030515     c                   write     A4prtAR53
045270071008x   2c                   else
045280071008     c                   eval      §PS2   = §LO2 - 0.500
045290071008     c                   write     A4prtAR54
045300071008e   2c                   endif
045310030515e   1c                   endif
045320041209      *
045330041209sel 1c                   select
045340040607      *
045350041209      * DDT KARNAK/TITANEDI/EDK
045360041209      * Stampa di logo, box, linee e testi fissi del DDT del mittente
045370040607      * (se si stà stampando tale DDT)
045380040607      * (ma NON per la parte "merce omaggio")
045390040607      * calcolando - prima - le posizioni iniziale e finale del box
045400041209w   1c                   when           $DDT_p    = *on
045410071008     c                             and  §AR8tsd   = 'K1'
045420040607     c                             and  $DDT_o    = *off
045430040608      * - Stampa parte finale DDT
045440040608if  2c                   if        $DDTlastPg = *off
045450041209      * - - Calcolo della posizione finale del box
045460040607     c                   eval      PCnv2  = '          '
045470040607     c                                    + '*** SEGUE ***'
045480040608     c                   exsr      Posiz_DK
045490040607     c                   write     A4prtDK4
045500040608x   2c                   else
045510041209      * - - Stampa dei dati in chiusura DDT (se ultima pagina)
045520040608      *     (la posizione finale del box verrà lì calcolata)
045530050217     c                   exsr      PrtDdtKARN_V2
045540040608e   2c                   endif
045550040607      * - Stampa logo, box, linee e testi fissi
045560081017     c                   if            §AR8cxd = *blanks
045570081017     c                             or  DB0fa4  = 'P'
045580050218     c                   write     A4prtDT0
045590080923     c                   endif
045600040607     c                   write     A4prtDK0
045610041019      *
045620050414      * DDT LOGISTICA/SAIT Abrasivi
045630050215      * Stampa di logo, box, linee e testi fissi del DDT del mittente
045640041019      * (se si stà stampando tale DDT)
045650041209w   1c                   when           $DDT_p    = *on
045660060627     c                             and  %subst(§AR8tsd:1:1) = 'L'
045670041021     c                   exsr      Posiz_DL
045680050217      * - - Chiusura box in fase di stampa dettaglio DDT
045690050217      *     (la stampa dei dati in chiusura DDT sarà solo nell'ultima
045700050318      *      pagina - prima dell'eventuale packing-list)
045710050318      *     La gestione del salto alla 1ª pagina della packing-list
045720050321      *     (AR8trk = 'Z1' & Page = *zeros) avviene nella subr.
045730050318      *     "PrtDdtLOG1_Z1" (vedi subr. "PrtPckLst_1").
045740081017     c                   if            §AR8cxd = *blanks
045750081017     c                             or  DB0fa4  = 'P'
045760050308     c                   write     A4prtDT0
045770050331     c                   endif
045780050318sel 2c                   select
045790050505      *     - si sta stampando il DDT Logistica o Sait
045800050318w   2c                   when      %subst(AR8trk:1:1) <> 'Z'
045810071008     c                             and %subst(AR8trk:1:1) <> 'S'
045820050218     c                   write     A4prtDL0A
045830050615      *     - si sta stampando la Packing-List
045840050318w   2c                   when      %subst(AR8trk:1:1) =  'Z'
045850050615     c                             and    Page        >  *zeros
045860050318     c                   write     A4prtDL0C
045870050606      *     - si sta stampando la Fattura (oltre 1ª pag.)
045880071008w   2c                   when      %subst(AR8trk:1:1) =  'S'
045890081003     c                             and    Page        >  *zeros
045900050609     c                   write     A4prtDL0E
045910050318e   2c                   endsl
045920050113      *
045930050113      * DDT ARCTE
045940050215      * Stampa di logo, box, linee e testi fissi del DDT del mittente
045950050113      * (se si stà stampando tale DDT)
045960050113w   1c                   when           $DDT_p    = *on
045970050113     c                             and  AR8tmd    = 'ARCT'
045980050114     c                   exsr      Posiz_AC
045990050217      * - - Chiusura box in fase di stampa dettaglio DDT
046000050217      *     (la stampa dei dati in chiusura DDT sarà solo nell'ultima
046010050217      *      pagina)
046020081017     c                   if            §AR8cxd = *blanks
046030081017     c                             or  DB0fa4  = 'P'
046040050331     c                   write     A4prtDT0
046050050331     c                   endif
046060050217     c                   write     A4prtAC0A
046070050215      *
046080050215      * DDT ESPRINET
046090050215      * Stampa di box, linee e testi fissi del DDT del mittente
046100050215      * (se si stà stampando tale DDT)
046110050215      * · Solo nella 1ª pagina vengono stampati il logo e la parte
046120050215      *        superiore del DDT.
046130050215      * · Solo nell'ultima pagina vene stampato il "fondo".
046140050215w   1c                   when           $DDT_p    = *on
046150060627     c                             and  §AR8tsd   = 'E1'
046160110405if 1bc                   if        %subst(SAVtrk:1:1 ) <> 'P'
046170110405     c                             or   %eof(FIAR801L)
046180050215      * - Stampa parte finale DDT
046190050215      * - - Calcolo delle posizioni di box & linee
046200050215     c                   exsr      Posiz_ES
046210050215      * - - Chiusura box in fase di stampa dettaglio DDT
046220050215      *     (la stampa dei dati in chiusura DDT sarà solo nell'ultima
046230050215      *      pagina)
046240050215if  2c***                if        %subst(AR8trk:1:1) <> 'P'
046250081017     c                   if            §AR8cxd = *blanks
046260081017     c                             or  DB0fa4  = 'P'
046270050331     c                   write     A4prtDT0
046280050331     c                   endif
046290050215     c                   write     A4prtES0A
046300050215e   2c***                endif
046310110405e  1bc                   endif
046320041209      *
046330041209      * DDT GENERICO con LOGO
046340050215      * Stampa di logo e box del DDT del mittente
046350041209      * (se si stà stampando tale DDT)
046360041209w   1c                   when           $DDT_p    = *on
046370041209     c                             and  §AR8tsd   = 'S2'
046380041209     *** * - Chiusura del box con i dati di mittente e destinatario
046390041209     *** *   (Se si stanno ancora stampando 'sti dati: bisogna calcolare
046400041209     *** *    la posizione di chiusura temporanea del box)
046410041209if  2***c                   if             AR8trk =  'B1'
046420041209     ***c                             or   AR8trk =  'B2'
046430041209     *** *   Impostazione della posizione finale per il box
046440041209     ***c                   exsr      Rep_§LO2
046450041209     ***c                   eval      §PB2   = §LO2 + 0.065
046460041209e   2***c                   endif
046470041209     *** *   Stampa del box
046480041209     ***c                   write     A4prtDB0
046490041209      * - Il logo è già stato SICURAMENTE stampato
046500041209     c***                exsr      Rep_§LO2
046510041209     c***                eval      §PB1   = §LO2
046520041209      *** Stampa del logo
046530041209     c***                write     A4prtDB0
046540041209      *
046550041209e   1c                   endsl
046560030515      *
046570030515      * Stampa dell'indicazione "Segue..." su pagina corrente
046580030515     c                   clear                   Pseg0
046590030515     c                   clear                   Pseg1
046600030515     c                   eval      Pseg9  = '  Segue...'
046610080919      /free
046620080923         select;
046630080923           when  *in45   and   NOT *in46;
046640080923             §PSA = 0.200;   // Per campo vuoto !
046650080923             §PSB = 0.200;   // Per campo vuoto !
046660080923             §PSC = c_Len2_inch - 0.050;
046670080923           when  *in45   and   *in46;
046680080923             §PSA = c_Len2_inch + 0.200;   // Per campo vuoto !
046690080923             §PSB = c_Len2_inch + 0.200;   // Per campo vuoto !
046700080924             §PSC = c_Len2_inch * 2 - 0.200;
046710080923           when  NOT *in45;
046720080923             §PSA = 0.400;
046730080923             §PSB = 0.700;
046740080923             §PSC = 8.750;
046750080923         endsl;
046760080919      /end-free
046770030515     c                   write     A4prtSEG
046780030515      *
046790031031      * Se sono ancora nella prima pagina =>
046800030515      * stampo anche i dati per il destinatario e box/barcode/ecc.
046810080604sel 1c                   select
046820080923      * > Se NON sto stampando un DDT del mittente
046830080922      *   (nel qual caso non stampo null'altro)
046840080923      *   => nulla
046850080919w   1c                   when      wPage  > 1
046860080923      * > Se sto stampando le sole copie per il vettore (4 in A4)
046870080923      *    o la relativa packing-list
046880080923      *   (nel qual caso non stampo null'altro)
046890080923      *   => nulla
046900081003w   1c                   when      DB0fa4 = '4'   or   DB0fa4 = 'P'
046910040607      * > Se NON è ristampa in arrivo per recapito c/assegni  e
046920040707      *   se NON sto stampando una LdV Poste
046930080703      *   se NON sto stampando (anche) un DDT del mittente
046940080703      *   se NON sto stampando le sole copie per il vettore (4 in A4)
046950080923      *   => posizionamento e stampa
046960120113w   1c                   when          $No_x_Cons =  *off
046970081013     c                             and ($FIAR8    =  *off)
046980081013     c***-=»>                       or  DB0fl1    =  'D')
046990080605      * - Posizionamento sul foglio A4 in base al n° pagina in stampa
047000130418     c                   eval      *in33 = *on
047010130418     c                   exsr      Posiz_Box
047020130418      * - Stampa 2ª parte della LdV (copia per il destinatario)
047030130418     c                   write     A4prtLDV
047040031031      * - Stampa "cornice" / barcode / righine varie in parte inferiore
047050130418     c                   write     A4prtBOX
047060110426     c*//                write     A4LogoBAR
047070110426      /free
047080120530         //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
047090120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
047100120530         //  // >?Simil-Logo Bartolini/Brt?
047110120530         //  write  A4LogoBR2;
047120120530         //  write  A4LogoDA2;
047130120530         //else;
047140120530         //  // >?Simil-Logo Bartolini?
047150120530         //  write  A4LogoBAR;
047160120530         //  write  A4LogoDA1;
047170120530         //endif;
047180120530         // >?Simil-Logo BRT?
047190120530         write  A4LogoBRT;
047200120530         write  A4LogoDA3;
047210110426      /end-free
047220120113      *//* > Se sto stampando una LdV Poste per recapito c/assegni
047230120113w   1c*//                   when          $No_x_Cons =  '2'
047240120113      *//* - Stampa 2ª parte "nulla" della LdV
047250130418     c*//                   eval      *in33 = *on
047260130418     c*//                   exsr      Posiz_Box
047270120113     c*//                   write     A4prtLDVB
047280080604e   1c                   endsl
047290080923      /free
047300110426         // -?Se richiesta la stampa della sola packing-list:?
047310110426         //  ?incremento del n° spedizione per calcolare correttam.?
047320110426         //  ?il n° di pagina iniziale per la spediz. successiva?
047330080925         if  DB0fa4 = 'P';
047340080925             wNumLdV += 1;
047350080925         endif;
047360080925
047370110426         // -?Reimpostazione degli indicatori per il posizionamento?
047380110426         //  ?all'interno del foglio A4?
047390080923         *in45 = (DB0fa4 = 'P');
047400080923         *in46 = (DB0fa4 = 'P'   and   %rem(wNumLdv : 2) = *zero);
047410080923      /end-free
047420030515      *
047430030515      * Salta a pagina successiva e indica "(Seguito)"
047440030515     c                   write     A4prtSP0
047450030515     c                   eval      wPage  = wPage + 1
047460080923      /free
047470081017         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
047480081017           write  A4prtNUL;
047490081017         endif;
047500081017
047510080923         IF  DB0fa4 <> 'P';
047520080922      /end-free
047530030515     c                   eval      Pseg0  = 'Spedizione '
047540110310     c                                    + %editc(DB0lnp : 'X') + ' '
047550110310     c                                    + %editc(DB0nrs : 'X') + ' '
047560110310     c                                    + %editc(DB0nsp : 'X')
047570030515     c                                    + ' del ' + %editc(Pdsp : 'Y')
047580030515     c                   eval      Pseg1  = '(Seguito)             Pag.'
047590030515     c                                    + %editc(wPage:'Z')
047600030515     c                   clear                   Pseg9
047610030515     c                   write     A4prtSEG
047620080922      /free
047630080923         ELSE;
047640081017
047650110426           // -?"Copia per il destinatario" già impostato in sr. "PrepPrt"?
047660081017           //if  §AR8cxd = *blank;
047670130729           //  Tcxd  = c_CopiaXdest;
047680130729           //  Tcxd2 = Tcxd;
047690081017           //endif;
047700081017
047710110426           // -?Estremi spedizione da impostare SOLO se si stampando?
047720110426           //  ?la sola packing-list (DB0FA4 = "P")?
047730081017           if  DB0fa4 = 'P';
047740101028             //Trsd = Prsd;   (già impostato nella subr. StampaPkgLst)?
047750081017             Trif = 'Spedizione '
047760081017                  + %editc(DB0lnp : 'X') + ' '
047770110310                  + %editc(DB0nrs : 'X') + ' '
047780110310                  + %editc(DB0nsp : 'X')
047790081017                  + ' del ' + %editc(Pdsp : 'Y')
047800081017                  + '        Pag.'
047810101028                  + %editc(wPage:'Z');
047820101028                //+ '        (Seguito)';
047830101028             //Tndc = Pndc;   (già impostato nella subr. StampaPkgLst)?
047840081017           endif;
047850080923
047860110426           // -?Verifica se la 2ª parte della packing-list sta nella?
047870110426           //  ?pag. attuale o se deve andare in quella successiva?
047880080923           wSeqLdV  = %rem( wNumLdV : $LdVxP );
047890080923           if  wSeqLdV = *zero   and   $LdVxP  = 2;
047900080923             wSeqLdV = 2;
047910080923           endif;
047920080923
047930110426           // -?Posizionamenti nella pagina in corso di stampa?
047940110426           //  ?(§POS = posizione verticale della costante "COPIA PER?
047950110426           //   ?IL DESTINATARIO" (con estremi spedizione e pag.)?
047960080923           wPosIni  = ($Len1LdV * (wSeqLdV - 1));
047970080924           if  wSeqLdV = 1;
047980080924             §Pos   = wPosIni + c_PosBox1As + 0.040;
047990090422             §Pos   = wPosIni + c_PosBox1As + 0.035;
048000080924           else;
048010080924             §Pos   = wPosIni + c_PosBox1As - 0.070;
048020080924           endif;
048030081017
048040081017           if  §AR8cxd = *blank   or   DB0fa4  = 'P';
048050081017             write A4prtDT0;
048060081017           endif;
048070080923
048080080923         ENDIF;
048090080922      /end-free
048100030515      *
048110030515      * Reimposta posizioni iniziali di stampa per linee
048120030515      *   (orizzontali-§LOn e verticali-§LVn) e campi (§PSn).
048130030515      *   Nelle pagine dalla 2ª in poi si parte da in alto:
048140030515      *   non vengono ri-stampati i 2 box "vettore" e "destinatario".
048150030515sel 1c                   select
048160120530      * - BRT
048170030515w   1c                   when      *in70
048180080711     c                   eval      §LO1   = c_§PS1
048190030515     c                   eval      §LO2   = §LO1
048200030515     c                   eval      §PS1   = §LO1
048210040607sel 2c                   select
048220050217      * - - con DDT del mittente KARNAK / TITANEDI / EDK
048230040607w   2c                   when           $DDT_p    = *on
048240071008     c                             and  §AR8tsd   = 'K1'
048250040608     c                   exsr      NewPageDK
048260050414      * - - con DDT del mittente LOGISTICA / SAIT ABRASIVI
048270041207w   2c                   when           $DDT_p    = *on
048280060627     c                             and  %subst(§AR8tsd:1:1) = 'L'
048290050321     c                   exsr      NewPageDL
048300050113      * - - con DDT del mittente ARCTE
048310050113w   2c                   when           $DDT_p    = *on
048320050113     c                             and  AR8tmd    = 'ARCT'
048330081002     c                   exsr      NewPageAC
048340050215      * - - con DDT del mittente ESPRINET
048350050215w   1c                   when           $DDT_p    = *on
048360060627     c                             and  §AR8tsd   = 'E1'
048370110405if  2c                   if        %subst(SAVtrk:1:1 ) <> 'P'
048380110405     c                             or   %eof(FIAR801L)
048390081002     c                   exsr      NewPageES
048400110405e   2c                   endif
048410041207      * - - con DDT generico (con marchio)
048420041207w   2c                   when           $DDT_p    = *on
048430041207     c                             and  §AR8tsd   = 'S2'
048440081017     c                   exsr      NewPageSTD
048450041207      * - - con DDT generico (senza marchio)
048460041207w   2c                   when           $DDT_p    = *on
048470041207     c*** ("scontato":)            and  §AR8tsd   = 'S1'
048480081017     c                   exsr      NewPageSTD
048490040607e   2c                   endsl
048500080922      * - DPD (ora in ex-A5)
048510080922w   1c***                when      *in71
048520030515      * - FedEx
048530030515w   1c                   when      *in72
048540030709     c                   eval      §LO1   = 1.600
048550030515     c                   eval      §LO2   = §LO1 + 0.425
048560030515     c                   eval      §LV1   = §LO1 + 0.300
048570030515     c                   eval      §LV2   = §LV1 + 0.070
048580030515     c                   write     A4prtFED1
048590030515do  2c                   dow       CurLineA4 < $MinLine1
048600030515     c                   write     A4prtNUL
048610030515e   2c                   enddo
048620030515     c                   write     A4prtFED2
048630030515     c                   eval      §LO1   = §LO2 - 0.250
048640030911     c                   eval      §PS1   = §LO1 + 0.150
048650040223      * - EuroExpress
048660040223w   1c                   when      *in73
048670030515      *
048680030515e   1c                   endsl
048690030515      *
048700030515      * N.B.: Per i SegnaColli non sequenziali non servono queste
048710030515      *       impostazioni: trattasi di stampa con salto riga -skipb-
048720030515      *       quindi sono già posizionato.
048730030515      *
048740080919     c                   ENDSR
048750030910      *
048760030910      *---------------------------------------------------------------*
048770050217      *?Reperimentoo della posizione (in pollici) in cui posizionarsi?*
048780050217      *?* per la stampa in base all'ultima riga stampata             ?*
048790030910      *---------------------------------------------------------------*
048800030910     c     Rep_§LO2      BEGSR
048810030910      *
048820030910sel 1c                   select
048830080825      *
048840080825      * Se ho stampato l'elenco dei segnacolli non sequenziali, devo
048850080825      *   "allineare" il posizionamento in pollici all'ultima riga
048860080825      *   stampata (in ottavi di pollice).
048870080825      *   =>   Num_ultima_riga_stampata / 8
048880080825w   1c                   when           DB0fns     = 'S'
048890080825     c                             and  DB0ncl     > 2
048900080825     c                             and  $A4_x_ScNS = *on
048910080825     c                             and  NOT  *in69
048920080825     c                   eval(h)   §LO2   = (CurLineA4 / 8) + 0,1
048930040607      *
048940040607      * Se ho stampato il DDT del mittente
048950080714      *   =>   Num_ultima_riga_stampata / 9
048960081013w   1c                   when           $FIAR8 =  *on
048970081013     c***-=»>                      and  DB0fl1 <> 'D'
048980080708     c                   eval(h)   §LO2   = (CurLineA4 / 9) + 0,1
048990030910      *
049000030910      * Se non ho stampato né elenco segnacolli non sequenziali né
049010040223      *   altri box, devo impostare comunque i dati per il posiziona-
049020030910      *   mento di questo primo box
049030030910w   1c                   when      §LO2   = *zeros
049040080624     c                   eval      §LO1   = 2.850
049050030910     c                   eval      §LO2   = §LO1
049060030910      *
049070030910e   1c                   endsl
049080030910      *
049090030910     c                   ENDSR
049100031124      *
049110031124      *---------------------------------------------------------------*
049120050217      *?Reperim. della posizione (in pollici) utlizzaile per la      ?*
049130080714      *?  stampa dei box in base all'ultima riga stampata in 1/9"    ?*
049140031124      *---------------------------------------------------------------*
049150031124     c     CalcRowPos    BEGSR
049160031124      *
049170031124      * Imposto le posizioni inziali per il primo dei box
049180080624     c                   select
049190080624      *
049200040607      * - in DDT del mittente: dopo eventuale elenco merce omaggio
049210120530      *   (come in LDV BRT)
049220120530      * - in LDV BRT: dopo eventuale elenco segnacolli non seq.
049230080624     c                   when      *in70
049240080624     c                   exsr      Rep_§LO2
049250080624      *
049260080922      * - in LDV DPD (ora in ex-A5)
049270080922     c***                when      *in71
049280080624      *
049290040607      * - in LDV FEDEX: posizione variabile
049300031124      *   (dipende dal numero dei segnacolli elencati: intanto
049310031124      *   posiziona la "base di partenza" sull'ultima posizione
049320031124      *   eventualmente usata per l'elenco dei segnacolli FedEx;
049330031124      *   poi si ri-calcoleranno in base al numero di righe stampate)
049340080624     c                   when      *in72
049350080624     c                   eval      §LO2   = §LO1 + 0.250
049360080624      *
049370080912      * - in LDV EUROEXPRESS: non più gestita
049380080624     c                   when      *in73
049390080912      *
049400080624     c                   endsl
049410080624      *
049420031124     c                   eval      §LO1   = §LO2
049430031124      *
049440031124     c                   ENDSR
049450130710      /free
049460130710       //--------------------------------------------------------------
049470130710       //?Forza un box "Promemoria per autotrasportatore"              ?
049480130710       //--------------------------------------------------------------
049490130710       BEGSR  ListaPMxAforz;
049500130710
049510130710         // -?Stampa testata?
049520130710         $First = *on;
049530130710         Par5   = '                    '
049540130710                + '                    '
049550130710                + '          '
049560130710                + C_PMxA;
049570130710         exsr  StampaAR5;
049580130710
049590130710         // -?Memorizza posizione iniziale per il box?
049600130710         //  ?(§PB1 sarà da reimpostare in §LO1 PRIMA della stampa del box)?
049610130710         §PB1   = §LO1;
049620130710
049630130710         // -?Stampa testo?
049640130710         §LO1   = §PS1 + 0.050;
049650130710         write  A4prtLO;
049660130710         §PS1   = §LO1;
049670130710         $First = 'G';
049680130710         Par5   = 'Testo FITTIZIO per FORZARE il modulo A4.';
049690130710         exsr  StampaAR5;
049700130710
049710130710         // -?Stampa (finale) box?
049720130710         //  ?(§PB1 è da reimpostare in §LO1: contiene il suo valore PRIMA?
049730130710         //  ?della stampa delle varie linee separatrici)?
049740130710         §LO1   = §PB1;
049750130710         §LO2   = §PS1 + 0.070;
049760130710         write  A4prtAR53;
049770130710
049780130710       ENDSR;
049790130710      /end-free
049800060524      *
049810060524      *---------------------------------------------------------------*
049820060524      *?Stampa box per riferimento al                                ?*
049830060524      *?DOCUMENTO di ACCOMPAGNAMENTO SEMPLIFICATO                    ?*
049840060524      *---------------------------------------------------------------*
049850060524     c     StampaDAS     BEGSR
049860060524      *
049870060524     c***                eval      Par5   = '*...+....1....+....2....+-
049880060524     c***                          ....3....+....4....+....5....+....6-
049890060524     c***                          ....+....7....+....8....+....9....+-
049900060524     c***                          ....A'
049910060524      *
049920060524      * - stampo testata
049930090302     ***c                   eval      $First = 'B'
049940090302     ***c                   eval      Par5   = C_PMxA
049950090302      /free
049960090302         $First = *on;
049970090302         Par5   = '                    '
049980090302                + '                    '
049990090302                + '          '
050000090302                + C_PMxA;
050010090302      /end-free
050020060524     c                   exsr      StampaAR5
050030090302      /free
050040090302
050050090302         // - memorizza posizione iniziale per il box
050060090302         //   (§PB1 sarà da reimpostare in §LO1 PRIMA della stampa del box)
050070090302         §PB1   = §LO1;
050080090302
050090090302         // - stampa linea separatrice
050100090302         §LO1   = §PS1 + 0.050;
050110090302         write  A4prtLO;
050120090302         §PS1   = §LO1;
050130090302      /end-free
050140060524      *
050150060524      * - stampo 1° testo
050160060524     c                   eval      $First = '2'
050170060524     c                   eval      Par5   = 'CONSEGNARE AL DESTINATARI-
050180060524     c                             O D.D.T. E D.A.S. (Documento di Acc-
050190060524     c                             ompagnamento Semplificato).'
050200060524     c                   exsr      StampaAR5
050210060524      *
050220060524      * - stampo 2° testo
050230061030     c*** (già fatto:)   eval      $First = *off
050240060524     c                   eval      Par5   = 'In caso di RIFIUTO compil-
050250060524     c                             are l''ultima pagina del D.A.S. ind-
050260060524     c                             icando:'
050270060524     c                   exsr      StampaAR5
050280060524     c                   eval      Par5   = 'a) nome Autotrasportatore-
050290060524     c                              nel campo "IL SOTTOSCRITTO ... INC-
050300060524     c                             ARICATO DEL TRASPORTO"'
050310060524     c                   exsr      StampaAR5
050320060524     c                   eval      Par5   = 'b) firma Autotrasportator-
050330060524     c                             e nel campo "FIRMA INCARICATO DEL T-
050340060524     c                             RASPORTO"'
050350060524     c                   exsr      StampaAR5
050360060524     c                   eval      Par5   = 'c) causale del rifiuto (b-
050370060524     c                             arrare "la merce non è conforme all-
050380060524     c                             ''ordine" o indicare altra motivazi-
050390060524     c                             one)'
050400060524     c                   exsr      StampaAR5
050410060524     c                   eval      Par5   = 'd) firma del destinatario-
050420060524     c                              nel campo "FIRMA DEL DESTINATARIO"'
050430060524     c                   exsr      StampaAR5
050440090302     *** *
050450090302     *** * - stampo box
050460090302     ***c                   eval      §LO2   = §PS1 + 0.050
050470090302     ***c                   write     A4prtAR53
050480090302      /free
050490090302         // - stampa box
050500090302         //   (§PB1 è da reimpostare in §LO1: contiene il suo valore PRIMA
050510090302         //   della stampa delle varie linee separatrici)
050520090302         §LO1   = §PB1;
050530090302         §LO2   = §PS1 + 0.070;
050540090302         write  A4prtAR53;
050550090302      /end-free
050560060524      *
050570060524     c                   ENDSR
050580031124      *
050590031124      *---------------------------------------------------------------*
050600050217      *?Prepara elenco per box "Promemoria per autotrasportatore"    ?*
050610031124      *---------------------------------------------------------------*
050620031124     c     ListaPMxA     BEGSR
050630031124      *
050640070308      * - stampo testata
050650070316     c                   eval      $First = *on
050660070319     c                   eval      Par5   = '                    '
050670070319     c                                    + '                    '
050680070319     c                                    + '          '
050690070319     c                                    + C_PMxA
050700031124     c                   exsr      StampaAR5
050710070315      *
050720070315      * - memorizzo posizione iniziale per il box
050730070320      *   (§PB1 sarà da reimpostare in §LO1 PRIMA della stampa del box)
050740070315     c                   eval      §PB1   = §LO1
050750031124      *
050760070308      * - stampo testo
050770081127      *
050780081212      /free
050790110411         // >?Mittente cliente in tab. "PXA"?
050800090302         IF  $TabPXA = *on;
050810090302
050820170605           clear  k05tntbe01;
050830090302           k_TBEcod = 'PXA';
050840090302           k_TBEke1 = %editc(DB0ccm : 'X');
050850100316           k_TBEke2 = '001            ';
050860090302           clear  k_TBElin;
050870090302           clear  k_TBEsif;
050880100316           //setll  %kds(k05tntbe01 : 3)  TNTBE000;  (già fatto)
050890090302           //reade  %kds(k05tntbe01 : 2)  TNTBE000;  (già fatto)
050900090302           clear  wx;
050910090302
050920100316           DoW  not  %eof(TNTBE01L)  and  TBEke2 <= '006            ';
050930090302
050940090302             if  TBEatb = *blank;
050950090302
050960110411               // -?Stampa della riga separatrice iniziale?
050970090302               if  wx = *zero;
050980090302                 §LO1   = §PS1 + 0.050;
050990090302                 write  A4prtLO;
051000090302                 §PS1   = §LO1;
051010090302                 $First = 'G';
051020090302               endif;
051030090302
051040110411               // -?Salta eventuali righe vuote?
051050090302               dow  %int( %trimr( TBEke2 ) ) > wx + 1;
051060090302                 clear  Par5;
051070090302                 exsr  StampaAR5;
051080090302                 wx += 1;
051090090302               enddo;
051100090302
051110110411               // -?Stampa la singola riga di testo?
051120090302               Par5   = TBEuni;
051130090302               exsr  StampaAR5;
051140090302               wx += 1;
051150090302
051160090302             endif;
051170090302
051180090302             reade  %kds(k05tntbe01 : 2)  TNTBE000;
051190090302
051200090302           EndDo;
051210090302
051220090302         ENDIF;
051230110411
051240110411         // >?Particolarità consegna in tab. "PXA"?
051250110411         IF  $TabPXA2 = *on;
051260110411
051270170605           clear  k05tntbe01;
051280110411           k_TBEcod = 'PXA';
051290110411           k_TBEke1 = DB0gma;
051300110411           k_TBEke2 = '001            ';
051310110411           clear  k_TBElin;
051320110411           clear  k_TBEsif;
051330110411           setll  %kds(k05tntbe01 : 3)  TNTBE000;
051340110411           reade  %kds(k05tntbe01 : 2)  TNTBE000;
051350110411           clear  wx;
051360110411
051370110411           DoW  not  %eof(TNTBE01L)  and  TBEke2 <= '006            ';
051380110411
051390110411             if  TBEatb = *blank;
051400110411
051410110411               // -?Stampa della riga separatrice iniziale?
051420110411               if  wx = *zero;
051430110411                 §LO1   = §PS1 + 0.050;
051440110411                 write  A4prtLO;
051450110411                 §PS1   = §LO1;
051460110411                 $First = 'G';
051470110411               endif;
051480110411
051490110411               // -?Salta eventuali righe vuote?
051500110411               dow  %int( %trimr( TBEke2 ) ) > wx + 1;
051510110411                 clear  Par5;
051520110411                 exsr  StampaAR5;
051530110411                 wx += 1;
051540110411               enddo;
051550110411
051560110411               // -?Stampa la singola riga di testo?
051570110411               Par5   = TBEuni;
051580110411               exsr  StampaAR5;
051590110411               wx += 1;
051600110411
051610110411             endif;
051620110411
051630110411             reade  %kds(k05tntbe01 : 2)  TNTBE000;
051640110411
051650110411           EndDo;
051660110411
051670110411         ENDIF;
051680090302
051690081212      /end-free
051700081127      *
051710031216sel 1c                   select
051720070306      * >
051730070306x   1c***                other
051740070306     c***                eval      $First = *off
051750070306     c***                clear                   Par5
051760070306     c***                exsr      StampaAR5
051770070306      *
051780070306e   1c                   endsl
051790061102      *
051800071114      * > Raccogliere Codice Fiscale e/o Partita Iva di ...
051810071114if  1c                   if             $No_CF = *on
051820071114     c                             or   $No_PI = *on
051830070320     c                   eval      §LO1   = §PS1 + 0.050
051840070320     c                   write     A4prtLO
051850070320     c                   eval      §PS1   = §LO1
051860070321     c                   eval      $First = 'G'
051870061102     c                   eval      Par5   = 'Spedizione n°'
051880061102     c                                    + %editw(DB0lnp:'0   ')
051890061102     c                                    + %editw(DB0nrs:'0  ')
051900061102     c                                    + ' '
051910061102     c                                    + %editc(DB0nsp:'Z')
051920061102     c                                    + ' del '
051930061102     c                                    + %editc(Pdsp:'Y')
051940061102     c                   exsr      StampaAR5
051950071114if  2c                   if        $No_PI = *on
051960071114     c*** (già fatto:)   eval      $First = 'G'
051970071114     c                   eval      Par5   = 'raccogliere +
051980071114     c                                      Partita I.v.a. di "'
051990071114     c                                    + %trim(wRSxCF)
052000071114     c                                    + '"'
052010071114     c                   exsr      StampaAR5
052020120504if  3c                   if        $No_CF = *off
052030120504     c*** (già fatto:)   eval      $First = 'G'
052040120504     c                   eval      Par5   = 'Codice Fiscale di "'
052050120504     c                                    + %trim(wRSxCF)
052060120504     c                                    + '": '
052070120504     c                                    + wB0cfi
052080120504     c                   exsr      StampaAR5
052090120504e   3c                   endif
052100071114e   2c                   endif
052110120503if  2c                   if        $No_CF = *on
052120120503     c*** (già fatto:)   eval      $First = 'G'
052130120503     c                   eval      Par5   = 'raccogliere +
052140120503     c                                      Codice Fiscale di "'
052150120503     c                                    + %trim(wRSxCF)
052160120503     c                                    + '"'
052170120503     c                   exsr      StampaAR5
052180120503e   2c                   endif
052190070306e   1c                   endif
052200070306      *
052210070306      * > Note bolla (dal file FIARN00F)
052220070306      *   (posizionamento e 1ª lettura già effettuate nella subr.
052230070306      *    "CheckFIARN")
052240070306if  1c                   if        $FIARN = *on
052250070320     c                   eval      §LO1   = §PS1 + 0.050
052260070320     c                   write     A4prtLO
052270070320     c                   eval      §PS1   = §LO1
052280070321     c                   eval      $First = 'G'
052290070319do  2c                   dou       %eof(FIARN11L)
052300070306     c                   eval      Par5   = ARNnob
052310070306     c                   exsr      StampaAR5
052320070319     c     K06ARN11      reade     FIARN000
052330070306e   2c                   enddo
052340070306e   1c                   endif
052350070319      *
052360070319      * > Consegne parziali precedenti (dal file FNART00F)
052370070406if  1c                   IF        $ConsParz     =  *on
052380101130     c*//(già fatto)?   exsr      SchConsParz
052390070320     c                   eval      §LO1   = §PS1 + 0.050
052400070320     c                   write     A4prtLO
052410070320     c                   eval      §PS1   = §LO1
052420070321     c                   eval      $First = 'G'
052430070319     c                   eval      xx     = 1
052440070319do  2c                   DoW            (xx      <= %elem($CP)
052450070319     c                             and  $CP(xx)  <  *all'9')
052460070319     c                   movel     $CP(xx)       Elem_CP
052470070319     c                   z-add     WCPdcm        w0080
052480070319     c     *iso          test(d)                 w0080                  31
052490070319if  3c                   if        NOT *in31
052500071025     c     *iso          movel     w0080         Data_EUR
052510071025     c     *eur          movel     Data_EUR      w0080
052520070319x   3c                   else
052530070319     c                   clear                   w0080
052540070319e   3c                   endif
052550140307 *...+*...1....+....2....+....3....+....4....+....5....+....6....+....7....+..
052560140307 Conse*nati colli 12345 in data 12/34/5678
052570140307 Conse*nati colli 12345 in data 12/34/5678 alle ore 12:34 - Firmatario: 123456
052580140307 Conse*nati colli 12345 il 12/34/5678 alle 12:34 a 1234567890123456789012345
052590140307 Conse*nati colli 12345 in data 12/34/5678 alle ore 12:34
052600140307      *                 Firmatario: 123456789012345678901234567890123456789012
052610070321     c                   eval      Par5   = 'Consegnati colli '
052620070319     c                                    + %editc(WCPncl:'Z')
052630070321     c                                    + ' in data '
052640070319     c                                    + %editc(w0080:'Y')
052650140307     c                   if        WCPocm > *zero
052660140307     c                   eval      Par5   = %trimr( Par5 )
052670140307     c                                    + ' alle ore '
052680140307     c                                    + %editw(WCPocm : '0 :  ')
052690140307     c                   endif
052700070319     c                   exsr      StampaAR5
052710140307     c                   if        WCPfir <> *blank
052720140307     c                   clear                   Par5
052730140307     c                   eval      %subst( Par5 : 24 ) = 'Firmatario: '
052740140307     c                                    + WCPfir
052750140307     c                   exsr      StampaAR5
052760140307     c                   endif
052770070319     c                   eval      xx     = xx + 1
052780070319e   2c                   EndDo
052790070319e   1c                   ENDIF
052800070321      *
052810070321      * > Reperita Comunicazione Anomalia
052820070321if  1c                   if        $C_A_  = *on
052830070321     c                   eval      §LO1   = §PS1 + 0.050
052840070321     c                   write     A4prtLO
052850070321     c                   eval      §PS1   = §LO1
052860070321     c                   eval      $First = 'G'
052870070321     c                   eval      Par5   = 'Presente Comunicazione +
052880070321     c                                                Anomalia'
052890070321     c                   exsr      StampaAR5
052900070321e   1c                   endif
052910031124      *
052920031124      * - stampo box
052930070320      *   (§PB1 è da reimpostare in §LO1: contiene il suo valore PRIMA
052940070320      *   della stampa delle varie linee separatrici)
052950070315     c                   eval      §LO1   = §PB1
052960070315     c                   eval      §LO2   = §PS1 + 0.070
052970031124     c                   write     A4prtAR53
052980031124      *
052990031124     c                   ENDSR
053000090924      /free
053010090924
053020090924       //--------------------------------------------------------------
053030090924       //?Prepara elenco per box "Note per il destinatario"            ?
053040090924       //--------------------------------------------------------------
053050090924       BEGSR  StampaNxD;
053060090924
053070090924         // - Stampa testata
053080090924         //$First = *on;
053090090924         //Par5   = '                    '
053100090924         //       + '                    '
053110090924         //       + '          '
053120090924         //       + c_NxD;
053130090924         //exsr  StampaAR5;
053140090924
053150090924         // - Memorizzazione posizione iniziale per il box
053160090924         //   (§PB1 sarà da reimpostare in §LO1 PRIMA della stampa del box)
053170090924         //§PB1 = §LO1;
053180090924
053190090924         // - Stampa della riga separatrice iniziale
053200090924         //§LO1   = §PS1 + 0.050;
053210090924         //write  A4prtLO;
053220090924         //§PS1   = §LO1;
053230090924
053240090924         // - Stampa la singola riga di testo
053250090925         $First = '1';
053260090924         Par5 = 'DECODER DIGITALE TERRESTRE OMAGGIO DA SKY PER AVER +
053270090924                 ADERITO ALL''OFFERTA';
053280090924         exsr  StampaAR5;
053290090924
053300090924         // - Memorizzazione posizione iniziale per il box
053310090924         //   (§PB1 sarà da reimpostare in §LO1 PRIMA della stampa del box)
053320090924         §PB1 = §LO1;
053330090924
053340090924         // - Stampa box
053350090924         //   (§PB1 è da reimpostare in §LO1: contiene il suo valore PRIMA
053360090924         //   della stampa delle varie linee separatrici)
053370090924         §LO1 = §PB1;
053380090925         §LO2 = §PS1 + 0.050;
053390090924         write  A4prtAR53;
053400090924
053410090924       ENDSR;
053420090924
053430090924      /end-free
053440030515      *
053450030515      *---------------------------------------------------------------*
053460050217      *?Prepara elenco per box - "Bancali da rendere al mittente"    ?*
053470150304      *?                         » NON PIÙ «                         ?*
053480050217      *?                       - "Dati per destinatario supermercato"?*
053490150304      *?                         » NON PIÙ «                         ?*
053500050217      *?                       - "Promemoria per autotrasportatore"  ?*
053510030515      *---------------------------------------------------------------*
053520030515     c     ListaAR5      BEGSR
053530030515      *
053540060921     c                   eval      $FIAR5 = *on
053550030515      *
053560030515      * Ripulisco i campi utilizzati in seguito
053570030515     c                   clear                   AR5trd
053580030515     c                   clear                   AR5dac
053590150304     c*//                clear                   dAR5ban
053600030717     c                   clear                   dAR5bnb
053610030903     c                   clear                   dAR5rcc
053620030515     c                   clear                   dAR5scr
053630030515     c                   reset                   $First
053640030515     c                   reset                   $FirstSCR
053650030515     c                   reset                   SAVtrd
053660030515      *
053670030515      * Posizionamento iniziale
053680040301if  1c                   if        DUTlpo = 'S'
053690040531     c     KLDV          setgt     FIAR531C
053700040301x   1c                   else
053710040607     c     KLDV          setgt     FIAR5000
053720040301e   1c                   endif
053730030515      *
053740030515      * Ciclo di lettura del file FIAR5
053750030515      *
053760030515do  1c                   DO        *Hival
053770030515      *
053780040607if  2c                   if        DUTlpo = 'S'
053790040531     c     KLDV          readpe    FIAR531C
053800040607x   2c                   else
053810040607     c     KLDV          readpe    FIAR5000
053820040607e   2c                   endif
053830040301      *
053840040301if  2c                   if           (DUTlpo =  'S'
053850040531     c                             and %eof(FIAR531C)
053860040301     c                             OR  DUTlpo <> 'S'
053870040301     c                             and %eof(FIAR501L))
053880030515     c                   leave
053890030515e   2c                   endif
053900030515if  2c                   if        AR5atb <> *blanks
053910030515     c                   iter
053920030515e   2c                   endif
053930030515      *
053940030515      * Controllo se devo stampare l'intestazione al nuovo box
053950030515if  2c                   if        AR5trd <> SAVtrd
053960030515      *   Stampa del precedente box (se stampata almeno una riga)
053970030910if  3c                   if             SAVtrd <> *hival
053980030910     c                             and  $First =  *off
053990030911     c                   eval      §LO2   = §PS1 + 0.050
054000030515     c                   write     A4prtAR53
054010030515e   3c                   endif
054020030515     c                   eval      $First =  *on
054030030515     c                   eval      SAVtrd =  AR5trd
054040030515e   2c                   endif
054050030515      *
054060030515sel 2c                   SELECT
054070150304      *//*
054080150304      *//* > Bancali da rendere al mittente
054090150304      *//*   (unico record, sequenzialmente prima dei rec. "SCR")
054100150304w   2c*//                WHEN      %subst(DB0gva:1:1) = 'B'
054110150304     c*//                          and    AR5trd      = 'BAN'
054120150304     c*//                movel     AR5uni        dAR5ban
054130150304      *//* - stampo testata
054140150304if  3c*//                if           (§AR5tba <> *zeros
054150150304     c*//                          or  §AR5tb2 <> *zeros)
054160150304     c*//                eval      Par5   = 'BANCALI DA RIPORTARE A MAGAZZINO E'
054170150304     c*//                                 + ' DA RENDERE AL MITTENTE           '
054180150304     c*//                exsr      StampaAR5
054190150304e   3c*//                endif
054200150304      *//* - stampo 1ª riga
054210150304if  3c*//                if        §AR5tba <> *zeros
054220150304     c*//                clear                   Par5
054230150304     c*//                clear                   Wtba
054240150304     c*//                eval      Wtba   = §AR5tba
054250150304     c*//                exsr      Tab_TBA
054260150304     c*//                eval      Par5   = §TBAdesc + 'N. '
054270150304     c*//                                 + %editc(§AR5nba : 'Z')
054280150304     c*//                exsr      StampaAR5
054290150304e   3c*//                endif
054300150304      *//* - stampo 2ª riga
054310150304if  3c*//                if        §AR5tb2 <> *zeros
054320150304     c*//                clear                   Par5
054330150304     c*//                clear                   Wtba
054340150304     c*//                eval      Wtba   = §AR5tb2
054350150304     c*//                exsr      Tab_TBA
054360150304     c*//                eval      Par5   = §TBAdesc + 'N. '
054370150304     c*//                                 + %editc(§AR5nb2 : 'Z')
054380150304     c*//                exsr      StampaAR5
054390150304e   3c*//                endif
054400150304      *//*
054410150304      *//* > Bancali da riportare a magazzino ("Oltre 20 colli")
054420150304      *//*   (unico record, sequenzialmente prima dei rec. "SCR",
054430150304      *//*    alternativo a "BAN" = bancali da rendere al mittente).
054440150304w   2c*//                WHEN      %subst(DB0gva:1:1) = 'O'
054450150304     c*//                          and    AR5trd      = 'BNB'
054460150304     c*//                movel     AR5uni        dAR5bnb
054470150304      *//* - stampo testata
054480150304if  3c*//                if           (§AR5btba <> *zeros
054490150304     c*//                          or  §AR5btb2 <> *zeros)
054500150304     c*//                eval      Par5   = 'BANCALI DA RIPORTARE A MAGAZZINO  '
054510150304     c*//                exsr      StampaAR5
054520150304e   3c*//                endif
054530150304      *//* - stampo 1ª riga
054540150304if  3c*//                if        §AR5btba <> *zeros
054550150304     c*//                clear                   Par5
054560150304     c*//                clear                   Wtba
054570150304     c*//                eval      Wtba   = §AR5btba
054580150304     c*//                exsr      Tab_TBA
054590150304     c*//                eval      Par5   = §TBAdesc + 'N. '
054600150304     c*//                                 + %editc(§AR5bnba : 'Z')
054610150304     c*//                exsr      StampaAR5
054620150304e   3c*//                endif
054630150304      *//* - stampo 2ª riga
054640150304if  3c*//                if        §AR5btb2 <> *zeros
054650150304     c*//                clear                   Par5
054660150304     c*//                clear                   Wtba
054670150304     c*//                eval      Wtba   = §AR5btb2
054680150304     c*//                exsr      Tab_TBA
054690150304     c*//                eval      Par5   = §TBAdesc + 'N. '
054700150304     c*//                                 + %editc(§AR5bnb2 : 'Z')
054710150304     c*//                exsr      StampaAR5
054720150304e   3c*//                endif
054730150304      *//* - stampo 3ª riga
054740150304if  3c*//                if        §AR5bcor <> *zeros
054750150304     c*//                clear                   Par5
054760150304     c*//                eval      Par5   = C_CoOr
054770150304     c*//                                 + %editc(§AR5bcor : 'K')
054780150304     c*//                exsr      StampaAR5
054790150304e   3c*//                endif
054800030515      *
054810030515      * > Destinatario supermercato
054820030515w   2c                   WHEN      (DB0tc1 = 'S' or DB0tc2 = 'S')
054830030515     c                             and AR5trd  = 'SCR'
054840030515     c                   movel     AR5uni        dAR5scr
054850030515      * - stampo testata
054860030515      *   (se subr. richiamata solo per verifica esistenza dati: esco)
054870030515if  3c                   if        $First =  *on
054880031124     c                   eval      Par5   = C_DeSM
054890030515     c                   exsr      StampaAR5
054900030515     c                   eval      $FirstSCR  = *off
054910030515e   3c                   endif
054920030515      * - stampo righe
054930030515     c                   clear                   Par5
054940030624     c                   eval      *in38 = *on
054950030515if  3c                   if           AR5dac <> SAVdac
054960030515     c                             or AR5orc <> SAVorc
054970030515     c                   eval      SAVdac = AR5dac
054980030515     c                   eval      SAVorc = AR5orc
054990030515      * - - giro data modifica
055000030515     c                   z-add     AR5dac        w0080
055010070315     c     *iso          test(d)                 w0080                  31
055020070315if  4c                   if        NOT *in31
055030071025     c     *iso          movel     w0080         Data_EUR
055040071025     c     *eur          movel     Data_EUR      w0080
055050070315x   4c                   else
055060070315     c                   clear                   w0080
055070070315e   4c                   endif
055080030515     c                   eval      Par5 = 'Modifica del '
055090030515     c                                  + %editc(w0080 : 'Y')
055100030515     c                                  + ' '
055110030515     c                                  + %editw(AR5orc : '  :  :  ')
055120030515     c                                  + ' '
055130040223     c                                  + §AR5Spru
055140030515     c                   exsr      StampaAR5
055150030515e   3c                   endif
055160040223     c                   eval      Par5 = §AR5Snom
055170030911     c                                  + '   Tel.: '
055180040223     c                                  + §AR5Stel
055190030911     c                   exsr      StampaAR5
055200040223     c                   eval      Par5 = §AR5Smot
055210030515     c                   exsr      StampaAR5
055220030624     c                   eval      *in38 = *off
055230031124      *
055240031124      * > Se destinatario assente inserire il plico in buca lettere
055250031124      *   (già stampato in box "Promemoria per autotrasportatore")
055260031124w   2c***                WHEN          DB0gma  = Ktab7V_CB
055270031124     c***                ...       ...
055280030903      *
055290030903      * > O.R.M. per ritiro contestuale alla consegna
055300030903      *   (unico record)
055310050510      *   Verrà stampato come ultimo box
055320050510      *   - prima del (eventuale) DDT del mittente
055330050510      *   - prima della copia per il destinatario
055340030910w   2c***                WHEN          DB0gma  = Ktab7V_RC
055350030910     c***                          and AR5trd  = 'RCC'
055360030903     c***                ...       ...
055370030515      *
055380030515e   2c                   ENDSL
055390030515      *
055400030515e   1c                   ENDDO
055410030515      *
055420030515      * Stampa del (ultimo) box (se stampata almeno una riga)
055430030515if  1c                   if        $First = *off
055440030911     c                   eval      §LO2   = §PS1 + 0.050
055450030515     c                   write     A4prtAR53
055460030515e   1c                   endif
055470030515      *
055480030515      * > Destinatario supermercato
055490030515      *   (stampa del box vuoto per memo se non già stampata almeno
055500030515      *   una riga)
055510030515if  1c                   IF        (DB0tc1 = 'S' or DB0tc2 = 'S')
055520030515     c                             and              $FirstSCR = *on
055530030515      * - stampo testata
055540030515     c                   eval      $First = *on
055550031124     c                   eval      Par5   = C_DeSM
055560030515     c                   exsr      StampaAR5
055570030515     c                   eval      $FirstSCR  = *off
055580030515      * - stampo 2 righe vuote
055590030515     c                   clear                   Par5
055600030515do  2c                   do        2
055610030515     c                   exsr      StampaAR5
055620030515e   2c                   enddo
055630030515      * - stampo box
055640030911     c                   eval      §LO2   = §PS1 + 0.050
055650030515     c                   write     A4prtAR53
055660030515e   1c                   ENDIF
055670070123      *
055680070123     c                   eval      $FIAR5 = *off
055690030515      *
055700030515     c                   ENDSR
055710030515      *
055720030515      *---------------------------------------------------------------*
055730050217      *?Stampa singola riga relativa a FIAR5                         ?*
055740030515      *---------------------------------------------------------------*
055750030515     c     StampaAR5     BEGSR
055760030515      *
055770030515      * Controllo se superata l'ultima riga
055780030515     c                   exsr      Test_OF
055790030515      *
055800030515      * Stampa del singolo record
055810030909sel 1c                   select
055820030909      *
055830030909w   1c                   when      $First = *off
055840030909      * - Incremento parametri per posizionamento nuova riga
055850030911     c                   eval      §PS1   = §PS1 + 0.150
055860030909     c                   write     A4prtAR52
055870030909      *
055880030909w   1c                   when      $First = *on
055890030515      * - Incremento parametri per posizionamento nuovo box
055900030515      *   e relativa intestazione
055910030911     c                   eval      §LO1   = §LO2 + 0.150
055920030911     c                   eval      §PS1   = §LO1 + 0.150
055930030515     c                   write     A4prtAR51
055940030515     c                   eval      $First = *off
055950060524      *
055960060524w   1c                   when      $First = '2'
055970060524      * - Incremento parametri per posizionamento nuova riga
055980060524      *   nel box "Promemoria per autotrasportatore"
055990060524     c                   eval      §PS1   = §PS1 + 0.150
056000060524     c                   write     A4prtAR51
056010060524     c                   eval      $First = *off
056020030909      *
056030030909w   1c                   when      $First = 'A'
056040030909      * - Incremento parametri per posizionamento nuovo box
056050030909      *   e relativa intestazione
056060030911     c                   eval      §LO1   = §LO2 + 0.150
056070030909     c                   eval      §PS1   = §LO1 + 0.200
056080030909     c                   write     A4prtAR5a
056090030909     c                   eval      $First = *off
056100150304      *//*
056110150304w   1c*//                when      $First = 'B'
056120150304      *//* - Incremento parametri per posizionamento nuovo box
056130150304      *//*   ed intestazione "Promemoria per autotrasportatore"
056140150304     c*//                eval      §LO1   = §LO2 + 0.150
056150150304     c*//                eval      §PS1   = §LO1 + 0.125
056160150304     c*//                write     A4prtAR5b
056170150304     c*//                eval      $First = 'C'
056180150304      *//*
056190150304w   1c*//                when      $First = 'C'
056200150304      *//* - Incremento parametri per posizionamento nuova riga
056210150304      *//*   nel box "Promemoria per autotrasportatore"
056220150304     c*//                eval      §PS1   = §PS1 + 0.250
056230150304     c*//                write     A4prtAR5c
056240070321      *
056250070321w   1c                   when      $First = 'G'
056260070321      * - Incremento parametri per posizionamento nuova riga
056270070321      *   nel box "Promemoria per autotrasportatore"
056280070321     c                   eval      §PS1   = §PS1 + 0.170
056290070321     c                   write     A4prtAR5g
056300030909      *
056310030909e   1c                   endsl
056320030515      *
056330030515     c                   ENDSR
056340030910      *
056350030910      *---------------------------------------------------------------*
056360050217      *?Stampa box "Ritiro Contestuale alla Consegna" da FIAR5/RCC   ?*
056370030910      *---------------------------------------------------------------*
056380030910     c     StampaRCC     BEGSR
056390030910      *
056400030910      * > O.R.M. per ritiro contestuale alla consegna
056410030910      *   (unico record)
056420031124      *   Verrà stampato come ultimo box (prima della copia per il
056430031124      *     destinatario)
056440030910      *
056450030910     c                   clear                   dAR5rcc
056460030910      *
056470030910     c                   eval      AR5trd = 'RCC'
056480040301if  1c                   if        DUTlpo = 'S'
056490040531     c     K05AR501      chain     FIAR531C
056500040301x   1c                   else
056510030910     c     K05AR501      chain     FIAR5000
056520040301e   1c                   endif
056530040301      *
056540071008if  1c                   if           (DUTlpo =  'S'
056550040531     c                             and %found(FIAR531C)
056560040301     c                             OR  DUTlpo <> 'S'
056570040301     c                             and %found(FIAR501L))
056580040301     c                             and AR5atb =  *blanks
056590030910     c                   movel     AR5uni        dAR5rcc
056600071015     c                   eval      ORMpoe = §AR5Rpoe
056610071015     c                   eval      ORMnsr = §AR5Rnsr
056620071015     c                   eval      ORMnor = §AR5Rnor
056630071015     c                   eval      ORMnrv = §AR5Rnrv
056640071015     c                   eval      Pbt2a  = 'RITIRO CONTESTUALE ALLA +
056650071015     c                                       CONSEGNA'
056660071015     c                   exsr      StampaORM
056670071015e   1c                   endif
056680071015      *
056690071015     c                   ENDSR
056700071015      *
056710071015      *---------------------------------------------------------------*
056720071015      *?Stampa box "Ritiro Porto Franco PrePagato"                   ?*
056730071015      *---------------------------------------------------------------*
056740071015     c     StampaRPFPP   BEGSR
056750071015      *
056760071015      * > O.R.M. per ritiro porto franco prepagato
056770071015      *   Verrà stampato come ultimo box (prima della copia per il
056780071015      *     destinatario)
056790071015      *
056800071015     c*** già fatto:     clear                   dsBL4M
056810071015     c*** già fatto:     exsr      RepAR4_M
056820071015      *
056830071015if  1c                   if             §B4poe  > *zeros
056840071015     c                             and  §B4nor  > *zeros
056850071015     c                   eval      ORMpoe = §B4poe
056860071015     c                   eval      ORMnsr = §B4nsr
056870071015     c                   eval      ORMnor = §B4nor
056880071015     c                   eval      ORMnrv = §B4nrv
056890071015     c                   eval      Pbt2a  = 'RITIRO PORTO FRANCO +
056900071015     c                                       PREPAGATO'
056910071015     c                   exsr      StampaORM
056920071015e   1c                   endif
056930071015      *
056940071015     c                   ENDSR
056950071015      *
056960071015      *---------------------------------------------------------------*
056970071015      *?Stampa dati O.R.M. in box "Ritiro Contestuale alla Consegna" ?*
056980071015      *?              o in quello "Ritiro Porto Franco Prepagato"    ?*
056990071015      *---------------------------------------------------------------*
057000071015     c     StampaORM     BEGSR
057010071015      *
057020071015     c                   eval      $FIAR5 = *on
057030071015      *
057040071015if  2c                   if        not %open(FNORM01L)
057050071015     c                   open      FNORM01L
057060071015e   2c                   endif
057070071015     c     K04ORM01      chain     FNORM000
057080071015      *
057090071015if  2c                   IF        %found(FNORM01L)
057100071015      *
057110071015      * Reperisco la posizione (in pollici) in cui posizionarmi
057120071015      *   per la stampa in base all'ultima riga stampata
057130071015      *   se non ho ancora stampato box
057140071015cas 3c     §LO2          caseq     *zeros        Rep_§LO2
057150071015e   3c                   endcs
057160071025      *
057170071015      * - stampo testata
057180071025      *   (con il n° O.R.M.)
057190071015     c                   eval      $First = 'A'
057200071015     c*** già impostato: eval      Pbt2a  = 'RITIRO CONTESTUALE ALLA +
057210071015     c***  "      "                          CONSEGNA'
057220071015     c                   eval      Pbt2b  = 'O.R.M. numero '
057230071015     c                                    + %editw(ORMpoe : '0   ')
057240071015     c                                    + ' '
057250071015     c                                    + %editw(ORMnsr : '0  ')
057260071015     c                                    + '  '
057270071015     c                                    + %editc(ORMnor : 'Z')
057280071015     c                                    + ' '
057290071015     c                                    + %editw(ORMnrv : '0  ')
057300071015     c                   exsr      StampaAR5
057310071025      *
057320071015      * - stampo i dati dell'O.R.M.
057330071015     c                   exsr      QUM_FnOrm
057340140411     c                   eval      Par5   = 'Quantità      : '
057350071015     c                                    + %trim($Qum(1)) + '    '
057360071015     c                                    + %trim($Qum(2)) + '    '
057370071015     c                                    + %trim($Qum(3))
057380071015     c                   exsr      StampaAR5
057390080228if  3c                   if        DB0tbo = '8'
057400140411     c                   eval      Par5   = 'Mittente      : '
057410080228     c                                    + ORMrsr
057420080228x   3c                   else
057430140411     c                   eval      Par5   = 'Destinatario  : '
057440071015     c                                    + ORMrsc
057450080228e   3c                   endif
057460071015     c                   exsr      StampaAR5
057470080228if  3c                   if        DB0tbo = '8'
057480140411     c                   eval      Par5   = '                '
057490080228     c                                    + ORMinr
057500080228x   3c                   else
057510140411     c                   eval      Par5   = '                '
057520071015     c                                    + ORMinc
057530080228e   3c                   endif
057540071015     c                   exsr      StampaAR5
057550080228if  3c                   if        DB0tbo = '8'
057560140411     c                   eval      Par5   = '                '
057570080228     c                                    + ORMcar + ' '
057580080228     c                                    + ORMlor + ' '
057590080228     c                                    + ORMprr + ' '
057600080228     c                                    + ORMnar
057610080228x   3c                   else
057620140411     c                   eval      Par5   = '                '
057630071015     c                                    + ORMcac + ' '
057640071015     c                                    + ORMloc + ' '
057650071015     c                                    + ORMprc + ' '
057660071015     c                                    + ORMnac
057670080228e   3c                   endif
057680071015     c                   exsr      StampaAR5
057690101022             // -?SE Ritiro Porto Franco Prepagato:?
057700080228if  3c                   if        DB0tbo = '8'
057710101022      /free
057720101022               // -?stampo il referente ed il relativo telefono?
057730101022               // -?(Comunque! Ci sono sempre!)?
057740140411               Par5   = 'Referente     : ' + ORMrer + '   '
057750101022                      + 'Telefono: '     + ORMter;
057760101022               exsr  StampaAR5;
057770101022      /end-free
057780140411     c                   eval      Par5   = 'Ordinante     : '
057790080228     c                                    + ORMrso
057800080228     c                   exsr      StampaAR5
057810140411     c                   eval      Par5   = 'Destinatario  : '
057820080303     c                                    + ORMrsc
057830080303     c                   exsr      StampaAR5
057840080228e   3c                   endif
057850071015      * - stampo le note
057860071015      *   (Comunque! Non solo "se ci sono". Al limite: scarto la 1ª.)
057870140411     c                   eval      Par5   = 'Note          : '
057880071015if  3c                   if        ORMno1 <> *blanks
057890071015     c                   eval      Par5   = %trim(Par5)  + ' '
057900071015     c                                    + ORMno1 + ' ' + ORMno2
057910071015x   3c                   else
057920071015     c                   eval      Par5   = %trim(Par5)  + ' '
057930071015     c                                    + ORMno2
057940071015e   3c                   endif
057950071015     c                   exsr      StampaAR5
057960140411      *
057970140411      * -?SE Ritiro Porto Franco Prepagato:?
057980140411if  3c                   IF        DB0tbo = '8'
057990140411      *
058000140411      * - stampo orari apertura (SE ci sono)
058010140411     c                   exsr      sr_OrariApert
058020071025      *
058030071025      * - stampo data & ora ritiro
058040071025     c                   z-add     ORMdar        w0080
058050071025     c     *iso          test(d)                 w0080                  31
058060071025if  4c                   if        NOT *in31
058070071025     c     *iso          movel     w0080         Data_EUR
058080071025     c     *eur          movel     Data_EUR      w0080
058090071025x   4c                   else
058100071025     c                   clear                   w0080
058110071025e   4c                   endif
058120140411     c                   eval      Par5   = 'Data ritiro   : '
058130071025     c                                    + %editc(w0080:'Y')
058140071025if  4c                   if        ORMorr > 1230
058150071025     c                   eval      Par5   = %trimr(Par5)
058160071025     c                                    + '          '
058170071025     c                                    + 'Ora ritiro: '
058180071025     c                                    + %editw(ORMorr:'0 :  ')
058190071025     c                   exsr      StampaAR5
058200071025x   4c                   else
058210071025     c                   exsr      StampaAR5
058220071025     c                   clear                   Par5
058230071025     c                   eval      %subst(Par5:50)
058240071025     c                                    = 'Ora ritiro: '
058250071025     c                                    + %editw(ORMorr:'0 :  ')
058260071025     c                   write     A4prtAR51
058270071025e   4c                   endif
058280140411      *
058290071025e   3c                   ENDIF
058300071025      *
058310071015      * - imposto codice a barre O.R.M.
058320071015     c                   movel     BarCodeORM    Pbco
058330071015      * - verifico se codice a barre da stampare
058340071015     c                   eval      *in52  =  (Pbco <> *zeros)
058350071025      *
058360071015      * - stampo box
058370071015     c                   eval      §LO2   = §PS1 + 0.050
058380071015if  3c                   if        NOT *in52
058390071015     c                   write     A4prtAR53
058400071015x   3c                   else
058410071025     c                   eval      §LV1   = §LO2 - §LO1
058420071015     c                   eval      §PS2   = §LO2 - 0.500
058430071015     c                   write     A4prtAR54
058440071015e   3c                   endif
058450071015      *
058460071015e   2c                   ENDIF
058470071015      *
058480071015     c                   eval      $FIAR5 = *off
058490071015      *
058500071015     c                   ENDSR
058510100513      /free
058520140411
058530140411       //--------------------------------------------------------------
058540140411       //?Reperimento, impostazione e stampa degli Orari di Apertura   ?
058550140411       //--------------------------------------------------------------
058560140411       BEGSR  sr_OrariApert;
058570140411
058580140411         clear  dOREorari;
058590140411
058600140411         // -?Recupero dati da estensione FNORE (trc "O ")?
058610140411         if  NOT  %open(FNORE01L);
058620140411           open  FNORE01L;
058630140411         endif;
058640140411         k_OREpoe = ORMpoe;
058650140411         k_OREnsr = ORMnsr;
058660140411         k_OREnor = ORMnor;
058670140411         k_OREnrv = ORMnrv;
058680140411         k_OREtrc = 'O ';
058690140411         chain  %kds(keyFNORE01)  FNORE000;
058700140411
058710140411         // -?...NON trovati => uscita dalla subroutine?
058720140411         if  Not  %found(FNORE01L);
058730140411           leavesr;
058740140411         endif;
058750140411
058760140411         // -?...Trovati => verifica degli orari?
058770140411         dOREorari = OREdati;
058780140411
058790140411         Select;
058800140411
058810140411           // -?C'è un solo orario "ALLE"?
058820140411           When  §OREoraMda = *zero  and  §OREoraMa > *zero  and
058830140411                 §OREoraPda = *zero  and  §OREoraPa = *zero;
058840140411             Par5 = 'Orari apertura: ' +
058850140411                    'Fino alle ' + %editw( §OREoraMa : ' 0:  ' );
058860140411           When  §OREoraMda = *zero  and  §OREoraMa = *zero  and
058870140411                 §OREoraPda = *zero  and  §OREoraPa > *zero;
058880140411             Par5 = 'Orari apertura: ' +
058890140411                    'Fino alle ' + %editw( §OREoraPa : ' 0:  ' );
058900140411
058910140411           // -?C'è un solo orario completo (per 1°)?
058920140411           When  §OREoraMda > *zero  and  §OREoraMa > *zero  and
058930140411                 §OREoraPda = *zero  and  §OREoraPa = *zero;
058940140411             Par5 = 'Orari apertura: ' +
058950140411                    %editw( §OREoraMda : ' 0:  ' ) + '  ' +
058960140411                    %editw( §OREoraMa  : ' 0:  ' );
058970140411
058980140411           // -?Ci sono due orari completi?
058990140411           When  §OREoraMda > *zero  and  §OREoraMa > *zero  and
059000140411                 §OREoraPda > *zero  and  §OREoraPa > *zero;
059010140411             Par5 = 'Orari apertura: ' +
059020140411                    %editw( §OREoraMda : ' 0:  ' ) + '  ' +
059030140411                    %editw( §OREoraMa  : ' 0:  ' ) + ' - ' +
059040140411                    %editw( §OREoraPda : ' 0:  ' ) + '  ' +
059050140411                    %editw( §OREoraPa  : ' 0:  ' );
059060140411
059070140411         EndSl;
059080140411
059090140411         exsr  StampaAR5;
059100140411
059110140411       ENDSR;
059120100513
059130100513       //--------------------------------------------------------------
059140100513       //?Verifica se presente ORM con preavviso mail                  ?
059150100513       //--------------------------------------------------------------
059160100513       BEGSR  sr_CheckORM_PrevvMail;
059170100514
059180100514         //clear  dOrm01               (già fatto)
059190100513
059200100513         // -?Reperimento num. ORM (se NON già reperito)?
059210100513         if  DB0tbo <> '8';
059220100513           exsr  RepAR4_M;
059230100513         endif;
059240100513
059250100513         // -?ORM inesistente: inutile proseguire?
059260100513         if  §B4poe = *zero    or   §B4nor = *zero;
059270100513           leavesr;
059280100513         endif;
059290100513
059300100513         // -?Verifica presenza O.R.M.?
059310100513         if  Not  %open(FNORM01L);
059320100513           open  FNORM01L;
059330100513         endif;
059340100513
059350100513         ORMpoe = §B4poe;
059360100513         ORMnsr = §B4nsr;
059370100513         ORMnor = §B4nor;
059380100513         ORMnrv = §B4nrv;
059390100513
059400100513         chain  K04ORM01  FNORM000;
059410100513
059420100513         // -?ORM non reperito: inutile proseguire?
059430100513         if  Not  %found(FNORM01L);
059440100513           leavesr;
059450100513         endif;
059460100513
059470100513         // -?Impostazione flag per stampa porto "ASSEGNATO"?
059480100513         dOrm01 = ORMflo;
059490100513         $PreavvORM = (§ORMpre = 'M');
059500100513
059510100513       ENDSR;
059520100513
059530100513      /end-free
059540030515      *
059550030515      *---------------------------------------------------------------*
059560050217      *?Stampa dati DPD                                              ?*
059570030515      *---------------------------------------------------------------*
059580030515     c     StampaDPD     BEGSR
059590030515      *
059600080922      * (EX) Bar-Code - non più stampato come codice a barre
059610030515      * (1' 12 char di Riferimento Mittente Alfanumerico - se numerici)
059620060626      *---
059630061030      * Il barcode DPD è stato variato ora può essere anche
059640061030      *   alfanumerico ed è lungo 14
059650171117      * Si trova sul file FIPND00F (vedi campo PNDIPN).
059660061030      * I controlli che faceva prima sono da cambiare
059670061030      *---
059680061030      *
059690060626      * nuovi controlli per nuovo barcode DPD
059700171117if  1c                   if        PNDipn <> *blanks
059710061030      * - controllo se vecchio o nuovo barcode
059720171117if  2c                   if        %subst(PNDipn:14:1) = *blanks
059730061030      * - - vecchio
059740171117     c                   eval      Pbcd2 = '01 ' + %subst(PNDipn:1:3) +
059750171117     c                                     ' '   + %subst(PNDipn:4:8)
059760061030      * - - nuovo
059770061030x   2c                   else
059780171117     c                   eval      Pbcd2 = '01 ' + %subst(PNDipn:1:4) +
059790171117     c                                     ' '   + %subst(PNDipn:5:10)
059800061030e   2c                   endif
059810061030e   1c                   endif
059820080922      /free
059830080922         // Impostazione data
059840080922         PdpA = 'Date ' + %editc(Pdsp:'Y');
059850080922
059860080922         // Impostazione numero colli
059870080922         PdpB = 'Parcel N. ' + %editc(Pncl:'Z');
059880080922
059890080922         // Impostazione numero parcel
059900171117         PdpC = PNDipn;
059910080922
059920080922         // Impostazione "costante" da stampare tra le Note
059930080922         PdpD = 'Controllare la corrispondenza';
059940080922         PdpE = 'del destinatario con l''indirizzo';
059950080922         PdpF = 'dell''etichetta DPD.';
059960080922
059970080922         // Impostaz. posizioni sulla copia per il vettore già stampata
059980080922         §DP1 = wPosIni + 1.100;
059990080922         §DP2 = §DP1 + 0.550;
060000080922         §DP3 = §DP2 + 0.120;
060010080922         §DP4 = §DP3 + 0.120;
060020080922         §DP5 = §DP4 + 0.120;
060030080922         §DP6 = §DP5 + 0.160;
060040080922         §DP7 = §DP6 + 0.130;
060050080922         §DP8 = §DP7 + 0.130;
060060080922      /end-free
060070080922      *
060080080922      * (inutile testare l'overflow perchè siamo in ex-A5)
060090080922     c                   write     A4prtDPD
060100030515      *
060110030515     c                   ENDSR
060120030515      *
060130030515      *---------------------------------------------------------------*
060140050217      *?Prepara elenco per box - Elenco "Package Tracking Number"    ?*
060150030515      *---------------------------------------------------------------*
060160030515     c     ListaFED      BEGSR
060170030515      *
060180030515      * Stampo il simbolo "FedEx" (sulla 1ª pagina):
060190030515      *   imposto le posizioni iniziali di stampa per le linee
060200030515      *   (orizzontali-§LOn e verticali-§LVn)
060210080912     c                   eval      §LO1   = 3.330
060220080912     c                   eval      §LO2   = §LO1 + 0.370
060230081023     c                   eval      §LV1   = §LO2 - 0.150
060240081023     c                   eval      §LV2   = §LO2 - 0.070
060250030515     c                   write     A4prtFED1
060260080818      *
060270080818      * Stampo le "costanti" di inizio lista
060280030515     c                   write     A4prtFED2
060290030515      *
060300030515      * Stampo elenco segnacolli FedEx
060310030709     c                   eval      §LO1   = §LO2 - 0.250
060320030515      * - per le bolle in partenza
060330030515if  1c                   IF           DB0tbo = 'P'
060340030515     c                             or DB0tbo = '8'
060350030515if  2c                   if        NOT %open(FNBLT01L)
060360030515     c                   open      FNBLT01L
060370030515e   2c                   endif
060380030515      * - per le bolle in arrivo
060390030515x   1c                   ELSE
060400070319if  2c                   if        NOT %open(FNART01L)
060410070319     c                   open      FNART01L
060420070319e   2c                   endif
060430030515e   1c                   ENDIF
060440070319if  1c                   if        NOT %open(FIARS01L)
060450070319     c                   open      FIARS01L
060460070319e   1c                   endif
060470030515     c                   eval      Wtrc = 'C'
060480030515     c                   clear                   Wnsc
060490030515     c                   clear                   Wawb
060500030515do  1c                   dou       Wawb = *blanks
060510030515     c                   exsr      StampaFED
060520030515e   1c                   enddo
060530051020      *
060540030515     c                   ENDSR
060550030515      *
060560030515      *---------------------------------------------------------------*
060570050217      *?Stampa singola riga relativa a FEDEX                         ?*
060580030515      *---------------------------------------------------------------*
060590030515     c     StampaFED     BEGSR
060600030515      *
060610030515      * Impostazione campi di stampa
060620030515     c                   clear                   Wawb
060630030515      * - Carico AWB FedEx
060640030515      * - - leggo segnacolli per bolle in partenza
060650030515if  1c                   IF           DB0tbo = 'P'
060660030515     c                             or DB0tbo = '8'
060670030515     c                   exsr      CarAWB_Blx
060680030515      * - - leggo segnacolli per bolle in arrivo
060690030515x   1c                   ELSE
060700030515     c                   exsr      CarAWB_Arx
060710030515e   1c                   ENDIF
060720030515     c     wAWB          cabeq     *blanks       End_FED
060730030515     c                   move      wAWB          Pptn
060740030515     c                   add       1             Psts
060750030515      *
060760030515      * Controllo se superata l'ultima riga
060770030515     c                   exsr      Test_OF
060780030709      *
060790030709      * Imposto le posizioni iniziali di stampa per le linee
060800030709      *   (orizzontali-§LOn e verticali-§LVn) e per i campi (§PSn)
060810030709     c                   eval      §LO1 = §LO1 + 0.250 - 0.008
060820030709     c                   eval      §LO2 = §LO1 + 0.250
060830030709     c                   eval      §LV1 = §LO1
060840030709     c                   eval      §LV2 = §LO2 - 0.100
060850030709     c                   eval      §PS1 = §LO1 + 0.200
060860030515      *
060870030515      * Stampa del singolo record
060880030515     c                   write     A4prtFED3
060890030515      *
060900030515     c     End_FED       ENDSR
060910030515      *
060920030515      *---------------------------------------------------------------*
060930051020      *?Reperimento AWB FedEx da file FNBLT e FIARS (bolle partenza) ?*
060940030515      *---------------------------------------------------------------*
060950030515     c     CarAWB_Blx    BEGSR
060960130215      *
060970130215      * Verifica se, al di là del codice bolla (vedi "$ "), esiste
060980130215      *   comunque una bolla mamma
060990130215     c                   exsr      sr_Mamma_FedEx
061000030515      *
061010030515      * Posizionamento
061020130215if  2c                   if        $Mamma = *off   and
061030130215     c                             $Mamma_FedEx = *off
061040040607     c     KLDVnsc       setgt     FNBLT000
061050070514x   2c                   else
061060070514     c     KLBLnsc       setgt     FNBLT000
061070070514e   2c                   endif
061080030515      *
061090030515      * Ciclo di lettura
061100030515do  1c                   DO        *hival
061110030515      *
061120130215if  2c                   if        $Mamma = *off   and
061130130215     c                             $Mamma_FedEx = *off
061140040607     c     KLDV          reade     FNBLT000
061150070514x   2c                   else
061160070514     c     KLBL          reade     FNBLT000
061170070514e   2c                   endif
061180030515if  2c                   if        %eof(FNBLT01L)
061190030515     c                   leave
061200030515e   2c                   endif
061210030515      * - per ogni segnacollo cerco l'AWB
061220060505     c     K05ARS01P     chain     FIARS000
061230060505if  2c                   if            %found(FIARS01L)
061240060505     c                             and %subst(ARSnot:1:1) >= *zeros
061250060505     c                   eval      wAWB = %subst(ARSnot:1:12)
061260060505     c                   eval      wNSC = ARSnsc
061270030515     c                   leave
061280030515e   2c                   endif
061290030515      *
061300030515e   1c                   ENDDO
061310030515      *
061320030515     c                   ENDSR
061330030515      *
061340030515      *---------------------------------------------------------------*
061350070319      *?Reperimento AWB FedEx da file FNART e FIARS (bolle arrivo)   ?*
061360030515      *---------------------------------------------------------------*
061370030515     c     CarAWB_Arx    BEGSR
061380130215      *
061390130215      * Verifica se, al di là del codice bolla (vedi "$ "), esiste
061400130215      *   comunque una bolla mamma
061410130215     c                   exsr      sr_Mamma_FedEx
061420030515      *
061430030515      * Posizionamento
061440130215if  2c                   if        $Mamma = *off   and
061450130215     c                             $Mamma_FedEx = *off
061460040607     c     KLDVnsc       setgt     FNART000
061470070514x   2c                   else
061480130215     c     KLBLnsc       setgt     FNART000
061490070514e   2c                   endif
061500030515      *
061510030515      * Ciclo di lettura
061520030515do  1c                   DO        *hival
061530030515      *
061540130215if  2c                   if        $Mamma = *off   and
061550130215     c                             $Mamma_FedEx = *off
061560040607     c     KLDV          reade     FNART000
061570070514x   2c                   else
061580070514     c     KLBL          reade     FNART000
061590070514e   2c                   endif
061600030515if  2c                   if        %eof(FNART01L)
061610030515     c                   leave
061620030515e   2c                   endif
061630030515      * - per ogni segnacollo cerco l'AWB
061640060505     c     K05ARS01A     chain     FIARS000
061650060505if  2c                   if            %found(FIARS01L)
061660030515     c                             and %subst(ARSnot:1:1) >= *zeros
061670030515     c                   eval      wAWB = %subst(ARSnot:1:12)
061680030515     c                   eval      wNSC = ARSnsc
061690030515     c                   leave
061700030515e   2c                   endif
061710030515      *
061720030515e   1c                   ENDDO
061730030515      *
061740030515     c                   ENDSR
061750130215      *
061760130215      /free
061770130215
061780130215       //--------------------------------------------------------------
061790130215       //?Verifica se la spedizione FedEx ha bolla mamma.              ?
061800130215       //--------------------------------------------------------------
061810130215       BEGSR  sr_Mamma_FedEx;
061820130215
061830130215         //$Mamma_FedEx = *off;          (già così)
061840130215
061850130215         If  Not $Mamma;
061860130215
061870130215           // -?Verifica se la spedizione ha comunque mamma?
061880130215           //  ?(al di là del codice bolla - vedi cod. "$ ")?
061890130215           if  not %open(FNLBL01L);
061900130215             open  FNLBL01L;
061910130215           endif;
061920130215           chain  (DB0aas : DB0lnp : DB0nrs : DB0nsp)  FNLBL000;
061930130215
061940130215           $Mamma_FedEx = %found(FNLBL01L);
061950130215
061960130215         EndIf;
061970130215
061980130215       ENDSR;
061990130215
062000130215      /end-free
062010040607      *
062020040607      *---------------------------------------------------------------*
062030080918      *?Stampa DDT / PACKING-LIST del mittente                       ?*
062040040607      *---------------------------------------------------------------*
062050040607     c     StampaDDT     BEGSR
062060130315      *
062070130315      * - Uscita in assenza del file FIAR801L
062080130315     c                   if        Not  $E_AR8
062090130315     c                   leavesr
062100130315     c                   endif
062110080825      *
062120080825      * Meglio togliere l'impostazione relativa ai segnacolli non seq.
062130080825      *   per far funzionare il 1° posizionamento nella subr. Rep_§LO2
062140080825     c                   reset                   $A4_x_ScNS
062150080918      /free
062160080923         *in45 = (DB0fa4 = 'P');
062170080923         *in46 = (DB0fa4 = 'P'   and   %rem(wNumLdv : 2) = *zero);
062180080918      /end-free
062190040607      *
062200040607      * Se già stampato qualcos'altro:
062210040607      *   forzo il salto alla pagina successiva
062220080926       // (SE NON si sta stampando SOLO una packing-list, nel qual caso
062230080926       //  il posizionamento nella pagina A4 verrà eseguito dagli
062240080926       //  indicatori 45 e 46)
062250080926if  1c                   if           (CurLineA4 > $MaxLine1
062260080926     c                             or  §LO2      > $LimOF1)
062270080926     c                             and NOT *in45
062280040607     c                   eval      CurLineA4 = wMaxLine
062290040607     c                   exsr      Test_OF
062300040607e   1c                   endif
062310040607      *
062320040607     c                   eval      $DDT_p = *on
062330040607      *
062340130315      /free
062350130315         // -?Posizionamento?
062360130315         clear  k06fiar801;
062370130315         k_AR8aas = DB0aas;
062380130315         k_AR8lnp = DB0lnp;
062390130315         k_AR8nrs = DB0nrs;
062400130315         k_AR8nsp = DB0nsp;
062410130315         k_AR8tmd = c_§PDF + '999';
062420130315         setgt  %kds( k06fiar801 : 5 )  FIAR8000;
062430130315      /end-free
062440130315      *
062450130315      * - Letura
062460040607do  1c                   dou          %eof(FIAR801L)
062470040607     c                             or AR8atb = *blanks
062480130308     c                             or AR8tmd <> c_§PDF
062490040607     c     KLDV          reade     FIAR8000
062500040607e   1c                   enddo
062510040607if  1c                   if        %eof(FIAR801L)
062520040607     c                   goto      NOprtDDT
062530040607e   1c                   endif
062540041207if  1c                   if        §AR8des   = *blanks
062550041207     c*** (già fatto:)   reset                   dAR8
062560041207     c                   reset                   TIBS02ds
062570041207     c                   eval      T02cod    = 'AR8'
062580041207     c                   movel(p)  AR8tmd        T02ke1
062590041207     c                   call      'TIBS02R'
062600041207     c                   parm                    KPJBA
062610041207     c                   parm                    TIBS02ds
062620041207if  2c                   if        T02err    = *blanks
062630041207     c                   movel     T02uni        dAR8
062640041207e   2c                   endif
062650041207e   1c                   endif
062660040607      *
062670040607sel 1c                   select
062680050217      * - Karnak o Titanedi o EDk (identiche)
062690080730w   1c                   when      §AR8tsd   = 'K1'
062700040607     c                   exsr      DDT_Karnak
062710050414      * - Logistica o Sait-Abrasivi
062720060627w   1c                   when      %subst(§AR8tsd:1:1) = 'L'
062730050218     c                   exsr      DDT_Logistica
062740050217      * - Arcte
062750050215w   1c                   when      AR8tmd    = 'ARCT'
062760050217     c                   exsr      DDT_Arcte
062770050214      * - Esprinet
062780060627w   1c                   when      §AR8tsd   = 'E1'
062790050214     c                   exsr      DDT_Esprinet
062800041207      * - dati generici (con logo)
062810041207w   1c                   when      §AR8tsd   = 'S2'
062820041207     c                   exsr      DDT_genericoL
062830041207      * - dati generici (senza logo)
062840041207w   1c***                when      §AR8tsd   = 'S1'
062850040607     c                   other
062860040607     c                   exsr      DDT_generico
062870040607e   1c                   endsl
062880040607      *
062890060921     c                   reset                   $FIAR8
062900040607     c                   reset                   $DDT_o
062910040607     c                   reset                   $DDT_p
062920040608     c                   reset                   $DDTlastPg
062930040608     c                   clear                   wOmaRowT
062940040608     c                   clear                   wOmaRowD
062950040607     c                   clear                   $AR8o1
062960040607      *
062970040607     c     NOprtDDT      ENDSR
062980040607      *
062990040607      *---------------------------------------------------------------*
063000050217      *?Stampa DDT del mittente KARNAK / TITANEDI                    ?*
063010040607      *---------------------------------------------------------------*
063020040607     c     DDT_Karnak    BEGSR
063030040607      *
063040080923      /free
063050080923         // Posizionamento iniziale all'interno del foglio A4
063060080923         write  A4prtSP1;
063070080923
063080080923         // Impostazione del nuovo limite x overflow
063090080923         select;
063100080923           when  NOT *in45;
063110080923             wMaxLine = $MaxLine3;
063120080923           when  *in45   and   NOT *in46;
063130080923             wMaxLine = $MaxLine3_pl1;
063140080923           when  *in45   and   *in46;
063150080923             wMaxLine = $MaxLine3_pl2;
063160080923         endsl;
063170080923      /end-free
063180040607      *
063190040607      * Impostazione testi fissi - - - - - - - - - - - - - - - - - - -*
063200040607      *
063210040607     c                   eval      PCtx1 = 'ATTENZIONE: VERIFICARE LA -
063220040607     c                             QUANTITA'' E L''INTEGRITA'' DEI CAR-
063230040607     c                             TONI. IN CASO DI DISCORDANZA RITIRA-
063240040607     c                             RE CON RISERVA.'
063250130118     c*//                eval      PCtx2 = 'La merce viagga a rischio -
063260130118     c*//                          e pericolo del committente anche se-
063270130118     c*//                           venduta franco destino. Trascorsi -
063280130118     c*//                          otto giorni dal ricevimento della m-
063290130118     c*//                          erce non si accettano reclami.   - -
063300130118     c*//                            IN CASO DI RITARDATO PAGAMENTO DE-
063310130118     c*//                          CORRERANNO GLI INTERESSI BANCARI IN-
063320130118     c*//                           VIGORE.'
063330130118     c*//                eval      PCtx3 = 'Per ogni vertenza l''acqui-
063340130118     c*//                          rente accetta la competenza della R-
063350130118     c*//                          epubblica di San Marino o in altern-
063360130118     c*//                          ativa del foro di Rimini.'
063370130118     c*//                eval      PCtx4 = 'NON SI ACCETTANO RESI E NO-
063380130118     c*//                          N SI EFFETTUANO SOSTITUZIONI SE NON-
063390130118     c*//                           AUTORIZZATI PER ISCRITTO DALLA NOS-
063400130118     c*//                          TRA DIREZIONE COMMERCIALE.'
063410130118     c*//                eval      PCtx5 = 'Quand''anche autorizzati, -
063420130118     c*//                          i resi verranno accettati e le sost-
063430130118     c*//                          ituzioni verranno effettuate solo d-
063440130118     c*//                          opo aver accertato il perfetto stat-
063450130118     c*//                          o nel nostro stabilimento della mer-
063460130118     c*//                          ce restituita.'
063470130118     c                   eval      PCtx2 = 'Eventuali reclami per mate-
063480130118     c                             riale mancante o danneggiato all''i-
063490130118     c                             nterno dei colli dovranno pervenire-
063500130118     c                              entro 8 (otto) giorni di calendari-
063510130118     c                             o dall''avvenuta consegna a MyO S.r-
063520130118     c                             .l. - Ufficio Resi - Via Santarcang-
063530130118     c                             iolese, 6 - 47825 Torriana (RN)'
063540130118     c                   eval      PCtx4 = 'Eventuali mancanze di coll-
063550130118     c                             i e/o avarie sono da contestare all-
063560130118     c                             ''atto della consegna con annotazio-
063570130118     c                             ne su lettera di vettura e DDT'
063580040709if  1c                   if        AR8tmd = 'TITA'
063590040709     c                   eval      PCtx6 = 'Depositario: K & K Logisti-
063600040709     c                             cs S.p.a. - via Santarcangiolese, 6-
063610040709     c                              - 47825 Torriana (RN) - P.I. 00953-
063620040709     c                             860400'
063630040709e   1c                   endif
063640080822      *
063650040607      * Conteggio delle righe "omaggio" - - - - - - - - - - - - - - -*
063660040607      *
063670040614     c                   clear                   xx
063680040607     c                   eval      SAVprr = AR8prr
063690040607     c     KLDV          setgt     FIAR8000
063700040607     c     KLDV          readpe    FIAR8000
063710040607do  1c                   dow       NOT %eof(FIAR801L)
063720040607sel 2c                   select
063730040607w   2c                   when      AR8atb <> *blanks
063740040611w   2c                   when      AR8trk =  'O2'
063750040608     c                   add       1             wOmaRowD
063760040611w   2c                   when      AR8trk =  'O1'
063770040608     c                   add       1             wOmaRowT
063780051110     c                   movel(p)  AR8uni        dAR8O1
063790040607     c                   add       1             xx
063800040607     c                   eval      $AR8o1(xx) =  §AR8O1a
063810040607x   2c                   other
063820040607     c                   leave
063830040607e   2c                   endsl
063840040607     c     KLDV          readpe    FIAR8000
063850040607e   1c                   enddo
063860040607      * - mi riposiziono
063870040607     c                   eval      AR8prr = SAVprr
063880040607     c     K06AR801      setll     FIAR8000
063890040607     c     K06AR801      reade     FIAR8000
063900040607      *
063910040607      * Reperimento dati, impostazione e stampa DDT mittente  - - - -*
063920040607      *
063930040607if  1c                   dow       NOT %eof(FIAR801L)
063940040607      *
063950040607sel 2c                   select
063960040607      * - Record annullato (non si stampa nulla)
063970040607w   2c                   when      AR8atb <> *blanks
063980040607      * - Dati di dettaglio: articolo, descrizione, quantità
063990040607w   2c                   when      AR8trk = 'C1'
064000050217     c                   exsr      PrtDdtKARN_C1
064010040607      * - Dati di dettaglio: testo per articoli non disponibili
064020040607w   2c                   when      AR8trk = 'C2'
064030050217     c                   exsr      PrtDdtKARN_C2
064040040607      * - Dati di dettaglio: descrizioni generiche (orario apertura)
064050040607w   2c                   when      AR8trk = 'C3'
064060050217     c                   exsr      PrtDdtKARN_C3
064070040607      * - Dati di testata: destinatario, cliente di fatturazione
064080040607w   2c                   when      AR8trk = 'T1'
064090050217     c                   exsr      PrtDdtKARN_T1
064100040607      * - Dati di testata: agente, numero ordine, estremi documento
064110040607w   2c                   when      AR8trk = 'T2'
064120050217     c                   exsr      PrtDdtKARN_T2
064130040607      * - Dati di fondo: mezzo e causale trasporto, colli, peso
064140040607w   2c                   when      AR8trk = 'V1'
064150050217     c                   exsr      PrtDdtKARN_V1
064160040607      * - Dati di fondo: vettore, annotazioni e note
064170040607w   2c                   when      AR8trk = 'V2'
064180050217     c                   exsr      PrtDdtKARN_V2
064190040607      * - Dati esterni: riga descrittiva con referente
064200040607w   2c                   when      AR8trk = 'L1'
064210050217     c                   exsr      PrtDdtKARN_L1
064220040607      * - Dati di omaggio: testo per articoli omaggiati
064230040607w   2c                   when      AR8trk = 'O1'
064240050217     c                   exsr      PrtDdtKARN_O1
064250040607      * - Dati di omaggio: articolo, descrizione, quantità
064260040607w   2c                   when      AR8trk = 'O2'
064270050217     c                   exsr      PrtDdtKARN_O2
064280040607e   2c                   endsl
064290040607      *
064300040607     c     KLDV          reade     FIAR8000
064310040607      *
064320040607e   1c                   enddo
064330040607      *
064340040607      * Reimpostazione del vecchio limite x overflow
064350040607     c                   eval      wMaxLine = $MaxLine2
064360040607      *
064370040607     c                   ENDSR
064380050214      *
064390040607      *---------------------------------------------------------------*
064400050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
064410050217      *?Dati di testata: destinatario, cliente di fatturazione       ?*
064420040607      *---------------------------------------------------------------*
064430050217     c     PrtDdtKARN_T1 BEGSR
064440040607      *
064450040607      * > logo
064460040607     c                   eval      §Logo  = 'LOGO' + AR8tmd
064470040607      *
064480051110     c                   movel(p)  AR8uni        dAR8T1
064490040607      *
064500040607      * > destinatario
064510040607     c                   eval      PCrsd  = §AR8t1a
064520040607     c                   eval      PCind  = §AR8t1b
064530040616if  1c                   if        §AR8t1c  > *zeros
064540040607     c                   eval      PCcad  = §AR8t1c
064550040616e   1c                   endif
064560040607     c                   eval      PClod  = §AR8t1d
064570040607     c                   eval      PCprd  = §AR8t1e
064580040607     c***                eval      PCnzd  = *blanks
064590040607      * > cliente di fatturazione
064600040607     c                   eval      PCrsf  = §AR8t1f
064610040607     c                   eval      PCinf  = §AR8t1g
064620040616if  1c                   if        §AR8t1h  > *zeros
064630040607     c                   eval      PCcaf  = §AR8t1h
064640040616e   1c                   endif
064650040607     c                   eval      PClof  = §AR8t1i
064660040607     c                   eval      PCprf  = §AR8t1j
064670040607     c***                eval      PCnzf  = *blanks
064680040607      *
064690040607     c                   ENDSR
064700040607      *
064710040607      *---------------------------------------------------------------*
064720050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
064730050217      *?Dati di testata: agente, ..., porto, ...                     ?*
064740040607      *---------------------------------------------------------------*
064750050217     c     PrtDdtKARN_T2 BEGSR
064760040607      *
064770051110     c                   movel(p)  AR8uni        dAR8T2
064780040607      *
064790040607      * > dati in testata DDT
064800040607      * - - codice agente
064810130122     c*//                eval      PCage  = §AR8t2a
064820040607      * - - numero e data ordine
064830040607     c                   eval      PCnro  = §AR8t2b
064840040607     c                   eval      PCdto  = §AR8t2c
064850040607      * - - riferimenti
064860040607     c                   eval      PCrif  = §AR8t2d
064870040607      * - - codice destinatario (dal mittente)
064880040611     c                   eval      PCcli  = §AR8t2e
064890040607      * - - numero e data documento (DDT)
064900040607     c                   eval      PCnrd  = §AR8t2f
064910040607     c                   eval      PCdtd  = §AR8t2g
064920040607      * - - numero pagina documento originale (qui ricalcolato)
064930040607     c***                eval      Page   = §AR8t2h
064940040607      * - - porto
064950040607     c                   eval      PCpto  = §AR8t2i
064960040607      * - - condizioni di pagamento
064970040611     c                   eval      PCpag  = §AR8t2j
064980040607     c                                    + ' '
064990040607     c                                    + %trim(§AR8t2k)
065000040607      * - - tipologia spedizione
065010040607     c                   eval      PCtsp  = §AR8t2l
065020040607      *
065030040607      * > Stampa testata DDT - - - - - - - - - - - - - - - - - - - - -*
065040040607      *
065050040607      *   - OverFlow impossibile qui! -
065060040607     c                   write     A4prtDK1
065070040607      *
065080040607     c                   ENDSR
065090040607      *
065100040607      *---------------------------------------------------------------*
065110050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
065120050217      *?Dati di dettaglio: articolo, descrizione, quantità           ?*
065130040607      *---------------------------------------------------------------*
065140050217     c     PrtDdtKARN_C1 BEGSR
065150040607      *
065160040607     c                   clear                   A4prtDK2
065170051110     c                   movel(p)  AR8uni        dAR8C1
065180040607      *
065190040607      * > dati in dettaglio DDT
065200040607      * - - codice prodotto
065210040607     c                   eval      PCcpr  = §AR8c1a
065220040607      * - - descrizione prodotto
065230040607     c                   eval      PCdpr  = §AR8c1b
065240040607      * - - quantità
065250040607     c                   eval      PCqta  = '     ' + §AR8c1c
065260040607      * - - note
065270040607     c***                eval      PCnot  = *blanks
065280040607      *
065290040607      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
065300040607      *
065310040607     c                   exsr      Test_OF
065320040607     c                   write     A4prtDK2
065330040607      *
065340040607     c                   ENDSR
065350040607      *
065360040607      *---------------------------------------------------------------*
065370050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
065380050217      *?Dati di dettaglio: testo per articoli non disponibili        ?*
065390040607      *---------------------------------------------------------------*
065400050217     c     PrtDdtKARN_C2 BEGSR
065410040607      *
065420040607     c                   clear                   A4prtDK3
065430051110     c                   movel(p)  AR8uni        dAR8C2
065440040607      *
065450040607      * > dati in note DDT
065460040607      * - - riga descrittiva
065470040607     c                   eval      PCtxt  = §AR8c2a
065480040607      *
065490040607      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
065500040607      *
065510040607     c                   exsr      Test_OF
065520040607     c                   write     A4prtDK3
065530040607      *
065540040607     c                   ENDSR
065550040607      *
065560040607      *---------------------------------------------------------------*
065570050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
065580050217      *?Dati di dettaglio: descrizioni generiche (orario apertura)   ?*
065590040607      *---------------------------------------------------------------*
065600050217     c     PrtDdtKARN_C3 BEGSR
065610040607      *
065620040607     c                   clear                   A4prtDK2
065630051110     c                   movel(p)  AR8uni        dAR8C3
065640040607      *
065650040607      * > dati in note DDT
065660040607      * - - riga descrittiva
065670040607     c                   eval      PCdpr  = §AR8c3a
065680040607      *
065690040607      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
065700040607      *
065710040607     c                   exsr      Test_OF
065720040607     c                   write     A4prtDK2
065730040607      *
065740040607     c                   ENDSR
065750040607      *
065760040607      *---------------------------------------------------------------*
065770050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
065780050217      *?Dati di fondo: mezzo e causale trasporto, colli, peso        ?*
065790040607      *---------------------------------------------------------------*
065800050217     c     PrtDdtKARN_V1 BEGSR
065810040607      *
065820051110     c                   movel(p)  AR8uni        dAR8V1
065830040607      *
065840040607      * Controllo se ci saranno da stampare tante righe omaggio da
065850040607      *   chiudere in pagina successiva:
065860040608      *   i campi del box finale del DDT (in totale 9 righe) verranno
065870040607      *   riempiti solo nella ultima pagina.
065880040607      *
065890080924      /free
065900080924         select;
065910080924           when  NOT *in45;
065920080924      /end-free
065930040608     c                   eval      $DDTlastPg = ($MaxLine2
065940040713     c                                        +  1
065950040608     c                                        -  CurLineA4
065960040608     c                                        -  $BoxRow1
065970040608     c                                        -  wOmaRowT
065980040608     c                                        >= wOmaRowD)
065990080924      /free
066000080924           when  *in45   and   NOT *in46;
066010110406             $DDTlastPg = ($MaxLine2_pl1k
066020080924                           + 1 - CurLineA4 - $BoxRow1 - wOmaRowT
066030080924                           >= wOmaRowD);
066040080924           when  *in45   and   *in46;
066050110406             $DDTlastPg = ($MaxLine2_pl2k
066060080924                           + 1 - CurLineA4 - $BoxRow1 - wOmaRowT
066070080924                           >= wOmaRowD);
066080080924         endsl;
066090080924      /end-free
066100040607      *
066110040607      * > dati in chiusura DDT
066120040607      *   Per ora imposto (ho già impostato) solo la DS "dAR8V1".
066130040607      *   I corrispondenti campi di stampa li impostero nella subr.
066140050217      *     "PrtDdtKARN_V2".
066150040607      *
066160040607     c                   ENDSR
066170040607      *
066180040607      *---------------------------------------------------------------*
066190050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
066200050217      *?Dati di fondo: vettore, annotazioni e note                   ?*
066210040607      *---------------------------------------------------------------*
066220050217     c     PrtDdtKARN_V2 BEGSR
066230040607      *
066240051110     c                   movel(p)  AR8uni        dAR8V2
066250040618      *
066260040618      * > dati in chiusura DDT
066270040618      *
066280040607      * Ho già controllato se ci saranno da stampare tante righe
066290040607      *   omaggio da chiudere in pagina successiva
066300040608if  1c                   if        $DDTlastPg = *off
066310040607     c                   eval      PCnv2  = '          '
066320040607     c                                    + '*** SEGUE ***'
066330040618x   1c                   else
066340050217     c                   exsr      PrtDdtKARNaV2
066350040607e   1c                   endif
066360040607      *
066370040607      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
066380040608      *
066390050217     c                   exsr      PrtDdtKARNbV2
066400040607      *
066410040607      * > Impostazione nuovo limite di OverFlow  - - - - - - - - - - -*
066420080923      /free
066430080923         select;
066440080923           when  NOT *in45;
066450080923             wMaxLine = $MaxLine2;
066460080923           when  *in45   and   NOT *in46;
066470080923             wMaxLine = $MaxLine2_pl1;
066480080923           when  *in45   and   *in46;
066490080923             wMaxLine = $MaxLine2_pl2;
066500080923         endsl;
066510080923      /end-free
066520040607      *
066530040608      * N.B.: la copia per il destinatario della LDV deve comparire
066540040608      *       nell'ultima pagina del DDT per il cliente (vedi subr.
066550040608      *       "ChiusuraLDV")
066560040607      *
066570040607     c                   ENDSR
066580040618      *
066590040618      *---------------------------------------------------------------*
066600040618      *
066610050217     c     PrtDdtKARNaV2 BEGSR
066620040618      *
066630040618      * > dati in chiusura DDT
066640040618      *
066650040618      * > > da dAR8V1
066660040618      * - - mezzo di trasporto
066670040618     c                   movel     §AR8v1a       PCmtr
066680040618      * - - causale trasporto
066690040618     c                   eval      PCctr  = §AR8v1b
066700040618      * - - numero colli
066710040618     c                   eval      PCncl  = §AR8v1c
066720040618      * - - peso in kg
066730040618     c                   eval      PCpkg  = §AR8v1d
066740040618      * - - aspetto esteriore dei beni
066750040618     c                   eval      PCaeb  = §AR8v1e
066760040618      *
066770040618      * > > da dAR8V2
066780120530      * - - dati vettore: · ragione sociale (BRT S.p.A.)
066790040618      *                   · indirizzo
066800040618      *                   · c.a.p./città/provincia
066810040618      *     => NON con i dati che manda il mittente !!!
066820040618      *        ...Vuoi mai che si sbagli ad inviarci della roba !!!
066830040618     c***                eval      PCvet  = %trim(§AR8v2a)
066840040618     c***                                 + ' - '
066850040618     c***                                 + %trim(§AR8v2b)
066860040618     c***                                 + ' - '
066870040618     c***                                 + %trim(§AR8v2c)
066880120530     c*//                if        (DB0dft > *zero       and
066890120530     c*//                           DB0dft > §vpoBRTdta)  OR
066900120530     c*//                          (DB0dft = *zero       and
066910120530     c*//                           Wdsp   > §vpoBRTdta)
066920110426     c                   eval      PCvet  = $BRT
066930110426     c                                    + ' - '
066940110426     c                                    + $Intest2
066950110426     c                                    + ' (BO)'
066960120530     c*//                else
066970120530     c*//                eval      PCvet  = $Bartolini
066980120530     c*//                                 + ' - '
066990120530     c*//                                 + $Intest2
067000120530     c*//                                 + ' (BO)'
067010120530     c*//                endif
067020040618      * - - data ritiro
067030040618     c*** non stampo:    eval      PCdtv  = §AR8v2d
067040040618      * - - orario ritiro
067050040618     c*** non stampo:    eval      PChmv  = §AR8v2e
067060040618      * - - annotazioni / variazioni
067070040618     c                   eval      PCvea  = §AR8v2f
067080040618      * - - note
067090040618     c                   eval      PCnv1  = §AR8v2g
067100040618     c                   eval      PCnv2  = §AR8v2h
067110040618      *
067120040618     c                   ENDSR
067130040618      *
067140040618      *---------------------------------------------------------------*
067150040618      *
067160050217     c     PrtDdtKARNbV2 BEGSR
067170040618      *
067180040618      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
067190040618      *
067200040618      * - - Calcolo la posizione finale del box
067210040618     c                   exsr      Posiz_DK
067220040618      *
067230040618      * - - Stampa parte finale DDT
067240040618     c                   write     A4prtDK4
067250040618      *
067260040618      * > Stampa logo, box, linee e testi fissi  - - - - - - - - - - -*
067270081017     c                   if            §AR8cxd = *blanks
067280081017     c                             or  DB0fa4  = 'P'
067290050331     c                   write     A4prtDT0
067300050331     c                   endif
067310040618     c                   write     A4prtDK0
067320040618      *
067330040618     c                   ENDSR
067340040607      *
067350040607      *---------------------------------------------------------------*
067360050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
067370050217      *?Dati esterni: riga descrittiva con referente                 ?*
067380040607      *---------------------------------------------------------------*
067390050217     c     PrtDdtKARN_L1 BEGSR
067400040607      *
067410051110     c                   movel(p)  AR8uni        dAR8L1
067420040607      *
067430040607      * > righe descrittive esterne al DDT
067440040607      * - - riga descrittiva
067450040607     c                   eval      PCtxx  = §AR8l1a
067460040607      *
067470040607      * > Stampa riga testo finale DDT - - - - - - - - - - - - - - - -*
067480040618      *   - OverFlow già testato per questa riga -
067490040607     c                   write     A4prtDK5
067500040607      *
067510040607     c                   ENDSR
067520040607      *
067530040607      *---------------------------------------------------------------*
067540050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
067550050217      *?Dati di omaggio: testo per articoli omaggiati                ?*
067560040607      *---------------------------------------------------------------*
067570050217     c     PrtDdtKARN_O1 BEGSR
067580040608      *
067590040608     c                   eval      $DDT_o = *on
067600040607      *
067610040607     c                   clear                   A4prtDK5
067620051110     c                   movel(p)  AR8uni        dAR8O1
067630040607      *
067640040607      * > righe descrittive esterne al DDT
067650040607      * - - riga descrittiva
067660040607     c                   eval      PCtxx  = §AR8o1a
067670040607      * - - nessuna sottolineatura
067680040608     c***                eval      *in39  = *off
067690040607      *
067700040607      * > Stampa testata omaggi in DDT - - - - - - - - - - - - - - - -*
067710040608      *   (se NON già stampata nella nuova pagina x OverFlow)
067720040608     c                   eval      SavLineA4 = CurLineA4
067730040607     c                   exsr      Test_OF
067740040608if  1c                   if        CurLineA4 = SavLineA4
067750040607     c                   write     A4prtDK5
067760040608e   1c                   endif
067770040608      * ...Dovendo ristampare le righe di intestazione degli omaggi
067780040608      *   in ogni pagina (da quella del 1° omaggio in poi), non calcolo
067790040608      *   quelle già stampate...
067800040608     c*** NO:            sub       1             wOmaRowT
067810040607      *
067820040607     c                   ENDSR
067830040607      *
067840040607      *---------------------------------------------------------------*
067850050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
067860050217      *?Dati di omaggio: articolo, descrizione, quantità             ?*
067870040607      *---------------------------------------------------------------*
067880050217     c     PrtDdtKARN_O2 BEGSR
067890040607      *
067900040607     c                   clear                   A4prtDK2
067910051110     c                   movel(p)  AR8uni        dAR8O2
067920040607      *
067930040607      * > dettaglio omaggi DDT
067940040607      * - - codice prodotto
067950040607     c                   eval      PCcpr  = §AR8o2a
067960040607      * - - descrizione prodotto
067970040607     c                   eval      PCdpr  = §AR8o2b
067980040607      * - - quantità
067990040618     c                   eval      PCqta  = '     ' + §AR8o2c
068000040607      * - - note
068010040607     c***                eval      PCnot  = *blanks
068020040607      *
068030040607      * > Stampa dettaglio omaggi in DDT - - - - - - - - - - - - - - -*
068040040607      *
068050040607     c                   exsr      Test_OF
068060040607     c                   write     A4prtDK2
068070040608     c                   sub       1             wOmaRowD
068080040607      *
068090040607     c                   ENDSR
068100040608      *
068110040608      *---------------------------------------------------------------*
068120081002      *?Impostazione nuova pagina di DDT Karnak / Titanedi / EDk     ?*
068130040608      *---------------------------------------------------------------*
068140081002     c     NewPageDK     BEGSR
068150040608      *
068160080923      /free
068170080923         // Impostazione del nuovo limite x overflow
068180080923         select;
068190080923           when  NOT *in45;
068200080923             wMaxLine = $MaxLine3;
068210080923           when  *in45   and   NOT *in46;
068220080923             wMaxLine = $MaxLine3_pl1;
068230080923           when  *in45   and   *in46;
068240080923             wMaxLine = $MaxLine3_pl2;
068250080923         endsl;
068260080923      /end-free
068270080923     c                   write     A4prtSP1
068280040608     c                   write     A4prtDK1
068290040608      *
068300040608if  1c                   if        $DDT_o = *on
068310040608      *
068320040608      * - Stampa 1 riga di dettaglio vuota
068330040608     c                   clear                   A4prtDK3
068340040608     c                   write     A4prtDK3
068350040618      * - Reimposta i dati nel box finale del DDT
068360050217     c                   exsr      PrtDdtKARNaV2
068370040618      * - Stampa fondo DDT
068380050217     c                   exsr      PrtDdtKARNbV2
068390040608      * - Stampa ultima riga
068400040608      *   ("Per qualsiasi problema o chiarimento contattare...)
068410040608     c                   clear                   A4prtDK5
068420040608     c                   eval      PCtxx  = §AR8l1a
068430040608     c                   write     A4prtDK5
068440040608      * - Stampa intestazione per merce omaggio
068450040608      *   xx = numero di righe per "intestazione omaggi"
068460040608     c                   eval      wx     = xx
068470040608do  2c                   dow           xx         >  *zeros
068480040608     c                             and xx         <= %elem($AR8o1)
068490040608     c                             and $AR8o1(xx) <> *blanks
068500040608     c                   eval      PCtxx  = $AR8o1(xx)
068510040608     c                   write     A4prtDK5
068520040608     c                   eval      xx     = xx - 1
068530040608e   2c                   enddo
068540040608     c                   eval      xx     = wx
068550040608      * -Se sta leggendo il tipo record "O1":?
068560040608      *   sposta il ciclo di lettura del file FIAR8 sul record
068570040608      *   successivo all'ultimo (appena stampato) dell'intestazione
068580040608      *   della merce omaggiata
068590040608if  2c                   if        AR8trk =  'O1'
068600040608if  3c                   dou       AR8trk <> 'O1'
068610040608     c     KLDV          reade     FIAR8000
068620040608e   3c                   enddo
068630040608if  3c*** è così:        if        AR8trk = 'O2'
068640040608     c     KLDV          readpe    FIAR8000
068650040707e   3c***                endif
068660040608e   2c                   endif
068670040608      * - Aggiorna controllo se "ultima pagina"
068680080924      /free
068690080924           select;
068700080924             when  NOT *in45;
068710080924      /end-free
068720040608     c                   eval      $DDTlastPg = ($MaxLine2
068730040713     c                                        +  1
068740040608     c                                        -  CurLineA4
068750040608     c                                        -  $BoxRow1
068760040608     c                                        -  wOmaRowT
068770040608     c                                        >= wOmaRowD)
068780080924      /free
068790080924             when  *in45   and   NOT *in46;
068800110406               $DDTlastPg = ($MaxLine2_pl1k
068810080924                             + 1 - CurLineA4 - $BoxRow1 - wOmaRowT
068820080924                             >= wOmaRowD);
068830080924             when  *in45   and   *in46;
068840110406               $DDTlastPg = ($MaxLine2_pl2k
068850080924                             + 1 - CurLineA4 - $BoxRow1 - wOmaRowT
068860080924                             >= wOmaRowD);
068870080924           endsl;
068880080924      /end-free
068890040608      *
068900040608e   1c                   endif
068910040608      *
068920040608     c                   ENDSR
068930040607      *
068940040607      *---------------------------------------------------------------*
068950050414      *?Posizionamento di linee e box nel DDT KARNAK / TITANEDI / EDk?*
068960040607      *---------------------------------------------------------------*
068970040607     c     Posiz_DK      BEGSR
068980040607      *
068990080918      /free
069000080923         // Impostazione posizioni di logo/box
069010080923         // (posizioni di "partenza" diverse a seconda del posizionam.
069020080923         //  iniziale nel foglio A4 attualmente in stampa)
069030080923         select;
069040080923           when  NOT *in45;
069050081002             §PB1 =  3.120;
069060080923           when  *in45   and   NOT *in46;
069070081002             §PB1 = 0.250;
069080080923           when  *in45   and   *in46;
069090081002             §PB1 = 6.000;
069100080923         endsl;
069110080918      /end-free
069120040607     c                   eval      §PB2 =  §PB1 + 1.23
069130080822     c                   eval      §PB3 =  §PB2 + 0.18
069140040607     c                   eval(h)   §PB4 =  CurLineA4 / 8 + 0.20
069150040607     c                   eval      §PB5 =  §PB4 + 0.18
069160040607     c                   eval      §PB6 =  §PB5 + 0.77
069170040607     c                   eval      §PB7 =  §PB5 + 0.165
069180040607     c                   eval      §PB8 =  §PB7 + 0.08
069190040607      * Impostazione posizioni di linee orizzontali nel 1° box
069200040607     c                   eval(h)   §LO1 =  §PB1 + (§PB2 - §PB1) / 2
069210040607      * Impostazione posizioni di linee orizzontali nel 2° box
069220080822     c                   eval      §LO2 =  §PB3 + 0.23
069230080822     c                   eval      §LO3 =  §LO2 + 0.145
069240080822     c                   eval      §LO4 =  §LO3 + 0.10
069250040607     c                   eval      §LO5 =  §PB4 - 0.15
069260040607      * Impostazione posizioni di linee orizzontali nel 3° box
069270040607     c                   eval      §LO6 =  §PB5 + 0.14
069280040607     c                   eval      §LO7 =  §LO6 + 0.12
069290040607     c                   eval      §LO8 =  §LO7 + 0.12
069300040607     c                   eval      §LO9 =  §LO8 + 0.13
069310040607      * Impostazione posizioni di linee verticali nel 2° box
069320040607     c                   eval      §LV1 =  §LO2 - §PB3
069330040607     c                   eval      §LV2 =  §LO3 - §LO2
069340040607     c                   eval      §LV3 =  §LO5 - §LO3
069350040607      * Impostazione posizioni di linee verticali nel 3° box
069360040607     c                   eval      §LV4 =  §LO6 - §PB5
069370040607     c                   eval      §LV5 =  §LO7 - §LO6
069380040607     c                   eval      §LV6 =  §LO8 - §LO7
069390040607     c                   eval      §LV7 =  §PB6 - §LO9
069400040607      * Impostazione posizioni di campi
069410040607     c                   eval      §PS1 =  §LO5 + 0.11
069420040607     c                   eval      §PS2 =  §PB4 + 0.08
069430040607     c                   eval      §PS3 =  §PS2 + 0.07
069440130118if  1c*//                if        §PB6 <  §PB1 + 4.58
069450130118     c*//                eval      §PR1 =  §PB1 + 4.58
069460130118if  1c                   if        §PB6 <  §PB1 + 3.30
069470130118     c                   eval      §PR1 =  §PB1 + 3.30
069480040607x   1c                   else
069490040607     c                   eval      §PR1 =  §PB6
069500040607e   1c                   endif
069510040607      *
069520040607     c                   ENDSR
069530041019      *
069540041019      *---------------------------------------------------------------*
069550050414      *?Stampa DDT del mittente LOGISTICA / SAIT ABRASIVI            ?*
069560041019      *---------------------------------------------------------------*
069570050218     c     DDT_Logistica BEGSR
069580041019      *
069590080924      /free
069600080924         // Impostazione del nuovo limite x overflow
069610080924         // (Codesto limite è valido solo per la stampa del dettaglio.
069620080924         //   Per la stampa del "fondo" bolla occorre REimpostarlo ad
069630080924         //   un valore più basso.
069640080924         //  Ovvero: stampo più righe di dettaglio che posso; se alla
069650080924         //  fine delle righe di dettaglio non rimane spazio a suffi-
069660080924         //  cienza per il fondo: prima della stampa del fondo si salta
069670080924         //  alla pagina successiva, dove non si stampa alcuna riga di
069680080924         //  dettaglio.)
069690080924         select;
069700080924           when  NOT *in45;
069710090323             wMaxLine = $MaxLine2;
069720080924           when  *in45   and   NOT *in46;
069730090323             wMaxLine = $MaxLine2_pl1;
069740080924           when  *in45   and   *in46;
069750090323             wMaxLine = $MaxLine2_pl2;
069760080924         endsl;
069770080924
069780080924         // Posizionamento iniziale all'interno del foglio A4
069790080924         write  A4prtSP1;
069800081006
069810081006         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
069820081006           write  A4prtNUL;
069830081006         endif;
069840080924      /end-free
069850041019      *
069860041019      * Impostazione testi fissi - - - - - - - - - - - - - - - - - - -*
069870041019      *
069880071008      * - prima solo per "Logistica", ora neanche più per lei...
069890071008      * - Sait Abrasivi SpA & Brumar Srl hanno Tipo Stampa DDT = "LC"
069900071008if  1c***                if        §AR8tsd <> 'LC'
069910071008     c***                eval      PCLtx1 = 'La merce viagga a rischio -
069920071008     c***                          e pericolo del Committente, pertanto -
069930071008     c***                          la Venditrice non risponde di -
069940071008     c***                          eventuali perdite o avarie derivanti -
069950071008     c***                          dal trasporto.'
069960071008     c***                eval      PCLtx2 = 'Il Destinatario, per -
069970071008     c***                          ottenere il risarcimento del danno, -
069980071008     c***                          deve far constatare al Corriere la -
069990071008     c***                          perdita o l''avaria all''atto del -
070000071008     c***                          ricevimento della merce.'
070010071008e   1c***                endif
070020050218      *
070030130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
070040130315      *  "StampaDDT")
070050050218     c***  KLDV          setll     FIAR8000       (già fatto)
070060050218     c***  KLDV          reade     FIAR8000       (già fatto)
070070041019      *
070080041019      * Reperimento dati, impostazione e stampa DDT mittente  - - - -*
070090041019      *
070100041019if  1c                   dow       NOT %eof(FIAR801L)
070110050318      *
070120050318if  2c                   if        AR8atb   = *blanks
070130050318     c                   eval      SAVtrk   = AR8trk
070140050318e   2c                   endif
070150041019      *
070160041019sel 2c                   select
070170041019      * - Record annullato (non si stampa nulla)
070180050218w   2c                   when      AR8atb  <> *blanks
070190041019      * - Dati di dettaglio: articolo, descrizione, quantità
070200050218w   2c                   when      AR8trk   = 'C1'
070210050217     c                   exsr      PrtDdtLOG1_C1
070220041103      * - Dati di dettaglio: descrizioni generiche
070230050218w   2c                   when      AR8trk   = 'C2'
070240050217     c                   exsr      PrtDdtLOG1_C2
070250041103      * - Dati di dettaglio: descrizioni generiche
070260050218w   2c                   when      AR8trk   = 'C3'
070270050217     c                   exsr      PrtDdtLOG1_C3
070280060614      * - Dati di dettaglio: corpo bolla non fincato
070290060614w   2c                   when      AR8trk   = 'C4'
070300060614     c                   exsr      PrtDdtLOG1_C4
070310090323      * - Dati di dettaglio: descrizioni generiche
070320090323w   2c                   when      AR8trk   = 'C5'
070330090323     c                   exsr      PrtDdtLOG1_C5
070340050318      * - Dati di packing-list: dettaglio
070350050318w   2c                   when      AR8trk   = 'Z1'
070360050318     c                   exsr      PrtDdtLOG1_Z1
070370041019      * - Dati di testata: sito e depositante
070380050218w   2c                   when      AR8trk   = 'T1'
070390050217     c                   exsr      PrtDdtLOG1_T1
070400041019      * - Dati di testata: destinatario, cliente di fatturazione
070410050218w   2c                   when      AR8trk   = 'T2'
070420050217     c                   exsr      PrtDdtLOG1_T2
070430041019      * - Dati di testata: agente, numero ordine, estremi documento
070440050218w   2c                   when      AR8trk   = 'T3'
070450050217     c                   exsr      PrtDdtLOG1_T3
070460041019      * - Dati di fondo: mezzo e causale trasporto, colli, peso
070470050218w   2c                   when      AR8trk   = 'P1'
070480050217     c                   exsr      PrtDdtLOG1_P1
070490050606      * - Dati di testata: FATTURA
070500050606w   2c                   when      AR8trk   = 'S1'
070510050606     c                   exsr      PrtDdtLOG1_S1
070520050606      * - Dati di dettaglio cpi 15: FATTURA
070530050606w   2c                   when      AR8trk   = 'S2'
070540050606     c                   exsr      PrtDdtLOG1_S2
070550050606      * - Dati di dettaglio cpi 20: FATTURA
070560050606w   2c                   when      AR8trk   = 'S3'
070570050606     c                   exsr      PrtDdtLOG1_S3
070580050606      * - Dati di fondo: FATTURA
070590050606w   2c                   when      AR8trk   = 'S4'
070600050606     c                   exsr      PrtDdtLOG1_S4
070610050606      * - Dati di fondo note: FATTURA
070620050606w   2c                   when      AR8trk   = 'S5'
070630050606     c                   exsr      PrtDdtLOG1_S5
070640041019e   2c                   endsl
070650041019      *
070660041019     c     KLDV          reade     FIAR8000
070670041019      *
070680041019e   1c                   enddo
070690050318      *
070700050318      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
070710050318      *
070720050505      * - se DDT con Packing-List:
070730050505      *   (altrimenti il box è già stato stampato in sr PrtDdtLOG1_P1)
070740050505if  1c                   if        %subst(SAVtrk:1:1) =  'Z'
070750050318      *
070760050318      * - - Calcolo la posizione finale del box e di linee separatrici
070770050318     c                   exsr      Posiz_DL
070780050318      *
070790050318      * > Stampa box, linee e testi fissi (no logo)  - - - - - - - - -*
070800081017     c                   if            §AR8cxd = *blanks
070810081017     c                             or  DB0fa4  = 'P'
070820050331     c                   write     A4prtDT0
070830050331     c                   endif
070840050318     c                   write     A4prtDL0C
070850050505      *
070860050505e   1c                   endif
070870050218      *
070880050218      * Reimpostazione del vecchio limite x overflow
070890080925      /free
070900080925         select;
070910080925           when  NOT *in45;
070920090323             wMaxLine = $MaxLine2;
070930080925           when  *in45   and   NOT *in46;
070940090323             wMaxLine = $MaxLine2_pl1;
070950080925           when  *in45   and   *in46;
070960090323             wMaxLine = $MaxLine2_pl2;
070970080925         endsl;
070980080925      /end-free
070990041019      *
071000041019     c                   ENDSR
071010081222
071020081222      /free
071030081222       //--------------------------------------------------------------
071040140307       //?Preparazione simil-logo Bartolini                            ?
071050081222       //--------------------------------------------------------------
071060090406       BEGSR  PrtDdtLOG1_SimilLogo;
071070081222
071080120530         //If  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
071090120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
071100120530         //
071110120530         //  // -?Preparazione simil-Logo BARTOLINI?
071120120530         //  §PLBLS  = §PBA + 0.150;
071130120530         //  §PLBLVS = 0.400;
071140120530         //  exsr  sr_SimilLogoBAR;
071150120530         //
071160120530         //  // -?Preparazione simil-Logo BRT?
071170120530         //  §P2blS  = §PlblS;
071180120530         //  //§P2blVS = §PlblVD + 0.050;
071190120530         //  §P2blVS = §PlblVD + 0.030;
071200120530         //  exsr  sr_SimilLogoBRT;
071210120530         //
071220120530         //Else;
071230120530         //
071240120530         //  // -?Preparazione simil-Logo BARTOLINI?
071250120530         //  §PLBLS  = §PBA + 0.050;
071260120530         //  §PLBLVS = 0.400;
071270120530         //  exsr  sr_SimilLogoBART;
071280120530         //
071290120530         //EndIf;
071300120530
071310120530         // -?Preparazione simil-Logo BRT?
071320120530         §PLBLS  = §PBA + 0.050;
071330120530         §PLBLVS = 0.400;
071340120530         exsr  sr_SimilLogoBRT_;
071350090406
071360120530         // -?Impostazione dati anagrafici BRT alla sua destra?
071370090422         §PC0    = §PBA    + 0.070 + 0.050;
071380090406         §PC1    = §PC0    + 0.100;
071390090406         §PC2    = §PC1    + 0.100;
071400090406         §PC3    = §PC2    + 0.090;
071410090406         §PC4    = §PC3    + 0.090;
071420090406         §PC5    = §PC4    + 0.090;
071430120530
071440120530         //§PLC    = 1.400;
071450120530         §PLC    = 1.000;
071460090406
071470120530         // -?"TBAR" è già correttamente impostato.?
071480120530         //TBAR    = $Bartolini;        (già così)
071490120530         //TBAR    = $BRT;              (già così)
071500090406         LBA1    = $Intest1;
071510090406         LBA2    = $Intest2_b;
071520120530         //LBA3    = $Intest3_b;
071530120530         LBA3    = $Intest3_b_brt;
071540090406         LBA4    = $Intest4_b;
071550090406         LBA5    = $Intest5_b;
071560081222
071570081222       ENDSR;
071580081222      /end-free
071590050214      *
071600041019      *---------------------------------------------------------------*
071610050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
071620050217      *?Dati di testata: sito e depositante                          ?*
071630041019      *---------------------------------------------------------------*
071640050217     c     PrtDdtLOG1_T1 BEGSR
071650041019      *
071660041019      * > logo
071670050218     c                   eval      §Logo    = 'LOGO' + AR8tmd
071680090107      /free
071690090107         // - Impostazione indicatore che condiziona la stampa del logo
071700120530         //   (o quella del "simil-logo" BRT)
071710090107         *in55 = (%subst(AR8tmd : 1 : 3) = 'LOG'          and
071720090107                  (§AR8tsd  = 'L1'  or  §AR8tsd  = 'L3'));
071730090107      /end-free
071740080908      *
071750060609      *? L O G O !! ?
071760060609      * per pulizia non vogliamo più avere tanti LOGx per ogni cliente che
071770060609      * vuole la stampa della fattura con il proprio logo e la bolla con
071780120530      * il logo BRT
071790060609      * anche Cappellini è d'accordo ad inviare i dati con il logo del
071800060609      * cliente da stampare nella LDV
071810060609      * quelli già esistenti avranno un nuovo logo un po' più piccolo per
071820071008      * problemi di spazio e i dati saranno inviati con AR8tmd = al nome del
071830071008      * nuovo logo e la tabella sarà fatta con §AR8tsd = 'L2' o 'L4'
071840060609      * i nuovi faremo in modo di avere o lo stesso logo oppure la stessa
071850060609      * logica descritta prima
071860060609      * ma può essere che un cliente voglia la fattura con il suo logo
071870120530      * mentre la bolla con il logo BRT quindi devo recuperare
071880060609      * il logo da stampare nella bolla non più da AR8TMD, che conterrà
071890060609      * il nome del logo da stampare nella fattura, ma dal campo del
071900120530      * del logo fattura, che conterrà il logo BRT, mentre il campo
071910071008      * §AR8tsd nella tabella sarà 'L1' o 'L3'
071920071008     c                   if        (§AR8tsd = 'L1' or §AR8tsd = 'L3') and
071930071008     c                             §AR8logof <> *blanks and
071940071008     c                             %subst(AR8tmd:1:3) <> 'LOG'
071950071008     c                   eval      §Logo    = 'LOGO' + §AR8logof
071960090107      /free
071970090107           // - Impostaz. indicatore che condiziona la stampa del logo
071980120530           //   (o quella del "simil-logo" BRT)
071990090107           *in55 = (%subst(§AR8logof : 1 : 3) = 'LOG');
072000090107      /end-free
072010060609     c                   endif
072020041019      *
072030080822     c                   movel(p)  AR8uni        dAR8LOG1T1
072040050414      *
072050060627if  1c                   if            §AR8tsd <> 'LC'
072060041019      *
072070041019      * > sito
072080060407     c                   select
072090071008     c                   when      §AR8tsd = 'L1' or
072100071008     c                             §AR8tsd = 'L3'
072110050707     c                   eval      PLOG1st0 = 'Deposito di:'
072120071008     c                   when      §AR8tsd = 'L2' or
072130071008     c                             §AR8tsd = 'L4'
072140060407     c                   eval      PLOG1st0 = 'Depositario:'
072150060407     c                   endsl
072160050707     c                   eval      PLOG1st1 = §A8LOG1t1a
072170050707     c                   eval      PLOG1st2 = §A8LOG1t1b
072180050707     c                   eval      PLOG1st3 = §A8LOG1t1c
072190050707     c                   eval      PLOG1st4 = §A8LOG1t1d
072200050707     c                   eval      PLOG1st5 = §A8LOG1t1e
072210041019      *
072220041019      * > depositante
072230060407     c                   select
072240071008     c                   when      §AR8tsd = 'L1' or
072250071008     c                             §AR8tsd = 'L3'
072260050707     c                   eval      PLOG1de0 = 'Depositario della:'
072270071008     c                   when      §AR8tsd = 'L2' or
072280071008     c                             §AR8tsd = 'L4'
072290060407     c                   eval      PLOG1de0 = 'Deposito di:'
072300060407     c                   endsl
072310050707     c                   eval      PLOG1de1 = §A8LOG1t1f
072320050707     c                   eval      PLOG1de2 = §A8LOG1t1g
072330050707     c                   eval      PLOG1de3 = §A8LOG1t1h
072340050707     c                   eval      PLOG1de4 = §A8LOG1t1i
072350050707     c                   eval      PLOG1de5 = §A8LOG1t1j
072360050414      *
072370050414x   1c                   else
072380050414      *
072390050707     c***                eval      PLOG1st0 = *blanks
072400050707     c                   eval      PLOG1st1 = §A8LOG1t1a + §A8LOG1t1b
072410050707     c                   eval      PLOG1st2 = §A8LOG1t1c + §A8LOG1t1d
072420050707     c                   eval      PLOG1st3 = §A8LOG1t1e + §A8LOG1t1f
072430050707     c                   eval      PLOG1st4 = §A8LOG1t1g + §A8LOG1t1h
072440050707     c                   eval      PLOG1st5 = §A8LOG1t1i + §A8LOG1t1j
072450050414      *
072460050414e   1c                   endif
072470050214      *
072480041022     c                   ENDSR
072490050214      *
072500041019      *---------------------------------------------------------------*
072510050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
072520050217      *?Dati di testata: destinatario, cliente di fatturazione       ?*
072530041019      *---------------------------------------------------------------*
072540050217     c     PrtDdtLOG1_T2 BEGSR
072550041019      *
072560051110     c                   clear                   dAR8LOG1T2
072570050218     c                   movel     AR8uni        dAR8LOG1T2
072580041019      *
072590041019      * > destinatario
072600050707if  1c                   if        §A8LOG1T2a <> *blanks
072610050707     c                   eval      PLOG1ccd   =  §A8LOG1T2a
072620050218e   1c                   endif
072630050707if  1c                   if        §A8LOG1T2c =  *blanks
072640050707     c                   clear                   PLOG1rsd
072650050707     c                   eval      PLOG1rsd2  =  §A8LOG1T2b
072660050218x   1c                   else
072670050707     c                   eval      PLOG1rsd   =  §A8LOG1T2b
072680050707     c                   eval      PLOG1rsd2  =  §A8LOG1T2c
072690050218e   1c                   endif
072700050707     c                   eval      PLOG1ind   =  §A8LOG1T2d
072710050707     c                   eval      PLOG1ind2  =  §A8LOG1T2e
072720050707if  1c                   if        §A8LOG1T2f >  *zeros
072730050707     c                   eval      PLOG1cad   =  §A8LOG1T2f
072740041019e   1c                   endif
072750050707     c                   eval      PLOG1lod   =  §A8LOG1T2g
072760041019      * > cliente di fatturazione
072770050707if  1c                   if        §A8LOG1T2h <> *blanks
072780050707     c                   eval      PLOG1ccf   =  §A8LOG1T2h
072790041019e   1c                   endif
072800050707if  1c                   if        §A8LOG1T2j =  *blanks
072810050707     c                   clear                   PLOG1rsf
072820050707     c                   eval      PLOG1rsf2  =  §A8LOG1T2i
072830050218x   1c                   else
072840050707     c                   eval      PLOG1rsf   =  §A8LOG1T2i
072850050707     c                   eval      PLOG1rsf2  =  §A8LOG1T2j
072860050218e   1c                   endif
072870050707     c                   eval      PLOG1inf   =  §A8LOG1T2k
072880050707     c                   eval      PLOG1inf2  =  §A8LOG1T2l
072890050707if  1c                   if        §A8LOG1T2m >  *zeros
072900050707     c                   eval      PLOG1caf   =  §A8LOG1T2m
072910041019e   1c                   endif
072920050707     c                   eval      PLOG1lof   =  §A8LOG1T2n
072930041019      *
072940041019     c                   ENDSR
072950041019      *
072960041019      *---------------------------------------------------------------*
072970050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
072980050217      *?Dati di testata: documento, rif.ordine, note                 ?*
072990041019      *---------------------------------------------------------------*
073000050217     c     PrtDdtLOG1_T3 BEGSR
073010041019      *
073020051110     c                   clear                   dAR8LOG1T3
073030050218     c                   movel     AR8uni        dAR8LOG1T3
073040050706      *
073050050706      * TEST effettiva numericità dei campi previsti numerici:
073060100513     c                   clear                   w053a
073070111013     ***c                   eval      WX    =  %len(§A8LOG1T3b)
073080111013     ***c                                   +  %len(§A8LOG1T3c)
073090111013     ***c                                   +  %len(§A8LOG1T3d)
073100111013     ***c                   eval      w053a =  %subst(dAR8LOG1T3: 4:WX)
073110111013     ***c                   eval      *in31 =  *off
073120111013     ***c     c_Digits      checkr    w053a :  WX                            31
073130111013if  1***c                   if        *in31
073140111013     ***c                   clear                   §A8LOG1T3b
073150111013     ***c                   clear                   §A8LOG1T3c
073160111013     ***c                   clear                   §A8LOG1T3d
073170111013e   1***c                   endif
073180121116     ***c                   eval      WX    =  %len(§A8LOG1T3b)
073190121116     ***c                   eval      w053a =  %subst(dAR8LOG1T3: 4:WX)
073200121116     ***c                   eval      *in31 =  *off
073210121116     ***c     c_Digits      checkr    w053a :  WX                            31
073220121116if  1***c                   if        *in31
073230121116     ***c                   clear                   §A8LOG1T3b
073240121116e   1***c                   endif
073250111013     c                   clear                   w053a
073260111013     c                   eval      WX    =  %len(§A8LOG1T3d)
073270111013     c                   eval      w053a =  %subst(dAR8LOG1T3:15:WX)
073280111013     c                   eval      *in31 =  *off
073290111013     c     c_Digits      checkr    w053a :  WX                            31
073300111013if  1c                   if        *in31
073310111013     c                   clear                   §A8LOG1T3d
073320111013e   1c                   endif
073330041019      *
073340041019      * > dati in testata DDT
073350041019      * - - numero e data documento (DDT)
073360050707     c                   eval      PLOG1nsr = §A8LOG1T3a
073370121116     ***c                   eval      PLOG1aas = §A8LOG1T3b
073380121116     c                   if        §A8Log1T3b <> *blanks   and
073390121116     c                             §A8Log1T3b <> *zeros
073400121116     c                   eval      PLOG1aas = §A8LOG1T3b
073410121116     c                   endif
073420111013     ***c                   if        §A8Log1T3c <> *zeros
073430111013     ***c                   eval      PLOG1nsp = %editc(§A8LOG1T3c:'X')
073440111013     ***c                   endif
073450111013     c                   if        §A8Log1T3c <> *blanks   and
073460111013     c                             §A8Log1T3c <> *zeros
073470111013     c                   eval      PLOG1nsp = §A8LOG1T3c
073480111013     c                   endif
073490100319     c                   if        §A8Log1T3a <> *blanks   and
073500121116     c                             §A8Log1T3b <> *blanks   and
073510100319     c                             §A8Log1T3b <> *zeros
073520100319     c                   eval      PLog1sepA = '/'
073530100319     c                   endif
073540121115     c                   if        §A8Log1T3b <> *zeros    and
073550121115     c                             §A8Log1T3c <> *blanks   and
073560100319     c                             §A8Log1T3c <> *zeros
073570100319     c                   eval      PLog1sepB = '/'
073580100319     c                   endif
073590110707      *     giro la data del documento
073600050707if  1c                   if        §A8LOG1T3d > *zeros
073610060922     c                   eval      *in31 =  *off
073620060922     c     *iso          test(d)                 §A8LOG1T3d             31
073630060922if  2c                   if        NOT *in31
073640050707     c     *iso          movel     §A8LOG1T3d    Data_EUR
073650050707     c     *eur          movel     Data_EUR      PLOG1dsp
073660060922x   2c                   else
073670060922     c                   clear                   PLOG1dsp
073680060922e   2c                   endif
073690050707e   1c                   endif
073700041019      * - - riferimento ordine
073710050707     c                   eval      PLOG1ref = §A8LOG1T3e
073720041019      * - - note consegna
073730050707     c                   eval      PLOG1not = §A8LOG1T3f
073740050707     c                   eval      PLOG1nt2 = §A8LOG1T3g
073750060612      * - - intestazione corpo bolla per Logistica tipo stampa L3 o L4
073760060922     c                   if        §AR8tsd = 'L3' or §AR8tsd = 'L4'
073770060612     c                   eval      *in23 = *On
073780060612     c                   eval      PLOG1int = §A8LOG1T3h
073790060612     c                   else
073800060612     c                   eval      *in23 = *off
073810060612     c                   endif
073820041019      *
073830041019      * > Stampa testata DDT - - - - - - - - - - - - - - - - - - - - -*
073840041019      *
073850041019      *   - OverFlow impossibile qui! -
073860050218     c***                exsr      Test_OF
073870050318     c                   exsr      Posiz_DL
073880081017     c                   if            §AR8cxd = *blanks
073890081017     c                             or  DB0fa4  = 'P'
073900050331     c                   write     A4prtDT0
073910050331     c                   endif
073920050218     c                   write     A4prtDL0
073930081222      /free
073940081222         if  *in55;
073950090406           exsr   PrtDdtLOG1_SimilLogo;
073960120530           //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
073970120530           //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
073980120530           //  write  A4LogoBR2;
073990120530           //else;
074000120530           //  write  A4LogoBAR;
074010120530           //endif;
074020120530           write  A4LogoBRT;
074030081222           write  A4prtDLL;
074040081222         endif;
074050081222      /end-free
074060050218     c*** quante righe?  write     A4prtDL0A
074070041028     c                   write     A4prtDL1A
074080050218     c                   write     A4prtDL1B
074090041019      *
074100041019     c                   ENDSR
074110050214      *
074120041019      *---------------------------------------------------------------*
074130050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
074140050217      *?Dati di dettaglio: articolo, quantità, descrizione           ?*
074150041019      *---------------------------------------------------------------*
074160050217     c     PrtDdtLOG1_C1 BEGSR
074170041019      *
074180041019     c                   clear                   A4prtDL2
074190051110     c                   clear                   dAR8LOG1C1
074200050218     c                   movel     AR8uni        dAR8LOG1C1
074210050706      *
074220050706      * TEST effettiva numericità dei campi previsti numerici:
074230100513     c                   clear                   w053a
074240050706     c                   eval      WX    =  %len(§A8LOG1C1b)
074250100513     c                   eval      w053a =  %subst(dAR8LOG1C1:21:WX)
074260050706     c                   eval      *in31 =  *off
074270110426     c     c_Digits      checkr    w053a :  WX                            31
074280050706if  1c                   if        *in31
074290050706     c                   clear                   §A8LOG1C1b
074300050706e   1c                   endif
074310041019      *
074320041019      * > dati in dettaglio DDT
074330041019      * - - codice prodotto
074340050707     c                   eval      PLOG1cpr  = §A8LOG1C1a
074350041019      * - - quantità
074360050707     c                   eval      PLOG1qta  = §A8LOG1C1b
074370041019      * - - descrizione prodotto
074380050707     c                   eval      PLOG1dpr  = §A8LOG1C1c
074390041019      *
074400041019      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
074410041019      *
074420041019     c                   exsr      Test_OF
074430041019     c                   write     A4prtDL2
074440041019      *
074450041019     c                   ENDSR
074460041019      *
074470041019      *---------------------------------------------------------------*
074480050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
074490050217      *?Dati di dettaglio: descrizione                               ?*
074500041019      *---------------------------------------------------------------*
074510050217     c     PrtDdtLOG1_C2 BEGSR
074520041019      *
074530041019     c                   clear                   A4prtDL2
074540051110     c                   clear                   dAR8LOG1C2
074550050218     c                   movel     AR8uni        dAR8LOG1C2
074560041019      *
074570041019      * > dati in note DDT
074580041019      * - - riga descrittiva
074590050707     c                   eval      PLOG1dpr  = §A8LOG1C2a
074600041019      *
074610041019      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
074620041019      *
074630041019     c                   exsr      Test_OF
074640041019     c                   write     A4prtDL2
074650041019      *
074660041019     c                   ENDSR
074670041019      *
074680041019      *---------------------------------------------------------------*
074690050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
074700050217      *?Dati di dettaglio: descrizioni                               ?*
074710041019      *---------------------------------------------------------------*
074720050217     c     PrtDdtLOG1_C3 BEGSR
074730041019      *
074740041019     c                   clear                   A4prtDL3
074750051110     c                   clear                   dAR8LOG1C3
074760050707     c                   movel     AR8uni        dAR8LOG1C3
074770041019      *
074780041019      * > dati in note DDT
074790041019      * - - riga descrittiva
074800050707     c                   eval      PLOG1txt  = §A8LOG1C3a
074810041019      *
074820041019      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
074830041019      *
074840041019     c                   exsr      Test_OF
074850041019     c                   write     A4prtDL3
074860041019      *
074870041019     c                   ENDSR
074880060614      *
074890060614      *---------------------------------------------------------------*
074900060614      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
074910060614      *?Dati di dettaglio: corpo bolla non fincato                   ?*
074920060614      *---------------------------------------------------------------*
074930060614     c     PrtDdtLOG1_C4 BEGSR
074940060614      *
074950060614     c                   clear                   A4prtDL21
074960060614     c                   clear                   dAR8LOG1C4
074970060614     c                   movel     AR8uni        dAR8LOG1C4
074980060614      *
074990060614      * > dati DDT
075000060614      * - - riga descrittiva
075010060614     c                   eval      PLOG1dpr1 = §A8LOG1C4a
075020060614      *
075030060614      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
075040060614      *
075050060614     c                   exsr      Test_OF
075060060614     c                   write     A4prtDL21
075070060614      *
075080060614     c                   ENDSR
075090090323      *
075100090323      *---------------------------------------------------------------*
075110090323      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
075120090323      *?Dati di dettaglio: descrizioni                               ?*
075130090323      *---------------------------------------------------------------*
075140090323     c     PrtDdtLOG1_C5 BEGSR
075150090323      *
075160090323     c                   clear                   A4prtDL31
075170090323     c                   clear                   dAR8LOG1S3
075180090323     c                   movel     AR8uni        dAR8LOG1S3
075190090323      *
075200090323      * > dati in note DDT
075210090323      * - - riga descrittiva
075220090323     c                   eval      PLOG1txt2 = §A8LOG1S3a
075230090323      *
075240090323      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
075250090323      *
075260090323     c                   exsr      Test_OF
075270090323     c                   write     A4prtDL31
075280090323      *
075290090323     c                   ENDSR
075300041019      *
075310041019      *---------------------------------------------------------------*
075320050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
075330050217      *?Dati di fondo: beni, colli, peso, volume, c/a, porto, trasp. ?*
075340041019      *---------------------------------------------------------------*
075350050217     c     PrtDdtLOG1_P1 BEGSR
075360041019      *
075370051110     c                   clear                   dAR8LOG1P1
075380050707     c                   movel     AR8uni        dAR8LOG1P1
075390050706      *
075400050706      * TEST effettiva numericità dei campi previsti numerici:
075410100513     c                   clear                   w053a
075420050706     c                   eval      WX    =  %len(§A8LOG1P1b)
075430050706     c                                   +  %len(§A8LOG1P1c)
075440050706     c                                   +  %len(§A8LOG1P1d)
075450100513     c                   eval      w053a =  %subst(dAR8LOG1P1:42:WX)
075460050706     c                   eval      *in31 =  *off
075470110426     c     c_Digits      checkr    w053a :  WX                            31
075480050706if  1c                   if        *in31
075490050706     c                   clear                   §A8LOG1P1b
075500050706     c                   clear                   §A8LOG1P1c
075510050706     c                   clear                   §A8LOG1P1d
075520050706e   1c                   endif
075530100513     c                   clear                   w053a
075540050706     c                   eval      WX    =  %len(§A8LOG1P1f)
075550100513     c                   eval      w053a =  %subst(dAR8LOG1P1:67:WX)
075560050706     c                   eval      *in31 =  *off
075570110426     c     c_Digits      checkr    w053a :  WX                            31
075580050706if  1c                   if        *in31
075590050706     c                   clear                   §A8LOG1P1f
075600050706e   1c                   endif
075610041019      *
075620041019      * > dati in chiusura DDT
075630041019      * - - aspetto esteriore dei beni
075640050707     c                   eval      PLOG1aeb  = §A8LOG1P1a
075650041019      * - - numero colli
075660050707     c                   eval      PLOG1ncl  = §A8LOG1P1b
075670041019      * - - peso in kg
075680050707     c                   eval      PLOG1pkg  = §A8LOG1P1c
075690041019      * - - volume in mc
075700050707     c                   eval      PLOG1vlm  = §A8LOG1P1d
075710041019      * - - c/assegno
075720050707     c                   eval      PLOG1cva  = §A8LOG1P1e
075730050707     c                   eval      PLOG1cas  = §A8LOG1P1f
075740041020      * - - causale trasporto
075750050707     c                   eval      PLOG1ctr  = §A8LOG1P1g
075760041020      * - - porto
075770050707     c                   eval      PLOG1pto  = §A8LOG1P1h
075780041020      * - - trasporto a cura
075790050707     c                   eval      PLOG1tra  = §A8LOG1P1i
075800041020      * - - trasportatore
075810050707     c                   eval      PLOG1vet  = §A8LOG1P1j
075820050505      *
075830050505      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
075840050505      *
075850050505      * - - Verifica OverFlow
075860080925      /free
075870080925           select;
075880080925             when  NOT *in45;
075890080925               wMaxLine = $MaxLine4;
075900080925             when  *in45   and   NOT *in46;
075910080925               wMaxLine = $MaxLine4_pl1;
075920080925             when  *in45   and   *in46;
075930080925               wMaxLine = $MaxLine4_pl2;
075940080925           endsl;
075950080925      /end-free
075960050505     c                   exsr      Test_OF
075970050505      * - - Calcolo della posizione finale di box e linee separatrici
075980050505     c                   exsr      Posiz_DL
075990050505      * - - Stampa parte finale DDT
076000050505     c                   write     A4prtDL4
076010050505      *
076020050505      * > Stampa box, linee e testi fissi (no logo)  - - - - - - - - -*
076030090331     c                   if            §AR8cxd = *blanks
076040090331     c                             or  DB0fa4  = 'P'
076050090331     c                   write     A4prtDT0
076060090331     c                   endif
076070050505     c                   write     A4prtDL0A
076080050505     c                   write     A4prtDL0B
076090050505      *
076100050505      * Azzero il numero di pagina per l'eventuale Packing-List
076110050505      *   (successiva)
076120050505     c                   clear                   Page
076130041019      *
076140041019      * N.B.: la copia per il destinatario della LDV deve comparire
076150041019      *       nell'ultima pagina del DDT per il cliente (vedi subr.
076160041019      *       "ChiusuraLDV")
076170041019      *
076180041019     c                   ENDSR
076190050318      *
076200050318      *---------------------------------------------------------------*
076210050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
076220050318      *?Dati di packing-list: dettaglio                              ?*
076230050318      *---------------------------------------------------------------*
076240050318     c     PrtDdtLOG1_Z1 BEGSR
076250050318      *
076260050318      * Prima della 1ª riga di packing-list: testata packing-list
076270050505if  1c                   if        Page     = *zeros
076280050318     c                   exsr      PrtPckLst_1
076290050318e   1c                   endif
076300050318      *
076310050318     c                   clear                   A4prtDL5
076320051110     c                   clear                   dAR8LOG1Z1
076330050707     c                   movel     AR8uni        dAR8LOG1Z1
076340050706      *
076350050706      * TEST effettiva numericità dei campi previsti numerici:
076360100513     c                   clear                   w053a
076370050706     c                   eval      WX    =  %len(§A8LOG1Z1d)
076380100513     c                   eval      w053a =  %subst(dAR8LOG1Z1:53:WX)
076390050706     c                   eval      *in31 =  *off
076400110426     c     c_Digits      checkr    w053a :  WX                            31
076410050706if  1c                   if        *in31
076420050706     c                   clear                   §A8LOG1Z1d
076430050706e   1c                   endif
076440050318      *
076450050318      * > dati in dettaglio Packing-List
076460050318      * - - sscc
076470050318     c                   eval      PCsscc   = §A8LOG1Z1a
076480050318      * - - gtin
076490050318     c                   eval      PCgtin   = §A8LOG1Z1b
076500050318      * - - codice prodotto
076510050318     c                   eval      PCcpr2   = §A8LOG1Z1c
076520050318      * - - quantità ul
076530050318     c                   eval      PLOG1QTA = §A8LOG1Z1d
076540050318      * - - tipo contenitore
076550050318     c                   eval      PCtcn    = §A8LOG1Z1e
076560050318      * - - codice contenitore
076570050318     c                   eval      PCccn    = §A8LOG1Z1f
076580050318      * - - numero lotto
076590050318     c                   eval      PClot    = §A8LOG1Z1g
076600050318      * - - data scadenza
076610050318     c                   eval      PCdts    = §A8LOG1Z1h
076620050318      *
076630050318      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
076640050318      *
076650050318     c                   exsr      Test_OF
076660050318     c                   write     A4prtDL5
076670050318      *
076680050318     c                   ENDSR
076690080908      *
076700050606      *---------------------------------------------------------------*
076710050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
076720080925      *?Dati di Fattura: Testata                                     ?*
076730050606      *---------------------------------------------------------------*
076740050606     c     PrtDdtLOG1_S1 BEGSR
076750050606      *
076760050606      * > logo Fattura
076770050606     c                   eval      §LogoF   = 'LOGO' + §AR8logof
076780090107      /free
076790090107         // - Impostazione indicatore che condiziona la stampa del logo
076800120530         //   (o quella del "simil-logo" BRT)
076810090107         *in55 = (%subst(§AR8logof : 1 : 3) = 'LOG'       and
076820090107                  (§AR8tsd  = 'L1'  or  §AR8tsd  = 'L3'));
076830090107      /end-free
076840080908      *
076850060609      *? L O G O !! ?
076860060609      *?vedi stampa testata bolla ?
076870071008     c                   if        (§AR8tsd = 'L1' or §AR8tsd = 'L3') and
076880071008     c                             §AR8logof <> *blanks and
076890071008     c                             %subst(AR8tmd:1:3) <> 'LOG'
076900071008     c                   eval      §LogoF   = 'LOGO' + AR8tmd
076910090107      /free
076920090107           // - Impostaz. indicatore che condiziona la stampa del logo
076930120530           //   (o quella del "simil-logo" BRT)
076940090107           *in55 = (%subst(AR8tmd : 1 : 3) = 'LOG');
076950090107      /end-free
076960060609     c                   endif
076970050606      *
076980051110     c                   clear                   dAR8LOG1S1
076990050606     c                   movel     AR8uni        dAR8LOG1S1
077000050706      *
077010050706      * TEST effettiva numericità dei campi previsti numerici:
077020100513     c                   clear                   w053a
077030050706     c                   eval      WX    =  %len(§A8LOG1S1i)
077040100513     c                   eval      w053a =  %subst(dAR8LOG1S1:216:WX)
077050050706     c                   eval      *in31 =  *off
077060110426     c     c_Digits      checkr    w053a :  WX                            31
077070050706if  1c                   if        *in31
077080050706     c                   clear                   §A8LOG1S1i
077090050706e   1c                   endif
077100050707      *
077110050606      * > intestazione fattura
077120050707if  1c                   if        §A8LOG1S1b =  *blanks
077130050707     c                   clear                   PLOG1rsf
077140050707     c                   eval      PLOG1rsf2  =  §A8LOG1S1a
077150050606x   1c                   else
077160050707     c                   eval      PLOG1rsf   =  §A8LOG1S1a
077170050707     c                   eval      PLOG1rsf2  =  §A8LOG1S1b
077180050606e   1c                   endif
077190050707     c                   eval      PLOG1inf   =  §A8LOG1S1c
077200050707     c                   eval      PLOG1inf2  =  §A8LOG1S1d
077210050707if  1c                   if        §A8LOG1S1e >  *zeros
077220050707     c                   eval      PLOG1caf   =  §A8LOG1S1e
077230050606e   1c                   endif
077240050707     c                   eval      PLOG1lof   =  §A8LOG1S1f
077250050606      * > partita IVA - codice fiscale
077260050707     c                   eval      PLOG1IVA   =  §A8LOG1S1g
077270050606      * > n./data fattura
077280050707     c                   eval      PLOG1nft   =  §A8LOG1S1h
077290050606      *     giro la data fattura
077300050706if  1c                   if        §A8LOG1S1i >  *zeros
077310060922     c                   eval      *in31 =  *off
077320060922     c     *iso          test(d)                 §A8LOG1S1i             31
077330060922if  2c                   if        NOT *in31
077340050707     c     *iso          movel     §A8LOG1S1i    Data_EUR
077350050707     c     *eur          movel     Data_EUR      PLOG1dft
077360060922x   2c                   else
077370060922     c                   clear                   PLOG1dft
077380060922e   2c                   endif
077390050706e   1c                   endif
077400050606      * > rif.ns.spedizione
077410060922     c                   eval      PLOG1nspa =   §A8LOG1S1j
077420050606      * > intestazione corpo fattura
077430050707     c                   eval      PLOG1tesf  =  §A8LOG1S1k
077440050707      *
077450050606      * > Stampa testata FATTURA - - - - - - - - - - - - - - - - - - -*
077460081015      *
077470080925      /free
077480081015         // - Forzatura del salto pagina
077490081015         CurLineA4 = wMaxLine + 1;
077500080925      /end-free
077510080925     c                   exsr      Test_OF
077520081015      /free
077530090323         // - Impostazione dei nuovi limiti di pagina
077540090323         select;
077550090323           when  NOT *in45;
077560090323             wMaxLine = $MaxLine2;
077570090323           when  *in45   and   NOT *in46;
077580090323             wMaxLine = $MaxLine2_pl1;
077590090323           when  *in45   and   *in46;
077600090323             wMaxLine = $MaxLine2_pl2;
077610090323         endsl;
077620081015      /end-free
077630080925     c                   exsr      Posiz_DL
077640081017     c                   if            §AR8cxd = *blanks
077650081017     c                             or  DB0fa4  = 'P'
077660080925     c                   write     A4prtDT0
077670080925     c                   endif
077680080925     c                   write     A4prtDL0D
077690050606     c                   write     A4prtDL1D
077700050606     c                   write     A4prtDL1E
077710050707      *
077720050606     c                   ENDSR
077730050606      *
077740050606      *---------------------------------------------------------------*
077750050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
077760050606      *?Dati di dettaglio corpo: FATTURA                             ?*
077770050606      *---------------------------------------------------------------*
077780050606     c     PrtDdtLOG1_S2 BEGSR
077790050606      *
077800050606     c                   clear                   A4prtDL6
077810051110     c                   clear                   dAR8LOG1S2
077820050606     c                   movel     AR8uni        dAR8LOG1S2
077830050606      *
077840050606      * > dati fattura
077850050606      * - - riga descrittiva 1
077860050707     c                   eval      PLOG1dft1 = §A8LOG1S2a
077870050606      *
077880050606      * > Stampa dettaglio FATTURA - - - - - - - - - - - - - - - - - -*
077890050606      *
077900050606     c                   exsr      Test_OF
077910050606     c                   write     A4prtDL6
077920050606      *
077930050606     c                   ENDSR
077940050606      *
077950050606      *---------------------------------------------------------------*
077960050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
077970050606      *?Dati di dettaglio corpo: FATTURA                             ?*
077980050606      *---------------------------------------------------------------*
077990050606     c     PrtDdtLOG1_S3 BEGSR
078000050606      *
078010050606     c                   clear                   A4prtDL7
078020051110     c                   clear                   dAR8LOG1S3
078030050707     c                   movel     AR8uni        dAR8LOG1S3
078040050606      *
078050050606      * > dati fattura
078060050606      * - - riga descrittiva 2
078070050707     c                   eval      PLOG1dft2 = §A8LOG1S3a
078080050606      *
078090050606      * > Stampa dettaglio FATTURA - - - - - - - - - - - - - - - - - -*
078100050606      *
078110050606     c                   exsr      Test_OF
078120050606     c                   write     A4prtDL7
078130050606      *
078140050606     c                   ENDSR
078150050606      *
078160050606      *---------------------------------------------------------------*
078170050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
078180050606      *?Dati di fondo: FATTURA                                       ?*
078190050606      *---------------------------------------------------------------*
078200050606     c     PrtDdtLOG1_S4 BEGSR
078210050606      *
078220051110     c                   clear                   dAR8LOG1S4
078230050707     c                   movel     AR8uni        dAR8LOG1S4
078240050706      *
078250050706      * TEST effettiva numericità dei campi previsti numerici:
078260100513     c                   clear                   w053a
078270050706     c                   eval      WX    =  %len(§A8LOG1S4a)
078280050706     c                                   +  %len(§A8LOG1S4b)
078290050706     c                                   +  %len(§A8LOG1S4c)
078300100513     c                   eval      w053a =  %subst(dAR8LOG1S4: 1:WX)
078310050706     c                   eval      *in31 =  *off
078320110426     c     c_Digits      checkr    w053a :  WX                            31
078330050706if  1c                   if        *in31
078340050706     c                   clear                   §A8LOG1S4a
078350050706     c                   clear                   §A8LOG1S4b
078360050706     c                   clear                   §A8LOG1S4c
078370050706e   1c                   endif
078380050606      *
078390050606      * > dati in chiusura FATTURA
078400050606      * - - dati di piede fattura riga 1
078410050707     c                   eval      PLOG1pft1 = §A8LOG1S4d
078420050606      * - - dati di piede fattura riga 2
078430050707     c                   eval      PLOG1pft2 = §A8LOG1S4e
078440050606      * - - dati di piede fattura riga 3
078450050707     c                   eval      PLOG1pft3 = §A8LOG1S4f
078460050606      * - - totale imponibile
078470050707     c                   eval      PLOG1timp = §A8LOG1S4a
078480050606      * - - totale iva
078490050707     c                   eval      PLOG1tiva = §A8LOG1S4b
078500050606      * - - totale fattura
078510050707     c                   eval      PLOG1tft  = §A8LOG1S4c
078520050606      *
078530050606      * > Stampa fondo FATTURA - - - - - - - - - - - - - - - - - - - -*
078540050606      *
078550050606      * - - Verifica OverFlow
078560080925      /free
078570080925           select;
078580080925             when  NOT *in45;
078590080925               wMaxLine = $MaxLine4;
078600080925             when  *in45   and   NOT *in46;
078610080925               wMaxLine = $MaxLine4_pl1;
078620080925             when  *in45   and   *in46;
078630080925               wMaxLine = $MaxLine4_pl2;
078640080925           endsl;
078650080925      /end-free
078660050606     c                   exsr      Test_OF
078670050606      * - - Calcolo della posizione finale di box e linee separatrici
078680050606     c                   exsr      Posiz_DL
078690050606      * - - Stampa parte finale FATTURA
078700050606     c                   write     A4prtDL8
078710050606      *
078720050606      * > Stampa box, linee (no logo)   - - - - - - - - - - - - - - -*
078730050606     c                   write     A4prtDL0E
078740050606     c                   write     A4prtDL0F
078750080908      *
078760050606     c                   ENDSR
078770050606      *
078780050606      *---------------------------------------------------------------*
078790050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
078800050606      *?Dati di fondo note: FATTURA                                  ?*
078810050606      *---------------------------------------------------------------*
078820050606     c     PrtDdtLOG1_S5 BEGSR
078830050606      *
078840051110     c                   clear                   dAR8LOG1S5
078850050707     c                   movel     AR8uni        dAR8LOG1S5
078860050606      *
078870050606      * > dati in chiusura FATTURA
078880050606      * - - nota 1
078890050707     c                   eval      PLOG1not1 = §A8LOG1S5a
078900050606      * - - nota 2
078910050707     c                   eval      PLOG1not2 = §A8LOG1S5b
078920050606      * - - nota 3
078930050707     c                   eval      PLOG1not3 = §A8LOG1S5c
078940050606      *
078950050606      * > Stampa fondo FATTURA - - - - - - - - - - - - - - - - - - - -*
078960050606      * - - Stampa parte finale FATTURA
078970050606     c                   write     A4prtDL9
078980080908      *
078990050606     c                   ENDSR
079000050321      *
079010050321      *---------------------------------------------------------------*
079020050414      *?Impostazione nuova pagina di DDT Logistica / Sait            ?*
079030050321      *---------------------------------------------------------------*
079040050321     c     NewPageDL     BEGSR
079050050321      *
079060080925      /free
079070080925         // Impostazione nuovo limite di OF
079080080925         SELECT;
079090080925           // Piede DDT   oppure   Piede Fattura
079100080925           WHEN  AR8trk = 'P1'   or   AR8trk = 'S4';
079110080925             select;
079120080925               when  NOT *in45;
079130080925                 wMaxLine = $MaxLine4;
079140080925               when  *in45   and   NOT *in46;
079150080925                 wMaxLine = $MaxLine4_pl1;
079160080925               when  *in45   and   *in46;
079170080925                 wMaxLine = $MaxLine4_pl2;
079180080925             endsl;
079190080925           // Dettaglio DDT  oppure   Testata & Dettaglio Fattura
079200080925           OTHER;
079210080925             select;
079220080925               when  NOT *in45;
079230090323                 wMaxLine = $MaxLine2;
079240080925               when  *in45   and   NOT *in46;
079250090323                 wMaxLine = $MaxLine2_pl1;
079260080925               when  *in45   and   *in46;
079270090323                 wMaxLine = $MaxLine2_pl2;
079280080925             endsl;
079290080925         ENDSL;
079300080925      /end-free
079310080925      *
079320080825      * RE-impostazione posizioni
079330080825     c                   exsr      Posiz_DL
079340080825      *
079350050321     c                   write     A4prtSP1
079360081006      /free
079370081006         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
079380081006           write  A4prtNUL;
079390081006         endif;
079400081006      /end-free
079410050321      *
079420050505sel 1c                   select
079430050505      * - altra pagina DDT Logistica / Sait
079440050707w   1c                   when          %subst(AR8trk:1:1) <> 'Z'
079450050606     c                             and %subst(AR8trk:1:1) <> 'S'
079460050321     c                   write     A4prtDL1B
079470050505      * - altra pagina Packing-List
079480050505w   1c                   when      %subst(AR8trk:1:1) =  'Z'
079490050502     c                              and   Page        >  *zeros
079500050321     c                   write     A4prtDL1C
079510050606      * - altra pagina Fattura
079520050606w   1c                   when      %subst(AR8trk:1:1) =  'S'
079530050606     c                              and   Page        >  *zeros
079540050606     c                   write     A4prtDL1E
079550050505e   1c                   endsl
079560050321      *
079570050321     c                   ENDSR
079580050318      *
079590050318      *---------------------------------------------------------------*
079600080709      *?Stampa testata della Packing-List per LOGISTICA              ?*
079610050318      *---------------------------------------------------------------*
079620050318     c     PrtPckLst_1   BEGSR
079630050318      *
079640050318      * > Salto a pagina successiva  - - - - - - - - - - - - - - - - -*
079650050318     c                   exsr      Test_OF
079660050318      *
079670050505      * > Stampa box, linee e testi fissi (CON logo)  - - - - - - - - -*
079680050318      *
079690050318      * - - Stampa "Copia per il destinatario" nella pagina successiva
079700081017     c*** poi in Test_OF:   if            §AR8cxd = *blanks
079710081017     c*** poi in Test_OF:             or  DB0fa4  = 'P'
079720050505     c*** poi in Test_OF:   write     A4prtDT0
079730050505     c*** poi in Test_OF:   endif
079740050318      * - - Stampa dati destinatario nel box superiore
079750050318     c                   write     A4prtDL1A
079760050318      * - - Stampa logo e box superiore x destinatario
079770050318     c                   write     A4prtDL0
079780081222      /free
079790081222         if  *in55;
079800090406           exsr   PrtDdtLOG1_SimilLogo;
079810120530           //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
079820120530           //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
079830120530           //  write  A4LogoBR2;
079840120530           //else;
079850120530           //  write  A4LogoBAR;
079860120530           //endif;
079870120530           write  A4LogoBRT;
079880081222           write  A4prtDLL;
079890081222         endif;
079900081222      /end-free
079910050318      * - - Stampa dati di testata nel box generale
079920050318     c                   write     A4prtDL1C
079930050318      * - - Stampa box generale
079940050318     c*** poi in Test_OF:   write     A4prtDL0C
079950050318      *
079960050318      * Impostazione del nuovo limite x overflow in packing-list
079970080925      /free
079980080925         select;
079990080925           when  NOT *in45;
080000090323             wMaxLine = $MaxLine2;
080010080925           when  *in45   and   NOT *in46;
080020090323             wMaxLine = $MaxLine2_pl1;
080030080925           when  *in45   and   *in46;
080040090323             wMaxLine = $MaxLine2_pl2;
080050080925         endsl;
080060080925      /end-free
080070050318      *
080080050318     c                   ENDSR
080090041019      *
080100041020      *---------------------------------------------------------------*
080110050414      *?Posizionamento di linee e box nel DDT LOGISTICA / SAIT       ?*
080120041020      *---------------------------------------------------------------*
080130041021     c     Posiz_DL      BEGSR
080140050218      *
080150080924      /free
080160080924         // Impostazione posizioni di logo/box
080170080924         // (posizioni di "partenza" diverse a seconda del posizionam.
080180080924         //  iniziale nel foglio A4 attualmente in stampa)
080190080924         select;
080200080924           when  NOT *in45;
080210081006             §PBA =  3.255;
080220080924           when  *in45   and   NOT *in46;
080230090422             §PBA = 0.255;
080240080924           when  *in45   and   *in46;
080250090422             §PBA = 6.005;
080260080924         endsl;
080270080924      /end-free
080280080708     c                   eval      §PBB  =  §PBA + 1.27
080290080708     c                   eval      §LOA  =  §PBA + 0.63
080300080708     c                   eval      §PS0  =  §PBA + 0.72
080310080708     c                   eval      §PS1  =  §PS0 + 0.10
080320080708     c                   eval      §PS2  =  §PS1 + 0.10
080330080708     c                   eval      §PS3  =  §PS2 + 0.10
080340080708     c                   eval      §PS4  =  §PS3 + 0.10
080350080708     c                   eval      §PS5  =  §PS4 + 0.10
080360080708      * Impostazione posizioni di logo/box per fattura
080370080708     c                   eval      §PBC  =  §PBA + 0.92
080380080708     c                   eval      §LOB  =  §PBA + 0.72
080390080708      * Impostazione posizioni di note fine fattura
080400080925     c                   eval      §PS6  =  §PBA + 5.35
080410081003     c                   eval      §PS7  =  §PS6 + 0.08
080420081003     c                   eval      §PS8  =  §PS7 + 0.08
080430080708      *
080440050321if  1c                   if        Page  <= 1
080450050218      *   (posizione di "partenza" su 1ª pagina)
080460080924     c                   eval      §PB1  =  §PBA + 1.375
080470050218x   1c                   else
080480050318      *   (posizione di "partenza" su pagine successive)
080490080822     c                   eval      §PB1  =  §PBA
080500050218e   1c                   endif
080510080924     c                   eval      §PB2  =  §PBA + 5.275
080520050218      * Impostazione posizioni di linee (orizzontali e verticali)
080530050218      *   nella parte superiore del box (testata)
080540050218     c                   eval      §LO1  =  §PB1 + 0.13
080550050318if  1c                   if        %subst(AR8trk:1:1) <> 'Z'
080560050218     c                   eval      §LO2  =  §LO1 + 0.13
080570050318     c                   eval      §LO3  =  §LO2 + 0.11
080580050318x   1c                   else
080590080825     c                   eval      §LO2  =  §LO1 + 0.115
080600050318     c                   eval      §LO3  =  §LO2
080610050318e   1c                   endif
080620050607if  1c                   if        %subst(AR8trk:1:1) = 'S'
080630050610     c                   eval      §LO3  =  §PB2 - 0.40
080640050607     c                   EndIf
080650050218      * Impostazione posizioni di linee (orizzontali e verticali)
080660050218      *   nella parte inferiore del box (piede)
080670050318if  1c                   if        %subst(AR8trk:1:1) <> 'Z'
080680080822     c                   eval      §LO4  =  §PB2 - 0.52
080690090422     c                   eval      §LO5  =  §LO4 + 0.13
080700090422     c                   eval      §LO6  =  §LO5 + 0.13
080710090422     c                   eval      §LO7  =  §LO6 + 0.13
080720050318e   1c                   endif
080730050218      * Impostazione lunghezza delle linee verticali
080740050218      *   nella parte superiore del box (testata)
080750050218     c                   eval      §LV1  =  §LO1 - §PB1
080760050218     c                   eval      §LV2  =  §LO2 - §LO1
080770050318if  1c                   if        %subst(AR8trk:1:1) <> 'Z'
080780050218     c                   eval      §LV3  =  §LO3 - §LO2
080790050318e   1c                   endif
080800050607if  1c                   if        %subst(AR8trk:1:1) = 'S'
080810050607     c                   eval      §LV2  =  0.40
080820050607     c                   EndIf
080830050218      * La lunghezza delle linee verticali separatrici nel dettaglio
080840050218      *   dipende dalla fase di stampa: se si sta stampando ancora il
080850050218      *   dettaglio deve essere più lunga, visto che non viene stampa-
080860050218      *   ta la parte finale del DDT...
080870080924if  1c                   if        %subst(AR8trk:1:1) = 'P'
080880080924     c                              and  (§PS1        < $LimOF
080890080924     c                                and NOT *in45
080900080924     c                                OR  §PS1        < ($LimOF - c_Len1_inch)
080910080924     c                                and *in45   and   NOT *in46
080920081003     c                                OR  §PS1        < ($LimOF + c_Len2_inch)
080930080924     c                                and *in45   and   *in46)
080940050318     c                              and   CurLineA4   <  wMaxLine
080950050218     c                   eval      §LV4  =  §LO4 - §LO3
080960050218x   1c                   else
080970050218     c                   eval      §LV4  =  §PB2 - §LO3
080980050218e   1c                   endif
080990080908      *
081000071008     c                   if        §AR8tsd = 'L3' or §AR8tsd = 'L4' and
081010060609     c                             %subst(AR8trk:1:1) <> 'Z' and
081020060609     c                             %subst(AR8trk:1:1) <> 'S'
081030071008     c                   eval      §LV3 = 0
081040071008     c                   eval      §LV4 = 0
081050060609     c                   endif
081060080908      *
081070050502      * - qui imposto le posizioni per il piede del box
081080050318if  1c                   if        %subst(AR8trk:1:1) <> 'Z'
081090050218     c                   eval      §LV5  =  §LO5 - §LO4
081100050218     c                   eval      §LV6  =  §LO6 - §LO5
081110050218     c                   eval      §LV7  =  §PB2 - §LO7
081120050318e   1c                   endif
081130041028      * Impostazione posizioni dei campi di testo verticali
081140080822     c                   eval      §PR1  =  §PB2 - 0.05
081150041020      *
081160041020     c                   ENDSR
081170050113      *
081180050113      *---------------------------------------------------------------*
081190050217      *?Stampa DDT del mittente ARCTE                                ?*
081200050113      *---------------------------------------------------------------*
081210050217     c     DDT_Arcte     BEGSR
081220050113      *
081230081002      /free
081240081002         // Impostazione del nuovo limite x overflow
081250081002         // (Codesto limite è valido solo per la stampa del dettaglio.
081260081002         //   Per la stampa del "fondo" bolla occorre REimpostarlo ad
081270081002         //   un valore più basso.
081280081002         //  Ovvero: stampo più righe di dettaglio che posso; se alla
081290081002         //  fine delle righe di dettaglio non rimane spazio a suffi-
081300081002         //  cienza per il fondo: prima della stampa del fondo si salta
081310081002         //  alla pagina successiva, dove non si stampa alcuna riga di
081320081002         //  dettaglio.)
081330081002         select;
081340081002           when  NOT *in45;
081350081002             wMaxLine = $MaxLine2;
081360081002           when  *in45   and   NOT *in46;
081370090324             wMaxLine = $MaxLine5_pl1;
081380081002           when  *in45   and   *in46;
081390090324             wMaxLine = $MaxLine5_pl2;
081400081002         endsl;
081410081002
081420081002         // Posizionamento iniziale all'interno del foglio A4
081430081002         write  A4prtSP1;
081440081002      /end-free
081450050113      *
081460050113      * Impostazione testi fissi - - - - - - - - - - - - - - - - - - -*
081470050113      *
081480050113     c                   eval      PCLtx1 =
081490050113     c                             'La merce viaggia a rischio e pericolo -
081500050113     c                              del committente. I reclami vanno fatti -
081510050113     c                              per iscritto entro 8 gg. dal ricevimento -
081520050113     c                              delle merce.'
081530050113     c                   eval      PCLtx2 =
081540050113     c                              'Il ritorno di merce deve -
081550050113     c                              essere preventivamente autorizzato -
081560050113     c                              dal Gruppo Arcte. Foro competente -
081570050113     c                              come da condizioni di vendita.'
081580050113      *
081590050113     c                   eval      PCLpx1 =
081600050113     c                             'CONTRIBUTO CONAI ASSOLTO'
081610050217      *
081620130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
081630130315      *  "StampaDDT")
081640050217     c***  KLDV          setll     FIAR8000       (già fatto)
081650050217     c***  KLDV          reade     FIAR8000       (già fatto)
081660050113      *
081670050113      * Reperimento dati, impostazione e stampa DDT mittente  - - - -*
081680050113      *
081690050113if  1c                   dow       NOT %eof(FIAR801L)
081700050113      *
081710050113sel 2c                   select
081720050113      * - Record annullato (non si stampa nulla)
081730050217w   2c                   when      AR8atb  <> *blanks
081740050707      * - Dati di dettaglio: descrizioni generiche
081750050217w   2c                   when      AR8trk   = 'C1'
081760050217     c                   exsr      PrtDdtARCT_C1
081770050217      * - Dati di dettaglio: articolo, descrizioni, taglie, q.tà
081780050217w   2c                   when      AR8trk   = 'C2'
081790050217     c                   exsr      PrtDdtARCT_C2
081800050113      * - Dati di testata: destinatario, cliente di fatturazione
081810050217w   2c                   when      AR8trk   = 'T1'
081820050217     c                   exsr      PrtDdtARCT_T1
081830050113      * - Dati di testata: agente, numero ordine, estremi documento
081840050217w   2c                   when      AR8trk   = 'T2'
081850050217     c                   exsr      PrtDdtARCT_T2
081860050113      * - Dati di fondo: mezzo e causale trasporto, colli, peso
081870050217w   2c                   when      AR8trk   = 'P1'
081880050217     c                   exsr      PrtDdtARCT_P1
081890050113e   2c                   endsl
081900050113      *
081910050113     c     KLDV          reade     FIAR8000
081920050113      *
081930050113e   1c                   enddo
081940050217      *
081950050217      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
081960050217      *
081970050217      * - - Verifica OverFlow
081980050217     c                   exsr      Test_OF
081990050218      *
082000050218      * - - Calcolo la posizione finale del box e di linee separatrici
082010050218     c                   exsr      Posiz_AC
082020050217      *
082030050217      * - - Stampa parte finale DDT
082040050217     c                   write     A4prtAC4
082050050217      *
082060050217      * > Stampa box, linee e testi fissi (no logo)  - - - - - - - - -*
082070081017     c                   if            §AR8cxd = *blanks
082080081017     c                             or  DB0fa4  = 'P'
082090050331     c                   write     A4prtDT0
082100050331     c                   endif
082110050217     c                   write     A4prtAC0A
082120050217     c                   write     A4prtAC0B
082130050217      *
082140050217      * Reimpostazione del vecchio limite x overflow
082150081002      /free
082160081002         select;
082170081002           when  NOT *in45;
082180081002             wMaxLine = $MaxLine2;
082190081002           when  *in45   and   NOT *in46;
082200081002             wMaxLine = $MaxLine2_pl1;
082210081002           when  *in45   and   *in46;
082220081002             wMaxLine = $MaxLine2_pl2;
082230081002         endsl;
082240081002      /end-free
082250050113      *
082260050113     c                   ENDSR
082270050214      *
082280050113      *---------------------------------------------------------------*
082290050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
082300050217      *?Dati di testata: destinatario, cliente di fatturazione       ?*
082310050113      *---------------------------------------------------------------*
082320050217     c     PrtDdtARCT_T1 BEGSR
082330050114      *
082340050114      * > logo
082350050114     c                   eval      §Logo  = 'LOGO' + AR8tmd
082360050113      *
082370051110     c                   clear                   dAR8ARCTT1
082380050707     c                   movel     AR8uni        dAR8ARCTT1
082390050706      *
082400050706      * TEST effettiva numericità dei campi previsti numerici:
082410100513     c                   clear                   w053a
082420050706     c                   eval      WX    =  %len(§ARCTT1a02)
082430100513     c                   eval      w053a =  %subst(dAR8ARCTT1:  7:WX)
082440050706     c                   eval      *in31 =  *off
082450110426     c     c_Digits      checkr    w053a :  WX                            31
082460050706if  1c                   if        *in31
082470050706     c                   clear                   §ARCTT1a02
082480050706e   1c                   endif
082490050706     c                   eval      WX    =  %len(§ARCTT1b02)
082500100513     c                   eval      w053a =  %subst(dAR8ARCTT1:163:WX)
082510050706     c                   eval      *in31 =  *off
082520110426     c     c_Digits      checkr    w053a :  WX                            31
082530050706if  1c                   if        *in31
082540050706     c                   clear                   §ARCTT1b02
082550050706e   1c                   endif
082560050113      *
082570050113      * > destinatario
082580050707     c                   eval      PARCTccd  = §ARCTT1a01
082590050707     c                   eval      PARCTccd1 = §ARCTT1a02
082600050707     c                   eval      PARCTrsd  = §ARCTT1a04
082610050707     c                   eval      PARCTrsd2 = §ARCTT1a05
082620050707     c                   eval      PARCTind  = §ARCTT1a06
082630050707     c                   eval      PARCTcad  = §ARCTT1a07
082640050707     c                   eval      PARCTlod  = §ARCTT1a08
082650050113      * > cliente di fatturazione
082660050707     c                   eval      PARCTccf  = §ARCTT1b01
082670050707     c                   eval      PARCTccf1 = §ARCTT1b02
082680050707     c                   eval      PARCTccf2 = §ARCTT1b03
082690050707     c                   eval      PARCTrsf  = §ARCTT1b04
082700050707     c                   eval      PARCTrsf2 = §ARCTT1b05
082710050707     c                   eval      PARCTinf  = §ARCTT1b06
082720050707     c                   eval      PARCTcaf  = §ARCTT1b07
082730050707     c                   eval      PARCTlof  = §ARCTT1b08
082740050113      * > magazzino spedizione
082750050707     c                   eval      PARCTmag  = §ARCTT1c01
082760050113      *
082770050113     c                   ENDSR
082780050113      *
082790050113      *---------------------------------------------------------------*
082800050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
082810050217      *?Dati di testata: documento                                   ?*
082820050113      *---------------------------------------------------------------*
082830050217     c     PrtDdtARCT_T2 BEGSR
082840050113      *
082850051110     c                   clear                   dAR8ARCTT2
082860050707     c                   movel     AR8uni        dAR8ARCTT2
082870050706      *
082880050706      * TEST effettiva numericità dei campi previsti numerici:
082890100513     c                   clear                   w053a
082900050706     c                   eval      WX    =  %len(§ARCTT2a02)
082910050706     c                                   +  %len(§ARCTT2a03)
082920100513     c                   eval      w053a =  %subst(dAR8ARCTT2: 9:WX)
082930050706     c                   eval      *in31 =  *off
082940110426     c     c_Digits      checkr    w053a :  WX                            31
082950050706if  1c                   if        *in31
082960050706     c                   clear                   §ARCTT2a02
082970050706     c                   clear                   §ARCTT2a03
082980050706e   1c                   endif
082990050113      *
083000050113      * > dati in testata DDT
083010050113      * - - numero e data documento (DDT)
083020050707     c                   eval      PARCTnsr = §ARCTT2a01
083030050707     c                   eval      PARCTnsp = §ARCTT2a02
083040050707     c                   eval      PARCTdsp = §ARCTT2a03
083050050113      *
083060050113      * > Stampa testata DDT - - - - - - - - - - - - - - - - - - - - -*
083070050113      *
083080050113      *   - OverFlow impossibile qui! -
083090050217     c***                exsr      Test_OF
083100050217     c                   exsr      Posiz_AC
083110081017     c                   if            §AR8cxd = *blanks
083120081017     c                             or  DB0fa4  = 'P'
083130050331     c                   write     A4prtDT0
083140050331     c                   endif
083150050217     c                   write     A4prtAC0
083160050217     c*** quante righe?  write     A4prtAC0A
083170050217     c                   write     A4prtAC1A
083180050217     c                   write     A4prtAC1B
083190050113      *
083200050113     c                   ENDSR
083210050214      *
083220050113      *---------------------------------------------------------------*
083230050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
083240050217      *?Dati di dettaglio: descrizioni generiche                     ?*
083250050113      *---------------------------------------------------------------*
083260050217     c     PrtDdtARCT_C1 BEGSR
083270050113      *
083280050114     c                   clear                   A4prtAC2
083290051110     c                   clear                   dAR8ARCTC1
083300050707     c                   movel     AR8uni        dAR8ARCTC1
083310050217      * > dati in note DDT
083320050217      * - - riga descrittiva
083330050707     c                   eval      PARCTdpr  = §ARCTC1a01
083340050113      *
083350050113      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
083360050113      *
083370050113     c                   exsr      Test_OF
083380050114     c                   write     A4prtAC2
083390050113      *
083400050113     c                   ENDSR
083410050113      *
083420050113      *---------------------------------------------------------------*
083430050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
083440050217      *?Dati di dettaglio: descrizione, taglie, qtà                  ?*
083450050113      *---------------------------------------------------------------*
083460050217     c     PrtDdtARCT_C2 BEGSR
083470050113      *
083480050114     c                   clear                   A4prtAC3
083490051110     c                   clear                   dAR8ARCTC2
083500050707     c                   movel     AR8uni        dAR8ARCTC2
083510050706      *
083520050706      * TEST effettiva numericità dei campi previsti numerici:
083530100513     c                   clear                   w053a
083540050706     c                   eval      WX    =  %len(§ARCTC2b00)
083550050706     c                                   +  %len(§ARCTC2b01)
083560050706     c                                   +  %len(§ARCTC2b02)
083570050706     c                                   +  %len(§ARCTC2b03)
083580050706     c                                   +  %len(§ARCTC2b04)
083590050706     c                                   +  %len(§ARCTC2b05)
083600050706     c                                   +  %len(§ARCTC2b06)
083610050706     c                                   +  %len(§ARCTC2b07)
083620050706     c                                   +  %len(§ARCTC2b08)
083630050706     c                                   +  %len(§ARCTC2b09)
083640100513     c                   eval      w053a =  %subst(dAR8ARCTC2:59:WX)
083650050706     c                   eval      *in31 =  *off
083660110426     c     c_Digits      checkr    w053a :  WX                            31
083670050706if  1c                   if        *in31
083680050706     c                   clear                   §ARCTC2b00
083690050706     c                   clear                   §ARCTC2b01
083700050706     c                   clear                   §ARCTC2b02
083710050706     c                   clear                   §ARCTC2b03
083720050706     c                   clear                   §ARCTC2b04
083730050706     c                   clear                   §ARCTC2b05
083740050706     c                   clear                   §ARCTC2b06
083750050706     c                   clear                   §ARCTC2b07
083760050706     c                   clear                   §ARCTC2b08
083770050706     c                   clear                   §ARCTC2b09
083780050706e   1c                   endif
083790050113      *
083800050113      * > dati in dettaglio DDT
083810050113      * - - descrizione articolo
083820050707     c                   eval      PARCTcpr  = §ARCTC2baa
083830050113      * - - taglia 0
083840050707     c                   eval      PARCTt0   = §ARCTC2b00
083850050113      * - - taglia 1
083860050707     c                   eval      PARCTt1   = §ARCTC2b01
083870050113      * - - taglia 2
083880050707     c                   eval      PARCTt2   = §ARCTC2b02
083890050113      * - - taglia 3
083900050707     c                   eval      PARCTt3   = §ARCTC2b03
083910050113      * - - taglia 4
083920050707     c                   eval      PARCTt4   = §ARCTC2b04
083930050113      * - - taglia 5
083940050707     c                   eval      PARCTt5   = §ARCTC2b05
083950050113      * - - taglia 6
083960050707     c                   eval      PARCTt6   = §ARCTC2b06
083970050113      * - - taglia 7
083980050707     c                   eval      PARCTt7   = §ARCTC2b07
083990050113      * - - taglia 8
084000050707     c                   eval      PARCTt8   = §ARCTC2b08
084010050113      * - - quantità
084020050707     c                   eval      PARCTqta  = §ARCTC2b09
084030050113      *
084040050113      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
084050050113      *
084060050113     c                   exsr      Test_OF
084070050114     c                   write     A4prtAC3
084080050113      *
084090050113     c                   ENDSR
084100050113      *
084110050113      *---------------------------------------------------------------*
084120050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
084130050217      *?Dati di fondo: beni, colli, peso, tot.qtà, porto, trasp.     ?*
084140050113      *---------------------------------------------------------------*
084150050217     c     PrtDdtARCT_P1 BEGSR
084160090324      *
084170090324      /free
084180090324           select;
084190090324             when  NOT *in45;
084200090324               wMaxLine = $MaxLine4;
084210090324             when  *in45   and   NOT *in46;
084220090324               wMaxLine = $MaxLine4_pl3;
084230090324             when  *in45   and   *in46;
084240090324               wMaxLine = $MaxLine4_pl4;
084250090324           endsl;
084260090324      /end-free
084270050113      *
084280051110     c                   clear                   dAR8ARCTP1
084290050707     c                   movel     AR8uni        dAR8ARCTP1
084300050706      *
084310050706      * TEST effettiva numericità dei campi previsti numerici:
084320100513     c                   clear                   w053a
084330050706     c                   eval      WX    =  %len(§ARCTP1a03)
084340100513     c                   eval      w053a =  %subst(dAR8ARCTP1:28:WX)
084350050706     c                   eval      *in31 =  *off
084360110426     c     c_Digits      checkr    w053a :  WX                            31
084370050706if  1c                   if        *in31
084380050706     c                   clear                   §ARCTP1a03
084390050706e   1c                   endif
084400050706     c                   eval      WX    =  %len(§ARCTP1b02)
084410050706     c                                   +  %len(§ARCTP1b03)
084420050706     c                                   +  %len(§ARCTP1b04)
084430100513     c                   eval      w053a =  %subst(dAR8ARCTP1:68:WX)
084440050706     c                   eval      *in31 =  *off
084450110426     c     c_Digits      checkr    w053a :  WX                            31
084460050706if  1c                   if        *in31
084470050706     c                   clear                   §ARCTP1b02
084480050706     c                   clear                   §ARCTP1b03
084490050706     c                   clear                   §ARCTP1b04
084500050706e   1c                   endif
084510050113      *
084520050113      * > dati in chiusura DDT
084530050114      * - - costanti di fine doc. e n. borderò
084540050707     c                   eval      PARCTc1   = §ARCTP1a01
084550050707     c                   eval      PARCTc2   = §ARCTP1a02
084560050707     c                   eval      PARCTc3   = §ARCTP1a03
084570050114      * - - costante segue fattura
084580050707     c                   eval      PARCTc4   = §ARCTp1a04
084590050113      * - - aspetto esteriore dei beni
084600050707     c                   eval      PARCTaeb  = §ARCTP1b01
084610050113      * - - numero colli
084620050707     c                   eval      PARCTncl  = §ARCTP1b02
084630050113      * - - peso in kg
084640050707     c                   eval      PARCTpkg  = §ARCTP1b03
084650050113      * - - totale qtà
084660050707     c                   eval      PARCTtqta = §ARCTP1b04
084670050113      * - - causale trasporto
084680050707     c                   eval      PARCTctr  = §ARCTP1c01
084690050113      * - - porto
084700050707     c                   eval      PARCTpto  = §ARCTP1c02
084710050113      * - - trasporto a cura
084720050707     c                   eval      PARCTtra  = §ARCTP1c03
084730050113      * - - trasportatore
084740050707     c                   eval      PARCTvet  = §ARCTP1d01
084750050217     *** *
084760050217     *** * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
084770050217     *** * ?(impostata a fine ciclo di lettura - sr. DDT_Arcte)?
084780050113      *
084790050113      * N.B.: la copia per il destinatario della LDV deve comparire
084800050113      *       nell'ultima pagina del DDT per il cliente (vedi subr.
084810050113      *       "ChiusuraLDV")
084820050113      *
084830050113     c                   ENDSR
084840081002      /free
084850081002       //--------------------------------------------------------------
084860140307       //?Impostazione nuova pagina di DDT Arcte                       ?
084870081002       //--------------------------------------------------------------
084880081002       BEGSR  NewPageAC;
084890081002
084900081002         // Impostazione del nuovo limite x overflow
084910081002         select;
084920081002           when  NOT *in45;
084930081002             wMaxLine = $MaxLine2;
084940081002           when  *in45   and   NOT *in46;
084950090324             wMaxLine = $MaxLine5_pl1;
084960081002           when  *in45   and   *in46;
084970090324             wMaxLine = $MaxLine5_pl2;
084980081002         endsl;
084990081002
085000081002         write  A4prtSP1;
085010081002         write  A4prtAC1B;
085020081002
085030081002       ENDSR;
085040081002      /end-free
085050050113      *
085060050113      *---------------------------------------------------------------*
085070050217      *?Posizionamento di linee e box nel DDT ARCTE                  ?*
085080050113      *---------------------------------------------------------------*
085090050114     c     Posiz_AC      BEGSR
085100080708      *
085110081002      /free
085120081002         // Impostazione posizioni di logo/box
085130081002         // (posizioni di "partenza" diverse a seconda del posizionam.
085140081002         //  iniziale nel foglio A4 attualmente in stampa)
085150081002         select;
085160081002           when  NOT *in45;
085170081002             §PBA =  3.130;
085180081002           when  *in45   and   NOT *in46;
085190081002             §PBA = 0.260;
085200081002           when  *in45   and   *in46;
085210081002             §PBA = 6.010;
085220081002         endsl;
085230081002      /end-free
085240080708     c                   eval      §PBB  =  §PBA + 1.27
085250080708     c                   eval      §LOA  =  §PBA + 0.63
085260050218      *
085270050217      * Impostazione posizioni di box
085280050217if  1c                   if        Page  <= 1
085290050217      *   (posizione di "partenza" su 1ª pagina)
085300081002     c                   eval      §PB1  =  §PBA + 1.38
085310050217x   1c                   else
085320050217      *   (posizione di "partenza" su pagine successive)
085330081002     c                   eval      §PB1  =  §PBA
085340050217e   1c                   endif
085350081002     c                   eval      §PB2  =  §PBA + 5.395
085360050217      * Impostazione posizioni di linee (orizzontali e verticali)
085370050217      *   nella parte superiore del box (testata)
085380080708     c                   eval      §LO1  =  §PB1 + 0.13
085390080708     c                   eval      §LO2  =  §LO1 + 0.13
085400050217      * Impostazione posizioni di linee (orizzontali e verticali)
085410050217      *   nella parte inferiore del box (piede)
085420081002     c                   eval      §LO3  =  §PBA + 5.005
085430081002     c                   eval      §LO4  =  §PBA + 5.135
085440081002     c                   eval      §LO5  =  §PBA + 5.265
085450050217      * Impostazione lunghezza delle linee verticali
085460050217      *   nella parte superiore del box (testata)
085470050217     c                   eval      §LV1  =  §LO1 - §PB1
085480050217     c                   eval      §LV2  =  §LO2 - §LO1
085490050217      * La lunghezza delle linee verticali separatrici nel dettaglio
085500050217      *   dipende dalla fase di stampa: se si sta stampando ancora il
085510050217      *   dettaglio deve essere più lunga, visto che non viene stampa-
085520050217      *   ta la parte finale del DDT...
085530050217if  1c                   if        %subst(AR8trk:1:1) = 'P'
085540050218     c                             and    §PS1        < $LimOF
085550050218     c                             and    CurLineA4   < wMaxLine
085560050217     c                   eval      §LV3  =  §LO3 - §LO2
085570050217x   1c                   else
085580050217     c                   eval      §LV3  =  §PB2 - §LO2
085590050217e   1c                   endif
085600050217     c                   eval      §LV4  =  §LO4 - §LO3
085610050217     c                   eval      §LV5  =  §LO5 - §LO4
085620050113      * Impostazione posizioni dei campi di testo verticali
085630081002     c                   eval      §PR1  =  §PBA + 5.35
085640081002     c                   eval      §PR2  =  §PBA + 5.47
085650050113      *
085660050113     c                   ENDSR
085670050214      *
085680050214      *---------------------------------------------------------------*
085690050217      *?Stampa DDT del mittente Esprinet                             ?*
085700050214      *---------------------------------------------------------------*
085710050214     c     DDT_Esprinet  BEGSR
085720050214      *
085730081002      /free
085740110405         // -?Impostazione del nuovo limite x overflow?
085750110405         //  ?Codesto limite è valido solo per la stampa del dettaglio.?
085760110405         //  ?Per la stampa del "fondo" bolla occorre REimpostarlo ad?
085770110405         //  ?un valore più basso.?
085780110405         //  ?Ovvero: stampo più righe di dettaglio che posso; se alla?
085790110405         //  ?fine delle righe di dettaglio non rimane spazio a suffi-?
085800110405         //  ?cienza per il fondo: prima della stampa del fondo si salta?
085810110405         //  ?alla pagina successiva, dove non si stampa alcuna riga di?
085820110405         //  ?dettaglio.?
085830081002         select;
085840081002           when  NOT *in45;
085850081002             wMaxLine = $MaxLine2;
085860081002           when  *in45   and   NOT *in46;
085870090324             wMaxLine = $MaxLine5_pl1;
085880081002           when  *in45   and   *in46;
085890090324             wMaxLine = $MaxLine5_pl2;
085900081002         endsl;
085910081002
085920110405         // -?Posizionamento iniziale all'interno del foglio A4?
085930081002         write  A4prtSP1;
085940110405
085950110405         // -?Pulizia tipo record?
085960110405         clear  SAVtrk;
085970081002      /end-free
085980050214      *
085990050214      * Impostazione testi fissi - - - - - - - - - - - - - - - - - - -*
086000050214      *
086010050325     ***c                   eval      PCLtx1 =
086020050325     ***c                             'Settimo Torinese - Tel. 011.895.39.29 *-
086030050325     ***c                              Padova - Tel. 049.870.50.40 *-
086040050325     ***c                              Bologna - Tel. 051.324.249 *-
086050050325     ***c                              Osimo - Tel. 071.781.96.75 *-
086060050325     ***c                              Roma - Tel. 06.723.15.43'
086070050325     ***c                   eval      PCLtx2 =
086080050325     ***c                             'Casalnuovo - Tel. 081.522.30.31 *-
086090050325     ***c                              Cinisello B. - Tel. 02.66.04.11.13 *-
086100050325     ***c                              Verona - Tel. 045.823.01.65 *-
086110050325     ***c                              Cagliari - Tel. 070.253.178'
086120050325     c                   eval      PCLtx3 =
086130131017     c                             'CONTRIBUTO AMBIENTALE CONAI ASSOLTO'
086140131017     c*//                           OVE DOVUTO'
086150050214      *
086160050214     c                   eval      PCLpx2 =
086170050214     c                             'IL PRESENTE DOCUMENTO E'' REGOLATO-
086180050214     c                              DALLE CONDIZIONI GENERALI DI VENDI-
086190050214     c                             TA PUBBLICATE SUL SITO INTERNET ALL-
086200050214     c                             ''INDIRIZZO www.esprinet.com CHE SI-
086210050325     c                              INTENDONO ESPRESSAMENTE CONOSCIUTE-
086220050325     c                              ED ACCETTATE'
086230050214      *
086240050214     c                   eval      PCLpx3a =
086250050214     c                             'NB. IN CASO DI RITARDATO PAGAMENTO-
086260050214     c                             , VERRANNO ADDEBITATI'
086270050214     c                   eval      PCLpx3b =
086280050214     c                             'INTERESSI AL TASSO DEL PRIME RATE -
086290050214     c                             MAGGIORATO DEL 2% A'
086300050214     c                   eval      PCLpx3c =
086310050214     c                             'DECORRERE DAL GIORNO SUCCESSIVO AL-
086320050214     c                             LA SCADENZA DELLA FATTURA'
086330050214      *
086340050214     c                   eval      PCLpx4a =
086350050214     c                             'Si prega di verificare la Vs. ragi-
086360050214     c                             one sociale, indirizzo e'
086370050214     c                   eval      PCLpx4b =
086380050214     c                             'partita IVA, dandoci eventuale sol-
086390050214     c                             lecita rettifica scritta.'
086400050214     c***                eval      PCLpx4c = *blanks
086410050214      *
086420050214     c                   eval      PCLpx5a =
086430050214     c                             '* I PRODOTTI IMPORTATI DAGLI U.S.A.-
086440050214     c                              SONO CON SPECIALE'
086450050214     c                   eval      PCLpx5b =
086460050214     c                             'LICENZA CHE PRECLUDE RIESPORTAZIONI-
086470050214     c                              NON AUTORIZZATE.'
086480050214     c***                eval      PCLpx5c = *blanks
086490050214      *
086500050325     ***c                   eval      PCLpx6  =
086510050325     ***c                             'CONTRIBUTO AMBIENTALE CONAI ASSOLTO-
086520050325     ***c                              OVE DOVUTO'
086530050325     c                   eval      PCLpx6  =
086540050325     c                             'LA SOCIETÀ SI IMPEGNA A VERSARE L''-
086550050325     c                             IVA NEI MODI E TERMINI PREVISTI (S-
086560050325     c                             OLIDARIETÀ ART. 60 BIS DPR 633/72-F-
086570050325     c                             INANZIARIA 2005)'
086580050214      *
086590130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
086600130315      *  "StampaDDT")
086610050214     c***  KLDV          setll     FIAR8000       (già fatto)
086620050214     c***  KLDV          reade     FIAR8000       (già fatto)
086630050214      *
086640050214      * Reperimento dati, impostazione e stampa DDT mittente  - - - -*
086650050214      *
086660050214if  1c                   dow       NOT %eof(FIAR801L)
086670050214      *
086680050214sel 2c                   select
086690050214      * - Record annullato (non si stampa nulla)
086700050217w   2c                   when      AR8atb  <> *blanks
086710050214      * - Dati di dettaglio: articolo, descrizione, quantità, ...
086720050217w   2c                   when      AR8trk   = 'C1'
086730050217     c                   exsr      PrtDdtESP1_C1
086740050214      * - Dati di dettaglio: testo (riga libera)
086750050217w   2c                   when      AR8trk   = 'C2'
086760050217     c                   exsr      PrtDdtESP1_C2
086770050214      * - Dati di testata: destinatario, cliente di fatturazione
086780050217w   2c                   when      AR8trk   = 'T1'
086790050217     c                   exsr      PrtDdtESP1_T1
086800050214      * - Dati di testata: estremi documento, riferimenti, ...
086810050217w   2c                   when      AR8trk   = 'T2'
086820050217     c                   exsr      PrtDdtESP1_T2
086830050214      * - Dati di fondo: totali del documento (netto merce, iva, ...)
086840050217w   2c                   when      AR8trk   = 'P1'
086850050217     c                   exsr      PrtDdtESP1_P1
086860050214      * - Dati di fondo: informazioni vettore
086870050217w   2c                   when      AR8trk   = 'P2'
086880050217     c                   exsr      PrtDdtESP1_P2
086890050214e   2c                   endsl
086900110405      *
086910110405     c                   eval      SAVtrk = AR8trk
086920050214      *
086930050214     c     KLDV          reade     FIAR8000
086940050214      *
086950050214e   1c                   enddo
086960050215      *
086970050215      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
086980050215      *
086990050215      * - - Verifica OverFlow
087000050215     c                   exsr      Test_OF
087010050218      *
087020050218      * - - Calcolo la posizione finale del box e di linee separatrici
087030050218     c                   exsr      Posiz_ES
087040050215      *
087050050215      * - - Stampa parte finale DDT
087060050215     c                   write     A4prtES4
087070050215      *
087080050215      * > Stampa box, linee e testi fissi (no logo)  - - - - - - - - -*
087090081017     c                   if            §AR8cxd = *blanks
087100081017     c                             or  DB0fa4  = 'P'
087110050331     c                   write     A4prtDT0
087120050331     c                   endif
087130050215     c                   write     A4prtES0A
087140050215     c                   write     A4prtES0B
087150050214      *
087160050214      * Reimpostazione del vecchio limite x overflow
087170081002      /free
087180081002         select;
087190081002           when  NOT *in45;
087200081002             wMaxLine = $MaxLine2;
087210081002           when  *in45   and   NOT *in46;
087220081002             wMaxLine = $MaxLine2_pl1;
087230081002           when  *in45   and   *in46;
087240081002             wMaxLine = $MaxLine2_pl2;
087250081002         endsl;
087260081002      /end-free
087270050214      *
087280050214     c                   ENDSR
087290050214      *
087300050214      *---------------------------------------------------------------*
087310050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
087320050217      *?Dati di testata: fatturato a, destinazione, cliente          ?*
087330050214      *---------------------------------------------------------------*
087340050217     c     PrtDdtESP1_T1 BEGSR
087350110405      *
087360110405      * - pulizia record a nuova packing-list
087370110405     c                   if            %subst( SAVtrk : 1 : 1 ) = 'P'
087380110405     c                             or  %subst( SAVtrk : 1 : 1 ) = 'C'
087390110405     c                   exsr      sr_ClearESP1
087400110405     c                   endif
087410050214      *
087420050214      * > logo
087430050214     c                   eval      §Logo  = 'LOGO' + AR8tmd
087440050214      *
087450051110     c                   movel(p)  AR8uni        dAR8ESP1T1
087460050214      *
087470050214      * > fatturato a
087480050707     c                   eval      PCrsf  = §A8ESP1T1a
087490050707     c                   eval      PCinf  = §A8ESP1T1b
087500050707if  1c                   if        §A8ESP1T1c > *zeros
087510050707     c                   eval      PCclpf = §A8ESP1T1c
087520050216     c                                    + ' '
087530050707     c                                    + §A8ESP1T1d
087540050216     c                                    + ' '
087550050707     c                                    + §A8ESP1T1e
087560050216x   1c                   else
087570050216     c                   eval      PCclpf = '     '
087580050216     c                                    + ' '
087590050707     c                                    + §A8ESP1T1d
087600050216     c                                    + ' '
087610050707     c                                    + §A8ESP1T1e
087620050216e   1c                   endif
087630050214      * > destinatazione
087640050707     c                   eval      PCrsd  = §A8ESP1T1f
087650050707     c                   eval      PCind  = §A8ESP1T1g
087660050707if  1c                   if        §A8ESP1T1h > *zeros
087670050707     c                   eval      PCclpd = §A8ESP1T1h
087680050216     c                                    + ' '
087690050707     c                                    + §A8ESP1T1i
087700050216     c                                    + ' '
087710050707     c                                    + §A8ESP1T1j
087720050216x   1c                   else
087730050216     c                   eval      PCclpd = '     '
087740050216     c                                    + ' '
087750050707     c                                    + §A8ESP1T1i
087760050216     c                                    + ' '
087770050707     c                                    + §A8ESP1T1j
087780050216e   1c                   endif
087790050214      * > dati in testata DDT
087800050214      * - - codice cliente
087810050707     c                   eval      PCcli  = §A8ESP1T1k
087820050214      * - - codice fiscale
087830050707     c                   eval      PCcf   = §A8ESP1T1l
087840050214      * - - partita iva
087850050707     c                   eval      PCpi   = §A8ESP1T1m
087860050214      *
087870050214     c                   ENDSR
087880050214      *
087890050214      *---------------------------------------------------------------*
087900050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
087910050217      *?Dati di testata: documento, riferimenti.                     ?*
087920050214      *---------------------------------------------------------------*
087930050217     c     PrtDdtESP1_T2 BEGSR
087940110405      *
087950110405      * - pulizia records a nuova packing-list
087960110405     c                   if            %subst( SAVtrk : 1 : 1 ) = 'P'
087970110405     c                             or  %subst( SAVtrk : 1 : 1 ) = 'C'
087980110405     c                   exsr      sr_ClearESP1
087990110405     c                   endif
088000050214      *
088010051110     c                   movel(p)  AR8uni        dAR8ESP1T2
088020050214      *
088030050214      * > dati in testata DDT
088040050214      * - - tipo documento
088050050707     c                   eval      PCtdo  = §A8ESP1T2a
088060050214      * - - data e numero documento (DDT)
088070050707     c                   eval      PCdtd  = §A8ESP1T2b
088080050707     c                   eval      PCnrd  = §A8ESP1T2c
088090050214      * - - riferimenti mittente
088100050707     c                   eval      PCrif  = §A8ESP1T2d
088110050214      * - - condizioni di pagamento
088120050707     c                   eval      PCpag  = §A8ESP1T2e
088130050214      * - - annotazioni
088140050707     c                   eval      PCann  = §A8ESP1T2f
088150050214      * - - agente
088160050707     c                   eval      PCag2  = §A8ESP1T2g
088170050214      *
088180050214      * > Stampa testata DDT - - - - - - - - - - - - - - - - - - - - -*
088190050214      *
088200050214      *   - OverFlow impossibile qui! -
088210050215     c***                exsr      Test_OF
088220050215     c                   exsr      Posiz_ES
088230081017     c                   if            §AR8cxd = *blanks
088240081017     c                             or  DB0fa4  = 'P'
088250050331     c                   write     A4prtDT0
088260050331     c                   endif
088270050214     c                   write     A4prtES0
088280081002     c*** (NON qui:)     write     A4prtES0A
088290081002     c                   write     A4prtSP1
088300050215     c                   write     A4prtES1A
088310050215     c                   write     A4prtES1B
088320050214      *
088330050214     c                   ENDSR
088340050214      *
088350050214      *---------------------------------------------------------------*
088360050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
088370050217      *?Dati di dettaglio: articolo, descrizione, quantità, ...      ?*
088380050214      *---------------------------------------------------------------*
088390050217     c     PrtDdtESP1_C1 BEGSR
088400110405      *
088410110405      * - pulizia records a nuova packing-list
088420110405     c                   if        %subst( SAVtrk : 1 : 1 ) = 'P'
088430110405     c                   exsr      sr_ClearESP1
088440110405     c                   endif
088450050214      *
088460050214     c                   clear                   A4prtES2
088470051110     c                   movel(p)  AR8uni        dAR8ESP1C1
088480050214      *
088490050214      * > dati in dettaglio DDT
088500050214      * - - codice prodotto
088510050707     c                   eval      PCcpr  = §A8ESP1C1a
088520050214      * - - descrizione prodotto
088530050707     c                   eval      PCdpr  = §A8ESP1C1b
088540050214      * - - quantità
088550050707     c                   eval      PCqta  = '      ' + §A8ESP1C1c
088560050214      * - - prezzo unitario
088570050707     c                   eval      PCpru  = §A8ESP1C1d
088580050214      * - - cambio: divisa + valore
088590050707     c                   eval      PCdiv  = §A8ESP1C1e
088600050707     c                   eval      PCcmb  = §A8ESP1C1f
088610050214      * - - % sconto
088620050707     c                   eval      PCprc  = §A8ESP1C1g
088630050214      * - - importo
088640050707     c                   eval      PCimp  = §A8ESP1C1h
088650050214      * - - codice iva
088660050707     c                   eval      PCciv  = §A8ESP1C1i
088670050214      *
088680050214      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
088690050214      *
088700050214     c                   exsr      Test_OF
088710050214     c                   write     A4prtES2
088720050214      *
088730050214     c                   ENDSR
088740050214      *
088750050214      *---------------------------------------------------------------*
088760050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
088770050217      *?Dati di dettaglio: note (riga libera)                        ?*
088780050214      *---------------------------------------------------------------*
088790050217     c     PrtDdtESP1_C2 BEGSR
088800110405      *
088810110405      * - pulizia records a nuova packing-list
088820110405     c                   if        %subst( SAVtrk : 1 : 1 ) = 'P'
088830110405     c                   exsr      sr_ClearESP1
088840110405     c                   endif
088850050214      *
088860050215     c                   clear                   A4prtES3
088870051110     c                   movel(p)  AR8uni        dAR8ESP1C2
088880050214      *
088890050214      * > dati in note DDT
088900050214      * - - riga descrittiva
088910050707     c                   eval      PCrig  = §A8ESP1C2a
088920050214      *
088930050214      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
088940050214      *
088950050214     c                   exsr      Test_OF
088960050214     c                   write     A4prtES3
088970050214      *
088980050214     c                   ENDSR
088990050214      *
089000050214      *---------------------------------------------------------------*
089010050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
089020050217      *?Dati di fondo: totali documento                              ?*
089030050214      *---------------------------------------------------------------*
089040050217     c     PrtDdtESP1_P1 BEGSR
089050050214      *
089060081002      /free
089070081002           select;
089080081002             when  NOT *in45;
089090081002               wMaxLine = $MaxLine6;
089100081002             when  *in45   and   NOT *in46;
089110081002               wMaxLine = $MaxLine6_pl1;
089120081002             when  *in45   and   *in46;
089130081002               wMaxLine = $MaxLine6_pl2;
089140081002           endsl;
089150081002      /end-free
089160051110     c                   movel(p)  AR8uni        dAR8ESP1P1
089170050215      *
089180050215      * > dati in fondo DDT
089190050215      * - - netto merce
089200050707     c                   eval      PCinm  = §A8ESP1P1a
089210050215      * - - spese di trasporto
089220050707     c                   eval      PCist  = §A8ESP1P1b
089230050215      * - - spese imballo
089240050707     c                   eval      PCisi  = §A8ESP1P1c
089250050215      * - - spese bolli
089260050707     c                   eval      PCisb  = §A8ESP1P1d
089270050215      * - - spese amministrative
089280050707     c                   eval      PCisa  = §A8ESP1P1e
089290050215      * - - spese varie
089300050707     c                   eval      PCisv  = §A8ESP1P1f
089310050215      * - - codici iva
089320050707     c                   eval      PCic1  = §A8ESP1P1g
089330050707     c                   eval      PCic2  = §A8ESP1P1m
089340050707     c                   eval      PCic3  = §A8ESP1P1s
089350050215      * - - descrizioni iva
089360050707     c                   eval      PCid1  = §A8ESP1P1h
089370050707     c                   eval      PCid2  = §A8ESP1P1n
089380050707     c                   eval      PCid3  = §A8ESP1P1t
089390050215      * - - imponibili iva: divise
089400050707     c                   eval      PCie1  = §A8ESP1P1i
089410050707     c                   eval      PCie2  = §A8ESP1P1o
089420050707     c                   eval      PCie3  = §A8ESP1P1u
089430050215      * - - imponibili iva: importi
089440050707     c                   eval      PCii1  = §A8ESP1P1j
089450050707     c                   eval      PCii2  = §A8ESP1P1p
089460050707     c                   eval      PCii3  = §A8ESP1P1v
089470050215      * - - aliquote iva
089480050707     c                   eval      PCia1  = §A8ESP1P1k
089490050707     c                   eval      PCia2  = §A8ESP1P1q
089500050707     c                   eval      PCia3  = §A8ESP1P1w
089510050215      * - - imposte iva
089520050707     c                   eval      PCiv1  = §A8ESP1P1l
089530050707     c                   eval      PCiv2  = §A8ESP1P1r
089540050707     c                   eval      PCiv3  = §A8ESP1P1x
089550050215      * - - Totali documento: divise + importi
089560050707     c                   eval      PCtdd  = §A8ESP1P1y
089570050707     c                   eval      PCtdi  = §A8ESP1P1z
089580050707     c                   eval      PCtdd2 = §A8ESP1P11
089590050707     c                   eval      PCtdi2 = §A8ESP1P12
089600050214      *
089610050214     c                   ENDSR
089620050214      *
089630050214      *---------------------------------------------------------------*
089640050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
089650050217      *?Dati di fondo: informazioni vettore                          ?*
089660050214      *---------------------------------------------------------------*
089670050217     c     PrtDdtESP1_P2 BEGSR
089680050214      *
089690081002      /free
089700081002           select;
089710081002             when  NOT *in45;
089720081002               wMaxLine = $MaxLine6;
089730081002             when  *in45   and   NOT *in46;
089740081002               wMaxLine = $MaxLine6_pl1;
089750081002             when  *in45   and   *in46;
089760081002               wMaxLine = $MaxLine6_pl2;
089770081002           endsl;
089780081002      /end-free
089790051110     c                   movel(p)  AR8uni        dAR8ESP1P2
089800050215      *
089810050215      * > dati in fondo DDT
089820050215      * - - resa
089830050707     c                   eval      PCres  = §A8ESP1P2a
089840050215      * - - trasporto a cura del
089850050707     c                   eval      PCtra  = §A8ESP1P2b
089860050215      * - - vettore
089870110426     c*//                eval      PCvet  = $Bartolini
089880110426     c*//                                 + ' - '
089890110426     c*//                                 + $Intest2
089900110426     c*//                                 + ' (BO)'
089910050707     c                   eval      PCvet2 = §A8ESP1P2c
089920050215      * - - causale trasporto
089930050707     c                   eval      PCctr2 = §A8ESP1P2d
089940050215      * - - aspetto esteriore dei beni
089950050707     c                   eval      PCaeb  = §A8ESP1P2e
089960050215      * - - numero colli
089970050707     c                   eval      PCncl2 = §A8ESP1P2f
089980050215      * - - peso complessivo
089990050707     c                   eval      PCpes  = §A8ESP1P2g
090000050215      * - - volume
090010050707     c                   eval      PCvol  = §A8ESP1P2h
090020050215      * - - inizio trasporto: data + ora
090030050707     c                   eval      PCdit  = §A8ESP1P2i
090040050707     c                   eval      PCoit  = §A8ESP1P2j
090050050215     *** *
090060050215     *** * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
090070050215     *** * ?(impostata a fine ciclo di lettura - sr. DDT_Esprinet)?
090080050217      *
090090050217      * N.B.: la copia per il destinatario della LDV deve comparire
090100050217      *       nell'ultima pagina del DDT per il cliente (vedi subr.
090110050217      *       "ChiusuraLDV")
090120050214      *
090130050214     c                   ENDSR
090140081002      /free
090150081002       //--------------------------------------------------------------
090160110405       //?Impostazione nuova pagina di DDT Esprinet                    ?
090170081002       //--------------------------------------------------------------
090180081002       BEGSR  NewPageES;
090190081002
090200110405         // -?Impostazione del nuovo limite x overflow?
090210081002         select;
090220081002           when  NOT *in45;
090230081002             wMaxLine = $MaxLine2;
090240081002           when  *in45   and   NOT *in46;
090250090324             wMaxLine = $MaxLine5_pl1;
090260081002           when  *in45   and   *in46;
090270090324             wMaxLine = $MaxLine5_pl2;
090280081002         endsl;
090290081002
090300081002         write  A4prtSP1;
090310081002         write  A4prtES1B;
090320081002
090330081002       ENDSR;
090340110405
090350110405       //--------------------------------------------------------------
090360110405       //?Pulizia campi a nuova packing-list relativa alla stessa LdV  ?
090370110405       //--------------------------------------------------------------
090380110405       BEGSR  sr_ClearESP1;
090390110405
090400110405         // -?Stampa fondo DDT?
090410110405
090420110405         // - -?Calcolo la posizione finale del box e di linee?
090430110405         //    ?separatrici?
090440110405         exsr  Posiz_ES;
090450110405
090460110405         // - -?Stampa parte finale DDT?
090470110405         write  A4prtES4;
090480110405
090490110405         // - -?Stampa box, linee e testi fissi (no logo)?
090500110405         if  §AR8cxd = *blank  or  DB0fa4 = 'P';
090510110405           write  A4prtDT0;
090520110405         endif;
090530110405         write  A4prtES0A;
090540110405         write  A4prtES0B;
090550110405
090560110405         // -?Pulizia record?
090570110405         select;
090580110405           when  %subst( AR8trk : 1 : 1 ) = 'T';
090590110405             clear  A4prtES0;
090600110405             clear  A4prtES0A;
090610110405             clear  A4prtES0B;
090620110405             clear  A4prtES1A;
090630110405             clear  A4prtES1B;
090640110405             clear  A4prtES2;
090650110405             clear  A4prtES3;
090660110405             clear  A4prtES4;
090670110405           when  %subst( AR8trk : 1 : 1 ) = 'C';
090680110405             clear  A4prtES2;
090690110405             clear  A4prtES3;
090700110405             clear  A4prtES4;
090710110405         endsl;
090720110405
090730110405         // -?Impostazione testi fissi?
090740131017         //PCLtx3 = 'CONTRIBUTO AMBIENTALE CONAI ASSOLTO OVE DOVUTO';
090750131017         PCLtx3 = 'CONTRIBUTO AMBIENTALE CONAI ASSOLTO';
090760110405
090770110405         PCLpx2 = 'IL PRESENTE DOCUMENTO E'' REGOLATO DALLE +
090780110405                   CONDIZIONI GENERALI DI VENDITA PUBBLICATE SUL SITO +
090790110405                   INTERNET ALL''INDIRIZZO www.esprinet.com CHE SI +
090800110405                   INTENDONO ESPRESSAMENTE CONOSCIUTE ED ACCETTATE';
090810110405
090820110405         PCLpx3a = 'NB. IN CASO DI RITARDATO PAGAMENTO, VERRANNO +
090830110405                    ADDEBITATI';
090840110405         PCLpx3b = 'INTERESSI AL TASSO DEL PRIME RATE MAGGIORATO +
090850110405                    DEL 2% A';
090860110405         PCLpx3c =
090870110405                   'DECORRERE DAL GIORNO SUCCESSIVO ALLA SCADENZA +
090880110405                    DELLA FATTURA';
090890110405
090900110405         PCLpx4a = 'Si prega di verificare la Vs. ragione sociale +
090910110405                    indirizzo e';
090920110405         PCLpx4b = 'partita IVA, dandoci eventuale sollecita +
090930110405                    rettifica scritta.';
090940110405
090950110405         PCLpx5a = '* I PRODOTTI IMPORTATI DAGLI U.S.A. SONO CON +
090960110405                    SPECIALE';
090970110405         PCLpx5b = 'LICENZA CHE PRECLUDE RIESPORTAZIONI NON +
090980110405                    AUTORIZZATE.';
090990110405
091000110405         PCLpx6  = 'LA SOCIETÀ SI IMPEGNA A VERSARE L''IVA NEI MODI +
091010110405                    E TERMINI PREVISTI (SOLIDARIETÀ ART. 60 BIS DPR +
091020110405                    633/72-FINANZIARIA 2005)';
091030110405
091040110405         // -?Salto Pagina?
091050110405         exsr  Test_OF;
091060110405
091070110405         // -?Re-Impostazione limiti di overflow?
091080110405         select;
091090110405           when  NOT *in45;
091100110405             wMaxLine = $MaxLine2;
091110110405           when  *in45   and   NOT *in46;
091120110405             wMaxLine = $MaxLine5_pl1;
091130110405           when  *in45   and   *in46;
091140110405             wMaxLine = $MaxLine5_pl2;
091150110405         endsl;
091160110405
091170110405         // -?Posizionamento iniziale all'interno del foglio A4?
091180110405         write  A4prtSP1;
091190110405
091200110405       ENDSR;
091210081002      /end-free
091220050214      *
091230050214      *---------------------------------------------------------------*
091240050217      *?Posizionamento di linee e box nel DDT ESPRINET               ?*
091250050214      *---------------------------------------------------------------*
091260050215     c     Posiz_ES      BEGSR
091270080708      *
091280081002      /free
091290081002         // Impostazione posizioni di logo/box
091300081002         // (posizioni di "partenza" diverse a seconda del posizionam.
091310081002         //  iniziale nel foglio A4 attualmente in stampa)
091320081002         select;
091330081002           when  NOT *in45;
091340081006             §PBA =  3.130;
091350081006             //§PBA =  3.255;        (più in basso di una riga)
091360081002           when  *in45   and   NOT *in46;
091370081002             §PBA = 0.255;
091380081002           when  *in45   and   *in46;
091390081002             §PBA = 6.005;
091400081002         endsl;
091410081002      /end-free
091420080708      * Impostazione posizioni di linee orizzontali
091430080708     c                   eval      §LOA  =  §PBA + 0.76
091440080708     c                   eval      §LOB  =  §LOA + 0.25
091450080708     c                   eval      §LOC  =  §LOB + 0.12
091460080708      * Impostazione posizioni di linee verticali
091470080708     c                   eval      §LVA  =  §LOA - 0.15
091480050214      *
091490050214      * Impostazione posizioni di box
091500050215if  1c                   if        Page  <= 1
091510050214      *   (posizione di "partenza" su 1ª pagina)
091520081002     c                   eval      §PB1  =  §PBA + 1.270
091530050214x   1c                   else
091540050214      *   (posizione di "partenza" su pagine successive)
091550081002     c                   eval      §PB1  =  §PBA + 0.020
091560050214e   1c                   endif
091570110405if  1c*//                if        %subst(AR8trk:1:1) = 'P'
091580110405if  1c                   if        %subst(SAVtrk:1:1) = 'P'
091590050331     c                             and    CurLineA4   < wMaxLine
091600081002     c                   eval      §PB2  =  §PBA + 5.360
091610050215x   1c                   else
091620050215     c                   eval(h)   §PB2  =  CurLineA4 / 8 + 0.05
091630081002     c                   if        §PB2  <  (§PBA + 5.320)
091640081002     c                   eval      §PB2  =  (§PBA + 5.320)
091650050331     c                   endif
091660050215e   1c                   endif
091670050215      * Impostazione posizioni di linee (orizzontali e verticali)
091680050214      *   nella parte superiore del box (testata)
091690050215     c                   eval      §LO1  =  §PB1 + 0.25
091700050214     c                   eval      §LO2  =  §LO1 + 0.11
091710050214      *   nella parte inferiore del box (piede)
091720110405if  1c*//                if        %subst(AR8trk:1:1) = 'P'
091730110405if  1c                   if        %subst(SAVtrk:1:1) = 'P'
091740050331     c                             and    CurLineA4   < wMaxLine
0917500810027.78 c                   eval      §LO3  =  §PBA + 3.900
0917600502158.00 c                   eval      §LO4  =  §LO3 + 0.24
0917700502158.39 c                   eval      §LO5  =  §LO4 + 0.36
0917800502158.52 c                   eval      §LO6  =  §LO5 + 0.14
0917900502148.64 c                   eval      §LO7  =  §LO6 + 0.12
0918000502148.77 c                   eval      §LO8  =  §LO7 + 0.13
0918100502148.89 c                   eval      §LO9  =  §LO8 + 0.12
0918200807088.97 c                   eval      §LO0  =  §LO9 + 0.10
091830050215e   1c                   endif
091840050214      * La lunghezza delle linee verticali separatrici nel dettaglio
091850050214      *   dipende dalla fase di stampa: se si sta stampando ancora il
091860050214      *   dettaglio deve essere più lunga, visto che non viene stampa-
091870050214      *   ta la parte finale del DDT...
091880110405if  1c*//                if        %subst(AR8trk:1:1) = 'P'
091890110405if  1c                   if        %subst(SAVtrk:1:1) = 'P'
091900050331     c                             and    CurLineA4   < wMaxLine
0919100502162.25 c                   eval      §LV1  =  §LO3 - §LO2
091920050215x   1c                   else
091930050216     c                   eval      §LV1  =  §PB2 - §LO2
091940050215e   1c                   endif
091950050214      *
091960050214      * Impostazione posizioni di campi di testo
091970050214      * - PCLtx1/tx2 (verticali)
0919800502179.10 c                   eval      §PR1  =  §PB2 - 0.07
091990050214      * - PCLpx2 ("Il presente documento...")
0920000502178.95 c                   eval      §PS2  =  §LO9 + 0.07
092010050214      * - PCLpx3a/px4a/px5a
0920200807089.04 c                   eval      §PS3  =  §LO0 + 0.07
092030050214      * - PCLpx3b/px4b/px5b
0920400502179.09 c                   eval      §PS4  =  §PS3 + 0.07
092050050214      * - PCLpx3c/px4c/px5c
0920600502179.14 c                   eval      §PS5  =  §PS4 + 0.07
092070050214      * - PCLpx6 ("Contributo ambientale CONAI...")
0920800502179.25 c                   eval      §PS6  =  §PB2 + 0.08
092090050214      *
092100050214     c                   ENDSR
092110041207      *
092120041207      *---------------------------------------------------------------*
092130050217      *?Stampa DDT del mittente (generico con logo)                  ?*
092140041207      *---------------------------------------------------------------*
092150041207     c     DDT_GenericoL BEGSR
092160041207      *
092170081003      /free
092180081003         // Impostazione del nuovo limite x overflow
092190081003         select;
092200081003           when  NOT *in45;
092210081003             wMaxLine = $MaxLine2;
092220081003           when  *in45   and   NOT *in46;
092230081003             wMaxLine = $MaxLine2_pl1;
092240081003           when  *in45   and   *in46;
092250081003             wMaxLine = $MaxLine2_pl2;
092260081003         endsl;
092270081003
092280081003         // Posizionamento iniziale all'interno del foglio A4
092290081003         write  A4prtSP1;
092300081006
092310081006         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
092320081006           write  A4prtNUL;
092330081006         endif;
092340081003      /end-free
092350041207      *
092360041207      * > logo
092370041207     c                   eval      §Logo  = 'LOGO' + AR8tmd
092380041207      *
092390081017     c                   if            §AR8cxd = *blanks
092400081017     c                             or  DB0fa4  = 'P'
092410080709     c                   write     A4prtDT0
092420080709     c                   endif
092430080709      *
092440041209      * Imposto posizione per logo
092450081003      /free
092460081003         select;
092470081003           when  NOT *in45;
092480081003             exsr  Rep_§LO2;
092490081003             §PB1 = §LO2 + (2 / 8);
092500081003           when  *in45   and   NOT *in46;
092510081003             §PB1 = 0.250;
092520081003           when  *in45   and   *in46;
092530081003             §PB1 = 6.000;
092540081003         endsl;
092550081003      /end-free
092560041209      *   Stampa del logo
092570041209     c                   write     A4prtDB0
092580041207      *
092590041207      * Reperimento dati e stampa DDT mittente - - - - - - - - - - - -*
092600041207      *
092610130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
092620130315      *  "StampaDDT")
092630041207     c***  KLDV          setll     FIAR8000       (già fatto)
092640041207     c***  KLDV          reade     FIAR8000       (già fatto)
092650041207      *
092660041207if  1c                   dow       NOT %eof(FIAR801L)
092670041207      *
092680041207if  2c                   if        AR8atb = *blanks
092690041207      *
092700041207      * > Stampa dei dati
092710041207     c                   exsr      Test_OF
092720041207sel 3c                   select
092730041207      * - Dati di dettaglio: testo in font a dimensione standard
092740041207w   3c                   when      AR8trk = 'C1'
092750051110     c                   movel(p)  AR8uni        PCddt
092760041207     c                   write     A4prtDC1
092770041207      * - Dati di testata:   testo in font a dimensioni massime
092780041207w   3c                   when      AR8trk = 'T1'
092790051110     c                   movel(p)  AR8uni        PCddt
092800041207     c                   write     A4prtDT1
092810041207      * - Dati di testata:   testo in font a dimensioni grandi
092820041207w   3c                   when      AR8trk = 'T2'
092830051110     c                   movel(p)  AR8uni        PCddt
092840041207     c                   write     A4prtDT2
092850041209      * - Linea separatrice
092860041209w   3c                   when      AR8trk = '--'
092870041209      *   - Stampa di una riga vuota per la riga separatrice
092880041209     c                   write     A4prtNUL
092890041209      *   - Stampa della linea orizzontale separatrice
092900090213     c                   eval(h)   §LO1   = (CurLineA4 / 8) - 0.050
092910041209     c                   write     A4prtLO
092920041207e   3c                   endsl
092930041207e   2c                   endif
092940041207      *
092950041207     c                   eval      SAVtrk = AR8trk
092960041207     c     KLDV          reade     FIAR8000
092970041207      *
092980041207e   1c                   enddo
092990080708      *
093000081003      /free
093010081003         select;
093020081003           when  NOT *in45;
093030081003             wMaxLine = $MaxLine2;
093040081003           when  *in45   and   NOT *in46;
093050081003             wMaxLine = $MaxLine2_pl1;
093060081003           when  *in45   and   *in46;
093070081003             wMaxLine = $MaxLine2_pl2;
093080081003         endsl;
093090081003      /end-free
093100041207      *
093110041207     c                   ENDSR
093120040607      *
093130040607      *---------------------------------------------------------------*
093140050217      *?Stampa DDT del mittente (generico senza logo)                ?*
093150040607      *---------------------------------------------------------------*
093160040607     c     DDT_Generico  BEGSR
093170040607      *
093180081003      /free
093190081003         // Impostazione del nuovo limite x overflow
093200081003         select;
093210081003           when  NOT *in45;
093220081003             wMaxLine = $MaxLine2;
093230081003           when  *in45   and   NOT *in46;
093240081003             wMaxLine = $MaxLine2_pl1;
093250081003           when  *in45   and   *in46;
093260081003             wMaxLine = $MaxLine2_pl2;
093270081003         endsl;
093280081003
093290081003         // Posizionamento iniziale all'interno del foglio A4
093300081003         write  A4prtSP1;
093310081006
093320081006         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
093330081006           write  A4prtNUL;
093340081006         endif;
093350081003      /end-free
093360040607      *
093370081017     c                   if            §AR8cxd = *blanks
093380081017     c                             or  DB0fa4  = 'P'
093390050331     c                   write     A4prtDT0
093400050331     c                   endif
093410040607      *
093420040607      * Reperimento dati e stampa DDT mittente - - - - - - - - - - - -*
093430040607      *
093440130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
093450130315      *  "StampaDDT")
093460040607     c***  KLDV          setll     FIAR8000       (già fatto)
093470040607     c***  KLDV          reade     FIAR8000       (già fatto)
093480040607      *
093490040607if  1c                   dow       NOT %eof(FIAR801L)
093500040607      *
093510040607if  2c                   if        AR8atb = *blanks
093520051110     c                   movel(p)  AR8uni        PCddt
093530040607     c                   exsr      Test_OF
093540040607sel 3c                   select
093550040607      * - Dati di dettaglio: testo in font a dimensione standard
093560040607w   3c                   when      AR8trk = 'C1'
093570040607     c                   write     A4prtDC1
093580040607      * - Dati di testata:   testo in font a dimensioni massime
093590040607w   3c                   when      AR8trk = 'T1'
093600040607     c                   write     A4prtDT1
093610040607      * - Dati di testata:   testo in font a dimensioni grandi
093620040607w   3c                   when      AR8trk = 'T2'
093630040607     c                   write     A4prtDT2
093640040607e   3c                   endsl
093650040607e   2c                   endif
093660040607      *
093670040607     c     KLDV          reade     FIAR8000
093680040607      *
093690040607e   1c                   enddo
093700080708      *
093710081003      /free
093720081003         select;
093730081003           when  NOT *in45;
093740081003             wMaxLine = $MaxLine2;
093750081003           when  *in45   and   NOT *in46;
093760081003             wMaxLine = $MaxLine2_pl1;
093770081003           when  *in45   and   *in46;
093780081003             wMaxLine = $MaxLine2_pl2;
093790081003         endsl;
093800081003      /end-free
093810040607      *
093820040607     c                   ENDSR
093830081017
093840081017      /free
093850081017       //--------------------------------------------------------------
093860140307       //?Impostazione nuova pagina di DDT Generico (con/senza logo)   ?
093870081017       //--------------------------------------------------------------
093880081017       BEGSR  NewPageSTD;
093890081017
093900081017         // Posizionamento iniziale nella nuova pagina
093910081017         //   (già fatto con A4prtSP0 se DB0FA4 = "P")
093920081017         if  DB0fa4 <> 'P';
093930081017           write  A4prtSP1;
093940081017           if  DB0fa4 = '1'   or   DB0fa4 = 'S';
093950081017             write  A4prtNUL;
093960081017           endif;
093970081017         endif;
093980081017
093990081017         // Stampa "COPIA PER IL DESTINATARIO"
094000081017         if  §AR8cxd = *blank   or   DB0fa4 = 'P';
094010081017           write  A4prtDT0;
094020081017         endif;
094030081017
094040081017         // Impostazione del nuovo limite x overflow
094050081017         select;
094060081017           when  NOT *in45;
094070081017             wMaxLine = $MaxLine2;
094080081017           when  *in45   and   NOT *in46;
094090081017             wMaxLine = $MaxLine2_pl1;
094100081017           when  *in45   and   *in46;
094110081017             wMaxLine = $MaxLine2_pl2;
094120081017         endsl;
094130081017
094140081017       ENDSR;
094150081017      /end-free
094160030515      *
094170030515      *---------------------------------------------------------------*
094180050217      *?Stampa del record "generico" (PRTGEN)                        ?*
094190030515      *---------------------------------------------------------------*
094200030515     c     StampaGEN     BEGSR
094210030515      *
094220030515      * Controllo se è stato raggiunto l'overflow nel formato in uso
094230030515     c                   exsr      Test_OF
094240030515      *
094250030911      * Stampo la riga PRTGEN
094260030911      * Se sto stampando una "testata" (*in39) salto una riga prima
094270030911      *   ed una riga dopo (per spaziare)
094280030911if  1c                   if        *in39 = *on
094290030515     c                   write     A4prtNUL
094300030911e   1c                   endif
094310030911     c                   write     A4prtGEN
094320030911if  1c                   if        *in39 = *on
094330030911     c                   write     A4prtNUL
094340030911e   1c                   endif
094350030515      *
094360030515     c                   ENDSR
094370030515      *
094380030515      *---------------------------------------------------------------*
094390050511      *?Prepara elenco per lista - SegnaColli Non Sequenziali (test.)?*
094400030515      *---------------------------------------------------------------*
094410030515     c     ListaScNS     BEGSR
094420030515      *
094430030515     c                   clear                   XX
094440030515      *
094450030515      * Nel foglio in formato "A5" devo solo riempire gli appositi
094460030715      *   campi "Note e Particolarità" (nessun salto riga/pagina è
094470030715      *   richiesto)
094480050513     c                   if           *in69       = *on
094490050513     c                             or $A4_x_ScNS  = *off
094500030515     c                   eval      WelSNS = $elSNSxA5
094510030515     c                   goto      ScNS_A5
094520030515     c                   endif
094530030515      *
094540030515      * Determino il numero di segnacolli da elencare per riga nel
094550030515      *   formato pagina "A4" (dove ci sono righe a parte).
094560030515      *   Dipende dal carattere specificato nel prtf: più grande è
094570030515      *   il carattere => meno segnacolli ci stanno sulla riga !
094580030911      *   A cpi 15 -come ora- ci sta tutta la schiera; a cpi 12 no !
094590030515     c                   eval      WelSNS = %elem($SC1)
094600030515      *
094610030515      * Spengo gli indicatori che condizionano in stampa le indicazioni
094620030515      *   per DPD / FedEx / Bancali-Supermercati-Autotrasportatori
094630030515      *   (se sono già state stampate nella 1ª pagina non devono più
094640030515      *   esserlo)
094650030910      *   (se non sono ancora state stampate, dovranno esserlo poi)
094660030515if  1c                   if        wPage > 1
094670050513     c                   movea     *in(70)       SAVin7x
094680040223     c                   movea     '0000'        *in(70)
094690030515e   1c                   endif
094700030515      *
094710030515      * Calcolo il numero di righe da saltare
094720030515     c                   eval      wMinLine = CurLineA4
094730030515if  1c                   if        §LO2     > §LO1
094740030515      * - se stampato almeno un box
094750030515     c                   eval(h)   wMinLine = (§LO2 * 8) + 2
094760030515x   1c                   else
094770030515      * - se non stampato alcun box
094780080730      /free
094790080729       // - Calcola la posizione della riga in 1/8" di pollice
094800080729       //   da quella attuale (in 1/9" di pollice)
094810080730         eval(h) wMinLine = (CurLineA4 * 8 / 9) + 1;
094820080730      /end-free
094830030515e   1c                   endif
094840030515      * ...e scarto il numero righe necessario
094850030515      *   (se non sono già da pagina nuova;
094860030515      *    se sono da pagina nuova: ci penserà poi la subr. "Test_OF"
094870030515      *       alla prima riga in stampa)
094880030515if  1c                   if        wMinLine  < wMaxLine
094890080729do  2c                   dou       CurLineA4 >= wMinLine
094900030515     c                   write     A4prtNUL
094910030515e   2c                   enddo
094920030515e   1c                   endif
094930030515      *
094940030515     c     ScNS_A5       tag
094950030515      *
094960030515      * Imposto l'indicatore per la stampa del titolo relativo all'
094970030515      *   elenco che segue (condiziona attributi di stampa diversi)
094980030515     c                   eval      *in39  = *on
094990050511      *
095000050511     c                   exsr      ListaScNSd
095010050511      *
095020050511     c                   ENDSR
095030050511      *
095040050511      *---------------------------------------------------------------*
095050050511      *?Prepara elenco per lista - SegnaColli Non Sequenziali (dett.)?*
095060050511      *---------------------------------------------------------------*
095070050511     c     ListaScNSd    BEGSR
095080030515      *
095090030515      * Stampo elenco dei segnacolli non sequenziali:
095100071008if  1c                   IF             DB0tbo  = 'P'
095110071008     c                             or   DB0tbo  = '8'
095120030515      * - per bolle in Partenza
095130030515     c                   exsr      ScNS_BLT
095140030515x   1c                   ELSE
095150081007      * - per bolle in Arrivo
095160030515     c                   exsr      ScNS_ART
095170030515e   1c                   ENDIF
095180030515      *
095190030515      * Alla fine: stampo eventuali segnacolli in elenco (non stampati)
095200030515      *              in foglio di formato "A4"
095210030515      *            imposto eventuali segnacolli in elenco (non messi in
095220030515      *              stampa tra le particolarità) in foglio "A5"
095230030515if  1c                   if        XX     > *zeros
095240030515     c                   eval      XX     = *hival
095250030515     c                   exsr      StampaScNS
095260030515e   1c                   endif
095270030910      *
095280030910      * Se sto stampando su formato pagina "A4": reimposto *in70, *in71
095290030910      *   e *in72 (spenti prima della stampa dell'elenco dei segnacolli
095300030910      *   non sequenziali) perchè saranno utilizzati per il posiziona-
095310030910      *   mento in nuova pagina (nella subr. "Test_OF") e per la stampa
095320040223      *   dei box per DPD, FedEx e quelli da FIAR5.
095330050513if  1c                   if             not *in69
095340030910     c                             and  not *in70
095350080922     c*** DPD non più in A4:       and  not *in71
095360030910     c                             and  not *in72
095370080912     c*** EEX non più gestita:     and  not *in73
095380030910     c                   movea     SAVin7x       *in(70)
095390030910e   1c                   endif
095400030515      *
095410030515     c                   ENDSR
095420030515      *
095430030515      *---------------------------------------------------------------*
095440050217      *?Prepara elenco SegnaColli Non Sequenziali da FNBLT (partenza)?*
095450030515      *---------------------------------------------------------------*
095460030515     c     ScNS_BLT      BEGSR
095470030515      *
095480030515if  1c                   if        not %open(FNBLT01L)
095490030515     c                   open      FNBLT01L
095500030515e   1c                   endif
095510030515      *
095520030515      * memorizzo l'ultimo numero di segnacollo
095530070514if  1c                   if        $Mamma = *off
095540040607     c     KLDV          setgt     FNBLT000
095550040607     c     KLDV          readpe    FNBLT000
095560070514x   1c                   else
095570070514     c     KLBL          setgt     FNBLT000
095580070514     c     KLBL          readpe    FNBLT000
095590070514e   1c                   endif
095600030515if  1c                   if        NOT %eof(FNBLT01L)
095610080922     c                   eval      %subst(Pnsx : 8) = ','
095620080922     c                                              + %editc(BLTnsc : 'Z')
095630030515e   1c                   endif
095640030515      *
095650030515      * memorizzo il primo numero di segnacollo
095660070514if  1c                   if        $Mamma = *off
095670040607     c     KLDV          setll     FNBLT000
095680040607     c     KLDV          reade     FNBLT000
095690070514x   1c                   else
095700070514     c     KLBL          setll     FNBLT000
095710070514     c     KLBL          reade     FNBLT000
095720070514e   1c                   endif
095730030515if  1c                   if        NOT %eof(FNBLT01L)
095740080922     c                   eval      %subst(Pnsx : 1 : 7) =
095750080922     c                                 %editc(BLTnsc : 'Z')
095760030515e   1c                   endif
095770030515      *
095780030515      * se ci sono solo 2 segnacolli non sequenziali:
095790030515      *   stanno già al posto del range - basta non stampare il msg
095800030515      *   ("NON SEQUENZIALI")
095810030515if  1c                   if        DB0ncl <= 2
095820050516     c                   eval      *in39  =  *off
095830030515     c                   goto      EndScNS_BLT
095840030515e   1c                   endif
095850030515      *
095860030515      * ciclo di lettura per il recupero di tutti i numeri di
095870030515      *   segnacollo
095880030515do  1c                   DOW       NOT %eof(FNBLT01L)
095890030515     c                   eval      Wnsc   =  BLTnsc
095900030515     c                   exsr      StampaScNS
095910070514if  2c                   if        $Mamma = *off
095920040607     c     KLDV          reade     FNBLT000
095930070514x   2c                   else
095940070514     c     KLBL          reade     FNBLT000
095950070514e   2c                   endif
095960030515e   1c                   ENDDO
095970030515      *
095980030515     c     EndScNS_BLT   ENDSR
095990030515      *
096000030515      *---------------------------------------------------------------*
096010050217      *?Prepara elenco SegnaColli Non Sequenziali da FNART (arrivo)  ?*
096020030515      *---------------------------------------------------------------*
096030030515     c     ScNS_ART      BEGSR
096040030515      *
096050070319if  1c                   if        not %open(FNART01L)
096060070319     c                   open      FNART01L
096070070319e   1c                   endif
096080030515      *
096090030515      * memorizzo l'ultimo numero di segnacollo
096100070514if  1c                   if        $Mamma = *off
096110040607     c     KLDV          setgt     FNART000
096120040607     c     KLDV          readpe    FNART000
096130070514x   1c                   else
096140070514     c     KLBL          setgt     FNART000
096150070514     c     KLBL          readpe    FNART000
096160070514e   1c                   endif
096170030515if  1c                   if        NOT %eof(FNART01L)
096180080922     c                   eval      %subst(Pnsx : 8) = ','
096190080922     c                                              + %editc(ARTnsc : 'Z')
096200030515e   1c                   endif
096210030515      *
096220030515      * memorizzo il primo numero di segnacollo
096230070514if  1c                   if        $Mamma = *off
096240040607     c     KLDV          setll     FNART000
096250040607     c     KLDV          reade     FNART000
096260070514x   1c                   else
096270070514     c     KLBL          setll     FNART000
096280070514     c     KLBL          reade     FNART000
096290070514e   1c                   endif
096300030515if  1c                   if        NOT %eof(FNART01L)
096310080922     c                   eval      %subst(Pnsx : 1 : 7) =
096320080922     c                                 %editc(ARTnsc : 'Z')
096330030515e   1c                   endif
096340030515      *
096350030515      * se ci sono solo 2 segnacolli non sequenziali:
096360030515      *   stanno già al posto del range - basta non stampare il msg
096370030515      *   ("NON SEQUENZIALI")
096380030515if  1c                   if        DB0ncl <= 2
096390050516     c                   eval      *in39  =  *off
096400030515     c                   goto      EndScNS_ART
096410030515e   1c                   endif
096420030515      *
096430030515      * ciclo di lettura per il recupero di tutti i numeri di
096440030515      *   segnacollo
096450030515do  1c                   DOW       NOT %eof(FNART01L)
096460030515     c                   eval      Wnsc   =  ARTnsc
096470030515     c                   exsr      StampaScNS
096480070514if  2c                   if        $Mamma = *off
096490040607     c     KLDV          reade     FNART000
096500070514x   2c                   else
096510070514     c     KLBL          reade     FNART000
096520070514e   2c                   endif
096530030515e   1c                   ENDDO
096540030515      *
096550030515     c     EndScNS_ART   ENDSR
096560030515      *
096570030515      *---------------------------------------------------------------*
096580050217      *?Stampa elenco SegnaColli Non Sequenziali in fogli "A4" o "A5"?*
096590030515      *---------------------------------------------------------------*
096600030515     c     StampaScNS    BEGSR
096610030515      *
096620030515      * In prima riga stampo la "intestazione"
096630030515if  1c                   if        *in39
096640050513if  2c                   if        *in69   or
096650050513     c                             $A4_x_ScNS = *off
096660050513      * - foglio "A5" oppure
096670050513      *   foglio "A4" in "NOTE E PARTICOLARITA'"
096680030515     c                   eval      YY      = YY  + 1
096690030515     c                   eval      $NP(yy) = C_ScNS + ':'
096700030515x   2c                   else
096710030515      * - foglio "A4"
096720030515     c                   eval      Prig  = C_ScNS
096730030515     c                                   + '         (dal '
096740080922     c                                   + %trim( %subst(Pnsx: 1: 7) )
096750030515     c                                   + ' al '
096760080922     c                                   + %trim( %subst(Pnsx: 9: 7) )
096770030515     c                                   + ')'
096780030515     c                   exsr      StampaGEN
096790030515e   2c                   endif
096800080922     c                   eval      Pnsx  =  'NON SEQUENZIALI'
096810030515     c                   eval      *in39 = *off
096820030515     c                   clear                   XX
096830030515     c                   clear                   $SC1
096840030515     c                   clear                   $SC2
096850030515e   1c                   endif
096860030515      *
096870030515      * Al riempimento della riga: stampo la parte di elenco segnacolli
096880030515if  1c                   if        XX  >= WelSNS
096890030515     c                   eval      $SC2 = %editc($SC1 : 'Z')
096900050513if  2c                   if        *in69   or
096910050513     c                             $A4_x_ScNS = *off
096920050513      * - foglio "A5"  oppure
096930050513      *   foglio "A4" in "NOTE E PARTICOLARITA'"
096940030515     c                   eval      YY      = YY  + 1
096950030515     c                   movea(p)  $SC2          $NP(yy)
096960030515x   2c                   else
096970030515      * - foglio "A4"
096980030911     c                   movea(p)  $SC2          Prig
096990030515     c                   exsr      StampaGEN
097000030515e   2c                   endif
097010030515if  2c                   if        XX  <  *hival
097020030515     c                   clear                   XX
097030030515e   2c                   endif
097040030515     c                   clear                   $SC1
097050030515     c                   clear                   $SC2
097060030515e   1c                   endif
097070030515      *
097080030515      * Aggiorno la nuova parte di elenco segnacolli
097090030515      *  (se non richiesta la stampa degli ultimi segnacolli non
097100030515      *   sequenziali residui - memorizzati ma non ancora stampati)
097110030515if  1c                   if        XX  <  *hival
097120030515     c                   eval      XX = xx  + 1
097130030515     c                   eval      $SC1(xx) = Wnsc
097140030515e   1c                   endif
097150030515      *
097160030515     c                   ENDSR
097170040607      *
097180040607      *---------------------------------------------------------------*
097190050217      *?Stampa della parte finale della Lettera di Vettura           ?*
097200040607      *---------------------------------------------------------------*
097210040607     c     ChiusuraLDV   BEGSR
097220080605      *
097230080605      * Posizionamento sul foglio A4 in base al n° pagina in stampa
097240130418     c                   eval      *in33 = *on
097250130418     c                   exsr      Posiz_Box
097260070307      *
097270070307      * Il tagliandino "Copia per il Destinatario" va ora stampato SE:
097280070307      * · spedizione NON in stato "lasciato avviso"   oppure
097290070307      * · spedizione in stato "lasciato avviso", ma P.A. con fattura
097300070307      *   immediata
097310040607      *
097320080703      * 2ª parte - copia per il destinatario (da lasciare al cliente)
097330040607      *   (Se NON è ristampa in arrivo per recapito c/assegni - vedi
097340040607      *   costante "Non utilizzare per la consegna")
097350130418      *                o per il mittente - se Porto Franco Prepagato
097360040607      * > "cornice" / barcode / righine varie in parte inferiore
097370040607      *
097380080604sel 1c                   select
097390080604w   1c                   when           $No_x_Cons         =  *off
097400070307     c                             and ($LascAvv           =  *off
097410070406     c                              or (%subst(DB0tb1:1:1) =  'A'
097420070406     c                              and DB0nft             >  *zeros))
097430130418     c                   write     A4prtLDV
097440130418     c                   write     A4prtBOX
097450110426     c*//                write     A4LogoBAR
097460110426      /free
097470120530         //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
097480120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
097490120530         //  // >?Simil-Logo Bartolini/Brt?
097500120530         //  write  A4LogoBR2;
097510120530         //  write  A4LogoDA2;
097520120530         //else;
097530120530         //  // >?Simil-Logo Bartolini?
097540120530         //  write  A4LogoBAR;
097550120530         //  write  A4LogoDA1;
097560120530         //endif;
097570120530         // >?Simil-Logo BRT?
097580120530         write  A4LogoBRT;
097590120530         write  A4LogoDA3;
097600110426      /end-free
097610080604w   1c                   when           $LascAvv           =  *on
097620070406     c                             and (%subst(DB0tb1:1:1) <> 'A'
097630070406     c                              or  DB0nft             =  *zeros)
097640070307     c                   eval      Pmsg3  =  Tla
097650070307     c                   write     A4prtLDVB
097660120113w   1c                   when      $No_x_Cons =  *on
097670120113w   1c*//                   when      $No_x_Cons =  '2'
097680120113     c*//                   eval      Pmsg3  =  ' NON UTILIZZARE PER'
097690120113     c*//                                    +  ' LASCIATO AVVISO -'
097700120113     c*//                                    +  ' SERVIZIO POSTE'
097710120113     c*//                   write     A4prtLDVB
097720080604e   1c                   endsl
097730040607      *
097740040607     c                   ENDSR
097750030515      *
097760030515      *---------------------------------------------------------------*
097770050217      *?Carico LDV da FNBLP00F                                       ?*
097780030515      *---------------------------------------------------------------*
097790030515     c     CarBLP        BEGSR
097800030515      *
097810030515      * Apertura files
097820030515     c                   if        not %open(FNBLP01L)
097830030515     c                   open      FNBLP01L
097840030515     c                   endif
097850030515     c                   if        not %open(FNARB01L)
097860030515     c                   open      FNARB01L
097870030515     c                   endif
097880070319     ***c v.sr sr_Start     if        not %open(FIAR401L)
097890070319     ***c " "     "         open      FIAR401L
097900070319     ***c " "     "         endif
097910030515     c                   if        not %open(FIAR601L)
097920030515     c                   open      FIAR601L
097930030515     c                   endif
097940030515     c                   if        not %open(FIAR701L)
097950030515     c                   open      FIAR701L
097960030515     c                   endif
097970060505     c                   if        not %open(FIAR901L)
097980060505     c                   open      FIAR901L
097990030515     c                   endif
098000071114      *
098010071114     c                   clear                   DB0cpi
098020030515      *
098030040607     c     KLDV          chain     FNBLP000
098040030515      *
098050030515if  1c                   if        NOT %found(FNBLP01L)
098060081117     c                   leavesr
098070030515e   1c                   endif
098080120509      *
098090120509      * Memorizzazione dei dati di FNBLP che serviranno anche dopo
098100120509      *   il reperimento dell'eventuale bolla mamma...
098110120509     c                   eval      wB0piv =  BLPcpi
098120030515      *
098130030515      * 1ª impostaz. campi della DS in base a quelli del file FNBLP
098140030515     c                   exsr      sr_DS_BLP
098150030515      *
098160070308      * Decodifico filiale di Arrivo
098170030515     c                   exsr      DecLNA
098180030515      *
098190030515      * 2ª impostaz. campi della DS in base a quelli delle tabelle
098200030515      *    "generiche" (3A, TB, 5E)
098210030515     c                   exsr      sr_DS_TAB
098220030515      *
098230030515      * Ultimi dati mancanti:
098240061030      * - Cod. cliente destinatario: imposto LNA9999
098250030515      *    SE   in Assegnato   O   c'è 2ª bolla
098260030515if  1c                   if           %subst(DB0tb1:1:1) = 'A'
098270030515     c                             or DB0tb2            <> *blanks
098280030515     c                   eval      DB0ccd = *all'9'
098290030515     c                   movel     BLPlna        DB0ccd
098300030515e   1c                   endif
098310120509      *
098320120509      * - Reperimento Data spedizione
098330120509if  1c                   if        DB0mgs <> *zeros
098340120509     c                   z-add     DB0mgs        Wdsp
098350120509     c                   movel     DB0aas        Wdsp
098360120509x   1c                   else
098370120509      *   prendo la data del giorno se non c'è la data spedizione
098380120509     c                   time                    w0140
098390120509     c                   move      w0140         w0080
098400120509     c     *eur          movel     w0080         Data_EUR
098410120509     c     *iso          movel     Data_EUR      Wdsp
098420120509e   1c                   endif
098430030515      *
098440061030      * 3ª impostaz. campi della DS: importi da calcolare
098450061031     c                   exsr      sr_DS_IMPO
098460030515      *
098470081117     c                   ENDSR
098480030515      *
098490030515      *---------------------------------------------------------------*
098500050217      *?Carico LDV da FNARB00F                                       ?*
098510030515      *---------------------------------------------------------------*
098520030515     c     CarARB        BEGSR
098530030515      *
098540030515      * Apertura files
098550030515     c                   if        not %open(FNARB01L)
098560030515     c                   open      FNARB01L
098570030515     c                   endif
098580070319     c                   if        NOT %open(FIARN11L)
098590070319     c                   open      FIARN11L
098600070319     c                   endif
098610030515     c                   if        not %open(FIAR601L)
098620030515     c                   open      FIAR601L
098630030515     c                   endif
098640030515     c                   if        not %open(FIAR701L)
098650030515     c                   open      FIAR701L
098660030515     c                   endif
098670060505     c                   if        not %open(FIAR901L)
098680060505     c                   open      FIAR901L
098690030515     c                   endif
098700071114      *
098710071114     c                   clear                   DB0cpi
098720030515      *
098730040607     c     KLDV          chain     FNARB01L
098740030515      *
098750030515if  1c                   if        NOT %found(FNARB01L)
098760070312     c                   clear                   ARBndc
098770070322     c                   clear                   ARBntc
098780070322     c                   clear                   ARBdcp
098790071015     c                   clear                   ARBpdc
098800101129     c                   clear                   ARBdcm
098810101129     c                   clear                   ARBicc
098820101129     c                   clear                   ARBica
098830120509     c                   clear                   ARBcpi
098840081117     c                   leavesr
098850030515e   1c                   endif
098860070514      *
098870070514      * Memorizzazione dei dati di FNARB che serviranno anche dopo
098880070514      *   il reperimento dell'eventuale bolla mamma...
098890070514     c                   eval      wB0ndc =  ARBndc
098900070514     c                   eval      wB0ntc =  ARBntc
098910070514     c                   eval      wB0dcp =  ARBdcp
098920081119     c                   eval      wB0tmc =  ARBtmc
098930071015     c                   move      ARBpdc        Wpdc4a
098940101125     c                   eval      wB0dcm =  ARBdcm
098950101125     c                   eval      wB0icc =  ARBicc
098960101125     c                   eval      wB0ica =  ARBica
098970120509     c                   eval      wB0piv =  ARBcpi
098980030515      *
098990030515      * 1ª impostaz. campi della DS in base a quelli del file FNARB
099000030515     c                   exsr      sr_DS_ARB
099010030515      *
099020070308      * Decodifico filiale di Arrivo
099030030515     c                   exsr      DecLNA
099040030515      *
099050030515      * 2ª impostaz. campi della DS in base a quelli delle tabelle
099060030515      *    "generiche" (3A, TB, 5E)
099070030515     c                   exsr      sr_DS_TAB
099080030515      *
099090030515      * Ultimi dati mancanti:
099100030515      * - Cod. cliente mittente
099110030515if  1c                   If        ARBccm <> *zeros
099120030515     c                   movel     ARBccm        DB0ccm
099130030515x   1c                   Else
099140030515if  2c                   if        %subst(DB0tb1:1:1) = 'F'
099150030515     c                   movel     ARBksc        DB0ccm
099160030515e   2c                   endif
099170030515e   1c                   EndIf
099180030515      * - Cod. cliente destinatario
099190030515if  2c                   if        %subst(DB0tb1:1:1) = 'A'
099200030515     c                   movel     ARBksc        DB0ccd
099210030515e   1c                   endif
099220030515      * - Se c'è 2ª bolla imposto LNA9999
099230030515if  1c                   if        DB0tb2 <> *blanks
099240030515     c                   eval      DB0ccd = *all'9'
099250030515     c                   movel     ARBlna        DB0ccd
099260030515e   1c                   endif
099270120509      * - Reperimento Data spedizione
099280120509if  1c                   if        DB0mgs <> *zeros
099290120509     c                   z-add     DB0mgs        Wdsp
099300120509     c                   movel     DB0aas        Wdsp
099310120509x   1c                   else
099320120509      *   prendo la data del giorno se non c'è la data spedizione
099330120509     c                   time                    w0140
099340120509     c                   move      w0140         w0080
099350120509     c     *eur          movel     w0080         Data_EUR
099360120509     c     *iso          movel     Data_EUR      Wdsp
099370120509e   1c                   endif
099380030515      *
099390061030      * 3ª impostaz. campi della DS: importi da calcolare
099400061031     c                   exsr      sr_DS_IMPO
099410030515      *
099420081117     c                   ENDSR
099430030515      *
099440030515      *---------------------------------------------------------------*
099450050217      *?1ª impostaz. campi della DS in base a quelli del file FNBLP  ?*
099460030515      *---------------------------------------------------------------*
099470030515     c     sr_DS_BLP     BEGSR
099480030515      *
099490030515     c                   eval      DB0cbo = BLPcbo
099500030515     c                   eval      DB0tsp = BLPtsp
099510030515     c                   eval      DB0mgs = BLPmgs
099520030515     c                   eval      DB0tc1 = BLPtc1
099530030515     c                   eval      DB0tc2 = BLPtc2
099540030515     c                   eval      DB0fns = BLPfns
099550030515     c                   eval      DB0rmn = BLPrmn
099560030515     c                   eval      DB0rma = BLPrma
099570030515     c                   eval      DB0ias = BLPias
099580030515     c                   eval      DB0vas = BLPvas
099590030515     c                   eval      DB0vmd = BLPvmd
099600030515     c                   eval      DB0vad = BLPvad
099610030515     c                   eval      DB0qft = BLPqft
099620030515     c                   eval      DB0lna = BLPlna
099630030515     c                   eval      DB0znc = BLPznc
099640030515     c                   eval      DB0rsm = BLPrsm
099650030515     c                   eval      DB0inm = BLPinm
099660030515     c                   eval      DB0cam = BLPcam
099670030515     c                   eval      DB0lom = BLPlom
099680030515     c                   eval      DB0prm = BLPprm
099690030515     c                   eval      DB0nzm = BLPnzm
099700030515     c                   eval      DB0fap = BLPfap
099710030515     c                   eval      DB0rmo = BLPrmo
099720030515     c                   eval      DB0rsd = BLPrsd
099730030515     c                   eval      DB0ind = BLPind
099740030515     c                   eval      DB0cad = BLPcad
099750030515     c                   eval      DB0lod = BLPlod
099760030515     c                   eval      DB0prd = BLPprd
099770030515     c                   eval      DB0nzd = BLPnzd
099780030515     c                   eval      DB0fin = BLPfin
099790030515     c                   eval      DB0gc1 = BLPgc1
099800030515     c                   eval      DB0gc2 = BLPgc2
099810030515     c                   eval      DB0nas = BLPnas
099820030515     c                   eval      DB0ncl = BLPncl
099830030515     c                   eval      DB0pkf = BLPpkf
099840030515     c                   eval      DB0fvf = BLPfvf
099850030515     c                   eval      DB0vlf = BLPvlf
099860030515     c                   eval      DB0fls = BLPfls
099870030515     c                   eval      DB0ncd = BLPncd
099880030515     c                   eval      DB0nca = BLPnca
099890030515     c                   eval      DB0xco = BLPxco
099900030515     c                   eval      DB0ffd = BLPffd
099910030515     c                   eval      DB0dcr = BLPdcr
099920030515     c                   eval      DB0hcr = BLPhcr
099930030515     c                   eval      DB0tcr = BLPtcr
099940030515     c                   eval      DB0fbr = BLPfbr
099950030515     c                   eval      DB0gga = BLPgga
099960030515     c                   eval      DB0gma = BLPgma
099970030515     c                   eval      DB0gva = BLPgva
099980030515     c                   move      ' '           DB0gva
099990030515     c                   eval      DB0sop = BLPsop
100000030515     c                   eval      DB0ctr = BLPctr
100010030515     c                   eval      DB0drt = BLPdrt
100020030515     c                   eval      DB0fpp = BLPfpp
100030030515     c                   eval      DB0nrt = BLPnrt
100040030515     c                   eval      DB0pdr = BLPpdr
100050030515      *
100060030515      * Cod. cliente mittente
100070040311     c                   move      BLPksc        w0040
100080030515if  1c                   IF        BLPccm <> *zeros
100090030515     c                   movel     BLPccm        DB0ccm
100100030515x   1c                   ELSE
100110030515      *   Lo imposto solo se non è codice generico destinatario
100120040311if  2c                   if        w0040  <> 9999
100130030515     c                   movel     BLPksc        DB0ccm
100140030515x   2c                   else
100150030515     c                   movel     BLPksc        DB0ccd
100160030515     c                   movel     BLPlna        DB0ccd
100170030515e   2c                   endif
100180030515e   1c                   ENDIF
100190030515      *
100200030515      * Allega documento di trasporto
100210081119     c                   clear                   DB0bam
100220060505if  1c                   if           BLPfst = 'S'
100230030515     c                             or BLPfst = 'C'
100240030515     c                   movel     'S'           DB0bam
100250060505e   1c                   endif
100260060524      *
100270060524      * Se bolla di RESO: Codice cliente da Assegnato
100280061107     c*** (già fatto:)   clear                   dsBL4A
100290060524if  1c                   if        DB0fbr = 'S'
100300060524     c                   exsr      RepAR4_A
100310060524e   1c                   endif
100320030515      *
100330030515      * Destinatario: vedo se esiste 2ª ragione sociale
100340061107     c                   clear                   DB0rd2
100350061107     c                   exsr      RepAR4_D
100360030515      *
100370030515      * Note
100380061107     c                   clear                   DB0nt1
100390061107     c                   clear                   DB0nt2
100400061107     c                   exsr      RepAR4_89
100410030515      *
100420030515     c                   ENDSR
100430030515      *
100440030515      *---------------------------------------------------------------*
100450050217      *?1ª impostaz. campi della DS in base a quelli del file FNARB  ?*
100460030515      *---------------------------------------------------------------*
100470030515     c     sr_DS_ARB     BEGSR
100480030515      *
100490030515     c                   eval      DB0cbo = ARBcbo
100500030515     c                   eval      DB0tsp = ARBtsp
100510030515     c                   eval      DB0mgs = ARBmgs
100520030515     c                   eval      DB0tc1 = ARBtc1
100530030515     c                   eval      DB0tc2 = ARBtc2
100540030515     c                   eval      DB0fns = ARBfns
100550030515     c                   eval      DB0rmn = ARBrmn
100560030515     c                   eval      DB0rma = ARBrma
100570030515     c                   eval      DB0ias = ARBias
100580030515     c                   eval      DB0vas = ARBvas
100590030515     c                   eval      DB0vmd = ARBvmd
100600030515     c                   eval      DB0vad = ARBvad
100610030515     c                   eval      DB0qft = ARBqft
100620030515     c                   eval      DB0lna = ARBlna
100630030515     c                   eval      DB0znc = ARBznc
100640030515     c                   eval      DB0rsm = ARBrsm
100650030515     c                   eval      DB0inm = ARBinm
100660030515     c                   eval      DB0cam = ARBcam
100670030515     c                   eval      DB0lom = ARBlom
100680030515     c                   eval      DB0prm = ARBprm
100690030515     c                   eval      DB0nzm = ARBnzm
100700030515     c                   eval      DB0fap = ARBfap
100710030515     c                   eval      DB0rmo = ARBrmo
100720030515     c                   eval      DB0rsd = ARBrsd
100730030515     c                   eval      DB0ind = ARBind
100740030515     c                   eval      DB0cad = ARBcad
100750030515     c                   eval      DB0lod = ARBlod
100760030515     c                   eval      DB0prd = ARBprd
100770030515     c                   eval      DB0nzd = ARBnzd
100780030515     c                   eval      DB0fin = ARBfin
100790030515     c                   eval      DB0gc1 = ARBgc1
100800030515     c                   eval      DB0gc2 = ARBgc2
100810030515     c                   eval      DB0nas = ARBnas
100820030515     c                   eval      DB0ncl = ARBncl
100830030515     c                   eval      DB0pkf = ARBpkf
100840030515     c                   eval      DB0fvf = ARBfvf
100850030515     c                   eval      DB0vlf = ARBvlf
100860030515     c                   eval      DB0fls = ARBfls
100870030515     c                   eval      DB0ncd = ARBncd
100880030515     c                   eval      DB0nca = ARBnca
100890030515     c                   eval      DB0xco = ARBxco
100900030515     c                   eval      DB0ffd = ARBffd
100910030515     c                   eval      DB0dcr = ARBdcr
100920030515     c                   eval      DB0hcr = ARBhcr
100930030515     c                   eval      DB0tcr = ARBtcr
100940030515     c                   eval      DB0fbr = ARBfbr
100950030515     c                   eval      DB0gga = ARBgga
100960030515     c                   eval      DB0gma = ARBgma
100970030515     c                   eval      DB0gva = ARBgva
100980030515     c                   move      ' '           DB0gva
100990030515     c                   clear                   DB0sop
101000030515     c                   eval      DB0ctr = ARBctr
101010030515     c                   clear                   DB0drt
101020030515     c                   clear                   DB0fpp
101030030515     c                   clear                   DB0nrt
101040030515     c                   clear                   DB0pdr
101050030515      *
101060030515      * Allega documento di trasporto
101070060524      * & Codice cliente da Assegnato
101080061107     c*** (già fatto:)   clear                   dsBL4A
101090031029     c                   clear                   DB0bam
101100061107     c                   exsr      RepAR4_A
101110060505     c                   if        §B4Abm = 'S' or §B4Abm = 'C' or
101120031029     c                             §B4Abm = 'P' or §B4Abm = 'Q'
101130030515     c                   movel     'S'           DB0bam
101140060505     c                   endif
101150030515      *
101160030515      * Destinatario: vedo se esiste 2ª ragione sociale
101170061107     c                   clear                   DB0rd2
101180061107     c                   exsr      RepAR4_D
101190030515      *
101200030515      * Note
101210061107     c                   clear                   DB0nt1
101220061107     c                   clear                   DB0nt2
101230061107     c                   exsr      RepAR4_89
101240030515      *
101250030515     c                   ENDSR
101260030515      *
101270030515      *---------------------------------------------------------------*
101280050217      *?2ª impostaz. campi della DS in base a quelli delle tabelle   ?*
101290050217      *?   "generiche" (non da "1A")                                 ?*
101300030515      *---------------------------------------------------------------*
101310030515     c     sr_DS_TAB     BEGSR
101320030515      *
101330030515      * Carico le tabelle "generiche" (3A, TB, 5E)
101340030515     c                   exsr      CarTAB
101350030515      *
101360030515      * Codici bolla
101370030515     c                   eval      DB0tb1 = §3Atb1
101380030515     c                   eval      DB0tb2 = §3Atb2
101390030515     c                   eval      DB0dcb = §3Ades
101400030515      * Tipo servizio
101410030515if  1c                   if        DB0dts = *blanks
101420030515     c                   eval      DB0dts = §5Edes
101430030515     c                   eval      DB0fst = §5Efst
101440030515e   1c                   endif
101450030515      * Consegne particolari
101460030515if  1c                   if            DB0tc1 <> *blanks
101470030515     c                             and DB0dc1 =  *blanks
101480030515     c                   movel(p)  DB0tc1        TBLkey
101490030515     c                   exsr      Tab_1P
101500030515     c                   eval      DB0dc1 = §1Pdes
101510030515e   1c                   endif
101520030515if  1c                   if            DB0tc2 <> *blanks
101530030515     c                             and DB0dc2 =  *blanks
101540030515     c                   movel(p)  DB0tc2        TBLkey
101550030515     c                   exsr      Tab_1P
101560030515     c                   eval      DB0dc2 = §1Pdes
101570030515e   1c                   endif
101580030515      * Esenzione IVA
101590030515if  1c                   if            DB0cei <> *blanks
101600030515     c                             and DB0dce =  *blanks
101610030515     c                   eval      DB0dce =  §EIdes
101620030515e   1c                   endif
101630030515      *
101640030515     c                   ENDSR
101650060524      *
101660060524      *---------------------------------------------------------------*
101670060524      *?Reperimento dati da FIAR400F x trk "A"                       ?*
101680061107      *?(dati aggiuntivi per P.Assegnato)                            ?*
101690060524      *---------------------------------------------------------------*
101700060524     c     RepAR4_A      BEGSR
101710060524      *
101720061107     c*** (già fatto:)   clear                   dsBL4A
101730061107      *
101740060524     c                   eval      Wtrc = 'A'
101750060524     c     KLDVtrc       chain     FIAR4000
101760060524      *
101770060524if  1c                   if        %found(FIAR401L)
101780060524     c                   movel     AR4Not        dsBL4A
101790060524e   1c                   endif
101800060524      *
101810060524     c                   ENDSR
101820061107      *
101830061107      *---------------------------------------------------------------*
101840061107      *?Reperimento dati da FIAR400F x trk "D"                       ?*
101850061107      *?(2ª ragione sociale del destinatario)                        ?*
101860061107      *---------------------------------------------------------------*
101870061107     c     RepAR4_D      BEGSR
101880061107      *
101890061107     c*** NON esiste:    clear                   dsBL4D
101900061107     c*** (già fatto:)   clear                   DB0rd2
101910061107      *
101920061107     c                   eval      Wtrc = 'D'
101930061107     c     KLDVtrc       chain     FIAR4000
101940061107      *
101950061107if  1c                   if        %found(FIAR401L)
101960061107     c                   movel     AR4not        DB0rd2
101970061107e   1c                   endif
101980061107      *
101990061107     c                   ENDSR
102000171117      *//*
102010171117      *//*---------------------------------------------------------------*
102020171117      *//*?Reperimento dati da FIAR400F x trk "I"                       ?*
102030171117      *//*?(dati DPD)                                                   ?*
102040171117      *//*---------------------------------------------------------------*
102050171117     c*//  RepAR4_I      BEGSR
102060061030      *
102070061107     c*** (già fatto:)   clear                   dsBL4I
102080061107      *
102090171117     c*//                eval      Wtrc = 'I'
102100171117     c*//  KLDVtrc       chain     FIAR4000
102110061030      *
102120171117if  1c*//                if        %found(FIAR401L)
102130171117     c*//                movel     AR4not        dsBL4I
102140171117e   1c*//                endif
102150061030      *
102160171117     c*//                ENDSR
102170071008      *
102180071015      *---------------------------------------------------------------*
102190071015      *?Reperimento dati da FIAR400F x trk "M"                       ?*
102200071015      *?(dati aggiuntivi per O.R.M. abbinato al P.Franco PrePagato)  ?*
102210071015      *---------------------------------------------------------------*
102220071015     c     RepAR4_M      BEGSR
102230071015      *
102240071015     c*** (già fatto:)   clear                   dsBL4M
102250071015      *
102260071015     c                   eval      Wtrc = 'M'
102270071015     c     KLDVtrc       chain     FIAR4000
102280071015      *
102290071015if  1c                   if        %found(FIAR401L)
102300071015     c                   movel     AR4Not        dsBL4M
102310071015e   1c                   endif
102320071015      *
102330071015     c                   ENDSR
102340061107      *
102350061107      *---------------------------------------------------------------*
102360061107      *?Reperimento dati da FIAR400F x trk "Q"                       ?*
102370061107      *?(Codice Fiscale del mittente a sx & del destinatario a dx)   ?*
102380061107      *---------------------------------------------------------------*
102390061107     c     RepAR4_Q      BEGSR
102400061107      *
102410061108     c*** (già fatto:)   clear                   dTA4Q
102420061107     c*** (già fatto:)   clear                   DB0cpi
102430061107      *
102440061107     c                   eval      Wtrc = 'Q'
102450061107     c     KLDVtrc       chain     FIAR4000
102460061107      *
102470061107if  1c                   if        %found(FIAR401L)
102480061108     c                   movel     AR4not        dTA4Q
102490061108e   1c                   endif
102500061107      *
102510061107     c                   ENDSR
102520061107      *
102530061107      *---------------------------------------------------------------*
102540061107      *?Reperimento dati da FIAR400F x trk "8" / "9"                 ?*
102550061107      *?(note)                                                       ?*
102560061107      *---------------------------------------------------------------*
102570061107     c     RepAR4_89     BEGSR
102580061107      *
102590061107     c*** NON esiste:    clear                   dsBL48
102600061107     c*** NON usata:     clear                   dsBL49
102610061107     c*** (già fatto:)   clear                   DB0nt1
102620061107     c*** (già fatto:)   clear                   DB0nt2
102630061107      *
102640061107     c                   eval      Wtrc = '8'
102650061107     c     KLDVtrc       chain     FIAR4000
102660061107if  1c                   if        %found(FIAR401L)
102670061107     c                   movel     AR4not        DB0nt1
102680061107e   1c                   endif
102690061107      *
102700061107     c                   eval      Wtrc = '9'
102710061107     c     KLDVtrc       chain     FIAR4000
102720061107if  1c                   if        %found(FIAR401L)
102730061107     c                   movel     AR4not        DB0nt2
102740061107e   1c                   endif
102750061107      *
102760061107     c                   ENDSR
102770171117      *
102780171117      *---------------------------------------------------------------*
102790171117      *?Reperimento dati da FIPND00F                                 ?*
102800171117      *?(dati DPD)                                                   ?*
102810171117      *---------------------------------------------------------------*
102820171117     c     RepFIPND      BEGSR
102830171117      *
102840171117if  1c                   if        NOT  %open(FIPND01L)
102850171117     c                   open      FIPND01L
102860171117e   1c                   endif
102870171117      *
102880171117     c     kLDV          chain     FIPND000
102890171117      *
102900171117if  1c                   if        NOT %found(FIPND01L)
102910171117     c                   clear                   PNDipn
102920171117e   1c                   endif
102930171117      *
102940171117     c                   ENDSR
102950030515      *
102960030515      *---------------------------------------------------------------*
102970050217      *?Carico le tabelle che servono: 3A/TB/5E.                     ?*
102980030515      *---------------------------------------------------------------*
102990030515     c     CarTAB        BEGSR
103000030515      *
103010030515      * Tab. "3A" - Codici Bolla
103020030515      * (NO se rimesso mittente)
103030030515     c                   clear                   ds3A
103040030515if  1c                   if        DB0ris <> 'M'
103050030515     c                   movel     '3A'          TBLcod
103060030515     c                   movel(p)  DB0cbo        TBLkey
103070030515     c     K03TAB        chain     TABEL
103080030515if  2c                   if        %found(TABEL00F)
103090030515     c                   movel     TBLuni        ds3A
103100030515e   2c                   endif
103110030515e   1c                   endif
103120070515     c                   eval      $Mamma =  (§3Abcm = 'S')
103130030515      *
103140030515      * Tab. "TB" - Tipi Bolla
103150030515      * (NO se rimesso mittente)
103160030515      * se Bolla in assegnato, con tipo bolla vedo se da contabilizzare
103170030515      * se no non stampo i dati fattura
103180030515     c                   clear                   dsTB
103190030515if  1c                   if        DB0ris <> 'M'
103200030515     c                   movel     'TB'          TBLcod
103210061030if  2c                   if            §3Atb2 <> *blanks
103220121029     c                             and DB0tbo <> 'P'
103230030515     c                             and DB0tbo <> '8'
103240030515     c                   movel(p)  §3Atb2        TBLkey
103250030515x   2c                   else
103260030515     c                   movel(p)  §3Atb1        TBLkey
103270030515e   2c                   endif
103280030515     c     K03TAB        chain     TABEL
103290030515if  2c                   if        %found(TABEL00F)
103300030515     c                   movel     TBLuni        dsTB
103310030515e   2c                   endif
103320030515e   1c                   endif
103330030515      *
103340030515      * Tab. "5E" - Tipi Servizio
103350030515     c                   clear                   ds5E
103360090513if  1c                   if        DB0tsp <> *blanks
103370030515     c                   movel     '5E'          TBLcod
103380030515     c                   movel(p)  DB0tsp        TBLkey
103390030515     c     K03TAB        chain     TABEL
103400030515if  2c                   if        %found(TABEL00F)
103410030515     c                   movel     TBLuni        ds5E
103420030515e   2c                   endif
103430030515e   1c                   endif
103440030515      *
103450030911      * Tab. "EI" - Esenzioni I.V.A.
103460030515     c                   clear                   dsEI
103470030911      * Tab. "1A" - Tipi incasso contrassegno
103480030911     c                   clear                   ds1A
103490030911      * Tab. "1P" - Consegne particolari
103500030911     c                   clear                   ds1P
103510150304      *//* Tab. "TBA" - Tipi bancale
103520150304     c*//                clear                   dTBA
103530030515      *
103540030515     c                   ENDSR
103550030911      *
103560030911      *---------------------------------------------------------------*
103570050217      *?Carico tabella "EI" (Esenzione I.V.A.)                       ?*
103580030911      *---------------------------------------------------------------*
103590030911     c     Tab_EI        BEGSR
103600030911      *
103610030911     c                   clear                   dsEI
103620030911if  1c                   if        DB0cei <> *blanks  and  DB0dce = *blanks
103630030911     c                   movel     'EI'          TBLcod
103640030911     c                   movel(p)  DB0cei        TBLkey
103650030911     c     K03TAB        chain     TABEL
103660030911if  2c                   if        %found(TABEL00F)
103670030911     c                   movel     TBLuni        dsEI
103680030911e   2c                   endif
103690030911e   1c                   endif
103700030911     c                   eval      DB0dce =  §EIdes
103710030911      *
103720030911     c                   ENDSR
103730030515      *
103740030515      *---------------------------------------------------------------*
103750050217      *?Carico tabella "1A" (Tipo incasso contrassegno)              ?*
103760030515      *---------------------------------------------------------------*
103770030515     c     Tab_1A        BEGSR
103780030515      *
103790030515     c                   clear                   ds1A
103800030515if  1c                   if        DB0cas <> *zeros
103810030515     c                   movel     '1A'          TBLcod
103820030515     c                   movel(p)  DB0tic        TBLkey
103830030515     c     K03TAB        chain     TABEL
103840030515if  2c                   if        %found(TABEL00F)
103850030515     c                   movel     TBLuni        ds1A
103860030515e   2c                   endif
103870030515e   1c                   endif
103880030515      *
103890030515      * Imposto i dati nella DS
103900030515     c                   eval      DB0dti = §1Ades
103910030515     c                   eval      DB0ucm = §1Aucm
103920030515     c                   eval      DB0ste = §1Aste
103930030515      *
103940030515     c                   ENDSR
103950030515      *
103960030515      *---------------------------------------------------------------*
103970050217      *?Carico tabella "1P" (Consegne particolari)                   ?*
103980030515      *---------------------------------------------------------------*
103990030515     c     Tab_1P        BEGSR
104000030515      *
104010030515     c                   clear                   ds1P
104020030515     c                   movel     '1P'          TBLcod
104030030515     c*** già impostato: movel(p)  DB0tc1/2      TBLkey
104040030515     c     K03TAB        chain     TABEL
104050030515if  1c                   if        %found(TABEL00F)
104060030515     c                   movel     TBLuni        ds1P
104070030515e   1c                   endif
104080030515      *
104090030515     c                   ENDSR
104100030515      *
104110150304      *//*---------------------------------------------------------------*
104120150304      *//*?Reperisco tabella "TBA" (Tipi Bancale)                       ?*
104130150304      *//*---------------------------------------------------------------*
104140150304     c*//  Tab_TBA       BEGSR
104150150304      *//*
104160150304     c*//                clear                   dTBA
104170150304     c*//                reset                   TIBS02ds
104180150304     c*//                eval      T02cod = 'TBA'
104190150304     c*//                movel(p)  Wtba          T02ke1
104200150304     c*//                call      'TIBS02R'
104210150304     c*//                parm                    KPJBA
104220150304     c*//                parm                    TIBS02ds
104230150304if  1c*//                if        T02err = *blanks
104240150304     c*//                movel     T02uni        dTBA
104250150304e   1c*//                endif
104260150304      *//*
104270150304     c*//                ENDSR
104280030515      *
104290030515      *---------------------------------------------------------------*
104300050217      *?Calcolo importi LDV                                          ?*
104310030515      *---------------------------------------------------------------*
104320061031     c     sr_DS_IMPO    BEGSR
104330030515      *
104340030515      * Vedo se esiste tassazione
104350030515      *  solo se tipo bolla contabilizzabile
104360061030if  1c                   IF        §TBfcb =  '1'
104370030515     c
104380061030     c                   eval      Wtrc   =  '1'
104390040607     c     KLDVtrc       chain     FIAR6000
104400030515     c
104410030515if  2c                   if        %found(FIAR601L)
104420030515     c                   movel     AR6div        DB0div
104430030515     c                   exsr      CtrDIV
104440030515      *
104450030515      * NON c'è 2ª bolla
104460061031if  3c                   if            DB0tb2 = *blanks
104470121029     c                             or  DB0tbo = 'P'
104480061031     c                             or  DB0tbo = '8'
104490030515      *  Estremi fattura
104500030515     c                   z-add     AR6nft        DB0nft
104510030515     c                   z-add     AR6dft        DB0dft
104520130124     c                   z-add     AR6fiv        wB0fiv
104530061031      *  Se è assegnato o franco prepagato => stampo tutta la tassaz.
104540061031if  4c                   if            %subst(DB0tb1:1:1) = 'A'
104550061031     c                             or  DB0tbo = '8'
104560061030     c                   eval      Wtrc   =  '1'
104570030515     c                   exsr      RepTASSAZ
104580030515e   4c                   endif
104590030515e   3c                   endif
104600030515e   2c                   endif
104610030515      *
104620061030      * C'è 2ª bolla
104630061031if  2c                   IF            DB0tb2 <> *blanks
104640121029     c                             and DB0tbo <> 'P'
104650061031     c                             and DB0tbo <> '8'
104660030515      *
104670061030     c                   eval      Wtrc   =  '2'
104680040607     c     KLDVtrc       chain     FIAR6000
104690030515      *
104700061030if  3c                   If        %found(FIAR601L)
104710030515     c                   movel     AR6div        DB0div
104720030515     c                   exsr      CtrDIV
104730030515     c                   movel     AR6cei        DB0cei
104740030515      *   Estremi fattura
104750030515     c                   z-add     AR6nft        DB0nft
104760030515     c                   z-add     AR6dft        DB0dft
104770130124     c                   z-add     AR6fiv        wB0fiv
104780030515if  4c                   if        AR6ksc <> *zeros
104790030515     c                   eval      DB0ccd =  AR6ksc
104800030515e   4c                   endif
104810061030     c                   eval      Wtrc   =  '2'
104820030515     c                   exsr      RepTASSAZ
104830061030e   3c                   EndIf
104840030515      *
104850030515e   2c                   ENDIF
104860030515      *
104870030515e   1c                   ENDIF
104880030515      *
104890030515      * Vedo se ha contrassegno
104900060505     c     KLDV          chain     FIAR9000
104910060505if  1c                   IF        %found(FIAR901L)
104920061030     c                   eval      DB0cas =  AR9cas
104930061030     c                   eval      DB0vca =  AR9vca
104940061030     c                   eval      DB0gca =  AR9gca
104950061030     c                   eval      DB0tic =  AR9tic
104960030515x   1c                   ELSE
104970030515     c                   clear                   DB0cas
104980030515     c                   clear                   DB0vca
104990030515     c                   clear                   DB0gca
105000030515     c                   clear                   DB0tic
105010030515     c                   clear                   DB0dti
105020030515     c                   clear                   DB0ucm
105030030515     c                   clear                   DB0ste
105040030515e   1c                   ENDIF
105050120509      *
105060120509      * Se P.F. Prepagato verificherò il Codice Fiscale del Mittente
105070120509      *   (NON se mittente estero)
105080120509if  1c                   if           ( DB0tbo  = '8'
105090120509     c                             and (DB0nft  <> *zeros
105100120509     c                              or  DB0ift  <> *zeros)
105110120509     c                             and  DB0nzm  =  *blanks )
105120120509     c                   exsr      RepAR4_Q
105130120509     c                   movel     §TA4Qcfm      DB0cpi
105140120509e   1c                   endif
105150121029      * Se P.A. con fattura immediata o con 2ª bolla verificherò il
105160121029      *   Codice Fiscale (NON se destinatario estero)
105170121026if  1c*//                if           ( DB0tbo  = 'A'
105180121029if  1c                   if          ( (%subst(DB0tb1:1:1) = 'A'
105190121029     c                             or  (DB0tb2  <> *blanks
105200121029     c                             and  DB0tbo  <> 'P'
105210121029     c                             and  DB0tbo  <> '8') )
105220120509     c                             and (DB0nft  <> *zeros
105230120509     c                              or  DB0ift  <> *zeros)
105240120509     c                             and  DB0nzd  =  *blanks )
105250120509     c                   exsr      RepAR4_Q
105260120509     c                   movel     §TA4Qcfd      DB0cpi
105270120509e   1c                   endif
105280120509     c                   eval      wB0cfi = DB0cpi
105290061030      *
105300030515      * Vedo se si tratta di bolla figlia:
105310040607      * Se bolla con MAMMA prendo c/a e segnacolli da bolla MAMMA
105320070515if  1c                   if        $Mamma =  *on
105330081117     c                   exsr      BollaFiglia
105340030515e   1c                   endif
105350030515      *
105360030515     c                   ENDSR
105370030515      *
105380030515      *---------------------------------------------------------------*
105390050217      *?Decodifica LNA                                               ?*
105400030515      *---------------------------------------------------------------*
105410030515     c     DecLNA        BEGSR
105420030515      *
105430140307if  1c*//                if            DB0dla =  *blanks
105440140307     c*//                          and DB0ris <> 'M'
105450140307if  1c                   if        DB0ris <> 'M'
105460030515      *
105470040311     c                   eval      SAVlna = DB0lna
105480070312     c                   clear                   OG146arr
105490061016     c                   clear                   OG148arr
105500070312     c                   clear                   OG150arr
105510030515     c     DB0lna        chain     AZORG
105520030515      *
105530030515if  2c                   if        %found(AZORG01L)
105540030515     c                   eval      DB0fag = ORGfag
105550030515     c                   eval      DB0dla = ORGdes
105560030515     c                   eval      DB0inl = ORGind
105570030515     c                   eval      DB0cal = ORGcpf
105580030515     c                   eval      DB0lol = ORGloc
105590030515     c                   eval      DB0prl = ORGpro
105600030515     c                   eval      DB0tel = ORGtel
105610140307     c*//                eval      DB0fax = ORGfax
105620140307     c                   eval      DB0fax = ORGdd2
105630070312     c                   movel     ORGde6        OG146arr
105640061016     c                   movel     ORGde8        OG148arr
105650070312     c                   movel     ORGdf0        OG150arr
105660030515e   2c                   endif
105670030515      *
105680030515e   1c                   endif
105690030515      *
105700030515     c                   ENDSR
105710030515      *
105720030515      *---------------------------------------------------------------*
105730050217      *?Reperimento dati di tutta la tassazione per la stampa        ?*
105740030515      *---------------------------------------------------------------*
105750030515     c     RepTASSAZ     BEGSR
105760030515      *
105770030515     c                   eval      DB0imv = AR6imv
105780030515     c                   eval      DB0cei = AR6cei
105790030515     c                   eval      DB0ift = AR6ift
105800030911      * Esenzione IVA
105810030911if  1c                   if            DB0cei <> *blanks
105820030911     c                             and DB0dce =  *blanks
105830030911     c                   exsr      Tab_EI
105840030911e   1c                   endif
105850030515      *
105860030515      * Controllo se ci sono varie esenti
105870030515      *
105880030515      * - varia 1
105890030515     c                   eval      Wsvx = AR6sv1
105900030515     c                   eval      Wvax = AR6va1
105910030515     c                   exsr      RepTASSAZ_b
105920030515      * - varia 2
105930030515     c                   eval      Wsvx = AR6sv2
105940030515     c                   eval      Wvax = AR6va2
105950030515     c                   exsr      RepTASSAZ_b
105960030515      * - varia 3
105970030515     c                   eval      Wsvx = AR6sv3
105980030515     c                   eval      Wvax = AR6va3
105990030515     c                   exsr      RepTASSAZ_b
106000030515      * - varie ulteriori solo per tipo record 1 (prima bolla)
106010040607     c     KLDVtrc       setll     FIAR7000
106020040607     c     KLDVtrc       reade     FIAR7000
106030030515      *
106040030515do  2c                   Dow       Not %eof(FIAR701L)
106050030515      *
106060030515     c                   eval      Wsvx = AR7svn
106070030515     c                   eval      Wvax = AR7van
106080030515     c                   exsr      RepTASSAZ_b
106090030515      *
106100040607     c     KLDVtrc       reade     FIAR7000
106110030515e   2c                   Enddo
106120030515      *
106130030515     c                   ENDSR
106140030515      *
106150030515      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
106160050217      *?Reperimento dati di tutta la tassazione per la stampa        ?*
106170050217      *?- 2ª parte -                                                 ?*
106180030515      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
106190030515     c     RepTASSAZ_b   BEGSR
106200030515      *
106210030515sel 1c                   Select
106220030515      *
106230030515w   1c                   When      Wsvx       = *blanks
106240030515      *
106250030515w   1c                   When      Wsvx       = §$2iva
106260030515     c                   eval      DB0iva     = Wvax
106270030515      *
106280030515w   1c                   When      Wsvx       = §$2bol
106290030515     c                   add       Wvax          DB0ese
106300050624     c                   z-add     Wvax          wBolloVirt
106310050624     c                   eval      $BolloVirt = *on
106320030515      *
106330030515x   1c                   Other
106340050624     c                   eval      *in31      = *off
106350030515     c     Wsvx          lookup    $Var                                   31
106360030515     c   31              add       Wvax          DB0ese
106370030515     c   31              sub       Wvax          DB0imv
106380030515      *
106390030515w   1c                   Endsl
106400030515      *
106410030515     c                   ENDSR
106420030515      *
106430030515      *---------------------------------------------------------------*
106440050217      *?Chain su TABEL per le PARTICOLARITA'                         ?*
106450030515      *---------------------------------------------------------------*
106460030515     c     TABxPAR       BEGSR
106470030515      *
106480100513     c                   movel(p)  WCdPar        w053a
106490030515     c                   movel     '7V'          TBLcod
106500030515     c                   movel(p)  KeyPAR        TBLkey
106510030515     c     K03TAB        chain     TABEL
106520030515      *
106530030515if  1c                   if        %found(TABEL00F)
106540030515     c                             and TBLflg = *blanks
106550100513     c                   eval      w053a = WCdPar + '-'
106560100513     c                                   + %trim(%subst(TBLuni:1:49))
106570030515x   1c                   else
106580030515     c                   clear                   TBLuni
106590030515e   1c                   endif
106600030515      *
106610030515     c                   ENDSR
106620030515      *
106630030515      *---------------------------------------------------------------*
106640050217      *?CONTROLLO C/A SE E' DA CONVERTIRE IN EURO                    ?*
106650030515      *---------------------------------------------------------------*
106660030515     c     CtrCAs        BEGSR
106670030515      *
106680030515      * E' da convertire
106690030515if  1c                   if        DB0ucm = 'S'
106700030515     c                   eval      $CAeur = *on
106710030515e   1c                   endif
106720030515      *
106730030515      * Se in EURO è da convertire e da stampare in EURO
106740030515if  1c                   if        DB0vca = §GEDcn
106750030515     c                   eval      $CAeur = *on
106760030515     c                   endif
106770030515      *
106780030515      * Devo convertire il c/a da valuta a moneta di conto
106790030624if  1c                   if        DB0vca <> §GEDcn
106800030515     c                   clear                   YEURCODS
106810030515     c                   movel     DB0vca        YECdvi
106820030515     c                   z-add     DB0cas        YECimi
106830030515     c                   movel     §GEDcn        YECdvo
106840030515     c                   call      'YEURCO'
106850030515     c                   parm                    YEURCODS
106860030515if  2c                   if        YECesi = *blanks
106870030515     c                   z-add     YECimo        Wcas
106880030515e   2c                   endif
106890030515x   1c                   else
106900030515     c                   z-add     DB0cas        Wcas
106910030515e   1c                   endif
106920030515      *
106930030515     c                   ENDSR
106940030515      *
106950030515      *---------------------------------------------------------------*
106960050217      *?CONTROLLO DIVISA DELLA FATTURA                               ?*
106970030515      *---------------------------------------------------------------*
106980030515     c     CtrDIV        BEGSR
106990030515      *
107000030515     c                   clear                   dsCV
107010030515      *
107020030515     c                   movel     'CV'          TBLcod
107030030515     c                   movel(p)  DB0div        TBLkey
107040030515     c     K03TAB        chain     TABEL
107050030515     c                   if        %found(TABEL00F)
107060030515     c                   movel     TBLuni        dsCV
107070030515     c                   movel     §CVfdc        DB0fdc
107080030515     c                   endif
107090030515      *
107100030515     c                   ENDSR
107110030515      *
107120030515      *---------------------------------------------------------------*
107130050217      *?SE BOLLA CON MAMMA PRENDO C/A E SEGNACOLLI DA BOLLA MAMMA    ?*
107140030515      *---------------------------------------------------------------*
107150081117     c     BollaFiglia   BEGSR
107160030515      *
107170090528     c                   if        not %open(FNLBL01L)
107180090528     c                   open      FNLBL01L
107190090528     c                   endif
107200090528      *
107210040607     c     KLDV          chain     FNLBL000
107220030515if  1c                   if        %found(FNLBL01L)
107230030515      *
107240030515     c     KLBL          chain     FNARB000
107250030515if  2c                   if        %found(FNARB01L)
107260030515      *
107270031015     c                   eval      wB0nrs = ARBnrs
107280030515     c                   eval      DB0fns = ARBfns
107290030515     c                   eval      DB0fls = ARBfls
107300030515     c                   eval      DB0ncd = ARBncd
107310030515     c                   eval      DB0nca = ARBnca
107320050114     c**!!!              eval      DB0xco = ARBxco
107330030515     c                   eval      DB0znc = ARBznc
107340030515      * Aggancio AR9
107350060505     c     KLBL          chain     FIAR9000
107360060505if  3c                   IF        %found(FIAR901L)
107370030515     c                   eval      DB0cas = AR9cas
107380030515     c                   eval      DB0vca = AR9vca
107390030515     c                   eval      DB0gca = AR9gca
107400030515     c                   eval      DB0tic = AR9tic
107410030515x   3c                   ELSE
107420030515     c                   clear                   DB0cas
107430030515     c                   clear                   DB0vca
107440030515     c                   clear                   DB0gca
107450030515     c                   clear                   DB0tic
107460030515     c                   clear                   DB0dti
107470030515     c                   clear                   DB0ucm
107480030515     c                   clear                   DB0ste
107490030515e   3c                   ENDIF
107500030515      *
107510030515e   2c                   endif
107520030515      *
107530030515e   1c                   endif
107540030515      *
107550030515     c                   ENDSR
107560030903      *
107570030903      *---------------------------------------------------------------*
107580050217      *?Impostazione Quantità e Unità di Misura da FNORM             ?*
107590030903      *---------------------------------------------------------------*
107600030903     c     QUM_FNORM     BEGSR
107610030903      *
107620030905     c*** (già fatto:)   clear                   $Qum
107630030905     c                   clear                   Wx
107640030903      * Colli
107650030905if  1c                   if        ORMncl   > *zeros
107660030905     c                             and Wx   < %elem($Qum)
107670030905     c                   add       1             wx
107680030905     c                   eval      $Qum(wx) = %editc(ORMncl : '4')
107690030905     c                                      + ' colli'
107700030903e   1c                   endif
107710030903      * Peso
107720030905if  1c                   if        ORMpkg   > *zeros
107730030905     c                             and Wx   < %elem($Qum)
107740030905     c                   add       1             wx
107750030905     c                   eval      $Qum(wx) = %editc(ORMpkg : '4')
107760030905     c                                      + ' kg'
107770030905e   1c                   endif
107780071025      * Volume bancali
107790071025if  1c                   if        ORMvlm   > *zeros
107800071025     c                             and Wx   < %elem($Qum)
107810071025     c                   add       1             wx
107820071025     c                   eval      $Qum(wx) = %editc(ORMvlm : '4')
107830071025     c                                      + ' mc'
107840071025e   1c                   endif
107850030905     *** * Volume
107860030905     *** * (in ultimo posto SOLO SE non ci sono bilici/motrici/autotreni)
107870030905if  1***c                   if        ORMvlm   > *zeros
107880030905     ***c                             and (Wx  < %elem($Qum) - 1
107890030905     ***c                              or  Wx  = %elem($Qum) - 1
107900030905     ***c                              and w0112 = *zeros)
107910030905     ***c                   add       1             wx
107920030905     ***c                   eval      $Qum(wx) = %editc(ORMvlm : '4')
107930030905     ***c                                      + ' metri cubi'
107940030905e   1***c                   endif
107950030905     *** * Bancali
107960030905     *** * (in ultimo posto SOLO SE non ci sono bilici/motrici/autotreni)
107970030905if  1***c                   if        ORMbnc   > *zeros
107980030905     ***c                             and (Wx  < %elem($Qum) - 1
107990030905     ***c                              or  Wx  = %elem($Qum) - 1
108000030905     ***c                              and w0112 = *zeros)
108010030905     ***c                   add       1             wx
108020030905     ***c                   eval      $Qum(wx) = %editc(ORMbnc : '4')
108030030905     ***c                                      + ' bancali'
108040030905e   1***c                   endif
108050030905     *** * Bilici
108060030905if  1***c                   if        ORMblc  > *zeros
108070030905     ***c                             and Wx  < %elem($Qum)
108080030905     ***c                   add       1             wx
108090030905     ***c                   eval      $Qum(wx) = %editc(ORMblc : '4')
108100030905     ***c                                      + ' bilici'
108110030905e   1***c                   endif
108120030905     *** * Motrici
108130030905if  1***c                   if        ORMmtc  > *zeros
108140030905     ***c                             and Wx  < %elem($Qum)
108150030905     ***c                   add       1             wx
108160030905     ***c                   eval      $Qum(wx) = %editc(ORMmtc : '4')
108170030905     ***c                                      + ' motrici'
108180030905e   1***c                   endif
108190030905     *** * Autotreni
108200030905if  1***c                   if        ORMatt  > *zeros
108210030905     ***c                             and Wx  < %elem($Qum)
108220030905     ***c                   add       1             wx
108230030905     ***c                   eval      $Qum(wx) = %editc(ORMatt : '4')
108240030905     ***c                                      + ' autotreni'
108250030905e   1***c                   endif
108260030903      *
108270030903     c                   ENDSR
108280130306      *
108290130306      /free
108300130306
108310130306       //--------------------------------------------------------------
108320130306       //?Scrittura record (relativo alla spedizione) nel *file PDFLIST?
108330130306       //--------------------------------------------------------------
108340130306       BEGSR  sr_WrtRec_PdfList;
108350130306
108360130307         // -?NON richiesta stampa del PDF?
108370130710         if  %parms() = 3;
108380130306           leavesr;
108390130306         endif;
108400130306
108410130306         // -?Reperimento numero "chiave elenco"?(SE non ricevuto)?
108420130306         If  DB6num <= *zero;
108430130306
108440130306           clear  TRUL33ds;
108450130306           I33tla = 'L';
108460130306           I33ope = *zero;
108470130306           I33cnu = 55;
108480130306           I33num = 1;
108490130306           kpjbu  = TRUL33ds;
108500130306
108510130306           TRUL33R ( kpjba );
108520130306
108530130306           TRUL33ds = kpjbu;
108540130307           if  O33err > *zero;
108550130307             Pmsg1  = $Msg(3);
108560130307             Pmsg2  = $Msg(O33err + 3);
108570130418             *in33  = *off;
108580130418             exsr  Posiz_Box;
108590130307             write  A4prtMSG;
108600130307             DB0tms = 'E';
108610130306             exsr  sr_End;
108620130306           endif;
108630130306
108640130306           DB6num = %editc( O33nri : 'X' );
108650130306
108660130306         EndIf;
108670130307
108680130307         // -?Apertura file?
108690130307         if  Not %open(PDFLIST);
108700130307           open  PDFLIST;
108710130307         endif;
108720130306
108730130308         // -?Ciclo di lettura record "§..." (ex "§PDF") in FIAR801L?
108740130306         clear  k06fiar801;
108750130306         k_AR8aas = DB0aas;
108760130306         k_AR8lnp = DB0lnp;
108770130306         k_AR8nrs = DB0nrs;
108780130306         k_AR8nsp = DB0nsp;
108790130306         k_AR8tmd = c_§PDF;
108800130306         setll  %kds( k06fiar801 : 5 )  FIAR8000;
108810130308         reade  %kds( k06fiar801 : 4 )  FIAR8000;
108820130306
108830130308         DoW  Not %eof(FIAR801L)  and  %subst(AR8tmd : 1 : 1) = '§';
108840130306
108850130306           dAR8§PDF = AR8uni;
108860130308
108870130315           If  AR8atb = *blank  and  §AR8Ptrt = 'S';
108880130306
108890130308             // -?Scrittura del rec. relativo alla sped. nel *file PDFLIST?
108900130308             clear  RPDF;
108910130308             PDFklist  = DB6num;
108920130308             PDFksped  = %editc( DB0aas : 'X' ) + %editc( DB0lnp : 'X' ) +
108930130308                         %editc( DB0nrs : 'X' ) + %editc( DB0nsp : 'X' );
108940130311             PDFkey    = %trimr(§AR8PpdfP) + '_' + §AR8PpdfN;
108950130311             //PDFpath   = *blank;
108960130308             PDFinfo   = c_CopiaXdest + ' - ' + Prsd +
108970130308                         ' Spedizione ' + %editc(DB0lnp : 'X') +
108980130308                                    ' ' + %editc(DB0nrs : 'X') +
108990130308                                    ' ' + %editc(DB0nsp : 'X') +
109000130308                         ' del ' + %editc(Pdsp : 'Y') + ' ' +
109010130308                         Pndc;
109020130308             PDFresol  = 'S';
109030130308             PDFcopieM = '1';
109040130308             PDFcopieD = '0';
109050130308             PDFcopieS = '0';
109060130308             //PDFnoInfo = *blank;
109070130308             //PDFinfoP  = *blank;
109080130308             //PDFprtFst = *blank;
109090130308             wTime_ds  = %char( %timestamp() );
109100131008             PDFinsDt  = wTime_ds.wDate_Iso;
109110131008             PDFinsTm  = wTime_ds.wTime;
109120131008             //PDFprtDt  = wTime_ds.wDate_Iso;
109130131008             //PDFprtTm  = wTime_ds.wTime;
109140130308             PDFusrins = SDSusr;
109150130308             //__________
109160130308             write  RPDF;
109170130308             //¯¯¯¯¯¯¯¯¯¯
109180130308
109190130308           EndIf;
109200130306
109210130418           reade  %kds( k06fiar801 : 4 )  FIAR8000;
109220130306
109230130306         EndDo;
109240130306
109250130306       ENDSR;
109260130306
109270130306       //--------------------------------------------------------------
109280140307       //?Stampa immagini associate alle spediz. (dati già in PDFLIST) ?
109290130306       //--------------------------------------------------------------
109300130306       BEGSR  sr_PrtPdf;
109310130306
109320130306         // -?Impostazione parametri?
109330130312         PDF_KList    = DB6num;
109340130312         PDF_Printer  = trulPOVRds.§ULPoOutQ;
109350130312         PDF_FormType = trulPOVRds.§ULPoFType;
109360130312         PDF_UsrDta   = trulPOVRds.§ULPoUDta;
109370130306
109380130306         // -?Richiamo funzione di stampa di tutte le immagini PDF?
109390130312         //  ?(segnalando - in stampa - l'eventuale errore)?
109400130312         callp(E)  Stampa_PDF ( PDF_KList :
109410130312                                PDF_Printer :
109420130312                                PDF_FormType :
109430130312                                PDF_UsrDta );
109440130312
109450130312         if  %error;
109460130312           Pmsg2  = $Msg(7);
109470130418           *in33  = *off;
109480130418           exsr  Posiz_Box;
109490130312           write  A4prtMSG;
109500130312           DB0tms = 'E';
109510130312           exsr  sr_End;
109520130312         endif;
109530130307
109540130307         // -?Restituzione tipo modulo stampato?
109550130307         DB0tms = 'F';
109560130306
109570130306       ENDSR;
109580130306
109590130306      /end-free
109600030903      *
109610030515      *---------------------------------------------------------------*
109620050217      *?DEFINIZIONE KEY-LIST                                         ?*
109630030515      *---------------------------------------------------------------*
109640030515     c     DefKLIST      BEGSR
109650030515      *
109660051115      * - FNBLP01L / FNBLT01L /
109670060505      *   FNARB01L / FNART01L /
109680030515      *   FNLBL01L /
109690060505      *   FIAR501L/31C / FIAR901L
109700040607     c     KLDV          klist
109710030515     c                   kfld                    DB0aas
109720030515     c                   kfld                    DB0lnp
109730030515     c                   kfld                    DB0nrs
109740030515     c                   kfld                    DB0nsp
109750060505      * - FIAR401L / FIAR601L / FIAR701L
109760060505     c     KLDVtrc       klist
109770060505     c                   kfld                    DB0aas
109780060505     c                   kfld                    DB0lnp
109790060505     c                   kfld                    DB0nrs
109800060505     c                   kfld                    DB0nsp
109810060505     c                   kfld                    Wtrc
109820040607      * - FIAR501L/31C
109830030717     c     K05AR501      klist
109840030717     c                   kfld                    DB0aas
109850030717     c                   kfld                    DB0lnp
109860030717     c                   kfld                    DB0nrs
109870030717     c                   kfld                    DB0nsp
109880030717     c                   kfld                    AR5trd
109890040607      * - FIAR801L
109900040607     c     K06AR801      klist
109910040607     c                   kfld                    DB0aas
109920040607     c                   kfld                    DB0lnp
109930040607     c                   kfld                    DB0nrs
109940040607     c                   kfld                    DB0nsp
109950040607     c                   kfld                    AR8tmd
109960040607     c                   kfld                    AR8prr
109970070514      * - FNBLT01L / FNART01L
109980070514     c     KLDVnsc       Klist
109990070514     c                   Kfld                    DB0aas
110000070514     c                   Kfld                    DB0lnp
110010070514     c                   Kfld                    DB0nrs
110020070514     c                   Kfld                    DB0nsp
110030070514     c                   Kfld                    Wnsc
110040070319      * - FIARN11L
110050070319     c     K06ARN11      klist
110060070319     c                   kfld                    DB0aas
110070070319     c                   kfld                    DB0lnp
110080070319     c                   kfld                    DB0nrs
110090070319     c                   kfld                    DB0nsp
110100070319     c                   kfld                    ARNcdn
110110070319     c                   kfld                    ARNgst
110120090528      * - FNBLP01L/ / FNARB01L / FIAR901L di bolla MAMMA
110130070514     c     KLBL          klist
110140070514     c                   kfld                    LBLaap
110150070514     c                   kfld                    LBLlpp
110160070514     c                   kfld                    LBLnrp
110170070514     c                   kfld                    LBLnsp
110180070514      * - FNBLT01L / FNART01L di bolla MAMMA
110190070514     c     KLBLnsc       Klist
110200070514     c                   kfld                    LBLaap
110210070514     c                   kfld                    LBLlpp
110220070514     c                   kfld                    LBLnrp
110230070514     c                   kfld                    LBLnsp
110240070514     c                   Kfld                    Wnsc
110250070514      * - FIARS01L
110260070514     c     K05ARS01P     klist
110270070514     c                   kfld                    BLTfls
110280070514     c                   kfld                    BLTlna
110290070514     c                   kfld                    BLTnrs
110300070514     c                   kfld                    BLTnsc
110310070514     c                   kfld                    ARStrc
110320070514     c     K05ARS01A     klist
110330070514     c                   kfld                    ARTfls
110340070514     c                   kfld                    ARTlna
110350070514     c                   kfld                    ARTnrs
110360070514     c                   kfld                    ARTnsc
110370070514     c                   kfld                    ARStrc
110380060505      * - FNORM01L
110390060505     c     K04ORM01      klist
110400071015     c                   kfld                    ORMpoe
110410071015     c                   kfld                    ORMnsr
110420071015     c                   kfld                    ORMnor
110430071015     c                   kfld                    ORMnrv
110440030515      * - TABEL00F
110450030515     c     K03TAB        klist
110460040223     c                   kfld                    TBLkut
110470030515     c                   kfld                    TBLcod
110480030515     c                   kfld                    TBLkey
110490030515     c     K02TAB        klist
110500040223     c                   kfld                    TBLkut
110510030515     c                   kfld                    TBLcod
110520030515      * - FLNUF01L
110530030515     c     K04NUF        klist
110540030515     c                   kfld                    NUFaaa
110550030515     c                   kfld                    NUFcnu
110560030515     c                   kfld                    NUFfil
110570030515     c                   kfld                    NUFfl2
110580030515      *
110590030515     c                   ENDSR
110600030515
110610030515** > $Msg ---------------------------------------------------------------------*
110620030515IMPOSSIBILE CONTINUARE LA RISTAMPA BOLLE
110630071112PERCHE' STAMPATE PIU' DI &NrLdV BOLLE !!!
110640130307ERRORE IN REPERIMENTO NUMERATORE PER STAMPA PDF ALLEGATO ALLA LDV:
110650130307   NON REPERITO IL NUMERATORE
110660130307   NUMERATORE ALLOCATO
110670130307   SUPERATO IL LIMITE MASSIMO PER IL NUMERATORE
110680130312RILEVATO ERRORE IN FASE DI STAMPA DEI DOCUMENTI ALLEGATI
110690140307** > $GgS/$Gio *
1107000306241LUNEDI
1107100306242MARTEDI
1107200410263MERCOLEDI
1107300306244GIOVEDI
1107400306245VENERDI
1107500305156SABATO
1107600305157DOMENICA
110770030515M          MATT.
110780030515P          POM.
