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
000840171116       // -?Bolle: estensione DPD?
000850171116     fFIPND01L  if   e           k disk    usropn
000860110411       // -?O.R.M.?
000870030903     fFNORM01L  if   e           k disk    usropn
000880140410     fFNORE01L  if   e           k disk    usropn
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
001140150226     f*//                                  rename(LSB6AR5B:A4prtAR5B)
001150150226     f*//                                  rename(LSB6AR5C:A4prtAR5C)
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
002660140915     d c_PosLin1OC3    c                   const(1.470)
002670080704     d c_PosLin1OD1    c                   const(1.010)
002680140917
002690140917       // -?Posizioni di altre costanti?
002700140917     d*// c_PosKBRTc      c                   const(0.970)     x FONT(00404)
002710140922     d c_PosKBRTc      c                   const(0.965)
002720140922     d*// c_PosKBRTc      c                   const(0.955)     x FONT(02305)
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
003560150429     d c_CliFedEx2     c                   const(0506514)
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
003780140129
003790140129       // -?Costanti per conversione caratteri maiuscoli/minuscoli?
003800140129     d c_Mai           c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
003810140129     d c_Min           c                   const('abcdefghijklmnopqrstuvwxyz')
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
004550171116     d*// dsBL4I        e ds                  inz
004560071015     d dsBL4M        e ds                  inz
004570061108     d dTA4Q         e ds                  inz
004580110427
004590110427       // -?Flag operativi di FNORM - campo ORMFLO?
004600110427     d dORM01        e ds                  inz
004610140410
004620140410       // -?Dati per trc "O " del file FNORE (orari apertura)?
004630140410     d dOREorari     e ds
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
004790150226     d*// dTBA          e ds                  inz
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
005260170517     d dCLI          e ds                  inz
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
005850140303     d Elem_CP         ds                  inz
005860140303     d   WCPdcm                       8s 0 inz
005870140303     d   WCPocm                       4s 0 inz
005880140303     d   WCPncl                       5s 0 inz
005890140303     d   WCPfir                            inz  like(EVB2dati)
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
006570131205     d wEVBdev         s                   like(EVBdev)   inz
006580131205     d wEVBhev         s                   like(EVBhev)   inz
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
006810140922     d Pdsp            s              8  0 inz
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
007290150226     d*// Wtba            s                   like(§AR5tba)  inz
007300080708
007310110427       // -?Comando da eseguire per QCMDEXC?
007320080708     d Qcmd            s            512    inz
007330080708
007340090302       //--------------------------------------------------------------
007350130306       //?Definizione prototipi procedure usate.                       ?
007360090302       //--------------------------------------------------------------
007370140915
007380140915       // -?Calcolo/Controllo BRTcode?
007390140915      /copy gaitrasrc/srcProtoPI,TRUL28R0
007400140915      /copy gaitrasrc/srcProtoPR,TRUL28R0
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
007830140303
007840140303       // -?File FNEVB22L?
007850140303     d keyFNEVB22    e ds                  extname(FNEVB22L : *key)
007860140303     d                                     prefix(k_)   inz
007870140410
007880140410       // -?File FNORE01L?
007890140410     d keyFNORE01    e ds                  extname(FNORE01L : *key)
007900140410     d                                     prefix(k_)
007910140410     d                                     inz
007920170517
007930170517       // -?File TFNTC01L?
007940170517     d keyTFNTC01    e ds                  extname(TFNTC01L : *key)
007950170517     d                                     prefix(k_)
007960170517     d                                     inz
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
008560140919      *§BOX1BJ = posiz. linea orizzontale fine   box BRTcode        ?*
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
010150130306     c                   WHEN      %parms() > 1  and  DB6pdf = 'S'
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
011520130306     c                   if        %parms() > 1  and  DB6pdf = 'X'
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
015440171116     c*//                clear                   dsBL4I
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
016030131205     c                   clear                   wEVBdev
016040131205     c                   clear                   wEVBhev
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
017010170718if  1c                   if        D55tfa <> ORGfil
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
018440170517         //if  §OGntw = 'DPD'   -  ?NON della LnP, MA della fil. mitt.:?
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
018680150429w   1c                   WHEN      DB0ccm = c_CliFedEx   or
018690150429     c                             DB0ccm = c_CliFedEx2
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
019140150224     *** * - con Bancali da rendere al mittente
019150150224     *** *   o   Bancali da riportare a magazzino
019160150224      * - con Destinatario supermercato
019170030515      *   e/o Promemoria per autotrasportatore
019180040223      *   [Se non trovo niente su FIAR5 né x supermercati né x bancali
019190030515      *    ma ho queste condizioni: devo stampare la bolla in "A4" per
019200030515      *    poter stampare comunque il box vuoto con la memo "Destina-
019210050510      *    tario Supermercato" per poter scrivere a mano i promemoria
019220050510      *    per l'autotrasportatore - ci dovrebbero comunque sempre
019230050510      *    essere note per supermercato.]
019240050510      *   e/o con formato A4 previsto in tab. "7R" (partic. consegna)
019250150224w   1c*//                WHEN         %subst(DB0gva:1:1) = 'B'
019260150224     c*//                          or %subst(DB0gva:1:1) = 'O'
019270150224w   1c                   WHEN                DB0tc1      = 'S'
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
022810131205      * - - salvataggio data/ora
022820131205     c                   eval      wEVBdev  = EVBdev
022830131205     c                   eval      wEVBhev  = EVBhev
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
023050170517           clear  k05tntbe01;
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
023180170517         clear  k05tntbe01;
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
023850130726     c*//                if        Not *in36     (NON ancora acceso)
023860130726if  1c                   if        DB0tbo <> '8'  or  DB0ris = 'M'
023870130726     c                   eval      Tcxd2  =  Tcxd
023880130726     c                   endif
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
024050030515     c                   eval      BARcdeLNP = DB0lnp
024060030515     c                   eval      BARcdeNRS = DB0nrs
024070030515     c                   eval      BARcdeNSP = DB0nsp
024080030515     c                   movel     BarCodeDS     I28cod
024090030515     c                   call      'TRUL28R1'
024100030515     c                   parm                    TRUL28DS
024110030515if  2c                   if        O28err =  *blanks
024120030515     c                   movel     O28cod        Tbcd
024130030515e   2c                   endif
024140140915      *
024150140915      /free
024160140915
024170140915         // -?BRTcode?
024180140915         reset  TRUL28ds0;
024190140915         trul28ds0.i280cod = BarCodeDS;
024200140915         trul28r0 ( TRUL28ds0 );
024210140915         if  trul28ds0.o280err <> *on;
024220140917           //TbrtC = trul28ds0.o280cod;
024230140917           TbrtC = %subst( trul28ds0.o280cod :  1 : 5 ) + ' ' +
024240140917                   %subst( trul28ds0.o280cod :  6 : 5 ) + ' ' +
024250140917                   %subst( trul28ds0.o280cod : 11 : 5 ) + ' ' +
024260140917                   %subst( trul28ds0.o280cod : 16 : 4 );
024270140915         endif;
024280140915
024290140915      /end-free
024300030515      *
024310030515      * - Sigla operatore video
024320030515     c                   eval      Psop   =  DB0sop
024330080721      /free
024340080721
024350130306         // -?Flag "allega documento di trasporto"?
024360130418         //   IN FUTURO: se al 1° tentativo di consegna
024370130418         //              (DB0FL1 = "D" - v. pgm FNLRS9R)
024380080721         select;
024390081119
024400130306           // -?LdV con DDT allegato oltre il 1° tentativo di consegna?
024410081127           when  DB0bam =  'S'   and   wB0tmc <> *blank;
024420081127             evalr Pddt =  'DDT sul collo';
024430081119
024440130306           // -?LdV con DDT allegato (al 1° tentativo di consegna)?
024450080723           when  DB0bam =  'S';
024460080723             evalr Pddt =  'DDT';
024470081119
024480130306           // -?Esiste packing-list abbinata?
024490130306           when  $FIAR8 =  *on   and   $§PDF =  *off;
024500081013           //if  DB0fl1 <> 'D';
024510081013               evalr Pddt =  'PACKING-LIST';
024520081013           //else;
024530081013           //  evalr Pddt =  'PACKING-LIST GIA'' CONSEGNATA';
024540081013           //endif;
024550130306
024560130306           // -?LdV con PDF allegato?
024570130306           when  $FIAR8 =  *off  and   $§PDF =  *on;
024580130306             evalr Pddt =  'PACKING-LIST PDF';
024590130306
024600130306           // -?LdV con packing-list abbinata & PDF allegato?
024610130306           when  $FIAR8 =  *on   and   $§PDF =  *on;
024620130308             evalr Pddt =  'PACKING-LIST PDF';
024630081119
024640130306           // -?LdV senza PDF allegato & senza packing-list abbinata?
024650080721           other;
024660080721             clear Pddt;
024670081119
024680080721         endsl;
024690080721
024700080721      /end-free
024710030515      *
024720030515      * - Mittente
024730170517     c                   if        *in71
024740170517     c                   exsr      sr_Mitt_DPD
024750170517     c                   endif
024760170517      *
024770030515     c                   if        DB0ccm <> *zeros
024780030515     c                   move      DB0ccm        Pccm
024790030515     c                   endif
024800030515     c                   eval      Prsm   =  DB0rsm
024810030515     c                   eval      Pinm   =  DB0inm
024820030515     c                   eval      Pcam   =  DB0cam
024830030515     c                   eval      Plom   =  DB0lom
024840030515     c                   eval      Pprm   =  DB0prm
024850030515     c                   eval      Pnzm   =  DB0nzm
024860030515      *   Mittente originale
024870030515if  1c                   if        DB0rmo <> *blanks
024880030515     c                   eval      Pkmo   =  'MITT. ORIGINALE'
024890030515     c                   eval      Prmo   =  DB0rmo
024900030515e   1c                   endif
024910030515      *
024920070308      * - Filiale di Arrivo
024930030515if  1c                   if        DB0ris <> 'M'
024940080721     c                   eval      Pkla   =  'FILIALE'
024950170717     c                   eval      Plna1  =  %editc(wDB0lnaCf£6 : 'X')
024960080722     c                                    +  ' '
024970170717     c                                    +  wDB0dlaCf£6
024980080722     c                                    +  ' - '
024990170717     c                                    +  wDB0inlCf£6
025000140129     c*//                                 +  ' Tel.'
025010140129     c                                    +  '     '
025020170717     c                                    +  %trimr(wDB0telCf£6)
025030170717     c                   eval      Plna2  =  %editc(wDB0calCf£6 : 'X')
025040080722     c                                    +  ' '
025050170717     c                                    +  wDB0lolCf£6
025060080722     c                                    +  ' '
025070170717     c                                    +  wDB0prlCf£6
025080140129     c*//                                 +  '                  '
025090140129     c*//                                 +  ' Fax '
025100170717     c*//                                 +  %trimr(wDB0faxCf£6)
025110170717     c                   if        wDB0faxCf£6 <> *blanks
025120170717     c     c_Mai:c_Min   xlate     wDB0faxCf£6   DB0fax
025130140129     c                   eval      Plna2  =  %trimr(Plna2)
025140140129     c                                    +  '          '
025150140129     c*//*                                +  ' E-mail: '
025160140129     c                                    +  '         '
025170170717     c                                    +  %trimr(wDB0faxCf£6) + '@brt.it'
025180140129     c                   endif
025190030515e   1c                   endif
025200030515      *
025210030515      * - Destinatario
025220030515if  1c                   if        DB0ccd <> *zeros
025230030515     c                   move      DB0ccd        Pccd
025240030515e   1c                   endif
025250030515     c                   eval      Prsd   =  DB0rsd
025260030515     c                   eval      Prse   =  DB0rd2
025270030515     c                   eval      Pind   =  DB0ind
025280030515     c                   eval      Pcad   =  DB0cad
025290030515     c                   eval      Plod   =  DB0lod
025300030515     c                   eval      Pprd   =  DB0prd
025310030515     c                   eval      Pnzd   =  DB0nzd
025320030515      *
025330030515      * - Riferimenti mittente
025340030515     c                   eval      Prmn   =  DB0rmn
025350030515     c                   eval      Prma   =  DB0rma
025360171116      *//* se bolla DPD recupero il riferimento da FIAR4 rcd "I"
025370171116      * se bolla DPD recupero il riferimento da FIPND00F
025380171116     c*//*** (già fatto:)   clear                   dsBL4I
025390060626     c                   if        *in71
025400171116     c*//                exsr      RepAR4_I
025410171116     c                   exsr      RepFIPND
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
025600140922     c     *dmy          movel     Data_EUR      Pdsp6
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
027240170413if  1c*//                if        DB0gva =  'P '
027250170413     c*//                evalr     Ppad = 'PRIVATO'
027260170413e   1c*//                endif
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
029370170517      *
029380170517      /free
029390170517       //--------------------------------------------------------------
029400170517       //?Controllo SE ragione sociale Mittente da FORZARE (TFNTC) per ?
029410170517       //?spedizione DPD.                                              ?
029420170517       //--------------------------------------------------------------
029430170517       BEGSR  sr_Mitt_DPD;
029440170517
029450170517         // -?Reperim. abilitazioni del Mittente dalla tab. "CLI"?
029460170517         clear  k05tntbe01;
029470170517         k_TBEcod = 'CLI';
029480170517         k_TBEke1 = %editc( DB0ccm : 'X' );
029490170517         chain  %kds( k05tntbe01 )  TNTBE000;
029500170517         if  Not %found( TNTBE01L )  or  TBEatb <> *blank;
029510170517           leavesr;
029520170517         endif;
029530170517         dCLI = TBEuni;
029540170517         // -?Cliente SENZA forzatura Mittente da note "AM" e "AS"?
029550170517         if  §CLItfntc <> 'S'  and  §CLInoIMtt <> 'S';
029560170517           leavesr;
029570170517         endif;
029580170517
029590170517         // -?Forzatura Mittente da note "AM"?
029600170517         If  §CLItfntc = 'S';
029610170517
029620170517           if  NOT %open( TFNTC01L );
029630170517             open  TFNTC01L;
029640170517           endif;
029650170517           clear  keyTFNTC01;
029660170517           k_NTCapl = 'C';
029670170517           k_NTCnk1 = %editc( DUTkci : 'X' ) + %editc( DB0ccm : 'X' );
029680170517           k_NTCtnt = 'AM';
029690170517           chain  %kds( keyTFNTC01 )  TFNTC;
029700170517           if  %found( TFNTC01L )  and  NTCsns = 'S';
029710170517             DB0rsm = NTCrnt;
029720170517           endif;
029730170517
029740170517         EndIf;
029750170517
029760170517         // -?Pulizia Indirizzo Mittente?
029770170517         If  §CLInoIMtt = 'S';
029780170517
029790170517           clear  DB0inm;
029800170517           clear  DB0cam;
029810170517           clear  DB0lom;
029820170517           clear  DB0prm;
029830170517           clear  DB0nzm;
029840170517
029850170517         EndIf;
029860170517
029870170517       ENDSR;
029880170517
029890170517      /end-free
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
030760150224     *** *     - se 'B' (bancali da rendere al mittente) comparirà un
030770150224     *** *       apposito box
030780150224     *** *     - se 'O' (oltre 20 colli => bancali da riportare in mag.)
030790150224     *** *       comparirà un apposito box, un'apposita nota e l'indica-
030800150224     *** *       zione nella copia per il destinatario.
030810030717      *     ...Potrebbero anche non esserci più elementi di schiera
030820030717      *     disponibili [xx >= %elem($NP)] !
030830030709if  1c                   IF            DB0gva <> *blanks
030840150224     c*//                          and DB0gva <> 'B '
030850150224     c*//                          and DB0gva <> 'O '
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
031650140304     c*//                eval      $CP(xx)     = *zeros
031660140304     c                   clear                   Elem_CP
031670140304     c                   movel     Elem_CP       $CP(xx)
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
031980140303      *
031990140303      /free
032000140303
032010140303       //--------------------------------------------------------------
032020140303       //?Reperimento nome firmatario                                  ?
032030140303       //--------------------------------------------------------------
032040140303       BEGSR  sr_Firmatario;
032050140303
032060140303if  1    if  NOT  %open(FNEVB22L);
032070140303           open  FNEVB22L;
032080140303e   1    endif;
032090140303
032100140303         clear  keyFNEVB22;
032110140303         k_EVB2aas = DB0aas;
032120140303         k_EVB2lnp = DB0lnp;
032130140303         k_EVB2nrs = DB0nrs;
032140140303         k_EVB2nsp = DB0nsp;
032150140303         k_EVB2cev = 'P  ';
032160140303         setll  %kds( keyFNEVB22 : 4 )  FNEVB200;
032170140303         reade  %kds( keyFNEVB22 : 4 )  FNEVB200;
032180140303         DoW  Not  %eof(FNEVB22L);
032190140303           if  EVB2cev = k_EVB2cev  and  EVB2trd = 'FIR';
032200140305             yy = %lookup( EVB2dev : $CPdcm );
032210140305             if  yy > *zero;
032220140305               Elem_CP = $CP(yy);
032230140303               if  WCPocm  <= EVB2hev;
032240140303                 WCPocm  = EVB2hev;
032250140303                 WCPfir  = EVB2dati;
032260140305                 $CP(yy) = Elem_CP;
032270140303               endif;
032280140303             endif;
032290140303           endif;
032300140304           reade  %kds( keyFNEVB22 : 4 )  FNEVB200;
032310140303         EndDo;
032320140303
032330140303       ENDSR;
032340140303
032350140303      /end-free
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
033110140513             clear  w0080;
033120140513             clear  w0140;
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
033450131205     c                   z-add     wEVBdev       w0080
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
033560131205     c                                    +  %editw(wEVBhev : '  :  ')
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
035440161012     c                             and ($FIAR8 =  *on
035450161012     c                              or  $§PDF  =  *on)
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
036220130726     c                   clear                   Tcxd2
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
038120140303       //?Reperimento Numero e Data del DDT originale del mittente     ?
038130140303       //?"Consorzio Lotterie Nazionali - Gratta & Vinci"              ?
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
038520140303       //?Da DS                                                        ?
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
039240140303       //?Stampa della sola Packing-List (2 per A4)                    ?
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
039520130726         //  Tcxd  = c_CopiaXdest;
039530130726         //  Tcxd2 = Tcxd;
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
040260130306         if  %parms() > 1  and  (DB6pdf = 'F'  or  DB6pdf = 'X')
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
040470081028      * > dati EuroExpress  (non più gestiti)
040480040223w   1c                   when      *in73
040490030515      *
040500030515e   1c                   endsl
040510030910      *
040520030910      * > elenco SEGNACOLLI NON SEQUENZIALI
040530050511      *  (no x max 2 perchè sono già stati elencati nell'apposita area)
040540030910      *  (no x DPD   perchè monocollo)
040550030910      *  (no x FedEx perchè segnacolliamo noi)
040560030910      *  (no x "A5"  perchè sono già stati elencati tra le "Note e
040570030910      *              Particolarità")
040580030910if  1c                   if        DB0fns =  'S'
040590050511     c                             and  DB0ncl > 2
040600050513     c                             and  $A4_x_ScNS  =   *on
040610030910     c***                          and  not *in71  and  not *in72
040620030910     c                             and  not *in69
040630030910     c                   exsr      ListaScNS
040640030910e   1c                   endif
040650031124      *
040660031124      * Reperimento della posizione utilizzabile per la stampa dei box
040670080825      *   (in pollici) in base all'ultima riga stampata in 1/8" o 1/9"
040680031124     c                   exsr      CalcRowPos
040690060524      *
040700070123      * > Spedizione con D.A.S. x cliente "PRAOIL OLEODOTTI ITALIANI"
040710081127if  1c                   if             DB0ccm = c_CliPraOil
040720060529     c                             or  (DB0fbr = 'S'
040730081127     c                             and  §B4Aks = c_CliPraOil)
040740060524     c                   exsr      StampaDAS
040750060524e   1c                   endif
040760031124      *
040770031124      * > Promemoria per autotrasportatore
040780110502if  1c*//*               if            (DB0gma = Ktab7V_CB
040790110502     c*//*                           or DB0gma = Ktab7V_C1
040800080625     c*** scontato x GMA='C1'       and DB0bam = 'S'
040810110502     c*//*                           or DB0gma = Ktab7V_C2
040820080729     c*** scontato x GMA='C2'       and DB0bam = 'S'
040830110502     c*//*                           or DB0gma = Ktab7V_C3
040840100513     c*** scontato x GMA='C3'       and DB0bam = 'S'
040850110411     c*//*                           or DB0gma = Ktab7V_C4
040860110411     c*//*                           or DB0gma = Ktab7V_C5
040870110502     c*//*                           or DB0gma = Ktab7V_R1
040880110502     c*//*                           or DB0gma = Ktab7V_NR)
040890110502     c*//*                         and  §7Rldv = 'S'
040900110502     c                   if            (§7Rldv = 'S'
040910110502     c                             and  $TabPXA2  = *on)
040920061106     c                             OR   $No_CF = *on
040930071114     c                             OR   $No_PI = *on
040940070306     c                             OR   $FIARN = *on
040950070315     c                             OR   $C_A_  = *on
040960080801     c                             OR   $ConsParz = *on
040970090302     c                             OR   $TabPXA   = *on
040980110411     c                             OR   $TabPXA2  = *on
040990031124     c                   exsr      ListaPMxA
041000031124e   1c                   endif
041010030515      *
041020030515      * > dati FIAR5
041030150224if  1c*//                if           %subst(DB0gva:1:1) = 'B'
041040150224     c*//                          or %subst(DB0gva:1:1) = 'O'
041050150224if  1c                   if                  DB0tc1      = 'S'
041060030717     c                             or        DB0tc2      = 'S'
041070030515     c                   exsr      ListaAR5
041080030515e   1c                   endif
041090030910      *
041100071015      * > Riferimento O.R.M.
041110071015sel 1c                   select
041120071015      * - > per ritiro contestuale alla consegna
041130100608w   1c                   when          (§7Rrc  = 'S'
041140071015     c                             and  §7Rldv = 'S')
041150030910     c                   exsr      StampaRCC
041160071015      * - > per ritiro porto franco prepagato
041170071015w   1c                   when           DB0tbo = '8'
041180071015     c                   exsr      StampaRPFPP
041190071015e   1c                   endsl
041200090924      /free
041210090924
041220110426         // >?Note per destinatario?
041230090924if  1    if  $BoxNxD;
041240090924           exsr  StampaNxD;
041250090924e   1    endif;
041260090924
041270090924      /end-free
041280040607      *
041290081013      * > DDT in LDV
041300081013      * - IN FUTURO: se al 1° tentativo di consegna
041310081013      *              (DB0FL1 = "D" - v. pgm FNLRS9R)
041320080917if  1c                   if             $FIAR8 =  *on
041330130315     c                             and  $E_AR8 =  *on
041340081013     c***-=»>                      and  DB0fl1 <> 'D'
041350040607     c                   exsr      StampaDDT
041360040607e   1c                   endif
041370050510      *
041380050510     c     End_LdV       tag
041390030515      *
041400080703      * 2ª parte - copia per il destinatario (da lasciare)
041410130418     c                   eval      *in33 = *on
041420040607     c                   exsr      ChiusuraLDV
041430030515      *
041440030515      * Stampa dei 3 asterischi finali (se richiesta)
041450030515if  1c                   if        DB0ges = '*'
041460030515     c                   move      '***'         Pmsg3
041470130418     c                   eval      *in33 = *off
041480130418     c                   exsr      Posiz_Box
041490030515     c                   write     A4prtMSG
041500030515e   1c                   endif
041510030515      *
041520030515     c                   ENDSR
041530080604      /free
041540080604
041550080604       //--------------------------------------------------------------
041560140303       //?Posizionamento del box all'interno della pag. A4             ?
041570080604       //--------------------------------------------------------------
041580130418       BEGSR  Posiz_Box;
041590080704
041600080704         select;
041610080704
041620110426           // -?Richiesta stampa di 1 LdV x pagina?
041630110426           //  ?(LdV in modifica o con packing-list del mitente)?
041640080704           when  DB0fa4 = '1'   or   DB0fa4 = 'S';
041650080704             $LdVxP   = 1;
041660080918             $Len1LdV = $LenLdV2;
041670080704
041680110426           // -?Richiesta stampa di 2 LdV x pagina (ex A5)?
041690110426           //  ?(copia per il vettore & copia per il destinatario)?
041700080704           when  DB0fa4 = '2';
041710080704             $LdVxP   = 2;
041720080704             $Len1LdV = $LenLdV2;
041730080704
041740110426           // -?Richiesta stampa di 4 LdV x pagina?
041750110426           //  ?(solo copia per il vettore)?
041760080704           when  DB0fa4 = '4';
041770080704             $LdVxP   = 4;
041780080704             $Len1LdV = $LenLdV4;
041790080918
041800110426           // -?Richiesta stampa della sola packing-list?
041810110426           //  ?(solo copia per il vettore)?
041820080918           when  DB0fa4 = 'P';
041830080918             $LdVxP   = 2;
041840080924             $Len1LdV = c_Len2_inch;
041850080704
041860080704           other;
041870080704             reset $LdVxP;
041880080704             reset $Len1LdV;
041890080704
041900080704         endsl;
041910080704
041920110426         // -?Richiesta "ripartenza" da foglio A4 successivo?
041930130418         if DB0fl6 = *on  and  Not  *in33;
041940080704           clear  wNumLdV;
041950080704         endif;
041960080704
041970110426         // -?Calcolo posizione di partenza all'interno del foglio A4.?
041980110426         //  ?[Se 2ª LdV nella pagina, ma LdV per A4 (intero):?
041990110426         //   ?si salta alla pag. A4 successiva; la LdV in esame?
042000110426         //   ?dev'essere stampata in posizione iniziale dell'A4,?
042010110426         //   ?seguita dai suoi dati ulteriori.]?
042020130418         if  Not  *in33;
042030130418           wNumLdV  += 1;
042040130418           wSeqLdV   = %rem( wNumLdV : $LdVxP );
042050130418         endif;
042060080704
042070130422         // -?Stampa della copia per il vettore?
042080130422         IF  NOT  *in33;
042090130422
042100130422           select;
042110130422
042120130422             // -?Modulo A4 intero?
042130130422             when  NOT  *in69    and   DB0fa4 <> '4'
042140130422                                 and   DB0fa4 <> 'P';
042150130422               wSeqLdV = 1;
042160130422             when  DB0fa4 = '1'   or   DB0fa4 = 'S';
042170130422               wSeqLdV = 1;
042180130422
042190130422             // -?Calcolo della posizione x box all'interno della pagina "A4"?
042200130422             when  wSeqLdV = *zero   and
042210130422                   $LdVxP  = 2;
042220130422               wSeqLdV = 2;
042230130422             when  wSeqLdV = *zero   and
042240130422                   DB0fa4  = '4';
042250130422               wSeqLdV = 4;
042260130422             when  wSeqLdV = *zero   and
042270130422                   *in69;
042280130422               wSeqLdV  = 2;
042290130422
042300130422           endsl;
042310130422
042320130422         ELSE;
042330130422
042340130422           select;
042350130422
042360130422             // -?Modulo A5 o ricevuta (¼ di A4)?
042370130422             when  *in69;
042380130422
042390130422             // -?Modulo A4 intero - 1ª parte (alta)?
042400130422             when  CurLineA4 <  26   and   §PS1 < c_§PS1;
042410130422               wSeqLdV = 1;
042420130422
042430130422             // -?Modulo A4 intero - 2ª parte (passa)?
042440130422           //when  CurLineA4 <  78;   (in 1/9", ma sarà in 1/8"...)
042450130422             when  CurLineA4 <  70;
042460130422               wSeqLdV  = 2;
042470130422
042480130422             // -?Modulo A4 intero - superata 2ª parte?
042490130422             //  ?PER ORA?IMPOSSIBILE !!!?
042500130422             other;
042510130422               clear wMaxLine;
042520130422               exsr  Test_OF;
042530130422               wMaxLine = $MaxLine2;
042540130422               wSeqLdV  = 1;
042550130422
042560130422           endsl;
042570130422
042580130422         ENDIF;
042590080919
042600110426         // -?Impostaz. delle posizioni nella pagina in corso di stampa?
042610080919         wPosIni = ($Len1LdV * (wSeqLdV - 1));
042620130422         if  *in33;       // -?Stampa della copia per il destinatario?
042630130422           wPosIni += $LenLdV4;
042640130422         endif;
042650110426
042660120530         //// -?Impostaz. dimensioni per il logo Bartolini-BRT?
042670120530         //If  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
042680120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
042690120530         //  reset  c_Spessore;
042700120530         //  reset  c_DeltaSpig;
042710120530         //  reset  c_LunghMaxV;
042720120530         //  reset  c_LunghV;
042730120530         //  reset  c_LunghO;
042740120530         //  reset  c_XspaceX;
042750120530         //  reset  c_BOXspace;
042760120530         //Else;
042770120530         //  c_Spessore  = c_1_Spessore;
042780120530         //  c_DeltaSpig = c_1_DeltaSpig;
042790120530         //  c_LunghMaxV = c_1_LunghMaxV;
042800120530         //  c_LunghV    = c_1_LunghV;
042810120530         //  c_LunghO    = c_1_LunghO;
042820120530         //  c_XspaceX   = c_1_XspaceX;
042830120530         //  c_BOXspace  = c_1_BOXspace;
042840120530         //EndIf;
042850120530         // -?Dimensioni per il logo BRT già definite?
042860120530         //c_Spessore  = c_2_Spessore;
042870120530         //c_DeltaSpig = c_2_DeltaSpig;
042880120530         //c_LunghMaxV = c_2_LunghMaxV;
042890120530         //c_LunghV    = c_2_LunghV;
042900120530         //c_LunghO    = c_2_LunghO;
042910120530         //c_XspaceX   = c_2_XspaceX;
042920120530         //c_BOXspace  = c_2_BOXspace;
042930080704
042940110426         // -?Se richiesta la stampa della sola packing-list?
042950110426         //  ?NON si dovranno stampare i dati della LdV?
042960110426         //  ?(quindi NON servono i posizionamenti che seguono)?
042970080918         if  DB0fa4 = 'P';
042980080918           leavesr;
042990080918         endif;
043000080918
043010110426         // -?Posizionamento nel foglio A4 in base al n° LdV in stampa?
043020110426         //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
043030110426
043040130418         select;
043050130418           when  DB0fa4 = '4';
043060130418             *in41 = (wSeqLdV = 1);
043070130418             *in42 = (wSeqLdV = 2);
043080130418             *in43 = (wSeqLdV = 3);
043090130418             *in44 = (wSeqLdV = 4);
043100130418           when Not *in33;
043110130418             *in41 = (wSeqLdV = 1);
043120130418             *in42 = *off;
043130130418             *in43 = (wSeqLdV = 2);
043140130418             *in44 = *off;
043150130418           other;
043160130418             *in41 = *off;
043170130418             *in42 = (wSeqLdV = 1);
043180130418             *in43 = *off;
043190130418             *in44 = (wSeqLdV = 2);
043200130418         endsl;
043210080704         WRITE  A4prtPOS;
043220080704
043230110426         // -?Posizionamenti nella copia per il vettore?
043240110426         //  ?¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯?
043250080704
043260110426         // -?Box in copia per il vettore?
043270080704         §Box1As = wPosIni + c_PosBox1As;
043280140919         §Box1Ai = wPosIni + c_PosBox1Bs  + c_LineNarrow;
043290080704         §Box1Bs = wPosIni + c_PosBox1Bs;
043300080704         §Box1Bi = wPosIni + c_PosBox1Bi;
043310140919         §Box1Bj = wPosIni + c_PosLin1oD1 + c_LineNarrow;
043320080704         §Box1Cs = wPosIni + c_PosBox1Cs;
043330140919         §Box1Ci = wPosIni + c_PosBox1Ds  + c_LineWide;
043340080704         §Box1Ds = wPosIni + c_PosBox1Ds;
043350091119         §Box1Di = §Box1Bi;
043360081218
043370120530         //If  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
043380120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
043390120530         //  // -?Simil-Logo Bartolini?
043400120530         //  §PlblS  = §Box1As + 0.125;
043410120530         //  §PlblVS = 7.092;
043420120530         //  exsr  sr_SimilLogoBAR;
043430120530         //  // -?Simil-Logo BRT?
043440120530         //  §P2blS  = §PlblS;
043450120530         //  §P2blVS = §PlblVD + 0.030;
043460120530         //  exsr  sr_SimilLogoBRT;
043470120530         //Else;
043480120530         //  // -?Simil-Logo Bartolini?
043490120530         //  §PlblS  = §Box1As;
043500120530         //  §PlblVS = 7.128;
043510120530         //  exsr  sr_SimilLogoBART;
043520120530         //EndIf;
043530120530         // -?Simil-Logo BRT?
043540120530         §PlblS  = §Box1As;
043550120530         §PlblVS = 7.581;
043560120530         exsr  sr_SimilLogoBRT_;
043570080704
043580120530         // -?Costanti dati anagrafici BRT?
043590110426         §PC0    = wPosIni + c_PosTBar;
043600110426         §PC1    = wPosIni + c_PosTBa1;
043610120530         //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
043620120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
043630120530         //  §PC2    = wPosIni + c_PosTBa2;
043640120530         //  §PC3    = wPosIni + c_PosTBa3;
043650120530         //  §PC4    = wPosIni + c_PosTBa4;
043660120530         //  §PC5    = wPosIni + c_PosTBa5;
043670120530         //  §PC6    = wPosIni + c_PosTBa6;
043680120530         //else;
043690120530         //  §PC2    = wPosIni + c_PosTBa2x;
043700120530         //  §PC3    = wPosIni + c_PosTBa3x;
043710120530         //  §PC4    = wPosIni + c_PosTBa4x;
043720120530         //  §PC5    = wPosIni + c_PosTBa5x;
043730120530         //  §PC6    = wPosIni + c_PosTBa6x;
043740120530         //endif;
043750120530         §PC2    = wPosIni + c_PosTBa2x;
043760120530         §PC3    = wPosIni + c_PosTBa3x;
043770120530         §PC4    = wPosIni + c_PosTBa4x;
043780120530         §PC5    = wPosIni + c_PosTBa5x;
043790120530         §PC6    = wPosIni + c_PosTBa6x;
043800130726         §PC7    = §Box1Bi - 0.150;
043810110426
043820110426         // -?Linee orizzontali nel box in copia per il vettore?
043830080704         §Lo1S1  = wPosIni + c_PosLin1oS1;
043840080704         §Lo1S2  = wPosIni + c_PosLin1oS2;
043850080922       //§Lo1S3  = wPosIni + c_PosLin1oS3;
043860080704         §Lo1S4  = wPosIni + c_PosLin1oS4;
043870091119         §Lo1S5  = wPosIni + c_PosLin1oS5;
043880080704         §Lo1C1  = wPosIni + c_PosLin1oC1;
043890080704         §Lo1C2  = wPosIni + c_PosLin1oC2;
043900140915         §Lo1C3  = wPosIni + c_PosLin1oC3;
043910080704         §Lo1D1  = wPosIni + c_PosLin1oD1;
043920080704
043930110426         // -?Lunghezza linee verticali nel box in copia per il vett.?
043940140922         §LLv1A  = §Lo1S5  - §Lo1S1;
043950140922         //§LLv1B  = §Lo1S4  - §Box1Bs;
043960140922         //§LLv1C  = §Box1Di - §Box1Bs;
043970140922         //***§LLv1B  = §Lo1S4  - §Box1Bj + c_LineNarrow;
043980140922         §LLv1B  = §Lo1S4  - §Box1Bj;
043990140922         §LLv1C  = §Box1Di - §Box1Bj;
044000140917
044010140917         // -?Costante "BRTcode" & codice BRTcode?
044020140917         §Pk1    = wPosIni + c_PosKBRTc;
044030080704
044040110426         // -?Costante "Consegna tassativa"?
044050080704         §Pk2    = wPosIni + c_PosKTass;
044060080704
044070110426         // -?Costanti "(nome e cognome in stampatello)", "(firma)" e?
044080110426         //           ?"(data e ora)"?
044090080704         §Pk3    = §Box1Bi - 0.015;
044100080704
044110110426         // -?Costante con rif. alla legge in vigore per RCV?
044120080704         §Pk4    = wPosIni + c_PosKRCV;
044130080704
044140110426         // -?Costante "Bollo assolto in modo virtuale..."?
044150080704         §Pk5    = wPosIni + c_PosKBAV;
044160080704         §Pk6    = §Pk5 + 0.060;
044170121017
044180121017         // -?Partita IVA o Cod. Fiscale del cliente?
044190121017         §Pk7    = §Pk4 + 0.080;
044200080704
044210110426         // -?Costante "COPIA PER IL DESTINATARIO" in DDT "generico"?
044220080709         §Pos    = c_PosKCXD2;
044230080704
044240080704       ENDSR;
044250080605
044260080604      /end-free
044270110426
044280140303       //--------------------------------------------------------------
044290140303       //?Posizionamento del simil-logo Bartolini (unico)              ?
044300140303       //--------------------------------------------------------------
044310120530      *///copy gaitrasrc/srcTNSY,LogoBAR_R2
044320110426
044330140303       //--------------------------------------------------------------
044340140303       //?Posizionamento del simil-logo Bartolini (con BRT)            ?
044350140303       //--------------------------------------------------------------
044360120530      *///copy gaitrasrc/srcTNSY,LogoBAR3R2
044370110426
044380140303       //--------------------------------------------------------------
044390140303       //?Posizionamento del simil-logo BRT (con BARTOLINI)            ?
044400140303       //--------------------------------------------------------------
044410120530      *///copy gaitrasrc/srcTNSY,LogoBRT3R2
044420120530
044430140303       //--------------------------------------------------------------
044440140303       //?Posizionamento del simil-logo BRT (unico)                    ?
044450140303       //--------------------------------------------------------------
044460120530      /copy gaitrasrc/srcTNSY,LogoBRT_R2
044470110426
044480030515      *---------------------------------------------------------------*
044490050217      *?Gestione overflow                                            ?*
044500030515      *---------------------------------------------------------------*
044510030515     c     Test_OF       BEGSR
044520030515      *
044530030515      * Test raggiungimento della posizione limite
044540081003     c                   if          ((§PS1 < $LimOF   and   NOT *in45)
044550081003     c                             or (§PS1 < ($LimOF - c_Len1_inch)
044560081003     c                                     and *in45   and   NOT *in46)
044570081003     c                             or (§PS1 < ($LimOF + c_Len1_inch)
044580081003     c                                     and *in45   and   *in46))
044590080924     c                             and CurLineA4 < wMaxLine
044600080919     c                   leavesr
044610030515e   1c                   endif
044620030515      *
044630030515      * Stampa del box per dati da FIAR5
044640030515      * (se è già stata stampata almeno una riga di tale box)
044650060921if  1c                   if        $FIAR5 = *on  and  $First = *off
044660030911     c                   eval      §LO2   = §PS1 + 0.050
044670071008if  2c                   if        NOT *in52
044680030515     c                   write     A4prtAR53
044690071008x   2c                   else
044700071008     c                   eval      §PS2   = §LO2 - 0.500
044710071008     c                   write     A4prtAR54
044720071008e   2c                   endif
044730030515e   1c                   endif
044740041209      *
044750041209sel 1c                   select
044760040607      *
044770041209      * DDT KARNAK/TITANEDI/EDK
044780041209      * Stampa di logo, box, linee e testi fissi del DDT del mittente
044790040607      * (se si stà stampando tale DDT)
044800040607      * (ma NON per la parte "merce omaggio")
044810040607      * calcolando - prima - le posizioni iniziale e finale del box
044820041209w   1c                   when           $DDT_p    = *on
044830071008     c                             and  §AR8tsd   = 'K1'
044840040607     c                             and  $DDT_o    = *off
044850040608      * - Stampa parte finale DDT
044860040608if  2c                   if        $DDTlastPg = *off
044870041209      * - - Calcolo della posizione finale del box
044880040607     c                   eval      PCnv2  = '          '
044890040607     c                                    + '*** SEGUE ***'
044900040608     c                   exsr      Posiz_DK
044910040607     c                   write     A4prtDK4
044920040608x   2c                   else
044930041209      * - - Stampa dei dati in chiusura DDT (se ultima pagina)
044940040608      *     (la posizione finale del box verrà lì calcolata)
044950050217     c                   exsr      PrtDdtKARN_V2
044960040608e   2c                   endif
044970040607      * - Stampa logo, box, linee e testi fissi
044980081017     c                   if            §AR8cxd = *blanks
044990081017     c                             or  DB0fa4  = 'P'
045000050218     c                   write     A4prtDT0
045010080923     c                   endif
045020040607     c                   write     A4prtDK0
045030041019      *
045040050414      * DDT LOGISTICA/SAIT Abrasivi
045050050215      * Stampa di logo, box, linee e testi fissi del DDT del mittente
045060041019      * (se si stà stampando tale DDT)
045070041209w   1c                   when           $DDT_p    = *on
045080060627     c                             and  %subst(§AR8tsd:1:1) = 'L'
045090041021     c                   exsr      Posiz_DL
045100050217      * - - Chiusura box in fase di stampa dettaglio DDT
045110050217      *     (la stampa dei dati in chiusura DDT sarà solo nell'ultima
045120050318      *      pagina - prima dell'eventuale packing-list)
045130050318      *     La gestione del salto alla 1ª pagina della packing-list
045140050321      *     (AR8trk = 'Z1' & Page = *zeros) avviene nella subr.
045150050318      *     "PrtDdtLOG1_Z1" (vedi subr. "PrtPckLst_1").
045160081017     c                   if            §AR8cxd = *blanks
045170081017     c                             or  DB0fa4  = 'P'
045180050308     c                   write     A4prtDT0
045190050331     c                   endif
045200050318sel 2c                   select
045210050505      *     - si sta stampando il DDT Logistica o Sait
045220050318w   2c                   when      %subst(AR8trk:1:1) <> 'Z'
045230071008     c                             and %subst(AR8trk:1:1) <> 'S'
045240050218     c                   write     A4prtDL0A
045250050615      *     - si sta stampando la Packing-List
045260050318w   2c                   when      %subst(AR8trk:1:1) =  'Z'
045270050615     c                             and    Page        >  *zeros
045280050318     c                   write     A4prtDL0C
045290050606      *     - si sta stampando la Fattura (oltre 1ª pag.)
045300071008w   2c                   when      %subst(AR8trk:1:1) =  'S'
045310081003     c                             and    Page        >  *zeros
045320050609     c                   write     A4prtDL0E
045330050318e   2c                   endsl
045340050113      *
045350050113      * DDT ARCTE
045360050215      * Stampa di logo, box, linee e testi fissi del DDT del mittente
045370050113      * (se si stà stampando tale DDT)
045380050113w   1c                   when           $DDT_p    = *on
045390050113     c                             and  AR8tmd    = 'ARCT'
045400050114     c                   exsr      Posiz_AC
045410050217      * - - Chiusura box in fase di stampa dettaglio DDT
045420050217      *     (la stampa dei dati in chiusura DDT sarà solo nell'ultima
045430050217      *      pagina)
045440081017     c                   if            §AR8cxd = *blanks
045450081017     c                             or  DB0fa4  = 'P'
045460050331     c                   write     A4prtDT0
045470050331     c                   endif
045480050217     c                   write     A4prtAC0A
045490050215      *
045500050215      * DDT ESPRINET
045510050215      * Stampa di box, linee e testi fissi del DDT del mittente
045520050215      * (se si stà stampando tale DDT)
045530050215      * · Solo nella 1ª pagina vengono stampati il logo e la parte
045540050215      *        superiore del DDT.
045550050215      * · Solo nell'ultima pagina vene stampato il "fondo".
045560050215w   1c                   when           $DDT_p    = *on
045570060627     c                             and  §AR8tsd   = 'E1'
045580110405if 1bc                   if        %subst(SAVtrk:1:1 ) <> 'P'
045590110405     c                             or   %eof(FIAR801L)
045600050215      * - Stampa parte finale DDT
045610050215      * - - Calcolo delle posizioni di box & linee
045620050215     c                   exsr      Posiz_ES
045630050215      * - - Chiusura box in fase di stampa dettaglio DDT
045640050215      *     (la stampa dei dati in chiusura DDT sarà solo nell'ultima
045650050215      *      pagina)
045660050215if  2c***                if        %subst(AR8trk:1:1) <> 'P'
045670081017     c                   if            §AR8cxd = *blanks
045680081017     c                             or  DB0fa4  = 'P'
045690050331     c                   write     A4prtDT0
045700050331     c                   endif
045710050215     c                   write     A4prtES0A
045720050215e   2c***                endif
045730110405e  1bc                   endif
045740041209      *
045750041209      * DDT GENERICO con LOGO
045760050215      * Stampa di logo e box del DDT del mittente
045770041209      * (se si stà stampando tale DDT)
045780041209w   1c                   when           $DDT_p    = *on
045790041209     c                             and  §AR8tsd   = 'S2'
045800041209     *** * - Chiusura del box con i dati di mittente e destinatario
045810041209     *** *   (Se si stanno ancora stampando 'sti dati: bisogna calcolare
045820041209     *** *    la posizione di chiusura temporanea del box)
045830041209if  2***c                   if             AR8trk =  'B1'
045840041209     ***c                             or   AR8trk =  'B2'
045850041209     *** *   Impostazione della posizione finale per il box
045860041209     ***c                   exsr      Rep_§LO2
045870041209     ***c                   eval      §PB2   = §LO2 + 0.065
045880041209e   2***c                   endif
045890041209     *** *   Stampa del box
045900041209     ***c                   write     A4prtDB0
045910041209      * - Il logo è già stato SICURAMENTE stampato
045920041209     c***                exsr      Rep_§LO2
045930041209     c***                eval      §PB1   = §LO2
045940041209      *** Stampa del logo
045950041209     c***                write     A4prtDB0
045960041209      *
045970041209e   1c                   endsl
045980030515      *
045990030515      * Stampa dell'indicazione "Segue..." su pagina corrente
046000030515     c                   clear                   Pseg0
046010030515     c                   clear                   Pseg1
046020030515     c                   eval      Pseg9  = '  Segue...'
046030080919      /free
046040080923         select;
046050080923           when  *in45   and   NOT *in46;
046060080923             §PSA = 0.200;   // Per campo vuoto !
046070080923             §PSB = 0.200;   // Per campo vuoto !
046080080923             §PSC = c_Len2_inch - 0.050;
046090080923           when  *in45   and   *in46;
046100080923             §PSA = c_Len2_inch + 0.200;   // Per campo vuoto !
046110080923             §PSB = c_Len2_inch + 0.200;   // Per campo vuoto !
046120080924             §PSC = c_Len2_inch * 2 - 0.200;
046130080923           when  NOT *in45;
046140080923             §PSA = 0.400;
046150080923             §PSB = 0.700;
046160080923             §PSC = 8.750;
046170080923         endsl;
046180080919      /end-free
046190030515     c                   write     A4prtSEG
046200030515      *
046210031031      * Se sono ancora nella prima pagina =>
046220030515      * stampo anche i dati per il destinatario e box/barcode/ecc.
046230080604sel 1c                   select
046240080923      * > Se NON sto stampando un DDT del mittente
046250080922      *   (nel qual caso non stampo null'altro)
046260080923      *   => nulla
046270080919w   1c                   when      wPage  > 1
046280080923      * > Se sto stampando le sole copie per il vettore (4 in A4)
046290080923      *    o la relativa packing-list
046300080923      *   (nel qual caso non stampo null'altro)
046310080923      *   => nulla
046320081003w   1c                   when      DB0fa4 = '4'   or   DB0fa4 = 'P'
046330040607      * > Se NON è ristampa in arrivo per recapito c/assegni  e
046340040707      *   se NON sto stampando una LdV Poste
046350080703      *   se NON sto stampando (anche) un DDT del mittente
046360080703      *   se NON sto stampando le sole copie per il vettore (4 in A4)
046370080923      *   => posizionamento e stampa
046380120113w   1c                   when          $No_x_Cons =  *off
046390081013     c                             and ($FIAR8    =  *off)
046400081013     c***-=»>                       or  DB0fl1    =  'D')
046410080605      * - Posizionamento sul foglio A4 in base al n° pagina in stampa
046420130418     c                   eval      *in33 = *on
046430130418     c                   exsr      Posiz_Box
046440130418      * - Stampa 2ª parte della LdV (copia per il destinatario)
046450130418     c                   write     A4prtLDV
046460031031      * - Stampa "cornice" / barcode / righine varie in parte inferiore
046470130418     c                   write     A4prtBOX
046480110426     c*//                write     A4LogoBAR
046490110426      /free
046500120530         //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
046510120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
046520120530         //  // >?Simil-Logo Bartolini/Brt?
046530120530         //  write  A4LogoBR2;
046540120530         //  write  A4LogoDA2;
046550120530         //else;
046560120530         //  // >?Simil-Logo Bartolini?
046570120530         //  write  A4LogoBAR;
046580120530         //  write  A4LogoDA1;
046590120530         //endif;
046600120530         // >?Simil-Logo BRT?
046610120530         write  A4LogoBRT;
046620120530         write  A4LogoDA3;
046630110426      /end-free
046640120113      *//* > Se sto stampando una LdV Poste per recapito c/assegni
046650120113w   1c*//                   when          $No_x_Cons =  '2'
046660120113      *//* - Stampa 2ª parte "nulla" della LdV
046670130418     c*//                   eval      *in33 = *on
046680130418     c*//                   exsr      Posiz_Box
046690120113     c*//                   write     A4prtLDVB
046700080604e   1c                   endsl
046710080923      /free
046720110426         // -?Se richiesta la stampa della sola packing-list:?
046730110426         //  ?incremento del n° spedizione per calcolare correttam.?
046740110426         //  ?il n° di pagina iniziale per la spediz. successiva?
046750080925         if  DB0fa4 = 'P';
046760080925             wNumLdV += 1;
046770080925         endif;
046780080925
046790110426         // -?Reimpostazione degli indicatori per il posizionamento?
046800110426         //  ?all'interno del foglio A4?
046810080923         *in45 = (DB0fa4 = 'P');
046820080923         *in46 = (DB0fa4 = 'P'   and   %rem(wNumLdv : 2) = *zero);
046830080923      /end-free
046840030515      *
046850030515      * Salta a pagina successiva e indica "(Seguito)"
046860030515     c                   write     A4prtSP0
046870030515     c                   eval      wPage  = wPage + 1
046880080923      /free
046890081017         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
046900081017           write  A4prtNUL;
046910081017         endif;
046920081017
046930080923         IF  DB0fa4 <> 'P';
046940080922      /end-free
046950030515     c                   eval      Pseg0  = 'Spedizione '
046960110310     c                                    + %editc(DB0lnp : 'X') + ' '
046970110310     c                                    + %editc(DB0nrs : 'X') + ' '
046980110310     c                                    + %editc(DB0nsp : 'X')
046990030515     c                                    + ' del ' + %editc(Pdsp : 'Y')
047000030515     c                   eval      Pseg1  = '(Seguito)             Pag.'
047010030515     c                                    + %editc(wPage:'Z')
047020030515     c                   clear                   Pseg9
047030030515     c                   write     A4prtSEG
047040080922      /free
047050080923         ELSE;
047060081017
047070110426           // -?"Copia per il destinatario" già impostato in sr. "PrepPrt"?
047080081017           //if  §AR8cxd = *blank;
047090130726           //  Tcxd  = c_CopiaXdest;
047100130726           //  Tcxd2 = Tcxd;
047110081017           //endif;
047120081017
047130110426           // -?Estremi spedizione da impostare SOLO se si stampando?
047140110426           //  ?la sola packing-list (DB0FA4 = "P")?
047150081017           if  DB0fa4 = 'P';
047160101028             //Trsd = Prsd;   (già impostato nella subr. StampaPkgLst)?
047170081017             Trif = 'Spedizione '
047180081017                  + %editc(DB0lnp : 'X') + ' '
047190110310                  + %editc(DB0nrs : 'X') + ' '
047200110310                  + %editc(DB0nsp : 'X')
047210081017                  + ' del ' + %editc(Pdsp : 'Y')
047220081017                  + '        Pag.'
047230101028                  + %editc(wPage:'Z');
047240101028                //+ '        (Seguito)';
047250101028             //Tndc = Pndc;   (già impostato nella subr. StampaPkgLst)?
047260081017           endif;
047270080923
047280110426           // -?Verifica se la 2ª parte della packing-list sta nella?
047290110426           //  ?pag. attuale o se deve andare in quella successiva?
047300080923           wSeqLdV  = %rem( wNumLdV : $LdVxP );
047310080923           if  wSeqLdV = *zero   and   $LdVxP  = 2;
047320080923             wSeqLdV = 2;
047330080923           endif;
047340080923
047350110426           // -?Posizionamenti nella pagina in corso di stampa?
047360110426           //  ?(§POS = posizione verticale della costante "COPIA PER?
047370110426           //   ?IL DESTINATARIO" (con estremi spedizione e pag.)?
047380080923           wPosIni  = ($Len1LdV * (wSeqLdV - 1));
047390080924           if  wSeqLdV = 1;
047400080924             §Pos   = wPosIni + c_PosBox1As + 0.040;
047410090422             §Pos   = wPosIni + c_PosBox1As + 0.035;
047420080924           else;
047430080924             §Pos   = wPosIni + c_PosBox1As - 0.070;
047440080924           endif;
047450081017
047460081017           if  §AR8cxd = *blank   or   DB0fa4  = 'P';
047470081017             write A4prtDT0;
047480081017           endif;
047490080923
047500080923         ENDIF;
047510080922      /end-free
047520030515      *
047530030515      * Reimposta posizioni iniziali di stampa per linee
047540030515      *   (orizzontali-§LOn e verticali-§LVn) e campi (§PSn).
047550030515      *   Nelle pagine dalla 2ª in poi si parte da in alto:
047560030515      *   non vengono ri-stampati i 2 box "vettore" e "destinatario".
047570030515sel 1c                   select
047580120530      * - BRT
047590030515w   1c                   when      *in70
047600080711     c                   eval      §LO1   = c_§PS1
047610030515     c                   eval      §LO2   = §LO1
047620030515     c                   eval      §PS1   = §LO1
047630040607sel 2c                   select
047640050217      * - - con DDT del mittente KARNAK / TITANEDI / EDK
047650040607w   2c                   when           $DDT_p    = *on
047660071008     c                             and  §AR8tsd   = 'K1'
047670040608     c                   exsr      NewPageDK
047680050414      * - - con DDT del mittente LOGISTICA / SAIT ABRASIVI
047690041207w   2c                   when           $DDT_p    = *on
047700060627     c                             and  %subst(§AR8tsd:1:1) = 'L'
047710050321     c                   exsr      NewPageDL
047720050113      * - - con DDT del mittente ARCTE
047730050113w   2c                   when           $DDT_p    = *on
047740050113     c                             and  AR8tmd    = 'ARCT'
047750081002     c                   exsr      NewPageAC
047760050215      * - - con DDT del mittente ESPRINET
047770050215w   1c                   when           $DDT_p    = *on
047780060627     c                             and  §AR8tsd   = 'E1'
047790110405if  2c                   if        %subst(SAVtrk:1:1 ) <> 'P'
047800110405     c                             or   %eof(FIAR801L)
047810081002     c                   exsr      NewPageES
047820110405e   2c                   endif
047830041207      * - - con DDT generico (con marchio)
047840041207w   2c                   when           $DDT_p    = *on
047850041207     c                             and  §AR8tsd   = 'S2'
047860081017     c                   exsr      NewPageSTD
047870041207      * - - con DDT generico (senza marchio)
047880041207w   2c                   when           $DDT_p    = *on
047890041207     c*** ("scontato":)            and  §AR8tsd   = 'S1'
047900081017     c                   exsr      NewPageSTD
047910040607e   2c                   endsl
047920080922      * - DPD (ora in ex-A5)
047930080922w   1c***                when      *in71
047940030515      * - FedEx
047950030515w   1c                   when      *in72
047960030709     c                   eval      §LO1   = 1.600
047970030515     c                   eval      §LO2   = §LO1 + 0.425
047980030515     c                   eval      §LV1   = §LO1 + 0.300
047990030515     c                   eval      §LV2   = §LV1 + 0.070
048000030515     c                   write     A4prtFED1
048010030515do  2c                   dow       CurLineA4 < $MinLine1
048020030515     c                   write     A4prtNUL
048030030515e   2c                   enddo
048040030515     c                   write     A4prtFED2
048050030515     c                   eval      §LO1   = §LO2 - 0.250
048060030911     c                   eval      §PS1   = §LO1 + 0.150
048070040223      * - EuroExpress
048080040223w   1c                   when      *in73
048090030515      *
048100030515e   1c                   endsl
048110030515      *
048120030515      * N.B.: Per i SegnaColli non sequenziali non servono queste
048130030515      *       impostazioni: trattasi di stampa con salto riga -skipb-
048140030515      *       quindi sono già posizionato.
048150030515      *
048160080919     c                   ENDSR
048170030910      *
048180030910      *---------------------------------------------------------------*
048190050217      *?Reperimentoo della posizione (in pollici) in cui posizionarsi?*
048200050217      *?* per la stampa in base all'ultima riga stampata             ?*
048210030910      *---------------------------------------------------------------*
048220030910     c     Rep_§LO2      BEGSR
048230030910      *
048240030910sel 1c                   select
048250080825      *
048260080825      * Se ho stampato l'elenco dei segnacolli non sequenziali, devo
048270080825      *   "allineare" il posizionamento in pollici all'ultima riga
048280080825      *   stampata (in ottavi di pollice).
048290080825      *   =>   Num_ultima_riga_stampata / 8
048300080825w   1c                   when           DB0fns     = 'S'
048310080825     c                             and  DB0ncl     > 2
048320080825     c                             and  $A4_x_ScNS = *on
048330080825     c                             and  NOT  *in69
048340080825     c                   eval(h)   §LO2   = (CurLineA4 / 8) + 0,1
048350040607      *
048360040607      * Se ho stampato il DDT del mittente
048370080714      *   =>   Num_ultima_riga_stampata / 9
048380081013w   1c                   when           $FIAR8 =  *on
048390081013     c***-=»>                      and  DB0fl1 <> 'D'
048400080708     c                   eval(h)   §LO2   = (CurLineA4 / 9) + 0,1
048410030910      *
048420030910      * Se non ho stampato né elenco segnacolli non sequenziali né
048430040223      *   altri box, devo impostare comunque i dati per il posiziona-
048440030910      *   mento di questo primo box
048450030910w   1c                   when      §LO2   = *zeros
048460080624     c                   eval      §LO1   = 2.850
048470030910     c                   eval      §LO2   = §LO1
048480030910      *
048490030910e   1c                   endsl
048500030910      *
048510030910     c                   ENDSR
048520031124      *
048530031124      *---------------------------------------------------------------*
048540050217      *?Reperim. della posizione (in pollici) utlizzaile per la      ?*
048550080714      *?  stampa dei box in base all'ultima riga stampata in 1/9"    ?*
048560031124      *---------------------------------------------------------------*
048570031124     c     CalcRowPos    BEGSR
048580031124      *
048590031124      * Imposto le posizioni inziali per il primo dei box
048600080624     c                   select
048610080624      *
048620040607      * - in DDT del mittente: dopo eventuale elenco merce omaggio
048630120530      *   (come in LDV BRT)
048640120530      * - in LDV BRT: dopo eventuale elenco segnacolli non seq.
048650080624     c                   when      *in70
048660080624     c                   exsr      Rep_§LO2
048670080624      *
048680080922      * - in LDV DPD (ora in ex-A5)
048690080922     c***                when      *in71
048700080624      *
048710040607      * - in LDV FEDEX: posizione variabile
048720031124      *   (dipende dal numero dei segnacolli elencati: intanto
048730031124      *   posiziona la "base di partenza" sull'ultima posizione
048740031124      *   eventualmente usata per l'elenco dei segnacolli FedEx;
048750031124      *   poi si ri-calcoleranno in base al numero di righe stampate)
048760080624     c                   when      *in72
048770080624     c                   eval      §LO2   = §LO1 + 0.250
048780080624      *
048790080912      * - in LDV EUROEXPRESS: non più gestita
048800080624     c                   when      *in73
048810080912      *
048820080624     c                   endsl
048830080624      *
048840031124     c                   eval      §LO1   = §LO2
048850031124      *
048860031124     c                   ENDSR
048870060524      *
048880060524      *---------------------------------------------------------------*
048890060524      *?Stampa box per riferimento al                                ?*
048900060524      *?DOCUMENTO di ACCOMPAGNAMENTO SEMPLIFICATO                    ?*
048910060524      *---------------------------------------------------------------*
048920060524     c     StampaDAS     BEGSR
048930060524      *
048940060524     c***                eval      Par5   = '*...+....1....+....2....+-
048950060524     c***                          ....3....+....4....+....5....+....6-
048960060524     c***                          ....+....7....+....8....+....9....+-
048970060524     c***                          ....A'
048980060524      *
048990060524      * - stampo testata
049000090302     ***c                   eval      $First = 'B'
049010090302     ***c                   eval      Par5   = C_PMxA
049020090302      /free
049030090302         $First = *on;
049040090302         Par5   = '                    '
049050090302                + '                    '
049060090302                + '          '
049070090302                + C_PMxA;
049080090302      /end-free
049090060524     c                   exsr      StampaAR5
049100090302      /free
049110090302
049120090302         // - memorizza posizione iniziale per il box
049130090302         //   (§PB1 sarà da reimpostare in §LO1 PRIMA della stampa del box)
049140090302         §PB1   = §LO1;
049150090302
049160090302         // - stampa linea separatrice
049170090302         §LO1   = §PS1 + 0.050;
049180090302         write  A4prtLO;
049190090302         §PS1   = §LO1;
049200090302      /end-free
049210060524      *
049220060524      * - stampo 1° testo
049230060524     c                   eval      $First = '2'
049240060524     c                   eval      Par5   = 'CONSEGNARE AL DESTINATARI-
049250060524     c                             O D.D.T. E D.A.S. (Documento di Acc-
049260060524     c                             ompagnamento Semplificato).'
049270060524     c                   exsr      StampaAR5
049280060524      *
049290060524      * - stampo 2° testo
049300061030     c*** (già fatto:)   eval      $First = *off
049310060524     c                   eval      Par5   = 'In caso di RIFIUTO compil-
049320060524     c                             are l''ultima pagina del D.A.S. ind-
049330060524     c                             icando:'
049340060524     c                   exsr      StampaAR5
049350060524     c                   eval      Par5   = 'a) nome Autotrasportatore-
049360060524     c                              nel campo "IL SOTTOSCRITTO ... INC-
049370060524     c                             ARICATO DEL TRASPORTO"'
049380060524     c                   exsr      StampaAR5
049390060524     c                   eval      Par5   = 'b) firma Autotrasportator-
049400060524     c                             e nel campo "FIRMA INCARICATO DEL T-
049410060524     c                             RASPORTO"'
049420060524     c                   exsr      StampaAR5
049430060524     c                   eval      Par5   = 'c) causale del rifiuto (b-
049440060524     c                             arrare "la merce non è conforme all-
049450060524     c                             ''ordine" o indicare altra motivazi-
049460060524     c                             one)'
049470060524     c                   exsr      StampaAR5
049480060524     c                   eval      Par5   = 'd) firma del destinatario-
049490060524     c                              nel campo "FIRMA DEL DESTINATARIO"'
049500060524     c                   exsr      StampaAR5
049510090302     *** *
049520090302     *** * - stampo box
049530090302     ***c                   eval      §LO2   = §PS1 + 0.050
049540090302     ***c                   write     A4prtAR53
049550090302      /free
049560090302         // - stampa box
049570090302         //   (§PB1 è da reimpostare in §LO1: contiene il suo valore PRIMA
049580090302         //   della stampa delle varie linee separatrici)
049590090302         §LO1   = §PB1;
049600090302         §LO2   = §PS1 + 0.070;
049610090302         write  A4prtAR53;
049620090302      /end-free
049630060524      *
049640060524     c                   ENDSR
049650031124      *
049660031124      *---------------------------------------------------------------*
049670050217      *?Prepara elenco per box "Promemoria per autotrasportatore"    ?*
049680031124      *---------------------------------------------------------------*
049690031124     c     ListaPMxA     BEGSR
049700031124      *
049710070308      * - stampo testata
049720070316     c                   eval      $First = *on
049730070319     c                   eval      Par5   = '                    '
049740070319     c                                    + '                    '
049750070319     c                                    + '          '
049760070319     c                                    + C_PMxA
049770031124     c                   exsr      StampaAR5
049780070315      *
049790070315      * - memorizzo posizione iniziale per il box
049800070320      *   (§PB1 sarà da reimpostare in §LO1 PRIMA della stampa del box)
049810070315     c                   eval      §PB1   = §LO1
049820031124      *
049830070308      * - stampo testo
049840081127      *
049850081212      /free
049860110411         // >?Mittente cliente in tab. "PXA"?
049870090302         IF  $TabPXA = *on;
049880090302
049890170517           clear  k05tntbe01;
049900090302           k_TBEcod = 'PXA';
049910090302           k_TBEke1 = %editc(DB0ccm : 'X');
049920100316           k_TBEke2 = '001            ';
049930090302           clear  k_TBElin;
049940090302           clear  k_TBEsif;
049950100316           //setll  %kds(k05tntbe01 : 3)  TNTBE000;  (già fatto)
049960090302           //reade  %kds(k05tntbe01 : 2)  TNTBE000;  (già fatto)
049970090302           clear  wx;
049980090302
049990100316           DoW  not  %eof(TNTBE01L)  and  TBEke2 <= '006            ';
050000090302
050010090302             if  TBEatb = *blank;
050020090302
050030110411               // -?Stampa della riga separatrice iniziale?
050040090302               if  wx = *zero;
050050090302                 §LO1   = §PS1 + 0.050;
050060090302                 write  A4prtLO;
050070090302                 §PS1   = §LO1;
050080090302                 $First = 'G';
050090090302               endif;
050100090302
050110110411               // -?Salta eventuali righe vuote?
050120090302               dow  %int( %trimr( TBEke2 ) ) > wx + 1;
050130090302                 clear  Par5;
050140090302                 exsr  StampaAR5;
050150090302                 wx += 1;
050160090302               enddo;
050170090302
050180110411               // -?Stampa la singola riga di testo?
050190090302               Par5   = TBEuni;
050200090302               exsr  StampaAR5;
050210090302               wx += 1;
050220090302
050230090302             endif;
050240090302
050250090302             reade  %kds(k05tntbe01 : 2)  TNTBE000;
050260090302
050270090302           EndDo;
050280090302
050290090302         ENDIF;
050300110411
050310110411         // >?Particolarità consegna in tab. "PXA"?
050320110411         IF  $TabPXA2 = *on;
050330110411
050340170517           clear  k05tntbe01;
050350110411           k_TBEcod = 'PXA';
050360110411           k_TBEke1 = DB0gma;
050370110411           k_TBEke2 = '001            ';
050380110411           clear  k_TBElin;
050390110411           clear  k_TBEsif;
050400110411           setll  %kds(k05tntbe01 : 3)  TNTBE000;
050410110411           reade  %kds(k05tntbe01 : 2)  TNTBE000;
050420110411           clear  wx;
050430110411
050440110411           DoW  not  %eof(TNTBE01L)  and  TBEke2 <= '006            ';
050450110411
050460110411             if  TBEatb = *blank;
050470110411
050480110411               // -?Stampa della riga separatrice iniziale?
050490110411               if  wx = *zero;
050500110411                 §LO1   = §PS1 + 0.050;
050510110411                 write  A4prtLO;
050520110411                 §PS1   = §LO1;
050530110411                 $First = 'G';
050540110411               endif;
050550110411
050560110411               // -?Salta eventuali righe vuote?
050570110411               dow  %int( %trimr( TBEke2 ) ) > wx + 1;
050580110411                 clear  Par5;
050590110411                 exsr  StampaAR5;
050600110411                 wx += 1;
050610110411               enddo;
050620110411
050630110411               // -?Stampa la singola riga di testo?
050640110411               Par5   = TBEuni;
050650110411               exsr  StampaAR5;
050660110411               wx += 1;
050670110411
050680110411             endif;
050690110411
050700110411             reade  %kds(k05tntbe01 : 2)  TNTBE000;
050710110411
050720110411           EndDo;
050730110411
050740110411         ENDIF;
050750090302
050760081212      /end-free
050770081127      *
050780031216sel 1c                   select
050790070306      * >
050800070306x   1c***                other
050810070306     c***                eval      $First = *off
050820070306     c***                clear                   Par5
050830070306     c***                exsr      StampaAR5
050840070306      *
050850070306e   1c                   endsl
050860061102      *
050870071114      * > Raccogliere Codice Fiscale e/o Partita Iva di ...
050880071114if  1c                   if             $No_CF = *on
050890071114     c                             or   $No_PI = *on
050900070320     c                   eval      §LO1   = §PS1 + 0.050
050910070320     c                   write     A4prtLO
050920070320     c                   eval      §PS1   = §LO1
050930070321     c                   eval      $First = 'G'
050940061102     c                   eval      Par5   = 'Spedizione n°'
050950061102     c                                    + %editw(DB0lnp:'0   ')
050960061102     c                                    + %editw(DB0nrs:'0  ')
050970061102     c                                    + ' '
050980061102     c                                    + %editc(DB0nsp:'Z')
050990061102     c                                    + ' del '
051000061102     c                                    + %editc(Pdsp:'Y')
051010061102     c                   exsr      StampaAR5
051020071114if  2c                   if        $No_PI = *on
051030071114     c*** (già fatto:)   eval      $First = 'G'
051040071114     c                   eval      Par5   = 'raccogliere +
051050071114     c                                      Partita I.v.a. di "'
051060071114     c                                    + %trim(wRSxCF)
051070071114     c                                    + '"'
051080071114     c                   exsr      StampaAR5
051090120504if  3c                   if        $No_CF = *off
051100120504     c*** (già fatto:)   eval      $First = 'G'
051110120504     c                   eval      Par5   = 'Codice Fiscale di "'
051120120504     c                                    + %trim(wRSxCF)
051130120504     c                                    + '": '
051140120504     c                                    + wB0cfi
051150120504     c                   exsr      StampaAR5
051160120504e   3c                   endif
051170071114e   2c                   endif
051180120503if  2c                   if        $No_CF = *on
051190120503     c*** (già fatto:)   eval      $First = 'G'
051200120503     c                   eval      Par5   = 'raccogliere +
051210120503     c                                      Codice Fiscale di "'
051220120503     c                                    + %trim(wRSxCF)
051230120503     c                                    + '"'
051240120503     c                   exsr      StampaAR5
051250120503e   2c                   endif
051260070306e   1c                   endif
051270070306      *
051280070306      * > Note bolla (dal file FIARN00F)
051290070306      *   (posizionamento e 1ª lettura già effettuate nella subr.
051300070306      *    "CheckFIARN")
051310070306if  1c                   if        $FIARN = *on
051320070320     c                   eval      §LO1   = §PS1 + 0.050
051330070320     c                   write     A4prtLO
051340070320     c                   eval      §PS1   = §LO1
051350070321     c                   eval      $First = 'G'
051360070319do  2c                   dou       %eof(FIARN11L)
051370070306     c                   eval      Par5   = ARNnob
051380070306     c                   exsr      StampaAR5
051390070319     c     K06ARN11      reade     FIARN000
051400070306e   2c                   enddo
051410070306e   1c                   endif
051420070319      *
051430070319      * > Consegne parziali precedenti (dal file FNART00F)
051440070406if  1c                   IF        $ConsParz     =  *on
051450101130     c*//(già fatto)?   exsr      SchConsParz
051460070320     c                   eval      §LO1   = §PS1 + 0.050
051470070320     c                   write     A4prtLO
051480070320     c                   eval      §PS1   = §LO1
051490070321     c                   eval      $First = 'G'
051500070319     c                   eval      xx     = 1
051510070319do  2c                   DoW            (xx      <= %elem($CP)
051520070319     c                             and  $CP(xx)  <  *all'9')
051530070319     c                   movel     $CP(xx)       Elem_CP
051540070319     c                   z-add     WCPdcm        w0080
051550070319     c     *iso          test(d)                 w0080                  31
051560070319if  3c                   if        NOT *in31
051570071025     c     *iso          movel     w0080         Data_EUR
051580071025     c     *eur          movel     Data_EUR      w0080
051590070319x   3c                   else
051600070319     c                   clear                   w0080
051610070319e   3c                   endif
051620140304 *...+*...1....+....2....+....3....+....4....+....5....+....6....+....7....+..
051630140305 Conse*nati colli 12345 in data 12/34/5678
051640140304 Conse*nati colli 12345 in data 12/34/5678 alle ore 12:34 - Firmatario: 123456
051650140304 Conse*nati colli 12345 il 12/34/5678 alle 12:34 a 1234567890123456789012345
051660140305 Conse*nati colli 12345 in data 12/34/5678 alle ore 12:34
051670140305      *                 Firmatario: 123456789012345678901234567890123456789012
051680070321     c                   eval      Par5   = 'Consegnati colli '
051690140305     c                                    + %editc(WCPncl:'Z')
051700140305     c                                    + ' in data '
051710070319     c                                    + %editc(w0080:'Y')
051720140304     c                   if        WCPocm > *zero
051730140304     c                   eval      Par5   = %trimr( Par5 )
051740140305     c                                    + ' alle ore '
051750140303     c                                    + %editw(WCPocm : '0 :  ')
051760140304     c                   endif
051770070319     c                   exsr      StampaAR5
051780140305     c                   if        WCPfir <> *blank
051790140305     c                   clear                   Par5
051800140305     c                   eval      %subst( Par5 : 24 ) = 'Firmatario: '
051810140305     c                                    + WCPfir
051820140305     c                   exsr      StampaAR5
051830140305     c                   endif
051840070319     c                   eval      xx     = xx + 1
051850070319e   2c                   EndDo
051860070319e   1c                   ENDIF
051870070321      *
051880070321      * > Reperita Comunicazione Anomalia
051890070321if  1c                   if        $C_A_  = *on
051900070321     c                   eval      §LO1   = §PS1 + 0.050
051910070321     c                   write     A4prtLO
051920070321     c                   eval      §PS1   = §LO1
051930070321     c                   eval      $First = 'G'
051940070321     c                   eval      Par5   = 'Presente Comunicazione +
051950070321     c                                                Anomalia'
051960070321     c                   exsr      StampaAR5
051970070321e   1c                   endif
051980031124      *
051990031124      * - stampo box
052000070320      *   (§PB1 è da reimpostare in §LO1: contiene il suo valore PRIMA
052010070320      *   della stampa delle varie linee separatrici)
052020070315     c                   eval      §LO1   = §PB1
052030070315     c                   eval      §LO2   = §PS1 + 0.070
052040031124     c                   write     A4prtAR53
052050031124      *
052060031124     c                   ENDSR
052070090924      /free
052080090924
052090090924       //--------------------------------------------------------------
052100090924       //?Prepara elenco per box "Note per il destinatario"            ?
052110090924       //--------------------------------------------------------------
052120090924       BEGSR  StampaNxD;
052130090924
052140090924         // - Stampa testata
052150090924         //$First = *on;
052160090924         //Par5   = '                    '
052170090924         //       + '                    '
052180090924         //       + '          '
052190090924         //       + c_NxD;
052200090924         //exsr  StampaAR5;
052210090924
052220090924         // - Memorizzazione posizione iniziale per il box
052230090924         //   (§PB1 sarà da reimpostare in §LO1 PRIMA della stampa del box)
052240090924         //§PB1 = §LO1;
052250090924
052260090924         // - Stampa della riga separatrice iniziale
052270090924         //§LO1   = §PS1 + 0.050;
052280090924         //write  A4prtLO;
052290090924         //§PS1   = §LO1;
052300090924
052310090924         // - Stampa la singola riga di testo
052320090925         $First = '1';
052330090924         Par5 = 'DECODER DIGITALE TERRESTRE OMAGGIO DA SKY PER AVER +
052340090924                 ADERITO ALL''OFFERTA';
052350090924         exsr  StampaAR5;
052360090924
052370090924         // - Memorizzazione posizione iniziale per il box
052380090924         //   (§PB1 sarà da reimpostare in §LO1 PRIMA della stampa del box)
052390090924         §PB1 = §LO1;
052400090924
052410090924         // - Stampa box
052420090924         //   (§PB1 è da reimpostare in §LO1: contiene il suo valore PRIMA
052430090924         //   della stampa delle varie linee separatrici)
052440090924         §LO1 = §PB1;
052450090925         §LO2 = §PS1 + 0.050;
052460090924         write  A4prtAR53;
052470090924
052480090924       ENDSR;
052490090924
052500090924      /end-free
052510030515      *
052520030515      *---------------------------------------------------------------*
052530050217      *?Prepara elenco per box - "Bancali da rendere al mittente"    ?*
052540150224      *?                         » NON PIÙ «                         ?*
052550050217      *?                       - "Dati per destinatario supermercato"?*
052560150224      *?                         » NON PIÙ «                         ?*
052570050217      *?                       - "Promemoria per autotrasportatore"  ?*
052580030515      *---------------------------------------------------------------*
052590030515     c     ListaAR5      BEGSR
052600030515      *
052610060921     c                   eval      $FIAR5 = *on
052620030515      *
052630030515      * Ripulisco i campi utilizzati in seguito
052640030515     c                   clear                   AR5trd
052650030515     c                   clear                   AR5dac
052660150304     c*//                clear                   dAR5ban
052670030717     c                   clear                   dAR5bnb
052680030903     c                   clear                   dAR5rcc
052690030515     c                   clear                   dAR5scr
052700030515     c                   reset                   $First
052710030515     c                   reset                   $FirstSCR
052720030515     c                   reset                   SAVtrd
052730030515      *
052740030515      * Posizionamento iniziale
052750040301if  1c                   if        DUTlpo = 'S'
052760040531     c     KLDV          setgt     FIAR531C
052770040301x   1c                   else
052780040607     c     KLDV          setgt     FIAR5000
052790040301e   1c                   endif
052800030515      *
052810030515      * Ciclo di lettura del file FIAR5
052820030515      *
052830030515do  1c                   DO        *Hival
052840030515      *
052850040607if  2c                   if        DUTlpo = 'S'
052860040531     c     KLDV          readpe    FIAR531C
052870040607x   2c                   else
052880040607     c     KLDV          readpe    FIAR5000
052890040607e   2c                   endif
052900040301      *
052910040301if  2c                   if           (DUTlpo =  'S'
052920040531     c                             and %eof(FIAR531C)
052930040301     c                             OR  DUTlpo <> 'S'
052940040301     c                             and %eof(FIAR501L))
052950030515     c                   leave
052960030515e   2c                   endif
052970030515if  2c                   if        AR5atb <> *blanks
052980030515     c                   iter
052990030515e   2c                   endif
053000030515      *
053010030515      * Controllo se devo stampare l'intestazione al nuovo box
053020030515if  2c                   if        AR5trd <> SAVtrd
053030030515      *   Stampa del precedente box (se stampata almeno una riga)
053040030910if  3c                   if             SAVtrd <> *hival
053050030910     c                             and  $First =  *off
053060030911     c                   eval      §LO2   = §PS1 + 0.050
053070030515     c                   write     A4prtAR53
053080030515e   3c                   endif
053090030515     c                   eval      $First =  *on
053100030515     c                   eval      SAVtrd =  AR5trd
053110030515e   2c                   endif
053120030515      *
053130150224sel 2c                   SELECT
053140150224      *//*
053150150224      *//* > Bancali da rendere al mittente
053160150224      *//*   (unico record, sequenzialmente prima dei rec. "SCR")
053170150224w   2c*//                WHEN      %subst(DB0gva:1:1) = 'B'
053180150224     c*//                          and    AR5trd      = 'BAN'
053190150224     c*//                movel     AR5uni        dAR5ban
053200150224      *//* - stampo testata
053210150224if  3c*//                if           (§AR5tba <> *zeros
053220150224     c*//                          or  §AR5tb2 <> *zeros)
053230150224     c*//                eval      Par5   = 'BANCALI DA RIPORTARE A MAGAZZINO E'
053240150224     c*//                                 + ' DA RENDERE AL MITTENTE           '
053250150224     c*//                exsr      StampaAR5
053260150224e   3c*//                endif
053270150224      *//* - stampo 1ª riga
053280150224if  3c*//                if        §AR5tba <> *zeros
053290150224     c*//                clear                   Par5
053300150224     c*//                clear                   Wtba
053310150224     c*//                eval      Wtba   = §AR5tba
053320150224     c*//                exsr      Tab_TBA
053330150224     c*//                eval      Par5   = §TBAdesc + 'N. '
053340150224     c*//                                 + %editc(§AR5nba : 'Z')
053350150224     c*//                exsr      StampaAR5
053360150224e   3c*//                endif
053370150224      *//* - stampo 2ª riga
053380150224if  3c*//                if        §AR5tb2 <> *zeros
053390150224     c*//                clear                   Par5
053400150224     c*//                clear                   Wtba
053410150224     c*//                eval      Wtba   = §AR5tb2
053420150224     c*//                exsr      Tab_TBA
053430150224     c*//                eval      Par5   = §TBAdesc + 'N. '
053440150224     c*//                                 + %editc(§AR5nb2 : 'Z')
053450150224     c*//                exsr      StampaAR5
053460150224e   3c*//                endif
053470150224      *//*
053480150224      *//* > Bancali da riportare a magazzino ("Oltre 20 colli")
053490150224      *//*   (unico record, sequenzialmente prima dei rec. "SCR",
053500150224      *//*    alternativo a "BAN" = bancali da rendere al mittente).
053510150224w   2c*//                WHEN      %subst(DB0gva:1:1) = 'O'
053520150224     c*//                          and    AR5trd      = 'BNB'
053530150224     c*//                movel     AR5uni        dAR5bnb
053540150224      *//* - stampo testata
053550150224if  3c*//                if           (§AR5btba <> *zeros
053560150224     c*//                          or  §AR5btb2 <> *zeros)
053570150224     c*//                eval      Par5   = 'BANCALI DA RIPORTARE A MAGAZZINO  '
053580150224     c*//                exsr      StampaAR5
053590150224e   3c*//                endif
053600150224      *//* - stampo 1ª riga
053610150224if  3c*//                if        §AR5btba <> *zeros
053620150224     c*//                clear                   Par5
053630150224     c*//                clear                   Wtba
053640150224     c*//                eval      Wtba   = §AR5btba
053650150224     c*//                exsr      Tab_TBA
053660150224     c*//                eval      Par5   = §TBAdesc + 'N. '
053670150224     c*//                                 + %editc(§AR5bnba : 'Z')
053680150224     c*//                exsr      StampaAR5
053690150224e   3c*//                endif
053700150224      *//* - stampo 2ª riga
053710150224if  3c*//                if        §AR5btb2 <> *zeros
053720150224     c*//                clear                   Par5
053730150224     c*//                clear                   Wtba
053740150224     c*//                eval      Wtba   = §AR5btb2
053750150224     c*//                exsr      Tab_TBA
053760150224     c*//                eval      Par5   = §TBAdesc + 'N. '
053770150224     c*//                                 + %editc(§AR5bnb2 : 'Z')
053780150224     c*//                exsr      StampaAR5
053790150224e   3c*//                endif
053800150224      *//* - stampo 3ª riga
053810150224if  3c*//                if        §AR5bcor <> *zeros
053820150224     c*//                clear                   Par5
053830150224     c*//                eval      Par5   = C_CoOr
053840150224     c*//                                 + %editc(§AR5bcor : 'K')
053850150224     c*//                exsr      StampaAR5
053860150224e   3c*//                endif
053870030515      *
053880030515      * > Destinatario supermercato
053890030515w   2c                   WHEN      (DB0tc1 = 'S' or DB0tc2 = 'S')
053900030515     c                             and AR5trd  = 'SCR'
053910030515     c                   movel     AR5uni        dAR5scr
053920030515      * - stampo testata
053930030515      *   (se subr. richiamata solo per verifica esistenza dati: esco)
053940030515if  3c                   if        $First =  *on
053950031124     c                   eval      Par5   = C_DeSM
053960030515     c                   exsr      StampaAR5
053970030515     c                   eval      $FirstSCR  = *off
053980030515e   3c                   endif
053990030515      * - stampo righe
054000030515     c                   clear                   Par5
054010030624     c                   eval      *in38 = *on
054020030515if  3c                   if           AR5dac <> SAVdac
054030030515     c                             or AR5orc <> SAVorc
054040030515     c                   eval      SAVdac = AR5dac
054050030515     c                   eval      SAVorc = AR5orc
054060030515      * - - giro data modifica
054070030515     c                   z-add     AR5dac        w0080
054080070315     c     *iso          test(d)                 w0080                  31
054090070315if  4c                   if        NOT *in31
054100071025     c     *iso          movel     w0080         Data_EUR
054110071025     c     *eur          movel     Data_EUR      w0080
054120070315x   4c                   else
054130070315     c                   clear                   w0080
054140070315e   4c                   endif
054150030515     c                   eval      Par5 = 'Modifica del '
054160030515     c                                  + %editc(w0080 : 'Y')
054170030515     c                                  + ' '
054180030515     c                                  + %editw(AR5orc : '  :  :  ')
054190030515     c                                  + ' '
054200040223     c                                  + §AR5Spru
054210030515     c                   exsr      StampaAR5
054220030515e   3c                   endif
054230040223     c                   eval      Par5 = §AR5Snom
054240030911     c                                  + '   Tel.: '
054250040223     c                                  + §AR5Stel
054260030911     c                   exsr      StampaAR5
054270040223     c                   eval      Par5 = §AR5Smot
054280030515     c                   exsr      StampaAR5
054290030624     c                   eval      *in38 = *off
054300031124      *
054310031124      * > Se destinatario assente inserire il plico in buca lettere
054320031124      *   (già stampato in box "Promemoria per autotrasportatore")
054330031124w   2c***                WHEN          DB0gma  = Ktab7V_CB
054340031124     c***                ...       ...
054350030903      *
054360030903      * > O.R.M. per ritiro contestuale alla consegna
054370030903      *   (unico record)
054380050510      *   Verrà stampato come ultimo box
054390050510      *   - prima del (eventuale) DDT del mittente
054400050510      *   - prima della copia per il destinatario
054410030910w   2c***                WHEN          DB0gma  = Ktab7V_RC
054420030910     c***                          and AR5trd  = 'RCC'
054430030903     c***                ...       ...
054440030515      *
054450030515e   2c                   ENDSL
054460030515      *
054470030515e   1c                   ENDDO
054480030515      *
054490030515      * Stampa del (ultimo) box (se stampata almeno una riga)
054500030515if  1c                   if        $First = *off
054510030911     c                   eval      §LO2   = §PS1 + 0.050
054520030515     c                   write     A4prtAR53
054530030515e   1c                   endif
054540030515      *
054550030515      * > Destinatario supermercato
054560030515      *   (stampa del box vuoto per memo se non già stampata almeno
054570030515      *   una riga)
054580030515if  1c                   IF        (DB0tc1 = 'S' or DB0tc2 = 'S')
054590030515     c                             and              $FirstSCR = *on
054600030515      * - stampo testata
054610030515     c                   eval      $First = *on
054620031124     c                   eval      Par5   = C_DeSM
054630030515     c                   exsr      StampaAR5
054640030515     c                   eval      $FirstSCR  = *off
054650030515      * - stampo 2 righe vuote
054660030515     c                   clear                   Par5
054670030515do  2c                   do        2
054680030515     c                   exsr      StampaAR5
054690030515e   2c                   enddo
054700030515      * - stampo box
054710030911     c                   eval      §LO2   = §PS1 + 0.050
054720030515     c                   write     A4prtAR53
054730030515e   1c                   ENDIF
054740070123      *
054750070123     c                   eval      $FIAR5 = *off
054760030515      *
054770030515     c                   ENDSR
054780030515      *
054790030515      *---------------------------------------------------------------*
054800050217      *?Stampa singola riga relativa a FIAR5                         ?*
054810030515      *---------------------------------------------------------------*
054820030515     c     StampaAR5     BEGSR
054830030515      *
054840030515      * Controllo se superata l'ultima riga
054850030515     c                   exsr      Test_OF
054860030515      *
054870030515      * Stampa del singolo record
054880030909sel 1c                   select
054890030909      *
054900030909w   1c                   when      $First = *off
054910030909      * - Incremento parametri per posizionamento nuova riga
054920030911     c                   eval      §PS1   = §PS1 + 0.150
054930030909     c                   write     A4prtAR52
054940030909      *
054950030909w   1c                   when      $First = *on
054960030515      * - Incremento parametri per posizionamento nuovo box
054970030515      *   e relativa intestazione
054980030911     c                   eval      §LO1   = §LO2 + 0.150
054990030911     c                   eval      §PS1   = §LO1 + 0.150
055000030515     c                   write     A4prtAR51
055010030515     c                   eval      $First = *off
055020060524      *
055030060524w   1c                   when      $First = '2'
055040060524      * - Incremento parametri per posizionamento nuova riga
055050060524      *   nel box "Promemoria per autotrasportatore"
055060060524     c                   eval      §PS1   = §PS1 + 0.150
055070060524     c                   write     A4prtAR51
055080060524     c                   eval      $First = *off
055090030909      *
055100030909w   1c                   when      $First = 'A'
055110030909      * - Incremento parametri per posizionamento nuovo box
055120030909      *   e relativa intestazione
055130030911     c                   eval      §LO1   = §LO2 + 0.150
055140030909     c                   eval      §PS1   = §LO1 + 0.200
055150030909     c                   write     A4prtAR5a
055160030909     c                   eval      $First = *off
055170150226      *//*
055180150226w   1c*//                when      $First = 'B'
055190150226      *//* - Incremento parametri per posizionamento nuovo box
055200150226      *//*   ed intestazione "Promemoria per autotrasportatore"
055210150226     c*//                eval      §LO1   = §LO2 + 0.150
055220150226     c*//                eval      §PS1   = §LO1 + 0.125
055230150226     c*//                write     A4prtAR5b
055240150226     c*//                eval      $First = 'C'
055250150226      *//*
055260150226w   1c*//                when      $First = 'C'
055270150226      *//* - Incremento parametri per posizionamento nuova riga
055280150226      *//*   nel box "Promemoria per autotrasportatore"
055290150226     c*//                eval      §PS1   = §PS1 + 0.250
055300150226     c*//                write     A4prtAR5c
055310070321      *
055320070321w   1c                   when      $First = 'G'
055330070321      * - Incremento parametri per posizionamento nuova riga
055340070321      *   nel box "Promemoria per autotrasportatore"
055350070321     c                   eval      §PS1   = §PS1 + 0.170
055360070321     c                   write     A4prtAR5g
055370030909      *
055380030909e   1c                   endsl
055390030515      *
055400030515     c                   ENDSR
055410030910      *
055420030910      *---------------------------------------------------------------*
055430050217      *?Stampa box "Ritiro Contestuale alla Consegna" da FIAR5/RCC   ?*
055440030910      *---------------------------------------------------------------*
055450030910     c     StampaRCC     BEGSR
055460030910      *
055470030910      * > O.R.M. per ritiro contestuale alla consegna
055480030910      *   (unico record)
055490031124      *   Verrà stampato come ultimo box (prima della copia per il
055500031124      *     destinatario)
055510030910      *
055520030910     c                   clear                   dAR5rcc
055530030910      *
055540030910     c                   eval      AR5trd = 'RCC'
055550040301if  1c                   if        DUTlpo = 'S'
055560040531     c     K05AR501      chain     FIAR531C
055570040301x   1c                   else
055580030910     c     K05AR501      chain     FIAR5000
055590040301e   1c                   endif
055600040301      *
055610071008if  1c                   if           (DUTlpo =  'S'
055620040531     c                             and %found(FIAR531C)
055630040301     c                             OR  DUTlpo <> 'S'
055640040301     c                             and %found(FIAR501L))
055650040301     c                             and AR5atb =  *blanks
055660030910     c                   movel     AR5uni        dAR5rcc
055670071015     c                   eval      ORMpoe = §AR5Rpoe
055680071015     c                   eval      ORMnsr = §AR5Rnsr
055690071015     c                   eval      ORMnor = §AR5Rnor
055700071015     c                   eval      ORMnrv = §AR5Rnrv
055710071015     c                   eval      Pbt2a  = 'RITIRO CONTESTUALE ALLA +
055720071015     c                                       CONSEGNA'
055730071015     c                   exsr      StampaORM
055740071015e   1c                   endif
055750071015      *
055760071015     c                   ENDSR
055770071015      *
055780071015      *---------------------------------------------------------------*
055790071015      *?Stampa box "Ritiro Porto Franco PrePagato"                   ?*
055800071015      *---------------------------------------------------------------*
055810071015     c     StampaRPFPP   BEGSR
055820071015      *
055830071015      * > O.R.M. per ritiro porto franco prepagato
055840071015      *   Verrà stampato come ultimo box (prima della copia per il
055850071015      *     destinatario)
055860071015      *
055870071015     c*** già fatto:     clear                   dsBL4M
055880071015     c*** già fatto:     exsr      RepAR4_M
055890071015      *
055900071015if  1c                   if             §B4poe  > *zeros
055910071015     c                             and  §B4nor  > *zeros
055920071015     c                   eval      ORMpoe = §B4poe
055930071015     c                   eval      ORMnsr = §B4nsr
055940071015     c                   eval      ORMnor = §B4nor
055950071015     c                   eval      ORMnrv = §B4nrv
055960071015     c                   eval      Pbt2a  = 'RITIRO PORTO FRANCO +
055970071015     c                                       PREPAGATO'
055980071015     c                   exsr      StampaORM
055990071015e   1c                   endif
056000071015      *
056010071015     c                   ENDSR
056020071015      *
056030071015      *---------------------------------------------------------------*
056040071015      *?Stampa dati O.R.M. in box "Ritiro Contestuale alla Consegna" ?*
056050071015      *?              o in quello "Ritiro Porto Franco Prepagato"    ?*
056060071015      *---------------------------------------------------------------*
056070071015     c     StampaORM     BEGSR
056080071015      *
056090071015     c                   eval      $FIAR5 = *on
056100071015      *
056110071015if  2c                   if        not %open(FNORM01L)
056120071015     c                   open      FNORM01L
056130071015e   2c                   endif
056140071015     c     K04ORM01      chain     FNORM000
056150071015      *
056160071015if  2c                   IF        %found(FNORM01L)
056170071015      *
056180071015      * Reperisco la posizione (in pollici) in cui posizionarmi
056190071015      *   per la stampa in base all'ultima riga stampata
056200071015      *   se non ho ancora stampato box
056210071015cas 3c     §LO2          caseq     *zeros        Rep_§LO2
056220071015e   3c                   endcs
056230071025      *
056240071015      * - stampo testata
056250071025      *   (con il n° O.R.M.)
056260071015     c                   eval      $First = 'A'
056270071015     c*** già impostato: eval      Pbt2a  = 'RITIRO CONTESTUALE ALLA +
056280071015     c***  "      "                          CONSEGNA'
056290071015     c                   eval      Pbt2b  = 'O.R.M. numero '
056300071015     c                                    + %editw(ORMpoe : '0   ')
056310071015     c                                    + ' '
056320071015     c                                    + %editw(ORMnsr : '0  ')
056330071015     c                                    + '  '
056340071015     c                                    + %editc(ORMnor : 'Z')
056350071015     c                                    + ' '
056360071015     c                                    + %editw(ORMnrv : '0  ')
056370071015     c                   exsr      StampaAR5
056380071025      *
056390071015      * - stampo i dati dell'O.R.M.
056400071015     c                   exsr      QUM_FnOrm
056410140410     c                   eval      Par5   = 'Quantità      : '
056420071015     c                                    + %trim($Qum(1)) + '    '
056430071015     c                                    + %trim($Qum(2)) + '    '
056440071015     c                                    + %trim($Qum(3))
056450071015     c                   exsr      StampaAR5
056460080228if  3c                   if        DB0tbo = '8'
056470140410     c                   eval      Par5   = 'Mittente      : '
056480080228     c                                    + ORMrsr
056490080228x   3c                   else
056500140410     c                   eval      Par5   = 'Destinatario  : '
056510071015     c                                    + ORMrsc
056520080228e   3c                   endif
056530071015     c                   exsr      StampaAR5
056540080228if  3c                   if        DB0tbo = '8'
056550140410     c                   eval      Par5   = '                '
056560080228     c                                    + ORMinr
056570080228x   3c                   else
056580140410     c                   eval      Par5   = '                '
056590071015     c                                    + ORMinc
056600080228e   3c                   endif
056610071015     c                   exsr      StampaAR5
056620080228if  3c                   if        DB0tbo = '8'
056630140410     c                   eval      Par5   = '                '
056640080228     c                                    + ORMcar + ' '
056650080228     c                                    + ORMlor + ' '
056660080228     c                                    + ORMprr + ' '
056670080228     c                                    + ORMnar
056680080228x   3c                   else
056690140410     c                   eval      Par5   = '                '
056700071015     c                                    + ORMcac + ' '
056710071015     c                                    + ORMloc + ' '
056720071015     c                                    + ORMprc + ' '
056730071015     c                                    + ORMnac
056740080228e   3c                   endif
056750071015     c                   exsr      StampaAR5
056760101022             // -?SE Ritiro Porto Franco Prepagato:?
056770080228if  3c                   if        DB0tbo = '8'
056780101022      /free
056790101022               // -?stampo il referente ed il relativo telefono?
056800101022               // -?(Comunque! Ci sono sempre!)?
056810140410               Par5   = 'Referente     : ' + ORMrer + '   '
056820101022                      + 'Telefono: '     + ORMter;
056830101022               exsr  StampaAR5;
056840101022      /end-free
056850140410     c                   eval      Par5   = 'Ordinante     : '
056860080228     c                                    + ORMrso
056870080228     c                   exsr      StampaAR5
056880140410     c                   eval      Par5   = 'Destinatario  : '
056890080303     c                                    + ORMrsc
056900080303     c                   exsr      StampaAR5
056910080228e   3c                   endif
056920071015      * - stampo le note
056930071015      *   (Comunque! Non solo "se ci sono". Al limite: scarto la 1ª.)
056940140410     c                   eval      Par5   = 'Note          : '
056950071015if  3c                   if        ORMno1 <> *blanks
056960071015     c                   eval      Par5   = %trim(Par5)  + ' '
056970071015     c                                    + ORMno1 + ' ' + ORMno2
056980071015x   3c                   else
056990071015     c                   eval      Par5   = %trim(Par5)  + ' '
057000071015     c                                    + ORMno2
057010071015e   3c                   endif
057020071015     c                   exsr      StampaAR5
057030071025      *
057040140410      * -?SE Ritiro Porto Franco Prepagato:?
057050071025if  3c                   IF        DB0tbo = '8'
057060140410      *
057070140410      * - stampo orari apertura (SE ci sono)
057080140410     c                   exsr      sr_OrariApert
057090140410      *
057100140410      * - stampo data & ora ritiro
057110071025     c                   z-add     ORMdar        w0080
057120071025     c     *iso          test(d)                 w0080                  31
057130071025if  4c                   if        NOT *in31
057140071025     c     *iso          movel     w0080         Data_EUR
057150071025     c     *eur          movel     Data_EUR      w0080
057160071025x   4c                   else
057170071025     c                   clear                   w0080
057180071025e   4c                   endif
057190140410     c                   eval      Par5   = 'Data ritiro   : '
057200071025     c                                    + %editc(w0080:'Y')
057210071025if  4c                   if        ORMorr > 1230
057220071025     c                   eval      Par5   = %trimr(Par5)
057230071025     c                                    + '          '
057240140410     c                                    + 'Ora ritiro: '
057250071025     c                                    + %editw(ORMorr:'0 :  ')
057260071025     c                   exsr      StampaAR5
057270071025x   4c                   else
057280071025     c                   exsr      StampaAR5
057290071025     c                   clear                   Par5
057300071025     c                   eval      %subst(Par5:50)
057310071025     c                                    = 'Ora ritiro: '
057320071025     c                                    + %editw(ORMorr:'0 :  ')
057330071025     c                   write     A4prtAR51
057340071025e   4c                   endif
057350140410      *
057360071025e   3c                   ENDIF
057370071025      *
057380071015      * - imposto codice a barre O.R.M.
057390071015     c                   movel     BarCodeORM    Pbco
057400071015      * - verifico se codice a barre da stampare
057410071015     c                   eval      *in52  =  (Pbco <> *zeros)
057420071025      *
057430071015      * - stampo box
057440071015     c                   eval      §LO2   = §PS1 + 0.050
057450071015if  3c                   if        NOT *in52
057460071015     c                   write     A4prtAR53
057470071015x   3c                   else
057480071025     c                   eval      §LV1   = §LO2 - §LO1
057490071015     c                   eval      §PS2   = §LO2 - 0.500
057500071015     c                   write     A4prtAR54
057510071015e   3c                   endif
057520071015      *
057530071015e   2c                   ENDIF
057540071015      *
057550071015     c                   eval      $FIAR5 = *off
057560071015      *
057570071015     c                   ENDSR
057580100513      /free
057590140410
057600140410       //--------------------------------------------------------------
057610140410       //?Reperimento, impostazione e stampa degli Orari di Apertura   ?
057620140410       //--------------------------------------------------------------
057630140410       BEGSR  sr_OrariApert;
057640140410
057650140410         clear  dOREorari;
057660140410
057670140410         // -?Recupero dati da estensione FNORE (trc "O ")?
057680140410         if  NOT  %open(FNORE01L);
057690140410           open  FNORE01L;
057700140410         endif;
057710140410         k_OREpoe = ORMpoe;
057720140410         k_OREnsr = ORMnsr;
057730140410         k_OREnor = ORMnor;
057740140410         k_OREnrv = ORMnrv;
057750140410         k_OREtrc = 'O ';
057760140410         chain  %kds(keyFNORE01)  FNORE000;
057770140410
057780140410         // -?...NON trovati => uscita dalla subroutine?
057790140410         if  Not  %found(FNORE01L);
057800140410           leavesr;
057810140410         endif;
057820140410
057830140410         // -?...Trovati => verifica degli orari?
057840140410         dOREorari = OREdati;
057850140410
057860140410         Select;
057870140410
057880140410           // -?C'è un solo orario "ALLE"?
057890140410           When  §OREoraMda = *zero  and  §OREoraMa > *zero  and
057900140410                 §OREoraPda = *zero  and  §OREoraPa = *zero;
057910140410             Par5 = 'Orari apertura: ' +
057920140410                    'Fino alle ' + %editw( §OREoraMa : ' 0:  ' );
057930140410           When  §OREoraMda = *zero  and  §OREoraMa = *zero  and
057940140410                 §OREoraPda = *zero  and  §OREoraPa > *zero;
057950140410             Par5 = 'Orari apertura: ' +
057960140410                    'Fino alle ' + %editw( §OREoraPa : ' 0:  ' );
057970140410
057980140410           // -?C'è un solo orario completo (per 1°)?
057990140410           When  §OREoraMda > *zero  and  §OREoraMa > *zero  and
058000140410                 §OREoraPda = *zero  and  §OREoraPa = *zero;
058010140410             Par5 = 'Orari apertura: ' +
058020140410                    %editw( §OREoraMda : ' 0:  ' ) + '  ' +
058030140410                    %editw( §OREoraMa  : ' 0:  ' );
058040140410
058050140410           // -?Ci sono due orari completi?
058060140410           When  §OREoraMda > *zero  and  §OREoraMa > *zero  and
058070140410                 §OREoraPda > *zero  and  §OREoraPa > *zero;
058080140410             Par5 = 'Orari apertura: ' +
058090140410                    %editw( §OREoraMda : ' 0:  ' ) + '  ' +
058100140410                    %editw( §OREoraMa  : ' 0:  ' ) + ' - ' +
058110140410                    %editw( §OREoraPda : ' 0:  ' ) + '  ' +
058120140410                    %editw( §OREoraPa  : ' 0:  ' );
058130140410
058140140410         EndSl;
058150140410
058160140410         exsr  StampaAR5;
058170140410
058180140410       ENDSR;
058190100513
058200100513       //--------------------------------------------------------------
058210100513       //?Verifica se presente ORM con preavviso mail                  ?
058220100513       //--------------------------------------------------------------
058230100513       BEGSR  sr_CheckORM_PrevvMail;
058240100514
058250100514         //clear  dOrm01               (già fatto)
058260100513
058270100513         // -?Reperimento num. ORM (se NON già reperito)?
058280100513         if  DB0tbo <> '8';
058290100513           exsr  RepAR4_M;
058300100513         endif;
058310100513
058320100513         // -?ORM inesistente: inutile proseguire?
058330100513         if  §B4poe = *zero    or   §B4nor = *zero;
058340100513           leavesr;
058350100513         endif;
058360100513
058370100513         // -?Verifica presenza O.R.M.?
058380100513         if  Not  %open(FNORM01L);
058390100513           open  FNORM01L;
058400100513         endif;
058410100513
058420100513         ORMpoe = §B4poe;
058430100513         ORMnsr = §B4nsr;
058440100513         ORMnor = §B4nor;
058450100513         ORMnrv = §B4nrv;
058460100513
058470100513         chain  K04ORM01  FNORM000;
058480100513
058490100513         // -?ORM non reperito: inutile proseguire?
058500100513         if  Not  %found(FNORM01L);
058510100513           leavesr;
058520100513         endif;
058530100513
058540100513         // -?Impostazione flag per stampa porto "ASSEGNATO"?
058550100513         dOrm01 = ORMflo;
058560100513         $PreavvORM = (§ORMpre = 'M');
058570100513
058580100513       ENDSR;
058590100513
058600100513      /end-free
058610030515      *
058620030515      *---------------------------------------------------------------*
058630050217      *?Stampa dati DPD                                              ?*
058640030515      *---------------------------------------------------------------*
058650030515     c     StampaDPD     BEGSR
058660030515      *
058670080922      * (EX) Bar-Code - non più stampato come codice a barre
058680030515      * (1' 12 char di Riferimento Mittente Alfanumerico - se numerici)
058690060626      *---
058700061030      * Il barcode DPD è stato variato ora può essere anche
058710061030      *   alfanumerico ed è lungo 14
058720171116      * Si trova sul file FIPND00F (vedi campo PNDIPN).
058730061030      * I controlli che faceva prima sono da cambiare
058740061030      *---
058750061030      *
058760060626      * nuovi controlli per nuovo barcode DPD
058770171116if  1c                   if        PNDipn <> *blanks
058780061030      * - controllo se vecchio o nuovo barcode
058790171116if  2c                   if        %subst(PNDipn:14:1) = *blanks
058800061030      * - - vecchio
058810171116     c                   eval      Pbcd2 = '01 ' + %subst(PNDipn:1:3) +
058820171116     c                                     ' '   + %subst(PNDipn:4:8)
058830061030      * - - nuovo
058840061030x   2c                   else
058850171116     c                   eval      Pbcd2 = '01 ' + %subst(PNDipn:1:4) +
058860171116     c                                     ' '   + %subst(PNDipn:5:10)
058870061030e   2c                   endif
058880061030e   1c                   endif
058890080922      /free
058900080922         // Impostazione data
058910080922         PdpA = 'Date ' + %editc(Pdsp:'Y');
058920080922
058930080922         // Impostazione numero colli
058940080922         PdpB = 'Parcel N. ' + %editc(Pncl:'Z');
058950080922
058960080922         // Impostazione numero parcel
058970171116         PdpC = PNDipn;
058980080922
058990080922         // Impostazione "costante" da stampare tra le Note
059000080922         PdpD = 'Controllare la corrispondenza';
059010080922         PdpE = 'del destinatario con l''indirizzo';
059020080922         PdpF = 'dell''etichetta DPD.';
059030080922
059040080922         // Impostaz. posizioni sulla copia per il vettore già stampata
059050080922         §DP1 = wPosIni + 1.100;
059060080922         §DP2 = §DP1 + 0.550;
059070080922         §DP3 = §DP2 + 0.120;
059080080922         §DP4 = §DP3 + 0.120;
059090080922         §DP5 = §DP4 + 0.120;
059100080922         §DP6 = §DP5 + 0.160;
059110080922         §DP7 = §DP6 + 0.130;
059120080922         §DP8 = §DP7 + 0.130;
059130080922      /end-free
059140080922      *
059150080922      * (inutile testare l'overflow perchè siamo in ex-A5)
059160080922     c                   write     A4prtDPD
059170030515      *
059180030515     c                   ENDSR
059190030515      *
059200030515      *---------------------------------------------------------------*
059210050217      *?Prepara elenco per box - Elenco "Package Tracking Number"    ?*
059220030515      *---------------------------------------------------------------*
059230030515     c     ListaFED      BEGSR
059240030515      *
059250030515      * Stampo il simbolo "FedEx" (sulla 1ª pagina):
059260030515      *   imposto le posizioni iniziali di stampa per le linee
059270030515      *   (orizzontali-§LOn e verticali-§LVn)
059280080912     c                   eval      §LO1   = 3.330
059290080912     c                   eval      §LO2   = §LO1 + 0.370
059300081023     c                   eval      §LV1   = §LO2 - 0.150
059310081023     c                   eval      §LV2   = §LO2 - 0.070
059320030515     c                   write     A4prtFED1
059330080818      *
059340080818      * Stampo le "costanti" di inizio lista
059350030515     c                   write     A4prtFED2
059360030515      *
059370030515      * Stampo elenco segnacolli FedEx
059380030709     c                   eval      §LO1   = §LO2 - 0.250
059390030515      * - per le bolle in partenza
059400030515if  1c                   IF           DB0tbo = 'P'
059410030515     c                             or DB0tbo = '8'
059420030515if  2c                   if        NOT %open(FNBLT01L)
059430030515     c                   open      FNBLT01L
059440030515e   2c                   endif
059450030515      * - per le bolle in arrivo
059460030515x   1c                   ELSE
059470070319if  2c                   if        NOT %open(FNART01L)
059480070319     c                   open      FNART01L
059490070319e   2c                   endif
059500030515e   1c                   ENDIF
059510070319if  1c                   if        NOT %open(FIARS01L)
059520070319     c                   open      FIARS01L
059530070319e   1c                   endif
059540030515     c                   eval      Wtrc = 'C'
059550030515     c                   clear                   Wnsc
059560030515     c                   clear                   Wawb
059570030515do  1c                   dou       Wawb = *blanks
059580030515     c                   exsr      StampaFED
059590030515e   1c                   enddo
059600051020      *
059610030515     c                   ENDSR
059620030515      *
059630030515      *---------------------------------------------------------------*
059640050217      *?Stampa singola riga relativa a FEDEX                         ?*
059650030515      *---------------------------------------------------------------*
059660030515     c     StampaFED     BEGSR
059670030515      *
059680030515      * Impostazione campi di stampa
059690030515     c                   clear                   Wawb
059700030515      * - Carico AWB FedEx
059710030515      * - - leggo segnacolli per bolle in partenza
059720030515if  1c                   IF           DB0tbo = 'P'
059730030515     c                             or DB0tbo = '8'
059740030515     c                   exsr      CarAWB_Blx
059750030515      * - - leggo segnacolli per bolle in arrivo
059760030515x   1c                   ELSE
059770030515     c                   exsr      CarAWB_Arx
059780030515e   1c                   ENDIF
059790030515     c     wAWB          cabeq     *blanks       End_FED
059800030515     c                   move      wAWB          Pptn
059810030515     c                   add       1             Psts
059820030515      *
059830030515      * Controllo se superata l'ultima riga
059840030515     c                   exsr      Test_OF
059850030709      *
059860030709      * Imposto le posizioni iniziali di stampa per le linee
059870030709      *   (orizzontali-§LOn e verticali-§LVn) e per i campi (§PSn)
059880030709     c                   eval      §LO1 = §LO1 + 0.250 - 0.008
059890030709     c                   eval      §LO2 = §LO1 + 0.250
059900030709     c                   eval      §LV1 = §LO1
059910030709     c                   eval      §LV2 = §LO2 - 0.100
059920030709     c                   eval      §PS1 = §LO1 + 0.200
059930030515      *
059940030515      * Stampa del singolo record
059950030515     c                   write     A4prtFED3
059960030515      *
059970030515     c     End_FED       ENDSR
059980030515      *
059990030515      *---------------------------------------------------------------*
060000051020      *?Reperimento AWB FedEx da file FNBLT e FIARS (bolle partenza) ?*
060010030515      *---------------------------------------------------------------*
060020030515     c     CarAWB_Blx    BEGSR
060030130215      *
060040130215      * Verifica se, al di là del codice bolla (vedi "$ "), esiste
060050130215      *   comunque una bolla mamma
060060130215     c                   exsr      sr_Mamma_FedEx
060070030515      *
060080030515      * Posizionamento
060090130215if  2c                   if        $Mamma = *off   and
060100130215     c                             $Mamma_FedEx = *off
060110040607     c     KLDVnsc       setgt     FNBLT000
060120070514x   2c                   else
060130070514     c     KLBLnsc       setgt     FNBLT000
060140070514e   2c                   endif
060150030515      *
060160030515      * Ciclo di lettura
060170030515do  1c                   DO        *hival
060180030515      *
060190130215if  2c                   if        $Mamma = *off   and
060200130215     c                             $Mamma_FedEx = *off
060210040607     c     KLDV          reade     FNBLT000
060220070514x   2c                   else
060230070514     c     KLBL          reade     FNBLT000
060240070514e   2c                   endif
060250030515if  2c                   if        %eof(FNBLT01L)
060260030515     c                   leave
060270030515e   2c                   endif
060280030515      * - per ogni segnacollo cerco l'AWB
060290060505     c     K05ARS01P     chain     FIARS000
060300060505if  2c                   if            %found(FIARS01L)
060310060505     c                             and %subst(ARSnot:1:1) >= *zeros
060320060505     c                   eval      wAWB = %subst(ARSnot:1:12)
060330060505     c                   eval      wNSC = ARSnsc
060340030515     c                   leave
060350030515e   2c                   endif
060360030515      *
060370030515e   1c                   ENDDO
060380030515      *
060390030515     c                   ENDSR
060400030515      *
060410030515      *---------------------------------------------------------------*
060420070319      *?Reperimento AWB FedEx da file FNART e FIARS (bolle arrivo)   ?*
060430030515      *---------------------------------------------------------------*
060440030515     c     CarAWB_Arx    BEGSR
060450130215      *
060460130215      * Verifica se, al di là del codice bolla (vedi "$ "), esiste
060470130215      *   comunque una bolla mamma
060480130215     c                   exsr      sr_Mamma_FedEx
060490030515      *
060500030515      * Posizionamento
060510130215if  2c                   if        $Mamma = *off   and
060520130215     c                             $Mamma_FedEx = *off
060530040607     c     KLDVnsc       setgt     FNART000
060540070514x   2c                   else
060550130215     c     KLBLnsc       setgt     FNART000
060560070514e   2c                   endif
060570030515      *
060580030515      * Ciclo di lettura
060590030515do  1c                   DO        *hival
060600030515      *
060610130215if  2c                   if        $Mamma = *off   and
060620130215     c                             $Mamma_FedEx = *off
060630040607     c     KLDV          reade     FNART000
060640070514x   2c                   else
060650070514     c     KLBL          reade     FNART000
060660070514e   2c                   endif
060670030515if  2c                   if        %eof(FNART01L)
060680030515     c                   leave
060690030515e   2c                   endif
060700030515      * - per ogni segnacollo cerco l'AWB
060710060505     c     K05ARS01A     chain     FIARS000
060720060505if  2c                   if            %found(FIARS01L)
060730030515     c                             and %subst(ARSnot:1:1) >= *zeros
060740030515     c                   eval      wAWB = %subst(ARSnot:1:12)
060750030515     c                   eval      wNSC = ARSnsc
060760030515     c                   leave
060770030515e   2c                   endif
060780030515      *
060790030515e   1c                   ENDDO
060800030515      *
060810030515     c                   ENDSR
060820130215      *
060830130215      /free
060840130215
060850130215       //--------------------------------------------------------------
060860130215       //?Verifica se la spedizione FedEx ha bolla mamma.              ?
060870130215       //--------------------------------------------------------------
060880130215       BEGSR  sr_Mamma_FedEx;
060890130215
060900130215         //$Mamma_FedEx = *off;          (già così)
060910130215
060920130215         If  Not $Mamma;
060930130215
060940130215           // -?Verifica se la spedizione ha comunque mamma?
060950130215           //  ?(al di là del codice bolla - vedi cod. "$ ")?
060960130215           if  not %open(FNLBL01L);
060970130215             open  FNLBL01L;
060980130215           endif;
060990130215           chain  (DB0aas : DB0lnp : DB0nrs : DB0nsp)  FNLBL000;
061000130215
061010130215           $Mamma_FedEx = %found(FNLBL01L);
061020130215
061030130215         EndIf;
061040130215
061050130215       ENDSR;
061060130215
061070130215      /end-free
061080040607      *
061090040607      *---------------------------------------------------------------*
061100080918      *?Stampa DDT / PACKING-LIST del mittente                       ?*
061110040607      *---------------------------------------------------------------*
061120040607     c     StampaDDT     BEGSR
061130130315      *
061140130315      * - Uscita in assenza del file FIAR801L
061150130315     c                   if        Not  $E_AR8
061160130315     c                   leavesr
061170130315     c                   endif
061180080825      *
061190080825      * Meglio togliere l'impostazione relativa ai segnacolli non seq.
061200080825      *   per far funzionare il 1° posizionamento nella subr. Rep_§LO2
061210080825     c                   reset                   $A4_x_ScNS
061220080918      /free
061230080923         *in45 = (DB0fa4 = 'P');
061240080923         *in46 = (DB0fa4 = 'P'   and   %rem(wNumLdv : 2) = *zero);
061250080918      /end-free
061260040607      *
061270040607      * Se già stampato qualcos'altro:
061280040607      *   forzo il salto alla pagina successiva
061290080926       // (SE NON si sta stampando SOLO una packing-list, nel qual caso
061300080926       //  il posizionamento nella pagina A4 verrà eseguito dagli
061310080926       //  indicatori 45 e 46)
061320080926if  1c                   if           (CurLineA4 > $MaxLine1
061330080926     c                             or  §LO2      > $LimOF1)
061340080926     c                             and NOT *in45
061350040607     c                   eval      CurLineA4 = wMaxLine
061360040607     c                   exsr      Test_OF
061370040607e   1c                   endif
061380040607      *
061390040607     c                   eval      $DDT_p = *on
061400040607      *
061410130315      /free
061420130315         // -?Posizionamento?
061430130315         clear  k06fiar801;
061440130315         k_AR8aas = DB0aas;
061450130315         k_AR8lnp = DB0lnp;
061460130315         k_AR8nrs = DB0nrs;
061470130315         k_AR8nsp = DB0nsp;
061480130315         k_AR8tmd = c_§PDF + '999';
061490130315         setgt  %kds( k06fiar801 : 5 )  FIAR8000;
061500130315      /end-free
061510130315      *
061520130315      * - Letura
061530040607do  1c                   dou          %eof(FIAR801L)
061540040607     c                             or AR8atb = *blanks
061550130308     c                             or AR8tmd <> c_§PDF
061560040607     c     KLDV          reade     FIAR8000
061570040607e   1c                   enddo
061580040607if  1c                   if        %eof(FIAR801L)
061590040607     c                   goto      NOprtDDT
061600040607e   1c                   endif
061610041207if  1c                   if        §AR8des   = *blanks
061620041207     c*** (già fatto:)   reset                   dAR8
061630041207     c                   reset                   TIBS02ds
061640041207     c                   eval      T02cod    = 'AR8'
061650041207     c                   movel(p)  AR8tmd        T02ke1
061660041207     c                   call      'TIBS02R'
061670041207     c                   parm                    KPJBA
061680041207     c                   parm                    TIBS02ds
061690041207if  2c                   if        T02err    = *blanks
061700041207     c                   movel     T02uni        dAR8
061710041207e   2c                   endif
061720041207e   1c                   endif
061730040607      *
061740040607sel 1c                   select
061750050217      * - Karnak o Titanedi o EDk (identiche)
061760080730w   1c                   when      §AR8tsd   = 'K1'
061770040607     c                   exsr      DDT_Karnak
061780050414      * - Logistica o Sait-Abrasivi
061790060627w   1c                   when      %subst(§AR8tsd:1:1) = 'L'
061800050218     c                   exsr      DDT_Logistica
061810050217      * - Arcte
061820050215w   1c                   when      AR8tmd    = 'ARCT'
061830050217     c                   exsr      DDT_Arcte
061840050214      * - Esprinet
061850060627w   1c                   when      §AR8tsd   = 'E1'
061860050214     c                   exsr      DDT_Esprinet
061870041207      * - dati generici (con logo)
061880041207w   1c                   when      §AR8tsd   = 'S2'
061890041207     c                   exsr      DDT_genericoL
061900041207      * - dati generici (senza logo)
061910041207w   1c***                when      §AR8tsd   = 'S1'
061920040607     c                   other
061930040607     c                   exsr      DDT_generico
061940040607e   1c                   endsl
061950040607      *
061960060921     c                   reset                   $FIAR8
061970040607     c                   reset                   $DDT_o
061980040607     c                   reset                   $DDT_p
061990040608     c                   reset                   $DDTlastPg
062000040608     c                   clear                   wOmaRowT
062010040608     c                   clear                   wOmaRowD
062020040607     c                   clear                   $AR8o1
062030040607      *
062040040607     c     NOprtDDT      ENDSR
062050040607      *
062060040607      *---------------------------------------------------------------*
062070050217      *?Stampa DDT del mittente KARNAK / TITANEDI                    ?*
062080040607      *---------------------------------------------------------------*
062090040607     c     DDT_Karnak    BEGSR
062100040607      *
062110080923      /free
062120080923         // Posizionamento iniziale all'interno del foglio A4
062130080923         write  A4prtSP1;
062140080923
062150080923         // Impostazione del nuovo limite x overflow
062160080923         select;
062170080923           when  NOT *in45;
062180080923             wMaxLine = $MaxLine3;
062190080923           when  *in45   and   NOT *in46;
062200080923             wMaxLine = $MaxLine3_pl1;
062210080923           when  *in45   and   *in46;
062220080923             wMaxLine = $MaxLine3_pl2;
062230080923         endsl;
062240080923      /end-free
062250040607      *
062260040607      * Impostazione testi fissi - - - - - - - - - - - - - - - - - - -*
062270040607      *
062280040607     c                   eval      PCtx1 = 'ATTENZIONE: VERIFICARE LA -
062290040607     c                             QUANTITA'' E L''INTEGRITA'' DEI CAR-
062300040607     c                             TONI. IN CASO DI DISCORDANZA RITIRA-
062310040607     c                             RE CON RISERVA.'
062320130118     c*//                eval      PCtx2 = 'La merce viagga a rischio -
062330130118     c*//                          e pericolo del committente anche se-
062340130118     c*//                           venduta franco destino. Trascorsi -
062350130118     c*//                          otto giorni dal ricevimento della m-
062360130118     c*//                          erce non si accettano reclami.   - -
062370130118     c*//                            IN CASO DI RITARDATO PAGAMENTO DE-
062380130118     c*//                          CORRERANNO GLI INTERESSI BANCARI IN-
062390130118     c*//                           VIGORE.'
062400130118     c*//                eval      PCtx3 = 'Per ogni vertenza l''acqui-
062410130118     c*//                          rente accetta la competenza della R-
062420130118     c*//                          epubblica di San Marino o in altern-
062430130118     c*//                          ativa del foro di Rimini.'
062440130118     c*//                eval      PCtx4 = 'NON SI ACCETTANO RESI E NO-
062450130118     c*//                          N SI EFFETTUANO SOSTITUZIONI SE NON-
062460130118     c*//                           AUTORIZZATI PER ISCRITTO DALLA NOS-
062470130118     c*//                          TRA DIREZIONE COMMERCIALE.'
062480130118     c*//                eval      PCtx5 = 'Quand''anche autorizzati, -
062490130118     c*//                          i resi verranno accettati e le sost-
062500130118     c*//                          ituzioni verranno effettuate solo d-
062510130118     c*//                          opo aver accertato il perfetto stat-
062520130118     c*//                          o nel nostro stabilimento della mer-
062530130118     c*//                          ce restituita.'
062540130118     c                   eval      PCtx2 = 'Eventuali reclami per mate-
062550130118     c                             riale mancante o danneggiato all''i-
062560130118     c                             nterno dei colli dovranno pervenire-
062570130118     c                              entro 8 (otto) giorni di calendari-
062580130118     c                             o dall''avvenuta consegna a MyO S.r-
062590130118     c                             .l. - Ufficio Resi - Via Santarcang-
062600130118     c                             iolese, 6 - 47825 Torriana (RN)'
062610130118     c                   eval      PCtx4 = 'Eventuali mancanze di coll-
062620130118     c                             i e/o avarie sono da contestare all-
062630130118     c                             ''atto della consegna con annotazio-
062640130118     c                             ne su lettera di vettura e DDT'
062650040709if  1c                   if        AR8tmd = 'TITA'
062660040709     c                   eval      PCtx6 = 'Depositario: K & K Logisti-
062670040709     c                             cs S.p.a. - via Santarcangiolese, 6-
062680040709     c                              - 47825 Torriana (RN) - P.I. 00953-
062690040709     c                             860400'
062700040709e   1c                   endif
062710080822      *
062720040607      * Conteggio delle righe "omaggio" - - - - - - - - - - - - - - -*
062730040607      *
062740040614     c                   clear                   xx
062750040607     c                   eval      SAVprr = AR8prr
062760040607     c     KLDV          setgt     FIAR8000
062770040607     c     KLDV          readpe    FIAR8000
062780040607do  1c                   dow       NOT %eof(FIAR801L)
062790040607sel 2c                   select
062800040607w   2c                   when      AR8atb <> *blanks
062810040611w   2c                   when      AR8trk =  'O2'
062820040608     c                   add       1             wOmaRowD
062830040611w   2c                   when      AR8trk =  'O1'
062840040608     c                   add       1             wOmaRowT
062850051110     c                   movel(p)  AR8uni        dAR8O1
062860040607     c                   add       1             xx
062870040607     c                   eval      $AR8o1(xx) =  §AR8O1a
062880040607x   2c                   other
062890040607     c                   leave
062900040607e   2c                   endsl
062910040607     c     KLDV          readpe    FIAR8000
062920040607e   1c                   enddo
062930040607      * - mi riposiziono
062940040607     c                   eval      AR8prr = SAVprr
062950040607     c     K06AR801      setll     FIAR8000
062960040607     c     K06AR801      reade     FIAR8000
062970040607      *
062980040607      * Reperimento dati, impostazione e stampa DDT mittente  - - - -*
062990040607      *
063000040607if  1c                   dow       NOT %eof(FIAR801L)
063010040607      *
063020040607sel 2c                   select
063030040607      * - Record annullato (non si stampa nulla)
063040040607w   2c                   when      AR8atb <> *blanks
063050040607      * - Dati di dettaglio: articolo, descrizione, quantità
063060040607w   2c                   when      AR8trk = 'C1'
063070050217     c                   exsr      PrtDdtKARN_C1
063080040607      * - Dati di dettaglio: testo per articoli non disponibili
063090040607w   2c                   when      AR8trk = 'C2'
063100050217     c                   exsr      PrtDdtKARN_C2
063110040607      * - Dati di dettaglio: descrizioni generiche (orario apertura)
063120040607w   2c                   when      AR8trk = 'C3'
063130050217     c                   exsr      PrtDdtKARN_C3
063140040607      * - Dati di testata: destinatario, cliente di fatturazione
063150040607w   2c                   when      AR8trk = 'T1'
063160050217     c                   exsr      PrtDdtKARN_T1
063170040607      * - Dati di testata: agente, numero ordine, estremi documento
063180040607w   2c                   when      AR8trk = 'T2'
063190050217     c                   exsr      PrtDdtKARN_T2
063200040607      * - Dati di fondo: mezzo e causale trasporto, colli, peso
063210040607w   2c                   when      AR8trk = 'V1'
063220050217     c                   exsr      PrtDdtKARN_V1
063230040607      * - Dati di fondo: vettore, annotazioni e note
063240040607w   2c                   when      AR8trk = 'V2'
063250050217     c                   exsr      PrtDdtKARN_V2
063260040607      * - Dati esterni: riga descrittiva con referente
063270040607w   2c                   when      AR8trk = 'L1'
063280050217     c                   exsr      PrtDdtKARN_L1
063290040607      * - Dati di omaggio: testo per articoli omaggiati
063300040607w   2c                   when      AR8trk = 'O1'
063310050217     c                   exsr      PrtDdtKARN_O1
063320040607      * - Dati di omaggio: articolo, descrizione, quantità
063330040607w   2c                   when      AR8trk = 'O2'
063340050217     c                   exsr      PrtDdtKARN_O2
063350040607e   2c                   endsl
063360040607      *
063370040607     c     KLDV          reade     FIAR8000
063380040607      *
063390040607e   1c                   enddo
063400040607      *
063410040607      * Reimpostazione del vecchio limite x overflow
063420040607     c                   eval      wMaxLine = $MaxLine2
063430040607      *
063440040607     c                   ENDSR
063450050214      *
063460040607      *---------------------------------------------------------------*
063470050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
063480050217      *?Dati di testata: destinatario, cliente di fatturazione       ?*
063490040607      *---------------------------------------------------------------*
063500050217     c     PrtDdtKARN_T1 BEGSR
063510040607      *
063520040607      * > logo
063530040607     c                   eval      §Logo  = 'LOGO' + AR8tmd
063540040607      *
063550051110     c                   movel(p)  AR8uni        dAR8T1
063560040607      *
063570040607      * > destinatario
063580040607     c                   eval      PCrsd  = §AR8t1a
063590040607     c                   eval      PCind  = §AR8t1b
063600040616if  1c                   if        §AR8t1c  > *zeros
063610040607     c                   eval      PCcad  = §AR8t1c
063620040616e   1c                   endif
063630040607     c                   eval      PClod  = §AR8t1d
063640040607     c                   eval      PCprd  = §AR8t1e
063650040607     c***                eval      PCnzd  = *blanks
063660040607      * > cliente di fatturazione
063670040607     c                   eval      PCrsf  = §AR8t1f
063680040607     c                   eval      PCinf  = §AR8t1g
063690040616if  1c                   if        §AR8t1h  > *zeros
063700040607     c                   eval      PCcaf  = §AR8t1h
063710040616e   1c                   endif
063720040607     c                   eval      PClof  = §AR8t1i
063730040607     c                   eval      PCprf  = §AR8t1j
063740040607     c***                eval      PCnzf  = *blanks
063750040607      *
063760040607     c                   ENDSR
063770040607      *
063780040607      *---------------------------------------------------------------*
063790050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
063800050217      *?Dati di testata: agente, ..., porto, ...                     ?*
063810040607      *---------------------------------------------------------------*
063820050217     c     PrtDdtKARN_T2 BEGSR
063830040607      *
063840051110     c                   movel(p)  AR8uni        dAR8T2
063850040607      *
063860040607      * > dati in testata DDT
063870040607      * - - codice agente
063880130122     c*//                eval      PCage  = §AR8t2a
063890040607      * - - numero e data ordine
063900040607     c                   eval      PCnro  = §AR8t2b
063910040607     c                   eval      PCdto  = §AR8t2c
063920040607      * - - riferimenti
063930040607     c                   eval      PCrif  = §AR8t2d
063940040607      * - - codice destinatario (dal mittente)
063950040611     c                   eval      PCcli  = §AR8t2e
063960040607      * - - numero e data documento (DDT)
063970040607     c                   eval      PCnrd  = §AR8t2f
063980040607     c                   eval      PCdtd  = §AR8t2g
063990040607      * - - numero pagina documento originale (qui ricalcolato)
064000040607     c***                eval      Page   = §AR8t2h
064010040607      * - - porto
064020040607     c                   eval      PCpto  = §AR8t2i
064030040607      * - - condizioni di pagamento
064040040611     c                   eval      PCpag  = §AR8t2j
064050040607     c                                    + ' '
064060040607     c                                    + %trim(§AR8t2k)
064070040607      * - - tipologia spedizione
064080040607     c                   eval      PCtsp  = §AR8t2l
064090040607      *
064100040607      * > Stampa testata DDT - - - - - - - - - - - - - - - - - - - - -*
064110040607      *
064120040607      *   - OverFlow impossibile qui! -
064130040607     c                   write     A4prtDK1
064140040607      *
064150040607     c                   ENDSR
064160040607      *
064170040607      *---------------------------------------------------------------*
064180050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
064190050217      *?Dati di dettaglio: articolo, descrizione, quantità           ?*
064200040607      *---------------------------------------------------------------*
064210050217     c     PrtDdtKARN_C1 BEGSR
064220040607      *
064230040607     c                   clear                   A4prtDK2
064240051110     c                   movel(p)  AR8uni        dAR8C1
064250040607      *
064260040607      * > dati in dettaglio DDT
064270040607      * - - codice prodotto
064280040607     c                   eval      PCcpr  = §AR8c1a
064290040607      * - - descrizione prodotto
064300040607     c                   eval      PCdpr  = §AR8c1b
064310040607      * - - quantità
064320040607     c                   eval      PCqta  = '     ' + §AR8c1c
064330040607      * - - note
064340040607     c***                eval      PCnot  = *blanks
064350040607      *
064360040607      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
064370040607      *
064380040607     c                   exsr      Test_OF
064390040607     c                   write     A4prtDK2
064400040607      *
064410040607     c                   ENDSR
064420040607      *
064430040607      *---------------------------------------------------------------*
064440050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
064450050217      *?Dati di dettaglio: testo per articoli non disponibili        ?*
064460040607      *---------------------------------------------------------------*
064470050217     c     PrtDdtKARN_C2 BEGSR
064480040607      *
064490040607     c                   clear                   A4prtDK3
064500051110     c                   movel(p)  AR8uni        dAR8C2
064510040607      *
064520040607      * > dati in note DDT
064530040607      * - - riga descrittiva
064540040607     c                   eval      PCtxt  = §AR8c2a
064550040607      *
064560040607      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
064570040607      *
064580040607     c                   exsr      Test_OF
064590040607     c                   write     A4prtDK3
064600040607      *
064610040607     c                   ENDSR
064620040607      *
064630040607      *---------------------------------------------------------------*
064640050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
064650050217      *?Dati di dettaglio: descrizioni generiche (orario apertura)   ?*
064660040607      *---------------------------------------------------------------*
064670050217     c     PrtDdtKARN_C3 BEGSR
064680040607      *
064690040607     c                   clear                   A4prtDK2
064700051110     c                   movel(p)  AR8uni        dAR8C3
064710040607      *
064720040607      * > dati in note DDT
064730040607      * - - riga descrittiva
064740040607     c                   eval      PCdpr  = §AR8c3a
064750040607      *
064760040607      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
064770040607      *
064780040607     c                   exsr      Test_OF
064790040607     c                   write     A4prtDK2
064800040607      *
064810040607     c                   ENDSR
064820040607      *
064830040607      *---------------------------------------------------------------*
064840050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
064850050217      *?Dati di fondo: mezzo e causale trasporto, colli, peso        ?*
064860040607      *---------------------------------------------------------------*
064870050217     c     PrtDdtKARN_V1 BEGSR
064880040607      *
064890051110     c                   movel(p)  AR8uni        dAR8V1
064900040607      *
064910040607      * Controllo se ci saranno da stampare tante righe omaggio da
064920040607      *   chiudere in pagina successiva:
064930040608      *   i campi del box finale del DDT (in totale 9 righe) verranno
064940040607      *   riempiti solo nella ultima pagina.
064950040607      *
064960080924      /free
064970080924         select;
064980080924           when  NOT *in45;
064990080924      /end-free
065000040608     c                   eval      $DDTlastPg = ($MaxLine2
065010040713     c                                        +  1
065020040608     c                                        -  CurLineA4
065030040608     c                                        -  $BoxRow1
065040040608     c                                        -  wOmaRowT
065050040608     c                                        >= wOmaRowD)
065060080924      /free
065070080924           when  *in45   and   NOT *in46;
065080110406             $DDTlastPg = ($MaxLine2_pl1k
065090080924                           + 1 - CurLineA4 - $BoxRow1 - wOmaRowT
065100080924                           >= wOmaRowD);
065110080924           when  *in45   and   *in46;
065120110406             $DDTlastPg = ($MaxLine2_pl2k
065130080924                           + 1 - CurLineA4 - $BoxRow1 - wOmaRowT
065140080924                           >= wOmaRowD);
065150080924         endsl;
065160080924      /end-free
065170040607      *
065180040607      * > dati in chiusura DDT
065190040607      *   Per ora imposto (ho già impostato) solo la DS "dAR8V1".
065200040607      *   I corrispondenti campi di stampa li impostero nella subr.
065210050217      *     "PrtDdtKARN_V2".
065220040607      *
065230040607     c                   ENDSR
065240040607      *
065250040607      *---------------------------------------------------------------*
065260050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
065270050217      *?Dati di fondo: vettore, annotazioni e note                   ?*
065280040607      *---------------------------------------------------------------*
065290050217     c     PrtDdtKARN_V2 BEGSR
065300040607      *
065310051110     c                   movel(p)  AR8uni        dAR8V2
065320040618      *
065330040618      * > dati in chiusura DDT
065340040618      *
065350040607      * Ho già controllato se ci saranno da stampare tante righe
065360040607      *   omaggio da chiudere in pagina successiva
065370040608if  1c                   if        $DDTlastPg = *off
065380040607     c                   eval      PCnv2  = '          '
065390040607     c                                    + '*** SEGUE ***'
065400040618x   1c                   else
065410050217     c                   exsr      PrtDdtKARNaV2
065420040607e   1c                   endif
065430040607      *
065440040607      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
065450040608      *
065460050217     c                   exsr      PrtDdtKARNbV2
065470040607      *
065480040607      * > Impostazione nuovo limite di OverFlow  - - - - - - - - - - -*
065490080923      /free
065500080923         select;
065510080923           when  NOT *in45;
065520080923             wMaxLine = $MaxLine2;
065530080923           when  *in45   and   NOT *in46;
065540080923             wMaxLine = $MaxLine2_pl1;
065550080923           when  *in45   and   *in46;
065560080923             wMaxLine = $MaxLine2_pl2;
065570080923         endsl;
065580080923      /end-free
065590040607      *
065600040608      * N.B.: la copia per il destinatario della LDV deve comparire
065610040608      *       nell'ultima pagina del DDT per il cliente (vedi subr.
065620040608      *       "ChiusuraLDV")
065630040607      *
065640040607     c                   ENDSR
065650040618      *
065660040618      *---------------------------------------------------------------*
065670040618      *
065680050217     c     PrtDdtKARNaV2 BEGSR
065690040618      *
065700040618      * > dati in chiusura DDT
065710040618      *
065720040618      * > > da dAR8V1
065730040618      * - - mezzo di trasporto
065740040618     c                   movel     §AR8v1a       PCmtr
065750040618      * - - causale trasporto
065760040618     c                   eval      PCctr  = §AR8v1b
065770040618      * - - numero colli
065780040618     c                   eval      PCncl  = §AR8v1c
065790040618      * - - peso in kg
065800040618     c                   eval      PCpkg  = §AR8v1d
065810040618      * - - aspetto esteriore dei beni
065820040618     c                   eval      PCaeb  = §AR8v1e
065830040618      *
065840040618      * > > da dAR8V2
065850120530      * - - dati vettore: · ragione sociale (BRT S.p.A.)
065860040618      *                   · indirizzo
065870040618      *                   · c.a.p./città/provincia
065880040618      *     => NON con i dati che manda il mittente !!!
065890040618      *        ...Vuoi mai che si sbagli ad inviarci della roba !!!
065900040618     c***                eval      PCvet  = %trim(§AR8v2a)
065910040618     c***                                 + ' - '
065920040618     c***                                 + %trim(§AR8v2b)
065930040618     c***                                 + ' - '
065940040618     c***                                 + %trim(§AR8v2c)
065950120530     c*//                if        (DB0dft > *zero       and
065960120530     c*//                           DB0dft > §vpoBRTdta)  OR
065970120530     c*//                          (DB0dft = *zero       and
065980120530     c*//                           Wdsp   > §vpoBRTdta)
065990110426     c                   eval      PCvet  = $BRT
066000110426     c                                    + ' - '
066010110426     c                                    + $Intest2
066020110426     c                                    + ' (BO)'
066030120530     c*//                else
066040120530     c*//                eval      PCvet  = $Bartolini
066050120530     c*//                                 + ' - '
066060120530     c*//                                 + $Intest2
066070120530     c*//                                 + ' (BO)'
066080120530     c*//                endif
066090040618      * - - data ritiro
066100040618     c*** non stampo:    eval      PCdtv  = §AR8v2d
066110040618      * - - orario ritiro
066120040618     c*** non stampo:    eval      PChmv  = §AR8v2e
066130040618      * - - annotazioni / variazioni
066140040618     c                   eval      PCvea  = §AR8v2f
066150040618      * - - note
066160040618     c                   eval      PCnv1  = §AR8v2g
066170040618     c                   eval      PCnv2  = §AR8v2h
066180040618      *
066190040618     c                   ENDSR
066200040618      *
066210040618      *---------------------------------------------------------------*
066220040618      *
066230050217     c     PrtDdtKARNbV2 BEGSR
066240040618      *
066250040618      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
066260040618      *
066270040618      * - - Calcolo la posizione finale del box
066280040618     c                   exsr      Posiz_DK
066290040618      *
066300040618      * - - Stampa parte finale DDT
066310040618     c                   write     A4prtDK4
066320040618      *
066330040618      * > Stampa logo, box, linee e testi fissi  - - - - - - - - - - -*
066340081017     c                   if            §AR8cxd = *blanks
066350081017     c                             or  DB0fa4  = 'P'
066360050331     c                   write     A4prtDT0
066370050331     c                   endif
066380040618     c                   write     A4prtDK0
066390040618      *
066400040618     c                   ENDSR
066410040607      *
066420040607      *---------------------------------------------------------------*
066430050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
066440050217      *?Dati esterni: riga descrittiva con referente                 ?*
066450040607      *---------------------------------------------------------------*
066460050217     c     PrtDdtKARN_L1 BEGSR
066470040607      *
066480051110     c                   movel(p)  AR8uni        dAR8L1
066490040607      *
066500040607      * > righe descrittive esterne al DDT
066510040607      * - - riga descrittiva
066520040607     c                   eval      PCtxx  = §AR8l1a
066530040607      *
066540040607      * > Stampa riga testo finale DDT - - - - - - - - - - - - - - - -*
066550040618      *   - OverFlow già testato per questa riga -
066560040607     c                   write     A4prtDK5
066570040607      *
066580040607     c                   ENDSR
066590040607      *
066600040607      *---------------------------------------------------------------*
066610050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
066620050217      *?Dati di omaggio: testo per articoli omaggiati                ?*
066630040607      *---------------------------------------------------------------*
066640050217     c     PrtDdtKARN_O1 BEGSR
066650040608      *
066660040608     c                   eval      $DDT_o = *on
066670040607      *
066680040607     c                   clear                   A4prtDK5
066690051110     c                   movel(p)  AR8uni        dAR8O1
066700040607      *
066710040607      * > righe descrittive esterne al DDT
066720040607      * - - riga descrittiva
066730040607     c                   eval      PCtxx  = §AR8o1a
066740040607      * - - nessuna sottolineatura
066750040608     c***                eval      *in39  = *off
066760040607      *
066770040607      * > Stampa testata omaggi in DDT - - - - - - - - - - - - - - - -*
066780040608      *   (se NON già stampata nella nuova pagina x OverFlow)
066790040608     c                   eval      SavLineA4 = CurLineA4
066800040607     c                   exsr      Test_OF
066810040608if  1c                   if        CurLineA4 = SavLineA4
066820040607     c                   write     A4prtDK5
066830040608e   1c                   endif
066840040608      * ...Dovendo ristampare le righe di intestazione degli omaggi
066850040608      *   in ogni pagina (da quella del 1° omaggio in poi), non calcolo
066860040608      *   quelle già stampate...
066870040608     c*** NO:            sub       1             wOmaRowT
066880040607      *
066890040607     c                   ENDSR
066900040607      *
066910040607      *---------------------------------------------------------------*
066920050217      *?Impostazione campi nel DDT del mittente KARNAK / TITANEDI    ?*
066930050217      *?Dati di omaggio: articolo, descrizione, quantità             ?*
066940040607      *---------------------------------------------------------------*
066950050217     c     PrtDdtKARN_O2 BEGSR
066960040607      *
066970040607     c                   clear                   A4prtDK2
066980051110     c                   movel(p)  AR8uni        dAR8O2
066990040607      *
067000040607      * > dettaglio omaggi DDT
067010040607      * - - codice prodotto
067020040607     c                   eval      PCcpr  = §AR8o2a
067030040607      * - - descrizione prodotto
067040040607     c                   eval      PCdpr  = §AR8o2b
067050040607      * - - quantità
067060040618     c                   eval      PCqta  = '     ' + §AR8o2c
067070040607      * - - note
067080040607     c***                eval      PCnot  = *blanks
067090040607      *
067100040607      * > Stampa dettaglio omaggi in DDT - - - - - - - - - - - - - - -*
067110040607      *
067120040607     c                   exsr      Test_OF
067130040607     c                   write     A4prtDK2
067140040608     c                   sub       1             wOmaRowD
067150040607      *
067160040607     c                   ENDSR
067170040608      *
067180040608      *---------------------------------------------------------------*
067190081002      *?Impostazione nuova pagina di DDT Karnak / Titanedi / EDk     ?*
067200040608      *---------------------------------------------------------------*
067210081002     c     NewPageDK     BEGSR
067220040608      *
067230080923      /free
067240080923         // Impostazione del nuovo limite x overflow
067250080923         select;
067260080923           when  NOT *in45;
067270080923             wMaxLine = $MaxLine3;
067280080923           when  *in45   and   NOT *in46;
067290080923             wMaxLine = $MaxLine3_pl1;
067300080923           when  *in45   and   *in46;
067310080923             wMaxLine = $MaxLine3_pl2;
067320080923         endsl;
067330080923      /end-free
067340080923     c                   write     A4prtSP1
067350040608     c                   write     A4prtDK1
067360040608      *
067370040608if  1c                   if        $DDT_o = *on
067380040608      *
067390040608      * - Stampa 1 riga di dettaglio vuota
067400040608     c                   clear                   A4prtDK3
067410040608     c                   write     A4prtDK3
067420040618      * - Reimposta i dati nel box finale del DDT
067430050217     c                   exsr      PrtDdtKARNaV2
067440040618      * - Stampa fondo DDT
067450050217     c                   exsr      PrtDdtKARNbV2
067460040608      * - Stampa ultima riga
067470040608      *   ("Per qualsiasi problema o chiarimento contattare...)
067480040608     c                   clear                   A4prtDK5
067490040608     c                   eval      PCtxx  = §AR8l1a
067500040608     c                   write     A4prtDK5
067510040608      * - Stampa intestazione per merce omaggio
067520040608      *   xx = numero di righe per "intestazione omaggi"
067530040608     c                   eval      wx     = xx
067540040608do  2c                   dow           xx         >  *zeros
067550040608     c                             and xx         <= %elem($AR8o1)
067560040608     c                             and $AR8o1(xx) <> *blanks
067570040608     c                   eval      PCtxx  = $AR8o1(xx)
067580040608     c                   write     A4prtDK5
067590040608     c                   eval      xx     = xx - 1
067600040608e   2c                   enddo
067610040608     c                   eval      xx     = wx
067620040608      * -Se sta leggendo il tipo record "O1":?
067630040608      *   sposta il ciclo di lettura del file FIAR8 sul record
067640040608      *   successivo all'ultimo (appena stampato) dell'intestazione
067650040608      *   della merce omaggiata
067660040608if  2c                   if        AR8trk =  'O1'
067670040608if  3c                   dou       AR8trk <> 'O1'
067680040608     c     KLDV          reade     FIAR8000
067690040608e   3c                   enddo
067700040608if  3c*** è così:        if        AR8trk = 'O2'
067710040608     c     KLDV          readpe    FIAR8000
067720040707e   3c***                endif
067730040608e   2c                   endif
067740040608      * - Aggiorna controllo se "ultima pagina"
067750080924      /free
067760080924           select;
067770080924             when  NOT *in45;
067780080924      /end-free
067790040608     c                   eval      $DDTlastPg = ($MaxLine2
067800040713     c                                        +  1
067810040608     c                                        -  CurLineA4
067820040608     c                                        -  $BoxRow1
067830040608     c                                        -  wOmaRowT
067840040608     c                                        >= wOmaRowD)
067850080924      /free
067860080924             when  *in45   and   NOT *in46;
067870110406               $DDTlastPg = ($MaxLine2_pl1k
067880080924                             + 1 - CurLineA4 - $BoxRow1 - wOmaRowT
067890080924                             >= wOmaRowD);
067900080924             when  *in45   and   *in46;
067910110406               $DDTlastPg = ($MaxLine2_pl2k
067920080924                             + 1 - CurLineA4 - $BoxRow1 - wOmaRowT
067930080924                             >= wOmaRowD);
067940080924           endsl;
067950080924      /end-free
067960040608      *
067970040608e   1c                   endif
067980040608      *
067990040608     c                   ENDSR
068000040607      *
068010040607      *---------------------------------------------------------------*
068020050414      *?Posizionamento di linee e box nel DDT KARNAK / TITANEDI / EDk?*
068030040607      *---------------------------------------------------------------*
068040040607     c     Posiz_DK      BEGSR
068050040607      *
068060080918      /free
068070080923         // Impostazione posizioni di logo/box
068080080923         // (posizioni di "partenza" diverse a seconda del posizionam.
068090080923         //  iniziale nel foglio A4 attualmente in stampa)
068100080923         select;
068110080923           when  NOT *in45;
068120081002             §PB1 =  3.120;
068130080923           when  *in45   and   NOT *in46;
068140081002             §PB1 = 0.250;
068150080923           when  *in45   and   *in46;
068160081002             §PB1 = 6.000;
068170080923         endsl;
068180080918      /end-free
068190040607     c                   eval      §PB2 =  §PB1 + 1.23
068200080822     c                   eval      §PB3 =  §PB2 + 0.18
068210040607     c                   eval(h)   §PB4 =  CurLineA4 / 8 + 0.20
068220040607     c                   eval      §PB5 =  §PB4 + 0.18
068230040607     c                   eval      §PB6 =  §PB5 + 0.77
068240040607     c                   eval      §PB7 =  §PB5 + 0.165
068250040607     c                   eval      §PB8 =  §PB7 + 0.08
068260040607      * Impostazione posizioni di linee orizzontali nel 1° box
068270040607     c                   eval(h)   §LO1 =  §PB1 + (§PB2 - §PB1) / 2
068280040607      * Impostazione posizioni di linee orizzontali nel 2° box
068290080822     c                   eval      §LO2 =  §PB3 + 0.23
068300080822     c                   eval      §LO3 =  §LO2 + 0.145
068310080822     c                   eval      §LO4 =  §LO3 + 0.10
068320040607     c                   eval      §LO5 =  §PB4 - 0.15
068330040607      * Impostazione posizioni di linee orizzontali nel 3° box
068340040607     c                   eval      §LO6 =  §PB5 + 0.14
068350040607     c                   eval      §LO7 =  §LO6 + 0.12
068360040607     c                   eval      §LO8 =  §LO7 + 0.12
068370040607     c                   eval      §LO9 =  §LO8 + 0.13
068380040607      * Impostazione posizioni di linee verticali nel 2° box
068390040607     c                   eval      §LV1 =  §LO2 - §PB3
068400040607     c                   eval      §LV2 =  §LO3 - §LO2
068410040607     c                   eval      §LV3 =  §LO5 - §LO3
068420040607      * Impostazione posizioni di linee verticali nel 3° box
068430040607     c                   eval      §LV4 =  §LO6 - §PB5
068440040607     c                   eval      §LV5 =  §LO7 - §LO6
068450040607     c                   eval      §LV6 =  §LO8 - §LO7
068460040607     c                   eval      §LV7 =  §PB6 - §LO9
068470040607      * Impostazione posizioni di campi
068480040607     c                   eval      §PS1 =  §LO5 + 0.11
068490040607     c                   eval      §PS2 =  §PB4 + 0.08
068500040607     c                   eval      §PS3 =  §PS2 + 0.07
068510130118if  1c*//                if        §PB6 <  §PB1 + 4.58
068520130118     c*//                eval      §PR1 =  §PB1 + 4.58
068530130118if  1c                   if        §PB6 <  §PB1 + 3.30
068540130118     c                   eval      §PR1 =  §PB1 + 3.30
068550040607x   1c                   else
068560040607     c                   eval      §PR1 =  §PB6
068570040607e   1c                   endif
068580040607      *
068590040607     c                   ENDSR
068600041019      *
068610041019      *---------------------------------------------------------------*
068620050414      *?Stampa DDT del mittente LOGISTICA / SAIT ABRASIVI            ?*
068630041019      *---------------------------------------------------------------*
068640050218     c     DDT_Logistica BEGSR
068650041019      *
068660080924      /free
068670080924         // Impostazione del nuovo limite x overflow
068680080924         // (Codesto limite è valido solo per la stampa del dettaglio.
068690080924         //   Per la stampa del "fondo" bolla occorre REimpostarlo ad
068700080924         //   un valore più basso.
068710080924         //  Ovvero: stampo più righe di dettaglio che posso; se alla
068720080924         //  fine delle righe di dettaglio non rimane spazio a suffi-
068730080924         //  cienza per il fondo: prima della stampa del fondo si salta
068740080924         //  alla pagina successiva, dove non si stampa alcuna riga di
068750080924         //  dettaglio.)
068760080924         select;
068770080924           when  NOT *in45;
068780090323             wMaxLine = $MaxLine2;
068790080924           when  *in45   and   NOT *in46;
068800090323             wMaxLine = $MaxLine2_pl1;
068810080924           when  *in45   and   *in46;
068820090323             wMaxLine = $MaxLine2_pl2;
068830080924         endsl;
068840080924
068850080924         // Posizionamento iniziale all'interno del foglio A4
068860080924         write  A4prtSP1;
068870081006
068880081006         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
068890081006           write  A4prtNUL;
068900081006         endif;
068910080924      /end-free
068920041019      *
068930041019      * Impostazione testi fissi - - - - - - - - - - - - - - - - - - -*
068940041019      *
068950071008      * - prima solo per "Logistica", ora neanche più per lei...
068960071008      * - Sait Abrasivi SpA & Brumar Srl hanno Tipo Stampa DDT = "LC"
068970071008if  1c***                if        §AR8tsd <> 'LC'
068980071008     c***                eval      PCLtx1 = 'La merce viagga a rischio -
068990071008     c***                          e pericolo del Committente, pertanto -
069000071008     c***                          la Venditrice non risponde di -
069010071008     c***                          eventuali perdite o avarie derivanti -
069020071008     c***                          dal trasporto.'
069030071008     c***                eval      PCLtx2 = 'Il Destinatario, per -
069040071008     c***                          ottenere il risarcimento del danno, -
069050071008     c***                          deve far constatare al Corriere la -
069060071008     c***                          perdita o l''avaria all''atto del -
069070071008     c***                          ricevimento della merce.'
069080071008e   1c***                endif
069090050218      *
069100130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
069110130315      *  "StampaDDT")
069120050218     c***  KLDV          setll     FIAR8000       (già fatto)
069130050218     c***  KLDV          reade     FIAR8000       (già fatto)
069140041019      *
069150041019      * Reperimento dati, impostazione e stampa DDT mittente  - - - -*
069160041019      *
069170041019if  1c                   dow       NOT %eof(FIAR801L)
069180050318      *
069190050318if  2c                   if        AR8atb   = *blanks
069200050318     c                   eval      SAVtrk   = AR8trk
069210050318e   2c                   endif
069220041019      *
069230041019sel 2c                   select
069240041019      * - Record annullato (non si stampa nulla)
069250050218w   2c                   when      AR8atb  <> *blanks
069260041019      * - Dati di dettaglio: articolo, descrizione, quantità
069270050218w   2c                   when      AR8trk   = 'C1'
069280050217     c                   exsr      PrtDdtLOG1_C1
069290041103      * - Dati di dettaglio: descrizioni generiche
069300050218w   2c                   when      AR8trk   = 'C2'
069310050217     c                   exsr      PrtDdtLOG1_C2
069320041103      * - Dati di dettaglio: descrizioni generiche
069330050218w   2c                   when      AR8trk   = 'C3'
069340050217     c                   exsr      PrtDdtLOG1_C3
069350060614      * - Dati di dettaglio: corpo bolla non fincato
069360060614w   2c                   when      AR8trk   = 'C4'
069370060614     c                   exsr      PrtDdtLOG1_C4
069380090323      * - Dati di dettaglio: descrizioni generiche
069390090323w   2c                   when      AR8trk   = 'C5'
069400090323     c                   exsr      PrtDdtLOG1_C5
069410050318      * - Dati di packing-list: dettaglio
069420050318w   2c                   when      AR8trk   = 'Z1'
069430050318     c                   exsr      PrtDdtLOG1_Z1
069440041019      * - Dati di testata: sito e depositante
069450050218w   2c                   when      AR8trk   = 'T1'
069460050217     c                   exsr      PrtDdtLOG1_T1
069470041019      * - Dati di testata: destinatario, cliente di fatturazione
069480050218w   2c                   when      AR8trk   = 'T2'
069490050217     c                   exsr      PrtDdtLOG1_T2
069500041019      * - Dati di testata: agente, numero ordine, estremi documento
069510050218w   2c                   when      AR8trk   = 'T3'
069520050217     c                   exsr      PrtDdtLOG1_T3
069530041019      * - Dati di fondo: mezzo e causale trasporto, colli, peso
069540050218w   2c                   when      AR8trk   = 'P1'
069550050217     c                   exsr      PrtDdtLOG1_P1
069560050606      * - Dati di testata: FATTURA
069570050606w   2c                   when      AR8trk   = 'S1'
069580050606     c                   exsr      PrtDdtLOG1_S1
069590050606      * - Dati di dettaglio cpi 15: FATTURA
069600050606w   2c                   when      AR8trk   = 'S2'
069610050606     c                   exsr      PrtDdtLOG1_S2
069620050606      * - Dati di dettaglio cpi 20: FATTURA
069630050606w   2c                   when      AR8trk   = 'S3'
069640050606     c                   exsr      PrtDdtLOG1_S3
069650050606      * - Dati di fondo: FATTURA
069660050606w   2c                   when      AR8trk   = 'S4'
069670050606     c                   exsr      PrtDdtLOG1_S4
069680050606      * - Dati di fondo note: FATTURA
069690050606w   2c                   when      AR8trk   = 'S5'
069700050606     c                   exsr      PrtDdtLOG1_S5
069710041019e   2c                   endsl
069720041019      *
069730041019     c     KLDV          reade     FIAR8000
069740041019      *
069750041019e   1c                   enddo
069760050318      *
069770050318      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
069780050318      *
069790050505      * - se DDT con Packing-List:
069800050505      *   (altrimenti il box è già stato stampato in sr PrtDdtLOG1_P1)
069810050505if  1c                   if        %subst(SAVtrk:1:1) =  'Z'
069820050318      *
069830050318      * - - Calcolo la posizione finale del box e di linee separatrici
069840050318     c                   exsr      Posiz_DL
069850050318      *
069860050318      * > Stampa box, linee e testi fissi (no logo)  - - - - - - - - -*
069870081017     c                   if            §AR8cxd = *blanks
069880081017     c                             or  DB0fa4  = 'P'
069890050331     c                   write     A4prtDT0
069900050331     c                   endif
069910050318     c                   write     A4prtDL0C
069920050505      *
069930050505e   1c                   endif
069940050218      *
069950050218      * Reimpostazione del vecchio limite x overflow
069960080925      /free
069970080925         select;
069980080925           when  NOT *in45;
069990090323             wMaxLine = $MaxLine2;
070000080925           when  *in45   and   NOT *in46;
070010090323             wMaxLine = $MaxLine2_pl1;
070020080925           when  *in45   and   *in46;
070030090323             wMaxLine = $MaxLine2_pl2;
070040080925         endsl;
070050080925      /end-free
070060041019      *
070070041019     c                   ENDSR
070080081222
070090081222      /free
070100081222       //--------------------------------------------------------------
070110140303       //?Preparazione simil-logo Bartolini                            ?
070120081222       //--------------------------------------------------------------
070130090406       BEGSR  PrtDdtLOG1_SimilLogo;
070140081222
070150120530         //If  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
070160120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
070170120530         //
070180120530         //  // -?Preparazione simil-Logo BARTOLINI?
070190120530         //  §PLBLS  = §PBA + 0.150;
070200120530         //  §PLBLVS = 0.400;
070210120530         //  exsr  sr_SimilLogoBAR;
070220120530         //
070230120530         //  // -?Preparazione simil-Logo BRT?
070240120530         //  §P2blS  = §PlblS;
070250120530         //  //§P2blVS = §PlblVD + 0.050;
070260120530         //  §P2blVS = §PlblVD + 0.030;
070270120530         //  exsr  sr_SimilLogoBRT;
070280120530         //
070290120530         //Else;
070300120530         //
070310120530         //  // -?Preparazione simil-Logo BARTOLINI?
070320120530         //  §PLBLS  = §PBA + 0.050;
070330120530         //  §PLBLVS = 0.400;
070340120530         //  exsr  sr_SimilLogoBART;
070350120530         //
070360120530         //EndIf;
070370120530
070380120530         // -?Preparazione simil-Logo BRT?
070390120530         §PLBLS  = §PBA + 0.050;
070400120530         §PLBLVS = 0.400;
070410120530         exsr  sr_SimilLogoBRT_;
070420090406
070430120530         // -?Impostazione dati anagrafici BRT alla sua destra?
070440090422         §PC0    = §PBA    + 0.070 + 0.050;
070450090406         §PC1    = §PC0    + 0.100;
070460090406         §PC2    = §PC1    + 0.100;
070470090406         §PC3    = §PC2    + 0.090;
070480090406         §PC4    = §PC3    + 0.090;
070490090406         §PC5    = §PC4    + 0.090;
070500120530
070510120530         //§PLC    = 1.400;
070520120530         §PLC    = 1.000;
070530090406
070540120530         // -?"TBAR" è già correttamente impostato.?
070550120530         //TBAR    = $Bartolini;        (già così)
070560120530         //TBAR    = $BRT;              (già così)
070570090406         LBA1    = $Intest1;
070580090406         LBA2    = $Intest2_b;
070590120530         //LBA3    = $Intest3_b;
070600120530         LBA3    = $Intest3_b_brt;
070610090406         LBA4    = $Intest4_b;
070620090406         LBA5    = $Intest5_b;
070630081222
070640081222       ENDSR;
070650081222      /end-free
070660050214      *
070670041019      *---------------------------------------------------------------*
070680050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
070690050217      *?Dati di testata: sito e depositante                          ?*
070700041019      *---------------------------------------------------------------*
070710050217     c     PrtDdtLOG1_T1 BEGSR
070720041019      *
070730041019      * > logo
070740050218     c                   eval      §Logo    = 'LOGO' + AR8tmd
070750090107      /free
070760090107         // - Impostazione indicatore che condiziona la stampa del logo
070770120530         //   (o quella del "simil-logo" BRT)
070780090107         *in55 = (%subst(AR8tmd : 1 : 3) = 'LOG'          and
070790090107                  (§AR8tsd  = 'L1'  or  §AR8tsd  = 'L3'));
070800090107      /end-free
070810080908      *
070820060609      *? L O G O !! ?
070830060609      * per pulizia non vogliamo più avere tanti LOGx per ogni cliente che
070840060609      * vuole la stampa della fattura con il proprio logo e la bolla con
070850120530      * il logo BRT
070860060609      * anche Cappellini è d'accordo ad inviare i dati con il logo del
070870060609      * cliente da stampare nella LDV
070880060609      * quelli già esistenti avranno un nuovo logo un po' più piccolo per
070890071008      * problemi di spazio e i dati saranno inviati con AR8tmd = al nome del
070900071008      * nuovo logo e la tabella sarà fatta con §AR8tsd = 'L2' o 'L4'
070910060609      * i nuovi faremo in modo di avere o lo stesso logo oppure la stessa
070920060609      * logica descritta prima
070930060609      * ma può essere che un cliente voglia la fattura con il suo logo
070940120530      * mentre la bolla con il logo BRT quindi devo recuperare
070950060609      * il logo da stampare nella bolla non più da AR8TMD, che conterrà
070960060609      * il nome del logo da stampare nella fattura, ma dal campo del
070970120530      * del logo fattura, che conterrà il logo BRT, mentre il campo
070980071008      * §AR8tsd nella tabella sarà 'L1' o 'L3'
070990071008     c                   if        (§AR8tsd = 'L1' or §AR8tsd = 'L3') and
071000071008     c                             §AR8logof <> *blanks and
071010071008     c                             %subst(AR8tmd:1:3) <> 'LOG'
071020071008     c                   eval      §Logo    = 'LOGO' + §AR8logof
071030090107      /free
071040090107           // - Impostaz. indicatore che condiziona la stampa del logo
071050120530           //   (o quella del "simil-logo" BRT)
071060090107           *in55 = (%subst(§AR8logof : 1 : 3) = 'LOG');
071070090107      /end-free
071080060609     c                   endif
071090041019      *
071100080822     c                   movel(p)  AR8uni        dAR8LOG1T1
071110050414      *
071120060627if  1c                   if            §AR8tsd <> 'LC'
071130041019      *
071140041019      * > sito
071150060407     c                   select
071160071008     c                   when      §AR8tsd = 'L1' or
071170071008     c                             §AR8tsd = 'L3'
071180050707     c                   eval      PLOG1st0 = 'Deposito di:'
071190071008     c                   when      §AR8tsd = 'L2' or
071200071008     c                             §AR8tsd = 'L4'
071210060407     c                   eval      PLOG1st0 = 'Depositario:'
071220060407     c                   endsl
071230050707     c                   eval      PLOG1st1 = §A8LOG1t1a
071240050707     c                   eval      PLOG1st2 = §A8LOG1t1b
071250050707     c                   eval      PLOG1st3 = §A8LOG1t1c
071260050707     c                   eval      PLOG1st4 = §A8LOG1t1d
071270050707     c                   eval      PLOG1st5 = §A8LOG1t1e
071280041019      *
071290041019      * > depositante
071300060407     c                   select
071310071008     c                   when      §AR8tsd = 'L1' or
071320071008     c                             §AR8tsd = 'L3'
071330050707     c                   eval      PLOG1de0 = 'Depositario della:'
071340071008     c                   when      §AR8tsd = 'L2' or
071350071008     c                             §AR8tsd = 'L4'
071360060407     c                   eval      PLOG1de0 = 'Deposito di:'
071370060407     c                   endsl
071380050707     c                   eval      PLOG1de1 = §A8LOG1t1f
071390050707     c                   eval      PLOG1de2 = §A8LOG1t1g
071400050707     c                   eval      PLOG1de3 = §A8LOG1t1h
071410050707     c                   eval      PLOG1de4 = §A8LOG1t1i
071420050707     c                   eval      PLOG1de5 = §A8LOG1t1j
071430050414      *
071440050414x   1c                   else
071450050414      *
071460050707     c***                eval      PLOG1st0 = *blanks
071470050707     c                   eval      PLOG1st1 = §A8LOG1t1a + §A8LOG1t1b
071480050707     c                   eval      PLOG1st2 = §A8LOG1t1c + §A8LOG1t1d
071490050707     c                   eval      PLOG1st3 = §A8LOG1t1e + §A8LOG1t1f
071500050707     c                   eval      PLOG1st4 = §A8LOG1t1g + §A8LOG1t1h
071510050707     c                   eval      PLOG1st5 = §A8LOG1t1i + §A8LOG1t1j
071520050414      *
071530050414e   1c                   endif
071540050214      *
071550041022     c                   ENDSR
071560050214      *
071570041019      *---------------------------------------------------------------*
071580050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
071590050217      *?Dati di testata: destinatario, cliente di fatturazione       ?*
071600041019      *---------------------------------------------------------------*
071610050217     c     PrtDdtLOG1_T2 BEGSR
071620041019      *
071630051110     c                   clear                   dAR8LOG1T2
071640050218     c                   movel     AR8uni        dAR8LOG1T2
071650041019      *
071660041019      * > destinatario
071670050707if  1c                   if        §A8LOG1T2a <> *blanks
071680050707     c                   eval      PLOG1ccd   =  §A8LOG1T2a
071690050218e   1c                   endif
071700050707if  1c                   if        §A8LOG1T2c =  *blanks
071710050707     c                   clear                   PLOG1rsd
071720050707     c                   eval      PLOG1rsd2  =  §A8LOG1T2b
071730050218x   1c                   else
071740050707     c                   eval      PLOG1rsd   =  §A8LOG1T2b
071750050707     c                   eval      PLOG1rsd2  =  §A8LOG1T2c
071760050218e   1c                   endif
071770050707     c                   eval      PLOG1ind   =  §A8LOG1T2d
071780050707     c                   eval      PLOG1ind2  =  §A8LOG1T2e
071790050707if  1c                   if        §A8LOG1T2f >  *zeros
071800050707     c                   eval      PLOG1cad   =  §A8LOG1T2f
071810041019e   1c                   endif
071820050707     c                   eval      PLOG1lod   =  §A8LOG1T2g
071830041019      * > cliente di fatturazione
071840050707if  1c                   if        §A8LOG1T2h <> *blanks
071850050707     c                   eval      PLOG1ccf   =  §A8LOG1T2h
071860041019e   1c                   endif
071870050707if  1c                   if        §A8LOG1T2j =  *blanks
071880050707     c                   clear                   PLOG1rsf
071890050707     c                   eval      PLOG1rsf2  =  §A8LOG1T2i
071900050218x   1c                   else
071910050707     c                   eval      PLOG1rsf   =  §A8LOG1T2i
071920050707     c                   eval      PLOG1rsf2  =  §A8LOG1T2j
071930050218e   1c                   endif
071940050707     c                   eval      PLOG1inf   =  §A8LOG1T2k
071950050707     c                   eval      PLOG1inf2  =  §A8LOG1T2l
071960050707if  1c                   if        §A8LOG1T2m >  *zeros
071970050707     c                   eval      PLOG1caf   =  §A8LOG1T2m
071980041019e   1c                   endif
071990050707     c                   eval      PLOG1lof   =  §A8LOG1T2n
072000041019      *
072010041019     c                   ENDSR
072020041019      *
072030041019      *---------------------------------------------------------------*
072040050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
072050050217      *?Dati di testata: documento, rif.ordine, note                 ?*
072060041019      *---------------------------------------------------------------*
072070050217     c     PrtDdtLOG1_T3 BEGSR
072080041019      *
072090051110     c                   clear                   dAR8LOG1T3
072100050218     c                   movel     AR8uni        dAR8LOG1T3
072110050706      *
072120050706      * TEST effettiva numericità dei campi previsti numerici:
072130100513     c                   clear                   w053a
072140111013     ***c                   eval      WX    =  %len(§A8LOG1T3b)
072150111013     ***c                                   +  %len(§A8LOG1T3c)
072160111013     ***c                                   +  %len(§A8LOG1T3d)
072170111013     ***c                   eval      w053a =  %subst(dAR8LOG1T3: 4:WX)
072180111013     ***c                   eval      *in31 =  *off
072190111013     ***c     c_Digits      checkr    w053a :  WX                            31
072200111013if  1***c                   if        *in31
072210111013     ***c                   clear                   §A8LOG1T3b
072220111013     ***c                   clear                   §A8LOG1T3c
072230111013     ***c                   clear                   §A8LOG1T3d
072240111013e   1***c                   endif
072250121116     ***c                   eval      WX    =  %len(§A8LOG1T3b)
072260121116     ***c                   eval      w053a =  %subst(dAR8LOG1T3: 4:WX)
072270121116     ***c                   eval      *in31 =  *off
072280121116     ***c     c_Digits      checkr    w053a :  WX                            31
072290121116if  1***c                   if        *in31
072300121116     ***c                   clear                   §A8LOG1T3b
072310121116e   1***c                   endif
072320111013     c                   clear                   w053a
072330111013     c                   eval      WX    =  %len(§A8LOG1T3d)
072340111013     c                   eval      w053a =  %subst(dAR8LOG1T3:15:WX)
072350111013     c                   eval      *in31 =  *off
072360111013     c     c_Digits      checkr    w053a :  WX                            31
072370111013if  1c                   if        *in31
072380111013     c                   clear                   §A8LOG1T3d
072390111013e   1c                   endif
072400041019      *
072410041019      * > dati in testata DDT
072420041019      * - - numero e data documento (DDT)
072430050707     c                   eval      PLOG1nsr = §A8LOG1T3a
072440121116     ***c                   eval      PLOG1aas = §A8LOG1T3b
072450121116     c                   if        §A8Log1T3b <> *blanks   and
072460121116     c                             §A8Log1T3b <> *zeros
072470121116     c                   eval      PLOG1aas = §A8LOG1T3b
072480121116     c                   endif
072490111013     ***c                   if        §A8Log1T3c <> *zeros
072500111013     ***c                   eval      PLOG1nsp = %editc(§A8LOG1T3c:'X')
072510111013     ***c                   endif
072520111013     c                   if        §A8Log1T3c <> *blanks   and
072530111013     c                             §A8Log1T3c <> *zeros
072540111013     c                   eval      PLOG1nsp = §A8LOG1T3c
072550111013     c                   endif
072560100319     c                   if        §A8Log1T3a <> *blanks   and
072570121116     c                             §A8Log1T3b <> *blanks   and
072580100319     c                             §A8Log1T3b <> *zeros
072590100319     c                   eval      PLog1sepA = '/'
072600100319     c                   endif
072610121115     c                   if        §A8Log1T3b <> *zeros    and
072620121115     c                             §A8Log1T3c <> *blanks   and
072630100319     c                             §A8Log1T3c <> *zeros
072640100319     c                   eval      PLog1sepB = '/'
072650100319     c                   endif
072660110707      *     giro la data del documento
072670050707if  1c                   if        §A8LOG1T3d > *zeros
072680060922     c                   eval      *in31 =  *off
072690060922     c     *iso          test(d)                 §A8LOG1T3d             31
072700060922if  2c                   if        NOT *in31
072710050707     c     *iso          movel     §A8LOG1T3d    Data_EUR
072720050707     c     *eur          movel     Data_EUR      PLOG1dsp
072730060922x   2c                   else
072740060922     c                   clear                   PLOG1dsp
072750060922e   2c                   endif
072760050707e   1c                   endif
072770041019      * - - riferimento ordine
072780050707     c                   eval      PLOG1ref = §A8LOG1T3e
072790041019      * - - note consegna
072800050707     c                   eval      PLOG1not = §A8LOG1T3f
072810050707     c                   eval      PLOG1nt2 = §A8LOG1T3g
072820060612      * - - intestazione corpo bolla per Logistica tipo stampa L3 o L4
072830060922     c                   if        §AR8tsd = 'L3' or §AR8tsd = 'L4'
072840060612     c                   eval      *in23 = *On
072850060612     c                   eval      PLOG1int = §A8LOG1T3h
072860060612     c                   else
072870060612     c                   eval      *in23 = *off
072880060612     c                   endif
072890041019      *
072900041019      * > Stampa testata DDT - - - - - - - - - - - - - - - - - - - - -*
072910041019      *
072920041019      *   - OverFlow impossibile qui! -
072930050218     c***                exsr      Test_OF
072940050318     c                   exsr      Posiz_DL
072950081017     c                   if            §AR8cxd = *blanks
072960081017     c                             or  DB0fa4  = 'P'
072970050331     c                   write     A4prtDT0
072980050331     c                   endif
072990050218     c                   write     A4prtDL0
073000081222      /free
073010081222         if  *in55;
073020090406           exsr   PrtDdtLOG1_SimilLogo;
073030120530           //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
073040120530           //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
073050120530           //  write  A4LogoBR2;
073060120530           //else;
073070120530           //  write  A4LogoBAR;
073080120530           //endif;
073090120530           write  A4LogoBRT;
073100081222           write  A4prtDLL;
073110081222         endif;
073120081222      /end-free
073130050218     c*** quante righe?  write     A4prtDL0A
073140041028     c                   write     A4prtDL1A
073150050218     c                   write     A4prtDL1B
073160041019      *
073170041019     c                   ENDSR
073180050214      *
073190041019      *---------------------------------------------------------------*
073200050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
073210050217      *?Dati di dettaglio: articolo, quantità, descrizione           ?*
073220041019      *---------------------------------------------------------------*
073230050217     c     PrtDdtLOG1_C1 BEGSR
073240041019      *
073250041019     c                   clear                   A4prtDL2
073260051110     c                   clear                   dAR8LOG1C1
073270050218     c                   movel     AR8uni        dAR8LOG1C1
073280050706      *
073290050706      * TEST effettiva numericità dei campi previsti numerici:
073300100513     c                   clear                   w053a
073310050706     c                   eval      WX    =  %len(§A8LOG1C1b)
073320100513     c                   eval      w053a =  %subst(dAR8LOG1C1:21:WX)
073330050706     c                   eval      *in31 =  *off
073340110426     c     c_Digits      checkr    w053a :  WX                            31
073350050706if  1c                   if        *in31
073360050706     c                   clear                   §A8LOG1C1b
073370050706e   1c                   endif
073380041019      *
073390041019      * > dati in dettaglio DDT
073400041019      * - - codice prodotto
073410050707     c                   eval      PLOG1cpr  = §A8LOG1C1a
073420041019      * - - quantità
073430050707     c                   eval      PLOG1qta  = §A8LOG1C1b
073440041019      * - - descrizione prodotto
073450050707     c                   eval      PLOG1dpr  = §A8LOG1C1c
073460041019      *
073470041019      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
073480041019      *
073490041019     c                   exsr      Test_OF
073500041019     c                   write     A4prtDL2
073510041019      *
073520041019     c                   ENDSR
073530041019      *
073540041019      *---------------------------------------------------------------*
073550050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
073560050217      *?Dati di dettaglio: descrizione                               ?*
073570041019      *---------------------------------------------------------------*
073580050217     c     PrtDdtLOG1_C2 BEGSR
073590041019      *
073600041019     c                   clear                   A4prtDL2
073610051110     c                   clear                   dAR8LOG1C2
073620050218     c                   movel     AR8uni        dAR8LOG1C2
073630041019      *
073640041019      * > dati in note DDT
073650041019      * - - riga descrittiva
073660050707     c                   eval      PLOG1dpr  = §A8LOG1C2a
073670041019      *
073680041019      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
073690041019      *
073700041019     c                   exsr      Test_OF
073710041019     c                   write     A4prtDL2
073720041019      *
073730041019     c                   ENDSR
073740041019      *
073750041019      *---------------------------------------------------------------*
073760050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
073770050217      *?Dati di dettaglio: descrizioni                               ?*
073780041019      *---------------------------------------------------------------*
073790050217     c     PrtDdtLOG1_C3 BEGSR
073800041019      *
073810041019     c                   clear                   A4prtDL3
073820051110     c                   clear                   dAR8LOG1C3
073830050707     c                   movel     AR8uni        dAR8LOG1C3
073840041019      *
073850041019      * > dati in note DDT
073860041019      * - - riga descrittiva
073870050707     c                   eval      PLOG1txt  = §A8LOG1C3a
073880041019      *
073890041019      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
073900041019      *
073910041019     c                   exsr      Test_OF
073920041019     c                   write     A4prtDL3
073930041019      *
073940041019     c                   ENDSR
073950060614      *
073960060614      *---------------------------------------------------------------*
073970060614      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
073980060614      *?Dati di dettaglio: corpo bolla non fincato                   ?*
073990060614      *---------------------------------------------------------------*
074000060614     c     PrtDdtLOG1_C4 BEGSR
074010060614      *
074020060614     c                   clear                   A4prtDL21
074030060614     c                   clear                   dAR8LOG1C4
074040060614     c                   movel     AR8uni        dAR8LOG1C4
074050060614      *
074060060614      * > dati DDT
074070060614      * - - riga descrittiva
074080060614     c                   eval      PLOG1dpr1 = §A8LOG1C4a
074090060614      *
074100060614      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
074110060614      *
074120060614     c                   exsr      Test_OF
074130060614     c                   write     A4prtDL21
074140060614      *
074150060614     c                   ENDSR
074160090323      *
074170090323      *---------------------------------------------------------------*
074180090323      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
074190090323      *?Dati di dettaglio: descrizioni                               ?*
074200090323      *---------------------------------------------------------------*
074210090323     c     PrtDdtLOG1_C5 BEGSR
074220090323      *
074230090323     c                   clear                   A4prtDL31
074240090323     c                   clear                   dAR8LOG1S3
074250090323     c                   movel     AR8uni        dAR8LOG1S3
074260090323      *
074270090323      * > dati in note DDT
074280090323      * - - riga descrittiva
074290090323     c                   eval      PLOG1txt2 = §A8LOG1S3a
074300090323      *
074310090323      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
074320090323      *
074330090323     c                   exsr      Test_OF
074340090323     c                   write     A4prtDL31
074350090323      *
074360090323     c                   ENDSR
074370041019      *
074380041019      *---------------------------------------------------------------*
074390050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
074400050217      *?Dati di fondo: beni, colli, peso, volume, c/a, porto, trasp. ?*
074410041019      *---------------------------------------------------------------*
074420050217     c     PrtDdtLOG1_P1 BEGSR
074430041019      *
074440051110     c                   clear                   dAR8LOG1P1
074450050707     c                   movel     AR8uni        dAR8LOG1P1
074460050706      *
074470050706      * TEST effettiva numericità dei campi previsti numerici:
074480100513     c                   clear                   w053a
074490050706     c                   eval      WX    =  %len(§A8LOG1P1b)
074500050706     c                                   +  %len(§A8LOG1P1c)
074510050706     c                                   +  %len(§A8LOG1P1d)
074520100513     c                   eval      w053a =  %subst(dAR8LOG1P1:42:WX)
074530050706     c                   eval      *in31 =  *off
074540110426     c     c_Digits      checkr    w053a :  WX                            31
074550050706if  1c                   if        *in31
074560050706     c                   clear                   §A8LOG1P1b
074570050706     c                   clear                   §A8LOG1P1c
074580050706     c                   clear                   §A8LOG1P1d
074590050706e   1c                   endif
074600100513     c                   clear                   w053a
074610050706     c                   eval      WX    =  %len(§A8LOG1P1f)
074620100513     c                   eval      w053a =  %subst(dAR8LOG1P1:67:WX)
074630050706     c                   eval      *in31 =  *off
074640110426     c     c_Digits      checkr    w053a :  WX                            31
074650050706if  1c                   if        *in31
074660050706     c                   clear                   §A8LOG1P1f
074670050706e   1c                   endif
074680041019      *
074690041019      * > dati in chiusura DDT
074700041019      * - - aspetto esteriore dei beni
074710050707     c                   eval      PLOG1aeb  = §A8LOG1P1a
074720041019      * - - numero colli
074730050707     c                   eval      PLOG1ncl  = §A8LOG1P1b
074740041019      * - - peso in kg
074750050707     c                   eval      PLOG1pkg  = §A8LOG1P1c
074760041019      * - - volume in mc
074770050707     c                   eval      PLOG1vlm  = §A8LOG1P1d
074780041019      * - - c/assegno
074790050707     c                   eval      PLOG1cva  = §A8LOG1P1e
074800050707     c                   eval      PLOG1cas  = §A8LOG1P1f
074810041020      * - - causale trasporto
074820050707     c                   eval      PLOG1ctr  = §A8LOG1P1g
074830041020      * - - porto
074840050707     c                   eval      PLOG1pto  = §A8LOG1P1h
074850041020      * - - trasporto a cura
074860050707     c                   eval      PLOG1tra  = §A8LOG1P1i
074870041020      * - - trasportatore
074880050707     c                   eval      PLOG1vet  = §A8LOG1P1j
074890050505      *
074900050505      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
074910050505      *
074920050505      * - - Verifica OverFlow
074930080925      /free
074940080925           select;
074950080925             when  NOT *in45;
074960080925               wMaxLine = $MaxLine4;
074970080925             when  *in45   and   NOT *in46;
074980080925               wMaxLine = $MaxLine4_pl1;
074990080925             when  *in45   and   *in46;
075000080925               wMaxLine = $MaxLine4_pl2;
075010080925           endsl;
075020080925      /end-free
075030050505     c                   exsr      Test_OF
075040050505      * - - Calcolo della posizione finale di box e linee separatrici
075050050505     c                   exsr      Posiz_DL
075060050505      * - - Stampa parte finale DDT
075070050505     c                   write     A4prtDL4
075080050505      *
075090050505      * > Stampa box, linee e testi fissi (no logo)  - - - - - - - - -*
075100090331     c                   if            §AR8cxd = *blanks
075110090331     c                             or  DB0fa4  = 'P'
075120090331     c                   write     A4prtDT0
075130090331     c                   endif
075140050505     c                   write     A4prtDL0A
075150050505     c                   write     A4prtDL0B
075160050505      *
075170050505      * Azzero il numero di pagina per l'eventuale Packing-List
075180050505      *   (successiva)
075190050505     c                   clear                   Page
075200041019      *
075210041019      * N.B.: la copia per il destinatario della LDV deve comparire
075220041019      *       nell'ultima pagina del DDT per il cliente (vedi subr.
075230041019      *       "ChiusuraLDV")
075240041019      *
075250041019     c                   ENDSR
075260050318      *
075270050318      *---------------------------------------------------------------*
075280050414      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
075290050318      *?Dati di packing-list: dettaglio                              ?*
075300050318      *---------------------------------------------------------------*
075310050318     c     PrtDdtLOG1_Z1 BEGSR
075320050318      *
075330050318      * Prima della 1ª riga di packing-list: testata packing-list
075340050505if  1c                   if        Page     = *zeros
075350050318     c                   exsr      PrtPckLst_1
075360050318e   1c                   endif
075370050318      *
075380050318     c                   clear                   A4prtDL5
075390051110     c                   clear                   dAR8LOG1Z1
075400050707     c                   movel     AR8uni        dAR8LOG1Z1
075410050706      *
075420050706      * TEST effettiva numericità dei campi previsti numerici:
075430100513     c                   clear                   w053a
075440050706     c                   eval      WX    =  %len(§A8LOG1Z1d)
075450100513     c                   eval      w053a =  %subst(dAR8LOG1Z1:53:WX)
075460050706     c                   eval      *in31 =  *off
075470110426     c     c_Digits      checkr    w053a :  WX                            31
075480050706if  1c                   if        *in31
075490050706     c                   clear                   §A8LOG1Z1d
075500050706e   1c                   endif
075510050318      *
075520050318      * > dati in dettaglio Packing-List
075530050318      * - - sscc
075540050318     c                   eval      PCsscc   = §A8LOG1Z1a
075550050318      * - - gtin
075560050318     c                   eval      PCgtin   = §A8LOG1Z1b
075570050318      * - - codice prodotto
075580050318     c                   eval      PCcpr2   = §A8LOG1Z1c
075590050318      * - - quantità ul
075600050318     c                   eval      PLOG1QTA = §A8LOG1Z1d
075610050318      * - - tipo contenitore
075620050318     c                   eval      PCtcn    = §A8LOG1Z1e
075630050318      * - - codice contenitore
075640050318     c                   eval      PCccn    = §A8LOG1Z1f
075650050318      * - - numero lotto
075660050318     c                   eval      PClot    = §A8LOG1Z1g
075670050318      * - - data scadenza
075680050318     c                   eval      PCdts    = §A8LOG1Z1h
075690050318      *
075700050318      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
075710050318      *
075720050318     c                   exsr      Test_OF
075730050318     c                   write     A4prtDL5
075740050318      *
075750050318     c                   ENDSR
075760080908      *
075770050606      *---------------------------------------------------------------*
075780050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
075790080925      *?Dati di Fattura: Testata                                     ?*
075800050606      *---------------------------------------------------------------*
075810050606     c     PrtDdtLOG1_S1 BEGSR
075820050606      *
075830050606      * > logo Fattura
075840050606     c                   eval      §LogoF   = 'LOGO' + §AR8logof
075850090107      /free
075860090107         // - Impostazione indicatore che condiziona la stampa del logo
075870120530         //   (o quella del "simil-logo" BRT)
075880090107         *in55 = (%subst(§AR8logof : 1 : 3) = 'LOG'       and
075890090107                  (§AR8tsd  = 'L1'  or  §AR8tsd  = 'L3'));
075900090107      /end-free
075910080908      *
075920060609      *? L O G O !! ?
075930060609      *?vedi stampa testata bolla ?
075940071008     c                   if        (§AR8tsd = 'L1' or §AR8tsd = 'L3') and
075950071008     c                             §AR8logof <> *blanks and
075960071008     c                             %subst(AR8tmd:1:3) <> 'LOG'
075970071008     c                   eval      §LogoF   = 'LOGO' + AR8tmd
075980090107      /free
075990090107           // - Impostaz. indicatore che condiziona la stampa del logo
076000120530           //   (o quella del "simil-logo" BRT)
076010090107           *in55 = (%subst(AR8tmd : 1 : 3) = 'LOG');
076020090107      /end-free
076030060609     c                   endif
076040050606      *
076050051110     c                   clear                   dAR8LOG1S1
076060050606     c                   movel     AR8uni        dAR8LOG1S1
076070050706      *
076080050706      * TEST effettiva numericità dei campi previsti numerici:
076090100513     c                   clear                   w053a
076100050706     c                   eval      WX    =  %len(§A8LOG1S1i)
076110100513     c                   eval      w053a =  %subst(dAR8LOG1S1:216:WX)
076120050706     c                   eval      *in31 =  *off
076130110426     c     c_Digits      checkr    w053a :  WX                            31
076140050706if  1c                   if        *in31
076150050706     c                   clear                   §A8LOG1S1i
076160050706e   1c                   endif
076170050707      *
076180050606      * > intestazione fattura
076190050707if  1c                   if        §A8LOG1S1b =  *blanks
076200050707     c                   clear                   PLOG1rsf
076210050707     c                   eval      PLOG1rsf2  =  §A8LOG1S1a
076220050606x   1c                   else
076230050707     c                   eval      PLOG1rsf   =  §A8LOG1S1a
076240050707     c                   eval      PLOG1rsf2  =  §A8LOG1S1b
076250050606e   1c                   endif
076260050707     c                   eval      PLOG1inf   =  §A8LOG1S1c
076270050707     c                   eval      PLOG1inf2  =  §A8LOG1S1d
076280050707if  1c                   if        §A8LOG1S1e >  *zeros
076290050707     c                   eval      PLOG1caf   =  §A8LOG1S1e
076300050606e   1c                   endif
076310050707     c                   eval      PLOG1lof   =  §A8LOG1S1f
076320050606      * > partita IVA - codice fiscale
076330050707     c                   eval      PLOG1IVA   =  §A8LOG1S1g
076340050606      * > n./data fattura
076350050707     c                   eval      PLOG1nft   =  §A8LOG1S1h
076360050606      *     giro la data fattura
076370050706if  1c                   if        §A8LOG1S1i >  *zeros
076380060922     c                   eval      *in31 =  *off
076390060922     c     *iso          test(d)                 §A8LOG1S1i             31
076400060922if  2c                   if        NOT *in31
076410050707     c     *iso          movel     §A8LOG1S1i    Data_EUR
076420050707     c     *eur          movel     Data_EUR      PLOG1dft
076430060922x   2c                   else
076440060922     c                   clear                   PLOG1dft
076450060922e   2c                   endif
076460050706e   1c                   endif
076470050606      * > rif.ns.spedizione
076480060922     c                   eval      PLOG1nspa =   §A8LOG1S1j
076490050606      * > intestazione corpo fattura
076500050707     c                   eval      PLOG1tesf  =  §A8LOG1S1k
076510050707      *
076520050606      * > Stampa testata FATTURA - - - - - - - - - - - - - - - - - - -*
076530081015      *
076540080925      /free
076550081015         // - Forzatura del salto pagina
076560081015         CurLineA4 = wMaxLine + 1;
076570080925      /end-free
076580080925     c                   exsr      Test_OF
076590081015      /free
076600090323         // - Impostazione dei nuovi limiti di pagina
076610090323         select;
076620090323           when  NOT *in45;
076630090323             wMaxLine = $MaxLine2;
076640090323           when  *in45   and   NOT *in46;
076650090323             wMaxLine = $MaxLine2_pl1;
076660090323           when  *in45   and   *in46;
076670090323             wMaxLine = $MaxLine2_pl2;
076680090323         endsl;
076690081015      /end-free
076700080925     c                   exsr      Posiz_DL
076710081017     c                   if            §AR8cxd = *blanks
076720081017     c                             or  DB0fa4  = 'P'
076730080925     c                   write     A4prtDT0
076740080925     c                   endif
076750080925     c                   write     A4prtDL0D
076760050606     c                   write     A4prtDL1D
076770050606     c                   write     A4prtDL1E
076780050707      *
076790050606     c                   ENDSR
076800050606      *
076810050606      *---------------------------------------------------------------*
076820050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
076830050606      *?Dati di dettaglio corpo: FATTURA                             ?*
076840050606      *---------------------------------------------------------------*
076850050606     c     PrtDdtLOG1_S2 BEGSR
076860050606      *
076870050606     c                   clear                   A4prtDL6
076880051110     c                   clear                   dAR8LOG1S2
076890050606     c                   movel     AR8uni        dAR8LOG1S2
076900050606      *
076910050606      * > dati fattura
076920050606      * - - riga descrittiva 1
076930050707     c                   eval      PLOG1dft1 = §A8LOG1S2a
076940050606      *
076950050606      * > Stampa dettaglio FATTURA - - - - - - - - - - - - - - - - - -*
076960050606      *
076970050606     c                   exsr      Test_OF
076980050606     c                   write     A4prtDL6
076990050606      *
077000050606     c                   ENDSR
077010050606      *
077020050606      *---------------------------------------------------------------*
077030050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
077040050606      *?Dati di dettaglio corpo: FATTURA                             ?*
077050050606      *---------------------------------------------------------------*
077060050606     c     PrtDdtLOG1_S3 BEGSR
077070050606      *
077080050606     c                   clear                   A4prtDL7
077090051110     c                   clear                   dAR8LOG1S3
077100050707     c                   movel     AR8uni        dAR8LOG1S3
077110050606      *
077120050606      * > dati fattura
077130050606      * - - riga descrittiva 2
077140050707     c                   eval      PLOG1dft2 = §A8LOG1S3a
077150050606      *
077160050606      * > Stampa dettaglio FATTURA - - - - - - - - - - - - - - - - - -*
077170050606      *
077180050606     c                   exsr      Test_OF
077190050606     c                   write     A4prtDL7
077200050606      *
077210050606     c                   ENDSR
077220050606      *
077230050606      *---------------------------------------------------------------*
077240050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
077250050606      *?Dati di fondo: FATTURA                                       ?*
077260050606      *---------------------------------------------------------------*
077270050606     c     PrtDdtLOG1_S4 BEGSR
077280050606      *
077290051110     c                   clear                   dAR8LOG1S4
077300050707     c                   movel     AR8uni        dAR8LOG1S4
077310050706      *
077320050706      * TEST effettiva numericità dei campi previsti numerici:
077330100513     c                   clear                   w053a
077340050706     c                   eval      WX    =  %len(§A8LOG1S4a)
077350050706     c                                   +  %len(§A8LOG1S4b)
077360050706     c                                   +  %len(§A8LOG1S4c)
077370100513     c                   eval      w053a =  %subst(dAR8LOG1S4: 1:WX)
077380050706     c                   eval      *in31 =  *off
077390110426     c     c_Digits      checkr    w053a :  WX                            31
077400050706if  1c                   if        *in31
077410050706     c                   clear                   §A8LOG1S4a
077420050706     c                   clear                   §A8LOG1S4b
077430050706     c                   clear                   §A8LOG1S4c
077440050706e   1c                   endif
077450050606      *
077460050606      * > dati in chiusura FATTURA
077470050606      * - - dati di piede fattura riga 1
077480050707     c                   eval      PLOG1pft1 = §A8LOG1S4d
077490050606      * - - dati di piede fattura riga 2
077500050707     c                   eval      PLOG1pft2 = §A8LOG1S4e
077510050606      * - - dati di piede fattura riga 3
077520050707     c                   eval      PLOG1pft3 = §A8LOG1S4f
077530050606      * - - totale imponibile
077540050707     c                   eval      PLOG1timp = §A8LOG1S4a
077550050606      * - - totale iva
077560050707     c                   eval      PLOG1tiva = §A8LOG1S4b
077570050606      * - - totale fattura
077580050707     c                   eval      PLOG1tft  = §A8LOG1S4c
077590050606      *
077600050606      * > Stampa fondo FATTURA - - - - - - - - - - - - - - - - - - - -*
077610050606      *
077620050606      * - - Verifica OverFlow
077630080925      /free
077640080925           select;
077650080925             when  NOT *in45;
077660080925               wMaxLine = $MaxLine4;
077670080925             when  *in45   and   NOT *in46;
077680080925               wMaxLine = $MaxLine4_pl1;
077690080925             when  *in45   and   *in46;
077700080925               wMaxLine = $MaxLine4_pl2;
077710080925           endsl;
077720080925      /end-free
077730050606     c                   exsr      Test_OF
077740050606      * - - Calcolo della posizione finale di box e linee separatrici
077750050606     c                   exsr      Posiz_DL
077760050606      * - - Stampa parte finale FATTURA
077770050606     c                   write     A4prtDL8
077780050606      *
077790050606      * > Stampa box, linee (no logo)   - - - - - - - - - - - - - - -*
077800050606     c                   write     A4prtDL0E
077810050606     c                   write     A4prtDL0F
077820080908      *
077830050606     c                   ENDSR
077840050606      *
077850050606      *---------------------------------------------------------------*
077860050606      *?Impostazione campi nel DDT del mittente LOGISTICA / SAIT     ?*
077870050606      *?Dati di fondo note: FATTURA                                  ?*
077880050606      *---------------------------------------------------------------*
077890050606     c     PrtDdtLOG1_S5 BEGSR
077900050606      *
077910051110     c                   clear                   dAR8LOG1S5
077920050707     c                   movel     AR8uni        dAR8LOG1S5
077930050606      *
077940050606      * > dati in chiusura FATTURA
077950050606      * - - nota 1
077960050707     c                   eval      PLOG1not1 = §A8LOG1S5a
077970050606      * - - nota 2
077980050707     c                   eval      PLOG1not2 = §A8LOG1S5b
077990050606      * - - nota 3
078000050707     c                   eval      PLOG1not3 = §A8LOG1S5c
078010050606      *
078020050606      * > Stampa fondo FATTURA - - - - - - - - - - - - - - - - - - - -*
078030050606      * - - Stampa parte finale FATTURA
078040050606     c                   write     A4prtDL9
078050080908      *
078060050606     c                   ENDSR
078070050321      *
078080050321      *---------------------------------------------------------------*
078090050414      *?Impostazione nuova pagina di DDT Logistica / Sait            ?*
078100050321      *---------------------------------------------------------------*
078110050321     c     NewPageDL     BEGSR
078120050321      *
078130080925      /free
078140080925         // Impostazione nuovo limite di OF
078150080925         SELECT;
078160080925           // Piede DDT   oppure   Piede Fattura
078170080925           WHEN  AR8trk = 'P1'   or   AR8trk = 'S4';
078180080925             select;
078190080925               when  NOT *in45;
078200080925                 wMaxLine = $MaxLine4;
078210080925               when  *in45   and   NOT *in46;
078220080925                 wMaxLine = $MaxLine4_pl1;
078230080925               when  *in45   and   *in46;
078240080925                 wMaxLine = $MaxLine4_pl2;
078250080925             endsl;
078260080925           // Dettaglio DDT  oppure   Testata & Dettaglio Fattura
078270080925           OTHER;
078280080925             select;
078290080925               when  NOT *in45;
078300090323                 wMaxLine = $MaxLine2;
078310080925               when  *in45   and   NOT *in46;
078320090323                 wMaxLine = $MaxLine2_pl1;
078330080925               when  *in45   and   *in46;
078340090323                 wMaxLine = $MaxLine2_pl2;
078350080925             endsl;
078360080925         ENDSL;
078370080925      /end-free
078380080925      *
078390080825      * RE-impostazione posizioni
078400080825     c                   exsr      Posiz_DL
078410080825      *
078420050321     c                   write     A4prtSP1
078430081006      /free
078440081006         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
078450081006           write  A4prtNUL;
078460081006         endif;
078470081006      /end-free
078480050321      *
078490050505sel 1c                   select
078500050505      * - altra pagina DDT Logistica / Sait
078510050707w   1c                   when          %subst(AR8trk:1:1) <> 'Z'
078520050606     c                             and %subst(AR8trk:1:1) <> 'S'
078530050321     c                   write     A4prtDL1B
078540050505      * - altra pagina Packing-List
078550050505w   1c                   when      %subst(AR8trk:1:1) =  'Z'
078560050502     c                              and   Page        >  *zeros
078570050321     c                   write     A4prtDL1C
078580050606      * - altra pagina Fattura
078590050606w   1c                   when      %subst(AR8trk:1:1) =  'S'
078600050606     c                              and   Page        >  *zeros
078610050606     c                   write     A4prtDL1E
078620050505e   1c                   endsl
078630050321      *
078640050321     c                   ENDSR
078650050318      *
078660050318      *---------------------------------------------------------------*
078670080709      *?Stampa testata della Packing-List per LOGISTICA              ?*
078680050318      *---------------------------------------------------------------*
078690050318     c     PrtPckLst_1   BEGSR
078700050318      *
078710050318      * > Salto a pagina successiva  - - - - - - - - - - - - - - - - -*
078720050318     c                   exsr      Test_OF
078730050318      *
078740050505      * > Stampa box, linee e testi fissi (CON logo)  - - - - - - - - -*
078750050318      *
078760050318      * - - Stampa "Copia per il destinatario" nella pagina successiva
078770081017     c*** poi in Test_OF:   if            §AR8cxd = *blanks
078780081017     c*** poi in Test_OF:             or  DB0fa4  = 'P'
078790050505     c*** poi in Test_OF:   write     A4prtDT0
078800050505     c*** poi in Test_OF:   endif
078810050318      * - - Stampa dati destinatario nel box superiore
078820050318     c                   write     A4prtDL1A
078830050318      * - - Stampa logo e box superiore x destinatario
078840050318     c                   write     A4prtDL0
078850081222      /free
078860081222         if  *in55;
078870090406           exsr   PrtDdtLOG1_SimilLogo;
078880120530           //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
078890120530           //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
078900120530           //  write  A4LogoBR2;
078910120530           //else;
078920120530           //  write  A4LogoBAR;
078930120530           //endif;
078940120530           write  A4LogoBRT;
078950081222           write  A4prtDLL;
078960081222         endif;
078970081222      /end-free
078980050318      * - - Stampa dati di testata nel box generale
078990050318     c                   write     A4prtDL1C
079000050318      * - - Stampa box generale
079010050318     c*** poi in Test_OF:   write     A4prtDL0C
079020050318      *
079030050318      * Impostazione del nuovo limite x overflow in packing-list
079040080925      /free
079050080925         select;
079060080925           when  NOT *in45;
079070090323             wMaxLine = $MaxLine2;
079080080925           when  *in45   and   NOT *in46;
079090090323             wMaxLine = $MaxLine2_pl1;
079100080925           when  *in45   and   *in46;
079110090323             wMaxLine = $MaxLine2_pl2;
079120080925         endsl;
079130080925      /end-free
079140050318      *
079150050318     c                   ENDSR
079160041019      *
079170041020      *---------------------------------------------------------------*
079180050414      *?Posizionamento di linee e box nel DDT LOGISTICA / SAIT       ?*
079190041020      *---------------------------------------------------------------*
079200041021     c     Posiz_DL      BEGSR
079210050218      *
079220080924      /free
079230080924         // Impostazione posizioni di logo/box
079240080924         // (posizioni di "partenza" diverse a seconda del posizionam.
079250080924         //  iniziale nel foglio A4 attualmente in stampa)
079260080924         select;
079270080924           when  NOT *in45;
079280081006             §PBA =  3.255;
079290080924           when  *in45   and   NOT *in46;
079300090422             §PBA = 0.255;
079310080924           when  *in45   and   *in46;
079320090422             §PBA = 6.005;
079330080924         endsl;
079340080924      /end-free
079350080708     c                   eval      §PBB  =  §PBA + 1.27
079360080708     c                   eval      §LOA  =  §PBA + 0.63
079370080708     c                   eval      §PS0  =  §PBA + 0.72
079380080708     c                   eval      §PS1  =  §PS0 + 0.10
079390080708     c                   eval      §PS2  =  §PS1 + 0.10
079400080708     c                   eval      §PS3  =  §PS2 + 0.10
079410080708     c                   eval      §PS4  =  §PS3 + 0.10
079420080708     c                   eval      §PS5  =  §PS4 + 0.10
079430080708      * Impostazione posizioni di logo/box per fattura
079440080708     c                   eval      §PBC  =  §PBA + 0.92
079450080708     c                   eval      §LOB  =  §PBA + 0.72
079460080708      * Impostazione posizioni di note fine fattura
079470080925     c                   eval      §PS6  =  §PBA + 5.35
079480081003     c                   eval      §PS7  =  §PS6 + 0.08
079490081003     c                   eval      §PS8  =  §PS7 + 0.08
079500080708      *
079510050321if  1c                   if        Page  <= 1
079520050218      *   (posizione di "partenza" su 1ª pagina)
079530080924     c                   eval      §PB1  =  §PBA + 1.375
079540050218x   1c                   else
079550050318      *   (posizione di "partenza" su pagine successive)
079560080822     c                   eval      §PB1  =  §PBA
079570050218e   1c                   endif
079580080924     c                   eval      §PB2  =  §PBA + 5.275
079590050218      * Impostazione posizioni di linee (orizzontali e verticali)
079600050218      *   nella parte superiore del box (testata)
079610050218     c                   eval      §LO1  =  §PB1 + 0.13
079620050318if  1c                   if        %subst(AR8trk:1:1) <> 'Z'
079630050218     c                   eval      §LO2  =  §LO1 + 0.13
079640050318     c                   eval      §LO3  =  §LO2 + 0.11
079650050318x   1c                   else
079660080825     c                   eval      §LO2  =  §LO1 + 0.115
079670050318     c                   eval      §LO3  =  §LO2
079680050318e   1c                   endif
079690050607if  1c                   if        %subst(AR8trk:1:1) = 'S'
079700050610     c                   eval      §LO3  =  §PB2 - 0.40
079710050607     c                   EndIf
079720050218      * Impostazione posizioni di linee (orizzontali e verticali)
079730050218      *   nella parte inferiore del box (piede)
079740050318if  1c                   if        %subst(AR8trk:1:1) <> 'Z'
079750080822     c                   eval      §LO4  =  §PB2 - 0.52
079760090422     c                   eval      §LO5  =  §LO4 + 0.13
079770090422     c                   eval      §LO6  =  §LO5 + 0.13
079780090422     c                   eval      §LO7  =  §LO6 + 0.13
079790050318e   1c                   endif
079800050218      * Impostazione lunghezza delle linee verticali
079810050218      *   nella parte superiore del box (testata)
079820050218     c                   eval      §LV1  =  §LO1 - §PB1
079830050218     c                   eval      §LV2  =  §LO2 - §LO1
079840050318if  1c                   if        %subst(AR8trk:1:1) <> 'Z'
079850050218     c                   eval      §LV3  =  §LO3 - §LO2
079860050318e   1c                   endif
079870050607if  1c                   if        %subst(AR8trk:1:1) = 'S'
079880050607     c                   eval      §LV2  =  0.40
079890050607     c                   EndIf
079900050218      * La lunghezza delle linee verticali separatrici nel dettaglio
079910050218      *   dipende dalla fase di stampa: se si sta stampando ancora il
079920050218      *   dettaglio deve essere più lunga, visto che non viene stampa-
079930050218      *   ta la parte finale del DDT...
079940080924if  1c                   if        %subst(AR8trk:1:1) = 'P'
079950080924     c                              and  (§PS1        < $LimOF
079960080924     c                                and NOT *in45
079970080924     c                                OR  §PS1        < ($LimOF - c_Len1_inch)
079980080924     c                                and *in45   and   NOT *in46
079990081003     c                                OR  §PS1        < ($LimOF + c_Len2_inch)
080000080924     c                                and *in45   and   *in46)
080010050318     c                              and   CurLineA4   <  wMaxLine
080020050218     c                   eval      §LV4  =  §LO4 - §LO3
080030050218x   1c                   else
080040050218     c                   eval      §LV4  =  §PB2 - §LO3
080050050218e   1c                   endif
080060080908      *
080070071008     c                   if        §AR8tsd = 'L3' or §AR8tsd = 'L4' and
080080060609     c                             %subst(AR8trk:1:1) <> 'Z' and
080090060609     c                             %subst(AR8trk:1:1) <> 'S'
080100071008     c                   eval      §LV3 = 0
080110071008     c                   eval      §LV4 = 0
080120060609     c                   endif
080130080908      *
080140050502      * - qui imposto le posizioni per il piede del box
080150050318if  1c                   if        %subst(AR8trk:1:1) <> 'Z'
080160050218     c                   eval      §LV5  =  §LO5 - §LO4
080170050218     c                   eval      §LV6  =  §LO6 - §LO5
080180050218     c                   eval      §LV7  =  §PB2 - §LO7
080190050318e   1c                   endif
080200041028      * Impostazione posizioni dei campi di testo verticali
080210080822     c                   eval      §PR1  =  §PB2 - 0.05
080220041020      *
080230041020     c                   ENDSR
080240050113      *
080250050113      *---------------------------------------------------------------*
080260050217      *?Stampa DDT del mittente ARCTE                                ?*
080270050113      *---------------------------------------------------------------*
080280050217     c     DDT_Arcte     BEGSR
080290050113      *
080300081002      /free
080310081002         // Impostazione del nuovo limite x overflow
080320081002         // (Codesto limite è valido solo per la stampa del dettaglio.
080330081002         //   Per la stampa del "fondo" bolla occorre REimpostarlo ad
080340081002         //   un valore più basso.
080350081002         //  Ovvero: stampo più righe di dettaglio che posso; se alla
080360081002         //  fine delle righe di dettaglio non rimane spazio a suffi-
080370081002         //  cienza per il fondo: prima della stampa del fondo si salta
080380081002         //  alla pagina successiva, dove non si stampa alcuna riga di
080390081002         //  dettaglio.)
080400081002         select;
080410081002           when  NOT *in45;
080420081002             wMaxLine = $MaxLine2;
080430081002           when  *in45   and   NOT *in46;
080440090324             wMaxLine = $MaxLine5_pl1;
080450081002           when  *in45   and   *in46;
080460090324             wMaxLine = $MaxLine5_pl2;
080470081002         endsl;
080480081002
080490081002         // Posizionamento iniziale all'interno del foglio A4
080500081002         write  A4prtSP1;
080510081002      /end-free
080520050113      *
080530050113      * Impostazione testi fissi - - - - - - - - - - - - - - - - - - -*
080540050113      *
080550050113     c                   eval      PCLtx1 =
080560050113     c                             'La merce viaggia a rischio e pericolo -
080570050113     c                              del committente. I reclami vanno fatti -
080580050113     c                              per iscritto entro 8 gg. dal ricevimento -
080590050113     c                              delle merce.'
080600050113     c                   eval      PCLtx2 =
080610050113     c                              'Il ritorno di merce deve -
080620050113     c                              essere preventivamente autorizzato -
080630050113     c                              dal Gruppo Arcte. Foro competente -
080640050113     c                              come da condizioni di vendita.'
080650050113      *
080660050113     c                   eval      PCLpx1 =
080670050113     c                             'CONTRIBUTO CONAI ASSOLTO'
080680050217      *
080690130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
080700130315      *  "StampaDDT")
080710050217     c***  KLDV          setll     FIAR8000       (già fatto)
080720050217     c***  KLDV          reade     FIAR8000       (già fatto)
080730050113      *
080740050113      * Reperimento dati, impostazione e stampa DDT mittente  - - - -*
080750050113      *
080760050113if  1c                   dow       NOT %eof(FIAR801L)
080770050113      *
080780050113sel 2c                   select
080790050113      * - Record annullato (non si stampa nulla)
080800050217w   2c                   when      AR8atb  <> *blanks
080810050707      * - Dati di dettaglio: descrizioni generiche
080820050217w   2c                   when      AR8trk   = 'C1'
080830050217     c                   exsr      PrtDdtARCT_C1
080840050217      * - Dati di dettaglio: articolo, descrizioni, taglie, q.tà
080850050217w   2c                   when      AR8trk   = 'C2'
080860050217     c                   exsr      PrtDdtARCT_C2
080870050113      * - Dati di testata: destinatario, cliente di fatturazione
080880050217w   2c                   when      AR8trk   = 'T1'
080890050217     c                   exsr      PrtDdtARCT_T1
080900050113      * - Dati di testata: agente, numero ordine, estremi documento
080910050217w   2c                   when      AR8trk   = 'T2'
080920050217     c                   exsr      PrtDdtARCT_T2
080930050113      * - Dati di fondo: mezzo e causale trasporto, colli, peso
080940050217w   2c                   when      AR8trk   = 'P1'
080950050217     c                   exsr      PrtDdtARCT_P1
080960050113e   2c                   endsl
080970050113      *
080980050113     c     KLDV          reade     FIAR8000
080990050113      *
081000050113e   1c                   enddo
081010050217      *
081020050217      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
081030050217      *
081040050217      * - - Verifica OverFlow
081050050217     c                   exsr      Test_OF
081060050218      *
081070050218      * - - Calcolo la posizione finale del box e di linee separatrici
081080050218     c                   exsr      Posiz_AC
081090050217      *
081100050217      * - - Stampa parte finale DDT
081110050217     c                   write     A4prtAC4
081120050217      *
081130050217      * > Stampa box, linee e testi fissi (no logo)  - - - - - - - - -*
081140081017     c                   if            §AR8cxd = *blanks
081150081017     c                             or  DB0fa4  = 'P'
081160050331     c                   write     A4prtDT0
081170050331     c                   endif
081180050217     c                   write     A4prtAC0A
081190050217     c                   write     A4prtAC0B
081200050217      *
081210050217      * Reimpostazione del vecchio limite x overflow
081220081002      /free
081230081002         select;
081240081002           when  NOT *in45;
081250081002             wMaxLine = $MaxLine2;
081260081002           when  *in45   and   NOT *in46;
081270081002             wMaxLine = $MaxLine2_pl1;
081280081002           when  *in45   and   *in46;
081290081002             wMaxLine = $MaxLine2_pl2;
081300081002         endsl;
081310081002      /end-free
081320050113      *
081330050113     c                   ENDSR
081340050214      *
081350050113      *---------------------------------------------------------------*
081360050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
081370050217      *?Dati di testata: destinatario, cliente di fatturazione       ?*
081380050113      *---------------------------------------------------------------*
081390050217     c     PrtDdtARCT_T1 BEGSR
081400050114      *
081410050114      * > logo
081420050114     c                   eval      §Logo  = 'LOGO' + AR8tmd
081430050113      *
081440051110     c                   clear                   dAR8ARCTT1
081450050707     c                   movel     AR8uni        dAR8ARCTT1
081460050706      *
081470050706      * TEST effettiva numericità dei campi previsti numerici:
081480100513     c                   clear                   w053a
081490050706     c                   eval      WX    =  %len(§ARCTT1a02)
081500100513     c                   eval      w053a =  %subst(dAR8ARCTT1:  7:WX)
081510050706     c                   eval      *in31 =  *off
081520110426     c     c_Digits      checkr    w053a :  WX                            31
081530050706if  1c                   if        *in31
081540050706     c                   clear                   §ARCTT1a02
081550050706e   1c                   endif
081560050706     c                   eval      WX    =  %len(§ARCTT1b02)
081570100513     c                   eval      w053a =  %subst(dAR8ARCTT1:163:WX)
081580050706     c                   eval      *in31 =  *off
081590110426     c     c_Digits      checkr    w053a :  WX                            31
081600050706if  1c                   if        *in31
081610050706     c                   clear                   §ARCTT1b02
081620050706e   1c                   endif
081630050113      *
081640050113      * > destinatario
081650050707     c                   eval      PARCTccd  = §ARCTT1a01
081660050707     c                   eval      PARCTccd1 = §ARCTT1a02
081670050707     c                   eval      PARCTrsd  = §ARCTT1a04
081680050707     c                   eval      PARCTrsd2 = §ARCTT1a05
081690050707     c                   eval      PARCTind  = §ARCTT1a06
081700050707     c                   eval      PARCTcad  = §ARCTT1a07
081710050707     c                   eval      PARCTlod  = §ARCTT1a08
081720050113      * > cliente di fatturazione
081730050707     c                   eval      PARCTccf  = §ARCTT1b01
081740050707     c                   eval      PARCTccf1 = §ARCTT1b02
081750050707     c                   eval      PARCTccf2 = §ARCTT1b03
081760050707     c                   eval      PARCTrsf  = §ARCTT1b04
081770050707     c                   eval      PARCTrsf2 = §ARCTT1b05
081780050707     c                   eval      PARCTinf  = §ARCTT1b06
081790050707     c                   eval      PARCTcaf  = §ARCTT1b07
081800050707     c                   eval      PARCTlof  = §ARCTT1b08
081810050113      * > magazzino spedizione
081820050707     c                   eval      PARCTmag  = §ARCTT1c01
081830050113      *
081840050113     c                   ENDSR
081850050113      *
081860050113      *---------------------------------------------------------------*
081870050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
081880050217      *?Dati di testata: documento                                   ?*
081890050113      *---------------------------------------------------------------*
081900050217     c     PrtDdtARCT_T2 BEGSR
081910050113      *
081920051110     c                   clear                   dAR8ARCTT2
081930050707     c                   movel     AR8uni        dAR8ARCTT2
081940050706      *
081950050706      * TEST effettiva numericità dei campi previsti numerici:
081960100513     c                   clear                   w053a
081970050706     c                   eval      WX    =  %len(§ARCTT2a02)
081980050706     c                                   +  %len(§ARCTT2a03)
081990100513     c                   eval      w053a =  %subst(dAR8ARCTT2: 9:WX)
082000050706     c                   eval      *in31 =  *off
082010110426     c     c_Digits      checkr    w053a :  WX                            31
082020050706if  1c                   if        *in31
082030050706     c                   clear                   §ARCTT2a02
082040050706     c                   clear                   §ARCTT2a03
082050050706e   1c                   endif
082060050113      *
082070050113      * > dati in testata DDT
082080050113      * - - numero e data documento (DDT)
082090050707     c                   eval      PARCTnsr = §ARCTT2a01
082100050707     c                   eval      PARCTnsp = §ARCTT2a02
082110050707     c                   eval      PARCTdsp = §ARCTT2a03
082120050113      *
082130050113      * > Stampa testata DDT - - - - - - - - - - - - - - - - - - - - -*
082140050113      *
082150050113      *   - OverFlow impossibile qui! -
082160050217     c***                exsr      Test_OF
082170050217     c                   exsr      Posiz_AC
082180081017     c                   if            §AR8cxd = *blanks
082190081017     c                             or  DB0fa4  = 'P'
082200050331     c                   write     A4prtDT0
082210050331     c                   endif
082220050217     c                   write     A4prtAC0
082230050217     c*** quante righe?  write     A4prtAC0A
082240050217     c                   write     A4prtAC1A
082250050217     c                   write     A4prtAC1B
082260050113      *
082270050113     c                   ENDSR
082280050214      *
082290050113      *---------------------------------------------------------------*
082300050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
082310050217      *?Dati di dettaglio: descrizioni generiche                     ?*
082320050113      *---------------------------------------------------------------*
082330050217     c     PrtDdtARCT_C1 BEGSR
082340050113      *
082350050114     c                   clear                   A4prtAC2
082360051110     c                   clear                   dAR8ARCTC1
082370050707     c                   movel     AR8uni        dAR8ARCTC1
082380050217      * > dati in note DDT
082390050217      * - - riga descrittiva
082400050707     c                   eval      PARCTdpr  = §ARCTC1a01
082410050113      *
082420050113      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
082430050113      *
082440050113     c                   exsr      Test_OF
082450050114     c                   write     A4prtAC2
082460050113      *
082470050113     c                   ENDSR
082480050113      *
082490050113      *---------------------------------------------------------------*
082500050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
082510050217      *?Dati di dettaglio: descrizione, taglie, qtà                  ?*
082520050113      *---------------------------------------------------------------*
082530050217     c     PrtDdtARCT_C2 BEGSR
082540050113      *
082550050114     c                   clear                   A4prtAC3
082560051110     c                   clear                   dAR8ARCTC2
082570050707     c                   movel     AR8uni        dAR8ARCTC2
082580050706      *
082590050706      * TEST effettiva numericità dei campi previsti numerici:
082600100513     c                   clear                   w053a
082610050706     c                   eval      WX    =  %len(§ARCTC2b00)
082620050706     c                                   +  %len(§ARCTC2b01)
082630050706     c                                   +  %len(§ARCTC2b02)
082640050706     c                                   +  %len(§ARCTC2b03)
082650050706     c                                   +  %len(§ARCTC2b04)
082660050706     c                                   +  %len(§ARCTC2b05)
082670050706     c                                   +  %len(§ARCTC2b06)
082680050706     c                                   +  %len(§ARCTC2b07)
082690050706     c                                   +  %len(§ARCTC2b08)
082700050706     c                                   +  %len(§ARCTC2b09)
082710100513     c                   eval      w053a =  %subst(dAR8ARCTC2:59:WX)
082720050706     c                   eval      *in31 =  *off
082730110426     c     c_Digits      checkr    w053a :  WX                            31
082740050706if  1c                   if        *in31
082750050706     c                   clear                   §ARCTC2b00
082760050706     c                   clear                   §ARCTC2b01
082770050706     c                   clear                   §ARCTC2b02
082780050706     c                   clear                   §ARCTC2b03
082790050706     c                   clear                   §ARCTC2b04
082800050706     c                   clear                   §ARCTC2b05
082810050706     c                   clear                   §ARCTC2b06
082820050706     c                   clear                   §ARCTC2b07
082830050706     c                   clear                   §ARCTC2b08
082840050706     c                   clear                   §ARCTC2b09
082850050706e   1c                   endif
082860050113      *
082870050113      * > dati in dettaglio DDT
082880050113      * - - descrizione articolo
082890050707     c                   eval      PARCTcpr  = §ARCTC2baa
082900050113      * - - taglia 0
082910050707     c                   eval      PARCTt0   = §ARCTC2b00
082920050113      * - - taglia 1
082930050707     c                   eval      PARCTt1   = §ARCTC2b01
082940050113      * - - taglia 2
082950050707     c                   eval      PARCTt2   = §ARCTC2b02
082960050113      * - - taglia 3
082970050707     c                   eval      PARCTt3   = §ARCTC2b03
082980050113      * - - taglia 4
082990050707     c                   eval      PARCTt4   = §ARCTC2b04
083000050113      * - - taglia 5
083010050707     c                   eval      PARCTt5   = §ARCTC2b05
083020050113      * - - taglia 6
083030050707     c                   eval      PARCTt6   = §ARCTC2b06
083040050113      * - - taglia 7
083050050707     c                   eval      PARCTt7   = §ARCTC2b07
083060050113      * - - taglia 8
083070050707     c                   eval      PARCTt8   = §ARCTC2b08
083080050113      * - - quantità
083090050707     c                   eval      PARCTqta  = §ARCTC2b09
083100050113      *
083110050113      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
083120050113      *
083130050113     c                   exsr      Test_OF
083140050114     c                   write     A4prtAC3
083150050113      *
083160050113     c                   ENDSR
083170050113      *
083180050113      *---------------------------------------------------------------*
083190050217      *?Impostazione campi nel DDT del mittente ARCTE                ?*
083200050217      *?Dati di fondo: beni, colli, peso, tot.qtà, porto, trasp.     ?*
083210050113      *---------------------------------------------------------------*
083220050217     c     PrtDdtARCT_P1 BEGSR
083230090324      *
083240090324      /free
083250090324           select;
083260090324             when  NOT *in45;
083270090324               wMaxLine = $MaxLine4;
083280090324             when  *in45   and   NOT *in46;
083290090324               wMaxLine = $MaxLine4_pl3;
083300090324             when  *in45   and   *in46;
083310090324               wMaxLine = $MaxLine4_pl4;
083320090324           endsl;
083330090324      /end-free
083340050113      *
083350051110     c                   clear                   dAR8ARCTP1
083360050707     c                   movel     AR8uni        dAR8ARCTP1
083370050706      *
083380050706      * TEST effettiva numericità dei campi previsti numerici:
083390100513     c                   clear                   w053a
083400050706     c                   eval      WX    =  %len(§ARCTP1a03)
083410100513     c                   eval      w053a =  %subst(dAR8ARCTP1:28:WX)
083420050706     c                   eval      *in31 =  *off
083430110426     c     c_Digits      checkr    w053a :  WX                            31
083440050706if  1c                   if        *in31
083450050706     c                   clear                   §ARCTP1a03
083460050706e   1c                   endif
083470050706     c                   eval      WX    =  %len(§ARCTP1b02)
083480050706     c                                   +  %len(§ARCTP1b03)
083490050706     c                                   +  %len(§ARCTP1b04)
083500100513     c                   eval      w053a =  %subst(dAR8ARCTP1:68:WX)
083510050706     c                   eval      *in31 =  *off
083520110426     c     c_Digits      checkr    w053a :  WX                            31
083530050706if  1c                   if        *in31
083540050706     c                   clear                   §ARCTP1b02
083550050706     c                   clear                   §ARCTP1b03
083560050706     c                   clear                   §ARCTP1b04
083570050706e   1c                   endif
083580050113      *
083590050113      * > dati in chiusura DDT
083600050114      * - - costanti di fine doc. e n. borderò
083610050707     c                   eval      PARCTc1   = §ARCTP1a01
083620050707     c                   eval      PARCTc2   = §ARCTP1a02
083630050707     c                   eval      PARCTc3   = §ARCTP1a03
083640050114      * - - costante segue fattura
083650050707     c                   eval      PARCTc4   = §ARCTp1a04
083660050113      * - - aspetto esteriore dei beni
083670050707     c                   eval      PARCTaeb  = §ARCTP1b01
083680050113      * - - numero colli
083690050707     c                   eval      PARCTncl  = §ARCTP1b02
083700050113      * - - peso in kg
083710050707     c                   eval      PARCTpkg  = §ARCTP1b03
083720050113      * - - totale qtà
083730050707     c                   eval      PARCTtqta = §ARCTP1b04
083740050113      * - - causale trasporto
083750050707     c                   eval      PARCTctr  = §ARCTP1c01
083760050113      * - - porto
083770050707     c                   eval      PARCTpto  = §ARCTP1c02
083780050113      * - - trasporto a cura
083790050707     c                   eval      PARCTtra  = §ARCTP1c03
083800050113      * - - trasportatore
083810050707     c                   eval      PARCTvet  = §ARCTP1d01
083820050217     *** *
083830050217     *** * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
083840050217     *** * ?(impostata a fine ciclo di lettura - sr. DDT_Arcte)?
083850050113      *
083860050113      * N.B.: la copia per il destinatario della LDV deve comparire
083870050113      *       nell'ultima pagina del DDT per il cliente (vedi subr.
083880050113      *       "ChiusuraLDV")
083890050113      *
083900050113     c                   ENDSR
083910081002      /free
083920081002       //--------------------------------------------------------------
083930140303       //?Impostazione nuova pagina di DDT Arcte                       ?
083940081002       //--------------------------------------------------------------
083950081002       BEGSR  NewPageAC;
083960081002
083970081002         // Impostazione del nuovo limite x overflow
083980081002         select;
083990081002           when  NOT *in45;
084000081002             wMaxLine = $MaxLine2;
084010081002           when  *in45   and   NOT *in46;
084020090324             wMaxLine = $MaxLine5_pl1;
084030081002           when  *in45   and   *in46;
084040090324             wMaxLine = $MaxLine5_pl2;
084050081002         endsl;
084060081002
084070081002         write  A4prtSP1;
084080081002         write  A4prtAC1B;
084090081002
084100081002       ENDSR;
084110081002      /end-free
084120050113      *
084130050113      *---------------------------------------------------------------*
084140050217      *?Posizionamento di linee e box nel DDT ARCTE                  ?*
084150050113      *---------------------------------------------------------------*
084160050114     c     Posiz_AC      BEGSR
084170080708      *
084180081002      /free
084190081002         // Impostazione posizioni di logo/box
084200081002         // (posizioni di "partenza" diverse a seconda del posizionam.
084210081002         //  iniziale nel foglio A4 attualmente in stampa)
084220081002         select;
084230081002           when  NOT *in45;
084240081002             §PBA =  3.130;
084250081002           when  *in45   and   NOT *in46;
084260081002             §PBA = 0.260;
084270081002           when  *in45   and   *in46;
084280081002             §PBA = 6.010;
084290081002         endsl;
084300081002      /end-free
084310080708     c                   eval      §PBB  =  §PBA + 1.27
084320080708     c                   eval      §LOA  =  §PBA + 0.63
084330050218      *
084340050217      * Impostazione posizioni di box
084350050217if  1c                   if        Page  <= 1
084360050217      *   (posizione di "partenza" su 1ª pagina)
084370081002     c                   eval      §PB1  =  §PBA + 1.38
084380050217x   1c                   else
084390050217      *   (posizione di "partenza" su pagine successive)
084400081002     c                   eval      §PB1  =  §PBA
084410050217e   1c                   endif
084420081002     c                   eval      §PB2  =  §PBA + 5.395
084430050217      * Impostazione posizioni di linee (orizzontali e verticali)
084440050217      *   nella parte superiore del box (testata)
084450080708     c                   eval      §LO1  =  §PB1 + 0.13
084460080708     c                   eval      §LO2  =  §LO1 + 0.13
084470050217      * Impostazione posizioni di linee (orizzontali e verticali)
084480050217      *   nella parte inferiore del box (piede)
084490081002     c                   eval      §LO3  =  §PBA + 5.005
084500081002     c                   eval      §LO4  =  §PBA + 5.135
084510081002     c                   eval      §LO5  =  §PBA + 5.265
084520050217      * Impostazione lunghezza delle linee verticali
084530050217      *   nella parte superiore del box (testata)
084540050217     c                   eval      §LV1  =  §LO1 - §PB1
084550050217     c                   eval      §LV2  =  §LO2 - §LO1
084560050217      * La lunghezza delle linee verticali separatrici nel dettaglio
084570050217      *   dipende dalla fase di stampa: se si sta stampando ancora il
084580050217      *   dettaglio deve essere più lunga, visto che non viene stampa-
084590050217      *   ta la parte finale del DDT...
084600050217if  1c                   if        %subst(AR8trk:1:1) = 'P'
084610050218     c                             and    §PS1        < $LimOF
084620050218     c                             and    CurLineA4   < wMaxLine
084630050217     c                   eval      §LV3  =  §LO3 - §LO2
084640050217x   1c                   else
084650050217     c                   eval      §LV3  =  §PB2 - §LO2
084660050217e   1c                   endif
084670050217     c                   eval      §LV4  =  §LO4 - §LO3
084680050217     c                   eval      §LV5  =  §LO5 - §LO4
084690050113      * Impostazione posizioni dei campi di testo verticali
084700081002     c                   eval      §PR1  =  §PBA + 5.35
084710081002     c                   eval      §PR2  =  §PBA + 5.47
084720050113      *
084730050113     c                   ENDSR
084740050214      *
084750050214      *---------------------------------------------------------------*
084760050217      *?Stampa DDT del mittente Esprinet                             ?*
084770050214      *---------------------------------------------------------------*
084780050214     c     DDT_Esprinet  BEGSR
084790050214      *
084800081002      /free
084810110405         // -?Impostazione del nuovo limite x overflow?
084820110405         //  ?Codesto limite è valido solo per la stampa del dettaglio.?
084830110405         //  ?Per la stampa del "fondo" bolla occorre REimpostarlo ad?
084840110405         //  ?un valore più basso.?
084850110405         //  ?Ovvero: stampo più righe di dettaglio che posso; se alla?
084860110405         //  ?fine delle righe di dettaglio non rimane spazio a suffi-?
084870110405         //  ?cienza per il fondo: prima della stampa del fondo si salta?
084880110405         //  ?alla pagina successiva, dove non si stampa alcuna riga di?
084890110405         //  ?dettaglio.?
084900081002         select;
084910081002           when  NOT *in45;
084920081002             wMaxLine = $MaxLine2;
084930081002           when  *in45   and   NOT *in46;
084940090324             wMaxLine = $MaxLine5_pl1;
084950081002           when  *in45   and   *in46;
084960090324             wMaxLine = $MaxLine5_pl2;
084970081002         endsl;
084980081002
084990110405         // -?Posizionamento iniziale all'interno del foglio A4?
085000081002         write  A4prtSP1;
085010110405
085020110405         // -?Pulizia tipo record?
085030110405         clear  SAVtrk;
085040081002      /end-free
085050050214      *
085060050214      * Impostazione testi fissi - - - - - - - - - - - - - - - - - - -*
085070050214      *
085080050325     ***c                   eval      PCLtx1 =
085090050325     ***c                             'Settimo Torinese - Tel. 011.895.39.29 *-
085100050325     ***c                              Padova - Tel. 049.870.50.40 *-
085110050325     ***c                              Bologna - Tel. 051.324.249 *-
085120050325     ***c                              Osimo - Tel. 071.781.96.75 *-
085130050325     ***c                              Roma - Tel. 06.723.15.43'
085140050325     ***c                   eval      PCLtx2 =
085150050325     ***c                             'Casalnuovo - Tel. 081.522.30.31 *-
085160050325     ***c                              Cinisello B. - Tel. 02.66.04.11.13 *-
085170050325     ***c                              Verona - Tel. 045.823.01.65 *-
085180050325     ***c                              Cagliari - Tel. 070.253.178'
085190050325     c                   eval      PCLtx3 =
085200131017     c                             'CONTRIBUTO AMBIENTALE CONAI ASSOLTO'
085210131017     c*//                           OVE DOVUTO'
085220050214      *
085230050214     c                   eval      PCLpx2 =
085240050214     c                             'IL PRESENTE DOCUMENTO E'' REGOLATO-
085250050214     c                              DALLE CONDIZIONI GENERALI DI VENDI-
085260050214     c                             TA PUBBLICATE SUL SITO INTERNET ALL-
085270050214     c                             ''INDIRIZZO www.esprinet.com CHE SI-
085280050325     c                              INTENDONO ESPRESSAMENTE CONOSCIUTE-
085290050325     c                              ED ACCETTATE'
085300050214      *
085310050214     c                   eval      PCLpx3a =
085320050214     c                             'NB. IN CASO DI RITARDATO PAGAMENTO-
085330050214     c                             , VERRANNO ADDEBITATI'
085340050214     c                   eval      PCLpx3b =
085350050214     c                             'INTERESSI AL TASSO DEL PRIME RATE -
085360050214     c                             MAGGIORATO DEL 2% A'
085370050214     c                   eval      PCLpx3c =
085380050214     c                             'DECORRERE DAL GIORNO SUCCESSIVO AL-
085390050214     c                             LA SCADENZA DELLA FATTURA'
085400050214      *
085410050214     c                   eval      PCLpx4a =
085420050214     c                             'Si prega di verificare la Vs. ragi-
085430050214     c                             one sociale, indirizzo e'
085440050214     c                   eval      PCLpx4b =
085450050214     c                             'partita IVA, dandoci eventuale sol-
085460050214     c                             lecita rettifica scritta.'
085470050214     c***                eval      PCLpx4c = *blanks
085480050214      *
085490050214     c                   eval      PCLpx5a =
085500050214     c                             '* I PRODOTTI IMPORTATI DAGLI U.S.A.-
085510050214     c                              SONO CON SPECIALE'
085520050214     c                   eval      PCLpx5b =
085530050214     c                             'LICENZA CHE PRECLUDE RIESPORTAZIONI-
085540050214     c                              NON AUTORIZZATE.'
085550050214     c***                eval      PCLpx5c = *blanks
085560050214      *
085570050325     ***c                   eval      PCLpx6  =
085580050325     ***c                             'CONTRIBUTO AMBIENTALE CONAI ASSOLTO-
085590050325     ***c                              OVE DOVUTO'
085600050325     c                   eval      PCLpx6  =
085610050325     c                             'LA SOCIETÀ SI IMPEGNA A VERSARE L''-
085620050325     c                             IVA NEI MODI E TERMINI PREVISTI (S-
085630050325     c                             OLIDARIETÀ ART. 60 BIS DPR 633/72-F-
085640050325     c                             INANZIARIA 2005)'
085650050214      *
085660130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
085670130315      *  "StampaDDT")
085680050214     c***  KLDV          setll     FIAR8000       (già fatto)
085690050214     c***  KLDV          reade     FIAR8000       (già fatto)
085700050214      *
085710050214      * Reperimento dati, impostazione e stampa DDT mittente  - - - -*
085720050214      *
085730050214if  1c                   dow       NOT %eof(FIAR801L)
085740050214      *
085750050214sel 2c                   select
085760050214      * - Record annullato (non si stampa nulla)
085770050217w   2c                   when      AR8atb  <> *blanks
085780050214      * - Dati di dettaglio: articolo, descrizione, quantità, ...
085790050217w   2c                   when      AR8trk   = 'C1'
085800050217     c                   exsr      PrtDdtESP1_C1
085810050214      * - Dati di dettaglio: testo (riga libera)
085820050217w   2c                   when      AR8trk   = 'C2'
085830050217     c                   exsr      PrtDdtESP1_C2
085840050214      * - Dati di testata: destinatario, cliente di fatturazione
085850050217w   2c                   when      AR8trk   = 'T1'
085860050217     c                   exsr      PrtDdtESP1_T1
085870050214      * - Dati di testata: estremi documento, riferimenti, ...
085880050217w   2c                   when      AR8trk   = 'T2'
085890050217     c                   exsr      PrtDdtESP1_T2
085900050214      * - Dati di fondo: totali del documento (netto merce, iva, ...)
085910050217w   2c                   when      AR8trk   = 'P1'
085920050217     c                   exsr      PrtDdtESP1_P1
085930050214      * - Dati di fondo: informazioni vettore
085940050217w   2c                   when      AR8trk   = 'P2'
085950050217     c                   exsr      PrtDdtESP1_P2
085960050214e   2c                   endsl
085970110405      *
085980110405     c                   eval      SAVtrk = AR8trk
085990050214      *
086000050214     c     KLDV          reade     FIAR8000
086010050214      *
086020050214e   1c                   enddo
086030050215      *
086040050215      * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
086050050215      *
086060050215      * - - Verifica OverFlow
086070050215     c                   exsr      Test_OF
086080050218      *
086090050218      * - - Calcolo la posizione finale del box e di linee separatrici
086100050218     c                   exsr      Posiz_ES
086110050215      *
086120050215      * - - Stampa parte finale DDT
086130050215     c                   write     A4prtES4
086140050215      *
086150050215      * > Stampa box, linee e testi fissi (no logo)  - - - - - - - - -*
086160081017     c                   if            §AR8cxd = *blanks
086170081017     c                             or  DB0fa4  = 'P'
086180050331     c                   write     A4prtDT0
086190050331     c                   endif
086200050215     c                   write     A4prtES0A
086210050215     c                   write     A4prtES0B
086220050214      *
086230050214      * Reimpostazione del vecchio limite x overflow
086240081002      /free
086250081002         select;
086260081002           when  NOT *in45;
086270081002             wMaxLine = $MaxLine2;
086280081002           when  *in45   and   NOT *in46;
086290081002             wMaxLine = $MaxLine2_pl1;
086300081002           when  *in45   and   *in46;
086310081002             wMaxLine = $MaxLine2_pl2;
086320081002         endsl;
086330081002      /end-free
086340050214      *
086350050214     c                   ENDSR
086360050214      *
086370050214      *---------------------------------------------------------------*
086380050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
086390050217      *?Dati di testata: fatturato a, destinazione, cliente          ?*
086400050214      *---------------------------------------------------------------*
086410050217     c     PrtDdtESP1_T1 BEGSR
086420110405      *
086430110405      * - pulizia record a nuova packing-list
086440110405     c                   if            %subst( SAVtrk : 1 : 1 ) = 'P'
086450110405     c                             or  %subst( SAVtrk : 1 : 1 ) = 'C'
086460110405     c                   exsr      sr_ClearESP1
086470110405     c                   endif
086480050214      *
086490050214      * > logo
086500050214     c                   eval      §Logo  = 'LOGO' + AR8tmd
086510050214      *
086520051110     c                   movel(p)  AR8uni        dAR8ESP1T1
086530050214      *
086540050214      * > fatturato a
086550050707     c                   eval      PCrsf  = §A8ESP1T1a
086560050707     c                   eval      PCinf  = §A8ESP1T1b
086570050707if  1c                   if        §A8ESP1T1c > *zeros
086580050707     c                   eval      PCclpf = §A8ESP1T1c
086590050216     c                                    + ' '
086600050707     c                                    + §A8ESP1T1d
086610050216     c                                    + ' '
086620050707     c                                    + §A8ESP1T1e
086630050216x   1c                   else
086640050216     c                   eval      PCclpf = '     '
086650050216     c                                    + ' '
086660050707     c                                    + §A8ESP1T1d
086670050216     c                                    + ' '
086680050707     c                                    + §A8ESP1T1e
086690050216e   1c                   endif
086700050214      * > destinatazione
086710050707     c                   eval      PCrsd  = §A8ESP1T1f
086720050707     c                   eval      PCind  = §A8ESP1T1g
086730050707if  1c                   if        §A8ESP1T1h > *zeros
086740050707     c                   eval      PCclpd = §A8ESP1T1h
086750050216     c                                    + ' '
086760050707     c                                    + §A8ESP1T1i
086770050216     c                                    + ' '
086780050707     c                                    + §A8ESP1T1j
086790050216x   1c                   else
086800050216     c                   eval      PCclpd = '     '
086810050216     c                                    + ' '
086820050707     c                                    + §A8ESP1T1i
086830050216     c                                    + ' '
086840050707     c                                    + §A8ESP1T1j
086850050216e   1c                   endif
086860050214      * > dati in testata DDT
086870050214      * - - codice cliente
086880050707     c                   eval      PCcli  = §A8ESP1T1k
086890050214      * - - codice fiscale
086900050707     c                   eval      PCcf   = §A8ESP1T1l
086910050214      * - - partita iva
086920050707     c                   eval      PCpi   = §A8ESP1T1m
086930050214      *
086940050214     c                   ENDSR
086950050214      *
086960050214      *---------------------------------------------------------------*
086970050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
086980050217      *?Dati di testata: documento, riferimenti.                     ?*
086990050214      *---------------------------------------------------------------*
087000050217     c     PrtDdtESP1_T2 BEGSR
087010110405      *
087020110405      * - pulizia records a nuova packing-list
087030110405     c                   if            %subst( SAVtrk : 1 : 1 ) = 'P'
087040110405     c                             or  %subst( SAVtrk : 1 : 1 ) = 'C'
087050110405     c                   exsr      sr_ClearESP1
087060110405     c                   endif
087070050214      *
087080051110     c                   movel(p)  AR8uni        dAR8ESP1T2
087090050214      *
087100050214      * > dati in testata DDT
087110050214      * - - tipo documento
087120050707     c                   eval      PCtdo  = §A8ESP1T2a
087130050214      * - - data e numero documento (DDT)
087140050707     c                   eval      PCdtd  = §A8ESP1T2b
087150050707     c                   eval      PCnrd  = §A8ESP1T2c
087160050214      * - - riferimenti mittente
087170050707     c                   eval      PCrif  = §A8ESP1T2d
087180050214      * - - condizioni di pagamento
087190050707     c                   eval      PCpag  = §A8ESP1T2e
087200050214      * - - annotazioni
087210050707     c                   eval      PCann  = §A8ESP1T2f
087220050214      * - - agente
087230050707     c                   eval      PCag2  = §A8ESP1T2g
087240050214      *
087250050214      * > Stampa testata DDT - - - - - - - - - - - - - - - - - - - - -*
087260050214      *
087270050214      *   - OverFlow impossibile qui! -
087280050215     c***                exsr      Test_OF
087290050215     c                   exsr      Posiz_ES
087300081017     c                   if            §AR8cxd = *blanks
087310081017     c                             or  DB0fa4  = 'P'
087320050331     c                   write     A4prtDT0
087330050331     c                   endif
087340050214     c                   write     A4prtES0
087350081002     c*** (NON qui:)     write     A4prtES0A
087360081002     c                   write     A4prtSP1
087370050215     c                   write     A4prtES1A
087380050215     c                   write     A4prtES1B
087390050214      *
087400050214     c                   ENDSR
087410050214      *
087420050214      *---------------------------------------------------------------*
087430050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
087440050217      *?Dati di dettaglio: articolo, descrizione, quantità, ...      ?*
087450050214      *---------------------------------------------------------------*
087460050217     c     PrtDdtESP1_C1 BEGSR
087470110405      *
087480110405      * - pulizia records a nuova packing-list
087490110405     c                   if        %subst( SAVtrk : 1 : 1 ) = 'P'
087500110405     c                   exsr      sr_ClearESP1
087510110405     c                   endif
087520050214      *
087530050214     c                   clear                   A4prtES2
087540051110     c                   movel(p)  AR8uni        dAR8ESP1C1
087550050214      *
087560050214      * > dati in dettaglio DDT
087570050214      * - - codice prodotto
087580050707     c                   eval      PCcpr  = §A8ESP1C1a
087590050214      * - - descrizione prodotto
087600050707     c                   eval      PCdpr  = §A8ESP1C1b
087610050214      * - - quantità
087620050707     c                   eval      PCqta  = '      ' + §A8ESP1C1c
087630050214      * - - prezzo unitario
087640050707     c                   eval      PCpru  = §A8ESP1C1d
087650050214      * - - cambio: divisa + valore
087660050707     c                   eval      PCdiv  = §A8ESP1C1e
087670050707     c                   eval      PCcmb  = §A8ESP1C1f
087680050214      * - - % sconto
087690050707     c                   eval      PCprc  = §A8ESP1C1g
087700050214      * - - importo
087710050707     c                   eval      PCimp  = §A8ESP1C1h
087720050214      * - - codice iva
087730050707     c                   eval      PCciv  = §A8ESP1C1i
087740050214      *
087750050214      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
087760050214      *
087770050214     c                   exsr      Test_OF
087780050214     c                   write     A4prtES2
087790050214      *
087800050214     c                   ENDSR
087810050214      *
087820050214      *---------------------------------------------------------------*
087830050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
087840050217      *?Dati di dettaglio: note (riga libera)                        ?*
087850050214      *---------------------------------------------------------------*
087860050217     c     PrtDdtESP1_C2 BEGSR
087870110405      *
087880110405      * - pulizia records a nuova packing-list
087890110405     c                   if        %subst( SAVtrk : 1 : 1 ) = 'P'
087900110405     c                   exsr      sr_ClearESP1
087910110405     c                   endif
087920050214      *
087930050215     c                   clear                   A4prtES3
087940051110     c                   movel(p)  AR8uni        dAR8ESP1C2
087950050214      *
087960050214      * > dati in note DDT
087970050214      * - - riga descrittiva
087980050707     c                   eval      PCrig  = §A8ESP1C2a
087990050214      *
088000050214      * > Stampa dettaglio DDT - - - - - - - - - - - - - - - - - - - -*
088010050214      *
088020050214     c                   exsr      Test_OF
088030050214     c                   write     A4prtES3
088040050214      *
088050050214     c                   ENDSR
088060050214      *
088070050214      *---------------------------------------------------------------*
088080050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
088090050217      *?Dati di fondo: totali documento                              ?*
088100050214      *---------------------------------------------------------------*
088110050217     c     PrtDdtESP1_P1 BEGSR
088120050214      *
088130081002      /free
088140081002           select;
088150081002             when  NOT *in45;
088160081002               wMaxLine = $MaxLine6;
088170081002             when  *in45   and   NOT *in46;
088180081002               wMaxLine = $MaxLine6_pl1;
088190081002             when  *in45   and   *in46;
088200081002               wMaxLine = $MaxLine6_pl2;
088210081002           endsl;
088220081002      /end-free
088230051110     c                   movel(p)  AR8uni        dAR8ESP1P1
088240050215      *
088250050215      * > dati in fondo DDT
088260050215      * - - netto merce
088270050707     c                   eval      PCinm  = §A8ESP1P1a
088280050215      * - - spese di trasporto
088290050707     c                   eval      PCist  = §A8ESP1P1b
088300050215      * - - spese imballo
088310050707     c                   eval      PCisi  = §A8ESP1P1c
088320050215      * - - spese bolli
088330050707     c                   eval      PCisb  = §A8ESP1P1d
088340050215      * - - spese amministrative
088350050707     c                   eval      PCisa  = §A8ESP1P1e
088360050215      * - - spese varie
088370050707     c                   eval      PCisv  = §A8ESP1P1f
088380050215      * - - codici iva
088390050707     c                   eval      PCic1  = §A8ESP1P1g
088400050707     c                   eval      PCic2  = §A8ESP1P1m
088410050707     c                   eval      PCic3  = §A8ESP1P1s
088420050215      * - - descrizioni iva
088430050707     c                   eval      PCid1  = §A8ESP1P1h
088440050707     c                   eval      PCid2  = §A8ESP1P1n
088450050707     c                   eval      PCid3  = §A8ESP1P1t
088460050215      * - - imponibili iva: divise
088470050707     c                   eval      PCie1  = §A8ESP1P1i
088480050707     c                   eval      PCie2  = §A8ESP1P1o
088490050707     c                   eval      PCie3  = §A8ESP1P1u
088500050215      * - - imponibili iva: importi
088510050707     c                   eval      PCii1  = §A8ESP1P1j
088520050707     c                   eval      PCii2  = §A8ESP1P1p
088530050707     c                   eval      PCii3  = §A8ESP1P1v
088540050215      * - - aliquote iva
088550050707     c                   eval      PCia1  = §A8ESP1P1k
088560050707     c                   eval      PCia2  = §A8ESP1P1q
088570050707     c                   eval      PCia3  = §A8ESP1P1w
088580050215      * - - imposte iva
088590050707     c                   eval      PCiv1  = §A8ESP1P1l
088600050707     c                   eval      PCiv2  = §A8ESP1P1r
088610050707     c                   eval      PCiv3  = §A8ESP1P1x
088620050215      * - - Totali documento: divise + importi
088630050707     c                   eval      PCtdd  = §A8ESP1P1y
088640050707     c                   eval      PCtdi  = §A8ESP1P1z
088650050707     c                   eval      PCtdd2 = §A8ESP1P11
088660050707     c                   eval      PCtdi2 = §A8ESP1P12
088670050214      *
088680050214     c                   ENDSR
088690050214      *
088700050214      *---------------------------------------------------------------*
088710050217      *?Impostazione campi nel DDT del mittente ESPRINET             ?*
088720050217      *?Dati di fondo: informazioni vettore                          ?*
088730050214      *---------------------------------------------------------------*
088740050217     c     PrtDdtESP1_P2 BEGSR
088750050214      *
088760081002      /free
088770081002           select;
088780081002             when  NOT *in45;
088790081002               wMaxLine = $MaxLine6;
088800081002             when  *in45   and   NOT *in46;
088810081002               wMaxLine = $MaxLine6_pl1;
088820081002             when  *in45   and   *in46;
088830081002               wMaxLine = $MaxLine6_pl2;
088840081002           endsl;
088850081002      /end-free
088860051110     c                   movel(p)  AR8uni        dAR8ESP1P2
088870050215      *
088880050215      * > dati in fondo DDT
088890050215      * - - resa
088900050707     c                   eval      PCres  = §A8ESP1P2a
088910050215      * - - trasporto a cura del
088920050707     c                   eval      PCtra  = §A8ESP1P2b
088930050215      * - - vettore
088940110426     c*//                eval      PCvet  = $Bartolini
088950110426     c*//                                 + ' - '
088960110426     c*//                                 + $Intest2
088970110426     c*//                                 + ' (BO)'
088980050707     c                   eval      PCvet2 = §A8ESP1P2c
088990050215      * - - causale trasporto
089000050707     c                   eval      PCctr2 = §A8ESP1P2d
089010050215      * - - aspetto esteriore dei beni
089020050707     c                   eval      PCaeb  = §A8ESP1P2e
089030050215      * - - numero colli
089040050707     c                   eval      PCncl2 = §A8ESP1P2f
089050050215      * - - peso complessivo
089060050707     c                   eval      PCpes  = §A8ESP1P2g
089070050215      * - - volume
089080050707     c                   eval      PCvol  = §A8ESP1P2h
089090050215      * - - inizio trasporto: data + ora
089100050707     c                   eval      PCdit  = §A8ESP1P2i
089110050707     c                   eval      PCoit  = §A8ESP1P2j
089120050215     *** *
089130050215     *** * > Stampa fondo DDT - - - - - - - - - - - - - - - - - - - - - -*
089140050215     *** * ?(impostata a fine ciclo di lettura - sr. DDT_Esprinet)?
089150050217      *
089160050217      * N.B.: la copia per il destinatario della LDV deve comparire
089170050217      *       nell'ultima pagina del DDT per il cliente (vedi subr.
089180050217      *       "ChiusuraLDV")
089190050214      *
089200050214     c                   ENDSR
089210081002      /free
089220081002       //--------------------------------------------------------------
089230110405       //?Impostazione nuova pagina di DDT Esprinet                    ?
089240081002       //--------------------------------------------------------------
089250081002       BEGSR  NewPageES;
089260081002
089270110405         // -?Impostazione del nuovo limite x overflow?
089280081002         select;
089290081002           when  NOT *in45;
089300081002             wMaxLine = $MaxLine2;
089310081002           when  *in45   and   NOT *in46;
089320090324             wMaxLine = $MaxLine5_pl1;
089330081002           when  *in45   and   *in46;
089340090324             wMaxLine = $MaxLine5_pl2;
089350081002         endsl;
089360081002
089370081002         write  A4prtSP1;
089380081002         write  A4prtES1B;
089390081002
089400081002       ENDSR;
089410110405
089420110405       //--------------------------------------------------------------
089430110405       //?Pulizia campi a nuova packing-list relativa alla stessa LdV  ?
089440110405       //--------------------------------------------------------------
089450110405       BEGSR  sr_ClearESP1;
089460110405
089470110405         // -?Stampa fondo DDT?
089480110405
089490110405         // - -?Calcolo la posizione finale del box e di linee?
089500110405         //    ?separatrici?
089510110405         exsr  Posiz_ES;
089520110405
089530110405         // - -?Stampa parte finale DDT?
089540110405         write  A4prtES4;
089550110405
089560110405         // - -?Stampa box, linee e testi fissi (no logo)?
089570110405         if  §AR8cxd = *blank  or  DB0fa4 = 'P';
089580110405           write  A4prtDT0;
089590110405         endif;
089600110405         write  A4prtES0A;
089610110405         write  A4prtES0B;
089620110405
089630110405         // -?Pulizia record?
089640110405         select;
089650110405           when  %subst( AR8trk : 1 : 1 ) = 'T';
089660110405             clear  A4prtES0;
089670110405             clear  A4prtES0A;
089680110405             clear  A4prtES0B;
089690110405             clear  A4prtES1A;
089700110405             clear  A4prtES1B;
089710110405             clear  A4prtES2;
089720110405             clear  A4prtES3;
089730110405             clear  A4prtES4;
089740110405           when  %subst( AR8trk : 1 : 1 ) = 'C';
089750110405             clear  A4prtES2;
089760110405             clear  A4prtES3;
089770110405             clear  A4prtES4;
089780110405         endsl;
089790110405
089800110405         // -?Impostazione testi fissi?
089810131017         //PCLtx3 = 'CONTRIBUTO AMBIENTALE CONAI ASSOLTO OVE DOVUTO';
089820131017         PCLtx3 = 'CONTRIBUTO AMBIENTALE CONAI ASSOLTO';
089830110405
089840110405         PCLpx2 = 'IL PRESENTE DOCUMENTO E'' REGOLATO DALLE +
089850110405                   CONDIZIONI GENERALI DI VENDITA PUBBLICATE SUL SITO +
089860110405                   INTERNET ALL''INDIRIZZO www.esprinet.com CHE SI +
089870110405                   INTENDONO ESPRESSAMENTE CONOSCIUTE ED ACCETTATE';
089880110405
089890110405         PCLpx3a = 'NB. IN CASO DI RITARDATO PAGAMENTO, VERRANNO +
089900110405                    ADDEBITATI';
089910110405         PCLpx3b = 'INTERESSI AL TASSO DEL PRIME RATE MAGGIORATO +
089920110405                    DEL 2% A';
089930110405         PCLpx3c =
089940110405                   'DECORRERE DAL GIORNO SUCCESSIVO ALLA SCADENZA +
089950110405                    DELLA FATTURA';
089960110405
089970110405         PCLpx4a = 'Si prega di verificare la Vs. ragione sociale +
089980110405                    indirizzo e';
089990110405         PCLpx4b = 'partita IVA, dandoci eventuale sollecita +
090000110405                    rettifica scritta.';
090010110405
090020110405         PCLpx5a = '* I PRODOTTI IMPORTATI DAGLI U.S.A. SONO CON +
090030110405                    SPECIALE';
090040110405         PCLpx5b = 'LICENZA CHE PRECLUDE RIESPORTAZIONI NON +
090050110405                    AUTORIZZATE.';
090060110405
090070110405         PCLpx6  = 'LA SOCIETÀ SI IMPEGNA A VERSARE L''IVA NEI MODI +
090080110405                    E TERMINI PREVISTI (SOLIDARIETÀ ART. 60 BIS DPR +
090090110405                    633/72-FINANZIARIA 2005)';
090100110405
090110110405         // -?Salto Pagina?
090120110405         exsr  Test_OF;
090130110405
090140110405         // -?Re-Impostazione limiti di overflow?
090150110405         select;
090160110405           when  NOT *in45;
090170110405             wMaxLine = $MaxLine2;
090180110405           when  *in45   and   NOT *in46;
090190110405             wMaxLine = $MaxLine5_pl1;
090200110405           when  *in45   and   *in46;
090210110405             wMaxLine = $MaxLine5_pl2;
090220110405         endsl;
090230110405
090240110405         // -?Posizionamento iniziale all'interno del foglio A4?
090250110405         write  A4prtSP1;
090260110405
090270110405       ENDSR;
090280081002      /end-free
090290050214      *
090300050214      *---------------------------------------------------------------*
090310050217      *?Posizionamento di linee e box nel DDT ESPRINET               ?*
090320050214      *---------------------------------------------------------------*
090330050215     c     Posiz_ES      BEGSR
090340080708      *
090350081002      /free
090360081002         // Impostazione posizioni di logo/box
090370081002         // (posizioni di "partenza" diverse a seconda del posizionam.
090380081002         //  iniziale nel foglio A4 attualmente in stampa)
090390081002         select;
090400081002           when  NOT *in45;
090410081006             §PBA =  3.130;
090420081006             //§PBA =  3.255;        (più in basso di una riga)
090430081002           when  *in45   and   NOT *in46;
090440081002             §PBA = 0.255;
090450081002           when  *in45   and   *in46;
090460081002             §PBA = 6.005;
090470081002         endsl;
090480081002      /end-free
090490080708      * Impostazione posizioni di linee orizzontali
090500080708     c                   eval      §LOA  =  §PBA + 0.76
090510080708     c                   eval      §LOB  =  §LOA + 0.25
090520080708     c                   eval      §LOC  =  §LOB + 0.12
090530080708      * Impostazione posizioni di linee verticali
090540080708     c                   eval      §LVA  =  §LOA - 0.15
090550050214      *
090560050214      * Impostazione posizioni di box
090570050215if  1c                   if        Page  <= 1
090580050214      *   (posizione di "partenza" su 1ª pagina)
090590081002     c                   eval      §PB1  =  §PBA + 1.270
090600050214x   1c                   else
090610050214      *   (posizione di "partenza" su pagine successive)
090620081002     c                   eval      §PB1  =  §PBA + 0.020
090630050214e   1c                   endif
090640110405if  1c*//                if        %subst(AR8trk:1:1) = 'P'
090650110405if  1c                   if        %subst(SAVtrk:1:1) = 'P'
090660050331     c                             and    CurLineA4   < wMaxLine
090670081002     c                   eval      §PB2  =  §PBA + 5.360
090680050215x   1c                   else
090690050215     c                   eval(h)   §PB2  =  CurLineA4 / 8 + 0.05
090700081002     c                   if        §PB2  <  (§PBA + 5.320)
090710081002     c                   eval      §PB2  =  (§PBA + 5.320)
090720050331     c                   endif
090730050215e   1c                   endif
090740050215      * Impostazione posizioni di linee (orizzontali e verticali)
090750050214      *   nella parte superiore del box (testata)
090760050215     c                   eval      §LO1  =  §PB1 + 0.25
090770050214     c                   eval      §LO2  =  §LO1 + 0.11
090780050214      *   nella parte inferiore del box (piede)
090790110405if  1c*//                if        %subst(AR8trk:1:1) = 'P'
090800110405if  1c                   if        %subst(SAVtrk:1:1) = 'P'
090810050331     c                             and    CurLineA4   < wMaxLine
0908200810027.78 c                   eval      §LO3  =  §PBA + 3.900
0908300502158.00 c                   eval      §LO4  =  §LO3 + 0.24
0908400502158.39 c                   eval      §LO5  =  §LO4 + 0.36
0908500502158.52 c                   eval      §LO6  =  §LO5 + 0.14
0908600502148.64 c                   eval      §LO7  =  §LO6 + 0.12
0908700502148.77 c                   eval      §LO8  =  §LO7 + 0.13
0908800502148.89 c                   eval      §LO9  =  §LO8 + 0.12
0908900807088.97 c                   eval      §LO0  =  §LO9 + 0.10
090900050215e   1c                   endif
090910050214      * La lunghezza delle linee verticali separatrici nel dettaglio
090920050214      *   dipende dalla fase di stampa: se si sta stampando ancora il
090930050214      *   dettaglio deve essere più lunga, visto che non viene stampa-
090940050214      *   ta la parte finale del DDT...
090950110405if  1c*//                if        %subst(AR8trk:1:1) = 'P'
090960110405if  1c                   if        %subst(SAVtrk:1:1) = 'P'
090970050331     c                             and    CurLineA4   < wMaxLine
0909800502162.25 c                   eval      §LV1  =  §LO3 - §LO2
090990050215x   1c                   else
091000050216     c                   eval      §LV1  =  §PB2 - §LO2
091010050215e   1c                   endif
091020050214      *
091030050214      * Impostazione posizioni di campi di testo
091040050214      * - PCLtx1/tx2 (verticali)
0910500502179.10 c                   eval      §PR1  =  §PB2 - 0.07
091060050214      * - PCLpx2 ("Il presente documento...")
0910700502178.95 c                   eval      §PS2  =  §LO9 + 0.07
091080050214      * - PCLpx3a/px4a/px5a
0910900807089.04 c                   eval      §PS3  =  §LO0 + 0.07
091100050214      * - PCLpx3b/px4b/px5b
0911100502179.09 c                   eval      §PS4  =  §PS3 + 0.07
091120050214      * - PCLpx3c/px4c/px5c
0911300502179.14 c                   eval      §PS5  =  §PS4 + 0.07
091140050214      * - PCLpx6 ("Contributo ambientale CONAI...")
0911500502179.25 c                   eval      §PS6  =  §PB2 + 0.08
091160050214      *
091170050214     c                   ENDSR
091180041207      *
091190041207      *---------------------------------------------------------------*
091200050217      *?Stampa DDT del mittente (generico con logo)                  ?*
091210041207      *---------------------------------------------------------------*
091220041207     c     DDT_GenericoL BEGSR
091230041207      *
091240081003      /free
091250081003         // Impostazione del nuovo limite x overflow
091260081003         select;
091270081003           when  NOT *in45;
091280081003             wMaxLine = $MaxLine2;
091290081003           when  *in45   and   NOT *in46;
091300081003             wMaxLine = $MaxLine2_pl1;
091310081003           when  *in45   and   *in46;
091320081003             wMaxLine = $MaxLine2_pl2;
091330081003         endsl;
091340081003
091350081003         // Posizionamento iniziale all'interno del foglio A4
091360081003         write  A4prtSP1;
091370081006
091380081006         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
091390081006           write  A4prtNUL;
091400081006         endif;
091410081003      /end-free
091420041207      *
091430041207      * > logo
091440041207     c                   eval      §Logo  = 'LOGO' + AR8tmd
091450041207      *
091460081017     c                   if            §AR8cxd = *blanks
091470081017     c                             or  DB0fa4  = 'P'
091480080709     c                   write     A4prtDT0
091490080709     c                   endif
091500080709      *
091510041209      * Imposto posizione per logo
091520081003      /free
091530081003         select;
091540081003           when  NOT *in45;
091550081003             exsr  Rep_§LO2;
091560081003             §PB1 = §LO2 + (2 / 8);
091570081003           when  *in45   and   NOT *in46;
091580081003             §PB1 = 0.250;
091590081003           when  *in45   and   *in46;
091600081003             §PB1 = 6.000;
091610081003         endsl;
091620081003      /end-free
091630041209      *   Stampa del logo
091640041209     c                   write     A4prtDB0
091650041207      *
091660041207      * Reperimento dati e stampa DDT mittente - - - - - - - - - - - -*
091670041207      *
091680130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
091690130315      *  "StampaDDT")
091700041207     c***  KLDV          setll     FIAR8000       (già fatto)
091710041207     c***  KLDV          reade     FIAR8000       (già fatto)
091720041207      *
091730041207if  1c                   dow       NOT %eof(FIAR801L)
091740041207      *
091750041207if  2c                   if        AR8atb = *blanks
091760041207      *
091770041207      * > Stampa dei dati
091780041207     c                   exsr      Test_OF
091790041207sel 3c                   select
091800041207      * - Dati di dettaglio: testo in font a dimensione standard
091810041207w   3c                   when      AR8trk = 'C1'
091820051110     c                   movel(p)  AR8uni        PCddt
091830041207     c                   write     A4prtDC1
091840041207      * - Dati di testata:   testo in font a dimensioni massime
091850041207w   3c                   when      AR8trk = 'T1'
091860051110     c                   movel(p)  AR8uni        PCddt
091870041207     c                   write     A4prtDT1
091880041207      * - Dati di testata:   testo in font a dimensioni grandi
091890041207w   3c                   when      AR8trk = 'T2'
091900051110     c                   movel(p)  AR8uni        PCddt
091910041207     c                   write     A4prtDT2
091920041209      * - Linea separatrice
091930041209w   3c                   when      AR8trk = '--'
091940041209      *   - Stampa di una riga vuota per la riga separatrice
091950041209     c                   write     A4prtNUL
091960041209      *   - Stampa della linea orizzontale separatrice
091970090213     c                   eval(h)   §LO1   = (CurLineA4 / 8) - 0.050
091980041209     c                   write     A4prtLO
091990041207e   3c                   endsl
092000041207e   2c                   endif
092010041207      *
092020041207     c                   eval      SAVtrk = AR8trk
092030041207     c     KLDV          reade     FIAR8000
092040041207      *
092050041207e   1c                   enddo
092060080708      *
092070081003      /free
092080081003         select;
092090081003           when  NOT *in45;
092100081003             wMaxLine = $MaxLine2;
092110081003           when  *in45   and   NOT *in46;
092120081003             wMaxLine = $MaxLine2_pl1;
092130081003           when  *in45   and   *in46;
092140081003             wMaxLine = $MaxLine2_pl2;
092150081003         endsl;
092160081003      /end-free
092170041207      *
092180041207     c                   ENDSR
092190040607      *
092200040607      *---------------------------------------------------------------*
092210050217      *?Stampa DDT del mittente (generico senza logo)                ?*
092220040607      *---------------------------------------------------------------*
092230040607     c     DDT_Generico  BEGSR
092240040607      *
092250081003      /free
092260081003         // Impostazione del nuovo limite x overflow
092270081003         select;
092280081003           when  NOT *in45;
092290081003             wMaxLine = $MaxLine2;
092300081003           when  *in45   and   NOT *in46;
092310081003             wMaxLine = $MaxLine2_pl1;
092320081003           when  *in45   and   *in46;
092330081003             wMaxLine = $MaxLine2_pl2;
092340081003         endsl;
092350081003
092360081003         // Posizionamento iniziale all'interno del foglio A4
092370081003         write  A4prtSP1;
092380081006
092390081006         if  DB0fa4 = '1'   or   DB0fa4 = 'S';
092400081006           write  A4prtNUL;
092410081006         endif;
092420081003      /end-free
092430040607      *
092440081017     c                   if            §AR8cxd = *blanks
092450081017     c                             or  DB0fa4  = 'P'
092460050331     c                   write     A4prtDT0
092470050331     c                   endif
092480040607      *
092490040607      * Reperimento dati e stampa DDT mittente - - - - - - - - - - - -*
092500040607      *
092510130315      * (Posizionamento e 1ª lettura già effettuati nella subr.
092520130315      *  "StampaDDT")
092530040607     c***  KLDV          setll     FIAR8000       (già fatto)
092540040607     c***  KLDV          reade     FIAR8000       (già fatto)
092550040607      *
092560040607if  1c                   dow       NOT %eof(FIAR801L)
092570040607      *
092580040607if  2c                   if        AR8atb = *blanks
092590051110     c                   movel(p)  AR8uni        PCddt
092600040607     c                   exsr      Test_OF
092610040607sel 3c                   select
092620040607      * - Dati di dettaglio: testo in font a dimensione standard
092630040607w   3c                   when      AR8trk = 'C1'
092640040607     c                   write     A4prtDC1
092650040607      * - Dati di testata:   testo in font a dimensioni massime
092660040607w   3c                   when      AR8trk = 'T1'
092670040607     c                   write     A4prtDT1
092680040607      * - Dati di testata:   testo in font a dimensioni grandi
092690040607w   3c                   when      AR8trk = 'T2'
092700040607     c                   write     A4prtDT2
092710040607e   3c                   endsl
092720040607e   2c                   endif
092730040607      *
092740040607     c     KLDV          reade     FIAR8000
092750040607      *
092760040607e   1c                   enddo
092770080708      *
092780081003      /free
092790081003         select;
092800081003           when  NOT *in45;
092810081003             wMaxLine = $MaxLine2;
092820081003           when  *in45   and   NOT *in46;
092830081003             wMaxLine = $MaxLine2_pl1;
092840081003           when  *in45   and   *in46;
092850081003             wMaxLine = $MaxLine2_pl2;
092860081003         endsl;
092870081003      /end-free
092880040607      *
092890040607     c                   ENDSR
092900081017
092910081017      /free
092920081017       //--------------------------------------------------------------
092930140303       //?Impostazione nuova pagina di DDT Generico (con/senza logo)   ?
092940081017       //--------------------------------------------------------------
092950081017       BEGSR  NewPageSTD;
092960081017
092970081017         // Posizionamento iniziale nella nuova pagina
092980081017         //   (già fatto con A4prtSP0 se DB0FA4 = "P")
092990081017         if  DB0fa4 <> 'P';
093000081017           write  A4prtSP1;
093010081017           if  DB0fa4 = '1'   or   DB0fa4 = 'S';
093020081017             write  A4prtNUL;
093030081017           endif;
093040081017         endif;
093050081017
093060081017         // Stampa "COPIA PER IL DESTINATARIO"
093070081017         if  §AR8cxd = *blank   or   DB0fa4 = 'P';
093080081017           write  A4prtDT0;
093090081017         endif;
093100081017
093110081017         // Impostazione del nuovo limite x overflow
093120081017         select;
093130081017           when  NOT *in45;
093140081017             wMaxLine = $MaxLine2;
093150081017           when  *in45   and   NOT *in46;
093160081017             wMaxLine = $MaxLine2_pl1;
093170081017           when  *in45   and   *in46;
093180081017             wMaxLine = $MaxLine2_pl2;
093190081017         endsl;
093200081017
093210081017       ENDSR;
093220081017      /end-free
093230030515      *
093240030515      *---------------------------------------------------------------*
093250050217      *?Stampa del record "generico" (PRTGEN)                        ?*
093260030515      *---------------------------------------------------------------*
093270030515     c     StampaGEN     BEGSR
093280030515      *
093290030515      * Controllo se è stato raggiunto l'overflow nel formato in uso
093300030515     c                   exsr      Test_OF
093310030515      *
093320030911      * Stampo la riga PRTGEN
093330030911      * Se sto stampando una "testata" (*in39) salto una riga prima
093340030911      *   ed una riga dopo (per spaziare)
093350030911if  1c                   if        *in39 = *on
093360030515     c                   write     A4prtNUL
093370030911e   1c                   endif
093380030911     c                   write     A4prtGEN
093390030911if  1c                   if        *in39 = *on
093400030911     c                   write     A4prtNUL
093410030911e   1c                   endif
093420030515      *
093430030515     c                   ENDSR
093440030515      *
093450030515      *---------------------------------------------------------------*
093460050511      *?Prepara elenco per lista - SegnaColli Non Sequenziali (test.)?*
093470030515      *---------------------------------------------------------------*
093480030515     c     ListaScNS     BEGSR
093490030515      *
093500030515     c                   clear                   XX
093510030515      *
093520030515      * Nel foglio in formato "A5" devo solo riempire gli appositi
093530030715      *   campi "Note e Particolarità" (nessun salto riga/pagina è
093540030715      *   richiesto)
093550050513     c                   if           *in69       = *on
093560050513     c                             or $A4_x_ScNS  = *off
093570030515     c                   eval      WelSNS = $elSNSxA5
093580030515     c                   goto      ScNS_A5
093590030515     c                   endif
093600030515      *
093610030515      * Determino il numero di segnacolli da elencare per riga nel
093620030515      *   formato pagina "A4" (dove ci sono righe a parte).
093630030515      *   Dipende dal carattere specificato nel prtf: più grande è
093640030515      *   il carattere => meno segnacolli ci stanno sulla riga !
093650030911      *   A cpi 15 -come ora- ci sta tutta la schiera; a cpi 12 no !
093660030515     c                   eval      WelSNS = %elem($SC1)
093670030515      *
093680030515      * Spengo gli indicatori che condizionano in stampa le indicazioni
093690030515      *   per DPD / FedEx / Bancali-Supermercati-Autotrasportatori
093700030515      *   (se sono già state stampate nella 1ª pagina non devono più
093710030515      *   esserlo)
093720030910      *   (se non sono ancora state stampate, dovranno esserlo poi)
093730030515if  1c                   if        wPage > 1
093740050513     c                   movea     *in(70)       SAVin7x
093750040223     c                   movea     '0000'        *in(70)
093760030515e   1c                   endif
093770030515      *
093780030515      * Calcolo il numero di righe da saltare
093790030515     c                   eval      wMinLine = CurLineA4
093800030515if  1c                   if        §LO2     > §LO1
093810030515      * - se stampato almeno un box
093820030515     c                   eval(h)   wMinLine = (§LO2 * 8) + 2
093830030515x   1c                   else
093840030515      * - se non stampato alcun box
093850080730      /free
093860080729       // - Calcola la posizione della riga in 1/8" di pollice
093870080729       //   da quella attuale (in 1/9" di pollice)
093880080730         eval(h) wMinLine = (CurLineA4 * 8 / 9) + 1;
093890080730      /end-free
093900030515e   1c                   endif
093910030515      * ...e scarto il numero righe necessario
093920030515      *   (se non sono già da pagina nuova;
093930030515      *    se sono da pagina nuova: ci penserà poi la subr. "Test_OF"
093940030515      *       alla prima riga in stampa)
093950030515if  1c                   if        wMinLine  < wMaxLine
093960080729do  2c                   dou       CurLineA4 >= wMinLine
093970030515     c                   write     A4prtNUL
093980030515e   2c                   enddo
093990030515e   1c                   endif
094000030515      *
094010030515     c     ScNS_A5       tag
094020030515      *
094030030515      * Imposto l'indicatore per la stampa del titolo relativo all'
094040030515      *   elenco che segue (condiziona attributi di stampa diversi)
094050030515     c                   eval      *in39  = *on
094060050511      *
094070050511     c                   exsr      ListaScNSd
094080050511      *
094090050511     c                   ENDSR
094100050511      *
094110050511      *---------------------------------------------------------------*
094120050511      *?Prepara elenco per lista - SegnaColli Non Sequenziali (dett.)?*
094130050511      *---------------------------------------------------------------*
094140050511     c     ListaScNSd    BEGSR
094150030515      *
094160030515      * Stampo elenco dei segnacolli non sequenziali:
094170071008if  1c                   IF             DB0tbo  = 'P'
094180071008     c                             or   DB0tbo  = '8'
094190030515      * - per bolle in Partenza
094200030515     c                   exsr      ScNS_BLT
094210030515x   1c                   ELSE
094220081007      * - per bolle in Arrivo
094230030515     c                   exsr      ScNS_ART
094240030515e   1c                   ENDIF
094250030515      *
094260030515      * Alla fine: stampo eventuali segnacolli in elenco (non stampati)
094270030515      *              in foglio di formato "A4"
094280030515      *            imposto eventuali segnacolli in elenco (non messi in
094290030515      *              stampa tra le particolarità) in foglio "A5"
094300030515if  1c                   if        XX     > *zeros
094310030515     c                   eval      XX     = *hival
094320030515     c                   exsr      StampaScNS
094330030515e   1c                   endif
094340030910      *
094350030910      * Se sto stampando su formato pagina "A4": reimposto *in70, *in71
094360030910      *   e *in72 (spenti prima della stampa dell'elenco dei segnacolli
094370030910      *   non sequenziali) perchè saranno utilizzati per il posiziona-
094380030910      *   mento in nuova pagina (nella subr. "Test_OF") e per la stampa
094390040223      *   dei box per DPD, FedEx e quelli da FIAR5.
094400050513if  1c                   if             not *in69
094410030910     c                             and  not *in70
094420080922     c*** DPD non più in A4:       and  not *in71
094430030910     c                             and  not *in72
094440080912     c*** EEX non più gestita:     and  not *in73
094450030910     c                   movea     SAVin7x       *in(70)
094460030910e   1c                   endif
094470030515      *
094480030515     c                   ENDSR
094490030515      *
094500030515      *---------------------------------------------------------------*
094510050217      *?Prepara elenco SegnaColli Non Sequenziali da FNBLT (partenza)?*
094520030515      *---------------------------------------------------------------*
094530030515     c     ScNS_BLT      BEGSR
094540030515      *
094550030515if  1c                   if        not %open(FNBLT01L)
094560030515     c                   open      FNBLT01L
094570030515e   1c                   endif
094580030515      *
094590030515      * memorizzo l'ultimo numero di segnacollo
094600070514if  1c                   if        $Mamma = *off
094610040607     c     KLDV          setgt     FNBLT000
094620040607     c     KLDV          readpe    FNBLT000
094630070514x   1c                   else
094640070514     c     KLBL          setgt     FNBLT000
094650070514     c     KLBL          readpe    FNBLT000
094660070514e   1c                   endif
094670030515if  1c                   if        NOT %eof(FNBLT01L)
094680080922     c                   eval      %subst(Pnsx : 8) = ','
094690080922     c                                              + %editc(BLTnsc : 'Z')
094700030515e   1c                   endif
094710030515      *
094720030515      * memorizzo il primo numero di segnacollo
094730070514if  1c                   if        $Mamma = *off
094740040607     c     KLDV          setll     FNBLT000
094750040607     c     KLDV          reade     FNBLT000
094760070514x   1c                   else
094770070514     c     KLBL          setll     FNBLT000
094780070514     c     KLBL          reade     FNBLT000
094790070514e   1c                   endif
094800030515if  1c                   if        NOT %eof(FNBLT01L)
094810080922     c                   eval      %subst(Pnsx : 1 : 7) =
094820080922     c                                 %editc(BLTnsc : 'Z')
094830030515e   1c                   endif
094840030515      *
094850030515      * se ci sono solo 2 segnacolli non sequenziali:
094860030515      *   stanno già al posto del range - basta non stampare il msg
094870030515      *   ("NON SEQUENZIALI")
094880030515if  1c                   if        DB0ncl <= 2
094890050516     c                   eval      *in39  =  *off
094900030515     c                   goto      EndScNS_BLT
094910030515e   1c                   endif
094920030515      *
094930030515      * ciclo di lettura per il recupero di tutti i numeri di
094940030515      *   segnacollo
094950030515do  1c                   DOW       NOT %eof(FNBLT01L)
094960030515     c                   eval      Wnsc   =  BLTnsc
094970030515     c                   exsr      StampaScNS
094980070514if  2c                   if        $Mamma = *off
094990040607     c     KLDV          reade     FNBLT000
095000070514x   2c                   else
095010070514     c     KLBL          reade     FNBLT000
095020070514e   2c                   endif
095030030515e   1c                   ENDDO
095040030515      *
095050030515     c     EndScNS_BLT   ENDSR
095060030515      *
095070030515      *---------------------------------------------------------------*
095080050217      *?Prepara elenco SegnaColli Non Sequenziali da FNART (arrivo)  ?*
095090030515      *---------------------------------------------------------------*
095100030515     c     ScNS_ART      BEGSR
095110030515      *
095120070319if  1c                   if        not %open(FNART01L)
095130070319     c                   open      FNART01L
095140070319e   1c                   endif
095150030515      *
095160030515      * memorizzo l'ultimo numero di segnacollo
095170070514if  1c                   if        $Mamma = *off
095180040607     c     KLDV          setgt     FNART000
095190040607     c     KLDV          readpe    FNART000
095200070514x   1c                   else
095210070514     c     KLBL          setgt     FNART000
095220070514     c     KLBL          readpe    FNART000
095230070514e   1c                   endif
095240030515if  1c                   if        NOT %eof(FNART01L)
095250080922     c                   eval      %subst(Pnsx : 8) = ','
095260080922     c                                              + %editc(ARTnsc : 'Z')
095270030515e   1c                   endif
095280030515      *
095290030515      * memorizzo il primo numero di segnacollo
095300070514if  1c                   if        $Mamma = *off
095310040607     c     KLDV          setll     FNART000
095320040607     c     KLDV          reade     FNART000
095330070514x   1c                   else
095340070514     c     KLBL          setll     FNART000
095350070514     c     KLBL          reade     FNART000
095360070514e   1c                   endif
095370030515if  1c                   if        NOT %eof(FNART01L)
095380080922     c                   eval      %subst(Pnsx : 1 : 7) =
095390080922     c                                 %editc(ARTnsc : 'Z')
095400030515e   1c                   endif
095410030515      *
095420030515      * se ci sono solo 2 segnacolli non sequenziali:
095430030515      *   stanno già al posto del range - basta non stampare il msg
095440030515      *   ("NON SEQUENZIALI")
095450030515if  1c                   if        DB0ncl <= 2
095460050516     c                   eval      *in39  =  *off
095470030515     c                   goto      EndScNS_ART
095480030515e   1c                   endif
095490030515      *
095500030515      * ciclo di lettura per il recupero di tutti i numeri di
095510030515      *   segnacollo
095520030515do  1c                   DOW       NOT %eof(FNART01L)
095530030515     c                   eval      Wnsc   =  ARTnsc
095540030515     c                   exsr      StampaScNS
095550070514if  2c                   if        $Mamma = *off
095560040607     c     KLDV          reade     FNART000
095570070514x   2c                   else
095580070514     c     KLBL          reade     FNART000
095590070514e   2c                   endif
095600030515e   1c                   ENDDO
095610030515      *
095620030515     c     EndScNS_ART   ENDSR
095630030515      *
095640030515      *---------------------------------------------------------------*
095650050217      *?Stampa elenco SegnaColli Non Sequenziali in fogli "A4" o "A5"?*
095660030515      *---------------------------------------------------------------*
095670030515     c     StampaScNS    BEGSR
095680030515      *
095690030515      * In prima riga stampo la "intestazione"
095700030515if  1c                   if        *in39
095710050513if  2c                   if        *in69   or
095720050513     c                             $A4_x_ScNS = *off
095730050513      * - foglio "A5" oppure
095740050513      *   foglio "A4" in "NOTE E PARTICOLARITA'"
095750030515     c                   eval      YY      = YY  + 1
095760030515     c                   eval      $NP(yy) = C_ScNS + ':'
095770030515x   2c                   else
095780030515      * - foglio "A4"
095790030515     c                   eval      Prig  = C_ScNS
095800030515     c                                   + '         (dal '
095810080922     c                                   + %trim( %subst(Pnsx: 1: 7) )
095820030515     c                                   + ' al '
095830080922     c                                   + %trim( %subst(Pnsx: 9: 7) )
095840030515     c                                   + ')'
095850030515     c                   exsr      StampaGEN
095860030515e   2c                   endif
095870080922     c                   eval      Pnsx  =  'NON SEQUENZIALI'
095880030515     c                   eval      *in39 = *off
095890030515     c                   clear                   XX
095900030515     c                   clear                   $SC1
095910030515     c                   clear                   $SC2
095920030515e   1c                   endif
095930030515      *
095940030515      * Al riempimento della riga: stampo la parte di elenco segnacolli
095950030515if  1c                   if        XX  >= WelSNS
095960030515     c                   eval      $SC2 = %editc($SC1 : 'Z')
095970050513if  2c                   if        *in69   or
095980050513     c                             $A4_x_ScNS = *off
095990050513      * - foglio "A5"  oppure
096000050513      *   foglio "A4" in "NOTE E PARTICOLARITA'"
096010030515     c                   eval      YY      = YY  + 1
096020030515     c                   movea(p)  $SC2          $NP(yy)
096030030515x   2c                   else
096040030515      * - foglio "A4"
096050030911     c                   movea(p)  $SC2          Prig
096060030515     c                   exsr      StampaGEN
096070030515e   2c                   endif
096080030515if  2c                   if        XX  <  *hival
096090030515     c                   clear                   XX
096100030515e   2c                   endif
096110030515     c                   clear                   $SC1
096120030515     c                   clear                   $SC2
096130030515e   1c                   endif
096140030515      *
096150030515      * Aggiorno la nuova parte di elenco segnacolli
096160030515      *  (se non richiesta la stampa degli ultimi segnacolli non
096170030515      *   sequenziali residui - memorizzati ma non ancora stampati)
096180030515if  1c                   if        XX  <  *hival
096190030515     c                   eval      XX = xx  + 1
096200030515     c                   eval      $SC1(xx) = Wnsc
096210030515e   1c                   endif
096220030515      *
096230030515     c                   ENDSR
096240040607      *
096250040607      *---------------------------------------------------------------*
096260050217      *?Stampa della parte finale della Lettera di Vettura           ?*
096270040607      *---------------------------------------------------------------*
096280040607     c     ChiusuraLDV   BEGSR
096290080605      *
096300080605      * Posizionamento sul foglio A4 in base al n° pagina in stampa
096310130418     c                   eval      *in33 = *on
096320130418     c                   exsr      Posiz_Box
096330070307      *
096340070307      * Il tagliandino "Copia per il Destinatario" va ora stampato SE:
096350070307      * · spedizione NON in stato "lasciato avviso"   oppure
096360070307      * · spedizione in stato "lasciato avviso", ma P.A. con fattura
096370070307      *   immediata
096380040607      *
096390080703      * 2ª parte - copia per il destinatario (da lasciare al cliente)
096400040607      *   (Se NON è ristampa in arrivo per recapito c/assegni - vedi
096410040607      *   costante "Non utilizzare per la consegna")
096420130418      *                o per il mittente - se Porto Franco Prepagato
096430040607      * > "cornice" / barcode / righine varie in parte inferiore
096440040607      *
096450080604sel 1c                   select
096460080604w   1c                   when           $No_x_Cons         =  *off
096470070307     c                             and ($LascAvv           =  *off
096480070406     c                              or (%subst(DB0tb1:1:1) =  'A'
096490070406     c                              and DB0nft             >  *zeros))
096500130418     c                   write     A4prtLDV
096510130418     c                   write     A4prtBOX
096520110426     c*//                write     A4LogoBAR
096530110426      /free
096540120530         //if  (DB0dft > *zero   and   DB0dft > §vpoBRTdta)  OR
096550120530         //    (DB0dft = *zero   and   Wdsp   > §vpoBRTdta);
096560120530         //  // >?Simil-Logo Bartolini/Brt?
096570120530         //  write  A4LogoBR2;
096580120530         //  write  A4LogoDA2;
096590120530         //else;
096600120530         //  // >?Simil-Logo Bartolini?
096610120530         //  write  A4LogoBAR;
096620120530         //  write  A4LogoDA1;
096630120530         //endif;
096640120530         // >?Simil-Logo BRT?
096650120530         write  A4LogoBRT;
096660120530         write  A4LogoDA3;
096670110426      /end-free
096680080604w   1c                   when           $LascAvv           =  *on
096690070406     c                             and (%subst(DB0tb1:1:1) <> 'A'
096700070406     c                              or  DB0nft             =  *zeros)
096710070307     c                   eval      Pmsg3  =  Tla
096720070307     c                   write     A4prtLDVB
096730120113w   1c                   when      $No_x_Cons =  *on
096740120113w   1c*//                   when      $No_x_Cons =  '2'
096750120113     c*//                   eval      Pmsg3  =  ' NON UTILIZZARE PER'
096760120113     c*//                                    +  ' LASCIATO AVVISO -'
096770120113     c*//                                    +  ' SERVIZIO POSTE'
096780120113     c*//                   write     A4prtLDVB
096790080604e   1c                   endsl
096800040607      *
096810040607     c                   ENDSR
096820030515      *
096830030515      *---------------------------------------------------------------*
096840050217      *?Carico LDV da FNBLP00F                                       ?*
096850030515      *---------------------------------------------------------------*
096860030515     c     CarBLP        BEGSR
096870030515      *
096880030515      * Apertura files
096890030515     c                   if        not %open(FNBLP01L)
096900030515     c                   open      FNBLP01L
096910030515     c                   endif
096920030515     c                   if        not %open(FNARB01L)
096930030515     c                   open      FNARB01L
096940030515     c                   endif
096950070319     ***c v.sr sr_Start     if        not %open(FIAR401L)
096960070319     ***c " "     "         open      FIAR401L
096970070319     ***c " "     "         endif
096980030515     c                   if        not %open(FIAR601L)
096990030515     c                   open      FIAR601L
097000030515     c                   endif
097010030515     c                   if        not %open(FIAR701L)
097020030515     c                   open      FIAR701L
097030030515     c                   endif
097040060505     c                   if        not %open(FIAR901L)
097050060505     c                   open      FIAR901L
097060030515     c                   endif
097070071114      *
097080071114     c                   clear                   DB0cpi
097090030515      *
097100040607     c     KLDV          chain     FNBLP000
097110030515      *
097120030515if  1c                   if        NOT %found(FNBLP01L)
097130081117     c                   leavesr
097140030515e   1c                   endif
097150120509      *
097160120509      * Memorizzazione dei dati di FNBLP che serviranno anche dopo
097170120509      *   il reperimento dell'eventuale bolla mamma...
097180120509     c                   eval      wB0piv =  BLPcpi
097190030515      *
097200030515      * 1ª impostaz. campi della DS in base a quelli del file FNBLP
097210030515     c                   exsr      sr_DS_BLP
097220030515      *
097230070308      * Decodifico filiale di Arrivo
097240030515     c                   exsr      DecLNA
097250030515      *
097260030515      * 2ª impostaz. campi della DS in base a quelli delle tabelle
097270030515      *    "generiche" (3A, TB, 5E)
097280030515     c                   exsr      sr_DS_TAB
097290030515      *
097300030515      * Ultimi dati mancanti:
097310061030      * - Cod. cliente destinatario: imposto LNA9999
097320030515      *    SE   in Assegnato   O   c'è 2ª bolla
097330030515if  1c                   if           %subst(DB0tb1:1:1) = 'A'
097340030515     c                             or DB0tb2            <> *blanks
097350030515     c                   eval      DB0ccd = *all'9'
097360030515     c                   movel     BLPlna        DB0ccd
097370030515e   1c                   endif
097380120509      *
097390120509      * - Reperimento Data spedizione
097400120509if  1c                   if        DB0mgs <> *zeros
097410120509     c                   z-add     DB0mgs        Wdsp
097420120509     c                   movel     DB0aas        Wdsp
097430120509x   1c                   else
097440120509      *   prendo la data del giorno se non c'è la data spedizione
097450120509     c                   time                    w0140
097460120509     c                   move      w0140         w0080
097470120509     c     *eur          movel     w0080         Data_EUR
097480120509     c     *iso          movel     Data_EUR      Wdsp
097490120509e   1c                   endif
097500030515      *
097510061030      * 3ª impostaz. campi della DS: importi da calcolare
097520061031     c                   exsr      sr_DS_IMPO
097530030515      *
097540081117     c                   ENDSR
097550030515      *
097560030515      *---------------------------------------------------------------*
097570050217      *?Carico LDV da FNARB00F                                       ?*
097580030515      *---------------------------------------------------------------*
097590030515     c     CarARB        BEGSR
097600030515      *
097610030515      * Apertura files
097620030515     c                   if        not %open(FNARB01L)
097630030515     c                   open      FNARB01L
097640030515     c                   endif
097650070319     c                   if        NOT %open(FIARN11L)
097660070319     c                   open      FIARN11L
097670070319     c                   endif
097680030515     c                   if        not %open(FIAR601L)
097690030515     c                   open      FIAR601L
097700030515     c                   endif
097710030515     c                   if        not %open(FIAR701L)
097720030515     c                   open      FIAR701L
097730030515     c                   endif
097740060505     c                   if        not %open(FIAR901L)
097750060505     c                   open      FIAR901L
097760030515     c                   endif
097770071114      *
097780071114     c                   clear                   DB0cpi
097790030515      *
097800040607     c     KLDV          chain     FNARB01L
097810030515      *
097820030515if  1c                   if        NOT %found(FNARB01L)
097830070312     c                   clear                   ARBndc
097840070322     c                   clear                   ARBntc
097850070322     c                   clear                   ARBdcp
097860071015     c                   clear                   ARBpdc
097870101129     c                   clear                   ARBdcm
097880101129     c                   clear                   ARBicc
097890101129     c                   clear                   ARBica
097900120509     c                   clear                   ARBcpi
097910081117     c                   leavesr
097920030515e   1c                   endif
097930070514      *
097940070514      * Memorizzazione dei dati di FNARB che serviranno anche dopo
097950070514      *   il reperimento dell'eventuale bolla mamma...
097960070514     c                   eval      wB0ndc =  ARBndc
097970070514     c                   eval      wB0ntc =  ARBntc
097980070514     c                   eval      wB0dcp =  ARBdcp
097990081119     c                   eval      wB0tmc =  ARBtmc
098000071015     c                   move      ARBpdc        Wpdc4a
098010101125     c                   eval      wB0dcm =  ARBdcm
098020101125     c                   eval      wB0icc =  ARBicc
098030101125     c                   eval      wB0ica =  ARBica
098040120509     c                   eval      wB0piv =  ARBcpi
098050030515      *
098060030515      * 1ª impostaz. campi della DS in base a quelli del file FNARB
098070030515     c                   exsr      sr_DS_ARB
098080030515      *
098090070308      * Decodifico filiale di Arrivo
098100030515     c                   exsr      DecLNA
098110030515      *
098120030515      * 2ª impostaz. campi della DS in base a quelli delle tabelle
098130030515      *    "generiche" (3A, TB, 5E)
098140030515     c                   exsr      sr_DS_TAB
098150030515      *
098160030515      * Ultimi dati mancanti:
098170030515      * - Cod. cliente mittente
098180030515if  1c                   If        ARBccm <> *zeros
098190030515     c                   movel     ARBccm        DB0ccm
098200030515x   1c                   Else
098210030515if  2c                   if        %subst(DB0tb1:1:1) = 'F'
098220030515     c                   movel     ARBksc        DB0ccm
098230030515e   2c                   endif
098240030515e   1c                   EndIf
098250030515      * - Cod. cliente destinatario
098260030515if  2c                   if        %subst(DB0tb1:1:1) = 'A'
098270030515     c                   movel     ARBksc        DB0ccd
098280030515e   1c                   endif
098290030515      * - Se c'è 2ª bolla imposto LNA9999
098300030515if  1c                   if        DB0tb2 <> *blanks
098310030515     c                   eval      DB0ccd = *all'9'
098320030515     c                   movel     ARBlna        DB0ccd
098330030515e   1c                   endif
098340120509      * - Reperimento Data spedizione
098350120509if  1c                   if        DB0mgs <> *zeros
098360120509     c                   z-add     DB0mgs        Wdsp
098370120509     c                   movel     DB0aas        Wdsp
098380120509x   1c                   else
098390120509      *   prendo la data del giorno se non c'è la data spedizione
098400120509     c                   time                    w0140
098410120509     c                   move      w0140         w0080
098420120509     c     *eur          movel     w0080         Data_EUR
098430120509     c     *iso          movel     Data_EUR      Wdsp
098440120509e   1c                   endif
098450030515      *
098460061030      * 3ª impostaz. campi della DS: importi da calcolare
098470061031     c                   exsr      sr_DS_IMPO
098480030515      *
098490081117     c                   ENDSR
098500030515      *
098510030515      *---------------------------------------------------------------*
098520050217      *?1ª impostaz. campi della DS in base a quelli del file FNBLP  ?*
098530030515      *---------------------------------------------------------------*
098540030515     c     sr_DS_BLP     BEGSR
098550030515      *
098560030515     c                   eval      DB0cbo = BLPcbo
098570030515     c                   eval      DB0tsp = BLPtsp
098580030515     c                   eval      DB0mgs = BLPmgs
098590030515     c                   eval      DB0tc1 = BLPtc1
098600030515     c                   eval      DB0tc2 = BLPtc2
098610030515     c                   eval      DB0fns = BLPfns
098620030515     c                   eval      DB0rmn = BLPrmn
098630030515     c                   eval      DB0rma = BLPrma
098640030515     c                   eval      DB0ias = BLPias
098650030515     c                   eval      DB0vas = BLPvas
098660030515     c                   eval      DB0vmd = BLPvmd
098670030515     c                   eval      DB0vad = BLPvad
098680030515     c                   eval      DB0qft = BLPqft
098690030515     c                   eval      DB0lna = BLPlna
098700030515     c                   eval      DB0znc = BLPznc
098710030515     c                   eval      DB0rsm = BLPrsm
098720030515     c                   eval      DB0inm = BLPinm
098730030515     c                   eval      DB0cam = BLPcam
098740030515     c                   eval      DB0lom = BLPlom
098750030515     c                   eval      DB0prm = BLPprm
098760030515     c                   eval      DB0nzm = BLPnzm
098770030515     c                   eval      DB0fap = BLPfap
098780030515     c                   eval      DB0rmo = BLPrmo
098790030515     c                   eval      DB0rsd = BLPrsd
098800030515     c                   eval      DB0ind = BLPind
098810030515     c                   eval      DB0cad = BLPcad
098820030515     c                   eval      DB0lod = BLPlod
098830030515     c                   eval      DB0prd = BLPprd
098840030515     c                   eval      DB0nzd = BLPnzd
098850030515     c                   eval      DB0fin = BLPfin
098860030515     c                   eval      DB0gc1 = BLPgc1
098870030515     c                   eval      DB0gc2 = BLPgc2
098880030515     c                   eval      DB0nas = BLPnas
098890030515     c                   eval      DB0ncl = BLPncl
098900030515     c                   eval      DB0pkf = BLPpkf
098910030515     c                   eval      DB0fvf = BLPfvf
098920030515     c                   eval      DB0vlf = BLPvlf
098930030515     c                   eval      DB0fls = BLPfls
098940030515     c                   eval      DB0ncd = BLPncd
098950030515     c                   eval      DB0nca = BLPnca
098960030515     c                   eval      DB0xco = BLPxco
098970030515     c                   eval      DB0ffd = BLPffd
098980030515     c                   eval      DB0dcr = BLPdcr
098990030515     c                   eval      DB0hcr = BLPhcr
099000030515     c                   eval      DB0tcr = BLPtcr
099010030515     c                   eval      DB0fbr = BLPfbr
099020030515     c                   eval      DB0gga = BLPgga
099030030515     c                   eval      DB0gma = BLPgma
099040030515     c                   eval      DB0gva = BLPgva
099050030515     c                   move      ' '           DB0gva
099060030515     c                   eval      DB0sop = BLPsop
099070030515     c                   eval      DB0ctr = BLPctr
099080030515     c                   eval      DB0drt = BLPdrt
099090030515     c                   eval      DB0fpp = BLPfpp
099100030515     c                   eval      DB0nrt = BLPnrt
099110030515     c                   eval      DB0pdr = BLPpdr
099120030515      *
099130030515      * Cod. cliente mittente
099140040311     c                   move      BLPksc        w0040
099150030515if  1c                   IF        BLPccm <> *zeros
099160030515     c                   movel     BLPccm        DB0ccm
099170030515x   1c                   ELSE
099180030515      *   Lo imposto solo se non è codice generico destinatario
099190040311if  2c                   if        w0040  <> 9999
099200030515     c                   movel     BLPksc        DB0ccm
099210030515x   2c                   else
099220030515     c                   movel     BLPksc        DB0ccd
099230030515     c                   movel     BLPlna        DB0ccd
099240030515e   2c                   endif
099250030515e   1c                   ENDIF
099260030515      *
099270030515      * Allega documento di trasporto
099280081119     c                   clear                   DB0bam
099290060505if  1c                   if           BLPfst = 'S'
099300030515     c                             or BLPfst = 'C'
099310030515     c                   movel     'S'           DB0bam
099320060505e   1c                   endif
099330060524      *
099340060524      * Se bolla di RESO: Codice cliente da Assegnato
099350061107     c*** (già fatto:)   clear                   dsBL4A
099360060524if  1c                   if        DB0fbr = 'S'
099370060524     c                   exsr      RepAR4_A
099380060524e   1c                   endif
099390030515      *
099400030515      * Destinatario: vedo se esiste 2ª ragione sociale
099410061107     c                   clear                   DB0rd2
099420061107     c                   exsr      RepAR4_D
099430030515      *
099440030515      * Note
099450061107     c                   clear                   DB0nt1
099460061107     c                   clear                   DB0nt2
099470061107     c                   exsr      RepAR4_89
099480030515      *
099490030515     c                   ENDSR
099500030515      *
099510030515      *---------------------------------------------------------------*
099520050217      *?1ª impostaz. campi della DS in base a quelli del file FNARB  ?*
099530030515      *---------------------------------------------------------------*
099540030515     c     sr_DS_ARB     BEGSR
099550030515      *
099560030515     c                   eval      DB0cbo = ARBcbo
099570030515     c                   eval      DB0tsp = ARBtsp
099580030515     c                   eval      DB0mgs = ARBmgs
099590030515     c                   eval      DB0tc1 = ARBtc1
099600030515     c                   eval      DB0tc2 = ARBtc2
099610030515     c                   eval      DB0fns = ARBfns
099620030515     c                   eval      DB0rmn = ARBrmn
099630030515     c                   eval      DB0rma = ARBrma
099640030515     c                   eval      DB0ias = ARBias
099650030515     c                   eval      DB0vas = ARBvas
099660030515     c                   eval      DB0vmd = ARBvmd
099670030515     c                   eval      DB0vad = ARBvad
099680030515     c                   eval      DB0qft = ARBqft
099690030515     c                   eval      DB0lna = ARBlna
099700030515     c                   eval      DB0znc = ARBznc
099710030515     c                   eval      DB0rsm = ARBrsm
099720030515     c                   eval      DB0inm = ARBinm
099730030515     c                   eval      DB0cam = ARBcam
099740030515     c                   eval      DB0lom = ARBlom
099750030515     c                   eval      DB0prm = ARBprm
099760030515     c                   eval      DB0nzm = ARBnzm
099770030515     c                   eval      DB0fap = ARBfap
099780030515     c                   eval      DB0rmo = ARBrmo
099790030515     c                   eval      DB0rsd = ARBrsd
099800030515     c                   eval      DB0ind = ARBind
099810030515     c                   eval      DB0cad = ARBcad
099820030515     c                   eval      DB0lod = ARBlod
099830030515     c                   eval      DB0prd = ARBprd
099840030515     c                   eval      DB0nzd = ARBnzd
099850030515     c                   eval      DB0fin = ARBfin
099860030515     c                   eval      DB0gc1 = ARBgc1
099870030515     c                   eval      DB0gc2 = ARBgc2
099880030515     c                   eval      DB0nas = ARBnas
099890030515     c                   eval      DB0ncl = ARBncl
099900030515     c                   eval      DB0pkf = ARBpkf
099910030515     c                   eval      DB0fvf = ARBfvf
099920030515     c                   eval      DB0vlf = ARBvlf
099930030515     c                   eval      DB0fls = ARBfls
099940030515     c                   eval      DB0ncd = ARBncd
099950030515     c                   eval      DB0nca = ARBnca
099960030515     c                   eval      DB0xco = ARBxco
099970030515     c                   eval      DB0ffd = ARBffd
099980030515     c                   eval      DB0dcr = ARBdcr
099990030515     c                   eval      DB0hcr = ARBhcr
100000030515     c                   eval      DB0tcr = ARBtcr
100010030515     c                   eval      DB0fbr = ARBfbr
100020030515     c                   eval      DB0gga = ARBgga
100030030515     c                   eval      DB0gma = ARBgma
100040030515     c                   eval      DB0gva = ARBgva
100050030515     c                   move      ' '           DB0gva
100060030515     c                   clear                   DB0sop
100070030515     c                   eval      DB0ctr = ARBctr
100080030515     c                   clear                   DB0drt
100090030515     c                   clear                   DB0fpp
100100030515     c                   clear                   DB0nrt
100110030515     c                   clear                   DB0pdr
100120030515      *
100130030515      * Allega documento di trasporto
100140060524      * & Codice cliente da Assegnato
100150061107     c*** (già fatto:)   clear                   dsBL4A
100160031029     c                   clear                   DB0bam
100170061107     c                   exsr      RepAR4_A
100180060505     c                   if        §B4Abm = 'S' or §B4Abm = 'C' or
100190031029     c                             §B4Abm = 'P' or §B4Abm = 'Q'
100200030515     c                   movel     'S'           DB0bam
100210060505     c                   endif
100220030515      *
100230030515      * Destinatario: vedo se esiste 2ª ragione sociale
100240061107     c                   clear                   DB0rd2
100250061107     c                   exsr      RepAR4_D
100260030515      *
100270030515      * Note
100280061107     c                   clear                   DB0nt1
100290061107     c                   clear                   DB0nt2
100300061107     c                   exsr      RepAR4_89
100310030515      *
100320030515     c                   ENDSR
100330030515      *
100340030515      *---------------------------------------------------------------*
100350050217      *?2ª impostaz. campi della DS in base a quelli delle tabelle   ?*
100360050217      *?   "generiche" (non da "1A")                                 ?*
100370030515      *---------------------------------------------------------------*
100380030515     c     sr_DS_TAB     BEGSR
100390030515      *
100400030515      * Carico le tabelle "generiche" (3A, TB, 5E)
100410030515     c                   exsr      CarTAB
100420030515      *
100430030515      * Codici bolla
100440030515     c                   eval      DB0tb1 = §3Atb1
100450030515     c                   eval      DB0tb2 = §3Atb2
100460030515     c                   eval      DB0dcb = §3Ades
100470030515      * Tipo servizio
100480030515if  1c                   if        DB0dts = *blanks
100490030515     c                   eval      DB0dts = §5Edes
100500030515     c                   eval      DB0fst = §5Efst
100510030515e   1c                   endif
100520030515      * Consegne particolari
100530030515if  1c                   if            DB0tc1 <> *blanks
100540030515     c                             and DB0dc1 =  *blanks
100550030515     c                   movel(p)  DB0tc1        TBLkey
100560030515     c                   exsr      Tab_1P
100570030515     c                   eval      DB0dc1 = §1Pdes
100580030515e   1c                   endif
100590030515if  1c                   if            DB0tc2 <> *blanks
100600030515     c                             and DB0dc2 =  *blanks
100610030515     c                   movel(p)  DB0tc2        TBLkey
100620030515     c                   exsr      Tab_1P
100630030515     c                   eval      DB0dc2 = §1Pdes
100640030515e   1c                   endif
100650030515      * Esenzione IVA
100660030515if  1c                   if            DB0cei <> *blanks
100670030515     c                             and DB0dce =  *blanks
100680030515     c                   eval      DB0dce =  §EIdes
100690030515e   1c                   endif
100700030515      *
100710030515     c                   ENDSR
100720060524      *
100730060524      *---------------------------------------------------------------*
100740060524      *?Reperimento dati da FIAR400F x trk "A"                       ?*
100750061107      *?(dati aggiuntivi per P.Assegnato)                            ?*
100760060524      *---------------------------------------------------------------*
100770060524     c     RepAR4_A      BEGSR
100780060524      *
100790061107     c*** (già fatto:)   clear                   dsBL4A
100800061107      *
100810060524     c                   eval      Wtrc = 'A'
100820060524     c     KLDVtrc       chain     FIAR4000
100830060524      *
100840060524if  1c                   if        %found(FIAR401L)
100850060524     c                   movel     AR4Not        dsBL4A
100860060524e   1c                   endif
100870060524      *
100880060524     c                   ENDSR
100890061107      *
100900061107      *---------------------------------------------------------------*
100910061107      *?Reperimento dati da FIAR400F x trk "D"                       ?*
100920061107      *?(2ª ragione sociale del destinatario)                        ?*
100930061107      *---------------------------------------------------------------*
100940061107     c     RepAR4_D      BEGSR
100950061107      *
100960061107     c*** NON esiste:    clear                   dsBL4D
100970061107     c*** (già fatto:)   clear                   DB0rd2
100980061107      *
100990061107     c                   eval      Wtrc = 'D'
101000061107     c     KLDVtrc       chain     FIAR4000
101010061107      *
101020061107if  1c                   if        %found(FIAR401L)
101030061107     c                   movel     AR4not        DB0rd2
101040061107e   1c                   endif
101050061107      *
101060061107     c                   ENDSR
101070171116      *//*
101080171116      *//*---------------------------------------------------------------*
101090171116      *//*?Reperimento dati da FIAR400F x trk "I"                       ?*
101100171116      *//*?(dati DPD)                                                   ?*
101110171116      *//*---------------------------------------------------------------*
101120171116     c*//  RepAR4_I      BEGSR
101130171116      *
101140171116     c*** (già fatto:)   clear                   dsBL4I
101150171116      *
101160171116     c*//                eval      Wtrc = 'I'
101170171116     c*//  KLDVtrc       chain     FIAR4000
101180171116      *
101190171116if  1c*//                if        %found(FIAR401L)
101200171116     c*//                movel     AR4not        dsBL4I
101210171116e   1c*//                endif
101220171116      *
101230171116     c*//                ENDSR
101240071008      *
101250071015      *---------------------------------------------------------------*
101260071015      *?Reperimento dati da FIAR400F x trk "M"                       ?*
101270071015      *?(dati aggiuntivi per O.R.M. abbinato al P.Franco PrePagato)  ?*
101280071015      *---------------------------------------------------------------*
101290071015     c     RepAR4_M      BEGSR
101300071015      *
101310071015     c*** (già fatto:)   clear                   dsBL4M
101320071015      *
101330071015     c                   eval      Wtrc = 'M'
101340071015     c     KLDVtrc       chain     FIAR4000
101350071015      *
101360071015if  1c                   if        %found(FIAR401L)
101370071015     c                   movel     AR4Not        dsBL4M
101380071015e   1c                   endif
101390071015      *
101400071015     c                   ENDSR
101410061107      *
101420061107      *---------------------------------------------------------------*
101430061107      *?Reperimento dati da FIAR400F x trk "Q"                       ?*
101440061107      *?(Codice Fiscale del mittente a sx & del destinatario a dx)   ?*
101450061107      *---------------------------------------------------------------*
101460061107     c     RepAR4_Q      BEGSR
101470061107      *
101480061108     c*** (già fatto:)   clear                   dTA4Q
101490061107     c*** (già fatto:)   clear                   DB0cpi
101500061107      *
101510061107     c                   eval      Wtrc = 'Q'
101520061107     c     KLDVtrc       chain     FIAR4000
101530061107      *
101540061107if  1c                   if        %found(FIAR401L)
101550061108     c                   movel     AR4not        dTA4Q
101560061108e   1c                   endif
101570061107      *
101580061107     c                   ENDSR
101590061107      *
101600061107      *---------------------------------------------------------------*
101610061107      *?Reperimento dati da FIAR400F x trk "8" / "9"                 ?*
101620061107      *?(note)                                                       ?*
101630061107      *---------------------------------------------------------------*
101640061107     c     RepAR4_89     BEGSR
101650061107      *
101660061107     c*** NON esiste:    clear                   dsBL48
101670061107     c*** NON usata:     clear                   dsBL49
101680061107     c*** (già fatto:)   clear                   DB0nt1
101690061107     c*** (già fatto:)   clear                   DB0nt2
101700061107      *
101710061107     c                   eval      Wtrc = '8'
101720061107     c     KLDVtrc       chain     FIAR4000
101730061107if  1c                   if        %found(FIAR401L)
101740061107     c                   movel     AR4not        DB0nt1
101750061107e   1c                   endif
101760061107      *
101770061107     c                   eval      Wtrc = '9'
101780061107     c     KLDVtrc       chain     FIAR4000
101790061107if  1c                   if        %found(FIAR401L)
101800061107     c                   movel     AR4not        DB0nt2
101810061107e   1c                   endif
101820061107      *
101830061107     c                   ENDSR
101840171116      *
101850171116      *---------------------------------------------------------------*
101860171116      *?Reperimento dati da FIPND00F                                 ?*
101870171116      *?(dati DPD)                                                   ?*
101880171116      *---------------------------------------------------------------*
101890171116     c     RepFIPND      BEGSR
101900171116      *
101910171116if  1c                   if        NOT  %open(FIPND01L)
101920171116     c                   open      FIPND01L
101930171116e   1c                   endif
101940171116      *
101950171116     c     kLDV          chain     FIPND000
101960171116      *
101970171116if  1c                   if        NOT %found(FIPND01L)
101980171116     c                   clear                   PNDipn
101990171116e   1c                   endif
102000171116      *
102010171116     c                   ENDSR
102020030515      *
102030030515      *---------------------------------------------------------------*
102040050217      *?Carico le tabelle che servono: 3A/TB/5E.                     ?*
102050030515      *---------------------------------------------------------------*
102060030515     c     CarTAB        BEGSR
102070030515      *
102080030515      * Tab. "3A" - Codici Bolla
102090030515      * (NO se rimesso mittente)
102100030515     c                   clear                   ds3A
102110030515if  1c                   if        DB0ris <> 'M'
102120030515     c                   movel     '3A'          TBLcod
102130030515     c                   movel(p)  DB0cbo        TBLkey
102140030515     c     K03TAB        chain     TABEL
102150030515if  2c                   if        %found(TABEL00F)
102160030515     c                   movel     TBLuni        ds3A
102170030515e   2c                   endif
102180030515e   1c                   endif
102190070515     c                   eval      $Mamma =  (§3Abcm = 'S')
102200030515      *
102210030515      * Tab. "TB" - Tipi Bolla
102220030515      * (NO se rimesso mittente)
102230030515      * se Bolla in assegnato, con tipo bolla vedo se da contabilizzare
102240030515      * se no non stampo i dati fattura
102250030515     c                   clear                   dsTB
102260030515if  1c                   if        DB0ris <> 'M'
102270030515     c                   movel     'TB'          TBLcod
102280061030if  2c                   if            §3Atb2 <> *blanks
102290121029     c                             and DB0tbo <> 'P'
102300030515     c                             and DB0tbo <> '8'
102310030515     c                   movel(p)  §3Atb2        TBLkey
102320030515x   2c                   else
102330030515     c                   movel(p)  §3Atb1        TBLkey
102340030515e   2c                   endif
102350030515     c     K03TAB        chain     TABEL
102360030515if  2c                   if        %found(TABEL00F)
102370030515     c                   movel     TBLuni        dsTB
102380030515e   2c                   endif
102390030515e   1c                   endif
102400030515      *
102410030515      * Tab. "5E" - Tipi Servizio
102420030515     c                   clear                   ds5E
102430090513if  1c                   if        DB0tsp <> *blanks
102440030515     c                   movel     '5E'          TBLcod
102450030515     c                   movel(p)  DB0tsp        TBLkey
102460030515     c     K03TAB        chain     TABEL
102470030515if  2c                   if        %found(TABEL00F)
102480030515     c                   movel     TBLuni        ds5E
102490030515e   2c                   endif
102500030515e   1c                   endif
102510030515      *
102520030911      * Tab. "EI" - Esenzioni I.V.A.
102530030515     c                   clear                   dsEI
102540030911      * Tab. "1A" - Tipi incasso contrassegno
102550030911     c                   clear                   ds1A
102560030911      * Tab. "1P" - Consegne particolari
102570030911     c                   clear                   ds1P
102580150226      *//* Tab. "TBA" - Tipi bancale
102590150226     c*//                clear                   dTBA
102600030515      *
102610030515     c                   ENDSR
102620030911      *
102630030911      *---------------------------------------------------------------*
102640050217      *?Carico tabella "EI" (Esenzione I.V.A.)                       ?*
102650030911      *---------------------------------------------------------------*
102660030911     c     Tab_EI        BEGSR
102670030911      *
102680030911     c                   clear                   dsEI
102690030911if  1c                   if        DB0cei <> *blanks  and  DB0dce = *blanks
102700030911     c                   movel     'EI'          TBLcod
102710030911     c                   movel(p)  DB0cei        TBLkey
102720030911     c     K03TAB        chain     TABEL
102730030911if  2c                   if        %found(TABEL00F)
102740030911     c                   movel     TBLuni        dsEI
102750030911e   2c                   endif
102760030911e   1c                   endif
102770030911     c                   eval      DB0dce =  §EIdes
102780030911      *
102790030911     c                   ENDSR
102800030515      *
102810030515      *---------------------------------------------------------------*
102820050217      *?Carico tabella "1A" (Tipo incasso contrassegno)              ?*
102830030515      *---------------------------------------------------------------*
102840030515     c     Tab_1A        BEGSR
102850030515      *
102860030515     c                   clear                   ds1A
102870030515if  1c                   if        DB0cas <> *zeros
102880030515     c                   movel     '1A'          TBLcod
102890030515     c                   movel(p)  DB0tic        TBLkey
102900030515     c     K03TAB        chain     TABEL
102910030515if  2c                   if        %found(TABEL00F)
102920030515     c                   movel     TBLuni        ds1A
102930030515e   2c                   endif
102940030515e   1c                   endif
102950030515      *
102960030515      * Imposto i dati nella DS
102970030515     c                   eval      DB0dti = §1Ades
102980030515     c                   eval      DB0ucm = §1Aucm
102990030515     c                   eval      DB0ste = §1Aste
103000030515      *
103010030515     c                   ENDSR
103020030515      *
103030030515      *---------------------------------------------------------------*
103040050217      *?Carico tabella "1P" (Consegne particolari)                   ?*
103050030515      *---------------------------------------------------------------*
103060030515     c     Tab_1P        BEGSR
103070030515      *
103080030515     c                   clear                   ds1P
103090030515     c                   movel     '1P'          TBLcod
103100030515     c*** già impostato: movel(p)  DB0tc1/2      TBLkey
103110030515     c     K03TAB        chain     TABEL
103120030515if  1c                   if        %found(TABEL00F)
103130030515     c                   movel     TBLuni        ds1P
103140030515e   1c                   endif
103150030515      *
103160030515     c                   ENDSR
103170150226      *
103180150226      *//*---------------------------------------------------------------*
103190150226      *//*?Reperisco tabella "TBA" (Tipi Bancale)                       ?*
103200150226      *//*---------------------------------------------------------------*
103210150226     c*//  Tab_TBA       BEGSR
103220150226      *//*
103230150226     c*//                clear                   dTBA
103240150226     c*//                reset                   TIBS02ds
103250150226     c*//                eval      T02cod = 'TBA'
103260150226     c*//                movel(p)  Wtba          T02ke1
103270150226     c*//                call      'TIBS02R'
103280150226     c*//                parm                    KPJBA
103290150226     c*//                parm                    TIBS02ds
103300150226if  1c*//                if        T02err = *blanks
103310150226     c*//                movel     T02uni        dTBA
103320150226e   1c*//                endif
103330150226      *//*
103340150226     c*//                ENDSR
103350030515      *
103360030515      *---------------------------------------------------------------*
103370050217      *?Calcolo importi LDV                                          ?*
103380030515      *---------------------------------------------------------------*
103390061031     c     sr_DS_IMPO    BEGSR
103400030515      *
103410030515      * Vedo se esiste tassazione
103420030515      *  solo se tipo bolla contabilizzabile
103430061030if  1c                   IF        §TBfcb =  '1'
103440030515     c
103450061030     c                   eval      Wtrc   =  '1'
103460040607     c     KLDVtrc       chain     FIAR6000
103470030515     c
103480030515if  2c                   if        %found(FIAR601L)
103490030515     c                   movel     AR6div        DB0div
103500030515     c                   exsr      CtrDIV
103510030515      *
103520030515      * NON c'è 2ª bolla
103530061031if  3c                   if            DB0tb2 = *blanks
103540121029     c                             or  DB0tbo = 'P'
103550061031     c                             or  DB0tbo = '8'
103560030515      *  Estremi fattura
103570030515     c                   z-add     AR6nft        DB0nft
103580030515     c                   z-add     AR6dft        DB0dft
103590130124     c                   z-add     AR6fiv        wB0fiv
103600061031      *  Se è assegnato o franco prepagato => stampo tutta la tassaz.
103610061031if  4c                   if            %subst(DB0tb1:1:1) = 'A'
103620061031     c                             or  DB0tbo = '8'
103630061030     c                   eval      Wtrc   =  '1'
103640030515     c                   exsr      RepTASSAZ
103650030515e   4c                   endif
103660030515e   3c                   endif
103670030515e   2c                   endif
103680030515      *
103690061030      * C'è 2ª bolla
103700061031if  2c                   IF            DB0tb2 <> *blanks
103710121029     c                             and DB0tbo <> 'P'
103720061031     c                             and DB0tbo <> '8'
103730030515      *
103740061030     c                   eval      Wtrc   =  '2'
103750040607     c     KLDVtrc       chain     FIAR6000
103760030515      *
103770061030if  3c                   If        %found(FIAR601L)
103780030515     c                   movel     AR6div        DB0div
103790030515     c                   exsr      CtrDIV
103800030515     c                   movel     AR6cei        DB0cei
103810030515      *   Estremi fattura
103820030515     c                   z-add     AR6nft        DB0nft
103830030515     c                   z-add     AR6dft        DB0dft
103840130124     c                   z-add     AR6fiv        wB0fiv
103850030515if  4c                   if        AR6ksc <> *zeros
103860030515     c                   eval      DB0ccd =  AR6ksc
103870030515e   4c                   endif
103880061030     c                   eval      Wtrc   =  '2'
103890030515     c                   exsr      RepTASSAZ
103900061030e   3c                   EndIf
103910030515      *
103920030515e   2c                   ENDIF
103930030515      *
103940030515e   1c                   ENDIF
103950030515      *
103960030515      * Vedo se ha contrassegno
103970060505     c     KLDV          chain     FIAR9000
103980060505if  1c                   IF        %found(FIAR901L)
103990061030     c                   eval      DB0cas =  AR9cas
104000061030     c                   eval      DB0vca =  AR9vca
104010061030     c                   eval      DB0gca =  AR9gca
104020061030     c                   eval      DB0tic =  AR9tic
104030030515x   1c                   ELSE
104040030515     c                   clear                   DB0cas
104050030515     c                   clear                   DB0vca
104060030515     c                   clear                   DB0gca
104070030515     c                   clear                   DB0tic
104080030515     c                   clear                   DB0dti
104090030515     c                   clear                   DB0ucm
104100030515     c                   clear                   DB0ste
104110030515e   1c                   ENDIF
104120120509      *
104130120509      * Se P.F. Prepagato verificherò il Codice Fiscale del Mittente
104140120509      *   (NON se mittente estero)
104150120509if  1c                   if           ( DB0tbo  = '8'
104160120509     c                             and (DB0nft  <> *zeros
104170120509     c                              or  DB0ift  <> *zeros)
104180120509     c                             and  DB0nzm  =  *blanks )
104190120509     c                   exsr      RepAR4_Q
104200120509     c                   movel     §TA4Qcfm      DB0cpi
104210120509e   1c                   endif
104220121029      * Se P.A. con fattura immediata o con 2ª bolla verificherò il
104230121029      *   Codice Fiscale (NON se destinatario estero)
104240121026if  1c*//                if           ( DB0tbo  = 'A'
104250121029if  1c                   if          ( (%subst(DB0tb1:1:1) = 'A'
104260121029     c                             or  (DB0tb2  <> *blanks
104270121029     c                             and  DB0tbo  <> 'P'
104280121029     c                             and  DB0tbo  <> '8') )
104290120509     c                             and (DB0nft  <> *zeros
104300120509     c                              or  DB0ift  <> *zeros)
104310120509     c                             and  DB0nzd  =  *blanks )
104320120509     c                   exsr      RepAR4_Q
104330120509     c                   movel     §TA4Qcfd      DB0cpi
104340120509e   1c                   endif
104350120509     c                   eval      wB0cfi = DB0cpi
104360061030      *
104370030515      * Vedo se si tratta di bolla figlia:
104380040607      * Se bolla con MAMMA prendo c/a e segnacolli da bolla MAMMA
104390070515if  1c                   if        $Mamma =  *on
104400081117     c                   exsr      BollaFiglia
104410030515e   1c                   endif
104420030515      *
104430030515     c                   ENDSR
104440030515      *
104450030515      *---------------------------------------------------------------*
104460050217      *?Decodifica LNA                                               ?*
104470030515      *---------------------------------------------------------------*
104480030515     c     DecLNA        BEGSR
104490030515      *
104500140206if  1c*//                if            DB0dla =  *blanks
104510140206     c*//                          and DB0ris <> 'M'
104520140206if  1c                   if        DB0ris <> 'M'
104530030515      *
104540040311     c                   eval      SAVlna = DB0lna
104550070312     c                   clear                   OG146arr
104560061016     c                   clear                   OG148arr
104570070312     c                   clear                   OG150arr
104580030515     c     DB0lna        chain     AZORG
104590030515      *
104600030515if  2c                   if        %found(AZORG01L)
104610030515     c                   eval      DB0fag = ORGfag
104620030515     c                   eval      DB0dla = ORGdes
104630030515     c                   eval      DB0inl = ORGind
104640030515     c                   eval      DB0cal = ORGcpf
104650030515     c                   eval      DB0lol = ORGloc
104660030515     c                   eval      DB0prl = ORGpro
104670030515     c                   eval      DB0tel = ORGtel
104680140129     c*//                eval      DB0fax = ORGfax
104690140129     c                   eval      DB0fax = ORGdd2
104700070312     c                   movel     ORGde6        OG146arr
104710061016     c                   movel     ORGde8        OG148arr
104720070312     c                   movel     ORGdf0        OG150arr
104730030515e   2c                   endif
104740030515      *
104750030515e   1c                   endif
104760030515      *
104770030515     c                   ENDSR
104780030515      *
104790030515      *---------------------------------------------------------------*
104800050217      *?Reperimento dati di tutta la tassazione per la stampa        ?*
104810030515      *---------------------------------------------------------------*
104820030515     c     RepTASSAZ     BEGSR
104830030515      *
104840030515     c                   eval      DB0imv = AR6imv
104850030515     c                   eval      DB0cei = AR6cei
104860030515     c                   eval      DB0ift = AR6ift
104870030911      * Esenzione IVA
104880030911if  1c                   if            DB0cei <> *blanks
104890030911     c                             and DB0dce =  *blanks
104900030911     c                   exsr      Tab_EI
104910030911e   1c                   endif
104920030515      *
104930030515      * Controllo se ci sono varie esenti
104940030515      *
104950030515      * - varia 1
104960030515     c                   eval      Wsvx = AR6sv1
104970030515     c                   eval      Wvax = AR6va1
104980030515     c                   exsr      RepTASSAZ_b
104990030515      * - varia 2
105000030515     c                   eval      Wsvx = AR6sv2
105010030515     c                   eval      Wvax = AR6va2
105020030515     c                   exsr      RepTASSAZ_b
105030030515      * - varia 3
105040030515     c                   eval      Wsvx = AR6sv3
105050030515     c                   eval      Wvax = AR6va3
105060030515     c                   exsr      RepTASSAZ_b
105070030515      * - varie ulteriori solo per tipo record 1 (prima bolla)
105080040607     c     KLDVtrc       setll     FIAR7000
105090040607     c     KLDVtrc       reade     FIAR7000
105100030515      *
105110030515do  2c                   Dow       Not %eof(FIAR701L)
105120030515      *
105130030515     c                   eval      Wsvx = AR7svn
105140030515     c                   eval      Wvax = AR7van
105150030515     c                   exsr      RepTASSAZ_b
105160030515      *
105170040607     c     KLDVtrc       reade     FIAR7000
105180030515e   2c                   Enddo
105190030515      *
105200030515     c                   ENDSR
105210030515      *
105220030515      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
105230050217      *?Reperimento dati di tutta la tassazione per la stampa        ?*
105240050217      *?- 2ª parte -                                                 ?*
105250030515      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
105260030515     c     RepTASSAZ_b   BEGSR
105270030515      *
105280030515sel 1c                   Select
105290030515      *
105300030515w   1c                   When      Wsvx       = *blanks
105310030515      *
105320030515w   1c                   When      Wsvx       = §$2iva
105330030515     c                   eval      DB0iva     = Wvax
105340030515      *
105350030515w   1c                   When      Wsvx       = §$2bol
105360030515     c                   add       Wvax          DB0ese
105370050624     c                   z-add     Wvax          wBolloVirt
105380050624     c                   eval      $BolloVirt = *on
105390030515      *
105400030515x   1c                   Other
105410050624     c                   eval      *in31      = *off
105420030515     c     Wsvx          lookup    $Var                                   31
105430030515     c   31              add       Wvax          DB0ese
105440030515     c   31              sub       Wvax          DB0imv
105450030515      *
105460030515w   1c                   Endsl
105470030515      *
105480030515     c                   ENDSR
105490030515      *
105500030515      *---------------------------------------------------------------*
105510050217      *?Chain su TABEL per le PARTICOLARITA'                         ?*
105520030515      *---------------------------------------------------------------*
105530030515     c     TABxPAR       BEGSR
105540030515      *
105550100513     c                   movel(p)  WCdPar        w053a
105560030515     c                   movel     '7V'          TBLcod
105570030515     c                   movel(p)  KeyPAR        TBLkey
105580030515     c     K03TAB        chain     TABEL
105590030515      *
105600030515if  1c                   if        %found(TABEL00F)
105610030515     c                             and TBLflg = *blanks
105620100513     c                   eval      w053a = WCdPar + '-'
105630100513     c                                   + %trim(%subst(TBLuni:1:49))
105640030515x   1c                   else
105650030515     c                   clear                   TBLuni
105660030515e   1c                   endif
105670030515      *
105680030515     c                   ENDSR
105690030515      *
105700030515      *---------------------------------------------------------------*
105710050217      *?CONTROLLO C/A SE E' DA CONVERTIRE IN EURO                    ?*
105720030515      *---------------------------------------------------------------*
105730030515     c     CtrCAs        BEGSR
105740030515      *
105750030515      * E' da convertire
105760030515if  1c                   if        DB0ucm = 'S'
105770030515     c                   eval      $CAeur = *on
105780030515e   1c                   endif
105790030515      *
105800030515      * Se in EURO è da convertire e da stampare in EURO
105810030515if  1c                   if        DB0vca = §GEDcn
105820030515     c                   eval      $CAeur = *on
105830030515     c                   endif
105840030515      *
105850030515      * Devo convertire il c/a da valuta a moneta di conto
105860030624if  1c                   if        DB0vca <> §GEDcn
105870030515     c                   clear                   YEURCODS
105880030515     c                   movel     DB0vca        YECdvi
105890030515     c                   z-add     DB0cas        YECimi
105900030515     c                   movel     §GEDcn        YECdvo
105910030515     c                   call      'YEURCO'
105920030515     c                   parm                    YEURCODS
105930030515if  2c                   if        YECesi = *blanks
105940030515     c                   z-add     YECimo        Wcas
105950030515e   2c                   endif
105960030515x   1c                   else
105970030515     c                   z-add     DB0cas        Wcas
105980030515e   1c                   endif
105990030515      *
106000030515     c                   ENDSR
106010030515      *
106020030515      *---------------------------------------------------------------*
106030050217      *?CONTROLLO DIVISA DELLA FATTURA                               ?*
106040030515      *---------------------------------------------------------------*
106050030515     c     CtrDIV        BEGSR
106060030515      *
106070030515     c                   clear                   dsCV
106080030515      *
106090030515     c                   movel     'CV'          TBLcod
106100030515     c                   movel(p)  DB0div        TBLkey
106110030515     c     K03TAB        chain     TABEL
106120030515     c                   if        %found(TABEL00F)
106130030515     c                   movel     TBLuni        dsCV
106140030515     c                   movel     §CVfdc        DB0fdc
106150030515     c                   endif
106160030515      *
106170030515     c                   ENDSR
106180030515      *
106190030515      *---------------------------------------------------------------*
106200050217      *?SE BOLLA CON MAMMA PRENDO C/A E SEGNACOLLI DA BOLLA MAMMA    ?*
106210030515      *---------------------------------------------------------------*
106220081117     c     BollaFiglia   BEGSR
106230030515      *
106240090528     c                   if        not %open(FNLBL01L)
106250090528     c                   open      FNLBL01L
106260090528     c                   endif
106270090528      *
106280040607     c     KLDV          chain     FNLBL000
106290030515if  1c                   if        %found(FNLBL01L)
106300030515      *
106310030515     c     KLBL          chain     FNARB000
106320030515if  2c                   if        %found(FNARB01L)
106330030515      *
106340031015     c                   eval      wB0nrs = ARBnrs
106350030515     c                   eval      DB0fns = ARBfns
106360030515     c                   eval      DB0fls = ARBfls
106370030515     c                   eval      DB0ncd = ARBncd
106380030515     c                   eval      DB0nca = ARBnca
106390050114     c**!!!              eval      DB0xco = ARBxco
106400030515     c                   eval      DB0znc = ARBznc
106410030515      * Aggancio AR9
106420060505     c     KLBL          chain     FIAR9000
106430060505if  3c                   IF        %found(FIAR901L)
106440030515     c                   eval      DB0cas = AR9cas
106450030515     c                   eval      DB0vca = AR9vca
106460030515     c                   eval      DB0gca = AR9gca
106470030515     c                   eval      DB0tic = AR9tic
106480030515x   3c                   ELSE
106490030515     c                   clear                   DB0cas
106500030515     c                   clear                   DB0vca
106510030515     c                   clear                   DB0gca
106520030515     c                   clear                   DB0tic
106530030515     c                   clear                   DB0dti
106540030515     c                   clear                   DB0ucm
106550030515     c                   clear                   DB0ste
106560030515e   3c                   ENDIF
106570030515      *
106580030515e   2c                   endif
106590030515      *
106600030515e   1c                   endif
106610030515      *
106620030515     c                   ENDSR
106630030903      *
106640030903      *---------------------------------------------------------------*
106650050217      *?Impostazione Quantità e Unità di Misura da FNORM             ?*
106660030903      *---------------------------------------------------------------*
106670030903     c     QUM_FNORM     BEGSR
106680030903      *
106690030905     c*** (già fatto:)   clear                   $Qum
106700030905     c                   clear                   Wx
106710030903      * Colli
106720030905if  1c                   if        ORMncl   > *zeros
106730030905     c                             and Wx   < %elem($Qum)
106740030905     c                   add       1             wx
106750030905     c                   eval      $Qum(wx) = %editc(ORMncl : '4')
106760030905     c                                      + ' colli'
106770030903e   1c                   endif
106780030903      * Peso
106790030905if  1c                   if        ORMpkg   > *zeros
106800030905     c                             and Wx   < %elem($Qum)
106810030905     c                   add       1             wx
106820030905     c                   eval      $Qum(wx) = %editc(ORMpkg : '4')
106830030905     c                                      + ' kg'
106840030905e   1c                   endif
106850071025      * Volume bancali
106860071025if  1c                   if        ORMvlm   > *zeros
106870071025     c                             and Wx   < %elem($Qum)
106880071025     c                   add       1             wx
106890071025     c                   eval      $Qum(wx) = %editc(ORMvlm : '4')
106900071025     c                                      + ' mc'
106910071025e   1c                   endif
106920030905     *** * Volume
106930030905     *** * (in ultimo posto SOLO SE non ci sono bilici/motrici/autotreni)
106940030905if  1***c                   if        ORMvlm   > *zeros
106950030905     ***c                             and (Wx  < %elem($Qum) - 1
106960030905     ***c                              or  Wx  = %elem($Qum) - 1
106970030905     ***c                              and w0112 = *zeros)
106980030905     ***c                   add       1             wx
106990030905     ***c                   eval      $Qum(wx) = %editc(ORMvlm : '4')
107000030905     ***c                                      + ' metri cubi'
107010030905e   1***c                   endif
107020030905     *** * Bancali
107030030905     *** * (in ultimo posto SOLO SE non ci sono bilici/motrici/autotreni)
107040030905if  1***c                   if        ORMbnc   > *zeros
107050030905     ***c                             and (Wx  < %elem($Qum) - 1
107060030905     ***c                              or  Wx  = %elem($Qum) - 1
107070030905     ***c                              and w0112 = *zeros)
107080030905     ***c                   add       1             wx
107090030905     ***c                   eval      $Qum(wx) = %editc(ORMbnc : '4')
107100030905     ***c                                      + ' bancali'
107110030905e   1***c                   endif
107120030905     *** * Bilici
107130030905if  1***c                   if        ORMblc  > *zeros
107140030905     ***c                             and Wx  < %elem($Qum)
107150030905     ***c                   add       1             wx
107160030905     ***c                   eval      $Qum(wx) = %editc(ORMblc : '4')
107170030905     ***c                                      + ' bilici'
107180030905e   1***c                   endif
107190030905     *** * Motrici
107200030905if  1***c                   if        ORMmtc  > *zeros
107210030905     ***c                             and Wx  < %elem($Qum)
107220030905     ***c                   add       1             wx
107230030905     ***c                   eval      $Qum(wx) = %editc(ORMmtc : '4')
107240030905     ***c                                      + ' motrici'
107250030905e   1***c                   endif
107260030905     *** * Autotreni
107270030905if  1***c                   if        ORMatt  > *zeros
107280030905     ***c                             and Wx  < %elem($Qum)
107290030905     ***c                   add       1             wx
107300030905     ***c                   eval      $Qum(wx) = %editc(ORMatt : '4')
107310030905     ***c                                      + ' autotreni'
107320030905e   1***c                   endif
107330030903      *
107340030903     c                   ENDSR
107350130306      *
107360130306      /free
107370130306
107380130306       //--------------------------------------------------------------
107390130306       //?Scrittura record (relativo alla spedizione) nel *file PDFLIST?
107400130306       //--------------------------------------------------------------
107410130306       BEGSR  sr_WrtRec_PdfList;
107420130306
107430130307         // -?NON richiesta stampa del PDF?
107440140404         if  %parms() = 1;
107450130306           leavesr;
107460130306         endif;
107470130306
107480130306         // -?Reperimento numero "chiave elenco"?(SE non ricevuto)?
107490130306         If  DB6num <= *zero;
107500130306
107510130306           clear  TRUL33ds;
107520130306           I33tla = 'L';
107530130306           I33ope = *zero;
107540130306           I33cnu = 55;
107550130306           I33num = 1;
107560130306           kpjbu  = TRUL33ds;
107570130306
107580130306           TRUL33R ( kpjba );
107590130306
107600130306           TRUL33ds = kpjbu;
107610130307           if  O33err > *zero;
107620130307             Pmsg1  = $Msg(3);
107630130307             Pmsg2  = $Msg(O33err + 3);
107640130418             *in33  = *off;
107650130418             exsr  Posiz_Box;
107660130307             write  A4prtMSG;
107670130307             DB0tms = 'E';
107680130306             exsr  sr_End;
107690130306           endif;
107700130306
107710130306           DB6num = %editc( O33nri : 'X' );
107720130306
107730130306         EndIf;
107740130307
107750130307         // -?Apertura file?
107760130307         if  Not %open(PDFLIST);
107770130307           open  PDFLIST;
107780130307         endif;
107790130306
107800130308         // -?Ciclo di lettura record "§..." (ex "§PDF") in FIAR801L?
107810130306         clear  k06fiar801;
107820130306         k_AR8aas = DB0aas;
107830130306         k_AR8lnp = DB0lnp;
107840130306         k_AR8nrs = DB0nrs;
107850130306         k_AR8nsp = DB0nsp;
107860130306         k_AR8tmd = c_§PDF;
107870130306         setll  %kds( k06fiar801 : 5 )  FIAR8000;
107880130308         reade  %kds( k06fiar801 : 4 )  FIAR8000;
107890130306
107900130308         DoW  Not %eof(FIAR801L)  and  %subst(AR8tmd : 1 : 1) = '§';
107910130306
107920130306           dAR8§PDF = AR8uni;
107930130308
107940130315           If  AR8atb = *blank  and  §AR8Ptrt = 'S';
107950130306
107960130308             // -?Scrittura del rec. relativo alla sped. nel *file PDFLIST?
107970130308             clear  RPDF;
107980130308             PDFklist  = DB6num;
107990130308             PDFksped  = %editc( DB0aas : 'X' ) + %editc( DB0lnp : 'X' ) +
108000130308                         %editc( DB0nrs : 'X' ) + %editc( DB0nsp : 'X' );
108010130311             PDFkey    = %trimr(§AR8PpdfP) + '_' + §AR8PpdfN;
108020130311             //PDFpath   = *blank;
108030130308             PDFinfo   = c_CopiaXdest + ' - ' + Prsd +
108040130308                         ' Spedizione ' + %editc(DB0lnp : 'X') +
108050130308                                    ' ' + %editc(DB0nrs : 'X') +
108060130308                                    ' ' + %editc(DB0nsp : 'X') +
108070130308                         ' del ' + %editc(Pdsp : 'Y') + ' ' +
108080130308                         Pndc;
108090130308             PDFresol  = 'S';
108100130308             PDFcopieM = '1';
108110130308             PDFcopieD = '0';
108120130308             PDFcopieS = '0';
108130130308             //PDFnoInfo = *blank;
108140130308             //PDFinfoP  = *blank;
108150130308             //PDFprtFst = *blank;
108160130308             wTime_ds  = %char( %timestamp() );
108170131008             PDFinsDt  = wTime_ds.wDate_Iso;
108180131008             PDFinsTm  = wTime_ds.wTime;
108190131008             //PDFprtDt  = wTime_ds.wDate_Iso;
108200131008             //PDFprtTm  = wTime_ds.wTime;
108210130308             PDFusrins = SDSusr;
108220130308             //__________
108230130308             write  RPDF;
108240130308             //¯¯¯¯¯¯¯¯¯¯
108250130308
108260130308           EndIf;
108270130306
108280130418           reade  %kds( k06fiar801 : 4 )  FIAR8000;
108290130306
108300130306         EndDo;
108310130306
108320130306       ENDSR;
108330130306
108340130306       //--------------------------------------------------------------
108350140303       //?Stampa immagini associate alle spediz. (dati già in PDFLIST) ?
108360130306       //--------------------------------------------------------------
108370130306       BEGSR  sr_PrtPdf;
108380130306
108390130306         // -?Impostazione parametri?
108400130312         PDF_KList    = DB6num;
108410130312         PDF_Printer  = trulPOVRds.§ULPoOutQ;
108420130312         PDF_FormType = trulPOVRds.§ULPoFType;
108430130312         PDF_UsrDta   = trulPOVRds.§ULPoUDta;
108440130306
108450130306         // -?Richiamo funzione di stampa di tutte le immagini PDF?
108460130312         //  ?(segnalando - in stampa - l'eventuale errore)?
108470130312         callp(E)  Stampa_PDF ( PDF_KList :
108480130312                                PDF_Printer :
108490130312                                PDF_FormType :
108500130312                                PDF_UsrDta );
108510130312
108520130312         if  %error;
108530130312           Pmsg2  = $Msg(7);
108540130418           *in33  = *off;
108550130418           exsr  Posiz_Box;
108560130312           write  A4prtMSG;
108570130312           DB0tms = 'E';
108580130312           exsr  sr_End;
108590130312         endif;
108600130307
108610130307         // -?Restituzione tipo modulo stampato?
108620130307         DB0tms = 'F';
108630130306
108640130306       ENDSR;
108650130306
108660130306      /end-free
108670030903      *
108680030515      *---------------------------------------------------------------*
108690050217      *?DEFINIZIONE KEY-LIST                                         ?*
108700030515      *---------------------------------------------------------------*
108710030515     c     DefKLIST      BEGSR
108720030515      *
108730051115      * - FNBLP01L / FNBLT01L /
108740060505      *   FNARB01L / FNART01L /
108750030515      *   FNLBL01L /
108760060505      *   FIAR501L/31C / FIAR901L
108770040607     c     KLDV          klist
108780030515     c                   kfld                    DB0aas
108790030515     c                   kfld                    DB0lnp
108800030515     c                   kfld                    DB0nrs
108810030515     c                   kfld                    DB0nsp
108820060505      * - FIAR401L / FIAR601L / FIAR701L
108830060505     c     KLDVtrc       klist
108840060505     c                   kfld                    DB0aas
108850060505     c                   kfld                    DB0lnp
108860060505     c                   kfld                    DB0nrs
108870060505     c                   kfld                    DB0nsp
108880060505     c                   kfld                    Wtrc
108890040607      * - FIAR501L/31C
108900030717     c     K05AR501      klist
108910030717     c                   kfld                    DB0aas
108920030717     c                   kfld                    DB0lnp
108930030717     c                   kfld                    DB0nrs
108940030717     c                   kfld                    DB0nsp
108950030717     c                   kfld                    AR5trd
108960040607      * - FIAR801L
108970040607     c     K06AR801      klist
108980040607     c                   kfld                    DB0aas
108990040607     c                   kfld                    DB0lnp
109000040607     c                   kfld                    DB0nrs
109010040607     c                   kfld                    DB0nsp
109020040607     c                   kfld                    AR8tmd
109030040607     c                   kfld                    AR8prr
109040070514      * - FNBLT01L / FNART01L
109050070514     c     KLDVnsc       Klist
109060070514     c                   Kfld                    DB0aas
109070070514     c                   Kfld                    DB0lnp
109080070514     c                   Kfld                    DB0nrs
109090070514     c                   Kfld                    DB0nsp
109100070514     c                   Kfld                    Wnsc
109110070319      * - FIARN11L
109120070319     c     K06ARN11      klist
109130070319     c                   kfld                    DB0aas
109140070319     c                   kfld                    DB0lnp
109150070319     c                   kfld                    DB0nrs
109160070319     c                   kfld                    DB0nsp
109170070319     c                   kfld                    ARNcdn
109180070319     c                   kfld                    ARNgst
109190090528      * - FNBLP01L/ / FNARB01L / FIAR901L di bolla MAMMA
109200070514     c     KLBL          klist
109210070514     c                   kfld                    LBLaap
109220070514     c                   kfld                    LBLlpp
109230070514     c                   kfld                    LBLnrp
109240070514     c                   kfld                    LBLnsp
109250070514      * - FNBLT01L / FNART01L di bolla MAMMA
109260070514     c     KLBLnsc       Klist
109270070514     c                   kfld                    LBLaap
109280070514     c                   kfld                    LBLlpp
109290070514     c                   kfld                    LBLnrp
109300070514     c                   kfld                    LBLnsp
109310070514     c                   Kfld                    Wnsc
109320070514      * - FIARS01L
109330070514     c     K05ARS01P     klist
109340070514     c                   kfld                    BLTfls
109350070514     c                   kfld                    BLTlna
109360070514     c                   kfld                    BLTnrs
109370070514     c                   kfld                    BLTnsc
109380070514     c                   kfld                    ARStrc
109390070514     c     K05ARS01A     klist
109400070514     c                   kfld                    ARTfls
109410070514     c                   kfld                    ARTlna
109420070514     c                   kfld                    ARTnrs
109430070514     c                   kfld                    ARTnsc
109440070514     c                   kfld                    ARStrc
109450060505      * - FNORM01L
109460060505     c     K04ORM01      klist
109470071015     c                   kfld                    ORMpoe
109480071015     c                   kfld                    ORMnsr
109490071015     c                   kfld                    ORMnor
109500071015     c                   kfld                    ORMnrv
109510030515      * - TABEL00F
109520030515     c     K03TAB        klist
109530040223     c                   kfld                    TBLkut
109540030515     c                   kfld                    TBLcod
109550030515     c                   kfld                    TBLkey
109560030515     c     K02TAB        klist
109570040223     c                   kfld                    TBLkut
109580030515     c                   kfld                    TBLcod
109590030515      * - FLNUF01L
109600030515     c     K04NUF        klist
109610030515     c                   kfld                    NUFaaa
109620030515     c                   kfld                    NUFcnu
109630030515     c                   kfld                    NUFfil
109640030515     c                   kfld                    NUFfl2
109650030515      *
109660030515     c                   ENDSR
109670030515
109680030515** > $Msg ---------------------------------------------------------------------*
109690030515IMPOSSIBILE CONTINUARE LA RISTAMPA BOLLE
109700071112PERCHE' STAMPATE PIU' DI &NrLdV BOLLE !!!
109710130307ERRORE IN REPERIMENTO NUMERATORE PER STAMPA PDF ALLEGATO ALLA LDV:
109720130307   NON REPERITO IL NUMERATORE
109730130307   NUMERATORE ALLOCATO
109740130307   SUPERATO IL LIMITE MASSIMO PER IL NUMERATORE
109750130312RILEVATO ERRORE IN FASE DI STAMPA DEI DOCUMENTI ALLEGATI
109760140303** > $GgS/$Gio *
1097700306241LUNEDI
1097800306242MARTEDI
1097900410263MERCOLEDI
1098000306244GIOVEDI
1098100306245VENERDI
1098200305156SABATO
1098300305157DOMENICA
109840030515M          MATT.
109850030515P          POM.
